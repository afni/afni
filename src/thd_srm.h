#ifndef _THD_SRM_HEADER_
#define _THD_SRM_HEADER_

/*----------------------------------------------------------------------------
  Dataset-agnostic Shared Response Model (SRM) numerics for 3dSRM.

  The model (Chen et al., NIPS 2015):    X_i  ~=  W_i S + noise

     X_i  view i: nvox[i] features x ntime    (a "view" is any feature matrix;
                                               voxels of one subject, EEG
                                               channels, an atlas parcellation)
     W_i  nvox[i] x nfeat, orthonormal columns (W_i' W_i = I)
     S    nfeat x ntime, shared across all views

  Conventions
    * Views may differ in nvox but must all have the same number of columns
      ("ntime"), and the columns must already correspond (this library never
      resamples or reorders).  A column is a SAMPLE: a time point of a
      time-locked stimulus, or an item (a trial or condition beta, matched
      across views by a key).  Nothing in the fitting cares which; only the
      metrics that use adjacency (segment matching) and the provenance do.
    * All matrices are float, row-major.
        view i, feature f, time t      x[i][f*ntime + t]    (one feature's
                                                             time series is
                                                             contiguous)
        W_i, feature f, component c    w[i][f*nfeat + c]
        S,  component c, time t        s[c*ntime + t]
      Accumulations of inner products are done in double.  Sums over views
      are formed per thread and merged, so results agree across thread counts
      to rounding, not bit for bit.
    * Features are assumed centered (3dSRM z-scores each feature per run).
      The model nevertheless stores a per-feature mean mu[i] and removes it
      before projecting, as BrainIAK does, so an uncentered fit still
      transforms consistently.  Set SRM_params.center = 0 if the caller has
      already centered and wants no means stored.
    * Functions return SRM_OK (0) or a negative SRM_ERR_* code;
      SRM_strerror() gives text.  Nothing here calls exit().
    * OpenMP: loops over views are parallel.  Thread count is the caller's
      (omp_set_num_threads).

  Gauge.  (W_i R, R'S) fits equally well for any orthogonal R, so neither S
  nor the component order has intrinsic meaning.  SRM_canonicalize() fixes the
  gauge: S rotated to uncorrelated rows, ordered by decreasing variance, each
  row's sign set so its skewness is non-negative (if it has none, so that its
  largest-magnitude sample is positive).  Rows with tied variance are not
  uniquely ordered.  Use
  SRM_procrustes_rotation() to compare two fits.  Statistics that are invariant
  to R (reconstruction R^2, projected-space correlations summed over
  components, segment matching) are the safe quantities to test across
  subjects; per-component maps are not.
                                                     -- P Molfese, Oct 2026
------------------------------------------------------------------------------*/

#define SRM_OK                0
#define SRM_ERR_ARG          -1   /* bad argument, NULL, or nonpositive size  */
#define SRM_ERR_NFEAT        -2   /* nfeat < 1 or nfeat > min(nvox[i], ntime) */
#define SRM_ERR_NONFINITE    -3   /* NaN/Inf in data                          */
#define SRM_ERR_MEMORY       -4
#define SRM_ERR_SINGULAR     -5   /* a Procrustes/SVD step could not be done  */
#define SRM_ERR_MISMATCH     -6   /* model and data disagree on sizes         */
#define SRM_ERR_FORMAT       -7   /* a model file is malformed or incomplete  */
#define SRM_ERR_IO           -8   /* a model file could not be read/written   */
#define SRM_ERR_VERSION      -9   /* a model file is from a newer version     */

#define SRM_ALGO_PROB         1   /* probabilistic SRM, EM (BrainIAK SRM)     */
#define SRM_ALGO_DET          2   /* deterministic SRM (BrainIAK DetSRM)      */
#define SRM_ALGO_RSRM         3   /* RESERVED: robust SRM with a sparse
                                     individual term.  Not implemented; the
                                     enum is here so callers and file formats
                                     do not need to change when it is.       */

#define SRM_COUPLING_SHARED  (-1.0)

#define SRM_INIT_RANDOM       1   /* random orthonormal W_i, from seed        */
#define SRM_INIT_MODEL        2   /* warm start from SRM_params.init          */

#define SRM_KERNEL_AUTO       0   /* GRAM when it is cheaper (see SRM_params) */
#define SRM_KERNEL_VOXEL      1   /* iterate on the data, V x T per view      */
#define SRM_KERNEL_GRAM       2   /* iterate on Z'Z, (T+k) x (T+k) per view   */

#ifdef __cplusplus
extern "C" {
#endif

/*----------------------------------------------------------------------------
  Fitting
------------------------------------------------------------------------------*/

struct SRM_model ;

typedef struct {
   int          algo ;       /* SRM_ALGO_*                          [PROB]    */
   int          nfeat ;      /* k, the shared dimension             required  */
   int          niter ;      /* maximum iterations                  [500]     */
   double       tol ;        /* stop when |d objective|/|objective| < tol
                                                                    [1e-7]
                                Real data converge slowly.  On 22 subjects
                                hearing a story, S after 10 iterations
                                (BrainIAK's default) was 38% from its converged
                                value, after 20 30%, after 100 15%, after 200
                                4%; tol 1e-7 stops near 170 iterations, 1e-8
                                near 250, and 1e-13 took about 550.            */
   unsigned int seed ;       /* SRM_INIT_RANDOM seed                          */
   int          init ;       /* SRM_INIT_*                          [RANDOM]  */
   const struct SRM_model *init_model ;
                             /* SRM_INIT_MODEL: warm start.  Must have the same
                                nview, nvox[], nfeat.  Used for leave-one-
                                subject-out refits (drop one view from the full
                                fit and iterate a few steps).  Not modified.  */
   int          center ;     /* 1: remove and store per-feature means [1].
                                BrainIAK also demeans, but takes the energy
                                ||X_i||^2 from the undemeaned data, which
                                inflates its noise variances when the data
                                have offsets; here it is the demeaned energy. */
   int          nseg ;       /* runs: nseg contiguous segments of the columns
                                with lengths seglen[], summing to ntime.  0 or
                                1 means a single segment.                      */
   const int   *seglen ;
   double       coupling ;   /* SRM_COUPLING_SHARED (the default): one W per
                                subject for all runs.  Otherwise lambda >= 0,
                                a separate W_ir per subject and run, tied to a
                                subject anchor W_i (see SRM_model below), and
                                nseg >= 2 with seglen[] is required.
                                Coupled iterations start from a shared-W fit
                                (made first, with the same niter and tol) unless
                                SRM_INIT_MODEL gives a start, so lambda -> inf
                                returns the shared solution and lambda = 0
                                gives independent runs begun there.  Very large
                                lambda therefore converges slowly away from it;
                                use SHARED instead.

                                Units.  The prior that ties W_ir to the anchor is
                                worth `lambda` time points of data: lambda = 0
                                frees the runs, lambda = T_r weighs the anchor
                                as much as run r's own data, lambda >> T_r is
                                nearly the shared model.  (A run of T_r samples
                                gives X_r S_r' ~ T_r s2 W, s2 being the mean
                                variance of S's rows; the pull on W_ir is
                                lambda s2 W_i, with s2 taken once from the
                                starting S so the objective does not change under
                                the optimizer.)  Objective, with tau_i the prior
                                weight (lambda s2 for DET, lambda s2 / rho2_i at
                                the start for PROB):
                                  DET:   sum |X_ir - W_ir S_r|^2
                                         + 2 sum_i tau_i sum_r (k - <W_ir,W_i>)
                                  PROB:  marginal log-likelihood
                                         + sum_i tau_i sum_r <W_ir,W_i>
                                (minimized / maximized; <.,.> is the Frobenius
                                inner product).  Every step is an exact (DET) or
                                conditional (PROB) optimum, so it never gets
                                worse, up to float32 rounding of the stored W:
                                the penalty terms are of order lambda*s2*k*nseg*
                                nview and cancel, so the trace can wobble by
                                about 1e-7 of that once converged.             */
   const unsigned char * const *obs ;
                             /* [nview] each ntime: 1 where the view was
                                observed, 0 where it is missing (a censored
                                time point of that subject only); NULL, or a
                                NULL entry, means all observed.  Missing
                                values are never read.  The fit then uses each
                                view where it was observed: DET averages, at
                                each t, the views observed there; PROB's
                                posterior of s_t uses the precision of those
                                views only, and each rho2_i is a mean over the
                                view's own observed samples.  Centering is over
                                observed samples.  With a W per run each W_ir
                                uses its run's observed samples; lambda 0 then
                                needs nfeat observed points in every run and
                                view (else SRM_ERR_ARG).  [NULL]              */
   const double *ndim ;      /* [nview] the number of features each view stands
                                for, when its rows are a compressed form of
                                more features (SRM_compress): the noise
                                variance rho2 is per original feature, so it
                                and the likelihood use ndim[i], not nvox[i].
                                NULL: nvox.  [NULL]                           */
   float * const *w0 ;       /* [nview] the starting W of each view (nvox x
                                nfeat) in place of the random one, for
                                SRM_INIT_RANDOM; SRM_compress_start() gives the
                                start that reproduces the uncompressed fit's.
                                NULL: random.  With SRM_fit_multistart only the
                                first start uses it.  [NULL]                   */
   const int *stream ;       /* [nview] the random-number stream of each view's
                                random start (with seed): a fit of a subset of
                                views can give each the stream it has in the
                                whole set, so a view starts the same way in
                                every fit.  NULL: the view's index.  [NULL]    */
   int          kernel ;     /* SRM_KERNEL_*                        [AUTO]
                                Every W the fit makes lies in the span of
                                Z_i = [X_i | W_i at the start], so W_i = Z_i C_i
                                with C_i (T+k) x k, and an iteration needs only
                                G_i = Z_i'Z_i.  GRAM forms G_i once (V (T+k)^2/2
                                multiply-adds), iterates on it at (T+k) T k per
                                view, and forms W_i at the end; VOXEL costs
                                2 V T k per view per iteration.  The two give
                                the same fit up to rounding.  AUTO picks GRAM
                                when every view has at least T+k features and
                                the count of multiply-adds, at niter
                                iterations, is lower.                          */
   int          verb ;       /* 0 quiet, 1 per-iteration objective to stderr  */
   void       (*progress)( int iter , int niter , double objective , void *ctx ) ;
                             /* called once per iteration, from the calling
                                thread, with the iteration just evaluated
                                (0 at the start of every start), the cap, and
                                the objective.  NULL for none.                 */
   void        *progress_ctx ;
} SRM_params ;

/*! Fill p with the defaults noted above (nfeat is left 0, so it must be set). */
extern void SRM_params_default( SRM_params *p ) ;

typedef struct SRM_model {
   int      algo , nview , ntime , nfeat ;
   int     *nvox ;          /* [nview]                                       */
   float  **w ;             /* [nview] each nvox[i] x nfeat                  */
   float   *s ;             /* nfeat x ntime, shared response of the fit
                               data (posterior mean for SRM_ALGO_PROB)       */
   double  *rho2 ;          /* [nview] noise variance per feature, PROB only;
                               NULL for DET.  This is how PROB down-weights a
                               noisy view (a 5%-reliable EEG view against a
                               good fMRI view, say); DET weights views only by
                               their feature count and variance.             */
   float   *sigma_s ;       /* nfeat x nfeat prior covariance of S, PROB only  */
   float  **mu ;            /* [nview] each nvox[i]; NULL if center == 0.
                               Not saved in model files.                     */
   int      center ;        /* 1 if the fit removed per-feature means.  Kept
                               (and saved) so a view added later is centered
                               the same way even when mu is absent.          */

   /* Separate W per run (SRM_params.coupling != SRM_COUPLING_SHARED).
      w[i] is then the anchor and wrun[i*nseg + r] is W for subject i in run r,
      each nvox[i] x nfeat with orthonormal columns.  The anchor is what a run
      that was not in the fit uses, and what SRM_project() uses when no run is
      named.  wrun is NULL, and nseg 1, for a shared-W fit.

      Fit.  Alternate (a) W_ir = polar( X_ir S_r' + pull_i * W_i ) for every run
      r, where pull_i = lambda s2 (DET) or lambda s2 rho2_i / rho2_i(0) (PROB)
      ties W_ir to the anchor with the strength of lambda pseudo-time-points
      (see SRM_params.coupling); (b) W_i = polar( sum_r W_ir ).  lambda = 0 fits
      runs independently (the gauge of S can then differ by run, which is why a
      positive lambda is advisable); lambda -> infinity is the shared-W model.
      polar(A) is the orthonormal factor U V' of the thin SVD of A.           */
   int      nseg ;
   float  **wrun ;
   int      coupled ;       /* 1 if the model was fit with a W per run.  Kept
                               apart from wrun != NULL because a model read from
                               a file has no W at all, yet SRM_update() must
                               still give its new subjects one per run.       */
   double   coupling ;      /* the lambda it was fit with (SRM_COUPLING_SHARED if
                               none): a default for the caller, nothing more  */

   /* Running sums that let S absorb new subjects without the old data.
      PROB:  a_sum = sum_i W_i' X_i / rho2_i      (nfeat x ntime, double)
             rho_inv_sum = sum_i 1 / rho2_i
      DET:   a_sum = sum_i W_i' X_i               rho_inv_sum unused (= 0)
      nsubj_eff is the number of views summed.  Filled by SRM_fit() and
      maintained by SRM_update().  With coupled W, the W_ir enter the sums run
      by run.                                                                  */
   double  *a_sum ;
   double   rho_inv_sum ;
   int      nsubj_eff ;

   /* The same per time point, when some view was not observed at some time
      point (SRM_params.obs): nsubj_t[t] views were observed at t, and (PROB)
      rho_inv_t[t] is the sum of their 1/rho2.  Both NULL when every view is
      observed everywhere; then they are nsubj_eff and rho_inv_sum at every t.
      The view's own a_sum terms are zero where it is missing.                */
   double  *nsubj_t ;
   double  *rho_inv_t ;

   /* Bookkeeping for SRM_update().  The gauge is fixed when the first fit is
      canonicalized and is never changed by an update, so earlier outputs
      stay comparable.  drift accumulates |S_new - S_old|^2 / |S_old|^2 over
      updates.  It measures how far S has moved since the oldest subjects'
      W_i were fitted, which is how stale those W_i may be; refit from the
      data when it is not small (a few percent).                              */
   int      nupdate ;
   double   drift ;
} SRM_model ;

typedef struct {
   int      niter_done ;
   int      converged ;     /* 1 if tol was met before niter                 */
   double  *objective ;     /* [niter_done + 1]: the value at the start of
                               each iteration and at the end.
                               PROB: marginal log-likelihood of the data under
                                 the current parameters, constant -(T*sum_i V_i
                                 /2) log(2 pi) omitted.  Never decreases.
                               DET: sum_i |X_i - W_i S|_F^2.  Never increases.
                               Checked against BrainIAK 0.12: the PROB value is
                               identical to its logged likelihood on mean-zero
                               data; its DetSRM value is this one times
                               1/(2*ntime).                                  */
} SRM_fitinfo ;

/*! Fit a model to nview views.  x[i] is view i, nvox[i] x ntime.  On success
    *model (and, if non-NULL, *info) are allocated and owned by the caller. */
extern int  SRM_fit( int nview , int ntime , const int *nvox , float * const *x ,
                     const SRM_params *p , SRM_model **model , SRM_fitinfo *info ) ;

/*! SRM_fit() from nstart random starts, keeping the best.  Start s uses seed
    p->seed + 7919*s, so start 0 is exactly SRM_fit(p) and nstart == 1 is the
    same call.  "Best" is the highest final PROB likelihood or the lowest final
    DET residual (the last entry of the objective trace).  Ties keep the earlier
    start.  Why: SRM is not convex.  On real story data both models reached the
    same optimum from every start once run to a tight tolerance, but a fit
    stopped early differs from its neighbours by the unconverged remainder, and
    one hard synthetic DET problem (noise varying 6-fold across views) stalled at
    a slightly worse stationary point from one start.  Comparing start_obj is the
    check: agreement to the tolerance run at means the same optimum.

    start_obj (nstart doubles, may be NULL) receives every start's final
    objective, and *best (may be NULL) the index kept.  *model and *info are those
    of the kept start only; the others are freed as they are beaten, so memory is
    that of two models at most.  Starts run one after another, each using all
    threads.  nstart > 1 cannot be combined with SRM_INIT_MODEL (every start
    would be the same).  On error nothing is returned. */
extern int  SRM_fit_multistart( int nview , int ntime , const int *nvox ,
                                float * const *x , const SRM_params *p ,
                                int nstart , SRM_model **model , SRM_fitinfo *info ,
                                double *start_obj , int *best ) ;

extern void SRM_free_model  ( SRM_model *m ) ;
extern void SRM_free_fitinfo( SRM_fitinfo *fi ) ;

/*! Deep copy of a model, with view `drop` removed when drop >= 0 (use -1 to
    keep all).  Starting point for leave-one-subject-out warm starts.  The
    running sums (a_sum, rho_inv_sum) include every view and cannot be split,
    so a copy with a view dropped has a_sum NULL and nsubj_eff 0: it keeps S
    as a starting value only, and cannot be passed to SRM_update(). */
extern SRM_model * SRM_model_copy( const SRM_model *m , int drop ) ;

/*! Estimated peak bytes of SRM_fit() for these sizes, including a working copy
    of every view if copy_x is nonzero.  Feeds a -memory_limit guard. */
extern double SRM_estimate_bytes( int nview , const int *nvox , int ntime ,
                                  int nfeat , int algo , int copy_x ) ;

/*----------------------------------------------------------------------------
  Using a fitted model, including on data it has not seen
------------------------------------------------------------------------------*/

/*! Project new data from view `iview` into the shared space:
    s_out = W_i' (x - mu_i), nfeat x ntime_new.  x is nvox[iview] x ntime_new;
    ntime_new need not equal the fit's ntime. */
extern int SRM_project( const SRM_model *m , int iview , const float *x ,
                        int ntime_new , float *s_out ) ;

/*! Shared response estimated from a subset of views of held-out data:
    the mean of the projections, or, for a PROB model with weighted != 0, the
    posterior mean given only those views (projections weighted by 1/rho2,
    shrunk by the prior sigma_s), which is on the same scale as the S the fit
    returns and is what reconstruction R^2 needs.  For a DET model weighted is
    ignored: S is always the plain mean.
    x[j] is the data of view iview[j], j = 0..nsel-1.  s_out: nfeat x ntime_new.
    Leaving a view out of nsel gives the leave-one-out reference used by the
    held-out ISC and segment-matching metrics. */
extern int SRM_shared_response( const SRM_model *m , int nsel , const int *iview ,
                                float * const *x , int ntime_new , int weighted ,
                                float *s_out ) ;

/*! As SRM_project(), using subject iview's W for run irun (wrun) when the
    model has one; otherwise, and for irun < 0, the anchor w[iview]. */
extern int SRM_project_run( const SRM_model *m , int iview , int irun ,
                            const float *x , int ntime_new , float *s_out ) ;

/*! Estimate W_ir for a view against a fixed s, tied to an anchor:
        W_ir = polar( X_ir S_r' + coupling * s2 * W_anchor ),
    s2 = |s|^2 / (nfeat ntime), so the anchor is worth `coupling` time points
    of data (see SRM_params.coupling); the noise variance cancels out.
    s (nfeat x ntime) and x (nvox x ntime) are cut into nseg runs of seglen[]
    columns.  x is used as given, with no centering: remove means first.
    w_anchor (nvox x nfeat, orthonormal) usually comes from SRM_fit_view().
    coupling >= 0 (SRM_COUPLING_SHARED is an error here).  Writes wrun_out[r],
    nseg arrays of nvox x nfeat floats allocated by the caller. */
extern int SRM_fit_view_coupled( const float *s , int nfeat , int ntime ,
                                 const float *x , int nvox , int nseg ,
                                 const int *seglen , const float *w_anchor ,
                                 double coupling , float * const *wrun_out ) ;

/*! How far run irun's W is from the subject's anchor: |W_ir - W_i|_F^2 / nfeat,
    0 for identical, 2 for subspaces that share nothing, up to 4 when the
    columns are sign-flipped.  SRM_ERR_MISMATCH if the model has no W_ir. */
extern int SRM_run_deviation( const SRM_model *m , int iview , int irun ,
                              double *deviation ) ;

/*! SRM_shared_response() from projections that have already been made, so that
    a leave-one-out reference for each of N views costs N sums, not N^2 projections.
    proj[j] is the nfeat x ntime_new projection (SRM_project()) of view iview[j].
    Same meaning of weighted as above; the result is identical to calling
    SRM_shared_response() on the data those projections came from. */
extern int SRM_shared_from_projections( const SRM_model *m , int nsel , const int *iview ,
                                        float * const *proj , int ntime_new , int weighted ,
                                        float *s_out ) ;

/*! SRM_shared_from_projections when views have missing time points: obs[j] (ntime_new; NULL =
    all observed) for proj[j].  At each t only the views observed there count: their mean, or (PROB,
    weighted) the posterior mean with precision Sigma_s^-1 + sum 1/rho2 over them.  A t with no view
    gives 0.  obs NULL is SRM_shared_from_projections. */
extern int SRM_shared_from_projections_obs( const SRM_model *m , int nsel , const int *iview ,
                                            float * const *proj , const unsigned char * const *obs ,
                                            int ntime_new , int weighted , float *s_out ) ;

/*! x_hat = W_i s + mu_i, nvox[iview] x ntime_new. */
extern int SRM_reconstruct( const SRM_model *m , int iview , const float *s ,
                            int ntime_new , float *xhat ) ;

/*! Estimate W (and rho2 for a PROB-style noise estimate) for a NEW view given a
    fixed shared response s (nfeat x ntime).  This is the new-subject step:
    x is nvox x ntime, aligned in time with s.  Procrustes solution, so
    nvox >= nfeat is required.  w_out is nvox x nfeat; rho2_out and mu_out
    (nvox floats) may be NULL.  center != 0 removes each feature's mean first
    (returned in mu_out), through the identity X_c S' = X S' - mu (S 1)' so no
    copy of x is made.  *rho2_out is the residual variance of this fit to the
    fixed s, |X_c - W S|^2 / (ntime nvox).  s is treated as known, so there is no
    posterior-uncertainty term, and it is in-sample: W has about nfeat free
    parameters per feature, each using up one of the ntime samples, so
        rho2_out  ~=  true noise variance * (1 - nfeat/ntime)
    (measured: 0.75, 0.85, 0.92, 0.97, 0.99 of the truth at nfeat/ntime =
    0.25, 0.15, 0.075, 0.03, 0.01; multiplying by ntime/(ntime-nfeat) lands
    within 1% in every case).  Fits from SRM_fit() have the same optimism, so the
    value is deliberately NOT corrected here: a new view stays on the same footing
    as the fitted ones, and as BrainIAK.  The honest noise level for a new
    subject is the held-out residual variance that -cv reports. */
extern int SRM_fit_view( const float *s , int nfeat , int ntime ,
                         const float *x , int nvox , int center ,
                         float *w_out , double *rho2_out , float *mu_out ) ;

/*! Add nnew views to a fitted model and update S, in place.
    Old views keep their W_i and rho2_i, and enter the update only through
    a_sum and rho_inv_sum, so their data are not needed.  The new views' W and
    rho2, and the model's S and prior covariance, are iterated (p->niter, p->tol,
    p->verb) with the old sums held fixed.  All nnew views are added together,
    so their order within one call cannot matter; across calls it can matter
    slightly, because earlier subjects' W_i are not refitted to the improved S.
    That is the price of not needing their data.  It is measured, not assumed:
    compare against a batch SRM_fit() in the unit tests.

    For a PROB model the new views also pass through the identical E-step
    formulas as in SRM_fit(); for a DET model S is the plain average.
    x[j] is nvox[j] x m->ntime, aligned and preprocessed like the fit data.
    p->algo, p->nfeat and p->center are ignored (taken from m).  If the model is
    run-coupled (m->wrun != NULL) the new views get a W per run too, and
    p->coupling >= 0, p->nseg == m->nseg and p->seglen are required; for a
    shared-W model they must be left at their defaults.  m->drift and m->nupdate
    are advanced.  On error m is unchanged.

    info->objective is the objective of the restricted problem: the same as
    SRM_fit()'s but with the old subjects' constant terms (their |X_i|^2 and
    log rho2_i, whose data are gone) left out, so it is comparable between
    iterations of this call and nothing else.  It is monotone as usual.

    Missing data: p->obs, if given, has one mask per NEW view (1 observed);
    each new view then enters only where observed, as in SRM_fit().  A model
    whose own fit had missing points carries per-time sums (nsubj_t,
    rho_inv_t) and is updated per time point; if either side has missing
    points the result carries them too.

    The model must carry its sums: a model that went through SRM_model_copy
    with a view dropped, or one never fitted, returns SRM_ERR_MISMATCH.

    What the approximation costs (22 subjects hearing a story; held-out
    shared-space correlation, fit on two runs, scored on the third): adding 1, 3 and 6
    subjects to 21, 19 and 16 gave drift 0.003, 0.015, 0.038, and the existing
    subjects scored 1.6%, 1.2% and 5.0% lower than after a batch refit, because
    their W are not refitted; the added subjects scored no lower.  S moves a
    quarter of the way toward the refit S, but held-out scores match those of
    mapping the new subjects into the frozen S.  An update keeps S current; it
    does not replace refitting.

    Privacy note for callers: a_sum is a sum over subjects of their projected
    data.  With one or two subjects it is not meaningfully anonymous. */
extern int SRM_update( SRM_model *m , int nnew , const int *nvox ,
                       float * const *x , const SRM_params *p , SRM_fitinfo *info ) ;

/*! Attach a new view to an existing model (grows nview by one), estimating its
    W_i and rho2 against the model's own s.  x is nvox x m->ntime and must be
    aligned with the data the model was fit to.  S is NOT changed, which is
    what makes it safe for scoring; use SRM_update() to let S absorb the view. */
extern int SRM_model_add_view( SRM_model *m , const float *x , int nvox ) ;

/*! The same, for a view observed only where obs[t] != 0 (obs NULL: all):
    W_i, rho2 and the feature means come from the observed columns only. */
extern int SRM_model_add_view_obs( SRM_model *m , const float *x , int nvox ,
                                   const unsigned char *obs ) ;

/*----------------------------------------------------------------------------
  Compressed views.  Everything the fit does with a view's data X (nvox x T)
  goes through X'X and through W, and W always lies in the span of X's
  columns.  So X can be replaced by any R (rank x T) with R'R = X'X: X = U R
  for an orthonormal U (nvox x rank), and a fit on R, with SRM_params.ndim
  set to nvox, is the fit on X with W = U W_R.  R is at most T x T, so a view
  of 150000 voxels and 1300 time points shrinks from 780 MB to 7 MB.
  Projections W'X, scores, S, rho2 and the objective are the same; voxel
  maps need X again: W = X M, M = SRM_compress_map(R, W_R).
------------------------------------------------------------------------------*/

/*! R (rank x ntime, allocated here, *R_out) with R'R = X'X up to the
    directions whose share of X'X is below tol (relative to its trace; 0
    gives 1e-12): a pivoted Cholesky of X'X, formed in double, in parallel
    over voxel blocks.  Columns of X that are all zero (missing values) give
    zero columns of R. */
extern int SRM_compress( const float *x , int nvox , int ntime , double tol ,
                         float **R_out , int *rank_out ) ;

/*! SRM_compress and SRM_compress_start together, from one pass over the
    voxels (X'[X W0] at once): R, and in w0_out (rank x nfeat; NULL for none)
    the random start of view `stream` carried into R.  The rank is at most
    ntime, so w0_out needs ntime x nfeat floats. */
extern int SRM_compress_ex( const float *x , int nvox , int ntime , double tol ,
                            int nfeat , unsigned int seed , int stream ,
                            float **R_out , int *rank_out , float *w0_out ) ;

/*! The random start of view `stream` (as SRM_fit makes it from seed for an
    nvox-feature view), carried into the compressed space: w0 = U' W0
    (rank x nfeat).  A fit of the R with this as SRM_params.w0 follows the
    uncompressed fit step for step.  x is the same X that gave R. */
extern int SRM_compress_start( const float *x , int nvox , int ntime ,
                               const float *R , int rank , int nfeat ,
                               unsigned int seed , int stream , float *w0_out ) ;

/*! M (ntime x ncol, double) = R' (R R')^-1 v, for v (rank x ncol): then
    U v = X M.  With v = W_R, W = X M on the original features. */
extern int SRM_compress_map( const float *R , int rank , int ntime ,
                             const float *v , int ncol , double *M_out ) ;

/*! out (nvox x ncol) = X M, in parallel over the features: W = X M on the
    original features, M from SRM_compress_map. */
extern int SRM_compress_expand( const float *x , int nvox , int ntime ,
                                const double *M , int ncol , float *out ) ;

/*----------------------------------------------------------------------------
  Gauge
------------------------------------------------------------------------------*/

/*! Rotate (W, S) into the canonical gauge described above.  The reconstruction
    W S is unchanged to rounding.  Everything defined in the gauge is rotated
    with it: the W_i (and any W_ir), sigma_s, and a_sum.  Do not call this on a
    model that has been updated, which would silently move the gauge that
    earlier outputs were expressed in. */
extern int SRM_canonicalize( SRM_model *m ) ;

/*! The orthogonal nfeat x nfeat R minimizing |R a - b|_F, for two nfeat x ntime
    matrices (e.g. the shared responses of two fits).  r_out is row-major. */
extern int SRM_procrustes_rotation( const float *a , const float *b ,
                                    int nfeat , int ntime , float *r_out ) ;

/*----------------------------------------------------------------------------
  Saved models.  A model file carries what is needed to map NEW subjects into
  an existing shared space without the original data: S, the noise variances,
  and enough provenance to preprocess the new data identically.  It does not
  carry the fit subjects' W_i (subject-specific, large, and not needed), so a
  loaded model has w[i] == NULL for every view; SRM_project() and
  SRM_reconstruct() on those views return SRM_ERR_MISMATCH.  SRM_fit_view() and
  SRM_model_add_view() are how a loaded model gains subjects.

  Because a model can absorb new subjects (SRM_update), the file also carries
  a_sum, rho_inv_sum, nsubj_eff, drift, nupdate, and sigma_s, which together
  are the whole state the update needs.

  The file is one plain-text file (conventionally PREFIX.srm), written to a
  temporary name and renamed, so a reader never sees half a file:

      # comment lines and blank lines are ignored
      srm_version 1
      algo prob | det        axis time | item       zscore none | run
      nfeat K   ntime T   nsubj N   wseg R   center 0|1   coupled 0|1
      coupling L   nsubj_eff N   nupdate U   drift D   rho_inv_sum P
      ntime_in Tin   nseg R   seglen a b c ...   keep 0110...  (or: keep all)
      nitem M                                (item axis)
      item KEY                               (item axis, M lines, in order)
      subject LABEL NVOX                     (N lines, in order)
      data S k T          then k*T numbers, any line breaks   (float, %.9g)
      data sigma_s k k    (PROB)                              (float, %.9g)
      data rho2 N 1       (PROB)                              (double, %.17g)
      data a_sum k T                                          (double, %.17g)
      data nsubj_t 1 T    (missing data)                      (double, %.17g)
      data rho_inv_t 1 T  (missing data, PROB)                (double, %.17g)
      end

  Floats are written with 9 significant digits and doubles with 17, so a
  value read back is bit-identical to the one written.  Keys it does not know
  are ignored, and so are unknown "data" blocks, so later versions can add
  fields; a file whose srm_version is higher than SRM_FILE_VERSION is refused
  (SRM_ERR_VERSION).  Labels and item keys may not contain whitespace.
------------------------------------------------------------------------------*/

#define SRM_FILE_VERSION     1

#define SRM_AXIS_TIME        1
#define SRM_AXIS_ITEM        2

#define SRM_ZSCORE_NONE      0
#define SRM_ZSCORE_RUN       1   /* each feature within each run (segment)     */
#define SRM_ZSCORE_ALL       2   /* each feature over all of the subject's
                                    columns (items from one GLM, say)          */

typedef struct {
   int       version ;       /* SRM_FILE_VERSION                              */
   int       axis ;          /* SRM_AXIS_TIME or SRM_AXIS_ITEM                */

   /* How the fit data were put on the model's timeline.  New data must be
      treated the same way, so these are applied to new subjects, not chosen. */
   int       ntime_in ;      /* time points before censoring                  */
   unsigned char *keep ;     /* [ntime_in] 1 = retained, 0 = censored.  Number
                                of 1s equals model ntime.                     */
   int       nseg ;          /* runs on the retained timeline                 */
   int      *seglen ;        /* [nseg], sums to model ntime                   */
   int       zscore ;        /* SRM_ZSCORE_*                                  */

   /* SRM_AXIS_ITEM: columns are items, matched by key, in this order.  New
      subjects supply the same keys in any order; ntime_in/keep/seglen above
      are unused.  A key is the Item label, or Item:repeat when an item recurs
      across runs and each recurrence is its own column.                      */
   int       nitem ;
   char    **item_key ;      /* [nitem]                                       */

   int       nsubj ;         /* subjects the model was fit to                 */
   char    **label ;         /* [nsubj] their Subj labels                     */
} SRM_provenance ;

extern SRM_provenance * SRM_provenance_new ( void ) ;
extern void             SRM_provenance_free( SRM_provenance *pv ) ;

/*! Write m and pv to fname (atomically: a temporary file in the same
    directory is renamed over it).  The W_i are not written.  pv is required and
    must agree with m: pv->nsubj == m->nview, one label per view, and on the
    time axis sum(seglen) == m->ntime and the number of retained time points ==
    m->ntime (on the item axis nitem == m->ntime).  Non-finite values in the
    model are refused (SRM_ERR_NONFINITE).  SRM_ERR_IO if the file cannot be
    written, SRM_ERR_MISMATCH if m and pv disagree. */
extern int SRM_save_model( const char *fname , const SRM_model *m ,
                           const SRM_provenance *pv ) ;

/*! Read a model file.  *model has nview == pv->nsubj, nvox[] as saved, and
    w[] all NULL.  Both outputs are allocated and owned by the caller.
    SRM_ERR_IO if the file cannot be read, SRM_ERR_VERSION if it is from a newer
    version, SRM_ERR_FORMAT if anything is missing, malformed, or inconsistent. */
extern int SRM_load_model( const char *fname , SRM_model **model ,
                           SRM_provenance **pv ) ;

/*! 0 if new data of ntime_in time points with run lengths seglen[0..nseg-1]
    (measured on the model's retained timeline) fit the model's provenance;
    otherwise SRM_ERR_MISMATCH.  3dSRM calls this before touching any new
    subject so the failure is one clear message, not a size error deep in a
    matrix product. */
extern int SRM_provenance_check( const SRM_provenance *pv , int ntime_in ,
                                 int nseg , const int *seglen ) ;

/*! Item-axis counterpart.  New data have nkey keys; they must be exactly the
    model's keys, each once, in any order.  perm[j] receives the model column
    that new column j belongs to, so the caller can reorder to the model's
    order.  SRM_ERR_MISMATCH if any key is missing, extra, or repeated. */
extern int SRM_provenance_match_items( const SRM_provenance *pv , int nkey ,
                                       char * const *key , int *perm ) ;

/*----------------------------------------------------------------------------
  Applying a subject's W to DIFFERENT data of the same subject.  This is the
  design that keeps the alignment independent of what is tested: fit on a movie
  (or other runs), then project the task data.  The caller loads W_i from the
  datasets 3dSRM saved (-save_W) and wraps it so SRM_project() works.
  The new data need not match the fit timeline in length or order.
------------------------------------------------------------------------------*/

/*! One-view model around a copy of W (nvox x nfeat) and, if non-NULL, the
    feature means mu (nvox; center is then 1).  algo and ntime are 0, and s and
    rho2 are NULL; only SRM_project() and SRM_reconstruct() are meaningful. */
extern SRM_model * SRM_model_from_w( int nvox , int nfeat , const float *w ,
                                     const float *mu ) ;

/*----------------------------------------------------------------------------
  Held-out metrics.  Each takes data the model was NOT fit to, otherwise the
  numbers are optimistic (W_i has nvox*nfeat free parameters).
------------------------------------------------------------------------------*/

/*! Reconstruction R^2 of view `iview`: 1 - |x - x_hat|^2 / |x - mean|^2, where
    x_hat = W_i s + mu_i and s is the shared response of the same time points
    (from SRM_shared_response() on the OTHER views).  If vox_r2 is non-NULL it
    receives the per-feature R^2 (nvox floats; constant features get 0).
    Returns the pooled R^2 (1 - total SSE / total SST) through *r2; the baseline
    is each feature's own mean over these columns.  SRM_ERR_SINGULAR if the data
    have no variance. */
extern int SRM_reconstruction_r2( const SRM_model *m , int iview , const float *x ,
                                  const float *s , int ntime_new ,
                                  double *r2 , float *vox_r2 ) ;

/*! Time-segment matching (Chen et al. 2015), as in BrainIAK's SRM example.
    a and b are nfeat x ntime projections of the same stimulus time from two
    sources (typically view i projected, against the leave-one-out reference
    from the other views).  There are nseg = ntime - win windows of win
    consecutive columns, window i starting at column i.  Window i of a is
    correlated (Pearson, over its nfeat*win values) with candidate windows of b:
    its own counterpart i, and every window j with |i - j| >= win, so overlapping
    windows are not candidates.  It is correct when its counterpart has the
    highest correlation (ties go to the lower index).  *accuracy is the fraction
    correct; *chance is the mean of 1/ncandidate over windows.  win = 9 is the
    published default.  Needs nseg > win so that non-overlapping candidates
    exist.  A window with no variance correlates 0 with everything. */
extern int SRM_segment_match( const float *a , const float *b , int nfeat ,
                              int ntime , int win ,
                              double *accuracy , double *chance ) ;

/*! SRM_segment_match with a validity mask (ntime; NULL = all valid): a window that contains a
    sample with valid 0 (missing in the subject, or with no reference) is neither scored nor a
    candidate.  accuracy and chance are over the scored windows.  SRM_ERR_ARG if none is left. */
extern int SRM_segment_match_obs( const float *a , const float *b , int nfeat , int ntime ,
                                  int win , const unsigned char *valid ,
                                  double *accuracy , double *chance ) ;

/*! Item identification, the item-axis analogue of segment matching.  a and b
    are nfeat x nitem projections of the same items from two sources.  Item j
    of a is matched to the item of b with the highest correlation across
    components; *accuracy is the fraction matched to item j, *chance is
    1/nitem.  Segment matching is meaningless when the columns are not
    adjacent in time, which is why this exists. */
extern int SRM_item_identify( const float *a , const float *b , int nfeat ,
                              int nitem , double *accuracy , double *chance ) ;

/*! Correlation of a and b per component (nfeat values in r_out) and their
    mean in Fisher-z space returned as r through *r_mean.  The shared-space
    analogue of leave-one-out ISC; a Spearman variant and the nulls come from
    the INSYNC_* and THD_phase_set_* routines, which the caller can feed the
    same arrays.  A component with no variance in either input gets r = 0. */
extern int SRM_component_corr( const float *a , const float *b , int nfeat ,
                               int ntime , float *r_out , double *r_mean ) ;

/*----------------------------------------------------------------------------
  Reliability.  A shared-space score mixes three things: how reliable the
  subject's own signal is, how much of it is shared, and how well the model
  was fit.  These separate them.
------------------------------------------------------------------------------*/

/*! Rotation-invariant similarity of two shared responses of the same nfeat x
    nsamp shape (e.g. S fitted to two disjoint halves of the subjects).  The
    rows of each are orthonormalized and the principal angles between the two
    row spaces computed.  *mean_cos2 is the mean squared cosine (1 = same
    subspace, nfeat/nsamp = chance-ish for random subspaces).  cos_out
    (nfeat floats, may be NULL) gets the cosines, largest first. */
extern int SRM_shared_similarity( const float *s1 , const float *s2 , int nfeat ,
                                  int nsamp , double *mean_cos2 , float *cos_out ) ;

/*! Within-subject reliability from two measurements xa, xb (nrow x ncol) of
    the same samples (repeats of a stimulus, or two halves of the repeats).
    *r_pattern: mean over columns of the correlation across rows (how well the
    spatial pattern for a sample repeats).  *r_series: mean over rows of the
    correlation across columns (how well a feature's profile repeats).  Both
    are Fisher-z means returned as r; either pointer may be NULL.  This is the
    ceiling a cross-subject score can aim at; it needs genuine repeats. */
extern int SRM_split_reliability( const float *xa , const float *xb , int nrow ,
                                  int ncol , double *r_pattern , double *r_series ) ;

/*! Spearman-Brown: reliability of a measure lengthened by `factor` (2 turns a
    split-half r into the reliability of the whole). */
extern double SRM_spearman_brown( double r , double factor ) ;

/*! Copy the columns of x (nrow x ncol) flagged in sel (ncol 0/1 values) into
    out, preserving order; returns how many were copied.  Scoring a subset of
    samples (one condition, one scene type) is: gather the columns of the
    held-out data and of the reference, then call the metric functions. */
extern int SRM_gather_columns( const float *x , int nrow , int ncol ,
                               const unsigned char *sel , float *out ) ;

/*----------------------------------------------------------------------------
  Preprocessing
------------------------------------------------------------------------------*/

/*! z-score every feature of x (nvox x ntime) separately within each of nseg
    contiguous time segments (the runs) of length seglen[0..nseg-1] (sum must
    equal ntime).  A feature with zero variance in any segment is flagged, set
    to zero throughout, and counted.  If bad is non-NULL it receives a 0/1
    flag per feature (1 = constant somewhere) so the caller can drop it.
    Uses the population standard deviation (divide by the segment length, like
    scipy.stats.zscore).  "Constant" means a standard deviation of at most
    1e-7 of the mean's magnitude, or exactly zero, which is what float32 data
    that are really constant come to.  Non-finite data are an error
    (SRM_ERR_NONFINITE), not a flag.
    Returns the number of flagged features, or a negative SRM_ERR_*. */
extern int SRM_zscore_segments( float *x , int nvox , int ntime ,
                                int nseg , const int *seglen , unsigned char *bad ) ;

extern const char * SRM_strerror( int code ) ;

#ifdef __cplusplus
}
#endif

#endif /* _THD_SRM_HEADER_ */
