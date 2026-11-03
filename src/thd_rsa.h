#ifndef _THD_RSA_HEADER_
#define _THD_RSA_HEADER_

#include "thd_simmatrix.h"
#include <stddef.h>

/*----------------------------------------------------------------------------
  Dataset-independent representational geometry workflows.

  thd_simmatrix owns individual matrix arithmetic.  This module owns the RSA
  workflow one level above it: condition-pattern preprocessing, one subject's
  condition RDM, collections of subject RDMs, and the second-order subject
  geometry built from those collections.  Dataset/ROI extraction, command-line
  policy, and output remain in the calling program. This module also owns
  fitted-model cross-validation and synchronized RSA bootstrap inference.
                                                     -- P Molfese, Oct 2026
------------------------------------------------------------------------------*/

typedef struct THD_rsa_rdm_workspace THD_rsa_rdm_workspace ;

/*! Reusable storage for at most max_subject subject RDMs of at most
    max_condition conditions. */
extern THD_rsa_rdm_workspace *
       THD_rsa_rdm_workspace_new( int max_subject, int max_condition ) ;
extern void THD_rsa_rdm_workspace_free( THD_rsa_rdm_workspace *ws ) ;

/*! Reset the active subject/condition dimensions without reallocating. */
extern int THD_rsa_rdm_workspace_reset( THD_rsa_rdm_workspace *ws,
                                        int nsubject, int ncondition,
                                        char *err, size_t errlen ) ;

/*! Remove the feature-wise mean over conditions in place.  patterns is
    [condition][feature]. */
extern int THD_rsa_center_condition_patterns( int ncondition, int nfeature,
                                              float *patterns,
                                              char *err, size_t errlen ) ;

/*! Build one subject's condition RDM from [condition][feature] patterns.
    When center_conditions is nonzero, angle-based metrics first remove each
    feature's mean over conditions.  Euclidean differences already cancel a
    shared mean exactly, so that metric deliberately skips the subtraction to
    preserve bitwise compatibility.  The caller-owned patterns may be centered
    in place. */
extern THD_simmat * THD_rsa_condition_rdm( int ncondition, int nfeature,
                                           float *patterns, int metric,
                                           int center_conditions,
                                           char *err, size_t errlen ) ;

/*! Strict condition RDM for participant outcomes. After optional centering,
    reject nonfinite patterns, constant Pearson/Spearman patterns, and zero
    cosine norms. Returns NULL for undefined geometry. This opt-in variant
    leaves the ordinary condition-RDM API's degenerate-pattern behavior intact.
    patterns is [condition][feature] and may be centered in place. */
extern THD_simmat * THD_rsa_condition_rdm_checked(
                       int ncondition, int nfeature, float *patterns,
                       int metric, int center_conditions ) ;

/*! Pearson or Spearman correlation of two RDM triangles, clipped to
    +/-0.999999f before atanhf. Returns NAN for nonfinite/constant triangles
    or invalid arguments. a, b, sc1 and sc2 each have ntriangle entries;
    sc1/sc2 are caller-owned scratch. Inputs a/b are not modified. */
extern float THD_rsa_fisher_corr( int ntriangle, float *a, float *b,
                                  int metric, float *sc1, float *sc2 ) ;

/*! Equal mean Fisher z over all unordered repeat-RDM pairs. triangles has
    layout [nrepeat][ntriangle]; sc1/sc2 each hold ntriangle scratch entries.
    Uses THD_rsa_fisher_corr's metric/clipping policy. Returns NAN if fewer
    than two repeats or any pair is undefined; never drops individual pairs. */
extern float THD_rsa_repeat_stability( int nrepeat, int ntriangle,
                                       float *triangles, int metric,
                                       float *sc1, float *sc2 ) ;

/*! Copy a matrix's strict upper triangle.  With as_distance nonzero,
    similarities are converted entrywise to 1-s while distances are retained.
    This is the common feature sense required by second-order RSA. */
extern int THD_rsa_simmat_to_triangle( const THD_simmat *sm, float *triangle,
                                       int as_distance,
                                       char *err, size_t errlen ) ;

/*! Store one subject RDM in the active workspace.  as_distance has the same
    meaning as THD_rsa_simmat_to_triangle.  All stored rows must have the same
    resulting similarity/distance sense. */
extern int THD_rsa_rdm_workspace_store( THD_rsa_rdm_workspace *ws,
                                        int subject, const THD_simmat *rdm,
                                        int as_distance,
                                        char *err, size_t errlen ) ;

/*! Build the subject-by-subject second-order geometry from all subject RDMs in
    the active workspace.  Similarity triangles are converted to 1-s in the
    reusable scratch buffer before the outer metric is applied. */
extern THD_simmat *
       THD_rsa_second_order_rdm( THD_rsa_rdm_workspace *ws, int outer_metric,
                                 char *err, size_t errlen ) ;

/*! Descriptive elementwise mean of compact subject RDM triangles.  The input
    layout is [subject][THD_NTRI(ncondition)] and is not modified. */
extern THD_simmat *
       THD_rsa_mean_subject_rdms( int nsubject, int ncondition,
                                  const float *triangles, int is_distance,
                                  char *err, size_t errlen ) ;


#define THD_RSA_SUBJECTS 1
#define THD_RSA_CONDITIONS 2
/* Borrowed component indices into the caller's model matrix array. */
typedef struct { int ncomp; int *comp; float ridge; } THD_rsa_fitmodel;
/* Borrowed condition-to-fold assignment, with >=3 conditions on each side. */
typedef struct { int ncond, nfold; int *fold; } THD_rsa_condfold;
typedef struct THD_rsa_fitws THD_rsa_fitws;
typedef struct {
   int nresample, ncond, ngroup, maxitem, maxtri, nvalid ;
   int *nitem ;             /* [nresample] expanded positions per draw */
   int *offset ;            /* [nresample+1] offsets into index */
   int *index ;             /* concatenated expanded original-condition indices */
   unsigned char *valid ;   /* draw has at least 3 distinct original conditions */
} THD_rsa_cond_resample ;

/* The following workflows consume validated, finite, dimension-matched arrays.
   Resampling sets are caller-owned and shared unchanged across locations;
   workspaces and scratch arrays must be private to each concurrent worker.
   CI alpha is the excluded probability (0.05 for 95% intervals). Percentiles
   sort their scratch input in place. No routine writes files or exits.
   CV returns tanh(mean valid-fold Fisher z), or zero with no valid folds.
   foldz/valid have nsubject*(condition_folds ? nfold : 1) entries; meanw has
   ncomp entries. Invalid foldz entries are unspecified: consult valid first.
   Bootstrap routines retain the existing zero interval/no-draw convention.
   Regression/commonality bounds use caller-owned [effect][location] arrays;
   qbase and loc select destinations, never dataset or ROI objects.
   Ordinary draw buffers have nresample entries; seen has nobs entries. Dyadic
   workspaces must accommodate nobs and the requested regression columns.
   Regression y/x scratch holds up to NTRI(nobs) dyads per column (nmod+nort
   columns); coef has nmod*nresample entries and beta has nmod+nort entries.
   Commonality y/a/b/c scratch each holds NTRI(nobs) dyads, and draw holds
   THD_NCOMMON*nresample or THD_NCOMMON3*nresample entries respectively.
   Condition triangle y/x scratch each holds cb->maxtri entries. Condition CI
   sum/draw hold cb->nresample entries. Dual CI cval is [nsub][nresample], work
   has 3*nresample entries, and subject/condition draw counts must match.
   LOO x/y scratch has n entries, pred is contiguous [p][n], and target has p
   pointers to n observations; completed predictions are resampled without refitting.
*/
/* Allocate for this axis and maximum subject/item/component counts. Returns
   NULL for invalid/overflowing dimensions or allocation failure. Subsequent
   calls must fit those capacities; free accepts NULL. */
extern THD_rsa_fitws * THD_rsa_fitws_new( int rdm_over, int nsub, int nitem, int ncomp ) ;
/* ncomp caller scratch entries, owned by ws until it is freed. */
extern float *THD_rsa_fitws_weights(THD_rsa_fitws *ws);
extern void THD_rsa_fitws_free( THD_rsa_fitws *fw ) ;
/* perm, when supplied, maps destination model labels to source labels.
   SUBJECTS uses neural[nsub x nsub]; CONDITIONS uses srdm[nsub][NTRI(nitem)].
   cf is only for CONDITIONS; it excludes cross-boundary dyads from both sides.
   mv is the model matrix array indexed by fm->comp. Inputs remain unchanged;
   meanw, foldz and valid are optional outputs. Fisher z is clipped at +/-4. */
extern float THD_rsa_fit_cv( int rdm_over, int nsub, int nitem,
                         float *srdm, THD_simmat *neural,
                         THD_rsa_fitmodel *fm, THD_simmat **mv, int *perm,
                         THD_rsa_condfold *cf, THD_rsa_fitws *fw, float *meanw,
                         float *foldz, unsigned char *valid ) ;
/*! Population superiority for two fitted models from their identically indexed
    held-out folds.  Every outer subject contributes the mean difference over
    folds valid for BOTH models; synchronized subject-bootstrap draws then test
    the centered paired effect and supply a spatial max-null. */
extern THD_permstat THD_rsa_fit_superiority_test(
       int nsub, int nfold, float *za, unsigned char *va,
       float *zb, unsigned char *vb, THD_resample_set *rset,
       float *draw, float *nullabs ) ;
/*! Linear-interpolated percentile of x[0..n-1].  x is scratch and is sorted. */
extern float THD_rsa_percentile( float *x , int n , float prob ) ;
extern void THD_rsa_cond_resample_free( THD_rsa_cond_resample *cb ) ;
/*! Extract one condition-bootstrap draw into compact, missing-diagonal-free
    neural/model triangles.  Returns the retained dyad count. */
extern int THD_rsa_cond_boot_tri( float *neural, int nmod, THD_simmat **model,
                              THD_rsa_cond_resample *cb, int bb, float *y, float **x ) ;
extern void THD_rsa_cond_boot_ci( float *sum, int nsub, THD_rsa_cond_resample *cb,
                              int do_tanh, float alpha, float *draw,
                              float *blo, float *bhi ) ;
/*! F6 corrected two-factor bootstrap interval.  vals contains the observed
    subject effects and cval[s,b] the effect for subject s under synchronized
    condition draw b, all on one working scale (Fisher z for correlations and
    paired contrasts, standardized beta for joint regression).  Following the
    dual-bootstrap estimator, subject-only (Vs), condition-only (Vc), and
    simultaneous (Vsc) variances are combined with their finite-sample factors,
    then bounded by the corrected one-factor variances and Vsc. */
extern int THD_rsa_dual_boot_ci( int nsub, float *vals, THD_resample_set *sr,
                             THD_rsa_cond_resample *cr, float *cval, int do_tanh,
                             float alpha, float *work, float *blo, float *bhi ) ;
/*! Percentile CI for a classic-RSA group effect.  vals are the independent
    subject effects: Fisher-z correlations for an ordinary model (do_tanh=1),
    or regression coefficients for a joint model (do_tanh=0). */
extern void THD_rsa_boot_subject_mean( int nsub , float *vals , THD_resample_set *rset,
                                   int do_tanh , float alpha , float *draw,
                                   float *blo , float *bhi ) ;
/*! Percentile interval for a completed LOO prediction vector.  The fitted
    out-of-sample predictions are held fixed and subject rows (prediction plus
    observed target) are resampled together.  For a profile target, the same
    row draw is used for every measure and the reported statistic remains the
    equal-weight mean measure-wise correlation.  This quantifies uncertainty
    over the evaluated subjects, not instability from refitting every fold. */
extern int THD_rsa_boot_loo_predictions( int n, int p, float *pred, float **target,
                                     int cmp, THD_resample_set *rset,
                                     THD_rdm_ws *ws, float alpha, float *draw,
                                     int *seen, float *x, float *y,
                                     float *blo, float *bhi ) ;
/*! Percentile CI for a plain IS-RSA Mantel effect.  Resampling subjects with
    replacement duplicates rows/columns.  A pair of sampled positions that
    both refer to the same original subject is the artificial diagonal created
    by resampling, not an observed dyad, and is therefore omitted.  Other dyads
    retain their bootstrap multiplicity.  Returns the number of usable draws. */
extern int THD_rsa_boot_isrsa( THD_simmat *neural , THD_simmat *model , int cmp,
                           THD_resample_set *rset , THD_rdm_ws *ws , float alpha,
                           float *draw , int *seen , float *blo , float *bhi ) ;
/*! Percentile CI for an IS-RSA model contrast.  Each subject resample is
    applied jointly to the neural RDM and both model RDMs, so the draw is the
    paired difference r(neural,A)-r(neural,B).  As in rsa_boot_isrsa(), dyads
    between duplicate copies of one original subject are omitted. */
extern int THD_rsa_boot_isrsa_contrast( THD_simmat *neural , THD_simmat *model_a,
                                    THD_simmat *model_b , int cmp,
                                    THD_resample_set *rset , THD_rdm_ws *ws,
                                    float alpha , float *draw , int *seen,
                                    float *blo , float *bhi ) ;
/*! Build exactly nresample usable subject draws for dyadic IS-RSA.  Draws with
    fewer than three distinct subjects cannot define a correlation after the
    artificial duplicate-copy diagonals are removed, so generate a reserve and
    compact those draws once, globally, before parallel inference. */
extern THD_resample_set * THD_rsa_isrsa_resample_set( int nsub, int nresample,
                                                  long seed, int *block ) ;
/*! Paired subject-bootstrap test of equal IS-RSA model performance.  Both model
    effects are recomputed on each identical subject draw; the null is centered
    as d* - d_obs, so it permits both models to have nonzero common alignment.
    nullabs[] receives the synchronized centered |null| for max-stat FWE. */
extern THD_permstat THD_rsa_isrsa_superiority_test(
       THD_simmat *neural, THD_simmat *model_a, THD_simmat *model_b, int cmp,
       THD_resample_set *rset, THD_rdm_ws *ws, float *draw, int *seen,
       float *nullabs ) ;
/*! Percentile CIs for IS-RSA regression coefficients.  Every subject draw is
    packed into compact neural/model/nuisance triangles after duplicate-copy
    diagonal dyads are removed, then the standardized regression is refit via
    THD_tri_regress.  joint!=0 fits all reported models together; otherwise
    each reported model is fit separately with all nuisance columns. */
extern int THD_rsa_boot_isrsa_regress( THD_simmat *neural, int nmod,
                                   THD_simmat **model, int nort,
                                   THD_simmat **orts, int cmp, int joint,
                                   THD_resample_set *rset, THD_rdm_ws *ws,
                                   float alpha, float *y, float **x,
                                   float *coef, float *beta,
                                   int *seen, float **blo, float **bhi, int loc ) ;
/*! Percentile CIs for one IS-RSA commonality request.  The neural and both
    model triangles use the same subject draw, with duplicate-copy diagonal
    dyads omitted, and all raw/partial quantities are recomputed together. */
extern int THD_rsa_boot_isrsa_commonality( THD_simmat *neural, THD_simmat *model_a,
                                       THD_simmat *model_b, int cmp,
                                       THD_resample_set *rset, THD_rdm_ws *ws,
                                       float alpha, float *y, float *a, float *b,
                                       float *draw, int *seen,
                                       float **blo, float **bhi, int qbase, int loc ) ;
/*! F8 counterpart for all seven raw three-model regions and the three
    conditional partial-R2 effects. */
extern int THD_rsa_boot_isrsa_commonality3( THD_simmat *neural, THD_simmat *model_a,
                                        THD_simmat *model_b, THD_simmat *model_c,
                                        int cmp, THD_resample_set *rset,
                                        THD_rdm_ws *ws, float alpha, float *y,
                                        float *a, float *b, float *c, float *draw,
                                        int *seen, float **blo, float **bhi,
                                        int qbase, int loc ) ;
/* gidx has ncond entries in [0,ng); every group must occur. Groups are
   sampled with replacement and expanded in original condition order. Requires
   at least three groups and ten usable draws (>=3 distinct conditions).
   NULL on invalid input/allocation failure, with a diagnostic in err. */
extern THD_rsa_cond_resample * THD_rsa_cond_resample_build(int ncond, int ng, const int *gidx, int nresample, long seed, char *err, size_t errlen);


/* Borrowed, dimension-matched subject matrices: run_mat[nrun] overrides mat
   when present. No command-line labels, datasets, or file ownership. */
typedef struct {
   THD_simmat *mat;
   THD_simmat **run_mat;
} THD_rsa_run_model;

/* Run-resolved inference uses one shared subject permutation set for all runs,
   with identity in slot zero. Inputs remain unchanged. Each worker owns ws
   and all scratch/output buffers; ws is sized for nsubjects and nmod columns.
   pset may be NULL: effects are still computed and p values are -1.
   Matrices and all buffer dimensions must be validated by the caller.

   Mantel: rstat/rpval/rzscr and nge have nrun entries; ntri holds
   nrun*NTRI(nsubjects) floats. meanstat/meanp/meanz are scalars.
   cweight is [ncon][nrun]; cstat/cpval/czscr/cnge/csum have ncon entries.
   Optional null outputs: rnull[nrun][nperm], mnull[nperm], cnull[ncon][nperm].

   Regression: run outputs are [nmod][nrun], means [nmod], contrasts
   [nmod][ncon], cweight [ncon][nrun]. With permutations, mnull[nmod][nperm]
   and (when ncon>0) cnull[nmod][ncon][nperm] are required working/output
   buffers; rnull[nmod][nrun][nperm] is optional. Without permutations all
   null buffers may be NULL. Contrast buffers may be NULL when ncon==0.
   Returned nulls are absolute values, formed AFTER signed run aggregation.
   Regression returns 0 on success, nonzero on allocation/fit failure; outputs
   are unspecified on failure. Neither routine reports errors or exits.
*/
/*! One fixed or run-specific subject model against run-specific neural
    geometries. The same subject relabeling is used for every run, so a varying
    behavioral model is permuted as a whole trajectory. Individual run nulls
    and the equal-run mean null remain signed until their two-sided tail and
    max-family statistics have been formed. */
extern void THD_rsa_runresolved_mantel(
   THD_simmat **neural, int nrun, THD_simmat *model, THD_simmat **run_model, int cmp,
   PERM_set *pset, THD_rdm_ws *ws, float *ntri,
   float *rstat, float *rpval, float *rzscr,
   float *meanstat, float *meanp, float *meanz,
   float *rnull, float *mnull, int *nge,
   int ncon, float *cweight, float *cstat, float *cpval, float *czscr,
   float *cnull, int *cnge, float *csum );

/*! Joint, run-resolved standardized dyadic regression.  Every run is fit with
    the fixed and run-varying model RDMs together.  THD_rdm_regress_signed uses
    a model-specific Freedman--Lane reduced model; using the same pset in every
    run preserves each subject's whole behavioral trajectory.  Signed null
    coefficients can therefore be combined into valid equal-run means and
    planned run contrasts before two-sided testing. */
extern int THD_rsa_runresolved_regress(
   THD_simmat **neural, int nrun, const THD_rsa_run_model *mod, int nmod, int cmp,
   PERM_set *pset, THD_rdm_ws *ws,
   int ncon, float *cweight,
   float *rbeta, float *rpr, float *rpval, float *rzscr,
   float *mbeta, float *mpr, float *mpval, float *mzscr,
   float *cbeta, float *cpr, float *cpval, float *czscr,
   float *rnull, float *mnull, float *cnull );


typedef struct { float r, i; } THD_rsa_complex;
/* Numerical model indices only; presentation names remain in the command. */
typedef struct { int ia, ib; } THD_rsa_contrast;
/* Borrowed per-worker storage; this descriptor never allocates or frees it.
   Let N=subjects, T=time points, M=N*(N-1)/2. For circular shifts provide
   lagtab[M*T], prep[N*T], norm[N]; need[M*T] may be NULL (all lags) or the
   immutable shared mask of lags used by the entire shift family. For phase
   draws provide phspec[N*T], phwork[T], phseries[N*T], phsc1[T], phsc2[T].
   Unused fields may be NULL. nullmat is an allocated N x N matrix. Keeping
   buffers caller-owned preserves the command's searchlight memory accounting.
*/
typedef struct {
   unsigned char *need;
   float *lagtab, *prep, *norm;
   THD_rsa_complex *phspec, *phwork;
   float *phseries, *phsc1, *phsc2;
   THD_simmat *nullmat;
} THD_rsa_temporal_workspace;

/* Caller storage uses the dimensions above; returns 0 on success.
   prepare writes spec; draw leaves spec unchanged and writes work/series/sm. */
/*! Cache one real-series Fourier spectrum per subject. */
extern int THD_rsa_phase_prepare( int nsub, int nt, float *series, THD_rsa_complex *spec );

/* Caller storage uses the dimensions above; returns 0 on success.
   prepare writes spec; draw leaves spec unchanged and writes work/series/sm. */
/*! Build one real-valued phase surrogate and its subject similarity matrix.
    Positive-frequency bins are rotated, their negative partners are filled by
    conjugate symmetry, and DC plus the even-length Nyquist bin are unchanged.
    AFNI's interleaved FFT scaling loop does not visit every complex sample,
    so inverse normalization is applied explicitly after the transform. */
extern int THD_rsa_phase_draw( THD_phase_set *pset, int iphase, THD_rsa_complex *spec,
                           THD_rsa_complex *work, float *series, THD_simmat *sm,
                           int metric, float *sc1, float *sc2 );

/* Exactly one of tset/phset must be supplied, with identity draw zero.
   All matrices are dimension-matched; Fmain is [N][T]. Model/nuisance inputs
   stay fixed, while each surrogate rebuilds only neural geometry. ws has
   capacity for N subjects and nmod+nort regression columns. Outputs stat,
   effect, pval, zscr have nmod entries; cstat/cpval/czscr have ncon entries.
   Optional nullabs is [nmod][ndraw], cnullabs is [ncon][ndraw]. Contrasts are
   marginal correlation differences, even with joint/nuisance regression.
   tw and ws must be private to each worker; sets/models may be shared.
   Preserves existing failure semantics: preparation/allocation failure gives
   zero effects/nulls and p=1; failed phase draws use zero neural matrices.
*/
/*! Test continuous IS-RSA effects against one synchronized temporal null.
    Each draw rebuilds only the neural response matrix; fixed and
    per-location dataset models, nuisance matrices, and their standardized
    regression designs remain unrandomized.  Thus regression coefficients retain
    their usual conditional interpretation while the null asks whether the
    raw-series temporal alignment contributes any model-associated geometry.

    Identity slot 0 and all temporal surrogates feed the same primary and paired
    contrast families.  Regression designs retain their pseudoinverse across
    draws, so neither temporal null reintroduces one SVD per draw. */
extern void THD_rsa_temporal_infer( THD_simmat *neural, int nmod,
                                 THD_simmat **model, int nort,
                                 THD_simmat **orts, int joint,
                                 int ncon, const THD_rsa_contrast *con, float *Fmain,
                                 int neu_metric, int cmp,
                                 THD_timeshift_set *tset,
                                 THD_phase_set *phset, THD_rdm_ws *ws,
                                 THD_rsa_temporal_workspace *tw, float *stat,
                                 float *effect, float *pval, float *zscr,
                                 float *nullabs, float *cstat, float *cpval,
                                 float *czscr, float *cnullabs );

#endif /* _THD_RSA_HEADER_ */
