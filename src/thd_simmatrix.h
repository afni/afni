#ifndef _THD_SIMMATRIX_HEADER_
#define _THD_SIMMATRIX_HEADER_

#include <stddef.h>

/*----------------------------------------------------------------------------
  Square item-by-item similarity (or dissimilarity) matrices, of the kind
  Representational Similarity Analysis is built on, plus the routines that
  make them from feature vectors or from a single column of numbers.

  The type is deliberately NOT called an "RDM".  These matrices are as often
  similarities as dissimilarities -- Pearson correlation and the annak/nn
  model rules all produce similarities -- and baking the "D" into the name
  would make half the code read wrong.  Each matrix carries a flag saying
  which sense it is in, so that a program can warn when they are mixed.
                                                     -- P Molfese, Jul 2026
------------------------------------------------------------------------------*/

/*--- how a matrix is built from feature vectors ---*/
#define SIM_PEARSON   1   /* correlation            -> SIMILARITY    */
#define SIM_SPEARMAN  2   /* rank correlation       -> SIMILARITY    */
#define SIM_COSINE    3   /* cosine                 -> SIMILARITY    */
#define SIM_EUCLID    4   /* Euclidean distance     -> DISSIMILARITY */

/*--- how a matrix is built from one column of numbers ---*/
#define RUL_ANNAK    1    /* mean(rank_i,rank_j)/n     -> SIMILARITY    */
#define RUL_NN       2    /* |rank_i-rank_j| rescaled  -> SIMILARITY    */
#define RUL_EUCLID   3    /* |x_i-x_j| rescaled        -> SIMILARITY    */
#define RUL_ABSDIFF  4    /* |x_i-x_j| raw             -> DISSIMILARITY */

/*--- how two matrices are compared ---*/
#define CMP_SPEARMAN 1
#define CMP_PEARSON  2
#define CMP_KTAUB    3
#define CMP_KTAUA    4   /* Kendall tau-a: no tie correction (Nili 2014 for
                            model RDMs with many tied entries) */
#define CMP_CORR_COV 5   /* covariance-whitened Pearson RDM correlation */
#define CMP_COS_COV  6   /* covariance-whitened RDM cosine / WUC          */
#define CMP_RHOA     7   /* expected Spearman rho under random tie breaks */

#define THD_NTRI(n) ( ((n)*((n)-1))/2 )  /* strict upper-triangle entries */

typedef struct {
   int    n ;        /* number of items (rows = columns) */
   float *mat ;      /* n*n, row major, symmetric */
   int    is_dist ;  /* 1 = dissimilarity (bigger means further apart)
                        0 = similarity   (bigger means more alike)     */
   char   name[128] ;
} THD_simmat ;

/* Opaque fixed standardized triangle design.  It retains only the
   pseudoinverse needed to refit many response triangles, as in a time-shift
   null where the models stay fixed but the neural matrix changes each draw. */
typedef struct THD_tri_design THD_tri_design ;

/*-- life cycle --*/
extern THD_simmat * THD_simmat_new ( int n ) ;
extern void         THD_simmat_free( THD_simmat *sm ) ;

/*-- construction --*/

/*! From nit feature vectors of length nfeat, stored row major in F. */
extern THD_simmat * THD_simmat_from_features( int nit, int nfeat, float *F,
                                               int metric ) ;

/*! Refill an existing matrix from feature rows without reallocating it.
    For SIM_SPEARMAN, sc1/sc2 are optional nfeat-element scratch vectors; when
    omitted, temporary vectors are allocated internally.  Returns 0 on
    success. */
extern int THD_simmat_fill_from_features( THD_simmat *sm, int nfeat, float *F,
                                           int metric, float *sc1, float *sc2 ) ;

/*! Precompute every circular relative-lag similarity for each item pair.
    table is pair-major [THD_NTRI(nit)*nfeat], with lags 0..nfeat-1 and the
    standard strict-upper-triangle pair order.  Optional need[] has the same
    layout and skips lags absent from the shared draw set. prep [nit*nfeat] and
    norm[nit] are caller-owned scratch, allowing one allocation per worker.
    Pearson centers each series once; Spearman ranks and centers it once;
    cosine retains its level; Euclidean stores direct squared-difference sums.
    Returns 0 on success. */
extern int THD_simmat_lag_table( int nit, int nfeat, float *F, int metric,
                                 unsigned char *need, float *table,
                                 float *prep, float *norm ) ;

/*! Fill an existing nit x nit matrix from a pair-major circular lag table.
    offset[i] is the circular shift of item i; pair (i,j) uses
    (offset[j]-offset[i]) mod nfeat. */
extern int THD_simmat_from_lag_table( THD_simmat *sm, int nfeat,
                                      float *table, int *offset ) ;

/*! From one column of n numbers, using one of the RUL_* rules. */
extern THD_simmat * THD_simmat_from_column( int n, float *x, int rule ) ;

/*! From a multivariate behavioral profile: p columns, each of length n (cols[k]
    is the k-th measure over the n subjects).  Each column is standardized
    (z-scored across subjects) so no measure dominates by scale, then the
    subject-by-subject Euclidean distance over the p-vector is rescaled to a
    similarity.  This is the multivariate generalization of RUL_EUCLID; the
    rank-based rules (annak/nn) do not generalize to a profile. */
extern THD_simmat * THD_simmat_from_profile( int n, int p, float **cols ) ;

/*! Mahalanobis version of THD_simmat_from_profile: instead of a plain Euclidean
    distance over the z-scored measures (which double-counts correlated ones), it
    whitens by the measures' covariance, so d(i,j)^2 = (z_i - z_j)' S^-1 (z_i-z_j)
    with S the correlation matrix of the z-scored columns.  S is regularized by
    Ledoit-Wolf shrinkage toward the identity (measures may approach or exceed the
    subject count) and small eigenvalues are floored before inversion, which
    projects out redundant directions.  When the measures are uncorrelated S -> I
    and this reduces exactly to the standardized Euclidean profile.  Returns NULL
    if a column is constant or non-finite.  *shrink (if non-NULL) receives the
    shrinkage intensity used (0..1) and *erank the effective rank of S, so the
    caller can report the conditioning. */
extern THD_simmat * THD_simmat_from_profile_mahal( int n, int p, float **cols,
                                                   float *shrink, int *erank ) ;

/*! Cross-validated squared Euclidean distance ("crossnobis" with W = I) between
    ncond conditions, from nrun independent runs.  pat[r] holds run r's condition
    patterns as [ncond*nvox], laid out [condition][voxel] exactly as
    THD_roi_pattern produces.  For a condition pair (i,j) let the run contrast be
    delta_ij,r = pattern_i,r - pattern_j,r (a length-nvox vector); then

       d(i,j) = mean over ORDERED run pairs r != s of ( delta_ij,r . delta_ij,s )
                / nvox .

    Taking the two sides of each product from DIFFERENT runs makes the noise in
    one multiply independent noise in the other, so its expectation is zero: the
    estimate is UNBIASED and its true value is 0 when conditions i and j do not
    differ.  d can therefore be NEGATIVE, and callers must store, compare, output
    and plot it UNCLIPPED -- clamping at zero reintroduces exactly the positive
    bias crossnobis exists to remove.  Returns an ncond x ncond THD_simmat with
    is_dist = 1, or NULL on a bad argument. */
extern THD_simmat * THD_simmat_crossnobis( int ncond, int nrun, int nvox,
                                           float **pat ) ;

/*! F21 unbalanced counterpart.  pat[r] is in the GLOBAL ncond order and
    nrep[r][c] is positive when run r contains condition c (the caller has
    already averaged any repeated local bricks).  Each condition pair is
    averaged only over ordered pairs of runs containing BOTH conditions, with
    its own valid-pair denominator.  Every pair needs at least two valid runs.
    The balanced all-present case is numerically identical to the function
    above. */
extern THD_simmat * THD_simmat_crossnobis_valid( int ncond, int nrun, int nvox,
                                                 float **pat, int **nrep ) ;

/*--- multivariate/univariate noise normalization for crossnobis (4c) ---------*/

#define NN_NONE    0   /* no whitening: plain cross-validated Euclidean */
#define NN_DIAG    1   /* univariate: divide each voxel by its noise SD  */
#define NN_SHRINK  2   /* multivariate: whiten by a shrunk full covariance */

/*! Univariate noise-normalization weights.  R is a demeaned residual matrix,
    [nresid*nvox] laid out [time][voxel].  Fills w[nvox] with 1/sqrt(var_v), the
    per-voxel noise precision, with variances floored to the median positive
    variance so a near-silent voxel does not blow up.  Whitening a pattern is
    then the elementwise product pattern[v]*w[v]. */
extern void THD_noise_wdiag( int nvox, int nresid, float *R, float *w ) ;

/*! Multivariate noise-normalization matrix Sigma^{-1/2}, [nvox*nvox], from a
    demeaned residual matrix R [nresid*nvox] ([time][voxel]).  Sigma = R'R/nresid
    is regularized by Ledoit-Wolf shrinkage toward (trace/nvox)*I -- essential
    when the voxels approach or exceed the residual time points -- its small
    eigenvalues are floored, and Sigma^{-1/2} = V diag(1/sqrt(lambda)) V'.
    The Ledoit-Wolf numerator uses the equivalent row-norm/Frobenius identity,
    avoiding a second nresid*nvox*nvox pass over the residual outer products.
    Whitening a pattern is then the matrix-vector product Whalf * pattern.
    *shrink (0..1) and *erank report the conditioning.  Returns 0 on success. */
extern int THD_noise_whalf( int nvox, int nresid, float *R, float *Whalf,
                            float *shrink, int *erank ) ;

/*! Read a square matrix from a 1D file; errors out unless it is nexpect square
    (pass 0 to accept any size). */
extern THD_simmat * THD_simmat_read_1D( char *fname, int nexpect ) ;

/*! Write as a 1D file, via mri_write_1D so that THD_simmat_read_1D can read
    it back unchanged. */
extern int THD_simmat_write_1D( char *fname, THD_simmat *sm ) ;

/*-- triangles: the strict upper triangle is what actually gets analyzed,
     since the matrices are symmetric and the diagonal is meaningless --*/

extern void THD_simmat_to_tri     ( THD_simmat *sm, float *tri ) ;

/*! Same, but relabel the items by p[] first, applying the permutation to rows
    and columns together.  This is the operation that makes a Mantel test a
    Mantel test; shuffling triangle entries independently is the classic
    error and gives wildly anticonservative p-values. */
extern void THD_simmat_to_tri_perm( THD_simmat *sm, int *p, float *tri ) ;

/*! Rebuild a symmetric matrix from a triangle (diagonal set to 0). */
extern void THD_tri_to_simmat( int n, float *tri, THD_simmat *sm ) ;

/*! Correlate two triangles.  Both inputs are preserved.  sc1/sc2 are caller
    supplied scratch of length m (so this is safe to call from many threads). */
extern float THD_tri_corr( int m, float *a, float *b, int cmp,
                           float *sc1, float *sc2 ) ;

/*! Transform one n-condition dissimilarity triangle into the centered second-
    moment representation whose ordinary Frobenius inner product is equivalent
    to weighting the triangle by V^-1, with

             V = (C C') o (C C') .

    This is the simplified zero-distance covariance from Diedrichsen et al.
    (2021), for exchangeable condition noise (Sigma_K proportional to I).
    If remove_mean is nonzero, subtract the ordinary triangle mean first,
    yielding corr_cov; otherwise the result yields cosine_cov / WUC.  out has
    n*n floats. */
extern void THD_rdm_cov_transform( int n, float *tri, int remove_mean,
                                    float *out ) ;

/*! Cosine/Frobenius similarity of two n*n covariance-whitened transforms. */
extern float THD_rdm_cov_cosine( int n, float *a, float *b ) ;

/*! Average ranks, 1-based, ties sharing their mean rank (as scipy.rankdata). */
extern void THD_rank_avg( int n, float *x, float *rk ) ;

/*! Human-readable name of a metric / rule, for help and output labels. */
extern char * THD_simmat_metric_label( int metric ) ;
extern char * THD_simmat_rule_label  ( int rule ) ;
extern char * THD_simmat_cmp_label   ( int cmp ) ;

/*============================================================================*/
/*  RDM permutation inference  (moved from thd_mantel.c)                       */
/*                                                                            */
/*  These tests relabel the ITEMS -- a subject relabeling applied to a matrix */
/*  as rows AND columns together -- which is what makes a Mantel test valid;  */
/*  shuffling triangle entries independently is the classic error.  The       */
/*  relabelings come from a shared PERM_set (thd_permute.h), so they can       */
/*  respect exchangeability blocks and drive one common null across elements.  */
/*============================================================================*/

#include "thd_permute.h"   /* PERM_set; brings in matrix.h */

typedef struct {
   float stat ;    /* the observed statistic (a correlation, or a t) */
   float pval ;    /* two-sided permutation p; -1 if no test was run */
   float zscr ;    /* signed z equivalent of pval */
   int   nperm ;
} THD_permstat ;

/*! Per-thread scratch for the RDM tests below.  Holds NO RNG and NO permutation
    -- relabelings come from a shared PERM_set that all threads read.  ncol is
    the largest design width (models of interest PLUS -ortvec nuisances). */
typedef struct {
   int    n ;         /* items */
   int    m ;         /* triangle length = n(n-1)/2 */
   int    ncol ;      /* design columns the workspace was sized for */
   float *tri , *sc1 , *sc2 , *rprep ;
   float *yperm , *yfit , *resid , *beta ;
   float *Xmat , *Pmat ;      /* design and its pseudo-inverse */
   float *Zfit , *Zres ;      /* reduced-model fit and residuals */
   THD_simmat *ework ;        /* residuals reshaped, for relabeling */
} THD_rdm_ws ;

/*! Immutable model-side cache for repeated fixed-model Mantel tests.  It holds
    every model triangle under every relabeling in pset, centered for Pearson
    or ranked-and-centered for Spearman/rho-a, so searchlight workers only
    prepare the changing neural triangle and take dot products.  The cache does
    not own models or pset, is safe to share read-only across threads, and is
    available only for CMP_PEARSON/CMP_SPEARMAN/CMP_RHOA; unsupported metrics
    return NULL so callers can retain the ordinary exact path. */
typedef struct THD_mantel_cache THD_mantel_cache ;

extern THD_mantel_cache * THD_mantel_cache_build( int nmodel,
                                                   THD_simmat **models,
                                                   int cmp, PERM_set *pset ) ;
extern void THD_mantel_cache_free( THD_mantel_cache *cache ) ;
extern size_t THD_mantel_cache_bytes( THD_mantel_cache *cache ) ;

extern THD_rdm_ws * THD_rdm_ws_new ( int n , int ncol ) ;
extern void         THD_rdm_ws_free( THD_rdm_ws *ws ) ;

/*! Fit a standardized least-squares model directly to compact triangle
    vectors.  y and each x[c] have m entries; under CMP_SPEARMAN they are ranked
    before z-scoring, under CMP_PEARSON they are only z-scored.  This is the
    missing-entry-aware primitive used by bootstrap samples whose duplicated
    items make selected diagonal pairs invalid.  ws must have ws->m >= m and
    ws->ncol >= nmod.  Returns 1 on success, 0 for a degenerate response/design. */
extern int THD_tri_regress( int m, float *y, int nmod, float **x, int cmp,
                            THD_rdm_ws *ws, float *beta ) ;

/*! Prepare/apply a fixed standardized triangle regression design.  x has
    ncol compact triangles of length m.  Under Spearman each column is ranked;
    under Pearson it is left raw; both are z-scored before the pseudoinverse is
    retained.  apply performs the identical transform on y and returns the
    standardized coefficients without rebuilding the fixed design. */
extern THD_tri_design * THD_tri_design_new( int m, int ncol, float **x, int cmp,
                                            THD_rdm_ws *ws ) ;
extern int THD_tri_design_apply( THD_tri_design *design, float *y,
                                 THD_rdm_ws *ws, float *beta ) ;
extern void THD_tri_design_free( THD_tri_design *design ) ;

/*! Single-model Mantel test: correlate the triangles of two matrices, then
    build a null by relabeling the items of the model matrix with each
    relabeling of pset (pset = NULL skips the test, reporting the observed r).
    permnull != NULL receives the |permuted r| of each relabeling (length
    pset->nperm), so the caller can pool a max-statistic null across elements
    for an FWE correction; pass NULL when no FWE null is wanted. */
extern THD_permstat THD_mantel_corr( THD_simmat *neural , THD_simmat *model ,
                                     int cmp , PERM_set *pset , THD_rdm_ws *ws ,
                                     float *permnull ) ;

/*! Cached equivalent of THD_mantel_corr.  model must be the model used to
    build slot imod; it is supplied separately so the observed statistic still
    follows THD_tri_corr's established path exactly. */
extern THD_permstat THD_mantel_corr_cached( THD_simmat *neural,
                                            THD_simmat *model,
                                            THD_mantel_cache *cache, int imod,
                                            THD_rdm_ws *ws, float *permnull ) ;

/*! Observed paired Mantel contrast effect, independent of any null generator.
    Returns r_A-r_B and optionally stores the two component correlations. */
extern float THD_mantel_contrast_effect( THD_simmat *neural, THD_simmat *A,
                                         THD_simmat *B, int cmp, THD_rdm_ws *ws,
                                         float *rA, float *rB ) ;

/*! Paired Mantel contrast: correlate the neural triangle with model A and with
    model B, and test their DIFFERENCE r_A - r_B.  The null applies the SAME item
    relabeling to BOTH models at every draw -- the two comparisons share their
    dyads, so relabeling them independently would invent a between-model variance
    that isn't there.  Returns stat = r_A - r_B, its two-sided permutation p, and
    a signed z; permnull (length pset->nperm) receives |perm difference| for a
    pooled max-statistic FWE null (NULL to skip).  pset = NULL reports the
    observed difference with no test. */
extern THD_permstat THD_mantel_contrast( THD_simmat *neural , THD_simmat *A ,
                                         THD_simmat *B , int cmp , PERM_set *pset ,
                                         THD_rdm_ws *ws , float *permnull ) ;

/*! Cached equivalent of THD_mantel_contrast; ia/ib select the cache slots that
    correspond to A/B. */
extern THD_permstat THD_mantel_contrast_cached( THD_simmat *neural,
                                                THD_simmat *A, THD_simmat *B,
                                                THD_mantel_cache *cache,
                                                int ia, int ib,
                                                THD_rdm_ws *ws,
                                                float *permnull ) ;

/*! Wilcoxon signed-rank test of per-subject values against zero, by sign
    flipping (the rank-based, outlier-robust sibling of THD_signflip_t; used for
    classic-RSA model contrasts).  The statistic is the signed-rank sum
    W = sum_i sign(v_i) * rank(|v_i|); its null flips the signs (pset must be an
    ISE set over the n subjects).  Returns stat = W, two-sided p, signed z;
    permnull (length pset->nperm) receives |perm W| for FWE.  pset = NULL falls
    back to a normal approximation. */
extern THD_permstat THD_signrank_signflip( int n , float *v , PERM_set *pset ,
                                           float *permnull ) ;

#define THD_NCOMMON 5
#define THD_NCOMMON3 10

/*! Two-model commonality on compact triangle vectors, for missing-dyad
    resamples.  The selected metric's rank transform (Spearman) and population
    z-scoring are applied before returning {unique-A, unique-B, common,
    partial-R2-A, partial-R2-B}; common remains unclipped and may be negative.
    Returns 0 on success. */
extern int THD_tri_commonality( int m, float *y, float *a, float *b, int cmp,
                                THD_rdm_ws *ws, float *out ) ;

/*! Two-model commonality (variance partitioning) of a neural RDM.  Where the
    contrast asks "which model fits better", this asks "how does the variance the
    two models jointly explain SPLIT" -- how much is UNIQUE to A, unique to B, and
    COMMON to both.  With R2_A, R2_B the single-model fits and R2_AB the joint
    (two-predictor) fit,

        uniq_A = R2_AB - R2_B ,  uniq_B = R2_AB - R2_A ,
        common = R2_A + R2_B - R2_AB  (so uniq_A + uniq_B + common = R2_AB) .

    Also returns the two partial effect sizes

        partialR2_A = uniq_A/(1-R2_B), partialR2_B = uniq_B/(1-R2_A).

    They quantify the fraction of variance remaining after the competing model
    that the added model explains; they are not additional decomposition terms.
    A partial value is defined as zero when its reduced fit leaves no residual
    variance.

    All fits use the chosen cmp metric (triangles are rank-transformed first for
    Spearman, exactly as -model_joint).  'common' CAN BE NEGATIVE -- mutual
    suppression, where the pair explains MORE together than the sum of their
    parts -- and is reported UNCLIPPED.  Unique-A and partial-R2-A use a
    Freedman-Lane null from the reduced y~B fit: only that fit's residual RDM is
    relabeled before A+B is refit.  Unique-B/partial-R2-B analogously use y~A.
    The common component retains the complete neural-item relabeling null.  The
    models stay fixed under every null, preserving their mutual structure; each
    of the five quantities gets a two-sided empirical p.  Fills
    out[THD_NCOMMON] in the order above; pval[] and
    zscr[] too when non-NULL and pset != NULL.  permnull, if non-NULL, receives
    |perm component| laid out permnull[c*pset->nperm + pk] (length
    THD_NCOMMON*nperm) for per-quantity max-statistic FWE across locations.
    With pset = NULL only out[] is filled (the
    point estimate, e.g. per subject in classic RSA).  Returns 0 on success. */
extern int THD_commonality( THD_simmat *neural , THD_simmat *A , THD_simmat *B ,
                            int cmp , PERM_set *pset , THD_rdm_ws *ws ,
                            float *out , float *pval , float *zscr ,
                            float *permnull ) ;

/*! Group classic-RSA commonality over nsub subject condition-RDM triangles.
    srdm is [nsub*THD_NTRI(ncond)], laid out one compact triangle per subject.
    The observed statistic is the mean subject component.  Unique-A/partial-A
    use a per-subject reduced neural~B fit whose residual RDM is relabeled by
    one shared CONDITION permutation; B is analogous.  The common component
    uses the complete neural-condition relabeling.  The same permutation is
    therefore synchronized across subjects and may also be shared across map
    locations for max-statistic FWE.  cset must be an EE set over ncond items.
    subout, if non-NULL, receives observed subject components as
    subout[component*nsub + subject], for subject-bootstrap intervals.
    permnull has the same component-major layout documented for THD_commonality.
    With cset=NULL, only out/subout are computed and pval is set to -1. */
extern int THD_classic_commonality( int nsub, int ncond, float *srdm,
                                    THD_simmat *A, THD_simmat *B, int cmp,
                                    PERM_set *cset, THD_rdm_ws *ws,
                                    float *out, float *pval, float *zscr,
                                    float *permnull, float *subout ) ;

/*! Three-model commonality.  The first seven outputs are the exhaustive raw
    decomposition {unique-A, unique-B, unique-C, common-AB-not-C,
    common-AC-not-B, common-BC-not-A, common-ABC}; they sum to R2_ABC and shared
    terms remain unclipped.  The final three outputs are partial-R2 A|BC, B|AC,
    and C|AB.  Unique/partial quantities use their matching two-predictor
    reduced-model Freedman-Lane null; all four shared quantities use complete
    neural-item relabeling.  permnull is component-major [THD_NCOMMON3*nperm]. */
extern int THD_tri_commonality3( int m, float *y, float *a, float *b, float *c,
                                 int cmp, THD_rdm_ws *ws, float *out ) ;
extern int THD_commonality3( THD_simmat *neural, THD_simmat *A,
                             THD_simmat *B, THD_simmat *C, int cmp,
                             PERM_set *pset, THD_rdm_ws *ws, float *out,
                             float *pval, float *zscr, float *permnull ) ;
extern int THD_classic_commonality3( int nsub, int ncond, float *srdm,
                                     THD_simmat *A, THD_simmat *B,
                                     THD_simmat *C, int cmp, PERM_set *cset,
                                     THD_rdm_ws *ws, float *out, float *pval,
                                     float *zscr, float *permnull,
                                     float *subout ) ;

/*! Multiple-model Freedman-Lane regression over triangles.  Regresses the
    neural triangle on the model triangles of interest plus any nuisance
    ("ortvec") triangles at once; each model coefficient is its contribution
    with the others AND the nuisances held fixed.  Triangles are z-scored (and
    rank-transformed first when cmp is CMP_SPEARMAN, giving a Spearman partial).
    To test model j, y is fit by every OTHER column and only that reduced
    residual is relabeled (via pset) before refitting.  Fills beta[nmod],
    partial_r[nmod], pval[nmod]; pval = NULL or pset = NULL skips permutation.
    Pass nort = 0, orts = NULL for no nuisances.  Returns 0 on success.
    permnull != NULL receives the |permuted beta| of every model at every
    relabeling, laid out permnull[mm*pset->nperm + pk] (length nmod*nperm), for
    a pooled max-statistic FWE null; pass NULL when no FWE null is wanted. */
extern int THD_rdm_regress( THD_simmat *neural , int nmod , THD_simmat **models ,
                            int nort , THD_simmat **orts , int cmp ,
                            PERM_set *pset , THD_rdm_ws *ws ,
                            float *beta , float *partial_r , float *pval ,
                            float *permnull ) ;

/*! As THD_rdm_regress, but permnull receives the signed permuted standardized
    coefficients.  This is needed when the same synchronized relabelings are
    combined into repeated-run means or planned run contrasts. */
extern int THD_rdm_regress_signed(
                            THD_simmat *neural , int nmod , THD_simmat **models ,
                            int nort , THD_simmat **orts , int cmp ,
                            PERM_set *pset , THD_rdm_ws *ws ,
                            float *beta , float *partial_r , float *pval ,
                            float *permnull ) ;

/*! Pairwise correlations among the model triangles, into cmat[nmod*nmod]. */
extern void THD_rdm_model_corr( int nmod , THD_simmat **models , int cmp ,
                                THD_rdm_ws *ws , float *cmat ) ;

/*! Leave-one-subject-out IS-RSA prediction: predict each held-out subject's
    behavior as the neural-similarity-rank-weighted mean of the others', and
    return the cross-validated accuracy (metric cmp).  The null (pset) permutes
    which subject holds which behavior score; pset = NULL skips it.  permnull !=
    NULL receives the |permuted accuracy| of each relabeling (length
    pset->nperm) for a pooled max-statistic FWE null; NULL to skip it. */
extern THD_permstat THD_isrsa_loo( THD_simmat *neural , float *behav ,
                                   int cmp , PERM_set *pset , THD_rdm_ws *ws ,
                                   float *permnull ) ;
/*! THD_isrsa_loo with the observed held-out predictions copied to pred[n]. */
extern THD_permstat THD_isrsa_loo_pred( THD_simmat *neural , float *behav ,
                                        int cmp , PERM_set *pset ,
                                        THD_rdm_ws *ws , float *permnull ,
                                        float *pred ) ;

/*! Anna-Karenina LOO prediction.  In each fold, neural typicality (mean
    closeness to the training subjects) is regressed onto behavior using only
    the n-1 training subjects; the held subject is then predicted from their
    mean closeness to that same training set.  Distance matrices are converted
    to closeness by negation.  Inference/output follow THD_isrsa_loo. */
extern THD_permstat THD_isrsa_loo_annak( THD_simmat *neural , float *behav ,
                                         int cmp , PERM_set *pset ,
                                         THD_rdm_ws *ws , float *permnull ) ;
/*! THD_isrsa_loo_annak with observed held-out predictions in pred[n]. */
extern THD_permstat THD_isrsa_loo_annak_pred(
                                         THD_simmat *neural , float *behav ,
                                         int cmp , PERM_set *pset ,
                                         THD_rdm_ws *ws , float *permnull ,
                                         float *pred ) ;

/*! Multivariate-profile LOO prediction.  The same neural-neighbor rank weights
    predict all p measures of each held subject.  Accuracy is the equal-weight
    arithmetic mean of the p measure-wise held-out correlations (metric cmp).
    A null relabeling moves complete profile rows, preserving associations among
    measures.  Constant target measures contribute zero accuracy. */
extern THD_permstat THD_isrsa_loo_profile( THD_simmat *neural , int p ,
                                           float **behav , int cmp ,
                                           PERM_set *pset , THD_rdm_ws *ws ,
                                           float *permnull ) ;
/*! THD_isrsa_loo_profile with variable-major observed predictions pred[p*n]. */
extern THD_permstat THD_isrsa_loo_profile_pred(
                                           THD_simmat *neural , int p ,
                                           float **behav , int cmp ,
                                           PERM_set *pset , THD_rdm_ws *ws ,
                                           float *permnull , float *pred ) ;

/*! One-sample test of per-subject values against zero, by sign flipping (for
    classic within-subject RSA).  pset must be an ISE (sign-flip) set over the n
    subjects; pset = NULL falls back to a parametric two-sided t.  permnull !=
    NULL receives the |permuted t| of each relabeling (length pset->nperm) for a
    pooled max-statistic FWE null; NULL to skip it (and always when pset=NULL). */
extern THD_permstat THD_signflip_t( int n , float *v , PERM_set *pset ,
                                    float *permnull ) ;

/*! One-sample t against zero.  Returns a large sentinel rather than infinity
    when every value is identical, since that is the MOST significant case. */
extern float THD_onesamp_t( int n , float *v ) ;

/*! Two-sided p -> signed z, for display and for thresholding in the GUI. */
extern float THD_p_to_z( float p , float sign_of ) ;

#endif /* _THD_SIMMATRIX_HEADER_ */
