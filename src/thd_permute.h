#ifndef THD_PERMUTE_HEADER_
#define THD_PERMUTE_HEADER_

/*----------------------------------------------------------------------------
  thd_permute.h -- shared permutation-inference machinery for AFNI programs.

  See thd_permute.c for the algorithm descriptions and literature citations.

  This header only needs matrix.h.  It deliberately does not depend on
  mrilib.h, so it may be included either before or after mrilib.h.  For that
  reason masks are typed "unsigned char" rather than AFNI's "byte" typedef;
  the two are identical.
------------------------------------------------------------------------------*/

#include "matrix.h"   /* double-precision matrix/vector from matrix.c */

#ifdef __cplusplus
extern "C" {
#endif

/*--------------------------------------------------------------------------*/
/*! Which relabelings of the data leave the null hypothesis unchanged.

    PERM_EE  ("exchangeable errors")           -> shuffle observations.
    PERM_ISE ("independent symmetric errors")  -> flip signs of observations.
    PERM_BOTH                                  -> both, as a product set.

    Terminology follows Winkler et al. (2014); see thd_permute.c.
*//*------------------------------------------------------------------------*/

#define PERM_EE     1
#define PERM_ISE    2
#define PERM_BOTH   3

/*--------------------------------------------------------------------------*/
/*! How exchangeability blocks restrict the relabelings.

    PERM_WITHIN_BLOCK   observations move only inside their own block, and
                        sign flips are independent per observation.
    PERM_WHOLE_BLOCK    whole blocks trade places as rigid units (all blocks
                        must then have the same size), and one sign flip is
                        drawn per block and applied to all of its members.
    PERM_BOTH_BLOCK     blocks trade places AND observations are shuffled
                        inside them; sign flips are per observation.
*//*------------------------------------------------------------------------*/

#define PERM_WITHIN_BLOCK  0
#define PERM_WHOLE_BLOCK   1
#define PERM_BOTH_BLOCK    2

/*! Tail conventions for turning a statistic into a p-value. */

#define PERM_TAIL_TWO  0   /* |stat| is compared                 */
#define PERM_TAIL_ONE  1   /* stat is compared (upper tail only) */

/*! Returned by THD_perm_group_size() when the count exceeds INT_MAX. */

#define PERM_COUNT_TOOBIG (-1LL)

/*--------------------------------------------------------------------------*/
/*! Description of the exchangeability structure of a design.

    Build with THD_perm_scheme_new(), decorate with the setter functions,
    then hand to THD_perm_set_build().  All arrays are copied, so the
    caller keeps ownership of whatever it passes in.
*//*------------------------------------------------------------------------*/

typedef struct {
   int nobs ;         /*!< number of observations (rows of the design)      */
   int exchange ;     /*!< PERM_EE / PERM_ISE / PERM_BOTH                   */
   int blockmode ;    /*!< PERM_WITHIN_BLOCK / WHOLE / BOTH                 */
   int exact ;        /*!< 1 = enumerate the whole group if it is feasible  */
   int nperm ;        /*!< number of relabelings wanted when not exact      */
   long seed ;        /*!< seed for the random generator                    */

   int  nblock ;      /*!< number of exchangeability blocks (>= 1)          */
   int *block ;       /*!< [nobs] block index of each observation, 0-based  */

   int *eqclass ;     /*!< [nobs] equivalence-class label, or NULL.  Two
                           observations sharing a label are interchangeable
                           as far as the statistic is concerned, so exact
                           enumeration visits each *distinct* relabeling
                           once instead of once per redundant copy.

                           CONTRACT: two observations may share a class ONLY if
                           swapping them cannot change the statistic -- in a GLM
                           that means IDENTICAL design rows (e.g. same group in
                           a two-sample test).  Label observations with
                           different design rows the same and the null is
                           silently wrong: distinct relabelings get collapsed as
                           if redundant when they are not.  When in doubt, leave
                           this NULL (every relabeling enumerated).           */
} PERM_scheme ;

/*--------------------------------------------------------------------------*/
/*! A materialized set of relabelings.

    Relabeling ip maps destination slot i to source observation
    perm[ip*nobs+i], with its value multiplied by sign[ip*nobs+i].
*//*------------------------------------------------------------------------*/

typedef struct {
   int nperm ;           /*!< number of relabelings held here               */
   int nobs ;            /*!< observations per relabeling                   */
   int is_exact ;        /*!< 1 if this is the complete group               */
   int *perm ;           /*!< [nperm*nobs] source index per slot            */
   signed char *sign ;   /*!< [nperm*nobs] +1/-1 per slot, never NULL       */
} PERM_set ;

/*--------------------------------------------------------------------------*/
/*! A materialized set of ordinary subject-bootstrap samples.

    This is deliberately a different type from PERM_set.  A permutation set
    represents a null hypothesis and contains one-to-one relabelings; a
    resample set represents sampling uncertainty and draws observations with
    replacement.  Keeping the two objects separate prevents bootstrap draws
    from accidentally entering a permutation null (or vice versa).

    Sample ir maps destination slot i to source observation
    index[ir*nobs+i].  The same immutable set can be shared across threads and
    across every voxel in a map.
*//*------------------------------------------------------------------------*/

typedef struct {
   int nresample ;        /*!< number of bootstrap samples held here         */
   int nobs ;             /*!< observations per sample                       */
   int *index ;           /*!< [nresample*nobs] source index per slot         */
} THD_resample_set ;

/*--------------------------------------------------------------------------*/
/*! A materialized set of circular time shifts for multi-subject series.

    Slot 0 is the all-zero identity, so the observed statistic is a member of
    the Monte Carlo set and empirical p-values cannot be zero.  Every later
    slot independently shifts each subject by an offset whose circular
    distance from zero is at least min_shift.  The immutable offsets can be
    shared across all ROIs/searchlights, which is required for a synchronized
    max-statistic null and thread-count reproducibility.
*//*------------------------------------------------------------------------*/

typedef struct {
   int nshift ;           /*!< number of sets, including identity slot 0     */
   int nobs ;             /*!< subjects per shift set                         */
   int ntime ;            /*!< time points in every subject series            */
   int min_shift ;        /*!< minimum circular distance from zero, in TRs   */
   int *offset ;          /*!< [nshift*nobs], offsets in 0..ntime-1           */
} THD_timeshift_set ;

/*--------------------------------------------------------------------------*/
/*! A deterministic phase-randomization family for real-valued series.

    Slot 0 is the identity.  Later slots assign an independent uniform phase
    to every subject and positive-frequency Fourier bin.  The phases are
    generated statelessly from (seed, slot, subject, frequency), so the set is
    immutable, consumes constant memory, and can be shared across threads and
    spatial locations for synchronized max-statistic inference.
*//*------------------------------------------------------------------------*/

typedef struct {
   int nphase ;           /*!< number of sets, including identity slot 0     */
   int nobs ;             /*!< subjects per set                               */
   int ntime ;            /*!< samples in every real-valued series            */
   int nfreq ;            /*!< randomized positive bins: (ntime-1)/2         */
   unsigned long long stream ; /*!< seed after the phase-null stream separator */
} THD_phase_set ;

/*--------------------------------------------------------------------------*/
/*! Everything about a general linear model that does not vary voxel to
    voxel.  Built once by THD_perm_glm_setup() and reused for every element
    and every relabeling.
*//*------------------------------------------------------------------------*/

typedef struct {
   int nobs ;        /*!< rows of the design                                */
   int nreg ;        /*!< columns of X, the regressors of interest          */
   int nnuis ;       /*!< columns of Z, the nuisance regressors             */
   int npar ;        /*!< nreg + nnuis                                      */
   int ncon ;        /*!< rows of the contrast, i.e. its rank               */
   int rank ;        /*!< numerical rank of the full design                 */
   int dof ;         /*!< nobs - rank                                       */
   int is_ftest ;    /*!< 1 when ncon > 1                                   */

   matrix M ;        /*!< [nobs x npar] full design, X columns then Z       */
   matrix Z ;        /*!< [nobs x nnuis] nuisance-only design               */
   matrix pinvM ;    /*!< [npar x nobs] pseudo-inverse of M                 */
   matrix pinvZ ;    /*!< [nnuis x nobs] pseudo-inverse of Z                */
   matrix Ct ;       /*!< [ncon x npar] contrast, one row per constraint    */
   matrix cmc_inv ;  /*!< [ncon x ncon] inv( Ct (M'M)^+ Ct' )               */
   double cvar ;     /*!< the 1x1 case of cmc_inv, inverted back            */
} PERM_glm ;

/*--------------------------------------------------------------------------*/
/*! Observed statistics, the max-statistic null, and the resulting p-values. */

typedef struct {
   int nelem ;        /*!< number of elements (voxels) tested               */
   int nperm ;        /*!< relabelings used                                 */
   int dof ;          /*!< residual degrees of freedom, or -1 if undefined  */
   int tail ;         /*!< PERM_TAIL_TWO / PERM_TAIL_ONE                    */
   int is_ftest ;     /*!< 1 when the statistic is one-sided by nature      */

   float *stat ;      /*!< [nelem] observed statistic                       */
   float *effect ;    /*!< [nelem] observed effect size, or NULL            */
   float *p_unc ;     /*!< [nelem] uncorrected empirical p-value            */
   float *p_fwe ;     /*!< [nelem] max-statistic FWE-corrected p-value      */
   float *z_unc ;     /*!< [nelem] signed z equivalent of p_unc             */
   float *z_fwe ;     /*!< [nelem] signed z equivalent of p_fwe             */
   float *max_null ;  /*!< [nperm] max statistic per relabeling, sorted     */
   int   *cnt_unc ;   /*!< [nelem] exceedance counts behind p_unc           */
} PERM_result ;

/*--------------------------------------------------------------------------*/
/*! Statistic for the generic (non-GLM) permutation driver.

    yperm holds the relabeled data for one element; ud is the caller's
    context.  Store the effect size through effect when it is not NULL.
*//*------------------------------------------------------------------------*/

typedef float (*PERM_statfunc)( float *yperm, int nobs, float *effect, void *ud ) ;

/*! Progress hook, called once per element as the drivers sweep over them
    (they loop elements on the outside and relabelings on the inside, so that
    the per-element model fit happens once).  May be NULL. */

typedef void (*PERM_progfunc)( int done, int total, void *ud ) ;

/*--------------------------------------------------------------------------*/
/* Exchangeability scheme                                                   */
/*--------------------------------------------------------------------------*/

extern PERM_scheme * THD_perm_scheme_new ( int nobs ) ;
extern void          THD_perm_scheme_free( PERM_scheme *ps ) ;

/*! Assign block labels.  blk is [nobs] of arbitrary integer labels; they are
    renumbered internally to 0..nblock-1 in order of first appearance.
    Passing NULL restores the single-block default.  Returns nblock, or 0 on
    a bad argument. */

extern int THD_perm_scheme_set_blocks ( PERM_scheme *ps, int *blk ) ;

/*! Assign equivalence-class labels; see PERM_scheme.eqclass.  NULL clears. */

extern int THD_perm_scheme_set_eqclass( PERM_scheme *ps, int *cls ) ;

/*! Size of the relabeling group implied by the scheme, or PERM_COUNT_TOOBIG
    when it does not fit in an int. */

extern long long THD_perm_group_size( PERM_scheme *ps ) ;

/*--------------------------------------------------------------------------*/
/* Relabeling sets                                                          */
/*--------------------------------------------------------------------------*/

/*! Materialize the relabelings a scheme describes.

    When ps->exact is set and the group is small enough to walk, the result is
    the complete group and ps->nperm is ignored.  The same happens when
    ps->nperm is at least the size of the group, since enumerating it is then
    both cheaper and exact.  Otherwise ps->nperm relabelings are drawn at
    random, with the identity in slot 0.  Either way
    the identity is a member, so an empirical p-value always counts the
    observed statistic among the null values and can never be zero.

    Costs nperm*nobs ints plus nperm*nobs bytes.  Free with
    THD_perm_set_free(). */

extern PERM_set * THD_perm_set_build( PERM_scheme *ps ) ;
extern void       THD_perm_set_free ( PERM_set *pset ) ;

/*! Apply relabeling ip of pset to yin, writing nobs values to yout. */

extern void THD_perm_set_apply( PERM_set *pset, int ip, float *yin, float *yout ) ;

/*! Materialize nresample ordinary bootstrap samples of nobs observations.
    Every slot is drawn independently and uniformly with replacement.  The
    generator is local to this builder (not the permutation RNG), and the
    resulting set is read-only/thread-safe. */

extern THD_resample_set * THD_resample_set_build( int nobs, int nresample,
                                                   long seed ) ;
/*! Materialize a stratified subject bootstrap.  Destination observation i is
    sampled with replacement only from observations sharing block[i].  Thus
    every draw preserves each stratum's original sample size.  Block labels
    may be arbitrary integers.  This is not a whole-cluster bootstrap. */
extern THD_resample_set * THD_resample_set_build_stratified(
                                                   int nobs, int nresample,
                                                   long seed, int *block ) ;
extern void THD_resample_set_free( THD_resample_set *rset ) ;

/*! Materialize circular-shift sets for equal-length, gap-free time series.
    Requires min_shift >= 1 and at least two permitted nonzero offsets. */

extern THD_timeshift_set * THD_timeshift_set_build( int nobs, int ntime,
                                                     int nshift, int min_shift,
                                                     long seed ) ;
extern void THD_timeshift_set_free( THD_timeshift_set *tset ) ;

/*! Build a constant-memory phase family and return one unit-complex phase.
    freq is a positive Fourier-bin index in 1..nfreq.  DC and the even-length
    Nyquist bin are deliberately outside this API and remain unchanged. */

extern THD_phase_set * THD_phase_set_build( int nobs, int ntime,
                                             int nphase, long seed ) ;
extern void THD_phase_set_factor( const THD_phase_set *pset, int iphase,
                                  int iobs, int freq, float *co, float *si ) ;
extern void THD_phase_set_free( THD_phase_set *pset ) ;

/*! Reentrant Fisher-Yates shuffle of arr[0..n-1], using the caller's 48-bit
    RNG state xs[3] (as for nrand48).  Thread-safe: each thread passes its own
    state.  A materialized PERM_set is usually the better source of relabelings
    -- it can enumerate small groups exactly, respects blocks, and lets the
    same set drive max-statistic FWE -- but this is here for the simple case of
    an unrestricted shuffle with no blocks and no FWE. */

extern void THD_perm_shuffle_r( int *arr, int n, unsigned short xs[3] ) ;

/*--------------------------------------------------------------------------*/
/* General linear model setup                                               */
/*--------------------------------------------------------------------------*/

/*! Bundle a partitioned design for the Freedman-Lane driver.

    X   regressors of interest, [nobs x nreg].
    Z   nuisance regressors, [nobs x nnuis]; pass a matrix that has been
        through matrix_initialize() and nothing else when there are none.
    Ct  contrast with one row per constraint and npar = nreg+nnuis columns,
        or NULL for the default "all of X jointly" contrast [ I | 0 ].

    Returns NULL if the design is unusable.  Free with THD_perm_glm_free(). */

extern PERM_glm * THD_perm_glm_setup( matrix X, matrix Z, matrix *Ct ) ;
extern void       THD_perm_glm_free ( PERM_glm *glm ) ;

/*! Split a full design M and contrast Ct into effect-of-interest and
    nuisance partitions, so that testing Ct in M is equivalent to testing all
    of X in [X Z].  This is the partitioning of Beckmann et al. as used by
    PALM (Winkler et al. 2014, section 2.3).  X and Z must have been passed
    through matrix_initialize() first.  Returns 1 on success. */

extern int THD_perm_partition_design( matrix M, matrix Ct, matrix *X, matrix *Z ) ;

/*--------------------------------------------------------------------------*/
/* Permutation drivers                                                      */
/*--------------------------------------------------------------------------*/

/*! Freedman-Lane permutation test of the contrast in glm.

    data[i] points to nelem values for observation i (an AFNI sub-brick, for
    instance).  mask may be NULL; elements outside it are left at neutral
    values.  tail is ignored for F-contrasts, which are always upper-tailed. */

extern PERM_result * THD_permute_freedman_lane(
                        float **data, int nobs, int nelem, unsigned char *mask,
                        PERM_glm *glm, PERM_set *pset, int tail,
                        PERM_progfunc pfunc, void *pdata ) ;

/*! Permutation test around a caller-supplied statistic.  Same data layout as
    THD_permute_freedman_lane(); sfunc sees each relabeled data vector. */

extern PERM_result * THD_permute_generic(
                        float **data, int nobs, int nelem, unsigned char *mask,
                        PERM_set *pset, PERM_statfunc sfunc, void *sdata,
                        int tail, PERM_progfunc pfunc, void *pdata ) ;

/*--------------------------------------------------------------------------*/
/* Result handling                                                          */
/*--------------------------------------------------------------------------*/

extern PERM_result * THD_perm_result_new ( int nelem, int nperm ) ;
extern void          THD_perm_result_free( PERM_result *pr ) ;

/*! Turn accumulated counts and the max-statistic null into p and z bricks.
    Called for you by the drivers; exposed for callers that accumulate their
    own counts. */

extern void THD_perm_result_finish( PERM_result *pr, unsigned char *mask ) ;

/*! Upper-tail empirical p-value of obs against an ascending sorted null. */

extern float THD_perm_emp_pvalue( float *sorted, int nperm, float obs ) ;

/*! Signed z-score matching an empirical p-value, for FIZT-tagged output. */

extern float THD_perm_signed_z( float pval, float observed_stat, int tail ) ;

#ifdef __cplusplus
}
#endif

#endif /* THD_PERMUTE_HEADER_ */
