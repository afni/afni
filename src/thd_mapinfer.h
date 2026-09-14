#ifndef THD_MAPINFER_HEADER_
#define THD_MAPINFER_HEADER_

/*----------------------------------------------------------------------------
  Small, dataset-agnostic helpers shared by AFNI pattern-analysis programs.

  This interface deliberately owns only arithmetic over caller-owned arrays.
  It does not know about AFNI datasets, RSA models, classifiers, output bricks,
  OpenMP scheduling, or a program's warning/error policy.
                                                     -- P Molfese, Aug 2026
------------------------------------------------------------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

/*! Benjamini-Hochberg q-values over all n finite p-values.  p and q may alias.
    A non-positive n is a no-op. */
extern void THD_bh_fdr( int n, const float *p, float *q ) ;

/*! Benjamini-Hochberg q-values over entries for which valid[i] is nonzero.
    Invalid entries receive q=1 and do not enter the family size.  A NULL valid
    pointer declares every entry valid.  p and q may alias. */
extern void THD_bh_fdr_masked( int n, const float *p,
                               const unsigned char *valid, float *q ) ;

/*! Elementwise running maximum: dst[i] = max(dst[i],src[i]). */
extern void THD_max_accum( int n, float *dst, const float *src ) ;

/*! Generic peak-memory ledger.  Programs own the category estimates and all
    warning/refusal policy; this shared object only gives those categories a
    common representation and computes their sum. */
typedef struct {
   double input ;       /*!< resident input data                             */
   double geometry ;    /*!< neighborhoods, ROI indices, or analogous maps   */
   double shared ;      /*!< other allocations shared by all workers          */
   double output ;      /*!< concurrently resident output buffers              */
   double per_thread ;  /*!< scratch allocated once per worker                 */
   double total ;       /*!< computed peak estimate                            */
   double system ;      /*!< optional detected system memory                   */
   double limit ;       /*!< optional caller-selected enforcement limit        */
   int nthread ;        /*!< workers multiplying per_thread                    */
} THD_memory_plan ;

/*! Compute total = input + geometry + shared + output
                    + nthread * per_thread. */
extern void THD_memory_plan_finish( THD_memory_plan *plan ) ;

#ifdef __cplusplus
}
#endif

#endif /* THD_MAPINFER_HEADER_ */
