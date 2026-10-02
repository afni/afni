#ifndef _THD_INSYNC_HEADER_
#define _THD_INSYNC_HEADER_

/*----------------------------------------------------------------------------
  Dataset-agnostic numerical helpers for 3dInSync.

  Time-series data are subject-major: subject s begins at data+s*ntime.
  Pairwise matrices are dense, symmetric, row-major nsub x nsub arrays.
  Group labels are caller-defined nonnegative integers.
                                                     -- P Molfese, Sep 2026
------------------------------------------------------------------------------*/

#define INSYNC_SUMMARY_MEDIAN       1
#define INSYNC_SUMMARY_FISHER_MEAN  2
#define INSYNC_CORR_PEARSON          1
#define INSYNC_CORR_SPEARMAN         2

#ifdef __cplusplus
extern "C" {
#endif

/*! Summarize finite values using INSYNC_SUMMARY_*.  scratch must hold n
    floats.  Returns NaN when no finite values are present. */
extern float INSYNC_summary( int n, const float *value, int summary,
                             float *scratch, int *nvalid ) ;

/*! Summarize within-group entries from a pairwise matrix.  scratch must hold
    at least nsub*(nsub-1)/2 floats. */
extern float INSYNC_pairwise_group( int nsub, const float *corr,
                                    const int *group, int target,
                                    int summary, float *scratch,
                                    int *nvalid ) ;

/*! Summarize the dyads among nmember subject slots.  member maps each slot to
    a row/column of the nsub x nsub correlation matrix.  Pairs whose slots map
    to the same original subject are omitted; this removes the artificial
    diagonal introduced by subject bootstrap sampling. */
extern float INSYNC_pairwise_indexed( int nsub, const float *corr,
                                      int nmember, const int *member,
                                      int summary, float *scratch,
                                      int *nvalid ) ;

/*! Leave-one-out ISC within one group.  Each subject is correlated with the
    mean of the other subjects in that group.  ref must hold ntime floats and
    values must hold nsub floats. */
extern float INSYNC_loo_group( int nsub, int ntime, const float *data,
                               const int *group, int target, int summary,
                               float *ref, float *values, int *nvalid ) ;

/*! Leave-one-out ISC for an indexed set of subject slots.  Repeated indices
    are retained as repeated bootstrap observations. */
extern float INSYNC_loo_indexed( int nsub, int ntime, const float *data,
                                 int nmember, const int *member, int summary,
                                 float *ref, float *values, int *nvalid ) ;

/*! Metric-selecting LOO variants. Spearman uses average ranks for ties before
    correlating each subject with its leave-one-out group reference. */
extern float INSYNC_loo_group_metric( int nsub, int ntime, const float *data,
                                      const int *group, int target, int summary,
                                      int metric, float *ref, float *values,
                                      float *rank1, float *rank2, int *nvalid ) ;
extern float INSYNC_loo_indexed_metric( int nsub, int ntime, const float *data,
                                        int nmember, const int *member,
                                        int summary, int metric, float *ref,
                                        float *values, float *rank1,
                                        float *rank2, int *nvalid ) ;

/*! Linear-interpolated percentile.  x is scratch and is sorted in place. */
extern float INSYNC_percentile( float *x, int n, float probability ) ;

#ifdef __cplusplus
}
#endif

#endif /* _THD_INSYNC_HEADER_ */
