#ifndef _THD_INSYNC_HEADER_
#define _THD_INSYNC_HEADER_

#include "thd_datatable.h"

/*----------------------------------------------------------------------------
  Analysis driver and dataset-agnostic numerical helpers for 3dInSync.
  The numerical helpers are implemented separately in thd_insync_stats.c.

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

/* Dataset-analysis options. Strings and the table passed to INSYNC_run are
   borrowed from the caller and must remain valid for the entire run. */
#define INSYNC_METHOD_PAIRWISE 1
#define INSYNC_METHOD_LOO      2
#define INSYNC_TNULL_NONE      0
#define INSYNC_TNULL_TIMESHIFT 1
#define INSYNC_TNULL_PHASE     2
#define INSYNC_MISSING_ERROR   0
#define INSYNC_MISSING_COMMON  1
#define INSYNC_PROGRESS_AUTO   0
#define INSYNC_PROGRESS_BAR    1
#define INSYNC_PROGRESS_LINE   2
#define INSYNC_PROGRESS_OFF    3
#define INSYNC_TAIL_BI  2

typedef struct {
   int edges ;         /* -edges: ISC of every ROI-pair edge series           */
   int rss ;           /* -edges_rss: ISC of global cofluctuation amplitude   */
   int events ;        /* -edges_events: shared high-amplitude frames         */
   int matrix ;        /* -edges_matrix: also write .netcc matrices           */
   float event_frac ;  /* fraction of frames called "high" in each subject    */
   int nnull ;         /* -edges_nnull: surrogate sets, identity slot included */
   int min_shift ;     /* minimum circular distance of a null shift           */
   long seed ;
} INSYNC_edge_opts ;

typedef struct {
   /* Core analysis and reporting options. */
   int method ;
   int summary ;
   int show_table ;
   int corr_metric ;
   int missing_policy ;
   int quiet ;
   int progress_mode ;
   int memory_override ;
   int memory_limit_given ;
   double memory_limit_gib ;

   /* Resampling and inference options. */
   int nboot ;
   int nperm ;
   int do_exact ;
   int tail ;
   int temporal_mode ;
   int nnull ;
   int min_shift ;
   int min_shift_given ;
   int temporal_tail ;
   int temporal_tail_given ;
   int ncondperm ;
   int condition_exact ;
   int condition_tail ;
   long seed ;
   float boot_alpha ;

   /* Input column names, contrasts, and optional output names. */
   int condition_column_given ;
   int have_contrast ;
   char *prefix ;
   char *maskname ;
   char *group_column ;
   char *condition_column ;
   char *contrast_name0 ;
   char *contrast_name1 ;
   char *pair_prefix ;
   char *censor_name ;
   int do_zcensor ;
   int zcensor_warn_given ;
   float zcensor_warn ;
   char *atlas_name ;
   char *roi_sel ;
   char *matrix_name ;
   int roi_only ;
   int use_half ;
   INSYNC_edge_opts eo ;
} INSYNC_options ;

/*! Initialize all options to the 3dInSync command-line defaults. */
extern void INSYNC_options_default( INSYNC_options *opts ) ;

/*! One-shot AFNI dataset driver: validate options/table, load input, estimate
    ISC and null models, and write requested outputs. argc/argv are used only
    for output history. The caller performs AFNI process initialization.
    Like an AFNI command, failures call ERROR_exit and some analysis resources
    live until process exit; this is not a repeated-call, in-memory API.
    The caller's options and table are not modified or freed. */
extern int INSYNC_run( const INSYNC_options *opts, THD_datatable *tab,
                       int argc, char **argv ) ;

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
