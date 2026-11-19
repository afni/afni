/* How to read this file
   Public options and dataset-independent numerical contracts. Start with
   INSYNC_options, INSYNC_run, then the kernel declarations;
   implementations are split between thd_insync.c and thd_insync_stats.c.
*/
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
#define INSYNC_MISSING_PAIRWISE 2
#define INSYNC_MISSING_INTERP 3
#define INSYNC_MISSING_LOCAL(p) ((p)==INSYNC_MISSING_PAIRWISE || (p)==INSYNC_MISSING_INTERP)
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
   int interp_maxgap;             /* explicit maximum internal gap, in TRs */
   int missing_policy ;
   int between_groups; /* opt-in descriptive cross-group pair summaries */
   int quiet ;
   int verb ;          /* stream original-time missing-subject counts to stderr */
   int loo_min_others, loo_min_others_given;
   char *counts_name;   /* optional per-component coverage/correlation TSV */
   char *missing_name; /* optional TSV of the same coverage diagnostics */
   int progress_mode ;
   int memory_override ;
   int memory_limit_given ;
   double memory_limit_gib ;

   /* Original-time selection, coverage gates, and block aggregation. */
   int check_table, window, step, step_given, tr_given, whole_summary ;
   double selection_tr ;
   char *run_starts ;
   int xmat_blocks, xmat_across_runs, xmat_threshold_given ;
   double xmat_threshold ;
   char *xmat_stims, *xmat_runs ;
   int min_valid, min_valid_given, min_valid_frac_given;
   double min_valid_frac;
   int combine_pool, combine_given, segment_center, segment_center_given;
   int block_summary, block_summary_given, block_drop_common, block_missing_given;
   int min_blocks, min_blocks_given, min_block_frac_given, block_weight_given;
   int diagnostic_draws; /* -missing_diagnostics: source-mask tests, identity included */
   int block_weight, summary_weight, summary_weight_given; /* 0 equal, 1 used TRs */
   double min_block_frac;

   /* Resampling and inference options. */
   int nboot ;
   int nperm ;
   int do_exact ;
   int tail ;
   int temporal_censor ; /* explicit joint signal/mask shifts; 0 rejects censoring */
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
   char *isc_model, *isc_qvars, *isc_centers;
   char **isc_glt_names, **isc_glt_weights;
   int isc_nglt, isc_jobs, isc_jobs_given;
   char *censor_name ;
   char *censor_column;
   int censor_column_given;
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

/* Selection kernel: one reusable workspace per worker, no evaluation-time
   allocations. Matrix entries and invalid group statistics are NaN; nvalid
   counts finite edges/LOO subjects, complete tests the within-group pair graph. */
typedef struct INSYNC_kernel INSYNC_kernel;
extern INSYNC_kernel *INSYNC_kernel_new(int,int);
extern void INSYNC_kernel_free(INSYNC_kernel *);
extern void INSYNC_kernel_eval(INSYNC_kernel *,int,const float *,int,int,int,
                               int,const int *,int **,float *,int *,int *);
extern const float *INSYNC_kernel_matrix(const INSYNC_kernel *);
extern const float *INSYNC_kernel_loo(const INSYNC_kernel *);
/* Caller supplies a single admitted block set for all required components.
   blocks is block-major: nsub*nsub pair matrices or nsub LOO values. */
extern void INSYNC_kernel_aggregate(INSYNC_kernel *,int,const float *,const unsigned char *,
   int,int,int,int,const int *,int **,float *,int *,int *);
extern void INSYNC_kernel_fisher_matrix(const INSYNC_kernel *,float *);

/* Pairwise-missing estimator. A component stores raw overlap even when its
   correlation is invalid. used/blocks are zero on failure; eligible counts
   blocks with finite correlations before aggregate gates. min_others is the
   minimum LOO contributor count on raw usable TRs (zero when none exist).
   Arrays contain an nsub*nsub symmetric pair matrix followed by nsub LOO
   components; diagonals have no correlation. overlap/used are real observations;
   samples/used_samples include fills (raw/admitted respectively). Counts are
   never independent degrees of freedom. */
typedef struct {
   float r;
   int overlap, used, eligible, blocks, min_others;
   int samples, used_samples; /* augmented overlap; never used as real coverage */
} INSYNC_component;
typedef struct INSYNC_missing INSYNC_missing;
/* Allocate worker-private storage; return NULL on invalid size/allocation. */
extern INSYNC_missing *INSYNC_missing_new(int nsub,int capacity);
extern void INSYNC_missing_free(INSYNC_missing *work);
/* data is subject-major with nt samples, already source-censored to NaN.
   piece optionally identifies contiguous segments for overlap-specific mean
   centering. The nominal denominator includes censored/nonfinite TRs.
   Group membership may repeat original indices for subject bootstrap. LOO
   excludes ALL copies of the held original subject, retains other multiplicities,
   and gates on the count of distinct valid other people.
   Input arrays/options are borrowed. No evaluation-time allocation occurs. */
/* Fill only internal gaps <= maxgap, independently within each run. keep is
   optional original-time validity; censored/nonfinite samples become NaN.
   filled[nt] is cleared then marks synthetic values. Validate before mutation;
   return filled count, or -1 for invalid arguments. No extrapolation occurs. */
extern int INSYNC_interp_linear(int nt,float *data,const unsigned char *keep,
   int nrun,const int *runstart,int maxgap,unsigned char *filled);
/* As eval below, with subject-major original-observation flags. NULL means
   every finite value is observed. Gates/weights count real samples; r includes
   fills. A LOO TR is real only when held value AND all mean contributors are
   observed. samples/used_samples retain augmented coverage separately. */
extern void INSYNC_missing_eval_observed(INSYNC_missing *work,int nt,const float *data,
   const unsigned char *observed,const int *piece,int nominal,
   const INSYNC_options *options,int ngroup,const int *gcount,int **gmember);
extern void INSYNC_missing_eval(INSYNC_missing *work,int nt,const float *data,
   const int *piece,int nominal,const INSYNC_options *options,int ngroup,
   const int *gcount,int **gmember);
/* Aggregate block-major component arrays, applying the selected block policy
   and gates per component. Subject availability counts remain caller-owned. */
extern void INSYNC_missing_aggregate(INSYNC_missing *work,int nblock,
   const INSYNC_component *block,const INSYNC_options *options,int ngroup,
   const int *gcount,int **gmember);
/* Borrowed results valid until the next evaluation/aggregation/free. */
extern const INSYNC_component *INSYNC_missing_components(const INSYNC_missing *work);
extern const int *INSYNC_missing_subjects(const INSYNC_missing *work);
/* -between_groups: summarize the rectangular A x B portion of the existing
   pair matrix, even when within-group summaries use LOO. Groups must be
   nonempty/disjoint, with valid original-subject indices. Borrowed arrays;
   worker scratch is overwritten, component estimates remain unchanged.
   out = {r, finite_pairs, valid, complete, min_raw_overlap, min_used_overlap}.
   Minima include failed pairs; r is NaN when no pair qualifies. Counts are
   real observations, not independent df. Block admission is inherited from
   the existing components, including within-group drop_common requirements. */
extern void INSYNC_missing_between(INSYNC_missing *work,int na,const int *a,
   int nb,const int *b,int summary,float out[6]);
/* Summarize finite components; complete always describes the pair graph.
   summary_weight is taken from the options used by the last eval/aggregate.
   Repeated memberships preserve multiplicity, omitting dyadic self-pairs. */
extern void INSYNC_missing_summary(INSYNC_missing *work,int method,int summary,
   int ngroup,const int *gcount,int **gmember,float *stat,int *nvalid,int *complete);

/*! Linear-interpolated percentile.  x is scratch and is sorted in place. */
extern float INSYNC_percentile( float *x, int n, float probability ) ;

#ifdef __cplusplus
}
#endif

#endif /* _THD_INSYNC_HEADER_ */
