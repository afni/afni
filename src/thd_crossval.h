#ifndef _THD_CROSSVAL_HEADER_
#define _THD_CROSSVAL_HEADER_

#include "thd_sampleset.h"

/*----------------------------------------------------------------------------
  Dataset- and estimator-independent cross-validation folds.  A fold is an
  explicit pair of sample-index lists; samples outside both lists are excluded
  from that fold.  The latter permits within-subject CV: each subject/run fold
  trains and tests on one subject while the other subjects are absent.
                                                     -- P Molfese, Oct 2026
------------------------------------------------------------------------------*/

#define THD_CV_LEAVE_GROUP_OUT  1
#define THD_CV_GROUPED_KFOLD    2
#define THD_CV_FIXED            3

typedef struct {
   int kind , nsample , nfold , nstratum ;
   char *group_column , *within_column ;

   char **name ;             /* [nfold]                                      */
   int  *fold_stratum ;      /* [nfold], index into stratum_label             */
   char **stratum_label ;    /* [nstratum], "All" for an unstratified plan   */

   int *ntrain , *ntest ;    /* [nfold]                                      */
   int **train , **test ;    /* [nfold][ntrain/ntest] sample indexes          */

   int *sample_test_fold ;   /* [nsample], unique test fold; -1 for fixed
                                training samples                              */
} THD_foldplan ;

typedef struct {
   const THD_foldplan *outer; /* borrowed */
   THD_foldplan **inner;      /* local indexes into outer->train[fold] */
} THD_nested_foldplan;

/* k=0 leaves each group out; k>=2 balances whole groups. Every inner plan
   contains only its outer training rows, with compact local sample indexes. */
extern THD_nested_foldplan *THD_nested_foldplan_grouped(
   const THD_foldplan *outer, const int *group_id, int k, char *err, int nerr);
extern void THD_nested_foldplan_free(THD_nested_foldplan *plan);
extern void THD_nested_foldplan_print(const THD_nested_foldplan *plan,
                                     const THD_sampleset *ss, FILE *fp);

/* Shared grouped candidate search. Plans must already be validated and remain
   immutable. Candidates are arranged in nsetup batches of ncandidate values:
   setup() resets accumulators; prepare() fits shared training-only transforms;
   evaluate() fits one candidate and accumulates held-out results; finish()
   supplies a MINIMIZED pooled loss and number of valid components. prefer()
   resolves exact ties using model-specific parameter preferences. IDs are
   setup*ncandidate+candidate. Callbacks must not inspect outer-test outcomes.
   Train/test scratch each needs outer->nsample ints; losses needs the product.
   No allocation occurs in this driver. NaN loss is allowed only with nvalid=0;
   finite losses beat undefined ones. If every candidate is undefined, prefer()
   selects a deterministic fallback. Infinite or inconsistent losses fail.
   The returned winner is not fitted: caller refits on all outer training rows. */
typedef struct {
   int (*setup)(void *,int,char *,int);
   int (*prepare)(void *,int,const int *,int,const int *,int,char *,int);
   int (*evaluate)(void *,int,int,const int *,int,const int *,int,char *,int);
   int (*finish)(void *,int,int,double *,int *,char *,int);
   int (*prefer)(void *,int,int);
} THD_cv_candidate_ops;
extern int THD_nested_candidate_search(const THD_nested_foldplan *plan,int outer,
   int nsetup,int ncandidate,const THD_cv_candidate_ops *ops,void *context,
   int *train,int *test,double *losses,int *winner,int *winner_nvalid,
   char *err,int nerr);

/*! Build an unstratified leave-assigned-group-out plan without a sample table.
    test_fold[nsample] contains zero-based IDs into fold_name[nfold], whose
    order is preserved (independent of first occurrence). Every fold must
    have at least one test and one training sample; 2 <= nfold <= nsample.
    Each sample is tested exactly once; training is the complement of test.
    Both index lists preserve ascending sample order. Names must be nonempty
    and unique. All inputs are copied; free the result with THD_foldplan_free.
    Returns NULL with an error on invalid inputs or allocation failure. */
extern THD_foldplan *THD_foldplan_from_assignments(
   int nsample, int nfold, const int *test_fold, const char * const *fold_name,
   char *err, int nerr);

/*! Leave one level of group_column out.  With within_column, make independent
    folds inside every stratum (for example Run within Subj); other strata are
    excluded from each fold.  Without it, all samples share one stratum. */
extern THD_foldplan * THD_foldplan_leave_group_out(
                                      const THD_sampleset *ss,
                                      const char *group_column,
                                      const char *within_column,
                                      char *err, int nerr ) ;

/*! Assign whole groups deterministically to k size-balanced folds.  Group
    labels are never split.  With within_column, balancing and CV occur
    independently inside each stratum. */
extern THD_foldplan * THD_foldplan_grouped_kfold(
                                      const THD_sampleset *ss,
                                      const char *group_column,
                                      const char *within_column, int k,
                                      char *err, int nerr ) ;

/*! One fixed train/test split.  Every row must have exactly train_value or
    test_value in partition_column. */
extern THD_foldplan * THD_foldplan_fixed(
                                      const THD_sampleset *ss,
                                      const char *partition_column,
                                      const char *train_value,
                                      const char *test_value,
                                      char *err, int nerr ) ;

/*! Fixed partition independently within each level of within_column.
    Every stratum must contain both roles; other strata are excluded from each
    fit. NULL within_column is identical to THD_foldplan_fixed. Across fixed
    folds, every sample belongs exactly once to training OR test, never both. */
extern THD_foldplan *THD_foldplan_fixed_within(const THD_sampleset *ss,
   const char *column,const char *train_value,const char *test_value,
   const char *within_column,char *err,int nerr);

/*! Count distinct metadata keys shared by the global fixed training/test
    roles. Optional scope_column qualifies each key (e.g. Session within Subj);
    NULL compares global keys (e.g. canonical stimulus Item identities).
    Exact case-sensitive values; owned temporary state only, no data loading. */
extern int THD_foldplan_overlap_count(const THD_sampleset *ss,
   const THD_foldplan *plan,const char *column,const char *scope_column,
   int *count,char *err,int nerr);

/*! Structural validation, and classifier-specific class coverage.  Every
    class must occur in every training set; require_test also requires it in
    every test set. */
extern int THD_foldplan_validate( const THD_foldplan *plan,
                                  char *err, int nerr ) ;
extern int THD_foldplan_check_classes( const THD_foldplan *plan,
                                       const int *class_id, int nclass,
                                       int require_test,
                                       char *err, int nerr ) ;

/*! A deterministic row-level audit suitable for a future -show_folds. */
extern void THD_foldplan_print( const THD_foldplan *plan,
                                const THD_sampleset *ss, FILE *fp ) ;

extern void THD_foldplan_free( THD_foldplan *plan ) ;


#define THD_CV_BLOCK_PARTITION 1
#define THD_CV_BLOCK_STRATUM   2
typedef struct { int block, kind; } THD_cv_block_conflict;
/* Every permutation block must stay on one side of a FIXED train/test split
   and, if strata is provided, in one stratum. blocks[nsample] uses IDs in
   [0,nblock); strata[nsample] uses nonnegative IDs. Checks all rows, regardless
   of how many permutations will be sampled. Inputs are never modified.
   Returns 1/0; optional conflict identifies the first crossing in sample
   order (partition before stratum at the same row). Other failures and success
   leave conflict={-1,0}. err is a generic diagnostic; callers may use conflict
   to attach their own column and level names. */
extern int THD_foldplan_check_permutation_blocks(const THD_foldplan *plan,
   const int *blocks,int nblock,const int *strata,THD_cv_block_conflict *conflict,
   char *err,int nerr);

#endif /* _THD_CROSSVAL_HEADER_ */
