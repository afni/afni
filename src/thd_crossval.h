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

#endif /* _THD_CROSSVAL_HEADER_ */
