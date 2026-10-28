#ifndef _THD_MVPA_HEADER_
#define _THD_MVPA_HEADER_

#include "thd_crossval.h"
#include "thd_permute.h"

/*----------------------------------------------------------------------------
  Dataset-independent numerical support for multivariate prediction.  Fitted
  objects contain only statistics learned from supplied training rows;
  applying them never recomputes held-out statistics.
                                                     -- P Molfese, Oct 2026
------------------------------------------------------------------------------*/

#define THD_PREP_NONE             0
#define THD_PREP_FEATURE_ZSCORE   1
#define THD_PREP_SAMPLE_CENTER    2
#define THD_PREP_RUN_CENTER       3

#define THD_METRIC_ACCURACY              0
#define THD_METRIC_BALANCED_ACCURACY     1
#define THD_METRIC_PEARSON               2
#define THD_METRIC_SPEARMAN              3
#define THD_METRIC_CROSS_VALIDATED_R2    4
#define THD_METRIC_MAE                   5
#define THD_METRIC_RMSE                  6

#define THD_CENTROID_EUCLIDEAN           0
#define THD_CENTROID_CORRELATION         1
#define THD_CENTROID_COSINE              2

#define THD_LDA_DIAGONAL                 0
#define THD_LDA_SHRINKAGE                1
#define THD_LDA_FORM_DIAGONAL            0
#define THD_LDA_FORM_PRIMAL              1
#define THD_LDA_FORM_DUAL                2

typedef struct THD_lda_model THD_lda_model;
typedef struct THD_svm_model THD_svm_model;
typedef struct THD_nuisance THD_nuisance;

/* Native one-vs-rest linear squared-hinge SVM. Each binary problem minimizes
   .5*(||weight||^2+intercept^2) + C*sum(max(0,1-y*decision)^2).
   Intercept is penalized (constant feature = 1); no class weighting.
   Cyclic dual coordinate descent starts from zero and verifies max absolute
   projected dual gradient <= tolerance, or fails without a usable model.
   Tolerance is in [1e-12,1e-2]. Identical-pattern fits use an exact scalar reduction. No allocations during fit/predict. Raw argmax
   decisions are not probabilities; exact ties select the lowest class ID. */
extern THD_svm_model *THD_svm_alloc(int max_sample,int max_feature,int nclass,
   char *err,int nerr);
extern int THD_svm_fit(THD_svm_model *m,const float *x,const int *classes,
   int n,int p,double cost,double tolerance,int max_iterations,char *err,int nerr);
extern int THD_svm_predict(const THD_svm_model *m,const float *x,int n,int p,
   int *prediction,double *decision,char *err,int nerr);
extern void THD_svm_free(THD_svm_model *m);

/* Pooled within-class covariance uses n-nclass degrees of freedom (must be
   positive). Diagonal mode floors each variance at variance_floor*reference.
   Shrinkage mode uses (1-alpha)*S + max(alpha,variance_floor)*reference*I,
   with 0<alpha<=1. Reference is mean pooled variance; if zero, use overall
   training variance, then 1 for wholly constant data. variance_floor must be
   in [1e-12,1]. Priors are equal unless empirical_prior is nonzero. Models
   refit without allocating; shrinkage factors min(n,p) via primal/Woodbury
   solves. Decisions are centered linear discriminants, not probabilities. */
extern THD_lda_model *THD_lda_alloc(int mode, int max_sample, int max_feature,
   int nclass, char *err, int nerr);
extern int THD_lda_fit(THD_lda_model *m, const float *x, const int *classes,
   int n, int p, double alpha, double variance_floor, int empirical_prior,
   char *err, int nerr);
extern int THD_lda_predict(const THD_lda_model *m, const float *x, int n, int p,
   int *prediction, double *decision, char *err, int nerr);
extern void THD_lda_free(THD_lda_model *m);

typedef struct {
   int method , nfeature , nfit , ngroup , nconstant ;
   int nfeature_alloc , ngroup_alloc ;

   int *group_value ;       /* [ngroup], training group IDs for run_center  */
   int *group_count ;       /* [ngroup], reusable run_center counts         */
   double *center ;         /* [nfeature] or [ngroup][nfeature]             */
   double *scale ;          /* [nfeature], feature_zscore only              */
   unsigned char *constant ;/* [nfeature], zero-variance training features  */
   THD_nuisance *nuisance ; /* optional predictor adjustment before scaling */
} THD_preprocessor ;

/* Configure predictor-side OLS adjustment before the next fit. Covariates
   are borrowed [nsample][ncovariate] floats and must outlive p. Fits include
   an intercept and require full rank and ntrain > ncovariate+1 in every
   outer/inner training split. Covariate centering/scaling and slopes use
   training rows only. Numeric dummy columns are needed for categorical data;
   do not supply an intercept or all levels of a dummy-coded factor.
   No buffers are allocated during repeated fitting/application. Configure
   each active workspace's prep (ridge_cv->prep for ridge classification).
   Passing NULL covariates with ncovariate==0 disables adjustment. */
extern int THD_preprocess_set_nuisance(THD_preprocessor *p,
   const float *covariates,int nsample,int ncovariate,char *err,int nerr);
/* Validate rank/sample size in every outer and optional inner training fold.
   No neural data or outcomes enter this check. */
extern int THD_mvpa_check_nuisance(const THD_foldplan *plan,
   const THD_nested_foldplan *nested,const float *covariates,int ncovariate,
   char *err,int nerr);

typedef struct {
   int nsample , nclass , ncorrect , nclass_present ;
   double accuracy , balanced_accuracy ;
} THD_mvpa_class_score ;

typedef struct {
   int nsample ;
   double pearson , spearman , cv_r2 , mae , rmse ;
} THD_mvpa_continuous_score ;

#define THD_RIDGE_AUTO   0
#define THD_RIDGE_PRIMAL 1
#define THD_RIDGE_DUAL   2

typedef struct THD_ridge_workspace THD_ridge_workspace ;

/* Compact row-major matrices; positive lambda penalizes slopes only. Fit
   learns predictor/response means from training rows. AUTO factors the smaller
   of X'X and XX'. Factor capacity is min(max_sample,max_feature); a forced
   form must fit that capacity. Repeated fits/predictions allocate no buffers. */
extern THD_ridge_workspace *THD_ridge_alloc(int max_sample, int max_feature,
                                           int max_output, char *err, int nerr);
extern int THD_ridge_fit(THD_ridge_workspace *w, const float *x, const float *y,
                         int n, int p, int q, double lambda, int form,
                         char *err, int nerr);
extern int THD_ridge_predict(const THD_ridge_workspace *w, const float *x,
                             int n, float *prediction, char *err, int nerr);
/* Export slopes [feature][response] and unpenalized intercept [response]
   in the exact predictor space supplied to fit: y = intercept + X*coef.
   Caller provides p*q and q doubles. No preprocessing inversion is implied. */
extern int THD_ridge_coefficients(const THD_ridge_workspace *w,
   double *coef, double *intercept, char *err, int nerr);
extern void THD_ridge_free(THD_ridge_workspace *w);

typedef struct {
   const THD_foldplan *plan;
   int max_feature, max_output, preprocess;
   THD_preprocessor *prep;
   THD_ridge_workspace *ridge;
   float *train_x, *test_x, *train_y, *test_y;
   float *prediction, *baseline; /* [sample][active output]; NAN if untested */
   float *truth_scratch, *pred_scratch, *base_scratch;
   double *rank_scratch;
   int *index_scratch;
   THD_mvpa_continuous_score *score;      /* [output], pooled tested rows */
   THD_mvpa_continuous_score *fold_score; /* [fold][active output] */
   const THD_nested_foldplan *nested; /* optional borrowed tuning plan */
   int nlambda;
   double *lambda_grid, *selected_lambda; /* grid owned; selection [fold] */
   double *inner_loss; /* [outer fold][lambda], mean SSE/baseline SSE */
   int *inner_nvalid; /* [outer fold], responses with nonzero baseline SSE */
   int *inner_train, *inner_test; /* global sample index scratch */
   double *inner_sse, *inner_baseline; /* [lambda][max_output], [max_output] */
} THD_mvpa_ridge_workspace;

/* The validated borrowed plan must outlive the workspace. Active p/q may be
   smaller than allocation capacities. Results use the active q stride.
   group_id is required for run_center; held-out-only groups are rejected.
   Prediction/baseline buffers retain original sample order. */
extern THD_mvpa_ridge_workspace *THD_mvpa_ridge_workspace_alloc(
   const THD_foldplan *plan, int max_feature, int max_output, int preprocess,
   char *err, int nerr);
extern int THD_mvpa_run_ridge_cv(THD_mvpa_ridge_workspace *w,
   const float *x, int p, const float *y, int q, const int *group_id,
   double lambda, char *err, int nerr);
/* Enable nested tuning once per workspace. The nested plan must use w->plan.
   Select one lambda per outer fold by mean response-wise SSE/baseline SSE
   over pooled inner held-out rows. Exact ties, or all-constant responses,
   select the largest lambda. The run's fixed lambda argument is then unused. */
extern int THD_mvpa_ridge_set_tuning(THD_mvpa_ridge_workspace *w,
   const THD_nested_foldplan *nested, const double *lambda_grid, int nlambda,
   char *err, int nerr);
/* Shuffle complete response rows (decode) or predictor rows (encode), keeping
   folds fixed and refitting preprocessing, tuning, and models for each slot.
   permuted needs nsample*(shuffle_predictors?p:q) floats. null_score is
   [permutation][aggregate?1:q] in original metric units; undefined scores are
   NAN. Aggregate is the mean over defined response scores, as in encoding.
   Slot zero must be identity. Observed predictions/tuning are restored. */
extern int THD_mvpa_run_ridge_permutations(THD_mvpa_ridge_workspace *w,
   const float *x, int p, const float *y, int q, const int *group_id,
   double lambda, const PERM_set *pset, int shuffle_predictors,
   int metric, int aggregate, float *permuted, float *null_score,
   char *err, int nerr);
extern void THD_mvpa_ridge_workspace_free(THD_mvpa_ridge_workspace *w);

typedef struct {
   int distance , nclass , nfeature , nfit , nfeature_alloc ;
   int *class_count ;       /* [nclass]                                  */
   double *centroid ;       /* [nclass][nfeature]                         */
   double *centroid_mean ;  /* [nclass], correlation distance only       */
   double *centroid_norm ;  /* [nclass], correlation or cosine distance  */
} THD_centroid_model ;

typedef struct {
   const THD_foldplan *plan ; /* borrowed; must outlive the workspace       */
   const int *group_id ;      /* borrowed run_center groups, otherwise NULL */
   int nsample , nfold , nclass , nfeature_alloc , max_train , max_test ;
   int preprocess , ntest ;

   float *train_x , *test_x ;
   int *train_class , *test_class , *test_prediction ;
   double *test_decision ;

   THD_preprocessor *prep ;
   THD_centroid_model *centroid ;
   THD_mvpa_ridge_workspace *ridge_cv ; /* NULL unless ridge configured */
   THD_lda_model *lda ; /* NULL unless LDA configured */
   THD_svm_model *svm; /* NULL unless native linear SVM configured */
   double svm_cost,svm_tolerance;
   int svm_max_iter,svm_metric,svm_ngrid;
   const THD_nested_foldplan *svm_nested; /* borrowed */
   double *svm_grid,*svm_selected,*svm_loss; /* [candidate], [fold], [fold][candidate] */
   double *fold_svm_residual; /* maximum final KKT residual over classes */
   int *fold_svm_iterations,*svm_inner_train,*svm_inner_test;
   double lda_shrinkage, lda_variance_floor;
   int lda_empirical_prior;
   double *fold_covariance_scale, *fold_variance_floor; /* LDA [fold] */
   int *fold_covariance_form; /* THD_LDA_FORM_* [fold] */
   float *class_response ; /* [sample][class], 0/1 coding for ridge */
   double ridge_lambda ;
   int *train_count ;
   int *fold_confusion ;
   double *fold_recall ;

   int *prediction ;        /* [nsample], -1 when a row was not tested      */
   double *decision ;       /* [nsample][nclass], NAN when not tested       */
   int *fold_nsample ;      /* [nfold]                                      */
   double *fold_accuracy ;  /* [nfold]                                      */
   double *fold_balanced_accuracy ; /* [nfold]                               */
   int *confusion ;         /* [nclass][nclass], truth rows                 */
   double *recall ;         /* [nclass]                                     */
   THD_mvpa_class_score score ;
} THD_mvpa_workspace ;

/*! Allocate buffers for repeated analyses with one fold plan.  max_feature is
    the largest pattern width that will be passed to the runner.  For
    run_center, group_id is borrowed and must also outlive the workspace. */
extern THD_mvpa_workspace * THD_mvpa_workspace_alloc(
                                      const THD_foldplan *plan,
                                      int max_feature, int nclass,
                                      int preprocess, const int *group_id,
                                      char *err, int nerr ) ;

/* Configure ridge classification once, before repeated CV/permutation runs.
   Fits one 0/1 response per class, with unpenalized intercepts; argmax scores
   are not probabilities. Exact ties choose the lowest class ID. Optional
   nested tuning minimizes the existing mean class-wise SSE/baseline SSE,
   with largest-lambda ties. Every inner training set must contain all classes.
   The nested plan is borrowed and must outlive this workspace. */
extern int THD_mvpa_classification_set_ridge(THD_mvpa_workspace *w,
   double lambda, const THD_nested_foldplan *nested,
   const double *grid, int nlambda, char *err, int nerr);
extern int THD_mvpa_classification_set_lda(THD_mvpa_workspace *w, int mode,
   double alpha, double variance_floor, int empirical_prior, char *err, int nerr);
/* Optional grouped nested C selection minimizes 1-ACC/BACC over pooled inner
   held-out rows. Ties select smallest C. Every inner training set needs all
   classes. Preprocessing/tuning/fits repeat under every label permutation. */
extern int THD_mvpa_classification_set_svm(THD_mvpa_workspace *w,double cost,
   double tolerance,int max_iterations,int metric,
   const THD_nested_foldplan *nested,const double *grid,int ngrid,char *err,int nerr);
/* Default workspaces run centroid; configured workspaces run ridge, LDA or SVM.
   distance selects the centroid metric and is unused by ridge/LDA/SVM (pass zero).
   Results use the existing classification workspace fields for all estimators.
   Permutations require bijections, +1 signs and an identity in slot zero;
   preprocessing, tuning and fitting repeat, and observed results are restored. */
extern int THD_mvpa_run_classification_cv(const THD_foldplan *plan,
   int distance, const float *x, int nfeature, const int *class_id,
   const int *group_id, THD_mvpa_workspace *w, char *err, int nerr);
extern int THD_mvpa_run_classification_permutations(const THD_foldplan *plan,
   int distance, const float *x, int nfeature, const int *class_id,
   const int *group_id, int metric, const PERM_set *pset,
   int *permuted_class, float *null_score, THD_mvpa_workspace *w,
   char *err, int nerr);

/*! Run fold-local preprocessing, centroid fitting, held-out prediction, and
    classification scoring.  Results are workspace-owned and remain valid
    until the next call.  score pools all tested rows; fold scores preserve
    the inputs needed for later subject/stratum-aware aggregation.  group_id
    is required only for run_center and must match workspace allocation. */
extern int THD_mvpa_run_centroid_cv( const THD_foldplan *plan,
                                     int distance, const float *x,
                                     int nfeature, const int *class_id,
                                     const int *group_id,
                                     THD_mvpa_workspace *workspace,
                                     char *err, int nerr ) ;

/*! Repeat the complete centroid CV analysis under every synchronized label
    relabeling in pset.  null_score[pset->nperm] receives accuracy or balanced
    accuracy for each slot, including the identity in slot 0.  permuted_class
    is caller-owned scratch [plan->nsample].  On success the identity analysis
    is restored in workspace, so its predictions and fold diagnostics remain
    the observed result. */
extern int THD_mvpa_run_centroid_permutations(
                                     const THD_foldplan *plan,
                                     int distance, const float *x,
                                     int nfeature, const int *class_id,
                                     const int *group_id, int metric,
                                     const PERM_set *pset,
                                     int *permuted_class, float *null_score,
                                     THD_mvpa_workspace *workspace,
                                     char *err, int nerr ) ;

extern void THD_mvpa_workspace_free( THD_mvpa_workspace *workspace ) ;

/*! Centroid distances: euclidean, correlation, cosine. */
extern const char * THD_centroid_distance_name( int distance ) ;
extern int THD_centroid_distance_parse( const char *name, int *distance,
                                        char *err, int nerr ) ;

/*! Fit class centroids from a compact [nsample][nfeature] training matrix.
    class_id values must cover every integer in [0,nclass-1]. */
extern THD_centroid_model * THD_centroid_fit(
                                        int distance, const float *x,
                                        const int *class_id,
                                        int nsample, int nfeature, int nclass,
                                        char *err, int nerr ) ;

/*! Predict compact rows.  decision may be NULL; otherwise it receives
    [nsample][nclass] scores where larger is always better: negative squared
    Euclidean distance, Pearson correlation, or cosine similarity.  Exact
    ties resolve to the lowest class ID. */
extern int THD_centroid_predict( const THD_centroid_model *model,
                                 const float *x, int nsample, int nfeature,
                                 int *prediction, double *decision,
                                 char *err, int nerr ) ;

extern void THD_centroid_free( THD_centroid_model *model ) ;

/*! Scalar metric option names.  Per-class recall and the confusion matrix
    are diagnostics returned with classification scores, not scalar metrics. */
extern const char * THD_mvpa_metric_name( int metric ) ;
extern int THD_mvpa_metric_parse( const char *name, int *metric,
                                  char *err, int nerr ) ;

/*! Score integer class predictions.  confusion is caller-allocated
    [nclass][nclass], indexed [true class][predicted class], and recall is
    [nclass].  Recall for a class absent from truth is NAN and that class is
    excluded from balanced_accuracy. */
extern int THD_mvpa_score_classification(
                                  const int *truth, const int *prediction,
                                  int nsample, int nclass,
                                  int *confusion, double *recall,
                                  THD_mvpa_class_score *score,
                                  char *err, int nerr ) ;

/*! Score continuous predictions.  baseline[nsample] must contain predictions
    made without the held-out outcomes (normally each fold's training mean),
    so cv_r2 is 1-SSE(prediction)/SSE(baseline).  rank_scratch needs 2*nsample
    doubles and index_scratch needs 2*nsample ints.  Undefined correlations
    or cv_r2 are returned as NAN. */
extern int THD_mvpa_score_continuous(
                                  const float *truth, const float *prediction,
                                  const float *baseline, int nsample,
                                  double *rank_scratch, int *index_scratch,
                                  THD_mvpa_continuous_score *score,
                                  char *err, int nerr ) ;

/*! Exact public option names: none, feature_zscore, sample_center, run_center. */
extern const char * THD_preprocess_name( int method ) ;
extern int THD_preprocess_parse( const char *name, int *method,
                                 char *err, int nerr ) ;

/*! Fit using only fit_index[0..nfit-1].  x is [nsample][nfeature].
    group_id[nsample] is required only for run_center.  feature_zscore uses
    the population standard deviation of the training rows.  A feature that
    is constant to numerical precision in training is mapped to zero. */
extern THD_preprocessor * THD_preprocess_fit(
                                      int method, const float *x,
                                      int nsample, int nfeature,
                                      const int *fit_index, int nfit,
                                      const int *group_id,
                                      char *err, int nerr ) ;

/*! Apply a fitted transformation to selected rows.  out is caller-allocated
    [napply][nfeature], in apply_index order, and must not alias x.  run_center
    rejects a group not observed while fitting rather than estimating a mean
    from held-out samples. */
extern int THD_preprocess_apply( const THD_preprocessor *prep,
                                 const float *x, int nsample, int nfeature,
                                 const int *apply_index, int napply,
                                 const int *group_id, float *out,
                                 char *err, int nerr ) ;

/*! Fit on plan->train[fold], then transform train and test into compact caller-
    allocated matrices.  fitted may be NULL; otherwise the caller owns it. */
extern int THD_preprocess_fold( const THD_foldplan *plan, int fold, int method,
                                const float *x, int nfeature,
                                const int *group_id,
                                float *train_out, float *test_out,
                                THD_preprocessor **fitted,
                                char *err, int nerr ) ;

extern void THD_preprocess_free( THD_preprocessor *prep ) ;

#endif /* _THD_MVPA_HEADER_ */
