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

/* Inner-CV objectives are separate from outer reporting/inference metrics. */
#define THD_TUNE_DEFAULT          0
#define THD_TUNE_NORMALIZED_MSE   1
#define THD_TUNE_ACCURACY         2
#define THD_TUNE_BALANCED_ACCURACY 3
#define THD_TUNE_MSE              4
#define THD_TUNE_RMSE             5
#define THD_TUNE_MAE              6
#define THD_TUNE_LOG_LOSS         7
#define THD_TUNE_BRIER            8
extern const char *THD_mvpa_tune_objective_name(int objective);
extern int THD_mvpa_tune_objective_parse(const char *name,int *objective,char *err,int nerr);
extern int THD_mvpa_tune_objective_resolve(int requested,int classification,
   int ridge,int score_metric,int *objective,char *err,int nerr);

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
typedef struct THD_logistic_model THD_logistic_model;
/* Symmetric K-class softmax (including binary): mean natural-log loss plus
   lambda/2 * sum of squared weights; intercepts are unpenalized. Class IDs
   are 0..K-1 and every training class must be present. Deterministic L-BFGS
   starts from zero weights / empirical-prior intercepts. Fits require a
   verified maximum absolute gradient <= tolerance (1e-12..1e-2).
   No allocations during fit/predict; exact argmax ties use lowest class ID.
   Failed fits invalidate the model. Decision outputs are logits, not odds. */
extern THD_logistic_model *THD_logistic_alloc(int max_sample,int max_feature,
   int nclass,char *err,int nerr);
extern void THD_logistic_free(THD_logistic_model *m);
extern int THD_logistic_fit(THD_logistic_model *m,const float *x,const int *classes,
   int n,int p,double lambda,double tolerance,int max_iterations,char *err,int nerr);
extern int THD_logistic_predict(const THD_logistic_model *m,const float *x,int n,int p,
   int *prediction,double *decision,double *probability,char *err,int nerr);
/* Stable losses from logits. Brier sums all K squared probability errors;
   range [0,2], including binary (no division by K or binary rescaling). */
extern int THD_mvpa_probability_score(const double *logits,int nclass,int truth,
   double *log_loss,double *brier,char *err,int nerr);
extern int THD_mvpa_logistic_objective(int requested,int *objective,char *err,int nerr);

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

/* Descriptive comparison of saved held-out predictions. Keys are exact text:
   (unit_type, unit, response, sample), unique within each input. All strings
   must be non-NULL and remain alive through result use. Row order may differ;
   sample sets, subject/run/item/input_fold, fold/stratum, tested status and
   observed outcomes must match, including training-only rows. Continuous
   tested rows require identical finite baselines (a common CV-R2 denominator).
   Class IDs use one shared label dictionary; predictions need not be scores.
   No fitting, missing-row intersection, permutation or population inference.
   This validates supplied identities, not original datasets or training fits. */
typedef struct {
   const char *unit_type,*unit,*response,*sample,*subject,*run,*item;
   const char *input_fold,*fold,*stratum;
   int tested,truth_class,prediction_class;
   float observed,predicted,baseline;
} THD_mvpa_pair_row;

#define THD_MVPA_PAIR_POOLED  0
#define THD_MVPA_PAIR_FOLD    1
#define THD_MVPA_PAIR_SUBJECT 2
#define THD_MVPA_PAIR_STRATUM 3
typedef struct {
   const char *unit_type,*unit,*response,*group,*stratum;
   int scope,nsample;
   /* Indexed by THD_METRIC_*; irrelevant or undefined metrics are NAN.
      Raw delta is B-A; improvement is A-B for MAE/RMSE, B-A otherwise. */
   double a[7],b[7];
} THD_mvpa_pair_score;
typedef struct {
   int nrow,nscore;
   int *row_a,*row_b; /* aligned original zero-based row indices, key sorted */
   THD_mvpa_pair_score *score; /* pooled, fold, subject, stratum; no fold mean */
} THD_mvpa_pair_result;
extern THD_mvpa_pair_result *THD_mvpa_compare_predictions(
   const THD_mvpa_pair_row *a,int na,const THD_mvpa_pair_row *b,int nb,
   int classification,int nclass,char *err,int nerr);
extern void THD_mvpa_pair_result_free(THD_mvpa_pair_result *result);

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
/* One positive penalty per response. Means are shared; responses with equal
   penalties share a factorization. Up to q factors, no extra fit allocations.
   Coefficient/prediction layout and intercept convention match THD_ridge_fit. */
extern int THD_ridge_fit_response_lambda(THD_ridge_workspace *w,
   const float *x,const float *y,int n,int p,int q,const double *lambda,int form,
   char *err,int nerr);
extern int THD_ridge_predict(const THD_ridge_workspace *w, const float *x,
                             int n, float *prediction, char *err, int nerr);
/* Export slopes [feature][response] and unpenalized intercept [response]
   in the exact predictor space supplied to fit: y = intercept + X*coef.
   Caller provides p*q and q doubles. No preprocessing inversion is implied. */
extern int THD_ridge_coefficients(const THD_ridge_workspace *w,
   double *coef, double *intercept, char *err, int nerr);
extern void THD_ridge_free(THD_ridge_workspace *w);

/* Descriptive all-sample ridge refit, independent of CV, spatial extraction,
   and output formats. x[n][p], y[n][q] are compact row-major matrices; groups
   has n entries when preprocessing is run_center. Class indicators, if wanted,
   are supplied as y by the caller. Inputs are never modified.
   Coefficients are [p][q] in PREPROCESSED predictor units, intercepts [q] in
   original response units; the intercept is unpenalized. lambda must be finite
   and positive. AUTO uses the smaller primal/dual ridge system.
   The workspace reuses matrix/solver buffers up to its allocation capacities.
   Each call refits preprocessing on ALL n rows. Results and preprocessing
   metadata are borrowed until the next call (successful or failed) or free.
   This is descriptive estimation, not held-out prediction or significance. */
typedef struct THD_mvpa_refit_workspace THD_mvpa_refit_workspace;
typedef struct {
   int nsample, nfeature, nresponse;
   const THD_preprocessor *prep;
   const double *coef, *intercept;
} THD_mvpa_refit_result;
extern THD_mvpa_refit_workspace *THD_mvpa_refit_alloc(int max_sample,
   int max_feature,int max_response,char *err,int nerr);
/* NULL on failure with err populated; previous results are invalidated. */
extern const THD_mvpa_refit_result *THD_mvpa_refit_ridge(
   THD_mvpa_refit_workspace *w,const float *x,const float *y,int n,int p,int q,
   int preprocess,const int *groups,double lambda,char *err,int nerr);
extern void THD_mvpa_refit_free(THD_mvpa_refit_workspace *w);

/* Supervised selection follows nuisance adjustment and scaling of ALL input
   columns. Classification ranks between-class/total centered sum of squares
   (eta squared); continuous fits rank mean squared Pearson correlation over
   nonconstant response columns. Constant predictors/responses score zero.
   Ties use the smaller original input-column index. Counts are clipped to the
   active pattern width (e.g., edge searchlights). Each fit refits selection.
   Buffers are owned, nested plan borrowed. Audit indices are zero-based input
   columns, in descending score order; callers map them to voxel/node IDs or
   design-column names. Only observed outer-fold selections are retained. */
typedef struct THD_feature_selection {
   int max_feature, ngrid, npenalty, metric, nclass;
   int *grid, *chosen, *count, *indices; /* grid; [fold]; [fold]; [fold][max_feature] */
   double *scores, *loss; /* [fold][max_feature]; [fold][ngrid][npenalty], at chosen PCA count */
   const THD_nested_foldplan *nested;
   const int *classes; /* internal borrowed labels for ridge classification */
   void *rank; /* private sorting scratch */
   float *row;
   double *sums;
   int *counts, *train, *test;
} THD_feature_selection;

/* Centered, unwhitened PCA. Fit uses only supplied training rows. Eigenvalues
   are sample variances (n-1); descending components have their largest absolute
   loading positive (lowest column breaks ties). Repeated-eigenvalue bases are
   not unique. Rank threshold = largest eigenvalue * 64*DBL_EPSILON*max(n,p).
   Effective count clips to numerical rank; rank zero uses one zero model input
   while reporting zero retained components. No fit/apply allocations. Loadings
   use [component][active feature] stride; a failed fit invalidates application. */
typedef struct {
   int max_sample,max_feature,capacity,dimension,nfeature,rank,count;
   double total_variance;
   double *mean,*loading,*variance,*matrix,*vectors,*eigen,*row;
} THD_pca_model;
extern THD_pca_model *THD_pca_alloc(int n,int p,int capacity,char *err,int nerr);
extern int THD_pca_fit(THD_pca_model *m,const float *x,int n,int p,int requested,char *err,int nerr);
/* In-place row-major compaction from p inputs to max(1,count) scores. */
extern int THD_pca_apply(THD_pca_model *m,float *x,int n,char *err,int nerr);
extern void THD_pca_free(THD_pca_model *m);

typedef struct {
   THD_pca_model *model;
   const THD_nested_foldplan *nested;
   int ngrid,nselection,npenalty,metric;
   int *grid,*chosen,*count,*rank,*input_count,*train,*test;
   double *variance,*total,*loss; /* [fold][capacity], [fold], [fold][selection][PCA][penalty] */
   int *inner_min,*inner_max; /* [fold][selection][PCA], effective component range */
   double *means,*loadings; /* optional [fold][max_feature], [fold][capacity][max_feature] */
} THD_mvpa_pca;

/* Descriptive joint activation patterns A = Cov(X,S) Cov(S,S)^+.
   X is the preprocessed original feature matrix, S the continuous fitted
   score matrix, both restricted to one training set. Covariances are centered
   sample covariances (n-1). The symmetric Moore-Penrose inverse discards score
   eigenvalues <= 64*DBL_EPSILON*max(n,q)*largest eigenvalue. Rank zero gives
   zero patterns with rank=0, not evidence of absent signal/significance.
   No fitting allocations. Each fold occupies capacity-sized blocks; active
   pattern stride is q, covariance stride q, eigenvalues sorted descending. */
typedef struct {
   int max_sample,max_feature,max_output,nfold;
   int *rank;
   float *input; /* caller scratch [max_sample][max_feature] */
   double *scores; /* caller scratch [max_sample][max_output] */
   double *pattern,*xmean,*smean,*covariance,*inverse,*spectrum,*cutoff;
   double *matrix,*vectors,*eigen,*cross;
} THD_activation_patterns;
extern THD_activation_patterns *THD_activation_patterns_alloc(int n,int p,int q,
   int nfold,char *err,int nerr);
extern int THD_activation_patterns_fit(THD_activation_patterns *w,
   const float *x,const double *scores,int n,int p,int q,int fold,char *err,int nerr);
extern void THD_activation_patterns_free(THD_activation_patterns *w);

typedef struct {
   int nconfig; /* feature-count x PCA-count grid */
   double *selected; /* [outer fold][max_output] */
   double *loss; /* [outer fold][configuration][lambda][max_output] */
   double *config_loss; /* [outer fold][configuration], mean response minima */
   int *best; /* [outer fold][configuration][max_output], lambda-grid index */
   int *nvalid; /* [outer fold][configuration], defined response losses */
} THD_response_ridge;

/* Continuous estimators share the ridge CV/preprocessing/permutation engine.
   Candidate IDs are one-based, in caller order; ties prefer the first candidate
   after smaller feature/PCA counts. Penalties are shared across responses;
   elastic net and SVR fit each response independently; PLS fits them jointly.
   All arrays below are owned except the configuration passed to the setter,
   which is copied. Configure once, LAST, after selection/PCA/objective. */
#define THD_REG_ELASTIC_NET 1
#define THD_REG_BANDED_RIDGE 2
#define THD_REG_PLS 3
#define THD_REG_SVR 4
typedef struct {
   int method,ncandidate,ngroup,nfeature,max_iterations;
   double tolerance;
   const double *parameter; /* [candidate][2], alpha/rho, components/0, C/epsilon */
   const int *feature_group; /* banded only, [nfeature], IDs 0..ngroup-1 */
   const double *group_penalty; /* banded only, [candidate][ngroup] */
} THD_regression_config;
typedef struct THD_regression THD_regression;
typedef struct {
   int iterations,components,nonzero;
   double residual,objective;
} THD_regression_diagnostic;
/* EN: SSE/(2n)+alpha*rho*|b|_1+alpha*(1-rho)*||b||^2/2,
   training-centered X/Y, unpenalized intercept, absolute max KKT tolerance.
   Banded: SSE+sum_g lambda_g*||b_g||^2, unpenalized intercept; no group-size
   normalization; exact primal/linear-kernel solve, positive group penalties.
   PLS: centered, response-unscaled NIPALS regression deflation (PLS1/PLS2),
   unit predictor weights; power-iteration change <= tolerance; clips to
   min(n-1,p) and exhausted covariance. Response domain = one supplied fit.
   SVR: .5*(||b||^2+intercept^2)+C*sum(max(0,abs(y-Xb-intercept)-epsilon)^2),
   uncentered supplied X/Y, penalized intercept; signed dual coordinate descent,
   absolute max dual KKT tolerance. epsilon is in original response units.
   Failed fits invalidate predictions; all repeated fits reuse allocated scratch. */
extern THD_regression *THD_regression_alloc(int n,int p,int q,
   const THD_regression_config *config,char *err,int nerr);
extern int THD_regression_fit(THD_regression *m,const float *x,const float *y,
   int n,int p,int q,int candidate,char *err,int nerr);
extern int THD_regression_predict(const THD_regression *m,const float *x,int n,
   float *out,char *err,int nerr);
extern int THD_regression_coefficients(const THD_regression *m,double *coef,
   double *intercept,char *err,int nerr);
extern const THD_regression_diagnostic *THD_regression_diagnostics(const THD_regression *m);
extern void THD_regression_free(THD_regression *m);

typedef struct {
   const THD_foldplan *plan;
   int max_feature, max_output, preprocess;
   THD_preprocessor *prep;
   THD_feature_selection *selection;
   THD_mvpa_pca *pca;
   THD_ridge_workspace *ridge;
   THD_regression *regression; /* optional continuous estimator */
   THD_regression_diagnostic *reg_inner; /* [fold][feature/PCA config][candidate], inner maxima */
   THD_regression_diagnostic *reg_diagnostic; /* [fold][max_output] */
   double *reg_coef; /* optional EN [fold][max_feature][max_output], input order */
   double *reg_group; /* banded [fold][group][max_output][2]: variance,covariance */
   THD_activation_patterns *patterns; /* opt-in outer-training descriptive patterns */
   THD_response_ridge *response_ridge; /* optional continuous per-response tuning */
   float *train_x, *test_x, *train_y, *test_y;
   float *prediction, *baseline; /* [sample][active output]; NAN if untested */
   float *truth_scratch, *pred_scratch, *base_scratch;
   double *rank_scratch;
   int *index_scratch;
   THD_mvpa_continuous_score *score;      /* [output], pooled tested rows */
   THD_mvpa_continuous_score *fold_score; /* [fold][active output] */
   const THD_nested_foldplan *nested; /* optional borrowed tuning plan */
   int nlambda, classification, tune_objective;
   int *inner_confusion; /* [lambda][max_output][max_output], ACC/BACC only */
   double *lambda_grid, *selected_lambda; /* grid owned; selection [fold] */
   double *inner_loss; /* [outer fold][lambda], minimized inner objective */
   int *inner_nvalid; /* [outer fold], valid responses or represented classes */
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
extern int THD_mvpa_set_regression(THD_mvpa_ridge_workspace *w,
   const THD_regression_config *config,char *err,int nerr);
/* Enable nested tuning once per workspace. The nested plan must use w->plan.
   A one-value grid supports joint feature-count tuning at a fixed penalty.
   By default select one lambda per outer fold by mean response-wise
   SSE/baseline SSE over pooled inner held-out rows. Exact ties, or all-constant responses,
   select the largest lambda. The run's fixed lambda argument is then unused. */
extern int THD_mvpa_ridge_set_tuning(THD_mvpa_ridge_workspace *w,
   const THD_nested_foldplan *nested, const double *lambda_grid, int nlambda,
   char *err, int nerr);
/* Configure LAST after tuning, objective, selection and PCA. Continuous only.
   Each response minimizes its pooled inner loss; ties/all-undefined choose
   largest lambda. Shared feature/PCA counts minimize the mean of those minima,
   excluding undefined normalized-MSE responses; ties choose smaller counts.
   selected_lambda[fold] is NAN in this mode; read response_ridge->selected.
   The existing scalar candidate losses remain diagnostic shared-lambda losses.
   All fits, selection and audits are recomputed/restored under permutations. */
extern int THD_mvpa_ridge_enable_response_lambda(THD_mvpa_ridge_workspace *w,
   char *err,int nerr);
/* Enable observed outer-training pattern storage. Configure after pipeline
   options; supports ridge decoding/classification, including nuisance/selection/
   PCA. Pattern axes are all input features after nuisance/scaling, before
   selection/PCA. Selected-out features can have nonzero score covariance.
   Permutation runners restore observed patterns; no pattern inference. */
extern int THD_mvpa_ridge_enable_patterns(THD_mvpa_ridge_workspace *w,char *err,int nerr);
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

/* Frozen linear pipeline: fixed-partition or all-training fits, neural inputs,
   none/zscore/sample-center scaling, nuisance, selection and PCA. No run-center.
   Captures owned numerical state; prediction never calls a fitting routine.
   Float stage boundaries match the original fitted pipeline. */
typedef struct THD_mvpa_frozen_ridge THD_mvpa_frozen_ridge;
extern THD_mvpa_frozen_ridge *THD_mvpa_ridge_freeze(
   const THD_mvpa_ridge_workspace *w,char *err,int nerr);
extern int THD_mvpa_frozen_predict(const THD_mvpa_frozen_ridge *m,
   const float *x,int n,int p,const float *covariates,int ncovariate,
   float *prediction,char *err,int nerr);
extern void THD_mvpa_frozen_free(THD_mvpa_frozen_ridge *m);

typedef struct {
   int id,nfeature;
   char *label;
   int *feature; /* ordered original voxel indexes; full preprocessing domain */
   THD_mvpa_frozen_ridge *fit;
} THD_mvpa_model_unit;
typedef struct {
   int classification,searchlight,nunit,nresponse,ncovariate,ntrain;
   int estimator; /* 0=ridge (format v1), 1=logistic logits (format v2) */
   int dim[3],view;
   double affine[12]; /* voxel IJK -> DICOM real, row-major 3x4 */
   char *space,*target; /* target column(s); response/class labels retain order */
   char **response,**covariate;
   THD_mvpa_model_unit *unit;
} THD_mvpa_model;
/* Portable strict text data: ridge format v1, logistic format v2 (linear
   outputs are logits, followed by softmax at application). Finite doubles and
   length/hex-encoded strings (no executable content). Reader validates sizes,
   labels, geometry, feature order and pipeline domains; trailing data rejected.
   Atomic same-directory publication; overwrite=0 never replaces an existing
   path. Model owns every member. Caller enforces geometry against new images. */
extern int THD_mvpa_model_write(const char *path,const THD_mvpa_model *m,
   int overwrite,char *err,int nerr);
extern THD_mvpa_model *THD_mvpa_model_read(const char *path,char *err,int nerr);
extern void THD_mvpa_model_free(THD_mvpa_model *m);

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
   THD_feature_selection *selection;
   THD_mvpa_pca *pca;
   THD_centroid_model *centroid ;
   THD_mvpa_ridge_workspace *ridge_cv ; /* NULL unless ridge configured */
   THD_lda_model *lda ; /* NULL unless LDA configured */
   THD_svm_model *svm; /* NULL unless native linear SVM configured */
   THD_logistic_model *logistic;
   double *probability,*test_probability; /* [sample][class]; NaN when untested */
   double *fold_log_loss,*fold_brier;
   double class_penalty,class_tolerance;
   int tune_objective; /* DEFAULT retains the configured classifier metric */
   int class_max_iter,class_metric,class_ngrid;
   const THD_nested_foldplan *class_nested; /* borrowed */
   double *class_grid,*class_selected,*class_loss; /* [candidate], [fold], [fold][candidate] */
   double *fold_solver_residual; /* SVM: max KKT residual; logistic: max absolute gradient */
   int *fold_solver_iterations,*class_inner_train,*class_inner_test;
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

/* Configure once, after estimator/penalty tuning. One count is fixed; two or
   more counts require a matching grouped nested plan. Ridge count tuning uses
   the configured objective (default normalized squared loss) jointly with
   lambda; configure ridge tuning first (a one-value lambda grid is allowed for count-only tuning).
   Other classifiers use 1-ACC/BACC, jointly with C when configured. Exact ties
   prefer the smaller requested count, then largest lambda / smallest C.
   A fixed count is still refitted inside any penalty-tuning folds. */
extern int THD_mvpa_ridge_set_selection(THD_mvpa_ridge_workspace *w,
   const int *counts,int ncount,char *err,int nerr);
extern int THD_mvpa_classification_set_selection(THD_mvpa_workspace *w,
   const int *counts,int ncount,const THD_nested_foldplan *nested,int metric,
   char *err,int nerr);

/* Configure PCA once after estimator, penalty tuning and feature selection.
   Order: nuisance -> scale all inputs -> select -> PCA -> estimator.
   Component grids compose with count/penalty grids on the same nested plan.
   Exact loss ties: smaller feature count, smaller component count, then
   larger lambda / smaller C. Optional loading audits can consume substantial
   memory (folds * input width * retained component capacity). */
extern int THD_mvpa_ridge_set_pca(THD_mvpa_ridge_workspace *w,
   const int *counts,int nk,int keep_loadings,char *err,int nerr);
extern int THD_mvpa_classification_set_pca(THD_mvpa_workspace *w,
   const int *counts,int nk,const THD_nested_foldplan *nested,int metric,
   int keep_loadings,char *err,int nerr);

/* Set after estimator, penalty tuning, optional feature selection and PCA
   are configured. Requires a nested search. Defaults: normalized MSE for ridge,
   configured ACC/BACC for other classifiers. Classifying ridge additionally
   supports ACC/BACC; continuous ridge supports normalized MSE/MSE/RMSE/MAE.
   Losses pool inner held-out rows (not fold-wise means). Continuous objectives
   average response-wise losses equally, with RMSE rooted before averaging.
   Normalized MSE excludes zero baseline-SSE responses; if all are excluded,
   loss is NaN and deterministic parameter ties select a fallback. All other
   objectives must be finite; absent classes are excluded from pooled BACC.
   Existing predictors supply scores, not probabilities: no probability loss.
   Changing the objective does not change outer scoring or permutation tails. */
extern int THD_mvpa_ridge_set_objective(THD_mvpa_ridge_workspace *w,int objective,
   char *err,int nerr);
extern int THD_mvpa_classification_set_objective(THD_mvpa_workspace *w,int objective,
   char *err,int nerr);

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
   nested tuning defaults to mean class-wise SSE/baseline SSE,
   with largest-lambda ties. Every inner training set must contain all classes.
   The nested plan is borrowed and must outlive this workspace. */
extern int THD_mvpa_classification_set_ridge(THD_mvpa_workspace *w,
   double lambda, const THD_nested_foldplan *nested,
   const double *grid, int nlambda, char *err, int nerr);
extern int THD_mvpa_classification_set_lda(THD_mvpa_workspace *w, int mode,
   double alpha, double variance_floor, int empirical_prior, char *err, int nerr);
/* Optional grouped nested C selection defaults to 1-ACC/BACC over pooled inner
   held-out rows. Ties select smallest C. Every inner training set needs all
   classes. A one-value C grid supports feature-count-only tuning.
   Preprocessing/tuning/fits repeat under every label permutation. */
extern int THD_mvpa_classification_set_svm(THD_mvpa_workspace *w,double cost,
   double tolerance,int max_iterations,int metric,
   const THD_nested_foldplan *nested,const double *grid,int ngrid,char *err,int nerr);
extern THD_mvpa_frozen_ridge *THD_mvpa_logistic_freeze(const THD_mvpa_workspace *w,char *err,int nerr);
extern int THD_mvpa_classification_set_logistic(THD_mvpa_workspace *w,double lambda,
   double tolerance,int max_iterations,int metric,
   const THD_nested_foldplan *nested,const double *grid,int ngrid,char *err,int nerr);
/* Default workspaces run centroid; configured workspaces run ridge, LDA, SVM or logistic.
   distance selects the centroid metric and is unused by ridge/LDA/SVM/logistic (pass zero).
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

/* Validate class coverage, LDA training size, and run_center group availability.
   classes/groups are optional nsample arrays, using this plan's LOCAL indexes
   (including inner plans). scope is optional diagnostic context. No fitting,
   mutation, output, or process exit; returns 1 on success, 0 with err on failure. */
extern int THD_mvpa_validate_fold_inputs(const THD_foldplan *plan,
   const int *classes,int nclass,const int *groups,int lda,const char *scope,char *err,int nerr);

/* Canonical class order is first occurrence in plan->train[0], independent of
   held-out rows. Remaps classes[nsample] and reorders labels[nclass] pointers
   together; strings remain caller-owned. Requires all classes in that training
   set. Failure leaves both arrays unchanged. Returns 1/0 and an error message. */
extern int THD_mvpa_fixed_class_order(const THD_foldplan *plan,int *classes,
                               char **labels,int nclass,char *err,int nerr);

#endif /* _THD_MVPA_HEADER_ */
