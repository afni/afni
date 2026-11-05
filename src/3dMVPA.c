#include "mrilib.h"
#include "thd_crossval.h"
#include "thd_mapinfer.h"
#include "thd_mvpa.h"
#include "thd_patterns.h"
#include "thd_permute.h"
#include "thd_sampleset.h"
#include "thd_xmat.h"

#include <errno.h>
#include <float.h>
#include <limits.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef USE_OMP
#include <omp.h>
#endif

#ifdef USE_SUMA
#include "SUMA_suma.h"
#endif

#define PROGRAM_NAME "3dMVPA"
#define MVPA_PARTITION "MVPA_Partition"
#define MVPA_SOURCE_TABLE "MVPA_SourceTable"
#define MVPA_SOURCE_ROW "MVPA_SourceRow"

/*----------------------------------------------------------------------------
  Classification and continuous prediction in ROIs and spatial neighborhoods.
  Compact input adapters end at THD_sampleset; everything after that is shared.
                                                     -- P Molfese, December 2026
------------------------------------------------------------------------------*/

#define MVPA_PROGRESS_AUTO 0
#define MVPA_PROGRESS_BAR  1
#define MVPA_PROGRESS_LINE 2
#define MVPA_PROGRESS_OFF  3

typedef struct {
   int enabled, mode, is_tty, total, done, last_bucket ;
   double start, last_report ;
   const char *label, *unit ;
} MVPA_progress ;

static void mvpa_duration( double sec , char *buf , size_t nbuf )
{
   long ss=(long)((sec>0.0)?sec+0.5:0.0), hh=ss/3600, mm=(ss%3600)/60 ;
   ss %= 60 ;
   if( hh > 0 ) snprintf(buf,nbuf,"%ldh%02ldm%02lds",hh,mm,ss) ;
   else if( mm > 0 ) snprintf(buf,nbuf,"%ldm%02lds",mm,ss) ;
   else snprintf(buf,nbuf,"%lds",ss) ;
}

static void mvpa_progress_init( MVPA_progress *p, int mode, int quiet,
                               const char *label, int total, const char *unit )
{
   memset(p,0,sizeof(*p)) ;
   p->enabled = !quiet && mode != MVPA_PROGRESS_OFF && total > 0 ;
   p->is_tty = isatty(fileno(stderr)) ;
   p->mode = (mode==MVPA_PROGRESS_AUTO)
           ? (p->is_tty ? MVPA_PROGRESS_BAR : MVPA_PROGRESS_LINE)
           : mode ;
   p->label=label ; p->total=total ; p->unit=unit ;
   p->start=p->last_report=0.001*(double)NI_clock_time() ;
   if( !p->enabled ) return ;
   if( p->mode == MVPA_PROGRESS_BAR ){
     fprintf(stderr,"++ 3dMVPA %s [--------------------] 0/%d (0.0%%)",
             label,total) ;
     fflush(stderr) ;
   } else {
     INFO_message("3dMVPA %s: 0/%d %s complete",label,total,unit) ;
   }
}

static void mvpa_progress_advance( MVPA_progress *p )
{
   int done,bucket ; double now,elapsed,rate,eta ; char ebuf[32],tbuf[32] ;
   if( !p->enabled ) return ;
   /* Keep counting and rendering together so parallel completions cannot
      print an older count after a newer one (or after the final newline). */
#ifdef USE_OMP
#pragma omp critical(mvpa_progress_render)
#endif
   {
     done = ++p->done ;
     if( done > p->total ) done=p->total ;
     now=0.001*(double)NI_clock_time() ; bucket=(int)(10.0*done/p->total) ;
     if( done==p->total ||
         (p->mode==MVPA_PROGRESS_BAR && now-p->last_report>=1.0) ||
         (p->mode==MVPA_PROGRESS_LINE && bucket>p->last_bucket) ){
       elapsed=now-p->start ; rate=(elapsed>0.0)?done/elapsed:0.0 ;
       eta=(rate>0.0)?(p->total-done)/rate:0.0 ;
       mvpa_duration(elapsed,ebuf,sizeof(ebuf)) ;
       mvpa_duration(eta,tbuf,sizeof(tbuf)) ;
       if( p->mode==MVPA_PROGRESS_BAR ){
         int ii,fill=(int)(20.0*done/p->total) ;
         fprintf(stderr,"\r++ 3dMVPA %s [",p->label) ;
         for( ii=0 ; ii<20 ; ii++ ) fputc((ii<fill)?'#':'-',stderr) ;
         fprintf(stderr,"] %d/%d (%.1f%%) | %.2f %s/s | elapsed %s",
                 done,p->total,100.0*done/p->total,rate,p->unit,ebuf) ;
         if( done<p->total ) fprintf(stderr," | ETA ~%s",tbuf) ;
         if( p->is_tty ) fputs("\033[K",stderr) ; /* erase an older, longer ETA */
         if( done==p->total ) fprintf(stderr,"\n") ;
         fflush(stderr) ;
       } else {
         INFO_message("3dMVPA %s: %d/%d %s complete (%.1f%%); "
                      "%.2f %s/s; elapsed %s%s%s",
                      p->label,done,p->total,p->unit,
                      100.0*done/p->total,rate,p->unit,ebuf,
                      (done<p->total)?"; ETA ~":"",(done<p->total)?tbuf:"") ;
       }
       p->last_report=now ; p->last_bucket=bucket ;
     }
   }
}

static void mvpa_progress_finish( MVPA_progress *p )
{
   /* Keep error messages on a new line if a location failed. */
   if( p->enabled && p->mode==MVPA_PROGRESS_BAR && p->done<p->total ){
      fputc('\n',stderr) ; fflush(stderr) ;
   }
}

static char *mvpa_options[]={
   "-train_only","-apply_dset","-save_model","-apply_model","-compare_predictions","-mode","-dataTable","-dataTableFile","-trial_manifest","-xmat_stims",
   "-testTable","-testTableFile",
   "-target_column","-target_columns","-design_columns","-lambda","-score",
   "-enet_alpha","-enet_alpha_grid","-enet_l1_ratio","-enet_l1_ratio_grid",
   "-pls_components","-pls_components_grid","-svr_c","-svr_c_grid","-svr_epsilon","-svr_epsilon_grid",
   "-feature_groups","-banded_penalties","-reg_tol","-reg_max_iter",
   "-lambda_grid","-lambda_per_response","-inner_cv_column","-inner_cv_k","-nuisance_columns",
   "-select_features","-select_features_grid","-tune_metric","-save_tuning",
   "-pca_components","-pca_components_grid","-save_pca_loadings",
   "-shrinkage","-class_prior","-variance_floor",
   "-svm_c","-svm_c_grid","-svm_tol","-svm_max_iter",
   "-logistic_lambda","-logistic_lambda_grid","-logistic_tol","-logistic_max_iter",
   "-save_probability_scores","-save_calibration",
   "-cv_column","-cv","-cv_k","-within_column",
   "-partition_column","-train_value","-test_value",
   "-transfer_column","-train_domain","-test_domain","-transfer_subjects",
   "-transfer_items","-transfer_sessions","-session_column",
   "-subject_column","-run_column","-item_column","-fold_column","-sample_column",
   "-mask","-roi_sel","-searchlight","-surf","-method","-distance","-scale","-metric","-prefix",
   "-nperm","-seed","-permute_within","-block_column",
   "-save_predictions","-save_fold_scores","-save_confusion","-save_summary_scores",
   "-save_coefficients","-coef_lambda","-save_patterns",
   "-show_table","-show_samples","-show_folds","-validate_only","-quiet","-progress","-help","-h",NULL
} ;

typedef struct {
   char *column,*train,*test,*subjects,*items,*sessions,*session_column;
   const char *subject_column,*item_column;
   char *audit;
   int overlap[3]; /* global subjects, global items, subject-qualified sessions */
} mvpa_transfer_options;

static void usage_3dMVPA( void )
{
   printf(
"\n"
"Usage: 3dMVPA [options]                                      ~1~\n"
"\n"
"Cross-validated classification, continuous decoding, and encoding in\n"
"ROIs or volume/surface neighborhoods. Preprocessing, intercepts, and model\n"
"coefficients are fitted separately inside every training fold.\n"
"\n"
"Analysis modes and inputs:                                  ~1~\n"
"\n"
"  -mode classify        Brain response patterns -> categorical labels.\n"
"  -mode decode          Brain response patterns -> numeric targets.\n"
"  -mode encode          Numeric stimulus/task features -> brain responses.\n"
"\n"
"  -dataTable @TABLE\n"
"  -dataTableFile TABLE\n"
"  -dataTable Subj Run Condition InputFile ...\n"
"                         One row per sample; a table can include many subjects.\n"
"                         Subj identifies the subject for each row. Choose CV\n"
"                         to match within-subject or held-out-subject prediction.\n"
"                         InputFile must select one sub-brick when its dataset\n"
"                         has more than one.\n"
"                         A subject-level table with Xmat and whole-bucket\n"
"                         InputFile columns is expanded to the same sample\n"
"                         representation before analysis.\n"
"  -testTable @TABLE      Optional independent test table. When supplied,\n"
"  -testTableFile TABLE   -dataTable/-dataTableFile contains training rows only.\n"
"                         -testTable also accepts an inline table, with InputFile\n"
"                         last, just like -dataTable. Both tables must have the\n"
"                         same column names (order/case may differ), including\n"
"                         test targets for scoring. Both may use Xmat expansion.\n"
"                         Dataset paths retain the usual working-directory basis.\n"
"                         Explicit sample IDs must be unique across both tables;\n"
"                         omitted IDs are generated over the combined rows.\n"
"                         The same dataset/brick on both sides is an error\n"
"                         (including symlink aliases). Separate files alone do\n"
"                         not establish scientific independence.\n"
"                         Cannot accompany -trial_manifest, -partition_column,\n"
"                         -train_value, -test_value, or outer CV options. All\n"
"                         fixed-split tuning/inference restrictions below apply.\n"
"                         Prediction audits append Partition, SourceTable, and\n"
"                         SourceRow (one-based input data row, before expansion).\n"
"                         -show_samples also prints this provenance. Internal\n"
"                         MVPA_Partition/MVPA_SourceTable/MVPA_SourceRow columns\n"
"                         are reserved in two-table input.\n"
"  -trial_manifest FILE   Alternative to -dataTable: one row per trial beta.\n"
"                         InputFile names a whole LSS or LSA beta bucket;\n"
"                         Volume is its zero-based sub-brick index. Supply\n"
"                         trial, run and target metadata explicitly. Optional\n"
"                         Xmat and XColumn must occur together; XColumn is\n"
"                         the zero-based original design-matrix column. These\n"
"                         indices are checked; metadata is retained unchanged.\n"
"                         No beta-label matching or Xmat expansion is applied.\n"
"                         Cannot accompany -dataTable, -dataTableFile, or\n"
"                         -xmat_stims.\n"
"\n"
"  -target_column NAME    Categorical classification or numeric decoding target.\n"
"  -target_columns A,B    Multiple numeric decoding targets.\n"
"  -design_columns A,B    Numeric predictors for encoding neural responses.\n"
"  -score voxel|pattern  Encoding output: voxel [default] predicts each masked\n"
"                         voxel; pattern averages per-response metrics within\n"
"                         each ROI/searchlight, excluding undefined values.\n"
"  -cv_column NAME        Leave one level of NAME out at a time. Choose this\n"
"                         (or -cv), -partition_column, OR a separate -testTable.\n"
"  -mask DSET             Without -searchlight, positive values define ROIs and\n"
"                         a binary mask is one ROI. With -searchlight, nonzero\n"
"                         voxels define eligible centers and neighborhood voxels.\n"
"                         The mask and every sample must share a grid or exact\n"
"                         surface node-row domain. A surface searchlight may omit\n"
"                         this option to search every represented sample node.\n"
"\n"
"Cross-validation:                                          ~1~\n"
"\n"
"  -cv run|subject|item   Shorthand for -cv_column Run, Subj, or Item.\n"
"  -cv_k K               Assign whole -cv_column groups to K balanced folds\n"
"                         instead of leave-one-group-out.\n"
"  -within_column NAME    Build independent folds inside each NAME level.\n"
"                         For within-subject run CV, use Subj here.\n"
"\n"
"  -partition_column NAME One prespecified train/test split, for all modes.\n"
"                         Every row must have exactly one of the two values\n"
"                         below; both sets must be nonempty. Values are case-\n"
"                         sensitive. Cannot accompany -cv, -cv_column, -cv_k,\n"
"                         or -within_column. No automatic subject/run splitting.\n"
"  -train_value LABEL     Training value [train]; requires -partition_column.\n"
"  -test_value LABEL      Testing value [test]; requires -partition_column.\n"
"                         Only training rows fit preprocessing and the model;\n"
"                         only test rows contribute to scores. Prediction audits\n"
"                         retain training rows with NA predictions/folds.\n"
"                         All test classes must be represented in training;\n"
"                         class order comes from first appearance in training.\n"
"                         Tuned ridge/SVM require -inner_cv_column, with at\n"
"                         least two groups among training rows. Inner CV never\n"
"                         uses the reserved test rows.\n"
"                         Permutation blocks must lie entirely in one partition;\n"
"                         use a composite partition/block column if needed.\n"
"                         Full-data -save_coefficients is incompatible with a\n"
"                         reserved test set. Add -save_model for supported reuse.\n"
"\n"
"Cross-condition/task/session transfer:                      ~1~\n"
"\n"
"  -transfer_column NAME  Directional source -> destination analysis, for all\n"
"                         modes/estimators and ROI/volume/surface analyses.\n"
"                         Replaces outer CV/partition options. Every row must\n"
"                         belong to one of the two domains; no silent subsetting.\n"
"  -train_domain LABEL    Required source-domain value (case-sensitive).\n"
"  -test_domain LABEL     Required, distinct destination-domain value.\n"
"  -transfer_subjects within|disjoint  Required study design:\n"
"                         within: each subject has both domains; fit that\n"
"                         subject's source rows and test only their destination\n"
"                         rows. Other subjects never enter that fit or tuning.\n"
"                         disjoint: one pooled source fit; every destination\n"
"                         subject must be absent from source training.\n"
"  -transfer_items disjoint|allow  Default disjoint: no shared item identities\n"
"                         across domains, including across subjects. Use allow\n"
"                         for deliberate transfer on repeated stimuli/items.\n"
"                         Item IDs must identify the same stimulus globally;\n"
"                         unique row IDs alone cannot establish item novelty.\n"
"  -session_column NAME   Session identity column [Session], required for transfer.\n"
"  -transfer_sessions disjoint|allow  Default disjoint: no shared (Subj,Session)\n"
"                         keys across domains. Use allow for task/condition\n"
"                         transfer within the same session. Local session names\n"
"                         may repeat across different subjects.\n"
"\n"
"Transfer needs explicit subject and item roles (Subj, Item/Trial, or role\n"
"options) plus session metadata. Sample IDs remain unique. Repeated dataset/\n"
"brick selections, including symlink aliases, are rejected even within a domain.\n"
"Overlap checks use supplied exact identities, not image-content comparisons.\n"
"\n"
"One table may contain both domains; alternatively, -dataTable/-testTable must\n"
"contain only source/destination rows respectively, with the same columns.\n"
"Classification requires the same complete class-label set in each fit's two\n"
"domains. Numeric response/design columns retain their shared names and order;\n"
"values must be finite. Recode labels/align numeric units explicitly beforehand:\n"
"no automatic category mapping, response matching, registration or alignment.\n"
"Volume dimensions/view/space and real affine must match (1e-5 tolerance per\n"
"entry); surface node IDs/order must match. One shared mask defines features.\n"
"\n"
"Only source rows fit nuisance adjustment, scaling, selection, PCA and models.\n"
"Tuning requires -inner_cv_column with multiple source groups per fit. Test\n"
"rows alone are scored. Permutation blocks must stay within domain AND subject\n"
"for within-subject transfer; use composite keys such as Subj_Task_Run.\n"
"Existing null/metric rules apply; transfer scores are not population inference.\n"
"-save_coefficients is incompatible. -save_model retains #27's ridge/volume\n"
"limits and requires ONE fit: save subjects separately for within-subject reuse.\n"
"-apply_model does not repeat these study-design/overlap checks on new samples.\n"
"\n"
"PREFIX.transfer.1D records direction, policies, overlap counts, target/class\n"
"mapping and each sample's domain/role/fold/subject/item/session. Standard\n"
"prediction/fold audits remain available; training predictions are NA.\n"
"-validate_only prints the transfer design without fitting or writing files.\n"
"\n"
"Sample roles:                                               ~1~\n"
"\n"
"  -subject_column NAME   Subject role [auto-detect Subj].\n"
"  -run_column NAME       Run role [auto-detect Run].\n"
"  -item_column NAME      Item/trial role [auto-detect Trial or Item].\n"
"  -fold_column NAME      Fold role [auto-detect Fold].\n"
"  -sample_column NAME    Unique sample labels [auto-detect Sample; otherwise\n"
"                         row000001, ...].\n"
"  -xmat_stims LIST       Comma-separated X-matrix stimuli to expand [all].\n"
"\n"
"Model options:                                              ~1~\n"
"\n"
"  -method METHOD         Classify: centroid [default], ridge, svm, logistic,\n"
"                         lda_diag, lda_shrinkage. Decode: ridge [default], elastic_net,\n"
"                         pls, svr. Encode: ridge [default], elastic_net, pls,\n"
"                         banded_ridge. See continuous estimator options below.\n"
"                         Ridge classification fits one 0/1 response per class\n"
"                         with argmax decisions; exact ties choose the first\n"
"                         encoded class. Decisions are not probabilities.\n"
"  -logistic_lambda L|auto  L2 softmax classification; positive penalty [1].\n"
"                         Binary and multiclass use one joint K-class model:\n"
"                         mean natural-log loss + L/2 * sum(weights squared).\n"
"                         Intercepts are unpenalized; no class weighting.\n"
"                         Every training split must contain every class.\n"
"                         Decisions are logits; probabilities sum to one.\n"
"                         Exact argmax ties use the first encoded class.\n"
"  -logistic_lambda_grid L1,L2,...  Enables grouped nested penalty selection\n"
"                         [auto: 0.01,0.1,1,10,100]; cannot accompany numeric L.\n"
"                         Default inner objective: log_loss; also accepts brier,\n"
"                         ACC and BACC. Exact ties prefer larger lambda.\n"
"                         Uses -inner_cv_column/-inner_cv_k; composes with\n"
"                         nuisance adjustment, feature selection and PCA.\n"
"  -logistic_tol T        Maximum absolute objective gradient [1e-6],\n"
"                         range [1e-12,1e-2]. Verified on every fit.\n"
"  -logistic_max_iter N   L-BFGS iteration limit [1000]; nonconvergence fails.\n"
"                         Scale features for numerical stability. Solver storage\n"
"                         is about 152*K*(NFeature+1) bytes per worker, plus data.\n"
"                         -save_predictions adds ProbabilityClass0,... in the\n"
"                         printed class-label order; training-only rows are NA.\n"
"                         -save_fold_scores adds lambda, inner loss, convergence\n"
"                         diagnostics, held-out LogLoss and Brier.\n"
"                         Main score/permutation maps still use ACC/BACC.\n"
"                         Model-file, coefficient and pattern export remain ridge-only.\n"
"  -svm_c C|auto          Native linear SVM only: positive cost [1]. Each\n"
"                         one-vs-rest model minimizes .5*(||w||^2+b^2) +\n"
"                         C*sum(max(0,1-y*(Xw+b))^2). The intercept b IS\n"
"                         penalized (constant feature = 1); no class weights.\n"
"                         Larger C penalizes margin violations more strongly.\n"
"                         Raw class margins are not probabilities; argmax wins,\n"
"                         exact ties choose the first encoded class.\n"
"  -svm_c_grid C1,C2,... Positive distinct candidates; enables grouped nested\n"
"                         selection [auto default: 0.01,0.1,1,10,100]. Cannot\n"
"                         accompany numeric -svm_c. Uses -inner_cv_column and\n"
"                         -inner_cv_k; by default minimize 1-ACC/BACC (-metric)\n"
"                         over pooled inner held-out rows. Ties choose SMALLER\n"
"                         C. Every inner training set needs every class.\n"
"                         Scaling, selection and fitting repeat per permutation.\n"
"                         PREFIX.tuning.1D records all candidate losses.\n"
"  -svm_tol T            Absolute projected-dual-gradient tolerance [1e-6],\n"
"                         range [1e-12,1e-2]. Deterministic cyclic coordinate\n"
"                         descent verifies final KKT residual for every class.\n"
"  -svm_max_iter N       Maximum sweeps per class [10000]. Nonconvergence is\n"
"                         an error; increase this limit, rescale, or reduce C.\n"
"                         Fold audits append C, InnerNFold, InnerLoss, maximum\n"
"                         class SolverIterations and SolverResidual. SVM is\n"
"                         classification-only; descriptive coefficient export\n"
"                         currently remains ridge-only.\n"
"  -shrinkage A           lda_shrinkage only: fixed covariance shrinkage in\n"
"                         (0,1] [0.1]. Use (1-A)*S + max(A,F)*T*I, where S is\n"
"                         pooled within-class covariance, T its mean variance,\n"
"                         and F the relative variance floor. No CV tuning.\n"
"  -variance_floor F      LDA only: relative variance floor [1e-6], allowed\n"
"                         range [1e-12,1]. lda_diag floors each variance at\n"
"                         F*T. If all within-class variances are zero, T uses\n"
"                         overall training variance, then 1 for constant data.\n"
"  -class_prior P         LDA only: equal [default] or empirical (training\n"
"                         class proportions). LDA needs more training rows\n"
"                         than classes in every outer fold. Slopes, means,\n"
"                         covariances and priors use training rows only.\n"
"                         DecisionClass scores are centered linear discriminants,\n"
"                         not probabilities; exact ties choose the first class.\n"
"                         Fold audits append Shrinkage, CovarianceScale,\n"
"                         VarianceFloor (absolute), and CovarianceForm.\n"
"  -lambda L              Positive ridge penalty [1]. Slopes are penalized;\n"
"                         the training-fitted intercept is unpenalized.\n"
"                         Use -lambda auto for nested grouped selection.\n"
"  -lambda_grid L1,L2,... Positive distinct candidates; enables nested tuning\n"
"                         [auto default: 0.01,0.1,1,10,100]. Cannot accompany\n"
"                         a numeric -lambda. One penalty per outer fold/unit\n"
"                         is shared across responses. Default: mean response\n"
"                         SSE / training-mean-baseline SSE on inner held-out\n"
"                         rows; exclude constant responses. Exact ties or no\n"
"                         informative responses select the largest penalty.\n"
"                         For classification, responses are class indicators:\n"
"                         default tuning uses normalized class-wise squared error.\n"
"                         Override with -tune_metric ACC/BACC. Inner training sets\n"
"                         contain every class, including under permutations.\n"
"  -lambda_per_response  Continuous decode/encode only: select a separate lambda\n"
"                         for each response using grouped inner CV. Enables the\n"
"                         auto grid unless -lambda_grid is supplied; incompatible\n"
"                         with numeric -lambda. Shared lambda remains the default.\n"
"                         Each response minimizes its pooled inner -tune_metric\n"
"                         loss (default normalized_mse); ties/all-undefined losses\n"
"                         choose the largest lambda. Feature/PCA configurations\n"
"                         remain shared: their objective is the mean of response-\n"
"                         wise minimum losses, excluding undefined normalized-MSE\n"
"                         responses. Configuration ties choose smaller requested\n"
"                         feature count, then smaller requested PCA count.\n"
"                         PREFIX.tuning.1D and optional candidate/count audits\n"
"                         gain response-level losses, local lambda selections,\n"
"                         configuration losses and final selections. Fold/pattern\n"
"                         audits report each response's actual selected lambda.\n"
"                         Preprocessing, tuning and fitting repeat under nulls;\n"
"                         score aggregation/correction families do not change.\n"
"                         Pattern encoding gains independent voxel/node penalties\n"
"                         within each joint fit; singleton voxel encoding already\n"
"                         tunes one response. Coefficient export remains a separate\n"
"                         full-data shared-penalty fit requiring -coef_lambda.\n"
"                         Inner candidate fits still share factors across responses;\n"
"                         outer fits use up to min(NResponse,NLambda) factors.\n"
"                         Extra audit storage grows as outer folds * feature/PCA\n"
"                         configurations * lambdas * responses, per worker.\n"
"  -inner_cv_column NAME Inner grouping column [outer -cv_column].\n"
"                         Required explicitly with tuned fixed partitions.\n"
"  -inner_cv_k K          Balance whole inner groups into K folds [default:\n"
"                         leave one group out]. Every outer training set must\n"
"                         contain at least two inner groups (or K if given).\n"
"                         Shared by all candidate searches. Preprocessing is\n"
"                         refitted within every inner split.\n"
"                         PREFIX.tuning.1D records all candidate losses;\n"
"                         fold audits append selected penalties and losses.\n"
"  -distance D            Centroid only: euclidean [default], correlation, cosine.\n"
"  -tune_metric NAME      Inner-CV selection objective, independent of -metric.\n"
"                         Requires a lambda/C or feature-count search.\n"
"                         default: normalized_mse for ridge (including classify);\n"
"                         logistic uses log_loss; others use ACC/BACC from -metric.\n"
"                         Classification accepts ACC, BACC, and (ridge only)\n"
"                         normalized_mse. Decode/encode accepts normalized_mse,\n"
"                         MSE, RMSE, MAE. Logistic also accepts log_loss and brier,\n"
"                         pooled over inner held-out rows; see conventions below.\n"
"                         ACC/BACC minimize 1-score on pooled inner held-out rows.\n"
"                         Continuous errors pool rows within each response, then\n"
"                         average responses equally; RMSE takes each response's\n"
"                         square root before averaging. These errors retain target\n"
"                         units, so differently scaled targets affect selection.\n"
"                         normalized_mse divides each response's SSE by SSE of\n"
"                         the inner-training-mean baseline, excluding zero-baseline\n"
"                         responses. All excluded: NaN loss and deterministic tie\n"
"                         fallback. Other losses must be finite; failed fits abort.\n"
"                         BACC averages recalls over classes present in pooled\n"
"                         inner tests, not equally over folds. Exact ties prefer\n"
"                         fewer requested features, then larger lambda/smaller C.\n"
"                         Outer scores, permutation tails and feature ranking rules\n"
"                         do not change when the tuning objective changes.\n"
"  -save_tuning FILE      Optional common candidate audit for any nested search.\n"
"                         Includes objective, candidate ID, count, penalty, pooled\n"
"                         test size, valid components, loss and selected flag.\n"
"                         Existing PREFIX.tuning.1D and feature_tuning.1D remain.\n"
"\n"
"  -pca_components K     Fit centered, unwhitened PCA in every training split.\n"
"  -pca_components_grid K1,K2,... Tune distinct positive component counts\n"
"                         jointly with feature counts and lambda/C in inner CV.\n"
"                         Order: nuisance -> scale all inputs -> select -> PCA\n"
"                         -> estimator. PCA always centers using training means,\n"
"                         including with -scale none. In encode mode it projects\n"
"                         design predictors, never response voxels. Defaults off.\n"
"                         Counts clip to numerical rank and NTrain-1, separately\n"
"                         in each split. Rank threshold is 64*DBL_EPSILON*\n"
"                         max(NTrain,NFeat)*largest eigenvalue. Rank zero gives\n"
"                         one zero model input and reports zero components.\n"
"                         Ties: smaller feature count, smaller requested PCA\n"
"                         count, then larger lambda / smaller C. Refit PCA in\n"
"                         every permutation. PREFIX.pca.1D reports observed\n"
"                         fold ranks and sample variances; PREFIX.pca_tuning.1D\n"
"                         reports all nested candidates and inner count ranges.\n"
"  -save_pca_loadings FILE  Optional observed fold component loadings and means,\n"
"                         with original feature IDs. Coordinates are AFTER\n"
"                         nuisance/scaling/selection, not raw input weights.\n"
"                         Largest absolute loading is positive; tied eigenspaces\n"
"                         have no unique basis. Memory scales as folds * input\n"
"                         width * maximum retained count, per worker.\n"
"                         PCA cannot yet accompany -save_coefficients.\n"
"                         Correlation-distance centroid with one retained PC is\n"
"                         degenerate; Euclidean is the default distance.\n"
"\n"
"  -select_features K     Retain the top K predictors per training split.\n"
"  -select_features_grid K1,K2,... Tune distinct positive counts in grouped\n"
"                         inner CV, jointly with lambda/C when tuned. These\n"
"                         options are mutually exclusive; default is all features.\n"
"                         Counts are capped at each unit's available width.\n"
"                         Every training split needs at least two samples.\n"
"                         Classification: rank between-class / total variance\n"
"                         (eta squared). Decode/encode: mean squared Pearson\n"
"                         correlation over nonconstant response columns.\n"
"                         Constant features score zero; exact ties use the lower\n"
"                         original input-column index. Zero-score features can\n"
"                         fill the requested count; scores are not significance.\n"
"                         Order: nuisance OLS, scaling of ALL original columns,\n"
"                         supervised selection, optional PCA, model fit.\n"
"                         sample_center acts before selection and PCA.\n"
"                         Selection is refitted in every inner/outer split and\n"
"                         permutation. Never select once before cross-validation.\n"
"                         Default: ridge tunes normalized squared loss; logistic\n"
"                         tunes log_loss; other\n"
"                         classifiers use 1-ACC/BACC from -metric. -tune_metric\n"
"                         overrides the objective. Ties prefer smaller counts,\n"
"                         then smaller PCA counts (if enabled), then largest\n"
"                         lambda / smallest C. With constant ridge\n"
"                         responses, normalized_mse uses that rule (loss is NaN).\n"
"                         Encoding selects design columns, shared across the\n"
"                         current unit's responses; it does not select output voxels.\n"
"                         Writes PREFIX.features.1D with observed outer-fold\n"
"                         original voxel/node IDs or design names, ranks, scores,\n"
"                         requested/effective counts. With inner CV, also writes\n"
"                         PREFIX.feature_tuning.1D with all count/penalty losses.\n"
"                         With PCA these losses condition on its selected count.\n"
"                         Existing penalty audits condition on both counts.\n"
"                         NFeat in existing maps/tables remains the input width.\n"
"                         Fixed partitions need explicit -inner_cv_column for a\n"
"                         count grid. Incompatible with -save_coefficients.\n"
"\n"
"  -nuisance_columns C1,C2,... Numeric covariates for predictor-side OLS.\n"
"                         Fits intercept and slopes using training rows only,\n"
"                         subtracts their prediction, then applies -scale.\n"
"                         Repeats inside every inner/outer split and null fit.\n"
"                         Adjusts neural predictors in classify/decode; design\n"
"                         predictors in encode. Targets and scores retain their\n"
"                         original units; this is not target residualization,\n"
"                         incremental R2, or a causal/conditional effect test.\n"
"                         Covariates must be known for training AND test rows.\n"
"                         Supply numeric dummy columns for factors, omit one\n"
"                         level and do not add an intercept column. Each training\n"
"                         design must have full rank and ntrain > ncovariates+1.\n"
"                         Cannot reuse target/design columns as nuisances.\n"
"                         With -nperm, all covariate values must be identical\n"
"                         within each -permute_within block. Exchangeability\n"
"                         within those blocks remains the user's assumption.\n"
"                         General residual permutations are not implemented.\n"
"                         -save_coefficients is unavailable with this option\n"
"                         because that export lacks nuisance-fit parameters.\n"
"  -scale S               feature_zscore [default], sample_center, run_center,\n"
"                         or none. All learned values are training-fold only.\n"
"                         run_center requires a run role and cannot be applied\n"
"                         to a run absent from that fold's training data.\n"
"  -metric M              balanced_accuracy [default] or accuracy. This selects\n"
"                         the SCORE map; the ROI table reports both.\n"
"                         Continuous modes default to cross_validated_r2 and\n"
"                         also accept pearson, spearman, MAE, RMSE. R2 uses\n"
"                         each fold's training response mean as its baseline.\n"
"                         All five metrics are saved per target; encoding\n"
"                         pattern summaries weight responses equally. Undefined\n"
"                         scores are NaN in tables and zero in maps; NVALID\n"
"                         reports the primary metric's defined response count.\n"
"\n"
"Continuous estimator options:                               ~1~\n"
"\n"
"  -method elastic_net    Independent response fits for decoding/encoding.\n"
"  -enet_alpha A          Positive penalty strength [1].\n"
"  -enet_alpha_grid A,...  Nested candidates (at least two distinct values).\n"
"  -enet_l1_ratio R       L1 fraction in [0,1] [0.5]; 0=ridge, 1=lasso.\n"
"  -enet_l1_ratio_grid R,...  Nested mixing-ratio candidates.\n"
"                         Objective per response: SSE/(2*NTrain) + A*R*L1 +\n"
"                         A*(1-R)*L2/2; intercept unpenalized. No target scaling.\n"
"                         Alpha differs from ridge's unnormalized lambda:\n"
"                         at R=0, equivalent ridge lambda=NTrain*A.\n"
"                         Grids form the Cartesian product in supplied order,\n"
"                         alpha first, mixing ratio second. No multitask penalty.\n"
"                         Original-feature sparsity requires no PCA. Nuisance\n"
"                         adjustment/scaling/feature selection remain fold-local.\n"
"                         Sparse weights are predictive selections, not voxel\n"
"                         activations, significance, or stable causal effects.\n"
"\n"
"  -method banded_ridge   Encoding only; shared group-penalty vector over responses.\n"
"  -feature_groups FILE   Two-column table with exact header: Group Column\n"
"                         One row per -design_columns entry, in that order.\n"
"                         Every design column belongs to exactly one named group.\n"
"                         These are predictor groups, not sample/CV groups.\n"
"  -banded_penalties FILE Table header lists group names in first-appearance\n"
"                         order. Each row is one positive penalty vector.\n"
"                         One row fixes penalties; multiple rows enable nested\n"
"                         selection. Duplicate vectors/incorrect group order fail.\n"
"                         Objective: SSE + sum_g lambda_g*||beta_g||^2;\n"
"                         intercept unpenalized; no group-size normalization.\n"
"                         -scale applies within training folds before penalties;\n"
"                         default z-scoring is per column, not per group.\n"
"                         Selection/PCA are excluded to preserve group identity.\n"
"                         Group score variance/covariance audits are descriptive:\n"
"                         correlated groups do not have unique causal contributions.\n"
"\n"
"  -method pls            Supervised NIPALS PLS1/PLS2 regression (decode/encode).\n"
"  -pls_components K      Requested components [2].\n"
"  -pls_components_grid K,...  Nested component-count candidates.\n"
"                         Training-center X/Y after predictor preprocessing;\n"
"                         responses are NOT scaled. Use regression deflation\n"
"                         of both X and Y by each X score, unit X weights.\n"
"                         Initial score is the largest-variance residual Y column;\n"
"                         exact ties use its original column order.\n"
"                         Counts clip to min(NTrain-1,NFeat) and exhausted rank/\n"
"                         covariance; constants give a mean-only fit. NIPALS\n"
"                         weight-iterate change must converge, or the fit fails.\n"
"                         Multiple responses are joint within one fit unit:\n"
"                         decoding targets, or the ROI/searchlight neural pattern.\n"
"                         Voxel/node encoding instead fits each response alone;\n"
"                         these response domains need not give the same prediction.\n"
"                         Nested losses average response-wise losses equally;\n"
"                         component fitting itself uses unscaled response covariance.\n"
"\n"
"  -method svr            Linear squared epsilon-insensitive SVR, decoding only.\n"
"  -svr_c C               Positive loss weight [1].\n"
"  -svr_c_grid C,...      Nested C candidates.\n"
"  -svr_epsilon E         Nonnegative tolerance in original target units [0.1].\n"
"  -svr_epsilon_grid E,...  Nested epsilon candidates; Cartesian C/epsilon grid.\n"
"                         Objective per response: (||beta||^2+intercept^2)/2 +\n"
"                         C*sum(max(0,abs(y-prediction)-E)^2). Intercept IS\n"
"                         penalized (constant feature=1); loss is not divided by N.\n"
"                         No target centering/scaling; separate response fits.\n"
"                         Deterministic signed dual coordinate descent verifies\n"
"                         the maximum absolute dual KKT residual. This differs\n"
"                         from classification SVM and nonsquared epsilon loss.\n"
"                         Voxelwise encoding is not exposed pending cost assessment.\n"
"\n"
"  -reg_tol T             Solver tolerance [1e-7], range 1e-12 through 1e-2.\n"
"  -reg_max_iter N        Iteration limit [10000]. Failure to converge is an error.\n"
"                         Elastic net checks max absolute KKT; SVR checks dual\n"
"                         KKT; PLS checks weight-vector change. Banded ridge uses\n"
"                         a direct solve and reports normal-equation residual.\n"
"                         These residuals have different meanings/units.\n"
"\n"
"For these four estimators, parameters are selected jointly inside grouped inner\n"
"CV using -inner_cv_column/-inner_cv_k and -tune_metric. The default is normalized\n"
"MSE; MSE, RMSE and MAE are supported. Exact ties prefer smaller requested\n"
"feature/PCA counts, then FIRST candidate in supplied order (also when all\n"
"normalized losses are undefined). A fixed split with tuning needs explicit\n"
"-inner_cv_column. Do not use ridge -lambda options for these estimators.\n"
"\n"
"Automatic observed audits (also restored after permutations):\n"
"  PREFIX.regression.1D             Outer fit diagnostics and response metrics.\n"
"  PREFIX.regression_scores.1D      Pooled held-out per-response scores.\n"
"  PREFIX.regression_candidates.1D  All nested candidates and solver diagnostics.\n"
"  PREFIX.sparse_features.1D        Elastic net: original IDs, weights, zero flags.\n"
"  PREFIX.group_contributions.1D    Banded ridge: training group-score variance\n"
"                                  and covariance with total predicted score.\n"
"Candidate parameters are recorded in audit headers. Candidate is one-based;\n"
"generic pipeline Penalty fields contain that candidate ID for these methods.\n"
"For elastic net, coefficients use fold-preprocessed predictor units. Selected\n"
"means a nonzero coefficient; selected-out predictors are explicitly zero.\n"
"PLS diagnostics distinguish requested/effective components. Saved fitted-model,\n"
"coefficient-refit, and activation-pattern exports remain ridge-only.\n"
"\n"
"Inference uses the existing exchangeable-row null: decoding shuffles complete\n"
"response rows; encoding shuffles complete design rows within the declared\n"
"blocks. Every preprocessing stage, candidate search and fit is repeated. MAE/\n"
"RMSE use the lower tail; correlations/CV-R2 use the upper tail. Blocking is not\n"
"an autocorrelation model; nuisance values must be constant within each block.\n"
"\n"
"Permutation inference:                                    ~1~\n"
"\n"
"  -nperm N               Run N synchronized label/row relabelings [0: off]. The\n"
"                         identity is included. If the allowed group has at\n"
"                         most N distinct relabelings, it is enumerated exactly.\n"
"  -permute_within COL    Relabel only among rows sharing COL. Required\n"
"                         with -nperm; use 'none' only for an explicitly\n"
"                         unrestricted null. -block_column is an alias. Choose a\n"
"                         column that fully identifies the exchangeability unit,\n"
"                         such as Run, Subj, Fold, or a composite block column.\n"
"  -seed S                Monte-Carlo relabeling seed [1234567]. Exact tests do\n"
"                         not depend on this value. Classification must leave\n"
"                         every CV training fold with all target classes.\n"
"                         Continuous modes shuffle whole target rows (decode)\n"
"                         or design rows (encode), holding neural rows fixed.\n"
"                         All attribute columns move together; blocks and CV\n"
"                         metadata stay fixed. Rows must be exchangeable under\n"
"                         this null; serial time-series dependencies require\n"
"                         an appropriate design, not unrestricted row shuffling.\n"
"                         Preprocessing, tuning, and fitting repeat per null.\n"
"                         Each decoding target has a separate spatial family;\n"
"                         encoding tests its voxel or mean-pattern score.\n"
"                         MAE/RMSE use the lower tail; other metrics the upper.\n"
"                         PREFIX.inference.1D reports all continuous results.\n"
"                         Buckets add NULL_MEAN, ABOVE_NULL, PVAL/ZSCR/QVAL,\n"
"                         PFWE/ZFWE, and NNULL_VALID per target. ABOVE_NULL is\n"
"                         positive for improvement, including lower errors.\n"
"                         Undefined observations have neutral p=1 and z=0;\n"
"                         undefined nulls count conservatively as exceedances.\n"
"\n"
"Classification uses the selected metric's upper-tail label-permutation null.\n"
"The same relabeling slot drives every ROI/searchlight center. Output adds the\n"
"empirical score above null-chance, PVAL/ZSCR, BH QVAL, and max-statistic\n"
"PFWE/ZFWE bricks. ZSCR and ZFWE carry AFNI FIZT statistical codes.\n"
"\n"
"Spatial and output options:                                ~1~\n"
"\n"
"  -roi_sel LIST          Restrict positive mask values with an AFNI int-list.\n"
"                         This option cannot be combined with -searchlight.\n"
"  -searchlight NBHD      Classify a moving volumetric neighborhood at every\n"
"                         nonzero mask voxel. NBHD is a radius in mm or one of\n"
"                         SPHERE(r), RECT(a,b,c), RHDD(r), or TOHD(r).\n"
"                         OpenMP builds use one workspace per worker; set\n"
"                         OMP_NUM_THREADS to control the worker count.\n"
"  -surf SURFACE          Make -searchlight a geodesic surface searchlight on\n"
"                         this mesh (.gii/.asc/.ply/...). With -surf, NBHD must\n"
"                         be one positive radius in mm. Inputs and an optional\n"
"                         mask must be NIML/GIFTI datasets with identical dense\n"
"                         or sparse node-row domains. The output preserves sparse\n"
"                         node indexes. Requires a build with SUMA support.\n"
"  -prefix PREFIX         Output dataset prefix [MVPA].\n"
"                         Without inference the dataset contains SCORE, NFEAT,\n"
"                         and NFOLD. -nperm adds ABOVE_CHANCE, PVAL, ZSCR,\n"
"                         QVAL, PFWE, and ZFWE before NFEAT and NFOLD.\n"
"                         ROI analysis also writes PREFIX.roi.1D with SCORE,\n"
"                         ACC, BACC, NFEAT, and NFOLD for every ROI.\n"
"  -save_patterns PPREFIX Ridge decode/classify: descriptive activation patterns\n"
"                         from each OUTER TRAINING fit, using that fold's selected\n"
"                         lambda/features/PCA. No full-data refit or held-out\n"
"                         covariance. Supports fixed/two-table partitions.\n"
"                         A=Cov(X,S)*pinv(Cov(S,S)), where S is fitted training\n"
"                         scores (not targets), X is all neural inputs after\n"
"                         nuisance/scaling, before selection/PCA. Covariances\n"
"                         are centered sample covariances (NTrain-1).\n"
"                         Patterns use transformed feature units per score unit;\n"
"                         discarded features can have nonzero patterns through\n"
"                         correlation. Class scores use 0/1 ridge coding and are\n"
"                         not probabilities. Normalization is JOINT over outputs;\n"
"                         binary class patterns are opposite half-contrasts.\n"
"                         Pseudoinverse cutoff: 64*DBL_EPSILON*max(NTrain,NOutput)\n"
"                         *largest score eigenvalue. Rank zero yields zero maps\n"
"                         with ScoreRank=0 (no defined score direction).\n"
"                         Writes PPREFIX.patterns.1D, .pattern_covariance.1D and\n"
"                         PPREFIX.unit_ID.fold_N maps (N is one-based), one brick\n"
"                         per target/class, at the original voxel/node IDs.\n"
"                         Folds and overlapping units stay separate; many maps\n"
"                         can be produced. No pattern p-values, causal claims,\n"
"                         or automatic fold averaging. With -nperm only restored\n"
"                         observed patterns are exported. Other estimators and\n"
"                         encode mode are not supported by this option.\n"
"                         These patterns differ from raw predictive weights.\n"
"  -save_coefficients CPREFIX  Ridge only: separate descriptive full-data refit.\n"
"                         Writes CPREFIX.coefficients.1D and .preprocessing.1D.\n"
"                         Encoding without searchlights writes CPREFIX maps: one\n"
"                         brick per design column plus REFIT_INTERCEPT. Other\n"
"                         analyses write CPREFIX.unit_ID per ROI/searchlight: one\n"
"                         brick per decoding target/class (encoding: per design\n"
"                         column plus intercept). Weights occupy neural feature/\n"
"                         response voxels/nodes, preserving sparse node IDs.\n"
"                         Overlapping neighborhoods retain separate models;\n"
"                         this can create many datasets and large audit files.\n"
"                         TSV identifies unit, feature, response, map and brick;\n"
"                         intercepts and all fitted preprocessing are retained.\n"
"                         y = intercept + sum(preprocessed_X * coefficient).\n"
"                         Weights use the requested -scale predictor units;\n"
"                         responses are unscaled (class responses: 0/1).\n"
"                         feature_zscore uses full-data population SD; sample_\n"
"                         center subtracts each row mean; run_center subtracts\n"
"                         full-data subject/run means recorded in preprocessing.\n"
"                         These are descriptive predictive weights, NOT held-out\n"
"                         scores, activation/Haufe maps, or significance tests.\n"
"                         Correlated features and scaling affect weight values.\n"
"  -coef_lambda L         Positive full-data refit penalty [fixed -lambda].\n"
"                         Required with tuned CV; never changes CV penalties,\n"
"                         scores, predictions or permutation inference.\n"
"  -save_predictions FILE Write one held-out prediction row per sample and\n"
"                         spatial unit, including sample metadata, test fold,\n"
"                         observed/predicted class labels, and every class\n"
"                         decision value. Rows never tested are retained as NA.\n"
"  -save_fold_scores FILE Write ACC and BACC for every fold and spatial unit,\n"
"                         with stratum and train/test sample counts. These two\n"
"                         audit tables are opt-in because searchlight tables\n"
"                         can be very large.\n"
"  -save_confusion FILE   Classification only: truth-row confusion counts and\n"
"                         per-class recall for each spatial unit, pooled and\n"
"                         by CV fold, subject, and CV stratum. CountClassN\n"
"                         columns follow the # Class label dictionary.\n"
"                         Subjects use the resolved subject role; strata use\n"
"                         -within_column (All when absent).\n"
"                         CVFoldID is a zero-based fold index; CVStratum is\n"
"                         separate so labels containing '/' remain unambiguous.\n"
"  -save_probability_scores FILE\n"
"                         Logistic only: held-out LogLoss and Brier, pooled and\n"
"                         by fold, subject and stratum. Means weight rows equally.\n"
"                         LogLoss uses natural logs of true-class probabilities,\n"
"                         evaluated stably from logits without clipping.\n"
"                         Brier sums K squared probability errors before averaging\n"
"                         rows: range [0,2], including binary (both classes summed).\n"
"  -save_calibration FILE Logistic only: one-vs-rest reliability bins per class,\n"
"                         pooled across held-out rows. Ten equal-width bins:\n"
"                         [lo,hi), last includes 1; counts, mean probability and\n"
"                         observed class frequency; empty bins are NA.\n"
"                         This assesses raw probabilities; no calibration transform\n"
"                         is fitted. Pooling can hide subject/fold differences.\n"
"                         Probability scores and bins are descriptive, with no\n"
"                         probability-loss inference or population inference.\n"
"  -save_summary_scores FILE\n"
"                         Classification only: ACC/BACC for the same groups,\n"
"                         plus equal-subject and equal-stratum averages.\n"
"                         Pooled ACC weights samples equally; pooled BACC\n"
"                         weights present true classes equally. Equal-group\n"
"                         rows average group scores, excluding empty groups.\n"
"                         Missing true classes have NA recall and are omitted\n"
"                         from that group's BACC. Summaries use original held-\n"
"                         out predictions without refitting; they are descriptive\n"
"                         and do not establish population inference. Both new\n"
"                         audits are opt-in and can be large for searchlights.\n"
"  -show_table            Print the canonical table after input conversion.\n"
"  -show_samples          Print resolved datasets, bricks, and sample roles.\n"
"  -show_folds            Print every training/test row in every fold.\n"
"  -validate_only         Read and resolve the actual analysis inputs, print\n"
"                         all three reports above plus class/fold counts and\n"
"                         spatial coverage, then exit without fitting models\n"
"                         or writing result/audit files. Redirect stdout to\n"
"                         save this report. Errors return nonzero status.\n"
"                         Checks include selected beta values (finite inside\n"
"                         the selected ROIs/neighborhoods), numeric targets,\n"
"                         grids/node domains, and outer/inner training folds.\n"
"                         Values are checked after AFNI loading/conversion;\n"
"                         readers may repair bad floats. This is not a raw\n"
"                         file-integrity check.\n"
"                         Repeated dataset/brick selections are warnings.\n"
"                         Permutation block metadata is checked, but nulls\n"
"                         are not generated or validated. Output paths and\n"
"                         solver convergence are not tested. Supplied labels\n"
"                         and XColumn provenance are not proof of correct\n"
"                         event mapping, anatomical QC or run independence.\n"
"  -quiet                 Suppress informational and progress messages.\n"
"  -progress M            auto, bar, line, or off [auto]. Auto uses an updating\n"
"                         bar on a terminal and 10%% milestone lines when stderr\n"
"                         is redirected. Counts completed analysis locations\n"
"                         (ROIs, searchlight centers, or encoding voxels/nodes),\n"
"                         with throughput, elapsed time and approximate ETA.\n"
"                         Each location includes all folds, tuning and requested\n"
"                         permutations; a single large ROI can take a while\n"
"                         before its first update. -quiet overrides this option.\n"
"\n"
"Saved models (ridge v1; logistic v2):                       ~1~\n"
"\n"
"  -train_only           Fit all labeled input rows and export -save_model.\n"
"                         No test table, partition, outer CV or test labels.\n"
"                         Supports ridge classify/decode and logistic classify.\n"
"                         Tuning requires -inner_cv_column: fit preprocessing\n"
"                         inside each inner split, select parameters, then fit\n"
"                         all rows. Inner scores are model-selection results,\n"
"                         not independent performance estimates. Run separately\n"
"                         per subject unless pooling subjects is intentional.\n"
"                         Writes PREFIX.training.1D (fit counts/penalties), plus\n"
"                         requested tuning/selection/PCA audits. No score maps,\n"
"                         held-out predictions, permutations or pattern export.\n"
"                         -validate_only checks inputs without fitting/writing.\n"
"  -save_model FILE       Export a ridge or logistic fitted model. Requires\n"
"                         -train_only, -partition_column or -testTable, with\n"
"                         volume ROIs/searchlights. With a fixed partition only\n"
"                         training rows enter the model; no full-data refit.\n"
"                         Stores ordered voxel features and image grid/space,\n"
"                         target/class labels, nuisance column names/OLS fit,\n"
"                         fitted scaling, feature selection, PCA and estimator state\n"
"                         including response-specific penalties. Observed state\n"
"                         is saved after permutation restoration. All units are\n"
"                         kept in memory until the model is published atomically.\n"
"                         Export excludes encoding, surface inputs, ordinary multi-\n"
"                         fold CV exports, run_center and -save_coefficients.\n"
"                         Supported scales: none, feature_zscore, sample_center.\n"
"  3dMVPA -apply_model FILE -dataTableFile NEW_TABLE -prefix OUT\n"
"                         Standalone: -apply_model must be first. Also accepts\n"
"                         -dataTable @FILE, -sample_column and -subject_column.\n"
"                         No other analysis/fitted-setting overrides are allowed.\n"
"  3dMVPA -apply_model FILE -apply_dset DSET -prefix OUT\n"
"                         Alternative to a table: predict every sub-brick of an\n"
"                         AFNI HEAD/BRIK or NIfTI dataset. Optional selectors\n"
"                         preserve order and original zero-based brick indices,\n"
"                         e.g. 'cleaned+orig[10..$]' or 'cleaned.nii.gz[8,12,9]'.\n"
"                         Cannot combine with table input or role overrides.\n"
"                         Models requiring nuisance columns need table input.\n"
"                         GIfTI/surface saved models are not yet supported.\n"
"                         NEW_TABLE has InputFile (one volume/brick per row) and\n"
"                         the saved nuisance columns, if any. Targets are optional.\n"
"                         If supplied, observed classes must be known and numeric\n"
"                         targets finite. Neither is used to make predictions.\n"
"                         Inputs must match saved dimensions/view/space and real\n"
"                         IJK-to-DICOM affine (absolute tolerance 1e-5 per entry).\n"
"                         The saved voxel order is used; no mask is redefined.\n"
"                         Writes OUT.applied.1D: unit/sample/response predictions,\n"
"                         predicted class and optional observed target. Class\n"
"                         scores (Prediction) are ridge 0/1 outputs, NOT\n"
"                         probabilities, or logistic logits. Logistic adds a\n"
"                         Probability column: softmax over all trained classes.\n"
"                         Probabilities sum to one per ROI/sample but calibration\n"
"                         on a new data domain is not guaranteed. Exact\n"
"                         ties choose the first stored class. Never fits, tunes,\n"
"                         computes held-out scores or performs inference. Using\n"
"                         training rows here does not make predictions held-out.\n"
"                         Output is long form: one row per ROI x class x sample\n"
"                         (or ROI x continuous response x sample). A labeled\n"
"                         mask trains one model per positive ROI value, using\n"
"                         all its voxels; a binary mask trains one ROI model.\n"
"                         -apply_dset appends SourceBrick, BrickLabel, TimeSeconds.\n"
"                         TimeSeconds uses original dataset time metadata (NA\n"
"                         without a recognized time axis), not stimulus timing.\n"
"                         Whole-run baseline/nuisance cleanup is external; apply\n"
"                         comparable cleanup to training and application data.\n"
"                         All stage parameters and labels are versioned data;\n"
"                         unsupported versions or malformed models are rejected.\n"
"                         Grid checks cannot establish anatomical registration or\n"
"                         acquisition compatibility; no registration is performed.\n"
"\n"
"Training block means and predicting an unlabeled run:         ~2~\n"
"  3dTstat -mean -prefix block01.nii.gz 'cleaned.r01.nii.gz[8..11]'\n"
"  # blocks.1D: Subj Run Condition InputFile; one mean image per block.\n"
"  # rois.nii.gz: positive integers identify distinct, nonoverlapping ROIs.\n"
"  3dMVPA -train_only -mode classify -dataTable @blocks.1D \\\n"
"         -target_column Condition -mask rois.nii.gz -method logistic \\\n"
"         -logistic_lambda 1 -save_model decoder.mvpa -prefix training\n"
"  3dMVPA -apply_model decoder.mvpa -apply_dset cleaned.r03.nii.gz \\\n"
"         -prefix predictions\n"
"  # predictions.applied.1D: ROI x class x TR logits and probabilities.\n"
"  # For ridge scores instead, replace -method logistic -logistic_lambda 1\n"
"  # with -method ridge -lambda 1 and use a different model/prefix.\n"
"  # Optional tuning: -logistic_lambda auto -inner_cv_column Run;\n"
"  # ridge uses -lambda auto -inner_cv_column Run. Each inner training\n"
"  # split must contain every class. Averaged-block tuning does not measure\n"
"  # single-TR prediction accuracy; independent labeled TRs are needed for that.\n"
"\n"
"Standalone paired model comparisons:                         ~1~\n"
"\n"
"  3dMVPA -compare_predictions A.pred.1D B.pred.1D -prefix CMP\n"
"                         Compare two -save_predictions audits without refitting.\n"
"                         Must be the first option; only -prefix may follow.\n"
"                         Supports classification or continuous decode/encode;\n"
"                         compare the same targets and spatial unit definitions.\n"
"                         Matches complete unit/response/sample domains, including\n"
"                         training-only rows, regardless of audit row order.\n"
"                         Subject/run/item/input-fold, test fold/stratum, observed\n"
"                         outcomes and tested status must agree. Continuous runs\n"
"                         must have identical training-mean baselines for CV-R2.\n"
"                         Available spatial metadata must also agree. Source\n"
"                         table paths/row numbers are recorded, not identity keys.\n"
"                         Use stable explicit -sample_column IDs if input tables\n"
"                         are reordered. No silent intersection or missing pairs.\n"
"                         Writes CMP.comparison.1D and CMP.paired_predictions.1D.\n"
"                         Reports pooled, fold, subject and stratum scores: ACC/\n"
"                         BACC, or Pearson/Spearman/CV-R2/MAE/RMSE per response.\n"
"                         DeltaBminusA is B-A; ImprovementB is positive when B\n"
"                         improves (A-B for MAE/RMSE, B-A for other metrics).\n"
"                         Undefined metrics/differences stay NA. Pooled scores\n"
"                         use all held-out rows, not an average of fold scores.\n"
"                         Only saved predictions are used; original fits, image\n"
"                         registration and ROI definitions cannot be verified.\n"
"                         Descriptive only: no p-values, confidence intervals,\n"
"                         population inference, spatial maps or model selection.\n"
"\n"
"Preparing trial betas:                                      ~1~\n"
"\n"
"3dMVPA takes existing response maps, not BOLD time series. A short LSS route:\n"
"\n"
"  1. afni_proc.py: preprocess all runs onto a common grid; inspect alignment,\n"
"     censoring and masks. Retain each run's full-length scaled EPI.\n"
"\n"
"  2. Build an IM design with 3dDeconvolve (directly or via afni_proc.py).\n"
"     For LSS, put ALL trials in one -stim_times_IM stimulus; keep category\n"
"     labels separately. In afni_proc.py, use -regress_stim_types IM and\n"
"     -regress_3dD_stop to write X.xmat.1D and X.nocensor.xmat.1D without\n"
"     fitting the full GLM. Choose timing/basis/nuisance options for your study.\n"
"     Alternative LSA route: fit the IM GLM with 3dDeconvolve/3dREMLfit and\n"
"     select each trial's actual _Coef brick; skip the LSS steps below.\n"
"\n"
"  3. 1dXmatTool: check and split the multi-run DESIGN before running 3dLSS.\n"
"     From the results directory (Bash syntax):\n"
"\n"
"       mkdir -p split lss tables\n"
"       1dXmatTool -infile X.xmat.1D -uncensored X.nocensor.xmat.1D \\\n"
"                  -validate -for lss\n"
"       1dXmatTool -infile X.xmat.1D -uncensored X.nocensor.xmat.1D \\\n"
"                  -split_runs -prefix split/X\n"
"\n"
"     Local censoring and trial-column mappings are preserved in the split\n"
"     matrices and .rows.1D/.columns.1D tables. Review failed checks first.\n"
"\n"
"  4. 3dLSS: fit each run separately. Adapt run list and EPI filenames:\n"
"\n"
"       for run in 01 02 03 04; do\n"
"           3dLSS -matrix split/X.r${run}.xmat.1D \\\n"
"                 -input pb03.s01.r${run}.scale+orig \\\n"
"                 -mask mask_epi_anat.s01+orig -prefix lss/LSS.r${run}.nii.gz\n"
"       done\n"
"\n"
"     The scale-block number (pb03 here) depends on your afni_proc.py blocks.\n"
"     Use FULL-length per-run EPI; 3dLSS applies the local GoodList censoring.\n"
"\n"
"  5. 1dXmatTool -> 3dMVPA: map trials to estimates, then inspect the folds.\n"
"     Supply events.1D with Run Volume Condition [Item]: Run is one-based;\n"
"     Volume is the zero-based LSS volume within that run, in IM-column order.\n"
"     Category labels come from the trial log, not the single IM stimulus name.\n"
"\n"
"       1dXmatTool -infile X.xmat.1D -uncensored X.nocensor.xmat.1D \\\n"
"                  -export_manifest -events events.1D -subject s01 \\\n"
"                  -beta_pattern 'lss/LSS.r{run}.nii.gz' -prefix tables/lss\n"
"       3dMVPA -mode classify -trial_manifest tables/lss.trials.1D \\\n"
"              -target_column Condition -cv_column Run \\\n"
"              -mask mask_epi_anat.s01+orig -validate_only\n"
"\n"
"     To write InputFile[volume] rows for -dataTable instead, add\n"
"     -manifest_format selected to the export command. Add numeric targets\n"
"     or design predictors by matching trial identities for decode/encode.\n"
"     LSS can mix neighboring trials: keep separately estimated runs together\n"
"     in CV. Fold-local MVPA preprocessing cannot undo upstream dependence.\n"
"\n"
"Numbered examples:                                          ~1~\n"
"\n"
"Run-CV examples (1, 2, 4, 5, 7-9) fit each subject independently with\n"
"-cv_column Run -within_column Subj; provide at least four runs per subject.\n"
"Each classification run contains every class. Run IDs include the subject\n"
"(s01_r01, s02_r01), so permutation blocks never mix subjects.\n"
"For held-out-subject prediction, replace those two options with -cv_column\n"
"Subj; if tuning, also use -inner_cv_column Subj and enough training subjects.\n"
"Replace filenames and column names with your data.\n"
"Use -validate_only with an analysis command to inspect inputs before fitting.\n"
"\n"
"samples.1D has one row per trial; InputFile selects one beta brick. For example:\n"
"\n"
"  Subj Run     Item     Condition Rating Arousal Motion Luminance Contrast Shape InputFile\n"
"  s01  s01_r01 s01_t001 face      4.2    2.1     0.1    0.8       0.3      1.2   s01/lss/LSS.r01.nii.gz[0]\n"
"  s01  s01_r01 s01_t002 house     2.3    1.4     0.2    0.4       0.7      0.5   s01/lss/LSS.r01.nii.gz[1]\n"
"  s02  s02_r01 s02_t001 face      3.8    2.5     0.2    0.8       0.3      1.2   s02/lss/LSS.r01.nii.gz[0]\n"
"  s02  s02_r01 s02_t002 house     2.7    1.6     0.1    0.4       0.7      0.5   s02/lss/LSS.r01.nii.gz[1]\n"
"\n"
"Include all trials/runs for each subject, with unique Item labels here.\n"
"Condition is categorical; Rating/Arousal are numeric targets; Motion is a\n"
"numeric nuisance covariate. Luminance/Contrast/Shape are numeric encoding\n"
"predictors. Include only columns your command needs. All maps/masks must\n"
"share the feature grid.\n"
"rois.nii.gz is a labeled ROI mask (a binary mask is one ROI); brain.nii.gz is\n"
"a binary search mask. Paths are relative to the current working directory.\n"
"Pooled scores summarize held-out samples; they are not population inference.\n"
"\n"
"1. Classification: ROI baseline and error audits.            ~2~\n"
"\n"
"  3dMVPA -mode classify -dataTable @samples.1D \\\n"
"         -target_column Condition -cv_column Run -within_column Subj -mask rois.nii.gz \\\n"
"         -method centroid -scale feature_zscore -metric balanced_accuracy \\\n"
"         -save_predictions class_roi.pred.1D -save_confusion class_roi.confusion.1D \\\n"
"         -prefix class_roi\n"
"\n"
"Each subject/run is held out once. Read class_roi.roi.1D and the confusion table.\n"
"\n"
"2. Classification: volume searchlight with permutation inference. ~2~\n"
"\n"
"  3dMVPA -mode classify -dataTable @samples.1D \\\n"
"         -target_column Condition -cv_column Run -within_column Subj -mask brain.nii.gz \\\n"
"         -searchlight 'SPHERE(5)' -method svm -svm_c 1 -scale feature_zscore \\\n"
"         -metric balanced_accuracy -nperm 5000 -permute_within Run -seed 314159 \\\n"
"         -prefix class_search\n"
"\n"
"The radius is 5 mm. Within-run shuffling must suit your experiment's null.\n"
"Maps include BACC, PVAL and PFWE; use ZSCR/ZFWE for positive-only AFNI thresholds.\n"
"Without -nperm, scores describe prediction performance without significance.\n"
"\n"
"3. Classification: separate training/test tables and nested ridge tuning. ~2~\n"
"\n"
"  3dMVPA -mode classify -dataTable @training.1D -testTable @testing.1D \\\n"
"         -target_column Condition -mask rois.nii.gz -method ridge \\\n"
"         -lambda auto -inner_cv_column Run -tune_metric BACC \\\n"
"         -scale feature_zscore -save_predictions class_test.pred.1D \\\n"
"         -save_tuning class_test.candidates.1D -prefix class_test\n"
"\n"
"Tables have the same columns and distinct sample/brick identities. Reserve\n"
"whole runs/sessions/subjects as appropriate; training needs multiple inner\n"
"Run groups and every class. This fit pools training rows across subjects;\n"
"only test rows are scored. For one table, use -partition_column Split with\n"
"train/test values (example 6).\n"
"\n"
"4. Decoding: predict two continuous attributes from ROI patterns. ~2~\n"
"\n"
"  3dMVPA -mode decode -dataTable @samples.1D \\\n"
"         -target_columns Rating,Arousal -cv_column Run -within_column Subj -mask rois.nii.gz \\\n"
"         -method ridge -lambda_per_response -inner_cv_column Run \\\n"
"         -scale feature_zscore -metric cross_validated_r2 \\\n"
"         -save_predictions decode_roi.pred.1D -prefix decode_roi\n"
"\n"
"Each target gets its own training-selected penalty. Inspect CVR2, correlation\n"
"and errors in decode_roi.continuous.1D; negative CVR2 is possible.\n"
"\n"
"5. Decoding: searchlight with nuisance adjustment, selection and PCA. ~2~\n"
"\n"
"  3dMVPA -mode decode -dataTable @samples.1D -target_column Rating \\\n"
"         -cv_column Run -within_column Subj -mask brain.nii.gz -searchlight 'SPHERE(5)' \\\n"
"         -method ridge -lambda auto -inner_cv_column Run \\\n"
"         -nuisance_columns Motion -scale feature_zscore \\\n"
"         -select_features 50 -pca_components 5 -prefix decode_search\n"
"\n"
"Training-only order: nuisance OLS -> scaling -> selection -> PCA -> ridge.\n"
"Counts clip to available features/rank. Motion must be a valid, full-rank\n"
"covariate in every training split. Nuisance adjustment acts on neural inputs.\n"
"\n"
"6. Decoding: fixed holdout, save the fitted pipeline, apply to new samples. ~2~\n"
"\n"
"  3dMVPA -mode decode -dataTable @partitioned.1D -target_column Rating \\\n"
"         -partition_column Split -mask rois.nii.gz -method ridge \\\n"
"         -lambda auto -inner_cv_column Run -scale feature_zscore \\\n"
"         -save_model rating.mvpa -save_predictions decode_test.pred.1D \\\n"
"         -prefix decode_test\n"
"  3dMVPA -apply_model rating.mvpa -dataTableFile new_samples.1D \\\n"
"         -prefix rating_new\n"
"\n"
"partitioned.1D adds Split=train/test to the sample table. Only training rows\n"
"fit the saved pipeline, pooled across subjects. New samples need matching\n"
"grids, but no Rating column.\n"
"rating_new.applied.1D contains predictions; application never refits or scores.\n"
"\n"
"7. Encoding: stimulus features predict each voxel's response. ~2~\n"
"\n"
"  3dMVPA -mode encode -dataTable @samples.1D \\\n"
"         -design_columns Luminance,Contrast,Shape -cv_column Run -within_column Subj \\\n"
"         -mask brain.nii.gz -score voxel -method ridge -lambda 1 \\\n"
"         -scale feature_zscore -metric cross_validated_r2 -prefix encode_voxel\n"
"\n"
"Scaling acts on design predictors. Output maps score each voxel's held-out\n"
"prediction; use beta coefficients as InputFile responses, not t-statistics.\n"
"\n"
"8. Encoding: ROI pattern scores with response-specific regularization. ~2~\n"
"\n"
"  3dMVPA -mode encode -dataTable @samples.1D \\\n"
"         -design_columns Luminance,Contrast,Shape -cv_column Run -within_column Subj \\\n"
"         -mask rois.nii.gz -score pattern -method ridge -lambda_per_response \\\n"
"         -inner_cv_column Run -scale feature_zscore \\\n"
"         -save_tuning encode_pattern.candidates.1D -prefix encode_pattern\n"
"\n"
"Each response voxel gets a penalty; pattern scores average defined voxel\n"
"metrics within each ROI. This is not correlation between spatial patterns.\n"
"\n"
"9. Encoding: test the feature-to-response association by permutation. ~2~\n"
"\n"
"  3dMVPA -mode encode -dataTable @samples.1D \\\n"
"         -design_columns Luminance,Contrast,Shape -cv_column Run -within_column Subj \\\n"
"         -mask brain.nii.gz -score voxel -method ridge -lambda 1 \\\n"
"         -scale feature_zscore -nperm 5000 -permute_within Run -seed 314159 \\\n"
"         -prefix encode_perm\n"
"\n"
"Whole design rows shuffle within Run; predictor columns move together.\n"
"Use only when those rows are exchangeable. See encode_perm.inference.1D\n"
"and PVAL/PFWE maps; this is not a population test across subjects.\n"
"\n"
"Output tables use .1D with # Columns: headers and mixed text/numeric fields;\n"
"read them as tables. Explicit -save_* filenames are honored as given.\n"
"\n"
"10. Cross-condition generalization: visual -> auditory within each subject. ~2~\n"
"\n"
"  3dMVPA -mode classify -dataTable @transfer.1D -target_column Condition \\\n"
"         -transfer_column Task -train_domain visual -test_domain auditory \\\n"
"         -transfer_subjects within -transfer_items allow \\\n"
"         -transfer_sessions allow -mask rois.nii.gz -method ridge \\\n"
"         -lambda auto -inner_cv_column Run -scale feature_zscore \\\n"
"         -save_predictions transfer.pred.1D -prefix transfer\n"
"\n"
"transfer.1D adds Task and Session, with canonical Item IDs. This example\n"
"intentionally permits repeated items and same-session transfer. Each subject\n"
"needs both tasks and multiple source Run groups. Inspect transfer.transfer.1D.\n"
"For held-out-subject transfer use -transfer_subjects disjoint, with entirely\n"
"different destination subjects. Reverse the domain values in a separate run\n"
"with a new prefix to test the reverse direction; results need not be symmetric.\n"
"\n"
"11. Classification: tune class probabilities and assess calibration. ~2~\n"
"\n"
"  3dMVPA -mode classify -dataTable @samples.1D -target_column Condition \\\n"
"         -cv_column Run -within_column Subj -mask rois.nii.gz \\\n"
"         -method logistic -logistic_lambda auto -inner_cv_column Run \\\n"
"         -save_predictions logistic.pred.1D -save_fold_scores logistic.fold.1D \\\n"
"         -save_probability_scores logistic.scores.1D \\\n"
"         -save_calibration logistic.calibration.1D -prefix logistic\n"
"\n"
"Nested selection minimizes held-out log loss. Inspect probability scores and\n"
"reliability bins alongside ACC/BACC; probabilities need not be well calibrated.\n"
"Use -tune_metric brier to select lambda by squared probability error instead.\n"
"\n"
"12. Sparse continuous decoding (elastic net):                ~1~\n"
"\n"
"  3dMVPA -mode decode -dataTable @samples.1D -target_columns Rating,Arousal \\\n"
"         -cv_column Run -within_column Subj -mask rois.nii.gz \\\n"
"         -method elastic_net -enet_alpha_grid .01,.1,1 \\\n"
"         -enet_l1_ratio_grid .25,.75,1 -inner_cv_column Run \\\n"
"         -metric cross_validated_r2 -save_predictions sparse.pred.1D \\\n"
"         -prefix sparse\n"
"\n"
"Inspect sparse.sparse_features.1D alongside held-out response scores. Different\n"
"folds may choose different voxels when features are correlated.\n"
"\n"
"13. Named feature-space encoding (banded ridge):             ~1~\n"
"\n"
"For -design_columns Visual1,Visual2,Semantic1,Semantic2, groups.1D contains:\n"
"  Group    Column\n"
"  visual   Visual1\n"
"  visual   Visual2\n"
"  semantic Semantic1\n"
"  semantic Semantic2\n"
"\n"
"penalties.1D lists explicit candidate vectors, with the same group order:\n"
"  visual semantic\n"
"  0.1    10\n"
"  1      1\n"
"  10     0.1\n"
"\n"
"  3dMVPA -mode encode -dataTable @samples.1D \\\n"
"         -design_columns Visual1,Visual2,Semantic1,Semantic2 \\\n"
"         -cv_column Run -mask brain.nii.gz -score voxel \\\n"
"         -method banded_ridge -feature_groups groups.1D \\\n"
"         -banded_penalties penalties.1D -inner_cv_column Run -prefix banded\n"
"\n"
"14. Joint-target supervised components (PLS regression):     ~1~\n"
"\n"
"  3dMVPA -mode decode -dataTable @samples.1D -target_columns Rating,Arousal \\\n"
"         -cv_column Run -within_column Subj -mask rois.nii.gz \\\n"
"         -method pls -pls_components_grid 1,2,4 -inner_cv_column Run \\\n"
"         -metric RMSE -save_predictions pls.pred.1D -prefix pls\n"
"\n"
"Response units affect the joint PLS components. Report those units and inspect\n"
"retained counts in pls.regression.1D; PLS is distinct from unsupervised PCA.\n"
"\n"
"15. Epsilon-insensitive behavioral prediction (linear SVR):  ~1~\n"
"\n"
"  3dMVPA -mode decode -dataTable @samples.1D -target_column Rating \\\n"
"         -cv_column Run -within_column Subj -mask rois.nii.gz \\\n"
"         -method svr -svr_c_grid .1,1,10 -svr_epsilon_grid 0,.1,.5 \\\n"
"         -inner_cv_column Run -tune_metric MAE -metric MAE \\\n"
"         -save_predictions svr.pred.1D -prefix svr\n"
"\n"
"Epsilon has Rating's units. Compare aligned held-out predictions with the\n"
"ridge baseline; changing the loss does not guarantee improved prediction.\n"
"\n"
"References:                                                 ~1~\n"
"\n"
"Selected methodological references; cite the papers relevant to your analysis.\n"
"The options above specify 3dMVPA's objectives, scaling and inference contracts.\n"
"\n"
"* Cox (1996), Comput Biomed Res 29:162-173. [AFNI]\n"
"  AFNI: software for analysis and visualization of functional magnetic\n"
"  resonance neuroimages. https://doi.org/10.1006/cbmr.1996.0014\n"
"* Norman, Polyn, Detre & Haxby (2006), Trends Cogn Sci 10:424-430. [MVPA]\n"
"  Beyond mind-reading: multi-voxel pattern analysis of fMRI data.\n"
"  https://doi.org/10.1016/j.tics.2006.07.005\n"
"* Kriegeskorte, Goebel & Bandettini (2006), PNAS 103:3863-3868. [searchlights]\n"
"  Information-based functional brain mapping.\n"
"  https://doi.org/10.1073/pnas.0600244103\n"
"* Varma & Simon (2006), BMC Bioinformatics 7:91. [nested cross-validation]\n"
"  Bias in error estimation when using cross-validation for model selection.\n"
"  https://doi.org/10.1186/1471-2105-7-91\n"
"* Hoerl & Kennard (1970), Technometrics 12:55-67. [ridge regression]\n"
"  Ridge regression: biased estimation for nonorthogonal problems.\n"
"  https://doi.org/10.1080/00401706.1970.10488634\n"
"* Zou & Hastie (2005), J R Stat Soc B 67:301-320. [elastic net]\n"
"  Regularization and variable selection via the elastic net.\n"
"  https://doi.org/10.1111/j.1467-9868.2005.00503.x\n"
"* Friedman, Hastie & Tibshirani (2010), J Stat Softw 33(1):1-22.\n"
"  Regularization paths for generalized linear models via coordinate descent.\n"
"  https://doi.org/10.18637/jss.v033.i01 [penalized regression/logistic models]\n"
"* Hsieh et al. (2008), ICML:408-415. [linear SVM dual coordinate descent]\n"
"  A dual coordinate descent method for large-scale linear SVM.\n"
"  https://doi.org/10.1145/1390156.1390208\n"
"* Ho & Lin (2012), J Mach Learn Res 13:3323-3348. [linear SVR]\n"
"  Large-scale linear support vector regression.\n"
"  https://www.jmlr.org/papers/v13/ho12a.html\n"
"* Dupre la Tour, Eickenberg, Nunez-Elizalde & Gallant (2022),\n"
"  NeuroImage 264:119728. [banded ridge encoding]\n"
"  Feature-space selection with banded ridge regression.\n"
"  https://doi.org/10.1016/j.neuroimage.2022.119728\n"
"* Wold, Sjostrom & Eriksson (2001), Chemom Intell Lab Syst 58:109-130.\n"
"  PLS-regression: a basic tool of chemometrics. [PLS regression]\n"
"  https://doi.org/10.1016/S0169-7439(01)00155-1\n"
"* Jolliffe & Cadima (2016), Philos Trans R Soc A 374:20150202. [PCA]\n"
"  Principal component analysis: a review and recent developments.\n"
"  https://doi.org/10.1098/rsta.2015.0202\n"
"* Haufe et al. (2014), NeuroImage 87:96-110. [activation patterns]\n"
"  On the interpretation of weight vectors of linear models in multivariate\n"
"  neuroimaging. https://doi.org/10.1016/j.neuroimage.2013.10.067\n"
"* Winkler et al. (2014), NeuroImage 92:381-397.\n"
"  Permutation inference for the general linear model.\n"
"  [permutation inference and exchangeability]\n"
"  https://doi.org/10.1016/j.neuroimage.2014.01.060\n"
"* Stelzer, Chen & Turner (2013), NeuroImage 65:69-82.\n"
"  Statistical inference and multiple testing correction in classification-based\n"
"  multi-voxel pattern analysis (MVPA): random permutations and cluster size\n"
"  control. [MVPA searchlight inference background]\n"
"  https://doi.org/10.1016/j.neuroimage.2012.09.063\n"
"* Phipson & Smyth (2010), Stat Appl Genet Mol Biol 9:39.\n"
"  Permutation P-values should never be zero: calculating exact P-values\n"
"  when permutations are randomly drawn. [nonzero permutation p-values]\n"
"  https://doi.org/10.2202/1544-6115.1585\n"
"* Nichols & Holmes (2002), Hum Brain Mapp 15:1-25. [permutation/max-FWE]\n"
"  Nonparametric permutation tests for functional neuroimaging:\n"
"  a primer with examples. https://doi.org/10.1002/hbm.1058\n"
"* Benjamini & Hochberg (1995), J R Stat Soc B 57:289-300. [FDR]\n"
"  Controlling the false discovery rate: a practical and powerful approach\n"
"  to multiple testing. https://doi.org/10.1111/j.2517-6161.1995.tb02031.x\n"
"\n"
"-- P Molfese, December 2026\n"
) ;
   PRINT_AFNI_OMP_USAGE(PROGRAM_NAME,NULL) ;
   PRINT_COMPILE_DATE ;
}

static long mvpa_parse_long( const char *opt, const char *arg, long lo, long hi )
{
   char *ep ; long vv ; errno=0 ; vv=strtol(arg,&ep,10) ;
   if( errno==ERANGE || ep==arg || *ep!='\0' || vv<lo || vv>hi )
     ERROR_exit(PROGRAM_NAME ": %s needs an integer in [%ld,%ld], not '%s'",opt,lo,hi,arg) ;
   return vv ;
}

#ifdef USE_SUMA
static float mvpa_parse_positive_float( const char *opt, const char *arg )
{
   char *ep ; double vv ; errno=0 ; vv=strtod(arg,&ep) ;
   if( errno==ERANGE || ep==arg || *ep!='\0' || !isfinite(vv) ||
       vv<=0.0 || vv>FLT_MAX )
     ERROR_exit(PROGRAM_NAME ": %s needs one finite number > 0, not '%s'",opt,arg) ;
   return (float)vv ;
}
#endif

static void mvpa_nuisance_header(FILE *fp,const char *columns)
{
   if(fp && columns)fprintf(fp,"# Nuisance: predictors_only_OLS; columns=%s; intercept=yes; fit=each_training_split; order=nuisance_then_scale; targets=original_units\n# Nuisance permutation contract: covariates fixed within exchangeability blocks; no residual-permutation or causal interpretation.\n",columns);
}

static void mvpa_write_roi_table( const char *fname, const THD_roilist *rl,
                                  const float *score, const float *acc,
                                  const float *bacc, const float *nfeat,
                                  const float *nfold, const char *metric,
                                  const char *target, char **class_label,
                                  int nclass, const PERM_result *pr,
                                  const float *qval,
                                  const char *permute_within,const char *nuisance_columns )
{
   FILE *fp ; int rr,cc ;
   if( THD_deathcon() && THD_is_file((char *)fname) )
     ERROR_exit(PROGRAM_NAME ": ROI table '%s' already exists",fname) ;
   fp=fopen(fname,"w") ;
   if( fp==NULL ) ERROR_exit(PROGRAM_NAME ": cannot write ROI table '%s'",fname) ;
   mvpa_nuisance_header(fp,nuisance_columns);
   fprintf(fp,"# 3dMVPA ROI classification; target=%s; primary_metric=%s\n",target,metric) ;
   fprintf(fp,"# Classes:") ;
   for( cc=0 ; cc<nclass ; cc++ ) fprintf(fp," %d=%s",cc,class_label[cc]) ;
   if( pr!=NULL )
     fprintf(fp,"\n# Label permutations: %d; within=%s; tail=upper; FWE_family=%d_ROIs",
             pr->nperm,permute_within,rl->nroi) ;
   fprintf(fp,"\n# Columns: ROI\tLabel\tSCORE\tACC\tBACC") ;
   if( pr!=NULL ) fputs("\tABOVE_CHANCE\tPVAL\tZSCR\tQVAL\tPFWE\tZFWE",fp) ;
   fputs("\tNFEAT\tNFOLD\n",fp) ;
   for( rr=0 ; rr<rl->nroi ; rr++ ){
     fprintf(fp,"%d\t%s\t%.9g\t%.9g\t%.9g",
             rl->val[rr],rl->lab[rr]?rl->lab[rr]:"-",score[rr],acc[rr],bacc[rr]) ;
     if( pr!=NULL )
       fprintf(fp,"\t%.9g\t%.9g\t%.9g\t%.9g\t%.9g\t%.9g",
               pr->effect[rr],pr->p_unc[rr],pr->z_unc[rr],qval[rr],
               pr->p_fwe[rr],pr->z_fwe[rr]) ;
     fprintf(fp,"\t%.0f\t%.0f\n",nfeat[rr],nfold[rr]) ;
   }
   if( fclose(fp)!=0 ) ERROR_exit(PROGRAM_NAME ": error closing ROI table '%s'",fname) ;
}

static THD_3dim_dataset * mvpa_make_roi_map( THD_3dim_dataset *mset,
                                             const THD_roilist *rl,
                                             const char *prefix,
                                             const float *score,
                                             const float *nfeat,
                                             const float *nfold,
                                             const PERM_result *pr,
                                             const float *qval,
                                             const char *score_label,
                                             int argc, char **argv )
{
   const float *value[9] ;
   const char *label[9] ;
   char domain_err[512] ;
   int is_z[9]={0,0,0,0,0,0,0,0,0},nval=0,bb ;
   THD_3dim_dataset *oset=EDIT_empty_copy(mset) ;
   if( oset==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate the output dataset") ;
   value[nval]=score ; label[nval++]=score_label ;
   if( pr!=NULL ){
     value[nval]=pr->effect ; label[nval++]="ABOVE_CHANCE" ;
     value[nval]=pr->p_unc  ; label[nval++]="PVAL" ;
     value[nval]=pr->z_unc  ; label[nval]="ZSCR" ; is_z[nval++]=1 ;
     value[nval]=qval       ; label[nval++]="QVAL" ;
     value[nval]=pr->p_fwe  ; label[nval++]="PFWE" ;
     value[nval]=pr->z_fwe  ; label[nval]="ZFWE" ; is_z[nval++]=1 ;
   }
   value[nval]=nfeat ; label[nval++]="NFEAT" ;
   value[nval]=nfold ; label[nval++]="NFOLD" ;
   EDIT_dset_items(oset,ADN_prefix,prefix,ADN_nvals,nval,ADN_ntt,0,
                   ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_none) ;
   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(oset)) )
     ERROR_exit(PROGRAM_NAME ": output dataset %s already exists",DSET_HEADNAME(oset)) ;
   for( bb=0 ; bb<nval ; bb++ ){
     EDIT_substitute_brick(oset,bb,MRI_float,NULL) ;
     THD_roilist_paint(DSET_ARRAY(oset,bb),rl,value[bb]) ;
     EDIT_BRICK_LABEL(oset,bb,(char *)label[bb]) ;
     if( is_z[bb] ) EDIT_BRICK_TO_FIZT(oset,bb) ;
     else EDIT_BRICK_TO_NOSTAT(oset,bb) ;
     EDIT_BRICK_FACTOR(oset,bb,0.0f) ;
   }
   THD_copy_labeltable_atr(oset->dblk,mset->dblk) ;
   if( !THD_copy_surface_node_domain(oset,mset,domain_err,sizeof(domain_err)) )
     ERROR_exit(PROGRAM_NAME ": %s",domain_err) ;
   tross_Copy_History(mset,oset) ; tross_Make_History(PROGRAM_NAME,argc,argv,oset) ;
   return oset ;
}

static void mvpa_1D_text(FILE *fp,const char *text)
{ THD_1D_print_token(fp,text); }

/* Append fields only for two-table input, leaving existing audit schemas intact.
   Original source rows are stamped before Xmat expansion, so one source row may
   correspond to several canonical TableRows/Items. */
static void mvpa_source_fields(FILE *fp,const THD_sampleset *ss,int row)
{
   const char *names[]={MVPA_PARTITION,MVPA_SOURCE_TABLE,MVPA_SOURCE_ROW};int j;
   if(row<0){fputs("\tPartition\tSourceTable\tSourceRow",fp);return;}
   for(j=0;j<3;j++){
      int col=THD_datatable_column(ss->metadata,(char *)names[j]);
      if(col<0)ERROR_exit(PROGRAM_NAME ": missing two-table provenance %s",names[j]);
      fputc('\t',fp);mvpa_1D_text(fp,DT_CELL(ss->metadata,ss->table_row[row],col));
   }
}

static FILE * mvpa_open_audit( const char *fname, const char *what )
{
   FILE *fp ;
   if( fname==NULL || fname[0]=='\0' )
     ERROR_exit(PROGRAM_NAME ": %s needs a nonempty filename",what) ;
   if( THD_deathcon() && THD_is_file((char *)fname) )
     ERROR_exit(PROGRAM_NAME ": %s file '%s' already exists",what,fname) ;
   fp=fopen(fname,"w") ;
   if( fp==NULL ) ERROR_exit(PROGRAM_NAME ": cannot write %s file '%s'",what,fname) ;
   return fp ;
}

static void mvpa_write_audit_header( FILE *fp, const char *kind,
                                     const char *target, char **class_label,
                                     int nclass, int searchlight, int surface )
{
   int cc ;
   fprintf(fp,"# 3dMVPA %s\n# Target\t",kind) ; mvpa_1D_text(fp,target) ; fputc('\n',fp) ;
   for( cc=0 ; cc<nclass ; cc++ ){
     fprintf(fp,"# Class\t%d\t",cc) ; mvpa_1D_text(fp,class_label[cc]) ; fputc('\n',fp) ;
   }
   if( surface )
     fputs("# Surface searchlight UnitID is the mesh node index; I/J/K are NA.\n",fp) ;
   else if( searchlight )
     fputs("# Searchlight UnitID and I/J/K are zero-based center indexes.\n",fp) ;
}

static void mvpa_write_spatial_prefix( FILE *fp, THD_3dim_dataset *mset,
                                       const THD_roilist *rl, int rr,
                                       int searchlight, int surface )
{
   int idx=rl->center[rr] ;
   fputs(surface?"Node\t":searchlight?"Voxel\t":"ROI\t",fp) ;
   fprintf(fp,"%d\t",rl->val[rr]) ;
   mvpa_1D_text(fp,rl->lab[rr]?rl->lab[rr]:"NA") ; fputc('\t',fp) ;
   if( surface ) fputs("NA\tNA\tNA\t",fp) ;
   else if( searchlight ){
     int nx=DSET_NX(mset),nxy=nx*DSET_NY(mset) ;
     fprintf(fp,"%d\t%d\t%d\t",idx%nx,(idx%nxy)/nx,idx/nxy) ;
   } else fputs("NA\tNA\tNA\t",fp) ;
   fprintf(fp,"%d",rl->vox[rr].nar) ;
}

static const char * mvpa_level( char **label, int index )
{
   return (index>=0 && label!=NULL && label[index]!=NULL) ? label[index] : "NA" ;
}

/* Reporting scratch is shared only in the ordered output region. Reuse the
   library scorer on selected held-out rows; never fit a model for a summary. */
typedef struct {
   int *truth, *prediction, *confusion;
   double *recall;
} mvpa_class_audit;

static void mvpa_write_fold_identity( FILE *fp, int fold, const char *stratum )
{
   if(fold>=0) fprintf(fp,"\t%d",fold);
   else fputs("\tNA",fp);
   fputc('\t',fp);mvpa_1D_text(fp,stratum);
}

static void mvpa_write_class_summary( FILE *fp, const char *scope,
                                      const char *group,
                                      const THD_mvpa_class_score *score,
                                      int ngroup, int average,
                                      int fold, const char *stratum )
{
   fputc('\t',fp); mvpa_1D_text(fp,scope);
   fputc('\t',fp); mvpa_1D_text(fp,group);
   fprintf(fp,"\t%d",score->nsample);
   if(average) fputs("\tNA\tNA",fp);
   else fprintf(fp,"\t%d\t%d",score->ncorrect,score->nclass_present);
   fprintf(fp,"\t%d",ngroup);
   if(isfinite(score->accuracy)) fprintf(fp,"\t%.17g",score->accuracy);
   else fputs("\tNA",fp);
   if(isfinite(score->balanced_accuracy)) fprintf(fp,"\t%.17g",score->balanced_accuracy);
   else fputs("\tNA",fp);
   mvpa_write_fold_identity(fp,fold,stratum);
   fputc('\n',fp);
}

static void mvpa_write_class_audits( FILE *conf, FILE *summary,
                                     THD_3dim_dataset *mset,
                                     const THD_roilist *rl, int rr,
                                     int searchlight, int surface,
                                     const THD_sampleset *ss,
                                     const THD_foldplan *plan,
                                     const int *class_id, char **class_label,
                                     const THD_mvpa_workspace *w,
                                     mvpa_class_audit *a )
{
   const char *scope[]={"Pooled","Fold","Subject","Stratum"};
   int kind,gg,ii,cc,pp;
   for(kind=0;kind<4;kind++){
     int ng=kind==0?1:kind==1?plan->nfold:kind==2?ss->nsubject:plan->nstratum;
     int nvalid=0,ntotal=0;
     double sumacc=0,sumbacc=0;
     for(gg=0;gg<ng;gg++){
       int n=0;
       const char *label=kind==0?"All":kind==1?plan->name[gg]:
                         kind==2?ss->subject_label[gg]:plan->stratum_label[gg];
       const char *stratum=kind==1?plan->stratum_label[plan->fold_stratum[gg]]:
                           kind==3?plan->stratum_label[gg]:NULL;
       int fold=kind==1?gg:-1;
       THD_mvpa_class_score score={0}; char err[512];
       score.nclass=w->nclass; score.accuracy=score.balanced_accuracy=NAN;
       memset(a->confusion,0,sizeof(int)*(size_t)w->nclass*w->nclass);
       for(cc=0;cc<w->nclass;cc++) a->recall[cc]=NAN;
       for(ii=0;ii<ss->nsample;ii++){
         int ff=plan->sample_test_fold[ii];
         if(ff<0) continue;
         if((kind==1 && ff!=gg) || (kind==2 && ss->subject[ii]!=gg) ||
            (kind==3 && plan->fold_stratum[ff]!=gg)) continue;
         a->truth[n]=class_id[ii]; a->prediction[n++]=w->prediction[ii];
       }
       if(n && !THD_mvpa_score_classification(a->truth,a->prediction,n,w->nclass,
                        a->confusion,a->recall,&score,err,sizeof(err)))
         ERROR_exit(PROGRAM_NAME ": classification audit: %s",err);
       if(n){nvalid++;ntotal+=n;sumacc+=score.accuracy;sumbacc+=score.balanced_accuracy;}
       if(summary){
         mvpa_write_spatial_prefix(summary,mset,rl,rr,searchlight,surface);
         mvpa_write_class_summary(summary,scope[kind],label,&score,n?1:0,0,fold,stratum);
       }
       if(conf) for(cc=0;cc<w->nclass;cc++){
         int total=0;
         for(pp=0;pp<w->nclass;pp++) total+=a->confusion[(size_t)cc*w->nclass+pp];
         mvpa_write_spatial_prefix(conf,mset,rl,rr,searchlight,surface);
         fputc('\t',conf);mvpa_1D_text(conf,scope[kind]);
         fputc('\t',conf);mvpa_1D_text(conf,label);
         fprintf(conf,"\t%d\t",cc);mvpa_1D_text(conf,class_label[cc]);
         fprintf(conf,"\t%d",total);
         if(isfinite(a->recall[cc])) fprintf(conf,"\t%.17g",a->recall[cc]);
         else fputs("\tNA",conf);
         for(pp=0;pp<w->nclass;pp++) fprintf(conf,"\t%d",a->confusion[(size_t)cc*w->nclass+pp]);
         mvpa_write_fold_identity(conf,fold,stratum);
         fputc('\n',conf);
       }
     }
     if(summary && kind>=2){
       THD_mvpa_class_score mean={0};
       mean.nsample=ntotal;
       mean.accuracy=nvalid?sumacc/nvalid:NAN;
       mean.balanced_accuracy=nvalid?sumbacc/nvalid:NAN;
       mvpa_write_spatial_prefix(summary,mset,rl,rr,searchlight,surface);
       mvpa_write_class_summary(summary,kind==2?"EqualSubject":"EqualStratum",
                                "All",&mean,nvalid,1,-1,NULL);
     }
   }
}

static void mvpa_logistic_header(FILE *fp,double tol,int maxiter)
{
   fprintf(fp,"# Method: logistic; symmetric_K_class_softmax; objective=mean_log_loss+lambda/2*sum_weights_squared; intercept_unpenalized; tolerance=%.17g; max_iter=%d\n",tol,maxiter);
   fputs("# DecisionClass=logits; ProbabilityClass=softmax; no fitted calibration. Class IDs follow the label dictionary above.\n"
         "# LogLoss=mean natural-log loss, computed from logits without clipping; Brier=mean sum of K squared probability errors, range [0,2] (binary also sums both classes).\n",fp);
}
static void mvpa_write_probability_audits(FILE *scores,FILE *calibration,
 THD_3dim_dataset *mset,const THD_roilist *rl,int rr,int searchlight,int surface,
 const THD_sampleset *ss,const THD_foldplan *plan,const int *truth,char **labels,const THD_mvpa_workspace *w)
{
   int kind,g,i,c,b;const char *scope[]={"Pooled","Fold","Subject","Stratum"};
   if(scores)for(kind=0;kind<4;kind++){
      int ng=kind==0?1:kind==1?plan->nfold:kind==2?ss->nsubject:plan->nstratum;
      for(g=0;g<ng;g++){
         double ll=0,br=0;int n=0;char err[256];
         const char *label=kind==0?"All":kind==1?plan->name[g]:kind==2?ss->subject_label[g]:plan->stratum_label[g];
         const char *stratum=kind==1?plan->stratum_label[plan->fold_stratum[g]]:kind==3?plan->stratum_label[g]:NULL;
         for(i=0;i<ss->nsample;i++){
            int f=plan->sample_test_fold[i];double a,z;
            if(f<0 || (kind==1 && f!=g) || (kind==2 && ss->subject[i]!=g) || (kind==3 && plan->fold_stratum[f]!=g))continue;
            if(!THD_mvpa_probability_score(w->decision+(size_t)i*w->nclass,w->nclass,truth[i],&a,&z,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
            ll+=a;br+=z;n++;
         }
         mvpa_write_spatial_prefix(scores,mset,rl,rr,searchlight,surface);
         fprintf(scores,"\t%s\t",scope[kind]);mvpa_1D_text(scores,label);fprintf(scores,"\t%d",n);
         if(n)fprintf(scores,"\t%.17g\t%.17g",ll/n,br/n);else fputs("\tNA\tNA",scores);
         mvpa_write_fold_identity(scores,kind==1?g:-1,stratum);fputc('\n',scores);
      }
   }
   if(calibration)for(c=0;c<w->nclass;c++){
      int count[10]={0},observed[10]={0};double sum[10]={0};
      for(i=0;i<ss->nsample;i++)if(w->prediction[i]>=0){
         double pr=w->probability[(size_t)i*w->nclass+c];b=(int)(10*pr);if(b>9)b=9;
         count[b]++;sum[b]+=pr;observed[b]+=truth[i]==c;
      }
      for(b=0;b<10;b++){
         mvpa_write_spatial_prefix(calibration,mset,rl,rr,searchlight,surface);
         fprintf(calibration,"\t%d\t",c);mvpa_1D_text(calibration,labels[c]);
         fprintf(calibration,"\t%d\t%.1f\t%.1f\t%d",b,b/10.,(b+1)/10.,count[b]);
         if(count[b])fprintf(calibration,"\t%.17g\t%.17g",sum[b]/count[b],(double)observed[b]/count[b]);else fputs("\tNA\tNA",calibration);
         fputc('\n',calibration);
      }
   }
}

static void mvpa_write_prediction_rows( FILE *fp, THD_3dim_dataset *mset,
                                        const THD_roilist *rl, int rr,
                                        int searchlight, int surface,
                                        const THD_sampleset *ss,
                                        const THD_foldplan *plan,
                                        const int *class_id, char **class_label,
                                        const THD_mvpa_workspace *workspace,int two_tables )
{
   int ii,cc ;
   for( ii=0 ; ii<ss->nsample ; ii++ ){
     int ff=plan->sample_test_fold[ii],pred=workspace->prediction[ii] ;
     mvpa_write_spatial_prefix(fp,mset,rl,rr,searchlight,surface) ;
     fprintf(fp,"\t%d\t",ss->table_row[ii]+1) ; mvpa_1D_text(fp,ss->sample_label[ii]) ;
     fputc('\t',fp) ; mvpa_1D_text(fp,mvpa_level(ss->subject_label,ss->subject[ii])) ;
     fputc('\t',fp) ; mvpa_1D_text(fp,mvpa_level(ss->run_label,ss->run[ii])) ;
     fputc('\t',fp) ; mvpa_1D_text(fp,mvpa_level(ss->item_label,ss->item[ii])) ;
     fputc('\t',fp) ; mvpa_1D_text(fp,mvpa_level(ss->fold_label,ss->fold[ii])) ;
     fputc('\t',fp) ; mvpa_1D_text(fp,(ff>=0)?plan->name[ff]:"NA") ;
     fputc('\t',fp) ;
     mvpa_1D_text(fp,(ff>=0)?mvpa_level(plan->stratum_label,plan->fold_stratum[ff]):"NA") ;
     fputc('\t',fp) ; mvpa_1D_text(fp,class_label[class_id[ii]]) ; fputc('\t',fp) ;
     if( pred>=0 ){
       mvpa_1D_text(fp,class_label[pred]) ; fprintf(fp,"\t%d",pred==class_id[ii]) ;
     } else fputs("NA\tNA",fp) ;
     for( cc=0 ; cc<workspace->nclass ; cc++ ){
       double dd=workspace->decision[(size_t)ii*workspace->nclass+cc] ;
       if( isfinite(dd) ) fprintf(fp,"\t%.17g",dd) ; else fputs("\tNA",fp) ;
     }
     if(workspace->logistic)for(cc=0;cc<workspace->nclass;cc++){
       double pr=workspace->probability[(size_t)ii*workspace->nclass+cc];
       if(isfinite(pr))fprintf(fp,"\t%.17g",pr);else fputs("\tNA",fp);
     }
     if(two_tables)mvpa_source_fields(fp,ss,ii);
     fputc('\n',fp) ;
   }
}

static void mvpa_write_fold_rows( FILE *fp, THD_3dim_dataset *mset,
                                  const THD_roilist *rl, int rr,
                                  int searchlight, int surface,
                                  const THD_foldplan *plan,
                                  const THD_mvpa_workspace *workspace )
{
   int ff ;
   for( ff=0 ; ff<plan->nfold ; ff++ ){
     mvpa_write_spatial_prefix(fp,mset,rl,rr,searchlight,surface) ; fputc('\t',fp) ;
     mvpa_1D_text(fp,plan->name[ff]) ; fputc('\t',fp) ;
     mvpa_1D_text(fp,mvpa_level(plan->stratum_label,plan->fold_stratum[ff])) ;
     fprintf(fp,"\t%d\t%d\t%.17g\t%.17g",plan->ntrain[ff],
             workspace->fold_nsample[ff],workspace->fold_accuracy[ff],
             workspace->fold_balanced_accuracy[ff]) ;
     if(workspace->ridge_cv){
       const THD_mvpa_ridge_workspace *rw=workspace->ridge_cv;double loss=NAN;int l;
       if(rw->nested)for(l=0;l<rw->nlambda;l++)if(rw->lambda_grid[l]==rw->selected_lambda[ff])
         loss=rw->inner_loss[(size_t)ff*rw->nlambda+l];
       fprintf(fp,"\t%.17g\t%d\t%d\t%.17g",rw->selected_lambda[ff],
               rw->nested?rw->nested->inner[ff]->nfold:0,rw->inner_nvalid[ff],loss);
     }
     if(workspace->svm || workspace->logistic){
       double loss=NAN;int l;
       if(workspace->class_nested)for(l=0;l<workspace->class_ngrid;l++)
         if(workspace->class_grid[l]==workspace->class_selected[ff])loss=workspace->class_loss[(size_t)ff*workspace->class_ngrid+l];
       fprintf(fp,"\t%.17g\t%d\t%.17g\t%d\t%.17g",workspace->class_selected[ff],
         workspace->class_nested?workspace->class_nested->inner[ff]->nfold:0,loss,
         workspace->fold_solver_iterations[ff],workspace->fold_solver_residual[ff]);
     }
     if(workspace->logistic)fprintf(fp,"\t%.17g\t%.17g",workspace->fold_log_loss[ff],workspace->fold_brier[ff]);
     if(workspace->lda){
       int form=workspace->fold_covariance_form[ff];
       fprintf(fp,"\t%.17g\t%.17g\t%.17g\t%s",workspace->lda_shrinkage,
               workspace->fold_covariance_scale[ff],workspace->fold_variance_floor[ff],
               form==THD_LDA_FORM_DIAGONAL?"diagonal":form==THD_LDA_FORM_PRIMAL?"primal":"dual");
     }
     fputc('\n',fp);
   }
}

static double mvpa_cont_value(const THD_mvpa_continuous_score *s,int metric)
{
   switch(metric){
      case THD_METRIC_PEARSON:return s->pearson;
      case THD_METRIC_SPEARMAN:return s->spearman;
      case THD_METRIC_CROSS_VALIDATED_R2:return s->cv_r2;
      case THD_METRIC_MAE:return s->mae;
      default:return s->rmse;
   }
}

/* CLI list syntax and requested output labels stay here; numeric validation
   and extraction in sample order are shared library operations. */
static float *mvpa_numeric_columns(const THD_sampleset *ss,const char *list,
                                    char ***names,int *count)
{
   char *copy,*cur,*next,err[1024];int n=1,j;float *v;
   if(!list || !*list)ERROR_exit(PROGRAM_NAME ": numeric column list is empty");
   for(cur=(char *)list;*cur;cur++)if(*cur==','){
      if(n==INT_MAX)ERROR_exit(PROGRAM_NAME ": too many numeric columns");
      n++;
   }
   if((size_t)n>SIZE_MAX/sizeof(char *))ERROR_exit(PROGRAM_NAME ": too many numeric columns");
   *names=calloc((size_t)n,sizeof(char *));
   copy=strdup(list);
   if(!*names || !copy)ERROR_exit(PROGRAM_NAME ": cannot allocate numeric column names");
   cur=copy;
   for(j=0;j<n;j++){
      next=strchr(cur,',');if(next)*next++='\0';
      (*names)[j]=strdup(cur);
      if(!(*names)[j])ERROR_exit(PROGRAM_NAME ": cannot copy numeric column name");
      cur=next;
   }
   free(copy);
   v=THD_sampleset_numeric_columns(ss,(const char *const *)*names,n,err,sizeof(err));
   if(!v){THD_sampleset_free_levels(*names,n);*names=NULL;ERROR_exit(PROGRAM_NAME ": %s",err);}
   *count=n;return v;
}

static void mvpa_nuisance_blocks(const THD_sampleset *ss,const float *z,int k,const char *column)
{
   int *code=NULL,*first,nlevel=1,i,j;char **labels=NULL,err[512];
   if(strcasecmp(column,"none")){
      if(!THD_sampleset_encode_column(ss,column,&code,&labels,&nlevel,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
   }else code=calloc((size_t)ss->nsample,sizeof(int));
   first=malloc(sizeof(int)*(size_t)nlevel);
   if(!code || !first)ERROR_exit(PROGRAM_NAME ": cannot validate nuisance permutation blocks");
   for(i=0;i<nlevel;i++)first[i]=-1;
   for(i=0;i<ss->nsample;i++){
      int b=code[i];
      if(first[b]<0)first[b]=i;
      for(j=0;j<k;j++)if(z[(size_t)i*k+j]!=z[(size_t)first[b]*k+j])
         ERROR_exit(PROGRAM_NAME ": nuisance values vary within permutation block '%s'; use nuisance-constant exchangeability blocks or omit -nperm (general residual permutations are unsupported)",labels?labels[b]:"none");
   }
   free(first);free(code);THD_sampleset_free_levels(labels,nlevel);
}

static THD_roilist *mvpa_voxel_units(THD_3dim_dataset *mask)
{
   THD_roilist *atlas=THD_roilist_from_dset(mask,NULL),*units;
   int r,j,k=0,n=0;
   if(!atlas)ERROR_exit(PROGRAM_NAME ": encoding mask has no positive voxels/nodes");
   for(r=0;r<atlas->nroi;r++)n+=atlas->vox[r].nar;
   units=calloc(1,sizeof(*units));if(!units)ERROR_exit(PROGRAM_NAME ": cannot allocate voxel units");
   units->nroi=n;units->val=malloc(sizeof(int)*(size_t)n);
   units->center=malloc(sizeof(int)*(size_t)n);
   units->vox=calloc((size_t)n,sizeof(intvec));units->lab=calloc((size_t)n,sizeof(char *));
   if(!units->val || !units->center || !units->vox || !units->lab)
      ERROR_exit(PROGRAM_NAME ": cannot allocate voxel unit buffers");
   for(r=0;r<atlas->nroi;r++)for(j=0;j<atlas->vox[r].nar;j++,k++){
      int v=atlas->vox[r].ar[j];
      units->center[k]=v;units->val[k]=mask->dblk->node_list?mask->dblk->node_list[v]:v;
      units->vox[k].nar=1;units->vox[k].ar=malloc(sizeof(int));
      if(!units->vox[k].ar)ERROR_exit(PROGRAM_NAME ": cannot allocate voxel index");
      units->vox[k].ar[0]=v;
   }
   THD_roilist_free(atlas);return units;
}

static double *mvpa_penalty_grid(const char *text,int *count,const char *what)
{
   int n=1,i,j;char *copy=strdup(text),*cur,*next,*end;double *grid;
   if(!copy)ERROR_exit(PROGRAM_NAME ": cannot allocate penalty grid");
   for(cur=copy;*cur;cur++)if(*cur==',')n++;
   if(n<2)ERROR_exit(PROGRAM_NAME ": lambda grid needs at least two candidates");
   grid=malloc(sizeof(double)*(size_t)n);if(!grid)ERROR_exit(PROGRAM_NAME ": cannot allocate penalty candidates");
   cur=copy;
   for(i=0;i<n;i++){
      next=strchr(cur,',');if(next)*next++='\0';errno=0;grid[i]=strtod(cur,&end);
      if(errno || cur==end || *end || !isfinite(grid[i]) || grid[i]<=0)
         ERROR_exit(PROGRAM_NAME ": lambda grid values must be finite and positive");
      for(j=0;j<i;j++)if(grid[i]==grid[j])ERROR_exit(PROGRAM_NAME ": lambda grid contains duplicate penalties");
      cur=next;
   }
   free(copy);*count=n;return grid;
}

static void mvpa_neural_id(char *text,int size,THD_3dim_dataset *mset,int row);

static void mvpa_tune_header(FILE *fp,int requested,int objective)
{
   if(fp && requested!=THD_TUNE_DEFAULT)
      fprintf(fp,"# InnerCVObjective=%s; minimized_loss=%s; aggregation=%s; zero_baseline_responses=excluded_only_for_normalized_mse; other_undefined_losses=error\n",
              THD_mvpa_tune_objective_name(objective),
              objective==THD_TUNE_ACCURACY || objective==THD_TUNE_BALANCED_ACCURACY?"1-score":"error",
              objective==THD_TUNE_LOG_LOSS || objective==THD_TUNE_BRIER?"pooled_held_out_rows":"pooled_rows_then_equal_response_or_class_mean");
}
static void mvpa_candidate_header(FILE *fp,int objective,int ridge,int pca)
{
   fprintf(fp,"# Inner-CV candidates; Objective=%s; all losses minimized; ACC/BACC loss=1-score; continuous aggregation=equal_mean_of_pooled_response_losses.\n",
           THD_mvpa_tune_objective_name(objective));
   if(objective==THD_TUNE_LOG_LOSS || objective==THD_TUNE_BRIER)fputs("# Probability loss is the mean over pooled inner held-out rows (one joint class distribution).\n",fp);
   if(pca)fprintf(fp,"# Exact ties: smaller requested feature count, smaller requested PCA count, then %s. No feature selection: count=input width. Penalty=NA for centroid/LDA.\n",
           ridge==2?"first regression candidate":ridge?"larger lambda":"smaller C (if applicable)");
   else fprintf(fp,"# Exact ties: smaller requested count, then %s. CandidateID is zero-based within each unit/fold. No feature selection: count=input width. Penalty=NA for centroid/LDA.\n",
           ridge==2?"first regression candidate":ridge?"larger lambda":"smaller C (if applicable)");
   fputs("# InnerNValid counts represented classes for ACC/BACC, one probability distribution for log_loss/brier, responses otherwise; normalized_mse excludes zero-baseline responses and permits all-undefined tie fallback.\n",fp);
   if(pca)fputs("# PCA enabled: CandidateID zero-based within unit/fold; order=feature_count,PCA_count,penalty; ties=smaller_feature_count_then_smaller_PCA_count_then_penalty. Inner effective counts can differ by fold/rank.\n",fp);
   fputs("# Columns: Unit\tCVFold\tCVStratum\tCandidateID\tObjective\tFeatureSelection\tRequestedCount\tEffectiveCount\tPenaltyName\tPenalty\tInnerNFold\tInnerNTest\tInnerNValid\tInnerLoss\tSelected",fp);
   if(pca)fputs("\tPCARequested\tInnerPCAMin\tInnerPCAMax",fp);
   fputc('\n',fp);
}
static void mvpa_candidate_write(FILE *fp,const THD_foldplan *plan,int unit,int p,int objective,
 const THD_mvpa_ridge_workspace *rw,const THD_mvpa_workspace *cw)
{
   const THD_feature_selection *sel;const THD_nested_foldplan *nested;
   const THD_mvpa_pca *pc;int f,c,l,h,nk,nl,np;
   if(!fp)return;
   sel=rw?rw->selection:cw->selection;pc=rw?rw->pca:cw->pca;np=pc?pc->ngrid:1;
   nested=pc?pc->nested:rw?rw->nested:cw->class_nested?cw->class_nested:sel->nested;
   nk=sel?sel->ngrid:1;nl=rw?rw->nlambda:cw->class_nested?cw->class_ngrid:1;
   for(f=0;f<plan->nfold;f++)for(c=0;c<nk;c++)for(h=0;h<np;h++)for(l=0;l<nl;l++){
      int count=sel?sel->grid[c]:p,chosen_count=sel?sel->chosen[f]:0,ntest=0,j;
      int valid=rw?rw->inner_nvalid[f]:(objective==THD_TUNE_LOG_LOSS || objective==THD_TUNE_BRIER)?1:cw->nclass;
      double penalty=rw?rw->lambda_grid[l]:cw->class_nested?cw->class_grid[l]:cw->class_penalty;
      double chosen=rw?rw->selected_lambda[f]:(cw->svm || cw->logistic)?cw->class_selected[f]:0;
      double loss=pc?pc->loss[(((size_t)f*nk+c)*np+h)*nl+l]:sel?sel->loss[((size_t)f*nk+c)*nl+l]:rw?rw->inner_loss[(size_t)f*nl+l]:cw->class_loss[(size_t)f*nl+l];
      for(j=0;j<nested->inner[f]->nfold;j++)ntest+=nested->inner[f]->ntest[j];
      fprintf(fp,"%d\t",unit);mvpa_1D_text(fp,plan->name[f]);fputc('\t',fp);
      mvpa_1D_text(fp,plan->stratum_label[plan->fold_stratum[f]]);
      fprintf(fp,"\t%d\t%s\t%d\t%d\t%d\t%s\t",(c*np+h)*nl+l,THD_mvpa_tune_objective_name(objective),
              sel!=NULL,count,count<p?count:p,rw?(rw->regression?"regression_candidate":"lambda"):cw->logistic?"lambda":cw->svm?"C":"none");
      if(rw || cw->svm || cw->logistic)fprintf(fp,"%.17g",penalty);else fputs("NA",fp);
      fprintf(fp,"\t%d\t%d\t%d\t%.17g\t%d",nested->inner[f]->nfold,ntest,valid,loss,
              c==chosen_count && (!pc || h==pc->chosen[f]) && (!(rw || cw->svm || cw->logistic) || penalty==chosen));
      if(pc){size_t at=((size_t)f*nk+c)*np+h;fprintf(fp,"\t%d\t%d\t%d",pc->grid[h],pc->inner_min[at],pc->inner_max[at]);}
      fputc('\n',fp);
   }
}

static void mvpa_response_header(FILE *fp,int objective)
{
   fprintf(fp,"# Per-response lambda; Objective=%s; InnerLoss is pooled inner loss for this response. InnerNValid=0/1.\n",THD_mvpa_tune_objective_name(objective));
   fputs("# Each response selects largest lambda on ties/all-undefined losses. Shared feature/PCA counts minimize ConfigurationLoss: equal mean of response-wise minimum losses, excluding undefined normalized-MSE responses. Configuration ties prefer smaller feature count then smaller PCA count.\n"
         "# ResponseSelected marks each configuration's response optimum; Selected also requires ConfigurationSelected. PCARequested/InnerPCAMin/Max=0 when PCA is disabled. Observed fits restored after permutations.\n"
         "# Columns: Unit\tCVFold\tCVStratum\tCandidateID\tObjective\tFeatureSelection\tRequestedCount\tEffectiveCount\tPenaltyName\tPenalty\tInnerNFold\tInnerNTest\tInnerNValid\tInnerLoss\tSelected\tPCARequested\tInnerPCAMin\tInnerPCAMax\tResponse\tResponseSelected\tConfigurationLoss\tConfigurationSelected\n",fp);
}
static void mvpa_response_candidates(FILE *fp,const THD_mvpa_ridge_workspace *w,
 THD_3dim_dataset *mset,const THD_roilist *rl,int unit,int p,int q,char **names,int encoding,int objective)
{
   const THD_response_ridge *a=w->response_ridge;const THD_foldplan *plan=w->plan;
   int f,c,h,l,o,np=w->pca?w->pca->ngrid:1,nk=w->selection?w->selection->ngrid:1;
   if(!fp)return;
   for(f=0;f<plan->nfold;f++)for(c=0;c<nk;c++)for(h=0;h<np;h++)for(l=0;l<w->nlambda;l++)for(o=0;o<q;o++){
      size_t at=(size_t)f*a->nconfig+c*np+h;char response[256];
      int count=w->selection?w->selection->grid[c]:p,ne=0,j;
      int config_chosen=(!w->selection || c==w->selection->chosen[f]) && (!w->pca || h==w->pca->chosen[f]);
      int response_chosen=l==a->best[at*w->max_output+o];
      double loss=a->loss[(at*w->nlambda+l)*w->max_output+o];
      for(j=0;j<w->nested->inner[f]->nfold;j++)ne+=w->nested->inner[f]->ntest[j];
      if(encoding)mvpa_neural_id(response,sizeof(response),mset,rl->vox[unit].ar[o]);
      else snprintf(response,sizeof(response),"%s",names[o]);
      fprintf(fp,"%d\t",rl->val[unit]);mvpa_1D_text(fp,plan->name[f]);fputc('\t',fp);mvpa_1D_text(fp,plan->stratum_label[plan->fold_stratum[f]]);
      fprintf(fp,"\t%d\t%s\t%d\t%d\t%d\tlambda\t%.17g\t%d\t%d\t%d\t%.17g\t%d\t%d\t%d\t%d\t",
         (c*np+h)*w->nlambda+l,THD_mvpa_tune_objective_name(objective),w->selection!=NULL,count,count<p?count:p,
         w->lambda_grid[l],w->nested->inner[f]->nfold,ne,isfinite(loss)?1:0,loss,config_chosen && response_chosen,
         w->pca?w->pca->grid[h]:0,w->pca?w->pca->inner_min[at]:0,w->pca?w->pca->inner_max[at]:0);
      mvpa_1D_text(fp,response);fprintf(fp,"\t%d\t%.17g\t%d\n",response_chosen,a->config_loss[at],config_chosen);
   }
}
static void mvpa_cont_candidates(FILE *fp,const THD_mvpa_ridge_workspace *w,
 THD_3dim_dataset *mset,const THD_roilist *rl,int unit,int p,int q,char **names,int encoding,int objective)
{
   if(w->response_ridge)mvpa_response_candidates(fp,w,mset,rl,unit,p,q,names,encoding,objective);
   else mvpa_candidate_write(fp,w->plan,rl->val[unit],p,objective,w,NULL);
}

typedef struct {FILE *summary,*tuning,*loadings;} mvpa_pca_audit;
static void mvpa_pca_open(mvpa_pca_audit *a,const char *prefix,const char *loading_file,
 int nested,int objective,int ridge,int response_lambda)
{
   char *name=malloc(strlen(prefix)+32);
   if(!name)ERROR_exit(PROGRAM_NAME ": cannot name PCA audits");
   snprintf(name,strlen(prefix)+32,"%s.pca.1D",prefix);a->summary=mvpa_open_audit(name,"PCA summary");
   fputs("# Observed outer-training PCA after nuisance, scaling and optional selection; centered, unwhitened. Eigenvalues are sample variances (NTrain-1). Effective count clips to numerical rank. Component=0 marks rank zero (one zero estimator input).\n",a->summary);
   fputs("# Rank threshold=64*DBL_EPSILON*max(NTrain,NFeat)*largest_eigenvalue; repeated eigenvalues have nonunique bases. All fits repeated under inner CV and permutations; observed audits restored.\n",a->summary);
   fputs("# Columns: Unit\tCVFold\tCVStratum\tNTrain\tInputNFeat\tRequestedComponents\tEffectiveComponents\tNumericalRank\tComponent\tVariance\tVarianceRatio\tCumulativeRatio\tTotalVariance\n",a->summary);
   if(nested){
      snprintf(name,strlen(prefix)+32,"%s.pca_tuning.1D",prefix);a->tuning=mvpa_open_audit(name,"PCA candidate audit");
      if(response_lambda)mvpa_response_header(a->tuning,objective);else mvpa_candidate_header(a->tuning,objective,ridge,1);
   }
   if(loading_file){
      a->loadings=mvpa_open_audit(loading_file,"PCA loadings");
      fputs("# PCA coordinates after nuisance/scaling/selection, not raw-input coefficients or activation patterns. Mean is training-only in this coordinate system; scores=(x-mean)*loading. Largest absolute loading positive; repeated-eigenvalue bases nonunique. FeatureIndex is original zero-based input column; Component is one-based (0 means rank zero, loading=0).\n",a->loadings);
      fputs("# Columns: Unit\tCVFold\tCVStratum\tComponent\tFeatureIndex\tFeature\tMean\tLoading\n",a->loadings);
   }
   free(name);
}
static void mvpa_pca_write(const mvpa_pca_audit *a,const THD_mvpa_pca *pc,
 const THD_foldplan *plan,THD_3dim_dataset *mset,const THD_roilist *rl,int unit,
 char **names,const THD_feature_selection *sel)
{
   int f,k,j;char text[64];
   if(!pc || !a->summary)return;
   for(f=0;f<plan->nfold;f++){
      double cumulative=0;int count=pc->count[f],p=pc->input_count[f],cap=pc->model->capacity;
      for(k=0;k<(count?count:1);k++){
         double v=count?pc->variance[(size_t)f*cap+k]:0,ratio=pc->total[f]>0?v/pc->total[f]:0;
         FILE *fp=a->summary;cumulative+=ratio;
         fprintf(fp,"%d\t",rl->val[unit]);mvpa_1D_text(fp,plan->name[f]);fputc('\t',fp);
         mvpa_1D_text(fp,plan->stratum_label[plan->fold_stratum[f]]);
         fprintf(fp,"\t%d\t%d\t%d\t%d\t%d\t%d\t%.17g\t%.17g\t%.17g\t%.17g\n",plan->ntrain[f],p,
           pc->grid[pc->chosen[f]],count,pc->rank[f],count?k+1:0,v,ratio,cumulative,pc->total[f]);
         if(a->loadings)for(j=0;j<p;j++){
            int col=sel?sel->indices[(size_t)f*sel->max_feature+j]:j;
            double loading=count?pc->loadings[(size_t)f*cap*pc->model->max_feature+(size_t)k*p+j]:0;
            fp=a->loadings;fprintf(fp,"%d\t",rl->val[unit]);mvpa_1D_text(fp,plan->name[f]);fputc('\t',fp);
            mvpa_1D_text(fp,plan->stratum_label[plan->fold_stratum[f]]);
            fprintf(fp,"\t%d\t%d\t",count?k+1:0,col);
            if(!names)mvpa_neural_id(text,sizeof(text),mset,rl->vox[unit].ar[col]);
            mvpa_1D_text(fp,names?names[col]:text);
            fprintf(fp,"\t%.17g\t%.17g\n",pc->means[(size_t)f*pc->model->max_feature+j],loading);
         }
      }
   }
}
static void mvpa_pca_close(mvpa_pca_audit *a)
{
   FILE *files[]={a->summary,a->tuning,a->loadings};int i,failed=0;
   for(i=0;i<3;i++)if(files[i]){if(ferror(files[i]))failed=1;if(fclose(files[i]))failed=1;}
   if(failed)ERROR_exit(PROGRAM_NAME ": error writing PCA audits");
}

typedef struct { const char *prefix;FILE *table,*covariance; } mvpa_pattern_audit;
static THD_3dim_dataset *mvpa_coefficient_map(THD_3dim_dataset *,const char *,int,int,char **);
static void mvpa_pattern_mapname(char *name,size_t capacity,THD_3dim_dataset *mset,
 const char *prefix,int unit,int fold)
{
   int surface=DBLK_IS_NI_SURF_DSET(mset->dblk) || DBLK_IS_GIFTI(mset->dblk);
   size_t len=strlen(prefix);
   if(surface && len>=10 && !strcmp(prefix+len-10,".niml.dset"))len-=10;
   snprintf(name,capacity,"%.*s.unit_%d.fold_%d%s",(int)len,prefix,unit,fold+1,surface?".niml.dset":"");
}
static void mvpa_pattern_open(mvpa_pattern_audit *a,const char *prefix,int scale,const char *nuisance,int classes)
{
   char *name=malloc(strlen(prefix)+40);int i;
   if(!name)ERROR_exit(PROGRAM_NAME ": cannot name pattern audits");
   a->prefix=prefix;
   for(i=0;i<2;i++){
      FILE *fp;
      snprintf(name,strlen(prefix)+40,"%s.%s.1D",prefix,i?"pattern_covariance":"patterns");
      fp=mvpa_open_audit(name,"descriptive activation patterns");
      fputs("# Descriptive OUTER-TRAINING ridge activation patterns, separately for each fitted fold; NOT held-out scores, causal effects, activation significance, or full-data refits.\n",fp);
      fputs("# A=Cov(X,S)*pinv(Cov(S,S)); S=fitted training scores, NOT observed targets. Centered sample covariances use NTrain-1. Joint symmetric Moore-Penrose normalization, not separate per-output variance division.\n",fp);
      fprintf(fp,"# X=all neural inputs after nuisance/scaling, before selection/PCA; scale=%s. Patterns use these transformed feature units per unit of fitted score.\n",THD_preprocess_name(scale));
      mvpa_nuisance_header(fp,nuisance);
      fputs("# Selected-out features can have nonzero patterns through score covariance. PCA/loadings are composed implicitly through model scores; no raw-space weight export is implied.\n",fp);
      fputs("# Rank cutoff=64*DBL_EPSILON*max(NTrain,NOutput)*largest_score_eigenvalue. Rank=0 gives zero patterns (undefined score direction), not evidence of absent signal.\n",fp);
      if(classes)fputs("# Scores are ridge 0/1 class-indicator fits, not probabilities. Their constant-sum dependency is handled jointly with a pseudoinverse; binary class patterns are opposite half-contrast directions.\n",fp);
      fputs("# No fold averaging or pattern inference. With permutations only restored observed patterns are exported.\n",fp);
      if(i){
         fputs("# Columns: Unit\tCVFold\tCVStratum\tScoreRow\tScoreColumn\tCovariance\tPseudoinverse\tScoreRank\tRankCutoff\n",fp);a->covariance=fp;
      }else{
         fputs("# Map bricks are zero-based; fold filename indices are one-based. Outside-unit map values are zero; original sparse node IDs are retained.\n",fp);
         fputs("# Columns: Unit\tCVFold\tCVStratum\tNTrain\tFeatureIndex\tFeature\tResponse\tPattern\tInputMean\tScoreMean\tScoreRank\tLambda\tSelectedFeature\tModelNFeat\tMapPrefix\tMapBrick\n",fp);a->table=fp;
      }
   }free(name);
}
static void mvpa_pattern_write(const mvpa_pattern_audit *a,const THD_mvpa_ridge_workspace *w,
 THD_3dim_dataset *mset,const THD_roilist *rl,int unit,char **names,int q,int argc,char **argv)
{
   const THD_activation_patterns *pat=w->patterns;const THD_foldplan *plan=w->plan;
   char *name,feature[64],label[256];int f,j,o,k,p=rl->vox[unit].nar;
   if(!a || !a->table || !pat)return;
   name=malloc(strlen(a->prefix)+96);if(!name)ERROR_exit(PROGRAM_NAME ": cannot name pattern map");
   for(f=0;f<plan->nfold;f++){
      THD_3dim_dataset *out;int width=w->pca?(w->pca->count[f]?w->pca->count[f]:1):w->selection?w->selection->count[f]:p;
      const double *pattern=pat->pattern+(size_t)f*pat->max_feature*pat->max_output;
      const double *cov=pat->covariance+(size_t)f*pat->max_output*pat->max_output;
      const double *inv=pat->inverse+(size_t)f*pat->max_output*pat->max_output;
      mvpa_pattern_mapname(name,strlen(a->prefix)+96,mset,a->prefix,rl->val[unit],f);
      out=mvpa_coefficient_map(mset,name,q,argc,argv);
      for(j=0;j<p;j++){
         int selected=w->selection?0:1,row=rl->vox[unit].ar[j];
         if(w->selection)for(k=0;k<w->selection->count[f];k++)if(w->selection->indices[(size_t)f*w->selection->max_feature+k]==j)selected=1;
         mvpa_neural_id(feature,sizeof(feature),mset,row);
         for(o=0;o<q;o++){
            double value=pattern[(size_t)j*q+o];FILE *fp=a->table;
            if(!isfinite(value) || fabs(value)>FLT_MAX)ERROR_exit(PROGRAM_NAME ": activation pattern exceeds map float range");
            ((float *)DSET_ARRAY(out,o))[row]=(float)value;
            fprintf(fp,"%d\t",rl->val[unit]);mvpa_1D_text(fp,plan->name[f]);fputc('\t',fp);
            mvpa_1D_text(fp,plan->stratum_label[plan->fold_stratum[f]]);
            fprintf(fp,"\t%d\t%d\t",plan->ntrain[f],j);mvpa_1D_text(fp,feature);fputc('\t',fp);mvpa_1D_text(fp,names[o]);
            fprintf(fp,"\t%.17g\t%.17g\t%.17g\t%d\t%.17g\t%d\t%d\t",value,
               pat->xmean[(size_t)f*pat->max_feature+j],pat->smean[(size_t)f*pat->max_output+o],
               pat->rank[f],w->response_ridge?w->response_ridge->selected[(size_t)f*w->max_output+o]:w->selected_lambda[f],selected,width);
            mvpa_1D_text(fp,name);fprintf(fp,"\t%d\n",o);
         }
      }
      for(o=0;o<q;o++){
         snprintf(label,sizeof(label),"TRAIN_PATTERN_%s",names[o]);EDIT_BRICK_LABEL(out,o,label);
         for(k=0;k<q;k++){
            FILE *fp=a->covariance;fprintf(fp,"%d\t",rl->val[unit]);mvpa_1D_text(fp,plan->name[f]);fputc('\t',fp);
            mvpa_1D_text(fp,plan->stratum_label[plan->fold_stratum[f]]);fputc('\t',fp);
            mvpa_1D_text(fp,names[o]);fputc('\t',fp);mvpa_1D_text(fp,names[k]);
            fprintf(fp,"\t%.17g\t%.17g\t%d\t%.17g\n",cov[(size_t)o*q+k],inv[(size_t)o*q+k],pat->rank[f],pat->cutoff[f]);
         }
      }
      if(DSET_write(out)!=True)ERROR_exit(PROGRAM_NAME ": failed writing activation-pattern map");
      DSET_delete(out);
   }free(name);
}
static void mvpa_pattern_close(mvpa_pattern_audit *a)
{
   FILE *files[]={a->table,a->covariance};int i,failed=0;
   for(i=0;i<2;i++)if(files[i]){if(ferror(files[i]))failed=1;if(fclose(files[i]))failed=1;}
   if(failed)ERROR_exit(PROGRAM_NAME ": error writing activation-pattern audit");
}

typedef struct { FILE *features,*tuning; } mvpa_selection_audit;
static char *mvpa_selection_filename(const char *prefix,int tuning)
{
   char *name=malloc(strlen(prefix)+32);
   if(!name)ERROR_exit(PROGRAM_NAME ": cannot name feature selection audit");
   snprintf(name,strlen(prefix)+32,"%s.%s.1D",prefix,tuning?"feature_tuning":"features");return name;
}
static void mvpa_selection_open(mvpa_selection_audit *a,const char *prefix,
 int continuous,int nested,int metric,const char *nuisance_columns,int ridge,
 int tune_requested,int tune_objective,int response_lambda)
{
   int i;
   for(i=0;i<(nested?2:1);i++){
      char *name=mvpa_selection_filename(prefix,i);
      FILE *fp=mvpa_open_audit(name,"feature selection audit");free(name);
      mvpa_nuisance_header(fp,nuisance_columns);
      fprintf(fp,"# Selection: %s; order=nuisance_then_scale_all_columns_then_select; fit=each_training_split; score_ties=original_column_index; constants=zero; counts=capped_at_input_width\n",
              continuous?"mean_squared_Pearson_over_nonconstant_responses":"between_class_over_total_variance");
      if(i && response_lambda){mvpa_response_header(fp,tune_objective);a->tuning=fp;}
      else if(i){
         mvpa_tune_header(fp,tune_requested,tune_objective);
         if(tune_requested==THD_TUNE_DEFAULT)fprintf(fp,"# objective=%s; ties=smallest_requested_count_then_%s; all count/penalty pairs; observed fits restored after permutations\n",
                 ridge?"mean_response_SSE_over_training_mean_baseline_SSE":metric==THD_METRIC_ACCURACY?"1-ACC":"1-BACC",
                 ridge==2?"first_regression_candidate":ridge?"largest_lambda":"smallest_C");
         else fprintf(fp,"# ties=smallest_requested_count_then_%s; all count/penalty pairs; observed fits restored after permutations\n",ridge==2?"first_regression_candidate":ridge?"largest_lambda":"smallest_C");
         fputs("# Columns: Unit\tCVFold\tCVStratum\tInnerNFold\tRequestedCount\tEffectiveCount\tPenalty\tInnerLoss\tSelected\n",fp);a->tuning=fp;
      }else{
         fputs("# Observed outer-fold selection only; FeatureIndex is zero-based within the input pattern/design; Rank is one-based. Scores are descriptive, not p-values.\n",fp);
         fputs("# Columns: Unit\tCVFold\tCVStratum\tNTrain\tInputNFeat\tRequestedCount\tEffectiveCount\tRank\tFeatureIndex\tFeature\tSelectionScore\n",fp);a->features=fp;
      }
   }
}
static void mvpa_selection_write(const mvpa_selection_audit *a,const THD_feature_selection *sel,
 const THD_foldplan *plan,THD_3dim_dataset *mset,const THD_roilist *rl,int unit,
 char **design_names,int p,const double *grid,const double *selected,double fixed)
{
   int f,j,c,l;char name[64];
   if(!sel || !a->features)return;
   for(f=0;f<plan->nfold;f++){
      for(j=0;j<sel->count[f];j++){
         size_t at=(size_t)f*sel->max_feature+j;int col=sel->indices[at];FILE *fp=a->features;
         fprintf(fp,"%d\t",rl->val[unit]);mvpa_1D_text(fp,plan->name[f]);fputc('\t',fp);
         mvpa_1D_text(fp,plan->stratum_label[plan->fold_stratum[f]]);
         fprintf(fp,"\t%d\t%d\t%d\t%d\t%d\t%d\t",plan->ntrain[f],p,sel->grid[sel->chosen[f]],sel->count[f],j+1,col);
         if(!design_names)mvpa_neural_id(name,sizeof(name),mset,rl->vox[unit].ar[col]);
         mvpa_1D_text(fp,design_names?design_names[col]:name);
         fprintf(fp,"\t%.17g\n",sel->scores[at]);
      }
      if(a->tuning)for(c=0;c<sel->ngrid;c++)for(l=0;l<sel->npenalty;l++){
         FILE *fp=a->tuning;double penalty=grid?grid[l]:fixed;
         fprintf(fp,"%d\t",rl->val[unit]);mvpa_1D_text(fp,plan->name[f]);fputc('\t',fp);
         mvpa_1D_text(fp,plan->stratum_label[plan->fold_stratum[f]]);
         fprintf(fp,"\t%d\t%d\t%d\t%.17g\t%.17g\t%d\n",sel->nested->inner[f]->nfold,
                 sel->grid[c],sel->grid[c]<p?sel->grid[c]:p,penalty,
                 sel->loss[((size_t)f*sel->ngrid+c)*sel->npenalty+l],
                 c==sel->chosen[f] && (!selected || penalty==selected[f]));
      }
   }
}
static void mvpa_selection_close(mvpa_selection_audit *a)
{
   int failed=0;
   if(a->features){if(ferror(a->features))failed=1;if(fclose(a->features))failed=1;}
   if(a->tuning){if(ferror(a->tuning))failed=1;if(fclose(a->tuning))failed=1;}
   if(failed)ERROR_exit(PROGRAM_NAME ": error writing feature selection audit");
}

typedef struct {
   THD_regression_config config;
   char *alpha,*ratio,*components,*cost,*epsilon,*groups_file,*penalties_file;
   int supplied;
   double *parameters,*penalties;int *groups;char **group_names;
   FILE *fits,*features,*contributions,*candidates,*scores;
} mvpa_reg_options;
static const char *mvpa_reg_name(int method)
{
   switch(method){case THD_REG_ELASTIC_NET:return "elastic_net";case THD_REG_BANDED_RIDGE:return "banded_ridge";
      case THD_REG_PLS:return "pls";case THD_REG_SVR:return "svr";default:return "ridge";}
}
static double *mvpa_reg_grid(const char *text,int *count,double low,double high,int integer,const char *what)
{
   char *copy=strdup(text),*cur,*next,*end;double *values;int i,j,n=1;
   if(!copy)ERROR_exit(PROGRAM_NAME ": cannot parse %s",what);
   for(cur=copy;*cur;cur++)if(*cur==',')n++;
   values=malloc(sizeof(double)*(size_t)n);if(!values)ERROR_exit(PROGRAM_NAME ": cannot allocate %s",what);
   cur=copy;for(i=0;i<n;i++){
      next=strchr(cur,',');if(next)*next++='\0';errno=0;values[i]=strtod(cur,&end);
      if(errno || cur==end || *end || !isfinite(values[i]) || values[i]<low || values[i]>high || (integer && values[i]!=floor(values[i])))ERROR_exit(PROGRAM_NAME ": invalid %s value '%s'",what,cur);
      for(j=0;j<i;j++)if(values[j]==values[i])ERROR_exit(PROGRAM_NAME ": duplicate %s value",what);
      cur=next;
   }free(copy);*count=n;return values;
}
static int mvpa_reg_option(mvpa_reg_options *a,int argc,char **argv,int *nopt)
{
   const char *opt=argv[*nopt];char **text=NULL;int grid=0;
#define REG_OPT(name,field) if(!strcasecmp(opt,name) || !strcasecmp(opt,name "_grid")){text=&a->field;grid=!strcasecmp(opt,name "_grid");}
   REG_OPT("-enet_alpha",alpha)
   else REG_OPT("-enet_l1_ratio",ratio)
   else REG_OPT("-pls_components",components)
   else REG_OPT("-svr_c",cost)
   else REG_OPT("-svr_epsilon",epsilon)
   else if(!strcasecmp(opt,"-feature_groups"))text=&a->groups_file;
   else if(!strcasecmp(opt,"-banded_penalties"))text=&a->penalties_file;
   else if(strcasecmp(opt,"-reg_tol") && strcasecmp(opt,"-reg_max_iter"))return 0;
#undef REG_OPT
   if(++*nopt>=argc)ERROR_exit(PROGRAM_NAME ": missing argument after %s",opt);
   if(text){
      if(*text)ERROR_exit(PROGRAM_NAME ": %s was specified twice",opt);
      *text=argv[(*nopt)++];
      if(text!=&a->groups_file && text!=&a->penalties_file && ((strchr(*text,',')!=NULL)!=grid))ERROR_exit(PROGRAM_NAME ": %s requires %s",opt,grid?"at least two comma-separated values":"one value (use the _grid option for candidates)");
   }else if(!strcasecmp(opt,"-reg_max_iter"))a->config.max_iterations=(int)mvpa_parse_long(opt,argv[(*nopt)++],1,INT_MAX);
   else{
      int n;double *v=mvpa_reg_grid(argv[(*nopt)++],&n,1e-12,1e-2,0,opt);
      if(n!=1)ERROR_exit(PROGRAM_NAME ": -reg_tol needs one value");
      a->config.tolerance=v[0];free(v);
   }
   a->supplied=1;return 1;
}
static void mvpa_reg_configure(mvpa_reg_options *a,const char *method,int continuous,int encoding,const char *design)
{
   THD_regression_config *c=&a->config;double *first=NULL,*second=NULL;int na=0,nb=1,i,j,k;
   if(!strcasecmp(method,"elastic_net"))c->method=THD_REG_ELASTIC_NET;
   else if(!strcasecmp(method,"banded_ridge"))c->method=THD_REG_BANDED_RIDGE;
   else if(!strcasecmp(method,"pls"))c->method=THD_REG_PLS;
   else if(!strcasecmp(method,"svr"))c->method=THD_REG_SVR;
   if(!c->method){if(a->supplied)ERROR_exit(PROGRAM_NAME ": regression options require elastic_net, banded_ridge, pls, or svr");return;}
   if(!continuous)ERROR_exit(PROGRAM_NAME ": %s requires continuous decoding/encoding",method);
   if(c->method==THD_REG_SVR && encoding)ERROR_exit(PROGRAM_NAME ": SVR currently supports decoding only; encoding awaits resource validation");
   if(c->method==THD_REG_BANDED_RIDGE && !encoding)ERROR_exit(PROGRAM_NAME ": banded_ridge requires encoding and named design-column groups");
   if((a->alpha || a->ratio) && c->method!=THD_REG_ELASTIC_NET)ERROR_exit(PROGRAM_NAME ": enet options require elastic_net");
   if(a->components && c->method!=THD_REG_PLS)ERROR_exit(PROGRAM_NAME ": pls_components requires pls");
   if((a->cost || a->epsilon) && c->method!=THD_REG_SVR)ERROR_exit(PROGRAM_NAME ": svr options require svr");
   if((a->groups_file || a->penalties_file) && c->method!=THD_REG_BANDED_RIDGE)ERROR_exit(PROGRAM_NAME ": feature_groups/banded_penalties require banded_ridge");
   if(!c->max_iterations)c->max_iterations=10000;
   if(!c->tolerance)c->tolerance=1e-7;
   if(c->method==THD_REG_BANDED_RIDGE){
      THD_datatable *groups,*penalties;char *copy,*cur,*next;
      if(!design || !a->groups_file || !a->penalties_file)ERROR_exit(PROGRAM_NAME ": banded ridge needs -design_columns, -feature_groups and -banded_penalties");
      groups=THD_read_datatable_file(a->groups_file);penalties=THD_read_datatable_file(a->penalties_file);
      if(!groups || !penalties || groups->ncol!=2 || strcmp(groups->cname[0],"Group") || strcmp(groups->cname[1],"Column"))ERROR_exit(PROGRAM_NAME ": feature group table needs exactly 'Group Column' header");
      c->nfeature=groups->nrow;a->groups=calloc((size_t)c->nfeature,sizeof(int));a->group_names=calloc((size_t)c->nfeature,sizeof(char *));copy=strdup(design);
      if(!a->groups || !a->group_names || !copy)ERROR_exit(PROGRAM_NAME ": cannot allocate predictor groups");
      cur=copy;for(i=0;i<c->nfeature;i++){
         if(!cur || !*cur)ERROR_exit(PROGRAM_NAME ": feature group rows must exactly cover design columns in order");
         next=strchr(cur,',');if(next)*next++='\0';
         if(strcmp(cur,DT_CELL(groups,i,1)))ERROR_exit(PROGRAM_NAME ": feature group row %d must name design column '%s'",i+1,cur);
         for(j=0;j<i;j++)if(!strcmp(DT_CELL(groups,j,1),cur))ERROR_exit(PROGRAM_NAME ": duplicate grouped design column");
         for(j=0;j<c->ngroup;j++)if(!strcmp(a->group_names[j],DT_CELL(groups,i,0)))break;
         if(j==c->ngroup){a->group_names[j]=strdup(DT_CELL(groups,i,0));if(!a->group_names[j])ERROR_exit(PROGRAM_NAME ": cannot name group");c->ngroup++;}
         a->groups[i]=j;cur=next;
      }
      if(cur)ERROR_exit(PROGRAM_NAME ": feature group table omits design columns");
      free(copy);
      if(penalties->ncol!=c->ngroup)ERROR_exit(PROGRAM_NAME ": penalty table needs one column per named group");
      for(j=0;j<c->ngroup;j++)if(strcmp(penalties->cname[j],a->group_names[j]))ERROR_exit(PROGRAM_NAME ": penalty columns must follow first group appearance in design order");
      c->ncandidate=penalties->nrow;
      a->penalties=malloc(sizeof(double)*(size_t)c->ncandidate*c->ngroup);a->parameters=calloc((size_t)c->ncandidate*2,sizeof(double));
      if(!a->penalties || !a->parameters)ERROR_exit(PROGRAM_NAME ": cannot allocate banded candidates");
      for(i=0;i<c->ncandidate;i++){
         a->parameters[2*i]=1;
         for(j=0;j<c->ngroup;j++){
            int count;double *v=mvpa_reg_grid(DT_CELL(penalties,i,j),&count,DBL_MIN,DBL_MAX,0,"group penalty");
            if(count!=1)ERROR_exit(PROGRAM_NAME ": one penalty per table cell required");
            a->penalties[(size_t)i*c->ngroup+j]=v[0];free(v);
         }
         for(k=0;k<i;k++){for(j=0;j<c->ngroup;j++)if(a->penalties[(size_t)i*c->ngroup+j]!=a->penalties[(size_t)k*c->ngroup+j])break;
            if(j==c->ngroup)ERROR_exit(PROGRAM_NAME ": duplicate banded penalty vector");}
      }
      c->feature_group=a->groups;c->group_penalty=a->penalties;THD_free_datatable(groups);THD_free_datatable(penalties);
   }else{
      if(c->method==THD_REG_ELASTIC_NET){first=mvpa_reg_grid(a->alpha?a->alpha:"1",&na,DBL_MIN,DBL_MAX,0,"elastic-net alpha");second=mvpa_reg_grid(a->ratio?a->ratio:"0.5",&nb,0,1,0,"L1 ratio");}
      if(c->method==THD_REG_PLS)first=mvpa_reg_grid(a->components?a->components:"2",&na,1,INT_MAX,1,"PLS component count");
      if(c->method==THD_REG_SVR){first=mvpa_reg_grid(a->cost?a->cost:"1",&na,DBL_MIN,DBL_MAX,0,"SVR C");second=mvpa_reg_grid(a->epsilon?a->epsilon:"0.1",&nb,0,DBL_MAX,0,"SVR epsilon");}
      if(na>INT_MAX/nb)ERROR_exit(PROGRAM_NAME ": too many regression candidates");
      c->ncandidate=na*nb;
      a->parameters=calloc((size_t)c->ncandidate*2,sizeof(double));if(!a->parameters)ERROR_exit(PROGRAM_NAME ": cannot allocate regression candidates");
      for(i=0;i<na;i++)for(j=0;j<nb;j++){a->parameters[2*(i*nb+j)]=first[i];a->parameters[2*(i*nb+j)+1]=second?second[j]:0;}
      free(first);free(second);
   }
   c->parameter=a->parameters;
}
static void mvpa_reg_free(mvpa_reg_options *a)
{
   int j;free(a->parameters);free(a->penalties);free(a->groups);
   for(j=0;j<a->config.ngroup;j++)free(a->group_names[j]);
   free(a->group_names);
}
static void mvpa_reg_header(FILE *fp,const mvpa_reg_options *a)
{
   const THD_regression_config *c=&a->config;int i,j;
   fprintf(fp,"# Estimator=%s; shared candidate across responses; ties=smaller feature/PCA counts then first candidate; Candidate is ONE-based.\n",mvpa_reg_name(c->method));
   fprintf(fp,"# Regression tolerance=%.17g; max_iterations=%d; preprocessing and fits repeat inside every inner/outer/permutation training split.\n",c->tolerance,c->max_iterations);
   if(c->method==THD_REG_ELASTIC_NET)fputs("# Objective=SSE/(2*NTrain)+alpha*L1Ratio*L1+alpha*(1-L1Ratio)*L2/2; independent response fits; unpenalized intercept; residual=max absolute KKT. Sparsity is predictive selection, not activation or significance.\n",fp);
   if(c->method==THD_REG_BANDED_RIDGE)fputs("# Objective=SSE+sum(group_lambda*group_L2); unpenalized intercept; no group-size normalization; residual=max absolute normal-equation residual.\n",fp);
   if(c->method==THD_REG_PLS)fputs("# PLS1/PLS2=NIPALS regression deflation; centered predictors/responses; responses unscaled; fit unit is all responses in one ROI/searchlight (voxel encoding fits one response). Rank/covariance exhaustion clips component count. Residual=last weight-iterate change; objective=response training MSE.\n",fp);
   if(c->method==THD_REG_SVR)fputs("# Objective=(L2+intercept^2)/2+C*sum(max(0,abs(error)-epsilon)^2); independent responses; penalized intercept; epsilon in target units; residual=max absolute dual KKT.\n",fp);
   for(i=0;i<c->ncandidate;i++){
      fprintf(fp,"# Candidate %d:",i+1);
      if(c->method==THD_REG_BANDED_RIDGE)for(j=0;j<c->ngroup;j++)fprintf(fp," %s=%.17g",a->group_names[j],a->penalties[(size_t)i*c->ngroup+j]);
      else fprintf(fp," %s=%.17g %s=%.17g",c->method==THD_REG_ELASTIC_NET?"alpha":c->method==THD_REG_PLS?"components":"C",c->parameter[2*i],c->method==THD_REG_ELASTIC_NET?"L1Ratio":c->method==THD_REG_PLS?"unused":"epsilon",c->parameter[2*i+1]);
      fputc('\n',fp);
   }
}
static void mvpa_reg_open(mvpa_reg_options *a,const char *prefix,int nested,int objective)
{
   char *name;const char *suffix[]={"regression","sparse_features","group_contributions","regression_candidates","regression_scores"};FILE **files[]={&a->fits,&a->features,&a->contributions,&a->candidates,&a->scores};int j;
   if(!a->config.method)return;
   name=malloc(strlen(prefix)+40);if(!name)ERROR_exit(PROGRAM_NAME ": cannot name regression audits");
   for(j=0;j<5;j++){
      if((j==1 && a->config.method!=THD_REG_ELASTIC_NET) || (j==2 && a->config.method!=THD_REG_BANDED_RIDGE) || (j==3 && !nested))continue;
      sprintf(name,"%s.%s.1D",prefix,suffix[j]);*files[j]=mvpa_open_audit(name,"regression audit");mvpa_reg_header(*files[j],a);
   }free(name);
   fputs("# Observed outer fits; component diagnostics are joint for PLS, repeated by response.\n# Columns: Unit\tResponse\tCVFold\tCVStratum\tCandidate\tNTrain\tNTest\tIterations\tResidual\tObjective\tComponents\tNonzero\tCVR2\tMAE\tRMSE\n",a->fits);
   fputs("# Pooled held-out scores by response; undefined correlations/CVR2 are NaN. Each baseline is its training-fold response mean.\n# Columns: Unit\tResponse\tNTest\tPEARSON\tSPEARMAN\tCVR2\tMAE\tRMSE\n",a->scores);
   if(a->features)fputs("# All original predictors, including zero coefficients; coefficients are in fold-preprocessed units. Selected=nonzero fitted coefficient. No activation/significance inference.\n# Columns: Unit\tResponse\tCVFold\tCVStratum\tFeature\tSelected\tCoefficient\n",a->features);
   if(a->contributions)fputs("# Training-centered group scores: variance and covariance with total model score (NTrain-1 divisor). Covariances sum to total score variance; correlated groups may yield negative covariance. Descriptive, not unique/causal contributions or held-out incremental effects.\n# Columns: Unit\tResponse\tCVFold\tCVStratum\tGroup\tLambda\tScoreVariance\tCovarianceWithTotal\n",a->contributions);
   if(a->candidates)fprintf(a->candidates,"# Objective=%s; pooled inner rows then equal response mean. All candidates include repeated preprocessing and supervised fits.\n# Columns: Unit\tCVFold\tCVStratum\tFeatureCount\tPCACount\tCandidate\tInnerLoss\tSelected\tInnerMaxIterations\tInnerMaxResidual\tInnerMinComponents\n",THD_mvpa_tune_objective_name(objective));
}
static void mvpa_reg_write(const mvpa_reg_options *a,const THD_mvpa_ridge_workspace *w,
 THD_3dim_dataset *mset,const THD_roilist *rl,int unit,int p,int q,char **names,int encoding)
{
   int f,o,j,g,c,h,l,nk=w->selection?w->selection->ngrid:1,np=w->pca?w->pca->ngrid:1;
   const THD_foldplan *plan=w->plan;
   if(!a->config.method)return;
   for(o=0;o<q;o++){
      char response[256];const THD_mvpa_continuous_score *s=w->score+o;
      if(encoding)mvpa_neural_id(response,sizeof(response),mset,rl->vox[unit].ar[o]);else snprintf(response,sizeof(response),"%s",names[o]);
      fprintf(a->scores,"%d\t",rl->val[unit]);mvpa_1D_text(a->scores,response);
      fprintf(a->scores,"\t%d\t%.17g\t%.17g\t%.17g\t%.17g\t%.17g\n",s->nsample,s->pearson,s->spearman,s->cv_r2,s->mae,s->rmse);
   }
   for(f=0;f<plan->nfold;f++){
      int chosen=(int)w->selected_lambda[f]-1;
      for(o=0;o<q;o++){
         char response[256];const THD_regression_diagnostic *dg=w->reg_diagnostic+(size_t)f*w->max_output+o;
         const THD_mvpa_continuous_score *s=w->fold_score+(size_t)f*q+o;
         if(encoding)mvpa_neural_id(response,sizeof(response),mset,rl->vox[unit].ar[o]);else snprintf(response,sizeof(response),"%s",names[o]);
#define REG_ROW(fp) do{fprintf(fp,"%d\t",rl->val[unit]);mvpa_1D_text(fp,response);fputc('\t',fp);mvpa_1D_text(fp,plan->name[f]);fputc('\t',fp);mvpa_1D_text(fp,plan->stratum_label[plan->fold_stratum[f]]);}while(0)
         REG_ROW(a->fits);
         fprintf(a->fits,"\t%d\t%d\t%d\t%d\t%.17g\t%.17g\t%d\t%d\t%.17g\t%.17g\t%.17g\n",chosen+1,plan->ntrain[f],plan->ntest[f],dg->iterations,dg->residual,dg->objective,dg->components,dg->nonzero,s->cv_r2,s->mae,s->rmse);
         if(a->features)for(j=0;j<p;j++){
            char feature[256];double b=w->reg_coef[((size_t)f*w->max_feature+j)*w->max_output+o];
            if(encoding)snprintf(feature,sizeof(feature),"%s",names[j]);else mvpa_neural_id(feature,sizeof(feature),mset,rl->vox[unit].ar[j]);
            REG_ROW(a->features);fputc('\t',a->features);mvpa_1D_text(a->features,feature);fprintf(a->features,"\t%d\t%.17g\n",b!=0,b);
         }
         if(a->contributions)for(g=0;g<a->config.ngroup;g++){
            double *v=w->reg_group+(((size_t)f*a->config.ngroup+g)*w->max_output+o)*2;
            REG_ROW(a->contributions);fputc('\t',a->contributions);mvpa_1D_text(a->contributions,a->group_names[g]);fprintf(a->contributions,"\t%.17g\t%.17g\t%.17g\n",a->penalties[(size_t)chosen*a->config.ngroup+g],v[0],v[1]);
         }
#undef REG_ROW
      }
      if(a->candidates)for(c=0;c<nk;c++)for(h=0;h<np;h++)for(l=0;l<w->nlambda;l++){
         size_t at=(((size_t)f*nk+c)*np+h)*w->nlambda+l;
         double loss=w->pca?w->pca->loss[at]:w->selection?w->selection->loss[at]:w->inner_loss[at];
         const THD_regression_diagnostic *dg=w->reg_inner+at;
         fprintf(a->candidates,"%d\t",rl->val[unit]);mvpa_1D_text(a->candidates,plan->name[f]);fputc('\t',a->candidates);mvpa_1D_text(a->candidates,plan->stratum_label[plan->fold_stratum[f]]);
         fprintf(a->candidates,"\t%d\t%d\t%d\t%.17g\t%d\t%d\t%.17g\t%d\n",w->selection?w->selection->grid[c]:p,w->pca?w->pca->grid[h]:0,l+1,loss,l==chosen && (!w->selection || c==w->selection->chosen[f]) && (!w->pca || h==w->pca->chosen[f]),dg->iterations,dg->residual,dg->components);
      }
   }
}
static void mvpa_reg_close(mvpa_reg_options *a)
{
   FILE *files[]={a->fits,a->features,a->contributions,a->candidates,a->scores};int i,failed=0;
   for(i=0;i<5;i++)if(files[i]){if(ferror(files[i]))failed=1;if(fclose(files[i]))failed=1;}
   if(failed)ERROR_exit(PROGRAM_NAME ": error writing regression audits");
}

static void mvpa_continuous_analysis(THD_sampleset *ss,const THD_foldplan *plan,
 THD_3dim_dataset *mset,THD_roilist *rl,int encoding,const char *columns,
 int preprocess,const int *groups,double lambda,int metric,const char *prefix,
 const char *predfile,const char *foldfile,int nworker,int quiet,int progress_mode,
 const THD_nested_foldplan *nested,const double *grid,int nl,const char *inner_column,
 const PERM_set *pset,const char *permute_within,
 const float *nuisance,int nnuisance,const char *nuisance_columns,
 const int *feature_counts,int nfeature_counts,const mvpa_selection_audit *selection_audit,
 int tune_requested,int tune_objective,FILE *candidate_audit,
 const int *pca_counts,int npca_counts,const mvpa_pca_audit *pca_audit,
 const mvpa_pattern_audit *pattern_audit,
 int two_tables,int response_lambda,THD_mvpa_model *frozen_model,mvpa_reg_options *reg,int argc,char **argv)
{
   MVPA_progress progress;
   char **names=NULL,err[1024],label[256],*tablefile,runerr[1024]="";
   float *attr,**patterns,*values,*paint;int na,maxvox,r,o,k,b,nout,nbrick,i,failed=0;
   THD_mvpa_ridge_workspace **workers;THD_3dim_dataset *out;
   FILE *table,*pred=NULL,*fold=NULL,*tuning=NULL;char *tunefile=NULL;
   PERM_result **inference=NULL;
   float **permuted=NULL,**null_score=NULL,**max_null=NULL,*qval=NULL,*null_mean=NULL,*null_valid=NULL;
   unsigned char *valid=NULL;
   int np=pset?pset->nperm:0;double direction=metric==THD_METRIC_MAE || metric==THD_METRIC_RMSE?-1.0:1.0;
   const char *mlabel[]={"PEARSON","SPEARMAN","CVR2","MAE","RMSE"};
   const char *ilabel[]={"NULL_MEAN","ABOVE_NULL","PVAL","ZSCR","QVAL","PFWE","ZFWE","NNULL_VALID"};
   attr=mvpa_numeric_columns(ss,columns,&names,&na);maxvox=THD_roilist_maxvox(rl);
   nout=encoding?1:na;
   if(nout>(32760-2)/(pset?15:7))ERROR_exit(PROGRAM_NAME ": too many output targets");
   nbrick=nout*(pset?15:7)+2;
   workers=calloc((size_t)nworker,sizeof(*workers));patterns=calloc((size_t)nworker,sizeof(*patterns));
   if(!workers || !patterns)ERROR_exit(PROGRAM_NAME ": cannot allocate ridge workers");
   if(pset){
      if((size_t)np>SIZE_MAX/sizeof(float)/(size_t)nout)
         ERROR_exit(PROGRAM_NAME ": continuous null dimensions too large");
      inference=calloc((size_t)nout,sizeof(*inference));
      permuted=calloc((size_t)nworker,sizeof(*permuted));
      null_score=calloc((size_t)nworker,sizeof(*null_score));max_null=calloc((size_t)nworker,sizeof(*max_null));
      valid=calloc((size_t)nout*rl->nroi,1);
      qval=malloc(sizeof(float)*(size_t)nout*rl->nroi);
      null_mean=malloc(sizeof(float)*(size_t)nout*rl->nroi);
      null_valid=calloc((size_t)nout*rl->nroi,sizeof(float));
      if(!inference || !permuted || !null_score || !max_null || !valid || !qval || !null_mean || !null_valid)
         ERROR_exit(PROGRAM_NAME ": cannot allocate continuous inference");
      for(o=0;o<nout;o++){
         inference[o]=THD_perm_result_new(rl->nroi,np);
         if(!inference[o])ERROR_exit(PROGRAM_NAME ": cannot allocate continuous inference family");
         inference[o]->tail=PERM_TAIL_ONE;
         for(i=0;i<np;i++)inference[o]->max_null[i]=-FLT_MAX;
      }
   }
   for(i=0;i<nworker;i++){
      workers[i]=THD_mvpa_ridge_workspace_alloc(plan,encoding?na:maxvox,encoding?maxvox:na,
                                               preprocess,err,sizeof(err));
      patterns[i]=malloc(sizeof(float)*(size_t)ss->nsample*maxvox);
      if(!workers[i] || !patterns[i])ERROR_exit(PROGRAM_NAME ": cannot allocate ridge worker: %s",err);
      if(nuisance && !THD_preprocess_set_nuisance(workers[i]->prep,nuisance,ss->nsample,nnuisance,err,sizeof(err)))
         ERROR_exit(PROGRAM_NAME ": %s",err);
      if(nested && !THD_mvpa_ridge_set_tuning(workers[i],nested,grid,nl,err,sizeof(err)))
         ERROR_exit(PROGRAM_NAME ": %s",err);
      if(nfeature_counts && !THD_mvpa_ridge_set_selection(workers[i],feature_counts,nfeature_counts,err,sizeof(err)))
         ERROR_exit(PROGRAM_NAME ": %s",err);
      if(npca_counts && !THD_mvpa_ridge_set_pca(workers[i],pca_counts,npca_counts,pca_audit->loadings!=NULL,err,sizeof(err)))
         ERROR_exit(PROGRAM_NAME ": %s",err);
      if(nested && !THD_mvpa_ridge_set_objective(workers[i],tune_objective,err,sizeof(err)))
         ERROR_exit(PROGRAM_NAME ": %s",err);
      if(response_lambda && !THD_mvpa_ridge_enable_response_lambda(workers[i],err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      if(pattern_audit->table && !THD_mvpa_ridge_enable_patterns(workers[i],err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      if(reg->config.method && !THD_mvpa_set_regression(workers[i],&reg->config,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      if(pset){
         size_t j;
         permuted[i]=malloc(sizeof(float)*(size_t)ss->nsample*na);
         null_score[i]=malloc(sizeof(float)*(size_t)np*nout);
         max_null[i]=malloc(sizeof(float)*(size_t)np*nout);
         if(!permuted[i] || !null_score[i] || !max_null[i])ERROR_exit(PROGRAM_NAME ": cannot allocate continuous null worker");
         for(j=0;j<(size_t)np*nout;j++)max_null[i][j]=-FLT_MAX;
      }
   }
   mvpa_reg_open(reg,prefix,nested!=NULL,tune_objective);
   if((size_t)rl->nroi>SIZE_MAX/sizeof(float)/(size_t)nbrick)
      ERROR_exit(PROGRAM_NAME ": continuous output dimensions too large");
   values=calloc((size_t)nbrick*rl->nroi,sizeof(float));
   paint=malloc(sizeof(float)*(size_t)rl->nroi);
   tablefile=malloc(strlen(prefix)+20);
   if(!values || !paint || !tablefile)ERROR_exit(PROGRAM_NAME ": out of memory in continuous analysis");
   sprintf(tablefile,"%s.continuous.1D",prefix);
   if((predfile && !strcmp(predfile,tablefile)) || (foldfile && !strcmp(foldfile,tablefile)))
      ERROR_exit(PROGRAM_NAME ": continuous result and audit filenames must differ");
   table=mvpa_open_audit(tablefile,"continuous results");mvpa_nuisance_header(table,nuisance_columns);mvpa_tune_header(table,tune_requested,tune_objective);
   if(response_lambda)fputs("# RidgePenaltyScope=per_response; shared feature/PCA configuration minimizes mean response-wise minimum inner loss. Score aggregation and correction families unchanged.\n",table);
   fprintf(table,"# 3dMVPA %s; %s=%s; scale=%s; primary_metric=%s\n",
           encoding?"encode":"decode",reg->config.method?"candidate":"lambda",nested?"nested":"fixed",THD_preprocess_name(preprocess),THD_mvpa_metric_name(metric));
   if(!nested && !reg->config.method)fprintf(table,"# fixed lambda=%.17g\n",lambda);
   fprintf(table,"# Pattern summaries: arithmetic mean over defined per-response metrics; NaN when none are defined.\n");
   fprintf(table,"# Undefined metrics are NaN here and zero in maps; NVALID counts defined primary-metric responses.\n");
   if(pset)fprintf(table,"# Inference in PREFIX.inference.1D: %d identity-inclusive row permutations within %s; one spatial family per decoding target (one for encoding).\n",np,permute_within);
   fprintf(table,"# Columns: Unit\tResponse\tNResponse\tNValid\tSCORE\tPEARSON\tSPEARMAN\tCVR2\tMAE\tRMSE\n");
   if(predfile){
      pred=mvpa_open_audit(predfile,"continuous predictions");mvpa_nuisance_header(pred,nuisance_columns);mvpa_tune_header(pred,tune_requested,tune_objective);
      if(response_lambda)fputs("# RidgePenaltyScope=per_response; selected penalties are in fold and tuning audits.\n",pred);
      fprintf(pred,"# Columns: Unit\tResponse\tTableRow\tSample\tSubject\tRun\tCVFold\tObserved\tPredicted\tTrainingMean\tItem\tInputFold\tCVStratum");
      if(two_tables)mvpa_source_fields(pred,ss,-1);
      fputc('\n',pred);
   }
   if(foldfile){
      fold=mvpa_open_audit(foldfile,"continuous fold scores");mvpa_nuisance_header(fold,nuisance_columns);mvpa_tune_header(fold,tune_requested,tune_objective);
      if(response_lambda)fputs("# RidgePenaltyScope=per_response; Lambda and InnerLoss refer to this response; InnerNValid=0/1 (undefined/defined inner response loss).\n",fold);
      fprintf(fold,"# Columns: Unit\tResponse\tCVFold\tCVStratum\tNTrain\tNTest\tPEARSON\tSPEARMAN\tCVR2\tMAE\tRMSE\t%s\tInnerNFold\tInnerNValid\tInnerLoss\n",reg->config.method?"Candidate":"Lambda");
   }
   if(nested){
      tunefile=malloc(strlen(prefix)+20);if(!tunefile)ERROR_exit(PROGRAM_NAME ": cannot name tuning audit");
      sprintf(tunefile,"%s.tuning.1D",prefix);
      if((predfile && !strcmp(predfile,tunefile)) || (foldfile && !strcmp(foldfile,tunefile)))
         ERROR_exit(PROGRAM_NAME ": tuning result and prediction/fold filenames must differ");
      tuning=mvpa_open_audit(tunefile,"lambda tuning audit");mvpa_nuisance_header(tuning,nuisance_columns);
      if(response_lambda)mvpa_response_header(tuning,tune_objective);
      else{
      if(npca_counts)fputs("# Losses condition on selected PCA count; all candidates are in PREFIX.pca_tuning.1D.\n",tuning);
      if(nfeature_counts)fputs("# Losses condition on the selected feature count; all pairs are in PREFIX.feature_tuning.1D.\n",tuning);
      mvpa_tune_header(tuning,tune_requested,tune_objective);
      if(reg->config.method)fprintf(tuning,"# inner_group=%s; objective=%s; ties=first_candidate\n",inner_column,THD_mvpa_tune_objective_name(tune_objective));
      else if(tune_requested==THD_TUNE_DEFAULT)fprintf(tuning,"# inner_group=%s; objective=mean_response_SSE_over_training_mean_baseline_SSE; ties_or_no_valid_responses=largest_lambda\n",inner_column);
      else fprintf(tuning,"# inner_group=%s; objective=%s; ties=largest_lambda\n",inner_column,THD_mvpa_tune_objective_name(tune_objective));
      fprintf(tuning,"# Columns: Unit\tCVFold\tCVStratum\tInnerNFold\tInnerNValid\t%s\tInnerLoss\tSelected\n",reg->config.method?"Candidate":"Lambda");
      }
   }
   if(reg->config.method){
      FILE *audit[]={table,pred,fold,tuning,candidate_audit,selection_audit->features,selection_audit->tuning,pca_audit->summary,pca_audit->tuning};
      int j;for(j=0;j<9;j++)if(audit[j]){
         mvpa_reg_header(audit[j],reg);
         fputs("# For this estimator, generic pipeline Penalty fields hold ONE-based regression candidate IDs. First candidate wins exact ties after smaller feature/PCA counts.\n",audit[j]);
      }
   }
   mvpa_progress_init(&progress,progress_mode,quiet,encoding?"Encoding":"Decoding",rl->nroi,"locations");
   AFNI_OMP_START;
#ifdef USE_OMP
#pragma omp parallel for num_threads(nworker) schedule(dynamic,1) ordered
#endif
   for(r=0;r<rl->nroi;r++){
      int q=encoding?rl->vox[r].nar:na,tid=0,ok,o,k,b,i,f;
      char localerr[1024];float *neural;THD_mvpa_ridge_workspace *w;
      const float *xx,*yy;
#ifdef USE_OMP
      tid=omp_get_thread_num();
#endif
      neural=patterns[tid];w=workers[tid];
      ok=THD_roi_samples(ss,NULL,ss->nsample,rl->vox+r,neural,localerr,sizeof(localerr));
      xx=encoding?attr:neural;yy=encoding?neural:attr;
      if(ok && pset)ok=THD_mvpa_run_ridge_permutations(w,xx,encoding?na:rl->vox[r].nar,yy,q,
         groups,lambda,pset,encoding,metric,encoding,permuted[tid],null_score[tid],localerr,sizeof(localerr));
      else if(ok)ok=THD_mvpa_run_ridge_cv(w,xx,encoding?na:rl->vox[r].nar,yy,q,groups,lambda,localerr,sizeof(localerr));
#ifdef USE_OMP
#pragma omp ordered
#endif
      {
      if(!ok){
         if(!failed){failed=1;snprintf(runerr,sizeof(runerr),"unit %d: %.900s",rl->val[r],localerr);}
      }else{
      if(pset)for(o=0;o<nout;o++){
         PERM_result *pr=inference[o];double mean=0;int ip,count=0;
         float obs=null_score[tid][o];
         valid[(size_t)o*rl->nroi+r]=isfinite(obs);
         pr->stat[r]=isfinite(obs)?(float)(direction*obs):0;
         for(ip=0;ip<np;ip++){
            float v=null_score[tid][(size_t)ip*nout+o];
            float directed=isfinite(v)?(float)(direction*v):isfinite(obs)?FLT_MAX:-FLT_MAX;
            if(isfinite(v)){mean+=v;count++;}
            if(isfinite(obs) && (!isfinite(v) || directed>=pr->stat[r]))pr->cnt_unc[r]++;
            if(directed>max_null[tid][(size_t)o*np+ip])max_null[tid][(size_t)o*np+ip]=directed;
         }
         null_mean[(size_t)o*rl->nroi+r]=count?(float)(mean/count):NAN;
         null_valid[(size_t)o*rl->nroi+r]=(float)count;
         pr->effect[r]=isfinite(obs) && count?(float)(direction*(obs-mean/count)):0;
      }
      if(frozen_model){frozen_model->unit[r].fit=THD_mvpa_ridge_freeze(w,localerr,sizeof(localerr));if(!frozen_model->unit[r].fit)ERROR_exit(PROGRAM_NAME ": %s",localerr);}
      mvpa_reg_write(reg,w,mset,rl,r,encoding?na:rl->vox[r].nar,q,names,encoding);
      mvpa_pattern_write(pattern_audit,w,mset,rl,r,names,na,argc,argv);
      mvpa_cont_candidates(candidate_audit,w,mset,rl,r,encoding?na:rl->vox[r].nar,q,names,encoding,tune_objective);
      mvpa_pca_write(pca_audit,w->pca,plan,mset,rl,r,encoding?names:NULL,w->selection);
      mvpa_cont_candidates(pca_audit->tuning,w,mset,rl,r,encoding?na:rl->vox[r].nar,q,names,encoding,tune_objective);
      {mvpa_selection_audit audit=*selection_audit;if(response_lambda)audit.tuning=NULL;
       mvpa_selection_write(&audit,w->selection,plan,mset,rl,r,encoding?names:NULL,
                           encoding?na:rl->vox[r].nar,grid,w->selected_lambda,lambda);}
      if(response_lambda){
         mvpa_response_candidates(tuning,w,mset,rl,r,encoding?na:rl->vox[r].nar,q,names,encoding,tune_objective);
         mvpa_response_candidates(selection_audit->tuning,w,mset,rl,r,encoding?na:rl->vox[r].nar,q,names,encoding,tune_objective);
      }
      if(tuning && !response_lambda)for(f=0;f<plan->nfold;f++)for(i=0;i<nl;i++){
         fprintf(tuning,"%d\t",rl->val[r]);mvpa_1D_text(tuning,plan->name[f]);
         fputc('\t',tuning);mvpa_1D_text(tuning,plan->stratum_label[plan->fold_stratum[f]]);
         fprintf(tuning,"\t%d\t%d\t%.17g\t%.17g\t%d\n",nested->inner[f]->nfold,
                 w->inner_nvalid[f],grid[i],w->inner_loss[(size_t)f*nl+i],grid[i]==w->selected_lambda[f]);
      }
      for(o=0;o<nout;o++){
         double sum[6]={0,0,0,0,0,0};int count[6]={0,0,0,0,0,0},j;
         for(j=encoding?0:o;j<(encoding?q:o+1);j++)for(k=0;k<6;k++){
            double v=mvpa_cont_value(w->score+j,k==0?metric:THD_METRIC_PEARSON+k-1);
            if(isfinite(v)){sum[k]+=v;count[k]++;}
         }
         b=o*7;
         for(k=0;k<6;k++)values[(size_t)(b+k)*rl->nroi+r]=count[k]?(float)(sum[k]/count[k]):NAN;
         values[(size_t)(b+6)*rl->nroi+r]=(float)count[0];
         fprintf(table,"%d\t",rl->val[r]);mvpa_1D_text(table,encoding?"pattern":names[o]);
         fprintf(table,"\t%d\t%d",encoding?q:1,count[0]);
         for(k=0;k<6;k++)fprintf(table,"\t%.9g",values[(size_t)(b+k)*rl->nroi+r]);
         fputc('\n',table);
      }
      values[(size_t)(nbrick-2)*rl->nroi+r]=(float)rl->vox[r].nar;
      values[(size_t)(nbrick-1)*rl->nroi+r]=(float)plan->nfold;
      for(o=0;o<q;o++){
         char response[256];
         if(encoding){
            int v=rl->vox[r].ar[o];
            int surface=DBLK_IS_NI_SURF_DSET(mset->dblk) || DBLK_IS_GIFTI(mset->dblk);
            int id=surface && mset->dblk->node_list?mset->dblk->node_list[v]:v;
            snprintf(response,sizeof(response),"%s_%d",surface?"node":"voxel",id);
         }
         else snprintf(response,sizeof(response),"%s",names[o]);
         if(pred)for(i=0;i<ss->nsample;i++){
            f=plan->sample_test_fold[i];
            fprintf(pred,"%d\t",rl->val[r]);mvpa_1D_text(pred,response);
            fprintf(pred,"\t%d\t",ss->table_row[i]+1);mvpa_1D_text(pred,ss->sample_label[i]);
            fputc('\t',pred);mvpa_1D_text(pred,ss->subject_label[ss->subject[i]]);
            fputc('\t',pred);mvpa_1D_text(pred,ss->run[i]>=0?ss->run_label[ss->run[i]]:NULL);
            fputc('\t',pred);mvpa_1D_text(pred,f>=0?plan->name[f]:NULL);
            fprintf(pred,"\t%.9g",yy[(size_t)i*q+o]);
            if(f>=0)fprintf(pred,"\t%.9g\t%.9g\t",w->prediction[(size_t)i*q+o],w->baseline[(size_t)i*q+o]);
            else fputs("\tNA\tNA\t",pred);
            mvpa_1D_text(pred,ss->item[i]>=0?ss->item_label[ss->item[i]]:NULL);
            fputc('\t',pred);mvpa_1D_text(pred,ss->fold[i]>=0?ss->fold_label[ss->fold[i]]:NULL);
            fputc('\t',pred);mvpa_1D_text(pred,f>=0?plan->stratum_label[plan->fold_stratum[f]]:NULL);
            if(two_tables)mvpa_source_fields(pred,ss,i);
            fputc('\n',pred);
         }
         if(fold)for(f=0;f<plan->nfold;f++){
            fprintf(fold,"%d\t",rl->val[r]);mvpa_1D_text(fold,response);
            fputc('\t',fold);mvpa_1D_text(fold,plan->name[f]);
            fputc('\t',fold);mvpa_1D_text(fold,plan->stratum_label[plan->fold_stratum[f]]);
            fprintf(fold,"\t%d\t%d",plan->ntrain[f],plan->ntest[f]);
            for(k=0;k<5;k++)fprintf(fold,"\t%.9g",mvpa_cont_value(w->fold_score+(size_t)f*q+o,THD_METRIC_PEARSON+k));
            {
               double loss=NAN,penalty=w->selected_lambda[f];int l,valid=w->inner_nvalid[f];
               if(response_lambda){
                  THD_response_ridge *a=w->response_ridge;int config=(w->selection?w->selection->chosen[f]:0)*(w->pca?w->pca->ngrid:1)+(w->pca?w->pca->chosen[f]:0);
                  size_t at=(size_t)f*a->nconfig+config;
                  penalty=a->selected[(size_t)f*w->max_output+o];l=a->best[at*w->max_output+o];
                  loss=a->loss[(at*nl+l)*w->max_output+o];valid=isfinite(loss)?1:0;
               }else if(nested)for(l=0;l<nl;l++)if(grid[l]==w->selected_lambda[f])loss=w->inner_loss[(size_t)f*nl+l];
               fprintf(fold,"\t%.17g\t%d\t%d\t%.17g",penalty,
                       nested?nested->inner[f]->nfold:0,valid,loss);
            }
            fputc('\n',fold);
         }
      }
      }
      }
      if(ok)mvpa_progress_advance(&progress);
   }
   AFNI_OMP_END;
   mvpa_progress_finish(&progress);
   if(ferror(table) || fclose(table))ERROR_exit(PROGRAM_NAME ": error writing continuous results");
   if(pred && (ferror(pred) || fclose(pred)))ERROR_exit(PROGRAM_NAME ": error writing continuous predictions");
   if(fold && (ferror(fold) || fclose(fold)))ERROR_exit(PROGRAM_NAME ": error writing continuous folds");
   if(tuning && (ferror(tuning) || fclose(tuning)))ERROR_exit(PROGRAM_NAME ": error writing tuning audit");
   mvpa_reg_close(reg);
   if(failed)ERROR_exit(PROGRAM_NAME ": %s",runerr);
   if(pset){
      FILE *fp;char *fname=malloc(strlen(prefix)+20);int ww,ip;
      if(!fname)ERROR_exit(PROGRAM_NAME ": cannot name inference audit");
      sprintf(fname,"%s.inference.1D",prefix);
      if((predfile && !strcmp(predfile,fname)) || (foldfile && !strcmp(foldfile,fname)))
         ERROR_exit(PROGRAM_NAME ": inference audit and prediction/fold filenames must differ");
      fp=mvpa_open_audit(fname,"continuous inference audit");mvpa_nuisance_header(fp,nuisance_columns);
      fprintf(fp,"# null=%s_rows_within_%s; nperm=%d; exact=%d; tail=%s; correction_family=spatial_per_response\n",
              encoding?"design":"target",permute_within,np,pset->is_exact,direction>0?"upper":"lower");
      fprintf(fp,"# All preprocessing/tuning/models refitted per relabeling. Undefined observed metrics have p=q=pfwe=1 and z=0.\n");
      fprintf(fp,"# Undefined null metrics count as exceedances for defined observations and conservatively enter that slot's family maximum.\n");
      fprintf(fp,"# Columns: Unit\tResponse\tSCORE\tNULL_MEAN\tABOVE_NULL\tPVAL\tZSCR\tQVAL\tPFWE\tZFWE\tNNULL_VALID\tNPERM\n");
      for(o=0;o<nout;o++){
         PERM_result *pr=inference[o];
         for(ww=0;ww<nworker;ww++)THD_max_accum(np,pr->max_null,max_null[ww]+(size_t)o*np);
         for(ip=0;ip<np;ip++)if(pr->max_null[ip]==-FLT_MAX)pr->max_null[ip]=0;
         THD_perm_result_finish(pr,valid+(size_t)o*rl->nroi);
         THD_bh_fdr_masked(rl->nroi,pr->p_unc,NULL,qval+(size_t)o*rl->nroi);
         for(r=0;r<rl->nroi;r++){
            float vv[8]={null_mean[(size_t)o*rl->nroi+r],pr->effect[r],pr->p_unc[r],pr->z_unc[r],
                         qval[(size_t)o*rl->nroi+r],pr->p_fwe[r],pr->z_fwe[r],null_valid[(size_t)o*rl->nroi+r]};
            for(k=0;k<8;k++)values[(size_t)(nout*7+o*8+k)*rl->nroi+r]=vv[k];
            fprintf(fp,"%d\t",rl->val[r]);mvpa_1D_text(fp,encoding?"pattern":names[o]);
            fprintf(fp,"\t%.9g",values[(size_t)(o*7)*rl->nroi+r]);
            for(k=0;k<8;k++)fprintf(fp,"\t%.9g",vv[k]);
            fprintf(fp,"\t%d\n",np);
         }
      }
      if(ferror(fp) || fclose(fp))ERROR_exit(PROGRAM_NAME ": error writing inference audit");
      free(fname);
      if(!quiet)INFO_message(PROGRAM_NAME ": %d continuous inference famil%s, %d spatial units each, %d synchronized row relabelings",
                            nout,nout==1?"y":"ies",rl->nroi,np);
   }
   out=EDIT_empty_copy(mset);
   if(!out)ERROR_exit(PROGRAM_NAME ": cannot allocate continuous map");
   EDIT_dset_items(out,ADN_prefix,prefix,ADN_nvals,nbrick,ADN_ntt,0,
                  ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_none);
   if(THD_deathcon() && THD_is_file(DSET_HEADNAME(out)))ERROR_exit(PROGRAM_NAME ": output dataset already exists");
   for(b=0;b<nbrick;b++){
      if(b>=nbrick-2)snprintf(label,sizeof(label),"%s",b==nbrick-2?"NFEAT":"NFOLD");
      else if(b<nout*7){
         o=b/7;k=b%7;
         snprintf(label,sizeof(label),"%s_%s",k==0?"SCORE":k==6?"NVALID":mlabel[k-1],encoding?"neural":names[o]);
      }
      else{
         o=(b-nout*7)/8;k=(b-nout*7)%8;
         snprintf(label,sizeof(label),"%s_%s",ilabel[k],encoding?"neural":names[o]);
      }
      EDIT_substitute_brick(out,b,MRI_float,NULL);
      for(r=0;r<rl->nroi;r++)paint[r]=isfinite(values[(size_t)b*rl->nroi+r])?values[(size_t)b*rl->nroi+r]:0.0f;
      THD_roilist_paint(DSET_ARRAY(out,b),rl,paint);
      EDIT_BRICK_LABEL(out,b,label);EDIT_BRICK_TO_NOSTAT(out,b);EDIT_BRICK_FACTOR(out,b,0.0f);
      if(pset && b>=nout*7 && b<nbrick-2 && ((b-nout*7)%8==3 || (b-nout*7)%8==6))EDIT_BRICK_TO_FIZT(out,b);
   }
   THD_copy_labeltable_atr(out->dblk,mset->dblk);
   if(!THD_copy_surface_node_domain(out,mset,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
   tross_Copy_History(mset,out);tross_Make_History(PROGRAM_NAME,argc,argv,out);
   if(DSET_write(out)!=True)ERROR_exit(PROGRAM_NAME ": failed writing continuous map");
   if(!quiet)INFO_message(PROGRAM_NAME ": %s %s: %d units, %d folds; wrote %s",mvpa_reg_name(reg->config.method),encoding?"encoding":"decoding",rl->nroi,plan->nfold,tablefile);
   DSET_delete(out);
   for(i=0;i<nworker;i++){THD_mvpa_ridge_workspace_free(workers[i]);free(patterns[i]);}
   if(pset){
      for(i=0;i<nworker;i++){free(permuted[i]);free(null_score[i]);free(max_null[i]);}
      for(o=0;o<nout;o++)THD_perm_result_free(inference[o]);
      free(inference);free(permuted);free(null_score);free(max_null);
      free(valid);free(qval);free(null_mean);free(null_valid);
   }
   free(workers);free(patterns);
   free(tunefile);
   THD_sampleset_free_levels(names,na);free(attr);free(values);free(paint);free(tablefile);
}

/* Descriptive refits are an independent final pass. They cannot replace CV
   predictions, tuning diagnostics or permutation statistics. Neural axes are
   mapped at their actual feature/response rows, never at searchlight centers. */
static THD_3dim_dataset *mvpa_coefficient_map(THD_3dim_dataset *mset,
 const char *prefix,int nb,int argc,char **argv)
{
   int b;char err[512];THD_3dim_dataset *out=EDIT_empty_copy(mset);
   if(!out || nb<1 || nb>32760)ERROR_exit(PROGRAM_NAME ": invalid descriptive map dimensions");
   EDIT_dset_items(out,ADN_prefix,prefix,ADN_nvals,nb,ADN_ntt,0,
                  ADN_type,HEAD_FUNC_TYPE,ADN_func_type,FUNC_BUCK_TYPE,ADN_none);
   if(THD_deathcon() && THD_is_file(DSET_HEADNAME(out)))
      ERROR_exit(PROGRAM_NAME ": descriptive dataset '%s' already exists",prefix);
   for(b=0;b<nb;b++){
      float *v=calloc((size_t)DSET_NVOX(mset),sizeof(float));
      if(!v)ERROR_exit(PROGRAM_NAME ": cannot allocate descriptive map");
      EDIT_substitute_brick(out,b,MRI_float,v);
      EDIT_BRICK_TO_NOSTAT(out,b);EDIT_BRICK_FACTOR(out,b,0.0f);
   }
   if(!THD_copy_surface_node_domain(out,mset,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
   tross_Copy_History(mset,out);tross_Make_History(PROGRAM_NAME,argc,argv,out);
   return out;
}

static void mvpa_neural_id(char *text,int size,THD_3dim_dataset *mset,int row)
{
   int surface=DBLK_IS_NI_SURF_DSET(mset->dblk) || DBLK_IS_GIFTI(mset->dblk);
   int id=surface && mset->dblk->node_list?mset->dblk->node_list[row]:row;
   snprintf(text,size,"%s_%d",surface?"node":"voxel",id);
}

static float mvpa_coefficient_float(double v)
{
   if(!isfinite(v) || fabs(v)>FLT_MAX)
      ERROR_exit(PROGRAM_NAME ": descriptive coefficient exceeds map float range");
   return (float)v;
}

static void mvpa_coefficient_prefix(char *name,size_t capacity,
 THD_3dim_dataset *mset,const char *prefix,int combined,int id)
{
   int surface=DBLK_IS_NI_SURF_DSET(mset->dblk) || DBLK_IS_GIFTI(mset->dblk);
   size_t stemlen=strlen(prefix);
   if(surface && stemlen>=10 && !strcmp(prefix+stemlen-10,".niml.dset"))stemlen-=10;
   if(combined)snprintf(name,capacity,"%.*s%s",(int)stemlen,prefix,surface?".niml.dset":"");
   else snprintf(name,capacity,"%.*s.unit_%d%s",(int)stemlen,prefix,id,surface?".niml.dset":"");
}

typedef struct {char *path;int pattern;} mvpa_pattern_path;
static int mvpa_pattern_path_order(const void *a,const void *b)
{return strcmp(((const mvpa_pattern_path *)a)->path,((const mvpa_pattern_path *)b)->path);}
/* Resolve existing parent directories so relative, absolute and ./ aliases
   cannot route an explicit audit onto a generated map or sidecar. */
static char *mvpa_pattern_canonical_path(const char *name)
{
   char *copy=strdup(name),*slash,*base,*directory,*result;size_t len;
   if(!copy)return NULL;
   slash=strrchr(copy,'/');base=strdup(slash?slash+1:copy);
   if(!base){free(copy);return NULL;}
   if(slash){if(slash==copy)slash[1]='\0';else *slash='\0';}
   directory=realpath(slash?copy:".",NULL);
   if(!directory){free(copy);free(base);return strdup(name);}
   len=strlen(directory)+strlen(base)+2;result=malloc(len);
   if(result)snprintf(result,len,"%s/%s",directory,base);
   free(copy);free(base);free(directory);return result;
}
static void mvpa_pattern_path_add(mvpa_pattern_path **paths,size_t *n,size_t *capacity,const char *name,int pattern)
{
   if(!name || !*name)return;
   if(*n==*capacity){
      mvpa_pattern_path *p;size_t cap=*capacity?*capacity*2:64;
      if(cap<*capacity || cap>SIZE_MAX/sizeof(*p))ERROR_exit(PROGRAM_NAME ": too many pattern output paths");
      p=realloc(*paths,cap*sizeof(*p));if(!p)ERROR_exit(PROGRAM_NAME ": cannot allocate pattern paths");*paths=p;*capacity=cap;
   }
   (*paths)[*n].path=mvpa_pattern_canonical_path(name);(*paths)[*n].pattern=pattern;
   if(!(*paths)[*n].path)ERROR_exit(PROGRAM_NAME ": cannot allocate pattern path");(*n)++;
   if(pattern && !THD_ok_overwrite() && THD_is_file(name))ERROR_exit(PROGRAM_NAME ": activation-pattern output '%s' already exists",name);
}
static void mvpa_pattern_dataset_paths(mvpa_pattern_path **paths,size_t *n,size_t *capacity,
 THD_3dim_dataset *tmp,const char *prefix,int pattern)
{
   EDIT_dset_items(tmp,ADN_prefix,prefix,ADN_none);
   mvpa_pattern_path_add(paths,n,capacity,DSET_HEADNAME(tmp),pattern);
   if(strcmp(DSET_HEADNAME(tmp),DSET_BRIKNAME(tmp)))mvpa_pattern_path_add(paths,n,capacity,DSET_BRIKNAME(tmp),pattern);
}
static void mvpa_pattern_preflight(const char *prefix,const char *score_prefix,const char *coef_prefix,
 const char **files,int nfiles,THD_3dim_dataset *mset,const THD_foldplan *plan,const THD_roilist *rl)
{
   const char *suffix[]={".roi.1D",".continuous.1D",".inference.1D",".tuning.1D",".features.1D",".feature_tuning.1D",".pca.1D",".pca_tuning.1D"};
   mvpa_pattern_path *paths=NULL;size_t n=0,capacity=0,i,len=strlen(prefix)+strlen(score_prefix)+(coef_prefix?strlen(coef_prefix):0)+96;
   char *name=malloc(len);THD_3dim_dataset *tmp=EDIT_empty_copy(mset);int f,r;
   if(!name || !tmp)ERROR_exit(PROGRAM_NAME ": cannot check pattern output paths");
   for(f=0;f<plan->nfold;f++)if(plan->ntrain[f]<2)ERROR_exit(PROGRAM_NAME ": activation patterns need at least two outer training samples");
   for(i=0;i<2;i++){
      snprintf(name,len,"%s.%s.1D",prefix,i?"pattern_covariance":"patterns");
      mvpa_pattern_path_add(&paths,&n,&capacity,name,1);
   }
   for(r=0;r<rl->nroi;r++)for(f=0;f<plan->nfold;f++){
      mvpa_pattern_mapname(name,len,mset,prefix,rl->val[r],f);
      mvpa_pattern_dataset_paths(&paths,&n,&capacity,tmp,name,1);
   }
   mvpa_pattern_dataset_paths(&paths,&n,&capacity,tmp,score_prefix,0);
   for(i=0;i<8;i++){snprintf(name,len,"%s%s",score_prefix,suffix[i]);mvpa_pattern_path_add(&paths,&n,&capacity,name,0);}
   for(r=0;r<nfiles;r++)mvpa_pattern_path_add(&paths,&n,&capacity,files[r],0);
   if(coef_prefix){
      for(i=0;i<2;i++){
         snprintf(name,len,"%s.%s.1D",coef_prefix,i?"preprocessing":"coefficients");mvpa_pattern_path_add(&paths,&n,&capacity,name,0);
      }
      for(r=0;r<rl->nroi;r++){
         mvpa_coefficient_prefix(name,len,mset,coef_prefix,0,rl->val[r]);
         mvpa_pattern_dataset_paths(&paths,&n,&capacity,tmp,name,0);
      }
   }
   qsort(paths,n,sizeof(*paths),mvpa_pattern_path_order);
   for(i=1;i<n;i++)if(!strcmp(paths[i-1].path,paths[i].path) && (paths[i-1].pattern || paths[i].pattern))
      ERROR_exit(PROGRAM_NAME ": activation-pattern and other output filenames must differ: '%s'",paths[i].path);
   for(i=0;i<n;i++)free(paths[i].path);free(paths);free(name);DSET_delete(tmp);
}

static void mvpa_descriptive_coefficients(THD_sampleset *ss,THD_3dim_dataset *mset,
 THD_roilist *rl,int encoding,int searchlight,const char *columns,
 const int *classes,char **class_names,int nc,int preprocess,const int *groups,
 double lambda,const char *prefix,int quiet,int argc,char **argv)
{
   char **names=NULL,err[1024],feature[256],response[256],label[256];
   char *fname=malloc(strlen(prefix)+32),*mapname=malloc(strlen(prefix)+64);
   float *attr,*neural;const double *coef,*intercept;
   int na,n=ss->nsample,maxvox=THD_roilist_maxvox(rl),maxp,maxq;
   int r,j,o,g,i,combined=encoding && !searchlight;
   FILE *table,*prepfile;THD_mvpa_refit_workspace *refit;THD_3dim_dataset *out=NULL;
   if(!fname || !mapname)ERROR_exit(PROGRAM_NAME ": cannot name descriptive outputs");
   if(classes){
      na=nc;names=class_names;attr=calloc((size_t)n*nc,sizeof(float));
      if(!attr)ERROR_exit(PROGRAM_NAME ": cannot allocate descriptive class indicators");
      for(i=0;i<n;i++)attr[(size_t)i*nc+classes[i]]=1;
   }else attr=mvpa_numeric_columns(ss,columns,&names,&na);
   maxp=encoding?na:maxvox;maxq=encoding?maxvox:na;
   if((size_t)maxp>SIZE_MAX/sizeof(double)/(size_t)maxq ||
      (size_t)n>SIZE_MAX/sizeof(float)/(size_t)maxp)
      ERROR_exit(PROGRAM_NAME ": descriptive refit dimensions too large");
   neural=malloc(sizeof(float)*(size_t)n*maxvox);
   refit=THD_mvpa_refit_alloc(n,maxp,maxq,err,sizeof(err));
   if(!neural || !refit)ERROR_exit(PROGRAM_NAME ": cannot allocate descriptive refit");
   sprintf(fname,"%s.coefficients.1D",prefix);table=mvpa_open_audit(fname,"descriptive coefficients");
   fprintf(table,"# Descriptive full-data ridge refit; NOT held-out scores, activation/Haufe maps, or coefficient significance.\n");
   fprintf(table,"# scale=%s; lambda=%.17g; nfit=%d; objective=sum_squared_error+lambda*sum_squared_slopes; intercept_unpenalized\n",
           THD_preprocess_name(preprocess),lambda,n);
   fprintf(table,"# y = Intercept + sum(preprocessed_feature * Coefficient); responses retain original units%s.\n",
           classes?" (classification: 0/1 indicators; decisions are not probabilities)":"");
   fprintf(table,"# Preprocessing fitted on ALL samples; see PREFIX.preprocessing.1D. Weights use transformed predictor units.\n");
   fprintf(table,"# Map bricks are zero-based; outside-unit values are zero. Overlapping fits have separate unit maps.\n");
   fprintf(table,"# Columns: Unit\tFeature\tResponse\tCoefficient\tIntercept\tLambda\tNFit\tMapPrefix\tMapBrick\n");
   sprintf(fname,"%s.preprocessing.1D",prefix);prepfile=mvpa_open_audit(fname,"descriptive preprocessing");
   fprintf(prepfile,"# scale=%s; feature_zscore: (x-center)/scale (population SD); constant features become zero.\n",THD_preprocess_name(preprocess));
   fprintf(prepfile,"# none: x unchanged; sample_center: subtract each sample's own mean across this unit's predictors.\n");
   fprintf(prepfile,"# run_center: subtract full-data subject/run feature means; Group is SubjectRunID below.\n");
   if(groups)for(i=0;i<n;i++){
      fprintf(prepfile,"# SampleGroup\t%d\t%d\t",ss->table_row[i]+1,groups[i]);
      mvpa_1D_text(prepfile,ss->subject_label[ss->subject[i]]);fputc('\t',prepfile);
      mvpa_1D_text(prepfile,ss->run_label[ss->run[i]]);fputc('\n',prepfile);
   }
   fprintf(prepfile,"# Columns: Unit\tFeature\tGroup\tCenter\tScale\tConstant\n");
   if(combined){
      mvpa_coefficient_prefix(mapname,strlen(prefix)+64,mset,prefix,1,0);
      out=mvpa_coefficient_map(mset,mapname,na+1,argc,argv);
   }
   for(r=0;r<rl->nroi;r++){
      int p=encoding?na:rl->vox[r].nar,q=encoding?rl->vox[r].nar:na;
      const float *xx,*yy;const THD_preprocessor *prep;
      const THD_mvpa_refit_result *fit;
      if(!THD_roi_samples(ss,NULL,n,rl->vox+r,neural,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      xx=encoding?attr:neural;yy=encoding?neural:attr;
      fit=THD_mvpa_refit_ridge(refit,xx,yy,n,p,q,preprocess,groups,lambda,err,sizeof(err));
      if(!fit)ERROR_exit(PROGRAM_NAME ": descriptive unit %d: %s",rl->val[r],err);
      prep=fit->prep;coef=fit->coef;intercept=fit->intercept;
      if(!combined){
         mvpa_coefficient_prefix(mapname,strlen(prefix)+64,mset,prefix,0,rl->val[r]);
         out=mvpa_coefficient_map(mset,mapname,encoding?na+1:na,argc,argv);
      }
      for(j=0;j<p;j++){
         const char *feature_id;
         if(!encoding)mvpa_neural_id(feature,sizeof(feature),mset,rl->vox[r].ar[j]);
         feature_id=encoding?names[j]:feature;
         for(g=0;g<(preprocess==THD_PREP_RUN_CENTER?prep->ngroup:1);g++){
            double center=0,scale=1;int constant=0;
            if(preprocess==THD_PREP_FEATURE_ZSCORE){center=prep->center[j];scale=prep->scale[j];constant=prep->constant[j];}
            if(preprocess==THD_PREP_RUN_CENTER)center=prep->center[(size_t)g*p+j];
            fprintf(prepfile,"%d\t",rl->val[r]);mvpa_1D_text(prepfile,feature_id);
            if(preprocess==THD_PREP_RUN_CENTER)fprintf(prepfile,"\t%d",prep->group_value[g]);
            else fputs("\tNA",prepfile);
            fprintf(prepfile,"\t%.17g\t%.17g\t%d\n",center,scale,constant);
         }
         for(o=0;o<q;o++){
            int brick=encoding?j:o,row=rl->vox[r].ar[encoding?o:j];
            const char *response_id;
            if(encoding)mvpa_neural_id(response,sizeof(response),mset,row);
            response_id=encoding?response:names[o];
            fprintf(table,"%d\t",rl->val[r]);mvpa_1D_text(table,feature_id);fputc('\t',table);mvpa_1D_text(table,response_id);
            fprintf(table,"\t%.17g\t%.17g\t%.17g\t%d\t",coef[(size_t)j*q+o],intercept[o],lambda,n);
            mvpa_1D_text(table,mapname);fprintf(table,"\t%d\n",brick);
            ((float *)DSET_ARRAY(out,brick))[row]=mvpa_coefficient_float(coef[(size_t)j*q+o]);
         }
      }
      for(i=0;i<na;i++){
         snprintf(label,sizeof(label),"REFIT_B%d_%s",i,names[i]);EDIT_BRICK_LABEL(out,i,label);
      }
      if(encoding){
         EDIT_BRICK_LABEL(out,na,"REFIT_INTERCEPT");
         for(o=0;o<q;o++)((float *)DSET_ARRAY(out,na))[rl->vox[r].ar[o]]=mvpa_coefficient_float(intercept[o]);
      }
      if(!combined){
         if(DSET_write(out)!=True)ERROR_exit(PROGRAM_NAME ": failed writing coefficient map");
         DSET_delete(out);out=NULL;
      }
   }
   if(combined){
      if(DSET_write(out)!=True)ERROR_exit(PROGRAM_NAME ": failed writing coefficient map");
      DSET_delete(out);
   }
   if(ferror(table) || fclose(table))ERROR_exit(PROGRAM_NAME ": error writing coefficients");
   if(ferror(prepfile) || fclose(prepfile))ERROR_exit(PROGRAM_NAME ": error writing descriptive preprocessing");
   if(!quiet)INFO_message(PROGRAM_NAME ": descriptive full-data coefficients: %d units, lambda %.9g; %s.coefficients.1D",rl->nroi,lambda,prefix);
   THD_mvpa_refit_free(refit);free(neural);
   if(!classes)THD_sampleset_free_levels(names,na);
   free(attr);free(fname);free(mapname);
}

/* Check model-independent training requirements without allocating a solver.
   Inner-fold indexes are local to the outer training set. */
static void mvpa_validate_fold_inputs(const THD_foldplan *plan,
   const int *classes,int nclass,const int *groups,int lda,const char *scope)
{
   char err[512];
   if(!THD_mvpa_validate_fold_inputs(plan,classes,nclass,groups,lda,scope,err,sizeof(err)))
      ERROR_exit(PROGRAM_NAME ": %s",err);
}

/* Use the resolved samples, folds and spatial units, not a second input parser.
   Scan the union of selected features once per sample, using the same extraction
   routine as fitting. No estimator, permutation set or result file is created. */
static void mvpa_validate_report(THD_sampleset *ss,const THD_foldplan *plan,
   const THD_nested_foldplan *nested,const THD_roilist *rl,
   const char *spatial_reference,const char *mode,const char *method,const char *target,int continuous,
   const int *classes,char **labels,int nclass,const int *groups,int lda,
   int nperm,const char *permute_within)
{
   int i,j,f,c,nfeature=0,warnings=0;char err[1024];
   unsigned char *selected;intvec vox={0,NULL};float *values;
   THD_sampleset_identity *identity;
   printf("\n# 3dMVPA input validation (no model fitting)\n");
   printf("# Mode\t%s\n# Method\t%s\n# TargetColumns\t%s\n",mode,method,target);
   printf("# SpatialReference\t%s\n# SampleGridOrNodeDomain\tMATCHED\n",spatial_reference);
   printf("# Samples\t%d\n# Subjects\t%d\n# RunLevels\t%d\n# Datasets\t%d\n# Folds\t%d\n",
          ss->nsample,ss->nsubject,ss->nrun,ss->ndset,plan->nfold);
   if(continuous){
      char **names=NULL;int count=0;
      float *numeric=mvpa_numeric_columns(ss,target,&names,&count);
      printf("# FiniteNumericColumns\t%d\n",count);
      free(numeric);THD_sampleset_free_levels(names,count);
   }
   mvpa_validate_fold_inputs(plan,classes,nclass,groups,lda,"outer");
   if(nested)for(f=0;f<plan->nfold;f++){
      int *local_class=classes?malloc(sizeof(int)*(size_t)plan->ntrain[f]):NULL;
      int *local_group=groups?malloc(sizeof(int)*(size_t)plan->ntrain[f]):NULL;
      char scope[256];
      if((classes && !local_class) || (groups && !local_group))
         ERROR_exit(PROGRAM_NAME ": cannot allocate inner-fold validation rows");
      for(i=0;i<plan->ntrain[f];i++){
         if(classes)local_class[i]=classes[plan->train[f][i]];
         if(groups)local_group[i]=groups[plan->train[f][i]];
      }
      snprintf(scope,sizeof(scope),"inner folds of outer %s",plan->name[f]);
      mvpa_validate_fold_inputs(nested->inner[f],local_class,nclass,local_group,lda,scope);
      free(local_class);free(local_group);
   }
   printf("# FoldSummary\nFold\tStratum\tNTrain\tNTest\n");
   for(f=0;f<plan->nfold;f++)
      printf("%s\t%s\t%d\t%d\n",plan->name[f],plan->stratum_label[plan->fold_stratum[f]],
             plan->ntrain[f],plan->ntest[f]);
   if(classes){
      int *train=calloc((size_t)nclass,sizeof(int)),*test=calloc((size_t)nclass,sizeof(int));
      if(!train || !test)ERROR_exit(PROGRAM_NAME ": cannot allocate validation class counts");
      printf("# Classes\t%d\n# ClassCounts\nClass\tLabel\tNSample\n",nclass);
      for(i=0;i<ss->nsample;i++)train[classes[i]]++;
      for(c=0;c<nclass;c++)printf("%d\t%s\t%d\n",c,labels[c],train[c]);
      printf("# FoldClassCounts\nFold\tStratum\tClass\tLabel\tNTrain\tNTest\n");
      for(f=0;f<plan->nfold;f++){
         memset(train,0,sizeof(int)*(size_t)nclass);memset(test,0,sizeof(int)*(size_t)nclass);
         for(i=0;i<plan->ntrain[f];i++)train[classes[plan->train[f][i]]]++;
         for(i=0;i<plan->ntest[f];i++)test[classes[plan->test[f][i]]]++;
         for(c=0;c<nclass;c++){
            printf("%s\t%s\t%d\t%s\t%d\t%d\n",plan->name[f],
                   plan->stratum_label[plan->fold_stratum[f]],c,labels[c],train[c],test[c]);
            if(!test[c] && plan->kind!=THD_CV_TRAIN_ONLY){
               printf("# WARNING: fold %s has no test observations of class %s; its BACC omits that class.\n",
                      plan->name[f],labels[c]);warnings++;
            }
         }
      }
      free(train);free(test);
   }
   identity=THD_sampleset_build_identity(ss,err,sizeof(err));
   if(!identity)ERROR_exit(PROGRAM_NAME ": %s",err);
   for(i=0;i<ss->nsample;i++){
      j=identity->first_sample[i];
      if(j!=i){
         printf("# WARNING: repeated beta selection in rows %d (%s) and %d (%s); inspect fold assignments.\n",
                j+1,ss->sample_label[j],i+1,ss->sample_label[i]);warnings++;
      }
   }
   THD_sampleset_free_identity(identity);
   if(nperm>0 && strcasecmp(permute_within,"none")){
      int *code=NULL,count=0;char **levels=NULL;
      if(!THD_sampleset_encode_column(ss,permute_within,&code,&levels,&count,err,sizeof(err)))
         ERROR_exit(PROGRAM_NAME ": %s",err);
      printf("# PermutationBlockLevels\t%d\n",count);
      free(code);THD_sampleset_free_levels(levels,count);
   }
   selected=calloc((size_t)ss->nvox,1);
   if(!selected)ERROR_exit(PROGRAM_NAME ": cannot allocate validation feature mask");
   for(i=0;i<rl->nroi;i++)for(j=0;j<rl->vox[i].nar;j++)selected[rl->vox[i].ar[j]]=1;
   for(i=0;i<ss->nvox;i++)nfeature+=selected[i]!=0;
   if(nfeature<1)ERROR_exit(PROGRAM_NAME ": no selected features to validate");
   vox.nar=nfeature;vox.ar=malloc(sizeof(int)*(size_t)nfeature);
   values=malloc(sizeof(float)*(size_t)nfeature);
   if(!vox.ar || !values)ERROR_exit(PROGRAM_NAME ": cannot allocate validation feature buffer");
   for(i=0,j=0;i<ss->nvox;i++)if(selected[i])vox.ar[j++]=i;
   free(selected);
   for(i=0;i<ss->nsample;i++){
      if(!THD_roi_samples(ss,&i,1,&vox,values,err,sizeof(err)))
         ERROR_exit(PROGRAM_NAME ": %s",err);
      for(j=0;j<nfeature;j++)if(!isfinite(values[j]))
         ERROR_exit(PROGRAM_NAME ": sample %d (%s), selected voxel/node row %d is not finite",
                    i+1,ss->sample_label[i],vox.ar[j]);
   }
   free(values);free(vox.ar);
   printf("# SpatialUnits\t%d\n# SelectedFeatures\t%d\n# FiniteSelectedValues\t%llu\n",
          rl->nroi,nfeature,(unsigned long long)ss->nsample*(unsigned long long)nfeature);
   printf("# Values checked after AFNI loading/conversion; reader repairs of bad floats are not a raw-file integrity check.\n");
   printf("# Not checked: event-label correctness, beta/design correspondence beyond index ranges, anatomical QC, or upstream independence.\n");
   printf("# No models fitted, no permutations generated, no result files written; output paths and solver convergence not checked.\n");
   printf("# Validation PASS\t%d warning(s)\n",warnings);
}

/* Fixed-split class coding must not let test-row order or labels choose the
   argmax tie rule. Class coverage has already been checked against training. */
static void mvpa_fixed_class_order(const THD_foldplan *plan,int *classes,
                                   char **labels,int nclass)
{
   char err[512];
   if(!THD_mvpa_fixed_class_order(plan,classes,labels,nclass,err,sizeof(err)))
      ERROR_exit(PROGRAM_NAME ": %s",err);
}

/* Preserve the reserved set even in null fits. Validate block membership before
   drawing permutations, including for -validate_only and small random draws. */
static void mvpa_fixed_permutation_blocks(const THD_sampleset *ss,
   const THD_foldplan *plan,const char *column)
{
   int *code=NULL,*strata=NULL,nlevel=0,ns=0;char **labels=NULL,**slab=NULL,err[1024];
   THD_cv_block_conflict conflict;
   if(!strcasecmp(column,"none"))
      ERROR_exit(PROGRAM_NAME ": fixed partitions require permutation blocks confined to train or test; -permute_within none crosses the split");
   if(!THD_sampleset_encode_column(ss,column,&code,&labels,&nlevel,err,sizeof(err)))
      ERROR_exit(PROGRAM_NAME ": %s",err);
   if(plan->within_column &&
      !THD_sampleset_encode_column(ss,plan->within_column,&strata,&slab,&ns,err,sizeof(err)))
      ERROR_exit(PROGRAM_NAME ": %s",err);
   if(!THD_foldplan_check_permutation_blocks(plan,code,nlevel,strata,&conflict,err,sizeof(err))){
      if(conflict.kind==THD_CV_BLOCK_PARTITION)
         ERROR_exit(PROGRAM_NAME ": permutation block '%s' in '%s' crosses the train/test partition; use a composite partition/block column",labels[conflict.block],column);
      if(conflict.kind==THD_CV_BLOCK_STRATUM)
         ERROR_exit(PROGRAM_NAME ": transfer permutation block '%s' crosses subject strata; use subject/domain/block keys",labels[conflict.block]);
      ERROR_exit(PROGRAM_NAME ": %s",err);
   }
   free(strata);THD_sampleset_free_levels(slab,ns);
   free(code);THD_sampleset_free_levels(labels,nlevel);
}

/* MVPA policy adds roles and source provenance to the shared concatenation.
   Paths, table schemas and row copying are handled by thd_datatable. */
static THD_datatable *mvpa_train_test_table(THD_datatable *train,THD_datatable *test)
{
   THD_datatable *tables[]={train,test},*combined,*out;
   const char *reserved[]={MVPA_PARTITION,MVPA_SOURCE_TABLE,MVPA_SOURCE_ROW};
   char **tok,(*rownum)[32],err[1024];int c,j,s,r,k=0,ncol;
   size_t ntok;
   for(s=0;s<2;s++){
      THD_datatable *dt=tables[s];
      if(dt->nrow<1 || dt->icol_input<0)
         ERROR_exit(PROGRAM_NAME ": both training and test tables need samples and InputFile");
      for(c=0;c<dt->ncol;c++)for(j=0;j<3;j++)if(!strcasecmp(dt->cname[c],reserved[j]))
         ERROR_exit(PROGRAM_NAME ": column '%s' is reserved for two-table provenance",dt->cname[c]);
   }
   combined=THD_datatable_concat_rows(train,test,err,sizeof(err));
   if(!combined)ERROR_exit(PROGRAM_NAME ": %s",err);
   if(combined->ncol>INT_MAX-3)
      ERROR_exit(PROGRAM_NAME ": provenance table has too many columns");
   ncol=combined->ncol+3;
   if((size_t)combined->nrow+1>(size_t)INT_MAX/ncol)
      ERROR_exit(PROGRAM_NAME ": provenance table has too many cells");
   ntok=((size_t)combined->nrow+1)*ncol;
   if(ntok>SIZE_MAX/sizeof(char *) || (size_t)combined->nrow>SIZE_MAX/sizeof(*rownum))
      ERROR_exit(PROGRAM_NAME ": provenance table is too large");
   tok=malloc(ntok*sizeof(char *));rownum=malloc(sizeof(*rownum)*(size_t)combined->nrow);
   if(!tok || !rownum)ERROR_exit(PROGRAM_NAME ": cannot allocate table provenance");
   for(c=0;c<combined->ncol;c++)if(c!=combined->icol_input)tok[k++]=combined->cname[c];
   for(j=0;j<3;j++)tok[k++]=(char *)reserved[j];
   tok[k++]="InputFile";
   for(r=0;r<combined->nrow;r++){
      s=r>=train->nrow;
      for(c=0;c<combined->ncol;c++)if(c!=combined->icol_input)tok[k++]=DT_CELL(combined,r,c);
      snprintf(rownum[r],sizeof(rownum[r]),"%d",(s?r-train->nrow:r)+1);
      tok[k++]=s?"test":"train";
      tok[k++]=tables[s]->source?tables[s]->source:(s?"test table":"training table");
      tok[k++]=rownum[r];tok[k++]=DT_CELL(combined,r,combined->icol_input);
   }
   out=THD_parse_datatable(tok,k,"combined training and test tables");
   free(tok);free(rownum);THD_free_datatable(combined);
   return out;
}

/* MVPA policy rejects an equivalent selection on both sides of a two-table
   split; dataset/brick identity and alias resolution belong to thd_sampleset. */
static void mvpa_check_table_overlap(const THD_sampleset *ss,const THD_foldplan *plan)
{
   int i,j;char err[1024];
   THD_sampleset_identity *identity=THD_sampleset_build_identity(ss,err,sizeof(err));
   if(!identity)ERROR_exit(PROGRAM_NAME ": %s",err);
   for(i=0;i<plan->ntrain[0];i++)for(j=0;j<plan->ntest[0];j++){
      int a=plan->train[0][i],b=plan->test[0][j];
      if(identity->first_sample[a]==identity->first_sample[b])
         ERROR_exit(PROGRAM_NAME ": training sample '%s' and test sample '%s' select the same dataset/brick (%s[%d]); separate tables must not share beta selections",
                    ss->sample_label[a],ss->sample_label[b],identity->dataset_path[ss->sample_dset[a]],ss->sample_brick[a]);
   }
   THD_sampleset_free_identity(identity);
}

/* Standalone descriptive comparison adapter. Shared matching and scoring are
   in thd_mvpa; this layer reads existing audit schemas and writes text only. */
static int mvpa_pair_column(THD_datatable *t,const char *name)
{
   int c=THD_datatable_column(t,(char *)name);
   if(c<0)ERROR_exit(PROGRAM_NAME ": prediction file '%s' lacks column %s",t->source,name);
   return c;
}
static float mvpa_pair_number(const char *s,const char *file,int row,const char *column)
{
   char *end;double d;errno=0;d=strtod(s,&end);
   if(errno || end==s || *end || !isfinite(d) || fabs(d)>FLT_MAX || (d!=0 && (float)d==0))
      ERROR_exit(PROGRAM_NAME ": %s row %d: %s must be finite numeric text",file,row+1,column);
   return (float)d;
}
static char *mvpa_pair_target(const char *file)
{
   FILE *fp=fopen(file,"r");char line[16384],*target=NULL;
   if(!fp)ERROR_exit(PROGRAM_NAME ": cannot open prediction file %s",file);
   while(fgets(line,sizeof(line),fp))if(!strncmp(line,"# Target\t",9)){
      if(target)ERROR_exit(PROGRAM_NAME ": duplicate classification Target header in %s",file);
      line[strcspn(line,"\r\n")]='\0';target=strdup(line+9);
   }
   if(ferror(fp) || fclose(fp))ERROR_exit(PROGRAM_NAME ": cannot read prediction metadata %s",file);
   if(!target || !*target)ERROR_exit(PROGRAM_NAME ": classification prediction file %s needs its original Target header",file);
   return target;
}
static int mvpa_pair_label(const char *s,char ***labels,int *n,int add)
{
   int i;for(i=0;i<*n;i++)if(!strcmp(s,(*labels)[i]))return i;
   if(!add)return -1;
   {char **tmp=realloc(*labels,sizeof(char *)*(size_t)(*n+1));
    if(!tmp)ERROR_exit(PROGRAM_NAME ": cannot allocate paired class labels");*labels=tmp;}
   (*labels)[*n]=(char *)s;return (*n)++;
}
static void mvpa_pair_adapt(THD_datatable *t,int classification,
 THD_mvpa_pair_row *rows,char ***labels,int *nclass,int first)
{
   const char *names[]={classification?"UnitID":"Unit",classification?"Subj":"Subject",
      "Sample","Run","Item","InputFold","CVFold","CVStratum","Observed","Predicted","TableRow"};
   int c[11],i,j,resp=-1,type=-1,base=-1,correct=-1;
   for(i=0;i<t->ncol;i++)for(j=0;j<i;j++)if(!strcasecmp(t->cname[i],t->cname[j]))
      ERROR_exit(PROGRAM_NAME ": duplicate prediction column %s",t->cname[i]);
   for(i=0;i<11;i++)c[i]=mvpa_pair_column(t,names[i]);
   if(classification){type=mvpa_pair_column(t,"UnitType");correct=mvpa_pair_column(t,"Correct");}
   else{resp=mvpa_pair_column(t,"Response");base=mvpa_pair_column(t,"TrainingMean");}
   if(classification && first)for(i=0;i<t->nrow;i++)mvpa_pair_label(DT_CELL(t,i,c[8]),labels,nclass,1);
   for(i=0;i<t->nrow;i++){
      THD_mvpa_pair_row *v=rows+i;const char *obs=DT_CELL(t,i,c[8]),*pred=DT_CELL(t,i,c[9]);
      char *end;long tablerow;
      v->unit=DT_CELL(t,i,c[0]);v->subject=DT_CELL(t,i,c[1]);v->sample=DT_CELL(t,i,c[2]);
      v->run=DT_CELL(t,i,c[3]);v->item=DT_CELL(t,i,c[4]);v->input_fold=DT_CELL(t,i,c[5]);
      v->fold=DT_CELL(t,i,c[6]);v->stratum=DT_CELL(t,i,c[7]);
      v->unit_type=classification?DT_CELL(t,i,type):"Continuous";v->response=classification?"Class":DT_CELL(t,i,resp);
      errno=0;tablerow=strtol(DT_CELL(t,i,c[10]),&end,10);
      if(errno || end==DT_CELL(t,i,c[10]) || *end || tablerow<1 || tablerow>INT_MAX)
         ERROR_exit(PROGRAM_NAME ": invalid TableRow at %s row %d",t->source,i+1);
      v->tested=strcmp(classification?DT_CELL(t,i,correct):pred,"NA")!=0;
      if(!v->tested && (strcmp(pred,"NA") || strcmp(v->fold,"NA") || strcmp(v->stratum,"NA")))
         ERROR_exit(PROGRAM_NAME ": inconsistent training-only row in %s row %d",t->source,i+1);
      if(classification){
         const char *cc=DT_CELL(t,i,correct);
         v->truth_class=mvpa_pair_label(obs,labels,nclass,0);v->prediction_class=v->tested?mvpa_pair_label(pred,labels,nclass,0):-1;
         if(v->truth_class<0 || (v->tested && v->prediction_class<0))ERROR_exit(PROGRAM_NAME ": unknown class label in %s row %d",t->source,i+1);
         if(v->tested && strcmp(cc,v->truth_class==v->prediction_class?"1":"0"))
            ERROR_exit(PROGRAM_NAME ": inconsistent Correct field in %s row %d",t->source,i+1);
         if(strcmp(v->unit_type,"ROI") && strcmp(v->unit_type,"Voxel") && strcmp(v->unit_type,"Node"))
            ERROR_exit(PROGRAM_NAME ": unknown classification UnitType in %s row %d",t->source,i+1);
      }else{
         v->observed=mvpa_pair_number(obs,t->source,i,"Observed");
         if(v->tested){v->predicted=mvpa_pair_number(pred,t->source,i,"Predicted");v->baseline=mvpa_pair_number(DT_CELL(t,i,base),t->source,i,"TrainingMean");}
         else if(strcmp(DT_CELL(t,i,base),"NA"))ERROR_exit(PROGRAM_NAME ": training-only TrainingMean must be NA in %s row %d",t->source,i+1);
      }
   }
}
static void mvpa_pair_text(FILE *fp,const char *s)
{fputc('\t',fp);mvpa_1D_text(fp,s);}
static void mvpa_pair_value(FILE *fp,double x)
{if(isfinite(x))fprintf(fp,"\t%.17g",x);else fputs("\tNA",fp);}
static int mvpa_pair_same_file(const char *a,const char *b)
{
   struct stat sa,sb;char *ca,*cb;int same;
   if(!stat(a,&sa) && !stat(b,&sb) && sa.st_dev==sb.st_dev && sa.st_ino==sb.st_ino)return 1;
   ca=mvpa_pattern_canonical_path(a);cb=mvpa_pattern_canonical_path(b);
   if(!ca || !cb)ERROR_exit(PROGRAM_NAME ": cannot check paired output paths");
   same=!strcmp(ca,cb);free(ca);free(cb);return same;
}
/* Transfer policy is explicit; split construction and overlap counting are
   reusable cross-validation operations. Matching labels do not prove matching
   experimental meaning, registration or upstream independence. */
static void mvpa_transfer_check(mvpa_transfer_options *t,const THD_sampleset *ss,
 const THD_foldplan *plan,int two_tables,int continuous,const char *targets)
{
   char err[1024],**names=NULL;float *numeric=NULL;int i,j,n=0;
   const char *columns[]={t->subject_column,t->item_column,t->session_column};
   THD_sampleset_identity *identity;
   for(i=0;i<ss->metadata->ncol;i++)for(j=0;j<i;j++)
      if(!strcasecmp(ss->metadata->cname[i],ss->metadata->cname[j]))
         ERROR_exit(PROGRAM_NAME ": duplicate transfer column '%s'",ss->metadata->cname[i]);
   for(i=0;i<3;i++){
      if(!strcasecmp(t->column,columns[i]))ERROR_exit(PROGRAM_NAME ": transfer domain must be distinct from subject/item/session columns");
      if(!THD_foldplan_overlap_count(ss,plan,columns[i],i==2?t->subject_column:NULL,t->overlap+i,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
   }
   if(!strcmp(t->subjects,"disjoint") && t->overlap[0])ERROR_exit(PROGRAM_NAME ": transfer disjoint subjects: %d subject identities occur in both domains",t->overlap[0]);
   if(!strcmp(t->items,"disjoint") && t->overlap[1])ERROR_exit(PROGRAM_NAME ": transfer disjoint items: %d item identities occur in both domains; use -transfer_items allow for an intentional repeated-item design",t->overlap[1]);
   if(!strcmp(t->sessions,"disjoint") && t->overlap[2])ERROR_exit(PROGRAM_NAME ": transfer disjoint sessions: %d subject/session identities occur in both domains; use -transfer_sessions allow for intentional same-session transfer",t->overlap[2]);
   if(two_tables){
      int col=THD_datatable_column(ss->metadata,MVPA_PARTITION);
      for(i=0;i<ss->nsample;i++)if(strcmp(DT_CELL(ss->metadata,ss->table_row[i],col),plan->sample_test_fold[i]>=0?"test":"train"))
         ERROR_exit(PROGRAM_NAME ": transfer table/domain mismatch at sample %d: training table must contain only train_domain and test table only test_domain",i+1);
   }
   identity=THD_sampleset_build_identity(ss,err,sizeof(err));if(!identity)ERROR_exit(PROGRAM_NAME ": %s",err);
   for(i=0;i<ss->nsample;i++)if(identity->first_sample[i]!=i)
      ERROR_exit(PROGRAM_NAME ": transfer samples '%s' and '%s' select the same dataset/brick; repeated selections are not independent samples",ss->sample_label[identity->first_sample[i]],ss->sample_label[i]);
   THD_sampleset_free_identity(identity);
   if(continuous){
      numeric=mvpa_numeric_columns(ss,targets,&names,&n);
      for(i=0;i<n;i++)if(!strcasecmp(t->column,names[i]))ERROR_exit(PROGRAM_NAME ": transfer domain cannot be a target/design predictor");
      free(numeric);THD_sampleset_free_levels(names,n);
   }
}
static void mvpa_transfer_report(FILE *fp,const mvpa_transfer_options *t,
 const THD_sampleset *ss,const THD_foldplan *plan,const char *targets,char **labels,int nclass)
{
   int f,i,k,role,two_tables=THD_datatable_column(ss->metadata,MVPA_PARTITION)>=0;
   const char *names[]={t->column,t->subject_column,t->item_column,t->session_column,"InputFile"};int cols[5];
   for(i=0;i<5;i++)cols[i]=THD_datatable_column(ss->metadata,(char *)names[i]);
   fputs("# TransferDirection\t",fp);mvpa_1D_text(fp,t->train);mvpa_pair_text(fp,t->test);
   fprintf(fp,"\n# SubjectDesign\t%s\n# ItemOverlapPolicy\t%s\n# SessionOverlapPolicy\t%s\n",t->subjects,t->items,t->sessions);
   fprintf(fp,"# SharedSubjects\t%d\n# SharedItems\t%d\n# SharedSubjectSessions\t%d\n",t->overlap[0],t->overlap[1],t->overlap[2]);
   fputs("# DomainColumn\t",fp);mvpa_1D_text(fp,t->column);
   fputs("\n# SubjectColumn\t",fp);mvpa_1D_text(fp,t->subject_column);
   fputs("\n# ItemColumn\t",fp);mvpa_1D_text(fp,t->item_column);
   fputs("\n# SessionColumn\t",fp);mvpa_1D_text(fp,t->session_column);
   fputs("\n# TargetOrDesignColumns\t",fp);mvpa_1D_text(fp,targets);
   fputs("\n# TargetMapping\texact shared column names and class labels; no automatic recoding\n",fp);
   for(i=0;i<nclass;i++){fprintf(fp,"# ClassLabel\t%d\t",i);mvpa_1D_text(fp,labels[i]);fputc('\n',fp);}
   fputs("# FeatureDomain\tshared mask/ordered features; exact surface node domain or matched volume dimensions/view/space/real affine\n",fp);
   fputs("# Design audit only: identities do not establish semantic equivalence, registration, exchangeability or population inference.\n",fp);
   fputs("# Columns: Fold\tStratum\tRole\tTableRow\tSample\tDomain\tSubject\tItem\tSession\tInputFile",fp);
   if(two_tables)mvpa_source_fields(fp,ss,-1);fputc('\n',fp);
   for(f=0;f<plan->nfold;f++)for(role=0;role<2;role++)for(k=0;k<(role?plan->ntest[f]:plan->ntrain[f]);k++){
      int row=role?plan->test[f][k]:plan->train[f][k];
      mvpa_1D_text(fp,plan->name[f]);mvpa_pair_text(fp,plan->stratum_label[plan->fold_stratum[f]]);
      fprintf(fp,"\t%s\t%d\t",role?"test":"train",ss->table_row[row]+1);mvpa_1D_text(fp,ss->sample_label[row]);
      for(i=0;i<5;i++)mvpa_pair_text(fp,DT_CELL(ss->metadata,ss->table_row[row],cols[i]));
      if(two_tables)mvpa_source_fields(fp,ss,row);fputc('\n',fp);
   }
}
static void mvpa_audit_preflight(const char *kind,const char *path,const char *prefix,const char **other,int n,
 const THD_sampleset *ss,THD_3dim_dataset *mask)
{
   int i;char *name=malloc(strlen(prefix)+32);THD_3dim_dataset *tmp=EDIT_empty_copy(mask);
   const char *suffix[]={".roi.1D",".continuous.1D",".inference.1D",".tuning.1D",".features.1D",".feature_tuning.1D",".pca.1D",".pca_tuning.1D"};
   if(!name || !tmp)ERROR_exit(PROGRAM_NAME ": cannot check audit output paths");
   if(!THD_ok_overwrite() && THD_is_file((char *)path))ERROR_exit(PROGRAM_NAME ": %s '%s' already exists",kind,path);
   for(i=0;i<n;i++)if(other[i] && mvpa_pair_same_file(path,other[i]))ERROR_exit(PROGRAM_NAME ": %s must differ from input and other output paths",kind);
   if(ss->source && mvpa_pair_same_file(path,ss->source))ERROR_exit(PROGRAM_NAME ": %s cannot replace input table",kind);
   for(i=0;i<=ss->ndset;i++){
      THD_3dim_dataset *d=i==ss->ndset?mask:ss->dset[i];
      if(mvpa_pair_same_file(path,DSET_HEADNAME(d)) || mvpa_pair_same_file(path,DSET_BRIKNAME(d)))ERROR_exit(PROGRAM_NAME ": %s cannot replace input images",kind);
   }
   if(mvpa_pair_same_file(path,prefix))ERROR_exit(PROGRAM_NAME ": %s cannot replace score output",kind);
   EDIT_dset_items(tmp,ADN_prefix,prefix,ADN_none);
   if(mvpa_pair_same_file(path,DSET_HEADNAME(tmp)) || mvpa_pair_same_file(path,DSET_BRIKNAME(tmp)))ERROR_exit(PROGRAM_NAME ": %s cannot replace score dataset",kind);
   for(i=0;i<8;i++){
      snprintf(name,strlen(prefix)+32,"%s%s",prefix,suffix[i]);
      if(mvpa_pair_same_file(path,name))ERROR_exit(PROGRAM_NAME ": %s cannot replace automatic outputs",kind);
   }
   free(name);DSET_delete(tmp);
}

static int mvpa_compare_command(int argc,char **argv)
{
   char *prefix="MVPA",*file[2],*out[2],*target[2]={NULL,NULL},**labels=NULL,err[1024];
   THD_datatable *t[2];THD_mvpa_pair_row *rows[2];THD_mvpa_pair_result *r;
   FILE *fp[2];int side,i,j,m,classification,nclass=0,has_prefix=0,tablecol[2],sourcecol[2][3];
   const char *sourcefields[]={"Partition","SourceTable","SourceRow"};
   const char *scopes[]={"Pooled","Fold","Subject","Stratum"};
   if(argc<4)ERROR_exit(PROGRAM_NAME ": -compare_predictions needs model-A and model-B prediction files");
   file[0]=argv[2];file[1]=argv[3];
   for(i=4;i<argc;i++){
      if(!strcasecmp(argv[i],"-prefix") && i+1<argc && !has_prefix){prefix=argv[++i];has_prefix=1;}
      else ERROR_exit(PROGRAM_NAME ": -compare_predictions is standalone; only one optional -prefix is accepted");
   }
   if(!*prefix)ERROR_exit(PROGRAM_NAME ": empty comparison prefix");
   for(side=0;side<2;side++){
      t[side]=THD_read_datatable_file(file[side]);if(!t[side])ERROR_exit(PROGRAM_NAME ": cannot read prediction table %s",file[side]);
      rows[side]=calloc((size_t)t[side]->nrow,sizeof(**rows));if(!rows[side])ERROR_exit(PROGRAM_NAME ": cannot allocate paired rows");
   }
   classification=THD_datatable_column(t[0],"Correct")>=0;
   if(classification!=(THD_datatable_column(t[1],"Correct")>=0))ERROR_exit(PROGRAM_NAME ": cannot compare classification and continuous predictions");
   if(classification){
      for(side=0;side<2;side++)target[side]=mvpa_pair_target(file[side]);
      if(strcmp(target[0],target[1]))ERROR_exit(PROGRAM_NAME ": classification Target headers differ");
   }
   for(side=0;side<2;side++)mvpa_pair_adapt(t[side],classification,rows[side],&labels,&nclass,side==0);
   r=THD_mvpa_compare_predictions(rows[0],t[0]->nrow,rows[1],t[1]->nrow,classification,nclass,err,sizeof(err));
   if(!r)ERROR_exit(PROGRAM_NAME ": %s",err);
   /* Spatial labels/coordinates are identities; table paths and row numbers
      are provenance, and may change when comparing different design tables. */
   {const char *fields[]={"UnitLabel","I","J","K"};
    for(j=0;j<4;j++){
       int ca=THD_datatable_column(t[0],(char *)fields[j]),cb=THD_datatable_column(t[1],(char *)fields[j]);
       if((ca<0)!=(cb<0))ERROR_exit(PROGRAM_NAME ": prediction metadata column %s is present in only one model",fields[j]);
       if(ca>=0)for(i=0;i<r->nrow;i++)if(strcmp(DT_CELL(t[0],r->row_a[i],ca),DT_CELL(t[1],r->row_b[i],cb)))
          ERROR_exit(PROGRAM_NAME ": paired %s mismatch at sample '%s'",fields[j],rows[0][r->row_a[i]].sample);
    }}
   for(side=0;side<2;side++){
      int present=0;tablecol[side]=mvpa_pair_column(t[side],"TableRow");
      for(j=0;j<3;j++){sourcecol[side][j]=THD_datatable_column(t[side],(char *)sourcefields[j]);present+=sourcecol[side][j]>=0;}
      if(present && present!=3)ERROR_exit(PROGRAM_NAME ": incomplete two-table provenance in %s",file[side]);
      if(present)for(i=0;i<t[side]->nrow;i++){
         const char *s=DT_CELL(t[side],i,sourcecol[side][2]);char *end;long n;
         errno=0;n=strtol(s,&end,10);
         if(errno || end==s || *end || n<1 || n>INT_MAX ||
            strcmp(DT_CELL(t[side],i,sourcecol[side][0]),rows[side][i].tested?"test":"train") ||
            !*DT_CELL(t[side],i,sourcecol[side][1]))
            ERROR_exit(PROGRAM_NAME ": invalid two-table provenance in %s row %d",file[side],i+1);
      }
   }
   for(side=0;side<2;side++){
      size_t len=strlen(prefix)+32;out[side]=malloc(len);if(!out[side])ERROR_exit(PROGRAM_NAME ": cannot allocate paired output name");
      snprintf(out[side],len,"%s.%s.1D",prefix,side?"paired_predictions":"comparison");
      for(j=0;j<2;j++)if(mvpa_pair_same_file(out[side],file[j]))ERROR_exit(PROGRAM_NAME ": comparison output cannot replace an input prediction file");
      if(!THD_ok_overwrite() && THD_is_file(out[side]))ERROR_exit(PROGRAM_NAME ": comparison output %s already exists",out[side]);
   }
   if(mvpa_pair_same_file(out[0],out[1]))ERROR_exit(PROGRAM_NAME ": comparison output paths alias each other");
   for(side=0;side<2;side++){
      fp[side]=fopen(out[side],THD_ok_overwrite()?"w":"wx");if(!fp[side])ERROR_exit(PROGRAM_NAME ": cannot write comparison output %s",out[side]);
      fputs("# 3dMVPA descriptive paired predictions; no refitting, p-values, confidence intervals or population inference.\n# ModelA\t",fp[side]);
      mvpa_1D_text(fp[side],file[0]);fputs("\n# ModelB\t",fp[side]);mvpa_1D_text(fp[side],file[1]);fputc('\n',fp[side]);
      if(classification)fprintf(fp[side],"# Target\t%s\n",target[0]);
      fputs("# Matching verifies saved identities and outcomes, not image registration, ROI definitions or original training membership.\n",fp[side]);
   }
   fputs("# Scores are recomputed from saved held-out rows; Pooled is not a mean of folds.\n"
         "# DeltaBminusA=B-A; ImprovementB positive favors B (A-B for MAE/RMSE). Undefined metrics/differences are NA.\n"
         "# Columns: UnitType\tUnit\tResponse\tScope\tGroup\tCVStratum\tNPaired\tMetric\tScoreA\tScoreB\tDeltaBminusA\tImprovementB\n",fp[0]);
   for(i=0;i<r->nscore;i++){
      THD_mvpa_pair_score *s=r->score+i;
      for(m=classification?0:2;m<(classification?2:7);m++){
         double delta=s->b[m]-s->a[m];mvpa_1D_text(fp[0],s->unit_type);mvpa_pair_text(fp[0],s->unit);mvpa_pair_text(fp[0],s->response);
         mvpa_pair_text(fp[0],scopes[s->scope]);mvpa_pair_text(fp[0],s->group);mvpa_pair_text(fp[0],s->stratum);
         fprintf(fp[0],"\t%d\t%s",s->nsample,THD_mvpa_metric_name(m));mvpa_pair_value(fp[0],s->a[m]);mvpa_pair_value(fp[0],s->b[m]);
         mvpa_pair_value(fp[0],delta);mvpa_pair_value(fp[0],m>=THD_METRIC_MAE?-delta:delta);fputc('\n',fp[0]);
      }
   }
   fputs("# AuditRowA/B are one-based data-row indexes in the input audit files. Training-only pairs are retained but never scored.\n"
         "# Columns: UnitType\tUnit\tResponse\tSample\tSubject\tRun\tItem\tInputFold\tCVFold\tCVStratum\tTested\tAuditRowA\tAuditRowB\tTableRowA\tTableRowB\tObserved\tPredictedA\tPredictedB\tTrainingMean\tPartitionA\tSourceTableA\tSourceRowA\tPartitionB\tSourceTableB\tSourceRowB\n",fp[1]);
   for(i=0;i<r->nrow;i++){
      int a=r->row_a[i],b=r->row_b[i];THD_mvpa_pair_row *v=rows[0]+a,*w=rows[1]+b;
      mvpa_1D_text(fp[1],v->unit_type);mvpa_pair_text(fp[1],v->unit);mvpa_pair_text(fp[1],v->response);mvpa_pair_text(fp[1],v->sample);
      mvpa_pair_text(fp[1],v->subject);mvpa_pair_text(fp[1],v->run);mvpa_pair_text(fp[1],v->item);mvpa_pair_text(fp[1],v->input_fold);
      mvpa_pair_text(fp[1],v->fold);mvpa_pair_text(fp[1],v->stratum);fprintf(fp[1],"\t%d\t%d\t%d",v->tested,a+1,b+1);
      mvpa_pair_text(fp[1],DT_CELL(t[0],a,tablecol[0]));mvpa_pair_text(fp[1],DT_CELL(t[1],b,tablecol[1]));
      if(classification){mvpa_pair_text(fp[1],labels[v->truth_class]);mvpa_pair_text(fp[1],v->tested?labels[v->prediction_class]:"NA");mvpa_pair_text(fp[1],v->tested?labels[w->prediction_class]:"NA");fputs("\tNA",fp[1]);}
      else{mvpa_pair_value(fp[1],v->observed);mvpa_pair_value(fp[1],v->tested?v->predicted:NAN);mvpa_pair_value(fp[1],v->tested?w->predicted:NAN);mvpa_pair_value(fp[1],v->tested?v->baseline:NAN);}
      for(side=0;side<2;side++)for(j=0;j<3;j++)mvpa_pair_text(fp[1],sourcecol[side][j]<0?"NA":DT_CELL(t[side],side?b:a,sourcecol[side][j]));
      fputc('\n',fp[1]);
   }
   for(side=0;side<2;side++){
      int failed=ferror(fp[side]);if(fclose(fp[side]))failed=1;if(failed)ERROR_exit(PROGRAM_NAME ": error writing %s",out[side]);
      INFO_message(PROGRAM_NAME ": wrote %s",out[side]);
   }
   THD_mvpa_pair_result_free(r);
   for(side=0;side<2;side++){free(rows[side]);THD_free_datatable(t[side]);free(target[side]);free(out[side]);}free(labels);
   return 0;
}

static THD_mvpa_model *mvpa_model_metadata(THD_3dim_dataset *mset,const THD_roilist *rl,
 const THD_sampleset *ss,const THD_foldplan *plan,const char *target,char **classes,int nc,
 char **covariates,int nk,int searchlight)
{
   THD_mvpa_model *m=calloc(1,sizeof(*m));char **names=NULL;float *values=NULL;int q=nc,i,j;mat44 affine=DSET_CMAT(mset,1);
   if(!m)ERROR_exit(PROGRAM_NAME ": cannot allocate model metadata");
   if(!classes)values=mvpa_numeric_columns((THD_sampleset *)ss,target,&names,&q);
   m->classification=classes!=NULL;m->searchlight=searchlight;m->nunit=rl->nroi;m->nresponse=q;m->ncovariate=nk;m->ntrain=plan->ntrain[0];
   m->dim[0]=DSET_NX(mset);m->dim[1]=DSET_NY(mset);m->dim[2]=DSET_NZ(mset);m->view=mset->view_type;
   for(i=0;i<3;i++)for(j=0;j<4;j++)m->affine[4*i+j]=affine.m[i][j];
   m->space=strdup(THD_get_space(mset)?THD_get_space(mset):"UNKNOWN");m->target=strdup(target);
   m->response=calloc((size_t)q,sizeof(char *));m->covariate=calloc(nk?(size_t)nk:1,sizeof(char *));m->unit=calloc((size_t)m->nunit,sizeof(*m->unit));
   if(!m->space || !m->target || !m->response || !m->covariate || !m->unit)ERROR_exit(PROGRAM_NAME ": cannot allocate model metadata");
   for(i=0;i<q;i++){m->response[i]=strdup(classes?classes[i]:names[i]);if(!m->response[i])ERROR_exit(PROGRAM_NAME ": cannot copy model labels");}
   for(i=0;i<nk;i++){m->covariate[i]=strdup(covariates[i]);if(!m->covariate[i])ERROR_exit(PROGRAM_NAME ": cannot copy model covariates");}
   for(i=0;i<m->nunit;i++){
      THD_mvpa_model_unit *u=m->unit+i;u->id=rl->val[i];u->nfeature=rl->vox[i].nar;
      u->label=strdup(rl->lab[i]?rl->lab[i]:"NA");u->feature=malloc(sizeof(int)*(size_t)u->nfeature);
      if(!u->label || !u->feature)ERROR_exit(PROGRAM_NAME ": cannot copy model feature domain");
      memcpy(u->feature,rl->vox[i].ar,sizeof(int)*(size_t)u->nfeature);
   }
   free(values);THD_sampleset_free_levels(names,classes?0:q);return m;
}
static void mvpa_exact_volume_grid(THD_3dim_dataset *mask,THD_3dim_dataset *d,const char *operation)
{
   mat44 want=DSET_CMAT(mask,1),got=DSET_CMAT(d,1);int j;
   const char *ws=THD_get_space(mask),*gs=THD_get_space(d);
   if(DBLK_IS_NI_SURF_DSET(d->dblk) || DBLK_IS_GIFTI(d->dblk) || DSET_NX(d)!=DSET_NX(mask) || DSET_NY(d)!=DSET_NY(mask) || DSET_NZ(d)!=DSET_NZ(mask) ||
      d->view_type!=mask->view_type || strcmp(ws?ws:"UNKNOWN",gs?gs:"UNKNOWN"))ERROR_exit(PROGRAM_NAME ": %s requires matching volume dimensions/view/space for mask and samples",operation);
   for(j=0;j<12;j++)if(!isfinite(want.m[j/4][j%4]) || !isfinite(got.m[j/4][j%4]) || fabs(want.m[j/4][j%4]-got.m[j/4][j%4])>1e-5)
      ERROR_exit(PROGRAM_NAME ": %s requires matching real affine/feature grids for mask and samples",operation);
}
static void mvpa_model_preflight(const char *path,const char *prefix,const char **audits,int na,
 const THD_sampleset *ss,THD_3dim_dataset *mask)
{
   const char *suffix[]={".roi.1D",".continuous.1D",".inference.1D",".tuning.1D",".features.1D",".feature_tuning.1D",".pca.1D",".pca_tuning.1D"};
   char *name=malloc(strlen(prefix)+32);THD_3dim_dataset *tmp=EDIT_empty_copy(mask);int i;
   if(!name || !tmp)ERROR_exit(PROGRAM_NAME ": cannot check model output paths");
   if(!THD_ok_overwrite() && THD_is_file((char *)path))ERROR_exit(PROGRAM_NAME ": saved model '%s' already exists",path);
   for(i=0;i<na;i++)if(audits[i] && mvpa_pair_same_file(path,audits[i]))ERROR_exit(PROGRAM_NAME ": model and input/audit paths must differ");
   if(ss->source && mvpa_pair_same_file(path,ss->source))ERROR_exit(PROGRAM_NAME ": model cannot replace its input table");
   for(i=0;i<ss->ndset+1;i++){
      THD_3dim_dataset *d=i==ss->ndset?mask:ss->dset[i];
      if(mvpa_pair_same_file(path,DSET_HEADNAME(d)) || mvpa_pair_same_file(path,DSET_BRIKNAME(d)))ERROR_exit(PROGRAM_NAME ": model cannot replace input images");
      if(i<ss->ndset)mvpa_exact_volume_grid(mask,d,"model export");
   }
   EDIT_dset_items(tmp,ADN_prefix,prefix,ADN_none);
   if(mvpa_pair_same_file(path,DSET_HEADNAME(tmp)) || mvpa_pair_same_file(path,DSET_BRIKNAME(tmp)))ERROR_exit(PROGRAM_NAME ": model and score dataset paths must differ");
   for(i=0;i<8;i++){snprintf(name,strlen(prefix)+32,"%s%s",prefix,suffix[i]);if(mvpa_pair_same_file(path,name))ERROR_exit(PROGRAM_NAME ": model and automatic audit paths must differ");}
   free(name);DSET_delete(tmp);
}
/* Fits the same ridge/logistic pipelines as fixed-partition evaluation, without scoring. */
static void mvpa_train_model(THD_sampleset *ss,const THD_foldplan *plan,
 THD_3dim_dataset *mask,THD_roilist *rl,THD_mvpa_model *model,const char *target,
 const int *classes,char **labels,int nc,int scale,double lambda,
 const THD_nested_foldplan *nested,const double *grid,int nl,int objective,
 const float *nuisance,int nk,const int *features,int nf,const int *pcs,int npc,
 int response_lambda,FILE *candidates,const mvpa_selection_audit *selection,
 const mvpa_pca_audit *pca,const char *prefix,int quiet,int progress_mode,
 int logistic,double tolerance,int max_iterations,int metric)
{
   char err[1024],*summary_name;char **names=NULL;int q=nc,r,i,j,pmax=THD_roilist_maxvox(rl);
   float *y,*x;THD_mvpa_ridge_workspace *w=NULL;THD_mvpa_workspace *cw=NULL;FILE *summary;MVPA_progress progress;
   if(classes){
      y=calloc((size_t)ss->nsample*nc,sizeof(float));
      if(y)for(i=0;i<ss->nsample;i++)y[(size_t)i*nc+classes[i]]=1;
      names=labels;
      if(nested)for(i=0;i<nested->inner[0]->nfold;i++){
         const THD_foldplan *in=nested->inner[0];int *counts=calloc((size_t)nc,sizeof(int));
         if(!counts)ERROR_exit(PROGRAM_NAME ": cannot check inner classes");
         for(j=0;j<in->ntrain[i];j++)counts[classes[in->train[i][j]]]++;
         for(j=0;j<nc;j++)if(!counts[j])ERROR_exit(PROGRAM_NAME ": inner fold %s has no class %s in training",in->name[i],labels[j]);
         free(counts);
      }
   }else y=mvpa_numeric_columns(ss,target,&names,&q);
   x=malloc(sizeof(float)*(size_t)ss->nsample*pmax);
   if(logistic){
      cw=THD_mvpa_workspace_alloc(plan,pmax,q,scale,NULL,err,sizeof(err));
      if(!cw || !THD_mvpa_classification_set_logistic(cw,lambda,tolerance,max_iterations,metric,nested,grid,nl,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      if(nuisance && !THD_preprocess_set_nuisance(cw->prep,nuisance,ss->nsample,nk,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      if(nf && !THD_mvpa_classification_set_selection(cw,features,nf,nested,metric,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      if(npc && !THD_mvpa_classification_set_pca(cw,pcs,npc,nested,metric,pca->loadings!=NULL,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      if(nested && !THD_mvpa_classification_set_objective(cw,objective,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
   }else{
      w=THD_mvpa_ridge_workspace_alloc(plan,pmax,q,scale,err,sizeof(err));
      if(!y || !x || !w)ERROR_exit(PROGRAM_NAME ": cannot allocate training workspace");
      w->classification=classes!=NULL;
      if(nuisance && !THD_preprocess_set_nuisance(w->prep,nuisance,ss->nsample,nk,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      if(nested && !THD_mvpa_ridge_set_tuning(w,nested,grid,nl,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      if(nf && !THD_mvpa_ridge_set_selection(w,features,nf,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      if(w->selection)w->selection->classes=classes;
      if(npc && !THD_mvpa_ridge_set_pca(w,pcs,npc,pca->loadings!=NULL,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      if(nested && !THD_mvpa_ridge_set_objective(w,objective,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
      if(response_lambda && !THD_mvpa_ridge_enable_response_lambda(w,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
   }
   if(!y || !x)ERROR_exit(PROGRAM_NAME ": cannot allocate training samples");
   summary_name=malloc(strlen(prefix)+32);if(!summary_name)ERROR_exit(PROGRAM_NAME ": cannot name fit summary");
   sprintf(summary_name,"%s.training.1D",prefix);summary=mvpa_open_audit(summary_name,"training summary");
   fputs("# All supplied samples fitted; no held-out predictions or scores.\n# Inner tuning is model selection, not independent performance evaluation.\n# Columns: Unit\tResponse\tNTrain\tNFeat\tLambda\n",summary);
   mvpa_progress_init(&progress,progress_mode,quiet,"Training",rl->nroi,"locations");
   if(candidates)fputs("# Training-only: inner model selection; no independent performance estimate.\n",candidates);
   for(r=0;r<rl->nroi;r++){
      int p=rl->vox[r].nar;
      if(logistic){
            if(!THD_roi_samples(ss,NULL,ss->nsample,rl->vox+r,x,err,sizeof(err)) ||
               !THD_mvpa_run_classification_cv(plan,0,x,p,classes,NULL,cw,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": training unit %d: %s",rl->val[r],err);
            model->unit[r].fit=THD_mvpa_logistic_freeze(cw,err,sizeof(err));
            if(!model->unit[r].fit)ERROR_exit(PROGRAM_NAME ": %s",err);
            mvpa_candidate_write(candidates,plan,rl->val[r],p,objective,NULL,cw);
            mvpa_pca_write(pca,cw->pca,plan,mask,rl,r,NULL,cw->selection);
            mvpa_candidate_write(pca->tuning,plan,rl->val[r],p,objective,NULL,cw);
            mvpa_selection_write(selection,cw->selection,plan,mask,rl,r,NULL,p,grid,cw->class_selected,lambda);
         }else{
         if(!THD_roi_samples(ss,NULL,ss->nsample,rl->vox+r,x,err,sizeof(err)) ||
            !THD_mvpa_run_ridge_cv(w,x,p,y,q,NULL,lambda,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": training unit %d: %s",rl->val[r],err);
         model->unit[r].fit=THD_mvpa_ridge_freeze(w,err,sizeof(err));
         if(!model->unit[r].fit)ERROR_exit(PROGRAM_NAME ": %s",err);
         mvpa_cont_candidates(candidates,w,mask,rl,r,p,q,names,0,objective);
         mvpa_pca_write(pca,w->pca,plan,mask,rl,r,NULL,w->selection);
         mvpa_cont_candidates(pca->tuning,w,mask,rl,r,p,q,names,0,objective);
         {mvpa_selection_audit audit=*selection;if(response_lambda)audit.tuning=NULL;
          mvpa_selection_write(&audit,w->selection,plan,mask,rl,r,NULL,p,grid,w->selected_lambda,lambda);}
         if(response_lambda)mvpa_response_candidates(selection->tuning,w,mask,rl,r,p,q,names,0,objective);
      }
      for(i=0;i<q;i++){
         fprintf(summary,"%d\t",rl->val[r]);mvpa_1D_text(summary,names[i]);
         fprintf(summary,"\t%d\t%d\t%.17g\n",ss->nsample,p,logistic?cw->class_selected[0]:response_lambda?w->response_ridge->selected[i]:w->selected_lambda[0]);
      }
      mvpa_progress_advance(&progress);
   }
   {int bad=ferror(summary);if(fclose(summary))bad=1;if(bad)ERROR_exit(PROGRAM_NAME ": error writing fit summary");}
   mvpa_progress_finish(&progress);
   free(summary_name);free(x);free(y);if(!classes)THD_sampleset_free_levels(names,q);THD_mvpa_ridge_workspace_free(w);THD_mvpa_workspace_free(cw);
}

/* Expand selected original bricks into the ordinary application sample table. */
static THD_datatable *mvpa_application_dataset(const char *spec)
{
   const char *sel=strchr(spec,'['),*end;char *base,*tokens[3],**tok;
   THD_3dim_dataset *ds;THD_datatable *table;int *list=NULL,n,i,k;
   size_t len=sel?(size_t)(sel-spec):strlen(spec);
   if(!len || strchr(spec,'<') || strchr(spec,'{'))ERROR_exit(PROGRAM_NAME ": -apply_dset accepts a dataset with an optional sub-brick selector");
   if(sel){end=strrchr(sel,']');if(!end || end[1])ERROR_exit(PROGRAM_NAME ": malformed -apply_dset selector");}
   base=malloc(len+1);if(!base)ERROR_exit(PROGRAM_NAME ": cannot allocate dataset name");
   memcpy(base,spec,len);base[len]=0;
   ds=THD_open_dataset(base);if(!ds)ERROR_exit(PROGRAM_NAME ": cannot open application dataset '%s'",base);
   if(DBLK_IS_NI_SURF_DSET(ds->dblk) || DBLK_IS_GIFTI(ds->dblk))ERROR_exit(PROGRAM_NAME ": saved model v1 requires volume inputs; surface model export is not supported");
   if(sel){list=MCW_get_thd_intlist(ds,(char *)sel);if(!list || list[0]<1)ERROR_exit(PROGRAM_NAME ": invalid or empty -apply_dset selector");}
   n=list?list[0]:DSET_NVALS(ds);
   if(n<1 || n>(INT_MAX-3)/3)ERROR_exit(PROGRAM_NAME ": excessive application sample count");
   tok=calloc((size_t)(n+1)*3,sizeof(char *));if(!tok)ERROR_exit(PROGRAM_NAME ": cannot allocate application table");
   tokens[0]="Subj";tokens[1]="Sample";tokens[2]="InputFile";
   for(k=0;k<3;k++)tok[k]=strdup(tokens[k]);
   for(i=0;i<n;i++){
      char id[80];int b=list?list[i+1]:i;
      if(b<0 || b>=DSET_NVALS(ds))ERROR_exit(PROGRAM_NAME ": application sub-brick out of range");
      snprintf(id,sizeof(id),"sample_%d_brick_%d",i,b);
      tok[3+3*i]=strdup("Dataset");tok[4+3*i]=strdup(id);
      tok[5+3*i]=malloc(len+40);if(!tok[5+3*i])ERROR_exit(PROGRAM_NAME ": cannot allocate brick selector");
      snprintf(tok[5+3*i],len+40,"%s[%d]",base,b);
   }
   for(k=0;k<3*(n+1);k++)if(!tok[k])ERROR_exit(PROGRAM_NAME ": cannot allocate application tokens");
   table=THD_parse_datatable(tok,3*(n+1),(char *)spec);
   for(k=0;k<3*(n+1);k++)free(tok[k]);
   free(tok);free(list);free(base);DSET_delete(ds);
   if(!table)ERROR_exit(PROGRAM_NAME ": cannot expand application dataset");
   return table;
}

static int mvpa_apply_model_command(int argc,char **argv)
{
   char *dsetspec=NULL,*tablefile=NULL,*prefix="MVPA",*sample=NULL,*subject=NULL,*outname,*temp;int a,i,j,o,u,fd,failed=0,have_prefix=0;
   THD_mvpa_model *m;THD_datatable *table;THD_sampleset *ss;THD_sampleset_columns columns;
   char err[1024];FILE *fp;float *z=NULL,*prediction;int *observed;const char *modelpath;
   if(argc<3)ERROR_exit(PROGRAM_NAME ": -apply_model requires a model file");
   modelpath=argv[2];
   for(a=3;a<argc;a++){
      if(a+1>=argc)ERROR_exit(PROGRAM_NAME ": missing saved-model application option argument");
      if(!strcasecmp(argv[a],"-dataTableFile") && !tablefile)tablefile=argv[++a];
      else if(!strcasecmp(argv[a],"-dataTable") && !tablefile && argv[a+1][0]=='@')tablefile=argv[++a]+1;
      else if(!strcasecmp(argv[a],"-apply_dset") && !dsetspec)dsetspec=argv[++a];
      else if(!strcasecmp(argv[a],"-prefix") && !have_prefix){prefix=argv[++a];have_prefix=1;}
      else if(!strcasecmp(argv[a],"-sample_column") && !sample)sample=argv[++a];
      else if(!strcasecmp(argv[a],"-subject_column") && !subject)subject=argv[++a];
      else ERROR_exit(PROGRAM_NAME ": -apply_model is standalone; accepts one input table or -apply_dset, -prefix, -sample_column and -subject_column; fitted settings cannot be overridden");
   }
   if((!tablefile && !dsetspec) || (tablefile && dsetspec) || !*prefix)ERROR_exit(PROGRAM_NAME ": -apply_model needs exactly one table or -apply_dset and a nonempty prefix");
   if(dsetspec && (sample || subject))ERROR_exit(PROGRAM_NAME ": -apply_dset generates sample/subject IDs; role overrides require a table");
   m=THD_mvpa_model_read(modelpath,err,sizeof(err));if(!m)ERROR_exit(PROGRAM_NAME ": %s",err);
   if(dsetspec && m->ncovariate)ERROR_exit(PROGRAM_NAME ": saved nuisance columns require application table input, not -apply_dset");
   table=dsetspec?mvpa_application_dataset(dsetspec):THD_read_datatable_file(tablefile);if(!table)ERROR_exit(PROGRAM_NAME ": cannot read application table");
   for(i=0;i<table->ncol;i++)for(j=0;j<i;j++)if(!strcasecmp(table->cname[i],table->cname[j]))ERROR_exit(PROGRAM_NAME ": duplicate application column '%s'",table->cname[i]);
   THD_sampleset_detect_columns(table,&columns);if(sample)columns.sample=sample;if(subject)columns.subject=subject;
   ss=THD_sampleset_from_table(table,&columns,err,sizeof(err));THD_free_datatable(table);
   if(!ss)ERROR_exit(PROGRAM_NAME ": %s",err);
   for(i=0;i<ss->ndset;i++){
      THD_3dim_dataset *ds=ss->dset[i];mat44 affine=DSET_CMAT(ds,1);const char *space=THD_get_space(ds);
      if(DBLK_IS_NI_SURF_DSET(ds->dblk) || DBLK_IS_GIFTI(ds->dblk))ERROR_exit(PROGRAM_NAME ": saved model v1 requires volume inputs");
      if(DSET_NX(ds)!=m->dim[0] || DSET_NY(ds)!=m->dim[1] || DSET_NZ(ds)!=m->dim[2] || ds->view_type!=m->view || strcmp(space?space:"UNKNOWN",m->space))
         ERROR_exit(PROGRAM_NAME ": saved-model image dimensions/view/space do not match input '%s'",ss->dset_name[i]);
      for(j=0;j<12;j++)if(!isfinite(affine.m[j/4][j%4]) || fabs(affine.m[j/4][j%4]-m->affine[j])>1e-5)
         ERROR_exit(PROGRAM_NAME ": saved-model affine/feature grid does not match input '%s'",ss->dset_name[i]);
      DSET_load(ds);if(!DSET_LOADED(ds))ERROR_exit(PROGRAM_NAME ": cannot load model application input");
   }
   if(m->ncovariate){
      z=THD_datatable_numeric_columns(ss->metadata,(const char * const *)m->covariate,m->ncovariate,NULL,ss->nsample,err,sizeof(err));
      if(!z)ERROR_exit(PROGRAM_NAME ": saved nuisance columns: %s",err);
   }
   observed=malloc(sizeof(int)*(size_t)m->nresponse);if(!observed)ERROR_exit(PROGRAM_NAME ": cannot allocate response metadata");
   for(o=0;o<m->nresponse;o++)observed[o]=THD_datatable_column(ss->metadata,m->classification?m->target:m->response[o]);
   for(o=0;o<(m->classification?1:m->nresponse);o++)if(observed[o]>=0)for(i=0;i<ss->nsample;i++){
      const char *value=DT_CELL(ss->metadata,ss->table_row[i],observed[o]);
      if(m->classification){for(j=0;j<m->nresponse;j++)if(!strcmp(value,m->response[j]))break;if(j==m->nresponse)ERROR_exit(PROGRAM_NAME ": unknown observed class label '%s' for saved model",value);}
      else (void)mvpa_pair_number(value,tablefile,i,m->response[o]);
   }
   outname=malloc(strlen(prefix)+32);temp=malloc(strlen(prefix)+48);
   if(!outname || !temp || (size_t)ss->nsample>SIZE_MAX/sizeof(float)/(size_t)m->nresponse)ERROR_exit(PROGRAM_NAME ": excessive model application size");
   snprintf(outname,strlen(prefix)+32,"%s.applied.1D",prefix);
   if(mvpa_pair_same_file(outname,modelpath) || (tablefile && mvpa_pair_same_file(outname,tablefile)))ERROR_exit(PROGRAM_NAME ": model application output cannot replace inputs");
   for(i=0;i<ss->ndset;i++)if(mvpa_pair_same_file(outname,DSET_HEADNAME(ss->dset[i])) || mvpa_pair_same_file(outname,DSET_BRIKNAME(ss->dset[i])))ERROR_exit(PROGRAM_NAME ": model application output cannot replace input images");
   if(!THD_ok_overwrite() && THD_is_file(outname))ERROR_exit(PROGRAM_NAME ": model application output already exists");
   snprintf(temp,strlen(prefix)+48,"%s.tmp.XXXXXX",outname);fd=mkstemp(temp);if(fd<0)ERROR_exit(PROGRAM_NAME ": cannot create application output");
   fp=fdopen(fd,"w");if(!fp){close(fd);unlink(temp);ERROR_exit(PROGRAM_NAME ": cannot open application output");}
   prediction=malloc(sizeof(float)*(size_t)ss->nsample*m->nresponse);if(!prediction){fclose(fp);unlink(temp);ERROR_exit(PROGRAM_NAME ": cannot allocate predictions");}
   fprintf(fp,"# Frozen %s model application; format=%d; no fitting, tuning, held-out scores or inference.\n# Model\t",m->estimator?"logistic":"ridge",m->estimator?2:1);mvpa_1D_text(fp,modelpath);
   fprintf(fp,"\n# NTrain=%d; %s; ties choose first stored class.\n",m->ntrain,m->estimator?"Prediction is a logistic logit; Probability is softmax over classes":"classification scores are 0/1 ridge outputs, not probabilities");
   fputs("# Columns: UnitType\tUnit\tUnitLabel\tNFeat\tTableRow\tSample\tSubject\tResponse\tPrediction\tPredictedClass\tObserved",fp);
   if(m->estimator)fputs("\tProbability",fp);
   if(dsetspec)fputs("\tSourceBrick\tBrickLabel\tTimeSeconds",fp);
   fputc('\n',fp);
   for(u=0;u<m->nunit;u++){
      THD_mvpa_model_unit *unit=m->unit+u;intvec vox={0};float *x;
      if((size_t)ss->nsample>SIZE_MAX/sizeof(float)/(size_t)unit->nfeature){snprintf(err,sizeof(err),"excessive application feature matrix");failed=1;break;}
      x=malloc(sizeof(float)*(size_t)ss->nsample*unit->nfeature);vox.nar=unit->nfeature;vox.ar=unit->feature;
      if(!x){snprintf(err,sizeof(err),"cannot allocate application features");failed=1;break;}
      if(!THD_roi_samples(ss,NULL,ss->nsample,&vox,x,err,sizeof(err)) || !THD_mvpa_frozen_predict(unit->fit,x,ss->nsample,unit->nfeature,z,m->ncovariate,prediction,err,sizeof(err))){free(x);failed=1;break;}
      free(x);
      for(i=0;i<ss->nsample;i++){
         double denom=0;int best=0;for(o=1;o<m->nresponse;o++)if(prediction[(size_t)i*m->nresponse+o]>prediction[(size_t)i*m->nresponse+best])best=o;
         if(m->estimator)for(o=0;o<m->nresponse;o++)denom+=exp((double)prediction[(size_t)i*m->nresponse+o]-prediction[(size_t)i*m->nresponse+best]);
         for(o=0;o<m->nresponse;o++){
            fprintf(fp,"%s\t%d\t",m->searchlight?"Voxel":"ROI",unit->id);mvpa_1D_text(fp,unit->label);fprintf(fp,"\t%d\t%d\t",unit->nfeature,ss->table_row[i]+1);
            mvpa_1D_text(fp,ss->sample_label[i]);mvpa_pair_text(fp,mvpa_level(ss->subject_label,ss->subject[i]));mvpa_pair_text(fp,m->response[o]);
            fprintf(fp,"\t%.9g\t",prediction[(size_t)i*m->nresponse+o]);mvpa_1D_text(fp,m->classification?m->response[best]:"NA");
            mvpa_pair_text(fp,observed[o]>=0?DT_CELL(ss->metadata,ss->table_row[i],observed[o]):"NA");
            if(m->estimator)fprintf(fp,"\t%.9g",exp((double)prediction[(size_t)i*m->nresponse+o]-prediction[(size_t)i*m->nresponse+best])/denom);
            if(dsetspec){
               int b=ss->sample_brick[i];THD_3dim_dataset *ds=ss->dset[ss->sample_dset[i]];
               double factor=DSET_TIMEUNITS(ds)==UNITS_SEC_TYPE?1:DSET_TIMEUNITS(ds)==UNITS_MSEC_TYPE?.001:0;
               fprintf(fp,"\t%d",b);mvpa_pair_text(fp,DSET_BRICK_LABEL(ds,b));
               if(DSET_NUM_TIMES(ds)>1 && factor && DSET_TR(ds)>0)fprintf(fp,"\t%.9g",factor*(DSET_TIMEORIGIN(ds)+b*DSET_TR(ds)));
               else fputs("\tNA",fp);
            }
            fputc('\n',fp);
         }
      }
   }
   if(ferror(fp)){snprintf(err,sizeof(err),"application write failure");failed=1;}
   if(fclose(fp)){snprintf(err,sizeof(err),"application close failure");failed=1;}
   if(!failed && (THD_ok_overwrite()?rename(temp,outname):link(temp,outname))){snprintf(err,sizeof(err),"application publication failed");failed=1;}
   unlink(temp);if(failed)ERROR_exit(PROGRAM_NAME ": %s",err);
   INFO_message(PROGRAM_NAME ": applied frozen model without fitting; wrote %s",outname);
   free(prediction);free(observed);free(temp);free(outname);free(z);THD_sampleset_free(ss);THD_mvpa_model_free(m);return 0;
}

int main( int argc, char **argv )
{
   THD_datatable *tab=NULL,*test_tab=NULL,*expanded=NULL ; THD_sampleset *ss=NULL ;
   THD_sampleset_columns columns ; THD_foldplan *plan=NULL ;
   THD_nested_foldplan *nested=NULL;
   THD_3dim_dataset *mset=NULL,*oset=NULL ; THD_roilist *rl=NULL ;
   THD_mvpa_workspace **workspace=NULL ;
   PERM_scheme *perm_scheme=NULL ; PERM_set *perm_set=NULL ;
   PERM_result *perm_result=NULL ;
   char *mode=NULL,*target_column=NULL,*cv_column=NULL,*within_column=NULL ;
   char *partition_column=NULL,*train_value=NULL,*test_value=NULL;
   mvpa_transfer_options transfer={0};
   mvpa_reg_options reg={0};
   char *subject_column=NULL,*run_column=NULL,*item_column=NULL ;
   char *fold_column=NULL,*sample_column=NULL,*xmat_stims=NULL ;
   int trial_manifest=0,validate_only=0,two_tables=0 ;
   char *maskname=NULL,*roi_sel=NULL,*searchlight=NULL,*surf_file=NULL ;
   char *prefix="MVPA",*method=NULL,*design_columns=NULL,*score_mode=NULL ;
   char *tune_metric_name=NULL,*save_tuning=NULL;FILE *candidate_audit=NULL;
   int tune_requested=THD_TUNE_DEFAULT,tune_objective=THD_TUNE_DEFAULT,lambda_per_response=0;
   char *feature_text=NULL;int feature_fixed=0,*feature_counts=NULL,nfeature_counts=0;
   char *pca_text=NULL,*save_pca_loadings=NULL;int pca_fixed=0,*pca_counts=NULL,npca_counts=0;
   mvpa_pca_audit pca_audit={0};
   int train_only=0;
   char *pattern_prefix=NULL,*save_model=NULL;mvpa_pattern_audit pattern_audit={0};
   THD_mvpa_model *frozen_model=NULL;char *model_train_source=NULL,*model_test_source=NULL;
   mvpa_selection_audit selection_audit={0};
   char *nuisance_columns=NULL;char **nuisance_names=NULL;float *nuisance=NULL;int nnuisance=0;
   char *coef_prefix=NULL;double coef_lambda=0;int coef_lambda_given=0;
   int logistic_class=0,logistic_tune=0,logistic_numeric=0,logistic_options=0,logistic_ngrid=0,logistic_max_iter=1000;
   double logistic_cost=1,logistic_tolerance=1e-6,*logistic_grid=NULL;char *logistic_grid_text=NULL;
   char *save_probability_scores=NULL,*save_calibration=NULL;FILE *probability_fp=NULL,*calibration_fp=NULL;
   int svm_class=0,svm_tune=0,svm_numeric=0,svm_options=0,svm_ngrid=0,svm_max_iter=10000;
   double svm_cost=1,svm_tolerance=1e-6,*svm_grid=NULL;char *svm_grid_text=NULL;
   double lambda=1.0;int continuous=0,encoding=0,voxel_encoding=0,ridge_class=0,distance_given=0;
   int lda_mode=-1,shrinkage_given=0,floor_given=0,empirical_prior=0;
   double shrinkage=.1,variance_floor=1e-6;char *class_prior=NULL;
   char *grid_text=NULL,*inner_column=NULL;int inner_k=0,tune=0,numeric_lambda=0,nlambda=0;
   double *lambda_grid=NULL;
   char *save_predictions=NULL,*save_fold_scores=NULL ;
   char *save_confusion=NULL,*save_summary_scores=NULL;
   FILE *confusion_fp=NULL,*summary_fp=NULL;
   mvpa_class_audit class_audit={0};
   char *permute_within=NULL ; long seed=1234567L ;
   char *distance_name="euclidean",*scale_name="feature_zscore" ;
   char *metric_name=NULL,err[1024],*table_name=NULL ;
   char **class_label=NULL,**perm_block_label=NULL ;
   int *class_id=NULL,*group_id=NULL,*perm_block=NULL,**perm_class=NULL ;
   float **x=NULL,**perm_score=NULL,**perm_max=NULL ;
   float *score=NULL,*acc=NULL,*bacc=NULL,*nfeat=NULL,*nfold=NULL,*qval=NULL ;
   int nclass=0,distance=THD_CENTROID_EUCLIDEAN,preprocess=THD_PREP_FEATURE_ZSCORE ;
   int metric=THD_METRIC_BALANCED_ACCURACY,cv_k=0,nperm=0,nperm_block=0 ;
   int show_table=0,show_samples=0,show_folds=0,quiet=0,nopt=1,ii,rr,maxvox ;
   int progress_mode=MVPA_PROGRESS_AUTO; MVPA_progress progress;
   int nworker=1,failed=0,io_failed=0 ; char runerr[1024]="" ;
   FILE *prediction_fp=NULL,*fold_fp=NULL,*class_tuning=NULL ;char *class_tunefile=NULL;

   if( argc<2 ){ usage_3dMVPA() ; return 0 ; }
   mainENTRY(PROGRAM_NAME " main") ; machdep() ; AFNI_logger(PROGRAM_NAME,argc,argv) ;
   PRINT_VERSION(PROGRAM_NAME) ; AUTHOR("P Molfese") ; THD_check_AFNI_version(PROGRAM_NAME) ;

   if(!strcasecmp(argv[1],"-apply_model"))return mvpa_apply_model_command(argc,argv);
   if(!strcasecmp(argv[1],"-compare_predictions"))return mvpa_compare_command(argc,argv);

   while( nopt<argc && argv[nopt][0]=='-' ){
#define IS(s) (strcasecmp(argv[nopt],(s))==0)
#define NEEDARG(what) do{ if(++nopt>=argc)ERROR_exit(PROGRAM_NAME ": need %s after '%s'",(what),argv[nopt-1]); }while(0)
     if( IS("-help") || IS("-h") ){ usage_3dMVPA() ; return 0 ; }
     else if( IS("-mode") ){ NEEDARG("classify, decode, or encode") ; mode=argv[nopt++] ; }
     else if(mvpa_reg_option(&reg,argc,argv,&nopt)){}
     else if( IS("-target_column") ){ NEEDARG("a column name") ; target_column=argv[nopt++] ; }
     else if( IS("-target_columns") ){ NEEDARG("numeric column names") ; target_column=argv[nopt++] ; }
     else if( IS("-design_columns") ){ NEEDARG("numeric column names") ; design_columns=argv[nopt++] ; }
     else if( IS("-score") ){ NEEDARG("voxel or pattern") ; score_mode=argv[nopt++] ; }
     else if( IS("-lambda") ){
       char *end;NEEDARG("a positive penalty or auto");
       if(tune || numeric_lambda)ERROR_exit(PROGRAM_NAME ": -lambda was specified twice");
       if(!strcasecmp(argv[nopt],"auto"))tune=1;
       else{
         errno=0;lambda=strtod(argv[nopt],&end);numeric_lambda=1;
         if(errno || end==argv[nopt] || *end || !isfinite(lambda) || lambda<=0)
           ERROR_exit(PROGRAM_NAME ": -lambda must be finite and positive, or auto");
       }
       nopt++;
     }
     else if(IS("-svm_c")){
       char *end;NEEDARG("a positive C or auto");svm_options=1;
       if(svm_tune || svm_numeric)ERROR_exit(PROGRAM_NAME ": -svm_c was specified twice");
       if(!strcasecmp(argv[nopt],"auto"))svm_tune=1;
       else{
         errno=0;svm_cost=strtod(argv[nopt],&end);svm_numeric=1;
         if(errno || end==argv[nopt] || *end || !isfinite(svm_cost) || svm_cost<=0 || !isfinite(.5/svm_cost) || .5/svm_cost<=0)
           ERROR_exit(PROGRAM_NAME ": -svm_c must be finite and positive, or auto");
       }
       nopt++;
     }
     else if(IS("-svm_c_grid")){
       NEEDARG("comma-separated positive C values");svm_options=1;
       if(svm_grid_text)ERROR_exit(PROGRAM_NAME ": -svm_c_grid was specified twice");
       svm_grid_text=argv[nopt++];
     }
     else if(IS("-svm_tol")){
       char *end;NEEDARG("a tolerance in [1e-12,1e-2]");svm_options=1;
       errno=0;svm_tolerance=strtod(argv[nopt],&end);
       if(errno || end==argv[nopt] || *end || !isfinite(svm_tolerance) || svm_tolerance<1e-12 || svm_tolerance>1e-2)
         ERROR_exit(PROGRAM_NAME ": -svm_tol must be in [1e-12,1e-2]");
       nopt++;
     }
     else if(IS("-svm_max_iter")){
       NEEDARG("a positive sweep limit");svm_options=1;
       svm_max_iter=(int)mvpa_parse_long("-svm_max_iter",argv[nopt++],1,INT_MAX);
     }
     else if(IS("-logistic_lambda")){
       char *end;NEEDARG("a positive lambda or auto");logistic_options=1;
       if(logistic_tune || logistic_numeric)ERROR_exit(PROGRAM_NAME ": -logistic_c was specified twice");
       if(!strcasecmp(argv[nopt],"auto"))logistic_tune=1;
       else{
         errno=0;logistic_cost=strtod(argv[nopt],&end);logistic_numeric=1;
         if(errno || end==argv[nopt] || *end || !isfinite(logistic_cost) || logistic_cost<=0 || !isfinite(.5/logistic_cost) || .5/logistic_cost<=0)
           ERROR_exit(PROGRAM_NAME ": -logistic_lambda must be finite and positive, or auto");
       }
       nopt++;
     }
     else if(IS("-logistic_lambda_grid")){
       NEEDARG("comma-separated positive lambda values");logistic_options=1;
       if(logistic_grid_text)ERROR_exit(PROGRAM_NAME ": -logistic_lambda_grid was specified twice");
       logistic_grid_text=argv[nopt++];
     }
     else if(IS("-logistic_tol")){
       char *end;NEEDARG("a tolerance in [1e-12,1e-2]");logistic_options=1;
       errno=0;logistic_tolerance=strtod(argv[nopt],&end);
       if(errno || end==argv[nopt] || *end || !isfinite(logistic_tolerance) || logistic_tolerance<1e-12 || logistic_tolerance>1e-2)
         ERROR_exit(PROGRAM_NAME ": -logistic_tol must be in [1e-12,1e-2]");
       nopt++;
     }
     else if(IS("-logistic_max_iter")){
       NEEDARG("a positive iteration limit");logistic_options=1;
       logistic_max_iter=(int)mvpa_parse_long("-logistic_max_iter",argv[nopt++],1,INT_MAX);
     }
     else if(IS("-lambda_per_response")){
       if(lambda_per_response)ERROR_exit(PROGRAM_NAME ": -lambda_per_response specified twice");
       lambda_per_response=1;nopt++;
     }
     else if( IS("-lambda_grid") ){
       NEEDARG("comma-separated positive penalties");
       if(grid_text)ERROR_exit(PROGRAM_NAME ": -lambda_grid was specified twice");
       grid_text=argv[nopt++];
     }
     else if( IS("-inner_cv_column") ){NEEDARG("an inner grouping column");inner_column=argv[nopt++];}
     else if( IS("-inner_cv_k") ){NEEDARG("K >= 2");inner_k=(int)mvpa_parse_long("-inner_cv_k",argv[nopt++],2,INT_MAX);}
     else if(IS("-transfer_column") || IS("-train_domain") || IS("-test_domain") ||
             IS("-transfer_subjects") || IS("-transfer_items") || IS("-transfer_sessions") || IS("-session_column")){
       char **dest=IS("-transfer_column")?&transfer.column:IS("-train_domain")?&transfer.train:
          IS("-test_domain")?&transfer.test:IS("-transfer_subjects")?&transfer.subjects:
          IS("-transfer_items")?&transfer.items:IS("-transfer_sessions")?&transfer.sessions:&transfer.session_column;
       if(*dest)ERROR_exit(PROGRAM_NAME ": %s specified twice",argv[nopt]);
       NEEDARG("a nonempty transfer column, domain or policy");
       if(!argv[nopt][0])ERROR_exit(PROGRAM_NAME ": transfer arguments must be nonempty");
       *dest=argv[nopt++];
     }
     else if(IS("-partition_column") || IS("-train_value") || IS("-test_value")){
       char **dest=IS("-partition_column")?&partition_column:IS("-train_value")?&train_value:&test_value;
       if(*dest)ERROR_exit(PROGRAM_NAME ": %s was specified twice",argv[nopt]);
       NEEDARG("a nonempty column name or partition value");
       if(!argv[nopt][0])ERROR_exit(PROGRAM_NAME ": partition column and values must be nonempty");
       *dest=argv[nopt++];
     }
     else if( IS("-cv_column") ){ NEEDARG("a column name") ; cv_column=argv[nopt++] ; }
     else if( IS("-cv") ){
       NEEDARG("run, subject, or item") ;
            if( strcasecmp(argv[nopt],"run")==0 ) cv_column="Run" ;
       else if( strcasecmp(argv[nopt],"subject")==0 ) cv_column="Subj" ;
       else if( strcasecmp(argv[nopt],"item")==0 ) cv_column="Item" ;
       else ERROR_exit(PROGRAM_NAME ": -cv must be run, subject, or item") ;
       nopt++ ;
     }
     else if( IS("-cv_k") ){ NEEDARG("K >= 2") ; cv_k=(int)mvpa_parse_long("-cv_k",argv[nopt++],2,INT_MAX) ; }
     else if( IS("-within_column") ){ NEEDARG("a column name") ; within_column=argv[nopt++] ; }
     else if( IS("-subject_column") ){ NEEDARG("a column name") ; subject_column=argv[nopt++] ; }
     else if( IS("-run_column") ){ NEEDARG("a column name") ; run_column=argv[nopt++] ; }
     else if( IS("-item_column") ){ NEEDARG("a column name") ; item_column=argv[nopt++] ; }
     else if( IS("-fold_column") ){ NEEDARG("a column name") ; fold_column=argv[nopt++] ; }
     else if( IS("-sample_column") ){ NEEDARG("a column name") ; sample_column=argv[nopt++] ; }
     else if( IS("-xmat_stims") ){ NEEDARG("a comma-separated stimulus list") ; xmat_stims=argv[nopt++] ; }
     else if( IS("-mask") ){ NEEDARG("a dataset") ; maskname=argv[nopt++] ; }
     else if( IS("-roi_sel") ){ NEEDARG("an AFNI integer list") ; roi_sel=argv[nopt++] ; }
     else if( IS("-searchlight") ){ NEEDARG("a radius or neighborhood") ; searchlight=argv[nopt++] ; }
     else if( IS("-surf") ){ NEEDARG("a surface mesh") ; surf_file=argv[nopt++] ; }
     else if(IS("-shrinkage") || IS("-variance_floor")){
       int isshrink=IS("-shrinkage");char *end;double value;
       if(isshrink?shrinkage_given:floor_given)ERROR_exit(PROGRAM_NAME ": repeated LDA regularization option");
       NEEDARG("a finite regularization value");errno=0;value=strtod(argv[nopt],&end);
       if(errno || end==argv[nopt] || *end || !isfinite(value) ||
          (isshrink?(value<=0 || value>1):(value<1e-12 || value>1)))
         ERROR_exit(PROGRAM_NAME ": %s must be in %s",isshrink?"-shrinkage":"-variance_floor",isshrink?"(0,1]":"[1e-12,1]");
       nopt++;
       if(isshrink){shrinkage=value;shrinkage_given=1;}else{variance_floor=value;floor_given=1;}
     }
     else if(IS("-class_prior")){
       if(class_prior)ERROR_exit(PROGRAM_NAME ": -class_prior was specified twice");
       NEEDARG("equal or empirical");class_prior=argv[nopt++];
       if(strcasecmp(class_prior,"equal") && strcasecmp(class_prior,"empirical"))
         ERROR_exit(PROGRAM_NAME ": -class_prior must be equal or empirical");
       empirical_prior=!strcasecmp(class_prior,"empirical");
     }
     else if( IS("-method") ){ NEEDARG("an estimator name (see -help)") ; method=argv[nopt++] ; }
     else if( IS("-distance") ){ NEEDARG("euclidean, correlation, or cosine") ; distance_name=argv[nopt++] ;distance_given=1; }
     else if(IS("-tune_metric")){
       NEEDARG("an inner-CV objective");
       if(tune_metric_name)ERROR_exit(PROGRAM_NAME ": -tune_metric was specified twice");
       tune_metric_name=argv[nopt++];
       if(!THD_mvpa_tune_objective_parse(tune_metric_name,&tune_requested,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
     }
     else if(IS("-save_tuning")){
       NEEDARG("a candidate audit filename");
       if(save_tuning)ERROR_exit(PROGRAM_NAME ": -save_tuning was specified twice");
       save_tuning=argv[nopt++];
     }
     else if(IS("-pca_components") || IS("-pca_components_grid")){
       int fixed=IS("-pca_components");NEEDARG("positive PCA component count(s)");
       if(pca_text)ERROR_exit(PROGRAM_NAME ": choose only one -pca_components or -pca_components_grid");
       pca_text=argv[nopt++];pca_fixed=fixed;
     }
     else if(IS("-save_pca_loadings")){
       NEEDARG("PCA loading audit filename");
       if(save_pca_loadings)ERROR_exit(PROGRAM_NAME ": -save_pca_loadings specified twice");
       save_pca_loadings=argv[nopt++];
     }
     else if(IS("-select_features") || IS("-select_features_grid")){
       int fixed=IS("-select_features");NEEDARG("positive feature count(s)");
       if(feature_text)ERROR_exit(PROGRAM_NAME ": choose only one -select_features or -select_features_grid");
       feature_text=argv[nopt++];feature_fixed=fixed;
     }
     else if(IS("-nuisance_columns")){
       NEEDARG("comma-separated numeric nuisance columns");
       if(nuisance_columns)ERROR_exit(PROGRAM_NAME ": -nuisance_columns was specified twice");
       nuisance_columns=argv[nopt++];
     }
     else if( IS("-scale") ){ NEEDARG("none, feature_zscore, sample_center, or run_center") ; scale_name=argv[nopt++] ; }
     else if( IS("-metric") ){ NEEDARG("a prediction metric") ; metric_name=argv[nopt++] ; }
     else if( IS("-nperm") ){ NEEDARG("a nonnegative count") ; nperm=(int)mvpa_parse_long("-nperm",argv[nopt++],0,INT_MAX) ; }
     else if( IS("-seed") ){ NEEDARG("an integer seed") ; seed=mvpa_parse_long("-seed",argv[nopt++],LONG_MIN,LONG_MAX) ; }
     else if( IS("-permute_within") || IS("-block_column") ){
       NEEDARG("an exchangeability-block column or none") ;
       if( permute_within!=NULL ) ERROR_exit(PROGRAM_NAME ": the permutation block was given twice") ;
       permute_within=argv[nopt++] ;
     }
     else if( IS("-prefix") ){ NEEDARG("an output prefix") ; prefix=argv[nopt++] ; }
     else if(IS("-train_only")){if(train_only)ERROR_exit(PROGRAM_NAME ": -train_only specified twice");train_only=1;nopt++;}
     else if(IS("-save_model")){
       NEEDARG("a model filename");if(save_model)ERROR_exit(PROGRAM_NAME ": -save_model specified twice");save_model=argv[nopt++];
     }
     else if(IS("-save_patterns")){
       NEEDARG("activation-pattern prefix");
       if(pattern_prefix)ERROR_exit(PROGRAM_NAME ": -save_patterns specified twice");
       pattern_prefix=argv[nopt++];
     }
     else if(IS("-save_coefficients")){
       NEEDARG("a separate descriptive output prefix");
       if(coef_prefix)ERROR_exit(PROGRAM_NAME ": -save_coefficients was specified twice");
       coef_prefix=argv[nopt++];
     }
     else if(IS("-coef_lambda")){
       char *end;NEEDARG("a positive descriptive refit penalty");
       if(coef_lambda_given)ERROR_exit(PROGRAM_NAME ": -coef_lambda was specified twice");
       errno=0;coef_lambda=strtod(argv[nopt],&end);coef_lambda_given=1;
       if(errno || end==argv[nopt] || *end || !isfinite(coef_lambda) || coef_lambda<=0)
         ERROR_exit(PROGRAM_NAME ": -coef_lambda must be finite and positive");
       nopt++;
     }
     else if( IS("-save_predictions") ){ NEEDARG("a filename") ; save_predictions=argv[nopt++] ; }
     else if( IS("-save_fold_scores") ){ NEEDARG("a filename") ; save_fold_scores=argv[nopt++] ; }
     else if( IS("-save_confusion") ){ NEEDARG("a filename") ; save_confusion=argv[nopt++] ; }
     else if(IS("-save_probability_scores")){NEEDARG("a filename");save_probability_scores=argv[nopt++];}
     else if(IS("-save_calibration")){NEEDARG("a filename");save_calibration=argv[nopt++];}
     else if( IS("-save_summary_scores") ){ NEEDARG("a filename") ; save_summary_scores=argv[nopt++] ; }
     else if( IS("-show_table") ){ show_table=1 ; nopt++ ; }
     else if( IS("-show_samples") ){ show_samples=1 ; nopt++ ; }
     else if( IS("-show_folds") ){ show_folds=1 ; nopt++ ; }
     else if( IS("-validate_only") ){ validate_only=1 ; nopt++ ; }
     else if( IS("-quiet") ){ quiet=1 ; nopt++ ; }
     else if( IS("-progress") ){
       NEEDARG("auto, bar, line, or off") ;
            if(!strcasecmp(argv[nopt],"auto"))progress_mode=MVPA_PROGRESS_AUTO;
       else if(!strcasecmp(argv[nopt],"bar"))progress_mode=MVPA_PROGRESS_BAR;
       else if(!strcasecmp(argv[nopt],"line"))progress_mode=MVPA_PROGRESS_LINE;
       else if(!strcasecmp(argv[nopt],"off"))progress_mode=MVPA_PROGRESS_OFF;
       else ERROR_exit(PROGRAM_NAME ": -progress must be auto, bar, line, or off");
       nopt++;
     }
     else if( IS("-trial_manifest") ){
       NEEDARG("a trial manifest filename") ;
       if( tab!=NULL ) ERROR_exit(PROGRAM_NAME ": choose one of -trial_manifest and -dataTable/-dataTableFile") ;
       tab=THD_read_datatable_file(argv[nopt++]) ; trial_manifest=1 ;
     }
     else if( IS("-dataTableFile") ){
       NEEDARG("a table file") ;
       if( tab!=NULL ) ERROR_exit(PROGRAM_NAME ": the data table was given twice") ;
       tab=THD_read_datatable_file(argv[nopt++]) ;
     }
     else if( IS("-dataTable") ){
       int nused=0 ; NEEDARG("a table or @file") ;
       if( tab!=NULL ) ERROR_exit(PROGRAM_NAME ": the data table was given twice") ;
       tab=THD_read_datatable_args(argc,argv,nopt,mvpa_options,&nused) ; nopt+=nused ;
     }
     else if(IS("-testTable") || IS("-testTableFile")){
       int file=IS("-testTableFile"),nused=0;NEEDARG("a test table or @file");
       if(test_tab)ERROR_exit(PROGRAM_NAME ": the test table was given twice");
       if(file)test_tab=THD_read_datatable_file(argv[nopt++]);
       else{test_tab=THD_read_datatable_args(argc,argv,nopt,mvpa_options,&nused);nopt+=nused;}
     }
     else ERROR_exit(PROGRAM_NAME ": unknown option '%s'",argv[nopt]) ;
#undef NEEDARG
#undef IS
   }
   if( nopt<argc ) ERROR_exit(PROGRAM_NAME ": unexpected argument '%s'",argv[nopt]) ;
   if( validate_only ) show_table=show_samples=show_folds=1 ;
   if( mode==NULL || (strcasecmp(mode,"classify") && strcasecmp(mode,"decode") && strcasecmp(mode,"encode")) )
     ERROR_exit(PROGRAM_NAME ": -mode must be classify, decode, or encode") ;
   encoding=!strcasecmp(mode,"encode");continuous=encoding || !strcasecmp(mode,"decode");
   if(!method)method=continuous?"ridge":"centroid";
   mvpa_reg_configure(&reg,method,continuous,encoding,design_columns);
   if(reg.config.method){
     if(tune || numeric_lambda || grid_text || lambda_per_response)ERROR_exit(PROGRAM_NAME ": -lambda options require ridge; use estimator-specific parameters");
     if(reg.config.method==THD_REG_BANDED_RIDGE && (feature_text || pca_text))ERROR_exit(PROGRAM_NAME ": banded ridge preserves design groups; selection/PCA are not supported");
     if(reg.config.method==THD_REG_ELASTIC_NET && pca_text)ERROR_exit(PROGRAM_NAME ": elastic-net original-feature sparsity requires no PCA");
   }
   ridge_class=!continuous && !strcasecmp(method,"ridge");
   logistic_class=!continuous && !strcasecmp(method,"logistic");
   if((logistic_options || save_probability_scores || save_calibration) && !logistic_class)
     ERROR_exit(PROGRAM_NAME ": logistic/probability options require -mode classify -method logistic");
   if(logistic_grid_text && logistic_numeric)ERROR_exit(PROGRAM_NAME ": -logistic_lambda_grid cannot accompany numeric -logistic_lambda");
   if(logistic_grid_text)logistic_tune=1;
   svm_class=!continuous && !strcasecmp(method,"svm");
   if(svm_options && !svm_class)ERROR_exit(PROGRAM_NAME ": SVM options require -mode classify -method svm");
   if(svm_grid_text && svm_numeric)ERROR_exit(PROGRAM_NAME ": -svm_c_grid cannot accompany numeric -svm_c");
   if(svm_grid_text)svm_tune=1;
   if(!continuous && !strcasecmp(method,"lda_diag"))lda_mode=THD_LDA_DIAGONAL;
   if(!continuous && !strcasecmp(method,"lda_shrinkage"))lda_mode=THD_LDA_SHRINKAGE;
   if(lda_mode<0 && (shrinkage_given || floor_given || class_prior))
     ERROR_exit(PROGRAM_NAME ": shrinkage, variance floor and class-prior options require LDA classification");
   if(shrinkage_given && lda_mode!=THD_LDA_SHRINKAGE)
     ERROR_exit(PROGRAM_NAME ": -shrinkage requires -method lda_shrinkage");
   if(lambda_per_response){
     if(!continuous)ERROR_exit(PROGRAM_NAME ": -lambda_per_response requires continuous decoding or encoding");
     if(numeric_lambda)ERROR_exit(PROGRAM_NAME ": -lambda_per_response cannot accompany numeric -lambda; use -lambda_grid or -lambda auto");
     tune=1;
   }
   if(grid_text && numeric_lambda)ERROR_exit(PROGRAM_NAME ": -lambda_grid cannot accompany a numeric -lambda");
   if(grid_text)tune=1;
   if((tune || numeric_lambda) && !continuous && !ridge_class)
     ERROR_exit(PROGRAM_NAME ": ridge lambda and inner-CV options require -method ridge");
   if((ridge_class || svm_class || logistic_class || lda_mode>=0) && distance_given)ERROR_exit(PROGRAM_NAME ": -distance is only used by centroid classification");
   if(feature_text){
     char *copy=strdup(feature_text),*cur,*next;int i,j;
     if(!copy)ERROR_exit(PROGRAM_NAME ": cannot parse feature counts");
     nfeature_counts=1;for(cur=copy;*cur;cur++)if(*cur==',')nfeature_counts++;
     if((feature_fixed && nfeature_counts!=1) || (!feature_fixed && nfeature_counts<2))
       ERROR_exit(PROGRAM_NAME ": -select_features needs one count; -select_features_grid needs at least two");
     feature_counts=malloc(sizeof(int)*(size_t)nfeature_counts);
     if(!feature_counts)ERROR_exit(PROGRAM_NAME ": cannot allocate feature counts");
     cur=copy;for(i=0;i<nfeature_counts;i++){
       next=strchr(cur,',');if(next)*next++='\0';
       feature_counts[i]=(int)mvpa_parse_long("feature count",cur,1,INT_MAX);
       for(j=0;j<i;j++)if(feature_counts[i]==feature_counts[j])ERROR_exit(PROGRAM_NAME ": duplicate feature count");
       cur=next;
     }free(copy);
     if(coef_prefix)ERROR_exit(PROGRAM_NAME ": -save_coefficients cannot yet export feature selection; omit it with -select_features/-select_features_grid");
   }
   if(pca_text){
     char *copy=strdup(pca_text),*cur,*next;int i,j;
     if(!copy)ERROR_exit(PROGRAM_NAME ": cannot parse PCA counts");
     npca_counts=1;for(cur=copy;*cur;cur++)if(*cur==',')npca_counts++;
     if((pca_fixed && npca_counts!=1) || (!pca_fixed && npca_counts<2))
       ERROR_exit(PROGRAM_NAME ": -pca_components needs one count; -pca_components_grid needs at least two");
     pca_counts=malloc(sizeof(int)*(size_t)npca_counts);
     if(!pca_counts)ERROR_exit(PROGRAM_NAME ": cannot allocate PCA counts");
     cur=copy;for(i=0;i<npca_counts;i++){
       next=strchr(cur,',');if(next)*next++='\0';
       pca_counts[i]=(int)mvpa_parse_long("PCA count",cur,1,INT_MAX);
       for(j=0;j<i;j++)if(pca_counts[i]==pca_counts[j])ERROR_exit(PROGRAM_NAME ": duplicate PCA count");
       cur=next;
     }free(copy);
     if(coef_prefix)ERROR_exit(PROGRAM_NAME ": -save_coefficients cannot yet export PCA preprocessing");
   }
   if(reg.config.method && reg.config.ncandidate>1)tune=1;
   if(save_pca_loadings && !npca_counts)ERROR_exit(PROGRAM_NAME ": -save_pca_loadings requires PCA");
   if(!tune && !svm_tune && !logistic_tune && nfeature_counts<2 && npca_counts<2 && (inner_column || inner_k))ERROR_exit(PROGRAM_NAME ": inner-CV options require a feature/PCA count grid or tuned ridge/SVM/logistic penalties");
   if(tune && !reg.config.method)lambda_grid=mvpa_penalty_grid(grid_text?grid_text:"0.01,0.1,1,10,100",&nlambda,"lambda");
   if(logistic_tune)logistic_grid=mvpa_penalty_grid(logistic_grid_text?logistic_grid_text:"0.01,0.1,1,10,100",&logistic_ngrid,"logistic lambda");
   if(svm_tune)svm_grid=mvpa_penalty_grid(svm_grid_text?svm_grid_text:"0.01,0.1,1,10,100",&svm_ngrid,"SVM C");
   if((nfeature_counts>1 || npca_counts>1) && (continuous || ridge_class) && !tune){
     lambda_grid=malloc(sizeof(double));if(!lambda_grid)ERROR_exit(PROGRAM_NAME ": cannot allocate fixed lambda");
     lambda_grid[0]=lambda;nlambda=1;
   }
   if((nfeature_counts>1 || npca_counts>1) && svm_class && !svm_tune){
     svm_grid=malloc(sizeof(double));if(!svm_grid)ERROR_exit(PROGRAM_NAME ": cannot allocate fixed C");
     svm_grid[0]=svm_cost;svm_ngrid=1;
   }
   if((nfeature_counts>1 || npca_counts>1) && logistic_class && !logistic_tune){
     logistic_grid=malloc(sizeof(double));if(!logistic_grid)ERROR_exit(PROGRAM_NAME ": cannot allocate fixed logistic lambda");
     logistic_grid[0]=logistic_cost;logistic_ngrid=1;
   }
   if(reg.config.method && (tune || nfeature_counts>1 || npca_counts>1)){
     free(lambda_grid);nlambda=reg.config.ncandidate;lambda_grid=malloc(sizeof(double)*(size_t)nlambda);
     if(!lambda_grid)ERROR_exit(PROGRAM_NAME ": cannot allocate regression search IDs");
     for(ii=0;ii<nlambda;ii++)lambda_grid[ii]=ii+1;
   }
   if(nuisance_columns && coef_prefix)
     ERROR_exit(PROGRAM_NAME ": -save_coefficients cannot yet export nuisance-adjusted preprocessing; omit it with -nuisance_columns");
   if(save_model){
     if(encoding || (strcasecmp(method,"ridge") && !logistic_class))ERROR_exit(PROGRAM_NAME ": -save_model supports ridge decoding/classification only, or logistic classification");
     if(preprocess==THD_PREP_RUN_CENTER || !strcasecmp(scale_name,"run_center"))ERROR_exit(PROGRAM_NAME ": -save_model v1 does not support run_center");
     if(coef_prefix)ERROR_exit(PROGRAM_NAME ": -save_model v1 cannot accompany the separate -save_coefficients refit");
     if(!THD_filename_ok(save_model))ERROR_exit(PROGRAM_NAME ": invalid model filename");
   }
   if(pattern_prefix){
     if(encoding || strcasecmp(method,"ridge"))ERROR_exit(PROGRAM_NAME ": -save_patterns requires ridge decoding or ridge classification");
     if(!THD_filename_ok(pattern_prefix) || !strcmp(pattern_prefix,prefix) || (coef_prefix && !strcmp(pattern_prefix,coef_prefix)))
       ERROR_exit(PROGRAM_NAME ": activation-pattern prefix must be legal and distinct from score/coefficient prefixes");
   }
   if(coef_lambda_given && !coef_prefix)ERROR_exit(PROGRAM_NAME ": -coef_lambda requires -save_coefficients");
   if(coef_prefix){
     if(strcasecmp(method,"ridge"))ERROR_exit(PROGRAM_NAME ": -save_coefficients requires -method ridge");
     if(!THD_filename_ok(coef_prefix) || !strcmp(coef_prefix,prefix))
       ERROR_exit(PROGRAM_NAME ": descriptive coefficient prefix must be legal and different from -prefix");
     if(tune && !coef_lambda_given)
       ERROR_exit(PROGRAM_NAME ": tuned CV requires an explicit -coef_lambda for descriptive full-data refits");
     if(!coef_lambda_given)coef_lambda=lambda;
   }
   if(!metric_name)metric_name=continuous?"cross_validated_r2":"balanced_accuracy";
   if(encoding){
     if(!design_columns)ERROR_exit(PROGRAM_NAME ": encoding requires -design_columns");
     if(target_column)ERROR_exit(PROGRAM_NAME ": encoding responses come from InputFile; use -design_columns");
     if(!score_mode)score_mode="voxel";
     if(strcasecmp(score_mode,"voxel") && strcasecmp(score_mode,"pattern"))
       ERROR_exit(PROGRAM_NAME ": encoding -score must be voxel or pattern");
     if(!strcasecmp(score_mode,"voxel")){
       if(searchlight || surf_file || roi_sel)ERROR_exit(PROGRAM_NAME ": voxel encoding uses -mask without -searchlight, -surf, or -roi_sel");
       voxel_encoding=1;
     }
   }else if(design_columns || score_mode)ERROR_exit(PROGRAM_NAME ": -design_columns and -score require -mode encode");
   if( tab==NULL ) ERROR_exit(PROGRAM_NAME ": -dataTable or -dataTableFile is required") ;
   if( !encoding && target_column==NULL ) ERROR_exit(PROGRAM_NAME ": -target_column or -target_columns is required") ;
   if(test_tab){
     if(trial_manifest || partition_column || train_value || test_value)
       ERROR_exit(PROGRAM_NAME ": -testTable cannot accompany -trial_manifest, -partition_column, -train_value, or -test_value");
     two_tables=1;partition_column=MVPA_PARTITION;
   }
   if(transfer.column){
     if((partition_column && !two_tables) || train_value || test_value || cv_column || cv_k || within_column)
       ERROR_exit(PROGRAM_NAME ": transfer replaces outer CV/partition options; use only -transfer_column and domain values");
     if(!transfer.train || !transfer.test || !strcmp(transfer.train,transfer.test) || !transfer.subjects ||
        (strcmp(transfer.subjects,"within") && strcmp(transfer.subjects,"disjoint")))
       ERROR_exit(PROGRAM_NAME ": transfer needs distinct -train_domain/-test_domain and -transfer_subjects within|disjoint");
     if(!transfer.items)transfer.items="disjoint";
     if(!transfer.sessions)transfer.sessions="disjoint";
     if(!transfer.session_column)transfer.session_column="Session";
     if((strcmp(transfer.items,"allow") && strcmp(transfer.items,"disjoint")) ||
        (strcmp(transfer.sessions,"allow") && strcmp(transfer.sessions,"disjoint")))
       ERROR_exit(PROGRAM_NAME ": transfer item/session policies must be disjoint or allow");
     partition_column=transfer.column;train_value=transfer.train;test_value=transfer.test;
   }else if(transfer.train || transfer.test || transfer.subjects || transfer.items || transfer.sessions || transfer.session_column)
     ERROR_exit(PROGRAM_NAME ": transfer options require -transfer_column");
   if(train_only){
     if(!save_model)ERROR_exit(PROGRAM_NAME ": -train_only requires -save_model");
     if(partition_column || test_tab || train_value || test_value || transfer.column || cv_column || cv_k || within_column)
       ERROR_exit(PROGRAM_NAME ": -train_only cannot accompany test tables, partitions, transfer or outer CV");
     if(nperm || permute_within || save_predictions || save_fold_scores || save_confusion || save_summary_scores || save_probability_scores || save_calibration || coef_prefix || pattern_prefix)
       ERROR_exit(PROGRAM_NAME ": -train_only does not compute predictions, held-out scores, patterns or inference");
     if((tune || logistic_tune || nfeature_counts>1 || npca_counts>1) && !inner_column)
       ERROR_exit(PROGRAM_NAME ": tuned -train_only requires an explicit -inner_cv_column");
   }
   if(partition_column){
     if(cv_column || cv_k || within_column)
       ERROR_exit(PROGRAM_NAME ": -partition_column/-testTable cannot accompany -cv, -cv_column, -cv_k, or -within_column");
     if((tune || svm_tune || logistic_tune || nfeature_counts>1 || npca_counts>1) && !inner_column)
       ERROR_exit(PROGRAM_NAME ": tuned fixed partitions require an explicit -inner_cv_column using training groups");
     if(coef_prefix)
       ERROR_exit(PROGRAM_NAME ": -save_coefficients refits all rows and cannot accompany -partition_column/-testTable; keep the reserved test set out of refitting");
     if(!train_value)train_value="train";
     if(!test_value)test_value="test";
     if(!strcmp(train_value,test_value))
       ERROR_exit(PROGRAM_NAME ": fixed partitions require distinct train/test values");
   }else{
     if(train_value || test_value)
       ERROR_exit(PROGRAM_NAME ": -train_value and -test_value require -partition_column");
     if(!cv_column && !train_only)ERROR_exit(PROGRAM_NAME ": -cv_column, -cv, -partition_column or -train_only is required");
   }
   if( nperm==1 ) ERROR_exit(PROGRAM_NAME ": -nperm must be 0 or at least 2") ;
   if( nperm>0 && permute_within==NULL )
     ERROR_exit(PROGRAM_NAME ": -nperm requires -permute_within COLUMN (or explicit 'none')") ;
   if( nperm==0 && permute_within!=NULL )
     ERROR_exit(PROGRAM_NAME ": -permute_within is only used with -nperm") ;
   if( surf_file!=NULL && searchlight==NULL )
     ERROR_exit(PROGRAM_NAME ": -surf is only used with -searchlight") ;
   if( maskname==NULL && !(surf_file!=NULL && searchlight!=NULL) )
     ERROR_exit(PROGRAM_NAME ": -mask is required, except for a surface searchlight over every represented node") ;
   if( searchlight!=NULL && roi_sel!=NULL )
     ERROR_exit(PROGRAM_NAME ": -roi_sel cannot be combined with -searchlight") ;
   if(continuous && (save_confusion || save_summary_scores))
     ERROR_exit(PROGRAM_NAME ": -save_confusion and -save_summary_scores require -mode classify");
   {
     const char *files[]={save_predictions,save_fold_scores,save_confusion,save_summary_scores,save_tuning};
     int a,b;
     for(a=0;a<5;a++) if(files[a]){
       if(!files[a][0]) ERROR_exit(PROGRAM_NAME ": audit tables need nonempty filenames");
       for(b=0;b<a;b++) if(files[b] && !strcmp(files[a],files[b]))
         ERROR_exit(PROGRAM_NAME ": prediction/fold/confusion/summary audits need different files");
     }
   }
   if(continuous?(!reg.config.method && strcasecmp(method,"ridge")!=0):(!ridge_class && !svm_class && !logistic_class && lda_mode<0 && strcasecmp(method,"centroid")!=0))
     ERROR_exit(PROGRAM_NAME ": mode %s requires -method %s",mode,continuous?"ridge, elastic_net, banded_ridge, pls, or svr":"centroid, ridge, svm, logistic, lda_diag, or lda_shrinkage") ;
   if( !THD_filename_ok(prefix) ) ERROR_exit(PROGRAM_NAME ": illegal -prefix '%s'",prefix) ;
   if( !THD_centroid_distance_parse(distance_name,&distance,err,sizeof(err)) )
     ERROR_exit(PROGRAM_NAME ": %s",err) ;
   if( !THD_preprocess_parse(scale_name,&preprocess,err,sizeof(err)) )
     ERROR_exit(PROGRAM_NAME ": %s",err) ;
   if( !THD_mvpa_metric_parse(metric_name,&metric,err,sizeof(err)) )
     ERROR_exit(PROGRAM_NAME ": %s",err) ;
   if( !continuous && metric!=THD_METRIC_ACCURACY && metric!=THD_METRIC_BALANCED_ACCURACY )
     ERROR_exit(PROGRAM_NAME ": classification -metric must be accuracy or balanced_accuracy") ;
   if(continuous && metric<THD_METRIC_PEARSON)
     ERROR_exit(PROGRAM_NAME ": continuous metric must be pearson, spearman, cross_validated_r2, MAE, or RMSE");

   if((tune_metric_name || save_tuning) && !tune && !svm_tune && !logistic_tune && nfeature_counts<2 && npca_counts<2)
     ERROR_exit(PROGRAM_NAME ": -tune_metric and -save_tuning require a lambda/C or feature/PCA count search");
   if(logistic_class){if(!THD_mvpa_logistic_objective(tune_requested,&tune_objective,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);}
   else if(!THD_mvpa_tune_objective_resolve(tune_requested,!continuous,continuous || ridge_class,metric,&tune_objective,err,sizeof(err)))
     ERROR_exit(PROGRAM_NAME ": %s",err);
   if(save_model || transfer.column || save_probability_scores || save_calibration){model_train_source=tab->source?strdup(tab->source):NULL;model_test_source=test_tab && test_tab->source?strdup(test_tab->source):NULL;}
   if(two_tables){
     expanded=mvpa_train_test_table(tab,test_tab);
     THD_free_datatable(tab);THD_free_datatable(test_tab);test_tab=NULL;
     tab=expanded;expanded=NULL;
   }
   if( trial_manifest ){
     if( xmat_stims!=NULL ) ERROR_exit(PROGRAM_NAME ": -xmat_stims cannot accompany -trial_manifest") ;
     expanded=THD_datatable_trial_manifest(tab) ;
     THD_free_datatable(tab) ; tab=expanded ; expanded=NULL ;
     if( tab==NULL ) ERROR_exit(PROGRAM_NAME ": trial manifest conversion failed") ;
   } else if( THD_datatable_column(tab,"Xmat")>=0 ){
     const char *icol=item_column?item_column:"Item" ;
     const char *rcol=run_column?run_column:"Run" ; int have_run=0 ;
     expanded=THD_datatable_expand_xmat_items(tab,"Xmat",icol,rcol,xmat_stims,&have_run) ;
     THD_free_datatable(tab) ; tab=expanded ; expanded=NULL ;
     if( tab==NULL ) ERROR_exit(PROGRAM_NAME ": X-matrix table expansion failed") ;
   } else if( xmat_stims!=NULL )
     ERROR_exit(PROGRAM_NAME ": -xmat_stims requires an Xmat column") ;
   if( show_table ) THD_datatable_print(tab,stdout) ;

   THD_sampleset_detect_columns(tab,&columns) ;
   if( subject_column!=NULL ) columns.subject=subject_column ;
   if( run_column!=NULL ) columns.run=run_column ;
   if( item_column!=NULL ) columns.item=item_column ;
   if( fold_column!=NULL ) columns.fold=fold_column ;
   if( sample_column!=NULL ) columns.sample=sample_column ;
   ss=THD_sampleset_from_table(tab,&columns,err,sizeof(err)) ;
   THD_free_datatable(tab) ; tab=NULL ;
   if( ss==NULL ) ERROR_exit(PROGRAM_NAME ": %s",err) ;
   if( show_samples ) THD_sampleset_print(ss,stdout) ;
   if(show_samples && two_tables){
     fputs("# Two-table sample provenance\n# Columns: TableRow\tSample",stdout);
     mvpa_source_fields(stdout,ss,-1);fputc('\n',stdout);
     for(ii=0;ii<ss->nsample;ii++){
       fprintf(stdout,"%d\t",ss->table_row[ii]+1);mvpa_1D_text(stdout,ss->sample_label[ii]);
       mvpa_source_fields(stdout,ss,ii);fputc('\n',stdout);
     }
   }

   if( !continuous && !THD_sampleset_encode_column(ss,target_column,&class_id,&class_label,&nclass,err,sizeof(err)) )
     ERROR_exit(PROGRAM_NAME ": %s",err) ;
   if( !continuous && nclass<2 ) ERROR_exit(PROGRAM_NAME ": target column '%s' has only %d class",target_column,nclass) ;
   if(transfer.column){
     transfer.subject_column=columns.subject;transfer.item_column=columns.item;
     if(!columns.subject || !columns.item)
       ERROR_exit(PROGRAM_NAME ": transfer requires explicit subject and item columns (Subj and Item/Trial, or role overrides)");
   }
   plan=train_only ? THD_foldplan_train_only(ss->nsample,err,sizeof(err)) : transfer.column ? THD_foldplan_fixed_within(ss,transfer.column,transfer.train,transfer.test,
          !strcmp(transfer.subjects,"within")?columns.subject:NULL,err,sizeof(err))
        :partition_column ? THD_foldplan_fixed(ss,partition_column,train_value,test_value,err,sizeof(err))
        :(cv_k>0) ? THD_foldplan_grouped_kfold(ss,cv_column,within_column,cv_k,err,sizeof(err))
                 : THD_foldplan_leave_group_out(ss,cv_column,within_column,err,sizeof(err)) ;
   if( plan==NULL ) ERROR_exit(PROGRAM_NAME ": %s",err) ;
   if(two_tables && !transfer.column)mvpa_check_table_overlap(ss,plan);
   if(transfer.column)mvpa_transfer_check(&transfer,ss,plan,two_tables,continuous,encoding?design_columns:target_column);
   if(save_model && !train_only && (plan->kind!=THD_CV_FIXED || plan->nfold!=1))ERROR_exit(PROGRAM_NAME ": -save_model v1 requires one explicit fixed train/test partition; save within-subject transfer models one subject at a time");
   if( !continuous && !THD_foldplan_check_classes(plan,class_id,nclass,transfer.column!=NULL,err,sizeof(err)) )
     ERROR_exit(PROGRAM_NAME ": %s",err) ;
   if(partition_column && !continuous)mvpa_fixed_class_order(plan,class_id,class_label,nclass);
   if(partition_column && nperm>0)mvpa_fixed_permutation_blocks(ss,plan,permute_within);
   if(partition_column && !transfer.column && !quiet)
     INFO_message(PROGRAM_NAME ": fixed partition %s: %d training (%s), %d test (%s); scores use test rows only",
                  partition_column,plan->ntrain[0],train_value,plan->ntest[0],test_value);
   if(transfer.column && !quiet)INFO_message(PROGRAM_NAME ": transfer %s -> %s: %s subjects, %d independent fit(s); only destination rows are scored",
      transfer.train,transfer.test,transfer.subjects,plan->nfold);
   if( show_folds ) THD_foldplan_print(plan,ss,stdout) ;
   if(lambda_per_response && !quiet)INFO_message(PROGRAM_NAME ": response-specific lambda: shared preprocessing counts minimize mean response-wise best inner loss; outer fits use one factorization per distinct selected penalty");
   if(tune || svm_tune || logistic_tune || nfeature_counts>1 || npca_counts>1){
     int *inner_groups=NULL,ng=0;char **labels=NULL;
     if(!inner_column)inner_column=cv_column;
     if(!THD_sampleset_encode_column(ss,inner_column,&inner_groups,&labels,&ng,err,sizeof(err)))
       ERROR_exit(PROGRAM_NAME ": %s",err);
     nested=THD_nested_foldplan_grouped(plan,inner_groups,inner_k,err,sizeof(err));
     free(inner_groups);THD_sampleset_free_levels(labels,ng);
     if(!nested)ERROR_exit(PROGRAM_NAME ": %s",err);
     if(show_folds)THD_nested_foldplan_print(nested,ss,stdout);
     if(!quiet)INFO_message(PROGRAM_NAME ": nested tuning: %d feature-count, %d PCA-count, %d penalty candidates; inner groups from %s",nfeature_counts?nfeature_counts:1,npca_counts?npca_counts:1,logistic_class?logistic_ngrid:svm_class?svm_ngrid:nlambda?nlambda:1,inner_column);
   }

   if(nuisance_columns){
     int j;
     nuisance=mvpa_numeric_columns(ss,nuisance_columns,&nuisance_names,&nnuisance);
     /* Explicit targets/design predictors must not also be adjustment columns. */
     { char *copy=strdup(encoding?design_columns:target_column),*cur=copy,*next;
       if(!copy)ERROR_exit(PROGRAM_NAME ": cannot check nuisance column roles");
       while(cur){next=strchr(cur,',');if(next)*next++='\0';
         for(j=0;j<nnuisance;j++)if(!strcasecmp(cur,nuisance_names[j]))
           ERROR_exit(PROGRAM_NAME ": nuisance column '%s' also appears in the target/design columns",cur);
         cur=next;
       }free(copy);
     }
     if(!THD_mvpa_check_nuisance(plan,nested,nuisance,nnuisance,err,sizeof(err)))
       ERROR_exit(PROGRAM_NAME ": %s",err);
     if(nperm>0)mvpa_nuisance_blocks(ss,nuisance,nnuisance,permute_within);
     if(!quiet)INFO_message(PROGRAM_NAME ": predictor nuisance OLS: %s + intercept, fitted in every training split before scaling",nuisance_columns);
   }

   if( nperm>0 && !validate_only ){
     int ip,*check_class ; long long group_size ;
     perm_scheme=THD_perm_scheme_new(ss->nsample) ;
     if( perm_scheme==NULL ) ERROR_exit(PROGRAM_NAME ": cannot allocate the label-permutation scheme") ;
     perm_scheme->exchange=PERM_EE ; perm_scheme->blockmode=PERM_WITHIN_BLOCK ;
     perm_scheme->exact=0 ; perm_scheme->nperm=nperm ; perm_scheme->seed=seed ;
     if( strcasecmp(permute_within,"none")!=0 ){
       if( !THD_sampleset_encode_column(ss,permute_within,&perm_block,
                                        &perm_block_label,&nperm_block,
                                        err,sizeof(err)) )
         ERROR_exit(PROGRAM_NAME ": %s",err) ;
       if( THD_perm_scheme_set_blocks(perm_scheme,perm_block)!=nperm_block )
         ERROR_exit(PROGRAM_NAME ": cannot encode exchangeability blocks from '%s'",permute_within) ;
     } else nperm_block=1 ;
     if( !continuous && !THD_perm_scheme_set_eqclass(perm_scheme,class_id) )
       ERROR_exit(PROGRAM_NAME ": cannot encode equivalent target labels for permutation") ;
     group_size=THD_perm_group_size(perm_scheme) ;
     if( group_size!=PERM_COUNT_TOOBIG && group_size>0 && group_size<=nperm )
       perm_scheme->exact=1 ;
     perm_set=THD_perm_set_build(perm_scheme) ;
     THD_perm_scheme_free(perm_scheme) ; perm_scheme=NULL ;
     THD_sampleset_free_levels(perm_block_label,nperm_block) ; perm_block_label=NULL ;
     free(perm_block) ; perm_block=NULL ;
     if( perm_set==NULL ) ERROR_exit(PROGRAM_NAME ": cannot build the label-permutation set") ;
     if( perm_set->nperm<2 )
       ERROR_exit(PROGRAM_NAME ": -permute_within %s permits only the identity label arrangement",
                  permute_within) ;
     if(!continuous){
     check_class=(int *)malloc(sizeof(int)*(size_t)ss->nsample) ;
     if( check_class==NULL ) ERROR_exit(PROGRAM_NAME ": out of memory validating label permutations") ;
     for( ip=0 ; ip<perm_set->nperm ; ip++ ){
       for( ii=0 ; ii<ss->nsample ; ii++ ){
         int src=perm_set->perm[(size_t)ip*ss->nsample+ii] ;
         if( src<0 || src>=ss->nsample ||
             perm_set->sign[(size_t)ip*ss->nsample+ii]!=1 )
           ERROR_exit(PROGRAM_NAME ": invalid label relabeling %d at sample %d",ip+1,ii+1) ;
         check_class[ii]=class_id[src] ;
       }
       if( !THD_foldplan_check_classes(plan,check_class,nclass,0,err,sizeof(err)) )
         ERROR_exit(PROGRAM_NAME ": label relabeling %d violates CV class coverage: %s;\n"
                    "       choose a finer -permute_within column whose blocks preserve every training fold",
                    ip+1,err) ;
     }
     free(check_class) ;
     }
     if( !quiet )
       INFO_message(PROGRAM_NAME ": %d synchronized %s relabelings (%s; %d block%s from %s)",
                    perm_set->nperm,continuous?"row":"label",perm_set->is_exact?"exact":"Monte Carlo",
                    nperm_block,nperm_block==1?"":"s",permute_within) ;
   }

   if( preprocess==THD_PREP_RUN_CENTER ){
     if( ss->run==NULL || ss->nrun<1 )
       ERROR_exit(PROGRAM_NAME ": -scale run_center requires a Run role; use -run_column if needed") ;
     if( ss->nsubject>INT_MAX/ss->nrun )
       ERROR_exit(PROGRAM_NAME ": too many subject-by-run groups for run_center") ;
     group_id=(int *)malloc(sizeof(int)*(size_t)ss->nsample) ;
     if( group_id==NULL ) ERROR_exit(PROGRAM_NAME ": out of memory encoding subject-by-run groups") ;
     for( ii=0 ; ii<ss->nsample ; ii++ ){
       if( ss->subject[ii]<0 || ss->run[ii]<0 )
         ERROR_exit(PROGRAM_NAME ": sample %d has no subject/run role for run_center",ii+1) ;
       group_id[ii]=ss->subject[ii]*ss->nrun+ss->run[ii] ;
     }
   }

   mset=THD_open_dataset(maskname!=NULL?maskname:ss->dset_name[0]) ;
   if( mset==NULL )
     ERROR_exit(PROGRAM_NAME ": cannot open %s dataset '%s'",
                maskname!=NULL?"mask":"surface-domain",maskname!=NULL?maskname:ss->dset_name[0]) ;
   if(save_model && (surf_file || DBLK_IS_NI_SURF_DSET(mset->dblk) || DBLK_IS_GIFTI(mset->dblk)))ERROR_exit(PROGRAM_NAME ": -save_model v1 supports volume ROIs/searchlights only");
   if( surf_file!=NULL || DBLK_IS_NI_SURF_DSET(mset->dblk) || DBLK_IS_GIFTI(mset->dblk) ){
     for( ii=0 ; ii<ss->ndset ; ii++ )
       if( !THD_surface_domain_match(mset,ss->dset[ii],err,sizeof(err)) )
         ERROR_exit(PROGRAM_NAME ": %s",err) ;
   } else if( !EQUIV_GRIDS(mset,ss->dset[0]) )
     ERROR_exit(PROGRAM_NAME ": mask '%s' is not on the sample dataset grid",maskname) ;
   if(transfer.column && !DBLK_IS_NI_SURF_DSET(mset->dblk) && !DBLK_IS_GIFTI(mset->dblk))
     for(ii=0;ii<ss->ndset;ii++)mvpa_exact_volume_grid(mset,ss->dset[ii],"transfer");
   DSET_load(mset) ;
   if( !DSET_LOADED(mset) )
     ERROR_exit(PROGRAM_NAME ": cannot load %s dataset '%s'",
                maskname!=NULL?"mask":"surface-domain",maskname!=NULL?maskname:ss->dset_name[0]) ;
   if(voxel_encoding){
     rl=mvpa_voxel_units(mset);
   }else if( searchlight!=NULL ){
     if( surf_file!=NULL ){
#ifdef USE_SUMA
       float radius=mvpa_parse_positive_float("surface -searchlight radius",searchlight) ;
       char FuncName[]=PROGRAM_NAME ;
       SUMA_STANDALONE_INIT ;
       SUMAg_DOv=SUMA_Alloc_DisplayObject_Struct(SUMA_MAX_DISPLAYABLE_OBJECTS) ;
       if( !quiet )
         INFO_message(PROGRAM_NAME ": geodesic surface searchlight, radius %.3g mm on '%s'",
                      radius,surf_file) ;
       rl=THD_roilist_searchlight_surf(surf_file,mset,radius,maskname==NULL,
                                      err,sizeof(err)) ;
       if( rl==NULL && err[0]!='\0' ) ERROR_exit(PROGRAM_NAME ": %s",err) ;
       if( rl==NULL || rl->nroi<1 )
         ERROR_exit(PROGRAM_NAME ": surface searchlight has no represented in-mask nodes") ;
#else
       ERROR_exit(PROGRAM_NAME ": -surf needs a build with surface support (USE_SUMA/libSUMA)") ;
#endif
     } else {
       MCW_cluster *nbhd ;
       if( DBLK_IS_NI_SURF_DSET(mset->dblk) || DBLK_IS_GIFTI(mset->dblk) )
         ERROR_exit(PROGRAM_NAME ": a surface -searchlight also requires -surf MESH") ;
       nbhd=THD_searchlight_parse(searchlight,fabsf(DSET_DX(mset)),
                                  fabsf(DSET_DY(mset)),fabsf(DSET_DZ(mset)),
                                  err,sizeof(err)) ;
       if( nbhd==NULL ) ERROR_exit(PROGRAM_NAME ": %s",err) ;
       if( !quiet ) INFO_message(PROGRAM_NAME ": searchlight neighborhood '%s' = %d voxel offsets",
                                 searchlight,nbhd->num_pt) ;
       rl=THD_roilist_searchlight(mset,nbhd) ;
       KILL_CLUSTER(nbhd) ;
       if( rl==NULL || rl->nroi<1 )
         ERROR_exit(PROGRAM_NAME ": mask has no nonzero searchlight centers") ;
     }
   } else {
     rl=THD_roilist_from_dset(mset,roi_sel) ;
     if( rl==NULL || rl->nroi<1 )
       ERROR_exit(PROGRAM_NAME ": mask has no selected positive-valued ROIs") ;
   }
   if(save_tuning){
     const char *suffix[]={".roi.1D",".continuous.1D",".tuning.1D",".inference.1D",".features.1D",".feature_tuning.1D"};int j;
     char *name=malloc(strlen(prefix)+32);if(!name)ERROR_exit(PROGRAM_NAME ": cannot check tuning audit name");
     for(j=0;j<6;j++){
       snprintf(name,strlen(prefix)+32,"%s%s",prefix,suffix[j]);
       if(!strcmp(name,save_tuning))ERROR_exit(PROGRAM_NAME ": -save_tuning must differ from automatic result/audit filenames");
     }free(name);
   }
   if(nfeature_counts){
     int f,j;const char *files[]={save_predictions,save_fold_scores,save_confusion,save_summary_scores,save_tuning};
     for(f=0;f<(nested?2:1);f++){
       char *name=mvpa_selection_filename(prefix,f);
       for(j=0;j<5;j++)if(files[j] && !strcmp(name,files[j]))
         ERROR_exit(PROGRAM_NAME ": feature selection and explicit audit filenames must differ");
       free(name);
     }
     for(f=0;f<plan->nfold;f++){
       if(plan->ntrain[f]<2)ERROR_exit(PROGRAM_NAME ": feature selection needs at least two training samples");
       if(nested)for(j=0;j<nested->inner[f]->nfold;j++)if(nested->inner[f]->ntrain[j]<2)
         ERROR_exit(PROGRAM_NAME ": feature selection needs at least two inner training samples");
     }
   }
   if(npca_counts){
     int f,j;const char *files[]={save_predictions,save_fold_scores,save_confusion,save_summary_scores,save_tuning,save_pca_loadings};
     const char *suffix[]={".pca.1D",".pca_tuning.1D",".roi.1D",".continuous.1D",".tuning.1D",".inference.1D",".features.1D",".feature_tuning.1D"};
     char *name=malloc(strlen(prefix)+32);if(!name)ERROR_exit(PROGRAM_NAME ": cannot check PCA audit names");
     for(f=0;f<8;f++){
       snprintf(name,strlen(prefix)+32,"%s%s",prefix,suffix[f]);
       for(j=0;j<6;j++)if(files[j] && (f<2 || j==5) && !strcmp(name,files[j]))ERROR_exit(PROGRAM_NAME ": PCA/automatic and explicit audit filenames must differ");
     }free(name);
     for(j=0;j<5;j++)if(save_pca_loadings && files[j] && !strcmp(save_pca_loadings,files[j]))ERROR_exit(PROGRAM_NAME ": PCA loading and other audit filenames must differ");
     if(save_pca_loadings && !THD_filename_ok(save_pca_loadings))ERROR_exit(PROGRAM_NAME ": illegal PCA loading filename");
     for(f=0;f<plan->nfold;f++){
       if(plan->ntrain[f]<2)ERROR_exit(PROGRAM_NAME ": PCA needs at least two training samples");
       if(nested)for(j=0;j<nested->inner[f]->nfold;j++)if(nested->inner[f]->ntrain[j]<2)ERROR_exit(PROGRAM_NAME ": PCA needs at least two inner training samples");
     }
   }
   if(transfer.column){
     const char *files[]={save_predictions,save_fold_scores,save_confusion,save_summary_scores,save_tuning,save_pca_loadings,save_model,model_train_source,model_test_source};
     transfer.audit=malloc(strlen(prefix)+32);if(!transfer.audit)ERROR_exit(PROGRAM_NAME ": cannot name transfer audit");
     snprintf(transfer.audit,strlen(prefix)+32,"%s.transfer.1D",prefix);
     mvpa_audit_preflight("transfer audit",transfer.audit,prefix,files,9,ss,mset);
   }
   if(reg.config.method){
     int f,j;const char *files[]={save_predictions,save_fold_scores,save_tuning,save_pca_loadings,transfer.audit,reg.groups_file,reg.penalties_file,tab?tab->source:NULL};
     const char *suffix[]={"regression","sparse_features","group_contributions","regression_candidates","regression_scores"};
     char *name=malloc(strlen(prefix)+40);if(!name)ERROR_exit(PROGRAM_NAME ": cannot check regression audit paths");
     for(f=0;f<plan->nfold;f++){
       if(plan->ntrain[f]<2)ERROR_exit(PROGRAM_NAME ": regression estimators need at least two training rows");
       if(nested)for(j=0;j<nested->inner[f]->nfold;j++)if(nested->inner[f]->ntrain[j]<2)ERROR_exit(PROGRAM_NAME ": regression estimators need at least two inner training rows");
     }
     for(f=0;f<5;f++){
       if((f==1 && reg.config.method!=THD_REG_ELASTIC_NET) || (f==2 && reg.config.method!=THD_REG_BANDED_RIDGE) || (f==3 && !nested))continue;
       snprintf(name,strlen(prefix)+40,"%s.%s.1D",prefix,suffix[f]);
       mvpa_audit_preflight("regression audit",name,prefix,files,8,ss,mset);
     }free(name);
   }
   if(train_only){
     const char *files[]={save_model,save_tuning,save_pca_loadings,model_train_source};
     char *name=malloc(strlen(prefix)+32);if(!name)ERROR_exit(PROGRAM_NAME ": cannot name training summary");
     sprintf(name,"%s.training.1D",prefix);
     mvpa_audit_preflight("training summary",name,prefix,files,4,ss,mset);free(name);
   }
   if(save_model){
     const char *files[]={save_predictions,save_fold_scores,save_confusion,save_summary_scores,save_tuning,save_pca_loadings,model_train_source,model_test_source,transfer.audit};
     mvpa_model_preflight(save_model,prefix,files,9,ss,mset);
   }
   if(save_probability_scores || save_calibration){
     const char *files[]={save_predictions,save_fold_scores,save_confusion,save_summary_scores,save_tuning,save_pca_loadings,save_model,model_train_source,model_test_source,transfer.audit};
     if(save_probability_scores)mvpa_audit_preflight("probability audit",save_probability_scores,prefix,files,10,ss,mset);
     if(save_calibration)mvpa_audit_preflight("calibration audit",save_calibration,prefix,files,10,ss,mset);
     if(save_probability_scores && save_calibration && mvpa_pair_same_file(save_probability_scores,save_calibration))ERROR_exit(PROGRAM_NAME ": probability and calibration audits need different files");
   }
   free(model_train_source);free(model_test_source);model_train_source=model_test_source=NULL;
   if(pattern_prefix){
     const char *files[]={save_predictions,save_fold_scores,save_confusion,save_summary_scores,save_tuning,save_pca_loadings,save_model,transfer.audit};
     if(nclass>32760)ERROR_exit(PROGRAM_NAME ": too many class pattern bricks");
     mvpa_pattern_preflight(pattern_prefix,prefix,coef_prefix,files,8,mset,plan,rl);
   }
   if(validate_only){
     if(reg.config.method)mvpa_reg_header(stdout,&reg);
     if(transfer.column)mvpa_transfer_report(stdout,&transfer,ss,plan,encoding?design_columns:target_column,class_label,nclass);
     free(transfer.audit);transfer.audit=NULL;
     if(train_only)printf("# TrainingOnly\tyes; all rows train; no held-out scores\n");
     if(save_model)printf("# SavedModel\t%s_volume_pipeline\n# ModelFit\tno_validation_only\n",logistic_class?"version_2_logistic":"version_1_ridge");
     if(lambda_per_response)printf("# RidgePenaltyScope\tper_response\n# ConfigurationObjective\tmean_response_minimum_inner_loss\n# ResponseRidgeCost\touter_factors=min(number_of_responses,number_of_lambdas)_at_most; extra_audit_bytes~(8*nlambda+4)*nfold*nfeature_counts*npca_counts*nresponse_per_worker\n");
     if(pattern_prefix)printf("# ActivationPatterns\touter_training_ridge_joint_score_covariance_pseudoinverse\n# PatternCoordinates\tafter_nuisance_and_scale_before_selection_and_PCA\n# PatternFit\tno_validation_only\n");
     if(npca_counts){int j;printf("# PCAOrder\tnuisance_then_scale_all_columns_then_select_then_centered_PCA\n# PCAWhiten\tno\n# PCACounts");
       for(j=0;j<npca_counts;j++)printf("\t%d",pca_counts[j]);
       printf("\n# PCACountsCappedAtTrainingRank\tyes\n# PCAFit\tno_validation_only\n");}
     if(logistic_class)printf("# LogisticModel\tsymmetric_softmax_mean_log_loss_plus_lambda_over_two_times_weight_norm_squared\n# LogisticCalibration\tassessment_only_no_transform\n# LogisticFit\tno_validation_only\n");
     if(tune_metric_name || save_tuning || (logistic_class && nested))printf("# InnerCVObjective\t%s\n# InnerCVObjectiveAggregation\t%s\n",THD_mvpa_tune_objective_name(tune_objective),
       tune_objective==THD_TUNE_LOG_LOSS || tune_objective==THD_TUNE_BRIER?"pooled_held_out_rows":"pooled_rows_then_equal_response_or_class_mean");
     if(nfeature_counts){
       int j;printf("# FeatureSelection\t%s\n# FeatureSelectionOrder\tnuisance_then_scale_all_columns_then_select\n# FeatureCounts",continuous?"mean_squared_Pearson":"eta_squared");
       for(j=0;j<nfeature_counts;j++)printf("\t%d",feature_counts[j]);
       printf("\n# FeatureCountsCappedAtInputWidth\tyes\n# FeatureSelectionFitted\tno_validation_only\n");
     }
     if(nuisance){
       int f;
       printf("# NuisanceColumns\t%s\n# NuisanceAdjustment\tpredictors_only_OLS_before_scaling\n# NuisanceDesign\tintercept_plus_%d_numeric_columns\n",nuisance_columns,nnuisance);
       printf("# NuisanceTrainingRank\t%d\n# NuisanceOuterAndInnerRanks\tPASS\n",nnuisance+1);
       for(f=0;f<plan->nfold;f++)printf("# NuisanceOuterResidualDF\t%s\t%d\n",plan->name[f],plan->ntrain[f]-nnuisance-1);
       if(nperm)printf("# NuisancePermutation\trows_within_nuisance_constant_blocks\n");
     }
     mvpa_validate_report(ss,plan,nested,rl,DSET_HEADNAME(mset),mode,method,encoding?design_columns:target_column,
                          continuous,class_id,class_label,nclass,group_id,lda_mode>=0,
                          nperm,permute_within);
     DSET_delete(mset);THD_roilist_free(rl);THD_nested_foldplan_free(nested);THD_foldplan_free(plan);
     THD_sampleset_free_levels(class_label,nclass);free(class_id);free(group_id);
     mvpa_reg_free(&reg);free(feature_counts);free(lambda_grid);free(svm_grid);free(logistic_grid);THD_sampleset_free(ss);free(nuisance);THD_sampleset_free_levels(nuisance_names,nnuisance);
     return 0;
   }
   { const char *audit[]={save_predictions,save_fold_scores,save_confusion,save_summary_scores,save_tuning};int ai;
     for(ai=0;ai<5;ai++) if(audit[ai] && !strcmp(prefix,audit[ai]))
       ERROR_exit(PROGRAM_NAME ": audit cannot replace score output prefix '%s'",prefix);
   }
   if(coef_prefix){
     const char *suffix[]={".coefficients.1D",".preprocessing.1D"};int c;
     char *cf=malloc(strlen(coef_prefix)+32);
     if(!cf)ERROR_exit(PROGRAM_NAME ": cannot name coefficient audits");
     for(c=0;c<2;c++){
       sprintf(cf,"%s%s",coef_prefix,suffix[c]);
       if((save_predictions && !strcmp(cf,save_predictions)) ||
          (save_fold_scores && !strcmp(cf,save_fold_scores)) ||
          (save_confusion && !strcmp(cf,save_confusion)) ||
          (save_summary_scores && !strcmp(cf,save_summary_scores)) ||
          (save_tuning && !strcmp(cf,save_tuning)))
         ERROR_exit(PROGRAM_NAME ": coefficient and classification audit filenames must differ");
       if(THD_deathcon() && THD_is_file(cf))ERROR_exit(PROGRAM_NAME ": coefficient audit '%s' already exists",cf);
     }
     free(cf);
     {
       int c,combined=encoding && searchlight==NULL;
       char *name=malloc(strlen(coef_prefix)+64);
       THD_3dim_dataset *score_check=EDIT_empty_copy(mset),*coef_check=EDIT_empty_copy(mset);
       if(!name || !score_check || !coef_check)ERROR_exit(PROGRAM_NAME ": cannot check coefficient output paths");
       EDIT_dset_items(score_check,ADN_prefix,prefix,ADN_none);
       for(c=0;c<(combined?1:rl->nroi);c++){
         mvpa_coefficient_prefix(name,strlen(coef_prefix)+64,mset,coef_prefix,combined,rl->val[c]);
         EDIT_dset_items(coef_check,ADN_prefix,name,ADN_none);
         if(!strcmp(DSET_HEADNAME(score_check),DSET_HEADNAME(coef_check)))
           ERROR_exit(PROGRAM_NAME ": coefficient maps and held-out score dataset must differ");
         if(THD_deathcon() && THD_is_file(DSET_HEADNAME(coef_check)))
           ERROR_exit(PROGRAM_NAME ": descriptive dataset '%s' already exists",name);
       }
       DSET_delete(score_check);DSET_delete(coef_check);free(name);
     }
   }
   maxvox=THD_roilist_maxvox(rl) ;
   if( maxvox<1 || (size_t)ss->nsample>SIZE_MAX/sizeof(float)/(size_t)maxvox )
     ERROR_exit(PROGRAM_NAME ": sample-by-voxel workspace is too large") ;
#ifdef USE_OMP
   if( searchlight!=NULL || voxel_encoding ) nworker=omp_get_max_threads() ;
#endif
   if( nworker>rl->nroi ) nworker=rl->nroi ;
   for( ii=0 ; ii<ss->ndset ; ii++ ){
     DSET_load(ss->dset[ii]) ;
     if( !DSET_LOADED(ss->dset[ii]) )
       ERROR_exit(PROGRAM_NAME ": cannot load sample dataset '%s'",ss->dset_name[ii]) ;
   }

   if(transfer.column){
     FILE *fp=mvpa_open_audit(transfer.audit,"transfer design audit");
     mvpa_transfer_report(fp,&transfer,ss,plan,encoding?design_columns:target_column,class_label,nclass);
     {int failed=ferror(fp);if(fclose(fp))failed=1;if(failed)ERROR_exit(PROGRAM_NAME ": error writing transfer audit");}
     free(transfer.audit);transfer.audit=NULL;
   }
   if(save_model)frozen_model=mvpa_model_metadata(mset,rl,ss,plan,target_column,continuous?NULL:class_label,continuous?0:nclass,nuisance_names,nnuisance,searchlight!=NULL);
   if(frozen_model)frozen_model->estimator=logistic_class;
   if(pattern_prefix)mvpa_pattern_open(&pattern_audit,pattern_prefix,preprocess,nuisance_columns,!continuous);
   if(save_tuning){candidate_audit=mvpa_open_audit(save_tuning,"inner-CV candidate audit");if(lambda_per_response)mvpa_response_header(candidate_audit,tune_objective);else mvpa_candidate_header(candidate_audit,tune_objective,reg.config.method?2:continuous || ridge_class || logistic_class,npca_counts>0);}
   if(npca_counts)mvpa_pca_open(&pca_audit,prefix,save_pca_loadings,nested!=NULL,tune_objective,reg.config.method?2:continuous || ridge_class || logistic_class,lambda_per_response);
   if(nfeature_counts)mvpa_selection_open(&selection_audit,prefix,continuous,nested!=NULL,metric,nuisance_columns,reg.config.method?2:continuous || ridge_class || logistic_class,logistic_class?tune_objective:tune_requested,tune_objective,lambda_per_response);
   if(npca_counts && selection_audit.tuning && !lambda_per_response)fputs("# Losses condition on selected PCA count; all triples are in PREFIX.pca_tuning.1D.\n",selection_audit.tuning);
   if(train_only){
     mvpa_train_model(ss,plan,mset,rl,frozen_model,target_column,class_id,class_label,nclass,
       preprocess,logistic_class?logistic_cost:lambda,nested,logistic_class?logistic_grid:lambda_grid,logistic_class?logistic_ngrid:nlambda,tune_objective,nuisance,nnuisance,
       feature_counts,nfeature_counts,pca_counts,npca_counts,lambda_per_response,
       candidate_audit,&selection_audit,&pca_audit,prefix,quiet,progress_mode,logistic_class,logistic_tolerance,logistic_max_iter,metric);
     if(!THD_mvpa_model_write(save_model,frozen_model,THD_ok_overwrite(),err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
     if(!quiet)INFO_message(PROGRAM_NAME ": trained on all %d samples; wrote %s (no held-out scores)",ss->nsample,save_model);
     THD_mvpa_model_free(frozen_model);THD_roilist_free(rl);DSET_delete(mset);
     THD_nested_foldplan_free(nested);THD_foldplan_free(plan);THD_sampleset_free(ss);
     mvpa_selection_close(&selection_audit);mvpa_pca_close(&pca_audit);
     if(candidate_audit){int bad=ferror(candidate_audit);if(fclose(candidate_audit))bad=1;if(bad)ERROR_exit(PROGRAM_NAME ": error writing tuning audit");}
     free(class_id);THD_sampleset_free_levels(class_label,nclass);free(nuisance);THD_sampleset_free_levels(nuisance_names,nnuisance);
     free(feature_counts);free(pca_counts);free(lambda_grid);free(logistic_grid);free(group_id);mvpa_reg_free(&reg);return 0;
   }
   if(continuous){
     mvpa_continuous_analysis(ss,plan,mset,rl,encoding,encoding?design_columns:target_column,
       preprocess,group_id,lambda,metric,prefix,save_predictions,save_fold_scores,
       nworker,quiet,progress_mode,nested,lambda_grid,nlambda,inner_column,perm_set,permute_within,nuisance,nnuisance,nuisance_columns,feature_counts,nfeature_counts,&selection_audit,tune_requested,tune_objective,candidate_audit,pca_counts,npca_counts,&pca_audit,&pattern_audit,two_tables,lambda_per_response,frozen_model,&reg,argc,argv);
     if(frozen_model){if(!THD_mvpa_model_write(save_model,frozen_model,THD_ok_overwrite(),err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);THD_mvpa_model_free(frozen_model);frozen_model=NULL;}
     if(coef_prefix)mvpa_descriptive_coefficients(ss,mset,rl,encoding,searchlight!=NULL,
       encoding?design_columns:target_column,NULL,NULL,0,preprocess,group_id,coef_lambda,
       coef_prefix,quiet,argc,argv);
     DSET_delete(mset);THD_roilist_free(rl);THD_nested_foldplan_free(nested);THD_foldplan_free(plan);
     mvpa_selection_close(&selection_audit);free(feature_counts);mvpa_pca_close(&pca_audit);free(pca_counts);mvpa_pattern_close(&pattern_audit);
     if(candidate_audit && (ferror(candidate_audit) || fclose(candidate_audit)))ERROR_exit(PROGRAM_NAME ": error writing candidate audit");
     mvpa_reg_free(&reg);free(lambda_grid);
     THD_perm_set_free(perm_set);
     THD_sampleset_free(ss);free(nuisance);THD_sampleset_free_levels(nuisance_names,nnuisance);free(group_id);return 0;
   }

   x=(float **)calloc((size_t)nworker,sizeof(float *)) ;
   workspace=(THD_mvpa_workspace **)calloc((size_t)nworker,sizeof(*workspace)) ;
   if( perm_set!=NULL ){
     perm_class=(int **)calloc((size_t)nworker,sizeof(*perm_class)) ;
     perm_score=(float **)calloc((size_t)nworker,sizeof(*perm_score)) ;
     perm_max=(float **)calloc((size_t)nworker,sizeof(*perm_max)) ;
     perm_result=THD_perm_result_new(rl->nroi,perm_set->nperm) ;
     qval=(float *)malloc(sizeof(float)*(size_t)rl->nroi) ;
   }
   score=(float *)malloc(sizeof(float)*(size_t)rl->nroi) ;
   acc=(float *)malloc(sizeof(float)*(size_t)rl->nroi) ;
   bacc=(float *)malloc(sizeof(float)*(size_t)rl->nroi) ;
   nfeat=(float *)malloc(sizeof(float)*(size_t)rl->nroi) ;
   nfold=(float *)malloc(sizeof(float)*(size_t)rl->nroi) ;
   if( x==NULL || workspace==NULL || score==NULL || acc==NULL || bacc==NULL ||
       nfeat==NULL || nfold==NULL ||
       (perm_set!=NULL && (perm_class==NULL || perm_score==NULL || perm_max==NULL ||
                           perm_result==NULL || qval==NULL)) )
     ERROR_exit(PROGRAM_NAME ": out of memory allocating spatial analysis buffers") ;
   if( perm_result!=NULL ){
     perm_result->tail=PERM_TAIL_ONE ;
     for( ii=0 ; ii<perm_result->nperm ; ii++ ) perm_result->max_null[ii]=-FLT_MAX ;
   }
   for( ii=0 ; ii<nworker ; ii++ ){
     int ip ;
     x[ii]=(float *)malloc(sizeof(float)*(size_t)ss->nsample*maxvox) ;
     if( x[ii]==NULL ) ERROR_exit(PROGRAM_NAME ": out of memory allocating worker patterns") ;
     workspace[ii]=THD_mvpa_workspace_alloc(plan,maxvox,nclass,preprocess,
                                             group_id,err,sizeof(err)) ;
     if( workspace[ii]==NULL ) ERROR_exit(PROGRAM_NAME ": %s",err) ;
     if(ridge_class && !THD_mvpa_classification_set_ridge(workspace[ii],lambda,nested,
                                     lambda_grid,nlambda,err,sizeof(err)))
       ERROR_exit(PROGRAM_NAME ": %s",err);
     if(svm_class && !THD_mvpa_classification_set_svm(workspace[ii],svm_cost,svm_tolerance,
       svm_max_iter,metric,nested,svm_grid,svm_ngrid,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
     if(logistic_class && !THD_mvpa_classification_set_logistic(workspace[ii],logistic_cost,logistic_tolerance,
       logistic_max_iter,metric,nested,logistic_grid,logistic_ngrid,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
     if(lda_mode>=0 && !THD_mvpa_classification_set_lda(workspace[ii],lda_mode,shrinkage,
                          variance_floor,empirical_prior,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
     if(nuisance && !THD_preprocess_set_nuisance(ridge_class?workspace[ii]->ridge_cv->prep:workspace[ii]->prep,
                          nuisance,ss->nsample,nnuisance,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
     if(nfeature_counts && !THD_mvpa_classification_set_selection(workspace[ii],feature_counts,nfeature_counts,nested,metric,err,sizeof(err)))
       ERROR_exit(PROGRAM_NAME ": %s",err);
     if(npca_counts && !THD_mvpa_classification_set_pca(workspace[ii],pca_counts,npca_counts,nested,metric,pca_audit.loadings!=NULL,err,sizeof(err)))
       ERROR_exit(PROGRAM_NAME ": %s",err);
     if(nested && !THD_mvpa_classification_set_objective(workspace[ii],tune_objective,err,sizeof(err)))
       ERROR_exit(PROGRAM_NAME ": %s",err);
     if(pattern_prefix && !THD_mvpa_ridge_enable_patterns(workspace[ii]->ridge_cv,err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);
     if( perm_set!=NULL ){
       perm_class[ii]=(int *)malloc(sizeof(int)*(size_t)ss->nsample) ;
       perm_score[ii]=(float *)malloc(sizeof(float)*(size_t)perm_set->nperm) ;
       perm_max[ii]=(float *)malloc(sizeof(float)*(size_t)perm_set->nperm) ;
       if( perm_class[ii]==NULL || perm_score[ii]==NULL || perm_max[ii]==NULL )
         ERROR_exit(PROGRAM_NAME ": out of memory allocating permutation worker buffers") ;
       for( ip=0 ; ip<perm_set->nperm ; ip++ ) perm_max[ii][ip]=-FLT_MAX ;
     }
   }
   if( searchlight==NULL ){
     table_name=(char *)malloc(strlen(prefix)+9) ;
     if( table_name==NULL ) ERROR_exit(PROGRAM_NAME ": out of memory naming the ROI table") ;
     sprintf(table_name,"%s.roi.1D",prefix) ;
     if( save_predictions!=NULL && strcmp(save_predictions,table_name)==0 )
       ERROR_exit(PROGRAM_NAME ": -save_predictions cannot replace ROI table '%s'",table_name) ;
     if( save_fold_scores!=NULL && strcmp(save_fold_scores,table_name)==0 )
       ERROR_exit(PROGRAM_NAME ": -save_fold_scores cannot replace ROI table '%s'",table_name) ;
     if((save_confusion && !strcmp(save_confusion,table_name)) ||
        (save_summary_scores && !strcmp(save_summary_scores,table_name)))
       ERROR_exit(PROGRAM_NAME ": classification audit cannot replace ROI table '%s'",table_name);
   }
   /* Check generated tuning names before opening any classification audits. */
   if((ridge_class || svm_class || logistic_class) && nested){
     class_tunefile=malloc(strlen(prefix)+20);
     if(!class_tunefile) ERROR_exit(PROGRAM_NAME ": cannot name classification tuning audit");
     sprintf(class_tunefile,"%s.tuning.1D",prefix);
     if((save_predictions && !strcmp(save_predictions,class_tunefile)) ||
        (save_fold_scores && !strcmp(save_fold_scores,class_tunefile)) ||
        (save_confusion && !strcmp(save_confusion,class_tunefile)) ||
        (save_summary_scores && !strcmp(save_summary_scores,class_tunefile)))
       ERROR_exit(PROGRAM_NAME ": tuning and classification audit filenames must differ");
   }
   if(save_confusion || save_summary_scores){
     class_audit.truth=malloc(sizeof(int)*(size_t)ss->nsample);
     class_audit.prediction=malloc(sizeof(int)*(size_t)ss->nsample);
     class_audit.confusion=malloc(sizeof(int)*(size_t)nclass*nclass);
     class_audit.recall=malloc(sizeof(double)*(size_t)nclass);
     if(!class_audit.truth || !class_audit.prediction || !class_audit.confusion || !class_audit.recall)
       ERROR_exit(PROGRAM_NAME ": cannot allocate classification audit scratch");
     if(save_confusion){
       confusion_fp=mvpa_open_audit(save_confusion,"classification confusion audit");mvpa_nuisance_header(confusion_fp,nuisance_columns);mvpa_tune_header(confusion_fp,tune_requested,tune_objective);
       mvpa_write_audit_header(confusion_fp,"held-out classification confusion and recall",
                              target_column,class_label,nclass,searchlight!=NULL,surf_file!=NULL);
       fputs("# Descriptive only; original held-out predictions, no refits or population inference.\n"
             "# Subject is the resolved subject role; Stratum is -within_column (All when absent).\n"
             "# Untested rows are excluded; group BACC is mean recall over present true classes.\n",confusion_fp);
       fputs("# Counts: true-class rows, predicted-class columns. Absent true classes: NTrue=0, Recall=NA.\n",confusion_fp);
       fputs("# CVFoldID is zero-based for Fold rows (NA elsewhere); CVStratum is explicit for Fold/Stratum rows.\n",confusion_fp);
       fputs("# Columns: UnitType\tUnitID\tUnitLabel\tI\tJ\tK\tNFeat\tScope\tGroup\tTrueClassID\tTrueClass\tNTrue\tRecall",confusion_fp);
       for(ii=0;ii<nclass;ii++) fprintf(confusion_fp,"\tCountClass%d",ii);
       fputs("\tCVFoldID\tCVStratum\n",confusion_fp);
     }
     if(save_summary_scores){
       summary_fp=mvpa_open_audit(save_summary_scores,"classification summary audit");mvpa_nuisance_header(summary_fp,nuisance_columns);mvpa_tune_header(summary_fp,tune_requested,tune_objective);
       mvpa_write_audit_header(summary_fp,"held-out classification summaries",
                              target_column,class_label,nclass,searchlight!=NULL,surf_file!=NULL);
       fputs("# Descriptive only; original held-out predictions, no refits or population inference.\n"
             "# Subject is the resolved subject role; Stratum is -within_column (All when absent).\n"
             "# Pooled ACC=NCorrect/NTest; BACC=mean recall across true classes present in that group.\n"
             "# EqualSubject/EqualStratum: arithmetic mean of nonempty group ACC/BACC, one vote per group.\n"
             "# Equal-group NTest sums tested rows; NCorrect/NClassPresent are NA; NGroups counts nonempty groups.\n"
             "# Untested rows are excluded. Empty groups have NTest=0 and ACC/BACC=NA.\n",summary_fp);
       fputs("# CVFoldID is zero-based for Fold rows (NA elsewhere); CVStratum is explicit for Fold/Stratum rows.\n",summary_fp);
       fputs("# Columns: UnitType\tUnitID\tUnitLabel\tI\tJ\tK\tNFeat\tScope\tGroup\tNTest\tNCorrect\tNClassPresent\tNGroups\tACC\tBACC\tCVFoldID\tCVStratum\n",summary_fp);
     }
   }
   if(save_probability_scores){
     probability_fp=mvpa_open_audit(save_probability_scores,"held-out probability scores");
     mvpa_write_audit_header(probability_fp,"held-out probability scores",target_column,class_label,nclass,searchlight!=NULL,surf_file!=NULL);
     mvpa_logistic_header(probability_fp,logistic_tolerance,logistic_max_iter);
     fputs("# Means over tested rows only; empty groups=NA. Pooled, Fold, Subject and Stratum are descriptive; no population inference.\n"
           "# Columns: UnitType\tUnitID\tUnitLabel\tI\tJ\tK\tNFeat\tScope\tGroup\tNTest\tLogLoss\tBrier\tCVFoldID\tCVStratum\n",probability_fp);
   }
   if(save_calibration){
     calibration_fp=mvpa_open_audit(save_calibration,"held-out reliability bins");
     mvpa_write_audit_header(calibration_fp,"held-out reliability bins",target_column,class_label,nclass,searchlight!=NULL,surf_file!=NULL);
     mvpa_logistic_header(calibration_fp,logistic_tolerance,logistic_max_iter);
     fputs("# Per class, pooled held-out raw probabilities; 10 equal-width bins [lo,hi), final bin includes 1. Empty bins=NA.\n"
           "# Descriptive calibration assessment only, no calibration transform fitted; pooling across subjects/folds can hide differences.\n"
           "# Columns: UnitType\tUnitID\tUnitLabel\tI\tJ\tK\tNFeat\tClassID\tClass\tBin\tLower\tUpper\tNTest\tMeanProbability\tObservedFrequency\n",calibration_fp);
   }
   if( save_predictions!=NULL ){
     prediction_fp=mvpa_open_audit(save_predictions,"prediction audit") ;
     mvpa_nuisance_header(prediction_fp,nuisance_columns);mvpa_tune_header(prediction_fp,tune_requested,tune_objective);
     mvpa_write_audit_header(prediction_fp,"row-level held-out predictions",
                             target_column,class_label,nclass,searchlight!=NULL,
                             surf_file!=NULL) ;
     if(svm_class)fprintf(prediction_fp,"# Method: svm; loss=squared_hinge; intercept_penalized=1; C=%.17g; tuning=%s; tolerance=%.17g; max_iter=%d; decisions=raw_one_vs_rest_margins_not_probabilities\n",svm_cost,nested?"nested":"fixed",svm_tolerance,svm_max_iter);
     if(logistic_class)mvpa_logistic_header(prediction_fp,logistic_tolerance,logistic_max_iter);
     if(ridge_class)fputs("# Method: ridge; DecisionClass columns are 0/1-regression scores, not probabilities.\n",prediction_fp);
     if(lda_mode>=0)fprintf(prediction_fp,"# Method: %s; class_prior=%s; shrinkage=%.17g; relative_variance_floor=%.17g; decisions=centered_linear_discriminants_not_probabilities\n",
                           method,empirical_prior?"empirical":"equal",lda_mode==THD_LDA_DIAGONAL?0:shrinkage,variance_floor);
     fputs("# Columns: UnitType\tUnitID\tUnitLabel\tI\tJ\tK\tNFeat\tTableRow\tSample\tSubj\tRun\tItem\tInputFold\tCVFold\tCVStratum\tObserved\tPredicted\tCorrect",prediction_fp) ;
     for( ii=0 ; ii<nclass ; ii++ ) fprintf(prediction_fp,"\tDecisionClass%d",ii) ;
     if(logistic_class)for(ii=0;ii<nclass;ii++)fprintf(prediction_fp,"\tProbabilityClass%d",ii);
     if(two_tables)mvpa_source_fields(prediction_fp,ss,-1);
     fputc('\n',prediction_fp) ;
   }
   if( save_fold_scores!=NULL ){
     fold_fp=mvpa_open_audit(save_fold_scores,"fold-score audit") ;
     mvpa_nuisance_header(fold_fp,nuisance_columns);mvpa_tune_header(fold_fp,tune_requested,tune_objective);
     mvpa_write_audit_header(fold_fp,"per-fold classification scores",
                             target_column,class_label,nclass,searchlight!=NULL,
                             surf_file!=NULL) ;
     if(logistic_class)mvpa_logistic_header(fold_fp,logistic_tolerance,logistic_max_iter);
     if(svm_class)fprintf(fold_fp,"# Method: svm; loss=squared_hinge; intercept_penalized=1; tolerance=%.17g; max_iter=%d; inner_objective=1-%s; ties=smallest_C\n",svm_tolerance,svm_max_iter,tune_requested==THD_TUNE_DEFAULT?THD_mvpa_metric_name(metric):THD_mvpa_tune_objective_name(tune_objective));
     if(lda_mode>=0)fprintf(fold_fp,"# Method: %s; class_prior=%s; covariance_denominator=ntrain-nclass; relative_variance_floor=%.17g\n",
                           method,empirical_prior?"empirical":"equal",variance_floor);
     fputs("# Columns: UnitType\tUnitID\tUnitLabel\tI\tJ\tK\tNFeat\tCVFold\tStratum\tNTrain\tNTest\tACC\tBACC",fold_fp) ;
     if(svm_class)fputs("\tC\tInnerNFold\tInnerLoss\tSolverIterations\tSolverResidual",fold_fp);
     if(logistic_class)fputs("\tLambda\tInnerNFold\tInnerLoss\tSolverIterations\tSolverResidual\tLogLoss\tBrier",fold_fp);
     if(ridge_class)fputs("\tLambda\tInnerNFold\tInnerNValid\tInnerLoss",fold_fp);
     if(lda_mode>=0)fputs("\tShrinkage\tCovarianceScale\tVarianceFloor\tCovarianceForm",fold_fp);
     fputc('\n',fold_fp);
   }
   if((ridge_class || svm_class || logistic_class) && nested){
     class_tuning=mvpa_open_audit(class_tunefile,"classification tuning audit");mvpa_nuisance_header(class_tuning,nuisance_columns);
     if(npca_counts)fputs("# Losses condition on selected PCA count; all candidates are in PREFIX.pca_tuning.1D.\n",class_tuning);
     if(nfeature_counts)fputs("# Losses condition on the selected feature count; all pairs are in PREFIX.feature_tuning.1D.\n",class_tuning);
     mvpa_tune_header(class_tuning,tune_requested,tune_objective);
     if(logistic_class){
       fprintf(class_tuning,"# method=logistic; inner_group=%s; objective=%s; loss=minimized; ties=largest_lambda; aggregation=pooled_held_out_rows\n",inner_column,THD_mvpa_tune_objective_name(tune_objective));
       fputs("# Columns: Unit\tCVFold\tCVStratum\tInnerNFold\tLambda\tInnerLoss\tSelected\n",class_tuning);
     }else if(svm_class){
       fprintf(class_tuning,"# method=svm; inner_group=%s; objective=1-%s; ties=smallest_C; intercept_penalized=1\n",inner_column,tune_requested==THD_TUNE_DEFAULT?THD_mvpa_metric_name(metric):THD_mvpa_tune_objective_name(tune_objective));
       fputs("# Columns: Unit\tCVFold\tCVStratum\tInnerNFold\tC\tInnerLoss\tSelected\n",class_tuning);
     }else{
     if(tune_requested==THD_TUNE_DEFAULT)fprintf(class_tuning,"# method=ridge; coding=one_hot_0_1; inner_group=%s; objective=mean_class_SSE_over_training_mean_baseline_SSE; ties=largest_lambda\n",inner_column);
     else fprintf(class_tuning,"# method=ridge; coding=one_hot_0_1; inner_group=%s; objective=%s; ties=largest_lambda\n",inner_column,THD_mvpa_tune_objective_name(tune_objective));
     fputs("# Columns: Unit\tCVFold\tCVStratum\tInnerNFold\tInnerNValid\tLambda\tInnerLoss\tSelected\n",class_tuning);
     }
   }
   if( searchlight!=NULL && !quiet )
     INFO_message(PROGRAM_NAME ": %d centers, at most %d features, %d worker%s",
                  rl->nroi,maxvox,nworker,nworker==1?"":"s") ;

   mvpa_progress_init(&progress,progress_mode,quiet,"Classification",rl->nroi,"locations");
   AFNI_OMP_START ;
#ifdef USE_OMP
#pragma omp parallel for if(searchlight != NULL && rl->nroi > 1) \
                         num_threads(nworker) schedule(dynamic,1) ordered
#endif
   for( rr=0 ; rr<rl->nroi ; rr++ ){
     int tid=0,ok ; char localerr[1024] ;
#ifdef USE_OMP
     tid=omp_get_thread_num() ;
#endif
     ok=THD_roi_samples(ss,NULL,ss->nsample,rl->vox+rr,x[tid],
                        localerr,sizeof(localerr)) ;
     if( ok && perm_set!=NULL )
       ok=THD_mvpa_run_classification_permutations(plan,distance,x[tid],rl->vox[rr].nar,
                                             class_id,group_id,metric,perm_set,
                                             perm_class[tid],perm_score[tid],
                                             workspace[tid],localerr,sizeof(localerr)) ;
     else if( ok )
       ok=THD_mvpa_run_classification_cv(plan,distance,x[tid],rl->vox[rr].nar,
                                   class_id,group_id,workspace[tid],
                                   localerr,sizeof(localerr)) ;
     if( !ok ){
#ifdef USE_OMP
#pragma omp critical(mvpa_searchlight_error)
#endif
       {
         if( !failed ){
           failed=1 ;
           snprintf(runerr,sizeof(runerr),"%s %d: %.900s",
                    surf_file?"surface searchlight mesh node":
                    searchlight?"searchlight center voxel":"ROI",
                    rl->val[rr],localerr) ;
         }
       }
     } else {
       acc[rr]=(float)workspace[tid]->score.accuracy ;
       bacc[rr]=(float)workspace[tid]->score.balanced_accuracy ;
       score[rr]=(metric==THD_METRIC_ACCURACY)?acc[rr]:bacc[rr] ;
       nfeat[rr]=(float)rl->vox[rr].nar ; nfold[rr]=(float)plan->nfold ;
       if( perm_result!=NULL ){
         double mean=0.0 ; int ip ;
         perm_result->stat[rr]=score[rr] ;
         for( ip=0 ; ip<perm_set->nperm ; ip++ ){
           float vv=perm_score[tid][ip] ; mean+=vv ;
           if( vv>=score[rr] ) perm_result->cnt_unc[rr]++ ;
           if( vv>perm_max[tid][ip] ) perm_max[tid][ip]=vv ;
         }
         perm_result->effect[rr]=score[rr]-(float)(mean/(double)perm_set->nperm) ;
       }
     }
#ifdef USE_OMP
#pragma omp ordered
#endif
     {
       if( ok && prediction_fp!=NULL )
         mvpa_write_prediction_rows(prediction_fp,mset,rl,rr,searchlight!=NULL,
                                    surf_file!=NULL,
                                    ss,plan,class_id,class_label,workspace[tid],two_tables) ;
       if( ok && fold_fp!=NULL )
         mvpa_write_fold_rows(fold_fp,mset,rl,rr,searchlight!=NULL,
                              surf_file!=NULL,plan,workspace[tid]) ;
       if(ok && (confusion_fp || summary_fp))
         mvpa_write_class_audits(confusion_fp,summary_fp,mset,rl,rr,searchlight!=NULL,
                                surf_file!=NULL,ss,plan,class_id,class_label,
                                workspace[tid],&class_audit);
       if(ok && (probability_fp || calibration_fp))mvpa_write_probability_audits(probability_fp,calibration_fp,mset,rl,rr,searchlight!=NULL,surf_file!=NULL,ss,plan,class_id,class_label,workspace[tid]);
       if(ok && frozen_model){frozen_model->unit[rr].fit=logistic_class?THD_mvpa_logistic_freeze(workspace[tid],localerr,sizeof(localerr)):THD_mvpa_ridge_freeze(workspace[tid]->ridge_cv,localerr,sizeof(localerr));if(!frozen_model->unit[rr].fit)ERROR_exit(PROGRAM_NAME ": %s",localerr);}
       if(ok && pattern_prefix)mvpa_pattern_write(&pattern_audit,workspace[tid]->ridge_cv,mset,rl,rr,class_label,nclass,argc,argv);
       if(ok && candidate_audit)mvpa_candidate_write(candidate_audit,plan,rl->val[rr],rl->vox[rr].nar,
         tune_objective,workspace[tid]->ridge_cv,workspace[tid]);
       if(ok && npca_counts){
         THD_mvpa_workspace *w=workspace[tid];THD_mvpa_ridge_workspace *rw=w->ridge_cv;
         mvpa_pca_write(&pca_audit,rw?rw->pca:w->pca,plan,mset,rl,rr,NULL,rw?rw->selection:w->selection);
         if(pca_audit.tuning)mvpa_candidate_write(pca_audit.tuning,plan,rl->val[rr],rl->vox[rr].nar,tune_objective,rw,w);
       }
       if(ok && nfeature_counts){
         THD_mvpa_workspace *w=workspace[tid];THD_mvpa_ridge_workspace *rw=w->ridge_cv;
         mvpa_selection_write(&selection_audit,rw?rw->selection:w->selection,plan,mset,rl,rr,NULL,rl->vox[rr].nar,
                              rw?lambda_grid:logistic_class?logistic_grid:svm_class?svm_grid:NULL,rw?rw->selected_lambda:(svm_class || logistic_class)?w->class_selected:NULL,
                              rw?lambda:logistic_class?logistic_cost:svm_class?svm_cost:0);
       }
       if(ok && class_tuning && (svm_class || logistic_class)){
         int f,l;const THD_mvpa_workspace *sw=workspace[tid];
         for(f=0;f<plan->nfold;f++)for(l=0;l<sw->class_ngrid;l++){
           fprintf(class_tuning,"%d\t",rl->val[rr]);mvpa_1D_text(class_tuning,plan->name[f]);
           fputc('\t',class_tuning);mvpa_1D_text(class_tuning,plan->stratum_label[plan->fold_stratum[f]]);
           fprintf(class_tuning,"\t%d\t%.17g\t%.17g\t%d\n",nested->inner[f]->nfold,
             sw->class_grid[l],sw->class_loss[(size_t)f*sw->class_ngrid+l],sw->class_grid[l]==sw->class_selected[f]);
         }
       }
       if(ok && class_tuning && ridge_class){
         int f,l;const THD_mvpa_ridge_workspace *rw=workspace[tid]->ridge_cv;
         for(f=0;f<plan->nfold;f++)for(l=0;l<nlambda;l++){
           fprintf(class_tuning,"%d\t",rl->val[rr]);mvpa_1D_text(class_tuning,plan->name[f]);
           fputc('\t',class_tuning);mvpa_1D_text(class_tuning,plan->stratum_label[plan->fold_stratum[f]]);
           fprintf(class_tuning,"\t%d\t%d\t%.17g\t%.17g\t%d\n",nested->inner[f]->nfold,
             rw->inner_nvalid[f],lambda_grid[l],rw->inner_loss[(size_t)f*nlambda+l],lambda_grid[l]==rw->selected_lambda[f]);
         }
       }
     }
     if(ok)mvpa_progress_advance(&progress);
   }
   AFNI_OMP_END ;
   mvpa_progress_finish(&progress);
   if( prediction_fp!=NULL ){
     if( ferror(prediction_fp) ) io_failed=1 ;
     if( fclose(prediction_fp)!=0 ) io_failed=1 ;
     prediction_fp=NULL ;
   }
   if( fold_fp!=NULL ){
     if( ferror(fold_fp) ) io_failed=1 ;
     if( fclose(fold_fp)!=0 ) io_failed=1 ;
     fold_fp=NULL ;
   }
   if(class_tuning && (ferror(class_tuning) || fclose(class_tuning)))io_failed=1;
   if(candidate_audit && (ferror(candidate_audit) || fclose(candidate_audit)))io_failed=1;
   if(confusion_fp){
     if(ferror(confusion_fp)) io_failed=1;
     if(fclose(confusion_fp)) io_failed=1;
   }
   if(summary_fp){
     if(ferror(summary_fp)) io_failed=1;
     if(fclose(summary_fp)) io_failed=1;
   }
   if(probability_fp){if(ferror(probability_fp))io_failed=1;if(fclose(probability_fp))io_failed=1;}
   if(calibration_fp){if(ferror(calibration_fp))io_failed=1;if(fclose(calibration_fp))io_failed=1;}
   free(class_audit.truth);free(class_audit.prediction);
   free(class_audit.confusion);free(class_audit.recall);
   if( failed ) ERROR_exit(PROGRAM_NAME ": %s",runerr) ;
   if( io_failed ) ERROR_exit(PROGRAM_NAME ": error writing classification audit table") ;

   if( perm_result!=NULL ){
     int ip,ww ;
     for( ww=0 ; ww<nworker ; ww++ )
       THD_max_accum(perm_set->nperm,perm_result->max_null,perm_max[ww]) ;
     for( ip=0 ; ip<perm_set->nperm ; ip++ )
       if( perm_result->max_null[ip]==-FLT_MAX ) perm_result->max_null[ip]=0.0f ;
     THD_perm_result_finish(perm_result,NULL) ;
     THD_bh_fdr_masked(rl->nroi,perm_result->p_unc,NULL,qval) ;
   }

   oset=mvpa_make_roi_map(mset,rl,prefix,score,nfeat,nfold,perm_result,qval,
                          metric==THD_METRIC_ACCURACY?"ACC":"BACC",argc,argv) ;
   if( searchlight==NULL ){
     mvpa_write_roi_table(table_name,rl,score,acc,bacc,nfeat,nfold,
                          THD_mvpa_metric_name(metric),target_column,class_label,nclass,
                          perm_result,qval,permute_within,nuisance_columns) ;
   }
   if( DSET_write(oset)!=True ) ERROR_exit(PROGRAM_NAME ": failed writing output dataset") ;
   if( !quiet ){
     INFO_message(PROGRAM_NAME ": classified %d samples in %d %s%s with %d folds",
                  ss->nsample,rl->nroi,
                  surf_file?"surface searchlight mesh node":
                  searchlight?"searchlight center":"ROI",
                  rl->nroi==1?"":"s",plan->nfold) ;
     if( table_name!=NULL ) INFO_message(PROGRAM_NAME ": wrote ROI table %s",table_name) ;
     if( save_predictions!=NULL ) INFO_message(PROGRAM_NAME ": wrote predictions %s",save_predictions) ;
     if( save_fold_scores!=NULL ) INFO_message(PROGRAM_NAME ": wrote fold scores %s",save_fold_scores) ;
     if(save_confusion) INFO_message(PROGRAM_NAME ": wrote confusion/recall %s",save_confusion);
     if(save_summary_scores) INFO_message(PROGRAM_NAME ": wrote classification summaries %s",save_summary_scores);
     if( perm_result!=NULL )
       INFO_message(PROGRAM_NAME ": inference family: %d spatial units x %d synchronized relabelings",
                    rl->nroi,perm_result->nperm) ;
     INFO_message(PROGRAM_NAME ": wrote dataset %s",DSET_BRIKNAME(oset)) ;
   }

   if(frozen_model){if(!THD_mvpa_model_write(save_model,frozen_model,THD_ok_overwrite(),err,sizeof(err)))ERROR_exit(PROGRAM_NAME ": %s",err);THD_mvpa_model_free(frozen_model);frozen_model=NULL;}
   if(coef_prefix)mvpa_descriptive_coefficients(ss,mset,rl,0,searchlight!=NULL,
     target_column,class_id,class_label,nclass,preprocess,group_id,coef_lambda,
     coef_prefix,quiet,argc,argv);
   DSET_delete(oset) ; DSET_delete(mset) ; THD_roilist_free(rl) ;
   for( ii=0 ; ii<nworker ; ii++ ){
     THD_mvpa_workspace_free(workspace[ii]) ; free(x[ii]) ;
     if( perm_set!=NULL ){
       free(perm_class[ii]) ; free(perm_score[ii]) ; free(perm_max[ii]) ;
     }
   }
   free(workspace) ; free(x) ; THD_nested_foldplan_free(nested);THD_foldplan_free(plan) ;
   free(perm_class) ; free(perm_score) ; free(perm_max) ;
   THD_perm_result_free(perm_result) ; THD_perm_set_free(perm_set) ;
   THD_sampleset_free_levels(class_label,nclass) ; free(class_id) ; free(group_id) ;
   THD_sampleset_free(ss) ; free(nuisance);THD_sampleset_free_levels(nuisance_names,nnuisance); free(score) ; free(acc) ; free(bacc) ;
   free(nfeat) ; free(nfold) ; free(qval) ; free(table_name) ;
   mvpa_selection_close(&selection_audit);free(feature_counts);mvpa_pca_close(&pca_audit);free(pca_counts);mvpa_pattern_close(&pattern_audit);
   free(lambda_grid);free(svm_grid);free(logistic_grid);free(class_tunefile);
   return 0 ;
}
