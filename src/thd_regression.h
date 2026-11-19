#ifndef THD_REGRESSION_H
#define THD_REGRESSION_H

/* Dataset-independent continuous estimators shared by MVPA and Picard.
   Legacy float I/O, double accumulation, centering and penalty units are
   preserved. No implicit preprocessing beyond each documented fit contract.
   Allocators copy configuration arrays; fit candidate indices are zero-based.
   Ridge adds lambda directly to X'X (lambda = n*alpha for EN with rho=0). */

#define THD_RIDGE_AUTO 0
#define THD_RIDGE_PRIMAL 1
#define THD_RIDGE_DUAL 2

typedef struct THD_ridge_workspace THD_ridge_workspace;

/* Compact row-major matrices; positive lambda penalizes slopes only. Fit
   learns predictor/response means from training rows. AUTO factors the smaller
   of X'X and XX'. Factor capacity is min(max_sample,max_feature); a forced
   form must fit that capacity. Repeated fits/predictions allocate no buffers.
 */
extern THD_ridge_workspace *THD_ridge_alloc(int max_sample, int max_feature,
                                            int max_output, char *err,
                                            int nerr);
extern int THD_ridge_fit(THD_ridge_workspace *w, const float *x, const float *y,
                         int n, int p, int q, double lambda, int form,
                         char *err, int nerr);
/* One positive penalty per response. Means are shared; responses with equal
   penalties share a factorization. Up to q factors, no extra fit allocations.
   Coefficient/prediction layout and intercept convention match THD_ridge_fit.
 */
extern int THD_ridge_fit_response_lambda(THD_ridge_workspace *w, const float *x,
                                         const float *y, int n, int p, int q,
                                         const double *lambda, int form,
                                         char *err, int nerr);
extern int THD_ridge_predict(const THD_ridge_workspace *w, const float *x,
                             int n, float *prediction, char *err, int nerr);
/* Export slopes [feature][response] and unpenalized intercept [response]
   in the exact predictor space supplied to fit: y = intercept + X*coef.
   Caller provides p*q and q doubles. No preprocessing inversion is implied. */
extern int THD_ridge_coefficients(const THD_ridge_workspace *w, double *coef,
                                  double *intercept, char *err, int nerr);
extern void THD_ridge_free(THD_ridge_workspace *w);

#define THD_REG_ELASTIC_NET 1
#define THD_REG_BANDED_RIDGE 2
#define THD_REG_PLS 3
#define THD_REG_SVR 4
typedef struct {
   int method, ncandidate, ngroup, nfeature, max_iterations;
   double tolerance;
   const double
       *parameter; /* [candidate][2], alpha/rho, components/0, C/epsilon */
   const int *feature_group;    /* banded only, [nfeature], IDs 0..ngroup-1 */
   const double *group_penalty; /* banded only, [candidate][ngroup] */
} THD_regression_config;
typedef struct THD_regression THD_regression;
typedef struct {
   int iterations, components, nonzero;
   double residual, objective;
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
   Failed fits invalidate predictions; all repeated fits reuse allocated
   scratch. */
extern THD_regression *THD_regression_alloc(int n, int p, int q,
                                            const THD_regression_config *config,
                                            char *err, int nerr);
extern int THD_regression_fit(THD_regression *m, const float *x, const float *y,
                              int n, int p, int q, int candidate, char *err,
                              int nerr);
extern int THD_regression_predict(const THD_regression *m, const float *x,
                                  int n, float *out, char *err, int nerr);
extern int THD_regression_coefficients(const THD_regression *m, double *coef,
                                       double *intercept, char *err, int nerr);
extern const THD_regression_diagnostic *
THD_regression_diagnostics(const THD_regression *m);
extern void THD_regression_free(THD_regression *m);

/* Borrowed read-only fitted state. Arrays use active dimensions: xm[p],
   ym[q], coef[p][q], and group[p] only for banded ridge (otherwise NULL,
   ngroup=0). ym is the training response mean, including for SVR where it
   differs from the fitted intercept. xm is the centering offset used by the
   fit: the training predictor mean except for uncentered SVR, where it is zero.
   These doubles preserve centered-score precision for activation patterns.
   Views expire on the next fit attempt (successful or failed) or free.
   Accessors return 0 for NULL/unfitted models or NULL output, and clear a
   non-NULL output on failure. Copy arrays if ownership beyond a fit is needed.
   Use coefficient export to obtain original-input-space intercepts. */
typedef struct {
   int n, p, q, ngroup;
   const double *xm, *ym, *coef;
   const int *group;
} THD_regression_state;
extern int THD_ridge_get_state(const THD_ridge_workspace *w,
                               THD_regression_state *state, char *err,
                               int nerr);
extern int THD_regression_get_state(const THD_regression *m,
                                    THD_regression_state *state, char *err,
                                    int nerr);

/* Prepared double-precision fractional ridge. No centering or intercept is
   added: supply effective X and Y after nuisance projection. Preserve supplied
   coefficient units. Rank retains s > rank_tol*smax; rank_tol in (0,1).
   The reference is the minimum-norm LS solution in the retained singular span.
   Fraction 1 gives that reference; 0 gives zero slopes and infinite penalty.
   A zero reference gives zero slopes, lambda=0, achieved_fraction=NaN and
   ZERO_REFERENCE status for every requested fraction. Lambda is normalized:
   SSE/(2*n) + lambda*||beta||^2/2; penalty=n*lambda is added to X'X.
   An immutable design owns X and its SVD. Worker scratch borrows the design,
   which must outlive workers. Fits allocate no memory; failed fits invalidate
   results. Active q determines compact [feature][response] strides.
   These APIs do not tune/select a fraction, whiten, or standardize predictors.
 */
#define THD_FRACTION_OK 0
#define THD_FRACTION_ZERO_REFERENCE 1
#define THD_FRACTION_ZERO_ENDPOINT 2
#define THD_FRACTION_RANK_TOL 1.e-10
#define THD_FRACTION_TOL 1.e-10

typedef struct THD_regression_design THD_regression_design;
typedef struct THD_regression_workspace THD_regression_workspace;
typedef struct {
   double fraction, achieved_fraction, lambda, penalty, reference_norm;
   int rank, status, iterations;
} THD_fraction_diagnostic;
typedef struct {
   int n, p, q;
   const double *coef;
   const THD_fraction_diagnostic *diagnostic;
} THD_fraction_result;
extern THD_regression_design *
THD_regression_design_prepare(const double *x, int n, int p, double rank_tol,
                              char *err, int nerr);
extern int THD_regression_design_rank(const THD_regression_design *design);
extern void THD_regression_design_free(THD_regression_design *design);
extern THD_regression_workspace *
THD_regression_workspace_alloc(const THD_regression_design *design,
                               int max_response, char *err, int nerr);
extern void THD_regression_workspace_free(THD_regression_workspace *w);
/* fraction[q] allows selected settings per response; scalar helper fills
   the same fraction for every response. Results expire on fit/free. */
extern int THD_regression_fit_fractions(THD_regression_workspace *w,
                                        const double *y, int q,
                                        const double *fraction, char *err,
                                        int nerr);
extern int THD_regression_fit_fraction(THD_regression_workspace *w,
                                       const double *y, int q, double fraction,
                                       char *err, int nerr);
extern const THD_fraction_result *
THD_regression_fraction_result(const THD_regression_workspace *w);
/* Streams a candidate at a time; callback must copy anything retained.
   Callback rejection invalidates the fit and aborts the path. */
typedef int (*THD_fraction_sink)(void *context, int candidate,
                                 const THD_fraction_result *result, char *err,
                                 int nerr);
extern int THD_regression_fraction_path(THD_regression_workspace *w,
                                        const double *y, int q,
                                        const double *fractions, int count,
                                        THD_fraction_sink sink, void *context,
                                        char *err, int nerr);

/* Prepared elastic net: SSE/(2*n) + lambda *
   (rho*|theta|_1 + (1-rho)*||theta||^2/2). No implicit intercept or centering.
   X must already be projected against nuisance columns. Optional scaling
   divides each effective column by its RMS sqrt(sum(X^2)/n); zero columns
   keep scale 1. theta is in this scaled space, beta=theta/scale is exported
   in supplied units. Y is never scaled. Preparation copies X/configuration.
   A plan is immutable; workers borrow it and own all mutable scratch.
   Lambda must be finite and strictly positive; rho is in [0,1]. For rho=0,
   this is ridge with diagonal penalty n*lambda in the effective design.
   Convergence requires absolute max KKT <= tolerance, checked every sweep.
   A failed attempt invalidates results AND warm-start state. Fits allocate
   no memory. Diagnostics/objective refer to the scaled optimization space. */
typedef struct {
   double rho, tolerance;
   int max_iterations, standardize;
} THD_elastic_config;
typedef struct THD_elastic_design THD_elastic_design;
typedef struct THD_elastic_workspace THD_elastic_workspace;
typedef struct {
   double lambda, rho, lambda_max;
   THD_regression_diagnostic solver;
} THD_elastic_diagnostic;
typedef struct {
   int n, p, q;
   const double *coef;
   const THD_elastic_diagnostic *diagnostic;
} THD_elastic_result;
THD_elastic_design *THD_elastic_prepare(const double *x, int n, int p,
                                        const THD_elastic_config *config,
                                        char *err, int nerr);
void THD_elastic_design_free(THD_elastic_design *d);
const double *THD_elastic_scales(const THD_elastic_design *d);
THD_elastic_workspace *THD_elastic_workspace_alloc(const THD_elastic_design *d,
                                                   int max_response, char *err,
                                                   int nerr);
void THD_elastic_workspace_free(THD_elastic_workspace *w);
/* Warm starts require a previous successful fit with the same active q.
   Residuals are reconstructed from the supplied Y, never reused blindly. */
int THD_elastic_fit(THD_elastic_workspace *w, const double *y, int q,
                    const double *lambda, int warm, char *err, int nerr);
const THD_elastic_result *
THD_elastic_get_result(const THD_elastic_workspace *w);
/* Positive strictly decreasing lambdas, one common grid across responses.
   First fit is cold, subsequent fits warm. Callback data expires next step;
   callback rejection invalidates both results and warm-start state. */
typedef int (*THD_elastic_sink)(void *context, int candidate,
                                const THD_elastic_result *result, char *err,
                                int nerr);
int THD_elastic_path(THD_elastic_workspace *w, const double *y, int q,
                     const double *lambda, int count, THD_elastic_sink sink,
                     void *context, char *err, int nerr);

/* Prepared banded ridge: minimize SSE/(2*n) + sum_j lambda[group[j]]*B_j^2/2.
   No centering/scaling is added. Group IDs are 0..ngroup-1; unused groups are
   allowed in training folds. All penalties are finite and strictly positive.
   Designs copy X, groups and the candidate-by-group penalty table. */
typedef struct {
   int ngroup, ncandidate;
   const int *group;      /* [p], penalized-column order */
   const double *penalty; /* [ncandidate][ngroup], normalized */
} THD_banded_config;
typedef struct THD_banded_design THD_banded_design;
typedef struct THD_banded_workspace THD_banded_workspace;
typedef struct {
   int candidate;
   double objective, gradient; /* normalized objective/max gradient */
} THD_banded_diagnostic;
typedef struct {
   int p, q;
   const double *coef;
   const THD_banded_diagnostic *diagnostic;
} THD_banded_result;
THD_banded_design *THD_banded_prepare(const double *x, int n, int p,
                                      const THD_banded_config *config,
                                      char *err, int nerr);
void THD_banded_design_free(THD_banded_design *d);
THD_banded_workspace *THD_banded_workspace_alloc(const THD_banded_design *d,
                                                 int max_response, char *err,
                                                 int nerr);
void THD_banded_workspace_free(THD_banded_workspace *w);
/* Candidate indices are doubles to share Picard's path interface. Each must
   be an exact integer in range. Groups of responses selecting the same index
   share one factorization. No allocations during fitting. Results expire on
   any next fit attempt or workspace destruction. */
int THD_banded_fit(THD_banded_workspace *w, const double *y, int q,
                   const double *candidate, char *err, int nerr);
const THD_banded_result *THD_banded_get_result(const THD_banded_workspace *w);

#endif
