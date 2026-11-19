/* Shared regression mathematics for MVPA and Picard.

   Reading map:
     1. Cholesky primitive and legacy multiresponse ridge.
     2. Continuous estimators: elastic net, banded ridge, SVR and PLS.
     3. Prepared double-precision elastic-net and banded-ridge plans.
     4. Thin SVD and fractional-ridge paths (the former SVD source file).

   A prepared design owns reusable data/factors; each workspace borrows that
   design and owns changing per-fit arrays. Legacy THD_regression instead owns
   its training data and solver state together. Public results are borrowed
   until another fit; free functions release owned storage only.

   Arrays are row-major except the temporary column-major EISPACK adapter.
   n = observations, p = predictors, q = responses. Penalty conventions differ
   between the legacy interface and prepared interfaces; thd_regression.h
   states the objective/units for each. Do not interchange penalty numbers.
   Terminology: OLS is ordinary least squares; SVD is singular value
   decomposition. KKT (Karush-Kuhn-Tucker) conditions are the first-order
   optimality checks used by the convex solvers. A small coefficient update
   alone does not prove that those conditions hold.
   This file contains no dataset reading or cross-validation policy.
*/
#include "thd_regression.h"
#include "thd_regression_internal.h"

#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* EISPACK/f2c defines legacy macros; include it after the C headers. */
#include "eispack.h"

static int reg_error(char *err, int nerr, const char *fmt, ...)
{
   va_list ap;
   if (err != NULL && nerr > 0) {
      va_start(ap, fmt);
      vsnprintf(err, (size_t)nerr, fmt, ap);
      va_end(ap);
   }
   return 0;
}

/* Factor the lower triangle of a symmetric positive-definite matrix as L L^T.
   First solve L z = b (forward), then L^T x = z (backward). Multiple RHS
   columns share the factorization. Both a and b are overwritten; no explicit
   inverse is formed. A nonpositive pivot means the requested solve is invalid.
 */
int THD_regression_spd_solve(double *a, double *b, int d, int q)
{
   int i, j, k, o;
   for (i = 0; i < d; i++)
      for (j = 0; j <= i; j++) {
         double v = a[(size_t)i * d + j];
         for (k = 0; k < j; k++)
            v -= a[(size_t)i * d + k] * a[(size_t)j * d + k];
         if (i == j) {
            if (!isfinite(v) || v <= 0)
               return 0;
            a[(size_t)i * d + j] = sqrt(v);
         } else
            a[(size_t)i * d + j] = v / a[(size_t)j * d + j];
      }
   for (i = 0; i < d; i++)
      for (o = 0; o < q; o++) {
         double v = b[(size_t)i * q + o];
         for (k = 0; k < i; k++)
            v -= a[(size_t)i * d + k] * b[(size_t)k * q + o];
         b[(size_t)i * q + o] = v / a[(size_t)i * d + i];
      }
   for (i = d - 1; i >= 0; i--)
      for (o = 0; o < q; o++) {
         double v = b[(size_t)i * q + o];
         for (k = i + 1; k < d; k++)
            v -= a[(size_t)k * d + i] * b[(size_t)k * q + o];
         b[(size_t)i * q + o] = v / a[(size_t)i * d + i];
      }
   return 1;
}

/* 1. CHOLESKY AND LEGACY RIDGE (allocation, fitting, copied results). */
struct THD_ridge_workspace {
   int maxn, maxp, maxq, n, p, q, fitted;
   double *factor, *rhs, *coef, *xm, *ym;
};

void THD_ridge_free(THD_ridge_workspace *w)
{
   if (!w)
      return;
   free(w->factor);
   free(w->rhs);
   free(w->coef);
   free(w->xm);
   free(w->ym);
   free(w);
}

/* Allocate capacity, not a fitted model. AUTO ridge solves in the smaller
   of predictor space (p by p) and observation space (n by n), which controls
   factor memory. Active dimensions can shrink between calls without
   reallocating. */
THD_ridge_workspace *THD_ridge_alloc(int n, int p, int q, char *err, int nerr)
{
   THD_ridge_workspace *w;
   int d;
   if (n < 1 || p < 1 || q < 1) {
      reg_error(err, nerr, "ridge dimensions must be positive");
      return NULL;
   }
   d = n < p ? n : p;
   if ((size_t)d > SIZE_MAX / sizeof(double) / (size_t)d ||
       (size_t)p > SIZE_MAX / sizeof(double) / (size_t)q) {
      reg_error(err, nerr, "ridge dimensions are too large");
      return NULL;
   }
   w = calloc(1, sizeof(*w));
   if (!w) {
      reg_error(err, nerr, "out of memory allocating ridge");
      return NULL;
   }
   w->maxn = n;
   w->maxp = p;
   w->maxq = q;
   w->factor = malloc(sizeof(double) * (size_t)d * d);
   w->rhs = malloc(sizeof(double) * (size_t)d * q);
   w->coef = malloc(sizeof(double) * (size_t)p * q);
   w->xm = malloc(sizeof(double) * (size_t)p);
   w->ym = malloc(sizeof(double) * (size_t)q);
   if (!w->factor || !w->rhs || !w->coef || !w->xm || !w->ym) {
      THD_ridge_free(w);
      reg_error(err, nerr, "out of memory allocating ridge");
      return NULL;
   }
   return w;
}

/* Legacy ridge centers X and Y to recover an unpenalized intercept later.
   The primal solve uses X^T X + lambda I; the dual solve uses X X^T + lambda I
   and maps the solution back through X^T. Response-specific penalties require
   separate factors; a common penalty lets all responses share one factor.
   Clear fitted first so a rejected input cannot expose a previous fit as new.
 */
static int ridge_fit_penalties(THD_ridge_workspace *w, const float *x,
                               const float *y, int n, int p, int q,
                               double lambda, const double *penalties, int form,
                               char *err, int nerr)
{
   int i, j, k, t, o, d, dual, g;
   double *a, *b;
   if (w)
      w->fitted = 0;
   if (!w || !x || !y || n < 1 || n > w->maxn || p < 1 || p > w->maxp ||
       q < 1 || q > w->maxq || !isfinite(lambda) || lambda <= 0 || form < 0 ||
       form > 2)
      return reg_error(err, nerr,
                       "invalid ridge dimensions, form, or positive lambda");
   if (penalties)
      for (o = 0; o < q; o++)
         if (!isfinite(penalties[o]) || penalties[o] <= 0)
            return reg_error(
                err, nerr,
                "response ridge penalties must be finite and positive");
   dual = form == THD_RIDGE_DUAL || (form == THD_RIDGE_AUTO && p > n);
   d = dual ? n : p;
   if (d > (w->maxn < w->maxp ? w->maxn : w->maxp))
      return reg_error(
          err, nerr, "forced ridge form exceeds the allocated factor capacity");
   a = w->factor;
   b = w->rhs;
   for (j = 0; j < p; j++) {
      double s = 0;
      for (i = 0; i < n; i++) {
         double v = x[(size_t)i * p + j];
         if (!isfinite(v))
            return reg_error(err, nerr, "ridge predictor is not finite");
         s += v;
      }
      w->xm[j] = s / n;
   }
   for (o = 0; o < q; o++) {
      double s = 0;
      for (i = 0; i < n; i++) {
         double v = y[(size_t)i * q + o];
         if (!isfinite(v))
            return reg_error(err, nerr, "ridge response is not finite");
         s += v;
      }
      w->ym[o] = s / n;
   }
   for (g = 0; g < (penalties ? q : 1); g++) {
      if (penalties) {
         for (o = 0; o < g; o++)
            if (penalties[o] == penalties[g])
               break;
         if (o < g)
            continue;
         lambda = penalties[g];
      }
      for (i = 0; i < d; i++)
         for (j = 0; j <= i; j++) {
            double s = 0;
            for (t = 0; t < (dual ? p : n); t++) {
               double u = dual ? x[(size_t)i * p + t] - w->xm[t]
                               : x[(size_t)t * p + i] - w->xm[i];
               double v = dual ? x[(size_t)j * p + t] - w->xm[t]
                               : x[(size_t)t * p + j] - w->xm[j];
               s += u * v;
            }
            a[(size_t)i * d + j] = s + (i == j ? lambda : 0);
         }
      /* Cholesky factorization and triangular solves share one factor for all
       * responses. */
      for (i = 0; i < d; i++)
         for (j = 0; j <= i; j++) {
            double s = a[(size_t)i * d + j];
            for (k = 0; k < j; k++)
               s -= a[(size_t)i * d + k] * a[(size_t)j * d + k];
            if (i == j) {
               if (!isfinite(s) || s <= 0)
                  return reg_error(err, nerr,
                                   "ridge factorization failed; increase "
                                   "lambda or rescale predictors");
               a[(size_t)i * d + j] = sqrt(s);
            } else
               a[(size_t)i * d + j] = s / a[(size_t)j * d + j];
         }
      for (i = 0; i < d; i++)
         for (o = 0; o < q; o++) {
            double s = 0;
            if (penalties && penalties[o] != lambda)
               continue;
            if (dual)
               s = y[(size_t)i * q + o] - w->ym[o];
            else
               for (t = 0; t < n; t++)
                  s += (x[(size_t)t * p + i] - w->xm[i]) *
                       (y[(size_t)t * q + o] - w->ym[o]);
            for (k = 0; k < i; k++)
               s -= a[(size_t)i * d + k] * b[(size_t)k * q + o];
            b[(size_t)i * q + o] = s / a[(size_t)i * d + i];
         }
      for (i = d - 1; i >= 0; i--)
         for (o = 0; o < q; o++) {
            double s;
            if (penalties && penalties[o] != lambda)
               continue;
            s = b[(size_t)i * q + o];
            for (k = i + 1; k < d; k++)
               s -= a[(size_t)k * d + i] * b[(size_t)k * q + o];
            b[(size_t)i * q + o] = s / a[(size_t)i * d + i];
         }
      for (j = 0; j < p; j++)
         for (o = 0; o < q; o++) {
            double s = 0;
            if (penalties && penalties[o] != lambda)
               continue;
            if (dual)
               for (i = 0; i < n; i++)
                  s += (x[(size_t)i * p + j] - w->xm[j]) * b[(size_t)i * q + o];
            else
               s = b[(size_t)j * q + o];
            if (!isfinite(s))
               return reg_error(err, nerr, "ridge coefficient is not finite");
            w->coef[(size_t)j * q + o] = s;
         }
   } /* distinct response penalties */
   w->n = n;
   w->p = p;
   w->q = q;
   w->fitted = 1;
   return 1;
}

int THD_ridge_fit(THD_ridge_workspace *w, const float *x, const float *y, int n,
                  int p, int q, double lambda, int form, char *err, int nerr)
{
   return ridge_fit_penalties(w, x, y, n, p, q, lambda, NULL, form, err, nerr);
}
int THD_ridge_fit_response_lambda(THD_ridge_workspace *w, const float *x,
                                  const float *y, int n, int p, int q,
                                  const double *lambda, int form, char *err,
                                  int nerr)
{
   if (!lambda) {
      if (w)
         w->fitted = 0;
      return reg_error(err, nerr, "response ridge needs a penalty vector");
   }
   return ridge_fit_penalties(w, x, y, n, p, q, 1, lambda, form, err, nerr);
}

/* Copy centered-fit slopes into caller-owned output, and reconstruct the
   intercept as mean(Y) - mean(X) B. Internal coefficient storage stays owned
   by the workspace; callers may safely keep these copies after another fit. */
int THD_ridge_coefficients(const THD_ridge_workspace *w, double *coef,
                           double *intercept, char *err, int nerr)
{
   int j, o;
   if (!w || !w->fitted || !coef || !intercept)
      return reg_error(
          err, nerr,
          "ridge coefficient export needs a fitted model and output buffers");
   for (o = 0; o < w->q; o++) {
      double v = w->ym[o];
      for (j = 0; j < w->p; j++)
         v -= w->xm[j] * w->coef[(size_t)j * w->q + o];
      if (!isfinite(v))
         return reg_error(err, nerr, "ridge intercept is not finite");
      intercept[o] = v;
   }
   memcpy(coef, w->coef, sizeof(double) * (size_t)w->p * w->q);
   return 1;
}

int THD_ridge_predict(const THD_ridge_workspace *w, const float *x, int n,
                      float *out, char *err, int nerr)
{
   int i, j, o;
   if (!w || !w->fitted || !x || !out || n < 1)
      return reg_error(err, nerr,
                       "ridge prediction needs a fitted model and rows");
   for (i = 0; i < n; i++)
      for (o = 0; o < w->q; o++) {
         double s = w->ym[o];
         for (j = 0; j < w->p; j++) {
            double v = x[(size_t)i * w->p + j];
            if (!isfinite(v))
               return reg_error(err, nerr,
                                "ridge prediction input is not finite");
            s += (v - w->xm[j]) * w->coef[(size_t)j * w->q + o];
         }
         if (!isfinite(s) || fabs(s) > FLT_MAX)
            return reg_error(err, nerr, "ridge prediction exceeds float range");
         out[(size_t)i * w->q + o] = (float)s;
      }
   return 1;
}

/* Reusable continuous estimators. Spatial extraction and fold construction
   remain in their existing callers; no model sees rows outside its fit. */
/* The legacy estimator owns copies of configuration, candidate parameters,
   training arrays and all solver scratch. xx/yy hold centered data for EN,
   banded ridge and PLS; SVR keeps its documented explicit bias formulation.
   PLS deflates these copies, so each public fit must refill them from input. */
/* 2. LEGACY CONTINUOUS ESTIMATORS. */
struct THD_regression {
   THD_regression_config config;
   int maxn, maxp, maxq, n, p, q, fitted;
   double *parameter, *penalty;
   int *group;
   double *coef, *intercept, *xm, *ym, *xx, *yy, *residual, *norm, *dual;
   double *weight, *old, *score, *loading, *response, *rotation, *loadings;
   THD_ridge_workspace *linear;
   THD_regression_diagnostic *diagnostic;
};
void THD_regression_free(THD_regression *m)
{
   if (!m)
      return;
   free(m->parameter);
   free(m->penalty);
   free(m->group);
   free(m->coef);
   free(m->intercept);
   free(m->xm);
   free(m->ym);
   free(m->xx);
   free(m->yy);
   free(m->residual);
   free(m->norm);
   free(m->dual);
   free(m->weight);
   free(m->old);
   free(m->score);
   free(m->loading);
   free(m->response);
   free(m->rotation);
   free(m->loadings);
   free(m->diagnostic);
   THD_ridge_free(m->linear);
   free(m);
}
/* Allocate estimator-specific scratch after common dimensions are checked.
   Keep ownership on m immediately: the common destructor can unwind a failed
   partial allocation without each branch duplicating a cleanup sequence. */
static int regression_alloc_fields(THD_regression *m, int n, int p, int q,
                                   const THD_regression_config *cfg, char *err,
                                   int nerr)
{
   int d = n < p ? n : p;
   m->config = *cfg;
   m->maxn = n;
   m->maxp = p;
   m->maxq = q;
#define REG_ALLOC(field, count)                                                \
   do {                                                                        \
      m->field = calloc((size_t)(count), sizeof(*m->field));                   \
      if (!m->field)                                                           \
         return 0;                                                             \
   } while (0)
   REG_ALLOC(parameter, (size_t)cfg->ncandidate * 2);
   memcpy(m->parameter, cfg->parameter,
          sizeof(double) * (size_t)cfg->ncandidate * 2);
   m->config.parameter = m->parameter;
   if (cfg->method == THD_REG_BANDED_RIDGE) {
      REG_ALLOC(group, p);
      REG_ALLOC(penalty, (size_t)cfg->ncandidate * cfg->ngroup);
      memcpy(m->group, cfg->feature_group, sizeof(int) * (size_t)p);
      memcpy(m->penalty, cfg->group_penalty,
             sizeof(double) * (size_t)cfg->ncandidate * cfg->ngroup);
      m->config.feature_group = m->group;
      m->config.group_penalty = m->penalty;
      m->linear = THD_ridge_alloc(n, p, q, err, nerr);
      if (!m->linear)
         return 0;
   }
   REG_ALLOC(coef, (size_t)p * q);
   REG_ALLOC(intercept, q);
   REG_ALLOC(xm, p);
   REG_ALLOC(ym, q);
   REG_ALLOC(xx, (size_t)n * p);
   REG_ALLOC(yy, (size_t)n * q);
   REG_ALLOC(diagnostic, q);
   REG_ALLOC(residual, n);
   REG_ALLOC(norm, p > n ? p : n);
   REG_ALLOC(dual, n);
   if (cfg->method == THD_REG_PLS) {
      REG_ALLOC(weight, p);
      REG_ALLOC(old, p);
      REG_ALLOC(score, n);
      REG_ALLOC(loading, p);
      REG_ALLOC(response, q);
      REG_ALLOC(rotation, (size_t)d * p);
      REG_ALLOC(loadings, (size_t)d * p);
   }
#undef REG_ALLOC
   return 1;
}
/* Validate the candidate table and copy it into an owned model. Every
   estimator uses two parameter slots per candidate, but their meanings differ;
   reject invalid ranges before reserving large matrix work buffers. */
THD_regression *THD_regression_alloc(int n, int p, int q,
                                     const THD_regression_config *cfg,
                                     char *err, int nerr)
{
   THD_regression *m = NULL;
   int i, j;
   if (!cfg || n < 2 || p < 1 || q < 1 || cfg->method < 1 || cfg->method > 4 ||
       cfg->ncandidate < 1 || !cfg->parameter || !isfinite(cfg->tolerance) ||
       cfg->tolerance < 1e-12 || cfg->tolerance > 1e-2 ||
       cfg->max_iterations < 1) {
      reg_error(err, nerr,
                "invalid continuous estimator configuration/capacity");
      return NULL;
   }
   if ((size_t)n > SIZE_MAX / sizeof(double) / (size_t)(p > q ? p : q) ||
       (size_t)p > SIZE_MAX / sizeof(double) / (size_t)q ||
       (size_t)cfg->ncandidate > SIZE_MAX / sizeof(double) / 2) {
      reg_error(err, nerr, "continuous estimator too large");
      return NULL;
   }
   for (i = 0; i < cfg->ncandidate; i++) {
      double a = cfg->parameter[2 * i], b = cfg->parameter[2 * i + 1];
      if (!isfinite(a) || !isfinite(b) || a <= 0 ||
          (cfg->method == THD_REG_ELASTIC_NET && (b < 0 || b > 1)) ||
          (cfg->method == THD_REG_SVR && b < 0) ||
          (cfg->method == THD_REG_PLS && (a != floor(a) || a > INT_MAX))) {
         reg_error(err, nerr, "invalid regression candidate parameters");
         return NULL;
      }
   }
   if (cfg->method == THD_REG_BANDED_RIDGE) {
      if (cfg->nfeature != p || cfg->ngroup < 1 || cfg->ngroup > p ||
          !cfg->feature_group || !cfg->group_penalty ||
          (size_t)cfg->ncandidate >
              SIZE_MAX / sizeof(double) / (size_t)cfg->ngroup) {
         reg_error(err, nerr,
                   "banded ridge needs complete design-column groups and "
                   "penalty vectors");
         return NULL;
      }
      for (j = 0; j < p; j++)
         if (cfg->feature_group[j] < 0 ||
             cfg->feature_group[j] >= cfg->ngroup) {
            reg_error(err, nerr, "invalid predictor group ID");
            return NULL;
         }
      for (i = 0; i < cfg->ngroup; i++) {
         for (j = 0; j < p && cfg->feature_group[j] != i; j++)
            ;
         if (j == p) {
            reg_error(err, nerr, "empty predictor group");
            return NULL;
         }
      }
      for (i = 0; i < cfg->ncandidate; i++)
         for (j = 0; j < cfg->ngroup; j++)
            if (!isfinite(cfg->group_penalty[(size_t)i * cfg->ngroup + j]) ||
                cfg->group_penalty[(size_t)i * cfg->ngroup + j] <= 0) {
               reg_error(err, nerr,
                         "group penalties must be finite and positive");
               return NULL;
            }
   }
   m = calloc(1, sizeof(*m));
   if (m && regression_alloc_fields(m, n, p, q, cfg, err, nerr))
      return m;
   THD_regression_free(m);
   reg_error(err, nerr, "out of memory allocating continuous estimator");
   return NULL;
}

/* Soft thresholding is the scalar minimizer for an L1-penalized quadratic.
   reg_kkt below measures distance from its optimality condition: at a zero
   coefficient, any gradient inside [-l1,l1] is allowed by the subgradient. */
static double reg_soft(double v, double threshold)
{
   return v > threshold ? v - threshold : v < -threshold ? v + threshold : 0;
}
static double reg_kkt(double gradient, double beta, double l1)
{
   return beta > 0   ? fabs(gradient + l1)
          : beta < 0 ? fabs(gradient - l1)
                     : fmax(0, fabs(gradient) - l1);
}
/* Shared coordinate-descent kernel. The legacy MVPA wrapper supplies centered
   arrays; prepared Picard fits supply their explicit effective design instead.
   Coefficients use [feature][response]. Residual scratch holds ONE response.
   The caller initializes coefficients, allowing cold fits or warm path steps.
   Every sweep recomputes residuals and checks ALL KKT conditions: neither
   small coefficient changes nor an active-set-only check proves convergence. */
static int elastic_solve(const double *x, const double *y, int n, int p, int q,
                         int o, const double *norm, double *coef,
                         double *residual, double lambda, double rho,
                         double tol, int maxit, THD_regression_diagnostic *dg,
                         char *err, int nerr)
{
   int i, j, it;
   double l1 = lambda * rho, l2 = lambda * (1 - rho);
   memset(dg, 0, sizeof(*dg));
   for (i = 0; i < n; i++) {
      residual[i] = y[(size_t)i * q + o];
      for (j = 0; j < p; j++)
         residual[i] -= x[(size_t)i * p + j] * coef[(size_t)j * q + o];
   }
   for (it = 1; it <= maxit; it++) {
      for (j = 0; j < p; j++) {
         double old = coef[(size_t)j * q + o], v = 0;
         double den = norm[j] + l2, b;
         for (i = 0; i < n; i++)
            v += x[(size_t)i * p + j] * residual[i];
         b = den > 0 ? reg_soft(v / n + norm[j] * old, l1) / den : 0;
         if (!isfinite(b))
            return reg_error(err, nerr, "elastic-net nonfinite coefficient");
         coef[(size_t)j * q + o] = b;
         for (i = 0; i < n; i++)
            residual[i] -= x[(size_t)i * p + j] * (b - old);
      }
      for (i = 0; i < n; i++) {
         double r = y[(size_t)i * q + o];
         for (j = 0; j < p; j++)
            r -= x[(size_t)i * p + j] * coef[(size_t)j * q + o];
         residual[i] = r;
      }
      dg->residual = 0;
      for (j = 0; j < p; j++) {
         double g = l2 * coef[(size_t)j * q + o];
         for (i = 0; i < n; i++)
            g -= x[(size_t)i * p + j] * residual[i] / n;
         if (!isfinite(g))
            return reg_error(err, nerr, "elastic-net nonfinite KKT gradient");
         dg->residual =
             fmax(dg->residual, reg_kkt(g, coef[(size_t)j * q + o], l1));
      }
      if (dg->residual <= tol)
         break;
   }
   if (it > maxit)
      return reg_error(
          err, nerr,
          "elastic-net did not converge: response %d KKT %.9g exceeds "
          "%.9g after %d sweeps",
          o + 1, dg->residual, tol, maxit);
   dg->iterations = it;
   for (i = 0; i < n; i++)
      dg->objective += residual[i] * residual[i] / (2.0 * n);
   for (j = 0; j < p; j++) {
      double b = coef[(size_t)j * q + o];
      dg->objective += l1 * fabs(b) + l2 * b * b / 2;
      dg->nonzero += b != 0;
   }
   return isfinite(dg->objective) ||
          reg_error(err, nerr, "elastic-net nonfinite objective");
}

/* Preserve MVPA's centering, cold starts, units and public state layout. */
static int reg_elastic(THD_regression *m, int candidate, char *err, int nerr)
{
   int n = m->n, p = m->p, q = m->q, i, j, o;
   double a = m->parameter[2 * candidate];
   double rho = m->parameter[2 * candidate + 1];
   for (j = 0; j < p; j++) {
      double sum = 0;
      for (i = 0; i < n; i++)
         sum += m->xx[(size_t)i * p + j] * m->xx[(size_t)i * p + j];
      m->norm[j] = sum / n;
   }
   for (o = 0; o < q; o++)
      if (!elastic_solve(m->xx, m->yy, n, p, q, o, m->norm, m->coef,
                         m->residual, a, rho, m->config.tolerance,
                         m->config.max_iterations, m->diagnostic + o, err,
                         nerr))
         return 0;
   return 1;
}
/* Shared primal/dual banded kernel, used by legacy and prepared APIs.
   D is the positive diagonal penalty in unnormalized XtX units. For p<=n,
   solve (X'X+D)B=X'Y. For p>n, solve (I+X D^-1 X')A=Y, then B=D^-1 X'A.
   Choosing the smaller system bounds factor storage by min(n,p)^2.
   Optional gram caches X'X for primal prepared fits. Factors/RHS/diagnostics
   are private mutable scratch; callers zero diagnostics before each call. */
static int banded_solve(const double *x, const double *y, int n, int p, int q,
                        const int *group, const double *pen, const double *gram,
                        double *a, double *b, double *coef, double *residual,
                        THD_regression_diagnostic *diagnostic)
{
   int dual = p > n, d = dual ? n : p, i, j, k, o;
   for (i = 0; i < d; i++)
      for (j = 0; j <= i; j++) {
         double sum = 0;
         if (gram)
            sum = gram[(size_t)i * d + j];
         else
            for (k = 0; k < (dual ? p : n); k++)
               sum += dual ? x[(size_t)i * p + k] * x[(size_t)j * p + k] /
                                 pen[group[k]]
                           : x[(size_t)k * p + i] * x[(size_t)k * p + j];
         a[(size_t)i * d + j] = sum + (i == j ? (dual ? 1 : pen[group[i]]) : 0);
      }
   for (i = 0; i < d; i++)
      for (o = 0; o < q; o++) {
         double sum = 0;
         if (dual)
            sum = y[(size_t)i * q + o];
         else
            for (k = 0; k < n; k++)
               sum += x[(size_t)k * p + i] * y[(size_t)k * q + o];
         b[(size_t)i * q + o] = sum;
      }
   if (!THD_regression_spd_solve(a, b, d, q))
      return 0;
   for (j = 0; j < p; j++)
      for (o = 0; o < q; o++) {
         double v = 0;
         if (dual) {
            for (i = 0; i < n; i++)
               v += x[(size_t)i * p + j] * b[(size_t)i * q + o];
            v /= pen[group[j]];
         } else
            v = b[(size_t)j * q + o];
         coef[(size_t)j * q + o] = v;
      }
   for (o = 0; o < q; o++) {
      THD_regression_diagnostic *dg = diagnostic + o;
      dg->iterations = 1;
      for (i = 0; i < n; i++) {
         double r = y[(size_t)i * q + o];
         for (j = 0; j < p; j++)
            r -= x[(size_t)i * p + j] * coef[(size_t)j * q + o];
         residual[i] = r;
         dg->objective += r * r;
      }
      for (j = 0; j < p; j++) {
         double beta = coef[(size_t)j * q + o], g = pen[group[j]] * beta;
         for (i = 0; i < n; i++)
            g -= x[(size_t)i * p + j] * residual[i];
         dg->residual = fmax(dg->residual, fabs(g));
         dg->objective += pen[group[j]] * beta * beta;
         dg->nonzero += beta != 0;
      }
   }
   return 1;
}
/* Preserve legacy centering, unnormalized penalties and failure text. */
static int reg_banded(THD_regression *m, int candidate, char *err, int nerr)
{
   const double *pen = m->penalty + (size_t)candidate * m->config.ngroup;
   if (!banded_solve(m->xx, m->yy, m->n, m->p, m->q, m->group, pen, NULL,
                     m->linear->factor, m->linear->rhs, m->coef, m->residual,
                     m->diagnostic))
      return reg_error(
          err, nerr,
          "LDA factorization failed; increase shrinkage or variance floor");
   return 1;
}
/* Squared epsilon-insensitive linear SVR, solved one response at a time in
   dual coordinates. Each update adjusts the primal weights and bias cheaply.
   The extra 1 in the coordinate norm represents the constant bias feature;
   this bias is penalized, unlike the centered legacy ridge intercept.
   Reconstruct primal values each sweep, then test all dual KKT conditions. */
static int reg_svr(THD_regression *m, int candidate, char *err, int nerr)
{
   int n = m->n, p = m->p, q = m->q, i, j, o, it;
   double c = m->parameter[2 * candidate],
          eps = m->parameter[2 * candidate + 1], diag = 1 / (2 * c);
   for (i = 0; i < n; i++) {
      double v = 1 + diag;
      for (j = 0; j < p; j++)
         v += m->xx[(size_t)i * p + j] * m->xx[(size_t)i * p + j];
      m->norm[i] = v;
   }
   for (o = 0; o < q; o++) {
      THD_regression_diagnostic *dg = m->diagnostic + o;
      memset(m->dual, 0, sizeof(double) * (size_t)n);
      m->intercept[o] = 0;
      for (it = 1; it <= m->config.max_iterations; it++) {
         for (i = 0; i < n; i++) {
            double pred = m->intercept[o], g, old = m->dual[i], v, delta;
            for (j = 0; j < p; j++)
               pred += m->xx[(size_t)i * p + j] * m->coef[(size_t)j * q + o];
            g = pred - m->yy[(size_t)i * q + o] + diag * old;
            v = reg_soft(old - g / m->norm[i], eps / m->norm[i]);
            delta = v - old;
            m->dual[i] = v;
            m->intercept[o] += delta;
            for (j = 0; j < p; j++)
               m->coef[(size_t)j * q + o] += delta * m->xx[(size_t)i * p + j];
         }
         /* Reconstruct primal variables to prevent coordinate-update drift. */
         m->intercept[o] = 0;
         for (j = 0; j < p; j++)
            m->coef[(size_t)j * q + o] = 0;
         for (i = 0; i < n; i++) {
            m->intercept[o] += m->dual[i];
            for (j = 0; j < p; j++)
               m->coef[(size_t)j * q + o] +=
                   m->dual[i] * m->xx[(size_t)i * p + j];
         }
         dg->residual = 0;
         for (i = 0; i < n; i++) {
            double g =
                m->intercept[o] - m->yy[(size_t)i * q + o] + diag * m->dual[i];
            for (j = 0; j < p; j++)
               g += m->xx[(size_t)i * p + j] * m->coef[(size_t)j * q + o];
            dg->residual = fmax(dg->residual, reg_kkt(g, m->dual[i], eps));
         }
         if (!isfinite(dg->residual))
            return reg_error(err, nerr, "SVR nonfinite dual KKT residual");
         if (dg->residual <= m->config.tolerance)
            break;
      }
      if (it > m->config.max_iterations)
         return reg_error(err, nerr,
                          "SVR did not converge: response %d dual KKT %.9g "
                          "exceeds %.9g after %d sweeps",
                          o + 1, dg->residual, m->config.tolerance,
                          m->config.max_iterations);
      dg->iterations = it;
      dg->objective = m->intercept[o] * m->intercept[o] / 2;
      for (j = 0; j < p; j++) {
         double b = m->coef[(size_t)j * q + o];
         dg->objective += b * b / 2;
         dg->nonzero += b != 0;
      }
      for (i = 0; i < n; i++) {
         double r = m->yy[(size_t)i * q + o] - m->intercept[o];
         for (j = 0; j < p; j++)
            r -= m->xx[(size_t)i * p + j] * m->coef[(size_t)j * q + o];
         r = fmax(0, fabs(r) - eps);
         dg->objective += c * r * r;
      }
   }
   return 1;
}
/* Extract latent components using alternating predictor/response directions
   (NIPALS-style iteration). Each component yields a score vector in observation
   space and loadings in predictor/response space. Remove its explained part
   from xx and yy before seeking the next component. The rotation recurrence
   maps each deflated-space weight back to original predictor coordinates, so
   the accumulated coefficient matrix predicts with the original X. */
static int reg_pls(THD_regression *m, int candidate, char *err, int nerr)
{
   int n = m->n, p = m->p, q = m->q, limit = (int)m->parameter[2 * candidate],
       i, j, o, k, h, it, used = 0, maxit = 0;
   double initial = 0, last = 0, cutoff;
   if (limit > p)
      limit = p;
   if (limit > n - 1)
      limit = n - 1;
   for (i = 0; i < n; i++)
      for (j = 0; j < p; j++)
         initial += m->xx[(size_t)i * p + j] * m->xx[(size_t)i * p + j];
   cutoff = initial * 64 * DBL_EPSILON * (n > p ? n : p);
   for (k = 0; k < limit; k++) {
      int col = 0;
      double largest = 0, tt = 0;
      for (o = 0; o < q; o++) {
         double sum = 0;
         for (i = 0; i < n; i++)
            sum += m->yy[(size_t)i * q + o] * m->yy[(size_t)i * q + o];
         if (sum > largest) {
            largest = sum;
            col = o;
         }
      }
      if (largest <= DBL_MIN)
         break;
      for (i = 0; i < n; i++)
         m->residual[i] = m->yy[(size_t)i * q + col];
      memset(m->old, 0, sizeof(double) * (size_t)p);
      for (it = 1; it <= m->config.max_iterations; it++) {
         double norm = 0, cc = 0, diff = 0;
         for (j = 0; j < p; j++) {
            double sum = 0;
            for (i = 0; i < n; i++)
               sum += m->xx[(size_t)i * p + j] * m->residual[i];
            m->weight[j] = sum;
            norm += sum * sum;
         }
         if (norm <= DBL_MIN) {
            tt = 0;
            break;
         }
         norm = sqrt(norm);
         for (j = 0; j < p; j++)
            m->weight[j] /= norm;
         /* Fix each iterate's sign, including repeated singular-value cases. */
         {
            int pivot = 0;
            for (j = 1; j < p; j++)
               if (fabs(m->weight[j]) > fabs(m->weight[pivot]))
                  pivot = j;
            if (m->weight[pivot] < 0)
               for (j = 0; j < p; j++)
                  m->weight[j] = -m->weight[j];
         }
         tt = 0;
         for (i = 0; i < n; i++) {
            double sum = 0;
            for (j = 0; j < p; j++)
               sum += m->xx[(size_t)i * p + j] * m->weight[j];
            m->score[i] = sum;
            tt += sum * sum;
         }
         if (tt <= cutoff)
            break;
         for (o = 0; o < q; o++) {
            double sum = 0;
            for (i = 0; i < n; i++)
               sum += m->yy[(size_t)i * q + o] * m->score[i];
            m->response[o] = sum / tt;
            cc += m->response[o] * m->response[o];
         }
         if (cc <= DBL_MIN) {
            tt = 0;
            break;
         }
         for (j = 0; j < p; j++) {
            double d = m->weight[j] - m->old[j];
            diff += d * d;
            m->old[j] = m->weight[j];
         }
         last = sqrt(diff);
         if (q == 1 || last <= m->config.tolerance) {
            if (q == 1)
               last = 0;
            break;
         }
         for (i = 0; i < n; i++) {
            double sum = 0;
            for (o = 0; o < q; o++)
               sum += m->yy[(size_t)i * q + o] * m->response[o];
            m->residual[i] = sum / cc;
         }
      }
      if (it > m->config.max_iterations)
         return reg_error(err, nerr,
                          "PLS power iteration did not converge at component "
                          "%d (change %.9g)",
                          k + 1, last);
      if (tt <= cutoff)
         break;
      if (it > maxit)
         maxit = it;
      for (j = 0; j < p; j++) {
         double sum = 0;
         for (i = 0; i < n; i++)
            sum += m->xx[(size_t)i * p + j] * m->score[i];
         m->loading[j] = sum / tt;
         m->loadings[(size_t)k * p + j] = m->loading[j];
         m->rotation[(size_t)k * p + j] = m->weight[j];
      }
      for (h = 0; h < k; h++) {
         double cross = 0;
         for (j = 0; j < p; j++)
            cross += m->loadings[(size_t)h * p + j] * m->weight[j];
         for (j = 0; j < p; j++)
            m->rotation[(size_t)k * p + j] -=
                m->rotation[(size_t)h * p + j] * cross;
      }
      for (j = 0; j < p; j++)
         for (o = 0; o < q; o++)
            m->coef[(size_t)j * q + o] +=
                m->rotation[(size_t)k * p + j] * m->response[o];
      for (i = 0; i < n; i++) {
         for (j = 0; j < p; j++)
            m->xx[(size_t)i * p + j] -= m->score[i] * m->loading[j];
         for (o = 0; o < q; o++)
            m->yy[(size_t)i * q + o] -= m->score[i] * m->response[o];
      }
      used++;
   }
   for (o = 0; o < q; o++) {
      THD_regression_diagnostic *dg = m->diagnostic + o;
      dg->components = used;
      dg->iterations = maxit;
      dg->residual = used ? last : 0;
      for (i = 0; i < n; i++)
         dg->objective +=
             m->yy[(size_t)i * q + o] * m->yy[(size_t)i * q + o] / n;
      for (j = 0; j < p; j++)
         dg->nonzero += m->coef[(size_t)j * q + o] != 0;
   }
   return 1;
}
/* Start a fresh legacy fit: validate dimensions/candidate, copy input, apply
   the estimator-specific centering policy, and dispatch to its shared kernel.
   Publish fitted only after all responses succeed. A failed fit leaves state
   unavailable to prediction, coefficient access and serialization. */
int THD_regression_fit(THD_regression *m, const float *x, const float *y, int n,
                       int p, int q, int candidate, char *err, int nerr)
{
   int i, j, o, ok = 0, center;
   if (m)
      m->fitted = 0;
   if (!m || !x || !y || n < 2 || n > m->maxn || p < 1 || p > m->maxp ||
       q < 1 || q > m->maxq || candidate < 0 ||
       candidate >= m->config.ncandidate ||
       (m->config.method == THD_REG_BANDED_RIDGE && p != m->config.nfeature))
      return reg_error(err, nerr,
                       "invalid continuous estimator fit dimensions/candidate");
   m->n = n;
   m->p = p;
   m->q = q;
   center = m->config.method != THD_REG_SVR;
   memset(m->coef, 0, sizeof(double) * (size_t)p * q);
   memset(m->diagnostic, 0, sizeof(*m->diagnostic) * (size_t)q);
   for (j = 0; j < p; j++) {
      double sum = 0;
      for (i = 0; i < n; i++) {
         double v = x[(size_t)i * p + j];
         if (!isfinite(v))
            return reg_error(err, nerr, "nonfinite regression predictor");
         sum += v;
      }
      m->xm[j] = center ? sum / n : 0;
      for (i = 0; i < n; i++)
         m->xx[(size_t)i * p + j] = x[(size_t)i * p + j] - m->xm[j];
   }
   for (o = 0; o < q; o++) {
      double sum = 0;
      for (i = 0; i < n; i++) {
         double v = y[(size_t)i * q + o];
         if (!isfinite(v))
            return reg_error(err, nerr, "nonfinite regression response");
         sum += v;
      }
      m->ym[o] = sum / n;
      m->intercept[o] = center ? m->ym[o] : 0;
      for (i = 0; i < n; i++)
         m->yy[(size_t)i * q + o] =
             y[(size_t)i * q + o] - (center ? m->ym[o] : 0);
   }
   switch (m->config.method) {
   case THD_REG_ELASTIC_NET:
      ok = reg_elastic(m, candidate, err, nerr);
      break;
   case THD_REG_BANDED_RIDGE:
      ok = reg_banded(m, candidate, err, nerr);
      break;
   case THD_REG_PLS:
      ok = reg_pls(m, candidate, err, nerr);
      break;
   case THD_REG_SVR:
      ok = reg_svr(m, candidate, err, nerr);
      break;
   }
   if (!ok)
      return 0;
   for (o = 0; o < q; o++) {
      for (j = 0; j < p; j++) {
         double b = m->coef[(size_t)j * q + o];
         if (!isfinite(b))
            return reg_error(err, nerr, "nonfinite regression coefficient");
         m->intercept[o] -= m->xm[j] * b;
      }
      if (!isfinite(m->intercept[o]) || !isfinite(m->diagnostic[o].objective) ||
          !isfinite(m->diagnostic[o].residual))
         return reg_error(err, nerr, "nonfinite regression fit diagnostics");
   }
   m->fitted = 1;
   return 1;
}
int THD_regression_predict(const THD_regression *m, const float *x, int n,
                           float *out, char *err, int nerr)
{
   int i, j, o;
   if (!m || !m->fitted || !x || !out || n < 1)
      return reg_error(err, nerr,
                       "regression prediction requires a fitted model");
   for (i = 0; i < n; i++)
      for (o = 0; o < m->q; o++) {
         double v = m->intercept[o];
         for (j = 0; j < m->p; j++) {
            double z = x[(size_t)i * m->p + j];
            if (!isfinite(z))
               return reg_error(err, nerr,
                                "nonfinite regression prediction input");
            v += z * m->coef[(size_t)j * m->q + o];
         }
         if (!isfinite(v) || fabs(v) > FLT_MAX)
            return reg_error(err, nerr,
                             "regression prediction exceeds float range");
         out[(size_t)i * m->q + o] = (float)v;
      }
   return 1;
}
int THD_regression_coefficients(const THD_regression *m, double *coef,
                                double *intercept, char *err, int nerr)
{
   if (!m || !m->fitted || !coef || !intercept)
      return reg_error(err, nerr,
                       "regression coefficients require a fitted model");
   memcpy(coef, m->coef, sizeof(double) * (size_t)m->p * m->q);
   memcpy(intercept, m->intercept, sizeof(double) * (size_t)m->q);
   return 1;
}
const THD_regression_diagnostic *
THD_regression_diagnostics(const THD_regression *m)
{
   return m && m->fitted ? m->diagnostic : NULL;
}

/* Checked borrowed state views support existing model serialization without
   exposing mutable solver structs. They are valid only after a successful fit
   and only while the owning workspace/model has not been refitted or freed. */
int THD_ridge_get_state(const THD_ridge_workspace *w,
                        THD_regression_state *state, char *err, int nerr)
{
   if (state)
      memset(state, 0, sizeof(*state));
   if (!w || !w->fitted || !state)
      return reg_error(err, nerr,
                       "ridge state requires a fitted model and output view");
   state->n = w->n;
   state->p = w->p;
   state->q = w->q;
   state->xm = w->xm;
   state->ym = w->ym;
   state->coef = w->coef;
   return 1;
}

int THD_regression_get_state(const THD_regression *m,
                             THD_regression_state *state, char *err, int nerr)
{
   if (state)
      memset(state, 0, sizeof(*state));
   if (!m || !m->fitted || !state)
      return reg_error(
          err, nerr,
          "regression state requires a fitted model and output view");
   state->n = m->n;
   state->p = m->p;
   state->q = m->q;
   state->xm = m->xm;
   state->ym = m->ym;
   state->coef = m->coef;
   state->group = m->group;
   state->ngroup = m->group ? m->config.ngroup : 0;
   return 1;
}

/* Prepared designs share the same coordinate kernel as the legacy estimator.
   Scaling is immutable, so a training fold cannot borrow full-data scales. */
/* Prepared EN splits immutable design data from response scratch. Unlike
   the legacy wrapper, it does not implicitly center data: Picard has already
   applied its nuisance projection/whitening policy before reaching this layer.
 */
/* 3. PREPARED ELASTIC-NET AND BANDED DESIGNS. */
struct THD_elastic_design {
   int n, p;
   THD_elastic_config config;
   double *x, *norm, *scale;
};
struct THD_elastic_workspace {
   const THD_elastic_design *design;
   int maxq, valid, q;
   double *theta, *coef, *residual, *lambda;
   THD_elastic_diagnostic *diagnostic;
   THD_elastic_result result;
};

/* Safe for partial construction, keeping every failure path an ordinary return.
 */
void THD_elastic_design_free(THD_elastic_design *d)
{
   if (!d)
      return;
   free(d->x);
   free(d->norm);
   free(d->scale);
   free(d);
}

/* Copy and optionally RMS-scale the already projected predictor columns.
   hypot avoids overflowing a sum of squares before normalization. */
/* Copy the effective design and optionally divide columns by their RMS.
   Store those scales so coefficients can be returned in the original supplied
   units. A zero column keeps scale 1 and contributes no fitted signal. */
THD_elastic_design *THD_elastic_prepare(const double *x, int n, int p,
                                        const THD_elastic_config *config,
                                        char *err, int nerr)
{
   THD_elastic_design *d;
   int i, j;
   if (!x || !config || n < 1 || p < 1 ||
       (size_t)n > SIZE_MAX / sizeof(double) / (size_t)p ||
       !isfinite(config->rho) || config->rho < 0 || config->rho > 1 ||
       !isfinite(config->tolerance) || config->tolerance <= 0 ||
       config->max_iterations < 1 || config->max_iterations == INT_MAX ||
       (config->standardize != 0 && config->standardize != 1)) {
      reg_error(err, nerr, "invalid prepared elastic-net configuration");
      return NULL;
   }
   d = calloc(1, sizeof(*d));
   if (!d) {
      reg_error(err, nerr, "cannot allocate elastic-net design");
      return NULL;
   }
   d->n = n;
   d->p = p;
   d->config = *config;
   d->x = calloc((size_t)n * p, sizeof(double));
   d->norm = calloc((size_t)p, sizeof(double));
   d->scale = calloc((size_t)p, sizeof(double));
   if (!d->x || !d->norm || !d->scale) {
      THD_elastic_design_free(d);
      reg_error(err, nerr, "cannot allocate elastic-net design buffers");
      return NULL;
   }
   for (j = 0; j < p; j++) {
      double norm = 0;
      for (i = 0; i < n; i++) {
         double v = x[(size_t)i * p + j];
         if (!isfinite(v)) {
            THD_elastic_design_free(d);
            reg_error(err, nerr, "nonfinite elastic-net design");
            return NULL;
         }
         norm = hypot(norm, v);
      }
      d->scale[j] = config->standardize && norm > 0 ? norm / sqrt(n) : 1;
      for (i = 0; i < n; i++) {
         double v = x[(size_t)i * p + j] / d->scale[j];
         d->x[(size_t)i * p + j] = v;
         d->norm[j] += v * v / n;
      }
      if (!isfinite(d->scale[j]) || d->scale[j] <= 0 || !isfinite(d->norm[j])) {
         THD_elastic_design_free(d);
         reg_error(err, nerr, "elastic-net design exceeds double range");
         return NULL;
      }
   }
   return d;
}

/* Borrowed original-to-effective scale factors, one per predictor. */
const double *THD_elastic_scales(const THD_elastic_design *d)
{
   return d ? d->scale : NULL;
}

void THD_elastic_workspace_free(THD_elastic_workspace *w)
{
   if (!w)
      return;
   free(w->theta);
   free(w->coef);
   free(w->residual);
   free(w->lambda);
   free(w->diagnostic);
   free(w);
}

/* Coefficient buffers are bounded by the caller's response batch capacity. */
THD_elastic_workspace *THD_elastic_workspace_alloc(const THD_elastic_design *d,
                                                   int q, char *err, int nerr)
{
   THD_elastic_workspace *w;
   if (!d || q < 1 || (size_t)q > SIZE_MAX / sizeof(double) / (size_t)d->p) {
      reg_error(err, nerr, "invalid elastic-net worker capacity");
      return NULL;
   }
   w = calloc(1, sizeof(*w));
   if (!w) {
      reg_error(err, nerr, "cannot allocate elastic-net worker");
      return NULL;
   }
   w->design = d;
   w->maxq = q;
   w->theta = calloc((size_t)d->p * q, sizeof(double));
   w->coef = calloc((size_t)d->p * q, sizeof(double));
   w->residual = calloc((size_t)d->n, sizeof(double));
   w->lambda = calloc((size_t)q, sizeof(double));
   w->diagnostic = calloc((size_t)q, sizeof(*w->diagnostic));
   if (!w->theta || !w->coef || !w->residual || !w->lambda || !w->diagnostic) {
      THD_elastic_workspace_free(w);
      reg_error(err, nerr, "cannot allocate elastic-net worker buffers");
      return NULL;
   }
   return w;
}

/* Fit independent response columns, sharing immutable X/norm/scale only.
   lambda_max is response-specific in the same effective space as the KKT
   check. Pure ridge has no finite all-zero threshold, represented by NaN. */
/* Solve one penalty value per response using the shared coordinate kernel.
   A warm call may reuse optimization coefficients only for the same active
   batch width. The path API controls that lifetime; unrelated data must start
   cold. Return original-unit coefficients and per-response convergence
   evidence. */
int THD_elastic_fit(THD_elastic_workspace *w, const double *y, int q,
                    const double *lambda, int warm, char *err, int nerr)
{
   const THD_elastic_design *d;
   int i, j, o, was_valid = w && w->valid;
   if (w)
      w->valid = 0;
   if (!w || !y || !lambda || q < 1 || q > w->maxq ||
       (warm != 0 && warm != 1) || (warm && (!was_valid || q != w->q)))
      return reg_error(err, nerr, "invalid elastic-net batch or warm start");
   d = w->design;
   for (o = 0; o < q; o++) {
      if (!isfinite(lambda[o]) || lambda[o] <= 0)
         return reg_error(err, nerr, "elastic-net lambda must be positive");
      for (i = 0; i < d->n; i++)
         if (!isfinite(y[(size_t)i * q + o]))
            return reg_error(err, nerr, "nonfinite elastic-net response");
   }
   if (!warm)
      memset(w->theta, 0, (size_t)d->p * q * sizeof(double));
   for (o = 0; o < q; o++) {
      THD_elastic_diagnostic *dg = w->diagnostic + o;
      double maximum = 0;
      dg->lambda = lambda[o];
      dg->rho = d->config.rho;
      if (!elastic_solve(d->x, y, d->n, d->p, q, o, d->norm, w->theta,
                         w->residual, lambda[o], d->config.rho,
                         d->config.tolerance, d->config.max_iterations,
                         &dg->solver, err, nerr))
         return 0;
      for (j = 0; j < d->p; j++) {
         double covariance = 0;
         for (i = 0; i < d->n; i++)
            covariance +=
                d->x[(size_t)i * d->p + j] * y[(size_t)i * q + o] / d->n;
         maximum = fmax(maximum, fabs(covariance));
         w->coef[(size_t)j * q + o] = w->theta[(size_t)j * q + o] / d->scale[j];
         if (!isfinite(covariance) || !isfinite(w->coef[(size_t)j * q + o]))
            return reg_error(err, nerr, "elastic-net result exceeds range");
      }
      dg->lambda_max = d->config.rho > 0 ? maximum / d->config.rho : NAN;
      if (d->config.rho > 0 && !isfinite(dg->lambda_max))
         return reg_error(err, nerr, "elastic-net lambda_max exceeds range");
   }
   w->q = q;
   w->result.n = d->n;
   w->result.p = d->p;
   w->result.q = q;
   w->result.coef = w->coef;
   w->result.diagnostic = w->diagnostic;
   w->valid = 1;
   return 1;
}

const THD_elastic_result *THD_elastic_get_result(const THD_elastic_workspace *w)
{
   return w && w->valid ? &w->result : NULL;
}

/* Streaming warm path: callers copy only results they need to retain. */
/* Traverse a caller-supplied decreasing lambda grid. Warm starts connect
   neighboring penalties for this same response batch; the visitor must copy
   any result it wants to retain before the next step overwrites scratch. */
int THD_elastic_path(THD_elastic_workspace *w, const double *y, int q,
                     const double *lambda, int count, THD_elastic_sink sink,
                     void *context, char *err, int nerr)
{
   int c, o;
   if (w)
      w->valid = 0;
   if (!w || !y || !lambda || !sink || count < 1 || q < 1 || q > w->maxq)
      return reg_error(err, nerr, "invalid elastic-net path");
   for (c = 0; c < count; c++)
      if (!isfinite(lambda[c]) || lambda[c] <= 0 ||
          (c && lambda[c] >= lambda[c - 1]))
         return reg_error(err, nerr, "lambda path must strictly decrease");
   for (c = 0; c < count; c++) {
      for (o = 0; o < q; o++)
         w->lambda[o] = lambda[c];
      if (!THD_elastic_fit(w, y, q, w->lambda, c > 0, err, nerr) ||
          !sink(context, c, THD_elastic_get_result(w), err, nerr)) {
         w->valid = 0;
         return 0;
      }
   }
   return 1;
}

/* Prepared banded state owns only design-dependent arrays. The primal Gram
   is reused across penalty vectors and voxels. Dual Gram entries depend on
   the penalty vector, so recompute them in private scratch without storing
   an all-candidates tensor. */
struct THD_banded_design {
   int n, p, ngroup, ncandidate;
   int *group;
   double *x, *penalty, *gram;
};
struct THD_banded_workspace {
   const THD_banded_design *design;
   int maxq, valid;
   int *selected;
   double *factor, *rhs, *y, *beta, *coef, *residual;
   THD_regression_diagnostic *scratch;
   THD_banded_diagnostic *diagnostic;
   THD_banded_result result;
};
void THD_banded_design_free(THD_banded_design *d)
{
   if (!d)
      return;
   free(d->group);
   free(d->x);
   free(d->penalty);
   free(d->gram);
   free(d);
}
/* Copy X, predictor group IDs and the penalty-vector table. Each predictor
   receives its group penalty; groups need not be contiguous in column order.
   Choose primal or dual algebra from the dimensions without changing units. */
THD_banded_design *THD_banded_prepare(const double *x, int n, int p,
                                      const THD_banded_config *cfg, char *err,
                                      int nerr)
{
   THD_banded_design *d;
   int i, j, k;
   if (!x || !cfg || n < 1 || p < 1 || cfg->ngroup < 1 || cfg->ncandidate < 1 ||
       !cfg->group || !cfg->penalty ||
       (size_t)n > SIZE_MAX / sizeof(double) / (size_t)p ||
       (size_t)cfg->ncandidate >
           SIZE_MAX / sizeof(double) / (size_t)cfg->ngroup) {
      reg_error(err, nerr, "invalid prepared banded design");
      return NULL;
   }
   d = calloc(1, sizeof(*d));
   if (!d) {
      reg_error(err, nerr, "cannot allocate banded design");
      return NULL;
   }
   d->n = n;
   d->p = p;
   d->ngroup = cfg->ngroup;
   d->ncandidate = cfg->ncandidate;
   d->x = calloc((size_t)n * p, sizeof(double));
   d->group = calloc((size_t)p, sizeof(int));
   d->penalty = calloc((size_t)cfg->ncandidate * cfg->ngroup, sizeof(double));
   if (p <= n)
      d->gram = calloc((size_t)p * p, sizeof(double));
   if (!d->x || !d->group || !d->penalty || (p <= n && !d->gram)) {
      THD_banded_design_free(d);
      reg_error(err, nerr, "cannot allocate banded design arrays");
      return NULL;
   }
   for (j = 0; j < p; j++) {
      if (cfg->group[j] < 0 || cfg->group[j] >= cfg->ngroup) {
         THD_banded_design_free(d);
         reg_error(err, nerr, "invalid banded group ID");
         return NULL;
      }
      d->group[j] = cfg->group[j];
      for (i = 0; i < n; i++) {
         double value = x[(size_t)i * p + j];
         if (!isfinite(value)) {
            THD_banded_design_free(d);
            reg_error(err, nerr, "nonfinite banded design");
            return NULL;
         }
         d->x[(size_t)i * p + j] = value;
      }
   }
   for (i = 0; i < cfg->ncandidate; i++)
      for (j = 0; j < cfg->ngroup; j++) {
         size_t at = (size_t)i * cfg->ngroup + j;
         double value = cfg->penalty[at];
         if (!isfinite(value) || value <= 0 || !isfinite(n * value) ||
             n * value <= 0) {
            THD_banded_design_free(d);
            reg_error(err, nerr,
                      "banded penalties must be positive and finite");
            return NULL;
         }
         d->penalty[at] =
             n * value; /* normalized lambda -> this fold's XtX units */
      }
   if (d->gram)
      for (i = 0; i < p; i++)
         for (j = 0; j <= i; j++) {
            double sum = 0;
            for (k = 0; k < n; k++)
               sum += x[(size_t)k * p + i] * x[(size_t)k * p + j];
            if (!isfinite(sum)) {
               THD_banded_design_free(d);
               reg_error(err, nerr, "banded Gram exceeds double range");
               return NULL;
            }
            d->gram[(size_t)i * p + j] = sum;
         }
   return d;
}
void THD_banded_workspace_free(THD_banded_workspace *w)
{
   if (!w)
      return;
   free(w->selected);
   free(w->factor);
   free(w->rhs);
   free(w->y);
   free(w->beta);
   free(w->coef);
   free(w->residual);
   free(w->scratch);
   free(w->diagnostic);
   free(w);
}
THD_banded_workspace *THD_banded_workspace_alloc(const THD_banded_design *d,
                                                 int q, char *err, int nerr)
{
   THD_banded_workspace *w;
   int size;
   if (!d || q < 1 || (size_t)q > SIZE_MAX / sizeof(double) / (size_t)d->n ||
       (size_t)q > SIZE_MAX / sizeof(double) / (size_t)d->p) {
      reg_error(err, nerr, "invalid banded worker capacity");
      return NULL;
   }
   size = d->n < d->p ? d->n : d->p;
   w = calloc(1, sizeof(*w));
   if (!w) {
      reg_error(err, nerr, "cannot allocate banded worker");
      return NULL;
   }
   w->design = d;
   w->maxq = q;
   w->selected = calloc((size_t)q, sizeof(int));
   w->factor = calloc((size_t)size * size, sizeof(double));
   w->rhs = calloc((size_t)size * q, sizeof(double));
   w->y = calloc((size_t)d->n * q, sizeof(double));
   w->beta = calloc((size_t)d->p * q, sizeof(double));
   w->coef = calloc((size_t)d->p * q, sizeof(double));
   w->residual = calloc((size_t)d->n, sizeof(double));
   w->scratch = calloc((size_t)q, sizeof(*w->scratch));
   w->diagnostic = calloc((size_t)q, sizeof(*w->diagnostic));
   if (!w->selected || !w->factor || !w->rhs || !w->y || !w->beta || !w->coef ||
       !w->residual || !w->scratch || !w->diagnostic) {
      THD_banded_workspace_free(w);
      reg_error(err, nerr, "cannot allocate banded worker arrays");
      return NULL;
   }
   return w;
}
/* Gather responses with identical penalties, solve them together, scatter
   back to caller order. The last batch and each gathered group have their own
   active width; never use maxq as their row stride. */
/* Group responses by their requested penalty vector. Build/factor the system
   once for each vector present in the batch, then solve its response columns
   together. The output scatter preserves original response order. */
int THD_banded_fit(THD_banded_workspace *w, const double *y, int q,
                   const double *candidate, char *err, int nerr)
{
   const THD_banded_design *d;
   int c, i, j, o, s, count;
   if (w)
      w->valid = 0;
   if (!w || !y || !candidate || q < 1 || q > w->maxq)
      return reg_error(err, nerr, "invalid banded response batch");
   d = w->design;
   for (o = 0; o < q; o++) {
      double v = candidate[o];
      if (!isfinite(v) || v < 0 || v >= d->ncandidate || v != floor(v))
         return reg_error(err, nerr, "invalid banded candidate index");
      for (i = 0; i < d->n; i++)
         if (!isfinite(y[(size_t)i * q + o]))
            return reg_error(err, nerr, "nonfinite banded response");
   }
   for (c = 0; c < d->ncandidate; c++) {
      count = 0;
      for (o = 0; o < q; o++)
         if (candidate[o] == c)
            w->selected[count++] = o;
      if (!count)
         continue;
      for (i = 0; i < d->n; i++)
         for (s = 0; s < count; s++)
            w->y[(size_t)i * count + s] = y[(size_t)i * q + w->selected[s]];
      memset(w->scratch, 0, sizeof(*w->scratch) * (size_t)count);
      if (!banded_solve(d->x, w->y, d->n, d->p, count, d->group,
                        d->penalty + (size_t)c * d->ngroup, d->gram, w->factor,
                        w->rhs, w->beta, w->residual, w->scratch))
         return reg_error(err, nerr,
                          "banded factorization failed; increase penalties");
      for (s = 0; s < count; s++) {
         THD_banded_diagnostic *dg = w->diagnostic + w->selected[s];
         dg->candidate = c;
         dg->objective = w->scratch[s].objective / (2.0 * d->n);
         dg->gradient = w->scratch[s].residual / d->n;
         if (!isfinite(dg->objective) || !isfinite(dg->gradient))
            return reg_error(err, nerr,
                             "banded diagnostics exceed double range");
         for (j = 0; j < d->p; j++) {
            double v = w->beta[(size_t)j * count + s];
            if (!isfinite(v))
               return reg_error(err, nerr, "nonfinite banded coefficient");
            w->coef[(size_t)j * q + w->selected[s]] = v;
         }
      }
   }
   w->result = (THD_banded_result){d->p, q, w->coef, w->diagnostic};
   w->valid = 1;
   return 1;
}
const THD_banded_result *THD_banded_get_result(const THD_banded_workspace *w)
{
   return w && w->valid ? &w->result : NULL;
}

/* ========================================================================
   4. THIN SVD AND FRACTIONAL-RIDGE PATHS
   ======================================================================== */

/* Prepared fractional ridge, independent of MVPA and AFNI dataset types. */

static int svd_error(char *err, int nerr, const char *message)
{
   if (err && nerr > 0)
      snprintf(err, (size_t)nerr, "%s", message);
   return 0;
}
static void *svd_array(size_t n, size_t p)
{
   return p && n > SIZE_MAX / sizeof(double) / p
              ? NULL
              : calloc(n * p, sizeof(double));
}

void THD_numeric_svd_free(THD_numeric_svd *s)
{
   if (s) {
      free(s->u);
      free(s->s);
      free(s->v);
      free(s);
   }
}
/* Adapt compact row-major X to the existing column-major EISPACK interface.
   Factor the tall orientation (transpose wide X) to avoid a large p-by-p
   factor. Scale input first, then restore singular-value units and map U/V
   back to the original orientation. The rank cutoff is relative to smax.
   No global SVD option or existing AFNI implementation is changed here. */
THD_numeric_svd *THD_numeric_svd_prepare(const double *x, int n, int p,
                                         double tol, char *err, int nerr)
{
   THD_numeric_svd *s;
   double *a, *v, *work, scale = 0, smax = 0, res = 0, norm = 0;
   int i, j, k, r = n < p ? n : p, tall = n > p ? n : p, ok = 1;
   integer m = tall, nc = r, ld = tall, ldv = r, ierr = 0;
   logical yes = 1;
   if (!x || n < 1 || p < 1 || !isfinite(tol) || tol <= 0 || tol >= 1 ||
       (size_t)n > SIZE_MAX / sizeof(double) / (size_t)p) {
      svd_error(err, nerr,
                "invalid SVD design dimensions or relative rank tolerance");
      return NULL;
   }
   for (i = 0; i < n; i++)
      for (j = 0; j < p; j++) {
         double z = x[(size_t)i * p + j];
         if (!isfinite(z)) {
            svd_error(err, nerr, "nonfinite design value");
            return NULL;
         }
         scale = fmax(scale, fabs(z));
      }
   s = calloc(1, sizeof(*s));
   if (!s) {
      svd_error(err, nerr, "cannot allocate SVD");
      return NULL;
   }
   s->n = n;
   s->p = p;
   s->r = r;
   s->u = svd_array(n, r);
   s->v = svd_array(p, r);
   s->s = svd_array(r, 1);
   a = svd_array(tall, r);
   v = svd_array(r, r);
   work = svd_array(r, 1);
   if (!s->u || !s->v || !s->s || !a || !v || !work)
      ok = svd_error(err, nerr, "cannot allocate SVD factors");
   if (ok && scale > 0) {
      /* Factor the tall orientation, avoiding a p*p factor for wide designs. */
      for (i = 0; i < tall; i++)
         for (j = 0; j < r; j++)
            a[(size_t)j * tall + i] =
                (n >= p ? x[(size_t)i * p + j] : x[(size_t)j * p + i]) / scale;
      svd_(&m, &nc, &ld, a, s->s, &yes, &ld, a, &yes, &ldv, v, &ierr, work);
      if (ierr)
         ok = svd_error(err, nerr, "SVD did not converge");
      if (ok) {
         for (i = 0; i < n; i++)
            for (k = 0; k < r; k++)
               s->u[(size_t)i * r + k] =
                   n >= p ? a[(size_t)k * n + i] : v[(size_t)k * r + i];
         for (j = 0; j < p; j++)
            for (k = 0; k < r; k++)
               s->v[(size_t)j * r + k] =
                   n >= p ? v[(size_t)k * r + j] : a[(size_t)k * p + j];
         /* Check reconstruction before accepting this platform's SVD result. */
         for (i = 0; i < n; i++)
            for (j = 0; j < p; j++) {
               double z = 0, target = x[(size_t)i * p + j] / scale;
               for (k = 0; k < r; k++)
                  z += s->u[(size_t)i * r + k] * s->s[k] *
                       s->v[(size_t)j * r + k];
               res = hypot(res, z - target);
               norm = hypot(norm, target);
            }
         if (!isfinite(res) || res > 1e-8 * norm)
            ok = svd_error(err, nerr, "SVD reconstruction check failed");
         for (k = 0; k < r; k++) {
            s->s[k] *= scale;
            smax = fmax(smax, s->s[k]);
            if (!isfinite(s->s[k]) || s->s[k] < 0)
               ok = svd_error(err, nerr,
                              "SVD singular value exceeds double range");
         }
      }
   }
   free(a);
   free(v);
   free(work);
   if (!ok) {
      THD_numeric_svd_free(s);
      return NULL;
   }
   s->cutoff = tol * smax;
   for (k = 0; k < r; k++)
      s->rank += s->s[k] > s->cutoff;
   return s;
}

struct THD_regression_design {
   int n, p;
   double *x;
   THD_numeric_svd *svd;
};
struct THD_regression_workspace {
   const THD_regression_design *design;
   int maxq, valid;
   double *rotated, *coef, *fractions;
   THD_fraction_diagnostic *diagnostic;
   THD_fraction_result result;
};
void THD_regression_design_free(THD_regression_design *d)
{
   if (d) {
      free(d->x);
      THD_numeric_svd_free(d->svd);
      free(d);
   }
}
/* Own a copy of X and its thin SVD for reuse across voxel batches. Keeping
   X supports original-unit prediction, while the factors support minimum-norm
   OLS and every fractional-ridge penalty without refactorization. */
THD_regression_design *THD_regression_design_prepare(const double *x, int n,
                                                     int p, double tol,
                                                     char *err, int nerr)
{
   THD_regression_design *d = calloc(1, sizeof(*d));
   if (!d) {
      svd_error(err, nerr, "cannot allocate prepared design");
      return NULL;
   }
   d->svd = THD_numeric_svd_prepare(x, n, p, tol, err, nerr);
   if (!d->svd) {
      THD_regression_design_free(d);
      return NULL;
   }
   d->x = svd_array(n, p);
   if (!d->x) {
      THD_regression_design_free(d);
      svd_error(err, nerr, "cannot copy prepared design");
      return NULL;
   }
   d->n = n;
   d->p = p;
   memcpy(d->x, x, sizeof(double) * (size_t)n * p);
   return d;
}
int THD_regression_design_rank(const THD_regression_design *d)
{
   return d ? d->svd->rank : -1;
}
void THD_regression_workspace_free(THD_regression_workspace *w)
{
   if (w) {
      free(w->rotated);
      free(w->coef);
      free(w->fractions);
      free(w->diagnostic);
      free(w);
   }
}
THD_regression_workspace *
THD_regression_workspace_alloc(const THD_regression_design *d, int q, char *err,
                               int nerr)
{
   THD_regression_workspace *w;
   if (!d || q < 1) {
      svd_error(err, nerr, "invalid fractional workspace capacity");
      return NULL;
   }
   w = calloc(1, sizeof(*w));
   if (!w) {
      svd_error(err, nerr, "cannot allocate fractional workspace");
      return NULL;
   }
   w->design = d;
   w->maxq = q;
   w->rotated = svd_array(d->svd->r, q);
   w->coef = svd_array(d->p, q);
   w->fractions = svd_array(q, 1);
   w->diagnostic = calloc((size_t)q, sizeof(*w->diagnostic));
   if (!w->rotated || !w->coef || !w->fractions || !w->diagnostic) {
      THD_regression_workspace_free(w);
      svd_error(err, nerr, "cannot allocate fractional buffers");
      return NULL;
   }
   return w;
}
/* A ridge coefficient in singular direction s is multiplied by
   s^2/(s^2+penalty). The divided form avoids explicitly squaring s here.
   Orthogonal V preserves coefficient norm, so fraction_norm can evaluate the
   requested norm ratio in singular coordinates without reconstructing beta. */
static double fraction_shrink(double s, double penalty)
{
   return 1.0 / (1.0 + (penalty / s) / s);
}
static double fraction_norm(const THD_numeric_svd *s, const double *rot, int q,
                            int o, double norm, double penalty)
{
   double value = 0;
   int k;
   for (k = 0; k < s->r; k++)
      if (s->s[k] > s->cutoff)
         value = hypot(value, (rot[(size_t)k * q + o] / norm) *
                                  fraction_shrink(s->s[k], penalty));
   return value;
}
/* Rotate each response into the retained SVD span and form its minimum-norm
   OLS reference. Find a penalty giving ||beta_ridge|| / ||beta_OLS|| = fraction
   by bounded monotone bisection, then rotate shrunk coefficients back through
   V. Handle zero reference and fractions 0/1 separately: their diagnostics have
   deliberate undefined/infinite values that the AFNI adapter later labels. */
int THD_regression_fit_fractions(THD_regression_workspace *w, const double *y,
                                 int q, const double *fractions, char *err,
                                 int nerr)
{
   const THD_numeric_svd *s;
   int i, j, k, o;
   double smax = 0;
   if (w)
      w->valid = 0;
   if (!w || !y || !fractions || q < 1 || q > w->maxq)
      return svd_error(err, nerr, "invalid fractional fit inputs");
   s = w->design->svd;
   for (o = 0; o < q; o++)
      if (!isfinite(fractions[o]) || fractions[o] < 0 || fractions[o] > 1)
         return svd_error(err, nerr, "fractions must be finite and in [0,1]");
   for (i = 0; i < s->n; i++)
      for (o = 0; o < q; o++)
         if (!isfinite(y[(size_t)i * q + o]))
            return svd_error(err, nerr, "nonfinite response");
   memset(w->coef, 0, sizeof(double) * (size_t)s->p * q);
   for (k = 0; k < s->r; k++) {
      smax = fmax(smax, s->s[k]);
      for (o = 0; o < q; o++) {
         double z = 0;
         if (s->s[k] > s->cutoff) {
            for (i = 0; i < s->n; i++)
               z += s->u[(size_t)i * s->r + k] * y[(size_t)i * q + o];
            z /= s->s[k];
         }
         if (!isfinite(z))
            return svd_error(err, nerr,
                             "fractional reference exceeds double range");
         w->rotated[(size_t)k * q + o] = z;
      }
   }
   for (o = 0; o < q; o++) {
      THD_fraction_diagnostic *dg = w->diagnostic + o;
      double norm = 0, penalty = 0, f = fractions[o], achieved = f;
      int iter = 0;
      memset(dg, 0, sizeof(*dg));
      dg->fraction = f;
      dg->rank = s->rank;
      for (k = 0; k < s->r; k++)
         norm = hypot(norm, w->rotated[(size_t)k * q + o]);
      if (!isfinite(norm))
         return svd_error(err, nerr,
                          "fractional reference norm exceeds double range");
      dg->reference_norm = norm;
      if (norm == 0) {
         dg->status = THD_FRACTION_ZERO_REFERENCE;
         achieved = NAN;
      } else if (f == 0) {
         penalty = INFINITY;
         dg->status = THD_FRACTION_ZERO_ENDPOINT;
      } else if (f < 1) {
         double lo = 0, hi = smax * smax * (1 / f - 1);
         if (!isfinite(hi) || hi <= 0)
            return svd_error(err, nerr,
                             "fractional penalty bracket exceeds double range");
         /* Shrinkage decreases with penalty. The largest singular value
            bounds the required penalty above; update one endpoint per step. */
         for (iter = 0; iter < 256; iter++) {
            penalty = lo + (hi - lo) / 2;
            achieved = fraction_norm(s, w->rotated, q, o, norm, penalty);
            if (fabs(achieved - f) <= THD_FRACTION_TOL * f)
               break;
            if (achieved > f)
               lo = penalty;
            else
               hi = penalty;
         }
         if (iter == 256)
            return svd_error(err, nerr,
                             "fractional penalty search did not converge");
      }
      dg->penalty = penalty;
      dg->lambda = penalty / s->n;
      dg->achieved_fraction = achieved;
      dg->iterations = iter;
      for (j = 0; j < s->p; j++) {
         double b = 0;
         if (f > 0 && norm > 0)
            for (k = 0; k < s->r; k++)
               if (s->s[k] > s->cutoff)
                  b += s->v[(size_t)j * s->r + k] *
                       w->rotated[(size_t)k * q + o] *
                       fraction_shrink(s->s[k], penalty);
         if (!isfinite(b))
            return svd_error(err, nerr,
                             "fractional coefficient exceeds double range");
         w->coef[(size_t)j * q + o] = b;
      }
   }
   w->result.n = s->n;
   w->result.p = s->p;
   w->result.q = q;
   w->result.coef = w->coef;
   w->result.diagnostic = w->diagnostic;
   w->valid = 1;
   return 1;
}
int THD_regression_fit_fraction(THD_regression_workspace *w, const double *y,
                                int q, double fraction, char *err, int nerr)
{
   int o;
   if (w)
      w->valid = 0;
   if (!w || q < 1 || q > w->maxq)
      return svd_error(err, nerr, "invalid fractional response count");
   for (o = 0; o < q; o++)
      w->fractions[o] = fraction;
   return THD_regression_fit_fractions(w, y, q, w->fractions, err, nerr);
}
const THD_fraction_result *
THD_regression_fraction_result(const THD_regression_workspace *w)
{
   return w && w->valid ? &w->result : NULL;
}
int THD_regression_fraction_path(THD_regression_workspace *w, const double *y,
                                 int q, const double *fractions, int count,
                                 THD_fraction_sink sink, void *context,
                                 char *err, int nerr)
{
   int c;
   if (w)
      w->valid = 0;
   if (!w || !fractions || count < 1 || !sink)
      return svd_error(err, nerr, "invalid fractional path");
   for (c = 0; c < count; c++)
      if (!isfinite(fractions[c]) || fractions[c] < 0 || fractions[c] > 1)
         return svd_error(err, nerr, "invalid path fraction");
   for (c = 0; c < count; c++) {
      if (!THD_regression_fit_fraction(w, y, q, fractions[c], err, nerr))
         return 0;
      if (!sink(context, c, &w->result, err, nerr)) {
         w->valid = 0;
         return 0;
      }
   }
   return 1;
}
