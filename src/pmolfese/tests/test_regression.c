/* Standalone solver contracts: deliberately links only thd_regression and libm.
   MVPA's existing numerical and integration fixtures remain in place. */
#include "thd_regression.h"
#include "thd_regression_internal.h"
#include <math.h>
#include <stdio.h>
#include <string.h>

#define CHECK(test, message) do { if(!(test)) { \
   fprintf(stderr,"FAIL line %d: %s\n",__LINE__,message);return 1; \
} } while(0)
#define CLOSE(a,b) (fabs((a)-(b)) < 1e-9)

/* The caller owns allocation/cleanup, including when an assertion fails. */
static int ridge_contract(THD_ridge_workspace *w)
{
   char err[256];THD_regression_state s;
   float x[]={0,1,3},y[]={1,7,3,7,7,7},out[6];
   double coef[2],intercept[2],saved[2],lambda[]={2,4};int j;
   CHECK(!THD_ridge_get_state(w,&s,err,sizeof(err)),"unfitted ridge view rejected");
   CHECK(!s.coef && !s.n,"failed view cleared");
   CHECK(THD_ridge_fit(w,x,y,3,1,2,2,THD_RIDGE_PRIMAL,err,sizeof(err)),err);
   CHECK(THD_ridge_get_state(w,&s,err,sizeof(err)),err);
   CHECK(s.n==3 && s.p==1 && s.q==2 && !s.group && !s.ngroup,"active dimensions and no groups");
   CHECK(s.xm[0]==4.0/3 && s.ym[0]==11.0/3 && s.ym[1]==7,"training means");
   CHECK(s.xm[0]!=(double)(float)s.xm[0],"view preserves double precision");
   /* Sxx=14/3, Sxy=28/3: slope=7/5, intercept=9/5. */
   CHECK(CLOSE(s.coef[0],1.4) && s.coef[1]==0,"analytic ridge coefficients");
   CHECK(THD_ridge_coefficients(w,coef,intercept,err,sizeof(err)),err);
   CHECK(CLOSE(intercept[0],1.8) && intercept[1]==7,"unpenalized intercept");
   CHECK(!memcmp(coef,s.coef,sizeof(coef)),"export preserves doubles");
   memcpy(saved,coef,sizeof(saved));coef[0]=999;
   CHECK(s.coef[0]==saved[0],"export buffers are caller-owned");
   CHECK(THD_ridge_predict(w,x,3,out,err,sizeof(err)),err);
   CHECK(s.coef[0]==saved[0],"prediction does not modify the borrowed state");
   CHECK(THD_ridge_fit(w,x,y,3,1,2,2,THD_RIDGE_DUAL,err,sizeof(err)),err);
   CHECK(THD_ridge_get_state(w,&s,err,sizeof(err)),err);
   for(j=0;j<2;j++)CHECK(CLOSE(saved[j],s.coef[j]),"primal/dual agreement");
   CHECK(THD_ridge_fit_response_lambda(w,x,y,3,1,2,lambda,THD_RIDGE_AUTO,err,sizeof(err)),err);
   CHECK(THD_ridge_get_state(w,&s,err,sizeof(err)),err);
   CHECK(CLOSE(s.coef[0],1.4) && s.coef[1]==0,"per-response penalties");
   CHECK(!THD_ridge_fit(w,x,y,3,1,2,0,THD_RIDGE_AUTO,err,sizeof(err)),"bad refit rejected");
   CHECK(!THD_ridge_get_state(w,&s,err,sizeof(err)) && !s.coef && !s.n,"failed refit invalidates view");
   CHECK(!THD_ridge_predict(w,x,3,out,err,sizeof(err)),"failed refit invalidates prediction");
   CHECK(THD_ridge_fit(w,x,y,3,1,1,2,THD_RIDGE_AUTO,err,sizeof(err)),err);
   CHECK(THD_ridge_get_state(w,&s,err,sizeof(err)) && s.q==1,"refit updates active stride");
   CHECK(!THD_ridge_get_state(w,NULL,err,sizeof(err)),"NULL view rejected");
   return 0;
}

static int regression_contract(THD_regression *m,int method)
{
   char err[256];THD_regression_state s;
   float x[]={0,1,3},y[]={1,7,3,7,7,7},out[6];
   double coef[2],intercept[2],saved[2];int i,o;
   CHECK(!THD_regression_get_state(m,&s,err,sizeof(err)) && !s.coef,"unfitted view rejected");
   CHECK(!THD_regression_diagnostics(m),"unfitted diagnostics rejected");
   CHECK(THD_regression_fit(m,x,y,3,1,2,0,err,sizeof(err)),err);
   CHECK(THD_regression_get_state(m,&s,err,sizeof(err)),err);
   CHECK(s.n==3 && s.p==1 && s.q==2,"active regression dimensions");
   CHECK(s.ym[0]==11.0/3 && s.ym[1]==7,"training response means for every method");
   CHECK(s.xm[0]==(method==THD_REG_SVR?0:4.0/3),"documented centering offset");
   if(method==THD_REG_BANDED_RIDGE)
      CHECK(s.ngroup==1 && s.group && s.group[0]==0,"copied feature groups");
   else CHECK(!s.group && !s.ngroup,"groups only for banded ridge");
   CHECK(THD_regression_coefficients(m,coef,intercept,err,sizeof(err)),err);
   CHECK(!memcmp(coef,s.coef,sizeof(coef)),"coefficient export agrees with view");
   memcpy(saved,coef,sizeof(saved));coef[0]=999;
   CHECK(s.coef[0]==saved[0],"export does not alias solver storage");
   CHECK(THD_regression_predict(m,x,3,out,err,sizeof(err)),err);
   for(i=0;i<3;i++)for(o=0;o<2;o++)
      CHECK(out[i*2+o]==(float)(intercept[o]+x[i]*s.coef[o]),"prediction reconstructed from exported state");
   CHECK(THD_regression_diagnostics(m),"fitted diagnostics available");
   if(method==THD_REG_PLS)CHECK(CLOSE(s.coef[0],2),"rank-one PLS recovers exact slope");
   if(method==THD_REG_ELASTIC_NET)
      CHECK(CLOSE(s.coef[0],1.4),"rho=0 uses n*alpha ridge penalty units");
   if(method==THD_REG_BANDED_RIDGE)
      CHECK(CLOSE(s.coef[0],1.4),"banded penalty uses unnormalized ridge units");
   /* Zero predictors expose the SVR distinction between mean and intercept. */
   memset(x,0,sizeof(x));
   CHECK(THD_regression_fit(m,x,y,3,1,2,0,err,sizeof(err)),err);
   CHECK(THD_regression_get_state(m,&s,err,sizeof(err)),err);
   CHECK(THD_regression_coefficients(m,coef,intercept,err,sizeof(err)),err);
   CHECK(s.ym[1]==7,"constant response mean retained");
   if(method==THD_REG_SVR){
      /* C=1, epsilon=.2: b=2*n*C*(7-epsilon)/(1+2*n*C). */
      CHECK(CLOSE(intercept[1],6*6.8/7),"analytic SVR penalized intercept");
      CHECK(s.ym[1]!=intercept[1],"SVR baseline mean is not its intercept");
   }else CHECK(intercept[1]==7,"centered estimators preserve constant response");
   x[0]=NAN;
   CHECK(!THD_regression_fit(m,x,y,3,1,2,0,err,sizeof(err)),"nonfinite refit rejected");
   CHECK(!THD_regression_get_state(m,&s,err,sizeof(err)) && !s.coef && !s.n,"failed refit invalidates state");
   CHECK(!THD_regression_diagnostics(m),"failed refit invalidates diagnostics");
   CHECK(!THD_regression_predict(m,x,3,out,err,sizeof(err)),"failed refit invalidates predictions");
   x[0]=0;
   CHECK(THD_regression_fit(m,x,y,3,1,1,0,err,sizeof(err)),err);
   CHECK(THD_regression_get_state(m,&s,err,sizeof(err)) && s.q==1,"successful refit restores state");
   CHECK(!THD_regression_get_state(m,NULL,err,sizeof(err)),"NULL view rejected");
   return 0;
}

static int spd_contract(void)
{
   double a[]={4,0,2,3},b[]={8,2,8,-2},bad[]={-1};
   CHECK(THD_regression_spd_solve(a,b,2,2),"SPD multiple RHS solve");
   CHECK(CLOSE(b[0],1) && CLOSE(b[1],1.25) && CLOSE(b[2],2) && CLOSE(b[3],-1.5),"known SPD solution");
   CHECK(!THD_regression_spd_solve(bad,b,1,1),"nonpositive pivot rejected");
   bad[0]=NAN;
   CHECK(!THD_regression_spd_solve(bad,b,1,1),"nonfinite pivot rejected");
   return 0;
}

/* A path callback checks each warm solution against a separately cold solve.
   Duplicate columns make coefficient uniqueness inappropriate for lasso;
   compare fitted values and objectives as well as full KKT residuals. */
typedef struct {
   THD_elastic_workspace *cold;
   const double *x, *y, *lambda;
   int calls;
} ElasticCheck;
static int elastic_check_path(void *context, int candidate,
   const THD_elastic_result *r, char *err, int nerr)
{
   ElasticCheck *c = context;
   const THD_elastic_result *cold;
   double lambda[2] = {c->lambda[candidate], c->lambda[candidate]};
   int i, j, o;
   if (!THD_elastic_fit(c->cold, c->y, r->q, lambda, 0, err, nerr))
      return 0;
   cold = THD_elastic_get_result(c->cold);
   for (o = 0; o < r->q; o++) {
      if (r->diagnostic[o].solver.residual > 1e-11 ||
          fabs(r->diagnostic[o].solver.objective -
               cold->diagnostic[o].solver.objective) > 1e-9)
         return 0;
      for (i = 0; i < r->n; i++) {
         double delta = 0;
         for (j = 0; j < r->p; j++)
            delta += c->x[i * r->p + j] *
               (r->coef[j * r->q + o] - cold->coef[j * r->q + o]);
         if (fabs(delta) > 1e-8) return 0;
      }
   }
   c->calls++;
   return 1;
}
static int elastic_reject(void *context, int candidate,
   const THD_elastic_result *r, char *err, int nerr)
{
   (void)context; (void)candidate; (void)r; (void)err; (void)nerr;
   return 0;
}
static int prepared_elastic_contract(void)
{
   char err[256];
   double x[] = {1,1,0, 2,2,0, -1,-1,0, 3,3,0};
   double y[] = {2,0, 4,0, -2,0, 6,0}, grid[] = {10,1,.1};
   double lambda[] = {.1,.1}, bad[] = {NAN,.1};
   int scaled, endpoint;
   for (scaled = 0; scaled < 2; scaled++)
      for (endpoint = 0; endpoint < 3; endpoint++) {
         THD_elastic_config cfg = {endpoint * .5, 1e-11, 100000, scaled};
         THD_elastic_design *d = THD_elastic_prepare(x,4,3,&cfg,err,256);
         THD_elastic_workspace *w, *cold;
         ElasticCheck check;
         CHECK(d,err);
         CHECK(THD_elastic_scales(d)[2] == 1,"zero column scale");
         w = THD_elastic_workspace_alloc(d,2,err,256);
         cold = THD_elastic_workspace_alloc(d,2,err,256);
         CHECK(w && cold,err);
         check = (ElasticCheck){cold,x,y,grid,0};
         CHECK(!THD_elastic_fit(w,y,2,lambda,1,err,256),
               "warm start needs fitted state");
         CHECK(THD_elastic_path(w,y,2,grid,3,elastic_check_path,&check,
                                err,256),err);
         CHECK(check.calls == 3,"all path steps visited");
         CHECK(THD_elastic_get_result(w)->diagnostic[1].solver.nonzero == 0,
               "zero response stays zero");
         CHECK(!THD_elastic_fit(w,y,2,bad,0,err,256),"nonfinite lambda");
         CHECK(!THD_elastic_get_result(w),"failure invalidates result");
         CHECK(!THD_elastic_fit(w,y,2,lambda,1,err,256),
               "failure invalidates warm state");
         CHECK(THD_elastic_fit(w,y,2,lambda,0,err,256),err);
         CHECK(!THD_elastic_path(w,y,2,grid,3,elastic_reject,NULL,err,256),
               "callback rejection aborts");
         CHECK(!THD_elastic_get_result(w),"aborted path has no result");
         CHECK(THD_elastic_fit(w,y,1,lambda,0,err,256),err);
         THD_elastic_workspace_free(w);
         THD_elastic_workspace_free(cold);
         THD_elastic_design_free(d);
      }
   {
      /* No implicit centering: one constant predictor is penalized unless
         the caller has explicitly removed its span as nuisance. */
      double ones[] = {1,1,1}, response[] = {3,3,3}, penalty[] = {1};
      THD_elastic_config cfg = {1,1e-12,100,0};
      THD_elastic_design *d = THD_elastic_prepare(ones,3,1,&cfg,err,256);
      THD_elastic_workspace *w = THD_elastic_workspace_alloc(d,1,err,256);
      CHECK(w && THD_elastic_fit(w,response,1,penalty,0,err,256),err);
      CHECK(CLOSE(THD_elastic_get_result(w)->coef[0],2),"analytic lasso");
      CHECK(CLOSE(THD_elastic_get_result(w)->diagnostic[0].lambda_max,3),
            "analytic all-zero threshold");
      penalty[0] = 3;
      CHECK(THD_elastic_fit(w,response,1,penalty,0,err,256),err);
      CHECK(THD_elastic_get_result(w)->coef[0] == 0,"lambda_max is zero");
      THD_elastic_workspace_free(w);
      THD_elastic_design_free(d);
   }
   return 0;
}

/* Prepared and legacy wrappers must agree after explicitly reproducing only
   legacy centering and n*lambda conversion. Both primal and dual cases run;
   independent augmented least-squares checks live in the Python CLI tests. */
static int prepared_banded_contract(void)
{
   int wide, i, j, o, c;
   char err[256];
   for (wide = 0; wide < 2; wide++) {
      int n = wide ? 4 : 9, p = wide ? 7 : 3, q = 3;
      float x[63], y[27];
      double xd[63], yd[27], mean;
      int group[7];
      double penalty[] = {.2, 1.3, 2.1, .15};
      double legacy_pen[] = {n * .2, n * 1.3, n * 2.1, n * .15},
             params[] = {1, 0, 1, 0};
      THD_banded_config cfg = {2, 2, group, penalty};
      THD_banded_design *d;
      THD_banded_workspace *w;
      THD_regression_config old = {
          THD_REG_BANDED_RIDGE, 2, 2, p, 100, 1e-9, params, group, legacy_pen};
      THD_regression *legacy;
      const THD_banded_result *result;
      THD_regression_state state;
      double indices[] = {0, 1, 0}, saved[21];
      for (j = 0; j < p; j++)
         group[j] = j % 2;
      for (i = 0; i < n; i++) {
         for (j = 0; j < p; j++)
            x[i * p + j] = (float)(sin(i * 1.3 + j * .7) + i * .03 * j);
         for (o = 0; o < q; o++)
            y[i * q + o] = (float)(cos(i * .8 + o) + i * .2);
      }
      for (j = 0; j < p; j++) {
         mean = 0;
         for (i = 0; i < n; i++)
            mean += x[i * p + j];
         mean /= n;
         for (i = 0; i < n; i++)
            xd[i * p + j] = x[i * p + j] - mean;
      }
      for (o = 0; o < q; o++) {
         mean = 0;
         for (i = 0; i < n; i++)
            mean += y[i * q + o];
         mean /= n;
         for (i = 0; i < n; i++)
            yd[i * q + o] = y[i * q + o] - mean;
      }
      legacy = THD_regression_alloc(n, p, q, &old, err, sizeof(err));
      CHECK(legacy, err);
      d = THD_banded_prepare(xd, n, p, &cfg, err, sizeof(err));
      CHECK(d, err);
      w = THD_banded_workspace_alloc(d, q, err, sizeof(err));
      CHECK(w, err);
      /* Prepared state owns these inputs. Mutating them must not affect fit. */
      penalty[0] = NAN;
      group[0] = -1;
      xd[0] = NAN;
      CHECK(THD_banded_fit(w, yd, q, indices, err, sizeof(err)), err);
      result = THD_banded_get_result(w);
      CHECK(result, "banded result available");
      for (j = 0; j < p * q; j++)
         saved[j] = result->coef[j];
      for (c = 0; c < 2; c++) {
         CHECK(THD_regression_fit(legacy, x, y, n, p, q, c, err, sizeof(err)),
               err);
         CHECK(THD_regression_get_state(legacy, &state, err, sizeof(err)), err);
         for (o = 0; o < q; o++)
            if (indices[o] == c)
               for (j = 0; j < p; j++)
                  CHECK(fabs(saved[j * q + o] - state.coef[j * q + o]) < 1e-11,
                        "prepared/legacy banded units and centering parity");
      }
      indices[0] = .5;
      CHECK(!THD_banded_fit(w, yd, q, indices, err, sizeof(err)) &&
                !THD_banded_get_result(w),
            "invalid index invalidates result");
      indices[0] = 0;
      yd[0] = NAN;
      CHECK(!THD_banded_fit(w, yd, q, indices, err, sizeof(err)) &&
                !THD_banded_get_result(w),
            "nonfinite invalidates banded result");
      THD_banded_workspace_free(w);
      THD_banded_design_free(d);
      THD_regression_free(legacy);
   }
   return 0;
}

int main(void)
{
   char err[256];int failed=0,method;THD_regression_state s;
   THD_ridge_workspace *w=THD_ridge_alloc(3,3,2,err,sizeof(err));
   CHECK(w,err);
   failed+=ridge_contract(w);THD_ridge_free(w);
   CHECK(!THD_ridge_get_state(NULL,&s,err,sizeof(err)) && !s.coef,"NULL ridge rejected");
   CHECK(!THD_regression_get_state(NULL,&s,err,sizeof(err)) && !s.coef,"NULL regression rejected");
   for(method=THD_REG_ELASTIC_NET;method<=THD_REG_SVR;method++){
      double parameter[]={method==THD_REG_ELASTIC_NET?2.0/3:1,method==THD_REG_SVR?.2:0};
      double penalty[]={2};int group[]={0};
      THD_regression_config cfg={method,1,1,1,100000,1e-11,parameter,group,penalty};
      THD_regression *m=THD_regression_alloc(3,1,2,&cfg,err,sizeof(err));
      CHECK(m,err);
      /* Allocated models must not borrow the caller's configuration arrays. */
      parameter[0]=NAN;penalty[0]=NAN;group[0]=-1;
      failed+=regression_contract(m,method);THD_regression_free(m);
   }
   failed+=spd_contract();
   failed+=prepared_elastic_contract();
   failed+=prepared_banded_contract();
   if(failed)return 1;
   puts("PASS standalone regression state, ownership, precision, units, and failure contracts");
   return 0;
}
