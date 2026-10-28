#include "thd_mvpa.h"

#include <float.h>
#include <math.h>

#define CHECK(COND,MSG) do{ if(!(COND)){ fprintf(stderr,"FAIL: %s\n",MSG); fail=1; goto done; } }while(0)
#define CLOSE(A,B) (fabs((double)(A)-(double)(B)) < 1.0e-6)

static void fixed_plan( THD_foldplan *p, int nsample, int *train, int ntrain,
                        int *test, int ntest, int *sample_fold )
{
   static char *name[1]={"test"},*stratum_label[1]={"All"} ;
   static int fold_stratum[1]={0},ntr[1],nte[1] ;
   static int *tr[1],*te[1] ;
   memset(p,0,sizeof(*p)) ; ntr[0]=ntrain ; nte[0]=ntest ; tr[0]=train ; te[0]=test ;
   p->kind=THD_CV_FIXED ; p->nsample=nsample ; p->nfold=1 ; p->nstratum=1 ;
   p->name=name ; p->fold_stratum=fold_stratum ; p->stratum_label=stratum_label ;
   p->ntrain=ntr ; p->ntest=nte ; p->train=tr ; p->test=te ; p->sample_test_fold=sample_fold ;
}

static void paired_three_fold_plan( THD_foldplan *p )
{
   static char *name[3]={"pair1","pair2","pair3"},*stratum_label[1]={"All"} ;
   static char group_column[]="Pair" ;
   static int fold_stratum[3]={0,0,0},ntr[3]={4,4,4},nte[3]={2,2,2} ;
   static int tr0[]={2,3,4,5},tr1[]={0,1,4,5},tr2[]={0,1,2,3} ;
   static int te0[]={0,1},te1[]={2,3},te2[]={4,5},sample_fold[]={0,0,1,1,2,2} ;
   static int *tr[3]={tr0,tr1,tr2},*te[3]={te0,te1,te2} ;
   memset(p,0,sizeof(*p)) ;
   p->kind=THD_CV_LEAVE_GROUP_OUT ; p->nsample=6 ; p->nfold=3 ; p->nstratum=1 ;
   p->group_column=group_column ; p->name=name ; p->fold_stratum=fold_stratum ;
   p->stratum_label=stratum_label ; p->ntrain=ntr ; p->ntest=nte ;
   p->train=tr ; p->test=te ; p->sample_test_fold=sample_fold ;
}

static int nuisance_test(void)
{
   THD_foldplan plan;THD_mvpa_workspace *w=NULL;THD_nested_foldplan *nested=NULL;
   int tr[]={0,1,2,3,4,5,6,7},te[]={8,9},sf[]={-1,-1,-1,-1,-1,-1,-1,-1,0,0};
   int classes[]={0,0,0,0,1,1,1,1,0,1},groups[]={0,1,2,3,0,1,2,3,4,4};
   float z[20],x[20],saved[4],nulls[2];int i,j,perm[20],scratch[10],fail=0;char err[1024];
   signed char sign[20];PERM_set ps={0};
   fixed_plan(&plan,10,tr,8,te,2,sf);
   for(i=0;i<10;i++){
      z[2*i]=i<8?(i%4<2?-1:1):(i==8?2:-2);z[2*i+1]=i%2?1:-1;
      x[2*i]=5+3*z[2*i]-2*z[2*i+1]+(classes[i]?1:-1);
      x[2*i+1]=9+7*z[2*i];
   }
   CHECK(THD_mvpa_check_nuisance(&plan,NULL,z,2,err,sizeof(err)),err);
   nested=THD_nested_foldplan_grouped(&plan,groups,0,err,sizeof(err));CHECK(nested,err);
   CHECK(THD_mvpa_check_nuisance(&plan,nested,z,2,err,sizeof(err)),"inner rank validation uses global training indices");
   w=THD_mvpa_workspace_alloc(&plan,2,2,THD_PREP_FEATURE_ZSCORE,NULL,err,sizeof(err));CHECK(w,err);
   CHECK(THD_preprocess_set_nuisance(w->prep,z,10,2,err,sizeof(err)),err);
   CHECK(THD_mvpa_run_classification_cv(&plan,0,x,2,classes,NULL,w,err,sizeof(err)),err);
   CHECK(w->prediction[8]==0 && w->prediction[9]==1,"genuine signal survives removal of two nuisance effects");
   for(i=0;i<8;i++)CHECK(CLOSE(w->train_x[2*i],classes[i]?1:-1) && w->train_x[2*i+1]==0,
      "OLS removes confound-only feature before scaling, retaining orthogonal signal");
   for(i=0;i<4;i++)saved[i]=w->test_x[i];
   x[18]+=10000;z[18]+=100;
   CHECK(THD_mvpa_run_classification_cv(&plan,0,x,2,classes,NULL,w,err,sizeof(err)),err);
   CHECK(CLOSE(w->test_x[0],saved[0]) && CLOSE(w->test_x[1],saved[1]),"held-out perturbations cannot change another test prediction or fitted scaling");
   for(i=0;i<8;i++)CHECK(CLOSE(w->train_x[2*i],classes[i]?1:-1) && w->train_x[2*i+1]==0,"held-out perturbations leave training adjustment unchanged");
   x[18]-=10000;z[18]-=100;
   ps.nobs=10;ps.nperm=2;ps.perm=perm;ps.sign=sign;
   for(i=0;i<20;i++){perm[i]=i%10;sign[i]=1;}perm[10]=4;perm[14]=0;
   CHECK(THD_mvpa_run_classification_permutations(&plan,0,x,2,classes,NULL,THD_METRIC_ACCURACY,&ps,scratch,nulls,w,err,sizeof(err)),err);
   CHECK(w->prediction[8]==0 && w->prediction[9]==1 && nulls[0]==1,"nuisance-preserving permutations restore observed fit");
   perm[10]=1;perm[11]=4;
   CHECK(!THD_mvpa_run_classification_permutations(&plan,0,x,2,classes,NULL,THD_METRIC_ACCURACY,&ps,scratch,nulls,w,err,sizeof(err)) && strstr(err,"preserve every covariate"),"library rejects nuisance-changing permutations");
   CHECK(THD_preprocess_set_nuisance(w->prep,z,9,2,err,sizeof(err)),err);
   CHECK(!THD_mvpa_run_classification_permutations(&plan,0,x,2,classes,NULL,THD_METRIC_ACCURACY,&ps,scratch,nulls,w,err,sizeof(err)) && strstr(err,"dimensions"),"nuisance permutation dimensions checked before row access");
   for(i=0;i<10;i++)z[2*i+1]=2*z[2*i];
   CHECK(!THD_mvpa_check_nuisance(&plan,NULL,z,2,err,sizeof(err)) && strstr(err,"rank deficient"),"redundant covariates rejected");
   for(i=0;i<10;i++)for(j=0;j<2;j++)z[2*i+j]=1;
   CHECK(!THD_mvpa_check_nuisance(&plan,NULL,z,2,err,sizeof(err)) && strstr(err,"constant"),"constant nuisance column rejected");
   CHECK(THD_preprocess_set_nuisance(w->prep,NULL,0,0,err,sizeof(err)),"disable nuisance adjustment");
   CHECK(!THD_preprocess_apply(w->prep,x,10,2,te,2,NULL,saved,err,sizeof(err)),"configuration change invalidates previous fit");
done:
   THD_nested_foldplan_free(nested);THD_mvpa_workspace_free(w);return fail;
}

static int svm_test(void)
{
   int fail=0,i,j,pred[6],classes[]={0,1},majority[]={0,0,1};char err[1024];
   float x[]={-1,1},shifted[]={0,2},zero[]={0,0,0},wide[128]={0};
   double decision[12],saved[12],grid[]={10,.01},nulls[2],losses[6];
   float cvx[]={-1,1,-1,1,-1,1},null_score[2];int cvclass[]={0,1,0,1,0,1};
   int groups[]={0,0,1,1,2,2},permuted[6];
   THD_svm_model *m=NULL;THD_mvpa_workspace *w=NULL;THD_nested_foldplan *nested=NULL;
   THD_foldplan plan;PERM_set set;int perm[]={0,1,2,3,4,5, 1,0,2,3,4,5};
   signed char sign[]={1,1,1,1,1,1,1,1,1,1,1,1};
   m=THD_svm_alloc(6,64,2,err,sizeof(err));CHECK(m,err);
   CHECK(!THD_svm_predict(m,x,2,1,pred,decision,err,sizeof(err)),"unfitted SVM rejects prediction");
   CHECK(THD_svm_fit(m,x,classes,2,1,1,1e-10,10000,err,sizeof(err)),err);
   CHECK(THD_svm_predict(m,x,2,1,pred,decision,err,sizeof(err)),err);
   CHECK(pred[0]==0 && pred[1]==1 && CLOSE(decision[0],.8) && CLOSE(decision[1],-.8) && CLOSE(decision[3],.8),
         "native squared-hinge SVM matches analytic w=4C/(1+4C), b=0");
   CHECK(THD_svm_fit(m,shifted,classes,2,1,1,1e-10,10000,err,sizeof(err)),err);
   CHECK(THD_svm_predict(m,shifted,2,1,pred,decision,err,sizeof(err)),err);
   CHECK(CLOSE(decision[1],-16.0/29) && CLOSE(decision[3],24.0/29),
         "penalized intercept and slope match independent two-variable analytic solution");
   CHECK(THD_svm_fit(m,zero,majority,3,1,1,1e-10,10000,err,sizeof(err)),err);
   CHECK(THD_svm_predict(m,zero,3,1,pred,decision,err,sizeof(err)),err);
   CHECK(pred[0]==0 && CLOSE(decision[0],2.0/7) && CLOSE(decision[1],-2.0/7),
         "constant patterns with unequal class counts have analytic penalized biases");
   zero[0]=zero[1]=zero[2]=5;
   CHECK(THD_svm_fit(m,zero,majority,3,1,100,1e-10,1,err,sizeof(err)),err);
   CHECK(THD_svm_predict(m,zero,3,1,pred,decision,err,sizeof(err)),err);
   CHECK(CLOSE(decision[0],5200.0/15601) && CLOSE(decision[1],-5200.0/15601),
         "identical nonzero patterns at high C use exact constant-pattern reduction");
   wide[0]=-1;wide[64]=1;
   CHECK(THD_svm_fit(m,wide,classes,2,64,1,1e-10,10000,err,sizeof(err)),err);
   CHECK(THD_svm_predict(m,wide,2,64,pred,decision,err,sizeof(err)),err);
   CHECK(CLOSE(decision[0],.8) && CLOSE(decision[3],.8),"SVM handles p much greater than n and zero-variance redundant features");
   CHECK(!THD_svm_fit(m,shifted,classes,2,1,1,1e-12,1,err,sizeof(err)) && strstr(err,"did not converge"),
         "unfinished coordinate descent fails explicitly");
   CHECK(!THD_svm_predict(m,x,2,1,pred,decision,err,sizeof(err)),"failed SVM refit invalidates old fitted state");
   CHECK(!THD_svm_fit(m,x,classes,2,1,0,1e-6,10000,err,sizeof(err)),"SVM rejects nonpositive C");
   CHECK(!THD_svm_fit(m,x,classes,2,1,1,NAN,10000,err,sizeof(err)),"SVM rejects nonfinite tolerance");
   x[0]=NAN;CHECK(!THD_svm_fit(m,x,classes,2,1,1,1e-6,10000,err,sizeof(err)),"SVM rejects nonfinite predictors");x[0]=-1;
   THD_svm_free(m);m=NULL;
   {
      float multi[]={2,0,-2,0,0,2},probe[]={2,0,-2,0,0,2};int mc[]={0,1,2};
      m=THD_svm_alloc(3,2,3,err,sizeof(err));CHECK(m,err);
      CHECK(THD_svm_fit(m,multi,mc,3,2,.5,1e-9,10000,err,sizeof(err)),err);
      CHECK(THD_svm_predict(m,probe,3,2,pred,decision,err,sizeof(err)),err);
      for(i=0;i<3;i++)CHECK(pred[i]==i,"one-vs-rest multiclass argmax recovers three separable classes");
      memcpy(saved,decision,sizeof(double)*9);
      CHECK(THD_svm_fit(m,multi,mc,3,2,.5,1e-9,10000,err,sizeof(err)),err);
      CHECK(THD_svm_predict(m,probe,3,2,pred,decision,err,sizeof(err)),err);
      CHECK(!memcmp(saved,decision,sizeof(double)*9),"repeated SVM fits are byte deterministic");
      mc[2]=1;CHECK(!THD_svm_fit(m,multi,mc,3,2,.5,1e-9,10000,err,sizeof(err)) && strstr(err,"no class"),"SVM refuses missing one-vs-rest classes");
   }
   paired_three_fold_plan(&plan);
   nested=THD_nested_foldplan_grouped(&plan,groups,0,err,sizeof(err));CHECK(nested,err);
   w=THD_mvpa_workspace_alloc(&plan,1,2,THD_PREP_FEATURE_ZSCORE,NULL,err,sizeof(err));CHECK(w,err);
   CHECK(THD_mvpa_classification_set_svm(w,1,1e-9,10000,THD_METRIC_BALANCED_ACCURACY,nested,grid,2,err,sizeof(err)),err);
   CHECK(!THD_mvpa_run_centroid_cv(&plan,0,cvx,1,cvclass,NULL,w,err,sizeof(err)),"centroid entry point refuses an SVM workspace");
   CHECK(THD_mvpa_run_classification_cv(&plan,0,cvx,1,cvclass,NULL,w,err,sizeof(err)),err);
   CHECK(CLOSE(w->score.balanced_accuracy,1),"SVM fold runner scores known separable held-out patterns");
   for(i=0;i<3;i++)CHECK(w->svm_selected[i]==.01 && w->svm_loss[i*2]==0 && w->svm_loss[i*2+1]==0 &&
     w->fold_svm_residual[i]<=1e-9 && w->fold_svm_iterations[i]>0,"nested SVM ties choose smaller C and outer fits report verified convergence");
   memcpy(saved,w->decision,sizeof(saved));memcpy(losses,w->svm_loss,sizeof(losses));
   cvx[0]=-7;cvx[1]=9;
   CHECK(THD_mvpa_run_classification_cv(&plan,0,cvx,1,cvclass,NULL,w,err,sizeof(err)),err);
   CHECK(w->svm_selected[0]==.01 && CLOSE(w->decision[0],7*saved[0]) && CLOSE(w->decision[2],9*saved[2]),
         "outer held-out feature changes are transformed by unchanged training-only scaling and SVM fit");
   cvx[0]=-1;cvx[1]=1;
   cvclass[0]=1;cvclass[1]=0;
   CHECK(THD_mvpa_run_classification_cv(&plan,0,cvx,1,cvclass,NULL,w,err,sizeof(err)),err);
   CHECK(w->svm_selected[0]==.01 && CLOSE(w->decision[0],saved[0]) && CLOSE(w->decision[1],saved[1]),
         "outer held-out class changes cannot affect preprocessing, tuning, or model decisions for that fold");
   cvclass[0]=0;cvclass[1]=1;
   memset(&set,0,sizeof(set));set.nobs=6;set.nperm=2;set.perm=perm;set.sign=sign;
   CHECK(THD_mvpa_run_classification_permutations(&plan,0,cvx,1,cvclass,NULL,
      THD_METRIC_BALANCED_ACCURACY,&set,permuted,null_score,w,err,sizeof(err)),err);
   CHECK(!memcmp(saved,w->decision,sizeof(saved)) && !memcmp(losses,w->svm_loss,sizeof(losses)),
         "SVM permutations restore observed decisions and nested tuning audits exactly");
   for(i=0;i<2;i++){
      for(j=0;j<6;j++)permuted[j]=cvclass[perm[i*6+j]];
      CHECK(THD_mvpa_run_classification_cv(&plan,0,cvx,1,permuted,NULL,w,err,sizeof(err)),err);
      nulls[i]=w->score.balanced_accuracy;
      CHECK(CLOSE(nulls[i],null_score[i]),"SVM null scores match explicit complete preprocessing/tuning/model refits");
   }
done:
   THD_svm_free(m);THD_mvpa_workspace_free(w);THD_nested_foldplan_free(nested);return fail;
}

static int ridge_test(void)
{
   int fail=0,i;char err[512];
   float x[]={-1,1,3,-3},y[]={8,-1,12,5,16,11,4,-7},prediction[8];
   float wide[]={1,2,3,4,5, 2,1,0,-1,-2, 0,1,0,1,0};
   float wy[]={2,-1, 5,3, -2,4},primal[6],dual[6];
   double coef[10],intercept[2],primal_coef[10];
   int tr[]={0,1},te[]={2,3},sf[]={-1,-1,0,0};THD_foldplan plan;
   THD_ridge_workspace *r=NULL;THD_mvpa_ridge_workspace *w=NULL;
   r=THD_ridge_alloc(5,5,2,err,sizeof(err));CHECK(r,err);
   CHECK(THD_ridge_fit(r,x,y,2,1,2,2,THD_RIDGE_AUTO,err,sizeof(err)),err);
   CHECK(THD_ridge_coefficients(r,coef,intercept,err,sizeof(err)),err);
   CHECK(CLOSE(coef[0],1) && CLOSE(coef[1],1.5) && CLOSE(intercept[0],10) && CLOSE(intercept[1],2),
         "exported ridge slopes/intercepts match analytic multi-response answers");
   CHECK(THD_ridge_predict(r,x,4,prediction,err,sizeof(err)),err);
   CHECK(CLOSE(prediction[4],13) && CLOSE(prediction[5],6.5) &&
         CLOSE(prediction[6],7) && CLOSE(prediction[7],-2.5),
         "ridge slopes shrink while both unpenalized intercepts match known answers");
   CHECK(THD_ridge_fit(r,wide,wy,3,5,2,0.7,THD_RIDGE_PRIMAL,err,sizeof(err)),err);
   CHECK(THD_ridge_predict(r,wide,3,primal,err,sizeof(err)),err);
   CHECK(THD_ridge_coefficients(r,primal_coef,intercept,err,sizeof(err)),err);
   CHECK(THD_ridge_fit(r,wide,wy,3,5,2,0.7,THD_RIDGE_DUAL,err,sizeof(err)),err);
   CHECK(THD_ridge_predict(r,wide,3,dual,err,sizeof(err)),err);
   CHECK(THD_ridge_coefficients(r,coef,intercept,err,sizeof(err)),err);
   for(i=0;i<10;i++)CHECK(CLOSE(primal_coef[i],coef[i]),"primal/dual exported slopes agree");
   for(i=0;i<6;i++){
      int j;double v=intercept[i%2];
      for(j=0;j<5;j++)v+=wide[(i/2)*5+j]*coef[j*2+i%2];
      CHECK(CLOSE(v,dual[i]),"exported weights and intercept reconstruct predictions");
   }
   for(i=0;i<6;i++)CHECK(CLOSE(primal[i],dual[i]),"primal/dual multi-output predictions agree on a rank-deficient wide matrix");
   CHECK(!THD_ridge_fit(r,x,y,2,1,2,0,THD_RIDGE_AUTO,err,sizeof(err)),"zero ridge penalty is rejected");
   CHECK(!THD_ridge_predict(r,x,4,prediction,err,sizeof(err)),"failed refit invalidates the previous ridge model");
   CHECK(!THD_ridge_coefficients(r,coef,intercept,err,sizeof(err)),"failed ridge refit cannot export stale coefficients");
   fixed_plan(&plan,4,tr,2,te,2,sf);
   w=THD_mvpa_ridge_workspace_alloc(&plan,1,2,THD_PREP_NONE,err,sizeof(err));CHECK(w,err);
   CHECK(THD_mvpa_run_ridge_cv(w,x,1,y,2,NULL,2,err,sizeof(err)),err);
   CHECK(isnan(w->prediction[0]) && CLOSE(w->prediction[4],13) &&
         CLOSE(w->baseline[4],10) && CLOSE(w->baseline[5],2) &&
         CLOSE(w->score[0].cv_r2,0.75) && CLOSE(w->score[0].mae,3) &&
         CLOSE(w->fold_score[0].cv_r2,0.75),
         "ridge CV retains training-only baselines, held-out predictions, and analytic scores");
   y[4]=1000;y[5]=-1000;
   CHECK(THD_mvpa_run_ridge_cv(w,x,1,y,2,NULL,2,err,sizeof(err)),err);
   CHECK(CLOSE(w->prediction[4],13) && CLOSE(w->prediction[5],6.5) && CLOSE(w->baseline[4],10),
         "changing held-out responses cannot affect model predictions or baselines");
   THD_mvpa_ridge_workspace_free(w);w=NULL;
   x[0]=1;x[1]=3;x[2]=100;x[3]=-100;
   w=THD_mvpa_ridge_workspace_alloc(&plan,1,2,THD_PREP_FEATURE_ZSCORE,err,sizeof(err));CHECK(w,err);
   CHECK(THD_mvpa_run_ridge_cv(w,x,1,y,2,NULL,2,err,sizeof(err)),err);
   CHECK(CLOSE(w->prediction[4],108) && CLOSE(w->prediction[5],149) &&
         CLOSE(w->prep->center[0],2) && CLOSE(w->prep->scale[0],1),
         "ridge CV predictor scaling uses only the two training samples");
   x[3]=NAN;
   CHECK(!THD_mvpa_run_ridge_cv(w,x,1,y,2,NULL,2,err,sizeof(err)),"ridge CV rejects nonfinite predictors");
done:
   THD_ridge_free(r);THD_mvpa_ridge_workspace_free(w);return fail;
}

static int nested_ridge_test(void)
{
   int fail=0,i,j;char err[512];
   float x[]={-1,1,-1,1,-1,1,-1,1};
   float y[]={8,17,12,23,8,17,12,23,8,17,12,23,8,17,12,23};
   int tr[]={0,1,2,3,4,5},te[]={6,7},sf[]={-1,-1,-1,-1,-1,-1,0,0};
   int group[]={0,0,1,1,2,2,99,99},onegroup[]={0,0,0,0,0,0,1,1};
   double grid[]={4,1};THD_foldplan plan;
   THD_nested_foldplan *nested=NULL,*balanced=NULL;
   THD_mvpa_ridge_workspace *w=NULL;
   fixed_plan(&plan,8,tr,6,te,2,sf);
   nested=THD_nested_foldplan_grouped(&plan,group,0,err,sizeof(err));CHECK(nested,err);
   CHECK(nested->inner[0]->nsample==6 && nested->inner[0]->nfold==3,
         "nested folds exclude outer test rows and groups");
   for(i=0;i<3;i++){
      CHECK(nested->inner[0]->ntrain[i]==4 && nested->inner[0]->ntest[i]==2,
            "inner grouped splits have expected sample counts");
      for(j=0;j<2;j++)CHECK(nested->inner[0]->test[i][j]/2==i,
            "inner grouping never splits a repeated group");
   }
   balanced=THD_nested_foldplan_grouped(&plan,group,2,err,sizeof(err));CHECK(balanced,err);
   CHECK(balanced->inner[0]->nfold==2 && balanced->inner[0]->ntest[0]==4 &&
         balanced->inner[0]->ntest[1]==2,"nested grouped K-fold balances whole groups deterministically");
   CHECK(!THD_nested_foldplan_grouped(&plan,onegroup,0,err,sizeof(err)) && strstr(err,"inner groups"),
         "nested tuning rejects an outer training set with only one inner group");
   CHECK(!THD_nested_foldplan_grouped(&plan,group,4,err,sizeof(err)),
         "nested tuning rejects K exceeding outer training group count");
   w=THD_mvpa_ridge_workspace_alloc(&plan,1,2,THD_PREP_FEATURE_ZSCORE,err,sizeof(err));CHECK(w,err);
   CHECK(THD_mvpa_ridge_set_tuning(w,nested,grid,2,err,sizeof(err)),err);
   CHECK(THD_mvpa_run_ridge_cv(w,x,1,y,2,NULL,1000,err,sizeof(err)),err);
   CHECK(CLOSE(w->selected_lambda[0],1) && w->inner_nvalid[0]==2 &&
         CLOSE(w->inner_loss[0],0.25) && CLOSE(w->inner_loss[1],0.04),
         "nested multi-output normalized errors and selected lambda match analytic answers");
   CHECK(CLOSE(w->prediction[12],10-12.0/7) && CLOSE(w->prediction[13],20-18.0/7) &&
         CLOSE(w->baseline[12],10) && CLOSE(w->score[0].cv_r2,48.0/49),
         "selected lambda is refitted on all outer training samples before prediction");
   y[12]=10000;y[13]=-10000;x[6]=1000;
   CHECK(THD_mvpa_run_ridge_cv(w,x,1,y,2,NULL,1,err,sizeof(err)),err);
   CHECK(CLOSE(w->selected_lambda[0],1) && CLOSE(w->inner_loss[0],0.25) &&
         CLOSE(w->inner_loss[1],0.04) && CLOSE(w->prep->center[0],0) && CLOSE(w->baseline[12],10),
         "outer held-out predictors and responses cannot change inner losses, selection, or fitted preprocessing");
   for(i=0;i<6;i++){x[i]=(float)(10*(i/2)+2*(i%2));y[i*2]=2*x[i];y[i*2+1]=20+3*x[i];}
   CHECK(THD_mvpa_run_ridge_cv(w,x,1,y,2,NULL,1,err,sizeof(err)),err);
   CHECK(CLOSE(w->inner_loss[0],0.25) && CLOSE(w->inner_loss[1],0.04),
         "inner preprocessing refits changing group means/scales instead of reusing outer statistics");
   for(i=0;i<6;i++){y[i*2]=10;y[i*2+1]=20;}
   CHECK(THD_mvpa_run_ridge_cv(w,x,1,y,2,NULL,1,err,sizeof(err)),err);
   CHECK(CLOSE(w->selected_lambda[0],4) && w->inner_nvalid[0]==0 && isnan(w->inner_loss[0]),
         "uninformative responses deterministically select the largest lambda with undefined tuning loss");
done:
   THD_mvpa_ridge_workspace_free(w);THD_nested_foldplan_free(nested);
   THD_nested_foldplan_free(balanced);return fail;
}

static int ridge_permutation_test(void)
{
   int fail=0,i,o;char err[512];THD_foldplan plan;
   int group[]={0,0,1,1,2,2},perm[]={0,1,2,3,4,5, 1,0,2,3,4,5};
   signed char sign[12];PERM_set ps;
   float x[]={-1,1,-1,1,-1,1},y[]={8,17,12,23,8,17,12,23,8,17,12,23};
   float shuffled[12],nulls[4],observed[12],manual[2];double grid[]={.1,10};
   THD_nested_foldplan *nested=NULL;THD_mvpa_ridge_workspace *w=NULL;
   paired_three_fold_plan(&plan);memset(&ps,0,sizeof(ps));memset(sign,1,sizeof(sign));
   ps.nobs=6;ps.nperm=2;ps.perm=perm;ps.sign=sign;
   nested=THD_nested_foldplan_grouped(&plan,group,0,err,sizeof(err));CHECK(nested,err);
   w=THD_mvpa_ridge_workspace_alloc(&plan,1,2,THD_PREP_FEATURE_ZSCORE,err,sizeof(err));CHECK(w,err);
   CHECK(THD_mvpa_ridge_set_tuning(w,nested,grid,2,err,sizeof(err)),err);
   CHECK(THD_mvpa_run_ridge_cv(w,x,1,y,2,NULL,1,err,sizeof(err)),err);
   memcpy(observed,w->prediction,sizeof(observed));
   for(i=0;i<6;i++)for(o=0;o<2;o++)shuffled[i*2+o]=y[perm[6+i]*2+o];
   CHECK(THD_mvpa_run_ridge_cv(w,x,1,shuffled,2,NULL,1,err,sizeof(err)),err);
   manual[0]=(float)w->score[0].cv_r2;manual[1]=(float)w->score[1].cv_r2;
   CHECK(CLOSE(w->selected_lambda[1],10),"a permuted response changes the inner-selected penalty");
   CHECK(THD_mvpa_run_ridge_permutations(w,x,1,y,2,NULL,1,&ps,0,
         THD_METRIC_CROSS_VALIDATED_R2,0,shuffled,nulls,err,sizeof(err)),err);
   CHECK(CLOSE(nulls[2],manual[0]) && CLOSE(nulls[3],manual[1]),
         "response-row permutations match explicit complete nested refits for both outputs");
   CHECK(!memcmp(w->prediction,observed,sizeof(observed)) && CLOSE(w->selected_lambda[1],.1),
         "permutation runner restores observed predictions and selected penalties");
   CHECK(CLOSE(nulls[0],w->score[0].cv_r2),"identity null slot equals observed score");
   CHECK(THD_mvpa_run_ridge_cv(w,x,1,y,2,NULL,1,err,sizeof(err)),err);
   for(i=0;i<6;i++)shuffled[i]=x[perm[6+i]];
   CHECK(THD_mvpa_run_ridge_cv(w,shuffled,1,y,2,NULL,1,err,sizeof(err)),err);
   manual[0]=(float)((w->score[0].mae+w->score[1].mae)/2);
   CHECK(THD_mvpa_run_ridge_permutations(w,x,1,y,2,NULL,1,&ps,1,
         THD_METRIC_MAE,1,shuffled,nulls,err,sizeof(err)),err);
   CHECK(CLOSE(nulls[1],manual[0]) && !memcmp(w->prediction,observed,sizeof(observed)),
         "design-row shuffling matches explicit encoding refits and restores observed state");
   for(i=0;i<6;i++)y[i*2+1]=5;
   CHECK(THD_mvpa_run_ridge_permutations(w,x,1,y,2,NULL,1,&ps,0,
         THD_METRIC_PEARSON,0,shuffled,nulls,err,sizeof(err)),err);
   CHECK(isnan(nulls[1]) && isnan(nulls[3]),"undefined permutation metrics remain explicit rather than dropping null slots");
   perm[7]=1;
   CHECK(!THD_mvpa_run_ridge_permutations(w,x,1,y,2,NULL,1,&ps,0,
         THD_METRIC_MAE,0,shuffled,nulls,err,sizeof(err)),"continuous permutation runner rejects non-bijective relabelings");
done:
   THD_mvpa_ridge_workspace_free(w);THD_nested_foldplan_free(nested);return fail;
}

static int ridge_classification_test(void)
{
   int fail=0,i,j;char err[512];THD_foldplan plan;
   int train[]={0,1,2,3,4,5},test[]={6,7,8},sf[]={-1,-1,-1,-1,-1,-1,0,0,0};
   int classes[]={0,1,2,0,1,2,0,1,2},groups[]={0,0,0,1,1,1,2,2,2};
   float x[27],constant[27]={0},wide[81];double grid[]={10,1},saved[9];
   int index[]={0,1,2,3,4,5,6,7,8, 1,0,2,3,4,5,6,7,8};
   signed char sign[18];int permuted[9];float nullscore[2];PERM_set ps;
   THD_mvpa_workspace *w=NULL;THD_nested_foldplan *nested=NULL;
   fixed_plan(&plan,9,train,6,test,3,sf);
   for(i=0;i<9;i++)for(j=0;j<3;j++)x[i*3+j]=(i%3==j)?1:0;
   nested=THD_nested_foldplan_grouped(&plan,groups,0,err,sizeof(err));CHECK(nested,err);
   w=THD_mvpa_workspace_alloc(&plan,9,3,THD_PREP_NONE,NULL,err,sizeof(err));CHECK(w,err);
   CHECK(!THD_mvpa_classification_set_ridge(w,0,NULL,NULL,0,err,sizeof(err)),"classifier rejects zero penalty");
   CHECK(THD_mvpa_classification_set_ridge(w,1,nested,grid,2,err,sizeof(err)),err);
   CHECK(!THD_mvpa_classification_set_ridge(w,1,NULL,NULL,0,err,sizeof(err)),"configure ridge classifier only once");
   CHECK(THD_mvpa_run_classification_cv(&plan,0,x,3,classes,NULL,w,err,sizeof(err)),err);
   CHECK(CLOSE(w->score.accuracy,1) && w->prediction[0]==-1 && isnan(w->decision[0]),
         "multiclass ridge scores only held-out rows and preserves original sample order");
   for(i=6;i<9;i++)for(j=0;j<3;j++){
     double v=w->decision[i*3+j];saved[(i-6)*3+j]=v;
     CHECK(CLOSE(v,j==i%3?7.0/9:1.0/9),"multiclass ridge decisions match analytic one-hot regression");
   }
   CHECK(CLOSE(w->ridge_cv->selected_lambda[0],1) &&
         CLOSE(w->ridge_cv->inner_loss[0],100.0/121) && CLOSE(w->ridge_cv->inner_loss[1],.25),
         "classifier tuning matches analytic class-wise squared-error losses");
   classes[6]=2;
   CHECK(THD_mvpa_run_classification_cv(&plan,0,x,3,classes,NULL,w,err,sizeof(err)),err);
   CHECK(!memcmp(saved,w->decision+18,sizeof(saved)) && CLOSE(w->ridge_cv->selected_lambda[0],1),
         "held-out labels cannot change ridge decisions or lambda selection");
   classes[6]=0;
   classes[0]=1;classes[1]=0;
   CHECK(THD_mvpa_run_classification_cv(&plan,0,x,3,classes,NULL,w,err,sizeof(err)),err);
   CHECK(CLOSE(w->ridge_cv->selected_lambda[0],10),"null labels can change the selected classifier penalty");
   classes[0]=0;classes[1]=1;
   for(i=0;i<18;i++)sign[i]=1;
   memset(&ps,0,sizeof(ps));ps.nobs=9;ps.nperm=2;ps.perm=index;ps.sign=sign;
   CHECK(THD_mvpa_run_classification_permutations(&plan,0,x,3,classes,NULL,
     THD_METRIC_BALANCED_ACCURACY,&ps,permuted,nullscore,w,err,sizeof(err)),err);
   CHECK(CLOSE(nullscore[0],1) && CLOSE(nullscore[1],2.0/3) &&
         CLOSE(w->ridge_cv->selected_lambda[0],1) && !memcmp(saved,w->decision+18,sizeof(saved)),
         "ridge label nulls retune models and restore observed decisions and penalties");
   index[9]=0;
   CHECK(!THD_mvpa_run_classification_permutations(&plan,0,x,3,classes,NULL,
     THD_METRIC_ACCURACY,&ps,permuted,nullscore,w,err,sizeof(err)),"classification nulls reject non-bijective row permutations");
   classes[0]=1;
   CHECK(!THD_mvpa_run_classification_cv(&plan,0,x,3,classes,NULL,w,err,sizeof(err)) && strstr(err,"inner fold"),
         "classifier rejects an inner training set missing a class");
   classes[0]=0;
   CHECK(THD_mvpa_run_classification_cv(&plan,0,constant,3,classes,NULL,w,err,sizeof(err)),err);
   CHECK(w->prediction[6]==0 && w->prediction[7]==0 && w->prediction[8]==0 &&
         CLOSE(w->ridge_cv->selected_lambda[0],10),"constant patterns use intercepts, lowest-class decision ties, and largest-lambda tuning ties");
   THD_mvpa_workspace_free(w);w=NULL;
   w=THD_mvpa_workspace_alloc(&plan,9,3,THD_PREP_NONE,NULL,err,sizeof(err));CHECK(w,err);
   CHECK(THD_mvpa_classification_set_ridge(w,1,NULL,NULL,0,err,sizeof(err)),err);
   for(i=0;i<9;i++)for(j=0;j<9;j++)wide[i*9+j]=x[i*3+j%3];
   CHECK(THD_mvpa_run_classification_cv(&plan,0,wide,9,classes,NULL,w,err,sizeof(err)),err);
   CHECK(CLOSE(w->decision[18],19.0/21) && CLOSE(w->decision[19],1.0/21),
         "wide rank-deficient multiclass patterns use the dual ridge solve");
   classes[1]=0;classes[2]=0;
   CHECK(THD_mvpa_run_classification_cv(&plan,0,constant,3,classes,NULL,w,err,sizeof(err)),err);
   CHECK(CLOSE(w->decision[18],2.0/3) && CLOSE(w->decision[19],1.0/6),
         "unweighted ridge intercepts reflect training class frequencies, not test frequencies");
done:
   THD_mvpa_workspace_free(w);THD_nested_foldplan_free(nested);return fail;
}

static int lda_test(void)
{
   int fail=0,i,j,mode,pred[3],labels[]={0,0,1,1,1,0},permclass[6];char err[512];
   float x[]={-2,-3,0,1,2,1,4,5,3,3,-1,-1},probe[]={3,3,1,1};
   float wide[24],wp[]={3,3,3,3,3,3},flat[]={7,7,7,7,7};
   float floorx[]={-2,0,0,0,2,10,4,10},floorprobe[]={3,10};
   int flatlabels[]={0,0,0,1,1},train[]={0,1,2,3},test[]={4,5},sf[]={-1,-1,-1,-1,0,0};
   int index[]={0,1,2,3,4,5,2,1,0,3,4,5};signed char sign[12];float nullscore[2];PERM_set ps;
   double decision[6],saved[12],nullacc,cov;THD_lda_model *m=NULL;THD_mvpa_workspace *w=NULL;THD_foldplan plan;
   m=THD_lda_alloc(THD_LDA_DIAGONAL,5,6,2,err,sizeof(err));CHECK(m,err);
   CHECK(THD_lda_fit(m,x,labels,4,2,0,1e-6,0,err,sizeof(err)),err);
   CHECK(THD_lda_predict(m,probe,2,2,pred,decision,err,sizeof(err)),err);
   CHECK(pred[0]==1 && pred[1]==0 && CLOSE(decision[1],1.25-log(2.0)) &&
         CLOSE(decision[0],-3.75-log(2.0)),"diagonal LDA matches analytic pooled-variance discriminants and tie rule");
   CHECK(THD_lda_fit(m,floorx,labels,4,2,0,.25,0,err,sizeof(err)),err);
   CHECK(THD_lda_predict(m,floorprobe,1,2,pred,decision,err,sizeof(err)),err);
   CHECK(CLOSE(decision[1],51-log(2.0)),"diagonal LDA applies its relative floor to zero within-class feature variance");
   CHECK(THD_lda_fit(m,flat,flatlabels,5,1,0,1e-6,0,err,sizeof(err)),err);
   CHECK(THD_lda_predict(m,flat,1,1,pred,decision,err,sizeof(err)) && pred[0]==0 &&
         CLOSE(decision[0],-log(2.0)) && CLOSE(decision[1],-log(2.0)),"constant data and equal LDA priors tie at the first class");
   CHECK(THD_lda_fit(m,flat,flatlabels,5,1,0,1e-6,1,err,sizeof(err)),err);
   CHECK(THD_lda_predict(m,flat,1,1,pred,decision,err,sizeof(err)) &&
         CLOSE(decision[0],log(.6)) && CLOSE(decision[1],log(.4)),"empirical LDA priors use training class counts");
   CHECK(!THD_lda_fit(m,x,labels,2,2,0,1e-6,0,err,sizeof(err)) &&
         !THD_lda_predict(m,probe,1,2,pred,decision,err,sizeof(err)),"failed LDA refits invalidate the model and reject zero residual degrees of freedom");
   THD_lda_free(m);m=THD_lda_alloc(THD_LDA_SHRINKAGE,4,6,2,err,sizeof(err));CHECK(m,err);
   CHECK(THD_lda_fit(m,x,labels,4,2,.5,1e-6,0,err,sizeof(err)),err);
   CHECK(THD_lda_predict(m,probe,2,2,pred,decision,err,sizeof(err)),err);
   CHECK(CLOSE(decision[1],.64-log(2.0)) && CLOSE(decision[0],-1.92-log(2.0)),
         "shrinkage LDA matches the inverse of a known correlated 2x2 covariance");
   {
      float scaled[8],sp[2];int scale_index;
      for(scale_index=0;scale_index<2;scale_index++){
         float scale=scale_index?1e-20f:1e20f;
         for(i=0;i<8;i++)scaled[i]=x[i]*scale;
         sp[0]=probe[0]*scale;sp[1]=probe[1]*scale;
         CHECK(THD_lda_fit(m,scaled,labels,4,2,.5,1e-6,0,err,sizeof(err)),err);
         CHECK(THD_lda_predict(m,sp,1,2,pred,decision,err,sizeof(err)) && CLOSE(decision[1],.64-log(2.0)),
               "covariance normalization preserves LDA decisions for very large and small input units");
      }
   }
   for(i=0;i<4;i++)for(j=0;j<6;j++)wide[i*6+j]=x[i*2+j%2];
   CHECK(THD_lda_fit(m,wide,labels,4,6,.5,1e-6,0,err,sizeof(err)),err);
   CHECK(THD_lda_predict(m,wp,1,6,pred,decision,err,sizeof(err)),err);
   CHECK(CLOSE(decision[1],192.0/175-log(2.0)),"sample-space Woodbury LDA matches a known wide rank-one covariance inverse");
   CHECK(THD_lda_fit(m,x,labels,4,2,1,1e-6,0,err,sizeof(err)),err);
   CHECK(THD_lda_predict(m,probe,1,2,pred,decision,err,sizeof(err)) && CLOSE(decision[1],.8-log(2.0)),
         "full shrinkage gives the spherical covariance target");
   CHECK(!THD_lda_fit(m,x,labels,4,2,0,1e-6,0,err,sizeof(err)),"zero shrinkage is rejected");
   CHECK(!THD_lda_fit(m,x,labels,4,2,.1,0,0,err,sizeof(err)),"zero variance floor is rejected");
   {
      int missing[]={0,0,0,0};float bad[]={NAN,0};
      CHECK(!THD_lda_fit(m,x,missing,4,2,.5,1e-6,0,err,sizeof(err)),"standalone LDA rejects a missing training class");
      CHECK(THD_lda_fit(m,x,labels,4,2,.5,1e-6,0,err,sizeof(err)),err);
      CHECK(!THD_lda_predict(m,bad,1,2,pred,decision,err,sizeof(err)),"LDA rejects nonfinite prediction features");
   }
   fixed_plan(&plan,6,train,4,test,2,sf);
   for(i=0;i<12;i++)sign[i]=1;
   memset(&ps,0,sizeof(ps));ps.nobs=6;ps.nperm=2;ps.perm=index;ps.sign=sign;
   for(mode=0;mode<2;mode++){
      w=THD_mvpa_workspace_alloc(&plan,2,2,THD_PREP_NONE,NULL,err,sizeof(err));CHECK(w,err);
      CHECK(THD_mvpa_classification_set_lda(w,mode,.5,1e-6,0,err,sizeof(err)),err);
      CHECK(!THD_mvpa_classification_set_ridge(w,1,NULL,NULL,0,err,sizeof(err)),"LDA and ridge configurations are exclusive");
      CHECK(THD_mvpa_run_classification_cv(&plan,0,x,2,labels,NULL,w,err,sizeof(err)),err);
      CHECK(CLOSE(w->score.accuracy,1) && w->prediction[0]==-1 && isnan(w->decision[0]),"LDA fold runner preserves training-only rows");
      memcpy(saved,w->decision,sizeof(saved));cov=w->fold_covariance_scale[0];
      CHECK(CLOSE(cov,5) && CLOSE(w->fold_variance_floor[0],5e-6),"LDA fold diagnostics record training covariance scale and absolute floor");
      labels[4]=0;labels[5]=1;
      CHECK(THD_mvpa_run_classification_cv(&plan,0,x,2,labels,NULL,w,err,sizeof(err)),err);
      CHECK(!memcmp(saved,w->decision,sizeof(saved)) && CLOSE(cov,w->fold_covariance_scale[0]),"outer-test labels do not change LDA decisions or covariance");
      labels[4]=1;labels[5]=0;
      for(i=0;i<6;i++)permclass[i]=labels[index[6+i]];
      CHECK(THD_mvpa_run_classification_cv(&plan,0,x,2,permclass,NULL,w,err,sizeof(err)),err);
      nullacc=w->score.balanced_accuracy;
      CHECK(THD_mvpa_run_classification_permutations(&plan,0,x,2,labels,NULL,THD_METRIC_BALANCED_ACCURACY,
            &ps,permclass,nullscore,w,err,sizeof(err)),err);
      CHECK(CLOSE(nullscore[0],1) && CLOSE(nullscore[1],nullacc) && !memcmp(saved,w->decision,sizeof(saved)) &&
            CLOSE(w->fold_covariance_scale[0],cov),"LDA nulls refit covariance and restore observed decisions and diagnostics");
      THD_mvpa_workspace_free(w);w=NULL;
   }
   w=THD_mvpa_workspace_alloc(&plan,2,2,THD_PREP_FEATURE_ZSCORE,NULL,err,sizeof(err));CHECK(w,err);
   CHECK(THD_mvpa_classification_set_lda(w,THD_LDA_DIAGONAL,0,1e-6,0,err,sizeof(err)),err);
   CHECK(THD_mvpa_run_classification_cv(&plan,0,x,2,labels,NULL,w,err,sizeof(err)),err);
   CHECK(CLOSE(w->decision[9],1.25-log(2.0)) && CLOSE(w->prep->center[0],1) && CLOSE(w->prep->center[1],1),
         "diagonal LDA is invariant to nonsingular feature scaling and preprocessing fits only training rows");
   cov=w->fold_covariance_scale[0];x[8]=1e6;x[9]=-1e6;
   CHECK(THD_mvpa_run_classification_cv(&plan,0,x,2,labels,NULL,w,err,sizeof(err)),err);
   CHECK(CLOSE(w->fold_covariance_scale[0],cov) && CLOSE(w->prep->center[0],1),
         "held-out feature outliers cannot change training scales or LDA covariance");
   THD_mvpa_workspace_free(w);w=NULL;
   for(mode=0;mode<2;mode++){
      float multiclass[]={-2.5,-1.5,-.5,.5,1.5,2.5},mp[]={-2,0,2};
      float noiseless[]={-2,-2,0,0,2,2};int mc[]={0,0,1,1,2,2};double md[9];
      THD_lda_free(m);m=THD_lda_alloc(mode,6,1,3,err,sizeof(err));CHECK(m,err);
      CHECK(THD_lda_fit(m,multiclass,mc,6,1,.5,1e-6,0,err,sizeof(err)),err);
      CHECK(THD_lda_predict(m,mp,3,1,pred,md,err,sizeof(err)),err);
      CHECK(pred[0]==0 && pred[1]==1 && pred[2]==2 && CLOSE(md[0],4-log(3.0)) && CLOSE(md[4],-log(3.0)),
            "both LDA variants support analytic three-class discriminants");
      CHECK(THD_lda_fit(m,noiseless,mc,6,1,.5,1e-6,0,err,sizeof(err)),err);
      CHECK(THD_lda_predict(m,mp,3,1,pred,md,err,sizeof(err)) && pred[0]==0 && pred[1]==1 && pred[2]==2,
            "zero within-class variance falls back to training variance without losing class separation");
   }
done:
   THD_lda_free(m);THD_mvpa_workspace_free(w);return fail;
}

int main( void )
{
   float zmat[]={ 1,10,5, 3,14,5, 102,212,500, -98,-188,-500 } ;
   float smat[]={ 9,9,9, 2,4,6, 1,2,3, 8,6,4 } ;
   float rmat[]={ 1,3, 3,7, 10,20, 14,24, 4,9, 11,25 } ;
   float badmat[]={ 1,2,3, 4,NAN,6, 7,8,9, 10,11,12 } ;
   float overflow_mat[]={ 1.0f,1.0000001192092896f,FLT_MAX,0.0f } ;
   float train_out[12],test_out[12],apply_out[12] ;
   int tr2[]={0,1},te2[]={2,3},sf2[]={-1,-1,0,0} ;
   int tr4[]={0,1,2,3},te4[]={4,5},sf4[]={-1,-1,-1,-1,0,0} ;
   int group[]={10,10,20,20,10,20},bad_group[]={10,10,20,20,10,30} ;
   int class_truth[]={0,0,0,0,1,1},class_pred[]={0,0,1,0,1,0} ;
   int confusion[9],metric=-1,rank_index[8] ; double recall[3],rank_scratch[8] ;
   float cont_truth[]={1,2,3,4},cont_pred[]={1,3,2,5},cont_base[]={2.5,2.5,2.5,2.5} ;
   float tie_truth[]={1,1,2,3},tie_pred[]={1,2,2,3} ;
   float flat[]={2,2,2,2},zero_base[]={2,2,2,2} ;
   float euc_train[]={0,0, 0,2, 4,0, 4,2},euc_test[]={1,1, 3,2, 2,1} ;
   float corr_train[]={1,2,3, 2,4,6, 3,2,1, 6,4,2} ;
   float corr_test[]={10,20,30, 9,6,3},constant_test[]={5,5,5} ;
   float constant_train[]={1,1,1, 1,1,1, 1,2,3, 2,4,6} ;
   float cos_train[]={1,0, 2,0, 0,1, 0,2},cos_test[]={0.8f,0.2f, 0.1f,0.9f} ;
   float zero_pattern[]={0,0} ;
   float cv_x[]={0,0, 10,0, 1,1, 9,1, -1,2, 11,2} ;
   int cv_class[]={0,1,0,1,0,1},cv_group[]={0,0,1,1,2,2} ;
   int train_class[]={0,0,1,1},predicted[3],distance=-1 ; double decision[6] ;
   THD_mvpa_class_score cscore ; THD_mvpa_continuous_score dscore ;
   THD_centroid_model *cm=NULL ;
   THD_mvpa_workspace *ws=NULL ;
   THD_foldplan plan,cvplan ; THD_preprocessor *p=NULL ; char err[512] ; int fail=0,method=-1 ;

   fixed_plan(&plan,4,tr2,2,te2,2,sf2) ;
   CHECK(THD_preprocess_fold(&plan,0,THD_PREP_FEATURE_ZSCORE,zmat,3,NULL,
                             train_out,test_out,&p,err,sizeof(err)),err) ;
   CHECK(CLOSE(p->center[0],2) && CLOSE(p->center[1],12) && CLOSE(p->center[2],5),
         "feature_zscore learns training means only") ;
   CHECK(CLOSE(p->scale[0],1) && CLOSE(p->scale[1],2) && p->nconstant==1,
         "feature_zscore learns training scales and constant features") ;
   CHECK(CLOSE(train_out[0],-1) && CLOSE(train_out[1],-1) && CLOSE(train_out[2],0) &&
         CLOSE(train_out[3],1) && CLOSE(train_out[4],1) && CLOSE(train_out[5],0),
         "training rows use their fitted z-score") ;
   CHECK(CLOSE(test_out[0],100) && CLOSE(test_out[1],100) && CLOSE(test_out[2],0) &&
         CLOSE(test_out[3],-100) && CLOSE(test_out[4],-100) && CLOSE(test_out[5],0),
         "held-out values do not influence z-score parameters") ;
   THD_preprocess_free(p) ; p=NULL ;
   CHECK(THD_preprocess_fit(THD_PREP_FEATURE_ZSCORE,badmat,4,3,tr2,2,NULL,err,sizeof(err))==NULL &&
         strstr(err,"not finite")!=NULL,"non-finite training data are rejected") ;
   p=THD_preprocess_fit(THD_PREP_FEATURE_ZSCORE,overflow_mat,4,1,tr2,2,NULL,err,sizeof(err)) ; CHECK(p!=NULL,err) ;
   CHECK(!THD_preprocess_apply(p,overflow_mat,4,1,te2,2,NULL,apply_out,err,sizeof(err)) &&
         strstr(err,"outside the float range")!=NULL,"transformation overflow is rejected") ;
   THD_preprocess_free(p) ; p=NULL ;

   p=THD_preprocess_fit(THD_PREP_SAMPLE_CENTER,smat,4,3,tr2,2,NULL,err,sizeof(err)) ; CHECK(p!=NULL,err) ;
   CHECK(THD_preprocess_apply(p,smat,4,3,te2,2,NULL,apply_out,err,sizeof(err)),err) ;
   CHECK(CLOSE(apply_out[0],-1) && CLOSE(apply_out[1],0) && CLOSE(apply_out[2],1) &&
         CLOSE(apply_out[3],2) && CLOSE(apply_out[4],0) && CLOSE(apply_out[5],-2),
         "sample_center uses each sample independently") ;
   THD_preprocess_free(p) ; p=NULL ;

   p=THD_preprocess_fit(THD_PREP_NONE,smat,4,3,tr2,2,NULL,err,sizeof(err)) ; CHECK(p!=NULL,err) ;
   CHECK(THD_preprocess_apply(p,smat,4,3,te2,2,NULL,apply_out,err,sizeof(err)),err) ;
   CHECK(!memcmp(apply_out,smat+6,6*sizeof(float)),"none copies selected rows exactly") ;
   THD_preprocess_free(p) ; p=NULL ;

   fixed_plan(&plan,6,tr4,4,te4,2,sf4) ;
   CHECK(THD_preprocess_fold(&plan,0,THD_PREP_RUN_CENTER,rmat,2,group,
                             train_out,test_out,&p,err,sizeof(err)),err) ;
   CHECK(p->ngroup==2 && CLOSE(p->center[0],2) && CLOSE(p->center[1],5) &&
         CLOSE(p->center[2],12) && CLOSE(p->center[3],22),
         "run_center learns one training mean per run") ;
   CHECK(CLOSE(test_out[0],2) && CLOSE(test_out[1],4) &&
         CLOSE(test_out[2],-1) && CLOSE(test_out[3],3),
         "run_center applies training run means to held-out rows") ;
   CHECK(!THD_preprocess_apply(p,rmat,6,2,te4,2,bad_group,apply_out,err,sizeof(err)) &&
         strstr(err,"not present in training")!=NULL,
         "run_center rejects a held-out-only run") ;
   THD_preprocess_free(p) ; p=NULL ;

   CHECK(THD_preprocess_parse("FEATURE_ZSCORE",&method,err,sizeof(err)) && method==THD_PREP_FEATURE_ZSCORE,
         "preprocessing option parsing is case-insensitive") ;
   CHECK(!THD_preprocess_parse("mystery",&method,err,sizeof(err)) && strstr(err,"mystery")!=NULL,
         "unknown preprocessing option is rejected") ;

   CHECK(THD_mvpa_score_classification(class_truth,class_pred,6,2,confusion,recall,
                                        &cscore,err,sizeof(err)),err) ;
   CHECK(confusion[0]==3 && confusion[1]==1 && confusion[2]==1 && confusion[3]==1,
         "classification confusion matrix uses truth rows and prediction columns") ;
   CHECK(cscore.ncorrect==4 && cscore.nclass_present==2 && CLOSE(cscore.accuracy,2.0/3.0) &&
         CLOSE(recall[0],0.75) && CLOSE(recall[1],0.5) && CLOSE(cscore.balanced_accuracy,0.625),
         "classification accuracy and class-balanced recall are correct") ;
   CHECK(THD_mvpa_score_classification(class_truth,class_pred,6,3,confusion,recall,
                                        &cscore,err,sizeof(err)),err) ;
   CHECK(cscore.nclass_present==2 && isnan(recall[2]) && CLOSE(cscore.balanced_accuracy,0.625),
         "classes absent from truth are excluded from balanced accuracy") ;
   class_pred[5]=3 ;
   CHECK(!THD_mvpa_score_classification(class_truth,class_pred,6,3,confusion,recall,
                                         &cscore,err,sizeof(err)) && strstr(err,"outside")!=NULL,
         "invalid predicted classes are rejected") ;
   class_pred[5]=0 ;

   CHECK(THD_mvpa_score_continuous(cont_truth,cont_pred,cont_base,4,rank_scratch,rank_index,
                                    &dscore,err,sizeof(err)),err) ;
   CHECK(CLOSE(dscore.pearson,5.5/sqrt(43.75)) && CLOSE(dscore.spearman,0.8),
         "Pearson and Spearman correlations match known answers") ;
   CHECK(CLOSE(dscore.cv_r2,0.4) && CLOSE(dscore.mae,0.75) && CLOSE(dscore.rmse,sqrt(0.75)),
         "cross-validated R2 and absolute/squared errors match known answers") ;
   CHECK(THD_mvpa_score_continuous(tie_truth,tie_pred,cont_base,4,rank_scratch,rank_index,
                                    &dscore,err,sizeof(err)) && CLOSE(dscore.spearman,5.0/6.0),
         "Spearman scoring assigns average ranks to ties") ;
   CHECK(THD_mvpa_score_continuous(flat,flat,zero_base,4,rank_scratch,rank_index,
                                    &dscore,err,sizeof(err)),err) ;
   CHECK(isnan(dscore.pearson) && isnan(dscore.spearman) && isnan(dscore.cv_r2) &&
         CLOSE(dscore.mae,0) && CLOSE(dscore.rmse,0),
         "undefined continuous scores are NAN while error metrics remain defined") ;
   cont_pred[2]=NAN ;
   CHECK(!THD_mvpa_score_continuous(cont_truth,cont_pred,cont_base,4,rank_scratch,rank_index,
                                     &dscore,err,sizeof(err)) && strstr(err,"not finite")!=NULL,
         "non-finite continuous predictions are rejected") ;
   cont_pred[2]=2 ;

   CHECK(THD_mvpa_metric_parse("BALANCED_ACCURACY",&metric,err,sizeof(err)) &&
         metric==THD_METRIC_BALANCED_ACCURACY &&
         strcmp(THD_mvpa_metric_name(metric),"balanced_accuracy")==0,
         "metric option parsing is case-insensitive") ;
   CHECK(!THD_mvpa_metric_parse("mystery",&metric,err,sizeof(err)) && strstr(err,"mystery")!=NULL,
         "unknown metric option is rejected") ;

   cm=THD_centroid_fit(THD_CENTROID_EUCLIDEAN,euc_train,train_class,4,2,2,err,sizeof(err)) ;
   CHECK(cm!=NULL,err) ;
   CHECK(cm->class_count[0]==2 && cm->class_count[1]==2 &&
         CLOSE(cm->centroid[0],0) && CLOSE(cm->centroid[1],1) &&
         CLOSE(cm->centroid[2],4) && CLOSE(cm->centroid[3],1),
         "nearest-centroid fitting computes class means") ;
   CHECK(THD_centroid_predict(cm,euc_test,3,2,predicted,decision,err,sizeof(err)),err) ;
   CHECK(predicted[0]==0 && predicted[1]==1 && predicted[2]==0 &&
         CLOSE(decision[0],-1) && CLOSE(decision[1],-9) &&
         CLOSE(decision[2],-10) && CLOSE(decision[3],-2) &&
         CLOSE(decision[4],-4) && CLOSE(decision[5],-4),
         "Euclidean centroid scores and deterministic ties match known answers") ;
   THD_centroid_free(cm) ; cm=NULL ;

   cm=THD_centroid_fit(THD_CENTROID_CORRELATION,corr_train,train_class,4,3,2,err,sizeof(err)) ;
   CHECK(cm!=NULL,err) ;
   CHECK(THD_centroid_predict(cm,corr_test,2,3,predicted,decision,err,sizeof(err)),err) ;
   CHECK(predicted[0]==0 && predicted[1]==1 && CLOSE(decision[0],1) &&
         CLOSE(decision[1],-1) && CLOSE(decision[2],-1) && CLOSE(decision[3],1),
         "correlation-to-centroid predictions match known answers") ;
   CHECK(!THD_centroid_predict(cm,constant_test,1,3,predicted,decision,err,sizeof(err)) &&
         strstr(err,"constant")!=NULL,"correlation rejects a constant prediction pattern") ;
   THD_centroid_free(cm) ; cm=NULL ;
   CHECK(THD_centroid_fit(THD_CENTROID_CORRELATION,constant_train,train_class,4,3,2,
                          err,sizeof(err))==NULL && strstr(err,"constant")!=NULL,
         "correlation rejects a constant training centroid") ;

   cm=THD_centroid_fit(THD_CENTROID_COSINE,cos_train,train_class,4,2,2,err,sizeof(err)) ;
   CHECK(cm!=NULL,err) ;
   CHECK(THD_centroid_predict(cm,cos_test,2,2,predicted,NULL,err,sizeof(err)) &&
         predicted[0]==0 && predicted[1]==1,
         "cosine centroid prediction supports label-only output") ;
   CHECK(!THD_centroid_predict(cm,zero_pattern,1,2,predicted,decision,err,sizeof(err)) &&
         strstr(err,"zero norm")!=NULL,"cosine rejects a zero-norm prediction pattern") ;
   THD_centroid_free(cm) ; cm=NULL ;

   CHECK(THD_centroid_fit(THD_CENTROID_EUCLIDEAN,euc_train,train_class,4,2,3,
                          err,sizeof(err))==NULL && strstr(err,"class 2")!=NULL,
         "centroid fitting rejects a missing training class") ;
   CHECK(THD_centroid_distance_parse("CORRELATION",&distance,err,sizeof(err)) &&
         distance==THD_CENTROID_CORRELATION,
         "centroid distance parsing is case-insensitive") ;
   CHECK(!THD_centroid_distance_parse("mystery",&distance,err,sizeof(err)) &&
         strstr(err,"mystery")!=NULL,"unknown centroid distance is rejected") ;

   paired_three_fold_plan(&cvplan) ;
   ws=THD_mvpa_workspace_alloc(&cvplan,2,2,THD_PREP_FEATURE_ZSCORE,NULL,err,sizeof(err)) ;
   CHECK(ws!=NULL,err) ;
   {
     float *train_buffer=ws->train_x ; double *decision_buffer=ws->decision ;
     CHECK(THD_mvpa_run_centroid_cv(&cvplan,THD_CENTROID_EUCLIDEAN,cv_x,2,
                                    cv_class,NULL,ws,err,sizeof(err)),err) ;
     CHECK(ws->ntest==6 && ws->score.nsample==6 && ws->score.ncorrect==6 &&
           CLOSE(ws->score.accuracy,1) && CLOSE(ws->score.balanced_accuracy,1),
           "fold runner computes perfect out-of-fold scores") ;
     CHECK(!memcmp(ws->prediction,cv_class,6*sizeof(int)) &&
           ws->confusion[0]==3 && ws->confusion[1]==0 &&
           ws->confusion[2]==0 && ws->confusion[3]==3 &&
           CLOSE(ws->recall[0],1) && CLOSE(ws->recall[1],1),
           "fold runner restores predictions and diagnostics to original row order") ;
     for( method=0 ; method<3 ; method++ )
       CHECK(ws->fold_nsample[method]==2 && CLOSE(ws->fold_accuracy[method],1) &&
             CLOSE(ws->fold_balanced_accuracy[method],1),
             "fold runner records per-fold sample counts and scores") ;
     for( method=0 ; method<12 ; method++ )
       CHECK(isfinite(ws->decision[method]),"tested rows receive finite per-class decisions") ;
     CHECK(THD_mvpa_run_centroid_cv(&cvplan,THD_CENTROID_EUCLIDEAN,cv_x,2,
                                    cv_class,NULL,ws,err,sizeof(err)),err) ;
     CHECK(ws->train_x==train_buffer && ws->decision==decision_buffer,
           "repeated CV runs reuse workspace buffers") ;
     {
       int perm_index[]={0,1,2,3,4,5, 1,0,2,3,4,5} ;
       signed char perm_sign[]={1,1,1,1,1,1, 1,1,1,1,1,1} ;
       int perm_class[6] ; float null_score[2] ; PERM_set pset ;
       memset(&pset,0,sizeof(pset)) ; pset.nperm=2 ; pset.nobs=6 ;
       pset.perm=perm_index ; pset.sign=perm_sign ;
       CHECK(THD_mvpa_run_centroid_permutations(
                 &cvplan,THD_CENTROID_EUCLIDEAN,cv_x,2,cv_class,NULL,
                 THD_METRIC_BALANCED_ACCURACY,&pset,perm_class,null_score,
                 ws,err,sizeof(err)),err) ;
       CHECK(CLOSE(null_score[0],1) && isfinite(null_score[1]),
             "permutation runner returns the identity and relabeled CV scores") ;
       CHECK(CLOSE(ws->score.balanced_accuracy,1) &&
             !memcmp(ws->prediction,cv_class,6*sizeof(int)),
             "permutation runner restores observed predictions and scores") ;
       CHECK(!THD_mvpa_run_centroid_permutations(
                 &cvplan,THD_CENTROID_EUCLIDEAN,cv_x,2,cv_class,NULL,
                 THD_METRIC_PEARSON,&pset,perm_class,null_score,
                 ws,err,sizeof(err)) && strstr(err,"accuracy")!=NULL,
             "classification permutation runner rejects continuous metrics") ;
     }
   }
   THD_mvpa_workspace_free(ws) ; ws=NULL ;

   fixed_plan(&plan,6,tr4,4,te4,2,sf4) ;
   ws=THD_mvpa_workspace_alloc(&plan,2,2,THD_PREP_NONE,NULL,err,sizeof(err)) ;
   CHECK(ws!=NULL,err) ;
   CHECK(THD_mvpa_run_centroid_cv(&plan,THD_CENTROID_EUCLIDEAN,cv_x,2,
                                  cv_class,NULL,ws,err,sizeof(err)),err) ;
   CHECK(ws->ntest==2 && ws->score.nsample==2 && ws->prediction[4]==0 &&
         ws->prediction[5]==1 && ws->prediction[0]==-1 && isnan(ws->decision[0]),
         "fixed CV marks training-only rows and scores only held-out rows") ;
   THD_mvpa_workspace_free(ws) ; ws=NULL ;

   ws=THD_mvpa_workspace_alloc(&cvplan,2,2,THD_PREP_RUN_CENTER,cv_group,err,sizeof(err)) ;
   CHECK(ws!=NULL,err) ;
   CHECK(!THD_mvpa_run_centroid_cv(&cvplan,THD_CENTROID_EUCLIDEAN,cv_x,2,
                                   cv_class,cv_group,ws,err,sizeof(err)) &&
         strstr(err,"fold pair1")!=NULL && strstr(err,"not present in training")!=NULL,
         "fold runner reports run_center leakage restrictions with fold context") ;
   THD_mvpa_workspace_free(ws) ; ws=NULL ;

done:
   THD_mvpa_workspace_free(ws) ;
   THD_centroid_free(cm) ;
   THD_preprocess_free(p) ;
   if( fail || nuisance_test() || svm_test() || ridge_test() || nested_ridge_test() || ridge_permutation_test() || ridge_classification_test() || lda_test() ) return 1 ;
   puts("PASS thd_mvpa preprocessing, metrics, classifiers, CV, and permutation runner") ; return 0 ;
}
