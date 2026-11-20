/* Repeated selection calls must clear invalid state and expose valid matrices. */
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "../../thd_insync.h"
#define CHECK(x) do { if(!(x)){ fprintf(stderr,"FAIL line %d: %s\n",__LINE__,#x); return 1; } } while(0)
int main(void)
{
   INSYNC_kernel *w=INSYNC_kernel_new(3,4);
   int members[]={0,1,2},*member[]={members},size[]={3},n[1],complete[1]; float stat[1];
   float data[]={1,-1,1,-1, 1,1,-1,-1, 1,0,-1,0};
   float constant[]={7,7,7,7, 1,1,-1,-1, 1,0,-1,0};
   const float *matrix;
   CHECK(w);
   INSYNC_kernel_eval(w,4,data,INSYNC_CORR_PEARSON,INSYNC_METHOD_PAIRWISE,INSYNC_SUMMARY_MEDIAN,1,size,member,stat,n,complete);
   CHECK(stat[0]==0 && n[0]==3 && complete[0]==1);
   matrix=INSYNC_kernel_matrix(w); CHECK(matrix[1]==0 && matrix[3]==0 && matrix[0]==1);
   INSYNC_kernel_eval(w,4,constant,INSYNC_CORR_SPEARMAN,INSYNC_METHOD_PAIRWISE,INSYNC_SUMMARY_MEDIAN,1,size,member,stat,n,complete);
   CHECK(n[0]==1 && complete[0]==0 && isfinite(stat[0])); CHECK(isnan(matrix[1]) && isnan(matrix[3]));
   INSYNC_kernel_eval(w,2,data,INSYNC_CORR_PEARSON,INSYNC_METHOD_PAIRWISE,INSYNC_SUMMARY_MEDIAN,1,size,member,stat,n,complete);
   CHECK(isnan(stat[0]) && n[0]==0 && complete[0]==0 && isnan(matrix[5]));
   INSYNC_kernel_eval(w,4,data,INSYNC_CORR_SPEARMAN,INSYNC_METHOD_LOO,INSYNC_SUMMARY_FISHER_MEAN,1,size,member,stat,n,complete);
   CHECK(isfinite(stat[0]) && n[0]==3 && complete[0]==1);
   {
      float blocks[18],z[9],r[3]={.9f,.1f,.2f},q[3]={-.2f,.8f,.4f};
      unsigned char keep[2]={1,1}; int edge[3]={1,2,5},i;
      float expected[3];
      for(i=0;i<18;i++) blocks[i]=NAN;
      for(i=0;i<3;i++){
         int row=edge[i]/3,col=edge[i]%3;
         blocks[edge[i]]=blocks[col*3+row]=r[i];
         blocks[9+edge[i]]=blocks[9+col*3+row]=q[i];
         expected[i]=(float)tanh(.5*(atanh((double)r[i])+atanh((double)q[i])));
      }
      INSYNC_kernel_aggregate(w,2,blocks,keep,INSYNC_METHOD_PAIRWISE,INSYNC_SUMMARY_FISHER_MEAN,INSYNC_SUMMARY_MEDIAN,1,size,member,stat,n,complete);
      CHECK(n[0]==3 && complete[0]==1);
      INSYNC_kernel_fisher_matrix(w,z);
      for(i=0;i<3;i++){
         CHECK(fabs(matrix[edge[i]]-expected[i])<1.e-6);
         CHECK(fabs(z[edge[i]]-atanh((double)expected[i]))<1.e-6);
      }
      CHECK(isnan(z[0]));
      /* A single admitted block is identity, including exact endpoints. */
      blocks[1]=blocks[3]=1; keep[1]=0;
      INSYNC_kernel_aggregate(w,2,blocks,keep,INSYNC_METHOD_PAIRWISE,INSYNC_SUMMARY_FISHER_MEAN,INSYNC_SUMMARY_MEDIAN,1,size,member,stat,n,complete);
      CHECK(matrix[1]==1); INSYNC_kernel_fisher_matrix(w,z);
      CHECK(fabs(z[1]-atanhf(.999999f))<1.e-6);
      {
         float loo[]={.9f,.1f,.2f,-.2f,.8f,.4f}; keep[1]=1;
         INSYNC_kernel_aggregate(w,2,loo,keep,INSYNC_METHOD_LOO,INSYNC_SUMMARY_MEDIAN,INSYNC_SUMMARY_MEDIAN,1,size,member,stat,n,complete);
         CHECK(n[0]==3 && complete[0]==1 && fabs(stat[0]-.35)<1.e-6);
         CHECK(fabs(INSYNC_kernel_loo(w)[1]-.45)<1.e-6);
      }
      blocks[1]=NAN; keep[0]=keep[1]=1;
      INSYNC_kernel_aggregate(w,2,blocks,keep,INSYNC_METHOD_PAIRWISE,INSYNC_SUMMARY_MEDIAN,INSYNC_SUMMARY_MEDIAN,1,size,member,stat,n,complete);
      CHECK(isnan(stat[0]) && n[0]==0 && complete[0]==0 && isnan(matrix[5]));
      keep[0]=keep[1]=0;
      INSYNC_kernel_aggregate(w,2,blocks,keep,INSYNC_METHOD_PAIRWISE,INSYNC_SUMMARY_MEDIAN,INSYNC_SUMMARY_MEDIAN,1,size,member,stat,n,complete);
      CHECK(isnan(stat[0]) && n[0]==0 && complete[0]==0 && isnan(matrix[1]));
   }
   INSYNC_kernel_free(w); INSYNC_kernel_free(NULL);
   CHECK(INSYNC_kernel_new(2,4)==NULL && INSYNC_kernel_new(3,2)==NULL);
   {
      INSYNC_missing *m=INSYNC_missing_new(3,5);
      INSYNC_options o={.method=INSYNC_METHOD_PAIRWISE,.corr_metric=INSYNC_CORR_PEARSON,
         .summary=INSYNC_SUMMARY_MEDIAN,.block_summary=INSYNC_SUMMARY_MEDIAN,
         .min_valid=3,.min_blocks=1,.loo_min_others=1};
      float x[]={1,2,3,4,5, 1,NAN,3,4,5, 5,4,NAN,2,1};
      float constant_other[]={1,2,3,4,5, 2,2,2,2,2, 5,4,NAN,2,1};
      INSYNC_component blocks[24]; const INSYNC_component *v; int i;
      CHECK(m && !INSYNC_missing_new(2,5) && !INSYNC_missing_new(3,2));
      INSYNC_missing_eval(m,5,x,NULL,5,&o,1,size,member); v=INSYNC_missing_components(m);
      CHECK(v[1].overlap==4 && v[2].overlap==4 && v[5].overlap==3);
      CHECK(v[1].r==1 && v[2].r==-1 && v[5].r==-1);
      CHECK(INSYNC_missing_subjects(m)[0]==5 && INSYNC_missing_subjects(m)[1]==4);
      memcpy(blocks,v,12*sizeof(*v));
      INSYNC_missing_eval(m,5,constant_other,NULL,5,&o,1,size,member);
      memcpy(blocks+12,v,12*sizeof(*v));
      INSYNC_missing_aggregate(m,2,blocks,&o,1,size,member);
      CHECK(isnan(v[1].r) && v[1].eligible==1 && v[1].blocks==0 && v[1].used==0);
      CHECK(v[2].r==-1 && v[2].used==8 && v[2].blocks==2);
      INSYNC_missing_summary(m,o.method,o.summary,1,size,member,stat,n,complete);
      CHECK(n[0]==1 && complete[0]==0 && stat[0]==-1);
      o.block_drop_common=2; o.block_summary=INSYNC_SUMMARY_FISHER_MEAN;
      INSYNC_missing_aggregate(m,2,blocks,&o,1,size,member);
      CHECK(v[1].r==1 && v[1].used==4 && v[1].blocks==1 && v[2].blocks==2);
      o.block_drop_common=1; o.block_summary=INSYNC_SUMMARY_MEDIAN;
      INSYNC_missing_aggregate(m,2,blocks,&o,1,size,member);
      CHECK(v[1].blocks==1 && v[2].blocks==1 && v[2].used==4);
      o.min_valid_frac=.81;
      INSYNC_missing_eval(m,5,x,NULL,5,&o,1,size,member);
      CHECK(isnan(v[1].r) && v[1].overlap==4 && v[1].used==0);
      o.min_valid_frac=0; o.method=INSYNC_METHOD_LOO;
      INSYNC_missing_eval(m,5,x,NULL,5,&o,1,size,member);
      CHECK(v[9].overlap==5 && v[9].min_others==1 && fabs(v[9].r+1/sqrt(8.))<1.e-6);
      CHECK(v[10].overlap==4 && isnan(v[10].r));
      o.loo_min_others=2;
      INSYNC_missing_eval(m,5,x,NULL,5,&o,1,size,member);
      CHECK(v[9].overlap==3 && v[9].min_others==2 && isnan(v[9].r));
      INSYNC_missing_eval(m,0,x,NULL,0,&o,1,size,member);
      CHECK(v[1].overlap==0 && isnan(v[1].r) && INSYNC_missing_subjects(m)[0]==0);
      /* Repeated held-out copies must never enter their own LOO mean. */
      {
         int repeated[]={0,0,2},*repeat_members[]={repeated};
         o.loo_min_others=1;
         INSYNC_missing_eval(m,5,x,NULL,5,&o,1,size,repeat_members);
         CHECK(v[9].r==-1 && v[9].min_others==1 && v[11].r==-1);
         o.loo_min_others=2;
         INSYNC_missing_eval(m,5,x,NULL,5,&o,1,size,repeat_members);
         CHECK(isnan(v[9].r) && isnan(v[11].r));
      }
      /* Exact-half weighted median averages neighbors; unequal weights select
         the heavier block. Fisher weighting has an independent closed form. */
      o.method=INSYNC_METHOD_PAIRWISE; o.block_drop_common=2; o.block_weight=1;
      o.block_summary=INSYNC_SUMMARY_MEDIAN;
      for(i=0;i<24;i++) blocks[i]=(INSYNC_component){.r=NAN};
      blocks[1]=blocks[3]=(INSYNC_component){.r=.2f,.used=3,.overlap=3};
      blocks[13]=blocks[15]=(INSYNC_component){.r=.8f,.used=3,.overlap=3};
      INSYNC_missing_aggregate(m,2,blocks,&o,1,size,member);
      CHECK(fabs(v[1].r-.5)<1.e-6);
      blocks[13].used=blocks[15].used=4;
      INSYNC_missing_aggregate(m,2,blocks,&o,1,size,member);
      CHECK(fabs(v[1].r-.8)<1.e-6 && v[1].used==7);
      o.block_summary=INSYNC_SUMMARY_FISHER_MEAN;
      INSYNC_missing_aggregate(m,2,blocks,&o,1,size,member);
      CHECK(fabs(v[1].r-tanh((3*atanh(.2)+4*atanh(.8))/7))<1.e-6);
      INSYNC_missing_free(m); INSYNC_missing_free(NULL);
   }
   {
      float row[]={NAN,1,99,7,NAN,NAN,13,NAN, 9,NAN,5,NAN};
      unsigned char keep[]={1,1,0,1,1,1,1,1, 1,1,1,1},fill[12];
      int runs[]={0,8},bad[]={0,0};
      CHECK(INSYNC_interp_linear(12,row,keep,2,bad,2,fill)==-1 && row[2]==99);
      CHECK(INSYNC_interp_linear(12,row,keep,2,runs,2,fill)==4);
      CHECK(isnan(row[0]) && row[2]==4 && row[4]==9 && row[5]==11);
      CHECK(isnan(row[7]) && row[9]==7 && isnan(row[11]));
      CHECK(fill[2] && fill[4] && fill[5] && fill[9] && !fill[0]);
      row[4]=row[5]=NAN;
      CHECK(INSYNC_interp_linear(12,row,keep,2,runs,1,fill)==1);
      CHECK(isnan(row[4]) && isnan(row[5]));
   }
   {
      INSYNC_missing *m=INSYNC_missing_new(3,5);
      INSYNC_options o={.method=INSYNC_METHOD_LOO,.min_valid=4,.loo_min_others=1};
      float x[]={1,2,3,4,5, 5,4,3,2,1, 2,1,4,5,6};
      unsigned char observed[15]; const INSYNC_component *v;
      memset(observed,1,sizeof(observed)); observed[6]=0; observed[12]=0;
      INSYNC_missing_eval_observed(m,5,x,observed,NULL,5,&o,1,size,member);
      v=INSYNC_missing_components(m);
      CHECK(v[1].overlap==4 && v[1].samples==5 && v[1].used==4 && v[1].used_samples==5);
      CHECK(v[9].overlap==3 && v[9].samples==5 && isnan(v[9].r) && v[9].used_samples==0);
      CHECK(INSYNC_missing_subjects(m)[1]==4);
      INSYNC_missing_free(m);
   }
   {
      INSYNC_missing *m=INSYNC_missing_new(3,5);
      INSYNC_options o={.method=INSYNC_METHOD_PAIRWISE,.min_valid=3};
      int a[]={0},b[]={1,2}; float out[6];
      float x[]={1,2,3,4,5, 5,4,3,2,1, NAN,NAN,NAN,2,1};
      INSYNC_missing_eval(m,5,x,NULL,5,&o,1,size,member);
      INSYNC_missing_between(m,1,a,2,b,INSYNC_SUMMARY_MEDIAN,out);
      CHECK(out[0]==-1 && out[1]==1 && out[2]==1 && out[3]==0);
      CHECK(out[4]==2 && out[5]==0);
      CHECK(INSYNC_missing_components(m)[1].r==-1);
      INSYNC_missing_free(m);
   }
   {
      /* Zero endpoints cannot affect r; censoring leaves nominal weights in
         place. Three positive samples are the minimum, even with five TRs. */
      INSYNC_kernel *k=INSYNC_kernel_new(3,5);
      INSYNC_missing *m=INSYNC_missing_new(3,5);
      INSYNC_options o={.method=INSYNC_METHOD_PAIRWISE,.min_valid=3,.corr_metric=INSYNC_CORR_PEARSON};
      float x[]={1000,1,2,3,-1000, -2000,1,3,2,2000, 0,3,2,1,0};
      int t;
      CHECK(k && m);
      for(t=0;t<5;t++){
         double weight=INSYNC_window_weight(1,t,5);
         INSYNC_kernel_set_weight(k,t,weight); INSYNC_missing_set_weight(m,t,weight);
      }
      CHECK(INSYNC_window_weight(1,0,5)==0 && INSYNC_window_weight(1,4,5)==0);
      INSYNC_kernel_eval(k,5,x,INSYNC_CORR_PEARSON,INSYNC_METHOD_PAIRWISE,INSYNC_SUMMARY_MEDIAN,1,size,member,stat,n,complete);
      /* Interior weights .5,1,.5 give covariance .5, variances 1 and 1.375. */
      CHECK(fabs(INSYNC_kernel_matrix(k)[1]-.5/sqrt(1.375))<1.e-6);
      INSYNC_missing_eval(m,5,x,NULL,5,&o,1,size,member);
      CHECK(fabs(INSYNC_missing_components(m)[1].r-INSYNC_kernel_matrix(k)[1])<1.e-6);
      x[1]=x[2]=x[3]=7;
      INSYNC_kernel_eval(k,5,x,INSYNC_CORR_PEARSON,INSYNC_METHOD_PAIRWISE,INSYNC_SUMMARY_MEDIAN,1,size,member,stat,n,complete);
      CHECK(isnan(INSYNC_kernel_matrix(k)[1]));
      x[2]=NAN;
      INSYNC_missing_eval(m,5,x,NULL,5,&o,1,size,member);
      CHECK(isnan(INSYNC_missing_components(m)[1].r));
      CHECK(INSYNC_missing_components(m)[1].overlap==4);
      INSYNC_kernel_free(k); INSYNC_missing_free(m);
   }
   puts("PASS: selection/block kernel state, validity, aggregation order, Fisher export and pairwise-missing support"); return 0;
}
