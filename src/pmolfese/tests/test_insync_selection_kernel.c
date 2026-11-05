/* Repeated selection calls must clear invalid state and expose valid matrices. */
#include <math.h>
#include <stdio.h>
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
   puts("PASS: selection/block kernel state, validity, aggregation order and Fisher export"); return 0;
}
