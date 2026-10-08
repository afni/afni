#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "../../thd_insync.h"

#define CHECK(COND,MSG) do{ if(!(COND)){ fprintf(stderr,"FAIL: %s\n",MSG); return 1; } }while(0)
#define CLOSE(A,B,T) (fabs((double)(A)-(double)(B)) <= (T))

int main(void)
{
   float x[5]={-2,-1,0,1,2} ;
   float y[5]={-4,-2,0,2,4} ;
   float z[5]={-2,0,-1,2,1} ;
   float data[15],corr[9],scratch[8],ref[5],values[3] ;
   float rank1[5],rank2[5] ;
   float simple[2]={0.0f,0.5f},stat,expect ;
   int group[3]={0,0,0},sample[3]={0,0,2},nv=0,ii ;

   for( ii=0 ; ii<5 ; ii++ ){
     data[ii]=x[ii] ; data[5+ii]=y[ii] ; data[10+ii]=z[ii] ;
   }
   corr[0]=corr[4]=corr[8]=1.0f ;
   corr[1]=corr[3]=1.0f ;
   corr[2]=corr[6]=0.8f ;
   corr[5]=corr[7]=0.8f ;

   stat=INSYNC_pairwise_group(3,corr,group,0,INSYNC_SUMMARY_MEDIAN,scratch,&nv) ;
   CHECK(nv==3,"pairwise valid edge count") ;
   CHECK(CLOSE(stat,0.8,1e-6),"pairwise median") ;

   expect=(float)tanh(0.5*atanh(0.5)) ;
   stat=INSYNC_summary(2,simple,INSYNC_SUMMARY_FISHER_MEAN,scratch,&nv) ;
   CHECK(nv==2 && CLOSE(stat,expect,1e-6),"Fisher-z mean") ;

   stat=INSYNC_loo_group(3,5,data,group,0,INSYNC_SUMMARY_MEDIAN,
                         ref,values,&nv) ;
   CHECK(nv==3 && isfinite(stat),"leave-one-out summary") ;
   for( ii=0 ; ii<3 ; ii++ ) CHECK(isfinite(values[ii]),"leave-one-out value") ;

   stat=INSYNC_pairwise_indexed(3,corr,3,sample,INSYNC_SUMMARY_MEDIAN,
                                scratch,&nv) ;
   CHECK(nv==2 && CLOSE(stat,0.8,1e-6),"bootstrap duplicate diagonal omitted") ;

   stat=INSYNC_loo_indexed(3,5,data,3,sample,INSYNC_SUMMARY_MEDIAN,
                           ref,values,&nv) ;
   CHECK(nv==3 && isfinite(stat),"indexed leave-one-out summary") ;

   { float monotonic[15]={1,2,3,4,5, 1,4,9,16,25, 2,3,4,5,6} ;
     int member[3]={0,1,2} ;
     stat=INSYNC_loo_indexed_metric(3,5,monotonic,3,member,
             INSYNC_SUMMARY_MEDIAN,INSYNC_CORR_SPEARMAN,
             ref,values,rank1,rank2,&nv) ;
     CHECK(nv==3 && CLOSE(stat,1.0,1e-6),
           "Spearman leave-one-out average-rank correlation") ; }

   { float pct[4]={0.0f,10.0f,20.0f,30.0f} ;
     CHECK(CLOSE(INSYNC_percentile(pct,4,0.25f),7.5,1e-6),"percentile interpolation") ; }

   puts("PASS: thd_insync numeric tests") ;
   return 0 ;
}
