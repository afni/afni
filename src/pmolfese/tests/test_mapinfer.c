#include "thd_mapinfer.h"

#include <math.h>
#include <stdio.h>

static int nearf( float a, float b )
{
   return fabsf(a-b) <= 1.0e-7f ;
}

static int check_array( const char *name, int n,
                        const float *got, const float *want )
{
   int ii ;
   for( ii=0 ; ii<n ; ii++ ){
     if( !nearf(got[ii],want[ii]) ){
       fprintf(stderr,"FAIL %s[%d]: got %.9g, wanted %.9g\n",
               name,ii,got[ii],want[ii]) ;
       return 1 ;
     }
   }
   return 0 ;
}

int main( void )
{
   float p[4]={0.01f,0.04f,0.03f,0.002f} ;
   float q[4],want[4]={0.02f,0.04f,0.04f,0.008f} ;
   float masked[4],mwant[4]={0.015f,1.0f,0.03f,0.006f} ;
   unsigned char valid[4]={1,0,1,1} ;
   float tie[3]={0.5f,0.5f,1.0f},twant[3]={0.75f,0.75f,1.0f} ;
   float dst[4]={-2.0f,5.0f,3.0f,0.0f} ;
   float src[4]={-1.0f,4.0f,8.0f,0.0f} ;
   float xmax[4]={-1.0f,5.0f,8.0f,0.0f} ;
   THD_memory_plan plan={0} ;
   int fail=0 ;

   THD_bh_fdr(4,p,q) ;
   fail+=check_array("plain BH",4,q,want) ;
   THD_bh_fdr_masked(4,p,valid,masked) ;
   fail+=check_array("masked BH",4,masked,mwant) ;
   THD_bh_fdr(3,tie,tie) ;
   fail+=check_array("aliased tied BH",3,tie,twant) ;
   THD_bh_fdr_masked(0,NULL,NULL,NULL) ;

   THD_max_accum(4,dst,src) ;
   fail+=check_array("max accumulation",4,dst,xmax) ;

   plan.input=1.0 ; plan.geometry=2.0 ; plan.shared=3.0 ; plan.output=4.0 ;
   plan.per_thread=5.0 ; plan.nthread=3 ; plan.system=99.0 ; plan.limit=50.0 ;
   THD_memory_plan_finish(&plan) ;
   if( plan.total != 25.0 || plan.system != 99.0 || plan.limit != 50.0 ){
     fprintf(stderr,"FAIL memory plan: total %.9g system %.9g limit %.9g\n",
             plan.total,plan.system,plan.limit) ; fail++ ;
   }
   THD_memory_plan_finish(NULL) ;

   if( fail ) return 1 ;
   puts("PASS thd_mapinfer BH/masked-BH/max/memory-plan cases") ;
   return 0 ;
}
