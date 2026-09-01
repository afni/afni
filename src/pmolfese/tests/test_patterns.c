#include "mrilib.h"
#include "thd_patterns.h"

#include <stdio.h>
#include <string.h>

static int parser_test( void )
{
   const char *spec[5]={"2","SPHERE(2)","rect(1,2,3)","RHDD(2)","TOHD(2)"} ;
   const char *bad[]={"SPHERE(0)","SPHERE(-1)","SPHERE(nan)","SPHERE(2)junk",
                      "RECT(1,2)","RECT(1,0,2)","RECT(1,2,inf)","2junk","nan"} ;
   MCW_cluster *cc[5]={NULL,NULL,NULL,NULL,NULL} ;
   char err[256] ;
   int ii,fail=0 ;

   for( ii=0 ; ii<5 ; ii++ ){
     cc[ii]=THD_searchlight_parse(spec[ii],1.0f,1.0f,1.0f,err,sizeof(err)) ;
     if( cc[ii]==NULL || cc[ii]->num_pt<1 ){
       fprintf(stderr,"FAIL parser %s: %s\n",spec[ii],err) ; fail=1 ;
     }
   }
   if( cc[0]!=NULL && cc[1]!=NULL && cc[0]->num_pt!=cc[1]->num_pt ){
     fprintf(stderr,"FAIL bare radius differs from SPHERE radius\n") ; fail=1 ;
   }
   for( ii=0 ; ii<5 ; ii++ ) if( cc[ii]!=NULL ) KILL_CLUSTER(cc[ii]) ;

   cc[0]=THD_searchlight_parse("SPHERE(0)",1.0f,1.0f,1.0f,err,sizeof(err)) ;
   if( cc[0]!=NULL || strstr(err,"finite")==NULL || strstr(err,"> 0")==NULL ){
     fprintf(stderr,"FAIL invalid-radius diagnostic: %s\n",err) ; fail=1 ;
   }
   for( ii=0 ; ii<(int)(sizeof(bad)/sizeof(bad[0])) ; ii++ ){
     cc[0]=THD_searchlight_parse(bad[ii],1.0f,1.0f,1.0f,err,sizeof(err)) ;
     if( cc[0]!=NULL ){
       fprintf(stderr,"FAIL malformed parser accepted %s\n",bad[ii]) ;
       KILL_CLUSTER(cc[0]) ; fail=1 ;
     }
   }
   cc[0]=THD_searchlight_parse("BANANA(2)",1.0f,1.0f,1.0f,err,sizeof(err)) ;
   if( cc[0]!=NULL || strstr(err,"unknown -searchlight neighborhood")==NULL ){
     fprintf(stderr,"FAIL unknown-shape diagnostic: %s\n",err) ; fail=1 ;
   }
   return fail ;
}

static int paint_test( void )
{
   THD_roilist rl ;
   int v0[2]={0,2},center[2]={-1,3} ;
   intvec vox[2] ;
   float brick[4]={-9.0f,-9.0f,-9.0f,-9.0f} ;
   float values[2]={1.5f,2.5f} ;
   float want[4]={1.5f,-9.0f,1.5f,2.5f} ;
   int ii ;

   memset(&rl,0,sizeof(rl)) ; memset(vox,0,sizeof(vox)) ;
   vox[0].nar=2 ; vox[0].ar=v0 ;
   rl.nroi=2 ; rl.vox=vox ; rl.center=center ;
   THD_roilist_paint(brick,&rl,values) ;
   for( ii=0 ; ii<4 ; ii++ ) if( brick[ii]!=want[ii] ){
     fprintf(stderr,"FAIL paint[%d]: got %.9g, wanted %.9g\n",
             ii,brick[ii],want[ii]) ;
     return 1 ;
   }
   return 0 ;
}

int main( void )
{
   int fail=parser_test()+paint_test() ;
   if( fail ) return 1 ;
   puts("PASS thd_patterns parser and atlas/searchlight painting") ;
   return 0 ;
}
