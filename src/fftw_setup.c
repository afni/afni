#include <stdio.h>
#include "fftw.h"
#include "mrilib.h"

/*** write some FFTW wisdom ***/

int main( int argc , char * argv[] )
{
   fftw_plan p ;
   char fname[1024]="\0" , *w ;
   int ii , len[] = { 1024 , 3600 , 784 , 2000 , 864 , 0 } ;
   double cpu_old , cpu_new ;

   cpu_old = COX_cpu_time() ;
   for( ii=0 ; len[ii] > 1 ; ii++ ){
      fprintf(stderr,"FFTW calibrating N=%5d",len[ii]) ; fflush(stdout) ;
      p = fftw_create_plan(len[ii], FFTW_FORWARD , FFTW_MEASURE|FFTW_USE_WISDOM);
      fftw_destroy_plan(p) ;
      fprintf(stderr,".") ; fflush(stdout) ;
      p = fftw_create_plan(len[ii], FFTW_BACKWARD, FFTW_MEASURE|FFTW_USE_WISDOM);
      fftw_destroy_plan(p) ;
      fprintf(stderr,".") ; fflush(stdout) ;
      cpu_new = COX_cpu_time() ;
      fprintf(stderr," %.2f CPU s\n",cpu_new-cpu_old) ; cpu_old = cpu_new ;
   }

   w = fftw_export_wisdom_to_string() ;
   printf("%s\n",w) ;
   fftw_free(w) ;
   exit(0) ;
}
