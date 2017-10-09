/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include <stdlib.h>

/**** test FFT speeds ****/

int main( int argc , char * argv[] )
{
   int num , len , ii , type_ux=0 , kk , iv , nvec , nvec_in ;
   complex * cx ;
   float fac ;
   double tcpu , tclock ;
   int narg=1 , quiet=0 ;
   float kbytes ;

   if( argc < 4 ){printf("Usage: fftest [-q] len num nvec\n");exit(0);}

   if( strcmp(argv[narg],"-q") == 0 ){ quiet++ ; narg++ ; }
   if( strcmp(argv[narg],"-q") == 0 ){ quiet++ ; narg++ ; }
   if( strcmp(argv[narg],"-q") == 0 ){ quiet++ ; narg++ ; }
   if( strcmp(argv[narg],"-q") == 0 ){ quiet++ ; narg++ ; }

   (void) my_getenv("TMPDIR") ;
   if( AFNI_yesenv("AFNI_USE_FFTN") ) csfft_force_fftn(1) ;

   len = strtol( argv[narg++] , NULL , 10 ) ;
   num = strtol( argv[narg++] , NULL , 10 ) ;
   nvec_in = strtol( argv[narg++] , NULL , 10 ) ;

   if( len > 0 && len != csfft_nextup(len) ){
      fprintf(stderr,"Can't do FFT of length %d; try %d\n",len,csfft_nextup(len));
      exit(1) ;
   } else if ( len < 0 ){
      len = -len ;
   }

   if( nvec_in < 1 ){fprintf(stderr,"Illegal nvec value!\n"); exit(1) ;}

   cx = (complex *) malloc( sizeof(complex) * len * nvec_in) ;

   kbytes = sizeof(complex) * len * nvec_in / 1024.0 ;

   for( ii=0 ; ii < len*nvec_in ; ii++ ){ cx[ii].r = ii ; cx[ii].i = -0.3*ii ; }

   fac = sqrt(1.0/len) ;
   if( ! quiet ) printf("starting %d FFTs of length %d -- %d at a time\n",
                        num,len,nvec_in ) ;
   tcpu = COX_cpu_time() ;
   tclock = COX_clock_time() ;
   if( nvec_in == 1 ){
      if( ! quiet ) fprintf(stderr,"Using csfft\n") ;
      for( kk=0 ; kk < num ; kk++ ){
         csfft_cox( -1 , len , cx ) ;
         for( ii=0 ; ii < len ; ii++ ){ cx[ii].r *= fac ; cx[ii].i *= fac ; }
      }
   } else {
      if( ! quiet ) fprintf(stderr,"Using csfft_many\n") ;
      for( kk=0 ; kk < num ; kk+=nvec_in ){
         nvec = num - kk ; if( nvec > nvec_in ) nvec = nvec_in ;
         csfft_many( -1 , len , nvec , cx ) ;
         for( ii=0 ; ii < len*nvec ; ii++ ){ cx[ii].r *= fac ; cx[ii].i *= fac ; }
      }
   }
   tcpu = COX_cpu_time() - tcpu ;
   tclock = COX_clock_time() - tclock ;

   { double flops , m ;
     m = log( (double)len ) / log( (double)2.0 ) ;
     flops = (5.0 * m - 1.0 ) * len * num / tcpu / 1.e6;

     if(quiet == 1)
        printf("%8f %8f\n",kbytes,flops) ;
     else if(quiet == 2)
        printf("%8f\n",flops) ;
     else if(quiet == 3)
        printf("%8d %8f\n",len,flops) ;
     else
        printf(" CPU = %f  ELAPSED = %f  Kbytes = %f  Megaflops = %f\n",
               tcpu,tclock,kbytes,flops);
     fflush(stdout) ;
   }
   exit(0) ;
}
