#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int N ;
   complex * cx , * cs ;
   int ii ;
   float f , sum , xx,yy ;

   if( argc < 3 ){ printf("Usage: fftcheck N nfreq | more\n"); exit(1); }

   N = strtod( argv[1] , NULL ) ;

   if( N > 0 && N != csfft_nextup(N) ){
      fprintf(stderr,"Can't do FFT of length %d\n",N) ; exit(1) ;
   } else if ( N < 0 ){
      N = -N ;
   }

   f = strtod( argv[2] , NULL ) / N ;

   cs = (complex *) malloc(sizeof(complex)*N) ;
   cx = (complex *) malloc(sizeof(complex)*N) ;

   for( ii=0 ; ii < N ; ii++ ){
      cx[ii].r = cos(2*PI*f*ii) ; cx[ii].i = sin(2*PI*f*ii) ;
      cs[ii] = cx[ii] ;
   }

   csfft_cox( -1 , N , cx ) ;

   for( ii=0 ; ii < N ; ii++ )
      printf("%2d: %12.5g %12.5g\n" , ii , cx[ii].r , cx[ii].i ) ;

   csfft_cox( 1 , N , cx ) ;
   sum = 0.0 ;
   for( ii=0 ; ii < N ; ii++ ){
      xx = cx[ii].r/N - cs[ii].r ;
      yy = cx[ii].i/N - cs[ii].i ; sum += xx*xx + yy*yy ;
   }
   sum = sqrt( sum/N ) ;
   printf("RMS error = %g\n",sum) ;
   exit(0) ;
}
