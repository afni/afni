#include <stdio.h>
#include <math.h>
#include "cs_laguerre.c"

int main( int argc , char * argv[] )
{
   int n,ii ;
   double * xx , * ww , a,b , xc=0.5 , ss=0.0 , ff ;

   if( argc < 4 ){printf("Usage: lll n a b\n");exit(0);}

   n = (int) strtod(argv[1],NULL ) ;
   get_laguerre_table( n , &xx , &ww ) ;
   if( xx == NULL || ww == NULL ) exit(1) ;

   a = strtod(argv[2],NULL) ; b = strtod(argv[3],NULL) ;

   if( argc > 4 ) xc = strtod(argv[4],NULL) ;

   for( ii=n-1 ; ii >= 0 ; ii-- ){
      ff = 1.0 - xc*exp(-xx[ii]/a) ; ff = pow(ff,b-1.0) ; ss += ww[ii] * ff ;
   }

   ss *= pow(xc,a) / a ;

   printf("sum=%.10g\n",ss) ; exit(0) ;
}
