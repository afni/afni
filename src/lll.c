#include <stdio.h>
#include <math.h>
#include "cs_laguerre.c"

int main( int argc , char * argv[] )
{
   int n,ii , narg=1 , nfun=-1 ;
   double * xx , * ww , a,b , xc=0.5 , xv,ss,tt,uu , ff ;

   if( argc < 3 ){printf("Usage: lll a b [xc [0|1]]\n");exit(0);}

   n = 20 ;
   get_laguerre_table( n , &xx , &ww ) ;
   if( xx == NULL || ww == NULL ) exit(1) ;

   a = strtod(argv[narg++],NULL) ; b = strtod(argv[narg++],NULL) ;

   if( narg < argc ) xc = strtod(argv[narg++],NULL) ;
   if( narg < argc ) nfun = (int) strtod(argv[narg++],NULL) ;

   ss=tt=uu=0.0 ;
   for( ii=n-1 ; ii >= 0 ; ii-- ){
      xv = xc*exp(-xx[ii]/a) ;
      ff = pow(1.0-xv,b-1.0) ;
      ss += ww[ii] * ff ;
      tt += ww[ii] * ff * log(xv) ;
      uu += ww[ii] * ff * log(1.0-xv) ;
   }

   tt = tt / ss ; uu = uu / ss ;

   switch( nfun ){
      default: printf("<log(x)>=%15.10g  <log(1-x)>=%15.10g\n",tt,uu) ; break ;

      case 0: printf("%g\n",tt) ; break ;
      case 1: printf("%g\n",uu) ; break ;
   }
   exit(0) ;
}
