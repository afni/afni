#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int ii , mm , pp , nn , rr , kk , mode ;
   float ph , fac ;
   complex * cx ;

   if( argc < 6 ){ printf("Usage: qft N r k {r,q,l,a} p\n"); exit(1); }

   nn = strtol(argv[1],NULL,10) ; if( nn < 4 ) exit(1) ;
   nn = csfft_nextup(nn) ;

   rr = strtol(argv[2],NULL,10) ; if( rr < 2 ) exit(1) ;
   mm = rr*nn ;

   kk = strtol(argv[3],NULL,10) ;

   switch( argv[4][0] ){
      case 'r': mode = 0  ; break ;
      case 'q': mode = -1 ; break ;
      case 'l': mode = -2 ; break ;
      case 'a': mode =  1 ; break ;

      default: fprintf(stderr,"mode flag illegal\n");exit(1) ;
   }

   fac = strtod(argv[5],NULL) ;

   srand48((long)time(NULL)) ;

   cx = (complex *) malloc(sizeof(complex)*mm) ;

   for( ii=0 ; ii < mm ; ii++ ){
      switch( mode ){
         case  0:  pp = lrand48() ; break ;
         case -1:  pp = fac*ii*ii ; break ;
         case -2:  pp = fac*ii + (lrand48()%3 - 1) ; break ;
         case  1:  pp = fac*ii ; break ;
      }
      pp = (pp+nn) % nn ; printf("%d ",pp) ;

      ph = -(2.0*PI*kk*rr/mm) * pp ; cx[ii] = CEXPIT(ph) ;
   }
   printf("\n\n");

   csfft_cox( 1 , mm , cx ) ;

   for( ii=0 ; ii < mm ; ii++ ){
      printf("  %4d: %13.6f %13.6f" , ii,cx[ii].r,cx[ii].i ) ;
      if( ii%3 == 2 ) printf("\n") ;
   }
   printf("\n") ; exit(0) ;
}
