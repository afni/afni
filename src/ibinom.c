#include "mrilib.h"

int main( int argc , char * argv[] )
{
   double qq , nn , pp , tt ;

   if( argc < 4 || strcmp(argv[1],"-help")==0 ){
      printf("Usage: ibinom Q N p\n"
             "  Q = upper tail probability\n"
             "  N = number of trials\n"
             "  p = probability of success per trial\n"
             "Output is value 'm' such that the binomial probability\n"
             "of getting m or more successes in N trials is Q.\n"
            ) ;
      exit(0) ;
   }

   qq = strtod( argv[1] , NULL ) ;
   nn = strtod( argv[2] , NULL ) ;
   pp = strtod( argv[3] , NULL ) ;

#define BAD(str) ( fprintf(stderr,"Bad value of %s\n",str) , exit(1) )

   if( qq <= 0.0 || qq >= 1.0 ) BAD("Q") ;
   if( nn <= 1.0 )              BAD("N") ;
   if( pp <= 0.0 || pp >= 1.0 ) BAD("p") ;

   tt = binomial_p2t( qq , nn , pp ) ;
   printf("threshold = %g\n",tt) ;
   exit(0) ;
}
