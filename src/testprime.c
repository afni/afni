#include <stdlib.h>
#include "testprime.h"

/*----- max value of nnn is 2^31-1 so max value of pp is 46337 -----*/

int test_prime( int nnn )
{
   register int ii , pp , np , nloc ;

   if( nnn <= 0 ) return -1 ;
   if( nnn == 1 ) return  0 ;
   np   = NUMplist ;
   nloc = nnn ;
   for( ii=0 ; ii < np ; ii++ ){
     pp = (int)plist[ii] ;
     if( pp*pp      > nloc ) return 1 ; /* not divisible by prime <= sqrt(nloc) */
     if( nloc % pp == 0    ) return 0 ; /* divisible by this prime */
#if 0
     if( pp        == nloc ) return 1 ; /* equals this prime */
#endif
   }
   return 1 ; /* should not be reached */
}

/*----- function called by parser.f -----*/

double isprime_( double xxx )
{
   int nnn = (int)rint(xxx) ;

   if( fabs(xxx-(double)nnn) > 0.001 ) return -1.0 ;

   return (double)test_prime(nnn) ;
}
