#include <math.h>

/*-----------------------------------------------------------------------------
   Stuff for the non-central t distribtion moments.
-------------------------------------------------------------------------------*/

double nct_fac( double nu , int p )
{
   double fac ;
   int ii ;

   fac = 1.0 ;
   for( ii=p ; ii > 0 ; ii -= 2 ) fac *= nu / (nu-ii) ;

   if( p%2 == 1 ){               /* code from Maple V */
      double t1,t2,t3,t4,t8 ;    /* to approximate    */
                                 /*   (nu-1) * sqrt(nu/2) * GAMMA((nu-1)/2)   */
                                 /*   -------------------------------------   */
                                 /*             nu * GAMMA(nu/2)              */
      t1 = 1.0/nu ;
      t2 = 6.0*t1-1.0 ;
      t3 = t2*t2 ;
      t4 = t3*t2 ;
      t8 = (  0.696566964988        + 0.145169284551E1  *t1
            + 0.314558390354E-1 *t3 + 0.33793456015E-3  *t4 )
          /(  0.685844798446        + 0.175451410609E1  *t1
            + 0.434723677435E-1 *t3 + 0.163229741459E-2 *t4 ) ;

      fac *= t8 ;                /* accurate to 1e-9 for nu > 3 */
   }
   return fac ;
}

/*------------------------------------------------------------
  p-th moment about 0 of non-central t
--------------------------------------------------------------*/

double nct_mom( double delta , double nu , int p )
{
   double sum , fterm , dsq ;
   int nn ;

   if( p < 0 || p >= nu ) return 0.0 ;          /* illegal */
   if( p == 0 ) return 1.0 ;                    /* expectation of 1 */

   if( delta == 0.0 && p%2 == 1 ) return 0.0 ;  /* odd moment of central t */

   dsq = delta * delta ;

   switch( p ){

      case 1: sum = delta                       ; break ;
      case 2: sum = dsq + 1.0                   ; break ;
      case 3: sum = delta * ( dsq + 3.0 )       ; break ;
      case 4: sum = dsq * dsq + 6.0 * dsq + 3.0 ; break ;

      default:
         fterm = 1.0 ;
         sum   = pow( delta , (double) p ) ;
         for( nn=2 ; nn <= p ; nn += 2 ){
            fterm *= (p+1-nn) * (p+2-nn) / nn ;
            if( p > nn ) sum += fterm * pow( delta , (double)(p-nn) ) ;
            else         sum += fterm ;
         }
      break ;
   }

   return ( sum * nct_fac(nu,p) ) ;
}
