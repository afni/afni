#include "mrilib.h"

/*------------------------------------------------------------
  Set the one-sided tail probability at which we will cutoff
  the unusuality test.
--------------------------------------------------------------*/

static float zstar = 0.0 ;            /* the actual cutoff */
static float pstar = 0.0 ;            /* tail probability  */

void set_unusuality_tail( float p )
{
   if( p > 0.0 && p < 1.0 ){
      zstar = qginv(p) ;
      pstar = p ;
   }
   return ;
}

#define NEAR1 0.999

/*------------------------------------------------------------
  Inputs:  rr[0..nr-1] = array of correlation coefficients.
  Outputs: ihi[0..*nhi-1] = indexes of highly correlations
             from rr.  ihi must be declared at least length
             nr to allow for the worst case.
--------------------------------------------------------------*/

void find_unusual_correlations( int nrin  , float * rr ,
                                int * nhi , int * ihi   )
{
   register int ii,jj,nr ;
   register float *zz ;
   float zmid,zsig,zmed , rmid,rcut ;

   if( nhi == NULL ) return ;  /* illegal entry */

   *nhi = 0 ;                  /* default return */

   if( nrin < 1000 || rr == NULL || ihi == NULL  ) return ;  /* illegal entry */

   /*-- make workspace --*/

   zz = (float *) malloc(sizeof(float)*nrin) ;

   if( zstar <= 0.0 ){               /* initialize zstar if needed */
      char * cp = getenv("PTAIL") ;
      float pp = 0.0001 ;
      if( cp != NULL ){
         float xx = strtod( cp , NULL ) ;
         if( xx > 0.0 && xx < 1.0 ) pp = xx ;
      }
      set_unusuality_tail( pp ) ;
   }

   /*-- copy data into workspace (eliding 1's) --*/

   for( ii=jj=0 ; ii < nrin ; ii++ )
      if( rr[ii] <= NEAR1 ) zz[jj++] = rr[ii] ;
   nr = jj ;
   if( nr < 2 ){ free(zz); return; } /* shouldn't happen */

   rmid = qmed_float( nr , zz ) ;  /* median of correlations */
   zmid = atanh(rmid) ;            /* median of atanh(rr) */

   /*-- zz <- fabs( tanh( atanh(zz) - atanh(rmid) ) ) --*/

   for( ii=0 ; ii < nr ; ii++ )
      zz[ii] = fabs( (zz[ii]-rmid)/(1.0-zz[ii]*rmid) ) ;

   /*-- find MAD of atanh(rr) --*/

   zmed = qmed_float( nr , zz ) ; /* MAD = median absolute deviation of rr */
   zmed = atanh(zmed) ;           /* MAD of atanh(rr) */
   zsig = 1.4826 * zmed ;         /* estimate standard dev. of atanh(rr) */
                                  /* 1.4826 = 1/(sqrt(2)*erfinv(0.5))    */
   free(zz) ;

   if( zsig <= 0.0 ) return ; /* shouldn't happen */

   /*-- find values of correlation greater than threshold --*/
   /*-- that is, with (atanh(rr)-zmid)/zsig > zstar       --*/

   rcut = tanh( zsig*zstar + zmid ) ;

   for( ii=jj=0 ; ii < nrin ; ii++ )
      if( rr[ii] >= rcut && rr[ii] <= NEAR1 ) ihi[jj++] = ii ;

   *nhi = jj ; return ;
}
