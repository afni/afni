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

/*------------------------------------------------------------
  Inputs:  rr[0..nr-1] = array of correlation coefficients.
  Outputs: ihi[0..*nhi-1] = indexes of highly correlations
             from rr.  ihi must be declared at least length
             nr to allow for the worst case.
--------------------------------------------------------------*/

void find_unusual_correlations( int nr    , float * rr ,
                                int * nhi , int * ihi   )
{
   int   ii , nzero,mzero ;
   float zmid,zsig,zmed , rmid,rcut ;
   float * zz , * aa ;
   int   * iz ;

   if( nhi == NULL ) return ;  /* illegal entry */

   *nhi = 0 ;                  /* default return */

   if( nr < 1000 || rr == NULL || ihi == NULL ) return ;  /* illegal entry */

   /*-- make workspace --*/

   zz = (float *) malloc(sizeof(float)*nr*2) ; aa = zz + nr ;
   iz = (int *)   malloc(sizeof(int)  *nr  ) ;

   if( zstar <= 0.0 ){               /* initialize zstar if needed */
      char * cp = getenv("PTAIL") ;
      float pp = 0.0001 ;
      if( cp != NULL ){
         float xx = strtod( cp , NULL ) ;
         if( xx > 0.0 && xx < 1.0 ) pp = xx ;
      }
      set_unusuality_tail( pp ) ;
   }

   /*-- copy data into workspace --*/

   memcpy( zz , rr , sizeof(float)*nr ) ;             /* the copy */
   for( ii=0 ; ii < nr ; ii++ ) iz[ii] = ii ;         /* tag location */
   qsort_floatint( nr , zz , iz ) ;                   /* sort now */

   /*- trim off 1's (perfect correlations) -*/

   for( ii=nr-1 ; ii > 0 && zz[ii] > 0.999 ; ii-- ) ;  /* nada */
   if( ii == 0 ){ free(zz); free(iz); return; }        /* shouldn't happen */
   nr = ii+1 ;                                         /* the trim */

   /*-- find median of atanh(rr) --*/

   if( nr%2 == 1 )              /* median */
      zmid = zz[nr/2] ;
   else
      zmid = 0.5 * ( zz[nr/2] + zz[nr/2-1] ) ;

   rmid = zmid ;        /* median of rr */
   zmid = atanh(zmid) ; /* median of atanh(rr) */

   /*-- aa = fabs( tanh( atanh(zz) - atanh(rmid) ) ) --*/

   for( ii=0 ; ii < nr ; ii++ )
      aa[ii] = fabs( (zz[ii]-rmid)/(1.0-zz[ii]*rmid) ) ;

   /*-- find MAD of atanh(rr) --*/

   qsort_float( nr , aa ) ;
   if( nr%2 == 1 )              /* MAD = median absolute deviation */
      zmed = aa[nr/2] ;
   else
      zmed = 0.5 * ( aa[nr/2] + aa[nr/2-1] ) ;

   zmed = atanh(zmed) ;         /* MAD of atanh(rr) */
   zsig = 1.4826 * zmed ;       /* estimate standard dev. of atanh(rr) */
                                /* 1.4826 = 1/(sqrt(2)*erfinv(0.5))    */

   if( zsig <= 0.0 ){ free(zz); free(iz); return; } /* shouldn't happen */

   /*-- find values of correlation greater than threshold --*/
   /*-- that is, with (atanh(rr)-zmid)/zsig > zstar       --*/

   rcut = tanh( zsig*zstar + zmid ) ;
   for( ii=nr-1 ; ii > 0 ; ii-- ) if( zz[ii] < rcut ) break ;

   nzero = ii+1 ; mzero = nr - nzero ;

   *nhi = mzero ;
   for( ii=0 ; ii < mzero ; ii++ ) ihi[ii] = iz[nzero+ii] ;

   free(zz) ; free(iz) ; return ;
}
