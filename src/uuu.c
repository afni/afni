#include "mrilib.h"

/*------------------------------------------------------------
  Set the one-sided tail probability that we will
  cutoff the unusuality test at.
--------------------------------------------------------------*/

static float zstar = 0.0 ;            /* the actual cutoff */

void set_unusuality_tail( float p )
{
   if( p > 0.0 && p < 1.0 ) zstar = qginv(p) ;
   return ;
}

/*------------------------------------------------------------
  Inputs: rr[0..nr-1] = array of correlation coefficients.
--------------------------------------------------------------*/

float unusuality( int nr , float * rr )
{
   int ii , nzero , mzero ;
   float * zz , * aa ;
   float zmid,zsig,zmed, uval, fac, zrat, ff,fp, ss,ds,pp,ee , sigma ;

   if( nr < 1000 || rr == NULL ) return 0.0 ;

   /*-- make workspaces --*/

   zz = (float *) malloc(sizeof(float)*nr) ;
   aa = (float *) malloc(sizeof(float)*nr) ;

   if( zstar <= 0.0 ) zstar = qginv( 0.01 ) ;  /* default */

   /*-- copy data into workspace, converting to atanh --*/

   for( ii=0 ; ii < nr ; ii++ ) zz[ii] = atanh(rr[ii]) ;

   /*-- find median of zz [sort of brute force] --*/

   qsort_float( nr , zz ) ;
   if( nr%2 == 1 )              /* median */
      zmid = zz[nr/2] ;
   else
      zmid = 0.5 * ( zz[nr/2] + zz[nr/2-1] ) ;

   /*-- find MAD of zz --*/

   for( ii=0 ; ii < nr ; ii++ ) aa[ii] = fabs(zz[ii]-zmid) ;
   qsort_float( nr , aa ) ;
   if( nr%2 == 1 )              /* MAD = median absolute deviation */
      zmed = aa[nr/2] ;
   else
      zmed = 0.5 * ( aa[nr/2] + aa[nr/2-1] ) ;

   zsig = 1.4826 * zmed ;           /* estimate standard deviation of zz */
                                    /* 1/1.4826 = sqrt(2)*erfinv(0.5)    */

   if( zsig <= 0.0 ) zsig = 1.0 ;   /* should not happen */

   /*-- normalize zz (is already sorted) --*/

   fac = 1.0 / zsig ;
   for( ii=0 ; ii < nr ; ii++ ) zz[ii] = fac * ( zz[ii] - zmid ) ;

   /*-- find values >= zstar --*/

   for( ii=nr-1 ; ii > 0 ; ii-- ) if( zz[ii] < zstar ) break ;
   nzero = ii+1 ; mzero = nr - mzero ;

   if( nzero < 2 || mzero < 1 ){          /* too weird */
      free(zz) ; free(aa) ; return 0.0 ;
   }

   /*-- compute sigma-tilde squared --*/

   zsig = 0.0 ;
   for( ii=nzero ; ii < nr ; ii++ ) zsig += zz[ii]*zz[ii] ;
   zsig = zsig / mzero ;

   /*-- set up to compute f(s) --*/

#define SQRT_2PI 2.5066283

   zrat = zstar*zstar / zsig ;
   fac  = ( zrat * nzero ) / ( SQRT_2PI * mzero ) ;

   ss   = sqrt(zrat) ;  /* initial guess for s = zstar/sigma */

   /*-- Newton's method [almost] --*/

#undef  PHI
#define PHI(s) (1.0-0.5*normal_t2p(ss))

   for( ii=0 ; ii < 5 ; ii++ ){
      pp = PHI(ss) ;                              /* Phi(ss) \approx 1 */
      ee = exp(-0.5*ss*ss) ;

      ff = ss*ss - (fac/pp) * ss * ee + zrat ;    /* f(s) */

      fp = 2.0*ss - (fac/pp) * ee * (ss*ss-1.0) ; /* f'(s) */

      ds = ff / fp ;                              /* Newton step */

      fprintf(stderr,"Newton: ss=%g ds=%g ff=%g fp=%g pp=%g\n",ss,ds,ff,fp,pp) ;

      ss -= ds ;                                  /* update */
   }

   sigma = zstar / ss ;                           /* actual estimate of sigma */
                                                  /* from the upper tail data */

   /*-- compute the log-likelihood difference next --*/

   uval =  nzero * log( PHI(ss)/PHI(zstar) )
         - mzero * ( log( sigma )  + 0.5 * zsig * (1.0/(sigma*sigma)-1.0) ) ;

   /*-- done! --*/

   free(zz) ; free(aa) ;

   return uval ;
}
