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
  Inputs: rr[0..nr-1] = array of correlation coefficients.
--------------------------------------------------------------*/

#undef TANHALL

float unusuality( int nr , float * rr )
{
   int ii , nzero , mzero ;
   float * zz , * aa ;
   float zmid,zsig,zmed, uval, fac, zrat, ff,fp, ss,ds,pp,ee , sigma ;
#ifndef TANHALL
   float rmid , rcut ;
#endif

   if( nr < 1000 || rr == NULL ) return 0.0 ;

   /*-- make workspace --*/

   zz = (float *) malloc(sizeof(float)*nr*2) ; aa = zz + nr ;

   if( zstar <= 0.0 ){
      char * cp = getenv("PTAIL") ;
      float pp = 0.01 ;
      if( cp != NULL ){
         float xx = strtod( cp , NULL ) ;
         if( xx > 0.0 && xx < 1.0 ) pp = xx ;
      }
      set_unusuality_tail( pp ) ;
   }

   /*-- copy data into workspace, converting to atanh --*/

   memcpy( zz , rr , sizeof(float)*nr ) ;
   qsort_float( nr , zz ) ;                           /* sort now */

   /*- trim off 1's (perfect correlations) -*/

   for( ii=nr-1 ; ii > 0 && zz[ii] > 0.999 ; ii-- ) ; /* nada */
   if( ii == 0 ){ free(zz) ; return 0.0 ; }           /* shouldn't happen */
   nr = ii+1 ;                                        /* the trim */

#ifdef TANHALL
   for( ii=0 ; ii < nr ; ii++ ) zz[ii] = atanh(rr[ii]) ;
#endif

   /*-- find median of zz [brute force sort] --*/

   if( nr%2 == 1 )              /* median */
      zmid = zz[nr/2] ;
   else
      zmid = 0.5 * ( zz[nr/2] + zz[nr/2-1] ) ;

#ifdef TANHALL
   for( ii=0 ; ii < nr ; ii++ ) aa[ii] = fabs(zz[ii]-zmid) ;
#else
   rmid = zmid ; zmid = atanh(zmid) ;
   for( ii=0 ; ii < nr ; ii++ )
      aa[ii] = fabs( (zz[ii]-rmid)/(1.0-zz[ii]*rmid) ) ;
#endif

   /*-- find MAD of zz --*/

   qsort_float( nr , aa ) ;
   if( nr%2 == 1 )              /* MAD = median absolute deviation */
      zmed = aa[nr/2] ;
   else
      zmed = 0.5 * ( aa[nr/2] + aa[nr/2-1] ) ;

#ifndef TANHALL
   zmed = atanh(zmed) ;
#endif

   zsig = 1.4826 * zmed ;           /* estimate standard deviation of zz */
                                    /* 1/1.4826 = sqrt(2)*erfinv(0.5)    */

   if( zsig <= 0.0 ){               /* should not happen */
      free(zz) ; return 0.0 ;
   }

   /*-- normalize zz (is already sorted) --*/
   /*-- then, find values >= zstar       --*/

   fac = 1.0 / zsig ;
#ifdef TANHALL
   for( ii=0 ; ii < nr ; ii++ ) zz[ii] = fac * ( zz[ii] - zmid ) ;
   for( ii=nr-1 ; ii > 0 ; ii-- ) if( zz[ii] < zstar ) break ;
   nzero = ii+1 ; mzero = nr - nzero ;
#else
   rcut = tanh( zsig * zstar + zmid ) ;
   for( ii=nr-1 ; ii > 0 ; ii-- ){
      if( zz[ii] < rcut ) break ;
      else                zz[ii] = fac * ( atanh(zz[ii]) - zmid ) ;
   }
   nzero = ii+1 ; mzero = nr - nzero ;

#if 0
   fprintf(stderr,"uuu: nr=%d rcut=%g mzero=%d\n",nr,rcut,mzero) ;
#endif
#endif

   if( nzero < 2 || mzero < MAX(1.0,pstar*nr) ){          /* too weird */
      free(zz) ; return 0.0 ;
   }

   /*-- compute sigma-tilde squared --*/

   zsig = 0.0 ;
   for( ii=nzero ; ii < nr ; ii++ ) zsig += zz[ii]*zz[ii] ;
   zsig = zsig / mzero ;

   /*-- set up to compute f(s) --*/

#define SQRT_2PI 2.5066283

   zrat = zstar*zstar / zsig ;
   fac  = ( zrat * nzero ) / ( SQRT_2PI * mzero ) ;

   ss   = zstar ;          /* initial guess for s = zstar/sigma */

   /*-- Newton's method [almost] --*/

#undef  PHI
#define PHI(s) (1.0-0.5*normal_t2p(ss))           /* N(0,1) cdf */

   for( ii=0 ; ii < 5 ; ii++ ){
      pp = PHI(ss) ;                              /* Phi(ss) \approx 1 */
      ee = exp(-0.5*ss*ss) ;

      ff = ss*ss - (fac/pp) * ss * ee - zrat ;    /* f(s) */

      fp = 2.0*ss + (fac/pp) * ee * (ss*ss-1.0) ; /* f'(s) */

      ds = ff / fp ;                              /* Newton step */

#if 0
      fprintf(stderr,"Newton: ss=%g ds=%g ff=%g fp=%g pp=%g\n",ss,ds,ff,fp,pp) ;
#endif

      ss -= ds ;                                  /* update */
   }

   sigma = zstar / ss ;                           /* actual estimate of sigma */
                                                  /* from the upper tail data */

   if( sigma <= 1.0 ){                            /* the boring case */
      free(zz) ; return 0.0 ;
   }

   /*-- compute the log-likelihood difference next --*/

   uval =  nzero * log( PHI(ss)/(1.0-pstar) )
         - mzero * ( log(sigma) + 0.5 * zsig * (1.0/(sigma*sigma)-1.0) ) ;

   /*-- done! --*/

   free(zz) ; return uval ;
}
