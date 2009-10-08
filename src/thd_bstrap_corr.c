#include "mrilib.h"

static int acorr_demean = 1 ;

#undef  PHI
#define PHI(x) (1.0-qg(x))       /* CDF of N(0,1) */

#undef  PHINV
#define PHINV(x) qginv(1.0-(x))  /* inverse CDF of N(0,1) */

/*----------------------------------------------------------------------------*/
/*! Pearson correlation of x[] and y[] */

static INLINE float acorrfun( int n, float *x, float *y )
{
   float xv=0.0f , yv=0.0f , xy=0.0f , vv,ww ;
   float xm=0.0f , ym=0.0f ;
   register int jj ;

   if( acorr_demean ){
     for( jj=0 ; jj < n ; jj++ ){ xm += x[jj]; ym += y[jj]; }
     xm /= n ; ym /= n ;
   }
   for( jj=0 ; jj < n ; jj++ ){
     vv = x[jj]-xm; ww = y[jj]-ym; xv += vv*vv; yv += ww*ww; xy += vv*ww;
   }

   if( xv <= 0.0f || yv <= 0.0f ) return 0.0f ;
   return atanhf(xy/sqrtf(xv*yv)) ;
}

/*----------------------------------------------------------------------------*/

static INLINE float acorrfun_jack( int n, float *x, float *y, int qq )
{
   float xv=0.0f , yv=0.0f , xy=0.0f , vv,ww ;
   float xm=0.0f , ym=0.0f ;
   register int jj ;

   if( acorr_demean ){
     for( jj=0 ; jj < n ; jj++ ){ if( jj != qq ){ xm += x[jj]; ym += y[jj]; } }
     xm /= (n-1.0f) ; ym /= (n-1.0f) ;
   }
   for( jj=0 ; jj < n ; jj++ ){
     if( jj != qq ){
       vv = x[jj]-xm; ww = y[jj]-ym; xv += vv*vv; yv += ww*ww; xy += vv*ww;
     }
   }

   if( xv <= 0.0f || yv <= 0.0f ) return 0.0f ;
   return atanhf(xy/sqrtf(xv*yv)) ;
}

/*----------------------------------------------------------------------------*/
/* Bias-Corrected accelerated (BCa) boostrap estimation of correlation
   and confidence intervals: M Mudelsee, Mathematical Geology 35:651-665.
*//*--------------------------------------------------------------------------*/

float THD_bstrap_corr( int npt, float *xx, float *yy, float tau, int nboot )
{
   int kb , ii , qq ;
   float rxy , pboot , *rboot , *xb , *yb ; int qboot ;
   float rjbar , aa , anum,aden , val , *rjack ;
   float z0hat , phab ;

ENTRY("THD_bstrap_corr") ;

   if( npt < 19 || xx == NULL || yy == NULL ) RETURN(0.0f) ;

   if( nboot < 1000 ) nboot = 1000 ;

   rboot = (float *)malloc(sizeof(float)*nboot) ;
   xb    = (float *)malloc(sizeof(float)*npt) ;
   yb    = (float *)malloc(sizeof(float)*npt) ;

   rxy = acorrfun( npt , xx , yy ) ;  /* "raw" correlation */

INFO_message("rxy = %f",rxy) ;

   pboot = (tau <= 0.5f) ? 0.5f : 0.25f/tau ;  /* prob of random jump */
   qboot = (int)(pboot * ((1u << 31)-1u)) ;    /* scaled to int range */

   /** bootstrap work **/

   for( kb=0 ; kb < nboot ; kb++ ){
     qq = (lrand48() >> 3) % npt ;      /* random starting point */
     xb[0] = xx[qq] ; yb[0] = yy[qq] ;
     for( ii=1 ; ii < npt ; ii++ ){
       if( lrand48() < qboot ) qq = (lrand48() >> 3) % npt ; /* random jump */
       else                    qq = (qq+1) % npt ;           /* next point */
       xb[ii] = xx[qq] ; yb[ii] = yy[qq] ;
     }
     rboot[kb] = acorrfun( npt , xb , yb ) ;
   }
   free(yb) ; free(xb) ;

   /* find location of rxy in the sorted rboot array */

   qsort_float( nboot , rboot ) ;
   for( kb=0 ; kb < nboot && rboot[kb] < rxy ; kb++ ) ; /*nada*/
   z0hat = PHINV(kb/(double)nboot) ;  /* kb = number of rboot < rxy */

ININFO_message(" z0hat = %f",z0hat) ;

   /** jackknife work to get "acceleration" parameter aa **/

   rjack = (float *)malloc(sizeof(float)*npt) ;
   rjbar = 0.0f ;
   for( kb=0 ; kb < npt ; kb++ ){
     rjack[kb] = acorrfun_jack( npt, xx,yy , kb ) ; rjbar += rjack[kb] ;
   }
   rjbar /= npt ;
   anum = aden = 0.0f ;
   for( kb=0 ; kb < npt ; kb++ ){
     val = rjbar - rjack[kb] ; anum += val*val*val ; aden += val*val ;
   }
   aa = anum / (6.0f*aden*sqrtf(aden)) ;
   free(rjack) ;

ININFO_message(" aa = %f",aa) ;

   /* convert rboot into alpha values */

   for( kb=0 ; kb < nboot ; kb++ ){
     phab = PHINV( (kb+0.5f)/nboot ) - z0hat ;
     val  = phab / ( 1.0f + aa*phab ) - z0hat ;
     if( val < -5.0f ) val = -5.0f ; else if ( val > 5.0f ) val = 5.0f ;
     phab = PHI(val) ;
     printf("%f %f\n",rboot[kb],val) ;
   }

   /***/

   free(rboot) ;
   RETURN(0.0f) ;
}

int main( int argc , char *argv[] )
{
   int iarg=1 ;
   MRI_IMAGE *aim , *bim ;
   float *aar , *bar ;

   if( argc < 3 ) ERROR_exit("need 2 file args") ;

   if( AFNI_yesenv("NO_DEMEAN") ) acorr_demean = 0 ;
   SET_RAND_SEED ;

   aim = mri_read_1D(argv[1]) ; if( aim == NULL ) ERROR_exit("Can't read aim") ;
   bim = mri_read_1D(argv[2]) ; if( bim == NULL ) ERROR_exit("Can't read bim") ;
   if( aim->nx != aim->nx ) ERROR_exit("nx not same") ;

   (void)THD_bstrap_corr( aim->nx, MRI_FLOAT_PTR(aim), MRI_FLOAT_PTR(bim), 1.0f,0 ) ;
   exit(0) ;
}
