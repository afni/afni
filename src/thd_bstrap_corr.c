#include "mrilib.h"

/*----------------------------------------------------------------------------*/
/*! Pearson correlation of x[] and y[] */

static INLINE float corrfun( int n, float *x, float *y )
{
   float xv=0.0f , yv=0.0f , xy=0.0f , vv,ww ;
   float xm=0.0f , ym=0.0f ;
   int jj ;

   for( jj=0 ; jj < n ; jj++ ){ xm += x[jj] ; ym += y[jj] ; }
   xm /= n ; ym /= n ;
   for( jj=0 ; jj < n ; jj++ ){
     vv = x[jj]-xm ; ww = y[jj]-ym ;
     xv += vv*vv ; yv += ww*ww ; xy += vv*ww ;
   }

   if( xv <= 0.0f || yv <= 0.0f ) return 0.0f ;
   return atanhf(xy/sqrtf(xv*yv)) ;
}

static INLINE float corrfun_jack( int n, float *x, float *y, int qq )
{
   float xv=0.0f , yv=0.0f , xy=0.0f , vv,ww ;
   float xm=0.0f , ym=0.0f ;
   int jj ;

   for( jj=0 ; jj < n ; jj++ ){
     if( jj != qq ){ xm += x[jj] ; ym += y[jj] ; }
   }
   xm /= (n-1) ; ym /= (n-1) ;
   for( jj=0 ; jj < n ; jj++ ){
     if( jj != qq ){
       vv = x[jj]-xm ; ww = y[jj]-ym ;
       xv += vv*vv ; yv += ww*ww ; xy += vv*ww ;
     }
   }

   if( xv <= 0.0f || yv <= 0.0f ) return 0.0f ;
   return atanhf(xy/sqrtf(xv*yv)) ;
}

/*----------------------------------------------------------------------------*/

float THD_bstrap_corr( int npt, float *xx, float *yy, float tau, int nboot )
{
   int kb , ii , qq ;
   float rxy , pboot , *rboot , *xb , *yb ; int qboot ;
   float rjbar , aa , anum,aden , val , *rjack ;
   float z0hat ;

ENTRY("THD_bstrap_corr") ;

   if( npt < 9 || xx == NULL || yy == NULL ) RETURN(0.0f) ;

   if( nboot < 199 ) nboot = 199 ;

   rboot = (float *)malloc(sizeof(float)*nboot) ;
   xb    = (float *)malloc(sizeof(float)*npt) ;
   yb    = (float *)malloc(sizeof(float)*npt) ;

   rxy = corrfun( npt , xx , yy ) ;  /* "raw" correlation */

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
     rboot[kb] = corrfun( npt , xb , yb ) ;
   }
   free(yb) ; free(xb) ;
   qsort_float( nboot , rboot ) ;
   for( kb=0 ; kb < nboot && rboot[kb] < rxy ; kb++ ) ; /*nada*/
   z0hat = (float)qginv(1.0-kb/(double)nboot) ;

   /** jackknife work **/

   rjack = (float *)malloc(sizeof(float)*npt) ;
   rjbar = 0.0f ;
   for( kb=0 ; kb < npt ; kb++ ){
     rjack[kb] = corrfun_jack( npt, xx,yy , kb ) ; rjbar += rjack[kb] ;
   }
   rjbar /= npt ;
   anum = aden = 0.0f ;
   for( kb=0 ; kb < npt ; kb++ ){
     val = rjbar - rjack[kb] ; anum += val*val*val ; aden += val*val ;
   }
   aa = anum / (6.0f*aden*sqrtf(aden)) ;
   free(rjack) ;

   /***/

   free(rboot) ;
   RETURN(0.0f) ;
}
