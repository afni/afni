#include "mrilib.h"

/*----------------------------------------------------------------------------*/
/*! Pearson correlation of x[] and y[] */

static INLINE float corrfun( int n, float *x , float *y )
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
   return xy/sqrtf(xv*yv) ;
}

/*----------------------------------------------------------------------------*/

float THD_bstrap_corr( int npt, float *xx, float *yy, float tau, int nboot )
{
   int kb , ii , qq ;
   float rxy , pboot , *rboot , *xb , *yb ; int qboot ;

ENTRY("THD_bstrap_corr") ;

   if( npt < 9 || xx == NULL || yy == NULL ) RETURN(0.0f) ;

   if( nboot < 199 ) nboot = 199 ;

   rboot = (float *)malloc(sizeof(float)*nboot) ;
   xb    = (float *)malloc(sizeof(float)*npt) ;
   yb    = (float *)malloc(sizeof(float)*npt) ;

   rxy = corrfun( npt , xx , yy ) ;  /* "raw" correlation */

   pboot = (tau <= 0.5f) ? 0.5f : 0.25f/tau ;  /* prob of random jump */
   qboot = (int)(pboot * ((1 << 31)-1)) ;      /* scaled to int range */

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
   qsort_float( nboot , rboot ) ;

   /***/

   free(yb); free(xb); free(rboot);
   RETURN(0.0f) ;
}
