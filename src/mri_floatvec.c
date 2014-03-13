#include "mrilib.h"

   /* cubic interpolation polynomials */

#undef  P_M1
#undef  P_00
#undef  P_P1
#undef  P_P2

#define P_M1(x)  ((x)*(1.0-(x))*((x)-2.0)*0.1666667)
#define P_00(x)  (((x)+1.0)*((x)-1.0)*((x)-2.0)*0.5)
#define P_P1(x)  ((x)*((x)+1.0)*(2.0-(x))*0.5)
#define P_P2(x)  ((x)*((x)+1.0)*((x)-1.0)*0.1666667)

/*------------------------------------*/
/*! Cubic interpolation in a floatvec */

float interp_floatvec( floatvec *fv , float x )
{
   int ix , im1,ip1,ip2 , itop ;
   float fx , val , abot,atop ;

   if( fv == NULL || fv->ar == NULL ) return 0.0f ;
   itop = fv->nar - 1 ;

   if( itop <= 1 || fv->dx == 0.0 ) return(fv->ar[0]) ;

   /* if input x is out of range, return the edge value */

   fx = (x - fv->x0) / fv->dx ;
        if( fx <= 0.0f ) return(fv->ar[0]) ;
   else if( fx >= itop ) return(fv->ar[itop]) ;

   /* input x is between point #ix and #ix+1 */
   /* fractional offset between them is fx  */

   ix = (int)fx ; fx = fx - ix ;

   /* get indexes below (im1) and above (ip1 and ip2) */

   im1 = ix-1 ; if( im1 < 0 ) im1 = 0 ;
   ip1 = ix+1 ;
   if( ip1 > itop ){
     ip1 = ip2 = itop ;
   } else {
     ip2 = ip1+1 ; if( ip2 > itop ) ip2 = itop ;
   }

   /* cubic interpolation between these 4 points */

   val =  P_M1(fx)*fv->ar[im1] + P_00(fx)*fv->ar[ix]
        + P_P1(fx)*fv->ar[ip1] + P_P2(fx)*fv->ar[ip2]  ;

   /* make sure result lies in the local range of values */

   abot = fv->ar[ix] ; atop = fv->ar[ip1] ;
   if( abot > atop ){ fx = abot; abot = atop; atop = fx; }

   if( val < abot ) val = abot; else if( val > atop ) val = atop;

   return(val) ;
}

/*----------------------------------------------------------*/

static float regula_falsi_step( floatvec *fv, float y, float x0, float x1 )
{
   float y0 , y1 , dy ;

   y0 = interp_floatvec(fv,x0) ;
   y1 = interp_floatvec(fv,x1) ; dy = y1-y0 ;
   if( dy == 0.0f || fabsf(dy) < 0.00666f*(fabsf(y-y0)+fabsf(y-y1)) ) return x0;

   dy = x0 + (x1-x0)/dy * (y-y0) ; return dy ;
}

/*----------------------------------------------------------*/
/* Inverse interpolation in a floatvec (assumed monotonic). */

#undef DO_FIVE

float interp_inverse_floatvec( floatvec *fv , float y )
{
   int ip,itop ; float ym,yp,dx , x0,x1,x2 , xm,xp,y0,y1,y2 , xx[5],yy[5] ;

   /* check for stoopid inputs */

   if( fv == NULL ) return 0.0f ;
   itop = fv->nar - 1 ;
   if( fv->ar == NULL || itop <= 1 || fv->dx == 0.0 )      return(fv->x0) ;

   /* off the left edge? */

   if( (fv->ar[0] < fv->ar[itop] && y <= fv->ar[0]) ||
       (fv->ar[0] > fv->ar[itop] && y >= fv->ar[0])   )    return(fv->x0) ;

   /* off the right edge? */

   if( (fv->ar[0] < fv->ar[itop] && y >= fv->ar[itop]) ||
       (fv->ar[0] > fv->ar[itop] && y <= fv->ar[itop])   ) return(fv->x0+fv->dx*itop) ;

   /* find the intermediate point that brackets the desired result */
   /* [27 Feb 2014] -- replace simple linear interpolation with
      linear interpolation plus a regula falsi step for improvement
      (since the forward interpolation method is cubic, not linear). */

   for( ip=1 ; ip <= itop ; ip++ ){
     ym = fv->ar[ip-1] ; yp = fv->ar[ip] ;
     if( (y-ym) * (y-yp) <= 0.0f ){    /* the desired y is now bracketed */
       dx = (y-ym) / (yp-ym) ;
#if 0
       return( fv->x0 + fv->dx *(ip-1.0+dx) ) ;  /* old way = linear interp */
#else
                                    /* x0 = linear interp result */
       x0 = fv->x0 + fv->dx *(ip-1.0+dx) ;  y0 = interp_floatvec(fv,x0) ;
       x1 = x0 + 0.05f * fv->dx ;   /* try nearby points above and below */
       x2 = x0 - 0.05f * fv->dx ;   /* and then regula falsi from them  */
       xp = regula_falsi_step(fv,y,x0,x1) ; yp = interp_floatvec(fv,xp) ;
       xm = regula_falsi_step(fv,y,x0,x2) ; ym = interp_floatvec(fv,xm) ;
#ifdef DO_FIVE  /* extra regula falsi steps (not needed IMHO) */
       x1 = regula_falsi_step(fv,y,x0,xp) ; y1 = interp_floatvec(fv,x1) ;
       x2 = regula_falsi_step(fv,y,x0,xm) ; y2 = interp_floatvec(fv,x2) ;
       yp = fabsf(yp-y) ; ym = fabsf(ym-y) ; y0 = fabsf(y0-y) ;
       y1 = fabsf(y1-y) ; y2 = fabsf(y2-y) ;
       xx[0] = x0 ; xx[1] = x1 ; xx[2] = x2 ; xx[3] = xm ; xx[4] = xp ;
       yy[0] = y0 ; yy[1] = y1 ; yy[2] = y2 ; yy[3] = ym ; yy[4] = yp ;
       qsort_floatfloat(5,yy,xx) ;
#else /* not DO_FIVE */
       yp = fabsf(yp-y) ; ym = fabsf(ym-y) ; y0 = fabsf(y0-y) ;
       xx[0] = x0 ; xx[1] = xm ; xx[2] = xp ;  /* candidate x values */
       yy[0] = y0 ; yy[1] = ym ; yy[2] = yp ;  /* residuals at them */
       qsort_floatfloat(3,yy,xx) ;             /* find smallest residual */
#endif /* DO_FIVE */
       return xx[0] ;              /* x value with the smallest residual */
#endif
     }
   }

   /* should never happen */

   return( fv->x0 + fv->dx * 0.5*itop ) ;  /* the midpoint */
}
