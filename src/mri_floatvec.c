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
