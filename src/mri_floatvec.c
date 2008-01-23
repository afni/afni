#include "mrilib.h"

   /* cubic interpolation polynomials */

#define P_M1(x)  ((x)*(1.0-(x))*((x)-2.0)*0.1666667)
#define P_00(x)  (((x)+1.0)*((x)-1.0)*((x)-2.0)*0.5)
#define P_P1(x)  ((x)*((x)+1.0)*(2.0-(x))*0.5)
#define P_P2(x)  ((x)*((x)+1.0)*((x)-1.0)*0.1666667)

float interp_floatvector( floatvec *fv , float x )
{
   int ix , im1,ip1,ip2 , itop ;
   register float fx ;

   if( fv == NULL || fv->ar == NULL ) return 0.0f ;
   itop = fv->nar - 1 ;

   fx = (x - fv->x0) / fv->dx ;
        if( fx <= 0.0f ) return(fv->ar[0]) ;
   else if( fx >= itop ) return(fv->ar[itop]) ;

   ix = (int)fx ; fx = fx - ix ;

   im1 = ix-1 ; if( im1 < 0 ) im1 = 0 ;
   ip1 = ix+1 ;
   if( ip1 > itop ){
     ip1 = ip2 = itop ;
   } else {
     ip2 = ip1+1 ; if( ip2 > itop ) ip2 = itop ;
   }

   return( P_M1(fx)*fv->ar[im1]
          +P_00(fx)*fv->ar[ix]
          +P_P1(fx)*fv->ar[ip1]
          +P_P2(fx)*fv->ar[ip2] ) ;
}
