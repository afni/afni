#include "mrilib.h"

/*! Count number of values between bot and top (inclusive) */

int mri_counter( MRI_IMAGE *im , float bot , float top )
{
   register float *far ; register int ii,nv,cc ;
   if( im == NULL || im->kind != MRI_float || bot > top ) return -1 ;
   far = MRI_FLOAT_PTR(im) ; if( far == NULL ) return -1 ;
   nv  = im->nvox ;
   for( cc=ii=0 ; ii < nv ; ii++ ){
     if( far[ii] >= bot && far[ii] <= top ) cc++ ;
   }
   return cc ;
}
