#include "mrilib.h"

/*! Scale a float image to an integer type [20 Oct 2003]:
     - inim   = MRI_float image
     - kind   = integer type to scale to (MRI_byte, MRI_short, MRI_int)
     - return = scaled image
     - *sfac  = scale factor (nonzero on input ==> smallest value allowed)
*/

MRI_IMAGE * mri_scalize( MRI_IMAGE *inim , int kind , float *sfac )
{
   float gtop , fac , fimfac ;
   MRI_IMAGE *outim ;

ENTRY("mri_scalize") ;

   if( inim == NULL            ||
       inim->kind != MRI_float ||
      sfac == NULL             ||
      !MRI_IS_INT_TYPE(kind)     ) RETURN(NULL) ;

   fac = *sfac ; if( fac < 0.0 ) fac = 0.0 ;

   gtop = MCW_vol_amax( inim->nvox,1,1,MRI_float,MRI_FLOAT_PTR(inim) ) ;
   if( gtop == 0.0 ){
     fimfac = fac ;
   } else {
     fimfac = gtop / MRI_TYPE_maxval[kind] ;
     if( fimfac < fac ) fimfac = fac ;
   }
   outim = mri_new_conforming( inim , kind ) ;
   if( fimfac > 0.0 )
     EDIT_coerce_scale_type( inim->nvox , 1.0/fimfac ,
                             MRI_float , MRI_FLOAT_PTR(inim) ,
                             outim->kind , mri_data_pointer(outim) ) ;
   *sfac = fimfac ; RETURN(outim) ;
}
