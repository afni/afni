#include "mrilib.h"

/*-------------------------------------------------------------------
  Scale an image in place - 27 Nov 2001
---------------------------------------------------------------------*/

void mri_scale_inplace( float fac , MRI_IMAGE *im )
{
   register int ii , nvox ;
   void *vp ;

   if( im == NULL || fac == 1.0 || fac == 0.0 ) return ;
   vp = mri_data_pointer( im ) ; if( vp == NULL ) return ;
   nvox = im->nvox ;

   switch( im->kind ){

      case MRI_byte:{
         byte *pp = (byte *) vp ;
         for( ii=0 ; ii < nvox ; ii++ ) pp[ii] *= fac ;
      }
      break ;

      case MRI_short:{
         short *pp = (short *) vp ;
         for( ii=0 ; ii < nvox ; ii++ ) pp[ii] *= fac ;
      }
      break ;

      case MRI_float:{
         float *pp = (float *) vp ;
         for( ii=0 ; ii < nvox ; ii++ ) pp[ii] *= fac ;
      }
      break ;

      case MRI_int:{
         int *pp = (int *) vp ;
         for( ii=0 ; ii < nvox ; ii++ ) pp[ii] *= fac ;
      }
      break ;

      case MRI_double:{
         double *pp = (double *) vp ;
         for( ii=0 ; ii < nvox ; ii++ ) pp[ii] *= fac ;
      }
      break ;

      case MRI_complex:{
         complex *pp = (complex  *) vp ;
         for( ii=0 ; ii < nvox ; ii++ ){
           pp[ii].r *= fac; pp[ii].i *= fac;
         }
      }
      break ;

      case MRI_rgb:{
         byte *pp = (byte *) vp ;
         nvox *= 3 ;
         for( ii=0 ; ii < nvox ; ii++ ) pp[ii] *= fac ;
      }
      break ;
   }

   return ;
}
