#include "mrilib.h"

/*** 7D SAFE [some of it] ***/

MRI_IMAGE *mri_to_rgba( MRI_IMAGE *oldim )  /* 11 Feb 1999 */
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register rgba * oar ;

ENTRY("mri_to_rgba") ;

   if( oldim == NULL ) RETURN( NULL );

   newim = mri_new_conforming( oldim , MRI_rgba ) ; oar = MRI_RGBA_PTR(newim) ;
   npix  = oldim->nvox ;

   switch( oldim->kind ){

      case MRI_byte:
        for( ii=0 ; ii < npix ; ii++ ){
          oar[ii].r = oar[ii].g = oar[ii].b = oldim->im.byte_data[ii] ;
          oar[ii].a = 255 ;
        }
      break ;

      case MRI_float:
        for( ii=0 ; ii < npix ; ii++ ){
          oar[ii].r = oar[ii].g = oar[ii].b = oldim->im.float_data[ii] ;
          oar[ii].a = 255 ;
        }
      break ;

      case MRI_short:
        for( ii=0 ; ii < npix ; ii++ ){
          oar[ii].r = oar[ii].g = oar[ii].b = oldim->im.short_data[ii] ;
          oar[ii].a = 255 ;
        }
      break ;

      case MRI_rgb:
        for( ii=0 ; ii < npix ; ii++ ){
          oar[ii].r = oldim->im.rgb_data[3*ii] ;
          oar[ii].g = oldim->im.rgb_data[3*ii+1] ;
          oar[ii].b = oldim->im.rgb_data[3*ii+2] ;
          oar[ii].a = 255 ;
        }
      break ;

      case MRI_rgba:
        memcpy( oar , oldim->im.rgba_data , sizeof(rgba)*npix ) ;
      break ;

      default:
         fprintf(stderr,"mri_to_rgb:  unrecognized image conversion %d\n",oldim->kind) ;
         RETURN( NULL );
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}
