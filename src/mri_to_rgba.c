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

      case MRI_byte:{ byte *qar = MRI_BYTE_PTR(oldim) ;
        for( ii=0 ; ii < npix ; ii++ ){
          oar[ii].r = oar[ii].g = oar[ii].b = qar[ii] ;
          oar[ii].a = 255 ;
        }
      } break ;

      case MRI_float:{ float *qar = MRI_FLOAT_PTR(oldim) ;
        for( ii=0 ; ii < npix ; ii++ ){
          oar[ii].r = oar[ii].g = oar[ii].b = qar[ii] ;
          oar[ii].a = 255 ;
        }
      } break ;

      case MRI_short:{ short *qar = MRI_SHORT_PTR(oldim) ;
        for( ii=0 ; ii < npix ; ii++ ){
          oar[ii].r = oar[ii].g = oar[ii].b = qar[ii] ;
          oar[ii].a = 255 ;
        }
      } break ;

      case MRI_rgb:{ byte *qar = MRI_RGB_PTR(oldim) ;
        for( ii=0 ; ii < npix ; ii++ ){
          oar[ii].r = qar[3*ii] ;
          oar[ii].g = qar[3*ii+1] ;
          oar[ii].b = qar[3*ii+2] ;
          oar[ii].a = 255 ;   /* maximum opacity */
        }
      } break ;

      case MRI_rgba:{ rgba *qar = MRI_RGBA_PTR(oldim) ;
        memcpy( oar , qar , sizeof(rgba)*npix ) ;
      } break ;

      default:
         fprintf(stderr,"mri_to_rgb:  unrecognized image conversion %d\n",oldim->kind) ;
         RETURN( NULL );
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}
