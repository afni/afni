#include "mrilib.h"

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

/*---------------------------------------------------------------------------
   Inputs must be in same formats (# voxels and kinds - byte or float)!
-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_4to_rgba( MRI_IMAGE *rim , MRI_IMAGE *gim , MRI_IMAGE *bim , MRI_IMAGE *aim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register rgba * pval ;

ENTRY("mri_4to_rgba") ;

   if( rim == NULL || bim == NULL || gim == NULL || aim == NULL ) RETURN( NULL );

   newim = mri_new_conforming( rim , MRI_rgba ) ; pval = MRI_RGBA_PTR(newim) ;
   npix  = rim->nvox ;

   switch( rim->kind ){

      case MRI_byte:{
        byte *rr=MRI_BYTE_PTR(rim), *gg=MRI_BYTE_PTR(gim), *bb=MRI_BYTE_PTR(bim), *aa=MRI_BYTE_PTR(aim);
        for( ii=0 ; ii < npix ; ii++ ){
          pval[ii].r = rr[ii] ;
          pval[ii].g = gg[ii] ;
          pval[ii].b = bb[ii] ;
          pval[ii].a = aa[ii] ;
        }
      }
      break ;

      case MRI_float:{
        float *rr=MRI_FLOAT_PTR(rim), *gg=MRI_FLOAT_PTR(gim), *bb=MRI_FLOAT_PTR(bim), *aa=MRI_FLOAT_PTR(aim);
        for( ii=0 ; ii < npix ; ii++ ){
          pval[ii].r = BYTEIZE(rr[ii]) ;
          pval[ii].g = BYTEIZE(gg[ii]) ;
          pval[ii].b = BYTEIZE(bb[ii]) ;
          pval[ii].a = BYTEIZE(aa[ii]) ;
        }
      }
      break ;

      default:
        ERROR_message("mri_3to_rgb: unrecognized image conversion %d",rim->kind) ;
        mri_free(newim) ; RETURN(NULL) ;
   }

   MRI_COPY_AUX(newim,rim) ;
   RETURN( newim );
}

/*-------------------------------------------------------------------------------*/

MRI_IMARR * mri_rgba_to_4float( MRI_IMAGE *oldim )
{
   MRI_IMARR *imar ;
   MRI_IMAGE *rim , *gim , *bim , *aim ;
   float     *rr  , *gg  , *bb  , *aa;
   rgba      *pval ;
   int ii , npix ;

ENTRY("mri_rgba_to_4float") ;

   if( oldim == NULL || oldim->kind != MRI_rgba ) RETURN( NULL );

   rim = mri_new_conforming( oldim , MRI_float ) ; rr = MRI_FLOAT_PTR(rim) ;
   gim = mri_new_conforming( oldim , MRI_float ) ; gg = MRI_FLOAT_PTR(gim) ;
   bim = mri_new_conforming( oldim , MRI_float ) ; bb = MRI_FLOAT_PTR(bim) ;
   aim = mri_new_conforming( oldim , MRI_float ) ; aa = MRI_FLOAT_PTR(aim) ;
                                                  pval= MRI_RGBA_PTR(oldim);
   npix = oldim->nvox ;

   for( ii=0 ; ii < npix ; ii++ ){
     rr[ii] = (float)pval[ii].r ;
     gg[ii] = (float)pval[ii].g ;
     bb[ii] = (float)pval[ii].b ;
     aa[ii] = (float)pval[ii].a ;
   }

   INIT_IMARR(imar) ;
   ADDTO_IMARR(imar,rim) ; ADDTO_IMARR(imar,gim) ; ADDTO_IMARR(imar,bim) ;

   RETURN( imar );
}

/*-------------------------------------------------------------------------------*/

MRI_IMARR * mri_rgba_to_4byte( MRI_IMAGE *oldim )
{
   MRI_IMARR *imar ;
   MRI_IMAGE *rim , *gim , *bim , *aim ;
   byte      *rr  , *gg  , *bb  , *aa ;
   rgba *pval ;
   int ii , npix ;

ENTRY("mri_rgba_to_4byte") ;
   if( oldim == NULL || oldim->kind != MRI_rgba ) RETURN( NULL );

   rim = mri_new_conforming( oldim , MRI_byte ) ; rr = MRI_BYTE_PTR(rim) ;
   gim = mri_new_conforming( oldim , MRI_byte ) ; gg = MRI_BYTE_PTR(gim) ;
   bim = mri_new_conforming( oldim , MRI_byte ) ; bb = MRI_BYTE_PTR(bim) ;
   aim = mri_new_conforming( oldim , MRI_byte ) ; aa = MRI_BYTE_PTR(aim) ;
                                                 pval= MRI_RGBA_PTR(oldim);
   npix = oldim->nvox ;

   for( ii=0 ; ii < npix ; ii++ ){
     rr[ii] = pval[ii].r ;
     gg[ii] = pval[ii].g ;
     bb[ii] = pval[ii].b ;
     aa[ii] = pval[ii].a ;
   }

   INIT_IMARR(imar) ;
   ADDTO_IMARR(imar,rim) ; ADDTO_IMARR(imar,gim) ; ADDTO_IMARR(imar,bim) ;

   RETURN( imar );
}
