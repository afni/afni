/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*** 7D SAFE [some of it] ***/

MRI_IMAGE *mri_to_rgb( MRI_IMAGE *oldim )  /* 11 Feb 1999 */
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register byte * rgb ;

WHOAMI ; IMHEADER(oldim) ;

   if( oldim == NULL ) return NULL ;

   newim = mri_new_conforming( oldim , MRI_rgb ) ; rgb = MRI_RGB_PTR(newim) ;
   npix  = oldim->nvox ;

   switch( oldim->kind ){

      case MRI_byte:
         for( ii=0 ; ii < npix ; ii++ )
            rgb[3*ii] = rgb[3*ii+1] = rgb[3*ii+2] = oldim->im.byte_data[ii] ;
      break ;

      case MRI_float:
         for( ii=0 ; ii < npix ; ii++ )
            rgb[3*ii] = rgb[3*ii+1] = rgb[3*ii+2] = oldim->im.float_data[ii] ;
      break ;

      case MRI_short:
         for( ii=0 ; ii < npix ; ii++ )
            rgb[3*ii] = rgb[3*ii+1] = rgb[3*ii+2] = oldim->im.short_data[ii] ;
      break ;

      case MRI_rgb:
         memcpy( rgb , MRI_RGB_PTR(oldim) , 3*npix ) ;
      break ;

      case MRI_rgba:
         for( ii=0 ; ii < npix ; ii++ ){
            rgb[3*ii]   = oldim->im.rgba_data[ii].r ;
            rgb[3*ii+1] = oldim->im.rgba_data[ii].g ;
            rgb[3*ii+2] = oldim->im.rgba_data[ii].b ;
         }
      break ;

      default:
         fprintf(stderr,"mri_to_rgb:  unrecognized image conversion %d\n",oldim->kind) ;
         return NULL ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   return newim ;
}

/*---------------------------------------------------------------------------
   Inputs must be in same formats (# voxels and kinds)!
-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_3to_rgb( MRI_IMAGE * rim , MRI_IMAGE * gim , MRI_IMAGE * bim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register byte * rgb ;

   if( rim == NULL || bim == NULL || gim == NULL ) return NULL ;

   newim = mri_new_conforming( rim , MRI_rgb ) ; rgb = MRI_BYTE_PTR(newim) ;
   npix  = rim->nvox ;

   switch( rim->kind ){

      case MRI_byte:{
         byte * rr=MRI_BYTE_PTR(rim), * gg=MRI_BYTE_PTR(gim), * bb=MRI_BYTE_PTR(bim) ;
         for( ii=0 ; ii < npix ; ii++ ){
            rgb[3*ii  ] = rr[ii] ;
            rgb[3*ii+1] = gg[ii] ;
            rgb[3*ii+2] = bb[ii] ;
         }
      }
      break ;

      case MRI_float:{
         float * rr=MRI_FLOAT_PTR(rim), * gg=MRI_FLOAT_PTR(gim), * bb=MRI_FLOAT_PTR(bim) ;
         for( ii=0 ; ii < npix ; ii++ ){
            rgb[3*ii  ] = rr[ii] ;
            rgb[3*ii+1] = gg[ii] ;
            rgb[3*ii+2] = bb[ii] ;
         }
      }
      break ;

      default:
         fprintf(stderr,"mri_3to_rgb:  unrecognized image conversion %d\n",rim->kind) ;
         MRI_FATAL_ERROR ;
   }

   MRI_COPY_AUX(newim,rim) ;
   return newim ;
}

/*-------------------------------------------------------------------------------*/

MRI_IMARR * mri_rgb_to_3float( MRI_IMAGE * oldim )
{
   MRI_IMARR * imar ;
   MRI_IMAGE * rim , * gim , * bim ;
   float     * rr  , * gg  , * bb  ;
   byte      * rgb ;
   int ii , npix ;

   if( oldim == NULL || oldim->kind != MRI_rgb ) return NULL ;

   rim = mri_new_conforming( oldim , MRI_float ) ; rr = MRI_FLOAT_PTR(rim) ;
   gim = mri_new_conforming( oldim , MRI_float ) ; gg = MRI_FLOAT_PTR(gim) ;
   bim = mri_new_conforming( oldim , MRI_float ) ; bb = MRI_FLOAT_PTR(bim) ;
                                                   rgb= MRI_BYTE_PTR(oldim) ;
   npix = oldim->nvox ;

   for( ii=0 ; ii < npix ; ii++ ){
      rr[ii] = rgb[3*ii  ] ;
      gg[ii] = rgb[3*ii+1] ;
      bb[ii] = rgb[3*ii+2] ;
   }

   INIT_IMARR(imar) ;
   ADDTO_IMARR(imar,rim) ; ADDTO_IMARR(imar,gim) ; ADDTO_IMARR(imar,bim) ;

   return imar ;
}

/*-------------------------------------------------------------------------------*/

MRI_IMARR * mri_rgb_to_3byte( MRI_IMAGE * oldim )  /* 15 Apr 1999 */
{
   MRI_IMARR * imar ;
   MRI_IMAGE * rim , * gim , * bim ;
   byte      * rr  , * gg  , * bb  , * rgb ;
   int ii , npix ;

   if( oldim == NULL || oldim->kind != MRI_rgb ) return NULL ;

   rim = mri_new_conforming( oldim , MRI_byte ) ; rr = MRI_BYTE_PTR(rim) ;
   gim = mri_new_conforming( oldim , MRI_byte ) ; gg = MRI_BYTE_PTR(gim) ;
   bim = mri_new_conforming( oldim , MRI_byte ) ; bb = MRI_BYTE_PTR(bim) ;
                                                  rgb= MRI_BYTE_PTR(oldim) ;
   npix = oldim->nvox ;

   for( ii=0 ; ii < npix ; ii++ ){
      rr[ii] = rgb[3*ii  ] ;
      gg[ii] = rgb[3*ii+1] ;
      bb[ii] = rgb[3*ii+2] ;
   }

   INIT_IMARR(imar) ;
   ADDTO_IMARR(imar,rim) ; ADDTO_IMARR(imar,gim) ; ADDTO_IMARR(imar,bim) ;

   return imar ;
}

/*-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_sharpen_rgb( float phi , MRI_IMAGE * im )
{
   MRI_IMAGE * flim , * shim , * newim ;
   byte  * iar , * nar ;
   float * sar , * far ;
   int ii , nvox , rr,gg,bb ;
   float fac ;

   if( im == NULL ) return NULL ;

   if( im->kind != MRI_rgb ) return mri_sharpen( phi , 0 , im ) ;

   flim  = mri_to_float( im ) ;                  /* intensity of input */
   shim  = mri_sharpen( phi , 0 , flim ) ;       /* sharpen intensity */
   newim = mri_new_conforming( im , MRI_rgb ) ;  /* will be output    */

   nar = MRI_BYTE_PTR(newim) ; iar = MRI_BYTE_PTR(im) ;
   far = MRI_FLOAT_PTR(flim) ; sar = MRI_FLOAT_PTR(shim) ;

   nvox = newim->nvox ;
   for( ii=0 ; ii < nvox ; ii++ ){
      if( far[ii] <= 0.0 || sar[ii] <= 0.0 ){
         nar[3*ii] = nar[3*ii+1] = nar[3*ii+2] = 0 ;
      } else {
         fac = sar[ii] / far[ii] ; /* will be positive */
         rr  = fac * iar[3*ii]   ;
         gg  = fac * iar[3*ii+1] ;
         bb  = fac * iar[3*ii+2] ;
         nar[3*ii  ] = (rr > 255) ? 255 : rr ;
         nar[3*ii+1] = (gg > 255) ? 255 : gg ;
         nar[3*ii+2] = (bb > 255) ? 255 : bb ;
      }
   }

   mri_free(flim) ; mri_free(shim) ;

   MRI_COPY_AUX(newim,im) ;
   return newim ;
}

/*-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_flatten_rgb( MRI_IMAGE * im )
{
   MRI_IMAGE * flim , * shim , * newim ;
   byte  * iar , * nar ;
   float * sar , * far ;
   int ii , nvox , rr,gg,bb ;
   float fac ;

   if( im == NULL ) return NULL ;

   if( im->kind != MRI_rgb ) return mri_flatten( im ) ;

   flim  = mri_to_float( im ) ;                  /* intensity of input */
   shim  = mri_flatten( flim ) ;                 /* flatten intensity  */
   newim = mri_new_conforming( im , MRI_rgb ) ;  /* will be output     */

   nar = MRI_BYTE_PTR(newim) ; iar = MRI_BYTE_PTR(im) ;
   far = MRI_FLOAT_PTR(flim) ; sar = MRI_FLOAT_PTR(shim) ;
   nvox = newim->nvox ;

   for( ii=0 ; ii < nvox ; ii++ ){
      if( far[ii] <= 0.0 || sar[ii] <= 0.0 ){
         nar[3*ii] = nar[3*ii+1] = nar[3*ii+2] = 0 ;
      } else {
         fac = 255.9 * sar[ii] / far[ii] ; /* will be positive */
         rr  = fac * iar[3*ii]   ;
         gg  = fac * iar[3*ii+1] ;
         bb  = fac * iar[3*ii+2] ;
         nar[3*ii  ] = (rr > 255) ? 255 : rr ;
         nar[3*ii+1] = (gg > 255) ? 255 : gg ;
         nar[3*ii+2] = (bb > 255) ? 255 : bb ;
      }
   }

   mri_free(flim) ; mri_free(shim) ;

   MRI_COPY_AUX(newim,im) ;
   return newim ;
}

/*-----------------------------------------------------------------------------*/

void mri_invert_inplace( MRI_IMAGE *im )
{
   register byte *bar ;
   register int ii , nbar ;

   if( im == NULL ) return ;
   switch( im->kind ){
     default: return ;
     case MRI_byte:  nbar =   im->nvox ; bar = MRI_BYTE_PTR(im) ; break ;
     case MRI_rgb:   nbar = 3*im->nvox ; bar = MRI_RGB_PTR(im)  ; break ;
   }
   for( ii=0 ; ii < nbar ; ii++ ) bar[ii] = 255 - bar[ii] ;
   return ;
}
