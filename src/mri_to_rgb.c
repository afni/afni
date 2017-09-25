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

ENTRY("mri_to_rgb") ;

   if( oldim == NULL ) RETURN( NULL );

   newim = mri_new_conforming( oldim , MRI_rgb ) ; rgb = MRI_RGB_PTR(newim) ;
   npix  = oldim->nvox ;

   switch( oldim->kind ){

      case MRI_byte:{ byte *qar = MRI_BYTE_PTR(oldim) ;
        for( ii=0 ; ii < npix ; ii++ )
          rgb[3*ii] = rgb[3*ii+1] = rgb[3*ii+2] = qar[ii] ;
      } break ;

      case MRI_float:{ float *qar = MRI_FLOAT_PTR(oldim) ;
        for( ii=0 ; ii < npix ; ii++ )
          rgb[3*ii] = rgb[3*ii+1] = rgb[3*ii+2] = qar[ii] ;
      } break ;

      case MRI_short:{ short *qar = MRI_SHORT_PTR(oldim) ;
        for( ii=0 ; ii < npix ; ii++ )
          rgb[3*ii] = rgb[3*ii+1] = rgb[3*ii+2] = qar[ii] ;
      } break ;

      case MRI_rgb:
        memcpy( rgb , MRI_RGB_PTR(oldim) , 3*npix ) ;
      break ;

      case MRI_rgba:{ rgba *qar = MRI_RGBA_PTR(oldim) ;
        for( ii=0 ; ii < npix ; ii++ ){
          rgb[3*ii]   = qar[ii].r ;
          rgb[3*ii+1] = qar[ii].g ;
          rgb[3*ii+2] = qar[ii].b ;
        }
      } break ;

      default:
        ERROR_message("mri_to_rgb: unrecognized image conversion %d",oldim->kind) ;
        mri_free(newim) ; RETURN(NULL);
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}

/*---------------------------------------------------------------------------
   Inputs must be in same formats (# voxels and kinds)!
-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_3to_rgb( MRI_IMAGE *rim , MRI_IMAGE *gim , MRI_IMAGE *bim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;
   register byte * rgb ;

ENTRY("mri_3to_rgb") ;

   if( rim == NULL || bim == NULL || gim == NULL ) RETURN( NULL );

   newim = mri_new_conforming( rim , MRI_rgb ) ; rgb = MRI_BYTE_PTR(newim) ;
   npix  = rim->nvox ;

   switch( rim->kind ){

      case MRI_byte:{
        byte *rr=MRI_BYTE_PTR(rim), *gg=MRI_BYTE_PTR(gim), *bb=MRI_BYTE_PTR(bim) ;
        for( ii=0 ; ii < npix ; ii++ ){
          rgb[3*ii  ] = rr[ii] ;
          rgb[3*ii+1] = gg[ii] ;
          rgb[3*ii+2] = bb[ii] ;
        }
      }
      break ;

      case MRI_float:{
        float *rr=MRI_FLOAT_PTR(rim), *gg=MRI_FLOAT_PTR(gim), *bb=MRI_FLOAT_PTR(bim) ;
        for( ii=0 ; ii < npix ; ii++ ){
          rgb[3*ii  ] = rr[ii] ;
          rgb[3*ii+1] = gg[ii] ;
          rgb[3*ii+2] = bb[ii] ;
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

MRI_IMARR * mri_rgb_to_3float( MRI_IMAGE *oldim )
{
   MRI_IMARR *imar ;
   MRI_IMAGE *rim , *gim , *bim ;
   float     *rr  , *gg  , *bb  ;
   byte      *rgb ;
   int ii , npix ;

ENTRY("mri_rgb_to_3float") ;

   if( oldim == NULL || oldim->kind != MRI_rgb ) RETURN( NULL );

   rim = mri_new_conforming( oldim , MRI_float ) ; rr = MRI_FLOAT_PTR(rim) ;
   gim = mri_new_conforming( oldim , MRI_float ) ; gg = MRI_FLOAT_PTR(gim) ;
   bim = mri_new_conforming( oldim , MRI_float ) ; bb = MRI_FLOAT_PTR(bim) ;
                                                   rgb= MRI_BYTE_PTR(oldim);
   npix = oldim->nvox ;

   for( ii=0 ; ii < npix ; ii++ ){
     rr[ii] = (float)rgb[3*ii  ] ;
     gg[ii] = (float)rgb[3*ii+1] ;
     bb[ii] = (float)rgb[3*ii+2] ;
   }

   INIT_IMARR(imar) ;
   ADDTO_IMARR(imar,rim) ; ADDTO_IMARR(imar,gim) ; ADDTO_IMARR(imar,bim) ;

   RETURN( imar );
}

/*-------------------------------------------------------------------------------*/

MRI_IMARR * mri_rgb_to_3byte( MRI_IMAGE *oldim )  /* 15 Apr 1999 */
{
   MRI_IMARR *imar ;
   MRI_IMAGE *rim , *gim , *bim ;
   byte      *rr  , *gg  , *bb  , *rgb ;
   int ii , npix ;

ENTRY("mri_rgb_to_3byte") ;
   if( oldim == NULL || oldim->kind != MRI_rgb ) RETURN( NULL );

   rim = mri_new_conforming( oldim , MRI_byte ) ; rr = MRI_BYTE_PTR(rim) ;
   gim = mri_new_conforming( oldim , MRI_byte ) ; gg = MRI_BYTE_PTR(gim) ;
   bim = mri_new_conforming( oldim , MRI_byte ) ; bb = MRI_BYTE_PTR(bim) ;
                                                  rgb= MRI_BYTE_PTR(oldim);
   npix = oldim->nvox ;

   for( ii=0 ; ii < npix ; ii++ ){
     rr[ii] = rgb[3*ii  ] ;
     gg[ii] = rgb[3*ii+1] ;
     bb[ii] = rgb[3*ii+2] ;
   }

   INIT_IMARR(imar) ;
   ADDTO_IMARR(imar,rim) ; ADDTO_IMARR(imar,gim) ; ADDTO_IMARR(imar,bim) ;

   RETURN( imar );
}

/*-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_sharpen_rgb( float phi , MRI_IMAGE *im )
{
   MRI_IMAGE *flim , *shim , *newim ;
   byte  *iar , *nar ;
   float *sar , *far ;
   int ii , nvox , rr,gg,bb ;
   float fac ;

ENTRY("mri_sharpen_rgb") ;

   if( im == NULL ) RETURN( NULL );

   if( im->kind != MRI_rgb ) RETURN( mri_sharpen(phi,0,im) );

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
   RETURN( newim );
}

/*-----------------------------------------------------------------------------*/

static float mfac = 0.4f ;

MRI_IMAGE * mri_flatten_rgb( MRI_IMAGE *im )
{
   MRI_IMAGE *flim , *shim , *newim ;
   byte  *iar , *nar ;
   float *sar , *far ;
   int ii , nvox , rr,gg,bb ;
   float fac , ifac,ffac ;

ENTRY("mri_flatten_rgb") ;

   if( im == NULL ) RETURN( NULL );

   if( im->kind != MRI_rgb ) RETURN( mri_flatten(im) );

   flim  = mri_to_float( im ) ;                  /* intensity of input */
   shim  = mri_flatten( flim ) ;                 /* flatten intensity  */
   newim = mri_new_conforming( im , MRI_rgb ) ;  /* will be output     */

   nar = MRI_BYTE_PTR(newim) ; iar = MRI_BYTE_PTR(im) ;
   far = MRI_FLOAT_PTR(flim) ; sar = MRI_FLOAT_PTR(shim) ;
   nvox = newim->nvox ;

   ifac = mfac ;
   ffac = (1.0f-ifac) ;

   for( ii=0 ; ii < nvox ; ii++ ){
     if( far[ii] < 0.0f || sar[ii] < 0.0f ){
       nar[3*ii] = nar[3*ii+1] = nar[3*ii+2] = 0 ;
     } else {
       fac = ffac*(255.9f*sar[ii]/far[ii])+ifac ; /* will be positive */
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
   RETURN( newim );
}

/*-----------------------------------------------------------------------------*/

void mri_invert_inplace( MRI_IMAGE *im )
{
   register byte *bar ;
   register int ii , nbar ;

ENTRY("mri_invert_inplace") ;

   if( im == NULL ) EXRETURN ;
   switch( im->kind ){
     default: EXRETURN ;
     case MRI_byte:  nbar =   im->nvox ; bar = MRI_BYTE_PTR(im) ; break ;
     case MRI_rgb:   nbar = 3*im->nvox ; bar = MRI_RGB_PTR(im)  ; break ;
   }
   for( ii=0 ; ii < nbar ; ii++ ) bar[ii] = 255 - bar[ii] ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

/*** Am not sure if this works properly! ***/

void mri_gamma_rgb_inplace( float gam , MRI_IMAGE *im )  /* 15 Jan 2007 */
{
   MRI_IMAGE *flim ; byte *iar ; float *far ;
   int ii , nvox , rr,gg,bb ; float fac , val , gg1 ;

ENTRY("mri_gamma_rgb_inplace") ;

   if( im == NULL || im->kind != MRI_rgb || gam <= 0.0f ) EXRETURN ;

   flim = mri_to_float( im ) ; /* intensity of input */
   iar = MRI_BYTE_PTR(im) ; far = MRI_FLOAT_PTR(flim) ;
   fac = mri_max(flim) ;
   if( fac <= 0.0f ){ mri_free(flim); EXRETURN; } else fac = 1.0f/fac ;

   nvox = im->nvox ; gg1 = gam-1.0f ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( far[ii] <= 0.0f ){
       iar[3*ii] = iar[3*ii+1] = iar[3*ii+2] = 0 ;
     } else {
       val = powf( far[ii]*fac , gg1 ) ;
       rr  = rint(val * iar[3*ii  ]) ;
       gg  = rint(val * iar[3*ii+1]) ;
       bb  = rint(val * iar[3*ii+2]) ;
       iar[3*ii  ] = (rr > 255) ? 255 : rr ;
       iar[3*ii+1] = (gg > 255) ? 255 : gg ;
       iar[3*ii+2] = (bb > 255) ? 255 : bb ;
     }
   }

   mri_free(flim) ; EXRETURN ;
}

/*-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_make_rainbow( int nx , int ny , int ncol , rgbyte *col )
{
   MRI_IMAGE *bim ; byte *bar ; int ii,jj , pp ; float qq,rr ;

   if( ncol < 2 || col == NULL ) return NULL ;
   if( nx < 1      ) nx = 8 ;
   if( ny < 2*ncol ) ny = 2*ncol ;

   bim = mri_new(nx,ny,MRI_rgb) ; bar = MRI_RGB_PTR(bim) ;

   for( jj=0 ; jj < ny ; jj++ ){
     qq = jj*(ncol-1.001f)/(ny-1.0f) ; pp = (int)qq ;
     qq = qq-pp ; rr = 1.0f-qq ;
     for( ii=0 ; ii < nx ; ii++ ){
       bar[3*(ii+jj*nx)+0] = (byte)( rr*col[pp].r + qq*col[pp+1].r ) ;
       bar[3*(ii+jj*nx)+1] = (byte)( rr*col[pp].g + qq*col[pp+1].g ) ;
       bar[3*(ii+jj*nx)+2] = (byte)( rr*col[pp].b + qq*col[pp+1].b ) ;
     }
   }
   return bim ;
}
