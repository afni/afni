#include "mrilib.h"

/*-----------------------------------------------------------------------*/
/*! Composite a collection of MRI_rgb/MRI_rgba/MRI_byte images.
    The first image is on top, etc.  For MRI_rgb/MRI_byte images,
    the default opacity is alpha in 0..1.  Black (0,0,0) pixels on input
    are not overlaid. The output image is MRI_rgb; it is composited
    against a black backdrop.
-------------------------------------------------------------------------*/

MRI_IMAGE * mri_rgba_composite_array( MRI_IMARR *imar, float alpha )
{
   register int npix,ii,jj , nn,nim ;
   MRI_IMAGE *outim , *inim ;
   rgbyte *outar ;
   float  *usop  ;

ENTRY("mri_rgba_composite") ;

   if( imar == NULL || IMARR_COUNT(imar) == 0 ) RETURN(NULL) ;

   if( alpha <= 0.0 || alpha > 1.0 ) alpha = 1.0 ;  /* default default */

   nim   = IMARR_COUNT(imar) ;
   outim = mri_new_conforming( IMARR_SUBIM(imar,0) , MRI_rgb ) ;
   outar = (rgbyte *) MRI_RGB_PTR(outim) ;          /* is all zero */
   npix  = outim->nvox ;

   usop  = (float *) malloc(sizeof(float)*npix) ;   /* used up opacity */
   for( ii=0 ; ii < npix ; ii++ ) usop[ii] = 0.0 ;

#undef  MAX_OPACITY
#define MAX_OPACITY 0.95

   for( nn=0 ; nn < nim ; nn++ ){
     inim = IMARR_SUBIM(imar,nn) ;
     if( inim->nvox < npix ) continue ;  /* bad */

     switch( inim->kind ){
       default: break ;   /* bad */

       case MRI_byte:{
         byte *inar = (byte *) MRI_BYTE_PTR(inim) , val ;
         register float opa ;
         for( ii=0 ; ii < npix ; ii++ ){
           if( usop[ii] < MAX_OPACITY && inar[ii] > 0 ){

             opa = alpha * (1.0-usop[ii]) ; usop[ii] += opa ;
             val = (byte)( opa * inar[ii] ) ;

             outar[ii].r += val ;
             outar[ii].g += val ;
             outar[ii].b += val ;
           }
         }
       }

       case MRI_rgb:{
         rgbyte *inar = (rgbyte *) MRI_RGB_PTR(inim) ;
         register float opa ;
         for( ii=0 ; ii < npix ; ii++ ){
           if( usop[ii]   < MAX_OPACITY &&
               inar[ii].r > 0           && inar[ii].g > 0 && inar[ii].b > 0 ){

             opa = alpha * (1.0-usop[ii]) ; usop[ii] += opa ;

             outar[ii].r += (byte)( opa * inar[ii].r ) ;
             outar[ii].g += (byte)( opa * inar[ii].g ) ;
             outar[ii].b += (byte)( opa * inar[ii].b ) ;
           }
         }
       }
       break ;

       case MRI_rgba:{
         rgba *inar = (rgba *) MRI_RGBA_PTR(inim) ;
         register float opa ;
         for( ii=0 ; ii < npix ; ii++ ){
           if( usop[ii]   < MAX_OPACITY &&
               inar[ii].r > 0           && inar[ii].g > 0 && inar[ii].b > 0 ){

             opa = 0.00392156*inar[ii].a * (1.0-usop[ii]) ; usop[ii] += opa ;

             outar[ii].r += (byte)( opa * inar[ii].r ) ;
             outar[ii].g += (byte)( opa * inar[ii].g ) ;
             outar[ii].b += (byte)( opa * inar[ii].b ) ;
           }
         }
       }
       break ;
     }
   }

   free(usop) ; RETURN(outim) ;
}

/*--------------------------------------------------------------------------*/
#include <stdarg.h>
/*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_rgba_composite_VA( float alpha, ... )
{
   MRI_IMARR *imar=NULL ;
   MRI_IMAGE *im ;
   va_list vararg_ptr ;

   va_start( vararg_ptr , alpha ) ;

   while(1){
      im = va_arg( vararg_ptr , MRI_IMAGE * ) ;
      if( im == NULL ) break ;
      if( imar == NULL ) INIT_IMARR(imar) ;
      ADDTO_IMARR(imar,im) ;
   }

   va_end( vararg_ptr ) ;

   if( imar == NULL ) return NULL ;
   im = mri_rgba_composite_array( imar , alpha ) ;

   FREE_IMARR(imar) ; return im ;
}

/*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_rgba_composite_two( float alpha, MRI_IMAGE *i1, MRI_IMAGE *i2 )
{
   MRI_IMARR *imar ; MRI_IMAGE *im ;
   INIT_IMARR(imar) ;
   ADDTO_IMARR(imar,i1) ; ADDTO_IMARR(imar,i2) ;
   im = mri_rgba_composite_array( imar , alpha ) ;
   FREE_IMARR(imar) ; return im ;
}
