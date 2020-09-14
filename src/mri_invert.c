#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/* Invert the contrast of an image, overwriting itself.
     uperc = upper percentage point at which to invert
     mask  = what do you think?
   Works on 2D or 3D images.
*//*-------------------------------------------------------------------------*/

void mri_invertcontrast_inplace( MRI_IMAGE *im , float uperc , byte *mask )
{
   byte *mmm=NULL ; MRI_IMAGE *qim ;
   int nvox , nhist , ii ; float *hist=NULL , *imar , ucut ;

   if( im == NULL || im->ny == 1 ) return ;

        if( uperc <  90.0f ) uperc =  90.0f ;
   else if( uperc > 100.0f ) uperc = 100.0f ;

   /* make a float copy */

   qim = mri_to_float(im) ;

   /* find the mask */

   mmm = mask ;
   if( mmm == NULL ) mmm = mri_automask_image(qim) ;

   /* extract nonzero voxels */

   nvox = im->nvox ;
   hist = (float *)malloc(sizeof(float)*nvox) ;
   imar = MRI_FLOAT_PTR(qim) ;
   for( nhist=ii=0 ; ii < nvox ; ii++ ){ if( mmm[ii] ) hist[nhist++] = imar[ii]; }

   if( nhist < 100 ){
     if( mmm != mask ) free(mmm) ;
     free(hist) ; mri_free(qim) ; return ;
   }

   /* sort them to find the correct value at which to invert */

   qsort_float(nhist,hist) ;
   ii = (int)rintf(nhist*uperc*0.01f) ; ucut = hist[ii] ; free(hist) ;

   /* invert contrast */

   for( ii=0 ; ii < nvox ; ii++ ){
     if(  mmm[ii] && imar[ii] > 0.0f ) imar[ii] = ucut - imar[ii] ;
     if( !mmm[ii] || imar[ii] < 0.0f ) imar[ii] = 0.0f ;
   }
   if( mmm != mask ) free(mmm) ;

   /* apply the inverted contrast to the original image */

   switch( im->kind ){

     case MRI_float:{
       AAmemcpy( MRI_FLOAT_PTR(im) , imar , sizeof(float)*nvox ) ;
     }
     break ;

     case MRI_short:{
       short *shar = MRI_SHORT_PTR(im) ;
       for( ii=0 ; ii < nvox ; ii++ ) shar[ii] = SHORTIZE(imar[ii]) ;
     }
     break ;

#undef  INTENSITY
#define INTENSITY(r,g,b)   (0.299*(r)+0.587*(g)+0.114*(b))
     case MRI_rgb:{
       byte *bar = MRI_RGB_PTR(im) ; float r,g,b, irgb,iinv ;
       for( ii=0 ; ii < nvox ; ii++ ){
         iinv = imar[ii] ;
         if( iinv == 0.0f ){
           bar[3*ii+0] = bar[3*ii+1] = bar[3*ii+2] = 0 ;
         } else {
           r = bar[3*ii+0] ;
           g = bar[3*ii+1] ;
           b = bar[3*ii+2] ; irgb = 0.299*r + 0.587*g + 0.144*b ;
           if( irgb > 0.0f ){              /* should always be true */
             irgb = iinv / irgb ;
             r *= irgb ; bar[3*ii+0] = BYTEIZE(r) ;
             g *= irgb ; bar[3*ii+1] = BYTEIZE(g) ;
             b *= irgb ; bar[3*ii+2] = BYTEIZE(b) ;
           }
         }
       }
     }
     break ;

     default:
       WARNING_message("unknown image type %s in mri_invertcontrast_inplace()",
                       MRI_type_string(im->kind) ) ;
     break ;

   }

   mri_free(qim) ;
   return ;
}
