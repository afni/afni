/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*----------------------------------------------------------------------------*/
/*! Overlay a smaller 2D image into a larger 2D image.
    - 06 Aug 1999: Overlay imover into imbase at position ix,jy.
                   - Input images must contain the same kind of data!
                   - ix >= 0 is offset from left; ix < 0 is from right
                   - jy >= 0 is offset from top ; jy < 0 is from bottom
    - 08 Jun 2000: Modified to allow for imover being too big to fit.
    - 13 Nov 2002: Modified to allow some type conversion from imover to imbase.
    - 15 Apr 2003: Fixed use of imover to imov to get 'ov' pointer (oopsie).
------------------------------------------------------------------------------*/

void mri_overlay_2D( MRI_IMAGE *imbase , MRI_IMAGE *imover , int ix , int jy )
{
   byte *ba , *ov ;
   int nxba,nyba , nxov,nyov , jj , nxxov,nyyov , psiz ;
   MRI_IMAGE *imov ;

ENTRY("mri_overlay_2D") ;

   if( imbase == NULL || imover == NULL ) EXRETURN ;  /* bad inputs */

   /* 13 Nov 2002: possibly do type conversion on imover to match imbase */

   if( imbase->kind == imover->kind ){
     imov = imover ;
   } else if( imbase->kind == MRI_byte && imover->kind == MRI_rgb ){
     imov = mri_to_byte( imover ) ;
   } else if( imbase->kind == MRI_rgb  && imover->kind == MRI_byte ){
     imov = mri_to_rgb( imover ) ;
   } else {
     EXRETURN ;   /* bad inputs */
   }

   /* get dimensions and pointers of images */

   nxba = imbase->nx ; nyba = imbase->ny ; ba = mri_data_pointer(imbase) ;
   nxov = imov  ->nx ; nyov = imov  ->ny ; ov = mri_data_pointer(imov  ) ;
   psiz = imbase->pixel_size ;

   if( ix >= nxba || jy >= nyba ){          /* bad placement */
     if( imov != imover ) mri_free(imov) ;
     EXRETURN ;
   }

   /* negative offsets are from right & bottom */

   if( ix < 0 ){
     ix = nxba + ix ;
     if( ix < 0 ){ if( imov != imover ) mri_free(imov); EXRETURN; } /* bad */
   }
   if( jy < 0 ){
     jy = nyba + jy ;
     if( jy < 0 ){ if( imov != imover ) mri_free(imov); EXRETURN; } /* bad */
   }

   nxxov = nxov ;                           /* length of overlay row */
   if( ix+nxov > nxba ) nxxov = nxba - ix ; /* row too long for base? */

   nyyov = nyov ;                           /* number of overlay rows */
   if( jy+nyov > nyba ) nyyov = nyba - jy ; /* too many rows for base? */

   /* actually overlay each row */

   for( jj=0 ; jj < nyyov ; jj++ )
     memcpy( ba + ((jy+jj)*nxba + ix)*psiz , ov + jj*nxov*psiz , nxxov*psiz ) ;

   if( imov != imover ) mri_free(imov) ;  /* throw away converted overlay? */
   EXRETURN ;
}
