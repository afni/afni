/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*------------------------------------------------------------------------------
  06 Aug 1999: overlay image imover into image imbase at position ix,jy.
  08 Jun 2000: modified to allow for imover being too big to fit.

  Input images must contain the same kind of data!
  13 Nov 2002: modified to allow some type conversion from imover to imbase.
--------------------------------------------------------------------------------*/

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

   nxba = imbase->nx ; nyba = imbase->ny ; ba = mri_data_pointer(imbase) ;
   nxov = imov  ->nx ; nyov = imov  ->ny ; ov = mri_data_pointer(imov  ) ;
   psiz = imbase->pixel_size ;

   if( ix >= nxba || jy >= nyba ){          /* bad placement */
     if( imov != imover ) mri_free(imov) ;
     EXRETURN ;
   }

   if( ix < 0 ) ix = nxba + ix ;            /* offset from right */
   if( jy < 0 ) jy = nyba + jy ;            /* offset from bottom */

   nxxov = nxov ;
   if( ix+nxov > nxba ) nxxov = nxba - ix ; /* row too long? */

   nyyov = nyov ;
   if( jy+nyov > nyba ) nyyov = nyba - jy ; /* too many rows? */

   for( jj=0 ; jj < nyyov ; jj++ )
     memcpy( ba + ((jy+jj)*nxba + ix)*psiz , ov + jj*nxov*psiz , nxxov*psiz ) ;

   if( imov != imover ) mri_free(imov) ;
   EXRETURN ;
}
