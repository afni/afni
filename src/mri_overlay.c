#include "mrilib.h"

/*------------------------------------------------------------------------------
  06 Aug 1999: overlay image imover into image imbase at position ix,jy.
  08 Jun 2000: modified to allow for imover being too big to fit.

  Input images must contain the same kind of data!
--------------------------------------------------------------------------------*/

void mri_overlay_2D( MRI_IMAGE * imbase , MRI_IMAGE * imover , int ix , int jy )
{
   byte * ba , * ov ;
   int nxba,nyba , nxov,nyov , jj , nxxov,nyyov , psiz ;

   if( imbase == NULL || imover == NULL ) return ;
   if( imbase->kind != imover->kind     ) return ;

   nxba = imbase->nx ; nyba = imbase->ny ; ba = mri_data_pointer(imbase) ;
   nxov = imover->nx ; nyov = imover->ny ; ov = mri_data_pointer(imover) ;
   psiz = imbase->pixel_size ;

   if( ix >= nxba || jy >= nyba ) return ;  /* bad placement */

   if( ix < 0 ) ix = nxba + ix ;            /* offset from right */
   if( jy < 0 ) jy = nyba + jy ;            /* offset from bottom */

   nxxov = nxov ;
   if( ix+nxov > nxba ) nxxov = nxba - ix ; /* row too long? */

   nyyov = nyov ;
   if( jy+nyov > nyba ) nyyov = nyba - jy ; /* too many rows? */

   for( jj=0 ; jj < nyyov ; jj++ )
      memcpy( ba + ((jy+jj)*nxba + ix)*psiz , ov + jj*nxov*psiz , nxxov*psiz ) ;

   return ;
}
