#include "mrilib.h"

/*---------------------------------------------------------------------------------
  06 Aug 1999: overlay image imover into image imbase at position ix,jy.
-----------------------------------------------------------------------------------*/

void mri_overlay_2D( MRI_IMAGE * imbase , MRI_IMAGE * imover , int ix , int jy )
{
   byte * ba , * ov ;
   int nxba,nyba , nxov,nyov , jj ;

   if( imbase == NULL           || imover == NULL           ) return ;
   if( imbase->kind != MRI_byte || imover->kind != MRI_byte ) return ;

   nxba = imbase->nx ; nyba = imbase->ny ; ba = MRI_BYTE_PTR(imbase) ;
   nxov = imover->nx ; nyov = imover->ny ; ov = MRI_BYTE_PTR(imover) ;

   if( ix < 0 ) ix = nxba + ix ;
   if( jy < 0 ) jy = nyba + jy ;

   if( ix+nxov > nxba || jy+nyov > nyba ) return ;

   for( jj=0 ; jj < nyov ; jj++ )
      memcpy( ba + ((jy+jj)*nxba + ix) , ov + jj*nxov , nxov ) ;

   return ;
}
