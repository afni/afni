#include "mrilib.h"

/*** 7D SAFE ***/

void mri_histobyte( MRI_IMAGE * im , int * hist )
{
   register int ih , npix , ii ;
   byte * bar ;

   if( im == NULL || im->kind != MRI_byte || hist == NULL ) return ;

   npix = im->nvox ;
   bar  = MRI_BYTE_PTR(im) ;

   for( ih=0 ; ih < 256 ; ih++ ) hist[ih] = 0 ;

   for( ii=0 ; ii < npix ; ii++ )
      hist[ bar[ii] ] ++ ;

   return ;
}
