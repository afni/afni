/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*** 7D SAFE ***/

/** int hist[256] **/

void mri_histobyte( MRI_IMAGE * im , int * hist )
{
   register int ih , npix , ii ;
   byte * bar ;

ENTRY("mri_histobyte") ;

   if( im == NULL || im->kind != MRI_byte || hist == NULL ) EXRETURN ;

   npix = im->nvox ;
   bar  = MRI_BYTE_PTR(im) ;

   for( ih=0 ; ih < 256 ; ih++ ) hist[ih] = 0 ;

   for( ii=0 ; ii < npix ; ii++ )
      hist[ bar[ii] ] ++ ;

   EXRETURN ;
}
