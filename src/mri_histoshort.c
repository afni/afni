#include "mrilib.h"

/*** 7D SAFE ***/

#define NUM_SHORT 65536
#define OFF_SHORT 32768

/*-------- for this one, declare 'int hist[65536]' --------*/

void mri_histoshort_all( MRI_IMAGE * im , int * hist )
{
   register int ih , npix , ii ;
   short * sar ;

ENTRY("mri_histoshort_all") ;

   if( im == NULL || im->kind != MRI_short || hist == NULL ) EXRETURN ;

   npix = im->nvox ;
   sar  = MRI_SHORT_PTR(im) ;

   for( ih=0 ; ih < NUM_SHORT ; ih++ ) hist[ih] = 0 ;

   for( ii=0 ; ii < npix ; ii++ )
      hist[ sar[ii]+OFF_SHORT ] ++ ;

   EXRETURN ;
}

/*-------- for this one, declare 'int hist[32768]' --------*/

void mri_histoshort_nonneg( MRI_IMAGE * im , int * hist )
{
   register int ih , npix , ii ;
   short * sar ;

ENTRY("mri_histoshort_nonneg") ;

   if( im == NULL || im->kind != MRI_short || hist == NULL ) EXRETURN ;

   npix = im->nvox ;
   sar  = MRI_SHORT_PTR(im) ;

   for( ih=0 ; ih < OFF_SHORT ; ih++ ) hist[ih] = 0 ;

   for( ii=0 ; ii < npix ; ii++ )
      if( sar[ii] >= 0 ) hist[ sar[ii] ] ++ ;

   EXRETURN ;
}
