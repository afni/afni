#include "mrilib.h"

/*! Returns 1 if the image is RGB and R=G=B for
    all voxels, otherwise returns 0.  [03 Dec 2003] */

int mri_isgray( MRI_IMAGE *im )
{
   register int nvox , ii ;
   register byte *bar ;

ENTRY("mri_isgray") ;

   if( im == NULL || im->kind != MRI_rgb ) RETURN( 0 );

   nvox = im->nvox ;
   bar  = MRI_RGB_PTR(im) ;
   for( ii=0 ; ii < nvox ; ii++ )
     if( bar[3*ii] != bar[3*ii+1] ||
         bar[3*ii] != bar[3*ii+2]   ) RETURN( 0 );

   RETURN( 1 ) ;
}
