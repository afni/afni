#include "mrilib.h"

/** not 7D SAFE **/

/*--------------------------------------------------------------------
  12 Nov 2001:
  Get the center of mass of a 2D image;
  store it in the user-supplied locations *xcm, *ycm.
----------------------------------------------------------------------*/

void mri_get_cmass_2D( MRI_IMAGE *im , float *xcm , float *ycm )
{
   int ii,jj,joff , nx,ny ;
   float xx , yy , sum , val , *far ;
   MRI_IMAGE *flim ;

ENTRY("mri_get_cmass_2D") ;

   if( im == NULL || xcm == NULL || ycm == NULL ) EXRETURN ;

   if( im->kind != MRI_float ) flim = mri_to_float( im ) ;
   else                        flim = im ;

   far = MRI_FLOAT_PTR(flim) ;
   nx  = im->nx ; ny = im->ny ;

   sum = xx = yy = 0.0 ;
   for( jj=0 ; jj < ny ; jj++ ){
      joff = jj * nx ;
      for( ii=0 ; ii < nx ; ii++ ){
         val = fabs(far[ii+joff]) ;
         sum += val ;
         xx  += val * ii ;
         yy  += val * jj ;
      }
   }

   if( sum > 0.0 ){ xx /= sum ; yy /= sum ; }

   if( flim != im ) mri_free(flim) ;

   *xcm = xx ; *ycm = yy ; EXRETURN ;
}
