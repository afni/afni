#include "mrilib.h"

/*** NOT 7D SAFE ***/

/*** Makes a 2D image that is a given function of (x,y) ***/

MRI_IMAGE * mri_float_func( int   nx    , int   ny ,
                            float xzero , float yzero ,
                            float dx    , float dy ,
                            float (* func)( float , float ) )
{
   int ii , jj , jpos ;
   float yy ;
   MRI_IMAGE * im ;
   float *     flim ;

   im   = mri_new( nx ,ny , MRI_float ) ;
   flim = mri_data_pointer( im ) ;

   for( jj=0 ; jj < ny ; jj++ ){
      jpos = nx * jj ;
      yy   = yzero + jj * dy ;
      for( ii=0 ; ii < nx ; ii++ ){
         flim[ii+jpos] = func( xzero + ii*dx , yy ) ;
      }
   }

   return im ;
}
