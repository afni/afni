#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int nx,ny,ii,npix ;
   MRI_IMAGE * im1 , * im2 , * flim , * cxim ;
   float * a1 , * a2 ;
   complex * cxar ;

   if( argc < 4 ){
      printf("Usage: 2tocx im1 im2 cxim\n") ;
      exit(0) ;
   }

   im1 = mri_read( argv[1] ) ;
   im2 = mri_read( argv[2] ) ;
   if( im1 == NULL || im2 == NULL ) exit(1) ;

   flim = mri_to_float(im1) ; mri_free(im1) ; im1 = flim ; a1 = MRI_FLOAT_PTR(im1) ;
   flim = mri_to_float(im2) ; mri_free(im2) ; im2 = flim ; a2 = MRI_FLOAT_PTR(im2) ;

   nx = im1->nx ; ny = im1->ny ; npix = nx*ny ;

   cxim = mri_new( nx , ny , MRI_complex ) ;
   cxar = MRI_COMPLEX_PTR(cxim) ;

   for( ii=0 ; ii < npix ; ii++ ){
      cxar[ii].r = a1[ii] ;
      cxar[ii].i = a2[ii] ;
   }

   mri_write( argv[3] , cxim ) ;
   exit(0) ;
}
