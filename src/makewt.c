#include "mrilib.h"
#include <string.h>

int main( int argc , char * argv[] )
{
    MRI_IMAGE * imbar , * imsdev , * imwt ;
    int ii , nvox , nsum ;
    float * bar , * sdev , * wt ;
    float bmax , smax , bcut , scl , bsum ;
    float hist[11] ;

    if( argc < 4 ){
      printf("Usage: %s mean_image sdev_image wt_image\n",argv[0]) ;
      exit(0) ;
    }

    imbar  = mri_read_just_one( argv[1] ) ; if( imbar == NULL ) exit(1) ;
    imsdev = mri_read_just_one( argv[2] ) ; if( imsdev== NULL ) exit(1) ;

    if( imbar->kind != MRI_float ){
      imwt = mri_to_float(imbar) ; mri_free(imbar) ; imbar = imwt ;
    }

    if( imsdev->kind != MRI_float ){
      imwt = mri_to_float(imsdev) ; mri_free(imsdev) ; imsdev = imwt ;
    }

    nvox = imbar->nvox ;
    imwt = mri_new( imbar->nx , imbar->ny , MRI_float ) ;

    bar  = MRI_FLOAT_PTR(imbar) ;
    sdev = MRI_FLOAT_PTR(imsdev) ;
    wt   = MRI_FLOAT_PTR(imwt) ;

    bcut = 0.05 * mri_maxabs(imbar) ; bsum = 0.0 ; nsum = 0 ;
    for( ii=0 ; ii < nvox ; ii++ ){
      if( bar[ii] > bcut ){ bsum += bar[ii] ; nsum++ ; }
    }
    bcut = 0.25 * bsum / nsum ;

    smax = mri_maxabs(imsdev) ; scl  = 0.25 * bcut*bcut ;

    printf("cutoff value = %f\n",bcut) ;

    for( ii=0 ; ii < nvox ; ii++ ){
      if( bar[ii] < bcut || sdev[ii] <= 0.0 ) wt[ii] = 0.0 ;
      else                                    wt[ii] = scl / SQR(sdev[ii]) ;
    }

    mri_write( argv[3] , imwt ) ;
    exit(0) ;
}
