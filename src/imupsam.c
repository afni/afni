/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include <stdlib.h>

int main( int argc , char * argv[] )
{
   MRI_IMAGE * inim , * outim ;
   int nup ;

   if( argc < 4 || strcmp(argv[1],"-help") == 0 ){
      printf( "Usage: imupsam n input_image output_image\n"
              "* Upsamples the input 2D image by a factor of n and\n"
              "    writes result into output_image; n must be an\n"
              "    integer in the range 2..30.\n"
              "* 7th order polynomial interpolation is used in each\n"
              "    direction.\n"
              "* Inputs can be complex, float, short, PGM, or PPM.\n"
              "* Author: RW Cox -- 16 April 1999.\n" ) ;
      exit(0) ;
   }

   machdep() ;

   nup = strtol( argv[1] , NULL , 10 ) ;
   if( nup < 2 || nup > 30 ){
      fprintf(stderr,"** imupsam: nup=%s out of range\n",argv[1]); exit(1);
   }

   inim = mri_read_just_one( argv[2] ) ;
   if( inim == NULL ){
      fprintf(stderr,"** imupsam: can't read input image %s\n",argv[2]); exit(1);
   }

   outim = mri_dup2D( nup , inim ) ;
   if( outim == NULL ){
      fprintf(stderr,"** imupsam: upsampling fails!\n"); exit(1);
   }

   nup = mri_write( argv[3] , outim ) ;
   if( nup == 0 ){
      fprintf(stderr,"** imupsam: can't write output image %s\n",argv[3]); exit(1);
   }

   exit(0) ;
}
