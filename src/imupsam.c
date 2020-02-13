/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include <stdlib.h>

int main( int argc , char *argv[] )
{
   MRI_IMAGE *inim , *outim ;
   int nup , iarg=1 , do_ascii=0 ;

   if( argc < 4 || strcmp(argv[1],"-help") == 0 ){
      printf( "Usage: imupsam [-A] n input_image output_image\n"
              "\n"
              "*** Consider using the newer 2dcat for resampling.\n"
              "    byte and rgb images\n"
              "\n"
              "* Upsamples the input 2D image by a factor of n and\n"
              "    writes result into output_image; n must be an\n"
              "    integer in the range 2..30.\n"
              "* 7th order polynomial interpolation is used in each\n"
              "    direction.\n"
              "* Inputs can be complex, float, short, PGM, PPM, or JPG.\n"
              "* If input_image is in color (PPM or JPG), output will\n"
              "    be PPM unless output_image ends in '.jpg'.\n"
              "* If output_image is '-', the result will be written\n"
              "    to stdout (so you could pipe it into something else).\n"
              "* The '-A' option means to write the result in ASCII\n"
              "    format: all the numbers for the file are output,\n"
              "    and nothing else (no header info).\n"
              "* Author: RW Cox -- 16 April 1999.\n" ) ;
      exit(0) ;
   }

   machdep() ;

   if( strcmp(argv[iarg],"-A") == 0 ){
     iarg++ ; do_ascii = 1 ;
     if( iarg+3 > argc ){
       fprintf(stderr,"** imupsam: not enough arguments!\n") ; exit(1) ;
     }
   }

   nup = strtol( argv[iarg++] , NULL , 10 ) ;
   if( nup < 1 || nup > 30 ){
     fprintf(stderr,"** imupsam: nup=%s out of range 1..30\n",argv[iarg-1]); exit(1);
   }

   inim = mri_read_just_one( argv[iarg++] ) ;
   if( inim == NULL ){
     fprintf(stderr,"** imupsam: can't read input image %s\n",argv[iarg-1]); exit(1);
   }

   outim = mri_dup2D( nup , inim ) ;
   if( outim == NULL ){
     fprintf(stderr,"** imupsam: upsampling fails!\n"); exit(1);
   }

   if( do_ascii )
     nup = mri_write_ascii( argv[iarg] , outim ) ;
   else
     nup = mri_write( argv[iarg] , outim ) ;

   if( nup == 0 ){
     fprintf(stderr,"** imupsam: can't write output image %s\n",argv[iarg]); exit(1);
   }

   exit(0) ;
}
