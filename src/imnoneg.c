/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

int main( int argc , char * argv[] )
{
   MRI_IMAGE * imin ;
   short *     sar ;
   int npix , ii ;

   if( argc < 3 || strncmp(argv[1],"-help",4) == 0 ){
      printf("Usage: imnoneg input_image output_image\n"
             "Zeros out all negative pixel in input_image\n"
             "Only works on images of shorts!\n" ) ;
      exit(0) ;
   }

   machdep() ;

   imin = mri_read(argv[1]) ;
   if( imin == NULL || imin->kind != MRI_short ) exit(1) ;
   npix = imin->nx * imin->ny ;
   sar  = (short *) mri_data_pointer(imin) ;

   for( ii=0 ; ii < npix ; ii++ )
      if( sar[ii] < 0 ) sar[ii] = 0 ;

   mri_write(argv[2],imin) ;
   exit(0) ;
}
