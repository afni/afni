/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *im , *imb ;
   int narg ;

   if( argc < 2 || strncmp(argv[1],"-help",2) == 0 ){
      printf( "Converts an image to raw pgm format.\n") ;
      printf( "Results go to stdout and should be redirected.\n");
      printf( "Usage:   mritopgm input_image\n" ) ;
      printf( "Example: mritopgm fred.001 | ppmtogif > fred.001.gif\n") ;
      exit(0) ;
   }

   narg = 1 ;
   im   = mri_read_just_one( argv[narg] ) ;
   if( im == NULL ) exit(1) ;

   imb = mri_to_byte( im ) ;
   mri_free( im ) ;

   printf( "P5 %d %d 255\n" , imb->nx , imb->ny ) ;
   fwrite( imb->im.byte_data , imb->pixel_size , imb->nx * imb->ny , stdout ) ;

   exit(0) ;
}
