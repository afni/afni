/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "string.h"

#define ERREX(str) ( fprintf(stderr,"ERROR: %s\a\n",str) , exit(1) )

int main( int argc , char * argv[] )
{
   MRI_IMAGE * imin , * imout ;
   MRI_IMARR * imar ;

   if( argc < 3 || strncmp(argv[1],"-help",2) == 0 ){
      printf(
        "Usage: nsize image_in image_out\n"
        "  Zero pads 'image_in' to NxN, N=64,128,256,512, or 1024, \n"
        "  whichever is the closest size larger than 'image_in'.\n"
        "  [Works only for byte and short images.]\n" ) ;
      exit(0) ;
   }

   imar = mri_read_file( argv[1] ) ;  if( imar == NULL ) ERREX("can't continue!") ;
   if( imar->num != 1 ) ERREX("more than 1 image in input file!") ;
   imin = IMAGE_IN_IMARR(imar,0) ;

   imout = mri_nsize( imin ) ;
   if( imout == NULL ){
      fprintf(stderr,"*** failed to scale image! error exit!\n") ;
      exit(-1) ;
   }

   mri_write( argv[2] , imout ) ;
   exit(0) ;
}
