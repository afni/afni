/*-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  This software is Copyright 1994,1995 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application.  The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+*/

#include "mrilib.h"
#include "string.h"

#define ERREX(str) ( fprintf(stderr,"ERROR: %d\a\n",str) , exit(1) )

int main( int argc , char * argv[] )
{
   MRI_IMAGE * imin , * imout ;
   MRI_IMARR * imar ;

   if( argc < 3 || strncmp(argv[1],"-help",2) == 0 ){
      printf(
        "Copyright 1994,1995 Medical College of Wisconsin\n\n"
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
