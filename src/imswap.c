/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *im ;

   if( argc < 3 || strncmp(argv[1],"-help",5) == 0 ){
      fprintf( stderr , "Usage: imswap infile outfile\n"
                        "* Swaps bytes in infile to produce outfile!\n" ) ;
      exit(0) ;
   }

   im = mri_read_just_one( argv[1] ) ;  if( im == NULL ) exit(1) ;
   mri_swapbytes( im ) ;
   mri_write( argv[2] , im ) ;

   exit(0) ;
}
