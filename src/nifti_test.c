#include "nifti_1.c"

/********************* Test program for NIFTI-1 file format ******************/

/*   cc -o nifti_test -O3 nifti_test.c -I. -lm   */

int main( int argc , char *argv[] )
{
   nifti_image *nim ;
   int iarg=1 ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: nifti_test nifti_filename ...\n"
            "Prints out header information from the file(s).\n" ) ;
     exit(0) ;
   }

   for( ; iarg < argc ; iarg++ ){
     nim = nifti_image_read( argv[iarg] ) ;
     nifti_image_infodump( nim ) ;
     nifti_image_free( nim ) ;
   }
}
