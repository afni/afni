#include "nifti1.c"

/* cc -o nifti1_test -O2 nifti1_test.c -lm */

/****************************************************************************/

int main( int argc , char *argv[] )
{
   nifti_image *nim ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: nifti1_test infile [outfile]\n") ;
     printf("sizeof(nifti_1_header)=%d\n",sizeof(nifti_1_header)) ;
     exit(0) ;
   }

   nim = nifti_image_read( argv[1] , 1 ) ;
   if( nim == NULL ) exit(1) ;
   nifti_image_infodump( nim ) ;
   exit(0) ;
}
