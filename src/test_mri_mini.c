
#include "mrilib.h"

/*-----------------------------------------------------------------------
 * test_mri_mini - a *really* detailed program for testing libmri_mini.so
 *-----------------------------------------------------------------------
*/

int main( int argc, char * argv[] )
{
   /* check first arg for -help */
   if( argc > 1 && !strncmp(argv[1], "-h", 2)) {
      printf("usage: test_mri_mini\n");
      return 0;
   }

   INFO_message("hello: have %d arg(s)\n", argc-1);
   return 0;
}
