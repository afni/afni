#include "mrilib.h"

int main( int argc , char * argv )
{
   int bb = mri_short_order() ;

   if( bb == LSB_FIRST )
      printf("CPU byte order = LSB_FIRST\n") ;
   else
      printf("CPU byte order = MSB_FIRST\n") ;
   exit(0) ;
}
