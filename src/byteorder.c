/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
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
