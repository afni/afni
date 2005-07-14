/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

void Syntax(void)
{
   printf(
    "Prints out sort-of-useful information from a 3D dataset's header\n"
    "Usage: 3dinfo [-verb OR -short] dataset [dataset ...]\n"
    "  -verb means to print out lots of stuff\n"
    "  -short means to print out less stuff\n"
   ) ;
   exit(0) ;
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset ;
   int iarg , verbose = 0 ;
   char * outbuf ;

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax() ;

   mainENTRY("3dinfo main") ; machdep() ; PRINT_VERSION("3dinfo") ;

   iarg = 1 ;
        if( strncmp(argv[iarg],"-verb" ,5) == 0 ){ verbose =  1; iarg++; }
   else if( strncmp(argv[iarg],"-short",5) == 0 ){ verbose = -1; iarg++; }

   for( ; iarg < argc ; iarg++ ){
#if 0
      dset = THD_open_one_dataset( argv[iarg] ) ;
#else
      dset = THD_open_dataset( argv[iarg] ) ;
#endif
      if( dset == NULL ){
         printf("\nCan't open dataset %s\n",argv[iarg]) ;
         continue ;
      }

      outbuf = THD_dataset_info( dset , verbose ) ;
      if( outbuf != NULL ){
         printf("\n") ;
         puts(outbuf) ;
         free(outbuf) ; outbuf = NULL ;
      } else {
         printf("\nCan't get info for dataset %s\n",argv[iarg]) ;
      }

      THD_delete_3dim_dataset( dset , False ) ;
   }
   exit(0) ;
}
