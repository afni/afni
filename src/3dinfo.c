#include "mrilib.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

void Syntax(void)
{
   printf(
    "Copyright 1994-6 Medical College of Wisconsin\n\n"
    "Prints out sort-of-useful information from a 3D dataset's header\n"
    "Usage: 3dinfo [-v] dataset [dataset ...]\n"
    "  The -v option means print out verbose information.  At present,\n"
    "  it just causes the printing of all the statistics for each time\n"
    "  in a time-dependent dataset.\n"
   ) ;
   exit(0) ;
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset ;
   int iarg , verbose = 0 ;
   char * outbuf ;

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax() ;

   iarg = 1 ;
   if( strcmp(argv[iarg],"-v") == 0 ){ verbose = 1 ; iarg++ ; }

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
