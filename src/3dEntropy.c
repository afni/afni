#include "mrilib.h"

#include "thd_entropy16.c"

int main( int argc , char * argv[] )
{
   int iarg=1 ;
   THD_3dim_dataset * dset ;
   double eset ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("\n") ;
      printf("Usage: 3dEntropy [-zskip] dataset ...\n\n") ;
      printf(" * Datasets must be stored as 16 bit shorts.\n") ;
      printf(" * -zskip option means to skip 0 values in the computation.\n") ;
      printf(" * This program is not very useful :) :(\n") ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   while( iarg < argc && argv[iarg][0] == '-' ){
     if( strcasecmp(argv[iarg],"-zskip") == 0 ){
       do_zskip++ ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-perbin") == 0 ){
       do_perbin = 1 ; do_permax = 0 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-permax") == 0 ){
       do_perbin = 0 ; do_permax = 1 ; iarg++ ; continue ;
     }
     ERROR_exit("Unknown option '%s'") ; exit(1) ;
   }

   for( ; iarg < argc ; iarg++ ){
      dset = THD_open_dataset( argv[iarg] ) ; CHECK_OPEN_ERROR(dset,argv[iarg]) ;
      eset = ENTROPY_dataset(dset) ;
      printf("%s: %g\n",argv[iarg],eset) ;
      DSET_delete(dset) ;
   }
   exit(0) ;
}
