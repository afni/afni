/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

void Syntax(void)
{
   printf(
    "Prints out the number of sub-bricks in a 3D dataset\n"
    "Usage: 3dnvals [-verb] dataset\n"
   ) ;
   PRINT_COMPILE_DATE ; exit(0) ;
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset ;
   int iarg , verbose = 0 ;

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax() ;

   iarg = 1 ;
   if( strncmp(argv[iarg],"-verb",5) == 0 ){ verbose = 1 ; iarg++ ; }

   for( ; iarg < argc ; iarg++ ){
      dset = THD_open_dataset( argv[iarg] ) ;
      if( dset == NULL ){
         printf("-1\n") ;
         continue ;
      }
      printf("%d\n",DSET_NVALS(dset)) ;

      THD_delete_3dim_dataset( dset , False ) ;
   }
   exit(0) ;
}
