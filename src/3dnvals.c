#include "mrilib.h"

void Syntax(void)
{
   printf(
    "Prints out the number of sub-bricks in a 3D dataset\n"
    "Usage: 3dnvals dataset\n"
   ) ;
   exit(0) ;
}

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset ;
   int iarg , verbose = 0 ;

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax() ;

   iarg = 1 ;
   if( strcmp(argv[iarg],"-v") == 0 ){ verbose = 1 ; iarg++ ; }

   for( ; iarg < argc ; iarg++ ){
      dset = THD_open_one_dataset( argv[iarg] ) ;
      if( dset == NULL ){
         printf("-1\n") ;
         continue ;
      }
      printf("%d\n",DSET_NVALS(dset)) ;

      THD_delete_3dim_dataset( dset , False ) ;
   }
   exit(0) ;
}
