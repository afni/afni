#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int iarg=1 ;
   THD_3dim_dataset * dset ;
   double eset ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dEntropy dataset ...\n") ; exit(0) ;
   }

   for( ; iarg < argc ; iarg++ ){
      dset = THD_open_dataset( argv[iarg] ) ;
      if( !ISVALID_DSET(dset) ){
         printf("%s: Can't open\n",argv[iarg]) ; continue ;
      }
      eset = ENTROPY_dataset(dset) ;
      printf("%s: %g\n",argv[iarg],eset) ;
      DSET_delete(dset) ;
   }
   exit(0) ;
}
