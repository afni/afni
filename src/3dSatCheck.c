#include "mrilib.h"

#ifdef USE_OMP
#include "thd_satcheck.c"
#endif

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   float val ;

   if( argc < 2 ){
     printf("Usage: 3dSatCheck dataset\n") ; exit(0) ;
   }
   dset = THD_open_dataset( argv[1] ) ; CHECK_OPEN_ERROR(dset,argv[1]) ;
   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
   val = THD_saturation_check( dset , NULL ) ;
   INFO_message("satcheck = %.3f",val) ;
   exit(0) ;
}
