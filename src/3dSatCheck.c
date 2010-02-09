#include "mrilib.h"

#ifdef USE_OMP
#include "thd_satcheck.c"
#endif

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ; int aa ;
   float val ;

   if( argc < 2 ){
     printf("Usage: 3dSatCheck dataset [...]\n") ; exit(0) ;
   }
   for( aa=1 ; aa < argc ; aa++ ){
     dset = THD_open_dataset( argv[aa] ) ; CHECK_OPEN_ERROR(dset,argv[aa]) ;
     if( DSET_NVALS(dset) < 9 ) continue ;
     DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
     val = THD_saturation_check( dset , NULL ) ;
     INFO_message("%40.40s  satcheck = %.3f",argv[aa],val) ;
     DSET_delete(dset) ;
   }
   exit(0) ;
}
