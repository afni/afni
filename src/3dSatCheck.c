#include "mrilib.h"

#ifdef USE_OMP
#include "thd_satcheck.c"
#endif

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ; int aa,ll ; char *cpt ; float val ;

   if( argc < 2 ){
     printf("Usage: 3dSatCheck dataset [...]\n"
            "\n"
            "Prints the 'raw' initial transient (saturation) check\n"
            "value for each dataset on the command line.  Round this\n"
            "number to the nearest integer to get an estimate of\n"
            "how many non-saturated time points start a dataset.\n"
           ) ;
     exit(0) ;
   }
   for( aa=1 ; aa < argc ; aa++ ){
     dset = THD_open_dataset( argv[aa] ) ; if( !ISVALID_DSET(dset) ) continue ;
     if( DSET_NVALS(dset) < 9 ) continue ;
     DSET_load(dset) ; if( !DSET_LOADED(dset) ) continue ;
     val = THD_saturation_check( dset , NULL ) ;
     ll = strlen(argv[aa]) ;
     cpt = (ll <= 50) ? argv[aa] : argv[aa]+(ll-50) ;
     INFO_message("%-50.50s = %.3f",cpt,val) ;
     DSET_delete(dset) ;
   }
   exit(0) ;
}
