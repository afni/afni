#include "mrilib.h"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *mset=NULL , *iset=NULL , *oset=NULL ;
   char *prefix = "mthresh" ;
   int nopt , ii ;
   int nnlev,nnsid,nzthr ; float *zthr=NULL ;

   /*----- help, I'm trapped in an instance of vi and can't get out -----*/

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){
     printf("\n"
      "Program to apply a multi-threshold (mthresh) dataset\n"
      "to an input dataset.\n"
      "\n"
      "Usage:\n"
      "  3dMultiThresh OPTIONS\n"
      "\n"
      "OPTIONS (in any order)\n"
      "----------------------\n"
      "\n"
      " -mthresh mmm    = multi-threshold dataset from 3dClustSimX\n"
      " -input   ddd    = dataset to threshold\n"
      " -prefix  ppp    = prefix for output dataset\n"
     ) ;
     exit(0) ;
   }

   /*----- scan options -----*/

   nopt = 1 ;

   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcasecmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc )
         ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       prefix = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("prefix '%s' is illegal (and funny looking)",prefix) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-input") == 0 ){
       if( iset != NULL )
         ERROR_exit("you can't use '-input' twice!") ;
       if( ++nopt >= argc )
         ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       iset = THD_open_dataset(argv[nopt]) ;
       if( iset == NULL )
         ERROR_exit("can't open -input dataset '%s'",argv[nopt]) ;
       DSET_load(iset) ; CHECK_LOAD_ERROR(iset) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-mthresh") == 0 ){
       ATR_float *atr ; float *afl ;
       if( mset != NULL )
         ERROR_exit("you can't use '-mthresh' twice!") ;
       if( ++nopt >= argc )
         ERROR_exit("need 1 argument after option '%s'",argv[nopt-1]) ;
       mset = THD_open_dataset(argv[nopt]) ;
       if( mset == NULL )
         ERROR_exit("can't open -mthresh dataset '%s'",argv[nopt]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;

       atr = THD_find_float_atr( mset->dblk , "MULTI_THRESHOLDS" ) ;
       if( atr == NULL )
         ERROR_exit("-mthresh dataset does not have MULTI_THRESHOLDS attribute :(") ;
       afl = atr->fl ;

       nnlev = (int)afl[0] ;
       nnsid = (int)afl[1] ;
       nzthr = (int)afl[2] ;
       zthr  = (float *)malloc(sizeof(float)*nzthr) ;
       for( ii=0 ; ii < nzthr ; ii++ ) zthr[ii] = afl[3+ii] ;

         INFO_message("-mthresh dataset parameters") ;
       ININFO_message("  clustering NN=%d  thresholding=%d-sided  %d thresholds",
                      nnlev,nnsid,nzthr ) ;

       nopt++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[nopt]) ;
   }

   /*-- check for errors --*/

   if( iset == NULL ) ERROR_exit("-input is a mandatory option") ;
   if( mset == NULL ) ERROR_exit("-mthresh is a mandatory option") ;

   /*----- do the work (oog) -----*/

   exit(0) ;
}
