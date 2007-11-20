#include "mrilib.h"

/*------- Adapted from 3dTstat.c --------*/

static char prefix[THD_MAX_PREFIX] = "tsort" ;
static int datum                   = MRI_float ;
static int inc                     = 1 ;

static void SORTS_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *old_dset , *new_dset ;  /* input and output datasets */
   int nopt, ii , nvals ;

   /*----- Help the pitiful user? -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTsort [options] dataset\n"
             "Sorts each voxel and produces a new dataset.\n"
             "\n"
             "Options:\n"
             " -prefix p = use string 'p' for the prefix of the\n"
             "               output dataset [DEFAULT = 'tsort']\n"
             " -inc      = sort into increasing order [default]\n"
             " -dec      = sort into decreasing order\n"
           ) ;
      exit(0) ;
   }

   /* bureaucracy */

   mainENTRY("3dTsort main"); machdep(); AFNI_logger("3dTsort",argc,argv);
   PRINT_VERSION("3dTsort"); AUTHOR("RW Cox");

   /*--- scan command line for options ---*/

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /*-- prefix --*/

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("-prefix needs an argument!\n");
         MCW_strncpy(prefix,argv[nopt],THD_MAX_PREFIX) ;
         if( !THD_filename_ok(prefix) )
           ERROR_exit("%s is not a valid prefix!\n",prefix);
         nopt++ ; continue ;
      }

      /*-- -inc or -dec --*/

      if( strncmp(argv[nopt],"-inc",4) == 0 ){
        inc = 1 ; nopt++ ; continue ;
      }
      if( strncmp(argv[nopt],"-dec",4) == 0 ){
        inc = 0 ; nopt++ ; continue ;
      }

      /*-- Quien sabe'? --*/

      ERROR_exit("Unknown option: %s\n",argv[nopt]) ;
   }

   /*----- read input dataset -----*/

   if( nopt >= argc ) ERROR_exit(" No input dataset!?") ;

   old_dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(old_dset) )
     ERROR_exit("Can't open dataset %s\n",argv[nopt]);
   DSET_load(old_dset) ;
   if( !DSET_LOADED(old_dset) )
     ERROR_exit("Can't load dataset %s\n",argv[nopt]) ;

   nopt++ ;
   if( nopt < argc )
     WARNING_message("Trailing inputs on command line ignored: %s ...",argv[nopt]) ;

   nvals = DSET_NVALS(old_dset) ;
   if( nvals < 2 )
     ERROR_exit("Can't use dataset with < 2 values per voxel!\n") ;

   /*------------- ready to compute new dataset -----------*/

   new_dset = MAKER_4D_to_typed_fbuc(
                 old_dset ,             /* input dataset */
                 prefix ,               /* output prefix */
                 datum ,                /* output datum  */
                 0 ,                    /* ignore count  */
                 0 ,                    /* don't detrend */
                 nvals ,                /* number of briks */
                 SORTS_tsfunc ,         /* timeseries processor */
                 NULL                   /* data for tsfunc */
              ) ;

   if( new_dset != NULL ){
     tross_Copy_History( old_dset , new_dset ) ;
     tross_Make_History( "3dTsort" , argc,argv , new_dset ) ;
     if( DSET_NUM_TIMES(old_dset) > 1 )
       EDIT_dset_items( new_dset ,
                         ADN_ntt    , DSET_NVALS(old_dset) ,
                         ADN_ttorg  , DSET_TIMEORIGIN(old_dset) ,
                         ADN_ttdel  , DSET_TR(old_dset) ,
                         ADN_tunits , UNITS_SEC_TYPE ,
                       NULL ) ;
     DSET_write( new_dset ) ; WROTE_DSET( new_dset ) ;
   } else {
     ERROR_exit("Unable to compute output dataset!\n") ;
   }

   exit(0) ;
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

static void SORTS_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean, double ts_slope,
                          void *ud, int nbriks, float *val )
{
   int ii , nval ;

   /** is this a "notification"? **/

   if( val == NULL ){
#if 0
      if( npts > 0 ){  /* the "start notification" */
      } else {  /* the "end notification" */
      }
#endif
      return ;
   }

   /** do the work **/

   nval = MIN(nbriks,npts) ;
   memcpy( val , ts , sizeof(float)*nval ) ;
   if( inc == 0 ){
     for( ii=0 ; ii < nval ; ii++ ) val[ii] = -val[ii] ;
   }
   qsort_float( nval , val ) ;
   if( inc == 0 ){
     for( ii=0 ; ii < nval ; ii++ ) val[ii] = -val[ii] ;
   }
   return ;
}
