#include "mrilib.h"

static char prefix[THD_MAX_PREFIX] = "tnorm" ;
static int datum                   = MRI_float ;
static int mode                    =  2 ;
static int polort                  = -1 ;
static int dmode                   =  2 ;

static void NORM_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *old_dset , *new_dset ;  /* input and output datasets */
   int nopt, ii , nvals ;

   /*----- Help the blessedly ignorant user? -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 3dTnorm [options] dataset\n"
            "Takes each voxel time series and normalizes it\n"
            "(by multiplicative scaling) -- in some sense.\n"
            "\n"
            "Options:\n"
            " -prefix p = use string 'p' for the prefix of the\n"
            "               output dataset [DEFAULT = 'tnorm']\n"
            " -norm2    = L2 normalize (sum of squares = 1) [DEFAULT]\n"
            " -normR    = normalize so sum of squares = number of time points\n"
            "             * e.g., so RMS = 1.\n"
            " -norm1    = L1 normalize (sum of absolute values = 1)\n"
            " -normx    = Scale so max absolute value = 1 (L_infinity norm)\n"
            " -polort p = Detrend with polynomials of order p before normalizing\n"
            "               [DEFAULT = don't do this]\n"
            "             * Use '-polort 0' to remove the mean, for example\n"
            " -L1fit    = Detrend with L1 regression (L2 is the default)\n"
            "             * This option is here just for the hell of it\n"
            "\n"
            "Notes:\n"
            "* Each voxel is processed separately\n"
            "* A voxel that is all zero will be unchanged (duh)\n"
            "* Output dataset is in float format, no matter what the input format\n"
            "* This program is for producing regressors to use in 3dTfitter\n"
            "* Also see programs 1dnorm and 3dcalc\n"
          ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /* bureaucracy */

   mainENTRY("3dTnorm main"); machdep(); AFNI_logger("3dTnorm",argc,argv);
   PRINT_VERSION("3dTnorm"); AUTHOR("RW Cox");

   /*--- scan command line for options ---*/

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     /*-- prefix --*/

     if( strcmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("-prefix needs an argument!");
       MCW_strncpy(prefix,argv[nopt],THD_MAX_PREFIX) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("%s is not a valid prefix!",prefix);
       nopt++ ; continue ;
     }

     if( strncmp(argv[nopt],"-norm",5) == 0 ){
            if( argv[nopt][5] == '1' ) mode = 1 ;
       else if( argv[nopt][5] == 'x' ) mode = 666 ;
       else if( argv[nopt][5] == '2' ) mode = 2 ;
       else if( argv[nopt][5] == 'R' ) mode = 3 ;
       else ERROR_message("Don't understand option '%s'",argv[nopt]) ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-polort") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("-polort needs an argument!");
       polort = (int)strtod(argv[nopt],NULL) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-L1fit") == 0 ){
       dmode = 1 ; nopt++ ; continue ;
     }
     if( strcasecmp(argv[nopt],"-L2fit") == 0 ){
       dmode = 2 ; nopt++ ; continue ;
     }

     /*-- Quien sabe'? --*/

     ERROR_message("Unknown option: %s",argv[nopt]) ;
     suggest_best_prog_option(argv[0],argv[nopt]);
     exit(1);
   }

   /*----- read input dataset -----*/

   if( nopt >= argc ) ERROR_exit(" No input dataset!?") ;

   old_dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(old_dset) )
     ERROR_exit("Can't open dataset %s",argv[nopt]);
   DSET_load(old_dset) ;
   if( !DSET_LOADED(old_dset) )
     ERROR_exit("Can't load dataset %s",argv[nopt]) ;

   nvals = DSET_NVALS(old_dset) ;
   if( nvals < 2 || nvals <= polort )
     ERROR_exit("Need at least %d time points, but have only %d",
                MAX(2,polort+1) , nvals ) ;

   nopt++ ;
   if( nopt < argc )
     WARNING_message("Trailing inputs on command line ignored: %s ...",argv[nopt]) ;

   /*------------- ready to compute new dataset -----------*/

   new_dset = MAKER_4D_to_typed_fbuc(
                    old_dset ,             /* input dataset */
                    prefix ,               /* output prefix */
                    datum ,                /* output datum  */
                    0 ,                    /* ignore count  */
                    0 ,                    /* don't detrend */
                    nvals ,                /* number of briks in output */
                    NORM_tsfunc ,          /* timeseries processor */
                    NULL,                  /* data for tsfunc */
                    NULL,                  /* mask */
                    0   /* Allow auto scaling of output */
                 ) ;

   if( new_dset != NULL ){
     tross_Copy_History( old_dset , new_dset ) ;
     tross_Make_History( "3dTnorm" , argc,argv , new_dset ) ;
     EDIT_dset_items( new_dset ,
                         ADN_ntt    , nvals ,
                         ADN_ttorg  , DSET_TIMEORIGIN(old_dset) ,
                         ADN_ttdel  , DSET_TR(old_dset) ,
                         ADN_tunits , UNITS_SEC_TYPE ,
                       NULL ) ;
     DSET_write(new_dset) ; WROTE_DSET(new_dset) ;
   } else {
     ERROR_exit("Unable to compute output dataset for some unknowable reason :-(") ;
   }

   exit(0) ;
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

static void NORM_tsfunc( double tzero, double tdelta ,
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

   if( polort >= 0 ){
     switch( dmode ){
       default:
       case 2: THD_generic_detrend_LSQ( nval,val, polort, 0,NULL,NULL ); break;
       case 1: THD_generic_detrend_L1 ( nval,val, polort, 0,NULL,NULL ); break;
     }
   }

   switch( mode ){
     default:
     case 2:   THD_normalize( nval,val ) ; break ;
     case 1:   THD_normL1   ( nval,val ) ; break ;
     case 3:   THD_normRMS  ( nval,val ) ; break ;
     case 666: THD_normmax  ( nval,val ) ; break ;
   }

   return ;
}
