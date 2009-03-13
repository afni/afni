#include "mrilib.h"

/*------- Adapted from 3dTstat.c --------*/

static char prefix[THD_MAX_PREFIX] = "tsort" ;
static int datum                   = MRI_float ;
static int inc                     = 1 ;

static void SORTS_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;
static void SORTS_itsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;
static void SORTS_rtsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;
extern int *z_iqsort (float *x , int nx );

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *old_dset , *new_dset ;  /* input and output datasets */
   int nopt, ii , nvals , rank;

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
             " -rank     = output rank instead of sorted values\n"
             "             ranks range from 1 to Nvals\n"
             " -ind      = output sorting index. (0 to Nvals -1)\n"
             "             See example below.\n"
             " -val      = output sorted values (default)\n"
             " -datum D  = Coerce the output data to be stored as \n"
             "             the given type D, which may be  \n"
             "             byte, short, or float (default).         \n"
             "\n"
             "Notes:\n"
             "* Each voxel is sorted separately.\n"
             "* Sub-brick labels are not rearranged.\n"
             "* This program is useful only in limited cases.\n"
             "   It was written to sort the -stim_times_IM\n"
             "   beta weights output by 3dDeconvolve.\n"
             "* Also see program 1dTsort.\n"
             "\n"
             "Examples:\n"
             "setenv AFNI_1D_TIME YES\n"
             "echo '8 6 3 9 2 7' > test.1D\n"
             "    3dTsort -overwrite test.1D \n"
             "    1dcat tsort.1D\n"
             "\n"
             "    3dTsort -overwrite -rank test.1D \n"
             "    1dcat tsort.1D\n"
             "\n"
             "\n"
             "    3dTsort -overwrite -ind test.1D \n"
             "    1dcat tsort.1D\n"
             "\n"
             "    3dTsort -overwrite -dec test.1D \n"
             "    1dcat tsort.1D\n"
             "\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /* bureaucracy */

   mainENTRY("3dTsort main"); machdep(); AFNI_logger("3dTsort",argc,argv);
   PRINT_VERSION("3dTsort"); AUTHOR("RW Cox");

   /*--- scan command line for options ---*/

   nopt = 1 ;
   rank = 0;
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
      if( strncmp(argv[nopt],"-rank",5) == 0){
         rank = 1; nopt++; continue;
      }
      if( strncmp(argv[nopt],"-val",4) == 0){
         rank = 0; nopt++; continue;
      }
      if( strncmp(argv[nopt],"-ind",5) == 0){
         rank = -1; nopt++; continue;
      }
      if( strncasecmp(argv[nopt],"-datum",6) == 0 ){
         if( ++nopt >= argc )
           ERROR_exit("need an argument after -datum!\n") ;
         if( strcasecmp(argv[nopt],"short") == 0 ){
            datum = MRI_short ;
         } else if( strcasecmp(argv[nopt],"float") == 0 ){
            datum = MRI_float ;
         } else if( strcasecmp(argv[nopt],"byte") == 0 ){
            datum = MRI_byte ;
         } else {
            ERROR_exit( "-datum of type '%s' not supported "
                        "in 3dTsort!\n",argv[nopt]) ;
         }
         nopt++; continue;
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
   if (DSET_NUM_TIMES(old_dset)<2) {
      ERROR_exit( "Need at least 2 time points in series.\n"
                  "Have only %d\n"
                  "If using mutli-column 1D files, use\n"
                  "  setenv AFNI_1D_TIME YES\n"
                  , DSET_NUM_TIMES(old_dset));
   }
   nopt++ ;
   if( nopt < argc )
     WARNING_message("Trailing inputs on command line ignored: %s ...",argv[nopt]) ;

   nvals = DSET_NVALS(old_dset) ;
   if( nvals < 2 )
     ERROR_exit("Can't use dataset with < 2 values per voxel!\n") ;

   /*------------- ready to compute new dataset -----------*/ 

   if (!rank) {
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
   } else if (rank == 1) {
      new_dset = MAKER_4D_to_typed_fbuc(
                    old_dset ,             /* input dataset */
                    prefix ,               /* output prefix */
                    datum ,                /* output datum  */
                    0 ,                    /* ignore count  */
                    0 ,                    /* don't detrend */
                    nvals ,                /* number of briks */
                    SORTS_itsfunc ,        /* timeseries processor */
                    NULL                   /* data for tsfunc */
                 ) ;
   } else {
      new_dset = MAKER_4D_to_typed_fbuc(
                    old_dset ,             /* input dataset */
                    prefix ,               /* output prefix */
                    datum ,                /* output datum  */
                    0 ,                    /* ignore count  */
                    0 ,                    /* don't detrend */
                    nvals ,                /* number of briks */
                    SORTS_rtsfunc ,        /* timeseries processor */
                    NULL                   /* data for tsfunc */
                 ) ;
   } 
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
static void SORTS_itsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean, double ts_slope,
                          void *ud, int nbriks, float *val )
{
   int ii , nval ;
   int *rnk;
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
   
   /* Using an inverse sorting function, for excitement */
#if 0 /* the dumber way */
   for( ii=0 ; ii < nval ; ii++ ) val[ii] = -val[ii] ;
   rnk = z_iqsort( val, nval ) ;
   if( inc == 1 ){
     for( ii=0 ; ii < nval ; ii++ )  val[ii] = rnk[ii] +1 ;
   }else {
      for( ii=0 ; ii < nval ; ii++ ) val[ii] = nval - rnk[ii] ;
   }
#else /* the dumb way */
   rnk = z_iqsort( val, nval ) ;
   if( inc == 1 ){
     for( ii=0 ; ii < nval ; ii++ )  val[rnk[ii]] = nval - ii ;
   }else {
      for( ii=0 ; ii < nval ; ii++ ) val[rnk[ii]] = ii+1 ;
   }
#endif
   free(rnk); rnk=NULL;
   return ;
}
static void SORTS_rtsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean, double ts_slope,
                          void *ud, int nbriks, float *val )
{
   int ii , nval ;
   int *rnk;
   /** is this a "notification"? **/

   if( val == NULL ){
      return ;
   }

   /** do the work **/

   nval = MIN(nbriks,npts) ;
   memcpy( val , ts , sizeof(float)*nval ) ;
   
   /* Using an inverse sorting function, for excitement */
   rnk = z_iqsort( val, nval ) ;
   if( inc == 1 ){
     for( ii=0 ; ii < nval ; ii++ )  val[nval - ii-1] =  rnk[ii];
   }else {
      for( ii=0 ; ii < nval ; ii++ ) val[ii] = rnk[ii] ;
   }
   free(rnk); rnk=NULL;
   return ;
}
