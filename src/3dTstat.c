#include "mrilib.h"

/*------- Adapted from plug_stats.c ---------*/

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#define METH_MEAN  0
#define METH_SLOPE 1
#define METH_SIGMA 2
#define METH_CVAR  3

static int meth                    = METH_MEAN ;
static char prefix[THD_MAX_PREFIX] = "stat" ;
static int datum                   = MRI_float ;

static void STATS_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void * ud , float * val ) ;

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   int nopt ;

   /*----- Read command line -----*/

   if( argc < 2 ){
      printf("Usage: 3dTstat [options] dataset\n"
             "Computes a voxel-wise statistic for a 3D+time dataset\n"
             "\n"
             "Options:\n"
             " -mean   = compute mean of input voxels [DEFAULT]\n"
             " -slope  = compute mean slope of input voxels vs. time\n"
             " -stdev  = compute standard deviation of input voxels\n"
             "             [N.B.: this is computed after    ]\n"
             "             [      the slope has been removed]\n"
             " -cvar   = compute coefficient of variation of input\n"
             "             voxels = stdev/fabs(mean)\n"
             "\n"
             " -prefix p = use string 'p' for the prefix of the\n"
             "               output dataset [DEFAULT = 'stat']\n"
             " -datum d  = use data type 'd' for the type of storage\n"
             "               of the output, where 'd' is one of\n"
             "               'byte', 'short', or 'float' [DEFAULT=float]\n"
             "\n"
             "The output is a single sub-brick dataset.  The input dataset\n"
             "may use a sub-brick selection list, as in program 3dcalc.\n"
           ) ;
      exit(0) ;
   }

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /*-- methods --*/

      if( strcmp(argv[nopt],"-mean") == 0 ){
fprintf(stderr,"mean: %s\n",argv[nopt]) ;
         meth = METH_MEAN ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-slope") == 0 ){
fprintf(stderr,"slope: %s\n",argv[nopt]) ;
         meth = METH_SLOPE ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-stdev") == 0 ||
          strcmp(argv[nopt],"-sigma") == 0   ){

fprintf(stderr,"stdev: %s\n",argv[nopt]) ;
         meth = METH_SIGMA ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-cvar") == 0 ){
fprintf(stderr,"cvar: %s\n",argv[nopt]) ;
         meth = METH_CVAR ;
         nopt++ ; continue ;
      }

      /*-- prefix --*/

      if( strcmp(argv[nopt],"-prefix") == 0 ){
fprintf(stderr,"prefix: %s %s\n",argv[nopt],argv[nopt+1]) ;
         if( ++nopt >= argc ){
            fprintf(stderr,"*** -prefix needs an argument!\n"); exit(1);
         }
         MCW_strncpy(prefix,argv[nopt],THD_MAX_PREFIX) ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"*** %s is not a valid prefix!\n",prefix); exit(1);
         }
         nopt++ ; continue ;
      }

      /*-- datum --*/

      if( strcmp(argv[nopt],"-datum") == 0 ){
fprintf(stderr,"datum: %s %s\n",argv[nopt],argv[nopt+1]) ;
         if( ++nopt >= argc ){
            fprintf(stderr,"*** -datum needs an argument!\n"); exit(1);
         }
         if( strcmp(argv[nopt],"short") == 0 ){
            datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            datum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            datum = MRI_byte ;
         } else {
            fprintf(stderr,"-datum of type '%s' is not supported!\n",
                    argv[nopt] ) ;
            exit(1) ;
         }
         nopt++ ; continue ;
      }

      /*-- Quien sabe'? --*/

      fprintf(stderr,"*** Unknown option: %s\n",argv[nopt]) ; exit(1) ;
   }

   /*----- read input dataset -----*/

   if( nopt >= argc ){
      fprintf(stderr,"*** No input dataset!?\n"); exit(1);
   }

   old_dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(old_dset) ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt]); exit(1);
   }

   if( DSET_NVALS(old_dset) < 2 ){
      fprintf(stderr,"*** Can't use dataset with < 2 values per voxel!\n") ;
      exit(1) ;
   }

   if( DSET_NUM_TIMES(old_dset) < 2 ){
      printf("--- Input dataset is not 3D+time!\n"
             "--- Adding an artificial time axis with dt=1.0\n" ) ;
      EDIT_dset_items( old_dset ,
                          ADN_ntt    , DSET_NVALS(old_dset) ,
                          ADN_ttorg  , 0.0 ,
                          ADN_ttdel  , 1.0 ,
                          ADN_tunits , UNITS_SEC_TYPE ,
                       NULL ) ;
   }

   /*------------- ready to compute new dataset -----------*/

   new_dset = MAKER_4D_to_typed_fim(
                 old_dset ,             /* input dataset */
                 prefix ,               /* output prefix */
                 datum ,                /* output datum  */
                 0 ,                    /* ignore count  */
                 1 ,                    /* detrend = ON  */
                 STATS_tsfunc ,         /* timeseries processor */
                 NULL                   /* data for tsfunc */
              ) ;

   if( new_dset != NULL ){
      tross_Copy_History( old_dset , new_dset ) ;
      tross_Make_History( "3dTstat" , argc,argv , new_dset ) ;
      DSET_write( new_dset ) ;
      printf("--- Output dataset %s\n",DSET_FILECODE(new_dset)) ;
   } else {
      fprintf(stderr,"*** Unable to compute output dataset!\n") ;
      exit(1) ;
   }

   exit(0) ;
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

static void STATS_tsfunc( double tzero , double tdelta ,
                   int npts , float ts[] , double ts_mean , double ts_slope ,
                   void * ud , float * val )
{
   static int nvox , ncall ;

   /** is this a "notification"? **/

   if( val == NULL ){

      if( npts > 0 ){  /* the "start notification" */

         nvox  = npts ;                       /* keep track of   */
         ncall = 0 ;                          /* number of calls */

      } else {  /* the "end notification" */

         /* nothing to do here */

      }
      return ;
   }

   /** OK, actually do some work **/

   switch( meth ){

      default:
      case METH_MEAN:  *val = ts_mean  ; break ;

      case METH_SLOPE: *val = ts_slope ; break ;

      case METH_CVAR:
      case METH_SIGMA:{
         register int ii ;
         register double sum ;

         sum = 0.0 ;
         for( ii=0 ; ii < npts ; ii++ ) sum += ts[ii] * ts[ii] ;

         sum = sqrt( sum/(npts-1) ) ;

         if( meth == METH_SIGMA )  *val = sum ;
         else if( ts_mean != 0.0 ) *val = sum / fabs(ts_mean) ;
         else                      *val = 0.0 ;
      }
   }

   ncall++ ; return ;
}
