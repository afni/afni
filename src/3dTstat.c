/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*------- Adapted from plug_stats.c ---------*/

#define METH_MEAN   0
#define METH_SLOPE  1
#define METH_SIGMA  2
#define METH_CVAR   3

#define METH_MEDIAN 4   /* 14 Feb 2001 */
#define METH_MAD    5

#define METH_MAX    6
#define METH_MIN    7

static int meth                    = METH_MEAN ;
static char prefix[THD_MAX_PREFIX] = "stat" ;
static int datum                   = MRI_float ;
static int detrend                 = 1 ;

static void STATS_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void * ud , float * val ) ;

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   int nopt ;

   /*----- Read command line -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTstat [options] dataset\n"
             "Computes a single voxel-wise statistic for a 3D+time dataset\n"
             "(if you want more than one of these, run 3dTstat more than once).\n"
             "\n"
             "Options:\n"
             " -mean   = compute mean of input voxels [DEFAULT]\n"
             " -slope  = compute mean slope of input voxels vs. time\n"
             " -stdev  = compute standard deviation of input voxels\n"
             "             [N.B.: this is computed after    ]\n"
             "             [      the slope has been removed]\n"
             " -cvar   = compute coefficient of variation of input\n"
             "             voxels = stdev/fabs(mean)\n"
             "   **N.B.: You can add NOD to the end of the above 2\n"
             "           options to turn off detrending, as in\n"
             "             -stdevNOD or -cvarNOD\n"
             "\n"
             " -MAD    = compute MAD (median absolute deviation) of\n"
             "             input voxels = median(|voxel-median(voxel)|)\n"
             "             [N.B.: the trend is NOT removed for this]\n"
             " -median = compute median of input voxels  [undetrended]\n"
             " -min    = compute minimum of input voxels [undetrended]\n"
             " -max    = compute maximum of input voxels [undetrended]\n"
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
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      exit(0) ;
   }

   mainENTRY("3dTstat main"); machdep(); AFNI_logger("3dTstat",argc,argv);

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /*-- methods --*/

      if( strcmp(argv[nopt],"-median") == 0 ){
         meth = METH_MEDIAN ; detrend = 0 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-MAD") == 0 ){
         meth = METH_MAD ; detrend = 0 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-mean") == 0 ){
         meth = METH_MEAN ; detrend = 1 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-slope") == 0 ){
         meth = METH_SLOPE ; detrend = 1 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-stdev") == 0 ||
          strcmp(argv[nopt],"-sigma") == 0   ){

         meth = METH_SIGMA ; detrend = 1 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-cvar") == 0 ){
         meth = METH_CVAR ; detrend = 1 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-stdevNOD") == 0 ||
          strcmp(argv[nopt],"-sigmaNOD") == 0   ){  /* 07 Dec 2001 */

         meth = METH_SIGMA ; detrend = 0 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-cvarNOD") == 0 ){     /* 07 Dec 2001 */
         meth = METH_CVAR ; detrend = 0 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-min") == 0 ){
         meth = METH_MIN ; detrend = 0 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-max") == 0 ){
         meth = METH_MAX ; detrend = 0 ;
         nopt++ ; continue ;
      }

      /*-- prefix --*/

      if( strcmp(argv[nopt],"-prefix") == 0 ){
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
                 detrend ,              /* detrending?   */
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

static void STATS_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean, double ts_slope,
                          void * ud, float * val          )
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
         if( detrend ){
           for( ii=0 ; ii < npts ; ii++ ) sum += ts[ii] * ts[ii] ;
         } else {
           for( ii=0 ; ii < npts ; ii++ ) sum += (ts[ii]-ts_mean)
                                                *(ts[ii]-ts_mean) ;
         }

         sum = sqrt( sum/(npts-1) ) ;

         if( meth == METH_SIGMA )  *val = sum ;
         else if( ts_mean != 0.0 ) *val = sum / fabs(ts_mean) ;
         else                      *val = 0.0 ;
      }
      break ;

      /* 14 Feb 2000: these 2 new methods disturb the array ts[] */

      case METH_MEDIAN:
         *val = qmed_float( npts , ts ) ;
      break ;

      case METH_MAD:{
         register int ii ;
         register float vm ;
         vm = qmed_float( npts , ts ) ;
         for( ii=0 ; ii < npts ; ii++ ) ts[ii] = fabs(ts[ii]-vm) ;
         *val = qmed_float( npts , ts ) ;
      }
      break ;

      case METH_MIN:{
         register int ii ;
         register float vm=ts[0] ;
         for( ii=1 ; ii < npts ; ii++ ) if( ts[ii] < vm ) vm = ts[ii] ;
         *val = vm ;
      }
      break ;

      case METH_MAX:{
         register int ii ;
         register float vm=ts[0] ;
         for( ii=1 ; ii < npts ; ii++ ) if( ts[ii] > vm ) vm = ts[ii] ;
         *val = vm ;
      }
      break ;
   }

   ncall++ ; return ;
}
