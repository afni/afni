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

#define METH_DW     8   /* KRH 3 Dec 2002 */

#define METH_SIGMA_NOD     9   /* KRH 27 Dec 2002 */
#define METH_CVAR_NOD     10   /* KRH 27 Dec 2002 */

#define MAX_NUM_OF_METHS 20
static int meth[MAX_NUM_OF_METHS] = {METH_MEAN};
static char prefix[THD_MAX_PREFIX] = "stat" ;
static int datum                   = MRI_float ;
static char meth_names[][20] = {"Mean","Slope","Std Dev","Coef of Var","Median",
	                    "Med Abs Dev", "Max", "Min", "Durbin-Watson", "Std Dev (NOD)",
                            "Coef Var(NOD)"};
static void STATS_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void * ud , int nbriks, float * val ) ;

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   int nopt, nbriks, ii ;

   /*----- Read command line -----*/
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTstat [options] dataset\n"
             "Computes one or more voxel-wise statistics for a 3D+time dataset\n"
             "and stores them in a bucket dataset.\n"
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
             " -DW    = compute Durbin-Watson Statistic of\n"
             "             input voxels\n"
             "             [N.B.: the trend is removed for this]\n"
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
             "The output is a bucket dataset.  The input dataset\n"
             "may use a sub-brick selection list, as in program 3dcalc.\n"
           ) ;
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      exit(0) ;
   }

   mainENTRY("3dTstat main"); machdep(); AFNI_logger("3dTstat",argc,argv);

   nopt = 1 ;
   nbriks = 0 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /*-- methods --*/

      if( strcmp(argv[nopt],"-median") == 0 ){
         meth[nbriks++] = METH_MEDIAN ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-DW") == 0 ){
         meth[nbriks++] = METH_DW ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-MAD") == 0 ){
         meth[nbriks++] = METH_MAD ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-mean") == 0 ){
         meth[nbriks++] = METH_MEAN ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-slope") == 0 ){
         meth[nbriks++] = METH_SLOPE ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-stdev") == 0 ||
          strcmp(argv[nopt],"-sigma") == 0   ){

         meth[nbriks++] = METH_SIGMA ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-cvar") == 0 ){
         meth[nbriks++] = METH_CVAR ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-stdevNOD") == 0 ||
          strcmp(argv[nopt],"-sigmaNOD") == 0   ){  /* 07 Dec 2001 */

         meth[nbriks++] = METH_SIGMA_NOD ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-cvarNOD") == 0 ){     /* 07 Dec 2001 */
         meth[nbriks++] = METH_CVAR_NOD ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-min") == 0 ){
         meth[nbriks++] = METH_MIN ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-max") == 0 ){
         meth[nbriks++] = METH_MAX ;
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

   /*--- If no options selected, default to single stat MEAN--KRH---*/

   if (nbriks == 0) nbriks = 1;

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

   new_dset = MAKER_4D_to_typed_fbuc(
                 old_dset ,             /* input dataset */
                 prefix ,               /* output prefix */
                 datum ,                /* output datum  */
                 0 ,                    /* ignore count  */
                 0 ,              /* can't detrend in maker function  KRH 12/02*/
                 nbriks ,               /* number of briks */
                 STATS_tsfunc ,         /* timeseries processor */
                 NULL                   /* data for tsfunc */
              ) ;

   if( new_dset != NULL ){
      tross_Copy_History( old_dset , new_dset ) ;
      tross_Make_History( "3dTstat" , argc,argv , new_dset ) ;
      for (ii = 0; ii < nbriks; ii++) {
        EDIT_BRICK_LABEL(new_dset, ii, meth_names[meth[ii]]) ;
      }
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
                          void * ud, int nbriks, float * val          )
{
   static int nvox , ncall ;
   int meth_index, ii ;
   float* ts_det;

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


   /* KRH make copy and detrend it right here.  Use ts_mean and
    * ts_slope to detrend */

   ts_det = (float*)calloc(npts, sizeof(float));
   memcpy( ts_det, ts, npts * sizeof(float));
   for( ii = 0; ii < npts; ii++) ts_det[ii] -= (ts_mean + ts_slope * tdelta * ii) ;
   
   /** OK, actually do some work **/

   for (meth_index = 0; meth_index < nbriks; meth_index++) {
   switch( meth[meth_index] ){

      default:
      case METH_MEAN:  val[meth_index] = ts_mean  ; break ;

      case METH_SLOPE: val[meth_index] = ts_slope ; break ;

      case METH_CVAR_NOD:
      case METH_SIGMA_NOD:
      case METH_CVAR:
      case METH_SIGMA:{
         register int ii ;
         register double sum ;

         sum = 0.0 ;
         if((meth[meth_index] == METH_CVAR) || (meth[meth_index] == METH_SIGMA )){
           for( ii=0 ; ii < npts ; ii++ ) sum += ts_det[ii] * ts_det[ii] ;
         } else {
           for( ii=0 ; ii < npts ; ii++ ) sum += (ts[ii]-ts_mean)
                                                *(ts[ii]-ts_mean) ;
         }

         sum = sqrt( sum/(npts-1) ) ;

         if((meth[meth_index] == METH_SIGMA) || (meth[meth_index] == METH_SIGMA_NOD))  val[meth_index] = sum ;
         else if( ts_mean != 0.0 ) val[meth_index] = sum / fabs(ts_mean) ;
         else                      val[meth_index] = 0.0 ;
      }
      break ;

      /* 14 Feb 2000: these 2 new methods disturb the array ts[] */
      /* 18 Dec 2002: these 2 methods no longer disturb the array ts[] */

      case METH_MEDIAN:{
         float* ts_copy;
         ts_copy = (float*)calloc(npts, sizeof(float));
         memcpy( ts_copy, ts, npts * sizeof(float));
         val[meth_index] = qmed_float( npts , ts_copy ) ;
	 free(ts_copy);
      }
      break ;

      case METH_MAD:{
         float* ts_copy;
         register int ii ;
         register float vm ;
         ts_copy = (float*)calloc(npts, sizeof(float));
         memcpy( ts_copy, ts, npts * sizeof(float));
         vm = qmed_float( npts , ts_copy ) ;
         for( ii=0 ; ii < npts ; ii++ ) ts_copy[ii] = fabs(ts_copy[ii]-vm) ;
         val[meth_index] = qmed_float( npts , ts_copy ) ;
	 free(ts_copy);
      }
      break ;

      case METH_MIN:{
         register int ii ;
         register float vm=ts[0] ;
         for( ii=1 ; ii < npts ; ii++ ) if( ts[ii] < vm ) vm = ts[ii] ;
         val[meth_index] = vm ;
      }
      break ;

      case METH_DW:{
         register int ii ;
         register float den=ts[0]*ts[0] ;
         register float num=0 ;
         for( ii=1 ; ii < npts ; ii++ ) {
           num = num + (ts_det[ii] - ts_det[ii-1])
		       *(ts_det[ii] - ts_det[ii-1]);
           den = den + ts_det[ii] * ts_det[ii];
         }
         val[meth_index] = num/den ;
      }
      break ;

      case METH_MAX:{
         register int ii ;
         register float vm=ts[0] ;
         for( ii=1 ; ii < npts ; ii++ ) if( ts[ii] > vm ) vm = ts[ii] ;
         val[meth_index] = vm ;
      }
      break ;
   }
   }

   free(ts_det);
   ncall++ ; return ;
}
