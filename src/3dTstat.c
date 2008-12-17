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

#define METH_SIGMA_NOD     9  /* KRH 27 Dec 2002 */
#define METH_CVAR_NOD     10  /* KRH 27 Dec 2002 */

#define METH_AUTOCORR     11  /* KRH 16 Jun 2004 */
#define METH_AUTOREGP     12  /* KRH 16 Jun 2004 */

#define METH_ABSMAX       13  /* KRH 15 Feb 2005 */

#define METH_ARGMAX       14  /* KRH 4 Aug 2005 */
#define METH_ARGMIN       15  /* KRH 4 Aug 2005 */
#define METH_ARGABSMAX    16  /* KRH 4 Aug 2005 */

#define METH_SUM          17  /* RWCox 24 Apr 2006 */
#define METH_DURATION     18  /* DRG 15 Jun 2007 */
#define METH_CENTROID     19
#define METH_CENTDURATION 20

#define METH_ABSSUM       21

#define METH_NZMEAN       22  /* DRG 03 Oct 2007 */
#define METH_ONSET        23  /* DRG 08 Jan 2008 */
#define METH_OFFSET       24

#define METH_ACCUMULATE   25  /* RCR 04 Mar 2008 */

#define METH_SUM_SQUARES  26  /* ZSS 17 12 2008 */
#define MAX_NUM_OF_METHS  27

static int meth[MAX_NUM_OF_METHS]  = {METH_MEAN};
static int nmeths                  = 0;
static char prefix[THD_MAX_PREFIX] = "stat" ;
static int datum                   = MRI_float ;
static float basepercent           = 0.5;  /* 50% assumed for duration unless user specified */

static char *meth_names[] = {
   "Mean"          , "Slope"        , "Std Dev"       , "Coef of Var" ,
   "Median"        , "Med Abs Dev"  , "Max"           , "Min"         ,
   "Durbin-Watson" , "Std Dev(NOD)" , "Coef Var(NOD)" , "AutoCorr"    ,
   "AutoReg"       , "Absolute Max" , "ArgMax"        , "ArgMin"      ,
   "ArgAbsMax"     , "Sum"          , "Duration"      , "Centroid"    ,
   "CentDuration"  , "Absolute Sum" , "Non-zero Mean" , "Onset"       ,
   "Offset"        , "Accumulate" , "SS",
};

static void STATS_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;

static void autocorr( int npts, float ints[], int numVals, float outcoeff[] ) ;

static int Calc_duration(float *ts, int npts, float vmax, int max_index,
   int *onset, int *offset);
static float Calc_centroid(float *ts, int npts);


int main( int argc , char *argv[] )
{
   THD_3dim_dataset *old_dset , *new_dset ;  /* input and output datasets */
   int nopt, nbriks, ii ;
   int addBriks = 0 ;   /* n-1 sub-bricks out */
   int fullBriks = 0 ;  /* n   sub-bricks out */
   int tsout = 0 ;      /* flag to output a time series (not a stat bucket) */
   int numMultBriks,methIndex,brikIndex;

   /*----- Help the pitiful user? -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
"Usage: 3dTstat [options] dataset\n"
 "Computes one or more voxel-wise statistics for a 3D+time dataset\n"
 "and stores them in a bucket dataset.  If no statistic option is\n"
 "given, computes just the mean of each voxel time series.\n"
 "Multiple statistics options may be given, and will result in\n"
 "a multi-volume dataset.\n"
 "\n"
 "Statistics Options:\n"
 " -mean   = compute mean of input voxels\n"
 " -sum    = compute sum of input voxels\n"
 " -abssum = compute absolute sum of input voxels\n"
 " -slope  = compute mean slope of input voxels vs. time\n"
 " -sos    = compute sum of squares\n"
 " -stdev  = compute standard deviation of input voxels\n"
 "             [N.B.: this is computed after    ]\n"
 "             [      the slope has been removed]\n"
 " -cvar   = compute coefficient of variation of input\n"
 "             voxels = stdev/fabs(mean)\n"
 "   **N.B.: You can add NOD to the end of the above 2\n"
 "           options only, to turn off detrending, as in\n"
 "             -stdevNOD  and/or  -cvarNOD\n"
 "\n"
 " -MAD    = compute MAD (median absolute deviation) of\n"
 "             input voxels = median(|voxel-median(voxel)|)\n"
 "             [N.B.: the trend is NOT removed for this]\n"
 " -DW    = compute Durbin-Watson Statistic of input voxels\n"
 "             [N.B.: the trend IS removed for this]\n"
 " -median = compute median of input voxels  [undetrended]\n"
 " -min    = compute minimum of input voxels [undetrended]\n"
 " -max    = compute maximum of input voxels [undetrended]\n"
 " -absmax    = compute absolute maximum of input voxels [undetrended]\n"
 " -argmin    = index of minimum of input voxels [undetrended]\n"
 " -argmax    = index of maximum of input voxels [undetrended]\n"
 " -argabsmax = index of absolute maximum of input voxels [undetrended]\n"
 " -duration  = compute number of points around max above a threshold\n"
 "              Use basepercent option to set limits\n"
 " -onset     = beginning of duration around max where value\n"
 "              exceeds basepercent\n"
 " -offset    = end of duration around max where value\n"
 "              exceeds basepercent\n"
 " -centroid  = compute centroid of data time curves\n"
 "              (sum(i*f(i)) / sum(f(i)))\n"
 " -centduration = compute duration using centroid's index as center\n"
 " -nzmean    = compute mean of non-zero voxels\n"
 "\n"
 " -autocorr n = compute autocorrelation function and return\n"
 "               first n coefficients\n"
 " -autoreg n = compute autoregression coefficients and return\n"
 "               first n coefficients\n"
 "   [N.B.: -autocorr 0 and/or -autoreg 0 will return number\n"
 "          coefficients equal to the length of the input data]\n"
 "\n"
 " -accumulate = accumulate time series values (partial sums)\n"
 "               val[i] = sum old_val[t] over t = 0..i\n"
 "               (output length = input length)\n"
 "\n"
 " ** If no statistic option is given, then '-mean' is assumed **\n"
 "\n"
 "Other Options:\n"
 " -prefix p = use string 'p' for the prefix of the\n"
 "               output dataset [DEFAULT = 'stat']\n"
 " -datum d  = use data type 'd' for the type of storage\n"
 "               of the output, where 'd' is one of\n"
 "               'byte', 'short', or 'float' [DEFAULT=float]\n"
 " -basepercent nn = percentage of maximum for duration calculation\n"
 "\n"
 "If you want statistics on a detrended dataset and the option\n"
 "doesn't allow that, you can use program 3dDetrend first.\n"
 "\n"
 "The output is a bucket dataset.  The input dataset may\n"
 "use a sub-brick selection list, as in program 3dcalc.\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   /* bureaucracy */

   mainENTRY("3dTstat main"); machdep(); AFNI_logger("3dTstat",argc,argv);
   PRINT_VERSION("3dTstat"); AUTHOR("KR Hammett & RW Cox");

   /*--- scan command line for options ---*/

   nopt = 1 ;
   nbriks = 0 ;
   nmeths = 0 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      /*-- methods --*/

      if( strcmp(argv[nopt],"-median") == 0 ){
         meth[nmeths++] = METH_MEDIAN ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-DW") == 0 ){
         meth[nmeths++] = METH_DW ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-MAD") == 0 ){
         meth[nmeths++] = METH_MAD ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-mean") == 0 ){
         meth[nmeths++] = METH_MEAN ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-sum") == 0 ){
         meth[nmeths++] = METH_SUM ;
         nbriks++ ;
         nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-sos") == 0 ){
         meth[nmeths++] = METH_SUM_SQUARES ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-abssum") == 0 ){
         meth[nmeths++] = METH_ABSSUM ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-slope") == 0 ){
         meth[nmeths++] = METH_SLOPE ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-stdev") == 0 ||
          strcmp(argv[nopt],"-sigma") == 0   ){

         meth[nmeths++] = METH_SIGMA ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-cvar") == 0 ){
         meth[nmeths++] = METH_CVAR ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-stdevNOD") == 0 ||
          strcmp(argv[nopt],"-sigmaNOD") == 0   ){  /* 07 Dec 2001 */

         meth[nmeths++] = METH_SIGMA_NOD ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-cvarNOD") == 0 ){     /* 07 Dec 2001 */
         meth[nmeths++] = METH_CVAR_NOD ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-min") == 0 ){
         meth[nmeths++] = METH_MIN ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-max") == 0 ){
         meth[nmeths++] = METH_MAX ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-absmax") == 0 ){
         meth[nmeths++] = METH_ABSMAX ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-argmin") == 0 ){
         meth[nmeths++] = METH_ARGMIN ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-argmax") == 0 ){
         meth[nmeths++] = METH_ARGMAX ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-argabsmax") == 0 ){
         meth[nmeths++] = METH_ARGABSMAX ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-duration") == 0 ){
         meth[nmeths++] = METH_DURATION ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-onset") == 0 ){
         meth[nmeths++] = METH_ONSET ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-offset") == 0 ){
         meth[nmeths++] = METH_OFFSET ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-centroid") == 0 ){
         meth[nmeths++] = METH_CENTROID ;
         nbriks++ ;
         nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-centduration") == 0 ){
         meth[nmeths++] = METH_CENTDURATION ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-nzmean") == 0 ){
         meth[nmeths++] = METH_NZMEAN ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-autocorr") == 0 ){
         meth[nmeths++] = METH_AUTOCORR ;
         if( ++nopt >= argc ) ERROR_exit("-autocorr needs an argument!\n");
         meth[nmeths++] = atoi(argv[nopt++]);
         if (meth[nmeths - 1] == 0) {
           addBriks++;
         } else {
           nbriks+=meth[nmeths - 1] ;
         }
         continue ;
      }

      if( strcmp(argv[nopt],"-autoreg") == 0 ){
         meth[nmeths++] = METH_AUTOREGP ;
         if( ++nopt >= argc ) ERROR_exit("-autoreg needs an argument!\n");
         meth[nmeths++] = atoi(argv[nopt++]);
         if (meth[nmeths - 1] == 0) {
           addBriks++;
         } else {
           nbriks+=meth[nmeths - 1] ;
         }
         continue ;
      }

      if( strcmp(argv[nopt],"-accumulate") == 0 ){  /* 4 Mar 2008 [rickr] */
         meth[nmeths++] = METH_ACCUMULATE ;
         meth[nmeths++] = -1;   /* flag to add N (not N-1) output bricks */
         fullBriks++;
         tsout = 1;             /* flag to output a timeseries */
         nopt++ ; continue ;
      }

      /*-- prefix --*/

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("-prefix needs an argument!\n");
         MCW_strncpy(prefix,argv[nopt],THD_MAX_PREFIX) ;
         if( !THD_filename_ok(prefix) )
           ERROR_exit("%s is not a valid prefix!\n",prefix);
         nopt++ ; continue ;
      }

      /*-- datum --*/

      if( strcmp(argv[nopt],"-datum") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("-datum needs an argument!\n");
         if( strcmp(argv[nopt],"short") == 0 ){
            datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            datum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            datum = MRI_byte ;
         } else {
            ERROR_exit("-datum of type '%s' is not supported!\n",
                       argv[nopt] ) ;
         }
         nopt++ ; continue ;
      }

     /* base percentage for duration calcs */
     if (strcmp (argv[nopt], "-basepercent") == 0) {
         if( ++nopt >= argc ) ERROR_exit("-basepercent needs an argument!\n");
         basepercent = strtod(argv[nopt], NULL);
         if(basepercent>1) basepercent /= 100.0;  /* assume integer percent if >1*/
         nopt++;  continue;
        }

      /*-- Quien sabe'? --*/

      ERROR_exit("Unknown option: %s\n",argv[nopt]) ;
   }

   /*--- If no options selected, default to single stat MEAN -- KRH ---*/

   if (nmeths == 0) nmeths = 1;
   if (nbriks == 0 && addBriks == 0 && fullBriks == 0) nbriks = 1;

   /*----- read input dataset -----*/

   if( nopt >= argc ) ERROR_exit(" No input dataset!?") ;

   old_dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(old_dset) )
     ERROR_exit("Can't open dataset %s\n",argv[nopt]);

   nopt++ ;
   if( nopt < argc )
     WARNING_message("Trailing datasets on command line ignored: %s ...",argv[nopt]) ;

   if( DSET_NVALS(old_dset) < 2 )
     ERROR_exit("Can't use dataset with < 2 values per voxel!\n") ;

   if( DSET_NUM_TIMES(old_dset) < 2 ){
     WARNING_message("Input dataset is not 3D+time; assuming TR=1.0") ;
     EDIT_dset_items( old_dset ,
                        ADN_ntt    , DSET_NVALS(old_dset) ,
                        ADN_ttorg  , 0.0 ,
                        ADN_ttdel  , 1.0 ,
                        ADN_tunits , UNITS_SEC_TYPE ,
                      NULL ) ;
   }

   /* If one or more of the -autocorr/-autoreg options was called with */
   /* an argument of 0, then I'll now add extra BRIKs for the N-1 data */
   /* output points for each.                                          */
   nbriks += ((DSET_NVALS(old_dset)-1) * addBriks);
   nbriks += ((DSET_NVALS(old_dset)  ) * fullBriks);

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
      for (methIndex = 0,brikIndex = 0; methIndex < nmeths;
           methIndex++, brikIndex++) {
        if ((meth[methIndex] == METH_AUTOCORR)   ||
            (meth[methIndex] == METH_ACCUMULATE) ||
            (meth[methIndex] == METH_AUTOREGP)) {
          numMultBriks = meth[methIndex+1];

          /* note: this looks like it should be NV-1   4 Mar 2008 [rickr] */
          if (numMultBriks ==  0) numMultBriks = DSET_NVALS(old_dset)-1;
          /* new flag for NVALS [rickr] */
          if (numMultBriks == -1) numMultBriks = DSET_NVALS(old_dset);

          for (ii = 1; ii <= numMultBriks; ii++) {
            char tmpstr[25];
            if (meth[methIndex] == METH_AUTOREGP) {
              sprintf(tmpstr,"%s[%d](%d)",meth_names[meth[methIndex]],
                      numMultBriks,ii);
            } else {
              sprintf(tmpstr,"%s(%d)",meth_names[meth[methIndex]],ii);
            }
            EDIT_BRICK_LABEL(new_dset, (brikIndex + ii - 1), tmpstr) ;
          }
          methIndex++;
          brikIndex += numMultBriks - 1;
        } else {
          EDIT_BRICK_LABEL(new_dset, brikIndex, meth_names[meth[methIndex]]) ;
        }
      }

      if( tsout ) /* then change output to a time series */
         EDIT_dset_items( new_dset ,
                        ADN_ntt    , brikIndex ,
                        ADN_ttorg  , DSET_TIMEORIGIN(old_dset) ,
                        ADN_ttdel  , DSET_TIMESTEP(old_dset) ,
                        ADN_tunits , DSET_TIMEUNITS(old_dset) ,
                      NULL ) ;

      DSET_write( new_dset ) ;
      WROTE_DSET( new_dset ) ;
   } else {
      ERROR_exit("Unable to compute output dataset!\n") ;
   }

   exit(0) ;
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

static void STATS_tsfunc( double tzero, double tdelta ,
                          int npts, float ts[],
                          double ts_mean, double ts_slope,
                          void *ud, int nbriks, float *val          )
{
   static int nvox , ncall ;
   int meth_index, ii , out_index, nzpts, onset, offset, duration;
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
   for( ii = 0; ii < npts; ii++)
     ts_det[ii] -=
       (ts_mean - (ts_slope * (npts - 1) * tdelta/2) + ts_slope * tdelta * ii) ;

   /** OK, actually do some work **/

   /* This main loop now uses meth_index AND out_index as loop variables,     */
   /* mainly to avoid rewriting code that worked.                             */

   /* meth_index is an index into the static method array, which contains the */
   /* list of methods to be executed (and will also contain an integer        */
   /* parameter specifying the number of return values if -autocorr n and/or  */
   /* -autoreg p have been requested).                                        */
   /* out_index is an index into the output array.                            */

   for(meth_index=out_index=0 ; meth_index < nmeths; meth_index++,out_index++){
    switch( meth[meth_index] ){

      default:
      case METH_MEAN:  val[out_index] = ts_mean  ; break ;

      case METH_SUM:   val[out_index] = ts_mean * npts; break; /* 24 Apr 2006 */

      case METH_ABSSUM:{              /* 18 Jun 2006 */
        register int ii ;
        register float sum ;
        sum = 0.0;
        for(ii=0; ii< npts; ii++) sum += fabs(ts[ii]);
        val[out_index] = sum;
      }
      break;
      
      case METH_SUM_SQUARES:{           /* 18 Dec 2008 */
        register int ii ;
        register float sum ;
        sum = 0.0;
        for(ii=0; ii< npts; ii++) sum += ts[ii]*ts[ii];
        val[out_index] = sum;
      }
      break;
      
      case METH_SLOPE: val[out_index] = ts_slope ; break ;

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

        if((meth[meth_index] == METH_SIGMA) || (meth[meth_index] == METH_SIGMA_NOD))
          val[out_index] = sum ;
        else if( ts_mean != 0.0 )
          val[out_index] = sum / fabs(ts_mean) ;
        else
          val[out_index] = 0.0 ;
      }
      break ;

      /* 14 Feb 2000: these 2 new methods disturb the array ts[]       */
      /* 18 Dec 2002: these 2 methods no longer disturb the array ts[] */

      case METH_MEDIAN:{
        float* ts_copy;
        ts_copy = (float*)calloc(npts, sizeof(float));
        memcpy( ts_copy, ts, npts * sizeof(float));
        val[out_index] = qmed_float( npts , ts_copy ) ;
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
         val[out_index] = qmed_float( npts , ts_copy ) ;
         free(ts_copy);
      }
      break ;

      case METH_ARGMIN:
      case METH_MIN:{
         register int ii,outdex=0 ;
         register float vm=ts[0] ;
         for( ii=1 ; ii < npts ; ii++ ) if( ts[ii] < vm ) {
           vm = ts[ii] ;
           outdex = ii ;
         }
         if (meth[meth_index] == METH_MIN) {
           val[out_index] = vm ;
         } else {
           val[out_index] = outdex ;
         }
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
         if (den == 0) {
           val[out_index] = 0 ;
         } else {
           val[out_index] = num/den ;
         }
      }
      break ;

      case METH_ONSET:
      case METH_OFFSET:
      case METH_DURATION:
      case METH_ABSMAX:
      case METH_ARGMAX:
      case METH_ARGABSMAX:
      case METH_MAX:
      case METH_CENTDURATION:
      case METH_CENTROID:{
         register int ii, outdex=0 ;
         register float vm=ts[0] ;
         if ((meth[meth_index] == METH_ABSMAX) || (meth[meth_index] == METH_ARGABSMAX)) {
           vm = fabs(vm) ;
           for( ii=1 ; ii < npts ; ii++ ) {
             if( fabs(ts[ii]) > vm ) {
               vm = fabs(ts[ii]) ;
               outdex = ii ;
             }
           }
         } else {
           for( ii=1 ; ii < npts ; ii++ ) {
             if( ts[ii] > vm ) {
               vm = ts[ii] ;
               outdex = ii ;
             }
           }
         }
	 switch( meth[meth_index] ){
            default:
            case METH_ABSMAX:
            case METH_MAX:
               val[out_index] = vm ;
            break;

            case METH_ONSET:
            case METH_OFFSET:
            case METH_DURATION:
               duration = Calc_duration(ts, npts, vm, outdex,
                                    &onset,&offset);
               switch(meth[meth_index]) {
                  case METH_ONSET: val[out_index] = onset; break;
                  case METH_OFFSET: val[out_index] = offset; break;
                  case METH_DURATION: val[out_index] = duration; break;
               }

            break;

            case METH_ARGMAX:
            case METH_ARGABSMAX:
              val[out_index] = outdex ;
            break;

            case METH_CENTROID:
            case METH_CENTDURATION: {
              float cm;
              cm = Calc_centroid(ts, npts);
              if(meth[meth_index]== METH_CENTDURATION)
                 val[out_index] = Calc_duration(ts, npts, vm, (int) cm,
                                &onset,&offset);
              else
                 val[out_index] = cm;
            }
            break;
         }
      }
      break ;

      case METH_NZMEAN: {
        register int ii ;
        register float sum ;
           sum = 0.0;
           nzpts = 0;
           for( ii=0 ; ii < npts ; ii++ ) {
             if( ts[ii] != 0.0 ) {
               sum += ts[ii] ;
               nzpts++;
             }
           }
           if(npts>0)
              val[out_index] = sum / nzpts;
           else
              val[out_index] = 0.0;
      }
      break;

      case METH_AUTOCORR:{
        int numVals;
        float *ts_corr;
        /* for these new methods, the extra, needed integer */
        /* parameter is stored in the static array "meth",  */
        /* in the element right after the indicator for     */
        /* this method.  This parameter indicates the number*/
        /* of values to return, or 0 for the same length as */
        /* the input array.                                 */
        numVals = meth[meth_index + 1];
        if (numVals == 0) numVals = npts - 1;
        ts_corr = (float*)calloc(numVals,sizeof(float));
        /* Call the autocorrelation function, in this file. */
        autocorr(npts,ts,numVals,ts_corr);
        /* Copy the values into the output array val, which */
        /* will be returned to the fbuc MAKER caller to     */
        /* populate the appropriate BRIKs with the data.    */
        for( ii = 0; ii < numVals; ii++) {
          val[out_index + ii] = ts_corr[ii];
        }
        /* Although meth_index will be incremented by the   */
        /* for loop, it needs to be incremented an extra    */
        /* time to account for the extra parameter we       */
        /* pulled from the meth array.                      */
        meth_index++;
        /* Similarly, though the out_index will be incremented */
        /* by the for loop, we have added potentially several  */
        /* values to the output array, and we need to account  */
        /* for that here.                                      */
        out_index+=(numVals - 1);
        free(ts_corr);
      }
      break ;

      case METH_AUTOREGP:{
        int numVals,kk,mm;
        float *ts_corr, *y, *z;
        float alpha, beta, tmp;

        /* For these new methods, the extra, needed integer */
        /* parameter is stored in the static array "meth",  */
        /* in the element right after the indicator for     */
        /* this method.  This parameter indicates the number*/
        /* of values to return, or 0 for the same length as */
        /* the input array.                                 */
        numVals = meth[meth_index + 1];
        if (numVals == 0) numVals = npts - 1;

        /* Allocate space for the autocorrelation coefficients, */
        /* result, and a temp array for calculations.           */
        /* Correlation coeff array is larger, because we must   */
        /* disregard the r0, which is identically 1.            */
        /* Might fix this in autocorr function                  */
        ts_corr = (float*)malloc((numVals) * sizeof(float));
        y = (float*)malloc(numVals * sizeof(float));
        z = (float*)malloc(numVals * sizeof(float));

        /* Call autocorr function to generate the autocorrelation  */
        /* coefficients.                                           */
        autocorr(npts,ts,numVals,ts_corr);

        /* Durbin algorithm for solving Yule-Walker equations.        */
        /* The Yule-Walker equations specify the autoregressive       */
        /* coefficients in terms of the autocorrelation coefficients. */
        /* R*Phi = r, where r is vector of correlation coefficients,  */
        /* R is Toeplitz matrix formed from r, and Phi is a vector of */
        /* the autoregression coefficients.                           */

        /* In this implementation, 'y' is 'Phi' above and 'ts_corr' is 'r'    */

        y[0] = -ts_corr[0];
        alpha = -ts_corr[0];
        beta = 1;
        for (kk = 0 ; kk < (numVals - 1) ; kk++ ) {
          beta = (1 - alpha * alpha ) * beta ;
          tmp = 0;
          for ( mm = 0 ; mm <= kk ; mm++) {
            tmp = tmp + ts_corr[kk - mm] * y[mm];
          }
          alpha = - ( ts_corr[kk+1] + tmp ) / beta ;
          for ( mm = 0 ; mm <= kk ; mm++ ) {
            z[mm] = y[mm] + alpha * y[kk - mm];
          }
          for ( mm = 0 ; mm <= kk ; mm++ ) {
            y[mm] = z[mm];
          }
          y[kk+1] = alpha ;
        }

        /* Copy the values into the output array val, which */
        /* will be returned to the fbuc MAKER caller to     */
        /* populate the appropriate BRIKs with the data.    */
        for( ii = 0; ii < numVals; ii++) {
          val[out_index + ii] = y[ii];
          if (!finite(y[ii])){
            WARNING_message("BAD FLOAT y[%d]=%f; Call#%d\n",ii,y[ii],ncall);
            val[out_index + ii] = 0.0f ;
          }
        }
        /* Although meth_index will be incremented by the   */
        /* for loop, it needs to be incremented an extra    */
        /* time to account for the extra parameter we       */
        /* pulled from the meth array.                      */
        meth_index++;
        /* Similarly, though the out_index will be incremented */
        /* by the for loop, we have added potentially several  */
        /* values to the output array, and we need to account  */
        /* for that here.                                      */
        out_index+=(numVals - 1);
        free(ts_corr);
        free(y);
        free(z);
      }
      break ;

      case METH_ACCUMULATE:{
        register double sum = 0.0 ;
        register int    ii ;

        meth_index++;   /* go past dummy zero */

        for( ii = 0; ii < npts; ii++) {
          sum += ts[ii];  /* keep track as double */
          val[out_index + ii] = sum;
        }

        out_index+=(npts - 1);
      }
      break ;

    }
   }

   free(ts_det);
   ncall++ ; return ;
}


static void autocorr( int npts, float in_ts[], int numVals, float outcoeff[] )
{
  /* autocorr is the inv fourier xform of the fourier xform abs squared */

  int ii,nfft;
  double scaler;
  complex *cxar = NULL;

  /* Calculate size for FFT, including padding for eliminating overlap  */
  /* from circular convolution */
  nfft = csfft_nextup_one35(npts * 2 - 1);
/*  fprintf(stderr,"++ FFT length = %d\n",nfft) ; */

  cxar = (complex *) calloc( sizeof(complex) , nfft ) ;

  /* Populate complex array with input (real-only) time series */
  for( ii=0 ; ii < npts ; ii++ ){
    cxar[ii].r = in_ts[ii]; cxar[ii].i = 0.0;
  }
  /* Zero-pad input outside range of original time series */
  for( ii=npts ; ii < nfft ; ii++ ){ cxar[ii].r = cxar[ii].i = 0.0; }

  /* Calculate nfft-point FFT of input time series */
  /* First argument is "mode."  -1 value indicates */
  /* negative exponent in fourier sum, which means */
  /* this is an FFT and not an inverse FFT.        */
  /* Output will be complex and symmetrical.       */
  csfft_cox( -1 , nfft , cxar ) ;

  /* Use macro to calculate absolute square of FFT */
  for( ii=0 ; ii < nfft ; ii++ ){
    cxar[ii].r = CSQR(cxar[ii]) ; cxar[ii].i = 0 ;
  }

  /* Take inverse FFT of result.  First function called */
  /* sets a static variable that the output should be   */
  /* scaled by 1/N.  This plus the mode argument of 1   */
  /* indicate that this is an inverse FFT.              */
  csfft_scale_inverse( 1 ) ;
  csfft_cox( 1 , nfft, cxar ) ;

  /* Copy the requested number of coefficients to the   */
  /* output array outcoeff.  These will be copied into  */
  /* the output BRIKs or used for further calculations  */
  /* in the calling function, STATS_tsfunc() above.     */
  /* The output coefficients are scaled by 1/(M-p) to   */
  /* provide an unbiased estimate, and also scaled by   */
  /* the final value of the zeroth coefficient.         */
  scaler = cxar[0].r/npts;
  for (ii = 0 ; ii < numVals ; ii++ ) {
    outcoeff[ii] = cxar[ii+1].r/((npts - (ii+1)) * scaler);
  }
  free(cxar);
  cxar = NULL;
}

/* calculate the duration of a peak in number of subbricks */
/* duration is the number of points at or above the threshold*/
static int
Calc_duration(float ts[], int npts, float vmax, int max_index, 
  int *onset, int *offset)
{
   float minlimit;
   int i;
   ENTRY("Calc_duration");
   /* find beginning - onset - first point before max that falls below min */
   minlimit = basepercent * vmax;
   i = max_index;   /* for centroid option we need to start at max_index*/
   *onset = -1;
   *offset = npts;
   while(i>=0) {
     if(ts[i]<minlimit) {
        *onset = i;
        break;
     }
     i--;
   }

   /* find end of peak - offset - first point after max that falls below min */
   /*  this starting index needs to be the same for centroid or peak*/
   i = max_index + 1;
   while(i<npts) {
     if(ts[i]<minlimit) {
        *offset = i;
        break;
     }
     i++;
   }
  RETURN(*offset - *onset -1);
}

/* calculate the centroid of a time series*/
/* centroid or center of mass is defined as Sum(i*f(i)) / Sum(f(i)) */
static float
Calc_centroid(float ts[], int npts)
{
   int i;
   double sum=0.0, tvsum = 0.0;

   ENTRY("Calc_centroid");
   for(i=0;i<npts;i++) {
     sum += ts[i];
     tvsum += i*ts[i];
   }

   RETURN(tvsum / sum);
}

