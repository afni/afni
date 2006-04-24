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

#define MAX_NUM_OF_METHS  20
static int meth[MAX_NUM_OF_METHS]  = {METH_MEAN};
static int nmeths                  = 0;
static char prefix[THD_MAX_PREFIX] = "stat" ;
static int datum                   = MRI_float ;
static char *meth_names[] = {
   "Mean"          , "Slope"        , "Std Dev"       , "Coef of Var" ,
   "Median"        , "Med Abs Dev"  , "Max"           , "Min"         ,
   "Durbin-Watson" , "Std Dev(NOD)" , "Coef Var(NOD)" , "AutoCorr"    ,
   "AutoReg"       , "Absolute Max" , "ArgMax"        , "ArgMin"      ,
   "ArgAbsMax"     , "Sum"
};
static void STATS_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;

static void autocorr( int npts, float ints[], int numVals, float outcoeff[] ) ;

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *old_dset , *new_dset ;  /* input and output datasets */
   int nopt, nbriks, ii ;
   int addBriks = 0;
   int numMultBriks,methIndex,brikIndex;

   /*----- Help the pitiful user? -----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTstat [options] dataset\n"
             "Computes one or more voxel-wise statistics for a 3D+time dataset\n"
             "and stores them in a bucket dataset.  If no statistic option is\n"
             "given, computes just the mean of each voxel time series.\n"
             "Multiple statistics options may be given.\n"
             "\n"
             "Statistics Options:\n"
             " -mean   = compute mean of input voxels\n"
             " -sum    = compute sum of input voxels\n"
             " -slope  = compute mean slope of input voxels vs. time\n"
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
             "\n"
             " -autocorr n = compute autocorrelation function and return\n"
             "               first n coefficients\n"
             " -autoreg n = compute autoregression coefficients and return\n"
             "               first n coefficients\n"
             "   [N.B.: -autocorr 0 and/or -autoreg 0 will return number\n"
             "          coefficients equal to the length of the input data]\n"
             "\n"
             "Other Options:\n"
             " -prefix p = use string 'p' for the prefix of the\n"
             "               output dataset [DEFAULT = 'stat']\n"
             " -datum d  = use data type 'd' for the type of storage\n"
             "               of the output, where 'd' is one of\n"
             "               'byte', 'short', or 'float' [DEFAULT=float]\n"
             "\n"
             "If you want statistics on a detrended dataset and the option\n"
             "doesn't allow that, you can use program 3dDetrend first.\n"
             "\n"
             "The output is a bucket dataset.  The input dataset may\n"
             "use a sub-brick selection list, as in program 3dcalc.\n"
           ) ;
      exit(0) ;
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

      /*-- Quien sabe'? --*/

      ERROR_exit("Unknown option: %s\n",argv[nopt]) ;
   }

   /*--- If no options selected, default to single stat MEAN -- KRH ---*/

   if (nmeths == 0) nmeths = 1;
   if (nbriks == 0 && addBriks == 0) nbriks = 1;

   /*----- read input dataset -----*/

   if( nopt >= argc ) ERROR_exit(" No input dataset!?") ;

   old_dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(old_dset) )
     ERROR_exit("Can't open dataset %s\n",argv[nopt]);

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
      for (methIndex = 0,brikIndex = 0; methIndex < nmeths; methIndex++, brikIndex++) {
        if ((meth[methIndex] == METH_AUTOCORR) || (meth[methIndex] == METH_AUTOREGP)) {
          numMultBriks = meth[methIndex+1];
          if (numMultBriks == 0) numMultBriks = DSET_NVALS(old_dset);
          for (ii = 1; ii <= numMultBriks; ii++) {
            char tmpstr[25];
            if (meth[methIndex] == METH_AUTOREGP) {
              sprintf(tmpstr,"%s[%d](%d)",meth_names[meth[methIndex]],numMultBriks,ii);
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
   int meth_index, ii , out_index;
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

      case METH_ABSMAX:
      case METH_ARGMAX:
      case METH_ARGABSMAX:
      case METH_MAX:{
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

         if ((meth[meth_index] == METH_ABSMAX) || (meth[meth_index] == METH_MAX)) {
           val[out_index] = vm ;
         } else {
           val[out_index] = outdex ;
         }
      }
      break ;

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
