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

#define METH_SUM_SQUARES  26  /* ZSS 17 Dec 2008 */

#define METH_BMV          27  /* RWC 16 Oct 2009 */

#define METH_ARGMIN1      28  /* ZSS Blizzard 2010 */
#define METH_ARGMAX1      29  /* ZSS Blizzard 2010 */
#define METH_ARGABSMAX1   30  /* ZSS Blizzard 2010 */

#define METH_CENTROMEAN   31  /* RWC 01 Nov 2010 */

#define METH_CVARINV      32  /* RWC 09 Aug 2011 */
#define METH_CVARINVNOD   33

#define METH_ZCOUNT        34
#define METH_NZMEDIAN      35  /* RCR 27 Jun 2012 */
#define METH_SIGNED_ABSMAX 36  /* RCR 31 Aug 2012 */
#define METH_L2_NORM       37  /* RCR 07 Jan 2013 */
#define METH_NZCOUNT       38

#define METH_NZSTDEV       39  /* RWC 29 Jul 2015 */

#define METH_PERCENTILE    40  /* RWC 05 May 2016 */
static int perc_val = -666;

#define METH_FIRSTVALUE    41 /* returns the 1st value - to avoid exiting on invalid 1-input-methods */
#define METH_TSNR          42 /* JKR 10 April 2017 */

#define METH_MEAN_SSD      43 /* 19 Mar 2018 */
#define METH_MEAN_SSDQ     44
#define METH_MEDIAN_ASD    45

#define MAX_NUM_OF_METHS   46

/* allow single inputs for some methods (test as we care to add) */
#define NUM_1_INPUT_METHODS 12
static int valid_1_input_methods[NUM_1_INPUT_METHODS]
           = { METH_MEAN, METH_MAX, METH_MIN, METH_SUM,
               METH_ARGMAX, METH_ARGMIN, METH_ARGABSMAX,METH_SUM,
               METH_ABSSUM, METH_NZMEAN, METH_SUM_SQUARES, METH_FIRSTVALUE };


static int meth[MAX_NUM_OF_METHS]  = {METH_MEAN};
static int nmeths                  = 0;
static char prefix[THD_MAX_PREFIX] = "stat" ;
static int datum                   = MRI_float ;
static int nscale                  = 0;
static float basepercent           = 0.5;  /* 50% assumed for duration unless user specified */

static int do_tdiff = 0 ;  /* 25 May 2011 */

#if 1
# include "Tstat.h"
#else
 static char *meth_names[] = {
    "Mean"          , "Slope"        , "Std Dev"       , "Coef of Var" ,
    "Median"        , "Med Abs Dev"  , "Max"           , "Min"         ,
    "Durbin-Watson" , "Std Dev(NOD)" , "Coef Var(NOD)" , "AutoCorr"    ,
    "AutoReg"       , "Absolute Max" , "ArgMax"        , "ArgMin"      ,
    "ArgAbsMax"     , "Sum"          , "Duration"      , "Centroid"    ,
    "CentDuration"  , "Absolute Sum" , "Non-zero Mean" , "Onset"       ,
    "Offset"        , "Accumulate"   , "SS"            , "BiwtMidV"    ,
    "ArgMin+1"      , "ArgMax+1"     , "ArgAbsMax+1"   , "CentroMean"  ,
    "CVarInv"       , "CvarInv (NOD)", "ZeroCount"     , "NZ Median"   ,
    "Signed Absmax" , "L2 Norm"      , "NonZero Count" , "NZ Stdev"    ,
    "Percentile %d" , "FirstValue"   , "TSNR"          , "MSSD"        ,
    "MSSDsqrt"      , "MASDx"
 };
#endif

static void STATS_tsfunc( double tzero , double tdelta ,
                         int npts , float *ts , double ts_mean ,
                         double ts_slope , void *ud , int nbriks, float *val ) ;

static void autocorr( int npts, float ints[], int numVals, float outcoeff[] ) ;

static int Calc_duration(float *ts, int npts, float vmax, int max_index,
   int *onset, int *offset);
static float Calc_centroid(float *ts, int npts);

void usage_3dTstat(int detail)
{

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
 " -slope  = compute the slope of input voxels vs. time\n"
 " -sos    = compute sum of squares\n"
 " -stdev  = compute standard deviation of input voxels\n"
 "             [N.B.: this is computed after    ]\n"
 "             [      the slope has been removed]\n"
 " -l2norm = compute L2 norm (sqrt(sum squares))\n"
 " -cvar   = compute coefficient of variation of input\n"
 "             voxels = stdev/fabs(mean)\n"
 " -cvarinv= 1.0/cvar = 'signal to noise ratio' [for Vinai]\n"
 "           **N.B.: You can add NOD to the end of the above 3\n"
 "                   options only, to turn off detrending, as in\n"
 "                     -stdevNOD  and/or  -cvarNOD  and/or  -cvarinvNOD\n"
 "\n"
 " -tsnr      = compute temporal signal to noise ratio\n"
 "                fabs(mean)/stdev NOT DETRENDED (same as -cvarinvNOD)\n"
 " -MAD       = compute MAD (median absolute deviation) of\n"
 "                input voxels = median(|voxel-median(voxel)|)\n"
 "                [N.B.: the trend is NOT removed for this]\n"
 " -DW        = compute Durbin-Watson Statistic of input voxels\n"
 "                [N.B.: the trend IS removed for this]\n"
 " -median    = compute median of input voxels  [undetrended]\n"
 " -nzmedian  = compute median of non-zero input voxels [undetrended]\n"
 " -nzstdev   = standard deviation of non-zero input voxel [undetrended]\n"
 " -bmv       = compute biweight midvariance of input voxels [undetrended]\n"
 "                [actually is 0.989*sqrt(biweight midvariance), to make]\n"
 "                [the value comparable to the standard deviation output]\n"
 " -MSSD      = Von Neumann's Mean of Successive Squared Differences\n"
 "               = average of sum of squares of first time difference\n"
 " -MSSDsqrt  = Sqrt(MSSD)\n"
 " -MASDx     = Median of absolute values of first time differences\n"
 "               times 1.4826 (to scale it like standard deviation)\n"
 "               = a robust alternative to MSSDsqrt\n"
 " -min       = compute minimum of input voxels [undetrended]\n"
 " -max       = compute maximum of input voxels [undetrended]\n"
 " -absmax    = compute absolute maximum of input voxels [undetrended]\n"
 " -signed_absmax = (signed) value with absolute maximum [undetrended]\n"
 " -percentile P  = the P-th percentile point (0=min, 50=median, 100=max)\n"
 "                  of the data in each voxel time series.\n"
 "                  [this option can only be used once!]\n"
 " -argmin    = index of minimum of input voxels [undetrended]\n"
 " -argmin1   = index + 1 of minimum of input voxels [undetrended]\n"
 " -argmax    = index of maximum of input voxels [undetrended]\n"
 " -argmax1   = index + 1 of maximum of input voxels [undetrended]\n"
 " -argabsmax = index of absolute maximum of input voxels [undetrended]\n"
 " -argabsmax1= index +1 of absolute maximum of input voxels [undetrended]\n"
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
 " -zcount    = count number of zero values at each voxel\n"
 " -nzcount    = count number of non zero values at each voxel\n"
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
 " -centromean = compute mean of middle 50%% of voxel values [undetrended]\n"
 "\n"
 " -firstvalue = first value in dataset - typically just placeholder\n\n"
 " ** If no statistic option is given, then '-mean' is assumed **\n"
 "\n"
 "Other Options:\n"
 " -tdiff    = Means to take the first difference of each time\n"
 "               series before further processing.\n"
 " -prefix p = Use string 'p' for the prefix of the\n"
 "               output dataset [DEFAULT = 'stat']\n"
 " -datum d  = use data type 'd' for the type of storage\n"
 "               of the output, where 'd' is one of\n"
 "               'byte', 'short', or 'float' [DEFAULT=float]\n"
 " -nscale = Do not scale output values when datum is byte or short.\n"
 "           Scaling is done by default.\n"
 "\n"
 " -basepercent nn = Percentage of maximum for duration calculation\n"
 "\n"
 " -mask mset   Means to use the dataset 'mset' as a mask:\n"
 "                 Only voxels with nonzero values in 'mset'\n"
 "                 will be printed from 'dataset'.  Note\n"
 "                 that the mask dataset and the input dataset\n"
 "                 must have the same number of voxels.\n"
 "\n"
 " -mrange a b  Means to further restrict the voxels from\n"
 "                 'mset' so that only those mask values\n"
 "                 between 'a' and 'b' (inclusive) will\n"
 "                 be used.  If this option is not given,\n"
 "                 all nonzero values from 'mset' are used.\n"
 "                 Note that if a voxel is zero in 'mset', then\n"
 "                 it won't be included, even if a < 0 < b.\n"
 "\n"
 " -cmask 'opts' Means to execute the options enclosed in single\n"
 "                  quotes as a 3dcalc-like program, and produce\n"
 "                  produce a mask from the resulting 3D brick.\n"
 "       Examples:\n"
 "        -cmask '-a fred+orig[7] -b zork+orig[3] -expr step(a-b)'\n"
 "                  produces a mask that is nonzero only where\n"
 "                  the 7th sub-brick of fred+orig is larger than\n"
 "                  the 3rd sub-brick of zork+orig.\n"
 "        -cmask '-a fred+orig -expr 1-bool(k-7)'\n"
 "                  produces a mask that is nonzero only in the\n"
 "                  7th slice (k=7); combined with -mask, you\n"
 "                  could use this to extract just selected voxels\n"
 "                  from particular slice(s).\n"
 "       Notes: * You can use both -mask and -cmask in the same\n"
 "                  run - in this case, only voxels present in\n"
 "                  both masks will be dumped.\n"
 "              * Only single sub-brick calculations can be\n"
 "                  used in the 3dcalc-like calculations -\n"
 "                  if you input a multi-brick dataset here,\n"
 "                  without using a sub-brick index, then only\n"
 "                  its 0th sub-brick will be used.\n"
 "              * Do not use quotes inside the 'opts' string!\n"
 "\n"
"\n"
 "If you want statistics on a detrended dataset and the option\n"
 "doesn't allow that, you can use program 3dDetrend first.\n"
 "\n"
 "The output is a bucket dataset.  The input dataset may\n"
 "use a sub-brick selection list, as in program 3dcalc.\n\n"
 "*** If you are trying to compute the mean or std.dev. of multiple\n"
 "datasets (not across time), use 3dMean or 3dmerge instead.\n"

           ) ;

 printf("\n"
  "----------------- Processing 1D files with 3dTstat -----------------\n"
  "To analyze a 1D file and get statistics on each of its columns,\n"
  "you can do something like this:\n"
  "  3dTstat -stdev -bmv -prefix stdout: file.1D\\'\n"
  "where the \\' means to transpose the file on input, since 1D files\n"
  "read into 3dXXX programs are interpreted as having the time direction\n"
  "along the rows rather than down the columns.  In this example, the\n"
  "output is written to the screen, which could be captured with '>'\n"
  "redirection.  Note that if you don't give the '-prefix stdout:'\n"
  "option, then the output will be written into a NIML-formatted 1D\n"
  "dataset, which you might find slightly confusing (but still usable).\n"
 ) ;

   PRINT_COMPILE_DATE ;

   return;
}

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *old_dset , *new_dset ;  /* input and output datasets */
   THD_3dim_dataset *mask_dset=NULL  ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   byte *cmask=NULL ; int ncmask=0 ;
   byte *mmm   = NULL ;
   int mcount=0, verb=0;
   int nopt, nbriks, ii ;
   int addBriks = 0 ;   /* n-1 sub-bricks out */
   int fullBriks = 0 ;  /* n   sub-bricks out */
   int tsout = 0 ;      /* flag to output a time series (not a stat bucket) */
   int numMultBriks,methIndex,brikIndex;

   /*----- Help the pitiful user? -----*/


   /* bureaucracy */
   mainENTRY("3dTstat main"); machdep(); AFNI_logger("3dTstat",argc,argv);
   PRINT_VERSION("3dTstat"); AUTHOR("KR Hammett & RW Cox");

   /*--- scan command line for options ---*/

   if (argc == 1) { usage_3dTstat(1); exit(0); } /* Bob's help shortcut */
   nopt = 1 ;
   nbriks = 0 ;
   nmeths = 0 ;
   verb = 0;
   while( nopt < argc && argv[nopt][0] == '-' ){
      if (strcmp(argv[nopt], "-h") == 0 || strcmp(argv[nopt], "-help") == 0) {
         usage_3dTstat(strlen(argv[nopt]) > 3 ? 2:1);
         exit(0);
      }

      if( strcmp(argv[nopt],"-verb") == 0 ){
        verb++ ; nopt++ ; continue ;
      }

      /*-- methods --*/

      if( strcasecmp(argv[nopt],"-methnum") == 0 ){    /* 20 Mar 2018 */
        if( ++nopt == argc )                           /* for plugin use */
          ERROR_exit("no arg after '%s'",argv[nopt-1]);
        meth[nmeths++] = (int)strtod(argv[nopt],NULL) ;
        nbriks++ ;
        nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-centromean") == 0 ){ /* 01 Nov 2010 */
         meth[nmeths++] = METH_CENTROMEAN ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-bmv") == 0 ){
         meth[nmeths++] = METH_BMV ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-median") == 0 ){
         meth[nmeths++] = METH_MEDIAN ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-nzmedian") == 0 ){
         meth[nmeths++] = METH_NZMEDIAN ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-nzstdev") == 0 ){
         meth[nmeths++] = METH_NZSTDEV ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-DW") == 0 ){
         meth[nmeths++] = METH_DW ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-zcount") == 0 ){
         meth[nmeths++] = METH_ZCOUNT ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-nzcount") == 0 ){
         meth[nmeths++] = METH_NZCOUNT ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-MAD") == 0 ){
         meth[nmeths++] = METH_MAD ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-mean") == 0 ){
         meth[nmeths++] = METH_MEAN ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-percentile") == 0 ){ /* 05 May 2016 */
         if( ++nopt >= argc )
           ERROR_exit("need argument after option '%s'",argv[nopt-1]) ;
         if( perc_val >= 0 )
           ERROR_exit("you can't use option '%s' twice!",argv[nopt-1]) ;
         perc_val = (int)strtod(argv[nopt],NULL) ;
         if( perc_val < 0 || perc_val > 100 )
           ERROR_exit("illegal number '%d' after option '%s'",
                      perc_val , argv[nopt-1]) ;
         if( perc_val == 0 )
           WARNING_message("option '%s 0' is the same as '-min'",argv[nopt-1]) ;
         if( perc_val == 100 )
           WARNING_message("option '%s 100' is the same as '-max'",argv[nopt-1]) ;
         meth[nmeths++] = METH_PERCENTILE ;
         nbriks++ ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-MSSD") == 0 ){  /* 19 Mar 2018 */
         meth[nmeths++] = METH_MEAN_SSD ;
         nbriks++ ;
         nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-MSSDQ")    == 0 ||
          strcasecmp(argv[nopt],"-MSSDsqrt") == 0 ){  /* 20 Mar 2018 */
         meth[nmeths++] = METH_MEAN_SSDQ ;
         nbriks++ ;
         nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-MASDx") == 0 ){  /* 19 Mar 2018 */
         meth[nmeths++] = METH_MEDIAN_ASD ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-sum") == 0 ){
         meth[nmeths++] = METH_SUM ;
         nbriks++ ;
         nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-sos") == 0 ){
         meth[nmeths++] = METH_SUM_SQUARES ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-abssum") == 0 ){
         meth[nmeths++] = METH_ABSSUM ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-slope") == 0 ){
         meth[nmeths++] = METH_SLOPE ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-stdev") == 0 ||
          strcasecmp(argv[nopt],"-sigma") == 0   ){

         meth[nmeths++] = METH_SIGMA ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-cvar") == 0 ){
         meth[nmeths++] = METH_CVAR ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-cvarinv") == 0 ){
         meth[nmeths++] = METH_CVARINV ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-stdevNOD") == 0 ||
          strcasecmp(argv[nopt],"-sigmaNOD") == 0   ){  /* 07 Dec 2001 */

         meth[nmeths++] = METH_SIGMA_NOD ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-cvarNOD") == 0 ){     /* 07 Dec 2001 */
         meth[nmeths++] = METH_CVAR_NOD ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-cvarinvNOD") == 0 ){
         meth[nmeths++] = METH_CVARINVNOD ;
         nbriks++ ;
         nopt++ ; continue ;
      }

     if( strcasecmp(argv[nopt],"-tsnr") == 0 ){     /* 10 April 2017 */
        meth[nmeths++] = METH_TSNR ;
        nbriks++ ;
        nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-min") == 0 ){
         meth[nmeths++] = METH_MIN ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-max") == 0 ){
         meth[nmeths++] = METH_MAX ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-absmax") == 0 ){
         meth[nmeths++] = METH_ABSMAX ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-signed_absmax") == 0 ){
         meth[nmeths++] = METH_SIGNED_ABSMAX ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-argmin") == 0 ){
         meth[nmeths++] = METH_ARGMIN ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-argmin1") == 0 ){
         meth[nmeths++] = METH_ARGMIN1 ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-argmax") == 0 ){
         meth[nmeths++] = METH_ARGMAX ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-argmax1") == 0 ){
         meth[nmeths++] = METH_ARGMAX1 ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-argabsmax") == 0 ){
         meth[nmeths++] = METH_ARGABSMAX ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-argabsmax1") == 0 ){
         meth[nmeths++] = METH_ARGABSMAX1;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-duration") == 0 ){
         meth[nmeths++] = METH_DURATION ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-onset") == 0 ){
         meth[nmeths++] = METH_ONSET ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-offset") == 0 ){
         meth[nmeths++] = METH_OFFSET ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-centroid") == 0 ){
         meth[nmeths++] = METH_CENTROID ;
         nbriks++ ;
         nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-centduration") == 0 ){
         meth[nmeths++] = METH_CENTDURATION ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-nzmean") == 0 ){
         meth[nmeths++] = METH_NZMEAN ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-mask",5) == 0 ){
         if( mask_dset != NULL )
           ERROR_exit("Cannot have two -mask options!\n") ;
         if( nopt+1 >= argc )
           ERROR_exit("-mask option requires a following argument!\n");
         mask_dset = THD_open_dataset( argv[++nopt] ) ;
         if( mask_dset == NULL )
           ERROR_exit("Cannot open mask dataset!\n") ;
         if( DSET_BRICK_TYPE(mask_dset,0) == MRI_complex )
           ERROR_exit("Cannot deal with complex-valued mask dataset!\n");
         nopt++ ; continue ;
      }

      if( strncmp(argv[nopt],"-mrange",5) == 0 ){
         if( nopt+2 >= argc )
           ERROR_exit("-mrange option requires 2 following arguments!\n");
         mask_bot = strtod( argv[++nopt] , NULL ) ;
         mask_top = strtod( argv[++nopt] , NULL ) ;
         if( mask_top < mask_top )
           ERROR_exit("-mrange inputs are illegal!\n") ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-cmask") == 0 ){  /* 16 Mar 2000 */
         if( nopt+1 >= argc )
            ERROR_exit("-cmask option requires a following argument!\n");
         cmask = EDT_calcmask( argv[++nopt] , &ncmask, 0 ) ;
         if( cmask == NULL ) ERROR_exit("Can't compute -cmask!\n");
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-autocorr") == 0 ){
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

      if( strcasecmp(argv[nopt],"-autoreg") == 0 ){
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

      if( strcasecmp(argv[nopt],"-accumulate") == 0 ){  /* 4 Mar 2008 [rickr] */
         meth[nmeths++] = METH_ACCUMULATE ;
         meth[nmeths++] = -1;   /* flag to add N (not N-1) output bricks */
         fullBriks++;
         tsout = 1;             /* flag to output a timeseries */
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-l2norm") == 0 ){  /* 07 Jan 2013 [rickr] */
         meth[nmeths++] = METH_L2_NORM ;
         nbriks++ ;
         nopt++ ; continue ;
      }

      /*-- prefix --*/

      if( strcasecmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("-prefix needs an argument!\n");
         MCW_strncpy(prefix,argv[nopt],THD_MAX_PREFIX) ;
         if( !THD_filename_ok(prefix) )
           ERROR_exit("%s is not a valid prefix!\n",prefix);
         nopt++ ; continue ;
      }

      /*-- tdiff --*/

      if( strcasecmp(argv[nopt],"-tdiff") == 0 ){  /* 25 May 2011 */
        do_tdiff = 1 ; nopt++ ; continue ;
      }

      /*-- nscale --*/

      if( strcasecmp(argv[nopt],"-nscale") == 0 ){  /* 25 May 2011 */
        nscale = 1 ; nopt++ ; continue ;
      }

      /*-- datum --*/

      if( strcasecmp(argv[nopt],"-datum") == 0 ){
         if( ++nopt >= argc ) ERROR_exit("-datum needs an argument!\n");
         if( strcasecmp(argv[nopt],"short") == 0 ){
            datum = MRI_short ;
         } else if( strcasecmp(argv[nopt],"float") == 0 ){
            datum = MRI_float ;
         } else if( strcasecmp(argv[nopt],"byte") == 0 ){
            datum = MRI_byte ;
         } else {
            ERROR_exit("-datum of type '%s' is not supported!\n",
                       argv[nopt] ) ;
         }
         nopt++ ; continue ;
      }

     /* base percentage for duration calcs */
     if (strcasecmp (argv[nopt], "-basepercent") == 0) {
         if( ++nopt >= argc ) ERROR_exit("-basepercent needs an argument!\n");
         basepercent = strtod(argv[nopt], NULL);
         if(basepercent>1) basepercent /= 100.0;  /* assume integer percent if >1*/
         nopt++;  continue;
        }

      /*-- Quien sabe'? --*/

      ERROR_message("Unknown option: %s\n",argv[nopt]) ;
      suggest_best_prog_option(argv[0], argv[nopt]);
      exit(1);
   }

   if (argc < 2) {
     ERROR_message("Too few options, use -help for details");
     exit(1);
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

   if( DSET_NVALS(old_dset) == 1 ){
     WARNING_message("Input dataset has 1 sub-brick ==> -tdiff is turned off") ;
     do_tdiff = 0 ;
   }

   /* no input volumes is bad, 1 volume applies to only certain methods */
   /*                                                2 Nov 2010 [rickr] */
   if( DSET_NVALS(old_dset) == 0 ) {
      ERROR_exit("Time series is of length 0?\n") ;
   }
   else if( DSET_NVALS(old_dset) == 1 || (do_tdiff && DSET_NVALS(old_dset)==2) ) {
     int methOK;
     /* see if each method is valid for nvals == 1 */
     for( methIndex = 0; methIndex < nmeths; methIndex++ ) {
        methOK = 0;
        for( ii = 0; ii < NUM_1_INPUT_METHODS; ii++ ) {
            if( meth[methIndex] == valid_1_input_methods[ii] ) {
                methOK = 1;
                break;
            }
            else
               meth[methIndex] = METH_FIRSTVALUE;
        }
        if( ! methOK )
           WARNING_message("Using dataset with only %d values per voxel and giving back just the first value!" ,
                      DSET_NVALS(old_dset) ) ;
     }
     /* tell the library function that this case is okay */
     g_thd_maker_allow_1brick = 1;
   }

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

   /* ------------- Mask business -----------------*/
   if( mask_dset == NULL ){
      mmm = NULL ;
      if( verb )
         INFO_message("%d voxels in the entire dataset (no mask)\n",
                     DSET_NVOX(old_dset)) ;
   } else {
      if( DSET_NVOX(mask_dset) != DSET_NVOX(old_dset) )
        ERROR_exit("Input and mask datasets are not same dimensions!\n");
      mmm = THD_makemask( mask_dset , 0 , mask_bot, mask_top ) ;
      mcount = THD_countmask( DSET_NVOX(old_dset) , mmm ) ;
      if( mcount <= 0 ) ERROR_exit("No voxels in the mask!\n") ;
      if( verb ) INFO_message("%d voxels in the mask\n",mcount) ;
      DSET_delete(mask_dset) ;
   }

   if( cmask != NULL ){
      if( ncmask != DSET_NVOX(old_dset) )
        ERROR_exit("Input and cmask datasets are not same dimensions!\n");
      if( mmm != NULL ){
         for( ii=0 ; ii < DSET_NVOX(old_dset) ; ii++ )
            mmm[ii] = (mmm[ii] && cmask[ii]) ;
         free(cmask) ;
         mcount = THD_countmask( DSET_NVOX(old_dset) , mmm ) ;
         if( mcount <= 0 ) ERROR_exit("No voxels in the mask+cmask!\n") ;
         if( verb ) INFO_message("%d voxels in the mask+cmask\n",mcount) ;
      } else {
         mmm = cmask ;
         mcount = THD_countmask( DSET_NVOX(old_dset) , mmm ) ;
         if( mcount <= 0 ) ERROR_exit("No voxels in the cmask!\n") ;
         if( verb ) INFO_message("%d voxels in the cmask\n",mcount) ;
      }
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
                 NULL,                  /* data for tsfunc */
                 mmm,
                 nscale
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
        } else if( meth[methIndex] == METH_PERCENTILE ){ /* 05 May 2016 */
          char plabel[32] ;
          sprintf(plabel,meth_names[METH_PERCENTILE],perc_val) ;
          EDIT_BRICK_LABEL(new_dset, brikIndex, plabel ) ;
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
                          int npts, float *ts ,
                          double ts_mean, double ts_slope,
                          void *ud, int nbriks, float *val          )
{
   static int ncall ;
   int meth_index, ii , out_index, nzpts, onset, offset, duration;
   float *ts_det, *ts_dif=NULL ;

   /** is this a "notification"? **/

   if( val == NULL ){

      if( npts > 0 ){  /* the "start notification" */

         ncall = 0 ;                          /* number of calls */

      } else {  /* the "end notification" */

         /* nothing to do here */

      }
      return ;
   }

   /* RWC: first difference here [25 May 2011] */

   if( do_tdiff ){
     float tsm , tss ;
     ts_dif = (float*)calloc(npts, sizeof(float)) ;
	  for( ii=1 ; ii < npts ; ii++ ) ts_dif[ii-1] = ts[ii]-ts[ii-1] ;
     get_linear_trend( npts-1 , ts_dif , &tsm , &tss ) ;
     ts_mean = (double)tsm ; ts_slope = (double)tss ;
     npts-- ; ts = ts_dif ;
   }

   /* KRH make copy and detrend it right here.
      Use ts_mean and ts_slope for detrend-ization. */

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
      case METH_FIRSTVALUE:  val[out_index] = ts[0]; break ; /* placeholder drg 06/13/2016 */
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

      case METH_PERCENTILE:{         /* 05 May 2016 */
        float* ts_copy; int ii , fi ;
        ts_copy = (float*)calloc(npts, sizeof(float));
        memcpy( ts_copy, ts, npts * sizeof(float));
        qsort_float(npts,ts_copy) ;
        fi = (perc_val * npts)*0.01f ;
        ii = (int)fi ; fi = fi - ii ;
        if( ii >= npts-1 ){                  /* max */
          val[out_index] = ts_copy[npts-1] ;
        } else if( ii < 0 ){                 /* should never happen */
          val[out_index] = ts_copy[0] ;
        } else {                             /* the usual suspects */
          val[out_index] = (1.0f-fi)*ts_copy[ii] + fi*ts_copy[ii+1] ;
        }
        free(ts_copy) ;
      }
      break ;

      case METH_L2_NORM:                /* 07 Jan 2013 [rickr] */
      case METH_SUM_SQUARES:{           /* 18 Dec 2008 */
        register int ii ;
        register float sum ;
        sum = 0.0;
        for(ii=0; ii< npts; ii++) sum += ts[ii]*ts[ii];

        /* check multiple methods here */
        if ( meth[meth_index] == METH_SUM_SQUARES )
           val[out_index] = sum;
        else if ( meth[meth_index] == METH_L2_NORM ) {
           /* theory and practice do not always seem to agree, so... */
           if( sum >= 0.0 ) val[out_index] = sqrt(sum);
           else             val[out_index] = 0.0;
        }
      }
      break;

      case METH_SLOPE: val[out_index] = ts_slope ; break ;

      case METH_CVAR_NOD:     /* methods that depend on the mean and stdev */
      case METH_SIGMA_NOD:
      case METH_CVARINVNOD:
      case METH_CVAR:
      case METH_CVARINV:
      case METH_SIGMA:{
        register int ii ;
        register double sum ;

        int mm = meth[meth_index] ;
        int nod = (mm == METH_CVAR_NOD)  ||   /* no detrend flag */
                  (mm == METH_SIGMA_NOD) ||
                  (mm == METH_CVARINVNOD)  ;

        sum = 0.0 ;
        if( !nod ){   /* not no detrend ==> use detrended data */
          for( ii=0 ; ii < npts ; ii++ ) sum += ts_det[ii] * ts_det[ii] ;
        } else {      /* use data as God gave it to us */
          for( ii=0 ; ii < npts ; ii++ ) sum += (ts[ii]-ts_mean)
                                               *(ts[ii]-ts_mean) ;
        }

        sum = sqrt( sum/(npts-1.0) ) ;  /* stdev */

        if( mm == METH_SIGMA ||  mm == METH_SIGMA_NOD )
          val[out_index] = sum ;
        else if( mm == METH_CVAR || mm == METH_CVAR_NOD )
          val[out_index] = (ts_mean != 0.0) ? sum/fabs(ts_mean) : 0.0 ;
        else
          val[out_index] = (sum     != 0.0) ? fabs(ts_mean)/sum : 0.0 ;
      }
      break ;

      case METH_TSNR:{
        register int ii ;
        register double std = 0.0;
        register double sum = 0.0;

        /* no detrending */
        for( ii=0 ; ii < npts ; ii++ ) sum += (ts[ii]-ts_mean)
                                           *(ts[ii]-ts_mean) ;
        std = sqrt( sum/(npts-1.0) ) ;  /* stdev */
        val[out_index] = (std != 0.0) ? fabs(ts_mean)/std : 0.0 ;
      }
      break ;

      case METH_MEAN_SSDQ:
      case METH_MEAN_SSD:{  /* 19 Mar 2018 */
        val[out_index] = cs_mean_square_sd( npts , ts ) ;
        if( meth[meth_index] == METH_MEAN_SSDQ )
          val[out_index] = sqrtf( MAX(val[out_index],0.0f) ) ;
      }
      break ;

      case METH_MEDIAN_ASD:{  /* 19 Mar 2018 */
        val[out_index] = cs_median_abs_sd( npts , ts , NULL ) * 1.4826f ;
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

      case METH_NZMEDIAN:{      /* 27 Jun 2012 [rickr] */
        float* ts_copy;
        int    lind, lnzcount;
        ts_copy = (float*)calloc(npts, sizeof(float));
        /* replace memcpy with non-zero copy */
        lnzcount=0;
        for (lind=0; lind < npts; lind++)
           if( ts[lind] ) ts_copy[lnzcount++] = ts[lind];
        /* and get the result from the possibly shortened array */
        if( lnzcount > 0 ) val[out_index] = qmed_float( lnzcount , ts_copy ) ;
        else               val[out_index] = 0.0 ;
        free(ts_copy);
      }
      break ;

      case METH_NZSTDEV:{      /* 29 Jul 2015 [RWC] */
        float* ts_copy;
        int    lind, lnzcount;
        ts_copy = (float*)calloc(npts, sizeof(float));
        /* replace memcpy with non-zero copy */
        lnzcount=0;
        for (lind=0; lind < npts; lind++)
           if( ts[lind] ) ts_copy[lnzcount++] = ts[lind];
        /* and get the result from the possibly shortened array */
        if( lnzcount > 1 ) meansigma_float( lnzcount,ts_copy, NULL,val+out_index ) ;
        else               val[out_index] = 0.0 ;
        free(ts_copy);
      }
      break ;

      case METH_CENTROMEAN:{  /* 01 Nov 2010 */
        float* ts_copy;
        ts_copy = (float*)calloc(npts, sizeof(float));
        memcpy( ts_copy, ts, npts * sizeof(float));
        val[out_index] = centromean_float( npts , ts_copy ) ;
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

      case METH_BMV:{  /* 16 Oct 2009 */
        float bmv ;
        qmedmadbmv_float( npts , ts , NULL,NULL , &bmv ) ;
        val[out_index] = bmv ;
      }
      break ;

      case METH_ARGMIN:
      case METH_ARGMIN1:
      case METH_MIN:{
         register int ii,outdex=0 ;
         register float vm=ts[0] ;
         for( ii=1 ; ii < npts ; ii++ ) if( ts[ii] < vm ) {
           vm = ts[ii] ;
           outdex = ii ;
         }
         if (meth[meth_index] == METH_MIN) {
           val[out_index] = vm ;
         } else if (meth[meth_index] == METH_ARGMIN) {
            val[out_index] = outdex ;
         } else {
            val[out_index] = outdex +1;
         }
      }
      break ;

      case METH_ZCOUNT:{  /* 05 Apr 2012 */
        int ii , zc ;
        for( ii=zc=0 ; ii < npts ; ii++ ) if( ts[ii] == 0.0f ) zc++ ;
        val[out_index] = zc ;
      }
      break ;

      case METH_NZCOUNT:{  /* Turkey 2014 */
        int ii , zc ;
        for( ii=zc=0 ; ii < npts ; ii++ ) if( ts[ii] == 0.0f ) zc++ ;
        val[out_index] = npts-zc ;
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
      case METH_SIGNED_ABSMAX:
      case METH_ARGMAX:
      case METH_ARGMAX1:
      case METH_ARGABSMAX:
      case METH_ARGABSMAX1:
      case METH_MAX:
      case METH_CENTDURATION:
      case METH_CENTROID:{
         register int ii, outdex=0 ;
         register float vm=ts[0];
         if ( (meth[meth_index] == METH_ABSMAX) ||
               (meth[meth_index] == METH_ARGABSMAX) ||
               (meth[meth_index] == METH_ARGABSMAX1)  ) {
           vm = fabs(vm) ;
           for( ii=1 ; ii < npts ; ii++ ) {
             if( fabs(ts[ii]) > vm ) {
               vm = fabs(ts[ii]) ;
               outdex = ii ;
             }
           }
         } else if ( meth[meth_index] == METH_SIGNED_ABSMAX ) {
           /* for P Hamilton    31 Aug 2012 [rickr] */
           register float avm=fabs(vm);
           for( ii=1 ; ii < npts ; ii++ ) {
             if( fabs(ts[ii]) > avm ) {
               vm = ts[ii] ;
               avm = fabs(vm) ;
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
            case METH_SIGNED_ABSMAX:
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

            case METH_ARGMAX1:
            case METH_ARGABSMAX1:
              val[out_index] = outdex +1;
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
          if (!isfinite(y[ii])){
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

   free(ts_det); if( ts_dif == ts ) free(ts_dif) ;
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
  nfft = csfft_nextup_even(npts * 2 - 1);
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
