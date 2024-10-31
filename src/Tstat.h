#ifndef _TSTAT_NAMES_
#define _TSTAT_NAMES_

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

#define METH_FIRSTVALUE    41 /* returns the 1st value - to avoid exiting on invalid 1-input-methods */
#define METH_TSNR          42 /* JKR 10 April 2017 */

#define METH_MEAN_SSD      43 /* 19 Mar 2018 */
#define METH_MEAN_SSDQ     44
#define METH_MEDIAN_ASD    45

#define METH_SKEWNESS      46 /* PDL 17 July 2020 */
#define METH_KURTOSIS      47 /* PDL 17 July 2020 */

#define MAX_NUM_OF_METHS   48 /* *** must match meth_names, below *** */


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
   "MSSDsqrt"      , "MASDx"        , "Skewness"      , "Kurtosis"
};

static int meth_pluginned[] = {
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 0             ,
   0               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   1               , 1              , 1               , 1             ,
   0               , 1              , 1               , 1             ,
   1               , 1              , 0               , 0
};
#endif
