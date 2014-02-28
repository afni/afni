/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*** Adapted from AlphaSim.c ***/

#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/*---------------------------------------------------------------------------*/
#include "zgaussian.c"  /** Ziggurat Gaussian random number generator **/
/*---------------------------------------------------------------------------*/

#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#define MAX_NAME_LENGTH  THD_MAX_NAME /* max. string length for file names */
#define MAX_CLUSTER_SIZE 99999        /* max. size of cluster for freq. table */

/*---------------------------------------------------------------------------*/
/*
  Global data
*/

static THD_3dim_dataset  *mask_dset  = NULL ; /* mask dataset */
static byte              *mask_vol   = NULL;  /* mask volume */
static int mask_nvox = 0, mask_ngood = 0;     /* number of good voxels in mask volume */

static int max_cluster_size = MAX_CLUSTER_SIZE ;

static int   nx     = 64 ;
static int   ny     = 64 ;
static int   nz     = 32 ;
static int   nxy ;
static int   nxyz ;
static int   nxyz1 ;
static float dx     = 3.5f ;
static float dy     = 3.5f ;
static float dz     = 3.5f ;
static float fwhm_x = 0.0f ;
static float fwhm_y = 0.0f ;
static float fwhm_z = 0.0f ;
static int   niter  = 10000 ;

static float sigmax , sigmay , sigmaz ;
static int do_blur = 0 ;

static int nodec   = 0 ;
static int do_niml = 0 ;
static int do_1D   = 1 ;
static int do_ball = 0 ;

static int do_lohi = 0 ;  /* 20 Jan 2011 -- for debugging */

static int do_2sid = 0 ;  /* 21 Sep 2011 */

static int do_NN[4] = { 0 , 1 , 0 , 0 } ;

static unsigned int gseed = 123456789 ;

#define PMAX 0.2

static double pthr_init[8] = { 0.02 , 0.01, 0.005, 0.002, 0.001, 0.0005, 0.0002, 0.0001 } ;
static double athr_init[4] = { 0.10 , 0.05 , 0.02 , 0.01 } ;

static int   npthr_lots     = 29 ;
static double pthr_lots[29] = { 0.10,    0.09,    0.08,    0.07,    0.06,
                                0.05,    0.04,    0.03,    0.02,    0.015,    0.01,
                                0.007,   0.005,   0.003,   0.002,   0.0015,   0.001,
                                0.0007,  0.0005,  0.0003,  0.0002,  0.00015,  0.0001,
                                0.00007, 0.00005, 0.00003, 0.00002, 0.000015, 0.00001 } ;

static int   nathr_lots   = 10 ;
static double athr_lots[] = { 0.10, 0.09, .08, .07, .06, .05, .04, .03, .02, .01 } ;

static int    npthr = 8 ;
static double *pthr = NULL ;
static float  *zthr = NULL ;

static int    nathr = 4 ;
static double *athr = NULL ;

static int verb = 1 ;
static int nthr = 1 ;

static int minmask = 128 ;   /* 29 Mar 2011 */

static char *prefix = NULL ;

#undef  PSMALL
#define PSMALL 1.e-15

/*----------------------------------------------------------------------------*/
/*! Threshold for tail probability of N(0,1) */

double zthresh( double pval )
{
        if( pval <= 0.0 ) pval = PSMALL ;
   else if( pval >= 1.0 ) pval = 1.0 - PSMALL ;
   return qginv(pval) ;
}

/*---------------------------------------------------------------------------*/

static void vstep_print(void)
{
   static int nn=0 ;
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[nn%10] ) ;
   if( nn%10 == 9) fprintf(stderr,".") ;
   nn++ ;
}

/*---------------------------------------------------------------------------*/

void display_help_menu()
{
  printf(
   "Usage: 3dClustSim [options]\n"
   "\n"
   "Program to estimate the probability of false positive (noise-only) clusters.\n"
   "An adaptation of Doug Ward's AlphaSim, streamlined for various purposes.\n"
   "\n"
   "In particular, this program lets you run with multiple p-value thresholds\n"
   "(the '-pthr' option) and only outputs the cluster size threshold at chosen\n"
   "values of the alpha significance level (the '-athr' option).\n"
   "\n"
   "In addition, the program allows the output to be formatted for inclusion\n"
   "into an AFNI dataset's header, whence it can be used in the AFNI Clusterize\n"
   "interface to show approximate alpha values for the displayed clusters, where\n"
   "the per-voxel p-value is taken from the interactive threshold slider in the\n"
   "AFNI 'Define Overlay' control panel, and then the per-cluster alpha value\n"
   "is interpolated in this table from 3dClustSim.  As you change the threshold\n"
   "slider, the per-voxel p-value (shown below the slider) changes, and then\n"
   "the interpolated alpha values are updated.\n"
   "\n"
   "Three different clustering methods can be used -- the program can give\n"
   "you results for any or all of them in one run -- see the '-NN' option.\n"
   "\n"
   "-------\n"
   "OPTIONS  [at least 1 option is required, or you'll get this help message!]\n"
   "-------\n"
   " ***** Specify the volume over which the simulation will occur *****\n"
   "\n"
   "  --** (a) Directly give the spatial domain that will be used **--\n"
   "-nxyz n1 n2 n3 = Size of 3D grid to use for simulation\n"
   "                  [default values = 64 64 32]\n"
   "-dxyz d1 d2 d3 = give all 3 voxel sizes at once\n"
   "                  [default values = 3.5 3.5 3.5]\n"
   "-BALL          = inside the 3D grid, mask off points outside a ball\n"
   "                  at the center of the grid and touching the edges;\n"
   "                  this will keep about 1/2 the points in the 3D grid.\n"
   "                  [default = use all voxels in the 3D grid]\n"
   "\n"
   "  --** OR: (b) Specify the spatial domain using a dataset mask **--\n"
   "\n"
   "-mask mset     = Use the 0 sub-brick of dataset 'mset' as a mask\n"
   "                  to indicate which voxels to analyze (a sub-brick\n"
   "                  selector '[]' is allowed) \n"
   "\n"
   "-OKsmallmask   = Allow small masks. Normally, a mask volume must have\n"
   "                  128 or more nonzero voxels.  However, IF you know what\n"
   "                  you are doing, and IF you are willing to live life on\n"
   "                  the edge of statistical catastrophe, then you can use\n"
   "                  this option to allow smaller masks -- in a sense, this\n"
   "                  is the 'consent form' for such strange shenanigans.\n"
   "                 * If you use this option, it must come BEFORE '-mask'.\n"
   "                 * Also read the 'CAUTION and CAVEAT' section, far below.\n"
   "\n"
   "    ** '-mask' means that '-nxyz' & '-dxyz' & '-BALL' will be ignored. **\n"
   "\n"
   "-fwhm s        = Gaussian filter width (all 3 dimensions)\n"
   "                  [default = 0.0 = no smoothing]\n"
   "                 * If you wish to set different smoothing amounts for\n"
   "                   each axis, you can instead use option\n"
   "                     -fwhmxyz sx sy sz\n"
   "                   to specify the three values separately.\n"
   "\n"
   "-pthr p1 .. pn = list of uncorrected (per voxel) p-values at which to\n"
   "                  threshold the simulated images prior to clustering.\n"
   "                  [default = 0.02 0.01 0.005 0.002 0.001 0.0005 0.0002 0.0001]\n"
   "\n"
   "-2sided        = Normally, 3dClustSim does '1-sided' testing.  The random\n"
   "                  dataset of Gaussian noise-only values is generated, and then\n"
   "                  it is thresholded on the positive side so that the N(0,1)\n"
   "                  upper tail probability is pthr.  If you use this option, then\n"
   "                  the thresholding is instead done in a 2-sided way: positive\n"
   "                  AND negative values will be clustered together, with the\n"
   "                  thresholding chosen to have a 2-sided tail probability given\n"
   "                  by pthr.\n"
   "                 * This option is for experimentation, not for serious use!\n"
   "\n"
   "-athr a1 .. an = list of corrected (whole volume) alpha-values at which\n"
   "                  the simulation will print out the cluster size\n"
   "                  thresholds.  For each 'p' and 'a', the smallest cluster\n"
   "                  size C(p,a) for which the probability of the 'p'-thresholded\n"
   "                  image having a noise-only cluster of size C is less than 'a'\n"
   "                  is the output (cf. the sample output, below)\n"
   "                  [default = 0.10 0.05 0.02 0.01]\n"
   "\n"
   "         ** Both lists '-pthr' and '-athr' (of values between 0 and 0.2)    **\n"
   "         ** should be given in DESCENDING order.  They will be sorted to be **\n"
   "         ** that way in any case, and such is how the output will be given. **\n"
   "\n"
   "         ** The list of values following '-pthr' or '-athr' can be replaced **\n"
   "         ** with the single word 'LOTS', which will tell the program to use **\n"
   "         ** a longer list of values for these probabilities [try it & see!] **\n"
   "         ** (i.e., '-pthr LOTS' and/or '-athr LOTS' are legal options)      **\n"
   "\n"
   "-LOTS          = the same as using '-pthr LOTS -athr LOTS'\n"
   "\n"
   "-iter n        = number of Monte Carlo simulations [default = 10000]\n"
   "\n"
   "-NN abc        = Define the clustering method(s) to use.  'abc' contains\n"
   "                 some set of digits from the set { 1 , 2 , 3 }, where\n"
   "                  1 = Use first-nearest neighbor clustering\n"
   "                      * above threshold voxels cluster together if faces touch\n"
   "                  2 = Use second-nearest neighbor clustering\n"
   "                      * voxels cluster together if faces OR edges touch\n"
   "                  3 = Use third-nearest neighbor clustering\n"
   "                      * voxels cluster together if faces OR edges OR corners touch\n"
   "                 To get outputs from all 3 types of clustering, use '-NN 123'.\n"
   "                 If you don't use this option, then only first-nearest neighbor\n"
   "                 clustering will be computed (as if you used '-NN 1').\n"
   "\n"
   "              ** The clustering method only makes a difference at higher **\n"
   "              ** (less significant) values of pthr.   At small values of **\n"
   "              ** pthr (more significant),  all 3 clustering methods will **\n"
   "              ** give very similar results.                              **\n"
   "\n"
   "-nodec         = normally, the program prints the cluster size threshold to\n"
   "                  1 decimal place (e.g., 27.2).  Of course, clusters only come\n"
   "                  with an integer number of voxels -- this fractional value\n"
   "                  is interpolated to give the desired alpha level.  If you\n"
   "                  want no decimal places (so that 27.2 becomes 28), use '-nodec'.\n"
   "\n"
   "-seed S        = random number seed [default seed = 123456789]\n"
   "                  * if seed=0, then program will quasi-randomize it\n"
   "\n"
   "-niml          = Output the table in an XML/NIML format, rather than a .1D format.\n"
   "                  * This option is for use with other software programs;\n"
   "                    see the NOTES section below for details.\n"
   "                  * '-niml' also implicitly means '-LOTS'.\n"
   "                  * '-niml' also implicitly means '-NN 123'.\n"
   "                    If you DON'T want all 3 NN methods, then use an explicit\n"
   "                    '-NN' option AFTER the '-niml' option to choose what you want.\n"
   "\n"
   "-both          = Output the table in XML/NIML format AND in .1D format.\n"
   "                  * You probably want to use '-prefix' with this option!\n"
   "                    Otherwise, everything is mixed together on stdout.\n"
   "                  * '-both' does NOT imply '-NN 123'; if you want all 3 NN\n"
   "                    methods with '-both', you have to explicitly use '-NN 123'.\n"
   "\n"
   "-prefix ppp    = Write output for NN method #k to file 'ppp.NNk.1D' for k=1, 2, 3.\n"
   "                  * If '-prefix is not used, results go to standard output.\n"
   "                  * If '-niml' is used, the filename is 'ppp.NNk.niml'.\n"
   "                  * If '-niml' AND '-mask' are both used, then a compressed ASCII\n"
   "                    encoding of the mask volume is stored into file 'ppp.mask'.\n"
   "                    This string can be stored into a dataset header as an attribute\n"
   "                    with name AFNI_CLUSTSIM_MASK, and will be used in the AFNI\n"
   "                    Clusterize GUI, if present, to mask out above-threshold voxels\n"
   "                    before the clusterizing is done (which is how the mask is used\n"
   "                    here in 3dClustSim).\n"
   "                  * If the ASCII mask string is NOT stored into the statistics dataset\n"
   "                    header, then the Clusterize GUI will try to find the original\n"
   "                    mask dataset and use that instead.  If that fails, then masking\n"
   "                    won't be done in the Clusterize process.\n"
   "\n"
   "-quiet         = Don't print out the progress reports, etc.\n"
   "                  * Put this option first to quiet most informational messages.\n"
   "\n"
   "------\n"
   "NOTES:\n"
   "------\n"
   "* This program is like running AlphaSim once for each '-pthr' value and then\n"
   "  extracting the relevant information from its 'Alpha' output column.\n"
   " ++ One reason for 3dClustSim to be used in place of AlphaSim is that it will\n"
   "    be faster than running AlphaSim multiple times.\n"
   " ++ Another reason is that the resulting table can be stored in an AFNI\n"
   "    dataset's header, and used in the AFNI Clusterize GUI to see estimated\n"
   "    cluster significance (alpha) levels.\n"
   "\n"
   "* To be clear, the C(p,alpha) thresholds that are calculated are for\n"
   "  alpha = probability of a noise-only smooth random field, after masking\n"
   "  and then thresholding at the given per-voxel p value, producing a cluster\n"
   "  of voxels at least this big.\n"
   " ++ So if your cluster is larger than the C(p,0.01) threshold in size (say),\n"
   "    then it is very unlikely that noise BY ITSELF produced this result.\n"
   " ++ This statement does not mean that ALL the voxels in the cluster are\n"
   "    'truly' active -- it means that at least SOME of them are (very probably)\n"
   "    active.  The statement of low probability (0.01 in this example) of a\n"
   "    false positive result applies to the cluster as a whole, not to each\n"
   "    voxel within the cluster.\n"
   "\n"
   "* To add the cluster simulation C(p,alpha) table to the header of an AFNI\n"
   "  dataset, something like the following can be done [tcsh syntax]:\n"
   "     set fwhm = ( `3dFWHMx -combine -detrend time_series_dataset+orig` )\n"
   "     3dClustSim -mask mask+orig -fwhm $fwhm[4] -niml -prefix CStemp\n"
   "     3drefit -atrstring AFNI_CLUSTSIM_NN1 file:CStemp.NN1.niml \\\n"
   "             -atrstring AFNI_CLUSTSIM_MASK file:CStemp.mask    \\\n"
   "             statistics_dataset+orig\n"
   "     rm -f CStemp.*\n"
   "  AFNI's Clusterize GUI makes use of these attributes, if stored in a\n"
   "  statistics dataset (e.g., something from 3dDeconvolve, 3dREMLfit, etc.).\n"
   "\n"
   "   ** Nota Bene: afni_proc.py will automatically run 3dClustSim,  and **\n"
   "  *** put the results  into the statistical results  dataset for you. ***\n"
   " **** Another reason to use afni_proc.py for single-subject analyses! ****\n"
   "\n"
   "* 3dClustSim will print (to stderr) a 3drefit command fragment, similar\n"
   "  to the one above, that you can use to add cluster tables to any\n"
   "  relevant statistical datasets you have lolling about.\n"
   "\n"
   "* The C(p,alpha) table will be used in Clusterize to provide the cluster\n"
   "  level alpha value when the AFNI GUI is set so that the Overlay threshold\n"
   "  sub-brick is a statistical parameter (e.g., a t- or F-statistic), from which\n"
   "  a per-voxel p-value can be calculated, so that Clusterize can interpolate\n"
   "  in the C(p,alpha) table.\n"
   " ++ To be clear, the per-voxel p-value is taken from the AFNI GUI threshold\n"
   "    slider (the p-value is shown beneath the slider), and then the C(p,alpha)\n"
   "    table is inverse-interpolated to find the per-cluster alpha value for\n"
   "    each different cluster size.\n"
   " ++ As you move the AFNI threshold slider, the per-voxel (uncorrected for\n"
   "    multiple comparisons) p-value changes, the cluster sizes change (as fewer\n"
   "    or more voxels are included), and so the reported per-cluster alpha\n"
   "    values change for both reasons -- different p and different cluster size.\n"
   " ++ The alpha values reported are 'per-cluster', and are not themselves\n"
   "    corrected for multiple comparisons ACROSS clusters.  These alpha values\n"
   "    are corrected for multiple comparisons within a cluster.\n"
   "\n"
   "* AFNI will use the NN1, NN2, NN3 tables as needed in its Clusterize\n"
   "  interface if they are all stored in the statistics dataset header,\n"
   "  depending on the NN level chosen in the Clusterize controller.\n"
   "  ++ If only the NN1 table gets stored in the statistics dataset, then\n"
   "     that table will be used even if you ask for NN3 clusterizing inside\n"
   "     AFNI -- the idea being that to get SOME result is better than nothing.\n"
   "\n"
   "* The blur estimates (provided via -fwhm, say) can come from various\n"
   "  sources.\n"
   "     1) If '3dmerge -1blur_fwhm SIZE' is used to apply the blur to EPI\n"
   "        data, that blur is on top of what is already in the data.  It is\n"
   "        then appropriate to estimate the final blur size using 3dFWHMx on\n"
   "        the residual EPI time series (after regression).  The final blur\n"
   "        will generally be a bit larger than SIZE.  Consider how this is\n"
   "        done by afni_proc.py.\n"
   "     2) If '3dBlurToFWHM -FWHM SIZE' is used, then one can use SIZE\n"
   "        directly (since the resulting blur is SIZE, it is not on top of\n"
   "        what is in the data to begin with).\n"
   "     3) Some people prefer to estimate the smoothness from the stdev of\n"
   "        error in the given statistical test, rather than the residuals.\n"
   "\n"
   "-------------------\n"
   "CAUTION and CAVEAT: [January 2011]\n"
   "-------------------\n"
   "* If you use a small ROI mask and also have a large FWHM, then it might happen\n"
   "  that it is impossible to find a cluster size threshold C that works for a\n"
   "  given (p,alpha) combination.\n"
   "\n"
   "* Generally speaking, C(p,alpha) gets smaller as p gets smaller and C(p,alpha)\n"
   "  gets smaller as alpha gets larger.  As a result, in a small mask with small p\n"
   "  and large alpha, C(p,alpha) might shrink below 1.  But clusters of size C\n"
   "  less than 1 don't make any sense!\n"
   "\n"
   "* For example, suppose that for p=0.0005 that only 6%% of the simulations\n"
   "  have ANY above-threshold voxels inside the ROI mask.  In that case,\n"
   "  C(p=0.0005,alpha=0.06) = 1.  There is no smaller value of C where 10%%\n"
   "  of the simulations have a cluster of size C or larger.  Thus, it is\n"
   "  impossible to find the cluster size threshold for the combination of\n"
   "  p=0.0005 and alpha=0.10 in this case.\n"
   "\n"
   "* 3dClustSim will report a cluster size threshold of C=1 for such cases.\n"
   "  It will also print (to stderr) a warning message for all the (p,alpha)\n"
   "  combinations that had this problem.\n"
   "\n"
   "* This issue arises because 3dClustSim reports C for a given alpha.\n"
   "  In contrast, AlphaSim reports alpha for each given C, and leaves\n"
   "  you to interpret the resulting table; it doesn't try to find C(p,alpha)\n"
   "  for a given alpha, but finds alpha for various values of C.\n"
   "\n"
   "* If you wish to see this effect in action, the following commands\n"
   "  can be used as a starting point:\n"
   "\n"
   "3dClustSim -nxyz 8 8 8 -dxyz 2 2 2 -fwhm 8 -niter 10000\n"
   "AlphaSim -nxyz 8 8 8 -dxyz 2 2 2 -fwhm 8 -niter 10000 -quiet -fast -pthr 0.0005\n"
   "\n"
   "  From the 3dClustSim command above, you should get a warning message\n"
   "  similar to this, right after the table (only the first 2 lines are shown):\n"
   "\n"
   "*+ WARNING: Simulation not effective for these cases:\n"
   "   NN=1  pthr= 0.001000  alpha= 0.100 [max simulated alpha= 0.087]\n"
   "\n"
   "-----------------------------\n"
   "---- RW Cox -- July 2010 ----\n"
  ) ;

  printf(
   "\n"
   "-------------\n"
   "SAMPLE OUTPUT from the command '3dClustSim -fwhm 7'\n"
   "-------------\n"
   "# 3dClustSim -fwhm 7\n"
   "# Grid: 64x64x32 3.50x3.50x3.50 mm^3 (131072 voxels)\n"
   "#\n"
   "# CLUSTER SIZE THRESHOLD(pthr,alpha) in Voxels\n"
   "# -NN 1  | alpha = Prob(Cluster >= given size)\n"
   "#  pthr  |  0.100  0.050  0.020  0.010\n"
   "# ------ | ------ ------ ------ ------\n"
   " 0.020000    64.3   71.0   80.5   88.5\n"
   " 0.010000    40.3   44.7   50.7   55.1\n"
   " 0.005000    28.0   31.2   34.9   38.1\n"
   " 0.002000    19.0   21.2   24.2   26.1\n"
   " 0.001000    14.6   16.3   18.9   20.5\n"
   " 0.000500    11.5   13.0   15.1   16.7\n"
   " 0.000200     8.7   10.0   11.6   12.8\n"
   " 0.000100     7.1    8.3    9.7   10.9\n"
   "\n"
   "e.g., for this sample volume, if the per-voxel p-value threshold is set\n"
   "at 0.005, then to keep the probability of getting a single noise-only\n"
   "cluster at 0.05 or less, the cluster size threshold should be 32 voxels\n"
   "(the next integer above 31.2).\n"
   "\n"
   "If you ran the same simulation with the '-nodec' option, then the last\n"
   "line above would be\n"
   " 0.000100       8      9     10     11\n"
   "If you set the per voxel p-value to 0.0001 (1e-4), and want the chance\n"
   "of a noise-only false-positive cluster to be 5%% or less, then the cluster\n"
   "size threshold would be 9 -- that is, you would keep all NN clusters with\n"
   "9 or more voxels.\n"
   "\n"
   "The header lines start with the '#' (commenting) character so that the result\n"
   "is a correctly formatted AFNI .1D file -- it can be used in 1dplot, etc.\n"
  ) ;

  PRINT_AFNI_OMP_USAGE("3dClustSim",NULL) ;
  PRINT_COMPILE_DATE ;
  exit(0);
}

/*---------------------------------------------------------------------------*/
/* Routine to initialize the input options (values are in global variables). */

void get_options( int argc , char **argv )
{
  int nopt=1 , ii ;

  /*----- add to program log -----*/

  pthr = (double *)malloc(sizeof(double)*npthr) ;
  memcpy( pthr , pthr_init , sizeof(double)*npthr ) ;

  athr = (double *)malloc(sizeof(double)*nathr) ;
  memcpy( athr , athr_init , sizeof(double)*nathr ) ;

  if( getenv("AFNI_RANDOM_SEEDVAL") != NULL ){
    gseed = (unsigned int)AFNI_numenv("AFNI_RANDOM_SEEDVAL") ;
    if( gseed == 0 )
      gseed = ((unsigned int)time(NULL)) + 17*(unsigned int)getpid() ;
    INFO_message("random seed set to %u from AFNI_RANDOM_SEEDVAL",gseed) ;
  }

  while( nopt < argc ){

    /*-----  -nxyz n1 n2 n3 -----*/

    if( strcmp(argv[nopt],"-nxyz") == 0 ){
      nopt++ ; if( nopt+2 >= argc ) ERROR_exit("need 3 arguments after -nxyz") ;
      nx = (int)strtod(argv[nopt++],NULL); if( nx <= 0 ) ERROR_exit("illegal nx value") ;
      ny = (int)strtod(argv[nopt++],NULL); if( ny <= 0 ) ERROR_exit("illegal ny value") ;
      nz = (int)strtod(argv[nopt++],NULL); if( nz <= 0 ) ERROR_exit("illegal nz value") ;
      continue ;
    }

    /*-----  -dxyz d1 d2 d3 -----*/

    if( strcmp(argv[nopt],"-dxyz") == 0 ){
      nopt++ ; if( nopt+2 >= argc ) ERROR_exit("need 3 arguments after -dxyz") ;
      dx = strtod(argv[nopt++],NULL); if( dx <= 0.0f ) ERROR_exit("illegal dx value") ;
      dy = strtod(argv[nopt++],NULL); if( dy <= 0.0f ) ERROR_exit("illegal dy value") ;
      dz = strtod(argv[nopt++],NULL); if( dz <= 0.0f ) ERROR_exit("illegal dz value") ;
      continue ;
    }

    /**** -OKsmallmask ****/

    if( strcmp(argv[nopt],"-OKsmallmask") == 0 ){  /* 29 Mar 2011 */
      minmask = 2 ; nopt++ ; continue ;
    }

    /**** -mask mset ****/

    if( strcmp(argv[nopt],"-mask") == 0 ){
      if( mask_dset != NULL ) ERROR_exit("Can't use -mask twice!") ;
      nopt++ ; if( nopt >= argc ) ERROR_exit("need argument after -mask!") ;
      mask_dset = THD_open_dataset(argv[nopt]);
      if( mask_dset == NULL ) ERROR_exit("can't open -mask dataset!") ;
      mask_vol = THD_makemask( mask_dset , 0 , 1.0,0.0 ) ;
      if( mask_vol == NULL ) ERROR_exit("can't use -mask dataset!") ;
      mask_nvox = DSET_NVOX(mask_dset) ;
      DSET_unload(mask_dset) ;
      mask_ngood = THD_countmask( mask_nvox , mask_vol ) ;
      if( mask_ngood < minmask ){
        if( minmask > 2 && mask_ngood > 2 ){
          ERROR_message("-mask has only %d nonzero voxels; minimum allowed is %d.",
                        mask_ngood , minmask ) ;
          ERROR_message("To run this simulation, please read the CAUTION and CAVEAT in -help,") ;
          ERROR_message("and then you can use the '-OKsmallmask' option if you so desire.") ;
          ERROR_exit("Cannot continue -- may we meet under happier circumstances!") ;
        } else if( mask_ngood == 0 ){
          ERROR_exit("-mask has no nonzero voxels -- cannot use this at all :-(") ;
        } else {
          ERROR_exit("-mask has only %d nonzero voxel%s -- cannot use this :-(",
                     mask_ngood , (mask_ngood > 1) ? "s" : "\0" ) ;
        }
      }
      if( verb ) INFO_message("%d voxels in mask (%.2f%% of total)",
                              mask_ngood,100.0*mask_ngood/(double)mask_nvox) ;
      nopt++ ; continue ;
    }

    /*-----   -fwhm s   -----*/

    if( strcasecmp(argv[nopt],"-fwhm") == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after -fwhm");
      fwhm_x = (float)strtod(argv[nopt],NULL) ;
      if( fwhm_x < 0.0f ) ERROR_exit("illegal value after -fwhm") ;
      fwhm_y = fwhm_z = fwhm_x ; nopt++; continue;
    }

    /*-----   -fwhmxyz s s s   -----*/

    if( strcasecmp(argv[nopt],"-fwhmxyz") == 0 ){
      nopt++; if( nopt+2 >= argc ) ERROR_exit("need 3 arguments after -fwhmxyz");
      fwhm_x = (float)strtod(argv[nopt++],NULL) ;
      if( fwhm_x < 0.0f ) ERROR_exit("illegal value after -fwhmxyz") ;
      fwhm_y = (float)strtod(argv[nopt++],NULL) ;
      if( fwhm_y < 0.0f ) ERROR_exit("illegal value after -fwhmxyz") ;
      fwhm_z = (float)strtod(argv[nopt++],NULL) ;
      if( fwhm_z < 0.0f ) ERROR_exit("illegal value after -fwhmxyz") ;
      continue;
    }

    /*-----   -iter n  -----*/

    if( strcmp(argv[nopt],"-iter") == 0 || strncmp(argv[nopt],"-nite",5) == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
      niter = (int)strtod(argv[nopt],NULL) ;
      if( niter < 2000 ){
        WARNING_message("-iter %d replaced by 2000",niter) ; niter = 2000 ;
      }
      nopt++; continue;
    }

    /*-----   -seed S  -----*/

    if( strcasecmp(argv[nopt],"-seed") == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
      gseed = (unsigned int)strtol(argv[nopt],NULL,10) ;
      if( gseed == 0 ){
        gseed = ((unsigned int)time(NULL)) + 17*(unsigned int)getpid() ;
        if( verb ) INFO_message("-seed 0 resets to %u",gseed) ;
      }
      nopt++; continue;
    }

    /*-----   -LOTS     -----*/

    if( strcasecmp(argv[nopt],"-LOTS") == 0 ){
      npthr = npthr_lots ;
      pthr = (double *)realloc(pthr,sizeof(double)*npthr) ;
      memcpy( pthr , pthr_lots , sizeof(double)*npthr ) ;
      nathr = nathr_lots ;
      athr = (double *)realloc(athr,sizeof(double)*nathr) ;
      memcpy( athr , athr_lots , sizeof(double)*nathr ) ;
      nopt++ ; continue ;
    }

    /*-----   -2sided   ------*/

    if( strcasecmp(argv[nopt],"-2sided") == 0 ){
      do_2sid = 1 ; nopt++ ; continue ;
    }
    if( strcasecmp(argv[nopt],"-1sided") == 0 ){
      do_2sid = 0 ; nopt++ ; continue ;
    }

    /*-----   -pthr p   -----*/

    if( strcmp(argv[nopt],"-pthr") == 0 || strcmp(argv[nopt],"-pval") == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
      if( strcmp(argv[nopt],"LOTS") == 0 ){   /* built in big table of values */
        npthr = npthr_lots ;
        pthr = (double *)realloc(pthr,sizeof(double)*npthr) ;
        memcpy( pthr , pthr_lots , sizeof(double)*npthr ) ;
        nopt++ ;
      } else {                                /* a list of values */
        for( ii=nopt ; ii < argc && argv[ii][0] != '-' ; ii++ ) ; /*nada*/
        npthr = ii-nopt ;
        if( npthr <= 0 ) ERROR_exit("No positive values found after %s",argv[nopt-1]) ;
        pthr = (double *)realloc(pthr,sizeof(double)*npthr) ;
        for( ii=0 ; ii < npthr ; ii++ ){
          pthr[ii] = strtod(argv[nopt+ii],NULL) ;
          if( pthr[ii] <= 0.0 || pthr[ii] > PMAX )
            ERROR_exit("value '%s' after '%s' is illegal!",argv[nopt+ii],argv[nopt-1]) ;
        }
        if( npthr > 1 ){
          for( ii=0 ; ii < npthr ; ii++ ) pthr[ii] = -pthr[ii] ;  /* sort into */
          qsort_double( npthr , pthr ) ;                          /* descending */
          for( ii=0 ; ii < npthr ; ii++ ) pthr[ii] = -pthr[ii] ;  /* order */
          for( ii=1 ; ii < npthr ; ii++ ){
            if( pthr[ii] == pthr[ii-1] )
              WARNING_message("duplicate value %g after '%s'",pthr[ii],argv[nopt-1]) ;
          }
        }
        nopt += npthr ;
      }
      continue ;
    }

    /*-----   -athr p   -----*/

    if( strcmp(argv[nopt],"-athr") == 0 || strcmp(argv[nopt],"-aval") == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
      if( strcmp(argv[nopt],"LOTS") == 0 ){   /* built in big table of values */
        nathr = nathr_lots ;
        athr = (double *)realloc(athr,sizeof(double)*nathr) ;
        memcpy( athr , athr_lots , sizeof(double)*nathr ) ;
        nopt++ ;
      } else {                                /* a list of values */
        for( ii=nopt ; ii < argc && argv[ii][0] != '-' ; ii++ ) ; /*nada*/
        nathr = ii-nopt ;
        if( nathr <= 0 ) ERROR_exit("No positive values found after %s",argv[nopt-1]) ;
        athr = (double *)realloc(athr,sizeof(double)*nathr) ;
        for( ii=0 ; ii < nathr ; ii++ ){
          athr[ii] = strtod(argv[nopt+ii],NULL) ;
          if( athr[ii] <= 0.0 || athr[ii] > PMAX )
            ERROR_exit("value '%s' after '%s' is illegal!",argv[nopt+ii],argv[nopt-1]) ;
        }
        if( nathr > 1 ){
          for( ii=0 ; ii < nathr ; ii++ ) athr[ii] = -athr[ii] ;
          qsort_double( nathr , athr ) ;
          for( ii=0 ; ii < nathr ; ii++ ) athr[ii] = -athr[ii] ;
          for( ii=1 ; ii < nathr ; ii++ ){
            if( athr[ii] == athr[ii-1] )
              WARNING_message("duplicate value %g after '%s'",athr[ii],argv[nopt-1]) ;
          }
        }
        nopt += nathr ;
      }
      continue ;
    }

    /*----   -nodec   ----*/

    if( strcasecmp(argv[nopt],"-nodec") == 0 ){
      nodec = 1 ; nopt++ ; continue ;
    }

    if( strcasecmp(argv[nopt],"-alo") == 0 ){   /* 20 Jan 2011: debug stuff */
      do_lohi = 1 ; nopt++ ; continue ;
    }

    /*----   -niml   ----*/

    if( strcasecmp(argv[nopt],"-niml") == 0 ||
        strcasecmp(argv[nopt],"-both") == 0   ){
      npthr = npthr_lots ;
      pthr = (double *)realloc(pthr,sizeof(double)*npthr) ;
      memcpy( pthr , pthr_lots , sizeof(double)*npthr ) ;
      nathr = nathr_lots ;
      athr = (double *)realloc(athr,sizeof(double)*nathr) ;
      memcpy( athr , athr_lots , sizeof(double)*nathr ) ;
      do_niml = 1 ;
      if( strcasecmp(argv[nopt],"-both") == 0 )
        do_1D = 1 ;
      else
        do_NN[1] = do_NN[2] = do_NN[3] = 1 ;   /* 19 Jan 2011 */
      nopt++ ; continue ;
    }

    /*----   -BALL   ----*/

    if( strcasecmp(argv[nopt],"-BALL") == 0 ){
      do_ball = 1 ; nopt++ ; continue ;
    }

    /*----   -NN   ----*/

    if( strcasecmp(argv[nopt],"-NN") == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
      do_NN[1] = (strchr(argv[nopt],'1') != NULL) ;
      do_NN[2] = (strchr(argv[nopt],'2') != NULL) ;
      do_NN[3] = (strchr(argv[nopt],'3') != NULL) ;
      ii = do_NN[1] + do_NN[2] + do_NN[3] ;
      if( !ii )
        ERROR_exit("argument after %s does not contain digits 1, 2, or 3",argv[nopt-1]) ;
      nopt++ ; continue ;
    }

    /*-----  -prefix -----*/

    if( strcmp(argv[nopt],"-prefix") == 0 ){
      nopt++ ; if( nopt >= argc ) ERROR_exit("need argument after -prefix!") ;
      prefix = strdup(argv[nopt]) ;
      if( !THD_filename_ok(prefix) ) ERROR_exit("bad -prefix option!") ;
      nopt++ ; continue ;
    }

    /*----   -quiet   ----*/

    if( strcasecmp(argv[nopt],"-quiet") == 0 ){
      verb = 0 ; nopt++ ; continue ;
    }

    /*----- unknown option -----*/

    ERROR_exit("3dClustSim -- unknown option '%s'",argv[nopt]) ;
  }

  /*------- finalize some simple setup stuff --------*/

  if( mask_dset != NULL ){
    nx = DSET_NX(mask_dset) ;
    ny = DSET_NY(mask_dset) ;
    nz = DSET_NZ(mask_dset) ;
    dx = fabsf(DSET_DX(mask_dset)) ;
    dy = fabsf(DSET_DY(mask_dset)) ;
    dz = fabsf(DSET_DZ(mask_dset)) ;
  }

  nxy = nx*ny ; nxyz = nxy*nz ; nxyz1 = nxyz - nxy ;
  if( nxyz < 256 )
    ERROR_exit("Only %d voxels in simulation?! Need at least 256.",nxyz) ;

  if( mask_vol == NULL && do_ball ){
    float xq,yq,zq , nx2,ny2,nz2 , nxq,nyq,nzq ; int ii,jj,kk ;

    nx2 = 0.5f*(nx-1) ; nxq = nx2*nx2 + 0.501f ;
    ny2 = 0.5f*(ny-1) ; nyq = ny2*ny2 + 0.501f ;
    nz2 = 0.5f*(nz-1) ; nzq = nz2*nz2 + 0.501f ;
    mask_vol = (byte *)malloc(sizeof(byte)*nxyz) ;
    for( kk=0 ; kk < nz ; kk++ ){
      zq = kk - nz2 ; zq = (zq*zq) / nzq ;
      for( jj=0 ; jj < ny ; jj++ ){
        yq = jj - ny2 ; yq = (yq*yq) / nyq ;
          for( ii=0 ; ii < nx ; ii++ ){
            xq = ii - nx2 ; xq = (xq*xq) / nxq ;
            mask_vol[ii+jj*nx+kk*nxy] = ((xq+yq+zq) <= 1.0f) ;
    }}}
    mask_nvox  = nxyz ;
    mask_ngood = THD_countmask( mask_nvox , mask_vol ) ;
    if( verb ) INFO_message("%d voxels in BALL mask (%.1f%% of total)",
                             mask_ngood,100.0*mask_ngood/(double)mask_nvox) ;
  }

  if( mask_ngood == 0 ) mask_ngood = nxyz ;

  srand48(gseed) ;  /* not really needed */

  /*-- z-score thresholds for the various p-values --*/

  zthr = (float *)malloc(sizeof(float)*npthr) ;
  for( ii=0 ; ii < npthr ; ii++ ){
    zthr[ii] = (float)zthresh( (do_2sid) ? 0.5*pthr[ii] : pthr[ii] ) ;
    /** ININFO_message("pthr=%8.5f  zthr=%6.3f",pthr[ii],zthr[ii]) ; **/
  }

  /*-- check niter vs smallest alpha level --*/

  if( niter*athr[nathr-1] < 10.0f )
    WARNING_message(
      "Smallest athr=%g ==> should have at least %d iterations instead of %d" ,
      athr[nathr-1] , (int)(10.0f/athr[nathr-1])+1 , niter ) ;

  /*-- blurring stuff --*/

  sigmax = FWHM_TO_SIGMA(fwhm_x) ; if( nx < 2 ) sigmax = 0.0f ;
  sigmay = FWHM_TO_SIGMA(fwhm_y) ; if( ny < 2 ) sigmay = 0.0f ;
  sigmaz = FWHM_TO_SIGMA(fwhm_z) ; if( nz < 2 ) sigmaz = 0.0f ;

  do_blur = (sigmax > 0.0f || sigmay > 0.0f || sigmaz > 0.0f ) ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Generate random smoothed masked image, with stdev=1. */

void generate_image( float *fim , unsigned short xran[] )
{
  register int ii ; register float sum ;

  /* random N(0,1) stuff */

  for( ii=0 ; ii < nxyz ; ii++ ) fim[ii] = zgaussian_sss(xran) ;

  /* smoothization */

  if( do_blur ){
    FIR_blur_volume_3d(nx,ny,nz,dx,dy,dz,fim,sigmax,sigmay,sigmaz) ;
    for( sum=0.0f,ii=0 ; ii < nxyz ; ii++ ) sum += fim[ii]*fim[ii] ;
    sum = sqrtf( nxyz / sum ) ;  /* fix stdev back to 1 */
    for( ii=0 ; ii < nxyz ; ii++ ) fim[ii] *= sum ;
  }

  /* maskizing */

  if( mask_vol != NULL ){
    for( ii=0 ; ii < nxyz ; ii++ ) if( !mask_vol[ii] ) fim[ii] = 0.0f ;
  }

  /* absolution? */

  if( do_2sid ){
    for( ii=0 ; ii < nxyz ; ii++ ) fim[ii] = fabsf(fim[ii]) ;
  }

  return ;
}

/*---------------------------------------------------------------------------*/

#define DALL MAX_CLUSTER_SIZE

/*! Put (i,j,k) into the current cluster, if it is nonzero. */

#define CPUT(i,j,k)                                             \
  do{ ijk = THREE_TO_IJK(i,j,k,nx,nxy) ;                        \
      if( mmm[ijk] ){                                           \
        if( nnow == nall ){ /* increase array lengths */        \
          nall += DALL + nall/2 ;                               \
          inow = (short *) realloc(inow,sizeof(short)*nall) ;   \
          jnow = (short *) realloc(jnow,sizeof(short)*nall) ;   \
          know = (short *) realloc(know,sizeof(short)*nall) ;   \
        }                                                       \
        inow[nnow] = i ; jnow[nnow] = j ; know[nnow] = k ;      \
        nnow++ ; mmm[ijk] = 0 ;                                 \
      } } while(0)

#define USE_MEMCHR

static int    *nall_g = NULL ;  /* per-thread workspaces */
static short **inow_g = NULL ;  /* for clusterizationing */
static short **jnow_g = NULL ;
static short **know_g = NULL ;

/*----------------------------------------------------------------------------*/

int find_largest_cluster_NN1( byte *mmm , int ithr )
{
   int max_size=0 ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km ;
   int nnow  , nall ; short *inow , *jnow , *know ;
#ifdef USE_MEMCHR
   byte *mch ;
#endif

   /* get workspace for this thread */

   nall = nall_g[ithr] ;
   inow = inow_g[ithr] ; jnow = jnow_g[ithr] ; know = know_g[ithr] ;

   ijk_last = 0 ;  /* start scanning at the start */

   while(1) {
     /* find next nonzero point in mmm array */

#ifndef USE_MEMCHR
     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( mmm[ijk] ) break ;
#else
     mch = memchr( mmm+ijk_last , 1 , nxyz-ijk_last ) ;  /* quicker search */
     if( mch == NULL ) ijk = nxyz ;
     else              ijk = mch - mmm ;
#endif
     if( ijk == nxyz ) break ;  /* didn't find any! */
     ijk_last = ijk+1 ;         /* start here next time */

     mmm[ijk] = 0 ;                                /* clear found point */

     nnow = 1 ;                                    /* # pts in cluster */
     IJK_TO_THREE(ijk, inow[0],jnow[0],know[0] , nx,nxy) ;

     /* loop over points in cluster, checking their neighbors,
        growing the cluster if we find any that belong therein */

     for( icl=0 ; icl < nnow ; icl++ ){
       ii = inow[icl] ; jj = jnow[icl] ; kk = know[icl] ;
       im = ii-1      ; jm = jj-1      ; km = kk-1 ;
       ip = ii+1      ; jp = jj+1      ; kp = kk+1 ;

       if( im >= 0 ) CPUT(im,jj,kk) ;
       if( ip < nx ) CPUT(ip,jj,kk) ;
       if( jm >= 0 ) CPUT(ii,jm,kk) ;
       if( jp < ny ) CPUT(ii,jp,kk) ;
       if( km >= 0 ) CPUT(ii,jj,km) ;
       if( kp < nz ) CPUT(ii,jj,kp) ;
     }

     if( nnow > max_size ) max_size = nnow ;
   }

   if( nall > nall_g[ithr] ){  /* probably won't happen */
     nall_g[ithr] = nall ;
     inow_g[ithr] = inow ; jnow_g[ithr] = jnow ; know_g[ithr] = know ;
   }

   return max_size ;
}

/*----------------------------------------------------------------------------*/

int find_largest_cluster_NN2( byte *mmm , int ithr )
{
   int max_size=0 ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km ;
   int nnow  , nall ; short *inow , *jnow , *know ;
#ifdef USE_MEMCHR
   byte *mch ;
#endif

   /* get workspace for this thread */

   nall = nall_g[ithr] ;
   inow = inow_g[ithr] ; jnow = jnow_g[ithr] ; know = know_g[ithr] ;

   ijk_last = 0 ;  /* start scanning at the start */

   while(1) {
     /* find next nonzero point in mmm array */

#ifndef USE_MEMCHR
     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( mmm[ijk] ) break ;
#else
     mch = memchr( mmm+ijk_last , 1 , nxyz-ijk_last ) ;  /* quicker search */
     if( mch == NULL ) ijk = nxyz ;
     else              ijk = mch - mmm ;
#endif
     if( ijk == nxyz ) break ;  /* didn't find any! */
     ijk_last = ijk+1 ;         /* start here next time */

     mmm[ijk] = 0 ;                                /* clear found point */

     nnow = 1 ;                                    /* # pts in cluster */
     IJK_TO_THREE(ijk, inow[0],jnow[0],know[0] , nx,nxy) ;

     /* loop over points in cluster, checking their neighbors,
        growing the cluster if we find any that belong therein */

     for( icl=0 ; icl < nnow ; icl++ ){
       ii = inow[icl] ; jj = jnow[icl] ; kk = know[icl] ;
       im = ii-1      ; jm = jj-1      ; km = kk-1 ;
       ip = ii+1      ; jp = jj+1      ; kp = kk+1 ;

       if( im >= 0 ){  CPUT(im,jj,kk) ;
         if( jm >= 0 ) CPUT(im,jm,kk) ;  /* 2NN */
         if( jp < ny ) CPUT(im,jp,kk) ;  /* 2NN */
         if( km >= 0 ) CPUT(im,jj,km) ;  /* 2NN */
         if( kp < nz ) CPUT(im,jj,kp) ;  /* 2NN */
       }
       if( ip < nx ){  CPUT(ip,jj,kk) ;
         if( jm >= 0 ) CPUT(ip,jm,kk) ;  /* 2NN */
         if( jp < ny ) CPUT(ip,jp,kk) ;  /* 2NN */
         if( km >= 0 ) CPUT(ip,jj,km) ;  /* 2NN */
         if( kp < nz ) CPUT(ip,jj,kp) ;  /* 2NN */
       }
       if( jm >= 0 ){  CPUT(ii,jm,kk) ;
         if( km >= 0 ) CPUT(ii,jm,km) ;  /* 2NN */
         if( kp < nz ) CPUT(ii,jm,kp) ;  /* 2NN */
       }
       if( jp < ny ){  CPUT(ii,jp,kk) ;
         if( km >= 0 ) CPUT(ii,jp,km) ;  /* 2NN */
         if( kp < nz ) CPUT(ii,jp,kp) ;  /* 2NN */
       }
       if( km >= 0 )   CPUT(ii,jj,km) ;
       if( kp < nz )   CPUT(ii,jj,kp) ;
     }

     if( nnow > max_size ) max_size = nnow ;
   }

   if( nall > nall_g[ithr] ){  /* probably won't happen */
     nall_g[ithr] = nall ;
     inow_g[ithr] = inow ; jnow_g[ithr] = jnow ; know_g[ithr] = know ;
   }

   return max_size ;
}

/*----------------------------------------------------------------------------*/

int find_largest_cluster_NN3( byte *mmm , int ithr )
{
   int max_size=0 ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km ;
   int nnow  , nall ; short *inow , *jnow , *know ;
#ifdef USE_MEMCHR
   byte *mch ;
#endif

   /* get workspace for this thread */

   nall = nall_g[ithr] ;
   inow = inow_g[ithr] ; jnow = jnow_g[ithr] ; know = know_g[ithr] ;

   ijk_last = 0 ;  /* start scanning at the start */

   while(1) {
     /* find next nonzero point in mmm array */

#ifndef USE_MEMCHR
     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( mmm[ijk] ) break ;
#else
     mch = memchr( mmm+ijk_last , 1 , nxyz-ijk_last ) ;  /* quicker search */
     if( mch == NULL ) ijk = nxyz ;
     else              ijk = mch - mmm ;
#endif
     if( ijk == nxyz ) break ;  /* didn't find any! */
     ijk_last = ijk+1 ;         /* start here next time */

     mmm[ijk] = 0 ;                                /* clear found point */

     nnow = 1 ;                                    /* # pts in cluster */
     IJK_TO_THREE(ijk, inow[0],jnow[0],know[0] , nx,nxy) ;

     /* loop over points in cluster, checking their neighbors,
        growing the cluster if we find any that belong therein */

     for( icl=0 ; icl < nnow ; icl++ ){
       ii = inow[icl] ; jj = jnow[icl] ; kk = know[icl] ;
       im = ii-1      ; jm = jj-1      ; km = kk-1 ;
       ip = ii+1      ; jp = jj+1      ; kp = kk+1 ;

       if( im >= 0 ){  CPUT(im,jj,kk) ;
         if( jm >= 0 ) CPUT(im,jm,kk) ;  /* 2NN */
         if( jp < ny ) CPUT(im,jp,kk) ;  /* 2NN */
         if( km >= 0 ) CPUT(im,jj,km) ;  /* 2NN */
         if( kp < nz ) CPUT(im,jj,kp) ;  /* 2NN */
         if( jm >= 0 && km >= 0 ) CPUT(im,jm,km) ;  /* 3NN */
         if( jm >= 0 && kp < nz ) CPUT(im,jm,kp) ;  /* 3NN */
         if( jp < ny && km >= 0 ) CPUT(im,jp,km) ;  /* 3NN */
         if( jp < ny && kp < nz ) CPUT(im,jp,kp) ;  /* 3NN */
       }
       if( ip < nx ){  CPUT(ip,jj,kk) ;
         if( jm >= 0 ) CPUT(ip,jm,kk) ;  /* 2NN */
         if( jp < ny ) CPUT(ip,jp,kk) ;  /* 2NN */
         if( km >= 0 ) CPUT(ip,jj,km) ;  /* 2NN */
         if( kp < nz ) CPUT(ip,jj,kp) ;  /* 2NN */
         if( jm >= 0 && km >= 0 ) CPUT(ip,jm,km) ;  /* 3NN */
         if( jm >= 0 && kp < nz ) CPUT(ip,jm,kp) ;  /* 3NN */
         if( jp < ny && km >= 0 ) CPUT(ip,jp,km) ;  /* 3NN */
         if( jp < ny && kp < nz ) CPUT(ip,jp,kp) ;  /* 3NN */
       }
       if( jm >= 0 ){  CPUT(ii,jm,kk) ;
         if( km >= 0 ) CPUT(ii,jm,km) ;  /* 2NN */
         if( kp < nz ) CPUT(ii,jm,kp) ;  /* 2NN */
       }
       if( jp < ny ){  CPUT(ii,jp,kk) ;
         if( km >= 0 ) CPUT(ii,jp,km) ;  /* 2NN */
         if( kp < nz ) CPUT(ii,jp,kp) ;  /* 2NN */
       }
       if( km >= 0 )   CPUT(ii,jj,km) ;
       if( kp < nz )   CPUT(ii,jj,kp) ;
     }

     if( nnow > max_size ) max_size = nnow ;
   }

   if( nall > nall_g[ithr] ){  /* probably won't happen */
     nall_g[ithr] = nall ;
     inow_g[ithr] = inow ; jnow_g[ithr] = jnow ; know_g[ithr] = know ;
   }

   return max_size ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN1( int ipthr , float *fim , byte *bfim , int *mtab , int ithr )
{
  register int ii ; register float thr ; int siz ;

  thr = zthr[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
  siz = find_largest_cluster_NN1( bfim , ithr ) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN2( int ipthr , float *fim , byte *bfim , int *mtab , int ithr )
{
  register int ii ; register float thr ; int siz ;

  thr = zthr[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
  siz = find_largest_cluster_NN2( bfim , ithr ) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN3( int ipthr , float *fim , byte *bfim , int *mtab , int ithr )
{
  register int ii ; register float thr ; int siz ;

  thr = zthr[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
  siz = find_largest_cluster_NN3( bfim , ithr ) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/

static char * prob6(float p)
{
   static char str[16] ;
   sprintf(str,"%7.5f",p) ;
   return (str+1) ;
}

/*===========================================================================*/

int main( int argc , char **argv )
{
  int **max_table[4] ; int nnn , ipthr , first_mask=1 ;
  char *refit_cmd = NULL ;
#ifdef USE_OMP
  int ***mtab[4] ;
#endif

  /*----- does user request help menu? -----*/

  AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

  if( argc < 2 || strcmp(argv[1],"-help") == 0 ) display_help_menu() ;

  /*----- get the list of things to do -----*/

  mainENTRY("3dClustSim"); machdep();
  AFNI_logger("3dClustSim",argc,argv);
  PRINT_VERSION("3dClustSim"); AUTHOR("RW Cox and BD Ward");
  THD_check_AFNI_version("3dClustSim") ;

  get_options( argc , argv ) ;

  /*----- create some space for the results -----*/

  for( nnn=1 ; nnn <= 3 ; nnn++ ){
    if( do_NN[nnn] ){
      max_table[nnn] = (int **)malloc(sizeof(int *)*npthr) ;  /* array of tables */
      for( ipthr=0 ; ipthr < npthr ; ipthr++ )               /* create tables */
        max_table[nnn][ipthr] = (int *)calloc(sizeof(int),(max_cluster_size+1)) ;
    }
  }

 AFNI_OMP_START ;
#pragma omp parallel
 {
   int iter, ithr, ipthr, **mt[4] , nnn ;
   float *fim ; byte *bfim ; unsigned short xran[3] ;
   int vstep , vii ;

  /* create separate tables for each thread, if using OpenMP */
#ifdef USE_OMP
   ithr = omp_get_thread_num() ;
#pragma omp master  /* only in the master thread */
 {
   nthr = omp_get_num_threads() ;
   for( nnn=1 ; nnn <= 3 ; nnn++ ){
     if( do_NN[nnn] ) mtab[nnn] = (int ***)malloc(sizeof(int **) *nthr) ;
   }

   nall_g = (int *)   malloc(sizeof(int)    *nthr) ;  /* workspaces for */
   inow_g = (short **)malloc(sizeof(short *)*nthr) ;  /* find_largest_cluster() */
   jnow_g = (short **)malloc(sizeof(short *)*nthr) ;
   know_g = (short **)malloc(sizeof(short *)*nthr) ;
   if( verb ) INFO_message("Using %d OpenMP threads",nthr) ;
 }
#pragma omp barrier  /* all threads wait until the above is finished */
   /* create tables for each thread separately */
   for( nnn=1 ; nnn <= 3 ; nnn++ ){
     if( do_NN[nnn] ){
       mtab[nnn][ithr] = mt[nnn] = (int **)malloc(sizeof(int *)*npthr) ;
       for( ipthr=0 ; ipthr < npthr ; ipthr++ )
         mt[nnn][ipthr] = (int *)calloc(sizeof(int),(max_cluster_size+1)) ;
     }
   }

   /* create workspace for find_largest_cluster(), for this thread */

   nall_g[ithr] = DALL ;
   inow_g[ithr] = (short *) malloc(sizeof(short)*DALL) ;
   jnow_g[ithr] = (short *) malloc(sizeof(short)*DALL) ;
   know_g[ithr] = (short *) malloc(sizeof(short)*DALL) ;

   /* initialize random seed array for each thread separately */
   xran[2] = ( gseed        & 0xffff) + (unsigned short)ithr ;
   xran[1] = ((gseed >> 16) & 0xffff) - (unsigned short)ithr ;
   xran[0] = 0x330e                   + (unsigned short)ithr ;

#else /* not OpenMP ==> only one set of tables */
   ithr = 0 ;
   xran[2] = ( gseed        & 0xffff) ;
   xran[1] = ((gseed >> 16) & 0xffff) ;
   xran[0] = 0x330e ;
   nall_g = (int *)   malloc(sizeof(int)    *nthr) ;
   inow_g = (short **)malloc(sizeof(short *)*nthr) ;
   jnow_g = (short **)malloc(sizeof(short *)*nthr) ;
   know_g = (short **)malloc(sizeof(short *)*nthr) ;
   nall_g[ithr] = DALL ;
   inow_g[ithr] = (short *) malloc(sizeof(short)*DALL) ;
   jnow_g[ithr] = (short *) malloc(sizeof(short)*DALL) ;
   know_g[ithr] = (short *) malloc(sizeof(short)*DALL) ;
   for( nnn=1 ; nnn <= 3 ; nnn++ )
     if( do_NN[nnn] ) mt[nnn] = max_table[nnn] ;
#endif

   fim  = (float *)malloc(sizeof(float)*nxyz) ;  /* image space */
   bfim = (byte * )malloc(sizeof(byte) *nxyz) ;

   vstep = (int)( niter / (nthr*50.0f) + 0.901f) ;
   vii   = 0 ;
   if( ithr == 0 && verb ) fprintf(stderr,"Simulating:") ;

  /*----- Monte Carlo iterations -----*/

#pragma omp for
  for( iter=1 ; iter <= niter ; iter++ ){

    if( verb && ithr == 0 ){
      vii++ ; if( vii%vstep == vstep/2 ) vstep_print() ;
    }

    generate_image( fim , xran ) ;

    for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
      if( do_NN[1] )
        gather_stats_NN1( ipthr , fim , bfim , mt[1][ipthr] , ithr ) ;
      if( do_NN[2] )
        gather_stats_NN2( ipthr , fim , bfim , mt[2][ipthr] , ithr ) ;
      if( do_NN[3] )
        gather_stats_NN3( ipthr , fim , bfim , mt[3][ipthr] , ithr ) ;
    }

  } /* end of simulation loop */

  free(fim) ; free(bfim) ;
  free(inow_g[ithr]) ; free(jnow_g[ithr]) ; free(know_g[ithr]) ;

  if( ithr == 0 && verb ) fprintf(stderr,"\n") ;

 } /* end OpenMP parallelization */
 AFNI_OMP_END ;

   /*-------- sum tables from various threads into one result ----------*/

#ifdef USE_OMP
   { int ithr , ii , ipthr , **mt , *mth ;
     for( nnn=1 ; nnn <= 3 ; nnn++ ){
       if( do_NN[nnn] ){
         for( ithr=0 ; ithr < nthr ; ithr++ ){
           mt = mtab[nnn][ithr] ;
           for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
             mth = mt[ipthr] ;
             for( ii=1 ; ii <= max_cluster_size ; ii++ )
               max_table[nnn][ipthr][ii] += mth[ii] ;
           }
         }
       }
     }
   }
#endif

  enable_mcw_malloc() ;

  /*---------- compute and print the output table ----------*/

  { double *alpha , aval , ahi,alo ;
    float **clust_thresh , cmax=0.0f ;
    int ii , itop , iathr ;
    char *commandline = tross_commandline("3dClustSim",argc,argv) ;
    char fname[THD_MAX_NAME] , pname[THD_MAX_NAME] ;
    char *amesg = NULL ;  /* 20 Jan 2011 */

    alpha        = (double *)malloc(sizeof(double)*(max_cluster_size+1)) ;
    clust_thresh = (float **)malloc(sizeof(float *)*npthr) ;
    for( ipthr=0 ; ipthr < npthr ; ipthr++ )
      clust_thresh[ipthr] = (float *)malloc(sizeof(float)*nathr) ;

    for( nnn=1 ; nnn <= 3 ; nnn++ ){
      if( !do_NN[nnn] ) continue ;
      for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
        for( itop=ii=1 ; ii <= max_cluster_size ; ii++ ){
          alpha[ii] = max_table[nnn][ipthr][ii] / (double)niter ;
          if( alpha[ii] > 0.0 ) itop = ii ;
        }
        for( ii=itop-1 ; ii >= 1 ; ii-- ) alpha[ii] += alpha[ii+1] ;
#if 0
INFO_message("pthr[%d]=%g itop=%d",ipthr,pthr[ipthr],itop) ;
for( ii=1 ; ii <= itop ; ii++ )
  fprintf(stderr," %d=%g",ii,alpha[ii]) ;
fprintf(stderr,"\n") ;
#endif
        for( iathr=0 ; iathr < nathr ; iathr++ ){
          aval = athr[iathr] ;
          if( aval > alpha[1] ){  /* unpleasant situation */
            ii = 1 ;
            amesg = THD_zzprintf(
                     amesg ,
                     "   NN=%d  pthr=%9.6f  alpha=%6.3f [max simulated alpha=%6.3f]\n" ,
                     nnn , pthr[ipthr] , aval , alpha[1] ) ;
          } else {
            for( ii=1 ; ii < itop ; ii++ ){
              if( alpha[ii] >= aval && alpha[ii+1] <= aval ) break ;
            }
          }

          alo=alpha[ii] ; ahi=alpha[ii+1] ;
          if( do_lohi ){
            aval = ii ;    /* for debugging */
          } else {
            if( alo >= 1.0 ) alo = 1.0 - 0.1/niter ;
            if( ahi <= 0.0 ) ahi = 0.1/niter ;
            if( ahi >= alo ) ahi = 0.1*alo ;
            aval = log(-log(1.0-aval)) ;
            alo  = log(-log(1.0-alo)) ;
            ahi  = log(-log(1.0-ahi)) ;
            aval = ii + (aval-alo)/(ahi-alo) ;
                 if( aval < 1.0 ) aval = 1.0 ;
            else if( nodec      ) aval = (int)(aval+0.951) ;
          }
          clust_thresh[ipthr][iathr] = aval ;

          if( clust_thresh[ipthr][iathr] > cmax ) cmax = clust_thresh[ipthr][iathr] ;
        }
      }

      if( do_lohi == 0 ){
        /* edit each column to increase as pthr increases [shouldn't be needed] */

        for( iathr=0 ; iathr < nathr ; iathr++ ){
          for( ipthr=npthr-2 ; ipthr >= 0 ; ipthr-- ){
            if( clust_thresh[ipthr][iathr] < clust_thresh[ipthr+1][iathr] )
              clust_thresh[ipthr][iathr] = clust_thresh[ipthr+1][iathr] ;
          }
        }

        /* edit each row to increase as athr decreases [shouldn't be needed] */

        for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
          for( iathr=1 ; iathr < nathr ; iathr++ ){
            if( clust_thresh[ipthr][iathr] < clust_thresh[ipthr][iathr-1] )
              clust_thresh[ipthr][iathr] = clust_thresh[ipthr][iathr-1] ;
          }
        }
      }

#if 0
      if( !nodec && !do_niml && cmax > 9999.9f ){  /* if largest is way big, */
        for( ipthr=0 ; ipthr < npthr ; ipthr++ ){  /* then truncate to ints. */
          for( iathr=0 ; iathr < nathr ; iathr++ ){
            aval = clust_thresh[ipthr][iathr] ;
            aval = (int)(aval+0.951) ;
            clust_thresh[ipthr][iathr] = aval ;
          }
        }
        nodec = 1 ;
      }
#endif

MPROBE ;

      if( prefix != NULL ){
        sprintf(pname,"%s.NN%d.",prefix,nnn) ;
      } else {
        fflush(stderr) ; fflush(stdout) ;
      }

      if( do_1D ){  /* output in 1D format */
        FILE *fp = stdout ;
        if( prefix != NULL ){
          strcpy(fname,pname) ; strcat(fname,"1D") ; fp = fopen(fname,"w") ;
          if( fp == NULL ){
            ERROR_message("Can't open file %s -- using stdout",fname) ;
            fp = stdout ;
          }
        }
        fprintf(fp,
         "# %s\n"
         "# Grid: %dx%dx%d %.2fx%.2fx%.2f mm^3 (%d voxels%s)\n"
         "#\n"
         "# CLUSTER SIZE THRESHOLD(pthr,alpha) in Voxels\n"
         "# -NN %d  | alpha = Prob(Cluster >= given size)\n"
         "#  pthr  |" ,
         commandline ,
         nx,ny,nz , dx,dy,dz ,
         mask_ngood , (mask_ngood < nxyz) ? " in mask" : "\0" , nnn ) ;
#if 0
        for( iathr=0 ; iathr < nathr ; iathr++ ) fprintf(fp," %6.3f",athr[iathr]) ;
#else
        for( iathr=0 ; iathr < nathr ; iathr++ ) fprintf(fp," %s",prob6(athr[iathr])) ;
#endif
        fprintf(fp,"\n"
         "# ------ |" ) ;
        for( iathr=0 ; iathr < nathr ; iathr++ ) fprintf(fp," ------") ;
        fprintf(fp,"\n") ;
        for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
          fprintf(fp,"%9.6f ",pthr[ipthr]) ;
          for( iathr=0 ; iathr < nathr ; iathr++ ){
            if( nodec )
              fprintf(fp,"%7d"  ,(int)clust_thresh[ipthr][iathr]) ;
            else if( clust_thresh[ipthr][iathr] <= 9999.9f )
              fprintf(fp,"%7.1f",     clust_thresh[ipthr][iathr]) ;
            else
              fprintf(fp,"%7.0f",     clust_thresh[ipthr][iathr]) ;
          }
          fprintf(fp,"\n") ;
        }
      }

      if( do_niml ){ /* output in NIML format */
        NI_element *nel ; float *vec ; char buf[1024] , *bbb ; NI_float_array nfar ;
        sprintf(buf,"3dClustSim_NN%d",nnn) ;
        nel = NI_new_data_element( buf , npthr ) ;
        vec = (float *)malloc(sizeof(float)*MAX(npthr,nathr)) ;
        for( iathr=0 ; iathr < nathr ; iathr++ ){
          for( ipthr=0 ; ipthr < npthr ; ipthr++ )
            vec[ipthr] = clust_thresh[ipthr][iathr] ;
          NI_add_column( nel , NI_FLOAT , vec ) ;
        }
          NI_set_attribute( nel , "commandline" , commandline ) ;
        sprintf(buf,"%d,%d,%d",nx,ny,nz) ;
          NI_set_attribute(nel,"nxyz",buf) ;
        sprintf(buf,"%.3f,%.3f,%.3f",dx,dy,dz) ;
          NI_set_attribute(nel,"dxyz",buf) ;
        sprintf(buf,"%.2f,%.2f,%.2f",fwhm_x,fwhm_y,fwhm_z) ;
          NI_set_attribute(nel,"fwhmxyz",buf) ;
        sprintf(buf,"%d",niter) ;
          NI_set_attribute(nel,"iter",buf) ;
        for( ipthr=0 ; ipthr < npthr ; ipthr++ ) vec[ipthr] = pthr[ipthr] ;
        nfar.num = npthr ; nfar.ar = vec ; bbb = NI_encode_float_list(&nfar,",") ;
          NI_set_attribute(nel,"pthr",bbb) ; NI_free(bbb) ;
        for( iathr=0 ; iathr < nathr ; iathr++ ) vec[iathr] = athr[iathr] ;
        nfar.num = nathr ; nfar.ar = vec ; bbb = NI_encode_float_list(&nfar,",") ;
          NI_set_attribute(nel,"athr",bbb) ; NI_free(bbb) ;
        if( mask_dset != NULL ){
            NI_set_attribute(nel,"mask_dset_idcode",DSET_IDCODE_STR(mask_dset)) ;
            NI_set_attribute(nel,"mask_dset_name"  ,DSET_HEADNAME(mask_dset)) ;
          sprintf(buf,"%d",mask_ngood) ;
            NI_set_attribute(nel,"mask_count",buf) ;
        }
        if( prefix != NULL ){ strcpy(fname,pname) ; strcat(fname,"niml") ; }
        else                  strcpy(fname,"stdout:") ;
        NI_write_element_tofile( fname , nel , NI_TEXT_MODE ) ;
        NI_free_element( nel ) ;
        if( prefix != NULL ){
          if( refit_cmd == NULL )
            refit_cmd = THD_zzprintf( refit_cmd , "3drefit " ) ;
          refit_cmd = THD_zzprintf( refit_cmd ,
                                    "-atrstring AFNI_CLUSTSIM_NN%d file:%s " ,
                                    nnn , fname ) ;
        }
        if( prefix != NULL && mask_vol != NULL && first_mask ){
          bbb = mask_to_b64string( mask_nvox , mask_vol ) ; first_mask = 0 ;
          if( bbb != NULL ){
            FILE *fp ;
            sprintf(fname,"%s.mask",prefix) ; fp = fopen(fname,"w") ;
            if( fp != NULL ){ fprintf(fp,"%s\n",bbb); fclose(fp); }
            free(bbb) ;
            refit_cmd = THD_zzprintf( refit_cmd ,
                                      "-atrstring AFNI_CLUSTSIM_MASK file:%s " ,
                                      fname) ;
          }
        }
      } /* end of NIML output */

    } /* end of loop over nnn = NN degree */

    if( amesg != NULL ){
      /* WARNING_message cannot handle "%s" with a very long string (>16K),
         so break this up a little                      20 Feb 2014 [rickr] */
      /* option: change to vsnprintf() in output_message()?                 */
      WARNING_message("Simulation not effective for these cases:\n\n");
      fprintf(stderr, "%s\n", amesg);
      fprintf(stderr, 
                "*+ This means that not enough clusters, of any size, +*\n"
                "     of voxels at or below each pthr threshold, were +*\n"
                "     found to estimate at each alpha level.          +*\n"
                "*+ In other words, the probability that noise-only   +*\n"
                "     data (of the given smoothness) will cause       +*\n"
                "     above-threshold (at the given pthr) clusters is +*\n"
                "     smaller than the desired alpha levels.          +*\n"
                "*+ This problem can arise when the masked region     +*\n"
                "     being simulated is small and at the same time   +*\n"
                "     the smoothness (FWHM) is large.                 +*\n"
                "*+ Read the 'CAUTION and CAVEAT' section at the end  +*\n"
                "   of the '-help' output for a longer explanation.   +*\n\n");
      free(amesg) ;
    }

  } /* end of outputizationing */

  /*------- a minor aid for the pitiful helpless user [e.g., me] -------*/

  if( refit_cmd != NULL )
    INFO_message("Command fragment to put cluster results into a dataset header;\n"
                 " + Append the name of the datasets to be patched to this command:\n"
                 " %s" , refit_cmd ) ;

  /*-------- run away screaming into the night ----- AAUUGGGHHH!!! --------*/

  exit(0);
}
