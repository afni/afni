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

/*----------------------------------------------------------------------------*/
#include "zgaussian.c"         /** Ziggurat Gaussian random number generator **/
/*----------------------------------------------------------------------------*/
#include "mri_radial_random_field.c" /** 3D FFT-based random field generator **/
/*----------------------------------------------------------------------------*/

#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#define MAX_NAME_LENGTH  THD_MAX_NAME /* max. string length for file names */
#define MAX_CLUSTER_SIZE 99999        /* max. size of cluster for freq. table */

#undef  ALLOW_LOHI
#ifndef ALLOW_LOHI
# define do_lohi     0
#else
static int do_lohi = 0 ;
#endif

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

/*-- global variables for ACF simulation [30 Nov 2015] --*/

static int   do_acf = 0    ;
static float acf_a  = 0.0f ;
static float acf_b  = 0.0f ;
static float acf_c  = 0.0f ;
static float acf_parm[3] = {0.0f,0.0f,0.0f} ;
static int   acf_nxx=0 , acf_nyy=0 , acf_nzz=0 ;
static MRI_IMAGE **acf_aim=NULL , **acf_bim=NULL ;
static complex   **acf_tar=NULL ; static int acf_ntar=0 ;
static MRI_IMAGE *acf_wim=NULL ;

static int do_classic = 0 ; /* does nothing (yet) */

static int   ex_pad = 0  ;  /* 12 May 2015 -- allow for padding the volume */
static int   ey_pad = 0  ;
static int   ez_pad = 0  ;
static int   do_pad = 0  ;
static int   nx_pad      ;
static int   ny_pad      ;
static int   nz_pad      ;
static int   nxy_pad     ;
static int   nxyz_pad    ;
static int   allow_padding = 1 ;  /* on by default */

static float tdof = 0.0f ;        /* 26 May 2015 - secret stuff */

static float sigmax , sigmay , sigmaz ;
static int do_blur = 0 ;

static int                 do_ssave = 0 ;     /* 24 Apr 2014 */
static char           *ssave_prefix = NULL ;
static int             ssave_index  = 0 ;
static THD_3dim_dataset *ssave_dset = NULL ;

#define SSAVE_BLURRED   1
#define SSAVE_MASKED    2
#define SSAVE_ABSOLUTE  3  /* no longer used */

static int nodec   = 0 ;
static int do_niml = 0 ;
static int do_1D   = 1 ;
static int do_ball = 0 ;

static unsigned int gseed = 123456789 ;

#define PMAX 0.5

static double pthr_init[9] = { 0.05 , 0.02 , 0.01, 0.005, 0.002, 0.001, 0.0005, 0.0002, 0.0001 } ;
static double athr_init[4] = { 0.10 , 0.05 , 0.02 , 0.01 } ;

static int   npthr_lots     = 29 ;
static double pthr_lots[29] = { 0.10,    0.09,    0.08,    0.07,    0.06,
                                0.05,    0.04,    0.03,    0.02,    0.015,    0.01,
                                0.007,   0.005,   0.003,   0.002,   0.0015,   0.001,
                                0.0007,  0.0005,  0.0003,  0.0002,  0.00015,  0.0001,
                                0.00007, 0.00005, 0.00003, 0.00002, 0.000015, 0.00001 } ;

static int   npthr_mega     = 38 ;
static double pthr_mega[38] = { 0.100,   0.090,   0.080,   0.070,   0.060,
                                0.050,   0.045,   0.040,   0.035,   0.030,
                                0.025,   0.020,   0.015,   0.010,   0.009,
                                0.008,   0.007,   0.006,   0.005,   0.004,
                                0.003,   0.002,   0.001,   0.0009,  0.0008,
                                0.0007,  0.0006,  0.0005,  0.0004,  0.0003,
                                0.0002,  0.0001,  0.00007, 0.00005, 0.00003,
                                0.00002, 0.000015,0.00001 } ;

static int   nathr_lots     = 10 ;
static double athr_lots[10] = { 0.10, 0.09, .08, .07, .06, .05, .04, .03, .02, .01 } ;

static int   nathr_mega     = 22 ;
static double athr_mega[22] = { 0.100,0.095,0.090,0.085,0.080,0.075,0.070,0.065,0.060,0.055,
                                0.050,0.045,0.040,0.035,0.030,0.025,0.020,0.015,0.010,0.005,0.004,0.003 } ;

static int    npthr = 9 ;
static double *pthr = NULL ;

static float  *zthr_1sid = NULL ;
static float  *zthr_2sid = NULL ;

static int    nathr = 4 ;
static double *athr = NULL ;

/* the output:
     2D table of cluster size threshold as function
     of per-voxel threshold and volumetric alpha
     (will be interpolated from simulation results) */

static float **clust_thresh_1sid_NN1 = NULL ;
static float **clust_thresh_2sid_NN1 = NULL ;
static float **clust_thresh_bsid_NN1 = NULL ;

static float **clust_thresh_1sid_NN2 = NULL ;
static float **clust_thresh_2sid_NN2 = NULL ;
static float **clust_thresh_bsid_NN2 = NULL ;

static float **clust_thresh_1sid_NN3 = NULL ;
static float **clust_thresh_2sid_NN3 = NULL ;
static float **clust_thresh_bsid_NN3 = NULL ;

static int do_athr_sum = 0 ; /* 18 Dec 2015 */
static int athr_sum_bot=-1 , athr_sum_top=-1 ;

/* max_table_1sid[nnn][ipthr][cc] is the count
   of how often the maximum cluster had exact size cc voxels
   (out of niter trials), at the pthr[ipthr] voxel-wise threshold p-value,
   for the nnn-th NN level (1-3), for 1-sided voxel-wise thresholding.
   Mutatis mutandis for max_table_2sid and max_table_bsid (bi-sided threshold) */

static int **max_table_1sid[4] , **max_table_2sid[4] , **max_table_bsid[4] ;

#define SHAVE_MALLOC 1
#define SHAVE_MMAP   2
static int     do_shave =0 ;
size_t         shave_siz=0 ;
static int64_t shave_tot=0 ;
static short  *shave    =NULL ;

static int verb = 1 ;
static int nthr = 1 ;

#undef DECLARE_ithr   /* 30 Nov 2015 */
#ifdef USE_OMP
# define DECLARE_ithr const int ithr=omp_get_thread_num()
#else
# define DECLARE_ithr const int ithr=0
#endif

static int minmask = 128 ;   /* 29 Mar 2011 */

static char *prefix = NULL ;

#undef  PSMALL
#define PSMALL 1.e-15

/*----------------------------------------------------------------------------*/
/*! Threshold for upper tail probability of N(0,1) */

double zthresh( double pval )
{
        if( pval <= 0.0 ) pval = PSMALL ;
   else if( pval >= 1.0 ) pval = 1.0 - PSMALL ;
   return qginv(pval) ;
}

/*---------------------------------------------------------------------------*/

static int vsnn=0 ;

static void vstep_reset(void){ vsnn=0; }

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[vsnn%10] ) ;
   if( vsnn%10 == 9) fprintf(stderr,".") ;
   vsnn++ ;
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
   "************* IMPORTANT NOTE [Dec 2015] ***************************************\n"
   "A completely new method for estimating and using noise smoothness values is\n"
   "now available in 3dFWHMx and 3dClustSim. This method is implemented in the\n"
   "'-acf' options to both programs.  'ACF' stands for (spatial) AutoCorrelation\n"
   "Function, and it is estimated by calculating moments of differences out to\n"
   "a larger radius than before.\n"
   "\n"
   "Notably, real FMRI data does not actually have a Gaussian-shaped ACF, so the\n"
   "estimated ACF is then fit (in 3dFWHMx) to a mixed model (Gaussian plus\n"
   "mono-exponential) of the form\n"
   "  ACF(r) = a * exp(-r*r/(2*b*b)) + (1-a)*exp(-r/c)\n"
   "where 'r' is the radius, and 'a', 'b', 'c' are the fitted parameters.\n"
   "The apparent FWHM from this model is usually somewhat larger in real data\n"
   "than the FWHM estimated from just the nearest-neighbor differences used\n"
   "in the 'classic' analysis.\n"
   "\n"
   "The longer tails provided by the mono-exponential are also significant.\n"
   "3dClustSim has also been modified to use the ACF model given above to generate\n"
   "noise random fields.\n"
   "\n"
   "**----------------------------------------------------------------------------**\n"
   "** The take-away (TL;DR or summary) message is that the 'classic' 3dFWHMx and **\n"
   "** 3dClustSim analysis, using a pure Gaussian ACF, is not very correct for    **\n"
   "** FMRI data -- I cannot speak for PET or MEG data.                           **\n"
#if 0
   "**                                                                            **\n"
   "** You should start using  the '-acf' options in your own scripts.  AFNI      **\n"
   "** scripts from afni_proc.py are moving away from the 'classic' method.  At   **\n"
   "** some point in the future, '-acf' will become the default in both programs, **\n"
   "** and you will have to actively specify '-classic' to use the older method.  **\n"
#endif
   "**----------------------------------------------------------------------------**\n"
   "\n"
   "** ---------------------------------------------------------------------------**\n"
   "** IMPORTANT CHANGES -- February 2015 ******************************************\n"
   "** ---------------------------------------------------------------------------**\n"
   "** In the past, 3dClustSim did '1-sided' testing; that is, the random dataset\n"
   "** of Gaussian noise-only values is generated, and then it is thresholded on\n"
   "** the positive side so that the N(0,1) upper tail probability is pthr.\n"
   "**\n"
   "** NOW, 3dClustSim does 3 different types of thresholding:\n"
   "**   1-sided: as above\n"
   "**   2-sided: where positive and negative values above the threshold\n"
   "**            are included, and then clustered together\n"
   "**            (in this case, the threshold on the Gaussian values is)\n"
   "**            (fixed so that the 1-sided tail probability is pthr/2.)\n"
   "**  bi-sided: where positive values and negative values above the\n"
   "**            threshold are clustered SEPARATELY (with the 2-sided threshold)\n"
   "** For high levels of smoothness, the results from bi-sided and 2-sided are\n"
   "** very similar -- since for smooth data, it is unlikely that large clusters of\n"
   "** positive and negative values will be next to each other. With high smoothness,\n"
   "** it is also true that the 2-sided results for 2*pthr will be similar to the\n"
   "** 1-sided results for pthr, for the same reason. Since 3dClustSim is meant to be\n"
   "** useful when the noise is NOT very smooth, we provide tables for all 3 cases.\n"
   "**\n"
   "** In particular, note that when the AFNI GUI threshold is set to a t-statistic,\n"
   "** 2-sided testing is what is usually appropriate -- in that case, the cluster\n"
   "** size thresholds tend to be smaller than the 1-sided case, which means that\n"
   "** more clusters tend to be significant than in the past.\n"
   "**\n"
   "** In addition, the 3 different NN approaches (NN=1, NN=2, NN=3) are ALL\n"
   "** always computed now.  That is, 9 different tables are produced, each\n"
   "** of which has its proper place when combined with the AFNI Clusterize GUI.\n"
   "** The 3 different NN methods are:\n"
   "**  1 = Use first-nearest neighbor clustering\n"
   "**      * above threshold voxels cluster together if faces touch\n"
   "**  2 = Use second-nearest neighbor clustering\n"
   "**      * voxels cluster together if faces OR edges touch\n"
   "**  3 = Use third-nearest neighbor clustering\n"
   "**      * voxels cluster together if faces OR edges OR corners touch\n"
   "** The clustering method only makes a difference at higher (less significant)\n"
   "** values of pthr.   At small values of pthr (more significant),  all three\n"
   "** clustering methods will give very similar results.\n"
   "**\n"
   "**** PLEASE NOTE that the NIML outputs from this new version are not named the\n"
   "**** same as those from the older version. Thus, any script that takes the NIML\n"
   "**** format tables and inserts them into an AFNI dataset header must be modified\n"
   "**** to match the new names. The 3drefit command fragment output at the end of\n"
   "**** this program (and echoed into file '3dClustSim.cmd') shows the new form\n"
   "**** of the names involved.\n"
   "**** -------------------------------------------------------------------------**\n"
   "**** SMOOTHING CHANGE -- May 2015 **********************************************\n"
   "** ---------------------------------------------------------------------------**\n"
   "** It was pointed out to me (by Anders Eklund and Tom Nichols) that smoothing\n"
   "** the simulated data over a finite volume introduces 2 artifacts, which might\n"
   "** be called 'edge effects'.  To minimize these problems, this program now makes\n"
   "** extra-large (padded) simulated volumes before blurring, and then trims those\n"
   "** back down to the desired size, before continuing with the thresholding and\n"
   "** cluster-counting steps.  To run 3dClustSim without this padding added, use\n"
   "** the new '-nopad' option.\n"
#if 0
   "** Also see the manuscript\n"
   "**   A Eklund, T Nichols, M Andersson, and H Knutsson.\n"
   "**   Empirically Investigating the Statistical Validity of SPM, FSL and AFNI\n"
   "**   for Single Subject FMRI Analysis.\n"
#endif
   "**** -------------------------------------------------------------------------**\n"
   "\n"
   "-------\n"
   "OPTIONS  [at least 1 option is required, or you'll get this help message!]\n"
   "-------\n"
   " ******* Specify the volume over which the simulation will occur *******\n"
   "\n"
   "  -----** (a) Directly give the spatial domain that will be used **-----\n"
   "\n"
   "-nxyz n1 n2 n3 = Size of 3D grid to use for simulation\n"
   "                  [default values = 64 64 32]\n"
   "-dxyz d1 d2 d3 = give all 3 voxel sizes at once\n"
   "                  [default values = 3.5 3.5 3.5]\n"
   "-BALL          = inside the 3D grid, mask off points outside a ball\n"
   "                  at the center of the grid and touching the edges;\n"
   "                  this will keep about 1/2 the points in the 3D grid.\n"
   "                  [default = use all voxels in the 3D grid]\n"
   "\n"
   "  -----** OR: (b) Specify the spatial domain using a dataset mask **-----\n"
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
   "  ---** the remaining options control how the simulation is done **---\n"
   "\n"
   "-fwhm s         = Gaussian filter width (all 3 dimensions) in mm (non-negative)\n"
   "                   [default = 0.0 = no smoothing]\n"
   "                  * If you wish to set different smoothing amounts for each\n"
   "                    axis, you can instead use option\n"
   "                      -fwhmxyz sx sy sz\n"
   "                    to specify the three values separately.\n"
   "\n"
   "-acf a b c      = Alternative to Gaussian filtering: use the spherical\n"
   "                  autocorrelation function parameters output by 3dFWHMx\n"
   "                  to do non-Gaussian (long-tailed) filtering.\n"
   "                  * Using '-acf' will make '-fwhm' pointless!\n"
   "                  * The 'a' parameter must be between 0 and 1.\n"
   "                  * The 'b' and 'c' parameters (scale radii) must be positive.\n"
   "                  * The spatial autocorrelation function is given by\n"
   "                      ACF(r) = a * exp(-r*r/(2*b*b)) + (1-a)*exp(-r/c)\n"
   "  >>---------->>*** Combined with 3dFWHMx, the '-acf' method is now the\n"
   "                    recommended way to generate clustering statistics in AFNI!\n"
   "\n"
   "-nopad          = The program now [12 May 2015] adds 'padding' slices along\n"
   "                   each face to allow for edge effects of the smoothing process.\n"
   "                   If you want to turn this feature off, use the '-nopad' option.\n"
   "                  * For example, if you want to compare the 'old' (un-padded)\n"
   "                    results with the 'new' (padded) results.\n"
   "                  * '-nopad' has no effect when '-acf' is used, since that option\n"
   "                    automatically pads the volume when creating it (via FFTs) and\n"
   "                    then truncates it back to the desired size for clustering.\n"
   "\n"
   "-pthr p1 .. pn = list of uncorrected (per voxel) p-values at which to\n"
   "                  threshold the simulated images prior to clustering.\n"
   "                  [default = 0.05 0.02 0.01 0.005 0.002 0.001 0.0005 0.0002 0.0001]\n"
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
   "-MEGA          = adds even MORE values to the '-pthr' and '-athr' grids.\n"
   "\n"
   "-iter n        = number of Monte Carlo simulations [default = 10000]\n"
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
   "\n"
   "-both          = Output the table in XML/NIML format AND in .1D format.\n"
   "                  * You probably want to use '-prefix' with this option!\n"
   "                    Otherwise, everything is mixed together on stdout.\n"
   "                  * '-both' implies 'niml' which implies '-LOTS'.\n"
   "                    So '-pthr' (if desired) should follow '-both'/'-niml'\n"
   "\n"
   "-prefix ppp    = Write output for NN method #k to file 'ppp.NNk_Xsided.1D',\n"
   "                  for k=1, 2, 3, and for X=1sided, 2sided, bisided.\n"
   "                  * If '-prefix is not used, results go to standard output.\n"
   "                  * If '-niml' is used, the filename is 'ppp.NNk_Xsided.niml'.\n"
   "                    To be clear, the 9 files that will be named\n"
   "                      ppp.NN1_1sided.niml ppp.NN1_2sided.niml ppp.NN1_bisided.niml\n"
   "                      ppp.NN2_1sided.niml ppp.NN2_2sided.niml ppp.NN2_bisided.niml\n"
   "                      ppp.NN3_1sided.niml ppp.NN3_2sided.niml ppp.NN3_bisided.niml\n"
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
   "                  * Put this option first to silence most informational messages.\n"
   "\n"
   " -ssave:TYPE ssprefix = Save the un-thresholded generated random volumes into\n"
   "                        datasets ('-iter' of them). Here, 'TYPE' is one of these:\n"
   "                          * blurred == save the blurred 3D volume before masking\n"
   "                          * masked  == save the blurred volume after masking\n"
   "                        The output datasets will actually get prefixes generated\n"
   "                        with the string 'ssprefix' being appended by a 6 digit\n"
   "                        integer (the iteration index), starting at 000000.\n"
   "                        (You can use SOMETHING.nii as a prefix; it will work OK.)\n"
   "                        N.B.: This option will slow the program down a lot,\n"
   "                              and is intended to help just one specific user.\n"
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
   "     3drefit -atrstring AFNI_CLUSTSIM_NN1_1sided file:CStemp.NN1_1sided.niml \\\n"
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
   "    are corrected for multiple comparisons WITHIN a cluster.\n"
   "\n"
   "* AFNI will use the NN1, NN2, NN3 tables as needed in its Clusterize\n"
   "  interface if they are all stored in the statistics dataset header,\n"
   "  depending on the NN level chosen in the Clusterize controller.\n"
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
   "SAMPLE OUTPUT from the command '3dClustSim -fwhm 7' [only the NN=1 1-sided results]\n"
   "-------------\n"
   "# 3dClustSim -fwhm 7\n"
   "# 1-sided thresholding\n"
   "# Grid: 64x64x32 3.50x3.50x3.50 mm^3 (131072 voxels)\n"
   "#\n"
   "# CLUSTER SIZE THRESHOLD(pthr,alpha) in Voxels\n"
   "# -NN 1  | alpha = Prob(Cluster >= given size)\n"
   "#  pthr  |  0.100  0.050  0.020  0.010\n"
   "# ------ | ------ ------ ------ ------\n"
   " 0.050000   162.5  182.2  207.8  225.7\n"
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
  char * ep;
  int nopt=1 , ii , have_pthr=0;

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

    /**** -tdof dof ****/

    if( strcmp(argv[nopt],"-tdof") == 0 ){  /* 26 May 2015 -- hidden option */
      nopt++ ; if( nopt >= argc ) ERROR_exit("need argument after -tdof!") ;
      tdof = (float)strtod(argv[nopt],NULL) ;
      if( tdof > 0.0f && tdof < 4.0f ) ERROR_exit("illegal value after -tdof") ;
      nopt++ ; continue ;
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
      fwhm_x = (float)strtod(argv[nopt],&ep) ;
      if( fwhm_x < 0.0f || ep == argv[nopt] )
         ERROR_exit("illegal value after -fwhm") ;
      fwhm_y = fwhm_z = fwhm_x ; nopt++; continue;
    }

    if( strncasecmp(argv[nopt],"-classic",6) == 0 ){   /* 01 Dec 2015 */
      do_classic = 1 ; nopt++ ; continue ;
    }

    /*-----    -acf a b c   [30 Nov 2015] -----*/

    if( strcasecmp(argv[nopt],"-acf") == 0 ){
      nopt++; if( nopt+2 >= argc ) ERROR_exit("need 3 arguments after -acf");
      acf_a = (float)strtod(argv[nopt++],&ep) ;
      if( acf_a < 0.0f || acf_a > 1.0f || ep == argv[nopt-1] )
        ERROR_exit("-acf: 'a' value should be between 0 and 1 :-(") ;
      acf_b = (float)strtod(argv[nopt++],&ep) ;
      if( acf_b <= 0.0f || ep == argv[nopt-1] )
        ERROR_exit("-acf: 'b' value should be positive :-(") ;
      acf_c = (float)strtod(argv[nopt++],&ep) ;
      if( acf_c <= 0.0f || ep == argv[nopt-1] )
        ERROR_exit("-acf: 'c' value should be positive :-(") ;
      do_acf = 1 ; continue ;
    }

    /*-----   -nopad     -----*/

    if( strcasecmp(argv[nopt],"-nopad") == 0 ){  /* 12 May 2015 */
      allow_padding = 0 ; nopt++ ; continue ;
    }

    /*-----   -fwhmxyz s s s   -----*/

    if( strcasecmp(argv[nopt],"-fwhmxyz") == 0 ){
      nopt++; if(nopt+2 >= argc) ERROR_exit("need 3 arguments after -fwhmxyz");
      fwhm_x = (float)strtod(argv[nopt++],&ep) ;
      if( fwhm_x < 0.0f || ep == argv[nopt-1] )
         ERROR_exit("illegal x value after -fwhmxyz") ;
      fwhm_y = (float)strtod(argv[nopt++],&ep) ;
      if( fwhm_y < 0.0f || ep == argv[nopt-1] )
         ERROR_exit("illegal y value after -fwhmxyz") ;
      fwhm_z = (float)strtod(argv[nopt++],&ep) ;
      if( fwhm_z < 0.0f || ep == argv[nopt-1] )
         ERROR_exit("illegal z value after -fwhmxyz") ;
      continue;
    }

    /*-----   -iter n  -----*/

    if( strcmp(argv[nopt],"-iter") == 0 || strncmp(argv[nopt],"-nite",5) == 0 ){
      nopt++;
      if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
      niter = (int)strtod(argv[nopt],NULL) ;
      if( niter < 2000 && !do_ssave ){
        WARNING_message("-iter %d replaced by 2000",niter) ; niter = 2000 ;
      } else if( niter < 1 ){
        ERROR_exit("-iter %s is illegal :-(",argv[nopt]) ;
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

    /*-----   -MEGA     -----*/

    if( strcasecmp(argv[nopt],"-MEGA") == 0 ){   /* 18 Dec 2015 */
      npthr = npthr_mega ;
      pthr = (double *)realloc(pthr,sizeof(double)*npthr) ;
      memcpy( pthr , pthr_mega , sizeof(double)*npthr ) ;
      nathr = nathr_mega ;
      athr = (double *)realloc(athr,sizeof(double)*nathr) ;
      memcpy( athr , athr_mega , sizeof(double)*nathr ) ;
      athr_sum_bot = 13 ; athr_sum_top = 31 ;
      if( niter < 30000 ) niter = 30000 ;
      nopt++ ; continue ;
    }

    /*-----  -sumup     -----*/

    if( strcasecmp(argv[nopt],"-sumup") == 0 ){  /* 18 Dec 2015 */
      do_athr_sum++ ; nopt++ ; continue ;
    }

#if 0
    /*-----   -2sided   -----*/

    if( strcasecmp(argv[nopt],"-2sided") == 0 ){
      do_2sid = 1 ; nopt++ ; continue ;
    }
    if( strcasecmp(argv[nopt],"-1sided") == 0 ){
      do_2sid = 0 ; nopt++ ; continue ;
    }
#endif

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
      have_pthr = 1 ;  /* warn if -pthr is before -both/-niml */
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

#ifdef ALLOW_LOHI
    if( strcasecmp(argv[nopt],"-alo") == 0 ){   /* 20 Jan 2011: debug stuff */
      do_lohi = 1 ; nopt++ ; continue ;
    }
#endif

    /*----   -niml   ----*/

    if( strcasecmp(argv[nopt],"-niml") == 0 ||
        strcasecmp(argv[nopt],"-both") == 0   ){
      /* let users know what to do with -pthr    19 May 2015 [rickr] */
      if( have_pthr )
        ERROR_exit("-both or -niml cannot follow -pthr, as they override it\n"
                   "   i.e. -pthr should be after -both or -niml") ;
      npthr = npthr_lots ;
      pthr = (double *)realloc(pthr,sizeof(double)*npthr) ;
      memcpy( pthr , pthr_lots , sizeof(double)*npthr ) ;
      nathr = nathr_lots ;
      athr = (double *)realloc(athr,sizeof(double)*nathr) ;
      memcpy( athr , athr_lots , sizeof(double)*nathr ) ;
      do_niml = 1 ;
      do_1D   = ( strcasecmp(argv[nopt],"-both") == 0 ) ;
      nopt++ ; continue ;
    }

    /*----   -BALL   ----*/

    if( strcasecmp(argv[nopt],"-BALL") == 0 ){
      do_ball = 1 ; nopt++ ; continue ;
    }

    /*----   -NN   ----*/

    if( strcasecmp(argv[nopt],"-NN") == 0 ){
      WARNING_message("-NN option is no longer supported! All NN cases are computed now.") ;
      nopt++ ; if( isdigit(argv[nopt][0]) ) nopt++ ;
      continue ;
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

    /*-----   -ssave   -----*/

    if( strncasecmp(argv[nopt],"-ssave:",7) == 0 ){
      if( nopt+1 >= argc ) ERROR_exit("need argument after %s",argv[nopt]) ;
      if( strcasecmp(argv[nopt]+7,"blurred") == 0 ){
        do_ssave = SSAVE_BLURRED ;
      } else if( strcasecmp(argv[nopt]+7,"masked") == 0 ){
        do_ssave = SSAVE_MASKED ;
      } else {
        ERROR_exit("this form of '-ssave' is unknown: '%s'",argv[nopt]) ;
      }
      ssave_prefix = strdup(argv[++nopt]) ;
      if( !THD_filename_ok(ssave_prefix) )
        ERROR_exit("bad prefix after option '%s'",argv[nopt-1]) ;
      nopt++ ; continue ;
    }

    /*----- unknown option -----*/

    ERROR_exit("3dClustSim -- unknown option '%s'",argv[nopt]) ;
  }

  /*------- finalize some simple setup stuff --------*/

  if( do_athr_sum && athr_sum_bot < 0 || athr_sum_top < 0 )  /* 18 Dec 2015 */
    do_athr_sum = 0 ;

  if( mask_dset != NULL ){
    nx = DSET_NX(mask_dset) ;
    ny = DSET_NY(mask_dset) ;
    nz = DSET_NZ(mask_dset) ;
    dx = fabsf(DSET_DX(mask_dset)) ;
    dy = fabsf(DSET_DY(mask_dset)) ;
    dz = fabsf(DSET_DZ(mask_dset)) ;
    if( do_ssave > 0 ){                            /* 24 Apr 2014 */
      ssave_dset = EDIT_empty_copy( mask_dset ) ;
      EDIT_dset_items( ssave_dset ,
                         ADN_nvals , 1 ,
                         ADN_prefix , "RandomJunk" ,
                       ADN_none ) ;
    }
  }

  if( do_acf ){  /* 30 Nov 2015 */
    float val ; int_triple ijk ;
    if( fwhm_x > 0.0f )
      WARNING_message("-acf option mean -fwhm options are ignored!") ;
    if( nx < 4 || ny < 4 || nz < 4 )
      ERROR_exit("-acf option: minimum grid is 4x4x4, but you have %dx%dx%d",nx,ny,nz) ;
    fwhm_x = fwhm_y = fwhm_z = 0.0f ;
    acf_parm[0] = acf_a ;
    acf_parm[1] = acf_b ;
    acf_parm[2] = acf_c ;
    val = 2.0f * rfunc_inv(0.5f,acf_parm) ;
    ijk = get_random_field_size( nx,ny,nz , dx,dy,dz , acf_parm ) ;
    acf_nxx = ijk.i ; acf_nyy = ijk.j ; acf_nzz = ijk.k ;
    INFO_message("ACF(%.2f,%.2f,%.2f) => FWHM=%.2f => %dx%dx%d pads to %dx%dx%d",
                 acf_a,acf_b,acf_c,val,
                 nx,ny,nz , acf_nxx,acf_nyy,acf_nzz ) ;
    acf_wim = make_radial_weight( acf_nxx,acf_nyy,acf_nzz , dx,dy,dz , acf_parm ) ;
    acf_ntar = acf_nxx+acf_nyy+acf_nzz ;
  }

  if( do_ssave > 0 && ssave_dset == NULL ){        /* 24 Apr 2014 */
    char gstr[128] ; float xorg,yorg,zorg ;
    xorg = -0.5*dx*(nx-1); yorg = -0.5*dy*(ny-1); zorg = -0.5*dz*(nz-1);
    sprintf(gstr,"RAI:D:%d,%f,%f,%d,%f,%f,%d,%f,%f",
            nx,xorg,dx , ny,yorg,dy , nz,zorg,dz ) ;
    ssave_dset = EDIT_geometry_constructor(gstr,"RandomJunque") ;
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

  zthr_1sid = (float *)malloc(sizeof(float)*npthr) ;
  zthr_2sid = (float *)malloc(sizeof(float)*npthr) ;
  for( ii=0 ; ii < npthr ; ii++ ){
    zthr_1sid[ii] = (float)zthresh(     pthr[ii] ) ;
    zthr_2sid[ii] = (float)zthresh( 0.5*pthr[ii] ) ;
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

  if( do_acf ){                           /* 30 Nov 2015 */
    ex_pad = (acf_nxx-nx)/2 ;
    ey_pad = (acf_nyy-ny)/2 ;
    ez_pad = (acf_nzz-nz)/2 ;
    nx_pad = acf_nxx ;
    ny_pad = acf_nyy ;
    nz_pad = acf_nzz ;
  } else if( do_blur && allow_padding ){  /* 12 May 2015 */
    ex_pad = (int)rintf(1.666f*fwhm_x/dx) ;
    ey_pad = (int)rintf(1.666f*fwhm_y/dy) ;
    ez_pad = (int)rintf(1.666f*fwhm_z/dz) ;
    do_pad = (ex_pad > 0) || (ey_pad > 0) || (ez_pad > 0) ;
    nx_pad = nx + 2*ex_pad ;
    ny_pad = ny + 2*ey_pad ;
    nz_pad = nz + 2*ez_pad ;
    if( do_pad )
      INFO_message(
       "Padding by %d x %d x %d slices to allow for edge effects of blurring" ,
       ex_pad,ey_pad,ez_pad) ;
  } else {
    nx_pad = nx ; ny_pad = ny ; nz_pad = nz ;
  }
  nxy_pad  = nx_pad  * ny_pad ;
  nxyz_pad = nxy_pad * nz_pad ;

  return ;
}

/*---------------------------------------------------------------------------*/

void ssave_dataset( float *fim )  /* 24 Apr 2014 */
{
#pragma omp critical
   { char *spr , sbuf[16] ;
     sprintf(sbuf,"_%06d",ssave_index) ; ssave_index++ ;
     spr = modify_afni_prefix(ssave_prefix,NULL,sbuf) ;
     EDIT_substitute_brick(ssave_dset,0,MRI_float,fim) ;
     EDIT_dset_items(ssave_dset,ADN_prefix,spr,ADN_none) ;
     ssave_dset->idcode = MCW_new_idcode() ;
     DSET_write(ssave_dset) ;
     DSET_NULL_ARRAY(ssave_dset,0) ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/
/* Create the "functional" image, the ACF way [30 Nov 2015]. */

void generate_fim_acf( float *fim , unsigned short xran[] )
{
  DECLARE_ithr ;
  int ii,jj,kk,pp,qq ;
  float *afim ; int do_a ;

  /* the function generates images in pairs, but 3dClustSim only
     consumes them one at a time -- thus the artifice of the 'aim'
     and 'bim' images, which are created together, then aim is
     consumed right away and bim is consumed on the next call here. */

  if( acf_aim[ithr] == NULL && acf_bim[ithr] == NULL ){ /* need new aim & bim */
    MRI_IMARR *imar ;
    imar = make_radial_random_field( acf_nxx , acf_nyy , acf_nzz ,
                                     acf_wim , acf_tar[ithr] , xran ) ;
    acf_aim[ithr] = IMARR_SUBIM(imar,0) ;
    acf_bim[ithr] = IMARR_SUBIM(imar,1) ;
    FREE_IMARR(imar) ;
  }

  do_a = (acf_aim[ithr] != NULL) ;
  afim = (do_a) ? MRI_FLOAT_PTR(acf_aim[ithr]) : MRI_FLOAT_PTR(acf_bim[ithr]) ;

  /* cut back to smaller unpadded volume */

  for( pp=kk=0 ; kk < nz ; kk++ ){
   for( jj=0 ; jj < ny ; jj++ ){
    qq = ex_pad + (jj+ey_pad)*nx_pad + (kk+ez_pad)*nxy_pad ;
    for( ii=0 ; ii < nx ; ii++ ) fim[pp++] = afim[qq++] ;
  }}

  /* free and clear whichever image we consumed here */

  if( do_a ){
    mri_free(acf_aim[ithr]) ; acf_aim[ithr] = NULL ;
  } else {
    mri_free(acf_bim[ithr]) ; acf_bim[ithr] = NULL ;
  }

  return ;
}

/*---------------------------------------------------------------------------*/
/* Create the "functional" image, with smoothing and padding [12 May 2015] */

void generate_fim_padded( float *fim , float *pfim , unsigned short xran[] )
{
  int ii,jj,kk,pp,qq ;

  /* random N(0,1) stuff to create the larger (padded) volume */

  for( ii=0 ; ii < nxyz_pad ; ii++ ) pfim[ii] = zgaussian_sss(xran) ;

  /* smoothization */

  if( do_blur )
    FIR_blur_volume_3d(nx_pad,ny_pad,nz_pad,dx,dy,dz,pfim,sigmax,sigmay,sigmaz) ;

  /* cut back to smaller unpadded volume */

  for( pp=kk=0 ; kk < nz ; kk++ ){
   for( jj=0 ; jj < ny ; jj++ ){
    qq = ex_pad + (jj+ey_pad)*nx_pad + (kk+ez_pad)*nxy_pad ;
    for( ii=0 ; ii < nx ; ii++ ) fim[pp++] = pfim[qq++] ;
  }}

  return ;
}

/*---------------------------------------------------------------------------*/
/* Create the functional "image", with no padding and optional smoothing. */

void generate_fim_unpadded( float *fim , unsigned short xran[] )
{
  int ii ; float sum ;

  /* random N(0,1) stuff */

  for( ii=0 ; ii < nxyz ; ii++ ) fim[ii] = zgaussian_sss(xran) ;

  /* smoothization */

  if( do_blur )
    FIR_blur_volume_3d(nx,ny,nz,dx,dy,dz,fim,sigmax,sigmay,sigmaz) ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Generate random smoothed masked image, with stdev=1. */

void generate_image( float *fim , float *pfim , unsigned short xran[] )
{
  register int ii ; register float sum ;

  /* Outsource the creation of the smoothed random field [12 May 2015] */

  if( do_acf )
    generate_fim_acf( fim , xran ) ;
  else if( do_pad )
    generate_fim_padded( fim , pfim , xran ) ;
  else
    generate_fim_unpadded( fim , xran ) ;

  /* normalizing */

  for( sum=0.0f,ii=0 ; ii < nxyz ; ii++ ) sum += fim[ii]*fim[ii] ;
  sum = sqrtf( nxyz / sum ) ;  /* fix stdev back to 1 */
  for( ii=0 ; ii < nxyz ; ii++ ) fim[ii] *= sum ;

  /* save this volume? */

  if( do_ssave == SSAVE_BLURRED ) ssave_dataset(fim) ;

  /* maskizing? */

  if( mask_vol != NULL ){
    for( ii=0 ; ii < nxyz ; ii++ ) if( !mask_vol[ii] ) fim[ii] = 0.0f ;
  }

  if( tdof > 0.0f ){  /* 26 May 2015: secret stuff */
    float zfac = 1.0f/(1.0f-0.25f/tdof) ;
    float tfac = 0.5f/tdof ;
    float zhat , denom ;
    for( ii=0 ; ii < nxyz ; ii++ ){
      if( fim[ii] != 0.0f ){
        zhat = fim[ii]*zfac ;
        denom = 1.0f - zhat*zhat*tfac ; if( denom < 0.1f ) denom = 0.1f ;
        fim[ii] = zhat / sqrtf(denom) ;
      }
    }
  }

  /* save THIS volume? */

  if( do_ssave == SSAVE_MASKED ) ssave_dataset(fim) ;

#if 0
  /* absolution? */

  if( do_2sid ){
    for( ii=0 ; ii < nxyz ; ii++ ) fim[ii] = fabsf(fim[ii]) ;
  }
#endif

  return ;
}

/*---------------------------------------------------------------------------*/
/* 'shave' == 'short save' == saving generated images
   for re-use in the sumup phase of the program.
   Only the "in-mask" part is saved, and values are scale to shorts.
   Memory is malloc-ed for small needs, and is mmap-ed for large needs.
*//*-------------------------------------------------------------------------*/

#include <sys/mman.h>
#if !defined(MAP_ANON) && defined(MAP_ANONYMOUS)
# define MAP_ANON MAP_ANONYMOUS
#endif

void setup_shave(void)
{
   int64_t twogig = 2ll * 1024ll * 1024ll * 1024ll ;

ENTRY("setup_shave") ;

   shave_siz = (size_t)mask_ngood ;  /* in units of sizeof(short) */
   shave_tot = shave_siz * (int64_t)niter * sizeof(short) ;

   /* check is system is 32-bit and memory needed is over 2G */

   if( shave_tot >= twogig &&
       ( sizeof(void *) < 8 || sizeof(size_t) < 8 ) )
    ERROR_exit("Total space needed for internal save of simulations\n"
               "     exceeds 2 GB -- cannot proceed on a 32-bit system!") ;

#ifndef MAP_ANON
   do_shave = SHAVE_MALLOC
#else
   do_shave = (shave_tot >= twogig) ? SHAVE_MMAP : SHAVE_MALLOC ;
#endif

   if( do_shave == SHAVE_MALLOC ){
     shave = (short *)malloc((size_t)shave_tot) ;
   } else {
#ifdef MAP_ANON
     shave = mmap( (void *)0 , (size_t)shave_tot ,
                   PROT_READ | PROT_WRITE , MAP_ANON | MAP_SHARED , -1,0 ) ;
#endif
   }

   if( shave == NULL )
     ERROR_exit("Cannot allocate space for internal save of simulations :-(") ;

   if( verb )
     INFO_message("allocated %s (%s) bytes for sumup re-use",
                  commaized_integer_string((long long)shave_tot) ,
                  approximate_number_string((double)shave_tot)    ) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Trash the allocated shave space */

void destroy_shave(void)
{
ENTRY("destroy_shave") ;

   if( shave == NULL ) EXRETURN ;

   if( do_shave == SHAVE_MALLOC ) free(shave) ;
   else                           munmap(shave,(size_t)shave_tot) ;
   shave = NULL ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Actually save the iter-th image into the allocated space */

#define SHAVE_FAC 4681.0f        /* 32767/7 */
#define SHAVE_INV 0.0002136296f  /* 7/32767 */

void fim_to_shave( float *fim , int iter )
{
   short *shar = shave + (size_t)(iter * shave_siz) ;
   int ii , jj ;

#if 0
#pragma omp critical
{ fprintf(stderr,"f2s(%d) ",iter); }
#endif

   if( mask_vol != NULL ){
     for( jj=ii=0 ; ii < nxyz ; ii++ )
       if( mask_vol[ii] ) shar[jj++] = (short)(fim[ii]*SHAVE_FAC) ;
   } else {
     for( ii=0 ; ii < nxyz ; ii++ ) shar[ii] = (short)(fim[ii]*SHAVE_FAC) ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/
/* Retrieve the iter-th image from the allocated space */

void shave_to_fim( float *fim , int iter )
{
   short *shar = shave + (size_t)(iter * shave_siz) ;
   int ii , jj ;

#if 0
#pragma omp critical
{ fprintf(stderr,"s2f(%d) ",iter); }
#endif

   if( mask_vol != NULL ){
     for( jj=ii=0 ; ii < nxyz ; ii++ ){
       if( mask_vol[ii] ) fim[ii] = shar[jj++]*SHAVE_INV ;
       else               fim[ii] = 0.0f ;
     }
   } else {
     for( ii=0 ; ii < nxyz ; ii++ ) fim[ii] = shar[ii]*SHAVE_INV ;
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

void gather_stats_NN1_1sid( int ipthr , float *fim , byte *bfim , int *mtab , int ithr )
{
  register int ii ; register float thr ; int siz ;

  thr = zthr_1sid[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
  siz = find_largest_cluster_NN1( bfim , ithr ) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN2_1sid( int ipthr , float *fim , byte *bfim , int *mtab , int ithr )
{
  register int ii ; register float thr ; int siz ;

  thr = zthr_1sid[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
  siz = find_largest_cluster_NN2( bfim , ithr ) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN3_1sid( int ipthr , float *fim , byte *bfim , int *mtab , int ithr )
{
  register int ii ; register float thr ; int siz ;

  thr = zthr_1sid[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
  siz = find_largest_cluster_NN3( bfim , ithr ) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN1_2sid( int ipthr , float *fim , byte *bfim , int *mtab , int ithr )
{
  register int ii ; register float thr ; int siz ;

  thr = zthr_2sid[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) || (fim[ii] < -thr) ;
  siz = find_largest_cluster_NN1( bfim , ithr ) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN2_2sid( int ipthr , float *fim , byte *bfim , int *mtab , int ithr )
{
  register int ii ; register float thr ; int siz ;

  thr = zthr_2sid[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) || (fim[ii] < -thr) ;
  siz = find_largest_cluster_NN2( bfim , ithr ) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN3_2sid( int ipthr , float *fim , byte *bfim , int *mtab , int ithr )
{
  register int ii ; register float thr ; int siz ;

  thr = zthr_2sid[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) || (fim[ii] < -thr) ;
  siz = find_largest_cluster_NN3( bfim , ithr ) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN1_bsid( int ipthr , float *fim , byte *bfim , int *mtab , int ithr )
{
  register int ii ; register float thr ; int siz_p , siz_m , siz ;

  thr = zthr_2sid[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
  siz_p = find_largest_cluster_NN1( bfim , ithr ) ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] < -thr) ;
  siz_m = find_largest_cluster_NN1( bfim , ithr ) ;
  siz = MAX(siz_p,siz_m) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN2_bsid( int ipthr , float *fim , byte *bfim , int *mtab , int ithr )
{
  register int ii ; register float thr ; int siz_p , siz_m , siz ;

  thr = zthr_2sid[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
  siz_p = find_largest_cluster_NN2( bfim , ithr ) ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] < -thr) ;
  siz_m = find_largest_cluster_NN2( bfim , ithr ) ;
  siz = MAX(siz_p,siz_m) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats_NN3_bsid( int ipthr , float *fim , byte *bfim , int *mtab , int ithr )
{
  register int ii ; register float thr ; int siz_p , siz_m , siz ;

  thr = zthr_2sid[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
  siz_p = find_largest_cluster_NN3( bfim , ithr ) ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] < -thr) ;
  siz_m = find_largest_cluster_NN3( bfim , ithr ) ;
  siz = MAX(siz_p,siz_m) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*---------------------------------------------------------------------------*/

static char * prob6(float p)   /* format p-value into 6 char */
{
   static char str[16] ;
   if( p >= 0.00010f ){
     sprintf(str,"%7.5f",p) ;
   } else {
     int   dec = (int)(0.9999-log10(p)) ;
     float man = p * pow( 10.0 , (double)dec ) ;
     sprintf(str,"%4.1fe-%1d",man,dec) ;
   }
   return (str+1) ;
}

/*---------------------------------------------------------------------------*/

static char * prob9(float p)   /* format p-value into 9 char */
{
   static char str[16] ;
   if( p >= 0.00010f ){
     sprintf(str,"%9.6f",p) ;
   } else {
     int   dec = (int)(0.9999-log10(p)) ;
     float man = p * pow( 10.0 , (double)dec ) ;
     sprintf(str,"%6.3fe-%1d",man,dec) ;
   }
   return str ;
}

/*---------------------------------------------------------------------------*/

static int *fa_1sid_NN1, *fa_1sid_NN2, *fa_1sid_NN3 ;
static int *fa_2sid_NN1, *fa_2sid_NN2, *fa_2sid_NN3 ;
static int *fa_bsid_NN1, *fa_bsid_NN2, *fa_bsid_NN3 ;

void thresh_summer_fim( int iathr,
                        int ipthr_bot, int ipthr_top,
                        float *fim, byte *bfim , int ithr )
{
   register int ii ; register float thr ;
   int ipthr , siz ;

   fa_1sid_NN1[ithr]=0; fa_1sid_NN2[ithr]=0; fa_1sid_NN3[ithr]=0;
   fa_2sid_NN1[ithr]=0; fa_2sid_NN2[ithr]=0; fa_2sid_NN3[ithr]=0;
   fa_bsid_NN1[ithr]=0; fa_bsid_NN2[ithr]=0; fa_bsid_NN3[ithr]=0;

   for( ipthr=ipthr_bot ; ipthr <= ipthr_top ; ipthr++ ){

     if( !fa_1sid_NN1[ithr] ){ /* 1-sided, NN1 */
       thr = zthr_1sid[ipthr] ;
       for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
       siz = find_largest_cluster_NN1(bfim,0) ;
       fa_1sid_NN1[ithr] = (siz >= clust_thresh_1sid_NN1[ipthr][iathr] ) ;
     }

     if( !fa_1sid_NN2[ithr] ){ /* 1-sided, NN2 */
       thr = zthr_1sid[ipthr] ;
       for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
       siz = find_largest_cluster_NN2(bfim,0) ;
       fa_1sid_NN2[ithr] = (siz >= clust_thresh_1sid_NN2[ipthr][iathr] ) ;
     }

     if( !fa_1sid_NN3[ithr] ){ /* 1-sided, NN3 */
       thr = zthr_1sid[ipthr] ;
       for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
       siz = find_largest_cluster_NN3(bfim,0) ;
       fa_1sid_NN3[ithr] = (siz >= clust_thresh_1sid_NN3[ipthr][iathr] ) ;
     }

     if( do_athr_sum < 2 ) continue ; /* skip 2-sided and bi-sided */

     if( !fa_2sid_NN1[ithr] ){ /* 2-sided, NN1 */
       thr = zthr_2sid[ipthr] ;
       for( ii=0 ; ii < nxyz ; ii++ )
         bfim[ii] = (fim[ii] > thr) || (fim[ii] < -thr) ;
       siz = find_largest_cluster_NN1(bfim,0) ;
       fa_2sid_NN1[ithr] = (siz >= clust_thresh_2sid_NN1[ipthr][iathr] ) ;
     }

     if( !fa_2sid_NN2[ithr] ){ /* 2-sided, NN2 */
       thr = zthr_2sid[ipthr] ;
       for( ii=0 ; ii < nxyz ; ii++ )
         bfim[ii] = (fim[ii] > thr) || (fim[ii] < -thr) ;
       siz = find_largest_cluster_NN2(bfim,0) ;
       fa_2sid_NN2[ithr] = (siz >= clust_thresh_2sid_NN2[ipthr][iathr] ) ;
     }

     if( !fa_2sid_NN3[ithr] ){ /* 2-sided, NN3 */
       thr = zthr_2sid[ipthr] ;
       for( ii=0 ; ii < nxyz ; ii++ )
         bfim[ii] = (fim[ii] > thr) || (fim[ii] < -thr) ;
       siz = find_largest_cluster_NN3(bfim,0) ;
       fa_2sid_NN3[ithr] = (siz >= clust_thresh_2sid_NN3[ipthr][iathr] ) ;
     }

     if( !fa_bsid_NN1[ithr] ){  /* bi-sided, NN1 */
       thr = zthr_2sid[ipthr] ;
       for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
       siz = find_largest_cluster_NN1( bfim , 0 ) ;
       fa_bsid_NN1[ithr] = (siz >= clust_thresh_bsid_NN1[ipthr][iathr] ) ;
       if( !fa_bsid_NN1[ithr] ){
         for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] < -thr) ;
         siz = find_largest_cluster_NN1( bfim , 0 ) ;
         fa_bsid_NN1[ithr] = (siz >= clust_thresh_bsid_NN1[ipthr][iathr] ) ;
       }
     }

     if( !fa_bsid_NN2[ithr] ){  /* bi-sided, NN2 */
       thr = zthr_2sid[ipthr] ;
       for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
       siz = find_largest_cluster_NN2( bfim , 0 ) ;
       fa_bsid_NN2[ithr] = (siz >= clust_thresh_bsid_NN2[ipthr][iathr] ) ;
       if( !fa_bsid_NN2[ithr] ){
         for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] < -thr) ;
         siz = find_largest_cluster_NN2( bfim , 0 ) ;
         fa_bsid_NN2[ithr] = (siz >= clust_thresh_bsid_NN2[ipthr][iathr] ) ;
       }
     }

     if( !fa_bsid_NN3[ithr] ){  /* bi-sided, NN3 */
       thr = zthr_2sid[ipthr] ;
       for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
       siz = find_largest_cluster_NN3( bfim , 0 ) ;
       fa_bsid_NN3[ithr] = (siz >= clust_thresh_bsid_NN3[ipthr][iathr] ) ;
       if( !fa_bsid_NN3[ithr] ){
         for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] < -thr) ;
         siz = find_largest_cluster_NN3( bfim , 0 ) ;
         fa_bsid_NN3[ithr] = (siz >= clust_thresh_bsid_NN3[ipthr][iathr] ) ;
       }
     }

   }

   return ;
}

/*---------------------------------------------------------------------------*/

static float *rfa_1sid_NN1, *rfa_1sid_NN2, *rfa_1sid_NN3 ;
static float *rfa_2sid_NN1, *rfa_2sid_NN2, *rfa_2sid_NN3 ;
static float *rfa_bsid_NN1, *rfa_bsid_NN2, *rfa_bsid_NN3 ;

void thresh_summer_athr( int iathr_bot, int iathr_top, int ipthr_bot, int ipthr_top )
{
   float const drfa = 1.0f/niter ;

ENTRY("thresh_summer_athr") ;

   rfa_1sid_NN1 = (float *)calloc(sizeof(float),(iathr_top-iathr_bot+1)) ;
   rfa_2sid_NN1 = (float *)calloc(sizeof(float),(iathr_top-iathr_bot+1)) ;
   rfa_bsid_NN1 = (float *)calloc(sizeof(float),(iathr_top-iathr_bot+1)) ;
   rfa_1sid_NN2 = (float *)calloc(sizeof(float),(iathr_top-iathr_bot+1)) ;
   rfa_2sid_NN2 = (float *)calloc(sizeof(float),(iathr_top-iathr_bot+1)) ;
   rfa_bsid_NN2 = (float *)calloc(sizeof(float),(iathr_top-iathr_bot+1)) ;
   rfa_1sid_NN3 = (float *)calloc(sizeof(float),(iathr_top-iathr_bot+1)) ;
   rfa_2sid_NN3 = (float *)calloc(sizeof(float),(iathr_top-iathr_bot+1)) ;
   rfa_bsid_NN3 = (float *)calloc(sizeof(float),(iathr_top-iathr_bot+1)) ;


 AFNI_OMP_START;
#pragma omp parallel
 { DECLARE_ithr ;
   int iter , iathr , vstep , vii ;
   float *fim ; byte *bfim ;

#pragma omp master
 {
#ifdef USE_OMP
   nthr = omp_get_num_threads() ;
#else
   nthr = 1 ;
#endif
   fa_1sid_NN1 = (int *)malloc(sizeof(int)*nthr) ;
   fa_2sid_NN1 = (int *)malloc(sizeof(int)*nthr) ;
   fa_bsid_NN1 = (int *)malloc(sizeof(int)*nthr) ;
   fa_1sid_NN2 = (int *)malloc(sizeof(int)*nthr) ;
   fa_2sid_NN2 = (int *)malloc(sizeof(int)*nthr) ;
   fa_bsid_NN2 = (int *)malloc(sizeof(int)*nthr) ;
   fa_1sid_NN3 = (int *)malloc(sizeof(int)*nthr) ;
   fa_2sid_NN3 = (int *)malloc(sizeof(int)*nthr) ;
   fa_bsid_NN3 = (int *)malloc(sizeof(int)*nthr) ;
   if( verb ){
     vstep = (int)( niter / (nthr*50.0f) + 0.901f) ;
     vii   = 0 ; vstep_reset() ;
     fprintf(stderr,"Summing up:") ;
   }
 }
#pragma omp barrier

   fim  = (float *)malloc(sizeof(float)*nxyz) ;
   bfim = (byte  *)malloc(sizeof(byte) *nxyz) ;

#pragma omp for
   for( iter=1 ; iter <= niter ; iter++ ){

     if( ithr==0 && verb ){
       vii++ ; if( vii%vstep == vstep/2 ) vstep_print() ;
     }

     shave_to_fim(fim,iter-1) ;

     for( iathr=iathr_bot ; iathr <= iathr_top ; iathr++ ){

       thresh_summer_fim( iathr,ipthr_bot,ipthr_top,fim,bfim,ithr ) ;

#pragma omp critical
      {if( fa_1sid_NN1[ithr] ) rfa_1sid_NN1[iathr-iathr_bot] += drfa ;
       if( fa_1sid_NN2[ithr] ) rfa_1sid_NN2[iathr-iathr_bot] += drfa ;
       if( fa_1sid_NN3[ithr] ) rfa_1sid_NN3[iathr-iathr_bot] += drfa ;
       if( do_athr_sum > 1 ){
         if( fa_2sid_NN1[ithr] ) rfa_2sid_NN1[iathr-iathr_bot] += drfa ;
         if( fa_2sid_NN2[ithr] ) rfa_2sid_NN2[iathr-iathr_bot] += drfa ;
         if( fa_2sid_NN3[ithr] ) rfa_2sid_NN3[iathr-iathr_bot] += drfa ;
         if( fa_bsid_NN1[ithr] ) rfa_bsid_NN1[iathr-iathr_bot] += drfa ;
         if( fa_bsid_NN2[ithr] ) rfa_bsid_NN2[iathr-iathr_bot] += drfa ;
         if( fa_bsid_NN3[ithr] ) rfa_bsid_NN3[iathr-iathr_bot] += drfa ;
       }
      }
     }
   }

   free(bfim) ; free(fim) ;

 }
AFNI_OMP_END ;
   if( verb ) fprintf(stderr,"\n") ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

double get_one_clust_thresh( int **mtnn , int ipthr , double aval )
{
   static double *alpha=NULL ;
   double ahi,alo,jj ;
   float cmax=0.0f , **clust_thresh ;
   int ii , itop , iathr , mmm ;

   if( alpha == NULL )
     alpha = (double *)malloc(sizeof(double)*(max_cluster_size+1)) ;

   /* alpha[ii] = prob of getting clusters of exactly size ii */

   for( itop=ii=1 ; ii <= max_cluster_size ; ii++ ){
     alpha[ii] = mtnn[ipthr][ii] / (double)niter ;
     if( alpha[ii] > 0.0 ) itop = ii ;
   }

   /* alpha[ii] = prob of getting clusters of size ii or more */

   for( ii=itop-1 ; ii >= 1 ; ii-- ) alpha[ii] += alpha[ii+1] ;

   /* find ii that brackets the desired aval */

   if( aval > alpha[1] ){  /* unpleasant situation */
     static int first=2 ;  /* where there is no bracket */
     ii = 1 ;
     if( first ){
       WARNING_message(
              "pthr=%.6f ; desired alpha=%.6f -- but max simulated alpha=%.6f]\n" ,
              pthr[ipthr] , aval , alpha[1] ) ;
       first-- ;
       if( first == 0 )
         ININFO_message("       [further messages about alpha are suppressed]") ;
     }
   } else {
     for( ii=1 ; ii < itop ; ii++ ){
       if( alpha[ii] >= aval && alpha[ii+1] <= aval ) break ;
     }
   }

   /* inverse interpolate to find the index jj where alpha[jj]=aval
      -- except, of course, jj is a floating point value between ii and ii+1 */

   alo=alpha[ii] ; ahi=alpha[ii+1] ;

   if( alo >= 1.0 ) alo = 1.0 - 0.1/niter ;           /* unlikely */
   if( ahi <= 0.0 ) ahi = 0.1/niter ;   /* should not be possible */
   if( ahi >= alo ) ahi = 0.1*alo ;     /* should not be possible */
   jj   = log(-log(1.0-aval)) ;
   alo  = log(-log(1.0-alo)) ;
   ahi  = log(-log(1.0-ahi)) ;
   jj   = ii + (jj-alo)/(ahi-alo) ;
   if( jj < 1.0 ) jj = 1.0 ;

   return jj ;
}

/*===========================================================================*/

int main( int argc , char **argv )
{
  int nnn , ipthr , first_mask=1 ;
  char *refit_cmd = NULL ;
  double ct ;
#ifdef USE_OMP
  int ***mtab_1sid[4] , ***mtab_2sid[4] , ***mtab_bsid[4] ;
#endif

  /*----- does user request help menu? -----*/

  AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */
  (void)COX_clock_time() ;

  if( argc < 2 || strcmp(argv[1],"-help") == 0 ) display_help_menu() ;

  /*----- get the list of things to do -----*/

  mainENTRY("3dClustSim"); machdep();
  AFNI_logger("3dClustSim",argc,argv);
  PRINT_VERSION("3dClustSim"); AUTHOR("RW Cox and BD Ward");
  THD_check_AFNI_version("3dClustSim") ;

  get_options( argc , argv ) ;

  /*----- create some space for the results -----*/

  for( nnn=1 ; nnn <= 3 ; nnn++ ){
    max_table_1sid[nnn] = (int **)malloc(sizeof(int *)*npthr) ;  /* array of tables */
    max_table_2sid[nnn] = (int **)malloc(sizeof(int *)*npthr) ;  /* array of tables */
    max_table_bsid[nnn] = (int **)malloc(sizeof(int *)*npthr) ;  /* array of tables */
    for( ipthr=0 ; ipthr < npthr ; ipthr++ ){                    /* create tables */
      max_table_1sid[nnn][ipthr] = (int *)calloc(sizeof(int),(max_cluster_size+1)) ;
      max_table_2sid[nnn][ipthr] = (int *)calloc(sizeof(int),(max_cluster_size+1)) ;
      max_table_bsid[nnn][ipthr] = (int *)calloc(sizeof(int),(max_cluster_size+1)) ;
    }
  }

  if( do_athr_sum ) setup_shave() ;

  if( verb )
    INFO_message("Startup clock time = %.1f s",COX_clock_time()) ;

 AFNI_OMP_START ;
#pragma omp parallel
 {
   DECLARE_ithr ;
   int iter, ipthr, **mt_1sid[4],**mt_2sid[4],**mt_bsid[4] , nnn ;
   float *fim ; byte *bfim ; unsigned short xran[3] ;
   float *pfim ;
   int vstep , vii ;

  /* create separate tables for each thread, if using OpenMP */
#ifdef USE_OMP
#pragma omp master  /* only in the master thread */
 {
   nthr = omp_get_num_threads() ;
   for( nnn=1 ; nnn <= 3 ; nnn++ ){
     mtab_1sid[nnn] = (int ***)malloc(sizeof(int **) *nthr) ;
     mtab_2sid[nnn] = (int ***)malloc(sizeof(int **) *nthr) ;
     mtab_bsid[nnn] = (int ***)malloc(sizeof(int **) *nthr) ;
   }

   nall_g = (int *)   malloc(sizeof(int)    *nthr) ;  /* workspaces for */
   inow_g = (short **)malloc(sizeof(short *)*nthr) ;  /* find_largest_cluster() */
   jnow_g = (short **)malloc(sizeof(short *)*nthr) ;
   know_g = (short **)malloc(sizeof(short *)*nthr) ;
   if( do_acf ){
     acf_aim  = (MRI_IMAGE **)calloc(sizeof(MRI_IMAGE *),nthr) ;
     acf_bim  = (MRI_IMAGE **)calloc(sizeof(MRI_IMAGE *),nthr) ;
     acf_tar  = (complex **)  malloc(sizeof(complex *)  *nthr) ;
   }
   if( verb ) INFO_message("Using %d OpenMP threads",nthr) ;
 }
#pragma omp barrier  /* all threads wait until the above is finished */
   /* create tables for each thread separately */
   for( nnn=1 ; nnn <= 3 ; nnn++ ){
     mtab_1sid[nnn][ithr] = mt_1sid[nnn] = (int **)malloc(sizeof(int *)*npthr) ;
     mtab_2sid[nnn][ithr] = mt_2sid[nnn] = (int **)malloc(sizeof(int *)*npthr) ;
     mtab_bsid[nnn][ithr] = mt_bsid[nnn] = (int **)malloc(sizeof(int *)*npthr) ;
     for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
       mt_1sid[nnn][ipthr] = (int *)calloc(sizeof(int),(max_cluster_size+1)) ;
       mt_2sid[nnn][ipthr] = (int *)calloc(sizeof(int),(max_cluster_size+1)) ;
       mt_bsid[nnn][ipthr] = (int *)calloc(sizeof(int),(max_cluster_size+1)) ;
     }
   }

   /* create workspace for find_largest_cluster(), for this thread */

   nall_g[ithr] = DALL ;
   inow_g[ithr] = (short *) malloc(sizeof(short)*DALL) ;
   jnow_g[ithr] = (short *) malloc(sizeof(short)*DALL) ;
   know_g[ithr] = (short *) malloc(sizeof(short)*DALL) ;

   if( do_acf ){
     acf_tar[ithr] = (complex *)malloc(sizeof(complex)*acf_ntar) ;
   }

   /* initialize random seed array for each thread separately */
   xran[2] = ( gseed        & 0xffff) + (unsigned short)ithr ;
   xran[1] = ((gseed >> 16) & 0xffff) - (unsigned short)ithr ;
   xran[0] = 0x330e                   + (unsigned short)ithr ;

#else /* not OpenMP ==> only one set of tables */
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
   for( nnn=1 ; nnn <= 3 ; nnn++ ){
     mt_1sid[nnn] = max_table_1sid[nnn] ;
     mt_2sid[nnn] = max_table_2sid[nnn] ;
     mt_bsid[nnn] = max_table_bsid[nnn] ;
   }
   if( do_acf ){
     acf_aim  = (MRI_IMAGE **)calloc(sizeof(MRI_IMAGE *),nthr) ;
     acf_bim  = (MRI_IMAGE **)calloc(sizeof(MRI_IMAGE *),nthr) ;
     acf_tar  = (complex **)  calloc(sizeof(complex *)  ,nthr) ;
     acf_tar[ithr] = (complex *)malloc(sizeof(complex)*acf_ntar) ;
   }
#endif

   fim  = (float *)malloc(sizeof(float)*nxyz) ;  /* image space */
   bfim = (byte * )malloc(sizeof(byte) *nxyz) ;
   if( do_pad ) pfim = (float *)malloc(sizeof(float)*nxyz_pad); /* 12 May 2015 */
   else         pfim = NULL ;

   vstep = (int)( niter / (nthr*50.0f) + 0.901f) ;
   vii   = 0 ;
   if( ithr == 0 && verb ) fprintf(stderr,"Simulating:") ;

  /*----- Monte Carlo iterations -----*/

#pragma omp for
  for( iter=1 ; iter <= niter ; iter++ ){

    if( verb && ithr == 0 ){
      vii++ ; if( vii%vstep == vstep/2 ) vstep_print() ;
    }

    generate_image( fim , pfim , xran ) ;

    if( do_shave ) fim_to_shave(fim,iter-1) ;

    for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
      gather_stats_NN1_1sid( ipthr , fim , bfim , mt_1sid[1][ipthr] , ithr ) ;
      gather_stats_NN2_1sid( ipthr , fim , bfim , mt_1sid[2][ipthr] , ithr ) ;
      gather_stats_NN3_1sid( ipthr , fim , bfim , mt_1sid[3][ipthr] , ithr ) ;

      gather_stats_NN1_2sid( ipthr , fim , bfim , mt_2sid[1][ipthr] , ithr ) ;
      gather_stats_NN2_2sid( ipthr , fim , bfim , mt_2sid[2][ipthr] , ithr ) ;
      gather_stats_NN3_2sid( ipthr , fim , bfim , mt_2sid[3][ipthr] , ithr ) ;

      gather_stats_NN1_bsid( ipthr , fim , bfim , mt_bsid[1][ipthr] , ithr ) ;
      gather_stats_NN2_bsid( ipthr , fim , bfim , mt_bsid[2][ipthr] , ithr ) ;
      gather_stats_NN3_bsid( ipthr , fim , bfim , mt_bsid[3][ipthr] , ithr ) ;
    }

  } /* end of simulation loop */

  free(fim) ; free(bfim) ; if( pfim != NULL ) free(pfim) ;

  if( !do_athr_sum ){
    free(inow_g[ithr]) ; free(jnow_g[ithr]) ; free(know_g[ithr]) ;
    if( do_acf ){
      free(acf_tar[ithr]) ;
      if( acf_bim[ithr] != NULL ) mri_free(acf_bim[ithr]) ;
    }
  }

  if( ithr == 0 && verb ) fprintf(stderr,"!\n") ;

 } /* end OpenMP parallelization */
 AFNI_OMP_END ;

   if( verb )
     INFO_message("Clock time now = %.1f s",COX_clock_time()) ;

   if( do_acf && !do_athr_sum ){
     free(acf_aim) ; free(acf_bim) ; free(acf_tar) ;
   }

   if( do_ssave > 0 ) DSET_delete(ssave_dset) ; /* 24 Apr 2014 */

   /*-------- sum tables from various threads into one result ----------*/

#ifdef USE_OMP
   { int ithr , ii , ipthr , **mt_1sid,**mt_2sid,**mt_bsid , *mth_1sid,*mth_2sid,*mth_bsid ;
     for( nnn=1 ; nnn <= 3 ; nnn++ ){
       for( ithr=0 ; ithr < nthr ; ithr++ ){
         mt_1sid = mtab_1sid[nnn][ithr] ;
         mt_2sid = mtab_2sid[nnn][ithr] ;
         mt_bsid = mtab_bsid[nnn][ithr] ;
         for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
           mth_1sid = mt_1sid[ipthr] ;
           mth_2sid = mt_2sid[ipthr] ;
           mth_bsid = mt_bsid[ipthr] ;
           for( ii=1 ; ii <= max_cluster_size ; ii++ ){
             max_table_1sid[nnn][ipthr][ii] += mth_1sid[ii] ;
             max_table_2sid[nnn][ipthr][ii] += mth_2sid[ii] ;
             max_table_bsid[nnn][ipthr][ii] += mth_bsid[ii] ;
           }
         }
       }
     }
   }
#endif

#if 0
  enable_mcw_malloc() ;
#endif

  /*---------- compute and print the output tables ----------*/

  { double *alpha , aval , ahi,alo ;
    float cmax=0.0f , **clust_thresh ;
    int ii , itop , iathr , mmm, ***mtt , **mtnn ;
    char *commandline = tross_commandline("3dClustSim",argc,argv) ;
    char fname[THD_MAX_NAME] , pname[THD_MAX_NAME] ;
    char *amesg = NULL , *mlab = NULL , *mlll = NULL ;

    alpha = (double *)malloc(sizeof(double)*(max_cluster_size+1)) ;

    for( mmm=1 ; mmm <= 3 ; mmm++ ){  /* loop over sidedness */

      if( mmm == 1 ){
        mtt = max_table_1sid ; mlab = "1-sided" ; mlll = "1sided" ;
      } else if( mmm == 2 ){
        mtt = max_table_2sid ; mlab = "2-sided" ; mlll = "2sided" ;
      } else {
        mtt = max_table_bsid ; mlab = "bi-sided"; mlll = "bisided";
      }

      amesg = NULL ; cmax = 0.0f ;
      for( nnn=1 ; nnn <= 3 ; nnn++ ){  /* loop over NN level */

        clust_thresh = (float **)malloc(sizeof(float *)*npthr) ;
        for( ipthr=0 ; ipthr < npthr ; ipthr++ )
          clust_thresh[ipthr] = (float *)malloc(sizeof(float)*nathr) ;

        mtnn = mtt[nnn] ;

        for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
#if 0
          for( iathr=0 ; iathr < nathr ; iathr++ ){
            clust_thresh[ipthr][iathr] =
              get_one_clust_thresh( mtnn,ipthr,athr[iathr]) ;
            if( nodec ) clust_thresh[ipthr][iathr] =
                          (int)(clust_thresh[ipthr][iathr]+0.951f) ;
            if( clust_thresh[ipthr][iathr] > cmax ) cmax = clust_thresh[ipthr][iathr] ;
          }
#else
          for( itop=ii=1 ; ii <= max_cluster_size ; ii++ ){
            alpha[ii] = mtt[nnn][ipthr][ii] / (double)niter ;
            if( alpha[ii] > 0.0 ) itop = ii ;
          }
          for( ii=itop-1 ; ii >= 1 ; ii-- ) alpha[ii] += alpha[ii+1] ;
          for( iathr=0 ; iathr < nathr ; iathr++ ){
            aval = athr[iathr] ;
            if( aval > alpha[1] ){  /* unpleasant situation */
              ii = 1 ;
              amesg = THD_zzprintf(
                       amesg ,
                       " %s  NN=%d  pthr=%9.6f  alpha=%6.3f [max simulated alpha=%6.3f]\n" ,
                       mlab , nnn , pthr[ipthr] , aval , alpha[1] ) ;
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
#endif
        } /* end of loop over ipthr */

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
          sprintf(pname,"%s.NN%d_%s.",prefix,nnn,mlll) ;
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
           "# %s thresholding\n"
           "# Grid: %dx%dx%d %.2fx%.2fx%.2f mm^3 (%d voxels%s)\n"
           "#\n"
           "# CLUSTER SIZE THRESHOLD(pthr,alpha) in Voxels\n"
           "# -NN %d  | alpha = Prob(Cluster >= given size)\n"
           "#  pthr  |" ,
           commandline , mlab ,
           nx,ny,nz , dx,dy,dz ,
           mask_ngood , (mask_ngood < nxyz) ? " in mask" : "\0" , nnn ) ;
          for( iathr=0 ; iathr < nathr ; iathr++ ) fprintf(fp," %s",prob6(athr[iathr])) ;
          fprintf(fp,"\n"
           "# ------ |" ) ;
          for( iathr=0 ; iathr < nathr ; iathr++ ) fprintf(fp," ------") ;
          fprintf(fp,"\n") ;
          for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
            fprintf(fp,"%s ",prob9(pthr[ipthr])) ;
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
          if( fp != stdout ) fclose(fp) ;
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
            NI_set_attribute( nel , "commandline"  , commandline ) ;
            NI_set_attribute( nel , "thresholding" , mlab        ) ;
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
                                      "-atrstring AFNI_CLUSTSIM_NN%d_%s file:%s " ,
                                      nnn , mlll , fname ) ;
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

#if 0
        switch( nnn*10 + mmm ){
          case 11: clust_thresh_1sid_NN1 = clust_thresh ; break ;
          case 12: clust_thresh_2sid_NN1 = clust_thresh ; break ;
          case 13: clust_thresh_bsid_NN1 = clust_thresh ; break ;
          case 21: clust_thresh_1sid_NN2 = clust_thresh ; break ;
          case 22: clust_thresh_2sid_NN2 = clust_thresh ; break ;
          case 23: clust_thresh_bsid_NN2 = clust_thresh ; break ;
          case 31: clust_thresh_1sid_NN3 = clust_thresh ; break ;
          case 32: clust_thresh_2sid_NN3 = clust_thresh ; break ;
          case 33: clust_thresh_bsid_NN3 = clust_thresh ; break ;
          default:  /* should never transpire */
            ERROR_exit("nnn=%d mmm=%d :: This should never happen!",nnn,mmm) ;
          break ;
        }
#else
        if( nnn*10+mmm == 11 ) clust_thresh_1sid_NN1 = clust_thresh ;
        if( nnn*10+mmm == 12 ) clust_thresh_2sid_NN1 = clust_thresh ;
        if( nnn*10+mmm == 13 ) clust_thresh_bsid_NN1 = clust_thresh ;
        if( nnn*10+mmm == 21 ) clust_thresh_1sid_NN2 = clust_thresh ;
        if( nnn*10+mmm == 22 ) clust_thresh_2sid_NN2 = clust_thresh ;
        if( nnn*10+mmm == 23 ) clust_thresh_bsid_NN2 = clust_thresh ;
        if( nnn*10+mmm == 31 ) clust_thresh_1sid_NN3 = clust_thresh ;
        if( nnn*10+mmm == 32 ) clust_thresh_2sid_NN3 = clust_thresh ;
        if( nnn*10+mmm == 33 ) clust_thresh_bsid_NN3 = clust_thresh ;
#endif

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
        free(amesg) ; amesg = NULL ;
      }

   } /* end of loop over mmm == sidedness */

   if( do_athr_sum ){
     int iathr , iathr_bot=0 , iathr_top=nathr-1 ; FILE *fp ; char fname[256] ;

     thresh_summer_athr(iathr_bot,iathr_top,athr_sum_bot,athr_sum_top) ;

     sprintf(fname,"%s.sumup.1D",(prefix!=NULL)?prefix:"ClustSim") ;
     fp = fopen(fname,"w") ;
     fprintf(fp,"# %s\n",commandline) ;
     fprintf(fp,"# "
      "1dplot -one -box -xaxis 0:0.1:10:5 -yaxis 0:0.32:16:2"
      " -xlabel 'nominal \\alpha  at fixed p'"
      " -ylabel 'integrated FAR over p\\in[%g,%g]' "
      " -ynames 1s:NN1 1s:NN2 1s:NN3 2s:NN1 2s:NN2 2s:NN3 bs:NN1 bs:NN2 bs:NN3"
      " -x %s'[0]' -plabel '\\noesc %s' %s'[1..$]'\n" ,
      pthr[athr_sum_top] , pthr[athr_sum_top] , fname,fname,fname ) ;
     fprintf(fp,"# alpha ") ;
     fprintf(fp," 1s:NN1 1s:NN2 1s:NN3") ;
     if( do_athr_sum > 1 ){
       fprintf(fp," 2s:NN1 2s:NN2 2s:NN3") ;
       fprintf(fp," bs:NN1 bs:NN2 bs:NN3") ;
     }
     fprintf(fp,"\n") ;
     for( iathr=iathr_bot ; iathr <= iathr_top ; iathr++ ){
       fprintf(fp," %.4f ",athr[iathr]) ;
       fprintf(fp," %.4f %.4f %.4f",
                      rfa_1sid_NN1[iathr-iathr_bot],
                      rfa_1sid_NN2[iathr-iathr_bot], rfa_1sid_NN3[iathr-iathr_bot]) ;
       if( do_athr_sum > 1 ){
        fprintf(fp," %.4f %.4f %.4f",
                       rfa_2sid_NN1[iathr-iathr_bot],
                       rfa_2sid_NN2[iathr-iathr_bot], rfa_2sid_NN3[iathr-iathr_bot]) ;
        fprintf(fp," %.4f %.4f %.4f",
                       rfa_bsid_NN1[iathr-iathr_bot],
                       rfa_bsid_NN2[iathr-iathr_bot], rfa_bsid_NN3[iathr-iathr_bot]) ;
       }
       fprintf(fp,"\n") ;
     }
     fclose(fp) ;
   }

   destroy_shave() ;

   if( verb )
     INFO_message("Clock time now = %.1f s",COX_clock_time()) ;

  } /* end of outputizationing */

  /*------- a minor aid for the pitiful helpless user [e.g., me] -------*/

  if( refit_cmd != NULL ){
    FILE *fp ;
    INFO_message("Command fragment to put cluster results into a dataset header;\n"
                 " + (also echoed to file 3dClustSim.cmd for your scripting pleasure)\n"
                 " + Append the name of the datasets to be patched to this command:\n"
                 " %s" , refit_cmd ) ;
    fp = fopen("3dClustSim.cmd","w") ;
    if( fp == NULL ){
      ERROR_message("Can't write 3drefit command fragment to file 3dClustSim.cmd :-(") ;
    } else {
      fprintf(fp,"%s\n",refit_cmd) ; fclose(fp) ;
    }
  }

  /*-------- run away screaming into the night ----- AAUUGGGHHH!!! --------*/

  exit(0);
}
