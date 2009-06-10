/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
  This program estimates the statistical significance levels and statistical
  power of FMRI datasets by Monte Carlo simulation of random image generation,
  Gaussian filtering, intensity thresholding, and minimum cluster size
  thresholding.

  File:    AlphaSim.c
  Author:  B. D. Ward
  Date:    18 June 1997

  Mod:     Changed random number generator function from rand to drand48.
  Date:    26 August 1997

  Mod:     Corrected problem: attempt to close a file which was not open.
  Date:    21 June 1999

  Mod:     Corrected problem with count overflow.
  Date:    30 July 1999

  Mod:     Added -mask option.  (Adapted from: 3dpc.c)
  Date:    15 June 2000

  Mod:     Added call to AFNI_logger.
  Date:    15 August 2001

  Mod:     Set MAX_NAME_LENGTH equal to THD_MAX_NAME.
  Date:    02 December 2002

  Mod:     Added -max_clust_size, to override MAX_CLUSTER_SIZE.
           Also, increased default from 1000 to 10000.
  Date:    12 Apr 2006 [rickr]

  Mod:     Added AFNI_BLUR_* environment variable stuff.
  Date:    03 Apr 2007 [RWC]

  Mod:     Add -fast and -nxyz and -dxyz options.
  Date:    10 Jan 2008 [RWC]

  Mod:     Modify to use OpenMP to parallelize simulations.
  Date     09 Jun 2009 [RWC]

  Mod:     Modify to compute extreme value approximation to Alpha(i).
  Date:    10 Jun 2009 [RWC]

*/


/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "AlphaSim"                      /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_INITIAL "18 Jun 1997"     /* date of initial program release */
#define PROGRAM_LATEST  "09 Jun 2009"     /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#include <time.h>
#include <sys/types.h>
#include <unistd.h>

#define MAX_NAME_LENGTH THD_MAX_NAME /* max. string length for file names */
#define MAX_CLUSTER_SIZE 10000       /* max. size of cluster for freq. table */

/*---------------------------------------------------------------------------*/
/*
  Global data
*/

static char * mask_filename = NULL;  /* file containing the mask */
static byte * mask_vol  = NULL;      /* mask volume */
static int mask_ngood = 0;           /* number of good voxels in mask volume */
/* allow updating via the -max_clust_size option         12 Apr 2006 [rickr] */
static int g_max_cluster_size = MAX_CLUSTER_SIZE;

static int use_zg = 0 ;  /* 10 Jan 2008 */

static unsigned int gseed ;

/*---------------------------------------------------------------------------*/
/*
  Routine to display AlphaSim help menu.
*/

void display_help_menu()
{
  printf
    (
     "This program performs alpha probability simulations.  \n\n"
     "Usage: \n"
     "AlphaSim \n"
     "-nx n1        n1 = number of voxels along x-axis                      \n"
     "-ny n2        n2 = number of voxels along y-axis                      \n"
     "-nz n3        n3 = number of voxels along z-axis                      \n"
     "-dx d1        d1 = voxel size (mm) along x-axis                       \n"
     "-dy d2        d2 = voxel size (mm) along y-axis                       \n"
     "-dz d3        d3 = voxel size (mm) along z-axis                       \n"
     "-nxyz n1 n2 n3   = give all 3 grid dimensions at once                 \n"
     "-dxyz d1 d2 d3   = give all 3 voxel sizes at once                     \n"
     "[-mask mset]      Use the 0 sub-brick of dataset 'mset' as a mask     \n"
     "                    to indicate which voxels to analyze (a sub-brick  \n"
     "                    selector is allowed)  [default = use all voxels]  \n"
     "                  Note:  The -mask command also REPLACES the          \n"
     "                         -nx, -ny, -nz, -dx, -dy, and -dz commands,   \n"
     "                         and takes the volume dimensions from 'mset'. \n"
     "[-fwhm s]     s  = Gaussian filter width (FWHM, in mm)                \n"
     "[-fwhmx sx]   sx = Gaussian filter width, x-axis (FWHM)               \n"
     "[-fwhmy sy]   sy = Gaussian filter width, y-axis (FWHM)               \n"
     "[-fwhmz sz]   sz = Gaussian filter width, z-axis (FWHM)               \n"
     "[-sigma s]    s  = Gaussian filter width (1 sigma, in mm)             \n"
     "[-sigmax sx]  sx = Gaussian filter width, x-axis (1 sigma)            \n"
     "[-sigmay sy]  sy = Gaussian filter width, y-axis (1 sigma)            \n"
     "[-sigmaz sz]  sz = Gaussian filter width, z-axis (1 sigma)            \n"
     "\n"
     "[-power]      perform statistical power calculations                  \n"
     "[-ax n1]      n1 = extent of active region (in voxels) along x-axis   \n"
     "[-ay n2]      n2 = extent of active region (in voxels) along y-axis   \n"
     "[-az n3]      n3 = extent of active region (in voxels) along z-axis   \n"
     "[-zsep z]     z = z-score separation between signal and noise         \n"
     "\n"
     "[-rmm r]      r  = cluster connection radius (mm)                     \n"
     "                   Default is nearest neighbor connection only.       \n"
     "-pthr p       p  = individual voxel threshold probability             \n"
     "-iter n       n  = number of Monte Carlo simulations                  \n"
     "[-quiet]      suppress lengthy per-iteration screen output            \n"
     "[-out file]   file = name of output file [default value = screen]     \n"
     "[-max_clust_size size]  size = maximum allowed voxels in a cluster    \n"
     "[-seed S]     S  = random number seed\n"
     "                   default seed = 1234567\n"
     "                   if seed=0, then program will randomize it\n"
     "[-fast]       Use a faster random number generator:\n"
     "                Can speed program up by about a factor of 2,\n"
     "                but detailed results will differ slightly since\n"
     "                a different sequence of random values will be used.\n"
     "\n"
     "Unix environment variables you can use:\n"
     "---------------------------------------\n"
     " Set AFNI_BLUR_FFT to YES to require blurring be done with FFTs\n"
     "   (the oldest way, and slowest).\n"
     " Set AFNI_BLUR_FFT to NO and AFNI_BLUR_FIROLD to YES to require\n"
     "   blurring to be done with the old (crude) FIR code (not advised).\n"
     " If neither of these are set, then blurring is done using the newer\n"
     "   (more accurate) FIR code (recommended).\n"
     " Results will differ in detail depending on the blurring method\n"
     "   used to generate the simulated noise fields.\n"
     );

  printf("\n"
   "SAMPLE OUTPUT:\n"
   "--------------\n"
   " AlphaSim -nxyz 64 64 10 -dxyz 3 3 3 -iter 10000 -pthr 0.004 -fwhm 3 -quiet -fast\n"
   "\n"
   "Cl Size     Frequency    CumuProp     p/Voxel   Max Freq       Alpha\n"
   "      1       1316125    0.898079  0.00401170          0    1.000000\n"
   "      2        126353    0.984298  0.00079851       1023    1.000000\n"
   "      3         18814    0.997136  0.00018155       5577    0.897700\n"
   "      4          3317    0.999400  0.00004375       2557    0.340000\n"
   "      5           688    0.999869  0.00001136        653    0.084300\n"
   "      6           150    0.999971  0.00000296        148    0.019000\n"
   "      7            29    0.999991  0.00000076         29    0.004200\n"
   "      8             8    0.999997  0.00000027          8    0.001300\n"
   "      9             5    1.000000  0.00000011          5    0.000500\n"
   "\n"
   " That is, thresholded random noise alone (no signal) would produce a\n"
   " cluster of size 6 or larger 1.9%% (Alpha) of the time, in a 64x64x64\n"
   " volume with cubical 3 mm voxels and a FHWM noise smoothness of 3 mm.\n"
   "\n"
   "N.B.: If you run the exact command above, you will get slightly\n"
   " different results, due to variations in the random numbers generated\n"
   " in the simulations.\n"
   "\n"
  ) ;

  PRINT_AFNI_OMP_USAGE("AlphaSim",
                       "* OpenMP compilation implies '-fast'\n" ) ;

  exit(0);
}

/*---------------------------------------------------------------------------*/
/*
   Routine to print error message and stop.
*/

#if 0
void AlphaSim_error (char * message)
{
   fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
   exit(1);
}
#else
# define AlphaSim_error(m) ERROR_exit("AlphaSim: %s",(m))
#endif


/*---------------------------------------------------------------------------*/
/*
  Routine to initialize the input options.
*/

void initialize_options (
		 int * nx, int * ny, int * nz,
		 float * dx, float * dy, float * dz,
		 int * filter, float * sigmax, float * sigmay, float * sigmaz,
		 int * egfw,
		 int * power, int * ax, int * ay, int * az, float * zsep,
		 float * rmm, float * pthr, int * niter, int * quiet,
	         char ** outfilename, int * seed)

{
  *nx = 0;                   /* number of voxels along x-axis */
  *ny = 0;                   /* number of voxels along y-axis */
  *nz = 0;                   /* number of voxels along z-axis */
  *dx = 0.0;                 /* voxel size along x-axis */
  *dy = 0.0;                 /* voxel size along y-axis */
  *dz = 0.0;                 /* voxel size along z-axis */
  *filter = 0;               /* flag for Gaussian filtering */
  *sigmax = 0.0;             /* Gaussian filter width, x-axis (1 sigma) */
  *sigmay = 0.0;             /* Gaussian filter width, y-axis (1 sigma) */
  *sigmaz = 0.0;             /* Gaussian filter width, z-axis (1 sigma) */
  *egfw = 0;                 /* flag for estimation of filter width */
  *power = 0;                /* flag for power calculations */
  *ax = 0;                   /* number of activation voxels along x-axis */
  *ay = 0;                   /* number of activation voxels along y-axis */
  *az = 0;                   /* number of activation voxels along z-axis */
  *zsep = 0.0;               /* z-score signal and noise separation */
  *rmm = -1.0 ;              /* cluster connection radius (mm) */
  *pthr = 0.0;               /* individual voxel threshold prob. */
  *niter = 0;                /* number of Monte Carlo simulations  */
  *quiet = 0;                /* generate screen output (default)  */
  *outfilename = NULL;       /* name of output file */
  *seed = 1234567;           /* random number seed - user can override */
}


/*---------------------------------------------------------------------------*/
/*
  Routine to get user specified input options.
*/

void get_options (int argc, char ** argv,
		  int * nx, int * ny, int * nz,
		  float * dx, float * dy, float * dz,
		  int * filter, float * sigmax, float * sigmay, float * sigmaz,
		  int * egfw,
		  int * power, int * ax, int * ay, int * az, float * zsep,
		  float * rmm, float * pthr, int * niter, int * quiet,
		  char ** outfilename, int * seed)
{
  int nopt = 1;                  /* input option argument counter */
  int ival;                      /* integer input */
  float fval;                    /* float input */
  int mask_nx, mask_ny, mask_nz, mask_nvox;   /* mask dimensions */
  float  mask_dx, mask_dy, mask_dz;


  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();


  /*----- add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv);


  /*----- initialize the input options -----*/
  initialize_options (nx, ny, nz, dx, dy, dz, filter, sigmax, sigmay, sigmaz,
		      egfw, power, ax, ay, az, zsep, rmm, pthr, niter, quiet,
		      outfilename, seed);

  /*----- main loop over input options -----*/
  while (nopt < argc )
    {

      /*-----  -zg [10 Jan 2008] -----*/

      if( strcmp(argv[nopt],"-zg") == 0 || strcmp(argv[nopt],"-fast") == 0 ){
        use_zg = 1 ; nopt++ ; continue ;
      }
      if( strcmp(argv[nopt],"-nozg") == 0 || strcmp(argv[nopt],"-nofast") == 0 ){
        use_zg = 0 ; nopt++ ; continue ;
      }

      /*-----  -nxyz n1 n2 n3 [10 Jan 2008: RWC] -----*/

      if( strcmp(argv[nopt],"-nxyz") == 0 ){
        nopt++ ; if( nopt+2 >= argc ) AlphaSim_error ("need 3 arguments after -nxyz ") ;
        *nx = (int)strtod(argv[nopt++],NULL); if( *nx <= 0 ) AlphaSim_error("illegal n1 value") ;
        *ny = (int)strtod(argv[nopt++],NULL); if( *ny <= 0 ) AlphaSim_error("illegal n2 value") ;
        *nz = (int)strtod(argv[nopt++],NULL); if( *nz <= 0 ) AlphaSim_error("illegal n3 value") ;
        continue ;
      }

      /*-----  -dxyz d1 d2 d3 [10 Jan 2008: RWC] -----*/

      if( strcmp(argv[nopt],"-dxyz") == 0 ){
        nopt++ ; if( nopt+2 >= argc ) AlphaSim_error ("need 3 arguments after -dxyz ") ;
        *dx = strtod(argv[nopt++],NULL); if( *dx <= 0 ) AlphaSim_error("illegal d1 value") ;
        *dy = strtod(argv[nopt++],NULL); if( *dy <= 0 ) AlphaSim_error("illegal d2 value") ;
        *dz = strtod(argv[nopt++],NULL); if( *dz <= 0 ) AlphaSim_error("illegal d3 value") ;
        continue ;
      }

      /*-----   -nx n  -----*/
      if (strncmp(argv[nopt], "-nx", 3) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -nx ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    AlphaSim_error ("illegal argument after -nx ");
	  *nx = ival;
	  nopt++;
	  continue;
	}


      /*-----   -ny n  -----*/
      if (strncmp(argv[nopt], "-ny", 3) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -ny ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    AlphaSim_error ("illegal argument after -ny ");
	  *ny = ival;
	  nopt++;
	  continue;
	}


      /*-----   -nz n  -----*/
      if (strncmp(argv[nopt], "-nz", 3) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -nz ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    AlphaSim_error ("illegal argument after -nz ");
	  *nz = ival;
	  nopt++;
	  continue;
	}


      /*-----   -dx d   -----*/
      if (strncmp(argv[nopt], "-dx", 3) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -dx ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval <= 0.0)
	    AlphaSim_error ("illegal argument after -dx ");
	  *dx = fval;
	  nopt++;
	  continue;
	}


      /*-----   -dy d   -----*/
      if (strncmp(argv[nopt], "-dy", 3) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -dy ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval <= 0.0)
	    AlphaSim_error ("illegal argument after -dy ");
	  *dy = fval;
	  nopt++;
	  continue;
	}


      /*-----   -dz d   -----*/
      if (strncmp(argv[nopt], "-dz", 3) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -dz ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval <= 0.0)
	    AlphaSim_error ("illegal argument after -dz ");
	  *dz = fval;
	  nopt++;
	  continue;
	}


      /**** -mask mset [14 June 2000] ****/

      if (strcmp(argv[nopt],"-mask") == 0 )
	{
	  THD_3dim_dataset * mset ; int ii,mc ;
	  nopt++ ;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -mask!") ;
	  mask_filename = (char *) malloc (sizeof(char) * MAX_NAME_LENGTH);
	  if (mask_filename == NULL)
	    AlphaSim_error ("unable to allocate memory");
	  strcpy (mask_filename, argv[nopt]);
	  mset = THD_open_dataset (mask_filename);
	  if (mset == NULL)  AlphaSim_error ("can't open -mask dataset!") ;

	  mask_vol = THD_makemask( mset , 0 , 1.0,0.0 ) ;
	  if (mask_vol == NULL )  AlphaSim_error ("can't use -mask dataset!") ;

	  mask_nvox = DSET_NVOX(mset) ;
	  mask_nx   = DSET_NX(mset);
	  mask_ny   = DSET_NY(mset);
	  mask_nz   = DSET_NZ(mset);
	  mask_dx   = DSET_DX(mset);
	  mask_dy   = DSET_DY(mset);
	  mask_dz   = DSET_DZ(mset);

	  DSET_delete(mset) ;

	  for( ii=mc=0 ; ii < mask_nvox ; ii++ )  if (mask_vol[ii])  mc++ ;
	  if (mc == 0)  AlphaSim_error ("mask is all zeros!") ;
	  if( *quiet < 2 ) printf("++ %d voxels in mask\n",mc) ;
	  mask_ngood = mc;
	  nopt++ ; continue ;
	}


      /*-----   -fwhm s   -----*/
      if (strncmp(argv[nopt], "-fwhm", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -fwhm ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval < 0.0)
	    AlphaSim_error ("illegal argument after -fwhm ");
	  if (fval > 0.0)  *filter = 1;
	  *sigmax = FWHM_TO_SIGMA(fval);
	  *sigmay = FWHM_TO_SIGMA(fval);
	  *sigmaz = FWHM_TO_SIGMA(fval);
	  nopt++;
	  continue;
	}


      /*-----   -fwhmx sx   -----*/
      if (strncmp(argv[nopt], "-fwhmx", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -fwhmx ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval < 0.0)
	    AlphaSim_error ("illegal argument after -fwhmx ");
	  if (fval > 0.0)  *filter = 1;
	  *sigmax = FWHM_TO_SIGMA(fval);
	  nopt++;
	  continue;
	}


      /*-----   -fwhmy sy   -----*/
      if (strncmp(argv[nopt], "-fwhmy", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -fwhmy ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval < 0.0)
	    AlphaSim_error ("illegal argument after -fwhmy ");
	  if (fval > 0.0)  *filter = 1;
	  *sigmay = FWHM_TO_SIGMA(fval);
	  nopt++;
	  continue;
	}


      /*-----   -fwhmz sz   -----*/
      if (strncmp(argv[nopt], "-fwhmz", 6) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -fwhmz ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval < 0.0)
	    AlphaSim_error ("illegal argument after -fwhmz ");
	  if (fval > 0.0)  *filter = 1;
	  *sigmaz = FWHM_TO_SIGMA(fval);
	  nopt++;
	  continue;
	}


      /*-----   -sigma s   -----*/
      if (strncmp(argv[nopt], "-sigma", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -sigma ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval < 0.0)
	    AlphaSim_error ("illegal argument after -sigma ");
	  if (fval > 0.0)  *filter = 1;
	  *sigmax = fval;
	  *sigmay = fval;
	  *sigmaz = fval;
	  nopt++;
	  continue;
	}


      /*-----   -sigmax sx   -----*/
      if (strncmp(argv[nopt], "-sigmax", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -sigmax ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval < 0.0)
	    AlphaSim_error ("illegal argument after -sigmax ");
	  if (fval > 0.0)  *filter = 1;
	  *sigmax = fval;
	  nopt++;
	  continue;
	}


      /*-----   -sigmay sy   -----*/
      if (strncmp(argv[nopt], "-sigmay", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -sigmay ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval < 0.0)
	    AlphaSim_error ("illegal argument after -sigmay ");
	  if (fval > 0.0)  *filter = 1;
	  *sigmay = fval;
	  nopt++;
	  continue;
	}


      /*-----   -sigmaz sz   -----*/
      if (strncmp(argv[nopt], "-sigmaz", 7) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -sigmaz ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval < 0.0)
	    AlphaSim_error ("illegal argument after -sigmaz ");
	  if (fval > 0.0)  *filter = 1;
	  *sigmaz = fval;
	  nopt++;
	  continue;
	}


      /*-----   -egfw   -----*/
      if (strncmp(argv[nopt], "-egfw", 5) == 0)
	{
	  *egfw = 1;
	  nopt++;
	  continue;
	}


      /*-----   -power   -----*/
      if (strncmp(argv[nopt], "-power", 6) == 0)
	{
	  *power = 1;
	  nopt++;
	  continue;
	}


      /*-----   -ax n  -----*/
      if (strncmp(argv[nopt], "-ax", 3) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -ax ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    AlphaSim_error ("illegal argument after -ax ");
	  *ax = ival;
	  nopt++;
	  continue;
	}


      /*-----   -ay n  -----*/
      if (strncmp(argv[nopt], "-ay", 3) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -ay ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    AlphaSim_error ("illegal argument after -ay ");
	  *ay = ival;
	  nopt++;
	  continue;
	}


      /*-----   -az n  -----*/
      if (strncmp(argv[nopt], "-az", 3) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -az ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    AlphaSim_error ("illegal argument after -az ");
	  *az = ival;
	  nopt++;
	  continue;
	}


      /*-----   -zsep z   -----*/
      if (strncmp(argv[nopt], "-zsep", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -zsep ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval < 0.0)
	    AlphaSim_error ("illegal argument after -zsep ");
	  *zsep = fval;
	  nopt++;
	  continue;
	}


      /*-----   -rmm r   -----*/
      if (strncmp(argv[nopt], "-rmm", 4) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -rmm ");
	  sscanf (argv[nopt], "%f", &fval);
	  if (fval <= 0.0) INFO_message("-rmm set to %g ==> NN clustering",fval) ;
	  *rmm = fval;
	  nopt++;
	  continue;
	}


      /*-----   -pthr p   -----*/
      if (strncmp(argv[nopt], "-pthr", 5) == 0 || strcmp(argv[nopt],"-pval") == 0 )
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -pthr ");
	  sscanf (argv[nopt], "%f", &fval);
	  if ((fval <= 0.0) || (fval > 1.0))
	    AlphaSim_error ("illegal argument after -pthr ");
	  *pthr = fval;
	  nopt++;
	  continue;
	}


      /*-----   -iter n  -----*/
      if (strncmp(argv[nopt], "-iter", 5) == 0 || strcmp(argv[nopt],"-niter") == 0 )
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -iter ");
	  sscanf (argv[nopt], "%d", &ival);
	  if (ival <= 0)
	    AlphaSim_error ("illegal argument after -iter ");
	  *niter = ival;
	  nopt++;
	  continue;
	}


      /*-----   -out filename   -----*/
      if (strncmp(argv[nopt], "-out", 4) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -out ");
	  *outfilename = AFMALL( char, sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (*outfilename, argv[nopt]);
	  nopt++;
	  continue;
	}


      /*-----   -max_clust_size size   -----*/
      if (strncmp(argv[nopt], "-max_clust_size", 10) == 0)
	{
	  nopt++;
	  if (nopt >= argc)
             AlphaSim_error ("need argument after -max_clust_size ");
	  g_max_cluster_size = atoi( argv[nopt] );
	  nopt++;
	  continue;
	}

      /*-----   -quiet q  -----*/
      if (strncmp(argv[nopt], "-quiet", 2) == 0)
	{
	  (*quiet)++;
	  nopt++;
	  continue;
	}


      /*-----   -seed S  -----*/
      if (strncmp(argv[nopt], "-seed", 5) == 0)
	{
	  nopt++;
	  if (nopt >= argc)
             AlphaSim_error ("need argument after -seed ");
	  *seed = atoi(argv[nopt]);
     if( *seed == 0 ){
       *seed = (int)time(NULL) + (int)getpid() ;
       if( *seed < 0 ) *seed = -*seed ;
       INFO_message("-seed 0 resets to %d",*seed) ;
     }
	  nopt++;
	  continue;
	}


      /*----- unknown command -----*/
      ERROR_exit("AlphaSim -- unknown option '%s'",argv[nopt]) ;
    }


  /*----- If mask dataset is used, set dimensions accordingly -----*/
  if (mask_vol != NULL)
    {
      *nx = mask_nx;  *ny = mask_ny;  *nz = mask_nz;
      *dx = fabs(mask_dx);  *dy = fabs(mask_dy);  *dz = fabs(mask_dz);
    }

}


/*---------------------------------------------------------------------------*/
/*
  Routine to check for valid inputs.
*/

void check_for_valid_inputs (int nx,  int ny,  int nz,
			     float dx,  float dy,  float dz,  int filter,
			     float sigmax,  float sigmay,  float sigmaz,
			     int power, int ax,  int ay,  int az,  float zsep,
			     float rmm,  float pthr,  int niter,
			     char * outfilename)
{
  FILE * fout = NULL;
  char message[MAX_NAME_LENGTH];     /* error message */

  if (nx <= 0)  AlphaSim_error ("Illegal value for nx ");
  if (ny <= 0)  AlphaSim_error ("Illegal value for ny ");
  if (nz <= 0)  AlphaSim_error ("Illegal value for nz ");
  if (dx <= 0.0)  AlphaSim_error ("Illegal value for dx ");
  if (dy <= 0.0)  AlphaSim_error ("Illegal value for dy ");
  if (dz <= 0.0)  AlphaSim_error ("Illegal value for dz ");
  if (filter  &&  ((sigmax <= 0.0) || (sigmay <= 0.0) || (sigmaz <= 0.0)))
    AlphaSim_error ("Illegal Gaussian filter width specification ");
  if (power  &&  ((ax <= 0) || (ay <= 0) || (az <= 0)))
    AlphaSim_error ("Illegal dimensions for activation region ");
  if (power && (zsep <= 0.0))  AlphaSim_error ("Illegal value for zsep ");
  if ( (rmm < dx) && (rmm < dy) && (rmm < dz) ){
     rmm = -1.0f ; INFO_message("default NN connectivity being used") ;
  }
  if ((pthr <= 0.0) || (pthr >= 1.0))
    AlphaSim_error ("Illegal value for pthr ");
  if (niter <= 0)  AlphaSim_error ("Illegal value for niter ");

  if (outfilename != NULL && !THD_ok_overwrite())
    {
      /*----- see if output file already exists -----*/
      fout = fopen (outfilename, "r");
      if (fout != NULL)
	{
     fclose(fout) ;
	  sprintf (message, "Output file %s already exists. ", outfilename);
	  AlphaSim_error (message);
	}
    }

                                                  /* 12 Apr 2006 [rickr] */
  /* use a limit of ten million, more than the voxels in a +tlrc dataset */
  if (g_max_cluster_size < 2 || g_max_cluster_size > 10000000)
  {
      sprintf (message, "Invalid -max_clust_size %d.  Must be in [%d,%d]. ",
               g_max_cluster_size, 2, 10000000);
      AlphaSim_error (message);
  }
}


/*---------------------------------------------------------------------------*/
/*
  Routine to perform program initialization.
*/

void initialize (int argc, char ** argv,
		 int * nx, int * ny, int * nz,
		 float * dx, float * dy, float * dz,
		 int * filter, float * sigmax, float * sigmay, float * sigmaz,
		 int * egfw, float * avgsx, float * avgsy, float * avgsz,
		 int * power, int * ax, int * ay, int * az, float * zsep,
		 float * rmm, float * pthr, int * niter, int * quiet,
	         char ** outfilename, long * count,
		 double * sum, double * sumsq, float * power_thr,
		 float ** fim, float ** arfim,
		 long ** freq_table, long ** max_table)

{
  int i;
  int nxyz;

  int which;
  double p, q, z, mean, sd;
  int status, seed;
  double bound;


  /*----- get command line inputs -----*/
  get_options(argc, argv,
	      nx, ny, nz, dx, dy, dz, filter, sigmax, sigmay, sigmaz,
	      egfw, power, ax, ay, az, zsep, rmm, pthr, niter, quiet,
	      outfilename, &seed);


  /*----- check for valid inputs -----*/
  check_for_valid_inputs (*nx,  *ny,  *nz,  *dx,  *dy,  *dz,  *filter,
			  *sigmax,  *sigmay,  *sigmaz,
			  *power, *ax,  *ay,  *az,  *zsep,
			  *rmm,  *pthr,  *niter,  *outfilename);


  /** 09 Jun 2009: for OpenMP, have moved allocation of fim/arfim to main() **/

  /*----- allocate memory space for image data -----*/
#if 0
  nxyz = (*nx) * (*ny) * (*nz);
  *fim = (float *) malloc(nxyz * sizeof(float));
  if (*fim == NULL)
    AlphaSim_error ("memory allocation error");
#endif


  /*-- if power calculation, allocate memory space for activation region --*/
#if 0
  if (*power)
    {
      nxyz = (*ax) * (*ay) * (*az);
      *arfim = (float *) malloc(nxyz * sizeof(float));
      if (*arfim == NULL)
	AlphaSim_error ("memory allocation error");
    }
#endif

  /** 09 Jun 2009: however, the _table variables are common to all threads **/

  /*----- allocate memory space and initialize frequency table -----*/
  *freq_table = (long *) malloc( g_max_cluster_size * sizeof(long) );
  if (*freq_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 0;  i < g_max_cluster_size;  i++)
    (*freq_table)[i] = 0;


  /*----- allocate memory space and initialize max cluster size table -----*/
  *max_table = (long *) malloc( g_max_cluster_size * sizeof(long) );
  if (*max_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 0;  i < g_max_cluster_size;  i++)
    (*max_table)[i] = 0;


  /*----- initialize voxel intensity sums -----*/
  *count = 0;
  *sum = 0.0;
  *sumsq = 0.0;


  /*----- calculate power threshold -----*/
  if (*power)
    {
      which = 2;
      q = *pthr;
      p = 1.0 - q;
      mean = 0.0;
      sd = 1.0;
      cdfnor (&which, &p, &q, &z, &mean, &sd, &status, &bound);

      z = *zsep - z;
      which = 1;
      cdfnor (&which, &p, &q, &z, &mean, &sd, &status, &bound);
      *power_thr = p;
    }


  /*----- initialize ave. est. gaussian filter widths -----*/
  if (*egfw)
    {
      *avgsx = 0.0;   *avgsy = 0.0;   *avgsz = 0.0;
    }

  /*----- initialize random number generator -----*/
  srand48 (seed); gseed = (unsigned int)seed ;

}

/*---------------------------------------------------------------------------*/
#include "zgaussian.c"
/*---------------------------------------------------------------------------*/

#ifndef USE_OMP
/*---------------------------------------------------------------------------*/
/*
  Routine to generate a uniform U(0,1) random variate.
*/

float uniform ()
{
  return ( (float)drand48() );
}


/*---------------------------------------------------------------------------*/
/*
  Routine to generate a normal N(0,1) random variate.
*/

void normal (float * n1, float * n2)
{
  float u1, u2;
  float r;

  /** the -fast way **/
  if( use_zg ){ *n1 = zgaussian() ; *n2 = zgaussian() ; return ; }

  u1 = 0.0;
  while (u1 <= 0.0)
    {
      u1 = uniform();
    }
  u2 = uniform();

  r   = sqrt(-2.0*log(u1));
  *n1 = r * cos(2.0*PI*u2);
  *n2 = r * sin(2.0*PI*u2);
}
#endif


/*---------------------------------------------------------------------------*/
/*
  Routine to calculate the dimensions of the activation region.
*/

void activation_region (int nx, int ny, int nz, int ax, int ay, int az,
			int * xbot, int * xtop, int * ybot, int * ytop,
			int * zbot, int * ztop)
{
  *xbot = nx/2 - ax/2;
  *xtop = *xbot + ax;
  *ybot = ny/2 - ay/2;
  *ytop = *ybot + ay;
  *zbot = nz/2 - az/2;
  *ztop = *zbot + az;
}


/*---------------------------------------------------------------------------*/
/*
  Routine to generate volume of random voxel intensities.
*/

void generate_image (int nx, int ny, int nz, int power,
		     int ax, int ay, int az, float zsep, float * fim,
           unsigned short xran[] )
{
  int nxy, nxyz;
  int nxyzdiv2;
  int ixyz;
  float n1, n2;
  int xbot, xtop, ybot, ytop, zbot, ztop;
  int ix, jy, kz;


  /*----- initialize local variables -----*/
  nxy = nx * ny;
  nxyz = nxy * nz;
  nxyzdiv2 = nxyz / 2;

  /*----- generate random image -----*/
#ifndef USE_OMP
  for (ixyz = 0;  ixyz < nxyzdiv2;  ixyz++)
    {
      normal(&n1, &n2);
      fim[ixyz] = n1;
      fim[ixyz+nxyzdiv2] = n2;
    }
  normal(&n1, &n2);
  fim[nxyz-1] = n1;
#else
  for( ixyz=0 ; ixyz < nxyz ; ixyz++ )
    fim[ixyz] = zgaussian_sss(xran) ;
#endif

  /*----- if power calculation, generate "island" of activation -----*/
  if (power)
    {
      /*--- calculate dimensions of activation region ---*/
      activation_region (nx, ny, nz, ax, ay, az,
			 &xbot, &xtop, &ybot, &ytop, &zbot, &ztop);

      /*--- add z-score offset to voxels within activation region ---*/
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  IJK_TO_THREE (ixyz, ix, jy, kz, nx, nxy);
	  if ( (ix >= xbot) && (ix < xtop)
	    && (jy >= ybot) && (jy < ytop)
	    && (kz >= zbot) && (kz < ztop) )
	    fim[ixyz] += zsep;
	}
    }
}


/*---------------------------------------------------------------------------*/
/*
  Routine to apply Gaussian filter to the volume data.
*/

void gaussian_filter (int nx, int ny, int nz, float dx, float dy, float dz,
		      float rmm, float sigmax, float sigmay, float sigmaz,
		      float * fim)
{

  if( AFNI_yesenv("AFNI_BLUR_FFT") ) EDIT_blur_allow_fir(0) ;  /* 03 Apr 2007 */

  /*----- use Gaussian blur routine -----*/
  EDIT_blur_volume_3d (nx, ny, nz, dx, dy, dz,
		       MRI_float, fim, sigmax, sigmay, sigmaz);

}


/*---------------------------------------------------------------------------*/
/*
  Routine to estimate the Gaussian filter width required to generate the data.
*/

void estimate_gfw (int nx, int ny, int nz, float dx, float dy, float dz,
		   int niter, int quiet, float * fim,
		   float * avgsx, float * avgsy, float * avgsz)
{
  int nxy, nxyz;                /* total number of voxels */
  int ixyz;                     /* voxel index */
  int ix, jy, kz, ixyz2;
  float fsum, fsq, var;
  float dfdx, dfdxsum, dfdxsq, varxx;
  float dfdy, dfdysum, dfdysq, varyy;
  float dfdz, dfdzsum, dfdzsq, varzz;
  int countx, county, countz;
  float sx, sy, sz;
  float arg;


  /*----- initialize local variables -----*/
  nxy = nx * ny;
  nxyz = nxy * nz;


  /*----- estimate the variance of the data -----*/
  fsum = 0.0;
  fsq = 0.0;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      fsum += fim[ixyz];
      fsq  += fim[ixyz] * fim[ixyz];
    }
  var = (fsq - (fsum * fsum)/nxyz) / (nxyz-1);


  /*----- estimate the partial derivatives -----*/
  dfdxsum = 0.0;   dfdysum = 0.0;   dfdzsum = 0.0;
  dfdxsq = 0.0;    dfdysq  = 0.0;   dfdzsq = 0.0;
  countx = 0;      county = 0;      countz = 0;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    {
      IJK_TO_THREE (ixyz, ix, jy, kz, nx, nxy);

      if (ix+1 < nx)
	{
	  ixyz2 = THREE_TO_IJK (ix+1, jy, kz, nx, nxy);
	  dfdx = (fim[ixyz2] - fim[ixyz]) / 1.0;
	  dfdxsum += dfdx;
	  dfdxsq  += dfdx * dfdx;
	  countx += 1;
	}

      if (jy+1 < ny)
	{
	  ixyz2 = THREE_TO_IJK (ix, jy+1, kz, nx, nxy);
	  dfdy = (fim[ixyz2] - fim[ixyz]) / 1.0;
	  dfdysum += dfdy;
	  dfdysq  += dfdy * dfdy;
	  county += 1;
	}

      if (kz+1 < nz)
	{
	  ixyz2 = THREE_TO_IJK (ix, jy, kz+1, nx, nxy);
	  dfdz = (fim[ixyz2] - fim[ixyz]) / 1.0;
	  dfdzsum += dfdz;
	  dfdzsq  += dfdz * dfdz;
	  countz += 1;
	}

     }

  /*----- estimate the variance of the partial derivatives -----*/
  if (countx < 2)
    varxx = 0.0;
  else
    varxx = (dfdxsq - (dfdxsum * dfdxsum)/countx) / (countx-1);

  if (county < 2)
    varyy = 0.0;
  else
    varyy = (dfdysq - (dfdysum * dfdysum)/county) / (county-1);

  if (countz < 2)
    varzz = 0.0;
  else
    varzz = (dfdzsq - (dfdzsum * dfdzsum)/countz) / (countz-1);


  /*----- now estimate the equivalent Gaussian filter width -----*/
  arg = 1.0 - 0.5*(varxx/var);
  if ( (arg <= 0.0) || (varxx == 0.0) )
    sx = 0.0;
  else
    sx = sqrt( -1.0 / (4.0*log(arg)) ) * dx;

  arg = 1.0 - 0.5*(varyy/var);
  if ( (arg <= 0.0) || (varyy == 0.0) )
    sy = 0.0;
  else
    sy = sqrt( -1.0 / (4.0*log(arg)) ) * dy;

  arg = 1.0 - 0.5*(varzz/var);
  if ( (arg <= 0.0) || (varzz == 0.0) )
    sz = 0.0;
  else
    sz = sqrt( -1.0 / (4.0*log(arg)) ) * dz;

  /*-----  save results  -----*/
#pragma omp critical (AVG)
 {
  *avgsx += sx / niter;
  *avgsy += sy / niter;
  *avgsz += sz / niter;
 }

  /*-----  output results  -----*/
  if (!quiet)
    {
      printf ("var  =%f \n", var);
      printf ("varxx=%f varyy=%f varzz=%f \n", varxx, varyy, varzz);
      printf ("   sx=%f    sy=%f    sz=%f \n", sx, sy, sz);
    }
}


/*---------------------------------------------------------------------------*/
/*
  Routine to copy the activation region into a separate volume.
*/

void get_activation_region (int nx, int ny, int nz, int ax, int ay, int az,
			    float pthr, float zsep, float * fim, float * arfim)
{
  int nxy, nxyz;
  int axy, axyz;
  int ixyz, ixyz2;
  int xbot, xtop, ybot, ytop, zbot, ztop;
  int ix, jy, kz;


  /*----- initialize local variables -----*/
  nxy = nx * ny;
  nxyz = nxy * nz;
  axy = ax * ay;
  axyz = axy * az;


  /*--- calculate dimensions of activation region ---*/
  activation_region (nx, ny, nz, ax, ay, az,
		     &xbot, &xtop, &ybot, &ytop, &zbot, &ztop);

  /*--- copy activation region ---*/
  for (ixyz = 0;  ixyz < axyz;  ixyz++)
    {
      IJK_TO_THREE (ixyz, ix, jy, kz, ax, axy);
      ix += xbot;
      jy += ybot;
      kz += zbot;
      ixyz2 = THREE_TO_IJK (ix, jy, kz, nx, nxy);
      arfim[ixyz] = fim[ixyz2];
    }


}



/*---------------------------------------------------------------------------*/
/*
  Routine to calculate threshold probability.
*/

float pcalc (int nx, int ny, int nz, float * fim, float zthr)
{
  int nxyz;
  int ixyz;
  int pcount;
  float p;


  /*----- initialize local variables -----*/
  nxyz = nx * ny * nz;

  pcount = 0;
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (fim[ixyz] > zthr)  pcount ++;
  p = (float)pcount / (float)nxyz;

  return (p);
}


/*---------------------------------------------------------------------------*/
/*
  Routine to apply threshold to volume data.
*/

void threshold_data (int nx, int ny, int nz, float * fim,
		     float pthr, long * count, double * sum, double * sumsq,
		     int quiet, int iter)
{
  int ixyz;
  int nxyz;
  float zthr;
  float pact;

  int which;
  double p, q, z, mean, sd;
  int status;
  double bound;


  /*----- initialize local variables -----*/
  nxyz = nx * ny * nz;


  /*----- update sums -----*/
  if (*count < 1.0e+09)
    {
      *count += nxyz;
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  *sum += fim[ixyz];
	  *sumsq += fim[ixyz] * fim[ixyz];
	}
    }


  /*----- calculate z-threshold -----*/
  which = 2;
  p = 1.0 - pthr;
  q = pthr;
  mean = (*sum) / (*count);
  sd = sqrt(((*sumsq) - ((*sum) * (*sum))/(*count)) / ((*count)-1));
  cdfnor (&which, &p, &q, &z, &mean, &sd, &status, &bound);
  zthr = z;

  if (!quiet)
    {
      pact = pcalc (nx, ny, nz, fim, zthr);
      printf ("pthr=%f  zthr=%f  pact=%f  ", pthr, zthr, pact);
    }


  /*----- apply threshold to image data -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (fim[ixyz] > zthr)
      fim[ixyz] = 1.0;
    else
      fim[ixyz] = 0.0;


}


/*---------------------------------------------------------------------------*/
/*
  Routine to apply mask to volume data.
*/

void apply_mask (int nx, int ny, int nz, float * fim)

{
  int ixyz;
  int nxyz;


  /*----- initialize local variables -----*/
  nxyz = nx * ny * nz;


  /*----- apply mask to volume data -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
    if (! mask_vol[ixyz])
      fim[ixyz] = 0.0;


}


/*---------------------------------------------------------------------------*/
/*
  Routine to identify clusters.
*/

void identify_clusters (int nx,  int ny,  int nz,
			float dx,  float dy,  float dz,
			float rmm,  float * fim,  int quiet,
			long * freq_table,  long * max_table)
/*
  where
       rmm = cluster connection radius (mm)
       nx = number of voxels along x-axis
       ny = number of voxels along y-axis
       nz = number of voxels along z-axis
       dx = voxel size along x-axis
       dy = voxel size along y-axis
       dz = voxel size along z-axis
*/

{
  MCW_cluster_array * clar;
  MCW_cluster * cl;
  int nxy;
  int iclu;
  int size, max_size;


  /*----- initialize local variables -----*/
  nxy = nx * ny;

  /*----- create array of clusters -----*/
  clar = MCW_find_clusters( nx,ny,nz , dx,dy,dz ,
			    MRI_float , fim , rmm ) ;

  /*----- record cluster sizes -----*/
  if ((clar == NULL) || (clar->num_clu == 0))
    {
      if (!quiet) printf ("NumCl=%4d  MaxClSz=%4d\n", 0, 0);
      if (clar != NULL)  DESTROY_CLARR(clar);
    }
  else
    {
      max_size = 0;
      for (iclu = 0;  iclu < clar->num_clu;  iclu++)
	{
	  cl = clar->clar[iclu] ;
	  if( cl == NULL ) continue ;

	  size = cl->num_pt;

	  if (size < g_max_cluster_size) freq_table[size]++;
	  else                           freq_table[g_max_cluster_size-1]++;

	  if (size > max_size) max_size = size;

	}

      if (max_size < g_max_cluster_size) max_table[max_size]++;
      else                               max_table[g_max_cluster_size-1]++;

      if (!quiet)
	printf ("NumCl=%4d  MaxClSz=%4d\n", clar->num_clu, max_size);

      DESTROY_CLARR(clar);
    }

}


/*---------------------------------------------------------------------------*/
/*
  Routine to generate requested output.
*/

void output_results (int nx, int ny, int nz, float dx, float dy, float dz,
		     int filter, float sigmax, float sigmay, float sigmaz,
		     int egfw, float avgsx, float avgsy, float avgsz,
		     int power, int ax, int ay, int az, float zsep,
		     float rmm, float pthr, int niter, char * outfilename,
		     long * freq_table,  long * max_table, int quiet)
{
  const float EPSILON = 1.0e-6;
  int i, j;
  float divisor;
  float * prob_table;
  float * alpha_table;
  float * cum_prop_table;
  long total_num_clusters;
  char message[MAX_NAME_LENGTH];     /* error message */
  FILE * fout=NULL;

  float afit=0.0f , bfit=0.0f , cfit=0.0f , cpow=-1.0f , ipow=1.0f , val ;  /* 10 Jun 2009 */
  int ibot , ihigh , ilow , itop , ndim ;

  /*----- allocate memory space for probability table -----*/
  prob_table = (float *) malloc( g_max_cluster_size * sizeof(float) );
  if (prob_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 1;  i < g_max_cluster_size;  i++)
    prob_table[i] = 0.0;

  /*----- allocate memory space for alpha table -----*/
  alpha_table = (float *) malloc( g_max_cluster_size * sizeof(float) );
  if (alpha_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 1;  i < g_max_cluster_size;  i++)
    alpha_table[i] = 0.0;

  /*----- allocate memory space for cum. prop. of cluster size table  -----*/
  cum_prop_table = (float *) malloc( g_max_cluster_size * sizeof(float) );
  if (cum_prop_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 1;  i < g_max_cluster_size;  i++)
    cum_prop_table[i] = 0.0;

  total_num_clusters = 0;
  for (i = 1;  i < g_max_cluster_size;  i++)
    total_num_clusters += freq_table[i];

  if (power)
    divisor = (float)(niter) * ax * ay * az;
  else
    if (mask_vol)
      divisor = (float)(niter) * mask_ngood;
    else
      divisor = (float)(niter) * nx * ny * nz;

  for (i = 1;  i < g_max_cluster_size;  i++)
    {
      prob_table[i] = i * freq_table[i] / divisor;
      alpha_table[i] = (float)max_table[i] / (float)niter;
      cum_prop_table[i] = (float)freq_table[i] / (float)total_num_clusters;
    }

  for (i = 1;  i < g_max_cluster_size-1;  i++)
    {
      j = g_max_cluster_size - i;
      prob_table[j-1] += prob_table[j];
      alpha_table[j-1] += alpha_table[j];
      cum_prop_table[i+1] += cum_prop_table[i];
    }


  /*----- if output file has not been specified, use stdout -----*/
  if (outfilename == NULL)
    fout = stdout;
  else
    {

      if (!THD_ok_overwrite()) {
            /*----- see if output file already exists -----*/
            fout = fopen (outfilename, "r");
            if (fout != NULL)
	      {
           fclose(fout) ;
	        sprintf (message, "file %s already exists. ", outfilename);
	        AlphaSim_error (message);
	      }
      }

      /*----- open file for output -----*/
      fout = fopen (outfilename, "w");
      if (fout == NULL)
	{
	  AlphaSim_error ("unable to open output file ");
	}
    }

  /*----- print out the results -----*/
  if(quiet<2)fprintf (fout, "\n\n");
  if(quiet<2)fprintf (fout, "# Program:          %s \n", PROGRAM_NAME);
  if(quiet<2)fprintf (fout, "# Author:           %s \n", PROGRAM_AUTHOR);
  if(quiet<2)fprintf (fout, "# Initial Release:  %s \n", PROGRAM_INITIAL);
  if(quiet<2)fprintf (fout, "# Latest Revision:  %s \n", PROGRAM_LATEST);
  if(quiet<2)fprintf (fout, "\n");

  if(quiet<2)fprintf (fout, "# Data set dimensions: \n");
  if(quiet<2)fprintf (fout, "# nx = %5d   ny = %5d   nz = %5d   (voxels)\n",  nx, ny, nz);
  if(quiet<2)fprintf (fout, "# dx = %5.2f   dy = %5.2f   dz = %5.2f   (mm)\n", dx, dy, dz);

  if (mask_vol)
    if(quiet<2)fprintf (fout, "\n# Mask filename = %s \n", mask_filename);
  if (mask_vol && !power)
    if(quiet<2)fprintf (fout, "# Voxels in mask = %5d \n", mask_ngood);

  if(quiet<2)fprintf (fout, "\n# Gaussian filter widths: \n");
  if(quiet<2)fprintf (fout, "# sigmax = %5.2f   FWHMx = %5.2f \n",
	   sigmax, sigmax * 2.0*sqrt(2.0*log(2.0)));
  if(quiet<2)fprintf (fout, "# sigmay = %5.2f   FWHMy = %5.2f \n",
	   sigmay, sigmay * 2.0*sqrt(2.0*log(2.0)));
  if(quiet<2)fprintf (fout, "# sigmaz = %5.2f   FWHMz = %5.2f \n\n",
	   sigmaz, sigmaz * 2.0*sqrt(2.0*log(2.0)));

  if (egfw)
    {
      if(quiet<2)fprintf (fout, "# Estimated Gaussian filter widths: \n");
      if(quiet<2)fprintf (fout, "# Ave sx = %f   Ave sy = %f   Ave sz = %f \n\n",
	       avgsx, avgsy, avgsz);
    }

  if (power)
    {
      if(quiet<2)fprintf (fout, "# Activation Region for Power Calculations: \n");
      if(quiet<2)fprintf (fout, "# ax = %5d   ay = %5d   az = %5d   (voxels) \n",
	       ax, ay, az);
      if(quiet<2)fprintf (fout, "# z separation = %f \n\n", zsep);
    }

  if(quiet<2){
    if( rmm > 0.0f )
      fprintf (fout, "# Cluster connection radius: rmm = %5.2f \n\n", rmm);
    else
      fprintf (fout, "# Cluster connection = Nearest Neighbor\n") ;
  }
  if(quiet<2)fprintf (fout, "# Threshold probability: pthr = %e \n\n", pthr);
  if(quiet<2)fprintf (fout, "# Number of Monte Carlo iterations = %5d \n\n", niter);

  /** estimate alpha[i] as an extreme value distribution [10 Jun 2009] **/

  if( !power ){
    int ii ;
    float *zvec=NULL , yvec[4] , *rvec[3] , *iva,*ivb,*ivc , *ivz ;
    float atop=0.98f, ahigh=0.3f, alow=0.03f, abot=4.0f/niter ;
    float cout , pp , cbest ;

    for( ibot=1 ;    /* find first value of alpha <= atop */
         ibot < g_max_cluster_size && alpha_table[ibot] > atop ;
         ibot++ ) ;  /*nada*/
    if( ibot == g_max_cluster_size ) goto EXTREME_DONE ;

    for( ihigh=ibot+1 ; /* find last value of alpha >= ahigh */
         ihigh < g_max_cluster_size && alpha_table[ihigh] > ahigh ;
         ihigh++ ) ; /*nada*/
    ihigh-- ;
    if( ihigh-ibot < 5 ) goto EXTREME_DONE ;

    for( ilow=ihigh+1 ; /* find last value of alpha >= alow */
         ilow < g_max_cluster_size && alpha_table[ilow] > alow ;
         ilow++ ) ; /*nada*/
    ilow-- ;
    if( ilow-ihigh < 5 ) goto EXTREME_DONE ;

    for( itop=ilow+1 ; /* find last value of alpha >= abot */
         itop < g_max_cluster_size && alpha_table[itop] > abot ;
         itop++ ) ;    /*nada*/
    itop-- ;
    if( itop-ilow < 5 ) goto EXTREME_DONE ;
    ndim = itop-ibot+1 ;

    iva     = (float *)malloc(sizeof(float)*ndim) ;
    ivb     = (float *)malloc(sizeof(float)*ndim) ;
    ivc     = (float *)malloc(sizeof(float)*ndim) ;
    rvec[0] = (float *)malloc(sizeof(float)*ndim) ;
    rvec[2] = (float *)malloc(sizeof(float)*ndim) ;
    zvec    = (float *)malloc(sizeof(float)*ndim) ;
    for( i=0 ; i < ndim ; i++ ){
      ii         = i+ibot ;
      rvec[0][i] = 1.0f ;
      val        = (float)ii ;
      iva[i]     = -powf(val,0.90f) ;
      ivb[i]     = -powf(val,0.95f) ;
      ivc[i]     = -val ;
      zvec[i]    = logf( -logf( 1.0f - alpha_table[ii] ) ) ;
    }

    cbest = 1.e+38 ; cpow = -1.0f ;

    yvec[0] = 0.0f ; yvec[1] = 1.0f ; rvec[1] = iva ;
    cout = cl1_solve( ndim , 2 , zvec , rvec , yvec , 1 ) ;
INFO_message("ipow=0.90 cout=%g",cout) ;
    if( cout >= 0.0f && cout < cbest ){
      cbest = cout ; cpow = 1.0f ; afit = yvec[0] ; bfit = yvec[1] ; cfit = 0.0f ; ipow = 0.90f ;
      ivz = iva ;
    }

    yvec[0] = 0.0f ; yvec[1] = 1.0f ; rvec[1] = ivb ;
    cout = cl1_solve( ndim , 2 , zvec , rvec , yvec , 1 ) ;
INFO_message("ipow=0.95 cout=%g",cout) ;
    if( cout >= 0.0f && cout < cbest ){
      cbest = cout ; cpow = 1.0f ; afit = yvec[0] ; bfit = yvec[1] ; cfit = 0.0f ; ipow = 0.95f ;
      ivz = ivb ;
    }

    yvec[0] = 0.0f ; yvec[1] = 1.0f ; rvec[1] = ivc ;
    cout = cl1_solve( ndim , 2 , zvec , rvec , yvec , 1 ) ;
INFO_message("ipow=1.00 cout=%g",cout) ;
    if( cout >= 0.0f && cout < cbest ){
      cbest = cout ; cpow = 1.0f ; afit = yvec[0] ; bfit = yvec[1] ; cfit = 0.0f ; ipow = 1.00f ;
      ivz = ivc ;
    }
    if( cpow > 0.0f ){ cbest *= 0.90f ; rvec[1] = ivz ; }
    else             {                  rvec[1] = ivc ; }

    for( pp=1.0f ; pp <= 2.501f ; pp+=0.1f ){
      for( i=0 ; i < ndim ; i++ ){
        ii  = i+ibot ;
        val = (float)(ihigh-ii) ;
        if( val <= 0.0f ) val = 0.0f ;
        else              val = powf(val,pp) ;
        rvec[2][i] = val ;
      }
      yvec[0] = 0.0f ; yvec[1] = 1.0f ; yvec[2] = 0.0f ;
      cout = cl1_solve( ndim , 3 , zvec , rvec , yvec , 1 ) ;
      if( cout >= 0.0f && cout < cbest ){
        cbest = cout ; cpow = pp ; afit = yvec[0] ; bfit = yvec[1] ; cfit = yvec[2] ;
      }
    }

    if( cfit != 0.0f )   /* complicated fit was best */
      printf(
         "# Alpha(i) approx 1-exp[-exp"
         "(%.3f-%.4f*i^%.1f%+.4g*posval(%d-i)^%.1f)]\n" ,
         afit , bfit , ipow , cfit , ihigh , cpow ) ;
    else                 /* simple fit was best */
      printf(
         "# Alpha(i) approx 1-exp[-exp(%.3f-%.4f*i)]\n" ,
         afit , bfit ) ;

    EXTREME_DONE: ;
    if( zvec != NULL ){
      free(zvec); free(rvec[0]); free(rvec[2]); free(iva); free(ivb); free(ivc);
    }
  }

  /** print the tabular output **/

  if( quiet < 2 ){
    if (!power){
      fprintf (fout, "# Cl Size   Frequency    CumuProp     p/Voxel"
	                  "   Max Freq       Alpha");
      if( bfit > 0.0f && cpow > 0.0f ) fprintf(fout , "    Approx") ;
      fprintf(fout,"\n") ;
    }
    else {
      fprintf (fout, "# Cl Size   Frequency    CumuProp     p/Voxel"
	                  "   Max Freq       Power\n");
    }
  }

  for (i = 1;  i < g_max_cluster_size;  i++) {
    if (alpha_table[i] < EPSILON)
      break;
    else {
      fprintf (fout, "%7d  %12ld  %10.6f  %10.8f    %7ld  %10.6f",
	       i, freq_table[i], cum_prop_table[i], prob_table[i],
	       max_table[i], alpha_table[i]);
      if( bfit > 0.0f && cpow > 0.0f ){
        val = (float)(ihigh-i) ;
        if( val < 0.0f ) val = 0.0f ;
        else             val = powf(val,cpow) ;
        val = afit - bfit*powf((float)i,ipow) + cfit * val ;
        val = 1.0f - expf( -expf(val) ) ;
        fprintf(fout,"%10.6f",val) ;
      }
      fprintf(fout,"\n") ;
    }
  }

  if( fout != stdout ) fclose(fout);

  return ;
}


/*---------------------------------------------------------------------------*/
/*
  Routine to terminate program.
*/

void terminate (float ** fim,  float ** arfim,
		long ** freq_table,  long ** max_table)
{
#if 0
  if (*fim != NULL)
    { free (*fim);  *fim = NULL; }
#endif

#if 0
  if (*arfim != NULL)
    { free (*arfim);  *arfim = NULL; }
#endif

  if (*freq_table != NULL)
    { free (*freq_table);  *freq_table = NULL; }

  if (*max_table != NULL)
    { free (*max_table);  *max_table = NULL; }
}


/*---------------------------------------------------------------------------*/
/*
  Alpha simulation.
*/

int main (int argc, char ** argv)
{
  int nx;                  /* number of voxels along x-axis */
  int ny;                  /* number of voxels along y-axis */
  int nz;                  /* number of voxels along z-axis */
  float dx;                /* voxel size along x-axis */
  float dy;                /* voxel size along y-axis */
  float dz;                /* voxel size along z-axis */
  int filter;              /* flag for Gaussian filtering */
  float sigmax;            /* Gaussian filter width, x-axis (1 sigma) */
  float sigmay;            /* Gaussian filter width, y-axis (1 sigma) */
  float sigmaz;            /* Gaussian filter width, z-axis (1 sigma) */
  int egfw;                /* flag for estimation of filter width */
  float avgsx;             /* est. Gaussian filter width, x-axis (1 sigma) */
  float avgsy;             /* est. Gaussian filter width, x-axis (1 sigma) */
  float avgsz;             /* est. Gaussian filter width, x-axis (1 sigma) */
  int power;               /* flag for perform power calculations */
  int ax;                  /* number of activation voxels along x-axis */
  int ay;                  /* number of activation voxels along y-axis */
  int az;                  /* number of activation voxels along z-axis */
  float zsep;              /* z-score separation between signal and noise */
  float rmm;               /* cluster connection radius (mm) */
  float pthr;              /* individual voxel threshold probability */
  int niter;               /* number of Monte Carlo simulations */
  int quiet;               /* set to 1 to suppress screen output */
  char *outfilename;      /* name of output file */

  long count;
  double sum, sumsq;
  float power_thr;

  float *fim = NULL;          /* won't be used in OpenMP version */
  float *arfim = NULL;        /* won't be used in OpenMP version */
  long  *freq_table = NULL;
  long  *max_table = NULL;
#ifdef USE_OMP
  long **mtab=NULL , **ftab=NULL ; int nthr=1 ;
#endif


  /*----- Identify software -----*/
#if 0
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR);
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");
#endif

   PRINT_VERSION("AlphaSim") ; AUTHOR(PROGRAM_AUTHOR) ;
   mainENTRY("AlphaSim main") ; machdep() ;

  /*----- program initialization -----*/
  initialize (argc, argv,
	      &nx, &ny, &nz, &dx, &dy, &dz, &filter, &sigmax, &sigmay, &sigmaz,
	      &egfw, &avgsx, &avgsy, &avgsz, &power, &ax, &ay, &az, &zsep,
	      &rmm, &pthr, &niter, &quiet, &outfilename, &count, &sum, &sumsq,
	      &power_thr, &fim, &arfim, &freq_table, &max_table);


#pragma omp parallel if( niter > 666 )
 {
   int iter , qqq=quiet ; float *fim , *arfim=NULL ;
   long count=0; double sum=0.0, sumsq=0.0 ;
   long *mt , *ft ; int ithr=0 ; unsigned short xran[3] ;

 AFNI_OMP_START ;

#ifdef USE_OMP
   ithr = omp_get_thread_num() ;
   if( ithr > 0 ) qqq=1 ;  /* only master thread can be verbose */
#pragma omp master
 {
   nthr = omp_get_num_threads() ;
   mtab = (long **)malloc(sizeof(long *)*nthr) ;
   ftab = (long **)malloc(sizeof(long *)*nthr) ;
   INFO_message("Using %d OpenMP threads",nthr) ;
 }
#pragma omp barrier
   mtab[ithr] = mt = (long *) calloc( g_max_cluster_size , sizeof(long) ) ;
   ftab[ithr] = ft = (long *) calloc( g_max_cluster_size , sizeof(long) ) ;

   xran[2] = ( gseed        & 0xffff) + (unsigned short)ithr ;
   xran[1] = ((gseed >> 16) & 0xffff) - (unsigned short)ithr ;
   xran[0] = 0x330e                   + (unsigned short)ithr ;

#else /* not OpenMP */
   mt = max_table ;
   ft = freq_table ;
#endif

   /** malloc of image space local to each thread [09 Jun 2009] **/

   fim = (float *)malloc(sizeof(float)*nx*ny*nz) ;
   if( power )
     arfim = (float *)malloc(sizeof(float)*ax*ay*az) ;

  /*----- Monte Carlo iteration -----*/
#pragma omp for
  for (iter = 1;  iter <= niter;  iter++)
    {
      if (!qqq)  printf ("Iter =%5d  \n", iter);

      /*----- generate volume of random voxel intensities -----*/
      generate_image (nx, ny, nz, power, ax, ay, az, zsep, fim , xran );


      /*----- apply gaussian filter to volume data -----*/
      if (filter)  gaussian_filter (nx, ny, nz, dx, dy, dz, rmm,
				    sigmax, sigmay, sigmaz, fim);


      /*----- estimate equivalent gaussian filter width -----*/
      if (egfw)  estimate_gfw (nx, ny, nz, dx, dy, dz,
			       niter, qqq, fim, &avgsx, &avgsy, &avgsz);


      /*----- if power calculation, get volume corresponding to   -----*/
      /*----- activation region and corresponding power threshold -----*/
      if (power)  get_activation_region (nx, ny, nz, ax, ay, az, pthr, zsep,
					 fim, arfim);


      /*----- apply threshold to volume data -----*/
      if (power)  threshold_data (ax, ay, az,
                    arfim, power_thr, &count, &sum, &sumsq,
                    qqq, iter);
      else
                  threshold_data (nx, ny, nz,
                    fim, pthr, &count, &sum, &sumsq,
                    qqq, iter);


      /*----- apply mask to volume data -----*/
      if (mask_vol && (!power))  apply_mask (nx, ny, nz, fim);


      /*----- identify clusters -----*/
      if (power)
        identify_clusters (ax, ay, az, dx, dy, dz, rmm, arfim, qqq,
                           ft, mt);
      else
        identify_clusters (nx, ny, nz, dx, dy, dz, rmm, fim, qqq,
                           ft, mt);

    } /* end of long iteration loop */

    if( arfim != NULL ) free(arfim) ;  /* toss the local trash */
    free(fim) ;

 AFNI_OMP_END ;
 } /* end OpenMP */

#ifdef USE_OMP
   if( nthr == 1 ){
     memcpy(freq_table,ftab[0],sizeof(long)*g_max_cluster_size) ;
     memcpy(max_table ,mtab[0],sizeof(long)*g_max_cluster_size) ;
   } else {
     int ithr , ii ; long *ft , *mt ;
     for( ithr=0 ; ithr < nthr ; ithr++ ){
       ft = ftab[ithr] ; mt = mtab[ithr] ;
       for( ii=0 ; ii < g_max_cluster_size ; ii++ ){
         freq_table[ii] += ft[ii] ; max_table[ii] += mt[ii] ;
       }
     }
   }
#endif

  /*----- generate requested output -----*/
  output_results (nx, ny, nz, dx, dy, dz, filter, sigmax, sigmay, sigmaz,
		  egfw, avgsx, avgsy, avgsz, power, ax, ay, az, zsep,
		  rmm, pthr, niter, outfilename, freq_table, max_table,quiet);


  /*----- terminate program -----*/
  terminate (&fim, &arfim, &freq_table, &max_table);

  exit(0);
}
