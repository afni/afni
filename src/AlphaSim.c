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

*/


/*---------------------------------------------------------------------------*/

#define PROGRAM_NAME "AlphaSim"                      /* name of this program */
#define PROGRAM_AUTHOR "B. Douglas Ward"                   /* program author */
#define PROGRAM_INITIAL "18 June 1997"    /* date of initial program release */
#define PROGRAM_LATEST  "15 August 2001"  /* date of latest program revision */

/*---------------------------------------------------------------------------*/

#define MAX_NAME_LENGTH 80            /* max. strength length for file names */
#define MAX_CLUSTER_SIZE 1000        /* max. size of cluster for freq. table */

#include "mrilib.h"


/*---------------------------------------------------------------------------*/
/*
  Global data 
*/

static char * mask_filename = NULL;  /* file containing the mask */
static byte * mask_vol  = NULL;      /* mask volume */
static int mask_ngood = 0;           /* number of good voxels in mask volume */


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
     "[-mask mset]      Use the 0 sub-brick of dataset 'mset' as a mask     \n"
     "                    to indicate which voxels to analyze (a sub-brick  \n"
     "                    selector is allowed)  [default = use all voxels]  \n"
     "                  Note:  The -mask command REPLACES the -nx, -ny, -nz,\n"
     "                         -dx, -dy, and -dz commands.                  \n"
     "[-fwhm s]     s  = Gaussian filter width (FWHM)                       \n"
     "[-fwhmx sx]   sx = Gaussian filter width, x-axis (FWHM)               \n"
     "[-fwhmy sy]   sy = Gaussian filter width, y-axis (FWHM)               \n"
     "[-fwhmz sz]   sz = Gaussian filter width, z-axis (FWHM)               \n"
     "[-sigma s]    s  = Gaussian filter width (1 sigma)                    \n"
     "[-sigmax sx]  sx = Gaussian filter width, x-axis (1 sigma)            \n"
     "[-sigmay sy]  sy = Gaussian filter width, y-axis (1 sigma)            \n"
     "[-sigmaz sz]  sz = Gaussian filter width, z-axis (1 sigma)            \n"
     "[-power]      perform statistical power calculations                  \n"
     "[-ax n1]      n1 = extent of active region (in voxels) along x-axis   \n"
     "[-ay n2]      n2 = extent of active region (in voxels) along y-axis   \n"
     "[-az n3]      n3 = extent of active region (in voxels) along z-axis   \n"
     "[-zsep z]     z = z-score separation between signal and noise         \n"
     "-rmm r        r  = cluster connection radius (mm)                     \n"
     "-pthr p       p  = individual voxel threshold probability             \n"
     "-iter n       n  = number of Monte Carlo simulations                  \n"
     "[-quiet]     suppress screen output                                   \n"
     "[-out file]  file = name of output file                               \n"
     );
  
  exit(0);
}

/*---------------------------------------------------------------------------*/
/*
   Routine to print error message and stop.
*/

void AlphaSim_error (char * message)
{
   fprintf (stderr, "%s Error: %s \n", PROGRAM_NAME, message);
   exit(1);
}


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
	         char ** outfilename)
 
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
  *rmm = 0.0;                /* cluster connection radius (mm) */
  *pthr = 0.0;               /* individual voxel threshold prob. */
  *niter = 0;                /* number of Monte Carlo simulations  */
  *quiet = 0;                /* generate screen output (default)  */
  *outfilename = NULL;       /* name of output file */
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
		  char ** outfilename)
{
  int nopt = 1;                  /* input option argument counter */
  int ival;                      /* integer input */
  float fval;                    /* float input */
  char message[MAX_NAME_LENGTH];         /* error message */
  int mask_nx, mask_ny, mask_nz, mask_nvox;   /* mask dimensions */
  float  mask_dx, mask_dy, mask_dz;            

  
  /*----- does user request help menu? -----*/
  if (argc < 2 || strncmp(argv[1], "-help", 5) == 0)  display_help_menu();  
  
  
  /*----- add to program log -----*/
  AFNI_logger (PROGRAM_NAME,argc,argv); 


  /*----- initialize the input options -----*/
  initialize_options (nx, ny, nz, dx, dy, dz, filter, sigmax, sigmay, sigmaz,
		      egfw, power, ax, ay, az, zsep, rmm, pthr, niter, quiet,
		      outfilename); 

  /*----- main loop over input options -----*/
  while (nopt < argc )
    {
      
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
	  printf("--- %d voxels in mask\n",mc) ;
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
	  if (fval <= 0.0)
	    AlphaSim_error ("illegal argument after -rmm ");
	  *rmm = fval;
	  nopt++;
	  continue;
	}


      /*-----   -pthr p   -----*/
      if (strncmp(argv[nopt], "-pthr", 5) == 0)
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
      if (strncmp(argv[nopt], "-iter", 5) == 0)
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


      /*-----   -quiet q  -----*/
      if (strncmp(argv[nopt], "-quiet", 6) == 0)
	{
	  *quiet = 1;
	  nopt++;
	  continue;
	}


      /*-----   -out filename   -----*/
      if (strncmp(argv[nopt], "-out", 4) == 0)
	{
	  nopt++;
	  if (nopt >= argc)  AlphaSim_error ("need argument after -out ");
	  *outfilename = malloc (sizeof(char) * MAX_NAME_LENGTH);
	  strcpy (*outfilename, argv[nopt]);
	  nopt++;
	  continue;
	}
      

      /*----- unknown command -----*/
      AlphaSim_error ("unrecognized command line option ");
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
  if ( (rmm < dx) && (rmm < dy) && (rmm < dz) )
    AlphaSim_error ("Cluster connection radius is too small ");
  if ((pthr <= 0.0) || (pthr > 1.0))  
    AlphaSim_error ("Illegal value for pthr ");
  if (niter <= 0)  AlphaSim_error ("Illegal value for niter ");

  if (outfilename != NULL)
    {
      /*----- see if output file already exists -----*/
      fout = fopen (outfilename, "r");
      if (fout != NULL)
	{
	  sprintf (message, "Output file %s already exists. ", outfilename); 
	  AlphaSim_error (message);
	}
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
  int status;
  double bound;


  /*----- get command line inputs -----*/
  get_options(argc, argv, 
	      nx, ny, nz, dx, dy, dz, filter, sigmax, sigmay, sigmaz,
	      egfw, power, ax, ay, az, zsep, rmm, pthr, niter, quiet, 
	      outfilename);


  /*----- check for valid inputs -----*/
  check_for_valid_inputs (*nx,  *ny,  *nz,  *dx,  *dy,  *dz,  *filter,
			  *sigmax,  *sigmay,  *sigmaz,
			  *power, *ax,  *ay,  *az,  *zsep, 
			  *rmm,  *pthr,  *niter,  *outfilename);


  /*----- allocate memory space for image data -----*/
  nxyz = (*nx) * (*ny) * (*nz); 
  *fim = (float *) malloc(nxyz * sizeof(float));
  if (*fim == NULL)
    AlphaSim_error ("memory allocation error");


  /*-- if power calculation, allocate memory space for activation region --*/
  if (*power)
    {
      nxyz = (*ax) * (*ay) * (*az);
      *arfim = (float *) malloc(nxyz * sizeof(float));
      if (*arfim == NULL)
	AlphaSim_error ("memory allocation error");
    }

  
  /*----- allocate memory space and initialize frequency table -----*/   
  *freq_table = (long *) malloc( MAX_CLUSTER_SIZE * sizeof(long) );
  if (*freq_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 0;  i < MAX_CLUSTER_SIZE;  i++)
    (*freq_table)[i] = 0;

  
  /*----- allocate memory space and initialize max cluster size table -----*/  
  *max_table = (long *) malloc( MAX_CLUSTER_SIZE * sizeof(long) );
  if (*max_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 0;  i < MAX_CLUSTER_SIZE;  i++)
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
  srand48 (1234567);

}


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
		     int ax, int ay, int az, float zsep, float * fim)
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
  for (ixyz = 0;  ixyz < nxyzdiv2;  ixyz++)
    {
      normal(&n1, &n2);
      fim[ixyz] = n1;
      fim[ixyz+nxyzdiv2] = n2;
    }
  normal(&n1, &n2);
  fim[nxyz-1] = n1;

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
  *avgsx += sx / niter;
  *avgsy += sy / niter;
  *avgsz += sz / niter;

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
  const float EPSILON = 1.0e-8;
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
  int nxyz;                     /* total number of voxels */
  int iclu, ipt;
  int size, max_size;
  int count;


  /*----- initialize local variables -----*/
  nxy = nx * ny;

  /*----- create array of clusters -----*/
  clar = MCW_find_clusters( nx,ny,nz , dx,dy,dz ,
			    MRI_float , fim , rmm ) ;

  /*----- record cluster sizes -----*/
  if ((clar == NULL) || (clar->num_clu == 0))
    {
      if (!quiet)  
	printf ("NumCl=%4d  MaxClSz=%4d\n", 0, 0);
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

	  if (size < MAX_CLUSTER_SIZE)
	    freq_table[size]++;
	  else
	    freq_table[MAX_CLUSTER_SIZE-1]++;

	  if (size > max_size)
	    max_size = size;

	}

      if (max_size < MAX_CLUSTER_SIZE)
	max_table[max_size]++;
      else
	max_table[MAX_CLUSTER_SIZE-1]++;
      
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
		     long * freq_table,  long * max_table)
{
  const float EPSILON = 1.0e-6;
  int i, j;
  float divisor;
  float * prob_table;
  float * alpha_table;
  float * cum_prop_table;
  long total_num_clusters;
  char message[MAX_NAME_LENGTH];     /* error message */
  FILE * fout;

  
  /*----- allocate memory space for probability table -----*/   
  prob_table = (float *) malloc( MAX_CLUSTER_SIZE * sizeof(float) );
  if (prob_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 1;  i < MAX_CLUSTER_SIZE;  i++)
    prob_table[i] = 0.0;
  
  /*----- allocate memory space for alpha table -----*/   
  alpha_table = (float *) malloc( MAX_CLUSTER_SIZE * sizeof(float) );
  if (alpha_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 1;  i < MAX_CLUSTER_SIZE;  i++)
    alpha_table[i] = 0.0;

  /*----- allocate memory space for cum. prop. of cluster size table  -----*/ 
  cum_prop_table = (float *) malloc( MAX_CLUSTER_SIZE * sizeof(float) );
  if (cum_prop_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 1;  i < MAX_CLUSTER_SIZE;  i++)
    cum_prop_table[i] = 0.0;

  total_num_clusters = 0;
  for (i = 1;  i < MAX_CLUSTER_SIZE;  i++)
    total_num_clusters += freq_table[i];

  if (power)
    divisor = (float)(niter) * ax * ay * az;
  else
    if (mask_vol)
      divisor = (float)(niter) * mask_ngood;
    else
      divisor = (float)(niter) * nx * ny * nz;

  for (i = 1;  i < MAX_CLUSTER_SIZE;  i++)
    {
      prob_table[i] = i * freq_table[i] / divisor;
      alpha_table[i] = (float)max_table[i] / (float)niter;
      cum_prop_table[i] = (float)freq_table[i] / (float)total_num_clusters;
    }

  for (i = 1;  i < MAX_CLUSTER_SIZE-1;  i++)
    {
      j = MAX_CLUSTER_SIZE - i;
      prob_table[j-1] += prob_table[j];
      alpha_table[j-1] += alpha_table[j];
      cum_prop_table[i+1] += cum_prop_table[i];
    }


  /*----- if output file has not been specified, use stdout -----*/
  if (outfilename == NULL)
    fout = stdout;
  else
    {
      /*----- see if output file already exists -----*/
      fout = fopen (outfilename, "r");
      if (fout != NULL)
	{
	  sprintf (message, "file %s already exists. ", outfilename); 
	  AlphaSim_error (message);
	}
      
      /*----- open file for output -----*/
      fout = fopen (outfilename, "w");
      if (fout == NULL)
	{ 
	  AlphaSim_error ("unable to write file ");
	}
    }

  /*----- print out the results -----*/
  fprintf (fout, "\n\n");
  fprintf (fout, "Program:          %s \n", PROGRAM_NAME);
  fprintf (fout, "Author:           %s \n", PROGRAM_AUTHOR); 
  fprintf (fout, "Initial Release:  %s \n", PROGRAM_INITIAL);
  fprintf (fout, "Latest Revision:  %s \n", PROGRAM_LATEST);
  fprintf (fout, "\n");

  fprintf (fout, "Data set dimensions: \n");
  fprintf (fout, "nx = %5d   ny = %5d   nz = %5d   (voxels)\n",  nx, ny, nz);
  fprintf (fout, "dx = %5.2f   dy = %5.2f   dz = %5.2f   (mm)\n", dx, dy, dz);

  if (mask_vol)
    fprintf (fout, "\nMask filename = %s \n", mask_filename);
  if (mask_vol && !power)
    fprintf (fout, "Voxels in mask = %5d \n", mask_ngood);

  fprintf (fout, "\nGaussian filter widths: \n");
  fprintf (fout, "sigmax = %5.2f   FWHMx = %5.2f \n", 
	   sigmax, sigmax * 2.0*sqrt(2.0*log(2.0)));
  fprintf (fout, "sigmay = %5.2f   FWHMy = %5.2f \n", 
	   sigmay, sigmay * 2.0*sqrt(2.0*log(2.0)));
  fprintf (fout, "sigmaz = %5.2f   FWHMz = %5.2f \n\n", 
	   sigmaz, sigmaz * 2.0*sqrt(2.0*log(2.0)));

  if (egfw)
    {
      fprintf (fout, "Estimated Gaussian filter widths: \n");
      fprintf (fout, "Ave sx = %f   Ave sy = %f   Ave sz = %f \n\n", 
	       avgsx, avgsy, avgsz);
    }

  if (power)
    {
      fprintf (fout, "Activation Region for Power Calculations: \n");
      fprintf (fout, "ax = %5d   ay = %5d   az = %5d   (voxels) \n", 
	       ax, ay, az);
      fprintf (fout, "z separation = %f \n\n", zsep);
    }

  fprintf (fout, "Cluster connection radius: rmm = %5.2f \n\n", rmm);
  fprintf (fout, "Threshold probability: pthr = %e \n\n", pthr);
  fprintf (fout, "Number of Monte Carlo iterations = %5d \n\n", niter);
  if (!power)
    fprintf (fout, "Cl Size     Frequency    Cum Prop     p/Voxel"
	     "   Max Freq       Alpha\n");
  else
    fprintf (fout, "Cl Size     Frequency    Cum Prop     p/Voxel"
	     "   Max Freq       Power\n");    
  for (i = 1;  i < MAX_CLUSTER_SIZE;  i++)
    if (alpha_table[i] < EPSILON)
      break;
    else
      fprintf (fout, "%7d  %12ld  %10.6f  %10.8f    %7ld  %10.6f\n", 
	       i, freq_table[i], cum_prop_table[i], prob_table[i], 
	       max_table[i], alpha_table[i]);

  fclose(fout);

}
 
 
/*---------------------------------------------------------------------------*/
/*
  Routine to terminate program.
*/
  
void terminate (float ** fim,  float ** arfim,
		long ** freq_table,  long ** max_table)
{
  if (*fim != NULL)
    { free (*fim);  *fim = NULL; }

  if (*arfim != NULL)
    { free (*arfim);  *arfim = NULL; }

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
  char * outfilename;      /* name of output file */

  long count;
  double sum, sumsq;
  int iter;
  float power_thr;

  float * fim = NULL;
  float * arfim = NULL;
  long * freq_table = NULL;
  long * max_table = NULL;

  
  /*----- Identify software -----*/
  printf ("\n\n");
  printf ("Program:          %s \n", PROGRAM_NAME);
  printf ("Author:           %s \n", PROGRAM_AUTHOR); 
  printf ("Initial Release:  %s \n", PROGRAM_INITIAL);
  printf ("Latest Revision:  %s \n", PROGRAM_LATEST);
  printf ("\n");


  /*----- program initialization -----*/
  initialize (argc, argv, 
	      &nx, &ny, &nz, &dx, &dy, &dz, &filter, &sigmax, &sigmay, &sigmaz,
	      &egfw, &avgsx, &avgsy, &avgsz, &power, &ax, &ay, &az, &zsep, 
	      &rmm, &pthr, &niter, &quiet, &outfilename, &count, &sum, &sumsq, 
	      &power_thr, &fim, &arfim, &freq_table, &max_table);


  /*----- Monte Carlo iteration -----*/
  for (iter = 1;  iter <= niter;  iter++)
    {
      if (!quiet)  printf ("Iter =%5d  \n", iter);

      /*----- generate volume of random voxel intensities -----*/
      generate_image (nx, ny, nz, power, ax, ay, az, zsep, fim);

      
      /*----- apply gaussian filter to volume data -----*/
      if (filter)  gaussian_filter (nx, ny, nz, dx, dy, dz, rmm,
				    sigmax, sigmay, sigmaz, fim);


      /*----- estimate equivalent gaussian filter width -----*/
      if (egfw)  estimate_gfw (nx, ny, nz, dx, dy, dz, 
			       niter, quiet, fim, &avgsx, &avgsy, &avgsz);


      /*----- if power calculation, get volume corresponding to   -----*/
      /*----- activation region and corresponding power threshold -----*/
      if (power)  get_activation_region (nx, ny, nz, ax, ay, az, pthr, zsep, 
					 fim, arfim);


      /*----- apply threshold to volume data -----*/
      if (power)  threshold_data (ax, ay, az,
				  arfim, power_thr, &count, &sum, &sumsq, 
				  quiet, iter);
      else
	threshold_data (nx, ny, nz, 
			fim, pthr, &count, &sum, &sumsq, 
			quiet, iter);	


      /*----- apply mask to volume data -----*/
      if (mask_vol && (!power))  apply_mask (nx, ny, nz, fim);


      /*----- identify clusters -----*/
      if (power)
	identify_clusters (ax, ay, az, dx, dy, dz, rmm, arfim, quiet,
			   freq_table, max_table);
      else
	identify_clusters (nx, ny, nz, dx, dy, dz, rmm, fim, quiet,
			   freq_table, max_table);

    }
  

  /*----- generate requested output -----*/
  output_results (nx, ny, nz, dx, dy, dz, filter, sigmax, sigmay, sigmaz,
		  egfw, avgsx, avgsy, avgsz, power, ax, ay, az, zsep, 
		  rmm, pthr, niter, outfilename, freq_table, max_table);


  /*----- terminate program -----*/
  terminate (&fim, &arfim, &freq_table, &max_table);

  exit(0);
}





