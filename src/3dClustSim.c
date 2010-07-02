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
#define MAX_CLUSTER_SIZE 16384        /* max. size of cluster for freq. table */

/*---------------------------------------------------------------------------*/
/*
  Global data
*/

static THD_3dim_dataset  *mask_dset  = NULL ; /* mask dataset */
static byte              *mask_vol   = NULL;  /* mask volume */
static int mask_nvox = 0, mask_ngood = 0;     /* number of good voxels in mask volume */

static int max_cluster_size = MAX_CLUSTER_SIZE;

static int   nx     = 64 ;
static int   ny     = 64 ;
static int   nz     = 32 ;
static float dx     = 3.5f ;
static float dy     = 3.5f ;
static float dz     = 3.5f ;
static float fwhm_x = 0.0f ;
static float fwhm_y = 0.0f ;
static float fwhm_z = 0.0f ;
static int   niter  = 71802 ;  /* Bob Cox's US Zip code, reversed */

static float sigmax , sigmay , sigmaz ;
static int do_blur = 0 ;

static unsigned int gseed = 123456789 ;

static int   npthr = 5 ;
static float *pthr = { 0.01f, 0.005f, 0.001f, 0.0005f, 0.00001f } ;

static int   nathr = 4 ;
static float *athr = { 0.10f , 0.05f , 0.02f , 0.01f } ;

/*----------------------------------------------------------------------------*/
/*! Function to replace use of cdfnor(), which is not thread-safe (not used). */

double zthresh( double mn , double sd , double pval )
{
   double z ;

        if( pval <= 0.0 ) pval = 1.e-15 ;
   else if( pval >= 1.0 ) pval = 1.0 - 1.e-15 ;
   z = qginv(pval) ;

   if( sd > 0.0 ) z = sd*z + mn ;
   return z ;
}

/*---------------------------------------------------------------------------*/

void display_help_menu()
{
  printf(
   "Usage: 3dClustSim [options]\n"
   "\n"
   "Program to estimate the probability of false cluster positives.\n"
   "An adaptation of AlphaSim, streamlined for various purposes.\n"
   "\n"
   "Options:  (at least 1 option is required, or you'll get this help message!)\n"
   "--------\n"
   "-nxyz n1 n2 n3   = Size of 3D grid to use for simulation\n"
   "                    [default values = 64 64 32]\n"
   "-dxyz d1 d2 d3   = give all 3 voxel sizes at once\n"
   "                    [default values = 3.5 3.5 3.5]\n"
   "  ** OR **\n"
   "-mask mset       = Use the 0 sub-brick of dataset 'mset' as a mask\n"
   "                    to indicate which voxels to analyze (a sub-brick\n"
   "                    selector '[]' is allowed) \n"
   "\n"
   "-fwhm s          = Gaussian filter width (all 3 dimensions)\n"
   "                    [default = 0.0 = no smoothing]\n"
   "                    If you wish to set different smoothing amounts for\n"
   "                    each axis, you can instead use option\n"
   "                      -fwhmxyz sx sy sz\n"
   "                    to specify the three values separately.\n"
   "\n"
   "-pthr p1 .. pn   = list of uncorrected (per voxel) p-values at which to\n"
   "                    threshold the simulated images prior to clustering.\n"
   "                    [default = 0.01 0.005 0.001 0.0005 0.00001]\n"
   "\n"
   "-athr a1 .. an   = list of corrected (whole volume) alpha-values at which\n"
   "                    the simulation will print out the cluster size\n"
   "                    thresholds.  For each 'p' and 'a', the smallest cluster\n"
   "                    size C(p,a) for which the probability of the 'p'-thresholded\n"
   "                    image having a cluster of size C is less than 'a' is output.\n"
   "                    [default = 0.10 0.05 0.02 0.01]\n"
   "         ** Both lists '-pthr' and '-athr' of values between 0 and 1 should **\n"
   "         ** be given in DESCENDING order.  They will be sorted to be that   **\n"
   "         ** way in any case, and such is how the output will be displayed.  **\n"
   "\n"
   "-iter n          = number of Monte Carlo simulations [default = 71802]\n"
   "\n"
   "-seed S          = random number seed [default seed = 123456789]\n"
   "                    if seed=0, then program will randomize it\n"
   "\n"
   "Output goes to stdout (e.g., the terminal window).  Use Unix redirection\n"
   "to capture the results to a file, to save them for historical arvhives.\n"
   "\n"
   "-- RW Cox -- July 2010\n"
  ) ;

  PRINT_AFNI_OMP_USAGE("3dClustSim",NULL) ;
  exit(0);
}

/*---------------------------------------------------------------------------*/
/* Routine to initialize the input options (values are in global variables). */

void get_options( int argc , char **argv )
{
  int nopt = 1;                  /* input option argument counter */
  int ival;                      /* integer input */
  float fval;                    /* float input */


  /*----- does user request help menu? -----*/

  if( argc < 2 || strcmp(argv[1],"-help") == 0 ) display_help_menu() ;

  /*----- add to program log -----*/

  AFNI_logger("3dClustSim",argc,argv) ;

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
      if( mask_ngood < 9 ) ERROR_exit("-mask has only %d nonzero voxels!",mask_ngood) ;
      INFO_message("%d voxels in mask",mask_ngood) ;
      nopt++ ; continue ;
    }

    /*-----   -fwhm s   -----*/

    if( strcasecmp(argv[nopt],"-fwhm") == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after -fwhm");
      fval = (float)strtod(argv[nopt],NULL) ;
      if( fval < 0.0f ) ERROR_exit("illegal value after -fwhm") ;
      fwhm_x = fwhm_y = fwhm_z = fval ; nopt++; continue;
    }

    /*-----   -fwhmx s   -----*/

    if( strcasecmp(argv[nopt],"-fwhmx") == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after -fwhmx");
      fval = (float)strtod(argv[nopt],NULL) ;
      if( fval < 0.0f ) ERROR_exit("illegal value after -fwhmx") ;
      fwhm_x = fval ; nopt++; continue;
    }

    /*-----   -fwhmy s   -----*/

    if( strcasecmp(argv[nopt],"-fwhmy") == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after -fwhmy");
      fval = (float)strtod(argv[nopt],NULL) ;
      if( fval < 0.0f ) ERROR_exit("illegal value after -fwhmy") ;
      fwhm_y = fval ; nopt++; continue;
    }

    /*-----   -fwhmz s   -----*/

    if( strcasecmp(argv[nopt],"-fwhmz") == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after -fwhmz");
      fval = (float)strtod(argv[nopt],NULL) ;
      if( fval < 0.0f ) ERROR_exit("illegal value after -fwhmz") ;
      fwhm_z = fval ; nopt++; continue;
    }

    /*-----   -iter n  -----*/

    if( strcmp(argv[nopt],"-iter") == 0 || strncmp(argv[nopt],"-nite",5) == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
      ival = (int)strtod(argv[nopt],NULL) ;
      if( ival < 100 ) ERROR_exit("illegal value after %s",argv[nopt-1]) ;
      niter = ival ; nopt++; continue;
    }

    /*-----   -seed S  -----*/

    if( strcasecmp(argv[nopt],"-seed") == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
      gseed = (unsigned int)strtol(argv[nopt],NULL,10) ;
      if( gseed == 0 ){
        gseed = ((unsigned int)time(NULL)) + 17*(unsiged int)getpid() ;
        INFO_message("-seed 0 resets to %u",gseed) ;
      }
      nopt++; continue;
    }

    /*-----   -pthr p   -----*/

    if( strcmp(argv[nopt],"-pthr") == 0 || strcmp(argv[nopt],"-pval") == 0 ){
      int ii ;
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
      for( ii=nopt ; ii < argc && argv[ii][0] != '-' ; ii++ ) ; /*nada*/
      npthr = ii-nopt ;
      if( npthr <= 0 ) ERROR_exit("No positive values found after %s",argv[nopt-1]) ;
      pthr = (float *)malloc(sizeof(float)*npthr) ;
      for( ii=0 ; ii < npthr ; ii++ ){
        pthr[ii] = (float)strtod(argv[nopt+ii],NULL) ;
        if( pthr[ii] <= 0.0f || pthr >= 1.0f )
          ERROR_exit("value '%s' after '%s' is illegal!",argv[nopt+ii],argv[nopt-1]) ;
      }
      if( npthr > 1 ){
        for( ii=0 ; ii < npthr ; ii++ ) pthr[ii] = -pthr[ii] ;
        qsort_float( npthr , pthr ) ;
        for( ii=0 ; ii < npthr ; ii++ ) pthr[ii] = -pthr[ii] ;
        for( ii=1 ; ii < npthr ; ii++ ){
          if( pthr[ii] == pthr[ii-1] )
            WARNING_message("duplicate value %g after '%s'",pthr[ii],argv[nopt-1]) ;
        }
      }
      nopt += npthr ; continue ;
    }

    /*-----   -athr p   -----*/

    if( strcmp(argv[nopt],"-athr") == 0 || strcmp(argv[nopt],"-aval") == 0 ){
      int ii ;
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
      for( ii=nopt ; ii < argc && argv[ii][0] != '-' ; ii++ ) ; /*nada*/
      nathr = ii-nopt ;
      if( nathr <= 0 ) ERROR_exit("No positive values found after %s",argv[nopt-1]) ;
      athr = (float *)malloc(sizeof(float)*nathr) ;
      for( ii=0 ; ii < nathr ; ii++ ){
        athr[ii] = (float)strtod(argv[nopt+ii],NULL) ;
        if( athr[ii] <= 0.0f || athr >= 1.0f )
          ERROR_exit("value '%s' after '%s' is illegal!",argv[nopt+ii],argv[nopt-1]) ;
      }
      if( nathr > 1 ){
        for( ii=0 ; ii < nathr ; ii++ ) athr[ii] = -athr[ii] ;
        qsort_float( nathr , athr ) ;
        for( ii=0 ; ii < nathr ; ii++ ) athr[ii] = -athr[ii] ;
        for( ii=1 ; ii < nathr ; ii++ ){
          if( athr[ii] == athr[ii-1] )
            WARNING_message("duplicate value %g after '%s'",athr[ii],argv[nopt-1]) ;
        }
      }
      nopt += nathr ; continue ;
    }

    /*----- unknown command -----*/

    ERROR_exit("3dClustSim -- unknown option '%s'",argv[nopt]) ;
  }


  if( mask_dset != NULL ){
    nx = DSET_NX(mask_dset) ;
    ny = DSET_NY(mask_dset) ;
    nz = DSET_NZ(mask_dset) ;
    dx = fabsf(DSET_DX(mask_dset)) ;
    dy = fabsf(DSET_DY(mask_dset)) ;
    dz = fabsf(DSET_DZ(mask_dset)) ;
  }

  return ;
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


  /** 09 Jun 2009: however, the _table variables are common to all threads **/

  /*----- allocate memory space and initialize frequency table -----*/
  *freq_table = (long *) malloc( max_cluster_size * sizeof(long) );
  if (*freq_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 0;  i < max_cluster_size;  i++)
    (*freq_table)[i] = 0;


  /*----- allocate memory space and initialize max cluster size table -----*/
  *max_table = (long *) malloc( max_cluster_size * sizeof(long) );
  if (*max_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 0;  i < max_cluster_size;  i++)
    (*max_table)[i] = 0;


  /*----- initialize voxel intensity sums -----*/
  *count = 0;
  *sum = 0.0;
  *sumsq = 0.0;


  srand48(gseed) ;

  sigmax = FWHM_TO_SIGMA(fwhm_x) ; if( nx < 2 ) sigmax = 0.0f ;
  sigmay = FWHM_TO_SIGMA(fwhm_y) ; if( ny < 2 ) sigmay = 0.0f ;
  sigmaz = FWHM_TO_SIGMA(fwhm_z) ; if( nz < 2 ) sigmaz = 0.0f ;
  do_blur = (sigmax > 0.0f || sigmay > 0.0f || sigmaz > 0.0f ) ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Generate random image.  Most CPU time is spent here. */

void generate_image( float *fim , unsigned short xran[] )
{
  register int ixyz , nxyz ;
  nxyz = nx * ny * nz;
  for( ixyz=0 ; ixyz < nxyz ; ixyz++ ) fim[ixyz] = zgaussian_sss(xran) ;
  if( do_blur )
    EDIT_blur_volume_3d(nx,ny,nz, dx,dy,dz,MRI_float,fim,sigmax,sigmay,sigmaz) ;
  return ;
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
  int nzzz=0 ;


  /*----- initialize local variables -----*/
  nxyz = nx * ny * nz;


  /*----- update statistical sums -----*/
  if (*count < 1.0e+09)
    {
      *count += nxyz;
      for (ixyz = 0;  ixyz < nxyz;  ixyz++)
	{
	  *sum += fim[ixyz];
	  *sumsq += fim[ixyz] * fim[ixyz];
	}
    }


  /*----- calculate z-threshold from statistical sums -----*/
  which = 2;
  p = 1.0 - pthr;
  q = pthr;
  mean = (*sum) / (*count);
  sd = sqrt(((*sumsq) - ((*sum) * (*sum))/(*count)) / ((*count)-1));

#pragma omp critical (CDFNOR)
  cdfnor (&which, &p, &q, &z, &mean, &sd, &status, &bound);
  zthr = z;

  if (!quiet)
  {
#pragma critical (PRINTF)
    {
      pact = pcalc (nx, ny, nz, fim, zthr);
      printf ("pthr=%f zthr=%f pact=%f mean=%f sd=%f ", pthr, zthr, pact,mean,sd);
    }
  }


  /*----- apply threshold to image data -----*/
  for (ixyz = 0;  ixyz < nxyz;  ixyz++)
#if 1
    if (fim[ixyz] > zthr)
      fim[ixyz] = 1.0;
    else
      fim[ixyz] = 0.0;
#else
    if( fim[ixyz] <= zthr ){ fim[ixyz] = 0.0f ; nzzz++; }
#endif

#if 0
#pragma omp critical (PRINTF)
   if( nzzz < 0.05f*nxyz ) WARNING_message("nzzz=%d mean=%f sd=%f zthr=%f p=%f q=%f",nzzz,mean,sd,zthr,p,q) ;
#endif

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
  int do_save=0 ;


  /*----- initialize local variables -----*/
  nxy = nx * ny;

  /*----- create array of clusters -----*/
  clar = MCW_find_clusters( nx,ny,nz , dx,dy,dz ,
			    MRI_float , fim , rmm ) ;

  /*----- record cluster sizes -----*/
  if ((clar == NULL) || (clar->num_clu == 0))
    {
      if (!quiet){
#pragma critical (PRINTF)
        printf ("NumCl=%4d  MaxClSz=%4d\n", 0, 0);
      }
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

	  if (size < max_cluster_size) freq_table[size]++;
	  else                          {freq_table[max_cluster_size-1]++;
                                    WARNING_message("Cluster size = %d",size); }

	  if (size > max_size) max_size = size;

	}

      if (max_size < max_cluster_size) max_table[max_size]++;
      else                             max_table[max_cluster_size-1]++;

      if (!quiet){
#pragma critical (PRINTF)
	printf ("NumCl=%4d  MaxClSz=%4d\n", clar->num_clu, max_size);
      }

#if 0
      if( do_save ){
        for (iclu = 0;  iclu < clar->num_clu;  iclu++){
          cl = clar->clar[iclu] ; if( cl == NULL ) continue ;
          MCW_cluster_to_vol( nx,ny,nz , MRI_float , fim , cl ) ;
        }
#pragma critical (DO_SAVE)
        { char fname[32] ; FILE *fp ;
          for( iclu=0 ; ; iclu++ ){
            sprintf(fname,"Clu%06d",iclu) ;
            if( !THD_is_file(fname) ) break ;
          }
          fp = fopen(fname,"w") ;
          fwrite( fim , sizeof(float) , nx*ny*nz , fp ) ;
          fclose(fp) ;
          WARNING_message("Wrote file %s",fname) ;
        }
      }
#endif

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
  prob_table = (float *) malloc( max_cluster_size * sizeof(float) );
  if (prob_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 1;  i < max_cluster_size;  i++)
    prob_table[i] = 0.0;

  /*----- allocate memory space for alpha table -----*/
  alpha_table = (float *) malloc( max_cluster_size * sizeof(float) );
  if (alpha_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 1;  i < max_cluster_size;  i++)
    alpha_table[i] = 0.0;

  /*----- allocate memory space for cum. prop. of cluster size table  -----*/
  cum_prop_table = (float *) malloc( max_cluster_size * sizeof(float) );
  if (cum_prop_table == NULL)
    AlphaSim_error ("memory allocation error");
  for (i = 1;  i < max_cluster_size;  i++)
    cum_prop_table[i] = 0.0;

  total_num_clusters = 0;
  for (i = 1;  i < max_cluster_size;  i++)
    total_num_clusters += freq_table[i];

  if (power)
    divisor = (float)(niter) * ax * ay * az;
  else
    if (mask_vol)
      divisor = (float)(niter) * mask_ngood;
    else
      divisor = (float)(niter) * nx * ny * nz;

  for (i = 1;  i < max_cluster_size;  i++)
    {
      prob_table[i] = i * freq_table[i] / divisor;
      alpha_table[i] = (float)max_table[i] / (float)niter;
      cum_prop_table[i] = (float)freq_table[i] / (float)total_num_clusters;
    }

  for (i = 1;  i < max_cluster_size-1;  i++)
    {
      j = max_cluster_size - i;
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
#define NIPOW 31
#define DIPOW 0.02f
    int ii ;
    float *zvec=NULL , yvec[4] , *rvec[3] ;
    float *ivv[NIPOW] , ipp[NIPOW] ; int kpp ;
    float atop=0.98f, ahigh=0.3f, alow=0.03f, abot=4.0f/niter ;
    float cout , pp , cbest ;

    for( ibot=1 ;    /* find first value of alpha <= atop */
         ibot < max_cluster_size && alpha_table[ibot] > atop ;
         ibot++ ) ;  /*nada*/
    if( ibot == max_cluster_size ) goto EXTREME_DONE ;

    for( ihigh=ibot+1 ; /* find last value of alpha >= ahigh */
         ihigh < max_cluster_size && alpha_table[ihigh] > ahigh ;
         ihigh++ ) ; /*nada*/
    ihigh-- ;
    if( ihigh-ibot < 4 ) goto EXTREME_DONE ;

    for( ilow=ihigh+1 ; /* find last value of alpha >= alow */
         ilow < max_cluster_size && alpha_table[ilow] > alow ;
         ilow++ ) ; /*nada*/
    ilow-- ;
    if( ilow-ihigh < 4 ) goto EXTREME_DONE ;

    for( itop=ilow+1 ; /* find last value of alpha >= abot */
         itop < max_cluster_size && alpha_table[itop] > abot ;
         itop++ ) ;    /*nada*/
    itop-- ;
    if( itop-ilow < 4 ) goto EXTREME_DONE ;
    ndim = itop-ibot+1 ;

    /** setup different powers of i^ipow where ipow is stored in ipp[] */

    for( kpp=0 ; kpp < NIPOW ; kpp++ ){
      ipp[kpp] = 1.0f - kpp*DIPOW ;
      ivv[kpp] = (float *)malloc(sizeof(float)*ndim) ;
      for( i=0 ; i < ndim ; i++ ){
        val = (float)(i+ibot) ; ivv[kpp][i] = -powf(val,ipp[kpp]) ;
      }
    }

    /** setup other regressors and data **/

    rvec[0] = (float *)malloc(sizeof(float)*ndim) ;  /* regressor = 1 */
    rvec[2] = (float *)malloc(sizeof(float)*ndim) ;  /* posfunc(ihigh-i)^cpow */
    zvec    = (float *)malloc(sizeof(float)*ndim) ;  /* 'data' */
    for( i=0 ; i < ndim ; i++ ){
      ii         = i+ibot ;
      val        = (float)ii ;
      rvec[0][i] = 1.0f ;
      zvec[i]    = logf( -logf( 1.0f - alpha_table[ii] ) ) ;
    }

    cbest = 1.e+38 ; cpow = -1.0f ;

    /** do fits without the posfunc() component, keep best one **/

    for( kpp=0 ; kpp < NIPOW ; kpp++ ){
      yvec[0] = 0.0f ; yvec[1] = 1.0f ; rvec[1] = ivv[kpp] ;
      cout = cl1_solve( ndim , 2 , zvec , rvec , yvec , 1 ) ;
      if( cout >= 0.0f && cout < cbest ){
        cbest = cout; cpow = 1.0f; afit = yvec[0]; bfit = yvec[1]; cfit = 0.0f; ipow = ipp[kpp];
      }
    }

    cbest *= 0.90f ;  /* bias results towards no small i correction */

    /** now loop over powers pp for correction for small i values = posfunc(ihigh-i)^pp **/

    for( pp=1.0f ; pp <= 2.501f ; pp+=0.1f ){
      for( i=0 ; i < ndim ; i++ ){      /* setup rvec[2] regressor */
        ii  = i+ibot ;
        val = (float)(ihigh-ii) ;
        if( val <= 0.0f ) val = 0.0f ;
        else              val = powf(val,pp) ;
        rvec[2][i] = val ;
      }

      for( kpp=0 ; kpp < NIPOW ; kpp++ ){  /* loop over powers for the large i fit */
        yvec[0] = 0.0f ; yvec[1] = 1.0f ; yvec[2] = 0.0f ; rvec[1] = ivv[kpp] ;
        cout = cl1_solve( ndim , 3 , zvec , rvec , yvec , 1 ) ;
        if( cout >= 0.0f && cout < cbest ){
          cbest = cout; cpow = pp; afit = yvec[0]; bfit = yvec[1]; cfit = yvec[2]; ipow = ipp[kpp];
        }
      } /* of loop over ipow power */
    } /* end of loop over cpow=pp power */

    /** print Approx formula **/

    if( cfit != 0.0f )   /* two-part fit was best */
      printf(
         "# Alpha(i) approx 1-exp[-exp"
         "(%.3f-%.4f*i^%.2f%+.4g*posval(%d-i)^%.1f)]\n" ,
         afit , bfit , ipow , cfit , ihigh , cpow ) ;
    else                 /* simpler fit was best */
      printf(
         "# Alpha(i) approx 1-exp[-exp(%.3f-%.4f*i^%.2f)]\n" ,
         afit , bfit , ipow ) ;

    EXTREME_DONE:        /** toss the trash **/
    if( zvec != NULL ){
      free(zvec); free(rvec[0]); free(rvec[2]);
      for( kpp=0 ; kpp < NIPOW ; kpp++ ) free(ivv[kpp]) ;
    }
  }

  /** print the tabular output **/

  if( quiet < 2 ){
    if (!power){
      fprintf (fout, "# Cl Size   Frequency    CumuProp     p/Voxel"
	                  "   Max Freq       Alpha");
      if( gdo_approx && bfit > 0.0f && cpow > 0.0f ) fprintf(fout , "    Approx") ;
      fprintf(fout,"\n") ;
    }
    else {
      fprintf (fout, "# Cl Size   Frequency    CumuProp     p/Voxel"
	                  "   Max Freq       Power\n");
    }
  }

  for (i = 1;  i < max_cluster_size;  i++) {
    if (alpha_table[i] < EPSILON)
      break;
    else {
      fprintf (fout, "%7d  %12ld  %10.6f  %10.8f    %7ld  %10.6f",
	       i, freq_table[i], cum_prop_table[i], prob_table[i],
	       max_table[i], alpha_table[i]);
      if( gdo_approx && bfit > 0.0f && cpow > 0.0f ){
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
  long **mtab=NULL , **ftab=NULL ; int nthr=1 ;  /* arrays of tables */
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


#pragma omp parallel if( niter > 99 )
 {
   int iter , qqq=quiet ; float *fim , *arfim=NULL ;
   long count=0; double sum=0.0, sumsq=0.0 ;
   long *mt , *ft ; int ithr=0 ; unsigned short xran[3] ;

 AFNI_OMP_START ;

  /* create separate tables for each thread, if using OpenMP */
#ifdef USE_OMP
   ithr = omp_get_thread_num() ;
#pragma omp master  /* only in the master thread */
 {
   nthr = omp_get_num_threads() ;
   mtab = (long **)malloc(sizeof(long *)*nthr) ;  /* arrays of tables */
   ftab = (long **)malloc(sizeof(long *)*nthr) ;
   INFO_message("Using %d OpenMP threads",nthr) ;
 }
#pragma omp barrier  /* all threads wait until the above is finished */
   /* create tables for each thread separately */
   mtab[ithr] = mt = (long *) calloc( max_cluster_size , sizeof(long) ) ;
   ftab[ithr] = ft = (long *) calloc( max_cluster_size , sizeof(long) ) ;

   /* initialize random seed array for each thread separately */
   xran[2] = ( gseed        & 0xffff) + (unsigned short)ithr ;
   xran[1] = ((gseed >> 16) & 0xffff) - (unsigned short)ithr ;
   xran[0] = 0x330e                   + (unsigned short)ithr ;

#else /* not OpenMP ==> only one set of tables */
   mt = max_table ;
   ft = freq_table ;
#endif

   /** malloc of image space local to each thread [09 Jun 2009] **/

   fim = (float *)malloc(sizeof(float)*nx*ny*nz) ;
   if( power )
     arfim = (float *)malloc(sizeof(float)*ax*ay*az) ;

  /*----- Monte Carlo iterations -----*/
#pragma omp for
  for (iter = 1;  iter <= niter;  iter++)
    {
      if (!qqq){
#pragma critical (PRINTF)
       printf ("Iter =%5d  \n", iter);
      }

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


      /*----- identify clusters, add to tables ft[] and mt[] -----*/
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
 } /* end OpenMP parallelization */

#ifdef USE_OMP      /* sum tables from various threads into one result */
   if( nthr == 1 ){
     memcpy(freq_table,ftab[0],sizeof(long)*max_cluster_size) ;
     memcpy(max_table ,mtab[0],sizeof(long)*max_cluster_size) ;
   } else {
     int ithr , ii ; long *ft , *mt ;
     for( ithr=0 ; ithr < nthr ; ithr++ ){
       ft = ftab[ithr] ; mt = mtab[ithr] ;
       for( ii=0 ; ii < max_cluster_size ; ii++ ){
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
