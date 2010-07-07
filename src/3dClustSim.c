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
static float dx     = 3.5f ;
static float dy     = 3.5f ;
static float dz     = 3.5f ;
static float fwhm_x = 0.0f ;
static float fwhm_y = 0.0f ;
static float fwhm_z = 0.0f ;
static int   niter  = 10000 ;

static float sigmax , sigmay , sigmaz ;
static int do_blur = 0 ;

static int nodec = 0 ;
static int do_niml = 0 ;

static unsigned int gseed = 123456789 ;

#define PMAX 0.2

static double pthr_init[8] = { 0.02 , 0.01, 0.005, 0.002, 0.001, 0.0005, 0.0002, 0.0001 } ;
static double athr_init[4] = { 0.10 , 0.05 , 0.02 , 0.01 } ;

static int    npthr = 8 ;
static double *pthr = NULL ;
static float  *zthr = NULL ;

static int    nathr = 4 ;
static double *athr = NULL ;

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
   "Program to estimate the probability of false cluster positives.\n"
   "An adaptation of Doug Ward's AlphaSim, streamlined for various purposes.\n"
   "\n"
   "In particular, this program lets you run with multiple p-value thresholds\n"
   "(the '-pthr' option) and only outputs the cluster size threshold at chosen\n"
   "values of the alpha significance level (the '-athr' option).\n"
   "\n"
   "Options: [at least 1 option is required, or you'll get this help message!]\n"
   "--------\n"
   "-nxyz n1 n2 n3 = Size of 3D grid to use for simulation\n"
   "                  [default values = 64 64 32]\n"
   "-dxyz d1 d2 d3 = give all 3 voxel sizes at once\n"
   "                  [default values = 3.5 3.5 3.5]\n"
   "  ** OR **\n"
   "-mask mset     = Use the 0 sub-brick of dataset 'mset' as a mask\n"
   "                  to indicate which voxels to analyze (a sub-brick\n"
   "                  selector '[]' is allowed) \n"
   "         ** If '-mask' is used, then '-nxyz' and '-dxyz' will be ignored. **\n"
   "\n"
   "-fwhm s        = Gaussian filter width (all 3 dimensions)\n"
   "                  [default = 0.0 = no smoothing]\n"
   "                  If you wish to set different smoothing amounts for\n"
   "                  each axis, you can instead use option\n"
   "                    -fwhmxyz sx sy sz\n"
   "                  to specify the three values separately.\n"
   "\n"
   "-pthr p1 .. pn = list of uncorrected (per voxel) p-values at which to\n"
   "                  threshold the simulated images prior to clustering.\n"
   "                  [default = 0.02 0.01 0.005 0.002 0.001 0.0005 0.0002 0.0001]\n"
   "\n"
   "-athr a1 .. an = list of corrected (whole volume) alpha-values at which\n"
   "                  the simulation will print out the cluster size\n"
   "                  thresholds.  For each 'p' and 'a', the smallest cluster\n"
   "                  size C(p,a) for which the probability of the 'p'-thresholded\n"
   "                  image having a noise-only cluster of size C is less than 'a'\n"
   "                  is the output (cf. the sample output, below)\n"
   "                  [default = 0.10 0.05 0.02 0.01]\n"
   "         ** Both lists '-pthr' and '-athr' (of values between 0 and 0.2)    **\n"
   "         ** should be given in DESCENDING order.  They will be sorted to be **\n"
   "         ** that way in any case, and such is how the output will be given. **\n"
   "\n"
   "-iter n        = number of Monte Carlo simulations [default = 10000]\n"
   "\n"
   "-seed S        = random number seed [default seed = 123456789]\n"
   "                  if seed=0, then program will randomize it\n"
   "\n"
   "-nodec         = normally, the program prints the cluster size threshold to\n"
   "                  1 decimal place (e.g., 27.2).  Of course, clusters only come\n"
   "                  with an integer number of voxels -- this fractional value\n"
   "                  is interpolated to give the desired alpha level.  If you\n"
   "                  want no decimal places (so that 27.2 becomes 28), use '-nodec'.\n"
   "\n"
   "-niml          = Output the table in an XML/NIML format, rather than a .1D format.\n"
   "                  This option is for use with other software programs.\n"
   "\n"
   "NOTES:\n"
   "------\n"
   "* Output goes to stdout (e.g., the terminal window).  Use Unix redirection\n"
   "  to capture the results to a file, to save them for historical archives.\n"
   "\n"
   "* Nearest neigbhor clustering is the only type implemented at this time.\n"
   "  That is, voxels are considered neighbors only if they touch face-to-face;\n"
   "  in other words, if their (i,j,k) indexes in the grid differ in only 1 index\n"
   "  and only by plus-or-minus 1 in that direction.\n"
   "\n"
   "* This program is like running AlphaSim once for each '-pthr' value and then\n"
   "  extracting the relevant information from its 'Alpha' output column.\n"
   "\n"
   "-- RW Cox -- July 2010\n"
  ) ;

  printf(
   "\n"
   "SAMPLE OUTPUT from the command '3dClustSim -fwhm 7'\n"
   "\n"
   "# 3dClustSim -fwhm 7\n"
   "# Grid: 64x64x32 3.50x3.50x3.50 mm^3 (131072 voxels)\n"
   "#\n"
   "# CLUSTER SIZE THRESHOLD(pthr,alpha) in Voxels\n"
   "# ------ | alpha = Prob(Cluster >= given size)\n"
   "#  pthr  |  0.100  0.050  0.020  0.010\n"
   "# ------ | ------ ------ ------ ------\n"
   " 0.02000     64.3   71.0   80.5   88.5\n"
   " 0.01000     40.3   44.7   50.7   55.1\n"
   " 0.00500     28.0   31.2   34.9   38.1\n"
   " 0.00200     19.0   21.2   24.2   26.1\n"
   " 0.00100     14.6   16.3   18.9   20.5\n"
   " 0.00050     11.5   13.0   15.1   16.7\n"
   " 0.00020      8.7   10.0   11.6   12.8\n"
   " 0.00010      7.1    8.3    9.7   10.9\n"
   "\n"
   "e.g., for this sample volume, if the per-voxel p-value threshold is set\n"
   "at 0.005, then to keep the probability of getting a single noise-only\n"
   "cluster at 0.05 or less, the cluster size threshold should be 32 voxels\n"
   "(the next integer above 31.2).\n"
   "\n"
   "If you ran the same simulation with the '-nodec' option, then the last\n"
   "line above would be\n"
   " 0.00010        8      9     10     11\n"
   "If you set the per voxel p-value to 0.0001 (1e-4), and want the chance\n"
   "of a noise-only false-positive cluster to be 5%% or less, then the cluster\n"
   "size threshold would be 9 -- that is, you would keep all NN clusters with\n"
   "9 or more voxels.\n"
   "\n"
   "The header lines start with the '#' character so that the result is a\n"
   "correctly formatted AFNI .1D file -- it can be used in 1dplot, etc.\n"
  ) ;

  PRINT_AFNI_OMP_USAGE("3dClustSim",NULL) ;
  exit(0);
}

/*---------------------------------------------------------------------------*/
/* Routine to initialize the input options (values are in global variables). */

void get_options( int argc , char **argv )
{
  int nopt=1 , ii ;

  /*----- add to program log -----*/

  AFNI_logger("3dClustSim",argc,argv) ;

  pthr = (double *)malloc(sizeof(double)*npthr) ;
  memcpy( pthr , pthr_init , sizeof(double)*npthr ) ;

  athr = (double *)malloc(sizeof(double)*nathr) ;
  memcpy( athr , athr_init , sizeof(double)*nathr ) ;

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
      if( mask_ngood < 128 ) ERROR_exit("-mask has only %d nonzero voxels!",mask_ngood) ;
      INFO_message("%d voxels in mask (%.1f%% of total)",
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
        INFO_message("-seed 0 resets to %u",gseed) ;
      }
      nopt++; continue;
    }

    /*-----   -pthr p   -----*/

    if( strcmp(argv[nopt],"-pthr") == 0 || strcmp(argv[nopt],"-pval") == 0 ){
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
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
        for( ii=0 ; ii < npthr ; ii++ ) pthr[ii] = -pthr[ii] ;
        qsort_double( npthr , pthr ) ;
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
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
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
      nopt += nathr ; continue ;
    }

    /*----   -nodec   ----*/

    if( strcasecmp(argv[nopt],"-nodec") == 0 ){
      nodec = 1 ; nopt++ ; continue ;
    }

    /*----   -niml   ----*/

    if( strcasecmp(argv[nopt],"-niml") == 0 ){
      do_niml = 1 ; nopt++ ; continue ;
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

  nxy = nx*ny ; nxyz = nxy*nz ;
  if( nxyz < 256 )
    ERROR_exit("Only %d voxels in simulation?! Need at least 256.",nxyz) ;

  if( mask_ngood == 0 ) mask_ngood = nxyz ;

  srand48(gseed) ;  /* not really needed */

  /*-- z-score thresholds for the various p-values --*/

  zthr = (float *)malloc(sizeof(float)*npthr) ;
  for( ii=0 ; ii < npthr ; ii++ ){
    zthr[ii] = (float)zthresh(pthr[ii]) ;
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
  for( ii=0 ; ii < nxyz ; ii++ ) fim[ii] = zgaussian_sss(xran) ;
  if( do_blur ){
    FIR_blur_volume_3d(nx,ny,nz,dx,dy,dz,fim,sigmax,sigmay,sigmaz) ;
    for( sum=0.0f,ii=0 ; ii < nxyz ; ii++ ) sum += fim[ii]*fim[ii] ;
    sum = sqrtf( nxyz / sum ) ;
    for( ii=0 ; ii < nxyz ; ii++ ) fim[ii] *= sum ;
  }
  if( mask_vol != NULL ){
    for( ii=0 ; ii < nxyz ; ii++ ) if( !mask_vol[ii] ) fim[ii] = 0.0f ;
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

int find_largest_cluster( byte *mmm )
{
   int max_size=0 ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km ;
   int nnow  ; short *inow=NULL , *jnow , *know ; int nall ;
#ifdef USE_MEMCHR
   byte *mch ;
#endif

   ijk_last = 0 ;
   while(1) {
     /* find next nonzero point */

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
     if( inow == NULL ){
       nall = DALL ;                                 /* # allocated pts */
       inow = (short *) malloc(sizeof(short)*DALL) ; /* coords of pts */
       jnow = (short *) malloc(sizeof(short)*DALL) ;
       know = (short *) malloc(sizeof(short)*DALL) ;
     }
     IJK_TO_THREE(ijk, inow[0],jnow[0],know[0] , nx,nxy) ;

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

   if( inow != NULL ){ free(know) ; free(jnow) ; free(inow) ; }
   return max_size ;
}

/*---------------------------------------------------------------------------*/
/* Find clusters, save some info, re-populate array? */

void gather_stats( int ithr , float *fim , byte *bfim , int *mtab )
{
  register int ii ; register float thr ; int siz ;

  thr = zthr[ithr] ;
  for( ii=0 ; ii < nxyz ; ii++ ) bfim[ii] = (fim[ii] > thr) ;
  siz = find_largest_cluster( bfim ) ;
  if( siz > max_cluster_size ) siz = max_cluster_size ;
  mtab[siz]++ ;

  return ;
}

/*===========================================================================*/

int main( int argc , char **argv )
{
  int **max_table=NULL ; int ipthr , nthr=1 ;
#ifdef USE_OMP
  int ***mtab=NULL ;
#endif

  /*----- does user request help menu? -----*/

  if( argc < 2 || strcmp(argv[1],"-help") == 0 ) display_help_menu() ;

  /*----- get the list of things to do -----*/

  mainENTRY("3dClustSim"); machdep();
  AFNI_logger("3dClustSim",argc,argv);
  PRINT_VERSION("3dClustSim"); AUTHOR("RW Cox and BD Ward");

  get_options( argc , argv ) ;

MPROBE ;

  max_table = (int **)malloc(sizeof(int *)*npthr) ;  /* array of tables */
  for( ipthr=0 ; ipthr < npthr ; ipthr++ )           /* create tables */
    max_table[ipthr] = (int *)calloc(sizeof(int),(max_cluster_size+1)) ;

MPROBE ;

#pragma omp parallel
 {
   int iter, ithr, ipthr, **mt ;
   float *fim ; byte *bfim ; unsigned short xran[3] ;
   int vstep , vii ;

 AFNI_OMP_START ;

  /* create separate tables for each thread, if using OpenMP */
#ifdef USE_OMP
   ithr = omp_get_thread_num() ;
#pragma omp master  /* only in the master thread */
 {
   nthr = omp_get_num_threads() ;
   mtab = (int ***)malloc(sizeof(int **)*nthr) ;  /* array of arrays of tables */
   INFO_message("Using %d OpenMP threads",nthr) ;
 }
#pragma omp barrier  /* all threads wait until the above is finished */
   /* create tables for each thread separately */
   mtab[ithr] = mt = (int **)malloc(sizeof(int *)*npthr) ;
   for( ipthr=0 ; ipthr < npthr ; ipthr++ )
     mt[ipthr] = (int *)calloc(sizeof(int),(max_cluster_size+1)) ;

   /* initialize random seed array for each thread separately */
   xran[2] = ( gseed        & 0xffff) + (unsigned short)ithr ;
   xran[1] = ((gseed >> 16) & 0xffff) - (unsigned short)ithr ;
   xran[0] = 0x330e                   + (unsigned short)ithr ;

#else /* not OpenMP ==> only one set of tables */
   mt = max_table ; ithr = 0 ;
   xran[2] = ( gseed        & 0xffff) ;
   xran[1] = ((gseed >> 16) & 0xffff) ;
   xran[0] = 0x330e ;
#endif

   fim  = (float *)malloc(sizeof(float)*nxyz) ;  /* image space */
   bfim = (byte * )malloc(sizeof(byte) *nxyz) ;

   vstep = niter / (nthr*50) ;
   vii   = 0 ;
   if( ithr == 0 ) fprintf(stderr,"Simulating: ") ;

  /*----- Monte Carlo iterations -----*/

#pragma omp for
  for( iter=1 ; iter <= niter ; iter++ ){

    if( ithr == 0 ){
      vii++ ; if( vii%vstep == 0 ) vstep_print() ;
    }

    generate_image( fim , xran ) ;
MPROBE ;

    for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
      gather_stats( ipthr , fim , bfim , mt[ipthr] ) ;
MPROBE ;
    }

  } /* end of simulation loop */

MPROBE ;
  free(fim) ; free(bfim) ;
  if( ithr == 0 ) fprintf(stderr,"\n") ;

 AFNI_OMP_END ;
 } /* end OpenMP parallelization */

   /*-------- sum tables from various threads into one result ----------*/

MPROBE ;

#ifdef USE_OMP
   { int ithr , ii , ipthr , **mt , *mth ;
     for( ithr=0 ; ithr < nthr ; ithr++ ){
       mt = mtab[ithr] ;
       for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
         mth = mt[ipthr] ;
         for( ii=1 ; ii <= max_cluster_size ; ii++ )
           max_table[ipthr][ii] += mth[ii] ;
       }
     }
   }
#endif

MPROBE ;

  enable_mcw_malloc() ;

  /*---------- compute and print the output table ----------*/

  { double *alpha , aval ;
    float **clust_thresh , cmax=0.0f ;
    int ii , itop , iathr ;
    char *commandline = tross_commandline("3dClustSim",argc,argv) ;

    alpha        = (double *)malloc(sizeof(double)*(max_cluster_size+1)) ;
    clust_thresh = (float **)malloc(sizeof(float *)*npthr) ;
    for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
      clust_thresh[ipthr] = (float *)malloc(sizeof(float)*nathr) ;
      for( iathr=0 ; iathr < nathr ; iathr++ ) clust_thresh[ipthr][iathr] = -666.0f ;
      for( itop=ii=1 ; ii <= max_cluster_size ; ii++ ){
        alpha[ii] = max_table[ipthr][ii] / (double)niter ;
        if( alpha[ii] > 0.0 ) itop = ii ;
      }
      for( ii=itop-1 ; ii >= 1 ; ii-- ) alpha[ii] += alpha[ii+1] ;
      for( iathr=0 ; iathr < nathr ; iathr++ ){
        aval = athr[iathr] ;
        for( ii=1 ; ii < itop ; ii++ )
          if( alpha[ii] > aval && alpha[ii+1] <= aval ) break ;
        if( ii < itop ){
          double alo=alpha[ii] , ahi=alpha[ii+1] ;
#if 0
INFO_message("pthr=%g athr=%g ii=%d alpha[ii]=%g alpha[ii+1]=%g",pthr[ipthr],athr[iathr],ii,alpha[ii],alpha[ii+1]) ;
#endif
          if( alo < 1.0 && ahi > 0.0 ){
            aval = log(-log(1.0-aval)) ;
            alo  = log(-log(1.0-alo)) ;
            ahi  = log(-log(1.0-ahi)) ;
            aval = ii + (aval-alo)/(ahi-alo) ;
          } else {
            aval = ii + (alo-aval)/alo ;
          }
          if( nodec ) aval = (int)(aval+0.951) ;
          clust_thresh[ipthr][iathr] = aval ;
#if 0
ININFO_message("aval=%g alo=%g ahi=%g ==> thresh=%g",aval,alo,ahi,clust_thresh[ipthr][iathr]) ;
#endif
        } else {
#if 0
INFO_message("pthr=%g athr=%g ii=%d alpha[ii]=%g alpha[ii+1]=%g",pthr[ipthr],athr[iathr],ii,alpha[ii],alpha[ii+1]) ;
#endif
          clust_thresh[ipthr][iathr] = itop ;
        }
        if( clust_thresh[ipthr][iathr] > cmax ) cmax = clust_thresh[ipthr][iathr] ;
      }
    }

    if( !nodec && !do_niml && cmax > 9999.8f ){  /* if largest output is way big, */
      for( ipthr=0 ; ipthr < npthr ; ipthr++ ){  /* then truncate all to integers */
        for( iathr=0 ; iathr < nathr ; iathr++ ){
          aval = clust_thresh[ipthr][iathr] ;
          aval = (int)(aval+0.951) ;
          clust_thresh[ipthr][iathr] = aval ;
        }
      }
      nodec = 1 ;
    }

MPROBE ;

    fflush(stderr) ; fflush(stdout) ;

    if( !do_niml ){  /* output in 1D format */
      printf(
       "# %s\n"
       "# Grid: %dx%dx%d %.2fx%.2fx%.2f mm^3 (%d voxels%s)\n"
       "#\n"
       "# CLUSTER SIZE THRESHOLD(pthr,alpha) in Voxels\n"
       "# ------ | alpha = Prob(Cluster >= given size)\n"
       "#  pthr  |" ,
       commandline ,
       nx,ny,nz , dx,dy,dz ,
       mask_ngood , (mask_ngood < nxyz) ? " in mask" : "\0" ) ;
      for( iathr=0 ; iathr < nathr ; iathr++ ) printf(" %6.3f",athr[iathr]) ;
      printf("\n"
       "# ------ |" ) ;
      for( iathr=0 ; iathr < nathr ; iathr++ ) printf(" ------") ;
      printf("\n") ;
      for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
        printf("%8.5f  ",pthr[ipthr]) ;
        for( iathr=0 ; iathr < nathr ; iathr++ ){
          if( nodec ) printf("%7d"  ,(int)clust_thresh[ipthr][iathr]) ;
          else        printf("%7.1f",     clust_thresh[ipthr][iathr]) ;
        }
        printf("\n") ;
      }
    } else {       /* output in NIML format */
      NI_element *nel ; float *vec ; char buf[1024] , *bbb ; NI_float_array nfar ;
      nel = NI_new_data_element( "3dClustSim" , npthr ) ;
      vec = (float *)malloc(sizeof(float)*MAX(npthr,nathr)) ;
      for( iathr=0 ; iathr < nathr ; iathr++ ){
        for( ipthr=0 ; ipthr < npthr ; ipthr++ ) vec[ipthr] = clust_thresh[ipthr][iathr] ;
        NI_add_column( nel , NI_FLOAT , vec ) ;
      }
      NI_set_attribute( nel , "commandline" , commandline ) ;
      sprintf(buf,"%d,%d,%d",nx,ny,nz) ; NI_set_attribute(nel,"nxyz",buf) ;
      sprintf(buf,"%.3f,%.3f,%.3f",dx,dy,dz) ; NI_set_attribute(nel,"dxyz",buf) ;
      sprintf(buf,"%.2f,%.2f,%.2f",fwhm_x,fwhm_y,fwhm_z) ; NI_set_attribute(nel,"fwhmxyz",buf) ;
      sprintf(buf,"%d",niter) ; NI_set_attribute(nel,"iter",buf) ;
      for( ipthr=0 ; ipthr < npthr ; ipthr++ ) vec[ipthr] = pthr[ipthr] ;
      nfar.num = npthr ; nfar.ar = vec ; bbb = NI_encode_float_list(&nfar,",") ;
      NI_set_attribute(nel,"pthr",bbb) ; NI_free(bbb) ;
      for( iathr=0 ; iathr < nathr ; iathr++ ) vec[iathr] = athr[iathr] ;
      nfar.num = nathr ; nfar.ar = vec ; bbb = NI_encode_float_list(&nfar,",") ;
      NI_set_attribute(nel,"athr",bbb) ; NI_free(bbb) ;
      NI_write_element_tofile( "stdout:" , nel , NI_TEXT_MODE ) ;
    }
    fflush(stdout) ;
  }

  /* run away screaming */

  exit(0);
}
