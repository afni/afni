#include "mrilib.h"
#include <ctype.h>

#undef DEBUG_MEM
#if 0 && !defined(DONT_USE_MCW_MALLOC)
# define DEBUG_MEM
#endif

#ifdef USE_OMP
# include <omp.h>
#endif

#ifdef DEBUG_MEM
# include "mcw_malloc.c"
# define malloc(a)     mcw_malloc((a),__FILE__,__LINE__)
# define realloc(a,b)  mcw_realloc((a),(b),__FILE__,__LINE__)
# define calloc(a,b)   mcw_calloc((a),(b),__FILE__,__LINE__)
# define free(a)       mcw_free((a),__FILE__,__LINE__)
# undef  strdup
# define strdup(a)     mcw_strdup((a),__FILE__,__LINE__)
#endif

#include "mri_threshX.c"  /* important stuff: clustering, thresholding */
#include "thd_Xdataset.c" /* input dataset format and funcs */

#undef MEMORY_CHECK
#ifdef DEBUG_MEM
# define MEMORY_CHECK(mm)                                                               \
   do{ if( verb > 1 )                                                                    \
         ININFO_message("Memory %s: %s",mm,mcw_malloc_status("3dXClustSim.c",__LINE__)) ; \
   } while(0)
#else
# define MEMORY_CHECK(mm) /*nada*/
#endif

/*---------------------------------------------------------------------------*/
/*--- Global data ---*/

static THD_3dim_dataset  *mask_dset  = NULL ; /* mask dataset */
static byte              *mask_vol   = NULL;  /* mask volume */
static int mask_nvox = 0, mask_ngood = 0;     /* number of good voxels in mask volume */

#define INMASK(ijk) (mask_vol[ijk]!=0)

/* 3D indexes for each point in the mask */

static ind_t *ipmask=NULL , *jpmask=NULL , *kpmask=NULL ;
static int   *ijkmask=NULL ;

/* map from 1D index in volume to points in the mask */

static int *ijk_to_vec=NULL ;

static Xdataset *xinset=NULL ;  /* global struct of input dataset(s) */

static int   nx ;     /* 3D grid stuff */
static int   ny ;
static int   nz ;
static int   nxy ;
static int   nxyz ;
static int   nxyz1 ;
static float dx ;
static float dy ;
static float dz ;

static int   niter=0 ;  /* number of iterations (realizations per case) */
static float nit33=0.0f ;

/*-- stuff for extra outputs (don't enable this without good reason) --*/

#undef ALLOW_EXTRAS

#ifdef ALLOW_EXTRAS
 static int   do_FOMcount = 0 ;
 static int   do_FARvox   = 0 ;
#else
# define do_FOMcount 0
# define do_FARvox   0
#endif

/* p-value thresholds, etc. */

#define PMAX 0.5

static int    npthr = 5 ;
static double *pthr = NULL ;
static double pthr_init[5] = { 0.0100, 0.0056, 0.0031, 0.0018, 0.0010 } ;

static float  *zthr_1sid = NULL ;
static float  *zthr_2sid = NULL ;
static float  *zthr_used = NULL ;

/* hpow values: FOM = sum |z|^H for H=0, 1, and/or 2 */

static int    do_hpow0 = 0 ;  /* 1 or 0 */
static int    do_hpow1 = 0 ;
static int    do_hpow2 = 1 ;
static int    nhpow    = 1 ;  /* sum of the above */

/* cases (e.g., blurs) */

static int    ncase = 1 ;
static char **lcase = NULL ;

/* number of simulation volumes needed */

static int nsim  = 0 ;  /* ncase * niter = number of sim volumes */

/** athr would be for computing threshold for more than one goal **/

#if 0
static int    nathr = 5 ;
static double *athr = NULL ;
static double athr_init[5] = { 0.05 , 0.04 , 0.03 , 0.02 , 0.01 } ;
#endif

static int verb = 1 ;
static int nthr = 1 ;  /* default number of threads */

static int nnlev = 2 ; /* NN                = 1 or 2 or 3 */
static int nnsid = 2 ; /* sidedness of test = 1 or 2      */

/* macro for use inside parallel regions,
   to declare and get ithr = thread number */

#undef DECLARE_ithr
#ifdef USE_OMP
# define DECLARE_ithr const int ithr=omp_get_thread_num()
#else
# define DECLARE_ithr const int ithr=0
#endif

static const int min_mask = 1024 ;    /* min # voxels */
static const int min_nvol = 10000 ;   /* min # realizations */

static float dilate_fac = 0.0666f ;   /* for STEP 2 */

static char *prefix = "Xsim.nii" ;

static MRI_IMAGE *imtemplate = NULL ;

#undef  PSMALL
#define PSMALL 1.e-15

#define FARP_GOAL 5.00f    /* 5 percent */
#define FGFAC     1.00f    /* fudge factor (1 = no fudge for you) */

static float fgfac     = FGFAC ;
static float farp_goal = FARP_GOAL ;

/* lines directly below are also in 3dttest++.c
   only change them here if you change them there as well! */
#define NFARP 8
static float farplist[NFARP] = { 2.f, 3.f, 4.f, 5.f, 6.f, 7.f, 8.f, 9.f } ;
static int do_multifarp = 0 ;
static int numfarp = 1 ;

static char *abcd[NFARP]     = { "a", "b", "c", "d", "e", "f", "g", "h" } ;

#define FG_GOAL  (farp_goal*fgfac)
#define MAXITE   11

static int do_global_etac=1 ; /* Sep 2018 */
static int do_local_etac =1 ;

#define TOPFRAC 0.2468f

/*----------------------------------------------------------------------------*/
/*! Threshold for upper tail probability of N(0,1) = 1-inverseCDF(pval) */

double zthresh( double pval )
{
        if( pval <= 0.0 ) pval = PSMALL ;
   else if( pval >= 1.0 ) pval = 1.0 - PSMALL ;
   return qginv(pval) ;   /* mri_stats.c */
}

/*---------------------------------------------------------------------------*/
/* For use in progress counts [currently there is no progress] */

#if 0
static int vsnn=0 ;

static void vstep_reset(void){ vsnn=0; }

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[vsnn%10] ) ;
   if( vsnn%10 == 9) fprintf(stderr,".") ;
   vsnn++ ;
}
#endif

/*---------------------------------------------------------------------------*/
/* Routine to initialize the input options (values are in global variables). */

void get_options( int argc , char **argv )
{
  char *ep;
  int nopt=1 , ii ;

ENTRY("get_options") ;

  while( nopt < argc ){  /* scan until all args processed */

    /*-----  -ncase N lab1 lab2 ... labN -----*/

    if( strcasecmp(argv[nopt],"-ncase") == 0 ){  /* for multiple blurs (e.g.) */
      char *thisopt=argv[nopt] ;
      if( ++nopt >= argc )
        ERROR_exit("No argument after '%s' :(",thisopt) ;
      ncase = (int)strtod(argv[nopt],NULL) ;
      if( ncase < 1 )
        ERROR_exit("Value '%s' after option '%s' is illegal :(",argv[nopt],thisopt) ;
      if( ncase > 9 )
        WARNING_message("Value %d after option '%s' is very big (> 9)",ncase,thisopt) ;
      if( nopt+ncase >= argc )
        ERROR_exit("Need %d label arguments after option '%s %d'",ncase,thisopt,ncase) ;
      lcase = (char **)malloc(sizeof(char *)*ncase) ;
      for( ii=0 ; ii < ncase ; ii++ ){
        lcase[ii] = strdup(argv[nopt+ii+1]) ;
        if( lcase[ii][0] == '-' )
          WARNING_message("label '%s' after option '%s' looks like another option :(",
                          lcase[ii],thisopt) ;
        else if( lcase[ii][0] == '\0' || isspace(lcase[ii][0]) ){
          WARNING_message("label #%d after option '%s' is empty :(",
                          ii+1,thisopt) ;
        }
      }
      nopt += ncase+1 ; continue ;
    }

    /*-----  -dilfac ddd  -----*/

    if( strcasecmp(argv[nopt],"-dilfac") == 0 ){  /* dilation factor */
      if( ++nopt >= argc )
        ERROR_exit("You need 1 argument after option '-dilfac'") ;
      dilate_fac = (float)strtod(argv[nopt],NULL) ;
      nopt++ ; continue ;
    }

#if 0
    /*-----  -niter NNN  -----*/

    if( strcasecmp(argv[nopt],"-niter") == 0 ){
      if( ++nopt >= argc )
        ERROR_exit("You need 1 argument after option '-niter'") ;
      niter = (int)strtod(argv[nopt],NULL) ;
      if( niter < min_nvol )
        WARNING_message("running with -niter %d less than %d is STRONGLY deprecated!",
                        niter , min_nvol ) ;
      nopt++ ; continue ;
    }
#endif

    /*----- -NN 1 or 2 or 3 -----*/

    if( strcasecmp(argv[nopt],"-NN") == 0 ){
      if( ++nopt >= argc )
        ERROR_exit("You need 1 argument after option '-NN'") ;
      nnlev = (int)strtod(argv[nopt],NULL) ;
      if( nnlev < 1 || nnlev > 3 )
        ERROR_exit("-NN must be 1 or 2 or 3 :(") ;
      nopt++ ; continue ;
    }
    if( strcasecmp(argv[nopt],"-NN1") == 0 || -strcasecmp(argv[nopt],"-1NN") == 0 ){
      nnlev = 1 ; nopt++ ; continue ;
    }
    if( strcasecmp(argv[nopt],"-NN2") == 0 || -strcasecmp(argv[nopt],"-2NN") == 0 ){
      nnlev = 2 ; nopt++ ; continue ;
    }
    if( strcasecmp(argv[nopt],"-NN3") == 0 || -strcasecmp(argv[nopt],"-3NN") == 0 ){
      nnlev = 3 ; nopt++ ; continue ;
    }

    /*----- -sid 1 or 2 -----*/

    if( strcasecmp(argv[nopt],"-sid") == 0 ){
      if( ++nopt >= argc )
        ERROR_exit("You need 1 argument after option '-sid'") ;
      nnsid = (int)strtod(argv[nopt],NULL) ;
      if( nnsid < 1 || nnsid > 2 )
        ERROR_exit("-sid must be 1 or 2 :(") ;
      nopt++ ; continue ;
    }
    if( strcasecmp(argv[nopt],"-sid1") == 0 || strcasecmp(argv[nopt],"-1sid") == 0 ){
      nnsid = 1 ; nopt++ ; continue ;
    }
    if( strcasecmp(argv[nopt],"-sid2") == 0 || strcasecmp(argv[nopt],"-2sid") == 0 ){
      nnsid = 2 ; nopt++ ; continue ;
    }

    /*-----  -inset mask sdata ... -----*/

    if( strcasecmp(argv[nopt],"-inset" ) == 0 ||    /* no dataset inputs */
        strcasecmp(argv[nopt],"-insdat") == 0   ){  /* just sdat files */
      int nfile ;
      if( xinset != NULL )
        ERROR_exit("You can't use option '%s' more than once!",argv[nopt]) ;
      if( ++nopt >= argc-1 )
        ERROR_exit("You need at least 2 arguments after option '%s'",argv[nopt-1]) ;

      for( ii=nopt ; ii < argc && argv[ii][0] != '-' ; ii++ ) ; /*nada*/
      nfile = ii-nopt ;

      if( verb )
        INFO_message("Loading %s datasets",argv[nopt-1]) ;

      xinset = open_Xdataset( argv[nopt], nfile-1,argv+(nopt+1) ) ;

      if( xinset->ngood < min_mask )
        ERROR_exit("mask has %d good voxels; minimum allowed is %d",
                   xinset->ngood , min_mask ) ;

      if( xinset->nvtot < min_nvol )
        WARNING_message("only %d input volumes, less than minimum of %d",
                        xinset->nvtot,min_nvol) ;

      nopt += nfile ; continue ;
    }

    /*-----  -prefix -----*/

    if( strcasecmp(argv[nopt],"-prefix") == 0 ){
      nopt++ ; if( nopt >= argc ) ERROR_exit("need argument after -prefix!") ;
      prefix = strdup(argv[nopt]) ;
      if( !THD_filename_ok(prefix) ) ERROR_exit("bad -prefix option!") ;
      nopt++ ; continue ;
    }

    /*----   -quiet and -verb   ----*/

    if( strcasecmp(argv[nopt],"-quiet") == 0 ){  /* why would you want this? */
      verb = 0 ; nopt++ ; continue ;
    }

    if( strcasecmp(argv[nopt],"-verb") == 0 ){   /* more fun fun fun! */
      verb++ ; nopt++ ; continue ;
    }

#ifdef ALLOW_EXTRAS
    /*----   -FOMcount   ----*/

    if( strcasecmp(argv[nopt],"-FOMcount") == 0 ){
      do_FOMcount = 1 ; nopt++ ; continue ;
    }

    /*----   -noFARvox   ----*/

    if( strcasecmp(argv[nopt],"-noFARvox") == 0 ){
      do_FARvox = 0 ; nopt++ ; continue ;
    }

    /*----   -FARvox   ----*/

    if( strcasecmp(argv[nopt],"-FARvox") == 0 ){
      do_FARvox = 1 ; nopt++ ; continue ;
    }
#endif

    /*-----   -pthr p p p   -----*/

#define IS_NUMERIC(sss)                         \
 ( (                    isdigit((sss)[0]) ) ||  \
   ( (sss)[0] == '.' && isdigit((sss)[1]) )   )


    if( strcmp(argv[nopt],"-pthr") == 0 || strcmp(argv[nopt],"-pval") == 0 ){

      if( pthr != NULL )         ERROR_exit("you can't use -pthr twice!") ;
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);

      /* scan to see how many numeric args follow */

      for( ii=nopt ; ii < argc && IS_NUMERIC(argv[ii]) ; ii++ ) ; /*nada*/
      npthr = ii-nopt ;
      if( npthr <= 0 ) ERROR_exit("No positive values found after %s",argv[nopt-1]) ;

      /* re-scan to load numeric values */

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
      continue ;
    }

    /*-----   -hpow h h h  [28 Mar 2017]  -----*/

    if( strcmp(argv[nopt],"-hpow") == 0 ){
      int jj ;
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);

      /* scan for integers 0 and/or 1 and/or 2 */

      do_hpow0 = do_hpow1 = do_hpow2 = 0 ;
      for( ; nopt < argc && isdigit(argv[nopt][0]) ; nopt++ ){
        switch( (int)strtod(argv[nopt],NULL) ){
          case 0: do_hpow0 = 1 ; break ;
          case 1: do_hpow1 = 1 ; break ;
          case 2: do_hpow2 = 1 ; break ;
          default:
            ERROR_exit("value '%s' after '-hpow' is illegal (only 0, 1, and/or 2 allowed)",argv[nopt]) ;
        }
      }

      nhpow = do_hpow0 + do_hpow1 + do_hpow2 ;
      if( nhpow < 1 )
        ERROR_exit("No valid powers (0 and/or 1 and/or 2) found after '-hpow' :(") ;

      continue ;
    }

    /*-----  -multiFPR [23 Aug 2017]  -----*/

    if( strcasecmp(argv[nopt],"-multiFPR") == 0 ){
      do_multifarp = 1 ;
      nopt++ ; continue ;
    }

    /*-----  -minclust [21 Sep 2017]  -----*/

    if( strcasecmp(argv[nopt],"-minclust") == 0 ){
      int mc ;
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
      mc = (int)strtod(argv[nopt],NULL) ;
      if( mc < 2 )
        WARNING_message("value %d after %s is illegal",mc,argv[nopt-1]) ;
      else
        set_Xmin_clust(mc) ;

      nopt++ ; continue ;
    }


    /*-----  -FPR xx [23 Aug 2017]  -----*/

    if( strcasecmp(argv[nopt],"-FPR") == 0 ){
      float fgoal ;
      nopt++; if( nopt >= argc ) ERROR_exit("need argument after %s",argv[nopt-1]);
      do_multifarp = 0 ;
      fgoal = (float)rint(strtod(argv[nopt],NULL)) ;
      if( fgoal < 2.0f ){
        WARNING_message("fpr=%.1f%% too small : setting fpr=2",fgoal) ;
        fgoal = 2.0f ;
      } else if( fgoal > 9.0f ){
        WARNING_message("fpr=%.1f%% too large : setting fpr=9",fgoal) ;
        fgoal = 9.0f ;
      }
      farp_goal = fgoal ;
      nopt++ ; continue ;
    }

    /*-----  global and local [Sep 2018] -----*/

    if( strcasecmp(argv[nopt],"-noglobal") == 0 ){   /* Sep 2018 */
      do_global_etac = 0 ; nopt++ ; continue ;
    }
    if( strcasecmp(argv[nopt],"-nolocal") == 0 ){
      do_local_etac = 0 ; nopt++ ; continue ;
    }
    if( strcasecmp(argv[nopt],"-global") == 0 ){
      do_global_etac = 1 ; nopt++ ; continue ;
    }
    if( strcasecmp(argv[nopt],"-local") == 0 ){
      do_local_etac = 1 ; nopt++ ; continue ;
    }

    /*----- unknown option -----*/

    ERROR_exit("3dXClustSim -- unknown option '%s' :(",argv[nopt]) ;

  } /* end loop over command line args */

  if( !do_global_etac && !do_local_etac )
    ERROR_exit("3dXClustSim: global and local calculations both turned off! :(") ;

  /*-- sneaky way to change default parameters --*/

  fgfac = AFNI_numenv("AFNI_XCLUSTSIM_FGFAC") ;
  if( fgfac < 0.1f || fgfac > 1.0f ) fgfac = FGFAC ;

  if( !do_multifarp ){
    numfarp = 1 ; farplist[0] = farp_goal ;
    INFO_message("Single FPR goal: %.1f%%",farp_goal) ;
  } else {
    char msg[256] ;
    numfarp = NFARP ;
    strcpy(msg,"Multiple FPR goals:") ;
    for( ii=0 ; ii < numfarp ; ii++ )
      sprintf( msg+strlen(msg) , " %.1f%%%%" , farplist[ii] ) ;
    INFO_message(msg) ;
  }

  INFO_message("minimum cluster size = %d voxels",min_clust) ; /* 21 Sep 2017 */

  /*------- finalize some simple setup stuff --------*/

  if( xinset == NULL ) ERROR_exit("-inset or -insdat option is mandatory :(") ;

  /* create default pthr array, if none was given */

  if( pthr == NULL ){
    pthr = (double *)malloc(sizeof(double)*npthr) ;
    AAmemcpy( pthr , pthr_init , sizeof(double)*npthr ) ;
    if( verb > 1 )
      INFO_message("Using default %d p-value thresholds",npthr) ;
  }

  /* create the default label for ncase=1 */

  if( ncase == 1 & lcase == NULL ){
    lcase = (char **)malloc(sizeof(char *)) ;
    lcase[0] = strdup("A") ;
    if( verb > 1 )
      INFO_message("Using default case label 'A'") ;
  }

  /* check to see if -ncase is used correctly; set number of iterations */

  if( ncase > 1 ){
    if( xinset->nvtot % ncase != 0 ){
      ERROR_exit("number of input volumes=%d not evenly divisible by ncase=%d",
                 xinset->nvtot,ncase) ;
    } else {
      niter = xinset->nvtot / ncase ;
      if( verb > 1 ){
          INFO_message("number of input volumes = %d",xinset->nvtot) ;
        ININFO_message("number of cases         = %d",ncase) ;
        ININFO_message("number of iterations    = %d / %d = %d",xinset->nvtot,ncase,niter) ;
      }
    }
  } else {
    niter = xinset->nvtot ;
    if( verb > 1 )
      INFO_message("number of input volumes = number of iterations = %d",niter) ;
  }

  nsim = niter * ncase ;  /* number of simulation volumes */

#if 0
  if( athr == NULL ){
    athr = (double *)malloc(sizeof(double)*nathr) ;
    AAmemcpy( athr , athr_init , sizeof(double)*nathr ) ;
  }
#endif

  /* load the geometry variables */

  nx = DSET_NX(xinset->mask_dset) ;
  ny = DSET_NY(xinset->mask_dset) ;
  nz = DSET_NZ(xinset->mask_dset) ;
  dx = fabsf(DSET_DX(xinset->mask_dset)) ;
  dy = fabsf(DSET_DY(xinset->mask_dset)) ;
  dz = fabsf(DSET_DZ(xinset->mask_dset)) ;

  /* copy some variables because xinset was a late addition to the code */

  mask_dset  = xinset->mask_dset ;
  imtemplate = DSET_BRICK(xinset->mask_dset,0) ;  /* sample 3D volume */
  mask_ngood = xinset->ngood ;
  mask_vol   = xinset->mask_vol ;
  if( verb > 1 )
    INFO_message("mask has %d points",mask_ngood) ;

  nxy = nx*ny ; nxyz = nxy*nz ; nxyz1 = nxyz - nxy ;
  if( nxyz < 1024 )
    ERROR_exit("Only %d voxels in simulation?! Need at least 1024.",nxyz) ;

  /* make a list of the i,j,k coordinates of each point in the mask */

  { int pp,qq , xx,yy,zz ;
    ipmask = xinset->ipmask = (ind_t *)malloc(sizeof(ind_t)*mask_ngood) ;
    jpmask = xinset->jpmask = (ind_t *)malloc(sizeof(ind_t)*mask_ngood) ;
    kpmask = xinset->kpmask = (ind_t *)malloc(sizeof(ind_t)*mask_ngood) ;
    ijkmask= xinset->ijkmask ;
    for( pp=qq=0 ; qq < nxyz ; qq++ ){
      if( INMASK(qq) ){
        IJK_TO_THREE(qq,xx,yy,zz,nx,nxy) ;
        ipmask[pp] = (ind_t)xx; jpmask[pp] = (ind_t)yy; kpmask[pp] = (ind_t)zz;
        pp++ ;
      }
    }
    ijk_to_vec = xinset->ijk_to_vec = (int *)malloc(sizeof(int)*nxyz) ;
    for( pp=qq=0 ; qq < nxyz ; qq++ )
      ijk_to_vec[qq] = INMASK(qq) ? pp++ : -1 ;
  }

  /*-- z-score thresholds for the various p-values --*/

  zthr_1sid = (float *)malloc(sizeof(float)*npthr) ;
  zthr_2sid = (float *)malloc(sizeof(float)*npthr) ;
  for( ii=0 ; ii < npthr ; ii++ ){
    zthr_1sid[ii] = (float)zthresh(     pthr[ii] ) ;
    zthr_2sid[ii] = (float)zthresh( 0.5*pthr[ii] ) ;
  }
  zthr_used = (nnsid==1) ? zthr_1sid : zthr_2sid ;

  EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Load image (realization) for thresholding and clustering into fim array */

void generate_image( float *fim , int ind )
{
  load_from_Xdataset( xinset , ind , fim ) ;
  return ;
}

/*---------------------------------------------------------------------------*/
/* Global cluster collection:
     Xclustar_g[icase][ipthr][iter] = cluster array at iteration iter,
     for threshold ipthr, for case icase. The basic array created in main().
*//*-------------------------------------------------------------------------*/

static Xcluster_array ****Xclustar_g ;

/* Xclust_tot[icase][ipthr][nn] =
     consolidated clusters from all sims,
     for nn=0 .. nclust_tot[icase][ipthr]-1
   Usually nclust_tot[][] is less than niter, since not all
   iterations will have clusters in a given case/thresh combo */

static int          nclust_max ;
static int        **nclust_tot ; /* [ncase][npthr] */
static Xcluster ****Xclust_tot ; /* [ncase][npthr][nclust_tot[icase][ipthr]] */

/*---------------------------------------------------------------------------*/
/* Get a NN{nn}_{ss}sided cluster array at a particular thresh/sub-case
   (icase), at a particular iteration (iter), and save it into the global
   cluster collection Xclustar_g.  nn=1 or 2 or 3;  ss=1 or 2.
*//*-------------------------------------------------------------------------*/

void gather_clusters( int icase, int ipthr,
                      float *fim, MRI_IMAGE *tfim, int nn,int ss, int iter )
{
  register int ii ; register float thr ; float *tfar = MRI_FLOAT_PTR(tfim) ;

  thr = fabsf(zthr_used[ipthr]) ;

#if 1              /** older code **/
  if( ss == 1 ){
    if( iter%2 == 0 ){
      for( ii=0 ; ii < nxyz ; ii++ )
        tfar[ii] = (fim[ii] >= thr) ? fim[ii] : 0.0f ;
    } else {
      for( ii=0 ; ii < nxyz ; ii++ )
        tfar[ii] = (fim[ii] <= -thr) ? fim[ii] : 0.0f ;
    }
  } else {
    for( ii=0 ; ii < nxyz ; ii++ )
      tfar[ii] = (fabsf(fim[ii]) >= thr) ? fim[ii] : 0.0f ;
  }
  Xclustar_g[icase][ipthr][iter] = find_Xcluster_array( tfim,nn, 0,NULL,NULL,NULL ) ;

#else              /** new code: 1-sided uses both pos and neg [doesn't work] **/
  if( ss == 1 ){
    Xcluster_array *pcar , *ncar ;
    for( ii=0 ; ii < nxyz ; ii++ )
      tfar[ii] = (fim[ii] >= thr) ? fim[ii] : 0.0f ;
    pcar = find_Xcluster_array( tfim,nn, 0,NULL,NULL,NULL ) ;
    for( ii=0 ; ii < nxyz ; ii++ )
      tfar[ii] = (fim[ii] <= -thr) ? fim[ii] : 0.0f ;
    ncar = find_Xcluster_array( tfim,nn, 0,NULL,NULL,NULL ) ;
    MERGE_Xcluster_arrays(pcar,ncar) ; /* ncar is deleted */
    Xclustar_g[icase][ipthr][iter] = pcar ;
  } else {
    for( ii=0 ; ii < nxyz ; ii++ )
      tfar[ii] = (fabsf(fim[ii]) >= thr) ? fim[ii] : 0.0f ;
    Xclustar_g[icase][ipthr][iter] = find_Xcluster_array( tfim,nn, 0,NULL,NULL,NULL ) ;
  }
#endif

  return ;
}

/*---------------------------------------------------------------------------*/
/* Dilate a cluster by 1 voxel [don't change FOM values] */
/* DALL is defined in mri_threshX.c [included way above] */

#define DILATE_point(pqr)                                         \
 do{ int i,j,k , npt=xc->npt;                                     \
     IJK_TO_THREE(pqr,i,j,k,nx,nxy) ;                             \
     if( npt == xc->nall ){                                       \
       xc->nall += DALL + xc->nall/4 ;                            \
       xc->ip = (ind_t *)realloc(xc->ip,sizeof(ind_t)*xc->nall) ; \
       xc->jp = (ind_t *)realloc(xc->jp,sizeof(ind_t)*xc->nall) ; \
       xc->kp = (ind_t *)realloc(xc->kp,sizeof(ind_t)*xc->nall) ; \
       xc->ijk= (int *)  realloc(xc->ijk,sizeof(int) *xc->nall) ; \
     }                                                            \
     xc->ip[npt] = i; xc->jp[npt] = j; xc->kp[npt] = k;           \
     xc->ijk[npt]= pqr ; xc->npt++ ;                              \
 } while(0)

/*---------------------------------------------------------------------------*/
/* per-thread work arrays for dilation;
   the highest level arrays are created in main(),
   and the sub-arrays are never free()-ed, to avoid OpenMP degradation */

static int   **dilg_ijk=NULL ;
static int   *ndilg=NULL ;

void dilate_Xcluster( Xcluster *xc , int nnlev , int ithr )
{
   int npt,ntry,ii,jj , nx1=nx-1,ny1=ny-1,nz1=nz-1 ;
   int *ijkar , xx,yy,zz , ijk ;

   int pxpy=+1+nx , pxmy=+1-nx , mxpy=nx-1  , mxmy=-1-nx  ;  /* for NN2 */
   int pxpz=+1+nxy, pxmz=+1-nxy, mxpz=nxy-1 , mxmz=-1-nxy ;
   int pypz=nx+nxy, pymz=nx-nxy, mypz=nxy-nx, mymz=-nx-nxy;
   int pxpypz = +1+nx+nxy , pxpymz = +1+nx-nxy ,             /* for NN3 */
       pxmypz = +1-nx+nxy , pxmymz = +1-nx-nxy ,
       mxpypz = -1+nx+nxy , mxpymz = -1+nx-nxy ,
       mxmypz = -1-nx+nxy , mxmymz = -1-nx-nxy  ;

   if( xc == NULL || xc->npt == 0 ) return ; /* bad inputs */

   npt = xc->npt ;

   /* create the list of candidate points for dilation
      (most will not survive the second step: already in cluster) */

   ntry = npt * (  (nnlev<=1) ? 6 : (nnlev==2) ? 18 : 26 ) ;

   if( ntry > ndilg[ithr] ){  /* make it bigger if need be */
     dilg_ijk[ithr] = (int *)realloc(dilg_ijk[ithr],sizeof(int)*(ntry+64)) ;
     ndilg   [ithr] = ntry+64 ;
   }

   ijkar = dilg_ijk[ithr] ;

   for( jj=ii=0 ; ii < npt ; ii++ ){           /* candidates = neighbors */
     xx = xc->ip[ii] ; yy = xc->jp[ii] ; zz = xc->kp[ii] ; /* current pt */

     /* no dilation from outer edges of 3D grid */
     if( xx==0 || xx==nx1 || yy==0 || yy==ny1 || zz==0 || zz==nz1 ) continue ;

     ijk = xx + nx*yy + nxy*zz ;                     /* 3D grid index */

     ijkar[jj++] = ijk+1   ; ijkar[jj++] = ijk-1   ;  /* NN1 dilation */
     ijkar[jj++] = ijk+nx  ; ijkar[jj++] = ijk-nx  ;    /* candidates */
     ijkar[jj++] = ijk+nxy ; ijkar[jj++] = ijk-nxy ;
     if( nnlev >= 2 ){
       ijkar[jj++] = ijk+pxpy ; ijkar[jj++] = ijk+mxpy ;       /* NN2 */
       ijkar[jj++] = ijk+pxmy ; ijkar[jj++] = ijk+mxmy ;
       ijkar[jj++] = ijk+pxpz ; ijkar[jj++] = ijk+mxpz ;
       ijkar[jj++] = ijk+pxmz ; ijkar[jj++] = ijk+mxmz ;
       ijkar[jj++] = ijk+pypz ; ijkar[jj++] = ijk+mypz ;
       ijkar[jj++] = ijk+pymz ; ijkar[jj++] = ijk+mymz ;
       if( nnlev >=3 ){
         ijkar[jj++] = ijk+pxpypz ; ijkar[jj++] = ijk+pxpymz ; /* NN3 */
         ijkar[jj++] = ijk+pxmypz ; ijkar[jj++] = ijk+pxmymz ;
         ijkar[jj++] = ijk+mxpypz ; ijkar[jj++] = ijk+mxpymz ;
         ijkar[jj++] = ijk+mxmypz ; ijkar[jj++] = ijk+mxmymz ;
     }}
   }

   ntry = jj ;
   qsort_int(ntry,ijkar) ;  /* sorting makes it easy to skip duplicates */

   /* for each new point:
        compare to all other points in the cluster
        if it isn't any of them, add it to the cluster */

   for( jj=0 ; jj < ntry ; jj++ ){
     ijk = ijkar[jj] ; if( !INMASK(ijk) ) continue ;   /* not a useful pt */
     if( jj > 0 && ijk==ijkar[jj-1] ) continue ;          /* is duplicate */
     for( ii=0 ; ii < xc->npt ; ii++ ){    /* check if already in cluster */
       if( ijk == xc->ijk[ii] ) break ;          /* oops, already present */
     }
     if( ii == xc->npt ) DILATE_point(ijk) ;         /* add it in, Babee! */
   } /* note: xc->npt will expand as the cluster grows */

   return ;  /* cluster is all dilated now */
}

#undef DILATE_point

/*---------------------------------------------------------------------------*/
/* Vector struct.
   One for each point in the mask, to hold the FOM values found at that point.
*//*-------------------------------------------------------------------------*/

typedef struct {
  ind_t ip,jp,kp ; int ijk ;  /* where is this pt? */
  int npt , nall ;             /* array dimensions */
  float *far ;              /* array of FOM values */
  float val ;                 /* summary FOM value */
} Xvector ;

#define CREATE_Xvector(xv,siz)                         \
 do{ xv = (Xvector *)malloc(sizeof(Xvector)) ;         \
     xv->npt = 0 ; xv->nall = (siz) ;                  \
     xv->far = (float *)malloc(sizeof(float)*(siz)) ;  \
 } while(0)

#define DESTROY_Xvector(xv)                            \
 do{ free(xv->far); free(xv); } while(0)

#define ADDTO_Xvector(xv,val)                                        \
 do{ if( xv->npt >= xv->nall ){                                      \
       xv->nall = xv->npt + DALL + xv->nall/4 ;                      \
       xv->far  = (float *)realloc(xv->far,sizeof(float)*xv->nall) ; \
     }                                                               \
     xv->far[xv->npt++] = (val) ;                                    \
 } while(0)

/* global FOM vector arrays -- created in main() */

/* these are workspaces (for hpow=0,1,2) */

static Xvector **fomvec0   = NULL ;  /* [mask_ngood] */
static Xvector **fomvec1   = NULL ;  /* [mask_ngood] */
static Xvector **fomvec2   = NULL ;  /* [mask_ngood] */

/* these are sorted FOM vectors for each case, each pthr, each point */

static Xvector ****fomsort0 = NULL ;  /* [ncase][npthr][mask_ngood] */
static Xvector ****fomsort1 = NULL ;  /* [ncase][npthr][mask_ngood] */
static Xvector ****fomsort2 = NULL ;  /* [ncase][npthr][mask_ngood] */

/*---------------------------------------------------------------------------*/
/* Process the clusters to FOM vectors for a range of 3D indexes,
   for a given p-value thresh/subcase index icase.
     This function is intended to be use in multiple threads, operating
     over differing ijkbot..ijktop blocks:
       A given fomvec will not be in thread conflict, since it is linked
       to a given voxel in space (ijk), so there is no potential problem
       when updating that fomvec, or the cluster's ijk entry.
*//*-------------------------------------------------------------------------*/

void process_clusters_to_Xvectors( int ijkbot, int ijktop , int icase, int ipthr )
{
   Xcluster **xcar = Xclust_tot[icase][ipthr] ;  /* clusters we load from */
   int        ncar = nclust_tot[icase][ipthr] ;  /* number of cluster arrays */
   Xcluster *xc ;
   int cc , pp,npt , ijk,vin ;
   const int nx1=nx-1,ny1=ny-1,nz1=nz-1 ;

/* macro to add FOM values to the fomvecH vectors for voxel #iii */

#define PROCESS(iii)                                \
 do{ if( iii >= ijkbot && ijk <= ijktop ){          \
       vin = ijk_to_vec[iii] ;                      \
       if( vin >= 0 ){                              \
         ADDTO_Xvector(fomvec0[vin],xc->fomh[0]) ;  \
         ADDTO_Xvector(fomvec1[vin],xc->fomh[1]) ;  \
         ADDTO_Xvector(fomvec2[vin],xc->fomh[2]) ;  \
       }                                            \
   }} while(0)

   for( cc=0 ; cc < ncar ; cc++ ){                     /* loop over clusters */
     xc = xcar[cc] ; if( xc == NULL ) continue ;
     if( xc->ijkmin > ijktop ) continue ;
     if( xc->ijkmax < ijkbot ) continue ;
     npt = xc->npt ;                          /* number of points in cluster */
     for( pp=0 ; pp < npt ; pp++ ){          /* loop over pts inside cluster */
       ijk = xc->ijk[pp] ;                            /* index of pt in grid */
       PROCESS(ijk) ;   /* if pt in region and is good, add FOM to this list */

#ifdef LARGER_FOOTPRINT /* not enabled -- dilation is used instead */
       { int iqq ;
         if( xc->ip[pp] < nx1 ){ iqq = ijk+1   ; PROCESS(iqq) ; }
         if( xc->ip[pp] > 0   ){ iqq = ijk-1   ; PROCESS(iqq) ; }
         if( xc->jp[pp] < ny1 ){ iqq = ijk+nx  ; PROCESS(iqq) ; }
         if( xc->jp[pp] > 0   ){ iqq = ijk-nx  ; PROCESS(iqq) ; }
         if( xc->kp[pp] < nz1 ){ iqq = ijk+nxy ; PROCESS(iqq) ; }
         if( xc->kp[pp] > 0   ){ iqq = ijk-nxy ; PROCESS(iqq) ; }
       }
#endif

     } /* end of loop over pts inside cluster */
   } /* end of loop over clusters */

#undef PROCESS

   return ;
}

/*--------------------------------------------------------------------------*/
/* get median of number of counts in voxel fomvec's touched by this cluster */
/* that is, a measure of how often the voxels in this    */
/* cluster are 'hit' by other clusters in the simulation */

static int    *ncount_g ;  /* workspaces initialized in main() */
static float **fcount_g ;

int get_Xcluster_nbcount( Xcluster *xc , int ithr )
{
   int ii ; float *fc ;

   /* trivial cases */

   if( xc == NULL || xc->npt == 0 ) return 0 ;
   if( xc->npt == 1 ){
     xc->nbcount = fomvec0[ijk_to_vec[xc->ijk[0]]]->npt ;
     return xc->nbcount ;
   }

   /* size of this cluster is bigger than space we've set aside? */

   if( ncount_g[ithr] < xc->npt ){
     ncount_g[ithr] = xc->npt + DALL ;
     fcount_g[ithr] = (float *)realloc(fcount_g[ithr],
                                       sizeof(float)*ncount_g[ithr]) ;
   }

   /* assemble the count of FOM vector hits for each voxel in this cluster */

   fc = fcount_g[ithr] ;  /* thread-specific temp array */
   for( ii=0 ; ii < xc->npt ; ii++ )
     fc[ii] = fomvec0[ijk_to_vec[xc->ijk[ii]]]->npt ;  /* count */

   xc->nbcount = (int)qmed_float(xc->npt,fc) ;  /* find the median count */
   return xc->nbcount ;
}

/*---------------------------------------------------------------------------*/
/* find the intermediate value of x at alphat (t=target),
   given x at alpha0 and alpha1, where alpha is a tail (1-CDF) probability,
   assumed to be of the extreme value form
      alpha(x) = 1 - exp( -exp(-(x-p)/q) )
      log(-log(1-alpha(x))) = -x/q + p/q
   Define a(alpha) = log(-log(1-alpha)), so
      a(x) = -x/q + p/q  or  x(a) = p - q*a
   The alpha values are between 0 and 1; alphat is between alpha0 and alpha1.
*//*-------------------------------------------------------------------------*/

static float inverse_interp_extreme( float alpha0, float alpha1, float alphat,
                                     float x0    , float x1                   )
{
   float a0,a1,at , xx ;

   if( (alphat-alpha0)*(alphat-alpha1) < 0.0f ){ /* alphat is bracketed */
     a0 = logf(-logf(1.0f-alpha0)) ;
     a1 = logf(-logf(1.0f-alpha1)) ;
     at = logf(-logf(1.0f-alphat)) ;
     xx = (x0 + (x1-x0)/(a1-a0)*(at-a0)) ;  /* linear interp in a-space */
   } else if( fabsf(alphat-alpha0) <= fabsf(alphat-alpha1) ){
     xx = x0 ;  /* alphat is closer to alpha0 */
   } else {
     xx = x1 ;  /* alphat is closer to alpha1 */
   }
   return xx ;
}

/*===========================================================================*/
/* this is too complicated, and should be function-ized :( */
/* which will never happen :) */
/*===========================================================================*/

int main( int argc , char *argv[] )
{
   int qpthr , ii,xx,yy,zz,ijk , dijk , qcase ;
   int ndilstep , ndilated[4] , ndilsum , ndiltot ;
   int count_targ100 , count_targ80, count_targ60 ;
   THD_3dim_dataset *qset=NULL ;
   float *qar=NULL , ***gthresh0=NULL , ***gthresh1=NULL, ***gthresh2=NULL ;
   char qpr[1024] ;
   MRI_IMAGE *cim0  =NULL , *cim1  =NULL , *cim2  =NULL ;
   MRI_IMARR **cimar0=NULL , **cimar1=NULL , **cimar2=NULL ;
   float     ***car0 =NULL , ***car1 =NULL , ***car2 =NULL , **carHP ;
   float *farar=NULL ;
   int nfomkeep , nfar , itrac , ithresh , hp,ibr, ithresh_list[MAXITE] ;
   float tfrac=0.0006f, farperc=0.0f,farcut=0.0f, tfracold,farpercold,ttemp, farlast ;
   int ifarp ;
   float *gthrout , *gthrH ;
   int ntfp=0 , ntfp_all=0 ; float *tfs=NULL , *fps=NULL ;

   /*----- help me if you can (I'm feeling down) -----*/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf("\n"
       "This program takes as input random field simulations\n"
       "(e.g., from 3dttest++) and does the ETAC processing to\n"
       "find cluster figure of merit (FOM) thresholds that are\n"
       "equitable (AKA balanced) across\n"
       "  * voxel-wise p-values (-pthr option)\n"
       "  * blurring cases      (-ncase option)\n"
       "  * H power values      (-hpow option) -- probably not useful\n"
       "as well as being balanced across space to produce\n"
       "a False Positive Rate (FPR) that is approximately the\n"
       "same for each location and for each sub-case listed\n"
       "above. The usual goal is a global FPR of 5%%.\n"
       "\n"
       "* This program can be slow and consume a LOT of memory!\n"
       "  (And I mean a BIG LOT, not a small lot.)\n"
       "\n"
       "* The output is a set of multi-threshold (*.mthresh.*.nii)\n"
       "  files -- one for each of the -ncase inputs.\n"
       "\n"
       "* These files can be used via program 3dMultiThresh\n"
       "  to produce an 'activation' mask.\n"
       "\n"
       "* 3dXClustSim is intended to be used from 3dttest++\n"
       "  (via its '-ETAC' option) or some other script.\n"
       "\n"
       "* It is not intended to be run directly by any but the most\n"
       "  knowledgeable and astute users. Which is why this help is so terse.\n"
     ) ;

     printf("\n"
       "--------\n"
       "OPTIONS:\n"
       "--------\n"
       "\n"
       " -inset       mask sdata ... {MANDATORY} [from 3dtoXdataset or 3dttest++]\n"
       " -insdat                     Data files are in the '.sdat' format.\n"
       "\n"
       " -NN          1 or 2 or 3    [-NN1 or -NN2 or -NN3 will work; default = 2]\n"
       " -sid         1 or 2         [-1sid or -2sid will work; default = 2]\n"
       " -hpow        0 1 2          [or some subset of these; default = 2]\n"
       "\n"
       " -ncase       N lab1 .. labN [multiple processing cases; e.g., blurs]\n"
       "                             [default = 1 A]\n"
       "                             [example = 4 b04 b06 b08 b10]\n"
       "\n"
       " -pthr        list of values [default = 0.0100 0.0056 0.0031 0.0018 0.0010]\n"
       "                             [equiv z1= 2.326  2.536  2.731  2.911  3.090 ]\n"
       "                             [equiv z2= 2.576  2.770  2.958  3.121  3.291 ]\n"
       "\n"
       " -FPR ff      set global FPR goal to ff%%, where ff is an integer\n"
       "              from 2 to 9 (inclusive). Default value is 5.\n"
       "\n"
       " -multiFPR    compute results for multiple FPR goals (2%%, 3%%, ... 9%%)\n"
       "\n"
       " -minclust M  don't allow clusters smaller than M voxels [default M=5]\n"
       "\n"
       " -local       do the 'local' (voxelwise) ETAC computations\n"
       " -global      do the 'global' (volumewise) ETAC computations\n"
       " -nolocal     don't do the 'local'\n"
       " -noglobal    don't do the 'global'\n"
       "\n"
       " -prefix      something useful\n"
       " -verb        be more verbose\n"
       " -quiet       silentium est aureum\n"

#if 0 /* disabled */
       " -FOMcount    turn on FOMcount output\n"
       " -FARvox      turn on FARvox output\n"
#endif

       "\n"
       "**-----------------------------------------------------------\n"
       "** Authored by Lamont Cranston, also known as ... The Shadow.\n"
       "**-----------------------------------------------------------\n"
       "\n"
     ) ;
     exit(0) ;
   }

   (void)COX_clock_time() ; /* initialize the timer */

   /* startup paperwork */

   mainENTRY("3dXClustSim") ; machdep() ;
   AFNI_logger("3dXClustSim",argc,argv);
   PRINT_VERSION("3dXClustSim"); AUTHOR("Lamont Cranston") ;

#ifdef DEBUG_MEM
   enable_mcw_malloc() ;
#ifdef USE_OMP
   omp_set_num_threads(1) ; /* will be slow */
#endif
#endif

   /*----- load command line options -----*/

   get_options(argc,argv) ;

#ifdef DEBUG_MEM
   if( verb < 2 ) pause_mcw_malloc() ;
#endif

   /*----- get the number of threads -----------------------------------*/

   nthr = 1 ;
#ifdef USE_OMP
#pragma omp parallel /*------------ start parallel section ----------*/
 {
#pragma omp master
   { nthr = omp_get_num_threads() ; }
 }                   /*---------- end parallel section ----------*/
#endif

   if( verb ){
     if( nthr > 1 )
       INFO_message("3dXClustSim: Using %d OpenMP threads",nthr) ;
     else
       INFO_message("3dXClustSim: Using 1 thread -- this will be slow :-(") ;
   }

   /* setup some arrays in mri_threshX.c */

   mri_multi_threshold_setup() ;

   /*--- initialize the global cluster arrays ---*/

   /*--- Xclustar_g[qcase][qpthr][iter]
           = array of clusters for case qcase,
             at p-value pthr[qpthr],
             for realization iter             ---*/

   Xclustar_g = (Xcluster_array ****)malloc(sizeof(Xcluster_array ***)*ncase) ;
   for( qcase=0 ; qcase < ncase ; qcase++ ){
     Xclustar_g[qcase] = (Xcluster_array ***)malloc(sizeof(Xcluster_array **)*npthr) ;
     for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
       Xclustar_g[qcase][qpthr] = (Xcluster_array **)malloc(sizeof(Xcluster_array *)*niter) ;
     }
   }

   MEMORY_CHECK("startup") ;

   /*===================================================================*/
   /*--- STEP 1a: loop over realizations to load up Xclustar_g[][][] ---*/

   if( verb )
     INFO_message("STEP 1a: start %d-sided clustering with NN=%d",nnsid,nnlev) ;
   if( verb > 1 ) ININFO_message("  Elapsed time = %.1f s",COX_clock_time()) ;

 AFNI_OMP_START ;      /*------------ start parallel section ----------*/
#pragma omp parallel
 { DECLARE_ithr ;
   int iter , ipthr , icase , isim , isub ;
   MRI_IMAGE *fim , *tfim ; float *far , *tfar ;

   /* initialize thread-specific workspace images */

#pragma omp critical
   { fim  = mri_new_conforming( imtemplate , MRI_float ) ;
     tfim = mri_new_conforming( fim        , MRI_float ) ; }

   far  = MRI_FLOAT_PTR(fim) ;
   tfar = MRI_FLOAT_PTR(tfim) ;

   /* clusterize each volume at each p-value threshold */

/* #pragma omp for schedule(dynamic,500) */
#pragma omp for
   for( isim=0; isim < nsim ; isim++ ){ /* loop over realizations */
     generate_image( far , isim ) ;
     icase = isim / niter ; iter = isim % niter ;
     for( ipthr=0 ; ipthr < npthr ; ipthr++ ){  /* over thresholds */
       /* creates Xclustar_g[icase][ipthr][iter] */
       gather_clusters( icase, ipthr , far, tfim, nnlev,nnsid, iter ) ;
     }
     if( verb > 2 && nthr == 1 && isim%1000 == 666 ) fprintf(stderr,".") ;
     if( 0 && verb > 2 ){
#pragma omp critical
       { char mmm[32] ; sprintf(mmm,"isim=%d",isim) ; MEMORY_CHECK(mmm) ; }
     }
   }

   /* delete thread-specific workspace for this processing block */

#pragma omp critical
   { mri_free(fim) ; mri_free(tfim) ; }
 }
 AFNI_OMP_END ;       /*---------- end parallel section ----------*/

   MEMORY_CHECK("after clustering") ;

   /* don't need xinset's data for the nonce (maybe) */
   if( !do_global_etac ){
     if( verb > 1 ) ININFO_message("un-mapping input datasets") ;
     unmap_Xdataset(xinset) ;
   }

   /*=====================================================================*/
   /* STEP 1b: count num clusters total, and merge them into one big list */

   { int qter, qq,pp ; Xcluster_array *xcar ;

     /* create the _tot global arrays for the merged cluster lists */

     nclust_tot = (int **)malloc(sizeof(int *)*ncase) ;
     for( qcase=0 ; qcase < ncase ; qcase++ )
       nclust_tot[qcase] = (int *)malloc(sizeof(int)*npthr) ;

     Xclust_tot = (Xcluster ****)malloc(sizeof(Xcluster_array ***)*ncase) ;
     for( qcase=0 ; qcase < ncase ; qcase++ )
       Xclust_tot[qcase] = (Xcluster ***)malloc(sizeof(Xcluster **)*npthr) ;

     nclust_max = 0 ;  /* keep track of the largest situation */

     if( verb )
       ININFO_message("STEP 1b: merge cluster lists") ;
     if( verb > 1 ) ININFO_message("  Elapsed time = %.1f s",COX_clock_time()) ;

     for( qcase=0 ; qcase < ncase ; qcase++ ){
       for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
         nclust_tot[qcase][qpthr] = 0 ;
         for( qter=0 ; qter < niter ; qter++ ){   /* count total clusters */
           xcar = Xclustar_g[qcase][qpthr][qter] ;
           if( xcar != NULL ) nclust_tot[qcase][qpthr] += xcar->nclu ;
         }
         /* max number of clusters across all p-value thresh, all cases */
         if( nclust_tot[qcase][qpthr] > nclust_max ) nclust_max = nclust_tot[qcase][qpthr] ;
         if( verb > 1 )
           ININFO_message("     %d total clusters :: Case %s pthr=%.5f",
                          nclust_tot[qcase][qpthr], lcase[qcase] , pthr[qpthr] );

         /* assemble all clusters at this case and p-value thresh into one array */

         Xclust_tot[qcase][qpthr] = (Xcluster **)malloc(sizeof(Xcluster *)*nclust_tot[qcase][qpthr]) ;

         for( pp=qter=0 ; qter < niter ; qter++ ){           /* loop over realizations */
           xcar = Xclustar_g[qcase][qpthr][qter] ;
           if( xcar == NULL ) continue ;
           for( qq=0 ; qq < xcar->nclu ; qq++ ){        /* over clusts frm realization */
             Xclust_tot[qcase][qpthr][pp++] = xcar->xclu[qq] ;  /* add to output array */
           }
           free(xcar->xclu) ; free(xcar) ;
         }
         free(Xclustar_g[qcase][qpthr]) ;
       }
       free(Xclustar_g[qcase]) ;
     }
     free(Xclustar_g) ; Xclustar_g = NULL ; /* not needed no more */
   }

   MEMORY_CHECK("after counting") ;

   /*=========================================================================*/
   /*--- STEP 1c: find the global distributions and min thresholds -----------*/

#if 0
# define GTHRESH_FAC 0.799999f /* factor for method 1 */
# define GTHRESH_THA 0.008888f /* how far into clust table: method 1 (per %) */
# define GTHRESH_THB 0.013579f /* how far into clust table: method 2 (per %) */
#else
# define GTHRESH_FAC 0.135799f /* factor for method 1 */
# define GTHRESH_THA 0.011111f /* how far into clust table: method 1 (per %) */
# define GTHRESH_THB 0.044444f /* how far into clust table: method 2 (per %) */
#endif

   if( do_local_etac ){                  /* not needed for global ETAC */
     int nfom,jj,nfff; Xcluster **xcc;
     float a0,a1,f0,f1,fta,ftb , fmax , fg ;
     float *fomg0, *fomg1, *fomg2 ;

     fomg0 = calloc(sizeof(float),nclust_max) ; /* workspaces */
     fomg1 = calloc(sizeof(float),nclust_max) ; /* for this  */
     fomg2 = calloc(sizeof(float),nclust_max) ; /* section  */

     if( verb )
       ININFO_message("STEP 1c: compute minimum thresholds") ;
     if( verb > 1 ) ININFO_message("  Elapsed time = %.1f s",COX_clock_time()) ;

     /* arrays for global threshold:
          [ifarp=FPR goal][qcase=blurring][ipthr=p-value] */

     gthresh0 = (float ***)malloc(sizeof(float **)*numfarp) ;
     gthresh1 = (float ***)malloc(sizeof(float **)*numfarp) ;
     gthresh2 = (float ***)malloc(sizeof(float **)*numfarp) ;
     for( ifarp=0 ; ifarp < numfarp ; ifarp++ ){
       gthresh0[ifarp] = (float **)malloc(sizeof(float *)*ncase) ;
       gthresh1[ifarp] = (float **)malloc(sizeof(float *)*ncase) ;
       gthresh2[ifarp] = (float **)malloc(sizeof(float *)*ncase) ;
       for( qcase=0 ; qcase < ncase ; qcase++ ){
         gthresh0[ifarp][qcase] = (float *)calloc(sizeof(float),npthr) ; /* saved */
         gthresh1[ifarp][qcase] = (float *)calloc(sizeof(float),npthr) ; /* global */
         gthresh2[ifarp][qcase] = (float *)calloc(sizeof(float),npthr) ; /* thresholds */
       }
     }

     for( qcase=0 ; qcase < ncase ; qcase++ ){
       for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
         /* load all FOM values from all clusters for this case/pthr combo */
         xcc = Xclust_tot[qcase][qpthr] ;
         for( nfom=ii=0 ; ii < nclust_tot[qcase][qpthr] ; ii++ ){
           if( xcc[ii] != NULL ){
             fomg0[nfom] = xcc[ii]->fomh[0] ;
             fomg1[nfom] = xcc[ii]->fomh[1] ;
             fomg2[nfom] = xcc[ii]->fomh[2] ; nfom++ ;
           }
         }
         if( nfom < 50 ) continue ;      /* should never happen */
         nfff = niter ;
         if( nfff > nfom ) nfff = nfom ; /* very very unlikely */

         fmax = AFNI_numenv("AFNI_XCLUSTSIM_FMAX") ;
         if( fmax <= 0.01f || fmax > 1.0f ) fmax = 0.888f ;

         /* global threshold computed from tail of FOM distribution */

         qsort_float_rev( nfom, fomg0 ) ;      /* hpow=0 */
         qsort_float_rev( nfom, fomg1 ) ;      /* hpow=1 */
         qsort_float_rev( nfom, fomg2 ) ;      /* hpow=2 */

         for( ifarp=0 ; ifarp < numfarp ; ifarp++ ){
           fg  = farplist[ifarp] ;                  /* FPR goal in % */

           jj  = (int)rintf(GTHRESH_THA*nfff*fg) ;  /* method 1 */
           if( jj >= nfom ) jj = nfom-1 ;           /* should be impossible */
           fta = GTHRESH_FAC*fomg0[jj] ;
           jj  = (int)rintf(GTHRESH_THB*nfff*fg) ;  /* method 2 */
           if( jj >= nfom ) jj = nfom-1 ;           /* should be impossible */
           ftb = fomg0[jj] ;
           gthresh0[ifarp][qcase][qpthr] = (int)(fmax*MAX(fta,ftb)+(1.0f-fmax)*MIN(fta,ftb)) ;
           if( verb > 1 && do_hpow0 ){
             ININFO_message("     min threshold %.1f :: Case %s pthr=%.5f h=0 fgoal=%.1f%%",
                            gthresh0[ifarp][qcase][qpthr],lcase[qcase],pthr[qpthr],fg) ;
           }

           jj  = (int)rintf(GTHRESH_THA*nfff*fg) ;
           if( jj >= nfom ) jj = nfom-1 ;           /* should be impossible */
           fta = GTHRESH_FAC*fomg1[jj] ;
           jj  = (int)rintf(GTHRESH_THB*nfff*fg) ;
           if( jj >= nfom ) jj = nfom-1 ;           /* should be impossible */
           ftb = fomg1[jj] ;
           gthresh1[ifarp][qcase][qpthr] = (fmax*MAX(fta,ftb)+(1.0f-fmax)*MIN(fta,ftb)) ;
           if( verb > 1 && do_hpow1 ){
             ININFO_message("     min threshold %.1f :: Case %s pthr=%.5f h=1 fgoal=%.1f%%",
                            gthresh1[ifarp][qcase][qpthr],lcase[qcase],pthr[qpthr],fg) ;
           }

           jj  = (int)rintf(GTHRESH_THA*nfff*fg) ;
           if( jj >= nfom ) jj = nfom-1 ;           /* should be impossible */
           fta = GTHRESH_FAC*fomg2[jj] ;
           jj  = (int)rintf(GTHRESH_THB*nfff*fg) ;
           if( jj >= nfom ) jj = nfom-1 ;           /* should be impossible */
           ftb = fomg2[jj] ;
           gthresh2[ifarp][qcase][qpthr] = (fmax*MAX(fta,ftb)+(1.0f-fmax)*MIN(fta,ftb)) ;
           if( verb > 1 && do_hpow2 ){
             ININFO_message("     min threshold %.1f :: Case %s pthr=%.5f h=2 fgoal=%.1f%%",
                            gthresh2[ifarp][qcase][qpthr],lcase[qcase],pthr[qpthr],fg) ;
           }
         } /* end of loop over FPR goals (farp) */
       } /* end loop over thresholds */
     } /* end of loop over cases */

     free(fomg0) ; free(fomg1) ; free(fomg2) ;
   }

   MEMORY_CHECK("after globalizing") ;

   /*==========================================================================*/
   /*--- STEP 1d: do ETAC globally [Sep 2018] ---------------------------------*/
   /*:::::::::::: much of this code is a rehash of the voxelwise STEP 4, infra */

   if( do_global_etac ){
     float ***fomglob0, ***fomglob1, ***fomglob2 ;
     Xcluster **xcc ; int nfom , **nfomglob ;
     float **fthar0 , **fthar1 , **fthar2 ;

     if( verb )
       ININFO_message("STEP 1d: compute global ETAC thresholds") ;
     if( verb > 1 ) ININFO_message("  Elapsed time = %.1f s",COX_clock_time()) ;

     /*--- create vectors to hold all FOMs ---*/

     nfomkeep = (int)(TOPFRAC*niter) ; /* max number of FOMs to keep */

     fomglob0 = (float ***)malloc(sizeof(float **)*ncase) ;
     fomglob1 = (float ***)malloc(sizeof(float **)*ncase) ;
     fomglob2 = (float ***)malloc(sizeof(float **)*ncase) ;
     nfomglob = (int **)   malloc(sizeof(int *)   *ncase) ;
     fthar0   = (float **) malloc(sizeof(float *) *ncase) ;
     fthar1   = (float **) malloc(sizeof(float *) *ncase) ;
     fthar2   = (float **) malloc(sizeof(float *) *ncase) ;
     for( qcase=0 ; qcase < ncase ; qcase++ ){
       fomglob0[qcase] = (float **)malloc(sizeof(float *)*npthr) ;
       fomglob1[qcase] = (float **)malloc(sizeof(float *)*npthr) ;
       fomglob2[qcase] = (float **)malloc(sizeof(float *)*npthr) ;
       nfomglob[qcase] = (int *)   malloc(sizeof(int)    *npthr) ;
       fthar0[qcase]   = (float *) malloc(sizeof(float)  *npthr) ;
       fthar1[qcase]   = (float *) malloc(sizeof(float)  *npthr) ;
       fthar2[qcase]   = (float *) malloc(sizeof(float)  *npthr) ;
       for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
         fomglob0[qcase][qpthr] = (float *)malloc(sizeof(float)*nclust_max) ;
         fomglob1[qcase][qpthr] = (float *)malloc(sizeof(float)*nclust_max) ;
         fomglob2[qcase][qpthr] = (float *)malloc(sizeof(float)*nclust_max) ;

         xcc = Xclust_tot[qcase][qpthr] ;
         for( nfom=ii=0 ; ii < nclust_tot[qcase][qpthr] ; ii++ ){
           if( xcc[ii] != NULL ){
             fomglob0[qcase][qpthr][nfom] = xcc[ii]->fomh[0] ;
             fomglob1[qcase][qpthr][nfom] = xcc[ii]->fomh[1] ;
             fomglob2[qcase][qpthr][nfom] = xcc[ii]->fomh[2] ; nfom++ ;
           }
         }
#if 0
ININFO_message("-- found %d FOMs for qcase=%d qpthr=%d out of %d clusters",nfom,qcase,qpthr,nclust_tot[qcase][qpthr]) ;
#endif
         if( do_hpow0 ) qsort_float_rev( nfom , fomglob0[qcase][qpthr] ) ;
         if( do_hpow1 ) qsort_float_rev( nfom , fomglob1[qcase][qpthr] ) ;
         if( do_hpow2 ) qsort_float_rev( nfom , fomglob2[qcase][qpthr] ) ;
         if( nfom > nfomkeep ){
           fomglob0[qcase][qpthr] = (float *)realloc( fomglob0[qcase][qpthr],
                                                      sizeof(float)*nfomkeep ) ;
           fomglob1[qcase][qpthr] = (float *)realloc( fomglob1[qcase][qpthr],
                                                      sizeof(float)*nfomkeep ) ;
           fomglob2[qcase][qpthr] = (float *)realloc( fomglob2[qcase][qpthr],
                                                      sizeof(float)*nfomkeep ) ;
         }
         nfomglob[qcase][qpthr] = MIN(nfom,nfomkeep) ; /* how many we have */
#if 0
ININFO_message("  kept %d FOMs for qcase=%d qpthr=%d",nfomglob[qcase][qpthr],qcase,qpthr) ;
#endif
       }
     }

     /*--- multithreshold simulations for global ETAC ---*/

     nit33 = 1.0f/(niter+0.333f) ;

     ntfp = 0 ; ntfp_all = 128 ;
     tfs  = (float *)malloc(sizeof(float)*ntfp_all) ;
     fps  = (float *)malloc(sizeof(float)*ntfp_all) ;

     farlast = farplist[0] ;
     for( ifarp=0 ; ifarp < numfarp ; ifarp++ ){  /* loop over FAR goals */

       farp_goal = farplist[ifarp] ;

       if( verb )
         INFO_message("STEP 1d.%s: adjusting per-voxel FOM thresholds to reach FPR=%.2f%%",
                      abcd[ifarp] , farp_goal) ;
       if( verb > 1 ) ININFO_message("  Elapsed time = %.1f s",COX_clock_time()) ;

       /* tfrac = FOM count fractional threshold;
                  will be adjusted to find the farp_goal FPR goal */

       if( ifarp == 0 ){                                     /* first time thru */
         tfrac = (4.0f+farp_goal)*0.00066f ;
       } else if( ntfp < 2 ){                  /* if only 1 earlier calculation */
         tfrac *= 1.0777f * farp_goal / farlast ;     /* adjust previous result */
       } else {
         int jj, jd ; float adf, mdf ;         /* later: use closest in history */
         mdf = fabsf(fps[0]-farp_goal) ; jd = 0 ;   /* to adjust starting point */
         for( jj=1 ; jj < ntfp ; jj++ ){
           adf = fabsf(fps[jj]-farp_goal) ;
           if( adf < mdf ){ mdf = adf ; jd = jj ; }
         }
         tfrac = 1.0111f * tfs[jd] * farp_goal / fps[jd] ;
       }

       itrac = 0 ;
       farpercold = farperc = 0.0f ; tfracold = tfrac ;

   /*--- Loop back to here with adjusted tfrac ---*/

GARP_LOOPBACK:
     {
       float min_tfrac,a0,a1 ; int nedge,nmin,ntest,npt,jthresh ;
       min_tfrac = 6.0f / niter ; if( min_tfrac > 0.0001f ) min_tfrac = 0.0001f ;

       itrac++ ;                                      /* number of iterations */
       nfar = 0 ;                                          /* total FAR count */
       nedge = nmin = ntest = 0 ;                     /* number of edge cases */

         /* we take ithresh-th largest FOM for each case as the threshold */
#if 0
       ithresh = (int)(tfrac*niter) ;                  /* FOM count threshold */
#else
       ithresh = (int)rintf(tfrac*(niter-0.666f)+0.333f) ;
#endif

         /* Check if trying to re-litigate previous case [Cinco de Mayo 2017] */

       ithresh_list[itrac-1] = ithresh ;
       if( itrac > 3 &&
           (ithresh_list[itrac-2] == ithresh || ithresh_list[itrac-3] == ithresh) ){
         ININFO_message("     #%d: would re-iterate at %g ==> %d ; breaking out",
                        itrac,tfrac,ithresh ) ;
         goto GARP_BREAKOUT ;
       }

       if( verb )
         ININFO_message("     #%d: Testing global thresholds at %g ==> %d",itrac,tfrac,ithresh) ;

       /* compute the global thresholds for each case */

       for( qcase=0 ; qcase < ncase ; qcase++ ){
         for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
           npt = nfomglob[qcase][qpthr] ; /* how many FOM values here */
           jthresh = ithresh ;          /* default index of FOM thresh */

           if( jthresh > (int)(0.777f*npt) ){  /* edge case: */
             jthresh = (int)(0.777f*npt) ;     /* not many FOM values here */
             nedge++ ;
           }
           a0 = ((float)jthresh+0.666f)*nit33 ;
           a1 = a0 + nit33 ;
           fthar0[qcase][qpthr] =
            fthar1[qcase][qpthr] =
             fthar2[qcase][qpthr] = 0.0f ;
           if( do_hpow0 )
             fthar0[qcase][qpthr] = inverse_interp_extreme(
                                      a0,a1,tfrac,
                                      fomglob0[qcase][qpthr][jthresh],
                                      fomglob0[qcase][qpthr][jthresh+1] ) ;
           if( do_hpow1 )
             fthar1[qcase][qpthr] = inverse_interp_extreme(
                                      a0,a1,tfrac,
                                      fomglob1[qcase][qpthr][jthresh],
                                      fomglob1[qcase][qpthr][jthresh+1] ) ;
           if( do_hpow2 )
             fthar2[qcase][qpthr] = inverse_interp_extreme(
                                      a0,a1,tfrac,
                                      fomglob2[qcase][qpthr][jthresh],
                                      fomglob2[qcase][qpthr][jthresh+1] ) ;
       }}

       /*-------------- now loop over realizations and threshold them --------*/
 AFNI_OMP_START ;     /*------------ start parallel section ------------------*/
#pragma omp parallel
 {  DECLARE_ithr ;
    MRI_IMAGE *fim , *tfim ; float *far , *tfar ;
    int iter,npt,iv,jthresh , ipthr,icase , qq,hh ;
    float a0,a1,f0,f1,ft ;

    /* workspace image array for each thread (from realizations) */

#pragma omp critical
   { fim = mri_new_conforming( imtemplate , MRI_float ) ;
     far = MRI_FLOAT_PTR(fim) ; }

     /* use the thresholds computed above to multi-threshold
        each realization, and create count of total FA and in each voxel */

/* #pragma omp for schedule(dynamic,333) */
#pragma omp for                                      /* parallelized */
     for( iter=0 ; iter < niter ; iter++ ){          /* loop over iterations */
       for( icase=0 ; icase < ncase ; icase++ ){     /* over cases */
         generate_image( far , iter+icase*niter ) ;  /* get the image */

         /* multi-threshold this image with global FOM thresholds */

         tfim = mri_multi_threshold_Xcluster_fomth( fim, npthr,zthr_used,
                                                    nnsid,nnlev,
                                                    fthar0[icase] ,
                                                    fthar1[icase] ,
                                                    fthar2[icase] ,
                                                    0, NULL , NULL ) ;
         if( tfim != NULL ){   /* nothing found ==> NULL was returned */
#pragma omp critical
           { mri_free(tfim) ; nfar++ ;} /* nfar = count of total FA */
           break ; /* no need to continue thru cases (got a FA) */
         }
       } /* end of loop over cases */
     } /* end of loop over iterations (parallelized) */

#pragma omp critical
     { mri_free(fim) ; }  /* tossing the trash for this thread */
 }
 AFNI_OMP_END ;  /*------------ end parallel section ------------*/

     /* compute the FAR percentage at this tfrac */

     farpercold = farperc ;               /* save what we got last time */
     farperc    = (100.0f*nfar)/(float)niter ; /* what we got this time */
     farlast    = farperc ;           /* save for next FPR goal, if any */

     /* save results for later re-use */
     if( ntfp == ntfp_all ){
       ntfp_all += 128 ;
       tfs = (float *)realloc(tfs,sizeof(float)*ntfp_all) ;
       fps = (float *)realloc(tfs,sizeof(float)*ntfp_all) ;
     }
     tfs[ntfp] = tfrac ; fps[ntfp] = farperc ; ntfp++ ;

     /* farcut = precision desired for our FPR goal */
     farcut = 0.222f ;
     if( itrac > 2 ) farcut += (itrac-2)*0.0321f ;

     if( verb )
       ININFO_message("         global FPR=%.2f%% at %.1f s", farperc,COX_clock_time() ) ;
     MEMORY_CHECK(" ") ;

     /* if no substantial progress, quit */

     if( itrac > 2 && fabsf(farperc-farpercold) < 0.0222f ){
       ININFO_message("         ((Progress too slow - breaking out **))") ;
       goto GARP_BREAKOUT ;
     }

     /* try another tfrac to get closer to our goal? */

     if( itrac < MAXITE && fabsf(farperc-FG_GOAL) > farcut ){
       float fff , dtt ;
       if( itrac == 1 || (farperc-FG_GOAL)*(farpercold-FG_GOAL) > 0.1f ){ /* scale */
         fff = FG_GOAL/farperc ;
         if( fff > 2.222f ) fff = 2.222f ; else if( fff < 0.450f ) fff = 0.450f ;
         dtt  = (fff-1.0f)*tfrac ;        /* tfrac step */
         dtt *= (0.8666f+0.2222f*itrac) ; /* accelerate it */
         ttemp = tfrac ; tfrac += dtt ;
       } else {                                      /* linear inverse interpolate */
         fff = (farperc-farpercold)/(tfrac-tfracold) ;
         ttemp = tfrac ; tfrac = tfracold + (FG_GOAL-farpercold)/fff ;
       }
#define TFTOP (0.666f*TOPFRAC)
       tfracold = ttemp ;
            if( tfrac < min_tfrac ) tfrac = min_tfrac ;
       else if( tfrac > TFTOP     ) tfrac = TFTOP ;
       goto GARP_LOOPBACK ;
     }

GARP_BREAKOUT: ; /*nada*/
   } /* end of iterations to find the ideal farperc */

       /*---::: Write results out, then this farp_goal is done :::---*/

       { char *qpref , *cpt , *qax[2] ;
         NI_element *qel ;
         float *qar ; int nqq , qdim[2] , iqq ;

         /* constants for the NIML element to be created for each case */

         nqq = nhpow * npthr ;            /* number of thresholds */
         qar = (float *)malloc(sizeof(float)*nqq) ; /* thresholds */

         qdim[0] = npthr ; qdim[1] = nhpow;      /* order of data */
          qax[0] = "zthr";  qax[1] = "FOM";     /* labels for fun */

         qpref = strdup(prefix) ;            /* mangle the prefix */
         cpt   = strstr(qpref,".nii") ; if( cpt != NULL ) *cpt = '\0' ;

         for( qcase=0 ; qcase < ncase ; qcase++ ){ /* loop over cases */

           /* create a NIML element to hold the thresholds */

           qel = NI_new_data_element( "global_ETAC_thresholds" , nqq ) ;
           NI_set_dimen( qel , 2 , qdim ) ;
           NI_set_axes ( qel , qax ) ;

           /* store cluster-FOM thresholds we computed above */

           for( iqq=qpthr=0 ; qpthr < npthr ; qpthr++ ){
             if( do_hpow0 ) qar[iqq++] = fthar0[qcase][qpthr] ;
             if( do_hpow1 ) qar[iqq++] = fthar1[qcase][qpthr] ;
             if( do_hpow2 ) qar[iqq++] = fthar2[qcase][qpthr] ;
           }

           /* store them into the element */

           NI_add_column      ( qel , NI_FLOAT , qar ) ;
           NI_set_column_label( qel , 0 , "thresh" ) ;

           /* add some attributes for later use in 3dMultiThresh */

             /* simple constants */

           sprintf(qpr,"%d",nnlev)     ; NI_set_attribute(qel,"NNlev",qpr)     ;
           sprintf(qpr,"%d",nnsid)     ; NI_set_attribute(qel,"NNsid",qpr)     ;
           sprintf(qpr,"%d",min_clust) ; NI_set_attribute(qel,"min_clust",qpr) ;
           NI_set_attribute( qel , "case" , lcase[qcase] ) ;

             /* list of FOMs used */

           qpr[0] = '\0' ; iqq = 0 ;
           if( do_hpow0 ){ strcat(qpr,"hpow0") ; iqq++ ; }
           if( do_hpow1 ){
             if( iqq ) strcat(qpr,",") ;
             strcat(qpr,"hpow1") ; iqq++ ;
           }
           if( do_hpow2 ){
             if( iqq ) strcat(qpr,",") ;
             strcat(qpr,"hpow2") ; iqq++ ;
           }
           NI_set_attribute( qel , "FOM_list" , qpr ) ;

             /* list of voxelwise thresholds used */

           sprintf(qpr,"%.5f",zthr_used[0]) ;
           for( qpthr=1 ; qpthr < npthr ; qpthr++ )
             sprintf(qpr+strlen(qpr),",%.5f",zthr_used[qpthr]) ;
           NI_set_attribute( qel , "zthr_list" , qpr ) ;

             /* FAR goal (not really needed) */

           sprintf(qpr,"%.2f",farp_goal) ;
           NI_set_attribute( qel , "FAR_goal" , qpr ) ;

           /* invent a filename for the output */

           sprintf( qpr ,"globalETAC.mthresh.%s.%s.%dperc.niml" ,
                  qpref , lcase[qcase] , (int)rintf(farp_goal)     ) ;

           /* and write it out */

           NI_write_element_tofile( qpr , qel , NI_TEXT_MODE ) ;
           NI_free_element(qel) ;

           if( verb )
             ININFO_message("global ETAC thresholds written to %s",qpr) ;

         } /* end of loop over cases */

         free(qar) ; free(qpref) ;

       } /* end of writing out NIML elements with global thresholds */

     } /*--- end of loop over FAR goals ---*/

     /*::: it's NOT the end of the world (yet) :::*/

     /*--- delete vectors ---*/

     for( qcase=0 ; qcase < ncase ; qcase++ ){
       for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
         free(fomglob0[qcase][qpthr]) ;
         free(fomglob1[qcase][qpthr]) ;
         free(fomglob2[qcase][qpthr]) ;
       }
       free(fomglob0[qcase]); free(fomglob1[qcase]); free(fomglob2[qcase]);
       free(nfomglob[qcase]);
       free(fthar0[qcase])  ; free(fthar1[qcase])  ; free(fthar2[qcase])  ;
     }
     free(fomglob0); free(fomglob1); free(fomglob2); free(nfomglob);
     free(fthar0)  ; free(fthar1)  ; free(fthar2)  ;

     if( !do_local_etac ){
       INFO_message("=== 3dXClustSim ends: Elapsed time = %.1f s",COX_clock_time()) ;
       exit(0) ;
     }

     /* can unmap simulations now, until needed later */
     if( verb > 1 ) ININFO_message("un-mapping input datasets") ;
     unmap_Xdataset(xinset) ;

   } /*--------------- end of global ETAC-ization ---------------*/

   /***************************************************/
   /***** From here is is local (voxel-wise) ETAC *****/
   /***************************************************/

   /*==========================================================================*/
   /*--- STEP 2: dilate the clusters ------------------------------------------*/
   /*    The goal is to have at least so many "hits" at each voxel in the mask */

   /*--- initialize the FOM vector array for each voxel ---*/
   /*--- this is the list of FOMs (e.g., cluster sizes)
         that were found at that voxel                  ---*/

   fomvec0 = (Xvector **)malloc(sizeof(Xvector *)*mask_ngood) ;
   fomvec1 = (Xvector **)malloc(sizeof(Xvector *)*mask_ngood) ;
   fomvec2 = (Xvector **)malloc(sizeof(Xvector *)*mask_ngood) ;
   for( ii=0 ; ii < mask_ngood ; ii++ ){
     CREATE_Xvector(fomvec0[ii],DALL) ;
      fomvec0[ii]->ip  = ipmask[ii] ;   /* store location of this */
      fomvec0[ii]->jp  = jpmask[ii] ;   /* vector in the 3D grid */
      fomvec0[ii]->kp  = kpmask[ii] ;
      fomvec0[ii]->ijk = ijkmask[ii] ;
     CREATE_Xvector(fomvec1[ii],DALL) ;
      fomvec1[ii]->ip  = ipmask[ii] ;
      fomvec1[ii]->jp  = jpmask[ii] ;
      fomvec1[ii]->kp  = kpmask[ii] ;
      fomvec1[ii]->ijk = ijkmask[ii] ;
     CREATE_Xvector(fomvec2[ii],DALL) ;
      fomvec2[ii]->ip  = ipmask[ii] ;
      fomvec2[ii]->jp  = jpmask[ii] ;
      fomvec2[ii]->kp  = kpmask[ii] ;
      fomvec2[ii]->ijk = ijkmask[ii] ;
   }

   /* temp space to hold counts of FOM vec length for each voxel in a cluster */
   /* see get_Xcluster_nbcount() for usage */

   ncount_g = (int *)   malloc(sizeof(int)    *nthr) ; /* dimens of fcount_g */
   fcount_g = (float **)malloc(sizeof(float *)*nthr) ; /* counts */
   for( ii=0 ; ii < nthr ; ii++ ){
     fcount_g[ii] = (float *)malloc(sizeof(float)*DALL) ;
     ncount_g[ii] = DALL ;
   }

   /*--- thread-specific workspaces for clustering int dilate_Xcluster() ---*/

   ndilg    = (int * )malloc(sizeof(int  )*nthr) ; /* size of dilg_ijk */
   dilg_ijk = (int **)malloc(sizeof(int *)*nthr) ; /* candidates for dilate */
   for( ii=0 ; ii < nthr ; ii++ ){
     ndilg   [ii] = DALL ;
     dilg_ijk[ii] = (int *)malloc(sizeof(int)*DALL) ;
   }

   /*----- loop over p-thresholds and dilate in voxel chunks -----*/

                     dijk = nxyz / nthr ;  /* set chunk size */
   if( dijk > nxy  ) dijk = nxy ;
   if( dijk > 4096 ) dijk = 4096 ;

   /* target counts for voxel "hits" */

   count_targ100 = (int)rintf(dilate_fac*niter) ;
   if( count_targ100 > 333 )  /* why 333? It's reasonably sized */
     count_targ100 = (int)rintf(sqrtf(333.0f*count_targ100)) ;
   count_targ80  = (int)rintf(0.80f*count_targ100) ;
   count_targ60  = (int)rintf(0.60f*count_targ100) ;

   if( verb )
     INFO_message("STEP 2: start cluster dilations") ;
   if( verb > 1 ) ININFO_message("  Elapsed time = %.1f s",COX_clock_time()) ;

   for( qcase=0 ; qcase < ncase ; qcase++ ){  /* loop over cases */
    for( qpthr=0 ; qpthr < npthr ; qpthr++ ){  /* loop over p-value thresh */

     Xcluster **xcar = Xclust_tot[qcase][qpthr] ;
     int        ncar = nclust_tot[qcase][qpthr] ;

#define NDILMAX 9  /* max number of dilation steps */

     ndiltot = 0 ;
     for( ndilstep=0 ; ndilstep < NDILMAX ; ndilstep++ ){
       /* initialize counts of number of dilations for each NN type */
       ndilated[0] = ndilated[1] = ndilated[2] = ndilated[3] = 0 ;

#ifdef DEBUG_MEM
       if( verb > 1 ){
         char mmm[256] ;
         sprintf(mmm,"before dilation: qcase=%d qpthr=%d ndilstep=%d",qcase,qpthr,ndilstep) ;
         MEMORY_CHECK(mmm) ;
       }
#endif

 AFNI_OMP_START ;      /*------------ start parallel section ----------*/
#pragma omp parallel
 {  DECLARE_ithr ;
    int iter, idil , ijkbot,ijktop , nclu=nclust_tot[qcase][qpthr] ;

    /* reset the FOM vector to be empty, for each voxel */

#pragma omp for
    for( iter=0 ; iter < mask_ngood ; iter++ ){
      fomvec0[iter]->npt = 0 ;
      fomvec1[iter]->npt = 0 ;
      fomvec2[iter]->npt = 0 ;
    }

    /* set the Xcluster index ranges */

#pragma omp for
    for( iter=0 ; iter < ncar ; iter++ ){
      set_Xcluster_ijkminmax( xcar[iter] ) ;
    }

    /* load the FOM vectors (parallelized across 3D segments) */

/* #pragma omp for schedule(dynamic,1) */
#pragma omp for
    for( ijkbot=0 ; ijkbot < nxyz ; ijkbot+=dijk ){
      ijktop = ijkbot + (dijk-1) ; if( ijktop >= nxyz ) ijktop = nxyz-1 ;
      process_clusters_to_Xvectors( ijkbot, ijktop , qcase,qpthr ) ;
    }

    /* get median FOM counts in each cluster,
       then determine how to dilate (NN1, NN2, NN3)
       [parallelized across clusters = simulation iterations] */

/* #pragma omp for schedule(dynamic,666) */
#pragma omp for
    for( iter=0 ; iter < nclu ; iter++ ){
      if( Xclust_tot[qcase][qpthr][iter]          != NULL          &&
          Xclust_tot[qcase][qpthr][iter]->nbcount <  count_targ100   ){
        idil = get_Xcluster_nbcount( Xclust_tot[qcase][qpthr][iter] , ithr ) ;
        if( idil < count_targ100 ){  /* too few? */
          /* dilate:                   NN3 or                     NN2 or NN1 */
          idil = (idil < count_targ60) ? 3 :(idil < count_targ80) ? 2 :    1 ;
          dilate_Xcluster( Xclust_tot[qcase][qpthr][iter] , idil , ithr ) ;
#pragma omp atomic
          ndilated[idil]++ ;  /* count how many dilations of each type */
        }
      }
    } /* all clusters dilated now (or were OK) */

 }
 AFNI_OMP_END ;       /*---------- end parallel section ----------*/

       /* number of significant dilations (NN2+NN3) */

       ndilsum  = ndilated[2] + ndilated[3] ;
       ndiltot += ndilated[1] + ndilated[2] + ndilated[3] ;

       /* if not very many, then we are done with this p-value thresh */

       if( ndilstep < NDILMAX-1 && ndilsum < niter/50 ) break ;
     } /* end of loop over dilation steps */
     if( verb > 1 )
       ININFO_message("     %d dilation loops; %d total cluster dilations :: Case %s pthr=%.5f",
                      ndilstep,ndiltot,lcase[qcase],pthr[qpthr]) ;
    } /* end of loop over p-value thresh cluster collection */
   } /* end of loop over cases */

   /* free the dilation counting workspace for each thread */

   for( ii=0 ; ii < nthr ; ii++ ) free(dilg_ijk[ii]) ;
   free(ndilg); free(dilg_ijk);

   MEMORY_CHECK("after all dilations") ;

   /*=======================================================*/
   /*--- STEP 3: create sorted and truncated FOM vectors ---*/

   if( verb )
     INFO_message("STEP 3: re-loading & sorting FOM vectors after dilations") ;
   if( verb > 1 ) ININFO_message("  Elapsed time = %.1f s",COX_clock_time()) ;

   /*--- initialize the final sorted FOM vector array for each voxel ---*/

   fomsort0 = (Xvector ****)malloc(sizeof(Xvector ***)*ncase) ; /* hpow=0 */
   fomsort1 = (Xvector ****)malloc(sizeof(Xvector ***)*ncase) ; /*     =1 */
   fomsort2 = (Xvector ****)malloc(sizeof(Xvector ***)*ncase) ; /*     =2 */
   for( qcase=0 ; qcase < ncase ; qcase++ ){
     fomsort0[qcase] = (Xvector ***)malloc(sizeof(Xvector **)*npthr) ;
     fomsort1[qcase] = (Xvector ***)malloc(sizeof(Xvector **)*npthr) ;
     fomsort2[qcase] = (Xvector ***)malloc(sizeof(Xvector **)*npthr) ;
     for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
       fomsort0[qcase][qpthr] = (Xvector **)malloc(sizeof(Xvector *)*mask_ngood) ;
       fomsort1[qcase][qpthr] = (Xvector **)malloc(sizeof(Xvector *)*mask_ngood) ;
       fomsort2[qcase][qpthr] = (Xvector **)malloc(sizeof(Xvector *)*mask_ngood) ;
     }
   }

   nfomkeep = (int)(TOPFRAC*niter) ; /* max number of FOMs to keep at 1 voxel */

   for( qcase=0 ; qcase < ncase ; qcase++ ){ /* loop over cases */
    for( qpthr=0 ; qpthr < npthr ; qpthr++ ){ /* loop over p-value thresh */

     Xcluster **xcar = Xclust_tot[qcase][qpthr] ;
     int        ncar = nclust_tot[qcase][qpthr] ;

#ifdef DEBUG_MEM
     if( verb > 1 ){
       char mmm[256] ;
       sprintf(mmm,"before FOMsort: qcase=%d qpthr=%d",qcase,qpthr) ;
       MEMORY_CHECK(mmm) ;
     }
#else
     if( verb > 1 )
       ININFO_message("     start Case %s pthr=%.5f",lcase[qcase],pthr[qpthr]) ;
#endif

 AFNI_OMP_START ;      /*------------ start parallel section ----------*/
# pragma omp parallel
   { DECLARE_ithr ;
     int ijkbot,ijktop , iv,jj , npt , nclu=nclust_tot[qcase][qpthr] ;
     float a0,a1,f0,f1,ft ;

     /* re-load FOM vectors for this threshold */

#pragma omp for
    for( iv=0 ; iv < mask_ngood ; iv++ ){
      fomvec0[iv]->npt = 0 ;
      fomvec1[iv]->npt = 0 ;
      fomvec2[iv]->npt = 0 ;
    }

    /* set the Xcluster index ranges */

#pragma omp for
    for( iv=0 ; iv < ncar ; iv++ ){
      set_Xcluster_ijkminmax( xcar[iv] ) ;
    }

/* #pragma omp for schedule(dynamic,1) */
#pragma omp for
     for( ijkbot=0 ; ijkbot < nxyz ; ijkbot+=dijk ){
       ijktop = ijkbot + (dijk-1) ;
       process_clusters_to_Xvectors( ijkbot, ijktop , qcase,qpthr ) ;
     }

     /* delete Xclusters for this threshold,
        since we have the finalized FOM vectors */

#pragma omp master
     { for( jj=0 ; jj < nclu ; jj++ ){
         DESTROY_Xcluster(Xclust_tot[qcase][qpthr][jj]) ;
       }
       free(Xclust_tot[qcase][qpthr]) ; Xclust_tot[qcase][qpthr] = NULL ;
     }
#pragma omp barrier

     /* sort/truncate FOM vectors for this threshold */

     /* vectors loaded for each pt in space;
        now sort them (biggest first) to determine equitable thresholds */

/* #pragma omp for schedule(dynamic,666) */
#pragma omp for
     for( iv=0 ; iv < mask_ngood ; iv++ ){ /* loop over voxels in mask */
       if( fomvec0[iv]->npt < 3 ){          /* if it's way short */
         ADDTO_Xvector(fomvec0[iv],0.01f) ; /* put some padding in */
         ADDTO_Xvector(fomvec0[iv],0.01f) ;
         ADDTO_Xvector(fomvec0[iv],0.01f) ;
         ADDTO_Xvector(fomvec1[iv],0.01f) ;
         ADDTO_Xvector(fomvec1[iv],0.01f) ;
         ADDTO_Xvector(fomvec1[iv],0.01f) ;
         ADDTO_Xvector(fomvec2[iv],0.01f) ;
         ADDTO_Xvector(fomvec2[iv],0.01f) ;
         ADDTO_Xvector(fomvec2[iv],0.01f) ;
       }

       /* sort into decreasing order (largest FOMs first) */

       if( do_hpow0 ) qsort_float_rev( fomvec0[iv]->npt , fomvec0[iv]->far ) ;
       if( do_hpow1 ) qsort_float_rev( fomvec1[iv]->npt , fomvec1[iv]->far ) ;
       if( do_hpow2 ) qsort_float_rev( fomvec2[iv]->npt , fomvec2[iv]->far ) ;

       /* create a new vector to get the keeper FOM counts */

       if( do_hpow0 ){
         jj = MIN(fomvec0[iv]->npt,nfomkeep) ;    /* how many to keep */
         CREATE_Xvector(fomsort0[qcase][qpthr][iv],jj) ;
         fomsort0[qcase][qpthr][iv]->ip  = ipmask[iv] ;  /* location */
         fomsort0[qcase][qpthr][iv]->jp  = jpmask[iv] ;
         fomsort0[qcase][qpthr][iv]->kp  = kpmask[iv] ;
         fomsort0[qcase][qpthr][iv]->ijk = ijkmask[iv] ;
         fomsort0[qcase][qpthr][iv]->npt = jj ;          /* count */
         AAmemcpy(fomsort0[qcase][qpthr][iv]->far,fomvec0[iv]->far,sizeof(float)*jj) ;
       }

       if( do_hpow1 ){
         jj = MIN(fomvec1[iv]->npt,nfomkeep) ;    /* how many to keep */
         CREATE_Xvector(fomsort1[qcase][qpthr][iv],jj) ;
         fomsort1[qcase][qpthr][iv]->ip  = ipmask[iv] ;  /* location */
         fomsort1[qcase][qpthr][iv]->jp  = jpmask[iv] ;
         fomsort1[qcase][qpthr][iv]->kp  = kpmask[iv] ;
         fomsort1[qcase][qpthr][iv]->ijk = ijkmask[iv] ;
         fomsort1[qcase][qpthr][iv]->npt = jj ;          /* count */
         AAmemcpy(fomsort1[qcase][qpthr][iv]->far,fomvec1[iv]->far,sizeof(float)*jj) ;
       }

       if( do_hpow2 ){
         jj = MIN(fomvec2[iv]->npt,nfomkeep) ;    /* how many to keep */
         CREATE_Xvector(fomsort2[qcase][qpthr][iv],jj) ;
         fomsort2[qcase][qpthr][iv]->ip  = ipmask[iv] ;  /* location */
         fomsort2[qcase][qpthr][iv]->jp  = jpmask[iv] ;
         fomsort2[qcase][qpthr][iv]->kp  = kpmask[iv] ;
         fomsort2[qcase][qpthr][iv]->ijk = ijkmask[iv] ;
         fomsort2[qcase][qpthr][iv]->npt = jj ;          /* count */
         AAmemcpy(fomsort2[qcase][qpthr][iv]->far,fomvec2[iv]->far,sizeof(float)*jj) ;
       }
     } /* end of voxel loop */
   }
 AFNI_OMP_END ;       /*---------- end parallel section ----------*/

     /* save dataset of fomvec counts */

#ifdef ALLOW_EXTRAS
     if( do_FOMcount ){
       qset = EDIT_empty_copy(mask_dset) ;
       sprintf(qpr,".FOMc.%s.%d",lcase[qcase],qpthr) ;
       EDIT_dset_items( qset ,
                          ADN_prefix , modify_afni_prefix(prefix,NULL,qpr) ,
                          ADN_nvals  , 1 ,
                        ADN_none ) ;
       EDIT_substitute_brick( qset , 0 , MRI_float , NULL ) ;
       qar = DSET_ARRAY(qset,0) ;
       for( ii=0 ; ii < mask_ngood ; ii++ ){
         ijk = ijkmask[ii] ;
         qar[ijk] = (float)fomvec0[ii]->npt ;
         fomvec0[ii]->npt = 0 ;
       }
       EDIT_BRICK_LABEL(qset,0,"FOMcount") ;
       tross_Make_History( "3dXClustSim" , argc,argv , qset ) ;
       DSET_write(qset); WROTE_DSETI(qset);
       DSET_delete(qset); qset = NULL; qar = NULL;
     }
#endif

    } /* end of loop over qpthr */
   } /* end of loop over cases */

   /* destroy the unsorted full FOM vectors */

   for( ii=0 ; ii < mask_ngood ; ii++ ){  /* don't need no more */
     DESTROY_Xvector(fomvec0[ii]) ;
     DESTROY_Xvector(fomvec1[ii]) ;
     DESTROY_Xvector(fomvec2[ii]) ;
   }
   free(fomvec0) ; fomvec0 = NULL ;
   free(fomvec1) ; fomvec1 = NULL ;
   free(fomvec2) ; fomvec2 = NULL ;

   MEMORY_CHECK("after all FOMsort") ;

   /*=============================================================*/
   /*--- STEP 4: test FOM count thresholds to find desired FAR ---*/

   /* get the noise simulations back */

   if( verb > 1 ) ININFO_message("re-mapping input datasets") ;
   remap_Xdataset(xinset) ;  /* de-nonce-ification */

   /* create voxel-specific FOM threshold images (for each p-value thresh) */

   if( do_hpow0 ){
     cimar0 = (MRI_IMARR **)malloc(sizeof(MRI_IMARR *)*ncase) ;
     car0   = (float ***)malloc(sizeof(float **)*ncase) ;
     for( qcase=0 ; qcase < ncase ; qcase++ ){
       INIT_IMARR(cimar0[qcase]) ;
       car0[qcase] = (float **)malloc(sizeof(float *)*npthr) ;
       for( qpthr=0; qpthr < npthr ; qpthr++ ){
         cim0 = mri_new_conforming( imtemplate , MRI_float ) ;
         ADDTO_IMARR(cimar0[qcase],cim0) ;
         car0[qcase][qpthr] = MRI_FLOAT_PTR(cim0) ;
       }
     }
   }

   if( do_hpow1 ){
     cimar1 = (MRI_IMARR **)malloc(sizeof(MRI_IMARR *)*ncase) ;
     car1   = (float ***)malloc(sizeof(float **)*ncase) ;
     for( qcase=0 ; qcase < ncase ; qcase++ ){
       INIT_IMARR(cimar1[qcase]) ;
       car1[qcase] = (float **)malloc(sizeof(float *)*npthr) ;
       for( qpthr=0; qpthr < npthr ; qpthr++ ){
         cim1 = mri_new_conforming( imtemplate , MRI_float ) ;
         ADDTO_IMARR(cimar1[qcase],cim1) ;
         car1[qcase][qpthr] = MRI_FLOAT_PTR(cim1) ;
       }
     }
   }

   if( do_hpow2 ){
     cimar2 = (MRI_IMARR **)malloc(sizeof(MRI_IMARR *)*ncase) ;
     car2   = (float ***)malloc(sizeof(float **)*ncase) ;
     for( qcase=0 ; qcase < ncase ; qcase++ ){
       INIT_IMARR(cimar2[qcase]) ;
       car2[qcase] = (float **)malloc(sizeof(float *)*npthr) ;
       for( qpthr=0; qpthr < npthr ; qpthr++ ){
         cim2 = mri_new_conforming( imtemplate , MRI_float ) ;
         ADDTO_IMARR(cimar2[qcase],cim2) ;
         car2[qcase][qpthr] = MRI_FLOAT_PTR(cim2) ;
       }
     }
   }

   /* array to make map of false alarm count at each voxel */

#ifdef ALLOW_EXTRAS
   if( do_FARvox )
     farar = (float *)malloc(sizeof(float)*nxyz) ;
#endif

   /*--- loop over different FPR (farp) goals ---*/

   nit33 = 1.0f/(niter+0.333f) ;

   if( ntfp_all == 0 ){
     ntfp_all = 128 ;
     tfs  = (float *)malloc(sizeof(float)*ntfp_all) ;
     fps  = (float *)malloc(sizeof(float)*ntfp_all) ;
   }
   ntfp = 0 ;

   farlast = farplist[0] ;
   for( ifarp=0 ; ifarp < numfarp ; ifarp++ ){ /* 23 Aug 2017 */

     farp_goal = farplist[ifarp] ;

     if( verb )
       INFO_message("STEP 4.%s: adjusting per-voxel FOM thresholds to reach FPR=%.2f%%",
                    abcd[ifarp] , farp_goal) ;
     if( verb > 1 ) ININFO_message("  Elapsed time = %.1f s",COX_clock_time()) ;

     /* tfrac = FOM count fractional threshold;
                will be adjusted to find the farp_goal FPR goal */

     if( ifarp == 0 ){                                     /* first time thru */
       tfrac = (4.0f+farp_goal)*0.00005f ;
     } else if( ntfp < 2 ){                  /* if only 1 earlier calculation */
       tfrac *= 1.0666f * farp_goal / farlast ;     /* adjust previous result */
     } else {
       int jj, jd ; float adf, mdf ;         /* later: use closest in history */
       mdf = fabsf(fps[0]-farp_goal) ; jd = 0 ;   /* to adjust starting point */
       for( jj=1 ; jj < ntfp ; jj++ ){
         adf = fabsf(fps[jj]-farp_goal) ;
         if( adf < mdf ){ mdf = adf ; jd = jj ; }
       }
       tfrac = tfs[jd] * farp_goal / fps[jd] ;
     }

     itrac = 0 ;
     farpercold = farperc = 0.0f ; tfracold = tfrac ;

   /*--- Loop back to here with adjusted tfrac ---*/

FARP_LOOPBACK:
     {
       float min_tfrac ; int nedge,nmin,ntest ;
       min_tfrac = 6.0f / niter ; if( min_tfrac > 0.0001f ) min_tfrac = 0.0001f ;

       itrac++ ;                                      /* number of iterations */
       nfar = 0 ;                                          /* total FAR count */
       nedge = nmin = ntest = 0 ;                     /* number of edge cases */

         /* we take ithresh-th largest FOM at each voxel as its FOM threshold */
#if 0
       ithresh = (int)(tfrac*niter) ;                  /* FOM count threshold */
#else
       ithresh = (int)rintf(tfrac*(niter-0.666f)+0.333f) ;
#endif

         /* Check if trying to re-litigate previous case [Cinco de Mayo 2017] */

       ithresh_list[itrac-1] = ithresh ;
       if( itrac > 3 &&
           (ithresh_list[itrac-2] == ithresh || ithresh_list[itrac-3] == ithresh) ){
         ININFO_message("     #%d: would re-iterate at %g ==> %d ; breaking out",
                        itrac,tfrac,ithresh ) ;
         goto FARP_BREAKOUT ;
       }

       if( verb )
         ININFO_message("     #%d: Testing threshold images at %g ==> %d",itrac,tfrac,ithresh) ;

       for( qcase=0 ; qcase < ncase ; qcase++ ){  /* initialize FOM thresholds to 0 */
       for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
         if( do_hpow0 ){ AAmemset(car0[qcase][qpthr],0,sizeof(float)*nxyz) ; }
         if( do_hpow1 ){ AAmemset(car1[qcase][qpthr],0,sizeof(float)*nxyz) ; }
         if( do_hpow2 ){ AAmemset(car2[qcase][qpthr],0,sizeof(float)*nxyz) ; }
       }}

#ifdef ALLOW_EXTRAS
       if( do_FARvox ){
         AAmemset(farar,0,sizeof(float)*nxyz) ;  /* zero out FARvox array */
       }
#endif

 AFNI_OMP_START ;     /*------------ start parallel section ----------*/
#pragma omp parallel
 {  DECLARE_ithr ;
    MRI_IMAGE *fim , *tfim ; float *far , *tfar ;
    int iter,npt,iv,jthresh , ipthr,icase , qq,hh ;
    float a0,a1,f0,f1,ft ;
    Xvector ***fomsortH=NULL ; float **carH=NULL, *gthreshH ;

    /* workspace image array for each thread */

#pragma omp critical
   { fim = mri_new_conforming( imtemplate , MRI_float ) ;
     far = MRI_FLOAT_PTR(fim) ; }

     /* find the FOM threshold for each voxel, for each p-value thresh */

#pragma omp for
     for( iv=0 ; iv < mask_ngood ; iv++ ){          /* loop over voxels */
       for( icase=0 ; icase < ncase ; icase++ ){          /* over cases */
        for( ipthr=0 ; ipthr < npthr ; ipthr++ ){       /* over p-value */
         for( hh=0 ; hh < 3 ; hh++ ){               /* over hpow values */
           if( hh==0 ){
             if( !do_hpow0 ) continue ;
             fomsortH = fomsort0[icase]; carH = car0[icase]; gthreshH = gthresh0[ifarp][icase];
           }
           if( hh==1 ){
             if( !do_hpow1 ) continue ;
             fomsortH = fomsort1[icase]; carH = car1[icase]; gthreshH = gthresh1[ifarp][icase];
           }
           if( hh==2 ){
             if( !do_hpow2 ) continue ;
             fomsortH = fomsort2[icase]; carH = car2[icase]; gthreshH = gthresh2[ifarp][icase];
           }
           npt = fomsortH[ipthr][iv]->npt ;  /* how many FOM values here */
           jthresh = ithresh ;            /* default index of FOM thresh */

           if( jthresh > (int)(0.666f*npt) ){  /* edge case: */
             jthresh = (int)(0.666f*npt) ;     /* not many FOM values here */
#pragma omp atomic
             nedge++ ;
           }
           /* extract this FOM thresh by interpolation */
#if 0
           a0 = ((float)jthresh)/((float)niter) ;
           a1 = a0        + 1.0f/((float)niter) ;
#else
           a0 = ((float)jthresh+0.666f)*nit33 ;
           a1 = a0 + nit33 ;
#endif
           f0 = fomsortH[ipthr][iv]->far[jthresh] ;
           f1 = fomsortH[ipthr][iv]->far[jthresh+1] ;
           ft = inverse_interp_extreme( a0,a1,tfrac , f0,f1 ) ;

#pragma omp atomic                      /* total number of tests */
           ntest++ ;

           if( ft < gthreshH[ipthr] ){  /* min case: */
             ft = gthreshH[ipthr] ;     /* threshold falls below global */
#pragma omp atomic                      /* min computed for this situation */
             nmin++ ;
           }
           carH[ipthr][ijkmask[iv]] = ft ;  /* = FOM threshold for this voxel */
                                        /* = the goal of this entire program! */
         } /* end loop over hh */
        } /* end loop over p-thresh */
       } /* end loop over cases */
     } /* end loop over voxels (the parallelized loop) */

     /* now, use the thresholds just computed to multi-threshold
        each realization, and create count of total FA and in each voxel */

/* #pragma omp for schedule(dynamic,333) */
#pragma omp for                                      /* parallelized */
     for( iter=0 ; iter < niter ; iter++ ){          /* loop over iterations */
       for( icase=0 ; icase < ncase ; icase++ ){     /* over cases */
         generate_image( far , iter+icase*niter ) ;  /* get the image */

         /* threshold this image */

         tfim = mri_multi_threshold_Xcluster_cimar( fim, npthr,zthr_used,
                                                    nnsid,nnlev,
                                                    do_hpow0 ? cimar0[icase] : NULL ,
                                                    do_hpow1 ? cimar1[icase] : NULL ,
                                                    do_hpow2 ? cimar2[icase] : NULL ,
                                                    0, NULL , NULL ) ;
         if( tfim != NULL ){   /* nothing found ==> NULL is returned */
#ifdef ALLOW_EXTRAS
           tfar = MRI_FLOAT_PTR(tfim) ;
           for( qq=0 ; qq < nxyz ; qq++ ){
             if( do_FARvox && tfar[qq] != 0.0f )
#pragma omp atomic
               farar[qq]++ ;  /* count of FA at this voxel */
           }
#endif
#pragma omp critical
           { mri_free(tfim) ; nfar++ ;} /* nfar = count of total FA */
           break ; /* no need to continue thru cases (got a FA) */
         }
       } /* end of loop over cases */
     } /* end of loop over iterations (parallelized) */

#pragma omp critical
     { mri_free(fim) ; }  /* tossing the trash for this thread */
 }
 AFNI_OMP_END ;  /*------------ end parallel section ------------*/

     /* compute the FAR percentage at this tfrac */

     farpercold = farperc ;               /* save what we got last time */
     farperc    = (100.0f*nfar)/(float)niter ; /* what we got this time */
     farlast    = farperc ;           /* save for next FPR goal, if any */

     /* save results for later re-use [22 Feb 2018] */
     if( ntfp == ntfp_all ){
       ntfp_all += 128 ;
       tfs = (float *)realloc(tfs,sizeof(float)*ntfp_all) ;
       fps = (float *)realloc(tfs,sizeof(float)*ntfp_all) ;
     }
     tfs[ntfp] = tfrac ; fps[ntfp] = farperc ; ntfp++ ;

     /* farcut = precision desired for our FPR goal */
     farcut = 0.222f ;
     if( itrac > 2 ) farcut += (itrac-2)*0.0321f ;

     if( verb )
       ININFO_message("         local FPR=%.2f%% at %.1f s", farperc,COX_clock_time()) ;
     MEMORY_CHECK(" ") ;

     /* if no substantial progress, quit */

     if( itrac > 2 && fabsf(farperc-farpercold) < 0.0222f ){
       ININFO_message("         ((Progress too slow - breaking out **))") ;
       goto FARP_BREAKOUT ;
     }

     /* try another tfrac to get closer to our goal? */

     if( itrac < MAXITE && fabsf(farperc-FG_GOAL) > farcut ){
       float fff , dtt ;
       if( itrac == 1 || (farperc-FG_GOAL)*(farpercold-FG_GOAL) > 0.1f ){ /* scale */
         fff = FG_GOAL/farperc ;
         if( fff > 2.222f ) fff = 2.222f ; else if( fff < 0.450f ) fff = 0.450f ;
         dtt  = (fff-1.0f)*tfrac ;        /* tfrac step */
         dtt *= (0.8666f+0.2222f*itrac) ; /* accelerate it */
         ttemp = tfrac ; tfrac += dtt ;
       } else {                                      /* linear inverse interpolate */
         fff = (farperc-farpercold)/(tfrac-tfracold) ;
         ttemp = tfrac ; tfrac = tfracold + (FG_GOAL-farpercold)/fff ;
       }
#define TFTOP (0.666f*TOPFRAC)
       tfracold = ttemp ;
            if( tfrac < min_tfrac ) tfrac = min_tfrac ;
       else if( tfrac > TFTOP     ) tfrac = TFTOP ;
       goto FARP_LOOPBACK ;
     }

FARP_BREAKOUT: ; /*nada*/
   } /* end of iterations to find the ideal farperc */

     /*=====================================================*/
     /*--- Write stuff out, then this farp_goal is done ---*/

     gthrout = (float *)malloc(sizeof(float)*npthr*nhpow) ;

     for( qcase=0 ; qcase < ncase ; qcase++ ){ /* one dataset per case */

       qset = EDIT_empty_copy(mask_dset) ;

       sprintf(qpr,".mthresh.%s.%dperc",lcase[qcase],(int)rintf(farp_goal)) ; /* prefix modifier */

       EDIT_dset_items( qset ,
                          ADN_prefix    , modify_afni_prefix(prefix,NULL,qpr) ,
                          ADN_nvals     , npthr*nhpow ,
                          ADN_datum_all , MRI_float ,
                        ADN_none ) ;

       /* attach bricks for each pthr, for each hpow */

       for( ibr=0,qpthr=0 ; qpthr < npthr ; qpthr++ ){
        for( hp=0 ; hp < 3 ; hp++ ){
         if( hp==0 ){ if( !do_hpow0 ) continue;
                      else { carHP=car0[qcase]; gthrH=gthresh0[ifarp][qcase]; }}
         if( hp==1 ){ if( !do_hpow1 ) continue;
                      else { carHP=car1[qcase]; gthrH=gthresh1[ifarp][qcase]; }}
         if( hp==2 ){ if( !do_hpow2 ) continue;
                      else { carHP=car2[qcase]; gthrH=gthresh2[ifarp][qcase]; }}
         EDIT_substitute_brick( qset , ibr , MRI_float , NULL ) ;
         qar = DSET_ARRAY(qset,ibr) ;
         AAmemcpy( qar , carHP[qpthr] , sizeof(float)*nxyz ) ;
         sprintf(qpr,"Mth:%.4f:h=%d",pthr[qpthr],hp) ;
         EDIT_BRICK_LABEL(qset,ibr,qpr) ;
         gthrout[ibr] = gthrH[qpthr] ;
         ibr++ ;
       }}

       /* attach an attribute describing the multi-threshold setup */

       { float *afl=malloc(sizeof(float)*(npthr+5)) ;
         afl[0] = (float)nnlev ;                                       /* NN */
         afl[1] = (float)nnsid ;                      /* sidedness of t-test */
         afl[2] = (float)npthr ;                   /* number of z-thresholds */
         afl[3] = (float)(do_hpow0 + 2*do_hpow1 + 4*do_hpow2) ; /* hpow bits */
         for( qpthr=0 ; qpthr < npthr ; qpthr++ )    /* and the z-thresholds */
           afl[qpthr+4] = zthr_used[qpthr] ;

         afl[npthr+4] = (float)min_clust ; /* min cluster size [21 Sep 2017] */

         THD_set_float_atr( qset->dblk ,
                            "MULTI_THRESHOLDS" , npthr+5 , afl ) ;
         free(afl) ;

         /* attribute for the minimum thresholds per brick [20 Feb 2018] */

         THD_set_float_atr( qset->dblk ,
                            "MULTI_THRESHOLDS_MIN" , npthr*nhpow , gthrout ) ;
       }

       /* output dataset */

       tross_Make_History( "3dXClustSim" , argc,argv , qset ) ;
       DSET_write(qset); WROTE_DSETI(qset);
       DSET_delete(qset); qset = NULL; qar = NULL;

     } /* end of loop over cases (blurs) */

#ifdef ALLOW_EXTRAS
     if( do_FARvox ){                       /* output the per-voxel false alarm count */
       qset = EDIT_empty_copy(mask_dset) ;
       sprintf(qpr,".FARvox") ;
       EDIT_dset_items( qset ,
                          ADN_prefix , modify_afni_prefix(prefix,NULL,qpr) ,
                          ADN_nvals  , 1 ,
                        ADN_none ) ;
       EDIT_BRICK_LABEL(qset,0,"FARcount") ;
       EDIT_substitute_brick( qset , 0 , MRI_float , NULL ) ;
       qar = DSET_ARRAY(qset,0) ;
       AAmemcpy( qar , farar , sizeof(float)*nxyz ) ;
       tross_Make_History( "3dXClustSim" , argc,argv , qset ) ;
       DSET_write(qset); WROTE_DSETI(qset);
       DSET_delete(qset); qset = NULL; qar = NULL;
     }
  #endif

   } /* end of loop over farp goals */

   /* It's the end of the world, Calvin */

   INFO_message("=== 3dXClustSim ends: Elapsed time = %.1f s",COX_clock_time()) ;
   MEMORY_CHECK("THE END") ;
   exit(0) ;
}
