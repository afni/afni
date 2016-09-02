#include "mrilib.h"

/** to do:
     generalize equity loop to be over a struct array instead of just pthr
     write better help
**/

#ifdef USE_OMP
#include <omp.h>
#endif

#include "mri_threshX.c"  /* lots of important stuff */
#include "thd_Xdataset.c" /* input dataset format */

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

static int   niter=0 ;  /* number of iterations (realizations) */

static int   do_fixed                = 0 ;    /* fixed FOM threshold? */
static int   fixed_cluster_threshold = 0 ;
static float fixed_pvalue_threshold  = 0.0f ;

static int   do_FOMcount = 0 ;
static int   do_FARvox   = 1 ;

#define PMAX 0.5

static double pthr_init[5] = { 0.0100, 0.0056, 0.0031, 0.0018, 0.0010 } ;
static double athr_init[5] = { 0.05 , 0.04 , 0.03 , 0.02 , 0.01 } ;

static int    npthr = 5 ;
static double *pthr = NULL ;

static float  *zthr_1sid = NULL ;
static float  *zthr_2sid = NULL ;
static float  *zthr_used = NULL ;

static int    nathr = 5 ;     /* athr is not used at this time */
static double *athr = NULL ;

static int verb = 1 ;
static int nthr = 1 ;  /* default number of threads */

static int nnlev = 1 ; /* 1 or 2 or 3 */
static int nnsid = 1 ; /* 1 or 2 */

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

static char *prefix = "Xsim.nii" ;

static MRI_IMAGE *imtemplate = NULL ;

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
/* For use in progress counts */

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
  char * ep;
  int nopt=1 , ii ;

ENTRY("get_options") ;

  while( nopt < argc ){

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

    /*-----  -fixed pvalue clustersize  -----*/

    if( strcasecmp(argv[nopt],"-fixed") == 0 ){
      if( pthr != NULL )
        ERROR_exit("You can't use '-fixed' AND also use '-pthr'") ;
      if( ++nopt >= argc-1 )
        ERROR_exit("You need 2 arguments after option '-fixed'") ;
      fixed_pvalue_threshold  = (float)strtod(argv[nopt++],NULL) ;
      fixed_cluster_threshold = (int)  strtod(argv[nopt++],NULL) ;
      do_fixed = ( fixed_pvalue_threshold  > 0.0f &&
                   fixed_pvalue_threshold  < 0.1f &&
                   fixed_cluster_threshold > MIN_CLUST ) ;
      if( do_fixed ){
        npthr   = 1 ;
        pthr    = (double *)malloc(sizeof(double)) ;
        pthr[0] = fixed_pvalue_threshold ;
      } else {
        ERROR_exit("Illegal values after '-fixed'") ;
      }
      continue ;
    }

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

    /*-----  -inset mask sdata  -----*/

    if( strcasecmp(argv[nopt],"-inset" ) == 0 ||
        strcasecmp(argv[nopt],"-insdat") == 0   ){
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

    /*----   -quiet   ----*/

    if( strcasecmp(argv[nopt],"-quiet") == 0 ){
      verb = 0 ; nopt++ ; continue ;
    }

    /*----   -FOMcount   ----*/

    if( strcasecmp(argv[nopt],"-FOMcount") == 0 ){
      do_FOMcount = 1 ; nopt++ ; continue ;
    }

    /*----   -noFARvox   ----*/

    if( strcasecmp(argv[nopt],"-noFARvox") == 0 ){
      do_FARvox = 0 ; nopt++ ; continue ;
    }

    /*-----   -pthr p   -----*/

    if( strcmp(argv[nopt],"-pthr") == 0 || strcmp(argv[nopt],"-pval") == 0 ){
      if( pthr != NULL )         ERROR_exit("you can't use '-pthr' twice!'") ;
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

    /*----- unknown option -----*/

    ERROR_exit("3dClustSim -- unknown option '%s'",argv[nopt]) ;
  }

  /*------- finalize some simple setup stuff --------*/

  if( xinset == NULL ) ERROR_exit("-inset option is mandatory :(") ;

  if( pthr == NULL ){
    pthr = (double *)malloc(sizeof(double)*npthr) ;
    AAmemcpy( pthr , pthr_init , sizeof(double)*npthr ) ;
    if( !verb )
      INFO_message("Using default %d p-value thresholds",npthr) ;
  }

#if 0
  if( athr == NULL ){
    athr = (double *)malloc(sizeof(double)*nathr) ;
    AAmemcpy( athr , athr_init , sizeof(double)*nathr ) ;
  }
#endif

  nx = DSET_NX(xinset->mask_dset) ;
  ny = DSET_NY(xinset->mask_dset) ;
  nz = DSET_NZ(xinset->mask_dset) ;
  dx = fabsf(DSET_DX(xinset->mask_dset)) ;
  dy = fabsf(DSET_DY(xinset->mask_dset)) ;
  dz = fabsf(DSET_DZ(xinset->mask_dset)) ;

  /* copy some variables because xinset was a late addition to the code */

  mask_dset  = xinset->mask_dset ;
  imtemplate = DSET_BRICK(xinset->mask_dset,0) ;
  mask_ngood = xinset->ngood ;
  mask_vol   = xinset->mask_vol ;
  if( niter <= 0 || niter > xinset->nvtot ) niter = xinset->nvtot ;
  if( verb )
    INFO_message("using %d/%d volumes from -inset; mask has %d points",
                 niter, xinset->nvtot, mask_ngood ) ;

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
  zthr_used = (nnsid==1) ? zthr_1sid : zthr_1sid ;

  EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Load image (realization) for thresholdization */

void generate_image( float *fim , int iter )
{
  /* Outsource the creation of the random field */

  load_from_Xdataset( xinset , iter , fim ) ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Global cluster collection:
     Xclustar_g[ipthr][iter] = cluster array at iteration iter and
     threshold ipthr. The basic array is created in main().
*//*-------------------------------------------------------------------------*/

static Xcluster_array ***Xclustar_g ;

static int        *nclust_tot , nclust_max ;
static Xcluster ***Xclust_tot ;  /* [ipthr][nclust_tot[ipthr]] */

/*---------------------------------------------------------------------------*/
/* Get a NN{nn}_{ss}sided cluster array at a particular threshold (ipthr),
   at a particular iteration (iter), and save it into the global cluster
   collection Xclustar_g.  nn=1 or 2 or 3;  ss=1 or 2.
*//*-------------------------------------------------------------------------*/

void gather_clusters( int ipthr, float *fim, MRI_IMAGE *tfim, int nn,int ss, int iter )
{
  register int ii ; register float thr ; float *tfar = MRI_FLOAT_PTR(tfim) ;

  thr = zthr_used[ipthr] ;
  if( ss == 1 ){
    for( ii=0 ; ii < nxyz ; ii++ )
      tfar[ii] = (      fim[ii]  >= thr) ? fim[ii] : 0.0f ;
  } else {
    for( ii=0 ; ii < nxyz ; ii++ )
      tfar[ii] = (fabsf(fim[ii]) >= thr) ? fim[ii] : 0.0f ;
  }

  Xclustar_g[ipthr][iter] = find_Xcluster_array( tfim , nn , NULL ) ;

  return ;
}

/*---------------------------------------------------------------------------*/
/* Dilate a cluster by 1 voxel [don't change FOM] */

#define DILATE_point(pqr)                                         \
 do{ int i,j,k , npt=xc->npt;                                     \
     IJK_TO_THREE(pqr,i,j,k,nx,nxy) ;                             \
     if( npt == xc->nall ){                                       \
       xc->nall += DALL + xc->nall ;                              \
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
   again, the highest level arrays are created in main(),
   and the sub-arrays are never free()-ed to avoid OpenMP degradation */

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

   if( xc == NULL || xc->npt == 0 ) return ;

   npt = xc->npt ;

   /* create the list of candidate points for dilation
      (most will not survive the second step: already in cluster) */

   ntry = npt * (  (nnlev<=1) ? 6 : (nnlev==2) ? 18 : 26 ) ;
   if( ntry > ndilg[ithr] ){
     dilg_ijk[ithr] = (int *)realloc(dilg_ijk[ithr],sizeof(int)*(ntry+64)) ;
     ndilg  [ithr]  = ntry+64 ;
   }
   ijkar = dilg_ijk[ithr] ;

   for( jj=ii=0 ; ii < npt ; ii++ ){           /* candidates = neighbors */
     xx = xc->ip[ii] ; yy = xc->jp[ii] ; zz = xc->kp[ii] ; /* current pt */
     /* no dilation from outer edges of 3D grid */
     if( xx==0 || xx==nx1 || yy==0 || yy==ny1 || zz==0 || zz==nz-1 ) continue ;
     ijk = xx+nx*yy+nxy*zz ;

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

#define ADDTO_Xvector(xv,val)                                         \
 do{ if( xv->npt == xv->nall ){                                       \
       xv->nall += DALL ;                                             \
       xv->far   = (float *)realloc(xv->far,sizeof(float)*xv->nall) ; \
     }                                                                \
     xv->far[xv->npt++] = (val) ;                                     \
 } while(0)

/* global FOM vector array -- created in main() */

static Xvector **fomvec = NULL ;    /* [mask_ngood] */
static Xvector ***fomsort = NULL ;  /* [npthr][mask_ngood] */

/*---------------------------------------------------------------------------*/
/* Process the clusters to FOM vectors for a range of 3D indexes,
   for a given p-value threshold index ipthr.
     This function is intended to be use in multiple threads, operating
     over differing ijkbot..ijktop blocks:
       a given fomvec will not be in thread conflict, since it is linked
       to a given voxel in space (ijk), so there is no potential problem
       when updating that fomvec, or the cluster's ijk entry.
*//*-------------------------------------------------------------------------*/

void process_clusters_to_Xvectors( int ijkbot, int ijktop , int ipthr )
{
   Xcluster **xcar = Xclust_tot[ipthr] ;
   int        ncar = nclust_tot[ipthr] ;
   Xcluster *xc ;
   int cc , pp,npt , ijk,vin ;

   for( cc=0 ; cc < ncar ; cc++ ){                    /* loop over clusters */
     xc = xcar[cc] ; if( xc == NULL ) continue ;
     npt = xc->npt ;
     for( pp=0 ; pp < npt ; pp++ ){          /* loop over pts inside cluster */
       ijk = xc->ijk[pp] ;                            /* index of pt in grid */
       if( ijk >= ijkbot && ijk <= ijktop ){  /* is point inside our region? */
         vin = ijk_to_vec[ijk] ;                           /* find its index */
         if( vin >= 0 ){
           ADDTO_Xvector(fomvec[vin],xc->fom) ; /* add to vector of FOM vals */
         }
       }
     }
   }

   return ;
}

/*--------------------------------------------------------------------------*/
/* get median of number of counts in voxel fomvec's touched by this cluster */

static int    *ncount_g ;
static float **fcount_g ;

int get_Xcluster_nbcount( Xcluster *xc , int ithr )
{
   int ii ; float *fc ;

   /* trivial cases */

   if( xc == NULL || xc->npt == 0 ) return 0 ;
   if( xc->npt == 1 ){
     xc->nbcount = fomvec[ijk_to_vec[xc->ijk[0]]]->npt ;
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
     fc[ii] = fomvec[ijk_to_vec[xc->ijk[ii]]]->npt ;  /* count */

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
   float a0,a1,at ;

   a0 = logf(-logf(1.0f-alpha0)) ;
   a1 = logf(-logf(1.0f-alpha1)) ;
   at = logf(-logf(1.0f-alphat)) ;
   return (x0 + (x1-x0)/(a1-a0)*(at-a0)) ;  /* linear interp in a-space */
}

/*---------------------------------------------------------------------------*/
/* this is too complicated, and should be function-ized :( */

int main( int argc , char *argv[] )
{
   int qpthr , ii,xx,yy,zz,ijk , dijk ;
   int ndilstep , ndilated[4] , ndilsum ;
   int count_targ100 , count_targ80, count_targ50 ;
   THD_3dim_dataset *qset=NULL ;
   float *qar=NULL , *gthresh=NULL ;
   char qpr[32] ;
   MRI_IMAGE *cim ; MRI_IMARR *cimar ; float **car , *farar ;
   int nfomkeep , nfar , itrac , ithresh ;
   float tfrac , farperc,farcut , tfracold,farpercold,ttemp ;

   /*----- help me if you can -----*/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf("\n"
       "God only knows what this program does (if anything).\n") ;

     printf("\n"
       " -inset     mask sdata  {MANDATORY} [e.g., from 3dtoXdataset]\n"
       " -NN        1 or 2 or 3 [-NN1 or -NN2 or -NN3 will work]\n"
       " -sid       1 or 2      [-1sid or -2sid will work]\n"
       " -prefix    something\n"
       " -fixed     pvalue clustersize\n"
       " -pthr      list of values [default = 0.0100 0.0056 0.0031 0.0018 0.0010]\n"
       " -FOMcount  turn on FOMcount output\n"
       " -noFARvox  turn off FARvox output\n"
     ) ;
     exit(0) ;
   }

   (void)COX_clock_time() ;

   mainENTRY("3dClustSimX") ; machdep() ;
   AFNI_logger("3dClustSimX",argc,argv);
   PRINT_VERSION("3dClustSimX"); AUTHOR("Who Cares?") ;

   /*----- load command line options -----*/

   get_options(argc,argv) ;

   if( do_fixed ){
     do_FARvox = 1 ;
     INFO_message("-fixed turns FARvox output back on :)") ;
   }

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
       INFO_message("3dClustSimX: Using %d OpenMP threads",nthr) ;
     else
       INFO_message("3dClustSimX: Using 1 thread -- this will be slow :-(") ;
   }

   /*--- skip all the cluster size thresholding calculations,
         and just compute the final result for the fixed thresholds;
         this is for testing and comparison with the "old" methods  ---*/

   if( do_fixed ) goto FINAL_STUFF ;

   /*--- initialize the cluster arrays (one for each p-value thresh) ---*/
   /*--- Xclustar_g[qpthr][iter] = array of clusters at p-value
                                   pthr[qpthr] for realization iter  ---*/

   Xclustar_g = (Xcluster_array ***)malloc(sizeof(Xcluster_array **)*npthr) ;
   for( qpthr=0 ; qpthr < npthr ; qpthr++ )
     Xclustar_g[qpthr] = (Xcluster_array **)malloc(sizeof(Xcluster_array *)*niter) ;

   /*=================================================================*/
   /*--- STEP 1a: loop over realizations to load up Xclustar_g[][] ---*/

   if( verb )
     INFO_message("STEP 1: start %d-sided clustering with NN=%d",nnsid,nnlev) ;

 AFNI_OMP_START ;      /*------------ start parallel section ----------*/
#pragma omp parallel
 { DECLARE_ithr ;
   int iter , ipthr ;
   MRI_IMAGE *fim , *tfim ; float *far , *tfar ;

   /* initialize thread-specific workspace images */

#pragma omp critical
   { fim  = mri_new_conforming( imtemplate , MRI_float ) ;
     tfim = mri_new_conforming( fim        , MRI_float ) ; }
   far  = MRI_FLOAT_PTR(fim) ;
   tfar = MRI_FLOAT_PTR(tfim) ;

   /* clusterize each volume at each p-value threshold */

#pragma omp for schedule(dynamic,200)
   for( iter=0; iter < niter ; iter++ ){ /* loop over realizations */
     generate_image( far , iter ) ;
     for( ipthr=0 ; ipthr < npthr ; ipthr++ ){  /* over thresholds */
       /* creates Xclustar_g[qpthr][iter] */
       gather_clusters( ipthr, far, tfim, nnlev,nnsid, iter ) ;
     }
   }

   /* delete thread-specific workspace */

#pragma omp critical
   { mri_free(fim) ; mri_free(tfim) ; }
 }
 AFNI_OMP_END ;       /*---------- end parallel section ----------*/

   /*=====================================================================*/
   /* STEP 1b: count num clusters total, and merge them into one big list */

   { int qter, qq,pp ; Xcluster_array *xcar ;
     nclust_tot = (int *)malloc(sizeof(int)*npthr) ;
     Xclust_tot = (Xcluster ***)malloc(sizeof(Xcluster_array **)*npthr) ;
     nclust_max = 0 ;

     for( qpthr=0 ; qpthr < npthr ; qpthr++ ){  /* loop over p-value thresh */
       nclust_tot[qpthr] = 0 ;
       for( qter=0 ; qter < niter ; qter++ ){   /* count total clusters */
         xcar = Xclustar_g[qpthr][qter] ;
         if( xcar != NULL ) nclust_tot[qpthr] += xcar->nclu ;
       }
       /* max number of clusters across all p-value thresh */
       if( nclust_tot[qpthr] > nclust_max ) nclust_max = nclust_tot[qpthr] ;
#if 0
       ININFO_message("pthr=%.5f has %d total clusters",pthr[qpthr],nclust_tot[qpthr]);
#endif

       /* assemble all clusters at this p-value thresh into one big array */

       Xclust_tot[qpthr] = (Xcluster **)malloc(sizeof(Xcluster *)*nclust_tot[qpthr]) ;
       for( pp=qter=0 ; qter < niter ; qter++ ){    /* loop over realizations */
         xcar = Xclustar_g[qpthr][qter] ; if( xcar == NULL ) continue ;
         for( qq=0 ; qq < xcar->nclu ; qq++ ){ /* over clusts frm realization */
           Xclust_tot[qpthr][pp++] = xcar->xclu[qq] ;     /* add to big array */
         }
         free(xcar->xclu) ; free(xcar) ;
       }
       free(Xclustar_g[qpthr]) ;
     }
     free(Xclustar_g) ; Xclustar_g = NULL ; /* not needed no more */
   }

   /*============================================================================*/
   /*--- STEP 1c: find the global distributions [not needed but fun] ------------*/

#define GTHRESH_FAC 0.1f

   { int nfom,jj; Xcluster **xcc;
     float a0,a1,f0,f1,ft ;
     float *fomg=calloc(sizeof(float),nclust_max);

     gthresh = (float *)calloc(sizeof(float),npthr) ;
     for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
       xcc = Xclust_tot[qpthr] ;
       for( nfom=ii=0 ; ii < nclust_tot[qpthr] ; ii++ ){
         if( xcc[ii] != NULL ) fomg[nfom++] = xcc[ii]->fom ;
       }
       if( nfom < 100 ) continue ;  /* should not happen */
       qsort_float_rev( nfom, fomg ) ;
       jj = (int)(0.05f*niter) ;
       a0 = ((float)jj)/((float)niter) ; f0 = fomg[jj] ;
       a1 = a0 + 1.0f/((float)niter) ;   f1 = fomg[jj+1] ;
       ft = gthresh[qpthr] = GTHRESH_FAC * inverse_interp_extreme( a0,a1,0.05f, f0,f1 ) ;
       if( verb )
         ININFO_message("pthr=%.5f gets min threshold %.1f [nfom=%d]",
                        pthr[qpthr],ft,nfom) ;
     }
     free(fomg) ;
   }

   /*==========================================================================*/
   /*--- STEP 2: dilate the clusters ------------------------------------------*/
   /*    The goal is to have at least so many "hits" at each voxel in the mask */

   /*--- initialize the FOM vector array for each voxel ---*/
   /*--- this is the list of FOMs (e.g., cluster sizes)
         that were found at that voxel                  ---*/

   fomvec = (Xvector **)malloc(sizeof(Xvector *)*mask_ngood) ;
   for( ii=0 ; ii < mask_ngood ; ii++ ){
     CREATE_Xvector(fomvec[ii],100) ;
     fomvec[ii]->ip  = ipmask[ii] ;   /* store location of this */
     fomvec[ii]->jp  = jpmask[ii] ;   /* vector in the 3D grid */
     fomvec[ii]->kp  = kpmask[ii] ;
     fomvec[ii]->ijk = ijkmask[ii] ;
   }

   /* temp space to hold counts of FOM vec length for each voxel in a cluster */

   ncount_g = (int *)   malloc(sizeof(int)    *nthr) ; /* dimens of fcount_g */
   fcount_g = (float **)malloc(sizeof(float *)*nthr) ; /* counts */
   for( ii=0 ; ii < nthr ; ii++ ){
     fcount_g[ii] = (float *)malloc(sizeof(float)*DALL) ;
     ncount_g[ii] = DALL ;
   }

   /*--- make thread-specific workspaces for clustering ---*/

   ndilg    = (int * )malloc(sizeof(int  )*nthr) ; /* size of dilg_ijk */
   dilg_ijk = (int **)malloc(sizeof(int *)*nthr) ; /* candidates for dilate */
   for( ii=0 ; ii < nthr ; ii++ ){
     ndilg   [ii] = DALL ;
     dilg_ijk[ii] = (int *)malloc(sizeof(int)*DALL) ;
   }

   /*----- loop over p-thresholds and dilate in voxel chunks -----*/

                      dijk = nxyz / nthr ;  /* set chunk size */
   if( dijk > 2*nxy ) dijk = 2*nxy ;
   if( dijk > 16384 ) dijk = 16384 ;

   /* target counts for voxel "hits" */

   count_targ100 = (int)rintf(0.0100f*niter) ;
   if( count_targ100 > 200 )
     count_targ100 = (int)rintf(sqrtf(200.0f*count_targ100)) ;
   count_targ80  = (int)rintf(0.80f*count_targ100) ;
   count_targ50  = (int)rintf(0.50f*count_targ100) ;

   if( verb )
     INFO_message("STEP 2: start cluster dilation") ;

   for( qpthr=0 ; qpthr < npthr ; qpthr++ ){  /* loop over p-value thresh */

#define NDILMAX 9  /* max number of dilation steps */

     for( ndilstep=0 ; ndilstep < NDILMAX ; ndilstep++ ){
       /* initialize counts of number of dilations for each NN type */
       ndilated[0] = ndilated[1] = ndilated[2] = ndilated[3] = 0 ;

 AFNI_OMP_START ;      /*------------ start parallel section ----------*/
#pragma omp parallel
 {  DECLARE_ithr ;
    int iter, idil , ijkbot,ijktop , nclu=nclust_tot[qpthr] ;

    /* reset the FOM vector for each voxel */

#pragma omp for
    for( iter=0 ; iter < mask_ngood ; iter++ ) fomvec[iter]->npt = 0 ;

    /* load the FOM vectors (parallelized across 3D segments) */

#pragma omp for schedule(dynamic,1)
    for( ijkbot=0 ; ijkbot < nxyz ; ijkbot+=dijk ){
      ijktop = ijkbot + (dijk-1) ;
      process_clusters_to_Xvectors( ijkbot, ijktop , qpthr ) ;
    }

    /* get median FOM counts in each cluster,
       then determine how to dilate (NN1, NN2, NN3)
       [parallelized across clusters = simulation iterations] */

#pragma omp for schedule(dynamic,200)
    for( iter=0 ; iter < nclu ; iter++ ){
      if( Xclust_tot[qpthr][iter]          != NULL          &&
          Xclust_tot[qpthr][iter]->nbcount <  count_targ100   ){
        idil = get_Xcluster_nbcount( Xclust_tot[qpthr][iter] , ithr ) ;
        if( idil < count_targ100 ){  /* too few? */
          /* dilate:                   NN3 or                     NN2 or NN1 */
          idil = (idil < count_targ50) ? 3 :(idil < count_targ80) ? 2 :    1 ;
          dilate_Xcluster( Xclust_tot[qpthr][iter] , idil , ithr ) ;
#pragma omp atomic
          ndilated[idil]++ ;  /* count how many dilations of each type */
        }
      }
    }

 }
 AFNI_OMP_END ;       /*---------- end parallel section ----------*/

       /* number of significant dilations (NN2+NN3) */

       ndilsum = ndilated[2] + ndilated[3] ;

       /* if not very many, then we are done with this p-value thresh */

       if( ndilstep < NDILMAX-1 && ndilsum < niter/50 ) break ;
     } /* end of loop over dilation steps */
   } /* end of loop over p-value thresh cluster collection */

   /* free the counting workspace for each thread */

   for( ii=0 ; ii < nthr ; ii++ ) free(dilg_ijk[ii]) ;
   free(ndilg ); free(dilg_ijk );

   /*=======================================================*/
   /*--- STEP 3: create sorted and truncated FOM vectors ---*/

   if( verb )
     INFO_message("STEP 3: re-loading & sorting FOM vectors after final dilations") ;

   /*--- initialize the final sorted FOM vector array for each voxel ---*/

   fomsort = (Xvector ***)malloc(sizeof(Xvector **)*npthr) ;
   for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
     fomsort[qpthr] = (Xvector **)malloc(sizeof(Xvector *)*mask_ngood) ;
   }

#define TOPFRAC 0.0222f
   nfomkeep = (int)(TOPFRAC*niter) ; /* max number of FOMs to keep at 1 voxel */

   for( qpthr=0 ; qpthr < npthr ; qpthr++ ){ /* loop over p-value thresh */

 AFNI_OMP_START ;      /*------------ start parallel section ----------*/
# pragma omp parallel
   { DECLARE_ithr ;
     int ijkbot,ijktop , iv,jj , npt , nclu=nclust_tot[qpthr] ;
     float a0,a1,f0,f1,ft ;

     /* re-load FOM vectors for this threshold */

#pragma omp for
    for( iv=0 ; iv < mask_ngood ; iv++ ) fomvec[iv]->npt = 0 ;

#pragma omp for schedule(dynamic,1)
     for( ijkbot=0 ; ijkbot < nxyz ; ijkbot+=dijk ){
       ijktop = ijkbot + (dijk-1) ;
       process_clusters_to_Xvectors( ijkbot, ijktop , qpthr ) ;
     }

     /* delete Xclusters for this threshold,
        since we have the finalized FOM vectors */

#pragma omp master
     { for( jj=0 ; jj < nclu ; jj++ ){
         DESTROY_Xcluster(Xclust_tot[qpthr][jj]) ;
       }
       free(Xclust_tot[qpthr]) ; Xclust_tot[qpthr] = NULL ;
     }
#pragma omp barrier

     /* sort/truncate FOM vectors for this threshold */

     /* vectors loaded for each pt in space;
        now sort them (biggest first) to determine equitable thresholds */

#pragma omp for schedule(dynamic,200)
     for( iv=0 ; iv < mask_ngood ; iv++ ){
       if( fomvec[iv]->npt < 3 ){          /* if it's way short */
         ADDTO_Xvector(fomvec[iv],0.01f) ; /* put some padding in */
         ADDTO_Xvector(fomvec[iv],0.01f) ;
         ADDTO_Xvector(fomvec[iv],0.01f) ;
       }

       /* sort into decreasing order (largest FOMs first) */

       qsort_float_rev( fomvec[iv]->npt , fomvec[iv]->far ) ;

       /* how many to keep */

       jj = MIN(fomvec[iv]->npt,nfomkeep) ;

       /* create a new vector to get the keeper FOM counts */

       CREATE_Xvector(fomsort[qpthr][iv],jj) ;
       fomsort[qpthr][iv]->ip  = ipmask[iv] ;  /* location */
       fomsort[qpthr][iv]->jp  = jpmask[iv] ;
       fomsort[qpthr][iv]->kp  = kpmask[iv] ;
       fomsort[qpthr][iv]->ijk = ijkmask[iv] ;
       fomsort[qpthr][iv]->npt = jj ;          /* count */
       AAmemcpy(fomsort[qpthr][iv]->far,fomvec[iv]->far,sizeof(float)*jj) ;
     }
   }
 AFNI_OMP_END ;       /*---------- end parallel section ----------*/

     /* save dataset of fomvec counts */

     if( do_FOMcount ){
       qset = EDIT_empty_copy(mask_dset) ;
       sprintf(qpr,".FOMcount.%d",qpthr) ;
       EDIT_dset_items( qset ,
                          ADN_prefix , modify_afni_prefix(prefix,NULL,qpr) ,
                          ADN_nvals  , 1 ,
                        ADN_none ) ;
       EDIT_substitute_brick( qset , 0 , MRI_float , NULL ) ;
       qar = DSET_ARRAY(qset,0) ;
       for( ii=0 ; ii < mask_ngood ; ii++ ){
         ijk = ijkmask[ii] ;
         qar[ijk] = (float)fomvec[ii]->npt ;
         fomvec[ii]->npt = 0 ;
       }
       EDIT_BRICK_LABEL(qset,0,"FOMcount") ;
       tross_Make_History( "3dClustSimX" , argc,argv , qset ) ;
       DSET_write(qset); WROTE_DSET(qset);
       DSET_delete(qset); qset = NULL; qar = NULL;
     }

   } /*----- end of loop over qpthr -----*/

   /* destroy the original unsorted FOM vectors */

   for( ii=0 ; ii < mask_ngood ; ii++ )  /* don't need no more */
     DESTROY_Xvector(fomvec[ii]) ;
   free(fomvec) ; fomvec = NULL ;

   /*========================================================*/
   /*--- STEP 4: test FOM count thresholds to find FAR=5% ---*/

#define FARP_GOAL 5.00f  /* 5 percent */

FINAL_STUFF:

   /* create voxel-specific FOM threshold images (for each p-value thresh) */

   INIT_IMARR(cimar) ;
   car = (float **)malloc(sizeof(float *)*npthr) ;
   for( qpthr=0; qpthr < npthr ; qpthr++ ){
     cim = mri_new_conforming( imtemplate , MRI_float ) ;
     ADDTO_IMARR(cimar,cim) ;
     car[qpthr] = MRI_FLOAT_PTR(cim) ;
     if( do_fixed ){ /* special case: all values are given by user */
       for( ii=0 ; ii < nxyz ; ii++ ) car[qpthr][ii] = fixed_cluster_threshold;
     }
   }

   if( verb ){
     if( do_fixed )
       INFO_message("Computing FAR count for each voxel for -fixed") ;
     else
       INFO_message("STEP 4: adjusting per-voxel FOM thresholds to reach FAR=%.2f%%",FARP_GOAL) ;
   }

   /* tfrac = FOM count fractional threshold;
              will be adjusted to find the 5% FAR goal */

   tfrac = 0.0004f ; itrac = 0 ;
   farpercold = 0.0f ; tfracold = tfrac ;

   /* array to make map of false alarm count at each voxel */

   farar = (float *)malloc(sizeof(float)*nxyz) ;

FARP_LOOPBACK:
   {
     itrac++ ;                                        /* number of iterations */
     nfar = 0 ;                                            /* total FAR count */
     ithresh = (int)(tfrac*niter) ;                    /* FOM count threshold */
     /* we take the ithresh-th largest FOM at each voxel as its FOM threshold */

     if( !do_fixed ){
       if( verb )
         ININFO_message("Testing threshold images at %g ==> %d",tfrac,ithresh) ;
       for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
         AAmemset(car[qpthr],0,sizeof(float)*nxyz) ;
       }
     }
     AAmemset(farar,0,sizeof(float)*nxyz) ;

 AFNI_OMP_START ;     /*------------ start parallel section ----------*/
#pragma omp parallel
 {  DECLARE_ithr ;
    MRI_IMAGE *fim , *tfim ; float *far , *tfar ;
    int iter,npt,iv,jthresh , ipthr , qq ;
    float a0,a1,f0,f1,ft ;

    /* workspace array for each thread */

#pragma omp critical
   { fim = mri_new_conforming( imtemplate , MRI_float ) ;
     far = MRI_FLOAT_PTR(fim) ; }

     /* find the FOM threshold for each voxel, for each p-value thresh */

     if( !do_fixed ){
#pragma omp for
      for( iv=0 ; iv < mask_ngood ; iv++ ){          /* loop over voxels */
        for( ipthr=0 ; ipthr < npthr ; ipthr++ ){ /* over p-value thresh */
          npt = fomsort[ipthr][iv]->npt ;    /* how many FOM values here */
          jthresh = ithresh ;             /* default index of FOM thresh */
          if( jthresh > (int)(0.8f*npt) ) jthresh = (int)(0.8*npt) ;
          /* extract this FOM thresh by interpolation */
          a0 = ((float)jthresh)/((float)niter) ; f0 = fomsort[ipthr][iv]->far[jthresh] ;
          a1 = a0        + 1.0f/((float)niter) ; f1 = fomsort[ipthr][iv]->far[jthresh+1] ;
          ft = inverse_interp_extreme( a0,a1,tfrac , f0,f1 ) ;
          if( ft < 0.123f*gthresh[ipthr] ) ft = 0.123f*gthresh[ipthr] ;
          car[ipthr][ijkmask[iv]] = ft ;  /* = FOM threshold for this voxel */
                                          /* = the goal of this entire program! */
        }
      } /* end of loop over voxels */
     } /* if not fixed */

     /* now, use the thresholds just computed to multi-threshold
        each realization, and create count of total FA and in each voxel */

#pragma omp for schedule(dynamic,100)
     for( iter=0 ; iter < niter ; iter++ ){
       generate_image( far , iter ) ;
       tfim = mri_multi_threshold_Xcluster( fim, npthr,zthr_used,
                                            nnsid,nnlev, cimar, 0, NULL ) ;
       if( tfim != NULL ){   /* nothing found ==> NULL is returned */
         tfar = MRI_FLOAT_PTR(tfim) ;
         for( qq=0 ; qq < nxyz ; qq++ ){
           if( tfar[qq] != 0.0f )
#pragma omp atomic
             farar[qq]++ ;  /* count of FA at this voxel */
         }
#pragma omp critical
         { mri_free(tfim) ; nfar++ ;} /* count of total FA */
       }
     }

#pragma omp critical
     { mri_free(fim) ; }  /* tossing the trash for this thread */
 }
 AFNI_OMP_END ;  /*------------ end parallel section ------------*/

     /* compute the FAR percentage */

     farpercold = farperc ;               /* save what we got last time */
     farperc    = (100.0*nfar)/(float)niter ;  /* what we got this time */
     if( verb )
       ININFO_message("#%2d: False Alarm count = %d  Rate = %.2f%%",
                      itrac,nfar,farperc ) ;

     /* do we need to try another tfrac to get closer to our goal? */

          if( itrac < 5 ) farcut = 0.111f ; /* precision of goal meeting */
     else if( itrac < 9 ) farcut = 0.222f ;
     else                 farcut = 0.333f ;
     if( !do_fixed && itrac < 13 && fabsf(farperc-FARP_GOAL) > farcut ){
       float fff ;
       if( itrac == 1 || (farperc-FARP_GOAL)*(farpercold-FARP_GOAL) > 0.0f ){
         fff = FARP_GOAL/farperc ;
         if( fff > 2.222f ) fff = 2.222f ; else if( fff < 0.450f ) fff = 0.450f ;
/* ININFO_message("   scale tfrac by %g -- fold=%g fnew=%g",fff,farpercold,farperc) ; */
         ttemp = tfrac ; tfrac *= fff ;
       } else {
         fff = (farperc-farpercold)/(tfrac-tfracold) ;
         ttemp = tfrac ; tfrac = tfracold + (FARP_GOAL-farpercold)/fff ;
/* ININFO_message("   interpolate tfrac from told=%g fold=%g tnew=%g fnew=%g",
               tfracold,farpercold,ttemp,farperc) ; */
       }
       tfracold = ttemp ;
       if( tfrac < 0.0001f ) tfrac = 0.0001f ; else if( tfrac > 0.005f ) tfrac = 0.005f ;
       goto FARP_LOOPBACK ;
     }

   }

   /*============================================*/
   /*--- Write stuff out, then quit quit quit ---*/

   if( !do_fixed ){
     qset = EDIT_empty_copy(mask_dset) ;
     sprintf(qpr,".mthresh") ;
     EDIT_dset_items( qset ,
                        ADN_prefix , modify_afni_prefix(prefix,NULL,qpr) ,
                        ADN_nvals  , npthr ,
                      ADN_none ) ;
     for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
       EDIT_substitute_brick( qset , qpthr , MRI_float , NULL ) ;
       qar = DSET_ARRAY(qset,qpthr) ;
       AAmemcpy( qar , car[qpthr] , sizeof(float)*nxyz ) ;
       sprintf(qpr,"FARthr:%.4f",pthr[qpthr]) ;
       EDIT_BRICK_LABEL(qset,qpthr,qpr) ;
     }
     { float *afl=malloc(sizeof(float)*(npthr+3)) ;
       afl[0] = (float)nnlev ;
       afl[1] = (float)nnsid ;
       afl[2] = (float)qpthr ;
       for( qpthr=0 ; qpthr < npthr ; qpthr++ )
         afl[qpthr+3] = zthr_used[qpthr] ;
       THD_set_float_atr( qset->dblk ,
                          "MULTI_THRESHOLDS" , npthr+3 , afl ) ;
       free(afl) ;
     }
     tross_Make_History( "3dClustSimX" , argc,argv , qset ) ;
     DSET_write(qset); WROTE_DSET(qset);
     DSET_delete(qset); qset = NULL; qar = NULL;
   }

   if( do_FARvox ){
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
     tross_Make_History( "3dClustSimX" , argc,argv , qset ) ;
     DSET_write(qset); WROTE_DSET(qset);
     DSET_delete(qset); qset = NULL; qar = NULL;
   }

   if( verb ) INFO_message("Elapsed time = %.1f s",COX_clock_time()) ;
   exit(0) ;
}
