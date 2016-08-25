#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#include "mri_threshX.c"  /* lots of important stuff */

/*---------------------------------------------------------------------------*/
/*
  Global data
*/

static THD_3dim_dataset  *mask_dset  = NULL ; /* mask dataset */
static byte              *mask_vol   = NULL;  /* mask volume */
static int mask_nvox = 0, mask_ngood = 0;     /* number of good voxels in mask volume */

#define INMASK(ijk) (mask_vol[ijk]!=0)

/* 3D indexes for each point in the mask */

static ind_t *ipmask=NULL , *jpmask=NULL , *kpmask=NULL ;
static int   *ijkmask=NULL ;

/* map from 1D index in volume to points in the mask */

static int *ijk_to_vec=NULL ;

static int   nx ;     /* 3D grid stuff */
static int   ny ;
static int   nz ;
static int   nxy ;
static int   nxyz ;
static int   nxyz1 ;
static float dx ;
static float dy ;
static float dz ;
static int   niter ;  /* number of iterations */

#define PMAX 0.5

static double pthr_init[5] = { 0.0100, 0.0056, 0.0031, 0.0018, 0.0010 } ;
static double athr_init[5] = { 0.05 , 0.04 , 0.03 , 0.02 , 0.01 } ;

static int    npthr = 5 ;
static double *pthr = NULL ;

static float  *zthr_1sid = NULL ;
static float  *zthr_2sid = NULL ;

static int    nathr = 5 ;
static double *athr = NULL ;

static int verb = 1 ;
static int nthr = 1 ;  /* default number of threads */

static int nnlev = 1 ;
static int nndil = 0 ;

#undef DECLARE_ithr
#ifdef USE_OMP
# define DECLARE_ithr const int ithr=omp_get_thread_num()
#else
# define DECLARE_ithr const int ithr=0
#endif

static const int min_mask = 1024 ;
static const int min_nvol = 10000 ;

static char *prefix = "Xsim.nii" ;

static MRI_IMAGE *imtemplate = NULL ;

#undef  PSMALL
#define PSMALL 1.e-15

static char *cmd_fname = "3dClustSim.cmd" ;

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

#define SFAC 0.0002f

typedef struct {
  THD_3dim_dataset  *mask_dset ;                        /* mask dataset */
  byte              *mask_vol ;                          /* mask volume */
  int nvox, ngood ;
  ind_t *ipmask, *jpmask, *kpmask ;             /* indexes in 3D and 1D */
  int   *ijkmask ;                               /* for pts in the mask */
  int   *ijk_to_vec ;    /* map from index in volume to pts in the mask */

  int    nvol ;
  short *sdat ;
} Xdataset ;

static Xdataset *xinset=NULL ;

Xdataset * open_Xdataset( char *mask_fname , char *sdat_fname )
{
   Xdataset *xds ; int fdes ; int64_t fsiz ;

   if( mask_fname == NULL || sdat_fname == NULL )
     ERROR_exit("bad inputs to open_Xdataset") ;

   xds = (Xdataset *)calloc(sizeof(Xdataset),1) ;

   /*--- create the mask ---*/

   xds->mask_dset = THD_open_dataset(mask_fname) ;
   if( xds->mask_dset == NULL )
     ERROR_exit("can't open mask dataset '%s'",mask_fname) ;
   DSET_load(xds->mask_dset) ; CHECK_LOAD_ERROR(xds->mask_dset) ;

   xds->mask_vol = THD_makemask( xds->mask_dset , 0 , 1.0,0.0 ) ;
   if( xds->mask_vol == NULL )
     ERROR_exit("can't use -mask dataset '%s'",mask_fname) ;
   DSET_unload(xds->mask_dset) ;

   xds->nvox = DSET_NVOX(xds->mask_dset) ;
   xds->ngood = THD_countmask( xds->nvox , xds->mask_vol ) ;

   if( xds->ngood < min_mask ){
     if( min_mask > 2 && xds->ngood > 2 ){
       ERROR_message("mask has only %d nonzero voxels; minimum allowed is %d.",
                     xds->ngood , min_mask ) ;
       ERROR_exit("Cannot continue -- may we meet under happier circumstances!") ;
     } else if( xds->ngood == 0 ){
       ERROR_exit("mask has no nonzero voxels -- cannot use this at all :-(") ;
     } else {
       ERROR_exit("mask has only %d nonzero voxel%s -- cannot use this :-(",
                  xds->ngood , (xds->ngood > 1) ? "s" : "\0" ) ;
     }
   }

   /*--- open data file with the short-ized and mask-ized data ---*/

   fsiz = (int64_t)THD_filesize(sdat_fname) ;  /* in bytes */
   if( fsiz == 0 )
     ERROR_exit("can't find any data in file '%s'",sdat_fname) ;

   xds->nvol = (int)( fsiz /(sizeof(short)*xds->ngood) ) ;  /* num volumes */
   if( xds->nvol < min_nvol )                               /* in file */
     ERROR_exit("data file '%s' isn't long enough",sdat_fname) ;

   fdes = open( sdat_fname , O_RDONLY ) ;   /* open, get file descriptor */
   if( fdes < 0 )
     ERROR_exit("can't open data file '%s'",sdat_fname) ;

   /* memory map the data file */

   xds->sdat = (short *)mmap( 0 , (size_t)fsiz , PROT_READ, THD_MMAP_FLAG, fdes, 0 ) ;
   close(fdes) ;
   if( xds->sdat == (short *)(-1) )
     ERROR_exit("can't mmap() data file '%s' -- memory space exhausted?",sdat_fname) ;

   /* page fault the data into memory */

   { int64_t ii,sum=0 ; char *bdat = (char *)xds->sdat ;
     for( ii=0 ; ii < fsiz ; ii+=128 ) sum += (int64_t)bdat[ii] ;
   }

   /* e finito */

   return xds ;
}

/*---------------------------------------------------------------------------*/

void load_from_Xdataset( Xdataset *xds , int ival , float *far )
{
   int ii,jj ; short *spt ;

   if( ival >= xds->nvol)
     ERROR_exit("load_from_Xdataset: ival=%d nvol=%d",ival,xds->nvol) ;

/* fprintf(stderr,"load ival=%d",ival) ; */

   AAmemset( far , 0 , sizeof(float)*xds->nvox ) ;
/* fprintf(stderr," a") ; */
   spt = xds->sdat + ((size_t)ival)*((size_t)xds->ngood) ;
/* fprintf(stderr,"b") ; */
   for( ii=0 ; ii < xds->ngood ; ii++ ){
/* if( xds->ijkmask[ii] < 0 || xds->ijkmask[ii] >= xds->nvox ) */
/* fprintf(stderr,"[ii=%d ijk=%d]",ii,xds->ijkmask[ii]) ; */
     jj = xds->ijkmask[ii] ;
     far[jj] = SFAC * spt[ii] ;
   }
/* fprintf(stderr,"c\n") ; */
   return ;
}

/*---------------------------------------------------------------------------*/
/* Routine to initialize the input options (values are in global variables). */

void get_options( int argc , char **argv )
{
  char * ep;
  int nopt=1 , ii , have_pthr=0;

ENTRY("get_options") ;

  /*----- add to program log -----*/

  pthr = (double *)malloc(sizeof(double)*npthr) ;
  AAmemcpy( pthr , pthr_init , sizeof(double)*npthr ) ;

  athr = (double *)malloc(sizeof(double)*nathr) ;
  AAmemcpy( athr , athr_init , sizeof(double)*nathr ) ;

  while( nopt < argc ){

    /*-----*/

    if( strcmp(argv[nopt],"-NN") == 0 ){
      if( ++nopt >= argc )
        ERROR_exit("You need 1 argument after option '-NN'") ;
      nnlev = (int)strtod(argv[nopt],NULL) ;
      if( nnlev < 1 || nnlev > 3 )
        ERROR_exit("-NN must be 1 or or 3 :(") ;
      nopt++ ; continue ;
    }

    /*-----  -inset mask sdata  -----*/

    if( strcmp(argv[nopt],"-inset") == 0 ){  /* 02 Feb 2016 */
      int ii,nbad=0 ; THD_3dim_dataset *qset ;
      if( xinset != NULL )
        ERROR_exit("You can't use '-inset' more than once!") ;
      if( ++nopt >= argc-1 )
        ERROR_exit("You need 2 arguments after option '-inset'") ;

      xinset = open_Xdataset( argv[nopt], argv[nopt+1] ) ;

      nopt+=2 ; continue ;
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

#define INSET_PRELOAD

  if( xinset == NULL ) ERROR_exit("-inset option is mandatory :(") ;

  nx = DSET_NX(xinset->mask_dset) ;
  ny = DSET_NY(xinset->mask_dset) ;
  nz = DSET_NZ(xinset->mask_dset) ;
  dx = fabsf(DSET_DX(xinset->mask_dset)) ;
  dy = fabsf(DSET_DY(xinset->mask_dset)) ;
  dz = fabsf(DSET_DZ(xinset->mask_dset)) ;

  mask_dset  = xinset->mask_dset ;
  imtemplate = DSET_BRICK(xinset->mask_dset,0) ;
  niter      = xinset->nvol ;
  mask_ngood = xinset->ngood ;
  mask_vol   = xinset->mask_vol ;
  INFO_message("-inset has %d volumes; mask has %d points",niter,mask_ngood) ;

  nxy = nx*ny ; nxyz = nxy*nz ; nxyz1 = nxyz - nxy ;
  if( nxyz < 1024 )
    ERROR_exit("Only %d voxels in simulation?! Need at least 1024.",nxyz) ;

  /* make a list of the i,j,k coordinates of each point in the mask */

  { int pp,qq , xx,yy,zz ;
    ipmask = xinset->ipmask = (ind_t *)malloc(sizeof(ind_t)*mask_ngood) ;
    jpmask = xinset->jpmask = (ind_t *)malloc(sizeof(ind_t)*mask_ngood) ;
    kpmask = xinset->kpmask = (ind_t *)malloc(sizeof(ind_t)*mask_ngood) ;
    ijkmask= xinset->ijkmask= (int *  )malloc(sizeof(int)  *mask_ngood) ;
    for( pp=qq=0 ; qq < nxyz ; qq++ ){
      if( INMASK(qq) ){
        IJK_TO_THREE(qq,xx,yy,zz,nx,nxy) ;
        ipmask[pp] = (ind_t)xx; jpmask[pp] = (ind_t)yy; kpmask[pp] = (ind_t)zz;
        ijkmask[pp] = qq ;
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

  EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Generate random smoothed masked image, with stdev=1. */

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
/* Get a NN{nnlev}_1sided cluster array at a particular threshold (ipthr),
   at a particular iteration (iter), and save it into the global cluster
   collection Xclustar_g.
*//*-------------------------------------------------------------------------*/

void gather_clusters_1sid( int ipthr, float *fim, MRI_IMAGE *tfim, int nnlev, int iter, int ithr )
{
  register int ii ; register float thr ; float *tfar = MRI_FLOAT_PTR(tfim) ;

  thr = zthr_1sid[ipthr] ;
  for( ii=0 ; ii < nxyz ; ii++ )
    tfar[ii] = (fim[ii] > thr) ? fim[ii] : 0.0f ;

#if 0
#pragma omp critical
 { fprintf(stderr,"    + ipthr=%d iter=%d ithr=%d thr=%g\n",ipthr,iter,ithr,thr); }
#endif

  Xclustar_g[ipthr][iter] = find_Xcluster_array( tfim , nnlev , NULL ) ;

#if 0
#pragma omp critical
 { fprintf(stderr,"    - ipthr=%d iter=%d ithr=%d\n",ipthr,iter,ithr); }
#endif

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
   qsort_int(ntry,ijkar) ;

   /* for each new point:
        compare to all other points in the cluster
        if it isn't any of them, add it to the cluster */

   for( jj=0 ; jj < ntry ; jj++ ){
     ijk = ijkar[jj] ; if( !INMASK(ijk) ) continue ;   /* not a useful pt */
     if( jj > 0 && ijk==ijkar[jj-1] ) continue ;          /* not a new pt */
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

/*---------------------------------------------------------------------------*/
/* get median of the number of counts in fomvec's touched by this cluster */

static int    *ncount_g ;
static float **fcount_g ;

int get_Xcluster_nbcount( Xcluster *xc , int ithr )
{
   int nc,ii ; float *fc ;

   if( xc == NULL || xc->npt == 0 ) return 0 ;
   if( xc->npt == 1 ){
     xc->nbcount = fomvec[ijk_to_vec[xc->ijk[0]]]->npt ;
     return xc->nbcount ;
   }

   if( ncount_g[ithr] < xc->npt ){
     ncount_g[ithr] = xc->npt + DALL ;
     fcount_g[ithr] = (float *)realloc(fcount_g[ithr],
                                       sizeof(float)*ncount_g[ithr]) ;
   }
   nc = ncount_g[ithr] ;
   fc = fcount_g[ithr] ;
   for( ii=0 ; ii < xc->npt ; ii++ )
     fc[ii] = fomvec[ijk_to_vec[xc->ijk[ii]]]->npt ;

   xc->nbcount = (int)qmed_float(xc->npt,fc) ;
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

static __inline__ float
  inverse_interp_extreme( float alpha0 , float alpha1 , float alphat ,
                          float x0     , float x1                      )
{
   float a0,a1,at ;

   a0 = logf(-logf(1.0f-alpha0)) ;
   a1 = logf(-logf(1.0f-alpha1)) ;
   at = logf(-logf(1.0f-alphat)) ;
   return (x0 + (x1-x0)/(a1-a0)*(at-a0)) ;  /* linear interp in a-space */
}

/*---------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int qpthr , ii,xx,yy,zz,ijk , dijk ;
   int ndilstep , ndilated[4] , ndilsum ;
   int count_targ100 , count_targ80, count_targ50 ;
   THD_3dim_dataset *qset=NULL ;
   float *qar=NULL , *gthresh=NULL ;
   char qpr[32] ;
   MRI_IMAGE *cim ; MRI_IMARR *cimar ; float **car , *farar ;
   int nfomkeep , nfar , itrac , ithresh ; float tfrac , farperc ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf("\n"
       "God only knows what this program does (if anything).\n") ;

     printf("\n"
       " -NN     1 or 2 or 3\n"
       " -prefix something\n"
       " -inset  mask sdata\n"
     ) ;
     exit(0) ;
   }

   get_options(argc,argv) ;

   /*----- get the number of threads -----------------------------------*/

 AFNI_OMP_START ;     /*------------ start parallel section ----------*/
#pragma omp parallel
 {
#ifdef USE_OMP
#pragma omp master
   { nthr = omp_get_num_threads() ; }
#else
   nthr = 1 ;
#endif
 }
 AFNI_OMP_END ;       /*---------- end parallel section ----------*/
   if( nthr > 1 )
     INFO_message("3dClustSimX: Using %d OpenMP threads") ;
   else
     INFO_message("3dClustSimX: Using 1 thread -- this will be slow :-(") ;

   /*--- code to initialize the cluster arrays (one for each simulation) ---*/

   Xclustar_g = (Xcluster_array ***)malloc(sizeof(Xcluster_array **)*npthr) ;
   for( qpthr=0 ; qpthr < npthr ; qpthr++ )
     Xclustar_g[qpthr] = (Xcluster_array **)malloc(sizeof(Xcluster_array *)*niter) ;

   /*--- STEP 1a: loop over realizations to load up Xclustar_g[][] ---*/

INFO_message("start 1-sided clustering with NN=%d",nnlev) ;

 AFNI_OMP_START ;      /*------------ start parallel section ----------*/
#pragma omp parallel
 { DECLARE_ithr ;
   int iter , ipthr ;
   MRI_IMAGE *fim , *tfim ; float *far , *tfar ;

   /* code to initialize thread-specific workspace */

#pragma omp critical
   { fim  = mri_new_conforming( imtemplate , MRI_float ) ;
     tfim = mri_new_conforming( fim      , MRI_float ) ; }
   far  = MRI_FLOAT_PTR(fim) ;
   tfar = MRI_FLOAT_PTR(tfim) ;

#pragma omp for schedule(dynamic,200)
   for( iter=0; iter < niter ; iter++ ){ /* loop over realizations */
     generate_image( far , iter ) ;
     for( ipthr=0 ; ipthr < npthr ; ipthr++ ){  /* over thresholds */
       gather_clusters_1sid( ipthr, far, tfim, nnlev, iter , ithr ) ;
     }
   }

#if 1
#pragma omp critical
   { mri_free(fim) ; mri_free(tfim) ; }
#endif
 }
 AFNI_OMP_END ;       /*---------- end parallel section ----------*/

   /* STEP 1b: count number of clusters overall, and merge them  */

   { int qter, qq,pp ; Xcluster_array *xcar ;
     nclust_tot = (int *)malloc(sizeof(int)*npthr) ;
     Xclust_tot = (Xcluster ***)malloc(sizeof(Xcluster_array **)*npthr) ;
     nclust_max = 0 ;
     for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
       nclust_tot[qpthr] = 0 ;
       for( qter=0 ; qter < niter ; qter++ ){
         xcar = Xclustar_g[qpthr][qter] ;
         if( xcar != NULL ) nclust_tot[qpthr] += xcar->nclu ;
       }
#if 0
       ININFO_message("pthr=%.5f has %d total clusters",pthr[qpthr],nclust_tot[qpthr]);
#endif
       if( nclust_tot[qpthr] > nclust_max ) nclust_max = nclust_tot[qpthr] ;
       Xclust_tot[qpthr] = (Xcluster **)malloc(sizeof(Xcluster *)*nclust_tot[qpthr]) ;
       for( pp=qter=0 ; qter < niter ; qter++ ){
         xcar = Xclustar_g[qpthr][qter] ; if( xcar == NULL ) continue ;
         for( qq=0 ; qq < xcar->nclu ; qq++ ){
           Xclust_tot[qpthr][pp++] = xcar->xclu[qq] ;
         }
         free(xcar->xclu) ; free(xcar) ;
       }
       free(Xclustar_g[qpthr]) ;
     }
     free(Xclustar_g) ; Xclustar_g = NULL ;
   }

   /*--- STEP 1c: find the global distributions ---------------------------------*/

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
       ft = gthresh[qpthr] = inverse_interp_extreme( a0,a1,0.05f , f0,f1 ) ;
#if 0
       ININFO_message("5%% FOM for pthr=%.5f is %g (nfom=%d)",pthr[qpthr],ft,nfom) ;
#endif
     }
     free(fomg) ;
   }

   /*--- STEP 2: dilate the clusters -----------------------------------------*/

   /*--- initialize the FOM vector array for each voxel ---*/

   fomvec = (Xvector **)malloc(sizeof(Xvector *)*mask_ngood) ;
   for( ii=0 ; ii < mask_ngood ; ii++ ){
     CREATE_Xvector(fomvec[ii],100) ;
     fomvec[ii]->ip  = ipmask[ii] ;   /* store location of this */
     fomvec[ii]->jp  = jpmask[ii] ;   /* vector in the 3D grid */
     fomvec[ii]->kp  = kpmask[ii] ;
     fomvec[ii]->ijk = ijkmask[ii] ;
   }
   ncount_g = (int *)   malloc(sizeof(int)    *nthr) ;
   fcount_g = (float **)malloc(sizeof(float *)*nthr) ;
   for( ii=0 ; ii < nthr ; ii++ ){
     fcount_g[ii] = (float *)malloc(sizeof(float)*DALL) ;
     ncount_g[ii] = DALL ;
   }

   /*--- make thread-specific workspaces ---*/

   ndilg    = (int * )malloc(sizeof(int  )*nthr) ;
   dilg_ijk = (int **)malloc(sizeof(int *)*nthr) ;
   for( ii=0 ; ii < nthr ; ii++ ){
     ndilg   [ii] = DALL ;
     dilg_ijk[ii] = (int *)malloc(sizeof(int)*DALL) ;
   }

   /*----- loop over p-thresholds -----*/

                      dijk = nxyz / nthr ;
   if( dijk > 2*nxy ) dijk = 2*nxy ;
   if( dijk > 16384 ) dijk = 16384 ;

   count_targ100 = (int)rintf(0.0100f*niter) ;
   count_targ80  = (int)rintf(0.0080f*niter) ;
   count_targ50  = (int)rintf(0.0050f*niter) ;

INFO_message("start cluster dilation") ;

   for( qpthr=0 ; qpthr < npthr ; qpthr++ ){

#define NDILMAX 9

#if 0
     INFO_message("dilation at pthr=%.5f",pthr[qpthr]) ;
#endif
     for( ndilstep=0 ; ndilstep < NDILMAX ; ndilstep++ ){
#if 0
       ININFO_message(" step %d",ndilstep+1) ;
#endif
       ndilated[0] = ndilated[1] = ndilated[2] = ndilated[3] = 0 ;

 AFNI_OMP_START ;      /*------------ start parallel section ----------*/
#pragma omp parallel
 {  DECLARE_ithr ;
    int iter, idil , ijkbot,ijktop , nclu=nclust_tot[qpthr] ;

#if 0
#pragma omp master
    { ININFO_message("  compute FOM vectors") ; }
#endif

    /* reset the FOM vectors */

#pragma omp for
    for( iter=0 ; iter < mask_ngood ; iter++ ) fomvec[iter]->npt = 0 ;

    /* load the FOM vectors (parallelized across 3D segments) */

#pragma omp for schedule(dynamic,1)
    for( ijkbot=0 ; ijkbot < nxyz ; ijkbot+=dijk ){
      ijktop = ijkbot + (dijk-1) ;
      process_clusters_to_Xvectors( ijkbot, ijktop , qpthr ) ;
    }

    /* get median FOM counts in each cluster,
       then determine how to dilate
       (parallelized across clusters = simulation iterations) */

#if 0
#pragma omp master
    { ININFO_message("  dilate clusters") ; }
#endif

#pragma omp for schedule(dynamic,200)
    for( iter=0 ; iter < nclu ; iter++ ){
      if( Xclust_tot[qpthr][iter]          != NULL          &&
          Xclust_tot[qpthr][iter]->nbcount <  count_targ100   ){
        idil = get_Xcluster_nbcount( Xclust_tot[qpthr][iter] , ithr ) ;
        if( idil < count_targ100 ){
          idil = (idil < count_targ50) ? 3 :(idil < count_targ80) ? 2 : 1 ;
          dilate_Xcluster( Xclust_tot[qpthr][iter] , idil , ithr ) ;
#pragma omp atomic
          ndilated[idil]++ ;
        }
      }
    }

 }
 AFNI_OMP_END ;       /*---------- end parallel section ----------*/

       ndilsum = ndilated[2] + ndilated[3] ;
#if 0
       ININFO_message("  dilation counts:  NN1=%d  NN2=%d  NN3=%d  NN2+NN3=%d",
                      ndilated[1] , ndilated[2] , ndilated[3] , ndilsum ) ;
#endif

       if( ndilstep < NDILMAX-1 && ndilsum < niter/50 ){
#if 0
         ININFO_message("- breaking out of dilation") ;
#endif
         break ;
       }
     } /* end of loop over dilation steps */
   }

#if 0
   ININFO_message(" free dilation workspace") ;
#endif
   for( ii=0 ; ii < nthr ; ii++ ) free(dilg_ijk[ii]) ;
   free(ndilg ); free(dilg_ijk );

   /*--- STEP 3: create sorted FOM vectors ---*/

INFO_message("re-loading & sorting FOM vectors after final dilations") ;

   /*--- initialize the final sorted FOM vector array for each voxel ---*/

   fomsort = (Xvector ***)malloc(sizeof(Xvector **)*npthr) ;
   for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
     fomsort[qpthr] = (Xvector **)malloc(sizeof(Xvector *)*mask_ngood) ;
   }

#define TOPFRAC 0.02f
   nfomkeep = (int)(TOPFRAC*niter) ; /* max number of FOMs to keep at 1 voxel */

   for( qpthr=0 ; qpthr < npthr ; qpthr++ ){ /* loop over thresholds */

#if 0
     ININFO_message(" start pthr=%.5f",pthr[qpthr]) ;
#endif

 AFNI_OMP_START ;      /*------------ start parallel section ----------*/
# pragma omp parallel
   { DECLARE_ithr ;
     int ijkbot,ijktop , iv,jj , npt , nclu=nclust_tot[qpthr] ;
     float a0,a1,f0,f1,ft ;

#if 0
#pragma omp master
     { ININFO_message("  A: re-load FOM vectors for this threshold") ; }
#endif

#pragma omp for
    for( iv=0 ; iv < mask_ngood ; iv++ ) fomvec[iv]->npt = 0 ;

#pragma omp for schedule(dynamic,1)
     for( ijkbot=0 ; ijkbot < nxyz ; ijkbot+=dijk ){
       ijktop = ijkbot + (dijk-1) ;
       process_clusters_to_Xvectors( ijkbot, ijktop , qpthr ) ;
     }

#if 0
#pragma omp master
     { ININFO_message("  B: delete Xclusters for this threshold") ; }
#endif

     /* delete all clusters for this qpthr */
#pragma omp master
     { for( jj=0 ; jj < nclu ; jj++ ){
         DESTROY_Xcluster(Xclust_tot[qpthr][jj]) ;
       }
       free(Xclust_tot[qpthr]) ; Xclust_tot[qpthr] = NULL ;
     }
#pragma omp barrier

#if 0
#pragma omp master
     { ININFO_message("  C: sort/truncate FOM vectors for this threshold") ; }
#endif

     /* vectors loaded for each pt in space;
        now sort them (biggest first) to determine equitable thresholds */

#pragma omp for schedule(dynamic,200)
     for( iv=0 ; iv < mask_ngood ; iv++ ){
       if( fomvec[iv]->npt < 3 ){
         ADDTO_Xvector(fomvec[iv],0.01f) ;
         ADDTO_Xvector(fomvec[iv],0.01f) ;
         ADDTO_Xvector(fomvec[iv],0.01f) ;
       }
       qsort_float_rev( fomvec[iv]->npt , fomvec[iv]->far ) ;
       jj = MIN(fomvec[iv]->npt,nfomkeep) ;
       CREATE_Xvector(fomsort[qpthr][iv],jj) ;
       fomsort[qpthr][iv]->ip  = ipmask[iv] ;
       fomsort[qpthr][iv]->jp  = jpmask[iv] ;
       fomsort[qpthr][iv]->kp  = kpmask[iv] ;
       fomsort[qpthr][iv]->ijk = ijkmask[iv] ;
       fomsort[qpthr][iv]->npt = jj ;
       AAmemcpy(fomsort[qpthr][iv]->far,fomvec[iv]->far,sizeof(float)*jj) ;
     }
   }
 AFNI_OMP_END ;       /*---------- end parallel section ----------*/

     /* save datasets of fomvec counts */

#if 0
     ININFO_message("  D: write fomvec counts to dataset") ;
#endif
     qset = EDIT_empty_copy(mask_dset) ;
     sprintf(qpr,".%d",qpthr) ;
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
     DSET_write(qset); WROTE_DSET(qset);
     DSET_delete(qset); qset = NULL; qar = NULL;
#if 0
ININFO_message("  E: loopback") ;
#endif

   } /*----- end of loop over qpthr -----*/


   for( ii=0 ; ii < mask_ngood ; ii++ )  /* don't need no more */
     DESTROY_Xvector(fomvec[ii]) ;
   free(fomvec) ;

   /*--- STEP 4: test trial thresholds ---*/

   INIT_IMARR(cimar) ;
   car = (float **)malloc(sizeof(float *)*npthr) ;
   for( qpthr=0; qpthr < npthr ; qpthr++ ){
     cim = mri_new_conforming( imtemplate , MRI_float ) ;
     ADDTO_IMARR(cimar,cim) ;
     car[qpthr] = MRI_FLOAT_PTR(cim) ;
   }

#define FARP_GOAL 5.00f  /* percent */

   tfrac = 0.0003f ; itrac = 0 ;
   farar = (float *)malloc(sizeof(float)*nxyz) ;

FARP_LOOPBACK:
   {
     itrac++ ; nfar = 0 ; ithresh = (int)(tfrac*niter) ;
     INFO_message("testing threshold images at %g ==> %d",tfrac,ithresh) ;

     for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
       AAmemset(car[qpthr],0,sizeof(float)*nxyz) ;
     }
     AAmemset(farar,0,sizeof(float)*nxyz) ;

 AFNI_OMP_START ;     /*------------ start parallel section ----------*/
#pragma omp parallel
 {  DECLARE_ithr ;
    MRI_IMAGE *fim , *tfim ; float *far , *tfar ;
    int iter,npt,iv,jthresh , ipthr , qq ;
    float a0,a1,f0,f1,ft ;

#pragma omp critical
   { fim = mri_new_conforming( imtemplate , MRI_float ) ;
     far = MRI_FLOAT_PTR(fim) ; }

#pragma omp for
     for( iv=0 ; iv < mask_ngood ; iv++ ){
#if 0
#pragma omp critical
{fprintf(stderr,"    process voxel iv=%d\n",iv);}
#endif
       for( ipthr=0 ; ipthr < npthr ; ipthr++ ){
         npt = fomsort[ipthr][iv]->npt ;
#if 0
#pragma omp critical
{fprintf(stderr,"      iv=%d npt=%d\n",iv,npt);}
#endif
         jthresh = ithresh ;
         if( jthresh > npt/2 ) jthresh = npt/2 ;
#if 0
#pragma omp critical
{fprintf(stderr,"      iv=%d jthresh=%d\n",iv,jthresh);}
#endif
         a0 = ((float)jthresh)/((float)niter) ; f0 = fomsort[ipthr][iv]->far[jthresh] ;
         a1 = a0        + 1.0f/((float)niter) ; f1 = fomsort[ipthr][iv]->far[jthresh+1] ;
         ft = inverse_interp_extreme( a0,a1,tfrac , f0,f1 ) ;
         if( ft < 0.222f*gthresh[ipthr] ) ft = 0.222f*gthresh[ipthr] ;
#if 0
#pragma omp critical
{fprintf(stderr,"      iv=%d ft=%g\n",iv,ft);}
#endif
         car[ipthr][ijkmask[iv]] = ft ;
#if 0
#pragma omp critical
{fprintf(stderr,"      iv=%d ijk=%d\n",iv,ijkmask[iv]);}
#endif
       }
     }

#pragma omp for schedule(dynamic,100)
     for( iter=0 ; iter < niter ; iter++ ){
#if 0
#pragma omp critical
{fprintf(stderr,"    process image iter=%d\n",iter);}
#endif
       generate_image( far , iter ) ;
       tfim = mri_multi_threshold_Xcluster( fim, npthr,zthr_1sid, 1,nnlev, cimar ) ;
       if( tfim != NULL ){
         tfar = MRI_FLOAT_PTR(tfim) ;
         for( qq=0 ; qq < nxyz ; qq++ ){
           if( tfar[qq] != 0.0f )
#pragma omp atomic
             farar[qq]++ ;
         }

#pragma omp critical
         { mri_free(tfim) ; nfar++ ;}
       }
     }

#pragma omp critical
     { mri_free(fim) ; }

 }
 AFNI_OMP_END ;

     farperc = (100.0*nfar)/(float)niter ;
     ININFO_message("False Alarm count = %d   Rate = %.2f%%" ,
                    nfar , farperc ) ;

     if( itrac < 10 && fabsf(farperc-FARP_GOAL) > 0.5f ){
       float fff = FARP_GOAL/farperc ;
       if( fff > 3.0f ) fff = 3.0f ; else if( fff < 0.333f ) fff = 0.333f ;
       tfrac *= fff ;
       if( tfrac < 0.0002f ) tfrac = 0.0002f ; else if( tfrac > 0.004f ) tfrac = 0.004f ;
       goto FARP_LOOPBACK ;
     }

   }

   /*----------*/

   qset = EDIT_empty_copy(mask_dset) ;
   sprintf(qpr,".FAR=%.2f%%",farperc) ;
   EDIT_dset_items( qset ,
                      ADN_prefix , modify_afni_prefix(prefix,NULL,qpr) ,
                      ADN_nvals  , npthr+1 ,
                    ADN_none ) ;
   for( qpthr=0 ; qpthr < npthr ; qpthr++ ){
     EDIT_substitute_brick( qset , qpthr , MRI_float , NULL ) ;
     qar = DSET_ARRAY(qset,qpthr) ;
     AAmemcpy( qar , car[qpthr] , sizeof(float)*nxyz ) ;
   }
   EDIT_substitute_brick( qset , npthr , MRI_float , NULL ) ;
   qar = DSET_ARRAY(qset,npthr) ;
   AAmemcpy( qar , farar , sizeof(float)*nxyz ) ;

   DSET_write(qset); WROTE_DSET(qset);
   DSET_delete(qset); qset = NULL; qar = NULL;

   exit(0) ;
}
