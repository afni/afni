/****************************************************************************
 ***** This file is mean to be #include-d, especially for 3dXClustSim.c *****
 ****************************************************************************/

#include "mrilib.h"

/* get thread number if compiled with OpenMP -- or zero if not */

#undef DECLARE_ithr   /* 30 Nov 2015 */
#ifdef USE_OMP
# define DECLARE_ithr const int ithr=omp_get_thread_num()
#else
# define DECLARE_ithr const int ithr=0
#endif

/*---------------------------------------------------------------------------*/
/* Cluster definition. Index type ind_t can be byte or short. */

#define USE_UBYTE

/*====================================*/
#ifndef IND_T
#ifdef USE_UBYTE  /* for grids <= 255 on each side */

# define DALL    128
# define MAX_IND 255u

  typedef unsigned char ind_t ;

# define IND_T unsigned char

#else   /*==== for grids <= 65535 on each side ====*/

# define DALL    512
# define MAX_IND 32767u

  typedef unsigned short ind_t ;

# define IND_T unsigned short

#endif
#endif
/*====================================*/

/*----- struct to hold a single cluster of points -----*/

typedef struct {
  int npt ,                 /* number of points assigned */
      nall ,                /* number of points allocated */
      norig ,               /* number of original pts (before dilation) */
      nbcount ;             /* for dilation decisions (cf. 3dXClustSim) */
  float fomh[3] ;           /* Figure Of Merit for hpow=0, 1, 2 */
  float cth[3] ;            /* FOM threshold for hpow=0, 1, 2 */
  int hmask ;               /* which h FOM threshs were passed */
  ind_t *ip , *jp , *kp ;   /* 3D indexes for each point */
  int   *ijk ;              /* 1D index for each point */
  int   ijkmin,ijkmax ;     /* smallest and largest ijk[] values */
} Xcluster ;

#define MIN_CLUST 5  /* smallest cluster size allowed (voxels) */

static int min_clust = MIN_CLUST ; /* 21 Sep 2017 */
void set_Xmin_clust( int mc ){ min_clust = MAX(mc,2); return; }

/*----- struct to hold a bunch of clusters -----*/

typedef struct {
  int nclu , nall ;
  Xcluster **xclu ;
} Xcluster_array ;

/*----------------------------------------------------------------------------*/

#define CREATE_Xcluster_array(xcar,siz)                                 \
 do{ (xcar) = (Xcluster_array *)malloc(sizeof(Xcluster_array)) ;        \
     (xcar)->nclu = 0 ; xcar->nall = (siz) ;                            \
     (xcar)->xclu = (Xcluster **)calloc(sizeof(Xcluster *),(siz)) ;     \
 } while(0)

#define ADDTO_Xcluster_array(xcar,xc)                                          \
 do{ if( (xcar)->nclu >= (xcar)->nall ){                                       \
       (xcar)->nall = (xcar)->nclu + 32 ;                                      \
       (xcar)->xclu = (Xcluster **)realloc((xcar)->xclu,                       \
                                           sizeof(Xcluster **)*(xcar)->nall) ; \
     }                                                                         \
     (xcar)->xclu[(xcar)->nclu++] = (xc) ;                                     \
 } while(0)

#define DESTROY_Xcluster_array(xcar)         \
 do{ int cc ;                                \
     for( cc=0 ; cc < (xcar)->nclu ; cc++ )  \
       DESTROY_Xcluster((xcar)->xclu[cc]) ;  \
     free((xcar)->xclu) ; free(xcar) ;       \
 } while(0)

#define MERGE_Xcluster_arrays(xcar,ycar)                                       \
 do{ if( (xcar)->nclu+(ycar)->nclu > (xcar)->nall ){                           \
       (xcar)->nall = (xcar)->nclu+(ycar)->nclu ;                              \
       (xcar)->xclu = (Xcluster **)realloc((xcar)->xclu,                       \
                                           sizeof(Xcluster **)*(xcar)->nall) ; \
     }                                                                         \
     memcpy( (xcar)->xclu+(xcar)->nclu ,                                       \
             (ycar)->xclu , sizeof(Xcluster *)*(ycar)->nclu ) ;                \
     free((ycar)->xclu) ; free(ycar) ;                                         \
 } while(0)

/*----------------------------------------------------------------------------*/
/* Create a cluster with initial array allocation of siz */

#define CREATE_Xcluster(xc,siz)                                        \
 do{ xc = (Xcluster *)malloc(sizeof(Xcluster)) ;                       \
     xc->npt = xc->norig = xc->nbcount = 0 ; xc->nall = (siz) ;        \
     xc->fomh[0] = xc->fomh[1] = xc->fomh[2] = 0.0f ;                  \
     xc->cth [0] = xc->cth [1] = xc->cth [2] = 0.0f ;                  \
     xc->ip  = (ind_t *)malloc(sizeof(ind_t)*(siz)) ;                  \
     xc->jp  = (ind_t *)malloc(sizeof(ind_t)*(siz)) ;                  \
     xc->kp  = (ind_t *)malloc(sizeof(ind_t)*(siz)) ;                  \
     xc->ijk = (int *)  malloc(sizeof(int)  *(siz)) ;                  \
     xc->ijkmin = xc->ijkmax = -666 ;                                  \
 } while(0)

#define DESTROY_Xcluster(xc)                                           \
 do{ if( (xc) != NULL ){                                               \
       free((xc)->ip); free((xc)->jp); free((xc)->kp);                 \
       free((xc)->ijk); free(xc);                                      \
 }} while(0)

#if 0 /************************************************************************/
/*----------------------------------------------------------------------------*/
/* Copy one cluster's data over another's */

void copyover_Xcluster( Xcluster *xcin , Xcluster *xcout )
{
   int nin ;

   if( xcin == NULL || xcout == NULL ) return ;

   nin = xcin->npt ;
   if( nin > xcout->nall ){
     xcout->nall = nin ;
     xcout->ip  = (ind_t *)realloc(xcout->ip ,sizeof(ind_t)*nin) ;
     xcout->jp  = (ind_t *)realloc(xcout->jp ,sizeof(ind_t)*nin) ;
     xcout->kp  = (ind_t *)realloc(xcout->kp ,sizeof(ind_t)*nin) ;
     xcout->ijk = (int *)  realloc(xcout->ijk,sizeof(int)  *nin) ;
   }
   AA_memcpy( xcout->ip , xcin->ip , sizeof(ind_t)*nin ) ;
   AA_memcpy( xcout->jp , xcin->jp , sizeof(ind_t)*nin ) ;
   AA_memcpy( xcout->kp , xcin->kp , sizeof(ind_t)*nin ) ;
   AA_memcpy( xcout->ijk, xcin->ijk, sizeof(int)  *nin ) ;
   xcout->fomh[0] = xcin->fomh[0] ;
   xcout->fomh[1] = xcin->fomh[1] ;
   xcout->fomh[2] = xcin->fomh[2] ;
   xcout->cth [0] = xcin->cth [0] ;
   xcout->cth [1] = xcin->cth [1] ;
   xcout->cth [2] = xcin->cth [2] ;
   xcout->npt     = nin ;
   xcout->norig   = xcin->norig ;
   xcout->nbcount = xcin->nbcount ;
   xcout->ijkmin  = xcin->ijkmin ;
   xcout->ijkmax  = xcin->ijkmax ;
   return ;
}

/*----------------------------------------------------------------------------*/
/* Create a new cluster that is a copy of the input */

Xcluster * copy_Xcluster( Xcluster *xcc )
{
   Xcluster *xccout ;

   if( xcc == NULL ) return NULL ;
   CREATE_Xcluster(xccout,xcc->npt) ;
   copyover_Xcluster( xcc , xccout ) ;
   return xccout ;
}
#endif /***********************************************************************/

/*----------------------------------------------------------------------------*/
/* Add a point to current cluster (if far is nonzero at this point).
   For use only in the function directly below!
*//*--------------------------------------------------------------------------*/

static float **cthar0 = NULL ; /* arrays to keep track of cluster */
static int   *ncthar0 = NULL ; /* thresholds at each point and in */
static int   *kcthar0 = NULL ; /* each OpenMP thread --           */
                               /* see mri_multi_threshold_setup() */

static float **cthar1 = NULL ;
static int   *ncthar1 = NULL ;
static int   *kcthar1 = NULL ;

static float **cthar2 = NULL ;
static int   *ncthar2 = NULL ;
static int   *kcthar2 = NULL ;

static int   cth_mode = 2 ;    /* 0 = mean , 1 = median, 2 = cth_perc% */
static float cth_perc = 90.0f ;

#define ADDTO_CTHAR0(val,ith)                                                  \
 do{ if( kcthar0[ith] >= ncthar0[ith] ){                                       \
      ncthar0[ith] = 2*kcthar0[ith] ;                                          \
       cthar0[ith] = (float *)realloc(cthar0[ith],sizeof(float)*ncthar0[ith]); \
     }                                                                         \
     cthar0[ith][kcthar0[ith]++] = (val) ;                                     \
 } while(0)

#define ADDTO_CTHAR1(val,ith)                                                  \
 do{ if( kcthar1[ith] >= ncthar1[ith] ){                                       \
      ncthar1[ith] = 2*kcthar1[ith] ;                                          \
       cthar1[ith] = (float *)realloc(cthar1[ith],sizeof(float)*ncthar1[ith]); \
     }                                                                         \
     cthar1[ith][kcthar1[ith]++] = (val) ;                                     \
 } while(0)

#define ADDTO_CTHAR2(val,ith)                                                  \
 do{ if( kcthar2[ith] >= ncthar2[ith] ){                                       \
      ncthar2[ith] = 2*kcthar2[ith] ;                                          \
       cthar2[ith] = (float *)realloc(cthar2[ith],sizeof(float)*ncthar2[ith]); \
     }                                                                         \
     cthar2[ith][kcthar2[ith]++] = (val) ;                                     \
 } while(0)

#define CPUT_point(i,j,k)                                                    \
 do{ int pqr = (i)+(j)*nx+(k)*nxy , npt=(xcc)->npt ;                         \
     if( far[pqr] != 0.0f ){                                                 \
       if( npt >= (xcc)->nall ){                                             \
         (xcc)->nall = npt + DALL + (xcc)->nall/2 ;                          \
         (xcc)->ip = (ind_t *)realloc((xcc)->ip,sizeof(ind_t)*(xcc)->nall) ; \
         (xcc)->jp = (ind_t *)realloc((xcc)->jp,sizeof(ind_t)*(xcc)->nall) ; \
         (xcc)->kp = (ind_t *)realloc((xcc)->kp,sizeof(ind_t)*(xcc)->nall) ; \
         (xcc)->ijk= (int *)  realloc((xcc)->ijk,sizeof(int) *(xcc)->nall) ; \
       }                                                                     \
       (xcc)->ip[npt] = (i); (xcc)->jp[npt] = (j); (xcc)->kp[npt] = (k);     \
       (xcc)->ijk[npt] = pqr ;                                               \
       if( (xcc)->ijkmin > pqr ) (xcc)->ijkmin = pqr ;                       \
       if( (xcc)->ijkmax < pqr ) (xcc)->ijkmax = pqr ;                       \
       (xcc)->npt++ ; (xcc)->norig++ ;                                       \
       (xcc)->fomh[0] += 1.0f ;                                              \
       (xcc)->fomh[1] += fabsf(far[pqr]) ;                                   \
       (xcc)->fomh[2] += far[pqr]*far[pqr] ;                                 \
       if( docim0 ) ADDTO_CTHAR0(car0[pqr],ithr) ;                           \
       if( docim1 ) ADDTO_CTHAR1(car1[pqr],ithr) ;                           \
       if( docim2 ) ADDTO_CTHAR2(car2[pqr],ithr) ;                           \
       far[pqr] = 0.0f ;                                                     \
     } } while(0)

/*----------------------------------------------------------------------------*/
/* Find clusters of nonzero voxels:
     fim    = thresholded float image (will be zero-ed out at end)
     nnlev  = 1 or 2 or 3 (clustering neighborliness)
     thtype = threshold type (0 = float *, 1 = MRI_IMAGE *)
     thptrX = pointer to threshold (a single float, or an image)
*//*--------------------------------------------------------------------------*/

Xcluster_array * find_Xcluster_array( MRI_IMAGE *fim, int nnlev,
                                      int thtype ,
                                      void *thptr0 , void *thptr1 , void *thptr2 )
{
   Xcluster *xcc=NULL ; Xcluster_array *xcar=NULL ;
   float *far,*car0=NULL,*car1=NULL,*car2=NULL ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km , nx,ny,nz,nxy,nxyz ;
   const int do_nn2=(nnlev > 1) , do_nn3=(nnlev > 2) ;
   int docim0=0, docim1=0, docim2=0, doccc=0 ; ;
   MRI_IMAGE *cim0=NULL, *cim1=NULL, *cim2=NULL ;
   float     *fpt0=NULL, *fpt1=NULL, *fpt2=NULL ;
   float      fth0=0.0f,  fth1=0.0f,  fth2=0.0f ;
   int      dofth0=0   ,dofth1=0   ,dofth2=0    , dofff=0 ;
   int      dott0 =0   ,dott1 =0   ,dott2 =0    , dottt=0 ;
   DECLARE_ithr ;

   far = MRI_FLOAT_PTR(fim) ;
   nx  = fim->nx; ny = fim->ny; nxy = nx*ny; nz = fim->nz; nxyz = nxy*nz;

   /* setup thresholds: spatially constant (thtype==0) or variable (==1) */

   switch( thtype ){
     case 0:
       fpt0 = (float *)thptr0; if( fpt0 != NULL ){ fth0 = *fpt0; dofth0 = (fth0 > 0.0f); }
       fpt1 = (float *)thptr1; if( fpt1 != NULL ){ fth1 = *fpt1; dofth1 = (fth1 > 0.0f); }
       fpt2 = (float *)thptr2; if( fpt2 != NULL ){ fth2 = *fpt2; dofth2 = (fth2 > 0.0f); }
/* INFO_message("find_Xcluster_array: fth0=%.1f fth1=%.1f fth2=%.1f",fth0,fth1,fth2) ; */
       dofff = (dofth0||dofth1||dofth2) ;
     break ;

     case 1:
       cim0 = (MRI_IMAGE *)thptr0; docim0 = (cim0!=NULL); if( docim0 ) car0 = MRI_FLOAT_PTR(cim0);
       cim1 = (MRI_IMAGE *)thptr1; docim1 = (cim1!=NULL); if( docim1 ) car1 = MRI_FLOAT_PTR(cim1);
       cim2 = (MRI_IMAGE *)thptr2; docim2 = (cim2!=NULL); if( docim2 ) car2 = MRI_FLOAT_PTR(cim2);
       doccc =(docim0||docim1||docim2) ;
     break ;

     default:
       doccc = dofff = 0 ;
       if( thptr0 != NULL || thptr1 != NULL || thptr2 != NULL )
         WARNING_message("bad thtype=%d in find_Xcluster_array :(",thtype) ;
     break ;
   }
   dott0 = (dofth0 || docim0) ;
   dott1 = (dofth1 || docim1) ;
   dott2 = (dofth2 || docim2) ; dottt = (doccc || dofff) ;

   ijk_last = 0 ;  /* start scanning at the {..wait for it..} start */

   while(1){
     /* find next nonzero point in far array (not far away) */

     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( far[ijk] != 0.0f ) break ;
     if( ijk == nxyz ) break ;  /* didn't find any! */
     ijk_last = ijk+1 ;         /* start here next time */

     IJK_TO_THREE(ijk, ii,jj,kk , nx,nxy) ;  /* 3D coords of this point */

     /* build a new cluster starting with this 1 point */

     if( xcc == NULL )
       CREATE_Xcluster(xcc,16) ;  /* initialize to have just 16 points */

     xcc->ip[0]   = (ind_t)ii; xcc->jp[0] = (ind_t)jj; xcc->kp[0] = (ind_t)kk;
     xcc->ijk[0]  = xcc->ijkmin = xcc->ijkmax = ijk;
     xcc->npt     = xcc->norig = 1 ;
     xcc->hmask   = 0 ;
     xcc->fomh[0] = 1.0f ;
     xcc->fomh[1] = fabsf(far[ijk]) ;
     xcc->fomh[2] = far[ijk]*far[ijk] ; far[ijk] = 0.0f ;
     xcc->cth[0]  = xcc->cth[1] = xcc->cth[2] = 0.0f ;
     if( docim0 ){ cthar0[ithr][0] = car0[ijk]; kcthar0[ithr] = 1; } else { kcthar0[ithr] = 0; }
     if( docim1 ){ cthar1[ithr][0] = car1[ijk]; kcthar1[ithr] = 1; } else { kcthar1[ithr] = 0; }
     if( docim2 ){ cthar2[ithr][0] = car2[ijk]; kcthar2[ithr] = 1; } else { kcthar2[ithr] = 0; }

     /* loop over points in cluster, checking their neighbors,
        growing the cluster if we find any that belong therein */

     for( icl=0 ; icl < xcc->npt ; icl++ ){
       ii = xcc->ip[icl]; jj = xcc->jp[icl]; kk = xcc->kp[icl];
       im = ii-1        ; jm = jj-1        ; km = kk-1 ;  /* minus 1 indexes */
       ip = ii+1        ; jp = jj+1        ; kp = kk+1 ;  /* plus  1 indexes */

       /* CPUT_point(i,j,k) only does something if far[i,j,k] is nonzero */

       if( im >= 0 ){                 CPUT_point(im,jj,kk) ;  /* 1NN */
         if( do_nn2 ){
           if( jm >= 0 )              CPUT_point(im,jm,kk) ;  /* 2NN */
           if( jp < ny )              CPUT_point(im,jp,kk) ;  /* 2NN */
           if( km >= 0 )              CPUT_point(im,jj,km) ;  /* 2NN */
           if( kp < nz )              CPUT_point(im,jj,kp) ;  /* 2NN */
           if( do_nn3 ){
             if( jm >= 0 && km >= 0 ) CPUT_point(im,jm,km) ;  /* 3NN */
             if( jm >= 0 && kp < nz ) CPUT_point(im,jm,kp) ;  /* 3NN */
             if( jp < ny && km >= 0 ) CPUT_point(im,jp,km) ;  /* 3NN */
             if( jp < ny && kp < nz ) CPUT_point(im,jp,kp) ;  /* 3NN */
       }}}
       if( ip < nx ){                 CPUT_point(ip,jj,kk) ;  /* 1NN */
         if( do_nn2 ){
           if( jm >= 0 )              CPUT_point(ip,jm,kk) ;  /* 2NN */
           if( jp < ny )              CPUT_point(ip,jp,kk) ;  /* 2NN */
           if( km >= 0 )              CPUT_point(ip,jj,km) ;  /* 2NN */
           if( kp < nz )              CPUT_point(ip,jj,kp) ;  /* 2NN */
           if( do_nn3 ){
             if( jm >= 0 && km >= 0 ) CPUT_point(ip,jm,km) ;  /* 3NN */
             if( jm >= 0 && kp < nz ) CPUT_point(ip,jm,kp) ;  /* 3NN */
             if( jp < ny && km >= 0 ) CPUT_point(ip,jp,km) ;  /* 3NN */
             if( jp < ny && kp < nz ) CPUT_point(ip,jp,kp) ;  /* 3NN */
       }}}
       if( jm >= 0 ){                 CPUT_point(ii,jm,kk) ;  /* 1NN */
         if( do_nn2 ){
           if( km >= 0 )              CPUT_point(ii,jm,km) ;  /* 2NN */
           if( kp < nz )              CPUT_point(ii,jm,kp) ;  /* 2NN */
       }}
       if( jp < ny ){                 CPUT_point(ii,jp,kk) ;  /* 1NN */
         if( do_nn2 ){
           if( km >= 0 )              CPUT_point(ii,jp,km) ;  /* 2NN */
           if( kp < nz )              CPUT_point(ii,jp,kp) ;  /* 2NN */
       }}
       if( km >= 0 )                  CPUT_point(ii,jj,km) ;  /* 1NN */
       if( kp < nz )                  CPUT_point(ii,jj,kp) ;  /* 1NN */

     } /* since xcc->npt increases when CPUT_point adds a point,
          the loop continues until finally no new neighbors get added */

     if( xcc->npt < min_clust ){  /* too small ==> recycle */
       xcc->npt = xcc->norig = 0; continue ;
     }

     /* compute FOM thresholds, if they will be used */

     if( docim0 ){  /* h=0 */
       float cth , qmean=qmean_float(kcthar0[ithr],cthar0[ithr]) , qmed ;
       switch( cth_mode ){
         case 0: cth  = qmean; break;
         case 1: qmed = qmed_float(kcthar0[ithr],cthar0[ithr]); cth = MAX(qmean,qmed); break;
         default:
         case 2: qmed = qfrac_float(kcthar0[ithr],0.01f*cth_perc,cthar0[ithr]); cth = MAX(qmean,qmed); break;
       }
       xcc->cth[0] = cth ;
     } else if( dofth0 ) {
       xcc->cth[0] = fth0 ;
     }

     if( docim1 ){  /* h=1 */
       float cth , qmean=qmean_float(kcthar1[ithr],cthar1[ithr]) , qmed ;
       switch( cth_mode ){
         case 0: cth  = qmean; break;
         case 1: qmed = qmed_float(kcthar1[ithr],cthar1[ithr]); cth = MAX(qmean,qmed); break;
         default:
         case 2: qmed = qfrac_float(kcthar1[ithr],0.01f*cth_perc,cthar1[ithr]); cth = MAX(qmean,qmed); break;
       }
       xcc->cth[1] = cth ;
     } else if( dofth1 ) {
       xcc->cth[1] = fth1 ;
     }

     if( docim2 ){  /* h=2 */
       float cth , qmean=qmean_float(kcthar2[ithr],cthar2[ithr]) , qmed ;
       switch( cth_mode ){
         case 0: cth  = qmean; break;
         case 1: qmed = qmed_float(kcthar2[ithr],cthar2[ithr]); cth = MAX(qmean,qmed); break;
         default:
         case 2: qmed = qfrac_float(kcthar2[ithr],0.01f*cth_perc,cthar2[ithr]); cth = MAX(qmean,qmed); break;
       }
       xcc->cth[2] = cth ;
     } else if( dofth2 ) {
       xcc->cth[2] = fth2 ;
     }

     /* check thresholds vs cluster FOMs to see if cluster is worthy? */

     if( dottt ){
       int ngood=0 ;
       if( dott0 && xcc->fomh[0] >= xcc->cth[0] ) ngood += 1 ;
       if( dott1 && xcc->fomh[1] >= xcc->cth[1] ) ngood += 2 ;
       if( dott2 && xcc->fomh[2] >= xcc->cth[2] ) ngood += 4 ;
       if( ngood == 0 ){            /* didn't pass any threshold ==> recycle */
         xcc->npt = xcc->norig = 0; continue;  /* cluster is set to be empty */
       } else {
         xcc->hmask = ngood ;
       }
     }

     /* if cluster makes it to here, throw it on the pile */

     if( xcar == NULL ) CREATE_Xcluster_array(xcar,8) ;  /* create the list */
     ADDTO_Xcluster_array(xcar,xcc) ;
     xcc = NULL ;  /* no recycling ==> will create new xcc when needed */

   } /* loop until all nonzero points in far[] have been used up */

   if( xcc != NULL ) DESTROY_Xcluster(xcc) ; /* exited with unsaved cluster? */

   return xcar ;  /* could be NULL (if nothing survived the tests) */
}

#undef CPUT_point  /* not to be used again */

/*----------------------------------------------------------------------------*/

void set_Xcluster_ijkminmax( Xcluster *xcc )
{
   int ib,it , ii,npt ;

   if( xcc == NULL ) return ;

   npt = xcc->npt ; if( npt < 1 ) return ;
   ib  = it = xcc->ijk[0] ;
   for( ii=1 ; ii < npt ; ii++ ){
          if( xcc->ijk[ii] < ib ) ib = xcc->ijk[ii] ;
     else if( xcc->ijk[ii] > it ) it = xcc->ijk[ii] ;
   }
   xcc->ijkmin = ib ;
   xcc->ijkmax = it ;
   return ;
}

/*----------------------------------------------------------------------------*/
/* Allocate the initial cthar arrays for cluster FOM thresholding */

void mri_multi_threshold_setup(void)
{
   int nthr=1 , ithr ; char *eee ;
#ifdef USE_OMP
   nthr = omp_get_max_threads() ;
#endif
    cthar0 = (float **)malloc(sizeof(float *)*nthr) ;
   ncthar0 = (int *   )malloc(sizeof(int)*nthr) ;
   kcthar0 = (int *   )malloc(sizeof(int)*nthr) ;
    cthar1 = (float **)malloc(sizeof(float *)*nthr) ;
   ncthar1 = (int *   )malloc(sizeof(int)*nthr) ;
   kcthar1 = (int *   )malloc(sizeof(int)*nthr) ;
    cthar2 = (float **)malloc(sizeof(float *)*nthr) ;
   ncthar2 = (int *   )malloc(sizeof(int)*nthr) ;
   kcthar2 = (int *   )malloc(sizeof(int)*nthr) ;
   for( ithr=0 ; ithr < nthr ; ithr++ ){
     cthar0[ithr] = (float *)malloc(sizeof(float)*4096) ;
    ncthar0[ithr] = 4096 ;
    kcthar0[ithr] = 0 ;
     cthar1[ithr] = (float *)malloc(sizeof(float)*4096) ;
    ncthar1[ithr] = 4096 ;
    kcthar1[ithr] = 0 ;
     cthar2[ithr] = (float *)malloc(sizeof(float)*4096) ;
    ncthar2[ithr] = 4096 ;
    kcthar2[ithr] = 0 ;
   }
   eee = getenv("AFNI_MTHRESH_MODE") ;
   if( eee != NULL ){
     char *ppp ;
     cth_mode = 2 ;
     ppp = strcasestr(eee,"mean") ;
     if( ppp != NULL ) cth_mode = 0 ;
     ppp = strcasestr(eee,"median") ;
     if( ppp != NULL ) cth_mode = 1 ;
     ppp = strstr(eee,"%") ;
     if( ppp != NULL && isdigit(*(ppp+1)) ){
       cth_perc = (float)strtod(ppp+1,NULL) ;
       if( cth_perc >= 0.0f && cth_perc <= 100.0f ) cth_mode = 2 ;
       else {
         WARNING_message(
          "AFNI_MTHRESH_MODE says %.1f%% -- out of range 0..100, so using median",
          cth_perc) ;
         cth_mode = 1 ;
       }
     }
   }
#if 1
   switch( cth_mode ){
     default:
       INFO_message("MultiThresh cluster FOM local threshold method = MEAN"); break;
     case 1:
       INFO_message("MultiThresh cluster FOM local threshold method = MEDIAN"); break;
     case 2:
       INFO_message("MultiThresh cluster FOM local threshold method = cdf %.1f%%",cth_perc); break;
   }
#endif
   return ;
}

/*----------------------------------------------------------------------------*/
/* get rid of the cthar arrays */

void mri_multi_threshold_unsetup(void)
{
   int nthr=1 , ithr ;
#ifdef USE_OMP
   nthr = omp_get_max_threads() ;
#endif
   if( cthar0 != NULL ){
     for( ithr=0 ; ithr < nthr ; ithr++ ) free(cthar0[ithr]) ;
     free(cthar0) ; free(ncthar0) ; free(kcthar0) ;
   }
   if( cthar1 != NULL ){
     for( ithr=0 ; ithr < nthr ; ithr++ ) free(cthar1[ithr]) ;
     free(cthar1) ; free(ncthar1) ; free(kcthar1) ;
   }
   if( cthar2 != NULL ){
     for( ithr=0 ; ithr < nthr ; ithr++ ) free(cthar2[ithr]) ;
     free(cthar2) ; free(ncthar2) ; free(kcthar2) ;
   }
    cthar0 =  cthar1 =  cthar2 = NULL ;
   ncthar0 = ncthar1 = ncthar2 = NULL ;
   kcthar0 = kcthar1 = kcthar2 = NULL ;
   return ;
}

/*----------------------------------------------------------------------------*/

#define XTHRESH_OUTPUT_MASK  1

#define CIM0(i) ( (cimar0==NULL) ? NULL : IMARR_SUBIM(cimar0,(i)) )
#define CIM1(i) ( (cimar1==NULL) ? NULL : IMARR_SUBIM(cimar1,(i)) )
#define CIM2(i) ( (cimar2==NULL) ? NULL : IMARR_SUBIM(cimar2,(i)) )

/*----------------------------------------------------------------------------*/
/* fim   = image to threshold
   nthr  = num thresholds   (at least 1)
   thar  = threshold array  (length nthr)
   sid   = sideness of threshold (1 or 2)
   nnlev = NN cluster type (1 or 2 or 3)
   cimar = array of cluster fom threshold images
           (if NULL, all clusters >= min_clust voxels are kept)
   flags = bitwise OR (|) of some or all of the following
             XTHRESH_OUTPUT_MASK

   return value will be NULL if nothing was found
   if nhits!=NULL, the number of 'hits' (nonzero voxels in output) goes there
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_multi_threshold_Xcluster_cimar( MRI_IMAGE *fim ,
                                                int nthr  , float *thar ,
                                                int sid   , int nnlev   ,
                                                MRI_IMARR *cimar0 ,
                                                MRI_IMARR *cimar1 ,
                                                MRI_IMARR *cimar2 ,
                                                int flags , int *nhits ,
                                                MRI_IMARR **allmask     )
{
   MRI_IMAGE *tfim , *qfim=NULL , *cim0,*cim1,*cim2 ;
   float *tfar , *far , *qfar=NULL , cth,cval,thr ;
   int ii,nvox , kth , jhp , hm ;
   Xcluster_array *xcar ; Xcluster *xcc ; int icl,npt, *ijkar ;
   int do_mask = (flags & XTHRESH_OUTPUT_MASK) ;
   byte *qbyt=NULL ;
   MRI_IMARR *amask=NULL; MRI_IMAGE *afim=NULL; byte *abyt=NULL;

   /* bad inputs? */

   if( nhits   != NULL ) *nhits   = 0 ;
   if( allmask != NULL ) *allmask = NULL ;

   if( fim  == NULL || fim->kind != MRI_float  ) return NULL ;
   if( nthr <= 0    || thar      == NULL       ) return NULL ;

   if( cimar0 != NULL && IMARR_COUNT(cimar0) != nthr ) return NULL ;
   if( cimar1 != NULL && IMARR_COUNT(cimar1) != nthr ) return NULL ;
   if( cimar2 != NULL && IMARR_COUNT(cimar2) != nthr ) return NULL ;

   if( allmask != NULL ) INIT_IMARR(amask) ;

   nvox = fim->nvox ;
   far  = MRI_FLOAT_PTR(fim) ;

   for( kth=0 ; kth < nthr ; kth++ ){  /* loop over thresholds */
     thr  = thar[kth] ;
     cim0 = CIM0(kth) ; cim1 = CIM1(kth) ; cim2 = CIM2(kth) ;
     tfim = mri_copy(fim) ;
     tfar = MRI_FLOAT_PTR(tfim) ;

     /* voxel-wise thresholding */

     if( thr != 0.0f ){
       if( sid == 2 ){           /*-- 2 sided --*/
         thr = fabsf(thr) ;
         for( ii=0 ; ii < nvox ; ii++ )
           if( tfar[ii] > -thr && tfar[ii] < thr ) tfar[ii] = 0.0f ;
       } else if( thr > 0.0f ){  /*-- 1 sided (positive) --*/
         for( ii=0 ; ii < nvox ; ii++ )
           if( tfar[ii] < thr ) tfar[ii] = 0.0f ;
       } else if( thr < 0.0f ){  /*-- 1 sided (negative) --*/
         for( ii=0 ; ii < nvox ; ii++ )
           if( tfar[ii] > thr ) tfar[ii] = 0.0f ;
       }
     }

     /* clusterize and keep "good" clusters (relative to cim) */

     xcar = find_Xcluster_array( tfim , nnlev , 1,cim0,cim1,cim2 ) ;

     mri_free(tfim) ;

     if( xcar == NULL ){ /* we got nuthin at this threshold */
       if( amask != NULL ) ADDTO_IMARR(amask,NULL) ;
       continue ;
     }

     /* put "good" clusters into qfim (copying from original input image) */

     if( qfim == NULL ){          /* create output image if needed */
       if( do_mask ){
         qfim = mri_new_conforming(fim,MRI_byte) ;  /* zero filled */
         qbyt = MRI_BYTE_PTR(qfim) ;
       } else {
         qfim = mri_new_conforming(fim,MRI_float) ; /* zero filled */
         qfar = MRI_FLOAT_PTR(qfim) ;
       }
     }
     if( amask != NULL ){
       afim = mri_new_conforming(fim,MRI_byte) ;    /* zero filled */
       abyt = MRI_BYTE_PTR(afim) ;
     }

     for( icl=0 ; icl < xcar->nclu ; icl++ ){
       xcc = xcar->xclu[icl]; npt = xcc->npt; ijkar = xcc->ijk; hm = xcc->hmask;
       if( do_mask ){
         for( ii=0 ; ii < npt ; ii++ ) qbyt[ijkar[ii]]++ ;
       } else {
         for( ii=0 ; ii < npt ; ii++ ) qfar[ijkar[ii]] = far[ijkar[ii]] ;
       }
       if( amask != NULL ){
         for( ii=0 ; ii < npt ; ii++ ) abyt[ijkar[ii]] = hm ;
       }
     }

     DESTROY_Xcluster_array(xcar) ;

     if( amask != NULL ) ADDTO_IMARR(amask,afim) ;
   }

   /* count number of hits */

   if( nhits != NULL && qfim != NULL ){
     if( do_mask ){
       for( npt=ii=0 ; ii < nvox ; ii++ ) npt += (qbyt[ii] != 0) ;
     } else if( qfar != NULL ){
       for( npt=ii=0 ; ii < nvox ; ii++ ) npt += (qfar[ii] != 0.0f) ;
     }
     *nhits = npt ;
   }

   if( allmask != NULL ) *allmask = amask ;

   return qfim ;  /* nuthin nowhere nohow ==> NULL */
}

/*----------------------------------------------------------------------------*/
/* Version of the above with a single per-voxel
   threshold value and a single cluster-FOM threshold image
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_threshold_Xcluster( MRI_IMAGE *fim ,
                                    float thr , int sid , int nnlev ,
                                    MRI_IMAGE *cim0 ,
                                    MRI_IMAGE *cim1 ,
                                    MRI_IMAGE *cim2 ,
                                    int flags , int *nhits )
{
   float thar[1] ;
   MRI_IMARR *cimar0=NULL,*cimar1=NULL,*cimar2=NULL ;
   MRI_IMAGE *qfim ;

   if( cim0 != NULL ){ INIT_IMARR(cimar0); ADDTO_IMARR(cimar0,cim0); }
   if( cim1 != NULL ){ INIT_IMARR(cimar1); ADDTO_IMARR(cimar1,cim1); }
   if( cim2 != NULL ){ INIT_IMARR(cimar2); ADDTO_IMARR(cimar2,cim2); }

   thar[0] = thr ;

   qfim = mri_multi_threshold_Xcluster_cimar(fim,1,thar,sid,nnlev,
                                             cimar0,cimar1,cimar2,flags,nhits,NULL) ;

   if( cimar0 != NULL ) FREE_IMARR(cimar0) ;
   if( cimar1 != NULL ) FREE_IMARR(cimar1) ;
   if( cimar2 != NULL ) FREE_IMARR(cimar2) ;

   return qfim ;
}

/*----------------------------------------------------------------------------*/

#define FOM0(i) ( (fomth0==NULL) ? 0.0f : fomth0[(i)] )
#define FOM1(i) ( (fomth1==NULL) ? 0.0f : fomth1[(i)] )
#define FOM2(i) ( (fomth2==NULL) ? 0.0f : fomth2[(i)] )

/*----------------------------------------------------------------------------*/
/* fim   = image to threshold
   nthr  = num thresholds   (at least 1)
   thar  = threshold array  (length nthr)
   sid   = sideness of threshold (1 or 2)
   nnlev = NN cluster type (1 or 2 or 3)
   fomth = array of fom threshold values
           (if all NULL, all clusters >= min_clust voxels are kept)
   flags = bitwise OR (|) of some or all of the following
             XTHRESH_OUTPUT_MASK

   return value will be NULL if nothing was found
   if nhits!=NULL, the number of 'hits' (nonzero voxels in output) goes there
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_multi_threshold_Xcluster_fomth( MRI_IMAGE *fim ,
                                                int nthr  , float *thar ,
                                                int sid   , int nnlev   ,
                                                float *fomth0,
                                                float *fomth1,
                                                float *fomth2,
                                                int flags , int *nhits ,
                                                MRI_IMARR **allmask     )
{
   MRI_IMAGE *tfim , *qfim=NULL ;
   float *tfar , *far , *qfar=NULL , cth,cval,thr ;
   int ii,nvox , kth , jhp , hm ;
   Xcluster_array *xcar ; Xcluster *xcc ; int icl,npt, *ijkar ;
   int do_mask = (flags & XTHRESH_OUTPUT_MASK) ;
   byte *qbyt=NULL ;
   MRI_IMARR *amask=NULL; MRI_IMAGE *afim=NULL; byte *abyt=NULL;
   float fth0,fth1,fth2 ;

   /* bad inputs? */

   if( nhits   != NULL ) *nhits   = 0 ;
   if( allmask != NULL ) *allmask = NULL ;

   if( fim  == NULL || fim->kind != MRI_float  ) return NULL ;
   if( nthr <= 0    || thar      == NULL       ) return NULL ;

   if( allmask != NULL ) INIT_IMARR(amask) ;

   nvox = fim->nvox ;
   far  = MRI_FLOAT_PTR(fim) ;

   for( kth=0 ; kth < nthr ; kth++ ){  /* loop over thresholds */
     thr  = thar[kth] ;
     fth0 = FOM0(kth) ; fth1 = FOM1(kth) ; fth2 = FOM2(kth) ;
     tfim = mri_copy(fim) ;
     tfar = MRI_FLOAT_PTR(tfim) ;

     /* voxel-wise thresholding */

     if( thr != 0.0f ){
       if( sid == 2 ){           /*-- 2 sided --*/
         thr = fabsf(thr) ;
         for( ii=0 ; ii < nvox ; ii++ )
           if( tfar[ii] > -thr && tfar[ii] < thr ) tfar[ii] = 0.0f ;
       } else if( thr > 0.0f ){  /*-- 1 sided (positive) --*/
         for( ii=0 ; ii < nvox ; ii++ )
           if( tfar[ii] < thr ) tfar[ii] = 0.0f ;
       } else if( thr < 0.0f ){  /*-- 1 sided (negative) --*/
         for( ii=0 ; ii < nvox ; ii++ )
           if( tfar[ii] > thr ) tfar[ii] = 0.0f ;
       }
     }

     /* clusterize and keep "good" clusters (relative to fth) */

     xcar = find_Xcluster_array( tfim , nnlev , 0,&fth0,&fth1,&fth2 ) ;

     mri_free(tfim) ;

     if( xcar == NULL ){ /* we got nuthin at this threshold */
       if( amask != NULL ) ADDTO_IMARR(amask,NULL) ;
       continue ;
     }

     /* put "good" clusters into qfim (copying from original input image) */

     if( qfim == NULL ){          /* create output image if needed */
       if( do_mask ){
         qfim = mri_new_conforming(fim,MRI_byte) ;  /* zero filled */
         qbyt = MRI_BYTE_PTR(qfim) ;
       } else {
         qfim = mri_new_conforming(fim,MRI_float) ; /* zero filled */
         qfar = MRI_FLOAT_PTR(qfim) ;
       }
     }
     if( amask != NULL ){
       afim = mri_new_conforming(fim,MRI_byte) ;    /* zero filled */
       abyt = MRI_BYTE_PTR(afim) ;
     }

     for( icl=0 ; icl < xcar->nclu ; icl++ ){
       xcc = xcar->xclu[icl]; npt = xcc->npt; ijkar = xcc->ijk; hm = xcc->hmask;
       if( do_mask ){
         for( ii=0 ; ii < npt ; ii++ ) qbyt[ijkar[ii]]++ ;
       } else {
         for( ii=0 ; ii < npt ; ii++ ) qfar[ijkar[ii]] = far[ijkar[ii]] ;
       }
       if( amask != NULL ){
         for( ii=0 ; ii < npt ; ii++ ) abyt[ijkar[ii]] = hm ;
       }
     }

     DESTROY_Xcluster_array(xcar) ;

     if( amask != NULL ) ADDTO_IMARR(amask,afim) ;
   }

   /* count number of hits */

   if( nhits != NULL && qfim != NULL ){
     if( do_mask ){
       for( npt=ii=0 ; ii < nvox ; ii++ ) npt += (qbyt[ii] != 0) ;
     } else if( qfar != NULL ){
       for( npt=ii=0 ; ii < nvox ; ii++ ) npt += (qfar[ii] != 0.0f) ;
     }
     *nhits = npt ;
   }

   if( allmask != NULL ) *allmask = amask ;

   return qfim ;  /* nuthin nowhere nohow ==> NULL */
}
