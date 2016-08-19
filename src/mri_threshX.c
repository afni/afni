#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/* Cluster definition.  Index type ind_t can be byte or short. */

#define USE_UBYTE

/*====================================*/
#ifdef USE_UBYTE  /* for grids <= 255 */

# define DALL    128
# define MAX_IND 255u

  typedef unsigned char ind_t ;

#else   /*==== for grids <= 32767 ====*/

# define DALL    512
# define MAX_IND 32767u

  typedef unsigned short ind_t ;

#endif
/*====================================*/

/*----- struct to hold a single cluster of points -----*/

typedef struct {
  int npt ,                 /* number of points assigned */
      nall ,                /* number of points allocated */
      norig ,               /* number of original pts (before dilation) */
      nbcount ;             /* for dilation decisions (cf. 3dClustSimX) */
  float fom ;               /* Figure Of Merit for cluster */
  ind_t *ip , *jp , *kp ;   /* 3D indexes for each point */
  int   *ijk ;              /* 1D index for each point */
} Xcluster ;

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
 do{ if( (xcar)->nclu == (xcar)->nall ){                                       \
       (xcar)->nall += 16 ;                                                    \
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

/*----------------------------------------------------------------------------*/
/* Create a cluster with initial array allocation of siz */

#define CREATE_Xcluster(xc,siz)                                        \
 do{ xc = (Xcluster *)malloc(sizeof(Xcluster)) ;                       \
     xc->npt = xc->norig = xc->nbcount = 0 ; xc->nall = (siz) ;        \
     xc->fom = 0.0f ;                                                  \
     xc->ip  = (ind_t *)malloc(sizeof(ind_t)*(siz)) ;                  \
     xc->jp  = (ind_t *)malloc(sizeof(ind_t)*(siz)) ;                  \
     xc->kp  = (ind_t *)malloc(sizeof(ind_t)*(siz)) ;                  \
     xc->ijk = (int *)  malloc(sizeof(int)  *(siz)) ;                  \
 } while(0)

#define DESTROY_Xcluster(xc)                                           \
 do{ if( (xc) != NULL ){                                               \
       free((xc)->ip); free((xc)->jp); free((xc)->kp);                 \
       free((xc)->ijk); free(xc);                                      \
 }} while(0)

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
   xcout->fom   = xcin->fom ;
   xcout->npt   = nin ;
   xcout->norig = xcin->norig ;
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

/*----------------------------------------------------------------------------*/

#if 0  /* for later developments */
#  define ADDTO_FOM(val)                                               \
    ( (qpow==0) ? 1.0f :(qpow==1) ? fabsf(val) : (val)*(val) )
#else
#  define ADDTO_FOM(val) 1.0f
#endif

/*----------------------------------------------------------------------------*/
/* Add a point to current cluster (if far is nonzero at this point).
   For use only in the function directly below!
*//*--------------------------------------------------------------------------*/

#define TPUT_point(i,j,k)                                                    \
 do{ int pqr = (i)+(j)*nx+(k)*nxy , npt=(xcc)->npt ;                         \
     if( far[pqr] != 0.0f ){                                                 \
       if( npt == (xcc)->nall ){                                             \
         (xcc)->nall += DALL + (xcc)->nall/2 ;                               \
         (xcc)->ip = (ind_t *)realloc((xcc)->ip,sizeof(ind_t)*(xcc)->nall) ; \
         (xcc)->jp = (ind_t *)realloc((xcc)->jp,sizeof(ind_t)*(xcc)->nall) ; \
         (xcc)->kp = (ind_t *)realloc((xcc)->kp,sizeof(ind_t)*(xcc)->nall) ; \
         (xcc)->ijk= (int *)  realloc((xcc)->ijk,sizeof(int) *(xcc)->nall) ; \
       }                                                                     \
       (xcc)->ip[npt] = (i); (xcc)->jp[npt] = (j); (xcc)->kp[npt] = (k);     \
       (xcc)->ijk[npt] = pqr ;                                               \
       (xcc)->npt++ ; (xcc)->norig++ ;                                       \
       (xcc)->fom += ADDTO_FOM(far[pqr]) ;                                   \
       far[pqr] = 0.0f ;                                                     \
     } } while(0)

/*----------------------------------------------------------------------------*/
/* Find clusters of nonzero voxels:
     fim    = float image (will be zero-ed out at end)
     nnlev  = 1 or 2 or 3 (clustering neighborliness)
     minfom = minimum FOM cluster to keep
*//*--------------------------------------------------------------------------*/

static int threshX_keep_only_fomest = 0 ; /* for determining FAR: 3dClustSimX */

Xcluster_array * find_Xcluster_array( MRI_IMAGE *fim, int nnlev, float minfom )
{
   Xcluster *xcc ; Xcluster_array *xcar=NULL ; float *far ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km , nx,ny,nz,nxy,nxyz ;
   const int do_nn2=(nnlev > 1) , do_nn3=(nnlev > 2) ;

   if( fim == NULL || fim->kind != MRI_float ) return NULL ;
   far = MRI_FLOAT_PTR(fim) ;
   nx = fim->nx; ny = fim->ny; nxy = nx*ny; nz = fim->nz; nxyz = nxy*nz;

   ijk_last = 0 ;  /* start scanning at the {..wait for it..} start */

   while(1) {
     /* find next nonzero point in far array */

     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( far[ijk] != 0.0f ) break ;
     if( ijk == nxyz ) break ;  /* didn't find any! */
     ijk_last = ijk+1 ;         /* start here next time */

     IJK_TO_THREE(ijk, ii,jj,kk , nx,nxy) ;  /* 3D coords of this point */

     /* build a new cluster starting with this 1 point */

     CREATE_Xcluster(xcc,16) ;

     xcc->ip[0] = (ind_t)ii; xcc->jp[0] = (ind_t)jj; xcc->kp[0] = (ind_t)kk;
     xcc->ijk[0]= ijk;
     xcc->npt   = xcc->norig = 1 ;
     xcc->fom   = ADDTO_FOM(far[ijk]) ; far[ijk] = 0.0f ;

     /* loop over points in cluster, checking their neighbors,
        growing the cluster if we find any that belong therein */

     for( icl=0 ; icl < xcc->npt ; icl++ ){
       ii = xcc->ip[icl]; jj = xcc->jp[icl]; kk = xcc->kp[icl];
       im = ii-1        ; jm = jj-1        ; km = kk-1 ;  /* minus 1 indexes */
       ip = ii+1        ; jp = jj+1        ; kp = kk+1 ;  /* plus 1 indexes */

       if( im >= 0 ){                 TPUT_point(im,jj,kk) ;  /* 1NN */
         if( do_nn2 ){
           if( jm >= 0 )              TPUT_point(im,jm,kk) ;  /* 2NN */
           if( jp < ny )              TPUT_point(im,jp,kk) ;  /* 2NN */
           if( km >= 0 )              TPUT_point(im,jj,km) ;  /* 2NN */
           if( kp < nz )              TPUT_point(im,jj,kp) ;  /* 2NN */
           if( do_nn3 ){
             if( jm >= 0 && km >= 0 ) TPUT_point(im,jm,km) ;  /* 3NN */
             if( jm >= 0 && kp < nz ) TPUT_point(im,jm,kp) ;  /* 3NN */
             if( jp < ny && km >= 0 ) TPUT_point(im,jp,km) ;  /* 3NN */
             if( jp < ny && kp < nz ) TPUT_point(im,jp,kp) ;  /* 3NN */
       }}}
       if( ip < nx ){                 TPUT_point(ip,jj,kk) ;  /* 1NN */
         if( do_nn2 ){
           if( jm >= 0 )              TPUT_point(ip,jm,kk) ;  /* 2NN */
           if( jp < ny )              TPUT_point(ip,jp,kk) ;  /* 2NN */
           if( km >= 0 )              TPUT_point(ip,jj,km) ;  /* 2NN */
           if( kp < nz )              TPUT_point(ip,jj,kp) ;  /* 2NN */
           if( do_nn3 ){
             if( jm >= 0 && km >= 0 ) TPUT_point(ip,jm,km) ;  /* 3NN */
             if( jm >= 0 && kp < nz ) TPUT_point(ip,jm,kp) ;  /* 3NN */
             if( jp < ny && km >= 0 ) TPUT_point(ip,jp,km) ;  /* 3NN */
             if( jp < ny && kp < nz ) TPUT_point(ip,jp,kp) ;  /* 3NN */
       }}}
       if( jm >= 0 ){                 TPUT_point(ii,jm,kk) ;  /* 1NN */
         if( do_nn2 ){
           if( km >= 0 )              TPUT_point(ii,jm,km) ;  /* 2NN */
           if( kp < nz )              TPUT_point(ii,jm,kp) ;  /* 2NN */
       }}
       if( jp < ny ){                 TPUT_point(ii,jp,kk) ;  /* 1NN */
         if( do_nn2 ){
           if( km >= 0 )              TPUT_point(ii,jp,km) ;  /* 2NN */
           if( kp < nz )              TPUT_point(ii,jp,kp) ;  /* 2NN */
       }}
       if( km >= 0 )                  TPUT_point(ii,jj,km) ;  /* 1NN */
       if( kp < nz )                  TPUT_point(ii,jj,kp) ;  /* 1NN */

     } /* since xcc->npt increases if TPUT_point adds the point,
          the loop continues until finally no new neighbors get added */

     if( xcc->fom < minfom ){  /* too 'small' ==> toss onto the trash */
       DESTROY_Xcluster(xcc) ;
     } else {                  /* add to the cluster list */
       if( xcar == NULL ) CREATE_Xcluster_array(xcar,4) ;
       if( threshX_keep_only_fomest && xcar->nclu == 1 ){
         if( xcc->fom > xcar->xclu[1]->fom ){
           DESTROY_Xcluster(xcar->xclu[1]) ; xcar->xclu[1] = xcc ;
         } else {
           DESTROY_Xcluster(xcc) ;
         }
       } else {
         ADDTO_Xcluster_array(xcar,xcc) ;
       }
     }
     xcc = NULL ;

   } /* loop until all nonzero points in far[] have been used up */

   return xcar ;  /* could be NULL, if fim is all zeros */
}

#undef TPUT_point

/*----------------------------------------------------------------------------*/
/* fim   = image to threshold
   thr   = threshold
   sid   = sideness of threshold (1 or 2)
   nnlev = 1 or 2 or 3
   cim   = cluster fom threshold image
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_threshold_Xcluster( MRI_IMAGE *fim,
                                    float thr, int sid, int nnlev,
                                    MRI_IMAGE *cim )
{
   MRI_IMAGE *tfim ;
   float *car , *tfar , *far , cmin,cth,cval ;
   int ii,nvox,ncdon ;
   Xcluster_array *xcar ; Xcluster *xcc ; int icl,npt, *ijkar ;

   if( fim == NULL || fim->kind != MRI_float ) return NULL ;
   if( cim == NULL || cim->kind != MRI_float ) return NULL ;

   nvox = fim->nvox ;  if( cim->nvox != nvox ) return NULL ;
   car  = MRI_FLOAT_PTR(cim) ;
   far  = MRI_FLOAT_PTR(fim) ;

   cmin = WAY_BIG ;
   for( ii=0 ; ii < nvox ; ii++ )
     if( car[ii] > 0.0f && car[ii] < cmin ) cmin = car[ii] ;
   if( cmin == WAY_BIG ) return NULL ;

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

   /* clusterize */

   xcar = find_Xcluster_array( tfim , nnlev , cmin ) ;

   if( xcar == NULL ){ mri_free(tfim); return NULL; }  /* nada */

   /* put "good" clusters back into tfim */

   ncdon = 0 ;
   for( icl=0 ; icl < xcar->nclu ; icl++ ){
     xcc = xcar->xclu[icl]; npt = xcc->npt; ijkar = xcc->ijk; cth = 0.0f;
     /* average the threshold across the cluster */
     for( ii=0 ; ii < npt ; ii++ ){ cval = car[ijkar[ii]]; cth += MAX(cval,cmin); }
     cth /= npt ;
     if( xcc->fom >= cth ){  /* it's good, Jim */
       ncdon += npt ;
       for( ii=0 ; ii < npt ; ii++ )  /* copy from fim to tfim */
         tfar[ijkar[ii]] = far[ijkar[ii]] ;
     }
   }

   DESTROY_Xcluster_array(xcar) ;
   if( ncdon == 0 ){ mri_free(tfim) ; tfim = NULL ; }

   return tfim ;
}
