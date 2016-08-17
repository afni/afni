#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/* Cluster definition.  Index type ind_t can be byte or short. */

#define USE_UBYTE

/*====================================*/
#ifdef USE_UBYTE  /* for grids <= 255 */

# define DALL    256
# define MAX_IND 255u

  typedef unsigned char ind_t ;

#else   /*==== for grids <= 32767 ====*/

# define DALL    1024
# define MAX_IND 32767u

  typedef unsigned short ind_t ;

#endif
/*====================================*/

/*----- struct to hold a single cluster of points -----*/

typedef struct {
  int npt ,                 /* number of points assigned */
      nall ,                /* number of points allocated */
      ngood ;               /* number of good points left */
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
 do{ if( (xcar)->npt = (xcar)->nall ){                                         \
       (xcar)->nall += 16 ;                                                    \
       (xcar)->xclu = (Xcluster **)realloc((xcar)->xclu,                       \
                                           sizeof(Xcluster **)*(xcar)->nall) ; \
     }                                                                         \
     (xcar)->xclu[(xcar)->nclu++] = (xc) ;                                     \
 } while(0)

#define DESTROY_Xcluster_array(xcar)                                       \
 do{ int cc ;                                                              \
     for( cc=0 ; cc < (xcar)->nclu ; cc++ )                                \
       if( (xcar)->xclu[cc] != NULL ) DESTROY_Xcluster((xcar)->xclu[cc]) ; \
     free((xcar)->xclu) ; free(xcar) ;                                     \
 } while(0)

/*----------------------------------------------------------------------------*/
/* Create a cluster with initial array allocation of siz */

#define CREATE_Xcluster(xc,siz)                                        \
 do{ xc = (Xcluster *)malloc(sizeof(Xcluster)) ;                       \
     xc->npt = 0 ; xc->ngood = 0 ; xc->nall = (siz) ;                  \
     xc->fom = 0.0f ;                                                  \
     xc->ip  = (ind_t *)malloc(sizeof(ind_t)*(siz)) ;                  \
     xc->jp  = (ind_t *)malloc(sizeof(ind_t)*(siz)) ;                  \
     xc->kp  = (ind_t *)malloc(sizeof(ind_t)*(siz)) ;                  \
     xc->ijk = (int *)  malloc(sizeof(int)  *(siz)) ;                  \
 } while(0)

#define DESTROY_Xcluster(xc)                                           \
 do{ free((xc)->ip); free((xc)->jp); free((xc)->kp);                   \
     free((xc)->ijk); free(xc);                                        \
 } while(0)

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
   xcout->ngood = xcin->ngood ;
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
/* Add a point to a cluster (if fim is nonzero at this point) */

#define XPUT_point(xcc,i,j,k)                                                \
 do{ int pqr = (i)+(j)*nx+(k)*nxy , npt=(xcc)->npt ;                         \
     if( fim[pqr] != 0.0f ){                                                 \
       if( npt == (xcc)->nall ){                                             \
         (xcc)->nall += DALL + (xcc)->nall/2 ;                               \
         (xcc)->ip = (ind_t *)realloc((xcc)->ip,sizeof(ind_t)*(xcc)->nall) ; \
         (xcc)->jp = (ind_t *)realloc((xcc)->jp,sizeof(ind_t)*(xcc)->nall) ; \
         (xcc)->kp = (ind_t *)realloc((xcc)->kp,sizeof(ind_t)*(xcc)->nall) ; \
         (xcc)->ijk= (int *)  realloc((xcc)->ijk,sizeof(int) *(xcc)->nall) ; \
       }                                                                     \
       (xcc)->ip[npt] = (i); (xcc)->jp[npt] = (j); (xcc)->kp[npt] = (k);     \
       (xcc)->ijk[npt] = pqr ;                                               \
       (xcc)->npt++ ; (xcc)->ngood++ ;                                       \
       (xcc)->fom += ADDTO_FOM(fim[pqr]) ;                                   \
       fim[pqr] = 0.0f ;                                                     \
     } } while(0)

/*----------------------------------------------------------------------------*/
/* Find clusters */

Xcluster_array * find_Xcluster_array( float *fim , int nnlev , int minsiz )
{
   Xcluster *xcc ; Xcluster_array *xcar=NULL ;
   int ii,jj,kk, icl , ijk , ijk_last ;
   int ip,jp,kp , im,jm,km ;
   float fom_max=0.0f ;
   const int do_nn2=(nnlev > 1) , do_nn3=(nnlev > 2) ;

   ijk_last = 0 ;  /* start scanning at the {..wait for it..} start */

   while(1) {
     /* find next nonzero point in fim array */

     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( fim[ijk] != 0.0f ) break ;
     if( ijk == nxyz ) break ;  /* didn't find any! */
     ijk_last = ijk+1 ;         /* start here next time */

     IJK_TO_THREE(ijk, ii,jj,kk , nx,nxy) ;  /* 3D coords of this point */

     /* build a new cluster starting with this 1 point */

     CREATE_Xcluster(xcc,16) ;

     xcc->ip[0] = ii; xcc->jp[0] = jj; xcc->kp[0] = kk; xcc->ijk[0] = ijk;
     xcc->npt   = xcc->ngood = 1 ;
     xcc->fom   = ADDTO_FOM(fim[ijk]) ; fim[ijk] = 0.0f ;

     /* loop over points in cluster, checking their neighbors,
        growing the cluster if we find any that belong therein */

     for( icl=0 ; icl < xcc->npt ; icl++ ){
       ii = xcc->ip[icl]; jj = xcc->jp[icl]; kk = xcc->kp[icl];
       im = ii-1        ; jm = jj-1        ; km = kk-1 ;  /* minus 1 indexes */
       ip = ii+1        ; jp = jj+1        ; kp = kk+1 ;  /* plus 1 indexes */

       if( im >= 0 ){  XPUT_point(im,jj,kk) ;
         if( do_nn2 ){
           if( jm >= 0 ) XPUT_point(im,jm,kk) ;  /* 2NN */
           if( jp < ny ) XPUT_point(im,jp,kk) ;  /* 2NN */
           if( km >= 0 ) XPUT_point(im,jj,km) ;  /* 2NN */
           if( kp < nz ) XPUT_point(im,jj,kp) ;  /* 2NN */
           if( do_nn3 ){
             if( jm >= 0 && km >= 0 ) XPUT_point(im,jm,km) ;  /* 3NN */
             if( jm >= 0 && kp < nz ) XPUT_point(im,jm,kp) ;  /* 3NN */
             if( jp < ny && km >= 0 ) XPUT_point(im,jp,km) ;  /* 3NN */
             if( jp < ny && kp < nz ) XPUT_point(im,jp,kp) ;  /* 3NN */
       }}}
       if( ip < nx ){  XPUT_point(ip,jj,kk) ;
         if( do_nn2 ){
           if( jm >= 0 ) XPUT_point(ip,jm,kk) ;  /* 2NN */
           if( jp < ny ) XPUT_point(ip,jp,kk) ;  /* 2NN */
           if( km >= 0 ) XPUT_point(ip,jj,km) ;  /* 2NN */
           if( kp < nz ) XPUT_point(ip,jj,kp) ;  /* 2NN */
           if( do_nn3 ){
             if( jm >= 0 && km >= 0 ) XPUT_point(ip,jm,km) ;  /* 3NN */
             if( jm >= 0 && kp < nz ) XPUT_point(ip,jm,kp) ;  /* 3NN */
             if( jp < ny && km >= 0 ) XPUT_point(ip,jp,km) ;  /* 3NN */
             if( jp < ny && kp < nz ) XPUT_point(ip,jp,kp) ;  /* 3NN */
       }}}
       if( jm >= 0 ){  XPUT_point(ii,jm,kk) ;
         if( do_nn2 ){
           if( km >= 0 ) XPUT_point(ii,jm,km) ;  /* 2NN */
           if( kp < nz ) XPUT_point(ii,jm,kp) ;  /* 2NN */
       }}
       if( jp < ny ){  XPUT_point(ii,jp,kk) ;
         if( do_nn2 ){
           if( km >= 0 ) XPUT_point(ii,jp,km) ;  /* 2NN */
           if( kp < nz ) XPUT_point(ii,jp,kp) ;  /* 2NN */
       }}
       if( km >= 0 )   XPUT_point(ii,jj,km) ;
       if( kp < nz )   XPUT_point(ii,jj,kp) ;
     } /* since xcc->npt increases if XPUT_point adds the point,
          the loop continues until finally no new neighbors get added */

     if( xcc->npt < minsiz ){
       DESTROY_Xcluster(xcc) ; xcc = NULL ;
     } else {
       if( xcar == NULL ) CREATE_Xcluster_array(xcar,4) ;
       ADDTO_Xcluster_array(xcar,xcc) ;
     }

   } /* loop until all nonzero points in fim[] have been used up */

   return xcar ;  /* could be NULL, if fim is all zeros */
}

/*----------------------------------------------------------------------------*/

void mri_threshold_Xcluster( MRI_IMAGE *fim,
                             float thr, int sid, int nnlev, MRI_IMAGE *cim )
{
}
