#include "mrilib.h"

#define USE_MCLUST

#ifdef USE_MCLUST
static void THD_mask_clust( int nx, int ny, int nz, byte *mmm ) ;
static void THD_mask_erode( int nx, int ny, int nz, byte *mmm ) ;
#endif

/*---------------------------------------------------------------------*/
/*! Make a byte mask for a 3D+time dataset -- 13 Aug 2001 - RWCox.
    (compare to thd_makemask.c)
-----------------------------------------------------------------------*/

byte * THD_automask( THD_3dim_dataset * dset )
{
   MRI_IMAGE *medim ;
   float clip_val , *mar ;
   byte *mmm = NULL ;
   int nvox , ii , nmm , nx,ny,nz ;

   MCW_cluster_array *clar ;
   int iclu , kclu ;

ENTRY("THD_automask") ;

   /* median at each voxel */

   medim = THD_median_brick(dset) ; if( medim == NULL ) RETURN(NULL);

   /* clip value to excise small stuff */

   clip_val = THD_cliplevel(medim,0.5) ;

   /* create mask of values above clip value */

   nvox = medim->nvox ;
   mar  = MRI_FLOAT_PTR(medim) ;
   mmm  = (byte *) calloc( sizeof(byte)*nvox , 1 ) ;
   for( nmm=ii=0 ; ii < nvox ; ii++ )
     if( mar[ii] >= clip_val ){ mmm[ii] = 1; nmm++; }

   mri_free(medim) ;
   if( nmm == 0 ) RETURN(mmm) ;  /* should not happen */

   /*-- 10 Apr 2002: only keep the largest connected component --*/

   nx = DSET_NX(dset) ; ny = DSET_NY(dset) ; nz = DSET_NZ(dset) ;

#ifdef USE_MCLUST
   THD_mask_clust( nx,ny,nz, mmm ) ;
#else
   clar = MCW_find_clusters( nx,ny,nz , 1.0,1.0,1.0 ,
                             MRI_byte , mmm , 1.01   ) ;

   /* at this point, all nonzero data in mmm has been transferred to clar */

   if( clar == NULL ) RETURN(mmm) ; /* should not happen */

   /* find largest cluster */

   for( nmm=iclu=kclu=0 ; iclu < clar->num_clu ; iclu++ ){
     if( clar->clar[iclu]->num_pt > nmm ){
       nmm = clar->clar[iclu]->num_pt; kclu = iclu;
     }
   }

   /* put that cluster back into the volume */

   MCW_cluster_to_vol( nx,ny,nz , MRI_byte,mmm , clar->clar[kclu] ) ;

   DESTROY_CLARR(clar) ;
#endif /* USE_MCLUST */

   /* 18 Apr 2002: now erode the resulting volume
                   (to break off any thinly attached pieces) */

   MCW_erode_clusters( nx,ny,nz , 1.0,1.0,1.0 ,
                       MRI_byte,mmm , 1.42 , 0.90 , 1 ) ;

   /* now recluster it, and again keep only the largest survivor */

#ifdef USE_MCLUST
   THD_mask_clust( nx,ny,nz, mmm ) ;
#else
   clar = MCW_find_clusters( nx,ny,nz , 1.0,1.0,1.0 ,
                             MRI_byte , mmm , 1.01   ) ;

   if( clar == NULL ) RETURN(mmm) ; /* should not happen */

   /* find largest cluster (again) */

   for( nmm=iclu=kclu=0 ; iclu < clar->num_clu ; iclu++ ){
     if( clar->clar[iclu]->num_pt > nmm ){
       nmm = clar->clar[iclu]->num_pt; kclu = iclu;
     }
   }

   /* put back into volume (again) */

   MCW_cluster_to_vol( nx,ny,nz , MRI_byte,mmm , clar->clar[kclu] ) ;

   DESTROY_CLARR(clar) ;
#endif /* USE_MCLUST */

#if 1
   /* 19 Apr 2002: fill in small holes */

   nmm = 1 ;
   ii  = rint(0.016*nx) ; nmm = MAX(nmm,ii) ;
   ii  = rint(0.016*ny) ; nmm = MAX(nmm,ii) ;
   ii  = rint(0.016*nz) ; nmm = MAX(nmm,ii) ;
   (void) THD_mask_fillin_completely( nx,ny,nz, mmm , nmm ) ;
#endif

   /* 28 May 2002:
      invert the mask, then find the largest cluster of 1's;
      this will be the outside of the brain;
      put this back into the mask, then invert again;
      the effect will be to fill any holes left inside the brain */

   for( ii=0 ; ii < nvox ; ii++ ) mmm[ii] = !mmm[ii] ;

#ifdef USE_MCLUST
   THD_mask_clust( nx,ny,nz, mmm ) ;
#else
   /* get the clusters of what were 0's */

   clar = MCW_find_clusters( nx,ny,nz , 1.0,1.0,1.0 ,
                             MRI_byte , mmm , 1.01   ) ;

   if( clar == NULL ) RETURN(mmm) ; /* should not happen */

   /* find the largest cluster (yet again) */

   for( nmm=iclu=kclu=0 ; iclu < clar->num_clu ; iclu++ ){
     if( clar->clar[iclu]->num_pt > nmm ){
       nmm = clar->clar[iclu]->num_pt; kclu = iclu;
     }
   }

   /* put all clusters at least 20% of the largest one back in */

   nmm = rint(0.2*nmm) ;
   for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
     if( clar->clar[iclu]->num_pt >= nmm )
       MCW_cluster_to_vol( nx,ny,nz , MRI_byte,mmm , clar->clar[iclu] ) ;
   }

   DESTROY_CLARR(clar) ;
#endif

   /* and re-invert mask */

   for( ii=0 ; ii < nvox ; ii++ ) mmm[ii] = !mmm[ii] ;

   RETURN(mmm) ;
}

/*---------------------------------------------------------------------*/
/*! Fill in a byte mask.  Filling is done by looking to each side
    (plus/minus) of a non-filled voxel, and seeing if there is a
    filled voxel on both sides.  This looking is done parallel to
    the x-, y-, and z-axes, out to distance nside voxels.
    - nx,ny,nz = dimensions of mask
    - mmm      = mask itself (will be altered)
    - nside    = width of fill in look to each side
    - Return value is number of filled in voxels
-----------------------------------------------------------------------*/

int THD_mask_fillin_once( int nx, int ny, int nz, byte *mmm, int nside )
{
   int ii,jj,kk , nsx,nsy,nsz , nxy,nxyz , iv,jv,kv,ll , nfill ;
   byte *nnn ;

ENTRY("THD_mask_fillin_once") ;

   if( mmm == NULL || nside <= 0 ) RETURN(0) ;

   nsx = (nx-1)/2 ; if( nsx > nside ) nsx = nside ;
   nsy = (ny-1)/2 ; if( nsy > nside ) nsy = nside ;
   nsz = (nz-1)/2 ; if( nsz > nside ) nsz = nside ;

   if( nsx == 0 && nsy == 0 && nsz == 0 ) RETURN(0) ;

   nxy = nx*ny ; nxyz = nxy*nz ; nfill = 0 ;

   nnn = calloc(1,nxyz) ;  /* stores filled in values */

   /* loop over voxels */

   for( kk=nsz ; kk < nz-nsz ; kk++ ){
     kv = kk*nxy ;
     for( jj=nsy ; jj < ny-nsy ; jj++ ){
       jv = jj*nx + kv ;
       for( ii=nsx ; ii < nx-nsx ; ii++ ){
         iv = ii+jv ;
         if( mmm[iv] ) continue ;     /* already filled */

         /* check in +x direction, then -x if +x hits */

         for( ll=1 ; ll <= nsx ; ll++ ) if( mmm[iv+ll] ) break ;
         if( ll <= nsx ){
           for( ll=1 ; ll <= nsx ; ll++ ) if( mmm[iv-ll] ) break ;
           if( ll <= nsx ){ nnn[iv] = 1 ; nfill++ ; continue ; }
         }

         /* check in +y direction, then -y if +y hits */

         for( ll=1 ; ll <= nsy ; ll++ ) if( mmm[iv+ll*nx] ) break ;
         if( ll <= nsy ){
           for( ll=1 ; ll <= nsy ; ll++ ) if( mmm[iv-ll*nx] ) break ;
           if( ll <= nsy ){ nnn[iv] = 1 ; nfill++ ; continue ; }
         }

         /* check in +z direction, then -z if +z hits */

         for( ll=1 ; ll <= nsz ; ll++ ) if( mmm[iv+ll*nxy] ) break ;
         if( ll <= nsz ){
           for( ll=1 ; ll <= nsz ; ll++ ) if( mmm[iv-ll*nxy] ) break ;
           if( ll <= nsz ){ nnn[iv] = 1 ; nfill++ ; continue ; }
         }
   } } }

   /* copy fills back into mmm */

   if( nfill > 0 ){
     for( iv=0 ; iv < nxyz ; iv++ ) if( nnn[iv] ) mmm[iv] = 1 ;
   }

   free(nnn) ; RETURN(nfill) ;
}

/*----------------------------------------------------------------------*/
/*! Fill in a byte mask, repeatedly until it doesn't fill any more.
    Return value is number of voxels filled.
------------------------------------------------------------------------*/

int THD_mask_fillin_completely( int nx, int ny, int nz, byte *mmm, int nside )
{
   int nfill=0 , kfill ;

ENTRY("THD_mask_fillin_completely") ;

   do{
      kfill = THD_mask_fillin_once( nx,ny,nz , mmm , nside ) ;
      nfill += kfill ;
   } while( kfill > 0 ) ;

   return nfill ;
}

/*------------------------------------------------------------------*/

#ifdef USE_MCLUST
# define DALL 1024
# define CPUT(i,j,k)                                            \
  do{ ijk = THREE_TO_IJK(i,j,k,nx,nxy) ;                        \
      if( mmm[ijk] ){                                           \
        if( nnow == nall ){                                     \
          nall += DALL ;                                        \
          inow = (short *) realloc(inow,sizeof(short)*nall) ;   \
          jnow = (short *) realloc(jnow,sizeof(short)*nall) ;   \
          know = (short *) realloc(know,sizeof(short)*nall) ;   \
        }                                                       \
        inow[nnow] = i ; jnow[nnow] = j ; know[nnow] = k ;      \
        nnow++ ; mmm[ijk] = 0 ;                                 \
      } } while(0)

/*------------------------------------------------------------------*/
/*! Find the biggest cluster of nonzeros in the byte mask mmm.
--------------------------------------------------------------------*/

static void THD_mask_clust( int nx, int ny, int nz, byte *mmm )
{
   int ii,jj,kk, icl ,  nxy,nxyz , ijk , ijk_last , mnum ;
   int ip,jp,kp , im,jm,km ;
   int nbest ; short *ibest, *jbest , *kbest ;
   int nnow  ; short *inow , *jnow  , *know  ; int nall ;

   if( mmm == NULL ) return ;

   nxy = nx*ny ; nxyz = nxy * nz ;

   nbest = 0 ;
   ibest = malloc(sizeof(short)) ;
   jbest = malloc(sizeof(short)) ;
   kbest = malloc(sizeof(short)) ;

   /*--- scan through array, find nonzero point, build a cluster, ... ---*/

   ijk_last = 0 ;
   while(1) {
     /* find next nonzero point */

     for( ijk=ijk_last ; ijk < nxyz ; ijk++ ) if( mmm[ijk] ) break ;
     if( ijk == nxyz ) break ;  /* didn't find any! */

     ijk_last = ijk+1 ;         /* start here next time */

     /* init current cluster list with this point */

     nall = DALL ;
     inow = (short *) malloc(sizeof(short)*DALL) ;
     jnow = (short *) malloc(sizeof(short)*DALL) ;
     know = (short *) malloc(sizeof(short)*DALL) ;
     IJK_TO_THREE(ijk, inow[0],jnow[0],know[0] , nx,nxy) ;
     nnow = 1 ;
     mmm[ijk] = 0 ;             /* clear found point */

     /*--
        for each point in cluster:
           check neighboring points for nonzero entries in mmm
           enter those into cluster
           continue until end of cluster is reached
             (note that cluster is expanding as we progress)
     --*/

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

     /* see if new cluster is larger than best yet */

     if( nnow > nbest ){   /* new is bigger */
       free(ibest) ; free(jbest) ; free(kbest) ;
       nbest = nnow ; ibest = inow ;
       jbest = jnow ; kbest = know ;
     } else {              /* old is bigger */
       free(inow) ; free(jnow) ; free(know) ;
     }

   } /* loop ends when all nonzero points are clustered */

   /* put 1's back in at all points in best cluster */

   for( icl=0 ; icl < nbest ; icl++ ){
      ijk = THREE_TO_IJK(ibest[icl],jbest[icl],kbest[icl],nx,nxy) ;
      mmm[ijk] = 1 ;
   }
   free(ibest) ; free(jbest) ; free(kbest) ;

   return ;
}

/*--------------------------------------------------------------------------*/

void THD_mask_erode( int nx, int ny, int nz, byte *mmm )
{
   int ii,jj,kk , jy,kz, im,jm,km , ip,jp,kp , num ;
   int nxy=nx*ny , nxyz=nxy*nz ;
   byte *nnn ;

   if( mmm == NULL ) return ;

   nnn = calloc(sizeof(byte),nxyz) ;

   for( kk=1 ; kk < nz-1 ; kk++ ){
    km = (kk-1)*nxy ; kp = (kk+1)*nxy ; kz = kk*nxy ;
    for( jj=1 ; jj < ny-1 ; jj++ ){
     jm = (jj-1)*nx ; jp = (jj+1)*nx ; jy = jj*nx ;
     for( ii=1 ; ii < nx-1 ; ii++ ){
       if( mmm[ii+jy+kz] ){           /* count nonzero nbhrs */
         im = ii-1 ; ip = ii+1 ;
         num =  mmm[im+jy+km]
              + mmm[ii+jm+km] + mmm[ii+jy+km] + mmm[ii+jp+km]
              + mmm[ip+jy+km]
              + mmm[im+jm+kz] + mmm[im+jy+kz] + mmm[im+jp+kz]
              + mmm[ii+jm+kz]                 + mmm[ii+jp+kz]
              + mmm[ip+jm+kz] + mmm[ip+jy+kz] + mmm[ip+jp+kz]
              + mmm[im+jy+kp]
              + mmm[ii+jm+kp] + mmm[ii+jy+kp] + mmm[ii+jp+kp]
              + mmm[ip+jy+kp]
         if( num < 16 ) nnn[ii+jy+kz] = 1 ;  /* mark to erode */
       }
   } } }

   for( ii=0 ; ii < nxyz ; ii++ )            /* actually erode */
     if( nnn[ii] ) mmm[ii] = 0 ;

   /* re-dilate eroded voxels that are next to survivors */

}
#endif /* USE_MCLUST */
