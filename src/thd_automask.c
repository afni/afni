#include "mrilib.h"

#define USE_DILATE
#define USE_FILLIN

#undef DEBUG

/*---------------------------------------------------------------------*/
/*! Make a byte mask for a 3D+time dataset -- 13 Aug 2001 - RWCox.
     - compare to thd_makemask.c
     - 05 Mar 2003: modified to put most code into MRI_automask().
-----------------------------------------------------------------------*/

byte * THD_automask( THD_3dim_dataset * dset )
{
   MRI_IMAGE *medim ;
   byte *mmm ;

ENTRY("THD_automask") ;

   /* median at each voxel */

   medim = THD_median_brick(dset) ; if( medim == NULL ) RETURN(NULL);

   mmm = MRI_automask( medim ) ;

   mri_free(medim) ; RETURN(mmm) ;
}

/*---------------------------------------------------------------------*/
/*! Make a byte mask from an image (3D).  Adapted from THD_automask()
    to make it possible to do this on an image directly.
-----------------------------------------------------------------------*/

byte * MRI_automask( MRI_IMAGE * im )
{
   float clip_val , *mar ;
   byte *mmm = NULL ;
   int nvox , ii,jj , nmm , nx,ny,nz ;
   MRI_IMAGE *medim ;

ENTRY("MRI_automask") ;

   if( im == NULL ) return NULL ;

   if( im->kind != MRI_float ) medim = mri_to_float(im) ;
   else                        medim = im ;

   /* clip value to excise small stuff */

   clip_val = THD_cliplevel(medim,0.5) ;

   /* create mask of values above clip value */

   nvox = medim->nvox ;
   mar  = MRI_FLOAT_PTR(medim) ;
   mmm  = (byte *) calloc( sizeof(byte), nvox ) ;
   for( nmm=ii=0 ; ii < nvox ; ii++ )
     if( mar[ii] >= clip_val ){ mmm[ii] = 1; nmm++; }

   if( im != medim ) mri_free(medim) ;
   if( nmm == 0 ) RETURN(mmm) ;  /* should not happen */

   /*-- 10 Apr 2002: only keep the largest connected component --*/

   nx = im->nx ; ny = im->ny ; nz = im->nz ;

   THD_mask_clust( nx,ny,nz, mmm ) ;

   /* 18 Apr 2002: now erode the resulting volume
                   (to break off any thinly attached pieces) */

   THD_mask_erode( nx,ny,nz, mmm ) ;

   /* now recluster it, and again keep only the largest survivor */

   THD_mask_clust( nx,ny,nz, mmm ) ;

#ifdef USE_FILLIN
   /* 19 Apr 2002: fill in small holes */

   ii = THD_mask_fillin_once( nx,ny,nz , mmm , 1 ) ;
   if( ii > 0 ){
     ii = THD_mask_fillin_once( nx,ny,nz , mmm , 1 ) ;
     if( ii > 0 ){
       ii = THD_mask_fillin_once( nx,ny,nz , mmm , 1 ) ;
     }
   }

   nmm = 1 ;
   jj  = rint(0.016*nx) ; nmm = MAX(nmm,jj) ;
   jj  = rint(0.016*ny) ; nmm = MAX(nmm,jj) ;
   jj  = rint(0.016*nz) ; nmm = MAX(nmm,jj) ;

   if( nmm > 1 || ii > 0 ){
     for( ii=2 ; ii < nmm ; ii++ )
       THD_mask_fillin_once( nx,ny,nz , mmm , ii ) ;
     THD_mask_fillin_completely( nx,ny,nz, mmm , nmm ) ;
   }
#endif

   /* 28 May 2002:
      invert the mask, then find the largest cluster of 1's;
      this will be the outside of the brain;
      put this back into the mask, then invert again;
      the effect will be to fill any holes left inside the brain */

   for( ii=0 ; ii < nvox ; ii++ ) mmm[ii] = !mmm[ii] ;

   THD_mask_clust( nx,ny,nz, mmm ) ;

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
   int nx2,nx3,nx4 , nxy2,nxy3,nxy4 ;

ENTRY("THD_mask_fillin_once") ;

   if( mmm == NULL || nside <= 0 ) RETURN(0) ;

   nsx = (nx-1)/2 ; if( nsx > nside ) nsx = nside ;
   nsy = (ny-1)/2 ; if( nsy > nside ) nsy = nside ;
   nsz = (nz-1)/2 ; if( nsz > nside ) nsz = nside ;

   if( nsx == 0 && nsy == 0 && nsz == 0 ) RETURN(0) ;

#ifdef DEBUG
   fprintf(stderr,"THD_mask_fillin_once: nsx=%d nsy=%d nsz=%d\n",nsx,nsy,nsz);
#endif

   nxy = nx*ny ; nxyz = nxy*nz ; nfill = 0 ;

   nx2  = 2*nx  ; nx3  = 3*nx  ; nx4  = 4*nx  ;
   nxy2 = 2*nxy ; nxy3 = 3*nxy ; nxy4 = 4*nxy ;

   nnn = calloc(1,nxyz) ;  /* stores filled in values */

   /* loop over voxels */

#define FILLVOX                                     \
 do{ nnn[iv] = 1; nfill++; goto NextVox; } while(0)

   for( kk=nsz ; kk < nz-nsz ; kk++ ){
     kv = kk*nxy ;
     for( jj=nsy ; jj < ny-nsy ; jj++ ){
       jv = jj*nx + kv ;
       for( ii=nsx ; ii < nx-nsx ; ii++ ){
         iv = ii+jv ;
         if( mmm[iv] ) continue ;     /* already filled */

         /* check in +x direction, then -x if +x hits */

         switch( nsx ){
           case 1:
             if( mmm[iv+1] && mmm[iv-1] ) FILLVOX;
           break ;

           case 2:
             if( (mmm[iv+1]||mmm[iv+2]) &&
                 (mmm[iv-1]||mmm[iv-2])   ) FILLVOX;
           break ;

           case 3:
             if( (mmm[iv+1]||mmm[iv+2]||mmm[iv+3]) &&
                 (mmm[iv-1]||mmm[iv-2]||mmm[iv-3])   ) FILLVOX;
           break ;

           case 4:
             if( (mmm[iv+1]||mmm[iv+2]||mmm[iv+3]||mmm[iv+4]) &&
                 (mmm[iv-1]||mmm[iv-2]||mmm[iv-3]||mmm[iv-4])   ) FILLVOX;
           break ;

           default:
             for( ll=1 ; ll <= nsx ; ll++ ) if( mmm[iv+ll] ) break ;
             if( ll <= nsx ){
               for( ll=1 ; ll <= nsx ; ll++ ) if( mmm[iv-ll] ) break ;
               if( ll <= nsx ) FILLVOX;
             }
           break ;
         }

         /* check in +y direction, then -y if +y hits */

         switch( nsy ){
           case 1:
             if( mmm[iv+nx] && mmm[iv-nx] ) FILLVOX;
           break ;

           case 2:
             if( (mmm[iv+nx]||mmm[iv+nx2]) &&
                 (mmm[iv-nx]||mmm[iv-nx2])   ) FILLVOX;
           break ;

           case 3:
             if( (mmm[iv+nx]||mmm[iv+nx2]||mmm[iv+nx3]) &&
                 (mmm[iv-nx]||mmm[iv-nx2]||mmm[iv-nx3])   ) FILLVOX;
           break ;

           case 4:
             if( (mmm[iv+nx]||mmm[iv+nx2]||mmm[iv+nx3]||mmm[iv+nx4]) &&
                 (mmm[iv-nx]||mmm[iv-nx2]||mmm[iv-nx3]||mmm[iv-nx4])   ) FILLVOX;
           break ;

           default:
             for( ll=1 ; ll <= nsy ; ll++ ) if( mmm[iv+ll*nx] ) break ;
             if( ll <= nsy ){
               for( ll=1 ; ll <= nsy ; ll++ ) if( mmm[iv-ll*nx] ) break ;
               if( ll <= nsy ) FILLVOX;
             }
           break ;
         }

         /* check in +z direction, then -z if +z hits */

         switch( nsz ){
           case 1:
             if( mmm[iv+nxy] && mmm[iv-nxy] ) FILLVOX;
           break ;

           case 2:
             if( (mmm[iv+nxy]||mmm[iv+nxy2]) &&
                 (mmm[iv-nxy]||mmm[iv-nxy2])   ) FILLVOX;
           break ;

           case 3:
             if( (mmm[iv+nxy]||mmm[iv+nxy2]||mmm[iv+nxy3]) &&
                 (mmm[iv-nxy]||mmm[iv-nxy2]||mmm[iv-nxy3])   ) FILLVOX;
           break ;

           case 4:
             if( (mmm[iv+nxy]||mmm[iv+nxy2]||mmm[iv+nxy3]||mmm[iv+nxy4]) &&
                 (mmm[iv-nxy]||mmm[iv-nxy2]||mmm[iv-nxy3]||mmm[iv-nxy4])   ) FILLVOX;
           break ;

           default:
             for( ll=1 ; ll <= nsz ; ll++ ) if( mmm[iv+ll*nxy] ) break ;
             if( ll <= nsz ){
               for( ll=1 ; ll <= nsz ; ll++ ) if( mmm[iv-ll*nxy] ) break ;
               if( ll <= nsz ) FILLVOX;
             }
           break ;
         }

         NextVox: ; /* end of loop over ii */
   } } }

   /* copy fills back into mmm */

   if( nfill > 0 ){
     for( iv=0 ; iv < nxyz ; iv++ ) if( nnn[iv] ) mmm[iv] = 1 ;
   }

#ifdef DEBUG
   fprintf(stderr,"THD_mask_fillin_once: nfill=%d\n",nfill) ;
#endif

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

   RETURN(nfill) ;
}

/*------------------------------------------------------------------*/

# define DALL 1024  /* Allocation size for cluster arrays */

/*! Put (i,j,k) into the current cluster, if it is nonzero. */

# define CPUT(i,j,k)                                            \
  do{ ijk = THREE_TO_IJK(i,j,k,nx,nxy) ;                        \
      if( mmm[ijk] ){                                           \
        if( nnow == nall ){ /* increase array lengths */        \
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

void THD_mask_clust( int nx, int ny, int nz, byte *mmm )
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

     mmm[ijk] = 0 ;                                /* clear found point */
     nall = DALL ;                                 /* # allocated pts */
     nnow = 1 ;                                    /* # pts in cluster */
     inow = (short *) malloc(sizeof(short)*DALL) ; /* coords of pts */
     jnow = (short *) malloc(sizeof(short)*DALL) ;
     know = (short *) malloc(sizeof(short)*DALL) ;
     IJK_TO_THREE(ijk, inow[0],jnow[0],know[0] , nx,nxy) ;

     /*--
        for each point in cluster:
           check neighboring points for nonzero entries in mmm
           enter those into cluster (and clear them in mmm)
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

     /* see if now cluster is larger than best yet */

     if( nnow > nbest ){                         /* now is bigger; */
       free(ibest) ; free(jbest) ; free(kbest) ; /* replace best  */
       nbest = nnow ; ibest = inow ;             /* with now     */
       jbest = jnow ; kbest = know ;
     } else {                                    /* old is bigger */
       free(inow) ; free(jnow) ; free(know) ;    /* toss now     */
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
/*! Erode away nonzero voxels that aren't neighbored by mostly other
    nonzero voxels.  Then restore those that were eroded that are
    neighbors of survivors.  The neighbors are the 18 voxels closest
    in 3D (nearest and next-nearest neighbors).
----------------------------------------------------------------------------*/

void THD_mask_erode( int nx, int ny, int nz, byte *mmm )
{
   int ii,jj,kk , jy,kz, im,jm,km , ip,jp,kp , num ;
   int nxy=nx*ny , nxyz=nxy*nz ;
   byte *nnn ;

   if( mmm == NULL ) return ;

   nnn = calloc(sizeof(byte),nxyz) ;  /* mask of eroded voxels */

   /* mark interior voxels that don't have 17 out of 18 nonzero nbhrs */

   for( kk=0 ; kk < nz ; kk++ ){
    kz = kk*nxy ; km = kz-nxy ; kp = kz+nxy ;
         if( kk == 0    ) km = kz ;
    else if( kk == nz-1 ) kp = kz ;

    for( jj=0 ; jj < ny ; jj++ ){
     jy = jj*nx ; jm = jy-nx ; jp = jy+nx ;
          if( jj == 0    ) jm = jy ;
     else if( jj == ny-1 ) jp = jy ;

     for( ii=0 ; ii < nx ; ii++ ){
       if( mmm[ii+jy+kz] ){           /* count nonzero nbhrs */
         im = ii-1 ; ip = ii+1 ;
              if( ii == 0    ) im = 0 ;
         else if( ii == nx-1 ) ip = ii ;
         num =  mmm[im+jy+km]
              + mmm[ii+jm+km] + mmm[ii+jy+km] + mmm[ii+jp+km]
              + mmm[ip+jy+km]
              + mmm[im+jm+kz] + mmm[im+jy+kz] + mmm[im+jp+kz]
              + mmm[ii+jm+kz]                 + mmm[ii+jp+kz]
              + mmm[ip+jm+kz] + mmm[ip+jy+kz] + mmm[ip+jp+kz]
              + mmm[im+jy+kp]
              + mmm[ii+jm+kp] + mmm[ii+jy+kp] + mmm[ii+jp+kp]
              + mmm[ip+jy+kp] ;
         if( num < 17 ) nnn[ii+jy+kz] = 1 ;  /* mark to erode */
       }
   } } }

   for( ii=0 ; ii < nxyz ; ii++ )            /* actually erode */
     if( nnn[ii] ) mmm[ii] = 0 ;

   /* re-dilate eroded voxels that are next to survivors */

#ifdef USE_DILATE
   for( kk=0 ; kk < nz ; kk++ ){
    kz = kk*nxy ; km = kz-nxy ; kp = kz+nxy ;
         if( kk == 0    ) km = kz ;
    else if( kk == nz-1 ) kp = kz ;

    for( jj=0 ; jj < ny ; jj++ ){
     jy = jj*nx ; jm = jy-nx ; jp = jy+nx ;
          if( jj == 0    ) jm = jy ;
     else if( jj == ny-1 ) jp = jy ;

     for( ii=0 ; ii < nx ; ii++ ){
       if( nnn[ii+jy+kz] ){           /* was eroded */
         im = ii-1 ; ip = ii+1 ;
              if( ii == 0    ) im = 0 ;
         else if( ii == nx-1 ) ip = ii ;
         nnn[ii+jy+kz] =              /* see if has any nbhrs */
               mmm[im+jy+km]
            || mmm[ii+jm+km] || mmm[ii+jy+km] || mmm[ii+jp+km]
            || mmm[ip+jy+km]
            || mmm[im+jm+kz] || mmm[im+jy+kz] || mmm[im+jp+kz]
            || mmm[ii+jm+kz]                  || mmm[ii+jp+kz]
            || mmm[ip+jm+kz] || mmm[ip+jy+kz] || mmm[ip+jp+kz]
            || mmm[im+jy+kp]
            || mmm[ii+jm+kp] || mmm[ii+jy+kp] || mmm[ii+jp+kp]
            || mmm[ip+jy+kp] ;
       }
   } } }

   /* actually do the dilation */

   for( ii=0 ; ii < nxyz ; ii++ )
     if( nnn[ii] ) mmm[ii] = 1 ;
#endif

   free(nnn) ; return ;
}

/*--------------------------------------------------------------------------*/
/*! Dilate a mask - that is, fill in zero voxels that have at least ndil
    neighbors in the mask.  The neighbors are the 18 voxels closest
    in 3D (nearest and next-nearest neighbors).
----------------------------------------------------------------------------*/

void THD_mask_dilate( int nx, int ny, int nz, byte *mmm , int ndil )
{
   int ii,jj,kk , jy,kz, im,jm,km , ip,jp,kp , num ;
   int nxy=nx*ny , nxyz=nxy*nz ;
   byte *nnn ;

   if( mmm == NULL ) return ;
        if( ndil < 1  ) ndil =  1 ;
   else if( ndil > 17 ) ndil = 17 ;

   nnn = calloc(sizeof(byte),nxyz) ;  /* mask of dilated voxels */

   /* mark exterior voxels neighboring enough interior voxels */

   for( kk=0 ; kk < nz ; kk++ ){
    kz = kk*nxy ; km = kz-nxy ; kp = kz+nxy ;
         if( kk == 0    ) km = kz ;
    else if( kk == nz-1 ) kp = kz ;

    for( jj=0 ; jj < ny ; jj++ ){
     jy = jj*nx ; jm = jy-nx ; jp = jy+nx ;
          if( jj == 0    ) jm = jy ;
     else if( jj == ny-1 ) jp = jy ;

     for( ii=0 ; ii < nx ; ii++ ){
       if( mmm[ii+jy+kz] == 0 ){           /* count nonzero nbhrs */
         im = ii-1 ; ip = ii+1 ;
              if( ii == 0    ) im = 0 ;
         else if( ii == nx-1 ) ip = ii ;
         num =  mmm[im+jy+km]
              + mmm[ii+jm+km] + mmm[ii+jy+km] + mmm[ii+jp+km]
              + mmm[ip+jy+km]
              + mmm[im+jm+kz] + mmm[im+jy+kz] + mmm[im+jp+kz]
              + mmm[ii+jm+kz]                 + mmm[ii+jp+kz]
              + mmm[ip+jm+kz] + mmm[ip+jy+kz] + mmm[ip+jp+kz]
              + mmm[im+jy+kp]
              + mmm[ii+jm+kp] + mmm[ii+jy+kp] + mmm[ii+jp+kp]
              + mmm[ip+jy+kp] ;
         if( num >= ndil ) nnn[ii+jy+kz] = 1 ;  /* mark to dilate */
       }
   } } }

   for( ii=0 ; ii < nxyz ; ii++ )            /* actually dilate */
     if( nnn[ii] ) mmm[ii] = 1 ;

   free(nnn) ; return ;
}

/*---------------------------------------------------------------------*/
/*! Find a bounding box for the main cluster of large-ish voxels.
    [xm..xp] will be box for x index, etc.
-----------------------------------------------------------------------*/

void THD_autobbox( THD_3dim_dataset *dset ,
                   int *xm, int *xp , int *ym, int *yp , int *zm, int *zp )
{
   MRI_IMAGE *medim ;
   float clip_val , *mar ;
   int nvox , ii ;

ENTRY("THD_autobbox") ;

   medim = THD_median_brick(dset) ; if( medim == NULL ) EXRETURN ;

   mar  = MRI_FLOAT_PTR(medim) ;
   nvox = medim->nvox ;
   for( ii=0 ; ii < nvox ; ii++ ) mar[ii] = fabs(mar[ii]) ;

   clip_val = THD_cliplevel(medim,0.5) ;
   for( ii=0 ; ii < nvox ; ii++ )
     if( mar[ii] < clip_val ) mar[ii] = 0.0 ;

   MRI_autobbox( medim , xm,xp , ym,yp , zm,zp ) ;

   mri_free(medim) ; EXRETURN ;
}

/*------------------------------------------------------------------------*/

void MRI_autobbox( MRI_IMAGE *qim ,
                   int *xm, int *xp , int *ym, int *yp , int *zm, int *zp )
{
   MRI_IMAGE *fim ;
   float *mar ;
   byte *mmm = NULL ;
   int nvox , ii,jj,kk , nmm , nx,ny,nz,nxy ;

ENTRY("MRI_autobbox") ;

   /* find largest component as in first part of THD_automask() */

   if( qim->kind != MRI_float ) fim = mri_to_float(qim) ;
   else                         fim = qim ;

   nvox = fim->nvox ;
   mar  = MRI_FLOAT_PTR(fim) ;
   mmm  = (byte *) calloc( sizeof(byte) , nvox ) ;
   for( nmm=ii=0 ; ii < nvox ; ii++ )
     if( mar[ii] != 0.0 ){ mmm[ii] = 1; nmm++; }

   if( fim != qim ) mri_free(fim) ;

   if( nmm == 0 ){ free(mmm); EXRETURN; }

   nx = qim->nx; ny = qim->ny; nz = qim->nz; nxy = nx*ny;

   THD_mask_clust( nx,ny,nz, mmm ) ;
   THD_mask_erode( nx,ny,nz, mmm ) ;
   THD_mask_clust( nx,ny,nz, mmm ) ;

   /* For each plane direction,
      find the first and last index that have nonzero voxels in that plane */

   if( xm != NULL ){
     for( ii=0 ; ii < nx ; ii++ )
       for( kk=0 ; kk < nz ; kk++ )
         for( jj=0 ; jj < ny ; jj++ )
           if( mmm[ii+jj*nx+kk*nxy] ) goto CP5 ;
     CP5: *xm = ii ;
   }

   if( xp != NULL ){
     for( ii=nx-1 ; ii >= 0 ; ii-- )
       for( kk=0 ; kk < nz ; kk++ )
         for( jj=0 ; jj < ny ; jj++ )
           if( mmm[ii+jj*nx+kk*nxy] ) goto CP6 ;
     CP6: *xp = ii ;
   }

   if( ym != NULL ){
     for( jj=0 ; jj < ny ; jj++ )
       for( kk=0 ; kk < nz ; kk++ )
         for( ii=0 ; ii < nx ; ii++ )
           if( mmm[ii+jj*nx+kk*nxy] ) goto CP3 ;
     CP3: *ym = jj ;
   }

   if( yp != NULL ){
     for( jj=ny-1 ; jj >= 0 ; jj-- )
       for( kk=0 ; kk < nz ; kk++ )
         for( ii=0 ; ii < nx ; ii++ )
           if( mmm[ii+jj*nx+kk*nxy] ) goto CP4 ;
     CP4: *yp = jj ;
   }

   if( zm != NULL ){
     for( kk=0 ; kk < nz ; kk++ )
       for( jj=0 ; jj < ny ; jj++ )
         for( ii=0 ; ii < nx ; ii++ )
           if( mmm[ii+jj*nx+kk*nxy] ) goto CP1 ;
     CP1: *zm = kk ;
   }

   if( zp != NULL ){
     for( kk=nz-1 ; kk >= 0 ; kk-- )
       for( jj=0 ; jj < ny ; jj++ )
         for( ii=0 ; ii < nx ; ii++ )
           if( mmm[ii+jj*nx+kk*nxy] ) goto CP2 ;
     CP2: *zp = kk ;
   }

   free(mmm) ; EXRETURN ;
}
