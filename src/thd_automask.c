#include "mrilib.h"

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

   /* 18 Apr 2002: now erode the resulting volume
                   (to break off any thinly attached pieces) */

   MCW_erode_clusters( nx,ny,nz , 1.0,1.0,1.0 ,
                       MRI_byte,mmm , 1.42 , 0.90 , 1 ) ;

   /* now recluster it, and again keep only the largest survivor */

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
