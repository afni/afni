#include "mrilib.h"

#undef  DEBUG

#undef  ASSIF
#define ASSIF(p,v) if( p!= NULL ) *p = v

static int dall = 1024 ;

# define DALL 1024  /* Allocation size for cluster arrays */

/*---------------------------------------------------------------------*/

static int verb = 0 ;                            /* 28 Oct 2003 */
void THD_automask_verbose( int v ){ verb = v ; }

/*---------------------------------------------------------------------*/

static int exterior_clip = 0 ;
void THD_automask_extclip( int e ){ exterior_clip = e ; }

/*---------------------------------------------------------------------*/

static float clfrac = 0.5f ;                     /* 20 Mar 2006 */
void THD_automask_set_clipfrac( float f )
{
  clfrac = (f >= 0.1f && f <= 0.9f) ? f : 0.5f ;
}

/*---------------------------------------------------------------------*/

/* parameters for erode/restore peeling */

static int peelcount =  1 ;                      /* 24 Oct 2006 */
static int peelthr   = 17 ;
void THD_automask_set_peelcounts( int p , int t )
{
  peelcount = (p > 0)             ? p :  1 ;
  peelthr   = (t >= 9 && t <= 18) ? t : 17 ;
}

/*---------------------------------------------------------------------*/

static int gradualize = 1 ;
void THD_automask_set_gradualize( int n ){ gradualize = n; }

/*---------------------------------------------------------------------*/

static int cheapo = 0 ;
void THD_automask_set_cheapo( int n ){ cheapo = n; } /* 13 Aug 2007 */

/*---------------------------------------------------------------------*/

INLINE int mask_count( int nvox , byte *mmm )
{
   register int ii , nn ;
   if( nvox <= 0 || mmm == NULL ) return 0 ;
   for( nn=ii=0 ; ii < nvox ; ii++ ) nn += (mmm[ii] != 0) ;
   return nn ;
}

/*---------------------------------------------------------------------*/

int mask_intersect_count( int nvox , byte *mmm , byte *nnn )
{
   register int nint , ii ;
   if( nvox <= 0 || mmm == NULL || nnn == NULL ) return 0 ;
   for( nint=ii=0 ; ii < nvox ; ii++ ) nint += (mmm[ii] && nnn[ii]) ;
   return nint ;
}

/*---------------------------------------------------------------------*/

int mask_union_count( int nvox , byte *mmm , byte *nnn )
{
   register int nint , ii ;
   if( nvox <= 0 ) return 0 ;
   if( mmm == NULL && nnn != NULL ) return mask_count( nvox , nnn ) ;
   if( mmm != NULL && nnn == NULL ) return mask_count( nvox , mmm ) ;
   for( nint=ii=0 ; ii < nvox ; ii++ ) nint += (mmm[ii] || nnn[ii]) ;
   return nint ;
}

/*---------------------------------------------------------------------*/

float_triple mask_rgyrate( int nx, int ny, int nz , byte *mmm )
{
   float_triple xyz={0.0f,0.0f,0.0f} ;
   float xx,yy,zz , kq,jq , xc,yc,zc ; int ii,jj,kk , vv , nmmm ;

   if( nx < 1 || ny < 1 || nz < 1 || mmm == NULL ) return xyz ;

   xc = yc = zc = 0.0f ; nmmm = 0 ;
   for( vv=kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++,vv++ ){
       if( mmm[vv] ){ xc += ii ; yc += jj ; zc += kk ; nmmm++ ; }
   }}}
   if( nmmm <= 1 ) return xyz ;
   xc /= nmmm ; yc /= nmmm ; zc /= nmmm ;

   xx = yy = zz = 0.0f ;
   for( vv=kk=0 ; kk < nz ; kk++ ){
    kq = (kk-zc)*(kk-zc) ;
    for( jj=0 ; jj < ny ; jj++ ){
     jq = (jj-yc)*(jj-yc) ;
     for( ii=0 ; ii < nx ; ii++,vv++ ){
       if( mmm[vv] ){ xx += (ii-xc)*(ii-xc) ; yy += jq ; zz += kq ; }
   }}}

   xyz.a = xx/nmmm ; xyz.b = yy/nmmm ; xyz.c = zz/nmmm ;
   return xyz ;
}

/*---------------------------------------------------------------------*/
/*! Make a byte mask for a 3D+time dataset -- 13 Aug 2001 - RWCox.
     - compare to thd_makemask.c
     - 05 Mar 2003: modified to put most code into mri_automask_image().
-----------------------------------------------------------------------*/

byte * THD_automask( THD_3dim_dataset *dset )
{
   MRI_IMAGE *medim ;
   byte *mmm ;

ENTRY("THD_automask") ;

   /* median at each voxel */

#if 0
   medim = THD_median_brick(dset) ; if( medim == NULL ) RETURN(NULL);
#else
   medim = THD_aveabs_brick(dset) ; if( medim == NULL ) RETURN(NULL);
#endif

   mmm = mri_automask_image( medim ) ;

   mri_free(medim) ; RETURN(mmm) ;
}

/*---------------------------------------------------------------------*/
/*! Make a byte mask from the average of an array of 3D images.
    We assume that they all have the same (nx,ny,nz) dimensions.
-----------------------------------------------------------------------*/

byte * mri_automask_imarr( MRI_IMARR *imar )  /* 18 Nov 2004 */
{
   MRI_IMAGE *avim , *tim , *qim ;
   byte *mmm ;
   int ii , jj , nvox,nim ;
   float fac , *avar , *qar ;

ENTRY("mri_automask_imarr") ;

   if( imar == NULL || IMARR_COUNT(imar) < 1 ) RETURN(NULL) ;

   nim = IMARR_COUNT(imar) ;
   if( nim == 1 ){
     mmm = mri_automask_image( IMARR_SUBIMAGE(imar,0) ) ;
     RETURN(mmm) ;
   }

   avim = mri_new_conforming( IMARR_SUBIMAGE(imar,0) , MRI_float ) ;
   avar = MRI_FLOAT_PTR(avim) ;
   nvox = avim->nvox ;
   for( jj=0 ; jj < nim ; jj++ ){
     tim = IMARR_SUBIMAGE(imar,jj) ;
     if( tim->kind != MRI_float ) qim = mri_to_float(tim) ;
     else                         qim = tim ;
     qar = MRI_FLOAT_PTR(qim) ;
     for( ii=0 ; ii < nvox ; ii++ ) avar[ii] += qar[ii] ;
     if( qim != tim ) mri_free(qim) ;
   }
   fac = 1.0f / (float)nim ;
   for( ii=0 ; ii < nvox ; ii++ ) avar[ii] *= fac ;
   mmm = mri_automask_image( avim ) ;
   mri_free(avim) ;
   RETURN(mmm) ;
}

/*---------------------------------------------------------------------*/
/*! Make a byte mask from an image (3D).  Adapted from THD_automask()
    to make it possible to do this on an image directly.
-----------------------------------------------------------------------*/

byte * mri_automask_image( MRI_IMAGE *im )
{
   float clip_val , *mar ;
   byte *mmm = NULL ;
   int nvox , ii,jj , nmm , nx,ny,nz ;
   MRI_IMAGE *medim ;

ENTRY("mri_automask_image") ;

   if( im == NULL )RETURN(NULL) ;

   if( im->kind != MRI_float ) medim = mri_to_float(im) ;
   else                        medim = im ;

   /* find clip value to excise small stuff */

   clip_val = THD_cliplevel(medim,clfrac) ;

   if( verb ) ININFO_message("Fixed clip level = %f\n",clip_val) ;

   /* create mask of values above clip value */

   nvox = medim->nvox ;
   mar  = MRI_FLOAT_PTR(medim) ;
   mmm  = (byte *) calloc( sizeof(byte), nvox ) ;

   if( !gradualize ){
     for( nmm=ii=0 ; ii < nvox ; ii++ )
       if( mar[ii] >= clip_val ){ mmm[ii] = 1; nmm++; }
   } else {
     MRI_IMAGE *cim; float *car, cbot=1.e+38,ctop=-1.e+38 ;
     cim = THD_cliplevel_gradual(medim,clfrac); car = MRI_FLOAT_PTR(cim);
     for( nmm=ii=0 ; ii < nvox ; ii++ ){
       if( mar[ii] >= car[ii] ){ mmm[ii] = 1; nmm++; }
       if( car[ii] < cbot ) cbot = car[ii] ;
       if( car[ii] > ctop ) ctop = car[ii] ;
     }
     if( verb ) ININFO_message("Used gradual clip level = %f .. %f",cbot,ctop) ;
     mri_free(cim) ;
   }

   if( verb ) ININFO_message("Number voxels above clip level = %d\n",nmm) ;
   if( im != medim && (!exterior_clip || nmm==0) ){ mri_free(medim); medim=NULL; }
   if( nmm == 0 ){ cheapo=0; RETURN(mmm); }  /* should not happen */

   /*-- 6 Mar 2009: if we don't have volume data, stop here [rickr] --*/
   if( im->nx < 2 || im->ny < 2 || im->nz < 2 ) RETURN(mmm);

   /*-- 10 Apr 2002: only keep the largest connected component --*/

   nx = im->nx ; ny = im->ny ; nz = im->nz ;
   dall = (nx*ny*nz)/128 ;  /* allocation delta for clustering */

   THD_mask_clust( nx,ny,nz, mmm ) ;

   /* 18 Apr 2002: now erode the resulting volume
                   (to break off any thinly attached pieces) */

#if 0
   if( peelcount > 1 ){                              /* 25 Oct 2006 */
     THD_mask_erodemany( nx,ny,nz, mmm, 1 ) ;
     THD_mask_clust( nx,ny,nz, mmm ) ;
     THD_mask_fillin_once( nx,ny,nz , mmm , 1 ) ;
   }
#endif

   THD_mask_erodemany( nx,ny,nz, mmm, peelcount ) ;  /* 24 Oct 2006: multiple layers */

   /* now recluster it, and again keep only the largest survivor */

   THD_mask_clust( nx,ny,nz, mmm ) ;

   /* 19 Apr 2002: fill in small holes */

   jj = ii = THD_mask_fillin_once( nx,ny,nz , mmm , 1 ) ;
   if( ii > 0 ){
     jj += ii = THD_mask_fillin_once( nx,ny,nz , mmm , 1 ) ;
     if( ii > 0 ){
       jj += ii = THD_mask_fillin_once( nx,ny,nz , mmm , 1 ) ;
     }
   }

   if( cheapo ){
     if( medim != im ) mri_free(medim) ;  /* 13 Aug 2007 */
     cheapo = 0 ; RETURN(mmm) ;
   }

   if( jj > 0 && verb )
    ININFO_message("Filled %5d voxels in small holes; now have %d voxels\n",
            jj , mask_count(nvox,mmm) ) ;

   nmm = 1 ;
   jj  = rint(0.016*nx) ; nmm = MAX(nmm,jj) ;
   jj  = rint(0.016*ny) ; nmm = MAX(nmm,jj) ;
   jj  = rint(0.016*nz) ; nmm = MAX(nmm,jj) ;

   if( nmm > 1 || jj > 0 ){
     for( jj=0,ii=2 ; ii < nmm ; ii++ )
       jj += THD_mask_fillin_once( nx,ny,nz , mmm , ii ) ;
     jj += THD_mask_fillin_completely( nx,ny,nz, mmm , nmm ) ;
     if( jj > 0 && verb )
      ININFO_message("Filled %5d voxels in large holes; now have %d voxels\n",
              jj , mask_count(nvox,mmm) ) ;
   }

   THD_mask_erodemany( nx,ny,nz, mmm, 1 ) ;
   THD_mask_clust( nx,ny,nz, mmm ) ;

   /* 28 May 2002:
      invert the mask, then find the largest cluster of 1's;
      this will be the outside of the brain;
      put this back into the mask, then invert again;
      the effect will be to fill any holes left inside the brain */

   for( ii=0 ; ii < nvox ; ii++ ) mmm[ii] = !mmm[ii] ;

   if( verb ) ININFO_message("Clustering non-brain voxels ...\n") ;
   THD_mask_clust( nx,ny,nz, mmm ) ;

   /* mask is now 1 for non-brain voxels;
      if we want to clip off voxels neighboring the non-brain
      mask AND whose values are below clip_val, do so now     */

   if( exterior_clip ){
     float tclip=9999.9*clip_val ;
     jj = THD_mask_clip_neighbors( nx,ny,nz , mmm , clip_val,tclip,mar ) ;
     if( im != medim ) mri_free(medim) ;
     if( jj > 0 && verb )
       ININFO_message("Removed  %d exterior voxels below clip level\n",jj);
   } else {
     jj = 0 ;
   }

   /* and re-invert mask to get brain voxels */

   for( ii=0 ; ii < nvox ; ii++ ) mmm[ii] = !mmm[ii] ;
   if( verb ) ININFO_message("Mask now has %d voxels\n",mask_count(nvox,mmm)) ;

   if( exterior_clip && jj > 0 ){
     THD_mask_erodemany( nx,ny,nz, mmm, 1 ) ;
     THD_mask_clust( nx,ny,nz, mmm ) ;
   }

   RETURN(mmm) ;
}

/*---------------------------------------------------------------------*/
/*! Find voxels not in the mask, but neighboring it, that are also
    below the clip threshold in mar[].  Add these to the mask.
    Repeat until done.  Return value is number of voxels added.
-----------------------------------------------------------------------*/

int THD_mask_clip_neighbors( int nx, int ny, int nz ,
                            byte *mmm, float clip_val, float tclip, float *mar )
{
   int ii,jj,kk , ntot=0,nnew , jm,jp,j3 , km,kp,k3 , im,ip,i3 , nxy=nx*ny ;

   if( mmm == NULL || mar == NULL ) return 0 ;

   do{
    nnew = 0 ;
    for( kk=1 ; kk < nz-1 ; kk++ ){
     k3 = kk*nxy ;
     for( jj=1 ; jj < ny-1 ; jj++ ){
      j3 = k3 + jj*nx ;
      for( ii=1 ; ii < nx-1 ; ii++ ){
       i3 = ii+j3 ;
       if( mmm[i3] ||                                             /* in mask */
           (mar[i3] >= clip_val && mar[i3] <= tclip) ) continue ; /* or is OK */

       /* If here, voxel IS NOT in mask, and IS below threshold.
          If any neighbors are also in mask, then add it to mask. */

       if( mmm[i3-1]   || mmm[i3+1]   ||
           mmm[i3-nx]  || mmm[i3+nx]  ||
           mmm[i3-nxy] || mmm[i3+nxy]   ){ mmm[i3] = 1; nnew++; }
    }}}
    ntot += nnew ;
   } while( nnew > 0 ) ;

   return ntot ;
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
   ININFO_message("THD_mask_fillin_once: nsx=%d nsy=%d nsz=%d\n",nsx,nsy,nsz);
#endif

   nxy = nx*ny ; nxyz = nxy*nz ; nfill = 0 ;

   nx2  = 2*nx  ; nx3  = 3*nx  ; nx4  = 4*nx  ;
   nxy2 = 2*nxy ; nxy3 = 3*nxy ; nxy4 = 4*nxy ;

   nnn = AFMALL(byte, nxyz) ;  /* stores filled in values */

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
   ININFO_message("THD_mask_fillin_once: nfill=%d\n",nfill) ;
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

/*! Put (i,j,k) into the current cluster, if it is nonzero. */

# define CPUT(i,j,k)                                            \
  do{ ijk = THREE_TO_IJK(i,j,k,nx,nxy) ;                        \
      if( mmm[ijk] ){                                           \
        if( nnow == nall ){ /* increase array lengths */        \
          nall += dall + nall/4 ;                               \
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

ENTRY("THD_mask_clust") ;

   if( mmm == NULL ) EXRETURN ;

   nxy = nx*ny ; nxyz = nxy * nz ;

   nbest = 0 ;
   ibest = AFMALL(short, sizeof(short)) ;
   jbest = AFMALL(short, sizeof(short)) ;
   kbest = AFMALL(short, sizeof(short)) ;

   /*--- scan through array, find nonzero point, build a cluster, ... ---*/
   if(verb) ININFO_message("Clustering voxels ...");

   ijk_last = 0 ; if( dall < DALL ) dall = DALL ;
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

   if( verb ) ININFO_message("Largest cluster has %d voxels\n",nbest) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Erode away nonzero voxels that aren't neighbored by mostly other
    nonzero voxels.  Then restore those that were eroded that are
    neighbors of survivors.  The neighbors are the 18 voxels closest
    in 3D (nearest and next-nearest neighbors).
----------------------------------------------------------------------------*/

void THD_mask_erode( int nx, int ny, int nz, byte *mmm, int redilate )
{
   int ii,jj,kk , jy,kz, im,jm,km , ip,jp,kp , num ;
   int nxy=nx*ny , nxyz=nxy*nz ;
   byte *nnn ;

ENTRY("THD_mask_erode") ;

   if( mmm == NULL ) EXRETURN ;

   nnn = (byte *)calloc(sizeof(byte),nxyz) ;  /* mask of eroded voxels */
   if( nnn == NULL ) EXRETURN ;               /* WTF? */

   /* mark interior voxels that don't have 17 out of 18 nonzero nbhrs */

   STATUS("marking to erode") ;
   for( kk=0 ; kk < nz ; kk++ ){
    kz = kk*nxy ; km = kz-nxy ; kp = kz+nxy ;
    if( kk == 0    ) km = kz ;
    if( kk == nz-1 ) kp = kz ;

    for( jj=0 ; jj < ny ; jj++ ){
     jy = jj*nx ; jm = jy-nx ; jp = jy+nx ;
     if( jj == 0    ) jm = jy ;
     if( jj == ny-1 ) jp = jy ;

     for( ii=0 ; ii < nx ; ii++ ){
       if( mmm[ii+jy+kz] ){           /* count nonzero nbhrs */
         im = ii-1 ; ip = ii+1 ;
         if( ii == 0    ) im = 0 ;
         if( ii == nx-1 ) ip = ii ;
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

   STATUS("eroding") ;
   for( jj=ii=0 ; ii < nxyz ; ii++ )            /* actually erode */
     if( nnn[ii] ){ mmm[ii] = 0 ; jj++ ; }

   if( verb && jj > 0 ) ININFO_message("Eroded   %d voxels\n",jj) ;

   /* optionally re-dilate eroded voxels that are next to survivors */
   if(redilate) {
      STATUS("marking to redilate") ;
      for( kk=0 ; kk < nz ; kk++ ){
        kz = kk*nxy ; km = kz-nxy ; kp = kz+nxy ;
        if( kk == 0    ) km = kz ;
        if( kk == nz-1 ) kp = kz ;

        for( jj=0 ; jj < ny ; jj++ ){
          jy = jj*nx ; jm = jy-nx ; jp = jy+nx ;
          if( jj == 0    ) jm = jy ;
          if( jj == ny-1 ) jp = jy ;

          for( ii=0 ; ii < nx ; ii++ ){
            if( nnn[ii+jy+kz] ){           /* was eroded */
              im = ii-1 ; ip = ii+1 ;
              if( ii == 0    ) im = 0 ;
              if( ii == nx-1 ) ip = ii ;
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
      }}} /* end of ii,jj,kk loops */

      /* actually do the dilation */

      STATUS("redilating") ;
      for( jj=ii=0 ; ii < nxyz ; ii++ )
        if( nnn[ii] ){ mmm[ii] = 1 ; jj++ ; }

      if( verb && jj > 0 ) ININFO_message("Restored %d eroded voxels\n",jj) ;

   } /* end of redilate */

   free(nnn) ; EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Generalization of THD_mask_erode(), to peel away multiple layers and
    then redilate.
----------------------------------------------------------------------------*/

void THD_mask_erodemany( int nx, int ny, int nz, byte *mmm, int npeel )
{
   int ii,jj,kk , jy,kz, im,jm,km , ip,jp,kp , num , pp ;
   int nxy=nx*ny , nxyz=nxy*nz ;
   byte *nnn,*qqq , bpp , bth ;

ENTRY("THD_mask_erodemany") ;

   if( mmm == NULL || npeel < 1 || nxyz < 27 ) EXRETURN ;

   nnn = (byte *)calloc(sizeof(byte),nxyz) ;  /* mask of eroded voxels */
   if( nnn == NULL ) EXRETURN ;               /* WTF? */
   qqq = (byte *)malloc(sizeof(byte)*nxyz) ;  /* another copy */
   if( qqq == NULL ){ free(nnn); EXRETURN; }  /* WTF? */

   /* mark interior voxels that don't have 'peelthr' out of 18 nonzero nbhrs */

   STATUS("peelings, nothing more than peelings") ;
   for( pp=1 ; pp <= npeel ; pp++ ){   /* pp = peel layer index */
     bpp = (byte)pp ;
     for( kk=0 ; kk < nz ; kk++ ){
      kz = kk*nxy ; km = kz-nxy ; kp = kz+nxy ;
      if( kk == 0    ) km = kz ;
      if( kk == nz-1 ) kp = kz ;

      for( jj=0 ; jj < ny ; jj++ ){
       jy = jj*nx ; jm = jy-nx ; jp = jy+nx ;
       if( jj == 0    ) jm = jy ;
       if( jj == ny-1 ) jp = jy ;

       for( ii=0 ; ii < nx ; ii++ ){
         if( mmm[ii+jy+kz] ){           /* count nonzero nbhrs */
           im = ii-1 ; ip = ii+1 ;
           if( ii == 0    ) im = 0 ;
           if( ii == nx-1 ) ip = ii ;
           num =  mmm[im+jy+km]
                + mmm[ii+jm+km] + mmm[ii+jy+km] + mmm[ii+jp+km]
                + mmm[ip+jy+km]
                + mmm[im+jm+kz] + mmm[im+jy+kz] + mmm[im+jp+kz]
                + mmm[ii+jm+kz]                 + mmm[ii+jp+kz]
                + mmm[ip+jm+kz] + mmm[ip+jy+kz] + mmm[ip+jp+kz]
                + mmm[im+jy+kp]
                + mmm[ii+jm+kp] + mmm[ii+jy+kp] + mmm[ii+jp+kp]
                + mmm[ip+jy+kp] ;
           if( num < peelthr ) nnn[ii+jy+kz] = bpp ;  /* mark to erode */
         }
     }}}
     for( ii=0 ; ii < nxyz ; ii++ )             /* actually erode */
       if( nnn[ii] ) mmm[ii] = 0 ;

   } /* end of loop over peeling layers */

   /* re-dilate eroded voxels that are next to survivors */

   STATUS("unpeelings") ;
   for( pp=npeel ; pp >= 1 ; pp-- ){  /* loop from innermost peel to outer */
     bpp = (byte)pp ; memset(qqq,0,sizeof(byte)*nxyz) ;
     bth = (pp==npeel) ? 0 : 1 ;
     for( kk=0 ; kk < nz ; kk++ ){
      kz = kk*nxy ; km = kz-nxy ; kp = kz+nxy ;
      if( kk == 0    ) km = kz ;
      if( kk == nz-1 ) kp = kz ;

      for( jj=0 ; jj < ny ; jj++ ){
       jy = jj*nx ; jm = jy-nx ; jp = jy+nx ;
       if( jj == 0    ) jm = jy ;
       if( jj == ny-1 ) jp = jy ;

       for( ii=0 ; ii < nx ; ii++ ){
         if( nnn[ii+jy+kz] >= bpp && !mmm[ii+jy+kz] ){  /* was eroded before */
           im = ii-1 ; ip = ii+1 ;
           if( ii == 0    ) im = 0 ;
           if( ii == nx-1 ) ip = ii ;
           qqq[ii+jy+kz] =              /* count any surviving nbhrs */
                  mmm[im+jy+km]
                + mmm[ii+jm+km] + mmm[ii+jy+km] + mmm[ii+jp+km]
                + mmm[ip+jy+km]
                + mmm[im+jm+kz] + mmm[im+jy+kz] + mmm[im+jp+kz]
                + mmm[ii+jm+kz]                 + mmm[ii+jp+kz]
                + mmm[ip+jm+kz] + mmm[ip+jy+kz] + mmm[ip+jp+kz]
                + mmm[im+jy+kp]
                + mmm[ii+jm+kp] + mmm[ii+jy+kp] + mmm[ii+jp+kp]
                + mmm[ip+jy+kp] ;
         }
     }}} /* end of ii,jj,kk loops */

     /* actually do the dilation */

     for( ii=0 ; ii < nxyz ; ii++ )
       if( qqq[ii] > bth ) mmm[ii] = 1 ;

   } /* end of redilate loop */

   free(qqq); free(nnn); EXRETURN;
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

   nnn = (byte*)calloc(sizeof(byte),nxyz) ;  /* mask of dilated voxels */

   /* mark exterior voxels neighboring enough interior voxels */

   for( kk=0 ; kk < nz ; kk++ ){
    kz = kk*nxy ; km = kz-nxy ; kp = kz+nxy ;
    if( kk == 0    ) km = kz ;
    if( kk == nz-1 ) kp = kz ;

    for( jj=0 ; jj < ny ; jj++ ){
     jy = jj*nx ; jm = jy-nx ; jp = jy+nx ;
     if( jj == 0    ) jm = jy ;
     if( jj == ny-1 ) jp = jy ;

     for( ii=0 ; ii < nx ; ii++ ){
       if( mmm[ii+jy+kz] == 0 ){           /* count nonzero nbhrs */
         im = ii-1 ; ip = ii+1 ;
         if( ii == 0    ) im = 0 ;
         if( ii == nx-1 ) ip = ii ;
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

/* clip in autobox by default but allow turning it off */
/* thanks Judd */
static int bbox_clip=1 ;
void THD_autobbox_clip( int c ){ bbox_clip = c; }

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
   if( bbox_clip ){
      clip_val = THD_cliplevel(medim,clfrac) ;
      for( ii=0 ; ii < nvox ; ii++ )
	if( mar[ii] < clip_val ) mar[ii] = 0.0 ;
   }

   MRI_autobbox( medim , xm,xp , ym,yp , zm,zp ) ;

   mri_free(medim) ; EXRETURN ;
}

/*------------------------------------------------------------------------*/

static int bbox_clust=1 ;
void MRI_autobbox_clust( int c ){ bbox_clust = c; }

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

   if( bbox_clust ){
     THD_mask_clust( nx,ny,nz, mmm ) ;
     THD_mask_erodemany( nx,ny,nz, mmm, peelcount ) ;
     THD_mask_clust( nx,ny,nz, mmm ) ;
   }

   /* For each plane direction,
      find the first and last index that have nonzero voxels in that plane */

   if( xm != NULL ){
     for( ii=0 ; ii < nx ; ii++ )
      for( kk=0 ; kk < nz ; kk++ )
       for( jj=0 ; jj < ny ; jj++ )
        if( mmm[ii+jj*nx+kk*nxy] ) goto CP5 ;
     CP5: ASSIF(xm,ii) ;
   }

   if( xp != NULL ){
     for( ii=nx-1 ; ii >= 0 ; ii-- )
      for( kk=0 ; kk < nz ; kk++ )
       for( jj=0 ; jj < ny ; jj++ )
        if( mmm[ii+jj*nx+kk*nxy] ) goto CP6 ;
     CP6: ASSIF(xp,ii) ;
   }

   if( ym != NULL ){
     for( jj=0 ; jj < ny ; jj++ )
      for( kk=0 ; kk < nz ; kk++ )
       for( ii=0 ; ii < nx ; ii++ )
        if( mmm[ii+jj*nx+kk*nxy] ) goto CP3 ;
     CP3: ASSIF(ym,jj) ;
   }

   if( yp != NULL ){
     for( jj=ny-1 ; jj >= 0 ; jj-- )
      for( kk=0 ; kk < nz ; kk++ )
       for( ii=0 ; ii < nx ; ii++ )
        if( mmm[ii+jj*nx+kk*nxy] ) goto CP4 ;
     CP4: ASSIF(yp,jj) ;
   }

   if( zm != NULL ){
     for( kk=0 ; kk < nz ; kk++ )
      for( jj=0 ; jj < ny ; jj++ )
       for( ii=0 ; ii < nx ; ii++ )
        if( mmm[ii+jj*nx+kk*nxy] ) goto CP1 ;
     CP1: ASSIF(zm,kk) ;
   }

   if( zp != NULL ){
     for( kk=nz-1 ; kk >= 0 ; kk-- )
      for( jj=0 ; jj < ny ; jj++ )
       for( ii=0 ; ii < nx ; ii++ )
        if( mmm[ii+jj*nx+kk*nxy] ) goto CP2 ;
     CP2: ASSIF(zp,kk) ;
   }

   free(mmm) ; EXRETURN ;
}

/*------------------------------------------------------------------------*/

int THD_peel_mask( int nx, int ny, int nz , byte *mmm, int pdepth )
{
   int nxy=nx*ny , ii,jj,kk , ijk , bot,top , pd=pdepth ;
   int nxp=nx-pd , nyp=ny-pd , nzp=nz-pd ;
   int num=0 , dnum , nite ;

   for( nite=0 ; nite < pd ; nite++ ){
    dnum = 0 ;

    for( kk=0 ; kk < nz ; kk++ ){
     for( jj=0 ; jj < ny ; jj++ ){
       ijk = jj*nx + kk*nxy ;
       for( bot=0 ; bot < nx && !mmm[bot+ijk]; bot++ ) ;
       top = bot+pd ; if( top >= nx ) continue ;
       for( ii=bot+1 ; ii <= top && mmm[ii+ijk] ; ii++ ) ;
       if( ii <= top ){ mmm[bot+ijk] = 0; dnum++; }
    }}

    for( kk=0 ; kk < nz ; kk++ ){
     for( jj=0 ; jj < ny ; jj++ ){
       ijk = jj*nx + kk*nxy ;
       for( top=nx-1 ; top >= 0 && !mmm[top+ijk]; top-- ) ;
       bot = top-pd ; if( bot < 0 ) continue ;
       for( ii=top-1 ; ii >= bot && mmm[ii+ijk] ; ii-- ) ;
       if( ii >= bot ){ mmm[top+ijk] = 0; dnum++; }
    }}

    for( kk=0 ; kk < nz ; kk++ ){
     for( ii=0 ; ii < nx ; ii++ ){
       ijk = ii + kk*nxy ;
       for( bot=0 ; bot < ny && !mmm[bot*nx+ijk] ; bot++ ) ;
       top = bot+pd ;
       if( top >= ny ) continue ;
       for( jj=bot+1 ; jj <= top && mmm[jj*nx+ijk] ; jj++ ) ;
       if( jj <= top ){ mmm[bot*nx+ijk] = 0; dnum++; }
    }}

    for( kk=0 ; kk < nz ; kk++ ){
     for( ii=0 ; ii < nx ; ii++ ){
       ijk = ii + kk*nxy ;
       for( top=ny-1 ; top >= 0 && !mmm[top*nx+ijk] ; top-- ) ;
       bot = top-pd ; if( bot < 0 ) continue ;
       for( jj=top-1 ; jj >= bot && mmm[jj*nx+ijk] ; jj-- ) ;
       if( jj >= bot ){ mmm[top*nx+ijk] = 0; dnum++; }
    }}

    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++ ){
       ijk = ii + jj*nx ;
       for( top=nz-1 ; top >= 0 && !mmm[top*nxy+ijk] ; top-- ) ;
       bot = top-pd ; if( bot < 0 ) continue ;
       for( kk=top-1 ; kk >= bot && mmm[kk*nxy+ijk] ; kk-- ) ;
       if( kk >= bot ){ mmm[top*nxy+ijk] = 0; dnum++; }
    }}

    num += dnum ;
    if( dnum == 0 ) break ;
   }

   return num ;
}
