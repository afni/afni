#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/*** NOT 7D SAFE ***/

/****************************************************************************/
/** functions to "warp" 3D images -- not very efficient, but quite general **/
/****************************************************************************/

/*--------------------------------------------------------------------------*/

static int wtype = MRI_LINEAR ;
void mri_warp3D_method( int mode ){ wtype = mode ; } /** set interpolation **/

/*--------------------------------------------------------------------------*/

static byte *womask = NULL ;  /* 19 Nov 2004 */

void mri_warp3D_set_womask( MRI_IMAGE *wim )
{
   womask = (wim == NULL || wim->kind != MRI_byte) ? NULL
                                                   : MRI_BYTE_PTR(wim) ;
}

#undef  SKIP
#define SKIP(i,j,k) (womask!=NULL && womask[(i)+(j)*nxnew+(k)*nxynew]==0)

/*--------------------------------------------------------------------------*/

static int zout = 1 ;
void mri_warp3D_zerout( int zzzz ){ zout  = zzzz ; }      /** outside = 0? **/

/*--------------------------------------------------------------------------*/
/* Macros for interpolation and access to data arrays. */

#undef  FAR
#define FAR(i,j,k) far[(i)+(j)*nx+(k)*nxy]         /* input point */
#undef  NAR
#define NAR(i,j,k) nar[(i)+(j)*nxnew+(k)*nxynew]   /* output point */

/** clip input mm to range [0..nn] (for indexing) **/

#undef  CLIP
#define CLIP(mm,nn) if(mm < 0)mm=0; else if(mm > nn)mm=nn

/* define Lagrange cubic interpolation polynomials */

#undef  P_M1
#undef  P_00
#undef  P_P1
#undef  P_P2
#undef  P_FACTOR
#define P_M1(x)  (-(x)*((x)-1)*((x)-2))
#define P_00(x)  (3*((x)+1)*((x)-1)*((x)-2))
#define P_P1(x)  (-3*(x)*((x)+1)*((x)-2))
#define P_P2(x)  ((x)*((x)+1)*((x)-1))
#define P_FACTOR 4.62962963e-3            /* 1/216 = final scaling factor */

/*---------------------------------------------------------------------------*/
/*! Transform a 3D image geometrically, using cubic interpolation.
    See comments for mri_warp3D() for details about the parameters.
-----------------------------------------------------------------------------*/

MRI_IMAGE *mri_warp3D_cubic(
                    MRI_IMAGE *im, int nxnew, int nynew, int nznew,
                    void wf(float,float,float,float *,float *,float *) )
{
   MRI_IMAGE *imfl , *newImg ;
   float *ffar ;
   int nx,ny,nz,nxy, nxynew , nxyznew , pp ;
   float bot,top ;

ENTRY("mri_warp3D_cubic") ;

   /*--- sanity check inputs ---*/

   if( im == NULL || wf == NULL ) RETURN(NULL) ;

   /*-- dimensional analysis --*/

   nx = im->nx ;          /* input image dimensions */
   ny = im->ny ;
   nz = im->nz ; nxy = nx*ny ;

   nxnew = (nxnew > 0) ? nxnew : nx ;  /* output image dimensions */
   nynew = (nynew > 0) ? nynew : ny ;
   nznew = (nznew > 0) ? nznew : nz ;
   nxynew = nxnew*nynew ; nxyznew = nxynew * nznew ;

   /*----- Allow for different input image types, by breaking them into
           components, doing each one separately, and then reassembling.
           This is needed since we only warp float-valued images below.  -----*/

   switch( im->kind ){

     case MRI_float:                              /* use input directly */
       imfl = im ; break ;

     default:{                                    /* floatize input */
       imfl = mri_to_float(im) ;
       newImg  = mri_warp3D_cubic( imfl , nxnew,nynew,nznew , wf ) ;
       mri_free(imfl) ;
       imfl = mri_to_mri(im->kind,newImg) ;
       if( imfl != NULL ){ mri_free(newImg); newImg = imfl; }
       RETURN(newImg) ;
     }

     case MRI_rgb:{                                /* break into 3 pieces */
       MRI_IMARR *imar = mri_rgb_to_3float(im) ;
       MRI_IMAGE *rim,*gim,*bim ;
       rim = mri_warp3D_cubic( IMARR_SUBIM(imar,0), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,0) ) ;
       gim = mri_warp3D_cubic( IMARR_SUBIM(imar,1), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,1) ) ;
       bim = mri_warp3D_cubic( IMARR_SUBIM(imar,2), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,2) ) ;
       FREE_IMARR(imar) ;
       newImg = mri_3to_rgb( rim,gim,bim ) ;
       mri_free(rim); mri_free(gim); mri_free(bim); RETURN(newImg);
     }

     case MRI_complex:{                             /* break into 2 pieces */
       MRI_IMARR *imar = mri_complex_to_pair(im) ;
       MRI_IMAGE *rim, *iim ;
       rim = mri_warp3D_cubic( IMARR_SUBIM(imar,0), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,0) ) ;
       iim = mri_warp3D_cubic( IMARR_SUBIM(imar,1), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,1) ) ;
       FREE_IMARR(imar) ;
       newImg = mri_pair_to_complex( rim , iim ) ;
       mri_free(rim); mri_free(iim); RETURN(newImg);
     }

   } /* end of special cases of input datum */

   /*----- at this point, imfl is in float format -----*/

   ffar = MRI_FLOAT_PTR( imfl ) ;                         /* input image data */

   newImg = mri_new_vol( nxnew,nynew,nznew, MRI_float ) ;  /* make output image */

   bot = top = ffar[0] ;                             /* find input data range */
   for( pp=1 ; pp < imfl->nvox ; pp++ ){
          if( ffar[pp] > top ) top = ffar[pp] ;
     else if( ffar[pp] < bot ) bot = ffar[pp] ;
   }

   /*** loop over output points and warp to them ***/

#pragma omp parallel if( nxyznew > 99999 )
 {
   float *far , *nar ;
   float nxh,nyh,nzh , val ;
   int ix_m1,ix_00,ix_p1,ix_p2 ;         /* interpolation indices */
   int jy_m1,jy_00,jy_p1,jy_p2 ;         /* (input image) */
   int kz_m1,kz_00,kz_p1,kz_p2 ;
   float wt_m1,wt_00,wt_p1,wt_p2 ;   /* interpolation weights */
   float f_jm1_km1, f_j00_km1, f_jp1_km1, f_jp2_km1, /* interpolants */
         f_jm1_k00, f_j00_k00, f_jp1_k00, f_jp2_k00,
         f_jm1_kp1, f_j00_kp1, f_jp1_kp1, f_jp2_kp1,
         f_jm1_kp2, f_j00_kp2, f_jp1_kp2, f_jp2_kp2,
         f_km1    , f_k00    , f_kp1    , f_kp2     ;
   int nx1,ny1,nz1 , ii,jj,kk , ix,jy,kz , qq , do_zout=zout ;
   float xpr,ypr,zpr, xx,yy,zz, fx,fy,fz ;

   far = ffar ;                                          /* input image data */
   nar = MRI_FLOAT_PTR( newImg ) ;                       /* output image data */

   nxh = nx-0.5 ; nyh = ny-0.5 ; nzh = nz-0.5 ;

#pragma omp for
   for( qq=0 ; qq < nxyznew ; qq++ ){  /* voxel loop */
     xpr = ii = qq % nxnew ;           /* break 1D index qq into 3D index (ii,jj,kk) */
     zpr = kk = qq / nxynew ;
     ypr = jj = (qq-kk*nxynew) / nx ;
     if( SKIP(ii,jj,kk) ) continue ;    /* 19 Nov 2004 */
     wf( xpr,ypr,zpr , &xx,&yy,&zz ) ;  /* get xx,yy,zz in original image */

     if( do_zout &&
         (xx < -0.5 || xx > nxh ||
          yy < -0.5 || yy > nyh || zz < -0.5 || zz > nzh ) ){ nar[qq] = 0.0f; continue; }

     ix = floor(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floor(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floor(zz) ;  fz = zz - kz ;

     ix_m1 = ix-1    ; ix_00 = ix      ; ix_p1 = ix+1    ; ix_p2 = ix+2    ;
     CLIP(ix_m1,nx1) ; CLIP(ix_00,nx1) ; CLIP(ix_p1,nx1) ; CLIP(ix_p2,nx1) ;

     jy_m1 = jy-1    ; jy_00 = jy      ; jy_p1 = jy+1    ; jy_p2 = jy+2    ;
     CLIP(jy_m1,ny1) ; CLIP(jy_00,ny1) ; CLIP(jy_p1,ny1) ; CLIP(jy_p2,ny1) ;

     kz_m1 = kz-1    ; kz_00 = kz      ; kz_p1 = kz+1    ; kz_p2 = kz+2    ;
     CLIP(kz_m1,nz1) ; CLIP(kz_00,nz1) ; CLIP(kz_p1,nz1) ; CLIP(kz_p2,nz1) ;

     wt_m1 = P_M1(fx) ; wt_00 = P_00(fx) ;  /* interpolation weights */
     wt_p1 = P_P1(fx) ; wt_p2 = P_P2(fx) ;

#undef  XINT
#define XINT(j,k) wt_m1*FAR(ix_m1,j,k)+wt_00*FAR(ix_00,j,k)  \
                 +wt_p1*FAR(ix_p1,j,k)+wt_p2*FAR(ix_p2,j,k)

     /* interpolate to location ix+fx at each jy,kz level */

     f_jm1_km1 = XINT(jy_m1,kz_m1) ; f_j00_km1 = XINT(jy_00,kz_m1) ;
     f_jp1_km1 = XINT(jy_p1,kz_m1) ; f_jp2_km1 = XINT(jy_p2,kz_m1) ;
     f_jm1_k00 = XINT(jy_m1,kz_00) ; f_j00_k00 = XINT(jy_00,kz_00) ;
     f_jp1_k00 = XINT(jy_p1,kz_00) ; f_jp2_k00 = XINT(jy_p2,kz_00) ;
     f_jm1_kp1 = XINT(jy_m1,kz_p1) ; f_j00_kp1 = XINT(jy_00,kz_p1) ;
     f_jp1_kp1 = XINT(jy_p1,kz_p1) ; f_jp2_kp1 = XINT(jy_p2,kz_p1) ;
     f_jm1_kp2 = XINT(jy_m1,kz_p2) ; f_j00_kp2 = XINT(jy_00,kz_p2) ;
     f_jp1_kp2 = XINT(jy_p1,kz_p2) ; f_jp2_kp2 = XINT(jy_p2,kz_p2) ;

     /* interpolate to jy+fy at each kz level */

     wt_m1 = P_M1(fy) ; wt_00 = P_00(fy) ;
     wt_p1 = P_P1(fy) ; wt_p2 = P_P2(fy) ;

     f_km1 =  wt_m1 * f_jm1_km1 + wt_00 * f_j00_km1
            + wt_p1 * f_jp1_km1 + wt_p2 * f_jp2_km1 ;

     f_k00 =  wt_m1 * f_jm1_k00 + wt_00 * f_j00_k00
            + wt_p1 * f_jp1_k00 + wt_p2 * f_jp2_k00 ;

     f_kp1 =  wt_m1 * f_jm1_kp1 + wt_00 * f_j00_kp1
            + wt_p1 * f_jp1_kp1 + wt_p2 * f_jp2_kp1 ;

     f_kp2 =  wt_m1 * f_jm1_kp2 + wt_00 * f_j00_kp2
            + wt_p1 * f_jp1_kp2 + wt_p2 * f_jp2_kp2 ;

     /* interpolate to kz+fz to get output */

     wt_m1 = P_M1(fz) ; wt_00 = P_00(fz) ;
     wt_p1 = P_P1(fz) ; wt_p2 = P_P2(fz) ;

     val = P_FACTOR * (  wt_m1 * f_km1 + wt_00 * f_k00
                       + wt_p1 * f_kp1 + wt_p2 * f_kp2 ) ;

          if( val > top ) val = top ;   /* clip to input data range */
     else if( val < bot ) val = bot ;

     nar[qq] = val ;
   }
 } /* end OpenMP */

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   RETURN(newImg);
}

/*---------------------------------------------------------------------------*/
/*! Transform a 3D image geometrically, using linear interpolation.
    See comments for mri_warp3D() for details about the parameters.
-----------------------------------------------------------------------------*/

MRI_IMAGE *mri_warp3D_linear(
                    MRI_IMAGE *im, int nxnew, int nynew, int nznew,
                    void wf(float,float,float,float *,float *,float *) )
{
   MRI_IMAGE *imfl , *newImg ;
   int nx,ny,nz,nxy , nxynew,nxyznew ;

#if 0
int nzset ; float zzsum ;
#endif

ENTRY("mri_warp3D_linear") ;

   /*--- sanity check inputs ---*/

   if( im == NULL || wf == NULL ) RETURN(NULL) ;

   /*-- dimensional analysis --*/

   nx = im->nx ;          /* input image dimensions */
   ny = im->ny ;
   nz = im->nz ; nxy = nx*ny ;

#if 0
fprintf(stderr,"mri_warp3D_linear: nx=%d ny=%d nz=%d\n",nx,ny,nz) ;
nzset = 0 ; zzsum = 0.0 ;
#endif

   nxnew = (nxnew > 0) ? nxnew : nx ;  /* output image dimensions */
   nynew = (nynew > 0) ? nynew : ny ;
   nznew = (nznew > 0) ? nznew : nz ;
   nxynew = nxnew*nynew ; nxyznew = nxynew*nznew ;

   /*----- allow for different input image types, by breaking them into
           components, doing each one separately, and then reassembling -----*/

   switch( im->kind ){

     case MRI_float:                        /* use input directly */
       imfl = im ; break ;

     default:{                              /* floatize-input */
       imfl = mri_to_float(im) ;
       newImg  = mri_warp3D_linear( imfl , nxnew,nynew,nznew , wf ) ;
       mri_free(imfl) ;
       imfl = mri_to_mri(im->kind,newImg) ;
       if( imfl != NULL ){ mri_free(newImg); newImg = imfl; }
       RETURN(newImg) ;
     }

     case MRI_rgb:{
       MRI_IMARR *imar = mri_rgb_to_3float(im) ;
       MRI_IMAGE *rim,*gim,*bim ;
       rim = mri_warp3D_linear( IMARR_SUBIM(imar,0), nxnew,nynew,nznew, wf ) ;
                      mri_free( IMARR_SUBIM(imar,0) ) ;
       gim = mri_warp3D_linear( IMARR_SUBIM(imar,1), nxnew,nynew,nznew, wf ) ;
                      mri_free( IMARR_SUBIM(imar,1) ) ;
       bim = mri_warp3D_linear( IMARR_SUBIM(imar,2), nxnew,nynew,nznew, wf ) ;
                      mri_free( IMARR_SUBIM(imar,2) ) ;
       FREE_IMARR(imar) ;
       newImg = mri_3to_rgb( rim,gim,bim ) ;
       mri_free(rim); mri_free(gim); mri_free(bim); RETURN(newImg);
     }

     case MRI_complex:{
       MRI_IMARR *imar = mri_complex_to_pair(im) ;
       MRI_IMAGE *rim, *iim ;
       rim = mri_warp3D_linear( IMARR_SUBIM(imar,0), nxnew,nynew,nznew, wf ) ;
                      mri_free( IMARR_SUBIM(imar,0) ) ;
       iim = mri_warp3D_linear( IMARR_SUBIM(imar,1), nxnew,nynew,nznew, wf ) ;
                      mri_free( IMARR_SUBIM(imar,1) ) ;
       FREE_IMARR(imar) ;
       newImg = mri_pair_to_complex( rim , iim ) ;
       mri_free(rim); mri_free(iim); RETURN(newImg);
     }

   } /* end of special cases of input datum */

   /*----- at this point, imfl is in float format -----*/

   newImg = mri_new_vol( nxnew,nynew,nznew, MRI_float ) ;  /* make output image */

#pragma omp parallel if( nxyznew > 99999 )
 {
   float *far , *nar ;
   float nxh,nyh,nzh ;
   float xpr,ypr,zpr, xx,yy,zz, fx,fy,fz ;
   int ii,jj,kk , qq , ix,jy,kz , nx1,ny1,nz1 ;
   int ix_00,ix_p1 ;         /* interpolation indices */
   int jy_00,jy_p1 ;         /* (input image) */
   int kz_00,kz_p1 ;
   float wt_00,wt_p1 ;       /* interpolation weights */
   float f_j00_k00, f_jp1_k00, f_j00_kp1, f_jp1_kp1, f_k00, f_kp1 ;
   int do_zout = zout ;

   far = MRI_FLOAT_PTR( imfl ) ;                         /* input image data */
   nar = MRI_FLOAT_PTR( newImg ) ;                       /* output image data */

   /*** loop over output points and warp to them ***/

   nxh = nx-0.5 ; nyh = ny-0.5 ; nzh = nz-0.5 ;
   nx1 = nx-1   ; ny1 = ny-1   ; nz1 = nz-1   ;

#pragma omp for
   for( qq=0 ; qq < nxyznew ; qq++ ){  /* voxel loop */
     xpr = ii = qq % nxnew ;           /* break 1D index qq into 3D index (ii,jj,kk) */
     zpr = kk = qq / nxynew ;
     ypr = jj = (qq-kk*nxynew) / nx ;
     if( SKIP(ii,jj,kk) ) continue ;    /* 19 Nov 2004 */
     wf( xpr,ypr,zpr , &xx,&yy,&zz ) ;  /* get xx,yy,zz in original image */

     if( do_zout &&
         (xx < -0.5 || xx > nxh ||
          yy < -0.5 || yy > nyh || zz < -0.5 || zz > nzh ) ){ nar[qq] = 0.0f ; continue ; }

     ix = floor(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floor(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floor(zz) ;  fz = zz - kz ;   /* of warped point   */

     ix_00 = ix      ; ix_p1 = ix+1    ;
     CLIP(ix_00,nx1) ; CLIP(ix_p1,nx1) ;

     jy_00 = jy      ; jy_p1 = jy+1    ;
     CLIP(jy_00,ny1) ; CLIP(jy_p1,ny1) ;

     kz_00 = kz      ; kz_p1 = kz+1    ;
     CLIP(kz_00,nz1) ; CLIP(kz_p1,nz1) ;

     wt_00 = 1.0-fx ; wt_p1 = fx ;

#undef  XINT
#define XINT(j,k) wt_00*FAR(ix_00,j,k)+wt_p1*FAR(ix_p1,j,k)

     /* interpolate to location ix+fx at each jy,kz level */

     f_j00_k00 = XINT(jy_00,kz_00) ; f_jp1_k00 = XINT(jy_p1,kz_00) ;
     f_j00_kp1 = XINT(jy_00,kz_p1) ; f_jp1_kp1 = XINT(jy_p1,kz_p1) ;

     /* interpolate to jy+fy at each kz level */

     wt_00 = 1.0-fy ; wt_p1 = fy ;

     f_k00 =  wt_00 * f_j00_k00 + wt_p1 * f_jp1_k00 ;

     f_kp1 =  wt_00 * f_j00_kp1 + wt_p1 * f_jp1_kp1 ;

     /* interpolate to kz+fz to get output */

     nar[qq] = (1.0-fz) * f_k00 + fz * f_kp1 ;

   } /* end of voxel loop */

 } /* end OpenMP */

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   RETURN(newImg);
}

/*---------------------------------------------------------------------------*/
/*! Transform a 3D image geometrically, using NN 'interpolation'.
    See comments for mri_warp3D() for details about the parameters.
-----------------------------------------------------------------------------*/

MRI_IMAGE *mri_warp3D_NN(
             MRI_IMAGE *im, int nxnew, int nynew, int nznew,
             void wf(float,float,float,float *,float *,float *) )
{
   MRI_IMAGE *imfl , *newImg ;
   float *far , *nar ;
   float xpr,ypr,zpr, xx,yy,zz, fx,fy,fz ;
   int ii,jj,kk, nx,ny,nz,nxy, nx1,ny1,nz1, ix,jy,kz, nxynew ;
   float nxh,nyh,nzh ;

ENTRY("mri_warp3D_NN") ;

   /*--- sanity check inputs ---*/

   if( im == NULL || wf == NULL ) RETURN(NULL) ;

   /*-- dimensional analysis --*/

   nx = im->nx ; nx1 = nx-1 ;          /* input image dimensions */
   ny = im->ny ; ny1 = ny-1 ;
   nz = im->nz ; nz1 = nz-1 ; nxy = nx*ny ;

   nxnew = (nxnew > 0) ? nxnew : nx ;  /* output image dimensions */
   nynew = (nynew > 0) ? nynew : ny ;
   nznew = (nznew > 0) ? nznew : nz ;
   nxynew = nxnew*nynew ;

   /*----- allow for different input image types, by breaking them into
           components, doing each one separately, and then reassembling -----*/

   switch( im->kind ){

     case MRI_float:                        /* use input directly */
       imfl = im ; break ;

     default:{                              /* floatize-input */
       imfl = mri_to_float(im) ;
       newImg  = mri_warp3D_NN( imfl , nxnew,nynew,nznew , wf ) ;
       mri_free(imfl) ;
       imfl = mri_to_mri(im->kind,newImg) ;
       if( imfl != NULL ){ mri_free(newImg); newImg = imfl; }
       RETURN(newImg) ;
     }

     case MRI_rgb:{
       MRI_IMARR *imar = mri_rgb_to_3float(im) ;
       MRI_IMAGE *rim,*gim,*bim ;
       rim = mri_warp3D_NN( IMARR_SUBIM(imar,0), nxnew,nynew,nznew, wf ) ;
                  mri_free( IMARR_SUBIM(imar,0) ) ;
       gim = mri_warp3D_NN( IMARR_SUBIM(imar,1), nxnew,nynew,nznew, wf ) ;
                  mri_free( IMARR_SUBIM(imar,1) ) ;
       bim = mri_warp3D_NN( IMARR_SUBIM(imar,2), nxnew,nynew,nznew, wf ) ;
                  mri_free( IMARR_SUBIM(imar,2) ) ;
       FREE_IMARR(imar) ;
       newImg = mri_3to_rgb( rim,gim,bim ) ;
       mri_free(rim); mri_free(gim); mri_free(bim); RETURN(newImg);
     }

     case MRI_complex:{
       MRI_IMARR *imar = mri_complex_to_pair(im) ;
       MRI_IMAGE *rim, *iim ;
       rim = mri_warp3D_NN( IMARR_SUBIM(imar,0), nxnew,nynew,nznew, wf ) ;
                  mri_free( IMARR_SUBIM(imar,0) ) ;
       iim = mri_warp3D_NN( IMARR_SUBIM(imar,1), nxnew,nynew,nznew, wf ) ;
                  mri_free( IMARR_SUBIM(imar,1) ) ;
       FREE_IMARR(imar) ;
       newImg = mri_pair_to_complex( rim , iim ) ;
       mri_free(rim); mri_free(iim); RETURN(newImg);
     }

   } /* end of special cases of input datum */

   /*----- at this point, imfl is in float format -----*/

   far = MRI_FLOAT_PTR( imfl ) ;                         /* input image data */

   newImg = mri_new_vol( nxnew,nynew,nznew, MRI_float ) ;  /* make output image */
   nar = MRI_FLOAT_PTR( newImg ) ;                         /* output image data */

   /*** loop over output points and warp to them ***/

   nxh = nx-0.5 ; nyh = ny-0.5 ; nzh = nz-0.5 ;
   for( kk=0 ; kk < nznew ; kk++ ){
    zpr = kk ;
    for( jj=0 ; jj < nynew ; jj++ ){
     ypr = jj ;
     for( ii=0 ; ii < nxnew ; ii++ ){
       xpr = ii ;
       if( SKIP(ii,jj,kk) ) continue ;    /* 19 Nov 2004 */
       wf( xpr,ypr,zpr , &xx,&yy,&zz ) ;  /* get xx,yy,zz in original image */

       if( zout &&
           (xx < -0.5 || xx > nxh ||
            yy < -0.5 || yy > nyh || zz < -0.5 || zz > nzh ) ){

          NAR(ii,jj,kk) = 0.0 ; continue ;
       }

       ix = (int)rint(xx) ; jy = (int)rint(yy) ; kz = (int)rint(zz) ;
       CLIP(ix,nx1)  ; CLIP(jy,ny1)  ; CLIP(kz,nz1)  ;

       NAR(ii,jj,kk) = FAR(ix,jy,kz) ;
     }
    }
   }

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   RETURN(newImg);
}

/* define quintic interpolation polynomials (Lagrange) */

#undef  Q_M2
#undef  Q_M1
#undef  Q_00
#undef  Q_P1
#undef  Q_P2
#undef  Q_P3
#define Q_M2(x)  (x*(x*x-1.0)*(2.0-x)*(x-3.0)*0.008333333)
#define Q_M1(x)  (x*(x*x-4.0)*(x-1.0)*(x-3.0)*0.041666667)
#define Q_00(x)  ((x*x-4.0)*(x*x-1.0)*(3.0-x)*0.083333333)
#define Q_P1(x)  (x*(x*x-4.0)*(x+1.0)*(x-3.0)*0.083333333)
#define Q_P2(x)  (x*(x*x-1.0)*(x+2.0)*(3.0-x)*0.041666667)
#define Q_P3(x)  (x*(x*x-1.0)*(x*x-4.0)*0.008333333)

/*---------------------------------------------------------------------------*/
/*! Transform a 3D image geometrically, using quintic interpolation.
    See comments for mri_warp3D() for details about the parameters.
     - RWCox - 06 Aug 2003
-----------------------------------------------------------------------------*/

MRI_IMAGE *mri_warp3D_quintic(
                    MRI_IMAGE *im, int nxnew, int nynew, int nznew,
                    void wf(float,float,float,float *,float *,float *) )
{
   MRI_IMAGE *imfl , *newImg ;
   float *far , *nar ;
   float xpr,ypr,zpr, xx,yy,zz, fx,fy,fz ;
   int ii,jj,kk, nx,ny,nz,nxy, nx1,ny1,nz1, ix,jy,kz, nxynew ;
   float bot,top,val ;
   float nxh,nyh,nzh ;

   int ix_m2,ix_m1,ix_00,ix_p1,ix_p2,ix_p3 ; /* interpolation indices */
   int jy_m2,jy_m1,jy_00,jy_p1,jy_p2,jy_p3 ; /* (input image) */
   int kz_m2,kz_m1,kz_00,kz_p1,kz_p2,kz_p3 ;

   float wt_m2,wt_m1,wt_00,wt_p1,wt_p2,wt_p3 ; /* interpolation weights */

   float f_jm2_km2, f_jm1_km2, f_j00_km2, f_jp1_km2, f_jp2_km2, f_jp3_km2,
         f_jm2_km1, f_jm1_km1, f_j00_km1, f_jp1_km1, f_jp2_km1, f_jp3_km1,
         f_jm2_k00, f_jm1_k00, f_j00_k00, f_jp1_k00, f_jp2_k00, f_jp3_k00,
         f_jm2_kp1, f_jm1_kp1, f_j00_kp1, f_jp1_kp1, f_jp2_kp1, f_jp3_kp1,
         f_jm2_kp2, f_jm1_kp2, f_j00_kp2, f_jp1_kp2, f_jp2_kp2, f_jp3_kp2,
         f_jm2_kp3, f_jm1_kp3, f_j00_kp3, f_jp1_kp3, f_jp2_kp3, f_jp3_kp3,
         f_km2    , f_km1    , f_k00    , f_kp1    , f_kp2    , f_kp3     ;

ENTRY("mri_warp3D_quintic") ;

   /*--- sanity check inputs ---*/

   if( im == NULL || wf == NULL ) RETURN(NULL) ;

   /*-- dimensional analysis --*/

   nx = im->nx ; nx1 = nx-1 ;          /* input image dimensions */
   ny = im->ny ; ny1 = ny-1 ;
   nz = im->nz ; nz1 = nz-1 ; nxy = nx*ny ;

   nxnew = (nxnew > 0) ? nxnew : nx ;  /* output image dimensions */
   nynew = (nynew > 0) ? nynew : ny ;
   nznew = (nznew > 0) ? nznew : nz ;
   nxynew = nxnew*nynew ;

   /*----- Allow for different input image types, by breaking them into
           components, doing each one separately, and then reassembling.
           This is needed since we only warp float-valued images below.  -----*/

   switch( im->kind ){

     case MRI_float:                              /* use input directly */
       imfl = im ; break ;

     default:{                                    /* floatize input */
       imfl = mri_to_float(im) ;
       newImg  = mri_warp3D_quintic( imfl , nxnew,nynew,nznew , wf ) ;
       mri_free(imfl) ;
       imfl = mri_to_mri(im->kind,newImg) ;
       if( imfl != NULL ){ mri_free(newImg); newImg = imfl; }
       RETURN(newImg) ;
     }

     case MRI_rgb:{                                /* break into 3 pieces */
       MRI_IMARR *imar = mri_rgb_to_3float(im) ;
       MRI_IMAGE *rim,*gim,*bim ;
       rim = mri_warp3D_quintic( IMARR_SUBIM(imar,0), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,0) ) ;
       gim = mri_warp3D_quintic( IMARR_SUBIM(imar,1), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,1) ) ;
       bim = mri_warp3D_quintic( IMARR_SUBIM(imar,2), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,2) ) ;
       FREE_IMARR(imar) ;
       newImg = mri_3to_rgb( rim,gim,bim ) ;
       mri_free(rim); mri_free(gim); mri_free(bim); RETURN(newImg);
     }

     case MRI_complex:{                             /* break into 2 pieces */
       MRI_IMARR *imar = mri_complex_to_pair(im) ;
       MRI_IMAGE *rim, *iim ;
       rim = mri_warp3D_quintic( IMARR_SUBIM(imar,0), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,0) ) ;
       iim = mri_warp3D_quintic( IMARR_SUBIM(imar,1), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,1) ) ;
       FREE_IMARR(imar) ;
       newImg = mri_pair_to_complex( rim , iim ) ;
       mri_free(rim); mri_free(iim); RETURN(newImg);
     }

   } /* end of special cases of input datum */

   /*----- at this point, imfl is in float format -----*/

   far = MRI_FLOAT_PTR( imfl ) ;                         /* input image data */

   newImg = mri_new_vol( nxnew,nynew,nznew, MRI_float ) ;  /* make output image */
   nar = MRI_FLOAT_PTR( newImg ) ;                         /* output image data */

   bot = top = far[0] ;                             /* find input data range */
   for( ii=1 ; ii < imfl->nvox ; ii++ ){
          if( far[ii] > top ) top = far[ii] ;
     else if( far[ii] < bot ) bot = far[ii] ;
   }

   /*** loop over output points and warp to them ***/

   nxh = nx-0.5 ; nyh = ny-0.5 ; nzh = nz-0.5 ;
   for( kk=0 ; kk < nznew ; kk++ ){
    zpr = kk ;
    for( jj=0 ; jj < nynew ; jj++ ){
     ypr = jj ;
     for( ii=0 ; ii < nxnew ; ii++ ){
       xpr = ii ;
       if( SKIP(ii,jj,kk) ) continue ;    /* 19 Nov 2004 */
       wf( xpr,ypr,zpr , &xx,&yy,&zz ) ;  /* get xx,yy,zz in original image */

       if( zout &&
           (xx < -0.5 || xx > nxh ||
            yy < -0.5 || yy > nyh || zz < -0.5 || zz > nzh ) ){

          NAR(ii,jj,kk) = 0.0 ; continue ;
       }

       ix = floor(xx) ;  fx = xx - ix ;   /* integer and       */
       jy = floor(yy) ;  fy = yy - jy ;   /* fractional coords */
       kz = floor(zz) ;  fz = zz - kz ;   /* in input image    */

       /* compute indexes from which to interpolate (-2,-1,0,+1,+2,+3),
          but clipped to lie inside input image volume                 */

       ix_m1 = ix-1    ; ix_00 = ix      ; ix_p1 = ix+1    ; ix_p2 = ix+2    ;
       CLIP(ix_m1,nx1) ; CLIP(ix_00,nx1) ; CLIP(ix_p1,nx1) ; CLIP(ix_p2,nx1) ;
       ix_m2 = ix-2    ; ix_p3 = ix+3 ;
       CLIP(ix_m2,nx1) ; CLIP(ix_p3,nx1) ;

       jy_m1 = jy-1    ; jy_00 = jy      ; jy_p1 = jy+1    ; jy_p2 = jy+2    ;
       CLIP(jy_m1,ny1) ; CLIP(jy_00,ny1) ; CLIP(jy_p1,ny1) ; CLIP(jy_p2,ny1) ;
       jy_m2 = jy-2    ; jy_p3 = jy+3 ;
       CLIP(jy_m2,ny1) ; CLIP(jy_p3,ny1) ;

       kz_m1 = kz-1    ; kz_00 = kz      ; kz_p1 = kz+1    ; kz_p2 = kz+2    ;
       CLIP(kz_m1,nz1) ; CLIP(kz_00,nz1) ; CLIP(kz_p1,nz1) ; CLIP(kz_p2,nz1) ;
       kz_m2 = kz-2    ; kz_p3 = kz+3 ;
       CLIP(kz_m2,nz1) ; CLIP(kz_p3,nz1) ;

       wt_m1 = Q_M1(fx) ; wt_00 = Q_00(fx) ;  /* interpolation weights */
       wt_p1 = Q_P1(fx) ; wt_p2 = Q_P2(fx) ;  /* in x-direction        */
       wt_m2 = Q_M2(fx) ; wt_p3 = Q_P3(fx) ;

#undef  XINT
#define XINT(j,k) wt_m2*FAR(ix_m2,j,k)+wt_m1*FAR(ix_m1,j,k) \
                 +wt_00*FAR(ix_00,j,k)+wt_p1*FAR(ix_p1,j,k) \
                 +wt_p2*FAR(ix_p2,j,k)+wt_p3*FAR(ix_p3,j,k)

       /* interpolate to location ix+fx at each jy,kz level */

       f_jm2_km2 = XINT(jy_m2,kz_m2) ; f_jm1_km2 = XINT(jy_m1,kz_m2) ;
       f_j00_km2 = XINT(jy_00,kz_m2) ; f_jp1_km2 = XINT(jy_p1,kz_m2) ;
       f_jp2_km2 = XINT(jy_p2,kz_m2) ; f_jp3_km2 = XINT(jy_p3,kz_m2) ;

       f_jm2_km1 = XINT(jy_m2,kz_m1) ; f_jm1_km1 = XINT(jy_m1,kz_m1) ;
       f_j00_km1 = XINT(jy_00,kz_m1) ; f_jp1_km1 = XINT(jy_p1,kz_m1) ;
       f_jp2_km1 = XINT(jy_p2,kz_m1) ; f_jp3_km1 = XINT(jy_p3,kz_m1) ;

       f_jm2_k00 = XINT(jy_m2,kz_00) ; f_jm1_k00 = XINT(jy_m1,kz_00) ;
       f_j00_k00 = XINT(jy_00,kz_00) ; f_jp1_k00 = XINT(jy_p1,kz_00) ;
       f_jp2_k00 = XINT(jy_p2,kz_00) ; f_jp3_k00 = XINT(jy_p3,kz_00) ;

       f_jm2_kp1 = XINT(jy_m2,kz_p1) ; f_jm1_kp1 = XINT(jy_m1,kz_p1) ;
       f_j00_kp1 = XINT(jy_00,kz_p1) ; f_jp1_kp1 = XINT(jy_p1,kz_p1) ;
       f_jp2_kp1 = XINT(jy_p2,kz_p1) ; f_jp3_kp1 = XINT(jy_p3,kz_p1) ;

       f_jm2_kp2 = XINT(jy_m2,kz_p2) ; f_jm1_kp2 = XINT(jy_m1,kz_p2) ;
       f_j00_kp2 = XINT(jy_00,kz_p2) ; f_jp1_kp2 = XINT(jy_p1,kz_p2) ;
       f_jp2_kp2 = XINT(jy_p2,kz_p2) ; f_jp3_kp2 = XINT(jy_p3,kz_p2) ;

       f_jm2_kp3 = XINT(jy_m2,kz_p3) ; f_jm1_kp3 = XINT(jy_m1,kz_p3) ;
       f_j00_kp3 = XINT(jy_00,kz_p3) ; f_jp1_kp3 = XINT(jy_p1,kz_p3) ;
       f_jp2_kp3 = XINT(jy_p2,kz_p3) ; f_jp3_kp3 = XINT(jy_p3,kz_p3) ;

       /* interpolate to jy+fy at each kz level */

       wt_m1 = Q_M1(fy) ; wt_00 = Q_00(fy) ; wt_p1 = Q_P1(fy) ;
       wt_p2 = Q_P2(fy) ; wt_m2 = Q_M2(fy) ; wt_p3 = Q_P3(fy) ;

       f_km2 =  wt_m2 * f_jm2_km2 + wt_m1 * f_jm1_km2 + wt_00 * f_j00_km2
              + wt_p1 * f_jp1_km2 + wt_p2 * f_jp2_km2 + wt_p3 * f_jp3_km2 ;

       f_km1 =  wt_m2 * f_jm2_km1 + wt_m1 * f_jm1_km1 + wt_00 * f_j00_km1
              + wt_p1 * f_jp1_km1 + wt_p2 * f_jp2_km1 + wt_p3 * f_jp3_km1 ;

       f_k00 =  wt_m2 * f_jm2_k00 + wt_m1 * f_jm1_k00 + wt_00 * f_j00_k00
              + wt_p1 * f_jp1_k00 + wt_p2 * f_jp2_k00 + wt_p3 * f_jp3_k00 ;

       f_kp1 =  wt_m2 * f_jm2_kp1 + wt_m1 * f_jm1_kp1 + wt_00 * f_j00_kp1
              + wt_p1 * f_jp1_kp1 + wt_p2 * f_jp2_kp1 + wt_p3 * f_jp3_kp1 ;

       f_kp2 =  wt_m2 * f_jm2_kp2 + wt_m1 * f_jm1_kp2 + wt_00 * f_j00_kp2
              + wt_p1 * f_jp1_kp2 + wt_p2 * f_jp2_kp2 + wt_p3 * f_jp3_kp2 ;

       f_kp3 =  wt_m2 * f_jm2_kp3 + wt_m1 * f_jm1_kp3 + wt_00 * f_j00_kp3
              + wt_p1 * f_jp1_kp3 + wt_p2 * f_jp2_kp3 + wt_p3 * f_jp3_kp3 ;

       /* interpolate to kz+fz to get output */

       wt_m1 = Q_M1(fz) ; wt_00 = Q_00(fz) ; wt_p1 = Q_P1(fz) ;
       wt_p2 = Q_P2(fz) ; wt_m2 = Q_M2(fz) ; wt_p3 = Q_P3(fz) ;

       val =  wt_m2 * f_km2 + wt_m1 * f_km1 + wt_00 * f_k00
            + wt_p1 * f_kp1 + wt_p2 * f_kp2 + wt_p3 * f_kp3 ;

            if( val > top ) val = top ;   /* clip to input data range */
       else if( val < bot ) val = bot ;

       NAR(ii,jj,kk) = val ;
     }
    }
   }

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   RETURN(newImg);
}

/*--------------------------------------------------------------------------*/
/*! Generic warping function.  Calls the specific one for the desired
    interpolation method, which was set by mri_warp3D_method().
      - im = input image (3D).
      - nxnew,nynew,nznew = size of output image grid.
      - wf( inew,jnew,knew , iold,jold,kold ) is a function that takes
        as input 3 indices in [0..nxnew-1,0..nynew-1,0..nznew-1] (the output
        image index space) and returns indices in the input image space.
      - indices in and out of wf() are floats.
      - by default, values outside the original image volume are treated
        as zero; this mode can be turned off using mri_warp3D_zerout().
----------------------------------------------------------------------------*/

MRI_IMAGE *mri_warp3D(
                    MRI_IMAGE *im, int nxnew, int nynew, int nznew,
                    void wf(float,float,float,float *,float *,float *) )
{
   switch( wtype ){

     default:
     case MRI_CUBIC:
       return mri_warp3D_cubic  ( im , nxnew,nynew,nznew , wf ) ;

     case MRI_LINEAR:
       return mri_warp3D_linear ( im , nxnew,nynew,nznew , wf ) ;

     case MRI_NN:
       return mri_warp3D_NN     ( im , nxnew,nynew,nznew , wf ) ;

     case MRI_QUINTIC:  /* 06 Aug 2003 */
       return mri_warp3D_quintic( im , nxnew,nynew,nznew , wf ) ;
   }
   return NULL ;  /* unreachable */
}

/*---------------------------------------------------------------------------*/

static float sx_scale, sy_scale, sz_scale ;  /* global scaler data */

/*! Internal transform function for resize "warp". */

static INLINE void w3dMRI_scaler( float  a,float  b,float  c,
                                  float *x,float *y,float *z )
{
   *x = sx_scale * a; *y = sy_scale * b; *z = sz_scale * c;
}

/*--------------------------------------------------------------------------*/
/*! Special image warp just for resizing.
----------------------------------------------------------------------------*/

MRI_IMAGE *mri_warp3D_resize( MRI_IMAGE *im , int nxnew, int nynew, int nznew )
{
   int nx,ny,nz , nnx,nny,nnz ;

   if( im == NULL ) return NULL ;
   nx  = im->nx ; ny  = im->ny ; nz  = im->nz ;
   nnx = nxnew  ; nny = nynew  ; nnz = nznew  ;

   if( nnx <= 0 && nny <= 0 && nnz <= 0 ) return NULL ;

   sx_scale = (nnx > 0) ? ((float)nx)/nnx : 0.0 ;  /* global variables */
   sy_scale = (nny > 0) ? ((float)ny)/nny : 0.0 ;
   sz_scale = (nnz > 0) ? ((float)nz)/nnz : 0.0 ;

   if( nnx <= 0 ){
     sx_scale = MAX(sy_scale,sz_scale) ;
     nnx      = (int)rint(sx_scale*nx) ;
   }
   if( nny <= 0 ){
     sy_scale = MAX(sx_scale,sz_scale) ;
     nny      = (int)rint(sy_scale*ny) ;
   }
   if( nnz <= 0 ){
     sz_scale = MAX(sx_scale,sy_scale) ;
     nnz      = (int)rint(sz_scale*nz) ;
   }

   return mri_warp3D( im , nnx,nny,nnz , w3dMRI_scaler ) ;
}

/*---------------------------------------------------------------------------*/

static float a11_aff,a12_aff,a13_aff,a14_aff ,
             a21_aff,a22_aff,a23_aff,a24_aff ,
             a31_aff,a32_aff,a33_aff,a34_aff  ;

/*! Internal transform function for affine "warp". */

static INLINE void w3dMRI_affine( float  a,float  b,float  c,
                                  float *x,float *y,float *z )
{
   *x = a11_aff*a + a12_aff*b + a13_aff*c + a14_aff ;
   *y = a21_aff*a + a22_aff*b + a23_aff*c + a24_aff ;
   *z = a31_aff*a + a32_aff*b + a33_aff*c + a34_aff ;
}

/*--------------------------------------------------------------------------*/
/*! Special image warp for affine transformation of indices.
----------------------------------------------------------------------------*/

MRI_IMAGE * mri_warp3D_affine( MRI_IMAGE *im , THD_vecmat aff )
{
   if( im == NULL ) return NULL ;

   a11_aff = aff.mm.mat[0][0] ; a12_aff = aff.mm.mat[0][1] ;
   a13_aff = aff.mm.mat[0][2] ; a14_aff = aff.vv.xyz[0]    ;

   a21_aff = aff.mm.mat[1][0] ; a22_aff = aff.mm.mat[1][1] ;
   a23_aff = aff.mm.mat[1][2] ; a24_aff = aff.vv.xyz[1]    ;

   a31_aff = aff.mm.mat[2][0] ; a32_aff = aff.mm.mat[2][1] ;
   a33_aff = aff.mm.mat[2][2] ; a34_aff = aff.vv.xyz[2]    ;

   return mri_warp3D( im , 0,0,0 , w3dMRI_scaler ) ;
}

/*===========================================================================*/
/*===========================================================================*/
/******** Functions for warping a dataset using coordinate transforms, *******/
/******** rather than the index transformation of the functions above. *******/
/*===========================================================================*/
/*===========================================================================*/

static THD_vecmat ijk_to_dicom_in, ijk_to_dicom_out, dicom_to_ijk_in ;

static void (*warp_out_to_in)(float  ,float  ,float ,
                              float *,float *,float * ) ;

/*----------------------------------------------------------------------------*/
/*! This is the function passed to mri_warp3D() for the ijk_out to ijk_in
    transformation.  It does the ijk_out to dicom_out transform here,
    then calls the coordinate transform function for dicom_out to dicom_in,
    then does the dicom_in to ijk_in transform here.
------------------------------------------------------------------------------*/

static INLINE void w3d_warp_func( float xout, float yout, float zout,
                                  float *xin, float *yin, float *zin )
{
   THD_fvec3 xxx,yyy ; float xi,yi,zi ;

   LOAD_FVEC3(xxx,xout,yout,zout) ;
   yyy = VECMAT_VEC( ijk_to_dicom_out , xxx ) ;       /* ijk_out to dicom_out */
   warp_out_to_in( yyy.xyz[0],yyy.xyz[1],yyy.xyz[2], /* dicom_out to dicom_in */
                  &(xxx.xyz[0]) , &(xxx.xyz[1]), &(xxx.xyz[2]) ) ;
   yyy = VECMAT_VEC( dicom_to_ijk_in , xxx ) ;          /* dicom_in to ijk_in */
   *xin = yyy.xyz[0] ; *yin = yyy.xyz[1] ; *zin = yyy.xyz[2] ;
}

/*--------------------------------------------------------------------------*/
/*! - Find the 8 corners of the input dataset (voxel edges, not centers).
    - Warp each one using the provided wfunc().
    - Return the min and max (x,y,z) coordinates of these warped points.
----------------------------------------------------------------------------*/

static void warp_corners( THD_3dim_dataset *inset,
                          void wfunc(float,float,float,float *,float *,float *),
                          float *xb , float *xt ,
                          float *yb , float *yt , float *zb , float *zt )
{
   THD_dataxes *daxes = inset->daxes ;
   THD_fvec3 corner , wcorn ;
   float nx0 = -0.5          , ny0 = -0.5          , nz0 = -0.5           ;
   float nx1 = daxes->nxx-0.5, ny1 = daxes->nyy-0.5, nz1 = daxes->nzz-0.5 ;
   float xx,yy,zz , xbot,ybot,zbot , xtop,ytop,ztop ;

   LOAD_FVEC3( corner , nx0,ny0,nz0 ) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = xtop = xx ;
   ybot = ytop = yy ;
   zbot = ztop = zz ;

   LOAD_FVEC3( corner , nx1,ny0,nz0 ) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx0,ny1,nz0 ) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx1,ny1,nz0 ) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx0,ny0,nz1 ) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx1,ny0,nz1 ) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx0,ny1,nz1 ) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   LOAD_FVEC3( corner , nx1,ny1,nz1 ) ;
   wcorn = VECMAT_VEC( ijk_to_dicom_in , corner ) ;
   wfunc( wcorn.xyz[0],wcorn.xyz[1],wcorn.xyz[2] , &xx,&yy,&zz ) ;
   xbot = MIN(xx,xbot); xtop = MAX(xx,xtop);
   ybot = MIN(yy,ybot); ytop = MAX(yy,ytop);
   zbot = MIN(zz,zbot); ztop = MAX(zz,ztop);

   *xb = xbot; *xt = xtop;
   *yb = ybot; *yt = ytop; *zb = zbot; *zt = ztop; return;
}

/*--------------------------------------------------------------------------*/
/*! Geometrically transform a 3D dataset, producing a new dataset.
     - w_in2out transforms DICOM coords from input grid to output grid
       (only needed if newggg != NULL and flag == WARP3D_NEWGRID;
        otherwise, w_in2out can be NULL)
     - w_out2in is the inverse of w_in2out (cannot be NULL)
     - newggg is a pointer to something that determines the new
       grid on which the output dataset will be computed
        - NULL means compute on the inset grid
        - otherwise, flag determines how newggg is interpreted:
        - WARP3D_NEWGRID => newggg is a "float *" to a new grid size
        - WARP3D_NEWDSET => newggg is a "THD_3dim_dataset *" to a dataset
                            whose grid will be used for the output grid
     - prefix = new dataset prefix (if NULL, then "warped")
     - zpad   = number of planes to zeropad on each face of inset (>= 0)
     - flag   = see above
     - Interpolation method can be set using mri_warp3D_method().
     - At end, input dataset is unloaded from memory.
     - If input is 3D+time, the output dataset won't have slice-wise time
       offsets, even if the input did.
----------------------------------------------------------------------------*/

THD_3dim_dataset * THD_warp3D(
                     THD_3dim_dataset *inset ,
                     void w_in2out(float,float,float,float *,float *,float *),
                     void w_out2in(float,float,float,float *,float *,float *),
                     void *newggg , char *prefix , int zpad , int flag        )
{
   THD_3dim_dataset *outset , *qset , *newgset=NULL ;
   int nxin,nyin,nzin,nvals , ival ;
   int nxout,nyout,nzout ;
   float xbot,xtop , ybot,ytop , zbot,ztop ;
   int use_newgrid , use_oldgrid ;
   float ddd_newgrid=0.0 , fac ;
   MRI_IMAGE *inim , *outim , *wim ;

ENTRY("THD_warp3D") ;

   /*-- decide which grid the output will be computed on --*/

   use_newgrid = ((flag & WARP3D_GRIDMASK) == WARP3D_NEWGRID) && (newggg != NULL) ;
   if( use_newgrid ){
     float *gg = (float *)newggg ;
     if( thd_floatscan(1,gg) == 0 && *gg > 0.0 ) ddd_newgrid = *gg ;
     else                                        use_newgrid = 0 ;
   } else if( newggg != NULL && (flag & WARP3D_GRIDMASK) == WARP3D_NEWDSET ){
     newgset = (THD_3dim_dataset *) newggg ;
     if( !ISVALID_DSET(newgset) ) newgset = NULL ;
   }
   use_oldgrid = !use_newgrid && (newgset == NULL) ;

   /*-- see if inputs are valid --*/

   if( !ISVALID_DSET(inset)             ||
       w_out2in == NULL                 ||
       (w_in2out == NULL && use_newgrid)  ){

     fprintf(stderr,"** ERROR: THD_warp3D has bad inputs!\n") ;
     RETURN(NULL);
   }

   /*-- zeropad and replace input dataset, if desired --*/

   if( zpad > 0 ){
     qset = THD_zeropad( inset , zpad,zpad,zpad,zpad,zpad,zpad ,
                         "Quetzalcoatl" , ZPAD_PURGE ) ;
     DSET_unload(inset) ;
     if( qset == NULL ){
       fprintf(stderr,"** ERROR: THD_warp3D can't zeropad!\n"); RETURN(NULL);
     }
   } else {
     qset = inset ;
   }

   /*-- read input data from disk, if not already present --*/

   DSET_load(qset) ;
   if( !DSET_LOADED(qset) ){
     fprintf(stderr,"** ERROR: THD_warp3D can't load input dataset!\n") ;
     if( qset != inset ) DSET_delete(qset) ; else DSET_unload(qset) ;
     RETURN(NULL);
   }

   /*-- compute mapping from input dataset (i,j,k) to DICOM coords --*/

   { THD_vecmat ijk_to_xyz , xyz_to_dicom ;

     LOAD_DIAG_MAT( ijk_to_xyz.mm , qset->daxes->xxdel,
                                    qset->daxes->yydel, qset->daxes->zzdel );
     LOAD_FVEC3   ( ijk_to_xyz.vv , qset->daxes->xxorg,
                                    qset->daxes->yyorg, qset->daxes->zzorg );

     xyz_to_dicom.mm = qset->daxes->to_dicomm ;
     LOAD_FVEC3( xyz_to_dicom.vv , 0.0,0.0,0.0 ) ;

     ijk_to_dicom_in = MUL_VECMAT( xyz_to_dicom , ijk_to_xyz ) ;
     dicom_to_ijk_in = INV_VECMAT( ijk_to_dicom_in ) ;
   }

   /*-- make empty output dataset --*/

   nxin  = DSET_NX(qset) ;
   nyin  = DSET_NY(qset) ;
   nzin  = DSET_NZ(qset) ;
   nvals = DSET_NVALS(qset) ;

   if( nxin*nyin*nzin <= 1 ){
     fprintf(stderr,"** ERROR: THD_warp3D has nxin=%d nyin=%d nzin=%d!\n",
             nxin,nyin,nzin ) ;
     if( qset != inset ) DSET_delete(qset) ; else DSET_unload(qset) ;
     RETURN(NULL) ;
   }

   if( use_oldgrid ){                /*-- output is on same grid as input --*/

     outset = EDIT_empty_copy( qset ) ;

   } else if( newgset != NULL ){     /*-- output on same grid as newgset --*/

     outset = EDIT_empty_copy( newgset ) ;

     EDIT_dset_items( outset ,
                        ADN_nvals       , nvals ,
                        ADN_type        , qset->type ,
                        ADN_view_type   , qset->view_type ,
                        ADN_func_type   , qset->func_type ,
                        ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                      ADN_none ) ;

     if( DSET_NUM_TIMES(qset) > 1 )     /* and some time structure? */
       EDIT_dset_items( outset ,
                          ADN_ntt       , nvals ,
                          ADN_tunits    , DSET_TIMEUNITS(qset) ,
                          ADN_ttorg     , DSET_TIMEORIGIN(qset) ,
                          ADN_ttdel     , DSET_TR(qset) ,
                          ADN_ttdur     , DSET_TIMEDURATION(qset) ,
                        ADN_none ) ;

     THD_copy_datablock_auxdata( qset->dblk , outset->dblk ) ;  /* 08 Jun 2004 */
     INIT_STAT_AUX( qset , MAX_STAT_AUX , outset->stat_aux ) ;

   } else {                          /*-- output is on new grid --*/

     float xmid,ymid,zmid ;
     THD_ivec3 nxyz , orixyz ;
     THD_fvec3 dxyz , orgxyz ;

     /* compute DICOM coordinates of warped corners */

     warp_corners( qset, w_in2out, &xbot,&xtop, &ybot,&ytop, &zbot,&ztop ) ;

     nxout = (int)( (xtop-xbot)/ddd_newgrid+0.999 ); if( nxout < 1 ) nxout = 1;
     nyout = (int)( (ytop-ybot)/ddd_newgrid+0.999 ); if( nyout < 1 ) nyout = 1;
     nzout = (int)( (ztop-zbot)/ddd_newgrid+0.999 ); if( nzout < 1 ) nzout = 1;

     xmid = 0.5*(xbot+xtop); ymid = 0.5*(ybot+ytop); zmid = 0.5*(zbot+ztop);
     xbot = xmid-0.5*(nxout-1)*ddd_newgrid; xtop = xbot+(nxout-1)*ddd_newgrid;
     ybot = ymid-0.5*(nyout-1)*ddd_newgrid; ytop = ybot+(nyout-1)*ddd_newgrid;
     zbot = zmid-0.5*(nzout-1)*ddd_newgrid; ztop = zbot+(nzout-1)*ddd_newgrid;

#if 0
     if( verb )
       fprintf(stderr,"++ Transformed grid:\n"
                      "++   xbot = %10.4g  xtop = %10.4g  nx = %d\n"
                      "++   ybot = %10.4g  ytop = %10.4g  ny = %d\n"
                      "++   zbot = %10.4g  ztop = %10.4g  nz = %d\n" ,
               xbot,xtop,nxout , ybot,ytop,nyout , zbot,ztop,nzout    ) ;
#endif

     if( nxout*nyout*nzout <= 1 ){
       fprintf(stderr,"** ERROR: THD_warp3D has nxout=%d nyout=%d nzout=%d!\n",
               nxout,nyout,nzout ) ;
       if( qset != inset ) DSET_delete(qset) ; else DSET_unload(qset) ;
       RETURN(NULL) ;
     }

     nxyz.ijk[0] = nxout ; dxyz.xyz[0] = ddd_newgrid ;  /* setup axes */
     nxyz.ijk[1] = nyout ; dxyz.xyz[1] = ddd_newgrid ;
     nxyz.ijk[2] = nzout ; dxyz.xyz[2] = ddd_newgrid ;

     orixyz.ijk[0] = ORI_R2L_TYPE ; orgxyz.xyz[0] = xbot ;
     orixyz.ijk[1] = ORI_A2P_TYPE ; orgxyz.xyz[1] = ybot ;
     orixyz.ijk[2] = ORI_I2S_TYPE ; orgxyz.xyz[2] = zbot ;

     /** create dataset and mangle it into the desired shape **/

     outset = EDIT_empty_copy( NULL ) ;  /* void and formless dataset */

     EDIT_dset_items( outset ,           /* give it some structure! */
                        ADN_nxyz        , nxyz ,
                        ADN_xyzdel      , dxyz ,
                        ADN_xyzorg      , orgxyz ,
                        ADN_xyzorient   , orixyz ,
                        ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                        ADN_nvals       , nvals ,
                        ADN_type        , qset->type ,
                        ADN_view_type   , qset->view_type ,
                        ADN_func_type   , qset->func_type ,
                      ADN_none ) ;

     if( DSET_NUM_TIMES(qset) > 1 )     /* and some time structure? */
       EDIT_dset_items( outset ,
                          ADN_ntt       , nvals ,
                          ADN_tunits    , DSET_TIMEUNITS(qset) ,
                          ADN_ttorg     , DSET_TIMEORIGIN(qset) ,
                          ADN_ttdel     , DSET_TR(qset) ,
                          ADN_ttdur     , DSET_TIMEDURATION(qset) ,
                        ADN_none ) ;

     THD_copy_datablock_auxdata( qset->dblk , outset->dblk ) ;  /* 08 Jun 2004 */
     INIT_STAT_AUX( qset , MAX_STAT_AUX , outset->stat_aux ) ;

   } /*-- end of warping to new grid --*/

   nxout = DSET_NX(outset) ;
   nyout = DSET_NY(outset) ;
   nzout = DSET_NZ(outset) ;

   /*-- compute mapping from output dataset (i,j,k) to DICOM coords --*/

   { THD_vecmat ijk_to_xyz , xyz_to_dicom ;

     LOAD_DIAG_MAT( ijk_to_xyz.mm, outset->daxes->xxdel,
                                   outset->daxes->yydel, outset->daxes->zzdel );
     LOAD_FVEC3   ( ijk_to_xyz.vv, outset->daxes->xxorg,
                                   outset->daxes->yyorg, outset->daxes->zzorg );

     xyz_to_dicom.mm = outset->daxes->to_dicomm ;
     LOAD_FVEC3( xyz_to_dicom.vv , 0.0,0.0,0.0 ) ;

     ijk_to_dicom_out = MUL_VECMAT( xyz_to_dicom , ijk_to_xyz ) ;
   }

   /*-- add prefix to new dataset --*/

   if( !THD_filename_ok(prefix) ) prefix = "warped" ;
   EDIT_dset_items( outset , ADN_prefix,prefix , ADN_none ) ;

   /*-- loop over bricks and warp them --*/

   warp_out_to_in = w_out2in ;  /* for use in w3d_warp_func(), supra */

   for( ival=0 ; ival < nvals ; ival++ ){
     inim  = DSET_BRICK(qset,ival) ;
     fac   = DSET_BRICK_FACTOR(qset,ival) ;
     if( fac > 0.0 && fac != 1.0 ) wim = mri_scale_to_float( fac , inim ) ;
     else                          wim = inim ;
     outim = mri_warp3D( wim , nxout,nyout,nzout , w3d_warp_func ) ;
     if( outim == NULL ){
       fprintf(stderr,"** ERROR: THD_warp3D fails at ival=%d\n",ival);
       DSET_delete(outset);
       if( qset != inset ) DSET_delete(qset) ; else DSET_unload(qset) ;
       RETURN(NULL) ;
     }

     if( wim != inim ){  /* if we scaled input */
       float gtop , fimfac ;
       mri_free(wim) ;

       /* 20 Oct 2003: rescale if input was an integer type */

       if( outim->kind == MRI_float && MRI_IS_INT_TYPE(inim->kind) ){
         fimfac = fac ;
         wim = mri_scalize( outim , inim->kind , &fimfac ) ;
         EDIT_BRICK_FACTOR( outset , ival , fimfac ) ;
         mri_free(outim) ; outim = wim ;
       } else {                                     /* input not an integer: */
         EDIT_BRICK_FACTOR( outset , ival , 0.0 ) ; /* output=unscaled float */
       }

     }
     EDIT_substitute_brick( outset, ival,outim->kind,mri_data_pointer(outim) );
     DSET_unload_one( qset , ival ) ;
   }

   /*-- done!!! --*/

   if( qset != inset ) DSET_delete(qset) ; else DSET_unload(qset) ;

   THD_load_statistics( outset ) ;
   RETURN( outset ) ;
}

/*--------------------------------------------------------------------------*/

static THD_vecmat aff_out2in , aff_in2out ; /* data for afo2i() and afi2o() */

/*! Internal transform for THD_warp3D_affine(). ----------------------------*/

static INLINE void afo2i( float  a,float  b,float  c,
                          float *x,float *y,float *z )
{
   THD_fvec3 xxx , yyy ;
   LOAD_FVEC3( xxx , a,b,c ) ;
   yyy = VECMAT_VEC( aff_out2in , xxx ) ;
   UNLOAD_FVEC3( yyy , *x , *y , *z ) ;
}

/*! Internal transform for THD_warp3D_affine(). ----------------------------*/

static INLINE void afi2o( float  a,float  b,float  c,
                          float *x,float *y,float *z )
{
   THD_fvec3 xxx , yyy ;
   LOAD_FVEC3( xxx , a,b,c ) ;
   yyy = VECMAT_VEC( aff_in2out , xxx ) ;
   UNLOAD_FVEC3( yyy , *x , *y , *z ) ;
}

/*--------------------------------------------------------------------------*/
/*! A version of THD_warp3D() that uses an affine map, specified in the
    out2in parameter.  The main advantage over THD_warp3D() is that the
    transformation functions internal -- afo2i() and afi2o() -- which
    makes it simpler on the caller and also somewhat faster.
----------------------------------------------------------------------------*/

THD_3dim_dataset * THD_warp3D_affine(
                     THD_3dim_dataset *inset ,
                     THD_vecmat out2in ,
                     void *newggg , char *prefix , int zpad , int flag )
{
   aff_out2in = out2in ;
   aff_in2out = INV_VECMAT(aff_out2in) ;

   return THD_warp3D( inset , afi2o,afo2i , newggg,prefix,zpad,flag ) ;
}

/*--------------------------------------------------------------------------*/
/*! Internal transform functions for TT <-> MNI coords.  Both are RAI,
    as per AFNI internal logic.  11 Mar 2004 - RW Cox [Jury Duty Day]
    cf. http://www.mrc-cbu.cam.ac.uk/Imaging/Common/mnispace.shtml
----------------------------------------------------------------------------*/

static INLINE void w3d_mni2tta( float mx , float my , float mz ,
                                float *tx, float *ty, float *tz )
{
   *tx = 0.99 * mx ;

   if( mz > 0.0 ){
     *ty =  0.9688 * my + 0.0460 * mz ;
     *tz = -0.0485 * my + 0.9189 * mz ;
   } else {
     *ty =  0.9688 * my + 0.0420 * mz ;
     *tz = -0.0485 * my + 0.8390 * mz ;
   }
}

/*-------  Should be the inverse of the above! -----------------------------*/

static INLINE void w3d_tta2mni( float tx , float ty , float tz ,
                                float *mx, float *my, float *mz )
{
   *mx = 1.01010 * tx ;
   *my = 1.02962 * ty - 0.05154 * tz ;
   *mz = 0.05434 * ty + 1.08554 * tz ;
   if( *mz < 0.0 ) *mz *= 1.09523 ;
}

/*--------------------------------------------------------------------------*/

THD_3dim_dataset * THD_warp3D_tta2mni( THD_3dim_dataset *inset , void *newggg ,
                                       char *prefix , int zpad , int flag )
{
   return THD_warp3D( inset , w3d_tta2mni,w3d_mni2tta , NULL,prefix,0,0 ) ;
}

/*-------  Should be the inverse of the above! -----------------------------*/

THD_3dim_dataset * THD_warp3D_mni2tta( THD_3dim_dataset *inset , void *newggg ,
                                       char *prefix , int zpad , int flag )
{
   return THD_warp3D( inset , w3d_mni2tta,w3d_tta2mni , newggg,prefix,zpad,flag ) ;
}
