#include "mrilib.h"

/*** NOT 7D SAFE ***/

/****************************************************************************/
/** functions to "warp" 3D images -- not very efficient, but quite general **/
/****************************************************************************/

/*--------------------------------------------------------------------------*/

static int wtype = MRI_LINEAR ;
void mri_warp3D_method( int mode ){ wtype = mode ; }

/*--------------------------------------------------------------------------*/

static int zout  = 1 ;
void mri_warp3D_zerout( int zzzz ){ zout  = zzzz ; }

/*--------------------------------------------------------------------------*/

MRI_IMAGE *mri_warp3D( MRI_IMAGE *im, int nxnew, int nynew, int nznew,
                       void wf(float,float,float,float *,float *,float *) ){
   switch( wtype ){

     default:
     case MRI_CUBIC:
        return mri_warp3D_cubic ( im , nxnew,nynew,nznew , wf ) ;

     case MRI_LINEAR:
        return mri_warp3D_linear( im , nxnew,nynew,nznew , wf ) ;

     case MRI_NN:
        return mri_warp3D_NN    ( im , nxnew,nynew,nznew , wf ) ;
   }
   return NULL ;  /* unreachable */
}

/*--------------------------------------------------------------------------*/

#define FAR(i,j,k) far[(i)+(j)*nx+(k)*nxy]      /* input point */
#define NAR(i,j,k) nar[(i)+(j)*nx+(k)*nxynew]   /* output point */

/* clip input image index to valid range for x,y,z directions */

#define CLIP(mm,nn) if(mm < 0)mm=0; else if(mm > nn)mm=nn

/* define Lagrange cubic interpolation polynomials */

#define P_M1(x)  (-(x)*((x)-1)*((x)-2))
#define P_00(x)  (3*((x)+1)*((x)-1)*((x)-2))
#define P_P1(x)  (-3*(x)*((x)+1)*((x)-2))
#define P_P2(x)  ((x)*((x)+1)*((x)-1))
#define P_FACTOR 4.62962963e-3            /* 1/216 = final scaling factor */

/*---------------------------------------------------------------------------*/

#ifdef __GNUC__
# define INLINE inline
#else
# define INLINE /*nada*/
#endif

/*---------------------------------------------------------------------------*/

static float sx_scale, sy_scale, sz_scale ;  /* global scaler data */

static INLINE void w3dMRI_scaler( float  a,float  b,float  c,
                                  float *x,float *y,float *z )
{
   *x = sx_scale * a; *y = sy_scale * b; *z = sz_scale * c;
}

/*---------------------------------------------------------------------------*/

static float a11_aff,a12_aff,a13_aff,a14_aff ,
             a21_aff,a22_aff,a23_aff,a24_aff ,
             a31_aff,a32_aff,a33_aff,a34_aff  ;

static INLINE void w3dMRI_affine( float  a,float  b,float  c,
                                  float *x,float *y,float *z )
{
   *x = a11_aff*a + a12_aff*b + a13_aff*c + a14_aff ;
   *y = a21_aff*a + a22_aff*b + a23_aff*c + a24_aff ;
   *z = a31_aff*a + a32_aff*b + a33_aff*c + a34_aff ;
}

/*---------------------------------------------------------------------------*/
/*! Transform a 3D image geometrically, using cubic interpolation.
      - im = input image
      - nxnew,nynew,nznew = size of output image
      - wf( inew,jnew,knew , iold,jold,kold ) is a function that takes
        as input 3 indices in [0..nxnew-1,0..nynew-1,0..nznew-1] and
        returns indices in the input image space.
      - indices in and out of wf() are floats
-----------------------------------------------------------------------------*/

MRI_IMAGE *mri_warp3D_cubic( MRI_IMAGE *im, int nxnew, int nynew, int nznew,
                            void wf(float,float,float,float *,float *,float *) )
{
   MRI_IMAGE *imfl , *new ;
   float *far , *nar ;
   float xpr,ypr,zpr, xx,yy,zz, fx,fy,fz ;
   int ii,jj,kk, nx,ny,nz,nxy, nx1,ny1,nz1, ix,jy,kz, nxynew ;
   float bot,top,val ;
   float nxh,nyh,nzh ;

   int ix_m1,ix_00,ix_p1,ix_p2 ;         /* interpolation indices */
   int jy_m1,jy_00,jy_p1,jy_p2 ;         /* (input image) */
   int kz_m1,kz_00,kz_p1,kz_p2 ;

   float wt_m1,wt_00,wt_p1,wt_p2 ;   /* interpolation weights */

   float f_jm1_km1, f_j00_km1, f_jp1_km1, f_jp2_km1, /* interpolants */
         f_jm1_k00, f_j00_k00, f_jp1_k00, f_jp2_k00,
         f_jm1_kp1, f_j00_kp1, f_jp1_kp1, f_jp2_kp1,
         f_jm1_kp2, f_j00_kp2, f_jp1_kp2, f_jp2_kp2,
         f_km1    , f_k00    , f_kp1    , f_kp2     ;

ENTRY("mri_warp3D_cubic") ;

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
       new  = mri_warp3D_cubic( imfl , nxnew,nynew,nznew , wf ) ;
       mri_free(imfl) ;
       imfl = mri_to_mri(im->kind,new) ;
       if( imfl != NULL ){ mri_free(new); new = imfl; }
       RETURN(new) ;
     }

     case MRI_rgb:{
       MRI_IMARR *imar = mri_rgb_to_3float(im) ;
       MRI_IMAGE *rim,*gim,*bim ;
       rim = mri_warp3D_cubic( IMARR_SUBIM(imar,0), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,0) ) ;
       gim = mri_warp3D_cubic( IMARR_SUBIM(imar,1), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,1) ) ;
       bim = mri_warp3D_cubic( IMARR_SUBIM(imar,2), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,2) ) ;
       FREE_IMARR(imar) ;
       new = mri_3to_rgb( rim,gim,bim ) ;
       mri_free(rim); mri_free(gim); mri_free(bim); RETURN(new);
     }

     case MRI_complex:{
       MRI_IMARR *imar = mri_complex_to_pair(im) ;
       MRI_IMAGE *rim, *iim ;
       rim = mri_warp3D_cubic( IMARR_SUBIM(imar,0), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,0) ) ;
       iim = mri_warp3D_cubic( IMARR_SUBIM(imar,1), nxnew,nynew,nznew, wf ) ;
                     mri_free( IMARR_SUBIM(imar,1) ) ;
       FREE_IMARR(imar) ;
       new = mri_pair_to_complex( rim , iim ) ;
       mri_free(rim); mri_free(iim); RETURN(new);
     }

   } /* end of special cases of input datum */

   /*----- at this point, imfl is in float format -----*/

   far = MRI_FLOAT_PTR( imfl ) ;                         /* input image data */

   new = mri_new_vol( nxnew,nynew,nznew, MRI_float ) ;  /* make output image */
   nar = MRI_FLOAT_PTR( new ) ;                         /* output image data */

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
       wf( xpr,ypr,zpr , &xx,&yy,&zz ) ;  /* get xx,yy,zz in original image */

       if( zout &&
           (xx < -0.5 || xx > nxh ||
            yy < -0.5 || yy > nyh || zz < -0.5 || zz > nzh ) ){

          NAR(ii,jj,kk) = 0.0 ; continue ;
       }

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

       NAR(ii,jj,kk) = val ;
     }
    }
   }

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   RETURN(new);
}

/*---------------------------------------------------------------------------*/
/*! Transform a 3D image geometrically, using linear interpolation.
      - im = input image
      - nxnew,nynew,nznew = size of output image
      - wf( inew,jnew,knew , iold,jold,kold ) is a function that takes
        as input 3 indices in [0..nxnew-1,0..nynew-1,0..nznew-1] and
        returns indices in the input image space.
      - indices in and out of wf() are floats
-----------------------------------------------------------------------------*/

MRI_IMAGE *mri_warp3D_linear( MRI_IMAGE *im, int nxnew, int nynew, int nznew,
                            void wf(float,float,float,float *,float *,float *) )
{
   MRI_IMAGE *imfl , *new ;
   float *far , *nar ;
   float xpr,ypr,zpr, xx,yy,zz, fx,fy,fz ;
   int ii,jj,kk, nx,ny,nz,nxy, nx1,ny1,nz1, ix,jy,kz, nxynew ;
   float bot,top,val ;
   float nxh,nyh,nzh ;

   int ix_00,ix_p1 ;         /* interpolation indices */
   int jy_00,jy_p1 ;         /* (input image) */
   int kz_00,kz_p1 ;

   float wt_00,wt_p1 ;   /* interpolation weights */

   float f_j00_k00, f_jp1_k00, f_j00_kp1, f_jp1_kp1, f_k00, f_kp1 ;

ENTRY("mri_warp3D_linear") ;

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
       new  = mri_warp3D_linear( imfl , nxnew,nynew,nznew , wf ) ;
       mri_free(imfl) ;
       imfl = mri_to_mri(im->kind,new) ;
       if( imfl != NULL ){ mri_free(new); new = imfl; }
       RETURN(new) ;
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
       new = mri_3to_rgb( rim,gim,bim ) ;
       mri_free(rim); mri_free(gim); mri_free(bim); RETURN(new);
     }

     case MRI_complex:{
       MRI_IMARR *imar = mri_complex_to_pair(im) ;
       MRI_IMAGE *rim, *iim ;
       rim = mri_warp3D_linear( IMARR_SUBIM(imar,0), nxnew,nynew,nznew, wf ) ;
                      mri_free( IMARR_SUBIM(imar,0) ) ;
       iim = mri_warp3D_linear( IMARR_SUBIM(imar,1), nxnew,nynew,nznew, wf ) ;
                      mri_free( IMARR_SUBIM(imar,1) ) ;
       FREE_IMARR(imar) ;
       new = mri_pair_to_complex( rim , iim ) ;
       mri_free(rim); mri_free(iim); RETURN(new);
     }

   } /* end of special cases of input datum */

   /*----- at this point, imfl is in float format -----*/

   far = MRI_FLOAT_PTR( imfl ) ;                         /* input image data */

   new = mri_new_vol( nxnew,nynew,nznew, MRI_float ) ;  /* make output image */
   nar = MRI_FLOAT_PTR( new ) ;                         /* output image data */

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
       wf( xpr,ypr,zpr , &xx,&yy,&zz ) ;  /* get xx,yy,zz in original image */

       if( zout &&
           (xx < -0.5 || xx > nxh ||
            yy < -0.5 || yy > nyh || zz < -0.5 || zz > nzh ) ){

          NAR(ii,jj,kk) = 0.0 ; continue ;
       }

       ix = floor(xx) ;  fx = xx - ix ;   /* integer and       */
       jy = floor(yy) ;  fy = yy - jy ;   /* fractional coords */
       kz = floor(zz) ;  fz = zz - kz ;

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

       val = (1.0-fz) * f_k00 + fz * f_kp1 ;

            if( val > top ) val = top ;   /* clip to input data range */
       else if( val < bot ) val = bot ;

       NAR(ii,jj,kk) = val ;
     }
    }
   }

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   RETURN(new);
}

/*---------------------------------------------------------------------------*/
/*! Transform a 3D image geometrically, using NN 'interpolation'.
      - im = input image
      - nxnew,nynew,nznew = size of output image
      - wf( inew,jnew,knew , iold,jold,kold ) is a function that takes
        as input 3 indices in [0..nxnew-1,0..nynew-1,0..nznew-1] and
        returns indices in the input image space.
      - indices in and out of wf() are floats
-----------------------------------------------------------------------------*/

MRI_IMAGE *mri_warp3D_NN( MRI_IMAGE *im, int nxnew, int nynew, int nznew,
                          void wf(float,float,float,float *,float *,float *) )
{
   MRI_IMAGE *imfl , *new ;
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
       new  = mri_warp3D_NN( imfl , nxnew,nynew,nznew , wf ) ;
       mri_free(imfl) ;
       imfl = mri_to_mri(im->kind,new) ;
       if( imfl != NULL ){ mri_free(new); new = imfl; }
       RETURN(new) ;
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
       new = mri_3to_rgb( rim,gim,bim ) ;
       mri_free(rim); mri_free(gim); mri_free(bim); RETURN(new);
     }

     case MRI_complex:{
       MRI_IMARR *imar = mri_complex_to_pair(im) ;
       MRI_IMAGE *rim, *iim ;
       rim = mri_warp3D_NN( IMARR_SUBIM(imar,0), nxnew,nynew,nznew, wf ) ;
                  mri_free( IMARR_SUBIM(imar,0) ) ;
       iim = mri_warp3D_NN( IMARR_SUBIM(imar,1), nxnew,nynew,nznew, wf ) ;
                  mri_free( IMARR_SUBIM(imar,1) ) ;
       FREE_IMARR(imar) ;
       new = mri_pair_to_complex( rim , iim ) ;
       mri_free(rim); mri_free(iim); RETURN(new);
     }

   } /* end of special cases of input datum */

   /*----- at this point, imfl is in float format -----*/

   far = MRI_FLOAT_PTR( imfl ) ;                         /* input image data */

   new = mri_new_vol( nxnew,nynew,nznew, MRI_float ) ;  /* make output image */
   nar = MRI_FLOAT_PTR( new ) ;                         /* output image data */

   /*** loop over output points and warp to them ***/

   nxh = nx-0.5 ; nyh = ny-0.5 ; nzh = nz-0.5 ;
   for( kk=0 ; kk < nznew ; kk++ ){
    zpr = kk ;
    for( jj=0 ; jj < nynew ; jj++ ){
     ypr = jj ;
     for( ii=0 ; ii < nxnew ; ii++ ){
       xpr = ii ;
       wf( xpr,ypr,zpr , &xx,&yy,&zz ) ;  /* get xx,yy,zz in original image */

       if( zout &&
           (xx < -0.5 || xx > nxh ||
            yy < -0.5 || yy > nyh || zz < -0.5 || zz > nzh ) ){

          NAR(ii,jj,kk) = 0.0 ; continue ;
       }

       ix = rint(xx) ; jy = rint(yy) ; kz = rint(zz) ;
       CLIP(ix,nx1)  ; CLIP(jy,ny1)  ; CLIP(kz,nz1)  ;

       NAR(ii,jj,kk) = FAR(ix,jy,kz) ;
     }
    }
   }

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   RETURN(new);
}

/*--------------------------------------------------------------------------*/

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
     nnx      = rint(sx_scale*nx) ;
   }
   if( nny <= 0 ){
     sy_scale = MAX(sx_scale,sz_scale) ;
     nny      = rint(sy_scale*ny) ;
   }
   if( nnz <= 0 ){
     sz_scale = MAX(sx_scale,sy_scale) ;
     nnz      = rint(sz_scale*nz) ;
   }

   switch( wtype ){

     default:
     case MRI_CUBIC:
        return mri_warp3D_cubic ( im , nnx,nny,nnz , w3dMRI_scaler ) ;

     case MRI_LINEAR:
        return mri_warp3D_linear( im , nnx,nny,nnz , w3dMRI_scaler ) ;

     case MRI_NN:
        return mri_warp3D_NN    ( im , nnx,nny,nnz , w3dMRI_scaler ) ;
   }

   return NULL ; /* unreachable */
}

/*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_warp3D_affine( MRI_IMAGE *im , THD_vecmat aff )
{
   if( im == NULL ) return NULL ;

   a11_aff = aff.mm.mat[0][0] ; a12_aff = aff.mm.mat[0][1] ;
   a13_aff = aff.mm.mat[0][2] ; a14_aff = aff.vv.xyz[0]    ;

   a21_aff = aff.mm.mat[1][0] ; a22_aff = aff.mm.mat[1][1] ;
   a23_aff = aff.mm.mat[1][2] ; a24_aff = aff.vv.xyz[1]    ;

   a31_aff = aff.mm.mat[2][0] ; a32_aff = aff.mm.mat[2][1] ;
   a33_aff = aff.mm.mat[2][2] ; a34_aff = aff.vv.xyz[2]    ;

   switch( wtype ){

     default:
     case MRI_CUBIC:
        return mri_warp3D_cubic ( im , 0,0,0 , w3dMRI_scaler ) ;

     case MRI_LINEAR:
        return mri_warp3D_linear( im , 0,0,0 , w3dMRI_scaler ) ;

     case MRI_NN:
        return mri_warp3D_NN    ( im , 0,0,0 , w3dMRI_scaler ) ;
   }

   return NULL ; /* unreachable */
}
