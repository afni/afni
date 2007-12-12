#include "mrilib.h"

#undef  BIGVAL
#define BIGVAL 1.e+38

/* is a voxel 'good' to use? */

#undef  GOOD
#define GOOD(i) (mask==NULL || mask[i])

/* mark for a good setup */

#undef  SMAGIC
#define SMAGIC 208921148  /* Zip+4 Code for AFNI Group at NIMH */

/* global access to setup parameters */

static GA_setup *gstup = NULL ;

/*---------------------------------------------------------------------------*/
static int verb = 0 ;
void mri_genalign_verbose(int v){ verb = v ; }

/*---------------------------------------------------------------------------*/
static int gcd( int m , int n )    /* Euclid's Greatest Common Denominator */
{
  while( m > 0 ){
    if( n > m ){ int t=m; m=n; n=t; } /* swap */
    m -= n ;
  }
  return n ;
}
static int find_relprime_fixed( int n )  /* find number relatively prime to n */
{
   int dj , n5=n/5 ;
   if( n5 < 2 ) return 1 ;
   for( dj=n5 ; gcd(n,dj) > 1 ; dj++ ) ; /*nada*/
   return dj ;
}
/*---------------------------------------------------------------------------*/
/*! Smooth an image with a given method to a given radius.
    Assumes the dx,dy,dz parameters in the image struct are correct! */

static MRI_IMAGE * GA_smooth( MRI_IMAGE *im , int meth , float rad )
{
   MRI_IMAGE *om=NULL ;

   ENTRY("GA_smooth") ;

   if( im == NULL || im->kind != MRI_float || rad <= 0.0f ) RETURN(NULL) ;

   switch( meth ){
     default:
     case GA_SMOOTH_GAUSSIAN:
       om  = mri_to_float(im) ;
       rad = FWHM_TO_SIGMA(rad) ;   /* convert rad from FWHM to st.dev. */
       FIR_blur_volume_3d( om->nx , om->ny , om->nz ,
                           om->dx , om->dy , om->dz ,
                           MRI_FLOAT_PTR(om) , rad,rad,rad ) ;
     break ;

     case GA_SMOOTH_MEDIAN:{
       float d;
       d = MIN(im->dx,im->dy) ; d = MIN(d,im->dz) ;
       if( rad <= 1.01f*d ) rad = 1.01f*d ;
       if( rad >  0.0f ){ mri_medianfilter_usedxyz(1);            }
       else             { mri_medianfilter_usedxyz(0); rad=1.01f; }
       om = mri_medianfilter( im , rad , NULL , 0 ) ;
     }
     break ;
   }

   RETURN(om) ;
}

/*---------------------------------------------------------------------------*/

/* for interpolation access to the (i,j,k) element of an array */

#undef  FAR
#define FAR(i,j,k) far[(i)+(j)*nx+(k)*nxy]

/* clip value mm to range 0..nn */

#undef  CLIP
#define CLIP(mm,nn) if(mm < 0)mm=0; else if(mm > nn)mm=nn

static float outval = 0.0f ;                  /* value for 'outside' voxels */
void GA_set_outval( float v ){ outval = v; }  /* 28 Feb 2007 */

/*---------------------------------------------------------------*/
/*! Interpolate an image at npp (index) points, using NN method. */

static void GA_interp_NN( MRI_IMAGE *fim ,
                          int npp, float *ip, float *jp, float *kp, float *vv )
{
   int nx=fim->nx , ny=fim->ny , nz=fim->nz , nxy=nx*ny , ii,jj,kk , pp ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float *far = MRI_FLOAT_PTR(fim) ;

ENTRY("GA_interp_NN") ;

   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ vv[pp]=outval; continue; }
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ vv[pp]=outval; continue; }
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ vv[pp]=outval; continue; }

     ii = (int)(xx+0.5f) ; jj = (int)(yy+0.5f) ; kk = (int)(zz+0.5f) ;
     vv[pp] = FAR(ii,jj,kk) ;
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Interpolate an image at npp (index) points, using linear method. */

static void GA_interp_linear( MRI_IMAGE *fim ,
                              int npp, float *ip, float *jp, float *kp, float *vv )
{
   int nx=fim->nx , ny=fim->ny , nz=fim->nz , nxy=nx*ny , pp ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float fx,fy,fz ;
   float *far = MRI_FLOAT_PTR(fim) ;
   int nx1=nx-1,ny1=ny-1,nz1=nz-1 ;
   float ix,jy,kz ;
   int ix_00,ix_p1 ;         /* interpolation indices */
   int jy_00,jy_p1 ;
   int kz_00,kz_p1 ;
   float wt_00,wt_p1 ;       /* interpolation weights */
   float f_j00_k00, f_jp1_k00, f_j00_kp1, f_jp1_kp1, f_k00, f_kp1 ;

ENTRY("GA_interp_linear") ;

   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ vv[pp]=outval; continue; }
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ vv[pp]=outval; continue; }
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ vv[pp]=outval; continue; }

     ix = floorf(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floorf(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floorf(zz) ;  fz = zz - kz ;

     ix_00 = ix ; ix_p1 = ix_00+1 ; CLIP(ix_00,nx1) ; CLIP(ix_p1,nx1) ;
     jy_00 = jy ; jy_p1 = jy_00+1 ; CLIP(jy_00,ny1) ; CLIP(jy_p1,ny1) ;
     kz_00 = kz ; kz_p1 = kz_00+1 ; CLIP(kz_00,nz1) ; CLIP(kz_p1,nz1) ;

     wt_00 = 1.0f-fx ; wt_p1 = fx ;  /* weights for ix_00 and ix_p1 points */

#undef  XINT
#define XINT(j,k) wt_00*FAR(ix_00,j,k)+wt_p1*FAR(ix_p1,j,k)

     /* interpolate to location ix+fx at each jy,kz level */

     f_j00_k00 = XINT(jy_00,kz_00) ; f_jp1_k00 = XINT(jy_p1,kz_00) ;
     f_j00_kp1 = XINT(jy_00,kz_p1) ; f_jp1_kp1 = XINT(jy_p1,kz_p1) ;

     /* interpolate to jy+fy at each kz level */

     wt_00 = 1.0f-fy ; wt_p1 = fy ;
     f_k00 =  wt_00 * f_j00_k00 + wt_p1 * f_jp1_k00 ;
     f_kp1 =  wt_00 * f_j00_kp1 + wt_p1 * f_jp1_kp1 ;

     /* interpolate to kz+fz to get output */

     vv[pp] = (1.0f-fz) * f_k00 + fz * f_kp1 ;
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

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

/*------------------------------------------------------------------*/
/*! Interpolate an image at npp (index) points, using cubic method. */

static void GA_interp_cubic( MRI_IMAGE *fim ,
                             int npp, float *ip, float *jp, float *kp, float *vv )
{
   int nx=fim->nx , ny=fim->ny , nz=fim->nz , nxy=nx*ny , pp ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float fx,fy,fz ;
   float *far = MRI_FLOAT_PTR(fim) ;
   int nx1=nx-1,ny1=ny-1,nz1=nz-1, ix,jy,kz ;

   int ix_m1,ix_00,ix_p1,ix_p2 ;     /* interpolation indices */
   int jy_m1,jy_00,jy_p1,jy_p2 ;
   int kz_m1,kz_00,kz_p1,kz_p2 ;
   float wt_m1,wt_00,wt_p1,wt_p2 ;   /* interpolation weights */
   float f_jm1_km1, f_j00_km1, f_jp1_km1, f_jp2_km1, /* interpolants */
         f_jm1_k00, f_j00_k00, f_jp1_k00, f_jp2_k00,
         f_jm1_kp1, f_j00_kp1, f_jp1_kp1, f_jp2_kp1,
         f_jm1_kp2, f_j00_kp2, f_jp1_kp2, f_jp2_kp2,
         f_km1    , f_k00    , f_kp1    , f_kp2     ;

ENTRY("GA_interp_cubic") ;

   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ vv[pp]=outval; continue; }
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ vv[pp]=outval; continue; }
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ vv[pp]=outval; continue; }

     ix = floorf(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floorf(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floorf(zz) ;  fz = zz - kz ;

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

     vv[pp] = P_FACTOR * (  wt_m1 * f_km1 + wt_00 * f_k00
                          + wt_p1 * f_kp1 + wt_p2 * f_kp2 ) ;
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

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

/*--------------------------------------------------------------------*/
/*! Interpolate an image at npp (index) points, using quintic method. */

static void GA_interp_quintic( MRI_IMAGE *fim ,
                               int npp, float *ip, float *jp, float *kp, float *vv )
{
   int nx=fim->nx , ny=fim->ny , nz=fim->nz , nxy=nx*ny , pp ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float fx,fy,fz ;
   float *far = MRI_FLOAT_PTR(fim) ;
   int nx1=nx-1,ny1=ny-1,nz1=nz-1, ix,jy,kz ;
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

ENTRY("GA_interp_quintic") ;

   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ vv[pp]=outval; continue; }
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ vv[pp]=outval; continue; }
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ vv[pp]=outval; continue; }

     ix = floorf(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floorf(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floorf(zz) ;  fz = zz - kz ;

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

     vv[pp] =  wt_m2 * f_km2 + wt_m1 * f_km1 + wt_00 * f_k00
             + wt_p1 * f_kp1 + wt_p2 * f_kp2 + wt_p3 * f_kp3 ;
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

/* Macro to periodically reduce a variable into the range 0..1:
   for example: PRED01(1.2) == 0.8, PRED01(1.8) == 0.2, et cetera;
   graphically
               PRED01(x)|
                        | /\      /\      /\      /\      /\
                        |/  \    /  \    /  \    /  \    /
                        |    \  /    \  /    \  /    \  /
                        |     \/      \/      \/      \/
                        +------------------------------------> x
                          -3  -2  -1   0  +1  +2  +3  +4  +5
*/

#undef  PRED01
#define PRED01(x) fabsf( (x) - 2.0f*floorf(0.5f*((x)+1.0f)) )

/* Max number of points to warp at a time */

#undef  NPER
#define NPER 4096

/*--------------------------------------------------------------------*/
/*! Interpolate target image to control points in base image space.
    - Results go into avm, which must be pre-allocated.
    - If mpar==NULL, then warp parameters are all taken from gstup and
      the results are calculated at ALL points in the base image.
----------------------------------------------------------------------*/

static void GA_get_warped_values( int nmpar , double *mpar , float *avm )
{
   int    npar , ii,jj,kk,qq,pp,npp,mm,nx,ny,nxy , clip=0 , npt ;
   float *wpar , v ;
   float *imf , *jmf , *kmf ;
   float *imw , *jmw , *kmw ;
   MRI_IMAGE *aim ;

ENTRY("GA_get_warped_values") ;

   npar = gstup->wfunc_numpar ;
   wpar = (float *)calloc(sizeof(float),npar) ;

   /* load ALL the warping parameters, including the fixed values */

   if( mpar != NULL ){                              /* load from input array */
     for( ii=pp=0 ; ii < npar ; ii++ ){
       if( gstup->wfunc_param[ii].fixed ){                    /* fixed param */
         wpar[ii] = gstup->wfunc_param[ii].val_fixed ;
       } else {                                            /* variable param */
         v = (float)mpar[pp++] ;
         wpar[ii] = gstup->wfunc_param[ii].min        /* scale to true range */
                   +gstup->wfunc_param[ii].siz * PRED01(v) ;
       }
     }
   } else {                                     /* load directly from struct */
     for( ii=0 ; ii < gstup->wfunc_numpar ; ii++ )
       wpar[ii] = gstup->wfunc_param[ii].val_out ;
   }

   /* create space for unwarped indexes, if none given */

   if( mpar == NULL || gstup->im == NULL ){
     imf = (float *)calloc(sizeof(float),NPER) ;
     jmf = (float *)calloc(sizeof(float),NPER) ;
     kmf = (float *)calloc(sizeof(float),NPER) ;
     npt = gstup->bsim->nvox ;
   } else {
     npt = gstup->npt_match ;
   }

   /* create space for indexes of warped control points */

   imw = (float *)calloc(sizeof(float),NPER) ;
   jmw = (float *)calloc(sizeof(float),NPER) ;
   kmw = (float *)calloc(sizeof(float),NPER) ;

   nx = gstup->bsim->nx; ny = gstup->bsim->ny; nxy = nx*ny;

   /* send parameters to warping function for its setup */

   gstup->wfunc( npar , wpar , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;

   /* choose image from which to extract data */

   aim = (gstup->ajims != NULL && mpar != NULL ) ? gstup->ajims /* smoothed */
                                                 : gstup->ajim; /* unsmooth */

   /*--- do (up to) NPER points at a time ---*/

   for( pp=0 ; pp < npt ; pp+=NPER ){

     npp = MIN( NPER , npt-pp ) ;  /* number to do in this iteration */

     if( mpar == NULL || gstup->im == NULL ){  /* do all points */
       for( qq=0 ; qq < npp ; qq++ ){
         mm = pp+qq ;
         ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
         imf[qq] = (float)ii; jmf[qq] = (float)jj; kmf[qq] = (float)kk;
       }
     } else {
       imf = gstup->im->ar + pp ;  /* pointers to control points */
       jmf = gstup->jm->ar + pp ;
       kmf = gstup->km->ar + pp ;
     }

     /****-- warp control points to new locations ---****/
     /**** (warp does index-to-index transformation) ****/

     gstup->wfunc( npar , NULL ,
                   npp  , imf,jmf,kmf , imw,jmw,kmw ) ;

     /* interpolate target image at warped points */

     switch( gstup->interp_code ){
       case MRI_NN:
         GA_interp_NN( aim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;

       case MRI_LINEAR:
         GA_interp_linear( aim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;

       case MRI_CUBIC:
         clip = 1 ;
         GA_interp_cubic( aim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;

       default:        /* for higher order methods not implemented here */
       case MRI_QUINTIC:
         clip = 1 ;
         GA_interp_quintic( aim , npp , imw,jmw,kmw , avm+pp ) ;
       break ;
     }

   } /* end of loop over matching points */

   /* free the enslaved memory */

   free((void *)kmw); free((void *)jmw); free((void *)imw);
   if( mpar == NULL || gstup->im == NULL ){
     free((void *)kmf); free((void *)jmf); free((void *)imf);
   }
   free((void *)wpar) ;

   /* clip interpolated values to range of target image, if need be */

   if( clip ){
     float bb=gstup->ajbot , tt=gstup->ajtop ;
     for( pp=0 ; pp < npt ; pp++ )
            if( avm[pp] < bb ) avm[pp] = bb ;
       else if( avm[pp] > tt ) avm[pp] = tt ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Stuff for calling a user-supplied function every time the cost
   function is smaller than the previous minimal value (vbest).
   GA_fitter_dotter() is a simple example.
-----------------------------------------------------------------------------*/

static float fit_vbest = BIGVAL ;
static void (*fit_callback)(int,double *) = NULL ;

void GA_reset_fit_callback( void (*fc)(int,double*) )  /* user func is fc */
{
   fit_vbest = BIGVAL ; fit_callback = fc ; return ;
}

void GA_fitter_dotter(int n, double *mpar){ printf("."); fflush(stdout); }

void GA_do_dots(int x){ GA_reset_fit_callback( (x)?GA_fitter_dotter:NULL ); }

/*---------------------------------------------------------------------------*/
void GA_fitter_coster(int n, double *mpar){
  printf(" + Cost=%g\r",fit_vbest); fflush(stdout);
}
void GA_fitter_coster_tab(int n, double *mpar){
  printf(" + Cost=%g\t",fit_vbest); fflush(stdout);
}
void GA_do_cost(int x, byte use_tab){
   if (use_tab) {
      GA_reset_fit_callback( (x)?GA_fitter_coster_tab:NULL );
   } else {
      GA_reset_fit_callback( (x)?GA_fitter_coster:NULL );
   }
}

void GA_fitter_params( int n , double *mpar )
{
  int ii , pp ; double wpar , v ; int npar = gstup->wfunc_numpar ;
  printf(" + Cost=%g Param=",fit_vbest) ;
  for( ii=pp=0 ; ii < npar ; ii++ ){
    if( !gstup->wfunc_param[ii].fixed ){  /* variable param */
      v = mpar[pp++] ;
      wpar = gstup->wfunc_param[ii].min        /* scale to true range */
            +gstup->wfunc_param[ii].siz * PRED01(v) ;
      printf("%g ",wpar) ;
    }
  }
  printf("\n") ; fflush(stdout) ;
}
void GA_do_params( int x ){
   GA_reset_fit_callback( (x)?GA_fitter_params:NULL );
}


/*---------------------------------------------------------------------------*/

static void GA_setup_2Dhistogram( float *xar , float *yar )  /* 08 May 2007 */
{
ENTRY("GA_setup_2Dhistogram") ;

   switch( gstup->hist_mode ){

     default:
     case GA_HIST_EQWIDE:
       set_2Dhist_xybin( 0,NULL,NULL ) ;
     break ;

     case GA_HIST_CLEQWD:{
       int nbin=(int)gstup->hist_param , npt=gstup->npt_match ;
       float xbc,xtc , ybc,ytc ;

       if( nbin < 3 ) nbin = 0 ;
       set_2Dhist_hbin( nbin ) ;
       set_2Dhist_xyclip( npt , xar , yar ) ;

       if( verb > 1 ){
         (void)get_2Dhist_xyclip( &xbc,&xtc , &ybc,&ytc ) ;
         ININFO_message(" - histogram: source clip %g .. %g; base clip %g .. %g",
                        xbc,xtc , ybc,ytc ) ;
         ININFO_message(" - versus source range %g .. %g; base range %g .. %g",
                        gstup->ajbot, gstup->ajclip, gstup->bsbot, gstup->bsclip ) ;
       }
     }
     break ;

     case GA_HIST_EQHIGH:{
       int nbin=(int)gstup->hist_param , npt=gstup->npt_match , ii,dm,mm,nnew ;
       float *xx , *yy ;

       if( npt > 666*nbin ){                /* subsample data to save CPU time */
         dm = find_relprime_fixed( npt ) ;
         mm = 1 ; nnew = (int)(314.1593*nbin) ;
         xx = (float *)malloc(sizeof(float)*nnew) ;
         yy = (float *)malloc(sizeof(float)*nnew) ;
         for( ii=0 ; ii < nnew ; ii++,mm=(mm+dm)%npt ){
           xx[ii] = xar[mm] ; yy[ii] = yar[mm] ;
         }
         npt = nnew ;
       } else {                             /* just use all the data */
         xx = xar ; yy = yar ;
       }

       if( verb > 1 )
         ININFO_message("- setting up equalized histogram bins with %d pts",npt) ;

       set_2Dhist_xybin_eqhigh( nbin , npt , xx , yy ) ;
       if( xx != xar ){ free(yy); free(xx); }

       if( verb > 1 ){
         nbin = get_2Dhist_xybin( &xx , &yy ) ;
         ININFO_message("-- %d equalized histogram bins for source follow:",nbin) ;
         fprintf(stderr,"    ") ;
         for( ii=0 ; ii <= nbin ; ii++ ) fprintf(stderr," %g",xx[ii]) ;
         fprintf(stderr,"\n") ;
         ININFO_message("-- %d equalized histogram bins for base follow:",nbin) ;
         fprintf(stderr,"    ") ;
         for( ii=0 ; ii <= nbin ; ii++ ) fprintf(stderr," %g",yy[ii]) ;
         fprintf(stderr,"\n") ;
       }
     }
     break ;
   }

   gstup->need_hist_setup = 0 ; EXRETURN ;
}

/*===========================================================================*/
/*--- Stuff for storing sub-bloks of data points for localized cost funcs ---*/

typedef struct { int num , *nelm , **elm ; } GA_BLOK_set ;

#undef  FAS
#define FAS(a,s) ( (a) <= (s) && (a) >= -(s) )

#define GA_BLOK_inside_ball(a,b,c,siz) \
  ( ((a)*(a)+(b)*(b)+(c)*(c)) <= (siz) )

#define GA_BLOK_inside_cube(a,b,c,siz) \
  ( FAS((a),(siz)) && FAS((b),(siz)) && FAS((c),(siz)) )

#define GA_BLOK_inside_rhdd(a,b,c,siz)              \
  ( FAS((a)+(b),(siz)) && FAS((a)-(b),(siz)) &&     \
    FAS((a)+(c),(siz)) && FAS((a)-(c),(siz)) &&     \
    FAS((b)+(c),(siz)) && FAS((b)-(c),(siz))   )

#define GA_BLOK_inside(bt,a,b,c,s)                              \
 (  ((bt)==GA_BLOK_BALL) ? GA_BLOK_inside_ball((a),(b),(c),(s)) \
  : ((bt)==GA_BLOK_CUBE) ? GA_BLOK_inside_cube((a),(b),(c),(s)) \
  : ((bt)==GA_BLOK_RHDD) ? GA_BLOK_inside_rhdd((a),(b),(c),(s)) \
  : 0 )

#define GA_BLOK_ADDTO_intar(nar,nal,ar,val)                                 \
 do{ if( (nar) == (nal) ){                                                  \
       (nal) = 1.2*(nal)+16; (ar) = (int *)realloc((ar),sizeof(int)*(nal)); \
     }                                                                      \
     (ar)[(nar)++] = (val);                                                 \
 } while(0)

#define GA_BLOK_CLIP_intar(nar,nal,ar)                               \
 do{ if( (nar) < (nal) && (nar) > 0 ){                               \
       (nal) = (nar); (ar) = (int *)realloc((ar),sizeof(int)*(nal)); \
 }} while(0)

#define GA_BLOK_KILL(gbs)                                            \
 do{ int ee ;                                                        \
     if( (gbs)->nelm != NULL ) free((gbs)->nelm) ;                   \
     if( (gbs)->elm != NULL ){                                       \
       for( ee=0 ; ee < (gbs)->num ; ee++ )                          \
         if( (gbs)->elm[ee] != NULL ) free((gbs)->elm[ee]) ;         \
       free((gbs)->elm) ;                                            \
     }                                                               \
     free((gbs)) ;                                                   \
 } while(0)

/*----------------------------------------------------------------------------*/
/*! Fill a struct with list of points contained in sub-bloks of the base. */

static GA_BLOK_set * create_GA_BLOK_set( int   nx , int   ny , int   nz ,
                                         float dx , float dy , float dz ,
                                         int npt , float *im, float *jm, float *km ,
                                         int bloktype , float blokrad , int minel )
{
   GA_BLOK_set *gbs ;
   float dxp,dyp,dzp , dxq,dyq,dzq , dxr,dyr,dzr , xt,yt,zt ;
   float xx,yy,zz , uu,vv,ww , siz ;
   THD_mat33 latmat , invlatmat ; THD_fvec3 pqr , xyz ;
   int pb,pt , qb,qt , rb,rt , pp,qq,rr , nblok,nball , ii , nxy ;
   int aa,bb,cc , dd,ss , np,nq,nr,npq , *nelm,*nalm,**elm , ntot,nsav,ndup ;

ENTRY("create_GA_BLOK_set") ;

   if( nx < 3 || ny < 3 || nz < 1 ) RETURN(NULL) ;
   if( dx <= 0.0f ) dx = 1.0f ;
   if( dy <= 0.0f ) dy = 1.0f ;
   if( dz <= 0.0f ) dz = 1.0f ;

   if( npt <= 0 || im == NULL || jm == NULL || km == NULL ){
     im = jm = km = NULL ; npt = 0 ;
   }

   /* mark type of blok being stored */

   /* Create lattice vectors to generate translated bloks:
      The (p,q,r)-th blok -- for integral p,q,r -- is at (x,y,z) offset
        (dxp,dyp,dzp)*p + (dxq,dyq,dzq)*q + (dxr,dyr,dzr)*r
      Also set the 'siz' parameter for the blok, to test for inclusion. */

   switch( bloktype ){

     /* balls go on a hexagonal close packed lattice,
        but with lattice spacing reduced to avoid gaps
        (of course, then the balls overlap -- c'est la geometrie) */

     case GA_BLOK_BALL:{
       float s3=1.73205f ,           /* sqrt(3) */
             s6=2.44949f ,           /* sqrt(6) */
             a =blokrad*0.866025f ;  /* shrink spacing to avoid gaps */
       siz = blokrad*blokrad ;
       /* hexagonal close packing basis vectors for sphere of radius a */
       dxp = 2.0f * a ; dyp = 0.0f  ; dzp = 0.0f             ;
       dxq = a        ; dyq = a * s3; dzq = 0.0f             ;
       dxr = a        ; dyr = a / s3; dzr = a * 0.666667f*s6 ;
     }
     break ;

     /* cubes go on a simple cubical lattice, spaced so faces touch */

     case GA_BLOK_CUBE:{
       float a =  blokrad ;
       siz = a ;
       dxp = 2*a ; dyp = 0.0f; dzp = 0.0f ;
       dxq = 0.0f; dyq = 2*a ; dzq = 0.0f ;
       dxr = 0.0f; dyr = 0.0f; dzr = 2*a  ;
     }
     break ;

     /* rhombic dodecahedra go on their own hexagonal lattice,
        spaced so that faces touch (i.e., no volumetric overlap) */

     case GA_BLOK_RHDD:{
       float a = blokrad ;
       siz = a ;
       dxp = a   ; dyp = a   ; dzp = 0.0f ;
       dxq = 0.0f; dyq = a   ; dzq = a    ;
       dxr = a   ; dyr = 0.0f; dzr = a    ;
     }
     break ;

     default:  RETURN(NULL) ;  /** should not happen! **/
   }

   /* find range of (p,q,r) indexes needed to cover volume,
      by checking out all 7 corners besides (0,0,0) (where p=q=r=0) */

   LOAD_MAT( latmat, dxp , dxq , dxr ,
                     dyp , dyq , dyr ,
                     dzp , dzq , dzr  ) ; invlatmat = MAT_INV(latmat) ;

   xt = (nx-1)*dx ; yt = (ny-1)*dy ; zt = (nz-1)*dz ;
   pb = pt = qb = qt = rb = rt = 0 ;  /* initialize (p,q,r) bot, top values */

   LOAD_FVEC3(xyz , xt,0.0f,0.0f ); pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   LOAD_FVEC3(xyz , xt,yt,0.0f )  ; pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   LOAD_FVEC3(xyz , xt,0.0f,zt )  ; pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   LOAD_FVEC3(xyz , xt,yt,zt )    ; pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   LOAD_FVEC3(xyz , 0.0f,yt,0.0f ); pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   LOAD_FVEC3(xyz , 0.0f,0.0f,zt ); pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   LOAD_FVEC3(xyz , 0.0f,yt,zt )  ; pqr = MATVEC( invlatmat , xyz ) ;
   pp = (int)floorf( pqr.xyz[0] ) ; pb = MIN(pb,pp) ; pp++ ; pt = MAX(pt,pp) ;
   qq = (int)floorf( pqr.xyz[1] ) ; qb = MIN(qb,qq) ; qq++ ; qt = MAX(qt,qq) ;
   rr = (int)floorf( pqr.xyz[2] ) ; rb = MIN(rb,rr) ; rr++ ; rt = MAX(rt,rr) ;

   /* Lattice index range is (p,q,r) = (pb..pt,qb..qt,rb..rt) inclusive */

   np = pt-pb+1 ;                /* number of p values to consider */
   nq = qt-qb+1 ; npq = np*nq ;
   nr = rt-rb+1 ;
   nblok = npq*nr ;              /* total number of bloks to consider */

   /* Now have list of bloks, so put points into each blok list */

   nelm = (int *) calloc(sizeof(int)  ,nblok) ;  /* # pts in each blok */
   nalm = (int *) calloc(sizeof(int)  ,nblok) ;  /* # malloc-ed in each blok */
   elm  = (int **)calloc(sizeof(int *),nblok) ;  /* list of pts in each blok */

   nxy = nx*ny ; if( npt == 0 ) npt = nxy*nz ;

   for( ndup=ntot=ii=0 ; ii < npt ; ii++ ){
     if( im != NULL ){
       pp = (int)im[ii]; qq = (int)jm[ii]; rr = (int)km[ii]; /* xyz indexes */
     } else {
       pp = ii%nx ; rr = ii/nxy ; qq = (ii-rr*nxy)/nx ;
     }
     ss = ii ; /* index in 1D array */
     xx = pp*dx ; yy = qq*dy ; zz = rr*dz ; /* xyz spatial coordinates */
     LOAD_FVEC3( xyz , xx,yy,zz ) ;
     pqr = MATVEC( invlatmat , xyz ) ;      /* float lattice coordinates */
     pp = (int)floorf(pqr.xyz[0]+.499f) ;   /* integer lattice coords */
     qq = (int)floorf(pqr.xyz[1]+.499f) ;
     rr = (int)floorf(pqr.xyz[2]+.499f) ; nsav = 0 ;
     for( cc=rr-1 ; cc <= rr+1 ; cc++ ){    /* search nearby bloks */
       if( cc < rb || cc > rt ) continue ;  /* for inclusion of (xx,yy,zz) */
       for( bb=qq-1 ; bb <= qq+1 ; bb++ ){
         if( bb < qb || bb > qt ) continue ;
         for( aa=pp-1 ; aa <= pp+1 ; aa++ ){
           if( aa < pb || aa > pt ) continue ;
           LOAD_FVEC3( pqr , aa,bb,cc ) ;  /* compute center of this */
           xyz = MATVEC( latmat , pqr ) ;  /* blok into xyz vector  */
           uu = xx - xyz.xyz[0] ;    /* xyz coords relative to blok center */
           vv = yy - xyz.xyz[1] ;
           ww = zz - xyz.xyz[2] ;
           if( GA_BLOK_inside( bloktype , uu,vv,ww , siz ) ){
             dd = (aa-pb) + (bb-qb)*np + (cc-rb)*npq ; /* blok index */
             GA_BLOK_ADDTO_intar( nelm[dd], nalm[dd], elm[dd], ss ) ;
             ntot++ ; nsav++ ;
           }
         }
       }
     }
     if( nsav > 1 ) ndup++ ;
   }

   if( minel < 9 ){
     for( minel=dd=0 ; dd < nblok ; dd++ ) minel = MAX(minel,nelm[dd]) ;
     minel = (int)(0.456*minel)+1 ;
   }

   /* now cast out bloks that have too few points,
      and truncate those arrays that pass the threshold */

   for( nsav=dd=0 ; dd < nblok ; dd++ ){
     if( nelm[dd] < minel ){
       if( elm[dd] != NULL ){ free(elm[dd]); elm[dd] = NULL; }
       nelm[dd] = 0 ;
     } else {
       GA_BLOK_CLIP_intar( nelm[dd] , nalm[dd] , elm[dd] ) ; nsav++ ;
     }
   }
   free(nalm) ;

   if( nsav == 0 ){  /* didn't find any arrays to keep!? */
     ERROR_message("create_GA_BLOK_set can't get bloks with %d elements",minel);
     free(nelm) ; free(elm) ; RETURN(NULL) ;
   }

   /* create output struct */

   gbs = (GA_BLOK_set *)malloc(sizeof(GA_BLOK_set)) ;
   gbs->num  = nsav ;
   gbs->nelm = (int *) calloc(sizeof(int)  ,nsav) ;
   gbs->elm  = (int **)calloc(sizeof(int *),nsav) ;
   for( ntot=nsav=dd=0 ; dd < nblok ; dd++ ){
     if( nelm[dd] > 0 && elm[dd] != NULL ){
       gbs->nelm[nsav] = nelm[dd] ; ntot += nelm[dd] ;
       gbs->elm [nsav] = elm[dd]  ; nsav++ ;
     }
   }
   free(nelm) ; free(elm) ;

   if( verb > 1 )
     ININFO_message("%d total points stored in %d '%s(%g)' bloks",
                    ntot , gbs->num , GA_BLOK_STRING(bloktype) , blokrad ) ;

   RETURN(gbs) ;
}

/*---------------------------------------------------------------------------*/

float GA_pearson_local( int npt , float *avm, float *bvm, float *wvm )
{
   GA_BLOK_set *gbs ;
   int nblok , nelm , *elm , dd , ii,jj , nm ;
   float xv,yv,xy,xm,ym,vv,ww,ws , pcor , wt , psum=0.0f ;

   if( gstup->blokset == NULL ){
     float rad=gstup->blokrad , mrad ;
     if( gstup->smooth_code > 0 && gstup->smooth_radius_base > 0.0f )
       rad = sqrt( rad*rad +SQR(gstup->smooth_radius_base) ) ;
     mrad = 1.2345f*(gstup->base_di + gstup->base_dj + gstup->base_dk) ;
     rad  = MAX(rad,mrad) ;
     gstup->blokset = (void *)create_GA_BLOK_set(
                                gstup->bsim->nx, gstup->bsim->ny, gstup->bsim->nz,
                                gstup->base_di , gstup->base_dj , gstup->base_dk ,
                                gstup->npt_match ,
                                 gstup->im->ar , gstup->jm->ar , gstup->km->ar ,
                                gstup->bloktype , rad , gstup->blokmin ) ;
     if( gstup->blokset == NULL )
       ERROR_exit("Can't create GA_BLOK_set?!?") ;
   }

   gbs = (GA_BLOK_set *)gstup->blokset ;
   nblok = gbs->num ; nm = gstup->npt_match ;
   if( nblok < 1 ) ERROR_exit("Bad GA_BLOK_set?!") ;

   for( dd=0 ; dd < nblok ; dd++ ){
     nelm = gbs->nelm[dd] ; if( nelm < 9 ) continue ;
     elm = gbs->elm[dd] ;

     if( wvm == NULL ){   /*** unweighted correlation ***/
       xv=yv=xy=xm=ym=0.0f ; /*** ws = nelm ; ***/
       for( ii=0 ; ii < nelm ; ii++ ){
         jj = elm[ii] ;
         xm += avm[jj] ; ym += bvm[jj] ;
       }
       xm /= nelm ; ym /= nelm ;
       for( ii=0 ; ii < nelm ; ii++ ){
         jj = elm[ii] ;
         vv = avm[jj]-xm ; ww = bvm[jj]-ym ;
         xv += vv*vv ; yv += ww*ww ; xy += vv*ww ;
       }

     } else {             /*** weighted correlation ***/
       xv=yv=xy=xm=ym=ws=0.0f ;
       for( ii=0 ; ii < nelm ; ii++ ){
         jj = elm[ii] ;
         wt = wvm[jj] ; ws += wt ;
         xm += avm[jj]*wt ; ym += bvm[jj]*wt ;
       }
       xm /= ws ; ym /= ws ;
       for( ii=0 ; ii < nelm ; ii++ ){
         jj = elm[ii] ;
         wt = wvm[jj] ; vv = avm[jj]-xm ; ww = bvm[jj]-ym ;
         xv += wt*vv*vv ; yv += wt*ww*ww ; xy += wt*vv*ww ;
       }
     }

     if( xv <= 0.0f || yv <= 0.0f ) continue ;      /* skip this blok */
     pcor = xy/sqrtf(xv*yv) ;                       /* correlation */
     if( pcor > 1.0f ) pcor = 1.0f; else if( pcor < -1.0f ) pcor = -1.0f;
     pcor = logf( (1.0001f+pcor)/(1.0001f-pcor) ) ; /* 2*arctanh() */
     psum += pcor * fabsf(pcor) ;                   /* emphasize large values */
   }

   return (0.25f*psum/nblok) ; /* averaged stretched emphasized correlations */
}
/*======================== End of BLOK-iness functionality ==================*/

/*---------------------------------------------------------------------------*/
/*! Compute a particular fit cost function
    - avm = target image values warped to base
    - bvm = base image values
    - wvm = weight image values
-----------------------------------------------------------------------------*/

static double GA_scalar_costfun( int meth , int npt ,
                                 float *avm , float *bvm , float *wvm )
{
  double val=0.0f ;

ENTRY("GA_scalar_costfun") ;

  switch( meth ){

    default:
    case GA_MATCH_PEARSON_SCALAR:   /* Pearson correlation coefficient */
      val = (double)THD_pearson_corr_wt( gstup->npt_match, avm, bvm,wvm ) ;
      val = 1.0 - fabs(val) ;
    break ;

    case GA_MATCH_PEARSON_SIGNED:
      val = (double)THD_pearson_corr_wt( gstup->npt_match, avm, bvm,wvm ) ;
    break ;

    case GA_MATCH_PEARSON_LOCALS:
      val = (double)GA_pearson_local( gstup->npt_match, avm, bvm,wvm ) ;
    break ;

    case GA_MATCH_PEARSON_LOCALA:
      val = (double)GA_pearson_local( gstup->npt_match, avm, bvm,wvm ) ;
      val = 1.0 - fabs(val) ;
    break ;

    case GA_MATCH_SPEARMAN_SCALAR:  /* rank-order (Spearman) correlation */
      val = (double)THD_spearman_corr_nd( gstup->npt_match , avm,bvm ) ;
      val = 1.0 - fabs(val) ;
    break ;

    case GA_MATCH_KULLBACK_SCALAR:  /* AKA Mutual Information */
      val = -THD_mutual_info_scl( gstup->npt_match ,
                                  gstup->ajbot , gstup->ajclip , avm ,
                                  gstup->bsbot , gstup->bsclip , bvm , wvm ) ;
    break ;

    case GA_MATCH_NORMUTIN_SCALAR:  /* Normalized Mutual Information */
      val = THD_norm_mutinf_scl( gstup->npt_match ,
                                 gstup->ajbot , gstup->ajclip , avm ,
                                 gstup->bsbot , gstup->bsclip , bvm , wvm ) ;
    break ;

    case GA_MATCH_JOINTENT_SCALAR:  /* Joint Entropy */
      val = THD_jointentrop_scl( gstup->npt_match ,
                                 gstup->ajbot , gstup->ajclip , avm ,
                                 gstup->bsbot , gstup->bsclip , bvm , wvm ) ;
    break ;

    case GA_MATCH_CRAT_USYM_SCALAR: /* Correlation ratio (various flavors) */
    case GA_MATCH_CRAT_SADD_SCALAR:
    case GA_MATCH_CORRATIO_SCALAR:
           if( meth==GA_MATCH_CRAT_USYM_SCALAR )THD_corr_ratio_sym_not;
      else if( meth==GA_MATCH_CRAT_SADD_SCALAR )THD_corr_ratio_sym_add;
      else                                      THD_corr_ratio_sym_mul;

#if 0
      if( verb > 8 )
       INFO_message("THD_corr_ratio_scl(%d,%g,%g,bvm,%g,%g,bvm,wvm)",
                     gstup->npt_match ,
                     gstup->bsbot , gstup->bsclip ,
                     gstup->ajbot , gstup->ajclip  ) ;
#endif

      val = THD_corr_ratio_scl( gstup->npt_match ,
                                gstup->bsbot , gstup->bsclip , bvm ,
                                gstup->ajbot , gstup->ajclip , avm , wvm ) ;
      val = 1.0 - fabs(val) ;
    break ;

    case GA_MATCH_HELLINGER_SCALAR: /* Hellinger metric */
      val = -THD_hellinger_scl( gstup->npt_match ,
                                gstup->ajbot , gstup->ajclip , avm ,
                                gstup->bsbot , gstup->bsclip , bvm , wvm ) ;
    break ;
  }

  if( !finite(val) ) val = BIGVAL ;
  RETURN( val );
}

/*---------------------------------------------------------------------------*/
/*! Fit metric for matching base and target image value pairs.
    (Smaller is a better match.)  For use as a NEWUOA optimization function.
-----------------------------------------------------------------------------*/

static double GA_scalar_fitter( int npar , double *mpar )
{
  double val ;
  float *avm , *bvm , *wvm ;

ENTRY("GA_scalar_fitter") ;

  avm = (float *)calloc(gstup->npt_match,sizeof(float)) ; /* target points at */
  GA_get_warped_values( npar , mpar , avm ) ;             /* warped locations */

  bvm = gstup->bvm->ar ;                                  /* base points */
  wvm = (gstup->wvm != NULL) ? gstup->wvm->ar : NULL ;    /* weights */

  if( gstup->need_hist_setup ) GA_setup_2Dhistogram( avm , bvm ) ;

  val = GA_scalar_costfun( gstup->match_code, gstup->npt_match, avm,bvm,wvm ) ;

  free((void *)avm) ;    /* toss the trash */
  RETURN(val);
}

/*---------------------------------------------------------------------------*/

void mri_genalign_scalar_clrwght( GA_setup *stup )  /* 18 Oct 2006 */
{
ENTRY("mri_genalign_scalar_clrwght") ;
  if( stup != NULL ){
    if( stup->bwght != NULL ) mri_free(stup->bwght) ;
    if( stup->bmask != NULL ) free((void *)stup->bmask) ;
    stup->nmask = stup->nvox_mask = 0 ;
    stup->bwght = NULL ; stup->bmask = NULL ;
  }
  EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Macro for exiting mri_genalign_scalar_setup() with extreme prejudice.     */

#undef  ERREX
#define ERREX(s) \
 do{ ERROR_message("mri_genalign_scalar_setup: %s",(s)); EXRETURN; } while(0)

/*---------------------------------------------------------------------------*/
/*! Setup for generic alignment of scalar images.
    - If this is a new alignment, images basim and targim must be input;
      wghtim is optional.
    - If this is a continuation of a previously started alignment
      with modified parameters (e.g., smoothing method/radius), then image
      copies are already stored in stup and don't need to be resupplied.
    - cf. 3dAllineate.c for use of this function!
-----------------------------------------------------------------------------*/

void mri_genalign_scalar_setup( MRI_IMAGE *basim  , MRI_IMAGE *wghtim ,
                                MRI_IMAGE *targim , GA_setup  *stup    )

{
   int qq , rr , nx,ny,nz,nxy , mm,ii,jj,kk , qdim ;
   int use_all=0 , need_pts=0 , nmatch ;
   int need_smooth_base , do_smooth_base ;
   int need_smooth_targ , do_smooth_targ ;
   float *bsar ;

ENTRY("mri_genalign_scalar_setup") ;

   /* basic checks of input for rationality:
      - Must have stup struct (setup parameters)
      - Must have new base image (basim) or have it previously stored in stup
      - Must have new target image (targim) or have previously stored version */

   if( verb > 1 ) ININFO_message("* Enter alignment setup routine") ;

   if( stup == NULL ) ERREX("stup is NULL") ;
   stup->setup = 0 ;  /* mark stup struct as not being ready yet */

   if( basim  == NULL && stup->bsim == NULL ) ERREX("basim is NULL") ;
   if( targim == NULL && stup->ajim == NULL ) ERREX("targim is NULL") ;

   /* check dimensionality of input images (2D or 3D) */

   if( basim != NULL ){
     qdim = mri_dimensionality(basim) ;
     if( qdim < 2 || qdim > 3 )
       ERREX("basim dimensionality is not 2 or 3") ;
   } else {
     qdim = mri_dimensionality(stup->bsim) ;
   }
   stup->abdim = qdim ;

   if( targim != NULL ){
     if( qdim != mri_dimensionality(targim) )
       ERREX("basim & targim dimensionalities differ") ;
   }

   if( stup->wfunc_numpar < 1 || stup->wfunc==NULL || stup->wfunc_param==NULL )
     ERREX("illegal wfunc parameters") ;

   stup->dim_avec = stup->dim_bvec = 1 ;  /* scalars */

   /** copy new images into setup struct **/

   if( basim != NULL ){
     need_pts = 1 ;              /* will need to extract match points */
     if( stup->bsim != NULL ) mri_free(stup->bsim) ;
     if( verb > 1 ) ININFO_message("- copying base image") ;
     stup->bsim = mri_to_float(basim) ;
     if( stup->bsim->dx <= 0.0f ) stup->bsim->dx = 1.0f ;
     if( stup->bsim->dy <= 0.0f ) stup->bsim->dy = 1.0f ;
     if( stup->bsim->dz <= 0.0f ) stup->bsim->dz = 1.0f ;

     if( !ISVALID_MAT44(stup->base_cmat) ){
       LOAD_DIAG_MAT44( stup->base_cmat ,
                        stup->bsim->dx , stup->bsim->dy , stup->bsim->dz ) ;
       LOAD_MAT44_VEC( stup->base_cmat ,
                       -(stup->bsim->nx-1)*0.5f*stup->bsim->dx ,
                       -(stup->bsim->ny-1)*0.5f*stup->bsim->dy ,
                       -(stup->bsim->nz-1)*0.5f*stup->bsim->dz  ) ;
     }
     stup->base_imat = MAT44_INV( stup->base_cmat ) ;
     stup->base_di = MAT44_COLNORM(stup->base_cmat,0) ;  /* 22 Aug 2007 */
     stup->base_dj = MAT44_COLNORM(stup->base_cmat,1) ;
     stup->base_dk = MAT44_COLNORM(stup->base_cmat,2) ;
     if( stup->bsims != NULL ){ mri_free(stup->bsims); stup->bsims = NULL; }
   }
   nx = stup->bsim->nx; ny = stup->bsim->ny; nz = stup->bsim->nz; nxy = nx*ny;

   if( targim != NULL ){
     if( stup->ajim != NULL ) mri_free(stup->ajim) ;
     if( verb > 1 ) ININFO_message("- copying source image") ;
     stup->ajim = mri_to_float(targim) ;
     if( stup->ajim->dx <= 0.0f ) stup->ajim->dx = 1.0f ;
     if( stup->ajim->dy <= 0.0f ) stup->ajim->dy = 1.0f ;
     if( stup->ajim->dz <= 0.0f ) stup->ajim->dz = 1.0f ;

     if( !ISVALID_MAT44(stup->targ_cmat) ){
       LOAD_DIAG_MAT44( stup->targ_cmat ,
                        stup->ajim->dx , stup->ajim->dy , stup->ajim->dz ) ;
       LOAD_MAT44_VEC( stup->targ_cmat ,
                       -(stup->ajim->nx-1)*0.5f*stup->ajim->dx ,
                       -(stup->ajim->ny-1)*0.5f*stup->ajim->dy ,
                       -(stup->ajim->nz-1)*0.5f*stup->ajim->dz  ) ;
     }
     stup->targ_imat = MAT44_INV( stup->targ_cmat ) ;
     stup->targ_di = MAT44_COLNORM(stup->targ_cmat,0) ;  /* 22 Aug 2007 */
     stup->targ_dj = MAT44_COLNORM(stup->targ_cmat,1) ;
     stup->targ_dk = MAT44_COLNORM(stup->targ_cmat,2) ;
     if( stup->ajims != NULL ){ mri_free(stup->ajims); stup->ajims = NULL; }

     /* 07 Aug 2007: deal with target mask */

     if( stup->ajimor != NULL ){ mri_free(stup->ajimor); stup->ajimor = NULL; }

     if( stup->ajmask != NULL && stup->ajmask->nvox != stup->ajim->nvox ){
       WARNING_message("mri_genalign_scalar_setup: target image/mask mismatch") ;
       mri_free(stup->ajmask) ; stup->ajmask = NULL ;
     }
     if( stup->ajmask != NULL ){
       float *af, *qf ; byte *mmm ; float_pair quam ; float ubot,usiz , u1,u2;
       MRI_IMAGE *qim ; int nvox,pp ;
       stup->ajimor = mri_copy(stup->ajim) ;
       if( stup->usetemp ) mri_purge( stup->ajimor ) ;
       af  = MRI_FLOAT_PTR(stup->ajim) ;
       mmm = MRI_BYTE_PTR (stup->ajmask) ;
       qim = mri_new_conforming(stup->ajim,MRI_float);
       qf  = MRI_FLOAT_PTR(qim); nvox = qim->nvox;
       for( ii=pp=0 ; ii < nvox ; ii++ ){ if( mmm[ii] ) qf[pp++] = af[ii] ; }
       qim->nvox = pp; quam = mri_twoquantiles(qim,0.05f,0.95f); mri_free(qim);
       ubot = quam.a ; usiz = (quam.b - quam.a)*0.5f ;
       if( usiz > 0.0f ){
         if( verb > 2 ) ININFO_message("source mask: ubot=%g usiz=%g",ubot,usiz);
         for( ii=0 ; ii < nvox ; ii++ ){
           if( !mmm[ii] ){
             u1 = (float)drand48(); u2 = (float)drand48();
             af[ii] = ubot + usiz*(u1+u2) ;
           }
         }
       }
     }
   } /* end of processing input targim */

   /* smooth images if needed
      13 Oct 2006: separate smoothing radii for base and target */

   need_smooth_base = (stup->smooth_code > 0 && stup->smooth_radius_base > 0.0f);
   need_smooth_targ = (stup->smooth_code > 0 && stup->smooth_radius_targ > 0.0f);
   do_smooth_base   = need_smooth_base &&
                     ( stup->smooth_code        != stup->old_sc      ||
                       stup->smooth_radius_base != stup->old_sr_base ||
                       stup->bsims              == NULL                ) ;
   do_smooth_targ   = need_smooth_targ &&
                     ( stup->smooth_code        != stup->old_sc      ||
                       stup->smooth_radius_targ != stup->old_sr_targ ||
                       stup->ajims              == NULL                ) ;
   stup->old_sc      = stup->smooth_code   ;
   stup->old_sr_base = stup->smooth_radius_base ;
   stup->old_sr_targ = stup->smooth_radius_targ ;

   if( !need_smooth_base ){
     if( stup->bsims != NULL ){ mri_free(stup->bsims); stup->bsims = NULL; }
     stup->old_sr_base = -1.0f ;
     mri_unpurge(stup->bsim) ; /* 20 Dec 2006 */
   }
   if( !need_smooth_targ ){
     if( stup->ajims != NULL ){ mri_free(stup->ajims); stup->ajims = NULL; }
     stup->old_sr_targ = -1.0f ;
     mri_unpurge(stup->ajim) ; /* 20 Dec 2006 */
   }
   if( do_smooth_base ){
     if( stup->bsims != NULL ) mri_free(stup->bsims);
     if( verb > 1 )
       ININFO_message("- Smoothing base; radius=%.2f",stup->smooth_radius_base);
     stup->bsims = GA_smooth( stup->bsim , stup->smooth_code ,
                                           stup->smooth_radius_base ) ;
     if( stup->usetemp ) mri_purge( stup->bsim ) ;  /* 20 Dec 2006 */
   }
   if( do_smooth_targ ){
     if( stup->ajims != NULL ) mri_free(stup->ajims);
     if( verb > 1 )
       ININFO_message("- Smoothing source; radius=%.2f",stup->smooth_radius_targ);
     stup->ajims = GA_smooth( stup->ajim , stup->smooth_code ,
                                           stup->smooth_radius_targ ) ;
     if( stup->usetemp ) mri_purge( stup->ajim ) ;  /* 20 Dec 2006 */
   }

   /* get min and max values in base and target images */

   if( stup->ajims == NULL ){
     stup->ajbot = (float)mri_min(stup->ajim) ;
     stup->ajtop = (float)mri_max(stup->ajim) ;
     if( stup->ajbot >= 0.0f ) stup->ajclip = mri_topclip(stup->ajim) ;
     else                      stup->ajclip = stup->ajtop ;
   } else {
     stup->ajbot = (float)mri_min(stup->ajims) ;
     stup->ajtop = (float)mri_max(stup->ajims) ;
     if( stup->ajbot >= 0.0f ) stup->ajclip = mri_topclip(stup->ajims) ;
     else                      stup->ajclip = stup->ajtop ;
   }

   if( stup->bsims == NULL ){
     stup->bsbot = (float)mri_min(stup->bsim) ;
     stup->bstop = (float)mri_max(stup->bsim) ;
     if( stup->bsbot >= 0.0f ) stup->bsclip = mri_topclip(stup->bsim) ;
     else                      stup->bsclip = stup->bstop ;
   } else {
     stup->bsbot = (float)mri_min(stup->bsims) ;
     stup->bstop = (float)mri_max(stup->bsims) ;
     if( stup->bsbot >= 0.0f ) stup->bsclip = mri_topclip(stup->bsims) ;
     else                      stup->bsclip = stup->bstop ;
   }

   /** load weight and mask arrays **/

   if( wghtim != NULL ){              /*---- have new weight to load ----*/
     MRI_IMAGE *qim ; float *bar , bfac ;

     need_pts = 1 ;
     if( wghtim->nvox != stup->bsim->nvox )
       ERREX("basim and wghtim grids differ!?!") ;

     if( stup->bwght != NULL ) mri_free(stup->bwght) ;
     if( stup->bmask != NULL ) free((void *)stup->bmask) ;

     if( verb > 1 ) ININFO_message("- copying weight image") ;

     stup->bwght = mri_to_float(wghtim) ; bar = MRI_FLOAT_PTR(stup->bwght) ;
     qim = mri_new_conforming(wghtim,MRI_byte); stup->bmask = MRI_BYTE_PTR(qim);
     bfac = (float)mri_maxabs(stup->bwght) ;
     if( bfac == 0.0f ) ERREX("wghtim is all zero?!?") ;
     bfac = 1.0f / bfac ;
     for( ii=0 ; ii < wghtim->nvox ; ii++ ){   /* scale weight, make mask */
       bar[ii] = fabsf(bar[ii])*bfac ;
       stup->bmask[ii] = (bar[ii] != 0.0f) ;
     }

     mri_fix_data_pointer( NULL , qim ) ; mri_free(qim) ;
     stup->nmask = THD_countmask( wghtim->nvox , stup->bmask ) ;
     if( stup->nmask < 99 ){
       WARNING_message("mri_genalign_scalar: illegal input mask") ;
       free(stup->bmask) ;
       stup->bmask = NULL ; stup->nmask = stup->nvox_mask = 0 ;
       mri_free(stup->bwght) ; stup->bwght = NULL ;
     } else {
       stup->nvox_mask = wghtim->nvox ;
     }

   } else if( stup->nmask > 0 ){  /*---- have old mask to check ----*/

     if( stup->nvox_mask != stup->bsim->nvox ){
       WARNING_message("old mask and new base image differ in size") ;
       if( stup->bwght != NULL ) mri_free(stup->bwght) ;
       if( stup->bmask != NULL ) free((void *)stup->bmask) ;
       stup->nmask = stup->nvox_mask = 0 ;
       stup->bmask = NULL ; stup->bwght = NULL ;
     } else if( verb > 1 )
       ININFO_message("- retaining old weight image") ;

   } else {                           /*---- have no mask, new or old ----*/
     stup->bmask = NULL ;
     stup->bwght = NULL ;
     stup->nmask = stup->nvox_mask = 0 ;
     if( verb > 1 ) ININFO_message("- no weight image") ;
   }

   /*-- extract matching points from base image (maybe) --*/

   nmatch = stup->npt_match ;
   if( stup->npt_match <= 9 || stup->npt_match > stup->bsim->nvox ){
     nmatch = stup->bsim->nvox ; use_all = 1 ;
   }
   if( stup->nmask > 0 && stup->npt_match > stup->nmask ){
     nmatch = stup->nmask ; use_all = 2 ;
   }

   if( stup->im == NULL || stup->im->nar != nmatch || stup->npt_match != nmatch )
     need_pts = 1 ;

   if( need_pts ){
     MRI_IMAGE *bim ;

     stup->npt_match = nmatch ;
     if( verb > 1 ) ININFO_message("- using %d points from base image",nmatch) ;

     if( use_all == 1 ){         /*------------- all points, no mask -----------*/

       if( stup->im != NULL ){
         KILL_floatvec(stup->im) ;
         KILL_floatvec(stup->jm) ;
         KILL_floatvec(stup->km) ;
       }
       stup->im = stup->jm = stup->km = NULL ;

     } else if( use_all == 2 ){  /*------------- all points in mask ------------*/

       int nvox , pp ; byte *mask=stup->bmask ;

       if( stup->im != NULL ){
         KILL_floatvec(stup->im) ;
         KILL_floatvec(stup->jm) ;
         KILL_floatvec(stup->km) ;
       }
       MAKE_floatvec(stup->im,stup->npt_match) ;
       MAKE_floatvec(stup->jm,stup->npt_match) ;
       MAKE_floatvec(stup->km,stup->npt_match) ;

       for( mm=pp=0 ; pp < stup->npt_match ; mm++ ){
         if( GOOD(mm) ){
           ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
           stup->im->ar[pp] = ii; stup->jm->ar[pp] = jj; stup->km->ar[pp] = kk;
           pp++ ;
         }
       }

     } else {  /*--------------------- a subset of points ----------------------*/

       int nvox,pp,dm , *qm ; byte *mask = stup->bmask ;

       nvox = stup->bsim->nvox ;
       dm   = find_relprime_fixed(nvox) ;
       if( stup->im != NULL ){
         KILL_floatvec(stup->im) ;
         KILL_floatvec(stup->jm) ;
         KILL_floatvec(stup->km) ;
       }
       MAKE_floatvec(stup->im,stup->npt_match) ;
       MAKE_floatvec(stup->jm,stup->npt_match) ;
       MAKE_floatvec(stup->km,stup->npt_match) ;

       qm = (int *)calloc(sizeof(int),stup->npt_match) ;
       mm = (nx/2) + (ny/2)*nx + (nz/2)*nxy ;
       for( pp=0 ; pp < stup->npt_match ; mm=(mm+dm)%nvox )
         if( GOOD(mm) ) qm[pp++] = mm ;
       qsort_int( stup->npt_match , qm ) ;

       for( pp=0 ; pp < stup->npt_match ; pp++ ){
         mm = qm[pp] ;
         ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
         stup->im->ar[pp] = ii; stup->jm->ar[pp] = jj; stup->km->ar[pp] = kk;
       }
       free((void *)qm) ;
     }

     /*------------- extract values from base image for matching -------------*/

     KILL_floatvec(stup->bvm) ; KILL_floatvec(stup->wvm) ;
     bim = (stup->bsims != NULL ) ? stup->bsims : stup->bsim ;
     bsar = MRI_FLOAT_PTR(bim) ;
     MAKE_floatvec(stup->bvm,stup->npt_match) ;
     if( stup->bwght == NULL ) stup->wvm = NULL ;
     else                      MAKE_floatvec(stup->wvm,stup->npt_match) ;
     if( stup->im == NULL ){
       memcpy( stup->bvm->ar , bsar , sizeof(float)*stup->npt_match ) ;
       if( stup->wvm != NULL )
         memcpy( stup->wvm->ar , MRI_FLOAT_PTR(stup->bwght) ,
                                 sizeof(float)*stup->npt_match ) ;
     } else {
       for( qq=0 ; qq < stup->npt_match ; qq++ ){
         rr = (int)(stup->im->ar[qq] + stup->jm->ar[qq]*nx + stup->km->ar[qq]*nxy) ;
         stup->bvm->ar[qq] = bsar[rr] ;
       }
       if( stup->bwght != NULL ){
         bsar = MRI_FLOAT_PTR(stup->bwght) ;
         for( qq=0 ; qq < stup->npt_match ; qq++ ){
           rr = (int)(stup->im->ar[qq] + stup->jm->ar[qq]*nx + stup->km->ar[qq]*nxy) ;
           stup->wvm->ar[qq] = bsar[rr] ;
         }
       }
     }
     if( stup->usetemp ){
       mri_purge(stup->bwght) ;  /* 21 Dec 2006 */
       mri_purge(bim) ;          /* 22 Dec 2006 */
     }

     /* do match_code specific pre-processing of the extracted data */

#if 0
     switch( stup->match_code ){
       case GA_MATCH_SPEARMAN_SCALAR:
         STATUS("doing spearman_rank_prepare") ;
         stup->bvstat = spearman_rank_prepare( stup->npt_match, stup->bvm->ar );
       break ;
     }
#endif

   } /* end of if(need_pts) */

   if( stup->blokset != NULL ){                      /* 20 Aug 2007 */
     GA_BLOK_KILL( (GA_BLOK_set *)(stup->blokset) ) ;
     stup->blokset = NULL ;
   }

   stup->need_hist_setup = 1 ;   /* 08 May 2007 */

   if( verb > 1 ) ININFO_message("* Exit alignment setup routine") ;
   stup->setup = SMAGIC ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Set the byte mask for the target (or unset it).
    Should be done BEFORE mri_genalign_scalar_setup().  [07 Aug 2007] */

void mri_genalign_set_targmask( MRI_IMAGE *im_tmask , GA_setup *stup )
{
ENTRY("mri_genalign_set_targmask") ;
   if( stup == NULL ) EXRETURN ;
   if( stup->ajmask != NULL ){ mri_free(stup->ajmask); stup->ajmask = NULL; }
   if( im_tmask != NULL ){
     if( stup->ajim != NULL ){
       if( im_tmask->nvox != stup->ajim->nvox ){
         ERROR_message("mri_genalign_set_targmask: image mismatch!") ;
         EXRETURN ;
       } else {
         WARNING_message("mri_genalign_set_targmask: called after setup()?!") ;
       }
     }
     stup->ajmask = mri_copy(im_tmask) ;
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Setup parameters for optimizing.
-----------------------------------------------------------------------------*/

static void GA_param_setup( GA_setup *stup )
{
   int ii , qq ;

ENTRY("GA_param_setup") ;

   if( stup == NULL || stup->setup != SMAGIC ){
     ERROR_message("Illegal call to GA_param_setup()") ;
     EXRETURN ;
   }

   /* count free parameters to optimize over */

   for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ )
     if( !stup->wfunc_param[qq].fixed ) ii++ ;

   stup->wfunc_numfree = ii ;
   if( ii == 0 ){
     ERROR_message("No free parameters in GA_param_setup()?"); EXRETURN;
   }
   for( qq=0 ; qq < stup->wfunc_numpar ; qq++ )
     stup->wfunc_param[qq].siz = stup->wfunc_param[qq].max
                                -stup->wfunc_param[qq].min ;

#if 0
   if( verb ){
     fprintf(stderr," + %d free parameters:\n",stup->wfunc_numfree) ;
     for( qq=0 ; qq < stup->wfunc_numpar ; qq++ )
       fprintf(stderr,"  #%d = %s [%.2f..%.2f] (fixed=%d)\n",
               qq , stup->wfunc_param[qq].name ,
               stup->wfunc_param[qq].min , stup->wfunc_param[qq].max ,
               stup->wfunc_param[qq].fixed ) ;
   }
#endif

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Optimize warping parameters, after doing setup.
    Return value is number of optimization functional calls
    (if it reaches nstep, then final accuracy of rend was not reached).
-----------------------------------------------------------------------------*/

int mri_genalign_scalar_optim( GA_setup *stup ,
                               double rstart, double rend, int nstep )
{
   double *wpar ;
   int ii , qq , nfunc ;

ENTRY("mri_genalign_scalar_optim") ;

   if( stup == NULL || stup->setup != SMAGIC ){
     ERROR_message("Illegal call to mri_genalign_scalar_optim()") ;
     RETURN(-1) ;
   }

   GA_param_setup(stup) ;
   if( stup->wfunc_numfree <= 0 ) RETURN(-2) ;

   /* copy initial warp parameters into local array wpar,
      scaling to the range 0..1                          */

   wpar = (double *)calloc(sizeof(double),stup->wfunc_numfree) ;
   for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
     if( !stup->wfunc_param[qq].fixed ){
       wpar[ii] = ( stup->wfunc_param[qq].val_init
                   -stup->wfunc_param[qq].min    ) / stup->wfunc_param[qq].siz;
       if( wpar[ii] < 0.0 || wpar[ii] > 1.0 ) wpar[ii] = PRED01(wpar[ii]) ;
       ii++ ;
     }
   }

   gstup = stup ;  /* for global access, in other functions in this file */

   if( nstep <= 4*stup->wfunc_numfree+5 ) nstep = 6666 ;

        if( rstart >  0.2 ) rstart = 0.2 ;  /* our parameters are */
   else if( rstart <= 0.0 ) rstart = 0.1 ;  /* all in range 0..1 */

   if( rend >= 0.9*rstart || rend <= 0.0 ) rend = 0.0666 * rstart ;

   /*** all the real work takes place now ***/

   nfunc = powell_newuoa( stup->wfunc_numfree , wpar ,
                          rstart , rend , nstep , GA_scalar_fitter ) ;

   stup->vbest = GA_scalar_fitter( stup->wfunc_numfree , wpar ) ;

   /* copy+scale output parameter values back to stup struct */

   for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
     if( stup->wfunc_param[qq].fixed ){
       stup->wfunc_param[qq].val_out = stup->wfunc_param[qq].val_fixed ;
     } else {
       stup->wfunc_param[qq].val_out = stup->wfunc_param[qq].min
                                      +stup->wfunc_param[qq].siz
                                       *PRED01(wpar[ii]) ;
       ii++ ;
     }
   }

   free((void *)wpar) ;

   RETURN(nfunc) ;
}

/*---------------------------------------------------------------------------*/
/*! Get the cost function for the given setup. */
/*---------------------------------------------------------------------------*/

float mri_genalign_scalar_cost( GA_setup *stup , float *parm )
{
   double *wpar , val ;
   int ii , qq ;

ENTRY("mri_genalign_scalar_cost") ;

   if( stup == NULL || stup->setup != SMAGIC ){
     ERROR_message("Illegal call to mri_genalign_scalar_cost()") ;
     RETURN( BIGVAL );
   }

   GA_param_setup(stup) ;
   if( stup->wfunc_numfree <= 0 ) RETURN( BIGVAL );

   /* copy initial warp parameters into local array wpar,
      scaling to the range 0..1                          */

   wpar = (double *)calloc(sizeof(double),stup->wfunc_numfree) ;

   for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
     if( !stup->wfunc_param[qq].fixed ){
       val = (parm == NULL) ? stup->wfunc_param[qq].val_init : parm[qq] ;
       wpar[ii] = (val - stup->wfunc_param[qq].min) / stup->wfunc_param[qq].siz;
       if( wpar[ii] < 0.0 || wpar[ii] > 1.0 ) wpar[ii] = PRED01(wpar[ii]) ;
       ii++ ;
     }
   }

   gstup = stup ;  /* for global access, in other functions in this file */

   val = GA_scalar_fitter( stup->wfunc_numfree , wpar ) ;

   free((void *)wpar) ; RETURN( (float)val );
}

/*---------------------------------------------------------------------------*/
/*! Return ALL cost functions for a given setup. */
/*---------------------------------------------------------------------------*/

floatvec * mri_genalign_scalar_allcosts( GA_setup *stup , float *parm )
{
   floatvec *costvec ;
   double *wpar , val ;
   float *avm , *bvm , *wvm ;
   int ii , qq , meth ;

ENTRY("mri_genalign_scalar_allcosts") ;

   if( stup == NULL || stup->setup != SMAGIC ){
     ERROR_message("Illegal call to mri_genalign_scalar_allcosts()") ;
     RETURN(NULL) ;
   }

   GA_param_setup(stup) ;
   if( stup->wfunc_numfree <= 0 ) RETURN(NULL);

   /* copy initial warp parameters into local array wpar,
      scaling to the range 0..1                          */

   wpar = (double *)calloc(sizeof(double),stup->wfunc_numfree) ;
   for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
     if( !stup->wfunc_param[qq].fixed ){
       val = (parm == NULL) ? stup->wfunc_param[qq].val_init : parm[qq] ;
       wpar[ii] = (val - stup->wfunc_param[qq].min) / stup->wfunc_param[qq].siz;
       if( wpar[ii] < 0.0 || wpar[ii] > 1.0 ) wpar[ii] = PRED01(wpar[ii]) ;
       ii++ ;
     }
   }

   gstup = stup ;

   avm = (float *)calloc(stup->npt_match,sizeof(float)) ; /* target points at */
   GA_get_warped_values( stup->wfunc_numfree,wpar,avm ) ; /* warped locations */

   bvm = stup->bvm->ar ;                                 /* base points */
   wvm = (stup->wvm != NULL) ? stup->wvm->ar : NULL ;    /* weights */

   GA_setup_2Dhistogram( avm , bvm ) ;
   MAKE_floatvec( costvec , GA_MATCH_METHNUM_SCALAR ) ;

   for( meth=1 ; meth <= GA_MATCH_METHNUM_SCALAR ; meth++ )
     costvec->ar[meth-1] = GA_scalar_costfun( meth, stup->npt_match, avm,bvm,wvm ) ;

   free((void *)wpar); free((void *)avm);    /* toss the trash */
   RETURN(costvec) ;
}

/*---------------------------------------------------------------------------*/
/*! Test some random starting points.  Sets val_init values in stup.
-----------------------------------------------------------------------------*/

void mri_genalign_scalar_ransetup( GA_setup *stup , int nrand )
{
   double *wpar, *spar , val , vbest , *bpar , *qpar,*cpar , dist ;
   int ii , qq , twof , ss , nfr , icod , nt=0 ;
#define NKEEP 15
   double *kpar[NKEEP] , kval[NKEEP] ; int nk,kk,jj, ngrid,ngtot ;
   int ival[NKEEP] ; float fval[NKEEP] ;

ENTRY("mri_genalign_scalar_ransetup") ;

   if( stup == NULL || stup->setup != SMAGIC ){
     ERROR_message("Illegal call to mri_genalign_scalar_ransetup()") ;
     EXRETURN ;
   }
   if( nrand < NKEEP ) nrand = 2*NKEEP ;

   GA_param_setup(stup) ; gstup = stup ;
   if( stup->wfunc_numfree <= 0 ) EXRETURN ;

   nfr = stup->wfunc_numfree ;
   switch( nfr ){
     case 1: ngrid = 9 ; break ;
     case 2: ngrid = 5 ; break ;
     case 3: ngrid = 3 ; break ;
     case 4:
     case 5:
     case 6: ngrid = 2 ; break ;
    default: ngrid = 1 ; break ;
   }
   for( ngtot=1,qq=0 ; qq < nfr ; qq++ ) ngtot *= ngrid ;

   icod = stup->interp_code ;
   stup->interp_code = MRI_NN ;
   if( AFNI_yesenv("AFNI_TWOPASS_LIN") ) stup->interp_code = MRI_LINEAR ;

   wpar = (double *)calloc(sizeof(double),nfr) ;
   spar = (double *)calloc(sizeof(double),nfr) ;
   for( kk=0 ; kk < NKEEP ; kk++ )
     kpar[kk] = (double *)calloc(sizeof(double),nfr) ;

   /* try the middle of the allowed parameter range */

   for( qq=0 ; qq < nfr ; qq++ ) wpar[qq] = 0.5 ;
   val = GA_scalar_fitter( nfr , wpar ) ;

   memcpy(kpar[0],wpar,sizeof(double)*nfr) ;
   kval[0] = val ;
   for( kk=1 ; kk < NKEEP ; kk++ ) kval[kk] = BIGVAL ;

   /* try some random places, keep the best NKEEP of them */

   twof = 1 << nfr ;  /* 2^nfr */

   if( verb ) fprintf(stderr," + - Scanning %d:",nrand+ngtot) ;

   for( ii=0 ; ii < nrand+ngtot ; ii++ ){
     if( ii < ngtot ){                     /* grid points */
       val = 0.5/(ngrid+1.0) ; ss = ii ;
       for( qq=0 ; qq < nfr ; qq++ ){
         kk = ss % ngrid; ss = ss / ngrid; wpar[qq] = 0.5+(kk+1)*val;
       }
     } else {                              /* random */
       for( qq=0 ; qq < nfr ; qq++ ) wpar[qq] = 0.5*(1.05+0.90*drand48()) ;
     }

     for( ss=0 ; ss < twof ; ss++ ){   /* try divers reflections */
       for( qq=0 ; qq < nfr ; qq++ )
         spar[qq] = (ss & (1<<qq)) ? 1.0-wpar[qq] : wpar[qq] ;

       val = GA_scalar_fitter( nfr , spar ) ;     /* get error measurement */
       for( kk=0 ; kk < NKEEP ; kk++ ){   /* find if this is better than */
         if( val < kval[kk] ){            /* something we've seen so far */
           for( jj=NKEEP-2 ; jj >= kk ; jj-- ){  /* push those above kk up */
             memcpy( kpar[jj+1] , kpar[jj] , sizeof(double)*nfr ) ;
             kval[jj+1] = kval[jj] ;
           }
           memcpy( kpar[kk] , spar , sizeof(double)*nfr ) ;  /* save what */
           kval[kk] = val ;                              /* we just found */
           if( verb && kk < 3 ) fprintf(stderr,(kk==0)?"*":".") ;
           break ;
         }
       }
     }
   } /* end of initial scan; should have NKEEP best results in kpar & kval */

   for( kk=0 ; kk < NKEEP ; kk++ )  /* make sure are in 0..1 range */
     for( ii=0 ; ii < nfr ; ii++ ) kpar[kk][ii] = PRED01(kpar[kk][ii]) ;

   if( verb ){                    /* print table of results? */
     fprintf(stderr,"\n") ;
     fprintf(stderr," + - best random kval:\n") ;
     for(kk=0;kk<NKEEP;kk++){
      fprintf(stderr,"   %2d v=%g:",kk,kval[kk]);
      for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
       if( !stup->wfunc_param[qq].fixed ){
        val = stup->wfunc_param[qq].min+stup->wfunc_param[qq].siz*kpar[kk][ii];
        fprintf(stderr," %.2f",val) ; ii++ ;
       }
      }
      fprintf(stderr,"\n") ;
     }
   }

   /* try a little optimization on each of these parameter sets */

   vbest = BIGVAL ; jj = 0 ; if( icod != MRI_NN ) stup->interp_code = MRI_LINEAR ;
   for( kk=0 ; kk < NKEEP ; kk++ ){
     if( kval[kk] >= BIGVAL ) continue ;  /* should not happen */
     (void)powell_newuoa( nfr , kpar[kk] ,
                          0.05 , 0.005 , 9*nfr+7 , GA_scalar_fitter ) ;
     kval[kk] = GA_scalar_fitter( nfr , kpar[kk] ) ;
     if( kval[kk] < vbest ){ vbest = kval[kk]; jj = kk; }
   }
   stup->vbest = vbest ;  /* save for user's edification */

   for( kk=0 ; kk < NKEEP ; kk++ )  /* make sure are in 0..1 range */
     for( ii=0 ; ii < nfr ; ii++ ) kpar[kk][ii] = PRED01(kpar[kk][ii]) ;

   /* at this point, smallest error is vbest and best index in kpar is jj */

   if( verb ){                    /* print out optimized results? */
     fprintf(stderr," + - optimized random kval:\n") ;
     for(kk=0;kk<NKEEP;kk++){
      fprintf(stderr,"  %c%2d %g:",(kk==jj)?'*':' ',kk,kval[kk]);
      for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
       if( !stup->wfunc_param[qq].fixed ){
        val = stup->wfunc_param[qq].min+stup->wfunc_param[qq].siz*kpar[kk][ii];
        fprintf(stderr," %.2f",val) ; ii++ ;
       }
      }
      fprintf(stderr,"\n") ;
     }
   }

   /* save best result in the parameter structure */

   bpar = kpar[jj] ;
   for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
     if( !stup->wfunc_param[qq].fixed ){
       stup->wfunc_param[qq].val_init = stup->wfunc_param[qq].min
                                       +stup->wfunc_param[qq].siz*bpar[ii];
       ii++ ;
     }
     stup->wfunc_param[qq].val_out = stup->wfunc_param[qq].val_init ;
   }

   /* sort kval, then store a bunch of the best parameters,
      which are not too close to the absolute best set we just saved */

#undef  DTHRESH
#define DTHRESH 0.05
   for( ii=0 ; ii < NKEEP ; ii++ ){ fval[ii] = kval[ii]; ival[ii] = ii; }
   qsort_floatint( NKEEP , fval , ival ) ;
   for( qq=0 ; qq < stup->wfunc_numpar ; qq++ ){ /** save best into trial #0 **/
     if( !stup->wfunc_param[qq].fixed )
       stup->wfunc_param[qq].val_trial[0] = stup->wfunc_param[qq].val_init ;
     else
       stup->wfunc_param[qq].val_trial[0] = stup->wfunc_param[qq].val_fixed ;
   }
   nt = 1 ;
   for( jj=1 ; jj < NKEEP && nt < PARAM_MAXTRIAL ; jj++ ){
     qpar = kpar[ival[jj]] ;                 /* the jj-th best param set */
     for( kk=0 ; kk < jj ; kk++ ){   /* loop over the previous best ones */
       cpar =  kpar[ival[kk]] ;
       for( dist=0.0,ii=0 ; ii < nfr ; ii++ ){ /* compute dist from previous best */
         val = fabs(qpar[ii]-cpar[ii]) ; dist = MAX(dist,val) ;
       }
       if( dist < DTHRESH ){  /* too close to cpar ==> skip */
         if( verb > 1 ) ININFO_message("- skip #%d in trials",ival[jj]) ;
         goto NEXT_jj ;
       }
     }
     if( verb > 1 ) ININFO_message("- save #%d in trials",ival[jj]) ;
     for( ii=qq=0 ; qq < stup->wfunc_numpar ; qq++ ){
       if( !stup->wfunc_param[qq].fixed ){
         stup->wfunc_param[qq].val_trial[nt] = stup->wfunc_param[qq].min
                                              +stup->wfunc_param[qq].siz
                                               *qpar[ii];
         ii++ ;
       } else {
         stup->wfunc_param[qq].val_trial[nt] = stup->wfunc_param[qq].val_fixed ;
       }
     }
     nt++ ; /* 1 more trial set saved */
   NEXT_jj: ;
   }
   stup->wfunc_ntrial = nt ;

   /*** cleanup and exeunt ***/

   free((void *)wpar) ; free((void *)spar) ;
   for( kk=0 ; kk < NKEEP ; kk++ ) free((void *)kpar[kk]) ;

   stup->interp_code = icod ; EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Warp the entire target image to base coords.  Will be in float format.
-----------------------------------------------------------------------------*/

MRI_IMAGE * mri_genalign_scalar_warpim( GA_setup *stup )
{
   MRI_IMAGE *wim ;
   float     *war ;
   float      oot ;

ENTRY("mri_genalign_scalar_warpim") ;

   if( stup       == NULL || stup->setup != SMAGIC ||
       stup->ajim == NULL || stup->bsim  == NULL     ){
     ERROR_message("Illegal call to mri_genalign_scalar_warpim()") ;
     RETURN(NULL) ;
   }
   gstup = stup ;

   wim = mri_new_conforming( stup->bsim , MRI_float ) ;
   war = MRI_FLOAT_PTR(wim) ;

   oot = outval ; outval = 0.0f ;
   GA_get_warped_values( 0 , NULL , war ) ;
   outval = oot ;

   RETURN(wim) ;
}

/*-------------------------------------------------------------------------*/
/*! Warp an image to base coords, on an nnx X nny X nnz grid.
     - The mapping between ijk and base xyz coords, and
       the mapping between target xyz and ijk coords must have
       been set in mri_genalign_affine_set_befafter() before calling this!
     - The warping between base xyz and target xyz is given by the
       wfunc, which has npar parameters stored in wpar.
     - The interpolation method is in icode.
     - Output is in float format, no matter what input data format was.
     - Generalized from GA_get_warped_values() -- RWCox - 26 Sep 2006.
---------------------------------------------------------------------------*/

MRI_IMAGE * mri_genalign_scalar_warpone( int npar, float *wpar, GA_warpfunc *wfunc,
                                         MRI_IMAGE *imtarg ,
                                         int nnx , int nny , int nnz , int icode )
{
   int   ii,jj,kk,qq,pp,npp,mm,nx,ny,nxy,nz , npt ;
   float x,y,z ;
   float *imf , *jmf , *kmf ;
   float *imw , *jmw , *kmw ;
   MRI_IMAGE *wim , *inim ;
   float     *war , *inar ;
   float oot ;

ENTRY("mri_genalign_scalar_warpone") ;

   if( wfunc == NULL || imtarg == NULL ) RETURN(NULL) ;

   /* send parameters to warping function, for setup */

   if( verb > 1 ){
     fprintf(stderr,"++ image warp: parameters =") ;
     for( ii=0 ; ii < npar ; ii++ ) fprintf(stderr," %.4f",wpar[ii]) ;
     fprintf(stderr,"\n") ;
   }

   wfunc( npar , wpar , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;

   /* create float copy of input image, if needed */

   if( imtarg->kind == MRI_float ) inim = imtarg ;
   else                            inim = mri_to_float(imtarg) ;
   inar = MRI_FLOAT_PTR(inim) ;

   /* dimensions of output image */

   nx = nnx ; ny = nny ; nz = nnz ; nxy = nx*ny ; npt = nxy * nz ;
   wim = mri_new_vol( nx,ny,nz , MRI_float ) ;
   war = MRI_FLOAT_PTR(wim) ;

   /* ijk coordinates in base image to be warped to target ijk */

   imf = (float *)calloc(sizeof(float),NPER) ;
   jmf = (float *)calloc(sizeof(float),NPER) ;
   kmf = (float *)calloc(sizeof(float),NPER) ;

   /* ijk coordinates after warping */

   imw = (float *)calloc(sizeof(float),NPER) ;
   jmw = (float *)calloc(sizeof(float),NPER) ;
   kmw = (float *)calloc(sizeof(float),NPER) ;

   oot = outval ; outval = 0.0f ;

   /*--- do (up to) NPER points at a time ---*/

   for( pp=0 ; pp < npt ; pp+=NPER ){
     npp = MIN( NPER , npt-pp ) ;      /* number to do */

     /* get base ijk coords */

     for( qq=0 ; qq < npp ; qq++ ){
       mm = pp+qq ;
       ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
       imf[qq] = (float)ii; jmf[qq] = (float)jj; kmf[qq] = (float)kk;
     }

     /**** warp base points to new locations ****/

     wfunc( npar , NULL , npp  , imf,jmf,kmf , imw,jmw,kmw ) ;

     /* interpolate target image at warped points */

     switch( icode ){
       case MRI_NN:
         GA_interp_NN( inim , npp , imw,jmw,kmw , war+pp ) ;
       break ;

       case MRI_LINEAR:
         GA_interp_linear( inim , npp , imw,jmw,kmw , war+pp ) ;
       break ;

       case MRI_CUBIC:
         GA_interp_cubic( inim , npp , imw,jmw,kmw , war+pp ) ;
       break ;

       default:        /* for higher order methods not implemented here */
       case MRI_QUINTIC:
         GA_interp_quintic( inim , npp , imw,jmw,kmw , war+pp ) ;
       break ;
     }
   }

   outval = oot ;

   /* clip interpolated values to range of target image, if need be */

   if( MRI_HIGHORDER(icode) ){
     float bb=inar[0] , tt=inar[0] ; int nin=inim->nvox ;
     for( pp=1 ; pp < nin ; pp++ ) if( inar[pp] < bb ) bb = inar[pp] ;
                              else if( inar[pp] > tt ) tt = inar[pp] ;
     for( pp=0 ; pp < npt ; pp++ ) if( war[pp]  < bb ) war[pp] = bb ;
                              else if( war[pp]  > tt ) war[pp] = tt ;
   }

   /* free the enslaved memory */

   free((void *)kmw); free((void *)jmw); free((void *)imw);
   free((void *)kmf); free((void *)jmf); free((void *)imf);
   if( inim != imtarg ) mri_free(inim) ;

   RETURN(wim) ;
}

/*==========================================================================*/
/*****------------------------ Warping functions -----------------------*****/
/*--------------------------------------------------------------------------*/

#include "vecmat.h"

/****************************************************************************/
/**************************** General affine warp ***************************/

/***** 04 Oct 2006: Modify to use mat44 instead of vecmat stuff, mostly *****/

/*--------------------------------------------------------------------------*/

#define D2R (PI/180.0)                /* angles are in degrees */

static int matorder = MATORDER_SDU ;  /* cf. mrilib.h */
static int smat     = SMAT_LOWER ;
static int dcode    = DELTA_AFTER  ;  /* cf. 3ddata.h */

void mri_genalign_affine_setup( int mmmm , int dddd , int ssss )
{
   if( mmmm > 0 ) matorder = mmmm ;
   if( dddd > 0 ) dcode    = dddd ;
   if( ssss > 0 ) smat     = ssss ;
   return ;
}

/*--------------------------------------------------------------------------*/

static int   aff_use_before=0 , aff_use_after=0 ;
static mat44 aff_before       , aff_after       , aff_gamijk , aff_gamxyz ;

void mri_genalign_affine_set_befafter( mat44 *ab , mat44 *af )
{
   if( ab == NULL || !ISVALID_MAT44(*ab) ){
     aff_use_before = 0 ;
   } else {
     aff_use_before = 1 ; aff_before = *ab ;
   }

   if( af == NULL || !ISVALID_MAT44(*af) ){
     aff_use_after = 0 ;
   } else {
     aff_use_after = 1 ; aff_after = *af ;
   }
   return ;
}

void mri_genalign_affine_get_befafter( mat44 *ab , mat44 *af )
{
   if( ab != NULL ) *ab = aff_before ;
   if( af != NULL ) *af = aff_after  ;
}

void mri_genalign_affine_get_gammaijk( mat44 *gg )
{
  if( gg != NULL ) *gg = aff_gamijk ;
}

void mri_genalign_affine_get_gammaxyz( mat44 *gg )
{
  if( gg != NULL ) *gg = aff_gamxyz ;
}

/*--------------------------------------------------------------------------*/
/*! Compute a rotation matrix specified by 3 angles:
      Q = R3 R2 R1, where Ri is rotation about axis axi by angle thi.
----------------------------------------------------------------------------*/

static mat44 rot_matrix( int ax1, double th1,
                         int ax2, double th2, int ax3, double th3  )
{
   mat44 q , p ;

   LOAD_ROT_MAT44( q , th1 , ax1 ) ;
   LOAD_ROT_MAT44( p , th2 , ax2 ) ; q = MAT44_MUL( p , q ) ;
   LOAD_ROT_MAT44( p , th3 , ax3 ) ; q = MAT44_MUL( p , q ) ;

   return q ;
}

/*--------------------------------------------------------------------------*/
static int pgmat=0 ;
void mri_genalign_set_pgmat( int p ){ pgmat = p; }

/*--------------------------------------------------------------------------*/

static mat44 GA_setup_affine( int npar , float *parvec )
{
   mat44 ss,dd,uu,aa,bb , gam ;
   THD_fvec3 vv ;
   float     a,b,c , p,q,r ;

   if( pgmat ){
     int ii ;
     printf("GA_setup_affine params:") ;
     for( ii=0 ; ii < npar ; ii++ ) printf(" %g",parvec[ii]) ;
     printf("\n") ;
   }

   /* uu = rotation */

   a = b = c = 0.0f ;
   if( npar >= 4 ) a = D2R*parvec[3] ;
   if( npar >= 5 ) b = D2R*parvec[4] ;
   if( npar >= 6 ) c = D2R*parvec[5] ;
   if( a != 0.0f || b != 0.0f || c != 0.0f )
     uu = rot_matrix( 2,a , 0,b , 1,c ) ;
   else
     LOAD_DIAG_MAT44( uu , 1.0f,1.0f,1.0f ) ;

   if( pgmat ) DUMP_MAT44("GA_setup_affine uu",uu) ;

   /* dd = scaling */

   a = b = c = 1.0f ;
   if( npar >= 7 ){ a = parvec[6]; if( a <= 0.10f || a >= 10.0f ) a = 1.0f; }
   if( npar >= 8 ){ b = parvec[7]; if( b <= 0.10f || b >= 10.0f ) b = 1.0f; }
   if( npar >= 9 ){ c = parvec[8]; if( c <= 0.10f || c >= 10.0f ) c = 1.0f; }
   LOAD_DIAG_MAT44( dd , a,b,c ) ;

   if( pgmat ) DUMP_MAT44("GA_setup_affine dd",dd) ;

   /* ss = shear */

   a = b = c = 0.0f ;
   if( npar >= 10 ){ a = parvec[ 9]; if( fabsf(a) > 0.3333f ) a = 0.0f; }
   if( npar >= 11 ){ b = parvec[10]; if( fabsf(b) > 0.3333f ) b = 0.0f; }
   if( npar >= 12 ){ c = parvec[11]; if( fabsf(c) > 0.3333f ) c = 0.0f; }
#if 1
   switch( smat ){
     default:
     case SMAT_LOWER: LOAD_MAT44( ss , 1.0 , 0.0 , 0.0 , 0.0,
                                        a  , 1.0 , 0.0 , 0.0,
                                        b  ,  c  , 1.0 , 0.0 ) ; break ;

     case SMAT_UPPER: LOAD_MAT44( ss , 1.0 ,  a  ,  b , 0.0 ,
                                       0.0 , 1.0 ,  c , 0.0 ,
                                       0.0 , 0.0 , 1.0, 0.0  ) ; break ;

     case SMAT_XXX:   LOAD_MAT44( ss , 1.0 ,  a  ,  b , 0.0 ,
                                       0.0 , 1.0 , 0.0, 0.0 ,
                                       0.0 , 0.0 , 1.0, 0.0  ) ; break ;

     case SMAT_YYY:   LOAD_MAT44( ss , 1.0 , 0.0 , 0.0, 0.0 ,
                                        a  , 1.0 ,  b , 0.0 ,
                                       0.0 , 0.0 , 1.0, 0.0  ) ; break ;

     case SMAT_ZZZ:   LOAD_MAT44( ss , 1.0 , 0.0 , 0.0, 0.0 ,
                                       0.0 , 1.0 , 0.0, 0.0 ,
                                        a  ,  b  , 1.0, 0.0  ) ; break ;
   }
#else
   ksm = (smat % 3) ;           /* decode:  smat = ism*9 + jsm*3 + ksm    */
   ism = (smat / 9) ;           /* (ism,jsm,ksm) = permutation of (0,1,2) */
   jsm = (smat - 9*ism - ksm) ;
   LOAD_DIAG_MAT44( ss , 1.0 , 1.0 , 1.0 ) ;  /* identity */
   ss.m[jsm][ism] = a ;
   ss.m[ksm][ism] = b ;
   ss.m[ksm][jsm] = c ;
#endif

   if( pgmat ) DUMP_MAT44("GA_setup_affine ss",ss) ;

   /* multiply them, as ordered */

   switch( matorder ){
     default:
     case MATORDER_SDU:  aa = MAT44_MUL(ss,dd) ; bb = uu ; break ;
     case MATORDER_SUD:  aa = MAT44_MUL(ss,uu) ; bb = dd ; break ;
     case MATORDER_DSU:  aa = MAT44_MUL(dd,ss) ; bb = uu ; break ;
     case MATORDER_DUS:  aa = MAT44_MUL(dd,uu) ; bb = ss ; break ;
     case MATORDER_USD:  aa = MAT44_MUL(uu,ss) ; bb = dd ; break ;
     case MATORDER_UDS:  aa = MAT44_MUL(uu,dd) ; bb = ss ; break ;
   }
   gam = MAT44_MUL(aa,bb) ;

   /* shifts */

   a = b = c = 0.0f ;
   if( npar >= 1 ) a = parvec[0] ;
   if( npar >= 2 ) b = parvec[1] ;
   if( npar >= 3 ) c = parvec[2] ;

   if( dcode == DELTA_BEFORE ){
     MAT44_VEC( gam , a,b,c , p,q,r ) ;
     a = p ; b = q ; c = r ;
   }
   LOAD_MAT44_VEC( gam , a,b,c ) ;

   if( pgmat ) DUMP_MAT44("GA_setup_affine gam (xyz)",gam) ;

   /* before and after transformations? */

   aff_gamxyz = gam ;

   if( pgmat && aff_use_before ) DUMP_MAT44("GA_setup_affine before",aff_before) ;
   if( pgmat && aff_use_after  ) DUMP_MAT44("GA_setup_affine after ",aff_after ) ;

   if( aff_use_before ) gam = MAT44_MUL( gam , aff_before ) ;
   if( aff_use_after  ) gam = MAT44_MUL( aff_after , gam  ) ;

   aff_gamijk = gam ;

#if 0
   if( verb > 1 ){
     if( aff_use_before ) DUMP_MAT44("before",aff_before) ;
     if( aff_use_after  ) DUMP_MAT44("after ",aff_after ) ;
                          DUMP_MAT44("gam   ",gam       ) ;
   }
#endif

   if( pgmat ) DUMP_MAT44("GA_setup_affine gam (ijk)",gam) ;

   return gam ;
}

/*--------------------------------------------------------------------------*/
/*! A wfunc function for affine transformations. */
/*--------------------------------------------------------------------------*/

void mri_genalign_affine( int npar, float *wpar ,
                          int npt , float *xi, float *yi, float *zi ,
                                    float *xo, float *yo, float *zo  )
{
   static mat44 gam ;  /* saved general affine matrix */
   int ii ;

   /** new parameters ==> setup matrix */

   if( npar > 0 && wpar != NULL ){
     gam = GA_setup_affine( npar , wpar ) ;
     if( pgmat ) DUMP_MAT44("mri_genalign_affine",gam) ;
   }

   /* nothing to transform? */

   if( npt <= 0 || xi == NULL || xo == NULL ) return ;

   /* mutiply matrix times input vectors */

   for( ii=0 ; ii < npt ; ii++ )
     MAT44_VEC( gam , xi[ii],yi[ii],zi[ii] , xo[ii],yo[ii],zo[ii] ) ;

   return ;
}

/*--------------------------------------------------------------------------*/
/*! Similar to mri_genalign_affine(), but the 12 parameters are the matrix
    directly, with no physical interpretations such as angles, etc.
----------------------------------------------------------------------------*/

void mri_genalign_mat44( int npar, float *wpar,
                         int npt , float *xi, float *yi, float *zi ,
                                   float *xo, float *yo, float *zo  )
{
   static mat44 gam ;  /* saved general affine matrix */
   int ii ;

   /** new parameters ==> setup matrix */

   if( npar >= 12 && wpar != NULL ){
     LOAD_MAT44_AR(gam,wpar) ;
     if( pgmat ) DUMP_MAT44("mri_genalign_mat44",gam) ;
   }

   /* nothing to transform? */

   if( npt <= 0 || xi == NULL || xo == NULL ) return ;

   /* mutiply matrix times input vectors */

   for( ii=0 ; ii < npt ; ii++ )
     MAT44_VEC( gam , xi[ii],yi[ii],zi[ii] , xo[ii],yo[ii],zo[ii] ) ;

   return ;
}
