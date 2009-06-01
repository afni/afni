#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#undef  NPER
#define NPER 131072  /* 512 Kbytes per float array */

/*---------------------------------------------------------------------------*/

int GA_gcd( int m , int n )    /* Euclid's Greatest Common Denominator */
{
  while( m > 0 ){
    if( n > m ){ int t=m; m=n; n=t; } /* swap */
    m -= n ;
  }
  return n ;
}

/*---------------------------------------------------------------------------*/

int GA_find_relprime_fixed( int n )  /* find number relatively prime to n */
{
   int dj , n5=n/5 ;
   if( n5 < 2 ) return 1 ;
   for( dj=n5 ; GA_gcd(n,dj) > 1 ; dj++ ) ; /*nada*/
   return dj ;
}

/*---------------------------------------------------------------------------*/
/*! Smooth an image with a given method to a given radius.
    Assumes the dx,dy,dz parameters in the image struct are correct! */

MRI_IMAGE * GA_smooth( MRI_IMAGE *im , int meth , float rad )
{
   MRI_IMAGE *om=NULL ;

ENTRY("GA_smooth") ;

   if( im == NULL || rad <= 0.0f ) RETURN(NULL) ;

#undef  CALLME
#define CALLME(inp,out) (out) = GA_smooth( (inp), meth,rad )
   if( ISVECTIM(im) ){ VECTORME(im,om) ; RETURN(om) ; }

   if( im->kind != MRI_float ) RETURN(NULL) ;

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

#undef  FARJK
#define FARJK(j,k) (far+(j)*nx+(k)*nxy)

/* clip value mm to range 0..nn */

#undef  CLIP
#define CLIP(mm,nn) if(mm < 0)mm=0; else if(mm > nn)mm=nn

static float outval = 0.0f ;                   /* value for 'outside' voxels */
void  GA_set_outval( float v ){ outval = v; }  /* 28 Feb 2007 */
float GA_get_outval(void){ return outval; }    /* 10 Dec 2008 */

/*---------------------------------------------------------------*/
/*! Interpolate an image at npp (index) points, using NN method. */

void GA_interp_NN( MRI_IMAGE *fim ,
                   int npp, float *ip, float *jp, float *kp, float *vv )
{
ENTRY("GA_interp_NN") ;

#pragma omp parallel if(npp > 9999)
 {
   int nx=fim->nx , ny=fim->ny , nz=fim->nz , nxy=nx*ny , ii,jj,kk , pp ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float *far = MRI_FLOAT_PTR(fim) ;
 AFNI_OMP_START ;
#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ vv[pp]=outval; continue; }
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ vv[pp]=outval; continue; }
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ vv[pp]=outval; continue; }

     ii = (int)(xx+0.5f) ; jj = (int)(yy+0.5f) ; kk = (int)(zz+0.5f) ;
     vv[pp] = FAR(ii,jj,kk) ;
   }
 AFNI_OMP_END ;
 } /* end OpenMP */

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Interpolate an image at npp (index) points, using linear method. */

void GA_interp_linear( MRI_IMAGE *fim ,
                       int npp, float *ip, float *jp, float *kp, float *vv )
{
ENTRY("GA_interp_linear") ;

#pragma omp parallel if(npp > 9999)
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

 AFNI_OMP_START ;
#pragma omp for
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

 AFNI_OMP_END ;
 } /* end OpenMP */

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

void GA_interp_cubic( MRI_IMAGE *fim ,
                      int npp, float *ip, float *jp, float *kp, float *vv )
{
ENTRY("GA_interp_cubic") ;

#pragma omp parallel if(npp > 9999)
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

 AFNI_OMP_START ;
#pragma omp for
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

 AFNI_OMP_END ;
 } /* end OpenMP */

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* define variance preserving interpolation functions */

#undef  BP
#define BP   0.7611164839f        /* = (11+sqrt(33))/22 */
#undef  PSI
#define PSI (xb>-0.001f && xb<0.001f)                                       \
            ? (1.2222222f+0.52008386f*xb)*xb                                \
            : 0.16666667f*xb                                                \
              * sqrtf(  ( 28.0f*xxx*(xxx-1.0f)+10.0f+(6.0f-12.0f*xxx)*phi ) \
                      / (xb*xb) )
#undef  VPWT
#define VPWT(x)                                                         \
 { float xxx=(x) , xb , px , psi ;                                      \
   float phi = sqrtf(1.0f-8.0f*xxx*(xxx-1.0f)) ;                        \
   wt_m1 = wt_p2 = 0.25f * (1.0f-phi) ; px = 0.25f + 0.08333333f*phi ;  \
                    xb = xxx-BP ; psi = PSI ; wt_00 = px - psi ;        \
   xxx = 1.0f-xxx ; xb = xxx-BP ; psi = PSI ; wt_p1 = px - psi ; }

/*------------------------------------------------------------------*/
/*! Interpolate an image at npp (index) points, using VP method. */

void GA_interp_varp1( MRI_IMAGE *fim ,
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

ENTRY("GA_interp_varp1") ;

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

     VPWT(fx) ;  /* interpolation weights in x direction */

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

     VPWT(fy) ;  /* interpolation weights in y direction */

     f_km1 =  wt_m1 * f_jm1_km1 + wt_00 * f_j00_km1
            + wt_p1 * f_jp1_km1 + wt_p2 * f_jp2_km1 ;
     f_k00 =  wt_m1 * f_jm1_k00 + wt_00 * f_j00_k00
            + wt_p1 * f_jp1_k00 + wt_p2 * f_jp2_k00 ;
     f_kp1 =  wt_m1 * f_jm1_kp1 + wt_00 * f_j00_kp1
            + wt_p1 * f_jp1_kp1 + wt_p2 * f_jp2_kp1 ;
     f_kp2 =  wt_m1 * f_jm1_kp2 + wt_00 * f_j00_kp2
            + wt_p1 * f_jp1_kp2 + wt_p2 * f_jp2_kp2 ;

     /* interpolate to kz+fz to get output */

     VPWT(fz) ;  /* interpolation weights in z direction */

     vv[pp] =  wt_m1 * f_km1 + wt_00 * f_k00
             + wt_p1 * f_kp1 + wt_p2 * f_kp2 ;
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Interpolation with weighted (tapered) sinc in 3D.
   ++ Taper function ww(r) is defined to be 1 for 0 <= r <= WCUT
       and for WCUT < r < 1 is a raised c sine dropping down to ww(r=1) = 0.
       This choice was made to keep the variance smoothing artifact low.
   ++ Radius of sinc window is WRAD, so that the actual taper used is
       ww(r/WRAD) where r = sqrt(di*di+dj*dj+dk*dk), and di=change in i index.
*//*-------------------------------------------------------------------------*/

#undef  WCUT
#define WCUT 0.5f    /* cutoff point for taper */

#undef  WRAD
#define WRAD 5.0001f /* width of sinc interpolation (float) */

#undef  IRAD
#define IRAD 5       /* width of sinc interpolation (int) */

#undef  PIF
#define PIF 3.1415927f /* PI in float */

/* sinc function = sin(PI*x)/(PI*x) [N.B.: x will always be >= 0] */

#undef  sinc
#define sinc(x) ( ((x)>0.01f) ? sinf(PIF*(x))/(PIF*(x))     \
                              : 1.0f - 1.6449341f*(x)*(x) )

/* Weight (taper) function, declining from ww(WCUT)=1 to ww(1)=0 */
/* Note that the input to ww will always be between WCUT and 1. */

#undef  ww
#define ww(x) ( 0.5f+0.5f*cosf(PIF*((x)-WCUT)/(1.0f-WCUT)) )

/*---------------------------------------------------------------------------*/
#define UNROLL    /* unroll some loops */

#define USE_5P 1  /* use product-weighted sinc (5p),
                     rather than spherical weighted (5s), which is very slow */

#ifndef USE_5P    /* USE_5P is off */
/*---------------------------------------------------------------------------*/
/*! Interpolate an image at npp (index) points, using weighted sinc (slow!). */

void GA_interp_wsinc5s( MRI_IMAGE *fim ,
                        int npp, float *ip, float *jp, float *kp, float *vv )
{
   static MCW_cluster *smask=NULL ; static int nmask=0 ;
   static short *di=NULL , *dj=NULL , *dk=NULL ;

   int nx=fim->nx , ny=fim->ny , nz=fim->nz , nxy=nx*ny , pp ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float fx,fy,fz ;
   float *far = MRI_FLOAT_PTR(fim) ;
   int nx1=nx-1,ny1=ny-1,nz1=nz-1, ix,jy,kz ;

   float xw,yw,zw,rr , sum,wsum,wt ;
   int   iq,jq,kq , qq , ddi,ddj,ddk ;
   float xsin[1+2*IRAD] , ysin[1+2*IRAD] , zsin[1+2*IRAD] ;

ENTRY("GA_interp_wsinc5s") ;

   /*----- first time in: build spherical mask  -----*/
   /*((((( WRAD=5 ==> mask will have 515 points )))))*/

   if( smask == NULL ){
     smask = MCW_spheremask( 1.0f,1.0f,1.0f , WRAD ) ;
     nmask = smask->num_pt ;
     di    = smask->i ;
     dj    = smask->j ;
     dk    = smask->k ;
     if( PRINT_TRACING ){
       char str[256]; sprintf(str,"sinc mask=%d points",nmask); STATUS(str);
     }
   }

   /*----- loop over points -----*/

   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ vv[pp]=outval; continue; }
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ vv[pp]=outval; continue; }
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ vv[pp]=outval; continue; }

     ix = floorf(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floorf(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floorf(zz) ;  fz = zz - kz ;

     /*- compute sinc at all points plus/minus 5 indexes from current locale -*/

     for( qq=-IRAD ; qq <= IRAD ; qq++ ){
       xw = fabsf(fx - qq) ; xsin[qq+IRAD] = sinc(xw) ;
       yw = fabsf(fy - qq) ; ysin[qq+IRAD] = sinc(yw) ;
       zw = fabsf(fz - qq) ; zsin[qq+IRAD] = sinc(zw) ;
     }

     for( wsum=sum=0.0f,qq=0 ; qq < nmask ; qq++ ){
       ddi = di[qq] ; ddj = dj[qq] ; ddk = dk[qq] ;
       iq = ix + ddi ; CLIP(iq,nx1) ; xw = fx - (float)ddi ;
       jq = jy + ddj ; CLIP(jq,ny1) ; yw = fy - (float)ddj ;
       kq = kz + ddk ; CLIP(kq,nz1) ; zw = fz - (float)ddk ;
       rr = sqrtf(xw*xw+yw*yw+zw*zw) / WRAD ; if( rr >= 1.0f ) continue ;
       wt = xsin[ddi+IRAD] * ysin[ddj+IRAD] * zsin[ddk+IRAD] ;
       if( rr > WCUT ) wt *= ww(rr) ;
       wsum += wt ; sum += FAR(iq,jq,kq) * wt ;
     }

     vv[pp] = sum / wsum ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
#else /* USE_5P is on */

/*! Interpolate an image at npp (index) points, using weighted sinc (slow!). */

void GA_interp_wsinc5p( MRI_IMAGE *fim ,
                        int npp, float *ip, float *jp, float *kp, float *vv )
{
ENTRY("GA_interp_wsinc5p") ;

#pragma omp parallel if(npp > 9999)
 {
   int nx=fim->nx , ny=fim->ny , nz=fim->nz , nxy=nx*ny , pp ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float fx,fy,fz ;
   float *far = MRI_FLOAT_PTR(fim) , *farjk ;
   int nx1=nx-1,ny1=ny-1,nz1=nz-1, ix,jy,kz ;

   float xw,yw,zw,rr , sum,wsum,wfac,wt ;
   int   iq,jq,kq,iqp , qq,jj,kk , ddi,ddj,ddk ;
   float xsin[2*IRAD] , ysin[2*IRAD]        , zsin[2*IRAD] ;
   float wtt[2*IRAD]  , fjk[2*IRAD][2*IRAD] , fk[2*IRAD]   ;
   int   iqq[2*IRAD]  ;
   /*----- loop over points -----*/

   AFNI_OMP_START ;
#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ vv[pp]=outval; continue; }
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ vv[pp]=outval; continue; }
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ vv[pp]=outval; continue; }

     ix = floorf(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floorf(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floorf(zz) ;  fz = zz - kz ;

     /*- x interpolations -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){
       xw  = fabsf(fx - qq) ; wt = sinc(xw) ;
       xw /= WRAD ; if( xw > WCUT ) wt *= ww(xw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
       iq = ix+qq ; CLIP(iq,nx1) ; iqq[qq+(IRAD-1)] = iq ;
     }
     wfac = wsum ;

     for( jj=-IRAD+1 ; jj <= IRAD ; jj++ ){
       jq = jy+jj ; CLIP(jq,ny1) ;
       for( kk=-IRAD+1 ; kk <= IRAD ; kk++ ){
         kq = kz+kk ; CLIP(kq,nz1) ;
#ifndef UNROLL
         for( sum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){
           iq = iqq[qq+(IRAD-1)] ; sum += FAR(iq,jq,kq) * wtt[qq+(IRAD-1)] ;
         }
#else
         farjk = FARJK(jq,kq) ;
#if IRAD != 5
         for( sum=0.0f,qq=-IRAD+1 ; qq <  IRAD ; qq+=2 ){  /* unrolled by 2 */
           iq = iqq[qq+(IRAD-1)] ; iqp = iqq[qq+IRAD] ;
           sum += farjk[iq]  * wtt[qq+(IRAD-1)]
                 +farjk[iqp] * wtt[qq+ IRAD   ] ;
         }
#else
# define FW(i) farjk[iqq[i]]*wtt[i]
         sum = FW(0)+FW(1)+FW(2)+FW(3)+FW(4)+FW(5)+FW(6)+FW(7)+FW(8)+FW(9) ;
# undef  FW
#endif
#endif
         fjk[jj+(IRAD-1)][kk+(IRAD-1)] = sum ;
       }
     }

     /*- y interpolations -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){
       yw  = fabsf(fy - qq) ; wt = sinc(yw) ;
       yw /= WRAD ; if( yw > WCUT ) wt *= ww(yw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
     }
     wfac *= wsum ;

     for( kk=-IRAD+1 ; kk <= IRAD ; kk++ ){
#ifndef UNROLL
       for( sum=0.0f,jj=-IRAD+1 ; jj <= IRAD ; jj++ ){
         sum += wtt[jj+(IRAD-1)]*fjk[jj+(IRAD-1)][kk+(IRAD-1)] ;
       }
#else
       for( sum=0.0f,jj=-IRAD+1 ; jj <  IRAD ; jj+=2 ){  /* unrolled by 2 */
         sum += wtt[jj+(IRAD-1)]*fjk[jj+(IRAD-1)][kk+(IRAD-1)]
               +wtt[jj+ IRAD   ]*fjk[jj+ IRAD   ][kk+(IRAD-1)] ;
       }
#endif
       fk[kk+(IRAD-1)] = sum ;
     }

     /*- z interpolation -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){
       zw  = fabsf(fz - qq) ; wt = sinc(zw) ;
       zw /= WRAD ; if( zw > WCUT ) wt *= ww(zw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
     }
     wfac *= wsum ;

#ifndef UNROLL
     for( sum=0.0f,kk=-IRAD+1 ; kk <= IRAD ; kk++ ){
       sum += wtt[kk+(IRAD-1)] * fk[kk+(IRAD-1)] ;
     }
#else
     for( sum=0.0f,kk=-IRAD+1 ; kk <  IRAD ; kk+=2 ){  /* unrolled by 2 */
       sum += wtt[kk+(IRAD-1)] * fk[kk+(IRAD-1)]
             +wtt[kk+ IRAD   ] * fk[kk+ IRAD   ] ;
     }
#endif

     vv[pp] = sum / wfac ;
   }

 AFNI_OMP_END ;
 } /* end OpenMP */

   EXRETURN ;
}
#endif /* USE_5P */

#undef  WCUT
#undef  WRAD
#undef  IRAD
#undef  PIF

/*---------------------------------------------------------------------------*/

void GA_interp_wsinc5( MRI_IMAGE *fim ,
                       int npp, float *ip, float *jp, float *kp, float *vv )
{
#ifdef USE_5P
   GA_interp_wsinc5p( fim,npp,ip,jp,kp,vv ) ;
#else
   GA_interp_wsinc5s( fim,npp,ip,jp,kp,vv ) ;
#endif
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

void GA_interp_quintic( MRI_IMAGE *fim ,
                        int npp, float *ip, float *jp, float *kp, float *vv )
{
ENTRY("GA_interp_quintic") ;

#pragma omp parallel if(npp > 9999)
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

 AFNI_OMP_START ;
#pragma omp for
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

 AFNI_OMP_END ;
 } /* end OpenMP */

   EXRETURN ;
}

/*===========================================================================*/
/*--- Stuff for storing sub-BLOKs of data points for localized cost funcs ---*/

/* is abs(a) <= s ?? */

#undef  FAS
#define FAS(a,s) ( (a) <= (s) && (a) >= -(s) )

/** define inside of a ball; is point (a,b,c) inside?  **/
/** volume of ball = 4*PI/3 * siz**3 = 4.1888 * siz**3 **/

#define GA_BLOK_inside_ball(a,b,c,siz) \
  ( ((a)*(a)+(b)*(b)+(c)*(c)) <= (siz) )

/** define inside of a cube **/
/** volume of cube = 8 * siz**3 **/
/** lattice vectors = [2*siz,0,0]  [0,2*siz,0]  [0,0,2*siz] **/

#define GA_BLOK_inside_cube(a,b,c,siz) \
  ( FAS((a),(siz)) && FAS((b),(siz)) && FAS((c),(siz)) )

/** define inside of a rhombic dodecahedron (RHDD) **/
/** volume of RHDD = 2 * siz**3 **/
/** lattice vectors = [siz,siz,0]  [0,siz,siz]  [siz,0,siz] **/

#define GA_BLOK_inside_rhdd(a,b,c,siz)              \
  ( FAS((a)+(b),(siz)) && FAS((a)-(b),(siz)) &&     \
    FAS((a)+(c),(siz)) && FAS((a)-(c),(siz)) &&     \
    FAS((b)+(c),(siz)) && FAS((b)-(c),(siz))   )

/** define inside of a truncated octahedron (TOHD) **/
/** volume of TOHD = 4 * siz**3 **/
/** lattice vectors = [-siz,siz,siz]  [siz,-siz,siz]  [siz,siz,-siz] **/

#define GA_BLOK_inside_tohd(a,b,c,siz)                              \
  ( FAS((a),(siz)) && FAS((b),(siz)) && FAS((c),(siz))         &&   \
    FAS((a)+(b)+(c),1.5f*(siz)) && FAS((a)-(b)+(c),1.5f*(siz)) &&   \
    FAS((a)+(b)-(c),1.5f*(siz)) && FAS((a)-(b)-(c),1.5f*(siz))   )

/** define inside of an arbitrary blok type **/

#define GA_BLOK_inside(bt,a,b,c,s)                              \
 (  ((bt)==GA_BLOK_BALL) ? GA_BLOK_inside_ball((a),(b),(c),(s)) \
  : ((bt)==GA_BLOK_CUBE) ? GA_BLOK_inside_cube((a),(b),(c),(s)) \
  : ((bt)==GA_BLOK_RHDD) ? GA_BLOK_inside_rhdd((a),(b),(c),(s)) \
  : ((bt)==GA_BLOK_TOHD) ? GA_BLOK_inside_tohd((a),(b),(c),(s)) \
  : 0 )

/** add 1 value to a dynamically allocated  integer array **/

#define GA_BLOK_ADDTO_intar(nar,nal,ar,val)                                 \
 do{ if( (nar) == (nal) ){                                                  \
       (nal) = 1.2*(nal)+16; (ar) = (int *)realloc((ar),sizeof(int)*(nal)); \
     }                                                                      \
     (ar)[(nar)++] = (val);                                                 \
 } while(0)

/** truncate dynamically allocated integer array down to size **/

#define GA_BLOK_CLIP_intar(nar,nal,ar)                               \
 do{ if( (nar) < (nal) && (nar) > 0 ){                               \
       (nal) = (nar); (ar) = (int *)realloc((ar),sizeof(int)*(nal)); \
 }} while(0)

/*----------------------------------------------------------------------------*/
/*! Fill a struct with list of points contained in sub-bloks of the base.

    - nx,ny,nz = 3D grid dimensions
    - dx,dy,dz = 3D grid spacings
    - npt      = number of points stored in im,jm,km
    - im,jm,km = 3D indexes of points to blok-ize
                 (can be NULL, in which case all nx*ny*nz points are used)
    - bloktype = one of GA_BLOK_BALL, GA_BLOK_CUBE, GA_BLOK_RHDD, GA_BLOK_TOHD
    - blokrad  = radius parameter for the bloks to be built
    - minel    = minimum number of points to put in a blok
                 (if 0, function will pick a value)
    - shfac    = shrinkage factor -- normally, bloks don't overlap much, but
                 you can specify shfac to change the lattice size:
                 < 1 makes them overlap more, and > 1 makes them spaced apart
    - verb     = whether to print out some verbosity stuff FYI
*//*--------------------------------------------------------------------------*/

GA_BLOK_set * create_GA_BLOK_set( int   nx , int   ny , int   nz ,
                                  float dx , float dy , float dz ,
                                  int npt , float *im, float *jm, float *km,
                                  int bloktype , float blokrad , int minel ,
                                                 float shfac   , int verb   )
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

   if( shfac < 0.2f || shfac > 5.0f ) shfac = 1.0f ;

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
       a *= shfac ;
       dxp = 2.0f * a ; dyp = 0.0f  ; dzp = 0.0f             ;
       dxq = a        ; dyq = a * s3; dzq = 0.0f             ;
       dxr = a        ; dyr = a / s3; dzr = a * 0.666667f*s6 ;
     }
     break ;

     /* cubes go on a simple cubical lattice, spaced so faces touch */

     case GA_BLOK_CUBE:{
       float a =  blokrad ;
       siz = a ; a *= shfac ;
       dxp = 2*a ; dyp = 0.0f; dzp = 0.0f ;
       dxq = 0.0f; dyq = 2*a ; dzq = 0.0f ;
       dxr = 0.0f; dyr = 0.0f; dzr = 2*a  ;
     }
     break ;

     /* rhombic dodecahedra go on a FCC lattice,
        spaced so that faces touch (i.e., no volumetric overlap) */

     case GA_BLOK_RHDD:{
       float a = blokrad ;
       siz = a ; a *= shfac ;
       dxp = a   ; dyp = a   ; dzp = 0.0f ;
       dxq = 0.0f; dyq = a   ; dzq = a    ;
       dxr = a   ; dyr = 0.0f; dzr = a    ;
     }
     break ;

     /* truncated octahedra go on a BCC lattice,
        spaced so that faces touch (i.e., no volumetric overlap) */

     case GA_BLOK_TOHD:{
       float a = blokrad ;
       siz = a ; a *= shfac ;
       dxp = -a ; dyp =  a ; dzp =  a ;
       dxq =  a ; dyq = -a ; dzq =  a ;
       dxr =  a ; dyr =  a ; dzr = -a ;
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
/*! Return the vector of individual blok correlations, for further analysis.
    Each value in the returned vector corresponds to one blok in gbs.  Note
    that the number of points in the 'vm' arrays isn't needed as an input,
    since that is implicitly encoded in gbs.
*//*-------------------------------------------------------------------------*/

floatvec * GA_pearson_vector( GA_BLOK_set *gbs ,
                              float *avm, float *bvm, float *wvm )
{
   int nblok , nelm , *elm , dd , ii,jj , nm ;
   float xv,yv,xy,xm,ym,vv,ww,ws , wt ;
   floatvec *pv=NULL ; float *pvar ;

   if( gbs == NULL || avm == NULL || bvm == NULL ) return NULL ;

   nblok = gbs->num ; if( nblok < 1 ) return NULL ;

   MAKE_floatvec( pv , nblok ) ; pvar = pv->ar ;

   /* loop over bloks */

   for( dd=0 ; dd < nblok ; dd++ ){
     pvar[dd] = 0.0f ;                                /* default */
     nelm = gbs->nelm[dd] ; if( nelm < 9 ) continue ; /* skip it */
     elm  = gbs->elm[dd] ;  /* array of indexes in avm (etc.) to use */

     if( wvm == NULL ){   /*** unweighted correlation ***/
       xv=yv=xy=xm=ym=0.0f ;
       for( ii=0 ; ii < nelm ; ii++ ){  /* compute means */
         jj = elm[ii] ;
         xm += avm[jj] ; ym += bvm[jj] ;
       }
       xm /= nelm ; ym /= nelm ;
       for( ii=0 ; ii < nelm ; ii++ ){  /* compute (co)variances */
         jj = elm[ii] ;
         vv = avm[jj]-xm ; ww = bvm[jj]-ym ;
         xv += vv*vv ; yv += ww*ww ; xy += vv*ww ;
       }

     } else {             /*** weighted correlation ***/
       xv=yv=xy=xm=ym=ws=0.0f ;
       for( ii=0 ; ii < nelm ; ii++ ){  /* compute weighted means */
         jj = elm[ii] ;
         wt = wvm[jj] ; ws += wt ;
         xm += avm[jj]*wt ; ym += bvm[jj]*wt ;
       }
       xm /= ws ; ym /= ws ;
       for( ii=0 ; ii < nelm ; ii++ ){  /* compute weighted (co)variances */
         jj = elm[ii] ;
         wt = wvm[jj] ; vv = avm[jj]-xm ; ww = bvm[jj]-ym ;
         xv += wt*vv*vv ; yv += wt*ww*ww ; xy += wt*vv*ww ;
       }
     }

     if( xv > 0.0f && yv > 0.0f ) pvar[dd] = xy/sqrtf(xv*yv) ; /* correlation */
   }

   return pv ;
}

/*======================== End of BLOK-iness functionality ==================*/

/*---------------------------------------------------------------------------*/

MRI_IMAGE * GA_indexwarp( MRI_IMAGE *inim, int interp_code, MRI_IMAGE *wpim )
{
   float_triple delta = {0.0f,0.0f,0.0f} ;

   return GA_indexwarp_plus( inim , interp_code , wpim , delta , NULL ) ;
}

/*---------------------------------------------------------------------------*/
/* Output image will be on the same grid as the input, of course.
   Points not in the mask will be set to zero.

   Input image format ==> output format:
   ------------------     -------------
              fvect   ==> fvect
              rgb     ==> rgb
              complex ==> complex
              other   ==> float
*//*-------------------------------------------------------------------------*/

MRI_IMAGE * GA_indexwarp_plus( MRI_IMAGE *inim, int interp_code,
                               MRI_IMAGE *wpim, float_triple delta, byte *mask )
{
   MRI_IMAGE *outim=NULL , *fim , *iim,*jjm,*kkm ;
   float     *outar      , *far , *iar,*jar,*kar ;
   float ffmin=0.0f , ffmax=0.0f , delx,dely,delz ;
   int ii,jj , nvox,nx,ny,nz,nxy , do_clip ;
   byte *mmm=NULL ; int nmask=0 ; float *miar,*mjar,*mkar,*moutar ;

ENTRY("GA_indexwarp_plus") ;

   if( inim == NULL || wpim == NULL || wpim->kind != MRI_fvect ) RETURN(NULL);
   if( mri_data_pointer(inim) == NULL ||
       mri_data_pointer(wpim) == NULL || wpim->vdim != 3 )       RETURN(NULL);
   if( inim->nx != wpim->nx ||
       inim->ny != wpim->ny || inim->nz != wpim->nz )            RETURN(NULL);

   /*- if input is itself a vector, use recursion to process each sub-image -*/
   /*- (for usage of VECTORME macro, via CALLME, see mrilib.h) -*/

#undef  CALLME
#define CALLME(inee,outee) \
  outee = GA_indexwarp_plus( (inee),interp_code,wpim,delta,mask )

   if( ISVECTIM(inim) ){ VECTORME(inim,outim) ; RETURN(outim) ; }

   /*------------------ here, input image is scalar-valued ------------------*/
   /*                   (convert to float type, if needed)                   */

   fim = (inim->kind == MRI_float ) ? inim : mri_to_float(inim) ;
   far = MRI_FLOAT_PTR(fim) ;

   outim = mri_new_conforming( fim , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;  /* is zero filled */

   nx = fim->nx; ny = fim->ny; nz = fim->nz; nxy = nx*ny; nvox = nx*ny*nz;

   /* component images of 3D warp */

   iim = mri_fvect_subimage( wpim , 0 ) ;
   jjm = mri_fvect_subimage( wpim , 1 ) ;
   kkm = mri_fvect_subimage( wpim , 2 ) ;
#if 0
   if( kkm == NULL ){                               /* no k-direction info */
     kkm = mri_new_conforming( fim , MRI_float ) ;  /* so make some up */
     kar = MRI_FLOAT_PTR(kkm) ;
     for( ii=0 ; ii < nvox ; ii++ ) kar[ii] = (ii/nxy) ;
   }
#endif

   /* check mask */

   if( mask != NULL ){
     mmm = mask ; nmask = THD_countmask( nvox , mmm ) ;
     if( nmask < 1 ) nmask = nvox ;
   } else {
     nmask = nvox ;  /* no mask ==> do them all */
   }

   /* indexes at which to calculate output volume */

   iar = MRI_FLOAT_PTR(iim); jar = MRI_FLOAT_PTR(jjm); kar = MRI_FLOAT_PTR(kkm);

   /* make subset that fits the mask */

   if( nmask == nvox ){
     miar = iar ; mjar = jar ; mkar = kar ; moutar = outar ;
   } else {
     miar   = (float *)malloc(sizeof(float)*nmask) ;
     mjar   = (float *)malloc(sizeof(float)*nmask) ;
     mkar   = (float *)malloc(sizeof(float)*nmask) ;
     moutar = (float *)malloc(sizeof(float)*nmask) ;
     for( ii=jj=0 ; ii < nvox ; ii++ ){
       if( mmm[ii] ){
         miar[jj] = iar[ii] ; mjar[jj] = jar[ii] ; mkar[jj] = kar[ii] ; jj++ ;
       }
     }
   }

   /* shift by delta */

   delx = delta.a ; dely = delta.b ; delz = delta.c ;
   if( delx != 0.0f || dely != 0.0f || delz != 0.0f ){
     for( jj=0 ; jj < nmask ; jj++ ){
       miar[jj] += delx ; mjar[jj] += dely ; mkar[jj] += delz ;
     }
   }

   /* compute bounds on input to apply to output */

   do_clip = ( interp_code != MRI_NN && interp_code != MRI_LINEAR ) ;
   if( do_clip ){
     ffmin = ffmax = far[0] ;
     for( ii=1 ; ii < nvox ; ii++ ){
            if( far[ii] < ffmin ) ffmin = far[ii] ;
       else if( far[ii] > ffmax ) ffmax = far[ii] ;
     }
   }

   /*-- the actual interpolation work is outsourced --*/

   switch( interp_code ){

     case MRI_NN:
       GA_interp_NN     ( fim , nmask,miar,mjar,mkar,moutar ) ;
     break ;

     case MRI_LINEAR:
       GA_interp_linear ( fim , nmask,miar,mjar,mkar,moutar ) ;
     break ;

     case MRI_CUBIC:
       GA_interp_cubic  ( fim , nmask,miar,mjar,mkar,moutar ) ;
     break ;

     case MRI_VARP1:
       GA_interp_varp1  ( fim , nmask,miar,mjar,mkar,moutar ) ;
     break ;

     case MRI_WSINC5:
       GA_interp_wsinc5 ( fim , nmask,miar,mjar,mkar,moutar ) ;
     break ;

     default:
     case MRI_QUINTIC:
       GA_interp_quintic( fim , nmask,miar,mjar,mkar,moutar ) ;
     break ;
   }

   /* apply the bounds */

   if( do_clip ){
     for( jj=0 ; jj < nmask ; jj++ ){
            if( moutar[jj] < ffmin ) moutar[jj] = ffmin ;
       else if( moutar[jj] > ffmax ) moutar[jj] = ffmax ;
     }
   }

   /* copy subset values in moutar back to outar image */

   if( nmask < nvox ){
     for( ii=jj=0 ; ii < nvox ; ii++ ){
       if( mmm[ii] ) outar[ii] = moutar[jj++] ;
     }
     free(moutar) ; free(mkar) ; free(mjar) ; free(miar) ;
   }

   /*--- done! ---*/

   mri_free(kkm) ; mri_free(jjm) ; mri_free(iim) ;
   if( fim != inim ) mri_free(fim) ;

   RETURN(outim) ;
}

/*---------------------------------------------------------------------------*/
/*! Apply a matrix to a set of warp vectors (in place). */

void GA_affine_edit_warp( mat44 aff , MRI_IMAGE *wpim )
{
   int ii , nvox ;
   float *war , aa,bb,cc ;

ENTRY("GA_affine_edit_warp") ;

   if( !ISVALID_MAT44(aff) || wpim == NULL )                     EXRETURN ;

   if(  wpim->kind             != MRI_fvect ||
        mri_data_pointer(wpim) == NULL      || wpim->vdim != 3 ) EXRETURN ;

   nvox = wpim->nvox ;
   war  = (float *)mri_data_pointer(wpim) ;

   for( ii=0 ; ii < nvox ; ii++ ){
     aa = war[3*ii  ] ;
     bb = war[3*ii+1] ;
     cc = war[3*ii+2] ;
     MAT44_VEC( aff , aa,bb,cc , war[3*ii],war[3*ii+1],war[3*ii+2] ) ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* B(A(x)) */

#if 0
MRI_IMAGE * GA_compose_indexwarp( MRI_IMAGE *awpim , MRI_IMAGE *bwpim )
{

ENTRY("GA_compose_indexwarp") ;

   if( awpim == NULL || awpim->kind != MRI_fvect )           RETURN(NULL);
   if( bwpim == NULL || bwpim->kind != MRI_fvect )           RETURN(NULL);
   if( mri_data_pointer(awpim) == NULL || awpim->vdim != 3 ) RETURN(NULL) ;
   if( mri_data_pointer(bwpim) == NULL || bwpim->vdim != 3 ) RETURN(NULL) ;
   if( awpim->nx != bwpim->nx ||
       awpim->ny != bwpim->ny || awpim->nz != bwpim->nz )    RETURN(NULL);

   cwpim = GA_indexwarp( bwpim
#endif
