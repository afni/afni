#include "mrilib.h"

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

void GA_interp_linear( MRI_IMAGE *fim ,
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

void GA_interp_cubic( MRI_IMAGE *fim ,
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

/* sinc function = sin(PI*x)/(PI*x) */

#undef  sinc
#define sinc(x) ( (fabsf(x)>0.01f) ? sinf(PIF*(x))/(PIF*(x))     \
                                   : 1.0f - 1.6449341f*(x)*(x) )

/* Weight (taper) function, declining from ww(WCUT)=1 to ww(1)=0 */
/* Note that the input to ww will always be between WCUT and 1. */

#undef  ww
#define ww(x) ( 0.5f+0.5f*cosf(PIF*((x)-WCUT)/(1.0f-WCUT)) )

/*---------------------------------------------------------------------------*/
/*! Interpolate an image at npp (index) points, using weighted sinc (slow!). */

void GA_interp_wsinc5( MRI_IMAGE *fim ,
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

ENTRY("GA_interp_wsinc5") ;

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
       xw = fx - qq ; xsin[qq+IRAD] = sinc(xw) ;
       yw = fy - qq ; ysin[qq+IRAD] = sinc(yw) ;
       zw = fz - qq ; zsin[qq+IRAD] = sinc(zw) ;
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
/* Output image will be on the same grid as the input, of course.
   Input image format --> output format
              fvect   --> fvect
              rgb     --> rgb
              complex --> complex
              other   --> float
*//*-------------------------------------------------------------------------*/

MRI_IMAGE * GA_indexwarp( MRI_IMAGE *inim, int interp_code, MRI_IMAGE *wpim )
{
   MRI_IMAGE *outim=NULL , *fim , *iim,*jjm,*kkm ;
   float     *outar      , *far , *iar,*jar,*kar ;
   float ffmin=0.0f , ffmax=0.0f ;
   int ii , nvox,nx,ny,nz,nxy , do_clip ;

ENTRY("GA_indexwarp") ;

   if( inim == NULL || wpim == NULL || wpim->kind != MRI_fvect ) RETURN(NULL);
   if( mri_data_pointer(inim) == NULL ||
       mri_data_pointer(wpim) == NULL || wpim->vdim != 3 )       RETURN(NULL);
   if( inim->nx != wpim->nx ||
       inim->ny != wpim->ny || inim->nz != wpim->nz )            RETURN(NULL);

   /*- if input is itself a vector, use recursion to process each sub-image -*/

#undef  CALLME
#define CALLME(inee,outee) outee = GA_indexwarp( (inee), interp_code, wpim )
   if( ISVECTIM(inim) ){ VECTORME(inim,outim) ; RETURN(outim) ; }

   /*------------------ here, input image is scalar-valued ------------------*/

   fim = (inim->kind == MRI_float ) ? inim : mri_to_float(inim) ;
   far = MRI_FLOAT_PTR(fim) ;

   outim = mri_new_conforming( fim , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;

   nx = fim->nx; ny = fim->ny; nz = fim->nz; nxy = nx*ny; nvox = nx*ny*nz;

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

   iar = MRI_FLOAT_PTR(iim) ;
   jar = MRI_FLOAT_PTR(jjm) ;
   kar = MRI_FLOAT_PTR(kkm) ;

   do_clip = ( interp_code != MRI_NN && interp_code != MRI_LINEAR ) ;
   if( do_clip ){
     ffmin = ffmax = far[0] ;
     for( ii=1 ; ii < nvox ; ii++ ){
            if( far[ii] < ffmin ) ffmin = far[ii] ;
       else if( far[ii] > ffmax ) ffmax = far[ii] ;
     }
   }

   switch( interp_code ){

     case MRI_NN:
       GA_interp_NN( fim , nvox , iar,jar,kar , outar ) ;
     break ;

     case MRI_LINEAR:
       GA_interp_linear( fim , nvox , iar,jar,kar , outar ) ;
     break ;

     case MRI_CUBIC:
       GA_interp_cubic( fim , nvox , iar,jar,kar , outar ) ;
     break ;

     case MRI_VARP1:
       GA_interp_varp1( fim , nvox , iar,jar,kar , outar ) ;
     break ;

     case MRI_WSINC5:
       GA_interp_wsinc5( fim , nvox , iar,jar,kar , outar ) ;
     break ;

     default:        /* for higher order methods not implemented here */
     case MRI_QUINTIC:
       GA_interp_quintic( fim , nvox , iar,jar,kar , outar ) ;
     break ;
   }

   if( do_clip ){
     for( ii=0 ; ii < nvox ; ii++ ){
            if( outar[ii] < ffmin ) outar[ii] = ffmin ;
       else if( outar[ii] > ffmax ) outar[ii] = ffmax ;
     }
   }

   /*--- done! ---*/

   mri_free(kkm) ; mri_free(jjm) ; mri_free(iim) ;
   if( fim != inim ) mri_free(fim) ;

   RETURN(outim) ;
}

/*---------------------------------------------------------------------------*/
/* Apply a matrix to a set of warp vectors. */

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
