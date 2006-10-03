#include "mrilib.h"

#undef  BIGVAL
#define BIGVAL 1.e+38

/* is a voxel 'good' to use? */

#undef  GOOD
#define GOOD(i) (mask==NULL || mask[i])

/* mark for a good setup */

#undef  SMAGIC
#define SMAGIC 208921148  /* Zip Code for AFNI Group at NIMH */

/* global access to setup parameters */

static GA_setup *gstup = NULL ;

/* for stupid ancient compilers */

#ifdef SOLARIS
#define floorf floor
#endif

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
#if 0
static int find_relprime_random( int n ) /* another one relatively prime to n */
{
   int dj , n5=n/5 , n2=3*n5 ;
   if( n5 < 2 ) return 1 ;
   do{ dj = n5 + lrand48()%n2 ; } while( gcd(n,dj) > 1 ) ;
   return dj ;
}
#endif
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

/*---------------------------------------------------------------*/
/*! Interpolate an image at npp (index) points, using NN method. */

static void GA_interp_NN( MRI_IMAGE *fim ,
                          int npp, float *ip, float *jp, float *kp, float *vv )
{
   int nx=fim->nx , ny=fim->ny , nz=fim->nz , nxy=nx*ny , ii,jj,kk , pp ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float *far = MRI_FLOAT_PTR(fim) ;

   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ vv[pp]=0.0f; continue; }
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ vv[pp]=0.0f; continue; }
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ vv[pp]=0.0f; continue; }

     ii = (int)(xx+0.5f) ; jj = (int)(yy+0.5f) ; kk = (int)(zz+0.5f) ;
     vv[pp] = FAR(ii,jj,kk) ;
   }
   return ;
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
   int nx1=nx-1,ny1=ny-1,nz1=nz-1, ix,jy,kz ;
   int ix_00,ix_p1 ;         /* interpolation indices */
   int jy_00,jy_p1 ;
   int kz_00,kz_p1 ;
   float wt_00,wt_p1 ;       /* interpolation weights */
   float f_j00_k00, f_jp1_k00, f_j00_kp1, f_jp1_kp1, f_k00, f_kp1 ;

   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ vv[pp]=0.0f; continue; }
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ vv[pp]=0.0f; continue; }
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ vv[pp]=0.0f; continue; }

     ix = floorf(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floorf(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floorf(zz) ;  fz = zz - kz ;

     ix_00 = ix ; ix_p1 = ix+1 ; CLIP(ix_00,nx1) ; CLIP(ix_p1,nx1) ;
     jy_00 = jy ; jy_p1 = jy+1 ; CLIP(jy_00,ny1) ; CLIP(jy_p1,ny1) ;
     kz_00 = kz ; kz_p1 = kz+1 ; CLIP(kz_00,nz1) ; CLIP(kz_p1,nz1) ;

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
   return ;
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

   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ vv[pp]=0.0f; continue; }
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ vv[pp]=0.0f; continue; }
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ vv[pp]=0.0f; continue; }

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
   return ;
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

   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ vv[pp]=0.0f; continue; }
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ vv[pp]=0.0f; continue; }
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ vv[pp]=0.0f; continue; }

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
   return ;
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

/*----------------------------------------------*/
/*! Interpolate target image to control points. */

static void GA_get_warped_values( int nmpar , double *mpar , float *avm )
{
   int    npar , ii,jj,kk,qq,pp,npp,mm,nx,ny,nxy , clip=0 , npt ;
   float *wpar , v ;
   float *imf , *jmf , *kmf ;
   float *imw , *jmw , *kmw ;
   MRI_IMAGE *aim ;

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

   /* create space for default control points, if none given */

   if( mpar == NULL || gstup->im == NULL ){
     imf = (float *)calloc(sizeof(float),NPER) ;
     jmf = (float *)calloc(sizeof(float),NPER) ;
     kmf = (float *)calloc(sizeof(float),NPER) ;
     npt = gstup->bsim->nvox ;
   } else {
     npt = gstup->npt_match ;
   }

   /* create space for warped control points */

   imw = (float *)calloc(sizeof(float),NPER) ;
   jmw = (float *)calloc(sizeof(float),NPER) ;
   kmw = (float *)calloc(sizeof(float),NPER) ;

   nx = gstup->bsim->nx; ny = gstup->bsim->ny; nxy = nx*ny;

   /* send parameters to warping function */

   gstup->wfunc( npar , wpar , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;

   /*--- do (up to) NPER points at a time ---*/

   for( pp=0 ; pp < npt ; pp+=NPER ){
     npp = MIN( NPER , npt-pp ) ;  /* number to do */
     if( mpar == NULL || gstup->im == NULL ){
       for( qq=0 ; qq < npp ; qq++ ){  /* default control points */
         mm = pp+qq ;
         ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
         imf[qq] = (float)ii; jmf[qq] = (float)jj; kmf[qq] = (float)kk;
       }
       if( gstup->use_cmat ){   /* 24 Aug 2006 */
         float x,y,z ;
         for( qq=0 ; qq < npp ; qq++ ){
           x = imf[qq] ; y = jmf[qq] ; z = kmf[qq] ;
           MAT44_VEC( gstup->base_cmat , x,y,z ,
                      imf[qq] , jmf[qq] , kmf[qq] ) ;
         }
       } else if( gstup->bscali ){
         float xo=gstup->bsim->xo , dx=gstup->bsim->dx ;
         float yo=gstup->bsim->yo , dy=gstup->bsim->dy ;
         float zo=gstup->bsim->zo , dz=gstup->bsim->dz ;
         for( qq=0 ; qq < npp ; qq++ ){
           imf[qq] = xo + imf[qq]*dx ;   /* scale to spatial    */
           jmf[qq] = yo + jmf[qq]*dy ;   /* coords from indexes */
           kmf[qq] = zo + kmf[qq]*dz ;   /* in the bsim image   */
         }
       }
     } else {
       imf = gstup->im->ar + pp ;  /* pointers to control points */
       jmf = gstup->jm->ar + pp ;
       kmf = gstup->km->ar + pp ;
     }

     /* warp control points to new locations */

     gstup->wfunc( npar , NULL ,
                   npp  , imf,jmf,kmf , imw,jmw,kmw ) ;

     if( gstup->use_cmat ){   /* 24 Aug 2006 */
       float x,y,z ;
       for( qq=0 ; qq < npp ; qq++ ){
         x = imw[qq] ; y = jmw[qq] ; z = kmw[qq] ;
         MAT44_VEC( gstup->targ_imat , x,y,z ,
                    imw[qq] , jmw[qq] , kmw[qq] ) ;
       }
     } else if( gstup->ascali ){
       float xo=gstup->ajim->xo , dxi=1.0f/gstup->ajim->dx ;
       float yo=gstup->ajim->yo , dyi=1.0f/gstup->ajim->dy ;
       float zo=gstup->ajim->zo , dzi=1.0f/gstup->ajim->dz ;
       for( qq=0 ; qq < npp ; qq++ ){
         imw[qq] = (imw[qq]-xo) * dxi ;  /* unscale from spatial */
         jmw[qq] = (jmw[qq]-yo) * dyi ;  /* coords to indexes in */
         kmw[qq] = (kmw[qq]-zo) * dzi ;  /* the ajim image       */
       }
     }

     /* choose image from which to extract data:
                                     --smoothed--   -unsmoothed- */
     aim = (gstup->ajims != NULL && mpar != NULL ) ? gstup->ajims
                                                   : gstup->ajim ;

     /* interpolate target image at warped control points */

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
   }

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

   return ;
}

/*---------------------------------------------------------------------------*/
static float fit_vbest = BIGVAL ;
static void (*fit_callback)(int,double *) = NULL ;

void GA_reset_fit_callback( void (*fc)(int,double*) )
{
   fit_vbest = BIGVAL ; fit_callback = fc ; return ;
}

void GA_fitter_dotter(int n, double *mpar ){ printf("."); fflush(stdout); }

void GA_do_dots(int x){ GA_reset_fit_callback( (x)?GA_fitter_dotter:NULL ); }

/*---------------------------------------------------------------------------*/
/*! Fit metric for matching base and target image value pairs.
    (Smaller is a better match.)  For use as a NEWUOA optimization function.
-----------------------------------------------------------------------------*/

static double GA_scalar_fitter( int npar , double *mpar )
{
  float val=0.0f ;
  float *avm , *bvm , *wvm ;

  avm = (float *)calloc(gstup->npt_match,sizeof(float)) ; /* target points at */
  GA_get_warped_values( npar , mpar , avm ) ;             /* warped locations */

  bvm = gstup->bvm->ar ;                                  /* base points */
  wvm = (gstup->wvm != NULL) ? gstup->wvm->ar : NULL ;    /* weights */

  /* compare the avm and bvm arrays in some way */

  switch( gstup->match_code ){

    default:
    case GA_MATCH_PEARSON_SCALAR:   /* Pearson correlation coefficient */
      val = (double)THD_pearson_corr_wt( gstup->npt_match, avm, bvm,wvm ) ;
      val = 1.0 - fabs(val) ;
    break ;

    case GA_MATCH_SPEARMAN_SCALAR:  /* rank-order (Spearman) correlation */
      val = (double)spearman_rank_corr( gstup->npt_match, avm,
                                        gstup->bvstat   , bvm ) ;
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

    case GA_MATCH_CORRATIO_SCALAR:  /* Correlation Ratio */
      val = THD_corr_ratio_scl( gstup->npt_match ,
                                gstup->ajbot , gstup->ajclip , avm ,
                                gstup->bsbot , gstup->bsclip , bvm , wvm ) ;
      val = 1.0 - fabs(val) ;
    break ;

    case GA_MATCH_HELLINGER_SCALAR: /* Hellinger metric */
      val = -THD_hellinger_scl( gstup->npt_match ,
                                gstup->ajbot , gstup->ajclip , avm ,
                                gstup->bsbot , gstup->bsclip , bvm , wvm ) ;
    break ;
  }

  free((void *)avm) ;    /* toss the trash */

  if( fit_callback != NULL && val < fit_vbest ){
    fit_vbest = val ; fit_callback(npar,mpar) ;
  }

  return (double)val ;
}

/*---------------------------------------------------------------------------*/

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
-----------------------------------------------------------------------------*/

void mri_genalign_scalar_setup( MRI_IMAGE *basim  , MRI_IMAGE *wghtim ,
                                MRI_IMAGE *targim , GA_setup  *stup    )

{
   int qq , rr , nx,ny,nz,nxy , mm,ii,jj,kk , qdim ;
   int use_all=0 , need_pts=0 , nmatch , need_smooth,do_smooth ;
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
     qdim = MRI_DIMENSIONALITY(basim) ;
     if( qdim < 2 || qdim > 3 )
       ERREX("basim dimensionality is not 2 or 3") ;
   } else {
     qdim = MRI_DIMENSIONALITY(stup->bsim) ;
   }
   stup->abdim = qdim ;

   if( targim != NULL ){
     if( qdim != MRI_DIMENSIONALITY(targim) )
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

     if( stup->use_cmat ){   /* 24 Aug 2006 */
       STATUS("invert base_cmat") ;
       stup->base_imat = nifti_mat44_inverse( stup->base_cmat ) ;
     }

     stup->bscali = (stup->bsim->dx != 1.0f) || (stup->bsim->xo != 0.0f) ||
                    (stup->bsim->dy != 1.0f) || (stup->bsim->yo != 0.0f)   ;
     if( stup->abdim == 3 && !stup->bscali )
       stup->bscali = (stup->bsim->dz != 1.0f) || (stup->bsim->zo != 0.0f) ;
   }
   nx = stup->bsim->nx; ny = stup->bsim->ny; nz = stup->bsim->nz; nxy = nx*ny;

   if( targim != NULL ){
     if( stup->ajim != NULL ) mri_free(stup->ajim) ;
     if( verb > 1 ) ININFO_message("- copying source image") ;
     stup->ajim = mri_to_float(targim) ;
     if( stup->ajim->dx <= 0.0f ) stup->ajim->dx = 1.0f ;
     if( stup->ajim->dy <= 0.0f ) stup->ajim->dy = 1.0f ;
     if( stup->ajim->dz <= 0.0f ) stup->ajim->dz = 1.0f ;

     if( stup->use_cmat ){  /* 24 Aug 2006 */
       STATUS("invert targ_cmat") ;
       stup->targ_imat = nifti_mat44_inverse( stup->targ_cmat ) ;
     }

     stup->ascali = (stup->ajim->dx != 1.0f) || (stup->ajim->xo != 0.0f) ||
                    (stup->ajim->dy != 1.0f) || (stup->ajim->yo != 0.0f)   ;
     if( stup->abdim == 3 && !stup->ascali )
       stup->ascali = (stup->ajim->dz != 1.0f) || (stup->ajim->zo != 0.0f) ;
   }

   /* smooth and save target image if needed */

   need_smooth = (stup->smooth_code > 0 && stup->smooth_radius > 0.0f) ;
   do_smooth   = need_smooth && ( stup->smooth_code   != stup->old_sc ||
                                  stup->smooth_radius != stup->old_sr   ) ;
   stup->old_sc = stup->smooth_code   ;
   stup->old_sr = stup->smooth_radius ;

   if( !need_smooth ){
     if( stup->ajims != NULL ){ mri_free(stup->ajims); stup->ajims = NULL; }
     if( stup->bsims != NULL ){ mri_free(stup->bsims); stup->bsims = NULL; }
     stup->old_sc = -1 ; stup->old_sr = -1.0f ;
     if( verb > 1 ) ININFO_message("- Smoothing disabled") ;
   }
   if( do_smooth || (need_smooth && stup->bsims == NULL) ){
     if( stup->bsims != NULL ) mri_free(stup->bsims);
     if( verb > 1 ) ININFO_message("- Smoothing base; radius=%.2f",stup->smooth_radius) ;
     stup->bsims = GA_smooth( stup->bsim , stup->smooth_code ,
                                           stup->smooth_radius ) ;
   }
   if( do_smooth || (need_smooth && stup->ajims == NULL) ){
     double nxa=stup->ajim->nx, nya=stup->ajim->ny, nza=stup->ajim->nz ;
     float rad=cbrt(nxa*nya*nza/(nx*ny*nz)) * stup->smooth_radius ;
     if( stup->ajims != NULL ) mri_free(stup->ajims);
     if( verb > 1 )
       ININFO_message("- Smoothing source; radius=%.2f",stup->smooth_radius);
     stup->ajims = GA_smooth( stup->ajim , stup->smooth_code , rad ) ;
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
     if( stup->nvox_mask != stup->bsim->nvox )
       ERREX("old mask and new base image differ in size") ;

     if( verb > 1 ) ININFO_message("- retaining old weight image") ;

   } else {                           /*---- have no mask, new or old ----*/
     stup->bmask = NULL ;
     stup->nmask = stup->nvox_mask = 0 ;
     stup->bwght = NULL ;
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
         KILL_floatvec(stup->im); KILL_floatvec(stup->jm); KILL_floatvec(stup->km);
       }
       stup->im = stup->jm = stup->km = NULL ;

     } else if( use_all == 2 ){  /*------------- all points in mask ------------*/

       int nvox , pp ; byte *mask=stup->bmask ;

       if( stup->im != NULL ){
         KILL_floatvec(stup->im); KILL_floatvec(stup->jm); KILL_floatvec(stup->km);
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
         KILL_floatvec(stup->im); KILL_floatvec(stup->jm); KILL_floatvec(stup->km);
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
       if( stup->use_cmat ){   /* 24 Aug 2006 */
         float x,y,z ;
         STATUS("applying base_cmat") ;
         for( qq=0 ; qq < stup->npt_match ; qq++ ){
           x = stup->im->ar[qq]; y = stup->jm->ar[qq]; z = stup->km->ar[qq];
           MAT44_VEC( stup->base_cmat , x,y,z ,
                      stup->im->ar[qq] , stup->jm->ar[qq] , stup->km->ar[qq] ) ;
         }
       } else if( stup->bscali ){
         float xo=stup->bsim->xo , dx=stup->bsim->dx ;
         float yo=stup->bsim->yo , dy=stup->bsim->dy ;
         float zo=stup->bsim->zo , dz=stup->bsim->dz ;
         STATUS("applying bscali") ;
         for( qq=0 ; qq < stup->npt_match ; qq++ ){
           stup->im->ar[qq] = xo + stup->im->ar[qq]*dx ;
           stup->jm->ar[qq] = yo + stup->jm->ar[qq]*dy ;
           stup->km->ar[qq] = zo + stup->km->ar[qq]*dz ;
         }
       }
     }

     /* do match_code specific pre-processing of the extracted data */

     switch( stup->match_code ){
       case GA_MATCH_SPEARMAN_SCALAR:
         STATUS("doing spearman_rank_prepare") ;
         stup->bvstat = spearman_rank_prepare( stup->npt_match, stup->bvm->ar );
       break ;
     }

   } /* end of if(need_pts) */

   if( verb > 1 ) ININFO_message("* Exit alignment setup routine") ;
   stup->setup = SMAGIC ;
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

float mri_genalign_scalar_cost( GA_setup *stup )
{
   double *wpar , val ;
   int ii , qq ;

   if( stup == NULL || stup->setup != SMAGIC ){
     ERROR_message("Illegal call to mri_genalign_scalar_cost()") ;
     return BIGVAL ;
   }

   GA_param_setup(stup) ;
   if( stup->wfunc_numfree <= 0 ) return BIGVAL ;

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

   val = GA_scalar_fitter( stup->wfunc_numfree , wpar ) ;

   free((void *)wpar) ; return (float)val ;
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

   icod = stup->interp_code ; stup->interp_code = MRI_NN ;

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

ENTRY("mri_genalign_scalar_warpim") ;

   if( stup       == NULL || stup->setup != SMAGIC ||
       stup->ajim == NULL || stup->bsim  == NULL     ){
     ERROR_message("Illegal call to mri_genalign_scalar_warpim()") ;
     RETURN(NULL) ;
   }
   gstup = stup ;

   wim = mri_new_conforming( stup->bsim , MRI_float ) ;
   war = MRI_FLOAT_PTR(wim) ;

   GA_get_warped_values( 0 , NULL , war ) ;

   RETURN(wim) ;
}

/*-------------------------------------------------------------------------*/
/*! Warp an image to base coords, on an nnx X nny X nnz grid.
     - The mapping between ijk and base xyz coords is given by cmat_base.
     - The warping between base xyz and target xyz is given by the
       wfunc, which has npar parameters stored in wpar.
     - The mapping between ijk and target xyz is given by cmat_targ.
     - The interpolation method is in icode.
     - Output is in float format, no matter what input data format was.
     - Generalized from GA_get_warped_values() -- RWCox - 26 Sep 2006.
---------------------------------------------------------------------------*/

MRI_IMAGE * mri_genalign_scalar_warpone( int npar, float *wpar, GA_warpfunc *wfunc,
                                         MRI_IMAGE *imtarg ,
                                         mat44 cmat_base , mat44 cmat_targ ,
                                         int nnx , int nny , int nnz , int icode )
{
   int   ii,jj,kk,qq,pp,npp,mm,nx,ny,nxy,nz , npt ;
   float x,y,z ;
   float *imf , *jmf , *kmf ;
   float *imw , *jmw , *kmw ;
   MRI_IMAGE *wim , *inim ;
   float     *war , *inar ;
   mat44 imat_targ ;

ENTRY("mri_genalign_scalar_warpone") ;

   if( wfunc == NULL || imtarg == NULL ) RETURN(NULL) ;

   /* send parameters to warping function */

   wfunc( npar , wpar , 0,NULL,NULL,NULL , NULL,NULL,NULL ) ;

   /* create float copy of input image, if needed */

   if( imtarg->kind == MRI_float ) inim = imtarg ;
   else                            inim = mri_to_float(imtarg) ;
   inar = MRI_FLOAT_PTR(inim) ;

   /* dimensions of output image */

   nx = nnx ; ny = nny ; nz = nnz ; nxy = nx*ny ; npt = nxy * nz ;
   wim = mri_new_vol( nx,ny,nz , MRI_float ) ;
   war = MRI_FLOAT_PTR(wim) ;

   /* xyz coordinates in base image to be warped to target coords */

   imf = (float *)calloc(sizeof(float),NPER) ;
   jmf = (float *)calloc(sizeof(float),NPER) ;
   kmf = (float *)calloc(sizeof(float),NPER) ;

   /* xyz coordinates after warping */

   imw = (float *)calloc(sizeof(float),NPER) ;
   jmw = (float *)calloc(sizeof(float),NPER) ;
   kmw = (float *)calloc(sizeof(float),NPER) ;

   /* map to take warped xyz to target image ijk for interpolation */

   imat_targ = nifti_mat44_inverse( cmat_targ ) ;

   /*--- do (up to) NPER points at a time ---*/

   for( pp=0 ; pp < npt ; pp+=NPER ){
     npp = MIN( NPER , npt-pp ) ;      /* number to do */

     /* get base xyz coords */

     for( qq=0 ; qq < npp ; qq++ ){
       mm = pp+qq ;
       ii = mm % nx; kk = mm / nxy; jj = (mm-kk*nxy) / nx;
       x = (float)ii; y = (float)jj; z = (float)kk;
       MAT44_VEC( cmat_base , x,y,z , imf[qq] , jmf[qq] , kmf[qq] ) ;
     }

     /* warp base points to new locations */

     wfunc( npar , NULL , npp  , imf,jmf,kmf , imw,jmw,kmw ) ;

     /* convert back to target image ijk values */

     for( qq=0 ; qq < npp ; qq++ ){
       x = imw[qq] ; y = jmw[qq] ; z = kmw[qq] ;
       MAT44_VEC( imat_targ , x,y,z ,
                  imw[qq] , jmw[qq] , kmw[qq] ) ;
     }

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

/*--------------------------------------------------------------------------*/
/*! Compute a rotation matrix specified by 3 angles:
      Q = R3 R2 R1, where Ri is rotation about axis axi by angle thi.
----------------------------------------------------------------------------*/

static THD_mat33 rot_matrix( int ax1, double th1,
                             int ax2, double th2, int ax3, double th3  )
{
   THD_mat33 q , p ;
   LOAD_ROT_MAT( q , th1 , ax1 ) ;
   LOAD_ROT_MAT( p , th2 , ax2 ) ; q = MAT_MUL( p , q ) ;
   LOAD_ROT_MAT( p , th3 , ax3 ) ; q = MAT_MUL( p , q ) ;
   return q ;
}

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

static THD_vecmat GA_setup_affine( int npar , float *parvec )
{
   THD_mat33 ss,dd,uu,aa,bb ;
   THD_fvec3 vv ;
   float     a,b,c ;
   THD_vecmat mv_for ;

   /* rotation */

   a = b = c = 0.0f ;
   if( npar >= 4 ) a = D2R*parvec[3] ;
   if( npar >= 5 ) b = D2R*parvec[4] ;
   if( npar >= 6 ) c = D2R*parvec[5] ;
   if( a != 0.0f || b != 0.0f || c != 0.0f )
     uu = rot_matrix( 2,a , 0,b , 1,c ) ;
   else
     LOAD_DIAG_MAT( uu , 1.0f,1.0f,1.0f ) ;

   /* scaling */

   a = b = c = 1.0f ;
   if( npar >= 7 ){ a = parvec[6]; if( a <= 0.10f || a >= 10.0f ) a = 1.0f; }
   if( npar >= 8 ){ b = parvec[7]; if( b <= 0.10f || b >= 10.0f ) b = 1.0f; }
   if( npar >= 9 ){ c = parvec[8]; if( c <= 0.10f || c >= 10.0f ) c = 1.0f; }
   LOAD_DIAG_MAT( dd , a,b,c ) ;

   /* shear */

   a = b = c = 0.0f ;
   if( npar >= 10 ){ a = parvec[ 9]; if( fabsf(a) > 0.3333f ) a = 0.0f; }
   if( npar >= 11 ){ b = parvec[10]; if( fabsf(b) > 0.3333f ) b = 0.0f; }
   if( npar >= 12 ){ c = parvec[11]; if( fabsf(c) > 0.3333f ) c = 0.0f; }
   switch( smat ){
     default:
     case SMAT_LOWER: LOAD_MAT( ss , 1.0 , 0.0 , 0.0 ,
                                      a  , 1.0 , 0.0 ,
                                      b  ,  c  , 1.0  ) ; break ;

     case SMAT_UPPER: LOAD_MAT( ss , 1.0 ,  a  ,  b ,
                                     0.0 , 1.0 ,  c ,
                                     0.0 , 0.0 , 1.0  ) ; break ;

     case SMAT_XXX:   LOAD_MAT( ss , 1.0 ,  a  ,  b ,
                                     0.0 , 1.0 , 0.0,
                                     0.0 , 0.0 , 1.0  ) ; break ;

     case SMAT_YYY:   LOAD_MAT( ss , 1.0 , 0.0 , 0.0,
                                      a  , 1.0 ,  b ,
                                     0.0 , 0.0 , 1.0  ) ; break ;

     case SMAT_ZZZ:   LOAD_MAT( ss , 1.0 , 0.0 , 0.0,
                                     0.0 , 1.0 , 0.0,
                                      a  ,  b  , 1.0  ) ; break ;
   }

   /* multiply them, as ordered */

   switch( matorder ){
     default:
     case MATORDER_SDU:  aa = MAT_MUL(ss,dd) ; bb = uu ; break ;
     case MATORDER_SUD:  aa = MAT_MUL(ss,uu) ; bb = dd ; break ;
     case MATORDER_DSU:  aa = MAT_MUL(dd,ss) ; bb = uu ; break ;
     case MATORDER_DUS:  aa = MAT_MUL(dd,uu) ; bb = ss ; break ;
     case MATORDER_USD:  aa = MAT_MUL(uu,ss) ; bb = dd ; break ;
     case MATORDER_UDS:  aa = MAT_MUL(uu,dd) ; bb = ss ; break ;
   }
   mv_for.mm = MAT_MUL(aa,bb) ;

   /* shifts */

   a = b = c = 0.0f ;
   if( npar >= 1 ) a = parvec[0] ;
   if( npar >= 2 ) b = parvec[1] ;
   if( npar >= 3 ) c = parvec[2] ;
   LOAD_FVEC3( vv , a,b,c ) ;

   switch( dcode ){
     default:
     case DELTA_AFTER:  mv_for.vv = vv ;                      break ;
     case DELTA_BEFORE: mv_for.vv = MATVEC( mv_for.mm, vv ) ; break ;
   }

   return mv_for ;
}

/*--------------------------------------------------------------------------*/
/*! A wfunc function for affine transformations. */
/*--------------------------------------------------------------------------*/

void mri_genalign_affine( int npar, float *wpar ,
                          int npt , float *xi, float *yi, float *zi ,
                                    float *xo, float *yo, float *zo  )
{
   static THD_vecmat mv_for ;  /* saved transformation matrix */
   THD_fvec3 v , w ;
   int ii ;

   /** new parameters ==> setup matrix */

   if( npar > 0 && wpar != NULL )
     mv_for = GA_setup_affine( npar , wpar ) ;

   /* nothing to transform? */

   if( npt <= 0 || xi == NULL || xo == NULL ) return ;

   /* mutiply matrix times input vectors */

   for( ii=0 ; ii < npt ; ii++ ){
     LOAD_FVEC3( v , xi[ii],yi[ii],zi[ii] ) ;
     w = VECMAT_VEC( mv_for , v ) ;
     UNLOAD_FVEC3( w , xo[ii],yo[ii],zo[ii] ) ;
   }
   return ;
}
