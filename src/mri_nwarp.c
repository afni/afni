#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

typedef struct {
  int    nx ,  ny ,  nz ;
  float *xd , *yd , *zd , *hv ;
  float xxoff , yyoff , zzoff ,
        xxdel , yydel , zzdel  ;
} IndexWarp3D ;

#undef  FREEIFNN
#define FREEIFNN(x) do{ if((x)!=NULL) free((void *)(x)); } while(0)

/*---------------------------------------------------------------------------*/

IndexWarp3D * IW3D_create( int nx , int ny , int nz )
{
   IndexWarp3D *AA ;

   if( nx < 9 || ny < 9 || nz < 9 ) return NULL ;

   AA = (IndexWarp3D *)malloc(sizeof(IndexWarp3D)) ;
   AA->nx = nx ; AA->ny = ny ; AA->nz = nz ;
   AA->xd = (float *)calloc(nx*ny*nz,sizeof(float)) ;
   AA->yd = (float *)calloc(nx*ny*nz,sizeof(float)) ;
   AA->zd = (float *)calloc(nx*ny*nz,sizeof(float)) ;
   AA->hv = NULL ;
   AA->xxoff = AA->yyoff = AA->zzoff = 0.0f ;
   AA->xxdel = AA->yydel = AA->zzdel = 1.0f ;

   return AA ;
}

/*---------------------------------------------------------------------------*/

void IW3D_destroy( IndexWarp3D *AA )
{
   if( AA != NULL ){
     FREEIFNN(AA->xd); FREEIFNN(AA->yd); FREEIFNN(AA->zd); FREEIFNN(AA->hv);
     free(AA);
   }
   return ;
}

/*---------------------------------------------------------------------------*/

float IW3D_normL1( IndexWarp3D *AA , IndexWarp3D *BB )
{
   int qq , nxyz ; float sum , *xda,*yda,*zda ;

   if( AA == NULL ){
     if( BB == NULL ) return 0.0f ;
     AA = BB ; BB = NULL ;
   }

   nxyz = AA->nx * AA->ny * AA->nz ;
   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   sum = 0.0f ;
   if( BB == NULL ){
     for( qq=0 ; qq < nxyz ; qq++ )
       sum += fabsf(xda[qq])+fabsf(yda[qq])+fabsf(zda[qq]) ;
   } else if( BB->nx == AA->nx && BB->ny == AA->ny && BB->nz == AA->nz ){
     float *xdb=BB->xd , *ydb = BB->yd , *zdb = BB->zd ;
     for( qq=0 ; qq < nxyz ; qq++ )
       sum += fabsf(xda[qq]-xdb[qq])+fabsf(yda[qq]-ydb[qq])+fabsf(zda[qq]-zdb[qq]) ;
   }

   return sum ;
}

/*---------------------------------------------------------------------------*/

float IW3D_normL2( IndexWarp3D *AA , IndexWarp3D *BB )
{
   int qq , nxyz ; float sum , *xda,*yda,*zda ;

   if( AA == NULL ){
     if( BB == NULL ) return 0.0f ;
     AA = BB ; BB = NULL ;
   }

   nxyz = AA->nx * AA->ny * AA->nz ;
   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   sum = 0.0f ;
   if( BB == NULL ){
     for( qq=0 ; qq < nxyz ; qq++ )
       sum += SQR(xda[qq])+SQR(yda[qq])+SQR(zda[qq]) ;
   } else if( BB->nx == AA->nx && BB->ny == AA->ny && BB->nz == AA->nz ){
     float *xdb=BB->xd , *ydb = BB->yd , *zdb = BB->zd ;
     for( qq=0 ; qq < nxyz ; qq++ )
       sum += SQR(xda[qq]-xdb[qq])+SQR(yda[qq]-ydb[qq])+SQR(zda[qq]-zdb[qq]) ;
   }

   return sqrtf(sum) ;
}

/*---------------------------------------------------------------------------*/

IndexWarp3D * IW3D_empty_copy( IndexWarp3D *AA )
{
   IndexWarp3D *BB ; int nxyz ;

   if( AA == NULL ) return NULL ;

   BB = IW3D_create( AA->nx , AA->ny , AA->nz ) ;

   BB->xxoff = AA->xxoff ; BB->xxdel = AA->xxdel ;
   BB->yyoff = AA->yyoff ; BB->yydel = AA->yydel ;
   BB->zzoff = AA->zzoff ; BB->zzdel = AA->zzdel ;

   return BB ;
}

/*---------------------------------------------------------------------------*/

IndexWarp3D * IW3D_copy( IndexWarp3D *AA , float fac )
{
   IndexWarp3D *BB ; int nxyz ;

   if( AA == NULL ) return NULL ;

   BB = IW3D_empty_copy(AA) ;  /* all zero displacements */

   nxyz = AA->nx * AA->ny * AA->nz ;

   if( fac == 1.0f ){
     memcpy( BB->xd , AA->xd , sizeof(float)*nxyz ) ;
     memcpy( BB->yd , AA->yd , sizeof(float)*nxyz ) ;
     memcpy( BB->zd , AA->zd , sizeof(float)*nxyz ) ;
   } else if( fac != 0.0f ){
     int qq ;
     for( qq=0 ; qq < nxyz ; qq++ ){
       BB->xd[qq] = fac * AA->xd[qq] ;
       BB->yd[qq] = fac * AA->yd[qq] ;
       BB->zd[qq] = fac * AA->zd[qq] ;
     }
   }

   return BB ;
}

/*----------------------------------------------------------------------------*/
/* Volume of a hexahedron (distorted cube) given by 8 corners.
   Looking down from the top, the bottom plane points are numbered so:
       2 -- 3
       |    |  and the top plane is similar (add 4 to each index),
       0 -- 1  with point #(i+4) 'above' point #i.
*//*--------------------------------------------------------------------------*/

#undef  TRIPROD
#define TRIPROD(ax,ay,az,bx,by,bz,cx,cy,cz) ( (ax)*((by)*(cz)-(bz)*(cy)) \
                                             +(bx)*((cy)*(az)-(cz)*(ay)) \
                                             +(cx)*((ay)*(bz)-(az)*(by))  )
#undef  DA
#undef  DB
#undef  DC
#define DA(p,q) (p.a-q.a)
#define DB(p,q) (p.b-q.b)
#define DC(p,q) (p.c-q.c)

static INLINE float hexahedron_volume( float_triple x0 , float_triple x1 ,
                                       float_triple x2 , float_triple x3 ,
                                       float_triple x4 , float_triple x5 ,
                                       float_triple x6 , float_triple x7  )
{
   float xa,ya,za , xb,yb,zb , xc,yc,zc , vol ;

   xa = DA(x7,x1)+DA(x6,x0); ya = DB(x7,x1)+DB(x6,x0); za = DC(x7,x1)+DC(x6,x0);
   xb = DA(x7,x2)          ; yb = DB(x7,x2)          ; zb = DC(x7,x2) ;
   xc = DA(x3,x0)          ; yc = DB(x3,x0)          ; zc = DC(x3,x0) ;
   vol = TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   xa = DA(x6,x0)          ; ya = DB(x6,x0)          ; za = DC(x6,x0) ;
   xb = DA(x7,x2)+DA(x5,x0); yb = DB(x7,x2)+DB(x5,x0); zb = DC(x7,x2)+DC(x5,x0);
   xc = DA(x7,x4)          ; yc = DB(x7,x4)          ; zc = DC(x7,x4) ;
   vol += TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   xa = DA(x7,x1)          ; ya = DB(x7,x1)          ; za = DC(x7,x1) ;
   xb = DA(x5,x0)          ; yb = DB(x5,x0)          ; zb = DC(x5,x0) ;
   xc = DA(x7,x4)+DA(x3,x0); yc = DB(x7,x4)+DB(x3,x0); zc = DC(x7,x4)+DC(x3,x0);
   vol += TRIPROD(xa,ya,za,xb,yb,zb,xc,yc,zc) ;
   return (0.08333333f*vol) ;
}
#undef TRIPROD
#undef DA
#undef DB
#undef DC

/*---------------------------------------------------------------------------*/

#undef  IJK
#define IJK(i,j,k) ((i)+(j)*nx+(k)*nxy)

#undef  C2F
#define C2F(p,q,r,xx) ( (xx).a = (p) , (xx).b = (q) , (xx).c = (r) )

#undef  D2F
#define D2F(pqr,xx)   ( (xx).a+=xda[pqr], (xx).b+=yda[pqr], (xx).c+=zda[pqr] )

void IW3D_load_hexvol( IndexWarp3D *AA )
{
   float *xda, *yda , *zda , *hva ;
   int nx,ny,nz , nxy,nxyz ;

   if( AA == NULL ) return ;

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;

   hva = AA->hv ;
   if( hva == NULL ) hva = AA->hv = (float *)calloc(nxyz,sizeof(float)) ;

 AFNI_OMP_START ;
#pragma omp parallel if( nxyz > 33333 )
 { float_triple x0,x1,x2,x3,x4,x5,x6,x7 ;
   int ii,jj,kk , ip,jp,kp , ijk , qq ;
#pragma omp for
   ii = -1 ; jj = kk = 0 ;
   for( qq=0 ; qq < nxyz ; qq++ ){
     ii++; if( ii == nx ){ ii = 0; jj++; if( jj == ny ){ jj = 0; kk++; } }
     ip = ii+1 ; jp = jj+1 ; kp = kk+1 ;
     C2F(ii,jj,kk,x0); C2F(ip,jj,kk,x1); C2F(ii,jp,kk,x2); C2F(ip,jp,kk,x3);
     C2F(ii,jj,kp,x4); C2F(ip,jj,kp,x5); C2F(ii,jp,kp,x6); C2F(ip,jp,kp,x7);
     if( ip == nx ) ip-- ; if( jp == ny ) jp-- ; if( kp == nz ) kp-- ;
     ijk = IJK(ip,jj,kk) ; D2F(ijk,x1) ;
     ijk = IJK(ii,jp,kk) ; D2F(ijk,x2) ;
     ijk = IJK(ip,jp,kk) ; D2F(ijk,x3) ;
     ijk = IJK(ii,jj,kp) ; D2F(ijk,x4) ;
     ijk = IJK(ip,jj,kp) ; D2F(ijk,x5) ;
     ijk = IJK(ii,jp,kp) ; D2F(ijk,x6) ;
     ijk = IJK(ip,jp,kp) ; D2F(ijk,x7) ;
     ijk = qq            ; D2F(ijk,x0) ;
     hva[qq] = hexahedron_volume(x0,x1,x2,x3,x4,x5,x6,x7) ;
   }
 }
 AFNI_OMP_END ;

 return ;
}

/*---------------------------------------------------------------------------*/

#undef  CLIP
#define CLIP(mm,nn) if(mm < 0)mm=0; else if(mm > nn)mm=nn

#undef  AJK
#define AJK(aaa,j,k) ((aaa)+(j)*nx+(k)*nxy)

/*---------------------------------------------------------------------------*/
/*! Interpolate using linear method */

void IW3D_interp_linear( int nxx , int nyy , int nzz ,
                         float *aar , float *bar , float *car ,
                         int npp, float *ip, float *jp, float *kp,
                         float *uar , float *var , float *war     )
{
ENTRY("IW3D_interp_linear") ;

 AFNI_OMP_START ;
#pragma omp parallel if(npp > 9999)
 {
   int nx=nxx, ny=nyy, nz=nzz, nxy=nx*ny, pp, nx1=nx-1,ny1=ny-1,nz1=nz-1 ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float fx,fy,fz , ix,jy,kz ;
   int   ix_00,ix_p1 , jy_00,jy_p1 , kz_00,kz_p1 ;
   float wt_00,wt_p1 ;
   float f_j00_k00, f_jp1_k00, f_j00_kp1, f_jp1_kp1, f_k00, f_kp1 ;
   float g_j00_k00, g_jp1_k00, g_j00_kp1, g_jp1_kp1, g_k00, g_kp1 ;
   float h_j00_k00, h_jp1_k00, h_j00_kp1, h_jp1_kp1, h_k00, h_kp1 ;

#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}

     ix = floorf(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floorf(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floorf(zz) ;  fz = zz - kz ;

     ix_00 = ix ; ix_p1 = ix_00+1 ; CLIP(ix_00,nx1) ; CLIP(ix_p1,nx1) ;
     jy_00 = jy ; jy_p1 = jy_00+1 ; CLIP(jy_00,ny1) ; CLIP(jy_p1,ny1) ;
     kz_00 = kz ; kz_p1 = kz_00+1 ; CLIP(kz_00,nz1) ; CLIP(kz_p1,nz1) ;

     wt_00 = 1.0f-fx ; wt_p1 = fx ;  /* weights for ix_00 and ix_p1 points */

#undef  XINT
#define XINT(aaa,j,k) wt_00*aaa[IJK(ix_00,j,k)]+wt_p1*aaa[IJK(ix_p1,j,k)]

     /* interpolate to location ix+fx at each jy,kz level */

     f_j00_k00 = XINT(aar,jy_00,kz_00) ; f_jp1_k00 = XINT(aar,jy_p1,kz_00) ;
     f_j00_kp1 = XINT(aar,jy_00,kz_p1) ; f_jp1_kp1 = XINT(aar,jy_p1,kz_p1) ;
     g_j00_k00 = XINT(bar,jy_00,kz_00) ; g_jp1_k00 = XINT(bar,jy_p1,kz_00) ;
     g_j00_kp1 = XINT(bar,jy_00,kz_p1) ; g_jp1_kp1 = XINT(bar,jy_p1,kz_p1) ;
     h_j00_k00 = XINT(car,jy_00,kz_00) ; h_jp1_k00 = XINT(car,jy_p1,kz_00) ;
     h_j00_kp1 = XINT(car,jy_00,kz_p1) ; h_jp1_kp1 = XINT(car,jy_p1,kz_p1) ;

     /* interpolate to jy+fy at each kz level */

     wt_00 = 1.0f-fy ; wt_p1 = fy ;
     f_k00 =  wt_00 * f_j00_k00 + wt_p1 * f_jp1_k00 ;
     f_kp1 =  wt_00 * f_j00_kp1 + wt_p1 * f_jp1_kp1 ;
     g_k00 =  wt_00 * g_j00_k00 + wt_p1 * g_jp1_k00 ;
     g_kp1 =  wt_00 * g_j00_kp1 + wt_p1 * g_jp1_kp1 ;
     h_k00 =  wt_00 * h_j00_k00 + wt_p1 * h_jp1_k00 ;
     h_kp1 =  wt_00 * h_j00_kp1 + wt_p1 * h_jp1_kp1 ;

     /* interpolate to kz+fz to get output */

     uar[pp] = (1.0f-fz) * f_k00 + fz * f_kp1 ;
     var[pp] = (1.0f-fz) * g_k00 + fz * g_kp1 ;
     war[pp] = (1.0f-fz) * h_k00 + fz * h_kp1 ;
   }

 } /* end OpenMP */
 AFNI_OMP_END ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Interpolation with weighted (tapered) sinc in 3D.
   ++ Taper function wtap(r) is defined to be 1 for 0 <= r <= WCUT
       and for WCUT < r < 1 is a raised c sine dropping down to wtap(r=1) = 0.
       This choice was made to keep the variance smoothing artifact low.
   ++ Radius of sinc window is WRAD, so the actual taper used is wtap(x/WRAD)
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

/* Weight (taper) function, declining from wtap(WCUT)=1 to wtap(1)=0 */
/* Note that the input to wtap will always be between WCUT and 1.   */

#undef  wtap
#define wtap(x) ( 0.5f+0.5f*cosf(PIF*((x)-WCUT)/(1.0f-WCUT)) )

#undef AW
#undef BW
#undef CW
#define AW(i) aarjk[iqq[i]]*wtt[i]
#define BW(i) barjk[iqq[i]]*wtt[i]
#define CW(i) carjk[iqq[i]]*wtt[i]

/*---------------------------------------------------------------------------*/

void IW3D_interp_wsinc5( int nxx , int nyy , int nzz ,
                         float *aar , float *bar , float *car ,
                         int npp, float *ip, float *jp, float *kp,
                         float *uar , float *var , float *war     )
{
ENTRY("IW3D_interp_wsinc5") ;

 AFNI_OMP_START ;
#pragma omp parallel if(npp > 6666)
 {
   int nx=nxx , ny=nyy , nz=nzz , nxy=nx*ny , pp ;
   float nxh=nx-0.501f , nyh=ny-0.501f , nzh=nz-0.501f , xx,yy,zz ;
   float fx,fy,fz ;
   float *aarjk , *barjk , *carjk ;
   int nx1=nx-1,ny1=ny-1,nz1=nz-1, ix,jy,kz ;

   float xw,yw,zw,rr , asum,bsum,csum,wsum,wfac,wt ;
   int   iq,jq,kq,iqp , qq,jj,kk , ddi,ddj,ddk ;
   float xsin[2*IRAD] , ysin[2*IRAD]        , zsin[2*IRAD] ;
   float wtt[2*IRAD]  , ajk[2*IRAD][2*IRAD] , ak[2*IRAD]   ;
   float                bjk[2*IRAD][2*IRAD] , bk[2*IRAD]   ;
   float                cjk[2*IRAD][2*IRAD] , ck[2*IRAD]   ;
   int   iqq[2*IRAD]  ;
   /*----- loop over points -----*/

#pragma omp for
   for( pp=0 ; pp < npp ; pp++ ){
     xx = ip[pp] ; if( xx < -0.499f || xx > nxh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
     yy = jp[pp] ; if( yy < -0.499f || yy > nyh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}
     zz = kp[pp] ; if( zz < -0.499f || zz > nzh ){ uar[pp]=var[pp]=war[pp]=0.0f; continue;}

     ix = floorf(xx) ;  fx = xx - ix ;   /* integer and       */
     jy = floorf(yy) ;  fy = yy - jy ;   /* fractional coords */
     kz = floorf(zz) ;  fz = zz - kz ;

     /*- x interpolations -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){  /* weights */
       xw  = fabsf(fx - qq) ; wt = sinc(xw) ;
       xw /= WRAD ; if( xw > WCUT ) wt *= wtap(xw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
       iq = ix+qq ; CLIP(iq,nx1) ; iqq[qq+(IRAD-1)] = iq ;
     }
     wfac = wsum ;

     for( jj=-IRAD+1 ; jj <= IRAD ; jj++ ){  /* interps */
       jq = jy+jj ; CLIP(jq,ny1) ;
       for( kk=-IRAD+1 ; kk <= IRAD ; kk++ ){
         kq = kz+kk ; CLIP(kq,nz1) ;
         aarjk = AJK(aar,jq,kq) ;
         barjk = AJK(bar,jq,kq) ;
         carjk = AJK(car,jq,kq) ;
         ajk[jj+(IRAD-1)][kk+(IRAD-1)] =
           AW(0)+AW(1)+AW(2)+AW(3)+AW(4)+AW(5)+AW(6)+AW(7)+AW(8)+AW(9) ;
         bjk[jj+(IRAD-1)][kk+(IRAD-1)] =
           BW(0)+BW(1)+BW(2)+BW(3)+BW(4)+BW(5)+BW(6)+BW(7)+BW(8)+BW(9) ;
         cjk[jj+(IRAD-1)][kk+(IRAD-1)] =
           CW(0)+CW(1)+CW(2)+CW(3)+CW(4)+CW(5)+CW(6)+CW(7)+CW(8)+CW(9) ;
       }
     }

     /*- y interpolations -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){  /* weights */
       yw  = fabsf(fy - qq) ; wt = sinc(yw) ;
       yw /= WRAD ; if( yw > WCUT ) wt *= wtap(yw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
     }
     wfac *= wsum ;

     for( kk=-IRAD+1 ; kk <= IRAD ; kk++ ){  /* interps */
       for( asum=bsum=csum=0.0f,jj=-IRAD+1 ; jj <  IRAD ; jj+=2 ){  /* unrolled by 2 */
         asum += wtt[jj+(IRAD-1)]*ajk[jj+(IRAD-1)][kk+(IRAD-1)]
                +wtt[jj+ IRAD   ]*ajk[jj+ IRAD   ][kk+(IRAD-1)] ;
         bsum += wtt[jj+(IRAD-1)]*bjk[jj+(IRAD-1)][kk+(IRAD-1)]
                +wtt[jj+ IRAD   ]*bjk[jj+ IRAD   ][kk+(IRAD-1)] ;
         csum += wtt[jj+(IRAD-1)]*cjk[jj+(IRAD-1)][kk+(IRAD-1)]
                +wtt[jj+ IRAD   ]*cjk[jj+ IRAD   ][kk+(IRAD-1)] ;
       }
       ak[kk+(IRAD-1)] = asum ;
       bk[kk+(IRAD-1)] = bsum ;
       ck[kk+(IRAD-1)] = csum ;
     }

     /*- z interpolation -*/

     for( wsum=0.0f,qq=-IRAD+1 ; qq <= IRAD ; qq++ ){  /* weights */
       zw  = fabsf(fz - qq) ; wt = sinc(zw) ;
       zw /= WRAD ; if( zw > WCUT ) wt *= wtap(zw) ;
       wtt[qq+(IRAD-1)] = wt ; wsum += wt ;
     }
     wfac *= wsum ;

     /* interps */

     for( asum=bsum=csum=0.0f,kk=-IRAD+1 ; kk <  IRAD ; kk+=2 ){  /* unrolled by 2 */
       asum += wtt[kk+(IRAD-1)] * ak[kk+(IRAD-1)] + wtt[kk+IRAD] * ak[kk+IRAD] ;
       bsum += wtt[kk+(IRAD-1)] * bk[kk+(IRAD-1)] + wtt[kk+IRAD] * bk[kk+IRAD] ;
       csum += wtt[kk+(IRAD-1)] * ck[kk+(IRAD-1)] + wtt[kk+IRAD] * ck[kk+IRAD] ;
     }

     uar[pp] = asum / wfac ;
     var[pp] = bsum / wfac ;
     war[pp] = csum / wfac ;
   }

 } /* end OpenMP */
 AFNI_OMP_END ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

void IW3D_interp( int icode ,
                  int nxx , int nyy , int nzz ,
                  float *aar , float *bar , float *car ,
                  int npp, float *ip, float *jp, float *kp,
                  float *uar , float *var , float *war     )
{
   switch( icode ){
     case MRI_NN:
     case MRI_LINEAR:
       IW3D_interp_linear( nxx , nyy , nzz , aar , bar , car ,
                           npp , ip  , jp  , kp  , uar , var , war ) ;
     break ;

     default:
     case MRI_WSINC5:
       IW3D_interp_wsinc5( nxx , nyy , nzz , aar , bar , car ,
                           npp , ip  , jp  , kp  , uar , var , war ) ;
     break ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/

#undef  NPER
#define NPER 524288  /* 2 Mbyte per float array */

/*---------------------------------------------------------------------------*/
/* Compute B(A(x)) */

IndexWarp3D * IW3D_compose( IndexWarp3D *AA , IndexWarp3D *BB , int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , ii,jj,kk , pp,qq,qtop;
   float *xda,*yda,*zda , *xq,*yq,*zq , *xdc,*ydc,*zdc ;
   IndexWarp3D *CC ;

ENTRY("IW3D_compose") ;

        if( AA == NULL ){ CC = IW3D_copy(BB,1.0f) ; RETURN(CC) ; }
   else if( BB == NULL ){ CC = IW3D_copy(AA,1.0f) ; RETURN(CC) ; }

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = MIN(nxyz,NPER) ;

   xq = (float *)malloc(sizeof(float)*nall) ;
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;

   CC = IW3D_empty_copy(AA) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

   ii = -1 ; jj = kk = 0 ;
   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */

     for( qq=pp ; qq < qtop ; qq++ ){
       ii++ ;          /* get the 3D index from the 1D index */
       if( ii == nx ){ /* allowing for wraparounds at nx, ny */
         ii = 0 ; jj++ ; if( jj == ny ){ jj = 0 ; kk++ ; }
       }
       xq[qq-pp] = ii + xda[qq] ;  /* x+A(x) warped indexes */
       yq[qq-pp] = jj + yda[qq] ;
       zq[qq-pp] = kk + zda[qq] ;
     }

     /* Interpolate B() warp index displacments at the A() locations */

     IW3D_interp( icode, nx,ny,nz , BB->xd, BB->yd, BB->zd ,
                         qtop-pp  , xq    , yq    , zq     ,
                                    xdc+pp, ydc+pp, zdc+pp  ) ;

     /* Add in the A() displacments to get the total
        index displacment from each original position: A(x) + B(x+A(x)) */

     for( qq=pp ; qq < qtop ; qq++ ){
       xdc[qq] += xda[qq] ; ydc[qq] += yda[qq] ; zdc[qq] += zda[qq] ;
     }

   } /* end of loop over segments of length NPER (or less) */

   free(zq) ; free(yq) ; free(xq) ; RETURN(CC) ;
}

/*---------------------------------------------------------------------------*/
/* Compute B( 2x - A(B(x)) ) = Newton step for computing Ainv(x) */

static IndexWarp3D * IW3D_invert_newt( IndexWarp3D *AA, IndexWarp3D *BB, int icode )
{
   int nx,ny,nz,nxy,nxyz , nall , ii,jj,kk,iold,jold,kold , pp,qq,qtop ;
   float *xda,*yda,*zda , *xq,*yq,*zq,*xr,*yr,*zr , *xdc,*ydc,*zdc , *xdb,*ydb,*zdb ;
   IndexWarp3D *CC ;

ENTRY("IW3D_invert_newt") ;

   if( AA == NULL || BB == NULL ) RETURN(NULL) ;  /* stoopidd luser */

   nx = AA->nx ; ny = AA->ny ; nz = AA->nz ; nxy = nx*ny ; nxyz = nxy*nz ;

   if( nx != BB->nx || ny != BB->ny || nz != BB->nz ) RETURN(NULL) ;

   nall = MIN(nxyz,NPER) ;

   xq = (float *)malloc(sizeof(float)*nall) ;  /* workspace */
   yq = (float *)malloc(sizeof(float)*nall) ;
   zq = (float *)malloc(sizeof(float)*nall) ;
   xr = (float *)malloc(sizeof(float)*nall) ;
   yr = (float *)malloc(sizeof(float)*nall) ;
   zr = (float *)malloc(sizeof(float)*nall) ;

   CC = IW3D_empty_copy(AA) ;

   xda = AA->xd ; yda = AA->yd ; zda = AA->zd ;
   xdb = BB->xd ; ydb = BB->yd ; zdb = BB->zd ;
   xdc = CC->xd ; ydc = CC->yd ; zdc = CC->zd ;

   /* Warps are stored as displacements, so we have
      A(x)              = x + a(x)
      B(x)              = x + b(x),
      A(B(x))           = x + b(x) + a(x+b(x))
      2x - A(B(x))      = x - b(x) - a(x+b(x))
      B( 2x - A(B(x)) ) = x - b(x) - a(x+b(x)) + b(x-b(x)-a(x+b(x))) */

   ii = -1 ; jj = kk = 0 ;
   for( pp=0 ; pp < nxyz ; pp+=nall ){  /* loop over segments */

     qtop = MIN( nxyz , pp+nall ) ;  /* process points from pp to qtop-1 */
     iold = ii ; jold = jj ; kold = kk ;

     /* Compute [xq,yq,zq] = x+b(x) */

     for( qq=pp ; qq < qtop ; qq++ ){
       ii++ ;          /* get the 3D index from the 1D index */
       if( ii == nx ){ /* allowing for wraparounds at nx, ny */
         ii = 0 ; jj++ ; if( jj == ny ){ jj = 0 ; kk++ ; }
       }
       xq[qq-pp] = ii + xdb[qq] ;
       yq[qq-pp] = jj + ydb[qq] ;
       zq[qq-pp] = kk + zdb[qq] ;
     }

     /* Compute [xr,yr,zr] = a(x+b(x)) */

     IW3D_interp( icode, nx,ny,nz , xda, yda, zda,
                         qtop-pp  , xq , yq , zq ,
                                    xr , yr , zr  ) ;

     /* Compute [xr,yr,zr] = x - b(x) - a(x+b(x)) */

     ii = iold ; jj = jold ; kk = kold ;
     for( qq=pp ; qq < qtop ; qq++ ){
       ii++; if( ii == nx ){ ii = 0; jj++; if( jj == ny ){ jj = 0; kk++; } }
       xr[qq-pp] = ii - xdb[qq] - xr[qq-pp] ;
       yr[qq-pp] = jj - ydb[qq] - yr[qq-pp] ;
       zr[qq-pp] = kk - zdb[qq] - zr[qq-pp] ;
     }

     /* Compute [xq,yq,zq] = b(x-b(x)-a(x+b(x))) */

     IW3D_interp( icode, nx,ny,nz , xdb, ydb, zdb,
                         qtop-pp  , xr , yr , zr ,
                                    xq , yq , zq  ) ;

     /* Compute result = -b(x) - a(x+b(x)) + b(x-b(x)-a(x+b(x))) */

     ii = iold ; jj = jold ; kk = kold ;
     for( qq=pp ; qq < qtop ; qq++ ){
       ii++; if( ii == nx ){ ii = 0; jj++; if( jj == ny ){ jj = 0; kk++; } }
       xdc[qq] = xr[qq-pp] - ii + xq[qq-pp] ;
       ydc[qq] = yr[qq-pp] - jj + yq[qq-pp] ;
       zdc[qq] = zr[qq-pp] - kk + zq[qq-pp] ;
     }

   } /* end of loop over segments of length NPER (or less) */

   free(zr); free(yr); free(xr); free(zq); free(yq); free(xq); RETURN(CC);
}

/*---------------------------------------------------------------------------*/

IndexWarp3D * IW3D_invert( IndexWarp3D *AA , int icode )
{
   IndexWarp3D *BB , *CC ;
   float normAA , normBC ;
   int ii , jcode=MRI_LINEAR ;

ENTRY("IW3D_invert") ;

   if( AA == NULL ) RETURN(NULL) ;

   normAA = IW3D_normL1( AA , NULL ) ;
   if( normAA == 0.0f ){
     BB = IW3D_empty_copy(AA) ; RETURN(BB) ;
   }

   /* initial guess at inverse */

   BB = IW3D_copy( AA , -0.666f ) ;  /* how beastly */

   /* take 2 Newton steps: BB -> CC -> BB */

   CC = IW3D_invert_newt(AA,BB,jcode) ; IW3D_destroy(BB) ;
   BB = IW3D_invert_newt(AA,CC,jcode) ;

   for( ii=0 ; ii < 19 ; ii++ ){

     normBC = IW3D_normL1( BB , CC ) ; IW3D_destroy(CC) ;

     if( normBC/normAA < 0.001f ){
       if( jcode == icode ) RETURN(BB) ;  /* converged */
       jcode = icode ;
     }

     /* take another Newton step */

     CC = BB ; BB = IW3D_invert_newt(AA,CC,jcode) ;
   }

   /* failed to converge */

   IW3D_destroy(CC) ; RETURN(BB) ;
}
