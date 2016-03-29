#include "mrilib.h"

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

static INLINE double hexahedron_volume( double_triple x0 , double_triple x1 ,
                                        double_triple x2 , double_triple x3 ,
                                        double_triple x4 , double_triple x5 ,
                                        double_triple x6 , double_triple x7  )
{
   double xa,ya,za , xb,yb,zb , xc,yc,zc , vol ;

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
   return (0.083333333333333*vol) ;
}


/*----------------------------------------------------------------------------*/
/* C1 Hermite cubics over [-1..1].
   Scale factors are adjusted so that the functions peak values are all 1.
   Return value is a float_pair comprising the 2 function values.
*//*--------------------------------------------------------------------------*/

static INLINE double_pair HCwarp_eval_basis( double x )
{
   register double aa , bb ; double_pair ee ;

   aa = fabs(x) ;
   if( aa >= 1.0 ){
     ee.a = ee.b = 0.0 ;
   } else {
     bb = 1.0 - aa ; bb = bb*bb ;
     ee.a = bb * (1.0+2.0*aa) ;  /* f(0)  = 1 ; */
     ee.b = bb * x * 6.75 ;      /* f'(0) = 1 * 6.75 */
   }
   return ee ;
}

/*............................................................*/

static INLINE double_quint HCwarp_eval_basis5( double x )
{
   register double aa , bb ; double_quint ee ;

   aa = fabs(x) ;
   if( aa >= 1.0f ){               /* outside range ==> 0 */
     ee.a = ee.b = ee.c = ee.d = ee.e = 0.0 ;
   } else {
     bb = 1.0 - aa ; bb = bb*bb ;
     ee.a = bb * (1.0+2.0*aa) ;    /* f(0)  = 1 ; */
     ee.b = bb * x * 6.75 ;        /* f'(0) = 1 * 6.75 */
     ee.c = bb * (1.0+2.0*aa-aa*aa*15.0) ;
     ee.d = bb * x * (1.0-aa*aa*5.0) * 9.75 ;
     ee.e = bb * (1.0+2.0*aa-aa*aa*57.0+aa*aa*aa*84.0) ;
   }
   return ee ;
}

/*----------------------------------------------------------------------------*/
/* C2 Hermite quintics over [-1..1] */

static INLINE double_triple HQwarp_eval_basis( double x )
{
   register double aa , bb , aq ; double_triple ee ;

   aa = fabs(x) ;
   if( aa >= 1.0 ){
     ee.a = ee.b = ee.c = 0.0 ;
   } else {
     bb = 1.0 - aa ; bb = bb*bb*bb ; aq = aa*aa ;
     ee.a = bb * ( (6.0*aq+3.0)*aa + 1.0 ) ;       /* f(0)   = 1 */
     ee.b = bb * x * (3.0*aa+1.0) * 5.0625 ;       /* f'(0)  = 1 * 5.0625 */
     ee.c = aq * bb * 28.935 ;                     /* f''(0) = 1 * 28.935 */
   }
   return ee ;
}

/*----------------------------------------------------------------------------*/

static int nb=0 ;
static double *b0=NULL , *b1=NULL , *b2=NULL , *cc=NULL , del=0.0 ;
static double *xx=NULL , *yy=NULL , *zz=NULL ;
static double *b3=NULL , *b4=NULL ;
static double *bar[5] ;

/*----------------------------------------------------------------------------*/

void HCwarp_setup_warp_basis( int ng )
{
   double_pair ee ;
   int ii ; double di ;

   if( b0 != NULL ){
     free(b0); free(b1); free(b2); free(cc); b0=b1=b2=cc=NULL ;
     free(xx); free(yy); free(zz); xx=yy=zz=NULL ; nb=0 ;
     free(b3); free(b4); b3=b4=NULL ;
   }
   if( ng < 9 ) return ;

   nb = ng ;
   bar[0] = b0 = (double *)malloc(sizeof(double)*nb) ;
   bar[1] = b1 = (double *)malloc(sizeof(double)*nb) ;
   bar[2] = b2 = (double *)malloc(sizeof(double)*nb) ;
   bar[3] = b3 = (double *)malloc(sizeof(double)*nb) ;
   bar[4] = b4 = (double *)malloc(sizeof(double)*nb) ;
   cc = (double *)malloc(sizeof(double)*nb) ;

   del = di = 2.0 / (nb-1.0) ;
   for( ii=0 ; ii < nb ; ii++ ){
     cc[ii] = -1.0 + ii*di ; ee = HCwarp_eval_basis(cc[ii]) ;
     b0[ii] = ee.a ; b1[ii] = ee.b ; b2[ii] = b3[ii] = b4[ii] = 0.0 ;
   }

   xx = (double *)malloc(sizeof(double)*nb*nb*nb) ;
   yy = (double *)malloc(sizeof(double)*nb*nb*nb) ;
   zz = (double *)malloc(sizeof(double)*nb*nb*nb) ;

   return ;
}

/*----------------------------------------------------------------------------*/

void HCwarp_setup_warp_basis5( int ng )
{
   double_quint ee ;
   int ii ; double di ;

   if( b0 != NULL ){
     free(b0); free(b1); free(b2); free(cc); b0=b1=b2=cc=NULL ;
     free(xx); free(yy); free(zz); xx=yy=zz=NULL ; nb=0 ;
     free(b3); free(b4); b3=b4=NULL ;
   }
   if( ng < 9 ) return ;

   nb = ng ;
   bar[0] = b0 = (double *)malloc(sizeof(double)*nb) ;
   bar[1] = b1 = (double *)malloc(sizeof(double)*nb) ;
   bar[2] = b2 = (double *)malloc(sizeof(double)*nb) ;
   bar[3] = b3 = (double *)malloc(sizeof(double)*nb) ;
   bar[4] = b4 = (double *)malloc(sizeof(double)*nb) ;
   cc = (double *)malloc(sizeof(double)*nb) ;

   del = di = 2.0 / (nb-1.0) ;
   for( ii=0 ; ii < nb ; ii++ ){
     cc[ii] = -1.0 + ii*di ; ee = HCwarp_eval_basis5(cc[ii]) ;
     b0[ii] = ee.a; b1[ii] = ee.b; b2[ii] = ee.c; b3[ii] = ee.d; b4[ii] = ee.e;
   }

   xx = (double *)malloc(sizeof(double)*nb*nb*nb) ;
   yy = (double *)malloc(sizeof(double)*nb*nb*nb) ;
   zz = (double *)malloc(sizeof(double)*nb*nb*nb) ;

   return ;
}

/*----------------------------------------------------------------------------*/

void HQwarp_setup_warp_basis( int ng )
{
   double_triple ee ;
   int ii ; double di ;

   if( b0 != NULL ){
     free(b0); free(b1); free(b2); free(cc); b0=b1=b2=cc=NULL ;
     free(xx); free(yy); free(zz); xx=yy=zz=NULL ; nb=0 ;
     free(b3); free(b4); b3=b4=NULL ;
   }
   if( ng < 9 ) return ;

   nb = ng ;
   bar[0] = b0 = (double *)malloc(sizeof(double)*nb) ;
   bar[1] = b1 = (double *)malloc(sizeof(double)*nb) ;
   bar[2] = b2 = (double *)malloc(sizeof(double)*nb) ;
   bar[3] = b3 = (double *)malloc(sizeof(double)*nb) ;
   bar[4] = b4 = (double *)malloc(sizeof(double)*nb) ;
   cc = (double *)malloc(sizeof(double)*nb) ;

   del = di = 2.0 / (nb-1.0) ;
   for( ii=0 ; ii < nb ; ii++ ){
     cc[ii] = -1.0 + ii*di ; ee = HQwarp_eval_basis(cc[ii]) ;
     b0[ii] = ee.a ; b1[ii] = ee.b ; b2[ii] = ee.c ; b3[ii] = b4[ii] = 0.0 ;
   }

   xx = (double *)malloc(sizeof(double)*nb*nb*nb) ;
   yy = (double *)malloc(sizeof(double)*nb*nb*nb) ;
   zz = (double *)malloc(sizeof(double)*nb*nb*nb) ;

   return ;
}

/*----------------------------------------------------------------------------*/
/* npar is 2*2*2*3 = 24 */

static char *fname = NULL ;

double HCwarp_minhexvol( int npar , double *par )
{
   double hv , mhv , ddd ;
   double *xpar=par , *ypar=par+8 , *zpar=par+16 ;
   int ii,jj,kk,hh , nbq ;
   double_triple x0,x1,x2,x3,x4,x5,x6,x7 ;
   MRI_IMAGE *him ; float *har=NULL ;

   if( npar < 24 ) return 0.0 ;         /* something bad */

   if( nb < 9 ) HCwarp_setup_warp_basis(51) ;

   ddd = del*del*del ;

   for( hh=kk=0 ; kk < nb ; kk++ ){
    for( jj=0 ; jj < nb ; jj++ ){
     for( ii=0 ; ii < nb ; ii++,hh++ ){
      xx[hh] = cc[ii]
                +b0[kk]*b0[jj]*b0[ii]*xpar[0] +b1[kk]*b0[jj]*b0[ii]*xpar[1]
                                              +b0[kk]*b1[jj]*b0[ii]*xpar[2]
                +b1[kk]*b1[jj]*b0[ii]*xpar[3] +b0[kk]*b0[jj]*b1[ii]*xpar[4]
                +b1[kk]*b0[jj]*b1[ii]*xpar[5]
                +b0[kk]*b1[jj]*b1[ii]*xpar[6] +b1[kk]*b1[jj]*b1[ii]*xpar[7] ;
      yy[hh] = cc[jj]
                +b0[kk]*b0[jj]*b0[ii]*ypar[0] +b1[kk]*b0[jj]*b0[ii]*ypar[1]
                                              +b0[kk]*b1[jj]*b0[ii]*ypar[2]
                +b1[kk]*b1[jj]*b0[ii]*ypar[3] +b0[kk]*b0[jj]*b1[ii]*ypar[4]
                +b1[kk]*b0[jj]*b1[ii]*ypar[5]
                +b0[kk]*b1[jj]*b1[ii]*ypar[6] +b1[kk]*b1[jj]*b1[ii]*ypar[7] ;
      zz[hh] = cc[kk]
                +b0[kk]*b0[jj]*b0[ii]*zpar[0] +b1[kk]*b0[jj]*b0[ii]*zpar[1]
                                              +b0[kk]*b1[jj]*b0[ii]*zpar[2]
                +b1[kk]*b1[jj]*b0[ii]*zpar[3] +b0[kk]*b0[jj]*b1[ii]*zpar[4]
                +b1[kk]*b0[jj]*b1[ii]*zpar[5]
                +b0[kk]*b1[jj]*b1[ii]*zpar[6] +b1[kk]*b1[jj]*b1[ii]*zpar[7] ;
   }}}

#undef  IJK
#undef  TOT
#define IJK(p,q,r) ( (p) + (q)*nb + (r)*nbq )
#define TOT(tt,p)  ( tt.a = xx[p] , tt.b = yy[p] , tt.c = zz[p] )

   mhv = 666.666 ; nbq = nb*nb ;

   if( fname != NULL ){
     him = mri_new_vol( nb,nb,nb , MRI_float ) ; har = MRI_FLOAT_PTR(him) ;
   }

   for( kk=0 ; kk < nb-1 ; kk++ ){
    for( jj=0 ; jj < nb-1 ; jj++ ){
      for( ii=0 ; ii < nb-1 ; ii++ ){
        hh = IJK(ii,jj,kk) ;
        TOT(x0,hh) ; TOT(x1,hh+1) ; TOT(x2,hh+nb) ; TOT(x3,hh+nb+1) ;
        hh += nbq ;
        TOT(x4,hh) ; TOT(x5,hh+1) ; TOT(x6,hh+nb) ; TOT(x7,hh+nb+1) ;
        hv = hexahedron_volume(x0,x1,x2,x3,x4,x5,x6,x7) / ddd ;
        if( hv < mhv ) mhv = hv ;
        if( har != NULL ) har[ii+jj*nb+kk*nbq] = hv ;
   }}}

   if( fname != NULL ){
     MRI_IMAGE *xim,*yim,*zim,*dim ; float *xar,*yar,*zar,*dar ;
     THD_3dim_dataset *dset ; THD_ivec3 iv_nxyz ;
     xim = mri_new_vol( nb,nb,nb , MRI_float ) ; xar = MRI_FLOAT_PTR(xim) ;
     yim = mri_new_vol( nb,nb,nb , MRI_float ) ; yar = MRI_FLOAT_PTR(yim) ;
     zim = mri_new_vol( nb,nb,nb , MRI_float ) ; zar = MRI_FLOAT_PTR(zim) ;
     dim = mri_new_vol( nb,nb,nb , MRI_float ) ; dar = MRI_FLOAT_PTR(dim) ;
     for( hh=kk=0 ; kk < nb ; kk++ ){
      for( jj=0 ; jj < nb ; jj++ ){
       for( ii=0 ; ii < nb ; ii++,hh++ ){
         xar[hh] = (float)(xx[hh]-cc[ii]) ;
         yar[hh] = (float)(yy[hh]-cc[jj]) ;
         zar[hh] = (float)(zz[hh]-cc[kk]) ;
         dar[hh] = sqrtf(xar[hh]*xar[hh]+yar[hh]*yar[hh]+zar[hh]*zar[hh]) ;
         if( har[hh] < mhv ) har[hh] = mhv ;
     }}}
     dset = EDIT_empty_copy(NULL) ;
     LOAD_IVEC3( iv_nxyz , nb,nb,nb ) ;
     EDIT_dset_items( dset ,
                        ADN_nxyz   , iv_nxyz ,
                        ADN_prefix , fname   ,
                        ADN_nvals  , 5       ,
                        ADN_ntt    , 0       ,
                      ADN_none ) ;
     EDIT_substitute_brick(dset,0,MRI_float,xar) ; EDIT_BRICK_LABEL(dset,0,"xdis") ;
     EDIT_substitute_brick(dset,1,MRI_float,yar) ; EDIT_BRICK_LABEL(dset,1,"ydis") ;
     EDIT_substitute_brick(dset,2,MRI_float,zar) ; EDIT_BRICK_LABEL(dset,2,"zdis") ;
     EDIT_substitute_brick(dset,3,MRI_float,har) ; EDIT_BRICK_LABEL(dset,3,"hex") ;
     EDIT_substitute_brick(dset,4,MRI_float,dar) ; EDIT_BRICK_LABEL(dset,4,"disp") ;
     DSET_write(dset) ; WROTE_DSET(dset) ; DSET_delete(dset) ;
   }

   return mhv ;
}

/*----------------------------------------------------------------------------*/
/* npar is 3*3*3*3 = 81 */

double HQwarp_minhexvol( int npar , double *par )
{
   double hv , mhv , ddd ;
   double *xpar=par , *ypar=par+27 , *zpar=par+54 ;
   int ii,jj,kk,hh , nbq ;
   double_triple x0,x1,x2,x3,x4,x5,x6,x7 ;
   MRI_IMAGE *him ; float *har=NULL ;

   if( npar < 81 ) return 0.0 ;         /* something bad */

   if( nb < 9 ) HQwarp_setup_warp_basis(51) ;

   ddd = del*del*del ;

   for( hh=kk=0 ; kk < nb ; kk++ ){
    for( jj=0 ; jj < nb ; jj++ ){
     for( ii=0 ; ii < nb ; ii++,hh++ ){
      xx[hh] = cc[ii]
                +b0[kk]*b0[jj]*b0[ii]*xpar[ 0] +b1[kk]*b0[jj]*b0[ii]*xpar[ 1]
                +b2[kk]*b0[jj]*b0[ii]*xpar[ 2] +b0[kk]*b1[jj]*b0[ii]*xpar[ 3]
                +b1[kk]*b1[jj]*b0[ii]*xpar[ 4] +b2[kk]*b1[jj]*b0[ii]*xpar[ 5]
                +b0[kk]*b2[jj]*b0[ii]*xpar[ 6] +b1[kk]*b2[jj]*b0[ii]*xpar[ 7]
                +b2[kk]*b2[jj]*b0[ii]*xpar[ 8] +b0[kk]*b0[jj]*b1[ii]*xpar[ 9]
                +b1[kk]*b0[jj]*b1[ii]*xpar[10] +b2[kk]*b0[jj]*b1[ii]*xpar[11]
                +b0[kk]*b1[jj]*b1[ii]*xpar[12] +b1[kk]*b1[jj]*b1[ii]*xpar[13]
                +b2[kk]*b1[jj]*b1[ii]*xpar[14] +b0[kk]*b2[jj]*b1[ii]*xpar[15]
                +b1[kk]*b2[jj]*b1[ii]*xpar[16] +b2[kk]*b2[jj]*b1[ii]*xpar[17]
                +b0[kk]*b0[jj]*b2[ii]*xpar[18] +b1[kk]*b0[jj]*b2[ii]*xpar[19]
                +b2[kk]*b0[jj]*b2[ii]*xpar[20] +b0[kk]*b1[jj]*b2[ii]*xpar[21]
                +b1[kk]*b1[jj]*b2[ii]*xpar[22] +b2[kk]*b1[jj]*b2[ii]*xpar[23]
                +b0[kk]*b2[jj]*b2[ii]*xpar[24] +b1[kk]*b2[jj]*b2[ii]*xpar[25]
                +b2[kk]*b2[jj]*b2[ii]*xpar[26] ;
      yy[hh] = cc[jj]
                +b0[kk]*b0[jj]*b0[ii]*ypar[ 0] +b1[kk]*b0[jj]*b0[ii]*ypar[ 1]
                +b2[kk]*b0[jj]*b0[ii]*ypar[ 2] +b0[kk]*b1[jj]*b0[ii]*ypar[ 3]
                +b1[kk]*b1[jj]*b0[ii]*ypar[ 4] +b2[kk]*b1[jj]*b0[ii]*ypar[ 5]
                +b0[kk]*b2[jj]*b0[ii]*ypar[ 6] +b1[kk]*b2[jj]*b0[ii]*ypar[ 7]
                +b2[kk]*b2[jj]*b0[ii]*ypar[ 8] +b0[kk]*b0[jj]*b1[ii]*ypar[ 9]
                +b1[kk]*b0[jj]*b1[ii]*ypar[10] +b2[kk]*b0[jj]*b1[ii]*ypar[11]
                +b0[kk]*b1[jj]*b1[ii]*ypar[12] +b1[kk]*b1[jj]*b1[ii]*ypar[13]
                +b2[kk]*b1[jj]*b1[ii]*ypar[14] +b0[kk]*b2[jj]*b1[ii]*ypar[15]
                +b1[kk]*b2[jj]*b1[ii]*ypar[16] +b2[kk]*b2[jj]*b1[ii]*ypar[17]
                +b0[kk]*b0[jj]*b2[ii]*ypar[18] +b1[kk]*b0[jj]*b2[ii]*ypar[19]
                +b2[kk]*b0[jj]*b2[ii]*ypar[20] +b0[kk]*b1[jj]*b2[ii]*ypar[21]
                +b1[kk]*b1[jj]*b2[ii]*ypar[22] +b2[kk]*b1[jj]*b2[ii]*ypar[23]
                +b0[kk]*b2[jj]*b2[ii]*ypar[24] +b1[kk]*b2[jj]*b2[ii]*ypar[25]
                +b2[kk]*b2[jj]*b2[ii]*ypar[26] ;
      zz[hh] = cc[kk]
                +b0[kk]*b0[jj]*b0[ii]*zpar[ 0] +b1[kk]*b0[jj]*b0[ii]*zpar[ 1]
                +b2[kk]*b0[jj]*b0[ii]*zpar[ 2] +b0[kk]*b1[jj]*b0[ii]*zpar[ 3]
                +b1[kk]*b1[jj]*b0[ii]*zpar[ 4] +b2[kk]*b1[jj]*b0[ii]*zpar[ 5]
                +b0[kk]*b2[jj]*b0[ii]*zpar[ 6] +b1[kk]*b2[jj]*b0[ii]*zpar[ 7]
                +b2[kk]*b2[jj]*b0[ii]*zpar[ 8] +b0[kk]*b0[jj]*b1[ii]*zpar[ 9]
                +b1[kk]*b0[jj]*b1[ii]*zpar[10] +b2[kk]*b0[jj]*b1[ii]*zpar[11]
                +b0[kk]*b1[jj]*b1[ii]*zpar[12] +b1[kk]*b1[jj]*b1[ii]*zpar[13]
                +b2[kk]*b1[jj]*b1[ii]*zpar[14] +b0[kk]*b2[jj]*b1[ii]*zpar[15]
                +b1[kk]*b2[jj]*b1[ii]*zpar[16] +b2[kk]*b2[jj]*b1[ii]*zpar[17]
                +b0[kk]*b0[jj]*b2[ii]*zpar[18] +b1[kk]*b0[jj]*b2[ii]*zpar[19]
                +b2[kk]*b0[jj]*b2[ii]*zpar[20] +b0[kk]*b1[jj]*b2[ii]*zpar[21]
                +b1[kk]*b1[jj]*b2[ii]*zpar[22] +b2[kk]*b1[jj]*b2[ii]*zpar[23]
                +b0[kk]*b2[jj]*b2[ii]*zpar[24] +b1[kk]*b2[jj]*b2[ii]*zpar[25]
                +b2[kk]*b2[jj]*b2[ii]*zpar[26] ;
   }}}

#undef  IJK
#undef  TOT
#define IJK(p,q,r) ( (p) + (q)*nb + (r)*nbq )
#define TOT(tt,p)  ( tt.a = xx[p] , tt.b = yy[p] , tt.c = zz[p] )

   mhv = 666.666 ; nbq = nb*nb ;

   if( fname != NULL ){
     him = mri_new_vol( nb,nb,nb , MRI_float ) ; har = MRI_FLOAT_PTR(him) ;
   }

   for( kk=0 ; kk < nb-1 ; kk++ ){
    for( jj=0 ; jj < nb-1 ; jj++ ){
      for( ii=0 ; ii < nb-1 ; ii++ ){
        hh = IJK(ii,jj,kk) ;
        TOT(x0,hh) ; TOT(x1,hh+1) ; TOT(x2,hh+nb) ; TOT(x3,hh+nb+1) ;
        hh += nbq ;
        TOT(x4,hh) ; TOT(x5,hh+1) ; TOT(x6,hh+nb) ; TOT(x7,hh+nb+1) ;
        hv = hexahedron_volume(x0,x1,x2,x3,x4,x5,x6,x7) / ddd ;
        if( hv < mhv ) mhv = hv ;
        if( har != NULL ) har[ii+jj*nb+kk*nbq] = hv ;
   }}}

   if( fname != NULL ){
     MRI_IMAGE *xim,*yim,*zim,*dim ; float *xar,*yar,*zar,*dar ;
     THD_3dim_dataset *dset ; THD_ivec3 iv_nxyz ;
     xim = mri_new_vol( nb,nb,nb , MRI_float ) ; xar = MRI_FLOAT_PTR(xim) ;
     yim = mri_new_vol( nb,nb,nb , MRI_float ) ; yar = MRI_FLOAT_PTR(yim) ;
     zim = mri_new_vol( nb,nb,nb , MRI_float ) ; zar = MRI_FLOAT_PTR(zim) ;
     dim = mri_new_vol( nb,nb,nb , MRI_float ) ; dar = MRI_FLOAT_PTR(dim) ;
     for( hh=kk=0 ; kk < nb ; kk++ ){
      for( jj=0 ; jj < nb ; jj++ ){
       for( ii=0 ; ii < nb ; ii++,hh++ ){
         xar[hh] = (float)(xx[hh]-cc[ii]) ;
         yar[hh] = (float)(yy[hh]-cc[jj]) ;
         zar[hh] = (float)(zz[hh]-cc[kk]) ;
         dar[hh] = sqrtf(xar[hh]*xar[hh]+yar[hh]*yar[hh]+zar[hh]*zar[hh]) ;
         if( har[hh] < mhv ) har[hh] = mhv ;
     }}}
     dset = EDIT_empty_copy(NULL) ;
     LOAD_IVEC3( iv_nxyz , nb,nb,nb ) ;
     EDIT_dset_items( dset ,
                        ADN_nxyz   , iv_nxyz ,
                        ADN_prefix , fname   ,
                        ADN_nvals  , 5       ,
                        ADN_ntt    , 0       ,
                      ADN_none ) ;
     EDIT_substitute_brick( dset , 0 , MRI_float , xar ) ;
     EDIT_substitute_brick( dset , 1 , MRI_float , yar ) ;
     EDIT_substitute_brick( dset , 2 , MRI_float , zar ) ;
     EDIT_substitute_brick( dset , 3 , MRI_float , har ) ;
     EDIT_substitute_brick( dset , 4 , MRI_float , dar ) ;
     DSET_write(dset) ; WROTE_DSET(dset) ; DSET_delete(dset) ;
   }

   return mhv ;
}

/*----------------------------------------------------------------------------*/
static int nb5 = 3 ;  /* should be 2 or 3 or 4 or 5 */
/*----------------------------------------------------------------------------*/
/* npar is 3*nb5*nb5*nb5 = 24 or 81 or 192 or 375 */

double HCwarp_minhexvol5( int npar , double *par )
{
   double hv , mhv , ddd ;
   double *xpar , *ypar , *zpar ;
   int ii,jj,kk,hh , nbq , pp,qq,rr,ss ;
   double_triple x0,x1,x2,x3,x4,x5,x6,x7 ;
   MRI_IMAGE *him ; float *har=NULL ;
   int mpar = nb5*nb5*nb5 ;

   if( npar < 3*mpar ) return 0.0 ;         /* something bad */

   if( nb < 9 ) HCwarp_setup_warp_basis5(51) ;

   ddd  = del*del*del ;
   xpar = par ;
   ypar = par + mpar ;
   zpar = par + 2*mpar ;

   for( hh=kk=0 ; kk < nb ; kk++ ){
    for( jj=0 ; jj < nb ; jj++ ){
     for( ii=0 ; ii < nb ; ii++,hh++ ){
      xx[hh] = cc[ii] ; yy[hh] = cc[jj] ; zz[hh] = cc[kk] ;
      for( ss=rr=0 ; rr < nb5 ; rr++ ){
       for( qq=0 ; qq < nb5 ; qq++ ){
        for( pp=0 ; pp < nb5 ; pp++,ss++ ){
          xx[hh] += bar[pp][kk] * bar[qq][jj] * bar[rr][ii] * xpar[ss] ;
          yy[hh] += bar[pp][kk] * bar[qq][jj] * bar[rr][ii] * ypar[ss] ;
          zz[hh] += bar[pp][kk] * bar[qq][jj] * bar[rr][ii] * zpar[ss] ;
      }}}
   }}}

#undef  IJK
#undef  TOT
#define IJK(p,q,r) ( (p) + (q)*nb + (r)*nbq )
#define TOT(tt,p)  ( tt.a = xx[p] , tt.b = yy[p] , tt.c = zz[p] )

   mhv = 666.666 ; nbq = nb*nb ;

   if( fname != NULL ){
     him = mri_new_vol( nb,nb,nb , MRI_float ) ; har = MRI_FLOAT_PTR(him) ;
   }

   for( kk=0 ; kk < nb-1 ; kk++ ){
    for( jj=0 ; jj < nb-1 ; jj++ ){
      for( ii=0 ; ii < nb-1 ; ii++ ){
        hh = IJK(ii,jj,kk) ;
        TOT(x0,hh) ; TOT(x1,hh+1) ; TOT(x2,hh+nb) ; TOT(x3,hh+nb+1) ;
        hh += nbq ;
        TOT(x4,hh) ; TOT(x5,hh+1) ; TOT(x6,hh+nb) ; TOT(x7,hh+nb+1) ;
        hv = hexahedron_volume(x0,x1,x2,x3,x4,x5,x6,x7) / ddd ;
        if( hv < mhv ) mhv = hv ;
        if( har != NULL ) har[ii+jj*nb+kk*nbq] = hv ;
   }}}

   if( fname != NULL ){
     MRI_IMAGE *xim,*yim,*zim,*dim ; float *xar,*yar,*zar,*dar ;
     THD_3dim_dataset *dset ; THD_ivec3 iv_nxyz ;
     xim = mri_new_vol( nb,nb,nb , MRI_float ) ; xar = MRI_FLOAT_PTR(xim) ;
     yim = mri_new_vol( nb,nb,nb , MRI_float ) ; yar = MRI_FLOAT_PTR(yim) ;
     zim = mri_new_vol( nb,nb,nb , MRI_float ) ; zar = MRI_FLOAT_PTR(zim) ;
     dim = mri_new_vol( nb,nb,nb , MRI_float ) ; dar = MRI_FLOAT_PTR(dim) ;
     for( hh=kk=0 ; kk < nb ; kk++ ){
      for( jj=0 ; jj < nb ; jj++ ){
       for( ii=0 ; ii < nb ; ii++,hh++ ){
         xar[hh] = (float)(xx[hh]-cc[ii]) ;
         yar[hh] = (float)(yy[hh]-cc[jj]) ;
         zar[hh] = (float)(zz[hh]-cc[kk]) ;
         dar[hh] = sqrtf(xar[hh]*xar[hh]+yar[hh]*yar[hh]+zar[hh]*zar[hh]) ;
         if( har[hh] < mhv ) har[hh] = mhv ;
     }}}
     dset = EDIT_empty_copy(NULL) ;
     LOAD_IVEC3( iv_nxyz , nb,nb,nb ) ;
     EDIT_dset_items( dset ,
                        ADN_nxyz   , iv_nxyz ,
                        ADN_prefix , fname   ,
                        ADN_nvals  , 5       ,
                        ADN_ntt    , 0       ,
                      ADN_none ) ;
     EDIT_substitute_brick( dset , 0 , MRI_float , xar ) ;
     EDIT_substitute_brick( dset , 1 , MRI_float , yar ) ;
     EDIT_substitute_brick( dset , 2 , MRI_float , zar ) ;
     EDIT_substitute_brick( dset , 3 , MRI_float , har ) ;
     EDIT_substitute_brick( dset , 4 , MRI_float , dar ) ;
     DSET_write(dset) ; WROTE_DSET(dset) ; DSET_delete(dset) ;
   }

   return mhv ;
}

/*----------------------------------------------------------------------------*/

#define MPAR 375

int main( int argc , char *argv[] )
{
   int ngrid=51 , ii , nfunc , npar, nord , nopt=1 ;
   double btop , cost , bmin[MPAR], bmax[MPAR], beta[MPAR] ;

   void (*setup_warp_basis)(int) ;
   double (*minhexvol)(int,double *) ;

   if( argc < 3 ){
     printf("Usage: warping_test order btop [ngrid] [fname]\n") ;
     printf("\n"
            "Program to test warping functions and find maximum volume distortions.\n"
            "For use by Emperor Zhark only!!\n"
            "  order = 3 or 5 [cubic or quintic] or -3,-4,-5 [expanded cubic]\n"
            "  btop  = max value for warp coefficient\n"
            "  ngrid = grid size (default=51)\n"
            "  fname = prefix for output dataset (default=no output)\n"
     ) ;
     exit(0) ;
   }

   nord = (int)strtod( argv[nopt++] , NULL ) ;
   if( nord == 3 ){
     npar = 24 ;
     setup_warp_basis = HCwarp_setup_warp_basis ;
     minhexvol        = HCwarp_minhexvol ;
   } else if( nord == 5 ){
     npar = 81 ;
     setup_warp_basis = HQwarp_setup_warp_basis ;
     minhexvol        = HQwarp_minhexvol ;
   } else if( nord == -2 ){
     npar = 24 ; nb5 = 2 ;
     setup_warp_basis = HCwarp_setup_warp_basis5 ;
     minhexvol        = HCwarp_minhexvol5 ;
   } else if( nord == -3 ){
     npar = 81 ; nb5 = 3 ;
     setup_warp_basis = HCwarp_setup_warp_basis5 ;
     minhexvol        = HCwarp_minhexvol5 ;
   } else if( nord == -4 ){
     npar = 192 ; nb5 = 4 ;
     setup_warp_basis = HCwarp_setup_warp_basis5 ;
     minhexvol        = HCwarp_minhexvol5 ;
   } else if( nord == -5 ){
     npar = 375 ; nb5 = 5 ;
     setup_warp_basis = HCwarp_setup_warp_basis5 ;
     minhexvol        = HCwarp_minhexvol5 ;
   } else {
     ERROR_exit("Illegal order value %d -- must be 3 or 5",nord) ;
   }

   btop = strtod( argv[nopt++] , NULL ) ;
   if( btop <= 0.0 ) ERROR_exit("btop must be positive") ;

   if( argc > 3 ){
     ngrid = (int)strtod( argv[nopt++] , NULL ) ;
     if( ngrid < 9 ) ERROR_exit("ngrid must be >= 9") ;
   }

   setup_warp_basis( ngrid ) ;

   for( ii=0 ; ii < npar ; ii++ ){
     bmin[ii] = -btop ; bmax[ii] = btop ; beta[ii] = 0.0 ;
   }

   cost = minhexvol( npar , beta ) ;
   INFO_message("minhexvol(0) = %g",cost) ;

   powell_set_verbose(2) ;
   if( AFNI_yesenv("POWELL_BALL") ) powell_newuoa_set_con_ball() ;

   nfunc = powell_newuoa_constrained( npar , beta , &cost , bmin,bmax ,
                                      999  , 33   , 7     ,
                                      0.16 , 0.001, 6666  , minhexvol  ) ;

   printf("minhexvol = %g\n",cost ) ;
#if 0
   for( ii=0 ; ii < npar ; ii++ ) printf(" %g",beta[ii]) ;
   printf("\n") ;
#endif
#if 0
   { double bmax=0.0 , brad=0.0, bb ;
     for( ii=0 ; ii < npar ; ii++ ){
       bb = fabs(beta[ii]) ; if( bb > bmax ) bmax = bb ;
       brad += bb*bb ;
     }
     brad = sqrt(bb) ; printf("bmax = %g   brad = %g\n",bmax,brad) ;
   }
#endif

   if( argc > 4 ){
     fname = argv[nopt++] ; (void)minhexvol( npar , beta ) ;
   }

   exit(0) ;
}
