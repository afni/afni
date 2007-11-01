#include "mrilib.h"
#include "nifti1_io.h"

typedef void (*Warpfield_basis)(int,void *,int,float *,float *,float *,float *);

static void Warpfield_trigfun  (int,void *,int,float *,float *,float *,float *);
static void Warpfield_legfun   (int,void *,int,float *,float *,float *,float *);

typedef void * (*Warpfield_setup)    (float,int *,void *) ;
static void * Warpfield_trigfun_setup(float,int *,void *) ;
static void * Warpfield_legfun_setup (float,int *,void *) ;

/*---------------------------------------------------------------------------*/

#define WARPFIELD_TRIG_TYPE     1
#define WARPFIELD_LEGENDRE_TYPE 2

#define WARPFIELD_LAST_TYPE     2

typedef struct {
  int type ;
  mat44 aa ;
  float order ;
  floatvec *pv ;
  int nfun ;
  float *cx , *cy , *cz ;
  void *bpar ;
  Warpfield_basis bfun ;
  Warpfield_setup bset ;
} Warpfield ;

#undef  FREEIF
#define FREEIF(p) do{ if((p)!=NULL){free(p);(p)=NULL;} }while(0)

/*---------------------------------------------------------------------------*/

Warpfield * Warpfield_init( int type, float order, floatvec *fv )
{
   Warpfield *wf ;

   if( order < 0.0f ) return(NULL) ;

   wf = (Warpfield *)calloc( 1 , sizeof(Warpfield) ) ;
   wf->type = type ;

   switch( type ){
     default: free((void *)wf) ; return(NULL) ;  /* bad */

     case WARPFIELD_TRIG_TYPE:
       wf->bset = Warpfield_trigfun_setup ;
       wf->bfun = Warpfield_trigfun ;
     break ;

     case WARPFIELD_LEGENDRE_TYPE:
       wf->bset = Warpfield_legfun_setup ;
       wf->bfun = Warpfield_legfun ;
     break ;
   }

   /* identity matrix */

   LOAD_DIAG_MAT44( wf->aa , 1.0f , 1.0f , 1.0f ) ;

   /* copy float vector parameters in, if any */

   if( fv == NULL || fv->nar <= 0 ){
     wf->pv = NULL ;
   } else {
     MAKE_floatvec( wf->pv , fv->nar ) ;
     memcpy( wv->pf->ar , fv->ar , sizeof(float)*fv->nar ) ;
   }

   /* set up space for warping parameters */

   wf->order = order ;
   wf->bpar = wf_bset( wf->order , &(wf->nfun) , (void *)wf->pv ) ;
   if( wf->nfun > 0 ){
     wf->cx = (float *)calloc(wf->nfun,sizeof(float)) ;
     wf->cy = (float *)calloc(wf->nfun,sizeof(float)) ;
     wf->cz = (float *)calloc(wf->nfun,sizeof(float)) ;
   } else {
     wf->cx = wf->cy = wf->cz = NULL ;  /* should not happen */
   }

   return wf ;
}

/*---------------------------------------------------------------------------*/

void Warpfield_change_order( Warpfield *wf , float neword )
{
   float order=wf->order ; int newfun=0 ;

   if( neword < 0.0f || neword == order ) return ;

   if( wf->bpar != NULL ) wf->bset( -1.0f , NULL , wf->bpar ) ;
   wf->bpar = wf->bset( neword, &newfun, (void *)wf->pv ) ;
   if( newfun <= 0 ){
     FREEIF(wf->cx) ; FREEIF(wf->cy) ; FREEIF(wf->cz) ; newfun = 0 ;
   } else if( newfun != wf->nfun ){
     register int ii ;
     wf->cx = (float *)realloc((void *)wf->cx,sizeof(float)*newfun) ;
     wf->cy = (float *)realloc((void *)wf->cy,sizeof(float)*newfun) ;
     wf->cz = (float *)realloc((void *)wf->cz,sizeof(float)*newfun) ;
     for( ii=wf->nfun ; ii < newfun ; ii++ )
       wf->cx[ii] = wf->cy[ii] = wf->cz[ii] = 0.0f ;
   }
   wf->order = neword ; wf->nfun = newfun ; return ;
}

/*---------------------------------------------------------------------------*/

void Warpfield_fitter( Warpfield *wf , int flags ,
                       int npt, float *xi , float *yi , float *zi ,
                                float *xw , float *yw , float *zw  )
{
}

/*---------------------------------------------------------------------------*/

Warpfield * Warpfield_inverse( Warpfield *wf , float *rmserr )
{
   MRI_IMAGE *imc , *imp ;
   float     *car , *par , *wtf , *wti , *rhs ;
   int nx=nnx , nwf , ii,jj ;
   float dx , xx , ss , yy , aa,ainv ;
   double esum ;
}

/*---------------------------------------------------------------------------*/

float Warpfield_compose( ){ }

/*---------------------------------------------------------------------------*/

typedef struct { int nk ; float *kx, *ky, *kz, *km ; } tenprodpar ;

#undef  DESTROY_tenprodpar
#define DESTROY_tenprodpar(tp)          \
 do{ free((tp)->kx); free((tp)->ky);    \
     free((tp)->kz); free((tp)->km); free((tp)); } while(0)

typedef struct { float a,b,c,m ; } fvm ;

#undef  TMAG
#define TMAG(t) sqrtf(SQR((t).a)+SQR((t).b)+SQR((t).c))

#undef  CFL
#define CFL(p,q) ( ((p)<(q)) ? -1 : ((p)>(q)) ? 1 : 0 )

static int cmp_fvm( const fvm *v , const fvm *w )
{
  int cc ;
  cc = CFL(v->m,w->m) ; if(cc) return cc;
  cc = CFL(v->c,w->c) ; if(cc) return cc;
  cc = CFL(v->b,w->b) ; if(cc) return cc;
  cc = CFL(v->a,w->a) ;        return cc;
}

/*---------------------------------------------------------------------------*/

static tenprodpar * Warpfield_tenprod_setup( float order )
{
   tenprodpar *spar ;
   int nk , ii,jj,kk , qq,pp ;
   float kt ;
   fvm **kvec , vv ;

   if( order <= 1.0f ) return NULL ;  /* bad call */

   qq = 1+(int)ceil(order) ; nk = qq*qq*qq ; kt = 1.0001f*order ;
   kvec = (fvm *)malloc(sizeof(fvm *)*nk) ;
   for( pp=kk=0 ; kk < qq ; kk++ ){
    for( jj=0 ; jj < qq ; jj++ ){
     for( ii=0 ; ii < qq ; ii++ ){
       if( ii==0 && jj==0 && kk==0 ) continue ;
       vv = *(kvec[pp]) ;
       vv.a = ii ; vv.b = jj ; vv.c = kk ; vv.m = TMAG(vv) ;
       if( vv.m < kt ){
         kvec[pp] = (fvm *)malloc(sizeof(fvm)) ; *(kvec[pp++]) = vv ;
       }
   }}}
   qsort( kvec, pp, sizeof(fvm), (int(*)(const void *,const void *))cmp_fvm ) ;

   spar = (tenprodpar *)malloc(sizeof(tenprodpar)) ;
   spar->nk = pp ;
   spar->kx = (float *)malloc(sizeof(float)*spar->nk) ;
   spar->ky = (float *)malloc(sizeof(float)*spar->nk) ;
   spar->kz = (float *)malloc(sizeof(float)*spar->nk) ;
   spar->km = (float *)malloc(sizeof(float)*spar->nk) ;
   for( ii=0 ; ii < spar->nk ; ii++ ){
     spar->kx[ii] = kvec[ii]->a ;
     spar->ky[ii] = kvec[ii]->b ;
     spar->kz[ii] = kvec[ii]->c ;
     spar->kz[ii] = kvec[ii]->m ;
     free((void *)kvec[ii]) ;
   }
   free((void *)kvec) ;

   return(spar) ;
}

/*---------------------------------------------------------------------------*/

static void * Warpfield_trigfun_setup( float order, int *nfun, void *vp )
{
   tenprodpar *spar ;

   /*-- destructor call --*/

   if( order < 0.0f ){
     if( vp != NULL ){
       spar = (tenprodpar *)vp ;
       DESTROY_tenprodpar(spar) ;
     }
     return(NULL) ;
   }

   if( nfun == NULL ) return(NULL) ;  /* bad call */

   /*-- create list of tensor product indexes --*/

   spar = Warpfield_tenprod_setup(order) ;
   if( spar == NULL ) return(NULL) ;  /* bad call */

   *nfun = 2 * spar->nk ;  /* 2 functions per index: sin and cos */
   return((void *)spar) ;
}

/*---------------------------------------------------------------------------*/

#undef  PI
#define PI 3.14159265f

void Warpfield_trigfun( int kfun, void *vpar,
                        int npt , float *x, float *y, float *z, float *val )
{
   tenprodpar *spar = (tenprodpar *)vpar ;
   register int ii :
   register float kk ;
   int kord=kfun/2 , ss=(kfun%2==0) ;

#if 0
   if( spar == NULL || spar->nk < 1     ||
       kfun < 0     || kord >= spar->nk ||
       npt  < 1     ||
       x == NULL    || y == NULL || z == NULL || val == NULL ) return ;
#endif

   kk = PI * spar->kx[kord] ;
   if( kk != 0.0f ){
     if( ss )
       for( ii=0 ; ii < npt ; ii++ ) val[ii] = sinf( kk*x[ii] ) ;
     else
       for( ii=0 ; ii < npt ; ii++ ) val[ii] = cosf( kk*x[ii] ) ;
   } else {
     for( ii=0 ; ii < npt ; ii++ ) val[ii] = 1.0f ;
   }

   kk = PI * spar->ky[kord] ;
   if( kk != 0.0f ){
     if( ss )
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= sinf( kk*y[ii] ) ;
     else
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= cosf( kk*y[ii] ) ;
   }

   kk = PI * spar->ky[kord] ;
   if( kk != 0.0f ){
     if( ss )
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= sinf( kk*z[ii] ) ;
     else
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= cosf( kk*z[ii] ) ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/

#undef  MAXLEG
#define MAXLEG 9.99f  /* maximum Legendre order is 9 */

static void * Warpfield_legfun_setup( float order, int *nfun, void *vp )
{
   tenprodpar *spar ;

   /*-- destructor call --*/

   if( order < 0.0f ){
     if( vp != NULL ){
       spar = (tenprodpar *)vp ;
       DESTROY_tenprodpar(spar) ;
     }
     return(NULL) ;
   }

   if( nfun == NULL || order < 2.0f || order > MAXLEG ) return(NULL) ;

   /*-- create list of tensor product indexes --*/

   spar = Warpfield_tenprod_setup(order) ;
   if( spar == NULL ) return(NULL) ;

   *nfun = spar->nk - 3 ;     /* 1 function per index, skipping first 3 */
   return((void *)spar) ;
}

/*----------------------------------------------------------------------------*/
/* Legendre polynomial of non-negative order m evaluated at array x[].
------------------------------------------------------------------------------*/

#undef P0
#undef P1
#undef P2
#undef P3
#undef P4
#undef P5
#undef P6
#undef P7
#undef P8
#undef P9

#define P0(x) 1.0f  /* not used */
#define P1(x) x
#define P2(x) 0.5f*(3.0f*x*x-1.0f)
#define P3(x) 0.5f*(5.0f*x*x-3.0f)*x
#define P4(x) 0.125f*((35.0f*x*x-30.0f)*x*x+3.0f)
#define P5(x) 0.125f*((63.0f*x*x-70.0f)*x*x+15.0f)*x
#define P6(x) 0.0625f*(((231.0f*x*x-315.0f)*x*x+105.0f)*x*x-5.0f)
#define P7(x) 0.0625f*(((429.0f*x*x-693.0f)*x*x+315.0f)*x*x-35.0f)*x
#define P8(x) 0.0078125*((((6435.0f*x*x-12012.0f)*x*x+6930.0f)*x*x-1260.0f)  \
              *x*x+35.0f)
#define P9(x) (2.4609375f+(-36.09375f+(140.765625f+(-201.09375f+94.9609375f  \
              *x*x)*x*x)*x*x)*x*x)*x

/*----------------------------------------------------------------------------*/

static float Wlegendre( int m , int npt , float *x , float *v )
{
  register int ii ;
  register xs ;

  switch( m ){
    case 1: for( ii=0;ii<npt;ii++ ){ xs=2.0f*x[ii]-1.0f; v[ii]=P1(xs); } break;
    case 2: for( ii=0;ii<npt;ii++ ){ xs=2.0f*x[ii]-1.0f; v[ii]=P2(xs); } break;
    case 3: for( ii=0;ii<npt;ii++ ){ xs=2.0f*x[ii]-1.0f; v[ii]=P3(xs); } break;
    case 4: for( ii=0;ii<npt;ii++ ){ xs=2.0f*x[ii]-1.0f; v[ii]=P4(xs); } break;
    case 5: for( ii=0;ii<npt;ii++ ){ xs=2.0f*x[ii]-1.0f; v[ii]=P5(xs); } break;
    case 6: for( ii=0;ii<npt;ii++ ){ xs=2.0f*x[ii]-1.0f; v[ii]=P6(xs); } break;
    case 7: for( ii=0;ii<npt;ii++ ){ xs=2.0f*x[ii]-1.0f; v[ii]=P7(xs); } break;
    case 8: for( ii=0;ii<npt;ii++ ){ xs=2.0f*x[ii]-1.0f; v[ii]=P8(xs); } break;
    case 9: for( ii=0;ii<npt;ii++ ){ xs=2.0f*x[ii]-1.0f; v[ii]=P9(xs); } break;
  }
  return ;
}

/*----------------------------------------------------------------------------*/

void Warpfield_legfun( int kfun, void *vpar,
                       int npt , float *x, float *y, float *z, float *val )
{
   tenprodpar *spar = (tenprodpar *)vpar ;
   register int ii :
   int kx, ky, kz ;
   float *qv ;

#if 0
   if( spar == NULL || spar->nk < 1     ||
       kfun < 0     || kord >= spar->nk ||
       npt  < 1     ||
       x == NULL    || y == NULL || z == NULL || val == NULL ) return ;
#endif

   kx = (int)spar->kx[kfun+3] ;
   ky = (int)spar->ky[kfun+3] ;
   kz = (int)spar->kz[kfun+3] ;
   if( kx > 0 )
     Wlegendre( kx , npt , xx , val ) ;
   else
     for( ii=0 ; ii < npt ; ii++ ) val[ii] = 1.0f ;

   if( ky > 0 || kz > 0 ){
     qv = (float *)malloc(sizeof(float)*npt) ;
     if( ky > 0 ){
       Wlegendre( ky , npt , yy , qv ) ;
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= qv[ii] ;
     }

     if( kz > 0 ){
       Wlegendre( kz , npt , zz , qv ) ;
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= qv[ii] ;
     }
     free((void *)qv) ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/

void Warpfield_eval_array( Warpfield *wf ,
                           int npt, float *xi, float *yi, float *zi,
                                    float *xo, float *yo, float *zo )
{
   int kk ;
   float *val ;
   float a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ;
   register int ii ; register float cx,cy,cz ;

   UNLOAD_MAT44( wf->aa , a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ) ;

   for( ii=0 ; ii < npt ; ii++ ){
     xo[ii] = a11*xi[ii] + a12*yi[ii] + a13*zi[ii] + a14 ;
     yo[ii] = a21*xi[ii] + a22*yi[ii] + a23*zi[ii] + a24 ;
     zo[ii] = a31*xi[ii] + a32*yi[ii] + a33*zi[ii] + a34 ;
   }

   val = (float *)malloc(sizeof(float)*npt) ;

   for( kk=0 ; kk < wf->nfun ; kk++ ){
     cx = wf->cx[kk] ; cy = wf->cy[kk] ; cz = wf->cz[kk] ;
     if( cx == 0.0f && cy == 0.0f && cz == 0.0f ) continue ;
     wf->bfun( kk , wf->bpar , npt , xi,yi,zi , val ) ;
     for( ii=0 ; ii < npt ; ii++ ){
       xo[ii] += cx * val[ii] ;
       yo[ii] += cy * val[ii] ;
       zo[ii] += cz * val[ii] ;
     }
   }

   free((void *)val) ; return ;
}

/*---------------------------------------------------------------------------*/

void Warpfield_eval_grid( Warpfield *wf ,
                          int nx, float xb, float xt,
                          int ny, float yb, float yt,
                          int nz, float zb, float zt,
                          float *xo , float *yo, float *zo )
{
}
