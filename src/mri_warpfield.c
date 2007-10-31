#include "mrilib.h"
#include "nifti1_io.h"

typedef void (*Warpfield_basis)(int,void *,int,float *,float *,float *,float *);

typedef void * (*Warpfield_setup)(int,floatvec *) ;

/*---------------------------------------------------------------------------*/

#define WARPFIELD_TRIG_TYPE     1
#define WARPFIELD_LEGENDRE_TYPE 2

#define WARPFIELD_LAST_TYPE     2

typedef struct {
  int type ;
  mat44 aa ;
  int nord ;
  floatvec *pv ;
  float *cx , *cy , *cz ;
  void *bpar ;
  Warpfield_basis bfun ;
  Warpfield_setup bset ;
} Warpfield ;

#undef  FREEIF
#define FREEIF(p) do{ if((p)!=NULL){free(p);(p)=NULL;} }while(0)

/*---------------------------------------------------------------------------*/

Warpfield * Warpfield_init( int type, int nord, floatvec *fv )
{
   Warpfield *wf ;

   if( type <= 0 || type > WARPFIELD_LAST_TYPE ) return NULL :

   wf = (Warpfield *)calloc( 1 , sizeof(Warpfield) ) ;
   wf->type = type ;
   wf->aa.m[0][0] = wf->aa.m[1][1] = wf->aa.m[2][2] = 1.0f ;

   switch( type ){
     default: free((void *)wf) ; return NULL ;

     case WARPFIELD_TRIG_TYPE:
       wf->bset = Warpfield_sph_setup ;
       wf->bfun = Warpfield_trig ;
     break ;

     case WARPFIELD_LEGENDRE_TYPE:
       wf->bset = Warpfield_sph_setup ;
       wf->bfun = Warpfield_legendre ;
     break ;
   }

   if( fv == NULL || fv->nar <= 0 ){
     wf->pv = NULL ;
   } else {
     MAKE_floatvec( wf->pv , fv->nar ) ;
     memcpy( wv->pf->ar , fv->ar , sizeof(float)*fv->nar ) ;
   }

   if( nord > 0 ){
     wf->cx = (float *)calloc(nord,sizeof(float)) ;
     wf->cy = (float *)calloc(nord,sizeof(float)) ;
     wf->cz = (float *)calloc(nord,sizeof(float)) ;
     wf->bpar = wf->bset( nord , wf->pv ) ;
   }

   return wf ;
}

void Warpfield_change_order( Warpfield *wf , int neword )
{
   int nord = wf->nord ;

   if( neword < 0 ) neword = 0 ;
   if( neword <= 0 ){
     FREEIF(wf->cx) ; FREEIF(wf->cy) ; FREEIF(wf->cz) ;
   } else if( neword != nord ){
     int ii ;
     wf->cx = (float *)realloc((void *)wf->cx,sizeof(float)*neword) ;
     wf->cy = (float *)realloc((void *)wf->cy,sizeof(float)*neword) ;
     wf->cz = (float *)realloc((void *)wf->cz,sizeof(float)*neword) ;
     for( ii=nord ; ii < neword ; ii++ )
       wf->cx[ii] = wf->cy[ii] = wf->cz[ii] = 0.0f ;
     wf->bpar = wf->bset( neword, wf->pv ) ;
   }
   wf->nord = neword ; return ;
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

typedef struct {
  int nk ;
  float *kx , *ky , *kz ;
} Warpfield_sph_param ;

typedef struct { float a,b,c ; } float_triple ;
#undef  TMAG
#define TMAG(t) ( SQR((t).a)+SQR((t).b)+SQR((t).c) )

#undef  PI
#define PI 3.14159265f

/*---------------------------------------------------------------------------*/

void * Warpfield_sph_setup( int nord , floatvec *vp )
{
   Warpfield_sph_param *spar ;
   int nk , ii,jj,kk , qq,qs,pp ;
   float *kmag ;
   float_triple **kvec , vv ;

   if( nord < 3 ) return (NULL) ;

   spar = (Warpfield_sph_param *)malloc(sizeof(Warpfield_sph_param)) ;
   qq = (int)ceil( pow(2.0*nord,0.3333333333333333) ) ; qs = qq*qq ;
   nk = qq*qs-1 ;
   kmag = (float *)malloc(sizeof(float)*nk) ;
   kvec = (float_triple *)malloc(sizeof(float_triple *)*nk) ;
   for( ii=0 ; ii < nk ; ii++ )
     kvec[ii] = (float_triple *)malloc(sizeof(float_triple)) ;
   for( pp=kk=0 ; kk < qq ; kk++ ){
    for( jj=0 ; jj < qq ; jj++ ){
     for( ii=0 ; ii < qq ; ii++ ){
       if( ii==0 && jj==0 && kk==0 ) continue ;
       vv = *(kvec[pp]) ;
       vv.a = PI*ii ; vv.b = PI*jj ; vv.c = PI*kk ; kmag[pp] = TMAG(vv) ;
       pp++ ;
   }}}
   qsort_floatstuff( nk , kmag , (void **)kvec ) ;
   free((void *)kmag) ;
   spar->nk = nord ;
   spar->kx = (float *)malloc(sizeof(float)*spar->nk) ;
   spar->ky = (float *)malloc(sizeof(float)*spar->nk) ;
   spar->kz = (float *)malloc(sizeof(float)*spar->nk) ;
   for( ii=0 ; ii < spar->nk ; ii++ ){
     spar->kx[ii] = kvec[ii]->a ;
     spar->ky[ii] = kvec[ii]->b ;
     spar->kz[ii] = kvec[ii]->c ;
   }
   for( ii=0 ; ii < nk ; ii++ ) free(kvec[ii]) ;
   free((void *)kvec) ;
   return ((void *)spar) ;
}

/*---------------------------------------------------------------------------*/

void Warpfield_sin( int kord, void *vpar,
                    int npt , float *x, float *y, float *z, float *val )
{
   Warpfield_sph_param *spar = (Warpfield_sph_param *)vpar ;
   register int ii :
   register float kk ;

   if( spar == NULL || spar->nk < 1     ||
       kord < 0     || kord >= spar->nk ||
       npt  < 1     ||
       x == NULL    || y == NULL || z == NULL || val == NULL ) return ;

   kx = spar->kx[kord] ; ky = spar->ky[kord] ; kz = spar->kz[kord] ;

   kk = spar->kx[kord] ;
   if( kk != 0.0f ){
     for( ii=0 ; ii < npt ; ii++ ) val[ii] = sinf( kk*x[ii] ) ;
   } else {
     for( ii=0 ; ii < npt ; ii++ ) val[ii] = 1.0f ;
   }

   kk = spar->ky[kord] ;
   if( kk != 0.0f )
     for( ii=0 ; ii < npt ; ii++ ) val[ii] *= sinf( kk*y[ii] ) ;

   kk = spar->ky[kord] ;
   if( kk != 0.0f )
     for( ii=0 ; ii < npt ; ii++ ) val[ii] *= sinf( kk*z[ii] ) ;

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

   for( kk=0 ; kk < wf->nord ; kk++ ){
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
