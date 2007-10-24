#include "mrilib.h"
#include "nifti1_io.h"

typedef void (*Warpfield_basis)(int,void *,int,float *,float *,float *,float *) ;

/*---------------------------------------------------------------------------*/

typedef struct {
  float xbot,xtop , ybot,ytop , zbot,ztop ;
  mat44 aa ;
  int nord ;
  float *wx , *wy , *wz ;
  void *bpar ;
  Warpfield_basis bfun ;
} Warpfield ;

#undef  INIT_Warpfield
#define INIT_Warpfield(nam)                                             \
 do{ (nam) = (WarpField *)calloc(1,sizeof(WarpField)) ;                 \
     (nam)->aa.m[0][0] = (nam)->aa.m[1][1] = (nam)->aa.m[2][2] = 1.0f ; \
     (nam)->wx = (nam)->wy = (nam)->wz = (float *)NULL ;                \
     (nam)->bfun = (Warpfield_basis)NULL ;                              \
 } while(0)

#undef  FREEIF
#define FREEIF(p) do{ if((p)!=NULL){free(p);(p)=NULL;} }while(0)

/*---------------------------------------------------------------------------*/

float Warpfield_approx_inverse( int nnx , int nny , int nnz , Warpfield *wf ,
                                int nwxi, int nwyi, int nwzi, Warpfield *wi  )
{
   MRI_IMAGE *imc , *imp ;
   float     *car , *par , *wtf , *wti , *rhs ;
   int nx=nnx , nwf , ii,jj ;
   float dx , xx , ss , yy , aa,ainv ;
   double esum ;
}

/*---------------------------------------------------------------------------*/

typedef struct {
  int nk ;
  float *kx , *ky , *kz ;
} Warpfield_sin_param ;

typedef struct { float a,b,c ; } float_triple ;
#undef  TMAG
#define TMAG(t) ( SQR((t).a)+SQR((t).b)+SQR((t).c) )

#undef  PI
#define PI 3.14159265f

/*---------------------------------------------------------------------------*/

void * Warpfield_sin_setup( int nord )
{
   Warpfield_sin_param *spar ;
   int nk , ii,jj,kk , qq,qs,pp ;
   float *kmag ;
   float_triple **kvec , vv ;

   if( nord < 3 ) return (NULL) ;

   spar = (Warpfield_sin_param *)malloc(sizeof(Warpfield_sin_param)) ;
   qq = (int)ceil( pow(2.0*nord,0.3333333333333333) ) ; qs = qq*qq ;
   nk = spar->nk = qq*qs-1 ;
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
   spar->kx = (float *)malloc(sizeof(float)*nk) ;
   spar->ky = (float *)malloc(sizeof(float)*nk) ;
   spar->kz = (float *)malloc(sizeof(float)*nk) ;
   for( ii=0 ; ii < nk ; ii++ ){
     spar->kx[ii] = kvec[ii]->a ;
     spar->ky[ii] = kvec[ii]->b ;
     spar->kz[ii] = kvec[ii]->c ; free((void *)kvec[ii]) ;
   }
   free((void *)kvec) ;
   return ((void *)spar) ;
}

/*---------------------------------------------------------------------------*/

void Warpfield_sin( int kord, void *vpar,
                    int npt , float *x, float *y, float *z, float *val )
{
   Warpfield_sin_param *spar = (Warpfield_sin_param *)vpar ;
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

   if( kk != 0.0f )
     for( ii=0 ; ii < npt ; ii++ ) val[ii] *= sinf( kk*y[ii] ) ;

   if( kk != 0.0f )
     for( ii=0 ; ii < npt ; ii++ ) val[ii] *= sinf( kk*z[ii] ) ;

   return ;
}
