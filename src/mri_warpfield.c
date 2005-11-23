#include "mrilib.h"
#include "nifti1_io.h"

typedef void (*Warpfield_basis)(int,float *,int,float *,float *,float *,float *) ;

/*---------------------------------------------------------------------------*/

typedef struct {
  mat44 aa ;
  float param[9] ;
  int   nwx , nwy , nwz ;
  float *wx , *wy , *wz ;
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

#undef  TWOPI
#define TWOPI 6.283185307f

#undef  PACK3
#define PACK3(a,b,c) ( ((a)&0xff) | (((b)&0xff)<<8) | (((c)0xff)<<16) )

#undef  UNPACK3
#define UNPACK3(v,a,b,c) ( (a)=(v)&0xff, (b)=((v)>>8)&0xff, (c)=((v)>>16)&0xff )

void Warpfield_sinf( int nord, float *param ,
                     int npt, float *x, float *y, float *z, float *val )
{
   int nx,ny,nz , ii=0 ;
   float bot,top,scl ;

   if( nord < 1 || param == NULL || npt < 1 || val == NULL ) return ;

   UNPACK3(nord,nx,ny,nz) ;
   if( nx == 0 && ny == 0 && nz == 0 ) return ;

   memset( val , 0 , sizeof(float)*npt ) ;

   if( nx != 0 && x != NULL ){
     bot = param[0] ; top = param[1] ; scl = TWOPI/(top-bot) ;
     for( ii=0 ; ii < npt ; ii++ ) val[ii] = (x[ii]-bot)*scl ;
   }
   if( ny != 0 && y != NULL ){
     bot = param[2] ; top = param[3] ; scl = TWOPI/(top-bot) ;
     for( ii=0 ; ii < npt ; ii++ ) val[ii] +=(y[ii]-bot)*scl ;
   }
   if( nz != 0 && z != NULL ){
     bot = param[4] ; top = param[5] ; scl = TWOPI/(top-bot) ;
     for( ii=0 ; ii < npt ; ii++ ) val[ii] +=(z[ii]-bot)*scl ;
   }
   if( ii == 0 ) return ;

   for( ii=0 ; ii < npt ; ii++ ) val[ii] = sinf(val[ii]) ;
   return ;
}
