#include "mrilib.h"

typedef struct {
   int type ;
   float siz , sinv ;
   THD_mat33 latmat , invlatmat ;

   int nlat ;
   float *xlat , *ylat , *zlat ;
   float *xdis , *ydis , *zdis ;

   int    num_pqroff 
   int_triple pqroff ;
   float (*bfun)(float,float,float) ;
} LAT_func ;

int default_infunc( float x, float y, float z, void *p ){ return 1; }

LAT_func * create_LAT_func( int type  , float siz ,
                            float xsiz, float ysiz, float zsiz,
                            int (*infunc)(float,float,float,void *) ,
                            void *infuncdat                          )
{
   LAT_func *lfun ;
   THD_fvec3 xyz,pqr ;
   THD_mat33 lmat,ilmat ;
   float a=siz  , xb,xt,yb,yt,zb,zt , xx,yy,zz ;
   int pp,qq,rr , pb,pt,qb,qt,rb,rt ;

ENTRY("create_LAT_func") ;

   if( siz <= 0.0f || xsiz <= 0.0f || ysiz <= 0.0f || zsiz <= 0.0f ) RETURN(NULL);

   if( infunc == NULL ) infunc = default_infunc ;

   lfun = (LAT_func *)calloc(1,sizeof(LAT_func)) ;
   lfun->type = type ;
   lfun->siz  = siz ;
   lfun->sinv = 1.0f / siz ;

   switch( type ){

     case GA_BLOK_CUBE:{
       LOAD_MAT( lmat , a,0,0 , 0,a,0 , 0,0,a ) ;
     } break ;

     case GA_BLOCK_RHDD:{
       LOAD_MAT( lmat , -a,a,a , a,-a,a , a,a,-a ) ;
     }

     default: free((void *)lfun) ; RETURN(NULL) ;

   }

   lfun->latmat    = lmat ;
   lfun->invlatmat = ilmat = MAT_INV( lfun->latmat ) ;

   /* convert bounding box from spatial coords to lattice indexes */

   pb = pt = qb = qt = rb = rt = 0 ;  /* initialize (p,q,r) bot, top values */

   xb = -xsiz-2.0f*siz ; xt = xsiz+2.0f*siz ;
   yb = -ysiz-2.0f*siz ; yt = ysiz+2.0f*siz ;
   zb = -zsiz-2.0f*siz ; zt = zsiz+2.0f*siz ;

   /* macro to update pb,pt etc. for point (a,b,c) */

#undef  PQRBT
#define PQRBT(a,b,c)                                                       \
 do{ LOAD_FVEC3( xyz , (a),(b),(c) ); pqr = MATVEC(ilmat,xyz);             \
     pp = (int)floorf(pqr.xyz[0]); pb = MIN(pb,pp); pp++; pt = MAT(pt,pp); \
     qq = (int)floorf(pqr.xyz[1]); qb = MIN(qb,qq); qq++; qt = MAX(qt,qq); \
     rr = (int)floorf(pqr.xyz[2]); rb = MIN(rb,rr); rr++; rt = MAX(rt,rr); \
 } while(0)

   PQRBT(xb,yb,zb); PQRBT(xt,yb,zb); PQRBT(xb,yt,zb); PQRBT(xt,yt,zb);
   PQRBT(xb,yb,zt); PQRBT(xt,yb,zt); PQRBT(xb,yt,zt); PQRBT(xt,yt,zt);

   xb = -xsiz ; xt = xsiz ;
   yb = -ysiz ; yt = ysiz ;
   zb = -zsiz ; zt = zsiz ;

   nlat = (rt-rb+1)*(qt-qb+1)*(pt-pb+1) ;
   lfun->xlat = (float *)malloc(sizeof(float)*nlat) ;
   lfun->ylat = (float *)malloc(sizeof(float)*nlat) ;
   lfun->zlat = (float *)malloc(sizeof(float)*nlat) ;
   nlat = 0 ;
   for( rr=rb ; rr <= rt ; rr++){
    for( qq=qb ; qq <= qt ; qq++){
     for( pp=pb ; pp <= pt ; pq++){
       LOAD_FVEC3(pqr,pp,qq,rr) ;
       xyz = MATVEC(lmat,pqr) ; UNLOAD_FVEC3(xyz,xx,yy,zz) ;
       if( xx >= xb && xx <= xt &&
           yy >= yb && yy <= yt &&
           zz >= zb && zz <= zt && infunc(xx,yy,zz,infuncdat) ){

         lfun->xlat[nlat] = xx; lfun->ylat[nlat] = yy; lfun->zlat[nlat] = zz;
         nlat++ ;
       }
   }}}
   lfun->xlat = (float *)realloc( (void *)lfun->xlat , sizeof(float)*nlat) ;
   lfun->ylat = (float *)realloc( (void *)lfun->ylat , sizeof(float)*nlat) ;
   lfun->zlat = (float *)realloc( (void *)lfun->zlat , sizeof(float)*nlat) ;

   lfun->xdis = (float *)calloc(sizeof(float),nlat) ;
   lfun->ydis = (float *)calloc(sizeof(float),nlat) ;
   lfun->zdis = (float *)calloc(sizeof(float),nlat) ;

   RETURN(lfun) ;
}
