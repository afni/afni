#include "mrilib.h"
#include "mri_warpfield.h"

void Warpfield_trigfun  (int,void *,int,float *,float *,float *,float *);
void Warpfield_legfun   (int,void *,int,float *,float *,float *,float *);
void Warpfield_gegenfun (int,void *,int,float *,float *,float *,float *);

void * Warpfield_trigfun_setup(float,int *,void *) ;
void * Warpfield_polyfun_setup(float,int *,void *) ;

char * Warpfield_trigfun_label (int,void *) ;
char * Warpfield_legfun_label  (int,void *) ;
char * Warpfield_gegenfun_label(int,void *) ;

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
       wf->blab = Warpfield_trigfun_label ;
     break ;

     case WARPFIELD_LEGEN_TYPE:
       wf->bset = Warpfield_polyfun_setup ;
       wf->bfun = Warpfield_legfun ;
       /* wf->blab = Warpfield_legfun_label ; */
     break ;

     case WARPFIELD_GEGEN_TYPE:
       wf->bset = Warpfield_polyfun_setup ;
       wf->bfun = Warpfield_gegenfun ;
       /* wf->blab = Warpfield_gegenfun_label ; */
     break ;
   }

   /* identity matrix */

   LOAD_DIAG_MAT44( wf->aa , 1.0f , 1.0f , 1.0f ) ;

   /* copy float vector parameters in, if any */

   if( fv == NULL || fv->nar <= 0 ){
     wf->pv = NULL ;
   } else {
     MAKE_floatvec( wf->pv , fv->nar ) ;
     memcpy( wf->pv->ar , fv->ar , sizeof(float)*fv->nar ) ;
   }

   /* set up space for warping parameters */

   wf->order = order ;
   wf->bpar = wf->bset( wf->order , &(wf->nfun) , (void *)wf->pv ) ;
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
}

/*---------------------------------------------------------------------------*/

float Warpfield_compose(void)
{
}

/*---------------------------------------------------------------------------*/
/* Stuff for tensor product basis functions */

typedef struct { int nk; int *kx, *ky, *kz; float *km; } tenprodpar ;

#undef  DESTROY_tenprodpar
#define DESTROY_tenprodpar(tp)          \
 do{ free((tp)->kx); free((tp)->ky);    \
     free((tp)->kz); free((tp)->km); free((tp)); } while(0)

typedef struct { int a,b,c ; float m ; } fvm ;

#undef  TMAG
#define TMAG(t) sqrtf((float)(SQR((t).a)+SQR((t).b)+SQR((t).c)))

#undef  CFV
#define CFV(p,q) ( ((p)<(q)) ? -1 : ((p)>(q)) ? 1 : 0 )

static int cmp_fvm( const fvm *v , const fvm *w )  /* for qsort() */
{
  int cc ; float dd ;
  dd = v->m - w->m ;
       if( dd < -0.001f ) return(-1) ;
  else if( dd >  0.001f ) return(1) ;
  cc = CFV(v->c,w->c) ; if(cc) return(cc) ;
  cc = CFV(v->b,w->b) ; if(cc) return(cc) ;
  cc = CFV(v->a,w->a) ;        return(cc) ;
}

/*---------------------------------------------------------------------------*/

static tenprodpar * Warpfield_tenprod_setup( float order )
{
   tenprodpar *spar ;
   int nk , ii,jj,kk , qq,pp ;
   float kt ;
   fvm *kvec , vv ;

   if( order <= 1.0f ) return(NULL) ;  /* bad call */

   qq = 1+(int)ceil(order) ; nk = qq*qq*qq ; kt = 1.0001f*order ;
   kvec = (fvm *)malloc(sizeof(fvm)*nk) ;
   for( pp=kk=0 ; kk < qq ; kk++ ){
    for( jj=0 ; jj < qq ; jj++ ){
     for( ii=0 ; ii < qq ; ii++ ){
       if( ii==0 && jj==0 && kk==0 ) continue ;
       vv.a = ii ; vv.b = jj ; vv.c = kk ; vv.m = TMAG(vv) ;
       if( vv.m < kt ) kvec[pp++] = vv ;
   }}}
   if( pp <= 1 ){ free((void *)kvec); return(NULL); }
   qsort( kvec , (size_t)pp , sizeof(fvm) ,
          (int(*)(const void *,const void *))cmp_fvm ) ;

   spar = (tenprodpar *)malloc(sizeof(tenprodpar)) ;
   spar->nk = pp ;
   spar->kx = (int *)  malloc(sizeof(int)  *spar->nk) ;
   spar->ky = (int *)  malloc(sizeof(int)  *spar->nk) ;
   spar->kz = (int *)  malloc(sizeof(int)  *spar->nk) ;
   spar->km = (float *)malloc(sizeof(float)*spar->nk) ;
   for( ii=0 ; ii < spar->nk ; ii++ ){
     spar->kx[ii] = kvec[ii].a ;
     spar->ky[ii] = kvec[ii].b ;
     spar->kz[ii] = kvec[ii].c ;
     spar->km[ii] = kvec[ii].m ;
   }
   free((void *)kvec) ;

   return(spar) ;
}

/*---------------------------------------------------------------------------*/

void * Warpfield_trigfun_setup( float order, int *nfun, void *vp )
{
   tenprodpar *spar ;

   /*-- destructor call --*/

   if( order < 0.0f ){
     if( vp != NULL ){ spar = (tenprodpar *)vp; DESTROY_tenprodpar(spar); }
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

char * Warpfield_trigfun_label( int kfun , void *vpar )
{
   static char *name ;
   Warpfield_trigfun( kfun , vpar , 0 , NULL,NULL,NULL , (float *)(&name) ) ;
   return name ;
}

/*---------------------------------------------------------------------------*/

#undef  PI
#define PI    3.14159265f
#undef  TWOPI
#define TWOPI 6.28318531f

void Warpfield_trigfun( int kfun, void *vpar,
                        int npt , float *x, float *y, float *z, float *val )
{
   tenprodpar *spar = (tenprodpar *)vpar ;
   register int ii ;
   register float kk, qq ;
   int kord=kfun/8 , sx,sy,sz ;

   ii = kfun%8 ; sx = ((ii&1)!=0) ; sy = ((ii&2)!=0) ; sz = ((ii&4)!=0) ;

   /* NULL input ==> make up a name string */

   if( x == NULL ){
     char **cpt=(char **)val , xpt[32] , ypt[32] , zpt[32] ;
     static char name[128] ;
     int kx=spar->kx[kord] , ky=spar->ky[kord] , kz=spar->kz[kord] ;
     if( kx == 0 ){
       if( sx ) strcpy(xpt,"x") ; else xpt[0] = '\0' ;
     } else {
       if( sx ) strcpy(xpt,"sin(") ; else strcpy(xpt,"cos(") ;
       if( kx > 1 ) sprintf(xpt+4,"%d*",kx) ;
       strcat(xpt,"PI*x)") ;
     }
     if( ky == 0 ){
       if( sy ) strcpy(ypt,"y") ; else ypt[0] = '\0' ;
     } else {
       if( sy ) strcpy(ypt,"sin(") ; else strcpy(ypt,"cos(") ;
       if( ky > 1 ) sprintf(ypt+4,"%d*",ky) ;
       strcat(ypt,"PI*y)") ;
     }
     if( kz == 0 ){
       if( sz ) strcpy(zpt,"z") ; else zpt[0] = '\0' ;
     } else {
       if( sz ) strcpy(zpt,"sin(") ; else strcpy(zpt,"cos(") ;
       if( kz > 1 ) sprintf(zpt+4,"%d*",kz) ;
       strcat(zpt,"PI*z)") ;
     }
     if( xpt[0] != '\0' ) strcpy(name,xpt); else name[0] = '\0';
     if( ypt[0] != '\0' ){
       if( name[0] != '\0' ) strcat(name,"*") ;
       strcat(name,ypt) ;
     }
     if( zpt[0] != '\0' ){
       if( name[0] != '\0' ) strcat(name,"*") ;
       strcat(name,zpt) ;
     }
     *cpt = name ; return ;
   }

   /** do some work **/

   kk = PI * spar->kx[kord] ;
   if( kk != 0.0f ){
     if( sx )
       for( ii=0 ; ii < npt ; ii++ ) val[ii] = sinf( kk*x[ii] ) ;
     else
       for( ii=0 ; ii < npt ; ii++ ) val[ii] = cosf( kk*x[ii] ) ;
   } else {
     if( sx )
       for( ii=0 ; ii < npt ; ii++ ) val[ii] = x[ii] ;
     else
       for( ii=0 ; ii < npt ; ii++ ) val[ii] = 1.0f ;
   }

   kk = PI * spar->ky[kord] ;
   if( kk != 0.0f ){
     if( sy )
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= sinf( kk*y[ii] ) ;
     else
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= cosf( kk*y[ii] ) ;
   } else {
     if( sy )
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= y[ii] ;
   }

   kk = PI * spar->kz[kord] ;
   if( kk != 0.0f ){
     if( sz )
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= sinf( kk*z[ii] ) ;
     else
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= cosf( kk*z[ii] ) ;
   } else {
     if( sz )
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= z[ii] ;
   }

   /* taper downwards for values far from center */

   for( ii=0 ; ii < npt ; ii++ ){
     qq = x[ii]*x[ii] + y[ii]*y[ii] + z[ii]*z[ii] ;
     val[ii] /= (1.0f+qq*qq) ;
   }

   return ;
}

/*---------------------------------------------------------------------------*/

#undef  MAXPOL
#define MAXPOL 9.99f  /* maximum Legendre order is 9 */

void * Warpfield_polyfun_setup( float order, int *nfun, void *vp )
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

   if( nfun == NULL || order < 2.0f || order > MAXPOL ) return(NULL) ;

   /*-- create list of tensor product indexes --*/

   spar = Warpfield_tenprod_setup(order) ;
   if( spar == NULL ) return(NULL) ;

   *nfun = spar->nk - 3 ;     /* 1 function per index, skipping first 3 */
   return((void *)spar) ;
}

/*----------------------------------------------------------------------------*/
/* 1D Legendre polynomial of non-negative order m evaluated at array x[].
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

#define P0 1.0f  /* not used */
#define P1 x
#define P2 0.5f*(3.0f*xq-1.0f)
#define P3 0.5f*(5.0f*xq-3.0f)*x
#define P4 0.125f*((35.0f*xq-30.0f)*xq+3.0f)
#define P5 0.125f*((63.0f*xq-70.0f)*xq+15.0f)*x
#define P6 0.0625f*(((231.0f*xq-315.0f)*xq+105.0f)*xq-5.0f)
#define P7 0.0625f*(((429.0f*xq-693.0f)*xq+315.0f)*xq-35.0f)*x
#define P8 0.0078125*((((6435.0f*xq-12012.0f)*xq+6930.0f)*xq-1260.0f)*xq+35.0f)
#define P9 \
 (2.4609375f+(-36.09375f+(140.765625f+(-201.09375f+94.9609375f*xq)*xq)*xq)*xq)*x

/*----------------------------------------------------------------------------*/

static float Wlegendre( int m , int npt , float *xx , float *v )
{
  register int ii ;
  register float x , xq , xt ;

  switch( m ){
    case 1: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; v[ii]=P1/(1.0f+xq);
    }
    break;
    case 2: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; v[ii]=P2/(1.0f+xq*xq);
    }
    break;
    case 3: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; v[ii]=P3/(1.0f+xq*xq*xq);
    }
    break;
    case 4: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; xt=xq*xq; v[ii]=P4/(1.0f+xt*xt);
    }
    break;
    case 5: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; xt=xq*xq; v[ii]=P5/(1.0f+xt*xt*xq);
    }
    break;
    case 6: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; xt=xq*xq; v[ii]=P6/(1.0f+xt*xt*xt);
    }
    break;
    case 7: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; xt=xq*xq; v[ii]=P7/(1.0f+xt*xt*xt*xq);
    }
    break;
    case 8: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; xt=xq*xq; xt=xt*xt; v[ii]=P8/(1.0f+xt*xt);
    }
    break;
    case 9: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; xt=xq*xq; xt=xt*xt; v[ii]=P9/(1.0f+xt*xt*xq);
    }
    break;
  }
  return ;
}

/*----------------------------------------------------------------------------*/

void Warpfield_legfun( int kfun, void *vpar,
                       int npt , float *x, float *y, float *z, float *val )
{
   tenprodpar *spar = (tenprodpar *)vpar ;
   int kx, ky, kz ;
   register int ii ;
   register float qq ;

#if 0
   if( spar == NULL || spar->nk < 1     ||
       kfun < 0     || kord >= spar->nk ||
       npt  < 1     ||
       x == NULL    || y == NULL || z == NULL || val == NULL ) return ;
#endif

   kx = spar->kx[kfun+3] ;  /* we skip the first 3 tensor products */
   ky = spar->ky[kfun+3] ;  /* which are (1,0,0), (0,1,0), (0,0,1) */
   kz = spar->kz[kfun+3] ;

   if( kx > 0 )
     Wlegendre( kx , npt , x , val ) ;
   else
     for( ii=0 ; ii < npt ; ii++ ) val[ii] = 1.0f ;

   if( ky > 0 || kz > 0 ){
     float *qv = (float *)malloc(sizeof(float)*npt) ;
     if( ky > 0 ){
       Wlegendre( ky , npt , y , qv ) ;
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= qv[ii] ;
     }

     if( kz > 0 ){
       Wlegendre( kz , npt , z , qv ) ;
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= qv[ii] ;
     }
     free((void *)qv) ;
   }

   /* taper downwards for values far from center */

   for( ii=0 ; ii < npt ; ii++ ){
     qq = x[ii]*x[ii] + y[ii]*y[ii] + z[ii]*z[ii] ;
     val[ii] /= (1.0f+qq*qq) ;
   }

   return ;
}

/*----------------------------------------------------------------------------*/
/* 1D Gegenbauer (alpha=-0.5) polynomials
------------------------------------------------------------------------------*/

#undef G0
#undef G1
#undef G2
#undef G3
#undef G4
#undef G5
#undef G6
#undef G7
#undef G8
#undef G9

#define G0 1.0f  /* not used */
#define G1 x
#define G2 0.1666667f-0.5f*xq            /* G2(x)-1/3 : orthogonal to 1 */
#define G3 (0.3f-0.5f*xq)*x              /* G3(x)-x/5 : orthogonal to x */
#define G4 -0.125f+(0.75f-0.625f*xq)*xq
#define G5 (-0.375f+(1.25f-0.875f*xq)*xq)*x
#define G6 0.0625f+(-0.9375f+(2.1875f-1.3125f*xq)*xq)*xq
#define G7 (0.3125f+(-2.1875f+(3.9375f-2.0625f*xq)*xq)*xq)*x
#define G8 -0.0390625f+(1.09375f+(-4.921875f+(7.21875f-3.3515625f*xq)*xq)*xq)*xq
#define G9 (-0.2734375f+(3.28125f+(-10.828125f+(13.40625f-5.5859375f*xq)*xq)*xq)*xq)*x

/*----------------------------------------------------------------------------*/

static float Wgegen( int m , int npt , float *xx , float *v )
{
  register int ii ;
  register float x,xq,xt ;

  switch( m ){
    case 1: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; v[ii]=G1/(1.0f+xq);
    }
    break;
    case 2: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; v[ii]=G2/(1.0f+xq*xq);
    }
    break;
    case 3: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; v[ii]=G3/(1.0f+xq*xq*xq);
    }
    break;
    case 4: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; xt=xq*xq; v[ii]=G4/(1.0f+xt*xt);
    }
    break;
    case 5: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; xt=xq*xq; v[ii]=G5/(1.0f+xt*xt*xq);
    }
    break;
    case 6: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; xt=xq*xq; v[ii]=G6/(1.0f+xt*xt*xt);
    }
    break;
    case 7: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; xt=xq*xq; v[ii]=G7/(1.0f+xt*xt*xt*xq);
    }
    break;
    case 8: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; xt=xq*xq; xt=xt*xt; v[ii]=G8/(1.0f+xt*xt);
    }
    break;
    case 9: for( ii=0;ii<npt;ii++ ){
      x=xx[ii]; xq=x*x; xt=xq*xq; xt=xt*xt; v[ii]=G9/(1.0f+xt*xt*xq);
    }
    break;
  }
  return ;
}

/*----------------------------------------------------------------------------*/

void Warpfield_gegenfun( int kfun, void *vpar,
                         int npt , float *x, float *y, float *z, float *val )
{
   tenprodpar *spar = (tenprodpar *)vpar ;
   int kx, ky, kz ;
   register int ii ;
   register float qq ;

#if 0
   if( spar == NULL || spar->nk < 1     ||
       kfun < 0     || kord >= spar->nk ||
       npt  < 1     ||
       x == NULL    || y == NULL || z == NULL || val == NULL ) return ;
#endif

   kx = spar->kx[kfun+3] ;  /* we skip the first 3 tensor products */
   ky = spar->ky[kfun+3] ;  /* which are (1,0,0), (0,1,0), (0,0,1) */
   kz = spar->kz[kfun+3] ;

   if( kx > 0 )
     Wgegen( kx , npt , x , val ) ;
   else
     for( ii=0 ; ii < npt ; ii++ ) val[ii] = 1.0f ;

   if( ky > 0 || kz > 0 ){
     float *qv = (float *)malloc(sizeof(float)*npt) ;
     if( ky > 0 ){
       Wgegen( ky , npt , y , qv ) ;
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= qv[ii] ;
     }

     if( kz > 0 ){
       Wgegen( kz , npt , z , qv ) ;
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= qv[ii] ;
     }
     free((void *)qv) ;
   }

   /* taper downwards for values far from center */

   for( ii=0 ; ii < npt ; ii++ ){
     qq = x[ii]*x[ii] + y[ii]*y[ii] + z[ii]*z[ii] ;
     val[ii] /= (1.0f+qq*qq) ;
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
#if 0
INFO_message("fun#%02d: cx=%g cy=%g cz=%g",kk,cx,cy,cz) ;
#endif
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
   int nxy , ii,jj,kk ;
   float *xi,*yi,*zi , val,dx,dy,dz ;

   nxy = nx*ny ;
   xi  = (float *)malloc(sizeof(float)*nxy) ; /* one xy-plane */
   yi  = (float *)malloc(sizeof(float)*nxy) ; /* at a time   */
   zi  = (float *)malloc(sizeof(float)*nxy) ;

   dx = (nx > 1) ? (xt-xb)/(nx-1.0) : 0.0f ;
   dy = (ny > 1) ? (yt-yb)/(ny-1.0) : 0.0f ;
   dz = (nz > 1) ? (zt-zb)/(nz-1.0) : 0.0f ;

   for( jj=0 ; jj < ny ; jj++ ){
     val = yb + jj*dy ;
     for( ii=0 ; ii < nx ; ii++ ){
       xi[ii+nx*jj] = xb + ii*dx ;
       yi[ii+nx*jj] = val ;
     }
   }

   for( kk=0 ; kk < nz ; kk++ ){
     val = zb + kk*dz ;
     for( ii=0 ; ii < nxy ; ii++ ) zi[ii] = val ;
     Warpfield_eval_array( wf , nxy , xi,yi,zi ,
                           xo+kk*nxy , yo+kk*nxy , zo+kk*nxy ) ;
   }

   free((void *)zi); free((void *)yi); free((void *)xi); return ;
}

/*===========================================================================*/
/*===========================================================================*/

int main( int argc , char *argv[] )
{
   int ng , iarg=1 , nf , qq,ii  ;
   float order=2.0f ;
   Warpfield *wf ;
   float *xw , *yw , *zw ;
   THD_3dim_dataset *dset ;
   THD_ivec3 nxyz ;
   THD_fvec3 orgxyz , delxyz ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("%s gridsize [order]\n",argv[0]) ; exit(0) ;
   }

   ng = (int)strtod(argv[iarg++],NULL) ;
   if( ng < 9 ) ERROR_exit("illegal gridsize=%d",ng) ;

   if( iarg < argc ){
     order = (float)strtod(argv[iarg++],NULL) ;
     if( order <= 1.0f ) ERROR_exit("illegal order=%g",order) ;
   }

   wf = Warpfield_init( WARPFIELD_TRIG_TYPE , order , NULL ) ;
   if( wf == NULL ) ERROR_exit("wf is NULL!") ;

   nf = wf->nfun ; INFO_message("%d warp functions",nf) ;

   LOAD_DIAG_MAT44( wf->aa , 0.0f , 0.0f , 0.0f ) ;

   yw = (float *)calloc(sizeof(float),ng*ng*ng) ;
   zw = (float *)calloc(sizeof(float),ng*ng*ng) ;

   dset = EDIT_empty_copy(NULL) ;

   LOAD_IVEC3( nxyz , ng,ng,ng ) ;
   LOAD_FVEC3( orgxyz , -1.0f,-1.0f,-1.0f ) ;
   LOAD_FVEC3( delxyz , 2.0f/(ng-1) , 2.0f/(ng-1) , 2.0f/(ng-1) ) ;
   EDIT_dset_items( dset ,
                      ADN_nxyz   , nxyz   ,
                      ADN_xyzdel , delxyz ,
                      ADN_xyzorg , orgxyz ,
                      ADN_prefix , "warpfield" ,
                      ADN_nvals  , nf ,
                    ADN_none ) ;

   for( qq=0 ; qq < nf ; qq++ ){
     INFO_message("***** start sub-brick #%d",qq) ;
     for( ii=0 ; ii < nf ; ii++ ) wf->cx[ii] = 0.0f ;
     wf->cx[qq] = 1.0f ;
     if( wf->blab != NULL ){
       char *name = wf->blab( qq , wf->bpar ) ;
       if( name != NULL && name[0] != '\0' ) ININFO_message(" label = '%s'",name) ;
     }

     EDIT_substitute_brick( dset , qq , MRI_float , NULL ) ;
     xw = (float *)DSET_ARRAY(dset,qq) ;
     Warpfield_eval_grid( wf , ng , -1.0f , 1.0f ,
                               ng , -1.0f , 1.0f ,
                               ng , -1.0f , 1.0f , xw,yw,zw ) ;
   }
   free(yw);free(zw);

   DSET_write(dset) ; WROTE_DSET(dset) ;
   exit(0) ;
}
