#include "mrilib.h"

#undef  FREEIF
#define FREEIF(p) do{ if((p)!=NULL){free(p);(p)=NULL;} }while(0)

#undef  MAXORD
#define MAXORD 9.99f  /* maximum order in any direction is 9 */

#undef  PI
#define PI  3.14159265f

typedef void (*bfunc1D)(int,int,float *,float *) ;

void Warpfield_trigfun  (int,void *,int,float *,float *,float *,float *);
void Warpfield_legfun   (int,void *,int,float *,float *,float *,float *);
void Warpfield_gegenfun (int,void *,int,float *,float *,float *,float *);

void * Warpfield_prodfun_setup(float,int *,int,void *) ;

void Wtrig    (int,int,float *,float *) ;
void Wlegendre(int,int,float *,float *) ;
void Wgegen   (int,int,float *,float *) ;

void Warpfield_prodfun( int kfun, void *vpar, bfunc1D bf , int npt ,
                        float *x, float *y, float *z, float *val    ) ;

/*---------------------------------------------------------------------------*/

static int verb = 0 ;
void Warpfield_set_verbose( int vv ){ verb = vv; }

/*---------------------------------------------------------------------------*/
/*! Setup a new warpfield. */

Warpfield * Warpfield_init( int type, float order, int flags, floatvec *fv )
{
   Warpfield *wf ;

   if( order < 1.5f ) return(NULL) ;

   wf = (Warpfield *)calloc( 1 , sizeof(Warpfield) ) ;
   wf->type  = type ;
   wf->flags = flags ;

   switch( type ){
     default: free((void *)wf) ; return(NULL) ;  /* bad */

     case WARPFIELD_TRIG_TYPE:
       wf->bset = Warpfield_prodfun_setup ;
       wf->bfun = Warpfield_trigfun ;
     break ;

     case WARPFIELD_LEGEN_TYPE:
       wf->bset = Warpfield_prodfun_setup ;
       wf->bfun = Warpfield_legfun ;
     break ;

     case WARPFIELD_GEGEN_TYPE:
       wf->bset = Warpfield_prodfun_setup ;
       wf->bfun = Warpfield_gegenfun ;
     break ;
   }

   /* identity matrix */

   if( SKIPAFF(flags) )
     LOAD_DIAG_MAT44( wf->aa , 0.0f , 0.0f , 0.0f ) ;
   else
     LOAD_DIAG_MAT44( wf->aa , 1.0f , 1.0f , 1.0f ) ;

   /* copy float vector parameters in, if any [not used at present] */

   if( fv == NULL || fv->nar <= 0 ){
     wf->pv = NULL ;
   } else {
     MAKE_floatvec( wf->pv , fv->nar ) ;
     memcpy( wf->pv->ar , fv->ar , sizeof(float)*fv->nar ) ;
   }

   /* set up space for warping parameters */

   wf->order = order ;
   wf->bpar = wf->bset( wf->order , &(wf->nfun) , wf->flags , (void *)wf->pv ) ;

   if( wf->nfun <= 0 ){
     Warpfield_destroy(wf) ; return(NULL) ;  /* should never transpire */
   }

   wf->cx = (float *)calloc(wf->nfun,sizeof(float)) ;
   wf->cy = (float *)calloc(wf->nfun,sizeof(float)) ;
   wf->cz = (float *)calloc(wf->nfun,sizeof(float)) ;

   return wf ;
}

/*---------------------------------------------------------------------------*/

void Warpfield_destroy( Warpfield *wf )
{
   if( wf == NULL ) return ;
   KILL_floatvec(wf->pv) ;
   if( wf->bpar != NULL ) wf->bset( -1.0f, NULL, wf->flags, wf->bpar ) ;
   FREEIF(wf->cx) ; FREEIF(wf->cy) ; FREEIF(wf->cz) ;
   free((void *)wf) ; return ;
}

/*****************************************************************************/
/********************** Stuff that isn't used anywhere yet. ******************/
#if 0
/*---------------------------------------------------------------------------*/
/*! Change the expansion order of a warpfield. */

void Warpfield_change_order( Warpfield *wf , float neword )
{
   float order=wf->order ; int newfun=0 ;

   if( neword < 0.0f || neword == order ) return ;

   if( wf->bpar != NULL ) wf->bset( -1.0f , NULL , wf->flags , wf->bpar ) ;
   wf->bpar = wf->bset( neword, &newfun, wf->flags, (void *)wf->pv ) ;
   if( newfun <= 0 ){
     FREEIF(wf->cx) ; FREEIF(wf->cy) ; FREEIF(wf->cz) ; newfun = 0 ;
   } else if( newfun != wf->nfun ){
     register int ii ;
     wf->cx = (float *)realloc((void *)wf->cx,sizeof(float)*newfun) ;
     wf->cy = (float *)realloc((void *)wf->cy,sizeof(float)*newfun) ;
     wf->cz = (float *)realloc((void *)wf->cz,sizeof(float)*newfun) ;
     for( ii=wf->nfun ; ii < newfun ; ii++ ){
       wf->cx[ii] = wf->cy[ii] = wf->cz[ii] = 0.0f ;
     }
   }
   wf->order = neword ; wf->nfun = newfun ; return ;
}

/*---------------------------------------------------------------------------*/
/*! Compute coefficients in wf so that the basis function expansion
    evaluated at (xi,yi,zi) is a least squares fit to (xw,yw,zw).
*//*-------------------------------------------------------------------------*/

float Warpfield_lsqfit( Warpfield *wf , int flags , float order ,
                        int npt, float *xi , float *yi , float *zi ,
                                 float *xw , float *yw , float *zw  )
{
   MRI_IMAGE *imbase , *imbinv ;
   float     *bar    , *iar    , *car,*dar,*ear , qsum ;
   int ii , nrow,ncol ;
   register float cs,ds,es ; register int jj ;

   /* change order of expansion, if desired */

   if( order > 0.0f )
     Warpfield_change_order( wf , order ) ;

   /* ncol = number of variables to solve for
      nrow = number of equations being solved */

   ncol = wf->nfun ; nrow = npt ;
   if( ncol >= nrow ) return(-1.0f) ;

   if( verb > 1 ) ININFO_message(" lsqfit: nfun=%d nrow=%d",ncol,nrow) ;

#undef  B   /* macros to access image arrays like matrices */
#undef  P
#define B(i,j) bar[(i)+(j)*nrow]  /* i=0..nrow-1 , j=0..ncol-1 */
#define P(i,j) iar[(i)+(j)*ncol]  /* i=0..ncol-1 , j=0..nrow-1 */

   /* create matrix, each column of which is a
      basis function evaluated at the (xi,yi,zi) points */

   imbase = mri_new(nrow,ncol,MRI_float); bar = MRI_FLOAT_PTR(imbase);
   for( jj=0 ; jj < ncol ; jj++ ){  /* loop over columns */
     car = bar + jj*nrow ;          /* ptr to jj-th column in matrix */
     wf->bfun( jj , wf->bpar , nrow , xi,yi,zi , car ) ;
   }
   if( verb > 1 ) ININFO_message("   |imbase| = %g",mri_matrix_size(imbase)) ;

   /* compute pseudo-inverse of matrix */

   imbinv = mri_matrix_psinv( imbase , NULL , 1.e-8 ) ;
   if( imbinv == NULL ){ mri_free(imbase); return(-2.0f); }  /* bad */
   iar = MRI_FLOAT_PTR(imbinv) ;
   if( verb > 1 ) ININFO_message("   |imbinv| = %g",mri_matrix_size(imbinv)) ;

   /* apply pseudo-inverse to (xw,yw,zw) points,
      to get coefficients for each basis function */

   car = wf->cx ; dar = wf->cy ; ear = wf->cz ;
   for( ii=0 ; ii < ncol ; ii++ ){
     for( cs=ds=es=0.0f,jj=0 ; jj < nrow ; jj++ ){
       cs += P(ii,jj)*xw[jj] ; ds += P(ii,jj)*yw[jj] ; es += P(ii,jj)*zw[jj] ;
     }
     car[ii] = cs ; dar[ii] = ds ; ear[ii] = es ;
   }
   mri_free(imbinv) ;  /* done with this */

   /* compute the RMS error between the fitted basis functions
      evaluated at the (xi,yi,zi) points and the (xw,yw,zw) points */

   qsum = 0.0f ;
   for( ii=0 ; ii < nrow ; ii++ ){
     cs = -xw[ii] ; ds = -yw[ii] ; es = -zw[ii] ;
     for( jj=0 ; jj < ncol ; jj++ ){
       cs += B(ii,jj)*car[jj] ; ds += B(ii,jj)*dar[jj] ; es += B(ii,jj)*ear[jj] ;
     }
     qsum += cs*cs + ds*ds + es*es ;
   }
   mri_free(imbase) ;
   qsum = sqrtf(qsum/nrow) ; return(qsum) ;
}

#undef B
#undef P

/*---------------------------------------------------------------------------*/

#undef  NG
#define NG 18  /* number of grid points to use for invertizing */

/*! Find an approximate inverse to a warpfield. */

Warpfield * Warpfield_inverse( Warpfield *wf , float *rmserr )
{
   Warpfield *uf ;
   mat44 wa , ub ;
   float *xi,*yi,*zi , *xw,*yw,*zw , *gg , dg , ss,tt,uu , ord,egoal,orbot ;
   int npt=NG*NG*NG , ii,jj,kk,pp ;

   /* create the output warpfield */

   uf = Warpfield_init( wf->type , wf->order , wf->flags , wf->pv ) ;
   if( uf == NULL ) return(NULL) ;

   /* its matrix is the inverse of the input's */

   memset( &ub , 0 , sizeof(mat44) ) ;
   if( !WARPFIELD_SKIPAFF(wf) ){
     wa = wf->aa ; ub = uf->aa = MAT44_INV(wa) ;
   }

   /* workspaces */

   xi = (float *)malloc(sizeof(float)*npt) ;
   yi = (float *)malloc(sizeof(float)*npt) ;
   zi = (float *)malloc(sizeof(float)*npt) ;
   xw = (float *)malloc(sizeof(float)*npt) ;
   yw = (float *)malloc(sizeof(float)*npt) ;
   zw = (float *)malloc(sizeof(float)*npt) ;

   /* grid for approximation is non-uniform (weighted towards center) */

   gg = (float *)malloc(sizeof(float)*NG) ;
   dg = 2.0f / NG ;
   for( ii=0 ; ii < NG ; ii++ ) gg[ii] = (2.0f/PI)*asinf(-1.0f+(ii+0.499f)*dg);

   /* create 3D grid of x points for approximation */
   pp = 0 ;
   for( kk=0 ; kk < NG ; kk++ ){
    for( jj=0 ; jj < NG ; jj++ ){
     for( ii=0 ; ii < NG ; ii++ ){
       xw[pp] = gg[ii] ; yw[pp] = gg[jj]; zw[pp] = gg[kk] ; pp++ ;
   }}}
   free((void *)gg) ;

   /* evaluate input warpfield at these grid points */

   Warpfield_eval_array( wf , npt , xw,yw,zw , xi,yi,zi ) ;

   /* set up for approximating inverse at these grid points */

   if( !WARPFIELD_SKIPAFF(uf) ){
     for( ii=0 ; ii < npt ; ii++ ){
       MAT44_VEC( ub , xi[ii],yi[ii],zi[ii] , ss,tt,uu ) ;
       xw[ii] -= ss ; yw[ii] -= tt ; zw[ii] -= uu ;
     }
   }

   /* compute approximate inverse at various orders */

   if( verb ) INFO_message("Start inverse fitting with npt=%d",npt) ;
   if( rmserr != NULL && *rmserr > 0 ) egoal = *rmserr ;
   else                                egoal = 0.00222f ;
   orbot = 2.0f ;
   for( ord=orbot ; ord < MAXORD ; ord += 0.501 ){
     dg = Warpfield_lsqfit( uf , 0 , ord , npt , xi,yi,zi , xw,yw,zw ) ;
     if( verb > 1 ) ININFO_message(" order=%g rmserr=%g nfun=%d",ord,dg,uf->nfun) ;
     if( dg <= egoal ) break ;
   }

   free((void *)zw) ; free((void *)yw) ; free((void *)xw) ;
   free((void *)zi) ; free((void *)yi) ; free((void *)xi) ;

   if( rmserr != NULL ) *rmserr = dg ;
   return(uf) ;
}

/*---------------------------------------------------------------------------*/
/*! Find a lower order approximation to a warpfield. */

Warpfield * Warpfield_approx( Warpfield *wf , float ord , float *rmserr )
{
   Warpfield *uf ;
   mat44 ub ;
   float *xi,*yi,*zi , *xw,*yw,*zw , *gg , dg , ss,tt,uu ;
   int npt=NG*NG*NG , ii,jj,kk,pp ;

   if( ord >= wf->order ) return(NULL) ;

   /* create the output warpfield */

   uf = Warpfield_init( wf->type , ord , wf->flags , wf->pv ) ;
   if( uf == NULL ) return(NULL) ;

   /* its matrix is the same as the input's */

   ub = uf->aa = wf->aa ;

   /* workspaces */

   xi = (float *)malloc(sizeof(float)*npt) ;
   yi = (float *)malloc(sizeof(float)*npt) ;
   zi = (float *)malloc(sizeof(float)*npt) ;
   xw = (float *)malloc(sizeof(float)*npt) ;
   yw = (float *)malloc(sizeof(float)*npt) ;
   zw = (float *)malloc(sizeof(float)*npt) ;

   /* grid for approximation is non-uniform (weighted towards center) */

   gg = (float *)malloc(sizeof(float)*NG) ;
   dg = 2.0f / NG ;
   for( ii=0 ; ii < NG ; ii++ ) gg[ii] = (2.0f/PI)*asinf(-1.0f+(ii+0.499f)*dg);

   /* create 3D grid of x points for approximation */
   pp = 0 ;
   for( kk=0 ; kk < NG ; kk++ ){
    for( jj=0 ; jj < NG ; jj++ ){
     for( ii=0 ; ii < NG ; ii++ ){
       xi[pp] = gg[ii] ; yi[pp] = gg[jj]; zi[pp] = gg[kk] ; pp++ ;
   }}}
   free((void *)gg) ;

   /* evaluate input warpfield at these grid points */

   Warpfield_eval_array( wf , npt , xi,yi,zi , xw,yw,zw ) ;

   /* set up for approximating new warp at these grid points */

   if( !WARPFIELD_SKIPAFF(uf) ){
     for( ii=0 ; ii < npt ; ii++ ){
       MAT44_VEC( ub , xi[ii],yi[ii],zi[ii] , ss,tt,uu ) ;
       xw[ii] -= ss ; yw[ii] -= tt ; zw[ii] -= uu ;
     }
   }

   /* compute approximation */

   dg = Warpfield_lsqfit( uf , 0 , ord , npt , xi,yi,zi , xw,yw,zw ) ;

   free((void *)zw) ; free((void *)yw) ; free((void *)xw) ;
   free((void *)zi) ; free((void *)yi) ; free((void *)xi) ;

   if( rmserr != NULL ) *rmserr = dg ;
   return(uf) ;
}

/*---------------------------------------------------------------------------*/

float Warpfield_compose(void)  /* TBD */
{
}
#endif
/*****************************************************************************/
/*****************************************************************************/

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

int cmp_fvm( const fvm *v , const fvm *w )  /* for sorting */
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

tenprodpar * Warpfield_tenprod_setup( float order )
{
   tenprodpar *spar ;
   int nk , ii,jj,kk , qq,pp ;
   float kt ;
   fvm *kvec , vv ;

   if( order <= 1.5f ) return(NULL) ;  /* bad call */

   /* create a sphere of tensor products up to the given radius */

   qq = 1+(int)ceil(order) ; nk = qq*qq*qq ; kt = 1.0001f*order ;
   kvec = (fvm *)malloc(sizeof(fvm)*nk) ;
   for( pp=kk=0 ; kk < qq ; kk++ ){
    for( jj=0 ; jj < qq ; jj++ ){
     for( ii=0 ; ii < qq ; ii++ ){
       if( ii+jj+kk <= 1 ) continue ; /* skip the lowest orders */
       vv.a = ii ; vv.b = jj ; vv.c = kk ; vv.m = TMAG(vv) ;
       if( vv.m < kt ) kvec[pp++] = vv ;
   }}}
   if( pp <= 1 ){ free((void *)kvec); return(NULL); }

   /* sort by increasing radius */

   qsort( kvec , (size_t)pp , sizeof(fvm) ,
          (int(*)(const void *,const void *))cmp_fvm ) ;

   /* copy sorted tensor product indexes into output struct */

   spar = (tenprodpar *)malloc(sizeof(tenprodpar)) ;
   spar->nk = pp ;
   spar->kx = (int *)  malloc(sizeof(int)  *pp) ;
   spar->ky = (int *)  malloc(sizeof(int)  *pp) ;
   spar->kz = (int *)  malloc(sizeof(int)  *pp) ;
   spar->km = (float *)malloc(sizeof(float)*pp) ;
   for( ii=0 ; ii < pp ; ii++ ){
     spar->kx[ii] = kvec[ii].a ;
     spar->ky[ii] = kvec[ii].b ;
     spar->kz[ii] = kvec[ii].c ;
     spar->km[ii] = kvec[ii].m ;
   }

   free((void *)kvec) ; return(spar) ;
}

/*---------------------------------------------------------------------------*/

void * Warpfield_prodfun_setup( float order, int *nfun, int flags, void *vp )
{
   tenprodpar *spar ;

   /*-- destructor call --*/

   if( order < 0.0f ){
     if( vp != NULL ){
       spar = (tenprodpar *)vp ; DESTROY_tenprodpar(spar) ;
     }
     return(NULL) ;
   }

   if( nfun == NULL || order < 2.0f || order > MAXORD ) return(NULL) ;

   /*-- create list of tensor product indexes --*/

   spar = Warpfield_tenprod_setup(order) ;
   if( spar == NULL ) return(NULL) ;

   *nfun = spar->nk ;     /* 1 function per index */
   return((void *)spar) ;
}

/*----------------------------------------------------------------------------*/

void Warpfield_prodfun( int kfun, void *vpar, bfunc1D bff , int npt ,
                        float *x, float *y, float *z, float *val     )
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

   kx = spar->kx[kfun] ;
   ky = spar->ky[kfun] ;
   kz = spar->kz[kfun] ;

   if( kx > 0 )
     bff( kx , npt , x , val ) ;
   else
     for( ii=0 ; ii < npt ; ii++ ) val[ii] = 1.0f ;

   if( ky > 0 || kz > 0 ){
     register float *qv ;
     qv = (float *)malloc(sizeof(float)*npt) ;
     if( ky > 0 ){
       bff( ky , npt , y , qv ) ;
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= qv[ii] ;
     }

     if( kz > 0 ){
       bff( kz , npt , z , qv ) ;
       for( ii=0 ; ii < npt ; ii++ ) val[ii] *= qv[ii] ;
     }
     free((void *)qv) ;
   }

   /* taper downwards for values far from center */

#if 0
   for( ii=0 ; ii < npt ; ii++ ){
     qq = x[ii]*x[ii] + y[ii]*y[ii] + z[ii]*z[ii] ;
     val[ii] /= (1.0f+qq*qq*qq) ;
   }
#endif

   return ;
}

/*---------------------------------------------------------------------------*/
/* Trig functions in 1D. */

void Wtrig( int m , int npt , float *xx , float *v )
{
  register int ii ;
  register float fac ;

  fac = (0.5f*PI) * (float)m ;   /* frequency */

  switch( m%2 ){  /* odd = sin, even = cos */
    case 1: for( ii=0 ; ii < npt ; ii++ ) v[ii] = sinf( fac * xx[ii] ) ;
    break ;
    case 0: for( ii=0 ; ii < npt ; ii++ ) v[ii] = cosf( fac * xx[ii] ) ;
    break ;
  }
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

void Wlegendre( int m , int npt , float *xx , float *v )
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

void Wgegen( int m , int npt , float *xx , float *v )
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

/*---------------------------------------------------------------------------*/

void Warpfield_trigfun( int kfun, void *vpar,
                        int npt , float *x, float *y, float *z, float *val )
{
   static int first = 1 ;
   if( first ){ INFO_message("Warpfield trigfun initialized"); first = 0; }
   Warpfield_prodfun( kfun , vpar , Wtrig , npt , x,y,z , val ) ;
}

/*----------------------------------------------------------------------------*/

void Warpfield_legfun( int kfun, void *vpar,
                       int npt , float *x, float *y, float *z, float *val )
{
   static int first = 1 ;
   if( first ){ INFO_message("Warpfield legfun initialized"); first = 0; }
   Warpfield_prodfun( kfun , vpar , Wlegendre , npt , x,y,z , val ) ;
}

/*----------------------------------------------------------------------------*/

void Warpfield_gegenfun( int kfun, void *vpar,
                         int npt , float *x, float *y, float *z, float *val )
{
   static int first = 1 ;
   if( first ){ INFO_message("gegenfun"); first = 0; }
   Warpfield_prodfun( kfun , vpar , Wgegen , npt , x,y,z , val ) ;
}

/*---------------------------------------------------------------------------*/
/* Note that if SKIPAFF is turned on, then (xo,yo,zo) inputs are added to,
   but if SKIPAFF is off, then (xo,yo,zo) inputs are cast aside like trash.
*//*-------------------------------------------------------------------------*/

void Warpfield_eval_array( Warpfield *wf ,
                           int npt, float *xi, float *yi, float *zi,
                                    float *xo, float *yo, float *zo )
{
   int kk ;
   register int ii ; register float cx,cy,cz , *val ;

   if( !WARPFIELD_SKIPAFF(wf) ){
     float a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ;
     UNLOAD_MAT44( wf->aa , a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34 ) ;
     for( ii=0 ; ii < npt ; ii++ ){
       xo[ii] = a11*xi[ii] + a12*yi[ii] + a13*zi[ii] + a14 ;
       yo[ii] = a21*xi[ii] + a22*yi[ii] + a23*zi[ii] + a24 ;
       zo[ii] = a31*xi[ii] + a32*yi[ii] + a33*zi[ii] + a34 ;
     }
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
