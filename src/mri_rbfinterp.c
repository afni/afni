#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/******************************************************************************
  Radial Basis Function (RBF) interpolation in 3D.
  + These functions are not designed for high efficiency.
  + They are designed for repeated interpolations from the
    same set of knots, using different sets of values each time.
  + The C2 polynomial with compact support (0 < x < 1)
    f(x) = (1-x)^2*(4x+1) is the RBF used here, with a global linear
    polynomial.
  + Setup for a given collection of knots:        RBF_setup_knots()
  + Setup for a given set of values at the knots: RBF_setup_evalues()
  + Evaluation at an arbitrary set of points:     RBF_evaluate()
*******************************************************************************/

#undef  RBF_PC2
#undef  RBF_CTPS2a
#undef  RBF_CTPS2b
#undef  Rlog

#define Rlog(x)       (((x)<=0.0f) ? 0.0f : logf(x))

/*! C2 polynomial */

#define RBF_CP2(x)    ((1.0f-(x))*(1.0f-(x))*(4.0f*(x)+1.0f))

/*! C2 compact support thin plate spline (a) */

#define RBF_CTPS2a(x) \
  (1.0f+(x)*(x)*(-30.0f+(x)*(-10.0f-60.0f*Rlog(x)+(x)*(45.0f-6.0f*(x)))))

/*! C2 compact support thin plate spline (b) */

#define RBF_CTPS2b(x) \
  (1.0f+(x)*(x)*(-20.0f+(x)*(-80.0f+(x)*(-45.0f+60.0f*Rlog(x)-16.0f*(x)))))

#undef  RBF_func
#define RBF_func RBF_CP2         /* sets which radial basis function to use */

#undef  MM
#define MM(i,j) mat[(i)+mm*(j)]  /* access to matrix elements (column-major) */

/*----------------------------------------------------------------------------*/
/*! Evaluate RBF expansion on a set of points in rbg,
    given the set of knots in rbk, and given the coefficients of the RBF
    (or the values at the fit points) in rbe, and store results in array val
    -- which should be rbg->npt points long.
*//*--------------------------------------------------------------------------*/

int RBF_evaluate( RBF_knots *rbk, RBF_evalues *rbe,
                  RBF_evalgrid *rbg, float *val    )
{
   int npt , nk ;

ENTRY("RBF_evaluate") ;

   if( rbg == NULL || val == NULL ){
     ERROR_message("Bad call to RBF_evaluate") ; RETURN(0) ;
   }

   /* if needed, convert rbe to RBF weights, from function values */

   npt = RBF_setup_evalues( rbk , rbe ) ;
   if( npt == 0 ){
     ERROR_message("bad evalues input to RBF_evaluate") ; RETURN(0) ;
   }

   /* evaluate RBF + linear polynomial at each output point */

   npt = rbg->npt ;
   nk  = rbk->nknot ;

INFO_message("RBF: evaluate at %d points",npt) ;

#pragma omp parallel if(npt*nk > 9999)
 {
   int ii , jj ;
   float  rr , xm,ym,zm , xd,yd,zd , rad,rqq , xt,yt,zt , *xx,*yy,*zz ;
   float  xk , yk , zk , sum , *ev , b0,bx,by,bz ;

   /* load some local variables */

   rad = rbk->rad  ; rqq = rbk->rqq ;
   xm  = rbk->xmid ; ym  = rbk->ymid ; zm = rbk->zmid ;
   xd  = rbk->xscl ; yd  = rbk->yscl ; zd = rbk->zscl ;
   xx  = rbk->xknot; yy  = rbk->yknot; zz = rbk->zknot;
   ev  = rbe->val  ;
   b0  = rbe->b0   ; bx  = rbe->bx   ; by = rbe->by   ; bz = rbe->bz ;

#pragma omp for
   for( ii=0 ; ii < npt ; ii++ ){
     xt = rbg->xpt[ii]; yt = rbg->ypt[ii]; zt = rbg->zpt[ii]; /* output xyz */
     for( sum=b0,jj=0 ; jj < nk ; jj++ ){
       xk = xt-xx[jj]; yk = yt-yy[jj]; zk = zt-zz[jj];    /* dist from knot */
       rr = xk*xk + yk*yk + zk*zk ;
       if( rr >= rqq ) continue ; else rr = sqrtf(rr) / rad ;
       sum += ev[jj] * RBF_func(rr) ;
     }
     val[ii] = sum + bx*(xt-xm)*xd + by*(yt-ym)*yd + bz*(zt-zm)*zd ;
   }
 } /* end OpenMP */

   RETURN(1) ;
}

/*----------------------------------------------------------------------------*/
/*! Setup the particular values from which to interpolate RBF-wise;
    that is, convert them from values at each fit point to the weights 
    used for the RBF centered at each knot (plus the linear polynomial).
    Return value is 0 if bad things happened, 1 if good things happened.
*//*--------------------------------------------------------------------------*/

int RBF_setup_evalues( RBF_knots *rbk , RBF_evalues *rbe )
{
   int nk,nf , nn,mm , ii , jj ;
   float *mat , *aa , *vv , b0,bx,by,bz ;

ENTRY("RBF_setup_evalues") ;

   if( rbk == NULL || rbe == NULL || rbe->val == NULL ){
     ERROR_message("bad call to RBF_setup_evalues") ; RETURN(0) ;
   }

   if( rbe->code > 0 ) RETURN(1) ;  /* already contains RBF weights */

   nk  = rbk->nknot ;                             /* number of knots  */
   nf  = rbk->nfit ;                             /* number of fit points */
   mm  = nk+4 ;                                 /* matrix dimensions */
   nn  = nf+4 ;                                /* mm=#rows nn=#cols */
   mat = rbk->psmat ;                         /* inverse matrix   */
   aa  = (float *)calloc(sizeof(float),nk) ; /* output array = RBF weights */
   vv  = rbe->val ;                         /* input array = values at fit */

   /* evaluate weights for each RBF into aa,
      by multiplying inverse matrix into value vector
      (done in this order for column-major indexing efficiency) */

INFO_message("RBF: evaluate inverse matrix times value vector") ;

   for( jj=0 ; jj < nf ; jj++ ){
     for( ii=0 ; ii < nk ; ii++ ) aa[ii] += MM(ii,jj) * vv[jj] ;
   }

   /* evaluate linear polynomial coefficients,
      by multiplying last 4 rows of inverse matrix into value vector */

INFO_message("RBF: evaluate linear coefficients") ;

   b0 = bx = by = bz = 0.0f ;
   for( jj=0 ; jj < nf ; jj++ ){
     b0 += MM(nk  ,jj) * vv[jj] ;
     bx += MM(nk+1,jj) * vv[jj] ;
     by += MM(nk+2,jj) * vv[jj] ;
     bz += MM(nk+3,jj) * vv[jj] ;
   }

   /* put results back into rbe:
       N.B.: on input, vv has nf points and gets nk on output,
             but this is copasetic, since we must have nk <= nf */

   memcpy(vv,aa,sizeof(float)*nk) ; free(aa) ;
   rbe->b0 = b0 ; rbe->bx = bx ; rbe->by = by ; rbe->bz = bz ;
   rbe->code = 1 ; /* code that rbe is converted to RBF weights from values */

   RETURN(2) ;
}

/*----------------------------------------------------------------------------*/
/*! Setup the knots for radial basis function; mostly,
    making the inverse matrix for converting knot values to RBF weights.
*//*--------------------------------------------------------------------------*/

RBF_knots * RBF_setup_knots( int nk , float *xx , float *yy , float *zz ,
                             int nf , float *xf , float *yf , float *zf  )
{
   RBF_knots *rbk ;
   int ii , jj , nn , mm , coincide=0,jtop ;
   MRI_IMAGE *im_mat , *im_psmat ;
   float     *mat , rr , xm,ym,zm , xd,yd,zd , rad,rqq , xt,yt,zt ;

ENTRY("RBF_setup_knots") ;

   if( nk < 5 || xx == NULL || yy == NULL || zz == NULL ){
     ERROR_message("Illegal call to RBF_setup_knots") ; RETURN(NULL) ;
   }

   /* if no fit points are given, then use the knots */

   if( nf == 0 || xf == NULL || yf == NULL || zf == NULL ){
     nf = nk ; xf = xx ; yf = yy ; zf = zz ; coincide = 1 ;
   } else if( nf < nk ){
     ERROR_message("RBF_setup_knots: nf=%d < nk=%d",nf,nk) ; RETURN(NULL) ;
   }

   /* set up middle of knot field and scale radius */
   /* xm = middle (median) of the x knot coordinates
      xd = scale (MAD) of the x knot distances from xm;
           would be about L/4 if knots are uniformly spaced
           over a distance of L
      4*xd/nk is about inter-knot x distance in uniform case
      4*(xd+yd+zd)/(3*nk) is about average inter-knot distance in 3D
      the RBF support radius is chosen to be a small multiple of this distance */

   qmedmad_float( nk , xx , &xm , &xd ) ;
   qmedmad_float( nk , yy , &ym , &yd ) ;
   qmedmad_float( nk , zz , &zm , &zd ) ;
   rad = 4.99f*(xd+yd+zd) / cbrtf((float)nk) ;  /* RBF support radius */
   rqq = rad*rad ;

INFO_message("RBF: rad=%g  xd=%g  yd=%g  zd=%g",rad,xd,yd,zd ) ;
INFO_message("     xm=%g  ym=%g  zm=%g",xm,ym,zm) ;

   xd  = 1.0f / xd ;          /* scale factors : (x-xm)*xd is   */
   yd  = 1.0f / yd ;          /* dimensionless x-value relative */
   zd  = 1.0f / zd ;          /* to middle at xm                */

   /* set up matrix for interpolation */

   nn = nk+4 ;                                /* matrix dimensions */
   mm = nf+4 ;                                /* mm=#rows nn=#cols */
   im_mat = mri_new( mm , nn , MRI_float ) ;
   mat    = MRI_FLOAT_PTR(im_mat) ;           /* zero filled */

INFO_message("RBF: setup %d X %d matrix",mm,nn) ;

   for( ii=0 ; ii < nf ; ii++ ){
     jtop = (coincide) ? ii+1 : nk ;
     for( jj=0 ; jj < jtop ; jj++ ){            /* RBF part of matrix */
       xt = xf[ii]-xx[jj] ;
       yt = yf[ii]-yy[jj] ;
       zt = zf[ii]-zz[jj] ; rr = xt*xt + yt*yt + zt*zt ;
            if( rr == 0.0f ){                     MM(ii,jj) = 1.0f ;        }
       else if( rr <  rqq  ){ rr = sqrtf(rr)/rad; MM(ii,jj) = RBF_func(rr); }
       if( coincide && jj < ii ) MM(jj,ii) = MM(ii,jj) ;
     }
     MM(ii,nk)   = 1.0f ;
     MM(ii,nk+1) = (xf[ii]-xm)*xd ;
     MM(ii,nk+2) = (yf[ii]-ym)*yd ;
     MM(ii,nk+3) = (zf[ii]-zm)*zd ;
   }
   for( jj=0 ; jj < nk ; jj++ ){
     MM(nk  ,jj) = 1.0f ;
     MM(nk+1,jj) = (xx[ii]-xm)*xd ;
     MM(nk+2,jj) = (yy[ii]-ym)*yd ;
     MM(nk+3,jj) = (zz[ii]-zm)*zd ;
   }

#if 0
INFO_message("first row of matrix:") ;
for( jj=0 ; jj < nn ; jj++ ){
  fprintf(stderr," %.4f",MM(0,jj)) ;
  if( jj%10 == 0 && jj > 0 ) fprintf(stderr,"\n") ;
}
#endif

   /* compute pseudo-inverse:
          mat is (nf+4) X (nk+4)
        psmat is (nk+4) X (nf+4) */

INFO_message("RBF: compute pseudo-inverse") ;

   im_psmat = mri_matrix_psinv( im_mat , NULL , 0.00001f ) ;
   mri_free(im_mat) ;
   if( im_psmat == NULL ){
     ERROR_message("RBF_setup_knots: pseudo-inversion fails?!"); RETURN(NULL);
   }

   /* from malloc    12 Feb 2009 [lesstif patrol] */
   rbk = (RBF_knots *)calloc(1, sizeof(RBF_knots)) ;
   rbk->nknot = nk  ;
   rbk->nfit  = nf ;
   rbk->xmid  = xm  ; rbk->xscl = xd  ;
   rbk->ymid  = ym  ; rbk->yscl = yd  ;
   rbk->zmid  = zm  ; rbk->zscl = zd  ;
   rbk->rad   = rad ; rbk->rqq  = rqq ;
   rbk->xknot = (float *)malloc(sizeof(float)*nk) ;
   rbk->yknot = (float *)malloc(sizeof(float)*nk) ;
   rbk->zknot = (float *)malloc(sizeof(float)*nk) ;
   rbk->psmat = MRI_FLOAT_PTR(im_psmat) ;
   memcpy(rbk->xknot,xx,sizeof(float)*nk) ;
   memcpy(rbk->yknot,yy,sizeof(float)*nk) ;
   memcpy(rbk->zknot,zz,sizeof(float)*nk) ;
   mri_fix_data_pointer(NULL,im_psmat) ; mri_free(im_psmat) ;

   if( coincide ){
     rbk->xfit = rbk->yfit = rbk->zfit = NULL ;
   } else {
     rbk->xfit = (float *)malloc(sizeof(float)*nf) ;
     rbk->yfit = (float *)malloc(sizeof(float)*nf) ;
     rbk->zfit = (float *)malloc(sizeof(float)*nf) ;
     memcpy(rbk->xfit,xf,sizeof(float)*nf) ;
     memcpy(rbk->yfit,yf,sizeof(float)*nf) ;
     memcpy(rbk->zfit,zf,sizeof(float)*nf) ;
   }

   RETURN(rbk) ;
}
