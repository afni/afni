#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#undef DEBUG

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

#undef  Rlog
#define Rlog(x)       (((x)<=0.0f) ? 0.0f : logf(x))

/*! C2 polynomial; argument is 1-r, which should be between 0 and 1 */

#define RBF_func(x)    ((x)*(x)*(x)*(x)*(5.0f-4.0f*x))

#undef  MM
#define MM(i,j) mat[(i)+mm*(j)]  /* access to matrix elements (column-major) */

#define USE_LINPOL

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

#ifdef DEBUG
INFO_message("RBF: evaluate at %d points",npt) ;
#endif

#pragma omp parallel if(npt*nk > 9999)
 {
   int ii , jj ;
   float  rr , xm,ym,zm , xd,yd,zd , rad,rqq , xt,yt,zt , *xx,*yy,*zz ;
   float  xk , yk , zk , sum , *ev ;
#ifdef USE_LINPOL
   float b0,bx,by,bz ;
#endif

   /* load some local variables */

   rad = rbk->rad  ; rqq = rbk->rqq ;
   xm  = rbk->xmid ; ym  = rbk->ymid ; zm = rbk->zmid ;
   xd  = rbk->xscl ; yd  = rbk->yscl ; zd = rbk->zscl ;
   xx  = rbk->xknot; yy  = rbk->yknot; zz = rbk->zknot;
   ev  = rbe->val  ;
#ifdef USE_LINPOL
   b0  = rbe->b0   ; bx  = rbe->bx   ; by = rbe->by   ; bz = rbe->bz ;
#endif

#pragma omp for
   for( ii=0 ; ii < npt ; ii++ ){
     xt = rbg->xpt[ii]; yt = rbg->ypt[ii]; zt = rbg->zpt[ii]; /* output xyz */
     for( sum=0.0f,jj=0 ; jj < nk ; jj++ ){
       xk = xt-xx[jj]; yk = yt-yy[jj]; zk = zt-zz[jj];    /* dist from knot */
       rr = xk*xk + yk*yk + zk*zk ;
       if( rr >= rqq ) continue ;
       rr = 1.0f - sqrtf(rr) / rad ; sum += ev[jj] * RBF_func(rr) ;
     }
#ifdef USE_LINPOL
     val[ii] = sum + b0 + bx*(xt-xm)*xd + by*(yt-ym)*yd + bz*(zt-zm)*zd ;
#else
     val[ii] = sum ;
#endif
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
   float *mat , *aa , *vv ;
#ifdef USE_LINPOL
   float b0,bx,by,bz ;
#endif

ENTRY("RBF_setup_evalues") ;

   if( rbk == NULL || rbe == NULL || rbe->val == NULL ){
     ERROR_message("bad call to RBF_setup_evalues") ; RETURN(0) ;
   }

   if( rbe->code > 0 ) RETURN(1) ;  /* already contains RBF weights */

   nk  = rbk->nknot ;                             /* number of knots  */
   nf  = rbk->nfit ;                             /* number of fit points */
#ifdef USE_LINPOL
   mm  = nk+4 ;                                 /* matrix dimensions */
   nn  = nf+4 ;                                /* mm=#rows nn=#cols */
#else
   mm  = nk ;
   nn  = nf ;
#endif
   mat = rbk->psmat ;                         /* inverse matrix   */
   aa  = (float *)calloc(sizeof(float),nk) ; /* output array = RBF weights */
   vv  = rbe->val ;                         /* input array = values at fit */

   /* evaluate weights for each RBF into aa,
      by multiplying inverse matrix into value vector
      (done in this order for column-major indexing efficiency) */

#ifdef DEBUG
INFO_message("RBF: evaluate inverse matrix times value vector") ;
#endif

#ifdef DEBUG
   if( nk <= 20 && nf <= 20 ){
     INFO_message("value vector") ;
     for( jj=0 ; jj < nf ; jj++ ) fprintf(stderr," %+4f",vv[jj]) ;
     fprintf(stderr,"\n") ;
   }
#endif

STATUS("multiply matrix") ;
   for( jj=0 ; jj < nf ; jj++ ){
     for( ii=0 ; ii < nk ; ii++ ) aa[ii] += MM(ii,jj) * vv[jj] ;
   }

#ifdef DEBUG
   if( nf <= 20 && nk <= 20 ){
     INFO_message("coefficent vector") ;
     for( ii=0 ; ii < nk ; ii++ ) fprintf(stderr," %+4f",aa[ii]) ;
     fprintf(stderr,"\n") ;
   }
#endif

#ifdef USE_LINPOL
   /* evaluate linear polynomial coefficients,
      by multiplying last 4 rows of inverse matrix into value vector */

#ifdef DEBUG
INFO_message("RBF: evaluate linear coefficients") ;
#endif

   b0 = bx = by = bz = 0.0f ;
   for( jj=0 ; jj < nf ; jj++ ){
     b0 += MM(nk  ,jj) * vv[jj] ;
     bx += MM(nk+1,jj) * vv[jj] ;
     by += MM(nk+2,jj) * vv[jj] ;
     bz += MM(nk+3,jj) * vv[jj] ;
   }

#ifdef DEBUG
   if( nf <= 20 && nk <= 20 ){
     INFO_message(" linear coeff: b0=%g bx=%g by=%g bz=%g",b0,bx,by,bz) ;
   }
#endif
#endif /* USE_LINPOL */

   /* put results back into rbe:
       N.B.: on input, vv has nf points and gets nk on output,
             but this is copasetic, since we must have nk <= nf */

   memcpy(vv,aa,sizeof(float)*nk) ; free(aa) ;
#ifdef USE_LINPOL
   rbe->b0 = b0 ; rbe->bx = bx ; rbe->by = by ; rbe->bz = bz ;
#endif
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

#ifdef DEBUG
INFO_message("RBF: rad=%g  xd=%g  yd=%g  zd=%g",rad,xd,yd,zd ) ;
INFO_message("     xm=%g  ym=%g  zm=%g",xm,ym,zm) ;
#endif

   xd  = 1.0f / xd ;          /* scale factors : (x-xm)*xd is   */
   yd  = 1.0f / yd ;          /* dimensionless x-value relative */
   zd  = 1.0f / zd ;          /* to middle at xm                */

   /* set up matrix for interpolation */

#ifdef USE_LINPOL
   nn = nk+4 ;                                /* matrix dimensions */
   mm = nf+4 ;                                /* mm=#rows nn=#cols */
#else
   nn = nk ;
   mm = nf ;
#endif
   im_mat = mri_new( mm , nn , MRI_float ) ;
   mat    = MRI_FLOAT_PTR(im_mat) ;           /* zero filled */

#ifdef DEBUG
INFO_message("RBF: setup %d X %d matrix",mm,nn) ;
#endif

   for( ii=0 ; ii < nf ; ii++ ){
     jtop = (coincide) ? ii+1 : nk ;
     for( jj=0 ; jj < jtop ; jj++ ){            /* RBF part of matrix */
       xt = xf[ii]-xx[jj] ;
       yt = yf[ii]-yy[jj] ;
       zt = zf[ii]-zz[jj] ; rr = xt*xt + yt*yt + zt*zt ;
            if( rr == 0.0f ){                          MM(ii,jj) = 1.0f ;        }
       else if( rr <  rqq  ){ rr = 1.0f-sqrtf(rr)/rad; MM(ii,jj) = RBF_func(rr); }
       if( coincide && jj < ii ) MM(jj,ii) = MM(ii,jj) ;
     }
#ifdef USE_LINPOL
     MM(ii,nk)   = 1.0f ;
     MM(ii,nk+1) = (xf[ii]-xm)*xd ;
     MM(ii,nk+2) = (yf[ii]-ym)*yd ;
     MM(ii,nk+3) = (zf[ii]-zm)*zd ;
#endif
   }
#ifdef USE_LINPOL
   for( jj=0 ; jj < nk ; jj++ ){
     MM(nk  ,jj) = 1.0f ;
     MM(nk+1,jj) = (xx[jj]-xm)*xd ;
     MM(nk+2,jj) = (yy[jj]-ym)*yd ;
     MM(nk+3,jj) = (zz[jj]-zm)*zd ;
   }
#endif

#ifdef DEBUG
   if( nn <= 20 && mm <= 20 ){
     for( ii=0 ; ii < mm ; ii++ ){
       INFO_message("Matrix row %2d",ii) ;
       for( jj=0 ; jj < nn ; jj++ ) fprintf(stderr," %+.4f",MM(ii,jj)) ;
       fprintf(stderr,"\n") ;
     }
   } else {
     INFO_message("Matrix row 0") ;
     for( jj=0 ; jj < nn ; jj++ ){
       fprintf(stderr," %+.4f",MM(0,jj)) ;
       if( jj%20 == 19 ) fprintf(stderr,"\n") ;
     }
     fprintf(stderr,"\n") ;
   }
#endif

   /* compute pseudo-inverse:
          mat is (nf+4) X (nk+4)
        psmat is (nk+4) X (nf+4) */

#ifdef DEBUG
INFO_message("RBF: compute pseudo-inverse") ;
#endif

   im_psmat = mri_matrix_psinv( im_mat , NULL , 0.00001f ) ;
   mri_free(im_mat) ;
   if( im_psmat == NULL ){
     ERROR_message("RBF_setup_knots: pseudo-inversion fails?!"); RETURN(NULL);
   }

#ifdef DEBUG
   mat = MRI_FLOAT_PTR(im_psmat) ; ii = nn ; nn = mm ; mm = ii ;
   if( nn <= 20 && mm <= 20 ){
     for( ii=0 ; ii < mm ; ii++ ){
       INFO_message("pseudo-inverse Matrix row %2d",ii) ;
       for( jj=0 ; jj < nn ; jj++ ) fprintf(stderr," %+.4f",MM(ii,jj)) ;
       fprintf(stderr,"\n") ;
     }
   } else {
     INFO_message("pseudo-inverse Matrix row 0") ;
     for( jj=0 ; jj < nn ; jj++ ){
       fprintf(stderr," %+.4f",MM(0,jj)) ;
       if( jj%20 == 19 ) fprintf(stderr,"\n") ;
     }
     fprintf(stderr,"\n") ;
   }
#endif

   rbk = (RBF_knots *)calloc(1,sizeof(RBF_knots)) ;
   rbk->nknot = nk  ;
   rbk->nfit  = nf ;
   rbk->xmid  = xm  ; rbk->xscl = xd  ;
   rbk->ymid  = ym  ; rbk->yscl = yd  ;
   rbk->zmid  = zm  ; rbk->zscl = zd  ;
   rbk->rad   = rad ; rbk->rqq  = rqq ;
   rbk->xknot = (float *)calloc(sizeof(float),nk) ;
   rbk->yknot = (float *)calloc(sizeof(float),nk) ;
   rbk->zknot = (float *)calloc(sizeof(float),nk) ;
   rbk->psmat = MRI_FLOAT_PTR(im_psmat) ;
   memcpy(rbk->xknot,xx,sizeof(float)*nk) ;
   memcpy(rbk->yknot,yy,sizeof(float)*nk) ;
   memcpy(rbk->zknot,zz,sizeof(float)*nk) ;

   mri_fix_data_pointer(NULL,im_psmat) ; mri_free(im_psmat) ;

   if( coincide ){
     rbk->xfit = rbk->yfit = rbk->zfit = NULL ;
   } else {
     rbk->xfit = (float *)calloc(sizeof(float),nf) ;
     rbk->yfit = (float *)calloc(sizeof(float),nf) ;
     rbk->zfit = (float *)calloc(sizeof(float),nf) ;
     memcpy(rbk->xfit,xf,sizeof(float)*nf) ;
     memcpy(rbk->yfit,yf,sizeof(float)*nf) ;
     memcpy(rbk->zfit,zf,sizeof(float)*nf) ;
   }

   RETURN(rbk) ;
}
