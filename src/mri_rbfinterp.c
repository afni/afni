#include "mrilib.h"

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
#define MM(i,j) mat[(i)+nn*(j)]  /* access to matrix elements (column-major) */

/*----------------------------------------------------------------------------*/
/*! Evaluate RBF expansion on a set of points in rbg,
    given the set of knots in rbk, and the coefficients of the RBF
    (or the values at the knots) in rbe, and store results in array val.
*//*--------------------------------------------------------------------------*/

int RBF_evaluate( RBF_knots *rbk, RBF_evalues *rbe,
                  RBF_evalgrid *rbg, float *val    )
{
   int ii , jj ;
   int npt , nk ;
   float  rr , xm,ym,zm , xd,yd,zd , rad,rqq , xt,yt,zt , *xx,*yy,*zz ;
   float  xk , yk , zk , sum , *ev , b0,bx,by,bz ;

ENTRY("RBF_evaluate") ;

   if( rbg == NULL || val == NULL ) RETURN(0) ;

   /* if needed, convert rbe to RBF weights, from function values */

   ii = RBF_setup_evalues( rbk , rbe ) ; if( ii == 0 ) RETURN(0) ;

   /* load some local variable */

   npt = rbg->npt ;
   nk  = rbk->nknot ;
   rad = rbk->rad  ; rqq = rbk->rqq ;
   xm  = rbk->xmid ; ym  = rbk->ymid ; zm = rbk->zmid ;
   xd  = rbk->xscl ; yd  = rbk->yscl ; zd = rbk->zscl ;
   xx  = rbk->xknot; yy  = rbk->yknot; zz = rbk->zknot;
   ev  = rbe->val  ;
   b0  = rbe->b0   ; bx  = rbe->bx   ; by = rbe->by   ; bz = rbe->bz ;

   /* evaluate RBF + linear polynomial at each output point */

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

   RETURN(1) ;
}

/*----------------------------------------------------------------------------*/
/*! Setup the particular values from which to interpolate RBF-wise;
    that is, convert them from values at each knot to the weights used
    for the RBF centered at each knot (plus the linear polynomial).
    Return value is 0 if bad things happened, 1 if good things happened.
*//*--------------------------------------------------------------------------*/

int RBF_setup_evalues( RBF_knots *rbk , RBF_evalues *rbe )
{
   int nk , nn , ii , jj ;
   float *mat , *aa , *vv , b0,bx,by,bz ;

ENTRY("RBF_setup_evalues") ;

   if( rbk == NULL || rbe == NULL || rbe->val == NULL ) RETURN(0) ;

   if( rbe->code > 0 ) RETURN(1) ;  /* already contains RBF weights */

   nk  = rbk->nknot ;                           /* number of knots  */
   nn  = nk+4 ;                                /* matrix dimension */
   mat = rbk->psmat ;                         /* inverse matrix   */
   aa  = (float *)calloc(sizeof(float),nk) ; /* output array = RBF weights */
   vv  = rbe->val ;                         /* input array = values at knots */

   /* evaluate weights for each RBF into aa,
      by multiplying inverse matrix into value vector
      (done in this order for column-major indexing efficiency) */

   for( jj=0 ; jj < nk ; jj++ ){
     for( ii=0 ; ii < nk ; ii++ ) aa[ii] += MM(ii,jj) * vv[jj] ;
   }

   /* evaluate linear polynomial coefficients,
      by multiplying last 4 rows of inverse matrix into value vector */

   b0 = bx = by = bz = 0.0f ;
   for( jj=0 ; jj < nk ; jj++ ){
     b0 += MM(nk  ,jj) * vv[jj] ;
     bx += MM(nk+1,jj) * vv[jj] ;
     by += MM(nk+2,jj) * vv[jj] ;
     bz += MM(nk+3,jj) * vv[jj] ;
   }

   /* put results back into rbe */

   memcpy(vv,aa,sizeof(float)*nk) ; free(aa) ;
   rbe->b0 = b0 ; rbe->bx = bx ; rbe->by = by ; rbe->bz = bz ;
   rbe->code = 1 ; /* code that rbe is converted to RBF weights from values */

   RETURN(2) ;
}

/*----------------------------------------------------------------------------*/
/*! Setup the knots for radial basis function; mostly,
    making the inverse matrix for converting knot values to RBF weights.
*//*--------------------------------------------------------------------------*/

RBF_knots * RBF_setup_knots( int nk , float *xx , float *yy , float *zz )
{
   RBF_knots *rbk ;
   int ii , jj , nn ;
   MRI_IMAGE *im_mat , *im_psmat ;
   float     *mat , rr , xm,ym,zm , xd,yd,zd , rad,rqq , xt,yt,zt ;

ENTRY("RBF_setup_knots") ;

   if( nk < 5 || xx == NULL || yy == NULL || zz == NULL ) RETURN(NULL) ;

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
   rad = 4.99f*(xd+yd+zd) / nk ;          /* RBF support radius */
   rqq = rad*rad ;
   xd  = 1.0f / xd ;          /* scale factors : (x-xm)*xd is   */
   yd  = 1.0f / yd ;          /* dimensionless x-value relative */
   zd  = 1.0f / zd ;          /* to middle at xm                */

   /* set up matrix for interpolation */

   nn = nk+4 ;                                /* matrix dimension */
   im_mat = mri_new( nn , nn , MRI_float ) ;
   mat    = MRI_FLOAT_PTR(im_mat) ;           /* zero filled */

   for( ii=0 ; ii < nk ; ii++ ){
     for( jj=0 ; jj < ii ; jj++ ){            /* RBF part of matrix */
       xt = xx[ii]-xx[jj] ;
       yt = yy[ii]-yy[jj] ;
       zt = zz[ii]-zz[jj] ; rr = xt*xt + yt*yt + zt*zt ;
       if( rr >= rqq ) continue ; else rr = sqrtf(rr) / rad ;
       MM(ii,jj) = MM(jj,ii) = RBF_func(rr) ;
     }
     MM(ii,ii)   = MM(ii,nk)   = MM(nk,ii) = 1.0f ;
     MM(ii,nk+1) = MM(nk+1,ii) = (xx[ii]-xm)*xd ;  /* linear polynomial part */
     MM(ii,nk+2) = MM(nk+2,ii) = (yy[ii]-ym)*yd ;  /* (in scaled coords)     */
     MM(ii,nk+3) = MM(nk+3,ii) = (zz[ii]-zm)*zd ;
   }

   /* compute pseudo-inverse of matrix */

   im_psmat = mri_matrix_psinv( im_mat , NULL , 0.00001f ) ;
   mri_free(im_mat) ;
   if( im_psmat == NULL ) RETURN(NULL) ;

   rbk = (RBF_knots *)malloc(sizeof(RBF_knots)) ;
   rbk->nknot = nk  ;
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

   RETURN(rbk) ;
}
