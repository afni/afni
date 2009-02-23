#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#undef DEBUG  /* for some extra printouts */

/******************************************************************************
  Radial Basis Function (RBF) interpolation in 3D.
  + These functions are not designed for high efficiency!
  + They are designed for repeated interpolations from the
    same set of knots, using different sets of values each time.
  + The C2 polynomial with compact support (0 < r < 1)
    f(r) = (1-r)^4*(4r+1) is the RBF used here, with a global linear
    polynomial.
  + Setup for a given collection of knots:        RBF_setup_knots()
  + Setup for a given set of values at the knots: RBF_setup_evalues()
  + Evaluation at an arbitrary set of points:     RBF_evaluate()
    + optional: speedup evaluation a little via:  RBF_setup_kranges()
*******************************************************************************/

/*............................................................................*/
/* RBF expansion
              i=nk-1
     f(x) = sum      { alpha_i * RBF(x-q_i) } + b_0 + x*b_x +y*b_y + z*b_z
              i=0

   where the weights alpha_i for knot location q_i are chosen so that
   the expansion f(q_i) matches the function f() at that location, for
   each knot q_i.  The (optional) global linear polynomial is available
   to provide nonzero values of f(x) for x far away from any knots.
*//*..........................................................................*/

#if 0         /* would be needed for thin-plate spline RBF */
#undef  Rlog
#define Rlog(x)       (((x)<=0.0f) ? 0.0f : logf(x))
#endif

/*! C2 polynomial: argument is 1-r, which should be between 0 and 1;
    note that this function is "positive definite": [M] matrix will be p.d. */

#undef  RBF_func
#define RBF_func(x)    ((x)*(x)*(x)*(x)*(5.0f-4.0f*x))

/*----------------------------------------------------------------------------*/

static int verb = 0 ;
void RBF_set_verbosity( int ii ){ verb = ii ; }

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
   double ct ;

ENTRY("RBF_evaluate") ;

   if( rbk == NULL || rbe == NULL || rbg == NULL || val == NULL ){
     ERROR_message("Illegal call to RBF_evaluate?!") ;
     RETURN(0) ;
   }

   /* if needed, convert rbe to RBF weights, from function values */

   npt = RBF_setup_evalues( rbk , rbe ) ;
   if( npt == 0 ){
     ERROR_message("bad evalues input to RBF_evaluate") ; RETURN(0) ;
   }

   /* evaluate RBF + linear polynomial at each output point */

   npt = rbg->npt ;
   nk  = rbk->nknot ;

   if( verb )
     INFO_message("RBF_evaluate: %d points X %d knots",npt,nk) ;

   ct = COX_clock_time() ;

#pragma omp parallel if(npt*nk > 9999)
 {
   int ii , jj , uselin=rbk->uselin ;
   float  rr , xm,ym,zm , xd,yd,zd , rad,rqq , xt,yt,zt , *xx,*yy,*zz ;
   float  xk , yk , zk , sum , *ev ;
   float b0,bx,by,bz , rai ;
   RBFKINT *kfirst , *klast ; int kbot,ktop ;

   /* load some local variables */

   rad = rbk->rad  ; rqq = rbk->rqq  ; rai = 1.0f / rad ;
   xm  = rbk->xmid ; ym  = rbk->ymid ; zm  = rbk->zmid  ;
   xd  = rbk->xscl ; yd  = rbk->yscl ; zd  = rbk->zscl  ;
   xx  = rbk->xknot; yy  = rbk->yknot; zz  = rbk->zknot ;
   ev  = rbe->val  ;
   if( uselin ){
     b0 = rbe->b0 ; bx = rbe->bx ; by = rbe->by ; bz = rbe->bz ;
   }

   kfirst = rbg->kfirst ; klast = rbg->klast ; kbot = 0 ; ktop = nk-1 ;

#pragma omp for
   for( ii=0 ; ii < npt ; ii++ ){
     xt = rbg->xpt[ii]; yt = rbg->ypt[ii]; zt = rbg->zpt[ii]; /* output xyz */
     if( kfirst != NULL ){ kbot = kfirst[ii] ; ktop = klast[ii] ; }
     for( sum=0.0f,jj=kbot ; jj <= ktop ; jj++ ){
       xk = xt-xx[jj]; rr =xk*xk; if( rr >= rqq ) continue;
       yk = yt-yy[jj]; rr+=yk*yk; if( rr >= rqq ) continue;
       zk = zt-zz[jj]; rr+=zk*zk; if( rr >= rqq ) continue;
       rr = 1.0f - sqrtf(rr) * rai ; sum += ev[jj] * RBF_func(rr) ;
     }
     val[ii] = sum ;
     if( uselin )
       val[ii] += b0 + bx*(xt-xm)*xd + by*(yt-ym)*yd + bz*(zt-zm)*zd ;
   }
 } /* end OpenMP */

   if( verb ) ININFO_message("              Elapsed = %.1f",COX_clock_time()-ct) ;
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
   int nn , ii , jj ;
   float *mat , *vv ; double *aa ;

ENTRY("RBF_setup_evalues") ;

   if( rbk == NULL || rbe == NULL || rbe->val == NULL ){
     ERROR_message("bad call to RBF_setup_evalues") ; RETURN(0) ;
   }

   if( rbe->code > 0 ) RETURN(1) ;  /* already contains RBF weights */

   if( verb )
     INFO_message("RBF_setup_evalues: solve for knot weights") ;

   nn = rbk->nknot ;
   vv = rbe->val ;
   aa = (double *)calloc(sizeof(double),nn) ;
   for( ii=0 ; ii < nn ; ii++ ) aa[ii] = (double)vv[ii] ;

   /* compute [aa] = [Minv][vv] using Choleski factors */

   rcmat_lowert_solve( rbk->Lmat , aa ) ;  /* [Linv] [vv] */
   rcmat_uppert_solve( rbk->Lmat , aa ) ;  /* [Ltinv] [Linv] [vv] */

   /* adjust [aa] via [Q] matrix, for linear polynomial part of fit */

   if( rbk->uselin ){
     double q0,qx,qy,qz , b0,bx,by,bz ; dmat44 Q=rbk->Qmat ;
     float *P0=rbk->P0 , *Px=rbk->Px , *Py=rbk->Py , *Pz=rbk->Pz ;
     for( q0=qx=qy=qz=0.0,ii=0 ; ii < nn ; ii++ ){ /* compute [Pt][aa] */
       q0 += P0[ii]*aa[ii] ; qx += Px[ii]*aa[ii] ;
       qy += Py[ii]*aa[ii] ; qz += Pz[ii]*aa[ii] ;
     }
     DMAT44_VEC( Q , q0,qx,qy,qz , b0,bx,by,bz ) ; /* compute [beta] */
     rbe->b0 = b0 ; rbe->bx = bx ; rbe->by = by ; rbe->bz = bz ;
     for( ii=0 ; ii < nn ; ii++ )                  /* [aa] = [vv] - [P][beta] */
       aa[ii] = (double)vv[ii] - b0*P0[ii] - bx*Px[ii] - by*Py[ii] - bz*Pz[ii] ;

     rcmat_lowert_solve( rbk->Lmat , aa ) ; /* compute [Minv][aa] */
     rcmat_uppert_solve( rbk->Lmat , aa ) ; /* as final RBF weights */
#ifdef DEBUG
  INFO_message("RBF_setup_evalues: b0=%g bx=%g by=%g bz=%g",b0,bx,by,bz) ;
#endif
   }

   /* put results back into rbe struct */

   for( ii=0 ; ii < nn ; ii++ ) vv[ii] = (float)aa[ii] ;
   rbe->code = 1 ; /* code that rbe is converted to RBF weights from values */
   free(aa) ;

   RETURN(2) ;
}

/*----------------------------------------------------------------------------*/
/*! Setup the knots struct for radial basis function evaluation.
*//*--------------------------------------------------------------------------*/

RBF_knots * RBF_setup_knots( int nk, float radius, int uselin,
                             float *xx , float *yy , float *zz )
{
   RBF_knots *rbk ;
   int ii , jj , nn , jtop ;
   float **mat ;
   rcmat *rcm ;
   float *P0=NULL,*Px=NULL,*Py=NULL,*Pz=NULL ;
   float rr , xm,ym,zm , xd,yd,zd , rad,rqq , xt,yt,zt ;
   double ct ;

ENTRY("RBF_setup_knots") ;

   if( nk < 5 || xx == NULL || yy == NULL || zz == NULL ){
     ERROR_message("Illegal inputs to RBF_setup_knots") ; RETURN(NULL) ;
   }

   ct = COX_clock_time() ;

   /* Setup middle of knot field and scale radius:
       xm = middle (median) of the x knot coordinates
       xd = scale (MAD) of the x knot distances from xm;
             would be about L/4 if knots are uniformly spaced
             in 1D over a distance of L
       ==> 4*xd/nk is about inter-knot x distance in 1D uniform case
       ==> 4*(xd+yd+zd)/(3*cbrt(nk)) is about mean inter-knot distance in 3D
       RBF support radius is chosen to be a small multiple of this distance */

   qmedmad_float( nk , xx , &xm , &xd ) ;
   qmedmad_float( nk , yy , &ym , &yd ) ;
   qmedmad_float( nk , zz , &zm , &zd ) ;

   if( radius <= 0.0f )
     rad = 4.321f*(xd+yd+zd) / cbrtf((float)nk) ;  /* RBF support radius */
   else
     rad = radius ;                          /* user-selected RBF radius */

   rqq = rad*rad ;           /* for testing radius-squared */

   if( verb )
     INFO_message("RBF_setup_knots: knots=%d radius=%.1f",nk,rad) ;

   xd = 1.0f / xd ;          /* scale factors : (x-xm)*xd is   */
   yd = 1.0f / yd ;          /* dimensionless x-value relative */
   zd = 1.0f / zd ;          /* to middle at xm                */

   /*...................................................................*//*
      Set up symmetric nk X nk matrix for interpolation:
        mat[ii][jj] = RBF( knot[ii] - knot[jj] )  for jj=0..ii.
      Call this matrix M.  It is positive definite by the choice
      of the particular radial basis function being used.

      If we are NOT using linear polynomials, then the system of
      equations to solve for RBF knot weights [alpha] given knot
      function values [v] is
        [M] [alpha] = [v]
      We Choleski decompose [M] = [L] [Lt] so that we solve via
        [alpha] = [Ltinv][Linv] [v]
      This will be done in function RBF_setup_evalues(), which
      converts values of the function to be interpolated at the
      knots [v] into weights [alpha].

      If we ARE using a global linear polynomial, then the system
      of equations to solve is
        [ M  P ] [alpha] = [v]
        [ Pt 0 ] [beta ] = [0]
      where [P] is an nk X 4 matrix, with entries
        P_{i0}=1  P_{i1}=x_i  P_{i2}=y_i  P_{i3}=z_i (knot coordinates)
        Pt = P-transpose
        [beta] = 4-vector of weights for linear polynomial
      The purpose of this is to make the RBF part of the expansion
      not contain any linear component -- that will be carried by the
      global linear polynomial.  Without this constraint, the system
      would be over-determined: nk equations with nk+4 unknowns.

      The first nk equations express the interpolation property:
      the evaluated RBF sum plus the linear polynomial equals [v]
      at the knots.  The last 4 equations express the property that
      the [alpha] weights by themselves do NOT have any component of
      a global linear polynomial.

      The symmetric compound (nk+4)X(nk+4) matrix is obviously not
      positive definite (since it has 0 values on the diagonal), so
      cannot be solved directly by Choleski factorization.  Instead,
      we solve it via a bordering technique:
         [M][alpha] + [P][beta] = [v]
           ==> [alpha] = [Minv][v] - [Minv][P][beta]          (*)
         [Pt][alpha] = [0]
           ==> [Pt][Minv][v] = [Pt][Minv][P][beta]
           ==> [beta] = inv{ [Pt][Minv][P] }[Pt][Minv][v]     (**)
      Once [beta] is known from Eqn(**), [alpha] is found by using Eqn(*).
      In details, given the Choleski factors of [M]:
        Compute 4x4 matrix [Q] = inv{ [Pt][Ltinv][Linv][P] }  (done below)
        Given [v], [ahat] = [Ltinv][Linv][v]  (==[alpha] if not using [P])
                   [beta] = [Q][Pt][ahat]
                  [alpha] = [Ltinv][Linv]{ [v] - [P][beta] }
      We then use [alpha] to evaluate the RBF at each output grid point,
      and use [beta] to evaluate the global linear polynomial.
   *//*.....................................................................*/

   /* create the [M] matrix rows */

   nn = nk ;
   mat = (float **)malloc(sizeof(float *)*nn) ;
   for( ii=0 ; ii < nn ; ii++ )
     mat[ii] = (float *)calloc((ii+1),sizeof(float)) ;

   /* create the [P] matrix columns as needed for the linear polynomial */

   if( uselin ){
     P0 = (float *)calloc(nn,sizeof(float)) ;
     Px = (float *)calloc(nn,sizeof(float)) ;
     Py = (float *)calloc(nn,sizeof(float)) ;
     Pz = (float *)calloc(nn,sizeof(float)) ;
   }

   if( verb > 1 ) ININFO_message("                 matrix computation") ;

   /* load the [M] matrix and the [P] matrix */

   for( ii=0 ; ii < nn ; ii++ ){
     for( jj=0 ; jj < ii ; jj++ ){    /* RBF between knots */
       xt = xx[ii]-xx[jj] ;
       yt = yy[ii]-yy[jj] ;
       zt = zz[ii]-zz[jj] ; rr = xt*xt + yt*yt + zt*zt ;
       if( rr >= rqq ) continue ;
       rr = 1.0f - sqrtf(rr)/rad ; mat[ii][jj] = RBF_func(rr) ;
     }
     mat[ii][ii] = 1.0000005f ;  /* RBF(0) = 1 by definition */

     if( uselin ){               /* [P] matrix for linear polynomial */
       P0[ii] = 1.0f ;
       Px[ii] = (xx[ii]-xm)*xd ;
       Py[ii] = (yy[ii]-ym)*yd ;
       Pz[ii] = (zz[ii]-zm)*zd ;
     }
   }

   if( verb > 1 ) ININFO_message("                 matrix factorization") ;

   /* convert mat[][] into an rcmat sparse symmetric matrix struct */

   rcm = rcmat_from_rows( nn , mat ) ;

   for( ii=0 ; ii < nn ; ii++ ) free(mat[ii]) ;  /* don't need mat */
   free(mat) ;                                   /* no more */

   if( rcm == NULL ){
     ERROR_message("RBF_setup_knots: setup of rcmat fails!?") ;
     if( uselin ){ free(P0); free(Px); free(Py); free(Pz); }
     RETURN(NULL) ;
   }

   /* Choleski decompose M matrix */

   ii = rcmat_choleski( rcm ) ;
   if( ii > 0 ){
     ERROR_message("RBF_setup_knots: Choleski of rcmat fails at row %d!",ii) ;
     if( uselin ){ free(P0); free(Px); free(Py); free(Pz); }
     RETURN(NULL) ;
   }

   /* create output struct, save stuff into it */

   rbk = (RBF_knots *)calloc(1,sizeof(RBF_knots)) ;
   rbk->nknot = nk  ;
   rbk->rad   = rad ; rbk->rqq  = rqq ;
   rbk->xmid  = xm  ; rbk->xscl = xd  ;
   rbk->ymid  = ym  ; rbk->yscl = yd  ;
   rbk->zmid  = zm  ; rbk->zscl = zd  ;
   rbk->xknot = (float *)calloc(sizeof(float),nk) ; /* saved knots */
   rbk->yknot = (float *)calloc(sizeof(float),nk) ;
   rbk->zknot = (float *)calloc(sizeof(float),nk) ;
   memcpy(rbk->xknot,xx,sizeof(float)*nk) ;
   memcpy(rbk->yknot,yy,sizeof(float)*nk) ;
   memcpy(rbk->zknot,zz,sizeof(float)*nk) ;

   rbk->Lmat = rcm ;  /* saved [L] matrix */

   rbk->uselin = uselin ;
   rbk->P0 = P0 ; rbk->Px = Px ; rbk->Py = Py ; rbk->Pz = Pz ;

   /* compute the Q matrix for the linear polynomial coefficients */

   if( uselin ){
     double *vv[4],*vi,*vj ; dmat44 Q ; double sum ; register int kk ;
#ifdef DEBUG
  INFO_message("     compute Q matrix") ;
#endif
     vv[0] = (double *)malloc(sizeof(double)*nn) ;
     vv[1] = (double *)malloc(sizeof(double)*nn) ;
     vv[2] = (double *)malloc(sizeof(double)*nn) ;
     vv[3] = (double *)malloc(sizeof(double)*nn) ;
     for( ii=0 ; ii < nn ; ii++ ){
       vv[0][ii] = P0[ii] ; vv[1][ii] = Px[ii] ;
       vv[2][ii] = Py[ii] ; vv[3][ii] = Pz[ii] ;
     }
     /* compute [Linv][P] into the 4 columns [vv] */
     rcmat_lowert_solve(rcm,vv[0]) ; rcmat_lowert_solve(rcm,vv[1]) ;
     rcmat_lowert_solve(rcm,vv[2]) ; rcmat_lowert_solve(rcm,vv[3]) ;
     /* compute 4x4 matrix [vv]^T [vv], then invert it to get output [Q] */
     for( ii=0 ; ii < 4 ; ii++ ){
       vi = vv[ii] ;
       for( jj=0 ; jj < 4 ; jj++ ){
         vj = vv[jj] ;
         for( sum=0.0,kk=0 ; kk < nn ; kk++ ) sum += vi[kk]*vj[kk] ;
         Q.m[ii][jj] = sum ;
       }
     }
     free(vv[0]) ; free(vv[1]) ; free(vv[2]) ; free(vv[3]) ;
     rbk->Qmat = generic_dmat44_inverse(Q) ;  /* [Q] matrix to be saved */
#ifdef DEBUG
  DUMP_DMAT44("Q",Q) ;
  DUMP_DMAT44("Qinv",rbk->Qmat) ;
#endif
   }

   if( verb > 1 ) ININFO_message("                 Elapsed = %.1f",COX_clock_time()-ct) ;

   RETURN(rbk) ;
}

/*------------------------------------------------------------------*/

#if 0
#undef  ADDTO_intar
#define ADDTO_intar(nar,nal,ar,val)                                         \
 do{ if( (nar) == (nal) ){                                                  \
       (nal) = 1.2*(nal)+16; (ar) = (int *)realloc((ar),sizeof(int)*(nal)); \
     }                                                                      \
     (ar)[(nar)++] = (val);                                                 \
 } while(0)

#undef  CLIP_intar
#define CLIP_intar(nar,nal,ar)                                       \
 do{ if( (nar) < (nal) && (nar) > 0 ){                               \
       (nal) = (nar); (ar) = (int *)realloc((ar),sizeof(int)*(nal)); \
 }} while(0)
#endif

/*------------------------------------------------------------------*/
/*! For speedup, for each grid point compute the first and last
    knots that can affect its output value.  This will save time
    when looping over knots to evaluate the RBF expansion.
*//*----------------------------------------------------------------*/

void RBF_setup_kranges( RBF_knots *rbk , RBF_evalgrid *rbg )
{
   int npt , nk ;
   double ct ;

ENTRY("RBF_setup_kranges") ;

   if( rbk == NULL || rbg == NULL ) EXRETURN ;

   if( rbg->klast != NULL ){ free(rbg->klast) ; rbg->klast = NULL; }
   if( rbg->kfirst!= NULL ){ free(rbg->kfirst); rbg->kfirst= NULL; }

   if( rbk->nknot > 65535 ) EXRETURN ; /* can't store as unsigned short */

   /* load some local variables */

   npt = rbg->npt ;
   nk  = rbk->nknot ;

   rbg->kfirst = (RBFKINT *)calloc(sizeof(RBFKINT),npt) ;
   rbg->klast  = (RBFKINT *)calloc(sizeof(RBFKINT),npt) ;

   if( verb ){
     INFO_message("RBF_setup_kranges: %d grid points",npt) ;
   }

   ct = COX_clock_time() ;

#pragma omp parallel if(npt*nk > 9999)
 {
   int ii,jj , kbot,ktop ;
   float xt,yt,zt, rqq, xk,yk,zk, rr, *xx,*yy,*zz ;
   RBFKINT *klast , *kfirst ;

   rqq = rbk->rqq ; xx = rbk->xknot; yy = rbk->yknot; zz = rbk->zknot;
   kfirst = rbg->kfirst ; klast  = rbg->klast  ;
#pragma omp for
   for( ii=0 ; ii < npt ; ii++ ){
     xt = rbg->xpt[ii]; yt = rbg->ypt[ii]; zt = rbg->zpt[ii];
     kbot = ktop = -1 ;
     for( jj=0 ; jj < nk ; jj++ ){
       xk = xt-xx[jj]; rr =xk*xk; if( rr >= rqq ) continue;
       yk = yt-yy[jj]; rr+=yk*yk; if( rr >= rqq ) continue;
       zk = zt-zz[jj]; rr+=zk*zk; if( rr >= rqq ) continue;
       ktop = jj ;
       if( kbot < 0 ) kbot = jj ;
     }
     if( kbot >= 0 ){
       kfirst[ii] = (RBFKINT)kbot ;
       klast [ii] = (RBFKINT)ktop ;
     }
   }
 } /* end OpenMP */

   if( verb > 1 ){
     float ntot=0.0f ; int ii ;
     for( ii=0 ; ii < npt ; ii++ ) ntot += (1.0f+rbg->klast[ii]-rbg->kfirst[ii]) ;
     ININFO_message("                   average krange = %.1f  Elapsed = %.1f",
                    ntot/npt , COX_clock_time()-ct ) ;
   }

   EXRETURN ;
}
