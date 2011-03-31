#include "mrilib.h"

   /*-------------------------------------------------------------------*/
   /*** LASSO functions for AFNI programs.                            ***/
   /*** We start with some utilities for setting LASSO parameters.    ***/
   /*** Then some internal (static) functions for common necessities. ***/
   /*-------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/

/** set the fixed value of lambda (flam);
    note that flam will always be positive (never 0) **/

static float flam = 0.666f ;

void THD_lasso_fixlam( float x ){ if( x > 0.0f ) flam = x ; }

/*............................................................................*/

/** set the convergence parameter (deps) **/

static float deps = 0.0001234567f ;

void THD_lasso_setdeps( float x ){ if( x > 0.0f && x <= 0.1f ) deps = x ; }

/*............................................................................*/

/** set this to 1 to do 'post-LASSO' re-regression **/

static int do_post = 0 ;

void THD_lasso_dopost( int x ){ do_post = x ; }

/*............................................................................*/

/** set this to 1 to scale LASSO lambda by estimated sigma **/

static int do_sigest = 0 ;

void THD_lasso_dosigest( int x ){ do_sigest = x ; }

/*............................................................................*/

/** set the entire lambda vector **/

static floatvec *vlam = NULL ;

void THD_lasso_setlamvec( int nref , float *lam )
{
   register int ii ;
ENTRY("THD_lasso_setlamvec") ;
#pragma omp critical (MALLOC)
   { KILL_floatvec(vlam) ; }
   if( nref > 0 && lam != NULL ){
#pragma omp critical (MALLOC)
     { MAKE_floatvec(vlam,nref) ; }
     for( ii=0 ; ii < nref ; ii++ ) vlam->ar[ii] = lam[ii] ;
   }
   EXRETURN ;
}

/*............................................................................*/

/** set initial parameters estimates **/

static floatvec *vpar = NULL ;

void THD_lasso_setparvec( int nref , float *par )
{
   register int ii ;
ENTRY("THD_lasso_setparvec") ;
#pragma omp critical (MALLOC)
   { KILL_floatvec(vpar) ; }
   if( nref > 0 && par != NULL ){
#pragma omp critical (MALLOC)
     { MAKE_floatvec(vpar,nref) ; }
     for( ii=0 ; ii < nref ; ii++ ) vpar->ar[ii] = par[ii] ;
   }
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static float estimate_sigma( int npt , float *far )
{
   float *dif , mad1,mad2 ; int ii ;

   if( npt < 9 || far == NULL ) return 0.0f ;

#pragma omp critical (MALLOC)
   { dif = (float *)malloc(sizeof(float)*npt) ; }

   for( ii=0 ; ii < npt-1 ; ii++ ) dif[ii] = far[ii+1]-far[ii] ;
   qmedmad_float( npt-1 , dif , NULL , &mad1 ) ; mad1 *= 1.05f ;
   for( ii=0 ; ii < npt-2 ; ii++ ) dif[ii] = 0.5f*(far[ii+2]+far[ii]) - far[ii+1] ;
   qmedmad_float( npt-2 , dif , NULL , &mad2 ) ; mad2 *= 1.21f ;

#pragma omp critical (MALLOC)
   { free(dif) ; }

   return MAX(mad1,mad2) ;
}

/*----------------------------------------------------------------------------*/
/* Construct a local copy of lam[], and edit it softly. */

static float * edit_lamvec( int npt , int nref , float *lam )
{
   float *mylam ;
   int nfree , jj ;

ENTRY("edit_lamvec") ;

#pragma omp critical (MALLOC)
   { mylam = (float *)calloc(sizeof(float),nref) ; }

   nfree = nref ;
   if( lam != NULL ){                       /* copy input lam */
     for( nfree=jj=0 ; jj < nref ; jj++ ){
       mylam[jj] = MAX(0.0f,lam[jj]) ; if( mylam[jj] == 0.0f ) nfree++ ;
     }
   }

   if( nfree >= MIN(nref,npt) ){ /* no good input lam, so make one up */
     nfree = 0 ;
     if( vlam != NULL ){         /* take from user-supplied vector */
       int nvlam = vlam->nar ;
       for( jj=0 ; jj < nref ; jj++ ){
         if( jj < nvlam ){
           mylam[jj] = vlam->ar[jj] ;
                if( mylam[jj] <  0.0f ) mylam[jj] = flam ;
           else if( mylam[jj] == 0.0f ) nfree++ ;
         } else {
           mylam[jj] = flam ;
         }
       }
       if( nfree >= npt ){               /* too many free values */
         for( jj=0 ; jj < nref ; jj++ )
           if( mylam[jj] == 0.0f ) mylam[jj] = flam ;
       }
     } else {                            /* fixed value of lam */
       for( jj=0 ; jj < nref ; jj++ ) mylam[jj] = flam ;
     }
   }

   RETURN(mylam) ;
}

/*----------------------------------------------------------------------------*/
/* Compute the un-penalized solution to only the 'free' parameters,
   as marked in fr[].  The results are stored back into ppar[]; un-free
   parameters in ppar[] are not altered.
*//*--------------------------------------------------------------------------*/

static void compute_free_param( int npt  , float *far   ,
                                int nref , float *ref[] ,
                                int meth , float *ccon  ,
                                int nfree, byte  *fr    , float *ppar )
{
   float **qref,*qcon=NULL ; floatvec *qfit ; int nc,ii,jj ;

ENTRY("compute_free_param") ;

   if( nfree <= 0 || nfree >= npt || fr == NULL || ppar == NULL ) EXRETURN ;

#pragma omp critical (MALLOC)
   {                    qref = (float **)calloc(sizeof(float *),nfree) ;
     if( ccon != NULL ) qcon = (float * )calloc(sizeof(float)  ,nfree) ; }

   /* select the marked regressors and constraints */

   for( nc=ii=jj=0 ; jj < nref ; jj++ ){
     if( fr[jj] ){  /* use this parameter */
       if( ccon != NULL && ccon[jj] != 0 ){ qcon[ii] = ccon[jj]; nc++; }
       qref[ii++] = ref[jj] ;
     }
   }

#pragma omp critical (MALLOC)
   { if( nc == 0 ){ free(qcon); qcon = NULL; } }

   /* regress-ifization */

   qfit = THD_fitter( npt , far , nfree , qref , meth , qcon ) ;

   /* copy results into output */

   if( qfit != NULL ){
     for( ii=jj=0 ; jj < nref ; jj++ ){
       if( fr[jj] ) ppar[jj] = qfit->ar[ii++] ;
     }
   }

   /* vamoose the ranch */

#pragma omp critical (MALLOC)
   { free(qref) ;
     if( qcon != NULL ) free(qcon) ;
     if( qfit != NULL ) KILL_floatvec(qfit) ; }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Check inputs for stupidities */

static int check_inputs( int npt  , float *far ,
                         int nref , float *ref[] )
{
   int jj ;
   if( npt <= 1 || far == NULL || nref <= 0 || ref == NULL ) return 1 ;
   for( jj=0 ; jj < nref ; jj++ ) if( ref[jj] == NULL ) return 2 ;
   return 0 ;
}

/*----------------------------------------------------------------------------*/

floatvec * THD_lasso( int meth   ,
                      int npt    , float *far   ,
                      int nref   , float *ref[] ,
                      float *lam , float *ccon   )
{
   switch( meth ){

     default:
     case  2:
     case -2: return THD_lasso_L2fit( npt,far , nref,ref , lam,ccon ) ;

     case  1:
     case -1: return THD_sqrtlasso_L2fit( npt,far , nref,ref , lam,ccon ) ;

   }
   return NULL ; /* unreachable */
}

/*----------------------------------------------------------------------------*/
/* LASSO (L2 fit + L1 penalty) fit a vector to a set of reference vectors.
   Input parameters are
    * npt     = Length of input vectors
    * nref    = Number of references
    * far     = Vector to be fitted
    * ref[k]  = k-th reference vector, for k=0..nref-1
    * lam[k]  = L1 penalty factor for the k-th reference (non-negative)
                If lam == NULL, or all values are zero, then the value
                set by THD_lasso_fixlam() will be used instead.
    * ccon[k] = If ccon != NULL, ccon[k] is a sign constraint on the k-th
                output coefficient: ccon[k] = 0 == no constraint
                                            > 0 == coef #k must be >= 0
                                            < 0 == coef #k must be <= 0
   Unlike standard linear fitting, nref can be more than npt, since the
   L1 penalty can force some coefficients to be exactly zero.  However,
   at most npt-1 values of lam[] can be zero (or the problem is unsolvable).

   The return vector contains the nref coefficients.  If NULL is returned,
   then something bad bad bad transpired and you should hang your head.

   TT Wu and K Lange.
   Coordinate descent algorithms for LASSO penalized regression.
   Annals of Applied Statistics, 2: 224-244 (2008).
   http://arxiv.org/abs/0803.3876
*//*--------------------------------------------------------------------------*/

floatvec * THD_lasso_L2fit( int npt    , float *far   ,
                            int nref   , float *ref[] ,
                            float *lam , float *ccon   )
{
   int ii,jj, nfree,nite,nimax,ndel , do_slam ;
   float *mylam, *ppar, *resd, *rsq, *rj, pj,dg,dsum,ll ;
   floatvec *qfit ; byte *fr ;

ENTRY("THD_lasso_L2fit") ;

   jj = check_inputs( npt , far , nref , ref ) ;
   if( jj ){
     static int ncall=0 ;
     if( ncall == 0 ){ ERROR_message("LASSO: bad data and/or model"); ncall++; }
     RETURN(NULL) ;
   }

   /*--- construct a local copy of lam[], and edit it softly ---*/

   mylam = edit_lamvec( npt , nref , lam ) ;

   /*--- space for parameter iterates, etc (initialized to zero) ---*/

#pragma omp critical (MALLOC)
   { MAKE_floatvec(qfit,nref) ; ppar = qfit->ar ;   /* parameters = output */
     resd = (float *)calloc(sizeof(float),npt ) ;   /* residuals */
     rsq  = (float *)calloc(sizeof(float),nref) ;   /* sums of squares */
     fr   = (byte  *)calloc(sizeof(byte) ,nref) ; } /* free list */

   /*--- Save 1/(sum of squares) of each ref column ---*/

   dsum = (do_sigest) ? estimate_sigma(npt,far) : 1.0f ;

   nfree = 0 ;                    /* number of unconstrained parameters */
   for( jj=0 ; jj < nref ; jj++ ){
     rj = ref[jj] ;
     for( pj=ii=0 ; ii < npt ; ii++ ) pj += rj[ii]*rj[ii] ;
     if( pj > 0.0f ){
       rsq[jj] = 1.0f / pj ;
       if( mylam[jj] == 0.0f ){   /* unconstrained parameter */
         fr[jj] = 1 ;  nfree++ ;
       } else {
         mylam[jj] *= dsum * sqrtf(pj) ; /* scale for size of regressors */
       }
     }
   }

   /*--- if any parameters are free (no L1 penalty),
         initialize them by un-penalized least squares
         (implicitly assuming all other parameters are zero) ---*/

   if( vpar == NULL || vpar->nar < nref ){
     compute_free_param( npt,far,nref,ref,2,ccon , nfree,fr , ppar ) ;
   } else {
     for( ii=0 ; ii < nref ; ii++ ) ppar[ii] = vpar->ar[ii] ;
   }

   /*--- initialize residuals ---*/

   for( ii=0 ; ii < npt ; ii++ ) resd[ii] = far[ii] ;          /* data */

   for( jj=0 ; jj < nref ; jj++ ){  /* subtract off fit of each column */
     pj = ppar[jj] ; rj = ref[jj] ; /* with a nonzero parameter value */
     if( pj != 0.0f ){
       for( ii=0 ; ii < npt ; ii++ ) resd[ii] -= rj[ii]*pj ;
     }
   }

   /*--- if have a lot of references (relative to number of data points),
         then increase lam[] for the first iterations to speed convergence ---*/

   do_slam = 3 * (nref > npt/2) ;                        /* first 3 */
   if( do_slam ){                                        /*       | */
     for( jj=0 ; jj < nref ; jj++ ) mylam[jj] *= 8.0f ;  /* 8 = 2^3 */
   }

   /*---- outer iteration loop (until we are happy or worn out) ----*/

#undef  CON     /* CON(j) is true if the constraint on ppar[j] is violated */
#define CON(j)  (ccon != NULL && ppar[j]*ccon[j] < 0.0f)

#undef  CONP    /* CONP(j) is true if ppar[j] is supposed to be >= 0 */
#define CONP(j) (ccon != NULL && ccon[j] > 0.0f)

#undef  CONN    /* CONN(j) is true if ppar[j] is supposed to be <= 0 */
#define CONN(j) (ccon != NULL && ccon[j] < 0.0f)

   ii = MAX(nref,npt) ; jj = MIN(nref,npt) ; nimax = 17 + 5*ii + 31*jj ;
   dsum = 1.0f ;
   for( nite=0 ; nite < nimax && dsum > deps ; nite++ ){

     /*-- cyclic inner loop over parameters --*/

     for( dsum=ndel=jj=0 ; jj < nref ; jj++ ){  /* dsum = sum of param deltas */

       if( rsq[jj] == 0.0f ) continue ; /* all zero column!? */
       rj = ref[jj] ;                   /* j-th reference column */
       pj = ppar[jj] ;                  /* current value of j-th parameter */
       ll = mylam[jj] ;

       /* compute dg = -gradient of un-penalized function wrt ppar[jj] */
       /*            = direction we want to step in                    */

       for( dg=ii=0 ; ii < npt ; ii++ ) dg += resd[ii] * rj[ii] ;

       /*- modify parameter down the gradient -*/

       if( ll == 0.0f ){          /* un-penalized parameter */

         ppar[jj] += dg*rsq[jj] ; if( CON(jj) ) ppar[jj] = 0.0f ;

       } else {                   /* penalized parameter */

         /* Extra -gradient is -lambda for pj > 0, and is +lambda for pj < 0. */
         /* Merge this with dg, change ppar[jj], then see if we stepped thru */
         /* zero (or hit a constraint) -- if so, stop ppar[jj] at zero.     */

         if( pj > 0.0f || (pj == 0.0f && dg > ll) ){         /* on the + side */
           dg -= ll ; ppar[jj] += dg*rsq[jj] ;               /* shrink - way */
           if( ppar[jj] < 0.0f || CON(jj) ) ppar[jj] = 0.0f ;
         } else if( pj < 0.0f || (pj == 0.0f && dg < -ll) ){ /* on the - side */
           dg += ll ; ppar[jj] += dg*rsq[jj] ;               /* shrink + way */
           if( ppar[jj] > 0.0f || CON(jj) ) ppar[jj] = 0.0f ;
         }

       }

       dg = ppar[jj] - pj ;   /* change in parameter */
       if( dg != 0.0f ){      /* update convergence test and residuals */
         pj    = fabsf(ppar[jj]) + fabsf(pj) ;
         dsum += fabsf(dg) / MAX(pj,0.001f) ; ndel++ ;
         for( ii=0 ; ii < npt ; ii++ ) resd[ii] -= rj[ii] * dg ;
       }

     } /*-- end of inner loop over parameters --*/

     /**** test for convergence somehow ***/

     if( ndel > 0 ) dsum *= (2.0f/ndel) ;

     if( do_slam ){     /* shrink lam[] back, if it was augmented */
       do_slam-- ; dsum = 1.0f ;
       for( jj=0 ; jj < nref ; jj++ ) mylam[jj] *= 0.5f ;
     }

   } /*---- end of outer iteration loop ----*/

#if 1
   { static int ncall=0 ;
     if( ncall < 1 ){
       INFO_message("LASSO: nite=%d dsum=%g",nite,dsum) ; ncall++ ;
     }
   }
#endif

   /*--- if 'post' computation is ordered, re-do the
         regression without constraints, but using only
         the references with non-zero weights from above ---*/

   if( do_post ){
     nfree = 0 ;
     for( jj=0 ; jj < nref ; jj++ ){  /* count and mark params to use */
       fr[jj] = (ppar[jj] != 0.0f) ; nfree += fr[jj] ;
     }
     compute_free_param( npt,far,nref,ref,2,ccon , nfree,fr , ppar ) ;
   }

   /*--- Loading up the truck and heading to Beverlee ---*/

#pragma omp critical (MALLOC)
   { free(fr) ; free(rsq) ; free(resd) ; free(mylam) ; }

   RETURN(qfit) ;
}

/*----------------------------------------------------------------------------*/
/**------ minimizers of f(x) = sqrt(a*x*x+b*x+c) + d*x
          valid for d*d < a and for 4*a*c-b*b > 0  [positive quadratic] -----**/

#undef  XPLU             /* for a > d > 0 */
#define XPLU(a,b,c,d)                                               \
   ( -( (b) * ((a)-(d)*(d))                                         \
       + sqrtf( (d)*(d) * ((a)-(d)*(d)) * (4.0f*(a)*(c)-(b)*(b)) )  \
      ) / ( 2.0f * (a) * ((a)-(d)*(d)) )                            \
   )

#undef  XMIN             /* for -a < d < 0 */
#define XMIN(a,b,c,d)                                               \
   ( -( (b) * ((a)-(d)*(d))                                         \
       - sqrtf( (d)*(d) * ((a)-(d)*(d)) * (4.0f*(a)*(c)-(b)*(b)) )  \
      ) / ( 2.0f * (a) * ((a)-(d)*(d)) )                            \
   )

/*----------------------------------------------------------------------------*/
/* Square Root L2 LASSO, similar to L2 LASSO function directly above; see
     A Belloni, V Chernozhukov, and L Wang.
     Square-root LASSO: Pivotal recovery of sparse signals via conic programming.
     http://arxiv.org/abs/1009.5689
*//*--------------------------------------------------------------------------*/

floatvec * THD_sqrtlasso_L2fit( int npt    , float *far   ,
                                int nref   , float *ref[] ,
                                float *lam , float *ccon   )
{
   int ii,jj, nfree,nite,nimax,ndel ;
   float *mylam, *ppar, *resd, *rsq, *rj, pj,dg,dsum ;
   float rqsum,aa,bb,cc,ll,all , npinv , *ain,*qal ;
   floatvec *qfit ; byte *fr ;

ENTRY("THD_sqrtlasso_L2fit") ;

   /*--- check inputs for stupidities ---*/

   jj = check_inputs( npt , far , nref , ref ) ;
   if( jj ){
     static int ncall=0 ;
     if( ncall == 0 ){ ERROR_message("SQRT LASSO: bad data and/or model"); ncall++; }
     RETURN(NULL) ;
   }

   /*--- construct a local copy of lam[], and edit it softly ---*/

   mylam = edit_lamvec( npt , nref , lam ) ;

   /*--- space for parameter iterates, etc (initialized to zero) ---*/

#pragma omp critical (MALLOC)
   { MAKE_floatvec(qfit,nref) ; ppar = qfit->ar ;   /* parameters = output */
     resd = (float *)calloc(sizeof(float),npt ) ;   /* residuals */
     rsq  = (float *)calloc(sizeof(float),nref) ;   /* sums of squares */
     fr   = (byte  *)calloc(sizeof(byte) ,nref) ;   /* free list */
     ain  = (float *)calloc(sizeof(float),nref) ;
     qal  = (float *)calloc(sizeof(float),nref) ;
   }

   /*--- Save sum of squares of each ref column ---*/

   npinv = 1.0f / (float)npt ;
   nfree = 0 ;                    /* number of unconstrained parameters */
   for( jj=0 ; jj < nref ; jj++ ){
     rj = ref[jj] ;
     for( pj=ii=0 ; ii < npt ; ii++ ) pj += rj[ii]*rj[ii] ;
     rsq[jj] = pj * npinv ;
     if( pj > 0.0f ){
       if( mylam[jj] == 0.0f ){ fr[jj] = 1 ; nfree++ ; }  /* unconstrained */
       ain[jj] = -0.5f / rsq[jj] ;
     }
   }

   /* scale and edit mylam to make sure it isn't too big */

   cc = sqrtf(npinv) ;
   for( jj=0 ; jj < nref ; jj++ ){
     ll = mylam[jj] ;
     if( ll > 0.0f ){
       aa = sqrtf(rsq[jj]); ll *= aa*npinv; if( ll > 0.666f*aa ) ll = 0.666f*aa;
       mylam[jj] = ll ;
       qal[jj]   = ll*ll * 4.0f*rsq[jj]/(rsq[jj]-ll*ll) ;
     }
   }

   /*--- if any parameters are free (no L1 penalty),
         initialize them by un-penalized least squares
         (implicitly assuming all other parameters are zero) ---*/

   if( vpar == NULL || vpar->nar < nref ){
     compute_free_param( npt,far,nref,ref,2,ccon , nfree,fr , ppar ) ;
   } else {
     for( ii=0 ; ii < nref ; ii++ ) ppar[ii] = vpar->ar[ii] ;
   }

   /*--- initialize residuals ---*/

   for( ii=0 ; ii < npt ; ii++ ) resd[ii] = far[ii] ;          /* data */

   for( jj=0 ; jj < nref ; jj++ ){  /* subtract off fit of each column */
     pj = ppar[jj] ; rj = ref[jj] ; /* with a nonzero parameter value */
     if( pj != 0.0f ){
       for( ii=0 ; ii < npt ; ii++ ) resd[ii] -= rj[ii]*pj ;
     }
   }
   for( rqsum=ii=0 ; ii < npt ; ii++ ) rqsum += resd[ii]*resd[ii] ;
   rqsum *= npinv ;

   /*---- outer iteration loop (until we are happy or worn out) ----*/

#undef  CON    /* CON(j) is true if the constraint on ppar[j] is violated */
#define CON(j) (ccon != NULL && ppar[j]*ccon[j] < 0.0f)

   ii = MAX(nref,npt) ; jj = MIN(nref,npt) ; nimax = 17 + 5*ii + 31*jj ;
   dsum = 1.0f ;
   for( nite=0 ; nite < nimax && dsum > deps ; nite++ ){

     /*-- cyclic inner loop over parameters --*/

     for( dsum=ndel=jj=0 ; jj < nref ; jj++ ){  /* dsum = sum of param deltas */

       if( rsq[jj] == 0.0f ) continue ; /* all zero column!? */
       rj = ref[jj] ;                   /* j-th reference column */
       pj = ppar[jj] ;                  /* current value of j-th parameter */

       for( dg=ii=0 ; ii < npt ; ii++ ) dg += resd[ii] * rj[ii] ;
       dg *= npinv ;

       /* want to minimize (wrt x) function sqrt(aa*x*x+bb*x+cc) + ll*abs(x) */

       aa = rsq[jj] ;
       bb = -2.0f * (dg+aa*pj) ;
       cc = rqsum + (2.0f*dg + aa*pj)*pj ;
       ll = mylam[jj] ;

       /*- modify parameter -*/

       if( ll == 0.0f ){   /* un-penalized parameter */

         ppar[jj] = bb * ain[jj] ; if( CON(jj) ) ppar[jj] = 0.0f ;

       } else {
#if 0
         float qq = ll * sqrtf(4.0f*aa*cc/(aa-ll*ll)) ;
         if( pj > 0.0f || (pj == 0.0f && bb+qq < 0.0f) ){
           ppar[jj] = (bb+qq) * ain[jj] ;      /* solution on positive side */
           if( ppar[jj] < 0.0f || CON(jj) ) ppar[jj] = 0.0f ;
         } else if( pj < 0.0f || (pj == 0.0f && bb-qq > 0.0f) ){
           ppar[jj] = (bb-qq) * ain[jj] ;      /* solution on negative side */
           if( ppar[jj] > 0.0f || CON(jj) ) ppar[jj] = 0.0f ;
         }
#else
         float qq = qal[jj] * cc ;              /* positive by construction */
         if( pj > 0.0f ){
           ppar[jj] = (bb+sqrtf(qq)) * ain[jj] ;           /* positive side */
           if( ppar[jj] < 0.0f || CON(jj) ) ppar[jj] = 0.0f ;
         } else if( pj < 0.0f ){
           ppar[jj] = (bb-sqrtf(qq)) * ain[jj] ;           /* negative side */
           if( ppar[jj] > 0.0f || CON(jj) ) ppar[jj] = 0.0f ;
         } else {                                        /* initial pj == 0 */
           if( bb*bb > qq ){         /* gradient step overpowers L1 penalty */
             if( bb < 0.0f && !CONN(jj) ){                /* [note ain < 0] */
               ppar[jj] = (bb+sqrtf(qq)) * ain[jj] ;      /* step to + side */
             } else if( !CONP(jj) ){
               ppar[jj] = (bb-sqrtf(qq)) * ain[jj] ;      /* step to - side */
             }
           }
         }
#endif
       }

       dg = ppar[jj] - pj ;   /* change in parameter */
       if( dg != 0.0f ){      /* update convergence test and residuals */
         pj    = fabsf(ppar[jj]) + fabsf(pj) ;
         dsum += fabsf(dg) / MAX(pj,0.001f) ; ndel++ ;
         for( rqsum=ii=0 ; ii < npt ; ii++ ){
           resd[ii] -= rj[ii] * dg ; rqsum += resd[ii]*resd[ii] ;
         }
         rqsum *= npinv ;
       }

     } /*-- end of inner loop over parameters --*/

     /**** test for convergence somehow ***/

     if( ndel > 0 ) dsum *= (2.0f/ndel) ;

   } /*---- end of outer iteration loop ----*/

#if 1
   { static int ncall=0 ;
     if( ncall < 1 ){
       INFO_message("SQRTLASSO: nite=%d dsum=%g",nite,dsum) ; ncall++ ;
     }
   }
#endif

   /*--- if 'post' computation is ordered, re-do the
         regression without constraints, but using only
         the references with non-zero weights from above ---*/

   if( do_post ){
     nfree = 0 ;
     for( jj=0 ; jj < nref ; jj++ ){  /* count and mark params to use */
       fr[jj] = (ppar[jj] != 0.0f) ; nfree += fr[jj] ;
     }
     compute_free_param( npt,far,nref,ref,2,ccon , nfree,fr , ppar ) ;
   }

   /*--- Loading up the truck and heading to Beverlee ---*/

#pragma omp critical (MALLOC)
   { free(qal) ; free(ain) ; free(fr) ; free(rsq) ; free(resd) ; free(mylam) ; }

   RETURN(qfit) ;
}
