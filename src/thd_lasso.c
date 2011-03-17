#include "mrilib.h"

   /*-------------------------------------------------------------------*/
   /*** LASSO functions for AFNI programs.                            ***/
   /*** We start with some utilities for setting LASSO parameters.    ***/
   /*** Then some internal (static) functions for common necessities. ***/
   /*-------------------------------------------------------------------*/

/*----------------------------------------------------------------------------*/

/** set the fixed value of lambda (flam);
    note that flam will always be positive (never 0) **/

static float flam = 0.01f ;

void THD_lasso_fixlam( float x ){ if( x > 0.0f ) flam = x ; }

/*............................................................................*/

/** set the convergence parameter (deps) **/

static float deps = 0.0002f ;

void THD_lasso_setdeps( float x ){ if( x > 0.0f && x <= 0.1f ) deps = x ; }

/*............................................................................*/

/** set this to 1 to do 'post-LASSO' re-regression **/

static int do_post = 0 ;

void THD_lasso_dopost( int x ){ do_post = x ; }

/*............................................................................*/

/** set the entire lambda vector **/

static floatvec *vlam = NULL ;

void THD_lasso_setlamvec( int nref , float *lam )
{
#pragma omp critical (MALLOC)
   { KILL_floatvec(vlam) ; }
   if( nref > 0 && lam != NULL ){
#pragma omp critical (MALLOC)
     { MAKE_floatvec(vlam,nref) ; }
#pragma omp critical (MEMCPY)
     { memcpy(vlam->ar,lam,sizeof(float)*nref) ; }
   }
   return ;
}

/*----------------------------------------------------------------------------*/
/* Construct a local copy of lam[], and edit it softly. */

static float * edit_lamvec( int npt , int nref , float *lam )
{
   float *mylam ;
   int nfree , jj ;

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

   return mylam ;
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

   if( nfree <= 0 || nfree >= npt || fr == NULL || ppar == NULL ) return ;

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

   return ;
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
   float *mylam, *ppar, *resd, *rsq, *rj, pj,dg,dsum ;
   floatvec *qfit ; byte *fr ;

ENTRY("THD_lasso_L2fit") ;

   /*--- check inputs for stupidities ---*/

   if( npt <= 1 || far == NULL || nref <= 0 || ref == NULL ){
     static int ncall=0 ;
     if( ncall == 0 ){ ERROR_message("LASSO: bad data and/or model"); ncall++; }
     RETURN(NULL) ;
   }

   for( jj=0 ; jj < nref ; jj++ ){
     if( ref[jj] == NULL ){
       static int ncall=0 ;
       if( ncall == 0 ){ ERROR_message("LASSO: bad data and/or model"); ncall++; }
       RETURN(NULL) ;
     }
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

   nfree = 0 ;                    /* number of unconstrained parameters */
   for( jj=0 ; jj < nref ; jj++ ){
     rj = ref[jj] ;
     for( pj=ii=0 ; ii < npt ; ii++ ) pj += rj[ii]*rj[ii] ;
     if( pj > 0.0f ){
       rsq[jj] = 1.0f / pj ;
       if( mylam[jj] == 0.0f ){   /* unconstrained parameter */
         fr[jj] = 1 ;  nfree++ ;
       }
     }
   }

   /*--- if any parameters are free (no L1 penalty),
         initialize them by un-penalized least squares
         (implicitly assuming all other parameters are zero) ---*/

   compute_free_param( npt,far,nref,ref,2,ccon , nfree,fr , ppar ) ;

   /*--- initialize residuals ---*/

   for( ii=0 ; ii < npt ; ii++ ) resd[ii] = far[ii] ;          /* data */

   for( jj=0 ; jj < nref ; jj++ ){  /* subtract off fit of each column */
     pj = ppar[jj] ; rj = ref[jj] ; /* with a nonzero parameter value */
     if( pj != 0.0f ){
       for( ii=0 ; ii < npt ; ii++ ) resd[ii] -= rj[ii]*pj ;
     }
   }

   /*--- if have a lot of references (relative to number of data points),
         then increase lam[] for the first iteration to speed convergence ---*/

   do_slam = 2 * (nref > npt/2) ;
   if( do_slam ){
     for( jj=0 ; jj < nref ; jj++ ) mylam[jj] *= 4.0f ;
   }

   /*---- outer iteration loop (until we are happy or worn out) ----*/

#undef  CON    /* CON(j) is true if the constraint on ppar[j] is violated */
#define CON(j) (ccon != NULL && ppar[j]*ccon[j] < 0.0f)

   nimax = 29*nref + 66 ; dsum = 1.0f ;
   for( nite=0 ; nite < nimax && dsum > deps ; nite++ ){

     /*-- cyclic inner loop over parameters --*/

     for( dsum=ndel=jj=0 ; jj < nref ; jj++ ){  /* dsum = sum of param deltas */

       if( rsq[jj] == 0.0f ) continue ; /* all zero column!? */
       rj = ref[jj] ;                   /* j-th reference column */
       pj = ppar[jj] ;                  /* current value of j-th parameter */

       /* compute dg = -gradient of un-penalized function wrt ppar[jj] */
       /*            = direction we want to step in                    */

       for( dg=ii=0 ; ii < npt ; ii++ ) dg += resd[ii] * rj[ii] ;

       /*- modify parameter down the gradient -*/

       if( mylam[jj] == 0.0f ){   /* un-penalized parameter */

         ppar[jj] += dg*rsq[jj] ; if( CON(jj) ) ppar[jj] = 0.0f ;

       } else {                   /* penalized parameter */

         /* Extra -gradient is -lambda for pj > 0, and is +lambda for pj < 0. */
         /* Merge this with dg, change ppar[jj], then see if we stepped thru */
         /* zero (or hit a constraint) -- if so, stop ppar[jj] at zero.     */

         if( pj > 0.0f || (pj == 0.0f && dg > 0.0f) ){        /* on the + side */
           dg -= mylam[jj] ; ppar[jj] += dg*rsq[jj] ;         /* shrink - way */
           if( ppar[jj] < 0.0f || CON(jj) ) ppar[jj] = 0.0f ;
         } else if( pj < 0.0f || (pj == 0.0f && dg < 0.0f) ){ /* on the - side */
           dg += mylam[jj] ; ppar[jj] += dg*rsq[jj] ;         /* shrink + way */
           if( ppar[jj] > 0.0f || CON(jj) ) ppar[jj] = 0.0f ;
         }

       }

       dg = ppar[jj] - pj ;   /* change in parameter */
       if( dg != 0.0f ){      /* update convergence test and residuals */
         pj    = fabsf(ppar[jj]) ;
         dsum += fabsf(dg) / MAX(pj,0.01f) ; ndel++ ;
         for( ii=0 ; ii < npt ; ii++ ) resd[ii] -= rj[ii] * dg ;
       }

     } /*-- end of inner loop over parameters --*/

     /**** test for convergence somehow ***/

     if( ndel > 0 ) dsum /= ndel ; /* average fractional change per parameter */

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
/* At some point in the future, this will be the Square Root LASSO:

   A Belloni, V Chernozhukov, and L Wang.
   Square-root LASSO: Pivotal recovery of sparse signals via conic programming.
   http://arxiv.org/abs/1009.5689
*//*--------------------------------------------------------------------------*/

floatvec * THD_sqrtlasso_L2fit( int npt    , float *far   ,
                                int nref   , float *ref[] ,
                                float *lam , float *ccon   )
{
}
