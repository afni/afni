#include "mrilib.h"

/*----------------------------------------------------------------------------*/

/** note that flam will always be positive (never 0) **/

static float flam = 0.01f ;
void THD_lasso_fixlam( float x ){ if( x > 0.0f ) flam = x ; }

static floatvec *vlam = NULL ;
void THD_lasso_setlamvec( int nref , float *lam )
{
   KILL_floatvec(vlam) ;
   if( nref > 0 && lam != NULL ){
     MAKE_floatvec(vlam,nref) ;
     memcpy(vlam->ar,lam,sizeof(float)*nref) ;
   }
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
   L1 penalty can force some coefficients to be exactl zero.  However,
   at most npt-1 values of lam[] can be zero!

   The return vector contains the nref coefficients.  If NULL is returned,
   then something bad bad bad occurred and you should hang your head.

   TT Wu and K Lange.
   Coordinate descent algorithms for LASSO penalized regression.
   Annals of Applied Statistics, 2: 224-244 (2008).
   http://arxiv.org/abs/0803.3876
*//*--------------------------------------------------------------------------*/

floatvec * THD_lasso_L2fit( int npt    , float *far   ,
                            int nref   , float *ref[] ,
                            float *lam , float *ccon   )
{
   int ii,jj, nfree,nite,nimax,ndel ;
   float *mylam, *ppar, *resd, *rsq, *rj, pj,dg,dsum ;
   floatvec *qfit ;

ENTRY("THD_lasso_linearfit") ;

   /* check inputs for stupid users */

   if( npt  <= 1 || far == NULL || nref <= 0 || ref == NULL ) RETURN(NULL) ;

   for( jj=0 ; jj < nref ; jj++ ) if( ref[jj] == NULL ) RETURN(NULL) ;

   /* construct a local copy of lam[], and edit it softly */

   mylam = (float *)malloc(sizeof(float)*nref) ;

   nfree = nref ;
   if( lam != NULL ){
     for( nfree=jj=0 ; jj < nref ; jj++ ){
       mylam[jj] = MAX(0.0f,lam[jj]) ; if( mylam[jj] == 0.0f ) nfree++ ;
     }
   }
   if( nfree == nref ){
     nfree = 0 ;
     if( vlam != NULL ){
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
     } else {
       for( jj=0 ; jj < nref ; jj++ ) mylam[jj] = flam ;
     }
   } else if( nfree >= npt ){         /* too many un-penalized parameters */
     free(mylam) ; RETURN(NULL) ;
   }

   /* space for parameter iterates (initialized to zero) */

   ppar = (float *)calloc(sizeof(float),nref) ;  /* old params */
   resd = (float *)calloc(sizeof(float),npt ) ;  /* residuals  */
   rsq  = (float *)calloc(sizeof(float),nref) ;

   /* if any parameters are free (no L1 penalty),
      initialize them by un-penalized least squares
      (implicitly assuming all other parameters are zero) */

   if( nfree > 0 ){
     float **qref , *qcon=NULL ; int nc ;
     qref = (float **)malloc(sizeof(float *)*nfree) ;
     if( ccon != NULL ) qcon = (float *)calloc(sizeof(float),nfree) ;
     for( nc=ii=jj=0 ; jj < nref ; jj++ ){
       if( mylam[jj] <= 0.0f ){              /* use this parameter */
         qref[ii] = ref[jj] ;
         if( ccon != NULL && ccon[ii] != 0 ){ qcon[ii] = ccon[ii]; nc++; }
         ii++ ;
       }
     }
     if( nc == 0 ){ free(qcon); qcon = NULL; }
     qfit = THD_fitter( npt , far , nfree , qref , 2 , qcon ) ;
     if( qfit != NULL ){
       for( ii=jj=0 ; jj < nref ; jj++ ){
         if( mylam[jj] <= 0.0f ) ppar[jj] = qfit->ar[ii++] ;
       }
       KILL_floatvec(qfit) ;
     }
     free(qref) ; if( qcon != NULL ) free(qcon) ;
   }

   /* initialize residuals */

   for( ii=0 ; ii < npt ; ii++ ) resd[ii] = far[ii] ;          /* data */
   for( jj=0 ; jj < nref ; jj++ ){  /* subtract off fit of each column */
     pj = ppar[jj] ; rj = ref[jj] ;
     if( pj != 0.0f ){
       for( ii=0 ; ii < npt ; ii++ ) resd[ii] -= rj[ii]*pj ;
     }
     for( pj=ii=0 ; ii < npt ; ii++ ) pj += rj[ii]*rj[ii] ;
     if( pj == 0.0f ) pj = 1.0f ;             /* column of all zeros!? */
     rsq[jj] = 1.0f / pj ;   /* save 1/(sum of squares) of each column */
   }

   /*---- outer iteration loop ----*/

#undef  CON
#define CON(j) (ccon != NULL && ppar[j]*ccon[j] < 0.0f)

   nimax = 3*nref + 66 ; dsum = 1.0f ;
   for( nite=0 ; nite < nimax && dsum > 0.0002f ; nite++ ){

     /*-- cyclic inner loop over parameters --*/

     for( dsum=ndel=jj=0 ; jj < nref ; jj++ ){  /* dsum = sum of param deltas */

       rj = ref[jj] ;   /* j-th reference column */
       pj = ppar[jj] ;  /* current value of j-th parameter */

       /* compute dg = -gradient of un-penalized function wrt ppar[jj] */

       for( dg=ii=0 ; ii < npt ; ii++ ) dg += resd[ii] * rj[ii] ;

       if( mylam[jj] == 0.0f ){   /* un-penalized parameter */

         ppar[jj] += dg*rsq[jj] ; if( CON(jj) ) ppar[jj] = 0.0f ;

       } else {                   /* penalized parameter */

         if( pj > 0.0f || (pj == 0.0f && dg > 0.0f) ){        /* on the + side */
           dg -= mylam[jj] ; ppar[jj] += dg*rsq[jj] ;         /* shrink - way */
           if( ppar[jj] < 0.0f || CON(jj) ) ppar[jj] = 0.0f ;
         } else if( pj < 0.0f || (pj == 0.0f && dg < 0.0f) ){ /* on the - side */
           dg += mylam[jj] ; ppar[jj] += dg*rsq[jj] ;         /* shrink + way */
           if( ppar[jj] > 0.0f || CON(jj) ) ppar[jj] = 0.0f ;
         }

       }

       dg = ppar[jj] - pj ;   /* change in parameter */
       if( dg != 0.0f ){
         pj    = fabsf(ppar[jj]) ; if( pj < 0.01f ) pj = 0.01f ;
         dsum += fabsf(dg) / pj ; ndel++ ;
         for( ii=0 ; ii < npt ; ii++ ) resd[ii] -= rj[ii] * dg ;
       }

     } /*-- end of loop over parameters --*/

     /**** test for convergence somehow ***/

     if( ndel > 0 ) dsum /= ndel ;

   } /*---- end of outer iteration loop ----*/

   { static int ncall=0 ;
     if( ncall == 0 ){
       INFO_message("LASSO: nite=%d dsum=%g",nite,dsum) ; ncall++ ;
     }
   }

   /*--- Loading up the truck and heading to Beverlee ---*/

   MAKE_floatvec(qfit,nref) ;
   memcpy( qfit->ar , ppar , sizeof(float)*nref ) ;

   free(rsq) ; free(resd) ; free(ppar) ; free(mylam) ;

   RETURN(qfit) ;
}
