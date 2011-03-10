#include "mrilib.h"

/*----------------------------------------------------------------------------*/

static float flam = 0.01f ;
void THD_lasso_fixlam( float x ){ if( x > 0.0f ) flam = x ; }

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

floatvec * THD_lasso_linearfit( int npt    , float *far   ,
                                int nref   , float *ref[] ,
                                float *lam , float *ccon   )
{
   int ii,jj, nfree ;
   float *mylam, *ppar, *resd, *rsq , *rj,pj,dg ;
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
     for( jj=0 ; jj < nref ; jj++ ) mylam[jj] = flam ;
     nfree = 0 ;
   } else if( nfree >= npt ){         /* too many un-penalized parameters */
     free(mylam) ; RETURN(NULL) ;
   }

   /* space for parameter iterates (initialized to zero) */

   ppar = (float *)calloc(sizeof(float),nref) ;  /* old params */
   resd = (float *)calloc(sizeof(float),npt ) ;  /* residuals  */
   rsq  = (float *)calloc(sizeof(float),nref) ;

   /* if any parameters are free (no L1 penalty),
      initialize them by un-penalized least squares */

   if( nfree > 0 ){
     float **qref , *qcon=NULL ;
     qref = (float **)malloc(sizeof(float *)*nfree) ;
     if( ccon != NULL )
       qcon = (float *)malloc(sizeof(float)*nfree) ;
     for( ii=jj=0 ; jj < nref ; jj++ ){
       if( mylam[jj] > 0.0f ) continue ;
       qref[ii] = ref[jj] ; if( qcon != NULL ) qcon[ii] = ccon[ii] ;
       ii++ ;
     }
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

   for( ii=0 ; ii < npt ; ii++ ) resd[ii] = far[ii] ;
   for( jj=0 ; jj < nref ; jj++ ){
     pj = ppar[jj] ; rj = ref[jj] ;
     if( pj != 0.0f ){
       for( ii=0 ; ii < npt ; ii++ ) resd[ii] -= rj[ii]*pj ;
     }
     pj = 0.0f ;
     for( ii=0 ; ii < npt ; ii++ ) pj += rj[ii]*rj[ii] ;
     if( pj == 0.0f ) pj = 1.0f ;
     rsq[jj] = 1.0f / pj ;
   }

   /*-- outer iteration loop --*/

#undef  CON
#define CON(j) (ccon != NULL && ppar[j]*ccon[j] < 0.0f)

   while(1){
     float dg,dp,dm ;

     /*- inner loop over parameters -*/

     for( jj=0 ; jj < nref ; jj++ ){

       rj = ref[jj] ; pj = ppar[jj] ;

       for( dg=ii=0 ; ii < npt ; ii++ ) dg -= resd[ii] * rj[ii] ;

       if( mylam[jj] == 0.0f ){   /* un-penalized parameter */

         ppar[jj] -= dg*rsq[jj] ; if( CON(jj) ) ppar[jj] = 0.0f ;

       } else {                   /* penalized parameter */

         dp = dg + mylam[jj] ;
         dm = dg - mylam[jj] ;
         if( ppar[jj] > 0.0f || (ppar[jj] == 0.0f && dp < 0.0f) ){
           ppar[jj] -= dp*rsq[jj] ;
           if( ppar[jj] < 0.0f || CON(jj) ) ppar[jj] = 0.0f ;
         } else if( ppar[jj] < 0.0f || (ppar[jj] == 0.0f && dm < 0.0f) ){
           ppar[jj] -= dm*rsq[jj] ;
           if( ppar[jj] > 0.0f || CON(jj) ) ppar[jj] = 0.0f ;
         }

       }

       dg = ppar[jj] - pj ;
       if( dg != 0.0f ){
         for( ii=0 ; ii < npt ; ii++ ) resd[ii] += rj[ii] * dg ;
       }

     } /*- end of loop over parameters -*/

     /**** test for convergence somehow ***/

   } /*-- end of outer iteration loop --*/

   /*--- loading up the truck and heading to Beverlee ---*/

   MAKE_floatvec(qfit,nref) ;
   memcpy( qfit->ar , ppar , sizeof(float)*nref ) ;

   /*-- toss out the trash and vamoose the ranch --*/

   free(rsq) ; free(resd) ; free(ppar) ; free(mylam) ;

   RETURN(qfit) ;
}
