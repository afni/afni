#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

#include "mrilib.h"

#if 0
typedef struct { float r , i ; } complex ;
#endif

#ifndef MAX
#define MAX(a,b) (((a)<(b)) ? (b) : (a))
#endif

#define NPOL 1
#define NFIT ((NPOL+1)*(NPOL+1))

void pfit( int nz , complex * z , int nfit , float * psi[] , float * fit ) ;

int main( int argc , char * argv[] )
{
   MRI_IMAGE * rim , * iim , * cxim ;
   complex * cxar ;
   int nz , ii,jj,kk,ll  , nx,ny ;
   float * psi[NFIT] ;
   float xx , yy ;
   float fit[NFIT] ;

   rim = mri_read( "re_3.14" ) ; iim = mri_read( "im_3.14" ) ;
   cxim = mri_pair_to_complex( rim , iim ) ;
   cxar = MRI_COMPLEX_PTR(cxim) ;
   nx = cxim->nx ; ny = cxim->ny ; nz = nx * ny ;
   mri_free( rim ) ; mri_free( iim ) ;

   for( ii=0 ; ii < NFIT ; ii++ )
      psi[ii] = (float *) malloc( sizeof(float) * nz ) ;

   for( jj=0 ; jj < ny ; jj++ ){
      yy = (jj - 0.5*ny)/(0.5*ny) ;
      for( ii=0 ; ii < nx ; ii++ ){
         xx = (ii - 0.5*nx)/(0.5*nx) ;
         for( ll=0 ; ll <= NPOL ; ll++ )
            for( kk=0 ; kk <= NPOL ; kk++ )
               psi[kk+ll*(NPOL+1)][ii+jj*nx] = pow(xx,kk) * pow(yy,ll) ;
      }
   }

   for( ii=0 ; ii < NFIT ; ii++ ) fit[ii] = 1.234 * ii ;

   pfit( nz,cxar , NFIT , psi , fit ) ;
   exit(0) ;
}

/*------------------------------------------------------------------------
   Fit the phase.

      nz    =  Number of complex input points.
      z     =  Array of nz complex numbers.
      nfit  =  Number of fit parameters.
      psi   =  Array of nfit pointers to float arrays of length nz.
      fit   =  Array of nfit float fit parameters.

   The phase of z[k] will be fit to the function

     phi[k] = sum( fit[k] * psi[j][k] , j=0..nfit-1 )

   On input, fit[] contains the initial estimates of the fitting
   parameters.  On output, it contains the final estimate.

   The method is to maximize the function

     E = sum( Abs(z[k])**2  * cos( phi[k] - Arg(z[k]) ) , k=0..nz-1 )

   using Newton's method.
--------------------------------------------------------------------------*/

void pfit( int nz , complex * z , int nfit , float * psi[] , float * fit )
{
   float * zsqr , * zarg ;
   double * dfit , * egrad , * ehess ;
   double phi , cph,sph , sum , delta ;
   float ztop ;
   int ii , jj,kk , nite ;

   /*** Compute Abs(z[k])**2 and Arg(z[k]) ***/

   zsqr = (float *) malloc( sizeof(float) * nz ) ;
   zarg = (float *) malloc( sizeof(float) * nz ) ;

   if( zsqr==NULL || zarg==NULL ){
      fprintf(stderr,"\nCan't malloc workspace in pfit\n") ;
      exit(1) ;
   }

   ztop = 0.0 ;
   for( ii=0 ; ii < nz ; ii++ ){
      zsqr[ii] = z[ii].r * z[ii].r + z[ii].i * z[ii].i ;
      zarg[ii] = (zsqr[ii] > 0.0) ? atan2(z[ii].i,z[ii].r) : 0.0 ;
      ztop     = MAX( ztop , zsqr[ii] ) ;
   }
   ztop = 1.0 / ztop ;
   for( ii=0 ; ii < nz ; ii++ ) zsqr[ii] *= ztop ;

   /*** Allocate space for Newton's method stuff ***/

   dfit  = (double *) malloc( sizeof(double) * nfit ) ;
   egrad = (double *) malloc( sizeof(double) * nfit ) ;
   ehess = (double *) malloc( sizeof(double) * nfit * nfit ) ;

   for( ii=0 ; ii < nfit ; ii++ ) dfit[ii] = fit[ii] ;  /* initialize */

   /*** Begin Newton iterations ***/

fprintf(stderr,"pfit starts with nz=%d\n",nz) ;

   nite = 0 ;
   do{

      /*** compute gradient and Hessian ***/

#define HH(k,j) ehess[(k)+(j)*nfit]

      /*** initialize to zero ***/

      for( jj=0 ; jj < nfit ; jj++ ){
         egrad[jj] = 0.0 ;
         for( kk=0 ; kk <= jj ; kk++ ) HH(jj,kk) = 0.0 ;
      }

      /*** sum them up over all points in z[];
           note that only the lower triangle of the Hessian
           is computed (HH(jj,kk) for jj >= kk), since
           the matrix is symmetric and that's all we'll need ***/

      for( ii=0 ; ii < nz ; ii++ ){

         phi = -zarg[ii] ;                   /* compute fitted phase */
         for( jj=0 ; jj < nfit ; jj++ )      /* error at z[ii]       */
            phi += dfit[jj] * psi[jj][ii] ;

         cph = cos(phi) * zsqr[ii] ;         /* some useful factors */
         sph = sin(phi) * zsqr[ii] ;

         for( jj=0 ; jj < nfit ; jj++ ){
            egrad[jj] += sph * psi[jj][ii] ; /* gradient */

            for( kk=0 ; kk <= jj ; kk++ )
               HH(jj,kk) += cph * psi[jj][ii] * psi[kk][ii] ; /* Hessian */
         }
      }

fprintf(stderr,"\nHessian:\n") ;
for(jj=0;jj<nfit;jj++){
   for(kk=0;kk<=jj;kk++) fprintf(stderr," %g",HH(jj,kk)) ;
   fprintf(stderr,"\n") ;
}

      sph = cph = 0.0 ;
      for( jj=0 ; jj < nfit ; jj++ ){
         cph = MIN( cph , HH(jj,jj) ) ;
         sph = MAX( sph , HH(jj,jj) ) ;
      }
      if( cph <= 0 ){
         cph = -cph + 0.01*fabs(sph) ;
         for( jj=0 ; jj < nfit ; jj++ ) HH(jj,jj) += cph ;
      }

      /*** Choleski decompose Hessian ***/

      for( ii=0 ; ii < nfit ; ii++ ){    /* in the ii-th row */
         for( jj=0 ; jj < ii ; jj++ ){   /* in the jj-th column */
            sum = HH(ii,jj) ;
            for( kk=0 ; kk < jj ; kk++ ) sum -= HH(ii,kk) * HH(jj,kk) ;
            HH(ii,jj) = sum / HH(jj,jj) ;
         }
         sum = HH(ii,ii) ;
         for( kk=0 ; kk < ii ; kk++ ) sum -= HH(ii,kk) * HH(ii,kk) ;
         if( sum <= 0.0 ){fprintf(stderr,"Choleski fails: row %d\n",ii);exit(1);}
         HH(ii,ii) = sqrt(sum) ;
      }

      /*** forward solve ***/

      for( ii=0 ; ii < nfit ; ii++ ){
         sum = egrad[ii] ;
         for( jj=0 ; jj < ii ; jj++ ) sum -= HH(ii,jj) * egrad[jj] ;
         egrad[ii] = sum / HH(ii,ii) ;
      }

      /*** backward solve ***/

      delta = 0.0 ;
      for( ii=nfit-1 ; ii >= 0 ; ii-- ){
         sum = egrad[ii] ;
         for( jj=ii+1 ; jj < nfit ; jj++ ) sum -= HH(jj,ii) * egrad[jj] ;
         egrad[ii] = sum / HH(ii,ii) ;
         dfit[ii] -= egrad[ii] ;       /* change in fit parameters */

         sum    = fabs(dfit[ii]) ;
         delta += fabs(egrad[ii]) / MAX(sum,1.0) ;
      }
      delta /= nfit ;

      /*** check if we are done ***/

      nite++ ;

fprintf(stderr,"\nIteration %d:\n",nite) ;
for( ii=0 ; ii < nfit ; ii++ )
   fprintf(stderr,"  delta[%d] = %g   dfit[%d] = %g\n",
           ii,egrad[ii] , ii,dfit[ii] ) ;

   } while( delta > 1.e-3 && nite < 9 ) ;

   /*** clean up mess and exit ***/

   free(zsqr) ; free(zarg) ;
   free(dfit) ; free(egrad) ; free(ehess) ;

   for( ii=0 ; ii < nfit ; ii++ ) fit[ii] = dfit[ii] ;  /* output */
   return ;
}
