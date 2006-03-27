#include "mrilib.h"

/*-----------------------------------------------------------------------*/
/*! Compute the product of two matrices, stored in 2D float images.
-------------------------------------------------------------------------*/

MRI_IMAGE * mri_matrix_mult( MRI_IMAGE *ima , MRI_IMAGE *imb )
{
   int nr , nc , mm ;
   MRI_IMAGE *imc ;
   float *amat , *bmat , *cmat , sum ;
   int ii,jj,kk ;

   if( ima == NULL            || imb == NULL            ) return NULL ;
   if( ima->kind != MRI_float || imb->kind != MRI_float ) return NULL ;

   nr = ima->nx ; mm = ima->ny ; nc = imb->ny ; if( imb->nx != mm ) return NULL;

#undef  A
#undef  B
#undef  C
#define A(i,j) amat[(i)+(j)*nr]
#define B(i,j) bmat[(i)+(j)*mm]
#define C(i,j) cmat[(i)+(j)*nr]

   imc  = mri_new( nr , nc , MRI_float ) ;
   amat = MRI_FLOAT_PTR(ima); bmat = MRI_FLOAT_PTR(imb);
   cmat = MRI_FLOAT_PTR(imc);

   for( jj=0 ; jj < nc ; jj++ ){
     for( ii=0 ; ii < nr ; ii++ ){
       sum = 0.0f ;
       for( kk=0 ; kk < mm ; kk++ ) sum += A(ii,kk)*B(kk,jj) ;
       C(ii,jj) = sum ;
   }}

   return imc ;
}

/*-----------------------------------------------------------------------*/
static int force_svd = 0 ;
void mri_matrix_psinv_svd( int i ){ force_svd = i; }
/*-----------------------------------------------------------------------*/
/*! Compute the pseudo-inverse of a matrix stored in a 2D float image.
    If the input is mXn, the output is nXm.  wt[] is an optional array
    of positive weights, m of them.  The result can be used to solve
    the weighted least squares problem
      [imc] [b] = [v]
    where [b] is an n-vector and [v] is an m-vector, where m > n.
-------------------------------------------------------------------------*/

MRI_IMAGE * mri_matrix_psinv( MRI_IMAGE *imc , float *wt )
{
   float *rmat ;
   int m , n , ii,jj,kk ;
   double *amat , *umat , *vmat , *sval , *xfac , smax,del,ww ;
   MRI_IMAGE *imp=NULL ; float *pmat ;
   register double sum ;
   int do_svd= (force_svd || AFNI_yesenv("AFNI_PSINV_SVD")) ;

   if( imc == NULL || imc->kind != MRI_float ) return NULL ;
   m    = imc->nx ;
   n    = imc->ny ;
   rmat = MRI_FLOAT_PTR(imc) ;
   amat = (double *)calloc( sizeof(double),m*n ) ;  /* input matrix */
   xfac = (double *)calloc( sizeof(double),n   ) ;  /* column norms of [a] */

#undef  R
#undef  A
#undef  P
#undef  U
#undef  V
#define R(i,j) rmat[(i)+(j)*m]   /* i=0..m-1 , j=0..n-1 */
#define A(i,j) amat[(i)+(j)*m]   /* i=0..m-1 , j=0..n-1 */
#define P(i,j) pmat[(i)+(j)*n]   /* i=0..n-1 , j=0..m-1 */
#define U(i,j) umat[(i)+(j)*m]
#define V(i,j) vmat[(i)+(j)*n]

#undef  PSINV_EPS
#define PSINV_EPS 1.e-8

   /* copy input matrix into amat */

   for( ii=0 ; ii < m ; ii++ )
     for( jj=0 ; jj < n ; jj++ ) A(ii,jj) = R(ii,jj) ;

   /* weight rows? */

   if( wt != NULL ){
     for( ii=0 ; ii < m ; ii++ ){
       ww = wt[ii] ;
       if( ww > 0.0 ) for( jj=0 ; jj < n ; jj++ ) A(ii,jj) *= ww ;
     }
   }

   /* scale each column to have norm 1 */

   for( jj=0 ; jj < n ; jj++ ){
     sum = 0.0 ;
     for( ii=0 ; ii < m ; ii++ ) sum += A(ii,jj)*A(ii,jj) ;
     if( sum > 0.0 ) sum = 1.0/sqrt(sum) ; else do_svd = 1 ;
     xfac[jj] = sum ;
     for( ii=0 ; ii < m ; ii++ ) A(ii,jj) *= sum ;
   }

   /*** computations follow, via SVD or Choleski ***/

   vmat = (double *)calloc( sizeof(double),n*n );

   if( do_svd ) goto SVD_PLACE ;

   /*** Try the Choleski method first ***/

   for( ii=0 ; ii < n ; ii++ ){       /* form normal equations */
     for( jj=0 ; jj <= ii ; jj++ ){
       sum = 0.0 ;
       for( kk=0 ; kk < m ; kk++ ) sum += A(kk,ii) * A(kk,jj) ;
       V(ii,jj) = sum ;
     }
     V(ii,ii) += PSINV_EPS ;   /* note V(ii,ii)==1 before this */
   }

   /* Choleski factor V in place */

   for( ii=0 ; ii < n ; ii++ ){
     for( jj=0 ; jj < ii ; jj++ ){
       sum = V(ii,jj) ;
       for( kk=0 ; kk < jj ; kk++ ) sum -= V(ii,kk) * V(jj,kk) ;
       V(ii,jj) = sum / V(jj,jj) ;
     }
     sum = V(ii,ii) ;
     for( kk=0 ; kk < ii ; kk++ ) sum -= V(ii,kk) * V(ii,kk) ;
     if( sum <= 0.0 ){
       WARNING_message("Choleski fails in mri_matrix_psinv()!\n");
       do_svd = 1 ; goto SVD_PLACE ;
     }
     V(ii,ii) = sqrt(sum) ;
   }

   /* create pseudo-inverse from what's now in V */

   imp  = mri_new( n , m , MRI_float ) ;   /* recall that m > n */
   pmat = MRI_FLOAT_PTR(imp) ;

   sval = (double *)calloc( sizeof(double),n ) ; /* row #jj of A */

   for( jj=0 ; jj < m ; jj++ ){
     for( ii=0 ; ii < n ; ii++ ) sval[ii] = A(jj,ii) ; /* extract row */

     for( ii=0 ; ii < n ; ii++ ){  /* forward solve */
       sum = sval[ii] ;
       for( kk=0 ; kk < ii ; kk++ ) sum -= V(ii,kk) * sval[kk] ;
       sval[ii] = sum / V(ii,ii) ;
     }
     for( ii=n-1 ; ii >= 0 ; ii-- ){  /* backward solve */
       sum = sval[ii] ;
       for( kk=ii+1 ; kk < n ; kk++ ) sum -= V(kk,ii) * sval[kk] ;
       sval[ii] = sum / V(ii,ii) ;
     }

     for( ii=0 ; ii < n ; ii++ ) P(ii,jj) = (float)sval[ii] ;
   }
   free((void *)amat); free((void *)vmat); free((void *)sval);
   goto RESCALE_PLACE ;

  SVD_PLACE:

     umat = (double *)calloc( sizeof(double),m*n ); /* left singular vectors */
     vmat = (double *)calloc( sizeof(double),n*n ); /* right singular vectors */
     sval = (double *)calloc( sizeof(double),n   ); /* singular values */

     /* compute SVD of scaled matrix */

     svd_double( m , n , amat , sval , umat , vmat ) ;

     free((void *)amat) ;  /* done with this */

     /* find largest singular value */

     smax = sval[0] ;
     for( ii=1 ; ii < n ; ii++ ) if( sval[ii] > smax ) smax = sval[ii] ;

     if( smax <= 0.0 ){                        /* this is bad */
       ERROR_message("SVD fails in mri_matrix_psinv()!\n");
       free((void *)xfac); free((void *)sval);
       free((void *)vmat); free((void *)umat); return NULL;
     }

     for( ii=0 ; ii < n ; ii++ )
       if( sval[ii] < 0.0 ) sval[ii] = 0.0 ;  /* should not happen */

     /* "reciprocals" of singular values:  1/s is actually s/(s^2+del) */

     del = PSINV_EPS * smax*smax ;
     for( ii=0 ; ii < n ; ii++ )
       sval[ii] = sval[ii] / ( sval[ii]*sval[ii] + del ) ;

     /* create pseudo-inverse */

     imp  = mri_new( n , m , MRI_float ) ;   /* recall that m > n */
     pmat = MRI_FLOAT_PTR(imp) ;

     for( ii=0 ; ii < n ; ii++ ){
       for( jj=0 ; jj < m ; jj++ ){
         sum = 0.0 ;
         for( kk=0 ; kk < n ; kk++ ) sum += sval[kk] * V(ii,kk) * U(jj,kk) ;
         P(ii,jj) = (float)sum ;
       }
     }
     free((void *)sval); free((void *)vmat); free((void *)umat);

  RESCALE_PLACE:
   /** from either method, must now rescale rows from norming */

   for( ii=0 ; ii < n ; ii++ ){
     for( jj=0 ; jj < m ; jj++ ) P(ii,jj) *= xfac[ii] ;
   }
   free((void *)xfac);

   /* rescale cols for weight? */

   if( wt != NULL ){
     for( ii=0 ; ii < m ; ii++ ){
       ww = wt[ii] ;
       if( ww > 0.0 ) for( jj=0 ; jj < n ; jj++ ) P(jj,ii) *= ww ;
     }
   }

   return imp;
}
