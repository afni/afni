/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "eispack.h"

/*--------------------------------------------------------------------------
   Compute the eigenvalue/vector decomposition of a symmetric matrix,
   stored in double precision.
     n = order of matrix
     a = on input: matrix(i,j) is in a[i+n*j] for i=0..n-1 , j=0..n-1
           output: a[i+n*j] has the i'th component of the j'th eigenvector
     e = on input: not used (but the calling program must
                             allocate the space for e[0..n-1])
           output: e[j] has the j'th eigenvalue, ordered so that
           e[0] <= e[1] <= ... <= e[n-1]
   Uses the f2c translation of EISPACK.
----------------------------------------------------------------------------*/

void symeig_double( int n , double *a , double *e )
{
   integer nm , matz , ierr ;
   double *fv1 , *fv2 ;

   if( a == NULL || e == NULL || n < 1 ) return ;

   if( n == 1 ){
     e[0] = a[0] ; a[0] = 1.0 ; return ;  /* degenerate case */
   }

   fv1 = (double *) malloc(sizeof(double)*n) ;  /* workspaces */
   fv2 = (double *) malloc(sizeof(double)*n) ;

   nm = n ; matz = 1 ; ierr = 0 ;

   rs_( &nm , &nm , a , e , &matz , a , fv1 , fv2 , &ierr ) ;

   free((void *)fv1) ; free((void *)fv2) ;
   return ;
}

/*------------------ just compute the eigenvalues -------------------*/

void symeigval_double( int n , double *a , double *e )
{
   integer nm , matz , ierr ;
   double *fv1 , *fv2 ;

   if( a == NULL || e == NULL || n < 1 ) return ;

   if( n == 1 ){ e[0] = a[0] ; return ; } /* degenerate case */

   fv1 = (double *) malloc(sizeof(double)*n) ;  /* workspaces */
   fv2 = (double *) malloc(sizeof(double)*n) ;

   nm = n ; matz = 0 ; ierr = 0 ;

   rs_( &nm , &nm , a , e , &matz , a , fv1 , fv2 , &ierr ) ;

   free((void *)fv1) ; free((void *)fv2) ;
   return ;
}

/*--------------------------------------------------------------------*/
/*! Compute SVD of double precision matrix:                      T
                                            [a] = [u] diag[s] [v]
    - m = # of rows in a
    - n = # of columns in a
    - a = pointer to input matrix; a[i+j*m] has the (i,j) element
    - s = pointer to output singular values; length = n
    - u = pointer to output matrix, if desired; length = m*n
    - v = pointer to output matrix, if desired; length = n*n

----------------------------------------------------------------------*/

void svd_double( int m , int n , double *a , double *s , double *u , double *v )
{
   integer mm,nn , lda,ldu,ldv , ierr ;
   doublereal *aa, *ww , *uu , *vv , *rv1 ;
   logical    matu , matv ;

   if( a == NULL || s == NULL || m < 1 || n < 1 ) return ;

   mm  = m ;
   nn  = n ;
   aa  = a ;
   lda = m ;
   ww  = s ;

   if( u == NULL ){
     matu = (logical) 0 ;
     uu   = (doublereal *)malloc(sizeof(double)*m*n) ;
   } else {
     matu = (logical) 1 ;
     uu = u ;
   }
   ldu = m ;

   if( v == NULL ){
     matv = (logical) 0 ;
     vv   = NULL ;
   } else {
     matv = (logical) 1 ;
     vv = v ;
   }
   ldv = n ;

   rv1 = (double *) malloc(sizeof(double)*n) ;  /* workspace */

   (void) svd_( &mm , &nn , &lda , aa , ww ,
                &matu , &ldu , uu , &matv , &ldv , vv , &ierr , rv1 ) ;

   free((void *)rv1) ;

   if( u == NULL ) free((void *)uu) ;
   return ;
}

#if 0
/*--------------------------------------------------------------------*/
/*! Compute SVD of single precision matrix:                      T
                                            [a] = [u] diag[s] [v]
    - m = # of rows in a
    - n = # of columns in a
    - a = pointer to input matrix; a[i+j*m] has the (i,j) element
    - s = pointer to output singular values; length = n
    - u = pointer to output matrix, if desired; length = m*n
    - v = pointer to output matrix, if desired; length = n*n

----------------------------------------------------------------------*/

void svd_float( int m , int n , float *a , float *s , float *u , float *v )
{
   integer mm,nn , lda,ldu,ldv , ierr ;
   doublereal *aa, *ww , *uu , *vv , *rv1 ;
   logical    matu , matv ;

   int ii ;

   if( a == NULL || s == NULL || m < 1 || n < 1 ) return ;

   mm  = m ;
   nn  = n ;
   aa  = (doublereal *)malloc(sizeof(double)*m*n) ;
   lda = m ;
   ww  = (doublereal *)malloc(sizeof(double)*n) ;

   for( ii=0 ; ii < m*n ; ii++ ) aa[ii] = (doublereal)a[ii] ;

   matu = (logical) 1 ;
   uu   = (doublereal *)malloc(sizeof(double)*m*n) ;
   ldu  = m ;

   matv = (logical) 1 ;
   vv   = (doublereal *)malloc(sizeof(double)*n*n) ;
   ldv  = n ;

   rv1 = (double *) malloc(sizeof(double)*n) ;  /* workspace */

   (void) svd_( &mm , &nn , &lda , aa , ww ,
                &matu , &ldu , uu , &matv , &ldv , vv , &ierr , rv1 ) ;

   free((void *)rv1) ; free((void *)aa) ;

   /* copy results out */

   for( ii=0 ; ii < n ; ii++ ) s[ii] = (float)ww[ii] ;

   if( u != NULL )
     for( ii=0 ; ii < m*n ; ii++ ) u[ii] = (float)uu[ii] ;

   if( v != NULL )
     for( ii=0 ; ii < n*n ; ii++ ) v[ii] = (float)vv[ii] ;

   free((void *)uu) ; free((void *)vv) ;
   return ;
}
#endif
