#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "eispack.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*--------------------------------------------------------------------------
   Compute the eigenvalue/vector decomposition of a symmetric matrix.
     n = order of matrix
     a = on input: matrix(i,j) is in a[i+n*j] for i=0..n-1 , j=0..n-1
           output: a[i+n*j] has the i'th component of the j'th eigenvector
     e =   output: e[j] has the j'th eigenvalue, ordered so that
           e[0] <= e[1] <= ... <= e[n-1]
   Uses the f2c translation of EISPACK.
----------------------------------------------------------------------------*/

void symeig_double( int n , double * a , double * e )
{
   integer nm , matz , ierr ;
   double * fv1 , * fv2 ;

   if( a == NULL || e == NULL || n < 1 ) return ;

   if( n == 1 ){
      e[0] = a[0] ; a[0] = 1.0 ; return ;  /* degenerate case */
   }

   fv1 = (double *) malloc(sizeof(double)*n) ;  /* workspaces */
   fv2 = (double *) malloc(sizeof(double)*n) ;

   nm = n ; matz = 1 ; ierr = 0 ;

   rs_( &nm , &nm , a , e , &matz , a , fv1 , fv2 , &ierr ) ;

   free(fv1) ; free(fv2) ;
   return ;
}
