#include "mrilib.h"

/*==============================================================================*/
/*========== The following functions are lifted from afni_fimfunc.c ============*/
/*------------------------------------------------------------------------------
   Rank-order a float array, with ties getting the average rank.
   The output overwrites the input.
--------------------------------------------------------------------------------*/

static void rank_order_float( int n , float * a )
{
   register int ii , ns , n1 , ib ;
   static int    nb = 0 ;
   static int *   b = NULL ;  /* workspaces */
   static float * c = NULL ;
   float cs ;

   /*- handle special cases -*/

   if( a == NULL ){
      if( b != NULL ){ free(b); free(c); b=NULL ; c=NULL; nb=0; }  /* free workspaces */
      return ;
   }

   if( n < 1 ) return ;                     /* meaningless input */
   if( n == 1 ){ a[0] = 0.0 ; return ; }    /* only one point!? */

   /*- make workspaces, if needed -*/

   if( nb < n ){
      if( b != NULL ){ free(b); free(c); }
      b  = (int   *) malloc(sizeof(int  )*n) ;
      c  = (float *) malloc(sizeof(float)*n) ;
      nb = n ;
   }

   for( ii=0 ; ii < n ; ii++ ) c[ii] = b[ii] = ii ;

   /*- sort input, carrying b along -*/

   qsort_floatint( n , a , b ) ;  /* see cs_sort_fi.c */

   /* compute ranks into c[] */

   n1 = n-1 ;
   for( ii=0 ; ii < n1 ; ii++ ){
      if( a[ii] == a[ii+1] ){                  /* handle ties */
         cs = 2*ii+1 ; ns = 2 ; ib=ii ; ii++ ;
         while( ii < n1 && a[ii] == a[ii+1] ){ ii++ ; ns++ ; cs += ii ; }
         for( cs/=ns ; ib <= ii ; ib++ ) c[ib] = cs ;
      }
   }

   for( ii=0 ; ii < n ; ii++ ) a[b[ii]] = c[ii] ;

   return ;
}

/*---------------------------------------------------------------------------
   Rank orders a[], subtracts the mean rank, and returns the sum-of-squares
-----------------------------------------------------------------------------*/

static float spearman_rank_prepare( int n , float * a )
{
   register int ii ;
   register float rb , rs ;

   rank_order_float( n , a ) ;

   rb = 0.5*(n-1) ; rs=0.0 ;
   for( ii=0 ; ii < n ; ii++ ){
      a[ii] -= rb ;
      rs    += a[ii]*a[ii] ;
   }

   return rs ;
}

/*------------------------------------------------------------------------------*/

static float quadrant_corr_prepare( int n , float * a )
{
   register int ii ;
   register float rb , rs ;

   rank_order_float( n , a ) ;

   rb = 0.5*(n-1) ; rs=0.0 ;
   for( ii=0 ; ii < n ; ii++ ){
      a[ii] = (a[ii] > rb) ? 1.0
                           : (a[ii] < rb) ? -1.0 : 0.0 ;
      rs    += a[ii]*a[ii] ;
   }

   return rs ;
}

/*-----------------------------------------------------------------------------
    To correlate x[] with r[], do
      rv = spearman_rank_prepare(n,r) ;
    then
      corr = spearman_rank_corr(n,x,rv,r) ;
    Note that these 2 routines are destructive (r and x are replaced by ranks)
-------------------------------------------------------------------------------*/

static float spearman_rank_corr( int n , float * x , float rv , float * r )
{
   register int ii ;
   register float ss ; float xv ;

   xv = spearman_rank_prepare( n , x ) ; if( xv <= 0.0 ) return 0.0 ;

   for( ii=0,ss=0.0 ; ii < n ; ii++ ) ss += x[ii] * r[ii] ;

   return ( ss/sqrt(rv*xv) ) ;
}

/*------------------------------------------------------------------------------*/

static float quadrant_corr( int n , float * x , float rv , float * r )
{
   register int ii ;
   register float ss ; float xv ;

   xv = quadrant_corr_prepare( n , x ) ; if( xv <= 0.0 ) return 0.0 ;

   for( ii=0,ss=0.0 ; ii < n ; ii++ ) ss += x[ii] * r[ii] ;

   return ( ss/sqrt(rv*xv) ) ;
}

/*=============================================================================
  Compute correlations, destructively
===============================================================================*/

float THD_spearman_corr( int n , float *x , float *y )
{
   float xv = spearman_rank_prepare(n,x) ;
   if( xv <= 0.0 ) return 0.0 ;
   return spearman_rank_corr( n,y,xv,x ) ;
}

float THD_quadrant_corr( int n , float *x , float *y )
{
   float xv = quadrant_corr_prepare(n,x) ;
   if( xv <= 0.0 ) return 0.0 ;
   return quadrant_corr( n,y,xv,x ) ;
}

float THD_pearson_corr( int n, float *x , float *y )
{
   float xv=0 , yv=0 , xy=0 ;
   int ii ;

   for( ii=0 ; ii < n ; ii++ ){
      xv += x[ii]*x[ii] ; yv += y[ii]*y[ii] ; xy += x[ii]*y[ii] ;
   }

   if( xv <= 0.0 || yv <= 0.0 ) return 0.0 ;
   return xy/sqrt(xv*yv) ;
}
