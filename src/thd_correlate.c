#include "mrilib.h"

/*==============================================================================*/
/*========== The following functions were moved from afni_fimfunc.c -===========*/
/*==============================================================================*/

/*------------------------------------------------------------------------------*/
/*! Rank-order a float array, with ties getting the average rank.
   The output overwrites the input.
--------------------------------------------------------------------------------*/

void rank_order_float( int n , float *a )
{
   register int ii , ns , n1 , ib ;
   static int   nb = 0 ;
   static int   *b = NULL ;  /* workspaces */
   static float *c = NULL ;
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

/*---------------------------------------------------------------------------*/
/*! Rank orders a[], subtracts the mean rank, and returns the sum-of-squares.
-----------------------------------------------------------------------------*/

float spearman_rank_prepare( int n , float *a )
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

/*---------------------------------------------------------------------------*/
/*! Prepare for quadrant correlation with a[].
-----------------------------------------------------------------------------*/

float quadrant_corr_prepare( int n , float *a )
{
   register int ii ;
   register float rb , rs ;

   rank_order_float( n , a ) ;

   rb = 0.5*(n-1) ; rs=0.0 ;
   for( ii=0 ; ii < n ; ii++ ){
     a[ii] = (a[ii] > rb) ? 1.0
                          : (a[ii] < rb) ? -1.0 : 0.0 ;
     rs   += a[ii]*a[ii] ;
   }

   return rs ;
}

/*-----------------------------------------------------------------------------*/
/*! To Spearman (rank-order) correlate x[] with r[], first do
      rv = spearman_rank_prepare(n,r) ;
    then
      corr = spearman_rank_corr(n,x,rv,r) ;
    Note that these 2 routines are destructive (r and x are replaced by ranks).
-------------------------------------------------------------------------------*/

float spearman_rank_corr( int n , float *x , float rv , float *r )
{
   register int ii ;
   register float ss ; float xv ;

   xv = spearman_rank_prepare( n , x ) ; if( xv <= 0.0 ) return 0.0 ;

   for( ii=0,ss=0.0 ; ii < n ; ii++ ) ss += x[ii] * r[ii] ;

   return ( ss/sqrtf(rv*xv) ) ;
}

/*------------------------------------------------------------------------------*/
/*! To do quadrant correlation of x[] with r[], first do
      rv = quadrant_corr_prepare(n,r) ;
    then
      corr = quadrant_corr(n,x,rv,r) ;
    Note that these 2 routines are destructive (r and x are modified).
-------------------------------------------------------------------------------*/

float quadrant_corr( int n , float *x , float rv , float *r )
{
   register int ii ;
   register float ss ; float xv ;

   xv = quadrant_corr_prepare( n , x ) ; if( xv <= 0.0 ) return 0.0 ;

   for( ii=0,ss=0.0 ; ii < n ; ii++ ) ss += x[ii] * r[ii] ;

   return ( ss/sqrtf(rv*xv) ) ;
}

/*=============================================================================
  Compute correlations, destructively (i.e., mangling the input arrays)
===============================================================================*/

/*--------------------------------------------------------------------------*/
/*! Spearman rank-order correlation of x[] and y[] (x and y are modified).  */

float THD_spearman_corr( int n , float *x , float *y )
{
   float xv = spearman_rank_prepare(n,x) ;
   if( xv <= 0.0 ) return 0.0 ;
   return spearman_rank_corr( n,y,xv,x ) ;
}

/*--------------------------------------------------------------*/
/*! Quadrant correlation of x[] and y[] (x and y are modified). */

float THD_quadrant_corr( int n , float *x , float *y )
{
   float xv = quadrant_corr_prepare(n,x) ;
   if( xv <= 0.0 ) return 0.0 ;
   return quadrant_corr( n,y,xv,x ) ;
}

/*----------------------------------------------------------------*/
/*! Pearson correlation of x[] and y[] (x and y are NOT modified. */

float THD_pearson_corr( int n, float *x , float *y )
{
   float xv=0 , yv=0 , xy=0 ;
   int ii ;

   for( ii=0 ; ii < n ; ii++ ){
     xv += x[ii]*x[ii] ; yv += y[ii]*y[ii] ; xy += x[ii]*y[ii] ;
   }

   if( xv <= 0.0 || yv <= 0.0 ) return 0.0 ;
   return xy/sqrtf(xv*yv) ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the rank-order correlation between 2 images [08 Mar 2006].
----------------------------------------------------------------------------*/

float mri_spearman_corr( MRI_IMAGE *im , MRI_IMAGE *jm )
{
   float *far , *gar , cc ;
   MRI_IMAGE *fim , *gim ;

   if( im == NULL || jm == NULL || im->nvox != jm->nvox ) return 0.0f ;

   fim = mri_to_float(im) ; far = mri_data_pointer(fim) ;
   gim = mri_to_float(jm) ; gar = mri_data_pointer(gim) ;
   cc  = THD_spearman_corr( fim->nvox , far , gar ) ;
   mri_free(gim) ; mri_free(fim) ; return cc ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the mutual info between two vectors, sort of.  [16 Aug 2006]
----------------------------------------------------------------------------*/

float THD_mutual_info( int n , float xbot,float xtop,float *x ,
                               float ybot,float ytop,float *y  )
{
   int nbin,nbp , ii,jj,kk ;
   float xb,xi , yb,yi , xx,yy , x1,y1 , nbm , val ;
   static int n_old=-1 , nbin_old=255 ;
   static float *xc=NULL , *yc=NULL , *xyc=NULL ;

   if( n <= 1 || x == NULL || y == NULL ){
     if( xc  != NULL ){ free((void *)xc ); xc  = NULL; }
     if( yc  != NULL ){ free((void *)yc ); yc  = NULL; }
     if( xyc != NULL ){ free((void *)xyc); xyc = NULL; }
   }

   if( xbot >= xtop ){
     xbot = ybot = x[0] ;
     for( ii=0 ; ii < n ; ii++ )
            if( x[ii] > xtop ) xtop = x[ii] ;
       else if( x[ii] < xbot ) xbot = x[ii] ;
     if( xbot >= xtop ) return 0.0f ;
   }

   if( ybot >= ytop ){
     ybot = ybot = y[0] ;
     for( ii=0 ; ii < n ; ii++ )
            if( y[ii] > ytop ) ytop = y[ii] ;
       else if( y[ii] < ybot ) ybot = y[ii] ;
     if( ybot >= ytop ) return 0.0f ;
   }

   if( n = n_old ){
     nbin = nbin_old ;
   } else {
     nbin = (int)sqrtf((float)n); if( nbin > 255 ) nbin = 255 ;
     nbin_old = nbin ; n_old = n ;
     if( xc  != NULL ){ free((void *)xc ); xc  = NULL; }
     if( yc  != NULL ){ free((void *)yc ); yc  = NULL; }
     if( xyc != NULL ){ free((void *)xyc); xyc = NULL; }
   }
   nbp = nbin+1 ; nbm = nbin-1 ;

   if( xc  == NULL ) xc  = (float *)malloc(sizeof(float)*nbp) ;
   if( yc  == NULL ) yc  = (float *)malloc(sizeof(float)*nbp) ;
   if( xyc == NULL ) xyc = (float *)malloc(sizeof(float)*nbp*nbp) ;

#undef  XYC
#define XYC(p,q) xyc[(p)+(q)*nbp]

   memset( xc  , 0 , sizeof(float)*nbin ) ;
   memset( yc  , 0 , sizeof(float)*nbin ) ;
   memset( xyc , 0 , sizeof(float)*nbin*nbin ) ;

   xb = xbot ; xi = (nbin-0.0001)/(xtop-xbot) ;
   yb = ybot ; yi = (nbin-0.0001)/(ytop-xbot) ;
   for( ii=0 ; ii < n ; ii++ ){
     xx = (x[ii]-xb)*xi ;
     if( xx < 0.0f ) xx = 0.0f ; else if( xx > nbm ) xx = nbm ;
     jj = (int)xx ; xx = xx - jj ; x1 = 1.0f-xx ;
     yy = (y[ii]-yb)*yi ;
     if( yy < 0.0f ) yy = 0.0f ; else if( yy > nbm ) yy = nbm ;
     kk = (int)yy ; yy = yy - kk ; y1 = 1.0f-yy ;

     xc[jj] += x1 ; xc[jj+1] += xx ;
     yc[kk] += y1 ; yc[kk+1] += yy ;

     XYC(jj  ,kk  ) += x1*y1 ;
     XYC(jj+1,kk  ) += xx*y1 ;
     XYC(jj  ,kk+1) += x1*yy ;
     XYC(jj+1,kk+1) += xx*yy ;
   }

   val = 0.0f ;
   for( ii=0 ; ii < nbp ; ii++ ){
     for( jj=0 ; jj < nbp ; jj++ ){
       if( XYC(ii,jj) > 0.0f )
         val += XYC(ii,jj) * logf( XYC(ii,jj)/(xc[ii]*yc[jj]) ) ;
     }
   }
   return val ;
}
