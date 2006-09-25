#include "mrilib.h"

#ifdef SOLARIS
# define sqrtf sqrt
# define logf  log
#endif

#if defined(SOLARIS) || defined(SGI)
# define cbrtf  cbrt
#endif


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

/*--------------------------------------------------------------------------*/
/*! Spearman rank-order correlation of x[] and y[] (nondestructive).  */

float THD_spearman_corr_nd( int n , float *x , float *y )
{
   float *qx, *qy , cv=0.0f ;
   qx = (float *)malloc(sizeof(float)*n); memcpy(qx,x,sizeof(float)*n);
   qy = (float *)malloc(sizeof(float)*n); memcpy(qy,y,sizeof(float)*n);
   cv = THD_spearman_corr(n,qx,qy) ;
   free((void *)qy); free((void *)qx);
   return cv ;
}

/*--------------------------------------------------------------*/
/*! Quadrant correlation of x[] and y[] (x and y are modified). */

float THD_quadrant_corr( int n , float *x , float *y )
{
   float xv = quadrant_corr_prepare(n,x) ;
   if( xv <= 0.0 ) return 0.0 ;
   return quadrant_corr( n,y,xv,x ) ;
}

/*--------------------------------------------------------------------------*/
/*! Quadrant rank-order correlation of x[] and y[] (nondestructive).  */

float THD_quadrant_corr_nd( int n , float *x , float *y )
{
   float *qx, *qy , cv=0.0f ;
   qx = (float *)malloc(sizeof(float)*n); memcpy(qx,x,sizeof(float)*n);
   qy = (float *)malloc(sizeof(float)*n); memcpy(qy,y,sizeof(float)*n);
   cv = THD_quadrant_corr(n,qx,qy) ;
   free((void *)qy); free((void *)qx);
   return cv ;
}

/*----------------------------------------------------------------*/
/*! Pearson correlation of x[] and y[] (x and y are NOT modified. */

float THD_pearson_corr( int n, float *x , float *y )
{
   float xv=0.0f , yv=0.0f , xy=0.0f ;
   int ii ;

   for( ii=0 ; ii < n ; ii++ ){
     xv += x[ii]*x[ii] ; yv += y[ii]*y[ii] ; xy += x[ii]*y[ii] ;
   }

   if( xv <= 0.0f || yv <= 0.0f ) return 0.0f ;
   return xy/sqrtf(xv*yv) ;
}

/*----------------------------------------------------------------*/

float THD_pearson_corr_wt( int n, float *x , float *y , float *wt )
{
   float xv=0.0f , yv=0.0f , xy=0.0f ;
   int ii ;

   if( wt == NULL ) return THD_pearson_corr(n,x,y) ;

   for( ii=0 ; ii < n ; ii++ ){
     xv += wt[ii]*x[ii]*x[ii] ; yv += wt[ii]*y[ii]*y[ii] ;
     xy += wt[ii]*x[ii]*y[ii] ;
   }

   if( xv <= 0.0f || yv <= 0.0f ) return 0.0f ;
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

/****************************************************************************/
/*** Histogram-based measurements of dependence between two float arrays. ***/
/*--------------------------------------------------------------------------*/
#define LINHIST                           /* do linear spread in histogram  */
#undef  WW
#define WW(i) ((w==NULL) ? 1.0f : w[i])   /* weight function for i'th datum */

static int n_old=-1 , nbin_old=255 ;
static float *xc=NULL , *yc=NULL , *xyc=NULL , nww=0.0f ;
static int nbin , nbp ;

#undef  XYC
#define XYC(p,q) xyc[(p)+(q)*nbp]

/*--------------------------------------------------------------------------*/
/*! Build 2D histogram of x[0..n-1] and y[0..n-1], each point optionally
    weighted by w[0..n-1].
    Used in the histogram-based measures of dependence between x[] and y[i].
    If something is bad on input, nbin is set to 0.  Otherwise, these global
    variables are set:
      - nbin = # of bins
      - nbp  = nbin+1
      - nww  = sum of the weights (will be n if w==NULL)
      - xc   = marginal histogram of x[], for xc[0..nbin]
      - yc   = marginal histogram of y[], for yc[0..nbin]
      - xyc  = joint histogram of (x[],y[]), for XYC(0..nbin,0..nbin)
----------------------------------------------------------------------------*/

static void build_2Dhist( int n , float xbot,float xtop,float *x ,
                                  float ybot,float ytop,float *y , float *w )
{
   register int ii,jj,kk ;
   float xb,xi , yb,yi , xx,yy , x1,y1 , nbb , val , ww ;

   if( n <= 1 || x == NULL || y == NULL ){     /* clear the arrays */
     if( xc  != NULL ){ free((void *)xc ); xc  = NULL; }
     if( yc  != NULL ){ free((void *)yc ); yc  = NULL; }
     if( xyc != NULL ){ free((void *)xyc); xyc = NULL; }
     nbin = 0 ; return ;
   }

   if( xbot >= xtop ){
     xbot = ybot = x[0] ;
     for( ii=0 ; ii < n ; ii++ )
            if( x[ii] > xtop ) xtop = x[ii] ;
       else if( x[ii] < xbot ) xbot = x[ii] ;
     if( xbot >= xtop ){ nbin = 0 ; return ; }
   }

   if( ybot >= ytop ){
     ybot = ybot = y[0] ;
     for( ii=0 ; ii < n ; ii++ )
            if( y[ii] > ytop ) ytop = y[ii] ;
       else if( y[ii] < ybot ) ybot = y[ii] ;
     if( ybot >= ytop ){ nbin = 0 ; return ; }
   }

   if( n == n_old ){               /* can keep old arrays */
     nbin = nbin_old ;
   } else {                        /* need null arrays */
     nbin = (int)cbrtf((float)n);
     if( nbin > 255 ) nbin = 255; else if( nbin < 3 ) nbin = 3;
     nbin_old = nbin ; n_old = n ;
     if( xc  != NULL ){ free((void *)xc ); xc  = NULL; }
     if( yc  != NULL ){ free((void *)yc ); yc  = NULL; }
     if( xyc != NULL ){ free((void *)xyc); xyc = NULL; }
   }
   nbp = nbin+1 ; nbb = nbin-0.001f ;

   if( xc  == NULL ) xc  = (float *)malloc(sizeof(float)*nbp) ;
   if( yc  == NULL ) yc  = (float *)malloc(sizeof(float)*nbp) ;
   if( xyc == NULL ) xyc = (float *)malloc(sizeof(float)*nbp*nbp) ;

   memset( xc  , 0 , sizeof(float)*nbp     ) ;
   memset( yc  , 0 , sizeof(float)*nbp     ) ;
   memset( xyc , 0 , sizeof(float)*nbp*nbp ) ;

   xb = xbot ; xi = nbb/(xtop-xbot) ;
   yb = ybot ; yi = nbb/(ytop-xbot) ; nww = 0.0f ;
   for( ii=0 ; ii < n ; ii++ ){
     xx = (x[ii]-xb)*xi ;
     if( xx < 0.0f ) xx = 0.0f ; else if( xx > nbb ) xx = nbb ;
     jj = (int)xx ; xx = xx - jj ; x1 = 1.0f-xx ;
     yy = (y[ii]-yb)*yi ;
     if( yy < 0.0f ) yy = 0.0f ; else if( yy > nbb ) yy = nbb ;
     kk = (int)yy ; yy = yy - kk ; y1 = 1.0f-yy ;
     ww = WW(ii) ; nww += ww ;

#ifdef LINHIST
     xc[jj] +=  x1*ww ; xc[jj+1] +=  xx*ww ;
     yc[kk] += (y1*ww); yc[kk+1] += (yy*ww);

     XYC(jj  ,kk  ) += x1*(y1*ww) ;
     XYC(jj+1,kk  ) += xx*(y1*ww) ;
     XYC(jj  ,kk+1) += x1*(yy*ww) ;
     XYC(jj+1,kk+1) += xx*(yy*ww) ;
#else
     xc[jj] += ww ; yc[kk] += ww ; XYC(jj,kk) += ww ;
#endif
   }

   return ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the mutual info between two vectors, sort of.  [16 Aug 2006]
----------------------------------------------------------------------------*/

float THD_mutual_info_scl( int n , float xbot,float xtop,float *x ,
                                   float ybot,float ytop,float *y , float *w )
{
   register int ii,jj ;
   register float val ;

   /*-- build 2D histogram --*/

   build_2Dhist( n,xbot,xtop,x,ybot,ytop,y,w ) ;
   if( nbin <= 0 || nww <= 0 ) return 0.0f ;  /* something bad happened! */

   /*-- compute MI from histogram --*/

   val = 0.0f ;
   for( ii=0 ; ii < nbp ; ii++ ){
    for( jj=0 ; jj < nbp ; jj++ ){
     if( XYC(ii,jj) > 0.0f )
      val += XYC(ii,jj) * logf( nww*XYC(ii,jj)/(xc[ii]*yc[jj]) ) ;
   }}
   return (1.4427f*val/nww) ;  /* units are bits, just for fun */
}

/*--------------------------------------------------------------------------*/
/*! Compute MI of x[] and y[i], with autoscaling. */

float THD_mutual_info( int n , float *x , float *y )
{
   return THD_mutual_info_scl( n, 1.0f,-1.0f, x, 1.0f,-1.0f, y, NULL ) ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the normalized mutual info between two vectors, sort of.
    Actually, returns H(x,y) / [ H(x)+H(y) ], which should be small if
    x and y are redundant and should be large if they are independent.
----------------------------------------------------------------------------*/

float THD_norm_mutinf_scl( int n , float xbot,float xtop,float *x ,
                                   float ybot,float ytop,float *y , float *w )
{
   register int ii,jj ;
   register float numer , denom ;

   /*-- build 2D histogram --*/

   build_2Dhist( n,xbot,xtop,x,ybot,ytop,y,w ) ;
   if( nbin <= 0 || nww <= 0 ) return 0.0f ;  /* something bad happened! */

   /*-- compute NMI from histogram --*/

   denom = numer = 0.0f ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0f ) denom += xc[ii] * logf( xc[ii] / nww ) ;
     if( yc[ii] > 0.0f ) denom += yc[ii] * logf( yc[ii] / nww ) ;
     for( jj=0 ; jj < nbp ; jj++ ){
       if( XYC(ii,jj) > 0.0f ) numer += XYC(ii,jj) * logf( XYC(ii,jj) / nww );
     }
   }
   if( denom != 0.0f ) denom = numer / denom ;
   return denom ;
}

/*--------------------------------------------------------------------------*/
/*! Compute NMI of x[] & y[i], with autoscaling: cf. THD_norm_mutinf_scl(). */

float THD_norm_mutinf( int n , float *x , float *y )
{
   return THD_norm_mutinf_scl( n, 1.0f,-1.0f, x, 1.0f,-1.0f, y, NULL ) ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the joint entropy between two vectors, sort of.
----------------------------------------------------------------------------*/

float THD_jointentrop_scl( int n , float xbot,float xtop,float *x ,
                                   float ybot,float ytop,float *y , float *w )
{
   register int ii,jj ;
   register float val ;

   /*-- build 2D histogram --*/

   build_2Dhist( n,xbot,xtop,x,ybot,ytop,y,w ) ;
   if( nbin <= 0 || nww <= 0 ) return 0.0f ;  /* something bad happened! */

   /*-- compute MI from histogram --*/

   val = 0.0f ;
   for( ii=0 ; ii < nbp ; ii++ ){
    for( jj=0 ; jj < nbp ; jj++ ){
     if( XYC(ii,jj) > 0.0f )
      val -= XYC(ii,jj) * logf( XYC(ii,jj)/nww ) ;
   }}
   return (1.4427f*val/nww) ;  /* units are bits, just for fun */
}

/*--------------------------------------------------------------------------*/
/*! Compute MI of x[] and y[i], with autoscaling. */

float THD_jointentrop( int n , float *x , float *y )
{
   return THD_mutual_info_scl( n, 1.0f,-1.0f, x, 1.0f,-1.0f, y, NULL ) ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the correlation ratio between two vectors, sort of.  [23 Aug 2006]
----------------------------------------------------------------------------*/

float THD_corr_ratio_scl( int n , float xbot,float xtop,float *x ,
                                  float ybot,float ytop,float *y , float *w )
{
   register int ii,jj ;
   register float vv,mm ;
   float    val , cyvar , uyvar ;

   /*-- build 2D histogram --*/

   build_2Dhist( n,xbot,xtop,x,ybot,ytop,y,w ) ;
   if( nbin <= 0 ) return 0.0f ;  /* something bad happened! */

   /*-- compute CR from histogram --*/

   cyvar = 0.0f ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0f ){
       vv = mm = 0.0f ;               /* mm=E(y|x)  vv=E(y^2|x) */
       for( jj=0 ; jj < nbp ; jj++ ){
         mm += (jj * XYC(ii,jj)) ; vv += jj * (jj * XYC(ii,jj)) ;
       }
       cyvar += (vv - mm*mm/xc[ii] ) ;
     }
   }
   vv = mm = uyvar = 0.0f ;
   for( jj=0 ; jj < nbp ; jj++ ){
     mm += (jj * yc[jj]) ; vv += jj * (jj * yc[jj]) ;
   }
   uyvar = vv - mm*mm/nww ;
   val = 1.0f - cyvar/uyvar ; return val ;
}

/*--------------------------------------------------------------------------*/
/*! Compute CR of x[] and y[], using autoscaling. */

float THD_corr_ratio( int n , float *x , float *y )
{
   return THD_corr_ratio_scl( n , 1.0f,-1.0f , x, 1.0f,-1.0f , y , NULL ) ;
}
