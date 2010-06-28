#include "mrilib.h"

/*==============================================================================*/
/*========== The following functions were moved from afni_fimfunc.c -===========*/
/*===========================================================================*/

/*---------------------------------------------------------------------------*/
/*! Rank-order a float array, with ties getting the average rank.
   The output overwrites the input.
   [27 Jun 2010: modified to create/destroy workspace on each call]
*//*-------------------------------------------------------------------------*/

void rank_order_float( int n , float *a )
{
   register int ii , ns , n1 , ib ;
   int   *b ;  /* workspaces */
   float *c ;
   float cs ;

   /*- handle special cases -*/

   if( a == NULL || n < 1 ) return ;        /* meaningless input */
   if( n == 1 ){ a[0] = 0.0f ; return ; }    /* only one point!? */

   /*- make workspaces -*/

   b = (int   *) malloc(sizeof(int  )*n) ;
   c = (float *) malloc(sizeof(float)*n) ;

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

   free(c) ; free(b) ; return ;
}

/*---------------------------------------------------------------------------*/
/*! Rank orders a[], subtracts the mean rank, and returns the sum-of-squares.
-----------------------------------------------------------------------------*/

float spearman_rank_prepare( int n , float *a )
{
   register int ii ;
   register float rb , rs ;

   rank_order_float( n , a ) ;

   rb = 0.5f*(n-1) ; rs=0.0f ;
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

   rb = 0.5f*(n-1) ; rs=0.0f ;
   for( ii=0 ; ii < n ; ii++ ){
     a[ii] = (a[ii] > rb) ? 1.0
                          : (a[ii] < rb) ? -1.0f : 0.0f ;
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

   xv = spearman_rank_prepare( n , x ) ; if( xv <= 0.0f ) return 0.0f ;

   for( ii=0,ss=0.0f ; ii < n ; ii++ ) ss += x[ii] * r[ii] ;

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

   xv = quadrant_corr_prepare( n , x ) ; if( xv <= 0.0f ) return 0.0f ;

   for( ii=0,ss=0.0f ; ii < n ; ii++ ) ss += x[ii] * r[ii] ;

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
   if( xv <= 0.0f ) return 0.0f ;
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

/*--------------------------------------------------------------------------*/
/*! Kendall Tau_b (x and y are modified) */

float THD_ktaub_corr( int n , float *x , float *y )
{
   qsort_floatfloat( n , x , y ) ;    /* preliminary sorting of x, carrying y */
   return kendallNlogN( x , y , n ) ; /* the actual work */
}

/*--------------------------------------------------------------*/
/*! Quadrant correlation of x[] and y[] (x and y are modified). */

float THD_quadrant_corr( int n , float *x , float *y )
{
   float xv = quadrant_corr_prepare(n,x) ;
   if( xv <= 0.0f ) return 0.0f ;
   return quadrant_corr( n,y,xv,x ) ;
}

/*--------------------------------------------------------------------------*/
/*! Quadrant rank-order correlation of x[] and y[] (nondestructive).  */

#if 1
float THD_quadrant_corr_nd( int n , float *x , float *y )
{
   float *z ; register float xm,ym,qc ; register int ii ;

   z = (float *)malloc(sizeof(float)*n) ;
   memcpy( z , x , sizeof(float)*n ) ;
   xm = qmed_float( n , z ) ;
   z = (float *)malloc(sizeof(float)*n) ;
   memcpy( z , y , sizeof(float)*n ) ;
   ym = qmed_float( n , z ) ;
   free(z) ;

   qc = 0.0f ;
   for( ii=0 ; ii < n ; ii++ )
     qc += (x[ii] > xm) * (y[ii] > ym) ;
   qc = (4.0f*qc) / n - 1.0f ;
   if( qc < -1.0f ) qc = -1.0f; else if( qc > 1.0f ) qc = 1.0f;
   qc = sinf(1.570796f*qc) ;  /* adjust to normal model */
   return qc ;
}
#else
/*--------------------------------------------------------------------------*/
float THD_quadrant_corr_nd( int n , float *x , float *y )
{
   float *qx, *qy , cv=0.0f ;
   qx = (float *)malloc(sizeof(float)*n); memcpy(qx,x,sizeof(float)*n);
   qy = (float *)malloc(sizeof(float)*n); memcpy(qy,y,sizeof(float)*n);
   cv = THD_quadrant_corr(n,qx,qy) ;
   free((void *)qy); free((void *)qx);
   return cv ;
}
#endif

/*----------------------------------------------------------------*/
/*! Pearson correlation of x[] and y[] (x and y are NOT modified. */

float THD_pearson_corr( int n, float *x , float *y )
{
   float xv=0.0f , yv=0.0f , xy=0.0f , vv,ww ;
   float xm=0.0f , ym=0.0f ;
   int ii ;

   for( ii=0 ; ii < n ; ii++ ){ xm += x[ii] ; ym += y[ii] ; }
   xm /= n ; ym /= n ;
   for( ii=0 ; ii < n ; ii++ ){
     vv = x[ii]-xm ; ww = y[ii]-ym ;
     xv += vv*vv ; yv += ww*ww ; xy += vv*ww ;
   }

   if( xv <= 0.0f || yv <= 0.0f ) return 0.0f ;
   return xy/sqrtf(xv*yv) ;
}

/*----------------------------------------------------------------*/

float THD_pearson_corr_wt( int n, float *x , float *y , float *wt )
{
   float xv=0.0f , yv=0.0f , xy=0.0f , vv,ww ;
   float xm=0.0f , ym=0.0f , ws=0.0f ;
   int ii ;

   if( wt == NULL ) return THD_pearson_corr(n,x,y) ;

   for( ii=0 ; ii < n ; ii++ ){
     xm += wt[ii]*x[ii]; ym += wt[ii]*y[ii]; ws += wt[ii];
   }
   xm /= ws ; ym /= ws ;
   for( ii=0 ; ii < n ; ii++ ){
     vv = x[ii]-xm ; ww = y[ii]-ym ;
     xv += wt[ii]*vv*vv ; yv += wt[ii]*ww*ww ; xy += wt[ii]*vv*ww ;
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

/*----------------------------------------------------------------*/
/*! eta^2 (Cohen, NeuroImage 2008)              25 Jun 2010 [rickr]
 *
 *  eta^2 = 1 -  SUM[ (a_i - m_i)^2 + (b_i - m_i)^2 ]
 *               ------------------------------------
 *               SUM[ (a_i - M  )^2 + (b_i - M  )^2 ]
 *
 *  where  o  a_i and b_i are the vector elements
 *         o  m_i = (a_i + b_i)/2
 *         o  M = mean across both vectors
 -----------------------------------------------------------------*/
float THD_eta_squared( int n, float *x , float *y )
{
   double num=0.0f , denom = 0.0f ;
   float gm=0.0f , lm ;
   int ii ;

   for( ii=0 ; ii < n ; ii++ ){ gm += x[ii] + y[ii] ; }
   gm /= (2*n) ;

   for( ii=0 ; ii < n ; ii++ ){
     lm = 0.5 * ( x[ii] + y[ii] ) ;
     num   += (x[ii]-lm)*(x[ii]-lm) + (y[ii]-lm)*(y[ii]-lm) ;
     denom += (x[ii]-gm)*(x[ii]-gm) + (y[ii]-gm)*(y[ii]-gm) ;
   }

   if( num < 0.0f || denom <= 0.0f || num >= denom ) return 0.0f ;
   return 1.0 - num/denom ;
}

/****************************************************************************/
/*** Histogram-based measurements of dependence between two float arrays. ***/
/****************************************************************************/
/*--------------------------------------------------------------------------*/
/* Extensive changes from the olde method:
    * values below bot and above top are not used
    * histogram can have bot and top bins unequal, and in-between ones
      equal (if use_xyclip != 0)
    * histogram can have all unequal bin sizes (if nxybin > 2)
----------------------------------------------------------------------------*/
#define LINHIST                           /* do linear spread in histogram  */
#undef  WW
#define WW(i) ((w==NULL) ? 1.0f : w[i])   /* weight function for i'th datum */

static float *xc=NULL , *yc=NULL , *xyc=NULL , nww=0.0f ;
static int nbin=0 , nbp=0 ;

static float *xbin=NULL , *ybin=NULL ;
static int  nxybin=0 ;

static int   use_xyclip=0 ;
static float xclip_bot, xclip_top ;
static float yclip_bot, yclip_top ;

#undef  XYC
#define XYC(p,q) xyc[(p)+(q)*nbp]

#ifndef WAY_BIG
#  define WAY_BIG 1.e+10
#endif

#undef  GOODVAL
#define GOODVAL(x) ((x) < WAY_BIG)                 /* x is not preposterous */

#undef  RANGVAL
#define RANGVAL(x,b,t) ((x) >= (b) && (x) <= (t))  /* x between b and t */

#undef  FREEIF
#define FREEIF(x) \
  do{ if((x)!=NULL){ free((void *)(x)); (x)=NULL; } } while(0)

/*--------------------------------------------------------------------------*/
/*! Clear the internal 2D histogram (but not the bin settings).
----------------------------------------------------------------------------*/

void clear_2Dhist(void)
{
   FREEIF(xc); FREEIF(yc); FREEIF(xyc); nbin = nbp = 0; return;
}

/*--------------------------------------------------------------------------*/

static double hpow = 0.33333333333 ;
void set_2Dhist_hpower( double hh )
{
  hpow = (hh > 0.0 && hh < 1.0) ? hh
                                : 0.33333333333 ;
  clear_2Dhist() ;
}

/*--------------------------------------------------------------------------*/

static int nhbin = 0 ;
void set_2Dhist_hbin( int nn ){ nhbin = nn; clear_2Dhist(); }
int  get_2Dhist_hbin( void   ){ return nhbin ; }

/*--------------------------------------------------------------------------*/
/*! Retrieve the 2D histogram built previously in build_2Dhist().
    - Return value is the number of bins in each direction (may be 0).
    - *xyhist is points to internal 2D array (may be NULL).
----------------------------------------------------------------------------*/

int retrieve_2Dhist( float **xyhist )
{
   if( xyhist == NULL ) return 0 ;
   *xyhist = xyc ; return nbp ;
}

/*--------------------------------------------------------------------------*/

int retrieve_2Dhist1( float **xh , float **yh )
{
   if( xh == NULL || yh == NULL ) return 0 ;
   *xh = xc ; *yh = yc ; return nbp ;
}

/*--------------------------------------------------------------------------*/
/*! Set the bins to be used in 2D histogram-izing in build_2Dhist().
      - nb = number of bins; if nb < 3, equal sized bins will be used
        and the internal bin arrays will be cleared
      - xb = [0..nb] (nb+1 elements) = boundary pts for x-axis bins
      - yb = [0..nb] (nb+1 elements) = boundary pts for y-axis bins
      - Both xb[] and yb[] must be strictly increasing arrays!
      - Also see get_2Dhist_xybin()
----------------------------------------------------------------------------*/

void set_2Dhist_xybin( int nb , float *xb , float *yb )
{
   int ii ;
   FREEIF(xbin) ; FREEIF(ybin) ; nxybin = 0 ;
   if( nb > 2 && xb != NULL && yb != NULL ){
     for( ii=1 ; ii <= nb ; ii++ )  /* check that bin sizes are OK */
       if( xb[ii-1] >= xb[ii] || yb[ii-1] > yb[ii] ) break ;
     if( ii > nb ){
       nxybin = nb ;
       xbin   = (float *)malloc(sizeof(float)*(nb+1)) ;
       ybin   = (float *)malloc(sizeof(float)*(nb+1)) ;
       memcpy( xbin , xb , sizeof(float)*(nb+1) ) ;
       memcpy( ybin , yb , sizeof(float)*(nb+1) ) ;
     } else {
       WARNING_message("set_2Dhist_xybin: illegal inputs!") ;
     }
   }
   return ;
}

/*--------------------------------------------------------------------------*/

void set_2Dhist_xybin_eqwide( int nb, float xbot,float xtop,float ybot,float ytop )
{
   int ii ; float dx , dy ;

   FREEIF(xbin) ; FREEIF(ybin) ; nxybin = 0 ;
   if( nb > 2 && xbot < xtop && ybot < ytop ){
     nxybin = nb ;
     xbin   = (float *)malloc(sizeof(float)*(nb+1)) ;
     ybin   = (float *)malloc(sizeof(float)*(nb+1)) ;
     dx = (xtop-xbot) / nb ; dy = (ytop-ybot) / nb ;
     for( ii=0 ; ii < nb ; ii++ ){
       xbin[ii] = xbot + ii*dx ;
       ybin[ii] = ybot + ii*dy ;
     }
     xbin[nb] = xtop ; ybin[nb] = ytop ;
#if 0
     INFO_message("set_2Dhist_xybin_eqwide: %d %f..%f %f..%f",
                  nb,xbot,xtop,ybot,ytop ) ;
#endif
   }
   return ;
}

/*--------------------------------------------------------------------------*/

static float_pair clipate( int nval , float *xar )
{
   MRI_IMAGE *qim; float cbot,ctop, mmm , *qar; float_pair rr; int ii,nq;

ENTRY("clipate") ;

   qim = mri_new_vol( nval,1,1 , MRI_float ) ; qar = MRI_FLOAT_PTR(qim) ;
   for( ii=nq=0 ; ii < nval ; ii++ ) if( GOODVAL(xar[ii]) ) qar[nq++] = xar[ii];
   qim->nx = qim->nvox = nq ;
   if( nq < 666 ){ rr.a = 1.0f; rr.b = 0.0f; mri_free(qim); RETURN(rr); }
   mmm  = mri_min( qim ) ;
   if( mmm >= 0.0f ){   /* for positive images */
     cbot = THD_cliplevel( qim , 0.345f ) ;
     ctop = mri_quantile ( qim , 0.966f ) ;
     if( ctop > 4.321f*cbot ) ctop = 4.321f*cbot ;
   } else {  /* for images including negative values: no go */
     cbot = 1.0f; ctop = 0.0f;
   }
   mri_free(qim) ;
   rr.a = cbot ; rr.b = ctop ; RETURN(rr) ;
}

/*--------------------------------------------------------------------------*/

void set_2Dhist_xyclip( int nval , float *xval , float *yval )
{
   float_pair xcc , ycc ;

ENTRY("set_2Dhist_xyclip") ;

   use_xyclip = 0 ;
   if( nval < 666 || xval == NULL || yval == NULL ) EXRETURN ;

   xcc = clipate( nval , xval ) ;
   ycc = clipate( nval , yval ) ;

   if( xcc.a >= xcc.b || ycc.a >= ycc.b ) EXRETURN ;

   use_xyclip = 1 ; nxybin = 0 ;
   xclip_bot  = xcc.a ; xclip_top = xcc.b ;
   yclip_bot  = ycc.a ; yclip_top = ycc.b ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

int get_2Dhist_xyclip( float *xbc , float *xtc , float *ybc , float *ytc )
{
   *xbc = xclip_bot ; *xtc = xclip_top ;
   *ybc = yclip_bot ; *ytc = yclip_top ; return use_xyclip ;
}

/*--------------------------------------------------------------------------*/

static int eqhighate( int nb , int nval , float *xar , float *xb )
{
   float *xd , xbot,xtop , frac,fi , xtest ;
   int ii, pp, pbot,ptop , bsiz , pstart,pend , nxd ;

   xd = (float *)malloc(sizeof(float)*nval) ;
   for( ii=nxd=0 ; ii < nval ; ii++ ) if( GOODVAL(xar[ii]) ) xd[nxd++] = xar[ii];
   if( nxd < 7*nb ){ free(xd); return 0; }  /* bad */
   qsort_float( nxd , xd ) ;

   /** scan for plateaus = runs of constant values longer than
                           nominal bin width, at the bottom and top **/

   xbot = xb[0]  = xd[0] ;
   xtop = xb[nb] = xd[nxd-1] ; if( xtop <= xbot ){ free(xd); return 0; }
   bsiz = (nxd/nb) ;

   xtest = xbot + (xtop-xbot)/(100.0f*nb) ;
   for( pp=1 ; pp < nxd && xd[pp] < xtest ; pp++ ) ; /*nada*/
   if( pp == nxd ){ free(xd); return 0; }  /* data is constant? */
   pbot = (pp > bsiz) ? pp : 0 ;

   xtest = xtop - (xtop-xbot)/(100.0f*nb) ;
   for( pp=nxd-2 ; pp > 0 && xd[pp] > xtest ; pp-- ) ; /*nada*/
   if( pp <= pbot ){ free(xd); return 0; }  /* something screwy */
   ptop = (nxd-1-pp > bsiz) ? pp : nxd-1 ;

   if( pbot > 0 ){
     xb[1] = 0.999999f*xd[pbot] + 0.000001f*xbot ; pstart = 2 ;
   } else {
     pstart = 1 ;
   }
   if( ptop < nxd-1 ){
     xb[nb-1] = 0.999999f*xd[ptop] + 0.000001f*xtop ; pend = nb-2 ;
   } else {
     pend = nb-1 ;
   }

   frac = (ptop-pbot)/(pend-pstart+2.0f) ;
   for( pp=pstart ; pp <= pend ; pp++ ){
     fi = pbot + frac*(pp-pstart+1.0f) ; ii = (int)fi ; fi = fi - ii ;
     xb[pp] = (1.0f-fi) * xd[ii] + fi * xd[ii+1] ;
   }

   free(xd) ; return nb ;
}

/*--------------------------------------------------------------------------*/

void set_2Dhist_xybin_eqhigh( int nb, int nval, float *xval, float *yval )
{
   int ii,jj ;

   FREEIF(xbin) ; FREEIF(ybin) ; nxybin = 0 ;
   if( nb < 3 || nval < 9*nb || xval == NULL || yval == NULL ) return ;

   nxybin = nb ;
   xbin   = (float *)malloc(sizeof(float)*(nb+1)) ;
   ybin   = (float *)malloc(sizeof(float)*(nb+1)) ;

   ii = eqhighate( nb , nval , xval , xbin ) ;
   jj = eqhighate( nb , nval , yval , ybin ) ;

   if( ii == 0 || jj == 0 ){
     FREEIF(xbin) ; FREEIF(ybin) ; nxybin = 0 ;  /* bad things happened */
   }
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Sets pointers to the bins in 2D histogram-ization, and return value
    is the number of bins (if 0, equal size bins are in use).
    Do NOT screw with the contents of the pointers: these are internal arrays!
    Also see set_2Dhist_xybin().
----------------------------------------------------------------------------*/

int get_2Dhist_xybin( float **xb , float **yb )
{
   if( xb != NULL ) *xb = xbin ;
   if( yb != NULL ) *yb = ybin ;
   return nxybin ;
}

/*--------------------------------------------------------------------------*/
/*! Build 2D histogram of x[0..n-1] and y[0..n-1], each point optionally
    weighted by w[0..n-1] (weights are all 1 if w==NULL).
    Used in the histogram-based measures of dependence between x[] and y[i].
    If something is bad on input, nbin is set to 0.  Otherwise, these global
    variables are set:
      - nbin = # of bins, nbp = nbin+1
      - nww  = sum of the weights used
      - xc   = marginal histogram of x[], for xc[0..nbin]
      - yc   = marginal histogram of y[], for yc[0..nbin]
      - xyc  = joint histogram of (x[],y[]), for XYC(0..nbin,0..nbin)
      - The histograms are normalized (by 1/nww) to have sum==1
      - Histogram can be retrieved by retrieve_2Dhist() and can be
        erased by clear_2Dhist()
      - Default number of equal-spaced bins in each direction is n^(1/3)
        - the exponent can be changed with set_2Dhist_hpower()
        - you can set the number of bins with set_2Dhist_hbin()
        - you can set unequal bins with set_2Dhist_xybin()
      - x[] values outside the range xbot..xtop (inclusive) or outside
        the unequal bins set in set_2Dhist_xybin() (if applicable) will
        not be used in the histogram; mutatis mutandum for y[]
----------------------------------------------------------------------------*/

void build_2Dhist( int n , float xbot,float xtop,float *x ,
                           float ybot,float ytop,float *y , float *w )
{
   register int ii,jj,kk ;
   float xb,xi , yb,yi , xx,yy , x1,y1 , ww ;
   byte *good ; int ngood , xyclip , nbm ;

ENTRY("build_2Dhist") ;

   /* bad inputs? */

   if( n <= 1 || x == NULL || y == NULL ){ clear_2Dhist(); EXRETURN; }

   /* get the min..max range for x data? */

   STATUS("compute good[]") ;
   good = (byte *)malloc(sizeof(byte)*n) ;         /* 28 Feb 2007 */
   for( ii=0 ; ii < n ; ii++ )
     good[ii] = GOODVAL(x[ii]) && GOODVAL(y[ii]) ;

   if( nxybin > 2 ){                       /* unequal bins */
     xbot = xbin[0] ; xtop = xbin[nxybin] ;
#if 0
     INFO_message("build_2Dhist: nxybin=%d x=%f..%f",nxybin,xbot,xtop) ;
#endif
   } else if( xbot >= xtop ){              /* equal bins, and must find range */
     xbot = WAY_BIG ; xtop = -WAY_BIG ;
     STATUS("compute xbot..xtop") ;
     for( ii=0 ; ii < n ; ii++ )
       if( good[ii] ){
              if( x[ii] > xtop ) xtop = x[ii] ;
         else if( x[ii] < xbot ) xbot = x[ii] ;
       }
   }
   if( xbot >= xtop ){ clear_2Dhist(); free(good); EXRETURN; }

   /* get the min..max range for y data? */

   if( nxybin > 0 ){
     ybot = ybin[0] ; ytop = ybin[nxybin] ;
#if 0
     INFO_message("build_2Dhist: nxybin=%d y=%f..%f",nxybin,ybot,ytop) ;
#endif
   } else if( ybot >= ytop ){
     ybot = WAY_BIG ; ytop = -WAY_BIG ;
     STATUS("compute ybot..ytop") ;
     for( ii=0 ; ii < n ; ii++ )
       if( good[ii] ){
              if( y[ii] > ytop ) ytop = y[ii] ;
         else if( y[ii] < ybot ) ybot = y[ii] ;
       }
   }
   if( ybot >= ytop ){ clear_2Dhist(); free(good); EXRETURN; }

   /*--- allocate space for 2D and 1D histograms ---*/

   if( nxybin > 2 ){                        /* array size is fixed by bins */
     nbin = nxybin ;
   } else {                                 /* compute new histo array size */
     nbin = (nhbin > 2) ? nhbin : (int)pow((double)n,hpow) ;
     if( nbin > 255 ) nbin = 255; else if( nbin < 3 ) nbin = 3;
   }
   nbp = nbin+1 ; nbm = nbin-1 ;

   STATUS("free-ing") ;
   FREEIF(xc) ; FREEIF(yc) ; FREEIF(xyc) ;

   STATUS("malloc-ing") ;
   xc  = (float *)calloc(sizeof(float),nbp) ;
   yc  = (float *)calloc(sizeof(float),nbp) ;
   xyc = (float *)calloc(sizeof(float),nbp*nbp) ;

   /*-- count number of good values left in range (in both x and y) --*/

   STATUS("counting") ;
   memset(good,0,n) ;
   for( ngood=ii=0 ; ii < n ; ii++ ){
     if( RANGVAL(x[ii],xbot,xtop) && RANGVAL(y[ii],ybot,ytop) && WW(ii) > 0.0f ){
       good[ii] = 1 ; ngood++ ;
     }
   }
   if( ngood < 3*nbin ){ clear_2Dhist(); free(good); EXRETURN; }

   /*--------------- make the 2D and 1D histograms ---------------*/

   xyclip = (nxybin <= 0) && use_xyclip &&
            (xbot < xclip_bot) && (xclip_bot < xclip_top) && (xclip_top < xtop) &&
            (ybot < yclip_bot) && (yclip_bot < yclip_top) && (yclip_top < ytop) ;

   if( nxybin <= 0 && !xyclip ){  /*------------ equal size bins ------------*/

if(PRINT_TRACING){
  char str[256];
  sprintf(str,"equal size bins: xbot=%g xtop=%g ybot=%g ytop=%g nbin=%d nval=%d ngood=%d",
          xbot,xtop,ybot,ytop,nbin,n,ngood);
  STATUS(str);
}

     xb = xbot ; xi = nbm/(xtop-xbot) ;
     yb = ybot ; yi = nbm/(ytop-ybot) ; nww = 0.0f ;
     for( ii=0 ; ii < n ; ii++ ){
       if( !good[ii] ) continue ;
       xx = (x[ii]-xb)*xi ;
       jj = (int)xx ; xx = xx - jj ; x1 = 1.0f-xx ;
       yy = (y[ii]-yb)*yi ;
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

   } else if( xyclip ){  /*------------ mostly equal bins ----------------*/

     float xbc=xclip_bot , xtc=xclip_top , ybc=yclip_bot , ytc=yclip_top ;

if(PRINT_TRACING){
  char str[256];
  sprintf(str,"mostly equal bins: xbc=%g xtc=%g ybc=%g ytc=%g nbin=%d",
          xbc,xtc,ybc,ytc,nbin) ; STATUS(str);
}

     STATUS("mostly equal bins") ;
     xi = (nbin-2.000001f)/(xtc-xbc) ;
     yi = (nbin-2.000001f)/(ytc-ybc) ; nww = 0.0f ;
     for( ii=0 ; ii < n ; ii++ ){
       if( !good[ii] ) continue ;
       xx = x[ii] ;
            if( xx < xbc ){ jj = 0   ; xx = 0.0f ; }
       else if( xx > xtc ){ jj = nbm ; xx = 1.0f ; }
       else               { xx = 1.0f+(xx-xbc)*xi; jj = (int)xx; xx = xx - jj; }
       yy = y[ii] ;
            if( yy < ybc ){ kk = 0   ; yy = 0.0f ; }
       else if( yy > ytc ){ kk = nbm ; yy = 1.0f ; }
       else               { yy = 1.0f+(yy-ybc)*yi; kk = (int)yy; yy = yy - kk; }

       x1 = 1.0f-xx ; y1 = 1.0f-yy ; ww = WW(ii) ; nww += ww ;

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

   } else {  /*-------------------- unequal size bins --------------------*/

     float *xdup, *ydup, *xv, *yv, *wv, frac,val,xt,yt ;
     int   *xn, *yn, *xin, *yin , ib,nb , maxsort ;

     maxsort = 16*nxybin ; if( maxsort > 512 ) maxsort = 512 ;
     xdup = (float *)malloc(sizeof(float)*maxsort) ;
     ydup = (float *)malloc(sizeof(float)*maxsort) ;
     xv   = (float *)malloc(sizeof(float)*maxsort) ;
     yv   = (float *)malloc(sizeof(float)*maxsort) ;
     wv   = (float *)malloc(sizeof(float)*maxsort) ;
     xn   = (int *)  malloc(sizeof(float)*maxsort) ;
     yn   = (int *)  malloc(sizeof(float)*maxsort) ;
     xin  = (int *)  malloc(sizeof(float)*maxsort) ;
     yin  = (int *)  malloc(sizeof(float)*maxsort) ; /* not yang */

     /* outer loop: process points starting at index = ib */

     STATUS("unequal sized bins") ;
     nww = 0.0f ;
     for( ib=0 ; ib < n ; ){

       /* extract up to maxsort good points from the data arrays */

       for( nb=0,ii=ib ; ii < n && nb < maxsort ; ii++ ){
         if( good[ii] ){
           wv[nb]=WW(ii); xdup[nb]=x[ii]; ydup[nb]=y[ii]; xn[nb]=yn[nb]=nb++;
         }
       }
#if 0
       INFO_message("extracted %d data points up to index=%d",nb,ii) ;
#endif
       ib = ii ;             /* where to start extracting next outer loop */
       if( nb == 0 ) break ; /* didn't find any good data ==> done! */

       /* sort the extracted x data values in xdup[],
          and keep track of where they came from in xn[] */

       qsort_floatint(nb,xdup,xn) ;

       /* scan through sorted xdup[] values,
           which will go into bin #kk which is xb..xt;
          store the bin index and the fractional location within the bin;
          the reason for doing the sorting is that finding the bin index
           kk will be efficient
           (don't have to search from start as we would for unordered values) */

       xb = xbin[0] ; xt = xbin[1] ; kk=0 ; frac = 1.0f/(xt-xb) ;
       for( ii=0 ; ii < nb ; ii++ ){
         val = xdup[ii] ;
         if( val > xt ){  /* not in the xb..xt bin, so scan up until it is */
           for( kk++ ; kk+1 < nxybin && val > xbin[kk+1] ; kk++ ) ; /*nada*/
           xb = xbin[kk] ; xt = xbin[kk+1] ; frac = 1.0f/(xt-xb) ;
         }
         jj = xn[ii] ;              /* index in unsorted array */
         xv[jj]  = frac*(val-xb) ;  /* fractional position in bin */
         xin[jj] = kk ;             /* bin index */
       }

       /* repeat the above for the y arrays */

       qsort_floatint(nb,ydup,yn) ;
       yb = ybin[0] ; yt = ybin[1] ; kk=0 ; frac = 1.0f/(yt-yb) ;
       for( ii=0 ; ii < nb ; ii++ ){
         val = ydup[ii] ;
         if( val > yt ){  /* not in the yb..yt bin, so scan up until it is */
           for( kk++ ; kk+1 < nxybin && val > ybin[kk+1] ; kk++ ) ; /*nada*/
           yb = ybin[kk] ; yt = ybin[kk+1] ; frac = 1.0f/(yt-yb) ;
         }
         jj = yn[ii] ;              /* index in unsorted array */
         yv[jj]  = frac*(val-yb) ;  /* fractional position in bin */
         yin[jj] = kk ;             /* bin index */
       }

       /* now distribute the values into the 1D and 2D histograms */

       for( ii=0 ; ii < nb ; ii++ ){
         ww = wv[ii] ; nww += ww ;
         jj = xin[ii]; kk = yin[ii] ;
         xx = xv[ii] ; x1 = 1.0f-xx ;
         yy = yv[ii] ; y1 = 1.0f-yy ;

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

     } /* end of outer loop (ib) over blocks */

     free(yin); free(xin); free(yn); free(xn);
     free(wv); free(yv); free(xv); free(ydup); free(xdup);

   } /*----- end of test on equal or unequal size bins -----*/

   STATUS("binning done") ;

   /*--- 26 Sep 2006: scale histogram to have sum==1 ---*/

   if( nww > 0.0f ){
     register float ni ; register int nbq ;
     STATUS("scaling") ;
     ni = 1.0f / nww ;
     for( ii=0 ; ii < nbp ; ii++ ){ xc[ii]  *= ni; yc[ii] *= ni; }
     nbq = nbp*nbp ;
     for( ii=0 ; ii < nbq ; ii++ ){ xyc[ii] *= ni; }
   }

   free(good); EXRETURN;
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

MRI_IMAGE *build_byteized_vectors( int n ,
                                   float xbot,float xtop,float *x ,
                                   float ybot,float ytop,float *y  )
{
   register int ii,jj,kk , gg ;
   float xb,xi , yb,yi , xx,yy , x1,y1 ;
   byte *good ; int ngood , xyclip , nbm ;
   MRI_IMAGE *bim ; byte *xbar,*ybar ;

ENTRY("build_byteized_vectors") ;

   /* bad inputs? */

   if( n <= 1 || x == NULL || y == NULL ) RETURN(NULL) ;

   /* get the min..max range for x data? */

   STATUS("compute good[]") ;
   good = (byte *)malloc(sizeof(byte)*n) ;
   for( ii=0 ; ii < n ; ii++ )
     good[ii] = GOODVAL(x[ii]) && GOODVAL(y[ii]) ;

   if( nxybin > 2 ){                       /* unequal bins */
     xbot = xbin[0] ; xtop = xbin[nxybin] ;
   } else if( xbot >= xtop ){              /* equal bins, and must find range */
     xbot = WAY_BIG ; xtop = -WAY_BIG ;
     for( ii=0 ; ii < n ; ii++ )
       if( good[ii] ){
              if( x[ii] > xtop ) xtop = x[ii] ;
         else if( x[ii] < xbot ) xbot = x[ii] ;
       }
   }
   if( xbot >= xtop ){ free(good); RETURN(NULL); }

   /* get the min..max range for y data? */

   if( nxybin > 0 ){
     ybot = ybin[0] ; ytop = ybin[nxybin] ;
   } else if( ybot >= ytop ){
     ybot = WAY_BIG ; ytop = -WAY_BIG ;
     for( ii=0 ; ii < n ; ii++ )
       if( good[ii] ){
              if( y[ii] > ytop ) ytop = y[ii] ;
         else if( y[ii] < ybot ) ybot = y[ii] ;
       }
   }
   if( ybot >= ytop ){ free(good); RETURN(NULL); }

   /*--- figure out quantization levels ---*/

   if( nxybin > 2 ){                        /* array size is fixed by bins */
     nbin = nxybin ;
   } else {                                 /* compute new histo array size */
     nbin = (nhbin > 2) ? nhbin : (int)pow((double)n,hpow) ;
   }
   if( nbin > 255 ) nbin = 255; else if( nbin < 16 ) nbin = 16 ;
   nbm = nbin-1 ;

   /*-- count number of good values left in range (in both x and y) --*/

   memset(good,0,n) ;
   for( ngood=ii=0 ; ii < n ; ii++ ){
     if( RANGVAL(x[ii],xbot,xtop) && RANGVAL(y[ii],ybot,ytop) ){
       good[ii] = 1 ; ngood++ ;
     }
   }
   if( ngood < 3*nbin ){ free(good); RETURN(NULL); }

   /*--------------- make the output data -----------------*/

   bim  = mri_new( ngood , 2 , MRI_byte ) ;  /* zero filled */
   xbar = MRI_BYTE_PTR(bim) ;
   ybar = xbar + ngood ;

   xyclip = (nxybin <= 0) && use_xyclip &&
            (xbot < xclip_bot) && (xclip_bot < xclip_top) && (xclip_top < xtop) &&
            (ybot < yclip_bot) && (yclip_bot < yclip_top) && (yclip_top < ytop) ;

   if( nxybin <= 0 && !xyclip ){  /*------------ equal size bins ------------*/

     xb = xbot ; xi = (nbin-0.01f)/(xtop-xbot) ;
     yb = ybot ; yi = (nbin-0.01f)/(ytop-ybot) ;
     for( gg=ii=0 ; ii < n ; ii++ ){
       if( !good[ii] ) continue ;
       xbar[gg] = (byte)((x[ii]-xb)*xi) ;
       ybar[gg] = (byte)((y[ii]-yb)*yi) ; gg++ ;
     }

   } else if( xyclip ){  /*------------ mostly equal bins ----------------*/

     float xbc=xclip_bot , xtc=xclip_top , ybc=yclip_bot , ytc=yclip_top ;

     xi = (nbin-2.01f)/(xtc-xbc) ;
     yi = (nbin-2.01f)/(ytc-ybc) ;
     for( gg=ii=0 ; ii < n ; ii++ ){
       if( !good[ii] ) continue ;
       xx = x[ii] ;
            if( xx < xbc ){ xbar[gg] = 0   ; }
       else if( xx > xtc ){ xbar[gg] = nbm ; }
       else               { xbar[gg] = (byte)(1.0f+(xx-xbc)*xi) ; }
       yy = y[ii] ;
            if( yy < ybc ){ ybar[gg] = 0   ; }
       else if( yy > ytc ){ ybar[gg] = nbm ; }
       else               { ybar[gg] = (byte)(1.0f+(yy-ybc)*yi) ; }
       gg++ ;
     }

   } else {  /*-------------------- unequal size bins --------------------*/

     float *xdup, *ydup, val,xt,yt ;
     int   *xn, *yn, *xin, *yin , ib,nb , maxsort ;

     maxsort = 16*nxybin ; if( maxsort > 512 ) maxsort = 512 ;
     xdup = (float *)malloc(sizeof(float)*maxsort) ;
     ydup = (float *)malloc(sizeof(float)*maxsort) ;
     xn   = (int *)  malloc(sizeof(float)*maxsort) ;
     yn   = (int *)  malloc(sizeof(float)*maxsort) ;
     xin  = (int *)  malloc(sizeof(float)*maxsort) ;
     yin  = (int *)  malloc(sizeof(float)*maxsort) ; /* not yang */

     /* outer loop: process points starting at index = ib */

     for( gg=ib=0 ; ib < n ; ){

       /* extract up to maxsort good points from the data arrays */

       for( nb=0,ii=ib ; ii < n && nb < maxsort ; ii++ ){
         if( good[ii] ){
           xdup[nb]=x[ii]; ydup[nb]=y[ii]; xn[nb]=yn[nb]=nb++;
         }
       }
       ib = ii ;             /* where to start extracting next outer loop */
       if( nb == 0 ) break ; /* didn't find any good data ==> done done! */

       /* sort the extracted x data values in xdup[],
          and keep track of where they came from in xn[] */

       qsort_floatint(nb,xdup,xn) ;

       /* scan through sorted xdup[] values,
           which will go into bin #kk which is xb..xt;
          store the bin index;
          the reason for doing the sorting is that finding the bin index
           kk will be efficient
           (don't have to search from start as we would for unordered values) */

       xb = xbin[0] ; xt = xbin[1] ; kk=0 ;
       for( ii=0 ; ii < nb ; ii++ ){
         val = xdup[ii] ;
         if( val > xt ){  /* not in the xb..xt bin, so scan up until it is */
           for( kk++ ; kk+1 < nxybin && val > xbin[kk+1] ; kk++ ) ; /*nada*/
           xb = xbin[kk] ; xt = xbin[kk+1] ;
         }
         jj = xn[ii] ;              /* index in unsorted array */
         xin[jj] = kk ;             /* bin index */
       }

       /* repeat the above for the y arrays */

       qsort_floatint(nb,ydup,yn) ;
       yb = ybin[0] ; yt = ybin[1] ; kk=0 ;
       for( ii=0 ; ii < nb ; ii++ ){
         val = ydup[ii] ;
         if( val > yt ){  /* not in the yb..yt bin, so scan up until it is */
           for( kk++ ; kk+1 < nxybin && val > ybin[kk+1] ; kk++ ) ; /*nada*/
           yb = ybin[kk] ; yt = ybin[kk+1] ;
         }
         jj = yn[ii] ;              /* index in unsorted array */
         yin[jj] = kk ;             /* bin index */
       }

       /* now distribute the values into the 1D and 2D histograms */

       for( ii=0 ; ii < nb ; ii++ ){
         xbar[gg] = (byte)xin[ii] ; ybar[gg] = (byte)yin[ii] ; gg++ ;
       }

     } /* end of outer loop (ib) over blocks */

     free(yin); free(xin); free(yn); free(xn); free(ydup); free(xdup);

   } /*----- end of test on equal or unequal size bins -----*/

   free(good); RETURN(bim);
}

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

static int ignore_zz = 0 ;
void THD_correlate_ignore_zerozero( int z ){ ignore_zz = z ; }

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
#if 0
     if( ii==0 && jj==0 && ignore_zz ) continue ;
#endif
     if( XYC(ii,jj) > 0.0f )
      val += XYC(ii,jj) * logf( XYC(ii,jj)/(xc[ii]*yc[jj]) ) ;
   }}
   return (1.4427f*val) ;  /* units are bits, just for fun */
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
     if( xc[ii] > 0.0f ) denom += xc[ii] * logf( xc[ii] ) ;
     if( yc[ii] > 0.0f ) denom += yc[ii] * logf( yc[ii] ) ;
     for( jj=0 ; jj < nbp ; jj++ ){
#if 0
       if( ii==0 && jj==0 && ignore_zz ) continue ;
#endif
       if( XYC(ii,jj) > 0.0f ) numer += XYC(ii,jj) * logf( XYC(ii,jj) );
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
      val -= XYC(ii,jj) * logf( XYC(ii,jj) ) ;
   }}
   return (1.4427f*val) ;  /* units are bits, just for fun */
}

/*--------------------------------------------------------------------------*/
/*! Compute joint entropy of x[] and y[i], with autoscaling. */

float THD_jointentrop( int n , float *x , float *y )
{
   return THD_jointentrop_scl( n, 1.0f,-1.0f, x, 1.0f,-1.0f, y, NULL ) ;
}


/*--------------------------------------------------------------------------*/
/* Decide if THD_corr_ratio_scl() computes symmetric or unsymmetric.        */

static int cr_mode = 1 ;  /* 0=unsym  1=sym mult  2=sym add */

void THD_corr_ratio_mode( int mm ){ cr_mode = mm ; }

/*--------------------------------------------------------------------------*/
/*! Compute the correlation ratio between two vectors, sort of.  [23 Aug 2006]
----------------------------------------------------------------------------*/

float THD_corr_ratio_scl( int n , float xbot,float xtop,float *x ,
                                  float ybot,float ytop,float *y , float *w )
{
   register int ii,jj ;
   register float vv,mm ;
   float    val , cyvar , uyvar , yrat,xrat ;

   /*-- build 2D histogram --*/

   build_2Dhist( n,xbot,xtop,x,ybot,ytop,y,w ) ;
   if( nbin <= 0 ) return 0.0f ;  /* something bad happened! */

   /*-- compute CR(y|x) from histogram --*/

   cyvar = 0.0f ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0f ){
       vv = mm = 0.0f ;               /* mm=E(y|x)  vv=E(y^2|x) */
       for( jj=1 ; jj < nbp ; jj++ ){
         mm += (jj * XYC(ii,jj)) ; vv += jj * (jj * XYC(ii,jj)) ;
       }
       cyvar += (vv - mm*mm/xc[ii] ) ; /* Var(y|x) */
     }
   }
   vv = mm = uyvar = 0.0f ;
   for( jj=1 ; jj < nbp ; jj++ ){     /* mm=E(y)  vv=E(y^2) */
     mm += (jj * yc[jj]) ; vv += jj * (jj * yc[jj]) ;
   }
   uyvar = vv - mm*mm ;                  /* Var(y) */
   yrat  = (uyvar > 0.0f) ? cyvar/uyvar  /* Var(y|x) / Var(y) */
                          : 1.0f ;

   if( cr_mode == 0 ) return (1.0f-yrat) ;   /** unsymmetric **/

   /** compute CR(x|y) also, for symmetrization **/

   cyvar = 0.0f ;
   for( jj=0 ; jj < nbp ; jj++ ){
     if( yc[jj] > 0.0f ){
       vv = mm = 0.0f ;               /* mm=E(x|y)  vv=E(x^2|y) */
       for( ii=1 ; ii < nbp ; ii++ ){
         mm += (ii * XYC(ii,jj)) ; vv += ii * (ii * XYC(ii,jj)) ;
       }
       cyvar += (vv - mm*mm/yc[jj] ) ; /* Var(x|y) */
     }
   }
   vv = mm = uyvar = 0.0f ;
   for( ii=1 ; ii < nbp ; ii++ ){     /* mm=E(x)  vv=E(x^2) */
     mm += (ii * xc[ii]) ; vv += ii * (ii * xc[ii]) ;
   }
   uyvar = vv - mm*mm ;                 /* Var(x) */
   xrat  = (uyvar > 0.0f) ? cyvar/uyvar /* Var(x|y) / Var(x) */
                          : 1.0f ;

   if( cr_mode == 2 ) return (1.0f - 0.5f*(xrat+yrat)) ; /** additive **/

   return (1.0f - xrat*yrat) ;                           /** multiplicative **/
}

/*--------------------------------------------------------------------------*/
/*! Compute CR of x[] and y[], using autoscaling. */

float THD_corr_ratio( int n , float *x , float *y )
{
   return THD_corr_ratio_scl( n , 1.0f,-1.0f , x, 1.0f,-1.0f , y , NULL ) ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the Hellinger metric between two vectors, sort of.
----------------------------------------------------------------------------*/

float THD_hellinger_scl( int n , float xbot,float xtop,float *x ,
                                 float ybot,float ytop,float *y , float *w )
{
   register int ii,jj ;
   register float val , pq ;

   /*-- build 2D histogram --*/

   build_2Dhist( n,xbot,xtop,x,ybot,ytop,y,w ) ;
   if( nbin <= 0 || nww <= 0 ) return 0.0f ;  /* something bad happened! */

   /*-- compute metric from histogram --*/

   val = 0.0f ;
   for( ii=0 ; ii < nbp ; ii++ ){
    for( jj=0 ; jj < nbp ; jj++ ){
     pq = XYC(ii,jj) ;
     if( pq > 0.0f ) val += sqrtf( pq * xc[ii] * yc[jj] ) ;
   }}
   return (1.0f-val) ;
}

/*--------------------------------------------------------------------------*/
/*! Compute Hellinger metric between x[] and y[i], with autoscaling. */

float THD_hellinger( int n , float *x , float *y )
{
   return THD_hellinger_scl( n, 1.0f,-1.0f, x, 1.0f,-1.0f, y, NULL ) ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the Hellinger metric, mutual info, normalized MI, and
    symmetrized correlation ratio, and return all 4 (in that order)
    using the 1D and 2D histograms from build_2Dhist().

    The first 3 values all measure the closeness of the joint histogram to
    the product of the marginals:
      - Hellinger is smaller when the joint is closer to the marginals' product
      - MI is also smaller when the joint is closer to the marginal's product
      - NMI is larger when the joint is closer to the marginal's product
    Correlation ratio (symmetrized by addition == CRA) is larger when
    the two variables are nonlinearly correlated.

    As measures of association (generalized correlation): more closely
    associated variables correspond to larger Hellinger and MI and CRA,
    and to smaller NMI.
*//*------------------------------------------------------------------------*/

float_quad THD_helmicra_scl( int n , float xbot,float xtop,float *x ,
                             float ybot,float ytop,float *y , float *w )
{
   register int ii,jj ;
   register float hel , pq , vv,uu ;
   float    val , cyvar , uyvar , yrat,xrat ;
   float_quad hmc = {0.0f,0.0f,0.0f,0.f} ;

   /*-- build 2D histogram --*/

   build_2Dhist( n,xbot,xtop,x,ybot,ytop,y,w ) ;
   if( nbin <= 0 || nww <= 0 ) return hmc ;  /* something bad happened! */

   /*-- compute Hel, MI, NMI from histogram --*/

   hel = vv = uu = 0.0f ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0f ) vv += xc[ii] * logf( xc[ii] ) ;
     if( yc[ii] > 0.0f ) vv += yc[ii] * logf( yc[ii] ) ;
     for( jj=0 ; jj < nbp ; jj++ ){
       pq = XYC(ii,jj) ;
       if( pq > 0.0f ){
         hel += sqrtf( pq * xc[ii] * yc[jj] ) ;
         uu  += pq * logf( pq );
       }
     }
   }
   hmc.a = 1.0f - hel ;                   /* Hellinger */
   hmc.b = uu - vv ;                      /* MI */
   hmc.c = (vv != 0.0f) ? uu/vv : 0.0f ;  /* NMI */

   /*-- compute CR(y|x) from histogram --*/

   cyvar = 0.0f ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0f ){
       vv = uu = 0.0f ;               /* uu=E(y|x)  vv=E(y^2|x) */
       for( jj=1 ; jj < nbp ; jj++ ){
         uu += (jj * XYC(ii,jj)) ; vv += jj * (jj * XYC(ii,jj)) ;
       }
       cyvar += (vv - uu*uu/xc[ii] ) ; /* Var(y|x) */
     }
   }
   vv = uu = uyvar = 0.0f ;
   for( jj=1 ; jj < nbp ; jj++ ){     /* uu=E(y)  vv=E(y^2) */
     uu += (jj * yc[jj]) ; vv += jj * (jj * yc[jj]) ;
   }
   uyvar = vv - uu*uu ;                  /* Var(y) */
   yrat  = (uyvar > 0.0f) ? cyvar/uyvar  /* Var(y|x) / Var(y) */
                          : 1.0f ;

   /** compute CR(x|y) also, for symmetrization **/

   cyvar = 0.0f ;
   for( jj=0 ; jj < nbp ; jj++ ){
     if( yc[jj] > 0.0f ){
       vv = uu = 0.0f ;               /* uu=E(x|y)  vv=E(x^2|y) */
       for( ii=1 ; ii < nbp ; ii++ ){
         uu += (ii * XYC(ii,jj)) ; vv += ii * (ii * XYC(ii,jj)) ;
       }
       cyvar += (vv - uu*uu/yc[jj] ) ; /* Var(x|y) */
     }
   }
   vv = uu = uyvar = 0.0f ;
   for( ii=1 ; ii < nbp ; ii++ ){     /* uu=E(x)  vv=E(x^2) */
     uu += (ii * xc[ii]) ; vv += ii * (ii * xc[ii]) ;
   }
   uyvar = vv - uu*uu ;                 /* Var(x) */
   xrat  = (uyvar > 0.0f) ? cyvar/uyvar /* Var(x|y) / Var(x) */
                          : 1.0f ;

   hmc.d = 1.0f - 0.5f*(xrat+yrat) ; /** additive symmetrization **/
   return hmc ;
}

/*--------------------------------------------------------------------------*/
/*! see THD_helmicra_scl(). */

float_quad THD_helmicra( int n , float *x , float *y )
{
   return THD_helmicra_scl( n, 1.0f,-1.0f, x, 1.0f,-1.0f, y, NULL ) ;
}
