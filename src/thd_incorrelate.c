#include "mrilib.h"

typedef struct {
  int meth ;
} INCOR_generic ;

typedef struct {
  int meth ;
  int npt ;
  double sx , sxx , sy , syy , sxy , sw ;
} INCOR_pearson ;

typedef struct {
  int meth ;
  int nbin ;
  float *xc , *yc , *xyc , nww ;
  float xxbot , xxtop , yybot , yytop ;
  float xcbot , xctop , ycbot , yctop ;
} INCOR_2Dhist ;

#undef  INCOR_methcode
#define INCOR_methcode(vp) ( ((vp) != NULL) ? ((INCOR_generic *)vp)->meth : 0 )

/****************************************************************************/
/*** Histogram-based measurements of dependence between two float arrays. ***/
/****************************************************************************/

#undef  WW
#define WW(i) ((w==NULL) ? 1.0f : w[i])   /* weight function for i'th datum */

#undef  XYC
#define XYC(p,q) xyc[(p)+(q)*nbp]

#ifndef WAY_BIG
#  define WAY_BIG 1.e+10
#endif

#undef  GOODVAL
#define GOODVAL(x) ((x) < WAY_BIG)                 /* x is not preposterous */

#undef  RANGVAL
#define RANGVAL(x,b,t) ((x) >= (b) && (x) <= (t))  /* x between b and t */

/*--------------------------------------------------------------------------*/

static float_pair INCOR_clipate( int nval , float *xar )
{
   MRI_IMAGE *qim; float cbot,ctop, mmm , *qar; float_pair rr; int ii,nq;

ENTRY("INCOR_clipate") ;

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

static float_quad INCOR_2Dhist_xyclip( int nval , float *xval , float *yval )
{
   float_pair xcc , ycc ; float_quad xxyycc={0.0f,0.0f,0.0f,0.0f} ;

ENTRY("INCOR_2Dhist_xyclip") ;

   if( nval < 666 || xval == NULL || yval == NULL ) RETURN(xxyycc) ;

   xcc = clipate( nval , xval ) ;
   ycc = clipate( nval , yval ) ;

   if( xcc.a >= xcc.b || ycc.a >= ycc.b ) RETURN(xxyycc) ;

   xxyycc.a = xcc.a ; xxyycc.b = xcc.b ;
   xxyycc.c = ycc.a ; xxyycc.d = ycc.b ; RETURN(xxyycc) ;
}

/*--------------------------------------------------------------------------*/
/*! Load 2D histogram of x[0..n-1] and y[0..n-1], each point optionally
    weighted by w[0..n-1] (weights are all 1 if w==NULL).
    Used in the histogram-based measures of dependence between x[] and y[i].
    If something is bad on input, nbin is set to 0.  Otherwise, these global
    variables are set:
      - nbin = # of bins, nbp = nbin+1
      - nww  = sum of the weights used
      - xc   = marginal histogram of x[], for xc[0..nbin]   (nbp points in)
      - yc   = marginal histogram of y[], for yc[0..nbin]   (each direction)
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

static void INCOR_addto_2Dhist( INCOR_2Dhist *tdh , int n ,
                                float *x , float *y , float *w )
{
   register int ii,jj,kk ;
   float xb,xi , yb,yi , xx,yy , x1,y1 , ww ;
   byte *good ; int ngood , xyclip ;
   float xxbot,xxtop , yybot,yytop ;
   float xcbot,xctop , ycbot,yctop ;
   int nbin,nbp,nbm ;
   float *xc , *yc , *xyc ; float nww ;

ENTRY("INCOR_addto_2Dhist") ;

   if( tdh == NULL || tdh->nbin < 3 || n <= 0 || x == NULL || y == NULL ) EXRETURN ;

   nbin = tdh->nbin ; nbp = nbin+1 ; nbm = nbin-1 ;
   xc = tdh->xc ; yc = tdh->yc ; xyc = tdh->xyc ; nww = tdh->nww ;

   /* get the min..max range for x and y data? */

   good = (byte *)malloc(sizeof(byte)*n) ;
   for( ii=0 ; ii < n ; ii++ )
     good[ii] = GOODVAL(x[ii]) && GOODVAL(y[ii]) && (WW(ii) > 0.0f) ;

   xxbot = tdh->xxbot ; xxtop = tdh->xxtop ;
   yybot = tdh->yybot ; yytop = tdh->yytop ;
   xcbot = tdh->xcbot ; xctop = tdh->xctop ;
   ycbot = tdh->ycbot ; yctop = tdh->yctop ;

   if( (xxbot >= xxtop) || (yybot >= yytop) ){  /* data ranges undefined */

     xxbot = WAY_BIG ; xxtop = -WAY_BIG ;
     for( ii=0 ; ii < n ; ii++ ){
       if( good[ii] ){
              if( x[ii] > xxtop ) xxtop = x[ii] ;
         else if( x[ii] < xxbot ) xxbot = x[ii] ;
       }
     }
     if( xxbot >= xxtop ){ free(good); EXRETURN; }

     yybot = WAY_BIG ; yytop = -WAY_BIG ;
     for( ii=0 ; ii < n ; ii++ ){
       if( good[ii] ){
              if( y[ii] > yytop ) yytop = y[ii] ;
         else if( y[ii] < yybot ) yybot = y[ii] ;
       }
     }
     if( yybot >= yytop ){ free(good); EXRETURN; }

     tdh->xxbot = xxbot ; tdh->xxtop = xxtop ;
     tdh->yybot = yybot ; tdh->yytop = yytop ;
   }

   /*-- count number of good values left in range (in both x and y) --*/

   memset(good,0,n) ;
   for( ngood=ii=0 ; ii < n ; ii++ ){
     if( RANGVAL(x[ii],xbot,xtop) && RANGVAL(y[ii],ybot,ytop) && (WW(ii) > 0.0f) ){
       good[ii] = 1 ; ngood++ ;
     }
   }
   if( ngood == 0 ){ free(good) ; EXRETURN ; }

   /*--------------- make the 2D and 1D histograms ---------------*/

   xyclip = (xxbot < xcbot) && (xcbot < xctop) && (xctop < xtop) &&
            (yybot < ycbot) && (ycbot < yctop) && (yctop < ytop) ;

   if( !xyclip ){  /*------------ equal size bins ------------*/

     xb = xxbot ; xi = nbm/(xxtop-xxbot) ;
     yb = yybot ; yi = nbm/(yytop-yybot) ;
     for( ii=0 ; ii < n ; ii++ ){
       if( !good[ii] ) continue ;
       xx = (x[ii]-xb)*xi ;
       jj = (int)xx ; xx = xx - jj ; x1 = 1.0f-xx ;
       yy = (y[ii]-yb)*yi ;
       kk = (int)yy ; yy = yy - kk ; y1 = 1.0f-yy ;
       ww = WW(ii) ; nww += ww ;

       xc[jj] += (x1*ww); xc[jj+1] += (xx*ww);
       yc[kk] += (y1*ww); yc[kk+1] += (yy*ww);

       XYC(jj  ,kk  ) += x1*(y1*ww) ;
       XYC(jj+1,kk  ) += xx*(y1*ww) ;
       XYC(jj  ,kk+1) += x1*(yy*ww) ;
       XYC(jj+1,kk+1) += xx*(yy*ww) ;
     }

   } else if( xyclip ){  /*------------ mostly equal bins ----------------*/

     float xbc=xcbot , xtc=xctop , ybc=ycbot , ytc=yctop ;

     xi = (nbin-2.000001f)/(xtc-xbc) ;
     yi = (nbin-2.000001f)/(ytc-ybc) ;
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

       xc[jj] +=  x1*ww ; xc[jj+1] +=  xx*ww ;
       yc[kk] += (y1*ww); yc[kk+1] += (yy*ww);

       XYC(jj  ,kk  ) += x1*(y1*ww) ;
       XYC(jj+1,kk  ) += xx*(y1*ww) ;
       XYC(jj  ,kk+1) += x1*(yy*ww) ;
       XYC(jj+1,kk+1) += xx*(yy*ww) ;
     }

   }

   tdh->nww = nww ;
   free(good) ; EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* scale histogram to have sum==1 */

void INCOR_normalize_2Dhist( INCOR_2Dhist *tdh )
{
   float nww , *xc, *yc, *xyc ; int nbp ;
   if( tdh == NULL ) return ;
   nww = tdh->nww; xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;
   if( nww > 0.0f && xyc != NULL && xc != NULL && yc != NULL ){
     register float ni ; register int nbq , ii ;
     ni = 1.0f / nww ;
     for( ii=0 ; ii < nbp ; ii++ ){ xc[ii]  *= ni; yc[ii] *= ni; }
     nbq = nbp*nbp ;
     for( ii=0 ; ii < nbq ; ii++ ){ xyc[ii] *= ni; }
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
   normalize_2Dhist() ;

   /*-- compute MI from histogram --*/

   val = 0.0f ;
   for( ii=0 ; ii < nbp ; ii++ ){
    for( jj=0 ; jj < nbp ; jj++ ){
     if( XYC(ii,jj) > 0.0f )
      val += XYC(ii,jj) * logf( XYC(ii,jj)/(xc[ii]*yc[jj]) ) ;
   }}
   return (1.4427f*val) ;  /* units are bits, just for fun */
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
   normalize_2Dhist() ;

   /*-- compute NMI from histogram --*/

   denom = numer = 0.0f ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0f ) denom += xc[ii] * logf( xc[ii] ) ;
     if( yc[ii] > 0.0f ) denom += yc[ii] * logf( yc[ii] ) ;
     for( jj=0 ; jj < nbp ; jj++ ){
       if( XYC(ii,jj) > 0.0f ) numer += XYC(ii,jj) * logf( XYC(ii,jj) );
     }
   }
   if( denom != 0.0f ) denom = numer / denom ;
   return denom ;
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
   normalize_2Dhist() ;

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
   normalize_2Dhist() ;

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
   normalize_2Dhist() ;

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

/*----------------------------------------------------------------------------*/

static double hpow = 0.33333333333 ;
void INCOR_set_2Dhist_hpower( double hh )
{
  hpow = (hh > 0.0 && hh < 1.0) ? hh : 0.33333333333 ;
}

static int nhbin = 0 ;
void set_2Dhist_hbin( int nn ){ nhbin = nn; }

int INCOR_2Dhist_compute_nbin( int ndata )
{
   int nbin ;

   nbin = (nhbin > 2) ? nhbin : (int)pow((double)ndata,hpow) ;
   if( nbin > 255 ) nbin = 255 ; else if( nbin < 3 ) nbin = 3 ;
   return nbin ;
}

/*----------------------------------------------------------------------------*/

static INCOR_2Dhist * INCOR_create_2Dhist( int nbin ,
                                           float xbot , float xtop ,
                                           float ybot , float ytop ,
                                           float xcbot, float xctop,
                                           float ycbot, float yctop  )
{
   INCOR_2Dhist *tdh ; int nbp ;

ENTRY("INCOR_create_2Dhist") ;

   if( nbin < 3 ) RETURN(NULL) ;

   tdh = (INCOR_2Dhist *)malloc(sizeof(INCOR_2Dhist)) ;

   tdh->meth  = 0 ;  /* undefined as yet */
   tdh->nbin  = nbin ;
   tdh->xxbot = xbot ;  tdh->yybot = ybot ;
   tdh->xxtop = xtop ;  tdh->yytop = ytop ;
   tdh->xcbot = xcbot ; tdh->ycbot = ycbot ;
   tdh->xctop = xctop ; tdh->yctop = yctop ;

   nbp = nbin+1 ;
   tdh->xc  = (float *)calloc(sizeof(float),nbp) ;
   tdh->yc  = (float *)calloc(sizeof(float),nbp) ;
   tdh->xyc = (float *)calloc(sizeof(float),nbp*nbp) ;
   tdh->nww = 0.0f ;

   RETURN(tdh) ;
}

/*----------------------------------------------------------------------------*/

static void INCOR_destroy_2Dhist( INCOR_2Dhist *tin )
{
   if( tin == NULL ) return ;
   if( tin->xc  != NULL ) free(tin->xc) ;
   if( tin->yc  != NULL ) free(tin->yc) ;
   if( tin->xyc != NULL ) free(tin->xyc) ;
   free(tin) ;
   return ;
}

/*----------------------------------------------------------------------------*/

static void INCOR_copyover_2Dhist( INCOR_2Dhist *tin , INCOR_2Dhist *tout )
{
   int nbp ;

ENTRY("INCOR_copyover_2Dhist") ;

   if( tin == NULL || tout == NULL || tin == tout ) EXRETURN ;

   if( tout->xc  != NULL ) free(tout->xc) ;
   if( tout->yc  != NULL ) free(tout->yc) ;
   if( tout->xyc != NULL ) free(tout->xyc) ;

   tout->meth  = tin->meth ;
   tout->nbin  = tin->nbin ;
   tout->xxbot = tin->xxbot ; tout->yybot = tin->yybot ;
   tout->xxtop = tin->xxtop ; tout->yytop = tin->yytop ;
   tout->xcbot = tin->xcbot ; tout->ycbot = tin->ycbot ;
   tout->xctop = tin->xctop ; tout->yctop = tin->yctop ;
   tout->nww   = tin->nww ;

   nbp = tin->nbin + 1 ;
   tout->xc  = (float *)malloc(sizeof(float)*nbp) ;
   tout->yc  = (float *)malloc(sizeof(float)*nbp) ;
   tout->xyc = (float *)malloc(sizeof(float)*nbp*nbp) ;

   memcpy( tout->xc , tin->xc , sizeof(float)*nbp ) ;
   memcpy( tout->yc , tin->yc , sizeof(float)*nbp ) ;
   memcpy( tout->xyc, tin->xyc, sizeof(float)*nbp*nbp ) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static void INCOR_addto_incomplete_pearson( int n, float *x, float *y, float *w, INCOR_pearson *inpear )
{
   int ii ; double sx,sxx , sy,syy,sxy , sw ;

   if( n <= 0 || x == NULL || y == NULL || inpear == NULL ) return ;

   sx = inpear->sx ; sxx = inpear->sxx ;
   sy = inpear->sy ; syy = inpear->syy ; sxy = inpear->sxy ; sw = inpear->sw ;

   if( w == NULL ){
     double xx , yy ;
     for( ii=0 ; ii < n ; ii++ ){
       xx = (double)x[ii] ; yy = (double)y[ii] ;
       sx += xx ; sxx += xx*xx ; sy += yy ; syy += yy*yy ; sxy += xx*yy ;
     }
     sw += (double)n ;
   } else {
     double xx , yy , ww ;
     for( ii=0 ; ii < n ; ii++ ){
       xx = (double)x[ii] ; yy = (double)y[ii] ; ww = (double)w[ii] ;
       sx += xx*ww ; sxx += xx*xx*ww ;
       sy += yy*ww ; syy += yy*yy*ww ; sxy += xx*yy*ww ; sw += ww ;
     }
   }

   inpear->npt += n ;
   inpear->sx   = sx ; inpear->sxx = sxx ;
   inpear->sy   = sy ; inpear->syy = syy ; inpear->sxy = sxy ; inpear->sw = sw ;
   return ;
}

/*----------------------------------------------------------------------------*/

static void INCOR_destroy_incomplete_pearson( INCOR_pearson *inpear )
{
   if( inpear != NULL ) free((void *)inpear) ;
}

/*----------------------------------------------------------------------------*/

static INCOR_pearson * INCOR_create_incomplete_pearson(void)
{
   INCOR_pearson *inpear ;

   inpear = (INCOR_pearson *)malloc(sizeof(INCOR_pearson)) ;
   inpear->sx  = 0.0 ; inpear->sxx = 0.0 ;
   inpear->sy  = 0.0 ; inpear->syy = 0.0 ;
   inpear->sxy = 0.0 ; inpear->sw  = 0.0 ; inpear->npt = 0 ;

   inpear->meth = GA_MATCH_PEARSON_SCALAR ;
   return ;
}

/*----------------------------------------------------------------------------*/

static float INCOR_compute_incomplete_pearson( INCOR_pearson *inpear )
{
   double xv , yv , xy , swi ;

   if( inpear->sw <= 0.0 ) return 0.0f ;

   swi = 1.0 / inpear->sw ;

   xv = inpear->sxx - inpear->sx * inpear->sx * swi ;
   yv = inpear->syy - inpear->sy * inpear->sy * swi ;
   xy = inpear->sxy - inpear->sx * inpear->sy * swi ;

   if( xv <= 0.0 || yv <= 0.0 ) return 0.0f ;
   return (float)(xy/sqrt(xv*yv)) ;
}

/*============================================================================*/
/* Generic INCOR functions */

void * INCOR_create( int meth , floatvec *mpar )
{

ENTRY("INCOR_create") ;

   switch( meth ){

     case GA_MATCH_PEARSON_SCALAR:
       vinc = (void *)INCOR_create_incomplete_pearson() ;
     break ;

     case GA_MATCH_MUTINFO_SCALAR:     /* methods that use 2D histograms */ 
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_NORMUTIN_SCALAR:
     case GA_MATCH_HELLINGER_SCALAR:
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CRAT_USYM_SCALAR:{
       INCOR_2Dhist *tdh ;
       int nbin ;
       float xbot,xtop, ybot,ytop, xcbot,xctop, ycbot,yctop;
       if( mpar == NULL ) break ;
       nbin  = (int)mpar->ar[0] ;
       if( nbin < 0 ) nbin = INCOR_2Dhist_compute_nbin(-nbin) ;
       xbot  = mpar->ar[1] ; xtop  = mpar->ar[2] ;
       ybot  = mpar->ar[3] ; ytop  = mpar->ar[4] ;
       xcbot = mpar->ar[5] ; xctop = mpar->ar[6] ;
       ycbot = mpar->ar[7] ; yctop = mpar->ar[8] ;
       tdh = INCOR_create_2Dhist( nbin , xbot ,xtop , ybot ,ytop ,
                                         xcbot,xctop, ycbot,yctop ) ;
       if( tdh != NULL ){
         tdh->meth = meth ; vinc = (void *)tdh ;
       }
     }
     break ;

   }

   RETURN(vinc) ;
}

/*----------------------------------------------------------------------------*/

void INCOR_destroy( void *vp )
{
ENTRY("INCOR_destroy") ;

   if( vp == NULL ) EXRETURN ;

   switch( INCOR_methcod(vp) ){

     case GA_MATCH_PEARSON_SCALAR:
       INCOR_destroy_incomplete_pearson(vp) ;
     break ;

     case GA_MATCH_MUTINFO_SCALAR:
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_NORMUTIN_SCALAR:
     case GA_MATCH_HELLINGER_SCALAR:
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CRAT_USYM_SCALAR:
       INCOR_destroy_2Dhist(vp) ;
     break ;

   }
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void INCOR_copyover( void *vin , void *vout )
{
ENTRY("INCOR_copyover") ;

   if( vin == NULL || vout == NULL || vin == vout ) EXRETURN ;

   switch( INCOR_methcode(vin) ){

     case GA_MATCH_PEARSON_SCALAR:
       memcpy( vout , vin , sizeof(INCOR_pearson) ) ;
     break ;

     case GA_MATCH_MUTINFO_SCALAR:
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_NORMUTIN_SCALAR:
     case GA_MATCH_HELLINGER_SCALAR:
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CRAT_USYM_SCALAR:
       INCOR_copyover_2Dhist( vin , vout ) ;
     break ;

   }

   EXRETURN ;
}
