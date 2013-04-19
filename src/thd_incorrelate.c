#include "mrilib.h"

/*** This file is intended to be #include-d into mri_nwarp.c ***/

/***
     #ifdef USE_OMP
     #include <omp.h>
     #endif
***/

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
  int npt ;
  double sx , sxx , sy , syy , sxy , sw ;
  double xcbot , xctop , ycbot , yctop ;
  double xdbot , xdtop , ydbot , ydtop ;
} INCOR_pearclp ;

typedef struct {
  int meth ;
  int nbin ;
  float *xc , *yc , *xyc , nww ;
  float xxbot , xxtop , yybot , yytop ;
  float xcbot , xctop , ycbot , yctop ;
} INCOR_2Dhist ;

#undef  INCOR_methcode
#define INCOR_methcode(vp) ( ((vp) != NULL) ? ((INCOR_generic *)vp)->meth : 0 )

#undef  MYatanh
#define MYatanh(x) ( ((x)<-0.9993293) ? -4.0                \
                    :((x)>+0.9993293) ? +4.0 : atanh(x) )

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

float_pair INCOR_clipate( int nval , float *xar )
{
   MRI_IMAGE *qim; float cbot,ctop, mmm , *qar; float_pair rr; int ii,nq;

ENTRY("INCOR_clipate") ;

   qim = mri_new_vol( nval,1,1 , MRI_float ) ; qar = MRI_FLOAT_PTR(qim) ;
   for( ii=nq=0 ; ii < nval ; ii++ ) if( GOODVAL(xar[ii]) ) qar[nq++] = xar[ii];
   qim->nx = qim->nvox = nq ;
   if( nq < 666 ){ rr.a = 1.0f; rr.b = 0.0f; mri_free(qim); RETURN(rr); }
   mmm  = mri_min( qim ) ;
   if( mmm >= 0.0f ){   /* for positive images */
     cbot = THD_cliplevel( qim , 0.321f ) ;
     ctop = mri_quantile ( qim , 0.987f ) ;
     if( ctop > 6.543f*cbot ) ctop = 6.543f*cbot ;
   } else {  /* for images including negative values: no go */
     cbot = 1.0f; ctop = 0.0f;
   }
   mri_free(qim) ;
   rr.a = cbot ; rr.b = ctop ; RETURN(rr) ;
}

/*--------------------------------------------------------------------------*/

float_quad INCOR_2Dhist_xyclip( int nval , float *xval , float *yval )
{
   float_pair xcc , ycc ; float_quad xxyycc={0.0f,0.0f,0.0f,0.0f} ;

ENTRY("INCOR_2Dhist_xyclip") ;

   if( nval < 666 || xval == NULL || yval == NULL ) RETURN(xxyycc) ;

   xcc = INCOR_clipate( nval , xval ) ;
   ycc = INCOR_clipate( nval , yval ) ;

   if( xcc.a >= xcc.b || ycc.a >= ycc.b ) RETURN(xxyycc) ;

   xxyycc.a = xcc.a ; xxyycc.b = xcc.b ;
   xxyycc.c = ycc.a ; xxyycc.d = ycc.b ; RETURN(xxyycc) ;
}

/*--------------------------------------------------------------------------*/

float_quad INCOR_2Dhist_minmax( int nval , float *xval , float *yval )
{
   float_quad xxyy={0.0f,0.0f,0.0f,0.0f} ;
   int ii ; float xb,xt,yb,yt ;

ENTRY("INCOR_2Dhist_minmax") ;

   if( nval < 1 || xval == NULL || yval == NULL ) RETURN(xxyy) ;

   xb = xt = xval[0] ; yb = yt = yval[0] ;
   for( ii=1 ; ii < nval ; ii++ ){
          if( xval[ii] < xb ) xb = xval[ii] ;
     else if( xval[ii] > xt ) xt = xval[ii] ;
          if( yval[ii] < yb ) yb = yval[ii] ;
     else if( yval[ii] > yt ) yt = yval[ii] ;
   }
   xxyy.a = xb ; xxyy.b = xt ; xxyy.c = yb ; xxyy.d = yt ; RETURN(xxyy) ;
}

/*--------------------------------------------------------------------------*/

static byte *good=NULL ;
static int  agood=0 ;

void INCOR_setup_good( int ng )
{
   if( ng <= 0 ){
     if( good != NULL ){ free(good); good = NULL; }
     agood = 0 ;
   } else if( ng > agood ){
     good = realloc( good , sizeof(byte)*ng ) ; agood = ng ;
   }
   if( agood > 0 && good != NULL ) AAmemset(good,0,sizeof(byte)*agood) ;
   return ;
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
      - The histograms can be later normalized (by 1/nww) to have sum==1
      - Histogram can be retrieved by retrieve_2Dhist() and can be
        erased by clear_2Dhist()
      - Default number of equal-spaced bins in each direction is n^(1/3)
        - the exponent can be changed with INCOR_set_2Dhist_hpower()
        - you can set the number of bins with INCOR_set_2Dhist_hbin()
      - x[] values outside the range xbot..xtop (inclusive) will not be
        used in the histogram; mutatis mutandum for y[]
*//*------------------------------------------------------------------------*/

void INCOR_addto_2Dhist( INCOR_2Dhist *tdh , int n , float *x , float *y , float *w )
{
   int ii , ngood , xyclip ;
   float xxbot,xxtop , yybot,yytop ;
   float xcbot,xctop , ycbot,yctop ;
   int nbin,nbp,nbm ;
   float *xc , *yc , *xyc ; float nww ;
#if 0
   int use_omp , nthr ;
#else
#  define use_omp 0
#  define nthr    1
#endif

ENTRY("INCOR_addto_2Dhist") ;

   if( tdh == NULL || tdh->nbin < 3 || n <= 0 || x == NULL || y == NULL ) EXRETURN ;

   nbin = tdh->nbin ; nbp = nbin+1 ; nbm = nbin-1 ;
   xc = tdh->xc ; yc = tdh->yc ; xyc = tdh->xyc ; nww = tdh->nww ;

   /* get the min..max range for x and y data? */

   INCOR_setup_good(n+4) ;
   for( ngood=ii=0 ; ii < n ; ii++ ){
     if( GOODVAL(x[ii]) && GOODVAL(y[ii]) && (WW(ii) > 0.0f) ){
       good[ii] = 1 ; ngood++ ;
     }
   }
   if( ngood == 0 ) EXRETURN ;

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
     if( xxbot >= xxtop ) EXRETURN ;

     yybot = WAY_BIG ; yytop = -WAY_BIG ;
     for( ii=0 ; ii < n ; ii++ ){
       if( good[ii] ){
              if( y[ii] > yytop ) yytop = y[ii] ;
         else if( y[ii] < yybot ) yybot = y[ii] ;
       }
     }
     if( yybot >= yytop ) EXRETURN ;

     tdh->xxbot = xxbot ; tdh->xxtop = xxtop ;
     tdh->yybot = yybot ; tdh->yytop = yytop ;

     xcbot = ycbot = tdh->xcbot = tdh->ycbot =  WAY_BIG ;  /* disable */
     xctop = yctop = tdh->xctop = tdh->yctop = -WAY_BIG ;  /* clipping */
   }

   /*-- count number of good values left in range (in both x and y) --*/

   AAmemset(good,0,n) ;

   for( ngood=ii=0 ; ii < n ; ii++ ){
     if( RANGVAL(x[ii],xxbot,xxtop) && RANGVAL(y[ii],yybot,yytop) && (WW(ii) > 0.0f) ){
       good[ii] = 1 ; ngood++ ;
     }
   }
   if( ngood == 0 ) EXRETURN ;

   /*--------------- add to the 2D and 1D histograms ---------------*/

   xyclip = (xxbot < xcbot) && (xcbot < xctop) && (xctop < xxtop) &&
            (yybot < ycbot) && (ycbot < yctop) && (yctop < yytop) ;

#ifndef nthr
   nthr    = omp_get_max_threads() ;
   use_omp = (nthr > 1) ;
#endif

   if( !use_omp ){  /*** serial code ***/

     /* AFNI_do_nothing() ; fprintf(stderr,"h") ; */

     if( !xyclip ){  /*------------ equal size bins ------------*/

       float xb,xi , yb,yi , xx,yy , x1,y1 , ww ;
       int jj,kk ;

       xb = xxbot ; xi = nbm/(xxtop-xxbot) ;
       yb = yybot ; yi = nbm/(yytop-yybot) ;
       for( ii=0 ; ii < n ; ii++ ){
         if( !good[ii] ) continue ;
         xx = (x[ii]-xb)*xi ;
         jj = (int)xx ; xx = xx - jj ; x1 = 1.0f-xx ;
         yy = (y[ii]-yb)*yi ;
         kk = (int)yy ; yy = yy - kk ; y1 = 1.0f-yy ;
         ww = WW(ii) ; nww += ww ;

         if( jj < 0 || kk < 0 || jj >= nbin || kk >= nbin ) continue ;

         xc[jj] += (x1*ww); xc[jj+1] += (xx*ww);
         yc[kk] += (y1*ww); yc[kk+1] += (yy*ww);

         XYC(jj  ,kk  ) += x1*(y1*ww) ;
         XYC(jj+1,kk  ) += xx*(y1*ww) ;
         XYC(jj  ,kk+1) += x1*(yy*ww) ;
         XYC(jj+1,kk+1) += xx*(yy*ww) ;
       }

     } else if( xyclip ){  /*------------ mostly equal bins ----------------*/

       int jj,kk ;
       float xbc=xcbot , xtc=xctop , ybc=ycbot , ytc=yctop ;
       float xi,yi , xx,yy , x1,y1 , ww ;

       AFNI_do_nothing() ; /* fprintf(stderr,"c") ; */

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

         if( jj < 0 || kk < 0 || jj >= nbin || kk >= nbin ) continue ;

         x1 = 1.0f-xx ; y1 = 1.0f-yy ; ww = WW(ii) ; nww += ww ;

         xc[jj] += (x1*ww); xc[jj+1] += (xx*ww);
         yc[kk] += (y1*ww); yc[kk+1] += (yy*ww);

         XYC(jj  ,kk  ) += x1*(y1*ww) ;
         XYC(jj+1,kk  ) += xx*(y1*ww) ;
         XYC(jj  ,kk+1) += x1*(yy*ww) ;
         XYC(jj+1,kk+1) += xx*(yy*ww) ;
       }

       AFNI_do_nothing() ; /* fprintf(stderr,".") ; */

     } /* end of clipped code */

   } else {  /*** parallelized using OpenMP ***/

     float **xccar , **yccar , **xyccar , *nwwar ; int nbpq=nbp*nbp,itt ;

     xccar  = (float **)calloc(sizeof(float *),nthr) ;  /* arrays for   */
     yccar  = (float **)calloc(sizeof(float *),nthr) ;  /* accumulation */
     xyccar = (float **)calloc(sizeof(float *),nthr) ;  /* in separate  */
     nwwar  = (float * )calloc(sizeof(float)  ,nthr) ;  /* threads      */

     for( itt=0 ; itt < nthr ; itt++ ){
       xccar [itt] = (float *)calloc(sizeof(float),nbp ) ;
       yccar [itt] = (float *)calloc(sizeof(float),nbp ) ;
       xyccar[itt] = (float *)calloc(sizeof(float),nbpq) ;
     }

     AFNI_do_nothing() ; fprintf(stderr,"H") ;

#undef  XYCC
#define XYCC(p,q) xycc[(p)+(q)*nbp]

     if( !xyclip ){  /*------------ equal size bins ------------*/

     AFNI_do_nothing() ; fprintf(stderr,"y") ;

 AFNI_OMP_START ;
#pragma omp parallel  /*** start of parallel code ***/
 {
       float *xcc, *ycc , *xycc ;
       float xb,xi , yb,yi , xx,yy , x1,y1 , ww ;
       int ii,jj,kk , ithr ;

       ithr = omp_get_thread_num() ;
#pragma omp barrier
       fprintf(stderr,"%d",ithr) ;
       xcc = xccar[ithr] ; ycc = yccar[ithr] ; xycc = xyccar[ithr] ;
       xb = xxbot ; xi = nbm/(xxtop-xxbot) ;
       yb = yybot ; yi = nbm/(yytop-yybot) ;
#pragma omp for
       for( ii=0 ; ii < n ; ii++ ){
         if( !good[ii] ) continue ;
         xx = (x[ii]-xb)*xi ;
         jj = (int)xx ; xx = xx-jj ; x1 = 1.0f-xx ;
         yy = (y[ii]-yb)*yi ;
         kk = (int)yy ; yy = yy-kk ; y1 = 1.0f-yy ;
         ww = WW(ii) ; nwwar[ithr] += ww ;

         if( jj < 0 || kk < 0 || jj >= nbin || kk >= nbin ) continue ;

         xcc[jj] += (x1*ww); xcc[jj+1] += (xx*ww);
         ycc[kk] += (y1*ww); ycc[kk+1] += (yy*ww);

         XYCC(jj  ,kk  ) += x1*(y1*ww) ;
         XYCC(jj+1,kk  ) += xx*(y1*ww) ;
         XYCC(jj  ,kk+1) += x1*(yy*ww) ;
         XYCC(jj+1,kk+1) += xx*(yy*ww) ;
       }
 }  /*** end of parallel code ***/
 AFNI_OMP_END ;

     } else if( xyclip ){  /*------------ mostly equal bins ----------------*/

     AFNI_do_nothing() ; fprintf(stderr,"x") ;

 AFNI_OMP_START ;
#pragma omp parallel  /*** start of parallel code ***/
 {
       float *xcc, *ycc , *xycc ;
       int ii,jj,kk , ithr ;
       float xbc=xcbot , xtc=xctop , ybc=ycbot , ytc=yctop ;
       float xi,yi , xx,yy , x1,y1 , ww ;

       ithr = omp_get_thread_num() ;
       fprintf(stderr,"%d",ithr) ;
#pragma omp barrier
       fprintf(stderr,":") ;
       xcc = xccar[ithr] ; ycc = yccar[ithr] ; xycc = xyccar[ithr] ;
       xi = (nbin-2.000001f)/(xtc-xbc) ;
       yi = (nbin-2.000001f)/(ytc-ybc) ;
#pragma omp for
       for( ii=0 ; ii < n ; ii++ ){
         if( !good[ii] ) continue ;
         xx = x[ii] ;
              if( xx < xbc ){ jj = 0   ; xx = 0.0f ; }
         else if( xx > xtc ){ jj = nbm ; xx = 1.0f ; }
         else               { xx = 1.0f+(xx-xbc)*xi; jj = (int)xx; xx = xx-jj; }
         yy = y[ii] ;
              if( yy < ybc ){ kk = 0   ; yy = 0.0f ; }
         else if( yy > ytc ){ kk = nbm ; yy = 1.0f ; }
         else               { yy = 1.0f+(yy-ybc)*yi; kk = (int)yy; yy = yy-kk; }

         if( jj < 0 || kk < 0 || jj >= nbin || kk >= nbin ) continue ;

         x1 = 1.0f-xx ; y1 = 1.0f-yy ; ww = WW(ii) ; nwwar[ithr] += ww ;

         xcc[jj] += (x1*ww); xcc[jj+1] += (xx*ww);
         ycc[kk] += (y1*ww); ycc[kk+1] += (yy*ww);

         XYCC(jj  ,kk  ) += x1*(y1*ww) ;
         XYCC(jj+1,kk  ) += xx*(y1*ww) ;
         XYCC(jj  ,kk+1) += x1*(yy*ww) ;
         XYCC(jj+1,kk+1) += xx*(yy*ww) ;
       }
#pragma omp barrier
       fprintf(stderr,"%d",ithr) ;
 }  /*** end of parallel code ***/
 AFNI_OMP_END ;

     }  /*-- end of mostly equal bins --*/

     /* now merge the parallel thread results */

     for( itt=0 ; itt < nthr ; itt++ ){
       if( nwwar[itt] > 0.0f ){
         nww += nwwar[itt] ;
         for( ii=0 ; ii < nbp ; ii++ ){ xc[ii] += xccar[itt][ii]; yc[ii] += yccar[itt][ii]; }
         for( ii=0 ; ii < nbpq ; ii++ ){ xyc[ii] += xyccar[itt][ii] ; }
       }
       free(xccar[itt]) ; free(yccar[itt]) ; free(xyccar[itt]) ;
     }
     free(xccar) ; free(yccar) ; free(xyccar) ; free(nwwar) ;

   } /* end of using OpenMP */

   /* AFNI_do_nothing() ; fprintf(stderr,".") ; */

   tdh->nww = nww ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* scale histogram to have sum==1 */

void INCOR_normalize_2Dhist( INCOR_2Dhist *tdh )
{
   float nww , *xc, *yc, *xyc ; int nbp ;
   if( tdh == NULL ) return ;
   nww = tdh->nww; xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;
   if( nww > 0.0f && nww != 1.0f && xyc != NULL && xc != NULL && yc != NULL ){
     float ni ; int nbq , ii ;
     ni = 1.0f / nww ;
     for( ii=0 ; ii < nbp ; ii++ ){ xc[ii]  *= ni; yc[ii] *= ni; }
     nbq = nbp*nbp ;
     for( ii=0 ; ii < nbq ; ii++ ){ xyc[ii] *= ni; }
   }
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the mutual info from a histogram (which also normalizes it).
----------------------------------------------------------------------------*/

double INCOR_mutual_info( INCOR_2Dhist *tdh )
{
   int ii,jj ;
   double val , nww ;
   float *xc, *yc, *xyc ; int nbp ;

   if( tdh == NULL ) return 0.0 ;

   nww = (double)tdh->nww; if( nww <= 0.0 ) return 0.0 ;
   xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;

   INCOR_normalize_2Dhist(tdh) ;

   /*-- compute MI from histogram --*/

   val = 0.0 ;
   for( ii=0 ; ii < nbp ; ii++ ){
    for( jj=0 ; jj < nbp ; jj++ ){
     if( XYC(ii,jj) > 0.0f )
      val += XYC(ii,jj) * log( XYC(ii,jj)/(xc[ii]*yc[jj]) ) ;
   }}
   return (1.4427*val) ;  /* units are bits, just for fun */
}

/*--------------------------------------------------------------------------*/
/*! Compute the normalized mutual info from a 2D histogram.
    Actually, returns H(x,y) / [ H(x)+H(y) ], which should be small if
    x and y are redundant and should be large if they are independent.
----------------------------------------------------------------------------*/

double INCOR_norm_mutinf( INCOR_2Dhist *tdh )
{
   int ii,jj ;
   double numer , denom , nww ;
   float *xc, *yc, *xyc ; int nbp ;

   if( tdh == NULL ) return 0.0 ;

   nww = (double)tdh->nww; if( nww <= 0.0 ) return 0.0 ;
   xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;

   INCOR_normalize_2Dhist(tdh) ;

   /*-- compute NMI from histogram --*/

   denom = numer = 0.0 ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0f ) denom += xc[ii] * log( xc[ii] ) ;
     if( yc[ii] > 0.0f ) denom += yc[ii] * log( yc[ii] ) ;
     for( jj=0 ; jj < nbp ; jj++ ){
       if( XYC(ii,jj) > 0.0f ) numer += XYC(ii,jj) * log( XYC(ii,jj) );
     }
   }
   if( denom != 0.0 ) denom = numer / denom ;
   return denom ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the correlation ratio from a 2D histogram.
----------------------------------------------------------------------------*/

double INCOR_corr_ratio( INCOR_2Dhist *tdh , int crmode )
{
   int ii,jj ;
   double vv,mm , val , cyvar , uyvar , yrat,xrat , nww ;
   float *xc, *yc, *xyc ; int nbp ;

   if( tdh == NULL ) return 0.0 ;

   nww = (double)tdh->nww; if( nww <= 0.0 ) return 0.0 ;
   xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;

   INCOR_normalize_2Dhist(tdh) ;

   /*-- compute CR(y|x) from histogram --*/

   cyvar = 0.0 ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0f ){
       vv = mm = 0.0 ;                /* mm=E(y|x)  vv=E(y^2|x) */
       for( jj=1 ; jj < nbp ; jj++ ){
         mm += (jj * XYC(ii,jj)) ; vv += jj * (jj * XYC(ii,jj)) ;
       }
       cyvar += (vv - mm*mm/xc[ii] ) ; /* Var(y|x) */
     }
   }
   vv = mm = uyvar = 0.0 ;
   for( jj=1 ; jj < nbp ; jj++ ){        /* mm=E(y)  vv=E(y^2) */
     mm += (jj * yc[jj]) ; vv += jj * (jj * yc[jj]) ;
   }
   uyvar = vv - mm*mm ;                  /* Var(y) */
   yrat  = (uyvar > 0.0 ) ? cyvar/uyvar  /* Var(y|x) / Var(y) */
                          : 1.0  ;

   if( crmode == 0 ) return (1.0-yrat) ;   /** unsymmetric **/

   /** compute CR(x|y) also, for symmetrization **/

   cyvar = 0.0 ;
   for( jj=0 ; jj < nbp ; jj++ ){
     if( yc[jj] > 0.0 ){
       vv = mm = 0.0 ;                /* mm=E(x|y)  vv=E(x^2|y) */
       for( ii=1 ; ii < nbp ; ii++ ){
         mm += (ii * XYC(ii,jj)) ; vv += ii * (ii * XYC(ii,jj)) ;
       }
       cyvar += (vv - mm*mm/yc[jj] ) ; /* Var(x|y) */
     }
   }
   vv = mm = uyvar = 0.0 ;
   for( ii=1 ; ii < nbp ; ii++ ){     /* mm=E(x)  vv=E(x^2) */
     mm += (ii * xc[ii]) ; vv += ii * (ii * xc[ii]) ;
   }
   uyvar = vv - mm*mm ;                 /* Var(x) */
   xrat  = (uyvar > 0.0) ? cyvar/uyvar  /* Var(x|y) / Var(x) */
                         : 1.0 ;

   if( crmode == 2 ) return (1.0 - 0.5*(xrat+yrat)) ; /** additive **/

   return (1.0 - xrat*yrat) ;                          /** multiplicative **/
}

/*--------------------------------------------------------------------------*/
/*! Compute the Hellinger metric from a 2D histogram.
----------------------------------------------------------------------------*/

double INCOR_hellinger( INCOR_2Dhist *tdh )
{
   int ii,jj ;
   double val , pq , nww ;
   float *xc, *yc, *xyc ; int nbp ;

   if( tdh == NULL ) return 0.0 ;

   nww = (double)tdh->nww; if( nww <= 0.0 ) return 0.0 ;
   xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;

   INCOR_normalize_2Dhist(tdh) ;

   /*-- compute Hell metric from histogram --*/

   val = 0.0 ;
   for( ii=0 ; ii < nbp ; ii++ ){
    for( jj=0 ; jj < nbp ; jj++ ){
     pq = XYC(ii,jj) ;
     if( pq > 0.0 ) val += sqrt( pq * xc[ii] * yc[jj] ) ;
   }}
   return (1.0-val) ;
}

/*--------------------------------------------------------------------------*/
/*! Compute the Hellinger metric, mutual info, normalized MI, and
    (additively) symmetrized correlation ratio, and return all 4
    (in that order), from a 2D histogram.

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

double_quad INCOR_helmicra( INCOR_2Dhist *tdh )
{
   int ii,jj ;
   double hel , pq , vv,uu , nww ;
   double val , cyvar , uyvar , yrat,xrat ;
   double_quad hmc = {0.0,0.0,0.0,0.0} ;
   float *xc, *yc, *xyc ; int nbp ;

   if( tdh == NULL ) return hmc ;

   nww = (double)tdh->nww; if( nww <= 0.0 ) return hmc ;
   xc = tdh->xc; yc = tdh->yc; xyc = tdh->xyc; nbp = tdh->nbin+1;

   INCOR_normalize_2Dhist(tdh) ;

   /*-- compute Hel, MI, NMI from histogram --*/

   hel = vv = uu = 0.0 ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0 ) vv += xc[ii] * log( xc[ii] ) ;
     if( yc[ii] > 0.0 ) vv += yc[ii] * log( yc[ii] ) ;
     for( jj=0 ; jj < nbp ; jj++ ){
       pq = XYC(ii,jj) ;
       if( pq > 0.0 ){
         hel += sqrt( pq * xc[ii] * yc[jj] ) ;
         uu  += pq * log( pq );
       }
     }
   }
   hmc.a = 1.0 - hel ;                  /* Hellinger */
   hmc.b = uu - vv ;                    /* MI */
   hmc.c = (vv != 0.0) ? uu/vv : 0.0 ;  /* NMI */

   /*-- compute CR(y|x) from histogram --*/

   cyvar = 0.0 ;
   for( ii=0 ; ii < nbp ; ii++ ){
     if( xc[ii] > 0.0 ){
       vv = uu = 0.0 ;               /* uu=E(y|x)  vv=E(y^2|x) */
       for( jj=1 ; jj < nbp ; jj++ ){
         uu += (jj * XYC(ii,jj)) ; vv += jj * (jj * XYC(ii,jj)) ;
       }
       cyvar += (vv - uu*uu/xc[ii] ) ; /* Var(y|x) */
     }
   }
   vv = uu = uyvar = 0.0 ;
   for( jj=1 ; jj < nbp ; jj++ ){     /* uu=E(y)  vv=E(y^2) */
     uu += (jj * yc[jj]) ; vv += jj * (jj * yc[jj]) ;
   }
   uyvar = vv - uu*uu ;                  /* Var(y) */
   yrat  = (uyvar > 0.0) ? cyvar/uyvar   /* Var(y|x) / Var(y) */
                         : 1.0 ;

   /** compute CR(x|y) also, for symmetrization **/

   cyvar = 0.0 ;
   for( jj=0 ; jj < nbp ; jj++ ){
     if( yc[jj] > 0.0 ){
       vv = uu = 0.0 ;               /* uu=E(x|y)  vv=E(x^2|y) */
       for( ii=1 ; ii < nbp ; ii++ ){
         uu += (ii * XYC(ii,jj)) ; vv += ii * (ii * XYC(ii,jj)) ;
       }
       cyvar += (vv - uu*uu/yc[jj] ) ; /* Var(x|y) */
     }
   }
   vv = uu = uyvar = 0.0 ;
   for( ii=1 ; ii < nbp ; ii++ ){     /* uu=E(x)  vv=E(x^2) */
     uu += (ii * xc[ii]) ; vv += ii * (ii * xc[ii]) ;
   }
   uyvar = vv - uu*uu ;                 /* Var(x) */
   xrat  = (uyvar > 0.0) ? cyvar/uyvar  /* Var(x|y) / Var(x) */
                         : 1.0 ;

   hmc.d = 1.0 - 0.5*(xrat+yrat) ; /** additive symmetrization **/
   return hmc ;
}

/*----------------------------------------------------------------------------*/

static double hpow = 0.3333333333321 ;
void INCOR_set_2Dhist_hpower( double hh )
{
  hpow = (hh > 0.0 && hh < 1.0) ? hh : 0.3333333333321 ;
}

static int nhbin = 0 ;
void INCOR_set_2Dhist_hbin( int nn ){ nhbin = nn; }

int INCOR_2Dhist_compute_nbin( int ndata )
{
   int nbin ;

   nbin = (nhbin > 4) ? nhbin : (int)rint(pow((double)ndata,hpow)) ;
   if( nbin > 255 ) nbin = 255 ; else if( nbin < 5 ) nbin = 5 ;
   return nbin ;
}

/*----------------------------------------------------------------------------*/

INCOR_2Dhist * INCOR_create_2Dhist( int nbin ,
                                    float xbot , float xtop ,
                                    float ybot , float ytop ,
                                    float xcbot, float xctop,
                                    float ycbot, float yctop  )
{
   INCOR_2Dhist *tdh ; int nbp ;

ENTRY("INCOR_create_2Dhist") ;

   if( nbin < 3 ) nbin = 3 ;

   tdh = (INCOR_2Dhist *)calloc(1,sizeof(INCOR_2Dhist)) ;

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

void INCOR_destroy_2Dhist( INCOR_2Dhist *tin )
{
   if( tin == NULL ) return ;
   if( tin->xc  != NULL ) free(tin->xc) ;
   if( tin->yc  != NULL ) free(tin->yc) ;
   if( tin->xyc != NULL ) free(tin->xyc) ;
   free(tin) ;
   return ;
}

/*----------------------------------------------------------------------------*/

void INCOR_copyover_2Dhist( INCOR_2Dhist *tin , INCOR_2Dhist *tout )
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

   AAmemcpy( tout->xc , tin->xc , sizeof(float)*nbp ) ;
   AAmemcpy( tout->yc , tin->yc , sizeof(float)*nbp ) ;
   AAmemcpy( tout->xyc, tin->xyc, sizeof(float)*nbp*nbp ) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

void INCOR_addto_incomplete_pearson( int n, float *x, float *y,
                                            float *w, INCOR_pearson *inpear )
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
       ww = (double)w[ii] ;
       if( ww > 0.0 ){
         xx = (double)x[ii] ; yy = (double)y[ii] ;
         sx += xx*ww ; sxx += xx*xx*ww ;
         sy += yy*ww ; syy += yy*yy*ww ; sxy += xx*yy*ww ; sw += ww ;
       }
     }
   }

   inpear->npt += n ;
   inpear->sx   = sx ; inpear->sxx = sxx ;
   inpear->sy   = sy ; inpear->syy = syy ; inpear->sxy = sxy ; inpear->sw = sw ;

   return ;
}

/*----------------------------------------------------------------------------*/

void INCOR_destroy_incomplete_pearson( INCOR_pearson *inpear )
{
   if( inpear != NULL ) free((void *)inpear) ;
}

/*----------------------------------------------------------------------------*/

INCOR_pearson * INCOR_create_incomplete_pearson(void)
{
   INCOR_pearson *inpear ;

   inpear = (INCOR_pearson *)calloc(1,sizeof(INCOR_pearson)) ;
   inpear->sx  = 0.0 ; inpear->sxx = 0.0 ;
   inpear->sy  = 0.0 ; inpear->syy = 0.0 ;
   inpear->sxy = 0.0 ; inpear->sw  = 0.0 ; inpear->npt = 0 ;

   inpear->meth = GA_MATCH_PEARSON_SCALAR ;
   return inpear ;
}

/*----------------------------------------------------------------------------*/

double INCOR_incomplete_pearson( INCOR_pearson *inpear )
{
   double xv , yv , xy , swi , val ;

   if( inpear->sw <= 0.0 ) return 0.0 ;

   swi = 1.0 / inpear->sw ;

   xv = inpear->sxx - inpear->sx * inpear->sx * swi ;
   yv = inpear->syy - inpear->sy * inpear->sy * swi ;
   xy = inpear->sxy - inpear->sx * inpear->sy * swi ;

   if( xv <= 0.0 || yv <= 0.0 ) return 0.0 ;
   val = (xy/sqrt(xv*yv)) ; val = MYatanh(val) ; return val ;
}

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

void INCOR_addto_incomplete_pearclp_SS( int n, float *x, float *y,
                                        float *w, INCOR_pearclp *inpear )
{
   int ii ; double sx,sxx , sy,syy,sxy , sw ;
   double xcb,xct , ycb,yct , xmid,ymid ;
   double xdb,xdt , ydb,ydt ;

   sx = inpear->sx ; sxx = inpear->sxx ;
   sy = inpear->sy ; syy = inpear->syy ; sxy = inpear->sxy ; sw = inpear->sw ;

   xcb = inpear->xcbot ; xct = inpear->xctop ; xmid = 0.5f*(xcb+xct) ;
   ycb = inpear->ycbot ; yct = inpear->yctop ; ymid = 0.5f*(ycb+yct) ;
   xdb = inpear->xdbot ; xdt = inpear->xdtop ;
   ydb = inpear->ydbot ; ydt = inpear->ydtop ;

   if( w == NULL ){
     double xx , yy , ww ; int cl ;
     for( ii=0 ; ii < n ; ii++ ){
       cl = 1 ;
       xx = (double)x[ii] ;
       if( xx <= xcb ){ xx = xdb; cl++; } else if( xx >= xct ){ xx = xdt; cl++; }
       yy = (double)y[ii] ;
       if( yy <= ycb ){ yy = ydb; cl++; } else if( yy >= yct ){ yy = ydt; cl++; }
       ww = 1.0 / cl ; xx -= xmid ; yy -= ymid ;
       sx += xx*ww ; sxx += xx*xx*ww ; sy += yy*ww ; syy += yy*yy*ww ; sxy += xx*yy*ww ; sw += ww ;
     }
   } else {
     double xx , yy , ww ; int cl ;
     for( ii=0 ; ii < n ; ii++ ){
       ww = (double)w[ii] ;
       if( ww > 0.0 ){
         cl = 1 ;
         xx = (double)x[ii] ;
         if( xx <= xcb ){ xx = xdb; cl++; } else if( xx >= xct ){ xx = xdt; cl++; }
         yy = (double)y[ii] ;
         if( yy <= ycb ){ yy = ydb; cl++; } else if( yy >= yct ){ yy = ydt; cl++; }
         ww /= cl ; xx -= xmid ; yy -= ymid ;
         sx += xx*ww ; sxx += xx*xx*ww ;
         sy += yy*ww ; syy += yy*yy*ww ; sxy += xx*yy*ww ; sw += ww ;
       }
     }
   }

   inpear->npt += n ;
   inpear->sx   = sx ; inpear->sxx = sxx ;
   inpear->sy   = sy ; inpear->syy = syy ; inpear->sxy = sxy ; inpear->sw = sw ;

   return ;
}

/*----------------------------------------------------------------------------*/
#ifdef USE_OMP /*--- Parallel-ized verison of the above ----------------------*/

void INCOR_addto_incomplete_pearclp_PP( int n, float *x, float *y,
                                        float *w, INCOR_pearclp *inpear )
{
   int jj ; double sx,sxx , sy,syy,sxy , sw ;
   double xcb,xct , ycb,yct , xmid,ymid ;
   double xdb,xdt , ydb,ydt ;

   xcb = inpear->xcbot ; xct = inpear->xctop ; xmid = 0.5f*(xcb+xct) ;
   ycb = inpear->ycbot ; yct = inpear->yctop ; ymid = 0.5f*(ycb+yct) ;
   xdb = inpear->xdbot ; xdt = inpear->xdtop ;
   ydb = inpear->ydbot ; ydt = inpear->ydtop ;

   for( jj=0 ; jj < nthmax ; jj++ ){
     dhaar[jj] = dhbbr[jj] = dhccr[jj] = dhddr[jj] = dheer[jj] = dhffr[jj] = 0.0 ;
   }

   if( w == NULL ){

#pragma omp parallel
     { double xx , yy , ww ; int cl,ii ; int ith=omp_get_thread_num() ;
       double tx=0.0,txx=0.0,ty=0.0,tyy=0.0,txy=0.0,tw=0.0 ;
#pragma omp for
       for( ii=0 ; ii < n ; ii++ ){
         cl = 1 ;
         xx = (double)x[ii] ;
         if( xx <= xcb ){ xx = xdb; cl++; } else if( xx >= xct ){ xx = xdt; cl++; }
         yy = (double)y[ii] ;
         if( yy <= ycb ){ yy = ydb; cl++; } else if( yy >= yct ){ yy = ydt; cl++; }
         ww = 1.0 / cl ; xx -= xmid ; yy -= ymid ;
         tx  += ww*xx    ; txx += ww*xx*xx ; ty += ww*yy ;
         tyy += ww*yy*yy ; txy += ww*xx*yy ; tw += ww ;
       }
#pragma omp critical
       { dhaar[ith] = tx  ; dhbbr[ith] = txx ; dhccr[ith] = ty ;
         dhddr[ith] = tyy ; dheer[ith] = txy ; dhffr[ith] = tw ; }
     } /* end parallel */

   } else {

#pragma omp parallel
     { double xx , yy , ww ; int cl,ii ; int ith=omp_get_thread_num() ;
       double tx=0.0,txx=0.0,ty=0.0,tyy=0.0,txy=0.0,tw=0.0 ;
#pragma omp for
       for( ii=0 ; ii < n ; ii++ ){
         ww = (double)w[ii] ;
         if( ww > 0.0 ){
           cl = 1 ;
           xx = (double)x[ii] ;
           if( xx <= xcb ){ xx = xdb; cl++; } else if( xx >= xct ){ xx = xdt; cl++; }
           yy = (double)y[ii] ;
           if( yy <= ycb ){ yy = ydb; cl++; } else if( yy >= yct ){ yy = ydt; cl++; }
           ww /= cl ; xx -= xmid ; yy -= ymid ;
           tx  += ww*xx    ; txx += ww*xx*xx ; ty += ww*yy ;
           tyy += ww*yy*yy ; txy += ww*xx*yy ; tw += ww ;
         }
       }
#pragma omp critical
       { dhaar[ith] = tx  ; dhbbr[ith] = txx ; dhccr[ith] = ty ;
         dhddr[ith] = tyy ; dheer[ith] = txy ; dhffr[ith] = tw ; }
     } /* end parallel */
   }

   /*-- add partial sums from each thread to the results --*/

   sx  = inpear->sx  ; sxx = inpear->sxx ;
   sy  = inpear->sy  ; syy = inpear->syy ;
   sxy = inpear->sxy ; sw  = inpear->sw  ;
   for( jj=0 ; jj < nthmax ; jj++ ){
     sx  += dhaar[jj] ; sxx += dhbbr[jj] ;
     sy  += dhccr[jj] ; syy += dhddr[jj] ;
     sxy += dheer[jj] ; sw  += dhffr[jj] ;
   }

   inpear->npt += n ;
   inpear->sx   = sx ; inpear->sxx = sxx ;
   inpear->sy   = sy ; inpear->syy = syy ; inpear->sxy = sxy ; inpear->sw = sw ;

   return ;
}
#endif

/*----------------------------------------------------------------------------*/

void INCOR_addto_incomplete_pearclp( int n, float *x, float *y,
                                     float *w, INCOR_pearclp *inpear )
{
   if( n <= 0 || x == NULL || y == NULL || inpear == NULL ) return ;
#ifdef USE_OMP
   if( nthmax > 1 && n >= 333 ){
     INCOR_addto_incomplete_pearclp_PP( n , x,y,w , inpear ) ;
     return ;
   }
#endif
   INCOR_addto_incomplete_pearclp_SS( n , x,y,w , inpear ) ;
   return ;
}

/*----------------------------------------------------------------------------*/

void INCOR_destroy_incomplete_pearclp( INCOR_pearclp *inpear )
{
   if( inpear != NULL ) free((void *)inpear) ;
}

/*----------------------------------------------------------------------------*/

INCOR_pearclp * INCOR_create_incomplete_pearclp(void)
{
   INCOR_pearclp *inpear ;

   inpear = (INCOR_pearclp *)calloc(1,sizeof(INCOR_pearclp)) ;
   inpear->sx  = 0.0 ; inpear->sxx = 0.0 ;
   inpear->sy  = 0.0 ; inpear->syy = 0.0 ;
   inpear->sxy = 0.0 ; inpear->sw  = 0.0 ; inpear->npt = 0 ;

   inpear->xcbot = inpear->xctop = inpear->ycbot = inpear->yctop = 0.0 ;
   inpear->xdbot = inpear->xdtop = inpear->ydbot = inpear->ydtop = 0.0 ;

   inpear->meth = GA_MATCH_PEARCLP_SCALAR ;
   return inpear ;
}

/*----------------------------------------------------------------------------*/

double INCOR_incomplete_pearclp( INCOR_pearclp *inpear )
{
   double xv , yv , xy , swi , val ;

   if( inpear->sw <= 0.0 ) return 0.0 ;

   swi = 1.0 / inpear->sw ;

   xv = inpear->sxx - inpear->sx * inpear->sx * swi ;
   yv = inpear->syy - inpear->sy * inpear->sy * swi ;
   xy = inpear->sxy - inpear->sx * inpear->sy * swi ;

   if( xv <= 0.0 || yv <= 0.0 ) return 0.0 ;
   val = (xy/sqrt(xv*yv)) ; val = MYatanh(val) ; return val ;
}

/*============================================================================*/
/* Generic INCOR functions */
/*============================================================================*/

/*----------------------------------------------------------------------------*/
/* Check if a method code is implemented. */

int INCOR_check_meth_code( int meth )
{
  switch( meth ){
     case GA_MATCH_PEARSON_SCALAR:    return 1 ;

     case GA_MATCH_PEARCLP_SCALAR:    return 3 ;

     case GA_MATCH_MUTINFO_SCALAR:
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_NORMUTIN_SCALAR:
     case GA_MATCH_HELLINGER_SCALAR:
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CRAT_USYM_SCALAR:  return 2 ;  /* uses 2Dhist */
  }

  return 0 ;
}

/*----------------------------------------------------------------------------*/
/* Create an INCOR object, return a pointer to it.
     meth = method to use
     mpar = floatvec with parameters:

       For meth == GA_MATCH_PEARSON_SCALAR, mpar is not used.

       For meth == 2D histogram method, mpar is used as follows:
         mpar->ar[0] = nbin
         mpar->ar[1] = xbot  mpar->ar[2] = xtop
         mpar->ar[3] = ybot  mpar->ar[4] = ytop
         mpar->ar[5] = xcbot mpar->ar[6] = xctop
         mpar->ar[7] = ycbot mpar->ar[8] = yctop
         * If you have a good idea of how many data points are coming in,
           you can use function INCOR_2Dhist_compute_nbin() to compute
           a useful nbin from the number of data points.
         * If xbot >= xtop or if ybot >= ytop, then these values will be
           computed from the first set of data input (and then fixed).
         * If we have the relationships
                 xbot < xcbot < xctop < xtop
             AND ybot < ycbot < yctop < ytop
           then the first and last bins are 'oddly' sized, and all the interior
           bins are evenly spaced.  Otherwise, all the bins are evenly spaced.
         * If you have some sample data to supply, you can use function
           INCOR_2Dhist_minmax() to compute xbot, xtop, ybot, and ytop.
           In addition, you can use function INCOR_2Dhist_xyclip() to compute
           xcbot, xctop, ycbot, and yctop.

      At this time, the local correlation methods are not supported by INCOR.
*//*--------------------------------------------------------------------------*/

void * INCOR_create( int meth , floatvec *mpar )
{
   void *vinc = NULL;

ENTRY("INCOR_create") ;

   switch( meth ){

     case GA_MATCH_PEARSON_SCALAR:
       vinc = (void *)INCOR_create_incomplete_pearson() ;
     break ;

     case GA_MATCH_PEARCLP_SCALAR:{
       INCOR_pearclp *pc ;
       pc = INCOR_create_incomplete_pearclp() ; vinc = (void *)pc ;
       if( mpar != NULL && mpar->nar > 8 ){
         pc->xdbot = mpar->ar[1] ; pc->xdtop = mpar->ar[2] ;
         pc->ydbot = mpar->ar[3] ; pc->ydtop = mpar->ar[4] ;
         pc->xcbot = mpar->ar[5] ; pc->xctop = mpar->ar[6] ;
         pc->ycbot = mpar->ar[7] ; pc->yctop = mpar->ar[8] ;
       }
     }
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
       nbin = (mpar == NULL) ? 0 : (int)mpar->ar[0] ;
       if( nbin < 0 ) nbin = INCOR_2Dhist_compute_nbin(-nbin) ;
       if( nbin > 0 && mpar != NULL && mpar->nar > 8 ){
         xbot  = mpar->ar[1] ; xtop  = mpar->ar[2] ;
         ybot  = mpar->ar[3] ; ytop  = mpar->ar[4] ;
         xcbot = mpar->ar[5] ; xctop = mpar->ar[6] ;
         ycbot = mpar->ar[7] ; yctop = mpar->ar[8] ;
       } else {
         xbot = xtop = ybot = ytop = xcbot = xctop = ycbot = yctop = 0.0f ;
       }
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
/* Erase an INCOR struct from the Macrocosmic All. */

void INCOR_destroy( void *vp )
{
ENTRY("INCOR_destroy") ;

   if( vp == NULL ) EXRETURN ;

   switch( INCOR_methcode(vp) ){

     case GA_MATCH_PEARSON_SCALAR:
       INCOR_destroy_incomplete_pearson(vp) ;
     break ;

     case GA_MATCH_PEARCLP_SCALAR:
       INCOR_destroy_incomplete_pearclp(vp) ;
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
/* Copy the internal data of an INCOR struct 'vin' over that of 'vout'.
   This is used for adding data to vin while keeping vout pristine for re-use.
   If vin is NULL, then vout is cleared to the empty INCOR struct.
*//*--------------------------------------------------------------------------*/

void INCOR_copyover( void *vin , void *vout )
{
   int meth ;

ENTRY("INCOR_copyover") ;

   if( vout == NULL || vin == vout ) EXRETURN ;

   if( vin != NULL ) meth = INCOR_methcode(vin) ;
   else              meth = INCOR_methcode(vout) ;

   switch( meth ){

     case GA_MATCH_PEARSON_SCALAR:
       if( vin != NULL ){
         AAmemcpy( vout , vin , sizeof(INCOR_pearson) ) ;
       } else {
         INCOR_pearson *vp = (INCOR_pearson *)vout ;
         vp->sx  = 0.0 ; vp->sxx = 0.0 ;
         vp->sy  = 0.0 ; vp->syy = 0.0 ;
         vp->sxy = 0.0 ; vp->sw  = 0.0 ; vp->npt = 0 ;
       }
     break ;

     case GA_MATCH_PEARCLP_SCALAR:
       if( vin != NULL ){
         AAmemcpy( vout , vin , sizeof(INCOR_pearclp) ) ;
       } else {
         INCOR_pearclp *vp = (INCOR_pearclp *)vout ;
         vp->sx  = 0.0 ; vp->sxx = 0.0 ;
         vp->sy  = 0.0 ; vp->syy = 0.0 ;
         vp->sxy = 0.0 ; vp->sw  = 0.0 ; vp->npt = 0 ;
         vp->xcbot = vp->xctop = vp->ycbot = vp->yctop = 0.0 ;
         vp->xdbot = vp->xdtop = vp->ydbot = vp->ydtop = 0.0 ;
       }
     break ;

     case GA_MATCH_MUTINFO_SCALAR:
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_NORMUTIN_SCALAR:
     case GA_MATCH_HELLINGER_SCALAR:
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CRAT_USYM_SCALAR:
       if( vin != NULL ){
         INCOR_copyover_2Dhist( vin , vout ) ;
       } else {
         INCOR_2Dhist *tdh=(INCOR_2Dhist *)vout ; int nbp=1+tdh->nbin ;
         AAmemset(tdh->xc ,0,sizeof(float)*nbp) ;
         AAmemset(tdh->yc ,0,sizeof(float)*nbp) ;
         AAmemset(tdh->xyc,0,sizeof(float)*nbp*nbp) ;
         tdh->nww = 0.0f ;
       }
     break ;

   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Add data to an INCOR struct, for use in computing the 'correlation' later. */

void INCOR_addto( void *vin , int n , float *x , float *y , float *w )
{
ENTRY("INCOR_addto") ;

   if( vin == NULL || n <= 0 || x == NULL || y == NULL ) EXRETURN ;

   switch( INCOR_methcode(vin) ){

     case GA_MATCH_PEARSON_SCALAR:
       INCOR_addto_incomplete_pearson( n , x , y , w , vin ) ;
     break ;

     case GA_MATCH_PEARCLP_SCALAR:
       INCOR_addto_incomplete_pearclp( n , x , y , w , vin ) ;
     break ;

     case GA_MATCH_MUTINFO_SCALAR:
     case GA_MATCH_CORRATIO_SCALAR:
     case GA_MATCH_NORMUTIN_SCALAR:
     case GA_MATCH_HELLINGER_SCALAR:
     case GA_MATCH_CRAT_SADD_SCALAR:
     case GA_MATCH_CRAT_USYM_SCALAR:
       INCOR_addto_2Dhist( vin , n , x , y, w ) ;
     break ;

   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Evaluate the value of an INCOR struct, as currently constituted.
   * The data specified in arguments {n,x,y,w} is used (if n > 0), but is not
     added into the vin INCOR struct.
   * This feature lets you do correlations with a base set of unchanging
     data, plus a set of data that changes, and thence let you optimize the
     correlation as that varying data is munged around.
*//*--------------------------------------------------------------------------*/

double INCOR_evaluate( void *vin , int n , float *x , float *y , float *w )
{
   void *vtmp ; double val=0.0 ;

ENTRY("INCOR_evaluate") ;

   if( vin == NULL ) RETURN(val) ;

   vtmp = INCOR_create( INCOR_methcode(vin) , NULL ) ;
   INCOR_copyover( vin , vtmp ) ;
   INCOR_addto( vtmp , n , x , y , w ) ;

   switch( INCOR_methcode(vtmp) ){
     case GA_MATCH_PEARSON_SCALAR:   val = INCOR_incomplete_pearson(vtmp); break;
     case GA_MATCH_PEARCLP_SCALAR:   val = INCOR_incomplete_pearclp(vtmp); break;
     case GA_MATCH_MUTINFO_SCALAR:   val = INCOR_mutual_info(vtmp) ;       break;
     case GA_MATCH_NORMUTIN_SCALAR:  val = INCOR_norm_mutinf(vtmp) ;       break;
     case GA_MATCH_HELLINGER_SCALAR: val = INCOR_hellinger(vtmp) ;         break;
     case GA_MATCH_CORRATIO_SCALAR:  val = INCOR_corr_ratio(vtmp,0) ;      break;
     case GA_MATCH_CRAT_SADD_SCALAR: val = INCOR_corr_ratio(vtmp,2) ;      break;
     case GA_MATCH_CRAT_USYM_SCALAR: val = INCOR_corr_ratio(vtmp,1) ;      break;
   }

   INCOR_destroy(vtmp) ;
   RETURN(val) ;
}
