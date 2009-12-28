/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*-------------------------------------------------------------------------*/

void THD_const_detrend( int npt, float *xx, float *xx0 ) /* 24 Aug 2001 */
{
   int ii ; float xbar ;

   if( npt < 2 || xx == NULL ) return ;

   xbar = 0.0 ;
   for( ii=0 ; ii < npt ; ii++ ) xbar += xx[ii] ;
   xbar /= npt ;
   for( ii=0 ; ii < npt ; ii++ ) xx[ii] -= xbar ;
   if( xx0 != NULL ) *xx0 = xbar ;
   return ;
}

/*-------------------------------------------------------------------------*/

void get_linear_trend( int npt, float *xx, float *f0, float *f1 )
{
   double t1,t3,t10 , x0,x1 ;
   int ii ;

   if( npt < 2 || xx == NULL || f0 == NULL || f1 == NULL ) return ;

   x0 = xx[0] ; x1 = 0.0 ;
   for( ii=1 ; ii < npt ; ii++ ){
      x0 += xx[ii] ;
      x1 += xx[ii] * ii ;
   }

   t1 = npt*x0; t3 = 1.0/npt; t10 = npt*npt;

   *f0 = (float)(2.0/(npt+1.0)*t3*(2.0*t1-3.0*x1-x0));
   *f1 = (float)(-6.0/(t10-1.0)*t3*(-x0-2.0*x1+t1));
   return ;
}

/*-------------------------------------------------------------------------
   Linear detrend a 1D float array in place.
   If xx0 != NULL and xx1 != NULL, then the trend removed is
      far[i] -= (*xx0) + (*xx1) * i, for i=0..npt-1
---------------------------------------------------------------------------*/

void THD_linear_detrend( int npt, float *far, float *xx0, float *xx1 )
{
   register int ii ;
   float f0 , f1 ;

   if( npt < 3 || far == NULL ) return ;

   get_linear_trend( npt , far , &f0 , &f1 ) ;

   far[0] -= f0 ;
   for( ii=1 ; ii < npt ; ii++ ) far[ii] -= (f0 + f1*ii) ;

   if( xx0 != NULL ) *xx0 = f0 ;
   if( xx1 != NULL ) *xx1 = f1 ;

   return ;
}

/*---------------------------------------------------------------------------*/

void THD_linear_detrend_complex( int npt , complex *cx )  /* 05 Mar 2007 */
{
   register float *f ; register int ii ;

   if( npt < 3 || cx == NULL ) return ;

   f = malloc(sizeof(float)*npt) ;
   for( ii=0 ; ii < npt ; ii++ ) f[ii] = cx[ii].r ;
   THD_linear_detrend( npt , f , NULL,NULL ) ;
   for( ii=0 ; ii < npt ; ii++ ){
     cx[ii].r = f[ii] ; f[ii] = cx[ii].i ;
   }
   THD_linear_detrend( npt , f , NULL,NULL ) ;
   for( ii=0 ; ii < npt ; ii++ ) cx[ii].i = f[ii] ;
   return ;
}

/*---------------------------------------------------------------------------
   Given x[0..npt-1], return f0,f1,f2 as the least squares coefficients to
     x[j] = f0 + f1*j + f2*j*j
-----------------------------------------------------------------------------*/

void get_quadratic_trend( int npt, float *xx, float *f0, float *f1, float *f2 )
{
   register double x0,x1,x2 ; double N=npt ;
   register int ii ;

   if( npt < 3 || xx == NULL || f0 == NULL || f1 == NULL || f2 == NULL ) return;

   x0 = xx[0] ; x1 = x2 = 0.0 ;
   for( ii=1 ; ii < npt ; ii++ ){
     x0 +=  xx[ii] ;
     x1 += (xx[ii] * ii) ;
     x2 += (xx[ii] * ii) * ii ;
   }

   *f0 = (  3.0*(3.0*N*N-3.0*N+2.0) * x0
          -18.0*(-1.0+2.0*N)        * x1
          +30.0                     * x2 ) / (N*(N+2.0)*(N+1.0)) ;

   *f1 = ( -18.0*(-1.0+2.0*N)              * x0
           +12.0*(-1.0+2.0*N)*(8.0*N-11.0) * x1 /((N-1.0)*(N-2.0))
           -180.0                          * x2 /(N-2.0)          )
        / (N*(N+2.0)*(N+1.0)) ;

   *f2 = ( 30.0  * x0
          -180.0 * x1 / (N-2.0)
          +180.0 * x2 / ((N-1.0)*(N-2.0)) ) / (N*(N+2.0)*(N+1.0))  ;
   return ;
}

/*-------------------------------------------------------------------------
   Quadratic detrend a 1D float array in place.
   If xx0 != NULL, xx1 != NULL, xx2 != NULL, then the trend removed is
      far[i] -= (*xx0) + (*xx1) * i + (*xx2)*(i*i) , for i=0..npt-1
---------------------------------------------------------------------------*/

void THD_quadratic_detrend( int npt, float *far,
                            float *xx0, float *xx1, float *xx2 )
{
   register int ii ;
   float f0 , f1 , f2 ;

   if( npt < 4 || far == NULL ) return ;

   get_quadratic_trend( npt , far , &f0 , &f1 , &f2 ) ;

   far[0] -= f0 ;
   for( ii=1 ; ii < npt ; ii++ ) far[ii] -= ( (f2*ii + f1)*ii + f0 ) ;

   if( xx0 != NULL ) *xx0 = f0 ;
   if( xx1 != NULL ) *xx1 = f1 ;
   if( xx2 != NULL ) *xx2 = f2 ;

   return ;
}

/*------------------------------------------------------------------------------*/
/*! Cubic detrend a float array in place. */

void THD_cubic_detrend( int npt , float *far )  /* 15 Nov 1999 */
{
   register int ii ;
   float g0,g1,g2,g3 , f0,f1,f2,f3 , t1,t2,t5,t8 , t95,t56,t22,t25,txx ;

   if( npt < 5 || far == NULL ) return ;

   t8 = npt*npt ; t2 = npt-1.0 ; t5 = t2*(npt-2.0) ;
   t95 = 0.05*t5*(npt-3.0) ;
   t56 = 0.16666667*t5 ;
   t22 = 0.5*t2 ;
   t25 = 1.5*t2 ;
   txx = 0.6*t8-1.5*npt+1.1 ;

   g0=g1=g2=g3=0.0 ;
   for( ii=0 ; ii < npt ; ii++ ){
      t1 = ii*ii ;
      f1 = ii - t22 ;
      f2 = t1 - t2*ii + t56 ;
      f3 = t1*(ii - t25) + txx*ii - t95 ;

      g0 += far[ii] ;
      g1 += far[ii] * f1 ;
      g2 += far[ii] * f2 ;
      g3 += far[ii] * f3 ;
   }
   g0 *= (1.0/npt) ;
   g1 *= 12.0/(npt*(t8-1.0)) ;
   g2 *= 180.0/(npt*(t8-1.0)*(t8-4.0)) ;
   g3 *= 2800.0/(npt*(t8-1.0)*(t8-4.0)*(t8-9.0)) ;

   for( ii=0 ; ii < npt ; ii++ ){
      t1 = ii*ii ;
      f1 = ii- t22 ;
      f2 = t1 - t2*ii + t56 ;
      f3 = t1*(ii - t25) + txx*ii - t95 ;

      far[ii] -= ( g0 + g1*f1 + g2*f2 + g3*f3 ) ;
   }

   return ;
}

/*-------------------------------------------------------------------------
   Make a vector have L2 norm 1
---------------------------------------------------------------------------*/

float THD_normalize( int npt , float *far )
{
   register int ii ;
   register float fac ;

   if( npt <= 0 || far == NULL ) return 0.0f ;

   fac = 0.0f ;
   for( ii=0 ; ii < npt ; ii++ ) fac += far[ii]*far[ii] ;
   if( fac == 0.0f ) return 0.0f ;
   fac = 1.0f / sqrtf(fac) ;
   for( ii=0 ; ii < npt ; ii++ ) far[ii] *= fac ;
   return fac ;
}

/*-------------------------------------------------------------------------
   Make a vector have RMS value = 1
---------------------------------------------------------------------------*/

void THD_normRMS( int npt , float *far )
{
   register int ii ;
   register float fac ;

   if( npt <= 0 || far == NULL ) return ;

   fac = 0.0f ;
   for( ii=0 ; ii < npt ; ii++ ) fac += far[ii]*far[ii] ;
   if( fac == 0.0f ) return ;
   fac = 1.0f / sqrtf(fac/npt) ;
   for( ii=0 ; ii < npt ; ii++ ) far[ii] *= fac ;
   return ;
}

/*-----------------------------------------------------------------------*/
/* Make a vector have max abs 1 [26 Mar 2008]. */

void THD_normmax( int npt , float *far )
{
   register int ii ;
   register float fac , val ;

   if( npt <= 0 || far == NULL ) return ;
   fac = 0.0f ;
   for( ii=0 ; ii < npt ; ii++ ){ val = fabsf(far[ii]); fac = MAX(fac,val); }
   if( fac == 0.0f ) return ;
   fac = 1.0f / fac ;
   for( ii=0 ; ii < npt ; ii++ ) far[ii] *= fac ;
   return ;
}

/*-----------------------------------------------------------------------*/
/* Make a vector have max sum |far| = 1 [26 Mar 2008]. */

void THD_normL1( int npt , float *far )
{
   register int ii ;
   register float fac , val ;

   if( npt <= 0 || far == NULL ) return ;
   fac = 0.0f ;
   for( ii=0 ; ii < npt ; ii++ ){ val = fabsf(far[ii]); fac += val; }
   if( fac == 0.0f ) return ;
   fac = 1.0f / fac ;
   for( ii=0 ; ii < npt ; ii++ ) far[ii] *= fac ;
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Detrend a vector with a given polort level, plus some others, using
    least squares regression.
     - npt    = length of vector
     - far    = vector of data
     - polort = polynomial order (-1..3)
     - nort   = number of extra orts in ort[] (can be 0)
     - ort    = array of extra time series to detrend:
                 ort[j][i] for j=0..nort-1, i=0..npt-1
     - fit    = array of length nref=polort+nort+1 to hold parameters of fit
                 (can be NULL)
-------------------------------------------------------------------------*/

void THD_generic_detrend_LSQ( int npt, float *far ,
                              int polort, int nort, float **ort , float *fit )
{
   int ii,jj , nref ;
   float **ref , *qfit , xmid , xfac , val ;

   /* check inputs */

   if( npt <= 1 || far == NULL ) return ;
   if( nort > 0 ){
     if( ort == NULL ) return ;
     for( jj=0 ; jj < nort ; jj++ ) if( ort[jj] == NULL ) return ;
   }
   if( polort <  0 ) polort = -1 ;
   if( nort   <  0 ) nort   =  0 ;

   nref = polort+1+nort ;
   if( nref == 0 || nref >= npt-1 ) return ;

   /* assemble all reference vectors */

   ref  = (float **) malloc( sizeof(float *) * nref ) ;
   xmid = 0.5*(npt-1) ; xfac = 1.0 / xmid ;
   for( jj=0 ; jj <= polort ; jj++ ){
     ref[jj] = (float *) malloc( sizeof(float) * npt ) ;
#if 1  /* the new way */
     for( ii=0 ; ii < npt ; ii++ )
       ref[jj][ii] = (float)Plegendre(xfac*(ii-xmid),jj) ;
#else  /* the olden way */
     switch( jj ){
       case 0:
         for( ii=0 ; ii < npt ; ii++ ) ref[jj][ii] = 1.0 ;
       break ;

       case 1:
         for( ii=0 ; ii < npt ; ii++ ) ref[jj][ii] = xfac*(ii-xmid) ;
       break ;

       case 2:
         for( ii=0 ; ii < npt ; ii++ ){
           val = xfac*(ii-xmid) ; ref[jj][ii] = val*val ;
         }
       break ;

       case 3:
         for( ii=0 ; ii < npt ; ii++ ){
           val = xfac*(ii-xmid) ; ref[jj][ii] = val*val*val ;
         }
       break ;

       default:
         for( ii=0 ; ii < npt ; ii++ ){
           val = xfac*(ii-xmid) ;
           ref[jj][ii] = pow(val,(double)(jj)) ;
         }
       break ;
     }
#endif
   }
   for( jj=0 ; jj < nort ; jj++ )   /* user supplied refs */
     ref[polort+1+jj] = ort[jj] ;

   qfit = lsqfit( npt , far , NULL , nref , ref ) ;

   if( qfit != NULL ){                                  /* good */
     for( ii=0 ; ii < npt ; ii++ ){
       val = far[ii] ;
       for( jj=0 ; jj < nref ; jj++ ) val -= qfit[jj] * ref[jj][ii] ;
       far[ii] = val ;
     }
     if( fit != NULL ){ for( ii=0 ; ii < nref ; ii++ ) fit[ii] = qfit[ii] ; }
     free(qfit) ;
   } else {
     ERROR_message("THD_generic_detrend_LSQ: fit fails - no detrending!") ;
     if( fit != NULL ) memset(fit,0,sizeof(float)*nref) ;
   }

   for( jj=0 ; jj <= polort ; jj++ ) free(ref[jj]) ;
   free(ref) ; return ;
}

/*-----------------------------------------------------------------------*/
/*! Detrend a vector with a given polort level, plus some others, using
    L1 regression.
     - npt    = length of vector
     - far    = vector of data
     - polort = polynomial order (-1..3)
     - nort   = number of extra orts in ort[] (can be 0)
     - ort    = array of extra time series to detrend:
                 ort[j][i] for j=0..nort-1, i=0..npt-1
     - fit    = array of length nref=polort+nort+1 to hold parameters of fit
                 (can be NULL)
-------------------------------------------------------------------------*/

void THD_generic_detrend_L1( int npt, float *far ,
                             int polort, int nort, float **ort , float *fit )
{
   int ii,jj , nref ;
   float **ref , *qfit , xmid , xfac , val ;

   /* check inputs */

   if( npt <= 1 || far == NULL ) return ;
   if( nort > 0 ){
     if( ort == NULL ) return ;
     for( jj=0 ; jj < nort ; jj++ ) if( ort[jj] == NULL ) return ;
   }
   if( polort <  0 ) polort = -1 ;
   if( nort   <  0 ) nort   =  0 ;

   nref = polort+1+nort ;
   if( nref == 0 || nref >= npt-1 ) return ;

   /* assemble all reference vectors */

   ref  = (float **) malloc( sizeof(float *) * nref ) ;
   xmid = 0.5*(npt-1) ; xfac = 1.0 / xmid ;
   for( jj=0 ; jj <= polort ; jj++ ){
     ref[jj] = (float *) malloc( sizeof(float) * npt ) ;
     switch( jj ){
       case 0:
         for( ii=0 ; ii < npt ; ii++ ) ref[jj][ii] = 1.0 ;
       break ;

       case 1:
         for( ii=0 ; ii < npt ; ii++ ) ref[jj][ii] = xfac*(ii-xmid) ;
       break ;

       case 2:
         for( ii=0 ; ii < npt ; ii++ ){
           val = xfac*(ii-xmid) ; ref[jj][ii] = val*val ;
         }
       break ;

       case 3:
         for( ii=0 ; ii < npt ; ii++ ){
           val = xfac*(ii-xmid) ; ref[jj][ii] = val*val*val ;
         }
       break ;

       default:
         for( ii=0 ; ii < npt ; ii++ ){
           val = xfac*(ii-xmid) ; ref[jj][ii] = pow(val,(double)(jj)) ;
         }
       break ;
     }
   }
   for( jj=0 ; jj < nort ; jj++ )   /* user supplied refs */
     ref[polort+1+jj] = ort[jj] ;

   qfit = (float *)malloc(sizeof(float)*nref) ;
   val = cl1_solve( npt , nref , far , ref , qfit , 0 ) ;

   if( val >= 0.0f ){                                  /* good */
     for( ii=0 ; ii < npt ; ii++ ){
       val = far[ii] ;
       for( jj=0 ; jj < nref ; jj++ ) val -= qfit[jj] * ref[jj][ii] ;
       far[ii] = val ;
     }
     if( fit != NULL ){ for( ii=0 ; ii < nref ; ii++ ) fit[ii] = qfit[ii] ; }
   } else {
     ERROR_message("THD_generic_detrend_L1: fit fails - no detrending!") ;
     if( fit != NULL ) memset(fit,0,sizeof(float)*nref) ;
   }
   free(qfit) ;

   for( jj=0 ; jj <= polort ; jj++ ) free(ref[jj]) ;
   free(ref) ; return ;
}

/*-----------------------------------------------------------------------*/
/* Add the trend back into far; fit must not be NULL!
-------------------------------------------------------------------------*/

void THD_generic_retrend( int npt , float *far ,
                          int polort, int nort, float **ort , float *fit )
{
   int ii,jj , nref ;
   float **ref , xmid , xfac , val ;

   /* check inputs */

   if( npt <= 1 || far == NULL || fit == NULL ) return ;
   if( nort > 0 ){
     if( ort == NULL ) return ;
     for( jj=0 ; jj < nort ; jj++ ) if( ort[jj] == NULL ) return ;
   }
   if( polort <  0 ) polort = -1 ;
   if( nort   <  0 ) nort   =  0 ;

   nref = polort+1+nort ;
   if( nref == 0 || nref >= npt-1 ) return ;

   /* assemble all reference vectors */

   ref  = (float **) malloc( sizeof(float *) * nref ) ;
   xmid = 0.5*(npt-1) ; xfac = 1.0 / xmid ;
   for( jj=0 ; jj <= polort ; jj++ ){
     ref[jj] = (float *) malloc( sizeof(float) * npt ) ;
     switch( jj ){
       case 0:
         for( ii=0 ; ii < npt ; ii++ ) ref[jj][ii] = 1.0 ;
       break ;

       case 1:
         for( ii=0 ; ii < npt ; ii++ ) ref[jj][ii] = xfac*(ii-xmid) ;
       break ;

       case 2:
         for( ii=0 ; ii < npt ; ii++ ){
           val = xfac*(ii-xmid) ; ref[jj][ii] = val*val ;
         }
       break ;

       case 3:
         for( ii=0 ; ii < npt ; ii++ ){
           val = xfac*(ii-xmid) ; ref[jj][ii] = val*val*val ;
         }
       break ;

       default:
         for( ii=0 ; ii < npt ; ii++ ){
           val = xfac*(ii-xmid) ; ref[jj][ii] = pow(val,(double)(jj)) ;
         }
       break ;
     }
   }
   for( jj=0 ; jj < nort ; jj++ )   /* user supplied refs */
     ref[polort+1+jj] = ort[jj] ;

   for( ii=0 ; ii < npt ; ii++ ){
     val = far[ii] ;
     for( jj=0 ; jj < nref ; jj++ ) val += fit[jj] * ref[jj][ii] ;
     far[ii] = val ;
   }

   for( jj=0 ; jj <= polort ; jj++ ) free(ref[jj]) ;
   free(ref) ; return ;
}

/*----------------------------------------------------------------------------*/
/*! Returns nref reference functions. */

float ** THD_build_polyref( int nref , int nvals )  /* 20 Sep 2007 */
{
   int jj,iv,kk ;
   float **ref ;
   double fac , x ;

ENTRY("THD_build_polyref") ;

   if( nref < 1 || nvals <= nref ){
     ERROR_message("THD_build_polyref: nref=%d  nvals=%d",nref,nvals) ;
     RETURN(NULL) ;
   }

   fac = 2.0 / (nvals-1.0) ;
   ref = (float **)malloc(sizeof(float *)*nref) ;
   for( jj=0 ; jj < nref ; jj++ ){
     ref[jj] = (float *) malloc( sizeof(float) * nvals ) ;
     for( kk=0 ; kk < nvals ; kk++ ){
       x = fac * kk - 1.0 ; ref[jj][kk] = (float)Plegendre( x , jj ) ;
     }
   }

   RETURN(ref) ;
}

/*----------------------------------------------------------------------------*/
/*! Returns 2*corder+3 reference functions. */

float ** THD_build_trigref( int corder , int nvals )
{
   int nref=2*corder+3 , jj,iv,kk ;
   float **ref , tm,fac,fq ;

ENTRY("THD_build_trigref") ;

   if( corder < 0 || nvals <= nref ){
     ERROR_message("THD_build_trigref: corder=%d  nvals=%d",corder,nvals) ;
     RETURN(NULL) ;
   }

   ref = (float **)malloc(sizeof(float *)*nref) ;
   for( jj=0 ; jj < nref ; jj++ )
     ref[jj] = (float *) malloc( sizeof(float) * nvals ) ;

   /* ref[0]: r(t) = 1 */
   for( iv=0 ; iv < nvals ; iv++ ) ref[0][iv] = 1.0f ;

   /* ref[1]: r(t) = t - tmid */
   tm = 0.5f * (nvals-1.0f) ; fac = 2.0f / nvals ;
   for( iv=0 ; iv < nvals ; iv++ ) ref[1][iv] = (iv-tm)*fac ;

   /* ref[2]: r(t) = (t-tmid)**2 */
   fac = fac*fac ;
   for( iv=0 ; iv < nvals ; iv++ ) ref[2][iv] = (iv-tm)*(iv-tm)*fac ;

   /* higher order ref[jj]: */
   for( jj=3,kk=1 ; kk <= corder ; kk++ ){
     fq = (2.0*PI*kk)/nvals ;

     /* r(t) = sin(2*PI*k*t/N) */
     for( iv=0 ; iv < nvals ; iv++ ) ref[jj][iv] = sin(fq*iv) ;
     jj++ ;

     /* r(t) = cos(2*PI*k*t/N) */
     for( iv=0 ; iv < nvals ; iv++ ) ref[jj][iv] = cos(fq*iv) ;
     jj++ ;
   }

   RETURN(ref) ;
}

/*----------------------------------------------------------------------------*/
 #undef  INMASK                                     /* ZSS: Changed from    */ 
 #define INMASK(i) ((mask == NULL || mask[i]))      /*       mask != NULL   */
/*----------------------------------------------------------------------------*/
/* Fits each voxel time series to a linear model:

                 nref-1
     z[i][t] = SUM     ref[q][t] * fit[q][i]
                 q=0

   where
    - i = voxel index      0..DSET_NVOX(dset)-1
    - t = time index       0..DSET_NVALS(dset)-1
    - q = reference index  0..nref-1
    - Fitting is done in the L-p sense, where p=meth (1 or 2).
    - The dataset itself is not modified.
    _ Return value is the nref fit images, plus 1 extra image that is
      the MAD of the residuals for meth=1 and the standard deviation for meth=2.
    - In float format, of course.
    - If NULL is returned, something bad happened.  Be afraid.
    - Also see function THD_medmad_bricks() for something simpler & similar.
------------------------------------------------------------------------------*/

MRI_IMARR * THD_time_fit_dataset( THD_3dim_dataset *dset ,
                                  int nref , float **ref , int meth , byte *mask )
{
   int ii , nvox,nval , qq,tt ;
   float *far , *fit , *var , val ;
   MRI_IMARR *imar ; MRI_IMAGE *qim ; float **fitar ;

ENTRY("THD_time_fit_dataset") ;

   if( !ISVALID_DSET(dset) ||
       nref < 1 || nref >= DSET_NVALS(dset) || ref == NULL ) RETURN(NULL) ;
   DSET_load(dset) ; if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   /* construct output images */

   INIT_IMARR(imar) ;
   fitar = (float **)malloc(sizeof(float *)*nref) ;
   for( qq=0 ; qq < nref ; qq++ ){
     qim = mri_new_conforming( DSET_BRICK(dset,0) , MRI_float ) ;
     fitar[qq] = MRI_FLOAT_PTR(qim) ;
     ADDTO_IMARR(imar,qim) ;
   }
   qim = mri_new_conforming( DSET_BRICK(dset,0) , MRI_float ) ;
   var = MRI_FLOAT_PTR(qim) ; ADDTO_IMARR(imar,qim) ;

   nvox = DSET_NVOX(dset) ; nval = DSET_NVALS(dset) ;
   far  = (float *)malloc(sizeof(float)*nval) ;
   fit  = (float *)malloc(sizeof(float)*nref) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( !INMASK(ii) ) continue ;
     qq = THD_extract_array( ii , dset , 0 , far ) ;  /* get data */
     if( qq == 0 ){
       switch(meth){                                  /* get fit */
         default:
         case 2: THD_generic_detrend_LSQ( nval, far, -1, nref,ref, fit ); break;
         case 1: THD_generic_detrend_L1 ( nval, far, -1, nref,ref, fit ); break;
       }
       for( qq=0 ; qq < nref ; qq++ ) fitar[qq][ii] = fit[qq] ; /* save fit */

       /* at this point, far[] contains the residuals */

       switch(meth){                    /* get stdev or MAD */
         default:
         case 2:{
           register float mm,vv ;
           for( mm=0.0,tt=0 ; tt < nval ; tt++ ) mm += far[tt] ;
           mm /= nval ;
           for( vv=0.0,tt=0 ; tt < nval ; tt++ ) vv += (far[tt]-mm)*(far[tt]-mm) ;
           var[ii] = sqrtf( vv/(nval-1.0) ) ;
         }
         break ;

         case 1:{
           for( tt=0 ; tt < nval ; tt++ ) far[tt] = fabsf(far[tt]) ;
           var[ii] = qmed_float( nval , far ) ;
         }
         break ;
       }
     }
   }

   free(fit); free(far); free(fitar); RETURN(imar);
}

/*----------------------------------------------------------------------------*/
/* Get the time series data array at voxel #ii, detrended by the fit
   from THD_time_fit_dataset().  If scl != 0, also scale the result
   by the reciprocal of the stdev or MAD (last sub-brick in imar).
------------------------------------------------------------------------------*/

void THD_extract_detrended_array( THD_3dim_dataset *dset ,
                                  int nref, float **ref, MRI_IMARR *imar,
                                  int ii, int scl, float *far )
{
   int tt , nval , qq ;
   float val , **fitar , *var ;
   MRI_IMAGE *qim ;

ENTRY("THD_extract_detrended_array") ;

   if( !ISVALID_DSET(dset) ||
       nref < 1            || ref == NULL                ||
       imar == NULL        || IMARR_COUNT(imar) < nref+1 ||
       ii < 0              || ii >= DSET_NVOX(dset)      || far == NULL ) EXRETURN ;

   qq = THD_extract_array( ii , dset , 0 , far ) ;  /* get data */
   if( qq < 0 ) EXRETURN ;
   nval = DSET_NVALS(dset) ;

   fitar = (float **)malloc(sizeof(float *)*nref) ;
   for( qq=0 ; qq < nref ; qq++ ){
     qim = IMARR_SUBIM(imar,qq) ; fitar[qq] = MRI_FLOAT_PTR(qim) ;
   }
   qim = IMARR_SUBIM(imar,nref) ; var = MRI_FLOAT_PTR(qim) ;

   for( tt=0 ; tt < nval ; tt++ ){  /* get residuals */
     val = far[tt] ;
     for( qq=0 ; qq < nref ; qq++ ) val -= ref[qq][tt] * fitar[qq][ii] ;
     far[tt] = val ;
   }

   if( scl && var[ii] > 0.0f ){
     val = 1.0f / var[ii] ;
     for( tt=0 ; tt < nval ; tt++ ) far[tt] *= val ;
   }
   
   /* ZSS: Need to free fitar */
   free(fitar); fitar=NULL;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Detrend a dataset:
    - dset  = input time series dataset (nvals long)
    - nref  = number of ref vectors
    - ref   = reference vectors [0..nref-1][0..nvals-1]
    - meth  = 1 or 2 (for L_p fitting, where p=meth)
    - scl   = if 1, scale data by reciprocal of MAD or stdev (meth=1 or 2)
    - mask  = if not NULL, byte mask; voxel outside the mask are set to 0
    - imar  = if not NULL, coefficient images from THD_time_fit_dataset()

  Output is in float format, of course.  If NULL is returned, something bad
  transpired, and your mother will get an e-mail explaining how stupid
  you were.
------------------------------------------------------------------------------*/

THD_3dim_dataset * THD_detrend_dataset( THD_3dim_dataset *dset ,
                                        int nref , float **ref ,
                                        int meth , int scl ,
                                        byte *mask , MRI_IMARR **imar )
{
   MRI_IMARR *qmar ;
   int ii,jj,kk , nvals,nvox , iv ;
   float *var ;
   THD_3dim_dataset *newset ;

ENTRY("THD_detrend_dataset") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   nvals = DSET_NVALS(dset) ; nvox = DSET_NVOX(dset) ;

   qmar = THD_time_fit_dataset( dset , nref,ref , meth , mask ) ;
   if( qmar == NULL ) RETURN(NULL) ;

   newset = EDIT_empty_copy(dset) ;
   for( iv=0 ; iv < nvals ; iv++ ){
     EDIT_substitute_brick( newset , iv , MRI_float , NULL ) ;
     EDIT_BRICK_FACTOR( newset , iv , 0.0f ) ;  /* 04 Jun 2007 */
   }

   var = (float *)malloc(sizeof(float)*nvals) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( mask == NULL || mask[ii] )
       THD_extract_detrended_array( dset , nref,ref , qmar , ii,scl , var ) ;
     else
       memset(var,0,sizeof(float)*nvals) ;
     THD_insert_series( ii , newset , nvals , MRI_float , var , 0 ) ;
   }

   free(var) ;
   if( imar != NULL ) *imar = qmar ;
   else               DESTROY_IMARR(qmar) ;

   RETURN(newset) ;
}

/*----------------------------------------------------------------------------*/
/* Can be used as a sort-of inverse to THD_detrend_dataset() */

int THD_retrend_dataset( THD_3dim_dataset *dset ,
                         int nref , float **ref ,
                         int scl , byte *mask , MRI_IMARR *imar )
{
   int ii,qq,tt , nvals,nvox ;
   MRI_IMAGE *qim ; float **fitar , *far , fac , *var , val ;

ENTRY("THD_retrend_dataset") ;

   if( !ISVALID_DSET(dset) ||
       nref < 1            || ref == NULL ||
       imar == NULL        || IMARR_COUNT(imar) <= nref ) RETURN(0) ;

   nvals = DSET_NVALS(dset) ; nvox = DSET_NVOX(dset) ;

   fitar = (float **)malloc(sizeof(float *)*nref) ;
   for( qq=0 ; qq < nref ; qq++ ){
     qim = IMARR_SUBIM(imar,qq) ; fitar[qq] = MRI_FLOAT_PTR(qim) ;
   }
   qim = IMARR_SUBIM(imar,nref) ; var = MRI_FLOAT_PTR(qim) ;

   far  = (float *)malloc(sizeof(float)*nvals) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     if( mask != NULL && !mask[ii] ) continue ;
     qq = THD_extract_array( ii , dset , 0 , far ) ;  /* get data */
     if( qq < 0 ) continue ;
     fac = (scl) ? var[ii] : 1.0f ;
     for( tt=0 ; tt < nvals ; tt++ ){  /* add fit back in */
       val = far[tt] * fac ;
       for( qq=0 ; qq < nref ; qq++ ) val += ref[qq][tt] * fitar[qq][ii] ;
       far[tt] = val ;
     }
     THD_insert_series( ii , dset , nvals , MRI_float , far , 0 ) ;
   }

   free(far) ; free(fitar) ; RETURN(1) ;
}
