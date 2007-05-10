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
   double  x0,x1,x2 , N=npt ;
   int ii ;

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

void THD_normalize( int npt , float *far )
{
   register int ii ;
   register float fac ;

   if( npt <= 0 || far == NULL ) return ;

   fac = 0 ;
   for( ii=0 ; ii < npt ; ii++ ) fac += far[ii]*far[ii] ;
   if( fac == 0.0 ) return ;
   fac = 1.0 / sqrt(fac) ;
   for( ii=0 ; ii < npt ; ii++ ) far[ii] /= fac ;
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

   qfit = lsqfit( npt , far , NULL , nref , ref ) ;

   if( qfit != NULL ){                                  /* good */
     for( ii=0 ; ii < npt ; ii++ ){
       val = far[ii] ;
       for( jj=0 ; jj < nref ; jj++ ) val -= qfit[jj] * ref[jj][ii] ;
       far[ii] = val ;
     }
     if( fit != NULL ) memcpy(fit,qfit,sizeof(float)*nref) ;
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
     if( fit != NULL ) memcpy(fit,qfit,sizeof(float)*nref) ;
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

/*-----------------------------------------------------------------------*/

MRI_IMARR * THD_time_fit_dataset( THD_3dim_dataset *dset ,
                                  int nref , float **ref , int meth )
{
   int ii , nvox,nval , qq ;
   float *far , *fit ;
   MRI_IMARR *imar ; MRI_IMAGE *qim ; float **fitar ;

ENTRY("THD_time_fit_dataset") ;

   if( !ISVALID_DSET(dset) || nref < 1 || ref == NULL ) RETURN(NULL) ;
   DSET_load(dset) ; if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   INIT_IMARR(imar) ;
   fitar = (float **)malloc(sizeof(float *)*nref) ;
   for( qq=0 ; qq < nref ; qq++ ){
     qim = mri_new_conforming( DSET_BRICK(dset,0) , MRI_float ) ;
     fitar[qq] = MRI_FLOAT_PTR(qim) ;
     ADDTO_IMARR(imar,qim) ;
   }

   nvox = DSET_NVOX(dset) ; nval = DSET_NVALS(dset) ;
   far  = (float *)malloc(sizeof(float)*nval) ;
   fit  = (float *)malloc(sizeof(float)*nref) ;
   for( ii=0 ; ii < nvox ; ii++ ){
     qq = THD_extract_array( ii , dset , 0 , far ) ;
     if( qq == 0 ){
       switch(meth){
         default:
         case 2:
           THD_generic_detrend_LSQ( nval , far , -1 , nref,ref , fit ) ;
         break ;

         case 1:
           THD_generic_detrend_L1 ( nval , far , -1 , nref,ref , fit ) ;
         break ;
       }
       for( qq=0 ; qq < nref ; qq++ ) fitar[qq][ii] = fit[qq] ;
     }
   }

   free(fit); free(far); free(fitar); RETURN(imar);
}
