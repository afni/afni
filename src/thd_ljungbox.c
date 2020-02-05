#include "mrilib.h"

/*----------------------------------------------------------------------------*/
/* Compute the Ljung-Box statistic on uneven (censored) data.
   The true time index of the i-th point is tau[i].
   However, if tau==NULL, code asssumes tau[i] = i (no censoring).
   See the next function ljung_box_zcens() if you are interested
   in censoring via input val[i] = 0.0 instead of supplying tau.
     -- RWCox - 21 Jan 2020
*//*--------------------------------------------------------------------------*/

double ljung_box_uneven( int nval , int hh , double *val , int *tau )
{
   int kk , jj , jtop , *nj , dj ;
   double sum0 , gsum , ck , *sumk ;

   /* check for irrational inputs */

   if( nval < 10 || val == NULL ) return 0.0 ;

   if( hh < 2 || hh > nval/2 ){
     int h1 = nval/8 , h2 = (int)rintf(3.0f*logf((float)nval)) ;
     hh = 2+MIN(h1,h2) ; if( hh > nval/2 ) hh = nval/2 ;
   }

   /* compute denominator sum */

   sum0 = 0.0 ;
   for( jj=0 ; jj < nval ; jj++ ){
     sum0 += val[jj]*val[jj] ;
   }
   if( sum0 < 1.e-10 ) return 0.0 ;                   /* all zero input? */

   /* allocate memory for storing sums and counts */

   sumk = (double *)calloc(sizeof(double),(size_t)(hh+1)) ; /* init to 0 */
   nj   = (int *)   calloc(sizeof(int)   ,(size_t)(hh+1)) ;

   /* for each index difference kk */

   for( kk=1 ; kk <= hh ; kk++ ){
     jtop = nval-kk ;
     if( tau != NULL ){                        /* 'time' is given by tau */
       for( jj=0 ; jj < jtop ; jj++ ){
         dj = tau[jj+kk] - tau[jj] ;
         if( dj > 0 && dj <= hh ){
           sumk[dj] += val[jj]*val[jj+kk] ; nj[dj]++ ;
         }
       }
     } else {                                /* 'time' is given by index */
       for( jj=0 ; jj < jtop ; jj++ ){
         sumk[kk] += val[jj]*val[jj+kk] ; nj[kk]++ ;
       }
     }
   }

   gsum = 0.0 ;
   for( kk=1 ; kk <= hh ; kk++ ){
     if( nj[kk] > 1 ){
       ck    = sumk[kk] / sum0 ;
       gsum += ck*ck / nj[kk] ;
     }
   }
   gsum *= nval*(nval+2.0) ;
   if( gsum > 1.e+10 ) gsum = 1.e+10 ;             /* in case of trubble */

   free(nj) ; free(sumk) ;
   return gsum ;
}

/*----------------------------------------------------------------------------*/
/* Similar to above, but exclude (censor) exactly 0 values. */
/*----------------------------------------------------------------------------*/

double ljung_box_zcens( int nval , int hh , double *val )
{
   int jj , nnz , *tau ;
   double *vnz , gg ;

   /* check for irrational inputs */

   if( nval < 10 || val == NULL ) return 0.0 ;

   /* count nonzero inputs */

   for( nnz=jj=0 ; jj < nval ; jj++ )
     if( val[jj] != 0.0 ) nnz++ ;

   /* if all are nonzero, this is easy */

   if( nnz == nval ){
     gg = ljung_box_uneven( nval , hh , val , NULL ) ;
     return gg ;
   }

   /* manufacture what we need */

   vnz = (double *)malloc(sizeof(double)*nnz) ; /* censored data */
   tau = (int *)   malloc(sizeof(int)   *nnz) ; /* indexes of saved data */
   for( nnz=jj=0 ; jj < nval ; jj++ ){
     if( val[jj] != 0.0 ){
       vnz[nnz] = val[jj] ; tau[nnz] = jj ; nnz++ ;
     }
   }

   gg = ljung_box_uneven( nnz , hh , vnz , tau ) ;
   free(tau) ; free(vnz) ;
   return gg ;
}

/*----------------------------------------------------------------*/

MRI_IMAGE * mri_vec_to_ljmap( MRI_IMAGE *inim )  /* 05 Feb 2020 */
{
   int nx , ny , ii , jj ;
   MRI_IMAGE *outim ;
   float     *outar , *iar , *qar ;
   double *dar ;

   if( inim == NULL || inim->kind != MRI_float ) return NULL ;

   nx = inim->nx ; if( nx < 9 ) return NULL ;
   ny = inim->ny ; if( ny < 9 ) return NULL ;

   outim = mri_new( ny , 1 , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;
   dar   = (double *)malloc(sizeof(double)*nx) ;
   iar   = MRI_FLOAT_PTR(inim) ;

   for( ii=0 ; ii < ny ; ii++ ){
     qar = iar + ii*nx ;
     for( jj=0 ; jj < nx ; jj++ ) dar[jj] = qar[jj] ;
     outar[ii] = ljung_box_zcens( nx , 0 , dar ) ;
   }

   free(dar) ;
   return outim ;
}
