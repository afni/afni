/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*** 7D SAFE ***/

/*-----------------------------------------------------------------
   Routine to compute the least squares fit of one image
   (fitim) to a linear combination of other images (refim).
   If not NULL, wtim is an image of weighting factors.
   Returns a malloc-ed array of floats (the fit coefficients).
-------------------------------------------------------------------*/

float * mri_lsqfit( MRI_IMAGE * fitim , MRI_IMARR * refim , MRI_IMAGE * wtim )
{
   float *fit = NULL ;                 /* will be the output */
   MRI_IMAGE *ffitim , *tim , *wim ;   /* local versions of inputs */
   MRI_IMARR *frefim ;

   int ii , jj , npix,nref ;
   float **refar , *fitar , *war ;

   /****---- check inputs, convert to float type if needed ----****/

   if( fitim == NULL ){
      fprintf(stderr,"mri_lsqfit: NULL fitim!\a\n") ; exit(1) ;
   }

   if( fitim->kind == MRI_float ) ffitim = fitim ;
   else                           ffitim = mri_to_float( fitim ) ;
   npix  = ffitim->nvox ;
   fitar = mri_data_pointer(ffitim) ;

   if( wtim == NULL ){
      wim = NULL ;
      war = NULL ;
   } else if( wtim->kind == MRI_float ){
      wim = wtim ;
      war = mri_data_pointer( wim ) ;
      if( wim->nvox != npix ){
         fprintf(stderr,"mri_lsqfit: MISMATCH wtim\a\n") ; exit(1) ;
      }
   } else {
      wim = mri_to_float( wtim ) ;
      war = mri_data_pointer( wim ) ;
      if( wim->nvox != npix ){
         fprintf(stderr,"mri_lsqfit: MISMATCH wtim\a\n") ; exit(1) ;
      }
   }

   if( refim == NULL || refim->num < 1 ){
      fprintf(stderr,"mri_lsqfit: NULL refim!\a\n") ; exit(1) ;
   }

   nref = refim->num ;

   INIT_IMARR(frefim) ;
   refar = (float **) malloc( sizeof(float *) * nref ) ;
   if( refar == NULL ){
      fprintf(stderr,"mri_lsqfit: malloc failure for refar!\a\n") ; exit(1) ;
   }

   for( ii=0 ; ii < nref ; ii++ ){
      if( refim->imarr[ii] == NULL ){
         fprintf(stderr,"mri_lsqfit: NULL refim[%d]!\a\n",ii) ; exit(1) ;
      }
      if( refim->imarr[ii]->nvox != npix ){
         fprintf(stderr,"mri_lsqfit: MISMATCH refim[%d]!\a\n",ii) ; exit(1) ;
      }
      if( refim->imarr[ii]->kind == MRI_float ) tim = refim->imarr[ii] ;
      else                                      tim = mri_to_float(refim->imarr[ii]) ;
      ADDTO_IMARR(frefim,tim) ;
      refar[ii] = mri_data_pointer(tim) ;
   }

   /****---- get coefficients ----****/

   fit = lsqfit( npix , fitar , war , nref , refar ) ;

   /****---- clean up and exit ----****/

   if( ffitim != fitim ) mri_free( ffitim ) ;
   if( wim != NULL && wim != wtim ) mri_free( wim ) ;
   for( ii=0 ; ii < nref ; ii++ ){
      if( frefim->imarr[ii] != refim->imarr[ii] ) mri_free( frefim->imarr[ii] ) ;
   }
   FREE_IMARR(frefim) ;
   free(refar) ;

   return fit ;
}

/*----------------------------------------------------------------
     Routine to compute the weighted least square fit of
     a data vector to a bunch of reference vectors:

 veclen = length of vectors
 data   = array holding data vector (length veclen)
 wt     = array holding weight for each data point (length veclen)
          [if NULL, then a vector of all 1's is used]
 nref   = number of reference vectors, each of length veclen;
          must have 1 <= nref <= veclen
 ref    = array of pointers to reference vectors, so that
          ref[i][k] is the k-th component of the i-th reference,
          for i=0..nref-1, k=0..veclen-1

 Return value is a pointer to a vector of length nref with the
 weights of each reference;  if NULL is returned, there was an
 error.  The array is allocated by malloc and so should be freed
 when the caller is finished with it.

 Input and output vectors are floats.  Internal calculuations
 are done with doubles.
------------------------------------------------------------------*/

float * lsqfit( int veclen ,
                float *data , float *wt , int nref , float *ref[] )
{
   int    ii , jj , kk ;
   float  *alpha = NULL ;
   double *cc = NULL , *rr = NULL ;
   double sum ;

   /** macros will be used in routines below, as well! **/

#define DBLEVEC(ll) (double *) malloc( sizeof(double) * (ll) )
#define DISCARD(xx) if( xx != NULL ){free(xx); xx = NULL;}
#define CLEANUP     {DISCARD(cc); DISCARD(rr);}

   if( nref < 1 || veclen < nref || data == NULL || ref == NULL ) return NULL ;

   /*** form RHS vector into rr ***/

   rr = DBLEVEC(nref) ;
   cc = DBLEVEC(nref*nref) ;
   if( rr == NULL || cc == NULL ){ CLEANUP ; return NULL ; }

   if( wt != NULL ){
      for( ii=0 ; ii < nref ; ii++ ){
         sum = 0.0 ;
         for( jj=0 ; jj < veclen ; jj++ ) sum += ref[ii][jj] * wt[jj] * data[jj] ;
         rr[ii] = sum ;
      }
   } else {
      for( ii=0 ; ii < nref ; ii++ ){
         sum = 0.0 ;
         for( jj=0 ; jj < veclen ; jj++ ) sum += ref[ii][jj] * data[jj] ;
         rr[ii] = sum ;
      }
   }

   /*** form coefficient matrix into cc */

#define CC(i,j) cc[(i)+(j)*nref]

   if( wt != NULL ){
      for( jj=0 ; jj < nref ; jj++ ){
         for( ii=0 ; ii <= jj ; ii++ ){
            sum = 0.0 ;
            for( kk=0 ; kk < veclen ; kk++ ) sum += ref[ii][kk] * ref[jj][kk] * wt[kk] ;
            CC(ii,jj) = CC(jj,ii) = sum ;
         }
      }
   } else {
      for( jj=0 ; jj < nref ; jj++ ){
         for( ii=0 ; ii <= jj ; ii++ ){
            sum = 0.0 ;
            for( kk=0 ; kk < veclen ; kk++ ) sum += ref[ii][kk] * ref[jj][kk] ;
            CC(ii,jj) = CC(jj,ii) = sum ;
         }
      }
   }

   /*** Choleski decompose cc ***/

   for( ii=0 ; ii < nref ; ii++ ){
      for( jj=0 ; jj < ii ; jj++ ){
         sum = CC(ii,jj) ;
         for( kk=0 ; kk < jj ; kk++ ) sum -= CC(ii,kk) * CC(jj,kk) ;
         CC(ii,jj) = sum / CC(jj,jj) ;
      }
      sum = CC(ii,ii) ;
      for( kk=0 ; kk < ii ; kk++ ) sum -= CC(ii,kk) * CC(ii,kk) ;
      if( sum <= 0.0 ){ CLEANUP ; return NULL ; }
      CC(ii,ii) = sqrt(sum) ;
   }

   /*** forward solve ***/

   for( ii=0 ; ii < nref ; ii++ ){
      sum = rr[ii] ;
      for( jj=0 ; jj < ii ; jj++ ) sum -= CC(ii,jj) * rr[jj] ;
      rr[ii] = sum / CC(ii,ii) ;
   }

   /*** backward solve ***/

   for( ii=nref-1 ; ii >= 0 ; ii-- ){
      sum = rr[ii] ;
      for( jj=ii+1 ; jj < nref ; jj++ ) sum -= CC(jj,ii) * rr[jj] ;
      rr[ii] = sum / CC(ii,ii) ;
   }

   /*** put result into alpha ***/

   alpha = (float *) malloc( sizeof(float) * nref ) ;
   for( ii=0 ; ii < nref ; ii++ ) alpha[ii] = rr[ii] ;

   /*** cleanup and exit ***/

   CLEANUP ; return alpha ;
}

/*----------------------------------------------------------------
   Similar to above, but only produce the Choleski decomposition
   and weight scaled ref vectors for later use in delayed_lsqfit.
   This is to be used when fitting several data vectors to the
   same references with the same weight factors.

 veclen = length of vectors
 wt     = array holding weight for each data point (length veclen)
          [if NULL, then a vector of all 1's is used]
 nref   = number of reference vectors, each of length veclen;
          must have 1 <= nref <= veclen
 ref    = array of pointers to reference vectors, so that
          ref[i][k] is the k-th component of the i-th reference,
          for i=0..nref-1, k=0..veclen-1

   Return value is a (double *) which points to malloc-ed memory
   for the Choleksi decomposition.  It should later be passed
   to delayed_lsqfit.  If wt != NULL, then ref[ii][jj] is
   scaled by wt[jj] as well.

   If NULL is returned, an error occured.
------------------------------------------------------------------*/

double * startup_lsqfit( int veclen ,
                         float *wt , int nref , float *ref[] )
{
   int    ii , jj , kk ;
   double *cc = NULL ;
   double sum ;

   if( nref < 1 || veclen < nref || ref == NULL ){
      fprintf(stderr,"*** Illegal inputs to startup_lsqfit\n") ;
      return NULL ;
   }

   /*** form coefficient matrix into cc */

   cc = DBLEVEC(nref*nref) ;
   if( cc == NULL ){
      fprintf(stderr,"Can't malloc workspace in startup_lsqfit\n") ;
      return NULL ;
   }

   if( wt != NULL ){
      for( jj=0 ; jj < nref ; jj++ ){
         for( ii=0 ; ii <= jj ; ii++ ){
            sum = 0.0 ;
            for( kk=0 ; kk < veclen ; kk++ ) sum += ref[ii][kk] * ref[jj][kk] * wt[kk] ;
            CC(ii,jj) = CC(jj,ii) = sum ;
         }
      }
   } else {
      for( jj=0 ; jj < nref ; jj++ ){
         for( ii=0 ; ii <= jj ; ii++ ){
            sum = 0.0 ;
            for( kk=0 ; kk < veclen ; kk++ ) sum += ref[ii][kk] * ref[jj][kk] ;
            CC(ii,jj) = CC(jj,ii) = sum ;
         }
      }
   }

   /*** Choleski decompose cc ***/

   for( ii=0 ; ii < nref ; ii++ ){
      for( jj=0 ; jj < ii ; jj++ ){
         sum = CC(ii,jj) ;
         for( kk=0 ; kk < jj ; kk++ ) sum -= CC(ii,kk) * CC(jj,kk) ;
         CC(ii,jj) = sum / CC(jj,jj) ;
      }
      sum = CC(ii,ii) ;
      for( kk=0 ; kk < ii ; kk++ ) sum -= CC(ii,kk) * CC(ii,kk) ;
      if( sum <= 0.0 ){
         free(cc) ;
         fprintf(stderr,"Choleski factorization failure in startup_lsqfit\n") ;
         return NULL ;
      }
      CC(ii,ii) = sqrt(sum) ;
   }

   /*** scale ref by wt, if desired ***/

   if( wt != NULL ){
      for( ii=0 ; ii < nref ; ii++ )
         for( jj=0 ; jj < veclen ; jj++ ) ref[ii][jj] *= wt[jj] ;
   }

   return cc ;
}

/**------------------------------------------------------------------
  Given the data from startup_lsqfit, finish the job.
------------------------------------------------------------------**/

float * delayed_lsqfit( int veclen ,
                        float * data , int nref , float *ref[] , double * cc )
{
   int    ii , jj ;
   float  *alpha = NULL ;
   double *rr = NULL ;
   register double sum ;

   if( nref < 1 || veclen < nref ||
       data == NULL || ref == NULL || cc == NULL ) return NULL ;

   /*** form RHS vector into rr ***/

   rr = DBLEVEC(nref) ; if( rr == NULL ) return NULL ;

   for( ii=0 ; ii < nref ; ii++ ){
      sum = 0.0 ;
      for( jj=0 ; jj < veclen ; jj++ ) sum += ref[ii][jj] * data[jj] ;
      rr[ii] = sum ;
   }

   /*** forward solve ***/

   for( ii=0 ; ii < nref ; ii++ ){
      sum = rr[ii] ;
      for( jj=0 ; jj < ii ; jj++ ) sum -= CC(ii,jj) * rr[jj] ;
      rr[ii] = sum / CC(ii,ii) ;
   }

   /*** backward solve ***/

   for( ii=nref-1 ; ii >= 0 ; ii-- ){
      sum = rr[ii] ;
      for( jj=ii+1 ; jj < nref ; jj++ ) sum -= CC(jj,ii) * rr[jj] ;
      rr[ii] = sum / CC(ii,ii) ;
   }

   /*** put result into alpha ***/

   alpha = (float *) malloc( sizeof(float) * nref ) ; if( alpha == NULL ) return NULL ;
   for( ii=0 ; ii < nref ; ii++ ) alpha[ii] = rr[ii] ;

   /*** cleanup and exit ***/

   free(rr) ;
   return alpha ;
}

/*-----------------------------------------------------------------
   'startup' and 'delayed' versions for image fitting.
   N.B.: unlike mri_lsqfit, all the images in refim and wtim must
         be of MRI_float kind.
-------------------------------------------------------------------*/

double * mri_startup_lsqfit( MRI_IMARR * refim , MRI_IMAGE * wtim )
{
   double *cc = NULL ;                 /* will be the output */
   int ii , npix,nref ;
   float * wtar , ** refar ;

   /****---- check inputs ----****/

   if( wtim != NULL && wtim->kind != MRI_float ){
      fprintf(stderr,"mri_startup_lsqfit: non-float wtim!\a\n") ; exit(1) ;
   }
   wtar = (wtim == NULL) ? (NULL) : (MRI_FLOAT_PTR(wtim)) ;

   if( refim == NULL || refim->num < 1 ){
      fprintf(stderr,"mri_startup_lsqfit: NULL refim!\a\n") ; exit(1) ;
   }

   nref  = refim->num ;
   npix  = refim->imarr[0]->nvox ;
   refar = (float **) malloc( sizeof(float *) * nref ) ;
   if( refar == NULL ){
      fprintf(stderr,"mri_startup_lsqfit: malloc failure for refar!\a\n") ; exit(1) ;
   }

   for( ii=0 ; ii < nref ; ii++ ){
      if( refim->imarr[ii] == NULL ){
         fprintf(stderr,"mri_startup_lsqfit: NULL refim[%d]!\a\n",ii) ; exit(1) ;
      }
      if( refim->imarr[ii]->nvox != npix ){
         fprintf(stderr,"mri_startup_lsqfit: MISMATCH refim[%d]!\a\n",ii) ; exit(1) ;
      }
      if( refim->imarr[ii]->kind != MRI_float ){
         fprintf(stderr,"mri_startup_lsqfit: non-float refim[%d]!\a\n",ii) ; exit(1) ;
      }
      refar[ii] = MRI_FLOAT_PTR(refim->imarr[ii]) ;
   }

   /****---- get Choleski, send it out ----****/

   cc = startup_lsqfit( npix , wtar , nref , refar ) ;
   if( cc == NULL ){
         fprintf(stderr,"mri_startup_lsqfit: bad call to startup_lsqfit!\a\n") ; exit(1) ;
   }
   free(refar) ;
   return cc ;
}

float * mri_delayed_lsqfit( MRI_IMAGE * fitim , MRI_IMARR * refim , double * cc )
{
   int ii , npix,nref ;
   float * fit ;
   static float ** refar = NULL ;
   static int     nrefar = -1 ;

   nref = refim->num ;
   npix = refim->imarr[0]->nvox ;

   if( nrefar < nref ){
      if( refar != NULL ) free(refar) ;
      refar  = (float **) malloc( sizeof(float *) * nref ) ;
      nrefar = nref ;
   }
   if( refar == NULL ){
      fprintf(stderr,"mri_delayed_lsqfit: malloc failure for refar!\a\n") ; exit(1) ;
   }

   for( ii=0 ; ii < nref ; ii++ )
      refar[ii] = MRI_FLOAT_PTR(refim->imarr[ii]) ;

   fit = delayed_lsqfit( npix , MRI_FLOAT_PTR(fitim) , nref , refar , cc ) ;
   return fit ;
}
