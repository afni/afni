#include "mrilib.h"

/*------------------------------------------------------------------------*/
/*! Compute first principal component of a bunch of vectors
    * each vector has its own mean removed
    * the overall mean of the vectors is not removed
    * only time series points from ibot..itop (inclusive) are processed,
      and the output timeseries will be itop-ibot+1 points long
    * NULL is returned if something stupid happens
--------------------------------------------------------------------------*/

MRI_IMAGE * mri_pcvector( MRI_IMARR *imar , int ibot, int itop )
{
   int nx , nvec , ii,jj , npos,nneg ;
   float *amat , *umat , sum ;
   float *far ; MRI_IMAGE *tim ;

   if( imar == NULL ) return NULL ;
   nvec = IMARR_COUNT(imar) ;       if( nvec < 1 ) return NULL ;
   nx   = IMARR_SUBIM(imar,0)->nx ; if( nx   < 1 ) return NULL ;
   if( ibot < 0 ) ibot = 0 ;
   if( itop <= itop || itop >= nx ) itop = nx-1 ;
   nx = itop-ibot+1 ;               if( nx   < 2 ) return NULL ;

#define A(i,j) amat[(i)+(j)*nx]     /* nx X nvec matrix */

   amat = (float *)malloc( sizeof(float)*nx*nvec ) ;
   umat = (float *)malloc( sizeof(float)*nx      ) ;

   for( jj=0 ; jj < nvec ; jj++ ){
     tim = IMARR_SUBIM(imar,jj) ;
     far = MRI_FLOAT_PTR(tim) ;
     for( sum=ii=0 ; ii < nx ; ii++ ){
       A(ii,jj) = far[ii+ibot] ; sum += A(ii,jj) ;
     }
     sum /= nx ;  /* remove mean from each vector */
     for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) -= sum ;
   }

   jj = first_principal_vectors( nx , nvec , amat , 1 , NULL , umat ) ;

   if( jj <= 0 ){ free(umat); free(amat); return NULL; } /* bad */

   tim = mri_new( nx , 1 , MRI_float ) ;  /* all zero */
   far = MRI_FLOAT_PTR(tim) ;
   for( ii=0 ; ii < nx ; ii++ ) far[ii] = umat[ii] ;

   /* compute dot products with original vectors,
      and flip sign if more of them are negative than positive */

   for( npos=nneg=jj=0 ; jj < nvec ; jj++ ){
     for( sum=ii=0 ; ii < nx ; ii++ ) sum += A(ii,jj)*far[ii] ;
     if( sum > 0.0f ) npos++ ; else if( sum < 0.0f ) nneg++ ;
   }
   if( nneg > npos ){
     for( ii=0 ; ii < nx ; ii++ ) far[ii] = -far[ii] ;
   }

   free(umat); free(amat); return tim;
}

/*------------------------------------------------------------------------*/

MRI_IMAGE * mri_meanvector( MRI_IMARR *imar , int ibot, int itop )
{
   float *qar , *far ;
   int nx,nv,jj,kk , nxout ;
   MRI_IMAGE *im ;

   if( imar == NULL ) return NULL ;

   nx = IMARR_SUBIM(imar,0)->nx ; nv = IMARR_COUNT(imar) ;
   if( ibot < 0 ) ibot = 0 ;
   if( itop <= itop || itop >= nx ) itop = nx-1 ;
   nxout = itop-ibot+1 ;
   im = mri_new( nxout , 1 , MRI_float ) ; far = MRI_FLOAT_PTR(im) ;
   for( jj=0 ; jj < nv ; jj++ ){
     qar = MRI_FLOAT_PTR(IMARR_SUBIM(imar,jj)) ;
     for( kk=0 ; kk < nxout ; kk++ ) far[kk] += qar[kk+ibot] ;
   }
   for( kk=0 ; kk < nxout ; kk++ ) far[kk] /= nv ;
   return im ;
}
