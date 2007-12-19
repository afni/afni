#include "mrilib.h"

/*------------------------------------------------------------------------*/
/*! Compute first principal component of a bunch of vectors
    * each vector has its own mean removed
    * the overall mean of the vectors is not removed
    * the first 'ignore' points of each vector is skipped, and
      will be replaced with 0's in the output vector
    * NULL is returned if something stupid happens
--------------------------------------------------------------------------*/

MRI_IMAGE * mri_pcvector( MRI_IMARR *imar , int ignore )
{
   int nx , nvec , ii,jj , ign=ignore , npos,nneg ;
   double *amat , *umat , *vmat , *sval , sum ;
   float *far ; MRI_IMAGE *tim ;

   if( imar == NULL ) return NULL ;
   nvec = IMARR_COUNT(imar) ;       if( nvec < 1 ) return NULL ;
   nx   = IMARR_SUBIM(imar,0)->nx ; if( nx   < 1 ) return NULL ;
   if( ign < 0 || ign > nx-3 ) ign = 0 ;
   nx = nx - ign ;

#define A(i,j) amat[(i)+(j)*nx]     /* nx X nvec matrix */
#define U(i,j) umat[(i)+(j)*nx]     /* ditto */
#define V(i,j) vmat[(i)+(j)*nvec]   /* nvec X nvec matrix */
#define X(i,j) amat[(i)+(j)*nvec]   /* nvec X nx matrix */

   amat = (double *)malloc( sizeof(double)*nx*nvec ) ;
   umat = (double *)malloc( sizeof(double)*nx*nvec ) ;
   vmat = (double *)malloc( sizeof(double)*nvec*nvec ) ;
   sval = (double *)malloc( sizeof(double)*nvec ) ;

   for( jj=0 ; jj < nvec ; jj++ ){
     tim = IMARR_SUBIM(imar,jj) ;
     far = MRI_FLOAT_PTR(tim) ;
     for( sum=ii=0 ; ii < nx ; ii++ ){
       A(ii,jj) = (double)far[ii+ign] ; sum += A(ii,jj) ;
     }
     sum /= nx ;
     for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) -= sum ;
   }

   svd_double( nx , nvec , amat , sval , umat , vmat ) ;

   tim = mri_new( nx+ign , 1 , MRI_float ) ;
   far = MRI_FLOAT_PTR(tim) ;
   for( ii=0 ; ii < nx ; ii++ ) far[ii+ign] = (float)U(ii,0) ;

   /* compute dot product with original vectors */

   for( npos=nneg=jj=0 ; jj < nvec ; jj++ ){
     for( sum=ii=0 ; ii < nx ; ii++ ) sum += A(ii,jj)*far[ii+ign] ;
     if( sum > 0.0 ) npos++ ; else if( sum < 0.0 ) nneg++ ;
   }
   if( nneg > npos ){
     for( ii=0 ; ii < nx ; ii++ ) far[ii+ign] = -far[ii+ign] ;
   }

   free(sval); free(vmat); free(umat); free(amat); return tim;
}
