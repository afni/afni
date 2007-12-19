/*------------------------------------------------------------------------*/

MRI_IMAGE * mri_principal_vector( MRI_IMARR *imar )
{
   int nx , nvec , ii,jj ;
   double *amat , *umat , *vmat , *sval ;
   float *far ; MRI_IMAGE *tim ;

   if( imar == NULL ) return NULL ;
   nvec = IMARR_COUNT(imar) ;       if( nvec < 1 ) return NULL ;
   nx   = IMARR_SUBIM(imar,0)->nx ; if( nx   < 1 ) return NULL ;

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
     for( ii=0 ; ii < nx ; ii++ ) A(ii,jj) = (double)far[ii] ;
   }
#if 0
   if( nvec > 1 ){
     double sum ;
     for( ii=0 ; ii < nx ; ii++ ){
       sum = 0.0 ;
       for( jj=0 ; jj < nvec ; jj++ ) sum += A(ii,jj) ;
       sum /= nvec ;
       for( jj=0 ; jj < nvec ; jj++ ) A(ii,jj) -= sum ;
     }
   }
#endif

   svd_double( nx , nvec , amat , sval , umat , vmat ) ;

   tim = mri_new( nx , 1 , MRI_float ) ;
   far = MRI_FLOAT_PTR(tim) ;
   for( ii=0 ; ii < nx ; ii++ ) far[ii] = (float)U(ii,0) ;

   free(sval); free(vmat); free(umat); free(amat); return tim;
}
