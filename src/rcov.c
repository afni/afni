#include "mrilib.h"

typedef struct {
   int ndim ;
   float * cmat , * cfac , * mvec ;
} covmat ;

#define CM(i,j) cmat[(i)+(j)*ndim]
#define CH(i,j) cfac[(i)+(j)*ndim]

/*-----------------------------------------------------------------*/

void forward_solve_inplace( covmat * cv , float * vec )
{
   register int     ndim=cv->ndim , ii,jj ;
   register float * cfac=cv->cfac , sum ;

   for( ii=0 ; ii < ndim ; ii++ ){
      sum = vec[ii] ;
      for( jj=0 ; jj < ii ; jj++ ) sum -= CH(ii,jj) * vec[jj] ;
      vec[ii] = sum / CH(ii,ii) ;
   }
   return ;
}

/*-----------------------------------------------------------------*/

void backward_solve_inplace( covmat * cv , float * vec )
{
   register int     ndim=cv->ndim , ii,jj ;
   register float * cfac=cv->cfac , sum ;

   for( ii=ndim-1 ; ii >= 0 ; ii-- ){
      sum = vec[ii] ;
      for( jj=ii+1 ; jj < ndim ; jj++ ) sum -= CH(jj,ii) * vec[jj] ;
      vec[ii] = sum / CH(ii,ii) ;
   }
   return ;
}

/*-----------------------------------------------------------------*/

void compute_choleski( covmat * cv )
{
   register int     ndim=cv->ndim ,          ii,jj,kk ;
   register float * cmat=cv->cmat , * cfac , sum ;

   if( ndim < 2 || cmat == NULL ) return ;

   if( cv->cfac == NULL )
      cv->cfac = (float *) malloc(sizeof(float)*ndim*ndim) ;

   cfac = cv->cfac ;

   for( ii=0 ; ii < ndim ; ii++ ){
      for( jj=0 ; jj < ii ; jj++ ){
         sum = CM(ii,jj) ;
         for( kk=0 ; kk < jj ; kk++ ) sum -= CH(ii,kk) * CH(jj,kk) ;
         CH(ii,jj) = sum / CH(jj,jj) ;
      }
      sum = CM(ii,ii) ;
      for( kk=0 ; kk < ii ; kk++ ) sum -= CH(ii,kk) * CH(ii,kk) ;
      if( sum <= 0.0 ){ free(cv->cfac); cv->cfac = NULL; return; }
      CH(ii,ii) = sqrt(sum) ;
      for( jj=ii+1 ; jj < ndim ; jj++ ) CH(ii,jj) = 0.0 ;
   }
   return ;
}

/*-----------------------------------------------------------------*/

#define CCUT 3.5
#define EPS  1.e-4

covmat * robust_covar( int ndim , int nvec , float ** vec )
{
   covmat * cv ;
   float *nmat, *cmat , fnvec,fndim,cnorm,csum , *tv , *vv , *mv , *wv ;
   int ii , jj , kk , nite ;
   float bcut , cwt ;

fprintf(stderr,"Enter robust_covar:  ndim=%d  nvec=%d\n",ndim,nvec) ;

   if( ndim < 2 || nvec < ndim || vec == NULL ) return NULL ;

   cv = (covmat *) malloc(sizeof(covmat)) ;
   cv->ndim = ndim ;
   cv->cmat = NULL ;
   cv->cfac = NULL ;
   cv->mvec = NULL ;

   nmat = (float *) malloc(sizeof(float)*ndim*ndim) ;  /* matrix     */
   tv   = (float *) malloc(sizeof(float)*ndim) ;       /* temp vector */
   mv   = (float *) malloc(sizeof(float)*ndim) ;       /* mean vector  */
   wv   = (float *) malloc(sizeof(float)*nvec) ;       /* weight vector */

   fnvec = 1.0/nvec ; fndim = 1.0/ndim ;
   bcut  = 1.0 + CCUT*sqrt(fndim) ;

   /* compute initial mean & covariance matrix with all weights = 1 */

   for( jj=0 ; jj < ndim ; jj++ ) mv[jj] = 0.0 ;

   for( kk=0 ; kk < nvec ; kk++ ){   /* mean vector sum */
      vv = vec[kk] ;
      for( jj=0 ; jj < ndim ; jj++ ) mv[jj] += vv[jj] ;
   }
   for( jj=0 ; jj < ndim ; jj++ ) mv[jj] *= fnvec ;  /* scale mean vector */

   for( jj=0 ; jj < ndim ; jj++ )
      for( ii=0 ; ii < ndim ; ii++ ) nmat[ii+jj*ndim] = 0.0 ;

   for( kk=0 ; kk < nvec ; kk++ ){   /* covariance matrix sum */
      vv = vec[kk] ;
      for( jj=0 ; jj < ndim ; jj++ ){
         for( ii=0 ; ii <= jj ; ii++ )
            nmat[ii+jj*ndim] += (vv[ii]-mv[ii])*(vv[jj]-mv[jj]) ;
      }
   }
   for( jj=0 ; jj < ndim ; jj++ ){   /* scale covariance matrix */
      for( ii=0 ; ii < jj ; ii++ )
         nmat[jj+ii*ndim] = (nmat[ii+jj*ndim] *= fnvec) ;
      nmat[jj+jj*ndim] *= fnvec ;
   }

   /* now iterate */

   nite = 0 ;

   while(1){

      nite++ ;

      cmat = cv->cmat = nmat ;  /* put old matrix into cv */
      compute_choleski(cv) ;    /* decompose it */

      nmat = (float *) malloc(sizeof(float)*ndim*ndim) ; /* new matrix */
      cv->mvec = mv ;                                    /* old mean vector */
      mv = (float *) malloc(sizeof(float)*ndim) ;        /* new mean vector */

      for( jj=0 ; jj < ndim ; jj++ ){  /* initialize new things to zero */
         mv[jj] = 0.0 ;
         for( ii=0 ; ii < ndim ; ii++ ) nmat[ii+jj*ndim] = 0.0 ;
      }

fprintf(stderr,"\niteration %2d:\n",nite) ;

      /* update mean */

      csum = 0.0 ;
      for( kk=0 ; kk < nvec ; kk++ ){
         vv = vec[kk] ;

         /*                    -1/2          */
         /* compute tv = [cmat]    (vv-mvec) */

         for( jj=0 ; jj < ndim ; jj++ ) tv[jj] = vv[jj] - cv->mvec[jj] ;
         forward_solve_inplace(cv,tv) ;

         /* compute norm of tv, then weighting factor for this vector */

         cnorm = 0.0 ; for( ii=0 ; ii < ndim ; ii++ ) cnorm += tv[ii]*tv[ii] ;
         cnorm = cnorm*fndim ;
         cnorm = (cnorm <= bcut) ? 1.0 : bcut/cnorm ;
         wv[kk] = cnorm ; csum += cnorm ;
#if 0
fprintf(stderr,"  weight %2d = %12.4g\n",kk,cnorm) ;
#endif
#if 0
for( jj=0 ; jj < ndim ; jj++ ) fprintf(stderr," %f:%f:%f",vv[jj],tv[jj],cv->mvec[jj]) ;
fprintf(stderr,"\n") ;
#endif

         /* add vv into accumulating mean, with weight cnorm */

         for( jj=0 ; jj < ndim ; jj++ ) mv[jj] += cnorm*vv[jj] ;
      }
#if 0
fprintf(stderr,"  wv:");for(kk=0;kk<nvec;kk++)fprintf(stderr," %11.3g",wv[kk]);fprintf(stderr,"\n csum: %11.3g\n",csum);
#endif
      csum = 1.0 / csum ; cwt = nvec*csum ;
      for( jj=0 ; jj < ndim ; jj++ ) mv[jj] *= csum ;  /* scale new mean */

      /* update covariance */

      for( kk=0 ; kk < nvec ; kk++ ){
         vv = vec[kk] ; cnorm = wv[kk] ;
         for( jj=0 ; jj < ndim ; jj++ ){
            for( ii=0 ; ii <= jj ; ii++ )
               nmat[ii+jj*ndim] +=
                  cnorm*(vv[ii]-cv->mvec[ii])*(vv[jj]-cv->mvec[jj]) ;
         }
      }
#define DDD csum
      for( jj=0 ; jj < ndim ; jj++ ){
         for( ii=0 ; ii < jj ; ii++ )
            nmat[jj+ii*ndim] = (nmat[ii+jj*ndim] *= DDD) ;
         nmat[jj+jj*ndim] *= DDD ;
      }

      /* check for convergence - L1 norm */

      cnorm = csum = 0.0 ;
      for( jj=0 ; jj < ndim ; jj++ ){
         for( ii=0 ; ii <= jj ; ii++ ){
            cnorm += fabs( nmat[ii+jj*ndim] - cmat[ii+jj*ndim] ) ;
            csum  += fabs( nmat[ii+jj*ndim] ) ;
         }
      }

fprintf(stderr,"  |dif|=%12.4g  |mat|=%12.4g   cwt=%12.4g\n",cnorm,csum,cwt) ;
fprintf(stderr,"  matrix:\n") ;
for( ii=0 ; ii < ndim ; ii++ ){
   fprintf(stderr,"  Row%2d: %12.4g    ",ii,mv[ii]) ;
   for( jj=0 ; jj < ndim ; jj++ ) fprintf(stderr," %12.4g",nmat[ii+jj*ndim]) ;
   fprintf(stderr,"\n") ;
}

      free(cv->cmat) ; free(cv->mvec) ;
      if( cnorm <= EPS*csum ){ cv->cmat = nmat; cv->mvec = mv; break; }  /* exit loop */
   }

   free(wv) ; free(tv) ; compute_choleski(cv) ; return cv ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   MRI_IMAGE * im , * qim ;
   float * far , ** vec ;
   int ii,jj , ndim,nvec ;

   if( argc < 2 ){printf("Usage: rcov imfile\n"); exit(0); }

   im = mri_read_just_one( argv[1] ) ;
   if( im == NULL ) exit(1) ;
   if( im->kind != MRI_float ){
      qim = mri_to_float(im) ; mri_free(im) ; im = qim ;
   }
   qim = mri_transpose(im) ; mri_free(im) ; im = qim ;

   ndim = im->nx ; nvec = im->ny ;
   far = MRI_FLOAT_PTR(im) ;
   vec = (float **) malloc(sizeof(float *)*nvec) ;
   for( jj=0 ; jj < nvec ; jj++ ) vec[jj] = far + jj*ndim ;

#if 0
   far = (float *) malloc(sizeof(float)*ndim) ;
   for( ii=0 ; ii < ndim ; ii++ ) far[ii] = 0.0 ;
   for( jj=0 ; jj < nvec ; jj++ )
      for( ii=0 ; ii < ndim ; ii++ ) far[ii] += vec[jj][ii] ;
   for( ii=0 ; ii < ndim ; ii++ ) far[ii] /= nvec ;
   for( jj=0 ; jj < nvec ; jj++ )
      for( ii=0 ; ii < ndim ; ii++ ) vec[jj][ii] -= far[ii] ;
#endif

   (void) robust_covar( ndim , nvec , vec ) ;
   exit(0) ;
}
