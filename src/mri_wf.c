#include "mrilib.h"

#undef  TWOPI
#define TWOPI 6.283185307

typedef struct {
  int nwt ; float a ; float *wt ;
} wtarray ;

#undef  INIT_wtarray
#define INIT_wtarray(nam,aa,nn)                                    \
 do{ (nam) = (wtarray *)malloc(sizeof(wtarray)) ;                  \
     (nam)->nwt = (nn) ; (nam)->a = (aa) ;                         \
     (nam)->wt = ((nn) > 0) ? (float *)calloc((nn),sizeof(float))  \
                            : (float *)NULL ;                      \
 } while(0)

#undef  FREEIF
#define FREEIF(p) do{ if((p)!=NULL){free(p);(p)=NULL;} } while(0)

static MRI_IMAGE * mri_psinv( MRI_IMAGE *imc , float *wt ) ;

/*---------------------------------------------------------------------------*/

float wtarray_inverse( int nnx , wtarray *wf , int nwi , wtarray *wi )
{
   MRI_IMAGE *imc , *imp ;
   float     *car , *par , *wtf , *wti , *rhs ;
   int nx=nnx , nwf , ii,jj ;
   float dx , xx , ss , yy , aa,ainv ;
   double esum ;

   /** take care of pathological cases **/

   if( wi == NULL || nx < 2 ) return 0.0f ;
   if( wf == NULL || wf->a == 0.0f ){
     ERROR_message("wf is bad") ;
     wi->a = 1.0f ; wi->nwt = 0 ; FREEIF(wi->wt) ; return 0.0f ;
   }
   if( wf->nwt <= 0 ){
     ERROR_message("wf->nwt is %d",wf->nwt) ;
     wi->a = (wf->a != 0.0f) ? 1.0f/wf->a : 1.0f ;
     wi->nwt = 0 ; FREEIF(wi->wt) ; return 0.0f ;
   }
   if( nwi < 1 ) nwi = wf->nwt ;

   FREEIF(wi->wt) ;
   aa  = wf->a ; wi->a = ainv = 1.0f/aa ;
   dx  = TWOPI / (nx-1) ;
   nwf = wf->nwt ; wi->nwt = nwi ;
   wtf = wf->wt ;
   wti = wi->wt = (float *)malloc(sizeof(float *)*nwi) ;
   imc = mri_new( nx , nwi , MRI_float ) ;
   car = MRI_FLOAT_PTR(imc) ;
   rhs = (float *)malloc(sizeof(float)*nx) ;

   /* basis functions are sin(2*k*Pi*x) for k=1..nw, x=0..1 */

   for( ii=0 ; ii < nx ; ii++ ){
     xx = dx * ii ; ss = 0.0f ;
     for( jj=0 ; jj < nwf ; jj++ ) ss += wtf[jj] * sinf((jj+1)*xx) ;
     rhs[ii] = -ainv * ss ;
     yy = aa*xx + TWOPI*ss ;
     for( jj=0 ; jj < nwi ; jj++ ) car[ii+jj*nx] = sinf((jj+1)*yy) ;
   }

   imp = mri_psinv( imc , NULL ) ; mri_free(imc) ;
   par = MRI_FLOAT_PTR(imp) ;

   for( jj=0 ; jj < nwi ; jj++ ){
     ss = 0.0f ;
     for( ii=0 ; ii < nx ; ii++ ) ss += par[jj+ii*nwi] * rhs[ii] ;
     wti[jj] = ss ;
   }
   mri_free(imp) ; free((void *)rhs) ;

   esum = 0.0 ;
   for( ii=0 ; ii < nx ; ii++ ){
     xx = dx * ii ; ss = 0.0f ;
     for( jj=0 ; jj < nwf ; jj++ ) ss += wtf[jj] * sinf((jj+1)*xx) ;
     yy = aa*xx + TWOPI*ss ;
     ss = ainv*ss ;
     for( jj=0 ; jj < nwi ; jj++ ) ss += wti[jj] * sinf((jj+1)*yy) ;
     esum += ss*ss ;
   }
   return (float)sqrt(esum/nx) ;
}

/*-----------------------------------------------------------------------*/
/*! Compute the pseudo-inverse of a matrix stored in a 2D float image.
    If the input is mXn, the output is nXm.  wt[] is an optional array
    of positive weights, m of them.  The result can be used to solve
    the weighted least squares problem
      [imc] [b] = [v]
    where [b] is an n-vector and [v] is an m-vector, where m > n.
-------------------------------------------------------------------------*/

static MRI_IMAGE * mri_psinv( MRI_IMAGE *imc , float *wt )
{
   float *rmat=MRI_FLOAT_PTR(imc) ;
   int m=imc->nx , n=imc->ny , ii,jj,kk ;
   double *amat , *umat , *vmat , *sval , *xfac , smax,del,ww ;
   MRI_IMAGE *imp ; float *pmat ;
   register double sum ;
   int do_svd=0 ;

   amat = (double *)calloc( sizeof(double),m*n ) ;  /* input matrix */
   xfac = (double *)calloc( sizeof(double),n   ) ;  /* column norms of [a] */

#define R(i,j) rmat[(i)+(j)*m]   /* i=0..m-1 , j=0..n-1 */
#define A(i,j) amat[(i)+(j)*m]   /* i=0..m-1 , j=0..n-1 */
#define P(i,j) pmat[(i)+(j)*n]   /* i=0..n-1 , j=0..m-1 */

   /* copy input matrix into amat */

   for( ii=0 ; ii < m ; ii++ )
     for( jj=0 ; jj < n ; jj++ ) A(ii,jj) = R(ii,jj) ;

   /* weight rows? */

   if( wt != NULL ){
     for( ii=0 ; ii < m ; ii++ ){
       ww = wt[ii] ;
       for( jj=0 ; jj < n ; jj++ ) A(ii,jj) *= ww ;
     }
   }

   /* scale each column to have norm 1 */

   for( jj=0 ; jj < n ; jj++ ){
     sum = 0.0 ;
     for( ii=0 ; ii < m ; ii++ ) sum += A(ii,jj)*A(ii,jj) ;
     if( sum > 0.0 ) sum = 1.0/sqrt(sum) ; else do_svd = 1 ;
     xfac[jj] = sum ;
     for( ii=0 ; ii < m ; ii++ ) A(ii,jj) *= sum ;
   }

   /*** compute using Choleski or SVD ***/

   if( do_svd || AFNI_yesenv("AFNI_WARPDRIVE_SVD") ){ /***--- SVD method ---***/

#define U(i,j) umat[(i)+(j)*m]
#define V(i,j) vmat[(i)+(j)*n]

     umat = (double *)calloc( sizeof(double),m*n ); /* left singular vectors */
     vmat = (double *)calloc( sizeof(double),n*n ); /* right singular vectors */
     sval = (double *)calloc( sizeof(double),n   ); /* singular values */

     /* compute SVD of scaled matrix */

     svd_double( m , n , amat , sval , umat , vmat ) ;

     free((void *)amat) ;  /* done with this */

     /* find largest singular value */

     smax = sval[0] ;
     for( ii=1 ; ii < n ; ii++ ) if( sval[ii] > smax ) smax = sval[ii] ;

     if( smax <= 0.0 ){                        /* this is bad */
       fprintf(stderr,"** ERROR: SVD fails in mri_warp3D_align_setup!\n");
       free((void *)xfac); free((void *)sval);
       free((void *)vmat); free((void *)umat); return NULL;
     }

     for( ii=0 ; ii < n ; ii++ )
       if( sval[ii] < 0.0 ) sval[ii] = 0.0 ;  /* should not happen */

#define PSINV_EPS 1.e-8

     /* "reciprocals" of singular values:  1/s is actually s/(s^2+del) */

     del = PSINV_EPS * smax*smax ;
     for( ii=0 ; ii < n ; ii++ )
       sval[ii] = sval[ii] / ( sval[ii]*sval[ii] + del ) ;

     /* create pseudo-inverse */

     imp  = mri_new( n , m , MRI_float ) ;   /* recall that m > n */
     pmat = MRI_FLOAT_PTR(imp) ;

     for( ii=0 ; ii < n ; ii++ ){
       for( jj=0 ; jj < m ; jj++ ){
         sum = 0.0 ;
         for( kk=0 ; kk < n ; kk++ ) sum += sval[kk] * V(ii,kk) * U(jj,kk) ;
         P(ii,jj) = (float)sum ;
       }
     }
     free((void *)sval); free((void *)vmat); free((void *)umat);

   } else { /***----- Choleski method -----***/

     vmat = (double *)calloc( sizeof(double),n*n ); /* normal matrix */

     for( ii=0 ; ii < n ; ii++ ){
       for( jj=0 ; jj <= ii ; jj++ ){
         sum = 0.0 ;
         for( kk=0 ; kk < m ; kk++ ) sum += A(kk,ii) * A(kk,jj) ;
         V(ii,jj) = sum ;
       }
       V(ii,ii) += PSINV_EPS ;   /* note V(ii,ii)==1 before this */
     }

#if 0
fprintf(stderr,"NORMAL MATRIX::\n") ;
for( ii=0 ; ii < n ; ii++ ){
  fprintf(stderr,"%2d:",ii) ;
  for( jj=0 ; jj <= ii ; jj++ ) fprintf(stderr," %7.4f",V(ii,jj)) ;
  fprintf(stderr,"\n") ;
}
#endif
#if 1
{ double rr,rmax=0.0 ;
  for( ii=0 ; ii < n ; ii++ ){
    rr = 0.0 ;
    for( jj=0 ; jj < n ; jj++ ){
           if( jj < ii ) rr += fabs(V(ii,jj)) ;
      else if( jj > ii ) rr += fabs(V(jj,ii)) ;
    }
    rr = rr / V(ii,ii) ; if( rr > rmax ) rmax = rr ;
  }
  fprintf(stderr,"MAX row ratio = %g\n",rmax) ;
}
#endif

     /* Choleski factor */

     for( ii=0 ; ii < n ; ii++ ){
       for( jj=0 ; jj < ii ; jj++ ){
         sum = V(ii,jj) ;
         for( kk=0 ; kk < jj ; kk++ ) sum -= V(ii,kk) * V(jj,kk) ;
         V(ii,jj) = sum / V(jj,jj) ;
       }
       sum = V(ii,ii) ;
       for( kk=0 ; kk < ii ; kk++ ) sum -= V(ii,kk) * V(ii,kk) ;
       if( sum <= 0.0 ){
         fprintf(stderr,"** ERROR: Choleski fails in mri_warp3D_align_setup!\n");
         free((void *)xfac); free((void *)amat); free((void *)vmat); return NULL ;
       }
       V(ii,ii) = sqrt(sum) ;
     }

#if 0
fprintf(stderr,"CHOLESKI FACTOR::\n") ;
for( ii=0 ; ii < n ; ii++ ){
  fprintf(stderr,"%2d:",ii) ;
  for( jj=0 ; jj <= ii ; jj++ ) fprintf(stderr," %7.4f",V(ii,jj)) ;
  fprintf(stderr,"\n") ;
}
#endif

     /* create pseudo-inverse */

     imp  = mri_new( n , m , MRI_float ) ;   /* recall that m > n */
     pmat = MRI_FLOAT_PTR(imp) ;

     sval = (double *)calloc( sizeof(double),n ) ; /* row #jj of A */

     for( jj=0 ; jj < m ; jj++ ){
       for( ii=0 ; ii < n ; ii++ ) sval[ii] = A(jj,ii) ; /* extract row */

       for( ii=0 ; ii < n ; ii++ ){  /* forward solve */
         sum = sval[ii] ;
         for( kk=0 ; kk < ii ; kk++ ) sum -= V(ii,kk) * sval[kk] ;
         sval[ii] = sum / V(ii,ii) ;
       }
       for( ii=n-1 ; ii >= 0 ; ii-- ){  /* backward solve */
         sum = sval[ii] ;
         for( kk=ii+1 ; kk < n ; kk++ ) sum -= V(kk,ii) * sval[kk] ;
         sval[ii] = sum / V(ii,ii) ;
       }

       for( ii=0 ; ii < n ; ii++ ) P(ii,jj) = (float)sval[ii] ;
     }
     free((void *)amat); free((void *)vmat); free((void *)sval);
   }

   /* rescale rows from norming */

   for( ii=0 ; ii < n ; ii++ ){
     for( jj=0 ; jj < m ; jj++ ) P(ii,jj) *= xfac[ii] ;
   }
   free((void *)xfac);

   /* rescale cols for weight? */

   if( wt != NULL ){
     for( ii=0 ; ii < m ; ii++ ){
       ww = wt[ii] ;
       for( jj=0 ; jj < n ; jj++ ) P(jj,ii) *= ww ;
     }
   }

   return imp;
}

/*-----------------------------------------------------------------------*/

#define NW 10

int main( int argc , char *argv[] )
{
   wtarray *wt_for , *wt_inv ;
   int jj ;
   int nwf=NW , nwi=0 ;
   float err , aa=0.222 ;

   if( argc > 1 ){
     nwf = (int)strtod(argv[1],NULL) ;
     if( nwf < 1 || nwf > 99 ) nwf = NW ;
   }
   if( argc > 2 ){
     nwi = (int)strtod(argv[2],NULL) ;
     if( nwi < 1 || nwi > 999 ) nwi = 0 ;
   }
   if( nwi < 1 ) nwi = nwf ;
   if( argc > 3 ){
     aa = (float)strtod(argv[3],NULL) ;
   }

   INIT_wtarray(wt_for,1.0f,nwf) ;
   INIT_wtarray(wt_inv,1.0f,0  ) ;

   for( jj=1 ; jj <= nwf ; jj++ )
     wt_for->wt[jj-1] = aa /(jj*jj + 1.0f) ;

   err = wtarray_inverse( 66*MAX(nwf,nwi) , wt_for , nwi , wt_inv ) ;

   if( wt_inv->nwt == 0 || wt_inv->wt == NULL ) ERROR_exit("Bad wt_inv!") ;

   printf("err = %.5g\n",err) ;

   printf("z := x -> %f * x ",wt_inv->a) ;
   for( jj=1 ; jj <= nwi ; jj++ ){
     if( wt_inv->wt[jj-1] != 0.0f ){
       if( wt_inv->wt[jj-1] >  0.0f ) printf(" +") ;
       printf("%f * sin(%d*Pi*x)" , wt_inv->wt[jj-1] , 2*jj ) ;
     }
   }
   printf(";\n") ;

   printf("y := x -> %f * x ",wt_for->a) ;
   for( jj=1 ; jj <= nwf ; jj++ ){
     if( wt_for->wt[jj-1] != 0.0f ){
       if( wt_for->wt[jj-1] >  0.0f ) printf(" +") ;
       printf("%f * sin(%d*Pi*x)" , wt_for->wt[jj-1] , 2*jj ) ;
     }
   }
   printf(";\n") ;

   exit(0) ;
}
