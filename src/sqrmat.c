#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "sqrmat.h"


/*--------------------------------------------------------------*/

sqrmat * sm_iktk( sqrmat *K )
{
   int n , i,j,p ;
   double *mat , *nat , sum ;
   sqrmat *N ;

   n = K->n ;
   INIT_SQRMAT(N,n) ;
   mat = K->mat ; nat = N->mat ;
   for( j=0 ; j < n ; j++ ){
     for( i=0 ; i <= j ; i++ ){
       sum = -MAT(i,j) - MAT(j,i) ;
       for( p=0 ; p < n ; p++ ) sum += MAT(p,i)*MAT(p,j) ;
       NAT(i,j) = sum ;
     }
     for( i=0 ; i < j ; i++ ) NAT(j,i) = NAT(i,j) ;
     NAT(j,j) += 1.0 ;
   }
   return N ;
}

/*--------------------------------------------------------------*/

int sm_choleski( sqrmat *A )
{
   int n , i,j,p ;
   double *mat , sum ;

   n = A->n ; mat = A->mat ;
   if( MAT(0,0) <= 0.0 ) return 0 ;
   MAT(0,0) = sqrt(MAT(0,0)) ;
   for( j=1 ; j < n ; j++ ) MAT(0,j) = 0.0 ;
   for( i=1 ; i < n ; i++ ){
     MAT(i,0) /= MAT(0,0) ;
     for( j=1 ; j <= i ; j++ ){
       sum = MAT(i,j) ;
       for( p=0 ; p < j ; p++ ) sum -= MAT(i,p)*MAT(j,p) ;
            if( j < i     ) MAT(i,j) = sum / MAT(j,j) ;
       else if( sum > 0.0 ) MAT(i,i) = sqrt(sum) ;
       else                 return 0 ;
     }
     for( ; j < n ; j++ ) MAT(i,j) = 0.0 ;
   }
   return 1 ;
}

/*--------------------------------------------------------------*/

double sm_lndet_iktk( sqrmat *K )
{
   sqrmat *N ;
   double *mat , sum ; int i,n ;

   N = sm_iktk( K ) ; i = sm_choleski( N ) ;
   if( i == 0 ){ KILL_SQRMAT(N); return 0.0; }
   mat = N->mat ; n = N->n ; sum = 0.0 ;
   for( i=0 ; i < n ; i++ ) sum += log(MAT(i,i)) ;
   sum += sum;    /* need to double this for both upper and lower*/
   KILL_SQRMAT(N) ; return sum ;
}

/*--------------------------------------------------------------*/

sqrmat * sm_copy( sqrmat *A )
{
   sqrmat *B ; int n=A->n ;
   INIT_SQRMAT(B,n) ;
   memcpy((void *)B->mat,(void *)A->mat,sizeof(double)*n*n) ;
   return B ;
}

/*--------------------------------------------------------------*/

sqrmat * sm_transpose( sqrmat *A )
{
   sqrmat *B ; int n=A->n , i,j ; double *mat,*nat ;
   INIT_SQRMAT(B,n) ;
   mat = A->mat ; nat = B->mat ;
   for( i=0 ; i < n ; i++ ){
     NAT(i,i) = MAT(i,i) ;
     for( j=0 ; j < i ; j++ ){
       NAT(i,j) = MAT(j,i) ; NAT(j,i) = MAT(i,j) ;
     }
   }
   return B ;
}

/*--------------------------------------------------------------*/

sqrmat * sm_mult( sqrmat *A , sqrmat *B )
{
   sqrmat *C ; int n=A->n , i,j,k ; double *mat,*nat,*pat , sum ;
   INIT_SQRMAT(C,n) ;
   mat = A->mat ; nat = B->mat ; pat = C->mat ;
   for( i=0 ; i < n ; i++ ){
     for( j=0 ; j < n ; j++ ){
       sum = 0.0 ;
       for( k=0 ; k < n ; k++ ) sum += MAT(i,k)*NAT(k,j) ;
       PAT(i,j) = sum ;
     }
   }
   return C ;
}


sqrmat * sm_subtract( sqrmat *A , sqrmat *B )
{
   sqrmat *C ; int n=A->n , i,j,k ; double *mat,*nat,*pat , sum ;
   INIT_SQRMAT(C,n) ;
   mat = A->mat ; nat = B->mat ; pat = C->mat ;
   for( i=0 ; i < n ; i++ ){
     for( j=0 ; j < n ; j++ ){
       PAT(i,j) = MAT(i,j) - NAT(i,j) ;
     }
   }
   return C ;
}

sqrmat * sm_add( sqrmat *A , sqrmat *B )
{
   sqrmat *C ; int n=A->n , i,j,k ; double *mat,*nat,*pat;
   INIT_SQRMAT(C,n) ;
   mat = A->mat ; nat = B->mat ; pat = C->mat ;
   for( i=0 ; i < n ; i++ ){
     for( j=0 ; j < n ; j++ ){
       PAT(i,j) = MAT(i,j) + NAT(i,j) ;
     }
   }
   return C ;
}

/* create identity matrix of size nxn */
sqrmat * sm_identity(int n)
{
  sqrmat *i_mat;
  double *mat;
  int i;

  INIT_SQRMAT(i_mat, n);
  mat = i_mat->mat;
  for(i=0;i<n;i++) {
        MAT(i,i) = 1.0;
  }
  return(i_mat);
}


/* compute the large dot product of two matrices */
/* equals the trace of the product */
double sm_dot(sqrmat *A, sqrmat *B)
{
   int n=A->n , i,j;
   double *mat,*nat;
   double sum = 0.0;
   mat = A->mat ; nat = B->mat ;
   for( i=0 ; i < n ; i++ ){
     for( j=0 ; j < n ; j++ ){
       sum += MAT(i,j) * NAT(i,j) ;
     }
   }
   return(sum) ;
}

/* compute the trace of the matrix */
/* trace = sum of diagonal elements */
double sm_trace(sqrmat *A)
{
   int n=A->n , i;
   double *mat;
   double sum = 0.0;
   mat = A->mat ;
   for( i=0 ; i < n ; i++ ){
       sum += MAT(i,i);
   }
   return(sum) ;
}

/* scale matrix by factor*/
/* if newmatrix flag is set, return a new matrix with result */
/* otherwise scale each element of array in place overwriting */
/* elements */
sqrmat * sm_scale(sqrmat *A, double sc_factor, int newmatrix)
{
   int n=A->n, i, j;
   double *mat, *nat;
   sqrmat *B = NULL;

   mat = A->mat;
   if(newmatrix) {
       INIT_SQRMAT(B,n);
       nat = B->mat;
   }
   else
       nat = mat;
   for(i=0;i<n;i++)
      for(j=0;j<n;j++)
          NAT(i,j) = sc_factor * MAT(i,j);

   return(B);
}



/*--------------------------------------------------------------*/
#if 0
int main( int argc , char *argv[] )
{
   int n,nn , ii ;
   sqrmat *KK , *AA , *AAtr, *CH ;
   double *mat, *nat , val ;

   if( argc < 2 ) exit(1) ;
   n=nn = (int)strtod(argv[1],NULL); if( nn < 2 ) exit(1);

   INIT_SQRMAT(KK,nn) ; mat = KK->mat ;
   for( ii=1 ; ii < nn ; ii++ ){
     MAT(ii,ii-1) = (double)ii ;
     MAT(ii-1,ii) = (double)(ii*ii) ;
   }
   DUMP_SQRMAT("KK",KK) ;
   val = sm_lndet_iktk(KK) ; printf("ln[det[]] = %g\n",val) ;

   AA = sm_iktk( KK ) ;
   DUMP_SQRMAT("[I-K'][I-K]",AA) ;

   ii = sm_choleski( AA ) ;
   if( ii < 1 ) exit(1) ;
   DUMP_SQRMAT("Choleski",AA) ;

   AAtr = sm_transpose(AA) ;
   CH   = sm_mult( AA , AAtr ) ;
   DUMP_SQRMAT("[Ch][Ch']",CH) ;

   exit(0) ;
}
#endif
