#include "mrilib.h"

typedef void matvecfunc( int n , float *vin, float *vout , void *mat ) ;

int simult_eig_generic( int nn ,
                        void (*mvfunc)(int,float *,float*,void *) , void *mat ,
                        int mev , int gev , float *eval , float **evec )
{
   int n=nn , m , jbot ;
   float *umat ;  /* n X m */
   float *gmat ;  /* m X m */
   float *hmat ;  /* m X m */
   float *ymat ;  /* m X m */
   float *vmat ;  /* n X m */
   register int ii,jj,kk ; register float sum, *aar, *bar ;

   if( n < 2 || mvfunc == NULL || eval == NULL || mev >= n ) return(-1) ;

   if( evec != NULL ){
     for( jj=0 ; jj < mev ; jj++ ) if( evec[jj] == NULL ) return(-1) ;
   }

   if( gev < 0 || mev+gev >= n ) gev = 0 ;

   m = mev + gev ;

   umat = (float *)malloc( sizeof(float)*n*m ) ;
   vmat = (float *)malloc( sizeof(float)*n*m ) ;
   gmat = (float *)malloc( sizeof(float)*m*m ) ;
   hmat = (float *)malloc( sizeof(float)*m*m ) ;
   ymat = (float *)malloc( sizeof(float)*m*m ) ;

#define U(i,j) umat[(i)+(j)*n]
#define V(i,j) vmat[(i)+(j)*n]
#define G(i,j) gmat[(i)+(j)*m]
#define L      G
#define H(i,j) hmat[(i)+(j)*m]
#define Y(i,j) ymat[(i)+(j)*m]

   /** initialize trial eigenvectors **/

   if( evec != NULL ){               /** copy from evec **/
     for( jj=0 ; jj < mev ; jj++ ){
       memcpy( umat+jj*n , evec[jj] , sizeof(float)*n ) ;
       for( ii=0 ; ii < n && U(ii,jj)==0.0f ; ii++ ) ; /*nada*/
       if( ii == n ) break ;  /* all zero ==> quit now */
     }
     jbot = jj ;
   } else {
     jbot = 0 ;
   }

   for( jj=jbot ; jj < m ; jj++ ){  /** random stuff **/
     aar = umat+jj*n ;
     for( ii=0 ; ii < n ; ii++ ) aar[ii] = drand48()-0.5 ;
   }

   for( jj=0 ; jj < m ; jj++ ){     /** normalize **/
     sum = 0.0f ; aar = umat+jj*n ;
     for( ii=0 ; ii < n ; ii++ ) sum += aar[ii]*aar[ii] ;
     sum = 1.0f / sqrtf(sum) ;
     for( ii=0 ; ii < n ; ii++ ) aar[ii] *= sum ;
   }

   /**-------------------------- iteration loop --------------------------**/

   while(1){

     /** V = A U -- multiply matrix into each trial vector **/

     for( jj=0 ; jj < m ; jj++ )
       mvfunc( n , umat+jj*n , vmat+jj*n , mat ) ;

     /** G = U' U -- lower triangle only -- G(ii,jj) for ii >= jj **/

     for( jj=0 ; jj < m ; jj++ ){
       aar = umat+jj*n ;
       for( ii=jj ; ii < m ; ii++ ){
         bar = umat+ii*n ; sum = 0.0f ;
         for( kk=0 ; kk < n ; kk++ ) sum += aar[kk]*bar[kk] ;
         G(ii,jj) = sum ;
       }
     }

     /** H = U' V -- symmetric since matrix A (mvfunc) is symmetric **/

     for( jj=0 ; jj < m ; jj++ ){
       aar = umat+jj*n ;
       for( ii=jj ; ii < m ; ii++){
         bar = vmat+ii*n ; sum = 0.0f ;
         for( kk=0 ; kk < n ; kk++ ) sum += aar[kk]*bar[kk] ;
         H(ii,jj) = sum ; if( ii > jj ) H(jj,ii) = sum ;
       }
     }

     /** Choleski decompose G in place (call this L from now on) **/

     G(0,0) = sqrtf(G(0,0)) ;
     for( ii=1 ; ii < n ; ii++ ){
       for( jj=0 ; jj < ii ; jj++ ){
         sum = G(ii,jj) ;
         for( kk=0 ; kk < jj ; kk++ ) sum -= G(ii,kk)*G(jj,kk) ;
         G(ii,jj) = sum / G(jj,jj) ;
       }
       sum = G(ii,ii) ;
       for( kk=0 ; kk < ii ; kk++ ) sum -= G(ii,kk)*G(ii,kk) ;
       G(ii,ii) = sqrtf(sum) ;
     }

     /** solve L Y = H for Y **/

     /** transpose Y in place (call this Y' afterwards) **/

     /** solve L B = Y' for B **/

     /** eigensolve m X m matrix B to have B P = P diag[lam] **/

     /** solve L' P = Q for Q **/

     /** W = V Q, and normalize to get updated eigenvectors **/

     /** test for convergence **/

   }

   /** save eigenvalues **/

   /** save eigenvectors, if desired **/

   /** toss the trash and exit hurriedly, pursued by a bear **/

   return(mev) ;
}
