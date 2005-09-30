/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "eispack.h"

/*---------------------------------------------------------------------------*/

#undef  DET3
#define DET3(m) ( m[0]*m[4]*m[8]-m[0]*m[7]*m[5]-m[1]*m[3]*m[8] \
                 +m[1]*m[6]*m[5]+m[2]*m[3]*m[7]-m[2]*m[6]*m[4] )

#undef  PI
#define PI 3.14159265358979323846

#undef  MIN
#define MIN(a,b) (((a)>(b)) ? (b) : (a))

#undef  SWAP
#define SWAP(x,y) (th=(x),(x)=(y),(y)=th)

#undef  CSWAP
#define CSWAP(i,j) (SWAP(a[i],a[j]),SWAP(a[i+1],a[j+1]),SWAP(a[i+2],a[j+2]))

#undef  EPS
#undef  EPSQ
#define EPS  1.e-8
#define EPSQ 1.e-4   /* sqrt(EPS) */

/*---------------------------------------------------------------------------*/
/*! Do a 3x3 symmetric eigen-problem.
     - INPUT: double a[9] = input matrix; a[i+3*j] = A(i,j) element
     - OUTPUT: e[i] = i'th eigenvalue, with e[0] <= e[1] <= e[2].
     - OUTPUT: if(dovec) then a[] is replaced with eigenvectors,
               and this orthogonal matrix will have determinant=1.
-----------------------------------------------------------------------------*/

void symeig_3( double *a , double *e , int dovec )
{
   double aa,bb,cc,dd,ee,ff ;
   double a1,a2,a3 , qq,rr, qs,th , lam1,lam2,lam3 ;
   double aba,abb,abc,abd,abe,abf , ann ;
   double d12,d13,d23 ;
   double u1,u2,u3 , v1,v2,v3 , w1,w2,w3 , t1,t2,t3 , tn ;

   if( a == NULL || e == NULL ) return ;

   aa = a[0] ; bb = a[1] ; cc = a[2] ;  /* matrix is [ aa bb cc ]  */
   dd = a[4] ; ee = a[5] ; ff = a[8] ;  /*           [ bb dd ee ]  */
                                        /*           [ cc ee ff ]  */
   aba = fabs(aa) ; abb = fabs(bb) ; abc = fabs(cc) ;
   abd = fabs(dd) ; abe = fabs(ee) ; abf = fabs(ff) ;
   ann = aba+abb+abc+abd+abe+abf   ;                 /* matrix 'norm' */

   if( ann == 0.0 ){             /* matrix is all zero! */
     e[0] = e[1] = e[2] = 0.0 ;
     if( dovec ){
       a[0] = a[4] = a[8] = 1.0 ;
       a[1] = a[2] = a[3] = a[5] = a[6] = a[7] = 0.0 ;
     }
     return ;
   }

   /*----- check for matrix that is essentially diagonal -----*/

   if( EPS*aba > (abb+abc) && EPS*abd > (abb+abe) && EPS*abf > (abc+abe) ){

     lam1 = aa ; lam2 = dd ; lam3 = ff ;

     if( dovec ){
       a[0] = a[4] = a[8] = 1.0 ;
       a[1] = a[2] = a[3] = a[5] = a[6] = a[7] = 0.0 ;

       if( lam1 > lam2 ){ SWAP(lam1,lam2) ; CSWAP(0,3) ; }
       if( lam1 > lam3 ){ SWAP(lam1,lam3) ; CSWAP(0,6) ; }
       if( lam2 > lam3 ){ SWAP(lam2,lam3) ; CSWAP(3,6) ; }
       if( DET3(a) < 0.0 ){ a[6] = -a[6]; a[7] = -a[7]; a[8] = -a[8]; }
     } else {
       if( lam1 > lam2 )  SWAP(lam1,lam2) ;
       if( lam1 > lam3 )  SWAP(lam1,lam3) ;
       if( lam2 > lam3 )  SWAP(lam2,lam3) ;
     }
     e[0] = lam1 ; e[1] = lam2 ; e[2] = lam3 ;
     return ;
   }

   /*----- not diagonal ==> must solve cubic polynomial for eigenvalues -----*/
   /*      the cubic polynomial is x**3 + a1*x**2 + a2*x + a3 = 0            */

   a1 = -(aa+dd+ff) ;
   a2 =  (aa*ff+aa*dd+dd*ff - bb*bb-cc*cc-ee*ee) ;
   a3 =  ( aa*(ee*ee-dd*ff) + bb*(bb*ff-cc*ee) + cc*(cc*dd-bb*ee) ) ;

   qq = (a1*a1 - 3.0*a2) / 9.0 ;
   rr = (2.0*a1*a1*a1 - 9.0*a1*a2 + 27.0*a3) / 54.0 ;

   qs = sqrt(qq) ; rr = rr / (qs*qq) ;
   if( rr < -1.0 ) rr = -1.0 ; else if( rr > 1.0 ) rr = 1.0 ;
   th = acos(rr) ;

   lam1 = -2.0 * qs * cos(  th        /3.0 ) - a1 / 3.0 ;
   lam2 = -2.0 * qs * cos( (th+2.0*PI)/3.0 ) - a1 / 3.0 ;
   lam3 = -2.0 * qs * cos( (th+4.0*PI)/3.0 ) - a1 / 3.0 ;

   /*-- if not doing eigenvectors, just sort the eigenvalues to be done --*/

   if( !dovec ){
     if( lam1 > lam2 ) SWAP(lam1,lam2) ;
     if( lam1 > lam3 ) SWAP(lam1,lam3) ;
     if( lam2 > lam3 ) SWAP(lam2,lam3) ;
     e[0] = lam1 ; e[1] = lam2 ; e[2] = lam3 ;
     return ;
   }

   /*-- are doing eigenvectors; must do double root as a special case --*/

#undef  CROSS
#define CROSS(x1,x2,x3,y1,y2,y3,z1,z2,z3) \
 ( (z1)=(x2)*(y3)-(x3)*(y2), (z2)=(x3)*(y1)-(x1)*(y3), (z3)=(x1)*(y2)-(x2)*(y1) )

   d12 = fabs(lam1-lam2) ; d13 = fabs(lam1-lam3) ; d23 = fabs(lam2-lam3) ;
   rr  = MIN(d12,d13)    ; rr  = MIN(rr,d23)     ;

   if( rr > EPS*ann ){  /*---- not a double root ----*/

     if( lam1 > lam2 )  SWAP(lam1,lam2) ;  /* start by sorting eigenvalues */
     if( lam1 > lam3 )  SWAP(lam1,lam3) ;
     if( lam2 > lam3 )  SWAP(lam2,lam3) ;
     e[0] = lam1 ; e[1] = lam2 ; e[2] = lam3 ;

     /* find eigenvector for lam1 by computing Ay-lam1*y for
        vectors y1=[1,0,0], y2=[0,1,0], and y3=[0,0,1]; the eigenvector
        is orthogonal to all of these, so use the cross product to get it */

     u1 = aa-lam1 ; u2 = bb      ; u3 = cc ;   /* A*y1 - lam1*y1 */
     v1 = bb      ; v2 = dd-lam1 ; v3 = ee ;   /* A*y2 - lam1*y2 */
     CROSS(u1,u2,u3 , v1,v2,v3 , t1,t2,t3 ) ;     tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < EPSQ*ann ){                      /* u and v were parallel? */
       w1 = cc ; w2 = ee ; w3 = ff-lam1 ;      /* A*y3 - lam1*y3 */
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < EPSQ*ann ){                    /* u and w were parallel? */
         CROSS(v1,v2,v3 , w1,w2,w3 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     a[0] = t1/tn ; a[1] = t2/tn ; a[2] = t3/tn ;  /* normalize */

     /* do same for lam2 */

     u1 = aa-lam2 ; u2 = bb      ; u3 = cc ;
     v1 = bb      ; v2 = dd-lam2 ; v3 = ee ;
     CROSS(u1,u2,u3 , v1,v2,v3 , t1,t2,t3 ) ;     tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < EPSQ*ann ){
       w1 = cc ; w2 = ee ; w3 = ff-lam2 ;
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < EPSQ*ann ){
         CROSS(v1,v2,v3 , w1,w2,w3 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     a[3] = t1/tn ; a[4] = t2/tn ; a[5] = t3/tn ;

     /* orthgonality of eigenvectors ==> can get last one by cross product */

#if 1
     CROSS( a[0],a[1],a[2] , a[3],a[4],a[5] , a[6],a[7],a[8] ) ;
#else
     u1 = aa-lam3 ; u2 = bb      ; u3 = cc ;
     v1 = bb      ; v2 = dd-lam3 ; v3 = ee ;
     CROSS(u1,u2,u3 , v1,v2,v3 , t1,t2,t3 ) ;     tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < EPSQ*ann ){
       w1 = cc ; w2 = ee ; w3 = ff-lam3 ;
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < EPSQ*ann ){
         CROSS(v1,v2,v3 , w1,w2,w3 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     a[6] = t1/tn ; a[7] = t2/tn ; a[8] = t3/tn ;
#endif

     return ;

   } else { /*---- if here, we have a double root ----*/

     /* make sure that we have lam1=lam2 and lam3 is the outlier */

          if( d13 < d12 && d13 < d23 ) SWAP(lam2,lam3) ;
     else if( d23 < d12 && d23 < d13 ) SWAP(lam1,lam3) ;
     lam1 = lam2 = 0.5*(lam1+lam2) ;

     /* compute eigenvector for lam3 using method as above */

     u1 = aa-lam3 ; u2 = bb      ; u3 = cc ;
     v1 = bb      ; v2 = dd-lam3 ; v3 = ee ;
     CROSS(u1,u2,u3 , v1,v2,v3 , t1,t2,t3 ) ;     tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < EPSQ*ann ){
       w1 = cc ; w2 = ee ; w3 = ff-lam3 ;
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < EPSQ*ann ){
         CROSS(v1,v2,v3 , w1,w2,w3 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     w1 = a[6] = t1/tn ; w2 = a[7] = t2/tn ; w3 = a[8] = t3/tn ;

     /* find a vector orthogonal to it */

     CROSS(w1,w2,w3 , 1,0,0 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < EPSQ ){
       CROSS(w1,w2,w3 , 0,1,0 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < EPSQ ){
         CROSS(w1,w2,w3 , 0,0,1 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     a[0] = t1/tn ; a[1] = t2/tn ; a[2] = t3/tn ;

     /* and the final vector is the cross product of these two */

     CROSS( w1,w2,w3 , a[0],a[1],a[2] , a[3],a[4],a[5] ) ;

     /* sort results (we know lam1==lam2) */

     if( lam1 > lam3 ){
       SWAP(lam1,lam3) ; CSWAP(0,6) ;
       if( DET3(a) < 0.0 ){ a[6] = -a[6]; a[7] = -a[7]; a[8] = -a[8]; }
     }

     e[0] = lam1 ; e[1] = lam2 ; e[2] = lam3 ;
     return ;
   }
}

/*---------------------------------------------------------------------------*/

void symeig_2( double *a , double *e , int dovec )
{
   double sxx,sxy,syy , lam1,lam2 , ss,tt , x,y ;

   if( a == NULL || e == NULL ) return ;

   sxx = a[0] ; sxy = a[1] ; syy = a[3] ;

   ss = fabs(sxx) ; tt = fabs(syy) ; if( ss > tt ) ss = tt ;

   if( fabs(sxy) < EPS*ss ){   /* essentially a diagonal matrix */
     if( sxx <= syy ){
       lam1 = sxx ; lam2 = syy ;
       if( dovec ){ a[0]=a[3]=1.0; a[1]=a[2]=0.0; }
     } else {
       lam1 = syy ; lam2 = sxx ;
       if( dovec ){ a[0]=a[3]=1.0 ; a[1]=a[2]=1.0; }
     }
     e[0] = lam1 ; e[1] = lam2 ;
     return ;
   }

   /* non-diagonal matrix ==> solve quadratic equation for eignenvalues */

   ss = sqrt( (sxx-syy)*(sxx-syy) + 4.0*sxy*sxy ) ;
   lam1 = 0.5 * ( sxx + syy - ss ) ;  /* smaller */
   lam2 = 0.5 * ( sxx + syy + ss ) ;  /* larger */

   if( dovec ){
     x = 2.0*sxy ; y = syy - sxx - ss ; tt = sqrt(x*x+y*y) ;
     a[0] = x/tt ; a[1] = y/tt ;

     y = syy - sxx + ss ; tt = sqrt(x*x+y*y) ;
     a[2] = x/tt ; a[3] = y/tt ;
   }
   e[0] = lam1 ; e[1] = lam2 ;
   return ;
}

/*--------------------------------------------------------------------------*/

static int forbid_23 = 0 ;
void symeig_forbid_23( int ii ){ forbid_23 = ii ; return ; }

/*--------------------------------------------------------------------------
   Compute the eigenvalue/vector decomposition of a symmetric matrix,
   stored in double precision.
     n = order of matrix
     a = on input: matrix(i,j) is in a[i+n*j] for i=0..n-1 , j=0..n-1
           output: a[i+n*j] has the i'th component of the j'th eigenvector
     e = on input: not used (but the calling program must
                             allocate the space for e[0..n-1])
           output: e[j] has the j'th eigenvalue, ordered so that
           e[0] <= e[1] <= ... <= e[n-1]
   Uses the f2c translation of EISPACK.
----------------------------------------------------------------------------*/

void symeig_double( int n , double *a , double *e )
{
   integer nm , matz , ierr ;
   double *fv1 , *fv2 ;

   if( a == NULL || e == NULL || n < 1 ) return ;

   /* special cases of small n */

   if( n == 1 ){
     e[0] = a[0] ; a[0] = 1.0 ; return ;  /* degenerate case */
   } else if( !forbid_23 ){
     if( n == 2 ){ symeig_2( a , e , 1 ) ; return ; }
     if( n == 3 ){ symeig_3( a , e , 1 ) ; return ; }
   }

   /*-- default code: n > 3 --*/

   fv1 = (double *) malloc(sizeof(double)*n) ;  /* workspaces */
   fv2 = (double *) malloc(sizeof(double)*n) ;

   nm = n ; matz = 1 ; ierr = 0 ;

   rs_( &nm , &nm , a , e , &matz , a , fv1 , fv2 , &ierr ) ;

   free((void *)fv1) ; free((void *)fv2) ;
   return ;
}

/*------------------ just compute the eigenvalues -------------------*/

void symeigval_double( int n , double *a , double *e )
{
   integer nm , matz , ierr ;
   double *fv1 , *fv2 ;

   if( a == NULL || e == NULL || n < 1 ) return ;

   /* special cases of small n */

   if( n == 1 ){
     e[0] = a[0] ; return ;  /* degenerate case */
   } else if( !forbid_23 ){
     if( n == 2 ){ symeig_2( a , e , 0 ) ; return ; }
     if( n == 3 ){ symeig_3( a , e , 0 ) ; return ; }
   }

   /*-- here, deal with general n > 3 --*/

   fv1 = (double *) malloc(sizeof(double)*n) ;  /* workspaces */
   fv2 = (double *) malloc(sizeof(double)*n) ;

   nm = n ; matz = 0 ; ierr = 0 ;

   rs_( &nm , &nm , a , e , &matz , a , fv1 , fv2 , &ierr ) ;

   free((void *)fv1) ; free((void *)fv2) ;
   return ;
}

/*--------------------------------------------------------------------*/

#define CHECK_SVD

#undef CHK
#ifdef CHECK_SVD
# define CHK 1
# define A(i,j) aa[(i)+(j)*m]
# define U(i,j) uu[(i)+(j)*m]
# define V(i,j) vv[(i)+(j)*n]
#else
# define CHK 0
#endif

/*--------------------------------------------------------------------*/
/*! Compute SVD of double precision matrix:                      T
                                            [a] = [u] diag[s] [v]
    - m = # of rows in a
    - n = # of columns in a
    - a = pointer to input matrix; a[i+j*m] has the (i,j) element
    - s = pointer to output singular values; length = n
    - u = pointer to output matrix, if desired; length = m*n
    - v = pointer to output matrix, if desired; length = n*n

----------------------------------------------------------------------*/

void svd_double( int m , int n , double *a , double *s , double *u , double *v )
{
   integer mm,nn , lda,ldu,ldv , ierr ;
   doublereal *aa, *ww , *uu , *vv , *rv1 ;
   logical    matu , matv ;

   if( a == NULL || s == NULL || m < 1 || n < 1 ) return ;

   mm  = m ;
   nn  = n ;
   aa  = a ;
   lda = m ;
   ww  = s ;

   if( u == NULL ){
     matu = (logical) CHK ;
     uu   = (doublereal *)malloc(sizeof(double)*m*n) ;
   } else {
     matu = (logical) 1 ;
     uu = u ;
   }
   ldu = m ;

   if( v == NULL ){
     matv = (logical) CHK ;
     vv   = (CHK) ? (doublereal *)malloc(sizeof(double)*n*n) : NULL ;
   } else {
     matv = (logical) 1 ;
     vv   = v ;
   }
   ldv = n ;

   rv1 = (double *) malloc(sizeof(double)*n) ;  /* workspace */

   (void) svd_( &mm , &nn , &lda , aa , ww ,
                &matu , &ldu , uu , &matv , &ldv , vv , &ierr , rv1 ) ;

#ifdef CHECK_SVD
   { register int i,j,k ; register doublereal aij ; double err ;
     err = 0.0 ;
     for( j=0 ; j < n ; j++ ){
      for( i=0 ; i < m ; i++ ){
        aij = A(i,j) ;
        for( k=0 ; k < n ; k++ ) aij -= U(i,k)*V(j,k)*ww[k] ;
        err += fabs(aij) ;
     }}
     err /= (m*n) ;
     if( err >= 1.e-5 ){
       fprintf(stderr,"++ WARNING: svd err=%g; recomputing ...\n",err) ;
       (void) svd_slow_( &mm , &nn , &lda , aa , ww ,
                         &matu , &ldu , uu , &matv , &ldv , vv , &ierr , rv1 ) ;
       err = 0.0 ;
       for( j=0 ; j < n ; j++ ){
        for( i=0 ; i < m ; i++ ){
          aij = A(i,j) ;
          for( k=0 ; k < n ; k++ ) aij -= U(i,k)*V(j,k)*ww[k] ;
          err += fabs(aij) ;
       }}
       err /= (m*n) ;
       fprintf(stderr,"++ WARNING: recomputed svd err=%g %s\n",
               err , (err >= 1.e-5) ? "**BAD**" : "**OK**"      ) ;
     }
   }
#endif

   free((void *)rv1) ;

   if( u == NULL && uu != NULL ) free((void *)uu) ;
   if( v == NULL && vv != NULL ) free((void *)vv) ;
   return ;
}
