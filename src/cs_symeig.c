/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "eispack.h"

/*---------------------------------------------------------------------------*/

#undef  SQR
#define SQR(a)  ((a)*(a))

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
   double anni ;

   if( a == NULL || e == NULL ) return ;

   /*----- unload matrix into local variables -----*/

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

   if( abb+abc+abe == 0.0 ||
       ( EPS*aba > (abb+abc) && EPS*abd > (abb+abe) && EPS*abf > (abc+abe) ) ){

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

   /*-- Scale matrix so abs sum is 1; unscale e[i] on output [26 Oct 2005] --*/

   anni = 1.0 / ann ;                      /* ann != 0, from above */
   aa *= anni ; bb *= anni ; cc *= anni ;
   dd *= anni ; ee *= anni ; ff *= anni ;

   /*----- not diagonal ==> must solve cubic polynomial for eigenvalues -----*/
   /*      the cubic polynomial is x**3 + a1*x**2 + a2*x + a3 = 0            */

   a1 = -(aa+dd+ff) ;
   a2 =  (aa*ff+aa*dd+dd*ff - bb*bb-cc*cc-ee*ee) ;
   a3 =  ( aa*(ee*ee-dd*ff) + bb*(bb*ff-cc*ee) + cc*(cc*dd-bb*ee) ) ;

   /*-- Rewrite classical formula for qq as a sum of squares [26 Oct 2005] --*/
#if 0
   qq = (a1*a1 - 3.0*a2) / 9.0 ;
#else
   qq = (  0.5 * ( SQR(dd-aa) + SQR(ff-aa) + SQR(ff-dd) )
         + 3.0 * ( bb*bb      + cc*cc      + ee*ee      ) ) / 9.0 ;
#endif
   rr = (2.0*a1*a1*a1 - 9.0*a1*a2 + 27.0*a3) / 54.0 ;

   if( qq <= 0.0 ){       /*** This should never happen!!! ***/
     static int nerr=0 ;
#pragma omp critical (STDERR)
     {
     if( ++nerr < 4 )
       fprintf(stderr,"** ERROR in symeig_3: discrim=%g numer=%g\n",qq,rr) ;
     }
     qs = qq = rr = 0.0 ;
   } else {
     qs = sqrt(qq) ; rr = rr / (qs*qq) ;
     if( rr < -1.0 ) rr = -1.0 ; else if( rr > 1.0 ) rr = 1.0 ;
   }
   th = acos(rr) ;

   lam1 = -2.0 * qs * cos(  th        /3.0 ) - a1 / 3.0 ;
   lam2 = -2.0 * qs * cos( (th+2.0*PI)/3.0 ) - a1 / 3.0 ;
   lam3 = -2.0 * qs * cos( (th+4.0*PI)/3.0 ) - a1 / 3.0 ;

   /*-- if not doing eigenvectors, just sort the eigenvalues to be done --*/

   if( !dovec ){
     if( lam1 > lam2 ) SWAP(lam1,lam2) ;
     if( lam1 > lam3 ) SWAP(lam1,lam3) ;
     if( lam2 > lam3 ) SWAP(lam2,lam3) ;
     e[0] = ann*lam1 ; e[1] = ann*lam2 ; e[2] = ann*lam3 ;
     return ;
   }

   /*-- are doing eigenvectors; must do double root as a special case --*/

#undef  CROSS  /* cross product (x1,x2,x3) X (y1,y2,y3) -> (z1,z2,z3) */
#define CROSS(x1,x2,x3,y1,y2,y3,z1,z2,z3) \
 ( (z1)=(x2)*(y3)-(x3)*(y2), (z2)=(x3)*(y1)-(x1)*(y3), (z3)=(x1)*(y2)-(x2)*(y1) )

   d12 = fabs(lam1-lam2) ; d13 = fabs(lam1-lam3) ; d23 = fabs(lam2-lam3) ;
   rr  = MIN(d12,d13)    ; rr  = MIN(rr,d23)     ;

   if( rr > EPS*ann ){  /*---- not a double root ----*/

     if( lam1 > lam2 )  SWAP(lam1,lam2) ;  /* start by sorting eigenvalues */
     if( lam1 > lam3 )  SWAP(lam1,lam3) ;
     if( lam2 > lam3 )  SWAP(lam2,lam3) ;
     e[0] = ann*lam1 ; e[1] = ann*lam2 ; e[2] = ann*lam3 ;

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

     e[0] = ann*lam1 ; e[1] = ann*lam2 ; e[2] = ann*lam3 ;
     return ;
   }
}

/*---------------------------------------------------------------------------*/
/*! 2x2 symmetric eigenvalue/vector problem, like symeig_3() above. */

void symeig_2( double *a , double *e , int dovec )
{
   double sxx,sxy,syy , lam1,lam2 , ss,tt , x,y ;

   if( a == NULL || e == NULL ) return ;

   /*----- unload matrix into local variables -----*/

   sxx = a[0] ; sxy = a[1] ; syy = a[3] ;

   ss = fabs(sxx) ; tt = fabs(syy) ; if( ss > tt ) ss = tt ;

   if( fabs(sxy) < EPS*ss ){   /*--- essentially a diagonal matrix ---*/
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

   /*--- non-diagonal matrix ==> solve quadratic equation for eigenvalues ---*/

   ss = sqrt( (sxx-syy)*(sxx-syy) + 4.0*sxy*sxy ) ;   /* positive */
   lam1 = 0.5 * ( sxx + syy - ss ) ;                  /* smaller */
   lam2 = 0.5 * ( sxx + syy + ss ) ;                  /* larger */

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

/*! To turn off use of symeig_3() and symeig_2() in the symeig functions. */

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

   /* special cases of small n (much faster than EISPACK) */

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

   /* special cases of small n (much faster than EISPACK) */

   if( n == 1 ){
     e[0] = a[0] ; return ;  /* degenerate case */
   } else if( !forbid_23 ){
     if( n == 2 ){ symeig_2( a , e , 0 ) ; return ; }
     if( n == 3 ){ symeig_3( a , e , 0 ) ; return ; }
   }

   /*-- here, deal with general n > 3 --*/

   fv1 = (double *) malloc(sizeof(double)*(n+9)) ;  /* workspaces */
   fv2 = (double *) malloc(sizeof(double)*(n+9)) ;

   nm = n ; matz = 0 ; ierr = 0 ;

   rs_( &nm , &nm , a , e , &matz , a , fv1 , fv2 , &ierr ) ;

   if( ierr != 0 )
     fprintf(stderr,"** ERROR: symeigval_double error code = %d\n",(int)ierr) ;

   free((void *)fv1) ; free((void *)fv2) ;
   return ;
}

/*--------------------------------------------------------------------*/
/*! Return eigenvalues/eigenvectors indexed from bb to tt (bb <= tt),
    where index #0 = smallest eigenvalue, index #n-1 = largest
     n = order of matrix
     a = on input: matrix(i,j) is in a[i+n*j] for i=0..n-1 , j=0..n-1
           output: a[i+n*j] has the i'th component of the j'th
                   eigenvector, for j=0..tt-bb.
         However, if novec!=0, then eigenvectors will not be computed;
         only the eigenvalues will be output.
     e = on input: not used (but the calling program must
                             allocate the space for e[0..tt-bb])
           output: e[j] has the j'th eigenvalue, ordered so that
           e[0] <= e[1] <= ... <= e[tt-bb]

    Return value is 0 for all being done OK, nonzero for error.
*//*------------------------------------------------------------------*/

int symeig_irange( int n, double *a, double *e, int bb, int tt, int novec )
{
   integer nm , m11,mmm , ierr , *ind ;
   double *fv1, *fv2, *fv3, eps1, lb,ub, *rv4,*rv5,*rv6,*rv7,*rv8, *zzz ;
   int ii , nval ;

   if( n < 1 || a == NULL || e == NULL || bb < 0 || tt < bb || tt >= n )
     return -66666 ;

   if( bb==0 && tt==n-1 ){ symeig_double( n , a , e ) ; return 0 ; }

   /* reduction to tridiagonal form (stored in fv1..3) */

   nm  = n ;
   fv1 = (double *) malloc(sizeof(double)*(n+9)) ;  /* workspaces */
   fv2 = (double *) malloc(sizeof(double)*(n+9)) ;
   fv3 = (double *) malloc(sizeof(double)*(n+9)) ;

   tred1_( &nm , &nm , a , fv1,fv2,fv3 ) ;

   /* determination of the desired eigenvalues of the tridiagonal matrix */

   eps1 = 0.0 ;
   m11  = bb+1 ;
   mmm  = tt-bb+1 ;
   ierr = 0 ;
   ind  = (integer *)malloc(sizeof(integer)*(n+9)) ;
   rv4  = (double *) malloc(sizeof(double) *(n+9)) ;
   rv5  = (double *) malloc(sizeof(double) *(n+9)) ;

   tridib_( &nm , &eps1 , fv1,fv2,fv3 , &lb,&ub , &m11,&mmm , e ,
            ind , &ierr , rv4,rv5 ) ;

   if( ierr != 0 || novec != 0 ){
     free(rv5); free(rv4); free(ind); free(fv3); free(fv2); free(fv1);
     return -ierr ;
   }

   /* determination of the eigenvectors of the tridiagonal matrix */

   nval = nm * mmm ;
   zzz  = (double *) malloc(sizeof(double) *nval ) ;
   rv6  = (double *) malloc(sizeof(double) *(n+9)) ;
   rv7  = (double *) malloc(sizeof(double) *(n+9)) ;
   rv8  = (double *) malloc(sizeof(double) *(n+9)) ;

   tinvit_( &nm , &nm , fv1,fv2,fv3 , &mmm , e ,
            ind , zzz , &ierr , rv4,rv5,rv6,rv7,rv8 ) ;

   if( ierr != 0 ){
     free(rv8); free(rv7); free(rv6); free(zzz);
     free(rv5); free(rv4); free(ind); free(fv3); free(fv2); free(fv1);
     return ierr ;
   }

   /* transform eigenvectors back to original space */

   trbak1_( &nm , &nm , a , fv2 , &mmm , zzz ) ;

   /* copy output eigenvectors into a */

   for( ii=0 ; ii < nval ; ii++ ) a[ii] = zzz[ii] ;

   free(rv8); free(rv7); free(rv6); free(zzz);
   free(rv5); free(rv4); free(ind); free(fv3); free(fv2); free(fv1);
   return 0 ;
}

/*----------------------------------------------------------------------------*/

#undef  A
#define A(i,j) asym[(i)+(j)*nsym]

/*----------------------------------------------------------------------------*/
/*! Compute the nvec principal singular vectors of a set of m columns, each
    of length n, stored in array xx[i+j*n] for i=0..n-1, j=0..m-1.
    The singular values (largest to smallest) are stored in sval, and
    the left singular vectors [first nvec columns of U in X = U S V'] are
    stored into uvec[i+j*n] for i=0..n-1, j=0..nvec-1.

    The return value is the number of vectors computed.  If the return
    value is not positive, something bad happened.  Normally, the return
    value would be the same as nvec, but it cannot be larger than MIN(n,m).

    If sval==NULL, then the output into sval is skipped.
    If uval==NULL, then the output into uval is skipped.
    If both are NULL, exactly why did you want to call this function?
*//*--------------------------------------------------------------------------*/

int first_principal_vectors( int n , int m , float *xx ,
                             int nvec , float *sval , float *uvec )
{
   int nn=n , mm=m , nsym , ii,jj,kk,qq ;
   double *asym , *deval ;
   register double sum , qsum ; register float *xj , *xk ;

   nsym = MIN(nn,mm) ;  /* size of the symmetric matrix to create */

   if( nsym < 1 || xx == NULL || (uvec == NULL && sval == NULL) ) return -666 ;

   if( nvec > nsym ) nvec = nsym ;  /* can't compute more vectors than nsym! */

#pragma omp critical (MALLOC)
   { asym  = (double *)malloc(sizeof(double)*nsym*nsym) ;  /* symmetric matrix */
     deval = (double *)malloc(sizeof(double)*nsym) ;       /* its eigenvalues */
   }

   /** setup matrix to eigensolve: choose smaller of [X]'[X] and [X][X]' **/
   /**     since [X] is n x m, [X]'[X] is m x m and [X][X]' is n x n     **/

   if( nn > mm ){                       /* more rows than columns:  */
                                        /* so [A] = [X]'[X] = m x m */
     int n1 = nn-1 ;
     for( jj=0 ; jj < mm ; jj++ ){
       xj = xx + jj*nn ;
       for( kk=0 ; kk <= jj ; kk++ ){
         sum = 0.0 ; xk = xx + kk*nn ;
         for( ii=0 ; ii < n1 ; ii+=2 ) sum += xj[ii]*xk[ii] + xj[ii+1]*xk[ii+1];
         if( ii == n1 ) sum += xj[ii]*xk[ii] ;
         A(jj,kk) = sum ; if( kk < jj ) A(kk,jj) = sum ;
       }
     }

   } else {                             /* more columns than rows:  */
                                        /* so [A] = [X][X]' = n x n */
     float *xt ; int m1=mm-1 ;
#pragma omp critical (MALLOC)
     xt = (float *)malloc(sizeof(float)*nn*mm) ;
     for( jj=0 ; jj < mm ; jj++ ){      /* form [X]' into array xt */
       for( ii=0 ; ii < nn ; ii++ ) xt[jj+ii*mm] = xx[ii+jj*nn] ;
     }

     for( jj=0 ; jj < nn ; jj++ ){
       xj = xt + jj*mm ;
       for( kk=0 ; kk <= jj ; kk++ ){
         sum = 0.0 ; xk = xt + kk*mm ;
         for( ii=0 ; ii < m1 ; ii+=2 ) sum += xj[ii]*xk[ii] + xj[ii+1]*xk[ii+1];
         if( ii == m1 ) sum += xj[ii]*xk[ii] ;
         A(jj,kk) = sum ; if( kk < jj ) A(kk,jj) = sum ;
       }
     }

#pragma omp critical (MALLOC)
     free(xt) ;  /* don't need this no more */
   }

   /** compute the nvec eigenvectors corresponding to largest eigenvalues **/
   /** these eigenvectors are stored on top of first nvec columns of asym **/

   ii = symeig_irange( nsym, asym, deval, nsym-nvec, nsym-1, (uvec==NULL) ) ;

   if( ii != 0 ){
#pragma omp critical (MALLOC)
     { free(deval) ; free(asym) ; }
     return -33333 ;  /* eigensolver failed!? */
   }

   /** Store singular values (sqrt of eigenvalues), if desired:
       Note that symeig_irange returns things smallest to largest,
       but we want largest to smallest, so have to reverse the order **/

   if( sval != NULL ){
     for( jj=0 ; jj < nvec ; jj++ ){
       sum = deval[nvec-1-jj] ;
       sval[jj] = (sum <= 0.0) ? 0.0 : sqrt(sum) ;
     }
   }

   /** if no output vectors desired, we are done done done!!! **/

   if( uvec == NULL ){
#pragma omp critical (MALLOC)
     { free(deval) ; free(asym) ; }
     return nvec ;
   }

   /** SVD is [X] = [U] [S] [V]', where [U] = desired output vectors

       case n <= m: [A] = [X][X]' = [U] [S][S]' [U]'
                    so [A][U] = [U] [S][S]'
                    so eigenvectors of [A] are just [U]

       case n > m:  [A] = [X]'[X] = [V] [S]'[S] [V]'
                    so [A][V] = [V] [S'][S]
                    so eigenvectors of [A] are [V], but we want [U]
                    note that [X][V] = [U] [S]
                    so pre-multiplying each column vector in [V] by matrix [X]
                    will give the corresponding column in [U], but scaled;
                    below, just L2-normalize the column to get output vector **/

	if( nn <= mm ){                    /* copy eigenvectors into output directly */
                                      /* (e.g., more vectors than time points) */
     for( jj=0 ; jj < nvec ; jj++ ){
       qq = nvec-1-jj ;               /* eigenvalues are in reversed order */
       for( ii=0 ; ii < nn ; ii++ )
         uvec[ii+jj*nn] = (float)asym[ii+qq*nn] ;
     }

   } else {  /* n > m: transform eigenvectors to get left singular vectors */
             /* (e.g., more time points than vectors) */

     for( jj=0 ; jj < nvec ; jj++ ){
       qq = nvec-1-jj ; qsum = 0.0 ;  /* eigenvalues are in reversed order */
       for( ii=0 ; ii < nn ; ii++ ){
         sum = 0.0 ;
         for( kk=0 ; kk < mm ; kk++ ) sum += xx[ii+kk*nn] * asym[kk+qq*mm] ;
         uvec[ii+jj*nn] = sum ; qsum += sum*sum ;
       }
       if( qsum > 0.0 ){       /* L2 normalize */
         register float fac ;
         fac = (float)(1.0/sqrt(qsum)) ;
         for( ii=0 ; ii < nn ; ii++ ) uvec[ii+jj*nn] *= fac ;
       }
     }
   }

   /** free at last!!! **/

#pragma omp critical (MALLOC)
   { free(deval) ; free(asym) ; }
   return nvec ;
}

#undef A

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

/** setup for sorting SVD values:
      0 = no sort (whatever the function returns)
     +1 = sort in increasing order of singular values
     -1 = sort in descending order of singular values **/

static int svd_sort = 0 ;
void set_svd_sort( int ss ){ svd_sort = ss; }

/*----------------------------------------------------------------------------*/
/*! Compute SVD of double precision matrix:                      T
                                            [a] = [u] diag[s] [v]
    - m = # of rows in a = length of each column
    - n = # of columns in a = length of each row
    - a = pointer to input matrix; a[i+j*m] has the (i,j) element
          (m X n matrix, stored in column-first order)
    - s = pointer to output singular values; length = n (cannot be NULL)
    - u = pointer to output matrix, if desired; length = m*n (m X n matrix)
    - v = pointer to output matrix, if desired; length = n*n (n x n matrix)

  Modified 10 Jan 2007 to add sorting of s and corresponding columns of u & v.
------------------------------------------------------------------------------*/

void svd_double( int m, int n, double *a, double *s, double *u, double *v )
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

   /* make space for u matrix, if not supplied */

   if( u == NULL ){
     matu = (logical) CHK ;
     uu   = (doublereal *)malloc(sizeof(double)*m*n) ;
   } else {
     matu = (logical) 1 ;
     uu = u ;
   }
   ldu = m ;

   /* make space for v matrix if not supplied */

   if( v == NULL ){
     matv = (logical) CHK ;
     vv   = (CHK) ? (doublereal *)malloc(sizeof(double)*n*n) : NULL ;
   } else {
     matv = (logical) 1 ;
     vv   = v ;
   }
   ldv = n ;

   rv1 = (double *) malloc(sizeof(double)*n) ;  /* workspace */

   /** the actual SVD **/

   (void) svd_( &mm , &nn , &lda , aa , ww ,
                &matu , &ldu , uu , &matv , &ldv , vv , &ierr , rv1 ) ;

#ifdef CHECK_SVD
   /** back-compute [A] from [U] diag[ww] [V]'
       and see if it is close to the input matrix;
       if not, compute the results in another function;
       this is needed because the svd() function compiles with
       rare computational errors on some compilers' optimizers **/
   { register int i,j,k ; register doublereal aij ; double err ;
     err = 0.0 ;
     for( j=0 ; j < n ; j++ ){
      for( i=0 ; i < m ; i++ ){
        aij = A(i,j) ;
        for( k=0 ; k < n ; k++ ) aij -= U(i,k)*V(j,k)*ww[k] ;
        err += fabs(aij) ;
     }}
     err /= (m*n) ;  /* average absolute error per matrix element */
     if( err >= 1.e-5 ){
       WARNING_message("SVD avg err=%g; recomputing ...\n",err) ;
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
       WARNING_message("Recomputed SVD avg err=%g %s\n",
               err , (err >= 1.e-5) ? "**BAD**" : "**OK**"      ) ;
     }
   }
#endif

   free((void *)rv1) ;

   /* discard [u] and [v] spaces if not needed for output */

   if( u == NULL && uu != NULL ) free((void *)uu) ;
   if( v == NULL && vv != NULL ) free((void *)vv) ;

   /*--- 10 Jan 2007: sort the singular values and columns of U and V ---*/

   if( n > 1 && svd_sort != 0 ){
     double *sv , *uv ; int *iv , jj,kk ;
     sv = (double *)malloc(sizeof(double)*n) ;
     iv = (int *)   malloc(sizeof(int)   *n) ;
     for( kk=0 ; kk < n ; kk++ ){
       iv[kk] = kk ; sv[kk] = (svd_sort > 0) ? s[kk] : -s[kk] ;
     }
     qsort_doubleint( n , sv , iv ) ;
     if( u != NULL ){
       double *cc = (double *)malloc(sizeof(double)*m*n) ;
#pragma omp critical (MEMCPY)
       (void)memcpy( cc , u , sizeof(double)*m*n ) ;
       for( jj=0 ; jj < n ; jj++ ){
         kk = iv[jj] ;  /* where the new jj-th col came from */
#pragma omp critical (MEMCPY)
         (void)memcpy( u+jj*m , cc+kk*m , sizeof(double)*m ) ;
       }
       free((void *)cc) ;
     }
     if( v != NULL ){
       double *cc = (double *)malloc(sizeof(double)*n*n) ;
#pragma omp critical (MEMCPY)
       (void)memcpy( cc , v , sizeof(double)*n*n ) ;
       for( jj=0 ; jj < n ; jj++ ){
         kk = iv[jj] ;
#pragma omp critical (MEMCPY)
         (void)memcpy( v+jj*n , cc+kk*n , sizeof(double)*n ) ;
       }
       free((void *)cc) ;
     }
     for( kk=0 ; kk < n ; kk++ )
       s[kk] = (svd_sort > 0) ? sv[kk] : -sv[kk] ;
     free((void *)iv) ; free((void *)sv) ;
   }

   return ;
}

/*--------------------------------------------------------------------*/

#define DBG_PC 0
/*!
   Calculate the covariance matrix of data_mat based on code in 3dpc

   data_mat: Data matrix containg num_cols vectors that have num_rows elements.
   cov_mat:  On output, cov_mat will contain the covariance matrix. Caller must
             allocate num_cols x num_cols elements for it.
   Both matrices are stored in column major order. M(i,j) = Mv(i+j*num_rows);
   row_mask: mask vector num_rows long.
            NULL for no masking.
   num_rows, num_cols: 1st and 2nd dimensions of data_mat
   norm: flag for normalizing covariance.
         -1 no normalization,
         0, normalize by num_rows - 1,
         1, normalize by num_rows
   remove_mean: 0  : do nothing
                1  : remove the mean of each column in data_mat (like -dmean in 3dpc)

   To match matlab's cov function, you need to remove the mean of each column
   and set norm to 0 (default for matlab) or 1

   the function returns the trace of the covariance matrix if all went well;
   returns -1.0 in case of error.

*/

double covariance(float *data_mat, double *cov_mat, unsigned char * row_mask,
                  int num_rows, int num_cols, int norm, int remove_mean, int be_quiet)
{
   double atrace, dsum, normval=0.0;
   int idel, jj, nn, mm, ifirst, ilast, ii, PC_be_quiet, kk, nsum;

   if (norm == 0) normval = (double)num_rows  - 1.0;
   else if (norm == 1) normval = (double)num_rows;
   else if (norm == -1) normval = 0.0f;
   else {
     fprintf(stderr,"*** norm value of %d is not acceptable.\n", norm); return(-1.0);
   }

   if (remove_mean == 1) {
      for( jj=0 ; jj < num_cols ; jj++ ){ /* for each column */
         nsum = 0;
         dsum = 0.0 ;
         if( row_mask == NULL ){
            for( kk=0 ; kk < num_rows ; kk++ ) dsum += data_mat[kk+jj*num_rows]   ;
            dsum /= (double)num_rows;
         } else {
            for( kk=0 ; kk < num_rows ; kk++ )
                       if( row_mask[kk] ) { dsum += data_mat[kk+jj*num_rows]   ; ++nsum; }
            dsum /= (double)nsum;
         }
         if( row_mask == NULL ){
            for( kk=0 ; kk < num_rows ; kk++ ) data_mat[kk+jj*num_rows] -= dsum  ;
         } else {
            for( kk=0 ; kk < num_rows ; kk++ )
                       if( row_mask[kk] ) { data_mat[kk+jj*num_rows] -= dsum; }
         }
      }
   }

   idel = 1 ;                           /* ii goes forward */
   for( jj=0 ; jj < num_cols ; jj++ ){

      ifirst = (idel==1) ?    0 : jj ;  /* back and forth in ii to   */
      ilast  = (idel==1) ? jj+1 : -1 ;  /* maximize use of cache/RAM */

      for( ii=ifirst ; ii != ilast ; ii += idel ){
         dsum = 0.0 ;
         if( row_mask == NULL ){
            for( kk=0 ; kk < num_rows ; kk++ ) dsum += data_mat[kk+ii*num_rows] * data_mat[kk+jj*num_rows]   ;
         } else {
            for( kk=0 ; kk < num_rows ; kk++ )
                       if( row_mask[kk] ) dsum += data_mat[kk+ii*num_rows] * data_mat[kk+jj*num_rows]   ;
         }
         if (normval > 1) cov_mat[ii+jj*num_cols] = cov_mat[jj+ii*num_cols] = dsum/normval ;
         else cov_mat[ii+jj*num_cols] = cov_mat[jj+ii*num_cols] = dsum;
      }

      if( !be_quiet ){ printf("+"); fflush(stdout); }

      idel = -idel ;                    /* reverse direction of ii */
   }
   if( !be_quiet ){ printf("\n"); fflush(stdout); }

   /*-- check diagonal for OK-ness --**/

   atrace = 0.0 ;
   ii     = 0 ;
   for( jj=0 ; jj < num_cols ; jj++ ){
      if( cov_mat[jj+jj*num_cols] <= 0.0 ){
         fprintf(stderr,"*** covariance diagonal (%d,%d) = %g\n",
                 jj+1,jj+1,cov_mat[jj+jj*num_cols]) ;
         ii++ ;
      }
      atrace += cov_mat[jj+jj*num_cols] ;
   }
   if( ii > 0 ){ fprintf(stderr, "*** Warning %d zero or negative covariance on diagonals!\n", ii); }

   if( !be_quiet ){ printf("--- covariance trace = %g\n",atrace); fflush(stdout); }

   return(atrace);
}

/*!
   Principal Component calculation by doing SVD on the
   covariance matrix of the data.

   Based on code in 3dpc

   data_mat is a matrix of M (num_cols) column vectors, each N (num_rows) elements
            long, and stored in a column major order.
   row_mask is a byte mask vector for data values to consider in data_mat
            If row_mask[i] then row i is considered in the calculations
            If NULL then all rows are considered.
   num_rows is the length N of each column in data_mat
   num_cols is the number of columns (M) (second dimension of data_mat)
   be_quiet

   (HAVE YET TO MAKE THIS FUNCTION RETURN VALUES a la pca_fast3. I'll do it when
   I'll need it ZSS)

*/
void pca (float *data_mat, unsigned char * row_mask, int num_rows, int num_cols, int be_quiet) {
   double *aa=NULL, atrace, *wout, sum, *perc;
   int i, jj, ll, ii;

   /* calculate covariance matrix */
   aa = (double *)malloc(sizeof(double)*num_cols*num_cols);
   wout = (double*)malloc(sizeof(double)*num_cols);
   atrace = covariance(data_mat, aa, row_mask, num_rows, num_cols, 0, 1, be_quiet);

   /* calculate eigenvalues and vectors to be held in aa */
   symeig_double( num_cols , aa , wout ) ;

   /* print results for now */
   sum = 0.0 ;
   perc = (double *) malloc( sizeof(double) * num_cols) ;

   fprintf(stderr, "deal: Num.  --Eigenvalue--  -Var.Fraction-  -Cumul.Fract.-\n") ;
   for( jj=0 ; jj < num_cols ; jj++ ){
      ll = num_cols - 1-jj ;      /* reversed order of eigensolution! */
      perc[jj] = wout[ll]/atrace ;
      sum     += perc[jj] ;
      fprintf(stderr, "%4d  %14.7g  %14.7g  %14.7g\n",
             jj+1 , wout[ll] , perc[jj] , sum ) ;
   }

   /* now write the vectors */
   for (ii=0; ii< num_cols; ++ii) {  /* for each row */
      for( jj=0 ; jj < num_cols ; jj++ ){ /* for each column */
         ll = num_cols - 1- jj ;      /* reversed order of eigensolution! */
         fprintf(stderr, "%3.4f  ",
              aa[ii+ll*num_cols] ) ;
      }
      fprintf(stderr, "\n"); fflush(stdout);
   }

   free(perc); free(aa); free(wout);
   return;
}

/*!
   Principal Component calculation by doing SVD on the
   covariance matrix of the data.

   Optimized for matrices with 3 columns (x y z, typically)
   Based on code in 3dpc

   data_mat is a matrix of 3 column vectors, each N (num_rows) elements
            long, and stored in a column major order.
            x0, x1, x2, x3, ... , y0, y1, ... , z0, z1, ... ,zN-1

   num_rows is the length N of each column in data_mat
   be_quiet
   pca_vec is a matrix of 3 column vectors corresponding to the
            eigen values in pca_eig. NOTE: Storage is still column
            major order, like data_mat
   pca_eig is a vector of the 3 eigen values (sorted large to small)

   Function returns the trace of the covariance matrix
*/

double pca_fast3 (float *data_mat, int num_rows, int be_quiet, double *pca_mat, double *pca_eig) {
   double aa[9], atrace, wout[9], sum, perc[9];
   int i, jj, ll, ii, cnt;

   #if DBG_PC
      fprintf(stderr, "data_mat=[ %f %f %f\n%f %f %f\n%f %f %f]\n",
                        data_mat[0], data_mat[3], data_mat[6],
                        data_mat[1], data_mat[4], data_mat[7],
                        data_mat[2], data_mat[5], data_mat[8]);
   #endif
   /* calculate covariance */
   atrace = covariance(data_mat, aa, NULL, num_rows, 3, 0, 1, be_quiet);

   /* calc eigs */
   symeig_3( aa , wout, 1 ) ;

   /* now write the vectors */
   /* print results for now */
   sum = 0.0 ;
   #if DBG_PC
      fprintf(stderr, "deal3: Num.  --Eigenvalue--  -Var.Fraction-  -Cumul.Fract.-\n") ;
   #endif
   for( jj=0 ; jj < 3 ; jj++ ){
      ll = 3 - 1-jj ;      /* reversed order of eigensolution! */
      pca_eig[jj] = wout[ll];
      #if DBG_PC
         perc[jj] = wout[ll]/atrace ;
         sum     += perc[jj] ;
         fprintf(stderr, "%4d  %14.7g  %14.7g  %14.7g\n",
                jj+1 , wout[ll] , perc[jj] , sum ) ;
      #endif
   }

   cnt = 0;
   for (ii=0; ii< 3; ++ii) {  /* for each row */
      for( jj=2 ; jj > -1 ; jj-- ){ /* for each column (reversed order) */
         ll = ii+jj*3;
         #if DBG_PC
            fprintf(stderr, "%3.4f  ",
                 aa[ll] ) ;
         #endif
         pca_mat[cnt] = aa[ll];
         ++cnt;
      }
     #if DBG_PC
      fprintf(stderr, "\n");
     #endif
   }

   return(atrace);

}
