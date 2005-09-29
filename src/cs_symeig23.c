#include "mrilib.h"

#undef  SWAP
#define SWAP(x,y) (th=(x),(x)=(y),(y)=th)

#undef  CSWAP
#define CSWAP(i,j) (SWAP(a[i],a[j]),SWAP(a[i+1],a[j+1]),SWAP(a[i+2],a[j+2]))

#undef  EPS
#undef  EPSQ
#define EPS  1.e-6
#define EPSQ 1.e-3   /* sqrt(EPS) */

/*---------------------------------------------------------------------------*/
/*! Do a 3x3 symmetric eigen-problem.
     - INPUT: double a[9] = input matrix; a[i+3*j] = A(i,j) element
     - OUTPUT: e[i] = i'th eigenvalue, with e[0] >= e[1] >= e[2]
     - OUTPUT: if(dovec) then a[] is replaced with eigenvectors
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

   if( ann == 0.0 ){
     e[0] = e[1] = e[2] = 0.0 ; return ;  /* matrix is all zero! */
   }

   /*----- check for matrix that is essentially diagonal -----*/

   if( EPS*aba > (abb+abc) && EPS*abd > (abb+abe) && EPS*abf > (abc+abe) ){

     lam1 = aa ; lam2 = dd ; lam3 = ff ;

     if( dovec ){
       a[0] = a[4] = a[8] = 1.0 ;
       a[1] = a[2] = a[3] = a[5] = a[6] = a[7] = 0.0 ;

       if( lam1 < lam2 ){ SWAP(lam1,lam2) ; CSWAP(0,3) ; }
       if( lam1 < lam3 ){ SWAP(lam1,lam3) ; CSWAP(0,6) ; }
       if( lam2 < lam3 ){ SWAP(lam2,lam3) ; CSWAP(3,6) ; }
     } else {
       if( lam1 < lam2 )  SWAP(lam1,lam2) ;
       if( lam1 < lam3 )  SWAP(lam1,lam3) ;
       if( lam2 < lam3 )  SWAP(lam2,lam3) ;
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
     if( lam1 < lam2 )  SWAP(lam1,lam2) ;
     if( lam1 < lam3 )  SWAP(lam1,lam3) ;
     if( lam2 < lam3 )  SWAP(lam2,lam3) ;
     e[0] = lam1 ; e[1] = lam2 ; e[2] = lam3 ;
     return ;
   }

   /*-- are doing eigenvectors; must do double root as a special case --*/

#undef  CROSS
#define CROSS(x1,x2,x3,y1,y2,y3,z1,z2,z3) \
 ( (z1)=(x2)*(y3)-(x3)*(y2), (z2)=(x3)*(y1)-(x1)*(y3), (z3)=(x1)*(y2)-(x2)*(y1) )

   d12 = fabs(lam1-lam2) ; d13 = fabs(lam1-lam3) ; d23 = fabs(lam2-lam3) ;
   rr  = MAX(d12,d13)    ; rr  = MAX(rr,d23)     ;

   if( rr > EPS*ann ){  /*---- not a double root ----*/

     if( lam1 < lam2 )  SWAP(lam1,lam2) ;  /* start by sorting eigenvalues */
     if( lam1 < lam3 )  SWAP(lam1,lam3) ;
     if( lam2 < lam3 )  SWAP(lam2,lam3) ;
     e[0] = lam1 ; e[1] = lam2 ; e[2] = lam3 ;

     /* find eigenvector for lam1 by computing Ay-lam1*y for
        vectors y=[1,0,0], y=[0,1,0], and [0,0,1]; the eigenvector
        is orthogonal to all of these, so use the cross product to get it */

     u1 = aa-lam1 ; u2 = bb      ; u3 = cc ;
     v1 = bb      ; v2 = dd-lam1 ; v3 = ee ;
     CROSS(u1,u2,u3 , v1,v2,v3 , t1,t2,t3 ) ;     tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < EPSQ*ann ){
       w1 = cc ; w2 = ee ; w3 = ff-lam1 ;
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < EPSQ*ann ){
         CROSS(v1,v2,v3 , w1,w2,w3 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     a[0] = t1/tn ; a[1] = t2/tn ; a[2] = t3/tn ;

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

fprintf(stderr,"Double root case: lam1=lam2=%g  lam3=%g\n",lam1,lam3) ;

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

     if( lam2 < lam3 ){ SWAP(lam2,lam3) ; CSWAP(3,6) ; }

     e[0] = lam1 ; e[1] = lam2 ; e[2] = lam3 ;
     return ;
   }
}

int main( int argc , char **argv )
{
   double a[9] , e[3] ; int ii ;

   memset( a , 0 , sizeof(double)*9 ) ;
   for( ii=1 ; ii < argc && ii < 10 ; ii++ ) a[ii-1] = strtod(argv[ii],NULL) ;

   symeig_3( a , e , 1 ) ;

   fprintf(stderr,"#1: %10.5f  [ %10.5f %10.5f %10.5f ]\n",
           e[0], a[0],a[1],a[2] ) ;

   fprintf(stderr,"#2: %10.5f  [ %10.5f %10.5f %10.5f ]\n",
           e[1], a[3],a[4],a[5] ) ;

   fprintf(stderr,"#3: %10.5f  [ %10.5f %10.5f %10.5f ]\n",
           e[2], a[6],a[7],a[8] ) ;

   exit(0) ;
}
