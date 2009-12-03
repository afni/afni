#include "mrilib.h"

/*----------------------------------------------------------------------------*/

static float symeig_sim1( int n     , float *asym , float *vec ) ;
static void  symeig_2D  ( double *a , double *e   , int dovec  ) ;
static void  symeig_3D  ( double *a , double *e   , int dovec  ) ;

static float_pair symeig_sim2( int nn, float *asym, float *vec, float *wec ) ;

/*----------------------------------------------------------------------------*/

#undef  A
#define A(i,j) asym[(i)+(j)*nsym]
#undef  XPT
#define XPT(q) ( (xtyp<=0) ? xx+(q)*nn : xar[q] )

/*----------------------------------------------------------------------------*/
/*! Compute the mean vector of a set of m columns, each of length n.
   * If xtyp <=0, the columns are stored in one big array:
      ((float *)xp)[i+j*n] for i=0..n-1, j=0..m-1.
   * If xtyp > 0, the columns are stored in an array of arrays:
      ((float **)xp)[j][i]
   * The return value is the L2-norm of the vector, and the vector is
     stored into uvec.  The output vector is NOT normalized.
*//*--------------------------------------------------------------------------*/

float mean_vector( int n , int m , int xtyp , void *xp , float *uvec )
{
   int nn=n , mm=m , jj ; register int ii ;
   register float *xj , fac,sum ; float *xx=NULL , **xar=NULL ;

   if( nn < 1 || mm < 1 || xp == NULL || uvec == NULL ) return -1.0f ;

   if( xtyp <= 0 ) xx  = (float * )xp ;
   else            xar = (float **)xp ;

   for( ii=0 ; ii < nn ; ii++ ) uvec[ii] = 0.0f ;

   for( jj=0 ; jj < mm ; jj++ ){
     xj = XPT(jj) ;
     for( ii=0 ; ii < nn ; ii++ ) uvec[ii] += xj[ii] ;
   }

   fac = 1.0f / nn ; sum = 0.0f ;
   for( ii=0 ; ii < nn ; ii++ ){ uvec[ii] *= fac; sum += uvec[ii]*uvec[ii]; }
   return sqrtf(sum) ;
}

/*----------------------------------------------------------------------------*/
/*! Compute the principal singular vector of a set of m columns, each
    of length n.
   * If xtyp <=0, the columns are stored in one big array:
      ((float *)xp)[i+j*n] for i=0..n-1, j=0..m-1.
   * If xtyp > 0, the columns are stored in an array of arrays:
      ((float **)xp)[j][i]
   * The singular value is returned, and the vector is stored into uvec[].
   * tvec is a vector so that the sign of uvec dot tvec will be non-negative.
   * If the return value is not positive, something ugly happened.
*//*--------------------------------------------------------------------------*/

float principal_vector( int n , int m , int xtyp , void *xp ,
                               float *uvec , float *tvec            )
{
   int nn=n , mm=m , nsym , jj,kk,qq ;
   float *asym ;
   register float sum,qsum ; register float *xj,*xk ; register int ii ;
   float sval , *xx=NULL , **xar=NULL ;

   nsym = MIN(nn,mm) ;  /* size of the symmetric matrix to create */

   if( nsym < 1 || xp == NULL || uvec == NULL ) return (-666.0f) ;

   if( xtyp <= 0 ) xx  = (float * )xp ;
   else            xar = (float **)xp ;

   if( nsym == 1 ){  /*----- trivial case -----*/

     if( mm == 1 ){  /* really trivial: only 1 vector */
       xj = XPT(0) ; qsum = sum = 0.0f ;
       for( ii=0; ii < nn; ii++ ){
         uvec[ii] = xj[ii] ; qsum += uvec[ii]*uvec[ii] ;
         if( tvec != NULL ) sum += tvec[ii]*uvec[ii] ;
       }
       sval = sqrtf(qsum) ;
       if( sval > 0.0f ){
         qsum = 1.0f / sval ;
         if( sum < 0.0f ) qsum = -qsum ;
         for( ii=0 ; ii < nn ; ii++ ) uvec[ii] *= qsum ;
       } else {
         qsum = sqrtf(1.0f/nn) ;
         for( ii=0 ; ii < nn ; ii++ ) uvec[ii] = qsum ;
       }
     } else {  /* nn==1 and have mm > 1 such 'vectors' */
       uvec[0] = (tvec != NULL && tvec[0] < 0.0f) ? -1.0f : 1.0f ;
       for( sval=0.0f,ii=0 ; ii < mm ; ii++ ){
         xj = XPT(jj) ; sval += xj[0]*xj[0] ;
       }
       sval = sqrtf(sval) ;
     }
     return (sval) ;

   } /*----- end of trivial case -----*/

#pragma omp critical (MALLOC)
   asym = (float *)malloc(sizeof(float)*nsym*nsym) ;  /* symmetric matrix */

   /** setup matrix to eigensolve: choose smaller of [X]'[X] and [X][X]' **/
   /**     since [X] is n x m, [X]'[X] is m x m and [X][X]' is n x n     **/

   if( nn > mm ){                       /* more rows than columns:  */
                                        /* so [A] = [X]'[X] = m x m */
     for( jj=0 ; jj < mm ; jj++ ){
       xj = XPT(jj) ;
       for( kk=0 ; kk <= jj ; kk++ ){
         xk = XPT(kk) ;
         for( sum=0.0f,ii=0 ; ii < nn ; ii++ ) sum += xj[ii]*xk[ii] ;
         A(jj,kk) = sum ; if( kk < jj ) A(kk,jj) = sum ;
       }
     }

   } else {                             /* more columns than rows:  */
                                        /* so [A] = [X][X]' = n x n */
     float *xt ;
#pragma omp critical (MALLOC)
     xt = (float *)malloc(sizeof(float)*nn*mm) ;
     for( jj=0 ; jj < mm ; jj++ ){      /* form [X]' into array xt */
       if( xtyp <= 0 )
         for( ii=0 ; ii < nn ; ii++ ) xt[jj+ii*mm] = xx[ii+jj*nn] ;
       else
         for( ii=0 ; ii < nn ; ii++ ) xt[jj+ii*mm] = xar[jj][ii] ;
     }

     for( jj=0 ; jj < nn ; jj++ ){
       xj = xt + jj*mm ;
       for( kk=0 ; kk <= jj ; kk++ ){
         xk = xt + kk*mm ;
         for( sum=0.0f,ii=0 ; ii < mm ; ii++ ) sum += xj[ii]*xk[ii] ;
         A(jj,kk) = sum ; if( kk < jj ) A(kk,jj) = sum ;
       }
     }

#pragma omp critical (MALLOC)
     free(xt) ;  /* don't need this no more */
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

   if( nn <= mm ){                    /* copy eigenvector into output directly */
                                      /* (e.g., more vectors than time points) */

     (void)mean_vector( nsym , nsym , 0 , asym , uvec ) ;
     sval = symeig_sim1( nsym , asym , uvec ) ;

   } else {  /* n > m: transform eigenvector to get left singular vector */
             /* (e.g., more time points than vectors) */

     float *qvec ;

#pragma omp critical (MALLOC)
     qvec = (float *)malloc(sizeof(float)*nsym) ;
     (void)mean_vector( nsym , nsym , 0 , asym , qvec ) ;
     sval = symeig_sim1( nsym , asym , qvec ) ;
     for( qsum=0.0f,ii=0 ; ii < nn ; ii++ ){
       if( xtyp <= 0 )
         for( sum=0.0f,kk=0 ; kk < mm ; kk++ ) sum += xx[ii+kk*nn] * qvec[kk] ;
       else
         for( sum=0.0f,kk=0 ; kk < mm ; kk++ ) sum += xar[kk][ii]  * qvec[kk] ;
       uvec[ii] = sum ; qsum += sum*sum ;
     }
     if( qsum > 0.0f ){       /* L2 normalize */
       sum = 1.0f / sqrtf(qsum) ;
       for( ii=0 ; ii < nn ; ii++ ) uvec[ii] *= sum ;
     }
#pragma omp critical (MALLOC)
     free(qvec) ;
   }

   /** make it so that uvec dotted into the mean vector is positive **/

   if( tvec != NULL ){
     for( sum=0.0f,ii=0 ; ii < nn ; ii++ ) sum += tvec[ii]*uvec[ii] ;
     if( sum < 0.0f ){
       for( ii=0 ; ii < nn ; ii++ ) uvec[ii] = -uvec[ii] ;
     }
   }

   /** free at last!!! **/

#pragma omp critical (MALLOC)
   free(asym) ;

   return (sqrtf(sval)) ;
}

/*----------------------------------------------------------------------------*/
/* Carry out simultaneous iteration (generalized power iteration) on an
   nn X nn matrix stored in asym, with the initial vector stored in vec,
   and the final vector stored there.  Goal is to compute the dominant
   eigenvalue/vector -- the return value is the eigenvalue.  Three vectors
   are iterated to speed up the convergence.
*//*--------------------------------------------------------------------------*/

#undef  FEPS
#define FEPS 0.00000123456f  /* convergence test */

#undef  DEPS
#undef  DEPSQ
#define DEPS  1.e-8
#define DEPSQ 1.e-4   /* sqrt(DEPS) */

static float symeig_sim1( int nn , float *asym , float *vec )
{
   float *u1,*u2,*u3 , *v1,*v2,*v3 , *aj ;
   float sum1,sum2,sum3 , q1,q2,q3 , r1,r2,r3 , s1,s2,s3 ;
   int ii , jj , nite=0 , n=nn ;
   double bb[9] , ev[3] , evold=0.0 ;
   double g11,g12,g13,g22,g23,g33 ;
   double h11,h12,h13,h22,h23,h33 ;

   if( n < 1 || asym == NULL || vec == NULL ) return -666.0f ;

   /* special cases: 1x1 and 2x2 and 3x3 matrices */

   if( n == 1 ){
     vec[0] = 1 ; return asym[0] ;
   } else if( n == 2 ){
     for( ii=0 ; ii < 4 ; ii++ ) bb[ii] = asym[ii] ;
     symeig_2D( bb , ev , 1 ) ;
     vec[0] = bb[0] ; vec[1] = bb[1] ; return ev[0] ;
   } else if( n == 3 ){
     for( ii=0 ; ii < 9 ; ii++ ) bb[ii] = asym[ii] ;
     symeig_3D( bb , ev , 1 ) ;
     vec[0] = bb[0] ; vec[1] = bb[1] ; vec[2] = bb[2] ; return ev[0] ;
   }

   /* allocate iteration vectors */

#pragma omp critical (MALLOC)
   { u1 = (float *)malloc(sizeof(float)*n) ;
     u2 = (float *)malloc(sizeof(float)*n) ;
     u3 = (float *)malloc(sizeof(float)*n) ;
     v1 = (float *)malloc(sizeof(float)*n) ;
     v2 = (float *)malloc(sizeof(float)*n) ;
     v3 = (float *)malloc(sizeof(float)*n) ;
   }

   /* initialize u1 vector from input vec */

   for( sum1=0.0f,ii=0 ; ii < n ; ii++ ){
     u1[ii] = vec[ii] ; sum1 += u1[ii]*u1[ii] ;
   }
   if( sum1 == 0.0f ){  /* backup if input vector is all zero */
     for( ii=0 ; ii < n ; ii++ ){
       u1[ii] = drand48()-0.3 ; sum1 += u1[ii]*u1[ii] ;
     }
   }
   sum1 = 1.0f / sqrtf(sum1) ;
   for( ii=0 ; ii < n ; ii++ ) u1[ii] *= sum1 ;

   /* initialize u2, u3 by flipping signs in u1 */

   sum1 = 0.02468f / n ;
   for( ii=0 ; ii < n ; ii++ ){
     jj   = (int)lrand48() ;
     sum2 = sum1 * ((jj >> 1)%4 - 1.5f) ;
     sum3 = sum1 * ((jj >> 5)%4 - 1.5f) ;
     u2[ii] = (((jj >> 3)%2 == 0) ? u1[ii] : -u1[ii]) + sum2 ;
     u3[ii] = (((jj >> 7)%2 == 0) ? u1[ii] : -u1[ii]) + sum3 ;
   }

   /*----- iteration loop -----*/

   while(1){
     /* form V = A U */

     for( ii=0 ; ii < n ; ii++ ){ v1[ii] = v2[ii] = v3[ii] = 0.0f ; }
     for( jj=0 ; jj < n ; jj++ ){
       aj = asym + jj*n ;            /* ptr to jj-th column of asym */
       sum1 = u1[jj] ; sum2 = u2[jj] ; sum3 = u3[jj] ;
       for( ii=0 ; ii < n ; ii++ ){
         v1[ii] += aj[ii] * sum1 ;
         v2[ii] += aj[ii] * sum2 ;
         v3[ii] += aj[ii] * sum3 ;
       }
     }

     /* form G = U' U   and   H = U' V */

     g11 = g12 = g22 = g13 = g23 = g33 = 0.0 ;
     h11 = h12 = h22 = h13 = h23 = h33 = 0.0 ;
     for( ii=0 ; ii < n ; ii++ ){
       g11 += u1[ii] * u1[ii] ; g12 += u1[ii] * u2[ii] ;
       g22 += u2[ii] * u2[ii] ; g13 += u1[ii] * u3[ii] ;
       g23 += u2[ii] * u3[ii] ; g33 += u3[ii] * u3[ii] ;
       h11 += u1[ii] * v1[ii] ; h12 += u1[ii] * v2[ii] ;
       h22 += u2[ii] * v2[ii] ; h13 += u1[ii] * v3[ii] ;
       h23 += u2[ii] * v3[ii] ; h33 += u3[ii] * v3[ii] ;
     }

     /* Choleski-ize G = L L' (in place) */

     g11 = (g11 <= 0.0) ? DEPSQ : sqrt(g11) ;
     g12 = g12 / g11 ;
     g22 = g22 - g12*g12 ; g22 = (g22 <= 0.0) ? DEPSQ : sqrt(g22) ;
     g13 = g13 / g11 ;
     g23 = (g23 - g12*g13) / g22 ;
     g33 = g33 - g13*g13 * g23*g23 ; g33 = (g33 <= 0.0) ? DEPSQ : sqrt(g33) ;

     /* invert lower triangular L (in place) */

     g13 = ( g12*g23 - g22*g13 ) / (g11*g22*g33) ;
     g11 = 1.0 / g11 ; g22 = 1.0 / g22 ; g33 = 1.0 / g33 ;
     g23 = -g23 * g33 * g22 ;
     g12 = -g12 * g11 * g22 ;

     /* form B = inv[L] H inv[L]' (code from Maple 9.5) */

    { double t1, t3, t5, t13, t18, t34, t42, t47 ;
      t1  = g11 * g11 ; t3  = g11 * h11 ; t5  = g11 * h12 ;
      t13 = g12 * g12 ; t18 = g22 * g22 ; t34 = g13 * g13 ;
      t42 = g23 * g23 ; t47 = g33 * g33 ;
      bb[0] = t1 * h11 ;
      bb[4] = t13 * h11 + 2.0 * g12 * g22 * h12 + t18 * h22 ;
      bb[8] = t34 * h11 + 2.0 * g13 * g23 * h12 + 2.0 * g13 * g33 * h13
            + t42 * h22 + 2.0 * g23 * g33 * h23 + t47 * h33 ;
      bb[1] = bb[3] = t3 * g12 + t5 * g22 ;
      bb[2] = bb[6] = t3 * g13 + t5 * g23 + g11 * h13 * g33 ;
      bb[5] = bb[7] = g13 * g12 * h11 + g13 * g22 * h12 + g23 * g12 * h12
                    + g23 * g22 * h22 + g33 * g12 * h13 + g33 * g22 * h23 ;
    }

     /* eigensolve B P  = P D */

     symeig_3D( bb , ev , 1 ) ;  /* P is in bb, D is in ev */

#if 0
fprintf(stderr,"nite=%d  ev=%.9g %.9g %.9g\n",nite,ev[0],ev[1],ev[2]) ;
fprintf(stderr,"         bb=%.5g %.5g %.5g\n"
               "            %.5g %.5g %.5g\n"
               "            %.5g %.5g %.5g\n",
        bb[0],bb[1],bb[2],bb[3],bb[4],bb[5],bb[6],bb[7],bb[8]) ;
#endif

     /* form Q = inv[L]' P */

     q1 = g11*bb[0] + g12*bb[1] + g13*bb[2] ;
     q2 =             g22*bb[1] + g23*bb[2] ;
     q3 =                         g33*bb[2] ;

     /* form U = V Q and normalize it */

     for( sum1=0.0f,ii=0 ; ii < n ; ii++ ){
       u1[ii] = q1 * v1[ii] + q2 * v2[ii] + q3 * v3[ii]; sum1 += u1[ii]*u1[ii];
     }
     sum1 = 1.0f / sqrtf(sum1) ;
     for( ii=0 ; ii < n ; ii++ ) u1[ii] *= sum1 ;

     /* check first eigenvalue in D for convergence */

     if( nite > 99 ||
         ( nite > 0 && fabs(ev[0]-evold) <= FEPS*(fabs(evold)+FEPS) ) ) break ;

     /* not done yet ==> iterate */

     nite++ ; evold = ev[0] ;

     /* finish u2 and u3 vectors in U */

     r1 = g11*bb[3] + g12*bb[4] + g13*bb[5] ;
     r2 =             g22*bb[4] + g23*bb[5] ;
     r3 =                         g33*bb[5] ;

     s1 = g11*bb[6] + g12*bb[7] + g13*bb[8] ;
     s2 =             g22*bb[7] + g23*bb[8] ;
     s3 =                         g33*bb[8] ;

     for( sum2=sum3=0.0f,ii=0 ; ii < n ; ii++ ){
       u2[ii] = r1 * v1[ii] + r2 * v2[ii] + r3 * v3[ii]; sum2 += u2[ii]*u2[ii];
       u3[ii] = s1 * v1[ii] + s2 * v2[ii] + s3 * v3[ii]; sum3 += u3[ii]*u3[ii];
     }
     sum2 = 1.0f / sqrtf(sum2) ; sum3 = 1.0f / sqrtf(sum3) ;
     for( ii=0 ; ii < n ; ii++ ){ u2[ii] *= sum2 ; u3[ii] *= sum3 ; }

   } /*----- end of iteration loop -----*/

   /* result vector is u1 */

   for( ii=0 ; ii < n ; ii++ ) vec[ii] = u1[ii] ;

   /* free work vectors */

#pragma omp critical (MALLOC)
   { free(v3); free(v2); free(v1); free(u3); free(u2); free(u1); }

#if 0
   { static int first=1 ;
     if( first ){ fprintf(stderr,"nite=%d nsym=%d\n",nite,n) ; first=0 ; }
   }
#endif

   return (float)ev[0] ;
}

/*----------------------------------------------------------------------------*/
/*! Compute the first 2 principal singular vectors of a set of m columns,
    each of length n.
   * If xtyp <=0, the columns are stored in one big array:
      ((float *)xp)[i+j*n] for i=0..n-1, j=0..m-1.
   * If xtyp > 0, the columns are stored in an array of arrays:
      ((float **)xp)[j][i]
   * The singular value pair is returned, and the vectors are stored into
       uvec[] and vvec[]
   * tvec is a vector so that the sign of uvec dot tvec will be non-negative.
   * If the return values are not positive, something ugly happened.
*//*--------------------------------------------------------------------------*/

float_pair principal_vector_pair( int n , int m , int xtyp , void *xp ,
                                  float *uvec , float *vvec , float *tvec )
{
   int nn=n , mm=m , nsym , jj,kk,qq ;
   float *asym ;
   register float sum,qsum ; register float *xj,*xk ; register int ii ;
   float sval , *xx=NULL , **xar=NULL ;
   float_pair svout = {-666.0f,-666.0f} ;

   nsym = MIN(nn,mm) ;  /* size of the symmetric matrix to create */

   if( nsym < 1 || xp == NULL || uvec == NULL || vvec == NULL ) return (svout);

   if( xtyp <= 0 ) xx  = (float * )xp ;
   else            xar = (float **)xp ;

   if( nsym == 1 ){  /*----- trivial case -----*/

     for( ii=0 ; ii < nn ; ii++ ) vvec[ii] = 0.0f ; /* nothing to do */

     if( mm == 1 ){  /* really trivial: only 1 vector */
       xj = XPT(0) ; qsum = sum = 0.0f ;
       for( ii=0; ii < nn; ii++ ){
         uvec[ii] = xj[ii] ; qsum += uvec[ii]*uvec[ii] ;
         if( tvec != NULL ) sum += tvec[ii]*uvec[ii] ;
       }
       sval = sqrtf(qsum) ;
       if( sval > 0.0f ){
         qsum = 1.0f / sval ; if( sum < 0.0f ) qsum = -qsum ;
         for( ii=0 ; ii < nn ; ii++ ) uvec[ii] *= qsum ;
       } else {
         qsum = sqrtf(1.0f/nn) ;
         for( ii=0 ; ii < nn ; ii++ ) uvec[ii] = qsum ;
       }
     } else {  /* nn==1 and have mm > 1 such 'vectors' */
       uvec[0] = (tvec != NULL && tvec[0] < 0.0f) ? -1.0f : 1.0f ;
       for( sval=0.0f,ii=0 ; ii < mm ; ii++ ){
         xj = XPT(jj) ; sval += xj[0]*xj[0] ;
       }
       sval = sqrtf(sval) ;
     }

     svout.a = sval ; svout.b = 0.0f ; return (svout) ;

   } /*----- end of trivial case -----*/

#pragma omp critical (MALLOC)
   asym = (float *)malloc(sizeof(float)*nsym*nsym) ;  /* symmetric matrix */

   /** setup matrix to eigensolve: choose smaller of [X]'[X] and [X][X]' **/
   /**     since [X] is n x m, [X]'[X] is m x m and [X][X]' is n x n     **/

   if( nn > mm ){                       /* more rows than columns:  */
                                        /* so [A] = [X]'[X] = m x m */
     for( jj=0 ; jj < mm ; jj++ ){
       xj = XPT(jj) ;
       for( kk=0 ; kk <= jj ; kk++ ){
         xk = XPT(kk) ;
         for( sum=0.0f,ii=0 ; ii < nn ; ii++ ) sum += xj[ii]*xk[ii] ;
         A(jj,kk) = sum ; if( kk < jj ) A(kk,jj) = sum ;
       }
     }

   } else {                             /* more columns than rows:  */
                                        /* so [A] = [X][X]' = n x n */
     float *xt ;
#pragma omp critical (MALLOC)
     xt = (float *)malloc(sizeof(float)*nn*mm) ;
     for( jj=0 ; jj < mm ; jj++ ){      /* form [X]' into array xt */
       if( xtyp <= 0 )
         for( ii=0 ; ii < nn ; ii++ ) xt[jj+ii*mm] = xx[ii+jj*nn] ;
       else
         for( ii=0 ; ii < nn ; ii++ ) xt[jj+ii*mm] = xar[jj][ii] ;
     }

     for( jj=0 ; jj < nn ; jj++ ){
       xj = xt + jj*mm ;
       for( kk=0 ; kk <= jj ; kk++ ){
         xk = xt + kk*mm ;
         for( sum=0.0f,ii=0 ; ii < mm ; ii++ ) sum += xj[ii]*xk[ii] ;
         A(jj,kk) = sum ; if( kk < jj ) A(kk,jj) = sum ;
       }
     }

#pragma omp critical (MALLOC)
     free(xt) ;  /* don't need this no more */
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

   if( nn <= mm ){                    /* copy eigenvector into output directly */
                                      /* (e.g., more vectors than time points) */

     (void)mean_vector( nsym , nsym , 0 , asym , uvec ) ;
     svout = symeig_sim2( nsym , asym , uvec , vvec ) ;

   } else {  /* n > m: transform eigenvector to get left singular vector */
             /* (e.g., more time points than vectors) */

     float *qvec , *rvec , rsum , ssum ;

#pragma omp critical (MALLOC)
     qvec = (float *)malloc(sizeof(float)*nsym) ;
     rvec = (float *)malloc(sizeof(float)*nsym) ;
     (void)mean_vector( nsym , nsym , 0 , asym , qvec ) ;
     svout = symeig_sim2( nsym , asym , qvec , rvec ) ;
     for( rsum=qsum=0.0f,ii=0 ; ii < nn ; ii++ ){
       ssum = sum = 0.0f ;
       if( xtyp <= 0 ){
         for( kk=0 ; kk < mm ; kk++ ){
            sum += xx[ii+kk*nn] * qvec[kk] ;
           ssum += xx[ii+kk*nn] * rvec[kk] ;
         }
       } else {
         for( kk=0 ; kk < mm ; kk++ ){
            sum += xar[kk][ii]  * qvec[kk] ;
           ssum += xar[kk][ii]  * rvec[kk] ;
         }
       }
       uvec[ii] = sum ; vvec[ii] = ssum ; qsum += sum*sum ; rsum += ssum*ssum ;
     }
     if( qsum > 0.0f ){       /* L2 normalize uvec */
       sum = 1.0f / sqrtf(qsum) ;
       for( ii=0 ; ii < nn ; ii++ ) uvec[ii] *= sum ;
     }
     if( rsum > 0.0f ){       /* L2 normalize vvec */
       sum = 1.0f / sqrtf(rsum) ;
       for( ii=0 ; ii < nn ; ii++ ) vvec[ii] *= sum ;
     }
#pragma omp critical (MALLOC)
     { free(rvec) ; free(qvec) ; }
   }

   /** make it so that uvec dotted into the mean vector is positive **/

   if( tvec != NULL ){
     for( sum=0.0f,ii=0 ; ii < nn ; ii++ ) sum += tvec[ii]*uvec[ii] ;
     if( sum < 0.0f ){
       for( ii=0 ; ii < nn ; ii++ ) uvec[ii] = -uvec[ii] ;
     }
     for( sum=0.0f,ii=0 ; ii < nn ; ii++ ) sum += tvec[ii]*vvec[ii] ;
     if( sum < 0.0f ){
       for( ii=0 ; ii < nn ; ii++ ) vvec[ii] = -vvec[ii] ;
     }
   }

   /** free at last!!! **/

#pragma omp critical (MALLOC)
   free(asym) ;

   svout.a = sqrtf(svout.a) ; svout.b = sqrtf(svout.b) ; return (svout) ;
}

/*---------------------------------------------------------------------------*/
/* Same as symeig_sim1() but converge to top TWO eigenvectors. */

static float_pair symeig_sim2( int nn, float *asym, float *vec, float *wec )
{
   float *u1,*u2,*u3 , *v1,*v2,*v3 , *aj ;
   float sum1,sum2,sum3 , q1,q2,q3 , r1,r2,r3 , s1,s2,s3 ;
   int ii , jj , nite=0 , n=nn ;
   double bb[9] , ev[3] , evold1=0.0 , evold2=0.0 ;
   double g11,g12,g13,g22,g23,g33 ;
   double h11,h12,h13,h22,h23,h33 ;
   float_pair evout = {-666.0f,-666.0f} ;

   if( n < 1 || asym == NULL || vec == NULL || wec == NULL ) return (evout) ;

   /* special cases: 1x1 and 2x2 and 3x3 matrices */

   if( n == 1 ){
     vec[0] = 1.0f ; wec[0] = 0.0f ;
     evout.a = asym[0] ; evout.b = 0.0f ; return (evout) ;
   } else if( n == 2 ){
     for( ii=0 ; ii < 4 ; ii++ ) bb[ii] = asym[ii] ;
     symeig_2D( bb , ev , 1 ) ;
     vec[0] = bb[0] ; vec[1] = bb[1] ;
     wec[0] = bb[2] ; wec[1] = bb[3] ;
     evout.a = (float)ev[0] ; evout.b = (float)ev[1] ; return (evout) ;
   } else if( n == 3 ){
     for( ii=0 ; ii < 9 ; ii++ ) bb[ii] = asym[ii] ;
     symeig_3D( bb , ev , 1 ) ;
     vec[0] = bb[0] ; vec[1] = bb[1] ; vec[2] = bb[2] ;
     wec[0] = bb[3] ; wec[1] = bb[4] ; wec[2] = bb[5] ;
     evout.a = (float)ev[0] ; evout.b = (float)ev[1] ; return (evout) ;
   }

   /* allocate iteration vectors */

#pragma omp critical (MALLOC)
   { u1 = (float *)malloc(sizeof(float)*n) ;
     u2 = (float *)malloc(sizeof(float)*n) ;
     u3 = (float *)malloc(sizeof(float)*n) ;
     v1 = (float *)malloc(sizeof(float)*n) ;
     v2 = (float *)malloc(sizeof(float)*n) ;
     v3 = (float *)malloc(sizeof(float)*n) ;
   }

   /* initialize u1 vector from input vec */

   for( sum1=0.0f,ii=0 ; ii < n ; ii++ ){
     u1[ii] = vec[ii] ; sum1 += u1[ii]*u1[ii] ;
   }
   if( sum1 == 0.0f ){  /* backup if input vector is all zero */
     for( ii=0 ; ii < n ; ii++ ){
       u1[ii] = drand48()-0.3 ; sum1 += u1[ii]*u1[ii] ;
     }
   }
   sum1 = 1.0f / sqrtf(sum1) ;
   for( ii=0 ; ii < n ; ii++ ) u1[ii] *= sum1 ;

   /* initialize u2, u3 by flipping signs in u1 */

   sum1 = 0.02468f / n ;
   for( ii=0 ; ii < n ; ii++ ){
     jj   = (int)lrand48() ;
     sum2 = sum1 * ((jj >> 1)%4 - 1.5f) ;
     sum3 = sum1 * ((jj >> 5)%4 - 1.5f) ;
     u2[ii] = (((jj >> 3)%2 == 0) ? u1[ii] : -u1[ii]) + sum2 ;
     u3[ii] = (((jj >> 7)%2 == 0) ? u1[ii] : -u1[ii]) + sum3 ;
   }

   /*----- iteration loop -----*/

   while(1){
     /* form V = A U */

     for( ii=0 ; ii < n ; ii++ ){ v1[ii] = v2[ii] = v3[ii] = 0.0f ; }
     for( jj=0 ; jj < n ; jj++ ){
       aj = asym + jj*n ;            /* ptr to jj-th column of asym */
       sum1 = u1[jj] ; sum2 = u2[jj] ; sum3 = u3[jj] ;
       for( ii=0 ; ii < n ; ii++ ){
         v1[ii] += aj[ii] * sum1 ;
         v2[ii] += aj[ii] * sum2 ;
         v3[ii] += aj[ii] * sum3 ;
       }
     }

     /* form G = U' U   and   H = U' V */

     g11 = g12 = g22 = g13 = g23 = g33 = 0.0 ;
     h11 = h12 = h22 = h13 = h23 = h33 = 0.0 ;
     for( ii=0 ; ii < n ; ii++ ){
       g11 += u1[ii] * u1[ii] ; g12 += u1[ii] * u2[ii] ;
       g22 += u2[ii] * u2[ii] ; g13 += u1[ii] * u3[ii] ;
       g23 += u2[ii] * u3[ii] ; g33 += u3[ii] * u3[ii] ;
       h11 += u1[ii] * v1[ii] ; h12 += u1[ii] * v2[ii] ;
       h22 += u2[ii] * v2[ii] ; h13 += u1[ii] * v3[ii] ;
       h23 += u2[ii] * v3[ii] ; h33 += u3[ii] * v3[ii] ;
     }

     /* Choleski-ize G = L L' (in place) */

     g11 = (g11 <= 0.0) ? DEPSQ : sqrt(g11) ;
     g12 = g12 / g11 ;
     g22 = g22 - g12*g12 ; g22 = (g22 <= 0.0) ? DEPSQ : sqrt(g22) ;
     g13 = g13 / g11 ;
     g23 = (g23 - g12*g13) / g22 ;
     g33 = g33 - g13*g13 * g23*g23 ; g33 = (g33 <= 0.0) ? DEPSQ : sqrt(g33) ;

     /* invert lower triangular L (in place) */

     g13 = ( g12*g23 - g22*g13 ) / (g11*g22*g33) ;
     g11 = 1.0 / g11 ; g22 = 1.0 / g22 ; g33 = 1.0 / g33 ;
     g23 = -g23 * g33 * g22 ;
     g12 = -g12 * g11 * g22 ;

     /* form B = inv[L] H inv[L]' (code from Maple 9.5) */

    { double t1, t3, t5, t13, t18, t34, t42, t47 ;
      t1  = g11 * g11 ; t3  = g11 * h11 ; t5  = g11 * h12 ;
      t13 = g12 * g12 ; t18 = g22 * g22 ; t34 = g13 * g13 ;
      t42 = g23 * g23 ; t47 = g33 * g33 ;
      bb[0] = t1 * h11 ;
      bb[4] = t13 * h11 + 2.0 * g12 * g22 * h12 + t18 * h22 ;
      bb[8] = t34 * h11 + 2.0 * g13 * g23 * h12 + 2.0 * g13 * g33 * h13
            + t42 * h22 + 2.0 * g23 * g33 * h23 + t47 * h33 ;
      bb[1] = bb[3] = t3 * g12 + t5 * g22 ;
      bb[2] = bb[6] = t3 * g13 + t5 * g23 + g11 * h13 * g33 ;
      bb[5] = bb[7] = g13 * g12 * h11 + g13 * g22 * h12 + g23 * g12 * h12
                    + g23 * g22 * h22 + g33 * g12 * h13 + g33 * g22 * h23 ;
    }

     /* eigensolve B P  = P D */

     symeig_3D( bb , ev , 1 ) ;  /* P is in bb, D is in ev */

#if 0
fprintf(stderr,"nite=%d  ev=%.9g %.9g %.9g\n",nite,ev[0],ev[1],ev[2]) ;
fprintf(stderr,"         bb=%.5g %.5g %.5g\n"
               "            %.5g %.5g %.5g\n"
               "            %.5g %.5g %.5g\n",
        bb[0],bb[1],bb[2],bb[3],bb[4],bb[5],bb[6],bb[7],bb[8]) ;
#endif

     /* form Q = inv[L]' P */

     q1 = g11*bb[0] + g12*bb[1] + g13*bb[2] ;
     q2 =             g22*bb[1] + g23*bb[2] ;
     q3 =                         g33*bb[2] ;

     r1 = g11*bb[3] + g12*bb[4] + g13*bb[5] ;
     r2 =             g22*bb[4] + g23*bb[5] ;
     r3 =                         g33*bb[5] ;

     s1 = g11*bb[6] + g12*bb[7] + g13*bb[8] ;
     s2 =             g22*bb[7] + g23*bb[8] ;
     s3 =                         g33*bb[8] ;

     /* form U = V Q and normalize it */

     for( sum1=sum2=sum3=0.0f,ii=0 ; ii < n ; ii++ ){
       u1[ii] = q1 * v1[ii] + q2 * v2[ii] + q3 * v3[ii]; sum1 += u1[ii]*u1[ii];
       u2[ii] = r1 * v1[ii] + r2 * v2[ii] + r3 * v3[ii]; sum2 += u2[ii]*u2[ii];
       u3[ii] = s1 * v1[ii] + s2 * v2[ii] + s3 * v3[ii]; sum3 += u3[ii]*u3[ii];
     }
     sum1 = 1.0f/sqrtf(sum1); sum2 = 1.0f/sqrtf(sum2); sum3 = 1.0f/sqrtf(sum3);
     for( ii=0 ; ii < n ; ii++ ){ u1[ii] *= sum1; u2[ii] *= sum2; u3[ii] *= sum3; }

     /* check first two eigenvalues in D for convergence */

     if( nite > 99 ||
         ( nite > 0                                       &&
           fabs(ev[0]-evold1) <= FEPS*(fabs(evold1)+FEPS) &&
           fabs(ev[1]-evold2) <= FEPS*(fabs(evold2)+FEPS)   ) ) break ;

     /* not done yet ==> iterate */

     nite++ ; evold1 = ev[0] ; evold2 = ev[1] ;

   } /*----- end of iteration loop -----*/

   /* result vectors are u1, u2 */

   for( ii=0 ; ii < n ; ii++ ){ vec[ii] = u1[ii] ; wec[ii] = u2[ii] ; }

   /* free work vectors */

#pragma omp critical (MALLOC)
   { free(v3); free(v2); free(v1); free(u3); free(u2); free(u1); }

   evout.a = (float)ev[0] ; evout.b = (float)ev[1] ; return (evout) ;
}

/*---------------------------------------------------------------------------*/

#undef  SQR
#define SQR(a)  ((a)*(a))

#undef  DET3
#define DET3(m) ( m[0]*m[4]*m[8]-m[0]*m[7]*m[5]-m[1]*m[3]*m[8] \
                 +m[1]*m[6]*m[5]+m[2]*m[3]*m[7]-m[2]*m[6]*m[4] )

#undef  PI
#define PI 3.14159265358979323846

#undef  SWAP
#define SWAP(x,y) (th=(x),(x)=(y),(y)=th)

#undef  CSWAP       /* swap columns in a[] starting at indexes i and j */
#define CSWAP(i,j) (SWAP(a[i],a[j]),SWAP(a[i+1],a[j+1]),SWAP(a[i+2],a[j+2]))

/*----------------------------------------------------------------------------*/
/*! Do a 3x3 symmetric eigen-problem as fast as possible, using cubic formula.
     - INPUT: double a[9] = input matrix; a[i+3*j] = matrix (i,j) element
              (actually, only the 0,1,2,4,5,8 elements of a[] are used).
     - OUTPUT: e[i] = i'th eigenvalue, with e[0] >= e[1] >= e[2].
     - OUTPUT: if(dovec) then a[] is replaced with eigenvectors,
               and this orthogonal matrix will have determinant=1.
               Vector #0 is in a[0..2] , #1 in a[3..5], #3 in a[6..8].
*//*--------------------------------------------------------------------------*/

static void symeig_3D( double *a , double *e , int dovec )
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
       ( DEPS*aba > (abb+abc) && DEPS*abd > (abb+abe) && DEPS*abf > (abc+abe) ) ){

     lam1 = aa ; lam2 = dd ; lam3 = ff ;

     if( dovec ){
       a[0] = a[4] = a[8] = 1.0 ;
       a[1] = a[2] = a[3] = a[5] = a[6] = a[7] = 0.0 ;

       if( lam1 < lam2 ){ SWAP(lam1,lam2) ; CSWAP(0,3) ; }
       if( lam1 < lam3 ){ SWAP(lam1,lam3) ; CSWAP(0,6) ; }
       if( lam2 < lam3 ){ SWAP(lam2,lam3) ; CSWAP(3,6) ; }
       if( DET3(a) < 0.0 ){ a[6] = -a[6]; a[7] = -a[7]; a[8] = -a[8]; }
     } else {
       if( lam1 < lam2 )  SWAP(lam1,lam2) ;
       if( lam1 < lam3 )  SWAP(lam1,lam3) ;
       if( lam2 < lam3 )  SWAP(lam2,lam3) ;
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
#ifndef USE_OMP
     {
     static int nerr=0 ;
     if( ++nerr < 4 )
       fprintf(stderr,"** ERROR in symeig_3D: discrim=%g numer=%g\n",qq,rr) ;
     }
#endif
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
     if( lam1 < lam2 ) SWAP(lam1,lam2) ;
     if( lam1 < lam3 ) SWAP(lam1,lam3) ;
     if( lam2 < lam3 ) SWAP(lam2,lam3) ;
     e[0] = ann*lam1 ; e[1] = ann*lam2 ; e[2] = ann*lam3 ;
     return ;
   }

   /*-- are doing eigenvectors; must do double root as a special case --*/

#undef  CROSS  /* cross product (x1,x2,x3) X (y1,y2,y3) -> (z1,z2,z3) */
#define CROSS(x1,x2,x3,y1,y2,y3,z1,z2,z3) \
 ( (z1)=(x2)*(y3)-(x3)*(y2), (z2)=(x3)*(y1)-(x1)*(y3), (z3)=(x1)*(y2)-(x2)*(y1) )

   d12 = fabs(lam1-lam2) ; d13 = fabs(lam1-lam3) ; d23 = fabs(lam2-lam3) ;
   rr  = MIN(d12,d13)    ; rr  = MIN(rr,d23)     ;

   if( rr > DEPS*ann ){  /*---- not a double root ----*/

     if( lam1 < lam2 )  SWAP(lam1,lam2) ;  /* start by sorting eigenvalues */
     if( lam1 < lam3 )  SWAP(lam1,lam3) ;
     if( lam2 < lam3 )  SWAP(lam2,lam3) ;
     e[0] = ann*lam1 ; e[1] = ann*lam2 ; e[2] = ann*lam3 ;

     /* find eigenvector for lam1 by computing Ay-lam1*y for
        vectors y1=[1,0,0], y2=[0,1,0], and y3=[0,0,1]; the eigenvector
        is orthogonal to all of these, so use the cross product to get it */

     u1 = aa-lam1 ; u2 = bb      ; u3 = cc ;   /* A*y1 - lam1*y1 */
     v1 = bb      ; v2 = dd-lam1 ; v3 = ee ;   /* A*y2 - lam1*y2 */
     CROSS(u1,u2,u3 , v1,v2,v3 , t1,t2,t3 ) ;     tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < DEPSQ*ann ){                     /* u and v were parallel? */
       w1 = cc ; w2 = ee ; w3 = ff-lam1 ;      /* A*y3 - lam1*y3 */
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < DEPSQ*ann ){                   /* u and w were parallel? */
         CROSS(v1,v2,v3 , w1,w2,w3 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     a[0] = t1/tn ; a[1] = t2/tn ; a[2] = t3/tn ;  /* normalize */

     /* do same for lam2 */

     u1 = aa-lam2 ; u2 = bb      ; u3 = cc ;
     v1 = bb      ; v2 = dd-lam2 ; v3 = ee ;
     CROSS(u1,u2,u3 , v1,v2,v3 , t1,t2,t3 ) ;     tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < DEPSQ*ann ){
       w1 = cc ; w2 = ee ; w3 = ff-lam2 ;
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < DEPSQ*ann ){
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
     if( tn < DEPSQ*ann ){
       w1 = cc ; w2 = ee ; w3 = ff-lam3 ;
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < DEPSQ*ann ){
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
     if( tn < DEPSQ*ann ){
       w1 = cc ; w2 = ee ; w3 = ff-lam3 ;
       CROSS(u1,u2,u3 , w1,w2,w3 , t1,t2,t3 ) ;   tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < DEPSQ*ann ){
         CROSS(v1,v2,v3 , w1,w2,w3 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     w1 = a[6] = t1/tn ; w2 = a[7] = t2/tn ; w3 = a[8] = t3/tn ;

     /* find a vector orthogonal to it */

     CROSS(w1,w2,w3 , 1,0,0 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
     if( tn < DEPSQ ){
       CROSS(w1,w2,w3 , 0,1,0 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       if( tn < DEPSQ ){
         CROSS(w1,w2,w3 , 0,0,1 , t1,t2,t3 ) ; tn = sqrt(t1*t1+t2*t2+t3*t3) ;
       }
     }
     a[0] = t1/tn ; a[1] = t2/tn ; a[2] = t3/tn ;

     /* and the final vector is the cross product of these two */

     CROSS( w1,w2,w3 , a[0],a[1],a[2] , a[3],a[4],a[5] ) ;

     /* sort results (we know lam1==lam2) */

     if( lam1 < lam3 ){
       SWAP(lam1,lam3) ; CSWAP(0,6) ;
       if( DET3(a) < 0.0 ){ a[6] = -a[6]; a[7] = -a[7]; a[8] = -a[8]; }
     }

     e[0] = ann*lam1 ; e[1] = ann*lam2 ; e[2] = ann*lam3 ;
     return ;
   }
}

/*---------------------------------------------------------------------------*/
/*! 2x2 symmetric eigenvalue/vector problem, like symeig_3D() above. */

static void symeig_2D( double *a , double *e , int dovec )
{
   double sxx,sxy,syy , lam1,lam2 , ss,tt , x,y ;

   if( a == NULL || e == NULL ) return ;

   /*----- unload matrix into local variables -----*/

   sxx = a[0] ; sxy = a[1] ; syy = a[3] ;

   ss = fabs(sxx) ; tt = fabs(syy) ; if( ss > tt ) ss = tt ;

   if( fabs(sxy) < DEPS*ss ){   /*--- essentially a diagonal matrix ---*/
     if( sxx >= syy ){
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
   lam1 = 0.5 * ( sxx + syy + ss ) ;                  /* larger */
   lam2 = 0.5 * ( sxx + syy - ss ) ;                  /* smaller */

   if( dovec ){
     x = 2.0*sxy ; y = syy - sxx + ss ; tt = sqrt(x*x+y*y) ;
     a[0] = x/tt ; a[1] = y/tt ;

     y = syy - sxx - ss ; tt = sqrt(x*x+y*y) ;
     a[2] = x/tt ; a[3] = y/tt ;
   }
   e[0] = lam1 ; e[1] = lam2 ;
   return ;
}
