#include "mcwfft.h"

/*--------------------------------------------------------------------
   Complex-to-complex 2D FFT in place:
     modex = -1 or +1
     modey = -1 or +1
     idx   = dimension in x (power of 2)
     idy   = dimension in y (power of 2)
     zc    = input/output array
----------------------------------------------------------------------*/

void FFT_2dcx( int modex , int modey , int idx , int idy , complex * zc )
{
   int ii , jj ;
   static complex * tc = NULL ;
   static int ntc_old = -1 ;

   /*** make workspace, if needed ***/

   if( idy > ntc_old ){
      if( tc != NULL ) free(tc) ;
      tc = (complex *) malloc( sizeof(complex) * idy ) ;
      if( tc == NULL ){
         fprintf(stderr,"\n***FFT_2dcx cannot malloc workspace!\n") ;
         exit(-1) ;
      }
      ntc_old = idy ;
   }

   /*** x FFTs ***/

   for( jj=0 ; jj < idy ; jj++ )
      FFT_1dcx( modex , idx , zc + (jj*idx) ) ;

   /*** y FFTs ***/

   for( ii=0 ; ii < idx ; ii++ ){
      for( jj=0 ; jj < idy ; jj++ ) tc[jj] = zc[ii+jj*idx] ;
      FFT_1dcx( modey , idy , tc ) ;
      for( jj=0 ; jj < idy ; jj++ ) zc[ii+jj*idx] = tc[jj] ;
   }

   return ;
}

/*--------------------------------------------------------------------
   Complex-to-complex 1D FFT in place:
     mode = -1 or +1 [phase factor in exponent: NO SCALING ON INVERSE!]
     idim = dimension (power of 2)
     xc   = input/output array
   Automagically re-initializes itself when idim changes from
   previous call.  By AJJesmanowicz, modified by RWCox.
----------------------------------------------------------------------*/

void FFT_1dcx( int mode , int idim , complex * xc )
{
   register unsigned int  m, n, i0, i1, i2, i3, k;
   register complex       *r0, *r1, *csp;
   register float         co, si, f0, f1, f2, f3, f4;
   double                 al;

   static complex * csplus = NULL , * csminus = NULL ;  /* trig consts */
   static int nold = -666 ;

   /** perhaps initialize **/

   if( nold != idim ){

      if( idim > nold ){
         if( csplus != NULL ){ free(csplus) ; free(csminus) ; }
         csplus  = (complex *) malloc( sizeof(complex) * idim ) ;
         csminus = (complex *) malloc( sizeof(complex) * idim ) ;
         if( csplus == NULL || csminus == NULL ){
            fprintf(stderr,"\n*** FFT_1dcx cannot malloc workspace! ***\n");
            exit(-1) ;
         }
      }
      nold = n = idim ;

      f1 = 1.0 ;  /* csplus init */
      m  = 1;
      k  = 0;
      while (n > m) {
         i3 = m << 1;
         f2 = m;
         al = f1*PI/f2;
         co = cos(al);
         si = sin(al);
         (csplus + k)->r = 1.;
         (csplus + k)->i = 0.;
         for (i0=0; i0 < m; i0++) {
            k++;
            csp = csplus + k;
            r0 = csp - 1;
            csp->r = r0->r * co - r0->i * si;
            csp->i = r0->i * co + r0->r * si;
         }
         m = i3;
      }

      f1 = -1.0 ;  /* csminus init */
      m  = 1;
      k  = 0;
      while (n > m) {
         i3 = m << 1;
         f2 = m;
         al = f1*PI/f2;
         co = cos(al);
         si = sin(al);
         (csminus + k)->r = 1.;
         (csminus + k)->i = 0.;
         for (i0=0; i0 < m; i0++) {
            k++;
            csp = csminus + k;
            r0  = csp - 1;
            csp->r = r0->r * co - r0->i * si;
            csp->i = r0->i * co + r0->r * si;
         }
         m = i3;
      }
   }

   /** Main loop starts here **/

   n   = idim;
   i2  = idim >> 1;
   i1  = 0;
   csp = (mode > 0) ? csplus : csminus ;  /* choose const array */

   for (i0=0; i0 < n; i0 ++) {
      if ( i1 > i0 ) {
         r0    = xc + i0;
         r1    = xc + i1;
         f1    = r0->r;
         f2    = r0->i;
         r0->r = r1->r;
         r0->i = r1->i;
         r1->r = f1;
         r1->i = f2;
      }
      m = i2;
      while ( m && !(i1 < m) ) {
         i1 -= m;
         m >>= 1;
     }
     i1 += m;
   }

   m = 1;
   k = 0;
   while (n > m) {
      i3 = m << 1;
      for (i0=0; i0 < m; i0 ++) {
         co = (csp + k)->r;
         si = (csp + k)->i;
         for (i1=i0; i1 < n; i1 += i3) {
            r0    = xc + i1;
            r1    = r0 + m;
            f1    = r1->r * co;
            f2    = r1->i * si;
            f3    = r1->r * si;
            f4    = r1->i * co;
            f1   -= f2;
            f3   += f4;
            f2    = r0->r;
            f4    = r0->i;
            r1->r = f2 - f1;
            r1->i = f4 - f3;
            r0->r = f2 + f1;
            r0->i = f4 + f3;
         }
         k++;
      }
      m = i3;
   }

#ifdef SCALE_INVERSE
   if (mode > 0) {
      f0 = 1.0 / idim ;
      i0 = 0;
      i1 = 1;
      while (i0 < n) {
         r0 = xc + i0;
         r1 = xc + i1;
         f1 = r0->r;
         f2 = r0->i;
         f3 = r1->r;
         f4 = r1->i;
         f1 *= f0;
         f2 *= f0;
         f3 *= f0;
         f4 *= f0;
         r0->r = f1;
         r0->i = f2;
         r1->r = f3;
         r1->i = f4;
         i0 += 2;
         i1 += 2;
      }
   }
#endif

   return ;
}

/*----------------------------------------------------------------------
    SUM  SUM  x   exp( -2*Pi*I * (lp+mq)*alpha - 2*Pi*I*(mp-lq)*beta )
     l=0..N-1  lm
     m=0..N-1

  where N = idim and x = xc ; result overwrites array xc.
  alpha and beta are arbitrary floats;  N is arbitrary integer > 3.
------------------------------------------------------------------------*/

void FFT_2dchirpz( float alpha , float beta , int idim , complex * xc )
{
   int nn , ll , ll2 , pp,qq , ll2q , use_old , qloff , qnoff ;

   static complex * yc = NULL , * zc = NULL , * zhatc = NULL ;
   static int nn_old = -666 , ll_old = -666 ;
   static float alpha_old = -987.654 , beta_old = 314.1592 ;

   /*** if needed, create new workspace arrays ***/

   nn = idim ;
   if( nn == nn_old ){  /* same dimensions as before */
      ll   = ll_old ;
      ll2  = 2*ll ;
      ll2q = ll2 * ll2 ;

      use_old = ( alpha == alpha_old && beta == beta_old ) ;

   } else {             /* new dimensions */

      nn_old = nn ;
      for( ll=4 ; ll < nn ; ll *= 2 ) ;  /* until power of 2 >= nn */
      ll2  = 2*ll ;
      ll2q = ll2 * ll2 ;

      use_old = 0 ;

      if( ll != ll_old ){
         ll_old = ll ;
         if( yc != NULL ){ free(yc) ; free(zc) ; free(zhatc) ; }

         yc    = (complex *) malloc( sizeof(complex) * ll2q ) ;
         zc    = (complex *) malloc( sizeof(complex) * ll2q ) ;
         zhatc = (complex *) malloc( sizeof(complex) * ll2q ) ;

         if( yc == NULL || zc == NULL || zhatc == NULL ){
            fprintf(stderr,"\n*** FFT_2dchirpz cannot malloc workspace!\n") ;
            exit(-1) ;
         }
      }
   }

   /*** if needed, fill "z" workspace arrays ***/

   if( ! use_old ){
      float aq , bq , t , api ;

      for( pp=0 ; pp < ll2q ; pp++ ) zc[pp].r = zc[pp].i = 0.0 ;

      api = alpha * PI ;

      for( qq=-(nn-1) ; qq <= (nn-1) ; qq++ ){

         qloff = (qq < 0) ? ((qq+ll2)*ll2) : (qq*ll2) ;
         aq    = alpha * PI * qq * qq ;
         bq    = 2.0 * beta * PI * qq ;

         for( pp=0 ; pp < 2*nn ; pp++ ){
            t = aq - (api * pp + bq ) * pp ;
            zc[pp+qloff].r = cos(t) ;
            zc[pp+qloff].i = sin(t) ;
         }
      }

      t = 1.0 / ll2q ;
      for( pp=0 ; pp < ll2q ; pp++ ){
         zhatc[pp].r = t * zc[pp].r ;
         zhatc[pp].i = t * zc[pp].i ;
      }
      FFT_2dcx( 1 , 1 , ll2 , ll2 , zhatc ) ;
   }

   /*** fill "yc" workspace ***/

   for( pp=0 ; pp < ll2q ; pp++ ) yc[pp].r = yc[pp].i = 0.0 ;

   for( qq=0 ; qq < nn ; qq++ ){
      qloff = qq*ll2 ;
      qnoff = qq*nn ;
      for( pp=0 ; pp < nn ; pp++ )
         yc[pp] = CJMULT( xc[pp+qnoff] , zc[pp+qloff] ) ;
   }

   /*** 2D FFT, multiply, inverse 2D FFT ***/

   FFT_2dcx( -1 , -1 , ll2 , ll2 , yc ) ;
   for( pp=0 ; pp < ll2q ; pp++ ) yc[pp] = CMULT( yc[pp] , zhatc[pp] ) ;
   FFT_2dcx( -1 , 1 , ll2 , ll2 , yc ) ;

   /*** produce output ***/

   for( qq=0 ; qq < nn ; qq++ ){
      qloff = qq*ll2 ;
      qnoff = qq*nn ;
      for( pp=0 ; pp < nn ; pp++ )
         xc[pp+qnoff] = CMULT( zc[pp+qloff] , yc[pp+qloff] ) ;
   }

   return ;
}
