#include "mrilib.h"

static complex * csplus = NULL , * csminus = NULL ;  /* trig consts */
static int nold = -666 ;

#undef PI
#define PI (3.141592653589793238462643)

void csfft_trigconsts( int idim )
{
   register unsigned int  m, n, i0, i1, i2, i3, k;
   register complex       *r0, *csp;
   register float         co, si, f0, f1, f2, f3, f4;
   double                 al;

   if( idim == nold ) return ;

   if( idim > nold ){
      if( csplus != NULL ){ free(csplus) ; free(csminus) ; }
      csplus  = (complex *) malloc( sizeof(complex) * idim ) ;
      csminus = (complex *) malloc( sizeof(complex) * idim ) ;
      if( csplus == NULL || csminus == NULL ){
         fprintf(stderr,"\n*** fft cannot malloc space! ***\n"); exit(-1) ;
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
   return ;
}

/*--------------------------------------------------------------------
   Complex-to-complex FFT in place:
     mode = -1 or +1 (NO SCALING ON INVERSE!)
     idim = dimension (power of 2)
     xc   = input/output array
   Automagically re-initializes itself when idim changes from
   previous call.  By AJJesmanowicz, modified by RWCox.
----------------------------------------------------------------------*/

void csfft_cox( int mode , int idim , complex * xc )
{
   register unsigned int  m, n, i0, i1, i2, i3, k;
   register complex       *r0, *r1, *csp;
   register float         co, si, f0, f1, f2, f3, f4;
   double                 al;

   /** perhaps initialize **/

   if( nold != idim ) csfft_trigconsts( idim ) ;

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

/*--------------------------------------------------------------------
   Many complex-to-complex FFTs in place:
     mode = -1 or +1 (NO SCALING ON INVERSE!)
     idim = dimension (power of 2)
     nvec = number of vectors to do (vectors are contiguous)
     xc   = input/output array
   Automagically re-initializes itself when idim changes from
   previous call.  By AJJesmanowicz, modified by RWCox.
----------------------------------------------------------------------*/

void csfft_many( int mode , int idim , int nvec , complex * xc )
{
   register unsigned int  m, n, i0, i1, i2, i3, k , iv ;
   register complex       *r0, *r1, *csp , *xcx;
   register float         co, si, f0, f1, f2, f3, f4;
   double                 al;

   if( nvec == 1 ){
      csfft_cox( mode , idim , xc ) ;
      return ;
   }

   /** perhaps initialize **/

   if( nold != idim ) csfft_trigconsts( idim ) ;

   n   = idim;
   i2  = idim >> 1;
   i1  = 0;
   csp = (mode > 0) ? csplus : csminus ;  /* choose const array */

   for (i0=0; i0 < n; i0 ++) {
      if ( i1 > i0 ) {
         for( iv=0,xcx=xc ; iv < nvec ; iv++,xcx+=n ){
            r0    = xcx + i0;
            r1    = xcx + i1;
            f1    = r0->r;
            f2    = r0->i;
            r0->r = r1->r;
            r0->i = r1->i;
            r1->r = f1;
            r1->i = f2;
         }
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
            for( iv=0,r0=xc+i1 ; iv < nvec ; iv++,r0+=n ){
               r1    = r0 + m;
#if 1
               f1    = r1->r * co - r1->i * si ;
               f3    = r1->r * si + r1->i * co ;
#else
               f1    = r1->r * co ;
               f2    = r1->i * si ;
               f3    = r1->r * si ;
               f4    = r1->i * co ;
               f1   -= f2 ;
               f3   += f4 ;
#endif
               f2    = r0->r ;
               f4    = r0->i ;
               r1->r = f2 - f1 ;
               r1->i = f4 - f3 ;
               r0->r = f2 + f1 ;
               r0->i = f4 + f3 ;
            }
         }
         k++;
      }
      m = i3;
   }
   return ;
}
