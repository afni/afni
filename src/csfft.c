#undef STANDALONE

#ifdef STANDALONE
#  include <stdlib.h>   /* for use by itself (not in the AFNI package) */
#  include <stddef.h>
#  include <stdio.h>
#  include <math.h>
   typedef struct complex { float r , i ; } complex ;
#else
#  include "mrilib.h"   /* for use in AFNI package */
#endif

/*** Prototype:
        Complex-to-complex FFT in place:
          mode = -1 or +1 (NO SCALING ON INVERSE!)
          idim = dimension (power of 2)
          xc   = input/output array
        Re-initializes itself only when idim changes from previous call. ***/

void csfft_cox( int mode , int idim , complex * xc ) ;

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#undef PI
#define PI (3.141592653589793238462643)

/*---------- For the unrolled FFT routines: November 1998 ----------*/

#ifndef DONT_UNROLL_FFTS
   static void fft8  ( int mode , complex * xc ) ;
   static void fft16 ( int mode , complex * xc ) ;
   static void fft32 ( int mode , complex * xc ) ;  /* prototypes   */
   static void fft64 ( int mode , complex * xc ) ;  /* for internal */
   static void fft128( int mode , complex * xc ) ;  /* functions    */
   static void fft256( int mode , complex * xc ) ;
#endif

/*----------------------------------------------------------------------
   Speedups with unrolled FFTs [program fftest.c]:
   Pentium II 400 MHz:  58% (32) to 36% (256)  [gcc -O3 -ffast-math]
   SGI R10000 175 MHz:  47% (32) to 18% (256)  [cc -Ofast]
   HP PA-8000 200 MHz:  58% (32) to 40% (256)  [cc +O3 +Oaggressive]
------------------------------------------------------------------------*/

/*----------------- the csfft trig constants tables ------------------*/

static complex * csplus = NULL , * csminus = NULL ;
static int nold = -666 ;

/*--------------------------------------------------------------------
   Initialize csfft trig constants table.  Adapted from AJ's code.
   Does both mode=1 and mode=-1 tables.
----------------------------------------------------------------------*/

static void csfft_trigconsts( int idim )  /* internal function */
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
         fprintf(stderr,"\n*** csfft cannot malloc space! ***\n"); exit(1) ;
      }
   }
   nold = n = idim ;

   f1 = 1.0 ;  /* csplus init */
   m  = 1; k  = 0;
   while (n > m) {
      i3 = m << 1; f2 = m; al = f1*PI/f2;
      co = cos(al); si = sin(al);
      (csplus + k)->r = 1.; (csplus + k)->i = 0.;
      for (i0=0; i0 < m; i0++) {
         k++;
         csp = csplus + k; r0 = csp - 1;
         csp->r = r0->r * co - r0->i * si;
         csp->i = r0->i * co + r0->r * si;
      }
      m = i3;
   }

   f1 = -1.0 ;  /* csminus init */
   m  = 1; k  = 0;
   while (n > m) {
      i3 = m << 1; f2 = m; al = f1*PI/f2;
      co = cos(al); si = sin(al);
      (csminus + k)->r = 1.; (csminus + k)->i = 0.;
      for (i0=0; i0 < m; i0++) {
         k++;
         csp = csminus + k; r0  = csp - 1;
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
   previous call.  By AJ (Andrzej Jesmanowicz), modified by RWCox.
----------------------------------------------------------------------*/

void csfft_cox( int mode , int idim , complex * xc )
{
   register unsigned int  m, n, i0, i1, i2, i3, k;
   register complex       *r0, *r1, *csp;
   register float         co, si, f0, f1, f2, f3, f4;

   /*-- November 1998: maybe use the unrolled FFT routines --*/

#ifndef DONT_UNROLL_FFTS
   switch( idim ){
      case   8: fft8  (mode,xc) ; return ;
      case  16: fft16 (mode,xc) ; return ;
      case  32: fft32 (mode,xc) ; return ;
      case  64: fft64 (mode,xc) ; return ;
      case 128: fft128(mode,xc) ; return ;
      case 256: fft256(mode,xc) ; return ;
   }
#endif  /* end of unrollificationizing */

   /**-- perhaps initialize --**/

   if( nold != idim ) csfft_trigconsts( idim ) ;

   /** Main loop starts here **/

   n   = idim;
   i2  = idim >> 1;
   i1  = 0;
   csp = (mode > 0) ? csplus : csminus ;  /* choose const array */

   /*-- swap data --*/

   for (i0=0; i0 < n; i0 ++) {
      if ( i1 > i0 ) {
         r0    = xc + i0; r1    = xc + i1;
         f1    = r0->r;   f2    = r0->i;
         r0->r = r1->r;   r0->i = r1->i;
         r1->r = f1;      r1->i = f2;
      }
      m = i2;
      while ( m && !(i1 < m) ) { i1 -= m; m >>= 1; }
     i1 += m;
   }

   /*-- compute compute compute --*/

   m = 1; k = 0;
   while (n > m) {
      i3 = m << 1;
      for (i0=0; i0 < m; i0 ++) {
         co = (csp + k)->r; si = (csp + k)->i;
         for (i1=i0; i1 < n; i1 += i3) {
            r0    = xc + i1;    r1    = r0 + m;
            f1    = r1->r * co; f2    = r1->i * si;
            f3    = r1->r * si; f4    = r1->i * co;
            f1   -= f2;         f3   += f4;
            f2    = r0->r;      f4    = r0->i;
            r1->r = f2 - f1;    r1->i = f4 - f3;
            r0->r = f2 + f1;    r0->i = f4 + f3;
         }
         k++;
      }
      m = i3;
   }

#ifdef SCALE_INVERSE
   if (mode > 0) {
      f0 = 1.0 / idim ;
      i0 = 0; i1 = 1;
      while (i0 < n) {
         r0    = xc + i0; r1    = xc + i1; f1    = r0->r; f2    = r0->i;
         f3    = r1->r;   f4    = r1->i;   f1   *= f0;    f2   *= f0;
         f3   *= f0;      f4   *= f0;      r0->r = f1;    r0->i = f2;
         r1->r = f3;      r1->i = f4;
         i0 += 2; i1 += 2;
      }
   }
#endif

   return ;
}

/*--------------------------------------------------------------------
   Many complex-to-complex FFTs in place [vectorized]:
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

   if( nvec == 1 ){ csfft_cox( mode , idim , xc ) ; return ; }

   /** perhaps initialize **/

   if( nold != idim ) csfft_trigconsts( idim ) ;

   n   = idim;
   i2  = idim >> 1;
   i1  = 0;
   csp = (mode > 0) ? csplus : csminus ;  /* choose const array */

   for (i0=0; i0 < n; i0 ++) {
      if ( i1 > i0 ) {
         for( iv=0,xcx=xc ; iv < nvec ; iv++,xcx+=n ){
            r0    = xcx + i0; r1    = xcx + i1;
            f1    = r0->r;    f2    = r0->i;
            r0->r = r1->r;    r0->i = r1->i;
            r1->r = f1;       r1->i = f2;
         }
      }
      m = i2;
      while ( m && !(i1 < m) ) { i1 -= m; m >>= 1; }
     i1 += m;
   }

#define I00
#ifdef I00
#  define I0BOT 1
#else
#  define I0BOT 0
#endif

   m = 1;
   k = 0;
   while (n > m) {
      i3 = m << 1;

#ifdef I00
      /* handle i0=0 case [co=1,si=0] in special code */

      for (i1=0; i1 < n; i1 += i3) {
         for( iv=0,r0=xc+i1 ; iv < nvec ; iv++,r0+=n ){
            r1    = r0 + m;
            f1    = r1->r ;   f3    = r1->i ;
            f2    = r0->r ;   f4    = r0->i ;
            r1->r = f2 - f1 ; r1->i = f4 - f3 ;
            r0->r = f2 + f1 ; r0->i = f4 + f3 ;
         }
      }
      k++;
#endif

      for (i0=I0BOT; i0 < m; i0 ++) {
         co = (csp + k)->r; si = (csp + k)->i;
         for (i1=i0; i1 < n; i1 += i3) {
            for( iv=0,r0=xc+i1 ; iv < nvec ; iv++,r0+=n ){
               r1    = r0 + m;
#if 1
               f1    = r1->r * co - r1->i * si ;
               f3    = r1->r * si + r1->i * co ;
#else
               f1    = r1->r * co ; f2    = r1->i * si ;
               f3    = r1->r * si ; f4    = r1->i * co ;
               f1   -= f2 ;         f3   += f4 ;
#endif
               f2    = r0->r ;   f4    = r0->i ;
               r1->r = f2 - f1 ; r1->i = f4 - f3 ;
               r0->r = f2 + f1 ; r0->i = f4 + f3 ;
            }
         }
         k++;
      }
      m = i3;
   }
   return ;
}

/****************************************************************************
   Everything from here on down is for the unrolled FFT routines.
   They were generated with program fftprint.c.
*****************************************************************************/

#ifndef DONT_UNROLL_FFTS

/**************************************/
/* FFT routine unrolled of length   8 */

void fft8( int mode , complex * xc )
{
   register complex * csp , * xcx=xc;
   register float f1,f2,f3,f4 ;

   /** perhaps initialize **/

   if( nold != 8 ) csfft_trigconsts( 8 ) ;

   csp = (mode > 0) ? csplus : csminus ;  /* choose const array */

   /** data swapping part **/

   f1 = xcx[1].r ; f2 = xcx[1].i ;
   xcx[1].r = xcx[4].r ; xcx[1].i = xcx[4].i ;
   xcx[4].r = f1 ; xcx[4].i = f2 ;

   f1 = xcx[3].r ; f2 = xcx[3].i ;
   xcx[3].r = xcx[6].r ; xcx[3].i = xcx[6].i ;
   xcx[6].r = f1 ; xcx[6].i = f2 ;

   /** butterflying part **/

   f1 = xcx[1].r ; f3 = xcx[1].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[1].r = f2-f1 ; xcx[1].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[3].r ; f3 = xcx[3].i ;  /* cos=1 sin=0 */
   f2 = xcx[2].r ; f4 = xcx[2].i ;
   xcx[3].r = f2-f1 ; xcx[3].i = f4-f3 ;
   xcx[2].r = f2+f1 ; xcx[2].i = f4+f3 ;

   f1 = xcx[5].r ; f3 = xcx[5].i ;  /* cos=1 sin=0 */
   f2 = xcx[4].r ; f4 = xcx[4].i ;
   xcx[5].r = f2-f1 ; xcx[5].i = f4-f3 ;
   xcx[4].r = f2+f1 ; xcx[4].i = f4+f3 ;

   f1 = xcx[7].r ; f3 = xcx[7].i ;  /* cos=1 sin=0 */
   f2 = xcx[6].r ; f4 = xcx[6].i ;
   xcx[7].r = f2-f1 ; xcx[7].i = f4-f3 ;
   xcx[6].r = f2+f1 ; xcx[6].i = f4+f3 ;

   f1 = xcx[2].r ; f3 = xcx[2].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[2].r = f2-f1 ; xcx[2].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[6].r ; f3 = xcx[6].i ;  /* cos=1 sin=0 */
   f2 = xcx[4].r ; f4 = xcx[4].i ;
   xcx[6].r = f2-f1 ; xcx[6].i = f4-f3 ;
   xcx[4].r = f2+f1 ; xcx[4].i = f4+f3 ;

   f1 = xcx[3].r * csp[2].r - xcx[3].i * csp[2].i ; /* twiddles */
   f3 = xcx[3].r * csp[2].i + xcx[3].i * csp[2].r ;
   f2 = xcx[1].r ; f4 = xcx[1].i ;
   xcx[3].r = f2-f1 ; xcx[3].i = f4-f3 ;
   xcx[1].r = f2+f1 ; xcx[1].i = f4+f3 ;

   f1 = xcx[7].r * csp[2].r - xcx[7].i * csp[2].i ; /* twiddles */
   f3 = xcx[7].r * csp[2].i + xcx[7].i * csp[2].r ;
   f2 = xcx[5].r ; f4 = xcx[5].i ;
   xcx[7].r = f2-f1 ; xcx[7].i = f4-f3 ;
   xcx[5].r = f2+f1 ; xcx[5].i = f4+f3 ;

   f1 = xcx[4].r ; f3 = xcx[4].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[4].r = f2-f1 ; xcx[4].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[5].r * csp[4].r - xcx[5].i * csp[4].i ; /* twiddles */
   f3 = xcx[5].r * csp[4].i + xcx[5].i * csp[4].r ;
   f2 = xcx[1].r ; f4 = xcx[1].i ;
   xcx[5].r = f2-f1 ; xcx[5].i = f4-f3 ;
   xcx[1].r = f2+f1 ; xcx[1].i = f4+f3 ;

   f1 = xcx[6].r * csp[5].r - xcx[6].i * csp[5].i ; /* twiddles */
   f3 = xcx[6].r * csp[5].i + xcx[6].i * csp[5].r ;
   f2 = xcx[2].r ; f4 = xcx[2].i ;
   xcx[6].r = f2-f1 ; xcx[6].i = f4-f3 ;
   xcx[2].r = f2+f1 ; xcx[2].i = f4+f3 ;

   f1 = xcx[7].r * csp[6].r - xcx[7].i * csp[6].i ; /* twiddles */
   f3 = xcx[7].r * csp[6].i + xcx[7].i * csp[6].r ;
   f2 = xcx[3].r ; f4 = xcx[3].i ;
   xcx[7].r = f2-f1 ; xcx[7].i = f4-f3 ;
   xcx[3].r = f2+f1 ; xcx[3].i = f4+f3 ;

   return ;
}

/**************************************/
/* FFT routine unrolled of length  16 */

static void fft16( int mode , complex * xc )
{
   register complex * csp , * xcx=xc;
   register float f1,f2,f3,f4 ;

   /** perhaps initialize **/

   if( nold != 16 ) csfft_trigconsts( 16 ) ;

   csp = (mode > 0) ? csplus : csminus ;  /* choose const array */

   /** data swapping part **/

   f1 = xcx[1].r ; f2 = xcx[1].i ;
   xcx[1].r = xcx[8].r ; xcx[1].i = xcx[8].i ;
   xcx[8].r = f1 ; xcx[8].i = f2 ;

   f1 = xcx[2].r ; f2 = xcx[2].i ;
   xcx[2].r = xcx[4].r ; xcx[2].i = xcx[4].i ;
   xcx[4].r = f1 ; xcx[4].i = f2 ;

   f1 = xcx[3].r ; f2 = xcx[3].i ;
   xcx[3].r = xcx[12].r ; xcx[3].i = xcx[12].i ;
   xcx[12].r = f1 ; xcx[12].i = f2 ;

   f1 = xcx[5].r ; f2 = xcx[5].i ;
   xcx[5].r = xcx[10].r ; xcx[5].i = xcx[10].i ;
   xcx[10].r = f1 ; xcx[10].i = f2 ;

   f1 = xcx[7].r ; f2 = xcx[7].i ;
   xcx[7].r = xcx[14].r ; xcx[7].i = xcx[14].i ;
   xcx[14].r = f1 ; xcx[14].i = f2 ;

   f1 = xcx[11].r ; f2 = xcx[11].i ;
   xcx[11].r = xcx[13].r ; xcx[11].i = xcx[13].i ;
   xcx[13].r = f1 ; xcx[13].i = f2 ;

   /** butterflying part **/

   f1 = xcx[1].r ; f3 = xcx[1].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[1].r = f2-f1 ; xcx[1].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[3].r ; f3 = xcx[3].i ;  /* cos=1 sin=0 */
   f2 = xcx[2].r ; f4 = xcx[2].i ;
   xcx[3].r = f2-f1 ; xcx[3].i = f4-f3 ;
   xcx[2].r = f2+f1 ; xcx[2].i = f4+f3 ;

   f1 = xcx[5].r ; f3 = xcx[5].i ;  /* cos=1 sin=0 */
   f2 = xcx[4].r ; f4 = xcx[4].i ;
   xcx[5].r = f2-f1 ; xcx[5].i = f4-f3 ;
   xcx[4].r = f2+f1 ; xcx[4].i = f4+f3 ;

   f1 = xcx[7].r ; f3 = xcx[7].i ;  /* cos=1 sin=0 */
   f2 = xcx[6].r ; f4 = xcx[6].i ;
   xcx[7].r = f2-f1 ; xcx[7].i = f4-f3 ;
   xcx[6].r = f2+f1 ; xcx[6].i = f4+f3 ;

   f1 = xcx[9].r ; f3 = xcx[9].i ;  /* cos=1 sin=0 */
   f2 = xcx[8].r ; f4 = xcx[8].i ;
   xcx[9].r = f2-f1 ; xcx[9].i = f4-f3 ;
   xcx[8].r = f2+f1 ; xcx[8].i = f4+f3 ;

   f1 = xcx[11].r ; f3 = xcx[11].i ;  /* cos=1 sin=0 */
   f2 = xcx[10].r ; f4 = xcx[10].i ;
   xcx[11].r = f2-f1 ; xcx[11].i = f4-f3 ;
   xcx[10].r = f2+f1 ; xcx[10].i = f4+f3 ;

   f1 = xcx[13].r ; f3 = xcx[13].i ;  /* cos=1 sin=0 */
   f2 = xcx[12].r ; f4 = xcx[12].i ;
   xcx[13].r = f2-f1 ; xcx[13].i = f4-f3 ;
   xcx[12].r = f2+f1 ; xcx[12].i = f4+f3 ;

   f1 = xcx[15].r ; f3 = xcx[15].i ;  /* cos=1 sin=0 */
   f2 = xcx[14].r ; f4 = xcx[14].i ;
   xcx[15].r = f2-f1 ; xcx[15].i = f4-f3 ;
   xcx[14].r = f2+f1 ; xcx[14].i = f4+f3 ;

   f1 = xcx[2].r ; f3 = xcx[2].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[2].r = f2-f1 ; xcx[2].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[6].r ; f3 = xcx[6].i ;  /* cos=1 sin=0 */
   f2 = xcx[4].r ; f4 = xcx[4].i ;
   xcx[6].r = f2-f1 ; xcx[6].i = f4-f3 ;
   xcx[4].r = f2+f1 ; xcx[4].i = f4+f3 ;

   f1 = xcx[10].r ; f3 = xcx[10].i ;  /* cos=1 sin=0 */
   f2 = xcx[8].r ; f4 = xcx[8].i ;
   xcx[10].r = f2-f1 ; xcx[10].i = f4-f3 ;
   xcx[8].r = f2+f1 ; xcx[8].i = f4+f3 ;

   f1 = xcx[14].r ; f3 = xcx[14].i ;  /* cos=1 sin=0 */
   f2 = xcx[12].r ; f4 = xcx[12].i ;
   xcx[14].r = f2-f1 ; xcx[14].i = f4-f3 ;
   xcx[12].r = f2+f1 ; xcx[12].i = f4+f3 ;

   f1 = xcx[3].r * csp[2].r - xcx[3].i * csp[2].i ; /* twiddles */
   f3 = xcx[3].r * csp[2].i + xcx[3].i * csp[2].r ;
   f2 = xcx[1].r ; f4 = xcx[1].i ;
   xcx[3].r = f2-f1 ; xcx[3].i = f4-f3 ;
   xcx[1].r = f2+f1 ; xcx[1].i = f4+f3 ;

   f1 = xcx[7].r * csp[2].r - xcx[7].i * csp[2].i ; /* twiddles */
   f3 = xcx[7].r * csp[2].i + xcx[7].i * csp[2].r ;
   f2 = xcx[5].r ; f4 = xcx[5].i ;
   xcx[7].r = f2-f1 ; xcx[7].i = f4-f3 ;
   xcx[5].r = f2+f1 ; xcx[5].i = f4+f3 ;

   f1 = xcx[11].r * csp[2].r - xcx[11].i * csp[2].i ; /* twiddles */
   f3 = xcx[11].r * csp[2].i + xcx[11].i * csp[2].r ;
   f2 = xcx[9].r ; f4 = xcx[9].i ;
   xcx[11].r = f2-f1 ; xcx[11].i = f4-f3 ;
   xcx[9].r = f2+f1 ; xcx[9].i = f4+f3 ;

   f1 = xcx[15].r * csp[2].r - xcx[15].i * csp[2].i ; /* twiddles */
   f3 = xcx[15].r * csp[2].i + xcx[15].i * csp[2].r ;
   f2 = xcx[13].r ; f4 = xcx[13].i ;
   xcx[15].r = f2-f1 ; xcx[15].i = f4-f3 ;
   xcx[13].r = f2+f1 ; xcx[13].i = f4+f3 ;

   f1 = xcx[4].r ; f3 = xcx[4].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[4].r = f2-f1 ; xcx[4].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[12].r ; f3 = xcx[12].i ;  /* cos=1 sin=0 */
   f2 = xcx[8].r ; f4 = xcx[8].i ;
   xcx[12].r = f2-f1 ; xcx[12].i = f4-f3 ;
   xcx[8].r = f2+f1 ; xcx[8].i = f4+f3 ;

   f1 = xcx[5].r * csp[4].r - xcx[5].i * csp[4].i ; /* twiddles */
   f3 = xcx[5].r * csp[4].i + xcx[5].i * csp[4].r ;
   f2 = xcx[1].r ; f4 = xcx[1].i ;
   xcx[5].r = f2-f1 ; xcx[5].i = f4-f3 ;
   xcx[1].r = f2+f1 ; xcx[1].i = f4+f3 ;

   f1 = xcx[13].r * csp[4].r - xcx[13].i * csp[4].i ; /* twiddles */
   f3 = xcx[13].r * csp[4].i + xcx[13].i * csp[4].r ;
   f2 = xcx[9].r ; f4 = xcx[9].i ;
   xcx[13].r = f2-f1 ; xcx[13].i = f4-f3 ;
   xcx[9].r = f2+f1 ; xcx[9].i = f4+f3 ;

   f1 = xcx[6].r * csp[5].r - xcx[6].i * csp[5].i ; /* twiddles */
   f3 = xcx[6].r * csp[5].i + xcx[6].i * csp[5].r ;
   f2 = xcx[2].r ; f4 = xcx[2].i ;
   xcx[6].r = f2-f1 ; xcx[6].i = f4-f3 ;
   xcx[2].r = f2+f1 ; xcx[2].i = f4+f3 ;

   f1 = xcx[14].r * csp[5].r - xcx[14].i * csp[5].i ; /* twiddles */
   f3 = xcx[14].r * csp[5].i + xcx[14].i * csp[5].r ;
   f2 = xcx[10].r ; f4 = xcx[10].i ;
   xcx[14].r = f2-f1 ; xcx[14].i = f4-f3 ;
   xcx[10].r = f2+f1 ; xcx[10].i = f4+f3 ;

   f1 = xcx[7].r * csp[6].r - xcx[7].i * csp[6].i ; /* twiddles */
   f3 = xcx[7].r * csp[6].i + xcx[7].i * csp[6].r ;
   f2 = xcx[3].r ; f4 = xcx[3].i ;
   xcx[7].r = f2-f1 ; xcx[7].i = f4-f3 ;
   xcx[3].r = f2+f1 ; xcx[3].i = f4+f3 ;

   f1 = xcx[15].r * csp[6].r - xcx[15].i * csp[6].i ; /* twiddles */
   f3 = xcx[15].r * csp[6].i + xcx[15].i * csp[6].r ;
   f2 = xcx[11].r ; f4 = xcx[11].i ;
   xcx[15].r = f2-f1 ; xcx[15].i = f4-f3 ;
   xcx[11].r = f2+f1 ; xcx[11].i = f4+f3 ;

   f1 = xcx[8].r ; f3 = xcx[8].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[8].r = f2-f1 ; xcx[8].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[9].r * csp[8].r - xcx[9].i * csp[8].i ; /* twiddles */
   f3 = xcx[9].r * csp[8].i + xcx[9].i * csp[8].r ;
   f2 = xcx[1].r ; f4 = xcx[1].i ;
   xcx[9].r = f2-f1 ; xcx[9].i = f4-f3 ;
   xcx[1].r = f2+f1 ; xcx[1].i = f4+f3 ;

   f1 = xcx[10].r * csp[9].r - xcx[10].i * csp[9].i ; /* twiddles */
   f3 = xcx[10].r * csp[9].i + xcx[10].i * csp[9].r ;
   f2 = xcx[2].r ; f4 = xcx[2].i ;
   xcx[10].r = f2-f1 ; xcx[10].i = f4-f3 ;
   xcx[2].r = f2+f1 ; xcx[2].i = f4+f3 ;

   f1 = xcx[11].r * csp[10].r - xcx[11].i * csp[10].i ; /* twiddles */
   f3 = xcx[11].r * csp[10].i + xcx[11].i * csp[10].r ;
   f2 = xcx[3].r ; f4 = xcx[3].i ;
   xcx[11].r = f2-f1 ; xcx[11].i = f4-f3 ;
   xcx[3].r = f2+f1 ; xcx[3].i = f4+f3 ;

   f1 = xcx[12].r * csp[11].r - xcx[12].i * csp[11].i ; /* twiddles */
   f3 = xcx[12].r * csp[11].i + xcx[12].i * csp[11].r ;
   f2 = xcx[4].r ; f4 = xcx[4].i ;
   xcx[12].r = f2-f1 ; xcx[12].i = f4-f3 ;
   xcx[4].r = f2+f1 ; xcx[4].i = f4+f3 ;

   f1 = xcx[13].r * csp[12].r - xcx[13].i * csp[12].i ; /* twiddles */
   f3 = xcx[13].r * csp[12].i + xcx[13].i * csp[12].r ;
   f2 = xcx[5].r ; f4 = xcx[5].i ;
   xcx[13].r = f2-f1 ; xcx[13].i = f4-f3 ;
   xcx[5].r = f2+f1 ; xcx[5].i = f4+f3 ;

   f1 = xcx[14].r * csp[13].r - xcx[14].i * csp[13].i ; /* twiddles */
   f3 = xcx[14].r * csp[13].i + xcx[14].i * csp[13].r ;
   f2 = xcx[6].r ; f4 = xcx[6].i ;
   xcx[14].r = f2-f1 ; xcx[14].i = f4-f3 ;
   xcx[6].r = f2+f1 ; xcx[6].i = f4+f3 ;

   f1 = xcx[15].r * csp[14].r - xcx[15].i * csp[14].i ; /* twiddles */
   f3 = xcx[15].r * csp[14].i + xcx[15].i * csp[14].r ;
   f2 = xcx[7].r ; f4 = xcx[7].i ;
   xcx[15].r = f2-f1 ; xcx[15].i = f4-f3 ;
   xcx[7].r = f2+f1 ; xcx[7].i = f4+f3 ;

   return ;
}

/**************************************/
/* FFT routine unrolled of length  32 */

void fft32( int mode , complex * xc )
{
   register complex * csp , * xcx=xc;
   register float f1,f2,f3,f4 ;

   /** perhaps initialize **/

   if( nold != 32 ) csfft_trigconsts( 32 ) ;

   csp = (mode > 0) ? csplus : csminus ;  /* choose const array */

   /** data swapping part **/

   f1 = xcx[1].r ; f2 = xcx[1].i ;
   xcx[1].r = xcx[16].r ; xcx[1].i = xcx[16].i ;
   xcx[16].r = f1 ; xcx[16].i = f2 ;

   f1 = xcx[2].r ; f2 = xcx[2].i ;
   xcx[2].r = xcx[8].r ; xcx[2].i = xcx[8].i ;
   xcx[8].r = f1 ; xcx[8].i = f2 ;

   f1 = xcx[3].r ; f2 = xcx[3].i ;
   xcx[3].r = xcx[24].r ; xcx[3].i = xcx[24].i ;
   xcx[24].r = f1 ; xcx[24].i = f2 ;

   f1 = xcx[5].r ; f2 = xcx[5].i ;
   xcx[5].r = xcx[20].r ; xcx[5].i = xcx[20].i ;
   xcx[20].r = f1 ; xcx[20].i = f2 ;

   f1 = xcx[6].r ; f2 = xcx[6].i ;
   xcx[6].r = xcx[12].r ; xcx[6].i = xcx[12].i ;
   xcx[12].r = f1 ; xcx[12].i = f2 ;

   f1 = xcx[7].r ; f2 = xcx[7].i ;
   xcx[7].r = xcx[28].r ; xcx[7].i = xcx[28].i ;
   xcx[28].r = f1 ; xcx[28].i = f2 ;

   f1 = xcx[9].r ; f2 = xcx[9].i ;
   xcx[9].r = xcx[18].r ; xcx[9].i = xcx[18].i ;
   xcx[18].r = f1 ; xcx[18].i = f2 ;

   f1 = xcx[11].r ; f2 = xcx[11].i ;
   xcx[11].r = xcx[26].r ; xcx[11].i = xcx[26].i ;
   xcx[26].r = f1 ; xcx[26].i = f2 ;

   f1 = xcx[13].r ; f2 = xcx[13].i ;
   xcx[13].r = xcx[22].r ; xcx[13].i = xcx[22].i ;
   xcx[22].r = f1 ; xcx[22].i = f2 ;

   f1 = xcx[15].r ; f2 = xcx[15].i ;
   xcx[15].r = xcx[30].r ; xcx[15].i = xcx[30].i ;
   xcx[30].r = f1 ; xcx[30].i = f2 ;

   f1 = xcx[19].r ; f2 = xcx[19].i ;
   xcx[19].r = xcx[25].r ; xcx[19].i = xcx[25].i ;
   xcx[25].r = f1 ; xcx[25].i = f2 ;

   f1 = xcx[23].r ; f2 = xcx[23].i ;
   xcx[23].r = xcx[29].r ; xcx[23].i = xcx[29].i ;
   xcx[29].r = f1 ; xcx[29].i = f2 ;

   /** butterflying part **/

   f1 = xcx[1].r ; f3 = xcx[1].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[1].r = f2-f1 ; xcx[1].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[3].r ; f3 = xcx[3].i ;  /* cos=1 sin=0 */
   f2 = xcx[2].r ; f4 = xcx[2].i ;
   xcx[3].r = f2-f1 ; xcx[3].i = f4-f3 ;
   xcx[2].r = f2+f1 ; xcx[2].i = f4+f3 ;

   f1 = xcx[5].r ; f3 = xcx[5].i ;  /* cos=1 sin=0 */
   f2 = xcx[4].r ; f4 = xcx[4].i ;
   xcx[5].r = f2-f1 ; xcx[5].i = f4-f3 ;
   xcx[4].r = f2+f1 ; xcx[4].i = f4+f3 ;

   f1 = xcx[7].r ; f3 = xcx[7].i ;  /* cos=1 sin=0 */
   f2 = xcx[6].r ; f4 = xcx[6].i ;
   xcx[7].r = f2-f1 ; xcx[7].i = f4-f3 ;
   xcx[6].r = f2+f1 ; xcx[6].i = f4+f3 ;

   f1 = xcx[9].r ; f3 = xcx[9].i ;  /* cos=1 sin=0 */
   f2 = xcx[8].r ; f4 = xcx[8].i ;
   xcx[9].r = f2-f1 ; xcx[9].i = f4-f3 ;
   xcx[8].r = f2+f1 ; xcx[8].i = f4+f3 ;

   f1 = xcx[11].r ; f3 = xcx[11].i ;  /* cos=1 sin=0 */
   f2 = xcx[10].r ; f4 = xcx[10].i ;
   xcx[11].r = f2-f1 ; xcx[11].i = f4-f3 ;
   xcx[10].r = f2+f1 ; xcx[10].i = f4+f3 ;

   f1 = xcx[13].r ; f3 = xcx[13].i ;  /* cos=1 sin=0 */
   f2 = xcx[12].r ; f4 = xcx[12].i ;
   xcx[13].r = f2-f1 ; xcx[13].i = f4-f3 ;
   xcx[12].r = f2+f1 ; xcx[12].i = f4+f3 ;

   f1 = xcx[15].r ; f3 = xcx[15].i ;  /* cos=1 sin=0 */
   f2 = xcx[14].r ; f4 = xcx[14].i ;
   xcx[15].r = f2-f1 ; xcx[15].i = f4-f3 ;
   xcx[14].r = f2+f1 ; xcx[14].i = f4+f3 ;

   f1 = xcx[17].r ; f3 = xcx[17].i ;  /* cos=1 sin=0 */
   f2 = xcx[16].r ; f4 = xcx[16].i ;
   xcx[17].r = f2-f1 ; xcx[17].i = f4-f3 ;
   xcx[16].r = f2+f1 ; xcx[16].i = f4+f3 ;

   f1 = xcx[19].r ; f3 = xcx[19].i ;  /* cos=1 sin=0 */
   f2 = xcx[18].r ; f4 = xcx[18].i ;
   xcx[19].r = f2-f1 ; xcx[19].i = f4-f3 ;
   xcx[18].r = f2+f1 ; xcx[18].i = f4+f3 ;

   f1 = xcx[21].r ; f3 = xcx[21].i ;  /* cos=1 sin=0 */
   f2 = xcx[20].r ; f4 = xcx[20].i ;
   xcx[21].r = f2-f1 ; xcx[21].i = f4-f3 ;
   xcx[20].r = f2+f1 ; xcx[20].i = f4+f3 ;

   f1 = xcx[23].r ; f3 = xcx[23].i ;  /* cos=1 sin=0 */
   f2 = xcx[22].r ; f4 = xcx[22].i ;
   xcx[23].r = f2-f1 ; xcx[23].i = f4-f3 ;
   xcx[22].r = f2+f1 ; xcx[22].i = f4+f3 ;

   f1 = xcx[25].r ; f3 = xcx[25].i ;  /* cos=1 sin=0 */
   f2 = xcx[24].r ; f4 = xcx[24].i ;
   xcx[25].r = f2-f1 ; xcx[25].i = f4-f3 ;
   xcx[24].r = f2+f1 ; xcx[24].i = f4+f3 ;

   f1 = xcx[27].r ; f3 = xcx[27].i ;  /* cos=1 sin=0 */
   f2 = xcx[26].r ; f4 = xcx[26].i ;
   xcx[27].r = f2-f1 ; xcx[27].i = f4-f3 ;
   xcx[26].r = f2+f1 ; xcx[26].i = f4+f3 ;

   f1 = xcx[29].r ; f3 = xcx[29].i ;  /* cos=1 sin=0 */
   f2 = xcx[28].r ; f4 = xcx[28].i ;
   xcx[29].r = f2-f1 ; xcx[29].i = f4-f3 ;
   xcx[28].r = f2+f1 ; xcx[28].i = f4+f3 ;

   f1 = xcx[31].r ; f3 = xcx[31].i ;  /* cos=1 sin=0 */
   f2 = xcx[30].r ; f4 = xcx[30].i ;
   xcx[31].r = f2-f1 ; xcx[31].i = f4-f3 ;
   xcx[30].r = f2+f1 ; xcx[30].i = f4+f3 ;

   f1 = xcx[2].r ; f3 = xcx[2].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[2].r = f2-f1 ; xcx[2].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[6].r ; f3 = xcx[6].i ;  /* cos=1 sin=0 */
   f2 = xcx[4].r ; f4 = xcx[4].i ;
   xcx[6].r = f2-f1 ; xcx[6].i = f4-f3 ;
   xcx[4].r = f2+f1 ; xcx[4].i = f4+f3 ;

   f1 = xcx[10].r ; f3 = xcx[10].i ;  /* cos=1 sin=0 */
   f2 = xcx[8].r ; f4 = xcx[8].i ;
   xcx[10].r = f2-f1 ; xcx[10].i = f4-f3 ;
   xcx[8].r = f2+f1 ; xcx[8].i = f4+f3 ;

   f1 = xcx[14].r ; f3 = xcx[14].i ;  /* cos=1 sin=0 */
   f2 = xcx[12].r ; f4 = xcx[12].i ;
   xcx[14].r = f2-f1 ; xcx[14].i = f4-f3 ;
   xcx[12].r = f2+f1 ; xcx[12].i = f4+f3 ;

   f1 = xcx[18].r ; f3 = xcx[18].i ;  /* cos=1 sin=0 */
   f2 = xcx[16].r ; f4 = xcx[16].i ;
   xcx[18].r = f2-f1 ; xcx[18].i = f4-f3 ;
   xcx[16].r = f2+f1 ; xcx[16].i = f4+f3 ;

   f1 = xcx[22].r ; f3 = xcx[22].i ;  /* cos=1 sin=0 */
   f2 = xcx[20].r ; f4 = xcx[20].i ;
   xcx[22].r = f2-f1 ; xcx[22].i = f4-f3 ;
   xcx[20].r = f2+f1 ; xcx[20].i = f4+f3 ;

   f1 = xcx[26].r ; f3 = xcx[26].i ;  /* cos=1 sin=0 */
   f2 = xcx[24].r ; f4 = xcx[24].i ;
   xcx[26].r = f2-f1 ; xcx[26].i = f4-f3 ;
   xcx[24].r = f2+f1 ; xcx[24].i = f4+f3 ;

   f1 = xcx[30].r ; f3 = xcx[30].i ;  /* cos=1 sin=0 */
   f2 = xcx[28].r ; f4 = xcx[28].i ;
   xcx[30].r = f2-f1 ; xcx[30].i = f4-f3 ;
   xcx[28].r = f2+f1 ; xcx[28].i = f4+f3 ;

   f1 = - xcx[3].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[3].r * csp[2].i ;
   f2 = xcx[1].r ; f4 = xcx[1].i ;
   xcx[3].r = f2-f1 ; xcx[3].i = f4-f3 ;
   xcx[1].r = f2+f1 ; xcx[1].i = f4+f3 ;

   f1 = - xcx[7].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[7].r * csp[2].i ;
   f2 = xcx[5].r ; f4 = xcx[5].i ;
   xcx[7].r = f2-f1 ; xcx[7].i = f4-f3 ;
   xcx[5].r = f2+f1 ; xcx[5].i = f4+f3 ;

   f1 = - xcx[11].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[11].r * csp[2].i ;
   f2 = xcx[9].r ; f4 = xcx[9].i ;
   xcx[11].r = f2-f1 ; xcx[11].i = f4-f3 ;
   xcx[9].r = f2+f1 ; xcx[9].i = f4+f3 ;

   f1 = - xcx[15].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[15].r * csp[2].i ;
   f2 = xcx[13].r ; f4 = xcx[13].i ;
   xcx[15].r = f2-f1 ; xcx[15].i = f4-f3 ;
   xcx[13].r = f2+f1 ; xcx[13].i = f4+f3 ;

   f1 = - xcx[19].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[19].r * csp[2].i ;
   f2 = xcx[17].r ; f4 = xcx[17].i ;
   xcx[19].r = f2-f1 ; xcx[19].i = f4-f3 ;
   xcx[17].r = f2+f1 ; xcx[17].i = f4+f3 ;

   f1 = - xcx[23].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[23].r * csp[2].i ;
   f2 = xcx[21].r ; f4 = xcx[21].i ;
   xcx[23].r = f2-f1 ; xcx[23].i = f4-f3 ;
   xcx[21].r = f2+f1 ; xcx[21].i = f4+f3 ;

   f1 = - xcx[27].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[27].r * csp[2].i ;
   f2 = xcx[25].r ; f4 = xcx[25].i ;
   xcx[27].r = f2-f1 ; xcx[27].i = f4-f3 ;
   xcx[25].r = f2+f1 ; xcx[25].i = f4+f3 ;

   f1 = - xcx[31].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[31].r * csp[2].i ;
   f2 = xcx[29].r ; f4 = xcx[29].i ;
   xcx[31].r = f2-f1 ; xcx[31].i = f4-f3 ;
   xcx[29].r = f2+f1 ; xcx[29].i = f4+f3 ;

   f1 = xcx[4].r ; f3 = xcx[4].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[4].r = f2-f1 ; xcx[4].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[12].r ; f3 = xcx[12].i ;  /* cos=1 sin=0 */
   f2 = xcx[8].r ; f4 = xcx[8].i ;
   xcx[12].r = f2-f1 ; xcx[12].i = f4-f3 ;
   xcx[8].r = f2+f1 ; xcx[8].i = f4+f3 ;

   f1 = xcx[20].r ; f3 = xcx[20].i ;  /* cos=1 sin=0 */
   f2 = xcx[16].r ; f4 = xcx[16].i ;
   xcx[20].r = f2-f1 ; xcx[20].i = f4-f3 ;
   xcx[16].r = f2+f1 ; xcx[16].i = f4+f3 ;

   f1 = xcx[28].r ; f3 = xcx[28].i ;  /* cos=1 sin=0 */
   f2 = xcx[24].r ; f4 = xcx[24].i ;
   xcx[28].r = f2-f1 ; xcx[28].i = f4-f3 ;
   xcx[24].r = f2+f1 ; xcx[24].i = f4+f3 ;

   f1 = xcx[5].r * csp[4].r - xcx[5].i * csp[4].i ; /* twiddles */
   f3 = xcx[5].r * csp[4].i + xcx[5].i * csp[4].r ;
   f2 = xcx[1].r ; f4 = xcx[1].i ;
   xcx[5].r = f2-f1 ; xcx[5].i = f4-f3 ;
   xcx[1].r = f2+f1 ; xcx[1].i = f4+f3 ;

   f1 = xcx[13].r * csp[4].r - xcx[13].i * csp[4].i ; /* twiddles */
   f3 = xcx[13].r * csp[4].i + xcx[13].i * csp[4].r ;
   f2 = xcx[9].r ; f4 = xcx[9].i ;
   xcx[13].r = f2-f1 ; xcx[13].i = f4-f3 ;
   xcx[9].r = f2+f1 ; xcx[9].i = f4+f3 ;

   f1 = xcx[21].r * csp[4].r - xcx[21].i * csp[4].i ; /* twiddles */
   f3 = xcx[21].r * csp[4].i + xcx[21].i * csp[4].r ;
   f2 = xcx[17].r ; f4 = xcx[17].i ;
   xcx[21].r = f2-f1 ; xcx[21].i = f4-f3 ;
   xcx[17].r = f2+f1 ; xcx[17].i = f4+f3 ;

   f1 = xcx[29].r * csp[4].r - xcx[29].i * csp[4].i ; /* twiddles */
   f3 = xcx[29].r * csp[4].i + xcx[29].i * csp[4].r ;
   f2 = xcx[25].r ; f4 = xcx[25].i ;
   xcx[29].r = f2-f1 ; xcx[29].i = f4-f3 ;
   xcx[25].r = f2+f1 ; xcx[25].i = f4+f3 ;

   f1 = - xcx[6].i * csp[5].i ; /* cos=0 twiddles */
   f3 = xcx[6].r * csp[5].i ;
   f2 = xcx[2].r ; f4 = xcx[2].i ;
   xcx[6].r = f2-f1 ; xcx[6].i = f4-f3 ;
   xcx[2].r = f2+f1 ; xcx[2].i = f4+f3 ;

   f1 = - xcx[14].i * csp[5].i ; /* cos=0 twiddles */
   f3 = xcx[14].r * csp[5].i ;
   f2 = xcx[10].r ; f4 = xcx[10].i ;
   xcx[14].r = f2-f1 ; xcx[14].i = f4-f3 ;
   xcx[10].r = f2+f1 ; xcx[10].i = f4+f3 ;

   f1 = - xcx[22].i * csp[5].i ; /* cos=0 twiddles */
   f3 = xcx[22].r * csp[5].i ;
   f2 = xcx[18].r ; f4 = xcx[18].i ;
   xcx[22].r = f2-f1 ; xcx[22].i = f4-f3 ;
   xcx[18].r = f2+f1 ; xcx[18].i = f4+f3 ;

   f1 = - xcx[30].i * csp[5].i ; /* cos=0 twiddles */
   f3 = xcx[30].r * csp[5].i ;
   f2 = xcx[26].r ; f4 = xcx[26].i ;
   xcx[30].r = f2-f1 ; xcx[30].i = f4-f3 ;
   xcx[26].r = f2+f1 ; xcx[26].i = f4+f3 ;

   f1 = xcx[7].r * csp[6].r - xcx[7].i * csp[6].i ; /* twiddles */
   f3 = xcx[7].r * csp[6].i + xcx[7].i * csp[6].r ;
   f2 = xcx[3].r ; f4 = xcx[3].i ;
   xcx[7].r = f2-f1 ; xcx[7].i = f4-f3 ;
   xcx[3].r = f2+f1 ; xcx[3].i = f4+f3 ;

   f1 = xcx[15].r * csp[6].r - xcx[15].i * csp[6].i ; /* twiddles */
   f3 = xcx[15].r * csp[6].i + xcx[15].i * csp[6].r ;
   f2 = xcx[11].r ; f4 = xcx[11].i ;
   xcx[15].r = f2-f1 ; xcx[15].i = f4-f3 ;
   xcx[11].r = f2+f1 ; xcx[11].i = f4+f3 ;

   f1 = xcx[23].r * csp[6].r - xcx[23].i * csp[6].i ; /* twiddles */
   f3 = xcx[23].r * csp[6].i + xcx[23].i * csp[6].r ;
   f2 = xcx[19].r ; f4 = xcx[19].i ;
   xcx[23].r = f2-f1 ; xcx[23].i = f4-f3 ;
   xcx[19].r = f2+f1 ; xcx[19].i = f4+f3 ;

   f1 = xcx[31].r * csp[6].r - xcx[31].i * csp[6].i ; /* twiddles */
   f3 = xcx[31].r * csp[6].i + xcx[31].i * csp[6].r ;
   f2 = xcx[27].r ; f4 = xcx[27].i ;
   xcx[31].r = f2-f1 ; xcx[31].i = f4-f3 ;
   xcx[27].r = f2+f1 ; xcx[27].i = f4+f3 ;

   f1 = xcx[8].r ; f3 = xcx[8].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[8].r = f2-f1 ; xcx[8].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[24].r ; f3 = xcx[24].i ;  /* cos=1 sin=0 */
   f2 = xcx[16].r ; f4 = xcx[16].i ;
   xcx[24].r = f2-f1 ; xcx[24].i = f4-f3 ;
   xcx[16].r = f2+f1 ; xcx[16].i = f4+f3 ;

   f1 = xcx[9].r * csp[8].r - xcx[9].i * csp[8].i ; /* twiddles */
   f3 = xcx[9].r * csp[8].i + xcx[9].i * csp[8].r ;
   f2 = xcx[1].r ; f4 = xcx[1].i ;
   xcx[9].r = f2-f1 ; xcx[9].i = f4-f3 ;
   xcx[1].r = f2+f1 ; xcx[1].i = f4+f3 ;

   f1 = xcx[25].r * csp[8].r - xcx[25].i * csp[8].i ; /* twiddles */
   f3 = xcx[25].r * csp[8].i + xcx[25].i * csp[8].r ;
   f2 = xcx[17].r ; f4 = xcx[17].i ;
   xcx[25].r = f2-f1 ; xcx[25].i = f4-f3 ;
   xcx[17].r = f2+f1 ; xcx[17].i = f4+f3 ;

   f1 = xcx[10].r * csp[9].r - xcx[10].i * csp[9].i ; /* twiddles */
   f3 = xcx[10].r * csp[9].i + xcx[10].i * csp[9].r ;
   f2 = xcx[2].r ; f4 = xcx[2].i ;
   xcx[10].r = f2-f1 ; xcx[10].i = f4-f3 ;
   xcx[2].r = f2+f1 ; xcx[2].i = f4+f3 ;

   f1 = xcx[26].r * csp[9].r - xcx[26].i * csp[9].i ; /* twiddles */
   f3 = xcx[26].r * csp[9].i + xcx[26].i * csp[9].r ;
   f2 = xcx[18].r ; f4 = xcx[18].i ;
   xcx[26].r = f2-f1 ; xcx[26].i = f4-f3 ;
   xcx[18].r = f2+f1 ; xcx[18].i = f4+f3 ;

   f1 = xcx[11].r * csp[10].r - xcx[11].i * csp[10].i ; /* twiddles */
   f3 = xcx[11].r * csp[10].i + xcx[11].i * csp[10].r ;
   f2 = xcx[3].r ; f4 = xcx[3].i ;
   xcx[11].r = f2-f1 ; xcx[11].i = f4-f3 ;
   xcx[3].r = f2+f1 ; xcx[3].i = f4+f3 ;

   f1 = xcx[27].r * csp[10].r - xcx[27].i * csp[10].i ; /* twiddles */
   f3 = xcx[27].r * csp[10].i + xcx[27].i * csp[10].r ;
   f2 = xcx[19].r ; f4 = xcx[19].i ;
   xcx[27].r = f2-f1 ; xcx[27].i = f4-f3 ;
   xcx[19].r = f2+f1 ; xcx[19].i = f4+f3 ;

   f1 = - xcx[12].i * csp[11].i ; /* cos=0 twiddles */
   f3 = xcx[12].r * csp[11].i ;
   f2 = xcx[4].r ; f4 = xcx[4].i ;
   xcx[12].r = f2-f1 ; xcx[12].i = f4-f3 ;
   xcx[4].r = f2+f1 ; xcx[4].i = f4+f3 ;

   f1 = - xcx[28].i * csp[11].i ; /* cos=0 twiddles */
   f3 = xcx[28].r * csp[11].i ;
   f2 = xcx[20].r ; f4 = xcx[20].i ;
   xcx[28].r = f2-f1 ; xcx[28].i = f4-f3 ;
   xcx[20].r = f2+f1 ; xcx[20].i = f4+f3 ;

   f1 = xcx[13].r * csp[12].r - xcx[13].i * csp[12].i ; /* twiddles */
   f3 = xcx[13].r * csp[12].i + xcx[13].i * csp[12].r ;
   f2 = xcx[5].r ; f4 = xcx[5].i ;
   xcx[13].r = f2-f1 ; xcx[13].i = f4-f3 ;
   xcx[5].r = f2+f1 ; xcx[5].i = f4+f3 ;

   f1 = xcx[29].r * csp[12].r - xcx[29].i * csp[12].i ; /* twiddles */
   f3 = xcx[29].r * csp[12].i + xcx[29].i * csp[12].r ;
   f2 = xcx[21].r ; f4 = xcx[21].i ;
   xcx[29].r = f2-f1 ; xcx[29].i = f4-f3 ;
   xcx[21].r = f2+f1 ; xcx[21].i = f4+f3 ;

   f1 = xcx[14].r * csp[13].r - xcx[14].i * csp[13].i ; /* twiddles */
   f3 = xcx[14].r * csp[13].i + xcx[14].i * csp[13].r ;
   f2 = xcx[6].r ; f4 = xcx[6].i ;
   xcx[14].r = f2-f1 ; xcx[14].i = f4-f3 ;
   xcx[6].r = f2+f1 ; xcx[6].i = f4+f3 ;

   f1 = xcx[30].r * csp[13].r - xcx[30].i * csp[13].i ; /* twiddles */
   f3 = xcx[30].r * csp[13].i + xcx[30].i * csp[13].r ;
   f2 = xcx[22].r ; f4 = xcx[22].i ;
   xcx[30].r = f2-f1 ; xcx[30].i = f4-f3 ;
   xcx[22].r = f2+f1 ; xcx[22].i = f4+f3 ;

   f1 = xcx[15].r * csp[14].r - xcx[15].i * csp[14].i ; /* twiddles */
   f3 = xcx[15].r * csp[14].i + xcx[15].i * csp[14].r ;
   f2 = xcx[7].r ; f4 = xcx[7].i ;
   xcx[15].r = f2-f1 ; xcx[15].i = f4-f3 ;
   xcx[7].r = f2+f1 ; xcx[7].i = f4+f3 ;

   f1 = xcx[31].r * csp[14].r - xcx[31].i * csp[14].i ; /* twiddles */
   f3 = xcx[31].r * csp[14].i + xcx[31].i * csp[14].r ;
   f2 = xcx[23].r ; f4 = xcx[23].i ;
   xcx[31].r = f2-f1 ; xcx[31].i = f4-f3 ;
   xcx[23].r = f2+f1 ; xcx[23].i = f4+f3 ;

   f1 = xcx[16].r ; f3 = xcx[16].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[16].r = f2-f1 ; xcx[16].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[17].r * csp[16].r - xcx[17].i * csp[16].i ; /* twiddles */
   f3 = xcx[17].r * csp[16].i + xcx[17].i * csp[16].r ;
   f2 = xcx[1].r ; f4 = xcx[1].i ;
   xcx[17].r = f2-f1 ; xcx[17].i = f4-f3 ;
   xcx[1].r = f2+f1 ; xcx[1].i = f4+f3 ;

   f1 = xcx[18].r * csp[17].r - xcx[18].i * csp[17].i ; /* twiddles */
   f3 = xcx[18].r * csp[17].i + xcx[18].i * csp[17].r ;
   f2 = xcx[2].r ; f4 = xcx[2].i ;
   xcx[18].r = f2-f1 ; xcx[18].i = f4-f3 ;
   xcx[2].r = f2+f1 ; xcx[2].i = f4+f3 ;

   f1 = xcx[19].r * csp[18].r - xcx[19].i * csp[18].i ; /* twiddles */
   f3 = xcx[19].r * csp[18].i + xcx[19].i * csp[18].r ;
   f2 = xcx[3].r ; f4 = xcx[3].i ;
   xcx[19].r = f2-f1 ; xcx[19].i = f4-f3 ;
   xcx[3].r = f2+f1 ; xcx[3].i = f4+f3 ;

   f1 = xcx[20].r * csp[19].r - xcx[20].i * csp[19].i ; /* twiddles */
   f3 = xcx[20].r * csp[19].i + xcx[20].i * csp[19].r ;
   f2 = xcx[4].r ; f4 = xcx[4].i ;
   xcx[20].r = f2-f1 ; xcx[20].i = f4-f3 ;
   xcx[4].r = f2+f1 ; xcx[4].i = f4+f3 ;

   f1 = xcx[21].r * csp[20].r - xcx[21].i * csp[20].i ; /* twiddles */
   f3 = xcx[21].r * csp[20].i + xcx[21].i * csp[20].r ;
   f2 = xcx[5].r ; f4 = xcx[5].i ;
   xcx[21].r = f2-f1 ; xcx[21].i = f4-f3 ;
   xcx[5].r = f2+f1 ; xcx[5].i = f4+f3 ;

   f1 = xcx[22].r * csp[21].r - xcx[22].i * csp[21].i ; /* twiddles */
   f3 = xcx[22].r * csp[21].i + xcx[22].i * csp[21].r ;
   f2 = xcx[6].r ; f4 = xcx[6].i ;
   xcx[22].r = f2-f1 ; xcx[22].i = f4-f3 ;
   xcx[6].r = f2+f1 ; xcx[6].i = f4+f3 ;

   f1 = xcx[23].r * csp[22].r - xcx[23].i * csp[22].i ; /* twiddles */
   f3 = xcx[23].r * csp[22].i + xcx[23].i * csp[22].r ;
   f2 = xcx[7].r ; f4 = xcx[7].i ;
   xcx[23].r = f2-f1 ; xcx[23].i = f4-f3 ;
   xcx[7].r = f2+f1 ; xcx[7].i = f4+f3 ;

   f1 = - xcx[24].i * csp[23].i ; /* cos=0 twiddles */
   f3 = xcx[24].r * csp[23].i ;
   f2 = xcx[8].r ; f4 = xcx[8].i ;
   xcx[24].r = f2-f1 ; xcx[24].i = f4-f3 ;
   xcx[8].r = f2+f1 ; xcx[8].i = f4+f3 ;

   f1 = xcx[25].r * csp[24].r - xcx[25].i * csp[24].i ; /* twiddles */
   f3 = xcx[25].r * csp[24].i + xcx[25].i * csp[24].r ;
   f2 = xcx[9].r ; f4 = xcx[9].i ;
   xcx[25].r = f2-f1 ; xcx[25].i = f4-f3 ;
   xcx[9].r = f2+f1 ; xcx[9].i = f4+f3 ;

   f1 = xcx[26].r * csp[25].r - xcx[26].i * csp[25].i ; /* twiddles */
   f3 = xcx[26].r * csp[25].i + xcx[26].i * csp[25].r ;
   f2 = xcx[10].r ; f4 = xcx[10].i ;
   xcx[26].r = f2-f1 ; xcx[26].i = f4-f3 ;
   xcx[10].r = f2+f1 ; xcx[10].i = f4+f3 ;

   f1 = xcx[27].r * csp[26].r - xcx[27].i * csp[26].i ; /* twiddles */
   f3 = xcx[27].r * csp[26].i + xcx[27].i * csp[26].r ;
   f2 = xcx[11].r ; f4 = xcx[11].i ;
   xcx[27].r = f2-f1 ; xcx[27].i = f4-f3 ;
   xcx[11].r = f2+f1 ; xcx[11].i = f4+f3 ;

   f1 = xcx[28].r * csp[27].r - xcx[28].i * csp[27].i ; /* twiddles */
   f3 = xcx[28].r * csp[27].i + xcx[28].i * csp[27].r ;
   f2 = xcx[12].r ; f4 = xcx[12].i ;
   xcx[28].r = f2-f1 ; xcx[28].i = f4-f3 ;
   xcx[12].r = f2+f1 ; xcx[12].i = f4+f3 ;

   f1 = xcx[29].r * csp[28].r - xcx[29].i * csp[28].i ; /* twiddles */
   f3 = xcx[29].r * csp[28].i + xcx[29].i * csp[28].r ;
   f2 = xcx[13].r ; f4 = xcx[13].i ;
   xcx[29].r = f2-f1 ; xcx[29].i = f4-f3 ;
   xcx[13].r = f2+f1 ; xcx[13].i = f4+f3 ;

   f1 = xcx[30].r * csp[29].r - xcx[30].i * csp[29].i ; /* twiddles */
   f3 = xcx[30].r * csp[29].i + xcx[30].i * csp[29].r ;
   f2 = xcx[14].r ; f4 = xcx[14].i ;
   xcx[30].r = f2-f1 ; xcx[30].i = f4-f3 ;
   xcx[14].r = f2+f1 ; xcx[14].i = f4+f3 ;

   f1 = xcx[31].r * csp[30].r - xcx[31].i * csp[30].i ; /* twiddles */
   f3 = xcx[31].r * csp[30].i + xcx[31].i * csp[30].r ;
   f2 = xcx[15].r ; f4 = xcx[15].i ;
   xcx[31].r = f2-f1 ; xcx[31].i = f4-f3 ;
   xcx[15].r = f2+f1 ; xcx[15].i = f4+f3 ;

   return ;
}

/*-------- butterfly operations, for routines that follow -------*/

#define BFP(k) ( bkr = bb[k].r, bki = bb[k].i,                  \
                 tr  = cs[k].r, ti  = cs[k].i,                  \
                 t1  = tr*bkr - ti*bki , t2 = tr*bki + ti*bkr , \
                 akr = aa[k].r, aki = aa[k].i,                  \
                 xc[k   ].r = akr+t1, xc[k   ].i = aki+t2,      \
                 xc[k+32].r = akr-t1, xc[k+32].i = aki-t2 )

#define BFM(k) ( bkr = bb[k].r, bki = bb[k].i,                  \
                 tr  = cs[k].r, ti  = cs[k].i,                  \
                 t1  = tr*bkr + ti*bki , t2 = tr*bki - ti*bkr , \
                 akr = aa[k].r, aki = aa[k].i,                  \
                 xc[k   ].r = akr+t1, xc[k   ].i = aki+t2,      \
                 xc[k+32].r = akr-t1, xc[k+32].i = aki-t2 )


/*----------------------------------------------------------------
   Do a 64 FFT using fft32 and decimation-by-2
------------------------------------------------------------------*/

static void fft64( int mode , complex * xc )
{
   static complex cs[32] , aa[32] , bb[32] ;
   static int init=0 ;
   register int k , i ;
   register float akr,aki , bkr,bki , tr,ti , t1,t2 ;

   /*-- initialize cosine/sine table --*/

   if( !init ){
      double th = (PI/32.0) ;
      cs[0].r = 1.0 ; cs[0].i = 0.0 ; /* never used */
      for( k=1 ; k < 32 ; k++ ){ cs[k].r = cos(k*th); cs[k].i = sin(k*th); }
      init = 1 ;
   }

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0 ; k < 32 ; k++ ){ aa[k] = xc[i++] ; bb[k] = xc[i++] ; }

   fft32( mode , aa ) ; fft32( mode , bb ) ;

   /*-- recombine: 0 and Nyquist --*/

   xc[ 0].r = aa[0].r + bb[0].r ; xc[ 0].i = aa[0].i + bb[0].i ;
   xc[32].r = aa[0].r - bb[0].r ; xc[32].i = aa[0].i - bb[0].i ;

   /*-- recombine: all others --*/

   if( mode > 0 ){
#if 1
      for( k=1 ; k < 32 ; k++ ){
         bkr = bb[k].r; bki = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;

         t1  = tr*bkr - ti*bki ; t2 = tr*bki + ti*bkr ;

         akr = aa[k].r; aki = aa[k].i;

         xc[k   ].r = akr+t1; xc[k   ].i = aki+t2;
         xc[k+32].r = akr-t1; xc[k+32].i = aki-t2;
      }
#else
      BFP(1) ;
      for( k=2 ; k < 32 ; k+=2 ){ BFP(k) ; BFP(k+1) ; }  /* unrolled */
#endif
   } else {
#if 1
      for( k=1 ; k < 32 ; k++ ){
         bkr = bb[k].r; bki = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
                                                         /* only change from */
         t1  = tr*bkr + ti*bki ; t2 = tr*bki - ti*bkr ;  /* above is with ti */

         akr = aa[k].r; aki = aa[k].i;

         xc[k   ].r = akr+t1; xc[k   ].i = aki+t2;
         xc[k+32].r = akr-t1; xc[k+32].i = aki-t2;
      }
#else
      BFM(1) ;
      for( k=2 ; k < 32 ; k+=2 ){ BFM(k) ; BFM(k+1) ; }  /* unrolled */
#endif
   }

   return ;
}

/*----------------------------------------------------------------
   Do a 128 FFT using fft64 and decimation-by-2
------------------------------------------------------------------*/

static void fft128( int mode , complex * xc )
{
   static complex cs[64] , aa[64] , bb[64] ;
   static int init=0 ;
   register int k , i ;
   register float akr,aki , bkr,bki , tr,ti , t1,t2 ;

   /*-- initialize cosine/sine table --*/

   if( !init ){
      double th = (PI/64.0) ;
      cs[0].r = 1.0 ; cs[0].i = 0.0 ; /* never used */
      for( k=1 ; k < 64 ; k++ ){ cs[k].r = cos(k*th); cs[k].i = sin(k*th); }
      init = 1 ;
   }

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0 ; k < 64 ; k++ ){ aa[k] = xc[i++] ; bb[k] = xc[i++] ; }

   fft64( mode , aa ) ; fft64( mode , bb ) ;

   /*-- recombine: 0 and Nyquist --*/

   xc[ 0].r = aa[0].r + bb[0].r ; xc[ 0].i = aa[0].i + bb[0].i ;
   xc[64].r = aa[0].r - bb[0].r ; xc[64].i = aa[0].i - bb[0].i ;

   /*-- recombine: all others --*/

   if( mode > 0 ){
      for( k=1 ; k < 64 ; k++ ){
         bkr = bb[k].r; bki = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;

         t1  = tr*bkr - ti*bki ; t2 = tr*bki + ti*bkr ;

         akr = aa[k].r; aki = aa[k].i;

         xc[k   ].r = akr+t1; xc[k   ].i = aki+t2;
         xc[k+64].r = akr-t1; xc[k+64].i = aki-t2;
      }
   } else {
      for( k=1 ; k < 64 ; k++ ){
         bkr = bb[k].r; bki = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
                                                         /* only change from */
         t1  = tr*bkr + ti*bki ; t2 = tr*bki - ti*bkr ;  /* above is with ti */

         akr = aa[k].r; aki = aa[k].i;

         xc[k   ].r = akr+t1; xc[k   ].i = aki+t2;
         xc[k+64].r = akr-t1; xc[k+64].i = aki-t2;
      }
   }

   return ;
}

/*------------------------------------------------------------------
   Do a 256 FFT using fft128 and decimation-by-2
--------------------------------------------------------------------*/

static void fft256( int mode , complex * xc )
{
   static complex cs[128] , aa[128] , bb[128] ;
   static int init=0 ;
   register int k , i ;
   register float akr,aki , bkr,bki , tr,ti , t1,t2 ;

   /*-- initialize cosine/sine table --*/

   if( !init ){
      double th = (PI/128.0) ;
      cs[0].r = 1.0 ; cs[0].i = 0.0 ; /* never used */
      for( k=1 ; k < 128 ; k++ ){ cs[k].r = cos(k*th); cs[k].i = sin(k*th); }
      init = 1 ;
   }

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0 ; k < 128 ; k++ ){ aa[k] = xc[i++] ; bb[k] = xc[i++] ; }

   fft128( mode , aa ) ; fft128( mode , bb ) ;

   /*-- recombine: 0 and Nyquist --*/

   xc[  0].r = aa[0].r + bb[0].r ; xc[  0].i = aa[0].i + bb[0].i ;
   xc[128].r = aa[0].r - bb[0].r ; xc[128].i = aa[0].i - bb[0].i ;

   /*-- recombine: all others --*/

   if( mode > 0 ){
      for( k=1 ; k < 128 ; k++ ){
         bkr = bb[k].r; bki = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;

         t1  = tr*bkr - ti*bki ; t2 = tr*bki + ti*bkr ;

         akr = aa[k].r; aki = aa[k].i;

         xc[k    ].r = akr+t1; xc[k    ].i = aki+t2;
         xc[k+128].r = akr-t1; xc[k+128].i = aki-t2;
      }
   } else {
      for( k=1 ; k < 128 ; k++ ){
         bkr = bb[k].r; bki = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;

         t1  = tr*bkr + ti*bki ; t2 = tr*bki - ti*bkr ;

         akr = aa[k].r; aki = aa[k].i;

         xc[k    ].r = akr+t1; xc[k    ].i = aki+t2;
         xc[k+128].r = akr-t1; xc[k+128].i = aki-t2;
      }
   }

   return ;
}
#endif /* DONT_UNROLL_FFTS */
