/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/**** Complex in-place FFT functions.
      Prototypes below, after some inclusions.
      If USE_FFTN is enabled, any length is allowed;
      otherwise, limited to powers of 2 combined with
      powers of 3 and 5 (no more than 3^3 and 5^5, though).
      The original csfft_cox() functions are somehwat faster when
      applicable, and so are used in preference to the FFTN functions.
      However, the interface is still via csfft_cox(), described below. ****/

/*-------------------------------------------------------------*/
#undef STANDALONE  /* Define this if you want to use this code */
                   /* outside of the AFNI package (libmri.a).  */

#ifdef STANDALONE
#  include <stdlib.h>   /* include standard system headers */
#  include <stddef.h>
#  include <stdio.h>
#  include <math.h>
   typedef struct complex { float r,i ; } complex ;  /* need this! */
#  define EXIT exit
#else
#  include "mrilib.h"   /* AFNI package library header */
#endif  /* STANDALONE */
/*-------------------------------------------------------------*/

/* whether to use the fftn package, which allows for arbitrary lengths */

#define USE_FFTN
#ifdef USE_FFTN
#  define FFT_NODOUBLE    /* use only the float version */
#  include "fftn.c"
static int force_fftn=0 ;
static int internal_check=0 ; /* Jun 2018 */
#else
# define force_fftn 0
#endif

/*---- force the use of fftn even if csfft_cox is better;
       this ability is really here just for speed testing -----*/

void csfft_force_fftn( int fff )
{
#ifdef USE_FFTN
  force_fftn = fff ;
#endif
}

/***=====================================================================**
 *** Prototypes for externally callable functions:                       **
 ***    Complex-to-complex FFT in place:                                 **
 ***      mode = -1 or +1 (for inverse scaling, cf. csfft_scale_inverse  **
 ***      idim = dimension (power of 2, maybe with factors of 3 and 5)   **
 ***      xc   = input/output array                                      **
 ***    Re-initializes itself only when idim changes from previous call. **
 ***=====================================================================**/

void csfft_cox( int mode , int idim , complex *xc ) ;
int  csfft_nextup( int idim ) ;
int  csfft_nextup_one35( int idim ) ;
void csfft_scale_inverse( int scl ) ; /* scl=1 ==> force 1/N for mode=+1 **/
                                      /* scl=0 ==> no 1/N scaling here   **/

/*** Aug 1999:                                                           **
 ***   idim can now contain factors of 3 and/or 5, up to and including   **
 ***   3^3 * 5^3.  Some examples:                                        **
 ***       135 144 150 160 180 192 200 216 225 240 250                   **
 ***   The routine csfft_nextup(n) returns the smallest size FFT         **
 ***    >= n that csfft_cox() knows how to do.                           **
 ***   Note that efficiency of the different lengths can be quite        **
 ***   different.  In general, powers of 2 are fastest, but a            **
 ***   single factor of 3 and/or 5 doesn't cause too much slowdown.      **
 ***   The routine csfft_nextup_one35(n) returns the smallest size FFT   **
 ***    >= n that contains at most one power of 3, at most one power     **
 ***   of 5, and least one power of 2.  In trials, these are the most    **
 ***   time efficient.                                                   **/

/*** Oct 2017:
 ***   If fftn is enabled, idim can be arbitrary.       **
 ***   However, csfft_cox functions are used for their  **
 ***   special cases, since they are somewhat faster :) **/

/*-- Aug 1999: routines to do FFTs by decimation by 3 or 5 --*/

static void fft_3dec( int , int , complex * ) ;
static void fft_5dec( int , int , complex * ) ;

#undef  RMAX
#define RMAX 3   /* maximum levels of recursion for radix-3 and radix-5 */

#undef  PI
#define PI (3.141592653589793238462643)

/*-------------- To order the 1/N scaling on inversion: Aug 1999 ------------*/
/*-------------- Inversion is when mode == 1                     ------------*/

static int sclinv = 0 ;  /* set by csfft_scale_inverse */

void csfft_scale_inverse( int scl ){ sclinv = scl; return; }

/*-------------- For the unrolled FFT routines: November 1998 ---------------*/

#undef  DONT_UNROLL_FFTS
#ifndef DONT_UNROLL_FFTS
   static void fft8  ( int mode , complex *xc ) ; /* completely */
   static void fft16 ( int mode , complex *xc ) ; /* unrolled   */
   static void fft32 ( int mode , complex *xc ) ; /* routines   */

   static void fft64 ( int mode , complex *xc ) ; /* internal 2dec ->  32 */
   static void fft128( int mode , complex *xc ) ; /* internal 4dec ->  32 */
   static void fft256( int mode , complex *xc ) ; /* internal 4dec ->  64 */
   static void fft512( int mode , complex *xc ) ; /* internal 4dec -> 128 */

   static void fft_4dec( int , int , complex * ) ; /* general 4dec */

#  define fft1024(m,x) fft_4dec(m,1024,x)          /* 4dec -> 256 */
#  define fft2048(m,x) fft_4dec(m,2048,x)          /* 4dec -> 512 */

/*-- do FFTs of size 2 and 4 with macros --*/

#  define fft2(m,x) do{ float a,b,c,d ;                             \
                        a = x[0].r + x[1].r ; b = x[0].i + x[1].i ; \
                        c = x[0].r - x[1].r ; d = x[0].i - x[1].i ; \
                        x[0].r = a ; x[0].i = b ;                   \
                        x[1].r = c ; x[1].i = d ; } while(0)

# define USE_FFT4_MACRO
# ifndef USE_FFT4_MACRO
   static void fft4( int mode , complex *xc ) ; /* unrolled routine */
# else
#  define fft4(m,x) do{ float acpr,acmr,bdpr,bdmr, acpi,acmi,bdpi,bdmi; \
                        acpr = x[0].r + x[2].r; acmr = x[0].r - x[2].r; \
                        bdpr = x[1].r + x[3].r; bdmr = x[1].r - x[3].r; \
                        acpi = x[0].i + x[2].i; acmi = x[0].i - x[2].i; \
                        bdpi = x[1].i + x[3].i; bdmi = x[1].i - x[3].i; \
                        x[0].r = acpr+bdpr ; x[0].i = acpi+bdpi ;       \
                        x[2].r = acpr-bdpr ; x[2].i = acpi-bdpi ;       \
                        if(m > 0){                                      \
                          x[1].r = acmr-bdmi ; x[1].i = acmi+bdmr ;     \
                          x[3].r = acmr+bdmi ; x[3].i = acmi-bdmr ;     \
                        } else {                                        \
                          x[1].r = acmr+bdmi ; x[1].i = acmi-bdmr ;     \
                          x[3].r = acmr-bdmi ; x[3].i = acmi+bdmr ;     \
                        } } while(0)
# endif
#endif

/*----------------------------------------------------------------------
   Speedups with unrolled FFTs [program fftest.c]:
   Pentium II 400 MHz:  58% (32) to 36% (256)  [gcc -O3 -ffast-math]
   SGI R10000 175 MHz:  47% (32) to 18% (256)  [cc -Ofast]
   HP PA-8000 200 MHz:  58% (32) to 40% (256)  [cc +O3 +Oaggressive]
------------------------------------------------------------------------*/

/*----------------- the csfft trig constants tables ------------------*/

static complex *csplus = NULL , *csminus = NULL ;
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
         fprintf(stderr,"\n*** csfft cannot malloc space! ***\n"); EXIT(1) ;
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
     mode = -1 or +1 (1/idim scaling on inverse [+1] if sclinv
                      was set in the call to csfft_scale_inverse)
     idim = dimension (power of 2, possibly with a factor
                       of 3^p * 5^q also, for p,q=0..RMAX)
     xc   = input/output array
   Automagically re-initializes itself when idim changes from
   previous call.  By AJ (Andrzej Jesmanowicz), modified by RWCox.
----------------------------------------------------------------------*/

/*- Macro to do 1/N scaling on inverse:
      IF it is ordered by the user, and
      IF mode is positive, and
      IF this is not a recursive call.  -*/

#define SCLINV                                                 \
 if( sclinv && mode > 0 && rec == 0 ){                         \
   register int qq ; register float ff = 1.0 / (float) idim ;  \
   for( qq=0 ; qq < idim ; qq++ ){ xc[qq].r *= ff ; xc[qq].i *= ff ; } }

void csfft_cox( int mode , int idim , complex *xc )
{
   register unsigned int  m, n, i0, i1, i2, i3, k;
   register complex       *r0, *r1, *csp;
   register float         co, si, f0, f1, f2, f3, f4;

   static int rec=0 ;  /* recursion level */

   if( idim <= 1 ) return ;  /* stoopid inpoot */

   /*-- possibly use fftn instead of Cox/AJ funcs [Oct 2017] --*/

#ifdef USE_FFTN
   { static int last_idim=-1 , last_fftn=0 ;
     if( idim != last_idim ){
       internal_check = 1 ;
       m = csfft_nextup_even(idim) ; last_idim = idim ;
       internal_check = 0 ;
       last_fftn = (force_fftn || idim != m || idim > 32768 ) ;
#if 0
       INFO_message("csfft_cox(%d) %s replaced by fftn",
                    idim , (last_fftn) ? "IS" : "IS NOT" ) ;
#endif
     }
     if( last_fftn ){
       fftnf( idim, NULL, &(xc[0].r), &(xc[0].i), 2*mode, 0.0 ) ;
       SCLINV ; return ;
     }
   }
#endif

   /*-- November 1998: maybe use the unrolled FFT routines --*/

#ifndef DONT_UNROLL_FFTS
   switch( idim ){                                  /* none of these  */
      case    1:                           return;  /* routines will  */
      case    2: fft2   (mode,xc); SCLINV; return;  /* call csfft_cox */
      case    4: fft4   (mode,xc); SCLINV; return;  /* so don't need  */
      case    8: fft8   (mode,xc); SCLINV; return;  /* to do rec++/-- */
      case   16: fft16  (mode,xc); SCLINV; return;
      case   32: fft32  (mode,xc); SCLINV; return;
      case   64: fft64  (mode,xc); SCLINV; return;
      case  128: fft128 (mode,xc); SCLINV; return;
      case  256: fft256 (mode,xc); SCLINV; return;
      case  512: fft512 (mode,xc); SCLINV; return;
      case 1024: fft1024(mode,xc); SCLINV; return;
      case 2048: fft2048(mode,xc); SCLINV; return;

#if 1
      case  4096: fft_4dec(mode, 4096,xc); SCLINV; return; /* 4dec -> 1024 */
      case  8192: fft_4dec(mode, 8192,xc); SCLINV; return; /* 4dec -> 2048 */
      case 16384: fft_4dec(mode,16384,xc); SCLINV; return; /* 4dec -> 4096 */
      case 32768: fft_4dec(mode,32768,xc); SCLINV; return; /* 4dec -> 8192 */
#endif
   }
#endif  /* end of unrollificationizing */

   /*-- Aug 1999: deal with factors of 3 or 5 [might call csfft_cox] --*/

   if( idim%3 == 0 ){rec++; fft_3dec(mode,idim,xc); rec--; SCLINV; return;}
   if( idim%5 == 0 ){rec++; fft_5dec(mode,idim,xc); rec--; SCLINV; return;}

   /*-- below here: the old general power of 2 routine --*/

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

   SCLINV ; return ;
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

   if( idim % 3 == 0 ){                           /* Aug 1999 */
      for( m=0 ; m < nvec ; m++ )
         fft_3dec( mode , idim , xc + m*idim ) ;
      return ;
   } else if( idim % 5 == 0 ){
      for( m=0 ; m < nvec ; m++ )
         fft_5dec( mode , idim , xc + m*idim ) ;
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

#ifndef USE_FFT4_MACRO
/**************************************/
/* FFT routine unrolled of length   4 */

static void fft4( int mode , complex *xc )
{
   register complex *csp , *xcx=xc;
   register float f1,f2,f3,f4 ;

   /** perhaps initialize **/

   if( nold != 4 ) csfft_trigconsts( 4 ) ;

   csp = (mode > 0) ? csplus : csminus ;  /* choose const array */

   /** data swapping part **/

   f1 = xcx[1].r ; f2 = xcx[1].i ;
   xcx[1].r = xcx[2].r ; xcx[1].i = xcx[2].i ;
   xcx[2].r = f1 ; xcx[2].i = f2 ;

   /** butterflying part **/

   f1 = xcx[1].r ; f3 = xcx[1].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[1].r = f2-f1 ; xcx[1].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[3].r ; f3 = xcx[3].i ;  /* cos=1 sin=0 */
   f2 = xcx[2].r ; f4 = xcx[2].i ;
   xcx[3].r = f2-f1 ; xcx[3].i = f4-f3 ;
   xcx[2].r = f2+f1 ; xcx[2].i = f4+f3 ;

   f1 = xcx[2].r ; f3 = xcx[2].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[2].r = f2-f1 ; xcx[2].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = - xcx[3].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[3].r * csp[2].i ;
   f2 = xcx[1].r ; f4 = xcx[1].i ;
   xcx[3].r = f2-f1 ; xcx[3].i = f4-f3 ;
   xcx[1].r = f2+f1 ; xcx[1].i = f4+f3 ;

   return ;
}
#endif /* USE_FFT4_MACRO */

/**************************************/
/* FFT routine unrolled of length   8 */

static void fft8( int mode , complex *xc )
{
   register complex *csp , *xcx=xc;
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

static void fft16( int mode , complex *xc )
{
   register complex *csp , *xcx=xc;
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

static void fft32( int mode , complex *xc )
{
   register complex *csp , *xcx=xc;
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

/*----------------------------------------------------------------
   Do a 64 FFT using fft32 and decimation-by-2
   (complete unrolling wasn't as efficient)
------------------------------------------------------------------*/

static void fft64( int mode , complex *xc )
{
   static complex *cs=NULL , *aa , *bb ;
   register int k , i ;
   register float akr,aki , bkr,bki , tr,ti , t1,t2 ;

   /*-- initialize cosine/sine table --*/

   if( cs == NULL ){
      double th = (PI/32.0) ;
      cs = (complex *) malloc(sizeof(complex)*32) ;
      aa = (complex *) malloc(sizeof(complex)*32) ;
      bb = (complex *) malloc(sizeof(complex)*32) ;

      cs[0].r = 1.0 ; cs[0].i = 0.0 ; /* never used */
      for( k=1 ; k < 32 ; k++ ){ cs[k].r = cos(k*th); cs[k].i = sin(k*th); }
   }

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0 ; k < 32 ; k++ ){ aa[k] = xc[i++] ; bb[k] = xc[i++] ; }

   fft32( mode , aa ) ; fft32( mode , bb ) ;

   /*-- recombine: 0 and Nyquist --*/

   xc[ 0].r = aa[0].r + bb[0].r ; xc[ 0].i = aa[0].i + bb[0].i ;
   xc[32].r = aa[0].r - bb[0].r ; xc[32].i = aa[0].i - bb[0].i ;

   /*-- recombine: all others --*/

   if( mode > 0 ){
      for( k=1 ; k < 32 ; k++ ){
         bkr = bb[k].r; bki = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;

         t1  = tr*bkr - ti*bki ; t2 = tr*bki + ti*bkr ;

         akr = aa[k].r; aki = aa[k].i;

         xc[k   ].r = akr+t1; xc[k   ].i = aki+t2;
         xc[k+32].r = akr-t1; xc[k+32].i = aki-t2;
      }
   } else {
      for( k=1 ; k < 32 ; k++ ){
         bkr = bb[k].r; bki = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
                                                         /* only change from */
         t1  = tr*bkr + ti*bki ; t2 = tr*bki - ti*bkr ;  /* above is with ti */

         akr = aa[k].r; aki = aa[k].i;

         xc[k   ].r = akr+t1; xc[k   ].i = aki+t2;
         xc[k+32].r = akr-t1; xc[k+32].i = aki-t2;
      }
   }

   return ;
}

/*================================================================
  Decimation by 4 routines below from 12 Aug 1999 -- RWCox
  On a 400 MHz PII, about 8% faster than 2 decimation by 2 steps.
  Added fft512, fft1024, and fft2048 also, which are 50-80%
  faster than the unrolled csfft_cox() routine on the PII --
  somewhat less speedup on SGI R10K, but still worth something.
==================================================================*/

/*----------------------------------------------------------------
   Do a 128 FFT using fft32 and decimation by 4
------------------------------------------------------------------*/

#define N  128
#define M   32
#define M2  64
#define M3  96
static void fft128( int mode , complex *xc )
{
   static complex *cs=NULL , *aa , *bb , *cc , *dd ;
   register int k , i ;
   register float aar,aai, tr,ti, bbr,bbi, ccr,cci , ddr,ddi , t1,t2 ,
                  acpr,acmr , bdpr,bdmr , acpi,acmi , bdpi,bdmi ;

   /*-- initialize cosine/sine table and memory space --*/

   if( cs == NULL ){
      double th = (2.0*PI/N) ;
      cs = (complex *) malloc(sizeof(complex)*M3) ;
      aa = (complex *) malloc(sizeof(complex)*M ) ;
      bb = (complex *) malloc(sizeof(complex)*M ) ;
      cc = (complex *) malloc(sizeof(complex)*M ) ;
      dd = (complex *) malloc(sizeof(complex)*M ) ;

      cs[0].r = 1.0 ; cs[0].i = 0.0 ;
      for( k=1 ; k < M3 ; k++ ){ cs[k].r = cos(k*th); cs[k].i = sin(k*th); }
   }

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0; k < M; k++ ){
      aa[k]=xc[i++]; bb[k]=xc[i++]; cc[k]=xc[i++]; dd[k]=xc[i++];
   }

   fft32(mode,aa); fft32(mode,bb); fft32(mode,cc); fft32(mode,dd);

   /*-- recombination --*/

   if( mode > 0 ){
      for( k=0 ; k < M ; k++ ){
         t1  = bb[k].r; t2  = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
         bbr = tr*t1 - ti*t2  ; bbi = tr*t2 + ti*t1 ; /* b[k]*exp(i*2*Pi*k/N) */

         t1  = cc[  k].r; t2  = cc[  k].i;
         tr  = cs[2*k].r; ti  = cs[2*k].i;
         ccr = tr*t1 - ti*t2  ; cci = tr*t2 + ti*t1 ; /* c[k]*exp(i*4*Pi*k/N) */

         t1  = dd[  k].r; t2  = dd[  k].i;
         tr  = cs[3*k].r; ti  = cs[3*k].i;
         ddr = tr*t1 - ti*t2  ; ddi = tr*t2 + ti*t1 ; /* d[k]*exp(i*6*Pi*k/N) */

         aar = aa[k].r; aai = aa[k].i;                /* a[k] */

         acpr = aar + ccr ; acmr = aar - ccr ;
         bdpr = bbr + ddr ; bdmr = bbr - ddr ;
         acpi = aai + cci ; acmi = aai - cci ;
         bdpi = bbi + ddi ; bdmi = bbi - ddi ;

         xc[k   ].r = acpr+bdpr ; xc[k   ].i = acpi+bdpi ;
         xc[k+M ].r = acmr-bdmi ; xc[k+M ].i = acmi+bdmr ;
         xc[k+M2].r = acpr-bdpr ; xc[k+M2].i = acpi-bdpi ;
         xc[k+M3].r = acmr+bdmi ; xc[k+M3].i = acmi-bdmr ;
      }
   } else {
      for( k=0 ; k < M ; k++ ){
         t1  = bb[k].r; t2  = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
         bbr = tr*t1 + ti*t2  ; bbi = tr*t2 - ti*t1 ; /* b[k]*exp(-i*2*Pi*k/N) */

         t1  = cc[  k].r; t2  = cc[  k].i;
         tr  = cs[2*k].r; ti  = cs[2*k].i;
         ccr = tr*t1 + ti*t2  ; cci = tr*t2 - ti*t1 ; /* c[k]*exp(-i*4*Pi*k/N) */

         t1  = dd[  k].r; t2  = dd[  k].i;
         tr  = cs[3*k].r; ti  = cs[3*k].i;
         ddr = tr*t1 + ti*t2  ; ddi = tr*t2 - ti*t1 ; /* d[k]*exp(-i*6*Pi*k/N) */

         aar = aa[k].r; aai = aa[k].i;                 /* a[k] */

         acpr = aar + ccr ; acmr = aar - ccr ;
         bdpr = bbr + ddr ; bdmr = bbr - ddr ;
         acpi = aai + cci ; acmi = aai - cci ;
         bdpi = bbi + ddi ; bdmi = bbi - ddi ;

         xc[k   ].r = acpr+bdpr ; xc[k   ].i = acpi+bdpi ;
         xc[k+M ].r = acmr+bdmi ; xc[k+M ].i = acmi-bdmr ;
         xc[k+M2].r = acpr-bdpr ; xc[k+M2].i = acpi-bdpi ;
         xc[k+M3].r = acmr-bdmi ; xc[k+M3].i = acmi+bdmr ;
      }
   }

   return ;
}
#undef N
#undef M
#undef M2
#undef M3

/*------------------------------------------------------------------
   Do a 256 FFT using fft64 and decimation by 4
--------------------------------------------------------------------*/

#define N  256
#define M   64
#define M2 128
#define M3 192
static void fft256( int mode , complex *xc )
{
   static complex *cs=NULL , *aa , *bb , *cc , *dd ;
   register int k , i ;
   register float aar,aai, tr,ti, bbr,bbi, ccr,cci , ddr,ddi , t1,t2 ,
                  acpr,acmr , bdpr,bdmr , acpi,acmi , bdpi,bdmi ;

   /*-- initialize cosine/sine table and memory space --*/

   if( cs == NULL ){
      double th = (2.0*PI/N) ;
      cs = (complex *) malloc(sizeof(complex)*M3) ;
      aa = (complex *) malloc(sizeof(complex)*M ) ;
      bb = (complex *) malloc(sizeof(complex)*M ) ;
      cc = (complex *) malloc(sizeof(complex)*M ) ;
      dd = (complex *) malloc(sizeof(complex)*M ) ;

      cs[0].r = 1.0 ; cs[0].i = 0.0 ;
      for( k=1 ; k < M3 ; k++ ){ cs[k].r = cos(k*th); cs[k].i = sin(k*th); }
   }

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0; k < M; k++ ){
      aa[k]=xc[i++]; bb[k]=xc[i++]; cc[k]=xc[i++]; dd[k]=xc[i++];
   }

   fft64(mode,aa); fft64(mode,bb); fft64(mode,cc); fft64(mode,dd);

   /*-- recombination --*/

   if( mode > 0 ){
      for( k=0 ; k < M ; k++ ){
         t1  = bb[k].r; t2  = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
         bbr = tr*t1 - ti*t2  ; bbi = tr*t2 + ti*t1 ; /* b[k]*exp(i*2*Pi*k/N) */

         t1  = cc[  k].r; t2  = cc[  k].i;
         tr  = cs[2*k].r; ti  = cs[2*k].i;
         ccr = tr*t1 - ti*t2  ; cci = tr*t2 + ti*t1 ; /* c[k]*exp(i*4*Pi*k/N) */

         t1  = dd[  k].r; t2  = dd[  k].i;
         tr  = cs[3*k].r; ti  = cs[3*k].i;
         ddr = tr*t1 - ti*t2  ; ddi = tr*t2 + ti*t1 ; /* d[k]*exp(i*6*Pi*k/N) */

         aar = aa[k].r; aai = aa[k].i;                /* a[k] */

         acpr = aar + ccr ; acmr = aar - ccr ;
         bdpr = bbr + ddr ; bdmr = bbr - ddr ;
         acpi = aai + cci ; acmi = aai - cci ;
         bdpi = bbi + ddi ; bdmi = bbi - ddi ;

         xc[k   ].r = acpr+bdpr ; xc[k   ].i = acpi+bdpi ;
         xc[k+M ].r = acmr-bdmi ; xc[k+M ].i = acmi+bdmr ;
         xc[k+M2].r = acpr-bdpr ; xc[k+M2].i = acpi-bdpi ;
         xc[k+M3].r = acmr+bdmi ; xc[k+M3].i = acmi-bdmr ;
      }
   } else {
      for( k=0 ; k < M ; k++ ){
         t1  = bb[k].r; t2  = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
         bbr = tr*t1 + ti*t2  ; bbi = tr*t2 - ti*t1 ; /* b[k]*exp(-i*2*Pi*k/N) */

         t1  = cc[  k].r; t2  = cc[  k].i;
         tr  = cs[2*k].r; ti  = cs[2*k].i;
         ccr = tr*t1 + ti*t2  ; cci = tr*t2 - ti*t1 ; /* c[k]*exp(-i*4*Pi*k/N) */

         t1  = dd[  k].r; t2  = dd[  k].i;
         tr  = cs[3*k].r; ti  = cs[3*k].i;
         ddr = tr*t1 + ti*t2  ; ddi = tr*t2 - ti*t1 ; /* d[k]*exp(-i*6*Pi*k/N) */

         aar = aa[k].r; aai = aa[k].i;                /* a[k] */

         acpr = aar + ccr ; acmr = aar - ccr ;
         bdpr = bbr + ddr ; bdmr = bbr - ddr ;
         acpi = aai + cci ; acmi = aai - cci ;
         bdpi = bbi + ddi ; bdmi = bbi - ddi ;

         xc[k   ].r = acpr+bdpr ; xc[k   ].i = acpi+bdpi ;
         xc[k+M ].r = acmr+bdmi ; xc[k+M ].i = acmi-bdmr ;
         xc[k+M2].r = acpr-bdpr ; xc[k+M2].i = acpi-bdpi ;
         xc[k+M3].r = acmr-bdmi ; xc[k+M3].i = acmi+bdmr ;
      }
   }

   return ;
}
#undef N
#undef M
#undef M2
#undef M3

/*------------------------------------------------------------------
   Do a 512 FFT using fft128 and decimation by 4
--------------------------------------------------------------------*/

#define N  512
#define M  128
#define M2 256
#define M3 384
static void fft512( int mode , complex *xc )
{
   static complex *cs=NULL , *aa , *bb , *cc , *dd ;
   register int k , i ;
   register float aar,aai, tr,ti, bbr,bbi, ccr,cci , ddr,ddi , t1,t2 ,
                  acpr,acmr , bdpr,bdmr , acpi,acmi , bdpi,bdmi ;

   /*-- initialize cosine/sine table and memory space --*/

   if( cs == NULL ){
      double th = (2.0*PI/N) ;
      cs = (complex *) malloc(sizeof(complex)*M3) ;
      aa = (complex *) malloc(sizeof(complex)*M ) ;
      bb = (complex *) malloc(sizeof(complex)*M ) ;
      cc = (complex *) malloc(sizeof(complex)*M ) ;
      dd = (complex *) malloc(sizeof(complex)*M ) ;

      cs[0].r = 1.0 ; cs[0].i = 0.0 ;
      for( k=1 ; k < M3 ; k++ ){ cs[k].r = cos(k*th); cs[k].i = sin(k*th); }
   }

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0; k < M; k++ ){
      aa[k]=xc[i++]; bb[k]=xc[i++]; cc[k]=xc[i++]; dd[k]=xc[i++];
   }

   fft128(mode,aa); fft128(mode,bb); fft128(mode,cc); fft128(mode,dd);

   /*-- recombination --*/

   if( mode > 0 ){
      for( k=0 ; k < M ; k++ ){
         t1  = bb[k].r; t2  = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
         bbr = tr*t1 - ti*t2  ; bbi = tr*t2 + ti*t1  ;  /* b[k]*exp(+2*Pi*k/N) */

         t1  = cc[  k].r; t2  = cc[  k].i;
         tr  = cs[2*k].r; ti  = cs[2*k].i;
         ccr = tr*t1 - ti*t2  ; cci = tr*t2 + ti*t1  ;  /* c[k]*exp(+4*Pi*k/N) */

         t1  = dd[  k].r; t2  = dd[  k].i;
         tr  = cs[3*k].r; ti  = cs[3*k].i;
         ddr = tr*t1 - ti*t2  ; ddi = tr*t2 + ti*t1  ;  /* d[k]*exp(+6*Pi*k/N) */

         aar = aa[k].r; aai = aa[k].i;                  /* a[k] */

         acpr = aar + ccr ; acmr = aar - ccr ;
         bdpr = bbr + ddr ; bdmr = bbr - ddr ;
         acpi = aai + cci ; acmi = aai - cci ;
         bdpi = bbi + ddi ; bdmi = bbi - ddi ;

         xc[k   ].r = acpr+bdpr ; xc[k   ].i = acpi+bdpi ;
         xc[k+M ].r = acmr-bdmi ; xc[k+M ].i = acmi+bdmr ;
         xc[k+M2].r = acpr-bdpr ; xc[k+M2].i = acpi-bdpi ;
         xc[k+M3].r = acmr+bdmi ; xc[k+M3].i = acmi-bdmr ;
      }
   } else {
      for( k=0 ; k < M ; k++ ){
         t1  = bb[k].r; t2  = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
         bbr = tr*t1 + ti*t2  ; bbi = tr*t2 - ti*t1  ;  /* b[k]*exp(-2*Pi*k/N) */

         t1  = cc[  k].r; t2  = cc[  k].i;
         tr  = cs[2*k].r; ti  = cs[2*k].i;
         ccr = tr*t1 + ti*t2  ; cci = tr*t2 - ti*t1  ;  /* c[k]*exp(-4*Pi*k/N) */

         t1  = dd[  k].r; t2  = dd[  k].i;
         tr  = cs[3*k].r; ti  = cs[3*k].i;
         ddr = tr*t1 + ti*t2  ; ddi = tr*t2 - ti*t1  ;  /* d[k]*exp(-6*Pi*k/N) */

         aar = aa[k].r; aai = aa[k].i;                  /* a[k] */

         acpr = aar + ccr ; acmr = aar - ccr ;
         bdpr = bbr + ddr ; bdmr = bbr - ddr ;
         acpi = aai + cci ; acmi = aai - cci ;
         bdpi = bbi + ddi ; bdmi = bbi - ddi ;

         xc[k   ].r = acpr+bdpr ; xc[k   ].i = acpi+bdpi ;
         xc[k+M ].r = acmr+bdmi ; xc[k+M ].i = acmi-bdmr ;
         xc[k+M2].r = acpr-bdpr ; xc[k+M2].i = acpi-bdpi ;
         xc[k+M3].r = acmr-bdmi ; xc[k+M3].i = acmi+bdmr ;
      }
   }

   return ;
}
#undef N
#undef M
#undef M2
#undef M3

/*------------------------------------------------------------------
   fft_4dec: do a decimation by 4 for N=1024 and 2048.
   At most RMAX levels of recursion are allowed.
--------------------------------------------------------------------*/

static void fft_4dec( int mode , int idim , complex *xc )
{
   static int rec=0 ;
   static int rmold[RMAX] = {-1,-1,-1} ;
   static complex *rcs[RMAX] = {NULL,NULL,NULL} ;
   static complex *raa[RMAX], *rbb[RMAX], *rcc[RMAX] , *rdd[RMAX] ;

   int N=idim , M=idim/4 , M2=2*M , M3=3*M ;
   int mold=-1 ;
   complex *cs , *aa , *bb , *cc , *dd ;
   register int k , i ;
   register float aar,aai, tr,ti, bbr,bbi, ccr,cci , ddr,ddi , t1,t2 ,
                  acpr,acmr , bdpr,bdmr , acpi,acmi , bdpi,bdmi ;

   /*-- initialize cosine/sine table and memory space --*/

   if( rec >= RMAX ){
      fprintf(stderr,"\n*** fft_4dec: too many recursions!\n"); EXIT(1);
   }

   mold = rmold[rec] ;  /* what was done before at this recursion level */

   if( M != mold ){
      double th = (2.0*PI/N) ;
      if( M > mold ){
         if( rcs[rec] != NULL ){
            free(rcs[rec]);free(raa[rec]);
            free(rbb[rec]);free(rcc[rec]);free(rdd[rec]);
         }
         rcs[rec] = (complex *) malloc(sizeof(complex)*M3) ;
         raa[rec] = (complex *) malloc(sizeof(complex)*M ) ;
         rbb[rec] = (complex *) malloc(sizeof(complex)*M ) ;
         rcc[rec] = (complex *) malloc(sizeof(complex)*M ) ;
         rdd[rec] = (complex *) malloc(sizeof(complex)*M ) ;
      }

      rcs[rec][0].r = 1.0 ; rcs[rec][0].i = 0.0 ;
      for( k=1 ; k < M3 ; k++ ){
         rcs[rec][k].r = cos(k*th); rcs[rec][k].i = sin(k*th);
      }
      rmold[rec] = M ;
   }

   cs = rcs[rec]; aa = raa[rec]; bb = rbb[rec]; cc = rcc[rec]; dd = rdd[rec];

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0; k < M; k++ ){
      aa[k]=xc[i++]; bb[k]=xc[i++]; cc[k]=xc[i++]; dd[k]=xc[i++];
   }

   rec++ ;
   switch( N ){

      default: fprintf(stderr,"\n*** Illegal call to fft_4dec: N=%d\n",N);
               EXIT(1) ;

      case 1024: fft256(mode,aa); fft256(mode,bb);
                 fft256(mode,cc); fft256(mode,dd); break;

      case 2048: fft512(mode,aa); fft512(mode,bb);
                 fft512(mode,cc); fft512(mode,dd); break;

#if 1
      case 4096: fft_4dec(mode,1024,aa); fft_4dec(mode,1024,bb);         /* recurse once */
                 fft_4dec(mode,1024,cc); fft_4dec(mode,1024,dd); break ;

      case 8192: fft_4dec(mode,2048,aa); fft_4dec(mode,2048,bb);         /* recurse once */
                 fft_4dec(mode,2048,cc); fft_4dec(mode,2048,dd); break ;

      case 16384: fft_4dec(mode,4096,aa); fft_4dec(mode,4096,bb);        /* recurse twice */
                  fft_4dec(mode,4096,cc); fft_4dec(mode,4096,dd); break ;

      case 32768: fft_4dec(mode,8192,aa); fft_4dec(mode,8192,bb);        /* recurse twice */
                  fft_4dec(mode,8192,cc); fft_4dec(mode,8192,dd); break ;
#endif
   }
   rec-- ;

   /*-- recombination --*/

   if( mode > 0 ){
      for( k=0 ; k < M ; k++ ){
         t1  = bb[k].r; t2  = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
         bbr = tr*t1 - ti*t2  ; bbi = tr*t2 + ti*t1  ;  /* b[k]*exp(+2*Pi*k/N) */

         t1  = cc[  k].r; t2  = cc[  k].i;
         tr  = cs[2*k].r; ti  = cs[2*k].i;
         ccr = tr*t1 - ti*t2  ; cci = tr*t2 + ti*t1  ;  /* c[k]*exp(+4*Pi*k/N) */

         t1  = dd[  k].r; t2  = dd[  k].i;
         tr  = cs[3*k].r; ti  = cs[3*k].i;
         ddr = tr*t1 - ti*t2  ; ddi = tr*t2 + ti*t1  ;  /* d[k]*exp(+6*Pi*k/N) */

         aar = aa[k].r; aai = aa[k].i;                  /* a[k] */

         acpr = aar + ccr ; acmr = aar - ccr ;
         bdpr = bbr + ddr ; bdmr = bbr - ddr ;
         acpi = aai + cci ; acmi = aai - cci ;
         bdpi = bbi + ddi ; bdmi = bbi - ddi ;

         xc[k   ].r = acpr+bdpr ; xc[k   ].i = acpi+bdpi ;
         xc[k+M ].r = acmr-bdmi ; xc[k+M ].i = acmi+bdmr ;
         xc[k+M2].r = acpr-bdpr ; xc[k+M2].i = acpi-bdpi ;
         xc[k+M3].r = acmr+bdmi ; xc[k+M3].i = acmi-bdmr ;
      }
   } else {
      for( k=0 ; k < M ; k++ ){
         t1  = bb[k].r; t2  = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
         bbr = tr*t1 + ti*t2  ; bbi = tr*t2 - ti*t1  ;  /* b[k]*exp(-2*Pi*k/N) */

         t1  = cc[  k].r; t2  = cc[  k].i;
         tr  = cs[2*k].r; ti  = cs[2*k].i;
         ccr = tr*t1 + ti*t2  ; cci = tr*t2 - ti*t1  ;  /* c[k]*exp(-4*Pi*k/N) */

         t1  = dd[  k].r; t2  = dd[  k].i;
         tr  = cs[3*k].r; ti  = cs[3*k].i;
         ddr = tr*t1 + ti*t2  ; ddi = tr*t2 - ti*t1  ;  /* d[k]*exp(-6*Pi*k/N) */

         aar = aa[k].r; aai = aa[k].i;                  /* a[k] */

         acpr = aar + ccr ; acmr = aar - ccr ;
         bdpr = bbr + ddr ; bdmr = bbr - ddr ;
         acpi = aai + cci ; acmi = aai - cci ;
         bdpi = bbi + ddi ; bdmi = bbi - ddi ;

         xc[k   ].r = acpr+bdpr ; xc[k   ].i = acpi+bdpi ;
         xc[k+M ].r = acmr+bdmi ; xc[k+M ].i = acmi-bdmr ;
         xc[k+M2].r = acpr-bdpr ; xc[k+M2].i = acpi-bdpi ;
         xc[k+M3].r = acmr-bdmi ; xc[k+M3].i = acmi+bdmr ;
      }
   }

   return ;
}
#endif /* DONT_UNROLL_FFTS */

/*=======================================================================
  The following radix-3 and radix-5 routines are by RWCox -- Aug 1999.
  They are not inside unrolled code.
=========================================================================*/

/*------------------------------------------------------------------
   fft_3dec: do a decimation-by-3, plus a power-of-2.
   At most RMAX levels of recursion are allowed, which means
   that at most 3**RMAX can be a factor of idim.
--------------------------------------------------------------------*/

#undef  CC3
#undef  SS3
#define CC3 (-0.5)         /* cos(2*Pi/3) */
#define SS3 (0.8660254038) /* sin(2*Pi/3) */

static void fft_3dec( int mode , int idim , complex *xc )
{
   static int rec=0 ;
   static int rmold[RMAX] = {-1,-1,-1} ;
   static complex *rcs[RMAX] = {NULL,NULL,NULL} ;
   static complex *raa[RMAX], *rbb[RMAX], *rcc[RMAX] ;

   int N=idim , M=idim/3 , M2=2*M ;
   int mold ;
   complex *cs=NULL , *aa , *bb , *cc ;
   register int k , i ;
   register float aar,aai, tr,ti, bbr,bbi, ccr,cci,
                  t1,t2,t4,t5,t6,t8 ;

   /*-- initialize cosine/sine table and memory space --*/

   if( rec >= RMAX ){
      fprintf(stderr,"\n*** fft_3dec: too many recursions!\n"); EXIT(1);
   }

   mold = rmold[rec] ;  /* what was done before at this recursion level */

   if( M != mold ){
      double th = (2.0*PI/N) ;
      if( M > mold ){
         if( rcs[rec] != NULL ){
            free(rcs[rec]);free(raa[rec]);free(rbb[rec]);free(rcc[rec]);
         }
         rcs[rec] = (complex *) malloc(sizeof(complex)*M2) ;
         raa[rec] = (complex *) malloc(sizeof(complex)*M ) ;
         rbb[rec] = (complex *) malloc(sizeof(complex)*M ) ;
         rcc[rec] = (complex *) malloc(sizeof(complex)*M ) ;
      }

      rcs[rec][0].r = 1.0 ; rcs[rec][0].i = 0.0 ;
      for( k=1 ; k < M2 ; k++ ){
         rcs[rec][k].r = cos(k*th); rcs[rec][k].i = sin(k*th);
      }
      rmold[rec] = M ;
   }

   cs = rcs[rec] ; aa = raa[rec] ; bb = rbb[rec] ; cc = rcc[rec] ;

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0; k < M; k++ ){ aa[k]=xc[i++]; bb[k]=xc[i++]; cc[k]=xc[i++]; }

#ifndef DONT_UNROLL_FFTS
   switch( M ){
      case   1: break ;

      case   2: fft2  (mode,aa) ; fft2  (mode,bb) ; fft2  (mode,cc) ; break;
      case   4: fft4  (mode,aa) ; fft4  (mode,bb) ; fft4  (mode,cc) ; break;
      case   8: fft8  (mode,aa) ; fft8  (mode,bb) ; fft8  (mode,cc) ; break;
      case  16: fft16 (mode,aa) ; fft16 (mode,bb) ; fft16 (mode,cc) ; break;
      case  32: fft32 (mode,aa) ; fft32 (mode,bb) ; fft32 (mode,cc) ; break;
      case  64: fft64 (mode,aa) ; fft64 (mode,bb) ; fft64 (mode,cc) ; break;
      case 128: fft128(mode,aa) ; fft128(mode,bb) ; fft128(mode,cc) ; break;
      case 256: fft256(mode,aa) ; fft256(mode,bb) ; fft256(mode,cc) ; break;
      case 512: fft512(mode,aa) ; fft512(mode,bb) ; fft512(mode,cc) ; break;

      case 1024: fft1024(mode,aa) ; fft1024(mode,bb) ; fft1024(mode,cc) ; break;
      case 2048: fft2048(mode,aa) ; fft2048(mode,bb) ; fft2048(mode,cc) ; break;

      default:  rec++ ;
                csfft_cox(mode,M,aa) ;
                csfft_cox(mode,M,bb) ; csfft_cox(mode,M,cc) ;
                rec-- ; break ;
   }
#else
      rec++ ;
      csfft_cox(mode,M,aa) ; csfft_cox(mode,M,bb) ; csfft_cox(mode,M,cc) ;
      rec-- ;
#endif

   /*-- recombination --*/

   if( mode > 0 ){
      for( k=0 ; k < M ; k++ ){
         t1  = bb[k].r; t2  = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
         bbr = tr*t1  - ti*t2  ; bbi = tr*t2  + ti*t1 ; /* b[k]*exp(+2*Pi*k/N) */

         t1  = cc[  k].r; t2  = cc[  k].i;
         tr  = cs[2*k].r; ti  = cs[2*k].i;
         ccr = tr*t1  - ti*t2  ; cci = tr*t2  + ti*t1 ; /* c[k]*exp(+4*Pi*k/N) */

         t4 = bbr+ccr      ; t1 = t4*CC3       ;
         t8 = bbi+cci      ; t6 = t8*CC3       ;
         t5 = (bbr-ccr)*SS3; t2 = (bbi-cci)*SS3;

         aar = aa[k].r; aai = aa[k].i;                  /* a[k] */

         xc[k   ].r = aar+t4      ; xc[k   ].i = aai+t8      ;
         xc[k+M ].r = (aar+t1)-t2 ; xc[k+M ].i = (aai+t6)+t5 ;
         xc[k+M2].r = (aar+t1)+t2 ; xc[k+M2].i = (aai+t6)-t5 ;
      }
   } else {
      for( k=0 ; k < M ; k++ ){
         t1  = bb[k].r; t2  = bb[k].i;
         tr  = cs[k].r; ti  = cs[k].i;
         bbr = tr*t1  + ti*t2  ; bbi = tr*t2  - ti*t1 ; /* b[k]*exp(-2*Pi*k/N) */

         t1  = cc[  k].r; t2  = cc[  k].i;
         tr  = cs[2*k].r; ti  = cs[2*k].i;
         ccr = tr*t1  + ti*t2  ; cci = tr*t2  - ti*t1 ; /* c[k]*exp(-4*Pi*k/N) */

         t4 = bbr+ccr      ; t1 = t4*CC3       ;
         t8 = bbi+cci      ; t6 = t8*CC3       ;
         t5 = (bbr-ccr)*SS3; t2 = (bbi-cci)*SS3;

         aar = aa[k].r; aai = aa[k].i;                  /* a[k] */

         xc[k   ].r = aar+t4      ; xc[k   ].i = aai+t8      ;
         xc[k+M ].r = (aar+t1)+t2 ; xc[k+M ].i = (aai+t6)-t5 ;
         xc[k+M2].r = (aar+t1)-t2 ; xc[k+M2].i = (aai+t6)+t5 ;
      }
   }

   return ;
}

/*------------------------------------------------------------------
   fft_5dec: do a decimation-by-5, plus a power-of-2.
   At most RMAX levels of recursion are allowed, which means
   that at most 5**RMAX can be a factor of idim.
--------------------------------------------------------------------*/

#undef  COS72
#undef  SIN72
#define COS72  0.30901699   /* cos(72 deg) */
#define SIN72  0.95105652   /* sin(72 deg) */

static void fft_5dec( int mode , int idim , complex *xc )
{
   static int rec=0 ;
   static int rmold[RMAX] = {-1,-1,-1} ;
   static complex *rcs[RMAX] = {NULL,NULL,NULL} ;
   static complex *raa[RMAX], *rbb[RMAX], *rcc[RMAX], *rdd[RMAX], *ree[RMAX] ;

   int N=idim , M=idim/5 , M2=2*M , M3=3*M , M4=4*M ;
   int mold ;
   complex *cs, *aa, *bb, *cc, *dd, *ee ;
   register int k , i ;
   register float aar,aai,bbr,bbi,ccr,cci,ddr,ddi,eer,eei ;
   register float akp,akm,bkp,bkm , ajp,ajm,bjp,bjm ;
   register float ak,bk,aj,bj ;
   float c72 , s72 , c2,s2 ;
   int ss ;

   /*-- initialize cosine/sine table and memory space --*/

   if( rec >= RMAX ){
      fprintf(stderr,"\n*** fft_5dec: too many recursions!\n"); EXIT(1);
   }

   mold = rmold[rec] ;  /* what was done before at this recursion level */

   if( M != mold ){     /* new value of M? */
      double th = (2.0*PI/N) ;
      if( M > mold ){             /* need more space */
         if( rcs[rec] != NULL ){
            free(rcs[rec]);free(raa[rec]);free(rbb[rec]);
            free(rcc[rec]);free(rdd[rec]);free(ree[rec]);
         }
         rcs[rec] = (complex *) malloc(sizeof(complex)*M4) ;
         raa[rec] = (complex *) malloc(sizeof(complex)*M ) ;
         rbb[rec] = (complex *) malloc(sizeof(complex)*M ) ;
         rcc[rec] = (complex *) malloc(sizeof(complex)*M ) ;
         rdd[rec] = (complex *) malloc(sizeof(complex)*M ) ;
         ree[rec] = (complex *) malloc(sizeof(complex)*M ) ;
      }

      rcs[rec][0].r = 1.0 ; rcs[rec][0].i = 0.0 ;
      for( k=1 ; k < M4 ; k++ ){
         rcs[rec][k].r = cos(k*th); rcs[rec][k].i = sin(k*th);
      }
      rmold[rec] = M ;  /* you must remember this */
   }

   cs = rcs[rec] ; aa = raa[rec] ; bb = rbb[rec] ;
                   cc = rcc[rec] ; dd = rdd[rec] ; ee = ree[rec] ;

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0; k < M; k++ ){
      aa[k]=xc[i++]; bb[k]=xc[i++]; cc[k]=xc[i++]; dd[k]=xc[i++]; ee[k]=xc[i++];
   }

#ifndef DONT_UNROLL_FFTS
   switch( M ){
      case   1: break ;

      case   2: fft2  (mode,aa) ; fft2  (mode,bb) ; fft2  (mode,cc) ;
                fft2  (mode,dd) ; fft2  (mode,ee) ;                   break;

      case   4: fft4  (mode,aa) ; fft4  (mode,bb) ; fft4  (mode,cc) ;
                fft4  (mode,dd) ; fft4  (mode,ee) ;                   break;

      case   8: fft8  (mode,aa) ; fft8  (mode,bb) ; fft8  (mode,cc) ;
                fft8  (mode,dd) ; fft8  (mode,ee) ;                   break;

      case  16: fft16 (mode,aa) ; fft16 (mode,bb) ; fft16 (mode,cc) ;
                fft16 (mode,dd) ; fft16 (mode,ee) ;                   break;

      case  32: fft32 (mode,aa) ; fft32 (mode,bb) ; fft32 (mode,cc) ;
                fft32 (mode,dd) ; fft32 (mode,ee) ;                   break;

      case  64: fft64 (mode,aa) ; fft64 (mode,bb) ; fft64 (mode,cc) ;
                fft64 (mode,dd) ; fft64 (mode,ee) ;                   break;

      case 128: fft128(mode,aa) ; fft128(mode,bb) ; fft128(mode,cc) ;
                fft128(mode,dd) ; fft128(mode,ee) ;                   break;

      case 256: fft256(mode,aa) ; fft256(mode,bb) ; fft256(mode,cc) ;
                fft256(mode,dd) ; fft256(mode,ee) ;                   break;

      case 512: fft512(mode,aa) ; fft512(mode,bb) ; fft512(mode,cc) ;
                fft512(mode,dd) ; fft512(mode,ee) ;                   break;

      case 1024: fft1024(mode,aa) ; fft1024(mode,bb) ; fft1024(mode,cc) ;
                 fft1024(mode,dd) ; fft1024(mode,ee) ;                   break;

      case 2048: fft2048(mode,aa) ; fft2048(mode,bb) ; fft2048(mode,cc) ;
                 fft2048(mode,dd) ; fft2048(mode,ee) ;                   break;

      default:  rec++ ;
                csfft_cox(mode,M,aa) ;
                csfft_cox(mode,M,bb) ; csfft_cox(mode,M,cc) ;
                csfft_cox(mode,M,dd) ; csfft_cox(mode,M,ee) ;
                rec-- ; break ;
   }
#else
      rec++ ;
      csfft_cox(mode,M,aa) ; csfft_cox(mode,M,bb) ; csfft_cox(mode,M,cc) ;
      csfft_cox(mode,M,dd) ; csfft_cox(mode,M,ee) ;
      rec-- ;
#endif

   /*-- recombination --*/

   if( mode > 0 ){ s72 =  SIN72 ; ss =  1 ; }
   else          { s72 = -SIN72 ; ss = -1 ; }

   c72 = COS72 ;
   c2  = c72 * c72 - s72 * s72 ;
   s2  = 2.0 * c72 * s72 ;

   for( k=0 ; k < M ; k++ ){
      ak  = bb[k].r ; bk  = bb[k].i      ;
      aj  = cs[k].r ; bj  = cs[k].i * ss ;
      bbr = aj*ak - bj*bk ; bbi = aj*bk + bj*ak  ;  /* b[k]*exp(ss*2*Pi*k/N) */

      ak  = cc[  k].r ; bk  = cc[  k].i      ;
      aj  = cs[2*k].r ; bj  = cs[2*k].i * ss ;
      ccr = aj*ak - bj*bk ; cci = aj*bk + bj*ak  ;  /* c[k]*exp(ss*4*Pi*k/N) */

      ak  = dd[  k].r ; bk  = dd[  k].i      ;
      aj  = cs[3*k].r ; bj  = cs[3*k].i * ss ;
      ddr = aj*ak - bj*bk ; ddi = aj*bk + bj*ak  ;  /* d[k]*exp(ss*6*Pi*k/N) */

      ak  = ee[  k].r ; bk  = ee[  k].i      ;
      aj  = cs[4*k].r ; bj  = cs[4*k].i * ss ;
      eer = aj*ak - bj*bk ; eei = aj*bk + bj*ak  ;  /* e[k]*exp(ss*8*Pi*k/N) */

      aar = aa[k].r ; aai = aa[k].i ;               /* a[k] */

      /* the code below is (heavily) adapted from fftn.c */

      akp = bbr + eer ; akm = bbr - eer ;
      bkp = bbi + eei ; bkm = bbi - eei ;
      ajp = ccr + ddr ; ajm = ccr - ddr ;
      bjp = cci + ddi ; bjm = cci - ddi ;

      xc[k].r = aar + akp + ajp ; xc[k].i = aai + bkp + bjp ;

      ak = akp * c72 + ajp * c2 + aar ; bk = bkp * c72 + bjp * c2 + aai ;
      aj = akm * s72 + ajm * s2       ; bj = bkm * s72 + bjm * s2       ;

      xc[k+M ].r = ak - bj ; xc[k+M ].i = bk + aj ;
      xc[k+M4].r = ak + bj ; xc[k+M4].i = bk - aj ;

      ak = akp * c2 + ajp * c72 + aar ; bk = bkp * c2 + bjp * c72 + aai ;
      aj = akm * s2 - ajm * s72       ; bj = bkm * s2 - bjm * s72       ;

      xc[k+M2].r = ak - bj ; xc[k+M2].i = bk + aj ;
      xc[k+M3].r = ak + bj ; xc[k+M3].i = bk - aj ;
   }

   return ;
}

/*--------------------------------------------------------------------
   Return the smallest integer >= idim for which we can do an FFT.
----------------------------------------------------------------------*/

#define N35 ((RMAX+1)*(RMAX+1))

int csfft_nextup( int idim )
{
   static int *tf = NULL , *dn = NULL ;
   int ibase , ii ;

   /*-- build table of allowable powers of 3 and 5 [tf],
        the powers of 2 just less than them        [dn],
        and their ratios tf/dn                     [rat].
        Then sort tf and dn to be increasing in rat.     --*/

   if( tf == NULL ){
      int p , q , tt,ff , i=0 , j ; float *rat ; float r ;

      tf  = (int *)   malloc(sizeof(int)  *N35) ;
      dn  = (int *)   malloc(sizeof(int)  *N35) ;
      rat = (float *) malloc(sizeof(float)*N35) ;

      /* create tables */

      for( p=0,tt=1 ; p <= RMAX ; p++,tt*=3 ){         /* tt = 3^p */
         for( q=0,ff=1 ; q <= RMAX ; q++,ff*=5,i++ ){  /* ff = 5^q */
            tf[i] = tt * ff ;

            j=2; while( j < tf[i] ){ j*=2; } /* j = power of 2 just >= tf */
            dn[i] = j/2 ;                    /* dn = power of 2 just < tf */

            rat[i] = (float)(tf[i]) / (float)(dn[i]) ;
         }
      }

      /* sort on rat (crude, but it works) */

      do{
         for( i=1,p=0 ; i < N35 ; i++ ){
            if( rat[i-1] > rat[i] ){
               r = rat[i-1] ; rat[i-1] = rat[i] ; rat[i] = r ;
               q = tf [i-1] ; tf [i-1] = tf [i] ; tf [i] = q ;
               q = dn [i-1] ; dn [i-1] = dn [i] ; dn [i] = q ;
               p++ ;
            }
         }
      } while( p > 0 ) ;

      free(rat) ;
   }

   /*-- loop on powers of 2 (ibase);
        we can do FFTs of size = tf*ibase/dn (note 1 < tf/dn < 2);
        sinc tf/dn is sorted, we're scanning in increasing sizes  --*/

   ibase = 1 ;
   while(1){
      if( idim <= ibase ) return ibase ;

      for( ii=0 ; ii < N35 ; ii++ )
         if( dn[ii] <= ibase && idim <= tf[ii]*ibase/dn[ii] )
            return tf[ii]*ibase/dn[ii] ;

      ibase *= 2 ;
   }
}

/*----------------------------------------------------------------------
   return the next legal value that has only 1 power of 3 and/or 5,
   and that is also even
------------------------------------------------------------------------*/

int csfft_nextup_one35( int idim )
{
   int jj = idim ;
   do{
      jj = csfft_nextup(jj) ;
      if( jj%9 == 0 || jj%25 == 0 || jj%2 == 1 ) jj++ ;
      else                                       return jj ;
   } while(1) ;
   return 0 ; /* cannot be reached */
}

/*----------------------------------------------------------------------
   return the next legal value that is even [13 May 2003 - RWCox]
------------------------------------------------------------------------*/

int csfft_nextup_even( int idim )
{
   int jj = idim ;
#ifdef USE_FFTN             /* Jun 2018 */
   if( !internal_check ){
     if( jj%2 == 1 ) jj++ ;
     return jj ;
   }
#endif
   do{
      jj = csfft_nextup(jj) ;
      if( jj%2 == 1 ) jj++ ;
      else            return jj ;
   } while(1) ;
   return 0 ; /* cannot be reached */
}

/*------------------------------------------------------------------------*/

int csfft_allows_anything(void)             /* Jun 2018 */
{
#ifdef USE_FFTN
   return 1 ;
#else
   return 0 ;
#endif
}
