/*****************************************************************************
  The functions in this file are adaptations of csfft.c that should be
  thread-safe for use with OpenMP.  As such, this file is intended to be
  #include-d into a source file that uses OpenMP directives.  In such a case,
  the following inclusions are not really necessary, but included here for
  reference.

  When using these functions, csfft_cox_OMP_SETUP() must be called once
  first, in order to setup some thread specific static storage.  These
  variable names all start with 'CFO_'.
                                                          -- RWCox -- Dec 2015
******************************************************************************/

#include "mrilib.h"   /* AFNI package library header */
#ifdef USE_OMP
# include <omp.h>
#endif

/***=====================================================================**
 *** Prototypes for externally callable functions:                       **
 ***    Complex-to-complex FFT in place:                                 **
 ***      mode = -1 or +1 (for inverse scaling, csfft_scale_inverse_OMP  **
 ***      idim = dimension (power of 2, maybe with factors of 3 and 5)   **
 ***      xc   = input/output array                                      **
 ***    Re-initializes itself only when idim changes from previous call. **
 ***=====================================================================**/

void csfft_cox_OMP( int mode , int idim , complex *xc ) ;
void csfft_scale_inverse_OMP( int scl ) ; /* scl=1 ==> force 1/N for mode=+1 **/
                                          /* scl=0 ==> no 1/N scaling        **/

/*** Aug 1999:                                                           **
 ***   idim can now contain factors of 3 and/or 5, up to and including   **
 ***   3^3 * 5^3.  Some examples:                                        **
 ***       135 144 150 160 180 192 200 216 225 240 250                   **
 ***   The routine csfft_nextup(n) returns the smallest size FFT         **
 ***    >= n that csfft_cox_OMP() knows how to do.                        **
 ***   Note that efficiency of the different lengths can be quite        **
 ***   different.  In general, powers of 2 are fastest, but a            **
 ***   single factor of 3 and/or 5 doesn't cause too much slowdown.      **
 ***   The routine csfft_nextup_one35(n) returns the smallest size FFT   **
 ***    >= n that contains at most one power of 3, at most one power     **
 ***   of 5, and least one power of 2.  In trials, these are the most    **
 ***   time efficient.                                                   **/

/*-- Aug 1999: internal routines to do FFTs by decimation by 3, 4, 5 --*/

static void fft_3dec_OMP( int , int , complex * ) ;
static void fft_4dec_OMP( int , int , complex * ) ;
static void fft_5dec_OMP( int , int , complex * ) ;

#undef  RMAX
#define RMAX 7   /* maximum levels of recursion for decimation by 3, 4, 5 */

#undef  PI
#define PI (3.141592653589793238462643)  /* that should be good enough */

/*---------------------------------------------------------------------------*/
/* When using these OpenMP-ized functions,
   csfft_cox_OMP_SETUP() must be called once first!                [Dec 2015]
*//*-------------------------------------------------------------------------*/

static int       CFO_nthr   = 1   ;  /* max number of threads to allow for */
static complex **CFO_csplus =NULL ;  /* per-thread arrays of trig consts */
static complex **CFO_csminus=NULL ;
static int      *CFO_nold   =NULL ;  /* per-thread last dimension used */
static int      *CFO_sclinv =NULL ;  /* per-thread inverse scaling flag */
static int      *CFO_recC   =NULL ;  /* per-thread recursion level for csfft_cox_OMP */
static int      *CFO_recX   =NULL ;  /* per-thread recursion level for decimaion funcs */

static int *CFO_rmold3[RMAX] ;       /* per-thread, per-recursion last M value used */
static int *CFO_rmold4[RMAX] ;       /* for decimation by 3, 4, 5 functions */
static int *CFO_rmold5[RMAX] ;

static int     *nCFO_aa[RMAX] ;      /* per-thread, per-recursion temp arrays */
static int     *nCFO_bb[RMAX] ;      /* nCFO_aa[r][i] is the size of the 'aa' */
static int     *nCFO_cc[RMAX] ;      /*   array for recursion level r, thread i */
static int     *nCFO_dd[RMAX] ;      /* CFO_aa[r][i] is the corresponding array */
static int     *nCFO_ee[RMAX] ;
static int     *nCFO_cs[RMAX] ;
static complex **CFO_aa[RMAX] ;
static complex **CFO_bb[RMAX] ;
static complex **CFO_cc[RMAX] ;
static complex **CFO_dd[RMAX] ;
static complex **CFO_ee[RMAX] ;
static complex **CFO_cs[RMAX] ;

/* macro to declare and set the ithr = thread number (local variable) */

#undef DECLARE_ithr
#ifdef USE_OMP
# define DECLARE_ithr int ithr=omp_get_thread_num()
#else
# define DECLARE_ithr int ithr=0
#endif

/*----------------------------------------------------------------------------*/
/* Allocate the CFO arrays, just one time. */

void csfft_cox_OMP_SETUP(void)
{
   int rr ;

   if( CFO_nold != NULL ) return ;  /* was called before! */

#ifdef USE_OMP
   CFO_nthr = omp_get_max_threads() ;
#endif

   /* arrays that don't depend on recursion level, just thread number */

   CFO_nold    = (int *)     calloc(sizeof(int)      ,CFO_nthr) ;
   CFO_sclinv  = (int *)     calloc(sizeof(int)      ,CFO_nthr) ;
   CFO_recC    = (int *)     calloc(sizeof(int)      ,CFO_nthr) ;
   CFO_recX    = (int *)     calloc(sizeof(int)      ,CFO_nthr) ;
   CFO_csplus  = (complex **)calloc(sizeof(complex *),CFO_nthr) ;
   CFO_csminus = (complex **)calloc(sizeof(complex *),CFO_nthr) ;

   /* arrays that depend on recursion level and on thread number;
      note that we are just allocating pointer space now; the
      actual arrays are allocated by the CFO_assign macro (infra) */

   for( rr=0 ; rr < RMAX ; rr++ ){
     nCFO_aa[rr]  = (int *)     calloc(sizeof(int)      ,CFO_nthr) ;
     nCFO_bb[rr]  = (int *)     calloc(sizeof(int)      ,CFO_nthr) ;
     nCFO_cc[rr]  = (int *)     calloc(sizeof(int)      ,CFO_nthr) ;
     nCFO_dd[rr]  = (int *)     calloc(sizeof(int)      ,CFO_nthr) ;
     nCFO_ee[rr]  = (int *)     calloc(sizeof(int)      ,CFO_nthr) ;
     nCFO_cs[rr]  = (int *)     calloc(sizeof(int)      ,CFO_nthr) ;
      CFO_aa[rr]  = (complex **)calloc(sizeof(complex *),CFO_nthr) ;
      CFO_bb[rr]  = (complex **)calloc(sizeof(complex *),CFO_nthr) ;
      CFO_cc[rr]  = (complex **)calloc(sizeof(complex *),CFO_nthr) ;
      CFO_dd[rr]  = (complex **)calloc(sizeof(complex *),CFO_nthr) ;
      CFO_ee[rr]  = (complex **)calloc(sizeof(complex *),CFO_nthr) ;
      CFO_cs[rr]  = (complex **)calloc(sizeof(complex *),CFO_nthr) ;

      CFO_rmold3[rr] = (int *)  calloc(sizeof(int)      ,CFO_nthr) ;
      CFO_rmold4[rr] = (int *)  calloc(sizeof(int)      ,CFO_nthr) ;
      CFO_rmold5[rr] = (int *)  calloc(sizeof(int)      ,CFO_nthr) ;
   }

   return ;
}

/** this macro assigns a workspace of length ll (complex), at
    recursion level rec (0..RMAX-1), to pointer named 'ab', from
    static array CFO_'ab'[rr][ithr], in thread number ithr.
    ithr and rec are local to the function whence this is called!
    The name 'ab' can be one of 'aa' 'bb' 'cc' 'dd' 'ee' 'cs'.
    The C preprocessor token catenation operator ## is used here. */

#undef  CFO_assign
#define CFO_assign(ab,ll)                                                \
   do{ if( nCFO_##ab[rec][ithr] < (ll) ){                                \
         if( CFO_##ab[rec][ithr] != NULL ) free(CFO_##ab[rec][ithr]) ;   \
         CFO_##ab[rec][ithr] = (complex *)malloc(sizeof(complex)*(ll)) ; \
         nCFO_##ab[rec][ithr] = (ll) ;                                   \
       }                                                                 \
       ab = CFO_##ab[rec][ithr] ;                                        \
   } while(0)

/*-------------- To order the 1/N scaling on inversion: Aug 1999 ------------*/

void csfft_scale_inverse_OMP( int scl ){
  DECLARE_ithr ; CFO_sclinv[ithr] = scl; return;
}

/*-------------- For the unrolled FFT routines: November 1998 --------------*/

static void fft8 ( int mode , complex *xc ) ; /* completely */
static void fft16( int mode , complex *xc ) ; /* unrolled   */
static void fft32( int mode , complex *xc ) ; /* routines   */
static void fft64( int mode , complex *xc ) ;

/* convenience macros for decimation by 4 */

#undef  DONT_USE_4DEC
#ifndef DONT_USE_4DEC
#define fft128(m,x)   fft_4dec_OMP(m,  128,x)  /* via fft32   (non-recursive) */
#define fft256(m,x)   fft_4dec_OMP(m,  256,x)  /* via fft64   (non-recursive) */
#define fft512(m,x)   fft_4dec_OMP(m,  512,x)  /* via fft128  (recursive X 1) */
#define fft1024(m,x)  fft_4dec_OMP(m, 1024,x)  /* via fft256  (recursive X 1) */
#if 1
#define fft2048(m,x)  fft_4dec_OMP(m, 2048,x)  /* via fft512  (recursive X 2) */
#define fft4096(m,x)  fft_4dec_OMP(m, 4096,x)  /* via fft1024 (recursive X 2) */
#define fft8192(m,x)  fft_4dec_OMP(m, 8192,x)  /* via fft2048 (recursive X 3) */
#define fft16384(m,x) fft_4dec_OMP(m,16384,x)  /* via fft4096 (recursive X 3) */
#define fft32768(m,x) fft_4dec_OMP(m,32768,x)  /* via fft8192 (recursive X 4) */
#define fft65536(m,x) fft_4dec_OMP(m,65536,x)  /* via fft16384 (recursive X 4) */
#endif
#else
#define fft128(m,x)   csfft_cox_OMP(m,  128,x)
#define fft256(m,x)   csfft_cox_OMP(m,  256,x)
#define fft512(m,x)   csfft_cox_OMP(m,  512,x)
#define fft1024(m,x)  csfft_cox_OMP(m, 1024,x)
#if 1
#define fft2048(m,x)  csfft_cox_OMP(m, 2048,x)
#define fft4096(m,x)  csfft_cox_OMP(m, 4096,x)
#define fft8192(m,x)  csfft_cox_OMP(m, 8192,x)
#define fft16384(m,x) csfft_cox_OMP(m,16384,x)
#define fft32768(m,x) csfft_cox_OMP(m,32768,x)
#define fft65536(m,x) csfft_cox_OMP(m,65536,x)
#endif
#endif

/*-- do FFTs of size 2 and 4 with macros, not functions --*/

#define fft2(m,x) do{ float a,b,c,d ;                             \
                      a = x[0].r + x[1].r ; b = x[0].i + x[1].i ; \
                      c = x[0].r - x[1].r ; d = x[0].i - x[1].i ; \
                      x[0].r = a ; x[0].i = b ;                   \
                      x[1].r = c ; x[1].i = d ; } while(0)

#define fft4(m,x) do{ float acpr,acmr,bdpr,bdmr, acpi,acmi,bdpi,bdmi; \
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

/*----------------------------------------------------------------------
   Speedups with unrolled FFTs [program fftest.c]:
   Pentium II 400 MHz:  58% (32) to 36% (256)  [gcc -O3 -ffast-math]
   SGI R10000 175 MHz:  47% (32) to 18% (256)  [cc -Ofast]
   HP PA-8000 200 MHz:  58% (32) to 40% (256)  [cc +O3 +Oaggressive]
------------------------------------------------------------------------*/

/*--------------------------------------------------------------------
   Initialize csfft trig constants table.  Adapted from AJ's code.
   Does both mode=1 and mode=-1 tables.  (mode=+1 == inversion)
----------------------------------------------------------------------*/

static void csfft_trigconsts( int idim )  /* internal function */
{
   DECLARE_ithr ;
   register unsigned int  m, n, i0, i1, i2, i3, k;
   register complex       *r0, *csp;
   register float         co, si, f0, f1, f2, f3, f4;
   double                 al;
   complex *my_csp , *my_csm ;

   if( idim == CFO_nold[ithr] ) return ;

 { if( idim > CFO_nold[ithr] ){
     if( CFO_csplus[ithr] != NULL ){ free(CFO_csplus[ithr]) ; free(CFO_csminus[ithr]) ; }
      CFO_csplus[ithr]  = (complex *) malloc( sizeof(complex) * idim ) ;
      CFO_csminus[ithr] = (complex *) malloc( sizeof(complex) * idim ) ;
      if( CFO_csplus[ithr] == NULL || CFO_csminus[ithr] == NULL ){
        fprintf(stderr,"\n*** csfft_cox_OMP cannot malloc space idim=%d! ***\n",idim); EXIT(1) ;
      }
  }}

   CFO_nold[ithr] = n = idim ;

   my_csp = CFO_csplus[ithr] ; my_csm = CFO_csminus[ithr] ;

   f1 = 1.0 ;  /* csplus init */
   m  = 1; k  = 0;
   while (n > m) {
      i3 = m << 1; f2 = m; al = f1*PI/f2;
      co = cos(al); si = sin(al);
      (my_csp + k)->r = 1.; (my_csp + k)->i = 0.;
      for (i0=0; i0 < m; i0++) {
         k++;
         csp = my_csp + k; r0 = csp - 1;
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
      (my_csm + k)->r = 1.; (my_csm + k)->i = 0.;
      for (i0=0; i0 < m; i0++) {
         k++;
         csp = my_csm + k; r0  = csp - 1;
         csp->r = r0->r * co - r0->i * si;
         csp->i = r0->i * co + r0->r * si;
      }
      m = i3;
   }
   return ;
}

/*--------------------------------------------------------------------
   Complex-to-complex FFT in place:
     mode = -1 or +1 (1/idim scaling on inverse [+1] if CFO_sclinv
                      was set in the call to csfft_scale_inverse_OMP)
     idim = dimension (power of 2, possibly with a factor
                       of 3^p * 5^q also, for p,q=0..RMAX)
     xc   = input/output array
   Automagically re-initializes itself when idim changes from
   previous call.  By AJ (Andrzej Jesmanowicz), modified by RWCox.
----------------------------------------------------------------------*/

/*- Macro to do 1/N scaling on inverse:
      if it is ordered by the user, and
      if mode is positive, and
      if this is not a recursive call.  -*/

#define SCLINV                                                 \
 if( CFO_sclinv[ithr] && mode > 0 && rec == 0 ){               \
   register int qq ; register float ff = 1.0 / (float) idim ;  \
   for( qq=0 ; qq < idim ; qq++ ){ xc[qq].r *= ff ; xc[qq].i *= ff ; } }

void csfft_cox_OMP( int mode , int idim , complex *xc )
{
   DECLARE_ithr ;
   register unsigned int  m, n, i0, i1, i2, i3, k;
   register complex       *r0, *r1, *csp;
   register float         co, si, f0, f1, f2, f3, f4;
   int rec = CFO_recC[ithr] ;

   if( idim <= 1 ) return ;  /* stoopid inpoot */

   /*-- November 1998: maybe use the unrolled FFT routines --*/

/* INFO_message("csfft_cox_OMP(%d)",idim) ; */

   switch( idim ){                                   /* none of these  */
     case     1:                            return;  /* routines will  */
     case     2: fft2    (mode,xc); SCLINV; return;  /* call csfft_cox */
     case     4: fft4    (mode,xc); SCLINV; return;  /* so don't need  */
     case     8: fft8    (mode,xc); SCLINV; return;  /* to do rec++/-- */
     case    16: fft16   (mode,xc); SCLINV; return;
     case    32: fft32   (mode,xc); SCLINV; return;
     case    64: fft64   (mode,xc); SCLINV; return;
#ifndef DONT_USE_4DEC
     case   128: fft128  (mode,xc); SCLINV; return;
     case   256: fft256  (mode,xc); SCLINV; return;
     case   512: fft512  (mode,xc); SCLINV; return;
     case  1024: fft1024 (mode,xc); SCLINV; return;
#if 1
     case  2048: fft2048 (mode,xc); SCLINV; return;
     case  4096: fft4096 (mode,xc); SCLINV; return;
     case  8192: fft8192 (mode,xc); SCLINV; return;
     case 16384: fft16384(mode,xc); SCLINV; return;
     case 32768: fft32768(mode,xc); SCLINV; return;
     case 65536: fft65536(mode,xc); SCLINV; return;
#endif
#endif
   }

   /* fall thru to here ==> not one of the shorter 2^p cases above! */

   /*-- Aug 1999: deal with factors of 3 or 5 [might call csfft_cox_OMP again] --*/

   if( idim%3 == 0 ){CFO_recC[ithr]++; fft_3dec_OMP(mode,idim,xc); CFO_recC[ithr]--; SCLINV; return;}
   if( idim%5 == 0 ){CFO_recC[ithr]++; fft_5dec_OMP(mode,idim,xc); CFO_recC[ithr]--; SCLINV; return;}

   /*-- below here: the old general power of 2 routine (i.e., for idim > 65536) --*/

   /**-- perhaps initialize trig consts (for this thread) --**/

   if( CFO_nold[ithr] != idim ) csfft_trigconsts( idim ) ;

   /** Main loop starts here **/

   n   = idim;
   i2  = idim >> 1;
   i1  = 0;
   csp = (mode > 0) ? CFO_csplus[ithr] : CFO_csminus[ithr] ;  /* choose const array */

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

/******************************************************************************
  Routines below for unrolled FFT routines; generated with program fftprint.c.
*******************************************************************************/

/**************************************/
/* FFT routine unrolled of length   8 */

static void fft8( int mode , complex *xc )
{
   DECLARE_ithr ;
   register complex *csp , *xcx=xc;
   register float f1,f2,f3,f4 ;

/* ININFO_message("fft8") ; */

   /** perhaps initialize **/

   if( CFO_nold[ithr] != 8 ) csfft_trigconsts( 8 ) ;

   csp = (mode > 0) ? CFO_csplus[ithr] : CFO_csminus[ithr] ;  /* choose const array */

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
   DECLARE_ithr ;
   register complex *csp , *xcx=xc;
   register float f1,f2,f3,f4 ;

/* ININFO_message("fft16") ; */

   /** perhaps initialize **/

   if( CFO_nold[ithr] != 16 ) csfft_trigconsts( 16 ) ;

   csp = (mode > 0) ? CFO_csplus[ithr] : CFO_csminus[ithr] ;  /* choose const array */

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
   DECLARE_ithr ;
   register complex *csp , *xcx=xc;
   register float f1,f2,f3,f4 ;

/* ININFO_message("fft32") ; */

   /** perhaps initialize **/

   if( CFO_nold[ithr] != 32 ) csfft_trigconsts( 32 ) ;

   csp = (mode > 0) ? CFO_csplus[ithr] : CFO_csminus[ithr] ;  /* choose const array */

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

/**************************************/
/* FFT routine unrolled of length  64 */

void fft64( int mode , complex * xc )
{
   DECLARE_ithr ;
   register complex *csp , *xcx=xc;
   register float f1,f2,f3,f4 ;

/* ININFO_message("fft64") ; */

   /** perhaps initialize **/

   if( CFO_nold[ithr] != 64 ) csfft_trigconsts( 64 ) ;

   csp = (mode > 0) ? CFO_csplus[ithr] : CFO_csminus[ithr] ;  /* choose const array */

   /** data swapping part **/

   f1 = xcx[1].r ; f2 = xcx[1].i ;
   xcx[1].r = xcx[32].r ; xcx[1].i = xcx[32].i ;
   xcx[32].r = f1 ; xcx[32].i = f2 ;

   f1 = xcx[2].r ; f2 = xcx[2].i ;
   xcx[2].r = xcx[16].r ; xcx[2].i = xcx[16].i ;
   xcx[16].r = f1 ; xcx[16].i = f2 ;

   f1 = xcx[3].r ; f2 = xcx[3].i ;
   xcx[3].r = xcx[48].r ; xcx[3].i = xcx[48].i ;
   xcx[48].r = f1 ; xcx[48].i = f2 ;

   f1 = xcx[4].r ; f2 = xcx[4].i ;
   xcx[4].r = xcx[8].r ; xcx[4].i = xcx[8].i ;
   xcx[8].r = f1 ; xcx[8].i = f2 ;

   f1 = xcx[5].r ; f2 = xcx[5].i ;
   xcx[5].r = xcx[40].r ; xcx[5].i = xcx[40].i ;
   xcx[40].r = f1 ; xcx[40].i = f2 ;

   f1 = xcx[6].r ; f2 = xcx[6].i ;
   xcx[6].r = xcx[24].r ; xcx[6].i = xcx[24].i ;
   xcx[24].r = f1 ; xcx[24].i = f2 ;

   f1 = xcx[7].r ; f2 = xcx[7].i ;
   xcx[7].r = xcx[56].r ; xcx[7].i = xcx[56].i ;
   xcx[56].r = f1 ; xcx[56].i = f2 ;

   f1 = xcx[9].r ; f2 = xcx[9].i ;
   xcx[9].r = xcx[36].r ; xcx[9].i = xcx[36].i ;
   xcx[36].r = f1 ; xcx[36].i = f2 ;

   f1 = xcx[10].r ; f2 = xcx[10].i ;
   xcx[10].r = xcx[20].r ; xcx[10].i = xcx[20].i ;
   xcx[20].r = f1 ; xcx[20].i = f2 ;

   f1 = xcx[11].r ; f2 = xcx[11].i ;
   xcx[11].r = xcx[52].r ; xcx[11].i = xcx[52].i ;
   xcx[52].r = f1 ; xcx[52].i = f2 ;

   f1 = xcx[13].r ; f2 = xcx[13].i ;
   xcx[13].r = xcx[44].r ; xcx[13].i = xcx[44].i ;
   xcx[44].r = f1 ; xcx[44].i = f2 ;

   f1 = xcx[14].r ; f2 = xcx[14].i ;
   xcx[14].r = xcx[28].r ; xcx[14].i = xcx[28].i ;
   xcx[28].r = f1 ; xcx[28].i = f2 ;

   f1 = xcx[15].r ; f2 = xcx[15].i ;
   xcx[15].r = xcx[60].r ; xcx[15].i = xcx[60].i ;
   xcx[60].r = f1 ; xcx[60].i = f2 ;

   f1 = xcx[17].r ; f2 = xcx[17].i ;
   xcx[17].r = xcx[34].r ; xcx[17].i = xcx[34].i ;
   xcx[34].r = f1 ; xcx[34].i = f2 ;

   f1 = xcx[19].r ; f2 = xcx[19].i ;
   xcx[19].r = xcx[50].r ; xcx[19].i = xcx[50].i ;
   xcx[50].r = f1 ; xcx[50].i = f2 ;

   f1 = xcx[21].r ; f2 = xcx[21].i ;
   xcx[21].r = xcx[42].r ; xcx[21].i = xcx[42].i ;
   xcx[42].r = f1 ; xcx[42].i = f2 ;

   f1 = xcx[22].r ; f2 = xcx[22].i ;
   xcx[22].r = xcx[26].r ; xcx[22].i = xcx[26].i ;
   xcx[26].r = f1 ; xcx[26].i = f2 ;

   f1 = xcx[23].r ; f2 = xcx[23].i ;
   xcx[23].r = xcx[58].r ; xcx[23].i = xcx[58].i ;
   xcx[58].r = f1 ; xcx[58].i = f2 ;

   f1 = xcx[25].r ; f2 = xcx[25].i ;
   xcx[25].r = xcx[38].r ; xcx[25].i = xcx[38].i ;
   xcx[38].r = f1 ; xcx[38].i = f2 ;

   f1 = xcx[27].r ; f2 = xcx[27].i ;
   xcx[27].r = xcx[54].r ; xcx[27].i = xcx[54].i ;
   xcx[54].r = f1 ; xcx[54].i = f2 ;

   f1 = xcx[29].r ; f2 = xcx[29].i ;
   xcx[29].r = xcx[46].r ; xcx[29].i = xcx[46].i ;
   xcx[46].r = f1 ; xcx[46].i = f2 ;

   f1 = xcx[31].r ; f2 = xcx[31].i ;
   xcx[31].r = xcx[62].r ; xcx[31].i = xcx[62].i ;
   xcx[62].r = f1 ; xcx[62].i = f2 ;

   f1 = xcx[35].r ; f2 = xcx[35].i ;
   xcx[35].r = xcx[49].r ; xcx[35].i = xcx[49].i ;
   xcx[49].r = f1 ; xcx[49].i = f2 ;

   f1 = xcx[37].r ; f2 = xcx[37].i ;
   xcx[37].r = xcx[41].r ; xcx[37].i = xcx[41].i ;
   xcx[41].r = f1 ; xcx[41].i = f2 ;

   f1 = xcx[39].r ; f2 = xcx[39].i ;
   xcx[39].r = xcx[57].r ; xcx[39].i = xcx[57].i ;
   xcx[57].r = f1 ; xcx[57].i = f2 ;

   f1 = xcx[43].r ; f2 = xcx[43].i ;
   xcx[43].r = xcx[53].r ; xcx[43].i = xcx[53].i ;
   xcx[53].r = f1 ; xcx[53].i = f2 ;

   f1 = xcx[47].r ; f2 = xcx[47].i ;
   xcx[47].r = xcx[61].r ; xcx[47].i = xcx[61].i ;
   xcx[61].r = f1 ; xcx[61].i = f2 ;

   f1 = xcx[55].r ; f2 = xcx[55].i ;
   xcx[55].r = xcx[59].r ; xcx[55].i = xcx[59].i ;
   xcx[59].r = f1 ; xcx[59].i = f2 ;

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

   f1 = xcx[33].r ; f3 = xcx[33].i ;  /* cos=1 sin=0 */
   f2 = xcx[32].r ; f4 = xcx[32].i ;
   xcx[33].r = f2-f1 ; xcx[33].i = f4-f3 ;
   xcx[32].r = f2+f1 ; xcx[32].i = f4+f3 ;

   f1 = xcx[35].r ; f3 = xcx[35].i ;  /* cos=1 sin=0 */
   f2 = xcx[34].r ; f4 = xcx[34].i ;
   xcx[35].r = f2-f1 ; xcx[35].i = f4-f3 ;
   xcx[34].r = f2+f1 ; xcx[34].i = f4+f3 ;

   f1 = xcx[37].r ; f3 = xcx[37].i ;  /* cos=1 sin=0 */
   f2 = xcx[36].r ; f4 = xcx[36].i ;
   xcx[37].r = f2-f1 ; xcx[37].i = f4-f3 ;
   xcx[36].r = f2+f1 ; xcx[36].i = f4+f3 ;

   f1 = xcx[39].r ; f3 = xcx[39].i ;  /* cos=1 sin=0 */
   f2 = xcx[38].r ; f4 = xcx[38].i ;
   xcx[39].r = f2-f1 ; xcx[39].i = f4-f3 ;
   xcx[38].r = f2+f1 ; xcx[38].i = f4+f3 ;

   f1 = xcx[41].r ; f3 = xcx[41].i ;  /* cos=1 sin=0 */
   f2 = xcx[40].r ; f4 = xcx[40].i ;
   xcx[41].r = f2-f1 ; xcx[41].i = f4-f3 ;
   xcx[40].r = f2+f1 ; xcx[40].i = f4+f3 ;

   f1 = xcx[43].r ; f3 = xcx[43].i ;  /* cos=1 sin=0 */
   f2 = xcx[42].r ; f4 = xcx[42].i ;
   xcx[43].r = f2-f1 ; xcx[43].i = f4-f3 ;
   xcx[42].r = f2+f1 ; xcx[42].i = f4+f3 ;

   f1 = xcx[45].r ; f3 = xcx[45].i ;  /* cos=1 sin=0 */
   f2 = xcx[44].r ; f4 = xcx[44].i ;
   xcx[45].r = f2-f1 ; xcx[45].i = f4-f3 ;
   xcx[44].r = f2+f1 ; xcx[44].i = f4+f3 ;

   f1 = xcx[47].r ; f3 = xcx[47].i ;  /* cos=1 sin=0 */
   f2 = xcx[46].r ; f4 = xcx[46].i ;
   xcx[47].r = f2-f1 ; xcx[47].i = f4-f3 ;
   xcx[46].r = f2+f1 ; xcx[46].i = f4+f3 ;

   f1 = xcx[49].r ; f3 = xcx[49].i ;  /* cos=1 sin=0 */
   f2 = xcx[48].r ; f4 = xcx[48].i ;
   xcx[49].r = f2-f1 ; xcx[49].i = f4-f3 ;
   xcx[48].r = f2+f1 ; xcx[48].i = f4+f3 ;

   f1 = xcx[51].r ; f3 = xcx[51].i ;  /* cos=1 sin=0 */
   f2 = xcx[50].r ; f4 = xcx[50].i ;
   xcx[51].r = f2-f1 ; xcx[51].i = f4-f3 ;
   xcx[50].r = f2+f1 ; xcx[50].i = f4+f3 ;

   f1 = xcx[53].r ; f3 = xcx[53].i ;  /* cos=1 sin=0 */
   f2 = xcx[52].r ; f4 = xcx[52].i ;
   xcx[53].r = f2-f1 ; xcx[53].i = f4-f3 ;
   xcx[52].r = f2+f1 ; xcx[52].i = f4+f3 ;

   f1 = xcx[55].r ; f3 = xcx[55].i ;  /* cos=1 sin=0 */
   f2 = xcx[54].r ; f4 = xcx[54].i ;
   xcx[55].r = f2-f1 ; xcx[55].i = f4-f3 ;
   xcx[54].r = f2+f1 ; xcx[54].i = f4+f3 ;

   f1 = xcx[57].r ; f3 = xcx[57].i ;  /* cos=1 sin=0 */
   f2 = xcx[56].r ; f4 = xcx[56].i ;
   xcx[57].r = f2-f1 ; xcx[57].i = f4-f3 ;
   xcx[56].r = f2+f1 ; xcx[56].i = f4+f3 ;

   f1 = xcx[59].r ; f3 = xcx[59].i ;  /* cos=1 sin=0 */
   f2 = xcx[58].r ; f4 = xcx[58].i ;
   xcx[59].r = f2-f1 ; xcx[59].i = f4-f3 ;
   xcx[58].r = f2+f1 ; xcx[58].i = f4+f3 ;

   f1 = xcx[61].r ; f3 = xcx[61].i ;  /* cos=1 sin=0 */
   f2 = xcx[60].r ; f4 = xcx[60].i ;
   xcx[61].r = f2-f1 ; xcx[61].i = f4-f3 ;
   xcx[60].r = f2+f1 ; xcx[60].i = f4+f3 ;

   f1 = xcx[63].r ; f3 = xcx[63].i ;  /* cos=1 sin=0 */
   f2 = xcx[62].r ; f4 = xcx[62].i ;
   xcx[63].r = f2-f1 ; xcx[63].i = f4-f3 ;
   xcx[62].r = f2+f1 ; xcx[62].i = f4+f3 ;

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

   f1 = xcx[34].r ; f3 = xcx[34].i ;  /* cos=1 sin=0 */
   f2 = xcx[32].r ; f4 = xcx[32].i ;
   xcx[34].r = f2-f1 ; xcx[34].i = f4-f3 ;
   xcx[32].r = f2+f1 ; xcx[32].i = f4+f3 ;

   f1 = xcx[38].r ; f3 = xcx[38].i ;  /* cos=1 sin=0 */
   f2 = xcx[36].r ; f4 = xcx[36].i ;
   xcx[38].r = f2-f1 ; xcx[38].i = f4-f3 ;
   xcx[36].r = f2+f1 ; xcx[36].i = f4+f3 ;

   f1 = xcx[42].r ; f3 = xcx[42].i ;  /* cos=1 sin=0 */
   f2 = xcx[40].r ; f4 = xcx[40].i ;
   xcx[42].r = f2-f1 ; xcx[42].i = f4-f3 ;
   xcx[40].r = f2+f1 ; xcx[40].i = f4+f3 ;

   f1 = xcx[46].r ; f3 = xcx[46].i ;  /* cos=1 sin=0 */
   f2 = xcx[44].r ; f4 = xcx[44].i ;
   xcx[46].r = f2-f1 ; xcx[46].i = f4-f3 ;
   xcx[44].r = f2+f1 ; xcx[44].i = f4+f3 ;

   f1 = xcx[50].r ; f3 = xcx[50].i ;  /* cos=1 sin=0 */
   f2 = xcx[48].r ; f4 = xcx[48].i ;
   xcx[50].r = f2-f1 ; xcx[50].i = f4-f3 ;
   xcx[48].r = f2+f1 ; xcx[48].i = f4+f3 ;

   f1 = xcx[54].r ; f3 = xcx[54].i ;  /* cos=1 sin=0 */
   f2 = xcx[52].r ; f4 = xcx[52].i ;
   xcx[54].r = f2-f1 ; xcx[54].i = f4-f3 ;
   xcx[52].r = f2+f1 ; xcx[52].i = f4+f3 ;

   f1 = xcx[58].r ; f3 = xcx[58].i ;  /* cos=1 sin=0 */
   f2 = xcx[56].r ; f4 = xcx[56].i ;
   xcx[58].r = f2-f1 ; xcx[58].i = f4-f3 ;
   xcx[56].r = f2+f1 ; xcx[56].i = f4+f3 ;

   f1 = xcx[62].r ; f3 = xcx[62].i ;  /* cos=1 sin=0 */
   f2 = xcx[60].r ; f4 = xcx[60].i ;
   xcx[62].r = f2-f1 ; xcx[62].i = f4-f3 ;
   xcx[60].r = f2+f1 ; xcx[60].i = f4+f3 ;

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

   f1 = - xcx[35].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[35].r * csp[2].i ;
   f2 = xcx[33].r ; f4 = xcx[33].i ;
   xcx[35].r = f2-f1 ; xcx[35].i = f4-f3 ;
   xcx[33].r = f2+f1 ; xcx[33].i = f4+f3 ;

   f1 = - xcx[39].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[39].r * csp[2].i ;
   f2 = xcx[37].r ; f4 = xcx[37].i ;
   xcx[39].r = f2-f1 ; xcx[39].i = f4-f3 ;
   xcx[37].r = f2+f1 ; xcx[37].i = f4+f3 ;

   f1 = - xcx[43].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[43].r * csp[2].i ;
   f2 = xcx[41].r ; f4 = xcx[41].i ;
   xcx[43].r = f2-f1 ; xcx[43].i = f4-f3 ;
   xcx[41].r = f2+f1 ; xcx[41].i = f4+f3 ;

   f1 = - xcx[47].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[47].r * csp[2].i ;
   f2 = xcx[45].r ; f4 = xcx[45].i ;
   xcx[47].r = f2-f1 ; xcx[47].i = f4-f3 ;
   xcx[45].r = f2+f1 ; xcx[45].i = f4+f3 ;

   f1 = - xcx[51].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[51].r * csp[2].i ;
   f2 = xcx[49].r ; f4 = xcx[49].i ;
   xcx[51].r = f2-f1 ; xcx[51].i = f4-f3 ;
   xcx[49].r = f2+f1 ; xcx[49].i = f4+f3 ;

   f1 = - xcx[55].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[55].r * csp[2].i ;
   f2 = xcx[53].r ; f4 = xcx[53].i ;
   xcx[55].r = f2-f1 ; xcx[55].i = f4-f3 ;
   xcx[53].r = f2+f1 ; xcx[53].i = f4+f3 ;

   f1 = - xcx[59].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[59].r * csp[2].i ;
   f2 = xcx[57].r ; f4 = xcx[57].i ;
   xcx[59].r = f2-f1 ; xcx[59].i = f4-f3 ;
   xcx[57].r = f2+f1 ; xcx[57].i = f4+f3 ;

   f1 = - xcx[63].i * csp[2].i ; /* cos=0 twiddles */
   f3 = xcx[63].r * csp[2].i ;
   f2 = xcx[61].r ; f4 = xcx[61].i ;
   xcx[63].r = f2-f1 ; xcx[63].i = f4-f3 ;
   xcx[61].r = f2+f1 ; xcx[61].i = f4+f3 ;

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

   f1 = xcx[36].r ; f3 = xcx[36].i ;  /* cos=1 sin=0 */
   f2 = xcx[32].r ; f4 = xcx[32].i ;
   xcx[36].r = f2-f1 ; xcx[36].i = f4-f3 ;
   xcx[32].r = f2+f1 ; xcx[32].i = f4+f3 ;

   f1 = xcx[44].r ; f3 = xcx[44].i ;  /* cos=1 sin=0 */
   f2 = xcx[40].r ; f4 = xcx[40].i ;
   xcx[44].r = f2-f1 ; xcx[44].i = f4-f3 ;
   xcx[40].r = f2+f1 ; xcx[40].i = f4+f3 ;

   f1 = xcx[52].r ; f3 = xcx[52].i ;  /* cos=1 sin=0 */
   f2 = xcx[48].r ; f4 = xcx[48].i ;
   xcx[52].r = f2-f1 ; xcx[52].i = f4-f3 ;
   xcx[48].r = f2+f1 ; xcx[48].i = f4+f3 ;

   f1 = xcx[60].r ; f3 = xcx[60].i ;  /* cos=1 sin=0 */
   f2 = xcx[56].r ; f4 = xcx[56].i ;
   xcx[60].r = f2-f1 ; xcx[60].i = f4-f3 ;
   xcx[56].r = f2+f1 ; xcx[56].i = f4+f3 ;

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

   f1 = xcx[37].r * csp[4].r - xcx[37].i * csp[4].i ; /* twiddles */
   f3 = xcx[37].r * csp[4].i + xcx[37].i * csp[4].r ;
   f2 = xcx[33].r ; f4 = xcx[33].i ;
   xcx[37].r = f2-f1 ; xcx[37].i = f4-f3 ;
   xcx[33].r = f2+f1 ; xcx[33].i = f4+f3 ;

   f1 = xcx[45].r * csp[4].r - xcx[45].i * csp[4].i ; /* twiddles */
   f3 = xcx[45].r * csp[4].i + xcx[45].i * csp[4].r ;
   f2 = xcx[41].r ; f4 = xcx[41].i ;
   xcx[45].r = f2-f1 ; xcx[45].i = f4-f3 ;
   xcx[41].r = f2+f1 ; xcx[41].i = f4+f3 ;

   f1 = xcx[53].r * csp[4].r - xcx[53].i * csp[4].i ; /* twiddles */
   f3 = xcx[53].r * csp[4].i + xcx[53].i * csp[4].r ;
   f2 = xcx[49].r ; f4 = xcx[49].i ;
   xcx[53].r = f2-f1 ; xcx[53].i = f4-f3 ;
   xcx[49].r = f2+f1 ; xcx[49].i = f4+f3 ;

   f1 = xcx[61].r * csp[4].r - xcx[61].i * csp[4].i ; /* twiddles */
   f3 = xcx[61].r * csp[4].i + xcx[61].i * csp[4].r ;
   f2 = xcx[57].r ; f4 = xcx[57].i ;
   xcx[61].r = f2-f1 ; xcx[61].i = f4-f3 ;
   xcx[57].r = f2+f1 ; xcx[57].i = f4+f3 ;

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

   f1 = - xcx[38].i * csp[5].i ; /* cos=0 twiddles */
   f3 = xcx[38].r * csp[5].i ;
   f2 = xcx[34].r ; f4 = xcx[34].i ;
   xcx[38].r = f2-f1 ; xcx[38].i = f4-f3 ;
   xcx[34].r = f2+f1 ; xcx[34].i = f4+f3 ;

   f1 = - xcx[46].i * csp[5].i ; /* cos=0 twiddles */
   f3 = xcx[46].r * csp[5].i ;
   f2 = xcx[42].r ; f4 = xcx[42].i ;
   xcx[46].r = f2-f1 ; xcx[46].i = f4-f3 ;
   xcx[42].r = f2+f1 ; xcx[42].i = f4+f3 ;

   f1 = - xcx[54].i * csp[5].i ; /* cos=0 twiddles */
   f3 = xcx[54].r * csp[5].i ;
   f2 = xcx[50].r ; f4 = xcx[50].i ;
   xcx[54].r = f2-f1 ; xcx[54].i = f4-f3 ;
   xcx[50].r = f2+f1 ; xcx[50].i = f4+f3 ;

   f1 = - xcx[62].i * csp[5].i ; /* cos=0 twiddles */
   f3 = xcx[62].r * csp[5].i ;
   f2 = xcx[58].r ; f4 = xcx[58].i ;
   xcx[62].r = f2-f1 ; xcx[62].i = f4-f3 ;
   xcx[58].r = f2+f1 ; xcx[58].i = f4+f3 ;

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

   f1 = xcx[39].r * csp[6].r - xcx[39].i * csp[6].i ; /* twiddles */
   f3 = xcx[39].r * csp[6].i + xcx[39].i * csp[6].r ;
   f2 = xcx[35].r ; f4 = xcx[35].i ;
   xcx[39].r = f2-f1 ; xcx[39].i = f4-f3 ;
   xcx[35].r = f2+f1 ; xcx[35].i = f4+f3 ;

   f1 = xcx[47].r * csp[6].r - xcx[47].i * csp[6].i ; /* twiddles */
   f3 = xcx[47].r * csp[6].i + xcx[47].i * csp[6].r ;
   f2 = xcx[43].r ; f4 = xcx[43].i ;
   xcx[47].r = f2-f1 ; xcx[47].i = f4-f3 ;
   xcx[43].r = f2+f1 ; xcx[43].i = f4+f3 ;

   f1 = xcx[55].r * csp[6].r - xcx[55].i * csp[6].i ; /* twiddles */
   f3 = xcx[55].r * csp[6].i + xcx[55].i * csp[6].r ;
   f2 = xcx[51].r ; f4 = xcx[51].i ;
   xcx[55].r = f2-f1 ; xcx[55].i = f4-f3 ;
   xcx[51].r = f2+f1 ; xcx[51].i = f4+f3 ;

   f1 = xcx[63].r * csp[6].r - xcx[63].i * csp[6].i ; /* twiddles */
   f3 = xcx[63].r * csp[6].i + xcx[63].i * csp[6].r ;
   f2 = xcx[59].r ; f4 = xcx[59].i ;
   xcx[63].r = f2-f1 ; xcx[63].i = f4-f3 ;
   xcx[59].r = f2+f1 ; xcx[59].i = f4+f3 ;

   f1 = xcx[8].r ; f3 = xcx[8].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[8].r = f2-f1 ; xcx[8].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[24].r ; f3 = xcx[24].i ;  /* cos=1 sin=0 */
   f2 = xcx[16].r ; f4 = xcx[16].i ;
   xcx[24].r = f2-f1 ; xcx[24].i = f4-f3 ;
   xcx[16].r = f2+f1 ; xcx[16].i = f4+f3 ;

   f1 = xcx[40].r ; f3 = xcx[40].i ;  /* cos=1 sin=0 */
   f2 = xcx[32].r ; f4 = xcx[32].i ;
   xcx[40].r = f2-f1 ; xcx[40].i = f4-f3 ;
   xcx[32].r = f2+f1 ; xcx[32].i = f4+f3 ;

   f1 = xcx[56].r ; f3 = xcx[56].i ;  /* cos=1 sin=0 */
   f2 = xcx[48].r ; f4 = xcx[48].i ;
   xcx[56].r = f2-f1 ; xcx[56].i = f4-f3 ;
   xcx[48].r = f2+f1 ; xcx[48].i = f4+f3 ;

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

   f1 = xcx[41].r * csp[8].r - xcx[41].i * csp[8].i ; /* twiddles */
   f3 = xcx[41].r * csp[8].i + xcx[41].i * csp[8].r ;
   f2 = xcx[33].r ; f4 = xcx[33].i ;
   xcx[41].r = f2-f1 ; xcx[41].i = f4-f3 ;
   xcx[33].r = f2+f1 ; xcx[33].i = f4+f3 ;

   f1 = xcx[57].r * csp[8].r - xcx[57].i * csp[8].i ; /* twiddles */
   f3 = xcx[57].r * csp[8].i + xcx[57].i * csp[8].r ;
   f2 = xcx[49].r ; f4 = xcx[49].i ;
   xcx[57].r = f2-f1 ; xcx[57].i = f4-f3 ;
   xcx[49].r = f2+f1 ; xcx[49].i = f4+f3 ;

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

   f1 = xcx[42].r * csp[9].r - xcx[42].i * csp[9].i ; /* twiddles */
   f3 = xcx[42].r * csp[9].i + xcx[42].i * csp[9].r ;
   f2 = xcx[34].r ; f4 = xcx[34].i ;
   xcx[42].r = f2-f1 ; xcx[42].i = f4-f3 ;
   xcx[34].r = f2+f1 ; xcx[34].i = f4+f3 ;

   f1 = xcx[58].r * csp[9].r - xcx[58].i * csp[9].i ; /* twiddles */
   f3 = xcx[58].r * csp[9].i + xcx[58].i * csp[9].r ;
   f2 = xcx[50].r ; f4 = xcx[50].i ;
   xcx[58].r = f2-f1 ; xcx[58].i = f4-f3 ;
   xcx[50].r = f2+f1 ; xcx[50].i = f4+f3 ;

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

   f1 = xcx[43].r * csp[10].r - xcx[43].i * csp[10].i ; /* twiddles */
   f3 = xcx[43].r * csp[10].i + xcx[43].i * csp[10].r ;
   f2 = xcx[35].r ; f4 = xcx[35].i ;
   xcx[43].r = f2-f1 ; xcx[43].i = f4-f3 ;
   xcx[35].r = f2+f1 ; xcx[35].i = f4+f3 ;

   f1 = xcx[59].r * csp[10].r - xcx[59].i * csp[10].i ; /* twiddles */
   f3 = xcx[59].r * csp[10].i + xcx[59].i * csp[10].r ;
   f2 = xcx[51].r ; f4 = xcx[51].i ;
   xcx[59].r = f2-f1 ; xcx[59].i = f4-f3 ;
   xcx[51].r = f2+f1 ; xcx[51].i = f4+f3 ;

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

   f1 = - xcx[44].i * csp[11].i ; /* cos=0 twiddles */
   f3 = xcx[44].r * csp[11].i ;
   f2 = xcx[36].r ; f4 = xcx[36].i ;
   xcx[44].r = f2-f1 ; xcx[44].i = f4-f3 ;
   xcx[36].r = f2+f1 ; xcx[36].i = f4+f3 ;

   f1 = - xcx[60].i * csp[11].i ; /* cos=0 twiddles */
   f3 = xcx[60].r * csp[11].i ;
   f2 = xcx[52].r ; f4 = xcx[52].i ;
   xcx[60].r = f2-f1 ; xcx[60].i = f4-f3 ;
   xcx[52].r = f2+f1 ; xcx[52].i = f4+f3 ;

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

   f1 = xcx[45].r * csp[12].r - xcx[45].i * csp[12].i ; /* twiddles */
   f3 = xcx[45].r * csp[12].i + xcx[45].i * csp[12].r ;
   f2 = xcx[37].r ; f4 = xcx[37].i ;
   xcx[45].r = f2-f1 ; xcx[45].i = f4-f3 ;
   xcx[37].r = f2+f1 ; xcx[37].i = f4+f3 ;

   f1 = xcx[61].r * csp[12].r - xcx[61].i * csp[12].i ; /* twiddles */
   f3 = xcx[61].r * csp[12].i + xcx[61].i * csp[12].r ;
   f2 = xcx[53].r ; f4 = xcx[53].i ;
   xcx[61].r = f2-f1 ; xcx[61].i = f4-f3 ;
   xcx[53].r = f2+f1 ; xcx[53].i = f4+f3 ;

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

   f1 = xcx[46].r * csp[13].r - xcx[46].i * csp[13].i ; /* twiddles */
   f3 = xcx[46].r * csp[13].i + xcx[46].i * csp[13].r ;
   f2 = xcx[38].r ; f4 = xcx[38].i ;
   xcx[46].r = f2-f1 ; xcx[46].i = f4-f3 ;
   xcx[38].r = f2+f1 ; xcx[38].i = f4+f3 ;

   f1 = xcx[62].r * csp[13].r - xcx[62].i * csp[13].i ; /* twiddles */
   f3 = xcx[62].r * csp[13].i + xcx[62].i * csp[13].r ;
   f2 = xcx[54].r ; f4 = xcx[54].i ;
   xcx[62].r = f2-f1 ; xcx[62].i = f4-f3 ;
   xcx[54].r = f2+f1 ; xcx[54].i = f4+f3 ;

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

   f1 = xcx[47].r * csp[14].r - xcx[47].i * csp[14].i ; /* twiddles */
   f3 = xcx[47].r * csp[14].i + xcx[47].i * csp[14].r ;
   f2 = xcx[39].r ; f4 = xcx[39].i ;
   xcx[47].r = f2-f1 ; xcx[47].i = f4-f3 ;
   xcx[39].r = f2+f1 ; xcx[39].i = f4+f3 ;

   f1 = xcx[63].r * csp[14].r - xcx[63].i * csp[14].i ; /* twiddles */
   f3 = xcx[63].r * csp[14].i + xcx[63].i * csp[14].r ;
   f2 = xcx[55].r ; f4 = xcx[55].i ;
   xcx[63].r = f2-f1 ; xcx[63].i = f4-f3 ;
   xcx[55].r = f2+f1 ; xcx[55].i = f4+f3 ;

   f1 = xcx[16].r ; f3 = xcx[16].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[16].r = f2-f1 ; xcx[16].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[48].r ; f3 = xcx[48].i ;  /* cos=1 sin=0 */
   f2 = xcx[32].r ; f4 = xcx[32].i ;
   xcx[48].r = f2-f1 ; xcx[48].i = f4-f3 ;
   xcx[32].r = f2+f1 ; xcx[32].i = f4+f3 ;

   f1 = xcx[17].r * csp[16].r - xcx[17].i * csp[16].i ; /* twiddles */
   f3 = xcx[17].r * csp[16].i + xcx[17].i * csp[16].r ;
   f2 = xcx[1].r ; f4 = xcx[1].i ;
   xcx[17].r = f2-f1 ; xcx[17].i = f4-f3 ;
   xcx[1].r = f2+f1 ; xcx[1].i = f4+f3 ;

   f1 = xcx[49].r * csp[16].r - xcx[49].i * csp[16].i ; /* twiddles */
   f3 = xcx[49].r * csp[16].i + xcx[49].i * csp[16].r ;
   f2 = xcx[33].r ; f4 = xcx[33].i ;
   xcx[49].r = f2-f1 ; xcx[49].i = f4-f3 ;
   xcx[33].r = f2+f1 ; xcx[33].i = f4+f3 ;

   f1 = xcx[18].r * csp[17].r - xcx[18].i * csp[17].i ; /* twiddles */
   f3 = xcx[18].r * csp[17].i + xcx[18].i * csp[17].r ;
   f2 = xcx[2].r ; f4 = xcx[2].i ;
   xcx[18].r = f2-f1 ; xcx[18].i = f4-f3 ;
   xcx[2].r = f2+f1 ; xcx[2].i = f4+f3 ;

   f1 = xcx[50].r * csp[17].r - xcx[50].i * csp[17].i ; /* twiddles */
   f3 = xcx[50].r * csp[17].i + xcx[50].i * csp[17].r ;
   f2 = xcx[34].r ; f4 = xcx[34].i ;
   xcx[50].r = f2-f1 ; xcx[50].i = f4-f3 ;
   xcx[34].r = f2+f1 ; xcx[34].i = f4+f3 ;

   f1 = xcx[19].r * csp[18].r - xcx[19].i * csp[18].i ; /* twiddles */
   f3 = xcx[19].r * csp[18].i + xcx[19].i * csp[18].r ;
   f2 = xcx[3].r ; f4 = xcx[3].i ;
   xcx[19].r = f2-f1 ; xcx[19].i = f4-f3 ;
   xcx[3].r = f2+f1 ; xcx[3].i = f4+f3 ;

   f1 = xcx[51].r * csp[18].r - xcx[51].i * csp[18].i ; /* twiddles */
   f3 = xcx[51].r * csp[18].i + xcx[51].i * csp[18].r ;
   f2 = xcx[35].r ; f4 = xcx[35].i ;
   xcx[51].r = f2-f1 ; xcx[51].i = f4-f3 ;
   xcx[35].r = f2+f1 ; xcx[35].i = f4+f3 ;

   f1 = xcx[20].r * csp[19].r - xcx[20].i * csp[19].i ; /* twiddles */
   f3 = xcx[20].r * csp[19].i + xcx[20].i * csp[19].r ;
   f2 = xcx[4].r ; f4 = xcx[4].i ;
   xcx[20].r = f2-f1 ; xcx[20].i = f4-f3 ;
   xcx[4].r = f2+f1 ; xcx[4].i = f4+f3 ;

   f1 = xcx[52].r * csp[19].r - xcx[52].i * csp[19].i ; /* twiddles */
   f3 = xcx[52].r * csp[19].i + xcx[52].i * csp[19].r ;
   f2 = xcx[36].r ; f4 = xcx[36].i ;
   xcx[52].r = f2-f1 ; xcx[52].i = f4-f3 ;
   xcx[36].r = f2+f1 ; xcx[36].i = f4+f3 ;

   f1 = xcx[21].r * csp[20].r - xcx[21].i * csp[20].i ; /* twiddles */
   f3 = xcx[21].r * csp[20].i + xcx[21].i * csp[20].r ;
   f2 = xcx[5].r ; f4 = xcx[5].i ;
   xcx[21].r = f2-f1 ; xcx[21].i = f4-f3 ;
   xcx[5].r = f2+f1 ; xcx[5].i = f4+f3 ;

   f1 = xcx[53].r * csp[20].r - xcx[53].i * csp[20].i ; /* twiddles */
   f3 = xcx[53].r * csp[20].i + xcx[53].i * csp[20].r ;
   f2 = xcx[37].r ; f4 = xcx[37].i ;
   xcx[53].r = f2-f1 ; xcx[53].i = f4-f3 ;
   xcx[37].r = f2+f1 ; xcx[37].i = f4+f3 ;

   f1 = xcx[22].r * csp[21].r - xcx[22].i * csp[21].i ; /* twiddles */
   f3 = xcx[22].r * csp[21].i + xcx[22].i * csp[21].r ;
   f2 = xcx[6].r ; f4 = xcx[6].i ;
   xcx[22].r = f2-f1 ; xcx[22].i = f4-f3 ;
   xcx[6].r = f2+f1 ; xcx[6].i = f4+f3 ;

   f1 = xcx[54].r * csp[21].r - xcx[54].i * csp[21].i ; /* twiddles */
   f3 = xcx[54].r * csp[21].i + xcx[54].i * csp[21].r ;
   f2 = xcx[38].r ; f4 = xcx[38].i ;
   xcx[54].r = f2-f1 ; xcx[54].i = f4-f3 ;
   xcx[38].r = f2+f1 ; xcx[38].i = f4+f3 ;

   f1 = xcx[23].r * csp[22].r - xcx[23].i * csp[22].i ; /* twiddles */
   f3 = xcx[23].r * csp[22].i + xcx[23].i * csp[22].r ;
   f2 = xcx[7].r ; f4 = xcx[7].i ;
   xcx[23].r = f2-f1 ; xcx[23].i = f4-f3 ;
   xcx[7].r = f2+f1 ; xcx[7].i = f4+f3 ;

   f1 = xcx[55].r * csp[22].r - xcx[55].i * csp[22].i ; /* twiddles */
   f3 = xcx[55].r * csp[22].i + xcx[55].i * csp[22].r ;
   f2 = xcx[39].r ; f4 = xcx[39].i ;
   xcx[55].r = f2-f1 ; xcx[55].i = f4-f3 ;
   xcx[39].r = f2+f1 ; xcx[39].i = f4+f3 ;

   f1 = - xcx[24].i * csp[23].i ; /* cos=0 twiddles */
   f3 = xcx[24].r * csp[23].i ;
   f2 = xcx[8].r ; f4 = xcx[8].i ;
   xcx[24].r = f2-f1 ; xcx[24].i = f4-f3 ;
   xcx[8].r = f2+f1 ; xcx[8].i = f4+f3 ;

   f1 = - xcx[56].i * csp[23].i ; /* cos=0 twiddles */
   f3 = xcx[56].r * csp[23].i ;
   f2 = xcx[40].r ; f4 = xcx[40].i ;
   xcx[56].r = f2-f1 ; xcx[56].i = f4-f3 ;
   xcx[40].r = f2+f1 ; xcx[40].i = f4+f3 ;

   f1 = xcx[25].r * csp[24].r - xcx[25].i * csp[24].i ; /* twiddles */
   f3 = xcx[25].r * csp[24].i + xcx[25].i * csp[24].r ;
   f2 = xcx[9].r ; f4 = xcx[9].i ;
   xcx[25].r = f2-f1 ; xcx[25].i = f4-f3 ;
   xcx[9].r = f2+f1 ; xcx[9].i = f4+f3 ;

   f1 = xcx[57].r * csp[24].r - xcx[57].i * csp[24].i ; /* twiddles */
   f3 = xcx[57].r * csp[24].i + xcx[57].i * csp[24].r ;
   f2 = xcx[41].r ; f4 = xcx[41].i ;
   xcx[57].r = f2-f1 ; xcx[57].i = f4-f3 ;
   xcx[41].r = f2+f1 ; xcx[41].i = f4+f3 ;

   f1 = xcx[26].r * csp[25].r - xcx[26].i * csp[25].i ; /* twiddles */
   f3 = xcx[26].r * csp[25].i + xcx[26].i * csp[25].r ;
   f2 = xcx[10].r ; f4 = xcx[10].i ;
   xcx[26].r = f2-f1 ; xcx[26].i = f4-f3 ;
   xcx[10].r = f2+f1 ; xcx[10].i = f4+f3 ;

   f1 = xcx[58].r * csp[25].r - xcx[58].i * csp[25].i ; /* twiddles */
   f3 = xcx[58].r * csp[25].i + xcx[58].i * csp[25].r ;
   f2 = xcx[42].r ; f4 = xcx[42].i ;
   xcx[58].r = f2-f1 ; xcx[58].i = f4-f3 ;
   xcx[42].r = f2+f1 ; xcx[42].i = f4+f3 ;

   f1 = xcx[27].r * csp[26].r - xcx[27].i * csp[26].i ; /* twiddles */
   f3 = xcx[27].r * csp[26].i + xcx[27].i * csp[26].r ;
   f2 = xcx[11].r ; f4 = xcx[11].i ;
   xcx[27].r = f2-f1 ; xcx[27].i = f4-f3 ;
   xcx[11].r = f2+f1 ; xcx[11].i = f4+f3 ;

   f1 = xcx[59].r * csp[26].r - xcx[59].i * csp[26].i ; /* twiddles */
   f3 = xcx[59].r * csp[26].i + xcx[59].i * csp[26].r ;
   f2 = xcx[43].r ; f4 = xcx[43].i ;
   xcx[59].r = f2-f1 ; xcx[59].i = f4-f3 ;
   xcx[43].r = f2+f1 ; xcx[43].i = f4+f3 ;

   f1 = xcx[28].r * csp[27].r - xcx[28].i * csp[27].i ; /* twiddles */
   f3 = xcx[28].r * csp[27].i + xcx[28].i * csp[27].r ;
   f2 = xcx[12].r ; f4 = xcx[12].i ;
   xcx[28].r = f2-f1 ; xcx[28].i = f4-f3 ;
   xcx[12].r = f2+f1 ; xcx[12].i = f4+f3 ;

   f1 = xcx[60].r * csp[27].r - xcx[60].i * csp[27].i ; /* twiddles */
   f3 = xcx[60].r * csp[27].i + xcx[60].i * csp[27].r ;
   f2 = xcx[44].r ; f4 = xcx[44].i ;
   xcx[60].r = f2-f1 ; xcx[60].i = f4-f3 ;
   xcx[44].r = f2+f1 ; xcx[44].i = f4+f3 ;

   f1 = xcx[29].r * csp[28].r - xcx[29].i * csp[28].i ; /* twiddles */
   f3 = xcx[29].r * csp[28].i + xcx[29].i * csp[28].r ;
   f2 = xcx[13].r ; f4 = xcx[13].i ;
   xcx[29].r = f2-f1 ; xcx[29].i = f4-f3 ;
   xcx[13].r = f2+f1 ; xcx[13].i = f4+f3 ;

   f1 = xcx[61].r * csp[28].r - xcx[61].i * csp[28].i ; /* twiddles */
   f3 = xcx[61].r * csp[28].i + xcx[61].i * csp[28].r ;
   f2 = xcx[45].r ; f4 = xcx[45].i ;
   xcx[61].r = f2-f1 ; xcx[61].i = f4-f3 ;
   xcx[45].r = f2+f1 ; xcx[45].i = f4+f3 ;

   f1 = xcx[30].r * csp[29].r - xcx[30].i * csp[29].i ; /* twiddles */
   f3 = xcx[30].r * csp[29].i + xcx[30].i * csp[29].r ;
   f2 = xcx[14].r ; f4 = xcx[14].i ;
   xcx[30].r = f2-f1 ; xcx[30].i = f4-f3 ;
   xcx[14].r = f2+f1 ; xcx[14].i = f4+f3 ;

   f1 = xcx[62].r * csp[29].r - xcx[62].i * csp[29].i ; /* twiddles */
   f3 = xcx[62].r * csp[29].i + xcx[62].i * csp[29].r ;
   f2 = xcx[46].r ; f4 = xcx[46].i ;
   xcx[62].r = f2-f1 ; xcx[62].i = f4-f3 ;
   xcx[46].r = f2+f1 ; xcx[46].i = f4+f3 ;

   f1 = xcx[31].r * csp[30].r - xcx[31].i * csp[30].i ; /* twiddles */
   f3 = xcx[31].r * csp[30].i + xcx[31].i * csp[30].r ;
   f2 = xcx[15].r ; f4 = xcx[15].i ;
   xcx[31].r = f2-f1 ; xcx[31].i = f4-f3 ;
   xcx[15].r = f2+f1 ; xcx[15].i = f4+f3 ;

   f1 = xcx[63].r * csp[30].r - xcx[63].i * csp[30].i ; /* twiddles */
   f3 = xcx[63].r * csp[30].i + xcx[63].i * csp[30].r ;
   f2 = xcx[47].r ; f4 = xcx[47].i ;
   xcx[63].r = f2-f1 ; xcx[63].i = f4-f3 ;
   xcx[47].r = f2+f1 ; xcx[47].i = f4+f3 ;

   f1 = xcx[32].r ; f3 = xcx[32].i ;  /* cos=1 sin=0 */
   f2 = xcx[0].r ; f4 = xcx[0].i ;
   xcx[32].r = f2-f1 ; xcx[32].i = f4-f3 ;
   xcx[0].r = f2+f1 ; xcx[0].i = f4+f3 ;

   f1 = xcx[33].r * csp[32].r - xcx[33].i * csp[32].i ; /* twiddles */
   f3 = xcx[33].r * csp[32].i + xcx[33].i * csp[32].r ;
   f2 = xcx[1].r ; f4 = xcx[1].i ;
   xcx[33].r = f2-f1 ; xcx[33].i = f4-f3 ;
   xcx[1].r = f2+f1 ; xcx[1].i = f4+f3 ;

   f1 = xcx[34].r * csp[33].r - xcx[34].i * csp[33].i ; /* twiddles */
   f3 = xcx[34].r * csp[33].i + xcx[34].i * csp[33].r ;
   f2 = xcx[2].r ; f4 = xcx[2].i ;
   xcx[34].r = f2-f1 ; xcx[34].i = f4-f3 ;
   xcx[2].r = f2+f1 ; xcx[2].i = f4+f3 ;

   f1 = xcx[35].r * csp[34].r - xcx[35].i * csp[34].i ; /* twiddles */
   f3 = xcx[35].r * csp[34].i + xcx[35].i * csp[34].r ;
   f2 = xcx[3].r ; f4 = xcx[3].i ;
   xcx[35].r = f2-f1 ; xcx[35].i = f4-f3 ;
   xcx[3].r = f2+f1 ; xcx[3].i = f4+f3 ;

   f1 = xcx[36].r * csp[35].r - xcx[36].i * csp[35].i ; /* twiddles */
   f3 = xcx[36].r * csp[35].i + xcx[36].i * csp[35].r ;
   f2 = xcx[4].r ; f4 = xcx[4].i ;
   xcx[36].r = f2-f1 ; xcx[36].i = f4-f3 ;
   xcx[4].r = f2+f1 ; xcx[4].i = f4+f3 ;

   f1 = xcx[37].r * csp[36].r - xcx[37].i * csp[36].i ; /* twiddles */
   f3 = xcx[37].r * csp[36].i + xcx[37].i * csp[36].r ;
   f2 = xcx[5].r ; f4 = xcx[5].i ;
   xcx[37].r = f2-f1 ; xcx[37].i = f4-f3 ;
   xcx[5].r = f2+f1 ; xcx[5].i = f4+f3 ;

   f1 = xcx[38].r * csp[37].r - xcx[38].i * csp[37].i ; /* twiddles */
   f3 = xcx[38].r * csp[37].i + xcx[38].i * csp[37].r ;
   f2 = xcx[6].r ; f4 = xcx[6].i ;
   xcx[38].r = f2-f1 ; xcx[38].i = f4-f3 ;
   xcx[6].r = f2+f1 ; xcx[6].i = f4+f3 ;

   f1 = xcx[39].r * csp[38].r - xcx[39].i * csp[38].i ; /* twiddles */
   f3 = xcx[39].r * csp[38].i + xcx[39].i * csp[38].r ;
   f2 = xcx[7].r ; f4 = xcx[7].i ;
   xcx[39].r = f2-f1 ; xcx[39].i = f4-f3 ;
   xcx[7].r = f2+f1 ; xcx[7].i = f4+f3 ;

   f1 = xcx[40].r * csp[39].r - xcx[40].i * csp[39].i ; /* twiddles */
   f3 = xcx[40].r * csp[39].i + xcx[40].i * csp[39].r ;
   f2 = xcx[8].r ; f4 = xcx[8].i ;
   xcx[40].r = f2-f1 ; xcx[40].i = f4-f3 ;
   xcx[8].r = f2+f1 ; xcx[8].i = f4+f3 ;

   f1 = xcx[41].r * csp[40].r - xcx[41].i * csp[40].i ; /* twiddles */
   f3 = xcx[41].r * csp[40].i + xcx[41].i * csp[40].r ;
   f2 = xcx[9].r ; f4 = xcx[9].i ;
   xcx[41].r = f2-f1 ; xcx[41].i = f4-f3 ;
   xcx[9].r = f2+f1 ; xcx[9].i = f4+f3 ;

   f1 = xcx[42].r * csp[41].r - xcx[42].i * csp[41].i ; /* twiddles */
   f3 = xcx[42].r * csp[41].i + xcx[42].i * csp[41].r ;
   f2 = xcx[10].r ; f4 = xcx[10].i ;
   xcx[42].r = f2-f1 ; xcx[42].i = f4-f3 ;
   xcx[10].r = f2+f1 ; xcx[10].i = f4+f3 ;

   f1 = xcx[43].r * csp[42].r - xcx[43].i * csp[42].i ; /* twiddles */
   f3 = xcx[43].r * csp[42].i + xcx[43].i * csp[42].r ;
   f2 = xcx[11].r ; f4 = xcx[11].i ;
   xcx[43].r = f2-f1 ; xcx[43].i = f4-f3 ;
   xcx[11].r = f2+f1 ; xcx[11].i = f4+f3 ;

   f1 = xcx[44].r * csp[43].r - xcx[44].i * csp[43].i ; /* twiddles */
   f3 = xcx[44].r * csp[43].i + xcx[44].i * csp[43].r ;
   f2 = xcx[12].r ; f4 = xcx[12].i ;
   xcx[44].r = f2-f1 ; xcx[44].i = f4-f3 ;
   xcx[12].r = f2+f1 ; xcx[12].i = f4+f3 ;

   f1 = xcx[45].r * csp[44].r - xcx[45].i * csp[44].i ; /* twiddles */
   f3 = xcx[45].r * csp[44].i + xcx[45].i * csp[44].r ;
   f2 = xcx[13].r ; f4 = xcx[13].i ;
   xcx[45].r = f2-f1 ; xcx[45].i = f4-f3 ;
   xcx[13].r = f2+f1 ; xcx[13].i = f4+f3 ;

   f1 = xcx[46].r * csp[45].r - xcx[46].i * csp[45].i ; /* twiddles */
   f3 = xcx[46].r * csp[45].i + xcx[46].i * csp[45].r ;
   f2 = xcx[14].r ; f4 = xcx[14].i ;
   xcx[46].r = f2-f1 ; xcx[46].i = f4-f3 ;
   xcx[14].r = f2+f1 ; xcx[14].i = f4+f3 ;

   f1 = xcx[47].r * csp[46].r - xcx[47].i * csp[46].i ; /* twiddles */
   f3 = xcx[47].r * csp[46].i + xcx[47].i * csp[46].r ;
   f2 = xcx[15].r ; f4 = xcx[15].i ;
   xcx[47].r = f2-f1 ; xcx[47].i = f4-f3 ;
   xcx[15].r = f2+f1 ; xcx[15].i = f4+f3 ;

   f1 = - xcx[48].i * csp[47].i ; /* cos=0 twiddles */
   f3 = xcx[48].r * csp[47].i ;
   f2 = xcx[16].r ; f4 = xcx[16].i ;
   xcx[48].r = f2-f1 ; xcx[48].i = f4-f3 ;
   xcx[16].r = f2+f1 ; xcx[16].i = f4+f3 ;

   f1 = xcx[49].r * csp[48].r - xcx[49].i * csp[48].i ; /* twiddles */
   f3 = xcx[49].r * csp[48].i + xcx[49].i * csp[48].r ;
   f2 = xcx[17].r ; f4 = xcx[17].i ;
   xcx[49].r = f2-f1 ; xcx[49].i = f4-f3 ;
   xcx[17].r = f2+f1 ; xcx[17].i = f4+f3 ;

   f1 = xcx[50].r * csp[49].r - xcx[50].i * csp[49].i ; /* twiddles */
   f3 = xcx[50].r * csp[49].i + xcx[50].i * csp[49].r ;
   f2 = xcx[18].r ; f4 = xcx[18].i ;
   xcx[50].r = f2-f1 ; xcx[50].i = f4-f3 ;
   xcx[18].r = f2+f1 ; xcx[18].i = f4+f3 ;

   f1 = xcx[51].r * csp[50].r - xcx[51].i * csp[50].i ; /* twiddles */
   f3 = xcx[51].r * csp[50].i + xcx[51].i * csp[50].r ;
   f2 = xcx[19].r ; f4 = xcx[19].i ;
   xcx[51].r = f2-f1 ; xcx[51].i = f4-f3 ;
   xcx[19].r = f2+f1 ; xcx[19].i = f4+f3 ;

   f1 = xcx[52].r * csp[51].r - xcx[52].i * csp[51].i ; /* twiddles */
   f3 = xcx[52].r * csp[51].i + xcx[52].i * csp[51].r ;
   f2 = xcx[20].r ; f4 = xcx[20].i ;
   xcx[52].r = f2-f1 ; xcx[52].i = f4-f3 ;
   xcx[20].r = f2+f1 ; xcx[20].i = f4+f3 ;

   f1 = xcx[53].r * csp[52].r - xcx[53].i * csp[52].i ; /* twiddles */
   f3 = xcx[53].r * csp[52].i + xcx[53].i * csp[52].r ;
   f2 = xcx[21].r ; f4 = xcx[21].i ;
   xcx[53].r = f2-f1 ; xcx[53].i = f4-f3 ;
   xcx[21].r = f2+f1 ; xcx[21].i = f4+f3 ;

   f1 = xcx[54].r * csp[53].r - xcx[54].i * csp[53].i ; /* twiddles */
   f3 = xcx[54].r * csp[53].i + xcx[54].i * csp[53].r ;
   f2 = xcx[22].r ; f4 = xcx[22].i ;
   xcx[54].r = f2-f1 ; xcx[54].i = f4-f3 ;
   xcx[22].r = f2+f1 ; xcx[22].i = f4+f3 ;

   f1 = xcx[55].r * csp[54].r - xcx[55].i * csp[54].i ; /* twiddles */
   f3 = xcx[55].r * csp[54].i + xcx[55].i * csp[54].r ;
   f2 = xcx[23].r ; f4 = xcx[23].i ;
   xcx[55].r = f2-f1 ; xcx[55].i = f4-f3 ;
   xcx[23].r = f2+f1 ; xcx[23].i = f4+f3 ;

   f1 = xcx[56].r * csp[55].r - xcx[56].i * csp[55].i ; /* twiddles */
   f3 = xcx[56].r * csp[55].i + xcx[56].i * csp[55].r ;
   f2 = xcx[24].r ; f4 = xcx[24].i ;
   xcx[56].r = f2-f1 ; xcx[56].i = f4-f3 ;
   xcx[24].r = f2+f1 ; xcx[24].i = f4+f3 ;

   f1 = xcx[57].r * csp[56].r - xcx[57].i * csp[56].i ; /* twiddles */
   f3 = xcx[57].r * csp[56].i + xcx[57].i * csp[56].r ;
   f2 = xcx[25].r ; f4 = xcx[25].i ;
   xcx[57].r = f2-f1 ; xcx[57].i = f4-f3 ;
   xcx[25].r = f2+f1 ; xcx[25].i = f4+f3 ;

   f1 = xcx[58].r * csp[57].r - xcx[58].i * csp[57].i ; /* twiddles */
   f3 = xcx[58].r * csp[57].i + xcx[58].i * csp[57].r ;
   f2 = xcx[26].r ; f4 = xcx[26].i ;
   xcx[58].r = f2-f1 ; xcx[58].i = f4-f3 ;
   xcx[26].r = f2+f1 ; xcx[26].i = f4+f3 ;

   f1 = xcx[59].r * csp[58].r - xcx[59].i * csp[58].i ; /* twiddles */
   f3 = xcx[59].r * csp[58].i + xcx[59].i * csp[58].r ;
   f2 = xcx[27].r ; f4 = xcx[27].i ;
   xcx[59].r = f2-f1 ; xcx[59].i = f4-f3 ;
   xcx[27].r = f2+f1 ; xcx[27].i = f4+f3 ;

   f1 = xcx[60].r * csp[59].r - xcx[60].i * csp[59].i ; /* twiddles */
   f3 = xcx[60].r * csp[59].i + xcx[60].i * csp[59].r ;
   f2 = xcx[28].r ; f4 = xcx[28].i ;
   xcx[60].r = f2-f1 ; xcx[60].i = f4-f3 ;
   xcx[28].r = f2+f1 ; xcx[28].i = f4+f3 ;

   f1 = xcx[61].r * csp[60].r - xcx[61].i * csp[60].i ; /* twiddles */
   f3 = xcx[61].r * csp[60].i + xcx[61].i * csp[60].r ;
   f2 = xcx[29].r ; f4 = xcx[29].i ;
   xcx[61].r = f2-f1 ; xcx[61].i = f4-f3 ;
   xcx[29].r = f2+f1 ; xcx[29].i = f4+f3 ;

   f1 = xcx[62].r * csp[61].r - xcx[62].i * csp[61].i ; /* twiddles */
   f3 = xcx[62].r * csp[61].i + xcx[62].i * csp[61].r ;
   f2 = xcx[30].r ; f4 = xcx[30].i ;
   xcx[62].r = f2-f1 ; xcx[62].i = f4-f3 ;
   xcx[30].r = f2+f1 ; xcx[30].i = f4+f3 ;

   f1 = xcx[63].r * csp[62].r - xcx[63].i * csp[62].i ; /* twiddles */
   f3 = xcx[63].r * csp[62].i + xcx[63].i * csp[62].r ;
   f2 = xcx[31].r ; f4 = xcx[31].i ;
   xcx[63].r = f2-f1 ; xcx[63].i = f4-f3 ;
   xcx[31].r = f2+f1 ; xcx[31].i = f4+f3 ;

   return ;
}

/*================================================================*/

/*------------------------------------------------------------------
   fft_4dec: do a decimation by 4.
   At most RMAX levels of recursion are allowed.
--------------------------------------------------------------------*/

static void fft_4dec_OMP( int mode , int idim , complex *xc )
{
   DECLARE_ithr ;
   int rec , mold ;
   int N=idim , M=idim/4 , M2=2*M , M3=3*M ;
   complex *cs , *aa , *bb , *cc , *dd ;
   int k , i ;
   float aar,aai, tr,ti, bbr,bbi, ccr,cci , ddr,ddi , t1,t2 ,
         acpr,acmr , bdpr,bdmr , acpi,acmi , bdpi,bdmi ;

/* ININFO_message("fft_4dec_OMP(%d)",idim) ; */

   /*-- initialize cosine/sine table and memory space --*/

   rec = CFO_recX[ithr] ;  /* recursion level (0 .. RMAX-1) */
   if( rec >= RMAX ){
     fprintf(stderr,"\n*** fft_4dec_OMP: too many recursions with idim=%d\n",idim); EXIT(1);
   }

   CFO_assign(cs,M3) ;  /* assign workspace of length M3 to cs pointer */
   CFO_assign(aa,M) ;   /* at this recursion level for this thread (etc) */
   CFO_assign(bb,M) ;
   CFO_assign(cc,M) ;
   CFO_assign(dd,M) ;

   mold = CFO_rmold4[rec][ithr] ;  /* what was done before at this recursion level */

  { if( M != mold ){
      double th = (2.0*PI/N) ;
      cs[0].r = 1.0 ; cs[0].i = 0.0 ;
      for( k=1 ; k < M3 ; k++ ){ cs[k].r = cos(k*th); cs[k].i = sin(k*th); }
      CFO_rmold4[rec][ithr] = M ;
  }}

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0; k < M; k++ ){
     aa[k]=xc[i++]; bb[k]=xc[i++]; cc[k]=xc[i++]; dd[k]=xc[i++];
   }

   CFO_recX[ithr]++ ;  /* bump recursion level */
   switch( N ){

      default: fprintf(stderr,"\n*** Illegal call to fft_4dec: N=%d\n",N);
               EXIT(1) ;

      case 128: fft32(mode,aa); fft32(mode,bb);               /* no recursion */
                fft32(mode,cc); fft32(mode,dd); break;

      case 256: fft64(mode,aa); fft64(mode,bb);               /* no recursion */
                fft64(mode,cc); fft64(mode,dd); break;

      case 512: fft128(mode,aa); fft128(mode,bb);             /* recurse once */
                fft128(mode,cc); fft128(mode,dd); break;

      case 1024: fft256(mode,aa); fft256(mode,bb);            /* recurse once */
                 fft256(mode,cc); fft256(mode,dd); break;

#if 1
      case 2048: fft512(mode,aa); fft512(mode,bb);            /* recurse twice */
                 fft512(mode,cc); fft512(mode,dd); break;

      case 4096: fft1024(mode,aa); fft1024(mode,bb);          /* recurse twice */
                 fft1024(mode,cc); fft1024(mode,dd); break;

      case 8192: fft2048(mode,aa); fft2048(mode,bb);          /* recurse 3 times */
                 fft2048(mode,cc); fft2048(mode,dd); break;

      case 16384: fft4096(mode,aa); fft4096(mode,bb);         /* recurse 3 times */
                  fft4096(mode,cc); fft4096(mode,dd); break;

      case 32768: fft8192(mode,aa); fft8192(mode,bb);         /* recurse 4 times */
                  fft8192(mode,cc); fft8192(mode,dd); break;

      case 65536: fft16384(mode,aa); fft16384(mode,bb);       /* recurse 4 times */
                  fft16384(mode,cc); fft16384(mode,dd); break;
#endif
   }
   CFO_recX[ithr]-- ;  /* reduce recursion level */

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

static void fft_3dec_OMP( int mode , int idim , complex *xc )
{
   DECLARE_ithr ;
   int rec , mold ;

   int N=idim , M=idim/3 , M2=2*M ;
   complex *cs=NULL , *aa , *bb , *cc ;
   register int k , i ;
   register float aar,aai, tr,ti, bbr,bbi, ccr,cci,
                  t1,t2,t4,t5,t6,t8 ;

/* ININFO_message("fft_3dec_OMP(%d)",idim) ; */

   /*-- initialize cosine/sine table and memory space --*/

   rec = CFO_recX[ithr] ;  /* recursion level (0 .. RMAX-1) */
   if( rec >= RMAX ){
     fprintf(stderr,"\n*** fft_3dec_OMP: too many recursions at idim=%d\n",idim); EXIT(1);
   }

   CFO_assign(cs,M2) ;
   CFO_assign(aa,M) ;
   CFO_assign(bb,M) ;
   CFO_assign(cc,M) ;

   mold = CFO_rmold3[rec][ithr] ;  /* what was done before at this recursion level */

  { if( M != mold ){
      double th = (2.0*PI/N) ;
      cs[0].r = 1.0 ; cs[0].i = 0.0 ;
      for( k=1 ; k < M2 ; k++ ){ cs[k].r = cos(k*th); cs[k].i = sin(k*th); }
      CFO_rmold3[rec][ithr] = M ;
   }}

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0; k < M; k++ ){ aa[k]=xc[i++]; bb[k]=xc[i++]; cc[k]=xc[i++]; }

   CFO_recX[ithr]++ ;
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
#if 1
      case 2048: fft2048(mode,aa) ; fft2048(mode,bb) ; fft2048(mode,cc) ; break;
      case 4096: fft4096(mode,aa) ; fft4096(mode,bb) ; fft4096(mode,cc) ; break;
      case 8192: fft8192(mode,aa) ; fft8192(mode,bb) ; fft8192(mode,cc) ; break;

      case 16384: fft16384(mode,aa); fft16384(mode,bb); fft16384(mode,cc); break;
#endif

      default:  csfft_cox_OMP(mode,M,aa); csfft_cox_OMP(mode,M,bb); csfft_cox_OMP(mode,M,cc);
                break ;
   }
   CFO_recX[ithr]-- ;

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

static void fft_5dec_OMP( int mode , int idim , complex *xc )
{
   DECLARE_ithr ;
   int rec , mold ;
   int N=idim , M=idim/5 , M2=2*M , M3=3*M , M4=4*M ;
   complex *cs, *aa, *bb, *cc, *dd, *ee ;
   register int k , i ;
   register float aar,aai,bbr,bbi,ccr,cci,ddr,ddi,eer,eei ;
   register float akp,akm,bkp,bkm , ajp,ajm,bjp,bjm ;
   register float ak,bk,aj,bj ;
   float c72 , s72 , c2,s2 ;
   int ss ;

/* ININFO_message("fft_5dec_OMP(%d)",idim) ; */

   /*-- initialize cosine/sine table and memory space --*/

   rec = CFO_recX[ithr] ;  /* recursion level (0 .. RMAX-1) */
   if( rec >= RMAX ){
     fprintf(stderr,"\n*** fft_5dec_OMP: too many recursions at idim=%d\n",idim); EXIT(1);
   }

   CFO_assign(cs,M4) ;
   CFO_assign(aa,M) ;
   CFO_assign(bb,M) ;
   CFO_assign(cc,M) ;
   CFO_assign(dd,M) ;
   CFO_assign(ee,M) ;

   mold = CFO_rmold5[rec][ithr] ;  /* what was done before at this recursion level */

  { if( M != mold ){           /* new M? */
      double th = (2.0*PI/N) ;
      cs[0].r = 1.0 ; cs[0].i = 0.0 ;
      for( k=1 ; k < M4 ; k++ ){ cs[k].r = cos(k*th); cs[k].i = sin(k*th); }
      CFO_rmold5[rec][ithr] = M ;
   }}

   /*-- load subarrays, and FFT each one --*/

   for( i=k=0; k < M; k++ ){
      aa[k]=xc[i++]; bb[k]=xc[i++]; cc[k]=xc[i++]; dd[k]=xc[i++]; ee[k]=xc[i++];
   }

   CFO_recX[ithr]++ ;
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
                 fft1024(mode,dd) ; fft1024(mode,ee) ;                break;

#if 1
      case 2048: fft2048(mode,aa) ; fft2048(mode,bb) ; fft2048(mode,cc) ;
                 fft2048(mode,dd) ; fft2048(mode,ee) ;                break;

      case 4096: fft4096(mode,aa) ; fft4096(mode,bb) ; fft4096(mode,cc) ;
                 fft4096(mode,dd) ; fft4096(mode,ee) ;                break;

      case 8192: fft8192(mode,aa) ; fft8192(mode,bb) ; fft8192(mode,cc) ;
                 fft8192(mode,dd) ; fft8192(mode,ee) ;                break;

      case 16384: fft16384(mode,aa) ; fft16384(mode,bb) ; fft16384(mode,cc) ;
                  fft16384(mode,dd) ; fft16384(mode,ee) ;             break;
#endif

      default:  csfft_cox_OMP(mode,M,aa) ;
                csfft_cox_OMP(mode,M,bb) ; csfft_cox_OMP(mode,M,cc) ;
                csfft_cox_OMP(mode,M,dd) ; csfft_cox_OMP(mode,M,ee) ; break ;
   }
   CFO_recX[ithr]-- ;

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
