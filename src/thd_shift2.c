/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/******************* Routines to shift two rows at a time ********************/

/*---------------------------------------------------------------------------
   Set the interpolation method for shifting:
   input is one of MRI_NN, MRI_LINEAR, MRI_CUBIC, or MRI_FOURIER.
-----------------------------------------------------------------------------*/

typedef void (*shift_func)(int,int,float,float *,float,float *) ;
static  shift_func shifter      = fft_shift2 ;
static  int        shift_method = MRI_FOURIER ;

void SHIFT_set_method( int mode )
{
   shift_method = mode ;
   switch( mode ){
      default:          shift_method = MRI_FOURIER ;  /* fall thru */
      case MRI_FOURIER: shifter = fft_shift2   ; break ;

      case MRI_LINEAR:  shifter = lin_shift2   ; break ;
      case MRI_CUBIC:   shifter = cub_shift2   ; break ;
      case MRI_QUINTIC: shifter = quint_shift2 ; break ;  /* Nov 1998 */
      case MRI_HEPTIC:  shifter = hept_shift2  ; break ;  /* Nov 1998 */

      case MRI_NN:      shifter = nn_shift2    ; break ;  /* experimental */
      case MRI_TSSHIFT: shifter = ts_shift2    ; break ;  /* Dec 1999 */
   }
   return ;
}

int SHIFT_get_method( void ){ return shift_method ; }

/*--------------------------------------------------------------------------
   The main entry point - note that g can be NULL, but f cannot.
---------------------------------------------------------------------------*/

void SHIFT_two_rows( int n, int nup, float af, float * f, float ag, float * g )
{
   shifter( n,nup,af,f,ag,g ) ; return ;
}

/*--------------------------------------------------------------------------
   Shift 2 rows at a time with the FFT:
     n   = length of a row
     nup = length to use for FFTs (power of 2 >= n)
     af  = shift for row f
     ag  = shift for row g
   Input and output arrays are f[n] and g[n].  (Note: g may be NULL.)
----------------------------------------------------------------------------*/

#define ZFILL
#define RECUR

void fft_shift2( int n, int nup, float af, float * f, float ag, float * g )
{
   static int nupold=0 , nuptop=0 ;
   static complex * row=NULL , * cf=NULL , * cg=NULL ;

   int ii , nby2=nup/2 , n21=nby2+1 ;
   complex fac , gac ;
   float sf , sg , dk ;
#ifdef RECUR
   complex csf , csg ;
#endif

ENTRY("fft_shift2") ;

   /* 15 Mar 2001: shift too big ==> return all zeros */

   if( (af < -n || af > n) && (ag < -n || ag > n) ){
      for( ii=0 ; ii < n ; ii++ ) f[ii] = g[ii] = 0.0 ;
      EXRETURN ;
   }

   /* make new memory for row storage? */

   if( nup > nuptop ){
      if( row != NULL ){ free(row) ; free(cf) ; free(cg) ; }
      row = (complex *) malloc( sizeof(complex) * nup ) ;
      cf  = (complex *) malloc( sizeof(complex) * n21 ) ;
      cg  = (complex *) malloc( sizeof(complex) * n21 ) ;
      nuptop = nup ;
   }

   /* FFT the pair of rows */

   if( g != NULL )
      for( ii=0 ; ii < n ; ii++ ){ row[ii].r = f[ii] ; row[ii].i = g[ii] ; }
   else
      for( ii=0 ; ii < n ; ii++ ){ row[ii].r = f[ii] ; row[ii].i = 0 ; }

#ifdef ZFILL
   for( ii=n ; ii < nup ; ii++ ){ row[ii].r = row[ii].i = 0.0 ; }
#else
   if( nup > n ){
      sf = 0.5 * (row[0].r + row[n-1].r) ; sg = 0.5 * (row[0].i + row[n-1].i) ;
      for( ii=n ; ii < nup ; ii++ ){ row[ii].r = sf ; row[ii].i = sg ; }
   }
#endif

   csfft_cox( -1 , nup , row ) ;

   /* untangle FFT coefficients from row into cf,cg */

   cf[0].r = 2.0 * row[0].r ; cf[0].i = 0.0 ;  /* twice too big */
   cg[0].r = 2.0 * row[0].i ; cg[0].i = 0.0 ;
   for( ii=1 ; ii < nby2 ; ii++ ){
      cf[ii].r =  row[ii].r + row[nup-ii].r ;
      cf[ii].i =  row[ii].i - row[nup-ii].i ;
      cg[ii].r =  row[ii].i + row[nup-ii].i ;
      cg[ii].i = -row[ii].r + row[nup-ii].r ;
   }
   cf[nby2].r = 2.0 * row[nby2].r ; cf[nby2].i = 0.0 ;
   cg[nby2].r = 2.0 * row[nby2].i ; cg[nby2].i = 0.0 ;

   /* phase shift both rows (cf,cg) */

   dk = (2.0*PI) / nup ;
   sf = -af * dk ; sg = -ag * dk ;

#ifdef RECUR
   csf = CEXPIT(sf) ; csg = CEXPIT(sg) ;
   fac.r = gac.r = 1.0 ;
   fac.i = gac.i = 0.0 ;
#endif

   for( ii=1 ; ii <= nby2 ; ii++ ){
#ifdef RECUR
      fac = CMULT( csf , fac ) ; cf[ii] = CMULT( fac , cf[ii] ) ;
      gac = CMULT( csg , gac ) ; cg[ii] = CMULT( gac , cg[ii] ) ;
#else
      fac = CEXPIT(ii*sf) ; cf[ii] = CMULT( fac , cf[ii] ) ;
      gac = CEXPIT(ii*sg) ; cg[ii] = CMULT( gac , cg[ii] ) ;
#endif
   }
   cf[nby2].i = 0.0 ; cg[nby2].i = 0.0 ;

   /* retangle the coefficients from 2 rows */

   row[0].r = cf[0].r ; row[0].i = cg[0].r ;
   for( ii=1 ; ii < nby2 ; ii++ ){
      row[ii].r     =  cf[ii].r - cg[ii].i ;
      row[ii].i     =  cf[ii].i + cg[ii].r ;
      row[nup-ii].r =  cf[ii].r + cg[ii].i ;
      row[nup-ii].i = -cf[ii].i + cg[ii].r ;
   }
   row[nby2].r = cf[nby2].r ;
   row[nby2].i = cg[nby2].r ;

   /* inverse FFT and store back in output arrays */

   csfft_cox( 1 , nup , row ) ;

   sf = 0.5 / nup ;              /* 0.5 to allow for twice too big above */

   if( g != NULL )
      for( ii=0; ii < n; ii++ ){ f[ii] = sf*row[ii].r; g[ii] = sf*row[ii].i; }
   else
      for( ii=0; ii < n; ii++ ){ f[ii] = sf*row[ii].r; }

   EXRETURN ;
}

/*--------------------------------------------------------------------------
  Some stuff needed for polynomial interpolation
----------------------------------------------------------------------------*/

static int    nlcbuf = 0 ;     /* workspace */
static float * lcbuf = NULL ;

   /* f[i], but inside the legal range */

#ifdef ZFILL
#  define FINS(i) ( ((i)<0 || (i)>=n) ? 0.0 : f[(i)] )
#else
#  define FINS(i) ( ((i)<0) ? f[0] : ((i)>=n) ? f[n-1] : f[(i)] )
#endif

#define SEPARATE_FINS

/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
  Shift 1 row with with heptic Lagrange polynomial interpolation [Nov 1998].
  Note that heptic interpolation is about the same as Hamming-weighted
  3-sidelobe sinc interpolation .
-----------------------------------------------------------------------------*/

   /* seventh order polynomials */

#define S_M3(x) (x*(x*x-1.0)*(x*x-4.0)*(x-3.0)*(4.0-x)*0.0001984126984)
#define S_M2(x) (x*(x*x-1.0)*(x-2.0)*(x*x-9.0)*(x-4.0)*0.001388888889)
#define S_M1(x) (x*(x-1.0)*(x*x-4.0)*(x*x-9.0)*(4.0-x)*0.004166666667)
#define S_00(x) ((x*x-1.0)*(x*x-4.0)*(x*x-9.0)*(x-4.0)*0.006944444444)
#define S_P1(x) (x*(x+1.0)*(x*x-4.0)*(x*x-9.0)*(4.0-x)*0.006944444444)
#define S_P2(x) (x*(x*x-1.0)*(x+2.0)*(x*x-9.0)*(x-4.0)*0.004166666667)
#define S_P3(x) (x*(x*x-1.0)*(x*x-4.0)*(x+3.0)*(4.0-x)*0.001388888889)
#define S_P4(x) (x*(x*x-1.0)*(x*x-4.0)*(x*x-9.0)*0.0001984126984)

void hept_shift( int n , float af , float * f )
{
   int   ii , ia , ix ;
   float  wt_m1,wt_00,wt_p1,wt_p2 , aa , wt_m2,wt_p3,wt_m3,wt_p4;
#ifdef SEPARATE_FINS
   int ibot,itop ;
#endif

ENTRY("hept_shift") ;

   af = -af ; ia = (int) af ; if( af < 0 ) ia-- ;  /* ia = floor */

   /* 15 Mar 2001: if shift is too large, return all zeros */

   if( ia <= -n || ia >= n ){
      for( ii=0 ; ii < n ; ii++ ) f[ii] = 0.0 ;
      EXRETURN ;
   }

   aa = af - ia ;
   wt_m1 = S_M1(aa) ; wt_00 = S_00(aa) ;
   wt_p1 = S_P1(aa) ; wt_p2 = S_P2(aa) ;
   wt_m2 = S_M2(aa) ; wt_p3 = S_P3(aa) ;
   wt_m3 = S_M3(aa) ; wt_p4 = S_P4(aa) ;

   if( n > nlcbuf ){
      if( lcbuf != NULL ) free(lcbuf) ;
      lcbuf  = (float *) malloc( sizeof(float) * n ) ;
      nlcbuf = n ;
   }

#ifdef SEPARATE_FINS
   ibot = 3-ia ;   if( ibot < 0   ) ibot = 0 ;
   itop = n-5-ia ; if( itop > n-1 ) itop = n-1 ;

   for( ii=ibot ; ii <= itop ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] =  wt_m2 * f[ix-2] + wt_m1 * f[ix-1] + wt_00 * f[ix]
                 + wt_p1 * f[ix+1] + wt_p2 * f[ix+2] + wt_p3 * f[ix+3]
                 + wt_m3 * f[ix-3] + wt_p4 * f[ix+4] ;
   }

   if( ibot > n ) ibot = n ; /* 15 Mar 2001 */
   for( ii=0 ; ii < ibot ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] =  wt_m2 * FINS(ix-2) + wt_m1 * FINS(ix-1) + wt_00 * FINS(ix)
                 + wt_p1 * FINS(ix+1) + wt_p2 * FINS(ix+2) + wt_p3 * FINS(ix+3)
                 + wt_m3 * FINS(ix-3) + wt_p4 * FINS(ix+4) ;
   }

   if( itop < 0 ) itop = -1 ; /* 15 Mar 2001 */
   for( ii=itop+1 ; ii < n ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] =  wt_m2 * FINS(ix-2) + wt_m1 * FINS(ix-1) + wt_00 * FINS(ix)
                 + wt_p1 * FINS(ix+1) + wt_p2 * FINS(ix+2) + wt_p3 * FINS(ix+3)
                 + wt_m3 * FINS(ix-3) + wt_p4 * FINS(ix+4) ;
   }
#else /* not SEPARATE_FINS */
   for( ii=0 ; ii < n ; ii++ ){
      ix = ii + ia ;
      if( ix > 1 && ix < n-3 )
         lcbuf[ii] =  wt_m2 * f[ix-2] + wt_m1 * f[ix-1] + wt_00 * f[ix]
                    + wt_p1 * f[ix+1] + wt_p2 * f[ix+2] + wt_p3 * f[ix+3]
                    + wt_m3 * f[ix-3] + wt_p4 * f[ix+4] ;
      else
         lcbuf[ii] =  wt_m2 * FINS(ix-2) + wt_m1 * FINS(ix-1) + wt_00 * FINS(ix)
                    + wt_p1 * FINS(ix+1) + wt_p2 * FINS(ix+2) + wt_p3 * FINS(ix+3)
                    + wt_m3 * FINS(ix-3) + wt_p4 * FINS(ix+4) ;
   }
#endif /* SEPARATE_FINS */

   memcpy( f , lcbuf , sizeof(float)*n ) ;
   EXRETURN ;
}

void hept_shift2( int n, int nup, float af, float * f, float ag, float * g )
{
                   hept_shift( n , af , f ) ;
   if( g != NULL ) hept_shift( n , ag , g ) ;
   return ;
}

/*---------------------------------------------------------------------------
  Shift 1 row with with quintic Lagrange polynomial interpolation [Nov 1998].
  Note that quintic interpolation is about the same as Hamming-weighted
  2-sidelobe sinc interpolation .
-----------------------------------------------------------------------------*/

   /* quintic interpolation polynomials (Lagrange) */

#define Q_M2(x)  (x*(x*x-1.0)*(2.0-x)*(x-3.0)*0.008333333)
#define Q_M1(x)  (x*(x*x-4.0)*(x-1.0)*(x-3.0)*0.041666667)
#define Q_00(x)  ((x*x-4.0)*(x*x-1.0)*(3.0-x)*0.083333333)
#define Q_P1(x)  (x*(x*x-4.0)*(x+1.0)*(x-3.0)*0.083333333)
#define Q_P2(x)  (x*(x*x-1.0)*(x+2.0)*(3.0-x)*0.041666667)
#define Q_P3(x)  (x*(x*x-1.0)*(x*x-4.0)*0.008333333)

void quint_shift( int n , float af , float * f )
{
   int   ii , ia , ix ;
   float  wt_m1 , wt_00 , wt_p1 , wt_p2 , aa , wt_m2 , wt_p3 ;
#ifdef SEPARATE_FINS
   int ibot,itop ;
#endif

ENTRY("quint_shift") ;

   af = -af ; ia = (int) af ; if( af < 0 ) ia-- ;  /* ia = floor */

   /* 15 Mar 2001: if shift is too large, return all zeros */

   if( ia <= -n || ia >= n ){
      for( ii=0 ; ii < n ; ii++ ) f[ii] = 0.0 ;
      EXRETURN ;
   }

   aa = af - ia ;
   wt_m1 = Q_M1(aa) ; wt_00 = Q_00(aa) ;
   wt_p1 = Q_P1(aa) ; wt_p2 = Q_P2(aa) ;
   wt_m2 = Q_M2(aa) ; wt_p3 = Q_P3(aa) ;

   if( n > nlcbuf ){
      if( lcbuf != NULL ) free(lcbuf) ;
      lcbuf  = (float *) malloc( sizeof(float) * n ) ;
      nlcbuf = n ;
   }

#ifdef SEPARATE_FINS
   ibot = 2-ia ;   if( ibot < 0   ) ibot = 0 ;
   itop = n-4-ia ; if( itop > n-1 ) itop = n-1 ;

   for( ii=ibot ; ii <= itop ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] =  wt_m2 * f[ix-2] + wt_m1 * f[ix-1] + wt_00 * f[ix]
                 + wt_p1 * f[ix+1] + wt_p2 * f[ix+2] + wt_p3 * f[ix+3] ;
   }

   if( ibot > n ) ibot = n ; /* 15 Mar 2001 */
   for( ii=0 ; ii < ibot ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] =  wt_m2 * FINS(ix-2) + wt_m1 * FINS(ix-1) + wt_00 * FINS(ix)
                 + wt_p1 * FINS(ix+1) + wt_p2 * FINS(ix+2) + wt_p3 * FINS(ix+3) ;
   }

   if( itop < 0 ) itop = -1 ; /* 15 Mar 2001 */
   for( ii=itop+1 ; ii < n ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] =  wt_m2 * FINS(ix-2) + wt_m1 * FINS(ix-1) + wt_00 * FINS(ix)
                 + wt_p1 * FINS(ix+1) + wt_p2 * FINS(ix+2) + wt_p3 * FINS(ix+3) ;
   }
#else /* not SEPARATE_FINS */
   for( ii=0 ; ii < n ; ii++ ){
      ix = ii + ia ;
      if( ix > 1 && ix < n-3 )
         lcbuf[ii] =  wt_m2 * f[ix-2] + wt_m1 * f[ix-1] + wt_00 * f[ix]
                    + wt_p1 * f[ix+1] + wt_p2 * f[ix+2] + wt_p3 * f[ix+3] ;
      else
         lcbuf[ii] =  wt_m2 * FINS(ix-2) + wt_m1 * FINS(ix-1) + wt_00 * FINS(ix)
                    + wt_p1 * FINS(ix+1) + wt_p2 * FINS(ix+2) + wt_p3 * FINS(ix+3) ;
   }
#endif /* SEPARATE_FINS */

   memcpy( f , lcbuf , sizeof(float)*n ) ;
   EXRETURN ;
}

void quint_shift2( int n, int nup, float af, float * f, float ag, float * g )
{
                   quint_shift( n , af , f ) ;
   if( g != NULL ) quint_shift( n , ag , g ) ;
   return ;
}

/*---------------------------------------------------------------------------
  Shift 1 row with with cubic interpolation
-----------------------------------------------------------------------------*/

   /* cubic interpolation polynomials */

#define P_M1(x)  ((x)*(1.0-(x))*((x)-2.0)*0.1666667)
#define P_00(x)  (((x)+1.0)*((x)-1.0)*((x)-2.0)*0.5)
#define P_P1(x)  ((x)*((x)+1.0)*(2.0-(x))*0.5)
#define P_P2(x)  ((x)*((x)+1.0)*((x)-1.0)*0.1666667)

void cub_shift( int n , float af , float * f )
{
   int   ii , ia , ix ;
   float  wt_m1 , wt_00 , wt_p1 , wt_p2 , aa ;
#ifdef SEPARATE_FINS
   int ibot,itop ;
#endif

ENTRY("cub_shift") ;

   af = -af ; ia = (int) af ; if( af < 0 ) ia-- ;  /* ia = floor */

   /* 15 Mar 2001: if shift is too large, return all zeros */

   if( ia <= -n || ia >= n ){
      for( ii=0 ; ii < n ; ii++ ) f[ii] = 0.0 ;
      EXRETURN ;
   }

   aa = af - ia ;
   wt_m1 = P_M1(aa) ; wt_00 = P_00(aa) ;
   wt_p1 = P_P1(aa) ; wt_p2 = P_P2(aa) ;

   if( n > nlcbuf ){
      if( lcbuf != NULL ) free(lcbuf) ;
      lcbuf  = (float *) malloc( sizeof(float) * n ) ;
      nlcbuf = n ;
   }

#ifdef SEPARATE_FINS
   ibot = 1-ia ;   if( ibot < 0   ) ibot = 0 ;
   itop = n-3-ia ; if( itop > n-1 ) itop = n-1 ;

   for( ii=ibot ; ii <= itop ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] =  wt_m1 * f[ix-1] + wt_00 * f[ix]
                 + wt_p1 * f[ix+1] + wt_p2 * f[ix+2] ;
   }

   if( ibot > n ) ibot = n ; /* 15 Mar 2001 */
   for( ii=0 ; ii < ibot ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] =  wt_m1 * FINS(ix-1) + wt_00 * FINS(ix)
                 + wt_p1 * FINS(ix+1) + wt_p2 * FINS(ix+2) ;
   }

   if( itop < 0 ) itop = -1 ; /* 15 Mar 2001 */
   for( ii=itop+1 ; ii < n ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] =  wt_m1 * FINS(ix-1) + wt_00 * FINS(ix)
                 + wt_p1 * FINS(ix+1) + wt_p2 * FINS(ix+2) ;
   }
#else /* not SEPARATE_FINS */
   for( ii=0 ; ii < n ; ii++ ){
      ix = ii + ia ;
      if( ix > 0 && ix < n-2 )
         lcbuf[ii] =  wt_m1 * f[ix-1] + wt_00 * f[ix]
                    + wt_p1 * f[ix+1] + wt_p2 * f[ix+2] ;
      else
         lcbuf[ii] =  wt_m1 * FINS(ix-1) + wt_00 * FINS(ix)
                    + wt_p1 * FINS(ix+1) + wt_p2 * FINS(ix+2) ;
   }
#endif /* SEPARATE_FINS */

   memcpy( f , lcbuf , sizeof(float)*n ) ;
   EXRETURN ;
}

void cub_shift2( int n, int nup, float af, float * f, float ag, float * g )
{
                   cub_shift( n , af , f ) ;
   if( g != NULL ) cub_shift( n , ag , g ) ;
   return ;
}

/*---------------------------------------------------------------------------
   Shift one row with linear interpolation
-----------------------------------------------------------------------------*/

void lin_shift( int n , float af , float * f )
{
   int   ii , ia , ix ;
   float  wt_00 , wt_p1 , aa ;
#ifdef SEPARATE_FINS
   int ibot,itop ;
#endif

ENTRY("lin_shift") ;

   af = -af ; ia = (int) af ; if( af < 0 ) ia-- ;  /* ia = floor */
   aa = af - ia ;
   wt_00 = 1.0 - aa ; wt_p1 = aa ;  /* linear interpolation weights */

   /* 15 Mar 2001: if shift is too large, return all zeros */

   if( ia <= -n || ia >= n ){
      for( ii=0 ; ii < n ; ii++ ) f[ii] = 0.0 ;
      EXRETURN ;
   }

   if( n > nlcbuf ){
      if( lcbuf != NULL ) free(lcbuf) ;
      lcbuf  = (float *) malloc( sizeof(float) * n ) ;
      nlcbuf = n ;
   }

#ifdef SEPARATE_FINS
   ibot = -ia  ;   if( ibot < 0   ) ibot = 0 ;
   itop = n-2-ia ; if( itop > n-1 ) itop = n-1 ;

#if 0
if(PRINT_TRACING){
  char str[256]; sprintf(str,"n=%d ia=%d ibot=%d itop=%d",n,ia,ibot,itop); STATUS(str);
}
#endif

   for( ii=ibot ; ii <= itop ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] =  wt_00 * f[ix] + wt_p1 * f[ix+1] ;
   }

   if( ibot > n ) ibot = n ; /* 15 Mar 2001 */
   for( ii=0 ; ii < ibot ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] =  wt_00 * FINS(ix) + wt_p1 * FINS(ix+1) ;
   }

   if( itop < 0 ) itop = -1 ; /* 15 Mar 2001 */
   for( ii=itop+1 ; ii < n ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] =  wt_00 * FINS(ix) + wt_p1 * FINS(ix+1) ;
   }
#else
   for( ii=0 ; ii < n ; ii++ ){
      ix = ii + ia ;
      if( ix >= 0 && ix < n-1 )
         lcbuf[ii] =  wt_00 * f[ix] + wt_p1 * f[ix+1] ;
      else
         lcbuf[ii] =  wt_00 * FINS(ix) + wt_p1 * FINS(ix+1) ;
   }
#endif /* SEPARATE_FINS */

   memcpy( f , lcbuf , sizeof(float)*n ) ;
   EXRETURN ;
}

void lin_shift2( int n, int nup, float af, float * f, float ag, float * g )
{
                   lin_shift( n , af , f ) ;
   if( g != NULL ) lin_shift( n , ag , g ) ;
   return ;
}

/*--------------------------------------------------------------------------
  This is for experimental purposes only -- DO NOT USE!
----------------------------------------------------------------------------*/

void nn_shift( int n , float af , float * f )
{
   int   ii , ia , ix ;

ENTRY("nn_shift") ;

   af = -af ; ia = (int) af ; if( af < 0 ) ia-- ;  /* ia = floor */

   /* 15 Mar 2001: if shift is too large, return all zeros */

   if( ia <= -n || ia >= n ){
      for( ii=0 ; ii < n ; ii++ ) f[ii] = 0.0 ;
      EXRETURN ;
   }

   if( n > nlcbuf ){
      if( lcbuf != NULL ) free(lcbuf) ;
      lcbuf  = (float *) malloc( sizeof(float) * n ) ;
      nlcbuf = n ;
   }

   for( ii=0 ; ii < n ; ii++ ){
      ix = ii + ia ;
      lcbuf[ii] = FINS(ix) ;
   }

   memcpy( f , lcbuf , sizeof(float)*n ) ;
   EXRETURN ;
}

void nn_shift2( int n, int nup, float af, float * f, float ag, float * g )
{
                   nn_shift( n , af , f ) ;
   if( g != NULL ) nn_shift( n , ag , g ) ;
   return ;
}

/*---------------------------------------------------------------------------
   More experiments: two-step interpolation
-----------------------------------------------------------------------------*/

void ts_shift( int n , float af , float * f )
{
   register int ii , ia , ix ;
   float aa ;
   int ibot,itop ;

   af = -af ; ia = (int) af ; if( af < 0 ) ia-- ;  /* ia = floor */

   /* 15 Mar 2001: if shift is too large, return all zeros */

   if( ia <= -n || ia >= n ){
      for( ii=0 ; ii < n ; ii++ ) f[ii] = 0.0 ;
      EXRETURN ;
   }

   aa = af - ia ;

   if( n > nlcbuf ){
      if( lcbuf != NULL ) free(lcbuf) ;
      lcbuf  = (float *) malloc( sizeof(float) * n ) ;
      nlcbuf = n ;
   }

   ibot = -ia  ;   if( ibot < 0   ) ibot = 0 ;
   itop = n-2-ia ; if( itop > n-1 ) itop = n-1 ;

   if( aa < 0.30 ){
      memcpy( lcbuf+ibot, f+(ibot+ia)  , (itop+1-ibot)*sizeof(float) );
      for( ii=0 ; ii < ibot ; ii++ ){
         ix = ii + ia ; lcbuf[ii] = FINS(ix) ;
      }
      for( ii=itop+1 ; ii < n ; ii++ ){
         ix = ii + ia ; lcbuf[ii] = FINS(ix) ;
      }
   }

   else if( aa > 0.70 ){
      memcpy( lcbuf+ibot, f+(ibot+1+ia), (itop+1-ibot)*sizeof(float) );
      for( ii=0 ; ii < ibot ; ii++ ){
         ix = ii + ia ; lcbuf[ii] = FINS(ix+1) ;
      }
      for( ii=itop+1 ; ii < n ; ii++ ){
         ix = ii + ia ; lcbuf[ii] = FINS(ix+1) ;
      }

   } else {
      for( ii=ibot ; ii <= itop ; ii++ ){
         ix = ii + ia ; lcbuf[ii] =  0.5*( f[ix] + f[ix+1] ) ;
      }
      if( ibot > n ) ibot = n ; /* 15 Mar 2001 */
      for( ii=0 ; ii < ibot ; ii++ ){
         ix = ii + ia ; lcbuf[ii] =  0.5*( FINS(ix) + FINS(ix+1) ) ;
      }
      if( itop < 0 ) itop = -1 ; /* 15 Mar 2001 */
      for( ii=itop+1 ; ii < n ; ii++ ){
         ix = ii + ia ; lcbuf[ii] =  0.5*( FINS(ix) + FINS(ix+1) ) ;
      }
   }
   memcpy( f , lcbuf , sizeof(float)*n ) ;
   return ;
}

void ts_shift2( int n, int nup, float af, float * f, float ag, float * g )
{
                   ts_shift( n , af , f ) ;
   if( g != NULL ) ts_shift( n , ag , g ) ;
   return ;
}
