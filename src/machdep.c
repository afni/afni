#define _MCW_MALLOC_HEADER_
#include "mrilib.h"

#if defined(LINUX)
# include <malloc.h>
#endif

/*--------------------------------------------------------------------
   Code to provide runtime fixups for various machines
   (things that can't be fixed by declarations in machdep.h).
   This should be called at the start of every program.
----------------------------------------------------------------------*/

void machdep()
{
   /*-- force use of mcw_malloc.c functions - 05 Nov 2001 --*/

#ifdef USING_MCW_MALLOC
   if( AFNI_yesenv("AFNI_FORCE_MCW_MALLOC") ) enable_mcw_malloc();
#endif

   /*-- disable mmap() in malloc() [21 Aug 2002: mostly] --*/

#if defined(LINUX) && defined(M_MMAP_MAX)
   mallopt( M_MMAP_MAX , 1 ) ;
#endif

}

/*-------------------------------------------------------------------
  To use the random()/srandom() library instead of srand48, do
    #define USE_RANDOM
  in machdep.h for your machine -- RWCox - 04 Sep 2001
---------------------------------------------------------------------*/
#ifdef USE_RANDOM
void srand48( long int s ){ srandom((unsigned int) s); }

double drand48(void){ return (((double)random())/LONG_MAX); }

long int lrand48(void){ return random(); }

long int mrand48(void)       /* need 32 bits but random only gives 31 */
{ register long i , j , k ;
  i = random() ; j = random() ;
  k = ((i & 0xFFFF)<<16) | (j & 0xFFFF) ; return k ;
}
#endif /* USE_RANDOM */

/*-------------------------------------------------------------------
  If the system doesn't provide this function for some reason ...
---------------------------------------------------------------------*/

#ifdef NEED_XSETLOCALE
#include <locale.h>
char * _Xsetlocale( int category, const char * locale)
{ return setlocale(category,locale) ; }
#endif

/*----- 09 Apr 2002 -----*/

#ifdef NEED_NL_LANGINFO
char * nl_langinfo(){ return "ISO-8859-1"; }
#endif

#ifdef NO_GAMMA
/*-------------------------------------------------------------------*/
/* If the system doesn't provide lgamma() for some reason.
---------------------------------------------------------------------*/

/** log of gamma, for argument between 1 and 2 **/

static double gamma_12( double y )
{
   double x , g ;
   x = y - 1.0 ;
   g = ((((((( 0.035868343 * x - 0.193527818 ) * x
                               + 0.482199394 ) * x
                               - 0.756704078 ) * x
                               + 0.918206857 ) * x
                               - 0.897056937 ) * x
                               + 0.988205891 ) * x
                               - 0.577191652 ) * x + 1.0 ;
   return log(g) ;
}

/** asymptotic expansion of ln(gamma(x)) for large positive x **/

#define LNSQRT2PI 0.918938533204672  /* ln(sqrt(2*PI)) */

static double gamma_asympt(double x)
{
   double sum ;

   sum = (x-0.5)*log(x) - x + LNSQRT2PI + 1.0/(12.0*x) - 1./(360.0*x*x*x) ;
   return sum ;
}
/** log of gamma, argument positive (not very efficient!) **/

double lgamma( double x )
{
   double w , g ;

   if( x <= 0.0 ){
      fprintf(stderr,"Internal gamma: argument %g <= 0\a\n",x) ;
      return 0.0 ;
   }

   if( x <  1.0 ) return gamma_12( x+1.0 ) - log(x) ;
   if( x <= 2.0 ) return gamma_12( x ) ;
   if( x >= 6.0 ) return gamma_asympt(x) ;

   g = 0 ; w = x ;
   while( w > 2.0 ){
      w -= 1.0 ; g += log(w) ;
   }
   return ( gamma_12(w) + g ) ;
}
#endif  /* NO_GAMMA */
