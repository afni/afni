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
