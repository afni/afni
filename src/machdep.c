#include "mrilib.h"

#ifdef LINUX
# include <malloc.h>
#endif

/*--------------------------------------------------------------------
   Code to provide runtime fixups for various machines
   (things that can't be fixed by declarations in machdep.h).
   This should be called at the start of every program.
----------------------------------------------------------------------*/

void machdep()
{

#  if defined(LINUX) && defined(M_MMAP_MAX)
      mallopt( M_MMAP_MAX , 0 ) ;  /* disable mmap() in malloc() */
#  endif

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
