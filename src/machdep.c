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
