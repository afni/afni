/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include <math.h>
#include <stdlib.h>

#if defined(isnan) && defined(isfinite)
# define IS_NAN(x)    isnan(x)
# define IS_FINITE(x) isfinite(x)

#else

# if defined(SUN) || defined(SOLARIS) || defined(SGI)
#  include <ieeefp.h>
#  define USE_ISNANF
# endif

# if defined(HP) || defined(OSF1) || defined(LINUX)
#  define USE_ISNANF
#  define USE_FINITEF
# endif
#endif

#ifndef IS_NAN
# ifdef USE_ISNANF
#  define IS_NAN(x) isnanf(x)
# else
#  define IS_NAN(x) isnan(x)
# endif
#endif

#ifndef IS_FINITE
# ifdef USE_FINITEF
#  define IS_FINITE(x) finitef(x)
# else
#  define IS_FINITE(x) finite(x)
# endif
#endif

/*---------------------------------------------------------------------
   Scan an array of floats for illegal values, replacing them with 0.
   Return the number of illegal values found.
-----------------------------------------------------------------------*/

int thd_floatscan( int nbuf , float * fbuf )
{
   int ii , nerr ;

   if( nbuf <= 0 || fbuf == NULL ) return 0 ;

   for( nerr=ii=0 ; ii < nbuf ; ii++ ){
      if( ! IS_FINITE(fbuf[ii]) ){ fbuf[ii] = 0.0 ; nerr++ ; }
   }

   return nerr ;
}

typedef struct complex { float r , i ; } complex ;

int thd_complexscan( int nbuf , complex * cbuf )
{
   int ii , nerr ;

   if( nbuf <= 0 || cbuf == NULL ) return 0 ;

   for( nerr=ii=0 ; ii < nbuf ; ii++ ){
      if( ! IS_FINITE(cbuf[ii].r) ){ cbuf[ii].r = 0.0 ; nerr++ ; }
      if( ! IS_FINITE(cbuf[ii].i) ){ cbuf[ii].i = 0.0 ; nerr++ ; }
   }

   return nerr ;
}
