/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include <math.h>
#include <stdlib.h>

#if 1
#  define IS_GOOD_FLOAT(x) finite(x)
#else
#  define IS_GOOD_FLOAT(x) isnan(x)
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
      if( !IS_GOOD_FLOAT(fbuf[ii]) ){ fbuf[ii] = 0.0 ; nerr++ ; }
   }

   return nerr ;
}

typedef struct complex { float r , i ; } complex ;

int thd_complexscan( int nbuf , complex * cbuf )
{
   int ii , nerr ;

   if( nbuf <= 0 || cbuf == NULL ) return 0 ;

   for( nerr=ii=0 ; ii < nbuf ; ii++ ){
      if( !IS_GOOD_FLOAT(cbuf[ii].r) ){ cbuf[ii].r = 0.0 ; nerr++ ; }
      if( !IS_GOOD_FLOAT(cbuf[ii].i) ){ cbuf[ii].i = 0.0 ; nerr++ ; }
   }

   return nerr ;
}
