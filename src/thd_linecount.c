/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*---------------------------------------------------------------------------*/

int THD_linecount( char *  str )
{
   int nlin ;
   char * cpt ;

   if( str == NULL || str[0] == '\0' ) return 0 ;

   nlin = 0 ;
   for( cpt=str ; *cpt != '\0' ; cpt++ )
      if( *cpt == '\n' ) nlin++ ;

   if( *(cpt-1) != '\n' ) nlin++ ;

   return nlin ;
}
