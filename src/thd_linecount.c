#include "3ddata.h"
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
