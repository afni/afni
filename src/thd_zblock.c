#include "3ddata.h"
#include "thd.h"


/*-----------------------------------------------------------------
   utilities for writing char arrays to disk
   (replace 0's with ZBLOCK's, and vice-versa)
-------------------------------------------------------------------*/

void THD_zblock( int nch , char * ch )
{
   int ii ;
   if( nch <= 0 ) return ;

   for( ii=0 ; ii < nch ; ii++ ){
      if( ch[ii] == ZBLOCK ) ch[ii] = '*'    ;
      if( ch[ii] == '\0'   ) ch[ii] = ZBLOCK ;
   }
}

void THD_unzblock( int nch , char * ch )
{
   int ii ;
   if( nch <= 0 ) return ;

   for( ii=0 ; ii < nch ; ii++ )
      if( ch[ii] == ZBLOCK ) ch[ii] = '\0' ;

   ch[nch-1] = '\0' ;
}
