/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
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
