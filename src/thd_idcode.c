/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include <time.h>
#include "thd.h"

/*-----------------------------------------------------------------------*/
/*! Routine to return a (hopefully) unique ID code to be used to identify
    a dataset to other datasets.
-------------------------------------------------------------------------*/

MCW_idcode MCW_new_idcode(void)
{
   MCW_idcode newid ;
   time_t tnow ;
   int nn ;

   UNIQ_idcode_fill( newid.str ) ;  /* thd_md5.c */

   tnow = time(NULL) ;
   MCW_strncpy( newid.date , ctime(&tnow) , MCW_IDDATE ) ;
   nn = strlen(newid.date) ;
   if( nn > 0 && newid.date[nn-1] == '\n' ) newid.date[nn-1] = '\0' ;

   return newid ;
}

/*-----------------------------------------------------------------------*/
/*! Replace the string part of the ID code with a deterministic hash.
-------------------------------------------------------------------------*/

void MCW_hash_idcode( char *hstring , THD_3dim_dataset *dset ) /* 06 May 2005 */
{
   char *hhh , *rp , rpath[RPMAX] ;
   if( hstring == NULL || *hstring == '\0' && !ISVALID_DSET(dset) ) return ;
   rp = realpath( hstring , rpath ) ; if( rp == NULL ) rp = hstring ;
   hhh = UNIQ_hashcode(rp) ;
   MCW_strncpy( dset->idcode.str+3, hhh+3, MCW_IDSIZE-3 ); free((void *)hhh);
   return ;
}
