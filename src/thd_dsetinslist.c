/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include "thd.h"


/*-----------------------------------------------------------------
   find a dataset with a given name in a bunch of sessions,
   with the indicated session getting first priority
-------------------------------------------------------------------*/

THD_slist_find THD_dset_in_sessionlist( int find_type , void * target ,
                                        THD_sessionlist * ssl , int iss )
{
   int jss ;
   THD_slist_find find ;

   /*-- sanity check --*/

   if( ! ISVALID_SESSIONLIST(ssl) || ssl->num_sess <= 0 ){
      BADFIND(find) ; return find ;
   }

   /* search session # iss first */

   if( iss >=0 && iss < ssl->num_sess ){
      find = THD_dset_in_session( find_type,target , ssl->ssar[iss] ) ;
      if( find.dset != NULL ){ find.sess_index = iss ; return find ; }
   }

   /* search everybody else */

   for( jss=0 ; jss < ssl->num_sess ; jss++ ){
      if( jss == iss ) continue ;
      find = THD_dset_in_session( find_type,target , ssl->ssar[jss] ) ;
      if( find.dset != NULL ){ find.sess_index = jss ; return find ; }
   }

   return find ;
}
