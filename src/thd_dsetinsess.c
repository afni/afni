/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*-----------------------------------------------------------------
   Find a dataset with a given name in a session
   [28 Jul 2003] Modified for new THD_session struct.
-------------------------------------------------------------------*/

THD_slist_find THD_dset_in_session( int find_type , void * target ,
                                    THD_session * sess  )
{
   int id , iv , im ;
   THD_3dim_dataset * dset ;
   THD_slist_find find ;

   /*-- sanity check --*/

   if( ! ISVALID_SESSION(sess) || target == NULL ){
      BADFIND(find) ; return find ;
   }

   switch( find_type ){

      /**** search for a name ****/

      case FIND_NAME:{
         char * target_name = (char *) target ;
         if( strlen(target_name) == 0 ){
            BADFIND(find) ; return find ;
         }

         for( id=0 ; id < sess->num_dsset ; id++ ){
            for( iv=FIRST_VIEW_TYPE ; iv <= LAST_VIEW_TYPE ; iv++ ){
               dset = sess->dsset[id][iv] ;

               if( dset != NULL && strcmp(dset->self_name,target_name) == 0 ){
                  find.dset = dset ; find.dset_index = id ; find.view_index = iv ;
                  return find ;
               }
            }
         }
      }
      break ;

      /**** search for a prefix ****/

      case FIND_PREFIX:{
         char * target_prefix = (char *) target ;
         if( strlen(target_prefix) == 0 ){
            BADFIND(find) ; return find ;
         }

         for( id=0 ; id < sess->num_dsset ; id++ ){
            for( iv=FIRST_VIEW_TYPE ; iv <= LAST_VIEW_TYPE ; iv++ ){
               dset = sess->dsset[id][iv] ;

               if( dset != NULL && strcmp(DSET_PREFIX(dset),target_prefix) == 0 ){
                  find.dset = dset ; find.dset_index = id ; find.view_index = iv ;
                  return find ;
               }
            }
         }
      }
      break ;

      /**** search for an idcode ****/

      case FIND_IDCODE:{
         MCW_idcode target_id = * ((MCW_idcode *) target) ;

         if( ISZERO_IDCODE(target_id) ){
            BADFIND(find) ; return find ;
         }

         for( id=0 ; id < sess->num_dsset ; id++ ){
            for( iv=FIRST_VIEW_TYPE ; iv <= LAST_VIEW_TYPE ; iv++ ){
               dset = sess->dsset[id][iv] ;

               if( dset != NULL && EQUIV_IDCODES(target_id,dset->idcode) ){
                  find.dset = dset ; find.dset_index = id ; find.view_index = iv ;
                  return find ;
               }
            }
         }
      }
      break ;

   }  /* end of switch on find_type */

   /*-- fall thru --> not found --*/

   BADFIND(find) ; return find ;
}
