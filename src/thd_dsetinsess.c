/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"
#include "thd.h"


/*-----------------------------------------------------------------
   find a dataset with a given name in a session
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

         /*-- search anat list first --*/

         for( id=0 ; id < sess->num_anat ; id++ ){
            for( iv=FIRST_VIEW_TYPE ; iv <= LAST_VIEW_TYPE ; iv++ ){
               dset = sess->anat[id][iv] ;

               if( dset != NULL && strcmp(dset->self_name,target_name) == 0 ){
                  find.dset = dset ;
                  find.anat_index = id ; find.view_index = iv ; find.func_index = -1 ;
                  return find ;
               }
            }
         }

         /*-- search func list next --*/

         for( id=0 ; id < sess->num_func ; id++ ){
            for( iv=FIRST_VIEW_TYPE ; iv <= LAST_VIEW_TYPE ; iv++ ){
               dset = sess->func[id][iv] ;

               if( dset != NULL && strcmp(dset->self_name,target_name) == 0 ){
                  find.dset = dset ;
                  find.anat_index = -1 ; find.view_index = iv ; find.func_index = id ;
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

         /*-- search anat list first --*/

         for( id=0 ; id < sess->num_anat ; id++ ){
            for( iv=FIRST_VIEW_TYPE ; iv <= LAST_VIEW_TYPE ; iv++ ){
               dset = sess->anat[id][iv] ;

               if( dset != NULL && strcmp(DSET_PREFIX(dset),target_prefix) == 0 ){
                  find.dset = dset ;
                  find.anat_index = id ; find.view_index = iv ; find.func_index = -1 ;
                  return find ;
               }
            }
         }

         /*-- search func list next --*/

         for( id=0 ; id < sess->num_func ; id++ ){
            for( iv=FIRST_VIEW_TYPE ; iv <= LAST_VIEW_TYPE ; iv++ ){
               dset = sess->func[id][iv] ;

               if( dset != NULL && strcmp(DSET_PREFIX(dset),target_prefix) == 0 ){
                  find.dset = dset ;
                  find.anat_index = -1 ; find.view_index = iv ; find.func_index = id ;
                  return find ;
               }
            }
         }
      }
      break ;

#ifndef OMIT_DATASET_IDCODES
      /**** search for an idcode ****/

      case FIND_IDCODE:{
         MCW_idcode target_id = * ((MCW_idcode *) target) ;

         if( ISZERO_IDCODE(target_id) ){
            BADFIND(find) ; return find ;
         }

         /*-- search anat list first --*/

         for( id=0 ; id < sess->num_anat ; id++ ){
            for( iv=FIRST_VIEW_TYPE ; iv <= LAST_VIEW_TYPE ; iv++ ){
               dset = sess->anat[id][iv] ;

               if( dset != NULL && EQUIV_IDCODES(target_id,dset->idcode) ){
                  find.dset = dset ;
                  find.anat_index = id ; find.view_index = iv ; find.func_index = -1 ;
                  return find ;
               }
            }
         }

         /*-- search func list next --*/

         for( id=0 ; id < sess->num_func ; id++ ){
            for( iv=FIRST_VIEW_TYPE ; iv <= LAST_VIEW_TYPE ; iv++ ){
               dset = sess->func[id][iv] ;

               if( dset != NULL && EQUIV_IDCODES(target_id,dset->idcode) ){
                  find.dset = dset ;
                  find.anat_index = -1 ; find.view_index = iv ; find.func_index = id ;
                  return find ;
               }
            }
         }
      }
      break ;
#endif /* OMIT_DATASET_IDCODES */

   }  /* end of switch on find_type */

   /*-- fall thru --> not found --*/

   BADFIND(find) ; return find ;
}
