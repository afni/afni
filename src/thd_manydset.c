/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*----------------------------------------------------------------
  Create an array of associated 3D datasets from an array of
  datablocks
------------------------------------------------------------------*/

THD_3dim_dataset_array *
   THD_array_3dim_from_block( THD_datablock_array *blk_arr )
{
   THD_3dim_dataset_array *dset_arr ;
   THD_3dim_dataset       *dset ;
   int id ;
   Boolean dset_ok = True , all_anat , all_func ;

ENTRY("THD_array_3dim_from_block") ;

   INIT_3DARR( dset_arr ) ;

   if( blk_arr == NULL || blk_arr->num <= 0 ) RETURN(dset_arr) ;

   for( id=0 ; id < blk_arr->num ; id++ ){

      dset = THD_3dim_from_block( blk_arr->ar[id] ) ;

      if( dset != NULL ) ADDTO_3DARR( dset_arr , dset ) ;
   }

   if( dset_arr->num <= 0 ) RETURN(dset_arr) ;

   /******************************************************/
   /*-- now, check the set of datasets for consistency --*/
   /******************************************************/

   /*-- 1.  Images should all be anatomy type or function type --*/

   all_anat = all_func = True ;
   for( id=0 ; id < dset_arr->num ; id++ ){
      dset      = dset_arr->ar[id] ;
      all_anat  = all_anat && ISANAT(dset) ;
      all_func  = all_func && ISFUNC(dset) ;
   }
   if( !all_anat && !all_func )
     WARNING_message("dataset %s: mixed ANAT and FUNC? in different views?",
                     DSET_HEADNAME(dset_arr->ar[0]) ) ;

   SORT_3DARR( dset_arr ) ;

   /*-- 2.  If all images are anat, nothing to do at this moment. --*/

   if( all_anat ){
   } /* end of dealing with all_anat case */

   /*-- 3.  If all images are func .... --*/

   if( all_func ){
#if 0
      THD_3dim_dataset *dset0 ;
      int jd ;

      /* check for anat parents (should all have one) */

      for( id=0 ; id < dset_arr->num ; id++ ){  /* check for anat parent */
         dset = dset_arr->ar[id] ;
         if( strlen(dset->anat_parent_name) == 0 )
            DSET_WARN("functional image has no anatomical parent!") ;
      }
#endif

   } /* end of dealing with all_func case */

   /*********************************************************/
   /*---------- if an error occurred, clean up -------------*/
   /*********************************************************/

   if( ! dset_ok ){

      /*-- delete data in subsidiary data structures --*/

      for( id=0 ; id < dset_arr->num ; id++ ){
         THD_delete_3dim_dataset( dset_arr->ar[id] , False ) ;
         myXtFree( dset_arr->ar[id] ) ;
      }

      FREE_3DARR( dset_arr ) ;
      INIT_3DARR( dset_arr ) ;  /* return a blank array */
   }

   /*-- at last!
        give the caller the list of (nearly) initialized datasets --*/

   RETURN(dset_arr) ;
}
