#include "3ddata.h"
#include "thd.h"


/*----------------------------------------------------------------
  Create an array of associated 3D datasets from an array of
  datablocks
------------------------------------------------------------------*/

THD_3dim_dataset_array *
   THD_array_3dim_from_block( THD_datablock_array * blk_arr )
{
   THD_3dim_dataset_array * dset_arr ;
   THD_3dim_dataset *       dset ;
   int id ;
   Boolean dset_ok = True , all_anat , all_func ;

   INIT_3DARR( dset_arr ) ;

   if( blk_arr == NULL || blk_arr->num <= 0 ) return dset_arr ;

   for( id=0 ; id < blk_arr->num ; id++ ){

      dset = THD_3dim_from_block( blk_arr->ar[id] ) ;

      if( dset != NULL ) ADDTO_3DARR( dset_arr , dset ) ;
   }

   if( dset_arr->num <= 0 ) return dset_arr ;

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
      DSET_ERR("image type conflicts (ANAT and FUNC mixed)") ;

   SORT_3DARR( dset_arr ) ;

   /*-- 2.  If all images are anat, nothing to do at this moment. --*/

   if( all_anat ){
   } /* end of dealing with all_anat case */

   /*-- 3.  If all images are func .... --*/

   if( all_func ){
      THD_3dim_dataset * dset0 ;
      int jd ;

#if 0
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

   return dset_arr ;
}
