/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*---------------------------------------------------------------------
   Routine to make a copy of a dataset, with data attached.
-----------------------------------------------------------------------*/

THD_3dim_dataset * EDIT_full_copy( THD_3dim_dataset * dset , char * new_prefix )
{
   THD_3dim_dataset * new_dset ;
   int ival , ityp , nbytes , nvals ;
   void * new_brick , * old_brick ;

   /*-- sanity check --*/

   if( ! ISVALID_3DIM_DATASET(dset) ) return NULL ;

   /*-- make the empty copy --*/

   new_dset = EDIT_empty_copy( dset ) ;  /* copy is set to MALLOC memory */

   /*-- change its name? --*/

   if( new_prefix != NULL )
      EDIT_dset_items( new_dset ,
                          ADN_prefix , new_prefix ,
                          ADN_label1 , new_prefix ,
                       ADN_none ) ;

   /*-- make brick(s) for this dataset --*/

   THD_load_datablock( dset->dblk ) ;  /* make sure old one is in memory */

   nvals = DSET_NVALS(dset) ;

   for( ival=0 ; ival < nvals ; ival++ ){
      ityp      = DSET_BRICK_TYPE(new_dset,ival) ;   /* type of data */
      nbytes    = DSET_BRICK_BYTES(new_dset,ival) ;  /* how much data */
      new_brick = malloc( nbytes ) ;                 /* make room */

      if( new_brick == NULL ){
        THD_delete_3dim_dataset( new_dset , False ) ;
        return NULL ;
      }

      EDIT_substitute_brick( new_dset , ival , ityp , new_brick ) ;

      /*-- copy data from old brick to new brick --*/

      old_brick = DSET_BRICK_ARRAY(dset,ival) ;

      if( old_brick == NULL ){
         THD_delete_3dim_dataset( new_dset , False ) ;
         return NULL ;
      }

      memcpy( new_brick , old_brick , nbytes ) ;
   }

   return new_dset ;
}
