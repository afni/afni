#include "mrilib.h"

/*-------------------------------------------------------------------*/
/*! Make a warp-on-demand "duplicate" of a dataset.
---------------------------------------------------------------------*/

THD_3dim_dataset * EDIT_wod_copy( THD_3dim_dataset *dset_in )
{
   THD_3dim_dataset *dset_out ;
   THD_warp * warp ;

ENTRY("EDIT_wod_copy") ;

   if( !ISVALID_DSET(dset_in) ) RETURN(NULL) ;

   /*** copy header info ***/

   dset_out = EDIT_empty_copy( dset_in ) ;
   if( !ISVALID_3DIM_DATASET(dset_out) ) RETURN(NULL) ;

   warp = myRwcNew( THD_warp ) ; *warp = IDENTITY_WARP ;

   EDIT_dset_items( dset_out ,
                      ADN_prefix      , "dup" ,
                      ADN_type        , HEAD_ANAT_TYPE ,
                      ADN_func_type   , ANAT_BUCK_TYPE ,
                      ADN_warp        , warp    ,
                      ADN_warp_parent , dset_in ,
                    ADN_none ) ;

   dset_out->dblk->diskptr->storage_mode = STORAGE_UNDEFINED ;
   dset_out->dblk->malloc_type           = DATABLOCK_MEM_UNDEFINED ;

   RETURN(dset_out) ;
}
