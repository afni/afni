#include "mrilib.h"

THD_3dim_dataset * THD_remove_allzero( THD_3dim_dataset *iset )
{
   THD_3dim_dataset *oset=NULL ;
   int kv,iv , nvals , nbad , nvout ;
   MRI_IMAGE *bim ;

ENTRY("THD_remove_allzero") ;

   if( !ISVALID_DSET(iset) ) RETURN(NULL) ;
   DSET_load(iset) ;
   if( !DSET_LOADED(iset) ) RETURN(NULL) ;

   /* check if anything (or everything) is all zero */

   nvals = DSET_NVALS(iset) ;
   for( nbad=iv=0 ; iv < nvals ; iv++ ){
     if( mri_allzero(DSET_BRICK(iset,iv)) ) nbad++ ;
   }
   if( nbad == 0 || nbad == nvals ) RETURN(NULL) ;
   INFO_message("removing %d all-zero volume%s from %s",
                nbad , (nbad==1)?"\0":"s" , DSET_PREFIX(iset) ) ;

   nvout = nvals - nbad ;
   oset = EDIT_empty_copy(iset) ;
   EDIT_dset_items( oset ,
                      ADN_prefix , DSET_PREFIX(iset) ,
                      ADN_nvals  , nvout ,
                    ADN_none ) ;
   for( kv=iv=0 ; iv < nvals ; iv++ ){
     if( mri_allzero(DSET_BRICK(iset,iv)) ) continue ;
     bim = mri_copy(DSET_BRICK(iset,iv)) ;
     EDIT_substitute_brick(oset,kv,(int)bim->kind,mri_data_pointer(bim)) ;
     kv++ ;
   }

   DSET_unload(iset) ;
   RETURN(oset) ;
}
