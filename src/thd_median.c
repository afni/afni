#include "mrilib.h"

/*-----------------------------------------------------------------
  Compute median brick of a dataset - 12 Aug 2001
-------------------------------------------------------------------*/

MRI_IMAGE * THD_median_brick( THD_3dim_dataset * dset )
{
   int nvox , nvals , ii ;
   MRI_IMAGE *tsim , *medim ;
   float *medar ;

ENTRY("THD_median_brick") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   nvox  = DSET_NVOX(dset) ;
   nvals = DSET_NVALS(dset) ;

   tsim  = DSET_BRICK(dset,0) ;
   medim = mri_new_conforming( tsim , MRI_float ) ;
   medar = MRI_FLOAT_PTR(medim) ;

   for( ii=0 ; ii < nvox ; ii++ ){
      tsim = THD_extract_series( ii , dset , 0 ) ;
      medar[ii] = qmed_float( nvals , MRI_FLOAT_PTR(tsim) ) ;
      mri_free(tsim) ;
   }

   RETURN(medim) ;
}
