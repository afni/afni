#include "mrilib.h"

/*-----------------------------------------------------------------
  Compute median brick of a dataset - 12 Aug 2001
  05 Nov 2001: Modified to use THD_extract_array() instead
               of THD_extract_series()
-------------------------------------------------------------------*/

MRI_IMAGE * THD_median_brick( THD_3dim_dataset * dset )
{
   int nvox , nvals , ii ;
   MRI_IMAGE *tsim , *medim ;
   float *medar ;
   float *tsar ;  /* 05 Nov 2001 */

ENTRY("THD_median_brick") ;

   if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ) RETURN(NULL) ;

   nvals = DSET_NVALS(dset) ;
   tsim  = DSET_BRICK(dset,0) ;

   if( nvals == 1 ){
     medim = mri_scale_to_float( DSET_BRICK_FACTOR(dset,0), tsim ) ;
     RETURN(medim) ;
   }

   medim = mri_new_conforming( tsim , MRI_float ) ;
   medar = MRI_FLOAT_PTR(medim) ;
   nvox  = DSET_NVOX(dset) ;

   tsar = (float *) calloc( sizeof(float),nvals+1 ) ; /* 05 Nov 2001 */
   for( ii=0 ; ii < nvox ; ii++ ){
      THD_extract_array( ii , dset , 0 , tsar ) ;     /* 05 Nov 2001 */
      medar[ii] = qmed_float( nvals , tsar ) ;
   }

   free(tsar) ; RETURN(medim) ;
}
