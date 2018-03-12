#include "mrilib.h"

/*----------------------------------------------------------------------------*/

THD_3dim_dataset * THD_image_to_dset( MRI_IMAGE *im , char *prefix )
{
   THD_3dim_dataset *dset ;
   MRI_IMAGE *cim ;
   THD_ivec3 ivec , orixyz ;
   THD_fvec3 fvec ;

ENTRY("THD_image_to_dset") ;

   if( im == NULL ) RETURN(NULL) ;

   dset = EDIT_empty_copy(NULL) ;

   LOAD_IVEC3( ivec , im->nx , im->ny , im->nz ) ;

   LOAD_FVEC3( fvec , -0.5f*(im->nx-1) , -0.5f*(im->ny-1) , -0.5f*(im->nz-1) ) ;

   orixyz.ijk[0] = ORI_L2R_TYPE ; /* 02 Feb 2018 */
   orixyz.ijk[1] = ORI_A2P_TYPE ; /* orient it so Left-on-Left viewing is good */
   orixyz.ijk[2] = ORI_I2S_TYPE ; /* since L-on-L viewing is the AFNI default */

   EDIT_dset_items( dset ,
                     ADN_nxyz      , ivec ,
                     ADN_xyzorg    , fvec ,
                     ADN_xyzorient , orixyz ,
                     ADN_type      , HEAD_FUNC_TYPE ,
                     ADN_func_type , FUNC_FIM_TYPE ,
                    ADN_none ) ;

   if( THD_filename_ok(prefix) )
     EDIT_dset_items( dset , ADN_prefix , prefix , ADN_none ) ;

   dset->dblk->diskptr->storage_mode = STORAGE_BY_IMAGE_FILE ;
   DSET_superlock(dset) ;  /* can't be purged from memory */

   cim = mri_copy(im) ;
   EDIT_substitute_brick( dset , 0 , cim->kind , mri_data_pointer(cim) ) ;
   mri_clear_data_pointer(cim) ; mri_free(cim) ;

   RETURN(dset) ;
}

/*----------------------------------------------------------------------------*/

THD_3dim_dataset * THD_open_image( char *fname )
{
   MRI_IMAGE *im ; THD_3dim_dataset *dset ;

ENTRY("THD_open_image") ;

   im = mri_read_stuff(fname) ;

   if( im == NULL ) RETURN(NULL) ;

   dset = THD_image_to_dset(im,THD_trailname(fname,0)) ;
   mri_free(im) ;

   RETURN(dset) ;
}
