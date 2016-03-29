#include "mrilib.h"

/*! Return 1 if an image is all zero (or NULL),
    or return 0 if it is not all zero (bytewise) */

int mri_allzero( MRI_IMAGE *im )
{
   char *ar ; unsigned int ii , nch ;

   if( im == NULL ) return 1 ;
   ar = mri_data_pointer(im) ; if( ar == NULL ) return 1 ;
   nch = (unsigned int)im->nvox * (unsigned int)im->pixel_size ;
   for( ii=0 ; ii < nch ; ii++ ) if( ar[ii] != 0 ) return 0 ;
   return 1 ;
}

/*! Return count of nonzero voxels. */

int mri_nonzero_count( MRI_IMAGE *im )
{
   MRI_IMAGE *fim ; float *far ;
   int ii , nvox , nnz ;

   if( mri_allzero(im) ) return 0 ;

   if( im->kind == MRI_float ) fim = im ;
   else                        fim = mri_to_float(im) ;
   far = MRI_FLOAT_PTR(fim) ;
   nvox = fim->nvox ;
   for( nnz=ii=0 ; ii < nvox ; ii++ ) if( far[ii] != 0.0f ) nnz++ ;
   if( fim != im ) mri_free(fim) ;
   return nnz ;
}
