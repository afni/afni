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
