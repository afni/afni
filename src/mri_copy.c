#include "mrilib.h"

/*** 7D SAFE ***/

/*-----------------------------------------------------
  Copy an image
-------------------------------------------------------*/

MRI_IMAGE * mri_copy( MRI_IMAGE * oldim )
{
   MRI_IMAGE * newim ;
   void * oar , * nar ;

   if( oldim == NULL ) return NULL ;

   newim = mri_new_conforming( oldim , oldim->kind ) ;
   oar   = mri_data_pointer( oldim ) ;
   nar   = mri_data_pointer( newim ) ;
   memcpy( nar , oar , newim->nvox * newim->pixel_size ) ;
   MRI_COPY_AUX( newim , oldim ) ;
   return newim ;
}
