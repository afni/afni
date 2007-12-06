#include "mrilib.h"

/*-------------------------------------------------*/
/* break a 3D image into a collection of 2D slices */
/*-------------------------------------------------*/

MRI_IMARR * mri_to_imarr( MRI_IMAGE *imin )
{
   MRI_IMARR *imar ; MRI_IMAGE *qim ;
   int nx,ny,nz , kk ;

   if( imin == NULL ) return NULL ;

   nx = imin->nx ; ny = imin->ny ; nz = imin->nz ;

   INIT_IMARR(imar) ;

   for( kk=0 ; kk < nz ; kk++ ){
     qim = mri_cut_3D( imin , 0,nx-1 , 0,ny-1 , kk,kk ) ;
     ADDTO_IMARR(imar,qim) ;
   }

   return imar ;
}
