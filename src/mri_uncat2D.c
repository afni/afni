#include "mrilib.h"

/* not 7D safe */

/*---------------------------------------------------------------------------------------
  Extract images of size (nx,ny) from  larger image im
-----------------------------------------------------------------------------------------*/

MRI_IMARR * mri_uncat2D( int nx , int ny , MRI_IMAGE * im )
{
   MRI_IMARR * imar ;
   MRI_IMAGE * qim ;
   int nxim , nyim , ii,jj,kk , kind ;

   if( nx < 1 || ny < 1 || im == NULL ) return NULL ;

   kind = im->kind ;

   nxim = im->nx / nx ;
   nyim = im->ny / ny ;
   if( nxim < 1 || nyim < 1 ) return NULL ;

   INIT_IMARR(imar) ;

   for( jj=0 ; jj < nyim ; jj++ ){
      for( ii=0 ; ii < nxim ; ii++ ){
         qim = mri_cut_2D( im , ii*nx , (ii+1)*nx-1 , jj*ny , (jj+1)*ny-1 ) ;
         if( qim != NULL ) ADDTO_IMARR(imar,qim) ;
         else              fprintf(stderr,"mri_uncat2D: NULL image error!\n") ;
      }
   }

   return imar ;
}
