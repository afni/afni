/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** 7D SAFE ***/

/*-----------------------------------------------------
  Copy an image.
-------------------------------------------------------*/

MRI_IMAGE * mri_copy( MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   void *oar , *nar ;

ENTRY("mri_copy") ;

   if( oldim == NULL ) RETURN(NULL);

   newim = mri_new_conforming( oldim , oldim->kind ) ;
   oar   = mri_data_pointer( oldim ) ;
   nar   = mri_data_pointer( newim ) ;
   if( oar == NULL ){
     free(nar); mri_fix_data_pointer(NULL,newim);
   } else {
     memcpy( nar , oar , newim->nvox * newim->pixel_size ) ;
   }
   MRI_COPY_AUX( newim , oldim ) ;
   RETURN( newim );
}

/*---------------------------------------------------------------------*/
/* Copy a 2D image, expanding it nup times in x and y.
-----------------------------------------------------------------------*/

MRI_IMAGE * mri_expand_2D( int nup , MRI_IMAGE *imin )  /* 22 Feb 2004 */
{
   MRI_IMAGE *newim ;
   int nx,ny , nxup,nyup , ii,jj, pp,qq , ds ;
   char *nar , *iar ;

ENTRY("mri_expand") ;

   /*-- sanity checks --*/

   if( nup < 1 || imin == NULL || imin->nz > 1 ) RETURN(NULL);

   if( nup == 1 ){ newim = mri_copy(imin); RETURN(newim); }

   iar = (char *)mri_data_pointer(imin) ; if( iar == NULL ) RETURN(NULL);

   nx = imin->nx ; nxup = nx*nup ;
   ny = imin->ny ; nyup = ny*nup ;

   newim = mri_new( nxup,nyup , imin->kind ) ;
   nar   = (char *)mri_data_pointer(newim) ;
   ds    = imin->pixel_size ;

   for( jj=0 ; jj < nyup ; jj++ ){
     qq = jj / nup ;
     for( ii=0 ; ii < nxup ; ii++ ){
       pp = ii / nup ;
       memcpy( nar + (ii+jj*nxup)*ds , iar + (pp+qq*nx)*ds , ds ) ;
     }
   }

   MRI_COPY_AUX( newim , imin ) ;
   RETURN(newim) ;
}
