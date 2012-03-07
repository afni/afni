#include "mrilib.h"

#undef  GOOD
#define GOOD(i) ( bmmm == mask[i] )

/*----------------------------------------------------------------------------*/

void binarize_mask( int nvox , byte *mask )
{
   register int ii ;
   if( nvox <= 0 || mask == NULL ) return ;
   for( ii=0 ; ii < nvox ; ii++ ) if( mask[ii] ) mask[ii] = 1 ;
   return ;
}

/*----------------------------------------------------------------------------*/
/* Extract (into a 1D float image) the values inside the mask.
   If invert is nonzero, means to extract all values where mask=0.
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_extract_from_mask( MRI_IMAGE *imin , byte *mask , int invert )
{
   byte bmmm = (invert == 0) ? 1 : 0 ;
   int ii,jj , ngood , nvox ;
   float *iar , *oar ;
   MRI_IMAGE *outim ;

ENTRY("mri_extract_mask") ;

   if( imin == NULL || mask == NULL ) RETURN(NULL) ;  /* bad user == luser */

   /*-- not float?  create a float image and recurse! --*/

   if( imin->kind != MRI_float ){
     MRI_IMAGE *qim = mri_to_float(imin) ;
     outim = mri_extract_from_mask( qim , mask , invert ) ;
     mri_free(qim) ;
     RETURN(outim) ;
   }

   /*-- count up the good voxels --*/

   nvox = imin->nvox ;
   for( ngood=ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ) ngood++ ;
   if( ngood == 0 ) RETURN(NULL) ;

   /*-- create the output --*/

   outim = mri_new( ngood , 1 , MRI_float ) ;
   oar   = MRI_FLOAT_PTR(outim) ;
   iar   = MRI_FLOAT_PTR(imin) ;

   /*-- fill the output --*/

   for( jj=ii=0 ; ii < nvox ; ii++ ) if( GOOD(ii) ) oar[jj++] = iar[ii] ;

   RETURN(outim) ;
}
