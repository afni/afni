#include "mrilib.h"
#include "extrema.h"

/*-------------------------------------------------------------------------*/
/* Enhance the interior edges of a 3D volume (brain image).
   Positive return value = number of input voxels altered.
   Negative return value = error code
*//*-----------------------------------------------------------------------*/

int mri_interior_edgeize( MRI_IMAGE *inim , int erode , float frac )
{
   MRI_IMAGE *outim ;
   float *inar , *outar , inmax , outmax , sfac ;
   byte *mask ;
   int indims[3] , border[3] , ii , nx,ny,nz , nxyz , nadd,nmm ;
   float filterCoefs[3] = {1.0, 1.0, 1.0} ;
   recursiveFilterType filterType = ALPHA_DERICHE ;

   if( inim == NULL || inim->kind != MRI_float ) return -1 ;
   inar = MRI_FLOAT_PTR(inim) ;

        if( erode < 0    ) erode = 4 ;
        if( frac  < 0.0f ) frac  = 0.1f ;
   else if( frac  > 1.0f ) frac= 1.0f ;

   nx = indims[0] = inim->nx ; if( nx < 32 ) return -2 ;
   ny = indims[1] = inim->ny ; if( ny < 32 ) return -3 ;
   nz = indims[2] = inim->nz ; if( nz < 32 ) return -4 ;
   border[0] = nx / 4 ; if( border[0] < 16 ) border[0] = 16 ;
   border[1] = ny / 4 ; if( border[1] < 16 ) border[1] = 16 ;
   border[2] = nz / 4 ; if( border[2] < 16 ) border[2] = 32 ;

   nxyz  = nx*ny*nz ;
   inmax = mri_maxabs(inim) ; if( inmax == 0.0f ) return -5 ;

   outim = mri_new_conforming( inim , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;

   /* edge detection */

   ii = Extract_Gradient_Maxima_3D( (void *)inar , FLOAT ,
                                    (void *)outar, FLOAT ,
                                    indims , border ,
                                    filterCoefs , filterType ) ;

   if( ii == 0 ){ mri_free(outim); return -6; }

   outmax = mri_maxabs(outim) ;
   if( outmax == 0.0f ){ mri_free(outim); return -7; }

   /* make eroded mask (to keep only interior edges) */

   mask = mri_automask_image(inim) ;
   if( mask == NULL ){ mri_free(outim); return -8; }

   nmm = 1 ;
   ii  = rint(0.032*nx) ; nmm = MAX(nmm,ii) ;
   ii  = rint(0.032*ny) ; nmm = MAX(nmm,ii) ;
   ii  = rint(0.032*nz) ; nmm = MAX(nmm,ii) ;

   for( ii=0 ; ii < erode ; ii++ ){
     THD_mask_erode            ( nx,ny,nz , mask, 0 ) ;
     THD_mask_fillin_completely( nx,ny,nz , mask, nmm ) ;
   }

   /* throw away non-automask edge voxels */

   outmax = 0.0f ;
   for( ii=0 ; ii < nxyz ; ii++ ){
          if( !mask[ii]          ) outar[ii] = 0.0f ;
     else if( outar[ii] > outmax ) outmax = outar[ii] ;
   }
   free(mask) ;

   if( outmax == 0.0f ){ free(outim); return -9; }

   /* add scaled edge values to input image */

   sfac = frac * inmax / outmax ;

   for( nadd=ii=0 ; ii < nxyz ; ii++ ){
     if( outar[ii] > 0.0f ){ inar[ii] += sfac*outar[ii] ; nadd++ ; }
   }

   /* I'm done */

   free(outim) ; return nadd ;
}
