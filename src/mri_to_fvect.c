#include "mrilib.h"

/*! Convert an image of vectors to an array of single-valued images. */

MRI_IMARR * mri_fvect_to_imarr( MRI_IMAGE *inim )
{
   float *iar ;
   MRI_IMAGE *aim ; float *aar ; MRI_IMARR *outar ;
   int nvox , vd , kk , ii ;

ENTRY("mri_fvect_to_imarr") ;

   if( inim == NULL || inim->kind != MRI_fvect ) RETURN(NULL) ;

   iar = mri_data_pointer(inim) ; if( iar == NULL ) RETURN(NULL) ;

   vd   = inim->vdim ; if( vd <= 0 ) RETURN(NULL) ;
   nvox = inim->nvox ;

   INIT_IMARR(outar) ;

   for( kk=0 ; kk < vd ; kk++ ){
     aim = mri_new_conforming( inim , MRI_float ) ;
     aar = aim->im ;
     for( ii=0 ; ii < nvox ; ii++ ) aar[ii] = iar[ii*vd+kk] ;
     MRI_COPY_AUX(aim,inim) ; ADDTO_IMARR(outar,aim) ;
   }

   RETURN(outar) ;
}
