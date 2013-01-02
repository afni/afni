/*---------------------------------------------------------------------
  01 Feb 2001: compute the center of a dataset (DICOM coord order)
  Adapted from part of thd_info.c -- RWCox
-----------------------------------------------------------------------*/

#include "mrilib.h"

THD_fvec3 THD_dataset_center( THD_3dim_dataset * dset )
{
   THD_dataxes * daxes ;
   THD_fvec3 fv1 , fv2 ;

ENTRY("THD_dataset_center") ;

   if( !ISVALID_DSET(dset) ){ LOAD_FVEC3(fv1,0,0,0); RETURN(fv1); }

   daxes = dset->daxes ;

   LOAD_FVEC3(fv1 , daxes->xxorg , daxes->yyorg , daxes->zzorg) ;
   fv1 = THD_3dmm_to_dicomm( dset , fv1 ) ;

   LOAD_FVEC3(fv2 , daxes->xxorg + (daxes->nxx-1)*daxes->xxdel ,
                    daxes->yyorg + (daxes->nyy-1)*daxes->yydel ,
                    daxes->zzorg + (daxes->nzz-1)*daxes->zzdel  ) ;
   fv2 = THD_3dmm_to_dicomm( dset , fv2 ) ;

   fv1.xyz[0] = 0.5f * (fv1.xyz[0]+fv2.xyz[0]) ;
   fv1.xyz[1] = 0.5f * (fv1.xyz[1]+fv2.xyz[1]) ;
   fv1.xyz[2] = 0.5f * (fv1.xyz[2]+fv2.xyz[2]) ;

   RETURN(fv1) ;
}

/*-------------------------------------------------------------------------*/
/*! Get the center of mass of this volume, in DICOM coords, taken from 3dCM.
---------------------------------------------------------------------------*/

THD_fvec3 THD_cmass( THD_3dim_dataset *xset , int iv , byte *mmm )
{
   THD_fvec3 cmv ;
   MRI_IMAGE *im ;
   float *far , icm,jcm,kcm ;
   int ii , nvox ;

   LOAD_FVEC3(cmv,0,0,0) ;

   nvox = DSET_NVOX(xset) ;
   im   = mri_to_float( DSET_BRICK(xset,iv) ) ;
                             if( im  == NULL ) return cmv ;
   far = MRI_FLOAT_PTR(im) ; if( far == NULL ) return cmv ;

   if( mmm != NULL ){
     for( ii=0 ; ii < nvox ; ii++ )
       /* if mask is NOT set, clear it    2 Jan 2013 [rickr/dglen] */
       if( ! mmm[ii] ) far[ii] = 0.0 ;
   }

   mri_get_cmass_3D( im , &icm,&jcm,&kcm ) ; mri_free(im) ;
   LOAD_FVEC3(cmv,icm,jcm,kcm) ;
   cmv = THD_3dfind_to_3dmm( xset , cmv ) ;
   cmv = THD_3dmm_to_dicomm( xset , cmv ) ;
   return cmv ;
}
