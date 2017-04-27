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

// [PT, 20 Dec, 2016] Updated to output either: ijk, or the (default)
// xyz in DICOM coords.  New input variable 'cmode' controls this
// (default, cmode=0).

---------------------------------------------------------------------------*/

THD_fvec3 THD_cmass( THD_3dim_dataset *xset , int iv , byte *mmm,
                     int cmode)
{
   THD_fvec3 cmv ;
   THD_ivec3 cmvi ;
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

   // [PT, Dec, 2016]: a bit convoluted, but return ijk (as floats) if
   // the mode suits it.
   if( cmode == 1 ) {  // return int ijk in dset-orientation
      cmvi = THD_3dmm_to_3dind( xset , cmv );
      LOAD_FVEC3(cmv,cmvi.ijk[0], cmvi.ijk[1], cmvi.ijk[2]);
      return cmv; 
   }

   cmv = THD_3dmm_to_dicomm( xset , cmv ) ;
   return cmv ;
}

/*-------------------------------------------------------------------------*/
/*! Get the center of mass of integer labeled ROIs in a sub-brick. Returns
a float vector N_rois XYZ triplets.
---------------------------------------------------------------------------*/

float *THD_roi_cmass(THD_3dim_dataset *xset , int iv , int *rois, 
                     int N_rois, int cmode)
{
   float *xyz=NULL, roi;
   THD_fvec3 cmr ;
   int ir;
   byte *mmm;
   
   ENTRY("THD_roi_cmass");
   
   if (!xset || !rois || N_rois < 1) RETURN(NULL);
   
   xyz = (float *)calloc(N_rois*3, sizeof(float));
   for (ir = 0; ir < N_rois; ++ir) {
      roi = rois[ir];
      mmm = THD_makemask( xset, iv , roi , roi );
      cmr = THD_cmass( xset, iv , mmm, cmode);
      free(mmm); mmm = NULL;
      xyz[3*ir]   = cmr.xyz[0]; 
      xyz[3*ir+1] = cmr.xyz[1]; 
      xyz[3*ir+2] = cmr.xyz[2];
   }
   
   RETURN(xyz);
}
