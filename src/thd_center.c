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

   fv1.xyz[0] = 0.5*(fv1.xyz[0]+fv2.xyz[0]) ;
   fv1.xyz[1] = 0.5*(fv1.xyz[1]+fv2.xyz[1]) ;
   fv1.xyz[2] = 0.5*(fv1.xyz[2]+fv2.xyz[2]) ;

   RETURN(fv1) ;
}
