#include "mrilib.h"

/*---------------------------------------------------------------------
  01 Feb 2001: determine if 2 datasets don't match in some way for
   voxel-by-voxel comparison.
    return = 0 if they are good in all ways
    return = binary OR of MISMATCH_ codes (cf. 3ddata.h) if
             something in their dataxes don't match
-----------------------------------------------------------------------*/

int THD_dataset_mismatch( THD_3dim_dataset *ds1 , THD_3dim_dataset *ds2 )
{
   THD_dataxes * dax1 , * dax2 ;
   THD_fvec3 fv1 , fv2 , dv ;
   int code ;
   float cd,c1,c2 ;

ENTRY("THD_dataset_mismatch") ;

   if( !ISVALID_DSET(ds1) || !ISVALID_DSET(ds2) ) RETURN(-1) ;

   dax1 = ds1->daxes ;
   dax2 = ds1->daxes ;
   code = 0 ;           /* will be return value */

   /* check if the number of voxels in each direction is the same */

   if( dax1->nxx != dax2->nxx ||
       dax1->nyy != dax2->nyy ||
       dax1->nzz != dax2->nzz   ) code |= MISMATCH_DIMEN ;

   /* check if the grid spacings are the same */

   if( dax1->xxdel != dax2->xxdel ||
       dax1->yydel != dax2->yydel ||
       dax1->zzdel != dax2->zzdel   ) code |= MISMATCH_DELTA ;

   /* check if the orientations are the same */

   if( dax1->xxorient != dax2->xxorient ||
       dax1->yyorient != dax2->yyorient ||
       dax1->zzorient != dax2->zzorient   ) code |= MISMATCH_ORIENT ;

   /* check if they have the same centers */

   fv1 = THD_dataset_center( ds1 ) ;
   fv2 = THD_dataset_center( ds2 ) ;
   dv  = SUB_FVEC3(fv1,fv2) ; cd = SIZE_FVEC3(dv) ;

   LOAD_FVEC3(fv1,dax1->xxdel,dax1->yydel,dax1->zzdel) ; c1 = SIZE_FVEC3(fv1) ;
   LOAD_FVEC3(fv2,dax2->xxdel,dax2->yydel,dax2->zzdel) ; c2 = SIZE_FVEC3(fv1) ;

   if( cd > 0.1*(c1+c2) ) code |= MISMATCH_CENTER ;

   RETURN(code) ;
}
