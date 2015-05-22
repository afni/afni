#include "mrilib.h"

/*---------------------------------------------------------------------
  01 Feb 2001: determine if 2 datasets don't match in some way for
   voxel-by-voxel comparison.
    return = 0 if they are good in all ways
    return = binary OR of MISMATCH_ codes (cf. 3ddata.h) if
             something in their dataxes don't match
   A generalization of EQUIV_GRIDS
-----------------------------------------------------------------------*/

int THD_dataset_mismatch( THD_3dim_dataset *ds1 , THD_3dim_dataset *ds2 )
{
   THD_dataxes * dax1 , * dax2 ;
   THD_fvec3 fv1 , fv2 , dv ;
   int code ;
   float cd,c1,c2 ;
   double angle;

ENTRY("THD_dataset_mismatch") ;

   if( !ISVALID_DSET(ds1) || !ISVALID_DSET(ds2) ) RETURN(-1) ;

   dax1 = ds1->daxes ;
   dax2 = ds2->daxes ;
   code = 0 ;           /* will be return value */

   /* check if the number of voxels in each direction is the same */

   if( dax1->nxx != dax2->nxx ||
       dax1->nyy != dax2->nyy ||
       dax1->nzz != dax2->nzz   ) code |= MISMATCH_DIMEN ;

   /* check if the grid spacings are the same */

   if( fabs(dax1->xxdel-dax2->xxdel) > 0.01*fabs(dax1->xxdel) ||
       fabs(dax1->yydel-dax2->yydel) > 0.01*fabs(dax1->yydel) ||
       fabs(dax1->zzdel-dax2->zzdel) > 0.01*fabs(dax1->zzdel)   ) code |= MISMATCH_DELTA ;

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

   /* check if the obliquity is the same */
   /* (allow for truncation differnces)   22 May 2015 [rickr] */
   angle = dset_obliquity_angle_diff(ds1, ds2, OBLIQ_ANGLE_THRESH);
   if (angle > 0.0) code |= MISMATCH_OBLIQ ;
   
   RETURN(code) ;
}

/*---------------------------------------------------------------------
  23 Feb 2012: Return the absolute value of the difference between 
               two volumes, divided by the number of voxels
               and the number of sub-bricks. Voxels that are zero
               in both sets are not counted.
               Comparisons are done after conversion of data to double
    return = -1.0 ERROR
           =  0.0 Exactly the same
-----------------------------------------------------------------------*/
double THD_diff_vol_vals(THD_3dim_dataset *d1, THD_3dim_dataset *d2, int scl) {
   double dd=0.0, denom=0.0;
   int i=0, k=0;
   double *a1=NULL, *a2=NULL;
   MRI_IMAGE *b1 = NULL , *b2 = NULL;
   
   ENTRY("THD_diff_vol_vals");
   
   if (!d1 && !d2) RETURN(dd);
   if (!d1 || !d2) RETURN(-1.0);

   if (!EQUIV_GRIDS(d1,d2)) RETURN(-1.0);
   if (DSET_NVALS(d1) != DSET_NVALS(d2)) RETURN(-1.0);
  
   DSET_mallocize(d1) ; DSET_load(d1) ;
   DSET_mallocize(d2) ; DSET_load(d2) ;
   dd = 0.0; denom = 0;
   for (i=0; i<DSET_NVALS(d1); ++i) {
      b1 = THD_extract_double_brick(i, d1);
      b2 = THD_extract_double_brick(i, d2);
      a1 = MRI_DOUBLE_PTR(b1);
      a2 = MRI_DOUBLE_PTR(b2);
      for (k=0; k<DSET_NVOX(d1); ++k) {
         dd += ABS(a1[k]-a2[k]);
         if (a1[k]!=0.0 || a2[k]!=0.0) ++denom;
      }
      mri_clear_data_pointer(b1); mri_free(b1) ;
      mri_clear_data_pointer(b2); mri_free(b2) ;
   }
   if (scl && denom>0.0) dd /= denom;
   
   RETURN(dd);   
}  
