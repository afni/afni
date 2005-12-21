/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/*----------------------------------------------------------------
   Edit a THD_dataxes structure to allow for resampling to a new
   voxel size (resam).
-----------------------------------------------------------------*/

void THD_edit_dataxes( float resam , THD_dataxes *daxes ,
                                     THD_dataxes *wod_daxes )
{
   float lxx , lyy , lzz ;
   float rex , rey , rez ;

   if( !ISVALID_DATAXES(daxes) || !ISVALID_DATAXES(wod_daxes) ) return ;

   *wod_daxes = *daxes ;       /* copy insides, then edit them */

   if( resam <= 0.0 ) return ; /* error */

   rex = (daxes->xxdel > 0) ? resam : -resam ;  /* signed resampled */
   rey = (daxes->yydel > 0) ? resam : -resam ;  /* voxel sizes */
   rez = (daxes->zzdel > 0) ? resam : -resam ;

   lxx = daxes->nxx * daxes->xxdel ;    /* signed lengths of data box */
   lyy = daxes->nyy * daxes->yydel ;
   lzz = daxes->nzz * daxes->zzdel ;

   wod_daxes->nxx = (int)( lxx/rex + 0.499 ) ;  /* new dimensions */
   wod_daxes->nyy = (int)( lyy/rey + 0.499 ) ;  /* (will be > 0) */
   wod_daxes->nzz = (int)( lzz/rez + 0.499 ) ;

   /* go to old middle, then back out to get new edge */

   wod_daxes->xxorg = daxes->xxorg + 0.5*(lxx - daxes->xxdel)
                                   - 0.5*(wod_daxes->nxx - 1)*rex ;

   wod_daxes->yyorg = daxes->yyorg + 0.5*(lyy - daxes->yydel)
                                   - 0.5*(wod_daxes->nyy - 1)*rey ;

   wod_daxes->zzorg = daxes->zzorg + 0.5*(lzz - daxes->zzdel)
                                   - 0.5*(wod_daxes->nzz - 1)*rez ;

   /* new dimensions of the voxels */

   wod_daxes->xxdel = rex ;
   wod_daxes->yydel = rey ;
   wod_daxes->zzdel = rez ;

   /* do the bounding box thing again */

   THD_set_daxes_bbox( wod_daxes ) ;  /* 20 Dec 2005 */

   /** 15 Dec 2005: deal with the new matrix coordinate entries **/

   { mat44 new_mat ; int nxnew , nynew , nznew ;
     new_mat = THD_resample_mat44( daxes->ijk_to_dicom ,
                                   daxes->nxx , daxes->nyy , daxes->nzz ,
                                   resam      , resam      , resam      ,
                                   &nxnew     , &nynew     , &nznew      ) ;
     if( ISVALID_MAT44(new_mat) ){
       wod_daxes->ijk_to_dicom = new_mat ;
       wod_daxes->dicom_to_ijk = nifti_mat44_inverse( new_mat ) ;
       THD_set_dicom_box(wod_daxes) ;
     }
   }

   return ;
}

/*----------------------------------------------------------------*/

void THD_set_daxes_bbox( THD_dataxes *daxes )  /* 20 Dec 2005 */
{
    if( !ISVALID_DATAXES(daxes) ) return ;

    /*---------------------------------------*/
    /*-- set bounding box for this dataset --*/
    /*---------------------------------------*/

    daxes->xxmin = daxes->xxorg ;
    daxes->xxmax = daxes->xxorg + (daxes->nxx-1) * daxes->xxdel ;
    if( daxes->xxmin > daxes->xxmax ){
      float temp   = daxes->xxmin ;
      daxes->xxmin = daxes->xxmax ; daxes->xxmax = temp ;
    }

    daxes->yymin = daxes->yyorg ;
    daxes->yymax = daxes->yyorg + (daxes->nyy-1) * daxes->yydel ;
    if( daxes->yymin > daxes->yymax ){
      float temp   = daxes->yymin ;
      daxes->yymin = daxes->yymax ; daxes->yymax = temp ;
    }

    daxes->zzmin = daxes->zzorg ;
    daxes->zzmax = daxes->zzorg + (daxes->nzz-1) * daxes->zzdel ;
    if( daxes->zzmin > daxes->zzmax ){
      float temp   = daxes->zzmin ;
      daxes->zzmin = daxes->zzmax ; daxes->zzmax = temp ;
    }

#ifdef EXTEND_BBOX
    daxes->xxmin -= 0.5 * daxes->xxdel ;  /* pushes edges back by 1/2  */
    daxes->xxmax += 0.5 * daxes->xxdel ;  /* voxel dimensions (the box */
    daxes->yymin -= 0.5 * daxes->yydel ;  /* defined above is based on */
    daxes->yymax += 0.5 * daxes->yydel ;  /* voxel centers, not edges) */
    daxes->zzmin -= 0.5 * daxes->zzdel ;
    daxes->zzmax += 0.5 * daxes->zzdel ;
#endif

   return ;
}

/*----------------------------------------------------------------*/

void THD_set_daxes_to_dicomm( THD_dataxes *daxes )  /* 20 Dec 2005 */
{
    if( !ISVALID_DATAXES(daxes) ) return ;

    /*----------------------------------------------------------------*/
    /*--  matrix that transforms to Dicom (left-posterior-superior) --*/
    /*----------------------------------------------------------------*/

    LOAD_ZERO_MAT(daxes->to_dicomm) ;

    switch( daxes->xxorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: daxes->to_dicomm.mat[0][0] = 1.0 ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: daxes->to_dicomm.mat[1][0] = 1.0 ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: daxes->to_dicomm.mat[2][0] = 1.0 ; break ;
    }

    switch( daxes->yyorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: daxes->to_dicomm.mat[0][1] = 1.0 ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: daxes->to_dicomm.mat[1][1] = 1.0 ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: daxes->to_dicomm.mat[2][1] = 1.0 ; break ;
    }

    switch( daxes->zzorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: daxes->to_dicomm.mat[0][2] = 1.0 ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: daxes->to_dicomm.mat[1][2] = 1.0 ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: daxes->to_dicomm.mat[2][2] = 1.0 ; break ;
    }

    return ;
}
