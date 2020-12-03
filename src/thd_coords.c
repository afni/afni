/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

static int oblique_report_index = 0;
static int oblique_report_repeat = 20;
static int oblique_report_repeat2 = 100;
static int first_oblique = 1;
static int oblique_update = 0;
static int OBL_report=1;
void set_obliquity_report(int v) { OBL_report=v; } 


/*====================================================================
   3D coordinate conversion routines;
     tags for coordinate systems:
       fdind  = FD_brick voxel indices (ints)
       fdfind = FD_brick voxel indices (floats - added 30 Aug 2001)
       3dind  = THD_3dim_dataset voxel indices (ints)
       3dfind = THD_3dim_dataset floating point voxel indices
                  (used for subvoxel resolution)
       3dmm   = THD_3dim_dataset millimetric coordinates (floats)
       dicomm = DICOM 3.0 millimetric coordinates (floats)

     The 3dmm coordinate measurements are oriented the same way
     as the dicomm coordinates (which means that the ??del values
     can be negative if the voxel axes are reflected), but the 3dmm
     coordinates are in general a permutation of the DICOM coordinates.

     These routines all take as input and produce as output
     THD_?vec3 (i=int,f=float) structs.
======================================================================*/

THD_fvec3 THD_3dfind_to_3dmm( THD_3dim_dataset *dset , THD_fvec3 iv )
{
   THD_dataxes *daxes ;
   THD_fvec3    fv ;

   daxes = CURRENT_DAXES(dset) ;

   fv.xyz[0] = daxes->xxorg + iv.xyz[0] * daxes->xxdel ;
   fv.xyz[1] = daxes->yyorg + iv.xyz[1] * daxes->yydel ;
   fv.xyz[2] = daxes->zzorg + iv.xyz[2] * daxes->zzdel ;
   return fv ;
}

/*------------------------------------------------------------------*/

THD_fvec3 THD_3dind_to_3dmm( THD_3dim_dataset *dset , THD_ivec3 iv )
{
   THD_dataxes *daxes ;
   THD_fvec3    fv ;

   daxes = CURRENT_DAXES(dset) ;

   fv.xyz[0] = daxes->xxorg + iv.ijk[0] * daxes->xxdel ;
   fv.xyz[1] = daxes->yyorg + iv.ijk[1] * daxes->yydel ;
   fv.xyz[2] = daxes->zzorg + iv.ijk[2] * daxes->zzdel ;
   return fv ;
}

/*--------------------------------------------------------------------*/
/* this version is without using wod dataxes       7 Mar 2005 [rickr] */

THD_fvec3 THD_3dind_to_3dmm_no_wod( THD_3dim_dataset *dset , THD_ivec3 iv )
{
   THD_dataxes *daxes ;
   THD_fvec3    fv ;

   daxes = dset->daxes ;

   fv.xyz[0] = daxes->xxorg + iv.ijk[0] * daxes->xxdel ;
   fv.xyz[1] = daxes->yyorg + iv.ijk[1] * daxes->yydel ;
   fv.xyz[2] = daxes->zzorg + iv.ijk[2] * daxes->zzdel ;
   return fv ;
}

/*--------------------------------------------------------------------*/
/* 04 Apr 2012 - RWCox */

THD_fvec3 THD_3dind_to_dicomm_no_wod( THD_3dim_dataset *dset , THD_ivec3 iv )
{
   THD_fvec3 fv , gv ;
   fv = THD_3dind_to_3dmm_no_wod( dset , iv ) ;
   gv = THD_3dmm_to_dicomm      ( dset , fv ) ;
   return gv ;
}

/*--------------------------------------------------------------------*/

THD_fvec3 THD_3dmm_to_3dfind( THD_3dim_dataset *dset , THD_fvec3 fv )
{
   THD_dataxes *daxes ;
   THD_fvec3    iv ;

   daxes = CURRENT_DAXES(dset) ;

   iv.xyz[0] = (fv.xyz[0] - daxes->xxorg) / daxes->xxdel ;
   iv.xyz[1] = (fv.xyz[1] - daxes->yyorg) / daxes->yydel ;
   iv.xyz[2] = (fv.xyz[2] - daxes->zzorg) / daxes->zzdel ;

        if( iv.xyz[0] < 0            ) iv.xyz[0] = 0 ;
   else if( iv.xyz[0] > daxes->nxx-1 ) iv.xyz[0] = daxes->nxx-1 ;

        if( iv.xyz[1] <  0           ) iv.xyz[1] = 0 ;
   else if( iv.xyz[1] > daxes->nyy-1 ) iv.xyz[1] = daxes->nyy-1 ;

        if( iv.xyz[2] < 0            ) iv.xyz[2] = 0 ;
   else if( iv.xyz[2] > daxes->nzz-1 ) iv.xyz[2] = daxes->nzz-1 ;

   return iv ;
}

/*--------------------------------------------------------------------*/

THD_ivec3 THD_3dmm_to_3dind_warn( THD_3dim_dataset* dset ,
                                  THD_fvec3 fv, int *out )
{
   THD_dataxes *daxes ;
   THD_ivec3    iv ;

   *out = 0;
   daxes = CURRENT_DAXES(dset) ;

   iv.ijk[0] = (fv.xyz[0] - daxes->xxorg) / daxes->xxdel + 0.49f ;
   iv.ijk[1] = (fv.xyz[1] - daxes->yyorg) / daxes->yydel + 0.49f ;
   iv.ijk[2] = (fv.xyz[2] - daxes->zzorg) / daxes->zzdel + 0.49f ;

        if( iv.ijk[0] < 0            ) { iv.ijk[0] = 0 ; *out = 1; }
   else if( iv.ijk[0] > daxes->nxx-1 ) { iv.ijk[0] = daxes->nxx-1 ; *out = 1; }

        if( iv.ijk[1] < 0            ) { iv.ijk[1] = 0 ; *out = 1; }
   else if( iv.ijk[1] > daxes->nyy-1 ) { iv.ijk[1] = daxes->nyy-1 ; *out = 1; }

        if( iv.ijk[2] < 0            ) { iv.ijk[2] = 0 ; *out = 1; }
   else if( iv.ijk[2] > daxes->nzz-1 ) { iv.ijk[2] = daxes->nzz-1 ; *out = 1; }

   return iv ;
}

THD_ivec3 THD_3dmm_to_3dind( THD_3dim_dataset *dset , THD_fvec3 fv )
{
   THD_dataxes *daxes ;
   THD_ivec3    iv ;

   daxes = CURRENT_DAXES(dset) ;

   iv.ijk[0] = (fv.xyz[0] - daxes->xxorg) / daxes->xxdel + 0.49f ;
   iv.ijk[1] = (fv.xyz[1] - daxes->yyorg) / daxes->yydel + 0.49f ;
   iv.ijk[2] = (fv.xyz[2] - daxes->zzorg) / daxes->zzdel + 0.49f ;

        if( iv.ijk[0] < 0            ) iv.ijk[0] = 0 ;
   else if( iv.ijk[0] > daxes->nxx-1 ) iv.ijk[0] = daxes->nxx-1 ;

        if( iv.ijk[1] < 0            ) iv.ijk[1] = 0 ;
   else if( iv.ijk[1] > daxes->nyy-1 ) iv.ijk[1] = daxes->nyy-1 ;

        if( iv.ijk[2] < 0            ) iv.ijk[2] = 0 ;
   else if( iv.ijk[2] > daxes->nzz-1 ) iv.ijk[2] = daxes->nzz-1 ;

#if defined(USE_TRACING) && 0
INFO_message("THD_3dmm_to_3dind: fv=%f %f %f  iv=%d %d %d  from %s",
             fv.xyz[0],fv.xyz[1],fv.xyz[2] , iv.ijk[0],iv.ijk[1],iv.ijk[2] , DBROUT ) ;
#endif

   return iv ;
}

/*--------------------------------------------------------------------*/

/* this version is without using wod dataxes     28 Sep 2004 [rickr] */

THD_ivec3 THD_3dmm_to_3dind_no_wod( THD_3dim_dataset *dset , THD_fvec3 fv )
{
   THD_dataxes *daxes ;
   THD_ivec3    iv ;

   daxes = dset->daxes ;

   iv.ijk[0] = (fv.xyz[0] - daxes->xxorg) / daxes->xxdel + 0.49f ;
   iv.ijk[1] = (fv.xyz[1] - daxes->yyorg) / daxes->yydel + 0.49f ;
   iv.ijk[2] = (fv.xyz[2] - daxes->zzorg) / daxes->zzdel + 0.49f ;

        if( iv.ijk[0] < 0            ) iv.ijk[0] = 0 ;
   else if( iv.ijk[0] > daxes->nxx-1 ) iv.ijk[0] = daxes->nxx-1 ;

        if( iv.ijk[1] < 0            ) iv.ijk[1] = 0 ;
   else if( iv.ijk[1] > daxes->nyy-1 ) iv.ijk[1] = daxes->nyy-1 ;

        if( iv.ijk[2] < 0            ) iv.ijk[2] = 0 ;
   else if( iv.ijk[2] > daxes->nzz-1 ) iv.ijk[2] = daxes->nzz-1 ;

#if defined(USE_TRACING) && 0
INFO_message("THD_3dmm_to_3dind_no_wod: fv=%f %f %f  iv=%d %d %d  from %s",
             fv.xyz[0],fv.xyz[1],fv.xyz[2] , iv.ijk[0],iv.ijk[1],iv.ijk[2] , DBROUT ) ;
#endif

   return iv ;
}

/*---------------------------------------------------------------------
   convert from input image oriented x,y,z to Dicom x,y,z
     (x axis = R->L , y axis = A->P , z axis = I->S)

   N.B.: image distances are oriented the same as Dicom,
         just in a permuted order.
-----------------------------------------------------------------------*/

THD_fvec3 THD_3dmm_to_dicomm( THD_3dim_dataset *dset , THD_fvec3 imv )
{
   THD_fvec3 dicv ;
   float xim,yim,zim , xdic=0,ydic=0,zdic=0 ;

   xim = imv.xyz[0] ; yim = imv.xyz[1] ; zim = imv.xyz[2] ;

   switch( dset->daxes->xxorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: xdic = xim ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: ydic = xim ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: zdic = xim ; break ;

      default: THD_FATAL_ERROR("illegal xxorient code") ;
   }

   switch( dset->daxes->yyorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: xdic = yim ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: ydic = yim ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: zdic = yim ; break ;

      default: THD_FATAL_ERROR("illegal yyorient code") ;
   }

   switch( dset->daxes->zzorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: xdic = zim ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: ydic = zim ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: zdic = zim ; break ;

      default: THD_FATAL_ERROR("illegal zzorient code") ;
   }

   dicv.xyz[0] = xdic ; dicv.xyz[1] = ydic ; dicv.xyz[2] = zdic ;
   return dicv ;
}

/*---------------------------------------------------------------------
   convert to input image oriented x,y,z from Dicom x,y,z
-----------------------------------------------------------------------*/

THD_fvec3 THD_dicomm_to_3dmm( THD_3dim_dataset *dset , THD_fvec3 dicv )
{
   THD_fvec3 imv ;
   float xim,yim,zim , xdic,ydic,zdic ;

   xdic = dicv.xyz[0] ; ydic = dicv.xyz[1] ; zdic = dicv.xyz[2] ;

   switch( dset->daxes->xxorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: xim = xdic ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: xim = ydic ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: xim = zdic ; break ;

      default: THD_FATAL_ERROR("illegal xxorient code") ;
   }

   switch( dset->daxes->yyorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: yim = xdic ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: yim = ydic ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: yim = zdic ; break ;

      default: THD_FATAL_ERROR("illegal yyorient code") ;
   }

   switch( dset->daxes->zzorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: zim = xdic ; break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: zim = ydic ; break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: zim = zdic ; break ;

      default: THD_FATAL_ERROR("illegal zzorient code") ;
   }

   imv.xyz[0] = xim ; imv.xyz[1] = yim ; imv.xyz[2] = zim ;
   return imv ;
}

/*---------------------------------------------------------------------*/

THD_ivec3 THD_fdind_to_3dind( FD_brick *br , THD_ivec3 ib )
{
   THD_ivec3 id ;
   int qq , ax ;

   for( qq=0 ; qq < 3 ; qq++ ){
      ax = abs( br->a123.ijk[qq] ) - 1 ;   /* 0,1,2, for x,y,z */

      if( br->a123.ijk[qq] > 0 ) id.ijk[ax] = ib.ijk[qq] ;
      else                       id.ijk[ax] = br->sxyz.ijk[ax] - ib.ijk[qq];
   }

   return id ;
}

THD_ivec3 THD_3dind_to_fdind( FD_brick *br , THD_ivec3 id )
{
   THD_ivec3 ib ;
   int qq , ax ;

   for( qq=0 ; qq < 3 ; qq++ ){
      ax = abs( br->a123.ijk[qq] ) - 1 ;

      if( br->a123.ijk[qq] > 0 ) ib.ijk[qq] = id.ijk[ax] ;
      else                       ib.ijk[qq] = br->sxyz.ijk[ax] - id.ijk[ax];
   }

   return ib ;
}

/*---------------------------------------------------------------------*/

THD_fvec3 THD_fdfind_to_3dfind( FD_brick *br , THD_fvec3 ib ) /* 30 Aug 2001 */
{
   THD_fvec3 id ;
   int qq , ax ;

   for( qq=0 ; qq < 3 ; qq++ ){
      ax = abs( br->a123.ijk[qq] ) - 1 ;   /* 0,1,2, for x,y,z */

      if( br->a123.ijk[qq] > 0 ) id.xyz[ax] = ib.xyz[qq] ;
      else                       id.xyz[ax] = br->sxyz.ijk[ax] - ib.xyz[qq];
   }

   return id ;
}

THD_fvec3 THD_3dfind_to_fdfind( FD_brick *br , THD_fvec3 id ) /* 30 Aug 2001 */
{
   THD_fvec3 ib ;
   int qq , ax ;

   for( qq=0 ; qq < 3 ; qq++ ){
      ax = abs( br->a123.ijk[qq] ) - 1 ;

      if( br->a123.ijk[qq] > 0 ) ib.xyz[qq] = id.xyz[ax] ;
      else                       ib.xyz[qq] = br->sxyz.ijk[ax] - id.xyz[ax];
   }

   return ib ;
}

/*********************************************************************
   Convenience is bliss. Coordinate inter-conversion routines.
**********************************************************************/

void AFNI_ijk_to_xyz( THD_3dim_dataset * dset ,
                      int ii , int jj , int kk ,
                      float * xx , float * yy , float * zz )
{
   THD_fvec3 fv ;

   if( ! ISVALID_DSET(dset) ) return ;

   fv  = THD_3dind_to_3dmm( dset , TEMP_IVEC3(ii,jj,kk) ) ;
   *xx = fv.xyz[0] ;
   *yy = fv.xyz[1] ;
   *zz = fv.xyz[2] ;
   return ;
}

void AFNI_xyz_to_ijk( THD_3dim_dataset * dset ,
                      float xx , float yy , float zz ,
                      int * ii , int * jj , int * kk  )
{
   THD_ivec3 iv ;

   if( ! ISVALID_DSET(dset) ) return ;

   iv  = THD_3dmm_to_3dind ( dset , TEMP_FVEC3(xx,yy,zz) ) ;
   *ii = iv.ijk[0] ;
   *jj = iv.ijk[1] ;
   *kk = iv.ijk[2] ;
   return ;
}

void AFNI_xyz_to_dicomm( THD_3dim_dataset * dset ,
                         float xx , float yy , float zz ,
                         float * xd , float * yd , float * zd )
{
   THD_fvec3 fv ;

   if( ! ISVALID_DSET(dset) ) return ;

   fv  = THD_3dmm_to_dicomm( dset , TEMP_FVEC3(xx,yy,zz) ) ;
   *xd = fv.xyz[0] ;
   *yd = fv.xyz[1] ;
   *zd = fv.xyz[2] ;
   return ;
}

void AFNI_dicomm_to_xyz( THD_3dim_dataset * dset ,
                         float xd , float yd , float zd ,
                         float * xx , float * yy , float * zz )
{
   THD_fvec3 fv ;

   if( ! ISVALID_DSET(dset) ) return ;

   fv  = THD_3dmm_to_dicomm( dset , TEMP_FVEC3(xd,yd,zd) ) ;
   *xx = fv.xyz[0] ;
   *yy = fv.xyz[1] ;
   *zz = fv.xyz[2] ;
   return ;
}

/*-------------------------------------------------------------------*/

void THD_coorder_fill( char *in_orcode , THD_coorder *cord )
{
   char acod , orcode[4] ;
   int xx,yy,zz , ss1,ss2,ss3 , ii,ll ;

   if( cord == NULL ) return ;

   /* default values */

   cord->xxsign = cord->yysign = cord->zzsign = 1 ;
   cord->first  = 0 ;
   cord->second = 1 ;
   cord->third  = 2 ;
   cord->xxor   = ORI_R2L_TYPE ;
   cord->yyor   = ORI_A2P_TYPE ;
   cord->zzor   = ORI_I2S_TYPE ;
   strcpy(cord->orcode,"RAI") ;

   /* check string for OKness */

   if( in_orcode == NULL ) return ;
   strncpy(orcode,in_orcode,3) ; orcode[3] = '\0' ;
   ll = strlen(orcode) ; if( ll != 3 ) return ;
   for( ii=0 ; ii < 3 ; ii++ ) orcode[ii] = toupper(orcode[ii]) ;
   if( strncmp(orcode,"FLI",3) == 0 ) strcpy(orcode,"LPI") ;

   /* extract direction codes */

   acod = orcode[0] ; xx = ORCODE(acod) ;
   acod = orcode[1] ; yy = ORCODE(acod) ;
   acod = orcode[2] ; zz = ORCODE(acod) ;

   /* check direction codes for OKness */

   if( xx<0 || yy<0 || zz<0 || ! OR3OK(xx,yy,zz) ) return ;

   /* all is OK.  get signs of orientations */

   ss1 = (ORIENT_sign[xx] == '-') ? -1 : 1 ;
   ss2 = (ORIENT_sign[yy] == '-') ? -1 : 1 ;
   ss3 = (ORIENT_sign[zz] == '-') ? -1 : 1 ;

   /* whose on first? */

   cord->first  = xx / 2 ;
   cord->second = yy / 2 ;
   cord->third  = zz / 2 ;

   cord->xxsign = (cord->first ==0) ? ss1
                 :(cord->second==0) ? ss2 : ss3 ;

   cord->yysign = (cord->first ==1) ? ss1
                 :(cord->second==1) ? ss2 : ss3 ;

   cord->zzsign = (cord->first ==2) ? ss1
                 :(cord->second==2) ? ss2 : ss3 ;

   cord->xxor = xx ;
   cord->yyor = yy ;
   cord->zzor = zz ;

   strcpy(cord->orcode,orcode) ;
   return ;
}

/*---------------------------------------------------------------------
   convert to output order x,y,z from Dicom x,y,z
-----------------------------------------------------------------------*/

void THD_dicom_to_coorder( THD_coorder *cord ,
                           float *xx , float *yy , float *zz )
{
   float xval , yval , zval ;

   if( cord == NULL ) return ;

   /* changes signs first */

   xval = cord->xxsign * (*xx) ;
   yval = cord->yysign * (*yy) ;
   zval = cord->zzsign * (*zz) ;

   /* scramble order */

   *xx = (cord->first == 0) ? xval
        :(cord->first == 1) ? yval : zval ;

   *yy = (cord->second == 0) ? xval
        :(cord->second == 1) ? yval : zval ;

   *zz = (cord->third == 0) ? xval
        :(cord->third == 1) ? yval : zval ;

   return ;
}

/*-------------------------------------------------------------------
   convert to Dicom x,y,z from output order x,y,z
---------------------------------------------------------------------*/

void THD_coorder_to_dicom( THD_coorder *cord ,
                           float *xx , float *yy , float *zz )
{
   float xval , yval , zval ;

   if( cord == NULL ) return ;

   /* unscramble order */

   xval = (cord->first  == 0) ? (*xx)
         :(cord->second == 0) ? (*yy) : (*zz) ;

   yval = (cord->first  == 1) ? (*xx)
         :(cord->second == 1) ? (*yy) : (*zz) ;

   zval = (cord->first  == 2) ? (*xx)
         :(cord->second == 2) ? (*yy) : (*zz) ;

   /* change signs */

   *xx = cord->xxsign * xval ;
   *yy = cord->yysign * yval ;
   *zz = cord->zzsign * zval ;

   return ;
}

/*---------------------------------------------------------------------
   Return rotation and shift param. to go from i, j, k to Cardinal
   Dicom x,y,z (non-oblique)
-----------------------------------------------------------------------*/
void THD_dicom_card_xform (THD_3dim_dataset *dset ,
                           THD_dmat33 *tmat, THD_dfvec3 *dics )
{

   THD_dfvec3 dicr;
   
   /* rotation business */
   switch( dset->daxes->xxorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: 
           tmat->mat[0][0] = dset->daxes->xxdel ; tmat->mat[0][1] = tmat->mat[0][2] = 0.0;
         dics->xyz[0] = dset->daxes->xxorg;      
         break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: 
           tmat->mat[1][0] = dset->daxes->xxdel ; tmat->mat[1][1] = tmat->mat[1][2] = 0.0;
         dics->xyz[1] = dset->daxes->xxorg;      
         break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: 
           tmat->mat[2][0] = dset->daxes->xxdel ; tmat->mat[2][1] = tmat->mat[2][2] = 0.0;
         dics->xyz[2] = dset->daxes->xxorg;      
         break ;
      default: THD_FATAL_ERROR("illegal xxorient code") ;
   }

   switch( dset->daxes->yyorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: 
           tmat->mat[0][1] = dset->daxes->yydel ; tmat->mat[0][0] = tmat->mat[0][2] = 0.0;
         dics->xyz[0] = dset->daxes->yyorg;      
         break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: 
           tmat->mat[1][1] = dset->daxes->yydel ; tmat->mat[1][0] = tmat->mat[1][2] = 0.0;
         dics->xyz[1] = dset->daxes->yyorg;      
         break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: 
           tmat->mat[2][1] = dset->daxes->yydel ; tmat->mat[2][0] = tmat->mat[2][2] = 0.0;
         dics->xyz[2] = dset->daxes->yyorg;      
         break ;
      
      default: THD_FATAL_ERROR("illegal yyorient code") ;
   }

   switch( dset->daxes->zzorient ){
      case ORI_R2L_TYPE:
      case ORI_L2R_TYPE: 
           tmat->mat[0][2] = dset->daxes->zzdel ; tmat->mat[0][0] = tmat->mat[0][1] = 0.0;
         dics->xyz[0] = dset->daxes->zzorg;      
         break ;
      case ORI_P2A_TYPE:
      case ORI_A2P_TYPE: 
           tmat->mat[1][2] = dset->daxes->zzdel ; tmat->mat[1][0] = tmat->mat[1][1] = 0.0;
         dics->xyz[1] = dset->daxes->zzorg;      
         break ;
      case ORI_I2S_TYPE:
      case ORI_S2I_TYPE: 
           tmat->mat[2][2] = dset->daxes->zzdel ; tmat->mat[2][0] = tmat->mat[2][1] = 0.0;
         dics->xyz[2] = dset->daxes->zzorg;      
         break ;
      default: THD_FATAL_ERROR("illegal zzorient code") ;
   }
   
   return  ;
}

/* -------------------------------------------------------------------------
 * THD_dicom_real_to_card - convert ijk_to_dicom_real coords to ijk_to_dicom
 *                          version (both input and output coords are dicom)
 *
 * Convert input coords->xyz from ijk_to_dicom_real to ijk_to_dicom.
 * This function essentially just modifies the coords vector.
 * If rnd is set, round to the nearest voxel (i.e., round ijk indices).
 *
 *
 *    method: inverse warp dicom_real_xyz to indices
 *            possibly round
 *            warp to dicom_xyz
 *
 * Note: it is okay if the input or output coordinates are outside of the
 *       dataset's field of view.
 *
 * return 0 on success                                  23 Mar 2020 [rickr]
 * -------------------------------------------------------------------------
 */
int THD_dicom_real_to_card(THD_3dim_dataset *dset, THD_fvec3 * coords, int rnd)
{
   THD_ivec3 iv={{0,0,0}};
   float fi, fj, fk;
   mat44 r2i;   /* real xyz to ijk (inverse of ijk_to_dicom_real */

   ENTRY("THD_dicom_real_to_card");

   /* check for bad inputs */
   if ( !dset || !dset->daxes ) {
      WARNING_message("THD_dicom_real_to_card: null input");
      RETURN(1);
   }
   if ( !ISVALID_MAT44(dset->daxes->ijk_to_dicom) ) {
      WARNING_message("THD_dicom_real_to_card: invalid ijk_to_dicom");
      RETURN(1);
   }
   if ( !ISVALID_MAT44(dset->daxes->ijk_to_dicom_real) ) {
      WARNING_message("THD_dicom_real_to_card: invalid ijk_to_dicom_real");
      RETURN(1);
   }

   /* get dicom_to_ijk mat */
   r2i = nifti_mat44_inverse(dset->daxes->ijk_to_dicom_real);

   /* apply r2i to input coords */
   MAT44_VEC(r2i, coords->xyz[0], coords->xyz[1], coords->xyz[2], fi, fj, fk);

   /* possibly round to nearest voxel center (okay if outside dset grid) */
   if( rnd ) { fi = roundf(fi); fj = roundf(fj); fk = roundf(fk); }

   /* apply ijk_to_dicom to index coords */
   MAT44_VEC(dset->daxes->ijk_to_dicom, fi, fj, fk,
             coords->xyz[0], coords->xyz[1], coords->xyz[2]);

   RETURN(0);
}

/*---------------------------------------------------------------------
   Return rotation and shift param. to go from i, j, k to Real
   Dicom x,y,z 
-----------------------------------------------------------------------*/
void THD_dicom_real_xform(THD_3dim_dataset *dset ,
                          THD_dmat33 *tmat, THD_dfvec3 *dics )
{
   if (  !dset || !dset->daxes || 
         !ISVALID_MAT44(dset->daxes->ijk_to_dicom_real)) {
      THD_FATAL_ERROR("null input or no valid ijk_to_dicom_real") ;
   }
   
   UNLOAD_MAT44(dset->daxes->ijk_to_dicom_real,
      tmat->mat[0][0], tmat->mat[0][1], tmat->mat[0][2], dics->xyz[0],    \
      tmat->mat[1][0], tmat->mat[1][1], tmat->mat[1][2], dics->xyz[1],    \
      tmat->mat[2][0], tmat->mat[2][1], tmat->mat[2][2], dics->xyz[2]   );
   return;
}
#define MAXNUM(a,b) ( (a) > (b) ? (a):(b))
#define MAX3(a,b,c) ( (MAXNUM(a,b)) > (MAXNUM(a,c)) ? (MAXNUM(a,b)):(MAXNUM(a,c)))
#define MINNUM(a,b) ( (a) < (b) ? (a):(b))
#define MIN3(a,b,c) ( (MINNUM(a,b)) < (MINNUM(a,c)) ? (MINNUM(a,b)):(MINNUM(a,c)))

/* compute angle of greatest obliquity given transformation matrix */

float THD_compute_oblique_angle(mat44 ijk_to_dicom44, int verbose)
{
   float dxtmp, dytmp, dztmp ;
   float xmax, ymax, zmax ;
   float fig_merit, ang_merit ;


   dxtmp = sqrt ( ijk_to_dicom44.m[0][0] * ijk_to_dicom44.m[0][0] +
                  ijk_to_dicom44.m[1][0] * ijk_to_dicom44.m[1][0] +
                  ijk_to_dicom44.m[2][0] * ijk_to_dicom44.m[2][0] ) ;

   xmax = MAX3(fabs(ijk_to_dicom44.m[0][0]),fabs(ijk_to_dicom44.m[1][0]),fabs(ijk_to_dicom44.m[2][0])) / dxtmp ;

   dytmp = sqrt ( ijk_to_dicom44.m[0][1] * ijk_to_dicom44.m[0][1] +
                  ijk_to_dicom44.m[1][1] * ijk_to_dicom44.m[1][1] +
                  ijk_to_dicom44.m[2][1] * ijk_to_dicom44.m[2][1] ) ;

   ymax = MAX3(fabs(ijk_to_dicom44.m[0][1]),
               fabs(ijk_to_dicom44.m[1][1]),
               fabs(ijk_to_dicom44.m[2][1])) / dytmp ;

   dztmp = sqrt ( ijk_to_dicom44.m[0][2] * ijk_to_dicom44.m[0][2] +
                  ijk_to_dicom44.m[1][2] * ijk_to_dicom44.m[1][2] +
                  ijk_to_dicom44.m[2][2] * ijk_to_dicom44.m[2][2] ) ;

   zmax = MAX3(fabs(ijk_to_dicom44.m[0][2]),
               fabs(ijk_to_dicom44.m[1][2]),
               fabs(ijk_to_dicom44.m[2][2])) / dztmp ;

   fig_merit = MIN3(xmax,ymax,zmax) ;
   ang_merit = acos (fig_merit) * 180.0 / 3.141592653 ;

   if (fabs(ang_merit) > .01) {
     if ( verbose ) INFO_message("%f degrees from plumb.\n",ang_merit ) ;
   }
   else 
      ang_merit = 0.0;
   return(ang_merit);
}

/*
  [PT: Nov 3, 2020] more funcs for dealing with obliquity matrix
  (dset->daxes->ijk_to_dicom_real)

  [PT: Nov 12, 2020] adopting the following language/terminology for
  systematically approaching these mats, summarizing conversation with
  RCR:

  aform_real : mat44 that is the AFNI equivalent of NIFTI sform matrix
               ('a'form for AFNI), what we typically call
               ijk_to_dicom_real in the AFNI extension/header; likely
               only difference is that aform_* are RAI, while sform
               matrix is LPI (sigh).  aform_real, like sform, is not
               restricted to being orthogonal -- can represent any
               linear affine transform.

  aform_orth : mat44 that is the orthogonalized version of aform_real,
               which should correspond to the NIFTI quaternion, since
               the NIFTI quaternion is inherently orthogonalized when
               created from the NIFTI sform matrix; we even use the
               NIFTI2 mat44->quaternion->mat44 functions to calc
               aform_orth, so it really should agree, EXCEPT that,
               like all good things, it is RAI.  While orthogonal,
               this matrix might still contain obliquity information
               (i.e., rotation information: it need not be diagonal).

               In most cases in AFNI-generated programs, aform_orth
               should be the same as aform_real (as of the writing of
               this note).  But we allow for reading in more general
               matrix info.  

  aform_card : mat44 that is the unrotated (diagonalized) form of
               aform_orth, what we typically call ijk_to_dicom in the
               AFNI extension/header.  This is the matrix mostly used
               to display data in the GUI, and when we ignore
               obliquity information in transformations, etc.  Is also
               RAI.  We aim now to create this in a way that the (i*,
               j*, k*) location where (x, y, z) = (0, 0, 0) in
               aform_orth is preserved.  That is, we preserve the
               coordinate origin (hoping it is good!).
*/

// get the integer form of orientation in RLPAIS ordering.  So, a dset
// with "RAI" -> {0, 3, 4}.
void THD_orient_to_int_rlpais( char ochar[4], int oint[3] )
{
   int i, j;

   ENTRY("THD_orient_to_int_rlpais");

   // translate new orient to int form
   for( i=0 ; i<3 ; i++)
      for( j=0 ; j<6 ; j++ ) {
         if( strncmp(&ochar[i], &ORIENT_first[j], 1) == 0 ) {
            oint[i] = j;
            break;
         }
         if( j == 6 )
            ERROR_message("Perm calc, couldn't find orient char match\n"
                          "\tCheck inp orient string: %s", ochar);
      }

   EXRETURN;
}

// get the char form of orientation from int array in RLPAIS ordering.
// So, a dset with {0, 3, 4} -> "RAI".
void THD_int_to_orient_rlpais( int oint[3], char ochar[4])
{
   int i, j;

   ENTRY("THD_int_to_orient_rlpais");

   for( i=0 ; i<3 ; i++)
      ochar[i] = ORIENT_first[oint[i]];
   ochar[3] = '\0';

   EXRETURN;
}

/* Check if orient is valid: must contain exactly one member of each
   of the following pairs:  LR, AP, IS.
   Return 0 if valid, 1 otherwise.
*/
int is_valid_orient_char(char ochar[3]) 
{
   int ii; 
   int oint[3] = {0, 0, 0}; 

   ENTRY("is_valid_orient_char");

   THD_orient_to_int_rlpais(ochar, oint);  // get int form of orient
   ii = is_valid_orient_int(oint);         // ... and check this

   return ii;
}

/* Check if orient is valid: must contain exactly one member of each
   of the following pairs:  01, 23, 45.
   Return 0 if valid, 1 otherwise.
*/
int is_valid_orient_int(int oint[3]) 
{
   int i, j; 
   int score[3] = {0, 0, 0}; 
   int score_sum = 0;
   char ochar[4]; 

   ENTRY("is_valid_orient_int");

   for( i=0 ; i<3 ; i++ )
      score[ ORIENT_xyzint[ oint[i]]-1 ] = 1;

   for( i=0 ; i<3 ; i++ )
      score_sum+= score[i];
   
   THD_int_to_orient_rlpais( oint, ochar );

   if( score_sum == 3 )  return 1;   // valid
   else                  return 0;   // invalid
}

// Calc permutation mat33 needed when changing dset orientation from
// orient A (e.g., "SLP") to orient B (e.g., "RAI").  
mat33 THD_char_reorient_perm_mat33(char *ocharA, char *ocharB)
{
   int   i, j;
   int   ointA[3]  = {-1, -1, -1};  // int form of orient
   int   ointB[3]  = {-1, -1, -1};  
   mat33 P33;

   ENTRY("THD_char_reorient_perm_mat33");

   INFO_message("CHECKING %s -> %s", ocharA, ocharB);
   if( !is_valid_orient_char(ocharA) || !is_valid_orient_char(ocharB) )
      ERROR_exit("Invalid orientation for permuting: %s -> %s", 
                 ocharA, ocharB);

   // translate new orient to int form
   THD_orient_to_int_rlpais(ocharA, ointA);
   THD_orient_to_int_rlpais(ocharB, ointB);

   P33 = THD_int_reorient_perm_mat33(ointA, ointB);

   return P33;
}

// Calc permutation mat33 needed when changing dset orientation from
// orient A (e.g., "531") to orient B (e.g., "024").  
mat33 THD_int_reorient_perm_mat33(int *ointA, int *ointB)
{
   int   i, j;
   mat33 PA, PB, PAINV, PBINV;   // intermediate
   mat33 P33;
   char ocharA[4], ocharB[4];

   char ochar_rai[4] = "RAI\0";
   int   oint_rai[3] = {-1, -1, -1};  // int form of orient

   ENTRY("THD_int_reorient_perm_mat33");

   THD_orient_to_int_rlpais(ochar_rai, oint_rai);

   LOAD_ZERO_MAT33(PA);  // init mat
   LOAD_ZERO_MAT33(PB);  // init mat
   LOAD_ZERO_MAT33(P33);  // init mat

   if( !is_valid_orient_int(ointA) ) {
      THD_int_to_orient_rlpais( ointA, ocharA );
      ERROR_exit("Dset has invalid orientation for permuting: %s", ocharA);
   }

   if( !is_valid_orient_int(ointB) ) {
      THD_int_to_orient_rlpais( ointB, ocharB );
      ERROR_exit("Specified orientation is invalid for permuting: %s", ocharB);
   }

   // make permutation matrix; will be applied as first arg in
   // multiplications, such as: MAT33_MUL(P33, dset_mat33)
   for( i=0 ; i<3 ; i++ )
      for( j=0 ; j<3 ; j++ ){
         if( oint_rai[i] == ointA[j] )
            PA.m[i][j] = 1;
         else if ( ORIENT_OPPOSITE(oint_rai[i]) == ointA[j] ) 
            PA.m[i][j] = -1;
      }
   for( i=0 ; i<3 ; i++ )
      for( j=0 ; j<3 ; j++ ){
         if( oint_rai[i] == ointB[j] )
            PB.m[i][j] = 1;
         else if ( ORIENT_OPPOSITE(oint_rai[i]) == ointB[j] ) 
            PB.m[i][j] = -1;
      }

   PAINV = MAT33_INV(PA);
   P33   = MAT33_MUL(PB, PAINV);

   return P33;
}


/* OLD, apparently broken way of thinking about this


// Calc permutation mat33 needed when changing dset orientation from
// orient A (e.g., "531") to orient B (e.g., "024").  
mat33 THD_int_reorient_perm_mat33(int *ointA, int *ointB)
{
   int   i, j;
   mat33 P33;
   char ocharA[4], ocharB[4];

   ENTRY("THD_int_reorient_perm_mat33");

   LOAD_ZERO_MAT33(P33);  // init mat

   if( !is_valid_orient_int(ointA) ) {
      THD_int_to_orient_rlpais( ointA, ocharA );
      ERROR_exit("Dset has invalid orientation for permuting: %s", ocharA);
   }

   if( !is_valid_orient_int(ointB) ) {
      THD_int_to_orient_rlpais( ointB, ocharB );
      ERROR_exit("Specified orientation is invalid for permuting: %s", ocharB);
   }

   // make permutation matrix; will be applied as first arg in
   // multiplications, such as: MAT33_MUL(P33, dset_mat33)
   for( i=0 ; i<3 ; i++ )
      for( j=0 ; j<3 ; j++ ){
         if( ointA[i] == ointB[j] )
            P33.m[i][j] = 1;
         else if ( ORIENT_OPPOSITE(ointA[i]) == ointB[j] ) 
            P33.m[i][j] = -1;
      }

   return P33;
}
*/


// Calc permutation mat33 needed for ijk_to_dicom_real when changing
// orientation from what a current dset has to some new_ori.  'P33' is
// essentially calc'ed here for input dset and new_ori.
mat33 THD_dset_reorient_perm_mat33( THD_3dim_dataset *dsetA, char *ocharB)
{
   int   i, j;
   int   ointA[3] = {-1, -1, -1};  // current dset orient (int form)
   int   ointB[3] = {-1, -1, -1};  // new orient (int form)
   mat33 P33;

   ENTRY("THD_dset_reorient_perm_mat33");

   LOAD_ZERO_MAT33(P33);  // init mat

   if( !ISVALID_DSET(dsetA) ) return(P33);

   THD_fill_orient_int_3_rlpais( dsetA->daxes, ointA );  // dset orient as ints
   THD_orient_to_int_rlpais(ocharB, ointB);              // new orient as ints

   P33 = THD_int_reorient_perm_mat33(ointA, ointB);

   return P33;
}

// apply permutation+new orientation to dset, calc+return new
// ijk_to_dicom_real mat
mat44 THD_refit_orient_ijk_to_dicom_real( THD_3dim_dataset *dsetA, 
                                          char *ocharB )
{
   int   i, j;
   float origA[3], origB[3];
   mat33 P33;
   mat33 dsetA_mat33, dsetA_mat33_P; 
   mat44 dsetA_mat44_P;

   ENTRY("THD_refit_orient_ijk_to_dicom_real");

   ZERO_MAT44(dsetA_mat44_P);  // init mat

   // checks
   if( !ISVALID_DSET(dsetA) || oblique_report_repeat==0 ) return(dsetA_mat44_P);
   THD_check_oblique_field(dsetA);

   MAT44_TO_MAT33(dsetA->daxes->ijk_to_dicom_real, dsetA_mat33); // get current

   P33 = THD_dset_reorient_perm_mat33( dsetA, ocharB);  // perm from old to new

   // INITIAL 
   //INFO_message("OLD mats");
   //DUMP_MAT44("IJK_TO_DICOM_REAL", dsetA->daxes->ijk_to_dicom_real);
   //DUMP_MAT44("IJK_TO_DICOM", dsetA->daxes->ijk_to_dicom);

   for( i=0 ; i<3 ; i++ ) 
      origA[i] = dsetA->daxes->ijk_to_dicom_real.m[i][3];
   MAT33_VEC(P33, origA[0], origA[1], origA[2], 
             origB[0], origB[1], origB[2]);

   //INFO_message("BEFORE AND AFTER");
   //INFO_message("%f %f %f", origA[0], origA[1], origA[2]);
   //INFO_message("%f %f %f", origB[0], origB[1], origB[2]);

   // apply permutation to mat33, and then make M44
   dsetA_mat33_P = MAT33_MUL(P33, dsetA_mat33);
   MAT33_TO_MAT44(dsetA_mat33_P, dsetA_mat44_P);
   LOAD_MAT44_VEC(dsetA_mat44_P, 
                  origB[0], origB[1], origB[2]);

   // FINAL
   //INFO_message("NEW mat");
   //DUMP_MAT44("", dsetA_mat44_P);

   return dsetA_mat44_P;
}

/*
  [PT: Nov 16, 2020] Use the fact that nifti_mat44_to_quatern produces
  a quaternion representation of the input matrix that is
  orthogonalized, even if the input mat isn't. Thus, when converting
  quatern -> mat44, the output mat should be orthogonalized.

  Use this to calculate aform_orth (Mout) from aform_real (Min).
*/
mat44 nifti_orthogonalize_mat44( mat44 Min )
{
   mat44 Mout;
   float qb, qc, qd;
   float qx, qy, qz;
   float dx, dy, dz, qfac;

   nifti_mat44_to_quatern( Min,
                           &qb,  &qc,  &qd,
                           &qx,  &qy,  &qz,
                           &dx,  &dy,  &dz,  &qfac );
      
   Mout = nifti_quatern_to_mat44(  qb,  qc,  qd,
                                   qx,  qy,  qz,
                                   dx,  dy,  dz,  qfac );

   return Mout;
}

/*
  [PT: Nov 16, 2020] Test orthogonality by orthogonalizing the mat44,
  and then comparing sum of elementwise diffs.  Could be done
  differently, but typically this should be fine.
*/
int is_mat44_orthogonal(mat44 A)
{
   mat44 B;

   B = nifti_orthogonalize_mat44( A );

   return MAT44_FLEQ(A, B);
}


/*
  Calc the cardinal 3x3 matrix of a dset in RAI ordering based on
  voxelsize 

mat33 THD_dset_card_mat33( THD_3dim_dataset *dset )
{
   int i,j;
   mat33 M;
   THD_dmat33 tmat;
   THD_dfvec3 dics;

   THD_dicom_card_xform( dset,
                         &tmat, &dics );

   

   return M;
}

*/




void THD_report_obliquity(THD_3dim_dataset *dset)
{
   double angle;

   ENTRY("THD_report_obliquity");
   if(AFNI_yesenv("AFNI_NO_OBLIQUE_WARNING") || !OBL_report) EXRETURN;

   if( !ISVALID_DSET(dset) || oblique_report_repeat==0 ) EXRETURN;

   THD_check_oblique_field(dset); /* make sure oblique field is available*/
   angle = THD_compute_oblique_angle(dset->daxes->ijk_to_dicom_real, 0);
   if(angle == 0.0) EXRETURN;

   if(oblique_report_index<oblique_report_repeat) {
      if(first_oblique) {
         WARNING_message(
         "  If you are performing spatial transformations on an oblique dset,\n"
         "  such as %s,\n"
         "  or viewing/combining it with volumes of differing obliquity,\n"
         "  you should consider running: \n"
         "     3dWarp -deoblique \n"
         "  on this and  other oblique datasets in the same session.\n"
         " See 3dWarp -help for details.\n", DSET_BRIKNAME(dset));
         first_oblique = 0;
      }

      INFO_message("Oblique dataset:%s is %f degrees from plumb.\n",
        DSET_BRIKNAME(dset), angle  ) ;
      
   }


   oblique_report_index++;

   if(oblique_report_repeat2==-1) {   /* report obliquity n times, stop */
      if(oblique_report_index>oblique_report_repeat) 
         oblique_report_index = oblique_report_repeat;
      EXRETURN;
   }

   /* reset counter if needed*/
   if(oblique_report_index>=(oblique_report_repeat+oblique_report_repeat2))
      oblique_report_index = 0;

   EXRETURN;
}

/* set the number of times to report obliquity and 
   the number of oblique datasets to skip.
   If the first number is 0, don't report at all.
   If the second number is 0 (and the first number is not), always report.
   If the second number is -1, stop reporting after reporting the first n1
 */
void THD_set_oblique_report(int n1, int n2)
{
   oblique_report_repeat = n1;
   oblique_report_repeat2 = n2;
}


int THD_get_oblique_report()
{
   return(oblique_report_repeat);
}

void THD_reset_oblique_report_index()
{
   oblique_report_index = 0;
}

/* make daxes structure set to cardinal orientation versus oblique */
/* usually done if not already set (in older datasets) */
void THD_make_cardinal(THD_3dim_dataset *dset)
{
   THD_dmat33 tmat ;
   THD_dfvec3 tvec ;
   mat44 Tc, Tr;

   THD_dicom_card_xform(dset, &tmat, &tvec); 
   LOAD_MAT44(Tc, 
      tmat.mat[0][0], tmat.mat[0][1], tmat.mat[0][2], tvec.xyz[0],
      tmat.mat[1][0], tmat.mat[1][1], tmat.mat[1][2], tvec.xyz[1],
      tmat.mat[2][0], tmat.mat[2][1], tmat.mat[2][2], tvec.xyz[2]);
   dset->daxes->ijk_to_dicom_real = Tc;
/* DUMP_MAT44("thd_coords: set ijk_to_dicom_real",Tc) ; */
}

/* check the obliquity transformation field to see if it's valid */
/* if not, reset it to the cardinal field */
void THD_check_oblique_field(THD_3dim_dataset *dset)
{
   if( !ISVALID_MAT44(dset->daxes->ijk_to_dicom_real) )
      THD_make_cardinal(dset);
}

/* allow for updating of obliquity - IJK_TO_DICOM_REAL attribute and
   structure */
void THD_updating_obliquity(int update)
{
   oblique_update = update;
}

/* accessor function to get current status - can we update
   ijk_to_dicom_real*/
int THD_update_obliquity_status()
{
   return(oblique_update);
}
