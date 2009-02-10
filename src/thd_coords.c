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

   iv.ijk[0] = (fv.xyz[0] - daxes->xxorg) / daxes->xxdel + 0.499 ;
   iv.ijk[1] = (fv.xyz[1] - daxes->yyorg) / daxes->yydel + 0.499 ;
   iv.ijk[2] = (fv.xyz[2] - daxes->zzorg) / daxes->zzdel + 0.499 ;

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

   iv.ijk[0] = (fv.xyz[0] - daxes->xxorg) / daxes->xxdel + 0.499 ;
   iv.ijk[1] = (fv.xyz[1] - daxes->yyorg) / daxes->yydel + 0.499 ;
   iv.ijk[2] = (fv.xyz[2] - daxes->zzorg) / daxes->zzdel + 0.499 ;

        if( iv.ijk[0] < 0            ) iv.ijk[0] = 0 ;
   else if( iv.ijk[0] > daxes->nxx-1 ) iv.ijk[0] = daxes->nxx-1 ;

        if( iv.ijk[1] < 0            ) iv.ijk[1] = 0 ;
   else if( iv.ijk[1] > daxes->nyy-1 ) iv.ijk[1] = daxes->nyy-1 ;

        if( iv.ijk[2] < 0            ) iv.ijk[2] = 0 ;
   else if( iv.ijk[2] > daxes->nzz-1 ) iv.ijk[2] = daxes->nzz-1 ;

   return iv ;
}

/*--------------------------------------------------------------------*/

/* this version is without using wod dataxes     28 Sep 2004 [rickr] */

THD_ivec3 THD_3dmm_to_3dind_no_wod( THD_3dim_dataset *dset , THD_fvec3 fv )
{
   THD_dataxes *daxes ;
   THD_ivec3    iv ;

   daxes = dset->daxes ;

   iv.ijk[0] = (fv.xyz[0] - daxes->xxorg) / daxes->xxdel + 0.499 ;
   iv.ijk[1] = (fv.xyz[1] - daxes->yyorg) / daxes->yydel + 0.499 ;
   iv.ijk[2] = (fv.xyz[2] - daxes->zzorg) / daxes->zzdel + 0.499 ;

        if( iv.ijk[0] < 0            ) iv.ijk[0] = 0 ;
   else if( iv.ijk[0] > daxes->nxx-1 ) iv.ijk[0] = daxes->nxx-1 ;

        if( iv.ijk[1] < 0            ) iv.ijk[1] = 0 ;
   else if( iv.ijk[1] > daxes->nyy-1 ) iv.ijk[1] = daxes->nyy-1 ;

        if( iv.ijk[2] < 0            ) iv.ijk[2] = 0 ;
   else if( iv.ijk[2] > daxes->nzz-1 ) iv.ijk[2] = daxes->nzz-1 ;

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

void THD_report_obliquity(THD_3dim_dataset *dset)
{
   double angle;

   ENTRY("THD_report_obliquity");
   if(AFNI_yesenv("AFNI_NO_OBLIQUE_WARNING")) EXRETURN;

   if( !ISVALID_DSET(dset) || oblique_report_repeat==0 ) EXRETURN;

   THD_check_oblique_field(dset); /* make sure oblique field is available*/
   angle = THD_compute_oblique_angle(dset->daxes->ijk_to_dicom_real, 0);
   if(angle == 0.0) EXRETURN;

   if(oblique_report_index<oblique_report_repeat) {
      if(first_oblique) {
         WARNING_message(
         "  If you are performing spatial transformations on an oblique dset, \n"
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
}

/* check the obliquity transformation field to see if it's valid */
/* if not, reset it to the cardinal field */
void THD_check_oblique_field(THD_3dim_dataset *dset)
{
   if( !ISVALID_MAT44(dset->daxes->ijk_to_dicom_real) )
      THD_make_cardinal(dset);
}
