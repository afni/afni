/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

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

THD_fvec3 THD_3dfind_to_3dmm( THD_3dim_dataset * dset ,
                              THD_fvec3 iv )
{
   THD_dataxes * daxes ;
   THD_fvec3     fv ;

   daxes = CURRENT_DAXES(dset) ;

   fv.xyz[0] = daxes->xxorg + iv.xyz[0] * daxes->xxdel ;
   fv.xyz[1] = daxes->yyorg + iv.xyz[1] * daxes->yydel ;
   fv.xyz[2] = daxes->zzorg + iv.xyz[2] * daxes->zzdel ;
   return fv ;
}

/*------------------------------------------------------------------*/

THD_fvec3 THD_3dind_to_3dmm( THD_3dim_dataset * dset ,
                             THD_ivec3 iv )
{
   THD_dataxes * daxes ;
   THD_fvec3     fv ;

   daxes = CURRENT_DAXES(dset) ;

   fv.xyz[0] = daxes->xxorg + iv.ijk[0] * daxes->xxdel ;
   fv.xyz[1] = daxes->yyorg + iv.ijk[1] * daxes->yydel ;
   fv.xyz[2] = daxes->zzorg + iv.ijk[2] * daxes->zzdel ;
   return fv ;
}

/*--------------------------------------------------------------------*/

THD_fvec3 THD_3dmm_to_3dfind( THD_3dim_dataset * dset ,
                              THD_fvec3 fv )
{
   THD_dataxes * daxes ;
   THD_fvec3     iv ;

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

THD_ivec3 THD_3dmm_to_3dind( THD_3dim_dataset * dset ,
                             THD_fvec3 fv )
{
   THD_dataxes * daxes ;
   THD_ivec3     iv ;

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

/*---------------------------------------------------------------------
   convert from input image oriented x,y,z to Dicom x,y,z
     (x axis = R->L , y axis = A->P , z axis = I->S)

   N.B.: image distances are oriented the same as Dicom,
         just in a permuted order.
-----------------------------------------------------------------------*/

THD_fvec3 THD_3dmm_to_dicomm( THD_3dim_dataset * dset ,
                              THD_fvec3 imv )
{
   THD_fvec3 dicv ;
   float xim,yim,zim , xdic,ydic,zdic ;

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

THD_fvec3 THD_dicomm_to_3dmm( THD_3dim_dataset * dset ,
                              THD_fvec3 dicv )
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

THD_ivec3 THD_fdind_to_3dind( FD_brick * br , THD_ivec3 ib )
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

THD_ivec3 THD_3dind_to_fdind( FD_brick * br , THD_ivec3 id )
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

THD_fvec3 THD_fdfind_to_3dfind( FD_brick * br , THD_fvec3 ib ) /* 30 Aug 2001 */
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

THD_fvec3 THD_3dfind_to_fdfind( FD_brick * br , THD_fvec3 id ) /* 30 Aug 2001 */
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

void THD_coorder_fill( char * in_orcode , THD_coorder * cord )
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

void THD_dicom_to_coorder( THD_coorder * cord ,
                           float * xx , float * yy , float * zz )
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

void THD_coorder_to_dicom( THD_coorder * cord ,
                           float * xx , float * yy , float * zz )
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
