/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"
#include "thd.h"

/************************************************************************
  July 1997: moved warp manipulations routines from afni_warp.c to here.
*************************************************************************/

/*------------------------------------------------------------------------
  Inputs: DICOM coordinate warp from old_dset to new_dset
  Output: voxel index coordinates warp that can actually be used
--------------------------------------------------------------------------*/

THD_warp * AFNI_make_voxwarp( THD_warp * inwarp ,
                              THD_3dim_dataset * old_dset ,
                              THD_3dim_dataset * new_dset  )
{
   THD_warp * newwarp ;
   THD_linear_mapping * map ;
   THD_dataxes * new_daxes ;

   newwarp       = myRwcNew( THD_warp ) ;
   newwarp->type = inwarp->type ;
   new_daxes     = CURRENT_DAXES(new_dset) ;

   switch( inwarp->type ){

      default:{
         fprintf(stderr,"\a\n*** ILLEGAL warp code!!! %d\n",inwarp->type) ;
         sleep(1) ; EXIT(1) ;
      }
      break ;

      case WARP_AFFINE_TYPE:{

         map = AFNI_make_voxmap( &(inwarp->rig_bod.warp),
                                 old_dset->daxes , new_daxes ) ;

         /* load the (inclusive) voxel index ranges into the affine map */

         LOAD_FVEC3( map->bot, 0,0,0 ) ;
         LOAD_FVEC3( map->top, new_daxes->nxx-1,
                               new_daxes->nyy-1, new_daxes->nzz-1 ) ;


         newwarp->rig_bod.warp = *map ;

         myRwcFree( map ) ;
      }
      break ;

      case WARP_TALAIRACH_12_TYPE:{
         int iw ;
         for( iw=0 ; iw < 12 ; iw++ ){
            map = AFNI_make_voxmap( &(inwarp->tal_12.warp[iw]) ,
                                    old_dset->daxes , new_daxes ) ;

            map->bot = THD_dicomm_to_3dmm(new_dset,inwarp->tal_12.warp[iw].bot);
            map->top = THD_dicomm_to_3dmm(new_dset,inwarp->tal_12.warp[iw].top);

            map->bot = THD_3dmm_to_3dfind( new_dset , map->bot ) ;
            map->top = THD_3dmm_to_3dfind( new_dset , map->top ) ;

            newwarp->tal_12.warp[iw] = *map ;

            myRwcFree( map ) ;
         }
      }
      break ;
   }

   return newwarp ;
}

/*----------------------------------------------------------------*/

#ifdef SOLARIS
void do_nothing(int iii){return ;}  /* for compiler optimization error on Sun */
#else
#define do_nothing(iii)
#endif

/*-----------------------------------------------------------------
  This routine takes as input a linear mapping from Dicom to Dicom
  coordinates, and computes a linear mapping from voxel-index
  to voxel-index coordinates as output.
  Pointers to the old and new dataset axes structures are also input.

  x3dmm_new = [new_dicomm_to_3dmm]
              ( [dd_trans] [old_3dmm_to_dicomm] x3dmm_old - dd_base )

  is the conversion between true (3dmm) coordinates.  In turn, we also
  have

  x3dfind_new = [new_scale] ( x3dmm_new - new_origin )
  x3dmm_old   = [old_scale] x3dfind_old + old_origin

  as the transformations between voxel-index (3dfind) coordinates
  and true (3dmm) coordinates.
--------------------------------------------------------------------*/

THD_linear_mapping * AFNI_make_voxmap( THD_linear_mapping * inmap ,
                                       THD_dataxes * old_daxes ,
                                       THD_dataxes * new_daxes  )
{
   THD_mat33 old_scale , old_3dmm_to_dicomm ,
             dd_trans  , new_scale , new_dicomm_to_3dmm ,
             mt ; /* temp matrix */

   THD_fvec3 dd_base , new_origin , old_origin , vt0,vt1,vt2 ;

   THD_linear_mapping * newmap ;

   /*--- set up the elements of the transformation ---*/

   dd_trans = inmap->mfor ;

   LOAD_DIAG_MAT( old_scale , old_daxes->xxdel ,
                              old_daxes->yydel ,
                              old_daxes->zzdel  ) ;

   LOAD_DIAG_MAT( new_scale , 1.0/new_daxes->xxdel ,     /* notice */
                              1.0/new_daxes->yydel ,     /* inversion */
                              1.0/new_daxes->zzdel  ) ;

   old_3dmm_to_dicomm = old_daxes->to_dicomm ;
   new_dicomm_to_3dmm = TRANSPOSE_MAT(new_daxes->to_dicomm) ; /* inversion */

   /* vector elements */

   dd_base = inmap->bvec ;                        /* in new dicomm */

   LOAD_FVEC3( new_origin , new_daxes->xxorg ,    /* in old 3dmm */
                            new_daxes->yyorg ,
                            new_daxes->zzorg  ) ;

   LOAD_FVEC3( old_origin , old_daxes->xxorg ,    /* in new 3dmm */
                            old_daxes->yyorg ,
                            old_daxes->zzorg  ) ;

   /* multiply the matrices together */

   mt = MAT_MUL( old_3dmm_to_dicomm , old_scale ) ;   do_nothing(0) ;
   mt = MAT_MUL( dd_trans , mt ) ;                    do_nothing(0) ;
   mt = MAT_MUL( new_dicomm_to_3dmm , mt ) ;          do_nothing(0) ;
   mt = MAT_MUL( new_scale , mt ) ;                   do_nothing(0) ;

   /* compute the new bvec */

   vt0 = MATVEC( old_3dmm_to_dicomm , old_origin ) ;
   vt0 = MATVEC( dd_trans , vt0 ) ;
   vt0 = MATVEC( new_dicomm_to_3dmm , vt0 ) ;
   vt0 = MATVEC( new_scale , vt0 ) ;

   vt1 = MATVEC( new_dicomm_to_3dmm , dd_base ) ;
   vt1 = MATVEC( new_scale , vt1 ) ;

   vt2 = MATVEC( new_scale , new_origin ) ;  /* want vt1 + vt2 - vt0 */

   vt2 = ADD_FVEC3( vt1 , vt2 ) ;
   vt2 = SUB_FVEC3( vt2 , vt0 ) ;

   /* make the output map */

   newmap = myRwcNew( THD_linear_mapping ) ;

   newmap->type = MAPPING_LINEAR_TYPE ;
   newmap->mfor = mt ;
   newmap->mbac = MAT_INV(mt) ;
   newmap->bvec = vt2 ;

   newmap->svec = MATVEC(newmap->mbac,newmap->bvec) ;
   NEGATE_FVEC3(newmap->svec) ;

   return newmap ;
}

/*--------------------------------------------------------------------
  this routine takes as input two warps, A and B, and produces the
  warp A*B;  B is the warp that occurs before A.
  Restriction: A and B cannot both be Talairach_12 warps!
  The output is placed into A (*warp_in).

  Note that a NULL warp pointer for warp_prior will have the same
  effect as an identity warp, since the routine will return immediately.
----------------------------------------------------------------------*/

void AFNI_concatenate_warp( THD_warp * warp_in , THD_warp * warp_prior )
{
   THD_linear_mapping * prior_map , * new_map ;

   if( warp_in == NULL || warp_prior == NULL ) return ;

   switch( warp_in->type + 100*warp_prior->type ){

      default:
         warp_in->type = -1 ;  /* set error flag! */
         return ;

      /*-- 2 affine warps ==> a new affine warp --*/

      case WARP_AFFINE_TYPE + 100*WARP_AFFINE_TYPE:{
         prior_map = &(warp_prior->rig_bod.warp) ;
         new_map = AFNI_concatenate_lmap(
                      &(warp_in->rig_bod.warp) , prior_map ) ;

         warp_in->rig_bod.warp = *new_map ;  /* write over input warp */
         myRwcFree( new_map ) ;
      }
      break ;

      /*--- Talairach preceeded by affine ==> new Talairach --*/

      case WARP_TALAIRACH_12_TYPE + 100*WARP_AFFINE_TYPE:{
         int iw ;
         prior_map = &(warp_prior->rig_bod.warp) ;
         for( iw=0 ; iw < 12 ; iw++ ){

            new_map = AFNI_concatenate_lmap(
                         &(warp_in->tal_12.warp[iw]) , prior_map ) ;

            warp_in->tal_12.warp[iw] = *new_map ;  /* write over input warp */
            myRwcFree( new_map ) ;
         }
      }
      break ;

      /*-- affine preceeded by Talairach ==> new Talairach
           [this case is not currently used, since there are no warps
            AFTER a Talairach warp, but it may be useful in the future]
                                        -- RWCox, November 1994 A.D. --*/

      case WARP_AFFINE_TYPE + 100*WARP_TALAIRACH_12_TYPE:{
         int iw ;
         THD_talairach_12_warp * new_warp = myRwcNew( THD_talairach_12_warp ) ;

         new_warp->type = WARP_TALAIRACH_12_TYPE ;
         for( iw=0 ; iw < 12 ; iw++ ){
            prior_map = &(warp_prior->tal_12.warp[iw]) ;
            new_map   = AFNI_concatenate_lmap(
                          &(warp_in->rig_bod.warp) , prior_map ) ;
            new_warp->warp[iw] = *new_map ;
            myRwcFree( new_map ) ;
         }

         warp_in->tal_12 = *new_warp ;  /* write over input warp */
         myRwcFree( new_warp ) ;
      }
      break ;

   }  /* end of switch on warp types */

   return ;
}

/*-----------------------------------------------------------------------
   Concatenate 2 linear maps (including allowing for shift of origin)
-------------------------------------------------------------------------*/

THD_linear_mapping * AFNI_concatenate_lmap( THD_linear_mapping * map_2 ,
                                            THD_linear_mapping * map_1  )
{
   THD_linear_mapping * map_out ;
   THD_fvec3 tvec ;

   /* make a new linear mapping */

   map_out = myRwcNew(THD_linear_mapping) ;
   map_out->type = MAPPING_LINEAR_TYPE ;

   /* matrix */

   map_out->mfor = MAT_MUL( map_2->mfor , map_1->mfor ) ;
   map_out->mbac = MAT_INV( map_out->mfor ) ;

   /* vector */

   tvec          = MATVEC( map_2->mfor , map_1->bvec ) ;
   map_out->bvec = ADD_FVEC3( tvec , map_2->bvec ) ;
   map_out->svec = MATVEC( map_out->mbac , map_out->bvec ) ;
   NEGATE_FVEC3(map_out->svec) ;

   map_out->bot  = map_2->bot ;
   map_out->top  = map_2->top ;

   return map_out ;
}

/*--------------------------------------------------------------------------*/
/*! Make an affine warp from 12 input numbers:
     -         [ a11 a12 a13 ]        [ s1 ]
     - x_map = [ a21 a22 a23 ] x_in + [ s2 ]
     -         [ a31 a32 a33 ]        [ s3 ]

    27 Aug 2002 - RWCox.
----------------------------------------------------------------------------*/

THD_warp * AFNI_make_affwarp_12( float a11, float a12, float a13,  float s1 ,
                                 float a21, float a22, float a23,  float s2 ,
                                 float a31, float a32, float a33,  float s3  )
{
   THD_warp *warp ;
   THD_linear_mapping map ;
   float dd , nn ;

   warp       = myRwcNew( THD_warp ) ;
   warp->type = WARP_AFFINE_TYPE ;

   ZZME(map) ;
   map.type = MAPPING_LINEAR_TYPE ;

   LOAD_MAT(map.mfor,a11,a12,a13,a21,a22,a23,a31,a32,a33) ;
   dd = MAT_DET(map.mfor) ; nn = MAT_FNORM(map.mfor) ;
   if( fabs(dd) < 1.e-5*nn*nn*nn ) return NULL ;  /* bad input */
   LOAD_FVEC3(map.bvec,-s1,-s2,-s3) ;
   LOAD_INVERSE_LMAP(map) ;

   warp->rig_bod.warp = map ;

   return warp ;
}

/*-------------------------------------------------------------------------*/

THD_warp * AFNI_make_affwarp_mat( THD_mat33 mmm )
{
   return AFNI_make_affwarp_12( mmm.mat[0][0], mmm.mat[0][1], mmm.mat[0][2], 0.0 ,
                                mmm.mat[1][0], mmm.mat[1][1], mmm.mat[1][2], 0.0 ,
                                mmm.mat[2][0], mmm.mat[2][1], mmm.mat[2][2], 0.0  ) ;
}

/*-------------------------------------------------------------------------*/

THD_warp * AFNI_make_affwarp_matvec( THD_mat33 mmm , THD_fvec3 vvv )
{
   return AFNI_make_affwarp_12( mmm.mat[0][0], mmm.mat[0][1], mmm.mat[0][2], vvv.xyz[0] ,
                                mmm.mat[1][0], mmm.mat[1][1], mmm.mat[1][2], vvv.xyz[1] ,
                                mmm.mat[2][0], mmm.mat[2][1], mmm.mat[2][2], vvv.xyz[2]  ) ;
}
