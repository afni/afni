#include "mrilib.h"
#include "cs.h"

/*----------------------------------------------------------------------
   Begin coordinate transformation functions
   [moved from thd_ttatlas_query.c on 13 Jul 2020 - RWC]
------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
   Forward transform a vector following a warp
   Don't mess with this function, many programs use it. ZSS Feb 06
--------------------------------------------------------------------------*/

THD_fvec3 AFNI_forward_warp_vector( THD_warp * warp , THD_fvec3 old_fv )
{
   THD_fvec3 new_fv ;

   if( warp == NULL ) return old_fv ;

   switch( warp->type ){

      default: new_fv = old_fv ; break ;

      case WARP_TALAIRACH_12_TYPE:{
         THD_linear_mapping map ;
         int iw ;

         /* forward transform each possible case,
            and test if result is in bot..top of defined map */

         for( iw=0 ; iw < 12 ; iw++ ){
            map    = warp->tal_12.warp[iw] ;
            new_fv = MATVEC_SUB(map.mfor,old_fv,map.bvec) ;

            if( new_fv.xyz[0] >= map.bot.xyz[0] &&
                new_fv.xyz[1] >= map.bot.xyz[1] &&
                new_fv.xyz[2] >= map.bot.xyz[2] &&
                new_fv.xyz[0] <= map.top.xyz[0] &&
                new_fv.xyz[1] <= map.top.xyz[1] &&
                new_fv.xyz[2] <= map.top.xyz[2]   ) break ;  /* leave loop */
         }
      }
      break ;

      case WARP_AFFINE_TYPE:{
         THD_linear_mapping map = warp->rig_bod.warp ;
         new_fv = MATVEC_SUB(map.mfor,old_fv,map.bvec) ;
      }
      break ;

   }
   return new_fv ;
}

/*------------------------------------------------------------------------
   Backward transform a vector following a warp
   Don't mess with this function, many programs use it. ZSS Feb 06
--------------------------------------------------------------------------*/
THD_fvec3 AFNI_backward_warp_vector( THD_warp * warp , THD_fvec3 old_fv )
{
   THD_fvec3 new_fv ;

   if( warp == NULL ) return old_fv ;

   switch( warp->type ){

      default: new_fv = old_fv ; break ;

      case WARP_TALAIRACH_12_TYPE:{
         THD_linear_mapping map ;
         int iw ;

         /* test if input is in bot..top of each defined map */

         for( iw=0 ; iw < 12 ; iw++ ){
            map = warp->tal_12.warp[iw] ;

            if( old_fv.xyz[0] >= map.bot.xyz[0] &&
                old_fv.xyz[1] >= map.bot.xyz[1] &&
                old_fv.xyz[2] >= map.bot.xyz[2] &&
                old_fv.xyz[0] <= map.top.xyz[0] &&
                old_fv.xyz[1] <= map.top.xyz[1] &&
                old_fv.xyz[2] <= map.top.xyz[2]   ) break ;  /* leave loop */
         }
         new_fv = MATVEC_SUB(map.mbac,old_fv,map.svec) ;
      }
      break ;

      case WARP_AFFINE_TYPE:{
         THD_linear_mapping map = warp->rig_bod.warp ;
         new_fv = MATVEC_SUB(map.mbac,old_fv,map.svec) ;
      }
      break ;

   }
   return new_fv ;
}

/*!
   Coordinate transformation between N27 MNI
   and AFNI's TLRC. Transform was obtained by manually AFNI TLRCing
   the N27 dset supplied in Eickhoff & Zilles' v12 dbase before
   the volume itself was placed in MNI Anatomical space as was done in v13.
   Input and output are in RAI
*/
THD_fvec3 THD_mni__tta_N27( THD_fvec3 mv, int dir )
{
   static THD_talairach_12_warp *ww=NULL;
   float tx,ty,tz ;
   int iw, ioff;
   THD_fvec3 tv2;

   tx = ty = tz = -9000.0;
/*   LOAD_FVEC3( tv , tx,ty,tz ) ;*/
   LOAD_FVEC3( tv2 , tx,ty,tz ) ;

   /* Meth 2, xform in code, more fool proof*/
   if (!ww) {
      /* load the transform */
      ww = myRwcNew( THD_talairach_12_warp ) ;
      ww->type = WARP_TALAIRACH_12_TYPE;
      ww->resam_type = 0;
      for (iw=0; iw < 12; ++iw) {
         ww->warp[iw].type = MAPPING_LINEAR_TYPE ;

         ioff = iw * MAPPING_LINEAR_FSIZE ;
         COPY_INTO_STRUCT( ww->warp[iw] ,
                           MAPPING_LINEAR_FSTART ,
                           float ,
                           &(MNI_N27_to_AFNI_TLRC_WRP_VEC[ioff]) ,
                           MAPPING_LINEAR_FSIZE ) ;

      }
   }

   if (!ww) {
      ERROR_message("Failed to form built-in warp.");
      return tv2;
   } else {
      if (dir > 0) tv2 = AFNI_forward_warp_vector((THD_warp *)ww, mv);
      else tv2 = AFNI_backward_warp_vector((THD_warp *)ww, mv);
   }

   return tv2 ;
}

THD_fvec3 THD_mni_to_tta_N27( THD_fvec3 mv )
{
   return (THD_mni__tta_N27( mv , 1));
}

THD_fvec3 THD_tta_to_mni_N27( THD_fvec3 mv )
{
   return (THD_mni__tta_N27( mv , -1));
}

THD_fvec3 THD_mnia_to_tta_N27( THD_fvec3 mv )
{
   THD_fvec3 mva;
   /*go from MNI Anat to MNI (remember, shift is in RAI space, not LPI. See also script @Shift_volume)*/
   mva.xyz[0] = mv.xyz[0] + 0.0 ;
   mva.xyz[1] = mv.xyz[1] - 4.0 ;
   mva.xyz[2] = mv.xyz[2] - 5.0 ;

   return (THD_mni__tta_N27( mva , 1));
}

THD_fvec3 THD_tta_to_mnia_N27( THD_fvec3 mv )
{
   THD_fvec3 mva;

   mva = THD_mni__tta_N27( mv , -1);

   /*go from MNI to MNI Anat (remember, shift is in RAI space, not LPI. See also script @Shift_volume)*/
   mva.xyz[0] = mva.xyz[0] + 0.0 ;
   mva.xyz[1] = mva.xyz[1] + 4.0 ;
   mva.xyz[2] = mva.xyz[2] + 5.0 ;

   return (mva);
}
