#include "afni.h"

/*-----------------------------------------------------------------------
    03 Nov 1998: allow locking of time index
-------------------------------------------------------------------------*/

void AFNI_time_lock_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   Three_D_View * qq3d ;
   int            bval , ii , bold ;

ENTRY("AFNI_time_lock_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /* get current global setting and compare to changed lock box */

   bold = GLOBAL_library.time_lock ;
   bval = MCW_val_bbox( im3d->vwid->dmode->time_lock_bbox ) ;
   if( bval == bold ) EXRETURN ;                     /* same --> nothing to do */

   /* new value --> save in global setting */

   GLOBAL_library.time_lock = bval ;

   /* set all other controller lock boxes to the same value */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      qq3d = GLOBAL_library.controllers[ii] ;
      if( qq3d == im3d || ! IM3D_VALID(qq3d) ) continue ;

      MCW_set_bbox( qq3d->vwid->dmode->time_lock_bbox , bval ) ;
   }
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_time_lock_carryout( Three_D_View * im3d )
{
   Three_D_View * qq3d ;
   MCW_arrowval * tav ;
   int new_index , qq_index , qq_top , cc , glock , ii ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */

ENTRY("AFNI_time_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;     /* not a handgun */

   if( busy )                       EXRETURN ;  /* routine already busy */
   if( glock == 0 )                 EXRETURN ;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )           EXRETURN ;  /* bad input */
   if( GLOBAL_library.ignore_lock ) EXRETURN ;  /* ordered not to do anything */
   if( ! GLOBAL_library.time_lock ) EXRETURN ;  /* don't lock time */

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */

   if( ii < 0 ) EXRETURN ;                      /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 ) EXRETURN ;      /* input not locked */

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   /* load time index of this controller => all others get this value, too*/

   new_index = im3d->vinfo->time_index ;

   /* loop through other controllers:
        for those that ARE open, ARE NOT the current
        one, and ARE locked, jump to the new time index */

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

      qq3d = GLOBAL_library.controllers[cc] ; /* controller */

      if( IM3D_OPEN(qq3d) && qq3d != im3d && ((1<<cc) & glock) != 0 ){

         qq_index = qq3d->vinfo->time_index ;           /* old index */
         qq_top   = qq3d->vinfo->top_index ;            /* max allowed */

         if( qq3d->vinfo->time_on && qq_top > 1 && qq_index != new_index ){
           tav = qq3d->vwid->imag->time_index_av ;
           AV_assign_ival( tav , new_index ) ;         /* will check range */
           if( tav->ival != qq_index )
             AFNI_time_index_CB( tav , (XtPointer) qq3d ) ;
         }
      }
   }

   busy = 0 ;  /* OK, let this routine be activated again */
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   The following routines for "locks" were added 04 Nov 1996
-------------------------------------------------------------------------*/

void AFNI_lock_enforce_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_lock_enforce_CB") ;
   AFNI_lock_carryout( im3d ) ;
   AFNI_time_lock_carryout( im3d ) ;  /* 03 Nov 1998 */
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

void AFNI_lock_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   Three_D_View * qq3d ;
   int            bval , ii , bold ;

ENTRY("AFNI_lock_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /* get current global setting and compare to changed lock box */

   bold = GLOBAL_library.controller_lock ;
   bval = MCW_val_bbox( im3d->vwid->dmode->lock_bbox ) ;
   if( bval == bold ) EXRETURN ;                     /* same --> nothing to do */

   /* new value --> save in global setting */

   GLOBAL_library.controller_lock = bval ;

   /* set all other controller lock boxes to the same value */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      qq3d = GLOBAL_library.controllers[ii] ;
      if( qq3d == im3d || ! IM3D_VALID(qq3d) ) continue ;

      MCW_set_bbox( qq3d->vwid->dmode->lock_bbox , bval ) ;
   }
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

void AFNI_lock_clear_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View * qq3d ;
   int ii ;

ENTRY("AFNI_lock_clear_CB") ;

   GLOBAL_library.controller_lock = 0 ;
   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      qq3d = GLOBAL_library.controllers[ii] ;
      if( IM3D_VALID(qq3d) )
         MCW_set_bbox( qq3d->vwid->dmode->lock_bbox , 0 ) ;
   }
   EXRETURN ;
}

void AFNI_lock_setall_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View * qq3d ;
   int ii ;

ENTRY("AFNI_lock_setall_CB") ;

   GLOBAL_library.controller_lock = 0 ;
   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ )
      GLOBAL_library.controller_lock |= (1<<ii) ;

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      qq3d = GLOBAL_library.controllers[ii] ;
      if( IM3D_VALID(qq3d) )
         MCW_set_bbox( qq3d->vwid->dmode->lock_bbox ,
                       GLOBAL_library.controller_lock ) ;
   }
   EXRETURN ;
}

void AFNI_lock_carryout( Three_D_View * im3d )
{
   Three_D_View * qq3d ;
   int ii,jj,kk , cc , glock ;
   THD_fvec3 old_fv , fv ;
   THD_ivec3 iv ;
   THD_dataxes * qaxes , * daxes ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */

ENTRY("AFNI_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;

   if( busy )                       EXRETURN ;  /* routine already busy */
   if( glock == 0 )                 EXRETURN ;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )           EXRETURN ;  /* bad input */
   if( GLOBAL_library.ignore_lock ) EXRETURN ;  /* ordered not to do anything */

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */

   if( ii < 0 ) EXRETURN ;                      /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 ) EXRETURN ;      /* input not locked */

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   /* load Dicom location of current point of view in this controller */

   LOAD_FVEC3( old_fv , im3d->vinfo->xi, im3d->vinfo->yj, im3d->vinfo->zk ) ;

   LOAD_ANAT_VIEW(im3d) ;  /* prepare coordinates */
   daxes = CURRENT_DAXES(im3d->anat_now) ;

   /* loop through other controllers:
        for those that ARE open, ARE NOT the current one,
        and ARE locked, transform the above vector to the
        controller's dataset, and then jump to that point */

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

      qq3d = GLOBAL_library.controllers[cc] ; /* controller */

      if( IM3D_OPEN(qq3d) && qq3d != im3d && ((1<<cc) & glock) != 0 ){

         LOAD_ANAT_VIEW(qq3d) ;  /* prepare coordinates */
         qaxes = CURRENT_DAXES(qq3d->anat_now) ;

         if( !GLOBAL_library.ijk_lock ){  /* xyz coord lock */

            fv = AFNI_transform_vector( im3d->anat_now, old_fv, qq3d->anat_now ) ;
            fv = THD_dicomm_to_3dmm( qq3d->anat_now , fv ) ;
            iv = THD_3dmm_to_3dind ( qq3d->anat_now , fv ) ;
            ii = iv.ijk[0] ; jj = iv.ijk[1] ; kk = iv.ijk[2] ;

         } else {   /* 11 Sep 2000: ijk index lock */

            ii = im3d->vinfo->i1 * qaxes->nxx / daxes->nxx ;
            jj = im3d->vinfo->j2 * qaxes->nyy / daxes->nyy ;
            kk = im3d->vinfo->k3 * qaxes->nzz / daxes->nzz ;
         }

         /* if have good new ijk coords, jump to them */

         if( ii >= 0 && ii < qaxes->nxx &&
             jj >= 0 && jj < qaxes->nyy && kk >= 0 && kk < qaxes->nzz   ){

            SAVE_VPT(qq3d) ;
            AFNI_set_viewpoint( qq3d , ii,jj,kk , REDISPLAY_ALL ) ; /* jump */
         }
      }
   }

   busy = 0 ;  /* OK, let this routine be activated again */
   EXRETURN ;
}

/*-----------------------------------------------------------------------
    11 Sep 2000: allow locking using ijk instead of xyz
-------------------------------------------------------------------------*/

void AFNI_ijk_lock_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   Three_D_View * qq3d ;
   int            bval , ii , bold ;

ENTRY("AFNI_ijk_lock_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /* get current global setting and compare to changed lock box */

   bold = GLOBAL_library.ijk_lock ;
   bval = MCW_val_bbox( im3d->vwid->dmode->ijk_lock_bbox ) ;
   if( bval == bold ) EXRETURN ;                     /* same --> nothing to do */

   /* new value --> save in global setting */

   GLOBAL_library.ijk_lock = bval ;

   /* set all other controller lock boxes to the same value */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
      qq3d = GLOBAL_library.controllers[ii] ;
      if( qq3d == im3d || ! IM3D_VALID(qq3d) ) continue ;

      MCW_set_bbox( qq3d->vwid->dmode->ijk_lock_bbox , bval ) ;
   }
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_thresh_lock_carryout( Three_D_View *im3d )
{
   Three_D_View *qq3d ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */
   int glock , cc,ii ;
   float thresh ;
   char cmd[64] ;

ENTRY("AFNI_thresh_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;     /* not a handgun */

   if( busy )                         EXRETURN;  /* routine already busy */
   if( glock == 0 )                   EXRETURN;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )             EXRETURN;  /* bad input */
   if( GLOBAL_library.ignore_lock )   EXRETURN;  /* ordered not to do anything */
   if( ! GLOBAL_library.thresh_lock ) EXRETURN;  /* don't lock thresh */

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */

   if( ii < 0 ) EXRETURN ;                      /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 ) EXRETURN ;      /* input not locked */

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   /* get true threshold of this controller => all others get this value, too*/

   thresh = im3d->vinfo->func_threshold * im3d->vinfo->func_thresh_top ;

   /* loop through other controllers:
        for those that ARE open, ARE NOT the current
        one, and ARE locked, set the new threshold */

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

      qq3d = GLOBAL_library.controllers[cc] ; /* controller */

      if( IM3D_OPEN(qq3d) && qq3d != im3d && ((1<<cc) & glock) != 0 ){

         sprintf( cmd , "SET_THRESHNEW %c %.4f **" , 'A'+cc , thresh ) ;
         AFNI_driver( cmd ) ;
      }
   }

   busy = 0 ;  /* OK, let this routine be activated again */
   EXRETURN ;
}
