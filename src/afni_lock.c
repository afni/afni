#include "afni.h"

/*-----------------------------------------------------------------------
    03 Nov 1998: allow locking of time index
-------------------------------------------------------------------------*/

void AFNI_time_lock_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   Three_D_View *qq3d ;
   int           bval , ii , bold ;

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
/* Do the locking of time indexes */

void AFNI_time_lock_carryout( Three_D_View *im3d )
{
   Three_D_View *qq3d ;
   MCW_arrowval *tav ;
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
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_lock_enforce_CB") ;
   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   AFNI_space_lock_carryout( im3d ) ;
   AFNI_time_lock_carryout( im3d ) ;  /* 03 Nov 1998 */
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_lock_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   Three_D_View *qq3d ;
   int           bval , ii , bold ;

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

/*------------------------------------------------------------------------*/

void AFNI_lock_clear_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View *qq3d ;
   int ii ;

ENTRY("AFNI_lock_clear_CB") ;

   GLOBAL_library.controller_lock = 0 ;
   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
     qq3d = GLOBAL_library.controllers[ii] ;
     if( IM3D_VALID(qq3d) )
       MCW_set_bbox( qq3d->vwid->dmode->lock_bbox , 0 ) ;
   }
   if( w != (Widget)NULL ){
    static int ncall = 0 ;
    if( ncall == 0 )
      (void)MCW_popup_message( w ,
                                 " \n"
                                 "*** WARNING:             ***\n"
                                 "*** all AFNI controllers ***\n"
                                 "*** have been unlocked!  ***\n"
                                 "----------------------------\n"
                                 "*** The 'Set All' button ***\n"
                                 "*** can re-lock them :)  ***\n" ,
                               MCW_USER_KILL | MCW_TIMER_KILL ) ;
     ncall++ ;
   }

   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_lock_setall_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View *qq3d ;
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

/*------------------------------------------------------------------------*/
/* Do the locking of spatial coordinates */

void AFNI_space_lock_carryout( Three_D_View *im3d )
{
   Three_D_View *qq3d ;
   int ii,jj,kk , cc , glock ;
   THD_fvec3 old_fv , fv ;
   THD_ivec3 iv ;
   THD_dataxes *qaxes , *daxes ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */

ENTRY("AFNI_space_lock_carryout") ;

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
   Three_D_View *im3d = (Three_D_View *) cd ;
   Three_D_View *qq3d ;
   int           bval , ii , bold ;

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

int AFNI_bbox_thrlock_mask2val(int bval)
{
   int ii;
   for (ii=0; ii<3; ++ii) {
      if (bval & 1<<ii) {
         return(ii);
      }
   }
   return(0);
}

/*------------------------------------------------------------------------*/

void AFNI_set_all_thrlock_bboxes(Three_D_View *im3d, int bval)
{
   int ii;
   Three_D_View *qq3d ;

   if (bval < 0) bval = 1<<AFNI_thresh_lock_env_val();

   /* set all other controller lock boxes to the same value */
   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
     qq3d = GLOBAL_library.controllers[ii] ;
     if( qq3d == im3d || ! IM3D_VALID(qq3d) ) continue ;
     MCW_set_bbox( qq3d->vwid->dmode->thr_lock_bbox , bval ) ;
   }

   GLOBAL_library.thr_lock = AFNI_bbox_thrlock_mask2val(bval);

   return;
}

/*------------------------------------------------------------------------*/

void AFNI_func_thrlock_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int           bval , bold ;

ENTRY("AFNI_func_thrlock_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   bold = 1<<GLOBAL_library.thr_lock ;
   bval = MCW_val_bbox( im3d->vwid->dmode->thr_lock_bbox ) ;

   if( bval == bold ) EXRETURN ;                     /* same --> nothing to do */

   /* new value --> save in global setting */

   GLOBAL_library.thr_lock = AFNI_bbox_thrlock_mask2val(bval) ;

   /* And apply it to other controllers */
   AFNI_set_all_thrlock_bboxes(im3d, bval);

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

int AFNI_thresh_lock_env_val(void)
{
   int i=0;
   char *eee=NULL;

   eee = getenv( "AFNI_THRESH_LOCK" ) ;          /* determine how to lock */
   if( eee == NULL ) return(0) ;
   if(*eee == 'V' || *eee == 'v') return(1);
   if(*eee == 'P' || *eee == 'p') return(2);
   return(0);
}

/*------------------------------------------------------------------------*/
/* Do the locking of thresholds */

void AFNI_thresh_lock_carryout( Three_D_View *im3d )
{
   Three_D_View *qq3d ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */
   int glock , cc,ii , dopval,dothresh ;
   float thresh , pval=0.0f , tval ;
   char cmd[64] ;

ENTRY("AFNI_thresh_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;     /* not a handgun */

   if( busy )                         EXRETURN;  /* routine already busy */
   if( glock == 0 )                   EXRETURN;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )             EXRETURN;  /* bad input */
   if( GLOBAL_library.ignore_lock )   EXRETURN;  /* ordered not to do anything */
   if (!GLOBAL_library.thr_lock)      EXRETURN;
   dothresh = (GLOBAL_library.thr_lock == 1) ;
   dopval   = (GLOBAL_library.thr_lock == 2) && im3d->fim_now != NULL ;
   if( !dothresh && !dopval ) EXRETURN ;         /* no command? */

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */

   if( ii < 0 ) EXRETURN ;                      /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 ) EXRETURN ;      /* input not locked */

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   /* get true threshold of this controller => all others get this value, too*/

   thresh = get_3Dview_func_thresh(im3d,1) ;

   /* get p-value corresponding, if that is what's being locked */

   if( dopval ){
     pval = THD_stat_to_pval( thresh ,
                DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) ,
                DSET_BRICK_STATAUX (im3d->fim_now,im3d->vinfo->thr_index)  ) ;
     if( pval < 0.0 || pval > 1.0 ){ dopval = 0; dothresh = 1; }
   }

   /* loop through other controllers:
        for those that ARE open, ARE NOT the current
        one, and ARE locked, set the new threshold */

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

     qq3d = GLOBAL_library.controllers[cc] ; /* controller */

     if( IM3D_OPEN(qq3d) && qq3d != im3d && ((1<<cc) & glock) != 0 ){

       if( dothresh )
         sprintf( cmd , "SET_THRESHNEW %c %.4f **" , 'A'+cc , thresh ) ;
       else if( dopval && qq3d->fim_now != NULL &&
                DSET_BRICK_STATCODE(qq3d->fim_now,qq3d->vinfo->thr_index) > 0 )
         sprintf( cmd , "SET_THRESHNEW %c %g *p" , 'A'+cc , pval ) ;
       else
         continue ;  /* pval, but not a statistic? */

       AFNI_driver( cmd ) ;
     }
   }

   busy = 0 ;  /* OK, let this routine be activated again */
   EXRETURN ;
}

/*---------------------------------------------------------------*/

void AFNI_equate_pbars( Three_D_View *lh3d , Three_D_View *rh3d )
{
   MCW_pbar *lbar , *rbar ;
   char cmd[1024] ;
   int cc , qq ;
   MCW_DCOV *ovc = GLOBAL_library.dc->ovc ;

ENTRY("AFNI_equate_pbars") ;

   if( !IM3D_OPEN(lh3d) || !IM3D_OPEN(rh3d) ) EXRETURN ;

   lbar = lh3d->vwid->func->inten_pbar ;
   rbar = rh3d->vwid->func->inten_pbar ;

   if( MCW_pbars_equivalent(lbar,rbar) ) EXRETURN ;  /* 07 Jul 2014 */

   cc = AFNI_controller_index(lh3d) ; if( cc < 0 ) EXRETURN ;

   if( !rbar->bigmode ){
     sprintf(cmd,"SET_PBAR_ALL %c.%c%d" , 'A'+cc ,
             (rbar->mode) ? '+' : '-' , rbar->num_panes ) ;
     for( qq=0 ; qq < rbar->num_panes ; qq++ )
       sprintf(cmd+strlen(cmd)," %s=%s",
               AV_uformat_fval(rbar->pval[qq]) ,
               ovc->label_ov[rbar->ov_index[qq]] ) ;
   } else {
     if( rbar->big31 == 0 )
       sprintf(cmd,"SET_PBAR_ALL %c.%c%d %f %s\n" , 'A'+cc ,
               (rbar->mode) ? '+' : '-' , 99 ,
               rbar->bigtop , PBAR_get_bigmap(rbar) ) ;
     else
       sprintf(cmd,"SET_PBAR_ALL %c.%c%d %f::%f %s\n" , 'A'+cc ,
               (rbar->mode) ? '+' : '-' , 99 ,
               rbar->bigbot,rbar->bigtop , PBAR_get_bigmap(rbar) ) ;
     if( rbar->bigflip )
       sprintf(cmd+strlen(cmd)," FLIP") ;
     if( rbar->bigrota )
       sprintf(cmd+strlen(cmd)," ROTA=%d",rbar->bigrota) ;
     lbar->big30 = rbar->big30 ;
     lbar->big31 = rbar->big31 ;
     lbar->big32 = rbar->big32 ;
   }

   AFNI_driver(cmd) ; EXRETURN ;
}

/*---------------------------------------------------------------*/
/* Do the locking of pbars */

void AFNI_pbar_lock_carryout( Three_D_View *im3d )
{
   Three_D_View *qq3d ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */
   int glock , cc,ii ;

ENTRY("AFNI_pbar_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;     /* not a handgun */

   if( busy )                         EXRETURN;  /* routine already busy */
   if( glock == 0 )                   EXRETURN;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )             EXRETURN;  /* bad input */
   if( GLOBAL_library.ignore_lock )   EXRETURN;  /* ordered not to do anything */
   if( !AFNI_check_pbar_lock() )      EXRETURN;  /* not locked? */

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */

   if( ii < 0 ) EXRETURN ;                      /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 ) EXRETURN ;      /* input not locked */

#if 0
INFO_message("AFNI_pbar_lock_carryout( %d ) ******************* ",ii) ;
TRACEBACK ;
#endif

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   /* loop through other controllers:
        for those that ARE open, ARE NOT the current one, and ARE locked */

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

     qq3d = GLOBAL_library.controllers[cc] ; /* controller */

     if( IM3D_OPEN(qq3d) && qq3d != im3d && ((1<<cc) & glock) != 0 ){
#if 0
ININFO_message(" equate_pbars( %d )",cc) ;
#endif
       AFNI_equate_pbars( qq3d , im3d ) ;
     }
   }

   busy = 0 ;  /* OK, let this routine be activated again */
   EXRETURN ;
}

/*------------------------------------------------------------------------*/
/* Do the dynamic locking of dragging thresholds */

void AFNI_thrdrag_lock_carryout( Three_D_View *im3d )
{
   Three_D_View *qq3d ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */
   int glock , cc,ii , dothresh,dopval , ival , stop ;
   float thresh , pval=0.0f , tval ;

ENTRY("AFNI_thrdrag_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;     /* not a handgun */

   if( busy )                         EXRETURN;  /* routine already busy */
   if( glock == 0 )                   EXRETURN;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )             EXRETURN;  /* bad input */
   if( GLOBAL_library.ignore_lock )   EXRETURN;  /* ordered not to do anything */

   if (!GLOBAL_library.thr_lock) EXRETURN;
   dothresh = (GLOBAL_library.thr_lock == 1) ;
   dopval   = (GLOBAL_library.thr_lock == 2) && im3d->fim_now != NULL ;
   if( !dothresh && !dopval ) EXRETURN ;         /* no command? */

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */

   if( ii < 0 ) EXRETURN ;                      /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 ) EXRETURN ;      /* input not locked */

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   ival   = rint(im3d->vinfo->func_threshold/THR_factor) ;
   thresh = get_3Dview_func_thresh(im3d,1) ;
   stop   = (int)( rint( pow(10.0,THR_top_expon) ) - 1.0 ) ;

   if( dopval ){
     pval = THD_stat_to_pval( thresh ,
                DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) ,
                DSET_BRICK_STATAUX (im3d->fim_now,im3d->vinfo->thr_index)  ) ;
     if( pval < 0.0 || pval > 1.0 ){ dopval = 0; dothresh = 1; }
   }

   /* loop through other controllers:
        for those that ARE open, ARE NOT the current
        one, and ARE locked, set the new threshold */

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

     qq3d = GLOBAL_library.controllers[cc] ; /* controller */

     if( IM3D_OPEN(qq3d) && qq3d != im3d && ((1<<cc) & glock) != 0 ){

       if( qq3d->vinfo->func_thresh_top == im3d->vinfo->func_thresh_top ){

         if( dopval && qq3d->fim_now != NULL &&
             DSET_BRICK_STATCODE(qq3d->fim_now,qq3d->vinfo->thr_index) > 0 ){

           tval = THD_pval_to_stat( pval ,
                    DSET_BRICK_STATCODE(qq3d->fim_now,qq3d->vinfo->thr_index),
                    DSET_BRICK_STATAUX (qq3d->fim_now,qq3d->vinfo->thr_index) );
           ival = rint( tval/(THR_factor*qq3d->vinfo->func_thresh_top) ) ;
           if( ival < 0 ) ival = 0 ; else if( ival > stop ) ival = stop ;

         } else if( !dothresh ){
           continue ;  /* skip this [dopval set, but not a statistic] */
         }

         /* set the slider and pval marker */

         XmScaleSetValue( qq3d->vwid->func->thr_scale , ival ) ;
         qq3d->vinfo->func_threshold = THR_factor * ival ;
         AFNI_set_thr_pval( qq3d ) ;
       }
     }
   }

   busy = 0 ;  /* OK, let this routine be activated again */
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

int AFNI_check_pbar_lock()
{
   return AFNI_yesenv("AFNI_PBAR_LOCK") ;
}

/*------------------------------------------------------------------------*/

int AFNI_check_range_lock()
{
   if( AFNI_yesenv("AFNI_RANGE_LOCK")           ) return 1 ;
   if( PBAR_FULLRANGE && AFNI_check_pbar_lock() ) return 1 ;
   return 0 ;
}

/*------------------------------------------------------------------------*/
/* Do the locking of OLay ranges */

void AFNI_range_lock_carryout( Three_D_View *im3d )
{
   Three_D_View *qq3d ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */
   int glock , cc,ii,nn , qdone=0 ;
   float val ;
   char cmd[64] ;

ENTRY("AFNI_range_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;      /* not a handgun */

   if( busy )                         EXRETURN;  /* routine already busy */
   if( glock == 0 )                   EXRETURN;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )             EXRETURN;  /* bad input */
   if( GLOBAL_library.ignore_lock )   EXRETURN;  /* ordered not to do anything */
   if( !AFNI_check_range_lock() )     EXRETURN;  /* not locked? */

   ii = AFNI_controller_index(im3d);             /* which one am I? */

   if( ii < 0 )                       EXRETURN;  /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 )       EXRETURN;  /* input not locked */

   /* get range of this controller */

   val = im3d->vinfo->fim_range ;
   if( val <= 0.0 )                   EXRETURN;  /* shouldn't happen */

   /* count how many OTHER controllers are are open and locked;
      if none of them are, there is nothing to do [29 Apr 2005] */

   for( nn=cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){
     qq3d = GLOBAL_library.controllers[cc] ;
     if( qq3d != im3d && IM3D_OPEN(qq3d) && ((1<<cc) & glock) != 0 ) nn++ ;
   }
   if( nn < 1 )                       EXRETURN ;

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   /* loop through other controllers:
        for those that ARE open, and ARE locked, set the new range */

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

     qq3d = GLOBAL_library.controllers[cc] ; /* controller */

     if( IM3D_OPEN(qq3d) && ((1<<cc) & glock) != 0 ){  /* open and locked */

       if( qq3d == im3d &&    /* no need to set current if not autoRanged */
           MCW_val_bbox(im3d->vwid->func->range_bbox) == 0 ) continue;

       sprintf( cmd , "SET_FUNC_RANGE %c.%.6f" , 'A'+cc , val ) ;
       AFNI_driver( cmd ) ;
     }
   }

   busy = 0 ;  /* OK, let this routine be activated again */
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_set_all_rnglock_bboxes(Three_D_View *im3d, int bval)
{
   int ii;
   Three_D_View *qq3d ;

   if( bval < 0 || bval > 1 )
     bval = MCW_val_bbox( im3d->vwid->dmode->rng_lock_bbox ) ;

   /* set all other controller lock boxes to the same value */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
     qq3d = GLOBAL_library.controllers[ii] ;
     if( qq3d == im3d || ! IM3D_VALID(qq3d) ) continue ;
     MCW_set_bbox( qq3d->vwid->dmode->rng_lock_bbox , bval ) ;
   }

   if( AFNI_check_range_lock() != bval )
     AFNI_setenv( bval ? "AFNI_RANGE_LOCK=YES" : "AFNI_RANGE_LOCK=NO" ) ;

   return;
}

/*------------------------------------------------------------------------*/

void AFNI_func_rnglock_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   int           bval , bold ;

ENTRY("AFNI_func_rnglock_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   bold = AFNI_check_range_lock() ;
   bval = MCW_val_bbox( im3d->vwid->dmode->rng_lock_bbox ) ;

   if( bval == bold ) EXRETURN ;                     /* same --> nothing to do */

   /* New value ==> apply it to other controllers */

   AFNI_set_all_rnglock_bboxes(im3d,bval);
   AFNI_range_lock_carryout(im3d) ;

   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_set_all_pbarlock_bboxes(Three_D_View *im3d, int bval)
{
   int ii;
   Three_D_View *qq3d ;

   if( bval < 0 || bval > 1 )
     bval = MCW_val_bbox( im3d->vwid->dmode->pbar_lock_bbox ) ;

   /* set all other controller lock boxes to the same value */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
     qq3d = GLOBAL_library.controllers[ii] ;
     if( qq3d == im3d || ! IM3D_VALID(qq3d) ) continue ;
     MCW_set_bbox( qq3d->vwid->dmode->pbar_lock_bbox , bval ) ;
   }

   if( AFNI_check_pbar_lock() != bval )
     AFNI_setenv( bval ? "AFNI_PBAR_LOCK=YES" : "AFNI_PBAR_LOCK=NO" ) ;

   return;
}

/*------------------------------------------------------------------------*/

void AFNI_func_pbarlock_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   int           bval , bold ;

ENTRY("AFNI_func_pbarlock_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   bold = AFNI_check_pbar_lock() ;
   bval = MCW_val_bbox( im3d->vwid->dmode->pbar_lock_bbox ) ;

   if( bval == bold ) EXRETURN ;                     /* same --> nothing to do */

   /* New value ==> apply it to other controllers */

   AFNI_set_all_pbarlock_bboxes(im3d,bval);
   AFNI_pbar_lock_carryout(im3d) ;

   EXRETURN ;
}

/*-----------------------------------------------------------------------
    10 Dec 2019: zoom/pan locking on or off
-------------------------------------------------------------------------*/

void AFNI_zoompan_lock_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   Three_D_View *qq3d ;
   int           bval , ii , bold ;

ENTRY("AFNI_zoompan_lock_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /* get current global setting and compare to changed lock box */

   bold = GLOBAL_library.zoompan_lock ;
   bval = MCW_val_bbox( im3d->vwid->dmode->zoompan_lock_bbox ) ;
   if( bval == bold ) EXRETURN ;                     /* same --> nothing to do */

   /* new value --> save in global setting */

   GLOBAL_library.zoompan_lock = bval ;

   /* set all other controller lock boxes to the same value */

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
     qq3d = GLOBAL_library.controllers[ii] ;
     if( qq3d == im3d || ! IM3D_VALID(qq3d) ) continue ;

     MCW_set_bbox( qq3d->vwid->dmode->zoompan_lock_bbox , bval ) ;
   }
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*------------------------------------------------------------------------*/
/* Locking for the zoom+pan image viewers [10 Dec 2019] */

void AFNI_zoompan_lock_carryout( Three_D_View *im3d )
{
   static int busy = 0 ;  /* !=0 if this routine is "busy" */
   Three_D_View *qq3d ;
   MCW_imseq *seq ;
   int ii,jj,kk , cc , glock ;
   int axi_zlev=0 ; float axi_poff[2] ;
   int sag_zlev=0 ; float sag_poff[2] ;
   int cor_zlev=0 ; float cor_poff[2] ;

ENTRY("AFNI_zoompan_locks_carryout") ;

   if( busy || !IM3D_VALID(im3d)    ) EXRETURN ;
   if( GLOBAL_library.ignore_lock   ) EXRETURN ;
   if( !GLOBAL_library.zoompan_lock ) EXRETURN ;
   if( !IM3D_IMAGIZED(im3d)         ) EXRETURN ;

   glock = GLOBAL_library.controller_lock ;      /* not a handgun */
   if( glock == 0 )                   EXRETURN ; /* nothing to do */

   ii = AFNI_controller_index(im3d) ;            /* which one am I? */

   if( ii < 0 )                       EXRETURN ; /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 )       EXRETURN ; /* input not locked */

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   /* find the zoom and pan parameters for the current image viewers */

   seq = IM3D_AXIALIMAGE(im3d) ;
   if( seq != NULL ){
     drive_MCW_imseq( seq, isqDR_get_zoom  , &axi_zlev ) ;
     drive_MCW_imseq( seq, isqDR_get_panoff, axi_poff  ) ;
   }

   seq = IM3D_SAGITTALIMAGE(im3d) ;
   if( seq != NULL ){
     drive_MCW_imseq( seq, isqDR_get_zoom  , &sag_zlev ) ;
     drive_MCW_imseq( seq, isqDR_get_panoff, sag_poff  ) ;
   }

   seq = IM3D_CORONALIMAGE(im3d) ;
   if( seq != NULL ){
     drive_MCW_imseq( seq, isqDR_get_zoom  , &cor_zlev ) ;
     drive_MCW_imseq( seq, isqDR_get_panoff, cor_poff  ) ;
   }

   /* loop through other controllers:
        for those that ARE open, ARE NOT the current one,
        and ARE locked, set zoom and pan to match
        this controller's corresponding image viewers */

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

     qq3d = GLOBAL_library.controllers[cc] ; /* controller */

     if( IM3D_OPEN(qq3d) && qq3d != im3d && ((1<<cc) & glock) != 0 ){

       seq = IM3D_AXIALIMAGE(qq3d) ;
       if( seq != NULL ){
         drive_MCW_imseq( seq, isqDR_set_zoom  , &axi_zlev ) ;
         drive_MCW_imseq( seq, isqDR_set_panoff, axi_poff  ) ;
       }

       seq = IM3D_SAGITTALIMAGE(qq3d) ;
       if( seq != NULL ){
         drive_MCW_imseq( seq, isqDR_set_zoom  , &sag_zlev ) ;
         drive_MCW_imseq( seq, isqDR_set_panoff, sag_poff  ) ;
       }

       seq = IM3D_CORONALIMAGE(qq3d) ;
       if( seq != NULL ){
         drive_MCW_imseq( seq, isqDR_set_zoom  , &cor_zlev ) ;
         drive_MCW_imseq( seq, isqDR_set_panoff, cor_poff  ) ;
       }
     }
   }

   busy = 0 ;  /* OK, let this routine be activated again */
   EXRETURN ;
}

/*------------------------------------------------------------------------*/
/* Enforce all locks at the same time [03 Jul 2014] */

void AFNI_all_locks_carryout( Three_D_View *im3d )
{
   static int busy = 0 ;  /* !=0 if this routine is "busy" */

ENTRY("AFNI_all_locks_carryout") ;

   if( busy || !IM3D_VALID(im3d)  ) EXRETURN ;
   if( GLOBAL_library.ignore_lock ) EXRETURN ;

#if 0
INFO_message("AFNI_all_locks_carryout: im3d index = %d",AFNI_controller_index(im3d)) ;
#endif

   AFNI_space_lock_carryout  ( im3d ) ;
   AFNI_time_lock_carryout   ( im3d ) ;
   AFNI_thresh_lock_carryout ( im3d ) ;
   AFNI_pbar_lock_carryout   ( im3d ) ;
   AFNI_range_lock_carryout  ( im3d ) ;
   AFNI_zoompan_lock_carryout( im3d ) ; /* 10 Dec 2019 */

   AFNI_sleep(1) ; busy = 0 ; EXRETURN ;
}
