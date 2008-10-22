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
   AFNI_lock_carryout( im3d ) ;
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

void AFNI_lock_carryout( Three_D_View *im3d )
{
   Three_D_View *qq3d ;
   int ii,jj,kk , cc , glock ;
   THD_fvec3 old_fv , fv ;
   THD_ivec3 iv ;
   THD_dataxes *qaxes , *daxes ;
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

void AFNI_thresh_lock_carryout( Three_D_View *im3d )
{
   Three_D_View *qq3d ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */
   int glock , cc,ii , dopval,dothresh ;
   float thresh , pval=0.0f , tval ;
   char cmd[64] , *eee ;

ENTRY("AFNI_thresh_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;     /* not a handgun */

   if( busy )                         EXRETURN;  /* routine already busy */
   if( glock == 0 )                   EXRETURN;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )             EXRETURN;  /* bad input */
   if( GLOBAL_library.ignore_lock )   EXRETURN;  /* ordered not to do anything */

   eee = getenv( "AFNI_THRESH_LOCK" ) ;          /* determine how to lock */
   if( eee == NULL ) EXRETURN ;
   dothresh = (*eee == 'V' || *eee == 'v') ;
   dopval   = (*eee == 'P' || *eee == 'p') && im3d->fim_now != NULL ;
   if( !dothresh && !dopval ) EXRETURN ;         /* no command? */

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */

   if( ii < 0 ) EXRETURN ;                      /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 ) EXRETURN ;      /* input not locked */

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   /* get true threshold of this controller => all others get this value, too*/

   thresh = im3d->vinfo->func_threshold * im3d->vinfo->func_thresh_top ;

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

   cc = AFNI_controller_index(lh3d) ; if( cc < 0 ) EXRETURN ;

   if( !rbar->bigmode ){
     sprintf(cmd,"SET_PBAR_ALL %c.%c%d" , 'A'+cc ,
             (rbar->mode) ? '+' : '-' , rbar->num_panes ) ;
     for( qq=0 ; qq < rbar->num_panes ; qq++ )
       sprintf(cmd+strlen(cmd)," %s=%s",
               AV_uformat_fval(rbar->pval[qq]) ,
               ovc->label_ov[rbar->ov_index[qq]] ) ;
   } else {
     sprintf(cmd,"SET_PBAR_ALL %c.%c%d %f %s\n" , 'A'+cc ,
             (rbar->mode) ? '+' : '-' , 99 ,
             rbar->bigtop , PBAR_get_bigmap(rbar) ) ;
     if( rbar->bigflip )
       sprintf(cmd+strlen(cmd)," FLIP") ;
     if( rbar->bigrota )
       sprintf(cmd+strlen(cmd)," ROTA=%d",rbar->bigrota) ;
   }

   AFNI_driver( cmd ) ; EXRETURN ;
}

/*---------------------------------------------------------------*/

void AFNI_pbar_lock_carryout( Three_D_View *im3d )
{
   Three_D_View *qq3d ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */
   int glock , cc,ii ;
   char *eee ;

ENTRY("AFNI_pbar_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;     /* not a handgun */

   if( busy )                         EXRETURN;  /* routine already busy */
   if( glock == 0 )                   EXRETURN;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )             EXRETURN;  /* bad input */
   if( GLOBAL_library.ignore_lock )   EXRETURN;  /* ordered not to do anything */

   eee = getenv( "AFNI_PBAR_LOCK" ) ;            /* determine how to lock */
   if( eee == NULL ) EXRETURN ;
   if( *eee != 'Y' && *eee != 'y' ) EXRETURN ;

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */

   if( ii < 0 ) EXRETURN ;                      /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 ) EXRETURN ;      /* input not locked */

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   /* loop through other controllers:
        for those that ARE open, ARE NOT the current one, and ARE locked */

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){

     qq3d = GLOBAL_library.controllers[cc] ; /* controller */

     if( IM3D_OPEN(qq3d) && qq3d != im3d && ((1<<cc) & glock) != 0 ){

       AFNI_equate_pbars( qq3d , im3d ) ;
     }
   }

   busy = 0 ;  /* OK, let this routine be activated again */
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_thrdrag_lock_carryout( Three_D_View *im3d )
{
   Three_D_View *qq3d ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */
   int glock , cc,ii , dothresh,dopval , ival , stop ;
   float thresh , pval=0.0f , tval ;
   char *eee ;

ENTRY("AFNI_thrdrag_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;     /* not a handgun */

   if( busy )                         EXRETURN;  /* routine already busy */
   if( glock == 0 )                   EXRETURN;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )             EXRETURN;  /* bad input */
   if( GLOBAL_library.ignore_lock )   EXRETURN;  /* ordered not to do anything */

   eee = getenv( "AFNI_THRESH_LOCK" ) ;          /* determine how to lock */
   if( eee == NULL ) EXRETURN ;
   dothresh = (*eee == 'V' || *eee == 'v') ;
   dopval   = (*eee == 'P' || *eee == 'p') && im3d->fim_now != NULL ;
   if( !dothresh && !dopval ) EXRETURN ;         /* no command? */

   ii = AFNI_controller_index(im3d) ;           /* which one am I? */

   if( ii < 0 ) EXRETURN ;                      /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 ) EXRETURN ;      /* input not locked */

   /* something to do? */

   busy = 1 ;  /* don't let this routine be called recursively */

   ival   = rint(im3d->vinfo->func_threshold/THR_FACTOR) ;
   thresh = im3d->vinfo->func_threshold * im3d->vinfo->func_thresh_top ;
   stop   = (int)( rint( pow(10.0,THR_TOP_EXPON) ) - 1.0 ) ;

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
           ival = rint( tval/(THR_FACTOR*qq3d->vinfo->func_thresh_top) ) ;
           if( ival < 0 ) ival = 0 ; else if( ival > stop ) ival = stop ;

         } else if( !dothresh ){
           continue ;  /* skip this [dopval set, but not a statistic] */
         }

         /* set the slider and pval marker */

         XmScaleSetValue( qq3d->vwid->func->thr_scale , ival ) ;
         qq3d->vinfo->func_threshold = THR_FACTOR * ival ;
         AFNI_set_thr_pval( qq3d ) ;
       }
     }
   }

   busy = 0 ;  /* OK, let this routine be activated again */
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

void AFNI_range_lock_carryout( Three_D_View *im3d )
{
   Three_D_View *qq3d ;
   static int busy = 0 ;  /* !=0 if this routine is "busy" */
   int glock , cc,ii,nn ;
   float val ;
   char cmd[64] , *eee ;

ENTRY("AFNI_range_lock_carryout") ;

   /* first, determine if there is anything to do */

   glock = GLOBAL_library.controller_lock ;      /* not a handgun */

   if( busy )                         EXRETURN;  /* routine already busy */
   if( glock == 0 )                   EXRETURN;  /* nothing to do */
   if( !IM3D_OPEN(im3d) )             EXRETURN;  /* bad input */
   if( GLOBAL_library.ignore_lock )   EXRETURN;  /* ordered not to do anything */

   eee = getenv( "AFNI_RANGE_LOCK" );            /* determine how to lock */
   if( eee == NULL )                  EXRETURN;
   if( *eee != 'Y' && *eee != 'y' )   EXRETURN;

   ii = AFNI_controller_index(im3d);             /* which one am I? */

   if( ii < 0 )                       EXRETURN;  /* nobody? bad input! */
   if( ((1<<ii) & glock) == 0 )       EXRETURN;  /* input not locked */

   /* get range of this controller */

   val = im3d->vinfo->fim_range ;
   if( val <= 0.0 )                   EXRETURN;  /* shouldn't happen */

   /* count how OTHER controllers are are open and locked;
      if none of them, there is nothing to do [29 Apr 2005] */

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
