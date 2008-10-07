/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#undef MAIN

#include "afni.h"
#include "afni_plugout.h"

/*-------------------------------------------------------------------
   This routine is also used by the macros
      AFNI_SEE_FUNC_ON and AFNI_SEE_FUNC_OFF
   as well as the MCW_bbox that it is attached to as the callback.
---------------------------------------------------------------------*/

void AFNI_see_func_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int old_val , new_val ;

ENTRY("AFNI_see_func_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   old_val = (im3d->vinfo->func_visible) ? 1 : 0 ;
   new_val = MCW_val_bbox( im3d->vwid->view->see_func_bbox ) ;

   if( old_val != new_val ){
     im3d->vinfo->func_visible = (new_val == 1) ? True : False ;
     if( ! ISVALID_3DIM_DATASET(im3d->fim_now) ){             /* 29 Apr 1997 */
       im3d->vinfo->func_visible = False ;
       MCW_set_bbox( im3d->vwid->view->see_func_bbox , 0 ) ; /* 29 Jan 1999 */
     }
     AFNI_disable_suma_overlay( 0 ) ; /* 16 Jun 2003 */
     AFNI_redisplay_func( im3d ) ;    /* 05 Mar 2002 */
     im3d->vinfo->func_visible_count++ ; /* 03 Aug 2007 */
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! Get the threshold automatically.  [05 Mar 2007]
-------------------------------------------------------------------------*/

float AFNI_get_autothresh( Three_D_View *im3d )
{
   MRI_IMAGE *thrim ;
   float thrval,pval=0.0f ; int ival ;

ENTRY("AFNI_get_autothresh") ;

   if( !IM3D_OPEN(im3d) || im3d->fim_now == NULL ) RETURN(-1.0f) ;

   ival = im3d->vinfo->thr_index ;  /* threshold sub-brick index */

   if( DSET_BRICK_STATCODE(im3d->fim_now,ival) > 0 )
     pval = THD_pval_to_stat( 1.e-3 ,
                              DSET_BRICK_STATCODE(im3d->fim_now,ival) ,
                              DSET_BRICK_STATAUX (im3d->fim_now,ival)  ) ;

   DSET_load( im3d->fim_now ) ;
   thrim  = DSET_BRICK(im3d->fim_now,ival) ;
   thrval = THD_cliplevel_abs( thrim , -0.500f ) ;
   if( DSET_BRICK_FACTOR(im3d->fim_now,ival) > 0.0f )
     thrval *= DSET_BRICK_FACTOR(im3d->fim_now,ival) ;

   if( pval > 0.0f )
     thrval = (thrval <= 0.0f) ? pval : sqrt(thrval*pval) ;

   if( thrval == 0.0f ) thrval = -1.0f ;
   RETURN(thrval) ;
}

/*-----------------------------------------------------------------------*/
/*! 25 Jul 2007 */

void AFNI_func_autothresh_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   float new_thresh ;

ENTRY("AFNI_func_autothresh_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   new_thresh = AFNI_get_autothresh(im3d) ;
   if( new_thresh > 0.0f ) AFNI_set_threshold(im3d,new_thresh) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! 29 Jan 2008 */

void AFNI_func_fdr_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   THD_3dim_dataset *dset ; int nf ;

ENTRY("AFNI_func_fdr_CB") ;

   if( !IM3D_OPEN(im3d) || !ISVALID_DSET(im3d->fim_now) ) EXRETURN ;
   dset = im3d->fim_now ;
   SHOW_AFNI_PAUSE ;
   nf = THD_create_all_fdrcurves(dset) ;
   AFNI_set_thr_pval(im3d) ;
   INFO_message("Computed %d FDR curves in %s [but not saved on disk]" ,
                nf , DSET_FILECODE(dset) ) ;
   SHOW_AFNI_READY ;

   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! 08 Aug 2007 */

void AFNI_func_thrsign_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_func_thrsign_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   im3d->vinfo->thr_sign = av->ival ;
   AFNI_redisplay_func( im3d ) ;
   AFNI_set_window_titles( im3d ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! Set the threshold and slider.  [05 Mar 2007]
-------------------------------------------------------------------------*/

void AFNI_set_threshold( Three_D_View *im3d , float val )
{
   int olddec,newdec , smax,stop , ival ;
   static float tval[9] = { 1.0 , 10.0 , 100.0 , 1000.0 , 10000.0 ,
                            100000.0 , 1000000.0 , 10000000.0 , 100000000.0 } ;

ENTRY("AFNI_set_threshold") ;

   if( !IM3D_OPEN(im3d) || val < 0.0f || val > THR_TOP_VALUE ) EXRETURN;

   /* get current scale decimal setting */

   olddec = (int)rint( log10(im3d->vinfo->func_thresh_top) ) ;
        if( olddec < 0             ) olddec = 0 ;
   else if( olddec > THR_TOP_EXPON ) olddec = THR_TOP_EXPON ;
   newdec = olddec ;

   if( val > 0.0f ){
     newdec = (int)( log10(val) + 1.0 ) ;
          if( newdec < 0             ) newdec = 0 ;
     else if( newdec > THR_TOP_EXPON ) newdec = THR_TOP_EXPON ;
     if( newdec != olddec )
       AFNI_set_thresh_top( im3d , tval[newdec] ) ;
   }

   smax  = (int)rint( pow(10.0,THR_TOP_EXPON) ) ;
   stop  = smax - 1 ;                             /* max slider value */

   ival = rint( val/(THR_FACTOR*tval[newdec]) ) ;
        if( ival < 0    ) ival = 0    ;
   else if( ival > stop ) ival = stop ;

   XmScaleSetValue( im3d->vwid->func->thr_scale , ival ) ;
   AFNI_thr_scale_CB( im3d->vwid->func->thr_scale, (XtPointer)im3d, NULL ) ;
   FIX_SCALE_SIZE(im3d) ;
   AFNI_thresh_lock_carryout(im3d) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------
   Called when the scale for the threshold is adjusted.
   30 Oct 1996: changed scale factor from slider to threshold
                from 0.01 to 0.001 to allow for increased
                precision of scale (0..999 instead of 0..99).
-------------------------------------------------------------------------*/

void AFNI_thr_scale_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   Three_D_View *im3d = (Three_D_View *) client_data ;
   XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) call_data ;
   float fff ;
   int redisplay , ival ;

ENTRY("AFNI_thr_scale_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   if( cbs != NULL ) ival = cbs->value ;
   else              XmScaleGetValue( w , &ival ) ;

   fff = THR_FACTOR * ival ;
   if( fff >= 0.0 && fff <= 1.0 ) im3d->vinfo->func_threshold = fff ;

   FIX_SCALE_VALUE(im3d) ;
   FIX_SCALE_SIZE(im3d) ;   /* 09 May 2001 */

   redisplay = (im3d->vinfo->func_visible) ? REDISPLAY_OVERLAY
                                           : REDISPLAY_OPTIONAL ;
   AFNI_set_thr_pval( im3d ) ;

   MCW_discard_events_all( w , ButtonPressMask ) ;  /* 20 Mar 2007 */

   if( im3d->vinfo->func_pval >= 0.0 && im3d->vinfo->func_pval <= 1.0 ){
     char pstr[64] ;
     sprintf( pstr , "Nominal p=%.4e" , im3d->vinfo->func_pval ) ;
     if( im3d->vinfo->func_qval >= 0.0 && im3d->vinfo->func_qval <= 1.0 )
       sprintf(pstr+strlen(pstr),"; FDR q=%.4e",im3d->vinfo->func_qval) ;
     else
       sprintf(pstr+strlen(pstr),"; FDR q=N/A") ;
     MCW_register_hint( im3d->vwid->func->thr_pval_label , pstr ) ;
   } else {
     MCW_register_hint( im3d->vwid->func->thr_pval_label ,
                        "Nominal p-value per voxel"       ) ;
   }

   if( ! DOING_REALTIME_WORK ) AFNI_redisplay_func( im3d ) ;

   AFNI_thresh_lock_carryout(im3d) ;  /* 06 Feb 2004 */

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------
  Called when the user drags the threshold scale at all, even
  without releasing it.  Used to update the "p-value" interactively.
----------------------------------------------------------------------*/

void AFNI_thr_scale_drag_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   Three_D_View *im3d = (Three_D_View *) client_data ;
   XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *) call_data ;
   float fff ;

ENTRY("AFNI_thr_scale_drag CB") ;

   if( IM3D_OPEN(im3d) && ISVALID_3DIM_DATASET(im3d->fim_now) ){

      fff = THR_FACTOR * cbs->value ;
      if( fff >= 0.0 && fff <= 1.0 ) im3d->vinfo->func_threshold = fff ;

      FIX_SCALE_VALUE(im3d) ;
      if( FUNC_HAVE_PVAL(DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index)) )
        AFNI_set_thr_pval( im3d ) ;

      AFNI_thrdrag_lock_carryout( im3d ) ; /* 10 Feb 2004 */
   }
   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Set the top value used on the threshold slider.
  (Should be followed by a function redisplay if needed.)
-------------------------------------------------------------------------*/

void AFNI_set_thresh_top( Three_D_View *im3d , float tval )
{
   int decim ;

ENTRY("AFNI_set_thresh_top") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   if( tval <= 0.0 ) tval = 1.0 ;

   decim = (2*THR_TOP_EXPON) - (int)(THR_TOP_EXPON + 0.01 + log10(tval)) ;
   if( decim < 0 ) decim = 0 ;

   XtVaSetValues( im3d->vwid->func->thr_scale, XmNdecimalPoints, decim, NULL ) ;

   im3d->vinfo->func_thresh_top = tval ;

   FIX_SCALE_VALUE(im3d) ;
   FIX_SCALE_SIZE(im3d) ;   /* 09 May 2001 */
   AFNI_set_thr_pval( im3d ) ;

   /** fix the option menu at the bottom of the scale **/

   decim = THR_TOP_EXPON - decim ;
   if( decim != im3d->vwid->func->thr_top_av->ival )
     AV_assign_ival( im3d->vwid->func->thr_top_av , decim ) ;

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Return the label for the threshold top chooser.
-------------------------------------------------------------------------*/

char * AFNI_thresh_tlabel_CB( MCW_arrowval *av , XtPointer junk )
{
   static char tlab[8] ;
   sprintf(tlab,"%d",av->ival) ;
   return tlab ;
}

/*-----------------------------------------------------------------------
  Take action when the user changes the threshold top chooser.
-------------------------------------------------------------------------*/

void AFNI_thresh_top_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   static float tval[9] = { 1.0 , 10.0 , 100.0 , 1000.0 , 10000.0 ,
                            100000.0 , 1000000.0 , 10000000.0 , 100000000.0 } ;

ENTRY("AFNI_thresh_top_CB") ;

   if( IM3D_OPEN(im3d) && im3d->vinfo->func_thresh_top != tval[av->ival] ){

     AFNI_set_thresh_top( im3d , tval[av->ival] ) ;
     FIX_SCALE_SIZE(im3d) ;

     if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;

     AFNI_thresh_lock_carryout(im3d) ;  /* 06 Feb 2004 */
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Used to set the pval (significance) label at the bottom of the
  threshold scale.
-------------------------------------------------------------------------*/

void AFNI_set_thr_pval( Three_D_View *im3d )
{
   float thresh , pval , zval ;
   int   dec ;
   char  buf[32] ;

ENTRY("AFNI_set_thr_pval") ;

   if( ! IM3D_VALID(im3d) || ! ISVALID_3DIM_DATASET(im3d->fim_now) ) EXRETURN ;

   /* get the "true" threshold (scaled up from being in [0,1]) */

   thresh = im3d->vinfo->func_threshold * im3d->vinfo->func_thresh_top ;

   /* get the p-value that goes with this threshold, for this functional dataset */

   pval = THD_stat_to_pval( thresh ,
              DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) ,
              DSET_BRICK_STATAUX (im3d->fim_now,im3d->vinfo->thr_index)  ) ;

   im3d->vinfo->func_pval = pval ;  /* 06 Feb 2004 */
   im3d->vinfo->func_qval = 0.0f ;  /* 23 Jan 2008 */

if(PRINT_TRACING)
{ char buf[128] ;
  sprintf( buf, "thresh=%g  top=%g  pval=%g",
           thresh,im3d->vinfo->func_thresh_top,pval ) ; STATUS(buf) ; }

   if( pval < 0.0 ){
     strcpy( buf , THR_PVAL_LABEL_NONE ) ;
   } else {
     if( pval == 0.0 ){
       strcpy( buf , "p=0" ) ;
     } else if( pval >= 0.9999 ){
       strcpy( buf , "p=1" ) ;
     } else if( pval >= 0.0010 ){
       char qbuf[16] ;
       sprintf( qbuf , "%5.4f" , pval ) ;
       strcpy(buf,"p=") ; strcat( buf , qbuf+1 ) ; /* qbuf+1 skips leading 0 */
     } else {
       int dec = (int)(0.999 - log10(pval)) ;
       zval = pval * pow( 10.0 , (double) dec ) ;  /* between 1 and 10 */
       if( dec < 10 ) sprintf( buf , "p=%3.1f-%1d" ,           zval , dec ) ;
       else           sprintf( buf , "p=%1d.-%2d"  , (int)rint(zval), dec ) ;
     }
     if( im3d->vedset.code > 0 && im3d->fim_now->dblk->vedim != NULL )
       strcat(buf,"*") ;  /* mark that are in clustering mode [05 Sep 2006] */
   }

   /* 23 Jan 2007: q-value from FDR curve? */

   if( pval >= 0.0 ){
     zval = THD_fdrcurve_zval( im3d->fim_now, im3d->vinfo->thr_index, thresh ) ;
     if( zval > 0.0f ){
       float qval = 2.0*qg(zval) ;         /* convert z back to FDR q */
       im3d->vinfo->func_qval = qval ;
       if( qval > 0.0f & qval < 0.9999 ){
         char qbuf[16] ;
         if( qval >= 0.0010 ) sprintf(qbuf,"%5.4f",qval) ;
         else {
           int dec = (int)(0.999 - log10(qval)) ;
           zval = qval * pow( 10.0 , (double)dec ) ;  /* between 1 and 10 */
           if( dec < 10 ) sprintf( qbuf, " %3.1f-%1d",            zval, dec );
           else           sprintf( qbuf, " %1d.-%2d" , (int)rint(zval), dec );
         }
         strcat(buf,"\nq=") ; strcat(buf,qbuf+1) ;
       }
     } else {
       strcat(buf,"\nq=N/A") ;
     }
   }

   MCW_set_widget_label( im3d->vwid->func->thr_pval_label , buf ) ;
   FIX_SCALE_SIZE(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------
   30 Mar 2001: add range hints to a pbar
-----------------------------------------------------------------------------*/

void AFNI_hintize_pbar( MCW_pbar *pbar ,  float fac )
{
   int ip , np ;
   Widget w ;
   char sbot[16],stop[16] , hint[64] , *sb,*st ;
   float bot , top ;

ENTRY("AFNI_hintize_pbar") ;

   if( pbar == NULL || fac == 0.0 ) EXRETURN ;  /* bad */

   if( pbar->bigmode ){
     MCW_register_hint( pbar->panes[0] ,
                        "Button-1=flip;  Button-3=menu" ) ;
     pbar->bigfac = fac ;                      /* 11 Feb 2003 */
   } else {
     np = pbar->num_panes ;
     for( ip=0 ; ip < np ; ip++ ){
       w   = pbar->panes[ip] ;          /* the widget for the ip-th pane */
       top = pbar->pval[ip]   * fac ;   /* scaled top value */
       bot = pbar->pval[ip+1] * fac ;   /* scaled bot value */
       AV_fval_to_char( bot , sbot ) ;  /* convert to a nice string */
       AV_fval_to_char( top , stop ) ;
       sb = (sbot[0] == ' ') ? sbot+1 : sbot ;  /* skip leading blanks */
       st = (stop[0] == ' ') ? stop+1 : stop ;
       sprintf(hint,"%s .. %s",sb,st) ;         /* create hint */
       MCW_register_hint( w , hint ) ;          /* send to hint system */
     }
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------
  called when the pbar for the intensity mapping is adjusted
  (thresholds or colors)
------------------------------------------------------------------------------*/

void AFNI_inten_pbar_CB( MCW_pbar *pbar , XtPointer cd , int reason )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   float fac ;

ENTRY("AFNI_inten_pbar_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   if( im3d->vinfo->func_visible )
      AFNI_redisplay_func( im3d ) ;

   AFNI_hintize_pbar( pbar ,
                      (im3d->vinfo->fim_range != 0.0) ? im3d->vinfo->fim_range
                                                      : im3d->vinfo->fim_autorange ) ;

   FIX_SCALE_SIZE(im3d) ;

   AFNI_pbar_lock_carryout(im3d) ; /* 07 Feb 2004 */
   EXRETURN ;
}

/*-----------------------------------------------------------------------------
  30 Mar 2001: rotate the colors on the pbar
-------------------------------------------------------------------------------*/

void AFNI_range_rotate_av_CB( MCW_arrowval *av , XtPointer cd )
{
   MCW_pbar *pbar = (MCW_pbar *) cd ;
   int ddd ;

ENTRY("AFNI_range_rotate_av_CB") ;

   /* which way to rotate? */

   if( av->fval > av->old_fval ) ddd = +1 ;
   else                          ddd = -1 ;

   /* Shift+click ==> rotate by 4 (useful for bigmode) */

   if( av->xev.type == ButtonPress ){
     XButtonEvent *event = (XButtonEvent *) (&av->xev) ;
     if( event->state & ShiftMask ) ddd *= 4 ;
   }

   rotate_MCW_pbar( pbar , ddd ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------
  called to set up the pbar for the intensity mapping
-----------------------------------------------------------------------------*/

/** 3/27/95: changed to allow different initializations
             for the [0,1] and the [-1,1] range pbars.  **/

/** 6/01/95: changed to put the initialization constants
             in tables initialized in afni.c, not here. **/

void AFNI_setup_inten_pbar( Three_D_View *im3d )
{
  MCW_pbar *pbar ;
  int np , i , jm , lcol ;

ENTRY("AFNI_setup_inten_pbar") ;

  if( ! IM3D_VALID(im3d) ) EXRETURN ;

  pbar = im3d->vwid->func->inten_pbar ;
  jm   = pbar->mode ;
  lcol = im3d->dc->ovc->ncol_ov - 1 ;

  /** load the 'save' values for all possible pane counts **/

  for( np=NPANE_MIN ; np <= NPANE_MAX ; np++ ){

      for( i=0 ; i <= np ; i++ ){
         pbar->pval_save[np][i][0] = INIT_pval_sgn[np][i] ;
         pbar->pval_save[np][i][1] = INIT_pval_pos[np][i] ;
      }

      for( i=0 ; i <  np ; i++ ){
         pbar->ovin_save[np][i][0] = MIN( lcol , INIT_ovin_sgn[np][i] ) ;
         pbar->ovin_save[np][i][1] = MIN( lcol , INIT_ovin_pos[np][i] ) ;
      }
  }

  /** load the values for the current pane count **/

  np = pbar->num_panes ;
  jm = pbar->mode ;

  for( i=0 ; i <= np ; i++ ) pbar->pval[i]     = pbar->pval_save[np][i][jm] ;
  for( i=0 ; i <  np ; i++ ) pbar->ov_index[i] = pbar->ovin_save[np][i][jm] ;

  pbar->update_me = 1 ;
  EXRETURN ;
}

/*----------------------------------------------------------------------------
  called when the arrowval for the number of pbar panes is clicked
------------------------------------------------------------------------------*/

void AFNI_inten_av_CB( MCW_arrowval *av , XtPointer cd )
{
   MCW_pbar *pbar = (MCW_pbar *) cd ;
   Three_D_View *im3d = (Three_D_View *) pbar->parent ;

   HIDE_SCALE(im3d) ;
   if( av->ival > NPANE_MAX ){
     int npane=pbar->num_panes , jm=pbar->mode ;
     float pmax=pbar->pval_save[npane][0][jm] ,
           pmin=pbar->pval_save[npane][npane][jm] ;
     PBAR_set_bigmode( pbar , 1 , pmin,pmax ) ;
     AFNI_inten_pbar_CB( pbar , im3d , 0 ) ;
     POPUP_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
   } else {
     pbar->bigmode = 0 ;
     alter_MCW_pbar( pbar , av->ival , NULL ) ;
     NORMAL_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
   }
   FIX_SCALE_SIZE(im3d) ;
}

char * AFNI_inten_av_texter( MCW_arrowval *av , XtPointer cd )
{
   static char buf[4] ;
   if( av->ival > NPANE_MAX ) strcpy (buf,"**") ;
   else                       sprintf(buf,"%d",av->ival) ;
   return buf ;
}

/*---------------------------------------------------------------------
   Make a warped dataset whose grid corresponds to the anat_parent and
   whose data comes from the data_parent.
   Note that the assumption is made that data_parent and the warp parent
   of the anat_parent are both in the same coordinate system (up to the
   to_dicomm transformation of their dataxes structs).
-----------------------------------------------------------------------*/

THD_3dim_dataset * AFNI_follower_dataset( THD_3dim_dataset *anat_parent ,
                                          THD_3dim_dataset *data_parent  )
{
   THD_3dim_dataset *new_dset ;
   int ii ;

ENTRY("AFNI_follower_dataset") ;

   /* sanity checks */

   if( ! ISVALID_3DIM_DATASET(anat_parent) ||
       ! ISVALID_3DIM_DATASET(data_parent)   ) RETURN(NULL) ;

   /* can't warp a time-dependent dataset (OK, maybe you can) */

   if( DSET_NUM_TIMES(data_parent) > 1 && ! GLOBAL_argopt.warp_4D ) RETURN(NULL) ;

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"anat_parent=%s  data_parent=%s",
          DSET_HEADNAME(anat_parent) , DSET_HEADNAME(data_parent) ) ;
  STATUS(str); }

   /* make new dataset, copying appropriate fields from its various parents */

   new_dset = myXtNew( THD_3dim_dataset ) ; INIT_KILL( new_dset->kl ) ;

   new_dset->type      = data_parent->type ;        /* same data type */
   new_dset->func_type = data_parent->func_type ;
   new_dset->view_type = anat_parent->view_type ;   /* but different view type */

   new_dset->anat_parent = anat_parent ;            /* what else makes sense? */

   new_dset->tagset = NULL ;  /* Oct 1998 */

   MCW_strncpy( new_dset->anat_parent_name ,
                anat_parent->self_name , THD_MAX_NAME ) ;

   new_dset->anat_parent_idcode = anat_parent->idcode ;

   /* 11/09/94 addition: the data_parent may itself be a warp;
       in this case, we want the true warp parent to be the original data */

   new_dset->warp_parent =  (data_parent->warp_parent != NULL)
                          ? (data_parent->warp_parent) : (data_parent) ;

   MCW_strncpy( new_dset->warp_parent_name ,
                new_dset->warp_parent->self_name , THD_MAX_NAME ) ;

   new_dset->warp_parent_idcode = new_dset->warp_parent->idcode ;

   new_dset->idcode = MCW_new_idcode() ;

   /* make the actual warp from the warp_parent to this dataset */

   new_dset->vox_warp       = NULL ;
   new_dset->warp           = myXtNew( THD_warp ) ;
   *(new_dset->warp)        = IDENTITY_WARP ;  /* start with (Dicom) identity */
   new_dset->self_warp      = NULL ;  /* 26 Aug 2002 */

   /* follow the links backward from desired view to original view */

   AFNI_concatenate_warp( new_dset->warp , anat_parent->warp ) ;
   AFNI_concatenate_warp( new_dset->warp , data_parent->warp ) ;

   /* reset the bounds in the new warp to be the same as in the anat_parent */

   if( ISVALID_WARP(anat_parent->warp) &&
       anat_parent->warp->type == new_dset->warp->type ){

      switch( anat_parent->warp->type ){

         case WARP_AFFINE_TYPE:
            COPY_LMAP_BOUNDS( new_dset->warp->rig_bod.warp ,
                              anat_parent->warp->rig_bod.warp ) ;
         break ;

         case WARP_TALAIRACH_12_TYPE:
            for( ii=0 ; ii < 12 ; ii++ )
               COPY_LMAP_BOUNDS( new_dset->warp->tal_12.warp[ii] ,
                                 anat_parent->warp->tal_12.warp[ii] ) ;
         break ;
      }
   }

   /* make up some names for this new dataset */

   MCW_strncpy( new_dset->self_name  ,
                new_dset->warp_parent->self_name , THD_MAX_NAME ) ;
   ii = strlen( new_dset->self_name ) ;
   new_dset->self_name[ii++] = '@' ;
   MCW_strncpy( &(new_dset->self_name[ii]) ,
                VIEW_typestr[new_dset->view_type] , THD_MAX_NAME-ii ) ;

   MCW_strncpy( new_dset->label1 , data_parent->label1 , THD_MAX_LABEL ) ;
   MCW_strncpy( new_dset->label2 , data_parent->label2 , THD_MAX_LABEL ) ;

   /* set the axes for this new dataset
      (same as anatomy parent, since that's the meaning of this routine) */

   new_dset->daxes     = myXtNew( THD_dataxes ) ; /* copy data axes of */
   *(new_dset->daxes)  = *(anat_parent->daxes)  ; /* anatomy parent   */

   new_dset->wod_daxes = NULL ;
   new_dset->wod_flag  = True ;

   /* 06 Aug 1996: added ability to use 3D+t datasets here */

   if( DSET_NUM_TIMES(data_parent) < 2 ){
      new_dset->taxis = NULL ;
   } else {
      new_dset->taxis  = myXtNew( THD_timeaxis ) ;  /* new */
    *(new_dset->taxis) = *(data_parent->taxis) ;  /* copy insides */

      new_dset->taxis->nsl     = 0 ;                      /* no slice stuff */
      new_dset->taxis->toff_sl = NULL ;
      new_dset->taxis->zorg_sl = 0.0 ;
      new_dset->taxis->dz_sl   = 0.0 ;
   }

   /* create a datablock and diskptr, in case the data is ever
      filled into memory (instead of wod) and written to disk */

   new_dset->dblk = myXtNew( THD_datablock ) ; INIT_KILL( new_dset->dblk->kl ) ;

   new_dset->dblk->type        = DATABLOCK_TYPE ;
   new_dset->dblk->nvals       = data_parent->dblk->nvals ;
   new_dset->dblk->malloc_type = DATABLOCK_MEM_UNDEFINED ;
   new_dset->dblk->natr        = new_dset->dblk->natr_alloc  = 0 ;
   new_dset->dblk->atr         = NULL ;
   new_dset->dblk->parent      = (XtPointer) new_dset ;

   new_dset->dblk->vedim = NULL ;  /* 05 Sep 2006 */

   if( data_parent->dblk->brick_lab == NULL ){
     THD_init_datablock_labels( new_dset->dblk ) ; /* 30 Nov 1997 */
   } else {
     THD_copy_datablock_auxdata( data_parent->dblk , new_dset->dblk ) ;
   }

   DSET_unlock(new_dset) ;  /* Feb 1998 */

   new_dset->dblk->diskptr               = myXtNew( THD_diskptr ) ;
   new_dset->dblk->diskptr->type         = DISKPTR_TYPE ;
   new_dset->dblk->diskptr->nvals        = data_parent->dblk->nvals ;
   new_dset->dblk->diskptr->rank         = 3 ;
   new_dset->dblk->diskptr->storage_mode = STORAGE_UNDEFINED ;
   new_dset->dblk->diskptr->byte_order   = THD_get_write_order() ;  /* 25 April 1998 */
   new_dset->dblk->diskptr->dimsizes[0]  = new_dset->daxes->nxx ;
   new_dset->dblk->diskptr->dimsizes[1]  = new_dset->daxes->nyy ;
   new_dset->dblk->diskptr->dimsizes[2]  = new_dset->daxes->nzz ;

   new_dset->dblk->brick_fac   = NULL ;  /* initialized below */
   new_dset->dblk->brick_bytes = NULL ;
   new_dset->dblk->brick       = NULL ;
   THD_init_datablock_brick( new_dset->dblk , -1 , data_parent->dblk ) ;

   new_dset->dblk->master_nvals = 0 ;     /* 11 Jan 1999 */
   new_dset->dblk->master_ival  = NULL ;
   new_dset->dblk->master_bytes = NULL ;

   /* create the names for storage on disk (if ever)
      -- note we put it in the same directory as the data_parent */

   THD_init_diskptr_names( new_dset->dblk->diskptr ,
                           data_parent->dblk->diskptr->directory_name , NULL ,
                           data_parent->dblk->diskptr->prefix ,
                           new_dset->view_type , True ) ;

   ADDTO_KILL( new_dset->dblk->kl , new_dset->dblk->diskptr ) ;

   /* oh yeah, set the new_dset kill list,
      copy statistics if available, and NULL out any unused stuff */

   ADDTO_KILL( new_dset->kl , new_dset->warp ) ;
   ADDTO_KILL( new_dset->kl , new_dset->daxes ) ;
   ADDTO_KILL( new_dset->kl , new_dset->dblk ) ;

   new_dset->stats = NULL ;
   AFNI_copy_statistics( data_parent , new_dset ) ;

   INIT_STAT_AUX( new_dset , MAX_STAT_AUX , data_parent->stat_aux ) ;

   new_dset->markers     = NULL ;  /* no markers */
   new_dset->death_mark  = 0 ;     /* don't kill me! */
   new_dset->tcat_list   = 0 ;
   new_dset->tcat_num    = 0 ;
   new_dset->tcat_len    = NULL ;

#ifdef ALLOW_DATASET_VLIST
   new_dset->pts         = NULL ;
#endif

   PARENTIZE(new_dset,data_parent->parent) ;

   tross_Copy_History( data_parent , new_dset ) ; /* 18 Oct 1999 */

   RETURN(new_dset) ;
}

/*------------------------------------------------------------------------
  Scan through a sessionlist and find any datasets that can be "filled in"
  as 'followers' (as in AFNI_follower_dataset).  That is, find any slots
  that are not now filled, but are "downstream" from original view data
  that has an anatomy parent:

  SPGR:   orig  acpc  ----
           /^\                <--- anatomy parent
            |                 <--- relationship
  FIM:    orig  ----  ----

  In this picture, the first blank in the FIM line can be filled in, using
  the fim+orig dataset as the data_parent and the spgr+acpc dataset as
  the anat_parent for the new fim+acpc dataset.

  The algorithm used to find slots that may be filled in is as simple as
  the picture above -- the 'orig' view is presumed to contain all the basic
  anatomy parent relationships.  This may not be true in the future
  (e.g., when registering one dataset to another, the 'rgst' view will
  then have a basic anatomy parent relationship).  I'll worry about
  that later, as well as about the possibility that the newly created
  datasets may spawn further possible offspring (e.g., if they themselves
  are anatomical images that fill in slots that will become anat_parents).
                                              -- RWCox, November, 1994 A.D.

  3 June 1998:
    The original version of this is renamed to be AFNI_make_descendants_old,
    and is modified to do a sweep starting at the "vbase" view.  Then
    we sweep twice, once from +orig and once again from +acpc.  This is
    to allow for creation of descendants from +acpc that do not have
    ancestors at +orig.
---------------------------------------------------------------------------*/

void AFNI_make_descendants_old( THD_sessionlist * , int ) ; /* prototype */

void AFNI_make_descendants( THD_sessionlist *ssl )
{
   AFNI_make_descendants_old( ssl , VIEW_ORIGINAL_TYPE ) ;
#if 0
   AFNI_make_descendants_old( ssl , VIEW_ACPCALIGNED_TYPE ) ; /* doesn't work */
#endif
   return ;
}

/** In this routine, each occurence of vbase was originally VIEW_ORIGINAL_TYPE **/

void AFNI_make_descendants_old( THD_sessionlist *ssl , int vbase )
{
   int iss , jdd , kvv , num_made=0 ;
   THD_session *ss ;
   THD_3dim_dataset *orig_dset , *new_dset ;
   THD_slist_find     find ;
   THD_3dim_dataset **anat_parent_row , **orig_row ;

ENTRY("AFNI_make_descendants_old") ;

   if( ! ISVALID_SESSIONLIST(ssl) ) EXRETURN ;

   /* loop over each session */

   for( iss=0 ; iss < ssl->num_sess ; iss++ ){
      ss = ssl->ssar[iss] ;
      if( !ISVALID_SESSION(ss) ) continue ;  /* no good ==> skip */

      if( ss == GLOBAL_library.session ) continue ; /* 21 Dec 2001 */

      /* loop over datasets in this session */

      for( jdd=0 ; jdd < ss->num_dsset ; jdd++ ){
         orig_dset = ss->dsset[jdd][vbase] ;                  /* 03 Jun 98 */
         if( !ISVALID_3DIM_DATASET(orig_dset) ||              /* no good   */
             orig_dset->anat_parent == NULL   ||              /* no parent */
             orig_dset->anat_parent == orig_dset ) continue ; /* ==> skip  */

         if( DSET_in_global_session(orig_dset) ) continue ; /* 25 Dec 2001 */

         /* look for orig_dset's anat parent in this sessionlist */

         find = THD_dset_in_sessionlist( FIND_IDCODE ,
                                         &(orig_dset->anat_parent->idcode) ,
                                         ssl , iss ) ;
         if( find.dset == NULL )
            find = THD_dset_in_sessionlist( FIND_NAME ,
                                            orig_dset->anat_parent->self_name ,
                                            ssl , iss ) ;

         /* check for a good find; if it doesn't happen,
            then skip (this eventuality should never occur!) */

         if( find.dset       == NULL ||
             ISFUNC(find.dset)       ||  /* anat parent can't be a func! */
             find.view_index != vbase  ) continue ;

         /* make a pointer to the row of datasets of the anat
            parent (this is like the SPGR row in the picture above) */

         anat_parent_row =
           &(ssl->ssar[find.sess_index]->dsset[find.dset_index][0]) ;

         /* pointer to row of datasets being operated on now
            (like the FIM row in the picture above)          */

         orig_row = &(ss->dsset[jdd][0]) ;

         if( orig_row == anat_parent_row ) continue ;  /* 14 Dec 1999 */

         /* loop over downstream dataset positions (from orig_dset);
            those that don't exist yet, but have entries in the
            anat_parent_row can be brought into being now */

         for( kvv=vbase+1 ; kvv <= LAST_VIEW_TYPE ; kvv++ ){
            if( orig_row[kvv]        != NULL ) continue ;
            if( anat_parent_row[kvv] == NULL ) continue ;

            orig_row[kvv] = AFNI_follower_dataset( anat_parent_row[kvv] ,
                                                   orig_dset ) ;
            num_made ++ ;
         }
      }  /* end of loop over datasets in this session */

   }  /* end of loop over sessions */

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"total # descendants made = %d",num_made) ;
  STATUS(str) ; }

   EXRETURN ;
}

/*-----------------------------------------------------------------------
   Scan through a session and force any un-anat_parented functional
   datasets to have the first available parent, if any.
   (Note that only the dataset anat_parent pointer is set, not the
    anat_parent_name string, so that if the dataset is saved, no record
    of this will be kept.)

   06 Aug 1996: force adoption of anats as well if "do_anats" is True.
   03 Dec 1999: print messages about forced adoptions
-------------------------------------------------------------------------*/

void AFNI_force_adoption( THD_session *ss , Boolean do_anats )
{
   int aa , ff , vv , apref=0 , aset=-1 ;
   THD_3dim_dataset *dset ;
   int quiet = !AFNI_noenv("AFNI_NO_ADOPTION_WARNING") ; /* 03 Dec 1999 */
   int first = 1 ;

ENTRY("AFNI_force_adoption") ;

   if( ! ISVALID_SESSION(ss) || ss->num_dsset == 0 ) EXRETURN ;

   if( ss == GLOBAL_library.session ) EXRETURN ; /* 21 Dec 2001 */

   /* find a "preferred" parent (one with the most number of markers set) */

   for( aa=0 ; aa < ss->num_dsset ; aa++ ){

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"scanning dataset %d for markers",aa) ; STATUS(str) ; }

      dset = ss->dsset[aa][0] ;             /* original view */

      if( ISVALID_3DIM_DATASET(dset) &&     /* if a good dataset */
          ISANAT(dset)               &&     /* and is anatomical */
          dset->markers != NULL        ){   /* and has markers   */

         if( dset->markers->numset > aset ){ /* and has more markers than before */
            apref = aa ;                     /* try this as our "preferred" parent */
            aset  = dset->markers->numset ;
         }
      }
   }

   /* count potential parents [08 Dec 1999: modified from 03 Dec 1999 code] */

   vv = 0 ;
   if( aset >= 0 ){
     for( aa=0 ; aa < ss->num_dsset ; aa++ ){
       dset = ss->dsset[aa][0] ;
       if( ISVALID_DSET(dset)    &&
           ISANAT(dset)          &&
           dset->markers != NULL && dset->markers->numset >= aset ) vv++ ;
     }
   }

   quiet = ( quiet || vv <= 1 ) ; /* be quiet if ordered, or if no alternatives */

if(aset >= 0 && PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"session %s: apref=%d [%s] aset=%d",
          ss->lastname,apref,DSET_HEADNAME(ss->dsset[apref][0]),aset) ;
  STATUS(str) ; }

   /* scan through all datasets, all views */

   for( ff=0 ; ff < ss->num_dsset ; ff++ ){
      for( vv=VIEW_ORIGINAL_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ){

         dset = ss->dsset[ff][vv] ;  /* function that needs parent */

         if( ! ISVALID_3DIM_DATASET(dset) ||
             dset->anat_parent != NULL      ) continue ; /* nothing to do */

         if( !do_anats && ISANAT(dset)      ) continue ; /* 28 Jul 2003 */

         if( DSET_in_global_session(dset) ) continue ; /* 25 Dec 2001 */

         if( ISVALID_3DIM_DATASET(ss->dsset[apref][vv]) ){  /* if preferred is OK, */
            dset->anat_parent = ss->dsset[apref][vv] ;      /* use it here         */
         } else {
            for( aa=0 ; aa < ss->num_dsset ; aa++ ){          /* search for something, */
               if( ISVALID_3DIM_DATASET(ss->dsset[aa][vv])
                   && ISANAT(ss->dsset[aa][vv])           ){  /* anything, and use it  */
                  dset->anat_parent = ss->dsset[aa][vv] ; break ;
               }
            }
         }

         if( !quiet && dset->anat_parent != NULL && dset->anat_parent != dset ){
            if( first ){
              first = 0 ;
              fprintf(stderr,
                      "\nDatasets with a 'forced adoption' of anat parent:\n") ;
            }
            fprintf(stderr,
                    " %s gets parent %s\n",
                    DSET_HEADNAME(dset) ,
                    DSET_HEADNAME(dset->anat_parent) ) ;
         }
      }
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/* Hollow out the the overlay in place -- 21 Mar 2005 - RWCox.
   An interior pixel is defined as one whose 4 nearest neighbors are
   all nonzero.
-------------------------------------------------------------------------*/

static void mri_edgize( MRI_IMAGE *im )
{
   int ii , jj , nx,ny,nxy , joff ;

   if( im == NULL ) return ;

   nx = im->nx ; ny = im->ny ; nxy = nx * ny ;
   if( nx < 3 || ny < 3 ) return ;  /* no interior pixels at all?! */

   switch( im->kind ){

     case MRI_short:{
       short *ajj , *ajm , *ajp , *atemp , *ar ;
       ar    = MRI_SHORT_PTR(im) ;
       atemp = (short *)malloc(sizeof(short)*nxy); if( atemp == NULL ) return;
       memcpy(atemp,ar,sizeof(short)*nxy) ;
       for( jj=1 ; jj < ny-1 ; jj++ ){
         joff = jj * nx ;      /* offset into this row */
         ajj  = atemp + joff ; /* pointer to this row */
         ajm  = ajj-nx ;       /* pointer to last row */
         ajp  = ajj+nx ;       /* pointer to next row */
         for( ii=1 ; ii < nx-1 ; ii++ ){
           if( ajj[ii]   != 0 &&
               ajm[ii]   != 0 && ajp[ii]   != 0 &&
               ajj[ii+1] != 0 && ajj[ii-1] != 0   ) ar[ii+joff] = 0 ;
         }
       }
       free((void *)atemp) ;
     }
     return ;

     case MRI_rgb:{
       rgbyte *ajj , *ajm , *ajp , *atemp , *ar ;
       ar    = (rgbyte *)MRI_RGB_PTR(im) ;
       atemp = (rgbyte *)malloc(sizeof(rgbyte)*nxy); if( atemp == NULL ) return;
       memcpy(atemp,ar,sizeof(rgbyte)*nxy) ;
       for( jj=1 ; jj < ny-1 ; jj++ ){
         joff = jj * nx ;
         ajj  = atemp + joff ;
         ajm  = ajj-nx ;
         ajp  = ajj+nx ;
         for( ii=1 ; ii < nx-1 ; ii++ ){
           if( !RGBZEQ(ajj[ii])   &&
               !RGBZEQ(ajm[ii])   && !RGBZEQ(ajp[ii])   &&
               !RGBZEQ(ajj[ii+1]) && !RGBZEQ(ajj[ii-1])   ) RGBZAS(ar[ii+joff]) ;
         }
       }
       free((void *)atemp) ;
     }
     return ;

   }

   return ; /* default im->kind case ==> do nothing */
}

/*-----------------------------------------------------------------------*/

#undef  ZREG
#define ZREJ(val) (reject_zero && (val)==0)   /* 20 Apr 2005 */

#undef  THBOT
#undef  THTOP
#undef  THBIG
#define THBIG    1.e+9f
#define THBOT(t) ((thrsign==0 || thrsign==2) ? (-(t)) : (-THBIG))
#define THTOP(t) ((thrsign==0 || thrsign==1) ? (t)    :  (THBIG))

/*-----------------------------------------------------------------------*/
/*!  Make a functional overlay -- very simple routine at present;
   n = slice index , br_fim = data structure to extract slice from.
  Note that imseq requires that the overlay be an image of shorts.
  This routine requires that the function and threshold images be bytes,
  shorts, or floats (complex-valued functional images are not supported).
  (The underlay image may be any legal type; this image is not used here.)
-------------------------------------------------------------------------*/

MRI_IMAGE * AFNI_func_overlay( int n , FD_brick *br_fim )
{
   Three_D_View *im3d ;
   MRI_IMAGE *im_thr , *im_fim , *im_ov ;
   short *ar_ov ;
   short fim_ovc[NPANE_MAX+1] ;
   int npix , ii , lp , num_lp , ival ;
   float scale_factor , scale_thr ;
   MCW_pbar *pbar ;
   int simult_thr , need_thr ;
   int reject_zero = !AFNI_yesenv("AFNI_OVERLAY_ZERO") ; /* 20 Apr 2005 */
   int thrsign ; /* 08 Aug 2007 */

ENTRY("AFNI_func_overlay") ;

   /* sanity check */

   if( br_fim == NULL || n < 0 ) RETURN(NULL) ;  /* nothing to do */

   im3d = (Three_D_View *) br_fim->parent ;

   ival = im3d->vinfo->thr_index ;  /* threshold sub-brick index */

   /* get the component images */

   LOAD_DSET_VIEWS(im3d) ; /* 02 Nov 1996 */

   need_thr = (im3d->vinfo->func_threshold > 0.0) && im3d->vinfo->thr_onoff ;
   thrsign  = im3d->vinfo->thr_sign ; /* 08 Aug 2007 */

   /* 29 Mar 2005: make sure statistics of overlay dataset are ready */

   DSET_load( im3d->fim_now ) ;
   if( !im3d->vinfo->stats_func_ok || (!im3d->vinfo->stats_thresh_ok && need_thr) )
     AFNI_reset_func_range( im3d ) ;

   /* get the threshold image? */

   if( need_thr ){
     STATUS("fetch im_thr") ;
     im_thr = FD_warp_to_mri( n , ival , br_fim ) ;
   } else{
     STATUS("don't need im_thr") ;
     im_thr = NULL ;
   }

#if 1
   scale_thr = 1.0f ;   /* FD_warp_to_mri() already scales this */
#else
   scale_thr = DSET_BRICK_FACTOR(br_fim->dset,ival) ;
   if( scale_thr == 0.0f ) scale_thr = 1.0f ;
#endif

   /* get function image */

   { int ind ;

     ind = im3d->vinfo->fim_index ;
     if( ind >= DSET_NVALS(br_fim->dset) )
       ind = DSET_NVALS(br_fim->dset) - 1 ;

     if( im_thr != NULL && ind == ival ){   /* 06 Feb 2003: allow for */
       STATUS("copy im_thr to im_fim") ;
       im_fim = mri_copy( im_thr ) ;        /* func image = thr image */
     } else {
       STATUS("fetch im_fim") ;
       im_fim = FD_warp_to_mri( n, ind, br_fim ) ;  /* get func image */
     }
     scale_factor = im3d->vinfo->fim_range ;
     if( scale_factor == 0.0 ) scale_factor = im3d->vinfo->fim_autorange ;
     if( scale_factor == 0.0 ) scale_factor = 1.0 ;

     AFNI_set_valabel( br_fim , n , im_fim , im3d->vinfo->func_val ) ;
   }

   /* 04 Mar 2003: convert thresh to float if its datum is unacceptable */
   /* 21 Dec 2004: also allow RGB images as threshold via this mechanism */

   if( im_thr != NULL && !(im_thr->kind == MRI_float ||
                           im_thr->kind == MRI_short ||
                           im_thr->kind == MRI_byte    ) ){

     MRI_IMAGE *qim = mri_to_float(im_thr) ;
     STATUS("scaled im_thr to floats") ;
     mri_free(im_thr) ; im_thr = qim ; scale_thr = 1.0f ;
   }

   AFNI_set_valabel( br_fim , n , im_thr , im3d->vinfo->thr_val ) ;

   /* if component images not good, quit now */

   if( im_fim == NULL ){
     STATUS("couldn't get Func image!") ;
     KILL_1MRI(im_thr) ; RETURN(NULL) ;
   }

   /* 15 Apr 2002: allow overlay image to be pure RBG (no colorscale) */
   /* 20 Dec 2004: allow thresholding of RGB overlays */

   if( im_fim->kind == MRI_rgb ){                  /* 15 Apr 2002: RGB overlays */

     if( im_thr != NULL && im_thr != im_fim ){     /* 20 Dec 2004: threshold */
       float thresh = im3d->vinfo->func_threshold
                    * im3d->vinfo->func_thresh_top / scale_thr ;
       float thb=THBOT(thresh) , tht=THTOP(thresh) ; /* 08 Aug 2007 */
       mri_threshold( thb,tht , im_thr , im_fim ) ;  /* in place */
     }

     /* 27 Apr 2005: transform RGB func image in place? */

     mri_rgb_transform_nD( im_fim, 0, im3d->vwid->func->pbar_transform0D_func );
     mri_rgb_transform_nD( im_fim, 2, im3d->vwid->func->pbar_transform2D_func );

     if( im_thr != im_fim ) KILL_1MRI(im_thr) ;  /* toss the trash */
     RETURN(im_fim) ;
   }

   if( ! AFNI_GOOD_FUNC_DTYPE(im_fim->kind) ){   /* should never happen! */
     MRI_IMAGE *qim = mri_to_float(im_fim) ;     /* (but fix it if it does) */
     STATUS("had to convert im_fim to floats?????") ;
     mri_free(im_fim) ; im_fim = qim ;
   }

   /* 15 Jun 2000: transformation of functional image? */

   if( im3d->vwid->func->pbar_transform0D_func != NULL ){
     MRI_IMAGE *tim = mri_to_float(im_fim) ;
     STATUS("transform0D of im_fim") ;
#if 0
     im3d->vwid->func->pbar_transform0D_func( tim->nvox , MRI_FLOAT_PTR(tim) ) ;
#else
     AFNI_CALL_0D_function( im3d->vwid->func->pbar_transform0D_func ,
                            tim->nvox , MRI_FLOAT_PTR(tim)           ) ;
#endif
     if( im_fim != im_thr ) mri_free(im_fim) ;
     im_fim = tim ;
   }

   if( im3d->vwid->func->pbar_transform2D_func != NULL ){
     MRI_IMAGE *tim = mri_to_float(im_fim) ;
     STATUS("transform2D of im_fim") ;
#if 0
     im3d->vwid->func->pbar_transform2D_func( tim->nx, tim->ny,
                                              tim->dx, tim->dy,
                                              MRI_FLOAT_PTR(tim) ) ;
#else
     AFNI_CALL_2D_function( im3d->vwid->func->pbar_transform2D_func ,
                            tim->nx, tim->ny, tim->dx, tim->dy, MRI_FLOAT_PTR(tim) ) ;
#endif
     if( im_fim != im_thr ) mri_free(im_fim) ;
     im_fim = tim ;
   }

   pbar = im3d->vwid->func->inten_pbar ;

   /** 30 Jan 2003:
       if the pbar is in "big" mode,
       then create an RGB overlay in a separate function **/

   if( pbar->bigmode ){
     float thresh =  im3d->vinfo->func_threshold
                   * im3d->vinfo->func_thresh_top / scale_thr ;
     float thb=THBOT(thresh) , tht=THTOP(thresh) ; /* 08 Aug 2007 */

if( PRINT_TRACING && im_thr != NULL )
{ char str[256] ; float tmax ;
  sprintf(str,"im_thr: nx=%d ny=%d kind=%s",
          im_thr->nx,im_thr->ny,MRI_TYPE_NAME(im_thr)) ; STATUS(str) ;
  tmax = (float)mri_maxabs(im_thr) ;
  sprintf(str,"maxabs(im_thr)=%g scale_thr=%g thresh=%g",tmax,scale_thr,thresh);
  STATUS(str) ;
  sprintf(str,"func_threshold=%g func_thresh_top=%g",
          im3d->vinfo->func_threshold,im3d->vinfo->func_thresh_top); STATUS(str);
}

     im_ov = AFNI_newfunc_overlay( im_thr , thb,tht ,
                                   im_fim ,
                                   scale_factor*pbar->bigbot ,
                                   scale_factor*pbar->bigtop ,
                                   pbar->bigcolor              ) ;
     goto CLEANUP ;
   }

   /** create output image the old way (indexes into overlay colors) **/

   npix  = im_fim->nx * im_fim->ny ;
   im_ov = mri_new( im_fim->nx , im_fim->ny , MRI_short ) ;
   ar_ov = MRI_SHORT_PTR( im_ov ) ;

   /* set overlay colors */

   num_lp = pbar->num_panes ;

   for( lp=0 ; lp < num_lp ; lp++ ) fim_ovc[lp] = pbar->ov_index[lp] ;

   fim_ovc[num_lp] = (im3d->vinfo->use_posfunc) ? (0) : (fim_ovc[num_lp-1]) ;

   /** process im_fim into overlay, depending on data type **/

   simult_thr = (im_thr != NULL) &&                     /* do threshold    */
                (im3d->vinfo->func_threshold > 0.0) &&  /* simultaneously? */
                (im_fim->kind == im_thr->kind) ;        /* (July 15, 1996) */

   switch( im_fim->kind ){

      default:                             /* should not happen! */
         if( im_thr != im_fim ) mri_free(im_thr) ;
         mri_free(im_fim) ; mri_free(im_ov) ;
STATUS("bad im_fim->kind!") ;
      RETURN(NULL) ;

      case MRI_short:{
         short *ar_fim = MRI_SHORT_PTR(im_fim) ;
         float fim_thr[NPANE_MAX] ;  /* 13 Nov 1996: changed from short */

         for( lp=0 ; lp < num_lp ; lp++ )
           fim_thr[lp] = scale_factor * pbar->pval[lp+1] ;

         if( simult_thr ){
           float thresh = im3d->vinfo->func_threshold
                        * im3d->vinfo->func_thresh_top / scale_thr ;
           float thb=THBOT(thresh) , tht=THTOP(thresh) ; /* 08 Aug 2007 */
           short *ar_thr = MRI_SHORT_PTR(im_thr) ;
           for( ii=0 ; ii < npix ; ii++ ){
             if( (ar_thr[ii] > thb && ar_thr[ii] < tht) || ZREJ(ar_fim[ii]) ){
               ar_ov[ii] = 0 ;
             } else {
               for( lp=0 ; lp < num_lp && ar_fim[ii] < fim_thr[lp] ; lp++ ) ; /*nada*/
               ar_ov[ii] = fim_ovc[lp] ;
             }
           }
         } else {
           for( ii=0 ; ii < npix ; ii++ ){
             if( ZREJ(ar_fim[ii]) ){
               ar_ov[ii] = 0 ;
             } else {
               for( lp=0 ; lp < num_lp && ar_fim[ii] < fim_thr[lp] ; lp++ ) ; /*nada*/
                 ar_ov[ii] = fim_ovc[lp] ;
             }
           }
         }
      }
      break ;

      case MRI_byte:{
         byte *ar_fim = MRI_BYTE_PTR(im_fim) ;
         float fim_thr[NPANE_MAX] ;  /* 13 Nov 1996: changed from short */

         for( lp=0 ; lp < num_lp ; lp++ )
           fim_thr[lp] = (pbar->pval[lp+1] > 0.0) ? scale_factor*pbar->pval[lp+1]
                                                  : 0.0                          ;

         if( simult_thr ){
           float thresh = im3d->vinfo->func_threshold
                         * im3d->vinfo->func_thresh_top / scale_thr ;
           float thb=THBOT(thresh) , tht=THTOP(thresh) ; /* 08 Aug 2007 */
           byte *ar_thr = MRI_BYTE_PTR(im_thr) ;

           for( ii=0 ; ii < npix ; ii++ ){
             if( ar_thr[ii] < tht || ZREJ(ar_fim[ii]) ){  /* assuming thb <= 0 */
               ar_ov[ii] = 0 ;
             } else {
               for( lp=0 ; lp < num_lp && ar_fim[ii] < fim_thr[lp] ; lp++ ) ; /*nada*/
               ar_ov[ii] = fim_ovc[lp] ;
             }
           }
         } else {
           for( ii=0 ; ii < npix ; ii++ ){
             if( ZREJ(ar_fim[ii]) ){
               ar_ov[ii] = 0 ;
             } else {
               for( lp=0 ; lp < num_lp && ar_fim[ii] < fim_thr[lp] ; lp++ ) ; /*nada*/
                 ar_ov[ii] = fim_ovc[lp] ;
             }
           }
         }
      }
      break ;

      case MRI_float:{
         float *ar_fim = MRI_FLOAT_PTR(im_fim) ;
         float fim_thr[NPANE_MAX] ;

         for( lp=0 ; lp < num_lp ; lp++ )
           fim_thr[lp] = scale_factor * pbar->pval[lp+1] ;

         if( simult_thr ){
           float thresh = im3d->vinfo->func_threshold * im3d->vinfo->func_thresh_top ;
           float thb=THBOT(thresh) , tht=THTOP(thresh) ; /* 08 Aug 2007 */
           float *ar_thr = MRI_FLOAT_PTR(im_thr) ;

           for( ii=0 ; ii < npix ; ii++ ){
             if( (ar_thr[ii] > thb && ar_thr[ii] < tht) || ZREJ(ar_fim[ii])  ){
               ar_ov[ii] = 0 ;
             } else {
               for( lp=0 ; lp < num_lp && ar_fim[ii] < fim_thr[lp] ; lp++ ) ; /*nada*/
               ar_ov[ii] = fim_ovc[lp] ;
             }
           }
         } else {
           for( ii=0 ; ii < npix ; ii++ ){
             if( ZREJ(ar_fim[ii]) ){
               ar_ov[ii] = 0 ;
             } else {
               for( lp=0 ; lp < num_lp && ar_fim[ii] < fim_thr[lp] ; lp++ ) ; /*nada*/
               ar_ov[ii] = fim_ovc[lp] ;
             }
           }
         }
      }
      break ;
   }

   /** if have yet to do threshold, clip the overlay **/

   if( im_thr != NULL && im3d->vinfo->func_threshold > 0.0 && !simult_thr ){

     switch( im_thr->kind ){

       case MRI_short:{
         float thresh = im3d->vinfo->func_threshold
                      * im3d->vinfo->func_thresh_top / scale_thr ;
         float thb=THBOT(thresh) , tht=THTOP(thresh) ; /* 08 Aug 2007 */
         short *ar_thr = MRI_SHORT_PTR(im_thr) ;

         for( ii=0 ; ii < npix ; ii++ )
           if( ar_thr[ii] > thb && ar_thr[ii] < tht ) ar_ov[ii] = 0 ;
       }
       break ;

       case MRI_byte:{
         float thresh = im3d->vinfo->func_threshold
                      * im3d->vinfo->func_thresh_top / scale_thr ;
         float thb=THBOT(thresh) , tht=THTOP(thresh) ; /* 08 Aug 2007 */
         byte *ar_thr = MRI_BYTE_PTR(im_thr) ;

         for( ii=0 ; ii < npix ; ii++ )  /* assuming thb <= 0 */
           if( ar_thr[ii] < tht ) ar_ov[ii] = 0 ;
       }
       break ;

       case MRI_float:{
         float thresh = im3d->vinfo->func_threshold * im3d->vinfo->func_thresh_top ;
         float thb=THBOT(thresh) , tht=THTOP(thresh) ; /* 08 Aug 2007 */
         float *ar_thr = MRI_FLOAT_PTR(im_thr) ;

         for( ii=0 ; ii < npix ; ii++ )
           if( ar_thr[ii] > thb && ar_thr[ii] < tht ) ar_ov[ii] = 0 ;
       }
       break ;
     }
   }

   /* delete the overlay if it contains nothing */

   for( ii=0 ; ii < npix ; ii++ ) if( ar_ov[ii] != 0 ) break ;
   if( ii == npix ) KILL_1MRI(im_ov) ;  /* no nonzero values --> no overlay */

   /** time to trot, Bwana **/

CLEANUP:
   if( im_thr != NULL && im_thr != im_fim ) mri_free( im_thr ) ;
   mri_free( im_fim ) ;

   /* 21 Mar 2005: Hollow out overlay? */

   if( AFNI_yesenv("AFNI_EDGIZE_OVERLAY") ) mri_edgize(im_ov) ;

   RETURN(im_ov) ;
}

/*-----------------------------------------------------------------------*/
/*! Make a functional overlay the new way (30 Jan 2003):
    - im_thr = threshold image (may be NULL)
    - thbot  = pixels with values in im_thr in range (thbot..thtop)
    - thtop  =  don't get overlay
    - im_fim = image to make overlay from (may not be NULL)
    - fimbot = pixel value to map to fimcolor[0]
    - fimtop = pixel value to map to fimcolor[NPANE_BIG-1]

  [08 Aug 2007 -- change 'thresh' to 'thbot,thtop']
-------------------------------------------------------------------------*/

MRI_IMAGE * AFNI_newfunc_overlay( MRI_IMAGE *im_thr , float thbot,float thtop,
                                  MRI_IMAGE *im_fim ,
                                  float fimbot, float fimtop, rgbyte *fimcolor )
{
   MRI_IMAGE *im_ov ;
   byte *ovar ;
   int ii , npix , zbot , jj ;
   float fac , val ;
   int reject_zero = !AFNI_yesenv("AFNI_OVERLAY_ZERO") ; /* 20 Apr 2005 */
   int dothr = (thbot < thtop) ;                         /* 08 Aug 2007 */

ENTRY("AFNI_newfunc_overlay") ;

   if( im_fim == NULL || fimbot >= fimtop || fimcolor == NULL ) RETURN(NULL) ;

   /* create output image */

   im_ov = mri_new_conforming( im_fim , MRI_rgb ) ;
   ovar  = MRI_RGB_PTR(im_ov) ;
   npix  = im_ov->nvox ;
   zbot  = (fimbot == 0.0) ;             /* no color for negative values? */
   fac   = NPANE_BIG / (fimtop-fimbot) ;

   /* load output image with colors */

   switch( im_fim->kind ){

      default:                             /* should not happen! */
        mri_free(im_ov) ;
      RETURN(NULL) ;

      case MRI_short:{
        short *ar_fim = MRI_SHORT_PTR(im_fim) ;

        for( ii=0 ; ii < npix ; ii++ ){
          if( ZREJ(ar_fim[ii]) )       continue ;
          if( zbot && ar_fim[ii] < 0 ) continue ;
          val = fac*(fimtop-ar_fim[ii]) ;
          if( val < 0.0 ) val = 0.0;
          else if (ar_fim[ii] < fimbot) val = NPANE_BIG-1; /* overflow guard */
          jj = (int)(val+0.49);
          if( jj >= NPANE_BIG ) jj = NPANE_BIG-1;
          ovar[3*ii  ] = fimcolor[jj].r ;
          ovar[3*ii+1] = fimcolor[jj].g ;
          ovar[3*ii+2] = fimcolor[jj].b ;
        }
      }
      break ;

      case MRI_byte:{
        byte *ar_fim = MRI_BYTE_PTR(im_fim) ;

        for( ii=0 ; ii < npix ; ii++ ){
          if( ZREJ(ar_fim[ii]) ) continue ;
          val = fac*(fimtop-ar_fim[ii]) ;
          if( val < 0.0 ) val = 0.0;
          else if (ar_fim[ii] < fimbot) val = NPANE_BIG-1;
          jj = (int)(val+0.49);
          if( jj >= NPANE_BIG ) jj = NPANE_BIG-1;
          ovar[3*ii  ] = fimcolor[jj].r ;
          ovar[3*ii+1] = fimcolor[jj].g ;
          ovar[3*ii+2] = fimcolor[jj].b ;
        }
      }
      break ;

      case MRI_float:{
        float *ar_fim = MRI_FLOAT_PTR(im_fim) ;

        for( ii=0 ; ii < npix ; ii++ ){
          if( ZREJ(ar_fim[ii]) )        continue ;
          if( zbot && ar_fim[ii] < 0.0 ) continue ;
          val = fac*(fimtop-ar_fim[ii]) ;
          if( val < 0.0 ) val = 0.0;
          else if (ar_fim[ii] < fimbot) val = NPANE_BIG-1;
          jj = (int)(val+0.49);
          if( jj >= NPANE_BIG ) jj = NPANE_BIG-1;
          ovar[3*ii  ] = fimcolor[jj].r ;
          ovar[3*ii+1] = fimcolor[jj].g ;
          ovar[3*ii+2] = fimcolor[jj].b ;
        }
      }
      break ;
   }

   /** now apply threshold, if any **/

   if( dothr && im_thr != NULL ){
     switch( im_thr->kind ){

       case MRI_short:{
         register float thb=thbot , tht=thtop ;
         register short *ar_thr = MRI_SHORT_PTR(im_thr) ;

         for( ii=0 ; ii < npix ; ii++ ){
           if( ar_thr[ii] > thb && ar_thr[ii] < tht )
             ovar[3*ii] = ovar[3*ii+1] = ovar[3*ii+2] = 0 ;
         }
       }
       break ;

       case MRI_byte:{
         register float thb=thbot , tht=thtop ;
         register byte *ar_thr = MRI_BYTE_PTR(im_thr) ;

         for( ii=0 ; ii < npix ; ii++ )  /* assuming thb <= 0 always */
           if( ar_thr[ii] < tht )
             ovar[3*ii] = ovar[3*ii+1] = ovar[3*ii+2] = 0 ;
       }
       break ;

       case MRI_float:{
         register float thb=thbot , tht=thtop ;
         register float *ar_thr = MRI_FLOAT_PTR(im_thr) ;

         for( ii=0 ; ii < npix ; ii++ )
           if( ar_thr[ii] > thb && ar_thr[ii] < tht )
             ovar[3*ii] = ovar[3*ii+1] = ovar[3*ii+2] = 0 ;
       }
       break ;
     }
   }

   RETURN(im_ov) ;
}

/*-----------------------------------------------------------------------
  Make an overlay from the TT atlas
   n = slice index; (ax_1,ax_2,ax_3) = slice orientation codes
   fov = functional overlay image previously computed
         if non-NULL, then fov will be overlaid, and will be returned
         if NULL, then a new image will be created and returned
  -- 25 Jul 2001 - RWCox
-------------------------------------------------------------------------*/

MRI_IMAGE * AFNI_ttatlas_overlay( Three_D_View *im3d ,
                                  int n , int ax_1 , int ax_2 , int ax_3 ,
                                  MRI_IMAGE *fov )
{
   THD_3dim_dataset *dseTT ;
   TTRR_params *ttp ;
   byte *b0 , *b1 , *brik, *val, *ovc , g_ov,a_ov,final_ov ;
   short *ovar ;
   MRI_IMAGE *ovim=NULL , *b0im , *b1im ;
   int gwin , fwin , nreg , ii,jj , nov ;

ENTRY("AFNI_ttatlas_overlay") ;

   /* setup and sanity checks */

STATUS("checking if have Atlas dataset") ;

   /* 01 Aug 2001: retrieve atlas based on z-axis size of underlay dataset */
#if 1
   dseTT = TT_retrieve_atlas_nz( DSET_NZ(im3d->anat_now) ) ;
                                 if( dseTT == NULL )      RETURN(NULL) ;
#else
   dseTT = TT_retrieve_atlas() ; if( dseTT == NULL )      RETURN(NULL) ;
#endif

   /* make sure Atlas and current dataset match in size */

STATUS("checking if Atlas and anat dataset match") ;

   if( DSET_NVOX(dseTT) != DSET_NVOX(im3d->anat_now) )    RETURN(NULL) ;

   /* make sure we are actually drawing something */

STATUS("checking if Atlas Colors is on") ;

   ttp = TTRR_get_params() ; if( ttp == NULL )            RETURN(NULL) ;

   /* at this time, hemisphere processing doesn't work in this function */

#if 0
   switch( ttp->hemi ){
      case TTRR_HEMI_LEFT:  hbot=HEMX+1 ; break ;
      case TTRR_HEMI_RIGHT: hbot= 0     ; break ;
      case TTRR_HEMI_BOTH:  hbot= 0     ; break ;
   }
#endif

   /* get slices from TTatlas dataset */

STATUS("loading Atlas bricks") ;

   DSET_load(dseTT) ;
   b0im = AFNI_slice_flip( n , 0 , RESAM_NN_TYPE , ax_1,ax_2,ax_3 , dseTT ) ;
   if( b0im == NULL )                                     RETURN(NULL) ;

   b1im = AFNI_slice_flip( n , 1 , RESAM_NN_TYPE , ax_1,ax_2,ax_3 , dseTT ) ;
   if( b1im == NULL ){ mri_free(b0im) ;                   RETURN(NULL) ; }

   /* make a new overlay image, or just operate on the old one */

   if( fov == NULL ){
STATUS("making new overlay for Atlas") ;
      ovim = mri_new_conforming( b0im , MRI_short ) ;   /* new overlay */
      ovar = MRI_SHORT_PTR(ovim) ;
      memset( ovar , 0 , ovim->nvox * sizeof(short) ) ;
   } else{
STATUS("re-using old overlay for Atlas") ;
      ovim = fov ;                                      /* old overlay */
      ovar = MRI_SHORT_PTR(ovim) ;
      if( ovim->nvox != b0im->nvox ){                     /* shouldn't */
         mri_free(b0im) ; mri_free(b1im) ; RETURN(NULL) ; /* happen!  */
      }
   }

   b0 = MRI_BYTE_PTR(b0im) ; b1 = MRI_BYTE_PTR(b1im) ;

   /* fwin => function 'wins' over Atlas */
   /* gwin => gyral Atlas brick 'wins' over 'area' Atlas brick */

   fwin = (ttp->meth == TTRR_METH_FGA) || (ttp->meth == TTRR_METH_FAG) ;
   gwin = (ttp->meth == TTRR_METH_FGA) || (ttp->meth == TTRR_METH_GAF) ;

   nreg = ttp->num ;    /* number of 'on' regions     */
   brik = ttp->ttbrik ; /* which sub-brick in atlas    */
   val  = ttp->ttval ;  /* which code in that sub-brick */
   ovc  = ttp->ttovc ;  /* which overlay color index   */

   /* loop over image voxels, find overlays from Atlas */

STATUS("doing Atlas overlay") ;

   for( nov=ii=0 ; ii < ovim->nvox ; ii++ ){

      if( ovar[ii] && fwin ) continue ; /* function wins */

      /* check Atlas 'on' regions for hits */

      g_ov = a_ov = 0 ;
      for( jj=0 ; (g_ov==0 || a_ov==0) && jj<nreg ; jj++ ){
              if( b0[ii] == val[jj] ) g_ov = ovc[jj] ;
         else if( b1[ii] == val[jj] ) a_ov = ovc[jj] ;
      }

      if( g_ov==0 && a_ov==0 ) continue ;  /* no hit */

      /* find the winner */

      if( g_ov && (gwin || a_ov==0) ) final_ov = g_ov ;
      else                            final_ov = a_ov ;

      ovar[ii] = final_ov ;  /* and the winner is ... */
      nov++ ;
   }

   mri_free(b0im) ; mri_free(b1im) ;  /* free at last */

if(PRINT_TRACING)
{ char str[256]; sprintf(str,"Atlas overlaid %d pixels",nov); STATUS(str); }

   RETURN(ovim) ;
}

/*---------------------------------------------------------------------*/

char * AFNI_resam_texter( MCW_arrowval *av , XtPointer junk )
{
   return RESAM_shortstr[av->ival] ;
}

/*---------------------------------------------------------------------*/

void AFNI_resam_av_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int reunder , redisplay ;

ENTRY("AFNI_resam_av_CB") ;

   /* sanity check */

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /* assign resampling type based on which arrowval, and redraw */

   if( av == im3d->vwid->dmode->func_resam_av ){
STATUS("set func_resam_mode") ;
      im3d->vinfo->func_resam_mode = av->ival ;
      if( im3d->b123_fim != NULL ){
         im3d->b123_fim->resam_code =
          im3d->b231_fim->resam_code =
           im3d->b312_fim->resam_code = im3d->vinfo->func_resam_mode ;
      }

   } else if( av == im3d->vwid->dmode->thr_resam_av ){  /* 09 Dec 1997 */
STATUS("set thr_resam_mode") ;
      im3d->vinfo->thr_resam_mode = av->ival ;
      if( im3d->b123_fim != NULL ){
         im3d->b123_fim->thr_resam_code =
          im3d->b231_fim->thr_resam_code =
           im3d->b312_fim->thr_resam_code = im3d->vinfo->thr_resam_mode ;
      }

   } else if( av == im3d->vwid->dmode->anat_resam_av ){
STATUS("set anat_resam_mode") ;
      im3d->vinfo->anat_resam_mode = av->ival ;
      im3d->b123_anat->resam_code =
       im3d->b231_anat->resam_code =
        im3d->b312_anat->resam_code = im3d->vinfo->anat_resam_mode ;
   }

   SHOW_AFNI_PAUSE ;
   im3d->vinfo->tempflag = 1 ;
   AFNI_modify_viewing( im3d , False ) ;  /* redisplay */
   SHOW_AFNI_READY ;
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------
   called when the underlay needs to be changed:
     -- when the underlay type is altered by a button press
     -- when a new set of images is to be displayed
     -- when the existing underlay has been altered in some hideous way
   if the calling widget "w" is NULL, then the calling routine is
   required to do the redrawing later
------------------------------------------------------------------------*/

void AFNI_underlay_CB( Widget w , XtPointer cd , XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int bval ;
   Boolean seq_exist ;

ENTRY("AFNI_underlay_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   if( w != NULL ) bval = AFNI_first_tog( LAST_UNDERLAY_TYPE+1 ,
                                          im3d->vwid->func->underlay_bbox->wbut ) ;
   else            bval = im3d->vinfo->underlay_type ;

   if( bval == im3d->vinfo->underlay_type && w != NULL ) EXRETURN ;  /* nothing */

   im3d->vinfo->underlay_type = bval ;

   /*----- set the bricks to use for the underlay images -----*/

   switch( im3d->vinfo->underlay_type ){

      default:
         XBell( im3d->dc->display , 100 ) ;  /* beep and fall through! */

      case UNDERLAY_ANAT:                    /* set underlay to anat */

STATUS("anatomy underlay") ;

         im3d->b123_ulay = im3d->b123_anat ;
         im3d->b231_ulay = im3d->b231_anat ;
         im3d->b312_ulay = im3d->b312_anat ;
      break ;

      case UNDERLAY_ALLFUNC:
         if( ISVALID_DSET(im3d->fim_now) ){

STATUS("functional underlay") ;

            im3d->b123_ulay = im3d->b123_fim ;
            im3d->b231_ulay = im3d->b231_fim ;
            im3d->b312_ulay = im3d->b312_fim ;
         } else {

STATUS("defaulted anatomy underlay") ;

            XBell( im3d->dc->display , 100 ) ;

            im3d->b123_ulay = im3d->b123_anat ;
            im3d->b231_ulay = im3d->b231_anat ;
            im3d->b312_ulay = im3d->b312_anat ;

            MCW_set_bbox( im3d->vwid->func->underlay_bbox , 1<<UNDERLAY_ANAT ) ;
         }
      break ;
   }

   /*--- May 1996: destroy useless graph windows ---*/

   im3d->ignore_seq_callbacks = AFNI_IGNORE_REDRAWS ;  /* 16 Feb 97 */

   if( !DSET_GRAPHABLE(im3d->b123_ulay->dset) || !BRICK_GRAPHABLE(im3d->b123_ulay) ){
      if( im3d->g123 != NULL ){
         drive_MCW_grapher( im3d->g123 , graDR_destroy , NULL ) ;
         im3d->g123 = NULL ;
      }
      XtSetSensitive( im3d->vwid->imag->graph_xyz_pb , False ) ;
   } else {
      XtSetSensitive( im3d->vwid->imag->graph_xyz_pb , True ) ;
   }

   if( !DSET_GRAPHABLE(im3d->b231_ulay->dset) || !BRICK_GRAPHABLE(im3d->b231_ulay) ){
      if( im3d->g231 != NULL ){
         drive_MCW_grapher( im3d->g231 , graDR_destroy , NULL ) ;
         im3d->g231 = NULL ;
      }
      XtSetSensitive( im3d->vwid->imag->graph_yzx_pb , False ) ;
   } else {
      XtSetSensitive( im3d->vwid->imag->graph_yzx_pb , True ) ;
   }

   if( !DSET_GRAPHABLE(im3d->b312_ulay->dset) || !BRICK_GRAPHABLE(im3d->b312_ulay) ){
      if( im3d->g312 != NULL ){
         drive_MCW_grapher( im3d->g312 , graDR_destroy , NULL ) ;
         im3d->g312 = NULL ;
      }
      XtSetSensitive( im3d->vwid->imag->graph_zxy_pb , False ) ;
   } else {
      XtSetSensitive( im3d->vwid->imag->graph_zxy_pb , True ) ;
   }

   /** 05 Mar 1997: disable viewers if x or y dimension is 1 pixel **/

   if( !BRICK_DRAWABLE(im3d->b123_ulay) ){
      if( im3d->s123 != NULL ){
         drive_MCW_imseq( im3d->s123 , isqDR_destroy , NULL ) ;
         im3d->s123 = NULL ;
      }
      XtSetSensitive( im3d->vwid->imag->image_xyz_pb , False ) ;
   } else {
      XtSetSensitive( im3d->vwid->imag->image_xyz_pb , True ) ;
   }

   if( !BRICK_DRAWABLE(im3d->b231_ulay) ){
      if( im3d->s231 != NULL ){
         drive_MCW_imseq( im3d->s231 , isqDR_destroy , NULL ) ;
         im3d->s231 = NULL ;
      }
      XtSetSensitive( im3d->vwid->imag->image_yzx_pb , False ) ;
   } else {
      XtSetSensitive( im3d->vwid->imag->image_yzx_pb , True ) ;
   }

   if( !BRICK_DRAWABLE(im3d->b312_ulay) ){
      if( im3d->s312 != NULL ){
         drive_MCW_imseq( im3d->s312 , isqDR_destroy , NULL ) ;
         im3d->s312 = NULL ;
      }
      XtSetSensitive( im3d->vwid->imag->image_zxy_pb , False ) ;
   } else {
      XtSetSensitive( im3d->vwid->imag->image_zxy_pb , True ) ;
   }

   im3d->ignore_seq_callbacks = AFNI_IGNORE_NOTHING ;  /* 16 Feb 97 */

   /*--- attach any changes to open image displays ---*/

   seq_exist = (im3d->s123 != NULL) || (im3d->s231 != NULL) || (im3d->s312 != NULL) ||
               (im3d->g123 != NULL) || (im3d->g231 != NULL) || (im3d->g312 != NULL)   ;

   /* set the title for all windows */

   AFNI_set_window_titles( im3d ) ;

   if( seq_exist ){

      im3d->ignore_seq_callbacks = AFNI_IGNORE_EVERYTHING ;

      if( im3d->s123 != NULL )
         drive_MCW_imseq( im3d->s123 , isqDR_newseq ,
                                       (XtPointer) im3d->b123_ulay ) ;

      if( im3d->s231 != NULL )
         drive_MCW_imseq( im3d->s231 , isqDR_newseq ,
                                       (XtPointer) im3d->b231_ulay ) ;

      if( im3d->s312 != NULL )
         drive_MCW_imseq( im3d->s312 , isqDR_newseq ,
                                       (XtPointer) im3d->b312_ulay ) ;

      if( im3d->g123 != NULL )
         drive_MCW_grapher( im3d->g123 , graDR_newdata ,
                                         (XtPointer) im3d->b123_ulay ) ;

      if( im3d->g231 != NULL )
         drive_MCW_grapher( im3d->g231 , graDR_newdata ,
                                         (XtPointer) im3d->b231_ulay ) ;

      if( im3d->g312 != NULL )
         drive_MCW_grapher( im3d->g312 , graDR_newdata ,
                                         (XtPointer) im3d->b312_ulay ) ;

      im3d->ignore_seq_callbacks = AFNI_IGNORE_REDRAWS ;

      /** July 1996: get the sequences to send their montaging status **/

      if( im3d->s123 != NULL )
         drive_MCW_imseq( im3d->s123 , isqDR_sendmontage , NULL ) ;
      else
         CLEAR_MONTAGE( im3d , im3d->b123_ulay ) ;

      if( im3d->s231 != NULL )
         drive_MCW_imseq( im3d->s231 , isqDR_sendmontage , NULL ) ;
      else
         CLEAR_MONTAGE( im3d , im3d->b231_ulay ) ;

      if( im3d->s312 != NULL )
         drive_MCW_imseq( im3d->s312 , isqDR_sendmontage , NULL ) ;
      else
         CLEAR_MONTAGE( im3d , im3d->b312_ulay ) ;

      im3d->ignore_seq_callbacks = AFNI_IGNORE_NOTHING ;

      if( w != NULL ){            /* a real callback */
          SHOW_AFNI_PAUSE ;
          AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_ALL ) ;
          SHOW_AFNI_READY ;
      }
   }

   /* Feb 1998: if a receiver is open
                send it a message that something has altered */

   AFNI_process_alteration(im3d) ;

   HINTIZE_pbar(im3d) ; /* 15 Aug 2001 */
   FIX_SCALE_SIZE(im3d) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

char * AFNI_controller_label( Three_D_View *im3d )
{
   static char clabel[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;
   static char str[8] ;
   int ic ;

   ic = AFNI_controller_index( im3d ) ;
   if( ic < 0 || ic > 26 ) strcpy (str,"    ") ;  /* shouldn't happen */
   else                    sprintf(str,"[%c] ",clabel[ic]) ;
   return str ;
}

/*--------------------------------------------------------------------
  Set the titles in all windows
----------------------------------------------------------------------*/

#undef  USE_TITLE2
#define USE_TITLE2(ds)   ( ISVALID_DSET(ds)                     &&  \
                           AFNI_yesenv("AFNI_TITLE_LABEL2")     &&  \
                           *((ds)->label2) != '\0'              &&  \
                           strcmp( (ds)->label2 , "zyxt" ) != 0   )

void AFNI_set_window_titles( Three_D_View *im3d )
{
   Boolean redo_title ;
   char ttl[THD_MAX_NAME] , nam[THD_MAX_NAME] ;
   char *tnam , *clab ; int ilab ;
   char signum ; /* 08 Aug 2007 */

ENTRY("AFNI_set_window_titles") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   clab = AFNI_controller_label(im3d) ;
   switch( im3d->vinfo->thr_sign ){
     default: ilab = 2 ; break ;
     case 1:  ilab = 3 ; clab[3] = '+' ; break ;
     case 2:  ilab = 3 ; clab[3] = '-' ; break ;
   }
   switch( im3d->vinfo->underlay_type ){  /* 08 May 2008 */
     default:               clab[++ilab] = 'u' ; break ;
     case UNDERLAY_ALLFUNC: clab[++ilab] = 'o' ; break ;
   }
   clab[++ilab] = ' ' ; clab[++ilab] = '\0' ;

   if( im3d->anat_wod_flag )
     sprintf(ttl , "{warp}%s%s: " , clab,GLOBAL_argopt.title_name) ;
   else
     sprintf(ttl , "%s%s: " , clab,GLOBAL_argopt.title_name) ;

   if( USE_TITLE2(im3d->anat_now) ){
     strcat( ttl , im3d->anat_now->label2 ) ;
   } else {
     strcpy( nam , im3d->anat_now->dblk->diskptr->directory_name ) ;
     strcat( nam , im3d->anat_now->dblk->diskptr->filecode ) ;
     tnam = THD_trailname(nam,SESSTRAIL+1) ;
     strcat( ttl , tnam ) ;
   }

   if( ISVALID_3DIM_DATASET(im3d->fim_now) ){
     strcat( ttl , " & " ) ;
     if( USE_TITLE2(im3d->fim_now) ){
       strcat( ttl , im3d->fim_now->label2 ) ;
     } else {
       strcat( ttl , im3d->fim_now->dblk->diskptr->filecode ) ;
     }
   }

   redo_title = (Boolean) (strcmp(ttl,im3d->window_title) != 0 ) ;
   if( redo_title ){
     strcpy( im3d->window_title , ttl ) ;
     XtVaSetValues( im3d->vwid->top_shell , XmNtitle , ttl , NULL ) ;

     if( im3d->s123 != NULL )
       drive_MCW_imseq( im3d->s123 , isqDR_title , (XtPointer) ttl ) ;

     if( im3d->s231 != NULL )
       drive_MCW_imseq( im3d->s231 , isqDR_title , (XtPointer) ttl ) ;

     if( im3d->s312 != NULL )
       drive_MCW_imseq( im3d->s312 , isqDR_title , (XtPointer) ttl ) ;

     if( im3d->g123 != NULL )
       drive_MCW_grapher( im3d->g123 , graDR_title , (XtPointer) ttl ) ;

     if( im3d->g231 != NULL )
       drive_MCW_grapher( im3d->g231 , graDR_title , (XtPointer) ttl ) ;

     if( im3d->g312 != NULL )
       drive_MCW_grapher( im3d->g312 , graDR_title , (XtPointer) ttl ) ;
   }

   EXRETURN ;
}

/*--------------------------------------------------------------------*/
/*! Determine if dset is in the AFNI global session - 21 Dec 2001.
----------------------------------------------------------------------*/

int DSET_in_global_session( THD_3dim_dataset *dset )
{
   THD_slist_find find ;

   if( !ISVALID_DSET(dset) ) return 0 ;

   find = THD_dset_in_session( FIND_IDCODE ,
                               &(dset->idcode) ,
                               GLOBAL_library.session ) ;

   return (find.dset != NULL) ;
}

/*--------------------------------------------------------------------
   used to select between sessions and datasets
----------------------------------------------------------------------*/

/** labels for the chooser window **/

static char *dset_choice[] = { "Session" , "Underlay" , "Overlay" , "Dataset" } ;

/** max size of strings in the list **/

#define STRLIST_SIZE (THD_MAX_PREFIX+64)

void AFNI_choose_dataset_CB( Widget w , XtPointer cd , XtPointer cb )
{
   static char *strlist[THD_MAX_CHOICES] ;  /* strings to choose between */
   static int first_call = 1 ;              /* initialization flag */

   int num_str , ii , init_str , vv , jj ;
   char *label ;
   Widget wpar ;
   Three_D_View *im3d = (Three_D_View *) cd ;
   int llen , ltop ;
   int browse_select = 0 ;
   int is_other = 0 ;       /* 18 Dec 2007 */
   void (*cbfun)(Widget,XtPointer,MCW_choose_cbs *)=AFNI_finalize_dataset_CB;

ENTRY("AFNI_choose_dataset_CB") ;

   /*--- initialize ---*/

   if( ! IM3D_VALID(im3d) || w == (Widget)NULL ) EXRETURN ;

   if( GLOBAL_library.have_dummy_dataset ){  /* 26 Feb 2007: read session? */
     BEEPIT ;
     if( w == im3d->vwid->view->choose_sess_pb ||
         w == im3d->vwid->view->popchoose_sess_pb )
       AFNI_read_sess_CB(im3d->vwid->dmode->read_sess_pb,(XtPointer)im3d,NULL);
     EXRETURN ;
   }

#if 0
   if( AFNI_splash_isopen() == 1 ){ BEEPIT ; EXRETURN ; }
#endif

   /* how about a rescan ? ZSS - Fur Greg Detre */

   if( AFNI_yesenv("AFNI_RESCAN_AT_SWITCH") &&
      !(w == im3d->vwid->view->choose_sess_pb ||
        w == im3d->vwid->view->popchoose_sess_pb) ){

     STATUS("rescanning, per AFNI_RESCAN_AT_SWITCH") ;
     AFNI_rescan_CB( w , (XtPointer)im3d , NULL ) ;
   }

   if( first_call ){
     for( ii=0 ; ii < THD_MAX_CHOICES ; ii++ )
       strlist[ii] = (char*)XtMalloc( sizeof(char) * (STRLIST_SIZE+1) ) ;
     first_call = 0 ;
   }

   /*--- make a list of session names ---*/

   if( w == im3d->vwid->view->choose_sess_pb ||
       w == im3d->vwid->view->popchoose_sess_pb ){

      wpar    = im3d->vwid->view->choose_sess_pb ;
      num_str = GLOBAL_library.sslist->num_sess ;
      if( num_str < 1 ) EXRETURN ;

      for( ii=0 ; ii < num_str ; ii++ ){
         MCW_strncpy( strlist[ii] ,
                      GLOBAL_library.sslist->ssar[ii]->lastname ,
                      STRLIST_SIZE ) ;
      }

      init_str = im3d->vinfo->sess_num ;
      label    = dset_choice[0] ;

   /*--- make a list of anatomy names ---*/

   } else if( w == im3d->vwid->view->choose_anat_pb ||
              w == im3d->vwid->view->popchoose_anat_pb ){

      wpar    = im3d->vwid->view->choose_anat_pb ;
      num_str = im3d->ss_now->num_dsset ;
      if( num_str < 1 ) EXRETURN ;

      if( AFNI_yesenv("AFNI_DATASET_BROWSE") ) browse_select = 1 ;

      ltop = 4 ;
      for( ii=0 ; ii < num_str ; ii++ ){
         THD_report_obliquity(im3d->ss_now->dsset[ii][0]) ;  /* 20 Dec 2007 */

         for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ )
            if( ISVALID_3DIM_DATASET(im3d->ss_now->dsset[ii][vv]) ) break ;

         if( vv <= LAST_VIEW_TYPE ){
            llen = strlen( im3d->ss_now->dsset[ii][vv]->dblk->diskptr->prefix ) ;
            ltop = MAX( ltop , llen ) ;
         }
      }
      ltop = MIN(ltop,STRLIST_SIZE-24) ;  /* 06 Aug 2002 */

      for( ii=0 ; ii < num_str ; ii++ ){
         for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ )
            if( ISVALID_3DIM_DATASET(im3d->ss_now->dsset[ii][vv]) ) break ;

         if( vv <= LAST_VIEW_TYPE ){
            sprintf( strlist[ii] , "%-*s" ,
                     ltop,im3d->ss_now->dsset[ii][vv]->dblk->diskptr->prefix ) ;

            strcat( strlist[ii] , " [" ) ;
            strcat( strlist[ii] , DSET_PREFIXSTR(im3d->ss_now->dsset[ii][vv]) ) ;

            if( DSET_NUM_TIMES(im3d->ss_now->dsset[ii][vv]) > 1 ){
               int ll = strlen(strlist[ii]) ;
               sprintf( strlist[ii]+ll , ":3D+t:%d]" ,
                        DSET_NUM_TIMES(im3d->ss_now->dsset[ii][vv]) ) ;
            } else if( ISBUCKET(im3d->ss_now->dsset[ii][vv]) ){
               int ll = strlen(strlist[ii]) ;
               sprintf( strlist[ii]+ll , ":%d]" ,
                        DSET_NVALS(im3d->ss_now->dsset[ii][vv]) ) ;
            } else {
               strcat( strlist[ii] , "]" ) ;
            }

            if( DSET_GRAPHABLE(im3d->ss_now->dsset[ii][vv]) )
               strcat( strlist[ii] , "*" ) ;

            if( DSET_COMPRESSED(im3d->ss_now->dsset[ii][vv]) )
               strcat( strlist[ii] , "z" ) ;

            /* 20 Dec 2001: mark if this is a global dataset */

            if( DSET_in_global_session(im3d->ss_now->dsset[ii][vv]) )
              strcat( strlist[ii] , "G" ) ;

         } else {
#if 0
THD_3dim_dataset *qset ;
for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ){
 qset = im3d->ss_now->dsset[ii][vv] ;
 if( qset != NULL ){
  INFO_message("BAD: type=%d view_type=%d ibk=%d bkt=%d",
               qset->type , qset->view_type , qset->dblk != NULL , qset->dblk->type ) ;
 }
}
#endif
            MCW_strncpy( strlist[ii] , "??*BAD*??" , THD_MAX_PREFIX ) ;
         }
      }

      init_str = im3d->vinfo->anat_num ;
      label    = dset_choice[1] ;

   /*--- make a list of function names ---*/

   } else {
      int nn=0 , ndset=0 ; THD_3dim_dataset **dset_list=NULL , *dset ;

      is_other = !( w == im3d->vwid->view->choose_func_pb ||
                    w == im3d->vwid->view->popchoose_func_pb ) ;

      num_str = im3d->ss_now->num_dsset ;
      if( num_str < 1 ) EXRETURN ;

      if( is_other ){
        AFNI_dataset_choose_stuff *cs = (AFNI_dataset_choose_stuff *)cb ;
        if( cs == NULL ) EXRETURN ;
        ndset = cs->ndset ; if( ndset < 1 ) EXRETURN ;
        dset_list = cs->dset ; if( dset_list == NULL ) EXRETURN ;
        cbfun = cs->cb ; if( cbfun == NULL ) EXRETURN ;
        wpar = w ;
      } else {
        if( AFNI_yesenv("AFNI_DATASET_BROWSE") ) browse_select = 1 ;
        wpar = im3d->vwid->view->choose_func_pb ;
        ndset = num_str ;
      }

      ltop = 4 ;
      for( ii=0 ; ii < ndset ; ii++ ){
        if( is_other ){
          dset = dset_list[ii] ;
        } else {
          for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ){
            dset = im3d->ss_now->dsset[ii][vv]; if( ISVALID_DSET(dset) ) break;
          }
        }
        if( ISVALID_DSET(dset) ){
          llen = strlen( dset->dblk->diskptr->prefix ) ;
          ltop = MAX( ltop , llen ) ;
        }
      }
      ltop = MIN(ltop,STRLIST_SIZE-24) ;  /* 06 Aug 2002 */

      for( ii=0 ; ii < ndset ; ii++ ){

         if( is_other ){
           dset = dset_list[ii] ; if( !ISVALID_DSET(dset) ) continue ;
         } else {
           for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ){
             dset = im3d->ss_now->dsset[ii][vv]; if( ISVALID_DSET(dset) ) break;
           }
         }

         if( ISVALID_DSET(dset) ){
           sprintf( strlist[nn] , "%-*s" ,
                    ltop , dset->dblk->diskptr->prefix ) ;

           strcat( strlist[nn] , " [" ) ;
           strcat( strlist[nn] , DSET_PREFIXSTR(dset) ) ;

           if( DSET_NUM_TIMES(dset) > 1 ){
             int ll = strlen(strlist[nn]) ;
             sprintf( strlist[nn]+ll , ":3D+t:%d]" , DSET_NUM_TIMES(dset) ) ;
           } else if( ISBUCKET(dset) ){
             int ll = strlen(strlist[nn]) ;
             sprintf( strlist[nn]+ll , ":%d]" , DSET_NVALS(dset) ) ;
           } else {
             strcat( strlist[nn] , "]" ) ;
           }

           if( DSET_COMPRESSED(dset) ) strcat( strlist[nn] , "z" ) ;

           /* 20 Dec 2001: mark if this is a global dataset */

           if( DSET_in_global_session(dset) ) strcat( strlist[nn] , "G" ) ;

         } else { /* should never happen */
           MCW_strncpy( strlist[nn] , "**?BAD?**" , THD_MAX_PREFIX ) ;
         }

         nn++ ;
      }

      if( nn < 1 ) EXRETURN ;

      if( is_other ){
        init_str = 0 ;
        label    = dset_choice[3] ;
      } else {
        init_str = im3d->vinfo->func_num ;
        label    = dset_choice[2] ;
      }
      num_str = nn ;

   }

   /*--- call the chooser ---*/

   MCW_set_browse_select( browse_select ) ;

   MCW_choose_strlist( wpar , label , num_str , init_str , strlist ,
                       cbfun , (XtPointer)im3d ) ;

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void AFNI_finalize_dataset_CB( Widget wcall ,
                               XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   int old_sess , old_anat , old_func , old_view ;
   int new_sess=-1 , new_anat=-1 , new_func=-1 , new_view=-1 ;
   int ii , vv , ff ;
   THD_session *ss_new ;

ENTRY("AFNI_finalize_dataset_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   old_sess = im3d->vinfo->sess_num ;     /* record current status */
   old_anat = im3d->vinfo->anat_num ;
   old_func = im3d->vinfo->func_num ;
   old_view = im3d->vinfo->view_type ;

   /*--- switch sessions ---*/

   if( wcall == im3d->vwid->view->choose_sess_pb ){

      new_sess = cbs->ival ;
      if( new_sess < 0 || new_sess >= GLOBAL_library.sslist->num_sess ){
         XBell( im3d->dc->display , 100 ) ; EXRETURN ;  /* bad! */
      }

      ss_new = GLOBAL_library.sslist->ssar[new_sess] ;

      /* find an anat in new session to match current anat */

      if( ISVALID_3DIM_DATASET(ss_new->dsset[old_anat][old_view]) ){  /* are OK */
        new_anat = old_anat ;
      } else {
        for( ii=0 ; ii < ss_new->num_dsset ; ii++ )
          if( ISVALID_3DIM_DATASET(ss_new->dsset[ii][old_view]) ){
             new_anat = ii ; break ;
          }
      }
      if( new_anat < 0 ) new_anat = 0 ;  /* use 1st if no match */

      /* find a view to fit this chosen anat */

      if( ISVALID_3DIM_DATASET(ss_new->dsset[new_anat][old_view]) ){ /* are OK */
         new_view = old_view ;
      } else {
         for( vv=old_view-1 ; vv >= FIRST_VIEW_TYPE ; vv-- )  /* look below */
            if( ISVALID_3DIM_DATASET(ss_new->dsset[new_anat][vv]) ) break ;

         if( vv >= FIRST_VIEW_TYPE ){  /* found it below */
            new_view = vv ;
         } else {                      /* look above */
            for( vv=old_view+1 ; vv <= LAST_VIEW_TYPE ; vv++ )
               if( ISVALID_3DIM_DATASET(ss_new->dsset[new_anat][vv]) ) break ;

            if( vv <= LAST_VIEW_TYPE ){  /* found it above */
               new_view = vv ;
            } else {
               XBell(im3d->dc->display,100) ; EXRETURN ;  /* bad news */
            }
         }
      }

      /* find a func in new session that fits the new view */

#define FINDAFUNC
#ifdef FINDAFUNC
      if( ISVALID_3DIM_DATASET(ss_new->dsset[old_func][new_view]) ){  /* are OK */

         new_func = old_func ;
      } else {
         for( ff=0 ; ff < ss_new->num_dsset ; ff++ )  /* search */
            if( ISVALID_3DIM_DATASET(ss_new->dsset[ff][new_view]) ) break ;

         if( ff < ss_new->num_dsset ) new_func = ff ;  /* found one */
      }
      if( new_func < 0 ) new_func = 0 ;  /* no match */
#else
      new_func = old_func ;
#endif

   /*--- switch anatomy ---*/

   } else if( wcall == im3d->vwid->view->choose_anat_pb ){

      new_sess = old_sess ;
      ss_new   = GLOBAL_library.sslist->ssar[new_sess] ;

      new_anat = cbs->ival ;
      if( new_anat < 0 || new_anat >= ss_new->num_dsset ){
         XBell( im3d->dc->display , 100 ) ; EXRETURN ;  /* bad! */
      }

      /* find a view to fit this chosen anat */

      if( ISVALID_3DIM_DATASET(ss_new->dsset[new_anat][old_view]) ){ /* are OK */
         new_view = old_view ;
      } else {
         for( vv=old_view-1 ; vv >= FIRST_VIEW_TYPE ; vv-- )  /* look below */
            if( ISVALID_3DIM_DATASET(ss_new->dsset[new_anat][vv]) ) break ;

         if( vv >= FIRST_VIEW_TYPE ){  /* found it below */
            new_view = vv ;
         } else {                      /* look above */
            for( vv=old_view+1 ; vv <= LAST_VIEW_TYPE ; vv++ )
               if( ISVALID_3DIM_DATASET(ss_new->dsset[new_anat][vv]) ) break ;

            if( vv <= LAST_VIEW_TYPE ){  /* found it above */
               new_view = vv ;
            } else {
               XBell(im3d->dc->display,100) ; EXRETURN ;  /* bad news */
            }
         }
      }

      /* find a func to match this view */

#ifdef FINDAFUNC
      if( ISVALID_3DIM_DATASET(ss_new->dsset[old_func][new_view]) ){ /* are OK */

         new_func = old_func ;
      } else {
         for( ff=0 ; ff < ss_new->num_dsset ; ff++ )  /* search */
            if( ISVALID_3DIM_DATASET(ss_new->dsset[ff][new_view]) ) break ;

         if( ff < ss_new->num_dsset ) new_func = ff ;  /* found one */
      }
      if( new_func < 0 ) new_func = 0 ;  /* no match */
#else
      new_func = old_func ;
#endif

   /*--- switch function ---*/

   } else if( wcall == im3d->vwid->view->choose_func_pb ){

      new_sess = old_sess ;
      ss_new   = GLOBAL_library.sslist->ssar[new_sess] ;

      new_func = cbs->ival ;
      if( new_func < 0 || new_func >= ss_new->num_dsset ){
         XBell( im3d->dc->display , 100 ) ; EXRETURN ;  /* bad! */
      }

      /* find a view to fit this chosen func */

      if( ISVALID_3DIM_DATASET(ss_new->dsset[new_func][old_view]) ){ /* are OK */
         new_view = old_view ;
      } else {
         for( vv=old_view-1 ; vv >= FIRST_VIEW_TYPE ; vv-- )  /* look below */
            if( ISVALID_3DIM_DATASET(ss_new->dsset[new_func][vv]) ) break ;

         if( vv >= FIRST_VIEW_TYPE ){  /* found it below */
            new_view = vv ;
         } else {                      /* look above */
            for( vv=old_view+1 ; vv <= LAST_VIEW_TYPE ; vv++ )
               if( ISVALID_3DIM_DATASET(ss_new->dsset[new_func][vv]) ) break ;

            if( vv <= LAST_VIEW_TYPE ){  /* found it above */
               new_view = vv ;
            } else {
               XBell(im3d->dc->display,100) ; EXRETURN ;  /* bad news */
            }
         }
      }

      /* find an anat to go with the new view (this is NOT optional) */

      if( ISVALID_3DIM_DATASET(ss_new->dsset[old_anat][new_view]) ){  /* are OK */

         new_anat = old_anat ;
      } else {
         for( ff=0 ; ff < ss_new->num_dsset ; ff++ )  /* search */
            if( ISVALID_3DIM_DATASET(ss_new->dsset[ff][new_view]) ) break ;

         if( ff < ss_new->num_dsset ) new_anat = ff ;  /* found one */
      }
      if( new_anat < 0 ){
         XBell( im3d->dc->display , 100 ) ; EXRETURN ;  /* bad! */
      }

      /* 03 Aug 2007: turn 'See Overlay' on? */

      if( !im3d->vinfo->func_visible && im3d->vinfo->func_visible_count == 0 ){
        MCW_set_bbox( im3d->vwid->view->see_func_bbox , 1 ) ;
        AFNI_see_func_CB( NULL , im3d , NULL ) ;
      }

   /*--- switch to Hell? ---*/

   } else {
      XBell( im3d->dc->display , 100 ) ; EXRETURN ;  /* bad! */
   }

   /*--- make sure all values are set OK-ly ---*/

   if( new_view < 0 || new_sess < 0 || new_anat < 0 || new_func < 0 ){
     ERROR_message("Something bad happened when trying to 'Switch'\a") ;
     EXRETURN ;  /* bad! */
   }

   /*- beep & flash viewing control box if view type changes -*/

   if( old_view != new_view ){
     static int first=1 ;
     MCW_set_bbox( im3d->vwid->view->view_bbox , 1 << new_view ) ;
     UNCLUSTERIZE(im3d) ;  /* 14 Feb 2008 */

     /* this stuff is for Adam Thomas -- 18 Oct 2006 */

     WARNING_message("Forced switch from '%s' to '%s'\a",
                     VIEW_typestr[old_view] , VIEW_typestr[new_view] ) ;
     if( first && wcall != NULL ){
       char str[256] ; first = 0 ;
       sprintf(str," \nForced switch from\n  '%s'\nto\n  '%s'\n ",
                   VIEW_typestr[old_view] , VIEW_typestr[new_view] ) ;
       (void) MCW_popup_message( wcall, str, MCW_USER_KILL | MCW_TIMER_KILL ) ;
     }

     if( wcall != NULL && !AFNI_noenv("AFNI_FLASH_VIEWSWITCH") ){
       for( ii=0 ; ii < 6 ; ii++ ){
         MCW_invert_widget(im3d->vwid->view->view_bbox->wframe ); RWC_sleep(32);
         MCW_invert_widget(im3d->vwid->view->view_bbox->wrowcol); RWC_sleep(32);
         MCW_invert_widget(wcall) ;
         MCW_invert_widget(im3d->vwid->view->view_bbox->wframe ); RWC_sleep(32);
         MCW_invert_widget(im3d->vwid->view->view_bbox->wrowcol); RWC_sleep(32);
         MCW_invert_widget(wcall) ;
       }
     }
   }

   /*----- actually do the switch -----*/

   if( im3d->vinfo->sess_num != new_sess )  /* disable FIMage in a new session */
      im3d->fimdata->fimdset = NULL ;

   im3d->vinfo->view_type = new_view ;
   im3d->vinfo->sess_num  = new_sess ;
   im3d->vinfo->anat_num  = new_anat ;
   im3d->vinfo->func_num  = new_func ;

   SHOW_AFNI_PAUSE ;
   AFNI_initialize_view( im3d->anat_now , im3d ) ;
   SHOW_AFNI_READY ;
   FIX_SCALE_SIZE(im3d) ;

   if( old_view != new_view ){            /* ending flash */
     XBell( im3d->dc->display , 100 ) ;
     for( ii=0 ; ii < 8 ; ii++ ){
       MCW_invert_widget( im3d->vwid->view->view_bbox->wframe ); RWC_sleep(16);
       MCW_invert_widget( im3d->vwid->view->view_bbox->wrowcol); RWC_sleep(16);
       MCW_invert_widget(wcall) ;
       MCW_invert_widget( im3d->vwid->view->view_bbox->wframe ); RWC_sleep(16);
       MCW_invert_widget( im3d->vwid->view->view_bbox->wrowcol); RWC_sleep(16);
       MCW_invert_widget(wcall) ;
     }
   }

   if( wcall == im3d->vwid->view->choose_func_pb &&
       AFNI_yesenv("AFNI_THRESH_AUTO")              ){  /* 05 Mar 2007 */

     float new_thresh = AFNI_get_autothresh(im3d) ;
     if( new_thresh > 0.0f ) AFNI_set_threshold(im3d,new_thresh) ;
   }

   /* check obliquity of overlay and underlay */
   /* pop up warning if necessary */

   if(wcall == im3d->vwid->view->choose_func_pb)
     AFNI_check_obliquity(wcall, ss_new->dsset[new_func][0]);
   else
     AFNI_check_obliquity(wcall, ss_new->dsset[new_anat][0]);

   EXRETURN ;
}


/*-----------------------------------------------------------*/
/* check dataset for obliquity and pop-up warning if oblique */

void AFNI_check_obliquity(Widget w, THD_3dim_dataset *dset)
{
   double angle;
   char str[1024];

   ENTRY("AFNI_check_obliquity");
   if( !ISVALID_DSET(dset) ) EXRETURN ;

   if(AFNI_yesenv("AFNI_NO_OBLIQUE_WARNING")) EXRETURN;

   THD_check_oblique_field(dset);

   angle = THD_compute_oblique_angle(dset->daxes->ijk_to_dicom_real, 0);
   if(angle == 0.0) EXRETURN ;

   sprintf( str,
      " You have selected an oblique dataset (%s).\n"
      "  If you are performing spatial transformations on an oblique dset, \n"
      "  or viewing/combining it with volumes of differing obliquity,\n"
      "  you should consider running: \n"
      "     3dWarp -deoblique \n"
      "  on this and other oblique datasets in the same session.\n",
      DSET_BRIKNAME(dset));

   (void) MCW_popup_message( w , str, MCW_USER_KILL | MCW_TIMER_KILL ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------
   Routines to close and open a file selection
   dialog associated with the im3d viewer.
------------------------------------------------------------------------*/

void AFNI_close_file_dialog_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_close_file_dialog") ;

   if( im3d->vwid->file_dialog != NULL )
     RWC_XtPopdown( im3d->vwid->file_dialog ) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------
   After this is called, the calling routine must set the callbacks,
   window title, etc., appropriately.
------------------------------------------------------------------------*/

void AFNI_make_file_dialog( Three_D_View *im3d )
{
   Widget www ;

ENTRY("AFNI_make_file_dialog") ;

   /*** make a new dialog? ***/

   if( im3d->vwid->file_dialog == NULL ){
      XmString oklabel , cancellabel ;

STATUS("creating new dialog") ;

      im3d->vwid->file_dialog =
         XtVaCreatePopupShell(
           "menu" , xmDialogShellWidgetClass , im3d->vwid->top_shell ,
              XmNtitle , "GPL AFNI" ,
              XmNdeleteResponse , XmDO_NOTHING ,
              XmNinitialResourcesPersistent , False ,
              XmNkeyboardFocusPolicy , XmEXPLICIT ,
           NULL ) ;

      XmAddWMProtocolCallback(           /* make "Close" window menu work */
           im3d->vwid->file_dialog ,
           XmInternAtom( im3d->dc->display , "WM_DELETE_WINDOW" , False ) ,
           AFNI_close_file_dialog_CB , (XtPointer) im3d ) ;

      /** change button labels to conform to other AFNI dialogs **/

      oklabel     = XmStringCreateLtoR( "Set"  , XmFONTLIST_DEFAULT_TAG ) ;
      cancellabel = XmStringCreateLtoR( "Quit" , XmFONTLIST_DEFAULT_TAG ) ;

      im3d->vwid->file_sbox =
         XtVaCreateManagedWidget(
           "menu" , xmFileSelectionBoxWidgetClass , im3d->vwid->file_dialog ,
              XmNfileTypeMask      , XmFILE_ANY_TYPE ,
              XmNcancelLabelString , cancellabel ,
              XmNokLabelString     , oklabel ,
              XmNtraversalOn       , True ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

      XmStringFree(oklabel) ; XmStringFree(cancellabel) ;

      im3d->vwid->file_cb = NULL ;
      im3d->vwid->file_cd = NULL ;

   } else if( im3d->vwid->file_cb != NULL ){
      Widget www ;

STATUS("re-initializing old dialog") ;

      /*** re-initialize an old dialog to have no callbacks ***/

      XtRemoveCallback( im3d->vwid->file_sbox , XmNokCallback ,
                        im3d->vwid->file_cb , im3d->vwid->file_cd ) ;

      XtRemoveCallback( im3d->vwid->file_sbox , XmNcancelCallback ,
                        im3d->vwid->file_cb , im3d->vwid->file_cd ) ;

      XtRemoveCallback( im3d->vwid->file_sbox , XmNhelpCallback ,
                        im3d->vwid->file_cb , im3d->vwid->file_cd ) ;

      XtVaSetValues( im3d->vwid->file_sbox , XmNpattern,NULL , NULL ) ;

      im3d->vwid->file_cb = NULL ;
      im3d->vwid->file_cd = NULL ;

      XtVaSetValues(im3d->vwid->file_sbox,XmNfileTypeMask,XmFILE_ANY_TYPE,NULL);
   }

#if 0  /* doesn't work, because the button is a gadget, not a widget */
   www = XtNameToWidget( im3d->vwid->file_sbox , "OK" ) ;
   if( www != NULL )
     MCW_set_widget_bg( www , MCW_hotcolor(www) , 0 ) ;
#endif

   EXRETURN ;
}

/*----------------------------------------------------------------
  Start getting ready to read a new session in.  This
  is the CB for the "Read Sess" button.  We'll get a filename
  from the user, and process it in another routine.
------------------------------------------------------------------*/

void AFNI_read_sess_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   Widget www ;

ENTRY("AFNI_read_sess_CB") ;

   if( GLOBAL_library.sslist->num_sess >= THD_MAX_NUM_SESSION ){
      (void) MCW_popup_message( w ,
                                  "********************************\n"
                                  "** Maximum number of sessions **\n"
                                  "** would be exceeded.  Sorry! **\n"
                                  "********************************"
                                , MCW_USER_KILL | MCW_TIMER_KILL ) ;
      EXRETURN ;
   }

   AFNI_make_file_dialog( im3d ) ;

   XtAddCallback( im3d->vwid->file_sbox , XmNokCallback ,
                  AFNI_finalize_read_sess_CB , cd ) ;

   XtAddCallback( im3d->vwid->file_sbox , XmNcancelCallback ,
                  AFNI_finalize_read_sess_CB , cd ) ;

   XtAddCallback( im3d->vwid->file_sbox , XmNhelpCallback ,
                  AFNI_finalize_read_sess_CB , cd ) ;

   XtVaSetValues(im3d->vwid->file_sbox,XmNfileTypeMask,XmFILE_DIRECTORY,NULL);
   MCW_set_widget_label( XtNameToWidget(im3d->vwid->file_sbox,"Items") ,
                         "Sessions" ) ;

   im3d->vwid->file_cb = AFNI_finalize_read_sess_CB ;
   im3d->vwid->file_cd = cd ;

   XtVaSetValues( im3d->vwid->file_dialog,
                     XmNtitle, "AFNI: Read Session",
                  NULL ) ;

   XtPopup( im3d->vwid->file_dialog , XtGrabNone ) ; RWC_sleep(1);
   RWC_visibilize_widget( im3d->vwid->file_dialog ) ; /* 09 Nov 1999 */
   NORMAL_cursorize( im3d->vwid->file_dialog ) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------*/
/*! Append datasets in THD_session ssb to those in ssa.
    \date 20 Dec 2001
*/

void AFNI_append_sessions( THD_session *ssa , THD_session *ssb )
{
   int qs, qd, vv ;

ENTRY("AFNI_append_sessions") ;

   if( !ISVALID_SESSION(ssa) || !ISVALID_SESSION(ssb) ) EXRETURN ;
   if( THD_equiv_files(ssa->sessname,ssb->sessname)   ) EXRETURN ;

   qs = ssa->num_dsset ;
   for( qd=0; qd < ssb->num_dsset && qd+qs < THD_MAX_SESSION_SIZE ; qd++ )
     for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )
       ssa->dsset[qd+qs][vv] = ssb->dsset[qd][vv] ;
   ssa->num_dsset += qd ;

   EXRETURN ;
}


/*---------------------------------------------------------------------
   Got a button press from the file selection dialog,
   so process it (maybe read in a new session!)
-----------------------------------------------------------------------*/

void AFNI_finalize_read_sess_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *)cb ;

ENTRY("AFNI_finalize_read_sess_CB") ;

   if( !IM3D_OPEN(im3d) || cbs == NULL ) EXRETURN ;  /* 04 Feb 2008 */

   switch( cbs->reason ){

      /** close the file selection dialog **/

      case XmCR_CANCEL:
         RWC_XtPopdown( im3d->vwid->file_dialog ) ;
      break ;

      /** try to read a new session **/

      case XmCR_OK:{
         char *text = NULL ;
         XmStringGetLtoR( cbs->value , XmFONTLIST_DEFAULT_TAG , &text ) ;
         if( text != NULL ){

            THD_session *new_ss = NULL ;

            /** if the user selected a file, strip it back to a directory **/

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"input text = %s",text) ; STATUS(str) ; }

            if( THD_is_file(text) ){
               int ii = strlen(text)-1 ;
               for( ; ii > 0 && text[ii] != '/' ; ii-- ) text[ii] = '\0' ;

if(PRINT_TRACING)
{ char str[256] ; sprintf(str,"defiled text = %s",text) ; STATUS(str) ; }
            }

            /** if the name given is a directory, try to read it **/

            if( THD_is_directory(text) ){
               int ii , eq=0 ;
               THD_session *old_ss ;

               /** 1st check if this is the same as some other session **/

STATUS("comparing to other sessions") ;
               for( ii=0 ; ii < GLOBAL_library.sslist->num_sess ; ii++ ){
                  old_ss = GLOBAL_library.sslist->ssar[ii] ;
                  eq     = THD_equiv_files( old_ss->sessname , text ) ;
                  if( eq == 1 ) break ;
               }

               if( eq == 1 ){
STATUS("illegal duplicate session") ;
                  XBell(im3d->dc->display,100) ;
                  (void) MCW_popup_message( w ,
                                             "*******************************\n"
                                             "** Illegal duplicate session **\n"
                                             "*******************************"
                                            , MCW_USER_KILL | MCW_TIMER_KILL ) ;
                  break ;
               } else {
STATUS("reading new session") ;
                  new_ss = THD_init_session( text ) ;  /*** Read session! ***/
               }
            } else { /** wasn't a directory!? **/

STATUS("wasn't a directory") ;
               XBell(im3d->dc->display,100) ;
               (void) MCW_popup_message( w ,
                                          "***********************************\n"
                                          "** Cannot find session directory **\n"
                                          "***********************************"
                                         , MCW_USER_KILL | MCW_TIMER_KILL ) ;
               break ;
            }

            /** OK, was a directory and we tried to read it **/

            if( new_ss == NULL || new_ss->num_dsset == 0 ){ /** failed to read anything **/

STATUS("failed to read new session") ;
               XBell(im3d->dc->display,100) ;
               (void) MCW_popup_message( w ,
                                           "******************************\n"
                                           "** Cannot read any datasets **\n"
                                           "******************************"
                                         , MCW_USER_KILL | MCW_TIMER_KILL ) ;

            } else if( GLOBAL_library.sslist->num_sess >= THD_MAX_NUM_SESSION ){

STATUS("too many sessions") ;
               XBell(im3d->dc->display,100) ;
               (void) MCW_popup_message( w ,
                                           "****************************\n"
                                           "** Max number of sessions **\n"
                                           "** exceeded -- Sorry!     **\n"
                                           "****************************"
                                         , MCW_USER_KILL | MCW_TIMER_KILL ) ;

            } else {  /** GOOD!  Actually process a new session.   **/
                      /** (The following is from AFNI_read_inputs) **/
               int qd , vv ;
               char str[356] ;  /* for messages */
               THD_3dim_dataset *dset ;

STATUS("processing new session") ;

               new_ss->parent = NULL ;

               for( qd=0 ; qd < new_ss->num_dsset ; qd++ ){      /* parentize */
                 for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ){
                   dset = new_ss->dsset[qd][vv] ;
                   if( dset != NULL ){
                     PARENTIZE( dset , NULL ) ;
                     AFNI_inconstancy_check(NULL,dset) ; /* 06 Sep 2006 */
                   }
               } }
               AFNI_inconstancy_check(im3d,NULL); /* 06 Sep 2006 */

               /* 20 Dec 2001: if have global datasets, put them in here */

               AFNI_append_sessions( new_ss , GLOBAL_library.session ) ;

               /* if we were living with a dummy, fix that */

               if( GLOBAL_library.have_dummy_dataset ) UNDUMMYIZE ;

               /* put the new session into place in the list of sessions */

STATUS("adding new session to list") ;
               GLOBAL_library.sslist->ssar[GLOBAL_library.sslist->num_sess] = new_ss ;
               (GLOBAL_library.sslist->num_sess)++ ;
               THD_reconcile_parents( GLOBAL_library.sslist ) ;

               sprintf(str," \n Session #%2d"
                            "\n %s"
                            "\n %d datasets\n" ,
                       GLOBAL_library.sslist->num_sess ,
                       new_ss->sessname, new_ss->num_dsset ) ;

               (void) MCW_popup_message( im3d->vwid->dmode->read_sess_pb,
                                         str, MCW_USER_KILL | MCW_TIMER_KILL ) ;

STATUS("rescanning timeseries files") ;
               AFNI_rescan_timeseries_CB(NULL,NULL,NULL) ;

               /* 28 Aug 2002: deal with warptables */

               if( new_ss->warptable != NULL ){
                 if( GLOBAL_library.warptable == NULL ) /* create global warptable */
                   GLOBAL_library.warptable = new_Htable(101) ;
                 subsume_Htable( new_ss->warptable , GLOBAL_library.warptable ) ;
                 destroy_Htable( new_ss->warptable ) ;
                 new_ss->warptable = NULL ;
               }

               RWC_XtPopdown( im3d->vwid->file_dialog ) ;

               /* 04 Feb 2008: switch to this session */

               if( !AFNI_noenv("AFNI_NEWSESSION_SWITCH") ){
                 MCW_choose_cbs cbs;
                 cbs.ival = GLOBAL_library.sslist->num_sess - 1 ;
                 AFNI_finalize_dataset_CB( im3d->vwid->view->choose_sess_pb,
                                           (XtPointer)im3d ,  &cbs          ) ;
               }
            } /* end of if we actually read a new session */

STATUS("freeing 'text' variable") ;
            myXtFree(text) ;
         }
      }
      break ;

      case XmCR_HELP:
         (void) MCW_popup_message( w ,
                    "To read in a new session, use the\n"
                    "Directories and Sessions selectors,\n"
                    "and the Filter entry and button,\n"
                    "to get the 'Selection' box correct;\n"
                    "that is, 'Selection' should be the\n"
                    "be the name of the session directory.\n"
                    "Then press 'Set'.\n"
                    "\n"
                    "How to Use the 'Directories' list:\n"
                    " Click on or use arrow keys to select\n"
                    " a directory, then press 'Enter' or\n"
                    " double-click.  This will set the\n"
                    " Selection to that directory name,\n"
                    " and will show the sub-directories\n"
                    " in the Sessions list to the right.\n"
                    "-----------------------------------\n"
                    "N.B.: To see datasets in the new\n"
                    "      session, you must use the\n"
                    "      'Switch Session' button!\n"
                 , MCW_USER_KILL ) ;
      break ;
   }
   EXRETURN ;
}

/*----------------------------------------------------------------
  Start getting ready to read a new timeseries in.  This
  is the CB for the "Read 1D" button.  We'll get a filename
  from the user, and process it in another routine.
------------------------------------------------------------------*/

void AFNI_read_1D_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   XmString xstr ;

ENTRY("AFNI_read_1D_CB") ;

   AFNI_make_file_dialog( im3d ) ;

   XtAddCallback( im3d->vwid->file_sbox , XmNokCallback ,
                  AFNI_finalize_read_1D_CB , cd ) ;

   XtAddCallback( im3d->vwid->file_sbox , XmNcancelCallback ,
                  AFNI_finalize_read_1D_CB , cd ) ;

   XtAddCallback( im3d->vwid->file_sbox , XmNhelpCallback ,
                  AFNI_finalize_read_1D_CB , cd ) ;

   XtVaSetValues(im3d->vwid->file_sbox,XmNfileTypeMask,XmFILE_REGULAR,NULL);
   MCW_set_widget_label( XtNameToWidget(im3d->vwid->file_sbox,"Items") ,
                         "1D Files" ) ;


   /* 02 Feb 1998: put *.1D* in the filename pattern */

   xstr = XmStringCreateLtoR( "*.1D*" , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( im3d->vwid->file_sbox ,
                     XmNpattern , xstr ,
                  NULL ) ;
   XmStringFree(xstr) ;

   im3d->vwid->file_cb = AFNI_finalize_read_1D_CB ;
   im3d->vwid->file_cd = cd ;

   XtVaSetValues( im3d->vwid->file_dialog,
                     XmNtitle, "AFNI: Read 1D Timeseries",
                  NULL ) ;

   XtPopup( im3d->vwid->file_dialog , XtGrabNone ) ; RWC_sleep(1);
   RWC_visibilize_widget( im3d->vwid->file_dialog ) ; /* 09 Nov 1999 */

   EXRETURN ;
}

/*---------------------------------------------------------------------
   Got a button press from the file selection dialog,
   so process it (maybe read in a new timeseries!)
-----------------------------------------------------------------------*/

void AFNI_finalize_read_1D_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) cb ;

ENTRY("AFNI_finalize_read_1D_CB") ;

   switch( cbs->reason ){

      /** close the file selection dialog **/

      case XmCR_CANCEL:
         RWC_XtPopdown( im3d->vwid->file_dialog ) ;
      break ;

      /** try to read a new timeseries **/

      case XmCR_OK:{
         char *text = NULL ;
         MRI_IMAGE *flim ;
         float *far ;
         int ii ;

         XmStringGetLtoR( cbs->value , XmFONTLIST_DEFAULT_TAG , &text ) ;
         flim = mri_read_1D( text ) ;
         if( flim == NULL || flim->nx < 2 ){
            XBell(im3d->dc->display,100) ;
            (void) MCW_popup_message( w ,
                                       "********************************\n"
                                       "** Cannot read data from file **\n"
                                       "********************************"
                                      , MCW_USER_KILL | MCW_TIMER_KILL ) ;
            myXtFree(text) ;
            break ;
         }

         far = MRI_FLOAT_PTR(flim) ;
         for( ii=0 ; ii < flim->nvox ; ii++ )
            if( fabs(far[ii]) >= 33333.0 ) far[ii] = WAY_BIG ;

         PLUTO_register_timeseries( text , flim ) ;
         mri_free(flim) ;
         myXtFree(text) ;
         RWC_XtPopdown( im3d->vwid->file_dialog ) ;
      }
      break ;

      case XmCR_HELP:
         (void) MCW_popup_message( w ,
                    "To read in a new timeseries, use the\n"
                    "Directories and '1D Files' selectors,\n"
                    "and the Filter entry and button,\n"
                    "to get the 'Selection' box correct;\n"
                    "that is, 'Selection' should be the\n"
                    "be the name of the 1D file to read.\n"
                    "Then press 'Set'.\n"
                    "\n"
                    "How to Use the 'Directories' list:\n"
                    " Click on or use arrow keys to select\n"
                    " a directory, then press 'Enter' or\n"
                    " double-click.  This will set the\n"
                    " Selection to that directory name.\n"
                    " You must then choose the 1D file you\n"
                    " want from the '1D Files' list at the\n"
                    " right.\n"
                 , MCW_USER_KILL ) ;
      break ;
   }
   EXRETURN ;
}

/*--------------------------------------------------------------------
   26 Mar 2001: read a dataset from the Web
----------------------------------------------------------------------*/

void AFNI_read_Web_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   XmString xstr ;

ENTRY("AFNI_read_Web_CB") ;

#if 0
   if( AFNI_splash_isopen() == 1 ){ BEEPIT ; EXRETURN ; }
#endif

   MCW_choose_string( w ,
    "Complete http:// or ftp:// address of dataset (.HEAD or .mnc or .mnc.gz):\n"
    "Examples: ftp://afni.nimh.nih.gov/AFNI/data/astrip+orig.HEAD\n"
    "          http://afni.nimh.nih.gov/afni/norm305.mnc.gz"
     , NULL , AFNI_finalize_read_Web_CB , (XtPointer) im3d ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void AFNI_finalize_read_Web_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   THD_3dim_dataset *dset ;
   XtPointer_array *dsar ;
   THD_session *ss = GLOBAL_library.sslist->ssar[im3d->vinfo->sess_num] ;
   char str[256] ;
   int nds,dd,vv , nn, na=-1,nf=-1 ,nts ;

ENTRY("AFNI_finalize_read_Web_CB") ;

   if( cbs->reason  != mcwCR_string ||
       cbs->cval    == NULL         ||
       cbs->cval[0] == '\0'         ||
       (strstr(cbs->cval,"http://")==NULL && strstr(cbs->cval,"ftp://")==NULL) ){

      (void) MCW_popup_message( im3d->vwid->dmode->read_Web_pb ,
                                  " \n** Illegal URL **\n " ,
                                MCW_USER_KILL | MCW_TIMER_KILL      ) ;

      XBell( XtDisplay(w) , 100 ) ; EXRETURN ;
   }

   /** read a list of datasets? **/

   if( strstr(cbs->cval,"AFNILIST") != NULL ){

      SHOW_AFNI_PAUSE ;
      dsar = THD_fetch_many_datasets( cbs->cval ) ; /* get array of datasets */
      SHOW_AFNI_READY ;
      if( dsar == NULL || dsar->num == 0 ){
         (void) MCW_popup_message( im3d->vwid->dmode->read_Web_pb ,
                                     " \n"
                                     "** Can't get datasets **\n"
                                     "** from that URL!     **\n " ,
                                   MCW_USER_KILL | MCW_TIMER_KILL      ) ;
         XBell( XtDisplay(w) , 100 ) ; EXRETURN ;
      }

   } else {  /** read one dataset **/

      SHOW_AFNI_PAUSE ;
      dset = THD_fetch_dataset( cbs->cval ) ;
      SHOW_AFNI_READY ;
      if( dset == NULL ){
        (void) MCW_popup_message( im3d->vwid->dmode->read_Web_pb ,
                                   " \n"
                                   "** Can't get a dataset **\n"
                                   "** from that URL!      **\n " ,
                                  MCW_USER_KILL | MCW_TIMER_KILL      ) ;
        XBell( XtDisplay(w) , 100 ) ; EXRETURN ;
      }
      INIT_XTARR(dsar) ; ADDTO_XTARR(dsar,dset) ; XTARR_IC(dsar,0) = IC_DSET ;
   }

   /** loop over all datasets in array, place in current session **/

   for( nts=nds=dd=0 ; dd < dsar->num ; dd++ ){

      if( XTARR_IC(dsar,dd) == IC_FLIM ){  /* process a 1D file */
         AFNI_add_timeseries( (MRI_IMAGE *) XTARR_XT(dsar,dd) ) ;
         nts++ ; continue ;
      }
      if( XTARR_IC(dsar,dd) != IC_DSET ) continue ;    /* bad */

      dset = (THD_3dim_dataset *) XTARR_XT(dsar,dd) ;
      if( !ISVALID_DSET(dset) ) continue ;             /* bad */
      AFNI_inconstancy_check(NULL,dset) ;      /* 06 Sep 2006 */
      vv = dset->view_type ;
      nn = ss->num_dsset ;
      if( nn >= THD_MAX_SESSION_SIZE ){
        fprintf(stderr,"\a\n*** too many anatomical datasets!\n") ;
        DSET_delete(dset) ;  /* 01 Nov 2001 */
      } else {
        ss->dsset[nn][vv] = dset ;
        ss->num_dsset++ ; nds++ ;
        if( vv == im3d->vinfo->view_type && na == -1 ) na = nn ;
      }
   } /* end of loop over dd=datasets in dsar */

   FREE_XTARR(dsar) ;
   AFNI_inconstancy_check(im3d,NULL); /* 06 Sep 2006 */

   /*-- popup a message saying what happened --*/

   if( nts > 0 )
      sprintf(str," \n Read %d datasets and \n"
                     "      %d timeseries from\n"
                     " %s\n ",nds,nts,cbs->cval  ) ;
   else
      sprintf(str," \n Read %d datasets from\n"
                     " %s\n ",nds,cbs->cval    ) ;

   (void) MCW_popup_message( im3d->vwid->dmode->read_Web_pb ,
                             str , MCW_USER_KILL | MCW_TIMER_KILL ) ;

   if( nds == 0 ){ XBell(XtDisplay(w),100); EXRETURN; }

   /*-- prepare to switch back to AFNI --*/

   if( na >= 0 ) im3d->vinfo->anat_num = na ; /* 1st new anat in current view */
   if( nf >= 0 ) im3d->vinfo->func_num = nf ; /* 1st new func in current view */

   if( GLOBAL_library.have_dummy_dataset ){   /* switch away from dummy dataset */
     UNDUMMYIZE ;
     if( na < 0 && ss->num_dsset > 1 ){
       im3d->vinfo->anat_num = 1 ;
       im3d->vinfo->func_num = 1 ;            /* 07 Sep 2006 (oops) */
       for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ){
         if( ISVALID_DSET(ss->dsset[1][vv]) ){ im3d->vinfo->view_type = vv; break; }
       }
     } else if( na < 0 ){                         /* should be impossible */
       (void) MCW_popup_message( im3d->vwid->dmode->read_Web_pb ,
                                 " \n** No datasets available **\n " ,
                                 MCW_USER_KILL | MCW_TIMER_KILL ) ;
     }
   }

   AFNI_initialize_view( NULL , im3d ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------
   Obey the command to rescan the current session
------------------------------------------------------------------*/

void AFNI_rescan_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *)cd , *qq3d ;
   int cc ;
   char str[256+THD_MAX_NAME] ;

ENTRY("AFNI_rescan_CB") ;

   SHOW_AFNI_PAUSE ;
   cc = AFNI_rescan_session( im3d->vinfo->sess_num ) ;
   POPDOWN_strlist_chooser ;
   if( cc > 0 ){
      sprintf(str," \n"
                  " Added %d datasets to \n"
                  " %s\n" ,
             cc ,
             GLOBAL_library.sslist->ssar[im3d->vinfo->sess_num]->sessname ) ;
      (void) MCW_popup_message( w , str , MCW_USER_KILL | MCW_TIMER_KILL ) ;
   }

#if 1
   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){    /* 31 Mar 1999 */
      qq3d = GLOBAL_library.controllers[cc] ;
      if( IM3D_OPEN(qq3d) ) AFNI_process_dsetchange( qq3d ) ;
   }
#endif

   SHOW_AFNI_READY ;
   EXRETURN ;
}

/*----------------------------------------------------------------*/
/* 10 Nov 2005: check periodically for updated datasets */

static int block_rescan = 0 ;
void AFNI_block_rescan( int bb ){ block_rescan = bb ; }

void AFNI_rescan_timeout_CB( XtPointer client_data , XtIntervalId *id )
{
  XtAppContext *apc = (XtAppContext *)client_data ;

ENTRY("AFNI_rescan_timeout_CB") ;
  if( !block_rescan ) AFNI_rescan_all_CB(NULL,NULL,NULL) ;
  (void) XtAppAddTimeOut( *apc, 14999, AFNI_rescan_timeout_CB, apc ) ;
  EXRETURN ;
}

/*----------------------------------------------------------------*/

void AFNI_rescan_all_CB( Widget w, XtPointer cd, XtPointer cb )
{
   int iss , cc=0 , uu=(w!=(Widget)NULL) , pp=0 ;
   Three_D_View *im3d ;

ENTRY("AFNI_rescan_all_CB") ;

   for( iss=0 ; iss < GLOBAL_library.sslist->num_sess ; iss++ ){
     cc += AFNI_rescan_session( iss ) ;
     if( pp==0 && cc > 0 ){ SHOW_AFNI_PAUSE; pp=1; }
   }
   if( cc > 0 && uu ){
     char str[256] ;
     POPDOWN_strlist_chooser ;
     sprintf(str," \n"
                 " Added %d datasets total \n" , cc ) ;
     (void) MCW_popup_message( w , str , MCW_USER_KILL | MCW_TIMER_KILL ) ;
   } else if( cc == 0 && uu ){
     (void) MCW_popup_message( w ,
                               " \n Found no new datasets \n" ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
   }

#if 1
   if( cc > 0 ){
     for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){    /* 31 Mar 1999 */
       im3d = GLOBAL_library.controllers[cc] ;
       if( IM3D_OPEN(im3d) ) AFNI_process_dsetchange( im3d ) ;
     }
   }
#endif

   if( pp ) SHOW_AFNI_READY ;
   EXRETURN ;
}

/*----------------------------------------------------------------------*/
/*!
  Re-read the session indexed by "sss".  THE OLD WAY.
  Much of this code is taken from AFNI_read_inputs.
  WARNING: this will do bad things if the user deletes the
           session directory or the current active datasets within it
           before trying this.  On the other hand, if the user is that
           stupid, bad things will probably have happened to him already
           (like being unable to open dataset files, or being unable to
           tie his shoes correctly, or being named Mike Beauchamp).
------------------------------------------------------------------------*/

static int AFNI_rescan_session_OLD( int sss )  /* the old way */
{
   int vv , ii , cc , nold,nnew ;
   THD_session   *new_ss , *old_ss ;
   Three_D_View  *im3d ;
   MCW_idcode     anat_idcode[MAX_CONTROLLERS] ,
                  func_idcode[MAX_CONTROLLERS] ;
   THD_slist_find find ;
   THD_3dim_dataset *dset ;

ENTRY("AFNI_rescan_session_OLD") ;
{ char str[256]; sprintf(str,"session index %d\n",sss); STATUS(str); }

   if( GLOBAL_library.have_dummy_dataset ){ BEEPIT ; RETURN(0) ; }

#if 0
fprintf(stderr,"Enter AFNI_rescan_session_OLD on session index %d\n",sss) ;
#endif

   /*--- sanity checks ---*/

   if( sss < 0 || sss >= GLOBAL_library.sslist->num_sess ){ BEEPIT ; RETURN(0) ; }

   old_ss = GLOBAL_library.sslist->ssar[sss] ;
   if( ! ISVALID_SESSION(old_ss) ){ BEEPIT; RETURN(0); }

   if( old_ss == GLOBAL_library.session ) RETURN(0) ;  /* 21 Dec 2001 */

   /*--- Make sure that the dataset choosers are closed.
         Since these are just instances of the generic strlist
         chooser, and we can't tell what is being chosen just now,
         we'll just forcibly close the strlist chooser no matter what. ---*/

   POPDOWN_strlist_chooser ;

   /*--- mark all datasets in the old session for deletion from memory ---*/

STATUS("marking old session datasets") ;

   nold = old_ss->num_dsset ;

   for( ii=0 ; ii < old_ss->num_dsset ; ii++ )
      for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )
         if( ISVALID_3DIM_DATASET(old_ss->dsset[ii][vv]) )
            if( DSET_in_global_session(old_ss->dsset[ii][vv]) )
               old_ss->dsset[ii][vv] = NULL ;   /* will be added back in later */
            else
               DSET_MARK_FOR_DEATH( old_ss->dsset[ii][vv] ) ;

   /*--- mark all descendants for purging as well ---*/

   AFNI_mark_for_death( GLOBAL_library.sslist ) ;

   /*--- but before actual deletion, find the
         active datasets in each main controller window ---*/

STATUS("checking active controllers") ;

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){
      ZERO_IDCODE(anat_idcode[cc]) ; ZERO_IDCODE(func_idcode[cc]) ;
      im3d = GLOBAL_library.controllers[cc] ;
      if( IM3D_OPEN(im3d) && im3d->vinfo->sess_num == sss ){
         anat_idcode[cc] = im3d->anat_now->idcode ;

         if( ISVALID_3DIM_DATASET(im3d->fim_now) )
           func_idcode[cc] = im3d->fim_now->idcode ;

         XmUpdateDisplay(im3d->vwid->top_shell) ;
      }
   }

   /*--- now can flush the old datasets, prior to reading again ---*/

   AFNI_andersonville( GLOBAL_library.sslist , False ) ; /* keep files! */

   /*--- now read in the session again ---*/

STATUS("rescanning session now:") ;
STATUS(old_ss->sessname) ;

   new_ss = THD_init_session( old_ss->sessname ) ;

   if( new_ss == NULL || new_ss->num_dsset <= 0 ){
      fprintf(stderr,"\n*** Fatal error: Rescan of session %s finds nothing!\a\n",
              old_ss->sessname ) ;
      EXIT(1) ;
   }

   myXtFree( old_ss ) ;  /* no longer need this */

   /* set parent pointers */

STATUS("PARENTIZE-ing datasets in new session") ;

   new_ss->parent = NULL ;
   for( ii=0 ; ii < new_ss->num_dsset ; ii++ ){
     for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ){
       dset = new_ss->dsset[ii][vv] ;
       if( dset != NULL ){
         PARENTIZE( new_ss->dsset[ii][vv] , NULL ) ;
         AFNI_inconstancy_check(NULL,dset) ; /* 06 Sep 2006 */
       }
   } }
   AFNI_inconstancy_check(NULL,NULL);

   /* put the new session into place in the list of sessions */

   GLOBAL_library.sslist->ssar[sss] = new_ss ;

   /* 20 Dec 2001: add the global datasets back in, if any */

   AFNI_append_sessions( new_ss , GLOBAL_library.session ) ;

   /* assign the warp and anatomy parent pointers;
      then, make any datasets that don't exist but logically
      descend from the warp and anatomy parents just assigned */

   THD_reconcile_parents( GLOBAL_library.sslist ) ;
   AFNI_force_adoption( new_ss , GLOBAL_argopt.warp_4D ) ;
   AFNI_make_descendants( GLOBAL_library.sslist ) ;

   /* 28 Aug 2002: deal with warptables */

   if( new_ss->warptable != NULL ){
     if( GLOBAL_library.warptable == NULL ) /* create global warptable */
       GLOBAL_library.warptable = new_Htable(101) ;
     subsume_Htable( new_ss->warptable , GLOBAL_library.warptable ) ;
     destroy_Htable( new_ss->warptable ) ;
     new_ss->warptable = NULL ;
   }

   /*--- for each main controller window, must reset some pointers ---*/

STATUS("fixing active controllers") ;

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){
      im3d = GLOBAL_library.controllers[cc] ;
      if( IM3D_OPEN(im3d) && im3d->vinfo->sess_num == sss ){
         im3d->ss_now = new_ss ;
         vv           = im3d->vinfo->view_type ;  /* won't change this */

         im3d->fimdata->fimdset = NULL ;          /* disable FIMage */

         /* look for old anat dataset in new session */

         find = THD_dset_in_session( FIND_IDCODE ,
                                     &(anat_idcode[cc]) , new_ss ) ;

         /* if have it, use it, otherwise, pick first anat in this view */

         if( find.dset != NULL && find.view_index == vv ){
            im3d->vinfo->anat_num = find.dset_index ;
         } else {
            for( ii=0 ; ii < new_ss->num_dsset ; ii++ )
               if( ISVALID_3DIM_DATASET(new_ss->dsset[ii][vv]) ) break ;
            if( ii < new_ss->num_dsset ){
               im3d->vinfo->anat_num = ii ;
            } else {
               fprintf(stderr,
                       "\n*** Fatal error:"
                       " Cannot find anat dataset to switch to after"
                       " rescanning session %s\a\n",
                       new_ss->sessname ) ;
               EXIT(1) ;
            }
         }

         /* do the same for old func dataset, if any */

         if( ! ISZERO_IDCODE(func_idcode[cc]) ){
            find = THD_dset_in_session( FIND_IDCODE ,
                                        &(func_idcode[cc]) , new_ss ) ;

            if( find.dset != NULL && find.view_index == vv ){
               im3d->vinfo->func_num = find.dset_index ;
            } else {
               for( ii=0 ; ii < new_ss->num_dsset ; ii++ )
                  if( ISVALID_3DIM_DATASET(new_ss->dsset[ii][vv]) ) break ;
               if( ii < new_ss->num_dsset ){
                  im3d->vinfo->func_num = ii ;
               } else {
                  im3d->vinfo->func_num = 0 ;  /* no func is not fatal */
               }
            }
         } else {
            im3d->vinfo->func_num = 0 ;
         }

         /* switch this controller to the new datasets */

         AFNI_initialize_view( NULL , im3d ) ;
         XmUpdateDisplay(im3d->vwid->top_shell) ;
      }
   }

   nnew = new_ss->num_dsset ;

   RETURN( (nnew-nold) ) ;
}

/*----------------------------------------------------------------------*/
/*!
  Re-read the session indexed by "sss".  THE NEW WAY.
  Much of this code is taken from AFNI_read_inputs().

  WARNING:
    - This will do bad things if the user deletes the session directory
      or the current active datasets within it before trying this.
    - On the other hand, if the user is that stupid, bad things will
      probably have happened to him already (like being unable to open
      dataset files, or being unable to tie his shoes correctly).

  28 Dec 2002: modified extensively to not clobber existing pointers
               to datasets, but instead to insert new datasets into the
               existing session -- RWCox (MX&HNY)
------------------------------------------------------------------------*/

static int AFNI_rescan_session_NEW( int sss )   /* the new way */
{
   int vv , ii , nr , na_new=0 , nf_new=0 ;
   THD_session  *new_ss , *old_ss ;
   THD_slist_find find ;
   THD_3dim_dataset *new_dset ;

ENTRY("AFNI_rescan_session_NEW") ;
{ char str[256]; sprintf(str,"session index %d\n",sss); STATUS(str); }

   if( GLOBAL_library.have_dummy_dataset ){ BEEPIT; RETURN(0); }

#if 0
fprintf(stderr,"Enter AFNI_rescan_session_NEW on session index %d\n",sss) ;
#endif

   /*--- sanity checks ---*/

   if( sss < 0 || sss >= GLOBAL_library.sslist->num_sess ){ BEEPIT; RETURN(0); }

   old_ss = GLOBAL_library.sslist->ssar[sss] ;
   if( ! ISVALID_SESSION(old_ss) ){ BEEPIT; RETURN(0); }

                                     /* can't rescan global session */
   if( old_ss == GLOBAL_library.session ) RETURN(0); /* 21 Dec 2001 */

   /*--- read in the session again, into a new THD_session struct ---*/

STATUS("rescanning session now:") ;
STATUS(old_ss->sessname) ;

   new_ss = THD_init_session( old_ss->sessname ) ;
   if( ! ISVALID_SESSION(new_ss) ){ BEEPIT; RETURN(0); } /* this is BAD */

   /*--- scan datasets and remove those
         that already exist in this session ---*/

   for( ii=0 ; ii < new_ss->num_dsset ; ii++ ){
     for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ){
       new_dset = new_ss->dsset[ii][vv] ;
       if( ISVALID_DSET(new_dset) ){
         find = THD_dset_in_session( FIND_IDCODE, &(new_dset->idcode), old_ss );
         if( find.dset == NULL ){
          find = THD_dset_in_session(FIND_PREFIX, DSET_PREFIX(new_dset),old_ss);
          if( find.dset != NULL && find.view_index != vv ) find.dset = NULL ;
         }
         if( find.dset != NULL ){
           DSET_delete(new_dset); new_ss->dsset[ii][vv] = NULL;
         }
       }
     }
   }

   /*--- now scan survivors and put them
         at the end of the existing session ---*/

   for( ii=0 ; ii < new_ss->num_dsset ; ii++ ){
     for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )     /* see if row is empty */
       if( new_ss->dsset[ii][vv] != NULL ) break ;
     if( vv > LAST_VIEW_TYPE ) continue ;          /* empty row ==> skip  */
     AFNI_inconstancy_check(NULL,new_ss->dsset[ii][vv]) ;  /* 06 Sep 2006 */
     nr = old_ss->num_dsset ;                      /* next row in old_ss  */
     if( nr >= THD_MAX_SESSION_SIZE ) break ;      /* old session is full */
     for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )     /* copy new row to old */
       old_ss->dsset[nr][vv] = new_ss->dsset[ii][vv];
     old_ss->num_dsset++ ;  na_new++ ;             /* 1 more row in old   */
   }
   if( na_new == 0 ) RETURN(0) ;                   /* 10 Nov 2005 */
   AFNI_inconstancy_check(NULL,NULL);              /* 06 Sep 2006 */

   /*-- 15 Jan 2003: purge all datasets from memory (for Hauke Heekeren) --*/

   for( ii=0 ; ii < old_ss->num_dsset ; ii++ )
     for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )
       if( old_ss->dsset[ii][vv] != NULL ) DSET_unload(old_ss->dsset[ii][vv]);

   /* assign the warp and anatomy parent pointers;
      then, make any datasets that don't exist but logically
      descend from the warp and anatomy parents just assigned */

   THD_reconcile_parents( GLOBAL_library.sslist ) ;
   AFNI_force_adoption( old_ss , GLOBAL_argopt.warp_4D ) ;
   AFNI_make_descendants( GLOBAL_library.sslist ) ;

   /* 28 Aug 2002: deal with warptables */

   if( new_ss->warptable != NULL ){
     if( GLOBAL_library.warptable == NULL ) /* create global warptable */
       GLOBAL_library.warptable = new_Htable(101) ;
     subsume_Htable( new_ss->warptable , GLOBAL_library.warptable ) ;
     destroy_Htable( new_ss->warptable ) ;
     new_ss->warptable = NULL ;
   }
   free(new_ss) ;

   RETURN(na_new) ;
}

/*----------------------------------------------------------------------*/
/*! Use the old or the new rescan session methods -- 07 Feb 2003.
------------------------------------------------------------------------*/

int AFNI_rescan_session( int sss )
{
   char *eee = getenv("AFNI_RESCAN_METHOD") ;
   int use_new , use_rep ;
   static int first=1 ;

   use_rep = ( eee != NULL && strcasecmp(eee,"REPLACE") == 0 ) ;

   use_new = ( AFNI_yesenv("AFNI_AUTO_RESCAN")      ||
               AFNI_yesenv("AFNI_RESCAN_AT_SWITCH") || !use_rep ) ;

   if( use_rep && use_new && first ){  /* 07 Oct 2008 */
     WARNING_message(
       " \n"
       "   AFNI_RESCAN_METHOD = REPLACE is incompatible with\n"
       "   AFNI_AUTO_RESCAN = YES  and/or  AFNI_RESCAN_AT_SWITCH = YES" ) ;
     first = 0 ;
   }

   return (use_new) ? AFNI_rescan_session_NEW( sss )
                    : AFNI_rescan_session_OLD( sss ) ;
}

/*---------------------------------------------------------------
   Rescan for timeseries files
-----------------------------------------------------------------*/

void AFNI_rescan_timeseries_CB(Widget w, XtPointer cd, XtPointer cb)
{
   int iss , inew , jold , nnew , nold , nadd=0 ;
   THD_string_array *dlist ;
   THD_session *ss ;
   MRI_IMARR *newtsar ;
   MRI_IMAGE *newim , *oldim ;

ENTRY("AFNI_rescan_timeseries_CB") ;

   /** assemble list of directories **/

   if( GLOBAL_library.have_dummy_dataset ){ BEEPIT ; EXRETURN ; }

   INIT_SARR( dlist ) ;

   for( iss=0 ; iss < GLOBAL_library.sslist->num_sess ; iss++ ){
      ss = GLOBAL_library.sslist->ssar[iss] ;
      ADDTO_SARR(dlist,ss->sessname) ;
   }

   /** read timeseries into a new array **/

   newtsar = THD_get_many_timeseries( dlist ) ;
   DESTROY_SARR( dlist ) ;
   if( newtsar == NULL ) EXRETURN ;

   /** check to see which ones are in the old list **/

   nnew = IMARR_COUNT(newtsar) ;
   nold = IMARR_COUNT(GLOBAL_library.timeseries) ;

   for( inew=0 ; inew < nnew ; inew++ ){
      newim = IMARR_SUBIMAGE(newtsar,inew) ;  /* new timeseries */
      for( jold=0 ; jold < nold ; jold++ ){
         oldim = IMARR_SUBIMAGE(GLOBAL_library.timeseries,jold) ; /* old one */

         if( oldim != NULL && oldim->name != NULL &&        /* break out of loop */
             strcmp(oldim->name,newim->name) == 0 ) break ; /* when new == old */
      }

      if( jold == nold ){
         ADDTO_IMARR(GLOBAL_library.timeseries,newim); nadd++;  /* is new */
      } else {
         mri_free(newim) ;                                      /* is old */
      }
   }

   if( nadd > 0 ) POPDOWN_timeseries_chooser ;
   FREE_IMARR(newtsar) ;
   EXRETURN ;
}

/*---------------------------------------------------------------
   callback for the anatmode bbox
-----------------------------------------------------------------*/

void AFNI_anatmode_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int old_val , new_val ;
   THD_fvec3 fv ;
   THD_ivec3 iv ;

ENTRY("AFNI_anatmode_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   old_val = 1 << im3d->vinfo->force_anat_wod ;
   new_val = MCW_val_bbox( im3d->vwid->dmode->anatmode_bbox ) ;

if(PRINT_TRACING){
 char str[256] ; sprintf(str,"old_val=%d new_val=%d",old_val,new_val) ;
 STATUS(str) ; }

   if( new_val != old_val ){
     im3d->vinfo->force_anat_wod = (new_val != DMODE_BRICK_BVAL) ;
     SHOW_AFNI_PAUSE ;
     im3d->vinfo->tempflag = 1 ;           /* 15 Mar 2000 */
     AFNI_modify_viewing( im3d , True ) ;  /* redisplay */
     SHOW_AFNI_READY ;
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------
   callback for the funcmode bbox
-----------------------------------------------------------------*/

void AFNI_funcmode_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int old_val , new_val ;
   THD_fvec3 fv ;
   THD_ivec3 iv ;

ENTRY("AFNI_funcmode_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   old_val = 1 << im3d->vinfo->force_func_wod ;
   new_val = MCW_val_bbox( im3d->vwid->dmode->funcmode_bbox ) ;

if(PRINT_TRACING){
 char str[256] ; sprintf(str,"old_val=%d new_val=%d",old_val,new_val) ;
 STATUS(str) ; }

   if( new_val != old_val ){
     im3d->vinfo->force_func_wod = (new_val != DMODE_BRICK_BVAL) ;
     SHOW_AFNI_PAUSE ;
     im3d->vinfo->tempflag = 1 ;           /* 15 Mar 2000 */
     AFNI_modify_viewing( im3d , True ) ;  /* redisplay */
     SHOW_AFNI_READY ;
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*----------------------------------------------------------------
   Used to force redisplay when some datamode parameters have
   been altered
------------------------------------------------------------------*/

void AFNI_modify_viewing( Three_D_View *im3d , Boolean rescaled )
{
   THD_fvec3 fv ;
   THD_ivec3 iv ;

ENTRY("AFNI_modify_viewing") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   /* set up datasets for new imaging */

   AFNI_setup_viewing( im3d , rescaled ) ;

   /* transform current POV to new indices */

   if( im3d->type == AFNI_3DDATA_VIEW ){            /* 19 Oct 1999 */

      LOAD_ANAT_VIEW(im3d) ;  /* 02 Nov 1996 */
      fv = THD_dicomm_to_3dmm(
             im3d->anat_now ,
             TEMP_FVEC3(im3d->vinfo->xi, im3d->vinfo->yj, im3d->vinfo->zk) ) ;
      iv = THD_3dmm_to_3dind( im3d->anat_now , fv ) ;

   } else {

      iv = TEMP_IVEC3( im3d->vinfo->i1 , im3d->vinfo->j2 , im3d->vinfo->k3 ) ;
   }

   /* and redisplay the images */

   DISABLE_LOCK ;
   AFNI_set_viewpoint( im3d, iv.ijk[0],iv.ijk[1],iv.ijk[2] , REDISPLAY_ALL ) ;
   ENABLE_LOCK ;

   SAVE_VPT(im3d) ;
   FIX_SCALE_SIZE(im3d) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------
  23 Nov 1996: Setup to write out many datasets
----------------------------------------------------------------------*/

void AFNI_write_many_dataset_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   static MCW_idcode * idclist  = NULL ;
   static char       **strlist  = NULL ;
   static int          num_dset = -1 ;

   int iss , id , vv , llen , ltop ;
   THD_session *ss ;
   THD_3dim_dataset *dset ;
   char nam[THD_MAX_NAME+16] , *tnam , qnam[THD_MAX_NAME+16] ;

ENTRY("AFNI_write_many_dataset_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;
   if( GLOBAL_library.have_dummy_dataset ){ BEEPIT ; EXRETURN ; }

   if( num_dset > 0 && strlist != NULL )
      for( id=0 ; id < num_dset ; id++ ) myXtFree(strlist[id]) ;
   myXtFree(idclist) ; myXtFree(strlist) ;

   vv = im3d->vinfo->view_type ;  /* select view type */

   /** scan once to find longest string name **/

   ltop = 4 ;
   for( iss=0 ; iss < GLOBAL_library.sslist->num_sess ; iss++ ){
      ss = GLOBAL_library.sslist->ssar[iss] ;

      for( id=0 ; id < ss->num_dsset ; id++ ){
         dset = ss->dsset[id][vv] ;
         if( DSET_WRITEABLE(dset) ){
            strcpy( nam , dset->dblk->diskptr->directory_name ) ;
            strcat( nam , dset->dblk->diskptr->filecode ) ;
            tnam = THD_trailname(nam,SESSTRAIL+1) ;
            llen = strlen(tnam) ; ltop = MAX(ltop,llen) ;
         }
      }
   }
   ltop = MIN(ltop,THD_MAX_NAME) ; /* 06 Aug 2002 */

   num_dset = 0 ;
   for( iss=0 ; iss < GLOBAL_library.sslist->num_sess ; iss++ ){
      ss = GLOBAL_library.sslist->ssar[iss] ;

      /* check anat datasets */

      for( id=0 ; id < ss->num_dsset ; id++ ){
         dset = ss->dsset[id][vv] ;
         if( DSET_WRITEABLE(dset) ){
            num_dset++ ;
            idclist = (MCW_idcode *) XtRealloc( (char *) idclist ,
                                                sizeof(MCW_idcode) * num_dset ) ;
            strlist = (char **)      XtRealloc( (char *) strlist ,
                                                sizeof(char *)     * num_dset ) ;

            strcpy( nam , dset->dblk->diskptr->directory_name ) ;
            strcat( nam , dset->dblk->diskptr->filecode ) ;
            tnam = THD_trailname(nam,SESSTRAIL+1) ;

            if( ISANAT(dset) ){
              if( ISANATBUCKET(dset) )         /* 30 Nov 1997 */
                 sprintf(qnam,"%-*s [%s:%d]" ,
                         ltop,tnam , ANAT_prefixstr[dset->func_type] , DSET_NVALS(dset) ) ;

              else if( DSET_NUM_TIMES(dset) == 1 )
                 sprintf(qnam,"%-*s [%s]" ,
                         ltop,tnam ,ANAT_prefixstr[dset->func_type] ) ;

              else
                 sprintf(qnam,"%-*s [%s:3D+t]" ,
                         ltop,tnam , ANAT_prefixstr[dset->func_type] ) ;

            } else if( ISFUNC(dset) ){
              if( ISFUNCBUCKET(dset) )             /* 30 Nov 1997 */
                 sprintf(qnam,"%-*s [%s:%d]" ,
                         ltop,tnam , FUNC_prefixstr[dset->func_type] , DSET_NVALS(dset) ) ;

              else if( DSET_NUM_TIMES(dset) == 1 )
                 sprintf(qnam,"%-*s [%s]" ,
                         ltop,tnam , FUNC_prefixstr[dset->func_type] ) ;

              else
                 sprintf(qnam,"%-*s [%s:3D+t]" ,
                         ltop,tnam , FUNC_prefixstr[dset->func_type] ) ;
            }

            strlist[num_dset-1] = XtNewString(qnam) ;
            idclist[num_dset-1] = dset->idcode ;
         }
      } /* end of loop over datasets */

   } /* end of loop over sessions */

   if( num_dset <= 0 ){
      XBell(im3d->dc->display,100) ;  /* for the fun of it! */

      (void) MCW_popup_message( w ,
                 "*******************************\n"
                 "** No datasets are available **\n"
                 "** to write out to disk.     **\n"
                 "*******************************"  ,
              MCW_USER_KILL | MCW_TIMER_KILL ) ;
      EXRETURN ;
   }

#if 1
   MCW_choose_multi_strlist( w , "Datasets to Write" , mcwCT_multi_mode ,
                             num_dset , NULL , strlist ,
                             AFNI_do_many_writes , (XtPointer) idclist ) ;
#else
   { THD_string_array *sar ;  /*** This code is for experiments only! ***/
     INIT_SARR(sar) ;
     for( id=0 ; id < num_dset ; id++ ) ADDTO_SARR(sar,strlist[id]) ;

     MCW_choose_multi_editable_strlist( w , "Datasets to Write" , mcwCT_multi_mode ,
                                        sar , NULL ,
                                        AFNI_do_many_writes , (XtPointer) idclist ) ;
   }
#endif

   XtVaSetValues( w , XmNuserData , (XtPointer) im3d , NULL ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void AFNI_do_many_writes( Widget wpar , XtPointer cd , MCW_choose_cbs *cbs )
{
   MCW_idcode *idclist = (MCW_idcode *) cd ;
   Three_D_View *im3d = NULL , *qq3d ;
   THD_3dim_dataset *dset ;
   THD_dataxes      new_daxes ;
   int ib , resam_mode ;
   Boolean good ;
   Widget wmsg ;
   int cc , ccanat[MAX_CONTROLLERS] , ccfunc[MAX_CONTROLLERS] ;

ENTRY("AFNI_do_many_writes") ;

   XtVaGetValues( wpar , XmNuserData , &im3d , NULL ) ;
   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   SHOW_AFNI_PAUSE ;

   /** mark all controllers as untouched **/

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ) ccanat[cc] = ccfunc[cc] = 0 ;

   wmsg = MCW_popup_message( wpar ,
            "*******************************\n"
            "** Please wait for dataset   **\n"
            "** computations and disk I/O **\n"
            "*******************************" , MCW_CALLER_KILL ) ;
   AFNI_speak("Writing",0) ;

   /** loop through selected datasets and do the dirty work **/

   for( ib=0 ; ib < cbs->nilist ; ib++ ){
      dset = PLUTO_find_dset( idclist + cbs->ilist[ib] ) ;
      if( DSET_WRITEABLE(dset) ){

         fprintf(stderr,"-- writing dataset %s%s (%d of %d)\n" ,
                        dset->dblk->diskptr->directory_name ,
                        dset->dblk->diskptr->filecode ,
                        ib+1 , cbs->nilist ) ;

         new_daxes.type = DATAXES_TYPE ;

#ifdef USE_WRITEOWNSIZE
         if( im3d->vinfo->writeownsize )
            THD_edit_dataxes( im3d->vinfo->resam_vox , dset->daxes , &new_daxes ) ;
         else
#endif
            THD_edit_dataxes( im3d->vinfo->resam_vox ,
                              CURRENT_DAXES(im3d->anat_now) , &new_daxes ) ;

         resam_mode = (ISFUNC(dset)) ? im3d->vinfo->func_resam_mode
                                     : im3d->vinfo->anat_resam_mode ;

         good = AFNI_refashion_dataset( im3d , dset , &new_daxes , resam_mode ) ;

         /** if the output failed, put a message to the screen **/

         if( ! good ){  /* bad news! */
            char str[THD_MAX_NAME+128] ;
            sprintf( str , "**\n"      "** Attempt to write dataset\n"
                           "** %s%s\n" "** failed for unknown reasons!\n**" ,
                     dset->dblk->diskptr->directory_name ,
                     dset->dblk->diskptr->filecode ) ;
            (void) MCW_popup_message( wpar , str , MCW_USER_KILL | MCW_TIMER_KILL ) ;

            fprintf(stderr,"   [output of %s%s failed!]\n",
                           dset->dblk->diskptr->directory_name ,
                           dset->dblk->diskptr->filecode ) ;

          /** otherwise, check if the dataset just done is
              the active anatomy or function in any open controller window **/

         } else {
            for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){
               qq3d = GLOBAL_library.controllers[cc] ;
               if( ! IM3D_OPEN(qq3d) ) continue ;
               if( dset == qq3d->anat_now ) ccanat[cc] = 1 ;  /* flag their  */
               if( dset == qq3d->fim_now  ) ccfunc[cc] = 1 ;  /* controllers */
            }
         }
      }
   }

   /** for any controllers whose active datasets were written out,
       we need to set the "See Brick" buttons to be sensitive.     **/

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){
      qq3d = GLOBAL_library.controllers[cc] ;
      if( ! IM3D_OPEN(qq3d) ) continue ;

      if( ccfunc[cc] ){
STATUS("resetting 'use func brick' button") ;
         XtSetSensitive( qq3d->vwid->dmode->funcmode_bbox->wbut[DMODE_BRICK] , True ) ;
         AFNI_reset_func_range( qq3d ) ;
         AFNI_set_viewpoint( qq3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
      }

      if( ccanat[cc] ){
STATUS("resetting 'use anat brick' button") ;
         XtSetSensitive( qq3d->vwid->dmode->anatmode_bbox->wbut[DMODE_BRICK] , True ) ;
      }
   }

   XtDestroyWidget( wmsg ) ;
   SHOW_AFNI_READY ;
   EXRETURN ;
}

/*-----------------------------------------------------------------
    Obey the command to write out the current dataset
-------------------------------------------------------------------*/

void AFNI_write_dataset_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   THD_3dim_dataset *dset = NULL ;
   THD_dataxes        new_daxes ;
   Widget wmsg ;
   int resam_mode = 0;
   Boolean good , destroy ;

ENTRY("AFNI_write_dataset_CB") ;

   if( ! IM3D_VALID(im3d) || w == NULL ||
       ! XtIsWidget(w)    || ! XtIsRealized(w) ) EXRETURN ;
   if( GLOBAL_library.have_dummy_dataset ){ BEEPIT ; EXRETURN ; }

   if( w == im3d->vwid->dmode->write_anat_pb ){         /* write anatomy */
      dset       = im3d->anat_now ;
      resam_mode = im3d->vinfo->anat_resam_mode ;

   } else if( w == im3d->vwid->dmode->write_func_pb ){  /* write function */
      dset       = im3d->fim_now ;
      resam_mode = im3d->vinfo->func_resam_mode ;
   }

   good = ISVALID_3DIM_DATASET(dset)     &&     /* check for bad data */
          resam_mode >= FIRST_RESAM_TYPE &&
          resam_mode <= LAST_RESAM_TYPE  &&
          im3d->vinfo->resam_vox > 0.0   &&
         !DSET_IS_MINC(dset)             &&      /* 29 Oct 2001 */
         !DSET_IS_ANALYZE(dset)          &&      /* 27 Aug 2002 */
         !DSET_IS_CTFSAM(dset)           &&
         !DSET_IS_CTFMRI(dset)           &&
         !DSET_IS_1D(dset)               &&
         !DSET_IS_NIFTI(dset)                   /* 28 Aug 2003 */
       ;

   destroy = !DSET_WRITEABLE(dset) ;      /* check for destruction */

   if( good && destroy ){
      if( GLOBAL_argopt.destruct ){
         (void) MCW_popup_message( w ,
                                   "******************************\n"
                                   "** Potentially destructive  **\n"
                                   "** dataset write initiated! **\n"
                                   "******************************"  ,
                                  MCW_USER_KILL | MCW_TIMER_KILL ) ;
      } else {
         good = False ;  /* destruction not OK */
      }
   }

   if( !good ){

       XBell(im3d->dc->display,100) ;  /* for the fun of it! */

       (void) MCW_popup_message( w ,
                 "****************************************************\n"
                 "** Cannot write dataset for one of these reasons: **\n"
                 "**   -- It isn't allowed to write data in the     **\n"
                 "**        Original View.                          **\n"
                 "**   -- It isn't allowed to overwrite data that   **\n"
                 "**        is not warped from some other dataset.  **\n"
                 "**   -- An internal program error has occured!    **\n"
                 "****************************************************"  ,
              MCW_USER_KILL | MCW_TIMER_KILL ) ;

       EXRETURN;
   }

   SHOW_AFNI_PAUSE ;

   wmsg = MCW_popup_message( w ,
            "*******************************\n"
            "** Please wait for dataset   **\n"
            "** computations and disk I/O **\n"
            "*******************************" , MCW_CALLER_KILL ) ;

   XtSetSensitive( im3d->vwid->top_shell , False ) ;
   XmUpdateDisplay( im3d->vwid->top_shell ) ;

   LOAD_DSET_VIEWS(im3d) ;  /* 02 Nov 1996 */

   new_daxes.type = DATAXES_TYPE ;

#ifdef USE_WRITEOWNSIZE
   if( im3d->vinfo->writeownsize )
      THD_edit_dataxes( im3d->vinfo->resam_vox , dset->daxes , &new_daxes ) ;
   else
#endif
      new_daxes = *CURRENT_DAXES(im3d->anat_now) ;

   good = AFNI_refashion_dataset( im3d , dset , &new_daxes , resam_mode ) ;

if(PRINT_TRACING){
if( good ){
   STATUS("successful refashioning") ;
} else {
   STATUS("failed refashioning") ;
}}

   if( good && ISFUNC(dset) ){
      AFNI_reset_func_range( im3d ) ;
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;  /* redraw */
   }

   XtSetSensitive( im3d->vwid->top_shell , True ) ;

   XtDestroyWidget( wmsg ) ;
   SHOW_AFNI_READY ;

   /* allow the "use data brick" button, if we just wrote the anat out */

   if( good && w == im3d->vwid->dmode->write_anat_pb ){
STATUS("resetting 'use anat brick' button") ;
      XtSetSensitive( im3d->vwid->dmode->anatmode_bbox->wbut[DMODE_BRICK] , True ) ;
   }

   if( good && w == im3d->vwid->dmode->write_func_pb ){
STATUS("resetting 'use func brick' button") ;
      XtSetSensitive( im3d->vwid->dmode->funcmode_bbox->wbut[DMODE_BRICK] , True ) ;
   }

   if( ! good ){  /* bad news! */

      XBell(im3d->dc->display,100) ;  /* for the fun of it! */
      (void) MCW_popup_message( w ,
                "***************************************\n"
                "** Attempt to write dataset failed   **\n"
                "** for unknown reasons! Check files! **\n"
                "***************************************"  ,
             MCW_USER_KILL ) ;

      dset->wod_flag = True ;
   }

   EXRETURN ;
}

/*------------------------------------------------------------------
   (re)compute and (re)write a dataset to disk, with the indicated
   geometric parameters.  Note that the filenames for the save should
   already be initialized in the dset->dblk->diskptr.  Also, note
   that the dataset's "permanent" dataxes will be remade to fit the
   new geometry.
--------------------------------------------------------------------*/

#define PICTURIZE(px) \
    ( XtVaSetValues( im3d->vwid->picture, XmNlabelInsensitivePixmap,px, NULL ) , \
      MCW_expose_widget( im3d->vwid->picture ) )

#define UNPICTURIZE PICTURIZE(XmUNSPECIFIED_PIXMAP)

/** 09 Dec 1997: resam_mode is now ignored **/

Boolean AFNI_refashion_dataset( Three_D_View *im3d ,
                                THD_3dim_dataset *dset ,
                                THD_dataxes *daxes , int resam_mode )
{
   THD_datablock *dblk  = dset->dblk ;
   THD_diskptr   *dkptr = dset->dblk->diskptr ;
   Boolean good ;
   int npix , nx,ny,nz,nv , kk , ival , code , nzv , dsiz , isfunc , cmode ;
   MRI_IMAGE *im ;
   void *imar ;
   FILE *far ;
   float brfac_save ;
   int native_order , save_order ;  /* 23 Nov 1999 */

   Boolean picturize ;
   Pixmap brain_pixmap=XmUNSPECIFIED_PIXMAP ;

#ifndef DONT_USE_METER
   Widget meter = NULL ;
   int meter_perc , meter_pold ;
#endif

ENTRY("AFNI_refashion_dataset") ;

   picturize = IM3D_OPEN(im3d) && im3d->vwid->picture != NULL &&
               afni48_pixmap != XmUNSPECIFIED_PIXMAP ;

   if( picturize ){
      switch( ORIENT_xyz[daxes->zzorient] ){
         case 'x': brain_pixmap = afni48sag_pixmap     ; break ;
         case 'y': brain_pixmap = afni48cor_pixmap     ; break ;
         case 'z': brain_pixmap = afni48axi_pixmap     ; break ;
      }
   }

#ifndef DONT_USE_METER
   meter = MCW_popup_meter( im3d->vwid->top_shell , METER_TOP_WIDE ) ;
   meter_pold = 0 ;
#endif

   /* set up for warp-on-demand */

   dset->wod_daxes         = myXtNew(THD_dataxes) ; /* 02 Nov 1996 */
   dset->wod_daxes->type   = DATAXES_TYPE ;       /* 02 Nov 1996 */
   dset->vox_warp          = myXtNew(THD_warp) ;    /* 02 Nov 1996 */
   dset->self_warp         = NULL ;              /* 26 Aug 2002 */

   *(dset->wod_daxes)      = *daxes ;            /* copy insides of daxes */
   dset->wod_flag          = True ;              /* mark for warp-on-demand */
   dset->vox_warp->type    = ILLEGAL_TYPE ;      /* mark for recomputation */

   /* copy the new geometric information into various places */

   *(dset->daxes)     = *(daxes) ;               /* Make daxes permanent */
   dkptr->dimsizes[0] = dset->daxes->nxx ;       /* Will cause trouble */
   dkptr->dimsizes[1] = dset->daxes->nyy ;       /* if diskptr and     */
   dkptr->dimsizes[2] = dset->daxes->nzz ;       /* daxes don't match! */

   /* write the header out */

   THD_force_ok_overwrite(1);
   good = THD_write_3dim_dataset( NULL,NULL , dset , False ) ;
   THD_force_ok_overwrite(0);
   if( !good ){
     fprintf(stderr,"\a\n*** cannot write dataset header ***\n") ;
     if( picturize ) UNPICTURIZE ;
     RETURN(False) ;
   }
   STATUS("wrote output header file") ;

   /* purge the datablock that now exists,
      then delete the file on disk that now exists (if any) */

   DSET_unlock( dset ) ; /* Feb 1998 */
   PURGE_DSET( dset ) ;
   COMPRESS_unlink(dkptr->brick_name) ;

   /* refashion its brick data structure,
      which requires first saving in a temporary
      array the datum type for each sub-brick    */

   { int ibr ; int *typ ;
     typ = (int *) XtMalloc( sizeof(int) * dblk->nvals ) ;
     for( ibr=0 ; ibr < dblk->nvals ; ibr++ )
        typ[ibr] = DBLK_BRICK_TYPE(dblk,ibr) ;
     THD_init_datablock_brick( dblk , dblk->nvals , typ ) ;
     myXtFree( typ ) ;
   }

   /*-- 13 Mar 2006: check for free disk space --*/

   { int mm = THD_freemegabytes(dkptr->header_name) ;
     int rr = (int)(dblk->total_bytes/(1024*1024)) ;
     if( rr >= 666 )
       fprintf(stderr,"++ WARNING: output filesize %s will be %d Mbytes!\n"
                      "++ SUGGEST: increase voxel size to save disk space.\n",
               dkptr->brick_name , rr ) ;
     if( mm >= 0 && mm <= rr )
       WARNING_message("Disk space: writing file %s (%d MB),"
                       " but only %d free MB on disk"        ,
               dkptr->brick_name , rr , mm ) ;
   }

   dkptr->storage_mode = STORAGE_UNDEFINED ;       /* just for now */
   dblk->malloc_type   = DATABLOCK_MEM_UNDEFINED ;

   /*--- open the output file
         (N.B.: much of the following code is from THD_write_datablock) ---*/

   /*-- create directory if necessary --*/

   if( ! THD_is_directory(dkptr->directory_name) ){
      kk = mkdir( dkptr->directory_name , THD_MKDIR_MODE ) ;
      if( kk != 0 ){
         fprintf(stderr,
              "\a\n*** cannot mkdir new directory: %s\n",dkptr->directory_name) ;
         if( picturize ) UNPICTURIZE ;
         RETURN(False) ;
      }
      STATUS("created subdirectory") ;
   }

   /*-- open output file --*/

   cmode = THD_get_write_compression() ;
   far = COMPRESS_fopen_write( dkptr->brick_name , cmode ) ;
   if( far == NULL ){
      fprintf(stderr,
        "\a\n*** cannot open output file %s\n",dkptr->brick_name) ;
      if( picturize ) UNPICTURIZE ;
      RETURN(False) ;
   }
   STATUS("created output brick file") ;

   /*--------- now, create each slice and write it out ----------*/

   nx = dset->daxes->nxx ;
   ny = dset->daxes->nyy ;  npix = nx*ny ;
   nz = dset->daxes->nzz ;
   nv = dkptr->nvals     ;  nzv  = nz*nv ;

   isfunc = ISFUNC(dset) ;  /* 09 Dec 1997 */
   if( ! isfunc )
      resam_mode = im3d->vinfo->anat_resam_mode ;

   native_order = mri_short_order() ;                           /* 23 Nov 1999 */
   save_order   = (dkptr->byte_order > 0) ? dkptr->byte_order
                                          : THD_get_write_order() ;

   for( ival=0 ; ival < nv ; ival++ ){  /* for each sub-brick */

      dsiz = mri_datum_size( DSET_BRICK_TYPE(dset,ival) ) ;

      /** force return of unscaled slices for output **/

      brfac_save                   = DBLK_BRICK_FACTOR(dblk,ival) ;
      DBLK_BRICK_FACTOR(dblk,ival) = 0.0 ;

      if( isfunc )
         resam_mode = (DSET_BRICK_STATCODE(dset,ival) > 0)  /* 09 Dec 1997 */
                      ? im3d->vinfo->thr_resam_mode
                      : im3d->vinfo->func_resam_mode ;

      for( kk=0 ; kk < nz ; kk++ ){  /* for each slice */

         im = AFNI_dataset_slice( dset , 3 , kk , ival , resam_mode ) ;
STATUS("have new image") ;

         if( im == NULL ){
            fprintf(stderr,"\a\n*** failure to compute dataset slice %d\n",kk) ;
            COMPRESS_fclose(far) ;
            COMPRESS_unlink( dkptr->brick_name ) ;
            if( picturize ) UNPICTURIZE ;
#ifndef DONT_USE_METER
            MCW_popdown_meter(meter) ;
#endif
            RETURN(False) ;
         }

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"writing slice %d: type=%s nx=%d ny=%d\n",
          kk,MRI_TYPE_NAME(im) , im->nx,im->ny ) ;
  STATUS(str) ; }
#endif

         imar = mri_data_pointer(im) ;
         if( save_order != native_order ){                   /* 23 Nov 1999 */
            switch( im->kind ){
               case MRI_short:   mri_swap2(  npix,imar) ; break ;
               case MRI_float:
               case MRI_int:     mri_swap4(  npix,imar) ; break ;
               case MRI_complex: mri_swap4(2*npix,imar) ; break ;
            }
         }
         code = fwrite( imar , dsiz , npix , far ) ;
         mri_free(im) ;

         if( code != npix ){
            fprintf(stderr,
              "\a\n*** failure to write dataset slice %d (is disk full?)\n",kk) ;
            COMPRESS_fclose(far) ;
            COMPRESS_unlink( dkptr->brick_name ) ;
            if( picturize ) UNPICTURIZE ;
#ifndef DONT_USE_METER
            MCW_popdown_meter(meter) ;
#endif
            RETURN(False) ;
         }

         if( picturize && kk%7 == 0 ){
            Pixmap pp ;
            pp = (kk%2 == 0) ? brain_pixmap : afni48_pixmap ;
            PICTURIZE(pp) ;
            XmUpdateDisplay( im3d->vwid->picture ) ;
         }

#ifndef DONT_USE_METER
         meter_perc = (int) ( 100.0 * (kk+ival*nz) / nzv ) ;
         if( meter_perc != meter_pold ){
            MCW_set_meter( meter , meter_perc ) ;
            meter_pold = meter_perc ;
         }
#endif

      } /* end of loop over kk (z-direction) */

      /* restore the correct scaling of this sub-brick */

      DBLK_BRICK_FACTOR(dblk,ival) = brfac_save ;

   } /* end of loop over iv (nvals direction) */
   STATUS("all slices written") ;

   /*--------------------- done!!! ---------------------*/

   COMPRESS_fclose(far) ;
   STATUS("output file closed") ;

   if( picturize ) PICTURIZE(logo_pixmap) ;
#ifndef DONT_USE_METER
   MCW_set_meter( meter , 100 ) ;
#endif

   /*--- do a little surgery on the dataset's storage flags ---*/

   dkptr->storage_mode = STORAGE_BY_BRICK ;
#if MMAP_THRESHOLD > 0
   dblk->malloc_type   = (dblk->total_bytes > MMAP_THRESHOLD)
                         ? DATABLOCK_MEM_MMAP : DATABLOCK_MEM_MALLOC ;

   if( cmode >= 0 ) dblk->malloc_type = DATABLOCK_MEM_MALLOC ;
   DBLK_mmapfix(dblk) ;  /* 28 Mar 2005 */
#else
   dblk->malloc_type   = DATABLOCK_MEM_MALLOC ;
#endif

   /*--- recompute the statistics and rewrite the header to hold them ---*/

   STATUS("recomputing statistics") ;

   THD_load_statistics( dset ) ;

   STATUS("rewriting header") ;

   tross_Append_History( dset , "AFNI: resampled and rewritten" ) ;
   DSET_overwrite(dset) ;

   STATUS("purging datablock") ;

   PURGE_DSET( dset ) ;

   if( picturize ) UNPICTURIZE ;
#ifndef DONT_USE_METER
   MCW_popdown_meter(meter) ;
#endif

   myXtFree(dset->wod_daxes) ; myXtFree(dset->vox_warp) ;  /* 02 Nov 1996 */

   RETURN(True) ;
}

/*------------------------------------------------------------------
  This routine is part of the inverse of AFNI_make_descendants:
  it will mark for death the descendants of datasets that are about
  to be destroyed themselves (as noted by their "death_marks").

  Note that this routine is not set up for recursive massacres:
  that is, it doesn't try to find grandchildren that should also
  be killed.  That could be done by calling this routine more
  than once, until no changes occur.

  Note also that this routine does not actually destroy any datasets.
  That is done by AFNI_andersonville.

  31 Jan 1995: altered to avoid destruction of a dataset without
               a warp parent, since that dataset cannot be recreated.
  21 Dec 2001: modified to use DSET_MARK_FOR_DEATH(), which will
               not let a dataset marked for immortality be killed.
--------------------------------------------------------------------*/

void AFNI_mark_for_death( THD_sessionlist *ssl )
{
   int iss , jdd , kvv , num_marked=0 ;
   THD_session *ss ;
   THD_3dim_dataset *dset ;

ENTRY("AFNI_mark_for_death") ;

   if( ! ISVALID_SESSIONLIST(ssl) ) EXRETURN ;

   /* loop over each session */

   for( iss=0 ; iss < ssl->num_sess ; iss++ ){
      ss = ssl->ssar[iss] ;
      if( !ISVALID_SESSION(ss) ) continue ;  /* no good ==> skip */

      /* loop over datasets in this session */

      for( jdd=0 ; jdd < ss->num_dsset ; jdd++ ){
         for( kvv=FIRST_VIEW_TYPE ; kvv <= LAST_VIEW_TYPE ; kvv++ ){

            dset = ss->dsset[jdd][kvv] ;

            if( ISVALID_3DIM_DATASET(dset) &&                /* good dset */
                dset->anat_parent != NULL  &&                /* has parent */
                dset->anat_parent->death_mark == DOOMED &&   /* parent dies */
                dset->warp_parent != NULL                 ){ /* is a warp child */

               DSET_MARK_FOR_DEATH(dset) ;
               num_marked ++ ;
            }
         }
      }
   }  /* end of loop over sessions */

#ifdef AFNI_DEBUG
{ char str[256] ;
  sprintf(str,"total # descendants marked = %d",num_marked) ;
  STATUS(str) ; }
#endif

   EXRETURN ;
}

/*-------------------------------------------------------------------
  Mass death
---------------------------------------------------------------------*/

void AFNI_andersonville( THD_sessionlist *ssl , Boolean kill_files )
{
   int iss , jdd , kvv , num_killed=0 ;
   THD_session *ss ;
   THD_3dim_dataset *dset ;
   Boolean kill_me ;

ENTRY("AFNI_andersonville") ;

   if( ! ISVALID_SESSIONLIST(ssl) ) EXRETURN ;

   /* loop over each session */

   for( iss=0 ; iss < ssl->num_sess ; iss++ ){
      ss = ssl->ssar[iss] ;
      if( !ISVALID_SESSION(ss) ) continue ;  /* no good ==> skip */

      if( ss == GLOBAL_library.session ) continue ; /* 21 Dec 2001 */

      /* loop over datasets in this session */

      for( jdd=0 ; jdd < ss->num_dsset ; jdd++ ){
         for( kvv=FIRST_VIEW_TYPE ; kvv <= LAST_VIEW_TYPE ; kvv++ ){

            dset = ss->dsset[jdd][kvv] ;

            if( ISVALID_3DIM_DATASET(dset) &&    /* good dset */
                dset->death_mark == DOOMED   ){  /* alas, poor Yorick */

               kill_me = (kvv == VIEW_ORIGINAL_TYPE) ? False : kill_files ;
               THD_delete_3dim_dataset( dset , kill_me ) ;
               myXtFree( dset ) ;
               ss->dsset[jdd][kvv] = NULL ; num_killed ++ ;
            }
         }
      }

   }  /* end of loop over sessions */

#ifdef AFNI_DEBUG
  { char str[256] ;
    sprintf(str,"total # datasets killed = %d",num_killed) ;
    STATUS(str) ; }
#endif

   EXRETURN ;
}

/*--------------------------------------------------------------*/

void AFNI_imseq_clearstat( Three_D_View *im3d )
{

ENTRY("AFNI_imseq_clearstat") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   if( im3d->s123 != NULL )
      drive_MCW_imseq( im3d->s123 , isqDR_clearstat , NULL ) ;

   if( im3d->s231 != NULL )
      drive_MCW_imseq( im3d->s231 , isqDR_clearstat , NULL ) ;

   if( im3d->s312 != NULL )
      drive_MCW_imseq( im3d->s312 , isqDR_clearstat , NULL ) ;

   EXRETURN ;
}

/****************************************************************
  3/24/95: range controls in func panel for user management
           of the conversion of pbar values [-1..1] to
           thresholds for the coloring of functional data
*****************************************************************/

/*--------------------------------------------------------------
  create label for range display in function control panel
----------------------------------------------------------------*/

XmString AFNI_range_label( Three_D_View *im3d )
{
   char anat_minch[10] = " --------" , anat_maxch[10] = " --------" ,
        fim_minch[10]  = " --------" , fim_maxch[10]  = " --------" ,
        thr_minch[10]  = " --------" , thr_maxch[10]  = " --------"   ;
   char buf[256] , qbuf[16] ;
   XmString xstr ;
   int iv ;

ENTRY("AFNI_range_label") ;

   if( im3d != NULL && im3d->vinfo != NULL ){  /* 30 Mar 2005 */
     im3d->vinfo->stats_anat_ok =
      im3d->vinfo->stats_func_ok =
       im3d->vinfo->stats_thresh_ok = 0 ;
   }

   /*** anat statistics ***/

   if( IM3D_OPEN(im3d) ){
STATUS("RELOAD_STATS(anat_now)") ;
     RELOAD_STATS(im3d->anat_now) ;
   }

   if( IM3D_OPEN(im3d) &&
       ISVALID_3DIM_DATASET(im3d->anat_now) &&
       ISVALID_STATISTIC(im3d->anat_now->stats) ){

     iv = im3d->vinfo->anat_index ;

     if( DSET_VALID_BSTAT(im3d->anat_now,iv) ){
STATUS("anat_now statistics") ;
       AV_fval_to_char( im3d->anat_now->stats->bstat[iv].min , qbuf ) ;
       sprintf( anat_minch , "%9.9s" , qbuf ) ;
       AV_fval_to_char( im3d->anat_now->stats->bstat[iv].max , qbuf ) ;
       sprintf( anat_maxch , "%9.9s" , qbuf ) ;
       im3d->vinfo->stats_anat_ok = 1 ;
     } else {
STATUS("can't load anat_now bstat") ;
     }
   }

   /*** func statistics ***/

   if( IM3D_OPEN(im3d) ){
STATUS("RELOAD_STATS(fim_now)") ;
     RELOAD_STATS(im3d->fim_now) ;
   }

   if( IM3D_OPEN(im3d) &&
       ISVALID_3DIM_DATASET(im3d->fim_now) &&
       ISVALID_STATISTIC(im3d->fim_now->stats) ){

     iv = im3d->vinfo->fim_index ;

     if( DSET_VALID_BSTAT(im3d->fim_now,iv) ){
STATUS("fim_now statistics") ;
       AV_fval_to_char( im3d->fim_now->stats->bstat[iv].min , qbuf ) ;
       sprintf( fim_minch , "%9.9s" , qbuf ) ;
       AV_fval_to_char( im3d->fim_now->stats->bstat[iv].max , qbuf ) ;
       sprintf( fim_maxch , "%9.9s" , qbuf ) ;
       im3d->vinfo->stats_func_ok = 1 ;
     } else {
STATUS("can't load fim_now bstat") ;
     }

     iv = im3d->vinfo->thr_index ;

     if( DSET_VALID_BSTAT(im3d->fim_now,iv) ){
STATUS("thr_now statistics") ;
      AV_fval_to_char( im3d->fim_now->stats->bstat[iv].min , qbuf ) ;
      sprintf( thr_minch , "%9.9s" , qbuf ) ;
      AV_fval_to_char( im3d->fim_now->stats->bstat[iv].max , qbuf ) ;
      sprintf( thr_maxch , "%9.9s" , qbuf ) ;
      im3d->vinfo->stats_thresh_ok = 1 ;
     } else {
STATUS("can't load thr_now bstat") ;
     }
   }

   /*** make label ***/

STATUS("make buf label") ;

   sprintf( buf , "ULay %s:%s\nOLay %s:%s\nThr  %s:%s" ,
            anat_minch,anat_maxch, fim_minch,fim_maxch, thr_minch,thr_maxch ) ;

STATUS(buf) ;

   xstr = XmStringCreateLtoR( buf , XmFONTLIST_DEFAULT_TAG ) ;

   RETURN(xstr) ;
}

/*-----------------------------------------------------------
   find the autorange (default value) and make a label for
   the autorange control widget
-------------------------------------------------------------*/

XmString AFNI_autorange_label( Three_D_View *im3d )
{
   XmString xstr ;
   float rrr ;
   char buf[32] , qbuf[16] ;

ENTRY("AFNI_autorange_label") ;

   if( ! ISVALID_3DIM_DATASET(im3d->fim_now) ){  /* no function */
     rrr = DEFAULT_FIM_SCALE ;
   } else {
     RELOAD_STATS(im3d->fim_now) ;
     if( ISVALID_STATISTIC(im3d->fim_now->stats) ){
       float s1 , s2 ; int iv ;

       iv = im3d->vinfo->fim_index ;

       if( DSET_VALID_BSTAT(im3d->fim_now,iv) ){
         s1  = fabs(im3d->fim_now->stats->bstat[iv].min) ,
         s2  = fabs(im3d->fim_now->stats->bstat[iv].max) ;
         rrr = (s1<s2) ? s2 : s1 ;                      /* largest fim */
       } else {
         rrr = DEFAULT_FIM_SCALE ;                      /* don't have stats */
       }
     } else {
       rrr = DEFAULT_FIM_SCALE ;                        /* don't have stats */
     }
   }
   im3d->vinfo->fim_autorange = rrr ;
   AV_fval_to_char( rrr , qbuf ) ;
   sprintf( buf , "autoRange:%s" , qbuf ) ;
   xstr = XmStringCreateLtoR( buf , XmFONTLIST_DEFAULT_TAG ) ;

#ifdef AFNI_DEBUG
{ STATUS(buf) ;
  sprintf(buf,"rrr=%g",rrr) ; STATUS(buf) ; }
#endif

   RETURN(xstr) ;
}

/*----------------------------------------------------------------
   called when the user toggles the autorange button
------------------------------------------------------------------*/

void AFNI_range_bbox_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   Boolean new_auto ;

ENTRY("AFNI_range_bbox_CB") ;

   if( ! IM3D_VALID(im3d) ||
       w != im3d->vwid->func->range_bbox->wbut[RANGE_AUTOBUT] ) EXRETURN ;

   new_auto = (MCW_val_bbox(im3d->vwid->func->range_bbox) & RANGE_AUTOVAL) != 0 ;

   if( new_auto != im3d->vinfo->use_autorange ){  /* new button value */

      im3d->vinfo->use_autorange = new_auto ;

      im3d->vinfo->fim_range = (new_auto) ? (im3d->vinfo->fim_autorange)
                                          : (im3d->vwid->func->range_av->fval) ;

      AFNI_hintize_pbar( im3d->vwid->func->inten_pbar ,
                         (im3d->vinfo->fim_range != 0.0) ? im3d->vinfo->fim_range
                                                         : im3d->vinfo->fim_autorange );

      AV_SENSITIZE( im3d->vwid->func->range_av , ! new_auto ) ;

      AFNI_redisplay_func( im3d ) ;

      AFNI_range_lock_carryout(im3d) ;  /* 23 Feb 2004 */
   }

   EXRETURN ;
}

/*----------------------------------------------------------------
  called when the user (that rotten fellow) changes the fim range
------------------------------------------------------------------*/

void AFNI_range_av_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_range_av_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   im3d->vinfo->fim_range = av->fval ;
   AFNI_redisplay_func( im3d ) ;

   AFNI_range_lock_carryout(im3d) ;  /* 23 Feb 2004 */

   AFNI_hintize_pbar( im3d->vwid->func->inten_pbar ,
                      (im3d->vinfo->fim_range != 0.0) ? im3d->vinfo->fim_range
                                                      : im3d->vinfo->fim_autorange );
   EXRETURN ;
}

/*----------------------------------------------------------------
   called when the user toggles the posfunc button
------------------------------------------------------------------*/

void AFNI_inten_bbox_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   Boolean new_pos ;
   int jm ;
   MCW_pbar *pbar ;

ENTRY("AFNI_inten_bbox_CB") ;

   if( ! IM3D_VALID(im3d) ||
       w != im3d->vwid->func->inten_bbox->wbut[PBAR_MODEBUT] ) EXRETURN ;

   new_pos = (MCW_val_bbox(im3d->vwid->func->inten_bbox) & PBAR_MODEPOS) != 0 ;

   if( new_pos != im3d->vinfo->use_posfunc ){  /* new button value */

      im3d->vinfo->use_posfunc = new_pos ;   /* record for later use */

      pbar = im3d->vwid->func->inten_pbar ;
      jm   = pbar->mode = (new_pos) ? 1 : 0 ;  /* pbar mode */

      /* re-panel the pbar befitting its new mode */

      HIDE_SCALE(im3d) ;
      if( pbar->bigmode ){               /* 30 Jan 2003 */
        int npane=pbar->num_panes ;
        float pmax=pbar->pval_save[npane][0][jm] ,
              pmin=pbar->pval_save[npane][npane][jm] ;
        pbar->bigset = 0 ;
        PBAR_set_bigmode( pbar , 1 , pmin,pmax ) ;
        AFNI_inten_pbar_CB( pbar , im3d , 0 ) ;
      } else {
        alter_MCW_pbar( pbar , pbar->npan_save[jm] , NULL ) ;
      }
      FIX_SCALE_SIZE(im3d) ;

      /* set the count on the pbar control arrowval to match */

      if( pbar->bigmode )
        AV_assign_ival( im3d->vwid->func->inten_av, NPANE_MAX+1 ) ;
      else
        AV_assign_ival( im3d->vwid->func->inten_av, pbar->npan_save[jm] ) ;

      AFNI_redisplay_func( im3d ) ;
   }

   EXRETURN ;
}

/*--------------------------------------------------------------
   Called to reset the range related functional stuff
----------------------------------------------------------------*/

void AFNI_reset_func_range( Three_D_View *im3d )
{
   XmString xstr ;
   Boolean  same ;

ENTRY("AFNI_reset_func_range") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /*-- the range label widget --*/

   xstr = AFNI_range_label( im3d ) ;
   same = XmStringCompare( xstr , im3d->vinfo->old_range_label ) ;

   if( same == False ){
      XtVaSetValues( im3d->vwid->func->range_label ,    /* redisplay */
                        XmNlabelString , xstr ,         /* if changed */
                     NULL ) ;
      MCW_expose_widget( im3d->vwid->func->range_label ) ; /* redraw now! */
      XmStringFree(im3d->vinfo->old_range_label) ;         /* toss old */
      im3d->vinfo->old_range_label = xstr ;                /* new old */
   } else {
      XmStringFree( xstr ) ;  /* was same --> don't need this copy */
   }

   /*-- the autorange toggle widget --*/

   xstr = AFNI_autorange_label( im3d ) ;
   same = XmStringCompare( xstr , im3d->vinfo->autorange_label ) ;

   if( same == False ){
      Widget www = im3d->vwid->func->range_bbox->wbut[RANGE_AUTOBUT] ;
      XtVaSetValues( www , XmNlabelString , xstr , NULL ) ;
      MCW_expose_widget( www ) ;
      XmStringFree(im3d->vinfo->autorange_label) ;
      im3d->vinfo->autorange_label = xstr ;
   } else {
      XmStringFree( xstr ) ;  /* was same --> don't need this copy */
   }

   /*-- the functional range itself --*/

   im3d->vinfo->fim_range =
      (im3d->vinfo->use_autorange) ? (im3d->vinfo->fim_autorange)
                                   : (im3d->vwid->func->range_av->fval) ;

   HINTIZE_pbar(im3d) ; /* 22 Aug 2001 */

   EXRETURN ;
}

/*--------------------------------------------------------------------
  30 Nov 1997:  Callback for when the a bucket chooser is altered;
                will switch the viewing to a new sub-brick.
----------------------------------------------------------------------*/

void AFNI_bucket_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int doit=0 , iv , redisplay , dothr=0 ;

ENTRY("AFNI_bucket_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   /** Anat sub-brick [29 Jul 2003: lock to time_index controller as well] **/

   if( av == im3d->vwid->func->anat_buck_av ){
     iv = av->ival ;
     if( iv >= 0 && iv < DSET_NVALS(im3d->anat_now) ){
       doit = (iv != im3d->vinfo->anat_index) ;
       im3d->vinfo->anat_index = iv ;
       redisplay = REDISPLAY_ALL ;
       if( doit && im3d->vinfo->time_on )
         AV_assign_ival( im3d->vwid->imag->time_index_av , iv ) ;
     }
   }

   /** Func sub-brick **/

   else if( av == im3d->vwid->func->fim_buck_av ){
     iv = av->ival ;
     if( iv >= 0 && iv < DSET_NVALS(im3d->fim_now) ){
       doit = (iv != im3d->vinfo->fim_index) ;
       im3d->vinfo->fim_index = iv ;
       redisplay = REDISPLAY_OVERLAY ;
       if( doit && im3d->vinfo->time_on && DSET_NUM_TIMES(im3d->anat_now) == 1 )
         AV_assign_ival( im3d->vwid->imag->time_index_av , iv ) ;
     }
   }

   /** Thresh sub-brick **/

   else if( av == im3d->vwid->func->thr_buck_av ){
     iv = av->ival ;
     if( iv >= 0 && iv < DSET_NVALS(im3d->fim_now) ){
       doit = (iv != im3d->vinfo->thr_index) ;
       im3d->vinfo->thr_index = iv ;
       redisplay = REDISPLAY_OVERLAY ;
     }
     dothr = 1 ;
   }

   /** Change the view, if required **/

   if( doit ){
      SHOW_AFNI_PAUSE ;
      im3d->vinfo->tempflag = 1 ;
      AFNI_setup_viewing( im3d , False ) ;
      AFNI_set_viewpoint( im3d , -1,-1,-1 , redisplay ) ; /* redraw */
      if( redisplay == REDISPLAY_OVERLAY &&
          im3d->vinfo->func_visible        ) AFNI_process_funcdisplay(im3d) ;
      SHOW_AFNI_READY ;
   }

   if( dothr && AFNI_yesenv("AFNI_THRESH_AUTO") ){    /* 05 Mar 2007 */
     float new_thresh = AFNI_get_autothresh(im3d) ;
     if( new_thresh > 0.0f ) AFNI_set_threshold(im3d,new_thresh) ;
   }

   FIX_SCALE_SIZE(im3d) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Prepare a bucket label for a menu.
    21 Jun 2004: modified to allow label length to be different from 14.
----------------------------------------------------------------------------*/

char * AFNI_bucket_label_CB( MCW_arrowval *av , XtPointer cd )
{
   static THD_3dim_dataset *dset_last = NULL ;
   static int               nsiz_last = 4 ;

   THD_3dim_dataset *dset = (THD_3dim_dataset *) cd ;
   static char *fmt[3]={NULL,NULL,NULL} , sfmt[16] , blab[48] ;
   int nlab ;
   static int nlab_old = 0 ;

ENTRY("AFNI_bucket_label_CB") ;

   /** 04 May 2005: customize width to this dataset **/

   if( dset != dset_last && ISVALID_DSET(dset) ){
     int nvals,kk,blw,mblw=4 ; char *lab ;
     dset_last = dset ;
     nvals = DSET_NVALS(dset) ;
     for( kk=0 ; kk < nvals ; kk++ ){
       lab = DSET_BRICK_LAB(dset,kk) ;
       if( lab != NULL ){ blw=strlen(lab) ; if(blw>mblw)mblw=blw ; }
     }
     if( mblw > 32 ) mblw = 32 ;
     nsiz_last = mblw ;
   }

   /* see if the environment overrides the above */

#if 0
   nlab = (int)AFNI_numenv("AFNI_BUCKET_LABELSIZE") ;
        if( nlab <= 0 ) nlab = nsiz_last ;
   else if( nlab > 32 ) nlab = 32 ;
#else
   nlab = nsiz_last ;
#endif

   /* make the format for the label string: left justified, width=nlab */

   if( nlab != nlab_old ){
     nlab_old = nlab ;
     sprintf(sfmt,"%%-%d.%ds",nlab,nlab) ;
     if( fmt[0] == NULL ){
       fmt[0] = malloc(32); fmt[1] = malloc(32); fmt[2] = malloc(32);
     }

     /* and now the formats including the sub-brick index and the label */

#ifdef USE_RIGHT_BUCK_LABELS
     sprintf(fmt[0],"%s #%%1d",sfmt) ;   /* if the #xxx goes to the right */
     sprintf(fmt[1],"%s #%%2d",sfmt) ;
     sprintf(fmt[2],"%s #%%3d",sfmt) ;
#else
     sprintf(fmt[0],"#%%1d %s",sfmt) ;   /* if the #xxx goes to the left */
     sprintf(fmt[1],"#%%2d %s",sfmt) ;
     sprintf(fmt[2],"#%%3d %s",sfmt) ;
#endif
   }

   /* now actually make the label for this particular sub-brick */

   if( ISVALID_3DIM_DATASET(dset) ){

#ifdef USE_RIGHT_BUCK_LABELS
     if( DSET_NVALS(dset) < 10 )
       sprintf(blab, fmt[0] , DSET_BRICK_LABEL(dset,av->ival) , av->ival ) ;
     else if( DSET_NVALS(dset) < 100 )
       sprintf(blab, fmt[1] , DSET_BRICK_LABEL(dset,av->ival) , av->ival ) ;
     else
       sprintf(blab, fmt[2] , DSET_BRICK_LABEL(dset,av->ival) , av->ival ) ;
#else
     if( DSET_NVALS(dset) < 10 )
       sprintf(blab, fmt[0] , av->ival , DSET_BRICK_LABEL(dset,av->ival) ) ;
     else if( DSET_NVALS(dset) < 100 )
       sprintf(blab, fmt[1] , av->ival , DSET_BRICK_LABEL(dset,av->ival) ) ;
     else
       sprintf(blab, fmt[2] , av->ival , DSET_BRICK_LABEL(dset,av->ival) ) ;
#endif
   }
   else
     sprintf(blab," #%d ",av->ival) ; /* shouldn't hapeen, but you never know */

   RETURN(blab) ;
}

/*---------------------------------------------------------------
  Callback for all actions in the misc menu
-----------------------------------------------------------------*/

void AFNI_misc_CB( Widget w , XtPointer cd , XtPointer cbd )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) cbd ;

ENTRY("AFNI_misc_CB") ;

   if( !IM3D_OPEN(im3d) || w == NULL ) EXRETURN ;

   /*.........................................................*/

   if( w == im3d->vwid->dmode->misc_voxind_pb ){
      im3d->vinfo->show_voxind = MCW_val_bbox(im3d->vwid->dmode->misc_voxind_bbox);
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
   }

   /*.........................................................*/

#ifndef DONT_USE_HINTS
   else if( w == im3d->vwid->dmode->misc_hints_pb ){
      int val = MCW_val_bbox(im3d->vwid->dmode->misc_hints_bbox) ;
      if( val != GLOBAL_library.hints_on ){
         int ii ; Three_D_View *qq3d ;
         MCW_hint_toggle() ; GLOBAL_library.hints_on = val ;
         for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){             /* this loop:  */
            qq3d = GLOBAL_library.controllers[ii] ;             /* 07 Aug 1999 */
            if( IM3D_VALID(qq3d) )
               MCW_set_bbox( qq3d->vwid->dmode->misc_hints_bbox , val ) ;
         }
      }
   }
#endif

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_anat_info_pb ){
      char *inf ;
STATUS("getting anat info") ;
      inf = THD_dataset_info( im3d->anat_now , 0 ) ;
      if( inf != NULL ){
         if( DSET_ARRAY(im3d->anat_now,0) == NULL ){
            inf = THD_zzprintf( inf , "\n*** Not loaded into memory.\n") ;
         } else if( im3d->anat_now->dblk->malloc_type == DATABLOCK_MEM_MALLOC ){
            if( DBLK_LOCKED(im3d->anat_now->dblk) )
               inf = THD_zzprintf( inf , "\n*** Locked into memory using malloc.\n") ;
            else
               inf = THD_zzprintf( inf , "\n*** Loaded into memory using malloc.\n") ;
         } else if( im3d->anat_now->dblk->malloc_type == DATABLOCK_MEM_MMAP ){
            inf = THD_zzprintf( inf , "\n*** Loaded into memory using mmap.\n") ;
         } else if( im3d->anat_now->dblk->malloc_type == DATABLOCK_MEM_SHARED ){
            inf = THD_zzprintf( inf , "\n*** Loaded into shared memory segment %d.\n",
                                im3d->anat_now->dblk->shm_idint) ;
         }
         (void) new_MCW_textwin( im3d->vwid->imag->topper , inf , TEXT_READONLY ) ;
         free(inf) ;
      } else
         XBell( im3d->dc->display , 100 ) ;
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_func_info_pb ){
      char *inf ;
STATUS("getting func info") ;
      inf = THD_dataset_info( im3d->fim_now , 0 ) ;
STATUS("got func info") ;
      if( inf != NULL ){
         if( DSET_ARRAY(im3d->fim_now,0) == NULL ){
            inf = THD_zzprintf( inf , "\n*** Not loaded into memory.\n") ;
         } else if( im3d->fim_now->dblk->malloc_type == DATABLOCK_MEM_MALLOC ){
            if( DBLK_LOCKED(im3d->fim_now->dblk) )
               inf = THD_zzprintf( inf , "\n*** Locked into memory using malloc.\n") ;
            else
               inf = THD_zzprintf( inf , "\n*** Loaded into memory using malloc.\n") ;
         } else if( im3d->fim_now->dblk->malloc_type == DATABLOCK_MEM_MMAP ){
            inf = THD_zzprintf( inf , "\n*** Loaded into memory using mmap.\n") ;
         } else if( im3d->fim_now->dblk->malloc_type == DATABLOCK_MEM_SHARED ){
            inf = THD_zzprintf( inf , "\n*** Loaded into shared memory segment %d.\n",
                                im3d->fim_now->dblk->shm_idint) ;
         }
         (void) new_MCW_textwin( im3d->vwid->imag->topper , inf , TEXT_READONLY ) ;
         free(inf) ;
      } else
         XBell( im3d->dc->display , 100 ) ;
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_license_pb ){  /* 03 Dec 2000 */
#include "license.h"
      char *inf = NULL ; int ii ;

      for( ii=0 ; license[ii] != NULL ; ii++ )
         inf = THD_zzprintf( inf , "%s" , license[ii] ) ;
      (void) new_MCW_textwin( im3d->vwid->imag->topper , inf , TEXT_READONLY ) ;
      free(inf) ;
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_readme_env_pb ){  /* 05 Aug 2004 */
#include "readme_env.h"
      char *inf = NULL ; int ii ;

      for( ii=0 ; readme_env[ii] != NULL ; ii++ )
        inf = THD_zzprintf( inf , "%s" , readme_env[ii] ) ;
      (void) new_MCW_textwin( im3d->vwid->imag->topper , inf , TEXT_READONLY ) ;
      free(inf) ;
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_motd_pb ){  /* 29 Nov 2005 */
     AFNI_display_motd( im3d->vwid->imag->topper ) ;
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_hist_pb ){  /* 05 Mar 2008 */
     AFNI_display_hist( im3d->vwid->imag->topper ) ;
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_vcheck_pb ){  /* 11 Jan 2000 */
      FILE *fp = popen( "afni_vcheck" , "r" ); int vc;
      if( fp == NULL ){
         (void)  MCW_popup_message( im3d->vwid->imag->topper ,
                                     " \n"
                                     "* Cannot execute *\n"
                                     "* afni_vcheck!   *\n" ,
                                    MCW_USER_KILL | MCW_TIMER_KILL ) ;
         XBell( im3d->dc->display , 100 ) ;
      } else {
#define ISIZE 1024
         char *info=(char *)malloc(sizeof(char)*ISIZE) ; int ninfo ;
         strcpy(info," \n     Output of Program afni_vcheck  \n"
                        "   ---------------------------------\n \n"   ) ;
         ninfo = strlen(info) ;
         while( fgets(info+ninfo,ISIZE-ninfo,fp) != NULL ){
           ninfo = strlen(info) ;
           if( ninfo >= ISIZE-2 ) break ;
         }
         vc = pclose(fp) ;
         ninfo = strlen(info) ;
         if( ninfo+42 < ISIZE ){
                if( vc > 0  ) strcat(info,"\n\n****** VERSIONS DON'T MATCH !! ******\n");
           else if( vc == 0 ) strcat(info,"\n\n****** VERSIONS MATCH OK !! ******\n") ;
         }
         (void) MCW_popup_message( im3d->vwid->imag->topper , info ,
                                   MCW_USER_KILL | MCW_TIMER_KILL   ) ;
         free(info) ;

         if( vc > 0 ) AFNI_vcheck_flasher(im3d) ;
      }
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_purge_pb ){
     long long mb , ma ;
     mb = mcw_malloc_total() ;
     AFNI_purge_dsets( 1 ) ;
     ma = mcw_malloc_total() ;
     if( mb > 0 && ma > 0 )
       INFO_message("Purge: before=%lld  after=%lld  diff=%lld",mb,ma,mb-ma) ;
   }

   /*.........................................................*/

#ifdef USE_TRACING
   else if( w == im3d->vwid->dmode->misc_tracing_pb ){
      DBG_trace = (DBG_trace + 1) % 3 ;  /* Aug 23 1998: cycle between 0,1,2 */
      MCW_set_widget_label( im3d->vwid->dmode->misc_tracing_pb, DBG_label ) ; /* 01 Aug 1999 */

      XSynchronize( im3d->dc->display, (Bool)(DBG_trace==2) ) ; /* 01 Dec 1999 */
      if( DBG_trace == 2 ) STATUS("XSynchronize enabled") ;
      else                 STATUS("XSynchronize disabled") ;
   }
#endif

   /*.........................................................*/

#ifdef USING_MCW_MALLOC   /* 07 Mar 1999 */
   else if( MCW_MALLOC_enabled && w == im3d->vwid->dmode->misc_showmalloc_pb ){
      MCHECK ;
   }

   else if( MCW_MALLOC_enabled && w == im3d->vwid->dmode->misc_dumpmalloc_pb ){
      mcw_malloc_dump() ;
   }
#endif

   /*.........................................................*/

#ifdef USE_WRITEOWNSIZE
   else if( w == im3d->vwid->dmode->misc_writeownsize_pb ){
      im3d->vinfo->writeownsize = MCW_val_bbox( im3d->vwid->dmode->misc_writeownsize_bbox ) ;
   }
#endif

   /*.........................................................*/

#ifdef ALLOW_PLUGINS
   else if( w == im3d->vwid->dmode->misc_environ_pb ){ /* 20 Jun 2000 */
      static PLUGIN_interface *plint=NULL ;
      Widget wpop ;

      /* first time in: create interface like a plugin */

      if( plint == NULL ){
        plint = ENV_init() ;
        if( plint == NULL ){ XBell(im3d->dc->display,100); EXRETURN; }
        PLUG_setup_widgets( plint , GLOBAL_library.dc ) ;
      }

      /* code below is from PLUG_startup_plugin_CB() in afni_plugin.c */

      plint->im3d = im3d ;
      XtVaSetValues( plint->wid->shell ,
                      XmNtitle    , "AFNI Environmentalism", /* top of window */
                      XmNiconName , "Green AFNI"           , /* label on icon */
                     NULL ) ;
      PLUTO_cursorize( plint->wid->shell ) ;

      /*-- if possible, find where this popup should go --*/

      wpop = plint->wid->shell ;

      if( cbs != NULL && cbs->event != NULL
                      && cbs->event->type == ButtonRelease ){

         XButtonEvent *xev = (XButtonEvent *) cbs->event ;
         int xx = (int)xev->x_root , yy = (int)xev->y_root ;
         int ww,hh , sw,sh ;

         MCW_widget_geom( wpop , &ww,&hh , NULL,NULL ) ;
         sw = WidthOfScreen (XtScreen(wpop)) ;
         sh = HeightOfScreen(XtScreen(wpop)) ;

         if( xx+ww+3 >= sw && ww <= sw ) xx = sw-ww ;
         if( yy+hh+3 >= sh && hh <= sh ) yy = sh-hh ;

         XtVaSetValues( wpop , XmNx , xx , XmNy , yy , NULL ) ;
      } else if( im3d->vwid->butx >= 0 && im3d->vwid->buty >= 0 ){
         XtVaSetValues( wpop ,
                         XmNx , im3d->vwid->butx ,
                         XmNy , im3d->vwid->buty , NULL ) ; /* 17 May 2005 */
      }

      /*-- popup widgets --*/

      XtMapWidget( wpop ) ;  /* after this, is up to user */
      AFNI_sleep(1) ;
      RWC_visibilize_widget( wpop ) ;
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_1dchain_pb ){ /* 07 Aug 2001 */
      static PLUGIN_interface *plint=NULL ;
      Widget wpop ;

      /* first time in: create interface like a plugin */

      if( plint == NULL ){
         plint = F1D_init() ;
         if( plint == NULL ){ XBell(im3d->dc->display,100); EXRETURN; }
         PLUG_setup_widgets( plint , GLOBAL_library.dc ) ;
      }

      if( cbs == NULL ) EXRETURN ;  /* only for a setup call */

      /* code below is from PLUG_startup_plugin_CB() in afni_plugin.c */

      plint->im3d = im3d ;
      XtVaSetValues( plint->wid->shell ,
                      XmNtitle     , "AFNI 1DChain Function", /* top of window */
                      XmNiconName  , "1DChain"              , /* label on icon */
                     NULL ) ;
      PLUTO_cursorize( plint->wid->shell ) ;

      /*-- if possible, find where this popup should go --*/

      wpop = plint->wid->shell ;

      if( cbs->event != NULL && cbs->event->type == ButtonRelease ){

         XButtonEvent *xev = (XButtonEvent *) cbs->event ;
         int xx = (int)xev->x_root , yy = (int)xev->y_root ;
         int ww,hh , sw,sh ;

         MCW_widget_geom( wpop , &ww,&hh , NULL,NULL ) ;
         sw = WidthOfScreen (XtScreen(wpop)) ;
         sh = HeightOfScreen(XtScreen(wpop)) ;

         if( xx+ww+3 >= sw && ww <= sw ) xx = sw-ww ;
         if( yy+hh+3 >= sh && hh <= sh ) yy = sh-hh ;

         XtVaSetValues( wpop , XmNx , xx , XmNy , yy , NULL ) ;
      }

      /*-- popup widgets --*/

      XtMapWidget( wpop ) ;  /* after this, is up to user */
      RWC_visibilize_widget( wpop ) ;
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_2dchain_pb ){ /* 20 Jun 2000 */
      static PLUGIN_interface *plint=NULL ;
      Widget wpop ;

      /* first time in: create interface like a plugin */

      if( plint == NULL ){
         plint = F2D_init() ;
         if( plint == NULL ){ XBell(im3d->dc->display,100); EXRETURN; }
         PLUG_setup_widgets( plint , GLOBAL_library.dc ) ;
      }

      if( cbs == NULL ) EXRETURN ;  /* only for a setup call */

      /* code below is from PLUG_startup_plugin_CB() in afni_plugin.c */

      plint->im3d = im3d ;
      XtVaSetValues( plint->wid->shell ,
                      XmNtitle     , "AFNI 2DChain Function", /* top of window */
                      XmNiconName  , "2DChain"              , /* label on icon */
                     NULL ) ;
      PLUTO_cursorize( plint->wid->shell ) ;

      /*-- if possible, find where this popup should go --*/

      wpop = plint->wid->shell ;

      if( cbs->event != NULL && cbs->event->type == ButtonRelease ){

         XButtonEvent *xev = (XButtonEvent *) cbs->event ;
         int xx = (int)xev->x_root , yy = (int)xev->y_root ;
         int ww,hh , sw,sh ;

         MCW_widget_geom( wpop , &ww,&hh , NULL,NULL ) ;
         sw = WidthOfScreen (XtScreen(wpop)) ;
         sh = HeightOfScreen(XtScreen(wpop)) ;

         if( xx+ww+3 >= sw && ww <= sw ) xx = sw-ww ;
         if( yy+hh+3 >= sh && hh <= sh ) yy = sh-hh ;

         XtVaSetValues( wpop , XmNx , xx , XmNy , yy , NULL ) ;
      }

      /*-- popup widgets --*/

      XtMapWidget( wpop ) ;  /* after this, is up to user */
      RWC_visibilize_widget( wpop ) ;
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_plugout_pb ){ /* 07 Nov 2001 */
      AFNI_init_plugouts() ;
      XtSetSensitive(w,False) ;
      if( AFNI_have_niml() )  /* 02 Feb 2007 */
        XtSetSensitive(im3d->vwid->view->nimlpo_pb,False) ;
   }
#endif /* ALLOW_PLUGINS */

   else if( w == im3d->vwid->dmode->misc_niml_pb ){ /* 02 Mar 2002 */
      AFNI_init_niml() ;
      XtSetSensitive(w,False) ;
      if( AFNI_have_plugouts() )  /* 02 Feb 2007 */
        XtSetSensitive(im3d->vwid->view->nimlpo_pb,False) ;
   }

   /*.........................................................*/

   /****----- Get Outta Here -----****/

   EXRETURN ;
}

/*---------------------------------------------------------------*/

void AFNI_editenv_CB( Widget w , XtPointer cd , XtPointer cbd )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   AFNI_misc_CB( im3d->vwid->dmode->misc_environ_pb ,
                 (XtPointer) im3d , (XtPointer) NULL ) ;
}

#ifdef USE_HIDDEN

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  May 1995: Hidden popup menu stuff
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/

#ifndef ALLOW_PLUGINS
# undef WANT_RWCOX_IMAGE
#endif

#ifdef WANT_RWCOX_IMAGE
void RWCOX_popper(void) ;
#endif

#ifdef USE_SKIT
void SKIT_popper( Three_D_View * ) ;
#endif

static int    num_poem  = 0 ;    /* 15 Oct 2003 */
static char **fname_poem=NULL ;
static void AFNI_find_poem_files(void) ;

/*---------------------------------------------------------------
  Callback for all actions in the hidden popup
-----------------------------------------------------------------*/

void AFNI_hidden_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

ENTRY("AFNI_hidden_CB") ;

   if( ! IM3D_OPEN(im3d) || w == (Widget)NULL ) EXRETURN ;

#ifdef ALLOW_DATASET_VLIST
   /****----- Read points -----****/

   if( w == im3d->vwid->prog->hidden_readpts_ijk_pb ||
       w == im3d->vwid->prog->hidden_readpts_xyz_pb   ){

      im3d->vwid->prog->hidden_code =
        (w == im3d->vwid->prog->hidden_readpts_ijk_pb) ? PTS_READ_IJK
                                                       : PTS_READ_XYZ ;

      POPDOWN_ovcolor_chooser ;

      MCW_choose_string( im3d->vwid->picture ,

            (im3d->vwid->prog->hidden_code == PTS_READ_IJK)
              ? "Enter IJK input filename:"
              : "Enter XYZ input filename:" ,

            NULL , AFNI_hidden_pts_CB , cd ) ;
   }

   /****----- Write points -----****/

   else if( w == im3d->vwid->prog->hidden_writepts_ijk_pb ||
            w == im3d->vwid->prog->hidden_writepts_xyz_pb   ){

      if( im3d->anat_now->pts == NULL || im3d->anat_now->pts->num <= 0 ){
         (void)  MCW_popup_message( im3d->vwid->picture ,
                                    "No points to write out!" ,
                                    MCW_USER_KILL | MCW_TIMER_KILL ) ;
         XBell( im3d->dc->display , 100 ) ;
         EXRETURN ;
      }

      POPDOWN_ovcolor_chooser ;

      im3d->vwid->prog->hidden_code =
        (w == im3d->vwid->prog->hidden_writepts_ijk_pb) ? PTS_WRITE_IJK
                                                        : PTS_WRITE_XYZ ;

      MCW_choose_string( im3d->vwid->picture ,

            (im3d->vwid->prog->hidden_code == PTS_WRITE_IJK)
              ? "Enter IJK output filename:"
              : "Enter XYZ output filename:" ,

            NULL , AFNI_hidden_pts_CB , cd ) ;
   }

   /****----- Show points -----****/

   else if( w == im3d->vwid->prog->hidden_colorpts_pb ){

      POPDOWN_string_chooser ;

      im3d->vwid->prog->hidden_code = PTS_SET_COLOR ;

      MCW_choose_ovcolor( im3d->vwid->picture ,
                          im3d->dc ,
                          im3d->vinfo->pts_color ,
                          AFNI_hidden_pts_CB , cd  ) ;
   }
#endif /* ALLOW_DATASET_VLIST */

   /****----- Mission Statement -----****/

   if( w == im3d->vwid->prog->hidden_mission_pb ){
      (void)  MCW_popup_message( im3d->vwid->picture ,
                                    " \n"
                                    " AFNI Mission Statement\n"
                                    " ----------------------\n"
                                    " To empower neuroscientists\n"
                                    " to perform their own state-\n"
                                    " of-the-art data analyses and\n"
                                    " visualizations, so they will \n"
                                    " stop bugging me with their\n"
                                    " pitiful 'quick questions'.\n " ,
                                 MCW_USER_KILL | MCW_TIMER_KILL ) ;
   }

   else if( w == im3d->vwid->prog->hidden_gamberi_pb ){
     AFNI_speak( "The Rime of the Gamberi Cattivi" , 0 ) ;
     (void) MCW_popup_message( im3d->vwid->imag->topper ,
       " \n"
       "       The Rime of the Ancient Gamberi Cattivi\n"
       "       (with apologies to Coleridge and Dante)\n"
       "      -----------------------------------------\n\n"
       " Illuminato dal sole toscano,\n"
       " volsi lo sguardo all'orizzonte;\n"
       " nel mare bagnavo la mano,\n"
       " e al cielo volgevo la fronte.\n"
       " \n"
       " D'improvviso dal mare schiumante,\n"
       " due gamberi killer cattivi si avvicinarono.\n"
       " Uno stiletto mi poser d'innante,\n"
       " mentre i lunghi baffi neri si accarezzavano.\n"
       " \n"
       " Il piu giovane ordino:\n"
       "  \"La macchina fotografica dalla giacca sgancia!\"\n"
       " mentre la lama premeva forte la guancia.\n"
       " Prima di dichiarare \"Del Governo e proprieta,\n"
       " sul mio corpo il nemico passar dovra!\",\n"
       " il gambero malvagio nella bocca la camera si fionda\n"
       " e del Tirreno scuro e profondo ritorna a prender l'onda.\n"
       " \n"
       " Roberto Cox \"il Magnifico\" di Milwaukee e il dottor Emiliano\n"
       " corrono al grido di Ziad, ma e tutto invano.\n"
       " \n"
       " Ai nostri eroi non resta che accettare l'accaduto,\n"
       " e a tutta Italia AFNI e SUMA insegnare.\n"
       " Scusateci se lo stile e un po' caduto,\n"
       " ma e la scusa per l'anno prossimo a Pisa ritornare.\n" ,
                                 MCW_USER_KILL ) ;
   }

   else if( w == im3d->vwid->prog->hidden_ranpoem_pb ){
     static int *dold=NULL, ndold=0 ; int qq,dd ;
     char *poem ;

     if( num_poem < 0 ) EXRETURN ;
     if( num_poem == 0 ){
       AFNI_find_poem_files() ;
       if( num_poem <= 0 ){ num_poem = -1 ; EXRETURN ; }
     }
     if( ndold == 0 && num_poem > 1 ){
       ndold = num_poem/2 ;
       dold  = (int *) malloc(sizeof(int)*ndold) ;
       for( qq=0 ; qq < ndold ; qq++ ) dold[qq] = -1 ;
     }
   Retry_dd:
     dd = (lrand48() >> 8) % num_poem ;              /* pick random file */
     if( num_poem > 1 ){                       /* check if used recently */
       for( qq=0 ; qq < ndold && dold[qq] != dd ; qq++ ) ;       /* nada */
       if( qq < ndold ) goto Retry_dd ;                    /* was recent */
       for( qq=1 ; qq < ndold ; qq++ )        /* wasn't, so save in list */
         dold[qq-1] = dold[qq] ;
       dold[ndold-1] = dd ;
     }
     poem = AFNI_suck_file( fname_poem[dd] ) ;
     if( poem == NULL ) EXRETURN ;
     (void) MCW_popup_message( im3d->vwid->imag->topper, poem, MCW_USER_KILL );
     free( poem ) ;
   }

   /*------ Faces!  [17 Dec 2004] -----*/

   else if( w == im3d->vwid->prog->hidden_faces_pb && w != NULL ){

     AFNI_faceup() ;
   }

   /*------ Splashes!  [12 Sep 2007] -----*/

   else if( w == im3d->vwid->prog->hidden_splashes_pb && w != NULL ){

     AFNI_allsplash() ;
   }

   /*------ Browser [22 Apr 2005] -----*/

   else if( w == im3d->vwid->prog->hidden_browser_pb && w != NULL &&
            GLOBAL_browser != NULL ){

     char cmd[2345] ;
     sprintf(cmd,"%s http://afni.nimh.nih.gov/afni &",GLOBAL_browser) ;
     system(cmd) ;
   }

   /*------- random speaking --------*/

   else if( w == im3d->vwid->prog->hidden_speech_pb && w != NULL ){
     static char *words[] = { "What do you want?"               ,
                              "Nice to see you"                 ,
                              "Words fail me now"               ,
                              "I do not know what to say"       ,
                              "How are you feeling?"            ,
                              "Do you like, afnee?"             ,
                              "Do you use ess, pee, emm?"       ,
                              "Do you use eff, ess, ell?"       ,
                              "Exercise your hippocampus daily"
                            } ;
     if( AFNI_noenv("AFNI_SPEECH") )
       XBell( im3d->dc->display , 100 ) ;
     else {
       static int nold=-1 ;
       int nn = sizeof(words)/sizeof(char *) , jj ;
       do{ jj = lrand48()%nn ; } while( jj == nold ) ;
       AFNI_speak( words[jj] , 1 ) ; nold = jj ;
     }
   }

   /****----- Get Outta Here -----****/

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

static void AFNI_find_poem_files(void)  /* 15 Oct 2003 */
{
   char *epath , *elocal , *eee ;
   char edir[THD_MAX_NAME] , **ename ;
   int epos , ll , ii , id , npoem , nx,ny , nep ;
   char **fpoem ;

ENTRY("AFNI_find_poem_files") ;

   if( num_poem != 0 ) EXRETURN ; /* should never happen */

   /*----- get path to search -----*/

                       epath = getenv("AFNI_PLUGINPATH") ;
   if( epath == NULL ) epath = getenv("AFNI_PLUGIN_PATH") ;
   if( epath == NULL ) epath = getenv("PATH") ;
   if( epath == NULL ){ num_poem=-1; EXRETURN ; }

   /*----- copy path list into local memory -----*/

   ll = strlen(epath) ;
   elocal = AFMALL( char,  sizeof(char) * (ll+2) ) ;

   /*----- put a blank at the end -----*/

   strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

   /*----- replace colons with blanks -----*/

   for( ii=0 ; ii < ll ; ii++ )
      if( elocal[ii] == ':' ) elocal[ii] = ' ' ;


   /*----- extract blank delimited strings;
           use as directory names to look for files -----*/

   ename    = (char **) malloc(sizeof(char *)*2) ;
   ename[0] = (char *)  malloc(THD_MAX_NAME) ;
   ename[1] = (char *)  malloc(THD_MAX_NAME) ;

   epos = 0 ;

   do{
      ii = sscanf( elocal+epos , "%s%n" , edir , &id ); /* next substring */
      if( ii < 1 ) break ;                              /* none -> done   */

      /** check if edir occurs earlier in elocal **/

      eee = strstr( elocal , edir ) ;
      if( eee != NULL && (eee-elocal) < epos ){ epos += id ; continue ; }

      epos += id ;                                 /* char after last scanned */

      ii = strlen(edir) ;                          /* make sure name has   */
      if( edir[ii-1] != '/' ){                     /* a trailing '/' on it */
          edir[ii]  = '/' ; edir[ii+1] = '\0' ;
      }
      strcpy(ename[0],edir) ;
      strcat(ename[0],"poem_*.txt") ;        /* add filename pattern */
      nep = 1 ;

      MCW_file_expand( nep,ename, &npoem , &fpoem );   /* find files that match */
      if( npoem <= 0 ) continue ;                     /* no files found */

      /** add files we found to list **/

      if( fname_poem == NULL )
        fname_poem = (char **)malloc(sizeof(char *)*npoem) ;
      else
        fname_poem = (char **)realloc(fname_poem,sizeof(char *)*(num_poem+npoem));

      for( ii=0 ; ii < npoem ; ii++ )
        fname_poem[num_poem++] = strdup(fpoem[ii]) ;

      MCW_free_expand( npoem , fpoem ) ;

   } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */

   free(elocal) ; free(ename[0]) ; free(ename[1]) ; free(ename) ;

   if( num_poem == 0 ) num_poem = -1 ;
   EXRETURN ;
}

/*-----------------------------------------------------------------
  Event handler to find #3 button press for hidden popup
-------------------------------------------------------------------*/

void AFNI_hidden_EV( Widget w , XtPointer cd ,
                    XEvent *ev , Boolean *continue_to_dispatch )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_hidden_EV") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   /*** handle events ***/

   switch( ev->type ){

      /*----- take button press -----*/

      case ButtonPress:{
         XButtonEvent *event = (XButtonEvent *) ev ;

         if( event->button == Button3 ||
             (event->button == Button1 &&
              (event->state & (ShiftMask|ControlMask))) ){

            im3d->vwid->butx = event->x_root ;  /* 17 May 2005 */
            im3d->vwid->buty = event->y_root ;
            event->button    = Button3 ;                              /* fakeout */
            XmMenuPosition( im3d->vwid->prog->hidden_menu , event ) ; /* where */
            XtManageChild ( im3d->vwid->prog->hidden_menu ) ;         /* popup */
         }
#ifdef WANT_RWCOX_IMAGE
         else if( !NO_frivolities && event->button == Button1 ) RWCOX_popper() ;
#endif

#ifdef USE_SKIT
         else if( !NO_frivolities && event->button == Button2 ) SKIT_popper(im3d) ;
#endif
      }
      break ;

      /*----- take key press [09 Sep 2002] -----*/

      case KeyPress:{
         XKeyEvent *event = (XKeyEvent *) ev ;
         char           buf[32] ;
         KeySym         ks ;
         int            nbuf ;

         buf[0] = '\0' ;
         nbuf = XLookupString( event , buf , 32 , &ks , NULL ) ;

         switch( buf[0] ){
           case 'Q':
             exit(0) ;                         /* Death Now! */

           case 'q':
             AFNI_quit_CB( w , im3d, NULL ) ;  /* Close just this controller */
           break ;

           case 'f':
           case 'F':
             AFNI_faceup() ;                   /* Faces! [27 Dec 2004] */
           break ;

           case 'p':
           case 'P':
             AFNI_hidden_CB( im3d->vwid->prog->hidden_ranpoem_pb ,
                             (XtPointer)im3d , NULL ) ;
           break ;

           case 'g':
           case 'G':
             AFNI_hidden_CB( im3d->vwid->prog->hidden_gamberi_pb ,
                             (XtPointer)im3d , NULL ) ;
           break ;
         }
      }
      break ;
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------*/

#ifdef ALLOW_DATASET_VLIST
void AFNI_hidden_pts_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   THD_3dim_dataset *dset_now ;
   THD_fvec3 xyz_vec ;
   THD_ivec3 ijk_vec ;
   Boolean ijk_option , pause_it ;
   FILE *fil ;
   THD_vector_list *sv ;
   int ii ;

ENTRY("AFNI_hidden_pts_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;
   dset_now = im3d->anat_now ;
   if( ! ISVALID_3DIM_DATASET(dset_now) ) EXRETURN ;

   ijk_option = False ;
   switch( im3d->vwid->prog->hidden_code ){  /* action set by user */

      /*---- Set Color ----*/

      case PTS_SET_COLOR:
         im3d->vinfo->pts_color   = cbs->ival ;
         im3d->vinfo->pts_visible = (cbs->ival > 0) ? True : False ;
         AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;  /* redraw */
      break ;

      /*---- READ pts int ----*/

      case PTS_READ_IJK:
         ijk_option = True ;   /* fall thru on purpose */

      case PTS_READ_XYZ:

         /** open input file **/

         fil = fopen( cbs->cval , "r" ) ;
         if( fil == NULL ){
            char buf[256] ;
            sprintf(buf,"Cannot open file\n %s\nfor reading!",cbs->cval) ;
            (void)  MCW_popup_message( im3d->vwid->picture , buf ,
                                       MCW_USER_KILL | MCW_TIMER_KILL ) ;
            XBell( im3d->dc->display , 100 ) ;
            EXRETURN ;
         }

         POPDOWN_string_chooser ;

         pause_it = ( THD_filesize(cbs->cval) > 99999 ) ;
         if( pause_it ) SHOW_AFNI_PAUSE ;

         /** read points **/

         INIT_VLIST(sv,dset_now) ;
         do {
            if( ijk_option )
               ii = fscanf( fil , " %d %d %d\n",
                     &(ijk_vec.ijk[0]),&(ijk_vec.ijk[1]),&(ijk_vec.ijk[2])) ;
            else
               ii = fscanf( fil , " %f %f %f\n",
                     &(xyz_vec.xyz[0]),&(xyz_vec.xyz[1]),&(xyz_vec.xyz[2])) ;

            if( ii == 3 ){                         /* all 3 good! */
               if( ijk_option ){ ADD_IVEC_TO_VLIST(sv,ijk_vec) ; }
               else            { ADD_FVEC_TO_VLIST(sv,xyz_vec) ; }
            } else if( ii == EOF ){                /* end of data! */
               fclose(fil) ;
               break ;                             /* exit do loop */
            } else {
               char buf[256] ;                     /* bad data! */
               fclose(fil) ;
               sprintf(buf,"Bad read in file\n %s\nat point # %d",
                       cbs->cval , sv->num + 1 ) ;
               (void)  MCW_popup_message( im3d->vwid->picture , buf ,
                                          MCW_USER_KILL | MCW_TIMER_KILL ) ;
               XBell( im3d->dc->display , 100 ) ;
               DESTROY_VLIST(sv) ;
               EXRETURN ;
            }
         } while (1) ;

         /** destroy any point sets in all current datasets **/

         for( ii=0 ; ii <= LAST_VIEW_TYPE ; ii++ ){
            if( ISVALID_3DIM_DATASET(im3d->anat_dset[ii]) ){
               DESTROY_VLIST( im3d->anat_dset[ii]->pts ) ;
               im3d->anat_dset[ii]->pts_original = False ;
            }
         }

         /** put new point set into current dataset **/

         dset_now->pts          = sv ;
         dset_now->pts_original = True ;

         AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
         if( pause_it ) SHOW_AFNI_READY ;
      break ;  /* end of read pts */

      /*---- WRITE pts out ----*/

      case PTS_WRITE_IJK:
         ijk_option = True ;   /* fall thru on purpose */

      case PTS_WRITE_XYZ:

         sv = im3d->anat_now->pts ;
         if( sv == NULL || sv->num == 0 ){
            XBell( im3d->dc->display , 100 ) ;
            EXRETURN ;
         }

         if( cbs->cval[0] == '|' ){  /* send to standard output */
            fil = stdout ;
         } else {
            if( THD_is_file(cbs->cval) ){
               char buf[256] ;
               sprintf(buf,"Desired output file\n %s\nalready exists!",cbs->cval);
               (void)  MCW_popup_message( im3d->vwid->picture , buf ,
                                          MCW_USER_KILL | MCW_TIMER_KILL ) ;
               XBell( im3d->dc->display , 100 ) ;
               EXRETURN ;
            }

            fil = fopen( cbs->cval , "w" ) ;
            if( fil == NULL ){
               char buf[256] ;
               sprintf(buf,"Cannot open file\n %s\nfor writing!",cbs->cval) ;
               (void)  MCW_popup_message( im3d->vwid->picture , buf ,
                                          MCW_USER_KILL | MCW_TIMER_KILL ) ;
               XBell( im3d->dc->display , 100 ) ;
               EXRETURN ;
            }
         }

         POPDOWN_string_chooser ;

         pause_it = ( sv->num > 6666 ) ;
         if( pause_it ) SHOW_AFNI_PAUSE ;

         if( ijk_option ){
            for( ii=0 ; ii < sv->num ; ii++ )
               fprintf(fil,"%d %d %d\n", sv->ijk[ii].ijk[0],
                                         sv->ijk[ii].ijk[1],
                                         sv->ijk[ii].ijk[2]  ) ;
         } else {
            for( ii=0 ; ii < sv->num ; ii++ )
               fprintf(fil,"%g %g %g\n", sv->xyz[ii].xyz[0],
                                         sv->xyz[ii].xyz[1],
                                         sv->xyz[ii].xyz[2]  ) ;
         }

         if( fil != stdout ) fclose(fil) ;
         if( pause_it ) SHOW_AFNI_READY ;
      break ;  /* end of write pts */

   } /* end of switch */

   EXRETURN ;
}
#endif

/*====================================================================================*/
#if defined(WANT_RWCOX_IMAGE) && defined(ALLOW_PLUGINS)

void RWCOX_popper(void)
{
   AFNI_splashup() ;
   return ;
}
#endif /* WANT_RWCOX_IMAGE */
/*====================================================================================*/
#ifdef USE_SKIT
#define NSKIT 50
static char *skit[NSKIT][3] = {
   "artless" ,       "base-court" ,        "apple-john" ,
   "bawdy" ,         "bat-fowling" ,       "baggage" ,
   "beslubbering" ,  "beef-witted" ,       "barnacle" ,
   "bootless" ,      "beetle-headed" ,     "bladder" ,
   "churlish" ,      "boil-brained" ,      "boar-pig" ,
   "cockered" ,      "clapper-clawed" ,    "bugbear" ,
   "clouted" ,       "clay-brained" ,      "bum-bailey" ,
   "craven" ,        "common-kissing" ,    "canker-blossom" ,
   "currish" ,       "crook-pated" ,       "clack-dish" ,
   "dankish" ,       "dismal-dreaming" ,   "clotpole" ,
   "dissembling" ,   "dizzy-eyed" ,        "coxcomb" ,
   "droning" ,       "doghearted" ,        "codpiece" ,
   "errant" ,        "dread-bolted" ,      "death-token" ,
   "fawning" ,       "earth-vexing" ,      "dewberry" ,
   "fobbing" ,       "elf-skinned" ,       "flap-dragon" ,
   "froward" ,       "fat-kidneyed" ,      "flax-wench" ,
   "frothy" ,        "fen-sucked" ,        "flirt-gill" ,
   "gleeking" ,      "flap-mouthed" ,      "foot-licker" ,
   "goatish" ,       "fly-bitten" ,        "fustilarian" ,
   "gorbellied" ,    "folly-fallen" ,      "giglet" ,
   "impertinent" ,   "fool-born" ,         "gudgeon" ,
   "infectious" ,    "full-gorged" ,       "haggard" ,
   "jarring" ,       "guts-griping" ,      "harpy" ,
   "loggerheaded" ,  "half-faced" ,        "hedge-pig" ,
   "lumpish" ,       "hasty-witted" ,      "horn-beast" ,
   "mammering" ,     "hedge-born" ,        "hugger-mugger" ,
   "mangled" ,       "hell-hated" ,        "joithead" ,
   "mewling" ,       "idle-headed" ,       "lewdster" ,
   "paunchy" ,       "ill-breeding" ,      "lout" ,
   "pribbling" ,     "ill-nurtured" ,      "maggot-pie" ,
   "puking" ,        "knotty-pated" ,      "malt-worm" ,
   "puny" ,          "milk-livered" ,      "mammet" ,
   "qualling" ,      "motley-minded" ,     "measle" ,
   "rank" ,          "onion-eyed" ,        "minnow" ,
   "reeky" ,         "plume-plucked" ,     "miscreant" ,
   "roguish" ,       "pottle-deep" ,       "moldwarp" ,
   "ruttish" ,       "pox-marked" ,        "mumble-news" ,
   "saucy" ,         "reeling-ripe" ,      "nut-hook" ,
   "spleeny" ,       "rough-hewn" ,        "pigeon-egg" ,
   "spongy" ,        "rude-growing" ,      "pignut" ,
   "surly" ,         "rump-fed" ,          "puttock" ,
   "tottering" ,     "shard-borne" ,       "pumpion" ,
   "unmuzzled" ,     "sheep-biting" ,      "ratsbane" ,
   "vain" ,          "spur-galled" ,       "scut" ,
   "venomed" ,       "swag-bellied" ,      "skainsmate" ,
   "villainous" ,    "tardy-gaited" ,      "strumpet" ,
   "warped" ,        "tickle-brained" ,    "varlet" ,
   "wayward" ,       "toad-spotted" ,      "vassal" ,
   "weedy" ,         "unchin-snouted" ,    "whey-face" ,
   "yeasty" ,        "weather-bitten" ,    "wagtail"
} ;

static char skstr[512] ;

void SKIT_popper(Three_D_View *im3d)
{
   int ii,jj,kk ;

   ii = lrand48() % NSKIT ; jj = lrand48() % NSKIT ; kk = lrand48() % NSKIT ;
   sprintf(skstr,"Randomly generated Shakespearean Insult:\n\n"
                 " Thou %s %s %s! \n" ,
           skit[ii][0] , skit[jj][1] , skit[kk][2] ) ;

   if( lrand48()%7 == 0 ){
      ii = strlen(skstr) ;
      sprintf( skstr+ii , "\n  [Data provided by Sandy Kindermann]" ) ;
   }

   (void)  MCW_popup_message( im3d->vwid->picture , skstr ,
                              MCW_USER_KILL | MCW_TIMER_KILL ) ;
}
#endif /* USE_SKIT */
/*====================================================================================*/

#endif /* USE_HIDDEN */
