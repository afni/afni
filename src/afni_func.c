/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#undef MAIN

#include "afni.h"
#include "afni_plugout.h"
extern char **Atlas_Names_List(ATLAS_LIST *atl);

static THD_3dim_dataset *atlas_ovdset = NULL;

static float Thval[9] = { 1.0 , 10.0 , 100.0 , 1000.0 , 10000.0 ,
                         100000.0 , 1000000.0 , 10000000.0 , 100000000.0 } ;

/*-------------------------------------------------------------------
   This routine is also used by the macros
      AFNI_SEE_FUNC_ON and AFNI_SEE_FUNC_OFF
   as well as the MCW_bbox that it is attached to as the callback.
---------------------------------------------------------------------*/

void AFNI_see_func_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   int old_val , new_val ;

ENTRY("AFNI_see_func_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   old_val = (im3d->vinfo->func_visible) ? 1 : 0 ;
   new_val = MCW_val_bbox( im3d->vwid->view->see_func_bbox ) ;

   if( old_val != new_val ){
#if 0
     STATUS_IM3D_TMASK(im3d) ;
     STATUS("clear tmask") ;
#endif
     IM3D_CLEAR_TMASK(im3d) ;                                   /* Mar 2013 */
     IM3D_CLEAR_THRSTAT(im3d) ;                              /* 12 Jun 2014 */
     im3d->vinfo->func_visible = (new_val == 1) ? True : False ;
     if( ! ISVALID_3DIM_DATASET(im3d->fim_now) ){            /* 29 Apr 1997 */
       im3d->vinfo->func_visible = False ; new_val = 0 ;
       MCW_set_bbox( im3d->vwid->view->see_func_bbox , 0 ) ; /* 29 Jan 1999 */
     }
     IM3D_CLEAR_THRSTAT(im3d) ;          /* 12 Jun 2014 */
     OVERLAY_SUMA ;                      /* 16 Jun 2003 */
     AFNI_redisplay_func( im3d ) ;       /* 05 Mar 2002 */
     im3d->vinfo->func_visible_count++ ; /* 03 Aug 2007 */
     if( new_val == 0 ){                 /* 21 Jul 2014 */
       XtSetSensitive(im3d->vwid->func->pbar_jumpto_thmax_pb,False) ;
       XtSetSensitive(im3d->vwid->func->pbar_jumpto_thmin_pb,False) ;
     }
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Callback for "A" and "B" buttons on top of threshold slider [02 Nov 2018] */
/*---------------------------------------------------------------------------*/

void AFNI_func_thrtop_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_thrtop_CB") ;
   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   if( w == im3d->vwid->func->thrtop_alpha_pb ){

     MCW_invert_widget(w) ;
     im3d->vinfo->thr_use_alpha = !im3d->vinfo->thr_use_alpha ;
     PBAR_force_bigexpose(im3d->vwid->func->inten_pbar) ;

   } else if( w == im3d->vwid->func->thrtop_boxed_pb ){

     MCW_invert_widget(w) ;
     im3d->vinfo->thr_use_boxed = !im3d->vinfo->thr_use_boxed ;

   }

   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ;
   AFNI_redisplay_func(im3d) ;
   AFNI_set_window_titles(im3d) ;

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

   if( !IM3D_OPEN(im3d) || !ISVALID_DSET(im3d->fim_now) ) RETURN(-1.0f) ;

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

void AFNI_func_autothresh_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   float new_thresh ;

ENTRY("AFNI_func_autothresh_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

#if 0
   STATUS_IM3D_TMASK(im3d) ;
   STATUS("clear tmask") ;
#endif
   IM3D_CLEAR_TMASK(im3d) ;                                /* Mar 2013 */
   IM3D_CLEAR_THRSTAT(im3d) ;                           /* 12 Jun 2014 */
   new_thresh = AFNI_get_autothresh(im3d) ;
   if( new_thresh > 0.0f ) AFNI_set_threshold(im3d,new_thresh) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! 03 Dec 2013 */

#define TFLASH(iq) \
  do{ MCW_flash_widget(2,(iq)->vwid->func->thr_scale); BEEPIT; } while(0)

void AFNI_set_pval( Three_D_View *im3d , float pval )
{
   float thresh ; int sig , scode ;

ENTRY("AFNI_set_pval") ;

   if( !IM3D_OPEN(im3d) || pval <= 0.0f || pval >= 1.0f ) EXRETURN ;
   if( DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) <= 0 ){
     TFLASH(im3d) ; EXRETURN ;
   }

   scode = DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) ;
   sig   = THD_stat_is_2sided( scode , 0 ) ;
   if( sig > 0 && im3d->vinfo->thr_sign > 0 && scode != FUNC_FT_TYPE )
     pval *= 2.0f ;  /* Jan 2015 */

   thresh = THD_pval_to_stat( pval ,
              DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) ,
              DSET_BRICK_STATAUX (im3d->fim_now,im3d->vinfo->thr_index)  ) ;
   if( thresh < 0.0 ){ TFLASH(im3d); EXRETURN; }

   AFNI_set_threshold(im3d,thresh) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void AFNI_func_setpval_final_CB( Widget w, XtPointer cd, MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   float pval , thresh ;
   int newdec , olddec , stop,smax , ival ;
   char *cpt ;

ENTRY("AFNI_func_setpval_final_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   if( cbs->reason  != mcwCR_string ||
       cbs->cval    == NULL         ||
       cbs->cval[0] == '\0'           ){ TFLASH(im3d); EXRETURN; }

   if( DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) <= 0 ){
     TFLASH(im3d) ; EXRETURN ;
   }

   pval = (float)strtod(cbs->cval,&cpt) ;
   if( pval >  0.0f && *cpt == '%'  ) pval *= 0.01f ;
   if( pval <= 0.0f || pval >= 1.0f ){ TFLASH(im3d); EXRETURN; }

   im3d->vinfo->fix_qval   = 0 ;
   im3d->vinfo->fixed_qval = 0.0f ;

   AFNI_set_pval(im3d,pval) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void AFNI_func_setpval_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

ENTRY("AFNI_func_setpval_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   MCW_choose_string( w, "Enter p-value", NULL, AFNI_func_setpval_final_CB,cd ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void AFNI_func_setpval_001_CB( Widget w, XtPointer cd, XtPointer cb )
{
   MCW_choose_cbs cbs ;

ENTRY("AFNI_func_setpval_001_CB") ;

   cbs.reason = mcwCR_string ;
   cbs.cval   = "0.001" ;
   AFNI_func_setpval_final_CB( w , cd , &cbs ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! 26 Feb 2014 */

void AFNI_set_qval( Three_D_View *im3d , float qval )
{
   float zval , thresh ;

ENTRY("AFNI_set_qval") ;

   if( !IM3D_OPEN(im3d) || qval <= 0.0f || qval >= 1.0f ) EXRETURN ;
   if( DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) <= 0 ){
     TFLASH(im3d) ; EXRETURN ;
   }

   zval   = qginv(0.5*qval) ;
   thresh = THD_fdrcurve_zqtot( im3d->fim_now , im3d->vinfo->thr_index , zval ) ;
   if( thresh <= 0.0 ){ TFLASH(im3d); EXRETURN; }

   AFNI_set_threshold(im3d,thresh) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

static char *yesno[2] = { "No" , "Yes" } ;

void AFNI_func_setqval_final_CB( Widget w, XtPointer cd, int nval , void **val )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   float qval , zval , thresh ;
   char *cpt ;

ENTRY("AFNI_func_setqval_final_CB") ;

   if( !IM3D_OPEN(im3d) || nval != 2 || val == NULL ) EXRETURN ;

   if( DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) <= 0 ){
    TFLASH(im3d) ; EXRETURN ;
   }

   qval = (float)strtod((char *)val[0],&cpt) ;
   if( qval > 0.0f && *cpt == '%'  ) qval *= 0.01f ;
   if( qval < 0.0f || qval >= 1.0f ){ TFLASH(im3d); EXRETURN; }

   im3d->vinfo->fix_qval   = ( qval > 0.0f && strcmp((char *)val[1],yesno[1]) == 0 ) ;
   im3d->vinfo->fixed_qval = (im3d->vinfo->fix_qval) ? qval : 0.0f ;

   im3d->vinfo->fix_pval   = 0 ;
   im3d->vinfo->fixed_pval = 0.0f ;

   if( qval > 0.0f ) AFNI_set_qval(im3d,qval) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void AFNI_func_setqval_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   int ifix ;

ENTRY("AFNI_func_setqval_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   ifix = (im3d->vinfo->fix_qval) ? 1 : 0 ;

   MCW_choose_stuff( w , "FDR q-value Settings" ,
                     AFNI_func_setqval_final_CB , im3d ,
                       MSTUF_STRING  , "Set q-value"  ,
                       MSTUF_STRLIST , "Keep fixed? " , 2 , ifix , yesno ,
                     MSTUF_END ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! 29 Jan 2008: add FDR curves to the functional dataset */

void AFNI_func_fdr_CB( Widget w, XtPointer cd, XtPointer cb )
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
#if 0
   STATUS_IM3D_TMASK(im3d) ;
   STATUS("clear tmask") ;
#endif
   IM3D_CLEAR_TMASK(im3d) ;                                /* Mar 2013 */
   IM3D_CLEAR_THRSTAT(im3d) ;                           /* 12 Jun 2014 */
   AFNI_set_thr_pval( im3d ) ;                             /* Jan 2015 */
   AFNI_redisplay_func( im3d ) ;
   AFNI_set_window_titles( im3d ) ;
   EXRETURN ;
}

#if 0 /*** old alpha stuff, now replaced by the 'A' button ***/
/*-----------------------------------------------------------------------*/
/*! 08 Dec 2014 */

void AFNI_func_alpha_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

ENTRY("AFNI_func_alpha_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   im3d->vinfo->thr_use_alpha = av->ival ;
   PBAR_force_bigexpose(im3d->vwid->func->inten_pbar) ;

#if 0
   STATUS_IM3D_TMASK(im3d) ;
   STATUS("clear tmask") ;
#endif
   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ;
   AFNI_redisplay_func( im3d ) ;
   AFNI_set_window_titles( im3d ) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/*! 09 Dec 2014 */

void AFNI_func_floor_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

ENTRY("AFNI_func_floor_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   im3d->vinfo->thr_alpha_floor = av->ival * 0.2f ;
#if 0
   STATUS_IM3D_TMASK(im3d) ;
   STATUS("clear tmask") ;
#endif
   IM3D_CLEAR_TMASK(im3d) ;
   IM3D_CLEAR_THRSTAT(im3d) ;
   AFNI_redisplay_func( im3d ) ;
   AFNI_set_window_titles( im3d ) ;
   EXRETURN ;
}
#endif

/*-----------------------------------------------------------------------*/
/*! Set the threshold and slider.  [05 Mar 2007]
-------------------------------------------------------------------------*/

void AFNI_set_threshold( Three_D_View *im3d , float val )
{
   int olddec,newdec , smax,stop , ival ;

ENTRY("AFNI_set_threshold") ;

   if( !IM3D_OPEN(im3d) || val < 0.0f || val > THR_top_value ) EXRETURN;

   /* get current scale decimal setting */

   olddec = (int)rint( log10(im3d->vinfo->func_thresh_top) ) ;
        if( olddec < 0             ) olddec = 0 ;
   else if( olddec > THR_top_expon ) olddec = THR_top_expon ;
   newdec = olddec ;

   if( val > 0.0f ){
     newdec = (int)( log10(val) + 1.0 ) ;
          if( newdec < 0             ) newdec = 0 ;
     else if( newdec > THR_top_expon ) newdec = THR_top_expon ;
     if( newdec != olddec )
       AFNI_set_thresh_top( im3d , Thval[newdec] ) ;
   }

   smax  = (int)rint( pow(10.0,THR_top_expon) ) ;
   stop  = smax - 1 ;                             /* max slider value */

   ival = rint( val/(THR_factor*Thval[newdec]) ) ;
        if( ival < 0    ) ival = 0    ;
   else if( ival > stop ) ival = stop ;

#if 0
   STATUS_IM3D_TMASK(im3d) ;
   STATUS("clear tmask") ;
#endif
   IM3D_CLEAR_TMASK(im3d) ;                                /* Mar 2013 */
   IM3D_CLEAR_THRSTAT(im3d) ;                           /* 12 Jun 2014 */
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
   int ival ;

ENTRY("AFNI_thr_scale_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   if( cbs != NULL ) ival = cbs->value ;
   else              XmScaleGetValue( w , &ival ) ;

   fff = THR_factor * ival ;
   if( fff >= 0.0 && fff <= 1.0 ) im3d->vinfo->func_threshold = fff ;

   FIX_SCALE_VALUE(im3d) ;
   FIX_SCALE_SIZE(im3d) ;   /* 09 May 2001 */

   AFNI_set_thr_pval( im3d ) ;

   MCW_discard_events_all( w , ButtonPressMask ) ;  /* 20 Mar 2007 */

#if 0
   STATUS_IM3D_TMASK(im3d) ;
   STATUS("clear tmask") ;
#endif
   IM3D_CLEAR_TMASK(im3d) ;                                /* Mar 2013 */
   IM3D_CLEAR_THRSTAT(im3d) ;                           /* 12 Jun 2014 */
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

      fff = THR_factor * cbs->value ;
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

   decim = (2*THR_top_expon) - (int)(THR_top_expon + 0.01 + log10(tval)) ;
   if( decim < 0 ) decim = 0 ;

   XtVaSetValues( im3d->vwid->func->thr_scale, XmNdecimalPoints, decim, NULL ) ;

   im3d->vinfo->func_thresh_top = tval ;

#if 0
   STATUS_IM3D_TMASK(im3d) ;
   STATUS("clear tmask") ;
#endif
   IM3D_CLEAR_TMASK(im3d) ;                                /* Mar 2013 */
   IM3D_CLEAR_THRSTAT(im3d) ;                           /* 12 Jun 2014 */
   FIX_SCALE_VALUE(im3d) ;
   FIX_SCALE_SIZE(im3d) ;   /* 09 May 2001 */
   AFNI_set_thr_pval( im3d ) ;

   /** fix the option menu at the bottom of the scale **/

   decim = THR_top_expon - decim ;
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

ENTRY("AFNI_thresh_top_CB") ;

   if( IM3D_OPEN(im3d) && im3d->vinfo->func_thresh_top != Thval[av->ival] ){

     AFNI_set_thresh_top( im3d , Thval[av->ival] ) ;
     FIX_SCALE_SIZE(im3d) ;

     if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;

     AFNI_thresh_lock_carryout(im3d) ;  /* 06 Feb 2004 */
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
/* Something silly from Ziad */

float AFNI_thresh_from_percentile( Three_D_View *im3d, float perc)
{
   float *fv, thresh=0.0;
   int ithr;

   if (!(fv = get_3Dview_sort(im3d, "T"))) return(perc);

   ithr = (int)(perc*(float)(im3d->vinfo->N_th_sort-1)+0.5);
   if (ithr < 0) {
      thresh = fv[0]-1.0;
   } else if (ithr > im3d->vinfo->N_th_sort) {
      thresh = fv[im3d->vinfo->N_th_sort-1]+1.0;
   } else {
      thresh = fv[ithr];
   }

   #if 0
   INFO_message(  "Top val %f, bottom val %f\n"
                  "Sorting on set of %d voxels out of %d voxels in grid.\n"
                  "Thresholding bottom %f%% thresh=%f, ithr=%d.\n"
                  ,
                   fv[im3d->vinfo->N_th_sort-1], fv[0],
                   im3d->vinfo->N_th_sort, DSET_NVOX(im3d->fim_now),
                   100*perc, thresh, ithr
                   );
   #endif
   return(thresh);
}

/*-----------------------------------------------------------------------
  Used to set the pval (significance) label at the bottom of the
  threshold scale.  And the qval (FDR) label.
-------------------------------------------------------------------------*/

void AFNI_set_thr_pval( Three_D_View *im3d )
{
   float thresh , pval , zval , spval ;
   char  buf[32] ; int sig , scode ;

ENTRY("AFNI_set_thr_pval") ;

   if( ! IM3D_VALID(im3d) || ! ISVALID_3DIM_DATASET(im3d->fim_now) ) EXRETURN ;

   thresh = get_3Dview_func_thresh(im3d,1);

   /* get the p-value that goes with this threshold, for this functional dataset */

   pval = THD_stat_to_pval( thresh ,
              DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) ,
              DSET_BRICK_STATAUX (im3d->fim_now,im3d->vinfo->thr_index)  ) ;

   /* modify it if the threshold statistic is 2-sided but we are thresholding 1-sided */

   scode = DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) ;
   if( im3d->vinfo->thr_sign == 0 || scode == FUNC_FT_TYPE )
     sig = 0 ;
   else
     sig = THD_stat_is_2sided( scode , 0 ) ;
   spval = pval ; if( sig > 0 ) spval *= 0.5f ;             /* Jan 2015 */

   im3d->vinfo->func_pval = spval ;  /* 06 Feb 2004 -- changed to spval Jan 2015 */
   im3d->vinfo->func_qval = 0.0f  ;  /* 23 Jan 2008 */

if(PRINT_TRACING)
{ char buf[128] ;
  sprintf( buf, "thresh=%g  top=%g  pval=%g",
           thresh,im3d->vinfo->func_thresh_top,spval ) ; STATUS(buf) ; }

   if( pval < 0.0 ){
     strcpy( buf , THR_PVAL_LABEL_NONE ) ;
   } else {
     if( spval == 0.0 ){
       strcpy( buf , "p=0" ) ;
     } else if( spval >= 0.9999 ){
       strcpy( buf , "p=1" ) ;
     } else if( spval >= 0.0010 ){
       char qbuf[16] ;
       sprintf( qbuf , "%5.4f" , spval ) ;
       strcpy(buf,"p=") ; strcat( buf , qbuf+1 ) ; /* qbuf+1 skips leading 0 */
     } else {
       int dec = (int)(0.999 - log10(spval)) ;
       zval = spval * pow( 10.0 , (double) dec ) ;  /* between 1 and 10 */
       if( dec < 10 ) sprintf( buf , "p=%3.1f-%1d" ,           zval , dec ) ;
       else           sprintf( buf , "p=%1d.-%2d"  , (int)rint(zval), dec ) ;
     }
     if( im3d->vedset.code > 0 && im3d->fim_now->dblk->vedim != NULL )
       strcat(buf,"*") ;  /* mark that are in clustering mode [05 Sep 2006] */
     else if( im3d->vinfo->fix_pval && fabsf(im3d->vinfo->fixed_pval-pval)<1.e-4f )
       strcat(buf,"f") ;
   }

   /* 23 Jan 2007: q-value from FDR curve? */

   if( pval >= 0.0 ){
     zval = THD_fdrcurve_zval( im3d->fim_now, im3d->vinfo->thr_index, thresh ) ;
     if( zval > 0.0f ){
       float qval = 2.0*qg(zval) ;         /* convert z back to FDR q */
       im3d->vinfo->func_qval = qval ;
       if( qval > 0.0f && qval < 0.9999 ){
         char qbuf[16] ;
         if( qval >= 0.0010 ) sprintf(qbuf,"%5.4f",qval) ;
         else {
           int dec = (int)(0.999 - log10(qval)) ;
           zval = qval * pow( 10.0 , (double)dec ) ;  /* between 1 and 10 */
           if( dec < 10 ) sprintf( qbuf, " %3.1f-%1d",            zval, dec );
           else           sprintf( qbuf, " %1d.-%2d" , (int)rint(zval), dec );
         }
         strcat(buf,"\nq=") ; strcat(buf,qbuf+1) ;
         if( im3d->vinfo->fix_qval && fabsf(im3d->vinfo->fixed_qval-qval)<1.e-4f )
           strcat(buf,"f") ;
       }
     } else {
       strcat(buf,"\nq=N/A") ;
     }
   }

   MCW_set_widget_label( im3d->vwid->func->thr_pval_label , buf ) ;

   if( im3d->vinfo->func_pval >= 0.0f && im3d->vinfo->func_pval <= 1.0f ){
#define EEEE 2.718282f /* e */
#define EINV 0.367879f /* 1/e */
     char pstr[128] ; float mval ;
     sprintf( pstr , "Uncorrected p=%.4e" , im3d->vinfo->func_pval ) ;
     if( im3d->vinfo->func_pval > 0.0f && im3d->vinfo->func_pval < EINV &&
         THD_stat_is_2sided( DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index),
                             im3d->vinfo->thr_sign) > 0 )
       sprintf(pstr+strlen(pstr)," alpha(p)=%.4e",
               1/(1-1/(EEEE*im3d->vinfo->func_pval*log(im3d->vinfo->func_pval))) ) ;
     if( im3d->vinfo->func_qval >= 0.0f && im3d->vinfo->func_qval <= 1.0f )
       sprintf(pstr+strlen(pstr),"; FDR q=%.4e",im3d->vinfo->func_qval) ;
     else
       sprintf(pstr+strlen(pstr),"; FDR q=N/A") ;
     mval = THD_mdfcurve_mval( im3d->fim_now, im3d->vinfo->thr_index ,
                                              im3d->vinfo->func_pval  ) ;
#if 1
     if( mval >= 0.0f )
       sprintf(pstr+strlen(pstr),"; MDF=%.1f%%",100.0f*mval) ;
     else
       strcat(pstr,"; MDF=N/A") ;
#else
       sprintf(pstr+strlen(pstr),"; MDF=%.1f%%",100.0f*mval) ;
#endif
     MCW_register_hint( im3d->vwid->func->thr_pval_label , pstr ) ;
   } else {
     MCW_register_hint( im3d->vwid->func->thr_pval_label ,
                        "Uncorrected p-value per voxel"    ) ;
   }

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

   if( pbar == NULL || fac == 0.0f ) EXRETURN ;  /* bad */

   if( PBAR_FULLRANGE ) fac = 1.0f ;     /* 03 Jun 2014 */

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
   Three_D_View *im3d = (Three_D_View *)cd ;
   float fac ;

ENTRY("AFNI_inten_pbar_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   if( PBAR_FULLRANGE && !IM3D_ULAY_COHERENT(im3d) ){ /* 10 Jun 2014 */
     STATUS("incoherent ulay -- patching") ;
     ERROR_message("AFNI_inten_pbar_CB: incoherent ulay -- patching") ;
     AFNI_assign_ulay_bricks(im3d) ;
   }

#if 0
   STATUS_IM3D_TMASK(im3d) ;
   STATUS("clear tmask") ;
#endif
   IM3D_CLEAR_TMASK(im3d) ;                                /* Mar 2013 */
   IM3D_CLEAR_THRSTAT(im3d) ;                           /* 12 Jun 2014 */

#if 0
INFO_message("AFNI_inten_pbar_CB(%d)",AFNI_controller_index(im3d)) ;
#endif
   if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;

   AFNI_hintize_pbar( pbar , FIM_RANGE(im3d) ) ;

   FIX_SCALE_SIZE(im3d) ;

   AFNI_pbar_lock_carryout(im3d) ; /* 07 Feb 2004 */
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

#if 0
void AFNI_iab_pbar_CB( MCW_pbar *pbar , XtPointer cd , int reason )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

ENTRY("AFNI_iab_pbar_CB") ;

   if( pbar == NULL || !pbar->three_level || !IM3D_OPEN(im3d) ) EXRETURN ; /* error */

   switch( reason ){

     case pbCR_VALUE:{
       float top,bot , ntop,nbot ;
       ntop = top = pbar->pval[1] ; nbot = bot = pbar->pval[2] ;
       switch( im3d->vwid->func->iab_bot_av->ival ){
         case 1:          /* Bot = -Top */
           if( top > 0.0f ) nbot = -top ; else ntop = -bot ;
         break ;
         case 0:          /* Bot = 0 */
           if( top <= 0.0f ) ntop = -bot ;
           nbot = 0.0f ;
         break ;
       }

       if( ntop != top || nbot != bot ){
         float pval[4] ;
         pval[0] = pbar->pval[0] ; pval[3] = pbar->pval[3] ;
         pval[1] = ntop          ; pval[2] = nbot          ;
         alter_MCW_pbar( pbar , 0 , pval ) ;
       }
     }
     break ;

     case pbCR_COLOR: /* TBD */
     break ;

   }

   FIX_SCALE_SIZE(im3d) ; EXRETURN ;
}

/*----------------------------------------------------------------------------*/

void AFNI_iab_av_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   MCW_pbar *pbar = im3d->vwid->func->iab_pbar ;

ENTRY("AFNI_iab_av_CB") ;

   if( pbar == NULL ) EXRETURN ; /* error */

   if( av == im3d->vwid->func->iab_pow_av ){
     float pval[4] , fac ;
     fac = powf(10.0f,(float)av->ival) ;
     pval[0] =  1.2f*fac ; pval[1] =  1.0f*fac ;
     pval[2] = -1.0f*fac ; pval[3] = -1.2f*fac ;
     HIDE_SCALE(im3d) ;
     alter_MCW_pbar( pbar , 0 , pval ) ;
     FIX_SCALE_SIZE(im3d) ;

   } else if( av == im3d->vwid->func->iab_bot_av ){

     if( av->ival != 2 ) AFNI_iab_pbar_CB(pbar,im3d,pbCR_VALUE) ;

   }

   EXRETURN ;
}
#endif

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

void AFNI_setup_inten_pbar( MCW_pbar *pbar )
{
  int np , i , jm , lcol ;

ENTRY("AFNI_setup_inten_pbar") ;

  if( pbar == NULL ) EXRETURN ;

  jm   = pbar->mode ;
  lcol = pbar->dc->ovc->ncol_ov - 1 ;

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
   MCW_pbar *pbar = (MCW_pbar *)cd ;
   Three_D_View *im3d = (Three_D_View *)pbar->parent ;

   if( !IM3D_OPEN(im3d) ) return ;

   if( PBAR_FULLRANGE && !IM3D_ULAY_COHERENT(im3d) ){ /* 10 Jun 2014 */
     STATUS("incoherent ulay -- patching") ;
     ERROR_message("AFNI_inten_av_CB: incoherent ulay -- patching") ;
     AFNI_assign_ulay_bricks(im3d) ;
   }

   if( PBAR_FULLRANGE ) AFNI_redisplay_func_ignore(1) ;

#if 0
   STATUS_IM3D_TMASK(im3d) ;
   STATUS("clear tmask") ;
#endif
   IM3D_CLEAR_TMASK(im3d) ;                                /* Mar 2013 */
   IM3D_CLEAR_THRSTAT(im3d) ;                           /* 12 Jun 2014 */
   HIDE_SCALE(im3d) ;
   if( av->ival > NPANE_MAX ){
     int npane=pbar->num_panes , jm=pbar->mode ;
     float pmax,pmin ;
     pmax = (pbar->big31) ? pbar->bigtop : pbar->pval_save[npane][0    ][jm] ;
     pmin = (pbar->big31) ? pbar->bigbot : pbar->pval_save[npane][npane][jm] ;
     PBAR_set_bigmode( pbar , 1 , pmin,pmax ) ;
     AFNI_inten_pbar_CB( pbar , im3d , 0 ) ;
     POPUP_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
   } else {
     pbar->bigmode = 0 ;
     alter_MCW_pbar( pbar , av->ival , NULL ) ;
     NORMAL_cursorize( pbar->panew ) ;  /* 08 Apr 2005 */
   }
   FIX_SCALE_SIZE(im3d) ;

   if( PBAR_FULLRANGE ) AFNI_redisplay_func_ignore(0) ;

   if( PBAR_FULLRANGE ) AFNI_pbar_topset(im3d,im3d->vinfo->fim_range) ;
   else                 HINTIZE_pbar(im3d) ;

   return ;
}

/*----------------------------------------------------------------------------*/

char * AFNI_inten_av_texter( MCW_arrowval *av , XtPointer cd )
{
   static char buf[4] ;
   if( av->ival > NPANE_MAX ) strcpy (buf,"**") ;           /* label for  */
   else                       sprintf(buf,"%d",av->ival) ;  /* number of  */
   return buf ;                                             /* pbar panes */
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

   /* use template space of parent to mark as TLRC/MNI/... */
   MCW_strncpy( new_dset->atlas_space ,
      anat_parent->atlas_space , THD_MAX_NAME ) ;

   new_dset->anat_parent = anat_parent ;            /* what else makes sense? */

   new_dset->tagset = NULL ;  /* Oct 1998 */
   new_dset->Label_Dtable = NULL;                  /* ZSS Feb 26 2010 */

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
   THD_session *ss, *temp_ss ;
   THD_3dim_dataset *orig_dset , *temp_dset, *temp_anat_dset ;
   THD_slist_find     find ;
   THD_3dim_dataset *anat_parent_dset;
/*                  **orig_row ;*/
   int orig_row_key, anat_parent_row_key;

ENTRY("AFNI_make_descendants_old") ;

   if( ! ISVALID_SESSIONLIST(ssl) ) EXRETURN ;

   /* loop over each session */

   for( iss=0 ; iss < ssl->num_sess ; iss++ ){
      ss = ssl->ssar[iss] ;
      if( !ISVALID_SESSION(ss) ) continue ;  /* no good ==> skip */

      if( ss == GLOBAL_library.session ) continue ; /* 21 Dec 2001 */

      /* loop over datasets in this session */

      for( jdd=0 ; jdd < ss->num_dsset ; jdd++ ){
        orig_dset = GET_SESSION_DSET(ss, jdd, vbase);
/*         orig_dset = ss->dsset_xform_table[jdd][vbase] ; */ /* 03 Jun 98 */
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

   /* will need to change this test with updated session tables - drg 05/2010 */
         anat_parent_dset =
           GET_SESSION_DSET(ssl->ssar[find.sess_index], find.dset_index, vbase) ;
         if(orig_dset == anat_parent_dset) continue ;  /* 20 Jul 2010 */

         anat_parent_row_key = find.dset_index;
/*         anat_parent_row =
           &(ssl->ssar[find.sess_index]->dsset_xform_table[find.dset_index][0]) ;*/

         /* pointer to row of datasets being operated on now
            (like the FIM row in the picture above)          */
         orig_row_key = jdd;
/*         orig_row = &(ss->dsset_xform_table[jdd][0]) ; */
/*         if( orig_row == anat_parent_row ) continue ;*/  /* 14 Dec 1999 */

         /* loop over downstream dataset positions (from orig_dset);
            those that don't exist yet, but have entries in the
            anat_parent_row can be brought into being now */
         temp_ss = ssl->ssar[find.sess_index];

         for( kvv=vbase+1 ; kvv <= LAST_VIEW_TYPE ; kvv++ ){
            if( GET_SESSION_DSET(ss, orig_row_key, kvv) != NULL ) continue ;
            temp_anat_dset = GET_SESSION_DSET(temp_ss, anat_parent_row_key, kvv);
            if( temp_anat_dset == NULL ) continue ;

            temp_dset = AFNI_follower_dataset( temp_anat_dset, orig_dset ) ;

            SET_SESSION_DSET(temp_dset, ss, orig_row_key, kvv);
/*            orig_row[kvv] = AFNI_follower_dataset( anat_parent_row[kvv] ,
                                                   orig_dset ) ;*/
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
   THD_3dim_dataset *dset, *pref_dset, *anyanat_dset ;
   int quiet = !AFNI_noenv("AFNI_NO_ADOPTION_WARNING") ; /* 03 Dec 1999 */
   int first = 1 ;

ENTRY("AFNI_force_adoption") ;

   if( ! ISVALID_SESSION(ss) || ss->num_dsset == 0 || ss->is_collection ) EXRETURN ;

   if( ss == GLOBAL_library.session ) EXRETURN ; /* 21 Dec 2001 */

   /* find a "preferred" parent (one with the most number of markers set) */

   for( aa=0 ; aa < ss->num_dsset ; aa++ ){

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"scanning dataset %d for markers",aa) ; STATUS(str) ; }
      dset = GET_SESSION_DSET(ss, aa, 0);   /* original view */
/*      dset = ss->dsset_xform_table[aa][0] ;*/   /* original view */

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
       dset = GET_SESSION_DSET(ss, aa, 0);
       if( ISVALID_DSET(dset)    &&
           ISANAT(dset)          &&
           dset->markers != NULL && dset->markers->numset >= aset ) vv++ ;
     }
   }

   quiet = ( quiet || vv <= 1 ) ; /* be quiet if ordered, or if no alternatives */

if(aset >= 0 && PRINT_TRACING)
{ char str[256] ;
  THD_3dim_dataset *temp_dset;
  temp_dset = GET_SESSION_DSET(ss,apref,0);
  sprintf(str,"session %s: apref=%d [%s] aset=%d",
          ss->lastname,apref,DSET_HEADNAME(temp_dset),aset) ;
/*  sprintf(str,"session %s: apref=%d [%s] aset=%d",
          ss->lastname,apref,DSET_HEADNAME(ss->dsset_xform_table[apref][0]),aset) ;*/
  STATUS(str) ; }

   /* scan through all datasets, all views */

   for( ff=0 ; ff < ss->num_dsset ; ff++ ){
      for( vv=VIEW_ORIGINAL_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ){
         dset = GET_SESSION_DSET(ss, ff, vv);    /* function that needs parent */
/*         dset = ss->dsset_xform_table[ff][vv] ; */ /* function that needs parent */

         if( ! ISVALID_3DIM_DATASET(dset) ||
             dset->anat_parent != NULL      ) continue ; /* nothing to do */

         if( !do_anats && ISANAT(dset)      ) continue ; /* 28 Jul 2003 */

         if( DSET_in_global_session(dset) ) continue ; /* 25 Dec 2001 */

         pref_dset = GET_SESSION_DSET(ss,apref,vv);

         if( ISVALID_3DIM_DATASET(pref_dset) ){  /* if preferred is OK, */
            dset->anat_parent = pref_dset ;      /* use it here         */
         }
         else {
            for( aa=0 ; aa < ss->num_dsset ; aa++ ){          /* search for something, */
               anyanat_dset = GET_SESSION_DSET(ss, aa, vv);
               if( ISVALID_3DIM_DATASET(anyanat_dset)
                   && ISANAT(anyanat_dset)           ){  /* anything, and use it  */
                  dset->anat_parent = anyanat_dset ; break ;
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

#undef ALLOW_OLD_EDGIZE
#ifdef ALLOW_OLD_EDGIZE
/*-----------------------------------------------------------------------*/
/* Hollow out the overlay in place -- 21 Mar 2005 - RWCox.
   An interior pixel is defined as one whose 4 nearest neighbors are
   all nonzero.  This editing is done in-place (in the input image).
-------------------------------------------------------------------------*/

static void mri_edgize( MRI_IMAGE *im )
{
   int ii , jj , nx,ny,nxy , joff ;

   if( im == NULL ) return ;

   nx = im->nx ; ny = im->ny ; nxy = nx * ny ;
   if( nx < 3 || ny < 3 ) return ;  /* no interior pixels at all?! */

   switch( im->kind ){

     case MRI_byte:{                             /* 09 Dec 2014 */
       byte *ajj , *ajm , *ajp , *atemp , *ar ;
       ar    = MRI_BYTE_PTR(im) ;
       atemp = (byte *)malloc(sizeof(byte)*nxy); if( atemp == NULL ) return;
       memcpy(atemp,ar,sizeof(byte)*nxy) ;
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
#endif

/*-----------------------------------------------------------------------*/

static void mri_edgize_outer( MRI_IMAGE *im )
{
   int ii , jj , nx,ny,nxy , joff ;

   if( im == NULL ) return ;

   nx = im->nx ; ny = im->ny ; nxy = nx * ny ;
   if( nx < 3 || ny < 3 ) return ;  /* no interior pixels at all?! */

   switch( im->kind ){

     case MRI_byte:{                             /* 09 Dec 2014 */
       byte *ajj , *ajm , *ajp , *atemp , *ar ;
       ar    = MRI_BYTE_PTR(im) ;
#if 0
       (void)THD_mask_remove_isolas( im->nx , im->ny , 1 , ar ) ; /* 02 Nov 2018 */
#endif
       atemp = (byte *)calloc(sizeof(byte),nxy); if( atemp == NULL ) return;
       for( jj=1 ; jj < ny-1 ; jj++ ){
         joff = jj * nx ;      /* offset into this row */
         ajj  = ar    + joff ; /* pointer to this row */
         ajm  = ajj-nx ;       /* pointer to last row */
         ajp  = ajj+nx ;       /* pointer to next row */
         for( ii=1 ; ii < nx-1 ; ii++ ){
           if( ajj[ii] != 0 ){
             ajj[ii] = 1 ;
             if( ajm[ii]   == 0 || ajp[ii]   == 0 ||
                 ajj[ii+1] == 0 || ajj[ii-1] == 0   ) atemp[ii+joff] = 1 ;
           }
         }
         if( ajj[0] != 0 ){  /* deal with left edge of row */
           ajj[0] = 1 ;
           if( ajm[0] == 0 || ajp[0] == 0 || ajj[1] == 0 ) atemp[joff] = 1 ;
         }
         if( ajj[nx-1] != 0 ){  /* and right edge */
           ajj[nx-1] = 1 ;
           if( ajm[nx-1] == 0 || ajp[nx-1] == 0 || ajj[nx-2] == 0 ) atemp[nx-1+joff] = 1 ;
         }
       }
       /* deal with the top row */
       joff = 0 ; ajj  = ar + joff ; ajp = ajj+nx ;
       for( ii=1 ; ii < nx-1 ; ii++ ){
         if( ajj[ii] != 0 ){
           ajj[ii] = 1 ;
           if( ajp[ii]   == 0 || ajj[ii+1] == 0 || ajj[ii-1] == 0 ) atemp[ii+joff] = 1 ;
         }
       }
       /* deal with the bottom row */
       joff = (ny-1) * nx ; ajj  = ar + joff ; ajm  = ajj-nx ;
       for( ii=1 ; ii < nx-1 ; ii++ ){
         if( ajj[ii] != 0 ){
           ajj[ii] = 1 ;
           if( ajm[ii]   == 0 || ajj[ii+1] == 0 || ajj[ii-1] == 0 ) atemp[ii+joff] = 1 ;
         }
       }
       /* we do not deal with the 4 corner points -- let them eat cake */

       /* at this point, atemp = 1 only at edge points of clusters */
       /* now modify atemp so that only points that are zero AND are
          next to an edge point get a nonzero value == 'outer' edge */

       for( jj=1 ; jj < ny-1 ; jj++ ){
         joff = jj * nx ;      /* offset into this row */
         ajj  = atemp + joff ; /* pointer to this row */
         ajm  = ajj-nx ;       /* pointer to last row */
         ajp  = ajj+nx ;       /* pointer to next row */
         for( ii=1 ; ii < nx-1 ; ii++ ){
           if( ar[ii+joff] == 0 ){  /* not inside a blob */
             if( ajm[ii-1] != 0 || ajm[ii] != 0 || ajm[ii+1] != 0 ||
                 ajj[ii-1] != 0 || ajj[ii] != 0 || ajj[ii+1] != 0 ||
                 ajp[ii-1] != 0 || ajp[ii] != 0 || ajp[ii+1] != 0   ) ar[ii+joff] = 2 ;
           } else {
             ar[ii+joff] = 0 ;  /* inside a blob ==> zero out */
           }
         }
         if( ar[joff] == 0 ){   /* left edge of row */
           if( ajm[0] != 0 || ajm[0+1] != 0 ||
               ajj[0] != 0 || ajj[0+1] != 0 ||
               ajp[0] != 0 || ajp[0+1] != 0   ) ar[joff] = 2 ;
         } else {
           ar[joff] = 0 ;
         }
         if( ar[nx-1+joff] == 0 ){  /* right edge of row */
           if( ajm[nx-2] != 0 || ajm[nx-1] != 0 ||
               ajj[nx-2] != 0 || ajj[nx-1] != 0 ||
               ajp[nx-2] != 0 || ajp[nx-1] != 0   ) ar[nx-1+joff] = 2 ;
         } else {
           ar[nx-1+joff] = 0 ;
         }
       }
       /* deal with the bottom row */
       joff = 0 ; ajj = atemp + joff ; ajp = ajj+nx ;
       for( ii=1 ; ii < nx-1 ; ii++ ){
         if( ar[ii+joff] == 0 ){  /* not inside a blob */
           if( ajj[ii-1] != 0 || ajj[ii] != 0 || ajj[ii+1] != 0 ||
               ajp[ii-1] != 0 || ajp[ii] != 0 || ajp[ii+1] != 0   ) ar[ii+joff] = 2 ;
         } else {
           ar[ii+joff] = 0 ;  /* inside a blob ==> zero out */
         }
       }
       /* deal with the top row */
       joff = (ny-1) * nx ; ajj = atemp + joff ; ajm = ajj-nx ;
       for( ii=1 ; ii < nx-1 ; ii++ ){
         if( ar[ii+joff] == 0 ){  /* not inside a blob */
           if( ajm[ii-1] != 0 || ajm[ii] != 0 || ajm[ii+1] != 0 ||
               ajj[ii-1] != 0 || ajj[ii] != 0 || ajj[ii+1] != 0   ) ar[ii+joff] = 2 ;
         } else {
           ar[ii+joff] = 0 ;  /* inside a blob ==> zero out */
         }
       }

       free(atemp) ;
     }
     return ; /* end of MRI_byte */

   } /* end of switching */

   return ; /* default im->kind case ==> do nothing */
}

/*-----------------------------------------------------------------------*/

static int reject_zero = 0 ;

#undef  ZREJ
#define ZREJ(vvv) (reject_zero && (vvv)==0)   /* 20 Apr 2005 */

#undef  THBOT
#undef  THTOP
#undef  THBIG
#define THBIG    1.e+37f
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
   MRI_IMAGE *im_thr , *im_fim , *im_ov , *im_noved=NULL ;
   short *ar_ov ;
   short fim_ovc[NPANE_MAX+1] ;
   int npix , ii , lp , num_lp , ival ;
   float scale_factor , scale_thr ;
   MCW_pbar *pbar ;
   int simult_thr , need_thr ;
   int thrsign ;  /* 08 Aug 2007 */

ENTRY("AFNI_func_overlay") ;

   /* sanity check */

   if( br_fim == NULL || n < 0 ) RETURN(NULL) ;  /* nothing to do */

   im3d = (Three_D_View *) br_fim->parent ;
   if( !IM3D_OPEN(im3d) ) RETURN(NULL) ;         /* should not happen */

   pbar = im3d->vwid->func->inten_pbar ;

   /* 22 May 2009: check if functional dataset is ready */

   if( !ISVALID_DSET(im3d->fim_now) ){
     AFNI_SEE_FUNC_OFF(im3d) ; RETURN(NULL) ;
   }

   reject_zero = VEDIT_good(im3d->vedset) ||         /* 09 Dec 2014 */
                 !AFNI_yesenv("AFNI_OVERLAY_ZERO") ; /* 20 Apr 2005 */

   ival = im3d->vinfo->thr_index ;  /* threshold sub-brick index */

   /* get the component images */

   ival = im3d->vinfo->thr_index ;  /* threshold sub-brick index */
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
     AFNI_set_ignore_vedit(1) ;  /* 20 Oct 2016 */
     im_thr = FD_warp_to_mri( n , ival , br_fim ) ;
     AFNI_set_ignore_vedit(0) ;
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

     STATUS("fetch im_fim") ;
     im_fim = FD_warp_to_mri( n, ind, br_fim ) ;  /* get func image */
     scale_factor = FIM_RANGE(im3d) ;
     if( scale_factor == 0.0 || PBAR_FULLRANGE ) scale_factor = 1.0f ;

     /* Get non-volume-edited image as well, maybe [05 Nov 2018] */

     if( im_fim != NULL             &&
         VEDIT_good(im3d->vedset)   &&
         im3d->vinfo->thr_use_alpha && pbar->bigmode ){

       AFNI_set_ignore_vedit(1) ;
       STATUS("fetch im_noved") ;
       im_noved = FD_warp_to_mri( n , ind , br_fim ) ;
       AFNI_set_ignore_vedit(0) ;
       if( mri_equal(im_fim,im_noved) ){ mri_free(im_noved); im_noved=NULL; }
     }

     /* set OLay label string, for display as text in GUI */

     AFNI_set_valabel( br_fim , n ,
                       (im_noved != NULL) ? im_noved : im_fim ,
                       im3d->vinfo->func_val ) ;
   }

   /* 04 Mar 2003: convert thresh to float if its datum is unacceptable */
   /* 21 Dec 2004: also allow RGB images as threshold via this mechanism :) */

   if( im_thr != NULL && !(im_thr->kind == MRI_float ||
                           im_thr->kind == MRI_short ||
                           im_thr->kind == MRI_byte    ) ){

     MRI_IMAGE *qim = mri_to_float(im_thr) ;
     STATUS("scaled im_thr to floats") ;
     mri_free(im_thr) ; im_thr = qim ; scale_thr = 1.0f ;
   }

   /* set Thr label string, for display in GUI */

   AFNI_set_valabel( br_fim , n , im_thr , im3d->vinfo->thr_val ) ;

   /* if component images not good, quit now */

   if( im_fim == NULL ){
     STATUS("couldn't get Func image!") ;
     KILL_1MRI(im_thr) ; RETURN(NULL) ;
   }

   /* 15 Apr 2002: allow overlay image to be pure RBG (no colorscale) */
   /* 20 Dec 2004: allow thresholding of RGB overlays */

   if( im_fim->kind == MRI_rgb ){                  /* 15 Apr 2002: RGB overlays */

     if( im_thr != NULL && im_thr != im_fim ){          /* 20 Dec 2004 */
       float thresh = get_3Dview_func_thresh(im3d,1) / scale_thr ;
       float thb=THBOT(thresh) , tht=THTOP(thresh) ;    /* 08 Aug 2007 */
       mri_threshold( thb,tht , im_thr , im_fim ) ;     /* in place */
     }

     /* 27 Apr 2005: transform RGB func image in place? */

     mri_rgb_transform_nD( im_fim, 0, im3d->vwid->func->pbar_transform0D_func );
     mri_rgb_transform_nD( im_fim, 2, im3d->vwid->func->pbar_transform2D_func );

     if( im_thr != im_fim ) KILL_1MRI(im_thr) ;  /* toss the trash */
     RETURN(im_fim) ;
   }

   /* must be a scalar image at this point */

   if( ! AFNI_GOOD_FUNC_DTYPE(im_fim->kind) ){   /* should never happen! */
     MRI_IMAGE *qim = mri_to_float(im_fim) ;     /* (but fix it if it does) */
     STATUS("had to convert im_fim to floats?????") ;
     mri_free(im_fim) ; im_fim = qim ;
     if( im_noved != NULL ){ mri_free(im_noved); im_noved=NULL; }
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

     if( im_noved != NULL ){
       tim = mri_to_float(im_noved) ;
       AFNI_CALL_0D_function( im3d->vwid->func->pbar_transform0D_func ,
                              tim->nvox , MRI_FLOAT_PTR(tim)           ) ;
       mri_free(im_noved) ; im_noved = tim ;
     }
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

     if( im_noved != NULL ){
       tim = mri_to_float(im_noved) ;
       AFNI_CALL_2D_function( im3d->vwid->func->pbar_transform2D_func ,
                              tim->nx, tim->ny, tim->dx, tim->dy, MRI_FLOAT_PTR(tim) ) ;
       mri_free(im_noved) ; im_noved = tim ;
     }
   }

   /**---------- 30 Jan 2003:
                 if the pbar is in "big" mode,
                 then create an RGB overlay in a separate function ----------**/

   /* flag bits for using the newnew function */

#define NFO_ZBELOW_MASK  1
#define NFO_ZABOVE_MASK  2
#define NFO_ALIN_MASK    4
#define NFO_AQUA_MASK    8
#define NFO_USE_BOXED   16
#define NFO_POS_MASK   256

   if( pbar->bigmode ){ /* "continuous" colorscale */

     float thresh = get_3Dview_func_thresh(im3d,1) / scale_thr ;
     float thb=THBOT(thresh) , tht=THTOP(thresh) ; /* 08 Aug 2007 */
     int zbelow=0 , zabove=0 , flags ;

if( PRINT_TRACING && im_thr != NULL )
{ char str[256] ; float tmax ;
  sprintf(str,"im_thr: nx=%d ny=%d kind=%s",
          im_thr->nx,im_thr->ny,MRI_TYPE_NAME(im_thr)) ; STATUS(str) ;
  tmax = (float)mri_maxabs(im_thr) ;
  sprintf(str,"maxabs(im_thr)=%g scale_thr=%g thresh=%g",tmax,scale_thr,thresh);
  STATUS(str) ;
  sprintf(str,"func_threshold=%g func_thresh_top=%g cont_perc_thr=%d",
          im3d->vinfo->func_threshold,im3d->vinfo->func_thresh_top,
          im3d->cont_perc_thr);
  STATUS(str);
}

     /* Always use AFNI_newnewfunc_overlay() [05 Nov 2018] */

     if( pbar->big30 ) reject_zero = VEDIT_good(im3d->vedset) ;
     if( pbar->big31 ){                              /* Feb 2012 */
       zbelow = !pbar->big32 ; zabove = !pbar->big30 ;
     } else {
       zbelow = (pbar->bigbot == 0.0f) ;
     }
     flags = zbelow * NFO_ZBELOW_MASK + zabove * NFO_ZABOVE_MASK ;

     if( im3d->vinfo->thr_use_alpha ) flags |= NFO_AQUA_MASK ;  /* alpha fading? */
     if( im3d->vinfo->use_posfunc   ) flags |= NFO_POS_MASK  ;  /* pos only FIM? */
     if( im3d->vinfo->thr_use_boxed ) flags |= NFO_USE_BOXED ;  /* boxing day?   */

     im_ov = AFNI_newnewfunc_overlay( im_thr , thb,tht ,
                                      im_fim , im_noved ,
                                      scale_factor*pbar->bigbot ,
                                      scale_factor*pbar->bigtop ,
                                      pbar->bigcolor , flags ,
                                      im3d->vinfo->thr_alpha_floor , im3d->dc ) ;
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
         mri_free(im_fim) ; mri_free(im_ov) ; mri_free(im_noved) ;
         STATUS("bad im_fim->kind!") ;
      RETURN(NULL) ;

      case MRI_short:{
         short *ar_fim = MRI_SHORT_PTR(im_fim) ;
         float fim_thr[NPANE_MAX] ;  /* 13 Nov 1996: changed from short */

         for( lp=0 ; lp < num_lp ; lp++ )
           fim_thr[lp] = scale_factor * pbar->pval[lp+1] ;

         if( simult_thr ){
           float thresh = get_3Dview_func_thresh(im3d,1) / scale_thr ;
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
           float thresh = get_3Dview_func_thresh(im3d,1) / scale_thr ;
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
           float thresh = get_3Dview_func_thresh(im3d,1) ;
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
         float thresh = get_3Dview_func_thresh(im3d,1) / scale_thr ;
         float thb=THBOT(thresh) , tht=THTOP(thresh) ; /* 08 Aug 2007 */
         short *ar_thr = MRI_SHORT_PTR(im_thr) ;

         for( ii=0 ; ii < npix ; ii++ )
           if( ar_thr[ii] > thb && ar_thr[ii] < tht ) ar_ov[ii] = 0 ;
       }
       break ;

       case MRI_byte:{
         float thresh = get_3Dview_func_thresh(im3d,1) / scale_thr ;
         float thb=THBOT(thresh) , tht=THTOP(thresh) ; /* 08 Aug 2007 */
         byte *ar_thr = MRI_BYTE_PTR(im_thr) ;

         for( ii=0 ; ii < npix ; ii++ )  /* assuming thb <= 0 */
           if( ar_thr[ii] < tht ) ar_ov[ii] = 0 ;
       }
       break ;

       case MRI_float:{
         float thresh = get_3Dview_func_thresh(im3d,1) ;
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
   mri_free( im_fim ) ; mri_free( im_noved ) ;

#ifdef ALLOW_OLD_EDGIZE
   /* 21 Mar 2005: Hollow out overlay? */
   if( AFNI_yesenv("AFNI_EDGIZE_OVERLAY") ) mri_edgize(im_ov) ;
#endif

   RETURN(im_ov) ;
}

#if 0
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
                                  float fimbot, float fimtop, rgbyte *fimcolor,
                                  int flags )
{
   MRI_IMAGE *im_ov ;
   byte *ovar ;
   int ii , npix , jj ;
   float fac , val ;
   int dothr = (thbot < thtop) ;                         /* 08 Aug 2007 */

   int zbelow = (flags & NFO_ZBELOW_MASK) != 0 ;  /* Feb 2012 */
   int zabove = (flags & NFO_ZABOVE_MASK) != 0 ;

   byte *bfim=NULL ; short *sfim=NULL ; float *ffim=NULL ; int kk ;

ENTRY("AFNI_newfunc_overlay") ;

   if( im_fim == NULL || fimbot >= fimtop || fimcolor == NULL ) RETURN(NULL) ;

   /* create output image */

STATUS("create output image") ;
   im_ov = mri_new_conforming( im_fim , MRI_rgb ) ;
   ovar  = MRI_RGB_PTR(im_ov) ;
   npix  = im_ov->nvox ;
   fac   = NPANE_BIG / (fimtop-fimbot) ; /* scale from data value to color index */

   kk = (int)im_fim->kind ;
   switch( kk ){
     default: mri_free(im_ov) ; RETURN(NULL) ;   /* should never happen! */
     case MRI_short: sfim = MRI_SHORT_PTR(im_fim) ; break ;
     case MRI_byte : bfim = MRI_BYTE_PTR (im_fim) ; break ;
     case MRI_float: ffim = MRI_FLOAT_PTR(im_fim) ; break ;
   }

STATUS("colorization") ;
   for( ii=0 ; ii < npix ; ii++ ){
          if( kk == MRI_byte  ) val = (float)bfim[ii] ;
     else if( kk == MRI_short ) val = (float)sfim[ii] ;
     else                       val =        ffim[ii] ;
     if( ZREJ(val) || (zabove && val > fimtop) || (zbelow && val < fimbot) ) continue ;
     jj = (int)( fac*(fimtop-val) ) ;
     if( jj < 0 ) jj = 0 ; else if( jj > NPANE_BIG1 ) jj = NPANE_BIG1 ;
     ovar[3*ii  ] = fimcolor[jj].r ;
     ovar[3*ii+1] = fimcolor[jj].g ;
     ovar[3*ii+2] = fimcolor[jj].b ;
   }

   /** now apply threshold, if any **/

   if( dothr && im_thr != NULL ){
STATUS("thresholdization") ;
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
#endif

/*-----------------------------------------------------------------------*/
/* This function is only used to alpha fade the pbar image.
   However, the macros below are used in AFNI_newnewfunc_overlay().
*//*---------------------------------------------------------------------*/

#define ALIN(th,fac) (th*fac)
#define AQUA(th,fac) (ALIN(th,fac)*ALIN(th,fac))
#define ALFA(th,fac) (alcode==1) ? 255.0f*ALIN(th,fac)+af          \
                    :(alcode==2) ? 255.0f*AQUA(th,fac)+af : 0.0f

#define ALFABYTE(xx) (  ((xx) <   0.0) ? (byte)0                      \
                      : ((xx) > 222.0) ? (byte)222 : (byte)rintf(xx) )


void AFNI_alpha_fade_mri( Three_D_View *im3d , MRI_IMAGE *im )
{
   float af,th,fi,aa,rf,bf,gf ; int ii,jj,kk,nx,ny ;
   byte *iar ;
   const int alcode=1 ;

   if( !IM3D_OPEN(im3d) || im == NULL || im->kind != MRI_rgb ) return ;
   if( im3d->vinfo->thr_use_alpha <= 0 ) return ;

   af  = 255.0f*im3d->vinfo->thr_alpha_floor ;
   nx  = im->nx ; if( nx < 2 ) return ;
   ny  = im->ny ;
   fi  = 0.995f/(nx-1) ;
   iar = MRI_RGB_PTR(im) ;

   for( kk=jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++,kk+=3 ){
       if( ii==0 ) continue ;
       th = 1.0f - ii*fi ;
       aa = ALFA(th,1.0f) ;
       if( aa < 255.0f ){
         rf = iar[kk] ; gf = iar[kk+1] ; bf = iar[kk+2] ;
         if( rf==  0.0f && gf==  0.0f && bf==  0.0f ) continue ;  /* don't alter */
         if( rf==255.0f && gf==255.0f && bf==255.0f ) continue ;  /* black or white */
         rf = (aa/255.0f)*rf + (255.0f-aa) ; iar[kk  ] = BYTEIZE(rf) ;
         gf = (aa/255.0f)*gf + (255.0f-aa) ; iar[kk+1] = BYTEIZE(gf) ;
         bf = (aa/255.0f)*bf + (255.0f-aa) ; iar[kk+2] = BYTEIZE(bf) ;
       }
     }
   }
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Make a functional overlay the new new way (08 Dec 2014):
    - im_thr = threshold image (may be NULL)
    - thbot  = pixels with values in im_thr in range (thbot..thtop)
    - thtop  =  get translucent overlay values
    - im_fim = image to make overlay from (may not be NULL)
    - fimbot = pixel value to map to fimcolor[0]
    - fimtop = pixel value to map to fimcolor[NPANE_BIG-1]
 ** Changed Nov 2018 to allow a mixture of the volume edited
    FIM (im_fim) and the non-volume edited FIM (im_noved) to
    determine color and alpha.
-------------------------------------------------------------------------*/

MRI_IMAGE * AFNI_newnewfunc_overlay( MRI_IMAGE *im_thr , float thbot,float thtop,
                                     MRI_IMAGE *im_fim , MRI_IMAGE *im_noved ,
                                     float fimbot, float fimtop, rgbyte *fimcolor,
                                     int flags , float alpha_floor , MCW_DC *dc )
{
   MRI_IMAGE *im_ov ;
   rgba *ovar ;
   int ii , npix , jj ;
   float fac , val , vval ;
   int dothr = (thbot < thtop) && (im_thr != NULL);  /* 08 Aug 2007 */
   int alcode=0 ;                                    /* 09 Dec 2014 */
   int do_pos = (flags & NFO_POS_MASK) != 0 ;        /* 12 Dec 2014 */
   int do_box = (flags & NFO_USE_BOXED)!= 0 ;        /* 02 Nov 2018 */

   int zbelow = (flags & NFO_ZBELOW_MASK) != 0 ;     /* Feb 2012 */
   int zabove = (flags & NFO_ZABOVE_MASK) != 0 ;
   byte *bfim=NULL ; short *sfim=NULL ; float *ffim=NULL ; int kf ;
   byte *bvim=NULL ; short *svim=NULL ; float *fvim=NULL ;

ENTRY("AFNI_newnewfunc_overlay") ;

   if( im_fim == NULL || fimbot >= fimtop || fimcolor == NULL ) RETURN(NULL) ;

   /* create output image */

STATUS("create output image") ;
   im_ov = mri_new_conforming( im_fim , MRI_rgba ) ; /* zero filled */
   ovar  = MRI_RGBA_PTR(im_ov) ;      /* our job is to fill this up */
   npix  = im_ov->nvox ;
   fac   = NPANE_BIG / (fimtop-fimbot) ; /* scale from data value to color index */

   kf = (int)im_fim->kind ;  /* type of data in FIM */

   /* pointer for FIM data */

   switch( kf ){
     default: mri_free(im_ov) ; RETURN(NULL) ;   /* should never happen! */
     case MRI_short: sfim = MRI_SHORT_PTR(im_fim) ; break ;
     case MRI_byte : bfim = MRI_BYTE_PTR (im_fim) ; break ;
     case MRI_float: ffim = MRI_FLOAT_PTR(im_fim) ; break ;
   }

   /* setup pointer for non-volume-edited FIM data */

   if( im_noved == NULL || im_noved->kind != im_fim->kind )
     im_noved = im_fim ;     /* make it be the same as the FIM data */

   switch( kf ){
     case MRI_short: svim = MRI_SHORT_PTR(im_noved) ; break ;
     case MRI_byte : bvim = MRI_BYTE_PTR (im_noved) ; break ;
     case MRI_float: fvim = MRI_FLOAT_PTR(im_noved) ; break ;
   }

STATUS("colorization") ;
   /* compute the color of each pixel */
   /* color comes from the non-volume-edited data if available */
   /* note that pixels skipped here will get 0 alpha */
   for( ii=0 ; ii < npix ; ii++ ){
          if( kf == MRI_byte  ) vval = (float)bvim[ii] ;
     else if( kf == MRI_short ) vval = (float)svim[ii] ;
     else                       vval =        fvim[ii] ;

     if( ZREJ(vval) || (zabove && vval > fimtop) || (zbelow && vval < fimbot) ) continue ;
     if( do_pos && vval <= 0.0f ) continue ;  /* 12 Dec 2014 */

     jj = (int)( fac*(fimtop-vval) ) ;
     if( jj < 0 ) jj = 0 ; else if( jj > NPANE_BIG1 ) jj = NPANE_BIG1 ;
     ovar[ii].r = fimcolor[jj].r ; /* colorize */
     ovar[ii].g = fimcolor[jj].g ;
     ovar[ii].b = fimcolor[jj].b ;
     ovar[ii].a = 255 ;            /* default is opaque */
              /* but voxels skipped above are transparent */
   }

   /** now apply threshold, if any **/

        if( flags & NFO_ALIN_MASK ) alcode = 1 ;  /* linear fade */
   else if( flags & NFO_AQUA_MASK ) alcode = 2 ;  /* quadratic fade */
   else                             alcode = 0 ;  /* no fade = sharp cutoff */

   if( dothr ){
     MRI_IMAGE *eim=NULL ; byte *ear=NULL ;
     int do_edge = do_box ;
     float ft,fb,af ;

     /* scalings for alpha fading on the threshold value */

     ft = (thtop > 0.0f) ? (1.0f-alpha_floor)/thtop : 0.0f ;  /* for positive thr */
     fb = (thbot < 0.0f) ? (1.0f-alpha_floor)/thbot : 0.0f ;  /* for negative thr */
     af = 255.0f*alpha_floor ;

     if( do_edge ){  /* for later use in mri_edgize_outer() */
       eim = mri_new_conforming( im_fim , MRI_byte ) ; ear = MRI_BYTE_PTR(eim) ;
     }

STATUS("threshold-ization and alpha-ization") ;
     switch( im_thr->kind ){ /* the kind of data in the threshold image */

       case MRI_short:{
         register float thb=thbot , tht=thtop , aa ; register int rej, vvz ;
         register short *ar_thr = MRI_SHORT_PTR(im_thr) ;
         for( ii=0 ; ii < npix ; ii++ ){
                if( kf == MRI_byte  ){ val = (float)bfim[ii]; vval = (float)bvim[ii]; }
           else if( kf == MRI_short ){ val = (float)sfim[ii]; vval = (float)svim[ii]; }
           else                      { val =        ffim[ii]; vval =        fvim[ii]; }
           vvz = (val == 0.0f) && (vval != 0.0f) ;        /* was vedit-ed out? */
           rej = (ovar[ii].a == 0) || (ar_thr[ii] == 0) ; /* rejected out of hand */
           if( rej ){
                                        ovar[ii].a = 0 ;     /* transparent */
           } else if( ar_thr[ii] > 0 && (ar_thr[ii] < tht || vvz) ){
             aa = ALFA(ar_thr[ii],ft) ; ovar[ii].a = ALFABYTE(aa) ;
           } else if( ar_thr[ii] < 0 && (ar_thr[ii] > thb || vvz) ){
             aa = ALFA(ar_thr[ii],fb) ; ovar[ii].a = ALFABYTE(aa) ;
           } else if( do_edge ){
             if( !(do_pos && val <= 0.0f) ) ear[ii] = 1 ; /* not faded or vedit-ed */
           }
         }
       }
       break ;

       case MRI_byte:{
         register float thb=thbot , tht=thtop , aa ; register int rej, vvz ;
         register byte *ar_thr = MRI_BYTE_PTR(im_thr) ;
         for( ii=0 ; ii < npix ; ii++ ){ /* assuming thb <= 0 always */
                if( kf == MRI_byte  ){ val = (float)bfim[ii]; vval = (float)bvim[ii]; }
           else if( kf == MRI_short ){ val = (float)sfim[ii]; vval = (float)svim[ii]; }
           else                      { val =        ffim[ii]; vval =        fvim[ii]; }
           vvz = (val == 0.0f) && (vval != 0.0f) ;        /* was vedit-ed out? */
           rej = (ovar[ii].a == 0) || (ar_thr[ii] == 0) ; /* rejected out of hand */
           if( rej ){
                                        ovar[ii].a = 0 ;     /* transparent */
           } else if( ar_thr[ii] > 0 && (ar_thr[ii] < tht || vvz) ){
             aa = ALFA(ar_thr[ii],ft) ; ovar[ii].a = ALFABYTE(aa) ;
           } else if( do_edge ){
             if( !(do_pos && val <= 0.0f) ) ear[ii] = 1 ; /* not faded or vedit-ed */
           }
         }
       }
       break ;

       case MRI_float:{
         register float thb=thbot , tht=thtop , aa ; register int rej, vvz ;
         register float *ar_thr = MRI_FLOAT_PTR(im_thr) ;
         for( ii=0 ; ii < npix ; ii++ ){
                if( kf == MRI_byte  ){ val = (float)bfim[ii]; vval = (float)bvim[ii]; }
           else if( kf == MRI_short ){ val = (float)sfim[ii]; vval = (float)svim[ii]; }
           else                      { val =        ffim[ii]; vval =        fvim[ii]; }
           vvz = (val == 0.0f) && (vval != 0.0f) ;        /* was vedit-ed out? */
           rej = (ovar[ii].a == 0) || (ar_thr[ii] == 0) ; /* rejected out of hand */
           if( rej ){
                                        ovar[ii].a = 0 ;     /* transparent */
           } else if( ar_thr[ii] > 0 && (ar_thr[ii] < tht || vvz) ){
             aa = ALFA(ar_thr[ii],ft) ; ovar[ii].a = ALFABYTE(aa) ;
           } else if( ar_thr[ii] < 0 && (ar_thr[ii] > thb || vvz) ){
             aa = ALFA(ar_thr[ii],fb) ; ovar[ii].a = ALFABYTE(aa) ;
           } else if( do_edge ){
             if( !(do_pos && val <= 0.0f) ) ear[ii] = 1 ; /* not faded or vedit-ed */
           }
         }
       }
       break ;
     }

     /* process the edges of the above-threshold regions? */

     if( do_edge ){
       char *cpt ; byte rb=1,gb=1,bb=1 ;
       mri_edgize_outer(eim) ;  /* mark the edges of the unfaded regions */
       cpt = getenv("AFNI_FUNC_BOXED_COLOR") ;
       if( cpt != NULL ){
         float rf=0.005f, gf=0.005f, bf=0.005f ;
         DC_parse_color( dc , cpt , &rf,&gf,&bf ) ;
         rb = BYTEIZE(255.0f*rf); gb = BYTEIZE(255.0f*gf); bb = BYTEIZE(255.0f*bf);
       }
       for( ii=0 ; ii < npix ; ii++ ){
        if( ear[ii] ){ ovar[ii].r=rb; ovar[ii].g=gb; ovar[ii].b=bb; ovar[ii].a=254; }
       }
       mri_free(eim) ;
     }
   } /* end of threshold-ization and all the fun it has */

   RETURN(im_ov) ;
}
#undef ALIN
#undef AQUA
#undef ALFA

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
   byte *b0=NULL , *brik,  *ovc  ;
   short *s0=NULL, *ovar, *val, *fovar ;
   float *f0=NULL;
   MRI_IMAGE *ovim=NULL, *b0im=NULL, *fovim=NULL, *b1im=NULL;
   int gwin , fwin , nreg , ii,jj , nov ;
   int at_sbi, fim_type, at_vox, at_nsb;

ENTRY("AFNI_ttatlas_overlay") ;

   /* setup and sanity checks */

   /* make sure we are actually drawing something */
  if(AFNI_yesenv("AFNI_JILL_TRAVESTY"))
      printf("Starting AFNI_ttatlas_overlay\n");

   STATUS("checking if Atlas Colors is on") ;
   ttp = TTRR_get_params() ; if( ttp == NULL )            RETURN(NULL) ;

   STATUS("checking if Atlas dataset can be loaded") ;

   if((!atlas_ovdset) ||
      ( DSET_NVOX(atlas_ovdset) != DSET_NVOX(im3d->anat_now))){
       if(atlas_ovdset)
          DSET_unload(atlas_ovdset);
       dseTT = TT_retrieve_atlas_dset(Current_Atlas_Default_Name(),0);
       if( dseTT == NULL ) RETURN(NULL) ;
       DSET_load(dseTT) ;
       if(atlas_ovdset)    /* reset the atlas overlay dataset */
          DSET_unload(atlas_ovdset);
       atlas_ovdset = r_new_resam_dset ( dseTT, im3d->anat_now,  0, 0, 0, NULL,
                                       MRI_NN, NULL, 1, 0);
  if(AFNI_yesenv("AFNI_JILL_TRAVESTY"))
      printf("First time loading atlas dset\n");
/*       DSET_unload(dseTT);*/
       if(!atlas_ovdset) {
         if(AFNI_yesenv("AFNI_JILL_TRAVESTY"))
            printf("Could not load atlas dset\n");
         RETURN(NULL);
       }
   }

   if( DSET_NVOX(atlas_ovdset) != DSET_NVOX(im3d->anat_now) ){
      WARNING_message(
         "Voxels do not match between resampled atlas and the underlay dataset");
      RETURN(NULL) ;
   }
   /* get slices from TTatlas dataset */
   STATUS("loading Atlas bricks") ;

   /* extract slice from right direction from atlas */
   b0im = AFNI_slice_flip( n , 0 , RESAM_NN_TYPE , ax_1,ax_2,ax_3 ,
                           atlas_ovdset ) ;
   if( b0im == NULL )
      RETURN(NULL) ;

   /* make a new overlay image, or just operate on the old one */
   STATUS("making new overlay for Atlas") ;
   ovim = mri_new_conforming( b0im , MRI_short ) ;   /* new overlay */
   ovar = MRI_SHORT_PTR(ovim) ;
   memset( ovar , 0 , ovim->nvox * sizeof(short) ) ;
   /* only needed b0im to get resampled grid */
   mri_free(b0im);

   /* fwin => function 'wins' over Atlas - overlay image gets priority */
   /* gwin => gyral Atlas brick 'wins' over 'area' Atlas brick - */

   fwin = (ttp->meth == TTRR_METH_FGA) || (ttp->meth == TTRR_METH_FAG) ;
   gwin = (ttp->meth == TTRR_METH_FGA) || (ttp->meth == TTRR_METH_GAF) ;
   nreg = ttp->num ;    /* number of 'on' regions     */
   brik = ttp->ttbrik ; /* which sub-brick in atlas    */
   val  = ttp->ttval ;  /* which code in that sub-brick */
   ovc  = ttp->ttovc ;  /* which overlay color index   */

   /* loop over image voxels, find overlays from Atlas */
   STATUS("doing Atlas overlay") ;
   at_nsb = DSET_NVALS(atlas_ovdset);
   nov = 0;
   for( at_sbi=0; at_sbi < at_nsb; at_sbi++) {
      b1im = AFNI_slice_flip( n,at_sbi,RESAM_NN_TYPE,ax_1,ax_2,ax_3,
                             atlas_ovdset);
      if( b1im == NULL )
         RETURN(NULL) ;
      fim_type = b1im->kind ;
      switch( fim_type ){
         default:
            RETURN(NULL) ;
         case MRI_byte:
            b0 = MRI_BYTE_PTR(b1im);
         break ;
         case MRI_short:
            s0 = MRI_SHORT_PTR(b1im);
         break ;
         case MRI_float:
            f0 = MRI_FLOAT_PTR(b1im);
         break ;
      }

      for( ii=0 ; ii < ovim->nvox ; ii++ ){
         /* if the overlay array is already set in the overlay */
         /* earlier atlas voxel, keep it*/
         if( (ovar[ii] && gwin ) ) continue ;

         /* check Atlas 'on' regions for hits */
         for( jj=0 ; jj<nreg ; jj++ ){
            switch( fim_type ){
               default:
               case MRI_byte:
                  at_vox = (int) b0[ii];
               break ;
               case MRI_short:
                  at_vox = (int) s0[ii];
               break ;
               case MRI_float:
                  at_vox = (int) (f0[ii]+.1); /* show in overlay if >=0.4 */
               break ;
            }

            if( at_vox == val[jj] ) {
               ovar[ii] = ovc[jj] ;
               nov++ ;
            }
         }

      }
      mri_free(b1im) ;
   }

   if(PRINT_TRACING)
      { char str[256]; sprintf(str,"Atlas overlaid %d pixels",nov); STATUS(str); }

   if(fov == NULL) {  /* if there was no overlay, return what we have */
      if(AFNI_yesenv("AFNI_JILL_TRAVESTY"))
         printf("No overlay, that's okay\n");
      RETURN(ovim);
   }

   STATUS("re-using old overlay for Atlas") ;
   if(AFNI_yesenv("AFNI_JILL_TRAVESTY"))
      printf("re-using old overlay for atlas\n");
   fovim = fov ;                                      /* old overlay */
   fovar = MRI_SHORT_PTR(fovim) ;
   if( fovim->nvox != ovim->nvox ){                    /* shouldn't happen!  */
        if(AFNI_yesenv("AFNI_JILL_TRAVESTY"))
            printf("freeing ovim at early return\n");
         mri_free(ovim); RETURN(NULL) ;
   }
   nov = 0;
   for( ii=0 ; ii < ovim->nvox ; ii++ ){
      /* if the overlay array is already set in the overlay, keep it*/
      if( (fovar[ii]!=0) && fwin ) continue;
      if(ovar[ii]!=0) {
         fovar[ii] = ovar[ii];
         nov++;
      }
   }

   if(AFNI_yesenv("AFNI_JILL_TRAVESTY"))
       printf("freeing ovim at normal return\n");
   mri_free(ovim);
   if(AFNI_yesenv("AFNI_JILL_TRAVESTY"))
       printf("leaving AFNI_ttatlas_overlay\n");
   RETURN(fovim);
}

/* use to force reload of atlas for new default */
void
reset_atlas_ovdset()
{
   DSET_unload(atlas_ovdset);
   atlas_ovdset = NULL;
}

/* get the current resampled dataset used for overlay */
THD_3dim_dataset *
current_atlas_ovdset()
{
   return(atlas_ovdset);
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
#if 0
   STATUS_IM3D_TMASK(im3d) ;
   STATUS("clear tmask") ;
#endif
   IM3D_CLEAR_TMASK(im3d) ;                                /* Mar 2013 */
   IM3D_CLEAR_THRSTAT(im3d) ;                           /* 12 Jun 2014 */
   im3d->vinfo->tempflag = 1 ;
   AFNI_modify_viewing( im3d , False ) ;  /* redisplay */
   SHOW_AFNI_READY ;
   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------*/
/*----- set the bricks to use for the underlay images -----*/

void AFNI_assign_ulay_bricks( Three_D_View *im3d )
{
ENTRY("AFNI_assign_ulay_bricks") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   switch( im3d->vinfo->underlay_type ){
     default:
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
         WARNING_message("invalid Overlay dataset") ;
         im3d->b123_ulay = im3d->b123_anat ;
         im3d->b231_ulay = im3d->b231_anat ;
         im3d->b312_ulay = im3d->b312_anat ;
#ifdef USE_UNDERLAY_BBOX
         MCW_set_bbox( im3d->vwid->func->underlay_bbox , 1<<UNDERLAY_ANAT ) ;
#else
         im3d->vinfo->underlay_type = UNDERLAY_ANAT ;
#endif
       }
     break ;
   }

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
   Three_D_View *im3d = (Three_D_View *)cd ;
   int bval , force_redraw=(cb != NULL) ;
   Boolean seq_exist ;

ENTRY("AFNI_underlay_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

#ifdef USE_UNDERLAY_BBOX
   if( w != NULL ) bval = AFNI_first_tog( LAST_UNDERLAY_TYPE+1 ,
                                          im3d->vwid->func->underlay_bbox->wbut ) ;
   else            bval = im3d->vinfo->underlay_type ;
#else
   bval = im3d->vinfo->underlay_type ;
#endif

   if( bval == im3d->vinfo->underlay_type && w != NULL && IM3D_ULAY_COHERENT(im3d) ) EXRETURN ;

   im3d->vinfo->underlay_type = bval ;

   AFNI_assign_ulay_bricks(im3d) ;  /* 10 Jun 2014 */

#if 0
INFO_message("AFNI_underlay_CB: anat:%p %p %p  fim:%p %p %p  ulay:%p %p %p" ,
             im3d->b123_anat , im3d->b231_anat , im3d->b312_anat ,
             im3d->b123_fim  , im3d->b231_fim  , im3d->b312_fim  ,
             im3d->b123_ulay , im3d->b231_ulay , im3d->b312_ulay  ) ;
#endif

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
#if 0
      STATUS_IM3D_TMASK(im3d) ;
      STATUS("clear tmask") ;
#endif
      IM3D_CLEAR_TMASK(im3d) ;                                /* Mar 2013 */
      IM3D_CLEAR_THRSTAT(im3d) ;                           /* 12 Jun 2014 */

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

      if( w != NULL || force_redraw ){  /* a real callback */
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
   int ic, ib;

   ic = AFNI_controller_index( im3d ) ;
   if( ic < 0 || ic > 26 ) strcpy (str,"    ") ;  /* shouldn't happen */
   else {
      if ((ib = get_user_np_bloc())>-1) { /* ZSS June 2011 */
                           sprintf(str,"[%c%d] ",clabel[ic], ib) ;
      } else {
                           sprintf(str,"[%c] ",clabel[ic]) ;
      }
   }
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
   int ninit=0;

ENTRY("AFNI_set_window_titles") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   clab = AFNI_controller_label(im3d) ;
   ninit = strlen(clab)-1;
   switch( im3d->vinfo->thr_sign ){
     default: ilab = ninit -1; break ;
     case 1:  ilab = ninit ; clab[ninit] = '+' ; break ;
     case 2:  ilab = ninit ; clab[ninit] = '-' ; break ;
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

#define STRLIST_SIZE      (THD_MAX_PREFIX+256)
#define MAX_SESSTRAIL_LEN 48

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
   THD_3dim_dataset *temp_dset=NULL;
   int sesstrail = (strcmp(im3d->ss_now->sessname,"All_Datasets")==0) ;
   char *st=NULL , sst[STRLIST_SIZE+1] ;

ENTRY("AFNI_choose_dataset_CB") ;

   /*--- initialize ---*/

   if( ! IM3D_VALID(im3d) || w == (Widget)NULL ) EXRETURN ;

   if( GLOBAL_library.have_dummy_dataset ){  /* 26 Feb 2007: read session? */
     if( w == im3d->vwid->view->choose_sess_pb ||
         w == im3d->vwid->view->popchoose_sess_pb ){

       AFNI_read_sess_CB(im3d->vwid->dmode->read_sess_pb,(XtPointer)im3d,NULL) ;

     } else {
       BEEPIT ; WARNING_message("Can't choose dataset if all you have is Dummy") ;
     }
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
      THD_set_oblique_report(num_str-1, -1);
      ltop = 4 ;
      for( ii=0 ; ii < num_str ; ii++ ){
           #if 0 /* No more of this stuff    ZSS Nov 2011 */
               THD_report_obliquity(GET_SESSION_DSET(im3d->ss_now,ii,0)) ;
           #endif

         for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ )
            {
               temp_dset = GET_SESSION_DSET(im3d->ss_now, ii, vv);

               if( ISVALID_3DIM_DATASET(temp_dset) ) break ;
            }

         if( vv <= LAST_VIEW_TYPE ){
            llen = strlen( temp_dset->dblk->diskptr->prefix ) ;
            if( sesstrail ){
              st = THD_trailname(temp_dset->dblk->diskptr->directory_name,1) ;
              if( st != NULL ){
                if( strlen(st) > MAX_SESSTRAIL_LEN )
                  st += (strlen(st)-MAX_SESSTRAIL_LEN) ;
                llen += strlen(st) ;
              }
            }
            if( llen > ltop ) ltop = llen ;
         }
      }
      ltop = MIN(ltop,STRLIST_SIZE-24) ;  /* 06 Aug 2002 */

      for( ii=0 ; ii < num_str ; ii++ ){
         for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ) {
            temp_dset = GET_SESSION_DSET(im3d->ss_now, ii, vv);
            if( ISVALID_3DIM_DATASET(temp_dset) ) break ;
         }

         if( vv <= LAST_VIEW_TYPE ){
            if( sesstrail ){
              st = THD_trailname(temp_dset->dblk->diskptr->directory_name,1) ;
              if( st != NULL ){
                if( strlen(st) > MAX_SESSTRAIL_LEN )
                  st += (strlen(st)-MAX_SESSTRAIL_LEN) ;
              }
            } else {
              st = "\0" ;
            }
            strcpy(sst,st) ; strcat(sst,temp_dset->dblk->diskptr->prefix) ;
            sprintf( strlist[ii] , "%-*s" , ltop,sst ) ;

            strcat( strlist[ii] , " [" ) ;
            strcat( strlist[ii] , DSET_PREFIXSTR(temp_dset) ) ;

            if( DSET_NUM_TIMES(temp_dset) > 1 ){
               int ll = strlen(strlist[ii]) ;
               sprintf( strlist[ii]+ll , ":3D+t:%d]" ,
                        DSET_NUM_TIMES(temp_dset) ) ;
            } else if( ISBUCKET(temp_dset) ){
               int ll = strlen(strlist[ii]) ;
               sprintf( strlist[ii]+ll , ":%d]" ,
                        DSET_NVALS(temp_dset) ) ;
            } else {
               strcat( strlist[ii] , "]" ) ;
            }

            if( DSET_GRAPHABLE(temp_dset) )
               strcat( strlist[ii] , "*" ) ;

            if( DSET_COMPRESSED(temp_dset) )
               strcat( strlist[ii] , "z" ) ;

            /* 20 Dec 2001: mark if this is a global dataset */

            if( DSET_in_global_session(temp_dset) )
              strcat( strlist[ii] , "G" ) ;


         } else {
#if 1
THD_3dim_dataset *qset ; static int first=1 ;
if( first ){
 for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ){
  qset = GET_SESSION_DSET(im3d->ss_now, ii, vv);
  if( qset != NULL ){
   INFO_message("BAD dataset: type=%d view_type=%d ibk=%d bkt=%d",
                qset->type , qset->view_type , qset->dblk != NULL , qset->dblk->type ) ;
  }
 }
 first=0 ;
}
#endif
            MCW_strncpy( strlist[ii] , "??*BAD*??" , THD_MAX_PREFIX ) ;
         }
      }

      init_str = im3d->vinfo->anat_num ;
      label    = dset_choice[1] ;

   /*--- make a list of function names ---*/

   } else {
      int nn=0 , ndset=0 ; THD_3dim_dataset **dset_list=NULL , *dset=NULL;

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
        /*** sesstrail = 0 ; ***/
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
            dset = GET_SESSION_DSET(im3d->ss_now, ii, vv); if( ISVALID_DSET(dset) ) break;
/*            dset = im3d->ss_now->dsset_xform_table[ii][vv]; if( ISVALID_DSET(dset) ) break;*/
          }
        }
        if( ISVALID_DSET(dset) ){
          llen = strlen( dset->dblk->diskptr->prefix ) ;
          if( sesstrail ){
            st = THD_trailname(dset->dblk->diskptr->directory_name,1) ;
            if( st != NULL ){
              if( strlen(st) > MAX_SESSTRAIL_LEN )
                st += (strlen(st)-MAX_SESSTRAIL_LEN) ;
              llen += strlen(st) ;
            }
          }
          if( llen > ltop ) ltop = llen ;
        }
      }
      ltop = MIN(ltop,STRLIST_SIZE-24) ;  /* 06 Aug 2002 */

      for( ii=0 ; ii < ndset ; ii++ ){

         if( is_other ){
           dset = dset_list[ii] ; if( !ISVALID_DSET(dset) ) continue ;
         } else {
           for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ){
            dset = GET_SESSION_DSET(im3d->ss_now, ii, vv); if( ISVALID_DSET(dset) ) break;
/*             dset = im3d->ss_now->dsset_xform_table[ii][vv]; if( ISVALID_DSET(dset) ) break;*/
           }
         }

         if( ISVALID_DSET(dset) ){
           if( sesstrail ){
             st = THD_trailname(dset->dblk->diskptr->directory_name,1) ;
             if( st != NULL ){
               if( strlen(st) > MAX_SESSTRAIL_LEN )
                 st += (strlen(st)-MAX_SESSTRAIL_LEN) ;
             }
           } else {
             st = "\0" ;
           }
           strcpy(sst,st) ; strcat(sst,dset->dblk->diskptr->prefix) ;
           sprintf( strlist[nn] , "%-*s" , ltop,sst ) ;

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
#if 1
THD_3dim_dataset *qset ; static int first=1 ;
if( first ){
 for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ){
  qset = GET_SESSION_DSET(im3d->ss_now,ii,vv) ;
  if( qset != NULL ){
   INFO_message("BAD dataset: type=%d view_type=%d ibk=%d bkt=%d",
                qset->type , qset->view_type , qset->dblk != NULL , qset->dblk->type ) ;
  }
 }
 first=0 ;
}
#endif
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
   THD_3dim_dataset *temp_dset;

ENTRY("AFNI_finalize_dataset_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   old_sess = im3d->vinfo->sess_num ;     /* record current status */
   old_anat = im3d->vinfo->anat_num ;
   old_func = im3d->vinfo->func_num ;
   old_view = im3d->vinfo->view_type ;

#if 0
   STATUS_IM3D_TMASK(im3d) ;
   STATUS("clear tmask") ;
#endif
   IM3D_CLEAR_TMASK(im3d) ;                                /* Mar 2013 */
   IM3D_CLEAR_THRSTAT(im3d) ;                           /* 12 Jun 2014 */

   /*--- switch sessions ---*/

   if( wcall == im3d->vwid->view->choose_sess_pb ){

      DISABLE_INSTACORR(im3d) ; DESTROY_ICOR_setup(im3d->iset) ; /* 08 May 2009 */

      new_sess = cbs->ival ;
      if( new_sess < 0 || new_sess >= GLOBAL_library.sslist->num_sess ){
         BEEPIT ;
         WARNING_message("bad session index when finalizing choice") ;
         EXRETURN ;  /* bad! */
      }

      ss_new = GLOBAL_library.sslist->ssar[new_sess] ;

      /* find an anat in new session to match current anat */

      temp_dset = GET_SESSION_DSET(ss_new, old_anat, old_view);
      if( ISVALID_3DIM_DATASET(temp_dset) ){  /* are OK */
        new_anat = old_anat ;
      } else {
        for( ii=0 ; ii < ss_new->num_dsset ; ii++ ) {
          temp_dset = GET_SESSION_DSET(ss_new, ii, old_view);
          if( ISVALID_3DIM_DATASET(temp_dset) )
             new_anat = ii ; break ;
          }
      }
      if( new_anat < 0 ) new_anat = 0 ;  /* use 1st if no match */

      /* find a view to fit this chosen anat */

      temp_dset = GET_SESSION_DSET(ss_new, new_anat, old_view);
      if( ISVALID_3DIM_DATASET(temp_dset )) { /* are OK */
         new_view = old_view ;
      } else {
         for( vv=old_view-1 ; vv >= FIRST_VIEW_TYPE ; vv-- ) { /* look below */
            temp_dset = GET_SESSION_DSET(ss_new, new_anat, vv);
            if( ISVALID_3DIM_DATASET(temp_dset) ) break ;
         }
         if( vv >= FIRST_VIEW_TYPE ){  /* found it below */
            new_view = vv ;
         } else {                      /* look above */
            for( vv=old_view+1 ; vv <= LAST_VIEW_TYPE ; vv++ ) {
               temp_dset = GET_SESSION_DSET(ss_new, new_anat, vv);
               if( ISVALID_3DIM_DATASET(temp_dset) ) break ;
            }

            if( vv <= LAST_VIEW_TYPE ){  /* found it above */
               new_view = vv ;
            } else {
               BEEPIT ;
               WARNING_message("bad view code when finalizing choice") ;
               EXRETURN ;  /* bad news */
            }
         }
      }

      /* find a func in new session that fits the new view */

#define FINDAFUNC
#ifdef  FINDAFUNC
      temp_dset = GET_SESSION_DSET(ss_new, old_func, new_view);
      if( ISVALID_3DIM_DATASET(temp_dset) ){  /* are OK */
         new_func = old_func ;
      } else {
         for( ff=0 ; ff < ss_new->num_dsset ; ff++ ) { /* search */
            temp_dset = GET_SESSION_DSET(ss_new, ff, new_view);
            if( ISVALID_3DIM_DATASET(temp_dset) ) break ;
/*            if( ISVALID_3DIM_DATASET(ss_new->dsset_xform_table[ff][new_view]) ) break ;*/
         }

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
         BEEPIT ;
         WARNING_message("bad anat index when finalizing choice") ;
         EXRETURN ;  /* bad! */
      }

      /* find a view to fit this chosen anat */
      temp_dset = GET_SESSION_DSET(ss_new, new_anat, old_view);

      if( ISVALID_3DIM_DATASET(temp_dset) ){ /* are OK */
         new_view = old_view ;
      } else {
         for( vv=old_view-1 ; vv >= FIRST_VIEW_TYPE ; vv-- ) { /* look below */
            temp_dset = GET_SESSION_DSET(ss_new, new_anat, vv);
            if( ISVALID_3DIM_DATASET(temp_dset) ) break ;
         }

         if( vv >= FIRST_VIEW_TYPE ){  /* found it below */
            new_view = vv ;
         } else {                      /* look above */
            for( vv=old_view+1 ; vv <= LAST_VIEW_TYPE ; vv++ ){
               temp_dset = GET_SESSION_DSET(ss_new, new_anat, vv);
               if( ISVALID_3DIM_DATASET(temp_dset) ) break ;
            }

            if( vv <= LAST_VIEW_TYPE ){  /* found it above */
               new_view = vv ;
            } else {
               BEEPIT ;
               WARNING_message("bad func index when finalizing choice") ;
               EXRETURN ;  /* bad news */
            }
         }
      }

      /* find a func to match this view */

#ifdef FINDAFUNC
     temp_dset = GET_SESSION_DSET(ss_new, old_func, new_view);
     if( ISVALID_3DIM_DATASET(temp_dset) ) {
         new_func = old_func ;
      } else {
         for( ff=0 ; ff < ss_new->num_dsset ; ff++ ) { /* search */
            temp_dset = GET_SESSION_DSET(ss_new, ff, new_view);
            if( ISVALID_3DIM_DATASET(temp_dset) ) break ;
/*            if( ISVALID_3DIM_DATASET(ss_new->dsset_xform_table[ff][new_view]) ) break ;*/
         }

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
      if( new_func < 0 || new_func >= ss_new->num_dsset ){  /* should not happen */
         BEEPIT ;
         WARNING_message("bad func index when finalizing choice!") ;
         EXRETURN ;  /* bad! */
      }

      /* find a view to fit this chosen func */

      temp_dset = GET_SESSION_DSET(ss_new, new_func, old_view);
      if( ISVALID_3DIM_DATASET(temp_dset) ) {
         new_view = old_view ;
      } else {
         for( vv=old_view-1 ; vv >= FIRST_VIEW_TYPE ; vv-- ) { /* look below */
            temp_dset = GET_SESSION_DSET(ss_new, new_func, vv);
            if( ISVALID_3DIM_DATASET(temp_dset) ) break ;
         }

         if( vv >= FIRST_VIEW_TYPE ){  /* found it below */
            new_view = vv ;
         } else {                      /* look above */
            for( vv=old_view+1 ; vv <= LAST_VIEW_TYPE ; vv++ ) {
               temp_dset = GET_SESSION_DSET(ss_new, new_func, vv);
               if( ISVALID_3DIM_DATASET(temp_dset) ) break ;
            }

            if( vv <= LAST_VIEW_TYPE ){  /* found it above */
               new_view = vv ;
            } else {                     /* should not happen */
               BEEPIT ;
               WARNING_message("bad view index when finalizing choice!") ;
               EXRETURN ;  /* bad news */
            }
         }
      }

      /* find an anat to go with the new view (this is NOT optional) */

      temp_dset = GET_SESSION_DSET(ss_new, old_anat, new_view);
      if( ISVALID_3DIM_DATASET(temp_dset) ) {
         new_anat = old_anat ;
      } else {
         for( ff=0 ; ff < ss_new->num_dsset ; ff++ ) { /* search */
            temp_dset = GET_SESSION_DSET(ss_new, ff, new_view);
            if( ISVALID_3DIM_DATASET(temp_dset) ) break ;
         }
/*          if( ISVALID_3DIM_DATASET(ss_new->dsset_xform_table[ff][new_view]) ) break ; */

         if( ff < ss_new->num_dsset ) new_anat = ff ;  /* found one */
      }
      if( new_anat < 0 ){  /* should not happen */
         BEEPIT ;
         WARNING_message("bad anat index when finalizing choice!") ;
         EXRETURN ;  /* bad! */
      }

      /* 03 Aug 2007: turn 'See Overlay' on? */

      if( !im3d->vinfo->func_visible && im3d->vinfo->func_visible_count == 0 ){
        AFNI_SEE_FUNC_ON(im3d) ; OPEN_PANEL(im3d,func) ;
        im3d->vinfo->func_init_subbricks = 1 ;  /* 12 Jan 2017 */
      }

   /*--- switch to Hell? ---*/

   } else {
      BEEPIT ;
      WARNING_message("bad switch?!") ;
      EXRETURN ;  /* bad! */
   }

   /*--- make sure all values are set OK-ly ---*/

   if( new_view < 0 || new_sess < 0 || new_anat < 0 || new_func < 0 ){
     ERROR_message("Something bad happened when trying to 'Switch'\a") ;
     EXRETURN ;  /* bad! */
   }

   /*- beep & flash viewing control box if view type changes -*/

   if( old_view != new_view ){
     static int nwarn=0 ;
     MCW_set_bbox( im3d->vwid->view->view_bbox , 1 << new_view ) ;
     UNCLUSTERIZE(im3d) ;  /* 14 Feb 2008 */

     /* this stuff is for Adam Thomas -- 18 Oct 2006 */

     if( nwarn < 3 )
       WARNING_message("Forced switch from '%s' to '%s' [#%d]",
                       VIEW_typestr[old_view] , VIEW_typestr[new_view] , nwarn+1 ) ;

     if( AFNI_yesenv("AFNI_FLASH_VIEWSWITCH") ){

       if( nwarn==0 && wcall != NULL ){
         char str[256] ;
         sprintf(str," \nForced switch from\n  '%s'\nto\n  '%s'\n ",
                     VIEW_typestr[old_view] , VIEW_typestr[new_view] ) ;
         (void)MCW_popup_message( wcall, str, MCW_USER_KILL | MCW_TIMER_KILL ) ;
       }

       if( wcall != NULL ){
         for( ii=0 ; ii < 3 ; ii++ ){
           MCW_invert_widget(im3d->vwid->view->view_bbox->wframe ); RWC_sleep(16);
           MCW_invert_widget(im3d->vwid->view->view_bbox->wrowcol); RWC_sleep(16);
           MCW_invert_widget(wcall) ;
           MCW_invert_widget(im3d->vwid->view->view_bbox->wframe ); RWC_sleep(16);
           MCW_invert_widget(im3d->vwid->view->view_bbox->wrowcol); RWC_sleep(16);
           MCW_invert_widget(wcall) ;
         }
       }
     }

     nwarn++ ;  /* 16 Sep 2009 */
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

   if( AFNI_yesenv("AFNI_FLASH_VIEWSWITCH") && old_view != new_view ){ /* ending flash */
     BEEPIT ;
     for( ii=0 ; ii < 3 ; ii++ ){
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

   if( wcall == im3d->vwid->view->choose_func_pb ){  /* ZSS 25 Feb 2010 */
      AFNI_set_dset_pbar((XtPointer)im3d);
   }

   /* check obliquity of overlay and underlay */
   /* pop up warning if necessary */

   if(wcall == im3d->vwid->view->choose_func_pb) {
     AFNI_check_obliquity(wcall,
            GET_SESSION_DSET(ss_new, new_func, 0),
            GET_SESSION_DSET(ss_new, new_anat, 0) );
   } else {
     AFNI_check_obliquity(wcall,
            GET_SESSION_DSET(ss_new, new_anat, 0),
            GET_SESSION_DSET(ss_new, new_func, 0) );
   }
   CLU_setup_alpha_tables(im3d) ;

   EXRETURN ;
}

/*-----------------------------------------------------------*/
/* check dataset for obliquity and pop-up warning if oblique */

void AFNI_check_obliquity(Widget w, THD_3dim_dataset *dset,
                          THD_3dim_dataset *rset)
{
   double angle;
   char str[1024], sidcombo[256], *sid1=NULL, *sid2=NULL;
   static char *warncombos=NULL;
   static int num_warn = 0;

   ENTRY("AFNI_check_obliquity");
   if( !ISVALID_DSET(dset) ) EXRETURN ;

   if(AFNI_yesenv("AFNI_NO_OBLIQUE_WARNING")) EXRETURN;

   if(AFNI_yesenv("AFNI_ONE_OBLIQUE_WARNING") && num_warn) EXRETURN;

   angle = dset_obliquity_angle_diff(dset, rset, OBLIQ_ANGLE_THRESH);
   if(angle == 0.0) EXRETURN ;

   /* A simple way to keep AFNI form complaining about obliquity all the time */
   sid1= DSET_IDCODE_STR(dset);
   if (rset) sid2 = DSET_IDCODE_STR(rset);
   else sid2 = "NA";
   if (!sid1) sid1="NO_ID1";
   if (!sid2) sid2="NO_ID2";

   if (strcmp(sid1,sid2)<0) {
      sprintf(sidcombo,"-%s_WITH_%s-", sid1, sid2);
   } else {
      sprintf(sidcombo,"-%s_WITH_%s-", sid2, sid1);
   }

   if (!warncombos) {
      warncombos = (char *)malloc(sizeof(char)*(strlen(sidcombo)+1));
      strcpy(warncombos,sidcombo);
   } else {
      if (strstr(warncombos,sidcombo)) EXRETURN;
      else {
         warncombos = (char *)realloc(warncombos,
                           sizeof(char)*(strlen(sidcombo)+strlen(warncombos)+1));
         warncombos = strcat(warncombos, sidcombo);
      }
   }

   if (AFNI_yesenv("AFNI_ONE_OBLIQUE_WARNING")) {
      snprintf( str, 1022*sizeof(char),
   " The underlay/overlay pair of datasets (%s/%s) have oblique angle \n"
   " difference of %f degrees. This may cause them to appear out of alignment \n"
   " in the viewer.\n"
   "\n"
   "  If you are performing spatial transformations on an oblique dset, \n"
   "  or viewing/combining it with volumes of differing obliquity,\n"
   "  you should consider running: \n"
   "     3dWarp -deoblique \n"
   "  on this and other oblique datasets in the same session.\n"
   "\n"
   " ** Other oblique warnings will be muted because AFNI_ONE_OBLIQUE_WARNING\n"     "    is set to YES. Consider setting it back to NO, obliquity warnings\n"
   "    are much less overwhelming now.\n",
         DSET_PREFIX(dset), rset ? DSET_PREFIX(rset):"NA",
         angle );
   } else {
      snprintf( str, 1022*sizeof(char),
   " The underlay/overlay pair of datasets (%s/%s) have oblique angle \n"
   " difference of %f degrees. This may cause them to appear out of alignment \n"
   " in the viewer.\n"
   "\n"
   "  If you are performing spatial transformations on an oblique dset, \n"
   "  or viewing/combining it with volumes of differing obliquity,\n"
   "  you should consider running: \n"
   "     3dWarp -deoblique \n"
   "  on this and other oblique datasets in the same session.\n"
   "\n"
   " ** Warnings for the same data pair will be muted.\n",
         DSET_PREFIX(dset), rset ? DSET_PREFIX(rset):"NA",
         angle);
   }
   (void) MCW_popup_message( w , str, MCW_USER_KILL | MCW_TIMER_KILL ) ;

   ++num_warn;
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

   if( IM3D_VALID(im3d) && im3d->vwid->file_dialog != NULL )
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

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

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
                  BEEPIT ;
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
               BEEPIT ;
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
               BEEPIT ;
               (void) MCW_popup_message( w ,
                                           "******************************\n"
                                           "** Cannot read any datasets **\n"
                                           "******************************"
                                         , MCW_USER_KILL | MCW_TIMER_KILL ) ;

            } else if( GLOBAL_library.sslist->num_sess >= THD_MAX_NUM_SESSION ){

STATUS("too many sessions") ;
               BEEPIT ;
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
                   dset = GET_SESSION_DSET(new_ss,qd,vv) ;
/*                   dset = new_ss->dsset_xform_table[qd][vv] ;*/
                   if( dset != NULL ){
                     PARENTIZE( dset , NULL ) ;
                     AFNI_inconstancy_check(NULL,dset) ; /* 06 Sep 2006 */
                   }
               } }
               AFNI_inconstancy_check(im3d,NULL); /* 06 Sep 2006 */

               /* 20 Dec 2001: if have global datasets, put them in here */

               THD_append_sessions( new_ss , GLOBAL_library.session ) ;

               /* if we were living with a dummy, fix that */

               if( GLOBAL_library.have_dummy_dataset ) UNDUMMYIZE ;

               /* put the new session into place in the list of sessions */

STATUS("adding new session to list") ;
               GLOBAL_library.sslist->ssar[GLOBAL_library.sslist->num_sess] = new_ss ;
               (GLOBAL_library.sslist->num_sess)++ ;
               THD_reconcile_parents( GLOBAL_library.sslist ) ;
               AFNI_force_adoption( new_ss , GLOBAL_argopt.warp_4D ) ; /* 28 Jan 2011 */
               AFNI_make_descendants( GLOBAL_library.sslist ) ;        /* 28 Jan 2011 */

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

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

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

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

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
            BEEPIT ;
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

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   MCW_choose_string( w ,
    "Complete http:// or ftp:// address of dataset (.HEAD or .mnc or .mnc.gz):\n"
    "Examples: ftp://afni.nimh.nih.gov/AFNI/data/astrip+orig.HEAD\n"
    "          https://afni.nimh.nih.gov/afni/norm305.mnc.gz"
     , NULL , AFNI_finalize_read_Web_CB , (XtPointer) im3d ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void AFNI_finalize_read_Web_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   THD_3dim_dataset *dset, *temp_dset ;
   RwcPointer_array *dsar ;
   THD_session *ss = GLOBAL_library.sslist->ssar[im3d->vinfo->sess_num] ;
   char str[256] ;
   int nds,dd,vv , nn, na=-1,nf=-1 ,nts ;

ENTRY("AFNI_finalize_read_Web_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   if( cbs->reason  != mcwCR_string ||
       cbs->cval    == NULL         ||
       cbs->cval[0] == '\0'         ||
       (strstr(cbs->cval,"http://")==NULL && strstr(cbs->cval,"ftp://")==NULL) ){

      (void) MCW_popup_message( im3d->vwid->dmode->read_Web_pb ,
                                  " \n** Illegal URL **\n " ,
                                MCW_USER_KILL | MCW_TIMER_KILL      ) ;

      BEEPIT ; EXRETURN ;
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
         BEEPIT ; EXRETURN ;
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
        BEEPIT ; EXRETURN ;
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
          SET_SESSION_DSET(dset,ss,nn,vv) ;
/*        ss->dsset_xform_table[nn][vv] = dset ;*/
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

   if( nds == 0 ){ EXRETURN; }

   /*-- prepare to switch back to AFNI --*/

   if( na >= 0 ) im3d->vinfo->anat_num = na ; /* 1st new anat in current view */
   if( nf >= 0 ) im3d->vinfo->func_num = nf ; /* 1st new func in current view */

   if( GLOBAL_library.have_dummy_dataset ){   /* switch away from dummy dataset */
     UNDUMMYIZE ;
     if( na < 0 && ss->num_dsset > 1 ){
       im3d->vinfo->anat_num = 1 ;
       im3d->vinfo->func_num = 1 ;            /* 07 Sep 2006 (oops) */
       for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ){
         temp_dset = GET_SESSION_DSET(ss,1,vv);
         if( ISVALID_DSET(temp_dset) ){
            im3d->vinfo->view_type = vv; break;
         }
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

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

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
   THD_3dim_dataset *dset, *temp_dset ;

ENTRY("AFNI_rescan_session_OLD") ;
{ char str[256]; sprintf(str,"session index %d\n",sss); STATUS(str); }

   if( GLOBAL_library.have_dummy_dataset ){ RETURN(0) ; }

#if 0
fprintf(stderr,"Enter AFNI_rescan_session_OLD on session index %d\n",sss) ;
#endif

   /*--- sanity checks ---*/

   if( sss < 0 || sss >= GLOBAL_library.sslist->num_sess ){ RETURN(0) ; }

   old_ss = GLOBAL_library.sslist->ssar[sss] ;
   if( ! ISVALID_SESSION(old_ss) || old_ss->is_collection ){ RETURN(0); }

   if( old_ss == GLOBAL_library.session ) RETURN(0) ;  /* 21 Dec 2001 */

   if( ! THD_is_directory(old_ss->sessname) ) RETURN(0) ; /* 02 Jun 2016 */

   /*--- Make sure that the dataset choosers are closed.
         Since these are just instances of the generic strlist
         chooser, and we can't tell what is being chosen just now,
         we'll just forcibly close the strlist chooser no matter what. ---*/

   POPDOWN_strlist_chooser ;

   /*--- mark all datasets in the old session for deletion from memory ---*/

STATUS("marking old session datasets") ;

   nold = old_ss->num_dsset ;

   for( ii=0 ; ii < old_ss->num_dsset ; ii++ )
      for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ){
         temp_dset = GET_SESSION_DSET(old_ss, ii, vv);
         if( ISVALID_3DIM_DATASET(temp_dset) ){
/*         if( ISVALID_3DIM_DATASET(old_ss->dsset_xform_table[ii][vv]) )*/
            if( DSET_in_global_session(temp_dset) )
               SET_SESSION_DSET(NULL, old_ss, ii, vv);
/*            if( DSET_in_global_session(old_ss->dsset_xform_table[ii][vv]) )*/
/*               old_ss->dsset_xform_table[ii][vv] = NULL ; */  /* will be added back in later */
            else
               DSET_MARK_FOR_DEATH( temp_dset ) ;
/*               DSET_MARK_FOR_DEATH( old_ss->dsset_xform_table[ii][vv] ) ;*/
         }
      }
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
      fprintf(stderr,"\n*** Fatal error: Rescan of session %s finds nothing!\n",
              old_ss->sessname ) ;
      EXIT(1) ;
   }

   myXtFree( old_ss ) ;  /* no longer need this */

   /* set parent pointers */

STATUS("PARENTIZE-ing datasets in new session") ;

   new_ss->parent = NULL ;
   for( ii=0 ; ii < new_ss->num_dsset ; ii++ ){
     for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ){
       dset = GET_SESSION_DSET(new_ss, ii, vv);
/*       dset = new_ss->dsset_xform_table[ii][vv] ;*/
       if( dset != NULL ){
         PARENTIZE( dset, NULL ) ;
/*         PARENTIZE( new_ss->dsset_xform_table[ii][vv] , NULL ) ;*/
         AFNI_inconstancy_check(NULL,dset) ; /* 06 Sep 2006 */
       }
   } }
   AFNI_inconstancy_check(NULL,NULL);

   /* put the new session into place in the list of sessions */

   GLOBAL_library.sslist->ssar[sss] = new_ss ;

   /* 20 Dec 2001: add the global datasets back in, if any */

   THD_append_sessions( new_ss , GLOBAL_library.session ) ;

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
            for( ii=0 ; ii < new_ss->num_dsset ; ii++ ) {
               temp_dset = GET_SESSION_DSET(new_ss, ii, vv);
               if( ISVALID_3DIM_DATASET(temp_dset)) break ;
/*               if( ISVALID_3DIM_DATASET(new_ss->dsset_xform_table[ii][vv]) ) break ;*/
            }
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
               for( ii=0 ; ii < new_ss->num_dsset ; ii++ ) {
                  temp_dset = GET_SESSION_DSET(new_ss, ii, vv);
                  if( ISVALID_3DIM_DATASET(temp_dset) ) break ;
/*                if( ISVALID_3DIM_DATASET(new_ss->dsset_xform_table[ii][vv]) ) break ;*/
               }
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
   int vv , ii , nr , na_new=0 ;
   THD_session  *new_ss , *old_ss ;
   THD_slist_find find ;
   THD_3dim_dataset *new_dset, *temp_dset ;

ENTRY("AFNI_rescan_session_NEW") ;
{ char str[256]; sprintf(str,"session index %d\n",sss); STATUS(str); }

   if( GLOBAL_library.have_dummy_dataset ){ RETURN(0); }

#if 0
fprintf(stderr,"Enter AFNI_rescan_session_NEW on session index %d\n",sss) ;
#endif

   /*--- sanity checks ---*/

   if( sss < 0 || sss >= GLOBAL_library.sslist->num_sess ){ RETURN(0); }

   old_ss = GLOBAL_library.sslist->ssar[sss] ;
   if( ! ISVALID_SESSION(old_ss) || old_ss->is_collection ){ RETURN(0); }

                                     /* can't rescan global session */
   if( old_ss == GLOBAL_library.session ) RETURN(0); /* 21 Dec 2001 */

   if( ! THD_is_directory(old_ss->sessname) ) RETURN(0) ; /* 02 Jun 2016 */

   /*--- read in the session again, into a new THD_session struct ---*/

STATUS("rescanning session now:") ;
STATUS(old_ss->sessname) ;

   new_ss = THD_init_session( old_ss->sessname ) ;
   if( ! ISVALID_SESSION(new_ss) ){ RETURN(0); } /* this is BAD */

   /*--- scan datasets and remove those
         that already exist in this session ---*/

   for( ii=0 ; ii < new_ss->num_dsset ; ii++ ){
     for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ){
       new_dset = GET_SESSION_DSET(new_ss, ii, vv);
/*     new_dset = new_ss->dsset_xform_table[ii][vv] ;*/
       if( ISVALID_DSET(new_dset) ){
         find = THD_dset_in_session( FIND_IDCODE, &(new_dset->idcode), old_ss );
         if( find.dset == NULL ){
          find = THD_dset_in_session(FIND_PREFIX, DSET_PREFIX(new_dset),old_ss);
          if( find.dset != NULL && find.view_index != vv ) find.dset = NULL ;
         }
         if( find.dset != NULL ){
           DSET_delete(new_dset);
           SET_SESSION_DSET(NULL, new_ss, ii, vv);
           /* new_ss->dsset_xform_table[ii][vv] = NULL;*/
         }
       }
     }
   }

   /*--- now scan survivors and put them
         at the end of the existing session ---*/

   for( ii=0 ; ii < new_ss->num_dsset ; ii++ ){
     for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )   /* see if row is empty */
       if( GET_SESSION_DSET(new_ss, ii, vv) != NULL ) break ;
/*       if( new_ss->dsset_xform_table[ii][vv] != NULL ) break ;*/
     if( vv > LAST_VIEW_TYPE ) continue ;          /* empty row ==> skip  */
     AFNI_inconstancy_check(NULL,GET_SESSION_DSET(new_ss, ii,vv)) ;  /* 06 Sep 2006 */
/*     AFNI_inconstancy_check(NULL,new_ss->dsset_xform_table[ii][vv]) ;*/  /* 06 Sep 2006 */
     nr = old_ss->num_dsset ;                      /* next row in old_ss  */
     if( nr >= THD_MAX_SESSION_SIZE ) break ;      /* old session is full */
     for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ) {    /* copy new row to old */
         temp_dset = GET_SESSION_DSET(new_ss, ii, vv);
         SET_SESSION_DSET(temp_dset, old_ss, nr, vv);
/*       old_ss->dsset_xform_table[nr][vv] = new_ss->dsset_xform_table[ii][vv];*/
     }
     old_ss->num_dsset++ ;  na_new++ ;             /* 1 more row in old   */
   }
   if( na_new == 0 ) RETURN(0) ;                   /* 10 Nov 2005 */
   AFNI_inconstancy_check(NULL,NULL);              /* 06 Sep 2006 */

   /*-- 15 Jan 2003: purge all datasets from memory (for Hauke Heekeren) --*/

   for( ii=0 ; ii < old_ss->num_dsset ; ii++ )
     for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ) {
       temp_dset = GET_SESSION_DSET(old_ss, ii, vv);
       if( temp_dset != NULL ) DSET_unload(temp_dset);
/*     if( old_ss->dsset_xform_table[ii][vv] != NULL ) DSET_unload(old_ss->dsset_xform_table[ii][vv]);*/
     }
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

   if( GLOBAL_library.have_dummy_dataset ){ EXRETURN ; }

   INIT_SARR( dlist ) ;

   for( iss=0 ; iss < GLOBAL_library.sslist->num_sess ; iss++ ){
      ss = GLOBAL_library.sslist->ssar[iss] ;
      if( ss->is_collection ) continue ;
      ADDTO_SARR(dlist,ss->sessname) ;
   }

   if( dlist->num == 0 ){ DESTROY_SARR(dlist) ; EXRETURN ; }

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

void AFNI_anatmode_CB( Widget w, XtPointer cd, XtPointer cb )
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

void AFNI_funcmode_CB( Widget w, XtPointer cd, XtPointer cb )
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
   if( GLOBAL_library.have_dummy_dataset ){ EXRETURN ; }

   if( num_dset > 0 && strlist != NULL )
      for( id=0 ; id < num_dset ; id++ ) myXtFree(strlist[id]) ;
   myXtFree(idclist) ; myXtFree(strlist) ;

   vv = im3d->vinfo->view_type ;  /* select view type */

   /** scan once to find longest string name **/

   ltop = 4 ;
   for( iss=0 ; iss < GLOBAL_library.sslist->num_sess ; iss++ ){
      ss = GLOBAL_library.sslist->ssar[iss] ;
      if( ss->is_collection ) continue ;

      for( id=0 ; id < ss->num_dsset ; id++ ){
         dset = GET_SESSION_DSET(ss, id, vv);
/*         dset = ss->dsset_xform_table[id][vv] ;*/
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
      if( ss->is_collection ) continue ;

      /* check anat datasets */

      for( id=0 ; id < ss->num_dsset ; id++ ){
         dset = GET_SESSION_DSET(ss, id, vv);
/*         dset = ss->dsset_xform_table[id][vv] ;*/
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
      (void) MCW_popup_message( w ,
                 "*******************************\n"
                 "** No datasets are available **\n"
                 "** to write out to disk.     **\n"
                 "*******************************"  ,
              MCW_USER_KILL | MCW_TIMER_KILL ) ;
      BEEPIT ; EXRETURN ;
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

/*--------------------------------------------------------------------------*/

static THD_3dim_dataset *saveas_iset = NULL ;

void AFNI_saveas_dataset_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *) cd ;
   char *label ;

ENTRY("AFNI_saveas_dataset_CB") ;

   saveas_iset = NULL ;

   if( ! IM3D_VALID(im3d) || w == NULL ||
       ! XtIsWidget(w)    || ! XtIsRealized(w) ) EXRETURN ;
   if( GLOBAL_library.have_dummy_dataset ){ EXRETURN ; }

   if( w == im3d->vwid->dmode->saveas_anat_pb ){
     saveas_iset = im3d->anat_now ; label = "Underlay Prefix" ;
   } else if( w == im3d->vwid->dmode->saveas_func_pb ){
     saveas_iset = im3d->fim_now  ; label = "Overlay Prefix" ;
   } else {
     BEEPIT ; WARNING_message("SaveAs code improperly executed") ; EXRETURN ;
   }

   MCW_choose_string( w, label, NULL, AFNI_saveas_finalize_CB, NULL ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------------*/

void AFNI_saveas_finalize_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   char *prefix ;
   THD_3dim_dataset *oset ;

ENTRY("AFNI_saveas_finalize_CB") ;

   if( cbs->reason != mcwCR_string || saveas_iset == NULL ){
     BEEPIT; WARNING_message("SaveAs code improperly invoked"); EXRETURN;
   }

   prefix = cbs->cval ;
   if( !THD_filename_ok(prefix) ){
     BEEPIT; WARNING_message("SaveAs code improperly invoked"); EXRETURN;
   }

   DSET_load(saveas_iset) ;
   if( !DSET_LOADED(saveas_iset) ){
     BEEPIT; WARNING_message("SaveAs code improperly invoked"); EXRETURN;
   }

   MCW_invert_widget(w) ;
   oset = EDIT_full_copy( saveas_iset , prefix ) ;
   MCW_flash_widget(1,w) ;
   THD_force_ok_overwrite(1) ;
   DSET_write(oset) ;
   MCW_flash_widget(1,w) ;
   THD_force_ok_overwrite(0) ;
   WROTE_DSET(oset) ; DSET_delete(oset) ;

   saveas_iset = NULL ; POPDOWN_string_chooser ; MCW_invert_widget(w) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

void AFNI_writeout_dataset( THD_3dim_dataset *dset , char *prefix )
{
   THD_3dim_dataset *oset ;

ENTRY("AFNI_writeout_dataset") ;

   if( !ISVALID_DSET(dset) || !THD_filename_ok(prefix) ) EXRETURN ;
   DSET_load(dset) ;            if( !DSET_LOADED(dset) ) EXRETURN ;

   oset = EDIT_full_copy( dset , prefix ) ;
   THD_force_ok_overwrite(1) ;
   DSET_write(oset) ;
   THD_force_ok_overwrite(0) ;
   WROTE_DSET(oset) ; DSET_delete(oset) ;

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
   if( GLOBAL_library.have_dummy_dataset ){ EXRETURN ; }

   if( w == im3d->vwid->dmode->write_anat_pb ){         /* write anatomy */
      dset       = im3d->anat_now ;
      resam_mode = im3d->vinfo->anat_resam_mode ;

   } else if( w == im3d->vwid->dmode->write_func_pb ){  /* write function */
      dset       = im3d->fim_now ;
      resam_mode = im3d->vinfo->func_resam_mode ;
   }

   if( ISVALID_DSET(dset) && dset->dblk->diskptr->allow_directwrite == 1 ){
     INFO_message("Direct write of dataset '%s'",DSET_BRIKNAME(dset)) ;
     if (!AFNI_yesenv("AFNI_GUI_WRITE_AS_DECONFLICT")) { /* ZSS April 11 2010 */
        DSET_overwrite(dset) ;
     } else {
        char pfx[THD_MAX_PREFIX] ; /* to hold old one */
        MCW_strncpy( pfx , DSET_PREFIX(dset) , THD_MAX_PREFIX ) ;
        DSET_write(dset);
        /* Now put old prefix back in case there was deconflicting */
        EDIT_dset_items( dset , ADN_prefix , pfx , ADN_none ) ;
     }

     EXRETURN ;
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

       BEEPIT ; EXRETURN;
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

      BEEPIT ;
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
      if( ss->is_collection    ) continue ;

      /* loop over datasets in this session */

      for( jdd=0 ; jdd < ss->num_dsset ; jdd++ ){
         for( kvv=FIRST_VIEW_TYPE ; kvv <= LAST_VIEW_TYPE ; kvv++ ){
            dset = GET_SESSION_DSET(ss, jdd, kvv);
/*            dset = ss->dsset_xform_table[jdd][kvv] ;*/

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
      if( ss->is_collection    ) continue ;

      if( ss == GLOBAL_library.session ) continue ; /* 21 Dec 2001 */

      /* loop over datasets in this session */

      for( jdd=0 ; jdd < ss->num_dsset ; jdd++ ){
         for( kvv=FIRST_VIEW_TYPE ; kvv <= LAST_VIEW_TYPE ; kvv++ ){
              dset = GET_SESSION_DSET(ss, jdd, kvv);
/*            dset = ss->dsset_xform_table[jdd][kvv] ;*/

            if( ISVALID_3DIM_DATASET(dset) &&    /* good dset */
                dset->death_mark == DOOMED   ){  /* alas, poor Yorick */

               kill_me = (kvv == VIEW_ORIGINAL_TYPE) ? False : kill_files ;
               THD_delete_3dim_dataset( dset , kill_me ) ;
               myXtFree( dset ) ;
               SET_SESSION_DSET(NULL, ss, jdd, kvv);
/*               ss->dsset_xform_table[jdd][kvv] = NULL ;*/
               num_killed ++ ;
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
         s1  = fabsf(im3d->fim_now->stats->bstat[iv].min) ,
         s2  = fabsf(im3d->fim_now->stats->bstat[iv].max) ;
         rrr = (s1 < s2) ? s2 : s1 ;                    /* largest fim */
       } else {
         rrr = DEFAULT_FIM_SCALE ;                      /* don't have brick stats */
       }
     } else {
       rrr = DEFAULT_FIM_SCALE ;                        /* don't have brick stats */
     }
   }
   if( rrr > 1.0f && rrr < DEFAULT_FIM_SCALE ){
     float ppp = AFNI_numenv("AFNI_AUTORANGE_POWER") ;
     if( ppp > 0.0f && ppp < 1.0f ) rrr = powf(rrr,ppp) ;
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

void AFNI_range_bbox_CB( Widget w, XtPointer cd, XtPointer cb )
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

      if( PBAR_FULLRANGE && !IM3D_ULAY_COHERENT(im3d) ){ /* 10 Jun 2014 */
        STATUS("incoherent ulay -- patching") ;
        ERROR_message("AFNI_range_bbox_CB: incoherent ulay -- patching") ;
        AFNI_assign_ulay_bricks(im3d) ;
      }

      if( PBAR_FULLRANGE ){
        AFNI_redisplay_func_ignore(1) ;
        AFNI_pbar_topset(im3d,im3d->vinfo->fim_range) ;
        AFNI_redisplay_func_ignore(0) ;
      }
      else HINTIZE_pbar(im3d) ;

      AV_SENSITIZE( im3d->vwid->func->range_av , ! new_auto ) ;

      AFNI_redisplay_func( im3d ) ;

      AFNI_range_lock_carryout(im3d) ;  /* 23 Feb 2004 */
   }

   EXRETURN ;
}

/*----------------------------------------------------------------
   called when the user toggles the percentile button
------------------------------------------------------------------*/

void AFNI_perc_bbox_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

ENTRY("AFNI_perc_bbox_CB") ;

   if( ! IM3D_VALID(im3d) ||
       w != im3d->vwid->func->perc_bbox->wbut[PERC_AUTOBUT] ) EXRETURN ;

   im3d->cont_perc_thr = MCW_val_bbox(im3d->vwid->func->perc_bbox);
   flush_3Dview_sort(im3d, "T");

   AFNI_redisplay_func( im3d ) ;
   AFNI_thresh_lock_carryout(im3d) ;

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

   if( PBAR_FULLRANGE && !IM3D_ULAY_COHERENT(im3d) ){ /* 10 Jun 2014 */
     STATUS("incoherent ulay -- patching") ;
     ERROR_message("AFNI_range_av_CB: incoherent ulay -- patching") ;
     AFNI_assign_ulay_bricks(im3d) ;
   }

   if( PBAR_FULLRANGE ){
     AFNI_redisplay_func_ignore(1) ;
     AFNI_pbar_topset(im3d,im3d->vinfo->fim_range) ;
     AFNI_redisplay_func_ignore(0) ;
   }
   else HINTIZE_pbar(im3d) ;

   AFNI_redisplay_func( im3d ) ;

   AFNI_range_lock_carryout(im3d) ;  /* 23 Feb 2004 */

   EXRETURN ;
}

/*----------------------------------------------------------------
   called when the user toggles the posfunc button
------------------------------------------------------------------*/

void AFNI_inten_bbox_CB( Widget w, XtPointer cd, XtPointer cb )
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

      if( PBAR_FULLRANGE && !IM3D_ULAY_COHERENT(im3d) ){ /* 10 Jun 2014 */
        STATUS("incoherent ulay -- patching") ;
        ERROR_message("AFNI_inten_bbox_CB: incoherent ulay -- patching") ;
        AFNI_assign_ulay_bricks(im3d) ;
      }

      AFNI_redisplay_func_ignore(1) ;

      HIDE_SCALE(im3d) ;
      if( pbar->bigmode ){               /* 30 Jan 2003 */
        int npane=pbar->num_panes ;
        float pmin , pmax ;
        pmax = (pbar->big31) ? pbar->bigtop
                             : pbar->pval_save[npane][0    ][jm] ;
        pmin = (pbar->big31) ? ( (jm==1) ? 0.0f : -pmax )
                             : pbar->pval_save[npane][npane][jm] ;
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

      if( PBAR_FULLRANGE ) AFNI_pbar_topset(im3d,im3d->vinfo->fim_range) ;
      else                 HINTIZE_pbar(im3d) ;

      AFNI_redisplay_func_ignore(0) ;
      AFNI_redisplay_func( im3d ) ;
      if( AFNI_count_controllers() > 1 && AFNI_check_pbar_lock() ) /* 03 Jul 2014 */
        AFNI_redisplay_func_all( im3d ) ;
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

   if( PBAR_FULLRANGE && !IM3D_ULAY_COHERENT(im3d) ){ /* 10 Jun 2014 */
     STATUS("incoherent ulay -- patching") ;
     ERROR_message("AFNI_reset_func_range: incoherent ulay -- patching") ;
     AFNI_assign_ulay_bricks(im3d) ;
   }

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

   if( PBAR_FULLRANGE ){
     AFNI_redisplay_func_ignore(1) ;
     AFNI_pbar_topset(im3d,im3d->vinfo->fim_range) ;
     AFNI_redisplay_func_ignore(0) ;
   }
   else HINTIZE_pbar(im3d) ;

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

#if 0
   STATUS_IM3D_TMASK(im3d) ;
   STATUS("clear tmask") ;
#endif
   IM3D_CLEAR_TMASK(im3d) ;                                /* Mar 2013 */
   IM3D_CLEAR_THRSTAT(im3d) ;                           /* 12 Jun 2014 */

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
       AFNI_enforce_throlayx(im3d) ; /* 13 Aug 2010 */
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
   } else if( dothr ){
     if( im3d->vinfo->fix_pval && im3d->vinfo->fixed_pval > 0.0f )
       AFNI_set_pval(im3d,im3d->vinfo->fixed_pval) ;
     else if( im3d->vinfo->fix_qval && im3d->vinfo->fixed_qval > 0.0f )
       AFNI_set_qval(im3d,im3d->vinfo->fixed_qval) ;
   }


   FIX_SCALE_SIZE(im3d) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------------*/
/*! Prepare a bucket label for a menu.
    21 Jun 2004: modified to allow label length to be different from 14.
----------------------------------------------------------------------------*/

static int force_label_resize = 0 ;
void AFNI_force_bucket_label_resize( int ff ){ force_label_resize = ff ; }

char * AFNI_bucket_label_CB( MCW_arrowval *av , XtPointer cd )
{
   static THD_3dim_dataset *dset_last = NULL ;
   static int               nsiz_last = 4 ;

   THD_3dim_dataset *dset = (THD_3dim_dataset *) cd ;
   static char *fmt[3]={NULL,NULL,NULL}, sfmt[THD_MAX_SBLABEL],blab[26+THD_MAX_SBLABEL] ;
   int nlab ;
   static int nlab_old = 0 ;

ENTRY("AFNI_bucket_label_CB") ;

   /** 04 May 2005: customize width to this dataset **/

   if( dset != dset_last && ISVALID_DSET(dset) || force_label_resize ){
     int nvals,kk,blw,mblw=4 ; char *lab ;
     dset_last = dset ;
     nvals = DSET_NVALS(dset) ;
     for( kk=0 ; kk < nvals ; kk++ ){
       lab = DSET_BRICK_LAB(dset,kk) ;
       if( lab != NULL ){ blw=strlen(lab) ; if(blw>mblw)mblw=blw ; }
     }
     if( mblw > THD_MAX_SBLABEL ) mblw = THD_MAX_SBLABEL ;
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
       fmt[0] = malloc(THD_MAX_SBLABEL); fmt[1] = malloc(THD_MAX_SBLABEL);
       fmt[2] = malloc(THD_MAX_SBLABEL);
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
     sprintf(blab," #%d ",av->ival) ; /* shouldn't happen, but you never know */

   RETURN(blab) ;
}

/*---------------------------------------------------------------
  Callback for the 'AFNI Tips' button [27 Jun 2011]
-----------------------------------------------------------------*/

#ifndef DONT_USE_HTMLWIN
static int tips_open        = 0 ;
static MCW_htmlwin *tips_hw = NULL ;
void AFNI_tips_killfun( XtPointer junk ){
  tips_open = 0 ; tips_hw = NULL ; return ;
}
#endif

void AFNI_tips_CB( Widget w , XtPointer cd , XtPointer cbd )
{
#include "readme_afnigui.h"
   Three_D_View *im3d = (Three_D_View *)cd ;
   char *inf=NULL , *fpt=NULL ; int ii ;

ENTRY("AFNI_tips_CB") ;

#ifndef DONT_USE_HTMLWIN
   if( tips_open && tips_hw != NULL ){
     XMapRaised( XtDisplay(tips_hw->wshell) , XtWindow(tips_hw->wshell) ) ;
     EXRETURN ;
   } else if( !AFNI_noenv("AFNI_DONT_USE_HTMLWIN") ){
     fpt = THD_find_regular_file("afnigui.html", NULL) ;
     if( fpt != NULL && *fpt != '\0' ){
       inf = (char *)malloc(sizeof(char)*(strlen(fpt)+16)) ;
       strcpy(inf,"file:") ; strcat(inf,fpt) ; free(fpt) ;
       tips_hw = new_MCW_htmlwin( im3d->vwid->imag->topper, inf,
                                  AFNI_tips_killfun , NULL  ,
                                  NULL, 0   ) ;
       free(inf) ; tips_open = 1 ; EXRETURN ;
     }
   }
#endif

   for( ii=0 ; readme_afnigui[ii] != NULL ; ii++ ){
     inf = THD_zzprintf( inf , " %s" , readme_afnigui[ii] ) ;
   }
   (void)new_MCW_textwin( im3d->vwid->imag->topper , inf , TEXT_READONLY ) ;
   free(inf) ; EXRETURN ;
}

/*---------------------------------------------------------------*/

#include "PvalueStuff.h"

void AFNI_pvalue_CB( Widget w , XtPointer cd , XtPointer cbd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   char *inf=NULL ; int ii ;

ENTRY("AFNI_pvalue_CB") ;
   if( !IM3D_OPEN(im3d) || w == NULL ) EXRETURN ;

   for( ii=0 ; PvalueStuff[ii] != NULL ; ii++ )
     inf = THD_zzprintf( inf , " %s" , PvalueStuff[ii] ) ;
   (void) new_MCW_textwin( im3d->vwid->imag->topper , inf , TEXT_READONLY ) ;
   free(inf) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------*/
/* Print list of AFNI papers [02 May 2014] */

void AFNI_list_papers( Widget w )
{
#include "afni_papers.h"
   int ii ;

   if( w != (Widget)NULL && XtIsWidget(w) ){  /* open a window */
     char *inf=NULL ;
     for( ii=0 ; afni_papers[ii] != NULL ; ii++ ){
       inf = THD_zzprintf( inf , " %s" , afni_papers[ii] ) ;
     }
#ifdef DONT_USE_HTMLWIN
     (void) new_MCW_textwin( w , inf , TEXT_READONLY ) ;  /* plain text */
#else
     { char *qqq = convert_text_to_html(inf) ;       /* HTML with links */
       (void)new_MCW_htmlwin( w , qqq , NULL,NULL,NULL,0 ) ;
       free(qqq) ;
     }
#endif
     free(inf) ;

   } else {   /* write papers list to stdout */

     for( ii=0 ; afni_papers[ii] != NULL ; ii++ )
       fputs(afni_papers[ii],stdout) ;

   }
   return ;
}

/*--------------------------------------------------------------------*/

void AFNI_papers_CB( Widget w , XtPointer cd , XtPointer cbd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

   if( IM3D_OPEN(im3d) )
     AFNI_list_papers( im3d->vwid->imag->topper ) ;

   return ;
}

/*---------------------------------------------------------------
  Callback for all actions in the misc menu
-----------------------------------------------------------------*/

void AFNI_misc_CB( Widget w , XtPointer cd , XtPointer cbd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)cbd ;
   int ic ;

ENTRY("AFNI_misc_CB") ;

   if( !IM3D_OPEN(im3d) || w == NULL ) EXRETURN ;

   ic = AFNI_controller_index(im3d) ;

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
      } else {
         BEEPIT ; WARNING_message("Can't get anat info") ;
      }
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
      } else {
         BEEPIT ; WARNING_message("Can't get func info") ;
      }
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_license_pb ){  /* 03 Dec 2000 */
#include "license.h"
      char *inf=NULL ; int ii ;
      for( ii=0 ; license[ii] != NULL ; ii++ )
         inf = THD_zzprintf( inf , " %s" , license[ii] ) ;
      (void) new_MCW_textwin( im3d->vwid->imag->topper , inf , TEXT_READONLY ) ;
      free(inf) ;
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_readme_env_pb ){  /* 05 Aug 2004 */
#include "readme_env.h"
      char *inf=NULL ; int ii ;
      for( ii=0 ; readme_env[ii] != NULL ; ii++ )
        inf = THD_zzprintf( inf , " %s" , readme_env[ii] ) ;
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
         BEEPIT ;
         (void)  MCW_popup_message( im3d->vwid->imag->topper ,
                                     " \n"
                                     "* Cannot execute *\n"
                                     "* afni_vcheck!   *\n" ,
                                    MCW_USER_KILL | MCW_TIMER_KILL ) ;
      } else {
#define ISIZE 1024
         char *info=(char *)malloc(sizeof(char)*ISIZE) ; int ninfo ;
         strcpy(info," \n     Output of Program afni_vcheck  \n"
                        "   ---------------------------------\n \n"   ) ;
         ninfo = strlen(info) ;
         while( afni_fgets(info+ninfo,ISIZE-ninfo,fp) != NULL ){
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
       INFO_message("Purge: before=%s  after=%s  diff=%s",
                    commaized_integer_string(mb) ,
                    commaized_integer_string(ma) ,
                    commaized_integer_string(mb-ma) ) ;
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
        if( plint == NULL ){ BEEPIT; WARNING_message("WTF?!"); EXRETURN; }
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
         if( plint == NULL ){ BEEPIT; WARNING_message("WTF?!"); EXRETURN; }
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
         if( plint == NULL ){ BEEPIT; WARNING_message("WTF?!"); EXRETURN; }
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

   else if( w == im3d->vwid->func->icor_pb ){ /* 29 Apr 2009 */
      static PLUGIN_interface *plint[MAX_CONTROLLERS] ; static int first=1 ;
      Widget wpop ;
      char title[64] , *lc=AFNI_controller_label(im3d) ;

      if( first ){  /* initialize */
        int ii ;
        for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ) plint[ii] = NULL ;
        first = 0 ;
      }

      /* first time in for this controller: create interface like a plugin */

      if( plint[ic] == NULL ){
         plint[ic] = ICOR_init(lc) ;
         if( plint[ic] == NULL ){ BEEPIT; WARNING_message("WTF?!"); EXRETURN; }
         PLUG_setup_widgets( plint[ic] , GLOBAL_library.dc ) ;
         plint[ic]->im3d = im3d ;
      }

      if( cbs == NULL ){  /* synthetic call */
         XtUnmapWidget(plint[ic]->wid->shell) ; EXRETURN ;
      }

      /* code below is from PLUG_startup_plugin_CB() in afni_plugin.c */

      plint[ic]->im3d = im3d ;
      sprintf(title,"%sAFNI InstaCorr Setup Operations",lc) ;
      XtVaSetValues( plint[ic]->wid->shell ,
                      XmNtitle     , title       , /* top of window */
                      XmNiconName  , "InstaCorr" , /* label on icon */
                     NULL ) ;
      PLUTO_cursorize( plint[ic]->wid->shell ) ;

      /*-- if possible, find where this popup should go --*/

      wpop = plint[ic]->wid->shell ;

      if( cbs->event != NULL && cbs->event->type == ButtonRelease ){

         XButtonEvent *xev = (XButtonEvent *)cbs->event ;
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

   else if( w == im3d->vwid->func->gicor_pb ){ /* 22 Dec 2009 */
      static PLUGIN_interface *plint=NULL ;
      Widget wpop ;
      char title[64] , *lc=AFNI_controller_label(im3d) ;

      if( cbs == NULL && plint != NULL ){  /* synthetic call */
         XtUnmapWidget(plint->wid->shell) ; EXRETURN ;
      }

      if( im3d->giset == NULL || !im3d->giset->ready ){
        if( cbs != NULL ){
          BEEPIT ;
          (void)  MCW_popup_message( im3d->vwid->func->gicor_pb ,
                                     " \n"
                                     " ***** AFNI: ***** \n"
                                     "   3dGroupInCorr   \n"
                                     " isn't connected!! \n " ,
                                      MCW_USER_KILL | MCW_TIMER_KILL ) ;
        }
        EXRETURN ;
      }

      /* first time in for this controller: create interface like a plugin */

      if( plint == NULL ){
         plint = GICOR_init(lc) ;
         if( plint == NULL ){ BEEPIT; WARNING_message("WTF?!"); EXRETURN; }
         PLUG_setup_widgets( plint , GLOBAL_library.dc ) ;
         plint->im3d = im3d ;
      }

      /* code below is from PLUG_startup_plugin_CB() in afni_plugin.c */

      plint->im3d = im3d ;
      sprintf(title,"%sAFNI Group InstaCorr Setup",lc) ;
      XtVaSetValues( plint->wid->shell ,
                      XmNtitle     , title       , /* top of window */
                      XmNiconName  , "GrpInCorr" , /* label on icon */
                     NULL ) ;
      PLUTO_cursorize( plint->wid->shell ) ;

      /*-- if possible, find where this popup should go --*/

      wpop = plint->wid->shell ;

      if( cbs->event != NULL && cbs->event->type == ButtonRelease ){

         XButtonEvent *xev = (XButtonEvent *)cbs->event ;
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

   else if( w == im3d->vwid->func->icalc_pb ){ /* 18 Sep 2009 */
     Widget wtop ;

     if( im3d->vwid->func->iwid == NULL ){
       ICALC_make_widgets(im3d) ;
       if( im3d->vwid->func->iwid == NULL ){ BEEPIT; WARNING_message("WTF?!"); EXRETURN; }
     }

     if( im3d->icalc_setup == NULL )
       INIT_ICALC_setup(im3d->icalc_setup) ;

     if( !im3d->vwid->func->iwid->is_open ) INSTACALC_LABEL_OFF(im3d) ;
     im3d->icalc_setup->is_good = 0 ;

     wtop =  im3d->vwid->func->iwid->wtop ;

     if( cbs->event != NULL && cbs->event->type == ButtonRelease ){
       XButtonEvent *xev = (XButtonEvent *)cbs->event ;
       int xx=(int)xev->x_root , yy=(int)xev->y_root , ww,hh,sw,sh ;
       MCW_widget_geom( wtop , &ww,&hh , NULL,NULL ) ;
       sw = WidthOfScreen (XtScreen(wtop)) ;
       sh = HeightOfScreen(XtScreen(wtop)) ;
       if( xx+ww+3 >= sw && ww <= sw ) xx = sw-ww ;
       if( yy+hh+3 >= sh && hh <= sh ) yy = sh-hh ;
       XtVaSetValues( wtop , XmNx , xx , XmNy , yy , NULL ) ;
     }

     XtMapWidget(wtop) ; RWC_visibilize_widget( wtop ) ;
     XRaiseWindow( XtDisplay(wtop) , XtWindow(wtop) ) ;
     im3d->vwid->func->iwid->is_open = 1 ;
   }

   /*.........................................................*/

   else if( w == im3d->vwid->func->tstat_pb ){ /* 22 Mar 2018 */
      static PLUGIN_interface *plint[MAX_CONTROLLERS] ; static int first=1 ;
      Widget wpop ;
      char title[64] , *lc=AFNI_controller_label(im3d) ;

      if( first ){  /* initialize */
        int ii ;
        for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ) plint[ii] = NULL ;
        first = 0 ;
      }

      /* first time in for this controller: create interface like a plugin */

      if( plint[ic] == NULL ){
         plint[ic] = TSTAT_init(lc) ;
         if( plint[ic] == NULL ){ BEEPIT; WARNING_message("WTF?!"); EXRETURN; }
         PLUG_setup_widgets( plint[ic] , GLOBAL_library.dc ) ;
         plint[ic]->im3d = im3d ;
      }

      if( cbs == NULL ){  /* synthetic call */
         XtUnmapWidget(plint[ic]->wid->shell) ; EXRETURN ;
      }

      /* code below is from PLUG_startup_plugin_CB() in afni_plugin.c */

      plint[ic]->im3d = im3d ;
      sprintf(title,"%sAFNI Tstat Computations",lc) ;
      XtVaSetValues( plint[ic]->wid->shell ,
                      XmNtitle     , title       , /* top of window */
                      XmNiconName  , "InstaCorr" , /* label on icon */
                     NULL ) ;
      PLUTO_cursorize( plint[ic]->wid->shell ) ;

      /*-- if possible, find where this popup should go --*/

      wpop = plint[ic]->wid->shell ;

      if( cbs->event != NULL && cbs->event->type == ButtonRelease ){

         XButtonEvent *xev = (XButtonEvent *)cbs->event ;
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
/* Find a sub-brick containing a given text in its label */

int find_subbrick_with_label( THD_3dim_dataset *dset , char *lstr )
{
   int iv , nvals ;

   if( !ISVALID_DSET(dset) || lstr == NULL || *lstr == '\0' ) return -1 ;

   nvals = DSET_NVALS(dset) ;
   for( iv=0 ; iv < nvals ; iv++ ){
     if( strcasestr( DSET_BRICK_LABEL(dset,iv) , lstr ) != NULL )
       return iv ;
   }
   return -1 ;
}

/*-------- get a pair of indexes for OLay/Thr [11 Jan 2017] --------*/

int_pair find_reasonable_overlay_indexes( THD_3dim_dataset *dset )
{
   int_pair ovp={-1,-1} ;
   int ith, iov ;

   if( !ISVALID_DSET(dset) ) return ovp ;

   iov = find_subbrick_with_label( dset , "Coef" ) ;
   if( iov < 0 )
     iov = find_subbrick_with_label( dset , "Beta" ) ;
   if( iov >= 0 ){
     ovp.i = iov ;
     if( iov+1 < DSET_NVALS(dset) ) ovp.j = iov+1 ;
     return ovp ;
   }

   iov = find_subbrick_with_label( dset , "Tstat" ) ;
   if( iov < 0 )
     iov = find_subbrick_with_label( dset , "Fstat" ) ;
   if( iov >= 0 ){
     ovp.i = ovp.j = iov ;
     return ovp ;
   }

   return ovp ;
}

/*---------------------------------------------------------------*/

void AFNI_editenv_CB( Widget w , XtPointer cd , XtPointer cbd )
{
   Three_D_View *im3d = (Three_D_View *) cd ;

   if( !IM3D_OPEN(im3d) ) return ;
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

#include "uscon.h"

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
         BEEPIT ; EXRETURN ;
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

   else if( w == im3d->vwid->prog->hidden_uscon_pb ){  /* 30 Dec 2010 */
     char *inf=NULL ; int ii ;
     for( ii=0 ; uscon[ii] != NULL ; ii++ )
       inf = THD_zzprintf( inf , " %s" , uscon[ii] ) ;
     (void) new_MCW_textwin( im3d->vwid->imag->topper , inf , TEXT_READONLY ) ;
     free(inf) ;
   }

   else if( w == im3d->vwid->prog->hidden_usdecl_pb ){  /* 06 Jan 2011 */
     char *inf=NULL ; int ii ;
     for( ii=0 ; usdecl[ii] != NULL ; ii++ )
       inf = THD_zzprintf( inf , " %s" , usdecl[ii] ) ;
     (void) new_MCW_textwin( im3d->vwid->imag->topper , inf , TEXT_READONLY ) ;
     free(inf) ;
   }

   else if( w == im3d->vwid->prog->hidden_melter_pb ){   /* 18 Feb 2011 */
     MCW_melt_widget( im3d->vwid->top_form ) ;
     NI_sleep(333) ;
     MCW_melt_widget( im3d->vwid->top_form ) ;
     NI_sleep(333) ;
     if( GLOBAL_library.have_sox && GLOBAL_library.local_display )
       AFNI_startup_sound() ;
     SENSITIZE(w,0) ;
   }

   else if( w == im3d->vwid->prog->hidden_sound_pb ){    /* 20 Aug 2018 */
     if( GLOBAL_library.have_sox && GLOBAL_library.local_display )
       AFNI_startup_sound() ;
     else
       WARNING_message("sound playing not available :(") ;
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

   else if( w == im3d->vwid->prog->hidden_hbmjust_pb ){
     (void)MCW_popup_message( im3d->vwid->imag->topper ,
             "  Why I Must Attend HBM - RWCox\n"
             "---------------------------------\n"
             "I have been asked to state\n"
             "why HBM is great,\n"
             "and why it is a must\n"
             "that I incur this cost.\n"
             "Brain Mappers all converge\n"
             "to show their best new work,\n"
             "exactly in my field:\n"
             "ways to make data yield\n"
             "news about brain function,\n"
             "providing a conjunction\n"
             "of doers and of thinkers --\n"
             "ideal for science drinkers.\n"
             "This meet is Number One:\n"
             "the best come here to run\n"
             "their latest, their greatest\n"
             "past the world's most smartest.\n"
             "There is where I can see\n"
             "what's new and important to me\n"
             "in my work for the NIMH\n"
             "and to show them my batch\n"
             "of newly warped brains\n"
             "and how this entrains\n"
             "a new way to make\n"
             "FMRI a piece of (nonlinear) cake.\n" , MCW_USER_KILL ) ;
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
     whereami_browser("https://afni.nimh.nih.gov/afni/doc/program_help/index.html");
#if 0
     char cmd[2345] ;
     sprintf(cmd ,
             "%s https://afni.nimh.nih.gov/afni/doc/program_help/index.html &" ,
             GLOBAL_browser ) ;
     system(cmd) ;
#endif
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
       BEEPIT ;
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
   char edir[THD_MAX_NAME] , fdir[THD_MAX_NAME] , *udir , **ename ;
   int epos , ll , ii , id , npoem , nep ;
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

      strcpy(fdir,edir) ; strcat(fdir,"funstuff") ;  /* 07 Oct 2011 */
      if( THD_is_directory(fdir) ){ strcat(fdir,"/"); udir = fdir; } else { udir = edir; }

      strcpy(ename[0],udir) ;
      strcat(ename[0],"poem_*.txt") ;        /* add filename pattern */
      nep = 1 ;

      MCW_file_expand( nep,ename, &npoem , &fpoem );   /* find files that match */
      if( npoem <= 0 ) continue ;                     /* no files found */

      /** add files we found to list **/

      if( fname_poem == NULL )
        fname_poem = (char **)malloc(sizeof(char *)*npoem) ;
      else
        fname_poem = (char **)realloc(fname_poem,sizeof(char *)*(num_poem+npoem));

      for( ii=0 ; ii < npoem ; ii++ ){
        fname_poem[num_poem++] = strdup(fpoem[ii]) ;

        if( udir == fdir ){          /* 07 Oct 2011 */
          char qnam[THD_MAX_NAME] ;
          strcpy(qnam,edir) ; strcat(qnam,THD_trailname(fpoem[ii],0)) ;
          if( THD_is_file(qnam) ) remove(qnam) ;
        }
      }

      MCW_free_expand( npoem , fpoem ) ;

   } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */

   free(elocal) ; free(ename[0]) ; free(ename[1]) ; free(ename) ;

   if( num_poem == 0 ) num_poem = -1 ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* 18 Feb 2014 */

#ifdef USE_SKIT
static void AFNI_alter_controller_bg( Three_D_View *im3d , float fac )
{
   if( !IM3D_OPEN(im3d) || fac < 0.0f || fac > 2.0f ) return ;
   MCW_scale_widget_bg( im3d->vwid->top_form, fac, im3d->dc ) ;
   return ;
}
#endif

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
         else if( !NO_frivolities && event->button == Button4 ) AFNI_alter_controller_bg(im3d,0.960000f) ;
         else if( !NO_frivolities && event->button == Button5 ) AFNI_alter_controller_bg(im3d,1.041667f) ;
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
            BEEPIT ; EXRETURN ;
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
               DESTROY_VLIST(sv) ;
               BEEPIT ; EXRETURN ;
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
            BEEPIT ; WARNING_message("Can't write -- no points!") ; EXRETURN ;
         }

         if( cbs->cval[0] == '|' ){  /* send to standard output */
            fil = stdout ;
         } else {
            if( THD_is_file(cbs->cval) ){
               char buf[256] ;
               sprintf(buf,"Desired output file\n %s\nalready exists!",cbs->cval);
               (void)  MCW_popup_message( im3d->vwid->picture , buf ,
                                          MCW_USER_KILL | MCW_TIMER_KILL ) ;
               BEEPIT ; EXRETURN ;
            }

            fil = fopen( cbs->cval , "w" ) ;
            if( fil == NULL ){
               char buf[256] ;
               sprintf(buf,"Cannot open file\n %s\nfor writing!",cbs->cval) ;
               (void)  MCW_popup_message( im3d->vwid->picture , buf ,
                                          MCW_USER_KILL | MCW_TIMER_KILL ) ;
               BEEPIT ; EXRETURN ;
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

   if( !IM3D_OPEN(im3d) ) return ;

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
