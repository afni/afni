#undef MAIN

#include "afni.h"

#ifdef AFNI_DEBUG
#  define USE_TRACING
#endif
#include "dbtrace.h"

/*-------------------------------------------------------------------
   This routine is also used by the macros
      AFNI_SEE_FUNC_ON and AFNI_SEE_FUNC_OFF
   as well as the MCW_bbox that it is attached to as the callback.
---------------------------------------------------------------------*/

void AFNI_see_func_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View * im3d = (Three_D_View *) cd ;
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
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;  /* redraw */
   }

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*--------------------------------------------------------------
  Called when switching from FIM to THR function type
----------------------------------------------------------------*/

void AFNI_functype_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   int function_type , redisplay ;
   Boolean allow_autorange , allow_avrange , same ;
   XmString xstr ;

ENTRY("AFNI_functype_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   function_type = AFNI_first_tog( LAST_FUNC_TYPE + 1 ,
                                   im3d->vwid->func->functype_bbox->wbut ) ;

   if( im3d->vinfo->showfunc_type != function_type ){

      SHOW_AFNI_PAUSE ;

      im3d->vinfo->showfunc_type = function_type ;  /* FIM or THR */

      if( ISFUNC_UNDERLAY(im3d->vinfo->underlay_type) ){
         AFNI_underlay_CB( NULL , (XtPointer) im3d , NULL ) ;
         redisplay = REDISPLAY_ALL ;
      } else {
         redisplay = (im3d->vinfo->func_visible) ? REDISPLAY_OVERLAY
                                                 : REDISPLAY_OPTIONAL ;
      }

      /* 12 Aug 1996 */

      xstr = AFNI_autorange_label( im3d ) ;
      same = XmStringCompare( xstr , im3d->vinfo->autorange_label ) ;

      if( same == False ){
         Widget www = im3d->vwid->func->range_bbox->wbut[RANGE_AUTOBUT] ;
         XtVaSetValues( www , XmNlabelString , xstr , NULL ) ;
         XmStringFree(im3d->vinfo->autorange_label) ;
         im3d->vinfo->autorange_label = xstr ;
      } else {
         XmStringFree( xstr ) ;  /* was same --> don't need this copy */
      }

      im3d->vinfo->fim_range =
         (im3d->vinfo->use_autorange) ? (im3d->vinfo->fim_autorange)
                                      : (im3d->vwid->func->range_av->fval) ;

      AFNI_set_viewpoint( im3d , -1,-1,-1 , redisplay ) ; /* redraw */

      /** 3/24/95: turn range controls on or off **/

      allow_autorange = True ;
      allow_avrange   = (allow_autorange && ! im3d->vinfo->use_autorange) ;

      SENSITIZE( im3d->vwid->func->range_bbox->wrowcol , allow_autorange ) ;
      AV_SENSITIZE( im3d->vwid->func->range_av , allow_avrange ) ;

      /** done **/

      SHOW_AFNI_READY ;
   }

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
   Three_D_View * im3d = (Three_D_View *) client_data ;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call_data ;
   float fff ;
   int redisplay ;

ENTRY("AFNI_thr_scale_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   fff = THR_FACTOR * cbs->value ;  /* 30 Nov 1997 */
   if( fff >= 0.0 && fff <= 1.0 ) im3d->vinfo->func_threshold = fff ;

   FIX_SCALE_VALUE(im3d) ;

   switch( im3d->vinfo->underlay_type ){
      default:
      case UNDERLAY_ANAT:
      case UNDERLAY_ALLFUNC:
         redisplay = (im3d->vinfo->func_visible) ? REDISPLAY_OVERLAY
                                                 : REDISPLAY_OPTIONAL ;
      break ;

      case UNDERLAY_THRFUNC:
         redisplay = REDISPLAY_ALL ;
         AFNI_imseq_clearstat( im3d ) ;
      break ;
   }

   AFNI_set_thr_pval( im3d ) ;

   if( ! DOING_REALTIME_WORK )
      AFNI_set_viewpoint( im3d , -1,-1,-1 , redisplay ) ;  /* redraw */

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------
  Called when the user drags the threshold scale at all, even
  without releasing it.  Used to update the "p-value" interactively.
----------------------------------------------------------------------*/

void AFNI_thr_scale_drag_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   Three_D_View * im3d = (Three_D_View *) client_data ;
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) call_data ;
   float fff ;

ENTRY("AFNI_thr_scale_drag CB") ;

   if( IM3D_VALID(im3d)                        &&
       ISVALID_3DIM_DATASET(im3d->fim_now)     &&
       FUNC_HAVE_PVAL(im3d->fim_now->func_type)   ){

      fff = THR_FACTOR * cbs->value ;
      if( fff >= 0.0 && fff <= 1.0 ) im3d->vinfo->func_threshold = fff ;

      FIX_SCALE_VALUE(im3d) ;
      AFNI_set_thr_pval( im3d ) ;
   }
   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Set the top value used on the threshold slider.
  (Should be followed by a function redisplay if needed.)
-------------------------------------------------------------------------*/

void AFNI_set_thresh_top( Three_D_View * im3d , float tval )
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

char * AFNI_thresh_tlabel_CB( MCW_arrowval * av , XtPointer junk )
{
   static char tlab[8] ;
   sprintf(tlab,"%d",av->ival) ;
   return tlab ;
}

/*-----------------------------------------------------------------------
  Take action when the user changes the threshold top chooser.
-------------------------------------------------------------------------*/

void AFNI_thresh_top_CB( MCW_arrowval * av , XtPointer cd )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   static float tval[9] = { 1.0 , 10.0 , 100.0 , 1000.0 , 10000.0 ,
                            100000.0 , 1000000.0 , 10000000.0 , 100000000.0 } ;

ENTRY("AFNI_thresh_top_CB") ;

   if( IM3D_OPEN(im3d) && im3d->vinfo->func_thresh_top != tval[av->ival] ){

      AFNI_set_thresh_top( im3d , tval[av->ival] ) ;

      if( im3d->vinfo->func_visible )
         AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;  /* redraw */
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Used to set the pval (significance) label at the bottom of the
  threshold scale.
-------------------------------------------------------------------------*/

void AFNI_set_thr_pval( Three_D_View * im3d )
{
   float thresh , pval ;
   int   dec ;
   char  buf[16] ;

ENTRY("AFNI_set_thr_pval") ;

   if( ! IM3D_VALID(im3d) || ! ISVALID_3DIM_DATASET(im3d->fim_now) ) EXRETURN ;

   /* get the "true" threshold (scaled up from being in [0,1]) */

   thresh = im3d->vinfo->func_threshold * im3d->vinfo->func_thresh_top ;

   /* get the p-value that goes with this threshold, for this functional dataset */

   if( ISFUNCBUCKET(im3d->fim_now) ) /* 30 Nov 1997 */
      pval = THD_stat_to_pval( thresh ,
                 DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) ,
                 DSET_BRICK_STATAUX (im3d->fim_now,im3d->vinfo->thr_index)  ) ;
   else
      pval = THD_stat_to_pval( thresh , im3d->fim_now->func_type ,
                                        im3d->fim_now->stat_aux   ) ;

if(PRINT_TRACING)
{ char buf[128] ;
  sprintf( buf, "thresh=%g  top=%g  pval=%g",
           thresh,im3d->vinfo->func_thresh_top,pval ) ; STATUS(buf) ; }

   if( pval < 0.0 ){
      strcpy( buf , THR_PVAL_LABEL_NONE ) ;
   } else {
      if( pval == 0.0 ){
         strcpy( buf , "p = 0" ) ;
      } else if( pval >= 0.9999 ){
         strcpy( buf , "p = 1" ) ;
      } else if( pval >= 0.0010 ){
         char qbuf[16] ;
         sprintf( qbuf , "%5.4f" , pval ) ;
         strcpy( buf , qbuf+1 ) ;
      } else {
         int dec = (int)(0.999 - log10(pval)) ;
         pval = pval * pow( 10.0 , (double) dec ) ;  /* between 1 and 10 */
         if( dec < 10 ) sprintf( buf , "%3.1f-%1d" ,      pval, dec ) ;
         else           sprintf( buf , "%1d.-%2d"  , (int)pval, dec ) ;
      }
   }
   MCW_set_widget_label( im3d->vwid->func->thr_pval_label , buf ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------
  called when the pbar for the intensity mapping is adjusted
  (thresholds or colors)
------------------------------------------------------------------------------*/

void AFNI_inten_pbar_CB( MCW_pbar * pbar , XtPointer cd , int reason )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_inten_pbar_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   if( im3d->vinfo->func_visible )
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;  /* redraw */

   FIX_SCALE_SIZE(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------
  called to set up the pbar for the intensity mapping
-----------------------------------------------------------------------------*/

/** 3/27/95: changed to allow different initializations
             for the [0,1] and the [-1,1] range pbars.  **/

/** 6/01/95: changed to put the initialization constants
             in tables initialized in afni.c, not here. **/

void AFNI_setup_inten_pbar( Three_D_View * im3d )
{
  MCW_pbar * pbar ;
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

void AFNI_inten_av_CB( MCW_arrowval * av , XtPointer cd )
{
   MCW_pbar * pbar = (MCW_pbar *) cd ;
   Three_D_View * im3d = (Three_D_View *) pbar->parent ;

   HIDE_SCALE(im3d) ;
   alter_MCW_pbar( pbar , av->ival , NULL ) ;
   FIX_SCALE_SIZE(im3d) ;
}

/*---------------------------------------------------------------------
   Make a warped dataset whose grid corresponds to the anat_parent and
   whose data comes from the data_parent.
   Note that the assumption is made that data_parent and the warp parent
   of the anat_parent are both in the same coordinate system (up to the
   to_dicomm transformation of their dataxes structs).
-----------------------------------------------------------------------*/

THD_3dim_dataset * AFNI_follower_dataset( THD_3dim_dataset * anat_parent ,
                                          THD_3dim_dataset * data_parent  )
{
   THD_3dim_dataset * new_dset ;
   int ii ;

ENTRY("AFNI_follower_dataset") ;

   /* sanity checks */

   if( ! ISVALID_3DIM_DATASET(anat_parent) ||
       ! ISVALID_3DIM_DATASET(data_parent)   ) RETURN(NULL) ;

   /* can't warp a time-dependent dataset */

   if( DSET_NUM_TIMES(data_parent) > 1 && ! GLOBAL_argopt.warp_4D ) RETURN(NULL) ;

   /* make new dataset, copying appropriate fields from its various parents */

   new_dset = myXtNew( THD_3dim_dataset ) ; INIT_KILL( new_dset->kl ) ;

   new_dset->type      = data_parent->type ;        /* same data type */
   new_dset->func_type = data_parent->func_type ;
   new_dset->view_type = anat_parent->view_type ;   /* but different view type */

   new_dset->anat_parent = anat_parent ;            /* what else makes sense? */

   new_dset->tagset = NULL ;  /* Oct 1998 */

   MCW_strncpy( new_dset->anat_parent_name ,
                anat_parent->self_name , THD_MAX_NAME ) ;

#ifndef OMIT_DATASET_IDCODES
   new_dset->anat_parent_idcode = anat_parent->idcode ;
#endif

   /* 11/09/94 addition: the data_parent may itself be a warp;
       in this case, we want the true warp parent to be the original data */

   new_dset->warp_parent =  (data_parent->warp_parent != NULL)
                          ? (data_parent->warp_parent) : (data_parent) ;

   MCW_strncpy( new_dset->warp_parent_name ,
                new_dset->warp_parent->self_name , THD_MAX_NAME ) ;

#ifndef OMIT_DATASET_IDCODES
   new_dset->warp_parent_idcode = new_dset->warp_parent->idcode ;
#endif

#ifndef OMIT_DATASET_IDCODES
   new_dset->idcode = MCW_new_idcode() ;
#endif

   /* make the actual warp from the warp_parent to this dataset */

   new_dset->vox_warp       = NULL ;
   new_dset->warp           = myXtNew( THD_warp ) ;
   *(new_dset->warp)        = IDENTITY_WARP ;  /* start with (Dicom) identity */

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

   new_dset->daxes         = myXtNew( THD_dataxes ) ;  /* copy data axes of */
   *(new_dset->daxes)      = *(anat_parent->daxes) ; /* anatomy parent */

   new_dset->wod_daxes     = NULL ;
   new_dset->wod_flag      = True ;

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

   new_dset->merger_list = NULL ;  /* not a merger */
   new_dset->markers     = NULL ;  /* no markers */
   new_dset->death_mark  = 0 ;     /* don't kill me! */
   new_dset->pts         = NULL ;

   PARENTIZE(new_dset,data_parent->parent) ;

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

void AFNI_make_descendants_old( THD_sessionlist * , int ) ; /* proto */

void AFNI_make_descendants( THD_sessionlist * ssl )
{
   AFNI_make_descendants_old( ssl , VIEW_ORIGINAL_TYPE ) ;
#if 0
   AFNI_make_descendants_old( ssl , VIEW_ACPCALIGNED_TYPE ) ;
#endif
   return ;
}

/** In this routine, each occurence of vbase was originally VIEW_ORIGINAL_TYPE **/

void AFNI_make_descendants_old( THD_sessionlist * ssl , int vbase )
{
   int iss , jdd , kvv , num_made=0 ;
   THD_session * ss ;
   THD_3dim_dataset * orig_dset , * new_dset ;
   THD_slist_find     find ;
   THD_3dim_dataset ** anat_parent_row , ** orig_row ;

ENTRY("AFNI_make_descendants_old") ;

   if( ! ISVALID_SESSIONLIST(ssl) ) EXRETURN ;

   /* loop over each session */

   for( iss=0 ; iss < ssl->num_sess ; iss++ ){
      ss = ssl->ssar[iss] ;
      if( !ISVALID_SESSION(ss) ) continue ;  /* no good ==> skip */

      /* loop over anats in this session */

      for( jdd=0 ; jdd < ss->num_anat ; jdd++ ){
         orig_dset = ss->anat[jdd][vbase] ;                   /* 3 Jun 98 */
         if( !ISVALID_3DIM_DATASET(orig_dset) ||              /* no good */
             orig_dset->anat_parent == NULL   ||              /* no parent */
             orig_dset->anat_parent == orig_dset ) continue ; /* ==> skip */

         /* look for orig_dset's anat parent in this sessionlist */

#ifdef OMIT_DATASET_IDCODES
         find = THD_dset_in_sessionlist( FIND_NAME ,
                                         orig_dset->anat_parent->self_name ,
                                         ssl , iss ) ;
#else
         find = THD_dset_in_sessionlist( FIND_IDCODE ,
                                         &(orig_dset->anat_parent->idcode) ,
                                         ssl , iss ) ;
         if( find.dset == NULL )
            find = THD_dset_in_sessionlist( FIND_NAME ,
                                            orig_dset->anat_parent->self_name ,
                                            ssl , iss ) ;
#endif

         /* check for a good find; if it doesn't happen,
            then skip (this eventuality should never occur!) */

         if( find.dset       == NULL ||
             find.func_index >= 0    ||  /* anat parent can't be a func! */
             find.view_index != vbase  ) continue ;

         /* make a pointer to the row of datasets of the anat
            parent (this is like the SPGR row in the picture above) */

         anat_parent_row =
           &(ssl->ssar[find.sess_index]->anat[find.anat_index][0]) ;

         /* pointer to row of datasets being operated on now
            (like the FIM row in the picture above)          */

         orig_row = &(ss->anat[jdd][0]) ;

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
      }  /* end of loop over anats */

      /* do the same for funcs */

      for( jdd=0 ; jdd < ss->num_func ; jdd++ ){
         orig_dset = ss->func[jdd][vbase] ;
         if( !ISVALID_3DIM_DATASET(orig_dset) ||              /* no good */
             orig_dset->anat_parent == NULL   ||              /* no parent */
             orig_dset->anat_parent == orig_dset ) continue ; /* ==> skip */

         /* look for orig_dset's anat parent in this sessionlist */

#ifdef OMIT_DATASET_IDCODES
         find = THD_dset_in_sessionlist( FIND_NAME ,
                                         orig_dset->anat_parent->self_name ,
                                         ssl , iss ) ;
#else
         find = THD_dset_in_sessionlist( FIND_IDCODE ,
                                         &(orig_dset->anat_parent->idcode) ,
                                         ssl , iss ) ;
         if( find.dset == NULL )
            find = THD_dset_in_sessionlist( FIND_NAME ,
                                            orig_dset->anat_parent->self_name ,
                                            ssl , iss ) ;
#endif

         /* check for a good find; if it doesn't happen,
            then skip (this eventuality should never occur!) */

         if( find.dset       == NULL ||
             find.func_index >= 0    ||  /* no funcs for anat parents! */
             find.view_index != vbase  ) continue ;

         /* make a pointer to the row of datasets of the anat
            parent (this is like the SPGR row in the picture above) */

         anat_parent_row =
           &(ssl->ssar[find.sess_index]->anat[find.anat_index][0]) ;

         orig_row = &(ss->func[jdd][0]) ;

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
      }  /* end of loop over funcs */

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
-------------------------------------------------------------------------*/

void AFNI_force_adoption( THD_session * ss , Boolean do_anats )
{
   int aa , ff , vv , apref=0 , aset=-1 ;
   THD_3dim_dataset * dset ;

ENTRY("AFNI_force_adoption") ;

   if( ! ISVALID_SESSION(ss) || ss->num_anat == 0 ) EXRETURN ;

   /* find a "preferred" parent (one with the most number of markers set) */

   for( aa=0 ; aa < ss->num_anat ; aa++ ){
      dset = ss->anat[aa][0] ;              /* original view */

      if( ISVALID_3DIM_DATASET(dset) &&     /* if a good dataset */
          dset->markers != NULL      &&     /* and has markers   */
          dset->markers->numset > aset ){   /* and has more markers than before */

         apref = aa ;                       /* try this as our "preferred" parent */
         aset  = dset->markers->numset ;
      }
   }

   /* scan through all functions, all views */

   for( ff=0 ; ff < ss->num_func ; ff++ ){
      for( vv=VIEW_ORIGINAL_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ){

         dset = ss->func[ff][vv] ;  /* function that needs parent */

         if( ! ISVALID_3DIM_DATASET(dset) ||
             dset->anat_parent != NULL      ) continue ; /* nothing to do */

         if( ISVALID_3DIM_DATASET(ss->anat[apref][vv]) ){  /* if preferred is OK, */
            dset->anat_parent = ss->anat[apref][vv] ;      /* use it here         */
         } else {
            for( aa=0 ; aa < ss->num_anat ; aa++ ){           /* search for something, */
               if( ISVALID_3DIM_DATASET(ss->anat[aa][vv]) ){  /* anything, and use it  */
                  dset->anat_parent = ss->anat[aa][vv] ; break ;
               }
            }
         }
      }
   }

   /* 06 Aug 1996: do anats, maybe */

   if( do_anats ){
      for( ff=0 ; ff < ss->num_anat ; ff++ ){
         for( vv=VIEW_ORIGINAL_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ ){

            dset = ss->anat[ff][vv] ;  /* anatomy that needs parent */

            if( ! ISVALID_3DIM_DATASET(dset) ||
                dset->anat_parent != NULL      ) continue ; /* nothing to do */

            if( ISVALID_3DIM_DATASET(ss->anat[apref][vv]) ){  /* if preferred is OK, */
               dset->anat_parent = ss->anat[apref][vv] ;      /* use it here         */
            } else {
               for( aa=0 ; aa < ss->num_anat ; aa++ ){           /* search for something, */
                  if( ISVALID_3DIM_DATASET(ss->anat[aa][vv]) &&  /* anything, and use it  */
                      ss->anat[aa][vv] != dset                 ){
                     dset->anat_parent = ss->anat[aa][vv] ; break ;
                  }
               }
            }
         }
      }
   }

   EXRETURN ;
}

/*-----------------------------------------------------------------------
  Make a functional overlay -- very simple routine at present;
   n = slice index , br_fim = data structure to extract slice from.
  Note that imseq requires that the overlay be an image of shorts.
  This routine requires that the function and threshold images be bytes,
  shorts, or floats (complex-valued functional images are not supported).
  (The underlay image may be any legal type; this image is not used here.)
-------------------------------------------------------------------------*/

MRI_IMAGE * AFNI_func_overlay( int n , FD_brick * br_fim )
{
   Three_D_View * im3d ;
   MRI_IMAGE * im_thr , * im_fim , * im_ov ;
   short * ar_ov ;
   short fim_ovc[NPANE_MAX+1] ;
   int npix , ii , lp , num_lp , function_type , fdset_type , ival ;
   float scale_factor ;
   MCW_pbar * pbar ;
   Boolean have_thr ;
   int     simult_thr , need_thr ;

ENTRY("AFNI_func_overlay") ;

   /* sanity check */

   if( br_fim == NULL || n < 0 ) RETURN(NULL) ;  /* nothing to do */

   im3d = (Three_D_View *) br_fim->parent ;

   function_type = im3d->vinfo->showfunc_type ;  /* what to show? */
   fdset_type    = br_fim->dset->func_type ;
   have_thr      = FUNC_HAVE_THR( fdset_type ) ;

   if( ISFUNCBUCKET(br_fim->dset) )         /* 30 Nov 1997 */
      ival = im3d->vinfo->thr_index ;
   else
      ival = FUNC_ival_thr[fdset_type] ;    /* sub-brick for threshold */

   if( function_type == SHOWFUNC_THR && ! have_thr ){ /* wrong kind! */
      XBell( im3d->dc->display , 100 ) ; RETURN(NULL) ;
   }

   /* get the component images */

   LOAD_DSET_VIEWS(im3d) ; /* 02 Nov 1996 */

   need_thr = have_thr && ( function_type == SHOWFUNC_THR ||      /* 10 Dec 1997 */
                            im3d->vinfo->func_threshold > 0.0 ) ;

   if( need_thr ) im_thr = FD_warp_to_mri( n , ival , br_fim ) ;
   else           im_thr = NULL ;

   have_thr = (im_thr != NULL) ;

#ifdef ALLOW_BKGD_LAB
   AFNI_set_valabel( br_fim , n , im_thr , im3d->vinfo->thr_val ) ;
#endif

   if( function_type == SHOWFUNC_FIM ){
      int ind ;

      if( fdset_type == FUNC_FIM_TYPE ){   /* Mar 1997: allow for 3D+t FIM */
         ind = im3d->vinfo->time_index ;
         if( ind >= DSET_NUM_TIMES(br_fim->dset) )
            ind = DSET_NUM_TIMES(br_fim->dset) - 1 ;
      } else {
         if( ISFUNCBUCKET(br_fim->dset) )         /* 30 Nov 1997 */
            ind = im3d->vinfo->fim_index ;
         else
            ind = FUNC_ival_fim[fdset_type] ;
      }
      im_fim       = FD_warp_to_mri( n, ind, br_fim ) ;
      scale_factor = im3d->vinfo->fim_range ;
      if( scale_factor == 0.0 ) scale_factor = im3d->vinfo->fim_autorange ;

#ifdef ALLOW_BKGD_LAB
   AFNI_set_valabel( br_fim , n , im_fim , im3d->vinfo->func_val ) ;
#endif
   } else {
      im_fim = im_thr ;
#if 0
      if( im_fim->kind == MRI_short )
         scale_factor = FUNC_scale_short[fdset_type] * FUNC_topval[fdset_type] ;
      else if( im_fim->kind == MRI_byte )
         scale_factor = FUNC_scale_byte[fdset_type] * FUNC_topval[fdset_type] ;
      else
         scale_factor = FUNC_topval[fdset_type] ;
#else
      scale_factor = im3d->vinfo->fim_range ;
      if( scale_factor == 0.0 ) scale_factor = im3d->vinfo->fim_autorange ;
#endif

#ifdef ALLOW_BKGD_LAB
   im3d->vinfo->func_val[0] = '\0' ;
#endif
   }

   /* if component images not good, quit now */

   if( im_fim == NULL ){
STATUS("couldn't get Func image!") ;
      KILL_1MRI(im_thr) ; RETURN(NULL) ;
   }

   if( ! AFNI_GOOD_FUNC_DTYPE(im_fim->kind) ||
       ( im_thr != NULL && ! AFNI_GOOD_FUNC_DTYPE(im_thr->kind) ) ){

STATUS("bad function datatypes!!") ;
      KILL_1MRI(im_fim) ; KILL_1MRI(im_thr) ; RETURN(NULL) ;
   }

   /* create output image */

   npix  = im_fim->nx * im_fim->ny ;
   im_ov = mri_new( im_fim->nx , im_fim->ny , MRI_short ) ;
   ar_ov = MRI_SHORT_PTR( im_ov ) ;

   /* set overlay colors */

   pbar   = im3d->vwid->func->inten_pbar ;
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
         short * ar_fim = MRI_SHORT_PTR(im_fim) ;
         float fim_thr[NPANE_MAX] ;  /* 13 Nov 1996: changed from short */

         for( lp=0 ; lp < num_lp ; lp++ )
            fim_thr[lp] = scale_factor * pbar->pval[lp+1] ;

         if( simult_thr ){
            short thresh = im3d->vinfo->func_threshold
                         * im3d->vinfo->func_thresh_top * FUNC_scale_short[fdset_type] ;
            short * ar_thr = MRI_SHORT_PTR(im_thr) ;

            for( ii=0 ; ii < npix ; ii++ ){
               if( (ar_thr[ii] > -thresh && ar_thr[ii] < thresh) || ar_fim[ii] == 0 ){
                  ar_ov[ii] = 0 ;
               } else {
                  for( lp=0 ; lp < num_lp && ar_fim[ii] < fim_thr[lp] ; lp++ ) ; /*nada*/
                  ar_ov[ii] = fim_ovc[lp] ;
               }
            }
         } else {
            for( ii=0 ; ii < npix ; ii++ ){
               if( ar_fim[ii] == 0 ){
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
         byte * ar_fim = MRI_BYTE_PTR(im_fim) ;
         float fim_thr[NPANE_MAX] ;  /* 13 Nov 1996: changed from short */

         for( lp=0 ; lp < num_lp ; lp++ )
            if( pbar->pval[lp+1] <= 0.0 )
               fim_thr[lp] = 0 ;
            else
               fim_thr[lp] = scale_factor * pbar->pval[lp+1] ;

         if( simult_thr ){
            byte thresh = im3d->vinfo->func_threshold
                         * im3d->vinfo->func_thresh_top * FUNC_scale_byte[fdset_type] ;
            byte * ar_thr = MRI_BYTE_PTR(im_thr) ;

            for( ii=0 ; ii < npix ; ii++ ){
               if( ar_thr[ii] < thresh || ar_fim[ii] == 0 ){
                  ar_ov[ii] = 0 ;
               } else {
                  for( lp=0 ; lp < num_lp && ar_fim[ii] < fim_thr[lp] ; lp++ ) ; /*nada*/
                  ar_ov[ii] = fim_ovc[lp] ;
               }
            }
         } else {
            for( ii=0 ; ii < npix ; ii++ ){
               if( ar_fim[ii] == 0 ){
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
         float * ar_fim = MRI_FLOAT_PTR(im_fim) ;
         float fim_thr[NPANE_MAX] ;

         for( lp=0 ; lp < num_lp ; lp++ )
            fim_thr[lp] = scale_factor * pbar->pval[lp+1] ;

         if( simult_thr ){
            float thresh = im3d->vinfo->func_threshold * im3d->vinfo->func_thresh_top ;
            float * ar_thr = MRI_FLOAT_PTR(im_thr) ;

            for( ii=0 ; ii < npix ; ii++ ){
               if( (ar_thr[ii] > -thresh && ar_thr[ii] < thresh) || ar_fim[ii] == 0.0  ){
                  ar_ov[ii] = 0 ;
               } else {
                  for( lp=0 ; lp < num_lp && ar_fim[ii] < fim_thr[lp] ; lp++ ) ; /*nada*/
                  ar_ov[ii] = fim_ovc[lp] ;
               }
            }
         } else {
            for( ii=0 ; ii < npix ; ii++ ){
               if( ar_fim[ii] == 0.0 ){
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

   /** if have threshold, clip the overlay **/

   if( im_thr != NULL && im3d->vinfo->func_threshold > 0.0 && !simult_thr ){

      switch( im_thr->kind ){

         case MRI_short:{
            short thresh = im3d->vinfo->func_threshold
                         * im3d->vinfo->func_thresh_top * FUNC_scale_short[fdset_type] ;
            short * ar_thr = MRI_SHORT_PTR(im_thr) ;

            for( ii=0 ; ii < npix ; ii++ )
               if( ar_thr[ii] > -thresh && ar_thr[ii] < thresh ) ar_ov[ii] = 0 ;
         }
         break ;

         case MRI_byte:{
            byte thresh = im3d->vinfo->func_threshold
                        * im3d->vinfo->func_thresh_top * FUNC_scale_byte[fdset_type] ;
            byte * ar_thr = MRI_BYTE_PTR(im_thr) ;

            for( ii=0 ; ii < npix ; ii++ )
               if( ar_thr[ii] < thresh ) ar_ov[ii] = 0 ;
         }
         break ;

         case MRI_float:{
            float thresh = im3d->vinfo->func_threshold * im3d->vinfo->func_thresh_top ;
            float * ar_thr = MRI_FLOAT_PTR(im_thr) ;

            for( ii=0 ; ii < npix ; ii++ )
               if( ar_thr[ii] > -thresh && ar_thr[ii] < thresh ) ar_ov[ii] = 0 ;
         }
         break ;
      }
   }

   if( im_thr != NULL && im_thr != im_fim ) mri_free( im_thr ) ;
   mri_free( im_fim ) ;

   for( ii=0 ; ii < npix ; ii++ ) if( ar_ov[ii] != 0 ) break ;

   if( ii == npix ) KILL_1MRI(im_ov) ;  /* no nonzero values --> no overlay */

   RETURN(im_ov) ;
}

/*---------------------------------------------------------------------*/

char * AFNI_resam_texter( MCW_arrowval * av , XtPointer junk )
{
   return RESAM_shortstr[av->ival] ;
}

/*---------------------------------------------------------------------*/

void AFNI_resam_av_CB( MCW_arrowval * av , XtPointer cd )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   int reunder , redisplay , isfunc ;

ENTRY("AFNI_resam_av_CB") ;

   /* sanity check */

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   /* assign resampling type based on which arrowval, and redraw */

   if( av == im3d->vwid->dmode->func_resam_av ){
STATUS("set func_resam_mode") ;
      im3d->vinfo->func_resam_mode = av->ival ;
      isfunc = True ;
      if( im3d->b123_fim != NULL ){
         im3d->b123_fim->resam_code =
          im3d->b231_fim->resam_code =
           im3d->b312_fim->resam_code = im3d->vinfo->func_resam_mode ;
      }

   } else if( av == im3d->vwid->dmode->thr_resam_av ){  /* 09 Dec 1997 */
STATUS("set thr_resam_mode") ;
      im3d->vinfo->thr_resam_mode = av->ival ;
      isfunc = True ;
      if( im3d->b123_fim != NULL ){
         im3d->b123_fim->thr_resam_code =
          im3d->b231_fim->thr_resam_code =
           im3d->b312_fim->thr_resam_code = im3d->vinfo->thr_resam_mode ;
      }

   } else if( av == im3d->vwid->dmode->anat_resam_av ){
STATUS("set anat_resam_mode") ;
      im3d->vinfo->anat_resam_mode = av->ival ;
      isfunc = False ;
      im3d->b123_anat->resam_code =
       im3d->b231_anat->resam_code =
        im3d->b312_anat->resam_code = im3d->vinfo->anat_resam_mode ;
   } else {
      EXRETURN ;  /* should not occur! */
   }

   SHOW_AFNI_PAUSE ;
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
   Three_D_View * im3d = (Three_D_View *) cd ;
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

      case UNDERLAY_THRFUNC:                 /* set underlay to function */
      case UNDERLAY_ALLFUNC:
         if( ISVALID_3DIM_DATASET(im3d->fim_now) && ISFUNC(im3d->fim_now) ){

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

   /*--- May 1996: destroy useless needed graph windows ---*/

   im3d->ignore_seq_callbacks = AFNI_IGNORE_REDRAWS ;  /* 16 Feb 97 */

   if( ! DSET_GRAPHABLE(im3d->b123_ulay->dset) || ! BRICK_DRAWABLE(im3d->b123_ulay) ){
      if( im3d->g123 != NULL ){
         drive_MCW_grapher( im3d->g123 , graDR_destroy , NULL ) ;
         im3d->g123 = NULL ;
      }
      XtSetSensitive( im3d->vwid->imag->graph_xyz_pb , False ) ;
   } else {
      XtSetSensitive( im3d->vwid->imag->graph_xyz_pb , True ) ;
   }

   if( ! DSET_GRAPHABLE(im3d->b231_ulay->dset) || ! BRICK_DRAWABLE(im3d->b231_ulay) ){
      if( im3d->g231 != NULL ){
         drive_MCW_grapher( im3d->g231 , graDR_destroy , NULL ) ;
         im3d->g231 = NULL ;
      }
      XtSetSensitive( im3d->vwid->imag->graph_yzx_pb , False ) ;
   } else {
      XtSetSensitive( im3d->vwid->imag->graph_yzx_pb , True ) ;
   }

   if( ! DSET_GRAPHABLE(im3d->b312_ulay->dset) || ! BRICK_DRAWABLE(im3d->b312_ulay) ){
      if( im3d->g312 != NULL ){
         drive_MCW_grapher( im3d->g312 , graDR_destroy , NULL ) ;
         im3d->g312 = NULL ;
      }
      XtSetSensitive( im3d->vwid->imag->graph_zxy_pb , False ) ;
   } else {
      XtSetSensitive( im3d->vwid->imag->graph_zxy_pb , True ) ;
   }

   /** 05 Mar 1997: disable viewers if x or y dimension is 1 pixel **/

   if( ! BRICK_DRAWABLE(im3d->b123_ulay) ){
      if( im3d->s123 != NULL ){
         drive_MCW_imseq( im3d->s123 , isqDR_destroy , NULL ) ;
         im3d->s123 = NULL ;
      }
      XtSetSensitive( im3d->vwid->imag->image_xyz_pb , False ) ;
   } else {
      XtSetSensitive( im3d->vwid->imag->image_xyz_pb , True ) ;
   }

   if( ! BRICK_DRAWABLE(im3d->b231_ulay) ){
      if( im3d->s231 != NULL ){
         drive_MCW_imseq( im3d->s231 , isqDR_destroy , NULL ) ;
         im3d->s231 = NULL ;
      }
      XtSetSensitive( im3d->vwid->imag->image_yzx_pb , False ) ;
   } else {
      XtSetSensitive( im3d->vwid->imag->image_yzx_pb , True ) ;
   }

   if( ! BRICK_DRAWABLE(im3d->b312_ulay) ){
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

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

char * AFNI_controller_label( Three_D_View * im3d )
{
   static char clabel[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;
   static char str[8] ;
   int ic ;

   ic = AFNI_controller_index( im3d ) ;
   if( ic < 0 || ic > 26 ) strcpy(str," ") ;
   else                    sprintf(str,"[%c] ",clabel[ic]) ;
   return str ;
}

/*--------------------------------------------------------------------
  Set the titles in all windows
----------------------------------------------------------------------*/

void AFNI_set_window_titles( Three_D_View * im3d )
{
   Boolean redo_title ;
   char ttl[THD_MAX_NAME] , nam[THD_MAX_NAME] ;
   char * tnam ;

ENTRY("AFNI_set_window_titles") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   sprintf(ttl , "%s%s: " , AFNI_controller_label(im3d),GLOBAL_argopt.title_name) ;

   strcpy( nam , im3d->anat_now->dblk->diskptr->directory_name ) ;
   strcat( nam , im3d->anat_now->dblk->diskptr->filecode ) ;
   tnam = THD_trailname(nam,SESSTRAIL) ;
   strcat( ttl , tnam ) ;

   if( ISVALID_3DIM_DATASET(im3d->fim_now) ){
      strcat( ttl , " & " ) ;
      strcat( ttl , im3d->fim_now->dblk->diskptr->filecode ) ;
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

/*--------------------------------------------------------------------
   used to select between sessions and datasets
----------------------------------------------------------------------*/

/** labels for the chooser window **/

static char * dset_choice[] = { "Session" , "Anatomy" , "Function" } ;

/** max size of strings in the list **/

#define STRLIST_SIZE (THD_MAX_PREFIX+64)

void AFNI_choose_dataset_CB( Widget w , XtPointer cd , XtPointer cb )
{
   static char * strlist[THD_MAX_CHOICES] ;  /* strings to choose between */
   static int first_call = 1 ;               /* initialization flag */

   int num_str , ii , init_str , vv , jj ;
   char * label ;
   Widget wpar ;
   Three_D_View * im3d = (Three_D_View *) cd ;
   int llen , ltop ;

ENTRY("AFNI_choose_dataset_CB") ;

   /*--- initialize ---*/

   if( ! IM3D_VALID(im3d) ) EXRETURN ;
   if( GLOBAL_library.have_dummy_dataset ){ BEEPIT ; EXRETURN ; }

   if( first_call ){
      for( ii=0 ; ii < THD_MAX_CHOICES ; ii++ )
         strlist[ii] = XtMalloc( sizeof(char) * (STRLIST_SIZE+1) ) ;
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
                      THD_MAX_PREFIX ) ;
      }

      init_str = im3d->vinfo->sess_num ;
      label    = dset_choice[0] ;

   /*--- make a list of anatomy names ---*/

   } else if( w == im3d->vwid->view->choose_anat_pb ||
              w == im3d->vwid->view->popchoose_anat_pb ){

      wpar    = im3d->vwid->view->choose_anat_pb ;
      num_str = im3d->ss_now->num_anat ;
      if( num_str < 1 ) EXRETURN ;

      ltop = 4 ;
      for( ii=0 ; ii < num_str ; ii++ ){
         for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ )
            if( ISVALID_3DIM_DATASET(im3d->ss_now->anat[ii][vv]) ) break ;

         if( vv <= LAST_VIEW_TYPE ){
            llen = strlen( im3d->ss_now->anat[ii][vv]->dblk->diskptr->prefix ) ;
            ltop = MAX( ltop , llen ) ;
         }
      }

      for( ii=0 ; ii < num_str ; ii++ ){
         for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ )
            if( ISVALID_3DIM_DATASET(im3d->ss_now->anat[ii][vv]) ) break ;

         if( vv <= LAST_VIEW_TYPE ){
            sprintf( strlist[ii] , "%-*s" ,
                     ltop,im3d->ss_now->anat[ii][vv]->dblk->diskptr->prefix ) ;

            strcat( strlist[ii] , " [" ) ;
            strcat( strlist[ii] ,
                    ANAT_prefixstr[ im3d->ss_now->anat[ii][vv]->func_type ] ) ;

            if( DSET_NUM_TIMES(im3d->ss_now->anat[ii][vv]) > 1 ){
               int ll = strlen(strlist[ii]) ;
               sprintf( strlist[ii]+ll , ":3D+t:%d]" ,
                        DSET_NUM_TIMES(im3d->ss_now->anat[ii][vv]) ) ;
            } else if( ISANATBUCKET(im3d->ss_now->anat[ii][vv]) ){
               int ll = strlen(strlist[ii]) ;
               sprintf( strlist[ii]+ll , ":%d]" ,
                        DSET_NVALS(im3d->ss_now->anat[ii][vv]) ) ;
            } else {
               strcat( strlist[ii] , "]" ) ;
            }

            if( DSET_GRAPHABLE(im3d->ss_now->anat[ii][vv]) )
               strcat( strlist[ii] , "*" ) ;

            if( DSET_COMPRESSED(im3d->ss_now->anat[ii][vv]) )
               strcat( strlist[ii] , "z" ) ;

         } else
            MCW_strncpy( strlist[ii] , "?????????" , THD_MAX_PREFIX ) ;
      }

      init_str = im3d->vinfo->anat_num ;
      label    = dset_choice[1] ;

   /*--- make a list of function names ---*/

   } else if( w == im3d->vwid->view->choose_func_pb ||
              w == im3d->vwid->view->popchoose_func_pb ){

      wpar    = im3d->vwid->view->choose_func_pb ;
      num_str = im3d->ss_now->num_func ;
      if( num_str < 1 ) EXRETURN ;

      ltop = 4 ;
      for( ii=0 ; ii < num_str ; ii++ ){
         for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ )
            if( ISVALID_3DIM_DATASET(im3d->ss_now->func[ii][vv]) ) break ;

         if( vv <= LAST_VIEW_TYPE ){
            llen = strlen( im3d->ss_now->func[ii][vv]->dblk->diskptr->prefix ) ;
            ltop = MAX( ltop , llen ) ;
         }
      }

      for( ii=0 ; ii < num_str ; ii++ ){
         for( vv=FIRST_VIEW_TYPE ; vv <= LAST_VIEW_TYPE ; vv++ )
            if( ISVALID_3DIM_DATASET(im3d->ss_now->func[ii][vv]) ) break ;

         if( vv <= LAST_VIEW_TYPE ){
            sprintf( strlist[ii] , "%-*s" ,
                     ltop , im3d->ss_now->func[ii][vv]->dblk->diskptr->prefix ) ;

            strcat( strlist[ii] , " [" ) ;
            strcat( strlist[ii] ,
                    FUNC_prefixstr[ im3d->ss_now->func[ii][vv]->func_type ] ) ;

            if( DSET_NUM_TIMES(im3d->ss_now->func[ii][vv]) > 1 ){
               int ll = strlen(strlist[ii]) ;
               sprintf( strlist[ii]+ll , ":3D+t:%d]" ,
                        DSET_NUM_TIMES(im3d->ss_now->func[ii][vv]) ) ;
            } else if( ISFUNCBUCKET(im3d->ss_now->func[ii][vv]) ){
               int ll = strlen(strlist[ii]) ;
               sprintf( strlist[ii]+ll , ":%d]" ,
                        DSET_NVALS(im3d->ss_now->func[ii][vv]) ) ;
            } else {
               strcat( strlist[ii] , "]" ) ;
            }

            if( DSET_COMPRESSED(im3d->ss_now->anat[ii][vv]) )
               strcat( strlist[ii] , "z" ) ;

         } else
            MCW_strncpy( strlist[ii] , "*********" , THD_MAX_PREFIX ) ;

      }

      init_str = im3d->vinfo->func_num ;
      label    = dset_choice[2] ;

   } else {
      XBell( im3d->dc->display , 100 ) ; EXRETURN ;  /* bad news! */
   }

   /*--- call the chooser ---*/

   MCW_choose_strlist( wpar , label , num_str , init_str , strlist ,
                       AFNI_finalize_dataset_CB , (XtPointer) im3d ) ;

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/

void AFNI_finalize_dataset_CB( Widget wcall ,
                               XtPointer cd , MCW_choose_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   int old_sess , old_anat , old_func , old_view ;
   int new_sess=-1 , new_anat=-1 , new_func=-1 , new_view=-1 ;
   int ii , vv , ff ;
   THD_session * ss_new ;

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

      if( ISVALID_3DIM_DATASET(ss_new->anat[old_anat][old_view]) ){  /* are OK */
         new_anat = old_anat ;
      } else {
         for( ii=0 ; ii < ss_new->num_anat ; ii++ )
            if( ISVALID_3DIM_DATASET(ss_new->anat[ii][old_view]) ){
               new_anat = ii ; break ;
            }
      }
      if( new_anat < 0 ) new_anat = 0 ;  /* use 1st if no match */

      /* find a view to fit this chosen anat */

      if( ISVALID_3DIM_DATASET(ss_new->anat[new_anat][old_view]) ){ /* are OK */
         new_view = old_view ;
      } else {
         for( vv=old_view-1 ; vv >= FIRST_VIEW_TYPE ; vv-- )  /* look below */
            if( ISVALID_3DIM_DATASET(ss_new->anat[new_anat][vv]) ) break ;

         if( vv >= FIRST_VIEW_TYPE ){  /* found it below */
            new_view = vv ;
         } else {                      /* look above */
            for( vv=old_view+1 ; vv <= LAST_VIEW_TYPE ; vv++ )
               if( ISVALID_3DIM_DATASET(ss_new->anat[new_anat][vv]) ) break ;

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
      if( ISVALID_3DIM_DATASET(ss_new->func[old_func][new_view]) ){  /* are OK */

         new_func = old_func ;
      } else {
         for( ff=0 ; ff < ss_new->num_func ; ff++ )  /* search */
            if( ISVALID_3DIM_DATASET(ss_new->func[ff][new_view]) ) break ;

         if( ff < ss_new->num_func ) new_func = ff ;  /* found one */
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
      if( new_anat < 0 || new_anat >= ss_new->num_anat ){
         XBell( im3d->dc->display , 100 ) ; EXRETURN ;  /* bad! */
      }

      /* find a view to fit this chosen anat */

      if( ISVALID_3DIM_DATASET(ss_new->anat[new_anat][old_view]) ){ /* are OK */
         new_view = old_view ;
      } else {
         for( vv=old_view-1 ; vv >= FIRST_VIEW_TYPE ; vv-- )  /* look below */
            if( ISVALID_3DIM_DATASET(ss_new->anat[new_anat][vv]) ) break ;

         if( vv >= FIRST_VIEW_TYPE ){  /* found it below */
            new_view = vv ;
         } else {                      /* look above */
            for( vv=old_view+1 ; vv <= LAST_VIEW_TYPE ; vv++ )
               if( ISVALID_3DIM_DATASET(ss_new->anat[new_anat][vv]) ) break ;

            if( vv <= LAST_VIEW_TYPE ){  /* found it above */
               new_view = vv ;
            } else {
               XBell(im3d->dc->display,100) ; EXRETURN ;  /* bad news */
            }
         }
      }

      /* find a func to match this view */

#ifdef FINDAFUNC
      if( ISVALID_3DIM_DATASET(ss_new->func[old_func][new_view]) ){ /* are OK */

         new_func = old_func ;
      } else {
         for( ff=0 ; ff < ss_new->num_func ; ff++ )  /* search */
            if( ISVALID_3DIM_DATASET(ss_new->func[ff][new_view]) ) break ;

         if( ff < ss_new->num_func ) new_func = ff ;  /* found one */
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
      if( new_func < 0 || new_func >= ss_new->num_func ){
         XBell( im3d->dc->display , 100 ) ; EXRETURN ;  /* bad! */
      }

      /* find a view to fit this chosen func */

      if( ISVALID_3DIM_DATASET(ss_new->func[new_func][old_view]) ){ /* are OK */
         new_view = old_view ;
      } else {
         for( vv=old_view-1 ; vv >= FIRST_VIEW_TYPE ; vv-- )  /* look below */
            if( ISVALID_3DIM_DATASET(ss_new->func[new_func][vv]) ) break ;

         if( vv >= FIRST_VIEW_TYPE ){  /* found it below */
            new_view = vv ;
         } else {                      /* look above */
            for( vv=old_view+1 ; vv <= LAST_VIEW_TYPE ; vv++ )
               if( ISVALID_3DIM_DATASET(ss_new->func[new_func][vv]) ) break ;

            if( vv <= LAST_VIEW_TYPE ){  /* found it above */
               new_view = vv ;
            } else {
               XBell(im3d->dc->display,100) ; EXRETURN ;  /* bad news */
            }
         }
      }

      /* find an anat to go with the new view (this is NOT optional) */

      if( ISVALID_3DIM_DATASET(ss_new->anat[old_anat][new_view]) ){  /* are OK */

         new_anat = old_anat ;
      } else {
         for( ff=0 ; ff < ss_new->num_anat ; ff++ )  /* search */
            if( ISVALID_3DIM_DATASET(ss_new->anat[ff][new_view]) ) break ;

         if( ff < ss_new->num_anat ) new_anat = ff ;  /* found one */
      }
      if( new_anat < 0 ){
         XBell( im3d->dc->display , 100 ) ; EXRETURN ;  /* bad! */
      }

   /*--- switch to Hell? ---*/

   } else {
      XBell( im3d->dc->display , 100 ) ; EXRETURN ;  /* bad! */
   }

   /*--- make sure all values are set OK-ly ---*/

   if( new_view < 0 || new_sess < 0 || new_anat < 0 || new_func < 0 ){
      XBell( im3d->dc->display , 100 ) ; EXRETURN ;  /* bad! */
   }

   /*- beep & flash viewing control box if view type changes -*/

   if( old_view != new_view ){
      XBell( im3d->dc->display , 100 ) ;
      MCW_set_bbox( im3d->vwid->view->view_bbox , 1 << new_view ) ;
      MCW_invert_widget( im3d->vwid->view->view_bbox->wbut[new_view] ) ;
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

   if( old_view != new_view ){            /* end flash */
      XBell( im3d->dc->display , 100 ) ;
      MCW_invert_widget( im3d->vwid->view->view_bbox->wbut[new_view] ) ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------
   Routines to close and open a file selection
   dialog associated with the im3d viewer.
------------------------------------------------------------------------*/

void AFNI_close_file_dialog_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_close_file_dialog") ;

   if( im3d->vwid->file_dialog != NULL )
      XtPopdown( im3d->vwid->file_dialog ) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------
   After this is called, the calling routine must set the callbacks,
   window title, etc., appropriately.
------------------------------------------------------------------------*/

void AFNI_make_file_dialog( Three_D_View * im3d )
{

ENTRY("AFNI_make_file_dialog") ;

   /*** make a new dialog? ***/

   if( im3d->vwid->file_dialog == NULL ){
      XmString oklabel , cancellabel ;

STATUS("creating new dialog") ;

      im3d->vwid->file_dialog =
         XtVaCreatePopupShell(
           "menu" , xmDialogShellWidgetClass , im3d->vwid->top_shell ,
              XmNtitle , "MCW AFNI" ,
              XmNdeleteResponse , XmDO_NOTHING ,
              XmNinitialResourcesPersistent , False ,
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
              XmNtraversalOn       , False ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

      XmStringFree(oklabel) ; XmStringFree(cancellabel) ;

      im3d->vwid->file_cb = NULL ;
      im3d->vwid->file_cd = NULL ;

#if 1
      { static char * redcolor = NULL ;
        Widget www ;
        if( redcolor == NULL ){ HOTCOLOR(im3d->vwid->top_shell,redcolor) ; }
        www = XmFileSelectionBoxGetChild( im3d->vwid->file_sbox ,
                                          XmDIALOG_TEXT ) ;
        if( www != NULL ) MCW_set_widget_bg( www , redcolor , 0 ) ;
      }
#endif

   } else if( im3d->vwid->file_cb != NULL ){

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
   }

   EXRETURN ;
}

/*----------------------------------------------------------------
  Start getting ready to read a new session in.  This
  is the CB for the "Read Sess" button.  We'll get a filename
  from the user, and process it in another routine.
------------------------------------------------------------------*/

void AFNI_read_sess_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

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

   im3d->vwid->file_cb = AFNI_finalize_read_sess_CB ;
   im3d->vwid->file_cd = cd ;

   XtVaSetValues( im3d->vwid->file_dialog,
                     XmNtitle, "AFNI: Read Session",
                  NULL ) ;

   XtPopup( im3d->vwid->file_dialog , XtGrabNone ) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------
   Got a button press from the file selection dialog,
   so process it (maybe read in a new session!)
-----------------------------------------------------------------------*/

void AFNI_finalize_read_sess_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   XmFileSelectionBoxCallbackStruct * cbs = (XmFileSelectionBoxCallbackStruct *) cb ;

ENTRY("AFNI_finalize_read_sess_CB") ;

   switch( cbs->reason ){

      /** close the file selection dialog **/

      case XmCR_CANCEL:
         XtPopdown( im3d->vwid->file_dialog ) ;
      break ;

      /** try to read a new session **/

      case XmCR_OK:{
         char * text = NULL ;
         XmStringGetLtoR( cbs->value , XmFONTLIST_DEFAULT_TAG , &text ) ;
         if( text != NULL ){

            THD_session * new_ss = NULL ;

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
               int ii , eq ;
               THD_session * old_ss ;

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

            if( new_ss == NULL ){  /** failed to read anything useful **/

STATUS("failed to read new session") ;
               XBell(im3d->dc->display,100) ;
               (void) MCW_popup_message( w ,
                                           "******************************\n"
                                           "** Cannot read any datasets **\n"
                                           "******************************"
                                         , MCW_USER_KILL | MCW_TIMER_KILL ) ;

            } else if ( new_ss->num_anat <= 0 ){  /** no anatomical datasets? **/

STATUS("no anatomies") ;
               XBell(im3d->dc->display,100) ;
               (void) MCW_popup_message( w ,
                                           "******************************\n"
                                           "** Session has no anatomies **\n"
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

            } else {  /** GOOD!  Actually read in a new session.   **/
                      /** (The following is from AFNI_read_inputs) **/
               int qd , vv ;
               char str[356] ;

STATUS("processing new session") ;
               new_ss->parent = NULL ;

               for( qd=0 ; qd < new_ss->num_anat ; qd++ )
                  for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )
                     PARENTIZE( new_ss->anat[qd][vv] , NULL ) ;

               for( qd=0 ; qd < new_ss->num_func ; qd++ )
                  for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )
                     PARENTIZE( new_ss->func[qd][vv] , NULL ) ;

               /* if we were living with a dummy, fix that */

               if( GLOBAL_library.have_dummy_dataset ) UNDUMMYIZE ;

               /* put the new session into place in the list of sessions */

STATUS("adding new session to list") ;
               GLOBAL_library.sslist->ssar[GLOBAL_library.sslist->num_sess]
                 = new_ss ;
               (GLOBAL_library.sslist->num_sess)++ ;
               THD_reconcile_parents( GLOBAL_library.sslist ) ;

               sprintf(str," \n Session #%2d"
                            "\n %s"
                            "\n %d anatomical datasets,"
                            "\n %d functional datasets\n",
                       GLOBAL_library.sslist->num_sess ,
                       new_ss->sessname, new_ss->num_anat, new_ss->num_func ) ;

               (void) MCW_popup_message( im3d->vwid->dmode->read_sess_pb,
                                         str, MCW_USER_KILL | MCW_TIMER_KILL ) ;

STATUS("rescanning timeseries files") ;
               AFNI_rescan_timeseries_CB(NULL,NULL,NULL) ;

               XtPopdown( im3d->vwid->file_dialog ) ;
            }

STATUS("freeing 'text' variable") ;
            myXtFree(text) ;
         }
      }
      break ;

      case XmCR_HELP:
         (void) MCW_popup_message( w ,
                    "To choose a new session, use the\n"
                    "Directories and Files selectors,\n"
                    "and the Filter entry and button,\n"
                    "to get the 'Selection' box correct;\n"
                    "that is, 'Selection' should either\n"
                    "be the name of the session directory,\n"
                    "or the name of a file in the session\n"
                    "directory.  Then press 'Set'.\n"
                    "\n"
                    "N.B.: To see datasets in the new\n"
                    "      session, you must use the\n"
                    "      'Switch Session' button."
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
   Three_D_View * im3d = (Three_D_View *) cd ;
   XmString xstr ;

ENTRY("AFNI_read_1D_CB") ;

   AFNI_make_file_dialog( im3d ) ;

   XtAddCallback( im3d->vwid->file_sbox , XmNokCallback ,
                  AFNI_finalize_read_1D_CB , cd ) ;

   XtAddCallback( im3d->vwid->file_sbox , XmNcancelCallback ,
                  AFNI_finalize_read_1D_CB , cd ) ;

   XtAddCallback( im3d->vwid->file_sbox , XmNhelpCallback ,
                  AFNI_finalize_read_1D_CB , cd ) ;

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

   XtPopup( im3d->vwid->file_dialog , XtGrabNone ) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------
   Got a button press from the file selection dialog,
   so process it (maybe read in a new timeseries!)
-----------------------------------------------------------------------*/

void AFNI_finalize_read_1D_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   XmFileSelectionBoxCallbackStruct * cbs = (XmFileSelectionBoxCallbackStruct *) cb ;

ENTRY("AFNI_finalize_read_1D_CB") ;

   switch( cbs->reason ){

      /** close the file selection dialog **/

      case XmCR_CANCEL:
         XtPopdown( im3d->vwid->file_dialog ) ;
      break ;

      /** try to read a new timeseries **/

      case XmCR_OK:{
         char * text = NULL ;
         MRI_IMAGE * tsim = NULL , * flim ;
         float * far ;
         int ii ;

         XmStringGetLtoR( cbs->value , XmFONTLIST_DEFAULT_TAG , &text ) ;
         tsim = mri_read_ascii( text ) ;
         if( tsim == NULL || tsim->ny < 2 ){
            XBell(im3d->dc->display,100) ;
            (void) MCW_popup_message( w ,
                                       "********************************\n"
                                       "** Cannot read data from file **\n"
                                       "********************************"
                                      , MCW_USER_KILL | MCW_TIMER_KILL ) ;
            myXtFree(text) ;
            break ;
         }

         flim = mri_transpose(tsim) ; mri_free(tsim) ;
         far  = MRI_FLOAT_PTR(flim) ;
         for( ii=0 ; ii < flim->nvox ; ii++ )
            if( fabs(far[ii]) >= 33333.0 ) far[ii] = WAY_BIG ;

         PLUTO_register_timeseries( text , flim ) ;
         mri_free(flim) ;
         myXtFree(text) ;
         XtPopdown( im3d->vwid->file_dialog ) ;
      }
      break ;

      case XmCR_HELP:
         (void) MCW_popup_message( w ,
                    "To choose a new timeseries, use the\n"
                    "Directories and Files selectors,\n"
                    "and the Filter entry and button,\n"
                    "to get the 'Selection' box correct;\n"
                    "that is, 'Selection' should show the\n"
                    "name of the 1D file which will be input.\n"
                    "Then press 'Set'.\n"
                 , MCW_USER_KILL ) ;
      break ;
   }
   EXRETURN ;
}

/*----------------------------------------------------------------
   Obey the command to rescan the current session
------------------------------------------------------------------*/

void AFNI_rescan_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_rescan_CB") ;

   SHOW_AFNI_PAUSE ;
   AFNI_rescan_session( im3d->vinfo->sess_num ) ;
   AFNI_process_dsetchange( im3d ) ;               /* 31 Mar 1999 */
   SHOW_AFNI_READY ;

   EXRETURN ;
}

void AFNI_rescan_all_CB( Widget w, XtPointer cd, XtPointer cb )
{
   int iss , cc ;
   Three_D_View * im3d ;

ENTRY("AFNI_rescan_all_CB") ;

   SHOW_AFNI_PAUSE ;
   for( iss=0 ; iss < GLOBAL_library.sslist->num_sess ; iss++ )
      AFNI_rescan_session( iss ) ;

   for( cc=0 ; cc < MAX_CONTROLLERS ; cc++ ){    /* 31 Mar 1999 */
      im3d = GLOBAL_library.controllers[cc] ;
      if( IM3D_OPEN(im3d) ) AFNI_process_dsetchange( im3d ) ;
   }

   SHOW_AFNI_READY ;

   EXRETURN ;
}

/*----------------------------------------------------------------------
  Re-read the session indexed by "sss".
  Much of this code is taken from AFNI_read_inputs.
  WARNING: this will do bad things if the user deletes the
           session directory or the current active datasets within it
           before trying this.  On the other hand, if the user is that
           stupid, bad things will probably have happened to him already
           (like being unable to open dataset files, or being unable to
           tie his shoes correctly).
------------------------------------------------------------------------*/

void AFNI_rescan_session( int sss )
{
   int vv , ii , cc ;
   THD_session *  new_ss , * old_ss ;
   Three_D_View * im3d ;
   MCW_idcode     anat_idcode[MAX_CONTROLLERS] ,
                  func_idcode[MAX_CONTROLLERS] ;
   THD_slist_find find ;

ENTRY("AFNI_rescan_session") ;

   if( GLOBAL_library.have_dummy_dataset ){ BEEPIT ; EXRETURN ; }

   /*--- sanity checks ---*/

   if( sss < 0 || sss >= GLOBAL_library.sslist->num_sess ){ BEEPIT ; EXRETURN ; }

   old_ss = GLOBAL_library.sslist->ssar[sss] ;
   if( ! ISVALID_SESSION(old_ss) ){ BEEPIT ; EXRETURN ; }

   /*--- Make sure that the dataset choosers are closed.
         Since these are just instances of the generic strlist
         chooser, and we can't tell what is being chosen just now,
         we'll just forcibly close the strlist chooser no matter what. ---*/

   POPDOWN_strlist_chooser ;

   /*--- mark all datasets in the old session for deletion from memory ---*/

STATUS("marking old session datasets") ;

   for( ii=0 ; ii < old_ss->num_anat ; ii++ )
      for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )
         if( ISVALID_3DIM_DATASET(old_ss->anat[ii][vv]) )
            old_ss->anat[ii][vv]->death_mark = DOOMED ;

   for( ii=0 ; ii < old_ss->num_func ; ii++ )
      for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )
         if( ISVALID_3DIM_DATASET(old_ss->func[ii][vv]) )
            old_ss->func[ii][vv]->death_mark = DOOMED ;

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

   if( new_ss == NULL || new_ss->num_anat <= 0 ){
      fprintf(stderr,"\n*** Fatal error: Rescan of session %s finds no anats!\a\n",
              old_ss->sessname ) ;
      exit(1) ;
   }

   myXtFree( old_ss ) ;  /* no longer need this */

   /* for anats, just set parent pointers */

STATUS("PARENTIZE-ing datasets in new session") ;

   new_ss->parent = NULL ;
   for( ii=0 ; ii < new_ss->num_anat ; ii++ )
      for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )
         PARENTIZE( new_ss->anat[ii][vv] , NULL ) ;

   /* for funcs, just set parent pointers */

   for( ii=0 ; ii < new_ss->num_func ; ii++ )
      for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ )
         PARENTIZE( new_ss->func[ii][vv] , NULL ) ;

   /* put the new session into place in the list of sessions */

   GLOBAL_library.sslist->ssar[sss] = new_ss ;

   /* assign the warp and anatomy parent pointers;
      then, make any datasets that don't exist but logically
      descend from the warp and anatomy parents just assigned */

   THD_reconcile_parents( GLOBAL_library.sslist ) ;
   AFNI_force_adoption( new_ss , GLOBAL_argopt.warp_4D ) ;
   AFNI_make_descendants( GLOBAL_library.sslist ) ;

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
            im3d->vinfo->anat_num = find.anat_index ;
         } else {
            for( ii=0 ; ii < new_ss->num_anat ; ii++ )
               if( ISVALID_3DIM_DATASET(new_ss->anat[ii][vv]) ) break ;
            if( ii < new_ss->num_anat ){
               im3d->vinfo->anat_num = ii ;
            } else {
               fprintf(stderr,
                       "\n*** Fatal error:"
                       " Cannot find anat dataset to switch to after"
                       " rescanning session %s\a\n",
                       new_ss->sessname ) ;
               exit(1) ;
            }
         }

         /* do the same for old func dataset, if any */

         if( ! ISZERO_IDCODE(func_idcode[cc]) ){
            find = THD_dset_in_session( FIND_IDCODE ,
                                        &(func_idcode[cc]) , new_ss ) ;

            if( find.dset != NULL && find.view_index == vv ){
               im3d->vinfo->func_num = find.func_index ;
            } else {
               for( ii=0 ; ii < new_ss->num_func ; ii++ )
                  if( ISVALID_3DIM_DATASET(new_ss->func[ii][vv]) ) break ;
               if( ii < new_ss->num_func ){
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

   EXRETURN ;
}

/*---------------------------------------------------------------
   Rescan for timeseries files
-----------------------------------------------------------------*/

void AFNI_rescan_timeseries_CB(Widget w, XtPointer cd, XtPointer cb)
{
   int iss , inew , jold , nnew , nold ;
   THD_string_array * dlist ;
   THD_session * ss ;
   MRI_IMARR * newtsar ;
   MRI_IMAGE * newim , * oldim ;

ENTRY("AFNI_rescan_timeseries_CB") ;

   /** assemble list of directories **/

   if( GLOBAL_library.have_dummy_dataset ){ BEEPIT ; EXRETURN ; }

   POPDOWN_timeseries_chooser ;
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
         ADDTO_IMARR(GLOBAL_library.timeseries,newim) ;  /* is new */
      } else {
         mri_free(newim) ;                               /* is old */
      }
   }

   FREE_IMARR(newtsar) ;
   EXRETURN ;
}

/*---------------------------------------------------------------
   callback for the anatmode bbox
-----------------------------------------------------------------*/

void AFNI_anatmode_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   int old_val , new_val ;
   THD_fvec3 fv ;
   THD_ivec3 iv ;

ENTRY("AFNI_anatmode_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   old_val = im3d->vinfo->force_anat_wod ;
   new_val = MCW_val_bbox( im3d->vwid->dmode->anatmode_bbox ) ;
   new_val = (new_val == DMODE_BRICK_BVAL) ? (False) : (True) ;

   if( new_val != old_val ){
      im3d->vinfo->force_anat_wod = new_val ;
      SHOW_AFNI_PAUSE ;
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
   Three_D_View * im3d = (Three_D_View *) cd ;
   int old_val , new_val ;
   THD_fvec3 fv ;
   THD_ivec3 iv ;

ENTRY("AFNI_funcmode_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   old_val = im3d->vinfo->force_func_wod ;
   new_val = MCW_val_bbox( im3d->vwid->dmode->funcmode_bbox ) ;
   new_val = (new_val == DMODE_BRICK_BVAL) ? (False) : (True) ;

   if( new_val != old_val ){
      im3d->vinfo->force_func_wod = new_val ;
      SHOW_AFNI_PAUSE ;
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

void AFNI_modify_viewing( Three_D_View * im3d , Boolean rescaled )
{
   THD_fvec3 fv ;
   THD_ivec3 iv ;

ENTRY("AFNI_modify_viewing") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   /* set up datasets for new imaging */

   AFNI_setup_viewing( im3d , rescaled ) ;

   /* transform current POV to new indices */

   LOAD_ANAT_VIEW(im3d) ;  /* 02 Nov 1996 */

   fv = THD_dicomm_to_3dmm(
          im3d->anat_now ,
          TEMP_FVEC3(im3d->vinfo->xi, im3d->vinfo->yj, im3d->vinfo->zk) ) ;

   iv = THD_3dmm_to_3dind( im3d->anat_now , fv ) ;

   /* and redisplay the images */

   DISABLE_LOCK ;
   AFNI_set_viewpoint( im3d, iv.ijk[0],iv.ijk[1],iv.ijk[2] , REDISPLAY_ALL ) ;
   ENABLE_LOCK ;

   SAVE_VPT(im3d) ;
   EXRETURN ;
}

/*----------------------------------------------------------------
  23 Nov 1996: Setup to write out many datasets
------------------------------------------------------------------*/

void AFNI_write_many_dataset_CB( Widget w, XtPointer cd, XtPointer cb )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   static MCW_idcode *  idclist  = NULL ;
   static char       ** strlist  = NULL ;
   static int           num_dset = -1 ;

   int iss , id , vv , llen , ltop ;
   THD_session * ss ;
   THD_3dim_dataset * dset ;
   char nam[THD_MAX_NAME] , * tnam , qnam[THD_MAX_NAME] ;

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

      for( id=0 ; id < ss->num_anat ; id++ ){
         dset = ss->anat[id][vv] ;
         if( ISVALID_3DIM_DATASET(dset) && dset->warp_parent != NULL ){
            strcpy( nam , dset->dblk->diskptr->directory_name ) ;
            strcat( nam , dset->dblk->diskptr->filecode ) ;
            tnam = THD_trailname(nam,SESSTRAIL) ;
            llen = strlen(tnam) ; ltop = MAX(ltop,llen) ;
         }
      }

      for( id=0 ; id < ss->num_func ; id++ ){
         dset = ss->func[id][vv] ;
         if( ISVALID_3DIM_DATASET(dset) && dset->warp_parent != NULL ){
            strcpy( nam , dset->dblk->diskptr->directory_name ) ;
            strcat( nam , dset->dblk->diskptr->filecode ) ;
            tnam = THD_trailname(nam,SESSTRAIL) ;
            llen = strlen(tnam) ; ltop = MAX(ltop,llen) ;
         }
      }
   }

   num_dset = 0 ;
   for( iss=0 ; iss < GLOBAL_library.sslist->num_sess ; iss++ ){
      ss = GLOBAL_library.sslist->ssar[iss] ;

      /* check anat datasets */

      for( id=0 ; id < ss->num_anat ; id++ ){
         dset = ss->anat[id][vv] ;
         if( ISVALID_3DIM_DATASET(dset) && dset->warp_parent != NULL ){
            num_dset++ ;
            idclist = (MCW_idcode *) XtRealloc( (char *) idclist ,
                                                sizeof(MCW_idcode) * num_dset ) ;
            strlist = (char **)      XtRealloc( (char *) strlist ,
                                                sizeof(char *)     * num_dset ) ;

            strcpy( nam , dset->dblk->diskptr->directory_name ) ;
            strcat( nam , dset->dblk->diskptr->filecode ) ;
            tnam = THD_trailname(nam,SESSTRAIL) ;

            if( ISANATBUCKET(dset) )         /* 30 Nov 1997 */
               sprintf(qnam,"%-*s [%s:%d]" ,
                       ltop,tnam , ANAT_prefixstr[dset->func_type] , DSET_NVALS(dset) ) ;

            else if( DSET_NUM_TIMES(dset) == 1 )
               sprintf(qnam,"%-*s [%s]" ,
                       ltop,tnam ,ANAT_prefixstr[dset->func_type] ) ;

            else
               sprintf(qnam,"%-*s [%s:3D+t]" ,
                       ltop,tnam , ANAT_prefixstr[dset->func_type] ) ;

            strlist[num_dset-1] = XtNewString(qnam) ;
            idclist[num_dset-1] = dset->idcode ;
         }
      } /* end of loop over anats */

      /* check func datasets */

      for( id=0 ; id < ss->num_func ; id++ ){
         dset = ss->func[id][vv] ;
         if( ISVALID_3DIM_DATASET(dset) && dset->warp_parent != NULL ){
            num_dset++ ;
            idclist = (MCW_idcode *) XtRealloc( (char *) idclist ,
                                                sizeof(MCW_idcode) * num_dset ) ;
            strlist = (char **)      XtRealloc( (char *) strlist ,
                                                sizeof(char *)     * num_dset ) ;

            strcpy( nam , dset->dblk->diskptr->directory_name ) ;
            strcat( nam , dset->dblk->diskptr->filecode ) ;
            tnam = THD_trailname(nam,SESSTRAIL) ;

            if( ISFUNCBUCKET(dset) )             /* 30 Nov 1997 */
               sprintf(qnam,"%-*s [%s:%d]" ,
                       ltop,tnam , FUNC_prefixstr[dset->func_type] , DSET_NVALS(dset) ) ;

            else if( DSET_NUM_TIMES(dset) == 1 )
               sprintf(qnam,"%-*s [%s]" ,
                       ltop,tnam , FUNC_prefixstr[dset->func_type] ) ;

            else
               sprintf(qnam,"%-*s [%s:3D+t]" ,
                       ltop,tnam , FUNC_prefixstr[dset->func_type] ) ;

            strlist[num_dset-1] = XtNewString(qnam) ;
            idclist[num_dset-1] = dset->idcode ;
         }
      } /* end of loop over funcs */

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
   { THD_string_array * sar ;  /*** This code is for experiments only! ***/
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

void AFNI_do_many_writes( Widget wpar , XtPointer cd , MCW_choose_cbs * cbs )
{
   MCW_idcode * idclist = (MCW_idcode *) cd ;
   Three_D_View * im3d = NULL , * qq3d ;
   THD_3dim_dataset * dset ;
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

   /** loop through selected datasets and do the dirty work **/

   for( ib=0 ; ib < cbs->nilist ; ib++ ){
      dset = PLUTO_find_dset( idclist + cbs->ilist[ib] ) ;
      if( ISVALID_3DIM_DATASET(dset) && dset->warp_parent != NULL ){

         fprintf(stderr,"-- writing dataset %s%s (%d of %d)\n" ,
                        dset->dblk->diskptr->directory_name ,
                        dset->dblk->diskptr->filecode ,
                        ib+1 , cbs->nilist ) ;

         new_daxes.type = DATAXES_TYPE ;

         THD_edit_dataxes( im3d->vinfo->resam_vox , dset->daxes , &new_daxes ) ;

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
   Three_D_View * im3d = (Three_D_View *) cd ;
   THD_3dim_dataset * dset = NULL ;
   THD_dataxes      * daxes = NULL ;
   Widget wmsg ;
   int resam_mode ;
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
          im3d->vinfo->resam_vox > 0.0      ;

   destroy = (dset->warp_parent == NULL) ;      /* check for destruction */

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

   daxes = CURRENT_DAXES(im3d->anat_now) ;

   good = AFNI_refashion_dataset( im3d , dset , daxes , resam_mode ) ;

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

Boolean AFNI_refashion_dataset( Three_D_View * im3d ,
                                THD_3dim_dataset * dset ,
                                THD_dataxes * daxes , int resam_mode )
{
   THD_datablock * dblk  = dset->dblk ;
   THD_diskptr   * dkptr = dset->dblk->diskptr ;
   Boolean good ;
   int npix , nx,ny,nz,nv , kk , ival , code , nzv , dsiz , isfunc , cmode ;
   MRI_IMAGE * im ;
   void * imar ;
   FILE * far ;
   float brfac_save ;

   Boolean picturize ;
   Pixmap brain_pixmap ;

#ifndef DONT_USE_METER
   Widget meter = NULL ;
   int meter_perc , meter_pold ;
#endif

ENTRY("AFNI_refashion_dataset") ;

   picturize = IM3D_OPEN(im3d) && im3d->vwid->picture != NULL &&
               afni48_pixmap != XmUNSPECIFIED_PIXMAP ;

   if( picturize ){
      switch( ORIENT_xyz[daxes->zzorient] ){
         default:  brain_pixmap = XmUNSPECIFIED_PIXMAP ; break ;
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

   *(dset->wod_daxes)      = *daxes ;            /* copy insides of daxes */
   dset->wod_flag          = True ;              /* mark for warp-on-demand */
   dset->vox_warp->type    = ILLEGAL_TYPE ;      /* mark for recomputation */

   /* copy the new geometric information into various places */

   *(dset->daxes)     = *(daxes) ;               /* Make daxes permanent */
   dkptr->dimsizes[0] = dset->daxes->nxx ;       /* Will cause trouble */
   dkptr->dimsizes[1] = dset->daxes->nyy ;       /* if diskptr and     */
   dkptr->dimsizes[2] = dset->daxes->nzz ;       /* daxes don't match! */

   /* write the header out */

   good = THD_write_3dim_dataset( NULL,NULL , dset , False ) ;
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

   { int ibr ; int * typ ;
     typ = (int *) XtMalloc( sizeof(int) * dblk->nvals ) ;
     for( ibr=0 ; ibr < dblk->nvals ; ibr++ )
        typ[ibr] = DBLK_BRICK_TYPE(dblk,ibr) ;
     THD_init_datablock_brick( dblk , dblk->nvals , typ ) ;
     myXtFree( typ ) ;
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
          kk,MRI_TYPE_name[im->kind] , im->nx,im->ny ) ;
  STATUS(str) ; }
#endif

         imar = mri_data_pointer(im) ;
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

   if( picturize ) PICTURIZE(mcw_pixmap) ;
#ifndef DONT_USE_METER
   MCW_set_meter( meter , 100 ) ;
#endif

   /*--- do a little surgery on the dataset's storage flags ---*/

   dkptr->storage_mode = STORAGE_BY_BRICK ;
#if MMAP_THRESHOLD > 0
   dblk->malloc_type   = (dblk->total_bytes > MMAP_THRESHOLD)
                         ? DATABLOCK_MEM_MMAP : DATABLOCK_MEM_MALLOC ;

   if( cmode >= 0 ) dblk->malloc_type = DATABLOCK_MEM_MALLOC ;
#else
   dblk->malloc_type   = DATABLOCK_MEM_MALLOC ;
#endif

   /*--- recompute the statistics and rewrite the header to hold them ---*/

   STATUS("recomputing statistics") ;

   THD_load_statistics( dset ) ;

   STATUS("rewriting header") ;

   (void) THD_write_3dim_dataset( NULL,NULL , dset , False ) ;

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

  Jan 31, 1995: altered to avoid destruction of a dataset without
                a warp parent, since that dataset cannot be recreated.
--------------------------------------------------------------------*/

void AFNI_mark_for_death( THD_sessionlist * ssl )
{
   int iss , jdd , kvv , num_marked=0 ;
   THD_session * ss ;
   THD_3dim_dataset * dset ;

ENTRY("AFNI_mark_for_death") ;

   if( ! ISVALID_SESSIONLIST(ssl) ) EXRETURN ;

   /* loop over each session */

   for( iss=0 ; iss < ssl->num_sess ; iss++ ){
      ss = ssl->ssar[iss] ;
      if( !ISVALID_SESSION(ss) ) continue ;  /* no good ==> skip */

      /* loop over anats in this session */

      for( jdd=0 ; jdd < ss->num_anat ; jdd++ ){
         for( kvv=FIRST_VIEW_TYPE ; kvv <= LAST_VIEW_TYPE ; kvv++ ){

            dset = ss->anat[jdd][kvv] ;

            if( ISVALID_3DIM_DATASET(dset) &&                /* good dset */
                dset->anat_parent != NULL  &&                /* has parent */
                dset->anat_parent->death_mark == DOOMED &&   /* parent dies */
                dset->warp_parent != NULL                 ){ /* is a warp child */

               dset->death_mark = DOOMED ;
               num_marked ++ ;
            }
         }
      }

      /* do the same for funcs */

      for( jdd=0 ; jdd < ss->num_func ; jdd++ ){
         for( kvv=FIRST_VIEW_TYPE ; kvv <= LAST_VIEW_TYPE ; kvv++ ){

            dset = ss->func[jdd][kvv] ;

            if( ISVALID_3DIM_DATASET(dset) &&                /* good dset */
                dset->anat_parent != NULL  &&                /* has parent */
                dset->anat_parent->death_mark == DOOMED &&   /* parent dies */
                dset->warp_parent != NULL                 ){ /* is a warp child */

               dset->death_mark = DOOMED ;
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

void AFNI_andersonville( THD_sessionlist * ssl , Boolean kill_files )
{
   int iss , jdd , kvv , num_killed=0 ;
   THD_session * ss ;
   THD_3dim_dataset * dset ;
   Boolean kill_me ;

ENTRY("AFNI_andersonville") ;

   if( ! ISVALID_SESSIONLIST(ssl) ) EXRETURN ;

   /* loop over each session */

   for( iss=0 ; iss < ssl->num_sess ; iss++ ){
      ss = ssl->ssar[iss] ;
      if( !ISVALID_SESSION(ss) ) continue ;  /* no good ==> skip */

      /* loop over anats in this session */

      for( jdd=0 ; jdd < ss->num_anat ; jdd++ ){
         for( kvv=FIRST_VIEW_TYPE ; kvv <= LAST_VIEW_TYPE ; kvv++ ){

            dset = ss->anat[jdd][kvv] ;

            if( ISVALID_3DIM_DATASET(dset) &&    /* good dset */
                dset->death_mark == DOOMED   ){  /* alas, poor Yorick */

               kill_me = (kvv == VIEW_ORIGINAL_TYPE) ? False : kill_files ;
               THD_delete_3dim_dataset( dset , kill_me ) ;
               myXtFree( dset ) ;
               ss->anat[jdd][kvv] = NULL ; num_killed ++ ;
            }
         }
      }

      /* do the same for funcs */

      for( jdd=0 ; jdd < ss->num_func ; jdd++ ){
         for( kvv=FIRST_VIEW_TYPE ; kvv <= LAST_VIEW_TYPE ; kvv++ ){

            dset = ss->func[jdd][kvv] ;

            if( ISVALID_3DIM_DATASET(dset) &&    /* good dset */
                dset->death_mark == DOOMED   ){  /* alas, poor Yorick */

               kill_me = (kvv == VIEW_ORIGINAL_TYPE) ? False : kill_files ;
               THD_delete_3dim_dataset( dset , kill_me ) ;
               myXtFree( dset ) ;
               ss->func[jdd][kvv] = NULL ; num_killed ++ ;
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

void AFNI_imseq_clearstat( Three_D_View * im3d )
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

XmString AFNI_range_label( Three_D_View * im3d )
{
   char anat_minch[10] = " --------" , anat_maxch[10] = " --------" ,
        fim_minch[10]  = " --------" , fim_maxch[10]  = " --------" ,
        thr_minch[10]  = " --------" , thr_maxch[10]  = " --------"   ;
   char buf[256] , qbuf[16] ;
   XmString xstr ;
   int iv ;

ENTRY("AFNI_range_label") ;

   /*** anat statistics ***/

   if( IM3D_OPEN(im3d) ){ RELOAD_STATS(im3d->anat_now) ; }

   if( IM3D_OPEN(im3d) &&
       ISVALID_3DIM_DATASET(im3d->anat_now) &&
       ISVALID_STATISTIC(im3d->anat_now->stats) ){

      if( ISANATBUCKET(im3d->anat_now) )     /* 30 Nov 1997 */
         iv = im3d->vinfo->anat_index ;
      else
         iv = DSET_PRINCIPAL_VALUE(im3d->anat_now) ;

      if( DSET_NUM_TIMES(im3d->anat_now) > 1 )
         iv = MIN( im3d->vinfo->time_index , DSET_NUM_TIMES(im3d->anat_now) - 1 ) ;

      if( DSET_VALID_BSTAT(im3d->anat_now,iv) ){
         AV_fval_to_char( im3d->anat_now->stats->bstat[iv].min , qbuf ) ;
         sprintf( anat_minch , "%9.9s" , qbuf ) ;
         AV_fval_to_char( im3d->anat_now->stats->bstat[iv].max , qbuf ) ;
         sprintf( anat_maxch , "%9.9s" , qbuf ) ;
      } else {
STATUS("can't load anat bstat") ;
      }
   }

   /*** func statistics ***/

   if( IM3D_OPEN(im3d) ){ RELOAD_STATS(im3d->fim_now) ; }

   if( IM3D_OPEN(im3d) &&
       ISVALID_3DIM_DATASET(im3d->fim_now) &&
       ISVALID_STATISTIC(im3d->fim_now->stats) ){

      if( ISFUNCBUCKET(im3d->fim_now) )     /* 30 Nov 1997 */
         iv = im3d->vinfo->fim_index ;
      else
         iv = DSET_PRINCIPAL_VALUE(im3d->fim_now) ;

      if( DSET_NUM_TIMES(im3d->fim_now) > 1 )
         iv = MIN( im3d->vinfo->time_index , DSET_NUM_TIMES(im3d->fim_now) - 1 ) ;

      if( DSET_VALID_BSTAT(im3d->fim_now,iv) ){
         AV_fval_to_char( im3d->fim_now->stats->bstat[iv].min , qbuf ) ;
         sprintf( fim_minch , "%9.9s" , qbuf ) ;
         AV_fval_to_char( im3d->fim_now->stats->bstat[iv].max , qbuf ) ;
         sprintf( fim_maxch , "%9.9s" , qbuf ) ;
      } else {
STATUS("can't load func bstat") ;
      }

      if( FUNC_HAVE_THR(im3d->fim_now->func_type) ){
         if( ISFUNCBUCKET(im3d->fim_now) )     /* 30 Nov 1997 */
            iv = im3d->vinfo->thr_index ;
         else
            iv = FUNC_ival_thr[im3d->fim_now->func_type] ;

         if( DSET_VALID_BSTAT(im3d->fim_now,iv) ){
            AV_fval_to_char( im3d->fim_now->stats->bstat[iv].min , qbuf ) ;
            sprintf( thr_minch , "%9.9s" , qbuf ) ;
            AV_fval_to_char( im3d->fim_now->stats->bstat[iv].max , qbuf ) ;
            sprintf( thr_maxch , "%9.9s" , qbuf ) ;
         } else {
STATUS("can't load thresh bstat") ;
         }
      }
   }

   /*** make label ***/

   sprintf( buf , "Anat %s:%s\nFunc %s:%s\nThr  %s:%s" ,
            anat_minch,anat_maxch, fim_minch,fim_maxch, thr_minch,thr_maxch ) ;

STATUS(buf) ;

   xstr = XmStringCreateLtoR( buf , XmFONTLIST_DEFAULT_TAG ) ;

   RETURN(xstr) ;
}

/*-----------------------------------------------------------
   find the autorange (default value) and make a label for
   the autorange control widget
-------------------------------------------------------------*/

XmString AFNI_autorange_label( Three_D_View * im3d )
{
   XmString xstr ;
   float rrr ;
   char buf[32] , qbuf[16] ;

ENTRY("AFNI_autorange_label") ;

   if( ! ISVALID_3DIM_DATASET(im3d->fim_now) ){  /* no function */
      rrr = DEFAULT_FIM_SCALE ;
   } else {
      if( ISFUNCBUCKET(im3d->fim_now) ||
          im3d->vinfo->showfunc_type == SHOWFUNC_FIM ){  /* showing fim */

         RELOAD_STATS(im3d->fim_now) ;
         if( ISVALID_STATISTIC(im3d->fim_now->stats) ){
            float s1 , s2 ; int iv ;

            if( ISFUNCBUCKET(im3d->fim_now) )     /* 30 Nov 1997 */
               iv = im3d->vinfo->fim_index ;
            else
               iv = DSET_PRINCIPAL_VALUE(im3d->fim_now) ;

            if( DSET_VALID_BSTAT(im3d->fim_now,iv) ){
               s1  = fabs(im3d->fim_now->stats->bstat[iv].min) ,
               s2  = fabs(im3d->fim_now->stats->bstat[iv].max) ;
               rrr = (s1<s2) ? s2 : s1 ;                      /* largest fim */
            } else {
               rrr = DEFAULT_FIM_SCALE ;                      /* don't have stats */
            }
         } else {
            rrr = DEFAULT_FIM_SCALE ;                         /* don't have stats */
         }
      } else {                 /* 12 Aug 1996: showing threshold */
         MRI_IMAGE * im ;      /* so use built-in value for scaling */
         int iv , fdset_type ;
         float fac ;

         iv = FUNC_ival_thr[im3d->fim_now->func_type] ;

         im  = DSET_BRICK(im3d->fim_now,iv) ;        /* 3D image with threshold */
         fac = DSET_BRICK_FACTOR(im3d->fim_now,iv) ; /* scaling factor */

         fdset_type = im3d->fim_now->func_type ;

         if( im->kind == MRI_short && fac <= 0.0 )
            rrr = FUNC_scale_short[fdset_type] * FUNC_topval[fdset_type] ;
         else if( im->kind == MRI_byte && fac <= 0.0 )
            rrr = FUNC_scale_byte[fdset_type] * FUNC_topval[fdset_type] ;
         else
            rrr = FUNC_topval[fdset_type] ;
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
   Three_D_View * im3d = (Three_D_View *) cd ;
   Boolean new_auto ;

ENTRY("AFNI_range_bbox_CB") ;

   if( ! IM3D_VALID(im3d) ||
       w != im3d->vwid->func->range_bbox->wbut[RANGE_AUTOBUT] ) EXRETURN ;

   new_auto = (MCW_val_bbox(im3d->vwid->func->range_bbox) & RANGE_AUTOVAL) != 0 ;

   if( new_auto != im3d->vinfo->use_autorange ){  /* new button value */

      im3d->vinfo->use_autorange = new_auto ;

      im3d->vinfo->fim_range = (new_auto) ? (im3d->vinfo->fim_autorange)
                                          : (im3d->vwid->func->range_av->fval) ;

      AV_SENSITIZE( im3d->vwid->func->range_av , ! new_auto ) ;

      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;  /* redraw */
   }

   EXRETURN ;
}

/*----------------------------------------------------------------
  called when the user (that rotten fellow) changes the fim range
------------------------------------------------------------------*/

void AFNI_range_av_CB( MCW_arrowval * av , XtPointer cd )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_range_av_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   im3d->vinfo->fim_range = av->fval ;
   AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;  /* redraw */

   EXRETURN ;
}

/*----------------------------------------------------------------
   called when the user toggles the posfunc button
------------------------------------------------------------------*/

void AFNI_inten_bbox_CB( Widget w, XtPointer cd, XtPointer cb)
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   Boolean new_pos ;
   int jm ;

ENTRY("AFNI_inten_bbox_CB") ;

   if( ! IM3D_VALID(im3d) ||
       w != im3d->vwid->func->inten_bbox->wbut[PBAR_MODEBUT] ) EXRETURN ;

   new_pos = (MCW_val_bbox(im3d->vwid->func->inten_bbox) & PBAR_MODEPOS) != 0 ;

   if( new_pos != im3d->vinfo->use_posfunc ){  /* new button value */

      im3d->vinfo->use_posfunc = new_pos ;   /* record for later use */

      jm = im3d->vwid->func->inten_pbar->mode = (new_pos) ? 1 : 0 ;  /* pbar mode */

      /* re-panel the pbar befitting its new mode */

      HIDE_SCALE(im3d) ;
      alter_MCW_pbar( im3d->vwid->func->inten_pbar ,
                      im3d->vwid->func->inten_pbar->npan_save[jm] , NULL ) ;
      FIX_SCALE_SIZE(im3d) ;

      /* set the count on the pbar control arrowval to match */

      AV_assign_ival( im3d->vwid->func->inten_av ,
                      im3d->vwid->func->inten_pbar->npan_save[jm] ) ;

      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;  /* redraw */
   }

   EXRETURN ;
}

/*--------------------------------------------------------------
   Called to reset the range related functional stuff
----------------------------------------------------------------*/

void AFNI_reset_func_range( Three_D_View * im3d )
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

   EXRETURN ;
}

/*--------------------------------------------------------------------
  30 Nov 1997:  Callback for when the a bucket chooser is altered;
                will switch the viewing to a new sub-brick.
----------------------------------------------------------------------*/

void AFNI_bucket_CB( MCW_arrowval * av , XtPointer cd )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   int doit = 0 , iv , redisplay ;

ENTRY("AFNI_anat_bucket_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   /** Anat sub-brick **/

   if( av == im3d->vwid->func->anat_buck_av ){
      if( ISANATBUCKET(im3d->anat_now) ){
         iv = av->ival ;
         if( iv >= 0 && iv < DSET_NVALS(im3d->anat_now) ){
            doit = (iv != im3d->vinfo->anat_index) ;
            im3d->vinfo->anat_index = iv ;
            redisplay = REDISPLAY_ALL ;
         }
      }
   }

   /** Func sub-brick **/

   else if( av == im3d->vwid->func->fim_buck_av ){
      if( ISFUNCBUCKET(im3d->fim_now) ){
         iv = av->ival ;
         if( iv >= 0 && iv < DSET_NVALS(im3d->fim_now) ){
            doit = (iv != im3d->vinfo->fim_index) ;
            im3d->vinfo->fim_index = iv ;
            redisplay = REDISPLAY_OVERLAY ;
         }
      }
   }

   /** Thresh sub-brick **/

   else if( av == im3d->vwid->func->thr_buck_av ){
      if( ISFUNCBUCKET(im3d->fim_now) ){
         iv = av->ival ;
         if( iv >= 0 && iv < DSET_NVALS(im3d->fim_now) ){
            doit = (iv != im3d->vinfo->thr_index) ;
            im3d->vinfo->thr_index = iv ;
            redisplay = REDISPLAY_OVERLAY ;
         }
      }
   }

   /** Change the view, if required **/

   if( doit ){
      SHOW_AFNI_PAUSE ;
      AFNI_setup_viewing( im3d , False ) ;
      AFNI_set_viewpoint( im3d , -1,-1,-1 , redisplay ) ; /* redraw */
      SHOW_AFNI_READY ;
   }

   EXRETURN ;
}

char * AFNI_bucket_label_CB( MCW_arrowval * av , XtPointer cd )
{
   static char blab[32] ;
   THD_3dim_dataset * dset = (THD_3dim_dataset *) cd ;
   static char * lfmt[3] = { "#%1d %-14.14s" , "#%2d %-14.14s" , "#%3d %-14.14s"  } ;
   static char * rfmt[3] = { "%-14.14s #%1d" , "%-14.14s #%2d" , "%-14.14s #%3d"  } ;

ENTRY("AFNI_bucket_label_CB") ;

   if( ISVALID_3DIM_DATASET(dset) ){

#ifdef USE_RIGHT_BUCK_LABELS
      if( DSET_NVALS(dset) < 10 )
        sprintf(blab, rfmt[0] , DSET_BRICK_LABEL(dset,av->ival) , av->ival ) ;
      else if( DSET_NVALS(dset) < 100 )
        sprintf(blab, rfmt[1] , DSET_BRICK_LABEL(dset,av->ival) , av->ival ) ;
      else
        sprintf(blab, rfmt[2] , DSET_BRICK_LABEL(dset,av->ival) , av->ival ) ;
#else
      if( DSET_NVALS(dset) < 10 )
        sprintf(blab, lfmt[0] , av->ival , DSET_BRICK_LABEL(dset,av->ival) ) ;
      else if( DSET_NVALS(dset) < 100 )
        sprintf(blab, lfmt[1] , av->ival , DSET_BRICK_LABEL(dset,av->ival) ) ;
      else
        sprintf(blab, lfmt[2] , av->ival , DSET_BRICK_LABEL(dset,av->ival) ) ;
#endif
   }
   else
      sprintf(blab," #%d ",av->ival) ;

   RETURN(blab) ;
}

/*---------------------------------------------------------------
  Callback for all actions in the misc menu
-----------------------------------------------------------------*/

#include "newstuff.hhh"

void AFNI_misc_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_misc_CB") ;

   if( ! IM3D_OPEN(im3d) || w == NULL ) EXRETURN ;

   if( w == im3d->vwid->dmode->misc_voxind_pb ){
      im3d->vinfo->show_voxind = ! im3d->vinfo->show_voxind ;
      AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_OVERLAY ) ;
   }

#ifndef DONT_USE_HINTS
   else if( w == im3d->vwid->dmode->misc_hints_pb ){
      MCW_hint_toggle() ;
   }
#endif

   else if( w == im3d->vwid->dmode->misc_anat_info_pb ){
      char * inf = THD_dataset_info( im3d->anat_now , 0 ) ;
      if( inf != NULL ){
         if( DSET_ARRAY(im3d->anat_now,0) == NULL ){
            inf = THD_zzprintf( inf , "\n*** Not loaded into memory.\n") ;
         } else if( im3d->anat_now->dblk->malloc_type == DATABLOCK_MEM_MALLOC ){
            inf = THD_zzprintf( inf , "\n*** Loaded into memory using malloc.\n") ;
         } else if( im3d->anat_now->dblk->malloc_type == DATABLOCK_MEM_MMAP ){
            inf = THD_zzprintf( inf , "\n*** Loaded into memory using mmap.\n") ;
         }
         (void) new_MCW_textwin( im3d->vwid->imag->topper , inf , TEXT_READONLY ) ;
         free(inf) ;
      } else
         XBell( im3d->dc->display , 100 ) ;
   }

   else if( w == im3d->vwid->dmode->misc_func_info_pb ){
      char * inf = THD_dataset_info( im3d->fim_now , 0 ) ;
      if( inf != NULL ){
         if( DSET_ARRAY(im3d->fim_now,0) == NULL ){
            inf = THD_zzprintf( inf , "\n*** Not loaded into memory.\n") ;
         } else if( im3d->fim_now->dblk->malloc_type == DATABLOCK_MEM_MALLOC ){
            inf = THD_zzprintf( inf , "\n*** Loaded into memory using malloc.\n") ;
         } else if( im3d->fim_now->dblk->malloc_type == DATABLOCK_MEM_MMAP ){
            inf = THD_zzprintf( inf , "\n*** Loaded into memory using mmap.\n") ;
         }
         (void) new_MCW_textwin( im3d->vwid->imag->topper , inf , TEXT_READONLY ) ;
         free(inf) ;
      } else
         XBell( im3d->dc->display , 100 ) ;
   }

   else if( w == im3d->vwid->dmode->misc_newstuff_pb ){
      char * inf ;
      int lsum=8 , ii ;

      for( ii=0 ; ii < NUM_newstuff ; ii++ )
         lsum += strlen( newstuff[ii] ) ;

      inf = (char *) malloc(sizeof(char)*lsum) ; inf[0] = '\0' ;
      for( ii=0 ; ii < NUM_newstuff ; ii++ )
         strcat( inf , newstuff[ii] ) ;

      (void) new_MCW_textwin( im3d->vwid->imag->topper ,
                              inf , TEXT_READONLY ) ;

      free(inf) ;
   }

   else if( w == im3d->vwid->dmode->misc_purge_pb ){
      AFNI_purge_dsets( 1 ) ;
   }

#ifdef USE_TRACING
   else if( w == im3d->vwid->dmode->misc_tracing_pb ){
      DBG_trace = (DBG_trace + 1) % 3 ;  /* Aug 23 1998: cycle between 0,1,2 */
   }
#endif

#ifdef USING_MCW_MALLOC   /* 07 Mar 1999 */
   else if( MCW_MALLOC_enabled && w == im3d->vwid->dmode->misc_showmalloc_pb ){
      MCHECK ;
   }

   else if( MCW_MALLOC_enabled && w == im3d->vwid->dmode->misc_dumpmalloc_pb ){
      mcw_malloc_dump() ;
   }
#endif

   /****----- Get Outta Here -----****/

   EXRETURN ;
}

#ifdef USE_HIDDEN

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  May 1995: Hidden popup menu stuff
:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/

#ifdef WANT_RWCOX_IMAGE
void RWCOX_popper(void) ;
#endif

#ifdef USE_SKIT
void SKIT_popper( Three_D_View * ) ;
#endif

/*---------------------------------------------------------------
  Callback for all actions in the hidden popup
-----------------------------------------------------------------*/

void AFNI_hidden_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_hidden_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

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

   if( w == im3d->vwid->prog->hidden_writepts_ijk_pb ||
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

   if( w == im3d->vwid->prog->hidden_colorpts_pb ){

      POPDOWN_string_chooser ;

      im3d->vwid->prog->hidden_code = PTS_SET_COLOR ;

      MCW_choose_ovcolor( im3d->vwid->picture ,
                          im3d->dc ,
                          im3d->vinfo->pts_color ,
                          AFNI_hidden_pts_CB , cd  ) ;
   }

   /****----- Get Outta Here -----****/

   EXRETURN ;
}

/*-----------------------------------------------------------------
  Event handler to find #3 button press for hidden popup
-------------------------------------------------------------------*/

void AFNI_hidden_EV( Widget w , XtPointer cd ,
                    XEvent * ev , Boolean * continue_to_dispatch )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_hidden_EV") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   /*** handle events ***/

   switch( ev->type ){

      /*----- take button press -----*/

      case ButtonPress:{
         XButtonEvent * event = (XButtonEvent *) ev ;

         if( event->button == Button3 ){
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

   }

   EXRETURN ;
}

/*-------------------------------------------------------------------*/

void AFNI_hidden_pts_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;
   THD_3dim_dataset * dset_now ;
   THD_fvec3 xyz_vec ;
   THD_ivec3 ijk_vec ;
   Boolean ijk_option , pause_it ;
   FILE * fil ;
   THD_vector_list * sv ;
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

/*====================================================================================*/
#if defined(WANT_RWCOX_IMAGE) && defined(ALLOW_PLUGINS)

static byte mapcox[] =
  {  30,  59,  76,  93, 107, 118, 130, 141, 146, 152, 161, 166, 171,
    174, 177, 182, 192, 206, 221, 231, 237, 242, 246, 250, 252, 253 } ;

#define NX_COX 220
#define NY_COX 286

static char * bccc[] = {
   "9Z9Z9Z9Z9Z9Z6Z",
   "9Z9Z9Z9ZZYY4Z9YYX6Y",
   "ZYXX4YXYYX8Y9Z5ZY9Z9Z3Y4Z",
   "ZZ6Y9Z9ZZY5ZY9Z9Z8Z",
   "9Z9Z9Z9Z9Z9Z6Z",
   "9Z6ZY8Z8YX9YXX4YX9YYY",
   "9ZZZYZZY9Z9Z3YZZYZ6Y9Z5Z",
   "9Z9Z9Z9Z9Z9Z6Z",
   "9Z9ZZZY9Z9Z9Z9Z3Z",
   "ZZ6Y5XYYZZ6YX9Y9Y9Y5YZZYY",
   "9Y8Y3Z6Y3ZYY9Z9Z9ZZZ",
   "9Z9Z9Z9Z9Z6ZY5Z3Y",
   "ZYZZYZZY9Z9Z9Z3Z5Y3X3Y9XXX",
   "6X9Y9Y9Y9Y9Y6YXXZ",
   "YZ4YZZ3Y9Z9Z9Z9Z9Z4Z",
   "9Z9Z9Z9ZZ3YZ8YZZY6ZYY",
   "6ZY9ZZZ4YXXW3X3WXXW6XW5XYYXXYYXYYXX3Y",
   "9Y9Y9Y9YYYXZ6YZZYY7ZYZZ",
   "9ZY9Z9Z9Z9Z9Z5Z",
   "9Z7Z9Y9YYZ9YYY8Z5Y",
   "YXX4W5V4WXX4WXXYXYYXXYYXY3X9Y9Y5Y",
   "9Y9Y9Y3YZZ3YZZY9ZZZYY9Z",
   "9Z9Z9Z9Z8ZYZZY3Z9Y",
   "9Y3Y3XY5XY4XYYZZY6X4WVVUU3V3U4TU3VW",
   "VW5XY3XY5X3YXYYXYYXYY4X9Y7YXXYX7Y",
   "9Y3Y7ZY5ZYZYYZYY5ZY9Z9Z4Z",
   "9Z9Z6ZYZYY3Z9Y9Y3Y6XWW",
   "WXY4XY3X8WUU5T3U5TST4UVWW4XY5X5YXX",
   "X9YX4YX9Y4YXYZX6YZ7YZZ3YZYZZY3Z",
   "8ZYZYY9Z9Z9Z9Z9Z3Z",
   "ZYZZ9YYY3XYYXXY5XY9XWW4XWWXX4W4V3TS",
   "TT4S5T3STS3TUV3W5XW4XYYXY4X9Y4Y3XYY",
   "6YX5YXYXX3YZ7Y3Z8Y4ZY5Z4YZZYY4Z",
   "9Z9Z9Z9Z9Z6YXYY6X",
   "XWW9X5X3WX5WXWVVUTT4SRRQRR3S8R6ST",
   "UVV7W9XXX3Y9X9Y9Y3X6Y",
   "9Y6Y9ZZY3Z4Y9Z9Z9Z",
   "9Z3ZY4ZYY6Z5Y6XW3X9W3WXXWXWWXW",
   "7WVUU5SRR4QPQRSR6QRSTT3S4TUVV5W6XYXX",
   "X3Y9X3XYXX5YX9Y9Y9Y5YZYZ",
   "YYZZYYZYZ4Y3Z3Y9Z9Z4ZYYZZ5YZYYZZYZ3Y",
   "3YZ6Y4X8WVVWW3VWV9W3WVWVUTSS6RQQPP",
   "PONN4P3QPPRST7STTUV6W5XYY6XW9X4X",
   "XXYXX9YX9Y5YZZ9YYYZ9YYZ6Y",
   "9Z9Z6ZYZZ6Y3Z9YXXY3XWX7W",
   "VW6VWVV4U6V4T5SRRQOMMLKKJ5KLMMNPMOPPQR4S",
   "3STTUVUUV3WXWXXWW5X4WXX6YXYY6XYYX9Y3Y",
   "9Y5YZ9Y3Z9Y4Y9Z9ZZZ",
   "4ZYZZ9Y8Y7X3W5VU4V4UVVWV4U4T",
   "TT4S4RQPOONMLKK3JKKJJ6KNOPP3RSSRSTUUV3UVWX3WXWXX",
   "XWWXXW6XYYXYX5YXX9Y9Y3X9Y3YZYY",
   "4Y3Z6Y3Z4Y9Z9Z9Z9Y4Y",
   "9X4WVUVUUTTUTTU6TU3TUUTT5S5RQQPPOMLLKJJI",
   "IIHJJ5IJKLNNOPQRRSRSSUTUUTUVWX9W6W9X3X",
   "XYXX9Y9Y9Y9Y9Y9YYY",
   "9Z9Z9Z9Y6Y5X4WV6UVV",
   "WW3V3U4T6SR4QPNMLNNM3LKJI3H5GF6GHHJKLLMNQ",
   "QRRSSUUTTUU4VWW3V7WXY6X4Y5XYXX9Y4Y",
   "9Y9Y9Y9Y4Y9Z9ZZZ",
   "7Z9Y3XYY5X5W6U3V5U4TSTS3R3QPP",
   "NMLK9JIHH6G7FGGHHGJJKLLOPQRRSTVUTTUU4VW3V",
   "7W8XYX5YXX9Y9Y3YZ9Y5Y",
   "9Y9YYY9Z9Z8Z9YY4X",
   "6X3WVV4U3V3U3T3S6R3QPPNMMKKJI9H3H3G",
   "HG9F3GHHJJKLMO3RSTUTSTUUTUVVW3V7W3XWXXYY4X",
   "XYXX4YXYYXXYXX9Y9Y9Y9Y4YXXYY",
   "9Z6ZY7Z9Y3XYYXY6X3WV7UTTSS",
   "SS7RPPQONMMLKJJIJIH4G6F3E7FEEDED4EF4GHI",
   "JKOQQS3TSRSTSUT5UVVW5V4W9X4XY3XYYXYYXXYXY",
   "9YX9Y3YXX9Y3YXXYY9Z6ZY4Z",
   "3Z7Y3XYXY9XWWV6U3TSS4RQQR3QMOPL3KJJIH",
   "3H5G3FEF4EDEE3F3EDE6DEEFFGGIIKMPRR3SR4S5T",
   "TTUV4U3V4W9XXXYXY9XX9Y9Y3Y",
   "9Y8YXYY9Z9Z9Y3Y3XYY5X",
   "XWXX3WV3U5T3S4R4QPPNMMLLKJJII3H6G5F3EF3E",
   "EEFFEFE4DC3DE3FGHHIKMQ3RS4R3S6TUUTU4V7WXX",
   "XXW5XY7XYXX9Y9YX9Y9Y4Y",
   "9Z3ZYZZYYZ9Y3Y9X3X3WVVUU5TSS4R",
   "6QPNNLKK3JIHH5G4F3E3F6EDDEDEF3EFFB5CDDEF",
   "FG3HJKLMOQ3PQ7R4S3TU3V9WWW9XXYXYYXX",
   "4YX8YXX4YXXYY9X4XY3X9ZZZ3YZ5Y",
   "9YXYY9XX3W4U3T3SRRS5RQQPMLK6JHGGFF",
   "3F9E8EDEDD5EFDCBBCC3DEFF3GHI3JKLKMOPQ3P",
   "3Q3RSTSTTUUVVWVV9WW6XY5XYY9X3X3YXYY",
   "3Y9X4XY3X9ZZ4YZ9Y3Y3XY6XWWX",
   "XXWWV6T6S6RQPNKJJHHI3HGFF9EEDD4EDE4D",
   "6D4EFDCAB3CDEE4FHHIIHI3JKM4LMMNPQQRR4S3T5V",
   "8W6XY9X9XX3YX6Y9X7X",
   "6ZY5Z9Y8YXY7XWXXWWV6T4S3RQPP",
   "MLJKLKIHH7G5F9EE6D6C9DED5CD",
   "DE3FEF4GHHIHIJ3H5JKMNOPPQRRSS4TU3V7W9X",
   "9X9XXXY3XY9X6X9Z3Z8Y",
   "9Y8X5WVU5TSS3R3QNMMKLKKJJHGGHH6G3F",
   "3EF3EFFEEDC4D9CDDCCDDEEDC3BCDE3FEF3GHGG5HGG",
   "HII3JKKLMLOQ4R3S3TU3VWWX5W9X9X7X",
   "Y9X9XW9Z4Z9YYY3XYY8X3W",
   "WWU5TSSR4QPO7KJHGG3H4GFF9E4E3D7C",
   "4BCCDD4CD3EC3BCCEFFEEF3GFFGG4HGFG3HIJJIJKJLPPQQ3R",
   "SSTT3UV9W9X9X9XW9XW5X",
   "9ZZZ9Y4Y3XY8X4WVV4T3SRRQQPNNM3K",
   "3JIHHIHG3E7F5EDD7C3DC5BAABBCC5BCDDEFEC",
   "BC3D9F3GHIHF5GHHG3H3JKKLMPPRR3STT3U3V6W",
   "4W4X4WXW9XXX4WXWW5X3W9Z3ZYYZ5Y",
   "4Y3XYY6X5WVU4T4SRQPPONMLKK3JIHHG8F4ED",
   "6D7CBCCDDC7B3CBBCCBBCDEEDCCDEDEE8FGG3H",
   "GGFGGF6GHH4IKLMPRR3STT3U4V9W6XW6XWX",
   "XWXX9W3WX3W9Z3ZXYY3X8YXYY8XWWV",
   "U5TSS3R3QPOONOM3JH3G8F3E7D5CBCCB3CDDC",
   "BC4BCBCCBBCBBC3DFEDCDD3E4FEE3F3GHH6GHGFFGGHHIHJK",
   "KKOQ3R3TUU5V8W6XWXWXXW7XWWXWX9WWW",
   "9Z3Z9Y4YXXY6XWXWWVU4TSS6RQQMKKNLK",
   "HIJFFH3FGFFEEDC4DCC6D4CDCDDC3DC6DCDCDCC4BCCD",
   "DEEDBDD5EFF3E4F3GF4GHGGF6G3HIIKLLQRSSTT6VW",
   "9W6WXWWXX9W9WWW9ZZZ9Y",
   "5Y7X5WVV6TS5RQPO3KLJI3HFGHG5FEDDE4D",
   "9D9D7DE4DCD4CB4CD3ECCD9EEFF",
   "6GE3FGGFF6GHGHHIHIJORSSTTUU4V9W3WXWWXWWXW",
   "4WX9W6W9Z3Z9Y5Y7X3W3VT",
   "4TSSRRQRQPOMLLKJHGGHHFFG4F5EDDC9D3DEDD7E",
   "EED7EDED9CDEEDC3D5EFFEEF6G3E7FGFFG",
   "6GHIHLRSS4U5V9W9W9W9WWW",
   "9Z3Z9YYYXXYXYY4X3WVVU4TSS3RQPPOMKJJHHGG",
   "GH3GF6E6DE5D6E9F9F5FEDEE3D",
   "CBCCDEE7D8E7F3EFFE7F5GHGHJMQSSTTUVV",
   "6V9W9W9W7W9Z3Z8Y",
   "Y6XY5X3WVVU3TSSR4QPP3KJIHH3GJGFF7E3D5E",
   "EEDEF3E4F8GFGG3F4GFFGF5E3D3CDE7DEEF3E",
   "3E6F3EFF3EFEE4F5GHIJLRSSTTUU5V9W6W",
   "9W9WWW9Z5Z8YXXYXXY5X3WVUTT",
   "4SRRQQPPNKJJI3H3GIGF9EEF9E5F5GHH3GH",
   "7G3H5G3F3ED3CDDE9D5EDEFFEFF8EFFE",
   "FFE3F4GHJJPRRSTU6V9W9W9W8W",
   "9Z5Z9YY6XWX4WUTT4SRRQQPPNLKJJII4G",
   "HIED6EF3E5FEFFGF4G5H3JII3JIHIJHIIJJI6H3G",
   "FFEDD3BDEDCCDDFDCDDEDD9EEE5D7E4F3GJJMRSTU",
   "9VV9W9W9W3W9ZZZYZZ6Y",
   "4Y6X6WUTT4SRRQPOOLLKJJIH5GH4EFE3FE8F",
   "6G3HIHIIJK4JKKJHJKIJJKKJ3IJIIHH3GFEEDCCBCDDCCDE4C",
   "7DEEDE9D8E3FGGHIHPRS3UVUU5V9WW",
   "9W9WWW6ZYZZ3YZZ9YYY6X4WVUTTS",
   "SS3RP3OMLKJIIHH4GHEE3FEEFFE4FGF3G3HGH3IJIIJ6KL",
   "LK4JKJKLK6JIIHHGGFFDDCBBCDEDCDCD4C9D9D",
   "D8EFFGGHHILPRTUU8V9W9W5W3V4W",
   "9Z8YXYY9XX3WVVUTTS3R3QPPOMLK3JIIHGFF",
   "GG7FGFF6GHHI3JII4JLKKLMM4LMMLM4LK4L6KJ",
   "JIHHGGEED4CDEDD3CBBCCDD3CDCDC7DC5D5EFFGHHGHKR",
   "STUU5V9W9W9W4W6ZYZZ8YXYX",
   "9X4WVVUTTS3R3Q3ONMKLJIHHGFFEGH3FG3F3GHGG3HII",
   "JKK4J3K3LMNNMLLM3NON3LK3LMLL3KLKKJIHHGGFEDDCCD3EDCC",
   "3B9C3C3D4C4D6EF4GFHQRSTTUU4V9WW",
   "9W3WVWWV4W8Z9Y3Y9XXWW3VUTSSR",
   "RR3QOPOOKLKJJHGG3FHGF6G6H3I3J7KLLMN4OMLLO",
   "OP5MLK3LNMM5LKKJIIHGGFEDDBC6DCCB9CCCBCDDCC",
   "CC3DC4D3EFFGFFGMQRSTT3U4V9W9W9WW",
   "7Z8Y9X3X6WU3TSSRRQ4PNMNMLIGHGG4F",
   "4HGG3HI5JKJJ3KLLK3L3MOOP5OPPOONOOMNN4MNNMM4L",
   "LLKKJ3IHGGEED5C3D6CBCBBCBBAB6CDDC7DE4FGG",
   "JNRSSUTUU9VVV3W5VW3VWW6V6Z9Y5X",
   "4X7WVV3T3SRRQ3PN3MLJI3GFFHGFJJI3H5IJ8K",
   "LMMLLMNN5O7POO3PONO4NOONN3M4LKK3JIHGFFEDCCBBC",
   "3DB3C3B4C5BCCDCC3DCDC3DEE4FGHKRRS3TUU9VV",
   "3W9VVW6V6Z9YXXYY5X5WVVU4T3SR",
   "R3QPONMMLIHHGF4GHKKIHHI6J3KLLK3L6MN5O3POPPO",
   "NN3POO3P7ON5MLLKJJ3IGGFDD3CBBCD4C4BCBCC5B",
   "B3CDCD3CDCCDE4FGGHQRRSTTUU9V6VWVVW9VVV",
   "6Z9YXX3Y5X4WV4UTTSS4RQPOPOK3JH3G4HJ",
   "KJJ9K6LNNONN6ONMNO8PO9POO4P3N",
   "MMNML3K3JIHGFEDDCC6BCCBABAEBBCBC3BCB9C3CDEFGF",
   "FFJMPRSSTTUU3VWW6VW7VW8V6Z9Y4YX",
   "5X3WVVUUVUTTSS4RQQPPLJJHH3GHKIHJJ9K4LMMLMMNM",
   "NNOONOPOONMN6PO9P4P8ONNONMLL3KJJIHGFEDDC",
   "6B3CBBA3BABC6B9C3CEE4FGILQRSTT3UVVWW3V",
   "9V9VVV6Z9Y4Y7X3W4VUTT4R",
   "R4QKJKJIH3GKIHJKLML3KLKKL4M4N3ONOOPO4PONO8P",
   "9P4PN3OPP5ONMLL4KJIGGFEDD7BCC3BA4BCBB",
   "5B5CB5C3E4FGJPQRSTTU9V8VW9VV",
   "9Z3YX8Y6XWWVVUUTT3S3RQQPMLMLII3GHIJJLML",
   "MLLNMNMMNLMNM9ONO6PO9P9P9PPO",
   "OOPOONMMLLMLKLHGGFEEDD3C8B3A9BB4C3B4CDEE",
   "3FGGKNQRSTTUU9V5VW9VVV6ZYZZ3YX7Y",
   "Y6XWWV3UT4S3RQOMKMNKIHGGH3JKMN3MLMMNNMNMNN3OP4O",
   "PPOO9P3PQQ9PPO9PP3OPOPO4MLLMMJHGGFE",
   "3D3C9BAA9BBBCC3BD3CDDEE3FGHKPRRSTTU6V",
   "4VW9V6V6ZYYZ5YX6Y3XWX3WVUTT4SRRQ",
   "QP3MNNKHGGHIJKKMNMMNMLMNOOMM7ON3O9PPQ5P3QP",
   "Q4PQ4PQ9P6PONNM3LMKJHGGFEDD5C7BAA3B",
   "9BB8CD3EFFGHJNQRSTTU9VVW9V6V",
   "6Z8YX6Y3X4WVU3T3SRRQQP4NOOMKJKKLLNP3OM",
   "ONOM4OMPP4OPOOPOO9P6PQQP6Q6PQQ7P",
   "4PO3P6MLKKJIGEE4D5C9BA9B6BCBCD",
   "EEFFGHIJOQRSTTUU9V9V6V8Z9Y3Y",
   "4XWWVVU4T3SRRQQPN4OPNKKLONN4PONPOP6O3P3O5P",
   "7PQ9PPP6Q9P8POON3PN6MLLKIH",
   "FFE3D5C9BBBA9BBC5BDDEEFGJIJLPRSSTUU4V",
   "9V9VVV9Z9YYY3XWWVVUU4TSS3RQP",
   "ONOPP3NKJKNNO4PONPO9PPNOO4POPPO4PQQ4POPPQQPP",
   "PP3Q9P4P4ONNLNOON3MNNMLLKJHGFF5DCC4B4ABB",
   "AA9BA5BCBCDDEFFJIJKNQSST3U9V9V5V",
   "9Z6YXXYY5XWV3U3TSSRR4QPOPPNLMMLMOP5Q3P",
   "3QPPQPPQ3POPPO9PPQ9PO3MN5PO9P4P",
   "POMNONNO3NMOLMLLKJIHHGFFE3DCCBAB9A3ABA9B3B",
   "CCD3EGIKMPRS3T9UVUU9V3V9Z6YXXYYX",
   "4XWVUU3T3SRR4QNOOMLMLKMP7QPQP6Q6POPPO4P",
   "9PPPOO3PNLLNO4P3O9P4POMN4O3NOONMNMKKJ",
   "IHHGGFE3DCC3B9A6ABB4A6BD3EGHJKNRS4T3U",
   "8UVU9VV6Z9Y4Y5XWVUU3TSS3R3QO",
   "NPKJKNLKPQ4R4QPP6Q6POPO9PQ5P3O4POMO",
   "5POOPPO9PP5O3PONM3OMNL3KJIIHGGEEDDCC3B5A",
   "9A9ABBABCDEEFGIJLRSS3T9UUUVU9VV",
   "6Z9YXXYY4XWWVU4TSRRQQ3POMJIMONMPRRQ4R5Q",
   "6Q9P9P3PO9P6PON9P3POPP",
   "4PO3PONOONN3MLKL3KIHHGFFEDC6B9A9A3AB",
   "BABBCDEEGGIPRRSSTT8U9V5V7Z9Y3YX",
   "XX3WVUUTTSS3RQPPOMKHKPPOOQ7R9QPPQ9P4P",
   "5P3O9P9P9PO8PO3PONPONM3NMLL",
   "3KJIHGGFEDD6B9A8AE3ABBAABCDDEFGHPRRSSTTUU",
   "6U9V5V4ZY3Z9YYY3X3WVVUTTS4RQQPOK",
   "JHNQQOPQ8RQ3R3Q3PQ7PQ3POO5PNN3O9PP",
   "9P4POON4POPP3OPPON3ON3MOMKLK3JIHGGFEDCC5BA",
   "9A9A5ABBCDEFFHMQR3S6T3U9V5V",
   "9Z8Y6X3WVTTSSRR3QPNLIIKPPOQRRS9RRR",
   "QQ8POO4PO8P3O9P3POO4PO6PN3O3P",
   "PPONM3NONNPOOMNMN4L4KIHHFFEDCC8B9A8A",
   "5ACCDEEFJNQ3S6T5U7VU4V8Z7YXY3X",
   "XX4WVTSS3R3QPMLHJNPPMRR5S7R4Q3PO7PO5P",
   "PPOONOPOO3PO4POM3O3POO6POPPO6POONON6O3N3M",
   "LLKLKKIIHGFEDDCBC7BAAB9A9ABCCEEFHKPRRSSTT",
   "6T4UVU3VU4V6Z9Y6X5WVTSS3RQQPNMKI",
   "KPQPORR6S6R4Q9POPOPOO4POONOPOOPPONNO3POO",
   "NN4O7PNOPPO7P7O6N4MLLKKJJIHGFDD4CBB",
   "4B9AB9AAABBC3EGIMQRR3S6T6U3V3UVV",
   "4Z9YYYW5X5WVTSRRQQPPNMKKOPQPPRSSTT4S7R",
   "4QPQ5POOMO4PONOPPOPPOOPPONMNOPOONNO6P4O3PO3PN",
   "O9PPNO3POMOO3M3LKIIHGFFEDDCC6B9A7A",
   "5ABBCDDEFGINQQRRSS5T6UV6UV4Z8YXXYWW3X",
   "6WVTSRRQQN4MLO3PQRSSTT5S6R6Q4POONMN4PO",
   "NNONN7PON4O3N8POO4PO3PONO6P3ONNOPOONOO",
   "5MLKJJHHGFEDDCBCBC3B9A9A3ABBCDDEFGHLOQ3R",
   "SS4T6UV4UVUU4Z9YYYWXY3X4WVUSSR3QMLMLONP",
   "OPQRRSS4T3S6RQQP5QPP5O3POONOOMMNO3P3ONN4ON",
   "NN5P4O9PO6PONM5ONO3NMMNNMLKKJHIGFEDD4C",
   "4BABBAAB9A3BAA3BCDDEFGGLNP3R3S5T4UVUVVUVVU",
   "ZZY3Z7YXY6X4WVUSSRRQPKMLL4PLRS7T3S4RQQ",
   "5QRQ3P7ON5ONNOOPPOPOMNOOPONONO4PNOO7PO3P",
   "3PQPP5O5P4OMMNMMLLKJIHGFFE3DCDC3BABAABAB4AB4A",
   "3BAABCCDDEEFFJLNQ3R5STT4UV3U3VUZZY3Z7YXY5X",
   "X4WVUSSRRQP4L3PMOSSTU5T3S5R5QR3Q4P3N5O",
   "O3NO4P3ONMN4OPON4P7O3POPO6POOP5O4POPO",
   "3NMM3LKIIHFFEEDDCDC5BAAB9A3A3BABBCCDDEEFHJLQRR",
   "6S3T9UUUZZYZZ8YXY7X3WVUSSRQPNMLJMPOO",
   "LQSTTU5T3S5R9Q4PONN3ONNOOMM3OPP3O3NOPPOO",
   "ONPPOP3ON5O4PO5PO4P3O5PO4NMML3KIJHGFFEDDCC",
   "C6BAB8ABB3ABBA3BCDDEEFGHJPQ3R3S4T9UUU",
   "5Z7YXXY6X3WVUUSRQPPNONL4PQRT8U3S5RQ",
   "QR7QP3ONONN3ONNMNN5OPOO3NONNON3ONONOPOON7OQ",
   "5PO3POOPO6PONMMLMLLKKJIHGGF3DCDC3BAABAABABB3ABABB",
   "3A5BCCD3EFGHLMO3R3S4T9UV5Z7YXXY5X",
   "X3WVUTRRQPONMNNMNQPQRT4UVUUTSS5R3QRR4Q3POONN4ONN",
   "NNMM3N4OPPMNNO4NOPPOONNOO4P4NM9PPONO5PONN",
   "NNOMMLLKKJJHGFEDDCDD7BAABAAB3A3BAB4A4BCC3EFGGJLP",
   "3R3S3T9UUU5Z7YXXY6X3WVUTRRQPONMONJMQP",
   "RTUU5VUTSS6R3QR5Q4P4O4N4MNMLNOONOOMMNO3N",
   "NNOPOO3NO3P4ON5O7POO4PO6NMNML3KJHGFE3DCC",
   "9BBAA5BAB3ABBA5BCDEEFFGHJLQRR5S3T9U",
   "3ZYYZ7Y8X3WUTSRQQONMMOLIHNRTTU4VWVUUSS6RQ",
   "3R4QPPOPPON3M5NMM5NONNOO5NMLMNOO5MONNOONOOPP",
   "OOPPN7POOPOPMNNMMNO4MLKKIGFFEEDCDCC3BAAB4ABABB4A",
   "B5A5BDDE4FGHNQR3S6T5UVUU3Z9YYY6X",
   "X3WTSSRQQMLLNLJFHOSTU4VWWVVUSS6RQQ3RQQ5POO3MLMON",
   "N4M3NOONOP3O3N3M3N4MNPONPON6OP3OP5ONOPOONMM",
   "NMMN4MLKKJGFFE4DC4BAAB3ADABAA3B7A5BDD4EFFG",
   "LPQ3S6T8U3Z9YY8X3WTSRRQPLLMMJHEIPT",
   "UU4VWWVVUTS5R5QRQQ5PON4MNNMML3M3NOON5O4N",
   "MNMMNNMMNOOP3ONOPPNOPPON6ONMNON3ONONNON3MLJJI3FE4D",
   "D4BAABBABABBA4B3ABAA6BCDD4EFGJMQ3S6T5UVUU",
   "ZZYZZ8Y7X3WVTSRQPOMNNJGDFLQTUVV5WVUUSS4R3Q",
   "6Q6PNNON3M3LMNLMNNOO3N4O3N4O5NOOPPONMN3O",
   "3PONOPPN3ONN4ONM3O3M3K3J3GEE3DCC7BA3BAA4BA",
   "4ABBA4BCCD4EFGHILRSS7T7UZZY3Z9Y5X",
   "3WVSRRQPPNNOJFDHNRTU7WVUT4SRR9Q4POONMNONNML",
   "LLMMLMN3ONMM3NO3N5O5NOOP4ONM3P4OPPMNNPOO3NOON",
   "OOP3MLKKJIIGGFEE3DCC9B8B5ABAA4BCCD4EFF",
   "HHJQRSS7T6UZZY3Z6Y8XWWVUSRRQPPNMMHFEKPRU",
   "V7WVUT3S3R5QP3Q5POONOPON4LMMLMN3OMLLNMMNMMN",
   "NO6NM5NOPOML3P3O3PONOPOONMMOPONNOMLMLKKJJHGGFEE3D",
   "CC7BABBA6B3ABABAA4BCCDD3EFFGGHPRRSS7T5U",
   "ZZYZ9YY6XWWUTRRQ3PMMKFDEKQS7W3VUTTSS5RQ",
   "QPQQ5P7ONN3MPNMMNMMNMNNMMNNMMNM4N4MNOON8O",
   "9PP7OPOO5N3ML3J3GFEEDD4C3BABA3BA4BAB",
   "9BABCC3D4EFGGLPR5S4T5UZYY4Z7Y5XW",
   "WVTSR3QPPNLJFEEKQT3W3XW3VU3TS3RQRQQPQPQPQPPN3O4MNMM",
   "NN4M3NMNMM3NMMNMNNMNNMNNMN5O4NOO5POPP7OPPOO",
   "NN6MLKKJ3HGFFDD3C4BABBCBBA4BABA3BA6B3C3D3E",
   "FFGJOQ4S5T5UZZY4Z6Y5X3WUTSR3QPOMLKFEELRTW",
   "3WXXWVVUU3TSSRR5QPPQPQPOMNOMLMNMNMMNMM5NMMNML3NONNO",
   "ONMNOOPONNOOPOO4N3O8PO3P3OPOO3N4MLLKKJ3HGGFDD",
   "D3C5BC4BA3B4A3BA4B4C3D3EFFGILPR4S4T5U",
   "ZZYZ9YY4X3WUTS3QPO3MJFFGQTV5XWWVUU4TSS3RQQ",
   "QQOPPQ3PNOO3NMNONMM3NPONMMNMNNMNM3OMM4NMNOO4N4O3N",
   "NOP3OPOPOO6PO4NMNPOMMLKKIIHHGFEE3D3C4BA3BA5BA",
   "AA4BABB5C3DEDE3FGHK3R3S3T5UZYYZ9Y5XWW",
   "WTTS3QPNMNMJFGGRUW5XWWV3U3TSSRR4Q4PQPPOOPONMM3OMM",
   "NNMMON5MNNMNMO6NOOMNONMMNMM4O4NONOO3PONNOO3PNOO",
   "NMMOPOMMLKKJIHHGFEE3DCC5BABBAA5BAA4B3AB5C5DE",
   "E3FGJQRR3S3T5UZZY3Z8Y4X3WTSR3QPNMNMJFFGSVWX",
   "4XWV4U3TSSRR4QPO4P6ONN3OMMNMMNN7MNMNMMNON",
   "LMN3ON6ML4ONN3OMNOPPOONMOPOPP3ONNMNN3MLLKKJIHGGFED",
   "D3CBBCC4BA4BABA4BC3AB5C5DEE3FGIQRR3S3T5U",
   "5Z9Y4X3WTSRRQQNMMNLJFGITW6XWWVVU3T3SRR3Q",
   "QPO7PNMONONNMN5MNONL3MN5MNNOO3MNLL3MO3N4ONN",
   "N3O3POP5OPONO3NP3NMM4LKJJIHGFEEDCD4C7BABBAAB",
   "BABABABABDCBCC6D3EFFGOQRRSSTT6U6Z7Y5XWW",
   "WSRRQQPN3MLJGFJTWXYY3XWWVVUUTT3SRR4QPOPPO4P3OMM3NMM",
   "5MNNLLM3N3MNNMNNMLLMN4M3OPOON3ONONNOOP3OPOPPO3NOO",
   "M3N3MLLMLLJIHHGGFE6DC7BA6BA6BACCBC7D",
   "DDEEFGOQRRSS3T5U6Z6Y6XWWVSRR3QONMMKJGGLUWXY",
   "Y3XWWV4UT3SRR4Q9PPPOMLMNNMMNMNMLMN3MNM3NMNMMN",
   "MMNN3MLMM3NOPNNOPO4N7O4PONONOLM4NM5LJII3GFE",
   "4D4C6BABABBA8BABCBCC8DEEFFNQRRSS3T5U",
   "5Z7YXY4XWWVSR3QPNNOLKJGHPW7XWWVTUUTT3S3RQQ",
   "QQ9PPONN3O3MNNML3M4NMNNONNMM6NMMNN5ON3OM",
   "MMNOP3OMNOO4PONNOONONOON3ML3KJHHGFF4DCDD9B3BA",
   "5BCBBABCBBCDDC3DEED3EPQRRSS4T4U6Z8Y4XWW",
   "USRQQPPNOPLKJGIQW3YXX4WVUTUTT3S3R5QPPOPPOOPPOMM3NMM",
   "5M3LMNMML3MN3MNM3ONN4O5NPPO3MNNOPOOMNOO4P4O",
   "O3NON3ML4KJIHGFE4DCDD3C9B4BA3BAAB4CD3CDD",
   "EED3ENQ3RS4T4U6Z8Y4XWWUSRQPPONOPLKJGKRWXYY",
   "XX3WVV3UTT3S3R4QPPONPPOOPPONMNNMMNN4MLKLL4M6NM",
   "4NOON3O5NMPPONOONMM3OMNNO4PON3OMNNOM3L5KJIHGFE",
   "6DCBBC5BC7BA3BAAB4CDCBCC6DEMQRRSS4T4U",
   "3ZYYZ8Y5XWUSRQQPO3PLKJHLRX3YXXWXWVV3UTT4S4R",
   "QOP3OPPO3PONMONNMMNNMLMLML4MLNNOONMNOPNM4NMNNMMOONOMNN",
   "NMNONOONOOP3OPPOPOONMNMNMLK3LKJKJHHGFF4DCDCBBCBBA4BABC",
   "BB3A3BAAB5CBBC6DEJPQRSS3T5U3ZYZZ9YXYXWW",
   "USR3QPPQPMKJIOSXYZXXWXWWVV3UTT4S4RQ4PN3POPP3NOONMM",
   "3MLMMN5MLMNO3NOOPNONNONNONMNPOOPNONNMNOMOON3P3OPP4O",
   "NMMNNMLLK3LKKJ3HFFE3D3CBBCC6BABCBBABA3BAABDCCDCCBCC",
   "5DEHMQRSS3T5UY5Z9Y3XWWVSRR6QMLKJPSXZZX",
   "XWXWWVVUU3T4S4R3QPOM5P3ONOONN4MLNONN3MNM3NOOPN",
   "M7OMONNMONOO5NMONP4OPOON3OMOOMNMMNMLLK4LKJ3HGF",
   "EC4DCBBCC6BABCBBABA3BAAB3CD5C6DGKPRRS5T3U",
   "3ZYZZ8Y5XWWTSRR5QPOMMQSXZZ5XWVVUU3T4S4R",
   "3Q6PQPONON3O6MNMN4MNNM3NMNNMNONN4OP4N3O3N",
   "ONMLNOOPOPPOOPP3OM3N3ML3MKLKLLKIIHGGF3DCDCB3C7BCC",
   "BBAA4B3ADCCD5C6DFIOR3S5TUUZZYYZZ7Y6XW",
   "WUSS5RQPOMORTYZZXXWXXW3VUUTT5SRR5Q4POPPON3ONNLL",
   "L4MNO6MLNO3NMM4NOONOON4OPOOPOOMOMOP5O4P3ONN",
   "OON6MKL3KJJIHGGFE3DCCB3C5BABCCBBAA4B3ADCCDCCBBC",
   "4DCDFINR3S5TUUZZYYZZ6Y7XWWUTS5RQQPOPRT3ZX",
   "XWXWW3V3UTSRSS3R5Q7PONOONMM3LMMNMNN6MLM4NM",
   "N3ONMN5ONN4OPOOMNMOPPOONO6P6OM3NMM3LKKJIHHGG",
   "FE3DCCB3C4BCBBCCBBAA4BAABD5CBBC6DFINR3S5TUU",
   "5Z9Y7XVTS5R3QPPRT3ZXWWXWWVVUU3T3S5R",
   "6Q3POONMNNLMMLL9MLL3NMM3NP4OPONOONMNM3OPOPP",
   "3O5POON4POONOONNMPOO3M3L3KII3GFEDD3CB4C3BCBBCB",
   "BABA3B3ABDDB5CBDCC3DEHMRRSS5TUU5Z6YXYY6X",
   "WVTT5R3QPPRU3ZXWWX3WVV4T3S5R3Q3PQPP3N7ML",
   "MMN4MLLKLMMNM4N3ONN9O4ONPNO7PNNOO3POONMO",
   "3MNNOONM5LKJIHGGFEDD3CBB3CBB5C4BA3BAABBDC3BC3B",
   "DCC3DEHNRRSS3TUTUU4Z7YXY7XWVUT8RPPRT3ZX",
   "X4W3VU3T4S3R7Q3PONN7MLMMNN4MLLMN3MNMNNO",
   "OOMMNNONOOPPOMMPONOPOOPPOOPP8OPNMNNMNMMNONMMLMLLKJIHGF",
   "FEED3CBB3CBB5C4BA3BAABCDC3BCCBB3C3DFIPRRSS3TU3T",
   "3ZYYZ5Y9XWWUT8ROPRTYZZYXX3WVVU4T3S3RQQ",
   "QQPPQQ3PONN3MLMMLMMLLMLLMM3LMLMMNMNONMNNMM3NOOP3O3NOPP",
   "O4NO3NOONOPPOPNONPONMMNONNMMLLKK3JGGFEEDDCCBBCDCB6CB",
   "7BAABBCC3BCCBB4CDDGLPR4S4TUUZZ3YZ5YXY7X",
   "WWUT7RQOQRT3Z3X3WVVU4T3SRR5QPPQQ3PNN6MLLM",
   "3LM3LMML3MLLMNOOML4MO3NONMNNMNON4OPPONONNP3O6PO",
   "POO8M3LK3JHGGEEDD3CBCDCB4CDD6B3A3B3C3B3C",
   "5DEHMQR4S6TZZ7YX3Y7XWWUT8RPQST3ZX",
   "XX3WVVU3T4SRRQQRQQPP3QPPO4ML3M5LM3L6MLLMNOOM",
   "ML3M3NOP3MNNON3ONOPPOM3OP3OPOOP3ONON9MMLK3JHG",
   "GFEEDCDCBCDBCBB4C5BABA5BCBAAB3C5DEHMQR4S6T",
   "ZZ9Y9XWVUTQ7RQRSUZZ3YX3W3UTTSSTSSRRQRR",
   "3QPQQ3P4MLMLMMLMLM4LK4L3MN4ML6MONMNMNNMNO4N",
   "MMONNPN3ONOPOOPO3NMNN3MNNMM5LKKJHHGFFE3DCB4C3BCCBB",
   "3B5A6BAAB3C5DEJPQR3S7TZZ9YY7XW",
   "WWUT3Q5RQRSU3ZYYX3WVUUT6SRRQR4QPPQPOO4ML3MLLM",
   "LMM4LK5LMN9MMMOMMN3ONNMPO3NMNMOP5O3POON3O",
   "3N3MNNM7LKJHHGGFE3DCB4CB4C5B7A7B3C",
   "5DEKPR4S5TUUZ8YXYY7X3WVT3Q5RQRSU3ZY",
   "XX3WVUTTSST4S3R3QPPQQPPOMML3M6L3M9LLM4LM",
   "5MNNMLM3NMLMONMNOONMNOOPN5ONONN3ONN3MOOMLL3MLLKKHH",
   "GGFE3D6CB4C5B7ABBA4B3C5DELQR4S5TUU",
   "9Y9XX3WVTRPP7RSU3ZXX3WVVUTT6S3RQQ",
   "3POPPONNMLK9LMMLK3LKK3LKLK3M3L6MLL5MOMMLN",
   "MNNPONNONMMN4ONOOMNOONO3N3MNMLLMLKJIHGFFEDD5CBABBC3B",
   "3B7ABA5B3C5DFORR3S7TU9Y9XWW",
   "WWVTRPPQ6RSU3ZXX3WVV3TST3S4RQQ7PONMMLM4LKLL",
   "LMM6K8LMM6LM7L5MLMNOM3NOO3NOO6NL",
   "NONM3NMMLLMM4LKJJIGFFEEDCCDCBBABBC7B6ABA5B3C",
   "5DFPRR3S7TU9Y9X4WVTRQPQ5RSSUYZZX",
   "XXWWVVTTSST4SRR3QPPOOPPONN3M5LKLLK3L4K4LMLMMLMML",
   "LLKLLM7LMLLN4MN3MPOO3NPP5NMJL7NM3LMM4LJJ",
   "IGGFEEDCCDCBCA9BB9A4B3C4DEFPRR3S6TUU",
   "9Y7XWX4WVUR4Q5RTUXYZ3XWWVVUTSST4S3RQQ",
   "PPOOPONNMLLMMLKLKKLLKKLKLJKLKKLLKK3LMLLK3LK3L4KLM4LMK",
   "4LMN6MOONOONMLK5LMN7MLMMKJJHGFEEDD4CBA6B",
   "4B9A4B3C5DFQR4S5T3U6Y9XXWXWX",
   "3WUR4QRSSRRTUXYZ4XWVUUTSST3S4RQQPPONOONN4MLL4KLK",
   "9KKK8L4KL3KL3K3LML3M5LMML3MLM4NMONM",
   "6L4M3LMMLMLLKKIGG3ED4CBA9BB9AA3B3C",
   "5DFPRR3S6TUU7Y9XXX5WVS4QRRSRRSUXYZX",
   "XXWVVUTT5S4R3QPONNOPNMLM3LML3KLKJKLKKLLKLL3K5LKK",
   "J6K3LKLLM5LKK3LM6LMLMOMM3NL4MLL4MLL4MLLK",
   "KIGG4ED3C3BA9B9A3B3C5DFQRR3S6TUU",
   "7Y9XX7WTR3QRRSRRTVXXYYXWWVVUTT5S3R4Q",
   "OPMMON3ML9K8KL3KJ6KJ3K3J5KLLKLKJKKLK",
   "5KJJ6KJL3MNNL3MNMMLLM5LMLM3KJJHFEE3DC4BAA3B",
   "5BAB7A3B4C3DEHQRR3S4TUTTU6Y9XXX3W",
   "4WTS3Q4RSTVXYYXXWWVU3T5S3R4QNO3MNNMMLLKLKKJ4K",
   "L6KLKKJJ3KJ3K5JKKJ3KLL4KJ3KLK3JIHIJJK3JKLL3M",
   "LLMMNNM9LLM3KJJHG3EDDCC4BABABBAABB9A3B3C",
   "C3DEIQRR3S8T7Y9XXX6WTSRQQ4RSTVXYYX",
   "WWVVU3TSTS6R3QONMLLNMLL6KJJ3KLJJ7KLKKJJI3KI",
   "IJJKKJJ6KJK3J3KJJ3IHHII3J3K4LKKLMNNMLMLLKLKLKLLKK",
   "KKJIGFEE3D4C3BAABAABB9A3B4CDDEFKQRRSS9T",
   "YYX3Y9XX3WX4WUTS3Q3RSTVY3XWWVUUT3STRQ4R3QP",
   "OMLL4KLLKLKLLIJJKJKKJKJJIKKJKKJIH3JIHHIIJJ3I3HGHGHH3GH",
   "HHIIHHI3GHGIJKJ5KLK3LMLMMLLM8LKKJGGF3ED5C3BA",
   "AA4B8A3B3CDCCEIMQRRS6TUTUU6Y9X3XWW",
   "4WUTS3Q3RSTVY3XWWVUUTSSTTRQQ3RQPQPNML9KKKL5J",
   "K5JIJKJKJJHHI5HGI3HGF5GF5GFGHHIH7GHHKKJKJK",
   "J6KLMMLLM3LMM4LKJHGG3ECD4C5BA4B8A3B3C",
   "3DFJMRRSS6T4U4YXY8XW3X6WVUTQPP4RTVY3X",
   "WWVUTTSSTT3Q3RQ3PML6KJJ3KJKJI9JI6J6HG",
   "GFE3FGF3G5F3GFFGGHGGF4GFFGGH6J4KJKKLLMM3LMMLL",
   "LLKKHHGEEFDD4C9BB7A4BCCBCDDGJNRRS9TUU",
   "6YXY9XX6WVUTRPP4RTWY3XWWUUTSRRSRRQQR3Q3P",
   "LLK5JII9JJJK4JI3J3IHFGHGG3F3E7D3EFEEDE",
   "6E4FG5FEGHI5HIKL5KLLMML4MLKJHHFFEED7CB",
   "9BA9BBCDDILQRSS9TUT6Y9X3XWW",
   "5WVTRQPQ3RTVYXXWWVUTT6R6QPNMLKKJJIIHHII3JIJJ3I",
   "I5JIIJIIHIGEGGFGFFECCDCDDCDCC4DCCDEE3D4EFE4F3E3F",
   "FGG3HJ6KL4MN3MLLJIHGFEED7C9B9B3B",
   "CDEJNQSS7TUTTUU6Y9X5X5WVURQPQRRSTV3XW",
   "VVTTS6RQQ4PNMKK3JI4H3I8JIJJIIJ5IHHGFFGFFE",
   "EEDD4CBCBB4CB4CBBC7E3FEFE4FEE4GIIJJKJK4MNM",
   "MMLLKJIGGEED5CDCBC5BCC4BCCBBCBBCDDEKPRS9TTTUU",
   "5Y9X3X8WVVSRQQRRSTVXWWVUTSRR5QPOOMLLKJKJ",
   "IH7G4HIJJ8IJ6IHHGGFF3E3DCCBC9B3BC",
   "DED5CE3DEF4E4FGGFFG3HIJJKKLLMMLMM3LKJHGFED8C",
   "3C3B9CCCBCCDEFPRS3TU9TT5Y9XWXX3W",
   "5WVVSRQQRRSTVXWWUTTRRQOPQQPONMMLKJJIH3G5F4GHHIJ4I",
   "4IJIIH3IHHGGF5ECD5C9B5BC7BCCB3DEFFG",
   "4GFGHHGGIJIJJ8MLKJJHGGED7CBBCC3BCB9C3C",
   "DEGQRS9T5T4Y9X3X9WWVTSRQRRSTVXWVT",
   "TSQQPNQPOOPMKKJJIHHGFEEFEE3F4GHHJ6IJ3IJ3I3HGGFEEIE",
   "4D3CBCBCBC9BBB7C6BCCDF5G3HI4JKKLLMM",
   "M3LKKJHGGEDCD9CB9C3D4CDEGRSS9T5T",
   "3Y5XY3XYYXX9WWVTS4RSTVWVUSRRNKJOKK3OKKH6F",
   "FEED4EFF5G5HIHIIHH5I3HGGF4E5DGDDC7BCBC",
   "4DEFE4FEE3D3CBA4BCEFGGIIHIJK7L3KJIHGFE4D4C",
   "5CD5CDDEDD3CDEFJRS9TU5T3Y9X3X5W",
   "6WVUTS3RSTVUTSRQQLKMLJJPKKHGGFF5E4DEE3F5G5H",
   "4H6I5H3GF8EDBA3B3CD3EDEDD3EFHIJJIHHGEDCB",
   "6BAACDEF3JK4LMMLL3KIHGFE3D9CCCDDCC3DEED3CD",
   "EGLSS7T4U4T3Y9X3X9WWWVUTS3RSTUTSSR",
   "QP3MJIKKJJGFFEF4E4D5EFF3G9HH6IJII4G3F",
   "5EDC3B3CDEEFEE5DEFEF6GJJKHGDBCBABCCBAABBFHKK4L",
   "4L3KJHGFE3D3CBBCC3BC7DEEDDCCDEHNS8TUUTTU3T",
   "3Y3XY8X3WX8WVUTSRRS3TSRQONKJLKKHHFEE8D",
   "DED7EF4G5HIJ3IJJIJJ4IH4G3FECBABCEFE6D3E",
   "E4FGGI3JIHJHI3HIJKFBBAABDEEDBABADGIJJLKKLKKJIHFECCD5B",
   "BDCBB3CDE3DEFEDCDDGJQS9T7T3Y9X3X5W",
   "7WVUUSRRSTTSSROOM3JG3F3E7DED9EFF3G3HII",
   "3IJKJJKKJII3H3GFGFF3ABD4E9DD3EFFGHKLMK3JIHIIJ",
   "IJKE4ABEFFDEDAACD4EFF4GFED3C5BCCBBA3C5D3EDCDD",
   "HKQS9T7T4Y9XXX9W3WVVU3S3TSRRL",
   "LJKKH3F3EC6D9E4EF3G3H9JJ3KJIGGHGFGG",
   "DBAAB4F4E3D3C6DEEFGGH3KLKKIJJIIHKGB3ACEFEEGBACEE",
   "6EDDEE4D9C4BC3DED3EDCDDHKPSS9T6T",
   "5Y9XX9W3W3VTSRSTTSQQJIGEE4DCCBABB3EBBC",
   "CCB4D5EFFGHIHJ6KJ5KJJ3HGGE3AE9FFE5DE",
   "D3EFF4GFEFGIKNOK4JHHJD4ACEFFEAA4EDDEEDF6EDDEDDE",
   "DEDDC3B9DDFFIKMPQRSS9T3T5Y9X6W",
   "7W3VTSRSTUQKHEFEEFEGG3H4GHHGE3DC3BC3D4EFFHJJKK",
   "LL3KJ3K3JIHHGGCBABCGFE8FE5DEDEEFFGGHH3GFFGHHPQO",
   "IJII3HB4ADEEHAAD3EDDFED6E9DDCC3B3DCC3DEFGH",
   "JKLNOPRSS9TTT5YXXY6X9W4W3VTSRSRPEEFF",
   "GGHKLPQQPPOLMLJKJIHHFFD3BC5DEEFGIJK3LKKJKL4KII3H4A",
   "C4FEE6FEE7DEEF6H3GEFGHNRLIHGHGIE4ABCDEAADE",
   "EDDEEFDD4EDD4CBCBC5BC3D3CEEFGHJLL4MPRS9TTT",
   "3YXX3Y6X4WX6WVWWVWTPHDEEJOQRSSQPMIGG6F3E3D",
   "FGIJGFBBACC4DFGJNNPNNLLKM3LIIHGD3ABGFFG4F4GFE7D",
   "4DEEDEFFGGHHGGEFGGQPJH3GJI8AC9GGGFFEDCBCB3A",
   "3A4BC5DEGHJKK3L4MOOQS4TUU4T6YXY7XWXXWW",
   "WXX4WXWSFDDEHOSSTSRPKJKH5GF3EDD3E3DHJJEBAABCEDDEGIPP",
   "PMMKIHHGGHJHHCB3AEEFG5F3GFFE8D8CEDFGHGGFEFG",
   "LQMGHHGGJ6AEFG4HII4H3GFEDDCCB4ABB3CDD4EFHIK3L",
   "L7MORS3TUTU3T6Y9XW3XWWV3WVWSODCDEORSUVTR",
   "PONMMIHHGGF5E4F3EDFHKFB3A3CDEGG5EDC3BC3DBBAADGF",
   "GG9FFE7DCB3CBDECBBEFG3HEEGGQQGH3GHD5A3HII",
   "I6JIHHGGFEDDCBBAABBCCDD4EFFGJKK3MLML5KLRS9T",
   "6Y9XX6WVXSNE3CHQTXXWWUTR3QRPKHG9F3F",
   "FFGFFEEHNFB4ACHF3A6BCCB3ABBAEGGH4G4F7E3DEE",
   "4CBDOPPKEHOKHFGHFEFFLPH4GIF4AHI3J7K3JIIHGFEDCBB",
   "3BCDEF4GHIJ3L3MKKJI4HIPRS8T6YXY7X5W",
   "3WSFDBDCFSV3XYWVT5RPIGF5G7F5GFDEGPGB3ABDCAA",
   "9B3ABBAB3GHHGG5F6E3DEFEBBDCBBFGPNGJRQKEFHHG",
   "FGHQKH3GKHB3AIJ4KLKKLKL3KJJIHGECCD5CDEG5HJKLMLMLL",
   "KJJ3IHHIMRS8T5Y9XX6WSNFCCBDGQW3X3WVUS",
   "4RPGGHKKJHGFG5F4GIHFDDOOE4ACC3A7B8AHGHH",
   "HHG5F5EDDCDDEFD3CBACFJMGKRQOEFGHI3GPLJ3HJJF3AJJ3K",
   "K3LMLL3K3JIGFD6CDDFGH3IJKK4L3KJJ4IHIKQS8T",
   "5Y9XX4WRHCBCBCDQRT3YX5W3SRQKOSSOPOFEDDCCBCC",
   "EFFGJNNJFDHQCB3ACABEGF3EDB7AFGGHIHHGG4FEFFEDCCDDEFF",
   "FD5CEKJGJLQQ6HIIJMNIJJIJI3A4KLL6MLMLKKJJHFD3C",
   "C3DEHIJ3KL9K4KJIHIJPR8T5Y9XX3WSE",
   "BBCBBCFRTWY5XWWVTSSRPJSUSJHGECBBC3BCCDDFGKPLGDDKFB4AFQ",
   "RIFGFGFDC5ABGG5H7FGDC5DEEFFD3C3BNPMIPQMJJ3I",
   "3IKMMJJKIKJ3A4L9MMMLKKJHGED3C3DEIJK5L5KLK",
   "5KJ3IKOR8T5YX3Y6X3WLCBBABBDKSW3ZYYXX3WT",
   "SRROJTTQGEEDCBCCB4CBBDFGPNIEDEKE4ANRQEFFGFGEDB4ABJG3H",
   "3G3FGGHGDC3DE5FED3CDDQRNJRTRLKJJKJHHKKLJJKJJI3A3LNM",
   "N6MNMNMLLJIGE6DEFJKKMM3L3K7LKKJJIIKMR8T",
   "5Y9XX3WGBCBAACJPTIFGJLQRT3VTSRQNJQOHKJGEDBCCBADF",
   "CCBEDDIJHJFDNJDAABMQFEGGHHIHIC4AEMG3HGFFEFGHHGG8FGG",
   "6FGFRSSMVWPMKKLL5KLKKLLJJCAAJO6MN7MLLKJHGFEE",
   "EEFGHKKL3MLLKKLL4M3LKKJIHJKQS7T3Y9X3X3WFC",
   "BBAAEKQRFDCCDEGJPRRSSRQPKNJGQOIGEB3CBBCCBBEDDIKHHJEIQGAABIN",
   "EFH5IHE4AFPGGHHG4FGHHGGFFE5FGGFF5GHMPPMRTPLKLKL",
   "LKK6LMKJEAAJPNMN3MNN3MNN4MKJHGF3EFGIKLMLMMLLKLLMNMM",
   "MLL3KJIHJKQS7T5Y7XWXX3WGABBAAJOTHFGDEFEGIGJIL",
   "QRQPMNKHQQMHIE5CDCBCDCDDLKFQREMM3ADGFIJ5KJH4AIR3HG",
   "4GF3GFFEEDE4FGGH4IJJ3K4LKJKLLMM5L4MLJGBBHN3M",
   "NMNNMNONM3NMMNLJI4GIIJLLMM3LMMNO3NMLL4KJIHJKPS7T",
   "YY9X3X4WRDCBBAKQUHPSRSSR5S3RQNNQPNMQOHIG6C",
   "4D3CKOHQSEJQABACFG3KLLNLKH4AJTJIIHH7GFFEEDE3FGFG",
   "GH6J5KLKJ3K3MLLM3LMMLJHDCHOMN3MNNM7NONNMLKII",
   "IIJJKLMMLM3LM4ONML5KJIIJKOS7T3Y9XWWX4WS",
   "F3BLRUJSZZYY4WVVTTS3Q3R3J3GDCCDEEFFEE3DKRISWEHRBABDF",
   "HK3LMOKKJ4AJUIJIIJH4G4F4E5FGGHI3HJJKJJK4J3KL",
   "MM6LNMMKGDCHN3M4NMNOO4NO3NMLJKKJ3K3M3LKLMNNONMM",
   "MLL3KJIHJKOS7T3Y8XWXX5WXVBCFMSUJSYZ5X3WV",
   "UTSRR3TRPMJIH4GF4GFFEFFGFGIFFMGBBGJLMM3LMKKJC3AHXHJII",
   "IHGGHGGFF8E3F5GH3JIH3IJJI3KLKLLMNKKLNNKGDAFM3N",
   "ONONMMNNONMMNONNON4M3LMM3L3KL6MNMLK3JIHIJMS7T",
   "YY9XXWWX4WXVFEGOSTIRY3Z3XYXWVVU3SUVVSRPKJI6G",
   "HGG4FEF3EGGLJBBJL4MLLMKKJD3AEWIJIIJIGGHGGFFEF4E5F",
   "4GFGHH6I3JK4LKLLMLKKNMKGDBEOLN3ONOMNMOONMNNOO3NMM",
   "3MLL3MLLK3L6MNMMK3JIHIJMS7TYY9X3XWWX3W",
   "WBEIPSTJRWXZZ4YXWVWVSST3VTSQKJJH5GHHGGFFEEF3EHILKCCNP",
   "PNO3LMKJJG3ADUJHHIJJ3H3G3FE3FG3F5GFGHHIIJII3JKKLM",
   "M7LKMMKHDBCMLMNO3NMNMOONMMNON8MLLMM4L3MON4M",
   "NMLKJJIIHIKMS7TXXYY9XX7WDEIQTTJQTZZYYXXZXWW",
   "WV3TVVUTRRNKKI4HGH3GFF5EFKKNPCBRRQONMNLLK3JD3AOSIFH",
   "I3JI4HG4F5GFGGHI4HIIHIJJKKJ3LKKLML3MNOMKHDCEP3N",
   "NNOONMMOPONOMONONNONNOMLLMML3KLMMN5MNNMK3JIHIKNS7T",
   "9X5X6WXEEJQSTKPSZYZYYXYX4WUTT3VTSRMLKIHH3G",
   "4GFF5EGONMQCBSRQPPNMLLJJKJHAABIWLIHI3JII4HGGFF6G",
   "G3H3IJJI5JKK4LKLLMLMMNMMLGCBFPMNMNNOONMNNPON4MNO3N",
   "NNMLLMML3IJKLLML3MNNMK3J3ILNSS6T3XY9XXWWXXWW",
   "WGFJQSTKNR3ZY4X4WUTUVUUSRRNLKJIHH6G3FEEFFIPQORDBSQ",
   "PPOMMLM3KJJCAAFZPKGHIJJ6H9GG4HI9JJJ4K",
   "8L3MLGCCGPLOONN3ON5ONNMMNOON3OML3MLHGGHIJKLKLMM",
   "MNMK3JIHIKNSS6TYY9XW3XWX4WNCHQSUPMRWZZY4XWW",
   "WWVUUVVUSRQMLKHH9GG3FHKPQRQSCBRQ3PM3LJIIJIHAABXWLG",
   "FHI8HIH6G3HIJJKJ3KJIIJKJ5KLMLL4MOLLFDCJ4O",
   "P4NMM6NMNMN4ONMLMMLKG3FGHJJ3KL3NLKJJIHJKOS7T",
   "3Y9X3X6WQDGQSTPMQVZYY4X4WVUUVVTSRQMKJ6G",
   "9GKPQRRQSDCQRQQP4LJIH3JAACRZPHFG8HII4H3GH",
   "HHI5JKKJ3IKK3JKK4LM4NLKFDBLMNOOP3O4NMNONNMNNOONO",
   "ON3MNMKHGFGGHJJKKLL3NLKJJIIJKPS7T5Y9XX5W",
   "XSDFQRUQKPTZYY4X5WUUVUTSRQNKJH6GHGG4HJPQ4RQCFRR",
   "RQOLKLLJJI3JAABJWSIGEH5I5HGHHG5HI5JKJJ3H5JK",
   "K3L4MNNLLECBONNOPP3O3NMMNONNONM3N3O4MNLHH3GHJJ3KL",
   "NOOMKJJIIJKPS7T3YXY9X5WXWWIEQRTRLKRWXYYX4W",
   "3WV4USRROLLJJI3HIJJKJJMNQ6RHBMSRQOMMLKK5JIGAADMZJ",
   "HFGHH5I3HI8H6I3J3HI3JKK3L3M3NMLKDBBP4O",
   "4ONN3ON3ONN4MONO3MNONKIHGGHIJJ3KMNOMKKJIJKLRS7T",
   "3Y3XY8X4WXWWPDQQTRPJQTYY5X4WV3UTSRRPPOLKKJII",
   "3IJJLOQ3RSRQRFBQRRQOMMK3LK4JIEACGSQIGGHHIIJII3HI6H",
   "HHIHI3H3JIHHI3JKK3LNMMNONLLJDCB3OP4OPN8ON3M3N",
   "ON3M3OMKIGGHHI3JKMNNMK4JKOR8T3Y9X5X3W",
   "WXREQRSSQJQSW5X6WVUUTSRRQPOMK5JIJLOOQQRRSRRQSDD3R",
   "POM4LKLKKJJIGACDJTJGFGH4IHHI4HIHIHHIJII3H5JI4JKK",
   "LMMNNMMONLKJCBC5OP3ON8ON3MNO3N3MOOPOLJ4HIIJJK",
   "LNNLL4JKPR8T6Y9X8WJMRRVRPPQSXXY3XW",
   "3WVUTSS3RQQPML4KLMLQQ3RSS3RQUBLSSRQONMM3KJJK4JIABDQ",
   "VLFGHIIH6IHHII4H5IHI8J3K5MN3MLKGBBINNONO",
   "O3PN8ONMLMNN3ONMMOO3POJIIH4IJLMMLKK3JPRS8T",
   "3Y9X3X4WXWWXQIRRVSRLQRY5X3WVVUTSS3RQQPMNN3L",
   "OQPQ4RSSRRPRUBQSSRPMMLLKK3JK3JIJAACETSFGHJIHHIIJJ4I3H",
   "HHI5HJJI3JKJJ3L5MNONMMJEBBMNOOPOOP5ONON3ONN3MNO",
   "NN3M3O3PKJ6IJLMM3KJJKQRS8T3Y3XY9X4W",
   "3WTGQRVURKPQWXXWWX4WVUTTS5RQMOPMMNQ5R4SRRPRVBRSSR",
   "P4MLL3J4KJIBABGIUHFH3IH3IJ3IJI4HII3HI6JKKJ3L",
   "5M4NKIEBFOOPOP3OPP3ONNM3ONNM3NO4MNOO4PLKJ5IJ",
   "LMMKK3JKRSS8T3YXYY9XX7WXDJRTVTOHKSVX4W",
   "3WVUTSS3R3QPO3PQ4RQRSS5RVSL3SRQONMLLKLKJ5KJJEAC",
   "FFRMIJJ6IJJ4IJ7IHJJI3J6K6MNNMIDAEPONNONO",
   "8ONNMMN3M6NMMNOO4PMKJ5IJLMMK3JKLSS9T",
   "3Y9X3XWWX6WEDPSVVRIIRUX3W3XWVUTSS3R3QPPQPQR",
   "6RSS3RQRWQQSTSRPON5L4K4JKJCBCGRUMJJIJJI5JLIJJ",
   "IJ8I3J6KL7MNMGCBIPNONNMNOON3OPNO4NMNMM3N",
   "NO3MNO4PMKJ4IJK3LK3JKPS9TT3Y9X3X5W",
   "4WLBIRTVTQGPRV7WVUTSS3R6Q9RS4RPTWJSTTSQ",
   "POMLM5KJKKJ3KJHABEKWTIJK7JIJJIIJJIJ4IHIIJJ4K3L",
   "L4MOMNMEBBPMMOONN8ONO5N3MNONOO3MOO4PMKJ4IJK",
   "3MK3JKQS9TTYY9X4XWWXWX4WVIBQRVTTSJKK4W",
   "WXWVUTTS3R3Q9R7RQQWQB3SRQQMML4KL4K6JE",
   "BBGGFSMKJJKKJIIJKJJHIJIIJIIHIJIJ3KLL4KLLMNNMMGABFOMLM4O",
   "OOPOO5NMNN4M5N3MOO4PLKJIIHJJKNMMKJKKPR9TTT",
   "YY9X4X3WX6WSCORSVTUIKHT6WVUTTS5RQ6R",
   "8RQRQRVHBTTURPPKMKKJKJKKJ3K3JIJHDACGHWTNKJKK8J",
   "9JKJJKKLMM5KLMNNPHEABLMMNNOOP4O4NM5N4M3N",
   "MNMMLNO4PKJI3HIJKNMMKJJMQS9TTTYY9X4X3WXW",
   "6WJJQRVUUNHKQU5WVUUTT9R3RQQ5RQORQTSCDTTUQP",
   "OL5KJKJ3KJJIIHIHFDACGJZTK6JIH9J5JKKLLMM3K",
   "KKL3MLECAIMMNM3NONOO4NOLMMNN7M3NMMLNO3POKJI3HJJK",
   "NMLJKJORS9TTT9X6X9WWWVCIRTVVURIOORV",
   "WWVVUUTT9R3R3Q4R3QVXBBMTUTQMNLKJJ4KJ3KIJIJIJH",
   "IGBAAFGJUQNKKJHH9JKKJJKJKKLMML3K4LOKCCB3OLMNMNMOM",
   "MOOM3NKKL3MN4MNN5M4OMKI4HIKKLM5KRS9T3T",
   "YY9XXX9W5WECLRT3UQMNOU3VUVUTSS8RSRR",
   "QRQR5QSXWBBRTVUNOMLKLKKJJKLKKJKJI3J3HDBBCEHTWPOMIIHIHIJ",
   "JJII4JK3JKLL3KJLKKLPKGBBEPOOLMNONNMMLONM3N3KMMLN5MN",
   "5MNOPMKJIIHIJJLLMLKJJKKRS9T3T9X4X7W",
   "5WVWQBDRSTUUTHMPSUVVUUTTSS9RRRQQ3R4QXXSBFSUVSPN",
   "KKLL6K3JK5JIHGFDBABDMRTQOK4IJJIJ3I3J5KLM3KLL",
   "KLPKHCBDMNMOMMNO3NMMONM3O3K8MNN5MNOOLJJIJJKKL3M",
   "LKJKKMRS9T3TYY9XX9W5WVWTEGPR3USPJQ",
   "RS3U3T3SRRS5R6QPQQUVPCGSTWURMLLKL4KJKJII6JIK",
   "JHGGFBBADEMRVROMLL4K3J3KLML5KMMNOQNH3BKMMOMNOONNM3N",
   "OMNOONNLKMN8MN5MOOMJIILLMNNONMMKJJKMQS9T4T",
   "9X3X9W8WRCJPSTUTSOMQRTTUUTT3S5RQRQR",
   "4QPQQSWRDCPTUWTROML3K3L3KJIJKJIKJIJJIGHGD3BCFKNVURMLLM",
   "N4LKLMML4KMNOPQPJFCABENMNO3NOOLMMNOOMNO3NMLMN3ML4MN",
   "5MOMKIIJMMNNMNMML3JKPR9T5T9XXXWX7W",
   "9WVCCJRS3TSJLQSTTUTT3S7RQRQPQPOPSUUFCEQTUVSRNM",
   "L4KMLLKKJJKK8J3HEDC3BDGLQTSROMML3MONONMLLKMQRRPMJ",
   "DBAAGMNMNNLNONN3MOO3NONN3MNMNN5MNN5MOLJHJKM3NMMLKK",
   "JJKLQR9T5T9XXX9W8WVWVMCNQQSTTS",
   "QKJPR3T3S5RQQRQQPPNQSURECEHSUWTRPMKKJJLMNOLKLKKJKJK4J",
   "J5IGFDDC3ADFGIKRSTSS9RRNJGC3BCHNMMOONOONNOONMNNM",
   "3NO3N3MNOO5MNOON4MLKKLMPPONMLKKJKKLQRS9T5T",
   "9X3X9W9WWUMFMQRSTSSRHHL6S3R3QRQP",
   "POPRTVQECDFGTTUTQOLLJJKKOPPLKK3JKKIJK5JIIJHGEEDCCABBEEFH",
   "IKOQQR4QPLKIECC3BHKNMNNONNPONONOMNONMMNOON3M4NOML3MOO",
   "N5MLKLM4ONMMKJKKLLRSS9T5T9X3X8W",
   "9W3WUDFJPRRUTSQKJQR4S3RQPQQPPOPQVVTFDDFFJSTUTQOLL",
   "KJKKMPQLKIIJJKJJKKJK6JIHGFEDDCBCBCCDDEFGG3HGFFDD4BCFI",
   "3LM4ONONN4ONOOMMOM3NMM3NOOML3M4NMMN4MN4ONMMLKK",
   "KLLRS9T6T9XXX9W9WWVWVVSHFJMQ",
   "S3TSRQQRRQQR4QRQRSVUJDBDEEDH3STSRPK3JIJKJL3K3J3KJ3K",
   "3KJJKKJG3F5E3DC9BCDEEHHIKLLMNLOOPOONMNONPONM3N",
   "N3OMOOM5N3MNO4NMMNNMOOP4O3NMLLMMQS9T7T",
   "9X3XWWX9W6WVV3WTDDFLOQSTTSS3RQQ6RST",
   "URP3CFFEEOT3SRQP3JIIJ3K7JKJ3KL5KJKJIG4F3EFEF",
   "F4ED3EFFGHIHJJKLLMNNMMNOONN3OPO3N3ONNO6MNN5M3N",
   "NNMMN4OP4ONNO3MLORS9T7T9XX4WX5W",
   "9WVWWVVWYRHBFHKQRR5S5RSTTUTLE3CEFEEGRT3SRQPJH",
   "3HIJKJJ4IHIJJKKJKKJ3KJKJIGFGG7F3GFF6GHIJKJKKLM",
   "MMNNMMNO3NOOPONOOPPONNO4MN6MLM3N4MNN3OP7OMML",
   "LQR9T8T9X9W9W5WVV5WQ",
   "GDEGHNOP6RQNKE3CE4FEEDPTT4RQQLJ4HIJJGHGHG7HJ",
   "II6KJJ5G3F6GHHG4HJIJKLKMLMNOLNOM3ONMN3ONN3O",
   "3ONNMMON6M4N5M4NOON3MNN4OQSS9T8T",
   "9X3WX9W9WWVV6WTRFEDEFG3JIHGECBBCE",
   "GGH4FEERTS3RQQOLJIGGHIIHG7HIG5HJJK5JGF5G3F",
   "3G6HIJJIJLLKLML4NOMMPOOMMOPONNOPOPNMNNMMN8MNONN",
   "N3MNLNOPONMNM3NONPRS9T9T9X3X8W",
   "9W9W4WTRNE4CBBCCBCEFHJHGG3F3ERSR3QRQPNK",
   "I3G3H4GHIJGD3BEGGHJI4JIGF3GHG3FGG5H4IJJ3KLMLM",
   "KMNMOOMNO8N4ONMN5M3NMM7N3MNMMNOONMMNNPOONQ",
   "S9T9TT9XXXWX9W9W9W",
   "WVUUTTSS3RQQPOLK3I3G4FEEORRMIFHSRPNJ8G3HF7A",
   "ABDEG5I3GHHIH8GHHIJIJJHJLLMMLKONNMNMNONNMMNOPONPOP",
   "PO6NM9NNN3MNNON4ON3OMOOQR9T9TTT",
   "9XXXWX9W9W9WWW3UTSRRSRRQOMMLJJI",
   "5GFFEEIRRJHECGRQNJ5GHGGHGEDDCB4ABBCDEGHHIH3GHHI3HG",
   "5GHHIJ3IHJKKLMMNM6NM3NMMN3O4POONM3NMMNMMONN3ON",
   "MNMM4NOOPOPNPOOPRS9T9TTT9X4XWX5W",
   "9W9W3WVVUUTS5RQPKMMKKIH4GF4EPQIGECCFRP",
   "KH3GF3GHF3EDC3ABCDEDDFFGH4G3HI3H5G3HII4JKKLLMM",
   "LMNLMNNMNOONNOONOPOPOONNMMN3MNNMNON3ONMNNMNNMNO4POPONQRS",
   "9T9TTT9XXWWX9W9W9W",
   "WWVVU5SRQQPNLKJII4G5FEEGJGFECDDLPHH4F3GFF3GF3DE",
   "FFGGF4GFFGHII4JI4HGG3HII3JKKLKLLK3LMLLMNMN4ONOPPN",
   "NNMMON3MN3MN5ONNM4NMNOPOONNMNQS9T9T4T",
   "6XWXXW3X9W9WWVV8WVUTTSS3RQPQNLJJHH",
   "HH5G3FEEKJH5EOKGGF9GFG3F3GH5G3FGHHIJJKJI",
   "5HGGHHII3JKLLKLL3KL6M7N3O3NMNNMLLNMMNNMN3ON",
   "NLOOR3N4ONMNQRS9T9T4T9X3WX7W",
   "9W4WVV7WVUTTSS4RQPLKJI4H4GF3GFFGHJ3GLK",
   "IPKG3FG3H4G8H3G4FGHHIIJKJK3J3HGGHHIJIIJ4KLL",
   "3KL4MLMNNMMNN3ON3ML5M5NM5NMMNOONNP4NMMQRS3T",
   "9T9TTT8XWX9W9WWWVVWWVV4W",
   "WWVUTSS4RQQOLKJI5H3G6H4GHIQRROQRKIHGGF3G4HIIH",
   "HHG5FG3HIJKJKK3JKJ5HIHHII3JKK5LMLMN3MLLOON4ON",
   "NOOMN6M3NMNOO3N3MOONONNLPQRS9T9T7T",
   "7X9W9W5W4V8WVUUSS4RQPOLKJIHH",
   "3H3GHHI3JIIH3GKPQLKMPLKG5F3G5HG3F3GHIIHI4JKKJ",
   "3JI7HI4J4K5L4MNML3NOONONNON8M6NONN",
   "MMNNONOOMMORSS9T9T8T3XWXX9W5W",
   "9W6V7WVUTSS4ROONLKJI4H3GHIJJLL4K3IJM",
   "QMJKMMLGG5FGG4HGGFFGGHHIJJIIJJKJJK5JI6HIHHIJJKJK",
   "LKLL7ML3NOONO3NOO4MLM6NOONO4NMOONOQRSS7T",
   "9T9TTT3X4WX9W9W4W6V4W",
   "WWUTTSSRRQQPMMKKJI6HIJLNNMON3PONMKKPPK4JHGG3F3G4H",
   "3HG3H5IJIJI6JKKJJHI3H6I4KLLKKLL4ML3NONONO",
   "NN7MNMMNO3NO3NMMNNMNQSS9T9T9T3T",
   "3XWXX9W9W5W8V4WVUTTSSRRQQNLKKJ3IH",
   "4HJKOOPOPO5QOLLPPLKIHHGG3FGG4HGHGG3HIHIIJ5I5J",
   "JJK4JIHHIHIH3I3J4K7LMO4N4ONMMNN6MNO6N",
   "3NMNOQRS9T9T9T4T3XWXX9W5W",
   "5WVVW9V4WVUTTS3RQQMLKJ4I4HKKM6P3QRQPNP",
   "PPOLJIHHGFF5GHGGHG4H9III8JIIJJIHI3H3I3J",
   "3K5LMLLMNNONNMNOOMMN8MNO7NM3NQRS9T3T",
   "9T9TTT3XWX9W9W6W9VWW",
   "WVUTTSRRQQPMKKJII5HJOP5QRQ3R3QPPRPNL4HGGF3GHGGHHG",
   "HIHH4IJJII5JI4J4K5J5IJIIJ4KLL7MNO3MNN",
   "3M4N6MNO3NM4NOPSS9T9T9T6T",
   "XXW3X9W9W4W9VV3WVUTTSRRQQPMKJJII3H",
   "HJLQR5QRQ3R3QPQQPLKHGGHHGFGF4GHHG3H3IJI8JIIJJ",
   "JJKJK6JHH6IJJKK3LMNMLMLMMNMNMNNM3N5MNLMM3N3MN",
   "3NOPS9T9T9T7TXWW3X9W5W",
   "4W6VW7V3WV3TSRRQQPMKJJII3HJLORR3QRQRQQRS3QPQ",
   "QPLJHGGHGG3F4G7HIIJIJI3JIJIHI4J3IJJK3JH5IHJJ",
   "IJKK3LMLL5MLN4M3N6MLLM3N3MNNMNOQS9T5T",
   "9T9TTT3X9W9W5WVWVVWVVWW3VWW",
   "WVUTS3RQPPKKJI3HGHO5RQQ7R6QPKKHH4GFF3G3HIJ",
   "IIHHI4JIIJJHII4JHJJII5JIJIHHI4HIJ5K5L3NMN3M",
   "4NMN3M4L6M3NOOQS9T9T9T7T",
   "XXWXX9W9W4W9V3VWWVUTSSRRQNOKJJHIIHHJ",
   "Q5RQRQRQQ3R5QRMKK3HGGFF5G3HIIJIIJI3J4IHHIIJIJ",
   "JJI7JHJI3HIHHIIJJKK3LM3LM3NMLMNNMMNN3M3LMLN5MN",
   "NOPPRS9T9T9T7T3X9W8W",
   "7W9V3VWWVTTSSRRQMMKJ3IHHJM6RQQRQPQ5R3Q",
   "QNLJ3HGGFF3G5HIJJIHHIIJIJIHIHIIHIIJIJI4JKKJHJJI4HII",
   "IJ4K3LKKMNNO4MNOMMNN3M3LML7M3OPR9T6T",
   "9T9TTT3X9W9W4W9V5VW",
   "WVUTSSRRPNMJIIHHGKO7R3QPQQ5R3QPNKK3HGGFFGGHHGHIJH",
   "HIIJH3IHHIHH4I3J4IJIIJIJI3JII4HIJ4KLLKLLMMNMN3MN",
   "NMMNN9MMNONMNNOOQR9T9T9T8T",
   "3X9W9W4W9V5VWWVUTTSRQOLLJ3HGGLPR",
   "6RQQPOQ4R4QPPLKL3H3GFFGHHGHHJI3HIHJI3H3I3H3IJ",
   "4IJ4I5J4IHIIJKJ5KLL9M7MN8MNONN",
   "NOPRS9T9T9T8T4X9W7W",
   "5WVVW9V5VUTTRRQOMLI3HGGMQ7RQ3PQRQQR3QPP",
   "PMKJHH4GEFFG4HJJ5HJJIHGIIHH3IHHJJIIJKIIH6J3IJII",
   "IIJJ4K3L7MN7M7NLMM4NOOPRS9T6T",
   "9T9TTT4X3WX9W7W9V7V",
   "VUTTSRRQPNLIGHGGIQRS5RQQ3PQQ3R3QOQPKJH5G4F3GHHII",
   "5HII4HIJ3IHHJ3IHIJKJHHIH5JHJIIJ3IKK5L3ML4MN",
   "OMMNO4NO3NMMNNMMNOOQRS9T9T9T8T",
   "7X9W8W9V4VUUVVUTTSRRQPOLHHGGHJRSR",
   "5R4QP4QR3QPQOKJH3GHGG3F3G5HI3HI6HI4H3I",
   "IJII4JHHI5J3I5JKK5L3ML4MNNMMO4NM3NMMNO4N",
   "OOQRS9T9T9T8T6X9W5W",
   "4W9V8VUTTSRQQPNLIHHGIKRSS5R7QRR4QNO",
   "NKJIHGGHHG3FGG6H4IHHII4HGHIIJIHIJIHHJHIHIJ3IJJ3IJ",
   "4J4KLLKL7M4NO3N3MNMM6NOONRSS9T6T",
   "9T9TTT3X9W9W3W9V5VUV",
   "VT3SRRQPNLIHGGJM6RQRR7QR3QPNONLKHII4HGFFG6H",
   "4IHHIH4I7HIJJII4HIJJ9IJJI3K6L6MLM",
   "MMNOONMO4NM7NONRS9T9T9T9T",
   "9W9W4W9V5VUVVUUT3SRRQPNLHGGHKO3R",
   "5R5QRQRRQPQQPOPLKJIJJIIH3GHIJ9I3H4I4HIIHI",
   "JJII3HIIJJIH3IJHHI4JLK6LN5ML3MNOO5N3M6NO",
   "NNRS9T9T9T9T3X9W8W",
   "WWVVW9VVVUVVUVUSTS3ROMLHGGHKP5RQRRQRQPQRQQPQPRPPN",
   "OMK3JI4HGGHIJJIIJJIIJJ3IJIHIHH8IHHIIHII4JIHJJ3I",
   "4JKK6L7MLMNOO3NMMNMM9NNSS9T7T",
   "9T9TTTXWWXX9W8W9V9V",
   "UUTSS3ROMLIHHJMQ5RQQOPQR4QP4QP3OMKJ3IH3I3HI3HII",
   "IIJIIJIJ5IJ4I4H9I4IJJII5JKJ3KLK5LMLML",
   "MMNNONMNN9MMNNOS9T9T9T9TT",
   "9W9W3W9V9V3UTTSRRQOMLJHIKNQ3R",
   "R4QP7QRRQQPONMLJI4HII6H3G4HJI3JIIJIIHI4HI",
   "HIJIH3I3H4I3JIJJKKJ4KLKLL3MLMM3LMMNNMMNMNMNNMN3MNM",
   "MQS9T9T9T9TT3X9W8W",
   "9V7VUVV4UTS3ROLL3JKMPQ3RQQPPM3QRRQR3QPPNL",
   "KJHH3G5HGGHGFF4G3H3JHH3JHI4H4I8HIIHHIJJIJ",
   "9KKKLL5M3LNMM3NMNMMNNMNN5MQS9T8T",
   "9T9TTT5WX9W5W9V8V3U",
   "3UTT3RPONKJKJMOQQRQPOMOOMP3RQ4PLKHH4GFFE5FEEFEFFEF",
   "3FE3FHGG9H4H6G3HIJJIIHHJ3K5LKKLLKM4LKK",
   "MNO5N5MNMMLLMMNRS9T9T9T9TT",
   "XX9W9WWW9V5V8UTSRRPPOL3KMO3Q",
   "ONLLNNMPQRQQPOLKKJ6G4F9E4F5E4F9GG",
   "8GHG5IGIIHIJKKLKKLLKKLLKMLLMMKLL3NONN9MLLMM",
   "OR9T9T9T9TTTXX9W6WVWW",
   "WW9V7V6UTSRRQPPMLLJLNPPQNMKJJKLNPQPNNMLJIHHG",
   "GFHJFGG7E3FGGFF7EFF3E9F8FGGHIJHHJHI",
   "JJ7K4LMLKLM3L3NOONLNN6MLLMMPR9T9T",
   "9T9TTTXX9W9W4W9V4V3U",
   "UUVUTSSRQQPPOOKKLMLKKJHGG4FGIJKJKJHH3G6F9EEFEF",
   "EED9EE5DC5BDEE4FGGHHIJJII7KLLM3LKLKMLMM",
   "MMNM3N6MLMLLMLMRS9T9T9T9TTT",
   "XX9W8W9V8V8UTSSRQQ4P4KJJ",
   "IHGGFFDCDEFGJKKIHHGFGF3EF9E4DEEDDEDDEEDE3D3C4B",
   "CDDEFFG4F3GHIJIJIJ5KLM7LKLL3MNNM3N5MKLMLLMMP",
   "RS9T9T9T9TTTX9W4WV4WV",
   "9V3VUVUVV8UTSRRQQ3P3KJIHH4GFEFGHIIGHIGFEED",
   "DCDBC5DEE3DE4CBBCCBCDCBC3BCBC4DEFFGHGH7GHHIJIJ",
   "J6KLLMLLMM4L3MNNM3N3MLLK5LMPRS9T9T",
   "9T9TTTX9W6WV4W9V7VUVV",
   "5UTSSRR4QLKKIHH4GHHMQ6R4QKFDD4C5BDCBCCBCBC",
   "5DCCDEE3F7G3H4JIHHGH3G4HJJ3KL3KLMM8LM",
   "4MN5MLKKLM4LQSS9T9T9T9TTT",
   "X9W9WWW9V4VUVUUV6UTTS3R3QMMLIHG",
   "3GHIKPRRS3RSSRSRPJGEEDDEEDEDEEDDE4D6EFEEGGF5G4H",
   "IJI5JI3H4G3HJJKJ6K4LK3LKL6MLMLMMLKK6LR",
   "SS9T9T9T9TTTXXWWX9W6W",
   "9V5VUVUUV6UTTS3R3QNMLI3HGHIKMRR7SRSSRP",
   "NHGFF4GFG9FFG3F6GF3G4HJIJJ4KJJII3HGGHIIJ",
   "4JKJK3LKKLKLK4L4M4LMML3K5LRSS9T9T",
   "9T9TTTX9W9WW9V8VUUV",
   "UUVUU3T5RQMMKJJ4HKOQRR4ST5SRRPLPKJIHG5H7G",
   "4GFGHHIHJHHGGH3I4J4KJK3JI5HIIJJIKKJKLLK3LM6L",
   "9MLL5KLKLQQR4S9T9T9T7T",
   "3X9W8W9V8VUUV6UTTS4RQOMLJJI",
   "GHHLPQRR4STTSS4RNOPKI7H6GFGGHH4GH5I3HJIJJ",
   "3J4KJ3KJJHGHGHI4JKL3KM9LL3MN3MLLM3L5KLMP",
   "PQ3R4STS9T9T9TTT3X9W8W",
   "VW9V5V3UV3UVUUTTSS3RQPNLJJIHHIMPQR6STSSRRQQ",
   "PNLKIHHGGH3GF4GFGG4H3G6IHHII5J8KJJI4HIJ",
   "3J5KLM3LKK5L3MLMMLLMLL7KLMLLNPP3R3S9T",
   "9T9TTTWXX9W9WW9V5V4U",
   "6UTT3S3RPOMKJJHIKPQR7SRSSRRQOOKIHFF7GF6G",
   "GG3HIHHJII3JIIHI3J8K4J5I5J4KL4KLLK5L",
   "MMLLM5L6K3LJGFDDEFGIMQRSS9T9T9T",
   "5X9W8W9V8V7UTT3S3RQPMKJI",
   "IJLPQR7S5RQOLJHHFF4G6F3GFGGHI3JIJJ3IJIIHJJK",
   "4KJJ3K4J5I5JKLLKL5K7LMM3LML4KJ4KLLMJ",
   "GE3BCCDGKPRSS9T9T8T6X9W5W",
   "WW9V8V4UV3UT3S3RQPNK3JKMQRR6S5RQQO",
   "KIHGG3FG7F6GHII3HJJIHIHHIHJJ5KJJ4K5JIH3I",
   "3JKKL7K7LM4LML4KJJ3KLLMJGDBBA3BCFIRSS6T",
   "9T9TTT3X9W8WVWW9V4V3UV",
   "3UV4UT3SRRQQPKJJLMORR7S4RPOKJIIHH3FG8FHFGF",
   "F5G6H4IJKKJ4K3J3KJK5JI5JKJ5KJ4K4LK",
   "K3LKL5KJJ3KLMNKGD7ABCKQR9T9T8T",
   "3X9W4WV5W9V8V3UV5UT3SRRQQPLKJ",
   "MOQR4SRS4RQQPLKIHGG4FE3FEF3E7F7G3H3I4J",
   "JJKK4J4KJKJIIJIJIJJ5KLJ6KLKK6L5KJ5KLMMJ",
   "GE8ABGMQS9T9T7T3X9W8W",
   "WW9V8V9UT4SRQQPLKKNPQR3S7RQRPNKI",
   "HG3F6EDD8E3F6GHH5I6JKKJKKJ3K4J3IJ",
   "I9KJ7K4L8KJJ4KLMNJGD8ABEJPS5T",
   "9T9TTT3X9W9W9V8VUU",
   "8UTT3SRRQPNKLQQ9RRR4QPKJHGGFF9EEEDEEF",
   "FF6G8HII7JKJJII6JIHII3JKKL9KKL3K",
   "LL6K6JKKLMNKGD9ABEJRS9T9T6T",
   "WXX9W9WW9V3VUVVUVV8UTT3SRRQPNLM",
   "QQRSS7RQRQQPOLKJ3GFE3F5EFF3E3F6G8HIHIJ",
   "8JII7J3I3J9K4KL4KL5K7JKKLMMK",
   "GD9ABCGRS9T9T6T3X9W8W",
   "W9V5VUVUVV8UTT4SRQQNNOQ9RRR3QPPNLK",
   "JGG8FEE8F3GFGHG4HI4HI8JI9J4I",
   "3J9K9K3KJK8JKKLMMKGD9ABCERSS3T",
   "9T9TTT3X9W9WVW9V6VUU",
   "9UTT3SRRQ3PQRR3S5R4QPNMLKJIH4GFGFFGFF5GF",
   "3GHG6H3IHIHH5IJ3I4J3IJJH3I3J3KJJK3JKJJ5K",
   "4K3JII4J3K3MJGD9ABBDORS9T9T5T",
   "WXX9W9W3VW9V3VUV9UUTT3SRRQQPP",
   "Q3RSS4RQPRQPPONLKKIHH4GFF5GF9G7H3I4HI",
   "6I6JIIJJ3IJIJJKKJJKKJIJ4KLL6KJJ3I4JKKLMMLJ",
   "GD9ABCDLRS9T9T5TWX9W9W",
   "W9V3VU5V9UUTT3S3R3Q4RSS4R5QPONKM",
   "LJHH9G9G3G8HII6H4IJJK5J5IJJ",
   "IJJ3K4JI4J6K4J4I4JKLL3MKGD9ABCDKQR3T",
   "9T9TTTXX9W7WVVW9V9VU",
   "9UUT4SRR3QRRSRSS3R5QPPOMKKLIHHGHH9G4G",
   "6GHGHG5HIJ3HII6JII6JI3JI9J7JKKJ",
   "4J3IHIJJ3KLLNMMKHE9ABCEGNRS9T9T4T",
   "X9W9WW9V4VUUVV4UV9U5SRRQQ",
   "RR5SRR5Q3PN3KJIHHI4H8GHHG4H3GHGHHIHIHIIHI",
   "JIJI9J4I9J9J9J5IJ5K4MK",
   "HE9ABCDFKQS9T9T4TXX9W9W",
   "9V8V4UV9UT4SRRQ3R5S3RQPQPOPPNKJ",
   "ILKKIIJJIIG8HG5H3G4HIHIIHHI3JII9JIIJ4I",
   "9JIJI9JJ7IJ5KMMLLKHE9ABCCEIPSTT",
   "9T9TTT9W9WVVW9V4VUUV3U",
   "9UUUTT4S4R7S3RQ3POOM3NPPOOQLKLJHII7H",
   "8HIJJIHHIIJII3J3IJKK9JII4JIJJ3I9JJJ",
   "7IJJ5K5LKHE9ABBCDEJRS9T9T3T",
   "9W6WVWW9V9VV9U5UTT4S3R",
   "R7S3RQPPOOP3O3QONOLLKJIJJ9H3H4I3J3IJ3IJ",
   "JJIJJKK9J3JIIJJH5JI6JIJ9IJ6K6L",
   "IE9ABBCDEHRS9T9T3TXWX9W3WV4W",
   "W9V4VUV9U7UTT3S5R6S3RQPOO4PQ",
   "QQ4PONLK3J3I6H7I5JIJIHHI5JKK9J4J",
   "3I6JH3I3JII7HIIJ6K5LKIF9ABCCDDGQRS",
   "9T9TTT9W9WW9V4VU3V4U",
   "9U3UTT4S5R5S3RQ6PQRR3PQPPLMLKJJ5IHH",
   "HHJII3JI4JIJI9J6JKK4JIJJIJIH4IJ9IHH",
   "HHIHIJJKLL3K3LK3LJF8ABC3DEEMQR9T9TTT",
   "9W9W9V7VUVUVV9U6UT4SR",
   "RR6SRSSQPPO5QRQP3QPML3KJJ4I4HJI4JI4J3I4J",
   "JJIIJJI9JJI3JIJ9I3I6HI3HJKLML3K6LK",
   "JF8AB3D3EJPR9T9TTT9W9WVV",
   "9VVUVV5UV9U6UTSSRSSR7SRSRPPQP3QRQ",
   "R4QPNMLKKJJ3IHI3HGHIJJIIJJI3JIIJJK3JHII8JIJJIIJJ",
   "IIJJH5I9HHIIHHIJKKLKKLK3LKLLKJF8AB3D3EHNQ",
   "S9T9TTX9W9WW9VVU5V4U",
   "9U6UTT3S3RSTT3SRQO3QRR7QPONOML3J3I3HI",
   "I3HIJJI3JIIK8J5I4JIJI3JI3HII9H3HGG3H",
   "HHIHJKJKKLKK7LKJF7ABCDD3EFFHNRS9T9T",
   "X9W8W9V8V9U7UTTU3T3S",
   "RR7SR3QRQRQQP5QP3MLK3JII4HIHIIHIJJ6IKKJIJJI",
   "JJIJJ3I5J5I9HIH3G3HGG3HIIHIJJ5KLLMLKKLKJ",
   "IF7ABCD4EFFGKRSS9T8T9W9WVV",
   "9V7V9U7UTTUUT3SRR7SRRQ4R4Q",
   "R3QN3LKJ4I8H3I3JIJJI9J4JIHIIH3JIJIIHIH",
   "6HGHH9G3HIHIJ5K4LKKMKKJHF7ABC3DEE3FI",
   "RSS9T8T9W9W9VU5V7U",
   "9U5U5TS3R6SRS3RQR7QPMLKJKJ4I6H",
   "H3I7JIJ3I3JI3JKJIHHJJI3JIHI7H9GGG5H",
   "HIIJ7KL5KLKJHFB6ABCDD4E3FPRS9T8T",
   "9W6WVWW9V6V9U9UTTU6T",
   "3R8S4R8QPMLK4JII7H4IJJIIJ3IJI6J",
   "5JIHI3JIIJI9H9G3G4HIIJJ6KLL4KLKKI",
   "HFB6ABC3DEE4FMQR9T8T9W6WVWWVV",
   "6VUV3UVV9U9U9TSRR3ST4S4R5Q",
   "Q3PLK6JI6H5IJ3IJH3IJI5JI5J3IJIHI7H",
   "9G7GHGHIH3J9KKLKLLKJHFB6ABC3DEE4F",
   "JPR9T8TWX9W4WVWW9VU3V9U",
   "6UT4U8TSRRQRR7SRRQQOQPPQPONK4JKJJI4HIHH",
   "4HIHHJJ3I3HI3J5I4JIIJ3I3H9G3GF5G5H",
   "IJJ9KKKLL3KHGFB6ABBCDDEEFFEEFMQS9T7T",
   "9W9W9VVV4UV9U7U8TSS",
   "QPMQRR6SRRQPL5PNLKJJIJJII6HIHHIHIIHIJHH3IHHIJIJI",
   "JJIIJ8I3H9GGGFF6G3H3IJJ8KLKKL3KJH",
   "GFB6ABC4D6EJPS9T7T9W4W7V",
   "9VV9U9UTU7TSSRMJGMRR6S3RNMQPP",
   "POMLKJIIJJ9HIHIHHIHIIHH4IHIJIIJJI3J3IHII4H4GF",
   "F4G4F5GHIHHII3J9K5KJIHGFB6ABC5D4E",
   "DHOS9T7T9W3W9V6V3UV9U",
   "4UTUU9TSRPKFEDDFJR5S3R4POO3K3J3IHG3HGHGGH",
   "HHIHHIHHGHIH5IJJKI4JH3IJI3H4G9F6G3HIJII",
   "I3J9K3KJJHGGEB5ABCD4ED3EDCDJRS9T6T",
   "9W3W9V9V9U5U4TU5TSSROKH",
   "EDDCCEP5S3RQ3PNNKKLIIJI3H6G9H3H6IH3JI",
   "3IJJHHI4H3G9F4F6GH3I6J9K3KJJHG",
   "FEB5ABCD3EDD3EDCDGRS9T6T9W3W8V",
   "9VV9UUUTUU6TU3TSRQJGFD5CHPT3S3RQQPOM",
   "MKKLIIJHHGH6G9H5HIIH4IJJ3IJII3H5G4FGF",
   "8F5G3HIJJI3J9K4KJIHGFDB5ACDD8ED",
   "CCFQRS9T5T9W3W9VV3UVV9U4U",
   "UUTTU6TSSRNKHEDD3C5BF4SRR3QMMLLKJJIIHHG4H5GH",
   "3HIHGHG5HG3HJI3JIHI3H3G9FFFG3F6G3H3IJJ",
   "3J5KJ5K3JHGFEDB5A3D9E3CNRS9T5T",
   "9W4W9V3VUV9U7U8TSRQJGE3C",
   "3C4BAB3S3RQPOMNLKKLJIHHGGHGGHHG8H5GHHGHHG3HII",
   "3I3H5G9FF3GF7G3H3I6JK5J4KJJIHGE",
   "DC5AB3D8ED3CKQR9T5T9W4W7V",
   "4V9U9U9TRQMFDDCD7B3APTS3RQPNLN",
   "3LKI3HGGH5G5HG3H7GHHG3H3IHI4HGG9FFF",
   "3F8G3HIIHIJ3IJJ3K3J3K3JHHGEDC5AB3D8ED",
   "BBCIPR9T5T9WWWVW9VUUV9U6U",
   "UU7TSSPJFC8B6ACJ4RQOMNMJKK3H4GFF7GH",
   "GG3H7G5HIHG3HGH3GFFEE9FF7GHGH3IHIIJI",
   "I3J3KJ5KIIHGGFDCB5AB3DEED4EDDBBCEKQS9T4T",
   "9WWWVW9VUUV9U3UTT3U6TSSRKGD6B",
   "3B6ABFORRQPONMLKKJI3H3GFF9GG4H7G3HGHHG",
   "3H4GFFEE9FFF8G4H5IJI6J5KJIIHGGFD",
   "BB5ABCCDEED4EDDBBCEHPS9T4T9WW9VV",
   "VVUUV9U3U9TTTSRQGEC7B9AEJRRQQPNKK",
   "LKJIHH4G3F9G8HGH5GH9GFFEE9F",
   "F8G3H7IKI6J4KJJIHHGFEDBB5ABBCC3D3E3D",
   "3BDGPSS9T3T9WW9V3V9U6U3T",
   "8TRPJCC6B9AAADGQRRQPOKLKJJHGGF3G6FGGFF",
   "9GHH9G4G3FE9F4F7G3HI5JIJJ",
   "6JK6JIHHFEDCB5A3B3CD4EDD3CBDELRS9T3T",
   "9WWW9VVV9U4U9T3TSQLGC3BC3BA",
   "9AADGPQRQPPKLKIIHGGF3GFFEE8F6G5H7G",
   "FGG3FE5FE8FGF6GHHI7JKK3J3K6JHHGFEDB",
   "B5A3B4CDDEEDDCCBBDEKRSS9TTT6WVWW9VVV",
   "V9U4U9T3TSRMHFCBB3CB9A3ACFPQRQPPLK",
   "JIHHGGF3GFF4E6F6G3H8GFFG3FE9F5F",
   "FGF4G3HII6J3K3J3K6JHGGEEDB5A6BCCDEEDDCC",
   "BCBDFJQRSS9TT9W9VUVV9UU9T",
   "5TSRPGE4CDCC9A4ABELP3QPMNKIHGG8F3E4F",
   "4F7GHGFFGG5FEE9F7F7G3HIJ3I5J",
   "3K8JIHGFEDCC6ABC4B3CEED5CBEGHKNQRSS8T",
   "6W9V3VUUV9U9T6TRQLE7CB3A",
   "9AABEKNP3QMLJGHHGG7F3E9FFG3FGG8F",
   "3E5FE9F4F4G3HIIJII6JKK8JIHHFFEDCBA",
   "4ABEEDCBB3CEDDC4BCFGHHILQRRSS6T5WVWW9V3V",
   "V9UT3U9TSSQOID7CB9A4ABDJMO3QPN",
   "L3G8F4E3FEFE6FG4FE4F4E6FE9FFF",
   "FF4G3HIIJHI6JKK6JIJHGGFEECBB4ABBEFFDC3BCEDD3B",
   "CCBFHGGFGLPQ3S5T3W9V8V9UU9TS",
   "SSRRQKGECBB4CBB9A4ABDHJLPPQ3RQLGGFEF8ED3E",
   "ED9E9E4E9F7F4G3H7I4JKK",
   "7JIIHGGFEDDBB4A4BCEEC3BCDDC4BCFGHGFEEFGINPR3STT",
   "WVV3WVVW6VUUVVUUV6U9TSS3RQPOHEC4BCC3B3A",
   "9AABCGILOPPQ4RQHF4EDEE9DDED7EDD6E",
   "4E3FE9F3F3G3H5IJII4J3K6JIIHIFFEDCBBAA",
   "AA5BCED3BCDDC4BCEGHGEDDEEFILPQRRSTW9V3VUU3VUV",
   "6U9TSSRRQPONMKFD5BCC3B9A4ABCGHJMOPQQ",
   "4RPLJFEEDE9D3DE8D9EEE3FE9FFGG",
   "3G3H4IJJII3JKK8JIHGFEEDCCBB4A5BCDC3BCDDC3B",
   "BCEGHG4DEEGHJOQRRSW9V3V5U3T9SS4RQPMKH",
   "GGIJHEC8B9A6ABCFGHKMMPPQ4RQPOMLJHGF6E",
   "3DED9E8EFEFE9FFF5GH3I6J3I5J",
   "4JIHHGGFEEDCCBB5A5BCDC3BCDCCA3BDEHGE3BCCDEFFGHJMP",
   "9VV3U5T6S8RQPOMJHGEEF3HDC6B7A",
   "9AAACFGHKKLNOP5RQPNMKJI3F7EDEEG9E4E",
   "EEF5E9F3G3HI6J3I9JJIHHGGFFEDDCBB4A",
   "A6BCC5BCCBABBCEFHFD3ABBCDE4FHJ8VUU4T4SRR",
   "R9QPPNKJHFEDCDFHHGEDC4B9AAAB7ABEFGJJKMM",
   "PQ5RQPOMKKHGF4ED9E9E6EFFE6F6G",
   "3HI9J3JK5JIIHGGFFEDDCCBB3A8BCC5BC4BA",
   "CEFGFD5ACDDE4FGVV3U3TSS3R3QPOLKJ4H5I3GEDCBAAB",
   "EGIHFEDDCBB9AA3B7ABDEFHIJKKLPQ5RQQPMMKJHFFEE",
   "EED9E7E9F3F7GHH4I6JI6JIJJ",
   "JIHHGGF3EDCCBBAABB4C9BB3CBAABDEFFDB6ABCDDEEDD",
   "3UTTSSRR3QNMLKJIHG6FGG3FEECBB3ACFHIGF3DCCB9A",
   "A3B7ABDEFGHIJKKNPQRRSRRQQPNLKJGFF9E9E3F",
   "9F9GHH6I5JI9J3HGFFEEDCCBB3ABCD",
   "DDCC4BCC7BABAEEFEDB7ABC4DC3TSSRQQPMKJHHGGFFEE",
   "3DED6EDD3B3ADGIHGF3DCCB9A4B7ABDEFGHHJJ",
   "JLOQ6RQPOMLKHGF9E8E9F3F9G3H",
   "6I9JJ3I3HGG3FEDDCCB3ABCCDEDDCC3BCC7BAAB",
   "EFGEDB7ABB4DCRQQNKJGFEDCC8BC6DCC4DCC4BF",
   "HIGED6C8A4B8ABDEEFGGHIJKLLQ6RQQOMKHHGF",
   "9E3E7FGF9GGGHH3G4H5I6JI4JIHH",
   "3GF3EDDCCBBAABCDD3EDDCBBA3C6BACCFGGEC7B3ABCCDD",
   "NLKHGFDCBB5A5BCDEDE5D3EDC3BCGIIFEDCC4B8AB",
   "BB9ABDEEFGGHHJIJKPQ6RQPNLIIHFF9E8FGG",
   "F9GG4HGGHI3HIIHHI4JII3JIHH4GFEEDDCC3BAABCDDE",
   "EEDCBBAB3C5BAAFEFGGD3C8BABCDDJHGEDCB9A3BC",
   "C3EDEDD5EDCCBCCHIJFEDC5B9ABB9ACDEEF3GH",
   "IHIJMPQ6RPOMJIHGFF8E6F9GGGHHG9HH",
   "4IHII3J3IJJI3HGGFFEEDD3CBBAABB3D3EDC4BCDC4B3ADE",
   "GGFDD9CBAABDDF3EDCB9ABBCEE3FEEDD5EDD3CEH",
   "IHEDC3B9A9A4ABCDEEFF3GHHIJKLNQ5RQPPKJIG",
   "G9FF4GF3G4HGHGG9HHHIIHH9IJHIHH3GF",
   "F3EDDCC3BAABCDDEE3DCBBAABCCDB5ACFFHGE5D3C4DC3BC",
   "F3EDDBB7ABBCEF3GF3EDD3ED3CBCEIJIDCC3B9A3A",
   "9AABCD3EFF3GHHIJJLQQ4RQQPLKJHGG9F9G",
   "3H3G9HHHI3H4IH4IJIHI4GFF3EDDCC6BCCD3ED",
   "DDBB3ABBCC6ADEGGFE4D5C3DCBBABF3EDDCBB6ABCDFG",
   "GGFF3E5DCC3BCFJJHDC4B9A9A4ABCD3EFFGG",
   "G3HIJKPQ4RQQPMKJH3G6F9G3GHHGHG9H5H",
   "4IJI3H3IH4GFF4ED3C6BCDD3EDDCBBAABCBCCB5AEFG",
   "GFE4D4C3DEDCBAA5DCC7BCDEF3GFFEEDDCC3B4ABCGK",
   "KHD4CB9A9A3ACBCD3EFF5G3HJLNP6QNLKH",
   "H3G3F6G9HGG9H4H7I3HIHH5GFF3E",
   "EDD4CBAA3B3C4DCC5BABCC5ACFGGFE5D4C4DCCBAA",
   "6C6BCCDEFFGFFEE3DC3B6ABDHKKGDCCDDC9A3A",
   "9AACCDEE3F7GHIKLN6QOMKIHH9G9H",
   "9H9HI5HII4HGGFGFF3E3DCCBCBBAABB5CDDC",
   "CC3BAABBCB4ABDFHHE6D9CCBB9A9A3A"
} ;

#define NRRR 1049  /* number of rows above */

void RWCOX_popper(void)
{
   static void * handle = NULL ;
   PLUGIN_impopper * ppp ;
   MRI_IMAGE * imcox ;
   int ii , jj , cc,rr , dd,ee ;
   char   bb ;
   byte * bcox  ;

   if( ! PLUTO_popup_open(handle) ){
      imcox = mri_new( NX_COX , NY_COX , MRI_byte ) ;
      bcox  = MRI_BYTE_PTR(imcox) ;
      mri_add_name( "Robert W. Cox" , imcox ) ;

      cc = rr = 0 ;
      for( ii=0 ; ii < imcox->nvox && rr < NRRR ; ){
         bb = bccc[rr][cc++] ; if( bb == '\0' ) break ;
         if( bb >= 'A' && bb <= 'Z' ){
            jj = bb - 'A' ; bcox[ii++] = mapcox[jj] ;
         } else {
            dd = bb - '0' ; bb = bccc[rr][cc++] ; if( bb == '\0' ) break ;
            jj = bb - 'A' ;
            for( ee=0 ; ee < dd && ii < imcox->nvox ; ee++ )
               bcox[ii++] = mapcox[jj] ;
         }
         if( bccc[rr][cc] == '\0' ){ cc = 0 ; rr++ ; }
      }

      handle = PLUTO_popup_image( handle , imcox ) ;
      mri_free(imcox) ;

      ppp = (PLUGIN_impopper *) handle ;
      drive_MCW_imseq( ppp->seq, isqDR_imhelptext,
                       (XtPointer) " \n Born September 1954 in Wallace, Idaho. \n"
                                   " Hobbies: Star Trek, Hiking, Skiing.\n"
                                   " Favorite Bagel: Pumpernickel.\n"
                                   " Favorite Authors: Richard Posner,\n"
                                   "    Robert Heinlein, JRR Tolkien, ....\n"
                                   " \n"
                                   " `Beyond all hope, set free to light.'\n"
                     ) ;
   } else {
      ppp = (PLUGIN_impopper *) handle ;
      if( ISQ_REALZ(ppp->seq) )
         XMapRaised( XtDisplay(ppp->seq->wtop) , XtWindow(ppp->seq->wtop) ) ;
   }
   return ;
}
#endif /* WANT_RWCOX_IMAGE */
/*====================================================================================*/
#ifdef USE_SKIT
#define NSKIT 50
static char * skit[NSKIT][3] = {
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

void SKIT_popper(Three_D_View * im3d)
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
