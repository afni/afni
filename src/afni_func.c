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
   03 Dec 1999: print messages about forced adoptions
-------------------------------------------------------------------------*/

void AFNI_force_adoption( THD_session * ss , Boolean do_anats )
{
   int aa , ff , vv , apref=0 , aset=-1 ;
   THD_3dim_dataset * dset ;
   int quiet = (NULL != my_getenv("AFNI_NO_ADOPTION_WARNING")) ; /* 03 Dec 1999 */
   int first = 1 ;

ENTRY("AFNI_force_adoption") ;

   if( ! ISVALID_SESSION(ss) || ss->num_anat == 0 ) EXRETURN ;

   /* find a "preferred" parent (one with the most number of markers set) */

   for( aa=0 ; aa < ss->num_anat ; aa++ ){

if(PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"scanning anat dataset %d for markers",aa) ; STATUS(str) ; }

      dset = ss->anat[aa][0] ;              /* original view */

      if( ISVALID_3DIM_DATASET(dset) &&     /* if a good dataset */
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
      for( aa=0 ; aa < ss->num_anat ; aa++ ){
         dset = ss->anat[aa][0] ;
         if( ISVALID_DSET(dset)    &&
             dset->markers != NULL && dset->markers->numset >= aset ) vv++ ;
      }
   }

   quiet = ( quiet || vv <= 1 ) ; /* be quiet if ordered, or if no alternatives */

if(aset >= 0 && PRINT_TRACING)
{ char str[256] ;
  sprintf(str,"session %s: apref=%d [%s] aset=%d",
          ss->lastname,apref,DSET_HEADNAME(ss->anat[apref][0]),aset) ;
  STATUS(str) ; }

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

         if( !quiet && dset->anat_parent != NULL ){
            if( first ){
               first = 0 ;
               fprintf(stderr,
                       "\n"
                       "Datasets with a 'forced adoption' of anat parent:\n") ;
            }
            fprintf(stderr,
                    " %s gets parent %s\n",
                    DSET_HEADNAME(dset) ,
                    DSET_HEADNAME(dset->anat_parent) ) ;
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

            if( !quiet && dset->anat_parent != NULL && dset->anat_parent != dset ){
               if( first ){
                  first = 0 ;
                  fprintf(stderr,
                          "\n"
                          "Datasets with a 'forced adoption' of anat parent:\n") ;
               }
               fprintf(stderr,
                       " %s gets parent %s\n",
                       DSET_HEADNAME(dset) ,
                       DSET_HEADNAME(dset->anat_parent) ) ;
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

   if( im3d->anat_wod_flag )
      sprintf(ttl , "{warp} %s%s: " ,
              AFNI_controller_label(im3d),GLOBAL_argopt.title_name) ;
   else
      sprintf(ttl , "%s%s: " ,
              AFNI_controller_label(im3d),GLOBAL_argopt.title_name) ;

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
      { Widget www ;
        www = XmFileSelectionBoxGetChild( im3d->vwid->file_sbox ,
                                          XmDIALOG_TEXT ) ;
        if( www != NULL ) MCW_set_widget_bg( www , MCW_hotcolor(www) , 0 ) ;
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
   RWC_visibilize_widget( im3d->vwid->file_dialog ) ; /* 09 Nov 1999 */

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
   RWC_visibilize_widget( im3d->vwid->file_dialog ) ; /* 09 Nov 1999 */

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

void AFNI_modify_viewing( Three_D_View * im3d , Boolean rescaled )
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
   Three_D_View * im3d = (Three_D_View *) cd ;
   THD_3dim_dataset * dset = NULL ;
   THD_dataxes        new_daxes ;
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
   int native_order , save_order ;  /* 23 Nov 1999 */

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
          kk,MRI_TYPE_name[im->kind] , im->nx,im->ny ) ;
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

   tross_Append_History( dset , "AFNI: resampled and rewritten" ) ;
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
      im3d->vinfo->tempflag = 1 ;
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

void AFNI_misc_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View * im3d = (Three_D_View *) cd ;

ENTRY("AFNI_misc_CB") ;

   if( ! IM3D_OPEN(im3d) || w == NULL ) EXRETURN ;

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
         int ii ; Three_D_View * qq3d ;
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
      char * inf ;
STATUS("getting anat info") ;
      inf = THD_dataset_info( im3d->anat_now , 0 ) ;
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

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_func_info_pb ){
      char * inf ;
STATUS("getting func info") ;
      inf = THD_dataset_info( im3d->fim_now , 0 ) ;
STATUS("got func info") ;
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

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_vcheck_pb ){  /* 11 Jan 2000 */
      FILE * fp = popen( "afni_vcheck" , "r" ) ;
      if( fp == NULL ){
         (void)  MCW_popup_message( im3d->vwid->imag->topper ,
                                     " \n"
                                     "* Cannot execute *\n"
                                     "* afni_vcheck!   *\n" ,
                                    MCW_USER_KILL | MCW_TIMER_KILL ) ;
         XBell( im3d->dc->display , 100 ) ;
      } else {
#define ISIZE 1024
         char * info=(char *)malloc(sizeof(char)*ISIZE) ; int ninfo ;
         strcpy(info," \n     Output of Program afni_vcheck  \n"
                        "   ---------------------------------\n \n"   ) ;
         ninfo = strlen(info) ;
         while( fgets(info+ninfo,ISIZE-ninfo,fp) != NULL ){
            ninfo = strlen(info) ;
            if( ninfo >= ISIZE-2 ) break ;
         }
         pclose(fp) ;
         (void) MCW_popup_message( im3d->vwid->imag->topper , info ,
                                   MCW_USER_KILL | MCW_TIMER_KILL   ) ;
         free(info) ;
      }
   }

   /*.........................................................*/

   else if( w == im3d->vwid->dmode->misc_purge_pb ){
      AFNI_purge_dsets( 1 ) ;
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

void RWCOX_popper(void)
{
   AFNI_splashup() ;
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
