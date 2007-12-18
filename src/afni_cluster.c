#undef MAIN
#include "afni.h"

static void AFNI_cluster_textkill( Three_D_View *im3d ) ;
static void AFNI_cluster_textize( Three_D_View *im3d , int force ) ;
static void AFNI_cluster_widgkill( Three_D_View *im3d ) ;
static void AFNI_cluster_widgize( Three_D_View *im3d , int force ) ;
static void AFNI_clus_update_widgets( Three_D_View *im3d ) ;

/*****************************************************************************/
/*************  Functions for all actions in the cluster group ***************/

/*---------------------------------------------------------------------*/
/* Put a '*' next to the active item in the vedit list on the menu.    */

static char *clubutlab[] = { " Clear Edit" ,
                             " Clusterize"  } ;
#define NTHRBUT (sizeof(clubutlab)/sizeof(char *))

void set_vedit_label( Three_D_View *im3d , int ll )
{
   char lab[64] ;
   if( !IM3D_OPEN(im3d) ) return ;

   strcpy(lab,clubutlab[0]); if( ll==0 ) lab[0] = '*' ;
   MCW_set_widget_label( im3d->vwid->func->clu_clear_pb , lab ) ;

   strcpy(lab,clubutlab[1]); if( ll==1 ) lab[0] = '*' ;
   MCW_set_widget_label( im3d->vwid->func->clu_cluster_pb , lab ) ;

   return ;
}

/*---------------------------------------------------------------*/
/* Callback from the clusterize parameter chooser.               */

static void AFNI_cluster_choose_CB( Widget wc, XtPointer cd, MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   float *vec = (float *)(cbs->cval) , rmm,vmul ;

ENTRY("AFNI_cluster_choose_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   rmm = vec[0] ; vmul = vec[1] ;
   if( vmul <= 0.0 ){
     im3d->vedset.code = 0 ;
     AFNI_vedit_clear( im3d->fim_now ) ;
     set_vedit_label(im3d,0) ; VEDIT_unhelpize(im3d) ;
   } else {
     im3d->vedset.code     = VEDIT_CLUST ;
     im3d->vedset.param[2] = rmm ;
     im3d->vedset.param[3] = vmul ;
     set_vedit_label(im3d,1) ;
   }
   if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------*/
/* Callback for items on the clu_label menu itself.              */

void AFNI_clu_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

ENTRY("AFNI_clu_CB") ;

   if( !IM3D_OPEN(im3d) || w == NULL ) EXRETURN ;

   /*--- Clear editing ---*/

   if( w == im3d->vwid->func->clu_clear_pb ){
     im3d->vedset.code = 0 ;
     AFNI_vedit_clear( im3d->fim_now ) ;
     set_vedit_label(im3d,0) ; VEDIT_unhelpize(im3d) ;
     AFNI_cluster_dispkill(im3d) ;
     if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;
     EXRETURN ;
   }

   /*--- Get clusterizing parameters ---*/

   if( w == im3d->vwid->func->clu_cluster_pb ){
     char *lvec[2] = { "rmm " , "vmul" } ;
     float fvec[2] ;
     if( im3d->vedset.code == VEDIT_CLUST ){
       fvec[0] = im3d->vedset.param[2]; if( fvec[0] <= 0.0f ) fvec[0] =  0.0f;
       fvec[1] = im3d->vedset.param[3]; if( fvec[1] <= 0.0f ) fvec[1] = 20.0f;
     } else {
       fvec[0] = 0.0f ; fvec[1] = 20.0f ;
     }
     MCW_choose_vector( im3d->vwid->func->thr_label ,
                       "------ Set Clusterize Parameters ------\n"
                       "* rmm=0 is nearest neighbor clustering\n"
                       "  with vmul is cluster volume threshold\n"
                       "  measured in voxel count\n"
                       "* rmm>0 is clustering radius in mm, and\n"
                       "  vmul is cluster volume threshold in\n"
                       "  microliters\n"
                       "* Use 'BHelp' on 'Cluster Edit' label\n"
                       "  to get summary of clustering results.\n"
                       "---------------------------------------"
                       , 2 , lvec,fvec ,
                        AFNI_cluster_choose_CB , (XtPointer)im3d ) ;
     EXRETURN ;
   }

   EXRETURN ;  /* should be unreachable */
}

/*---------------------------------------------------------------------------*/

void AFNI_cluster_dispkill( Three_D_View *im3d )
{
   if( AFNI_yesenv("AFNI_CLUSTER_TEXTWIN") )
     AFNI_cluster_textkill(im3d) ;
   else
     AFNI_cluster_widgkill(im3d) ;
}

void AFNI_cluster_dispize( Three_D_View *im3d , int force )
{
   if( AFNI_yesenv("AFNI_CLUSTER_TEXTWIN") )
     AFNI_cluster_textize( im3d , force ) ;
   else
     AFNI_cluster_widgize( im3d , force ) ;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

static MCW_textwin *clu_twin[MAX_CONTROLLERS] ;

static void AFNI_cluster_textkill( Three_D_View *im3d )
{
   int ic ;

ENTRY("AFNI_cluster_textkill") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   ic = AFNI_controller_index(im3d) ;
   if( ic < 0 || ic >= MAX_CONTROLLERS ) EXRETURN ;
   if( clu_twin[ic] != NULL ){
     XtDestroyWidget(clu_twin[ic]->wshell); myXtFree(clu_twin[ic]);
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_cluster_textdeath( XtPointer cb )
{
   int ic = (int)cb ;
   if( ic >= 0 || ic < MAX_CONTROLLERS ) clu_twin[ic] = NULL ;
/** fprintf(stderr,"AFNI_cluster_textdeath(%d)\n",ic) ; **/
   return ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_cluster_textize( Three_D_View *im3d , int force )
{
   int nclu , ic , ii ;
   char *msg , *rpt , line[80] ;
   float px,py,pz ;
   mri_cluster_detail *cld ;

ENTRY("AFNI_cluster_textize") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   ic = AFNI_controller_index(im3d) ;
   if( ic < 0 || ic >= MAX_CONTROLLERS ) EXRETURN ;
   if( !force && clu_twin[ic] == NULL ) EXRETURN ;

   nclu = im3d->vwid->func->clu_num ;
   cld  = im3d->vwid->func->clu_det ;
   if( nclu == 0 || cld == NULL ){
     AFNI_cluster_textkill(im3d) ;
     EXRETURN ;
   }

   if( nclu > 22 ) nclu = 20 ;  /* don't get carried away */
   msg = (char *)malloc(sizeof(char)*(nclu*99+999)) ;
   rpt = im3d->vwid->func->clu_rep ;  /* summary report */
   if( rpt != NULL ) strcpy(msg,rpt) ;
   else              strcpy(msg,"Cluster Report\n") ;
   strcat(msg ,
           "----------------------------------\n"
           "##  --Size-- ---Peak DICOM xyz---\n"
         ) ;

   for( ii=0 ; ii < nclu ; ii++ ){
     MAT44_VEC( im3d->fim_now->daxes->ijk_to_dicom ,
                cld[ii].xpk,cld[ii].ypk,cld[ii].zpk , px,py,pz ) ;
     if( cld[ii].nvox <= 99999 )
       sprintf(line,"%2d:%5d vox %+6.1f %+6.1f %+6.1f\n",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     else
       sprintf(line,"%2d:%9d %+6.1f %+6.1f %+6.1f\n",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     strcat(msg,line) ;
   }

   if( clu_twin[ic] == NULL ){
     clu_twin[ic] = new_MCW_textwin_2001( im3d->vwid->func->options_label ,
                                          msg , TEXT_READONLY ,
                                          AFNI_cluster_textdeath,(XtPointer)ic ) ;
   } else {
     MCW_textwin_alter( clu_twin[ic] , msg ) ;
   }
   free((void *)msg ) ;
   EXRETURN ;
}
/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

void AFNI_cluster_EV( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   if( AFNI_yesenv("AFNI_CLUSTER_TEXTWIN") )
     AFNI_cluster_textize((Three_D_View *)cd , 1 ) ;
   else
     AFNI_cluster_widgize((Three_D_View *)cd , 1 ) ;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

static void AFNI_clus_make_widgets( Three_D_View *, int ) ;
static void AFNI_clus_done_CB  ( Widget,XtPointer,XtPointer ) ;

static void AFNI_clus_av_CB( MCW_arrowval * , XtPointer ) ;
static void AFNI_clus_action_CB( Widget,XtPointer,XtPointer ) ;

/*---------------------------------------------------------------------------*/
/*! Make the widgets for one row of the cluster display/control panel.
    The row itself will not be managed at this time; that comes later. */

#undef  MAKE_CLUS_ROW
#define MAKE_CLUS_ROW(ii)                                           \
 do{ Widget rc ; char *str[1]={"abcdefghijklmn: "} ;                \
     rc = cwid->clu_rc[ii] =                                        \
         XtVaCreateWidget(                                          \
           "dialog" , xmRowColumnWidgetClass , cwid->rowcol ,       \
             XmNpacking     , XmPACK_TIGHT ,                        \
             XmNorientation , XmHORIZONTAL ,                        \
             XmNadjustMargin , True ,                               \
             XmNmarginHeight , 1 , XmNmarginWidth , 0 ,             \
             XmNtraversalOn , True ,                                \
           NULL ) ;                                                 \
     cwid->clu_lab[ii] = XtVaCreateManagedWidget(                   \
            "menu" , xmLabelWidgetClass , rc ,                      \
            LABEL_ARG("##:xxxxx vox +xxx.x +xxx.x +xxx.x") ,        \
            XmNalignment , XmALIGNMENT_BEGINNING ,                  \
            XmNrecomputeSize , False ,  XmNtraversalOn , True ,     \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     cwid->clu_jump_pb[ii] = XtVaCreateManagedWidget(               \
            "menu" , xmPushButtonWidgetClass , rc ,                 \
            LABEL_ARG("Jump") , XmNtraversalOn , True ,             \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     cwid->clu_gave_pb[ii] = XtVaCreateManagedWidget(               \
            "menu" , xmPushButtonWidgetClass , rc ,                 \
            LABEL_ARG("Aver") , XmNtraversalOn , True ,             \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     cwid->clu_gpc1_pb[ii] = XtVaCreateManagedWidget(               \
            "menu" , xmPushButtonWidgetClass , rc ,                 \
            LABEL_ARG("PC#1") , XmNtraversalOn , True ,             \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     XtAddCallback( cwid->clu_jump_pb[ii],                          \
                    XmNactivateCallback,AFNI_clus_action_CB,im3d ); \
     XtAddCallback( cwid->clu_gave_pb[ii],                          \
                    XmNactivateCallback,AFNI_clus_action_CB,im3d ); \
     XtAddCallback( cwid->clu_gpc1_pb[ii],                          \
                    XmNactivateCallback,AFNI_clus_action_CB,im3d ); \
  } while(0)

/*---------------------------------------------------------------------------*/

static void AFNI_clus_make_widgets( Three_D_View *im3d, int num )
{
   AFNI_clu_widgets *cwid ;
   Widget ww , rc ;
   XmString xstr ;
   char str[32] , *eee ;
   int ii ;

ENTRY("AFNI_clus_make_widgets") ;

   if( !IM3D_OPEN(im3d) || im3d->vwid->func->cwid != NULL ) EXRETURN ; /* bad */

   im3d->vwid->func->cwid = cwid = myXtNew( AFNI_clu_widgets ) ;
   cwid->dset = NULL ; cwid->ignore = cwid->cmode = 0 ;

   /* shell to hold it all */

   sprintf(str,"AFNI Cluster Results %s",AFNI_controller_label(im3d)) ;

   cwid->wtop = XtVaAppCreateShell(
                  "AFNI" , "AFNI" ,
                   topLevelShellWidgetClass , im3d->dc->display ,
                   XmNallowShellResize   , True ,
                   XmNtitle              , str ,
                   XmNmappedWhenManaged  , False ,        /* manage manually */
                   XmNdeleteResponse     , XmDO_NOTHING , /* deletion handled below */
                   XmNkeyboardFocusPolicy , XmEXPLICIT ,
                NULL ) ;
   DC_yokify( cwid->wtop , im3d->dc ) ;
   XmAddWMProtocolCallback(           /* make "Close" window menu work */
           cwid->wtop ,
           XmInternAtom( im3d->dc->display , "WM_DELETE_WINDOW" , False ) ,
           AFNI_clus_done_CB , im3d ) ;

   /* vertical rowcol to hold it all */

   cwid->rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , cwid->wtop ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNspacing      , 0 ,
            XmNadjustMargin , True ,
            XmNtraversalOn  , True ,
         NULL ) ;

   /* top label to say what session we are dealing with */

   xstr = XmStringCreateLtoR( " xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx \n"
                              " xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx \n"
                              " xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx \n"
                              " xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx \n"
                              " xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx "   ,
                              XmFONTLIST_DEFAULT_TAG ) ;
   cwid->top_lab = XtVaCreateManagedWidget(
                    "dialog" , xmLabelWidgetClass , cwid->rowcol ,
                       XmNrecomputeSize , False ,
                       XmNlabelString , xstr ,
                       XmNtraversalOn , True  ,
                    NULL ) ;
   XmStringFree(xstr) ;

   /* Separator from other widgets */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass,cwid->rowcol,
                                      XmNseparatorType   , XmSHADOW_ETCHED_IN ,
                                      XmNshadowThickness , 3 ,
                                   NULL ) ;

   /* horiz rowcol for top controls */

   rc = XtVaCreateWidget(
          "dialog" , xmRowColumnWidgetClass , cwid->rowcol ,
             XmNpacking      , XmPACK_TIGHT ,
             XmNorientation  , XmHORIZONTAL   ,
             XmNadjustMargin , True ,
             XmNtraversalOn , True  ,
          NULL ) ;

   /* Timeseries chooser */

   xstr = XmStringCreateLtoR( "Timeseries Dataset" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->dataset_pb = XtVaCreateManagedWidget(
           "dialog" , xmPushButtonWidgetClass , rc ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
         NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( cwid->dataset_pb, XmNactivateCallback, AFNI_clus_action_CB, im3d );

   /* Ignore chooser */

   cwid->ignore_av = new_MCW_optmenu( rc , "Ignore" , 0,19,0,0 ,
                                      AFNI_clus_av_CB , im3d , NULL,NULL ) ;

   cwid->coord_av = NULL ;  /* not yet implemented */

   /* Done button */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass, rc ,
                                      XmNorientation   , XmVERTICAL    ,
                                      XmNseparatorType , XmDOUBLE_LINE ,
                                   NULL ) ;
   xstr = XmStringCreateLtoR( "Done" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->done_pb =
     XtVaCreateManagedWidget(
           "dialog" , xmPushButtonWidgetClass , rc ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
         NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( cwid->done_pb, XmNactivateCallback, AFNI_clus_done_CB, im3d );
   MCW_set_widget_bg( cwid->done_pb, MCW_hotcolor(cwid->done_pb), 0 ) ;

   XtManageChild(rc) ;

   /* Separator from other widgets */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass,cwid->rowcol,
                                      XmNseparatorType   , XmSHADOW_ETCHED_IN ,
                                      XmNshadowThickness , 3 ,
                                   NULL ) ;

   /* Now create rows of widgets to display results from clusters */

   cwid->nall = num ;
   cwid->nrow = 0 ;     /* none are managed at this time */

   cwid->clu_rc      = (Widget *) XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_lab     = (Widget *) XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_jump_pb = (Widget *) XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_gave_pb = (Widget *) XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_gpc1_pb = (Widget *) XtCalloc( num , sizeof(Widget) ) ;

   for( ii=0 ; ii < num ; ii++ ){ MAKE_CLUS_ROW(ii) ; }

   XtManageChild( cwid->rowcol ) ;
   XtRealizeWidget( cwid->wtop ) ; NI_sleep(1) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_cluster_widgize( Three_D_View *im3d , int force )
{
   if( !IM3D_OPEN(im3d) ) return ;
   if( !force ){
     if( im3d->vwid->func->cwid == NULL ) return ;
     if( !MCW_widget_visible(im3d->vwid->func->cwid->wtop) ) return ;
   }
   AFNI_clus_update_widgets( im3d ) ;
   if( im3d->vwid->func->cwid != NULL ){
     XtMapWidget( im3d->vwid->func->cwid->wtop ) ;
     XRaiseWindow( XtDisplay(im3d->vwid->func->cwid->wtop) ,
                   XtWindow (im3d->vwid->func->cwid->wtop)  ) ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_cluster_widgkill( Three_D_View *im3d )
{
   if( !IM3D_OPEN(im3d) || im3d->vwid->func->cwid == NULL ) return ;
   XtUnmapWidget( im3d->vwid->func->cwid->wtop ) ;
   return ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_clus_update_widgets( Three_D_View *im3d )
{
   AFNI_clu_widgets *cwid ;
   char *rpt , *rrr ;
   mri_cluster_detail *cld ;
   int nclu , ii ;
   float px,py,pz ;
   char line[128] ;

ENTRY("AFNI_clus_update_widgets") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   nclu = im3d->vwid->func->clu_num ;
   cld  = im3d->vwid->func->clu_det ;
   rpt  = im3d->vwid->func->clu_rep ;
   if( rpt == NULL || *rpt == '\0' ) rpt = " \n --- Cluster Report --- \n " ;

   if( nclu == 0 || cld == NULL ) EXRETURN ;
   if( nclu > 17 ) nclu = 15 ;  /* don't get carried away */

   cwid = im3d->vwid->func->cwid ;
   if( cwid == NULL ){
     AFNI_clus_make_widgets( im3d , nclu ) ;
     cwid = im3d->vwid->func->cwid ;
   }

   STATUS("set top_lab") ;
   rrr = malloc(strlen(rpt)+8) ; strcpy(rrr," \n") ; strcat(rrr,rpt) ;
   MCW_set_widget_label( cwid->top_lab , rrr ) ; free(rrr) ;

   /* make more widget rows? (1 per cluster is needed) */

   if( cwid->nall < nclu ){
     STATUS("make rows") ;
     cwid->clu_rc     =(Widget *)XtRealloc((char *)cwid->clu_rc     ,nclu*sizeof(Widget));
     cwid->clu_lab    =(Widget *)XtRealloc((char *)cwid->clu_lab    ,nclu*sizeof(Widget));
     cwid->clu_jump_pb=(Widget *)XtRealloc((char *)cwid->clu_jump_pb,nclu*sizeof(Widget));
     cwid->clu_gave_pb=(Widget *)XtRealloc((char *)cwid->clu_gave_pb,nclu*sizeof(Widget));
     cwid->clu_gpc1_pb=(Widget *)XtRealloc((char *)cwid->clu_gpc1_pb,nclu*sizeof(Widget));
     for( ii=cwid->nall ; ii < nclu ; ii++ ){ MAKE_CLUS_ROW(ii) ; }
     cwid->nall = nclu ;
   }

   /* map or unmap widget rows? */

   if( cwid->nrow < nclu ){
     STATUS("mapping") ;
     for( ii=cwid->nrow ; ii < nclu ; ii++ )
       XtManageChild( cwid->clu_rc[ii] ) ;
   } else if( cwid->nrow > nclu ){
     STATUS("unmapping") ;
     for( ii=nclu ; ii < cwid->nrow ; ii++ )
       XtUnmanageChild( cwid->clu_rc[ii] ) ;
   }
   cwid->nrow = nclu ;  /* # of managed rows */

   /* change labels for each row */

   STATUS("labeling") ;
   for( ii=0 ; ii < nclu ; ii++ ){
     MAT44_VEC( im3d->fim_now->daxes->ijk_to_dicom ,
                cld[ii].xpk,cld[ii].ypk,cld[ii].zpk , px,py,pz ) ;
     if( cld[ii].nvox <= 99999 )
       sprintf(line,"%2d:%5d vox %+6.1f %+6.1f %+6.1f",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     else
       sprintf(line,"%2d:%9d %+6.1f %+6.1f %+6.1f",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     MCW_set_widget_label( cwid->clu_lab[ii] , line ) ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Callback for "Done" button for cluster display panel. */

static void AFNI_clus_done_CB( Widget w , XtPointer cd, XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;

ENTRY("AFNI_clus_done_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ;
   if( cwid != NULL ) XtUnmapWidget( cwid->wtop ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
static AFNI_dataset_choose_stuff cdds = { 0, NULL, NULL } ;

static void AFNI_clus_finalize_dataset_CB( Widget w, XtPointer cd, MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   int ival , vv ;
   THD_3dim_dataset *dset ;

ENTRY("AFNI_clus_finalize_dataset_CB") ;
   if( !IM3D_OPEN(im3d) || cbs == NULL ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   ival = cbs->ival ;
   if( ival < 0 || ival >= cdds.ndset ) EXRETURN ;
   dset = cdds.dset[cdds.ndset] ; if( !ISVALID_DSET(dset) ) EXRETURN ;
   cwid->dset = dset ;
fprintf(stderr,"Chosen = %s\n",DSET_HEADNAME(dset)) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_clus_action_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   int nclu , ii ;
   mri_cluster_detail *cld ;

ENTRY("AFNI_clus_action_CB") ;
   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   if( w == cwid->dataset_pb ){
     int vv = im3d->vinfo->view_type ;
     THD_3dim_dataset *dset ;

     if( cdds.dset != NULL ) free((void *)cdds.dset) ;
     cdds.ndset = 0 ;
     cdds.dset = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)
                                             *im3d->ss_now->num_dsset  ) ;
     cdds.cb = AFNI_clus_finalize_dataset_CB ;
     for( ii=0 ; ii < im3d->ss_now->num_dsset ; ii++ ){
       dset = im3d->ss_now->dsset[ii][vv] ;
       if( ISVALID_DSET(dset) && DSET_NVALS(dset) >= cwid->ignore+3 )
         cdds.dset[cdds.ndset++] = dset ;
     }
     AFNI_choose_dataset_CB( cwid->top_lab , im3d , &cdds ) ;
     EXRETURN ;
   }

   /*-- scan button list --*/

   nclu = im3d->vwid->func->clu_num ;
   cld  = im3d->vwid->func->clu_det ;
   if( nclu == 0 || cld == NULL ) EXRETURN ;

   for( ii=0 ; ii < nclu ; ii++ ){

     if( w == cwid->clu_jump_pb[ii] ){
       float px,py,pz ;
       MAT44_VEC( im3d->fim_now->daxes->ijk_to_dicom ,
                  cld[ii].xpk,cld[ii].ypk,cld[ii].zpk , px,py,pz ) ;
       AFNI_jumpto_dicom( im3d , px,py,pz ) ;
       EXRETURN ;

     } else if( w == cwid->clu_gave_pb[ii] ){
       fprintf(stderr,"** Not implemented **\n") ; EXRETURN ;
     } else if( w == cwid->clu_gpc1_pb[ii] ){
       fprintf(stderr,"** Not implemented **\n") ; EXRETURN ;
     }

   }

   fprintf(stderr,"** Unknown button? **\n") ; EXRETURN ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_clus_av_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;

ENTRY("AFNI_clus_av_CB") ;
   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   if( av == cwid->ignore_av ){
     cwid->ignore = av->ival ; EXRETURN ;
   }

   fprintf(stderr,"** Unknown button? **\n") ; EXRETURN ;
}

/*---------------------------------------------------------------------------*/

/*****************************************************************************/
/************  Functions for all actions in the thr_label popup **************/

/*-----------------------------------------------------------------
  Event handler to find #3 button press for thr_label popup;
  just pops up the menu for the user's gratification.
-------------------------------------------------------------------*/

void AFNI_thr_EV( Widget w , XtPointer cd ,
                  XEvent *ev , Boolean *continue_to_dispatch )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

ENTRY("AFNI_thr_EV") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   switch( ev->type ){
      case ButtonPress:{
         XButtonEvent *event = (XButtonEvent *) ev ;
         im3d->vwid->butx = event->x_root ;  /* 17 May 2005 */
         im3d->vwid->buty = event->y_root ;
         event->button    = Button3 ;                           /* fakeout */
         XmMenuPosition( im3d->vwid->func->thr_menu , event ) ; /* where */
         XtManageChild ( im3d->vwid->func->thr_menu ) ;         /* popup */
      }
      break ;
   }

   EXRETURN ;
}

void AFNI_thronoff_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   int qq , pp ;

ENTRY("AFNI_thronoff_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   qq = im3d->vinfo->thr_onoff ;
   pp = MCW_val_bbox( im3d->vwid->func->thr_onoff_bbox ) ;

   if( pp != qq ){
     im3d->vinfo->thr_onoff = pp ;
     if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;
   }
   EXRETURN ;
}
