#undef MAIN
#include "afni.h"

static void AFNI_cluster_widgkill( Three_D_View *im3d ) ;
static void AFNI_cluster_widgize( Three_D_View *im3d , int force ) ;
static void AFNI_clus_update_widgets( Three_D_View *im3d ) ;
static MRI_IMARR * AFNI_cluster_timeseries( Three_D_View *im3d , int ncl ) ;
static void AFNI_clus_viewpoint_CB( int why, int np, void *vp, void *cd ) ;

#undef  SET_INDEX_LAB
#define SET_INDEX_LAB(iq) AFNI_clus_viewpoint_CB(0,0,NULL,(void *)(iq))

#undef  PEAK_MODE
#undef  CMASS_MODE
#define PEAK_MODE  0
#define CMASS_MODE 1

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

        if( ll == 0 ) NORMAL_cursorize(im3d->vwid->func->clu_cluster_pb);
   else if( ll == 1 ) POPUP_cursorize (im3d->vwid->func->clu_cluster_pb);

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
   if( vmul <= 0.0f ){
     im3d->vedset.code = 0 ;
     AFNI_vedit_clear( im3d->fim_now ) ;
     set_vedit_label(im3d,0) ; VEDIT_unhelpize(im3d) ;
   } else {
     if( rmm <= 0.0f && vmul < 2.0f ){
       vmul = 2.0f ; rmm = 0.0f ;
       MCW_popup_message( im3d->vwid->func->clu_cluster_pb ,
                           "** WARNING **\n"
                           "** With rmm = 0, vmul is min cluster\n"
                           "** size in voxels, and is reset to 2\n "  ,
                          MCW_USER_KILL | MCW_TIMER_KILL ) ;
     }
     im3d->vedset.code     = VEDIT_CLUST ;
     im3d->vedset.param[2] = rmm ;
     im3d->vedset.param[3] = vmul ;  /* other params set in afni.c */
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
                       "* rmm=0 is Nearest Neighbor clustering;\n"
                       "  then vmul is cluster volume threshold\n"
                       "  measured in voxel count\n"
                       "* rmm>0 is clustering radius in mm; then\n"
                       "  vmul = volume threshold in microliters\n"
                       "* Use 'BHelp' on 'Cluster Edit' label\n"
                       "  to get summary of clustering results.\n"
                       "* Right-click on '*Clusterize' to open\n"
                       "  a more complete cluster report panel.\n"
                       "----------------------------------------"
                       , 2 , lvec,fvec ,
                        AFNI_cluster_choose_CB , (XtPointer)im3d ) ;
     EXRETURN ;
   }

   EXRETURN ;  /* should be unreachable */
}

/*---------------------------------------------------------------------------*/

void AFNI_cluster_dispkill( Three_D_View *im3d )
{
   AFNI_cluster_widgkill(im3d) ;
}

void AFNI_cluster_dispize( Three_D_View *im3d , int force )
{
   AFNI_cluster_widgize( im3d , force ) ;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

void AFNI_cluster_EV( Widget w , XtPointer cd ,
                      XEvent *ev , Boolean *continue_to_dispatch )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   XButtonEvent *event = (XButtonEvent *)ev ;
   if( event->button != Button3 ) return ;
   if( im3d->vedset.code != VEDIT_CLUST ) return ;
   AFNI_cluster_widgize(im3d,1) ;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

static void AFNI_clus_make_widgets( Three_D_View *, int ) ;
static void AFNI_clus_done_CB  ( Widget,XtPointer,XtPointer ) ;

static void AFNI_clus_av_CB( MCW_arrowval * , XtPointer ) ;
static void AFNI_clus_action_CB( Widget,XtPointer,XtPointer ) ;

/*---------------------------------------------------------------------------*/
static char *avlab[2] = { "Mean" , "PC#1" } ;

/*! Make the widgets for one row of the cluster display/control panel.
    The row itself will not be managed at this time; that comes later. */

#undef  MAKE_CLUS_ROW
#define MAKE_CLUS_ROW(ii)                                           \
 do{ Widget rc ; char *str[1]={"abcdefghijklmn: "} ;                \
     char *ff = (ii%2==0) ? "menu" : "dialog" ;                     \
     rc = cwid->clu_rc[ii] =                                        \
         XtVaCreateWidget(                                          \
           ff     , xmRowColumnWidgetClass , cwid->rowcol ,         \
             XmNpacking     , XmPACK_TIGHT ,                        \
             XmNorientation , XmHORIZONTAL ,                        \
             XmNadjustMargin , True ,                               \
             XmNmarginHeight , 2 , XmNmarginWidth , 0 ,             \
             XmNtraversalOn , True ,                                \
           NULL ) ;                                                 \
     cwid->clu_lab[ii] = XtVaCreateManagedWidget(                   \
            ff     , xmLabelWidgetClass , rc ,                      \
            LABEL_ARG("##:xxxxx vox +xxx.x +xxx.x +xxx.x") ,        \
            XmNalignment , XmALIGNMENT_BEGINNING ,                  \
            XmNrecomputeSize , False ,  XmNtraversalOn , True ,     \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     cwid->clu_jump_pb[ii] = XtVaCreateManagedWidget(               \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            LABEL_ARG("Jump") , XmNtraversalOn , True ,             \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     cwid->clu_aver_av[ii] = new_MCW_optmenu( rc , "\0" ,           \
            0,1,0,0 , NULL,NULL , MCW_av_substring_CB , avlab ) ;   \
     cwid->clu_plot_pb[ii] = XtVaCreateManagedWidget(               \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            LABEL_ARG("Plot") , XmNtraversalOn , True ,             \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     cwid->clu_save_pb[ii] = XtVaCreateManagedWidget(               \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            LABEL_ARG("Save") , XmNtraversalOn , True ,             \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     XtAddCallback( cwid->clu_jump_pb[ii],                          \
                    XmNactivateCallback,AFNI_clus_action_CB,im3d ); \
     XtAddCallback( cwid->clu_plot_pb[ii],                          \
                    XmNactivateCallback,AFNI_clus_action_CB,im3d ); \
     XtAddCallback( cwid->clu_save_pb[ii],                          \
                    XmNactivateCallback,AFNI_clus_action_CB,im3d ); \
  } while(0)

/*---------------------------------------------------------------------------*/

static void AFNI_clus_dsetlabel( Three_D_View *im3d )
{
   AFNI_clu_widgets *cwid ;
   char *str ;

ENTRY("AFNI_clus_dsetlabel") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   if( !ISVALID_DSET(cwid->dset) )
     str = " [No Timeseries Dataset selected yet] " ;
   else
     str = THD_trailname( DSET_HEADNAME(cwid->dset) , SESSTRAIL+1 ) ;

   MCW_set_widget_label( cwid->dset_lab , str ) ;
   EXRETURN ;
}

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
                    "menu" , xmLabelWidgetClass , cwid->rowcol ,
                       XmNrecomputeSize , False ,
                       XmNlabelString , xstr ,
                       XmNtraversalOn , True  ,
                    NULL ) ;
   XmStringFree(xstr) ;

   /* Separator from other widgets */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass,cwid->rowcol,
                                      XmNseparatorType   , XmSINGLE_LINE ,
                                   NULL ) ;

   /* horiz rowcol for top controls */

   rc = XtVaCreateWidget(
          "menu" , xmRowColumnWidgetClass , cwid->rowcol ,
             XmNpacking      , XmPACK_TIGHT ,
             XmNorientation  , XmHORIZONTAL   ,
             XmNadjustMargin , True ,
             XmNtraversalOn , True  ,
          NULL ) ;

   /* Timeseries chooser */

   xstr = XmStringCreateLtoR( "Timeseries Dataset" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->dataset_pb = XtVaCreateManagedWidget(
           "menu" , xmPushButtonWidgetClass , rc ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
         NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( cwid->dataset_pb, XmNactivateCallback, AFNI_clus_action_CB, im3d );
   MCW_register_hint( cwid->dataset_pb , "data for Plot/Save of cluster average" ) ;

   /* Ignore chooser */

   cwid->ignore_av = new_MCW_optmenu( rc , "Ignore" , 0,19,0,0 ,
                                      AFNI_clus_av_CB , im3d , NULL,NULL ) ;
   MCW_reghint_children( cwid->ignore_av->wrowcol , "TRs to ignore for Plot/Save" ) ;

   /* cmode chooser */

   { static char *clab[2] = { "Peak" , "Cmass" } ;
     cwid->cmode_av = new_MCW_optmenu( rc , "xyz" , 0,1,0,0 ,
                        AFNI_clus_av_CB,im3d , MCW_av_substring_CB,clab ) ;
     MCW_reghint_children( cwid->cmode_av->wrowcol , "Coordinate display type" ) ;
   }

   /* index label */

   (void) XtVaCreateManagedWidget( "menu", xmSeparatorWidgetClass, rc ,
                                      XmNorientation   , XmVERTICAL    ,
                                      XmNseparatorType , XmSINGLE_LINE ,
                                   NULL ) ;
   cwid->index_lab = XtVaCreateWidget(
                      "menu" , xmLabelWidgetClass , rc , NULL ) ;

   /* Done button */

   (void) XtVaCreateManagedWidget( "menu", xmSeparatorWidgetClass, rc ,
                                      XmNorientation   , XmVERTICAL    ,
                                      XmNseparatorType , XmSINGLE_LINE ,
                                   NULL ) ;
   xstr = XmStringCreateLtoR( "Done" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->done_pb =
     XtVaCreateManagedWidget(
           "menu" , xmPushButtonWidgetClass , rc ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
         NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( cwid->done_pb, XmNactivateCallback, AFNI_clus_done_CB, im3d );
   MCW_set_widget_bg( cwid->done_pb, MCW_hotcolor(cwid->done_pb), 0 ) ;

   XtManageChild(rc) ;

   /* Time series dataset label */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass,cwid->rowcol,
                                      XmNseparatorType   , XmSHADOW_ETCHED_IN ,
                                   NULL ) ;
   cwid->dset_lab = XtVaCreateManagedWidget(
                      "dialog" , xmLabelWidgetClass , cwid->rowcol , NULL ) ;
   AFNI_clus_dsetlabel(im3d) ;

   /* Separator from other widgets */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass,cwid->rowcol,
                                      XmNseparatorType   , XmSINGLE_LINE ,
                                   NULL ) ;

   /* Now create rows of widgets to display results from clusters */

   if( num < 1 ) num = 1 ;
   cwid->nall = num ;
   cwid->nrow = 0 ;     /* none are managed at this time */

   cwid->clu_rc      = (Widget *) XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_lab     = (Widget *) XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_jump_pb = (Widget *) XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_plot_pb = (Widget *) XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_save_pb = (Widget *) XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_aver_av = (MCW_arrowval **) XtCalloc( num , sizeof(MCW_arrowval *) ) ;

   for( ii=0 ; ii < num ; ii++ ){ MAKE_CLUS_ROW(ii) ; }
   MCW_register_hint( cwid->clu_lab[0]     , "DICOM coordinates of cluster (Peak or Cmass)" ) ;
   MCW_register_hint( cwid->clu_jump_pb[0] , "Set crosshairs to this point" ) ;
   MCW_register_hint( cwid->clu_plot_pb[0] , "Plot average over cluster of Timeseries Dataset" ) ;
   MCW_register_hint( cwid->clu_save_pb[0] , "Save average timeseries to 1D file" ) ;
   MCW_reghint_children( cwid->clu_aver_av[0]->wrowcol , "Set timeseries averaging method" ) ;

   XtManageChild( cwid->rowcol ) ;
   XtRealizeWidget( cwid->wtop ) ; NI_sleep(1) ;

   AFNI_receive_init( im3d, RECEIVE_VIEWPOINT_MASK,
                      AFNI_clus_viewpoint_CB, im3d, "AFNI_clus_viewpoint_CB" ) ;
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
     im3d->vwid->func->cwid->is_open = 1 ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_cluster_widgkill( Three_D_View *im3d )
{
   if( !IM3D_OPEN(im3d) || im3d->vwid->func->cwid == NULL ) return ;
   im3d->vwid->func->cwid->dset = NULL ;
   AFNI_clus_dsetlabel(im3d) ;
   XtUnmapWidget( im3d->vwid->func->cwid->wtop ) ;
   im3d->vwid->func->cwid->is_open = 0 ;
   DESTROY_CLARR(im3d->vwid->func->clu_list) ;
   return ;
}

/*---------------------------------------------------------------------------*/
/* Get the cluster index of the DICOM coords, if it exists. */

static int AFNI_clus_find_xyz( Three_D_View *im3d , float x,float y,float z )
{
   float xf,yf,zf ; int xi,yi,zi , ii,jj,npt,nclu ;
   MCW_cluster_array *clar ; MCW_cluster *cl ;

   if( !IM3D_OPEN(im3d) || !ISVALID_DSET(im3d->fim_now) ) return -1 ;
   clar = im3d->vwid->func->clu_list ; if( clar == NULL ) return -1 ;
   nclu = clar->num_clu ;              if( nclu == 0    ) return -1 ;

   MAT44_VEC( im3d->fim_now->daxes->dicom_to_ijk , x,y,z , xf,yf,zf ) ;
   xi = rint(xf) ; yi = rint(yf) ; zi = rint(zf) ;
   for( ii=0 ; ii < nclu ; ii++ ){
     cl = clar->clar[ii] ; npt = cl->num_pt ;
     for( jj=0 ; jj < npt ; jj++ ){
       if( xi == cl->i[jj] && yi == cl->j[jj] && zi == cl->k[jj] ) return ii;
     }
   }
   return -1 ;
}

static void AFNI_clus_viewpoint_CB( int why, int np, void *vp, void *cd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   int ncl ;

   if( !IM3D_VALID(im3d) ) return;

   ncl = AFNI_clus_find_xyz( im3d ,
                             im3d->vinfo->xi,im3d->vinfo->yj,im3d->vinfo->zk );

   im3d->vwid->func->clu_index = ncl ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) return ;
   if( ncl < 0 ){
     XtUnmanageChild( cwid->index_lab ) ;
   } else {
     char lab[8] ;
     XtManageChild( cwid->index_lab ) ;
     sprintf(lab,"#%d",ncl+1) ;
     MCW_set_widget_label( cwid->index_lab , lab ) ;
   }
   return ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_clus_makedetails( Three_D_View *im3d )
{
   MCW_cluster_array *clar ;
   int ii , nclu ;

ENTRY("AFNI_clus_makedetails") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   clar = im3d->vwid->func->clu_list ;
   if( clar == NULL ){
     im3d->vwid->func->clu_num = 0 ;
     free((void *)im3d->vwid->func->clu_det); im3d->vwid->func->clu_det=NULL;
   } else {
     im3d->vwid->func->clu_num = nclu = clar->num_clu ;
     im3d->vwid->func->clu_det = (mri_cluster_detail *)
                                 realloc( (void *)im3d->vwid->func->clu_det ,
                                          sizeof(mri_cluster_detail)*nclu    );
     for( ii=0 ; ii < nclu ; ii++ )
       im3d->vwid->func->clu_det[ii] = mri_clusterize_detailize(clar->clar[ii]);
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_clus_update_widgets( Three_D_View *im3d )
{
   AFNI_clu_widgets *cwid ;
   char *rpt , *rrr ;
   mri_cluster_detail *cld ;
   int nclu , ii ;
   float px,py,pz , xx,yy,zz ;
   char line[128] ;
   MCW_cluster_array *clar ;

ENTRY("AFNI_clus_update_widgets") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   clar = im3d->vwid->func->clu_list ;
   if( clar != NULL ){  /* sort and truncate */
     nclu = clar->num_clu ; if( nclu > 16 ) nclu = 15 ;
     SORT_CLARR(clar) ;
     for( ii=nclu ; ii < clar->num_clu ; ii++ ){ KILL_CLUSTER(clar->clar[ii]); }
     clar->num_clu = nclu ;
   }
   AFNI_clus_makedetails( im3d ) ;

   nclu = im3d->vwid->func->clu_num ;
   cld  = im3d->vwid->func->clu_det ;
   rpt  = im3d->vwid->func->clu_rep ;

   cwid = im3d->vwid->func->cwid ;
   if( cwid == NULL ){
     AFNI_clus_make_widgets( im3d , nclu ) ;
     cwid = im3d->vwid->func->cwid ;
   }

   if( nclu == 0 || cld == NULL ){
     for( ii=0 ; ii < cwid->nrow ; ii++ )
       XtUnmanageChild( cwid->clu_rc[ii] ) ;
     cwid->nrow = 0 ;  /* # of managed rows */
     EXRETURN ;
   }

   if( rpt == NULL || *rpt == '\0' ) rpt = " \n --- Cluster Report --- \n " ;

   rrr = malloc(strlen(rpt)+8) ; strcpy(rrr," \n") ; strcat(rrr,rpt) ;
   MCW_set_widget_label( cwid->top_lab , rrr ) ; free(rrr) ;

   /* make more widget rows? (1 per cluster is needed) */

   if( cwid->nall < nclu ){
     cwid->clu_rc     =(Widget *)XtRealloc((char *)cwid->clu_rc     ,nclu*sizeof(Widget));
     cwid->clu_lab    =(Widget *)XtRealloc((char *)cwid->clu_lab    ,nclu*sizeof(Widget));
     cwid->clu_jump_pb=(Widget *)XtRealloc((char *)cwid->clu_jump_pb,nclu*sizeof(Widget));
     cwid->clu_plot_pb=(Widget *)XtRealloc((char *)cwid->clu_plot_pb,nclu*sizeof(Widget));
     cwid->clu_save_pb=(Widget *)XtRealloc((char *)cwid->clu_save_pb,nclu*sizeof(Widget));
     cwid->clu_aver_av=(MCW_arrowval **)XtRealloc( (char *)cwid->clu_aver_av,
                                                   nclu*sizeof(MCW_arrowval *) ) ;
     for( ii=cwid->nall ; ii < nclu ; ii++ ){ MAKE_CLUS_ROW(ii) ; }
     cwid->nall = nclu ;
   }

   /* map or unmap widget rows? */

   if( cwid->nrow < nclu ){
     for( ii=cwid->nrow ; ii < nclu ; ii++ )
       XtManageChild( cwid->clu_rc[ii] ) ;
   } else if( cwid->nrow > nclu ){
     for( ii=nclu ; ii < cwid->nrow ; ii++ )
       XtUnmanageChild( cwid->clu_rc[ii] ) ;
   }
   cwid->nrow = nclu ;  /* # of managed rows */

   /* change labels for each row */

   for( ii=0 ; ii < nclu ; ii++ ){
     switch( cwid->cmode ){
       default:
       case PEAK_MODE:  xx=cld[ii].xpk; yy=cld[ii].ypk; zz=cld[ii].zpk; break;
       case CMASS_MODE: xx=cld[ii].xcm; yy=cld[ii].ycm; zz=cld[ii].zcm; break;
     }
     MAT44_VEC( im3d->fim_now->daxes->ijk_to_dicom , xx,yy,zz , px,py,pz ) ;
     if( cld[ii].nvox <= 99999 )
       sprintf(line,"%2d:%5d vox %+6.1f %+6.1f %+6.1f",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     else
       sprintf(line,"%2d:%9d %+6.1f %+6.1f %+6.1f",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     MCW_set_widget_label( cwid->clu_lab[ii] , line ) ;
   }

   SET_INDEX_LAB(im3d) ;

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
   if( cwid != NULL ){
     cwid->dset = NULL ; AFNI_clus_dsetlabel(im3d) ;
     XtUnmapWidget(cwid->wtop) ; cwid->is_open = 0 ;
     DESTROY_CLARR(im3d->vwid->func->clu_list) ;
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
static AFNI_dataset_choose_stuff cdds = { 0, NULL, NULL } ;

static void AFNI_clus_finalize_dataset_CB( Widget w, XtPointer cd, MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   THD_3dim_dataset *dset ;
   int ival ;

ENTRY("AFNI_clus_finalize_dataset_CB") ;
   if( !IM3D_OPEN(im3d) || cbs == NULL ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ;
   if( cwid == NULL || !cwid->is_open ){ POPDOWN_strlist_chooser; EXRETURN; }

   ival = cbs->ival ;
   if( ival < 0 || ival >= cdds.ndset ) EXRETURN ;
   dset = cdds.dset[ival] ;
   cwid->dset = ISVALID_DSET(dset) ? dset : NULL ;
   AFNI_clus_dsetlabel(im3d) ;
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

   /*-- timeseries dataset chooser --*/

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
       if( ISVALID_DSET(dset)                           &&  /* qualifications */
           DSET_NVALS(dset) >= cwid->ignore+3           &&  /* to be selected */
           DSET_NVOX(dset)  == DSET_NVOX(im3d->fim_now) &&
           DSET_INMEMORY(dset)                            )
         cdds.dset[cdds.ndset++] = dset ;
     }
     if( cdds.ndset > 0 )
       AFNI_choose_dataset_CB( cwid->top_lab , im3d , &cdds ) ;
     else
       MCW_popup_message( cwid->top_lab , " \n"
                                          "** No viable datasets **\n"
                                          "** available to graph **\n " ,
                          MCW_USER_KILL | MCW_TIMER_KILL ) ;

     EXRETURN ;
   }

   /*-- scan button list, see if widget matches on of them --*/

   nclu = im3d->vwid->func->clu_num ;
   cld  = im3d->vwid->func->clu_det ;
   if( nclu == 0 || cld == NULL ) EXRETURN ;

   for( ii=0 ; ii < nclu ; ii++ ){

     /*-- Jump to the cluster peak or cmass --*/

     if( w == cwid->clu_jump_pb[ii] ){
       float px,py,pz , xx,yy,zz ;
       switch( cwid->cmode ){
         default:
         case PEAK_MODE:  xx=cld[ii].xpk; yy=cld[ii].ypk; zz=cld[ii].zpk; break;
         case CMASS_MODE: xx=cld[ii].xcm; yy=cld[ii].ycm; zz=cld[ii].zcm; break;
       }
       MAT44_VEC( im3d->fim_now->daxes->ijk_to_dicom , xx,yy,zz , px,py,pz ) ;
       AFNI_jumpto_dicom( im3d , px,py,pz ) ;
       EXRETURN ;

     /*-- Process the cluster timeseries --*/

     } else if( w == cwid->clu_plot_pb[ii] || w == cwid->clu_save_pb[ii] ){
       int dosave = (w == cwid->clu_save_pb[ii]) ;
       int dopc   = (cwid->clu_aver_av[ii]->ival == 1) ;
       MRI_IMARR *imar = AFNI_cluster_timeseries(im3d,ii) ;
       MRI_IMAGE *im=NULL ;

       if( imar == NULL || IMARR_COUNT(imar) < 1 ){
         MCW_popup_message( w , " \n"
                                "** Can't get data!! **\n"
                                "** Need a dataset!! **\n " ,
                            MCW_USER_KILL | MCW_TIMER_KILL ) ;
         EXRETURN ;
       } else if( IMARR_COUNT(imar) == 1 ){   /* should not transpire */
         im = IMARR_SUBIM(imar,0) ;
       } else if( dopc ){           /* PC#1 */
         im = mri_pcvector( imar , cwid->ignore ) ;
       } else {                     /* Mean */
         im = mri_meanvector( imar , cwid->ignore ) ;
       }
       if( im != NULL ){
         if( !dosave ){                       /* Plotting (to rule the world) */
           char ylab[64] , tlab[THD_MAX_NAME+2] ;
           float *far = MRI_FLOAT_PTR(im) ;
           sprintf(ylab,"%s: Cluster #%d = %d voxels",
                   (dopc) ? "PC#1" : "Aver" , ii+1 , IMARR_COUNT(imar) ) ;
           sprintf(tlab,"\\noesc %s",
                   THD_trailname(DSET_HEADNAME(cwid->dset),SESSTRAIL+1)) ;
           plot_ts_xypush(1,0) ;
           plot_ts_lab( im3d->dc->display ,
                        im->nx , NULL , 1 , &far ,
                        "TR index" , ylab , tlab , NULL , NULL ) ;

         } else {                                       /* Saving (the world) */
           char fnam[32] , *ppp ; int jj ;
           ppp = getenv("AFNI_CLUSTER_PREFIX") ;
           if( ppp == NULL || *ppp == '\0' || !THD_filename_pure(ppp) )
             ppp = "Clust" ;
           sprintf(fnam,"%s%02d_%s.1D",ppp,ii+1,(dopc)?"pc":"mean") ;
           jj = mri_write_1D( fnam , im ) ;
           if( jj ) INFO_message("Wrote file %s",fnam) ;
         }
         if( im != IMARR_SUBIM(imar,0) ) mri_free(im) ;
       }
       DESTROY_IMARR(imar) ; EXRETURN ;
     }

   } /* end of loop over button rows */

   fprintf(stderr,"** Unknown button? **\n") ; EXRETURN ;
}

/*---------------------------------------------------------------------------*/

static MRI_IMARR * AFNI_cluster_timeseries( Three_D_View *im3d , int ncl )
{
   AFNI_clu_widgets *cwid ;
   MCW_cluster_array *clar ;
   MCW_cluster *cl ;
   THD_3dim_dataset *dset ;
   MRI_IMARR *imar ;
   int ii,npt , *ind ;

ENTRY("AFNI_cluster_timeseries") ;
   if( !IM3D_OPEN(im3d) ) RETURN(NULL) ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) RETURN(NULL) ;
   dset = im3d->vwid->func->cwid->dset ; if( !ISVALID_DSET(dset) ) RETURN(NULL) ;
   clar = im3d->vwid->func->clu_list ;
   if( clar == NULL || ncl < 0 || ncl >= clar->num_clu ) RETURN(NULL) ;
   cl = clar->clar[ncl] ;
   npt = cl->num_pt ; if( npt < 1 ) RETURN(NULL) ;
   ind = (int *)malloc(sizeof(int)*npt) ;
   for( ii=0 ; ii < npt ; ii++ )
     ind[ii] = DSET_ixyz_to_index(dset,cl->i[ii],cl->j[ii],cl->k[ii]) ;
   imar = THD_extract_many_series( npt , ind , dset ) ;
   free((void *)ind) ;
   RETURN(imar) ;
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
   } else if( av == cwid->cmode_av ){
     cwid->cmode = av->ival ;
     AFNI_clus_update_widgets(im3d) ;  /* redisplay coordinates */
     EXRETURN ;
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
         im3d->vwid->butx = event->x_root ;
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
