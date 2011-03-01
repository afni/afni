#undef MAIN
#include "afni.h"

static void AFNI_cluster_widgkill( Three_D_View *im3d ) ;
static void AFNI_cluster_widgize( Three_D_View *im3d , int force ) ;
static MRI_IMARR * AFNI_cluster_timeseries( Three_D_View *im3d , int ncl ) ;
static void AFNI_clus_viewpoint_CB( int why, int np, void *vp, void *cd ) ;
static char * AFNI_clus_3dclust( Three_D_View *im3d ) ;

#undef  SET_INDEX_LAB
#define SET_INDEX_LAB(iq) \
  AFNI_clus_viewpoint_CB(RECEIVE_VIEWPOINT,0,NULL,(void *)(iq))

#undef  PEAK_MODE
#undef  CMASS_MODE
#define PEAK_MODE  0
#define CMASS_MODE 1

#undef  MAX_INDEX
#define MAX_INDEX 99999

static Widget wtemp ;

static char *wherprog = NULL ;

char * get_alpha_string( int csiz  , float pval , Three_D_View *im3d    ) ;
int find_cluster_thresh( float athr, float pval , CLU_threshtable *ctab ) ;
CLU_threshtable * CLU_get_thresh_table( Three_D_View *im3d ) ;

/*****************************************************************************/
/*************  Functions for all actions in the cluster group ***************/

/*---------------------------------------------------------------------*/
/* Put a '*' next to the active item in the vedit list on the menu.    */

static char *clubutlab[] = { " Clear" ,          /* first blank saves */
                             " Clusterize"  } ;  /* space for a '*'  */

void set_vedit_cluster_label( Three_D_View *im3d , int ll )
{
   char lab[64] ;
   if( !IM3D_OPEN(im3d) ) return ;

   strcpy(lab,clubutlab[0]); if( ll==0 ) lab[0] = '*' ;
   MCW_set_widget_label( im3d->vwid->func->clu_clear_pb , lab ) ;

   strcpy(lab,clubutlab[1]); if( ll==1 ) lab[0] = '*' ;
   MCW_set_widget_label( im3d->vwid->func->clu_cluster_pb , lab ) ;

   SENSITIZE( im3d->vwid->func->clu_report_pb , (ll==1) ) ;

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

   rmm = -rintf(vec[0]) ;
        if( rmm >= -1.0f ) rmm = -1.0f ;
   else if( rmm >= -2.0f ) rmm = -2.0f ;
   else                    rmm = -3.0f ;
   vmul = vec[1] ;
   if( vmul < 2.0f ){
     static int first=1 ;
     vmul = 2.0f ;
     if( first ){
       MCW_popup_message( im3d->vwid->func->clu_cluster_pb ,
                           "** ---- WARNING ---- **\n"
                           "** Voxels is reset to 2\n " ,
                          MCW_USER_KILL | MCW_TIMER_KILL ) ;
       first = 0 ;
     }
   }
   im3d->vedset.code     = VEDIT_CLUST ;
   im3d->vedset.param[2] = rmm ;   /* is -1, -2, or -3 [Jul 2010] */
   im3d->vedset.param[3] = vmul ;  /* other params set in afni.c */
   set_vedit_cluster_label(im3d,1) ;
   if( ISVALID_3DIM_DATASET(im3d->fim_now) && !im3d->vinfo->func_visible ){
     MCW_set_bbox( im3d->vwid->view->see_func_bbox , 1 ) ;
     im3d->vinfo->func_visible = True ;
   }

   im3d->vwid->func->clu_nnlev = (int)(-rmm) ;

   if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------*/

static void AFNI_histrange_choose_CB( Widget wc, XtPointer cd, MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   float *vec = (float *)(cbs->cval) , hb,ht ;

ENTRY("AFNI_histrange_choose_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   hb = vec[0] ; ht = vec[1] ;
   if( hb >= ht ){ hb = ht = 0.0f ; }
   cwid->hbot = hb ; cwid->htop = ht ;
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
     set_vedit_cluster_label(im3d,0) ; VEDIT_unhelpize(im3d) ;
     AFNI_cluster_dispkill(im3d) ;
     if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;
     EXRETURN ;
   }

   /*--- Get clusterizing parameters ---*/

   if( w == im3d->vwid->func->clu_cluster_pb ){
     char *lvec[2] = { "NN level " , "Voxels   " } ;
     float fvec[2] ;
     MCW_arrowval **vav ; int nav ;

     if( im3d->giset != NULL && im3d->giset->ready ){  /* 20 Oct 2010 */
       (void)MCW_popup_message( w ,
                                 " You can't use Clusterize \n"
                                 " while Group InstaCorr is \n"
                                 " attached to this AFNI\n"
                                 " controller." ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
       BEEPIT ; EXRETURN ;
     }

     if( im3d->vedset.code == VEDIT_CLUST ){
       fvec[0] = -im3d->vedset.param[2];
       fvec[1] =  im3d->vedset.param[3]; if( fvec[1] < 2.0f ) fvec[1] = 20.0f;
     } else {
       fvec[0] =  1.0f ;  /* NN1 */
       fvec[1] = 20.0f ;
     }
     MCW_choose_vector( im3d->vwid->func->thr_label ,
                       "------ Set Clusterize Parameter -------\n"
                       "* NN level => NN clustering method\n"
                       "        1 = faces must touch\n"
                       "        2 = faces or edges touch\n"
                       "        3 = faces or edges or corners\n"
                       "* Voxels   => minimum cluster size that\n"
                       "              will be kept [at least 2]\n"
                       "---------------------------------------\n"
                       "* Use 'BHelp' on 'Cluster Edit' label\n"
                       "  to get summary of clustering results.\n"
                       "---------------------------------------\n"
                       "* Click on the 'Rpt' button to open a\n"
                       "  more complete cluster report panel.\n"
                       "---------------------------------------"
                       , 2 , lvec,fvec ,
                        AFNI_cluster_choose_CB , (XtPointer)im3d ) ;
     vav = MCW_choose_vector_avarray(&nav) ;
     if( nav > 1 && vav != NULL ){
       vav[0]->imin = vav[0]->fmin = 1 ; vav[0]->imax = vav[0]->fmax = 3 ;
       vav[1]->imin = vav[1]->fmin = 2 ;
     }
     EXRETURN ;
   }

   /*--- Open the report window ---*/

   if( w == im3d->vwid->func->clu_report_pb ){
     if( im3d->vedset.code == VEDIT_CLUST &&
         im3d->vinfo->func_visible        &&
         IM3D_IMAGIZED(im3d)                ){

       CLU_setup_alpha_tables(im3d) ; /* Jul 2010 */
       AFNI_cluster_widgize(im3d,1) ;
     } else {
       MCW_popup_message( im3d->vwid->func->clu_report_pb ,
                           " \n"
                           "** You must have 'Clusterize' on   **\n"
                           "** AND 'See Overlay' on AND have   **\n"
                           "** at least one image viewer open  **\n"
                           "** to get the cluster report table **\n " ,
                          MCW_USER_KILL | MCW_TIMER_KILL ) ;
     }
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
   AFNI_clu_widgets *cwid ;

ENTRY("AFNI_cluster_EV") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   switch( ev->type ){
     case ButtonPress:{
       XButtonEvent *event = (XButtonEvent *) ev ;
       im3d->vwid->butx = event->x_root ;
       im3d->vwid->buty = event->y_root ;
       event->button    = Button3 ;               /* fakeout */
       XmMenuPosition( cwid->top_menu , event ) ; /* where */
       XtManageChild ( cwid->top_menu ) ;         /* popup */
     }
     break ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

static void AFNI_clus_make_widgets( Three_D_View *, int ) ;
static void AFNI_clus_done_CB  ( Widget,XtPointer,XtPointer ) ;

static void AFNI_clus_av_CB( MCW_arrowval * , XtPointer ) ;
static void AFNI_clus_action_CB( Widget,XtPointer,XtPointer ) ;

/*---------------------------------------------------------------------------*/

#define SHRUTI_NUM 999

static int maxclu_default = -1 ;
static int scrolling      =  1 ;

/*! Make the widgets for one row of the cluster display/control panel.
    The row itself will not be managed at this time; that comes later. */

#undef  MAKE_CLUS_ROW
#define MAKE_CLUS_ROW(ii)                                           \
 do{ Widget rc,lb,mb ; char *str[1]={"abcdefghijklmn: "} ;          \
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
     lb = cwid->clu_lab[ii] = XtVaCreateManagedWidget(              \
            ff     , xmLabelWidgetClass , rc ,                      \
            LABEL_ARG("##:xxxxx vox +xxx.x +xxx.x +xxx.x") ,        \
            XmNalignment , XmALIGNMENT_BEGINNING ,                  \
            XmNrecomputeSize , False ,  XmNtraversalOn , True ,     \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     cwid->clu_jump_pb[ii] = XtVaCreateManagedWidget(               \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            LABEL_ARG("Jump") , XmNtraversalOn , True ,             \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     cwid->clu_flsh_pb[ii] = XtVaCreateManagedWidget(               \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            LABEL_ARG("Flash") , XmNtraversalOn , True ,            \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     cwid->clu_plot_pb[ii] = XtVaCreateManagedWidget(               \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            LABEL_ARG("Plot") , XmNtraversalOn , True ,             \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     cwid->clu_save_pb[ii] = XtVaCreateManagedWidget(               \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            LABEL_ARG("Save") , XmNtraversalOn , True ,             \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     mb = cwid->clu_alph_lab[ii] = XtVaCreateManagedWidget(         \
            ff     , xmLabelWidgetClass , rc ,                      \
            LABEL_ARG("------") ,                                   \
            XmNalignment , XmALIGNMENT_BEGINNING ,                  \
            XmNrecomputeSize , False ,  XmNtraversalOn , True ,     \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     XtAddCallback( cwid->clu_jump_pb[ii],                          \
                    XmNactivateCallback,AFNI_clus_action_CB,im3d ); \
     XtAddCallback( cwid->clu_plot_pb[ii],                          \
                    XmNactivateCallback,AFNI_clus_action_CB,im3d ); \
     XtAddCallback( cwid->clu_save_pb[ii],                          \
                    XmNactivateCallback,AFNI_clus_action_CB,im3d ); \
     XtAddCallback( cwid->clu_flsh_pb[ii],                          \
                    XmNactivateCallback,AFNI_clus_action_CB,im3d ); \
     if( scrolling && ii%2==1 ){                                    \
       MCW_set_widget_bg(rc,"black",0);                             \
       MCW_set_widget_bg(lb,"black",0);                             \
       MCW_set_widget_bg(mb,"black",0);                             \
     }                                                              \
  } while(0)

/*---------------------------------------------------------------------------*/
/* Set the label for the auxiliary dataset */

static void AFNI_clus_dsetlabel( Three_D_View *im3d )
{
   AFNI_clu_widgets *cwid ;
   char str[THD_MAX_NAME+66] ;

ENTRY("AFNI_clus_dsetlabel") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   if( !ISVALID_DSET(cwid->dset) )
     strcpy(str," [No Aux Dataset chosen]") ;
   else
     sprintf(str,"Aux=%s",THD_trailname(DSET_HEADNAME(cwid->dset),SESSTRAIL+1)) ;

   if( cwid->splotim != NULL )
     sprintf(str+strlen(str)," Scat.1D=%s",cwid->splotim->name) ;
   else
     sprintf(str+strlen(str)," [Scat.1D cleared]") ;

   MCW_set_widget_label( cwid->dset_lab , str ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Make the cluster report widgets initially */

static void AFNI_clus_make_widgets( Three_D_View *im3d, int num )
{
   AFNI_clu_widgets *cwid ;
   Widget ww , rc ;
   XmString xstr ;
   char str[32] , *eee ;
   int ii ;
   Widget shtop , swtop=NULL ;

ENTRY("AFNI_clus_make_widgets") ;

   if( !IM3D_OPEN(im3d) || im3d->vwid->func->cwid != NULL ) EXRETURN ; /* bad */

   im3d->vwid->func->cwid = cwid = myXtNew( AFNI_clu_widgets ) ;
   cwid->dset = NULL ; cwid->coord_mode = 0 ;

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

   if( !AFNI_noenv("AFNI_CLUSTER_SCROLL") )
     swtop = shtop = XtVaCreateManagedWidget(
                   "menu" , xmScrolledWindowWidgetClass , cwid->wtop ,
                      XmNscrollingPolicy        , XmAUTOMATIC ,
                      XmNvisualPolicy           , XmVARIABLE ,
                      XmNscrollBarDisplayPolicy , XmAS_NEEDED /* XmSTATIC */ ,
                      XmNinitialResourcesPersistent , False ,
                   NULL ) ;
   else
     shtop = cwid->wtop ;

   scrolling = (swtop!=NULL) ;
   if( maxclu_default < 0 ) maxclu_default = scrolling ? 15 : SHRUTI_NUM ;

   /* vertical rowcol to hold it all */

   cwid->rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , shtop ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNspacing      , 0 ,
            XmNadjustMargin , True ,
            XmNtraversalOn  , True ,
         NULL ) ;

   /* top label to describe the overall results */

   xstr = XmStringCreateLtoR( " If you see this text, this means   \n"
                              " that clustering hasn't happened.   \n"
                              " 'See Overlay' needs to be turned   \n"
                              " on, and you might need to force a  \n"
                              " redisplay by changing the threshold"   ,
                              XmFONTLIST_DEFAULT_TAG ) ;
   cwid->top_lab = XtVaCreateManagedWidget(
                    "menu" , xmLabelWidgetClass , cwid->rowcol ,
                       XmNrecomputeSize , True ,
                       XmNlabelString , xstr ,
                       XmNtraversalOn , True  ,
                    NULL ) ;
   XmStringFree(xstr) ; LABELIZE(cwid->top_lab) ;

   /*---- popup menu on top label ----*/

   XtInsertEventHandler( cwid->top_lab ,        /* handle events in label */
                           ButtonPressMask ,    /* button presses */
                           FALSE ,              /* nonmaskable events? */
                           AFNI_cluster_EV ,    /* handler */
                           (XtPointer) im3d ,   /* client data */
                           XtListTail           /* last in queue */
                       ) ;

   cwid->top_menu = XmCreatePopupMenu( cwid->top_lab , "menu" , NULL , 0 ) ;
   SAVEUNDERIZE(cwid->top_menu) ; VISIBILIZE_WHEN_MAPPED(cwid->top_menu) ;
   if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(cwid->top_menu) ;

   wtemp = XtVaCreateManagedWidget(
            "dialog" , xmLabelWidgetClass , cwid->top_menu ,
               LABEL_ARG("--- Cancel ---") ,
               XmNrecomputeSize , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ; LABELIZE(wtemp) ;

   xstr = XmStringCreateLtoR( "Hist range" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->histrange_pb = XtVaCreateManagedWidget(
           "menu" , xmPushButtonWidgetClass , cwid->top_menu ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
         NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( cwid->histrange_pb, XmNactivateCallback, AFNI_clus_action_CB, im3d );
   MCW_register_hint( cwid->histrange_pb , "Set Histogram data range" ) ;
   cwid->hbot = cwid->htop = 0.0f ;

   { char *blab = "SqrtHist?" ;
     cwid->histsqrt_bbox = new_MCW_bbox( cwid->top_menu , 1,&blab ,
                                         MCW_BB_check , MCW_BB_noframe , NULL,NULL ) ;
     MCW_reghint_children( cwid->histsqrt_bbox->wrowcol , "Plot square root of histogram?" ) ;
   }

   /*---- end of popup menu ----*/

#undef  VLINE
#undef  HLINE
#define VLINE(rr)                                                           \
     (void) XtVaCreateManagedWidget( "menu", xmSeparatorWidgetClass, (rr) , \
                                        XmNorientation   , XmVERTICAL    ,  \
                                        XmNseparatorType , XmSINGLE_LINE ,  \
                                     NULL )
#define HLINE(rr)                                                           \
   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass, (rr) , \
                                      XmNseparatorType   , XmSINGLE_LINE ,  \
                                   NULL )

   /* Separator from other widgets */

   HLINE(cwid->rowcol) ;

   /* horiz rowcol for row #1 controls */

   rc = XtVaCreateWidget(
          "menu" , xmRowColumnWidgetClass , cwid->rowcol ,
             XmNpacking      , XmPACK_TIGHT ,
             XmNorientation  , XmHORIZONTAL ,
             XmNadjustMargin , True ,
             XmNtraversalOn  , True ,
          NULL ) ;

   /* row #1: index label */

   cwid->index_lab = XtVaCreateManagedWidget( "menu" , xmLabelWidgetClass , rc , NULL ) ;
   LABELIZE(cwid->index_lab) ;
   MCW_register_hint( cwid->index_lab , "Crosshairs are in this cluster" ) ;
   MCW_register_help( cwid->index_lab , "Shows the cluster index that\n"
                                        "contains the crosshairs point.\n"
                                        "* '??' means that the crosshairs\n"
                                        "  are not in a cluster right now." ) ;
   VLINE(rc) ;

   /* row #1: coord_mode chooser */

   { static char *clab[2] = { "Peak" , "CMass" } ;
     cwid->cmode_av = new_MCW_optmenu( rc , "XYZ" , 0,1,0,0 ,
                        AFNI_clus_av_CB,im3d , MCW_av_substring_CB,clab ) ;
     MCW_reghint_children( cwid->cmode_av->wrowcol , "Coordinate display type" ) ;
     MCW_reghelp_children( cwid->cmode_av->wrowcol ,
                            "Choose whether to show the Peak or\n"
                            "Center-of-Mass X,Y,Z coordinates\n"
                            "for each cluster.\n"
                            "* The weights that define these\n"
                            "   locations are taken from the\n"
                            "   'OLay' sub-brick of the Overlay\n"
                            "   dataset.\n"
                            "* The coordinate signs (DICOM or\n"
                            "   SPM order) can be chosen in the\n"
                            "   main AFNI controller window by\n"
                            "   the right-click popup menu in\n"
                            "   the upper-left coordinate label.\n"
                         ) ;
   }

   /* row #1: 3dclust button */

   VLINE(rc) ;
   xstr = XmStringCreateLtoR( "3dclust" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->clust3d_pb = XtVaCreateManagedWidget(
           "menu" , xmPushButtonWidgetClass , rc ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
         NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( cwid->clust3d_pb, XmNactivateCallback, AFNI_clus_action_CB, im3d );
   MCW_register_hint( cwid->clust3d_pb , "Output equivalent 3dclust command" ) ;
   MCW_register_help( cwid->clust3d_pb ,
                       "Writes the equivalent 3dclust\n"
                       "command to the terminal (stdout)\n"
                       "* However, at this time, 3dclust\n"
                       "   has no way to use the 3dClustSim\n"
                       "   mask (if any) stored in the\n"
                       "   Overlay dataset's header, so\n"
                       "   the results might not be exactly\n"
                       "   identical to AFNI's Clusterize."
                    ) ;

   /* row #1: SaveTabl button */

   xstr = XmStringCreateLtoR( "SaveTabl" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->savetable_pb = XtVaCreateManagedWidget(
           "menu" , xmPushButtonWidgetClass , rc ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
         NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( cwid->savetable_pb, XmNactivateCallback, AFNI_clus_action_CB, im3d );
   MCW_register_hint( cwid->savetable_pb , "Write results to a text file" ) ;
   MCW_register_help( cwid->savetable_pb ,
                      "Write cluster locations (CM and Peak)\n"
                      "to a text file, whose name is of the\n"
                      "form 'NAME_table.1D', where 'NAME'\n"
                      "is the entry in the text field\n"
                      "(to the right).\n"
                      "* If 'NAME' is blank, then 'Clust' is used.\n"
                      "* If 'NAME' is '-', then stdout is used."
                    ) ;

   /* row #1: prefix textfield */

   { char *ppp = getenv("AFNI_CLUSTER_PREFIX") ;
     if( !THD_filename_pure(ppp) || strlen(ppp) > 61 ) ppp = "Clust" ;
     cwid->prefix_tf = XtVaCreateManagedWidget(
                       "menu" , xmTextFieldWidgetClass , rc ,
                           XmNvalue        , ppp ,
                           XmNcolumns      , 8 ,
                           XmNeditable     , True ,
                           XmNmaxLength    , 64 ,
                           XmNresizeWidth  , False ,
                           XmNmarginHeight , 1 ,
                           XmNmarginWidth  , 1 ,
                           XmNcursorPositionVisible , True ,
                           XmNblinkRate , 0 ,
                           XmNautoShowCursorPosition , True ,
                           XmNtraversalOn , True  ,
                           XmNinitialResourcesPersistent , False ,
                        NULL ) ;
     MCW_set_widget_bg( cwid->prefix_tf , "black" , (Pixel)0 ) ;
     MCW_register_hint( cwid->prefix_tf , "Output file prefix" ) ;
     MCW_register_help( cwid->prefix_tf , "This string is used to\n"
                                          "set the output filename\n"
                                          "for all 'Save' buttons."  ) ;
   }

   /* row #1: SaveMsk button [01 May 2008] */

   xstr = XmStringCreateLtoR( "SaveMsk" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->savemask_pb = XtVaCreateManagedWidget(
           "menu" , xmPushButtonWidgetClass , rc ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
         NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( cwid->savemask_pb, XmNactivateCallback, AFNI_clus_action_CB, im3d );
   MCW_register_hint( cwid->savemask_pb , "Write clusters to a mask dataset" ) ;
   MCW_register_help( cwid->savemask_pb ,
                       "Write the set of clusters to\n"
                       "a 3D dataset.  The value in\n"
                       "each voxel will be the cluster\n"
                       "index (1,2,...) for that voxel,\n"
                       "or 0 if that voxel isn't in\n"
                       "any cluster.  The text field\n"
                       "(to the left) is used to set\n"
                       "the dataset's prefix name."
                    ) ;

   /* row #1: Where button [04 Aug 2010] */

   wherprog = THD_find_executable("whereami") ;
   if( wherprog != NULL ){
     xstr = XmStringCreateLtoR( "WamI" , XmFONTLIST_DEFAULT_TAG ) ;
     cwid->whermask_pb = XtVaCreateManagedWidget(
             "menu" , xmPushButtonWidgetClass , rc ,
              XmNlabelString , xstr ,
              XmNtraversalOn , True  ,
           NULL ) ;
     XmStringFree(xstr) ;
     XtAddCallback( cwid->whermask_pb, XmNactivateCallback, AFNI_clus_action_CB, im3d );
     MCW_register_hint( cwid->whermask_pb , "SaveMsk, then 'whereami -omask'") ;
     MCW_register_help( cwid->whermask_pb ,
                         "Write the set of clusters to\n"
                         "a 3D dataset, then run program\n"
                         "       whereami -omask\n"
                         "to get a report of atlas locations\n"
                         "that overlap each cluster.\n"
                         "* At most the first 20 clusters\n"
                         "    will be passed to whereami.\n"
                         "* To change this upper limit, set\n"
                         "    AFNI_CLUSTER_WAMIMAX to a\n"
                         "    value between 1 and 99.\n"
                         "* Can only be run in Talairach View"
                      ) ;
     SENSITIZE(cwid->whermask_pb,
               (im3d->vinfo->view_type == VIEW_TALAIRACH_TYPE) ) ;

   } else {
     WARNING_message("No whereami program in Unix path ==> no WamI button in Clusterize!") ;
     cwid->whermask_pb = cwid->savemask_pb ;
   }

   /* row #1: Done button */

   VLINE(rc) ;
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
   MCW_register_hint(cwid->done_pb,"Close this window!") ;
   MCW_register_help(cwid->done_pb,"Don't you know what\n"
                                   "a 'Done' button means\n"
                                   "in AFNI by now?!!?"     ) ;

   XtManageChild(rc) ;  /* finished with row #1 setup */
   HLINE(cwid->rowcol) ;

   /* horiz rowcol for row #2 controls */

   rc = XtVaCreateWidget(
          "menu" , xmRowColumnWidgetClass , cwid->rowcol ,
             XmNpacking      , XmPACK_TIGHT ,
             XmNorientation  , XmHORIZONTAL   ,
             XmNadjustMargin , True ,
             XmNtraversalOn , True  ,
          NULL ) ;

   /* row #2: dataset chooser */

   xstr = XmStringCreateLtoR( "Aux.Dset" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->dataset_pb = XtVaCreateManagedWidget(
           "menu" , xmPushButtonWidgetClass , rc ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
         NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( cwid->dataset_pb, XmNactivateCallback, AFNI_clus_action_CB, im3d );
   MCW_register_hint( cwid->dataset_pb , "data for Plot/Save from cluster" ) ;
   MCW_register_help( cwid->dataset_pb ,
                       "If you choose an Auxiliary Dataset\n"
                       "  -- which must have the same grid     --\n"
                       "  -- dimensions as the Overlay dataset --\n"
                       "then for each cluster, you can extract\n"
                       "the corresponding data from that dataset\n"
                       "(sub-bricks 'From' until 'To') and do various\n"
                       "things with these numbers:\n"
                       "** Average them at each time-point:      'Mean'\n"
                       "** Pointwise median:                     'Median'\n"
                       "** Compute first principal component:    'PC#1'\n"
                       "** Histogram all the values:             'Histog'\n"
                       "** Scatterplot mean from each sub-brick: 'S:mean'\n"
                       "** Scatterplot all values:               'S:all'\n"
                       "   [-- Scat.1D button chooses x-axis for 'S:' --]\n"
                       "And then either 'Plot' or 'Save' these results\n"
                       "** If you 'Save' these results, the text field\n"
                       "   (between 'SaveTabl and 'SaveMsk') is used to\n"
                       "   define the output filename.\n"
                       "** If this text field is just the string '-',\n"
                       "   then 'Save' writes to the terminal window\n"
                       "   (stdout), instead of a file.\n"
                       "** If you set environment variable\n"
                       "   AFNI_CLUSTER_EBAR to YES, then the 'Mean'\n"
                       "   and 'Median' plots will also show error bars\n"
                       "   (the biweight midvariance)."
                    ) ;

   /* row #2: 'from' and 'to' choosers */

   cwid->from_av = new_MCW_arrowval( rc , "From" , MCW_AV_downup ,
                                     0,MAX_INDEX,0,MCW_AV_editext,0 ,
                                     NULL,NULL , NULL,NULL ) ;
   MCW_reghint_children( cwid->from_av->wrowcol ,
                         "first sub-brick to use from Aux Dataset" ) ;
   XtVaSetValues( cwid->from_av->wtext , XmNcolumns , 5 , NULL ) ;

   cwid->to_av = new_MCW_arrowval( rc , "To" , MCW_AV_downup ,
                                     0,MAX_INDEX,MAX_INDEX,MCW_AV_editext,0 ,
                                     NULL,NULL , NULL,NULL ) ;
   MCW_reghint_children( cwid->to_av->wrowcol ,
                         "last sub-brick to use from Aux Dataset" ) ;
   XtVaSetValues( cwid->to_av->wtext , XmNcolumns , 5 , NULL ) ;

   /* row #2: data processing method */

   { static char *clab[6] = { "Mean", "Median", "PC#1", "Histog", "S:mean", "S:all" } ;
     cwid->aver_av = new_MCW_optmenu( rc , " " , 0,5,0,0 ,
                                      NULL,NULL , MCW_av_substring_CB,clab ) ;
     MCW_reghint_children( cwid->aver_av->wrowcol ,
                           "Set data processing method for Plot/Save" ) ;
     MCW_reghelp_children( cwid->aver_av->wrowcol ,
                           "Defines how data extracted from the\n"
                           "Auxiliary Dataset will be processed:\n"
                           "* Mean   = averaged across voxels\n"
                           "* Median = median across voxels\n"
                           "* PC#1   = compute first principal\n"
                           "         component vector\n"
                           "* Histog = build a histogram across\n"
                           "           voxels and sub-bricks\n"
                           "* Error bars will be shown for 'Mean'\n"
                           "  and 'Median' if environment variable\n"
                           "  AFNI_CLUSTER_EBAR is set to YES.\n"
                           "* A hidden Button-3 popup menu on the\n"
                           "  cluster summary report label atop this\n"
                           "  window will let you choose the range\n"
                           "  of data to be histogram-ized.\n"
                           "* The options 'S:all' and 'S:mean' allow\n"
                           "  you to scatter plot all the data values\n"
                           "  from the cluster -- or just the mean from\n"
                           "  each sub-brick -- vs. an external 1D file,\n"
                           "  which is chosen with the 'Scat.1D' button."
                         ) ;
   }

   VLINE(rc) ; VLINE(rc) ;

   /* row #2: splot choosers */

   xstr = XmStringCreateLtoR( "Scat.1D" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->splot_pb = XtVaCreateManagedWidget(
           "menu" , xmPushButtonWidgetClass , rc ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
         NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( cwid->splot_pb, XmNactivateCallback, AFNI_clus_action_CB, im3d );
   MCW_register_hint( cwid->splot_pb , "pick ScatterPlot x-axis file" ) ;
   MCW_register_help( cwid->splot_pb ,
                      "* This button lets you choose a 1D file\n"
                      "   to use as the x-axis values for the\n"
                      "   scatterplot 'S:all' and 'S:mean' options.\n"
                      "* This 1D file should have as many lines\n"
                      "   as the Aux.Dset has sub-bricks; the\n"
                      "   initial 'From' lines will be skipped.\n"
                      "* The purpose is to let you plot the\n"
                      "   beta values from multiple subjects\n"
                      "   vs. some subject-level covariate."
                    ) ;

   xstr = XmStringCreateLtoR( "Clear" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->splot_clear_pb = XtVaCreateManagedWidget(
           "menu" , xmPushButtonWidgetClass , rc ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
         NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( cwid->splot_clear_pb, XmNactivateCallback, AFNI_clus_action_CB, im3d );
   MCW_register_hint( cwid->splot_clear_pb , "clear ScatterPlot x-axis file" ) ;
   MCW_register_help( cwid->splot_clear_pb ,
                      "Erase the Scat.1D file and\n"
                      "just use the sub-brick index\n"
                      "as the x-axis for the various\n"
                      "scatterplot options."
                    ) ;
   cwid->splotim = NULL ;

   XtManageChild(rc) ;  /* row #2 is finished */

   /* Time series dataset label */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass,cwid->rowcol,
                                      XmNseparatorType   , XmSHADOW_ETCHED_IN ,
                                   NULL ) ;
   cwid->dset_lab = XtVaCreateManagedWidget(
                      "dialog" , xmLabelWidgetClass , cwid->rowcol , NULL ) ;
   AFNI_clus_dsetlabel(im3d) ;
   MCW_set_widget_fg( cwid->dset_lab , "white" ) ;
   LABELIZE( cwid->dset_lab ) ;

   /* Separator from other widgets */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass,cwid->rowcol,
                                      XmNseparatorType   , XmSINGLE_LINE ,
                                   NULL ) ;

   /* Jul 2010: header line */

   ww = XtVaCreateManagedWidget( "dialog" , xmLabelWidgetClass , cwid->rowcol ,
                                  NULL ) ;
   MCW_set_widget_label( ww ,
                         "##: __Size__  __X__  __Y__  __Z__                        Alpha") ;
   MCW_set_widget_fg( ww , "white") ;

   /* Now create rows of widgets to display results from clusters */

   if( num < 2 ) num = 2 ;
   cwid->nall = num ;
   cwid->nrow = 0 ;     /* none are managed at this time */

   cwid->clu_rc      = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_lab     = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_jump_pb = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_plot_pb = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_save_pb = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_flsh_pb = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_alph_lab= (Widget *)XtCalloc( num , sizeof(Widget) ) ;

   for( ii=0 ; ii < num ; ii++ ){ MAKE_CLUS_ROW(ii) ; }
   for( ii=0 ; ii < num ; ii++ ){
     MCW_register_hint( cwid->clu_lab[ii]     ,
                        "Coordinates of cluster (Peak or CMass)" ) ;
     MCW_register_hint( cwid->clu_jump_pb[ii] ,
                        "Set crosshairs to these XYZ coordinates" ) ;
     MCW_register_hint( cwid->clu_plot_pb[ii] ,
                        "Plot average over cluster of Auxiliary 3D+time Dataset" ) ;
     MCW_register_hint( cwid->clu_save_pb[ii] ,
                        "Save average Aux dataset timeseries to 1D file" ) ;
     MCW_register_hint( cwid->clu_flsh_pb[ii] ,
                        "Flash cluster voxels in image viewers" ) ;
     MCW_register_hint( cwid->clu_alph_lab[ii] ,
                        "Approximate alpha value: use BHelp for more info" ) ;
     MCW_register_help( cwid->clu_alph_lab[ii] ,
                        "Alpha values come from 3dClustSim (via afni_proc.py).\n"
                        "\n"
                        "Alpha is the probability that a noise-only random\n"
                        "volume would produce a cluster of the given size,\n"
                        "after thresholding at a given per-voxel p-value.\n"
                        "\n"
                        "Possible strings shown below the 'alpha' label are:\n"
                        "\n"
                        " < 0.xx  means the cluster size + threshold p-value\n"
                        "          estimates the significance (alpha) level\n"
                        "          to be smaller than the 0.xx value shown.\n"
                        "\n"
                        " <<0.01  means the cluster alpha level is markedly\n"
                        "          below the 0.01 cutoff provided by 3dClustSim.\n"
                        "\n"
                        " > 0.10  means the cluster alpha level is above 0.10,\n"
                        "          so this cluster could 'easily' arise from\n"
                        "          pure noise.\n"
                        "\n"
                        " N/Csim  means that 3dClustSim results were not\n"
                        "          available in Overlay dataset's header.\n"
                        "\n"
                        " N/pval  means that the p-value of the threshold\n"
                        "          slider is too large to be used; you have\n"
                        "          to move the threshold slider UP until\n"
                        "          the p-value gets small enough.\n"
                        "\n"
                        " N/stat  means that the Overlay dataset's Threshold\n"
                        "          sub-brick is not a statistic, so it doesn't\n"
                        "          give a p-value to use for alpha significance.\n"
                        "\n"
                        "Any other string shown indicates an error in the\n"
                        "Clusterize software logic.  If this happens, please\n"
                        "blame anyone but RW Cox, who is completely innocent."
                      ) ;
   }

   XtManageChild( cwid->rowcol ) ;

   if( swtop != NULL ){
     int wx,hy , cmax ;
     MCW_widget_geom( cwid->rowcol  , &wx,&hy,NULL,NULL ) ;
     hy *= 2 ; cmax = im3d->dc->height-128 ; if( hy > cmax ) hy = cmax ;
     XtVaSetValues( cwid->wtop , XmNwidth,wx+17,XmNheight,hy+21 , NULL ) ;
   }

   XtRealizeWidget( cwid->wtop ) ;
   cwid->receive_on = 0 ;

   WAIT_for_window( cwid->wtop ) ;
   NORMAL_cursorize( cwid->rowcol ) ;
   POPUP_cursorize( cwid->top_lab ) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Display the cluster report */

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
/* Kill the cluster report */

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

/*---------------------------------------------------------------------------*/
/* A 'receive' function for when the viewpoint changes;
   used to report which cluster the crosshairs are in (if any) */

static void AFNI_clus_viewpoint_CB( int why, int np, void *vp, void *cd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   int ncl ; char lab[8] ;

   if( !IM3D_VALID(im3d) ) return;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) return ;

   switch( why ){

     case RECEIVE_VIEWPOINT:
       ncl = AFNI_clus_find_xyz( im3d ,
                                 im3d->vinfo->xi ,
                                 im3d->vinfo->yj , im3d->vinfo->zk ) ;

       im3d->vwid->func->clu_index = ncl ; /* not used at this time; someday? */
       if( ncl >= 0 ) sprintf(lab,"#%d",ncl+1) ;
       else           strcpy(lab,"??") ;
       MCW_set_widget_label( cwid->index_lab , lab ) ;
     break ;

     case RECEIVE_ALTERATION:
       if( !ISVALID_DSET(im3d->fim_now) ||
           !ISVALID_DSET(cwid->dset)    ||
           DSET_NVOX(im3d->fim_now) != DSET_NVOX(cwid->dset) ){

           cwid->dset = NULL ; AFNI_clus_dsetlabel(im3d) ;
       }
     break ;

     case RECEIVE_CLOSURE:
       AFNI_clus_popdown( im3d ) ;
       cwid->receive_on = 0 ;
     break ;

   }

   return ;
}

/*---------------------------------------------------------------------------*/
/* Extract details (coordinates, etc.) from the clusterized clusters */

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
/* Re-display the report to be updated to the current clusterization status. */

void AFNI_clus_update_widgets( Three_D_View *im3d )
{
   AFNI_clu_widgets *cwid ;
   char *rpt , *rrr ;
   mri_cluster_detail *cld ;
   int nclu , ii ;
   float px,py,pz , xx,yy,zz , pval ;
   char line[128] ;
   MCW_cluster_array *clar ;
   int maxclu ;
   CLU_threshtable *ctab=NULL ;
   int do_wami = (wherprog != NULL) ;

ENTRY("AFNI_clus_update_widgets") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   if( maxclu_default < 0 )
     maxclu_default = AFNI_noenv("AFNI_CLUSTER_SCROLL") ? 15 : SHRUTI_NUM ;

   clar = im3d->vwid->func->clu_list ;
   if( clar != NULL ){  /* sort and truncate */
     maxclu = maxclu_default ;
     if( !scrolling ){
       maxclu = AFNI_numenv("AFNI_CLUSTER_REPMAX") ;
       if( maxclu < 10 || maxclu > 9999 ) maxclu = 9999 ;
     }
     nclu = clar->num_clu ; nclu = MIN(nclu,maxclu) ;
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
     if( nclu == 0 ) EXRETURN ;
     AFNI_clus_make_widgets( im3d , nclu ) ;
     cwid = im3d->vwid->func->cwid ;
   }

   if( nclu == 0 || cld == NULL ){
     for( ii=0 ; ii < cwid->nrow ; ii++ ) XtUnmanageChild( cwid->clu_rc[ii] ) ;
     cwid->nrow = 0 ;  /* # of managed rows */
     EXRETURN ;
   }

   if( rpt == NULL || *rpt == '\0' ) rpt = " \n --- Cluster Report --- \n " ;

   rrr = malloc(strlen(rpt)+256) ; strcpy(rrr,rpt) ;

   pval = im3d->vinfo->func_pval ;
   ctab = CLU_get_thresh_table( im3d ) ;
   if( pval >= 0.0f && ctab != NULL ){
#if 0
     int csiz ;
     csiz = find_cluster_thresh( 0.10f , pval , ctab ) ;
     if( csiz > 0 ) sprintf( rrr+strlen(rrr) , " Cluster thresh(alpha=0.10) =%6d\n",csiz) ;
     else           strcat ( rrr             , " Cluster thresh(alpha=0.10) =   N/A\n") ;
     csiz = find_cluster_thresh( 0.05f , pval , ctab ) ;
     if( csiz > 0 ) sprintf( rrr+strlen(rrr) , " Cluster thresh(alpha=0.05) =%6d\n",csiz) ;
     else           strcat ( rrr             , " Cluster thresh(alpha=0.05) =   N/A\n") ;
     csiz = find_cluster_thresh( 0.01f , pval , ctab ) ;
     if( csiz > 0 ) sprintf( rrr+strlen(rrr) , " Cluster thresh(alpha=0.01) =%6d\n",csiz) ;
     else           strcat ( rrr             , " Cluster thresh(alpha=0.01) =   N/A\n") ;
#else
     int csiz ; char ssiz10[8] , ssiz05[8] , ssiz01[8] ;
     csiz = find_cluster_thresh( 0.10f , pval , ctab ) ;
     if( csiz > 0 ) sprintf(ssiz10,"%d",csiz) ; else strcpy(ssiz10,"N/A") ;
     csiz = find_cluster_thresh( 0.05f , pval , ctab ) ;
     if( csiz > 0 ) sprintf(ssiz05,"%d",csiz) ; else strcpy(ssiz05,"N/A") ;
     csiz = find_cluster_thresh( 0.01f , pval , ctab ) ;
     if( csiz > 0 ) sprintf(ssiz01,"%d",csiz) ; else strcpy(ssiz01,"N/A") ;
     sprintf( rrr+strlen(rrr) ,
              " Alpha -> Cluster thresh: 0.10->%s : 0.05->%s : 0.01->%s" ,
              ssiz10 , ssiz05 , ssiz01 ) ;
#endif
   }
   ii = strlen(rrr) ; if( rrr[ii-1] == '\n' ) rrr[ii-1] = '\0' ;

   MCW_set_widget_label( cwid->top_lab , rrr ) ; free(rrr) ;

   /* make more widget rows? (1 per cluster is needed) */

   if( cwid->nall < nclu ){
     cwid->clu_rc      =(Widget *)XtRealloc((char *)cwid->clu_rc      ,nclu*sizeof(Widget));
     cwid->clu_lab     =(Widget *)XtRealloc((char *)cwid->clu_lab     ,nclu*sizeof(Widget));
     cwid->clu_jump_pb =(Widget *)XtRealloc((char *)cwid->clu_jump_pb ,nclu*sizeof(Widget));
     cwid->clu_plot_pb =(Widget *)XtRealloc((char *)cwid->clu_plot_pb ,nclu*sizeof(Widget));
     cwid->clu_save_pb =(Widget *)XtRealloc((char *)cwid->clu_save_pb ,nclu*sizeof(Widget));
     cwid->clu_flsh_pb =(Widget *)XtRealloc((char *)cwid->clu_flsh_pb ,nclu*sizeof(Widget));
     cwid->clu_alph_lab=(Widget *)XtRealloc((char *)cwid->clu_alph_lab,nclu*sizeof(Widget));
     for( ii=cwid->nall ; ii < nclu ; ii++ ){ MAKE_CLUS_ROW(ii) ; }
     cwid->nall = nclu ;
   }

   /* map or unmap widget rows? */

   if( cwid->nrow < nclu ){
     for( ii=cwid->nrow ; ii < nclu ; ii++ ) XtManageChild( cwid->clu_rc[ii] ) ;
   } else if( cwid->nrow > nclu ){
     for( ii=nclu ; ii < cwid->nrow ; ii++ ) XtUnmanageChild( cwid->clu_rc[ii] ) ;
   }
   cwid->nrow = nclu ;  /* # of managed rows */

   /* change labels for each row */

   for( ii=0 ; ii < nclu ; ii++ ){
     switch( cwid->coord_mode ){
       default:
       case PEAK_MODE:  xx=cld[ii].xpk; yy=cld[ii].ypk; zz=cld[ii].zpk; break;
       case CMASS_MODE: xx=cld[ii].xcm; yy=cld[ii].ycm; zz=cld[ii].zcm; break;
     }
     MAT44_VEC( im3d->fim_now->daxes->ijk_to_dicom , xx,yy,zz , px,py,pz ) ;
     px *= GLOBAL_library.cord.xxsign ;
     py *= GLOBAL_library.cord.yysign ;
     pz *= GLOBAL_library.cord.zzsign ;
     if( cld[ii].nvox <= 99999 )
       sprintf(line,"%2d:%5d vox %+6.1f %+6.1f %+6.1f",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     else if( cld[ii].nvox <= 999999 )
       sprintf(line,"%2d:%6dvox %+6.1f %+6.1f %+6.1f",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     else if( cld[ii].nvox <= 9999999 )
       sprintf(line,"%2d:%7dvx %+6.1f %+6.1f %+6.1f",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     else if( cld[ii].nvox <= 99999999 )
       sprintf(line,"%2d:%8dv %+6.1f %+6.1f %+6.1f",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     else
       sprintf(line,"%2d:%9d %+6.1f %+6.1f %+6.1f",
               ii+1,cld[ii].nvox , px,py,pz ) ;
     MCW_set_widget_label( cwid->clu_lab[ii] , line ) ;

     rrr = get_alpha_string( cld[ii].nvox , pval , im3d ) ;
     MCW_set_widget_label( cwid->clu_alph_lab[ii] , rrr ) ;

   } /* end of loop over widget rows */

   SET_INDEX_LAB(im3d) ;

   if( !cwid->receive_on ){
     AFNI_receive_init(im3d, RECEIVE_VIEWPOINT_MASK,
                       AFNI_clus_viewpoint_CB, im3d, "AFNI_clus_viewpoint_CB") ;
     cwid->receive_on = 1 ;
   }

   if( do_wami ) SENSITIZE( cwid->whermask_pb ,                /* 04 Aug 2010 */
                            (im3d->vinfo->view_type == VIEW_TALAIRACH_TYPE) ) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/*! Callback for "Done" button for cluster display panel. */

static void AFNI_clus_done_CB( Widget w , XtPointer cd, XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;

ENTRY("AFNI_clus_done_CB") ;

   if( !IM3D_VALID(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ;
   if( cwid != NULL ){
     cwid->dset = NULL ; AFNI_clus_dsetlabel(im3d) ;
     cwid->hbot = cwid->htop = 0.0f ; MCW_set_bbox( cwid->histsqrt_bbox , 0 ) ;
     XtUnmapWidget(cwid->wtop) ; cwid->is_open = 0 ;
     DESTROY_CLARR(im3d->vwid->func->clu_list) ;
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Another way to force the hiding of the cluster report */

void AFNI_clus_popdown( Three_D_View *im3d )
{
   AFNI_clus_done_CB(NULL,(XtPointer)im3d,NULL) ;
}

/*---------------------------------------------------------------------------*/
/* Called when the user finally makes up his pitiful little mind. */

static AFNI_dataset_choose_stuff cdds = { 0, NULL, NULL } ;

static void AFNI_clus_finalize_dataset_CB( Widget w, XtPointer cd, MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   THD_3dim_dataset *dset ;
   int ival ;

ENTRY("AFNI_clus_finalize_dataset_CB") ;
   if( !IM3D_OPEN(im3d) || cbs == NULL ){ POPDOWN_strlist_chooser; EXRETURN; }
   cwid = im3d->vwid->func->cwid ;
   if( cwid == NULL || !cwid->is_open ) { POPDOWN_strlist_chooser; EXRETURN; }

   ival = cbs->ival ;
   if( ival < 0 || ival >= cdds.ndset ) EXRETURN ;
   dset = cdds.dset[ival] ;
   cwid->dset = ISVALID_DSET(dset) ? dset : NULL ;
   AFNI_clus_dsetlabel(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_clus_finalize_scat1D_CB( Widget w, XtPointer cd, MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   MRI_IMAGE *tim ; int ival ;

ENTRY("AFNI_clus_finalize_scat1D_CB") ;
   if( !IM3D_OPEN(im3d) || cbs == NULL ){ POPDOWN_timeseries_chooser; EXRETURN; }
   cwid = im3d->vwid->func->cwid ;
   if( cwid == NULL || !cwid->is_open ) { POPDOWN_timeseries_chooser; EXRETURN; }

   ival = cbs->ival ;
   if( ival >= 0 && ival < IMARR_COUNT(GLOBAL_library.timeseries) )
     cwid->splotim = IMARR_SUBIMAGE(GLOBAL_library.timeseries,ival) ;
   AFNI_clus_dsetlabel(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Callback for all pushbuttons (except 'Done') on the report window. */

static void AFNI_clus_action_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   int nclu , ii ;
   mri_cluster_detail *cld ;

ENTRY("AFNI_clus_action_CB") ;
   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   /*--------- dataset chooser ---------*/

   if( w == cwid->dataset_pb ){
     int vv = im3d->vinfo->view_type ;
     THD_3dim_dataset *dset ;

     if( cdds.dset != NULL ) free((void *)cdds.dset) ;
     cdds.ndset = 0 ;
     cdds.dset = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)
                                             *im3d->ss_now->num_dsset  ) ;
     cdds.cb = AFNI_clus_finalize_dataset_CB ;
     for( ii=0 ; ii < im3d->ss_now->num_dsset ; ii++ ){
       dset = GET_SESSION_DSET(im3d->ss_now, ii, vv) ;
/*     dset = im3d->ss_now->dsset_xform_table[ii][vv] ;*/
       if( ISVALID_DSET(dset)                          &&  /* qualifications */
           DSET_NVOX(dset) == DSET_NVOX(im3d->fim_now) &&
           DSET_INMEMORY(dset)                           )
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

   /*--------- Scat.1D button ----------*/

   if( w == cwid->splot_pb && IMARR_COUNT(GLOBAL_library.timeseries) > 0 ){
     int init_ts = AFNI_ts_in_library( cwid->splotim ) ;
     MCW_choose_timeseries( cwid->top_lab , "Scatterplot x-axis" ,
                                   GLOBAL_library.timeseries , init_ts ,
                                   AFNI_clus_finalize_scat1D_CB , (XtPointer)im3d ) ;
     EXRETURN ;
   }

   if( w == cwid->splot_clear_pb ){
     cwid->splotim = NULL ;
     AFNI_clus_dsetlabel(im3d) ;
     EXRETURN ;
   }

   /*--------- SaveTabl button ---------*/

   if( w == cwid->savetable_pb ){
     char fnam[128+THD_MAX_NAME] , *ppp ; FILE *fp ; int ff ;
     float px,py,pz , mx,my,mz , xx,yy,zz ;

     nclu = im3d->vwid->func->clu_num ;
     cld  = im3d->vwid->func->clu_det ;
     if( nclu == 0 || cld == NULL ) EXRETURN ;

     ppp = XmTextFieldGetString( cwid->prefix_tf ) ;
     if( !THD_filename_pure(ppp) ) ppp = "Clust" ;
     if( strcmp(ppp,"-") != 0 ){
       char *dnam = DSET_DIRNAME(im3d->fim_now) ;
       sprintf(fnam,"%s%s_table.1D",dnam,ppp) ;
       ff = THD_is_file(fnam) ;
       fp = fopen(fnam,"w") ;
     } else {
       fp = stdout ; ff = 0 ;
     }
     if( fp == NULL ){
       ERROR_message("Can't open file %s for writing",fnam) ;
     } else {
       ppp = AFNI_clus_3dclust(im3d) ;
       if( ppp != NULL )
         fprintf(fp,"# AFNI interactive cluster table\n# %s\n" , ppp ) ;
       fprintf(fp, "#Coordinate order = %s\n"
                   "#Voxels  CM x   CM y   CM z  Peak x Peak y Peak z\n"
                   "#------ ------ ------ ------ ------ ------ ------\n" ,
                  (GLOBAL_library.cord.xxsign < 0) ? "LPI" : "RAI" ) ;
       for( ii=0 ; ii < nclu ; ii++ ){
         xx=cld[ii].xpk; yy=cld[ii].ypk; zz=cld[ii].zpk;
         MAT44_VEC( im3d->fim_now->daxes->ijk_to_dicom , xx,yy,zz , px,py,pz ) ;
         xx=cld[ii].xcm; yy=cld[ii].ycm; zz=cld[ii].zcm;
         MAT44_VEC( im3d->fim_now->daxes->ijk_to_dicom , xx,yy,zz , mx,my,mz ) ;
         px *= GLOBAL_library.cord.xxsign ; mx *= GLOBAL_library.cord.xxsign ;
         py *= GLOBAL_library.cord.yysign ; my *= GLOBAL_library.cord.yysign ;
         pz *= GLOBAL_library.cord.zzsign ; mz *= GLOBAL_library.cord.zzsign ;
         fprintf(fp,"%7d %+6.1f %+6.1f %+6.1f %+6.1f %+6.1f %+6.1f\n" ,
                 cld[ii].nvox , mx,my,mz , px,py,pz ) ;
       }
       if( fp != stdout ){
         fclose(fp) ;
         if( ff ) WARNING_message("Over-wrote file %s",fnam) ;
         else     INFO_message   ("Wrote file %s"     ,fnam) ;
       }
     }
     EXRETURN ;
   }

   /*--------- SaveMsk button ---------*/

   if( w == cwid->savemask_pb || w == cwid->whermask_pb ){  /* 01 May 2008 */
     char pref[128] , *ppp , *cmd ;
     THD_3dim_dataset  *fset = im3d->fim_now , *mset ;
     MCW_cluster_array *clar = im3d->vwid->func->clu_list ;
     MCW_cluster *cl ;
     short *mask ; int ii,jj,nx,ny,nxy,nz,ijk ;
     int do_wami = (w == cwid->whermask_pb && wherprog != NULL) ;
     int jtop ;

     nclu = im3d->vwid->func->clu_num ;
     cld  = im3d->vwid->func->clu_det ;
     if( nclu == 0 || cld == NULL ) EXRETURN ;

     ppp = XmTextFieldGetString( cwid->prefix_tf ) ;
     if( !THD_filename_pure(ppp) || strcmp(ppp,"-") == 0 ) ppp = "Clust" ;
     sprintf(pref,"%s_mask",ppp) ;
     cmd = AFNI_clus_3dclust(im3d) ;

     mset = EDIT_empty_copy(fset) ;
     tross_Copy_History(fset,mset) ;
     tross_Append_History(mset,"== Interactive clusterize mask:") ;
     tross_Append_History(mset,cmd) ;
     EDIT_dset_items( mset ,
                        ADN_prefix    , pref ,
                        ADN_nvals     , 1    ,
                        ADN_ntt       , 0    ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                        ADN_func_type , FUNC_BUCK_TYPE ,
                      ADN_none ) ;
     nx = DSET_NX(mset); ny = DSET_NY(mset); nxy = nx*ny; nz = DSET_NZ(mset);
     EDIT_substitute_brick( mset , 0 , MRI_short , NULL ) ;
     mask = DSET_BRICK_ARRAY( mset , 0 ) ;
     jtop = clar->num_clu ;
     if( do_wami ){
       int etop = (int)AFNI_numenv("AFNI_CLUSTER_WAMIMAX") ;
            if( etop <  1 ) etop = 20 ;
       else if( etop > 99 ) etop = 99 ;
       if( jtop > etop ) jtop = etop ;
     }
     for( jj=0 ; jj < jtop ; jj++ ){
       cl = clar->clar[jj] ;
       for( ii=0 ; ii < cl->num_pt ; ii++ ){
         ijk = THREE_TO_IJK( cl->i[ii] , cl->j[ii] , cl->k[ii] , nx,nxy ) ;
         mask[ijk] = jj+1 ;
       }
     }
     THD_force_ok_overwrite(1) ;
     INFO_message("Writing mask dataset %s",DSET_BRIKNAME(mset)) ;
     DSET_write(mset) ;
     DSET_delete(mset) ;
     THD_force_ok_overwrite(0) ;

#undef  WSIZ
#define WSIZ 4096
     if( do_wami ){  /* 04 Aug 2010 */
       char *wout , ct[64] ; FILE *fp ; int inv ;
       SHOW_AFNI_PAUSE ;
       MCW_invert_widget(cwid->whermask_pb) ; inv = 1 ;
       wout = (char *)malloc(sizeof(char)*WSIZ) ;
       sprintf(wout,"%s -omask %s",wherprog,DSET_HEADNAME(mset)) ;
       if( jtop >= clar->num_clu ) strcpy (ct," ") ;
       else                        sprintf(ct," [first %d clusters]",jtop) ;
       INFO_message("Running '%s'%s" , wout , ct ) ;
       fp = popen( wout , "r" ) ;
       if( fp == NULL ){
         (void)MCW_popup_message(w," \n*** Can't run whereami command? ***\n ",
                                 MCW_USER_KILL) ;
       } else {
         wout[0] = '\0' ;
         while( fgets(wout+strlen(wout),WSIZ-2,fp) != NULL ){
           wout = (char *)realloc(wout,sizeof(char)*(strlen(wout)+WSIZ)) ;
           /*** MCW_invert_widget(cwid->whermask_pb) ; inv = !inv ; ***/
         }
         (void)pclose(fp) ;
         MCW_textwin_setbig(0) ;
         (void)new_MCW_textwin(w,wout,TEXT_READONLY) ;
       }
       free(wout) ;
       if( inv ) MCW_invert_widget(cwid->whermask_pb) ;
       SHOW_AFNI_READY ;
     }

     EXRETURN ;
   }

   /*--------- 3dclust button ---------*/

   if( w == cwid->clust3d_pb ){
     char *cmd = AFNI_clus_3dclust(im3d) ;
     if( cmd != NULL ) INFO_message("3dclust command:\n %s",cmd) ;
     else              ERROR_message("Can't generate 3dclust command!") ;
     EXRETURN ;
   }

   /*------ Hist range button ------*/

   if( w == cwid->histrange_pb ){
     static char *lvec[2] = { "Minimum" , "Maximum" } ;
     float fvec[2] ;
     if( cwid->hbot < cwid->htop ){ fvec[0]=cwid->hbot; fvec[1]=cwid->htop; }
     else                         { fvec[0] = fvec[1] = 0.0f;               }
     MCW_choose_vector( cwid->top_lab ,
                        "Set range of values to\n"
                        "include in Histogram. \n"
                        "[ Min=Max=0 means all]\n"
                        "[ data will be used  ]\n"
                        "----------------------" ,
                        2 , lvec,fvec ,
                        AFNI_histrange_choose_CB , (XtPointer)im3d ) ;
     EXRETURN ;
   }

   /*------ scan button list, see if widget matches one of them ------*/

   nclu = im3d->vwid->func->clu_num ;
   cld  = im3d->vwid->func->clu_det ;
   if( nclu == 0 || cld == NULL ) EXRETURN ;

   for( ii=0 ; ii < nclu ; ii++ ){

     /*-------- Jump to the cluster peak or cmass --------*/

     if( w == cwid->clu_jump_pb[ii] ){

       float px,py,pz , xx,yy,zz ;
       switch( cwid->coord_mode ){
         default:
         case PEAK_MODE:  xx=cld[ii].xpk; yy=cld[ii].ypk; zz=cld[ii].zpk; break;
         case CMASS_MODE: xx=cld[ii].xcm; yy=cld[ii].ycm; zz=cld[ii].zcm; break;
       }
       MAT44_VEC( im3d->fim_now->daxes->ijk_to_dicom , xx,yy,zz , px,py,pz ) ;
       AFNI_jumpto_dicom( im3d , px,py,pz ) ;
       EXRETURN ;

     /*----------- Process the cluster data -----------*/

     } else if( w == cwid->clu_plot_pb[ii] || w == cwid->clu_save_pb[ii] ){

       int dosave = (w == cwid->clu_save_pb[ii]) ;  /* save OR plot */
       int domean = (cwid->aver_av->ival == 0) ;
       int domedn = (cwid->aver_av->ival == 1) ;
       int dopc   = (cwid->aver_av->ival == 2) ;
       int dohist = (cwid->aver_av->ival == 3) ;
       int dosmea = (cwid->aver_av->ival == 4) ;
       int dosall = (cwid->aver_av->ival == 5) ;
       int doscat = (dosall || dosmea) ;
       MRI_IMARR *imar ; MRI_IMAGE *im=NULL ; int nx,ibot,itop ;
       MRI_IMAGE *sim=NULL ;

       SHOW_AFNI_PAUSE ;

       imar = AFNI_cluster_timeseries(im3d,ii) ;
       if( imar == NULL || IMARR_COUNT(imar) < 1 ){
         MCW_popup_message( w , " \n"
                                "** Can't get data!!! **\n"
                                "** Need Aux Dataset! **\n " ,
                            MCW_USER_KILL | MCW_TIMER_KILL ) ;
         SHOW_AFNI_READY; EXRETURN ;
       }

       if( doscat && dosave ){
         MCW_popup_message( w , " \n"
                                "** Can't use Scatterplot **\n"
                                "** and Save together :-( **\n " ,
                            MCW_USER_KILL | MCW_TIMER_KILL ) ;
         SHOW_AFNI_READY; EXRETURN ;
       }

       nx = IMARR_SUBIM(imar,0)->nx ;  /* number of voxel time series */
       ibot = cwid->from_av->ival ; itop = cwid->to_av->ival ;
       if( ibot >= nx ) ibot = 0 ;
       if( itop < ibot || itop >= nx ) itop = nx-1 ;

       { static float rrr[3] = { 0.6f , 0.0f , 0.1f } ;
         static float ggg[3] = { 0.0f , 0.5f , 0.1f } ;
         static float bbb[3] = { 0.1f , 0.0f , 0.7f } ;
         plot_ts_setcolors( 3 , rrr,ggg,bbb ) ;
       }

       /*---------- build histogram ----------*/

       if( dohist ){
         float *far,*hbin, hbot,htop,val,sbin ; int jj,kk,nbin,ih, nval , dosqrt ;
         hbot = cwid->hbot ; htop = cwid->htop ;   /* range from user */
         if( hbot >= htop ){            /* scan data for range to use */
           hbot = 1.e+33 ; htop = -hbot ;
           for( kk=0 ; kk < IMARR_COUNT(imar) ; kk++ ){
             far = MRI_FLOAT_PTR( IMARR_SUBIM(imar,kk) ) ;
             for( jj=ibot ; jj <= itop ; jj++ ){
               val = far[jj] ;
               if( hbot > val ) hbot = val ;
               if( htop < val ) htop = val ;
             }
           }
           if( hbot >= htop ){ DESTROY_IMARR(imar); SHOW_AFNI_READY; EXRETURN; } /* bad */
         }
         if( (int)hbot == hbot && (int)htop == htop ){
           nbin = htop - hbot ;
           if( nbin < 8 ){ nbin = 8 ; }
           else          { while( nbin > 1000 ) nbin /= 2 ; }
         } else {
           nbin = 100 ;
         }
         kk = (int)sqrt((double)((itop-ibot+1)*IMARR_COUNT(imar))) ;
         if( nbin > kk ) nbin = MAX(kk,4) ;
         sbin = 0.999999f * nbin / (htop-hbot) ;
         hbin = (float *)calloc(sizeof(float),(nbin+1)) ;
         for( nval=kk=0 ; kk < IMARR_COUNT(imar) ; kk++ ){
           far = MRI_FLOAT_PTR( IMARR_SUBIM(imar,kk) ) ;
           for( jj=ibot ; jj <= itop ; jj++ ){
             val = far[jj] ; if( val < hbot || val > htop ) continue ;
             ih = (int)(sbin*(val-hbot)) ; hbin[ih]++ ; nval++ ;
           }
         }

         dosqrt = !dosave && (MCW_val_bbox(cwid->histsqrt_bbox)==1) ;
         if( dosqrt )
           for( ih=0 ; ih<=nbin ; ih++ ) hbin[ih] = sqrtf(hbin[ih]);

         if( !dosave ){   /*----- plot histogram -----*/

           char xlab[64] , ylab[64] , tlab[THD_MAX_NAME+2] ;
           sprintf(xlab,"Data Value [%d bins; %d values in range]",nbin,nval) ;
           sprintf(ylab,"Cluster #%d = %d vox", ii+1 , IMARR_COUNT(imar) ) ;
           strcat(ylab,(dosqrt)?" [SqrtHist]" : " [Hist]") ;
           sprintf(tlab,"\\noesc %s[%d..%d]",
                   THD_trailname(DSET_HEADNAME(cwid->dset),SESSTRAIL+1) , ibot,itop ) ;
           plot_ts_xypush(0,-1) ; plot_ts_setthik(0.005f) ;
           PLUTO_histoplot_f( nbin,hbot,htop , hbin , xlab,ylab,tlab , 0,NULL ) ;

         } else {         /*----- save histogram -----*/

           char fnam[128] , *ppp ; FILE *fp ; int ff ;

           ppp = XmTextFieldGetString( cwid->prefix_tf ) ;
           if( !THD_filename_pure(ppp) ) ppp = "Clust" ;
           if( strcmp(ppp,"-") != 0 ){
             sprintf(fnam,"%s_%02d_hist.1D",ppp,ii+1) ;
             ff = THD_is_file(fnam) ;
             fp = fopen(fnam,"w") ;
           } else {
             fp = stdout ; ff = 0 ;
           }
           if( fp == NULL ){
             ERROR_message("Can't open file %s for writing",fnam) ;
           } else {
             ppp = AFNI_clus_3dclust(im3d) ;
             if( ppp != NULL )
               fprintf(fp,"# Histogram of %s[%d..%d]\n"
                          "# over Cluster #%d from\n"
                          "# %s\n" ,
                       DSET_HEADNAME(im3d->vwid->func->cwid->dset) ,
                       ibot , itop , ii+1 , ppp ) ;
             fprintf(fp,"# min data value = %g\n"
                        "# max data value = %g\n" , hbot,htop ) ;
             fprintf(fp,"# num of voxels  = %d\n"
                        "# num of values  = %d\n" , IMARR_COUNT(imar),nval ) ;
             for( jj=0 ; jj < nbin ; jj++ ) fprintf(fp,"%7d\n",(int)hbin[jj]) ;
             if( fp != stdout ){
               fclose(fp) ;
               if( ff ) WARNING_message("Over-wrote file %s",fnam) ;
               else     INFO_message   ("Wrote file %s"     ,fnam) ;
             }
           }

         }

         free((void *)hbin); DESTROY_IMARR(imar); SHOW_AFNI_READY; EXRETURN;

       } /* done with histogram-ification */

       /*------------ time series processing ------------*/

       if( (domean || dopc || domedn || dosmea) && itop <= ibot ){
         MCW_popup_message( w , " \n"
                                "** Need at least two   **\n"
                                "** time series indexes **\n"
                                "** for graphing that!  **\n " ,
                            MCW_USER_KILL | MCW_TIMER_KILL ) ;
         DESTROY_IMARR(imar) ; SHOW_AFNI_READY; EXRETURN ;
       }

       /*--- extract single vector for display or save ---*/

       if( IMARR_COUNT(imar) == 1 ){   /* should not transpire */
         im = IMARR_SUBIM(imar,0) ;
       } else if( dopc ){              /*-------- PC#1 --------*/
         im = mri_pcvector( imar , ibot,itop ) ;
       } else if( domean ){            /*-------- Mean --------*/
         im = mri_meanvector( imar , ibot,itop ) ;
         if( !dosave && AFNI_yesenv("AFNI_CLUSTER_EBAR") )
           sim = mri_MMBvector( imar,ibot,itop,2 ) ;
       } else if( domedn ){            /*-------- Median --------*/
         im = mri_MMBvector( imar , ibot,itop,0 ) ;
         if( !dosave && AFNI_yesenv("AFNI_CLUSTER_EBAR") )
           sim = mri_MMBvector( imar,ibot,itop,2 ) ;
       } else if( doscat ){  /* scatterplot */
         float *xar, *yar ; int nix, niy, nixy, jj,kk ;
         float a=0,b=0,pcor=0 ;
         char xlab[64] , ylab[64] , tlab[THD_MAX_NAME+2] ;
         if( dosmea ){
           im = mri_meanvector( imar , ibot,itop ) ; xar = MRI_FLOAT_PTR(im) ;
           nix = im->nx ; niy = 1 ; nixy = nix*niy ;
           yar = (float *)malloc(sizeof(float)*nixy) ;
           for( jj=0 ; jj < nix ; jj ++ ) yar[jj] = xar[jj] ;
           mri_free(im) ; im = NULL ;
         } else if( dosall ){
           nix = (itop-ibot+1) ; niy = IMARR_COUNT(imar) ; nixy = nix*niy ;
           yar = (float *)malloc(sizeof(float)*nixy) ;
           for( kk=0 ; kk < niy ; kk++ ){
             xar= MRI_FLOAT_PTR(IMARR_SUBIM(imar,kk)) ;
             for( jj=0 ; jj < nix ; jj++ ) yar[jj+kk*nix] = xar[jj] ;
           }
         }
         xar = (float *)malloc(sizeof(float)*nixy) ;
         if( cwid->splotim != NULL && cwid->splotim->nx >= nix+ibot ){
           float *spar = MRI_FLOAT_PTR(cwid->splotim) ;
           for( kk=0 ; kk < niy ; kk++ ){
             for( jj=0 ; jj < nix ; jj++ ) xar[jj+kk*nix] = spar[jj+ibot] ;
           }
           sprintf(xlab,"%.62s",cwid->splotim->name) ;
         } else {
           for( kk=0 ; kk < niy ; kk++ )
             for( jj=0 ; jj < nix ; jj++ ) xar[jj+kk*nix] = jj+ibot ;
           strcpy(xlab,"Index") ;
         }
         if( niy == 1 ){
           float xbar=0,ybar=0, xq=0,yq=0,xyq=0 ;
           for( jj=0 ; jj < nix ; jj++ ){ xbar += xar[jj]; ybar += yar[jj]; }
           xbar /= nix ; ybar /= nix ;
           for( jj=0 ; jj < nix ; jj++ ){
             xq  += (xar[jj]-xbar)*(xar[jj]-xbar) ;
             yq  += (yar[jj]-ybar)*(yar[jj]-ybar) ;
             xyq += (xar[jj]-xbar)*(yar[jj]-ybar) ;
           }
           if( xq > 0.0f && yq > 0.0f ){
             pcor = xyq/sqrtf(xq*yq); a = xyq/xq; b = (xq*ybar-xbar*xyq)/xq;
           }
         }
         sprintf(ylab,"Cluster #%d = %d voxels",ii+1,IMARR_COUNT(imar)) ;
         sprintf(tlab,"\\noesc %s[%d..%d]",
                 THD_trailname(DSET_HEADNAME(cwid->dset),SESSTRAIL),ibot,itop);
         if( pcor != 0.0f ) sprintf(tlab+strlen(tlab)," R=%.3f",pcor) ;
         PLUTO_set_xypush(1,0) ;
         PLUTO_scatterplot( nixy,xar,yar , xlab,ylab,tlab , a,b ) ;
         PLUTO_set_xypush(1,1) ;
         free(xar) ; free(yar) ;
       }  /* end of scatterplot */
       if( im != NULL ){
         if( !dosave ){                       /* Plotting (to rule the world) */
           char ylab[64] , tlab[THD_MAX_NAME+2] ;
           float *far = MRI_FLOAT_PTR(im) , *xax ;
           int jj ;
           sprintf(ylab,"%s: Cluster #%d = %d voxels",
                   (dopc) ? "PC#1" : (domean) ? "Mean" : "Median" ,
                   ii+1 , IMARR_COUNT(imar) ) ;
           sprintf(tlab,"\\noesc %s[%d..%d]",
                   THD_trailname(DSET_HEADNAME(cwid->dset),SESSTRAIL+1),
                   ibot,itop) ;
           plot_ts_xypush(1,0) ; plot_ts_setthik(0.006f) ;
           xax = (float *)malloc(sizeof(float)*im->nx) ;
           for( jj=0 ; jj < im->nx ; jj++ ) xax[jj] = ibot+jj ;
           if( sim == NULL ){
              plot_ts_lab( im3d->dc->display ,
                           im->nx , xax , 1 , &far ,
                           "TR index" , ylab , tlab , NULL , NULL ) ;
           } else {
              float *sar= MRI_FLOAT_PTR(sim) , fac=2.0f/sqrtf(IMARR_COUNT(imar));
              for( jj=0 ; jj < sim->nx ; jj++ ) sar[jj] *= fac ;
              plot_ts_ebar_win( im3d->dc->display ,
                                im->nx , xax , far , sar ,
                                "TR index" , ylab , tlab , NULL ) ;
           }
           free((void *)xax) ;

         } else {                                       /* Saving (the world) */
           char fnam[128] , *ppp ; int jj,kk,nx,ny,ff ; float *far ; FILE *fp ;
           ppp = XmTextFieldGetString( cwid->prefix_tf ) ;
           if( !THD_filename_pure(ppp) ) ppp = "Clust" ;
           if( strcmp(ppp,"-") != 0 ){
             sprintf(fnam,"%s_%02d_%s.1D",ppp,ii+1,(dopc)?"pc":"mean") ;
             ff = THD_is_file(fnam) ;
             fp = fopen(fnam,"w") ;
           } else {
             fp = stdout ; ff = 0 ;
           }
           if( fp == NULL ){
             ERROR_message("Can't open file %s for writing",fnam) ;
           } else {
             ppp = AFNI_clus_3dclust(im3d) ;
             if( ppp != NULL )
               fprintf(fp,"# %s of %s[%d..%d]\n"
                          "# over Cluster #%d from\n"
                          "# %s\n" ,
                       (dopc)?"pc#1":"mean" ,
                       DSET_HEADNAME(im3d->vwid->func->cwid->dset) ,
                       ibot , itop , ii+1 , ppp ) ;

             nx = im->nx ; ny = im->ny ; far = MRI_FLOAT_PTR(im) ;
             for( jj=0 ; jj < nx ; jj++ ){
               for( kk=0 ; kk < ny ; kk++ ){
                 fprintf(fp," %14g",far[jj+kk*ny]) ;
               }
               fprintf(fp,"\n") ;
             }
             if( fp != stdout ){
               fclose(fp) ;
               if( ff ) WARNING_message("Over-wrote file %s",fnam) ;
               else     INFO_message   ("Wrote file %s"     ,fnam) ;
             }
           }
         }
         if( im != IMARR_SUBIM(imar,0) ) mri_free(im) ;
         if( sim != NULL ) mri_free(sim) ;
       }
       DESTROY_IMARR(imar) ; SHOW_AFNI_READY; EXRETURN ;

     /*--------- flash the voxels for this cluster ---------*/

     } else if( w == cwid->clu_flsh_pb[ii] ){

       THD_3dim_dataset  *fset = im3d->fim_now ;
       MCW_cluster_array *clar = im3d->vwid->func->clu_list ; int jj ;
       STATUS("flashing") ;
       if( ISVALID_DSET(fset) && fset->dblk->vedim != NULL && clar != NULL ){
         double tz , tt ; int ss ;
         MRI_IMAGE *vm = fset->dblk->vedim ;
         im3d->vedskip = 1 ; tz = PLUTO_elapsed_time() ;
         for( jj=0 ; jj < 3 ; jj++ ){
           MCW_invert_widget(w) ;
           MCW_vol_to_cluster(vm->nx,vm->ny,vm->nz ,
                              vm->kind,mri_data_pointer(vm) , clar->clar[ii] );
           AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_ALL ) ;
           tt = PLUTO_elapsed_time() ; ss = 66-(int)(tt-tz) ; tz = tt ;
           if( ss > 0 ) NI_sleep(ss) ;
           MCW_invert_widget(w) ;
           MCW_cluster_to_vol(vm->nx,vm->ny,vm->nz ,
                              vm->kind,mri_data_pointer(vm) , clar->clar[ii] );
           AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_ALL ) ;
           tt = PLUTO_elapsed_time() ; ss = 66-(int)(tt-tz) ; tz = tt ;
           if( ss > 0 ) NI_sleep(ss) ;
         }
         im3d->vedskip = 0 ;
       }
       EXRETURN ;

     } /* end of flash */

   } /*---------- end of loop over button rows ----------*/

   /* this should never be reached, unless the code is haunted */

   fprintf(stderr,"** Unknown button? **\n\a") ; EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Get the time series from the points in a cluster */

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
/* Callback for arrowvals on the cluster report panel. */

static void AFNI_clus_av_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;

ENTRY("AFNI_clus_av_CB") ;
   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   if( av == cwid->cmode_av ){
     cwid->coord_mode = av->ival ;
     AFNI_clus_update_widgets(im3d) ;  /* redisplay coordinates */
     EXRETURN ;
   }

   fprintf(stderr,"** Unknown button? **\n") ; EXRETURN ;
}

/*---------------------------------------------------------------------------*/

#undef  THBOT
#undef  THTOP
#undef  THBIG
#define THBIG    1.e+9f
#define THBOT(t) ((thrsign==0 || thrsign==2) ? (-(t)) : (-THBIG))
#define THTOP(t) ((thrsign==0 || thrsign==1) ? (t)    :  (THBIG))

/* return the equivalent 3dclust command string */

static char * AFNI_clus_3dclust( Three_D_View *im3d )
{
   static char cmd[3333] ;
   VEDIT_settings vednew ;
   float thr,rmm,vmul,thb,tht ;
   int thrsign,posfunc,ithr , ival ;

   if( !IM3D_OPEN(im3d) ) return NULL ;

   vednew = im3d->vedset ;

   ival    =      vednew.ival     ;
   ithr    = (int)vednew.param[0] ;
   thr     =      vednew.param[1] ;
   rmm     =      vednew.param[2] ;
   vmul    =      vednew.param[3] ;
   thrsign = (int)vednew.param[4] ;
   posfunc = (int)vednew.param[5] ;

   thb = THBOT(thr) ; tht = THTOP(thr) ;

   sprintf(cmd,"3dclust -1Dformat -nosum -1dindex %d -1tindex %d",ival,ithr) ;

   if( posfunc )
     strcat(cmd," -1noneg") ;

   if( thb < tht )
     sprintf(cmd+strlen(cmd)," -2thresh %g %g",thb,tht) ;

   if( rmm <= 0.0f ){
     strcat(cmd," -dxyz=1") ;
     rmm = rintf(rmm) ;
          if( rmm >= -1.0f ) rmm = 1.01f ;
     else if( rmm >= -2.0f ) rmm = 1.44f ;
     else                    rmm = 1.75f ;
     if( vmul < 2.0f ) vmul = 2.0f ;
   } else {
     float vmin=2.0f*DSET_VOXVOL(im3d->fim_now) ;
     vmul = MAX(vmin,vmul) ;
   }

   sprintf(cmd+strlen(cmd)," %g %g %s",
           rmm , vmul , DSET_HEADNAME(im3d->fim_now) ) ;

   return cmd ;
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

/*-----------------------------------------------------------------*/

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

/*-----------------------------------------------------------------*/

void AFNI_throlay1_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   int qq , pp ;

ENTRY("AFNI_throlay1_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   qq = im3d->vinfo->thr_olay1 ;
   pp = MCW_val_bbox( im3d->vwid->func->thr_olay1_bbox ) ;

   if( pp != qq ){
     im3d->vinfo->thr_olay1 = pp ;
     MCW_invert_widget(im3d->vwid->func->thr_buck_av->wrowcol) ;
     AFNI_enforce_throlay1(im3d) ;
   }
   EXRETURN ;
}

/*-----------------------------------------------------------------*/

void AFNI_enforce_throlay1( Three_D_View *im3d )
{
   int ithr ;

ENTRY("AFNI_enforce_throlay1") ;

   if( !IM3D_VALID(im3d) || !ISVALID_DSET(im3d->fim_now)
                         || !im3d->vinfo->thr_olay1      ) EXRETURN ;

   ithr = im3d->vinfo->fim_index + 1 ;
   if( ithr >= DSET_NVALS(im3d->fim_now) ) EXRETURN ;
   AFNI_set_thr_index(im3d,ithr) ;
   if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;
   EXRETURN ;
}

/*****************************************************************************/
/********** Stuff for cluster size statistics ********************************/

/*----------------------------------------------------------------------------*/

void CLU_free_table( CLU_threshtable *ctab )
{
ENTRY("CLU_free_table") ;
   if( ctab != NULL ){
     if( ctab->pthr != NULL ) free(ctab->pthr) ;
     if( ctab->athr != NULL ) free(ctab->athr) ;
     if( ctab->cluthr != NULL ){
       int ii ;
       for( ii=0 ; ii < ctab->npthr ; ii++ ) free(ctab->cluthr[ii]) ;
       free(ctab->cluthr) ;
     }
     free(ctab) ;
   }
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static CLU_threshtable * format_cluster_table( NI_element *nel )
{
   CLU_threshtable *ctab ;
   NI_float_array *flar ;
   char *atr ; int iathr,ipthr , nathr,npthr ; float *vec ;

ENTRY("format_cluster_table") ;

   if( nel == NULL ) RETURN(NULL);

   ctab = (CLU_threshtable *)malloc(sizeof(CLU_threshtable)) ;

   atr = NI_get_attribute( nel , "pthr" ) ;
   if( atr == NULL ){
     free(ctab) ; NI_free_element(nel) ; RETURN(NULL);
   }
   flar = NI_decode_float_list(atr,",") ;
   if( flar == NULL || flar->num < 3 ){
     free(ctab) ; NI_free_element(nel) ; RETURN(NULL);
   }
   npthr = ctab->npthr = flar->num ;
   ctab->pthr  = (float *)malloc(sizeof(float)*ctab->npthr) ;
   memcpy( ctab->pthr , flar->ar , sizeof(float)*ctab->npthr ) ;
   NI_delete_float_array(flar) ;

   atr = NI_get_attribute( nel , "athr" ) ;
   if( atr == NULL ){
     free(ctab->pthr) ; free(ctab) ; NI_free_element(nel) ; RETURN(NULL);
   }
   flar = NI_decode_float_list(atr,",") ;
   if( flar == NULL || flar->num < 3 ){
     free(ctab->pthr) ; free(ctab) ; NI_free_element(nel) ; RETURN(NULL);
   }
   nathr = ctab->nathr = flar->num ;
   ctab->athr  = (float *)malloc(sizeof(float)*ctab->nathr) ;
   memcpy( ctab->athr , flar->ar , sizeof(float)*ctab->nathr ) ;
   NI_delete_float_array(flar) ;

   ctab->cluthr = (float **)malloc(sizeof(float *)*npthr) ;
   for( ipthr=0 ; ipthr < npthr ; ipthr++ )
     ctab->cluthr[ipthr] = (float *)malloc(sizeof(float)*nathr) ;

   for( iathr=0 ; iathr < nathr ; iathr++ ){
     vec = (float *)nel->vec[iathr] ;
     for( ipthr=0 ; ipthr < npthr ; ipthr++ )
       ctab->cluthr[ipthr][iathr] = vec[ipthr] ;
   }

   RETURN(ctab);
}

/*----------------------------------------------------------------------------*/
/* Input comes from 3dClustSim -niml */

void CLU_setup_alpha_tables( Three_D_View *im3d )
{
   THD_3dim_dataset *dset ;
   NI_element *nel ;
   CLU_threshtable *ctab ;
   ATR_string *atr ;
   char *msg=NULL ; int ntab=0,nmask=0 ; static int ntabold=-1 ;

ENTRY("CLU_setup_alpha_tables") ;

   if( !IM3D_VALID(im3d) ) EXRETURN ;

   /* free anything we have now */

   CLU_free_table( im3d->vwid->func->clu_tabNN1 ) ;
    im3d->vwid->func->clu_tabNN1 = NULL ;
   CLU_free_table( im3d->vwid->func->clu_tabNN2 ) ;
    im3d->vwid->func->clu_tabNN2 = NULL ;
   CLU_free_table( im3d->vwid->func->clu_tabNN3 ) ;
    im3d->vwid->func->clu_tabNN3 = NULL ;
   if( im3d->vwid->func->clu_mask != NULL ){
     free(im3d->vwid->func->clu_mask) ; im3d->vwid->func->clu_mask = NULL ;
   }

   /* get info from the Overlay dataset header, if present */

   dset = im3d->fim_now ; if( !ISVALID_DSET(dset) ) EXRETURN ;

   /* NN1 cluster C(p,alpha) table */

   atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN1" ) ;
   if( atr != NULL ){
     nel = NI_read_element_fromstring(atr->ch) ;  /* attribute string => NIML */
     ctab = format_cluster_table(nel) ;           /* NIML => C(p,alpha) table */
     im3d->vwid->func->clu_tabNN1 = ctab ;
     msg = THD_zzprintf(msg," NN=1") ; ntab += 1 ;

     /*  search for ASCII mask string */

     atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_MASK" ) ;
     if( atr != NULL ){
       int nvox = mask_b64string_nvox(atr->ch) ;  /* length of mask */
       if( nvox == DSET_NVOX(dset) ){         /* must match dataset */
         im3d->vwid->func->clu_mask = mask_from_b64string(atr->ch,&nvox) ;
       }
     } else {   /* search for original mask dataset via its idcode */
       char *idc = NI_get_attribute(nel,"mask_dset_idcode") ;
       THD_3dim_dataset *mset = PLUTO_find_dset_idc(idc) ;
       if( mset != NULL && DSET_NVOX(mset) == DSET_NVOX(dset) ){
         im3d->vwid->func->clu_mask = THD_makemask(mset,0,1.0,0.0) ;
         DSET_unload(mset) ;
       }
     }
     NI_free_element(nel) ;  /* get rid of the C(p,alpha) NIML element */
     if( im3d->vwid->func->clu_mask != NULL )
       nmask = THD_countmask( DSET_NVOX(dset) , im3d->vwid->func->clu_mask ) ;
   }

   /* NN2 and NN3 C(p,alpha) tables */

   atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN2" ) ;
   if( atr != NULL ){
     nel = NI_read_element_fromstring(atr->ch) ;
     ctab = format_cluster_table(nel) ; NI_free_element(nel) ;
     im3d->vwid->func->clu_tabNN2 = ctab ;
     msg = THD_zzprintf(msg," NN=2") ; ntab += 2 ;
   }

   atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN3" ) ;
   if( atr != NULL ){
     nel = NI_read_element_fromstring(atr->ch) ;
     ctab = format_cluster_table(nel) ; NI_free_element(nel) ;
     im3d->vwid->func->clu_tabNN3 = ctab ;
     msg = THD_zzprintf(msg," NN=3") ; ntab += 4 ;
   }

   if( ntab != ntabold ){
     if( msg != NULL ){
       INFO_message("%s3dClustSim tables found:%s" ,
                    AFNI_controller_label(im3d) , msg ) ;
       if( ntab < 7 )
         ININFO_message(" [if you require a missing NN table, will use%.5s]",msg);
       free(msg) ;
       if( nmask > 0 )
         ININFO_message(" %d voxels in 3dClustSim mask",nmask) ;
     } else if( ntabold >= 0 ){
       INFO_message("%s3dClustSim tables found: none" ,
                    AFNI_controller_label(im3d) ) ;
     }
     ntabold = ntab ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* Interpolate function y(x) at x=xout, given y(xa)=ya and y(xb)=yb.
   Assume y(x) = A * x^b, so that log(y(x)) = log(A) + b*log(x).
   In our application, x=p and y=C(p,alpha) for a fixed alpha.
*//*--------------------------------------------------------------------------*/

#undef  loginterp
#define loginterp(xout,xa,xb,ya,yb)                                   \
   ( (xout)==(xa) ) ? (ya)                                            \
 : ( (xout)==(xb) ) ? (yb)                                            \
 : ( (ya) * powf( (xout)/(xa) , logf((yb)/(ya)) / logf((xb)/(xa)) ) )

/*----------------------------------------------------------------------------*/
/* Find the significance level of a cluster with csiz voxels, which
   was thresholded at pval.  Return values can be
    * x <= 0   ==> cluster alpha value is greater than |x|
    * x == 0   ==> input error (alpha is N/A)
    * x >  0   ==> cluster alpha value is smaller than x
*//*--------------------------------------------------------------------------*/

float find_cluster_alpha( int csiz, float pval, CLU_threshtable *ctab )
{
   int   ipthr , iathr ;
   int   npthr , nathr ;
   float *pthr , *athr , **cluthr , cval ;

   if( csiz <= 1 || ctab == NULL || pval > ctab->pthr[0] ) return 0.0f ;

   npthr = ctab->npthr ; nathr = ctab->nathr ;
    pthr = ctab-> pthr ;  athr = ctab-> athr ; cluthr = ctab->cluthr ;

   /* find ipthr such that pthr[ipthr-1] > pval > pthr[ipthr] */

   for( ipthr=1 ; ipthr < npthr && pval <= pthr[ipthr] ; ipthr++ ) ; /*nada*/
   if( ipthr == npthr ){
     ipthr = npthr-1 ; pval = pthr[ipthr] ;  /* pval was too small */
   }

   /* scan in athr direction to find a C(p,alpha) value */

   for( iathr=0 ; iathr < nathr ; iathr++ ){
     cval = loginterp( pval,   pthr[ipthr-1]       ,   pthr[ipthr]       ,
                             cluthr[ipthr-1][iathr], cluthr[ipthr][iathr] ) ;
     if( csiz < cval ) break ;
   }
   if( iathr == 0 )                        return (       -athr[0]       ) ;
   if( iathr < nathr || csiz < 2.0f*cval ) return (        athr[iathr-1] ) ;
                                           return ( 0.1f * athr[nathr-1] ) ;
}

/*----------------------------------------------------------------------------*/

int find_cluster_thresh( float aaa, float pval, CLU_threshtable *ctab )
{
   int ipthr , iathr , npthr , nathr ;
   float *pthr , *athr , **cluthr , cval ;

   if( aaa <= 0.0f || ctab == NULL || pval  > ctab->pthr[0] ) return 0 ;

   npthr = ctab->npthr ; nathr = ctab->nathr ;
    pthr = ctab-> pthr ;  athr = ctab-> athr ; cluthr = ctab->cluthr ;

   /* find iathr to match athr */

   for( iathr=0 ; iathr < nathr && athr[iathr] > aaa ; iathr++ ) /*nada*/
   if( iathr == nathr ) return 0 ;   /* not found? */

   /* find ipthr such that pthr[ipthr-1] > pval > pthr[ipthr] */

   for( ipthr=1 ; ipthr < npthr && pval <= pthr[ipthr] ; ipthr++ ) ; /*nada*/
   if( ipthr == npthr ){
     ipthr = npthr-1 ; pval = pthr[ipthr] ;  /* pval was too small */
   }
   if( ipthr == npthr ){
     ipthr = npthr-1 ; pval = pthr[ipthr] ;  /* pval was too small */
   }

   cval = loginterp( pval,   pthr[ipthr-1]       ,   pthr[ipthr]       ,
                           cluthr[ipthr-1][iathr], cluthr[ipthr][iathr] ) ;

   return (int)(cval+0.951) ;
}

/*----------------------------------------------------------------------------*/

char * get_alpha_string( int csiz , float pval , Three_D_View *im3d )
{
   float alpha ; static char astr[32] ; CLU_threshtable *ctab ;

   ctab = CLU_get_thresh_table(im3d) ;

   if( ctab ==  NULL )  return "N/Csim" ;  /* no ClustSim */
   if( pval < 0.0f )    return "N/stat" ;  /* not a statistic */
   if( csiz <= 1 )      return "N/Clus" ;  /* not a cluster?! */
   if( pval < 1.e-11f ) return "<<0.01" ;  /* == smallest allowed */

   alpha = find_cluster_alpha( csiz , pval , ctab ) ;
   if( alpha > 0.0f ){
     if( alpha >= 0.01f ) sprintf(astr,"<%5.2f",alpha) ;
     else                 strcpy (astr,"<<0.01") ;
   } else if( alpha < 0.0f ){
     sprintf(astr,">%5.2f",-alpha) ;
   } else {
     strcpy(astr,"N/pval") ;
   }
   return astr ;
}

/*----------------------------------------------------------------------------*/

CLU_threshtable * CLU_get_thresh_table( Three_D_View *im3d )
{
   CLU_threshtable *ctab ;

   if( !IM3D_VALID(im3d) ) return NULL ;
   switch( im3d->vwid->func->clu_nnlev ){
     default: ctab = im3d->vwid->func->clu_tabNN1 ; break ;
     case 2:  ctab = im3d->vwid->func->clu_tabNN2 ; break ;
     case 3:  ctab = im3d->vwid->func->clu_tabNN3 ; break ;
   }
   if( ctab == NULL ) ctab = im3d->vwid->func->clu_tabNN1 ;
   if( ctab == NULL ) ctab = im3d->vwid->func->clu_tabNN2 ;
   if( ctab == NULL ) ctab = im3d->vwid->func->clu_tabNN3 ;
   return ctab ;
}
