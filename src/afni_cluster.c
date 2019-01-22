#undef MAIN
#include "afni.h"

static void AFNI_cluster_widgkill( Three_D_View *im3d ) ;
static void AFNI_cluster_widgize( Three_D_View *im3d , int force ) ;
static MRI_IMARR * AFNI_cluster_timeseries( Three_D_View *im3d , int ncl , int ids ) ;
static void AFNI_clus_viewpoint_CB( int why, int np, void *vp, void *cd ) ;
static char * AFNI_cluster_write_coord_table(Three_D_View *im3d);
static void AFNI_linkrbrain_av_CB( MCW_arrowval *av , XtPointer cd );

/*---------------*/

static int use_old_3dclust = 0 ;         /* 29 Oct 2019 */
static char * AFNI_clus_3dclust( Three_D_View *im3d , char *extraopts ) ;

/*---------------*/

#undef  SET_INDEX_LAB
#define SET_INDEX_LAB(iq) \
  AFNI_clus_viewpoint_CB(RECEIVE_VIEWPOINT,0,NULL,(void *)(iq))

#undef  PEAK_MODE
#undef  CMASS_MODE
#define PEAK_MODE  0
#define CMASS_MODE 1

#undef  MAX_INDEX
#define MAX_INDEX 99999

#undef  WAY_BIG
#define WAY_BIG 1.e+10   /* a big number */

static int init_clr[MAX_CLU_AUXDSET] = { 5 , 14 , 8 , 15 } ;

#define myceil(x)   ceil((x)-0.00005)
#define myfloor(x) floor((x)+0.00005)

static Widget wtemp ;

static char *wherprog = NULL ;

static char *yesno[2] = { "No" , "Yes" } ;

char * get_alpha_string( int csiz  , float pval , Three_D_View *im3d    ) ;
int find_cluster_thresh( float athr, float pval , CLU_threshtable *ctab ) ;
CLU_threshtable * CLU_get_thresh_table( Three_D_View *im3d ) ;

#undef  SET_CLUSTERS_LAB
#define SET_CLUSTERS_LAB(cw,starred)                                          \
 do{ char clab[128] =                                                         \
       "###: __Size__  __X__  __Y__  __Z__                           Alpha" ; \
     if( starred ) strcat(clab,"*") ;                                         \
     MCW_set_widget_label( (cw)->clusters_lab , clab ) ;                      \
 } while(0)

/*---------------------------------------------------------------------*/

char * print_color_string(float r, float g, float b)
{
   static char str[32] ; int rr,gg,bb ;

   if( r < 0.0 ) r = 0.0f ; else if( r > 1.0f ) r = 1.0f ;
   if( g < 0.0 ) g = 0.0f ; else if( g > 1.0f ) g = 1.0f ;
   if( b < 0.0 ) b = 0.0f ; else if( b > 1.0f ) b = 1.0f ;

   sprintf( str , "\\color:%1x%1x%1x" ,
            (int)(r*15.9f) , (int)(g*15.9f) , (int)(b*15.9f) ) ;
   return str ;
}

/*****************************************************************************/
/*************  Functions for all actions in the cluster group ***************/

/*---------------------------------------------------------------------*/
/* Put a '*' next to the active item in the vedit list on the menu.    */

static char *clubutlab[] = { " Clear" ,          /* first blank saves */
                             " Clusterize"  } ;  /* space for a '*'  */

void set_vedit_cluster_label( Three_D_View *im3d , int ll )
{
   char lab[64] ;

ENTRY("set_vedit_cluster_label") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   strcpy(lab,clubutlab[0]); if( ll==0 ) lab[0] = '*' ;
   MCW_set_widget_label( im3d->vwid->func->clu_clear_pb , lab ) ;

   strcpy(lab,clubutlab[1]); if( ll==1 ) lab[0] = '*' ;
   MCW_set_widget_label( im3d->vwid->func->clu_cluster_pb , lab ) ;

   SENSITIZE( im3d->vwid->func->clu_report_pb , (ll==1) ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/
/* Callback from the usemask toggle button */

void AFNI_clus_usemask_CB( Widget w ,
                           XtPointer client_data , XtPointer call_data )
{
   Three_D_View *im3d = (Three_D_View *)client_data ;
   AFNI_clu_widgets *cwid ;
   int bval ;

ENTRY("AFNI_clus_usemask_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   bval = MCW_val_bbox( cwid->usemask_bbox ) ;
   if( bval == !im3d->vednomask ) EXRETURN ;

   im3d->vednomask = !bval ;
   SET_CLUSTERS_LAB(cwid,im3d->vednomask) ;

   if( im3d->vinfo->func_visible ){
     im3d->vedset.flags = 1 ;
     AFNI_redisplay_func( im3d ) ;
     im3d->vedset.flags = 0 ;
   }
   EXRETURN ;
}

/*--------------------------------------------------------------------*/
/* 14 Jun 2014 */

void AFNI_clus_relabel_save_buttons( AFNI_clu_widgets *cwid )
{
   int ii ; char *lab , *hint ;

ENTRY("AFNI_clus_relabel_save_buttons") ;

   if( cwid == NULL ) EXRETURN ;

   lab  = (cwid->save_as_mask == 0 ) ? "Save" : "Mask" ;
   hint = (cwid->save_as_mask == 0 )
           ? "Save average Aux dataset timeseries to 1D file"
           : "Save mask dataset for this single cluster"      ;

   for( ii=0 ; ii < cwid->nall ; ii++ ){
     MCW_set_widget_label( cwid->clu_save_pb[ii] , lab ) ;
     MCW_register_hint( cwid->clu_save_pb[ii] , hint ) ;
   }

   EXRETURN ;
}

/*--------------------------------------------------------------------*/
/* Callback from the save_as_mask toggle button [14 Jun 2014] */

void AFNI_clus_save_as_mask_CB( Widget w ,
                               XtPointer client_data , XtPointer call_data )
{
   Three_D_View *im3d = (Three_D_View *)client_data ;
   AFNI_clu_widgets *cwid ;
   int bval ;

ENTRY("AFNI_clus_save_as_mask_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   bval = MCW_val_bbox( cwid->save_as_mask_bbox ) ;
   if( bval == cwid->save_as_mask ) EXRETURN ;

   cwid->save_as_mask = bval ;
   AFNI_clus_relabel_save_buttons( cwid ) ;

   EXRETURN ;
}

/*---------------------------------------------------------------*/
/* Callback from the clusterize parameter chooser.
   [30 Jan 2015 - changed from 'vector' to 'stuff' chooser]
*//*-------------------------------------------------------------*/

static void AFNI_cluster_choose_CB( Widget wc, XtPointer cd, int nval, void **val )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   int rmm,vmul,bsid ; char *cpt ;

ENTRY("AFNI_cluster_choose_CB") ;

   if( ! IM3D_OPEN(im3d) || nval != 3 || val == NULL ) EXRETURN ; /* bad bad */

   rmm  = - (int)(intptr_t)val[0] ;                   /* NN */
   vmul =   (int)(intptr_t)val[1] ;                   /* Voxels */
   bsid = strcmp( (char *)val[2] , yesno[1] ) == 0 ;  /* Bi-sided? */

   im3d->vedset.code     = VEDIT_CLUST ;
   im3d->vedset.param[2] = rmm ;   /* is -1, -2, or -3 [Jul 2010] */
   im3d->vedset.param[3] = vmul ;  /* other params set in afni.c */
   im3d->vedset.param[6] = bsid ;
   set_vedit_cluster_label(im3d,1) ;
   if( ISVALID_3DIM_DATASET(im3d->fim_now) && !im3d->vinfo->func_visible ){
     MCW_set_bbox( im3d->vwid->view->see_func_bbox , 1 ) ;
     im3d->vinfo->func_visible = True ;
   }

   im3d->vwid->func->clu_nnlev = (int)(-rmm) ;

   im3d->vinfo->clusterize_nnlev  = -rmm ;  /* 02 Nov 2018 */
   im3d->vinfo->clusterize_size   = vmul ;  /* save for re-use */
   im3d->vinfo->clusterize_bisid  = bsid ;

   IM3D_CLEAR_TMASK(im3d) ;      /* Mar 2013 */
   IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
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
     IM3D_CLEAR_TMASK(im3d) ;      /* Mar 2013 */
     IM3D_CLEAR_THRSTAT(im3d) ; /* 12 Jun 2014 */
     if( im3d->vinfo->func_visible ) AFNI_redisplay_func( im3d ) ;
     EXRETURN ;
   }

   /*--- Get clusterizing parameters ---*/

   if( w == im3d->vwid->func->clu_cluster_pb ){
     char *lvec[2] = { "NN level " , "Voxels   " } ;
     int nnlev=im3d->vinfo->clusterize_nnlev ;
     int ccsiz=im3d->vinfo->clusterize_size ;
     int bisid=im3d->vinfo->clusterize_bisid ;
     MCW_arrowval **vav ; int nav ;

#if 0
     if( im3d->giset != NULL && im3d->giset->ready ){  /* 20 Oct 2010 */
       (void)MCW_popup_message( w ,
                                 " You can't use Clusterize \n"
                                 " while Group InstaCorr is \n"
                                 " attached to this AFNI\n"
                                 " controller." ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
       BEEPIT ; EXRETURN ;
     }
#endif

     if( im3d->vedset.code == VEDIT_CLUST ){
       nnlev = -im3d->vedset.param[2];
       ccsiz =  im3d->vedset.param[3]; if( ccsiz < 2 ) ccsiz = 40 ;
     }
     MCW_choose_stuff( im3d->vwid->func->thr_label ,
                        "------ Set Clusterize Parameters ------\n"
                        "* NN level => NN clustering method\n"
                        "       1 : faces must touch\n"
                        "       2 : faces or edges touch\n"
                        "       3 : faces or edges or corners\n"
                        "* Voxels   => minimum cluster size that\n"
                        "              will be kept [at least 2]\n"
                        "* Bi-sided => positively and negatively\n"
                        "              thresholded voxels get\n"
                        "              clustered separately\n"
                        "              (eg, thresh=t-statistic)\n"
                        "---------------------------------------\n"
                        "* Click on the 'Rpt' button to open\n"
                        "  a complete cluster report panel.\n"     ,

                      AFNI_cluster_choose_CB , (XtPointer)im3d ,
                        MSTUF_INT     , "NN level " , 1 ,     3 , nnlev ,
                        MSTUF_INT     , "Voxels   " , 2 , 99999 , ccsiz ,
                        MSTUF_STRLIST , "Bi-sided?" , 2 , bisid , yesno ,
                      MSTUF_END ) ;
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
ENTRY("AFNI_cluster_dispkill") ;
   AFNI_cluster_widgkill(im3d) ; EXRETURN ;
}

/*---------------------------------------------------------------------------*/

void AFNI_cluster_dispize( Three_D_View *im3d , int force )
{
   MCW_cluster_array *clar ; int ii ;
   AFNI_clu_widgets *cwid ;

ENTRY("AFNI_cluster_dispize") ;

   AFNI_cluster_widgize( im3d , force ) ;

   /* 15 Oct 2015: turn all See/Hide boxes to See */

   cwid = im3d->vwid->func->cwid     ; if( cwid == NULL ) EXRETURN ;
   clar = im3d->vwid->func->clu_list ; if( clar == NULL ) EXRETURN ;

   STATUS("setting clu_see_bbox on") ;
   for( ii=0 ; ii < cwid->nall ; ii++ )
     MCW_set_bbox( cwid->clu_see_bbox[ii] , 1 ) ;

   EXRETURN ;
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

/*---------------------------------------------------------------------------*/

#define SHRUTI_NUM 999

static int maxclu_default = -1 ;
static int scrolling      =  1 ;

#undef  CLUST_SEE
#define CLUST_SEE(iq,qq)                                           \
 (  (IM3D_OPEN(iq)                    &&                           \
     (qq) >= 0                        &&                           \
     (qq) < (iq)->vwid->func->clu_num &&                           \
     (iq)->vwid->func->cwid != NULL     )                          \
   ? MCW_val_bbox((iq)->vwid->func->cwid->clu_see_bbox[qq]) : 0 )

/*! Make the widgets for one row of the cluster display/control panel.
    The row itself will not be managed at this time; that comes later. */

#undef  MAKE_CLUS_ROW
#define MAKE_CLUS_ROW(ii)                                           \
 do{ Widget rc,lb,mb,p1,p2,p3,p4,p5,sb ; char *slab ;               \
     char *ff = (ii%2==0) ? "dialog" : "menu" ;                     \
     rc = cwid->clu_rc[ii] =                                        \
         XtVaCreateWidget(                                          \
           ff     , xmRowColumnWidgetClass , cwid->rowcol ,         \
             XmNpacking     , XmPACK_TIGHT ,                        \
             XmNorientation , XmHORIZONTAL ,                        \
             XmNadjustMargin , True ,                               \
             XmNmarginHeight , 1 , XmNmarginWidth , 0 ,             \
             XmNspacing , 1 ,                                       \
             XmNtraversalOn , True ,                                \
           NULL ) ;                                                 \
     slab = (char *)malloc(sizeof(char)*8) ;                        \
     sprintf(slab,(ii<9)?"%1d:":"%2d",ii+1) ;                       \
     cwid->clu_see_bbox[ii] = new_MCW_bbox( rc , 1 , &slab ,        \
            MCW_BB_check , MCW_BB_noframe ,                         \
            AFNI_clus_action_CB , im3d ) ; free(slab) ;             \
     sb = cwid->clu_see_bbox[ii]->wbut[0] ;                         \
     XmToggleButtonSetState( cwid->clu_see_bbox[ii]->wbut[0] ,      \
            True , False ) ;                                        \
     lb = cwid->clu_lab[ii] = XtVaCreateManagedWidget(              \
            ff     , xmLabelWidgetClass , rc ,                      \
            LABEL_ARG("12345678 +xxx.x +xxx.x +xxx.x") ,            \
            XmNalignment , XmALIGNMENT_BEGINNING ,                  \
            XmNrecomputeSize , False ,  XmNtraversalOn , True ,     \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     p1 = cwid->clu_jump_pb[ii] = XtVaCreateManagedWidget(          \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            XmNmarginWidth , 0 ,                                    \
            LABEL_ARG("Jump") , XmNtraversalOn , True ,             \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     p2 = cwid->clu_flsh_pb[ii] = XtVaCreateManagedWidget(          \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            XmNmarginWidth , 0 ,                                    \
            LABEL_ARG("Flash") , XmNtraversalOn , True ,            \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     p3 = cwid->clu_plot_pb[ii] = XtVaCreateManagedWidget(          \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            XmNmarginWidth , 0 ,                                    \
            LABEL_ARG("Plot") , XmNtraversalOn , True ,             \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     p4 = cwid->clu_save_pb[ii] = XtVaCreateManagedWidget(          \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            XmNmarginWidth , 0 ,                                    \
            LABEL_ARG("Save") , XmNtraversalOn , True ,             \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     p5 = cwid->clu_writ_pb[ii] = XtVaCreateManagedWidget(          \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            XmNmarginWidth , 0 ,                                    \
            LABEL_ARG("Write") , XmNtraversalOn , True ,            \
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
     XtAddCallback( cwid->clu_writ_pb[ii],                          \
                    XmNactivateCallback,AFNI_clus_action_CB,im3d ); \
     if( scrolling ){                                               \
       char *bgclr = (ii%2==1) ? "black" : "gray14" ;               \
       char *fgclr = (ii%2==1) ? "white" : "yellow" ;               \
       MCW_set_widget_bg(rc,bgclr,0);                               \
       MCW_set_widget_bg(lb,bgclr,0); MCW_set_widget_fg(lb,fgclr);  \
       MCW_set_widget_bg(mb,bgclr,0); MCW_set_widget_fg(mb,fgclr);  \
                                      MCW_set_widget_fg(p1,fgclr);  \
                                      MCW_set_widget_fg(p2,fgclr);  \
                                      MCW_set_widget_fg(p3,fgclr);  \
                                      MCW_set_widget_fg(p4,fgclr);  \
                                      MCW_set_widget_fg(p5,fgclr);  \
                                      MCW_set_widget_fg(sb,fgclr);  \
     }                                                              \
  } while(0)

/*---------------------------------------------------------------------------*/
/* Set the labels for the auxiliary dataset (etc) */

static void AFNI_clus_auxdsetlabel( Three_D_View *im3d )
{
   AFNI_clu_widgets *cwid ;
   char str[THD_MAX_NAME+66] ;
   int ww , naux ;

ENTRY("AFNI_clus_auxdsetlabel") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   for( naux=ww=0 ; ww < MAX_CLU_AUXDSET ; ww++ ){
     if( ISVALID_DSET(cwid->auxdset[ww]) ){
       naux++ ;
       sprintf(str,"%.17s",DSET_FILECODE(cwid->auxdset[ww])) ;
       MCW_set_widget_label( cwid->auxdset_namlab[ww] , str ) ;
     } else {
       XmToggleButtonSetState( cwid->auxdset_bbox[ww]->wbut[0] , False , False ) ;
       MCW_set_widget_label( cwid->auxdset_namlab[ww] , "clear") ;
     }
     sprintf(str,"Auxiliary Dataset = %s" ,
             ISVALID_DSET(cwid->auxdset[ww]) ? DSET_FILECODE(cwid->auxdset[ww])
                                             : "clear" ) ;
     MCW_register_hint( cwid->auxdset_pb[ww] , str ) ;
   }

   sprintf(str,"%1d Aux Dset%s|Scat.1D=%.17s" ,
               naux , (naux==1) ? "\0" : "s" ,
               (cwid->splotim!=NULL) ? THD_trailname(cwid->splotim->name,0) : "clear" ) ;

   MCW_set_widget_label( cwid->auxdset_lab , str ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Stuff to set linkRbrain max cluster count from right-click popup
   menu on the 'linkRbrain' button [09 Sep 2015]
*//*-------------------------------------------------------------------------*/

void AFNI_clus_linknum_CB( Widget w , XtPointer cd , MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;

ENTRY("AFNI_clus_linknum_CB") ;
   if( !IM3D_OPEN(im3d) || cbs == NULL ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;
   cwid->linkrbrain_nclu = cbs->ival ; EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Right-click handler for linkRbrain button - asks for number [09 Sep 2015] */

void AFNI_clus_linknum_EV( Widget w , XtPointer client_data ,
                           XEvent *ev , Boolean *continue_to_dispatch )
{
   Three_D_View *im3d = (Three_D_View *)client_data ;
   AFNI_clu_widgets *cwid ;

ENTRY("AFNI_clus_linknum_EV") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   switch( ev->type ){
      case ButtonPress:{
         XButtonEvent *event = (XButtonEvent *)ev ;
         if( event->button == Button3 ){
            MCW_choose_integer( w , "Max linkRbrain clusters" ,
                                0 , 99 , cwid->linkrbrain_nclu ,
                                AFNI_clus_linknum_CB , client_data ) ;
         } else if( event->button == Button2 ){
            XBell(XtDisplay(w),100) ;
            MCW_popup_message( w, " \n U R Bad! \n ", MCW_USER_KILL|MCW_TIMER_KILL );
         }
      }
      break ;
   }

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Make the cluster report widgets initially */

static void AFNI_clus_make_widgets( Three_D_View *im3d, int num )
{
   AFNI_clu_widgets *cwid ;
   Widget rc ;
   XmString xstr ;
   char str[32] , *slab ;
   int ii ;
   Widget shtop , swtop=NULL ;

ENTRY("AFNI_clus_make_widgets") ;

   if( !IM3D_OPEN(im3d) || im3d->vwid->func->cwid != NULL ) EXRETURN ; /* bad */

   im3d->vwid->func->cwid = cwid = myXtNew( AFNI_clu_widgets ) ;
   CLU_CLEAR_AUXDSET(cwid) ;
   cwid->coord_mode = 0 ;

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
#if 0
   if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(cwid->top_menu) ;
#endif

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

   { char *blab = "Save->Mask??" ;   /* 14 Jun 2014 */
     cwid->save_as_mask_bbox = new_MCW_bbox( cwid->top_menu , 1,&blab ,
                                             MCW_BB_check , MCW_BB_noframe ,
                                             AFNI_clus_save_as_mask_CB , (XtPointer)im3d ) ;
     MCW_reghint_children( cwid->save_as_mask_bbox->wrowcol , "Change Save buttons to Mask buttons?" ) ;
     cwid->save_as_mask = 0 ;
   }

   { char *blab = "SqrtHist??" ;
     cwid->histsqrt_bbox = new_MCW_bbox( cwid->top_menu , 1,&blab ,
                                         MCW_BB_check , MCW_BB_noframe , NULL,NULL ) ;
     MCW_reghint_children( cwid->histsqrt_bbox->wrowcol , "Plot square root of histogram?" ) ;
   }

   { char *blab = "Spearman??" ; int sval ;
     cwid->spearman_bbox = new_MCW_bbox( cwid->top_menu , 1,&blab ,
                                         MCW_BB_check , MCW_BB_noframe , NULL,NULL ) ;
     MCW_reghint_children( cwid->spearman_bbox->wrowcol , "Scatterplot uses Spearman?") ;
     sval = AFNI_yesenv("AFNI_CLUSTER_SPEARMAN") ;
     MCW_set_bbox( cwid->spearman_bbox , sval ) ;
   }

   { char *blab = "Despike??" ; int sval ;
     cwid->despike_bbox = new_MCW_bbox( cwid->top_menu , 1,&blab ,
                                        MCW_BB_check , MCW_BB_noframe , NULL,NULL ) ;
     MCW_reghint_children( cwid->despike_bbox->wrowcol , "Despike before Mean plot?") ;
     sval = AFNI_yesenv("AFNI_CLUSTER_DESPIKE") ;
     MCW_set_bbox( cwid->despike_bbox , sval ) ;
   }

   { char *blab = "Detrend??" ; int sval ;
     cwid->detrend_bbox = new_MCW_bbox( cwid->top_menu , 1,&blab ,
                                        MCW_BB_check , MCW_BB_noframe , NULL,NULL ) ;
     MCW_reghint_children( cwid->detrend_bbox->wrowcol , "Detrend before Mean plot?") ;
     sval = AFNI_yesenv("AFNI_CLUSTER_DETREND") ;
     MCW_set_bbox( cwid->detrend_bbox , sval ) ;
   }

   { char *blab = "Error Bars??" ; int sval ;
     cwid->ebar_bbox = new_MCW_bbox( cwid->top_menu , 1,&blab ,
                                     MCW_BB_check , MCW_BB_noframe , NULL,NULL ) ;
     MCW_reghint_children( cwid->ebar_bbox->wrowcol , "Detrend before Mean plot?") ;
   }

   /*---- end of popup menu ----*/

#undef  VLINE
#undef  HLINE
#define VLINE(rr)                                                           \
     (void) XtVaCreateManagedWidget( "menu", xmSeparatorWidgetClass, (rr) , \
                                        XmNorientation   , XmVERTICAL    ,  \
                                        XmNseparatorType , XmDOUBLE_LINE ,  \
                                     NULL )
#define HLINE(rr)                                                           \
   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass, (rr) , \
                                      XmNseparatorType   , XmSINGLE_LINE ,  \
                                   NULL )

   /* Separator from other widgets */

   HLINE(cwid->rowcol) ;

   /* horiz rowcol for row #0 controls */

   rc = XtVaCreateWidget(
          "menu" , xmRowColumnWidgetClass , cwid->rowcol ,
             XmNpacking      , XmPACK_TIGHT ,
             XmNorientation  , XmHORIZONTAL ,
             XmNadjustMargin , True ,
             XmNtraversalOn  , True ,
          NULL ) ;

   /* Toggle button for using the clu_mask */

   { char *mask_label[1] = { "Use internal mask from 3dClustSim" } ;
     cwid->usemask_bbox = new_MCW_bbox( rc ,
                                        1 , mask_label ,
                                        MCW_BB_check , MCW_BB_noframe ,
                                        AFNI_clus_usemask_CB, (XtPointer)im3d );
     MCW_reghint_children( cwid->usemask_bbox->wrowcol ,
                           "Enable or disable use of internally stored mask" ) ;
     MCW_set_bbox( cwid->usemask_bbox , !im3d->vednomask ) ;
     SENSITIZE(cwid->usemask_bbox->wrowcol,(im3d->vwid->func->clu_mask!=NULL)) ;
   }

   VLINE(rc) ;
   wherprog = THD_find_executable("whereami") ;
   if( show_linkrbrain_link() && wherprog != NULL ){
     int  showlinkr; static char *lbhelp=NULL ;
     xstr = XmStringCreateLtoR( "linkRbrain" , XmFONTLIST_DEFAULT_TAG ) ;
     cwid->linkrbrain_pb = XtVaCreateManagedWidget(
             "menu" , xmPushButtonWidgetClass , rc ,
              XmNlabelString , xstr ,
              XmNtraversalOn , True  ,
           NULL ) ;
     XmStringFree(xstr) ;
     XtAddCallback( cwid->linkrbrain_pb, XmNactivateCallback, AFNI_clus_action_CB, im3d );
     MCW_register_hint( cwid->linkrbrain_pb ,
                         "Write cluster table, then run 'whereami -linkrbrain'") ;
     if( lbhelp == NULL ){
       lbhelp = (char *)malloc(sizeof(char)*2048) ;
       sprintf(lbhelp,
                "Query %s website for\n"
                "correlations of the set of cluster\n"
                "coordinates with known tasks or genes.\n"
                " * Right click to choose how many  *\n"
                " * cluster coordinates to transmit *" ,
              get_linkrbrain_site() ) ;
     }
     MCW_register_help( cwid->linkrbrain_pb , lbhelp ) ;
     showlinkr = ((im3d->vinfo->view_type == VIEW_TALAIRACH_TYPE) && show_linkrbrain_link());
     SENSITIZE(cwid->linkrbrain_pb, (showlinkr) ) ;

     XtInsertEventHandler( cwid->linkrbrain_pb,  /* 09 Sep 2015: popup menu */
                           ButtonPressMask ,     /* button presses */
                           FALSE ,               /* nonmaskable events? */
                           AFNI_clus_linknum_EV, /* handler */
                           (XtPointer)im3d ,     /* client data */
                           XtListTail            /* last in queue */
                          ) ;

     { static char *clab[2] = { "Tasks" , "Genes" } ;
       static char *lbhelp=NULL ;
       cwid->linkrbrain_av = new_MCW_optmenu( rc , "type" , 0,1,0,0 ,
                          AFNI_linkrbrain_av_CB,im3d , MCW_av_substring_CB,clab ) ;
       MCW_reghint_children( cwid->linkrbrain_av->wrowcol ,
                              "Correlate coordinates with tasks or genes" ) ;
       if( lbhelp == NULL ){
         lbhelp = (char *)malloc(sizeof(char)*1024) ;
         sprintf(lbhelp, "Choose whether to show the correlation or\n"
                         "of cluster coordinates with either tasks or\n"
                         "genes from the %s database.\n" , get_linkrbrain_site() ) ;
       }
       MCW_reghelp_children( cwid->linkrbrain_av->wrowcol , lbhelp ) ;
       AV_SENSITIZE(cwid->linkrbrain_av, (showlinkr));
     }
   } else {
/* WARNING_message("No whereami program in Unix path ==> no linkrbrain button in Clusterize!") ;*/
     cwid->linkrbrain_pb = cwid->savemask_pb ;
   }
   cwid->linkrbrain_nclu = 0 ; /* 09 Sep 2015 */

   XtManageChild(rc) ;  /* finished with row #0 setup */

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

   /* row #1: arrow for Aux Dset stuff */

   cwid->auxdset_arrow = XtVaCreateManagedWidget(
                            "arrow" , xmArrowButtonWidgetClass , rc ,
                            XmNarrowDirection , XmARROW_RIGHT ,
                            XmNheight , 15 , XmNwidth , 24 ,
                            XmNborderWidth , 0 ,
                            XmNinitialResourcesPersistent , False ,
                            XmNtraversalOn , True ,
                          NULL ) ;
   XtAddCallback( cwid->auxdset_arrow, XmNactivateCallback, AFNI_clus_action_CB, im3d ) ;
   MCW_register_hint( cwid->auxdset_arrow , "Open/Close Auxiliary Dataset selectors" ) ;
   MCW_set_widget_fg( cwid->auxdset_arrow , "white" ) ;
   VLINE(rc) ;

   /* row #1: index label */

   cwid->index_lab = XtVaCreateManagedWidget( "menu" , xmLabelWidgetClass , rc , NULL ) ;
   LABELIZE(cwid->index_lab) ;
   MCW_register_hint( cwid->index_lab , "Crosshairs are in this cluster" ) ;
   MCW_register_help( cwid->index_lab , "Shows the cluster index that\n"
                                        "contains the crosshairs point.\n"
                                        "* '--' means that the crosshairs\n"
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
   MCW_register_hint( cwid->clust3d_pb , "Output equivalent clustering command") ;
   MCW_register_help( cwid->clust3d_pb ,
                      "Writes the equivalent clustering\n"
                      "command line to the terminal (stdout).\n"
                      "\n"
                      "If you hold down the keyboard Shift\n"
                      "key while you click this button, then\n"
                      "AFNI will execute this command\n"
                      "as well as write it to the screen." ) ;

   if( AFNI_yesenv("AFNI_CLUSTERIZE_OLD") ) use_old_3dclust = 1 ;

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
                         "              whereami -omask\n"
                         "to get a report of atlas locations\n"
                         "that overlap each cluster.\n"
                         "* At most the first 20 clusters\n"
                         "   will be passed to whereami.\n"
                         "* To change this upper limit, set\n"
                         "   AFNI_CLUSTER_WAMIMAX to a\n"
                         "   value between 1 and 99.\n"
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

   /*----------- Holds all the auxdset widgets, to be turned on/off ----------*/

   cwid->auxdset_master_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , cwid->rowcol ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNspacing      , 0 ,
            XmNadjustMargin , True ,
            XmNtraversalOn  , True ,
         NULL ) ;

   /*--- [19 Oct 2015] Build a row for each possible Aux Dset ---*/

   slab = (char *)malloc(sizeof(char)*8) ;
   for( ii=0 ; ii < MAX_CLU_AUXDSET ; ii++ ){

     /* horiz rowcol to hold all widgets in this row */

     rc = XtVaCreateWidget(
            "menu" , xmRowColumnWidgetClass , cwid->auxdset_master_rowcol ,
               XmNpacking      , XmPACK_TIGHT ,
               XmNorientation  , XmHORIZONTAL   ,
               XmNadjustMargin , True ,
               XmNtraversalOn , True  ,
            NULL ) ;

     /* toggle button for aux dset */

     sprintf(slab,"%1d",ii+1) ;
     cwid->auxdset_bbox[ii] = new_MCW_bbox( rc , 1 , &slab ,
                                            MCW_BB_check, MCW_BB_noframe,
                                            NULL,NULL ) ;
     XmToggleButtonSetState( cwid->auxdset_bbox[ii]->wbut[0] , False , False ) ;
     MCW_reghint_children( cwid->auxdset_bbox[ii]->wrowcol ,
                           "See or Hide data from this Auxiliary Dataset" ) ;

     /* dataset chooser */

     xstr = XmStringCreateLtoR( "Aux" , XmFONTLIST_DEFAULT_TAG ) ;
     cwid->auxdset_pb[ii] = XtVaCreateManagedWidget(
                               "menu" , xmPushButtonWidgetClass , rc ,
                               XmNmarginHeight , 1 , XmNmarginWidth , 0 ,
                               XmNlabelString , xstr ,
                               XmNtraversalOn , True  ,
                            NULL ) ;
     XmStringFree(xstr) ;
     XtAddCallback( cwid->auxdset_pb[ii], XmNactivateCallback, AFNI_clus_action_CB, im3d );
     MCW_register_hint( cwid->auxdset_pb[ii] , "Auxiliary Dataset = NULL" ) ;
     MCW_register_help( cwid->auxdset_pb[ii] ,
                       "If you choose an Auxiliary Dataset\n"
                       "  -- which must have the same xyz grid --\n"
                       "  -- dimensions as the Overlay dataset --\n"
                       "then for each cluster, you can extract\n"
                       "the corresponding data from that dataset\n"
                       "(sub-bricks 'From' until 'To') and do various\n"
                       "things with these numbers:\n"
                       "** Average them at each time-point:      'Mean'\n"
                       "** Pointwise median:                     'Median'\n"
                       "** Compute first principal component:    'PC#1'\n"
                       "** Histogram all the values:             'Histog'\n"
                       "** Scatterplot MEAN from each sub-brick: 'S:mean'\n"
                       "** Scatterplot ALL values:               'S:all'\n"
                       "   [-- Scat.1D button chooses x-axis for 'S:' --]\n"
                       "And then either 'Plot' or 'Save' these results\n"
                       "** If you 'Save' these results, the text field\n"
                       "   (between 'SaveTabl and 'SaveMsk') is used to\n"
                       "   define the output filename.\n"
                       "** If this text field is just the string '-',\n"
                       "   then 'Save' writes to the terminal window\n"
                       "   (stdout), instead of a file.\n"
                    ) ;

     /* 'From' and 'To' choosers */

     cwid->auxdset_from_av[ii] = new_MCW_arrowval( rc , "From" , MCW_AV_downup ,
                                                   0,MAX_INDEX,0,MCW_AV_editext,0 ,
                                                   NULL,NULL , NULL,NULL ) ;
     MCW_reghint_children( cwid->auxdset_from_av[ii]->wrowcol ,
                           "first sub-brick to use from Aux Dataset" ) ;
     XtVaSetValues( cwid->auxdset_from_av[ii]->wtext , XmNcolumns , 5 , NULL ) ;

     cwid->auxdset_to_av[ii] = new_MCW_arrowval( rc , "To" , MCW_AV_downup ,
                                                0,MAX_INDEX,MAX_INDEX,MCW_AV_editext,0 ,
                                                NULL,NULL , NULL,NULL ) ;
     MCW_reghint_children( cwid->auxdset_to_av[ii]->wrowcol ,
                           "last sub-brick to use from Aux Dataset" ) ;
     XtVaSetValues( cwid->auxdset_to_av[ii]->wtext , XmNcolumns , 5 , NULL ) ;

     cwid->auxdset_clr_av[ii] = new_MCW_colormenu(
                                    rc , " " , im3d->dc ,
                                    1 , im3d->dc->ovc->ncol_ov-1 , init_clr[ii] ,
                                    NULL , NULL ) ;

     cwid->auxdset_namlab[ii] = XtVaCreateManagedWidget(
                                    "dialog" , xmLabelWidgetClass , rc , NULL ) ;
     MCW_set_widget_label( cwid->auxdset_namlab[ii] , "NULL") ;
     MCW_set_widget_fg( cwid->auxdset_namlab[ii] , "white" ) ;
     LABELIZE( cwid->auxdset_namlab[ii] ) ;

     XtManageChild(rc) ;  /* row #ii is finished */
   }
   free(slab) ;

   /*--- [19 Oct 2015] Next row = things that control the Aux Dset plotting ---*/

   rc = XtVaCreateWidget(
          "menu" , xmRowColumnWidgetClass , cwid->auxdset_master_rowcol ,
             XmNpacking      , XmPACK_TIGHT ,
             XmNorientation  , XmHORIZONTAL   ,
             XmNadjustMargin , True ,
             XmNtraversalOn , True  ,
          NULL ) ;

   /* data processing method */

   { static char *clab[6] = { "Mean", "Median", "PC#1", "Histog", "S:mean", "S:all" } ;
     cwid->aver_av = new_MCW_optmenu( rc , "Plot" , 0,5,0,0 ,
                                      NULL,NULL , MCW_av_substring_CB,clab ) ;
     MCW_reghint_children( cwid->aver_av->wrowcol ,
                           "Set data processing method for Plot/Save" ) ;
     MCW_reghelp_children( cwid->aver_av->wrowcol ,
                           "Defines how data extracted from the\n"
                           "Auxiliary Dataset will be processed:\n"
                           "* Mean   = averaged across voxels\n"
                           "* Median = median across voxels\n"
                           "* PC#1   = compute first principal\n"
                           "            component vector\n"
                           "* Histog = build a histogram across\n"
                           "            voxels and sub-bricks\n"
                           "* A hidden Button-3 popup menu on the\n"
                           "   cluster summary report label atop this\n"
                           "   window will let you choose the range\n"
                           "   of data to be histogram-ized.\n"
                           "* The options 'S:all' and 'S:mean' allow\n"
                           "   you to scatter plot all the data values\n"
                           "   from the cluster -- or just the mean from\n"
                           "   each sub-brick -- vs. an external 1D file,\n"
                           "   which is chosen with the 'Scat.1D' button."
                         ) ;
   }

   VLINE(rc) ;

   /* Scatter plot (splot) choosers */

   xstr = XmStringCreateLtoR( "Scat.1D" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->splot_pb = XtVaCreateManagedWidget(
           "menu" , xmPushButtonWidgetClass , rc ,
            XmNmarginHeight , 1 , XmNmarginWidth , 0 ,
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
                      "   vs. some subject-level covariate.\n"
                      "* If you don't choose a Scat.1D file,\n"
                      "   (or 'Clear' it later), then the\n"
                      "   sub-brick index is used to define\n"
                      "   the x-axis values.\n"
                      "* For the 'S:mean' option, the program\n"
                      "   computes the correlation coefficient\n"
                      "   between the x-axis and y-axis values\n"
                      "   (R) and displays that on top of the\n"
                      "   scatterplot.  It also shows a\n"
                      "   95%% confidence interval for R,\n"
                      "   computed via a bias-corrected\n"
                      "   (BC) bootstrap method."
                    ) ;

   xstr = XmStringCreateLtoR( "Clear" , XmFONTLIST_DEFAULT_TAG ) ;
   cwid->splot_clear_pb = XtVaCreateManagedWidget(
           "menu" , xmPushButtonWidgetClass , rc ,
            XmNmarginHeight , 1 , XmNmarginWidth , 0 ,
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

   VLINE(rc) ;

   /* Time series dataset label */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass,cwid->rowcol,
                                      XmNseparatorType   , XmSHADOW_ETCHED_IN ,
                                   NULL ) ;
   cwid->auxdset_lab = XtVaCreateManagedWidget(
                        "dialog" , xmLabelWidgetClass , rc , NULL ) ;
   AFNI_clus_auxdsetlabel(im3d) ;
   MCW_set_widget_fg( cwid->auxdset_lab , "white" ) ;
   LABELIZE( cwid->auxdset_lab ) ;

   XtManageChild(rc) ;  /* row is finished */

   /*----------- end of auxdset widgets in the master rowcol ----------*/

   /* Separator from other widgets */

   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass,cwid->rowcol,
                                      XmNseparatorType   , XmSINGLE_LINE ,
                                   NULL ) ;

   /* Jul 2010: header line */

   cwid->clusters_lab =
     XtVaCreateManagedWidget( "menu" , xmLabelWidgetClass , cwid->rowcol , NULL ) ;

   SET_CLUSTERS_LAB(cwid,im3d->vednomask) ;
   MCW_set_widget_fg( cwid->clusters_lab , "white") ;

   /*------- Now create rows of widgets to display results from clusters -------*/

   if( num < 2 ) num = 2 ;
   cwid->nall = num ;
   cwid->nrow = 0 ;     /* none are managed at this time */

   cwid->clu_rc      = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_lab     = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_jump_pb = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_plot_pb = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_save_pb = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_flsh_pb = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_writ_pb = (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_alph_lab= (Widget *)XtCalloc( num , sizeof(Widget) ) ;
   cwid->clu_see_bbox= (MCW_bbox**)XtCalloc(num , sizeof(MCW_bbox*));

   for( ii=0 ; ii < num ; ii++ ){ MAKE_CLUS_ROW(ii) ; }
   for( ii=0 ; ii < num ; ii++ ){
     MCW_reghint_children( cwid->clu_see_bbox[ii]->wrowcol ,
                           "See or Hide this cluster" ) ;
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
     MCW_register_hint( cwid->clu_writ_pb[ii] ,
                        "Write just THIS cluster to a dataset" ) ;
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
                        "blame anyone but RW Cox, who is COMPLETELY innocent.\n"
                        "\n"
                        "==>> If a '*' appears after the Alpha label, this means\n"
                        "     since the 3dClustSim mask has been turned off, the\n"
                        "     Alpha values are not precise, since the Alpha table\n"
                        "     from 3dClustSim was computed with using that mask."
                      ) ;
   }

   XtManageChild( cwid->rowcol ) ;

   /*------- finalize the report panel and make it visible ------*/

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
   if( show_linkrbrain_link() && wherprog != NULL )  /* 09 Sep 2015 */
     POPUP_cursorize( cwid->linkrbrain_pb ) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Display the cluster report */

static void AFNI_cluster_widgize( Three_D_View *im3d , int force )
{
ENTRY("AFNI_cluster_widgize") ;
   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   if( !force ){
     if( im3d->vwid->func->cwid == NULL ) EXRETURN ;
     if( !MCW_widget_visible(im3d->vwid->func->cwid->wtop) ) EXRETURN ;
     if( IM3D_SHFT_CTRL_DRAG(im3d) ) EXRETURN ;
   }
   AFNI_clus_update_widgets( im3d ) ;
   if( im3d->vwid->func->cwid != NULL ){
     XtMapWidget( im3d->vwid->func->cwid->wtop ) ;
     XRaiseWindow( XtDisplay(im3d->vwid->func->cwid->wtop) ,
                   XtWindow (im3d->vwid->func->cwid->wtop)  ) ;
     im3d->vwid->func->cwid->is_open = 1 ;
   }
   SENSITIZE(im3d->vwid->imag->pop_jumpto_clus_pb,True) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Kill the cluster report */

static void AFNI_cluster_widgkill( Three_D_View *im3d )
{
ENTRY("AFNI_cluster_widgkill") ;
   if( !IM3D_OPEN(im3d) || im3d->vwid->func->cwid == NULL ) EXRETURN ;
   CLU_CLEAR_AUXDSET(im3d->vwid->func->cwid) ;
   im3d->vwid->func->cwid->splotim = NULL ;
   AFNI_clus_auxdsetlabel(im3d) ;
   XtUnmapWidget( im3d->vwid->func->cwid->wtop ) ;  /* hide, don't destroy */
   im3d->vwid->func->cwid->is_open = 0 ;
   DESTROY_CLARR(im3d->vwid->func->clu_list) ;
   SENSITIZE(im3d->vwid->imag->pop_jumpto_clus_pb,False) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Get the cluster index of the DICOM coords, if it exists. */

int AFNI_clus_find_xyz( Three_D_View *im3d , float x,float y,float z )
{
   float xf,yf,zf ; int xi,yi,zi , ii,jj,npt,nclu ;
   MCW_cluster_array *clar ; MCW_cluster *cl ;

ENTRY("AFNI_clus_find_xyz") ;

   if( !IM3D_OPEN(im3d) || !ISVALID_DSET(im3d->fim_now) ) RETURN(-1) ;
   clar = im3d->vwid->func->clu_list ; if( clar == NULL ) RETURN(-1) ;
   nclu = clar->num_clu ;              if( nclu == 0    ) RETURN(-1) ;

   MAT44_VEC( im3d->fim_now->daxes->dicom_to_ijk , x,y,z , xf,yf,zf ) ;
   xi = rint(xf) ; yi = rint(yf) ; zi = rint(zf) ;
   for( ii=0 ; ii < nclu ; ii++ ){
     cl = clar->clar[ii] ; npt = cl->num_pt ;
     for( jj=0 ; jj < npt ; jj++ ){
       if( xi == cl->i[jj] && yi == cl->j[jj] && zi == cl->k[jj] ) RETURN(ii);
     }
   }
   RETURN(-1) ;
}

/*---------------------------------------------------------------------------*/
/* A 'receive' function for when the viewpoint changes;
   used to report which cluster the crosshairs are in (if any) */

static void AFNI_clus_viewpoint_CB( int why, int np, void *vp, void *cd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   int ncl ; char lab[8] ;

ENTRY("AFNI_clus_viewpoint_CB") ;

   if( !IM3D_VALID(im3d) ) EXRETURN;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   switch( why ){

     case RECEIVE_VIEWPOINT:
       ncl = AFNI_clus_find_xyz( im3d ,
                                 im3d->vinfo->xi ,
                                 im3d->vinfo->yj , im3d->vinfo->zk ) ;

       im3d->vwid->func->clu_index = ncl ; /* not used at this time; someday? */
       if( ncl >= 0 ) sprintf(lab,"#%d",ncl+1) ;
       else           strcpy(lab,"--") ;
       MCW_set_widget_label( cwid->index_lab , lab ) ;
     break ;

     case RECEIVE_ALTERATION:{  /* some dataset changed? */
       int ww , nch ;
       for( nch=ww=0 ; ww < MAX_CLU_AUXDSET ; ww++ ){
         if( !ISVALID_DSET(im3d->fim_now)     ||
             !ISVALID_DSET(cwid->auxdset[ww]) ||
             DSET_NVOX(im3d->fim_now) != DSET_NVOX(cwid->auxdset[ww]) ){

             cwid->auxdset[ww] = NULL ; nch++ ;
         }
       }
       if( nch > 0 ) AFNI_clus_auxdsetlabel(im3d) ; /* re-label */
     }
     break ;

     case RECEIVE_CLOSURE:  /* user closed the AFNI controller? */
       AFNI_clus_popdown( im3d ) ;
       cwid->receive_on = 0 ;
     break ;

   }

   EXRETURN ;
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
   float px,py,pz , xx,yy,zz , pval,qval ;
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

   SENSITIZE(cwid->usemask_bbox->wrowcol,(im3d->vwid->func->clu_mask!=NULL)) ;

   if( nclu == 0 || cld == NULL ){
     for( ii=0 ; ii < cwid->nrow ; ii++ ) XtUnmanageChild( cwid->clu_rc[ii] ) ;
     cwid->nrow = 0 ;  /* # of managed rows */
     EXRETURN ;
   }

   if( rpt == NULL || *rpt == '\0' ) rpt = " \n --- Cluster Report --- \n " ;

   rrr = malloc(strlen(rpt)+1024) ; strcpy(rrr,rpt) ;

   pval = im3d->vinfo->func_pval ;
   ctab = CLU_get_thresh_table( im3d ) ;
   if( pval >= 0.0f && ctab != NULL ){
     MCW_popup_message_once( im3d->vwid->func->clu_report_pb ,
                               "--------------- ***** NOTICE ***** -----------------\n"
                               "\n"
                               " Cluster alpha calculations in 3dClustSim have been\n"
                               " changed, which may change the significance of the\n"
                               " reported clusters.\n"
                               "\n"
                               " The main change is that separate threshold tables\n"
                               " are now produced for 1-sided, 2-sided, and bi-sided\n"
                               " statistical thresholding.\n"
                               "\n"
                               " Previously, only 1-sided tables were created.\n"
                               " That method was overly conservative for 2-sided\n"
                               " tests, which are common in AFNI (eg, t-statistics).\n"
                               "\n"
                               " Please read the output of 3dClustSim -help for more\n"
                               " information about the new tables.\n"
                               "\n"
                               "------------- ***** Feb 2015 ***** -----------------" ,
                             "01 Jan 2016" , "Clusterize#A" ) ;
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
              " Alpha -> Cluster thresh: 0.10->%s : 0.05->%s : 0.01->%s\n" ,
              ssiz10 , ssiz05 , ssiz01 ) ;
#endif
   }

   qval = DSET_BRICK_FDRMIN(im3d->fim_now,im3d->vinfo->thr_index) ;
   if( qval > 0.1f )
     sprintf(rrr+strlen(rrr),
             " Min FDR q in threshold = %.2f: True detections are rare or weak\n",qval) ;

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
     cwid->clu_writ_pb =(Widget *)XtRealloc((char *)cwid->clu_writ_pb ,nclu*sizeof(Widget));
     cwid->clu_alph_lab=(Widget *)XtRealloc((char *)cwid->clu_alph_lab,nclu*sizeof(Widget));
     cwid->clu_see_bbox=(MCW_bbox **)XtRealloc((char *)cwid->clu_see_bbox,nclu*sizeof(MCW_bbox*)) ;
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
     if( cld[ii].nvox <= 9999 )
       sprintf(line,"%4d vox %+6.1f %+6.1f %+6.1f", cld[ii].nvox , px,py,pz ) ;
     else if( cld[ii].nvox <= 99999 )
       sprintf(line,"%5dvox %+6.1f %+6.1f %+6.1f" , cld[ii].nvox , px,py,pz ) ;
     else if( cld[ii].nvox <= 999999 )
       sprintf(line,"%6dvx %+6.1f %+6.1f %+6.1f"  , cld[ii].nvox , px,py,pz ) ;
     else if( cld[ii].nvox <= 9999999 )
       sprintf(line,"%7dv %+6.1f %+6.1f %+6.1f"   , cld[ii].nvox , px,py,pz ) ;
     else
       sprintf(line,"%8d %+6.1f %+6.1f %+6.1f"    , cld[ii].nvox , px,py,pz ) ;
     MCW_set_widget_label( cwid->clu_lab[ii] , line ) ;

     rrr = get_alpha_string( cld[ii].nvox , pval , im3d ) ;
     MCW_set_widget_label( cwid->clu_alph_lab[ii] , rrr ) ;

   } /* end of loop over widget rows */

   AFNI_clus_relabel_save_buttons( cwid ) ;  /* 14 Jun 2014 */

   SET_INDEX_LAB(im3d) ;

   if( !cwid->receive_on ){
     AFNI_receive_init(im3d, RECEIVE_VIEWPOINT_MASK,
                       AFNI_clus_viewpoint_CB, im3d, "AFNI_clus_viewpoint_CB") ;
     cwid->receive_on = 1 ;
   }

   if( do_wami ) {
       Boolean show_linkr;
       SENSITIZE( cwid->whermask_pb ,                /* 04 Aug 2010 */
                            (im3d->vinfo->view_type == VIEW_TALAIRACH_TYPE) ) ;
       show_linkr = ((im3d->vinfo->view_type == VIEW_TALAIRACH_TYPE) &&
                             show_linkrbrain_link() );
       SENSITIZE( cwid->linkrbrain_pb , show_linkr);         /* 31 Mar 2014 */
       AV_SENSITIZE( cwid->linkrbrain_av , show_linkr);       /* 31 Mar 2014 */
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

   if( !IM3D_VALID(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ;
   if( cwid != NULL ){
     CLU_CLEAR_AUXDSET(cwid) ; cwid->splotim = NULL ;
     AFNI_clus_auxdsetlabel(im3d) ;
     cwid->hbot = cwid->htop = 0.0f ; MCW_set_bbox( cwid->histsqrt_bbox , 0 ) ;
     XtUnmapWidget(cwid->wtop) ; cwid->is_open = 0 ;
     DESTROY_CLARR(im3d->vwid->func->clu_list) ;
     SENSITIZE(im3d->vwid->imag->pop_jumpto_clus_pb,False) ;
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

static AFNI_dataset_choose_stuff cdds = { 0,0, NULL, NULL , NULL } ;

static void AFNI_clus_finalize_dataset_CB( Widget w, XtPointer cd, MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   THD_3dim_dataset *dset ;
   int ival , ibut ;

ENTRY("AFNI_clus_finalize_dataset_CB") ;

   if( !IM3D_OPEN(im3d) || cbs == NULL )    { POPDOWN_strlist_chooser; EXRETURN; }

   cwid = im3d->vwid->func->cwid ;
   if( cwid == NULL || !cwid->is_open )     { POPDOWN_strlist_chooser; EXRETURN; }

   ibut = cdds.ncode ;
   if( ibut < 0 || ibut >= MAX_CLU_AUXDSET ){ POPDOWN_strlist_chooser; EXRETURN; }

   ival = cbs->ival ;
   if( ival < 0 || ival >= cdds.ndset )     { POPDOWN_strlist_chooser; EXRETURN; }
   dset = cdds.dset[ival] ;
   if( ISVALID_DSET(dset) ){
     cwid->auxdset[ibut] = dset ;
     XmToggleButtonSetState( cwid->auxdset_bbox[ibut]->wbut[0] , True , False ) ;
   } else {
     cwid->auxdset[ibut] = NULL ;
     XmToggleButtonSetState( cwid->auxdset_bbox[ibut]->wbut[0] , False , False ) ;
   }
   AFNI_clus_auxdsetlabel(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

static void AFNI_clus_finalize_scat1D_CB( Widget w, XtPointer cd, MCW_choose_cbs *cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   int ival ;

ENTRY("AFNI_clus_finalize_scat1D_CB") ;

   if( !IM3D_OPEN(im3d) || cbs == NULL ){ POPDOWN_timeseries_chooser; EXRETURN; }
   cwid = im3d->vwid->func->cwid ;
   if( cwid == NULL || !cwid->is_open ) { POPDOWN_timeseries_chooser; EXRETURN; }

   ival = cbs->ival ;
   if( ival >= 0 && ival < IMARR_COUNT(GLOBAL_library.timeseries) )
     cwid->splotim = IMARR_SUBIMAGE(GLOBAL_library.timeseries,ival) ;
   AFNI_clus_auxdsetlabel(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Callback for all pushbuttons (except 'Done') on the report window. */

void AFNI_clus_action_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   AFNI_clu_widgets *cwid ;
   int nclu , ii ;
   mri_cluster_detail *cld ;

ENTRY("AFNI_clus_action_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   /*--------- Aux Dset arrow ----------*/

   if( w == cwid->auxdset_arrow ){
     unsigned char adir ;
     XtVaGetValues( cwid->auxdset_arrow , XmNarrowDirection , &adir , NULL ) ;
     if( adir == XmARROW_RIGHT ) adir = XmARROW_DOWN ;
     else                        adir = XmARROW_RIGHT ;
     XtVaSetValues( cwid->auxdset_arrow , XmNarrowDirection , adir , NULL ) ;
     if( adir == XmARROW_DOWN ){
       XtManageChild  ( cwid->auxdset_master_rowcol  ) ;
     } else {
       XtUnmanageChild( cwid->auxdset_master_rowcol  ) ;
     }
     EXRETURN ;
   }

   /*--------- Scat.1D button ----------*/

   if( w == cwid->splot_pb ){
     if( IMARR_COUNT(GLOBAL_library.timeseries) > 0 ){
       int init_ts = AFNI_ts_in_library(cwid->splotim) ;
       MCW_choose_timeseries( cwid->top_lab , "Scatterplot x-axis" ,
                                     GLOBAL_library.timeseries , init_ts ,
                                     AFNI_clus_finalize_scat1D_CB , (XtPointer)im3d ) ;
     } else {
       MCW_popup_message( w , " \n"
                              "** No 1D files have  **\n"
                              "** been read in yet! **\n " ,
                          MCW_USER_KILL | MCW_TIMER_KILL ) ;
     }
     EXRETURN ;
   }

   if( w == cwid->splot_clear_pb ){
     cwid->splotim = NULL ;
     AFNI_clus_auxdsetlabel(im3d) ;  /* that was easy */
     EXRETURN ;
   }

   /*--------- SaveTabl button ---------*/

   if( w == cwid->savetable_pb ){
     char *temp;

     nclu = im3d->vwid->func->clu_num ;
     cld  = im3d->vwid->func->clu_det ;
     if( nclu == 0 || cld == NULL ) EXRETURN ;
     temp = AFNI_cluster_write_coord_table(im3d);
     free(temp);
     EXRETURN ;
   }

   /*--------- SaveMsk button ---------*/

   if( w == cwid->savemask_pb || w == cwid->whermask_pb ){  /* 01 May 2008 */
     char pref[128] , *ppp , *cmd , exopt[128] ;
     THD_3dim_dataset  *fset = im3d->fim_now , *mset ;
     MCW_cluster_array *clar = im3d->vwid->func->clu_list ;
     MCW_cluster *cl ;
     short *mask ; int ii,jj,nx,ny,nxy,ijk ;
     int do_wami = (w == cwid->whermask_pb && wherprog != NULL) ;
     int jtop ;

     nclu = im3d->vwid->func->clu_num ;
     cld  = im3d->vwid->func->clu_det ;
     if( nclu == 0 || cld == NULL ) EXRETURN ;

     ppp = XmTextFieldGetString( cwid->prefix_tf ) ;
     if( !THD_filename_pure(ppp) || strcmp(ppp,"-") == 0 ) ppp = "Clust" ;
     sprintf(pref,"%s_mask",ppp) ;
     if( use_old_3dclust )
       sprintf(exopt,"-savemask %s",pref) ;
     else
       sprintf(exopt,"-pref_map %s",pref) ;
     cmd = AFNI_clus_3dclust(im3d,exopt) ;  /* get the 3dclust command */

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
     nx = DSET_NX(mset); ny = DSET_NY(mset); nxy = nx*ny;
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
     DSET_write(mset) ; DSET_delete(mset) ;
     THD_force_ok_overwrite(0) ;
     ININFO_message("%s",cmd) ;

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
       INFO_message("Running WamI command:%s",ct) ;
       ININFO_message("%s",wout) ;
       fp = popen( wout , "r" ) ;
       if( fp == NULL ){
         (void)MCW_popup_message(w," \n*** Can't run whereami command? ***\n ",
                                 MCW_USER_KILL) ;
       } else {
         wout[0] = '\0' ;
         while( afni_fgets(wout+strlen(wout),WSIZ-2,fp) != NULL ){
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
     XmPushButtonCallbackStruct *pbcbs = (XmPushButtonCallbackStruct *)cbs ;
     char *cmd = AFNI_clus_3dclust(im3d,NULL) ;  /* get the 3dclust command */
     if( cmd != NULL ){
       INFO_message("clustering command:\n %s",cmd) ;
     } else {
       ERROR_message("Can't generate clustering command :(") ; /* shouldn't happen */
     }

     if( cmd != NULL && pbcbs != NULL &&               /* 01 Aug 2011: run it */
         ( ((XButtonEvent *)(pbcbs->event))->state & ShiftMask ) )
       system(cmd) ;

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

   /****** linkRbrain website link ****************************************/
   if(w == cwid->linkrbrain_pb) {  /* 11 Feb 2014 */
    char *lb_fnam=NULL;
    MCW_cluster_array *clar = im3d->vwid->func->clu_list ;
    int do_linkrbrain = (w == cwid->linkrbrain_pb && wherprog != NULL) ;
    int *slist , qq , ss , nss ;

    int jtop , etop, coord_colx, coord_coly, coord_colz;
    char *wout , ct[64] , csuf[4096] ; FILE *fp ; int inv ;

    nclu = im3d->vwid->func->clu_num ;
    cld  = im3d->vwid->func->clu_det ;

    if( nclu == 0 || cld == NULL || do_linkrbrain == 0) EXRETURN ;

    slist = (int *)malloc(sizeof(int)*nclu) ;   /* 16 Oct 2015 */
    for( ss=qq=0 ; qq < nclu ; qq++ ){          /* make list of what is seen */
      if( CLUST_SEE(im3d,qq) ) slist[ss++] = qq ;
    }
    nss = ss ;
    if( nss == 0 ){ free(slist) ; EXRETURN ; }  /* nothing to do */

    /* write out the coordinates to file first as in SaveTabl function*/
    lb_fnam = AFNI_cluster_write_coord_table(im3d);
    if(lb_fnam == NULL){ free(slist); EXRETURN; }  /* couldn't create coordinate table */
#undef  WSIZ
#define WSIZ 4096
printf("wrote cluster table to %s\n", lb_fnam);
     SHOW_AFNI_PAUSE ;
     MCW_invert_widget(cwid->linkrbrain_pb) ; inv = 1 ;
     wout = (char *)malloc(sizeof(char)*WSIZ) ;
     if(cwid->coord_mode == 1){  /* cmass columns */     /*-----------------*/
         coord_colx = 1; coord_coly = 2; coord_colz = 3; /* RWC: these were */
     }                                                   /* reversed! Fixed */
     else{   /* peak columns */                          /* on 09 Sep 2015. */
         coord_colx = 4; coord_coly = 5; coord_colz = 6; /*-----------------*/
     }

     jtop = clar->num_clu ;
     etop = (int)AFNI_numenv("AFNI_CLUSTER_WAMIMAX") ;
          if( etop <  1   ) etop = 20 ;
     else if( etop > 99   ) etop = 99 ;
          if( jtop > etop ) jtop = etop ;
     if( cwid->linkrbrain_nclu > 0 && jtop > cwid->linkrbrain_nclu )
       jtop = cwid->linkrbrain_nclu ;                        /* 09 Sep 2015 */

     if( jtop > 0 && jtop < nss ) nss = jtop ;

     /* make column and row selectors for cluster list file [16 Oct 2015] */

     sprintf(csuf,"[%d,%d,%d]{",coord_colx, coord_coly, coord_colz) ;
     for( ss=0 ; ss < nss ; ss++ ){
       sprintf(csuf+strlen(csuf),"%d%c" , slist[ss] , ((ss==nss-1)?'}':',') ) ;
     }
     free(slist) ;

#if 0
     if( jtop > 0 && jtop < clar->num_clu )                  /* 09 Sep 2015 */
       sprintf(csuf,"[%d,%d,%d]{0..%d}",coord_colx, coord_coly, coord_colz,jtop-1) ;
     else
       sprintf(csuf,"[%d,%d,%d]"       ,coord_colx, coord_coly, coord_colz) ;
#endif

     if(cwid->linkrbrain_av->ival == 0)   /* task correlation = default */
        sprintf(wout,"%s -linkrbrain -coord_file %s'%s' -space %s",
           wherprog,lb_fnam, csuf ,
           THD_get_space(im3d->fim_now)) ;
     else   /* gene correlation */
        sprintf(wout,"%s -linkrbrain -linkr_type genes -coord_file %s'%s' -space %s",
           wherprog,lb_fnam, csuf ,
           THD_get_space(im3d->fim_now)) ;

     if( jtop >= clar->num_clu ) strcpy (ct," ") ;
     else                        sprintf(ct," [first %d clusters]",jtop) ;
     INFO_message("Running WamI linkrbrain command:%s",ct) ;
     ININFO_message("%s",wout) ;
     fp = popen( wout , "r" ) ;
     if( fp == NULL ){
       (void)MCW_popup_message(w," \n*** Can't run whereami command? ***\n ",
                               MCW_USER_KILL) ;
     } else {
       wout[0] = '\0' ;
       while( afni_fgets(wout+strlen(wout),WSIZ-2,fp) != NULL ){
         wout = (char *)realloc(wout,sizeof(char)*(strlen(wout)+WSIZ)) ;
         MCW_invert_widget(cwid->linkrbrain_pb) ; inv = !inv ;
       }
       (void)pclose(fp) ;
       MCW_textwin_setbig(0) ;
       (void)new_MCW_textwin(w,wout,TEXT_READONLY) ;
     }

     if(lb_fnam) free(lb_fnam);
     free(wout) ;
     if( inv ) MCW_invert_widget(cwid->linkrbrain_pb) ;
     SHOW_AFNI_READY ;
     EXRETURN ;
   } /* end of linkrbrain-ization */

   /*---- scan Aux Dset list of widgets ----*/

   for( ii=0 ; ii < MAX_CLU_AUXDSET ; ii++ ){

     /*- dataset chooser? -*/

     if( w == cwid->auxdset_pb[ii] ){  /* call AFNI_choose_dataset_CB */
       int vv = im3d->vinfo->view_type ;
       THD_3dim_dataset *dset ;

       if( cdds.dset != NULL ){
         STATUS("free(cdds.dset)") ; free((void *)cdds.dset) ; cdds.dset = NULL ;
       }
       cdds.ndset = 0 ;
       STATUS("realloc(cdds.dset)") ;
       cdds.dset = (THD_3dim_dataset **)realloc(cdds.dset,
                                                sizeof(THD_3dim_dataset *)
                                               *im3d->ss_now->num_dsset  ) ;
       cdds.ncode = ii ;  /* mark which button was used */
       cdds.cb = AFNI_clus_finalize_dataset_CB ;
       for( ii=0 ; ii < im3d->ss_now->num_dsset ; ii++ ){ /* list of datasets */
         dset = GET_SESSION_DSET(im3d->ss_now, ii, vv) ;
         if( ISVALID_DSET(dset)                          && /* qualifications */
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

     /* there are no actions to take when a Aux Dset toggle is pressed,
        or when a From/To arrowval is altered -- these things will be
        read out when the user actuates a Save or Plot cluster button. */

   }  /*-- end of Aux Dset items --*/

   /*------ scan button list, see if widget matches one of them ------*/

   nclu = im3d->vwid->func->clu_num ;
   cld  = im3d->vwid->func->clu_det ;
   if( nclu == 0 || cld == NULL ) EXRETURN ;

   for( ii=0 ; ii < nclu ; ii++ ){  /* ii = cluster button row */

     /*-------- Jump to the cluster peak or cmass --------*/

     if( w == cwid->clu_jump_pb[ii] ){

       float px,py,pz , xx,yy,zz ;
       switch( cwid->coord_mode ){
         default:
         case PEAK_MODE:  xx=cld[ii].xpk; yy=cld[ii].ypk; zz=cld[ii].zpk; break;
         case CMASS_MODE: xx=cld[ii].xcm; yy=cld[ii].ycm; zz=cld[ii].zcm; break;
       }
       MAT44_VEC( im3d->fim_now->daxes->ijk_to_dicom , xx,yy,zz , px,py,pz ) ;
       if( (intptr_t)666 == (intptr_t)cbs ) AFNI_creepto_dicom( im3d , px,py,pz ) ;
       else                                 AFNI_jumpto_dicom ( im3d , px,py,pz ) ;
       EXRETURN ;  /* end of Jump */

     /*----------- Save a single cluster's mask ----------*/

     } else if( (w == cwid->clu_save_pb[ii] && cwid->save_as_mask) ||
                 w == cwid->clu_writ_pb[ii]                          ){ /* 14 Jun 2014 */
       char pref[128] , *ppp ;
       THD_3dim_dataset  *fset = im3d->fim_now , *mset ;
       MCW_cluster_array *clar = im3d->vwid->func->clu_list ;
       MCW_cluster *cl ;
       short *mask ; int nx,ny,nxy,ijk,jj ;

       nclu = im3d->vwid->func->clu_num ;
       cld  = im3d->vwid->func->clu_det ;
       if( nclu == 0 || cld == NULL ) EXRETURN ;

       ppp = XmTextFieldGetString( cwid->prefix_tf ) ;
       if( !THD_filename_pure(ppp) || strcmp(ppp,"-") == 0 ) ppp = "Clust" ;
       sprintf(pref,"%s_mask_%04d",ppp,ii+1) ;

       mset = EDIT_empty_copy(fset) ;
       tross_Copy_History(fset,mset) ;
       tross_Append_History(mset,"== Interactive clusterize mask") ;
       EDIT_dset_items( mset ,
                          ADN_prefix    , pref ,
                          ADN_nvals     , 1    ,
                          ADN_ntt       , 0    ,
                          ADN_type      , HEAD_FUNC_TYPE ,
                          ADN_func_type , FUNC_BUCK_TYPE ,
                        ADN_none ) ;
       nx = DSET_NX(mset); ny = DSET_NY(mset); nxy = nx*ny;
       EDIT_substitute_brick( mset , 0 , MRI_short , NULL ) ;
       mask = DSET_BRICK_ARRAY( mset , 0 ) ;
       cl = clar->clar[ii] ;
       for( jj=0 ; jj < cl->num_pt ; jj++ ){
         ijk = THREE_TO_IJK( cl->i[jj] , cl->j[jj] , cl->k[jj] , nx,nxy ) ;
         mask[ijk] = ii+1 ;
       }
       THD_force_ok_overwrite(1) ;
       INFO_message("Writing mask dataset %s",DSET_BRIKNAME(mset)) ;
       DSET_write(mset) ; DSET_delete(mset) ;
       THD_force_ok_overwrite(0) ;
       EXRETURN ;   /* end of Mask or Write */

     /*----------- Process the Aux Dset data for cluster #ii -----------*/

     } else if( w == cwid->clu_plot_pb[ii] || w == cwid->clu_save_pb[ii] ){

       int dosave = (w == cwid->clu_save_pb[ii]) ;  /* was it save OR plot */
       int domean = (cwid->aver_av->ival == 0) ;    /* type of processing */
       int domedn = (cwid->aver_av->ival == 1) ;
       int dopc   = (cwid->aver_av->ival == 2) ;
       int dohist = (cwid->aver_av->ival == 3) ;
       int dosmea = (cwid->aver_av->ival == 4) ;
       int dosall = (cwid->aver_av->ival == 5) ;
       int doscat = (dosall || dosmea) ;            /* scatter plot? */
       int doebar ;                                 /* error bars? */
       MRI_IMARR *imar[MAX_CLU_AUXDSET] , *imaq ;
       int        nxar[MAX_CLU_AUXDSET] ,
                  nyar[MAX_CLU_AUXDSET] ,
                  ibot[MAX_CLU_AUXDSET] ,
                  itop[MAX_CLU_AUXDSET] ,
                  inds[MAX_CLU_AUXDSET] ,
                  nim=0 , nxx=0 , ibx=MAX_INDEX,itx=0 , nysum=0,nxysum=0 , ids,nds ;
       MRI_IMAGE  *im[MAX_CLU_AUXDSET] , *imq=NULL ;
       MRI_IMAGE *sim[MAX_CLU_AUXDSET] ;
       float      ts_rrr[2*MAX_CLU_AUXDSET] ,
                  ts_ggg[2*MAX_CLU_AUXDSET] , ts_bbb[2*MAX_CLU_AUXDSET] ;

       SHOW_AFNI_PAUSE ;

       if( doscat && dosave ){                      /* really hopeless user */
         MCW_popup_message( w , " \n"
                                "** Can't use Scatterplot **\n"
                                "** and Save together :-( **\n " ,
                            MCW_USER_KILL | MCW_TIMER_KILL ) ;
         SHOW_AFNI_READY; EXRETURN ;
       }

       /* extract data from the Aux Dsets that are active */

       for( ids=0 ; ids < MAX_CLU_AUXDSET ; ids++ ){      /* NULL out local */
         imar[ids] = NULL ; im[ids] = sim[ids] = NULL ;   /* data pointers */
       }

       for( nds=ids=0 ; ids < MAX_CLU_AUXDSET ; ids++ ){
         imaq = AFNI_cluster_timeseries(im3d,ii,ids) ;    /* get the data */
         if( imaq != NULL && IMARR_COUNT(imaq) > 0 ){     /* did we get it? */
           imar[nds] = imaq ;                             /* data time series */
           nxar[nds] = IMARR_SUBIM(imaq,0)->nx ;          /* length of data */
           nyar[nds] = IMARR_COUNT(imaq) ;                /* num time series */
           inds[nds] = ids ;                              /* dataset index */
           ibot[nds] = cwid->auxdset_from_av[ids]->ival ; /* From index */
           itop[nds] = cwid->auxdset_to_av  [ids]->ival ; /* To index */
           if( ibot[nds] >= nxar[nds] )
             ibot[nds] = 0 ;
           if( itop[nds] <  ibot[nds] || itop[nds] >= nxar[nds] )
             itop[nds] = nxar[nds]-1 ;
           if( nxar[nds] > nxx ) nxx = nxar[nds] ;  /* longest time series */
           if( ibot[nds] < ibx ) ibx = ibot[nds] ;  /* smallest From */
           if( itop[nds] > itx ) itx = itop[nds] ;  /* largest To */
           nysum  += nyar[nds] ;                    /* total num time series */
           nxysum += nyar[nds] * (itop[nds]-ibot[nds]+1) ; /* total data pts */
           nds++ ;                /* add one to the count of active datasets */
         } else if( MCW_val_bbox(cwid->auxdset_bbox[ids]) != 0 ){
           XmToggleButtonSetState( cwid->auxdset_bbox[ids]->wbut[0] , False , False ) ;
         }
       }

       if( nds == 0 ){                              /* hopeless user */
         MCW_popup_message( w , " \n"
                                "** Can't get data!!! **\n"
                                "** Need Aux Dataset! **\n " ,
                            MCW_USER_KILL | MCW_TIMER_KILL ) ;
         SHOW_AFNI_READY; EXRETURN ;
       }

       doebar = (nds==1) && MCW_val_bbox(cwid->ebar_bbox) ; /* error bars? */

#if 0
       if( nds > 1 && doscat ){        /* this is MY problem, not the user's */
         MCW_popup_message( w , " \n"
                                "** Scatterplot only uses **\n"
                                "** first selected AuxD!  **\n" ,
                            MCW_USER_KILL | MCW_TIMER_KILL ) ;
         for( ids=1 ; ids < nds ; ids++ ) DESTROY_IMARR(imar[ids]) ;
         nds = 1 ;
       }
#endif

       /*-- set plot colors:      RED  GREEN   BLUE  PURPLE  --*/

       { static float rrr[4] = { 0.7f , 0.0f , 0.1f , 0.7f } ;
         static float ggg[4] = { 0.0f , 0.5f , 0.1f , 0.0f } ;
         static float bbb[4] = { 0.1f , 0.0f , 0.7f , 0.7f } ;
         plot_ts_setcolors( 4 , rrr,ggg,bbb ) ;
       }

       /*---------- build histogram ----------*/

       if( dohist ){
         float *far,*hbin[MAX_CLU_AUXDSET], hbot,htop,val,sbin ;
         int jj,kk,nbin,ih, nval , dosqrt ;
         hbot = cwid->hbot ; htop = cwid->htop ;   /* range from user */
         if( hbot >= htop ){            /* scan data for range to use */
           hbot = WAY_BIG ; htop = -hbot ;
           for( ids=0 ; ids < nds ; ids++ ){
             imaq = imar[ids] ;
             for( kk=0 ; kk < nyar[ids] ; kk++ ){
               far = MRI_FLOAT_PTR( IMARR_SUBIM(imaq,kk) ) ;
               for( jj=ibot[ids] ; jj <= itop[ids] ; jj++ ){
                 val = far[jj] ; if( val >= WAY_BIG ) continue ;
                 if( hbot > val ) hbot = val ;
                 if( htop < val ) htop = val ;
               }
             }
           }
           if( hbot >= htop ){
             for( ids=0 ; ids < nds ; ids++ ) DESTROY_IMARR(imar[ids]) ;
             SHOW_AFNI_READY; EXRETURN;  /* bad */
           }
         }

         if( (int)hbot == hbot && (int)htop == htop ){     /* integral range */
           nbin = htop - hbot ;
           if( nbin < 8 ){ nbin = 8 ; }
           else          { while( nbin > 1000 ) nbin /= 2 ; }  /* keep small */
         } else {
           nbin = 100 ;                          /* arbitrary number of bins */
         }
         kk = (int)sqrt((double)nxysum) ;
         if( nbin > kk ) nbin = MAX(kk,8) ;     /* final setting of num bins */
         sbin = 0.999999f * nbin / (htop-hbot) ;

         for( nval=ids=0 ; ids < nds ; ids++ ){          /* build histogram! */
           hbin[ids] = (float *)calloc(sizeof(float),(nbin+1)) ;
           imaq = imar[ids] ;
           for( kk=0 ; kk < nyar[ids] ; kk++ ){
             far = MRI_FLOAT_PTR( IMARR_SUBIM(imaq,kk) ) ;
             for( jj=ibot[ids] ; jj <= itop[ids] ; jj++ ){
               val = far[jj] ; if( val < hbot || val > htop ) continue ;
               ih = (int)(sbin*(val-hbot)) ; hbin[ids][ih]++ ; nval++ ;
             }
           }
           { int cc = cwid->auxdset_clr_av[inds[ids]]->ival ;  /* set color */
             ts_rrr[ids] = ts_rrr[ids+nds] = im3d->dc->ovc->r_ov[cc] / 299.0f ;
             ts_ggg[ids] = ts_ggg[ids+nds] = im3d->dc->ovc->g_ov[cc] / 299.0f ;
             ts_bbb[ids] = ts_bbb[ids+nds] = im3d->dc->ovc->b_ov[cc] / 299.0f ;
           }
         }

         dosqrt = !dosave && (MCW_val_bbox(cwid->histsqrt_bbox)==1) ;
         if( dosqrt ){
           for( ids=0 ; ids < nds ; ids++ ){
             for( ih=0 ; ih<=nbin ; ih++ ) hbin[ids][ih] = sqrtf(hbin[ids][ih]);
           }
         }

         if( !dosave ){   /*----- plot histogram -----*/

           char xlab[64] , ylab[64] , tlab[MAX_CLU_AUXDSET*THD_MAX_NAME+16] ;
           sprintf(xlab,"Data Value [%d bins; total %d values in range]",nbin,nval) ;
           sprintf(ylab,"Cluster #%d = %d vox", ii+1 , nyar[0] ) ;
           strcat(ylab,(dosqrt)?" [SqrtHist]" : " [Hist]") ;
           if( nds == 1 )
             sprintf(tlab,"%s\\noesc %s[%d..%d]" ,
                     print_color_string(ts_rrr[0],ts_ggg[0],ts_bbb[0]) ,
                     DSET_FILECODE(cwid->auxdset[inds[0]]) , ibot[0],itop[0] ) ;
           else{
             strcpy(tlab,"\\small") ; if( nds > 2 ) strcat(tlab,"\\small") ;
             for( ids=0 ; ids < nds ; ids++ )
               sprintf(tlab+strlen(tlab),"\\esc%s\\noesc %s[%d..%d] ",
                       print_color_string(ts_rrr[ids],ts_ggg[ids],ts_bbb[ids]) ,
                       DSET_PREFIX(cwid->auxdset[inds[ids]]),ibot[ids],itop[ids] ) ;
           }
           plot_ts_xypush(0,-1) ; plot_ts_setTHIK(0.004f) ; plot_ts_setthik(0.0015f) ;
           plot_ts_setcolors( 2*nds , ts_rrr,ts_ggg,ts_bbb ) ;  /* colors */
           if( nds == 1 )
             PLUTO_histoplot_f( nbin,hbot,htop , hbin[0] , xlab,ylab,tlab , 0,NULL ) ;
           else
             PLUTO_histoplot_f( nbin,hbot,htop , hbin[0] , xlab,ylab,tlab , nds-1,hbin+1 ) ;

         } else {         /*----- save histogram into 1D file -----*/

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
             ERROR_message("Can't open file %s for writing",fnam) ;  /* bad */
           } else {
             for( ids=0 ; ids < nds ; ids++ ){
               fprintf(fp,"# Histogram of %s[%d..%d]\n",
                       DSET_FILECODE(cwid->auxdset[inds[ids]]) , ibot[ids], itop[ids] ) ;
             }
             ppp = AFNI_clus_3dclust(im3d,NULL) ;  /* get the 3dclust command */
             if( ppp != NULL ){
               fprintf(fp,"# over Cluster #%d from command\n"   "# %s\n" , ii+1 , ppp ) ;
             }
             fprintf(fp,"# min data value = %g\n"
                        "# max data value = %g\n" , hbot,htop ) ;
             fprintf(fp,"# num of voxels  = %d\n"
                        "# num of values  = %d\n" , nyar[0] , nxysum ) ;
             for( jj=0 ; jj < nbin ; jj++ ){
               for( ids=0 ; ids < nds ; ids++ ) fprintf(fp," %7d",(int)hbin[ids][jj]) ;
               fprintf(fp,"\n") ;
             }
             if( fp != stdout ){
               fclose(fp) ;
               if( ff ) WARNING_message("Over-wrote file %s",fnam) ;
               else     INFO_message   ("Wrote file %s"     ,fnam) ;
             }
           }
         } /*-- done with save histogram --*/

         /* clean up the histo-trash */

         for( ids=0 ; ids < nds ; ids++ ){
           free(hbin[ids]) ; DESTROY_IMARR(imar[ids]) ;
         }
         SHOW_AFNI_READY; EXRETURN;

       } /* done with histogram-ification */

       /*------------ time series processing [Plot or Save] ------------*/

       if( (domean || dopc || domedn || dosmea) && itx <= ibx ){  /* bad */
         MCW_popup_message( w , " \n"
                                "** Need at least two   **\n"
                                "** time series indexes **\n"
                                "** for graphing data!  **\n " ,
                            MCW_USER_KILL | MCW_TIMER_KILL ) ;
         for( ids=0 ; ids < nds ; ids++ ) DESTROY_IMARR(imar[ids]) ;
         SHOW_AFNI_READY; EXRETURN ;
       }

       /* Despike all (sub)vectors in the data before combining them [26 Oct 2015] */

       if( MCW_val_bbox(cwid->despike_bbox) == 1 && (domean || dopc || domedn) ){
         int qq , pp ; float *far ;
         STATUS("despiking cluster time series") ;
         for( ids=0 ; ids < nds ; ids++ ){
           imaq = imar[ids] ; if( imaq == NULL ) continue ;
           pp   = itop[ids]-ibot[ids]+1 ;
           if( pp < 3 ){  /* can't plot this! */
             DESTROY_IMARR(imar[ids]) ; continue ;
           }
           for( qq=0 ; qq < nyar[ids] ; qq++ ){
             far = MRI_FLOAT_PTR( IMARR_SUBIMAGE(imaq,qq) ) + ibot[ids] ;
             THD_despike9( itop[ids]-ibot[ids]+1 , far ) ;
           }
         }
       }

       /* Detrend all (sub)vectors in the data before combining them [14 May 2015] */

       if( MCW_val_bbox(cwid->detrend_bbox) == 1 && (domean || dopc || domedn) ){
         int qq , pp ; float *far ;
         STATUS("detrending cluster time series") ;
         for( ids=0 ; ids < nds ; ids++ ){
           imaq = imar[ids] ; if( imaq == NULL ) continue ;
           pp   = itop[ids]-ibot[ids]+1 ;
           if( pp < 3 ){  /* can't plot this! */
             DESTROY_IMARR(imar[ids]) ; continue ;
           }
           pp = 1 + (int)myfloor(pp/150) ;
           for( qq=0 ; qq < nyar[ids] ; qq++ ){
             far = MRI_FLOAT_PTR( IMARR_SUBIMAGE(imaq,qq) ) + ibot[ids] ;
             if( domedn )
               THD_generic_detrend_L1 ( itop[ids]-ibot[ids]+1 , far , pp , 0,NULL,NULL ) ;
             else
               THD_generic_detrend_LSQ( itop[ids]-ibot[ids]+1 , far , pp , 0,NULL,NULL ) ;
           }
         }
       }

       /*--- extract single vector from each Aux Dset for display or save ---*/

       for( nim=ids=0 ; ids < nds ; ids++ ){

         imaq = imar[ids]; im[ids] = sim[ids] = NULL; if( imaq == NULL ) continue;

         if( nyar[ids] == 1 ){           /* should not transpire */
           STATUS("extract single data vector??") ;
           im[ids] = mri_copy(IMARR_SUBIM(imaq,0)) ;

         } else if( dopc ){              /*-------- PC#1 --------*/
           STATUS("extract PC#1") ;
           im[ids] = mri_pcvector( imaq , ibot[ids],itop[ids] ) ;

         } else if( domean ){            /*-------- Mean --------*/
           STATUS("extract mean vector") ;
           im[ids] = mri_meanvector( imaq , ibot[ids],itop[ids] ) ;
           if( doebar && !dosave )
             sim[ids] = mri_MMBvector( imaq,ibot[ids],itop[ids],2 ) ;

         } else if( domedn ){            /*-------- Median --------*/
           STATUS("extract median vector") ;
           im[ids] = mri_MMBvector( imaq , ibot[ids],itop[ids],0 ) ;
           if( doebar && !dosave )
             sim[ids] = mri_MMBvector( imaq,ibot[ids],itop[ids],2 ) ;

         } else if( doscat ){            /*----- scatterplot NOW NOW NOW -----*/
           float *xar=NULL, *yar=NULL ; int nix=0, niy=0, nixy=0, jj,kk ;
           float a=0,b=0,pcor=0,p025=0,p975=0 ;
           char xlab[256] , ylab[256] , tlab[THD_MAX_NAME+256] , rlab[4]="?" ;

           STATUS("scatterplot NOW") ;

           if( dosmea ){  /* scatterplot mean of cluster */
             im[ids] = mri_meanvector( imaq , ibot[ids],itop[ids] ) ;
             xar = MRI_FLOAT_PTR(im[ids]) ;
             nix = im[ids]->nx ; niy = 1 ; nixy = nix*niy ;
             yar = (float *)malloc(sizeof(float)*nixy) ;
             for( jj=0 ; jj < nix ; jj ++ ) yar[jj] = xar[jj] ;
             mri_free(im[ids]) ; im[ids] = NULL ;
           } else if( dosall ){
             nix = (itop[ids]-ibot[ids]+1) ; niy = nyar[ids] ; nixy = nix*niy ;
             yar = (float *)malloc(sizeof(float)*nixy) ;
             for( kk=0 ; kk < niy ; kk++ ){
               xar = MRI_FLOAT_PTR(IMARR_SUBIM(imaq,kk)) ;
               for( jj=0 ; jj < nix ; jj++ ) yar[jj+kk*nix] = xar[jj+ibot[ids]] ;
             }
           }
           xar = (float *)malloc(sizeof(float)*nixy) ;
           if( cwid->splotim != NULL && cwid->splotim->nx >= nix ){
             float *spar = MRI_FLOAT_PTR(cwid->splotim) ; int sbot ; char *eee ;
             eee = getenv("AFNI_CLUSTER_SCAT1D_START") ;
             if( eee != NULL && isdigit(*eee) ){
               sbot = (int)strtod(eee,NULL) ;
             } else {
               sbot = (cwid->splotim->nx >= nix+ibot[ids]) ? ibot[ids] : 0 ;
             }
             for( kk=0 ; kk < niy ; kk++ ){
               for( jj=0 ; jj < nix ; jj++ ) xar[jj+kk*nix] = spar[jj+sbot] ;
             }
             sprintf(xlab,"\\noesc %.62s[%d..%d]",
                     THD_trailname(cwid->splotim->name,0),sbot,sbot+nix-1) ;
           } else {                           /* no Scat.1D file? */
             for( kk=0 ; kk < niy ; kk++ )
               for( jj=0 ; jj < nix ; jj++ ) xar[jj+kk*nix] = jj+ibot[ids] ;
             strcpy(xlab,"Index") ;
             if( cwid->splotim != NULL )
               WARNING_message("Scat.1D file [%s] too short [%d] for dataset [%d]",
                               cwid->splotim->name , cwid->splotim->nx , nix+ibot[ids]  ) ;
           }
           if( niy == 1 && nix >= 9 ){  /* do correlations */
             float_triple aaa,bbb,rrr ;
             if( MCW_val_bbox(cwid->spearman_bbox) == 0 ){
               THD_pearson_corr_boot( nix,xar,yar , &rrr,&aaa,&bbb ) ;
               pcor = rrr.a ; p025 = rrr.b ; p975 = rrr.c ; a = aaa.a ; b = bbb.a ;
               strcpy(rlab,"R") ;
             } else {          /* [02 Jan 2013] -- Spearman bootstrap -- for PK */
               float fit[2]={0.0f,0.0f} ;
               THD_spearman_corr_boot( nix,xar,yar , &rrr ) ;
               pcor = rrr.a ; p025 = rrr.b ; p975 = rrr.c ;
               THD_generic_detrend_L1( -nix , yar , 0 , 1 , &xar , fit ) ;
               b = fit[0] ; a = fit[1] ; strcpy(rlab,"S") ;
             }
           }
           sprintf(ylab,"Cluster #%d = %d voxels",ii+1,nyar[ids]) ;
           sprintf(tlab,"\\noesc %s[%d..%d]",
                   DSET_FILECODE(cwid->auxdset[inds[ids]]), ibot[ids],itop[ids] ) ;
           if( pcor != 0.0f ){
             if( p025 < pcor && p975 > pcor ){
               if( strlen(tlab) > 30 )
                 sprintf(tlab+strlen(tlab),
                         "\\esc\\red  %s=%.2f\\in[%.2f..%.2f]_{95%%}",rlab,pcor,p025,p975) ;
               else
                 sprintf(tlab+strlen(tlab),
                         "\\esc\\red  %s=%.3f\\in[%.3f..%.3f]_{95%%}",rlab,pcor,p025,p975) ;
               if( p025*p975 > 0.0f )
                 strcat(tlab,"^{*}") ;
             } else {
                 sprintf(tlab+strlen(tlab),"\\esc\\red  %s=%.3f\\black",rlab,pcor) ;
             }
             strcat(tlab,"\\black") ;
           }
           PLUTO_set_xypush( cwid->splotim == NULL , 0 ) ;
           PLUTO_scatterplot( nixy,xar,yar , xlab,ylab,tlab , a,b ) ;
           PLUTO_set_xypush(1,1) ;
           free(xar) ; free(yar) ;
         }  /*---------- end of scatterplot ----------*/

         if( im[ids] != NULL ){
           int cc = cwid->auxdset_clr_av[inds[ids]]->ival ;
           ts_rrr[nim] = im3d->dc->ovc->r_ov[cc] / 299.0f ;  /* color for */
           ts_ggg[nim] = im3d->dc->ovc->g_ov[cc] / 299.0f ;  /* this data */
           ts_bbb[nim] = im3d->dc->ovc->b_ov[cc] / 299.0f ;
           nim++ ; /* count of number of im[] created herein */
         }

       } /*----- end of loop over Aux Dsets -----*/

       /* if we have saved 1D files in the im[] array, Plot or Save them now */

       if( nim > 0 ){
         float *yar[MAX_CLU_AUXDSET] ; int numyar ;
         float *far ; int ib=MAX_INDEX , it=0 , ibt , pp,qq , ids0=-66 ;

         STATUS("have multiple extracted vectors") ;

         for( ids=0 ; ids < nds ; ids++ ){
           if( im[ids] != NULL ){
             if( ibot[ids] < ib ) ib = ibot[ids] ;  /* smallest bot */
             if( itop[ids] > it ) it = itop[ids] ;  /* largest top */
             if( ids0 < 0 ) ids0 = ids ;
           }
         }
         ibt = it-ib+1 ;
         if( ibt < 3 ){  /* should not happen */
           STATUS("all vectors are too short to process!") ;
           for( ids=0 ; ids < nds ; ids++ ){
             DESTROY_IMARR(imar[ids]) ;
             if(  im[ids] != NULL ) mri_free( im[ids]) ;
             if( sim[ids] != NULL ) mri_free(sim[ids]) ;
           }
           SHOW_AFNI_READY; EXRETURN ;
         }

         for( numyar=ids=0 ; ids < nds ; ids++ ){          /* y curves to plot/save */
           if( im[ids] != NULL ){
             STATUS("extract vector into plotting array") ;
             far = MRI_FLOAT_PTR(im[ids]) ;
             yar[numyar] = (float *)malloc(sizeof(float)*ibt) ;
             for( qq=0 ; qq < ibt ; qq++ ){
               pp = qq+ib ;
               if( pp < ibot[ids] || pp > itop[ids] ){
                 yar[numyar][qq] = WAY_BIG ;
               } else {
                 pp -= ibot[ids] ;
                 yar[numyar][qq] = (pp < im[ids]->nx) ? far[pp] : WAY_BIG ;
               }
             }
             numyar++ ;
           }
         }

         if( !dosave ){                       /* Plotting (to rule the world) */
           char ylab[64] , tlab[MAX_CLU_AUXDSET*THD_MAX_NAME+16] ;
           int jj ; float *xax ;

           STATUS("prepare to plot extracted vectors") ;
           plot_ts_setcolors( nim , ts_rrr,ts_ggg,ts_bbb ) ;  /* colors */
           sprintf(ylab,"%s: Cluster #%d = %d voxels",
                   (dopc) ? "PC#1" : (domean) ? "Mean" : "Median" ,
                   ii+1 , nyar[ids0] ) ;
                if( nim > 2 ) strcpy(tlab,"\\small\\small") ;
           else if( nim > 1 ) strcpy(tlab,"\\small") ;
           else               tlab[0] = '\0' ;
           for( ids=0 ; ids < nds ; ids++ ){
             if( im[ids] != NULL ){
               sprintf(tlab+strlen(tlab),"\\esc%s\\noesc %s[%d..%d] ",
                       print_color_string(ts_rrr[ids],ts_ggg[ids],ts_bbb[ids]) ,
                       DSET_PREFIX(cwid->auxdset[inds[ids]]),ibot[ids],itop[ids]) ;
             }
           }
           xax = (float *)malloc(sizeof(float)*ibt) ;       /* x-axis for plot */
           for( qq=0 ; qq < ibt ; qq++ ) xax[qq] = ib+qq ;
           plot_ts_xypush(1,0) ; plot_ts_setTHIK(0.006f) ; plot_ts_setthik(0.0015f) ;
           X11_SET_NEW_PLOT ;
           if( !doebar || numyar > 1 || sim[0] == NULL ){
             STATUS("call plot_ts_lab") ;
             plot_ts_lab( im3d->dc->display ,
                          ibt , xax , numyar , yar ,
                          "TR index" , ylab , tlab , NULL , NULL ) ;
           } else {
             float *sar = MRI_FLOAT_PTR(sim[0]) , fac=2.0f/sqrtf(nyar[0]) ;
             for( qq=0 ; qq < sim[0]->nx ; qq++ ) sar[qq] *= fac ;
             STATUS("call plot_ts_ebar_win") ;
             plot_ts_ebar_win( im3d->dc->display ,
                               ibt , xax , yar[0] , sar+ib ,
                               "TR index" , ylab , tlab , NULL ) ;
           }
           STATUS("free(xax)") ;
           free((void *)xax) ;  /* end of Plot */

         } else {                             /* Save (the world) [26 Oct 2015] */
           char fnam[128] , *ppp ;
           char *typ = (dopc) ? "pc" : (domean) ? "mean" : "median" ;
           int cc , ff ; FILE *fp ;

           ppp = XmTextFieldGetString( cwid->prefix_tf ) ;
           if( !THD_filename_pure(ppp) ) ppp = "Clust" ;
           if( strcmp(ppp,"-") != 0 ){
             sprintf(fnam,"%s_%02d_%s.1D",ppp,ii+1,typ) ;
             ff = THD_is_file(fnam) ;
             fp = fopen(fnam,"w") ;
           } else {
             fp = stdout ; ff = 0 ;
           }
           if( fp == NULL ){
             ERROR_message("Can't open file %s for writing",fnam) ;
           } else {
             ppp = AFNI_clus_3dclust(im3d,NULL) ;  /* get the 3dclust command */
             if( ppp != NULL ){
               fprintf(fp,"# %s from Cluster #%d from command\n"
                          "#  %s\n" ,
                       typ , ii+1 , ppp ) ;
             }
             fprintf(fp , "# From dataset%s\n" , (nim == 1) ? "\0" : "s" ) ;
             for( ids=0 ; ids < nds ; ids++ ){
               if( im[ids] != NULL ){
                 fprintf(fp,"#  %s[%d..%d]\n",
                         DSET_PREFIX(cwid->auxdset[inds[ids]]),ibot[ids],itop[ids]) ;
               }
             }
             for( qq=0 ; qq < ibt ; qq++ ){
               for( cc=0 ; cc < numyar ; cc++ ) fprintf(fp," %14g",yar[cc][qq]) ;
               fprintf(fp,"\n") ;
             }
             fflush(fp) ;
             if( fp != stdout ){
               fclose(fp) ;
               if( ff ) WARNING_message("Over-wrote file %s",fnam) ;
               else     INFO_message   ("Wrote file %s"     ,fnam) ;
             }
           }
         } /* end of Save */

         STATUS("free(yar)") ;
         for( ids=0 ; ids < numyar ; ids++ ) free(yar[ids]) ;

       } /*-- end of (nim > 0) --*/

       /*===== end of Plot or Save [except for taking out the trash] =====*/

       STATUS("destroy imar and im") ;
       for( ids=0 ; ids < nds ; ids++ ){
         DESTROY_IMARR(imar[ids]) ;
         if(  im[ids] != NULL ) mri_free( im[ids]) ;
         if( sim[ids] != NULL ) mri_free(sim[ids]) ;
       }
       STATUS("Plot/Save is finito") ;
       SHOW_AFNI_READY; EXRETURN ;  /* end of Plot/Save */

     /*----------- See or Hide a single cluster ----------*/

     } else if( w == cwid->clu_see_bbox[ii]->wbut[0] ){  /* 15 Oct 2015 */

       THD_3dim_dataset  *fset = im3d->fim_now ;
       MCW_cluster_array *clar=im3d->vwid->func->clu_list ; int kk ;
       STATUS("toggling") ;
       if( ISVALID_DSET(fset) ){
         im3d->vedset.ival     = im3d->vinfo->fim_index ;
         im3d->vedset.param[0] = (float)im3d->vinfo->thr_index ;
         im3d->vedset.param[1] = get_3Dview_func_thresh(im3d,1);
         im3d->vedset.param[4] = im3d->vinfo->thr_sign ;
         im3d->vedset.param[5] = im3d->vinfo->use_posfunc ;
         im3d->vedset.exinfo   = NULL ;
         if( fset->dblk->vedim != NULL ){
           mri_free(fset->dblk->vedim) ; fset->dblk->vedim = NULL ;
         }
         (void) AFNI_vedit( fset, im3d->vedset,
                            (im3d->vednomask) ? NULL : im3d->vwid->func->clu_mask ) ;
       }
       if( ISVALID_DSET(fset) && fset->dblk->vedim != NULL && clar != NULL ){
         MRI_IMAGE *vm = fset->dblk->vedim ;
         im3d->vedskip = 1 ;
         for( kk=0 ; kk < nclu ; kk++ ){
           if( !CLUST_SEE(im3d,kk) ){
             MCW_vol_to_cluster(vm->nx,vm->ny,vm->nz ,
                                vm->kind,mri_data_pointer(vm) , clar->clar[kk] );
           }
         }
         AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_FLASH ) ;
         im3d->vedskip = 0 ;
#if 0
         for( kk=0 ; kk < nclu ; kk++ ){
           if( !CLUST_SEE(im3d,kk) ){
             MCW_cluster_to_vol(vm->nx,vm->ny,vm->nz ,
                                vm->kind,mri_data_pointer(vm) , clar->clar[kk] );
           }
         }
#endif
       }

       EXRETURN ;  /* end of see/hide */

     /*--------- flash the voxels for this cluster ---------*/

     } else if( w == cwid->clu_flsh_pb[ii] ){

       THD_3dim_dataset  *fset = im3d->fim_now ;
       MCW_cluster_array *clar = im3d->vwid->func->clu_list ; int jj ;
       if( !CLUST_SEE(im3d,ii) ) EXRETURN ;  /* 16 Oct 2015 */
       STATUS("flashing") ;
       if( ISVALID_DSET(fset) && fset->dblk->vedim == NULL ){
         im3d->vedset.ival     = im3d->vinfo->fim_index ;
         im3d->vedset.param[0] = (float)im3d->vinfo->thr_index ;
         im3d->vedset.param[1] = get_3Dview_func_thresh(im3d,1);
         im3d->vedset.param[4] = im3d->vinfo->thr_sign ;
         im3d->vedset.param[5] = im3d->vinfo->use_posfunc ;
         im3d->vedset.exinfo   = NULL ;
         (void) AFNI_vedit( fset, im3d->vedset,
                            (im3d->vednomask) ? NULL : im3d->vwid->func->clu_mask ) ;
       }
       if( ISVALID_DSET(fset) && fset->dblk->vedim != NULL && clar != NULL ){
         double tz , tt ; int ss ;
         MRI_IMAGE *vm = fset->dblk->vedim ;
         im3d->vedskip = 1 ; tz = PLUTO_elapsed_time() ;
         for( jj=0 ; jj < 3 ; jj++ ){
           MCW_invert_widget(w) ;
           MCW_vol_to_cluster(vm->nx,vm->ny,vm->nz ,
                              vm->kind,mri_data_pointer(vm) , clar->clar[ii] );
           AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_FLASH ) ;
           tt = PLUTO_elapsed_time() ; ss = 66-(int)(tt-tz) ; tz = tt ;
           if( ss > 0 ) NI_sleep(ss) ;
           MCW_invert_widget(w) ;
           MCW_cluster_to_vol(vm->nx,vm->ny,vm->nz ,
                              vm->kind,mri_data_pointer(vm) , clar->clar[ii] );
           AFNI_set_viewpoint( im3d , -1,-1,-1 , REDISPLAY_FLASH ) ;
           tt = PLUTO_elapsed_time() ; ss = 66-(int)(tt-tz) ; tz = tt ;
           if( ss > 0 ) NI_sleep(ss) ;
         }
         im3d->vedskip = 0 ;
       }
       EXRETURN ; /* end of Flash */

     } /* end of if over which button type we might have hit */

   } /*---------- end of loop over button rows ----------*/

   /* this should never be reached, unless the code is haunted */

   fprintf(stderr,"** Unknown button? **\n\a") ; EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/* write coordinate table to file - used by itself and with linkrbrain output */

char * AFNI_cluster_write_coord_table(Three_D_View *im3d)
{
     char fnam[128+THD_MAX_NAME] , *ppp ; FILE *fp ; int ff ;
     float px,py,pz , mx,my,mz , xx,yy,zz ;
     int nclu , ii ;
     mri_cluster_detail *cld ;
     AFNI_clu_widgets *cwid ;
     char *coord_table;

ENTRY("AFNI_cluster_write_coord_table") ;

     cwid = im3d->vwid->func->cwid ;
     nclu = im3d->vwid->func->clu_num ;
     cld  = im3d->vwid->func->clu_det ;

     if( nclu == 0 || cld == NULL ) RETURN(NULL) ;

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
       ppp = AFNI_clus_3dclust(im3d,NULL) ;  /* get the 3dclust command */
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
         coord_table = nifti_strdup(fnam);
         RETURN(coord_table);
       }
     }

   RETURN(NULL);
}

/*---------------------------------------------------------------------------*/
/* Get the time series from the points in a cluster */

static MRI_IMARR * AFNI_cluster_timeseries( Three_D_View *im3d , int ncl , int ids )
{
   AFNI_clu_widgets *cwid ;
   MCW_cluster_array *clar ;
   MCW_cluster *cl ;
   THD_3dim_dataset *dset ;
   MRI_IMARR *imar ;
   int ii,npt , *ind ;

ENTRY("AFNI_cluster_timeseries") ;

   if( !IM3D_OPEN(im3d) )                                RETURN(NULL) ; /* bad viewer */
   if( ids < 0 || ids >= MAX_CLU_AUXDSET )               RETURN(NULL) ; /* bad index */
   cwid = im3d->vwid->func->cwid ;  if( cwid == NULL )   RETURN(NULL) ; /* bad widgets */
   if( MCW_val_bbox(cwid->auxdset_bbox[ids]) == 0    )   RETURN(NULL) ; /* don't use */
   dset = im3d->vwid->func->cwid->auxdset[ids] ;
   if( !ISVALID_DSET(dset) )                             RETURN(NULL) ; /* bad dataset */
   clar = im3d->vwid->func->clu_list ;
   if( clar == NULL || ncl < 0 || ncl >= clar->num_clu ) RETURN(NULL) ; /* bad cluster */
   cl = clar->clar[ncl] ;
   npt = cl->num_pt ;                      if( npt < 1 ) RETURN(NULL) ; /* bad cluster */
   ind = (int *)malloc(sizeof(int)*npt) ;
   for( ii=0 ; ii < npt ; ii++ )
     ind[ii] = DSET_ixyz_to_index(dset,cl->i[ii],cl->j[ii],cl->k[ii]) ;
   imar = THD_extract_many_series( npt , ind , dset ) ;
   free((void *)ind) ;
   RETURN(imar) ;
}

/*---------------------------------------------------------------------------*/
/* Callback for arrowvals on the linkrbrain correlation type (tasks/genes) report panel
   just dummy function for now */

static void AFNI_linkrbrain_av_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
/*   AFNI_clu_widgets *cwid ; */

ENTRY("AFNI_linkrbrain_av_CB") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ;

   EXRETURN;

#if 0
   cwid = im3d->vwid->func->cwid ; if( cwid == NULL ) EXRETURN ;

   if( av == cwid->linkrbrain_av ){
     cwid->coord_mode = av->ival ;
     AFNI_clus_update_widgets(im3d) ; */ /* redisplay coordinates */
     EXRETURN ;
   }

   fprintf(stderr,"** Unknown button? **\n") ; EXRETURN ;
#endif

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

static char * AFNI_clus_3dclust( Three_D_View *im3d , char *extraopts )
{
   static char cmd[3333] ;
   VEDIT_settings vednew ;
   float thr,rmm,vmul,thb,tht ;
   int thrsign,posfunc,ithr , ival , bsid ;

ENTRY("AFNI_clus_3dclust") ;

   if( !IM3D_OPEN(im3d) ) RETURN(NULL) ;

   vednew = im3d->vedset ;

   ival    =      vednew.ival     ;
   ithr    = (int)vednew.param[0] ;
   thr     =      vednew.param[1] ;
   rmm     =      vednew.param[2] ;
   vmul    =      vednew.param[3] ;
   thrsign = (int)vednew.param[4] ;
   posfunc = (int)vednew.param[5] ;
   bsid    = (int)vednew.param[6] ; /* 29 Oct 2018 */

   thb = THBOT(thr) ; tht = THTOP(thr) ;

   if( AFNI_yesenv("AFNI_CLUSTERIZE_OLD") ) use_old_3dclust = 1 ;

   if( use_old_3dclust ){ /*-- [the olden way] -------------------------------*/

     sprintf(cmd,"3dclust -1Dformat -nosum -1dindex %d -1tindex %d",ival,ithr) ;

     if( posfunc )
       strcat(cmd," -1noneg") ;

     if( thb < tht )
       sprintf(cmd+strlen(cmd)," -2thresh %g %g",thb,tht) ;

     if( im3d->vwid->func->clu_mask != NULL ){  /* 02 Aug 2011 */
       if( im3d->vednomask ) sprintf(cmd+strlen(cmd)," -no_inmask") ;
       else                  sprintf(cmd+strlen(cmd)," -inmask") ;
     }

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

     if( extraopts != NULL ) sprintf(cmd+strlen(cmd)," %s",extraopts) ;

     sprintf(cmd+strlen(cmd)," %g %g %s",
             rmm , vmul , DSET_HEADNAME(im3d->fim_now) ) ;

   } else { /*-- [29 Oct 2018] -----------------------------------------------*/

     sprintf(cmd,"3dClusterize -nosum -1Dformat") ;

     sprintf(cmd+strlen(cmd)," -inset %s -idat %d -ithr %d",
                             DSET_HEADNAME(im3d->fim_now) , ival , ithr ) ;

     sprintf(cmd+strlen(cmd)," -NN %d",im3d->vwid->func->clu_nnlev) ;

     sprintf(cmd+strlen(cmd)," -clust_nvox %d",(int)vmul) ;

     if( thb < tht ){
       if( bsid )
         sprintf(cmd+strlen(cmd)," -bisided %g %g",thb,tht) ;
       else
         sprintf(cmd+strlen(cmd)," -2sided %g %g",thb,tht) ;
     }

     if( im3d->vwid->func->clu_mask != NULL && !im3d->vednomask )
       sprintf(cmd+strlen(cmd)," -mask_from_hdr") ;

#if 0
     if( posfunc )
       strcat(cmd," -1noneg") ;  /* not available in 3dClusterize? */
#endif

     if( extraopts != NULL ) sprintf(cmd+strlen(cmd)," %s",extraopts) ;

   } /*-----------------------------------------------------------------------*/

   RETURN(cmd) ;
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
         if( event->button == Button4 ){
           AFNI_func_setpval_CB(w,cd,NULL) ;  /* special case */
         } else if( event->button == Button5 ){
           AFNI_func_setqval_CB(w,cd,NULL) ;  /* special case */
         } else {
           im3d->vwid->butx = event->x_root ;
           im3d->vwid->buty = event->y_root ;
           event->button    = Button3 ;                           /* fakeout */
           XmMenuPosition( im3d->vwid->func->thr_menu , event ) ; /* where */
           XtManageChild ( im3d->vwid->func->thr_menu ) ;         /* popup */
         }
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

void AFNI_throlayx_change_CB( Widget w , XtPointer cd , XtPointer calld )
{
   Three_D_View *im3d = (Three_D_View *)cd ;
   int qq , pp ;

ENTRY("AFNI_throlayx_change_CB") ;

   if( ! IM3D_VALID(im3d) ) EXRETURN ;

   qq = im3d->vinfo->thr_olayx ;
   pp = MCW_val_bbox( im3d->vwid->func->thr_olayx_bbox ) ;

   if( pp != qq ){
     im3d->vinfo->thr_olayx = pp ;
#if 0
     /** MCW_invert_widget(im3d->vwid->func->thr_buck_av->wrowcol) ; **/
#else
     MCW_set_widget_label( im3d->vwid->func->thr_buck_av->wlabel ,
                           (pp > 0) ? "Thr*" : "Thr " ) ;
#endif
     AFNI_enforce_throlayx(im3d) ;
   }
   EXRETURN ;
}

/*-----------------------------------------------------------------*/

void AFNI_enforce_throlayx( Three_D_View *im3d )
{
   int ithr ;

ENTRY("AFNI_enforce_throlayx") ;

   if( !IM3D_VALID(im3d) || !ISVALID_DSET(im3d->fim_now)
                         || !im3d->vinfo->thr_olayx      ) EXRETURN ;

   if( im3d->vinfo->thr_olayx == 1 )       /* 24 Jun 2013 */
     ithr = im3d->vinfo->fim_index ;       /* allow Thr=OLay */
   else if( im3d->vinfo->thr_olayx == 2 )  /* or */
     ithr = im3d->vinfo->fim_index + 1 ;   /* Thr=OLay+1 */
   else
     EXRETURN ;

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
   NI_element *nel = NULL ;
   char *mask_idc  = NULL ;
   CLU_threshtable *ctab ;
   ATR_string *atr ;
   char *msg=NULL ; int ntab=0,nmask=0 ; static int ntabold=-1 ;

#define GET_MASK_IDC                                          \
 do{ if( mask_idc == NULL && nel != NULL ){                   \
      char *idc = NI_get_attribute(nel,"mask_dset_idcode") ;  \
      if( idc != NULL ) mask_idc = strdup(idc) ;              \
 } } while(0)

ENTRY("CLU_setup_alpha_tables") ;

   if( !IM3D_VALID(im3d) ) EXRETURN ;

   /* free anything we have now */

   STATUS("free-ing old tables") ;

   CLU_free_table( im3d->vwid->func->clu_tabNN1_1sid ) ; im3d->vwid->func->clu_tabNN1_1sid = NULL ;
   CLU_free_table( im3d->vwid->func->clu_tabNN2_1sid ) ; im3d->vwid->func->clu_tabNN2_1sid = NULL ;
   CLU_free_table( im3d->vwid->func->clu_tabNN3_1sid ) ; im3d->vwid->func->clu_tabNN3_1sid = NULL ;

   CLU_free_table( im3d->vwid->func->clu_tabNN1_2sid ) ; im3d->vwid->func->clu_tabNN1_2sid = NULL ;
   CLU_free_table( im3d->vwid->func->clu_tabNN2_2sid ) ; im3d->vwid->func->clu_tabNN2_2sid = NULL ;
   CLU_free_table( im3d->vwid->func->clu_tabNN3_2sid ) ; im3d->vwid->func->clu_tabNN3_2sid = NULL ;

   CLU_free_table( im3d->vwid->func->clu_tabNN1_bsid ) ; im3d->vwid->func->clu_tabNN1_bsid = NULL ;
   CLU_free_table( im3d->vwid->func->clu_tabNN2_bsid ) ; im3d->vwid->func->clu_tabNN2_bsid = NULL ;
   CLU_free_table( im3d->vwid->func->clu_tabNN3_bsid ) ; im3d->vwid->func->clu_tabNN3_bsid = NULL ;

   if( im3d->vwid->func->clu_mask != NULL ){
     STATUS("free-ing old mask") ;
     free(im3d->vwid->func->clu_mask) ; im3d->vwid->func->clu_mask = NULL ;
   }

   /* get info from the Overlay dataset header, if present */

   dset = im3d->fim_now ; if( !ISVALID_DSET(dset) ) EXRETURN ;

   /* NN1 cluster C(p,alpha) tables */

   STATUS("look for _NN1_1sided") ;
   atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN1_1sided" ) ;
   if( atr == NULL ){
     STATUS("  instead look for _NN1") ;
     atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN1" ) ;  /* ye olde way */
   }
   if( atr != NULL ){
     STATUS("  read NIML element from NN1 attribute") ;
     nel = NI_read_element_fromstring(atr->ch) ;  /* attribute string => NIML */
     STATUS("  format cluster table from element") ;
     ctab = format_cluster_table(nel) ;           /* NIML => C(p,alpha) table */
     GET_MASK_IDC ;
     NI_free_element(nel) ;         /* get rid of the C(p,alpha) NIML element */
     im3d->vwid->func->clu_tabNN1_1sid = ctab ;
     msg = THD_zzprintf(msg," NN=1:1sid") ; ntab += 1 ;
   }

   STATUS("look for _NN1_2sided") ;
   atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN1_2sided" ) ;
   if( atr != NULL ){
     STATUS("  read NIML element from NN1 attribute") ;
     nel = NI_read_element_fromstring(atr->ch) ;  /* attribute string => NIML */
     STATUS("  format cluster table from element") ;
     ctab = format_cluster_table(nel) ;           /* NIML => C(p,alpha) table */
     GET_MASK_IDC ;
     NI_free_element(nel) ;         /* get rid of the C(p,alpha) NIML element */
     im3d->vwid->func->clu_tabNN1_2sid = ctab ;
     msg = THD_zzprintf(msg," NN=1:2sid") ; ntab += 1 << 1 ;
   }

   STATUS("look for _NN1_bisided") ;
   atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN1_bisided" ) ;
   if( atr != NULL ){
     STATUS("  read NIML element from NN1 attribute") ;
     nel = NI_read_element_fromstring(atr->ch) ;  /* attribute string => NIML */
     STATUS("  format cluster table from element") ;
     ctab = format_cluster_table(nel) ;           /* NIML => C(p,alpha) table */
     GET_MASK_IDC ;
     NI_free_element(nel) ;         /* get rid of the C(p,alpha) NIML element */
     im3d->vwid->func->clu_tabNN1_bsid = ctab ;
     msg = THD_zzprintf(msg," NN=1:bsid") ; ntab += 1 << 2 ;
   }

   /* NN2 cluster C(p,alpha) tables */

   STATUS("look for _NN2_1sided") ;
   atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN2_1sided" ) ;
   if( atr == NULL ){
     STATUS("  instead look for _NN2") ;
     atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN2" ) ;  /* ye olde way */
   }
   if( atr != NULL ){
     STATUS("  read NIML element from NN2 attribute") ;
     nel = NI_read_element_fromstring(atr->ch) ;  /* attribute string => NIML */
     STATUS("  format cluster table from element") ;
     ctab = format_cluster_table(nel) ;           /* NIML => C(p,alpha) table */
     GET_MASK_IDC ;
     NI_free_element(nel) ;         /* get rid of the C(p,alpha) NIML element */
     im3d->vwid->func->clu_tabNN2_1sid = ctab ;
     msg = THD_zzprintf(msg," NN=1:1sid") ; ntab += 1 << 3 ;
   }

   STATUS("look for _NN2_2sided") ;
   atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN2_2sided" ) ;
   if( atr != NULL ){
     STATUS("  read NIML element from NN2 attribute") ;
     nel = NI_read_element_fromstring(atr->ch) ;  /* attribute string => NIML */
     STATUS("  format cluster table from element") ;
     ctab = format_cluster_table(nel) ;           /* NIML => C(p,alpha) table */
     GET_MASK_IDC ;
     NI_free_element(nel) ;         /* get rid of the C(p,alpha) NIML element */
     im3d->vwid->func->clu_tabNN2_2sid = ctab ;
     msg = THD_zzprintf(msg," NN=1:2sid") ; ntab += 1 << 4 ;
   }

   STATUS("look for _NN2_bisided") ;
   atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN2_bisided" ) ;
   if( atr != NULL ){
     STATUS("  read NIML element from NN2 attribute") ;
     nel = NI_read_element_fromstring(atr->ch) ;  /* attribute string => NIML */
     STATUS("  format cluster table from element") ;
     ctab = format_cluster_table(nel) ;           /* NIML => C(p,alpha) table */
     GET_MASK_IDC ;
     NI_free_element(nel) ;         /* get rid of the C(p,alpha) NIML element */
     im3d->vwid->func->clu_tabNN2_bsid = ctab ;
     msg = THD_zzprintf(msg," NN=1:bsid") ; ntab += 1 << 5 ;
   }

   /* NN3 cluster C(p,alpha) tables */

   STATUS("look for _NN3_1sided") ;
   atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN3_1sided" ) ;
   if( atr == NULL ){
     STATUS("  instead look for _NN3") ;
     atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN3" ) ;  /* ye olde way */
   }
   if( atr != NULL ){
     STATUS("  read NIML element from NN3 attribute") ;
     nel = NI_read_element_fromstring(atr->ch) ;  /* attribute string => NIML */
     STATUS("  format cluster table from element") ;
     ctab = format_cluster_table(nel) ;           /* NIML => C(p,alpha) table */
     GET_MASK_IDC ;
     NI_free_element(nel) ;         /* get rid of the C(p,alpha) NIML element */
     im3d->vwid->func->clu_tabNN3_1sid = ctab ;
     msg = THD_zzprintf(msg," NN=1:1sid") ; ntab += 1 << 6 ;
   }

   STATUS("look for _NN3_2sided") ;
   atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN3_2sided" ) ;
   if( atr != NULL ){
     STATUS("  read NIML element from NN3 attribute") ;
     nel = NI_read_element_fromstring(atr->ch) ;  /* attribute string => NIML */
     STATUS("  format cluster table from element") ;
     ctab = format_cluster_table(nel) ;           /* NIML => C(p,alpha) table */
     GET_MASK_IDC ;
     NI_free_element(nel) ;         /* get rid of the C(p,alpha) NIML element */
     im3d->vwid->func->clu_tabNN3_2sid = ctab ;
     msg = THD_zzprintf(msg," NN=1:2sid") ; ntab += 1 << 7 ;
   }

   STATUS("look for _NN3_bisided") ;
   atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_NN3_bisided" ) ;
   if( atr != NULL ){
     STATUS("  read NIML element from NN3 attribute") ;
     nel = NI_read_element_fromstring(atr->ch) ;  /* attribute string => NIML */
     STATUS("  format cluster table from element") ;
     ctab = format_cluster_table(nel) ;           /* NIML => C(p,alpha) table */
     GET_MASK_IDC ;
     NI_free_element(nel) ;         /* get rid of the C(p,alpha) NIML element */
     im3d->vwid->func->clu_tabNN3_bsid = ctab ;
     msg = THD_zzprintf(msg," NN=1:bsid") ; ntab += 1 << 8 ;
   }

   /* search for ASCII mask string, if needed */

   if( ntab ){
     STATUS("look for _MASK") ;
     atr = THD_find_string_atr( dset->dblk , "AFNI_CLUSTSIM_MASK" ) ;
     if( atr != NULL ){
       int nvox ;
       STATUS("  count mask from string attribute") ;
       nvox = mask_b64string_nvox(atr->ch) ;  /* length of mask */
       if( nvox == DSET_NVOX(dset) ){         /* must match dataset */
         STATUS(" make mask from string attribute") ;
         im3d->vwid->func->clu_mask = mask_from_b64string(atr->ch,&nvox) ;
       }
     } else if( mask_idc != NULL ){ /* search for original mask dataset via its idcode */
       THD_3dim_dataset *mset ;
       STATUS("  instead look via mask_dset_idcode") ;
       mset = PLUTO_find_dset_idc(mask_idc) ;
       if( mset != NULL && DSET_NVOX(mset) == DSET_NVOX(dset) ){
         STATUS("  instead read mask from external dataset") ;
         im3d->vwid->func->clu_mask = THD_makemask(mset,0,1.0,0.0) ;
         DSET_unload(mset) ;
       }
     }
     if( im3d->vwid->func->clu_mask != NULL )
       nmask = THD_countmask( DSET_NVOX(dset) , im3d->vwid->func->clu_mask ) ;
   }

   if( mask_idc != NULL ) free(mask_idc) ;

   /* messages */

   if( ntab != ntabold ){
     if( msg != NULL ){
       INFO_message("%s3dClustSim tables found:%s" ,
                    AFNI_controller_label(im3d) , msg ) ;
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
   CLU_threshtable *ctab ; int sig , scod , bsid , pfun ;

ENTRY("CLU_get_thresh_table") ;

   if( !IM3D_VALID(im3d) ) RETURN(NULL) ;

   scod = DSET_BRICK_STATCODE(im3d->fim_now,im3d->vinfo->thr_index) ;
   sig  = THD_stat_is_2sided( scod , im3d->vinfo->thr_sign ) ;
   if( sig < 0 ) RETURN(NULL) ;  /* should never transpire */

   pfun = (int)im3d->vedset.param[5] ; if( pfun ) sig = 0 ;
   bsid = (int)im3d->vedset.param[6] ;
   if( sig && bsid && scod != FUNC_FT_TYPE ){
     switch( im3d->vwid->func->clu_nnlev ){
       default: ctab = im3d->vwid->func->clu_tabNN1_bsid ; /* INFO_message("b-sid NN=1"); */ break ;
       case 2:  ctab = im3d->vwid->func->clu_tabNN2_bsid ; /* INFO_message("b-sid NN=2"); */ break ;
       case 3:  ctab = im3d->vwid->func->clu_tabNN3_bsid ; /* INFO_message("b-sid NN=3"); */ break ;
     }
     RETURN(ctab) ;
   }

   switch( sig ){
     case 1: default:                          /* 2-sided */
       switch( im3d->vwid->func->clu_nnlev ){
         default: ctab = im3d->vwid->func->clu_tabNN1_2sid ; /* INFO_message("2-sid NN=1"); */ break ;
         case 2:  ctab = im3d->vwid->func->clu_tabNN2_2sid ; /* INFO_message("2-sid NN=2"); */ break ;
         case 3:  ctab = im3d->vwid->func->clu_tabNN3_2sid ; /* INFO_message("2-sid NN=3"); */ break ;
       }
     break ;

     case 0:                                  /* 1-sided */
       switch( im3d->vwid->func->clu_nnlev ){
         default: ctab = im3d->vwid->func->clu_tabNN1_1sid ; /* INFO_message("1-sid NN=1"); */ break ;
         case 2:  ctab = im3d->vwid->func->clu_tabNN2_1sid ; /* INFO_message("1-sid NN=2"); */ break ;
         case 3:  ctab = im3d->vwid->func->clu_tabNN3_1sid ; /* INFO_message("1-sid NN=3"); */ break ;
       }
     break ;
   }
   RETURN(ctab) ;
}
