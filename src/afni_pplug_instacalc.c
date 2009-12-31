#include "afni.h"
#include "parser.h"

static int DEBUG = 0 ;

/*--------------------- macros for drawing lines -----------------------------*/

#undef  VLINE
#define VLINE(rr)                                                           \
     (void) XtVaCreateManagedWidget( "menu", xmSeparatorWidgetClass, (rr) , \
                                        XmNorientation   , XmVERTICAL    ,  \
                                        XmNseparatorType , XmSINGLE_LINE ,  \
                                     NULL )
#undef  VLINEE
#define VLINEE(rr)                                                          \
     (void) XtVaCreateManagedWidget( "menu", xmSeparatorWidgetClass, (rr) , \
                                        XmNorientation   , XmVERTICAL    ,  \
                                      XmNseparatorType, XmSHADOW_ETCHED_IN, \
                                     NULL )
#undef  HLINE
#define HLINE(rr)                                                           \
   (void) XtVaCreateManagedWidget( "menu", xmSeparatorWidgetClass, (rr) ,   \
                                      XmNseparatorType   , XmSINGLE_LINE ,  \
                                   NULL )
#undef  HLINEE
#define HLINEE(rr)                                                          \
   (void) XtVaCreateManagedWidget( "menu", xmSeparatorWidgetClass, (rr) ,   \
                                      XmNseparatorType, XmSHADOW_ETCHED_IN, \
                                   NULL )

/*------------------------- various string constants -------------------------*/

#undef  ICALC_NUMTYPE
#define ICALC_NUMTYPE 3
static char *ICALC_typestr[] = { "Dataset: Value" ,
                                 "Dataset: Stat." ,
                                 "Constant Value"  } ;

static char *ICALC_choosestr[] = { "Choose Dataset" ,
                                   "Choose Dataset" ,
                                   "--------------"  } ;

static char *ICALC_labelstr[]  = { "diffsub:" ,
                                   "3D stat:" ,
                                   "Value:"     } ;

static char *ICALC_nothing_chosen = "---nothing chosen---" ;

/*--------------------------------- other macros -----------------------------*/

static char abet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;  /* the alphabet! */

#define ICALC_INVALID     -666
#define ICALC_DSET_VALUE  0
#define ICALC_DSET_STAT   1
#define ICALC_CONSTANT    2

#define DSHIFT_MODE_STOP  0
#define DSHIFT_MODE_WRAP  1
#define DSHIFT_MODE_ZERO  2

#define MANGLE_NONE       0
#define MANGLE_RAI        1
#define MANGLE_LPI        2

#define CX_REALPART       0
#define CX_IMAGPART       1
#define CX_MAGNITUDE      2
#define CX_PHASE          3

/*---------------------------------- prototypes ------------------------------*/

static void ICALC_tog_bbox_CB( Widget , XtPointer , XtPointer ) ;
static void ICALC_menu_av_CB ( MCW_arrowval * , XtPointer ) ;
static void ICALC_chooser_CB ( Widget, XtPointer, XtPointer ) ;
static void ICALC_index_av_CB( MCW_arrowval * , XtPointer ) ;

static char * ICALC_index_lab_CB( MCW_arrowval * , XtPointer ) ;

static void ICALC_quit_CB    ( Widget, XtPointer, XtPointer ) ;
static void ICALC_compute_CB ( Widget, XtPointer, XtPointer ) ;
static void ICALC_help_CB    ( Widget, XtPointer, XtPointer ) ;

static void ICALC_choose_dataset( ICALC_widget_set *, int ) ;
static void ICALC_finalize_dataset_CB( Widget, XtPointer, MCW_choose_cbs * ) ;

static void        ICALC_finalize_setup( ICALC_widget_set *, ICALC_setup * ) ;
static MRI_IMAGE * ICALC_compute( ICALC_setup * ) ;

/*------------------------------ define action area --------------------------*/

#undef  ICALC_NUMACT
#define ICALC_NUMACT 3
static MCW_action_item ICALC_act[] =
 { { "Quit InstaCalc"   , ICALC_quit_CB   , NULL,NULL, "Shutdown InstaCalc operations", 0 },
   { "Compute InstaCalc", ICALC_compute_CB, NULL,NULL, "Recompute ?_ICALC dataset"    , 1 },
   { "A Little Help"    , ICALC_help_CB   , NULL,NULL, "from my friends"              , 0 }
  } ;

/*------------- macros to toggle a row of ICALC widgets on or off ------------*/

#undef  ICALC_toggle_row
#define ICALC_toggle_row(rr,state)                                                          \
 do{ int qz = state && (rr).menu_av->ival != ICALC_CONSTANT ;                               \
     int wz = state &&                                                                      \
        ((rr).menu_av->ival == ICALC_DSET_VALUE || (rr).menu_av->ival == ICALC_DSET_STAT) ; \
     AV_SENSITIZE((rr).menu_av,state) ;                                                     \
     XtSetSensitive((rr).chooser_pb,qz) ; AV_SENSITIZE((rr).index_av,wz) ;                  \
     XtSetSensitive((rr).chooser_lab,qz) ;                                                  \
     XtSetSensitive((rr).string_text,state) ; XtSetSensitive((rr).string_lab,state) ;       \
 } while(0)

#undef  ICALC_row_off
#undef  ICALC_row_on
#define ICALC_row_off(ic,aa) ICALC_toggle_row((ic)->war[aa],False)
#define ICALC_row_on(ic,aa)  ICALC_toggle_row((ic)->war[aa],True)

/*-------------- longish macro to make one row of ICALC widgets --------------*/

#undef  ICALC_userdata
#define ICALC_userdata(w,x) XtVaSetValues((w),XmNuserData,(XtPointer)ITOP((x)),NULL)

#undef  MAKE_ICALC_ROW
#define MAKE_ICALC_ROW(aa)                                           \
 do{ char sss[32] , *sp[1] ; Widget rc ; int ee = (aa)%2==0 ;        \
     char *ff = (ee) ? "menu" : "dialog" ;                           \
     HLINEE(iwid->rowcol) ;                                          \
     rc = iwid->war[aa].rc =                                         \
        XtVaCreateWidget(                                            \
           ff     , xmRowColumnWidgetClass , iwid->rowcol ,          \
             XmNpacking     , XmPACK_TIGHT ,                         \
             XmNorientation , XmHORIZONTAL ,                         \
             XmNadjustMargin , True ,                                \
             XmNmarginHeight , 2 , XmNmarginWidth , 0 ,              \
             XmNtraversalOn , True ,                                 \
           NULL ) ;                                                  \
     if( !ee ) MCW_set_widget_bg(rc,"black",0) ;                     \
     sss[0] = abet[aa] ; sss[1] = '\0'; sp[0] = sss;                 \
     iwid->war[aa].tog_bbox =                                        \
        new_MCW_bbox( rc, 1,sp, MCW_BB_check,MCW_BB_noframe,         \
                      ICALC_tog_bbox_CB , (XtPointer)ITOP(iwid) ) ;  \
     ICALC_userdata(iwid->war[aa].tog_bbox->wbut[0],aa+1) ;          \
     VLINE(iwid->war[aa].rc) ;                                       \
     iwid->war[aa].menu_av =                                         \
        new_MCW_optmenu( rc , "Type" , 0, ICALC_NUMTYPE-1, 0, 0,     \
                         ICALC_menu_av_CB, (XtPointer)ITOP(iwid) ,   \
                         MCW_av_substring_CB, ICALC_typestr ) ;      \
     ICALC_userdata(iwid->war[aa].menu_av->wrowcol,aa+1) ;           \
     VLINEE(iwid->war[aa].rc) ;                                      \
     iwid->war[aa].chooser_pb =                                      \
        XtVaCreateManagedWidget(                                     \
            ff     , xmPushButtonWidgetClass , rc ,                  \
            LABEL_ARG(ICALC_choosestr[0]) , XmNtraversalOn , True ,  \
            XmNinitialResourcesPersistent , False , NULL ) ;         \
     XtAddCallback( iwid->war[aa].chooser_pb , XmNactivateCallback , \
                    ICALC_chooser_CB , (XtPointer)ITOP(iwid) ) ;     \
     ICALC_userdata(iwid->war[aa].chooser_pb,aa+1) ;                 \
     iwid->war[aa].index_av =                                        \
        new_MCW_optmenu( rc , "[-]" , -1,0, 0,0,                     \
                         ICALC_index_av_CB, (XtPointer)ITOP(iwid) ,  \
                         ICALC_index_lab_CB,(XtPointer)iwid  ) ;     \
     ICALC_userdata(iwid->war[aa].index_av->wrowcol,aa+1) ;          \
     VLINEE(iwid->war[aa].rc) ;                                      \
     iwid->war[aa].chooser_lab =                                     \
        XtVaCreateManagedWidget(                                     \
            ff     , xmLabelWidgetClass , rc ,                       \
            LABEL_ARG(ICALC_nothing_chosen) ,                        \
            XmNalignment , XmALIGNMENT_BEGINNING ,                   \
            XmNrecomputeSize , False ,  XmNtraversalOn , True ,      \
            XmNinitialResourcesPersistent , False , NULL ) ;         \
     VLINE(iwid->war[aa].rc) ;                                       \
     iwid->war[aa].string_lab =                                      \
        XtVaCreateManagedWidget(                                     \
            ff     , xmLabelWidgetClass , rc ,                       \
            LABEL_ARG(ICALC_labelstr[0]) ,                           \
            XmNalignment , XmALIGNMENT_BEGINNING ,                   \
            XmNrecomputeSize , False ,  XmNtraversalOn , True ,      \
            XmNinitialResourcesPersistent , False , NULL ) ;         \
     iwid->war[aa].string_text =                                     \
        XtVaCreateManagedWidget(                                     \
            ff     , xmTextFieldWidgetClass , rc ,                   \
              XmNvalue        , "\0" ,                               \
              XmNcolumns      , 12 ,                                 \
              XmNeditable     , True ,                               \
              XmNmaxLength    , 64 ,                                 \
              XmNresizeWidth  , False ,                              \
              XmNmarginHeight , 1 ,                                  \
              XmNmarginWidth  , 1 ,                                  \
              XmNcursorPositionVisible , True ,                      \
              XmNblinkRate , 0 ,                                     \
              XmNautoShowCursorPosition , True ,                     \
              XmNtraversalOn , True  ,                               \
              XmNinitialResourcesPersistent , False ,                \
            NULL ) ;                                                 \
     if( !ee ){                                                      \
       MCW_set_widget_bg(iwid->war[aa].menu_av->wrowcol ,"black",0); \
       MCW_set_widget_bg(iwid->war[aa].index_av->wrowcol,"black",0); \
       MCW_set_widget_bg(iwid->war[aa].tog_bbox->wbut[0],"black",0); \
       MCW_set_widget_bg(iwid->war[aa].string_lab       ,"black",0); \
       MCW_set_widget_bg(iwid->war[aa].chooser_pb       ,"black",0); \
       MCW_set_widget_bg(iwid->war[aa].chooser_lab      ,"black",0); \
     }                                                               \
     XtManageChild(rc) ; ICALC_toggle_row(iwid->war[aa],False) ;     \
  } while(0)

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

void ICALC_make_widgets( Three_D_View *im3d )
{
   ICALC_widget_set *iwid ;
   ICALC_setup      *ics ;
   Widget ww , rc , shtop , swtop ;
   XmString xstr ;
   char str[32] , *eee , *cpt ;
   int ii ;

ENTRY("ICALC_make_widgets") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ; /* bad */

   cpt =  AFNI_controller_label(im3d) ; sprintf(str,"AFNI InstCalc %s",cpt) ;

   if( im3d->vwid->func->iwid != NULL ){
     iwid = im3d->vwid->func->iwid ;
     XtMapWidget(iwid->wtop) ; iwid->is_open = 1 ;
     WAIT_for_window( iwid->wtop ) ;
     XRaiseWindow( XtDisplay(iwid->wtop) , XtWindow(iwid->wtop) ) ;
   }

   im3d->vwid->func->iwid = iwid = myXtNew( ICALC_widget_set ) ;
   for( ii=0 ; ii < 26 ; ii++ ) iwid->var[ii] = NULL ;

   if( im3d->icalc_setup == NULL ){
     INIT_ICALC_setup(im3d->icalc_setup) ;
     im3d->icalc_setup->prefix = (char *)malloc(sizeof(char)*16) ;
     sprintf(im3d->icalc_setup->prefix,"%c_ICALC",cpt[1]) ;
   }

   ics = im3d->icalc_setup ;

   iwid->wtop = XtVaAppCreateShell(
                  "AFNI" , "AFNI" ,
                   topLevelShellWidgetClass , im3d->dc->display ,
                   XmNallowShellResize   , True ,
                   XmNtitle              , str ,
                   XmNmappedWhenManaged  , False ,        /* manage manually */
                   XmNdeleteResponse     , XmDO_NOTHING , /* deletion handled below */
                   XmNkeyboardFocusPolicy , XmEXPLICIT ,
                NULL ) ;
   DC_yokify( iwid->wtop , im3d->dc ) ;
   XmAddWMProtocolCallback(           /* make "Close" window menu work */
           iwid->wtop ,
           XmInternAtom( im3d->dc->display , "WM_DELETE_WINDOW" , False ) ,
           ICALC_quit_CB , (XtPointer)iwid ) ;

   swtop = shtop = XtVaCreateManagedWidget(
                 "menu" , xmScrolledWindowWidgetClass , iwid->wtop ,
                    XmNscrollingPolicy        , XmAUTOMATIC ,
                    XmNvisualPolicy           , XmVARIABLE ,
                    XmNscrollBarDisplayPolicy , XmAS_NEEDED /* XmSTATIC */ ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;

   /* vertical rowcol to hold it all */

   iwid->rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , shtop ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNspacing      , 0 ,
            XmNadjustMargin , True ,
            XmNtraversalOn  , True ,
         NULL ) ;

   /* action buttons at top */

   for( ii=0 ; ii < ICALC_NUMACT ; ii++ ) ICALC_act[ii].data = (XtPointer)iwid ;

   iwid->actar = MCW_action_area( iwid->rowcol , ICALC_act , ICALC_NUMACT ) ;

   HLINE(iwid->rowcol) ; HLINE(iwid->rowcol) ;

   /* horizontal rowcol for OLay widgets */

   rc = XtVaCreateWidget(
          "menu" , xmRowColumnWidgetClass , iwid->rowcol ,
             XmNpacking      , XmPACK_TIGHT ,
             XmNorientation  , XmHORIZONTAL ,
             XmNadjustMargin , True ,
             XmNtraversalOn  , True ,
          NULL ) ;

   /* OLay label */

   (void)XtVaCreateManagedWidget(
            "bigtext"   , xmLabelWidgetClass , rc ,
            LABEL_ARG("OLay Expr") ,
            XmNalignment , XmALIGNMENT_BEGINNING ,
            XmNrecomputeSize , False ,  XmNtraversalOn , True ,
            XmNinitialResourcesPersistent , False , NULL ) ;

   /* OLay expression text */

   iwid->olay_expr_text = XtVaCreateManagedWidget(
                           "bigtext"  , xmTextFieldWidgetClass , rc ,
                             XmNvalue        , "\0" ,
                             XmNcolumns      , 68 ,
                             XmNeditable     , True ,
                             XmNmaxLength    , 248 ,
                             XmNresizeWidth  , False ,
                             XmNmarginHeight , 1 ,
                             XmNmarginWidth  , 1 ,
                             XmNcursorPositionVisible , True ,
                             XmNblinkRate , 0 ,
                             XmNautoShowCursorPosition , True ,
                             XmNtraversalOn , True  ,
                             XmNinitialResourcesPersistent , False ,
                           NULL ) ;
   XtManageChild(rc) ;
   HLINE(iwid->rowcol) ; HLINE(iwid->rowcol) ;

   /** rows of widgets to control variables **/

   for( ii=0 ; ii < 26 ; ii++ ) MAKE_ICALC_ROW(ii) ;

   XtManageChild(iwid->rowcol) ;

   if( swtop != NULL ){
     int wx,hy , cmax ;
     MCW_widget_geom( iwid->rowcol  , &wx,&hy,NULL,NULL ) ;
     cmax = im3d->dc->height-128 ; if( hy > cmax ) hy = cmax ;
     XtVaSetValues( iwid->wtop , XmNwidth,wx+6,XmNheight,hy+6 , NULL ) ;
   }

   XtRealizeWidget( iwid->wtop ) ;
   WAIT_for_window( iwid->wtop ) ;
   NORMAL_cursorize( iwid->rowcol ) ;
   iwid->im3d = im3d ; iwid->is_open = 1 ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static int ICALC_find_index( ICALC_widget_set *iwid , Widget ww )
{
   int aa ; XtPointer pp=NULL ;
   XtVaGetValues( ww , XmNuserData , &pp , NULL ) ;
   if( pp == NULL ) return -1 ;
   aa = PTOI(pp) ; return (aa-1) ;
}

/*----------------------------------------------------------------------------*/

static void ICALC_tog_bbox_CB( Widget w, XtPointer cd, XtPointer cbs )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   Three_D_View     *im3d = iwid->im3d ;
   ICALC_setup      *ics  = im3d->icalc_setup ;
   int                 aa = ICALC_find_index(iwid,w) ; Boolean bb ;

   if( aa < 0 ) return ;
   bb = MCW_val_bbox(iwid->war[aa].tog_bbox) ;
   ICALC_toggle_row(iwid->war[aa],bb) ;
   return ;
}

/*----------------------------------------------------------------------------*/

static void ICALC_menu_av_CB( MCW_arrowval *av , XtPointer cd )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   Three_D_View     *im3d = iwid->im3d ;
   ICALC_setup      *ics  = im3d->icalc_setup ;
   int                 aa = ICALC_find_index(iwid,av->wrowcol) ;
   int                 bb = av->ival ;

   if( aa < 0 || bb < 0 || bb >= ICALC_NUMTYPE ) return ;
   MCW_set_widget_label( iwid->war[aa].chooser_pb , ICALC_choosestr[bb]  ) ;
   MCW_set_widget_label( iwid->war[aa].chooser_lab, ICALC_nothing_chosen ) ;
   MCW_set_widget_label( iwid->war[aa].string_lab , ICALC_labelstr[bb]   ) ;
   XmTextFieldSetString( iwid->war[aa].string_text, "\0"                 ) ;

   ICALC_toggle_row(iwid->war[aa],True) ;
   ics->inset[aa] = NULL ;
   return ;
}

/*----------------------------------------------------------------------------*/

static PLUGIN_dsetval *dsv = NULL ;
static int             dsa = -1 ;

static void ICALC_chooser_CB( Widget w, XtPointer cd, XtPointer cbs )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   Three_D_View     *im3d = iwid->im3d ;
   ICALC_setup      *ics  = im3d->icalc_setup ;
   int                 aa = ICALC_find_index(iwid,w) , bb ;

   if( aa < 0 ) return ;
   bb = iwid->war[aa].menu_av->ival ;
   if( bb < 0 || bb >= ICALC_NUMTYPE ) return ;

   POPDOWN_strlist_chooser ;  /* death to the old regime */
   dsa = -1 ;

   switch( bb ){
     case ICALC_DSET_VALUE:
     case ICALC_DSET_STAT:  ICALC_choose_dataset( iwid , aa ) ; break ;

     case ICALC_CONSTANT:   break ;  /* nothing to do here */
   }
}

/*----------------------------------------------------------------------------*/

static void ICALC_quit_CB( Widget w, XtPointer cd, XtPointer cbs )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   Three_D_View     *im3d = iwid->im3d ;
   ICALC_setup      *ics  = im3d->icalc_setup ;
   XtUnmapWidget(iwid->wtop) ; iwid->is_open = 0 ;
   DISABLE_INSTACALC(iwid->im3d) ;
   return ;
}

/*----------------------------------------------------------------------------*/

static void ICALC_compute_CB( Widget w, XtPointer cd, XtPointer cbs )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   Three_D_View     *im3d = iwid->im3d ;
   ICALC_setup      *ics  = im3d->icalc_setup ;
   THD_slist_find    slf ;
   MRI_IMAGE        *iim ;
   float            *iar ;
   THD_3dim_dataset *icaset ;
   int               nds ;

ENTRY("ICALC_compute_CB") ;

   DEBUG = AFNI_yesenv("AFNI_INSTACALC_DEBUG") ;

if(DEBUG) INFO_message("call ICALC_finalize_setup") ;

   INSTACALC_LABEL_OFF(im3d) ;
   ICALC_finalize_setup( iwid , ics ) ;
   if( ics->is_good == 0 ) EXRETURN ;

if(DEBUG) INFO_message("call ICALC_compute") ;

   iim = ICALC_compute(ics) ;
   if( iim == NULL ) EXRETURN ;

if(DEBUG) INFO_message("add dataset") ;

   /* find the output dataset */

   slf = THD_dset_in_session( FIND_PREFIX , ics->prefix , im3d->ss_now ) ;

if(DEBUG) ININFO_message("find completed") ;

   /* if it doesn't exist, or is not the right grid, create it now */

   if( !ISVALID_DSET (slf.dset) ||
       !EQUIV_DATAXES(slf.dset->daxes,ics->dset_master->daxes) ){

if(DEBUG) ININFO_message("create new dataset") ;
     icaset = EDIT_empty_copy( ics->dset_master ) ;  /* make new dataset */
     EDIT_dset_items( icaset ,
                        ADN_prefix    , ics->prefix ,
                        ADN_nvals     , 1 ,
                        ADN_ntt       , 0 ,
                        ADN_func_type , FUNC_BUCK_TYPE ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                        ADN_datum_all , MRI_float ,
                      ADN_none ) ;
     DSET_superlock(icaset) ;

     if( slf.dset != NULL ){       /* exists, but isn't right for us */

       MCW_idcode old_idc = slf.dset->idcode ;
if(DEBUG) ININFO_message("hollowing out old dataset") ;
       THD_delete_3dim_dataset(slf.dset,True) ;  /* destroy the guts */
       *slf.dset = *icaset ;      /* copy the guts, keep the pointer */
       slf.dset->idcode = old_idc ;           /* and keep the idcode */
       nds = slf.dset_index ;
       INFO_message("trashed and re-used old dataset %s",ics->prefix) ;

     } else {                                  /* add to the session */
       int vv = icaset->view_type ;
       nds = im3d->ss_now->num_dsset ;
if(DEBUG) ININFO_message("adding to session: nds=%d",nds) ;
       im3d->ss_now->dsset[nds][vv] = icaset ;
       im3d->ss_now->num_dsset++ ;
if(DEBUG) ININFO_message("force adoption") ;
       AFNI_force_adoption( im3d->ss_now , False ) ;
if(DEBUG) ININFO_message("make descendants") ;
       AFNI_make_descendants( GLOBAL_library.sslist ) ;
       INFO_message("created new dataset %s",ics->prefix) ;
     }

   /* just need to use existing dataset that matches */

   } else {

if(DEBUG) ININFO_message("recycling old dataset") ;
     icaset = slf.dset ; nds = slf.dset_index ;

   }
   icaset->dblk->diskptr->allow_directwrite = 1 ;

   /* save the result into the output dataset */

if(DEBUG) ININFO_message("storing results into dataset: %d %d %d",iim->nx,iim->ny,iim->nz) ;
   iar = MRI_FLOAT_PTR(iim) ;
   EDIT_substitute_brick( icaset , 0 , MRI_float , iar ) ;
   mri_clear_data_pointer(iim) ; mri_free(iim) ;
if(DEBUG) ININFO_message("dataset statistics") ;
   DSET_KILL_STATS(icaset) ; THD_load_statistics(icaset) ;

if(DEBUG) ININFO_message("dataset label") ;
   EDIT_BRICK_LABEL(icaset,0,"InstaCalc") ;

   /* redisplay overlay */

   if( im3d->fim_now != icaset ){  /* switch to this dataset */
     MCW_choose_cbs cbs ; char cmd[32] , *cpt=AFNI_controller_label(im3d) ;
if(DEBUG) ININFO_message("change fim_now") ;
     cbs.ival = nds ;
     AFNI_finalize_dataset_CB( im3d->vwid->view->choose_func_pb ,
                               (XtPointer)im3d ,  &cbs           ) ;
     AFNI_set_fim_index(im3d,0) ;
     AFNI_set_thr_index(im3d,0) ;
   }
if(DEBUG) ININFO_message("reset func range") ;
   AFNI_reset_func_range(im3d) ;

   if( MCW_val_bbox(im3d->vwid->view->see_func_bbox) == 0 ){ /* overlay is off */
     char cmd[32] , *cpt=AFNI_controller_label(im3d) ;
     sprintf(cmd,"SEE_OVERLAY %c.+",cpt[1]) ;
if(DEBUG) ININFO_message("turn overlay on") ;
     AFNI_driver(cmd) ;
   } else {                                                  /* overlay is on */
if(DEBUG) ININFO_message("redisplay overlay") ;
     AFNI_redisplay_func(im3d) ;
   }
   INSTACALC_LABEL_ON(im3d) ;
   AFNI_process_drawnotice(im3d) ;

if(DEBUG) ININFO_message("done") ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static char *helpstring =
   "\n"
   "============================== AFNI InstaCalc ==============================\n"
   "\n"
   "This module lets you compute a 3D volume from 1 or more other datasets,\n"
   "much like 3dcalc does, but on-the-fly inside AFNI.\n"
   "\n"
   " (1) Enter the 3dcalc-like expression in the 'OLay Expr' text field.\n"
   "\n"
   " (2) Select datasets to corresponds to symbols A, B, ..., as desired,\n"
   "     and then select a sub-brick index '[-]' to use.\n"
   "*OR* Choose a statistic (MIN or MAX) to use from a given dataset volume.\n"
   "*OR* Set a given symbol to be a numeric constant.\n"
   "\n"
   " (3) Press the 'Compute InstaCalc' button to (re)calculate the output.\n"
   "\n"
   " * The output is stored into a dataset named A_ICALC (for controller A),\n"
   "   which will be switched to be the new overlay dataset.\n"
   " * If you make changes to the expression, the datasets chosen, etc., the\n"
   "   A_ICALC will NOT be recomputed automatically -- you must press the\n"
   "   'Compute InstaCalc' button again.\n"
   "\n"
   "If you make changes to the expression, or to the symbol definitions, you\n"
   "must press 'Compute InstaCalc' again for the changes to be reflected in\n"
   "the dataset.\n"
   "\n"
   "Simple Example: Dual Thresholding\n"
   "   A = Overlay sub-brick of interest\n"
   "   B = t-statistic for some effect\n"
   "   C = t-statistic for some other effect\n"
   "   Expression = 'A*astep(B,3.1)*astep(C,4.2)\n"
   "The effect is to colorize A only where abs(B) > 3.1 AND abs(C) > 4.2.\n"
   "\n"
   "Advanced Usage:\n"
   " * diffsub = as in 3dcalc, you can specify a differentially offset voxel,\n"
   "             which allows a limited amount of spatial processing.\n"
   "             Example:\n"
   "               A = dataset\n"
   "               B = instead of choose dataset, use 'A+I' for diffsub\n"
   "               C = use 'A-I' for diffsub\n"
   "               D,E,F,G = use 'A+I', 'A-J', 'A+K', 'A-K' for diffsub\n"
   "               Expression = '6*A-B-C-D-E-F-G' = 3D Laplacian = edge detector\n"
   " * Index   = choosing 'Index' as the sub-brick '[-]' means that the value\n"
   "             of the 'Index' time selector in the main AFNI controller window\n"
   "             will be used.  When you change the time 'Index' in AFNI,\n"
   "             InstaCalc will NOT be automatically updated -- you still have\n"
   "             to press 'Compute InstaCalc' to get A_ICALC re-computed.\n"
   "\n"
   "Author -- RW Cox -- Sep 2009\n"
   "\n"
   "DEBUGGING:\n"
   "  This feature is very new.  If you can make it realiably crash, please\n"
   "  re-run AFNI with the extra option '-DAFNI_INSTACALC_DEBUG=YES' and\n"
   "  then make InstaCalc crash; the screen printout of the processing steps\n"
   "  might help pinpoint the problem when you report it to me.\n"
   "\n"
   "============================================================================\n"
   "============ Description of Expression Syntax and Functionality ============\n"
   "\n"
   PARSER_HELP_STRING ;

static void ICALC_help_CB( Widget w, XtPointer cd, XtPointer cbs )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   if( iwid == NULL || iwid->is_open == 0 ) return ;
   (void)new_MCW_textwin( iwid->olay_expr_text , helpstring , TEXT_READONLY ) ;
   return ;
}

/*----------------------------------------------------------------------------*/

static void ICALC_choose_dataset( ICALC_widget_set *iwid , int aa )
{
   Three_D_View *im3d = iwid->im3d ;
   ICALC_setup  *ics  = im3d->icalc_setup ;
   THD_session *ss ;
   THD_3dim_dataset *dset ;
   int iss_bot , iss_top , iss , vv , kk ;
   int id , num_dset , qd ;
   MCW_idcode old_idcode ;
   char label[64] ;
   static char **strlist = NULL ;

ENTRY("ICALC_choose_dataset") ;

   iss_bot = iss_top = im3d->vinfo->sess_num ;  /* sessions */
   vv      = im3d->vinfo->view_type ;           /* view type */

   if( dsv != NULL ) myXtFree(dsv) ;
   dsv = myXtNew(PLUGIN_dsetval) ; dsa = aa ;

   dsv->dset_count  = 0 ;     /* will be array of datasets */
   dsv->dset_link   = NULL ;  /* we can choose amongst */
   dsv->dset_choice = -1 ;    /* will be index of our choice */
   dsv->multi       = 0 ;
   dsv->nchosen     = 0 ;
   dsv->chosen      = NULL ;
   dsv->current     = 0 ;
   dsv->idclist     = NULL ;

   /** Scan sessions **/

   num_dset = 0 ;
   for( iss=iss_bot ; iss <= iss_top ; iss++ ){
      ss = GLOBAL_library.sslist->ssar[iss] ;

      /* check datasets in this session */

      for( id=0 ; id < ss->num_dsset ; id++ ){
        dset = ss->dsset[id][vv] ; if( !ISVALID_DSET(dset)  ) continue ;
                                   if( !DSET_INMEMORY(dset) ) continue ;
           if( strncmp(DSET_PREFIX(dset)+1,"_ICALC",6) == 0 ) continue ;
        kk = DSET_BRICK_TYPE(dset,0); if( !IS_REAL_TYPE(kk) ) continue ;

        /* if we get here, then this dataset is OK to choose! */

        num_dset++ ;
        dsv->dset_link = (PLUGIN_dataset_link *)
                            XtRealloc( (char *) dsv->dset_link ,
                                       sizeof(PLUGIN_dataset_link)*num_dset ) ;

        make_PLUGIN_dataset_link( dset , dsv->dset_link + (num_dset-1) ) ;
      }
   } /* end of loop over sessions */

   if( num_dset == 0 ){ myXtFree(dsv) ; BEEPIT ; EXRETURN ; }

   dsv->dset_count = num_dset ;

   POPDOWN_strlist_chooser ;  /* death to the old regime */

   /* fix the dataset titles to be more fun */

   patch_PLUGIN_dataset_links( num_dset , dsv->dset_link ) ;

   strlist = (char **) XtRealloc( (char *)strlist , sizeof(char *)*num_dset ) ;
   for( id=0 ; id < num_dset ; id++ ) strlist[id] = dsv->dset_link[id].title ;

   sprintf( label , "AFNI Dataset from the %s" , VIEW_typestr[vv] ) ;

   MCW_choose_strlist( iwid->war[aa].chooser_pb , label ,
                       num_dset , dsv->dset_choice , strlist ,
                       ICALC_finalize_dataset_CB , (XtPointer)iwid ) ;

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static void ICALC_finalize_dataset_CB( Widget w, XtPointer fd, MCW_choose_cbs *cbs )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)fd ;
   Three_D_View     *im3d = iwid->im3d ;
   ICALC_setup      *ics  = im3d->icalc_setup ;
   int id = cbs->ival ;
   THD_3dim_dataset *dset ;
   int inival , topval ;

ENTRY("ICALC_finalize_dataset_CB") ;

   if( iwid->is_open == 0 ){
     POPDOWN_strlist_chooser ; dsa = -1 ; BEEPIT ; EXRETURN ;
   }

   if( dsv == NULL || id < 0 || id >= dsv->dset_count || dsa < 0 ){ BEEPIT; EXRETURN; }
   dset = PLUTO_find_dset( &(dsv->dset_link[id].idcode) ) ;
   if( !ISVALID_DSET(dset) )                                      { BEEPIT; EXRETURN; }
   MCW_set_widget_label( iwid->war[dsa].chooser_lab ,
                         dset->dblk->diskptr->filecode ) ;

   ics->inset[dsa] = dset ;
   inival = iwid->war[dsa].index_av->ival ;
   topval = DSET_NVALS(dset) - 1 ;
   if( inival > topval ) inival = topval ;
   refit_MCW_optmenu( iwid->war[dsa].index_av ,
                      -1 , topval , inival, 0 ,
                      ICALC_index_lab_CB,(XtPointer)iwid  ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------------*/

static void ICALC_index_av_CB( MCW_arrowval *av , XtPointer cd )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   Three_D_View     *im3d = iwid->im3d ;
   ICALC_setup      *ics  = im3d->icalc_setup ;
   int                 aa = ICALC_find_index(iwid,av->wrowcol) ;
   int                 bb = av->ival ;

   ics->inidx[aa] = bb ;
   return ;
}

/*----------------------------------------------------------------------------*/

static char * ICALC_index_lab_CB( MCW_arrowval *av , XtPointer cd )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   Three_D_View     *im3d = iwid->im3d ;
   ICALC_setup      *ics  = im3d->icalc_setup ;
   int                 aa = ICALC_find_index(iwid,av->wrowcol) ;
   int                 bb = av->ival ;
   static char        str[16] ;

   if( bb < 0 ) strcpy(str,"Index") ;
   else         sprintf(str," %d",bb) ;
   return str ;
}

/*============================================================================*/
/*------------------------ the actual computations ---------------------------*/
/*============================================================================*/

#define HAS_I(q)  (q)->has_sym[ 8]
#define HAS_J(q)  (q)->has_sym[ 9]
#define HAS_K(q)  (q)->has_sym[10]
#define HAS_L(q)  (q)->has_sym[11]
#define HAS_T(q)  (q)->has_sym[19]
#define HAS_X(q)  (q)->has_sym[23]
#define HAS_Y(q)  (q)->has_sym[24]
#define HAS_Z(q)  (q)->has_sym[25]

#define PREDEFINED_MASK ((1<< 8)|(1<< 9)|(1<<10)|(1<<11)| \
                         (1<<19)|(1<<23)|(1<<24)|(1<<25) )

#define Rfac  0.299f  /* for RGB input datasets */
#define Gfac  0.587f
#define Bfac  0.114f

#define VSIZE 1024    /* vector size for PARSER computations */

#define FIRSTMESS                                                                         \
 do{ if( first ){                                                                         \
       INFO_message("-------------- InstaCalc setup messages --------------"); first=0;   \
       errbuf=THD_zzprintf(errbuf,                                                        \
                    "-------------- InstaCalc Setup Error Messages --------------\n\n") ; \
 }} while(0)

/*----------------------------------------------------------------------------*/
/*! Finalize the InstaCalc setup, if possible.
    Returns 1 if the setup is OK, 0 if it is not.
*//*--------------------------------------------------------------------------*/

static void ICALC_finalize_setup( ICALC_widget_set *iwid , ICALC_setup *ics )
{
   PARSER_code *olay_pcode ;
   char *str , *errbuf=NULL ;
   int ids,jds , bb , first=1 , nbad=0 , nxyz ;
   THD_3dim_dataset *dset ;

ENTRY("ICALC_finalize_setup") ;

if(DEBUG) INFO_message("----- finalizing setup -------------------------") ;
   if( iwid == NULL || !iwid->is_open || ics == NULL ){ BEEPIT; EXRETURN; }

   ics->is_good = 0 ;

   /*- get and parse the expression -*/

if(DEBUG) ININFO_message("get expression") ;
   str = XmTextFieldGetString( iwid->olay_expr_text ) ;
   if( str == NULL || *str == '\0' ){ BEEPIT; EXRETURN; }

   if( ics->olay_expr != NULL ) free(ics->olay_expr) ;
   ics->olay_expr = strdup(str) ;

if(DEBUG) ININFO_message("parse expression") ;
   PARSER_set_printout(1) ;
   olay_pcode = PARSER_generate_code(ics->olay_expr) ;
   PARSER_set_printout(0) ;

   if( olay_pcode == NULL ){
     (void)MCW_popup_message( iwid->olay_expr_text , "Invalid expression" ,
                              MCW_USER_KILL | MCW_TIMER_KILL ) ;
     free(ics->olay_expr) ; ics->olay_expr = NULL ; BEEPIT ; EXRETURN ;
   }

if(DEBUG) ININFO_message("mark symbols") ;
   ics->olay_pcode = (void *)olay_pcode ;
   PARSER_mark_symbols( olay_pcode , ics->has_sym ) ;  /* what symbols are used? */

   /*------------- process each row of widgets [symbols] -------------*/

   ics->dset_master = NULL ;
   ics->has_predefined = ics->has_xyz = ics->mangle_xyz = 0 ;

   for( ids=0 ; ids < 26 ; ids++ ){

     ics->dshift[ids] = -1 ;

     bb = MCW_val_bbox(iwid->war[ids].tog_bbox) ;  /* is this row on? */

     if( !bb ){                          /** off ==> mark as undefined **/
       ics->intyp[ids] = ICALC_INVALID ;
       ics->inval[ids] = 0.0 ;
       if( ics->has_sym[ids] ){
         if( ((1<<ids) & PREDEFINED_MASK) == 0 ){
           FIRSTMESS ; nbad++ ;
           ERROR_message("InstaCalc uses undefined symbol '%c'\n",abet[ids]) ;
           errbuf = THD_zzprintf(errbuf,"InstaCalc uses undefined symbol '%c'\n",abet[ids]) ;
         } else {
           ics->has_predefined++ ; if( ids >= 23 ) ics->has_xyz++ ;
           ics->inset[ids] = NULL ; ics->intyp[ids] = ICALC_DSET_VALUE ;
if(DEBUG) ININFO_message("using predefined symbol '%c'",abet[ids]) ;
         }
       }
       continue ;  /* skip to next symbol */
     }

if(DEBUG) ININFO_message("** process row %c",abet[ids]) ;

     /**--- OK, this symbol is defined, so figure out what to do with it ---**/

     if( !ics->has_sym[ids] ){
       ics->intyp[ids] = ICALC_INVALID ;
       FIRSTMESS ;
       WARNING_message("InstaCalc defines symbol '%c' but doesn't use it",abet[ids]) ;
       continue ;  /* skip to next symbol */
     }

     /** type of value associated with this symbol **/

     ics->intyp[ids] = bb = iwid->war[ids].menu_av->ival ;

     switch( bb ){

       /* this case is REAL easy */

       case ICALC_CONSTANT:
if(DEBUG) ININFO_message("  get constant string") ;
         str = XmTextFieldGetString( iwid->war[ids].string_text) ;
         ics->inval[ids] = strtod(str,NULL) ;
       break ;

       /* these cases aren't so easy */

       case ICALC_DSET_VALUE:
       case ICALC_DSET_STAT:{
         int idx      = iwid->war[ids].index_av->ival ;
         char *difstr = XmTextFieldGetString( iwid->war[ids].string_text) ;
if(DEBUG) ININFO_message("  get dataset pointer") ;
         dset = ics->inset[ids] ; jds = ids ;
         if( !ISVALID_DSET(dset) ){
           if( difstr != NULL && isalpha(difstr[0]) ){  /* allow diffsub like 'a[0,3,1,0]' */
             jds = toupper(difstr[0]) - 'A' ;
if(DEBUG) ININFO_message("    diffsub dataset of '%c' is '%c'",abet[ids],abet[jds]) ;
             dset = ics->inset[jds] ; idx = iwid->war[jds].index_av->ival ; difstr++ ;
           }
           if( !ISVALID_DSET(dset) ){
             FIRSTMESS ; nbad++ ;
             ERROR_message("Symbol '%c' has undefined dataset\n",abet[ids]) ;
             errbuf = THD_zzprintf(errbuf,"Symbol '%c' has undefined dataset\n",abet[ids]) ;
             goto DSET_DONE ;
           }
         }

         if( idx < 0                 ) idx = iwid->im3d->vinfo->time_index ;
         if( idx >= DSET_NVALS(dset) ) idx = DSET_NVALS(dset)-1 ;
         ics->inidx[ids] = idx ;
if(DEBUG) ININFO_message("  dataset idx = %d",idx) ;

         if( bb == ICALC_DSET_VALUE ){  /*---- actual dataset voxel values ----*/

           if( ics->dset_master == NULL ){
if(DEBUG) ININFO_message("  set dset_master") ;
             ics->dset_master = dset ;
           }

           /* differential subscript? */

if(DEBUG) ININFO_message("  check diffsub") ;
           if( PLUG_nonblank_len(difstr) > 0 ){
             int *ijkl ; int ist ;
if(DEBUG) ININFO_message("  diffsub = %s",difstr) ;
             for( ist=0 ; isspace(ist) ; ist++ ) ; /*skip blanks*/
             if( difstr[ist] == '[' ){
               MCW_intlist_allow_negative(1) ;
               ijkl = MCW_get_intlist( 9999 , difstr+ist ) ;
               MCW_intlist_allow_negative(0) ;
               if( ijkl == NULL || ijkl[0] <= 0 ){
                 FIRSTMESS ; if( ijkl != NULL ) free(ijkl) ;
                 ERROR_message("Bad [..] diffsub for symbol '%c'\n",abet[ids]) ;
                 errbuf = THD_zzprintf(errbuf,"Bad [..] diffsub for symbol '%c'\n",abet[ids]) ;
                 nbad++ ; goto DSET_DONE ;
               }
             } else {
               ijkl = (int *) malloc( sizeof(int) * 5 ) ;
               ijkl[1] = ijkl[2] = ijkl[3] = ijkl[4] = 0 ; ijkl[0] = 4 ; /* initialize */
               switch( toupper(difstr[ist+1]) ){
                 default: FIRSTMESS ; nbad++ ;
                          ERROR_message(
                            "Bad diffsub for symbol '%c': expected +/- i/j/k/l; got %s\n",abet[ids],difstr+ist) ;
                          errbuf=THD_zzprintf(errbuf,
                            "Bad diffsub for symbol '%c': expected +/- i/j/k/l; got %s\n",abet[ids],difstr+ist) ;
                 goto DSET_DONE ;

                 case 'I': ijkl[1] = (difstr[ist]=='+') ? 1 : -1 ; break ;
                 case 'J': ijkl[2] = (difstr[ist]=='+') ? 1 : -1 ; break ;
                 case 'K': ijkl[3] = (difstr[ist]=='+') ? 1 : -1 ; break ;
                 case 'L': ijkl[4] = (difstr[ist]=='+') ? 1 : -1 ; break ;
               }
             }
             if( ijkl[1]==0 && ijkl[2]==0 && ijkl[3]==0 && ijkl[4]==0 ){
               FIRSTMESS ; free(ijkl) ;
               WARNING_message("diffsub for symbol '%c' is all zero -- ignoring",abet[ids]) ;
               goto DSET_DONE ;
             }
             ics->dshift  [ids] = jds ;
             ics->dshift_i[ids] = (ijkl[0] >= 1) ? ijkl[1] : 0 ;
             ics->dshift_j[ids] = (ijkl[0] >= 2) ? ijkl[2] : 0 ;
             ics->dshift_k[ids] = (ijkl[0] >= 3) ? ijkl[3] : 0 ;
             ics->dshift_l[ids] = (ijkl[0] >= 4) ? ijkl[4] : 0 ; free(ijkl) ;
           } /* end of diffsub-ization */
if(DEBUG) ININFO_message("  dataset finished") ;

         } else {  /*---------- statistics of dataset brick values ----------*/
           int ist ;

if(DEBUG) ININFO_message("  dataset statistic") ;
           if( !DSET_VALID_BSTAT(dset,idx) ) THD_load_statistics(dset) ;

if(DEBUG) ININFO_message("  get string") ;
           str = XmTextFieldGetString( iwid->war[ids].string_text) ;
           if( PLUG_nonblank_len(str) <= 0 ){
             FIRSTMESS ;
             ERROR_message("No statistic string entered for symbol '%c'\n",abet[ids]) ;
             errbuf=THD_zzprintf(errbuf,"No statistic string entered for symbol '%c'\n",abet[ids]) ;
             nbad++ ; goto DSET_DONE ;
           }
if(DEBUG) ININFO_message("  string = %s",str) ;
           for( ist=0 ; isspace(ist) ; ist++ ) ; /*skip blanks*/
           if( strncasecmp(str+ist,"max",3) == 0 ){
             ics->inval[ids] = dset->stats->bstat[idx].max ;
           } else if( strncasecmp(str+ist,"min",3) == 0 ){
             ics->inval[ids] = dset->stats->bstat[idx].min ;
           } else {
             FIRSTMESS ;
             ERROR_message("Unknown statistic '%s' entered for symbol '%c'\n",str,abet[ids]) ;
             errbuf=THD_zzprintf(errbuf,"Unknown statistic '%s' entered for symbol '%c'\n",str,abet[ids]) ;
             nbad++ ; goto DSET_DONE ;
           }
         } /* end of dataset statisick-ization */

if(DEBUG) ININFO_message("  completely done with dataset processing") ;
       DSET_DONE: /*nada*/ ;
       } /* end of processing the 2 dataset cases */
       break ;

     } /* end of switch on symbol type */
   } /* end of loop over symbols */

   /*-- if no master dataset set (all constants or predefined values) --*/

   if( ics->dset_master == NULL ){
     ics->dset_master = iwid->im3d->anat_now ;
     FIRSTMESS ;
     INFO_message("no 'Dataset: Value' chosen in symbol list") ;
   }

   /*-- check all datasets against the master for compatibility --*/

if(DEBUG) ININFO_message("check datasets for compatibility") ;
   nxyz = DSET_NVOX(ics->dset_master) ;
   for( ids=0 ; ids < 26 ; ids++ ){
     if( ics->intyp[ids] != ICALC_DSET_VALUE ) continue ;
     dset = ics->inset[ids] ;
     if( !ISVALID_DSET(dset) || dset == ics->dset_master ) continue ;
     if( DSET_NVOX(dset) != nxyz ){
       FIRSTMESS ;
       ERROR_message("Dataset '%c' does not grid-match all others in InstaCalc\n",abet[ids]) ;
       errbuf=THD_zzprintf(errbuf,"Dataset '%c' does not grid-match all others in InstaCalc\n",abet[ids]) ;
       nbad++ ;
     }
   }

   if( nbad > 0 ){
     ERROR_message("----- Cannot continue past the above error%s -----",
                   (nbad==1) ? "\0" : "s" ) ;
     errbuf=THD_zzprintf(errbuf,
              "\n----------- Cannot continue past the above error%s ----------\n ",
                   (nbad==1) ? "\0" : "s" ) ;
     (void)MCW_popup_message( iwid->olay_expr_text , errbuf ,
                              MCW_USER_KILL | MCW_TIMER_KILL ) ;
     BEEPIT ; EXRETURN ;
   }

if(DEBUG) ININFO_message("finalized!") ;
   if( errbuf != NULL ) free(errbuf) ;
   ics->is_good = 1 ; EXRETURN ;
}

#define CEBUG (DEBUG && ii==0)

/*----------------------------------------------------------------------------*/
/*! Compute a float-valued volume from the InstaCalc setup.
*//*--------------------------------------------------------------------------*/

static MRI_IMAGE * ICALC_compute( ICALC_setup *ics )
{
   double  temp[VSIZE] ;  /* output vector for computations */
   double *atoz[26] ;     /* input vector for computations */
   int ii , ids , jj, kk, ll, jbot, jtop , nbad ;
   float  *buf; MRI_IMAGE *bim ; double val ;

   THD_ivec3 iv ;
   THD_fvec3 fv ;
   float xxx[VSIZE], yyy[VSIZE], zzz[VSIZE] ;  /* xyz coordinates */
   int   iii,jjj,kkk , nx,ny,nz,nxy,nxyz ;
   THD_3dim_dataset *qset ;
   byte *bar; short *sar; float *far; complex *car; void *var;
   int dtyp , kts ; float ffac ;

ENTRY("ICALC_compute") ;

   if( ics == NULL || !ics->is_good ) RETURN(NULL) ;

   /* workspace vector for each alphabetic symbol, used or unused */

   for (ids=0; ids<26; ids++)
     atoz[ids] = (double *)calloc( sizeof(double) , VSIZE ) ;

   /* dimensions */

   qset = ics->dset_master ;

   nx = DSET_NX(qset); ny = DSET_NY(qset); nz = DSET_NZ(qset);
   nxy = nx*ny; nxyz = nxy*nz;

   /* create output image */

   bim = mri_new_vol( nx,ny,nz , MRI_float ) ; buf = MRI_FLOAT_PTR(bim) ;

   /***----- loop over voxels, do VSIZE voxels at time -----***/

if(DEBUG) INFO_message("Start computation loop") ;

   for( ii = 0 ; ii < nxyz ; ii += VSIZE ){

     jbot = ii ; jtop = MIN( ii + VSIZE , nxyz ) ;  /* do voxels jj..jtop-1 */

     /* load (x,y,z) coords of these voxels into arrays, if needed */

     if( ics->has_xyz ){
       for( jj=jbot ; jj < jtop ; jj++ ){
         LOAD_IVEC3( iv , jj%nx , (jj%nxy)/nx , jj/nxy ) ;        /* 3D index */
         fv = THD_3dind_to_3dmm( qset , iv ) ;              /* convert to xyz */
         if( ics->mangle_xyz ) fv = THD_3dmm_to_dicomm(qset,fv) ; /* to Dicom */
         UNLOAD_FVEC3(fv,xxx[jj-jbot],yyy[jj-jbot],zzz[jj-jbot]) ;    /* save */
         if( ics->mangle_xyz == MANGLE_LPI ){                       /* to LPI */
           xxx[jj-jbot] = -xxx[jj-jbot] ; yyy[jj-jbot] = -yyy[jj-jbot] ;
         }
       }
     }

     /* loop over datasets or other symbol definitions */

     for( ids=0 ; ids < 26 ; ids++){  /* scan the whole alphabet */

       switch( ics->intyp[ids] ){  /* what type of data goes with this symbol? */

         /* these are real easy */

         case ICALC_DSET_STAT:
         case ICALC_CONSTANT:
           val = ics->inval[ids] ;
           for( jj=jbot ; jj < jtop ; jj++ ) atoz[ids][jj-ii] = val ;
         break ;

         /* however, this is not so easy (too many sub-cases) */

         case ICALC_DSET_VALUE:{
           THD_3dim_dataset *dset ;

if(CEBUG) ININFO_message("extracting from dataset '%c'",abet[ids]) ;

           if( ics->dshift[ids] >= 0 ){   /* differential subscripted dataset */
             int jds = ics->dshift[ids] ;     /* actual dataset index */
             int jjs , ix,jy,kz ;
             int id=ics->dshift_i[ids], jd=ics->dshift_j[ids], /* index shifts */
                 kd=ics->dshift_k[ids], ld=ics->dshift_l[ids] ;
             int ijkd = ((id!=0) || (jd!=0) || (kd!=0)) ;   /* spatial shift? */
             int dsx=nx-1 , dsy=ny-1 , dsz=nz-1 , dst ;
             int mode=ics->dshift_mode , dun=0 ;   /* dun == are we done yet? */

if(CEBUG) ININFO_message("  dshift: jds=%d  id=%d jd=%d kd=%d ld=%d",jds,id,jd,kd,ld) ;
             dset = ics->inset[jds] ;
             dst  = DSET_NVALS(dset) - 1 ;  /* last allowed time index */

             kts = ics->inidx[jds] + ld ;    /* shifted time index */
             if( kts < 0 || kts > dst ){
                switch( mode ){
                  case DSHIFT_MODE_ZERO:
                    for( jj=jbot ; jj < jtop ; jj++ ) atoz[ids][jj-ii] = 0.0 ;
                    dun = 1 ;
                  break ;
                  default:
                  case DSHIFT_MODE_STOP:
                         if( kts <  0  ) kts = 0   ;
                    else if( kts > dst ) kts = dst ;
                  break ;
                  case DSHIFT_MODE_WRAP:
                    while( kts <  0  ) kts += (dst+1) ;
                    while( kts > dst ) kts -= (dst+1) ;
                  break ;
                }
             }

             if( !dun ){   /* must get some actual data */
               dtyp = DSET_BRICK_TYPE  (dset,kts);
               var  = DSET_ARRAY       (dset,kts);
               ffac = DSET_BRICK_FACTOR(dset,kts); if( ffac==0.0f ) ffac = 1.0f;
               if( var == NULL ){ DSET_load(dset); var = DSET_ARRAY(dset,kts); }
               switch(dtyp){
                 case MRI_rgb:
                 case MRI_byte:    bar = (byte *   )var ; break ;
                 case MRI_short:   sar = (short *  )var ; break ;
                 case MRI_float:   far = (float *  )var ; break ;
                 case MRI_complex: car = (complex *)var ; break ;
               }
               for( dun=0,jj=jbot ; jj < jtop ; jj++ ){ /* loop over voxels */
                 jjs = jj ;                  /* nominal voxel spatial index */
                 if( ijkd ){                 /* if spatial shift is ordered */
                   ix = DSET_index_to_ix(qset,jj) ;
                   jy = DSET_index_to_jy(qset,jj) ;
                   kz = DSET_index_to_kz(qset,jj) ;

                   ix += id ;                  /* shifted x index */
                   if( ix < 0 || ix > dsx ){
                     switch( mode ){
                       case DSHIFT_MODE_ZERO:
                         atoz[ids][jj-ii] = 0.0 ; dun = 1 ;
                       break ;
                       default:
                       case DSHIFT_MODE_STOP:
                              if( ix <  0  ) ix = 0   ;
                         else if( ix > dsx ) ix = dsx ;
                       break ;
                       case DSHIFT_MODE_WRAP:
                         while( ix <  0  ) ix += (dsx+1) ;
                         while( ix > dsx ) ix -= (dsx+1) ;
                       break ;
                     }
                   }
                   if( dun ){ dun=0; continue; } /* go to next jj */

                   jy += jd ;                  /* shifted y index */
                   if( jy < 0 || jy > dsy ){
                     switch( mode ){
                       case DSHIFT_MODE_ZERO:
                         atoz[ids][jj-ii] = 0.0 ; dun = 1 ;
                       break ;
                       default:
                       case DSHIFT_MODE_STOP:
                              if( jy <  0  ) jy = 0   ;
                         else if( jy > dsy ) jy = dsy ;
                       break ;
                       case DSHIFT_MODE_WRAP:
                         while( jy <  0  ) jy += (dsy+1) ;
                         while( jy > dsy ) jy -= (dsy+1) ;
                       break ;
                     }
                   }
                   if( dun ){ dun=0; continue; } /* go to next jj */

                   kz += kd ;                  /* shifted z index */
                   if( kz < 0 || kz > dsz ){
                     switch( mode ){
                       case DSHIFT_MODE_ZERO:
                         atoz[ids][jj-ii] = 0.0 ; dun = 1 ;
                       break ;
                       default:
                       case DSHIFT_MODE_STOP:
                              if( kz <  0  ) kz = 0   ;
                         else if( kz > dsz ) kz = dsz ;
                       break ;
                       case DSHIFT_MODE_WRAP:
                         while( kz <  0  ) kz += (dsz+1) ;
                         while( kz > dsz ) kz -= (dsz+1) ;
                       break ;
                     }
                   }
                   if( dun ){ dun=0; continue; } /* go to next jj */

                   jjs = DSET_ixyz_to_index(dset,ix,jy,kz) ;
                 } /* end of shifted spatial index calculation into jjs */

                 switch( dtyp ) {  /* extract one voxel value */
                   case MRI_short:
                     atoz[ids][jj-ii] = sar[jjs] * ffac ;
                   break ;
                   case MRI_float:
                     atoz[ids][jj-ii] = far[jjs] * ffac ;
                   break ;
                   case MRI_byte:
                     atoz[ids][jj-ii] = bar[jjs] * ffac ;
                   break ;
                   case MRI_rgb:
                     atoz[ids][jj-ii] = Rfac*bar[3*jjs]+Gfac*bar[3*jjs+1]+Bfac*bar[3*jjs+2] ;
                   break ;
                   case MRI_complex:{
                     complex cv=car[jjs] ; float xx=cv.r, yy=cv.i , vv ;
                     switch( ics->cxcode ){
                       case CX_REALPART:  vv = xx ;                    break ;
                       case CX_IMAGPART:  vv = yy ;                    break ;
                       case CX_PHASE:     vv = (xx==0.0f && yy==0.0f)
                                               ? 0.0f : atan2(yy,xx) ; break ;
                       default:
                       case CX_MAGNITUDE: vv = complex_abs(cv) ;       break ;
                     }
                     atoz[ids][jj-ii] = vv ;
                   }
                 } /* end of data type extraction switch */
               } /* end of jj loop over voxels */
             } /* end of if getting actual data (wasn't time shifted to 0) */
           } /* end of differential subscripted sub-case */

           else if( ISVALID_DSET(ics->inset[ids]) ) {  /* the "normal" dataset case */
             kts  = ics->inidx[ids] ;      /* time index */
             dset = ics->inset[ids] ;
             dtyp = DSET_BRICK_TYPE  (dset,kts);
             var  = DSET_ARRAY       (dset,kts);
             ffac = DSET_BRICK_FACTOR(dset,kts); if( ffac==0.0f ) ffac = 1.0f;
             if( var == NULL ){ DSET_load(dset); var = DSET_ARRAY(dset,kts); }
if(CEBUG) ININFO_message("  normal dataset: kts=%d",kts) ;
             switch(dtyp){
               case MRI_rgb:
               case MRI_byte:    bar = (byte *   )var ; break ;
               case MRI_short:   sar = (short *  )var ; break ;
               case MRI_float:   far = (float *  )var ; break ;
               case MRI_complex: car = (complex *)var ; break ;
             }

             switch( dtyp ) {  /* extract data for all voxels */
               case MRI_short:
                 for (jj =jbot ; jj < jtop ; jj ++ )
                   atoz[ids][jj-ii] = sar[jj] * ffac ;
               break ;
               case MRI_float:
                 for (jj =jbot ; jj < jtop ; jj ++ )
                   atoz[ids][jj-ii] = far[jj] * ffac ;
               break ;
               case MRI_byte:
                 for (jj =jbot ; jj < jtop ; jj ++ )
                   atoz[ids][jj-ii] = bar[jj] * ffac ;
               break ;
               case MRI_rgb:
                 for (jj =jbot ; jj < jtop ; jj ++ )
                   atoz[ids][jj-ii] = Rfac*bar[3*jj]+Gfac*bar[3*jj+1]+Bfac*bar[3*jj+2] ;
               break ;
               case MRI_complex:{
                 complex cv ; float xx, yy, vv ;
                 for (jj =jbot ; jj < jtop ; jj ++ ){
                   cv=car[jj] ; xx=cv.r ; yy=cv.i ;
                   switch( ics->cxcode ){
                     case CX_REALPART:  vv = xx ;                    break ;
                     case CX_IMAGPART:  vv = yy ;                    break ;
                     case CX_PHASE:     vv = (xx==0.0f && yy==0.0f)
                                             ? 0.0f : atan2(yy,xx) ; break ;
                     default:
                     case CX_MAGNITUDE: vv = complex_abs(cv) ;       break ;
                   }
                   atoz[ids][jj-ii] = vv ;
                 }
               }
               break ;
             } /* end of data type extraction switch */

          } /** end of 3D dataset normal sub-case **/

          /* the case of a voxel (x,y,z) or (i,j,k) coordinate */

          else if( ics->has_predefined ) {

if(CEBUG) ININFO_message("  predefined value '%c'",abet[ids]) ;

            switch( ids ){
               case 23:     /* x */
                 if( HAS_X(ics) )
                   for( jj=jbot ; jj < jtop ; jj++ )
                     atoz[ids][jj-ii] = xxx[jj-ii] ;
               break ;

               case 24:     /* y */
                 if( HAS_Y(ics) )
                   for( jj=jbot ; jj < jtop ; jj++ )
                     atoz[ids][jj-ii] = yyy[jj-ii] ;
               break ;

               case 25:     /* z */
                 if( HAS_Z(ics) )
                   for( jj=jbot ; jj < jtop ; jj++ )
                     atoz[ids][jj-ii] = zzz[jj-ii] ;
               break ;

               case 8:     /* i */
                 if( HAS_I(ics) )
                   for( jj=jbot ; jj < jtop ; jj++ )
                     atoz[ids][jj-ii] = (jj%nx) ;
               break ;

               case 9:     /* j */
                 if( HAS_J(ics) )
                   for( jj=jbot ; jj < jtop ; jj++ )
                     atoz[ids][jj-ii] = ((jj%nxy)/nx) ;
               break ;

               case 10:    /* k */
                 if( HAS_K(ics) )
                   for( jj=jbot ; jj < jtop ; jj++ )
                     atoz[ids][jj-ii] = (jj/nxy) ;
               break ;

#if 0
               case 19:    /* t */
                 if( HAS_T(ics) )
                   for( jj=jbot ; jj < jtop ; jj++ )
                     atoz[ids][jj-ii] = THD_timeof_vox(kts,jj,new_dset) ;
               break ;
#endif

#if 0
               case 11:    /* l */
                 if( HAS_L(ics) )
                   for( jj=jbot ; jj < jtop ; jj++ )
                     atoz[ids][jj-ii] = kts ;
               break ;
#endif
             } /* end of switch on symbol subscript */

           } /* end of choice over data type (if-else cascade) */
         }
         break ; /* end of dataset value case */

       } /* end of switch over ICALC mode for this symbol */

     } /* end of loop over datasets/symbols */

     /****----- actually do the calculation work! -----****/

if(CEBUG) ININFO_message("-- evaluate!") ;
     PARSER_evaluate_vector( (PARSER_code *)ics->olay_pcode ,
                              atoz , jtop-jbot , temp       );

     /****----- put results into output image array -----****/

     for( jj=jbot ; jj < jtop ; jj++ ) buf[jj] = (float)temp[jj-ii];

   } /*---------- end of loop over space (voxels) ----------*/

   /* check results for floating point validity */

   nbad = thd_floatscan( nxyz , buf ) ;
   if( nbad > 0 )
     WARNING_message("%d bad floats replaced by 0 in ICALC_compute",nbad) ;

   for( ids=0; ids < 26; ids++ ) free(atoz[ids]) ;  /* toss the trash */

   RETURN(bim) ;
}
