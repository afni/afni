#include "afni.h"

/*--- macros for drawing lines ---*/

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

/*--- string constants ---*/

#undef  ICALC_NUMTYPE
#define ICALC_NUMTYPE 4
static char *ICALC_typestr[] = { "Dataset: Value" ,
                                 "Dataset: Stat." ,
                                 "1D File"        ,
                                 "Constant Value"  } ;

static char *ICALC_choosestr[] = { "Choose Dataset" ,
                                   "Choose Dataset" ,
                                   "Choose 1D File" ,
                                   "--------------"  } ;

static char *ICALC_labelstr[]  = { "diffsub:" ,
                                   "3D stat:" ,
                                   "x,y,z,t:" ,
                                   "Value:"     } ;

static char *ICALC_nothing_chosen = "---nothing chosen---" ;

/*--- prototypes ---*/

static void ICALC_tog_bbox_CB( Widget , XtPointer , XtPointer ) ;
static void ICALC_menu_av_CB ( MCW_arrowval * , XtPointer ) ;
static void ICALC_chooser_CB ( Widget, XtPointer, XtPointer ) ;
static void ICALC_quit_CB    ( Widget, XtPointer, XtPointer ) ;
static void ICALC_apply_CB   ( Widget, XtPointer, XtPointer ) ;
static void ICALC_help_CB    ( Widget, XtPointer, XtPointer ) ;

static void ICALC_choose_dataset( ICALC_widget_set *, int ) ;
static void ICALC_finalize_dataset_CB( Widget, XtPointer, MCW_choose_cbs * ) ;

/*--- define action area ---*/

#undef  ICALC_NUMACT
#define ICALC_NUMACT 3
static MCW_action_item ICALC_act[] =
 { { "Quit InstaCalc" , ICALC_quit_CB , NULL,NULL, "Shutdown InstaCalc operations", 0 },
   { "Apply InstaCalc", ICALC_apply_CB, NULL,NULL, "Setup InstaCalc operations"   , 1 },
   { "A Little Help"  , ICALC_help_CB , NULL,NULL, "from my friends"              , 0 }
  } ;

/*--- macros to toggle a row of ICALC widgets on or off ---*/

#undef  ICALC_toggle_row
#define ICALC_toggle_row(rr,state)                            \
 do{ int qz = state && (rr).menu_av->ival != ICALC_CONSTANT ; \
     AV_SENSITIZE((rr).menu_av,state) ;                       \
     XtSetSensitive((rr).chooser_pb,qz) ;                     \
     XtSetSensitive((rr).chooser_lab,qz) ;                    \
     XtSetSensitive((rr).string_text,state) ;                 \
     XtSetSensitive((rr).string_lab,state) ;                  \
 } while(0)

#undef  ICALC_row_off
#undef  ICALC_row_on
#define ICALC_row_off(ic,aa) ICALC_toggle_row((ic)->war[aa],False)
#define ICALC_row_on(ic,aa)  ICALC_toggle_row((ic)->war[aa],True)

/*--- macro to make one row of ICALC widgets ---*/

#undef  ICALC_userdata
#define ICALC_userdata(w,x) XtVaSetValues((w),XmNuserData,(XtPointer)(x),NULL)

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
     sss[0] = 'A' + (aa); sss[1] = '\0'; sp[0] = sss;                \
     iwid->war[aa].tog_bbox =                                        \
        new_MCW_bbox( rc, 1,sp, MCW_BB_check,MCW_BB_noframe,         \
                      ICALC_tog_bbox_CB , (XtPointer)iwid     ) ;    \
     ICALC_userdata(iwid->war[aa].tog_bbox->wbut[0],aa+1) ;          \
     VLINE(iwid->war[aa].rc) ;                                       \
     iwid->war[aa].menu_av =                                         \
        new_MCW_optmenu( rc , "Type" , 0, ICALC_NUMTYPE-1, 0, 0,     \
                         ICALC_menu_av_CB, (XtPointer)iwid ,         \
                         MCW_av_substring_CB, ICALC_typestr ) ;      \
     ICALC_userdata(iwid->war[aa].menu_av->wrowcol,aa+1) ;           \
     VLINEE(iwid->war[aa].rc) ;                                      \
     iwid->war[aa].chooser_pb =                                      \
        XtVaCreateManagedWidget(                                     \
            ff     , xmPushButtonWidgetClass , rc ,                  \
            LABEL_ARG(ICALC_choosestr[0]) , XmNtraversalOn , True ,  \
            XmNinitialResourcesPersistent , False , NULL ) ;         \
     XtAddCallback( iwid->war[aa].chooser_pb , XmNactivateCallback , \
                    ICALC_chooser_CB , (XtPointer)iwid ) ;           \
     ICALC_userdata(iwid->war[aa].chooser_pb,aa+1) ;                 \
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
       MCW_set_widget_bg(iwid->war[aa].tog_bbox->wbut[0],"black",0); \
       MCW_set_widget_bg(iwid->war[aa].string_lab       ,"black",0); \
       MCW_set_widget_bg(iwid->war[aa].chooser_pb       ,"black",0); \
       MCW_set_widget_bg(iwid->war[aa].chooser_lab      ,"black",0); \
     }                                                               \
     XtManageChild(rc) ; ICALC_toggle_row(iwid->war[aa],False) ;     \
  } while(0)

/*----------------------------------------------------------------------------*/

void ICALC_make_widgets( Three_D_View *im3d )
{
   ICALC_widget_set *iwid ;
   Widget ww , rc , shtop , swtop ;
   XmString xstr ;
   char str[32] , *eee ;
   int ii ;

ENTRY("ICALC_make_widgets") ;

   if( !IM3D_OPEN(im3d) || im3d->vwid->func->iwid != NULL ) EXRETURN ; /* bad */

   im3d->vwid->func->iwid = iwid = myXtNew( ICALC_widget_set ) ;
   for( ii=0 ; ii < 26 ; ii++ ) iwid->var[ii] = NULL ;

   sprintf(str,"AFNI InstCalc %s",AFNI_controller_label(im3d)) ;

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
            "menu"   , xmLabelWidgetClass , rc ,
            LABEL_ARG("OLay Expr") ,
            XmNalignment , XmALIGNMENT_BEGINNING ,
            XmNrecomputeSize , False ,  XmNtraversalOn , True ,
            XmNinitialResourcesPersistent , False , NULL ) ;

   /* OLay expression text */

   iwid->olay_expr_text = XtVaCreateManagedWidget(
                           "menu"  , xmTextFieldWidgetClass , rc ,
                             XmNvalue        , "\0" ,
                             XmNcolumns      , 76 ,
                             XmNeditable     , True ,
                             XmNmaxLength    , 180 ,
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

/*----------------------------------------------------------------------*/

static int ICALC_find_index( ICALC_widget_set *iwid , Widget ww )
{
   int aa ; XtPointer pp=NULL ;
   XtVaGetValues( ww , XmNuserData , &pp , NULL ) ;
   if( pp == NULL ) return -1 ;
   return (int)(pp-1) ;
}

/*----------------------------------------------------------------------*/

static void ICALC_tog_bbox_CB( Widget w, XtPointer cd, XtPointer cbs )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   Three_D_View     *im3d = iwid->im3d ;
   int                 aa = ICALC_find_index(iwid,w) ; Boolean bb ;

   if( aa < 0 ) return ;
   bb = MCW_val_bbox(iwid->war[aa].tog_bbox) ;
   ICALC_toggle_row(iwid->war[aa],bb) ;
}

/*----------------------------------------------------------------------*/

static void ICALC_menu_av_CB( MCW_arrowval *av , XtPointer cd )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   Three_D_View     *im3d = iwid->im3d ;
   int                 aa = ICALC_find_index(iwid,av->wrowcol) ;
   int                 bb = av->ival , ss = (bb!=ICALC_CONSTANT) ;

INFO_message("menu %d:%d",aa,bb) ;
   if( aa < 0 || bb < 0 || bb >= ICALC_NUMTYPE ) return ;
   MCW_set_widget_label( iwid->war[aa].chooser_pb , ICALC_choosestr[bb]  ) ;
   MCW_set_widget_label( iwid->war[aa].chooser_lab, ICALC_nothing_chosen ) ;
   MCW_set_widget_label( iwid->war[aa].string_lab , ICALC_labelstr[bb]   ) ;
   XmTextFieldSetString( iwid->war[aa].string_text, "\0"                 ) ;

   XtSetSensitive( iwid->war[aa].chooser_pb  , ss ) ;
   XtSetSensitive( iwid->war[aa].chooser_lab , ss ) ;
}

/*----------------------------------------------------------------------*/

static void ICALC_chooser_CB( Widget w, XtPointer cd, XtPointer cbs )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   Three_D_View     *im3d = iwid->im3d ;
   int                 aa = ICALC_find_index(iwid,w) , bb ;

   if( aa < 0 ) return ;
   bb = iwid->war[aa].menu_av->ival ;
   if( bb < 0 || bb >= ICALC_NUMTYPE ) return ;
INFO_message("chooser %d:%d",aa,bb) ;

   POPDOWN_strlist_chooser ;  /* death to the old regime */

   switch( bb ){
     case ICALC_DSET_VALUE:
     case ICALC_DSET_STAT:  ICALC_choose_dataset( iwid , aa ) ; break ;

     case ICALC_1DFILE:     break ;
     case ICALC_CONSTANT:   break ;
   }
}

/*----------------------------------------------------------------------*/

static void ICALC_quit_CB( Widget w, XtPointer cd, XtPointer cbs )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   XtUnmapWidget(iwid->wtop) ; iwid->is_open = 0 ;
   DISABLE_INSTACALC(iwid->im3d) ;
}

/*----------------------------------------------------------------------*/

static void ICALC_apply_CB( Widget w, XtPointer cd, XtPointer cbs )
{
INFO_message("apply") ;
}

/*----------------------------------------------------------------------*/

static void ICALC_help_CB( Widget w, XtPointer cd, XtPointer cbs )
{
INFO_message("This software is beyond all hope or help") ;
}

/*----------------------------------------------------------------------*/

static PLUGIN_dsetval *dsv = NULL ;
static int             dsa = -1 ;

static void ICALC_choose_dataset( ICALC_widget_set *iwid , int aa )
{
   Three_D_View *im3d = iwid->im3d ;
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
        kk = DSET_BRICK_TYPE(dset,0) ;
        if( kk != MRI_byte && kk != MRI_short && kk != MRI_float ) continue ;

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

/*----------------------------------------------------------------------*/

static void ICALC_finalize_dataset_CB( Widget w, XtPointer fd, MCW_choose_cbs *cbs )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)fd ;
   int id = cbs->ival ;
   THD_3dim_dataset *dset ;

INFO_message("id=%d dsv->dset_count=%d",id,dsv->dset_count) ;

   if( dsv == NULL || id < 0 || id >= dsv->dset_count ){ BEEPIT; return; }
ININFO_message("  finding") ;
   dset = PLUTO_find_dset( &(dsv->dset_link[id].idcode) ) ;
   if( !ISVALID_DSET(dset) )                           { BEEPIT; return ;}
ININFO_message("  setting") ;
   MCW_set_widget_label( iwid->war[dsa].chooser_lab ,
                         dset->dblk->diskptr->filecode ) ;

   return ;
}
