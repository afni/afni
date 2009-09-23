#include "afni.h"
#include "parser.h"

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
static void ICALC_index_av_CB( MCW_arrowval * , XtPointer ) ;

static char * ICALC_index_lab_CB( MCW_arrowval * , XtPointer ) ;

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
     iwid->war[aa].index_av =                                        \
        new_MCW_optmenu( rc , "[-]" , -1,0, -1,0,                    \
                         ICALC_index_av_CB, (XtPointer)iwid ,        \
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

void ICALC_make_widgets( Three_D_View *im3d )
{
   ICALC_widget_set *iwid ;
   Widget ww , rc , shtop , swtop ;
   XmString xstr ;
   char str[32] , *eee ;
   int ii ;
   ICALC_setup *ics ;

ENTRY("ICALC_make_widgets") ;

   if( !IM3D_OPEN(im3d) ) EXRETURN ; /* bad */

   if( im3d->vwid->func->iwid != NULL ){
     XtMapWidget(iwid->wtop) ; iwid->is_open = 1 ;
     WAIT_for_window( iwid->wtop ) ;
     XRaiseWindow( XtDisplay(iwid->wtop) , XtWindow(iwid->wtop) ) ;
   }

   im3d->vwid->func->iwid = iwid = myXtNew( ICALC_widget_set ) ;
   for( ii=0 ; ii < 26 ; ii++ ) iwid->var[ii] = NULL ;

   if( im3d->icalc_setup == NULL )
     INIT_ICALC_setup(im3d->icalc_setup) ;
   ics = im3d->icalc_setup ;

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
   ICALC_setup      *ics  = im3d->icalc_setup ;
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
   ICALC_setup      *ics  = im3d->icalc_setup ;
   int                 aa = ICALC_find_index(iwid,av->wrowcol) ;
   int                 bb = av->ival ;

   if( aa < 0 || bb < 0 || bb >= ICALC_NUMTYPE ) return ;
   MCW_set_widget_label( iwid->war[aa].chooser_pb , ICALC_choosestr[bb]  ) ;
   MCW_set_widget_label( iwid->war[aa].chooser_lab, ICALC_nothing_chosen ) ;
   MCW_set_widget_label( iwid->war[aa].string_lab , ICALC_labelstr[bb]   ) ;
   XmTextFieldSetString( iwid->war[aa].string_text, "\0"                 ) ;

   ICALC_toggle_row(iwid->war[aa],True) ;
}

/*----------------------------------------------------------------------*/

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

     case ICALC_1DFILE:     break ;  /* not implemented yet */
     case ICALC_CONSTANT:   break ;
   }
}

/*----------------------------------------------------------------------*/

static void ICALC_quit_CB( Widget w, XtPointer cd, XtPointer cbs )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   Three_D_View *im3d     = iwid->im3d ;
   ICALC_setup      *ics  = im3d->icalc_setup ;
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

/*----------------------------------------------------------------------*/

static void ICALC_finalize_dataset_CB( Widget w, XtPointer fd, MCW_choose_cbs *cbs )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)fd ;
   Three_D_View     *im3d = iwid->im3d ;
   ICALC_setup      *ics  = im3d->icalc_setup ;
   int id = cbs->ival ;
   THD_3dim_dataset *dset ;
   int inival , topval ;

   if( dsv == NULL || id < 0 || id >= dsv->dset_count || dsa < 0 ){ BEEPIT; return; }
   dset = PLUTO_find_dset( &(dsv->dset_link[id].idcode) ) ;
   if( !ISVALID_DSET(dset) )                                      { BEEPIT; return; }
   MCW_set_widget_label( iwid->war[dsa].chooser_lab ,
                         dset->dblk->diskptr->filecode ) ;

   ics->inset[dsa] = dset ;
   inival = iwid->war[dsa].index_av->ival ;
   topval = DSET_NVALS(dset) - 1 ;
   if( inival > topval ) inival = topval ;
   refit_MCW_optmenu( iwid->war[dsa].index_av ,
                      -1 , DSET_NVALS(dset)-1 , inival, 0 ,
                      ICALC_index_lab_CB,(XtPointer)iwid  ) ;
   return ;
}

/*----------------------------------------------------------------------*/

static void ICALC_index_av_CB( MCW_arrowval *av , XtPointer cd )
{
   ICALC_widget_set *iwid = (ICALC_widget_set *)cd ;
   Three_D_View     *im3d = iwid->im3d ;
   ICALC_setup      *ics  = im3d->icalc_setup ;
   int                 aa = ICALC_find_index(iwid,av->wrowcol) ;
   int                 bb = av->ival ;

}

/*----------------------------------------------------------------------*/

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

/*-------------------------------------------------------------------------*/
/*------------------------ the actual computations ------------------------*/

#define DSHIFT_MODE_STOP  0
#define DSHIFT_MODE_WRAP  1
#define DSHIFT_MODE_ZERO  2

#define HAS_I(q)  (q)->has_sym[ 8]
#define HAS_J(q)  (q)->has_sym[ 9]
#define HAS_K(q)  (q)->has_sym[10]
#define HAS_L(q)  (q)->has_sym[11]
#define HAS_T(q)  (q)->has_sym[19]
#define HAS_X(q)  (q)->has_sym[23]
#define HAS_Y(q)  (q)->has_sym[24]
#define HAS_Z(q)  (q)->has_sym[25]

static char abet[] = "abcdefghijklmnopqrstuvwxyz" ;

#define PREDEFINED_MASK ((1<< 8)|(1<< 9)|(1<<10)|(1<<11)| \
                         (1<<19)|(1<<23)|(1<<24)|(1<<25) )

#define MANGLE_NONE 0
#define MANGLE_RAI  1
#define MANGLE_LPI  2

#define Rfac = 0.299f ;
#define Gfac = 0.587f ;
#define Bfac = 0.114f ;

#define CX_REALPART  0               /* 10 Mar 2006: complex to real methods */
#define CX_IMAGPART  1
#define CX_MAGNITUDE 2
#define CX_PHASE     3

/*------------------------------------------------------------------*/

#define VSIZE 1024  /* vector size for PARSER computations */

#if 0
MRI_IMAGE * ICALC_compute( ICALC_setup *ics )
{
   double  temp[VSIZE];
   double *atoz[26] ;
   int ii , ids , jj, kk, kt, ll, jbot, jtop ;
   float  *buf; MRI_IMAGE *bim ;
   int     nbad ;      /* 09 Aug 2000: check for bad results */

   THD_ivec3 iv ;
   THD_fvec3 fv ;
   float xxx[VSIZE], yyy[VSIZE], zzz[VSIZE] ;
   int   iii,jjj,kkk , nx,nxy ;
   THD_dataxes *daxes ; THD_3dim_dataset *qset ;
   byte *bar; short *sar; float *sar; complex *car; void *var;
   int dtyp , kts ; float ffac ;

ENTRY("ICALC_compute") ;

   /* workspace vector for each alphabetic symbol, used or unused */

   for (ids=0; ids<26; ids++)
     atoz[ids] = (double *)calloc( sizeof(double) , VSIZE ) ;

   /* dimensions */

   nx = daxes->nxx; ny = daxes->nyy; nz = daxes->nzz; nxy = nx*ny; nxyz = nxy*nz;

   /* malloc output image space */

   bim = mri_new_vol( nx,ny,nz , MRI_float ) ; buf = MRI_FLOAT_PTR(bim) ;

   /* fake dataset */

   qset = EDT_empty_copy(NULL) ;
   *(qset->daxes) = *daxes ; qset->daxes->parent = (XtPointer)qset ;

   /***----- loop over voxels, do VSIZE voxels at time -----***/

   for( ii = 0 ; ii < nxyz ; ii += VSIZE ){

     jbot = ii ; jtop = MIN( ii + VSIZE , nxyz ) ;  /* do voxels jj..jtop-1 */

     /* load (x,y,z) coords of these voxels into arrays, if needed */

     if( ics->has_xyz ){
       for( jj=jbot ; jj < jtop ; jj++ ){
         LOAD_IVEC3( iv , jj%nx , (jj%nxy)/nx , jj/nxy ) ;
         fv = THD_3dind_to_3dmm( qset , iv ) ;
         if( ics->mangle_xyz ) fv = THD_3dmm_to_dicomm(qset,fv) ;
         UNLOAD_FVEC3(fv,xxx[jj-jbot],yyy[jj-jbot],zzz[jj-jbot]) ;
         if( ics->mangle_xyz == MANGLE_LPI ){
           xxx[jj-jbot] = -xxx[jj-jbot] ; yyy[jj-jbot] = -yyy[jj-jbot] ;
         }
       }
     }

     /* loop over datasets or other symbol definitions */

     for (ids = 0 ; ids < 26 ; ids ++ ){  /* the whole alphabet */

       switch( ics->intyp[ids] ){

         /* this is real easy */

         case ICALC_DSET_STAT:
         case ICALC_CONSTANT:
           for (jj =jbot ; jj < jtop ; jj ++ ) atoz[ids][jj-ii] = ics->inval[ids] ;
         break ;

         /* however, this is not so easy */

         case ICALC_DSET_VALUE:{
           THD_3dim_dataset *dset ;

           if( ics->dshift[ids] >= 0 ){   /* differential subscripted dataset */
             int jds = ics->dshift[ids] ;     /* actual dataset index */
             int jjs , ix,jy,kz ;
             int id=ics->dshift_i[ids] , jd=ics->dshift_j[ids] ,
                 kd=ics->dshift_k[ids] , ld=ics->dshift_l[ids] ;
             int ijkd = ((id!=0) || (jd!=0) || (kd!=0)) ;
             int dsx=nx-1 , dsy=ny-1 , dsz=nz-1 , dst ;
             int mode=ics->dshift_mode , dun=0 ;  /* dun == 'are we done yet?' */

             dset = ics->inset[jds] ;
             dst  = DSET_NVALS(dset) - 1 ;

             kts = ics->inidx[jds] + ld ;    /* t shift */
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
                   ix = DAXES_index_to_ix(daxes,jj) ;
                   jy = DAXES_index_to_jy(daxes,jj) ;
                   kz = DAXES_index_to_kz(daxes,jj) ;

                   ix += id ;                  /* x shift */
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

                   jy += jd ;                  /* y shift */
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

                   kz += kd ;                  /* z shift */
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

                   jjs = DSET_ixyz_to_index(daxes,ix,jy,kz) ;
                 } /* end of spatial shift index calculation */

                 switch( dtyp ) {  /* extract data */
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
                   case MRI_complex:{                        /* 10 Mar 2006 */
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
               } /* end of loop over voxels */
             } /* end of getting actual data */
           } /* end of differential subscripted input */

           else if( ISVALID_DSET(ics->inset[ids]) ) {  /* the "normal" dataset case */
             kts  = ics->inidx[ids] ;
             dset = ics->inset[ids] ;
             dtyp = DSET_BRICK_TYPE  (dset,kts);
             var  = DSET_ARRAY       (dset,kts);
             ffac = DSET_BRICK_FACTOR(dset,kts); if( ffac==0.0f ) ffac = 1.0f;
             switch(dtyp){
               case MRI_rgb:
               case MRI_byte:    bar = (byte *   )var ; break ;
               case MRI_short:   sar = (short *  )var ; break ;
               case MRI_float:   far = (float *  )var ; break ;
               case MRI_complex: car = (complex *)var ; break ;
             }

             switch( dtyp ) {  /* extract data */
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
               case MRI_complex:{                        /* 10 Mar 2006 */
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

          } /** end of 3D dataset **/

          /* the case of a voxel (x,y,z) or (i,j,k) coordinate */

          else if( ics->has_predefined ) {

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

               case 19:    /* t */
                 if( HAS_T(ics) )
                   for( jj=jbot ; jj < jtop ; jj++ )
                     atoz[ids][jj-ii] = THD_timeof_vox(kt,jj,new_dset) ;
               break ;

               case 11:    /* l */
                 if( HAS_L(ics) )
                   for( jj=jbot ; jj < jtop ; jj++ )
                     atoz[ids][jj-ii] = kt ;
               break ;
             } /* end of switch on symbol subscript */

           } /* end of choice over data type (if-else cascade) */
         }
         break ; /* end of dataset value case */

       } /* end of switch over ICALC mode for this symbol */

     } /* end of loop over datasets/symbols */

     /**** actually do the calculation work! ****/

     PARSER_evaluate_vector(CALC_code, atoz, jtop-jbot, temp);

     /**** put results into output image array ****/

     for ( jj = jbot ; jj < jtop ; jj ++ ) buf[jj] = temp[jj-ii];

   } /*---------- end of loop over space (voxels) ----------*/

   /* check results for validity */

   nbad = thd_floatscan( nxyz , buf ) ;
   if( nbad > 0 )
     WARNING_message("%d bad floats replaced by 0 in ICALC_compute",nbad) ;

   for( ids=0; ids<26; ids++ ) free(atoz[ids]) ;  /* toss the trash */
   DSET_delete(qset) ;

   RETURN(bim) ;
}
#endif
