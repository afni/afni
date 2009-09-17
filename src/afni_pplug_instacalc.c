#include "afni.h"

#undef  VLINE
#define VLINE(rr)                                                           \
     (void) XtVaCreateManagedWidget( "menu", xmSeparatorWidgetClass, (rr) , \
                                        XmNorientation   , XmVERTICAL    ,  \
                                        XmNseparatorType , XmSINGLE_LINE ,  \
                                     NULL )
#undef  HLINE
#define HLINE(rr)                                                           \
   (void) XtVaCreateManagedWidget( "dialog", xmSeparatorWidgetClass, (rr) , \
                                      XmNseparatorType   , XmSINGLE_LINE ,  \
                                   NULL )
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

extern void ICALC_tog_bbox_CB( Widget , XtPointer , XtPointer ) ;
extern void ICALC_menu_av_CB ( MCW_arrowval * , XtPointer ) ;
extern void ICALC_chooser_CB ( Widget, XtPointer, XtPointer ) ;
extern void ICALC_quit_CB    ( Widget, XtPointer, XtPointer ) ;
extern void ICALC_apply_CB   ( Widget, XtPointer, XtPointer ) ;
extern void ICALC_help_CB    ( Widget, XtPointer, XtPointer ) ;

#undef  ICALC_NUMACT
#define ICALC_NUMACT 3
static MCW_action_item ICALC_act[] =
 { { "Quit InstaCalc"  , ICALC_quit_CB  , NULL,NULL , "Shut down InstaCalc operations" , 0 } ,
   { "Apply InstaCalc" , ICALC_apply_CB , NULL,NULL , "Setup InstaCalc operations"     , 1 } ,
   { "A Little Help"   , ICALC_help_CB  , NULL,NULL , "from my friends"                , 0 }
  } ;

#undef  MAKE_ICALC_ROW
#define MAKE_ICALC_ROW(aa)                                          \
 do{ char sss[32] , *sp[1] ; Widget rc ; int ee = (aa)%2==0 ;       \
     char *ff = (ee) ? "menu" : "dialog" ;                          \
     rc = iwid->war[aa].rc =                                        \
        XtVaCreateWidget(                                           \
           ff     , xmRowColumnWidgetClass , iwid->rowcol ,         \
             XmNpacking     , XmPACK_TIGHT ,                        \
             XmNorientation , XmHORIZONTAL ,                        \
             XmNadjustMargin , True ,                               \
             XmNmarginHeight , 2 , XmNmarginWidth , 0 ,             \
             XmNtraversalOn , True ,                                \
           NULL ) ;                                                 \
     if( !ee ) MCW_set_widget_bg(rc,"black",0) ;                    \
     sss[0] = 'A' + (aa) ; sss[1] = '\0' ; sp[0] = sss ;            \
     iwid->war[aa].tog_bbox =                                       \
        new_MCW_bbox( rc, 1,sp, MCW_BB_check,MCW_BB_noframe,        \
                      ICALC_tog_bbox_CB , (XtPointer)iwid     ) ;   \
     iwid->war[aa].menu_av =                                        \
        new_MCW_optmenu( rc , "Type" , 0, ICALC_NUMTYPE-1, 0, 0,    \
                         ICALC_menu_av_CB, (XtPointer)iwid ,        \
                         MCW_av_substring_CB, ICALC_typestr ) ;     \
     iwid->war[aa].chooser_pb =                                     \
        XtVaCreateManagedWidget(                                    \
            ff     , xmPushButtonWidgetClass , rc ,                 \
            LABEL_ARG(ICALC_choosestr[0]) , XmNtraversalOn , True , \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     XtAddCallback( iwid->war[aa].chooser_pb , XmNactivateCallback ,\
                    ICALC_chooser_CB , (XtPointer)iwid ) ;          \
     iwid->war[aa].chooser_lab =                                    \
        XtVaCreateManagedWidget(                                    \
            ff     , xmLabelWidgetClass , rc ,                      \
            LABEL_ARG("                    ") ,                     \
            XmNalignment , XmALIGNMENT_BEGINNING ,                  \
            XmNrecomputeSize , False ,  XmNtraversalOn , True ,     \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     iwid->war[aa].string_lab =                                     \
        XtVaCreateManagedWidget(                                    \
            ff     , xmLabelWidgetClass , rc ,                      \
            LABEL_ARG(ICALC_labelstr[0]) ,                          \
            XmNalignment , XmALIGNMENT_BEGINNING ,                  \
            XmNrecomputeSize , False ,  XmNtraversalOn , True ,     \
            XmNinitialResourcesPersistent , False , NULL ) ;        \
     iwid->war[aa].string_text =                                    \
        XtVaCreateManagedWidget(                                    \
            ff     , xmTextFieldWidgetClass , rc ,                  \
              XmNvalue        , "\0" ,                              \
              XmNcolumns      , 12 ,                                \
              XmNeditable     , True ,                              \
              XmNmaxLength    , 64 ,                                \
              XmNresizeWidth  , False ,                             \
              XmNmarginHeight , 1 ,                                 \
              XmNmarginWidth  , 1 ,                                 \
              XmNcursorPositionVisible , True ,                     \
              XmNblinkRate , 0 ,                                    \
              XmNautoShowCursorPosition , True ,                    \
              XmNtraversalOn , True  ,                              \
              XmNinitialResourcesPersistent , False ,               \
            NULL ) ;                                                \
     if( !ee ){                                                     \
       MCW_set_widget_bg(iwid->war[aa].string_lab ,"black",0) ;     \
       MCW_set_widget_bg(iwid->war[aa].chooser_lab,"black",0) ;     \
     }                                                              \
  } while(0)

/*----------------------------------------------------------------------------*/

static void ICALC_make_widgets( Three_D_View *im3d )
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

   sprintf(str,"AFNI InstCalc [%s]",AFNI_controller_label(im3d)) ;

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
            LABEL_ARG("OLay Expr:") ,
            XmNalignment , XmALIGNMENT_BEGINNING ,
            XmNrecomputeSize , False ,  XmNtraversalOn , True ,
            XmNinitialResourcesPersistent , False , NULL ) ;

   /* OLay expression text */

   iwid->olay_expr = XtVaCreateManagedWidget(
                       "menu"  , xmTextFieldWidgetClass , rc ,
                         XmNvalue        , "\0" ,
                         XmNcolumns      , 32 ,
                         XmNeditable     , True ,
                         XmNmaxLength    , 128 ,
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
   HLINE(iwid->rowcol) ;

   /** rows of widgets to control variables **/

   for( ii=0 ; ii < 26 ; ii++ ) MAKE_ICALC_ROW(ii) ;

   XtManageChild(iwid->rowcol) ;

   if( swtop != NULL ){
     int wx,hy , cmax ;
     MCW_widget_geom( iwid->rowcol  , &wx,&hy,NULL,NULL ) ;
     hy *= 2 ; cmax = im3d->dc->height-128 ; if( hy > cmax ) hy = cmax ;
     XtVaSetValues( iwid->wtop , XmNwidth,wx+31,XmNheight,hy+21 , NULL ) ;
   }

   XtRealizeWidget( iwid->wtop ) ;
   WAIT_for_window( iwid->wtop ) ;
   NORMAL_cursorize( iwid->rowcol ) ;

   EXRETURN ;
}
