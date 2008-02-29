#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to add to an afni_history_*.c file.
  Makes a custom interface.
************************************************************************/

/*---------- prototypes for internal routines ----------*/

char * AHIST_main( PLUGIN_interface * ) ;

static void AHIST_make_widgets(void) ;

static void AHIST_quit_CB ( Widget , XtPointer , XtPointer ) ;
static void AHIST_save_CB ( Widget , XtPointer , XtPointer ) ;
static void AHIST_clear_CB( Widget , XtPointer , XtPointer ) ;
static void AHIST_show_CB ( Widget , XtPointer , XtPointer ) ;

static PLUGIN_interface *plint = NULL ;

static int line_count( char * ) ;

#define TWIDTH  72
#define THEIGHT 19

static char *histfilename = NULL ;

#define BMARKER "/*=====BELOW THIS LINE=====*/"
#define AMARKER "/*=====ABOVE THIS LINE=====*/"

#define NLEVEL 5

static char *level_labs[5] = { "MICRO" , "MINOR" ,
                               "MAJOR" , "SUPER" , "SUPERDUPER" } ;

#define NTYPE 6
static char *type_labs[6] = { "TYPE_GENERAL" , "TYPE_NEW_PROG" ,
                              "TYPE_NEW_OPT" , "TYPE_NEW_ENV"  ,
                              "TYPE_BUG_FIX" , "TYPE_MODIFY"    } ;

/***********************************************************************
   Set up the interface to the user.  Note that we bypass the
   normal interface creation, and simply have the menu selection
   directly call the main function, which will create a custom
   set of interface widgets the first time in.
************************************************************************/

DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   char *cpt ;

   if( ncall > 0 ) return NULL ;  /* at most one interface */

   /* check if we can access the defined history file */

   histfilename = getenv("AFNI_HISTORY_PERSONAL_FILE") ;
   cpt = AFNI_suck_file(histfilename) ;
   if( cpt == NULL ){
     /* INFO_message("Can't suck file %s",(histfilename)?histfilename:"NULL") ; */
     return NULL ;
   }   /* can't use this plugin */
   if( strstr(cpt,BMARKER) == NULL &&
       strstr(cpt,AMARKER) == NULL   ){
     /* INFO_message("file %s doesn't have marker string",histfilename) ; */
     free(cpt); return NULL;
   }
   free(cpt) ;

   plint = PLUTO_new_interface( "+AFNI History" , NULL , NULL ,
                                PLUGIN_CALL_IMMEDIATELY , AHIST_main ) ;

   PLUTO_add_hint( plint , "Add to your personal afni_history file" ) ;

   return plint ;
}

/***************************************************************************
  Will be called from AFNI when user selects from Plugins menu.
****************************************************************************/

/* Interface widgets */

typedef struct {
   Widget frame, vert_rc , horz_rc , note_label ,
          delete_pb , restore_pb , scrollw , textw ;

   char * note_orig , * date_orig ;
} HIST_wind ;

static HIST_wind * AHIST_make_note(void) ;

static Widget shell=NULL , topman ;
static Widget quit_pb , save_pb , clear_pb , show_pb , verbtext ;
MCW_arrowval *program_av , *level_av , *type_av , *oneline_av ;

static int text_width = 0 ;
static int text_height = 0 ;

static MCW_DC * dc ;                 /* display context */
static Three_D_View * im3d ;         /* AFNI controller */
static THD_3dim_dataset * dset ;     /* The dataset!    */
static MCW_idcode         dset_idc ; /* 31 Mar 1999     */

static int editor_open  = 0 ;

char * AHIST_main( PLUGIN_interface * plint )
{
   XmString xstr ;
   int ii ;

   /*-- sanity checks --*/

   if( ! IM3D_OPEN(plint->im3d) ) return "AFNI Controller\nnot opened?!" ;

   if( editor_open ){
     XtMapWidget(shell) ;
     XRaiseWindow( XtDisplay(shell) , XtWindow(shell) ) ;
     return NULL ;
   }

   im3d = plint->im3d ;  /* save for local use */

   /*-- create widgets, first time through --*/

   if( shell == NULL ){
      dc = im3d->dc ;        /* save this too */
      AHIST_make_widgets() ;
INFO_message("set_topshell") ;
      PLUTO_set_topshell( plint , shell ) ;  /* 22 Sep 2000 */
INFO_message("visibilize") ;
      RWC_visibilize_widget( shell ) ;       /* 27 Sep 2000 */
   }

#if 0
   XtVaSetValues( shell , XmNtitle , histfilename , NULL ) ;
#endif

   /*-- pop the widget up --*/

INFO_message("map") ;
   XtMapWidget(shell) ;
INFO_message("cursorize") ;
   PLUTO_cursorize(shell) ;

   editor_open  = 1 ;      /* editor is now open for business */

INFO_message("finito") ;
   return NULL ;
}

/*------------------------------------------------------------------------
  Make the control popup window for this beast
--------------------------------------------------------------------------*/

/*-- structures defining action buttons (at bottom of popup) --*/

#define NACT 4  /* number of action buttons */

static MCW_action_item AHIST_actor[NACT] = {
 {"Quit",AHIST_quit_CB,NULL,
  "Clear and close this Editor" , "Clear and close" , 1 } ,

 {"Clear", AHIST_clear_CB,NULL,
  "Clear the text fields" , "Clear text fields" , 0 } ,

 {"Save",AHIST_save_CB,NULL,
  "Save edits to disk and Clear" , "Save to disk & Clear",0} ,

 {"Show",AHIST_show_CB,NULL,
  "Run afni_history and show output" , "Run afni_history",0}
} ;

static void AHIST_make_widgets(void)
{
   XmString xstr ;
   Widget twid  ;

   /*** top level shell for window manager ***/

INFO_message("shell") ;
   shell =
      XtVaAppCreateShell(
           "AFNI" , "AFNI" , topLevelShellWidgetClass , dc->display ,

           XmNtitle             , histfilename , /* top of window */
           XmNiconName          , "History"    , /* label on icon */
           XmNdeleteResponse    , XmDO_NOTHING , /* deletion handled below */
           XmNallowShellResize  , True ,         /* let code resize shell? */
           XmNmappedWhenManaged , False ,        /* must map it manually */
      NULL ) ;

   DC_yokify( shell , dc ) ;

   if( afni48_good )             /* set icon pixmap */
      XtVaSetValues( shell ,
                        XmNiconPixmap , afni48_pixmap ,
                     NULL ) ;

   XmAddWMProtocolCallback(      /* make "Close" window menu work */
           shell ,
           XmInternAtom( dc->display , "WM_DELETE_WINDOW" , False ) ,
           AHIST_quit_CB , (XtPointer) plint ) ;

   /*** Vertical rowcol widget to hold all user interface stuff ***/

INFO_message("topman") ;
   topman = XtVaCreateWidget(
             "AFNI" , xmRowColumnWidgetClass , shell ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmVERTICAL ,
                XmNtraversalOn , True  ,
             NULL ) ;

   /*** horizontal rowcol to hold top row of controls ***/

   twid = XtVaCreateWidget(
             "AFNI" , xmRowColumnWidgetClass , topman ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmHORIZONTAL ,
                XmNtraversalOn , True  ,
             NULL ) ;

INFO_message("program_av") ;
   program_av = new_MCW_arrowval( twid , "Program" , MCW_AV_downup ,
                                  -999999,999999,0,0 ,
                                  MCW_AV_noactext , NULL,NULL,NULL,NULL ) ;

INFO_message("level_av") ;
   level_av = new_MCW_optmenu( twid , "Level" ,
                               0,NLEVEL-1 , 0,0 , NULL,NULL ,
                               MCW_av_substring_CB , level_labs ) ;

INFO_message("type_av") ;
   type_av = new_MCW_optmenu( twid , "Type" ,
                              0,NTYPE-1 , 0,0 , NULL,NULL ,
                              MCW_av_substring_CB , type_labs ) ;

   XtManageChild(twid) ;  /*** end of top row ***/

   /*** separator for visual neatness ***/

   (void)XtVaCreateManagedWidget(
              "AFNI" , xmSeparatorWidgetClass , topman ,
              XmNseparatorType , XmSINGLE_LINE , NULL ) ;

   /*** One liner text window ***/

INFO_message("oneline_av") ;
   oneline_av = new_MCW_arrowval( topman , "Note" , MCW_AV_downup ,
                                  -999999,999999,0,0 ,
                                  MCW_AV_noactext , NULL,NULL,NULL,NULL ) ;

   /*** separator for visual neatness ***/

   (void)XtVaCreateManagedWidget(
              "AFNI" , xmSeparatorWidgetClass , topman ,
              XmNseparatorType , XmSINGLE_LINE , NULL ) ;

   /*** longer text window for the long note ***/

INFO_message("verb label") ;
   xstr = XmStringCreateLtoR( "Verbose Text" , XmFONTLIST_DEFAULT_TAG ) ;
   (void)XtVaCreateManagedWidget( "AFNI" , xmLabelWidgetClass , topman ,
                                    XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

INFO_message("verbtext") ;
   verbtext = XtVaCreateManagedWidget(
                    "AFNI" , xmTextWidgetClass , topman ,
                     XmNeditMode               , XmMULTI_LINE_EDIT ,
                     XmNautoShowCursorPosition , True ,
                     XmNeditable               , True ,
                     XmNcursorPositionVisible  , True ,
                     XmNcolumns                , TWIDTH ,
                     XmNrows                   , THEIGHT,
                     XmNwordWrap               , True ,
                  NULL ) ;

   { char cbuf[TWIDTH+8] ; int ii ;
     XmFontList xflist ;

INFO_message("get width") ;
     for( ii=0; ii < TWIDTH+3; ii++ ) cbuf[ii] = 'x' ; cbuf[ii] = '\0' ;
     xstr = XmStringCreateLtoR( cbuf , XmFONTLIST_DEFAULT_TAG ) ;
     XtVaGetValues( verbtext , XmNfontList , &xflist , NULL ) ;
     text_width  = XmStringWidth ( xflist , xstr ) + 14 ;
     text_height = XmStringHeight( xflist , xstr ) ;
     XmStringFree( xstr ) ;
     ii = WidthOfScreen(XtScreen(shell)) - 128 ;
     if( text_width > ii ) text_width = ii ;
   }

   /*** separator for visual neatness ***/

   (void)XtVaCreateManagedWidget(
             "AFNI" , xmSeparatorWidgetClass , topman ,
              XmNseparatorType   , XmSINGLE_LINE , NULL ) ;

   /*** a set of action buttons below the line ***/

INFO_message("action area") ;
   (void)MCW_action_area( topman , AHIST_actor , NACT ) ;

   quit_pb  = (Widget) AHIST_actor[0].data ;
   clear_pb = (Widget) AHIST_actor[1].data ;
   save_pb  = (Widget) AHIST_actor[2].data ;
   show_pb  = (Widget) AHIST_actor[3].data ;

   /*** done ***/

INFO_message("manage") ;

   XtManageChild(topman) ;
   XtRealizeWidget(shell) ; NI_sleep(1) ;

INFO_message("unmanage") ;
   XtUnmanageChild(program_av->wup); XtUnmanageChild(program_av->wdown);
INFO_message("set width") ;
   /** XtVaSetValues(program_av->wtext,XmNcolumns,19,NULL) ; **/

INFO_message("unmanage") ;
   XtUnmanageChild(oneline_av->wup); XtUnmanageChild(oneline_av->wdown);
INFO_message("set width") ;
   /** XtVaSetValues(oneline_av->wtext,XmNcolumns,TWIDTH,NULL) ; **/

INFO_message("widgets done") ;
   return ;
}

/*-------------------------------------------------------------------*/

static int line_count( char * msg )
{
   char * cpt ; int nlin ;
   if( msg == NULL ) return 0 ;
   for( nlin=1,cpt=msg ; *cpt != '\0' ; cpt++ ) if( *cpt == '\n' ) nlin++ ;
   return nlin ;
}

/*-------------------------------------------------------------------
  Callback for quit button
---------------------------------------------------------------------*/

static void AHIST_quit_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   AHIST_clear_CB(NULL,NULL,NULL) ;
   XtUnmapWidget( shell ) ; editor_open = 0 ; 
   return ;
}

/*-------------------------------------------------------------------
  Callback for save button
---------------------------------------------------------------------*/

static void AHIST_save_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_invert_widget(w) ;
   MCW_invert_widget(w) ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for choose button.
---------------------------------------------------------------------*/

static void AHIST_clear_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_invert_widget(w) ;
   MCW_invert_widget(w) ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for show button
---------------------------------------------------------------------*/

static void AHIST_show_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_invert_widget(w) ;
   MCW_invert_widget(w) ;
   return ;
}
