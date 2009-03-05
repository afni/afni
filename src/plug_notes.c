/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to edit/view dataset notes.
  Makes a custom interface.
  01 Sep 1999 - RWCox
************************************************************************/

/*---------- prototypes for internal routines ----------*/

char * NOTES_main( PLUGIN_interface * ) ;

static void NOTES_make_widgets(void) ;

static void NOTES_done_CB   ( Widget , XtPointer , XtPointer ) ;
static void NOTES_help_CB   ( Widget , XtPointer , XtPointer ) ;
static void NOTES_quit_CB   ( Widget , XtPointer , XtPointer ) ;
static void NOTES_save_CB   ( Widget , XtPointer , XtPointer ) ;
static void NOTES_choose_CB ( Widget , XtPointer , XtPointer ) ;
static void NOTES_add_CB    ( Widget , XtPointer , XtPointer ) ;
static void NOTES_delete_CB ( Widget , XtPointer , XtPointer ) ;
static void NOTES_restore_CB( Widget , XtPointer , XtPointer ) ;
static void NOTES_refit_CB  ( Widget , XtPointer , XtPointer ) ;

static void NOTES_finalize_dset_CB( Widget , XtPointer , MCW_choose_cbs * ) ;

static PLUGIN_interface * plint = NULL ;

#define TWIDTH 75
#define newstring(str) strcpy(malloc(strlen(str)+1),str)
static int line_count( char * ) ;

#define THEIGHT 9
static int max_tlines = 0 ;

/***********************************************************************
   Set up the interface to the user.  Note that we bypass the
   normal interface creation, and simply have the menu selection
   directly call the main function, which will create a custom
   set of interface widgets the first time in.
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{

   if( ncall > 0 ) return NULL ;  /* only one interface */

   plint = PLUTO_new_interface( "Dataset NOTES" , NULL , NULL ,
                                PLUGIN_CALL_IMMEDIATELY , NOTES_main ) ;

   PLUTO_add_hint( plint , "Edit/View Notes attached to dataset" ) ;

   PLUTO_set_sequence( plint , "A:olddset:notes" ) ;

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
} NOTE_wind ;

static NOTE_wind * NOTES_make_note(void) ;

static Widget shell=NULL , topman , info_lab , choose_pb ;
static Widget done_pb , help_pb , quit_pb , save_pb , add_pb ;
static Widget notesw , noterc , refit_pb ;

static int text_width = 0 ;
static int text_height = 0 ;

static NOTE_wind ** notar = NULL ;
static int          notar_num = 0 ;
static int          num_notes = 0 ;

static MCW_DC * dc ;                 /* display context */
static Three_D_View * im3d ;         /* AFNI controller */
static THD_3dim_dataset * dset ;     /* The dataset!    */
static MCW_idcode         dset_idc ; /* 31 Mar 1999     */

static int editor_open  = 0 ;

#define NUM_DH 10
static char * default_history[NUM_DH] = {

   "The mind is its own place, and in itself\n"
   "Can make a Heaven of Hell, a Hell of Heaven.\n"
   "-- John Milton (Paradise Lost)\n" ,

   "Those who cannot remember the past are condemned to repeat it.\n"
   "-- George Santayana" ,

   "Chanter of Personality, outlining what is yet to be,\n"
   "I project the history of the future.\n"
   "-- Walt Whitman" ,

   "I shall cheerfully bear the reproach of having descended below the\n"
   "dignity of history.\n"
   "-- Thomas Macaulay" ,

   "There was a time - we see it in the marvellous dawn of Hellenic life -\n"
   "when history was distinguished neither from poetry, from mythology,\n"
   "nor from the first dim beginnings of science.\n"
   "-- Theodore Roosevelt" ,

   "You cannot escape the responsibility of tomorrow by evading it today.\n"
   "-- Abraham Lincoln" ,

   "Once you get into this great stream of history, you can't get out.\n"
   "-- Richard Nixon" ,

   "History is the version of past events that people have decided to\n"
   "agree upon.\n"
   "--Napoleon" ,

   "Ever returning Spring, trinity sure to me you bring:\n"
   "Lilac blooming perennial, and drooping star in the West,\n"
   "And thought of him I love.\n"
   "-- Walt Whitman" ,

   "What has occurred in this case, must ever recur in similar cases.\n"
   "Human nature will not change. In any future great national trial,\n"
   "compared with the men of this, we shall have as weak, and as strong;\n"
   "as silly and as wise; as bad and good. Let us, therefore, study the\n"
   "incidents of this, as philosophy to learn wisdom from, and none of\n"
   "them as wrongs to be revenged.\n"
   "-- Abraham Lincoln"

} ;

static char * empty_note = "-- Empty Note Text --" ;
static char * no_date    = "no date" ;

static void scroll_topbot( Widget , int ) ;

char * NOTES_main( PLUGIN_interface * plint )
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
      NOTES_make_widgets() ;
      PLUTO_set_topshell( plint , shell ) ;  /* 22 Sep 2000 */
      RWC_visibilize_widget( shell ) ;       /* 27 Sep 2000 */
   }

   /*-- unmanage all notes sub-windows --*/

   XtUnmanageChild( notesw ) ;
   XtUnmanageChild( noterc ) ;
   for( ii=0 ; ii < notar_num ; ii++ )
      XtUnmanageChild( notar[ii]->frame ) ;

   /*-- set titlebar --*/

   { char ttl[32] ;
     sprintf( ttl , "AFNI Notes %s" , AFNI_controller_label(im3d) ) ;
     XtVaSetValues( shell , XmNtitle , ttl , NULL ) ;
   }

   /*-- set the info label --*/

   xstr = XmStringCreateLtoR( "[No dataset]" ,
                              XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( info_lab , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   /*-- pop the widget up --*/

   XtMapWidget(shell) ;
   PLUTO_cursorize(shell) ;

   /*-- misc initialization --*/

   dset         = NULL ;   /* not editing anything   */
   editor_open  = 1 ;      /* editor is now open for business */
   num_notes    = 0 ;      /* don't have any notes now */

   return NULL ;
}

/*------------------------------------------------------------------------
  Make the control popup window for this beast
--------------------------------------------------------------------------*/

/*-- structures defining action buttons (at bottom of popup) --*/

#define NACT 6  /* number of action buttons */

static MCW_action_item NOTES_actor[NACT] = {
 {"Quit",NOTES_quit_CB,NULL,
  "Discard edits since last\nSave and close Editor" ,
   "Discard edits and close",0} ,

 {"Help",NOTES_help_CB,NULL,
  "Displays more help" , "Displays more help",0} ,

 {"Add",NOTES_add_CB,NULL,
  "Add a new Note to the\nlist (at the end)", "Add a new Note" , 0 } ,

 {"Refit",NOTES_refit_CB,NULL,
  "Resize the Notes' sub-windows\nto fit the Notes' texts." ,
  "Resize Notes sub-windows" , 0 } ,

 {"Save",NOTES_save_CB,NULL,
  "Save edits to disk\nand continue" , "Save to disk; continue",0} ,

 {"Done",NOTES_done_CB,NULL,
  "Save edits to disk\nand close Editor" , "Save and Quit",1}
} ;

static void NOTES_make_widgets(void)
{
   XmString xstr ;
   Widget twid , tsep ;

   /*** get max # of text lines ***/

   if( max_tlines < 1 ){
      char * cc = my_getenv("AFNI_NOTES_DLINES") ;
      if( cc != NULL ) max_tlines = strtol( cc , NULL , 10 ) ;

           if( max_tlines <  1 ) max_tlines = THEIGHT ;
      else if( max_tlines > 99 ) max_tlines = 99 ;     /* 15 Sep 1999 */
   }

   /*** top level shell for window manager ***/

   shell =
      XtVaAppCreateShell(
           "AFNI" , "AFNI" , topLevelShellWidgetClass , dc->display ,

           XmNtitle             , "Notes Editor" , /* top of window */
           XmNiconName          , "Notes"        , /* label on icon */
           XmNdeleteResponse    , XmDO_NOTHING   , /* deletion handled below */
           XmNallowShellResize  , True ,           /* let code resize shell? */
           XmNmappedWhenManaged , False ,          /* must map it manually */
      NULL ) ;

   DC_yokify( shell , dc ) ;

   if( afni48_good )             /* set icon pixmap */
      XtVaSetValues( shell ,
                        XmNiconPixmap , afni48_pixmap ,
                     NULL ) ;

   XmAddWMProtocolCallback(      /* make "Close" window menu work */
           shell ,
           XmInternAtom( dc->display , "WM_DELETE_WINDOW" , False ) ,
           NOTES_quit_CB , (XtPointer) plint ) ;

   /*** Form widget to hold all user interface stuff ***/

   topman = XtVaCreateWidget(
                "AFNI" , xmFormWidgetClass , shell ,
                  XmNborderWidth , 0 ,
                  XmNborderColor , 0 ,
                  XmNtraversalOn , True  ,
                NULL ) ;

   /*** horizontal rowcol to hold top row of controls ***/

   twid = XtVaCreateWidget(
             "AFNI" , xmRowColumnWidgetClass , topman ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmHORIZONTAL ,
                XmNtraversalOn , True  ,
                XmNleftAttachment , XmATTACH_FORM ,
                XmNrightAttachment, XmATTACH_FORM ,
                XmNtopAttachment  , XmATTACH_FORM ,
                XmNtopOffset      , 1 ,
             NULL ) ;

   /*** button to let user choose dataset to edit ***/

   xstr = XmStringCreateLtoR( "Choose Dataset" , XmFONTLIST_DEFAULT_TAG ) ;
   choose_pb = XtVaCreateManagedWidget(
                  "AFNI" , xmPushButtonWidgetClass , twid ,
                     XmNlabelString , xstr ,
                     XmNtraversalOn , True  ,
                  NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( choose_pb, XmNactivateCallback, NOTES_choose_CB, NULL ) ;
   MCW_register_help( choose_pb ,
                      "Use this to popup a\n"
                      "'chooser' that lets\n"
                      "you select which\n"
                      "dataset to edit."
                    ) ;
   MCW_register_hint( choose_pb , "Popup a dataset chooser" ) ;

   /*** label at top to let user know who we are ***/

   xstr = XmStringCreateLtoR( "[No dataset]" , XmFONTLIST_DEFAULT_TAG ) ;
   info_lab = XtVaCreateManagedWidget(
                 "AFNI" , xmLabelWidgetClass , twid ,
                    XmNlabelString , xstr ,
                 NULL ) ;
   XmStringFree(xstr) ;
   MCW_register_help( info_lab , "Shows dataset being edited" ) ;
   MCW_register_hint( info_lab , "Dataset being edited" ) ;

   XtManageChild(twid) ;  /*** end of top row ***/

   /*** separator for visual neatness ***/

   tsep   = XtVaCreateManagedWidget(
             "AFNI" , xmSeparatorWidgetClass , topman ,
                XmNseparatorType , XmSINGLE_LINE ,
                XmNleftAttachment  , XmATTACH_FORM ,
                XmNrightAttachment , XmATTACH_FORM ,
                XmNtopAttachment   , XmATTACH_WIDGET ,
                XmNtopWidget       , info_lab ,
                XmNtopOffset       , 1 ,
             NULL ) ;

   /*** a set of action buttons below the line ***/

   twid = MCW_action_area( topman , NOTES_actor , NACT ) ;

   XtVaSetValues( twid ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNrightAttachment, XmATTACH_FORM ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopWidget      , tsep   ,
                     XmNtopOffset      , 1 ,
                  NULL ) ;

   quit_pb  = (Widget) NOTES_actor[0].data ;
   help_pb  = (Widget) NOTES_actor[1].data ;
   add_pb   = (Widget) NOTES_actor[2].data ;
   refit_pb = (Widget) NOTES_actor[3].data ;
   save_pb  = (Widget) NOTES_actor[4].data ;
   done_pb  = (Widget) NOTES_actor[5].data ;

   /*** separator for visual neatness ***/

   tsep   = XtVaCreateManagedWidget(
              "AFNI" , xmSeparatorWidgetClass , topman ,
                 XmNseparatorType , XmSINGLE_LINE ,
                 XmNleftAttachment  , XmATTACH_FORM ,
                 XmNrightAttachment , XmATTACH_FORM ,
                 XmNtopAttachment   , XmATTACH_WIDGET ,
                 XmNtopWidget       , twid ,
                 XmNtopOffset       , 1 ,
              NULL ) ;

   /*** a scrolled window + rowcol to hold all the Notes windows ***/

   notesw = XtVaCreateWidget(
                    "AFNI" , xmScrolledWindowWidgetClass , topman ,
                       XmNscrollingPolicy        , XmAUTOMATIC ,
                       XmNvisualPolicy           , XmCONSTANT ,
                       XmNshadowThickness        , 0 ,
                       XmNscrollBarDisplayPolicy , XmAS_NEEDED ,
                       XmNscrollBarPlacement     , XmTOP_LEFT ,
                       XmNleftAttachment  , XmATTACH_FORM ,
                       XmNrightAttachment , XmATTACH_FORM ,
                       XmNbottomAttachment, XmATTACH_FORM ,
                       XmNtopAttachment   , XmATTACH_WIDGET ,
                       XmNtopWidget       , tsep   ,
                       XmNtopOffset       , 1 ,
                    NULL ) ;

   noterc = XtVaCreateWidget(
             "AFNI" , xmRowColumnWidgetClass , notesw ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmVERTICAL ,
                XmNmarginHeight, 0 ,
                XmNmarginWidth , 0 ,
                XmNtraversalOn , True  ,
             NULL ) ;

   /**** make an initial Note window
         and modify it to be readonly - for the History ****/

   notar     = (NOTE_wind **) malloc( sizeof(NOTE_wind *) ) ;
   notar[0]  = NOTES_make_note() ;
   notar_num = 1 ;

   XtUnmanageChild( notar[0]->delete_pb  ) ;  /* these actions not   */
   XtUnmanageChild( notar[0]->restore_pb ) ;  /* allowed for History */
   XtVaSetValues( notar[0]->textw ,
                    XmNautoShowCursorPosition , False ,
                    XmNeditable               , False ,
                    XmNcursorPositionVisible  , False ,
                  NULL ) ;
   xstr = XmStringCreateLtoR( "----- History -----" , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( notar[0]->note_label , XmNlabelString , xstr , NULL ) ;
   MCW_register_hint( notar[0]->textw , "Dataset History; or Edifying Text" ) ;

   /*** compute width of popup window ***/

   { char cbuf[TWIDTH+8] ; int ii ;
     XmFontList xflist=(XmFontList)NULL ;

     for( ii=0; ii < TWIDTH+3; ii++ ) cbuf[ii] = 'x' ; cbuf[ii] = '\0' ;
     xstr = XmStringCreateLtoR( cbuf , XmFONTLIST_DEFAULT_TAG ) ;
     XtVaGetValues( notar[0]->textw , XmNfontList , &xflist , NULL ) ;
     text_width  = XmStringWidth ( xflist , xstr ) + 14 ;
     text_height = XmStringHeight( xflist , xstr ) ;
     XmStringFree( xstr ) ;
     ii = WidthOfScreen(XtScreen(shell)) - 128 ;
     if( text_width > ii ) text_width = ii ;
   }

   /*** done ***/

   XtManageChild(topman) ;
   XtRealizeWidget(shell) ; NI_sleep(1) ; /* will not be mapped */

   return ;
}

/*-------------------------------------------------------------------
   make a new Note widget structure (with nothing in it)
---------------------------------------------------------------------*/

static NOTE_wind * NOTES_make_note( void )
{
   NOTE_wind * nw ;
   XmString xstr ;

   nw = (NOTE_wind *) calloc( 1 , sizeof(NOTE_wind) ) ;

   nw->frame = XtVaCreateWidget(
                  "AFNI" , xmFrameWidgetClass , noterc ,
                  XmNshadowType , XmSHADOW_IN ,
                  XmNshadowThickness , 1 ,
                  XmNtraversalOn , True  ,
               NULL ) ;

   nw->vert_rc = XtVaCreateWidget(
                    "AFNI" , xmRowColumnWidgetClass , nw->frame ,
                       XmNpacking     , XmPACK_TIGHT ,
                       XmNorientation , XmVERTICAL ,
                       XmNtraversalOn , True  ,
                       XmNmarginHeight, 0 ,
                       XmNmarginWidth , 0 ,
                    NULL ) ;

   nw->horz_rc = XtVaCreateWidget(
                    "AFNI" , xmRowColumnWidgetClass , nw->vert_rc ,
                       XmNpacking     , XmPACK_TIGHT ,
                       XmNorientation , XmHORIZONTAL ,
                       XmNtraversalOn , True  ,
                       XmNmarginHeight, 0 ,
                       XmNmarginWidth , 0 ,
                    NULL ) ;

   nw->note_label = XtVaCreateManagedWidget(
                       "AFNI" , xmLabelWidgetClass , nw->horz_rc ,
                       XmNmarginHeight, 0 ,
                       XmNmarginWidth , 0 ,
                       NULL ) ;

   xstr = XmStringCreateLtoR( "Delete" , XmFONTLIST_DEFAULT_TAG ) ;
   nw->delete_pb = XtVaCreateManagedWidget(
                      "AFNI" , xmPushButtonWidgetClass , nw->horz_rc ,
                         XmNlabelString , xstr ,
                         XmNtraversalOn , True  ,
                         XmNmarginHeight, 0 ,
                         XmNmarginWidth , 0 ,
                      NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( nw->delete_pb, XmNactivateCallback, NOTES_delete_CB, NULL ) ;
   MCW_register_help( nw->delete_pb ,
                      "Use this button to delete\n"
                      "this Note from the dataset."
                    ) ;
   MCW_register_hint( nw->delete_pb , "Delete this Note" ) ;

   xstr = XmStringCreateLtoR( "Restore" , XmFONTLIST_DEFAULT_TAG ) ;
   nw->restore_pb = XtVaCreateManagedWidget(
                      "AFNI" , xmPushButtonWidgetClass , nw->horz_rc ,
                         XmNlabelString , xstr ,
                         XmNtraversalOn , True  ,
                         XmNmarginHeight, 0 ,
                         XmNmarginWidth , 0 ,
                      NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( nw->restore_pb, XmNactivateCallback, NOTES_restore_CB, NULL ) ;
   MCW_register_help( nw->restore_pb ,
                      "Use this button to restore\n"
                      "this Note to its original\n"
                      "value (i.e., as it currently\n"
                      "stored on in the HEAD file)."
                    ) ;
   MCW_register_hint( nw->restore_pb , "Restore Note from disk" ) ;

   XtManageChild(nw->horz_rc) ;

   (void) XtVaCreateManagedWidget(
             "AFNI" , xmSeparatorWidgetClass , nw->vert_rc ,
                XmNseparatorType , XmSINGLE_LINE ,
             NULL ) ;

   nw->scrollw = XtVaCreateManagedWidget(
                    "AFNI" , xmScrolledWindowWidgetClass , nw->vert_rc ,
                       XmNscrollingPolicy        , XmAPPLICATION_DEFINED ,
                       XmNvisualPolicy           , XmVARIABLE ,
                       XmNshadowThickness        , 0 ,
                    NULL ) ;

   nw->textw = XtVaCreateManagedWidget(
                    "AFNI" , xmTextWidgetClass , nw->scrollw ,
                       XmNeditMode               , XmMULTI_LINE_EDIT ,
                       XmNautoShowCursorPosition , True ,
                       XmNeditable               , True ,
                       XmNcursorPositionVisible  , True ,
                       XmNcolumns                , TWIDTH ,
                       XmNrows                   , 1    ,
                       XmNwordWrap               , True ,
                    NULL ) ;

   XtManageChild(nw->vert_rc) ;  /* we DON'T manage the frame */

   nw->note_orig = nw->date_orig = NULL ;
   return nw ;
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
  Callback for done button
---------------------------------------------------------------------*/

static void NOTES_done_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( dset != NULL && num_notes > 0 ) NOTES_save_CB(NULL,NULL,NULL) ;
   NOTES_quit_CB(NULL,NULL,NULL) ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for quit button
---------------------------------------------------------------------*/

static void NOTES_quit_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   int ii ;

   XtUnmapWidget( shell ) ; editor_open = 0 ; dset = NULL ;

   XtUnmanageChild( notesw ) ;
   XtUnmanageChild( noterc ) ;
   for( ii=0 ; ii < notar_num ; ii++ ){
      XtUnmanageChild( notar[ii]->frame ) ;

      if( notar[ii]->note_orig != NULL ){
          free(notar[ii]->note_orig) ; notar[ii]->note_orig = NULL ;
      }

      if( notar[ii]->date_orig != NULL ){
          free(notar[ii]->date_orig) ; notar[ii]->date_orig = NULL ;
      }

      XmTextSetString( notar[ii]->textw , "\0" ) ;  /* to save some space */
   }

   return ;
}

/*-------------------------------------------------------------------
  Callback for save button
---------------------------------------------------------------------*/

static void NOTES_save_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   int ii , nnew ;
   char * nstr , str[256] ;
   XmString xstr ;

   if( dset == NULL || num_notes == 0 ){ XBell(dc->display,100); return; }

   MCW_invert_widget(save_pb) ;

   /* copy notes in windows into dataset, if they are changed */

   for( nnew=0,ii=1 ; ii <= num_notes ; ii++ ){
      nstr = XmTextGetString( notar[ii]->textw ) ;  /* get text in window */
      if( strcmp(nstr,notar[ii]->note_orig) != 0 ){ /* compare to original */

         tross_Store_Note( dset , ii , nstr ) ; nnew++ ;  /* save it */
         free( notar[ii]->note_orig ) ;                   /* make new copy */
         notar[ii]->note_orig = newstring(nstr) ;
         free( notar[ii]->date_orig ) ;                   /* re-fetch date */
         notar[ii]->date_orig = tross_Get_Notedate( dset , ii ) ;

         sprintf(str,"----- NOTE %d [%s] -----",ii,notar[ii]->date_orig) ;
         xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
         XtVaSetValues( notar[ii]->note_label , XmNlabelString , xstr , NULL ) ;
         XmStringFree(xstr) ;
      }
      XtFree(nstr) ;
   }

   /* now save notes */

   if( nnew > 0 ){
      putenv("AFNI_DECONFLICT=OVERWRITE") ;
      DSET_overwrite_header(dset) ;
   }
   else if( w != NULL )
      (void) MCW_popup_message( save_pb ,
                                " \n Nothing has changed! \n " ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;

   MCW_invert_widget(save_pb) ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for help button
---------------------------------------------------------------------*/

static void NOTES_help_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   (void ) new_MCW_textwin( choose_pb ,

   " \n"
   "This plugin is used to view and edit the Notes attached to a dataset.\n"
   "---------------------------------------------------------------------\n"
   "The buttons at the top perform the following functions:\n"
   "\n"
   "  Choose Dataset: Use this to select a dataset to deal with.\n"
   "\n"
   "  Quit:           Exit the plugin without saving any edits to Notes\n"
   "                  made since the last 'Save' button press.\n"
   "\n"
   "  Help:           I hope you already got the idea for this widget.\n"
   "\n"
   "  Add:            Add a new Note at the end of the existing notes.\n"
   "\n"
   "  Refit:          Resize all Notes' sub-windows to fit the number of\n"
   "                  lines of text in each Note.  The maximum line count\n"
   "                  for each sub-window is set by the Unix environment\n"
   "                  variable AFNI_NOTES_DLINES; if this is not defined,\n"
   "                  the default maximum is 9.\n"
   "\n"
   "  Save:           Save the current Notes to the dataset .HEAD file.\n"
   "\n"
   "  Done:           Save then Quit.\n"
   "-------------------------------------------------------------------------- \n"
   "Below these buttons are the Notes sub-windows.\n"
   "\n"
   "The first Note sub-window shows the dataset History Note.  This Note can't\n"
   "be edited by the user - it is created by each AFNI program as a record of\n"
   "the actions that led to this dataset. If no History Note is present in the\n"
   "dataset .HEAD file, some edifying text will be shown here instead.\n"
   "\n"
   "Each other Note sub-window shows the Note number, the date of the Note's\n"
   "creation (or last change), and the Note itself.  These windows are\n"
   "editable.  If you want to remove a Note from the dataset, use the\n"
   "Delete button for that Note.  (This operation is NOT reversible - using\n"
   "the Quit button will not get a deleted Note back!)  If you edit a note\n"
   "badly and want to restore it to the value saved in the dataset header,\n"
   "use the Restore button.\n"
   "\n"
   "Notes can also be viewed with the program 3dinfo, and with the Info\n"
   "buttons from the Datamode/Misc menu.  The command line program 3dNotes\n"
   "lets you create dataset Notes in a batch script file.\n"
   "\n"
   "--- Bob Cox - September 1999\n"
   "    (Based on ideas and some code from Tom Ross of MCW)\n"

    , TEXT_READONLY ) ;

   return ;
}

/*-------------------------------------------------------------------
  Callback for choose button.
    - must be in current session
  Much of this code is adapted from PLUG_choose_dataset_CB.
---------------------------------------------------------------------*/

static int                  ndsl = 0 ;
static PLUGIN_dataset_link * dsl = NULL ;

static void NOTES_choose_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   THD_session * ss  = im3d->ss_now ;           /* current session */
   int           vv  = im3d->vinfo->view_type ; /* view type */
   THD_3dim_dataset * qset ;
   int id , ltop , llen ;
   char qnam[THD_MAX_NAME] , label[THD_MAX_NAME] ;
   static char ** strlist = NULL ;

   /* initialize */

   ndsl = 0 ;

   /* scan datasets */

   for( id=0 ; id < ss->num_dsset ; id++ ){
      qset = ss->dsset[id][vv] ;

      if( ! ISVALID_DSET (qset) ) continue ;  /* skip */

      ndsl++ ;
      dsl = (PLUGIN_dataset_link *)
              XtRealloc( (char *) dsl , sizeof(PLUGIN_dataset_link)*ndsl ) ;

      make_PLUGIN_dataset_link( qset , dsl + (ndsl-1) ) ;
   }

   /* found nothing?  exit */

   if( ndsl < 1 ){
      (void) MCW_popup_message( choose_pb ,
                                   "Didn't find any\ndatasets to edit!" ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
      XBell( dc->display , 100 ) ;
      return ;
   }

   /*--- 23 Nov 1996: loop over dataset links and patch their titles
                      to include an indicator of the dataset type    ---*/

   ltop = 4 ;
   for( id=0 ; id < ndsl ; id++ ){
      llen = strlen(dsl[id].title) ;
      ltop = MAX(ltop,llen) ;
   }

   for( id=0 ; id < ndsl ; id++ ){
      qset = PLUTO_find_dset( &(dsl[id].idcode) ) ;
      if( ! ISVALID_DSET(qset) ) continue ;
      if( ISANAT(qset) ){
         if( ISANATBUCKET(qset) )         /* 30 Nov 1997 */
            sprintf(qnam,"%-*s [%s:%d]" ,
                    ltop,dsl[id].title ,
                    ANAT_prefixstr[qset->func_type] , DSET_NVALS(qset) ) ;

         else if( DSET_NUM_TIMES(qset) == 1 )
            sprintf(qnam,"%-*s [%s]" ,
                    ltop,dsl[id].title ,
                    ANAT_prefixstr[qset->func_type] ) ;

         else
            sprintf(qnam,"%-*s [%s:3D+t:%d]" ,
                    ltop,dsl[id].title ,
                    ANAT_prefixstr[qset->func_type] , DSET_NUM_TIMES(qset) ) ;

      } else {
         if( ISFUNCBUCKET(qset) )         /* 30 Nov 1997 */
            sprintf(qnam,"%-*s [%s:%d]" ,
                    ltop,dsl[id].title ,
                    FUNC_prefixstr[qset->func_type] , DSET_NVALS(qset) ) ;

         else if( DSET_NUM_TIMES(qset) == 1 )
            sprintf(qnam,"%-*s [%s]" ,
                    ltop,dsl[id].title ,
                    FUNC_prefixstr[qset->func_type] ) ;

         else
            sprintf(qnam,"%-*s [%s:3D+t:%d]" ,
                    ltop,dsl[id].title ,
                    FUNC_prefixstr[qset->func_type] , DSET_NVALS(qset) ) ;
      }

      if( DSET_COMPRESSED(qset) ) strcat(qnam,"z") ;

      strcpy( dsl[id].title , qnam ) ;
   }

   /*--- make a popup chooser for the user to browse ---*/

   POPDOWN_strlist_chooser ;

   strlist = (char **) XtRealloc( (char *)strlist , sizeof(char *)*ndsl ) ;
   for( id=0 ; id < ndsl ; id++ ) strlist[id] = dsl[id].title ;

   sprintf( label , "AFNI Dataset from\nthe %s" , VIEW_typestr[vv] ) ;

   MCW_choose_strlist( w , label , ndsl , -1 , strlist ,
                       NOTES_finalize_dset_CB , NULL     ) ;

   return ;
}

/*----------------------------------------------------------------------------*/

static void NOTES_finalize_dset_CB( Widget w, XtPointer fd, MCW_choose_cbs * cbs )
{
   int id = cbs->ival ;
   THD_3dim_dataset * qset ;
   XmString xstr ;
   char str[256] , * his ;
   int ii , nl , nltot , ww,hh,qh ;

   /* check for errors */

   if( ! editor_open ){ POPDOWN_strlist_chooser; XBell(dc->display,100); return; }

   if( id < 0 || id >= ndsl ){ XBell(dc->display,100) ; return ; }

   qset = PLUTO_find_dset( &(dsl[id].idcode) ) ;  /* the new dataset? */

   if( qset == NULL ){ XBell(dc->display,100) ; return ; }

   /* accept this dataset */

   dset     = qset ;
   dset_idc = qset->idcode ;   /* 31 Mar 1999 */

   /* write the informational label */

   strcpy(str,dsl[id].title) ;
   xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( info_lab , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   /* clear any existing notes windows */

   XtUnmanageChild( notesw ) ;
   XtUnmanageChild( noterc ) ;
   for( ii=0 ; ii < notar_num ; ii++ ){
      XtUnmanageChild( notar[ii]->frame ) ;

      if( notar[ii]->note_orig != NULL ){
          free(notar[ii]->note_orig) ; notar[ii]->note_orig = NULL ;
      }

      if( notar[ii]->date_orig != NULL ){
          free(notar[ii]->date_orig) ; notar[ii]->date_orig = NULL ;
      }
   }

   /* make new Notes windows, if necessary */

   num_notes = tross_Get_Notecount( dset ) ;

   if( notar_num < num_notes + 1 ){
      notar = (NOTE_wind **) realloc( notar, sizeof(NOTE_wind *)*(num_notes+1) ) ;
      for( ii=notar_num ; ii <= num_notes ; ii++ )
         notar[ii]  = NOTES_make_note() ;
      notar_num = num_notes + 1 ;
   }

   /* get and set Notes text for each window */

   his = tross_Get_History( dset ) ;
   if( his == NULL ){
      ii = ( lrand48() >> 8) % NUM_DH ;
      notar[0]->note_orig = newstring(default_history[ii]) ;
      xstr = XmStringCreateLtoR( "----- EDIFYING TEXT -----" , XmFONTLIST_DEFAULT_TAG ) ;
   } else {
      notar[0]->note_orig = tross_breakup_string( his , 2*TWIDTH/3 , TWIDTH-1 ) ;
      free(his) ;
      xstr = XmStringCreateLtoR( "----- HISTORY -----" , XmFONTLIST_DEFAULT_TAG ) ;
   }
   notar[0]->date_orig = NULL ;

   XtVaSetValues( notar[0]->note_label , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;
   XmTextSetString( notar[0]->textw , notar[0]->note_orig ) ;

   nl = line_count( notar[0]->note_orig ) ;
   if( nl > max_tlines ) nl = max_tlines ;
   XtVaSetValues( notar[0]->textw , XmNrows , nl , NULL ) ;
   nltot = nl ;

   XtManageChild( notar[0]->frame ) ;

   for( ii=1 ; ii <= num_notes ; ii++ ){
      notar[ii]->note_orig = tross_Get_Note    ( dset , ii ) ;
      notar[ii]->date_orig = tross_Get_Notedate( dset , ii ) ;

      if( notar[ii]->note_orig == NULL )
         notar[ii]->note_orig = newstring( empty_note ) ;

      if( notar[ii]->date_orig == NULL )
         notar[ii]->date_orig = newstring( no_date ) ;

      sprintf(str,"----- NOTE %d [%s] -----",ii,notar[ii]->date_orig) ;
      xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
      XtVaSetValues( notar[ii]->note_label , XmNlabelString , xstr , NULL ) ;
      XmStringFree(xstr) ;
      XmTextSetString( notar[ii]->textw , notar[ii]->note_orig ) ;

      nl = line_count( notar[ii]->note_orig ) ;
      if( nl > max_tlines ) nl = max_tlines ;
      XtVaSetValues( notar[ii]->textw , XmNrows , nl , NULL ) ;
      nltot += nl ;

      XtManageChild( notar[ii]->frame ) ;
   }

   XtVaSetValues( notar[0]->scrollw , XmNwidth , text_width , NULL ) ;

   XtManageChild( noterc ) ;
   XtManageChild( notesw ) ;

   /* set size of scrolling area for notes */

   MCW_widget_geom( noterc , &ww,&hh , NULL,NULL ) ;
   ww += 4 ; hh += 4 ; qh = hh ;
   if( ww > dc->width - 128 ) ww = dc->width - 128 ;
   if( hh > dc->height- 128 ) hh = dc->height- 128 ;

   XtVaSetValues( notesw , XmNwidth,ww , XmNheight,hh , NULL ) ;

   if( qh > hh ) scroll_topbot( notesw , 0 ) ;
   return ;
}

/*----------------------------------------------------------------------------*/

static void NOTES_add_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   int nl , hh , ii , qh ;
   char str[256] ;
   XmString xstr ;

   if( dset == NULL ){ XBell(dc->display,100) ; return ; }

   if( num_notes >= MAX_DSET_NOTES ){
      (void) MCW_popup_message( add_pb ,
                                " \n"
                                " Max number of notes\n"
                                " would be exceeded!\n " ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
      XBell(dc->display,100) ; return ;
   }

   tross_Add_Note( dset , empty_note ) ;

   num_notes ++ ;

   if( notar_num < num_notes + 1 ){
      notar = (NOTE_wind **) realloc( notar, sizeof(NOTE_wind *)*(num_notes+1) ) ;
      for( ii=notar_num ; ii <= num_notes ; ii++ )
         notar[ii]  = NOTES_make_note() ;
      notar_num = num_notes + 1 ;
   }

   notar[num_notes]->note_orig = tross_Get_Note    ( dset , num_notes ) ;
   notar[num_notes]->date_orig = tross_Get_Notedate( dset , num_notes ) ;

   if( notar[num_notes]->note_orig == NULL )
      notar[num_notes]->note_orig = newstring( empty_note ) ;

   if( notar[num_notes]->date_orig == NULL )
      notar[num_notes]->date_orig = newstring( no_date ) ;

   sprintf(str,"----- NOTE %d [%s] -----",num_notes,notar[num_notes]->date_orig) ;
   xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( notar[num_notes]->note_label , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;
   XmTextSetString( notar[num_notes]->textw , notar[num_notes]->note_orig ) ;

   nl = line_count( notar[num_notes]->note_orig ) ;
   if( nl > max_tlines ) nl = max_tlines ;
   XtVaSetValues( notar[num_notes]->textw , XmNrows , nl , NULL ) ;

   XtManageChild( notar[num_notes]->frame ) ;

   /* set size of scrolling area for notes */

   MCW_widget_geom( noterc , NULL,&hh , NULL,NULL ) ; hh +=4 ; qh = hh ;
   if( hh > dc->height- 128 ) hh = dc->height- 128 ;

   XtVaSetValues( notesw , XmNheight,hh , NULL ) ;

   if( qh > hh ) scroll_topbot( notesw , 1 ) ;
   return ;
}

/*----------------------------------------------------------------------------*/

static void NOTES_refit_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   int ii , hh , nl , qh ;
   char * ts ;

   if( dset == NULL ){ XBell(dc->display,100) ; return ; }

   for( ii=0 ; ii <= num_notes ; ii++ ){
      ts =  XmTextGetString( notar[ii]->textw ) ;  /* get text in window */
      nl = line_count( ts ) ; XtFree( ts ) ;       /* count lines */
      if( nl > max_tlines ) nl = max_tlines ;
      XtVaSetValues( notar[ii]->textw , XmNrows , nl , NULL ) ;
   }

   /* set size of scrolling area for notes */

   MCW_widget_geom( noterc , NULL,&hh , NULL,NULL ) ; hh += 4 ; qh = hh ;
   if( hh > dc->height- 128 ) hh = dc->height- 128 ;

   XtVaSetValues( notesw , XmNheight,hh , NULL ) ;
   return ;
}

/*----------------------------------------------------------------------------*/

static void NOTES_delete_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   int ii , kk , hh , nl ;
   char str[256] , * ts ;
   XmString xstr ;

   if( dset == NULL ) return ;    /* should never happen */

   for( kk=1 ; kk <= num_notes ; kk++ )
      if( w == notar[kk]->delete_pb ) break ;

   if( kk > num_notes ) return ;  /* should never happen */

   tross_Delete_Note( dset , kk ) ;  /* delete it in the dataset */

   /* erase the info in the kk-th Note window */

   free( notar[kk]->note_orig ) ;  /* this is history */
   free( notar[kk]->date_orig ) ;

   /* move the info in the Note windows above kk down to the previous window */

   for( ii=kk+1 ; ii <= num_notes ; ii++ ){
      notar[ii-1]->note_orig = notar[ii]->note_orig ;  /* move the orig */
      notar[ii-1]->date_orig = notar[ii]->date_orig ;  /* stuff down    */

      ts = XmTextGetString( notar[ii]->textw ) ;       /* move the text */
      XmTextSetString( notar[ii-1]->textw , ts ) ;     /* in the window */
      nl = line_count( ts ) ;
      if( nl > max_tlines ) nl = max_tlines ;
      XtVaSetValues( notar[ii-1]->textw , XmNrows , nl , NULL ) ;
      XtFree(ts) ;

      sprintf(str,"----- NOTE %d [%s] -----",ii-1,notar[ii-1]->date_orig) ;
      xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
      XtVaSetValues( notar[ii-1]->note_label , XmNlabelString , xstr , NULL ) ;
      XmStringFree(xstr) ;
   }

   notar[num_notes]->note_orig = NULL ;
   notar[num_notes]->date_orig = NULL ;
   XmTextSetString( notar[num_notes]->textw , "\0" ) ;
   XtUnmanageChild( notar[num_notes]->frame ) ;
   num_notes-- ;

   MCW_widget_geom( noterc , NULL,&hh , NULL,NULL ) ;
   if( hh > dc->height- 128 ) hh = dc->height- 128 ;
   XtVaSetValues( notesw , XmNheight,hh , NULL ) ;

   return ;
}

/*----------------------------------------------------------------------------*/

static void NOTES_restore_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   int kk ;

   if( dset == NULL ) return ;    /* should never happen */

   for( kk=1 ; kk <= num_notes ; kk++ )
      if( w == notar[kk]->restore_pb ) break ;

   if( kk > num_notes ) return ;  /* should never happen */

   XmTextSetString( notar[kk]->textw , notar[kk]->note_orig ) ;
   return ;
}

/*----------------------------------------------------------------------------*/

static void scroll_topbot( Widget sw , int where )
{
   Widget sb=NULL ;
   int val,siz,inc,pag , smin=0,smax=0 ;

   if( sw == NULL ) return ;

   XtVaGetValues( sw ,
                     XmNverticalScrollBar , &sb ,
                     XmNmaximum           , &smax ,
                     XmNminimum           , &smin ,
                  NULL ) ;
   if( sb == NULL ) return ;

   XmScrollBarGetValues( sb , &val,&siz,&inc,&pag ) ;

   if( where == 0 ) val = smin ;  /* to top */
   else             val = smax ;  /* to bot */

   XmScrollBarSetValues( sb , val,siz,inc,pag , True ) ;
   return ;
}
