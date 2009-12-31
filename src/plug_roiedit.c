/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
/***********************************************************************
 *
 * plug_roiedit.c               - a region of interest editor (afni plugin)
 *
 * Rick Reynolds
 * Medical College of WI
 *
 *************
 * history:
 *
 * October 7, 2003  [rickr]
 *   - renamed old and new fields of r_alg_s to Bold and Bnew
 * June 10, 2005 [rickr]
 *   - added lots of ENTRY/RETURN pairs
 *   - continue on 'set fill point' with no data
 *   - use DSET_load in case use has not opened afni windows yet
 ***********************************************************************
 */

#include <Xm/FileSB.h>

#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

#include "plug_roiedit.h"               /* RCR stuff */

/***********************************************************************
  Plugin to draw values into a dataset.
  Makes a custom interface.
************************************************************************/

/*---------- prototypes for internal routines ----------*/

static char * DRAW_main( PLUGIN_interface * ) ;

static void DRAW_make_widgets(void) ;

static void DRAW_done_CB  ( Widget , XtPointer , XtPointer ) ;
static void DRAW_undo_CB  ( Widget , XtPointer , XtPointer ) ;
static void DRAW_help_CB  ( Widget , XtPointer , XtPointer ) ;
static void DRAW_quit_CB  ( Widget , XtPointer , XtPointer ) ;
static void DRAW_save_CB  ( Widget , XtPointer , XtPointer ) ;
static void DRAW_choose_CB( Widget , XtPointer , XtPointer ) ;
static void DRAW_color_CB ( MCW_arrowval * , XtPointer ) ;
static void DRAW_mode_CB  ( MCW_arrowval * , XtPointer ) ;
static void DRAW_value_CB ( MCW_arrowval * , XtPointer ) ;

static void DRAW_receiver( int , int , void * , void * ) ;
static void DRAW_into_dataset( int , int * , int * , int * , void * ) ;
static void DRAW_finalize_dset_CB( Widget , XtPointer , MCW_choose_cbs * ) ;
static void DRAW_2dfiller( int nx , int ny , int ix , int jy , byte * ar ) ;

static PLUGIN_interface * plint = NULL ;

/***********************************************************************
   Set up the interface to the user.  Note that we bypass the
   normal interface creation, and simply have the menu selection
   directly call the main function, which will create a custom
   set of interface widgets.
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   if( ncall > 0 ) return NULL ;  /* only one interface */

   plint = PLUTO_new_interface( "Gyrus Finder" , NULL , NULL ,
				PLUGIN_CALL_IMMEDIATELY , DRAW_main ) ;

   PLUTO_add_hint( plint , "Interactive Region of Interest Editor" ) ;

   PLUTO_set_sequence( plint , "z:Reynolds" ) ;

   return plint ;
}

/***************************************************************************
  Will be called from AFNI when user selects from Plugins menu.
****************************************************************************/

/* Interface widgets */

static Widget shell=NULL , rowcol , info_lab , choose_pb ;
static Widget done_pb , undo_pb , help_pb , quit_pb , save_pb ;
static MCW_arrowval * value_av , * color_av , * mode_av ;

/* Other data */

#define MODE_CURVE       0
#define MODE_CLOSED      1
#define MODE_POINTS      2
#define MODE_FLOOD_VAL   3
#define MODE_FLOOD_NZ    4
#define MODE_VOL_FILL    5
#define MODE_CONN_PTS    6

static char * mode_strings[] = {
  "Open Curve" , "Closed Curve" , "Points" , "Flood->Value",
  "Flood->Nonzero", "Set Fill Point", "Connect Points" };

static int    mode_ints[] = {
  DRAWING_LINES , DRAWING_FILL , DRAWING_POINTS , DRAWING_POINTS ,
  DRAWING_POINTS, DRAWING_POINTS, DRAWING_POINTS };

#define NUM_modes (sizeof(mode_ints)/sizeof(int))

static MCW_DC * dc ;                /* display context */
static Three_D_View * im3d ;        /* AFNI controller */
static THD_3dim_dataset * dset ;    /* The dataset!    */

static int   color_index = 1 ;               /* from color_av */
static int   mode_ival   = MODE_CURVE ;
static int   mode_index  = DRAWING_LINES ;   /* from mode_av  */
static int   value_int   = 1 ;               /* from value_av */
static float value_float = 1.0 ;             /* ditto         */

static int editor_open  = 0 ;
static int dset_changed = 0 ;
static int recv_open    = 0 ;
static int recv_key     = -1 ;   /* 15 Jun 1999 by RWCox */

static int undo_bufsiz = 0 ;     /* size of undo_buf in bytes */
static int undo_bufnum = 0 ;     /* size of undo_xyz in ints */
static int undo_bufuse = 0 ;     /* number of entries in undo buffer */
static void * undo_buf = NULL ;  /* stores data to be copied back to dataset */
static int  * undo_xyz = NULL ;  /* stores voxel indices for copying */

static THD_dataxes dax_save ;    /* save this for later referenc */

char * DRAW_main( PLUGIN_interface * plint )
{
   XmString xstr ;

   ENTRY("DRAW_main");

#if 0
   /* RCR - this plugin is not authorized outside of the mcw domain */
   if ( ! r_check_host( ) )
      return NULL;
#endif

   /*-- sanity checks --*/

   if( ! IM3D_OPEN(plint->im3d) ) RETURN( "AFNI Controller\nnot opened?!") ;

   if( editor_open )
   {
      XMapRaised( XtDisplay(shell) , XtWindow(shell) ) ; NI_sleep(1);

      if ( gRX.main_is_open )
      {
	  XMapRaised( XtDisplay( gRX.main ) , XtWindow( gRX.main ) ) ; NI_sleep(1);
      }
      else
      {
	  XtMapWidget( gRX.main ); NI_sleep(1) ;
	  gRX.main_is_open = 1;
      }

      RETURN(NULL);
   }

   im3d = plint->im3d ;  /* save for local use */

   /*-- create widgets, first time through --*/

   if( shell == NULL ){
      dc = im3d->dc ;        /* save this too */
      DRAW_make_widgets() ;
      PLUTO_set_topshell( plint , shell ) ;  /* 22 Sep 2000 */
      RWC_visibilize_widget( shell ) ;       /* 27 Sep 2000 */
   }

   /*-- set titlebar --*/

   { static char clabel[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ; /* see afni_func.c */
     char ttl[PLUGIN_STRING_SIZE] ; int ic ;

     ic = AFNI_controller_index(im3d) ;  /* find out which controller */

     if( ic >=0 && ic < 26 ){
	sprintf( ttl , "'AFNI' GyrusFinder [%c]" , clabel[ic] ) ;
	XtVaSetValues( shell , XmNtitle , ttl , NULL ) ;
     }
   }

   /*-- set the info label --*/

   xstr = XmStringCreateLtoR( "[No dataset]" ,
			      XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( info_lab , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   /*-- pop the widget up --*/

   XtMapWidget(shell) ; NI_sleep(1) ;

   if ( gRX.main_is_open )
   {
      XMapRaised( XtDisplay( gRX.main ) , XtWindow( gRX.main ) ) ; NI_sleep(1);
   }
   else
   {
      XtMapWidget( gRX.main ); NI_sleep(1) ;
      gRX.main_is_open = 1;
   }


   /*-- misc initialization --*/

   dset         = NULL ;   /* not editing anything   */
   dset_changed = 0 ;      /* not yet changed */
   editor_open  = 1 ;      /* editor is now open for business */
   recv_open    = 0 ;      /* receiver is not yet open */
   recv_key     = -1;

   SENSITIZE(undo_pb,0) ;  undo_bufuse = 0 ;
   SENSITIZE(save_pb,0) ;
   SENSITIZE(choose_pb,1) ;

   RETURN(NULL) ;
}

/*------------------------------------------------------------------------
  Make the control popup for this thing
--------------------------------------------------------------------------*/

/*-- structures defining action buttons (at bottom of popup) --*/

#define NACT 5  /* number of action buttons */

static MCW_action_item DRAW_actor[NACT] = {
 {"Undo",DRAW_undo_CB,NULL,
  "Undoes previous draw\naction, if possible","Undo last change",0} ,

 {"Help",DRAW_help_CB,NULL,
  "Displays more help" , "Displays more help",0} ,

 {"Quit",DRAW_quit_CB,NULL,
  "Discard edits since last Save\nand close Editor" ,
   "Discard edits and close",0} ,

 {"Save",DRAW_save_CB,NULL,
  "Save edits to disk\nand continue" , "Save to disk and continue",0} ,

 {"Done",DRAW_done_CB,NULL,
  "Save edits to disk\nand close Editor" , "Save and close",1}
} ;

static void DRAW_make_widgets(void)
{
   XmString xstr ;

   ENTRY("DRAW_make_widgets");

   /*** top level shell for window manager ***/

   shell =
      XtVaAppCreateShell(
	   "AFNI" , "AFNI" , topLevelShellWidgetClass , dc->display ,
	   XmNtitle             , "GFinder Editor", /* top of window */
	   XmNiconName          , "GFinder"       , /* label on icon */
	   XmNdeleteResponse    , XmDO_NOTHING    , /* deletion handled below */
	   XmNallowShellResize  , True ,            /* let code resize shell? */
	   XmNmappedWhenManaged , False ,           /* must map it manually */
	   XmNinitialResourcesPersistent , False ,
      NULL ) ;

   if( afni48_good )             /* set icon pixmap */
      XtVaSetValues( shell ,
			XmNiconPixmap , afni48_pixmap ,
		     NULL ) ;

   if( MCW_isitmwm(shell) )      /* remove some MWM functions */
      XtVaSetValues( shell ,
		       XmNmwmFunctions ,
		       MWM_FUNC_MOVE | MWM_FUNC_CLOSE | MWM_FUNC_MINIMIZE ,
		     NULL ) ;

   XmAddWMProtocolCallback(      /* make "Close" window menu work */
	   shell ,
	   XmInternAtom( dc->display , "WM_DELETE_WINDOW" , False ) ,
	   DRAW_quit_CB , (XtPointer) plint ) ;

   /*** rowcolumn widget to hold all user interface stuff ***/

   rowcol = XtVaCreateWidget(
	     "AFNI" , xmRowColumnWidgetClass , shell ,
		XmNpacking     , XmPACK_TIGHT ,
		XmNorientation , XmVERTICAL ,
		XmNtraversalOn , True  ,
		XmNinitialResourcesPersistent , False ,
	     NULL ) ;

   /*** label at top to let user know who we are ***/

   xstr = XmStringCreateLtoR( "[No dataset]" ,
			      XmFONTLIST_DEFAULT_TAG ) ;
   info_lab = XtVaCreateManagedWidget(
		 "AFNI" , xmLabelWidgetClass , rowcol ,
		    XmNlabelString , xstr ,
		    XmNinitialResourcesPersistent , False ,
		 NULL ) ;
   XmStringFree(xstr) ;
   MCW_register_help( info_lab , "Shows dataset being edited" ) ;
   MCW_register_hint( info_lab , "Shows dataset being edited" ) ;

   /*** separator for visual neatness ***/

   (void) XtVaCreateManagedWidget(
	     "AFNI" , xmSeparatorWidgetClass , rowcol ,
		XmNseparatorType , XmSINGLE_LINE ,
		XmNinitialResourcesPersistent , False ,
	     NULL ) ;

   /*** button to let user choose dataset to edit ***/

   xstr = XmStringCreateLtoR( "Choose Dataset" , XmFONTLIST_DEFAULT_TAG ) ;
   choose_pb = XtVaCreateManagedWidget(
		  "AFNI" , xmPushButtonWidgetClass , rowcol ,
		     XmNlabelString , xstr ,
		     XmNtraversalOn , True  ,
		     XmNinitialResourcesPersistent , False ,
		  NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( choose_pb, XmNactivateCallback, DRAW_choose_CB, NULL ) ;
   MCW_register_help( choose_pb ,
		      "Use this to popup a\n"
		      "'chooser' that lets\n"
		      "you select which\n"
		      "dataset to edit."
		    ) ;
   MCW_register_hint( choose_pb , "Popup a dataset chooser" ) ;

   /***  arrowval to choose value that is drawn into dataset voxels  ***/

   value_av = new_MCW_arrowval( rowcol , "Drawing Value " ,
				MCW_AV_downup , -32767,32767,value_int ,
				MCW_AV_editext , 0 ,
				DRAW_value_CB , NULL , NULL,NULL ) ;

   MCW_reghelp_children( value_av->wrowcol ,
			 "Use this to set the value that\n"
			 "will be drawn into the dataset\n"
			 "using mouse button 2."
		       ) ;
   MCW_reghint_children( value_av->wrowcol , "Goes into dataset voxels" ) ;

   /*** option menu to choose drawing color ***/

   color_av = new_MCW_colormenu( rowcol , "Drawing Color " , dc ,
				 1 , dc->ovc->ncol_ov - 1 , color_index ,
				 DRAW_color_CB , NULL ) ;

   MCW_reghelp_children( color_av->wrowcol ,
			 "Use this to set the color that is\n"
			 "shown during mouse button 2 drawing.\n"
			 "N.B.: After drawing is completed,\n"
			 "  the dataset will be displayed\n"
			 "  with the chosen value replacing\n"
			 "  the drawing color.  This color\n"
			 "  is used ONLY while button 2 is\n"
			 "  actually pressed down."
		       ) ;
   MCW_reghint_children( color_av->wrowcol , "Used when button 2 is drawing" ) ;

   /*** arrowval to choose drawing mode ***/

   mode_av = new_MCW_optmenu( rowcol , "Drawing Mode  " ,
			      0 , NUM_modes-1 , 0,0 ,
			      DRAW_mode_CB , NULL ,
			      MCW_av_substring_CB , mode_strings ) ;

   MCW_reghelp_children( mode_av->wrowcol ,
			 "Use this to set the way in which\n"
			 "drawing pixels on the screen is\n"
			 "used to select dataset voxels:\n"
			 "Open Curve     = voxels picked along lines drawn;\n"
			 "Closed Curve   = voxels forming a closed curve\n"
			 "Points         = only voxels at X11 notify pixels;\n"
			 "Flood->Value   = flood fill from the chosen point\n"
			 "                 out to points = Drawing Value\n"
			 "Flood->Nonzero = flood fill from chosen point out\n"
			 "                 to any nonzero point"
		       ) ;
   MCW_reghint_children( mode_av->wrowcol , "How voxels are chosen") ;

   /*** separator for visual neatness ***/

   (void) XtVaCreateManagedWidget(
	     "AFNI" , xmSeparatorWidgetClass , rowcol ,
		XmNseparatorType , XmSINGLE_LINE ,
		XmNinitialResourcesPersistent , False ,
	     NULL ) ;

   /*** a set of action buttons below the line ***/

   (void) MCW_action_area( rowcol , DRAW_actor , NACT ) ;

   undo_pb = (Widget) DRAW_actor[0].data ;
   help_pb = (Widget) DRAW_actor[1].data ;
   quit_pb = (Widget) DRAW_actor[2].data ;
   save_pb = (Widget) DRAW_actor[3].data ;
   done_pb = (Widget) DRAW_actor[4].data ;

   /*** that's all  (for Bob) ***/

   XtManageChild(rowcol) ;
   XtRealizeWidget(shell) ; NI_sleep(1) ;       /* will not be mapped */

   r_main_mk_main_shell( );

   EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Check the hostname of the computer.  If the name is not part of the
**  mcw.edu domain, then the machine is not authorized to use this plugin.
**
**----------------------------------------------------------------------
 */
#if 0
static int
r_check_host( void )
{
    long hostid = 0;

    hostid = gethostid( );

    if ( ( hostid & R_HOSTID_MASK ) != R_HOSTID_VAL )
    {
	rERROR( "Your machine is not currently authorized to use this plugin.");

	system( "echo GF usage :`whoami`@`hostname`|mail rickr@mcw.edu" );

	return 0;
    }

    return 1;
}
#endif

/*----------------------------------------------------------------------
**
**  Create main GyrusFinder shell.
**
**----------------------------------------------------------------------
 */
static void
r_main_mk_main_shell( void )
{
    ENTRY("r_main_mk_main_shell");
#ifdef R_LOG_INFO_D
    if ( ! r_open_log_file( ) ) EXRETURN ;
#endif

    r_wtgr_mk_main_shell  ( &gRX );
    r_INT_mk_main_shell   ( &gRI );             /* interpolation shell */
    r_HL_mk_main_shell    ( &gRH );             /* hole filler shell */
    r_main_mk_show_buttons( );

    EXRETURN;
}


#ifdef R_LOG_INFO_D
/*----------------------------------------------------------------------
**
**  Open R_LOG_FILE to log messages to.
**
**----------------------------------------------------------------------
 */
static int
r_open_log_file( void )
{
    if ( ( gr_logfile = fopen( R_LOG_FILE, "a" ) ) == NULL )
    {
	sprintf( gRmessage, "Failed to open '%s' for append.", R_LOG_FILE );
	rERROR( gRmessage );

	return 0;
    }

    return 1;
}
#endif


/*----------------------------------------------------------------------
**
**  Main window for buttons to show other windows.
**
**----------------------------------------------------------------------
 */
static void
r_main_mk_show_buttons( void )
{
    int      ac;
    Arg      al[ 10 ];
    Widget   button, tmpb, tmpb2;
    XmString xstr;

    ENTRY("r_main_mk_show_buttons");

    gRX.main_is_open = 1;

    ac = 0;
    XtSetArg( al[ac], XmNinitialResourcesPersistent, False );  ac++;
    XtSetArg( al[ac], XmNdeleteResponse, XmDO_NOTHING );  ac++;
    gRX.main = XtAppCreateShell( "GyrusFinder", "rshell",
			topLevelShellWidgetClass, gRX.display, al, ac );

    XmAddWMProtocolCallback( gRX.main,
	XmInternAtom( gRX.display, "WM_DELETE_WINDOW", False ),
	( XtCallbackProc )r_main_cb_quit, ( XtPointer )NULL );

    ac = 0;
    gRX.mainForm = XmCreateForm( gRX.main, "form", al, ac );


    /* raise interpolator button */
    ac = 0;
    XtSetArg( al[ac], XmNalignment, XmALIGNMENT_CENTER );  ac++;
    XtSetArg( al[ac], XmNwidth, 220 );  ac++;
    xstr = XmStringCreateLtoR( "Interpolator", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button = XmCreatePushButton( gRX.mainForm, "INT", al, ac );
    XtManageChild( button );
    XtAddCallback( button, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_raise, "INT" );
    XtSetSensitive( button, True );
    XmStringFree(xstr) ;
    tmpb = button;

    /* raise wtgr button */
    ac = 0;
    XtSetArg( al[ac], XmNalignment, XmALIGNMENT_CENTER );  ac++;
    XtSetArg( al[ac], XmNtopAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNtopWidget, tmpb );  ac++;
    XtSetArg( al[ac], XmNwidth, 220 );  ac++;
    xstr = XmStringCreateLtoR( "White/Gray Finder", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    button = XmCreatePushButton( gRX.mainForm, "WhiteGray", al, ac );
    XtManageChild( button );
    XtAddCallback( button, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_raise, "wtgr" );
    XtSetSensitive( button, True );
    XmStringFree( xstr );
    tmpb = button;


    /* raise holes button */
    ac = 0;
    XtSetArg( al[ac], XmNalignment, XmALIGNMENT_CENTER );  ac++;
    XtSetArg( al[ac], XmNtopAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNtopWidget, tmpb );  ac++;
    XtSetArg( al[ac], XmNwidth, 220 );  ac++;
    xstr = XmStringCreateLtoR( "Hole Filler", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button = XmCreatePushButton( gRX.mainForm, "fillholes", al, ac );
    XtManageChild( button );
    XtAddCallback( button, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_raise, "HL" );
    XtSetSensitive( button, True );
    XmStringFree( xstr );
    tmpb = button;


    /* UNDO button */

    ac = 0;
    XtSetArg( al[ac], XmNalignment, XmALIGNMENT_CENTER );  ac++;
    XtSetArg( al[ac], XmNtopAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNtopWidget, tmpb );  ac++;
    XtSetArg( al[ac], XmNwidth, 110 );  ac++;
    xstr = XmStringCreateLtoR( "UNDO", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button = XmCreatePushButton( gRX.mainForm, "saveas", al, ac );
    XtManageChild( button );
    XtAddCallback( button, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_undo, NULL );
    XtSetSensitive( button, True );
    XmStringFree( xstr );
    tmpb2 = button;


    /* save as button */

    ac = 0;
    XtSetArg( al[ac], XmNalignment, XmALIGNMENT_CENTER );  ac++;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button );  ac++;
    XtSetArg( al[ac], XmNtopAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNtopWidget, tmpb );  ac++;
    XtSetArg( al[ac], XmNwidth, 110 );  ac++;
    xstr = XmStringCreateLtoR( "Save As", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button = XmCreatePushButton( gRX.mainForm, "saveas", al, ac );
    XtManageChild( button );
    XtAddCallback( button, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_raise, "saveas" );
    XtSetSensitive( button, True );
    XmStringFree( xstr );
    tmpb = button;

    /*
    **  We're going to cheat a little here.  The "save as" widget will
    **  be implemented as a child of the main RC.  So we need to create
    **  the widget now, so the main "save as" button can raise/manage it.
    */

    r_main_mk_save_as_fr( gRX.mainForm );


    /* help button */
    ac = 0;
    XtSetArg( al[ac], XmNalignment, XmALIGNMENT_CENTER );  ac++;
    XtSetArg( al[ac], XmNtopAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNtopWidget, tmpb2 );  ac++;
    XtSetArg( al[ac], XmNwidth, 110 );  ac++;
    xstr = XmStringCreateLtoR( "Help", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button = XmCreatePushButton( gRX.mainForm, "help", al, ac );
    XtManageChild( button );
    XtAddCallback( button, XmNactivateCallback,
		   ( XtCallbackProc )r_main_cb_help, NULL );
    XtSetSensitive( button, True );
    XmStringFree( xstr );


    /* show data structures button */
    ac = 0;
    XtSetArg( al[ac], XmNalignment, XmALIGNMENT_CENTER );  ac++;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button );  ac++;
    XtSetArg( al[ac], XmNtopAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNtopWidget, tmpb );  ac++;
    XtSetArg( al[ac], XmNwidth, 110 );  ac++;
    xstr = XmStringCreateLtoR( "structs", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button = XmCreatePushButton( gRX.mainForm, "structs", al, ac );
    XtManageChild( button );
    XtAddCallback( button, XmNactivateCallback,
		   ( XtCallbackProc )r_main_cb_show_structs, NULL );
    XtSetSensitive( button, True );
    XmStringFree( xstr );


    XtManageChild( gRX.mainForm );
    XtRealizeWidget( gRX.main ); NI_sleep(1) ;

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Create the save as widget under the main RC.
**
**----------------------------------------------------------------------
 */
static void
r_main_mk_save_as_fr( Widget parent )
{
    Widget   button, junk, hrc, vrc, frame;
    Arg      al[ 10 ];
    int      ac;
    char     string[ 25 ];

    ENTRY("r_main_mk_save_as_fr");

      XtVaSetValues( shell ,
		       XmNmwmFunctions ,
		       MWM_FUNC_MOVE | MWM_FUNC_CLOSE | MWM_FUNC_MINIMIZE ,
		     NULL ) ;
    ac = 0;
    XtSetArg( al[ ac ], XmNdialogTitle,
	    XmStringCreateLtoR( "Save As", gRX.charset ) );  ac++;
    XtSetArg( al[ ac ], XmNtextString, 
	    XmStringCreateLtoR( gRA.save_as_name, gRX.charset ) );  ac++;
    XtSetArg( al[ ac ], XmNselectionLabelString, 
	    XmStringCreateLtoR( "AFNI prefix : ", gRX.charset ) );  ac++;
    XtSetArg( al[ ac ], XmNokLabelString, 
	    XmStringCreateLtoR( "save", gRX.charset ) );            ac++;
    XtSetArg( al[ ac ], XmNapplyLabelString, 
	    XmStringCreateLtoR( "overwrite", gRX.charset ) );       ac++;
    XtSetArg( al[ ac ], XmNcancelLabelString, 
	    XmStringCreateLtoR( "hide", gRX.charset ) );            ac++;
    XtSetArg( al[ ac ], XmNminimizeButtons, True );                 ac++;
    XtSetArg( al[ac], XmNinitialResourcesPersistent, False );       ac++;
    XtSetArg( al[ac], XmNmappedWhenManaged, False );                ac++;
    XtSetArg( al[ac], XmNdeleteResponse, XmDO_NOTHING );            ac++;
    gRX.save_as_file_d = XmCreatePromptDialog( parent, "dialog", al, ac );

    XmAddWMProtocolCallback( gRX.save_as_file_d,
	XmInternAtom( gRX.display, "WM_DELETE_WINDOW", False ),
	( XtCallbackProc )r_any_cb_hide, "saveas" );

    XtAddCallback( gRX.save_as_file_d, XmNokCallback, 
	    ( XtCallbackProc )r_main_cb_saveas, ( XtPointer )0 );
    XtAddCallback( gRX.save_as_file_d, XmNapplyCallback, 
	    ( XtCallbackProc )r_main_cb_saveas, ( XtPointer )1 );
    XtAddCallback( gRX.save_as_file_d, XmNcancelCallback, 
	    ( XtCallbackProc )r_any_cb_hide, "saveas" );

    XtManageChild( XmSelectionBoxGetChild( gRX.save_as_file_d,
					     XmDIALOG_APPLY_BUTTON ) );
    XtUnmanageChild( XmSelectionBoxGetChild( gRX.save_as_file_d,
					     XmDIALOG_HELP_BUTTON ) );

    XtManageChild( gRX.save_as_file_d );

    VISIBILIZE_WHEN_MAPPED(gRX.save_as_file_d) ; /* 27 Sep 2000 */

    /*
    ** Once we prevent it from popping up immediately, set it to display
    ** anytime it's managed.
    */
    ac = 0;
    XtSetArg( al[ac], XmNmappedWhenManaged, True );  ac++;
    XtSetValues( gRX.save_as_file_d, al, ac );

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Create main shell and gray and white matter function frames.
**
**----------------------------------------------------------------------
 */
static void
r_wtgr_mk_main_shell( r_X_s * X )
{
    Widget    junk, frame, rc;
    XmString  xstring;
    Arg       al[ 20 ];
    int       ac;

    ENTRY("r_wtgr_mk_main_shell");

/* create a main RC widget with four frame children */
    
    if ( ! r_init_Alg_values( &gRA ) )
	EXRETURN;

    if ( ! r_init_pt_conn_s( &gRCP ) )
	EXRETURN;

    X->display = dc->display;
    X->charset = XmSTRING_DEFAULT_CHARSET;

    ac = 0;
    XtSetArg( al[ac], XmNinitialResourcesPersistent, False );  ac++;
    XtSetArg( al[ac], XmNdeleteResponse, XmDO_NOTHING );  ac++;
    XtSetArg( al[ac], XmNmappedWhenManaged, False );  ac++;
    X->wtgr_main = XtAppCreateShell( "White/Gray Finder", "rshell",
		topLevelShellWidgetClass, X->display, al, ac );

    XmAddWMProtocolCallback( gRX.wtgr_main,
	XmInternAtom( gRX.display, "WM_DELETE_WINDOW", False ),
	( XtCallbackProc )r_any_cb_hide, "wtgr" );

    ac = 0;
    XtSetArg( al[ac], XmNspacing,      15 );  ac++;
    XtSetArg( al[ac], XmNmarginWidth,  10 );  ac++;
    XtSetArg( al[ac], XmNmarginHeight, 10 );  ac++;
    X->wtgr_mainRC = XmCreateRowColumn( X->wtgr_main, "rowcolumn", al, ac );

    /* create the main white and gray selection frames */
    ( void )r_wt_mk_main_frame( X, X->wtgr_mainRC );
    ( void )r_gr_mk_main_frame( X, X->wtgr_mainRC );


    XtManageChild( X->wtgr_mainRC );
    XtRealizeWidget( X->wtgr_main );

    EXRETURN ;
}


/*----------------------------------------------------------------------
**
**  Create the Hole Filler shell.
**
**----------------------------------------------------------------------
 */
static void
r_HL_mk_main_shell( holes_s * H )
{
    Widget    junk, frame, rc;
    XmString  xstring;
    Arg       al[ 20 ];
    int       ac;

    ENTRY("r_HL_mk_main_shell");

    if ( ! r_init_holes_vals( H ) )
	EXRETURN;

    ac = 0;
    XtSetArg( al[ac], XmNinitialResourcesPersistent, False );  ac++;
    XtSetArg( al[ac], XmNdeleteResponse, XmDO_NOTHING );  ac++;
    XtSetArg( al[ac], XmNmappedWhenManaged, False );  ac++;
    H->main = XtAppCreateShell( "Hole Filler", "rshell",
			topLevelShellWidgetClass, gRX.display, al, ac );

    XmAddWMProtocolCallback( gRH.main,
	XmInternAtom( gRX.display, "WM_DELETE_WINDOW", False ),
	( XtCallbackProc )r_any_cb_hide, "HL" );

    ac = 0;
    XtSetArg( al[ac], XmNspacing,      15 );  ac++;
    XtSetArg( al[ac], XmNmarginWidth,  10 );  ac++;
    XtSetArg( al[ac], XmNmarginHeight, 10 );  ac++;
    H->mainRC = XmCreateRowColumn( H->main, "rowcolumn", al, ac );

    ( void )r_HL_mk_fillval_fr( H, H->mainRC );
    ( void )r_HL_mk_maxsize_fr( H, H->mainRC );
    ( void )r_HL_mk_buttons   ( H, H->mainRC );

    XtManageChild  ( H->mainRC );
    XtRealizeWidget( H->main ); NI_sleep(1) ;

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Create buttons for the "hole filling" app.
**
**----------------------------------------------------------------------
 */
static Widget
r_HL_mk_buttons( holes_s * H, Widget parent )
{
    int      ac;
    Arg      al[ 10 ];
    Widget   form, frame, button1, button2;
    XmString xstr;

    ENTRY("r_HL_mk_buttons");

    /* create frame to hold form and buttons */
    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );

    /* create form to hold push buttons */
    ac = 0;
    XtSetArg( al[ac], XmNhorizontalSpacing, R_BUTTON_SPACE );  ac++;
    form = XmCreateForm( frame, "form", al, ac );


    /* region fill button */
    ac = 0;
    xstr = XmStringCreateLtoR( "FILL", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button1 = XmCreatePushButton( form, "fill", al, ac );
    XtManageChild( button1 );
    XtAddCallback( button1, XmNactivateCallback,
		   ( XtCallbackProc )r_HL_cb_fill, H );
    XtSetSensitive( button1, True );
    XmStringFree( xstr );

    /* unfill button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button1 );  ac++;
    xstr = XmStringCreateLtoR( "unfill", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button2 = XmCreatePushButton( form, "unfill", al, ac );
    XtManageChild( button2 );
    XtAddCallback( button2, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_unfill, &H->fill_val );
    XtSetSensitive( button2, True );
    XmStringFree( xstr );


    /* stats button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button2 );  ac++;
    xstr = XmStringCreateLtoR( "stats", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button1 = XmCreatePushButton( form, "stats", al, ac );
    XtManageChild( button1 );
    XtAddCallback( button1, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_fill_stats, &gRH.fill_val );
    XtSetSensitive( button1, True );
    XmStringFree( xstr );

    /* hide button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button1 );  ac++;
    xstr = XmStringCreateLtoR( "hide", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button2 = XmCreatePushButton( form, "hide", al, ac );
    XtManageChild( button2 );
    XtAddCallback( button2, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_hide, "HL" );
    XtSetSensitive( button2, True );
    XmStringFree( xstr );


    XtManageChild( form );
    XtManageChild( frame );

    RETURN( frame );
}


/*----------------------------------------------------------------------
**
**  Create the "max hole size" widget for hole filler window.
**
**----------------------------------------------------------------------
 */
static Widget
r_HL_mk_maxsize_fr( holes_s * H, Widget parent )
{
    Widget   junk, rc, frame;
    XmString xstr;
    Arg      al[ 10 ];
    int      ac;
    char     string[ 15 ];

    ENTRY("r_HL_mk_maxsize_fr");

    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );
    
    ac = 0;
    XtSetArg( al[ ac ], XmNpacking, XmPACK_TIGHT );  ac++;
    XtSetArg( al[ ac ], XmNorientation, XmHORIZONTAL );  ac++;
    rc = XmCreateRowColumn( frame, "rowcolumn", al, ac );


    sprintf( string, "%d", H->max_size );

    ac = 0;
    xstr = XmStringCreateLtoR( "max size      : ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateLabel( rc, "label", al, ac );
    XtManageChild( junk );
    XmStringFree( xstr );

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 80 );  ac++;
    H->maxsize_w = XmCreateText( rc, "text", al, ac );
    XtManageChild( H->maxsize_w );
    XtAddCallback( H->maxsize_w, XmNactivateCallback, 
	    ( XtCallbackProc )r_HL_cb_set_maxsize, NULL );
    XtAddCallback( H->maxsize_w, XmNlosingFocusCallback, 
	    ( XtCallbackProc )r_HL_cb_set_maxsize, NULL );
    
    
    XtManageChild( rc );
    XtManageChild( frame );

    RETURN( frame );
}


/*----------------------------------------------------------------------
**
**  Create the "fill value" widget for hole filler window.
**
**----------------------------------------------------------------------
*/
static Widget
r_HL_mk_fillval_fr( holes_s * H, Widget parent )
{
    Widget   junk, rc, frame;
    XmString xstr;
    Arg      al[ 10 ];
    int      ac;
    char     string[ 15 ];

    ENTRY("r_HL_mk_fillval_fr");

    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );
    
    ac = 0;
    XtSetArg( al[ ac ], XmNpacking, XmPACK_TIGHT );  ac++;
    XtSetArg( al[ ac ], XmNorientation, XmHORIZONTAL );  ac++;
    rc = XmCreateRowColumn( frame, "rowcolumn", al, ac );


    sprintf( string, "%d", H->fill_val );

    ac = 0;
    xstr = XmStringCreateLtoR( "fill value    : ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateLabel( rc, "label", al, ac );
    XtManageChild( junk );
    XmStringFree( xstr );

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 80 );  ac++;
    H->fillval_w = XmCreateText( rc, "text", al, ac );
    XtManageChild( H->fillval_w );
    XtAddCallback( H->fillval_w, XmNactivateCallback, 
	    ( XtCallbackProc )r_HL_cb_set_fill_val, NULL );
    XtAddCallback( H->fillval_w, XmNlosingFocusCallback, 
	    ( XtCallbackProc )r_HL_cb_set_fill_val, NULL );
    
    
    XtManageChild( rc );
    XtManageChild( frame );

    RETURN( frame );
}


/*----------------------------------------------------------------------
**
**  Initialize the holes_s structure.
**
**----------------------------------------------------------------------
 */
static int
r_init_holes_vals( holes_s * H )
{
    ENTRY("r_init_holes_vals");

    H->max_size = 6;
    H->fill_val = 3;

    H->filled.points = NULL;
    H->filled.used   = 0;
    H->filled.M      = 0;

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Fill all interior holes, subject to a size restriction.
**
**----------------------------------------------------------------------
 */
static void
r_HL_cb_fill(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    points_t   Marked, Search;
    holes_s  * H = (holes_s *)client_data;
    short    * fdata = gRA.fdata;
    short    * fptr, * uptr;
    int      * wgpts, *tmpp;
    int        count, fill_size, coord, status, pcount;

    ENTRY("r_HL_cb_fill");

    /* first store fdata to undo_data  */
    fptr = gRA.fdata; uptr = gRA.undo_data;
    for ( count = 0; count < gRA.nvox; count++ )
	*uptr++ = *fptr++;


    Search.points = Marked.points = NULL;

    /* add gRA.border and gr_edge to wtgr_edge */
    H->wtgr_edge.used = 0;
    for ( count = 0, wgpts = H->gr_edge.points;
	  count < H->gr_edge.used;
	  count++, wgpts++ )
	if ( ! r_add_to_boundary( &H->wtgr_edge, *wgpts ) )
	    EXRETURN;

    for ( count = 0, wgpts = gRA.border.points;
	  count < gRA.border.used;
	  count++, wgpts++ )
	if ( ! r_add_to_boundary( &H->wtgr_edge, *wgpts ) )
	    EXRETURN;

    /* for each point in white/gray boundary - try to grow */
    /* (watch for 3-D brick edges) */
    for ( count = 0, wgpts = H->wtgr_edge.points;
	  count < H->wtgr_edge.used;
	  count++, wgpts++ )
    {

	fill_size = 0;                  /* init for current fill region */

	fptr = fdata + *wgpts;          /* addr + coordinate (offset) */
	if ( *fptr )
	   continue;

	if ( ! r_add_to_boundary( &Search, *wgpts ) )
	    EXRETURN;

	while ( Search.used > 0 )
	{
	    coord = Search.points[Search.used - 1];
	    Search.used--;

	    *(fdata + coord) = H->fill_val;     /* mark as found */
	    if ( ! r_add_to_boundary( &Marked, coord ) )
		EXRETURN;

	    fill_size++;

	    /* 1 = OK, 0 = BAD point found, -1 = memory error */
	    if ( ( status = r_HL_check_neighbors( &Search, coord ) ) == -1 )
		EXRETURN;
	    
	    if ( ( fill_size > H->max_size ) || ( status == 0 ) )
	    {
		status = 0;     /* if we enter through fill_size */

		for ( pcount = 0, tmpp = Search.points;
		      pcount < Search.used;
		      pcount++, tmpp++ )
		    *(fdata + *tmpp) = R_HL_BAD_VAL;

		Search.used = 0;        /* will terminate search loop */

		for ( pcount = 0, tmpp = Marked.points;
		      pcount < Marked.used;
		      pcount++, tmpp++ )
		    *(fdata + *tmpp) = R_HL_BAD_VAL;

		Marked.used = 0;
	    }
	}

	Marked.used = 0;
    }

    /* reset all BAD_VAL points - we haven't saved them */
    for ( count = 0, fptr = fdata; count < gRA.nvox; count++, fptr++ )
	if ( *fptr == R_HL_BAD_VAL )
	    *fptr = 0;


    THD_load_statistics( gRA.func ) ;
    PLUTO_dset_redisplay( gRA.func ) ;

    dset_changed = 1;

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Check neighbors for insertion into searchlist, or for BAD values.
**
**  Make sure we don't "leave" the 3-D image.
**
**  return 1 - normal
**         0 - BAD point found
**        -1 - memory error ( r_add_to_boundary() )
**
**----------------------------------------------------------------------
 */
static int
r_HL_check_neighbors( points_t * P, int coord )
{
    short * nptr  = gRA.neighbors;
    short * fvalp = NULL;
    int     nx, nxy, value;

    ENTRY("r_HL_check_neighbors");

    nx  = gRA.nx;
    nxy = gRA.nxy;

    if ( nptr[ coord ] == -1 )  /* do not accept edge points */
	RETURN(1);  /* normal return */

    fvalp = gRA.fdata + coord;

    value = *(fvalp - 1);
    if ( value == R_HL_BAD_VAL )
	RETURN(0);
    else if ( ! value )
	if ( ! r_add_to_boundary( P, coord - 1 ) )
	    RETURN(-1);

    value = *(fvalp + 1);
    if ( value == R_HL_BAD_VAL )
	RETURN(0);
    else if ( ! value )
	if ( ! r_add_to_boundary( P, coord + 1 ) )
	    RETURN(-1);

    value = *(fvalp - nx);
    if ( value == R_HL_BAD_VAL )
	RETURN(0);
    else if ( ! value )
	if ( ! r_add_to_boundary( P, coord - nx ) )
	    RETURN(-1);

    value = *(fvalp + nx);
    if ( value == R_HL_BAD_VAL )
	RETURN(0);
    else if ( ! value )
	if ( ! r_add_to_boundary( P, coord + nx ) )
	    RETURN(-1);

    value = *(fvalp - nxy);
    if ( value == R_HL_BAD_VAL )
	RETURN(0);
    else if ( ! value )
	if ( ! r_add_to_boundary( P, coord - nxy ) )
	    RETURN(-1);

    value = *(fvalp + nxy);
    if ( value == R_HL_BAD_VAL )
	RETURN(0);
    else if ( ! value )
	if ( ! r_add_to_boundary( P, coord + nxy ) )
	    RETURN(-1);

    RETURN(0);
}


/*----------------------------------------------------------------------
**
**  Create interpolation shell.
**
**----------------------------------------------------------------------
 */
static void
r_INT_mk_main_shell( interp_s * I )
{
    Widget    junk, frame, rc;
    XmString  xstring;
    Arg       al[ 20 ];
    int       ac;

    ENTRY("r_INT_mk_main_shell");

    if ( ! r_init_interp_vals( I ) )
	EXRETURN;

    ac = 0;
    XtSetArg( al[ac], XmNinitialResourcesPersistent, False );  ac++;
    XtSetArg( al[ac], XmNdeleteResponse, XmDO_NOTHING );  ac++;
    XtSetArg( al[ac], XmNmappedWhenManaged, False );  ac++;
    I->main = XtAppCreateShell( "The Interpolator",
		"rshell", topLevelShellWidgetClass, gRX.display, al, ac );

    XmAddWMProtocolCallback( I->main,
	XmInternAtom( gRX.display, "WM_DELETE_WINDOW", False ),
	( XtCallbackProc )r_any_cb_hide, "INT" );

    ac = 0;
    XtSetArg( al[ac], XmNspacing,      15 );  ac++;
    XtSetArg( al[ac], XmNmarginWidth,  10 );  ac++;
    XtSetArg( al[ac], XmNmarginHeight, 10 );  ac++;
    I->mainRC = XmCreateRowColumn( I->main, "rowcolumn", al, ac );

    /* create the main white and gray selection frames */
    ( void )r_INT_mk_fillval_fr ( I, I->mainRC );
    ( void )r_INT_mk_app_buttons( I, I->mainRC );

    XtManageChild( I->mainRC );
    XtRealizeWidget( I->main ); NI_sleep(1) ;

    EXRETURN ;
}


/*----------------------------------------------------------------------
**
**  Create boundary (gray matter) buttons.
**
**----------------------------------------------------------------------
 */
static Widget
r_INT_mk_app_buttons( interp_s * I, Widget parent )
{
    int      ac;
    Arg      al[ 10 ];
    Widget   form, frame, button1, button2;
    XmString xstr;

    ENTRY("r_INT_mk_app_buttons");

    /* create frame to hold form and buttons */
    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );

    /* create form to hold push buttons */
    ac = 0;
    XtSetArg( al[ac], XmNhorizontalSpacing, R_BUTTON_SPACE );  ac++;
    form = XmCreateForm( frame, "form", al, ac );


    /* region fill button */
    ac = 0;
    xstr = XmStringCreateLtoR( "FILL", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button1 = XmCreatePushButton( form, "fill", al, ac );
    XtManageChild( button1 );
    XtAddCallback( button1, XmNactivateCallback,
		   ( XtCallbackProc )r_INT_cb_fill, I );
    XtSetSensitive( button1, True );
    XmStringFree( xstr );
    MCW_register_hint( button1 , "interpolate between the last two lines" );

    /* unfill button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button1 );  ac++;
    xstr = XmStringCreateLtoR( "unfill", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button2 = XmCreatePushButton( form, "unfill", al, ac );
    XtManageChild( button2 );
    XtAddCallback( button2, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_unfill, &gRI.fill_val );
    XtSetSensitive( button2, True );
    XmStringFree( xstr );
    MCW_register_hint( button2 , "unfill at interpolation \"fill value\"" );

    /* apply button - set interpolation values to draw values */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button2 );  ac++;
    xstr = XmStringCreateLtoR( "apply", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button1 = XmCreatePushButton( form, "apply", al, ac );
    XtManageChild( button1 );
    XtAddCallback( button1, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_apply, &gRI.fill_val );
    XtSetSensitive( button1, True );
    XmStringFree( xstr );
    MCW_register_hint( button1 , "sets fill values to main draw values" );


    /* stats button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button1 );  ac++;
    xstr = XmStringCreateLtoR( "stats", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button2 = XmCreatePushButton( form, "stats", al, ac );
    XtManageChild( button2 );
    XtAddCallback( button2, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_fill_stats, &gRI.fill_val );
    XtSetSensitive( button2, True );
    XmStringFree( xstr );

    /* hide button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button2 );  ac++;
    xstr = XmStringCreateLtoR( "hide", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button1 = XmCreatePushButton( form, "hide", al, ac );
    XtManageChild( button1 );
    XtAddCallback( button1, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_hide, "INT" );
    XtSetSensitive( button1, True );
    XmStringFree( xstr );


    XtManageChild( form );
    XtManageChild( frame );

    RETURN( frame );
}


/*----------------------------------------------------------------------
**
**  Create the "fill value" widget for interpolation window.
**
**----------------------------------------------------------------------
 */
static Widget
r_INT_mk_fillval_fr( interp_s * I, Widget parent )
{
    Widget   junk, rc, frame;
    XmString xstr;
    Arg      al[ 10 ];
    int      ac;
    char     string[ 15 ];

    ENTRY("r_INT_mk_fillval_fr");

    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );
    
    ac = 0;
    XtSetArg( al[ ac ], XmNpacking, XmPACK_TIGHT );  ac++;
    XtSetArg( al[ ac ], XmNorientation, XmHORIZONTAL );  ac++;
    rc = XmCreateRowColumn( frame, "rowcolumn", al, ac );


    sprintf( string, "%d", I->fill_val );

    ac = 0;
    xstr = XmStringCreateLtoR( "fill value   : ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateLabel( rc, "label", al, ac );
    XtManageChild( junk );
    XmStringFree( xstr );

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 80 );  ac++;
    I->fillval_w = XmCreateText( rc, "text", al, ac );
    XtManageChild( I->fillval_w );
    XtAddCallback( I->fillval_w, XmNactivateCallback, 
	    ( XtCallbackProc )r_INT_cb_set_fill_val, NULL );
    XtAddCallback( I->fillval_w, XmNlosingFocusCallback, 
	    ( XtCallbackProc )r_INT_cb_set_fill_val, NULL );
    
    
    XtManageChild( rc );
    XtManageChild( frame );

    RETURN( frame );
}


/*----------------------------------------------------------------------
**
**  Callback to apply the interpolation values (change them to
**  drawn values).
**
**----------------------------------------------------------------------
 */
static void
r_any_cb_apply(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    short  * fptr, * uptr;
    int      clear_val = *( (int *)client_data );
    int      count;

    ENTRY("r_any_cb_apply");

    if ( ! gRA.fdata )  /* error beep */
    {
	fprintf( stderr, "%c", 7 );
	EXRETURN;
    }

    /* first store to undo_data */
    fptr = gRA.fdata; uptr = gRA.undo_data;
    for ( count = 0; count < gRA.nvox; count++ )
	*uptr++ = *fptr++;

    for ( count = 0, fptr = gRA.fdata; count < gRA.nvox; count++, fptr++ )
	if ( *fptr == clear_val )
	    *fptr = value_int;

    THD_load_statistics( gRA.func ) ;
    PLUTO_dset_redisplay( gRA.func ) ;

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Callback to hide windows.
**
**----------------------------------------------------------------------
 */
static void
r_any_cb_hide( 
	Widget w,
	char *    client_data,
	XtPointer call_data
	)
{
    ENTRY("r_any_cb_hide");

    if      ( ! strcmp( client_data, "INT" ) )
	XtUnmapWidget( gRI.main );
    else if ( ! strcmp( client_data, "HL" ) )
	XtUnmapWidget( gRH.main );
    else if ( ! strcmp( client_data, "wtgr" ) )
	XtUnmapWidget( gRX.wtgr_main );
    else if ( ! strcmp( client_data, "saveas" ) )
	XtUnmanageChild( gRX.save_as_file_d );
    else if ( ! strcmp( client_data, "all" ) )
    {
	XtUnmapWidget( gRI.main );
	XtUnmapWidget( gRH.main );
	XtUnmapWidget( gRX.wtgr_main );
	XtUnmanageChild( gRX.save_as_file_d );
    }
    else
    {
	sprintf( gRmessage, "rach_10 : client_data is '%s'\n", client_data );
	rERROR( gRmessage );
    }

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Callback to raise windows.
**
**----------------------------------------------------------------------
 */
static void
r_any_cb_raise( 
	Widget w,
	char    * client_data,
	XtPointer call_data
	)
{
    ENTRY("r_any_cb_raise");

    if      ( ! strcmp( client_data, "INT" ) )
    {
	XMapRaised( XtDisplay( gRI.main ) , XtWindow( gRI.main ) ); NI_sleep(1);
    }
    else if ( ! strcmp( client_data, "HL" ) )
    {
	XMapRaised( XtDisplay( gRH.main ) , XtWindow( gRH.main ) ); NI_sleep(1);
    }
    else if ( ! strcmp( client_data, "wtgr" ) )
    {
	XMapRaised( XtDisplay( gRX.wtgr_main ) , XtWindow( gRX.wtgr_main ) ); NI_sleep(1);
    }
    else if ( ! strcmp( client_data, "saveas" ) )
    {
	XtManageChild( gRX.save_as_file_d );
	XtPopup( XtParent( gRX.save_as_file_d ), XtGrabNone ); NI_sleep(1);
    }
    else if ( ! strcmp( client_data, "all" ) )
    {
	XMapRaised( XtDisplay( gRI.main ) , XtWindow( gRI.main ) ); NI_sleep(1);
	XMapRaised( XtDisplay( gRH.main ) , XtWindow( gRH.main ) ); NI_sleep(1);
	XMapRaised( XtDisplay( gRX.wtgr_main ) , XtWindow( gRX.wtgr_main ) ); NI_sleep(1);
	XtManageChild( gRX.save_as_file_d );
	XtPopup( XtParent( gRX.save_as_file_d ), XtGrabNone ); NI_sleep(1);
    }
    else
    {
	sprintf( gRmessage, "racr_10 : client_data is '%s'\n", client_data );
	rERROR( gRmessage );
    }

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Callback to undo previous action (restore fdata from undo_data).
**
**----------------------------------------------------------------------
 */
static void
r_any_cb_undo( 
	Widget    w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    short * fptr, * uptr;
    int     count;

    ENTRY("r_any_cb_undo");

    fptr = gRA.fdata; uptr = gRA.undo_data;
    if ( !fptr || !uptr )
    {
        fprintf(stderr,"** undo without pointers: (%p,%p)\n",fptr,uptr);
        EXRETURN;
    }

    for ( count = 0; count < gRA.nvox; count++ )
	*fptr++ = *uptr++;

    THD_load_statistics( gRA.func ) ;
    PLUTO_dset_redisplay( gRA.func ) ;

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Callback to interpolate between user drawn lines.
**
**----------------------------------------------------------------------
 */
static void
r_INT_cb_fill(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    r_ipt_t    sourcep, destp;
    interp_s * I = (interp_s *)client_data;
    points_t * lb, * sb;
    double     index_ratio, tot_dist;
    double     dx, dy, dz, dcurr;
    double     fx, fy, fz;
    short    * fnptr = gRA.fdata, * sptr;
    short    * fptr, * uptr;
    int      * ip1;
    int        count, dest_index, coord;
    int        nx = gRA.nx, ny = gRA.ny, nz = gRA.nz;

    ENTRY("r_INT_cb_fill");

    if ( ! gRA.fdata )
    {
	fprintf( stderr, "%c", 7 );
	EXRETURN;
    }

    if ( ! I )
    {
	rERROR( "Error : entered r_INT_cb_fill() without client_data." );
	EXRETURN;
    }

    /* check that neither border is empty */
    if ( I->A.used <= 0 || I->B.used <= 0 )
    {
	fprintf( stderr, "%c", 7 );
	rWARNING( "Missing bounding curve for interpolation." );
	EXRETURN;
    }

    /* first store to undo_data */
    fptr = gRA.fdata; uptr = gRA.undo_data;
    for ( count = 0; count < gRA.nvox; count++ )
	*uptr++ = *fptr++;

    if ( I->A.used >= I->B.used )
    {
	lb = &I->A;
	sb = &I->B;
	index_ratio = I->B.used / (double)I->A.used;
    }
    else
    {
	lb = &I->B;
	sb = &I->A;
	index_ratio = I->A.used / (double)I->B.used;
    }

    for ( count = 0, ip1 = lb->points; count < lb->used; count++, ip1++ )
    {
	sourcep    = r_index2pt( *ip1, nx, ny, nz );
	dest_index = count * index_ratio + 0.0001;
	destp      = r_index2pt( sb->points[dest_index], nx, ny, nz );

	tot_dist   = r_p_distance( sourcep, destp );
	dx         = R_DIST_STEP * ( destp.x - sourcep.x ) / tot_dist;
	dy         = R_DIST_STEP * ( destp.y - sourcep.y ) / tot_dist;
	dz         = R_DIST_STEP * ( destp.z - sourcep.z ) / tot_dist;
	fx         = sourcep.x + dx;
	fy         = sourcep.y + dy;
	fz         = sourcep.z + dz;

	for ( dcurr = R_DIST_STEP; dcurr < tot_dist; dcurr += R_DIST_STEP )
	{
	    coord = (int)fx + nx * ( (int)fy + ny  * (int)fz );

	    sptr = fnptr + coord;

	    /* if ( ! *sptr )   -  draw through anything? */
	    *sptr = I->fill_val;

	    fx += dx;
	    fy += dy;
	    fz += dz;
	}
    }

    THD_load_statistics( gRA.func );
    PLUTO_dset_redisplay( gRA.func );
    dset_changed = 1;

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Compute the distance between two points.
**
**----------------------------------------------------------------------
 */
static double
r_p_distance( r_ipt_t p1, r_ipt_t p2 )
{
    return( sqrt( ( p1.x - p2.x ) * ( p1.x - p2.x ) +
		  ( p1.y - p2.y ) * ( p1.y - p2.y ) +
		  ( p1.z - p2.z ) * ( p1.z - p2.z ) )
	  );
}


/*----------------------------------------------------------------------
**
**  Convert the coordinate to a point.
**
**  We assume coord is of the form coord = X + nx * ( Y + ny * Z ).
**  The resulting point is P = { X, Y, Z }.
**
**----------------------------------------------------------------------
 */
static r_ipt_t
r_index2pt( int coord, int nx, int ny, int nz )
{
    r_ipt_t p = { 0, 0, 0 };
    int     tmp;

    ENTRY("r_index2pt");

    if ( coord < 0 )
    {
	sprintf( gRmessage, "Coordinate %d is out of range!\n", coord );
	rERROR( gRmessage );
	RETURN(p);
    }

    p.x = coord % nx;
    tmp = coord / nx;
    p.y = tmp   % ny;
    p.z = tmp   / ny;

    if ( p.z >= nz )
    {
	sprintf( gRmessage, "Coordinate %d is out of range!\n"
		 "Associated point is (%d,%d,%d).\n",
		 coord, p.x, p.y, p.z );
	rERROR( gRmessage );
    }

    RETURN(p);
}


/*----------------------------------------------------------------------
**
**  Initialize the point connection structure.
**
**  structure : 
**
** typedef struct
** {
**     r_ipt_t   source;
**     r_ipt_t   dest;
** 
**     int       cur_pt;        * flag denoting first or second point *
** } r_pt_conn_s;
**
**----------------------------------------------------------------------
 */
static int
r_init_pt_conn_s( r_pt_conn_s * P )
{
    r_ipt_t p = { 0, 0, 0 };

    P->cur_pt = 1;              /* may be either 1 or 2            */
    P->source = p;              /* just to initialize to SOMEthing */
    P->dest   = p;

    P->plist.points = NULL;     /* init border structure */
    P->plist.used   = 0;
    P->plist.M      = 0;

    return 1;
}


/*----------------------------------------------------------------------
**
**  Initialize the Algorithm values.
**
**  structure : 
**
**  typedef struct
**  {
**      Widget    main;         * main application widget *
**      Widget    mainRC;       * main rowcolumn widget   *
**
**      int       fill_val;
**      int       afni_undo;
**
**      points_t  A, B;         * two point structures for interpolation *
**  } interp_s;
**
**----------------------------------------------------------------------
 */
static int
r_init_interp_vals( interp_s * I )
{
    ENTRY("r_init_interp_vals");

    I->fill_val  = 2;
    I->afni_undo = 0;

    I->A.used   = 0;
    I->A.M      = 100;
    I->A.points = (int *)malloc( I->A.M * sizeof( int ) );

    if ( I->A.points == NULL )
    {
	fprintf( stderr, "\nError: riiv10\n"
		 "Failed to allocate %d ints for interpolation struct.\n\n",
		 I->A.M );
	RETURN(0);
    }

    I->B.used   = 0;
    I->B.M      = 100;
    I->B.points = (int *)malloc( I->B.M * sizeof( int ) );

    if ( I->B.points == NULL )
    {
	fprintf( stderr, "\nError: riiv20\n"
		 "Failed to allocate %d ints for interpolation struct.\n\n",
		 I->B.M );
	RETURN(0);
    }

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Create main filling widgets.
**
**----------------------------------------------------------------------
 */
static Widget
r_wt_mk_main_frame( r_X_s * X, Widget parent )
{
    Widget    frame, rc, label;
    XmString  xstring;
    Arg       al[ 20 ];
    int       ac;

    ENTRY("r_wt_mk_main_frame");

    ac = 0;
    XtSetArg( al[ ac ], XmNmarginHeight, 3 );  ac++;
    frame = XmCreateFrame( parent, "frame", al, ac );

    ac = 0;
    XtSetArg( al[ ac ], XmNlabelString,
		XmStringCreate( "White Matter Fill :", X->charset ) );  ac++;
    XtSetArg( al[ ac ], XmNchildType, XmFRAME_TITLE_CHILD );  ac++;
    XtSetArg( al[ ac ], XmNchildVerticalAlignment,
		XmALIGNMENT_BASELINE_BOTTOM);  ac++;
    label = XmCreateLabel( frame, "label", al, ac );
    XtManageChild( label );
    
    ac = 0;
    rc = XmCreateRowColumn( frame, "rowcolumn", al, ac );

    ( void )r_wt_mk_range_fr      ( X, rc );
    ( void )r_wt_mk_fillval_fr    ( X, rc );
    ( void )r_wt_mk_nbrs_fr       ( X, rc );
    ( void )r_wt_mk_diag_conn_fr  ( X, rc );
    ( void )r_wt_mk_strong_bord_fr( X, rc );

    r_wt_mk_fill_buttons( X, rc );


    XtManageChild( rc );
    XtManageChild( frame );

    RETURN(frame);
}


/*----------------------------------------------------------------------
**
**  Create the main gray matter function frame.
**
**----------------------------------------------------------------------
 */
static Widget
r_gr_mk_main_frame( r_X_s * X, Widget parent )
{
    Widget    frame, rc, label;
    XmString  xstring;
    Arg       al[ 20 ];
    int       ac;

    ENTRY("r_gr_mk_main_frame");

    ac = 0;
    XtSetArg( al[ ac ], XmNmarginHeight, 3 );  ac++;
    frame = XmCreateFrame( parent, "frame", al, ac );

    ac = 0;
    XtSetArg( al[ ac ], XmNlabelString,
		XmStringCreate( "Gray Matter Fill :", X->charset ) );  ac++;
    XtSetArg( al[ ac ], XmNchildType, XmFRAME_TITLE_CHILD );  ac++;
    XtSetArg( al[ ac ], XmNchildVerticalAlignment,
		XmALIGNMENT_BASELINE_BOTTOM);  ac++;
    label = XmCreateLabel( frame, "label", al, ac );
    XtManageChild( label );
    
    ac = 0;
    rc = XmCreateRowColumn( frame, "rowcolumn", al, ac );


    ( void )r_gr_mk_range_fr    ( X, rc );
    ( void )r_gr_mk_fillval_fr  ( X, rc );
    ( void )r_gr_mk_max_dist_w  ( X, rc );
    ( void )r_gr_mk_fill_buttons( X, rc );



    XtManageChild( rc );
    XtManageChild( frame );

    RETURN(frame);
}


/*----------------------------------------------------------------------
**
**  Initialize the Algorithm values.
**
**----------------------------------------------------------------------
 */
static int
r_init_Alg_values( r_alg_s * A )
{
    ENTRY("r_init_Alg_values");

    A->point_value       = 75;
    A->point_coord       = -1;  /* means user has not yet selected a point */
    A->adjust_point      = 0;

    strcpy( A->save_as_name, "default" );

    A->wt_fill_val       = 5;
    A->wt_range_min      = r_wtgr_calc_min_frm_val( A->point_value );
    A->wt_range_max      = r_wtgr_calc_max_frm_val( A->point_value );
    A->wt_diag_connect   = 0;   /* any connection */

    A->gr_fill_val       = 10;
    A->gr_range_min      = r_wtgr_calc_min_frm_val( 42 );
    A->gr_range_max      = r_wtgr_calc_max_frm_val( 42 );
    A->gr_max_dist       = 4;

    A->anat              = plint->im3d->anat_now;

    fprintf(stderr,"r_init_Alg_values(): A->anat = %p\n",A->anat);
    if ( A->anat )
    {
        if ( ! DSET_LOADED(A->anat) )
        {
            DSET_load( A->anat );
            fprintf(stderr,"-d loading anat...\n");
        }
	A->adata = (short *)DSET_BRICK_ARRAY( A->anat, 0 );
	/*fprintf(stderr,"Set A->adata to %p\n",A->adata);*/
    }
    
    /*
    ** Create boundary memory and check for malloc success.
    */
    A->Bold.used   = 0;
    A->Bold.M      = 1000;
    A->Bold.points = (int *)malloc( A->Bold.M * sizeof( int ) );

    if ( A->Bold.points == NULL )
    {
	fprintf( stderr, "Error: riAv10\n"
		 "Failed to allocate %d ints for boundary.\n", A->Bold.M );
	RETURN(0);
    }

    A->Bnew.used   = 0;
    A->Bnew.M      = 1000;
    A->Bnew.points = (int *)malloc( A->Bnew.M * sizeof( int ) );

    if ( A->Bnew.points == NULL )
    {
	fprintf( stderr, "\nError: riAv20\n"
		 "Failed to allocate %d ints for boundary.\n\n", A->Bnew.M );
	RETURN(0);
    }

    A->border.used   = 0;
    A->border.M      = 1000;
    A->border.points = (int *)malloc( A->border.M * sizeof( int ) );

    if ( A->border.points == NULL )
    {
	fprintf( stderr, "\nError: riAv30\n"
		 "Failed to allocate %d ints for boundary.\n\n",
		 A->border.M );
	RETURN(0);
    }

    /* we will initialize this after we know we have data */
    A->neighbors = NULL;                /* - will be nvox */
    A->undo_data = NULL;                /* - will be nvox */

    A->min_nbrs  = 0;
    A->strong_borders = 1;


    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Initialize the afni values.
**
**----------------------------------------------------------------------
 */
static void
r_init_afni_vars( r_alg_s * A, THD_3dim_dataset * func )
{
    ENTRY("r_init_afni_vars");

    A->anat              = plint->im3d->anat_now;

    if ( func )
    {
	if ( DSET_BRICK_TYPE( func, 0 ) != MRI_short )
	{
	    ( void )MCW_popup_message( gRX.main,
			"Serious error.\n"
			"\n"
			"Functional data is not of\n"
			"type short.  Please choose\n"
			"another dataset.\n",
			MCW_USER_KILL | MCW_TIMER_KILL );
	    EXRETURN;
	}

	A->func   = func;

	A->factor = DSET_BRICK_FACTOR( A->func, 0 );
	A->factor = ( A->factor == 0.0 ) ? 1.0 : A->factor;
	A->fdata  = (short *)DSET_BRICK_ARRAY( A->func, 0 );

	A->nx     = DSET_NX( A->func );
	A->ny     = DSET_NY( A->func );
	A->nz     = DSET_NZ( A->func );
	A->nxy    = A->nx * A->ny;
	A->nvox   = A->nx * A->ny * A->nz;

	if ( A->neighbors == NULL )
	{
	    A->neighbors = malloc( A->nvox * sizeof( short ) );
	    if ( A->neighbors == NULL )
	    {
		fprintf( stderr, "\nError: riav10\n"
		"Failed to allocate %d ints for neighbors.\n\n", A->nvox );
	    }

	    A->undo_data = calloc( A->nvox, sizeof( short ) );
	    if ( A->undo_data == NULL )
	    {
		fprintf( stderr, "\nError: riav20\n"
		"Failed to allocate %d ints for undo_data.\n\n", A->nvox );
	    }
	}
    }
    else
	( void )MCW_popup_message( gRX.main,
		    "Serious error.\n"
		    "\n"
		    "Missing functional dataset.\n"
		    "Please choose another dataset.\n",
		    MCW_USER_KILL | MCW_TIMER_KILL );

    fprintf(stderr,"r_init_afni_vars(): A->anat = %p\n",A->anat);
    if ( A->anat )
    {
	if ( DSET_BRICK_TYPE( A->anat, 0 ) != MRI_short )
	{
	    ( void )MCW_popup_message( gRX.main,
			"Serious error.\n"
			"\n"
			"Anatomical data is not of\n"
			"type short.  Please choose\n"
			"another dataset.\n",
			MCW_USER_KILL | MCW_TIMER_KILL );
	    EXRETURN;
	}

        if ( ! DSET_LOADED(A->anat) )
        {
            fprintf(stderr,"-d loading anat...\n");
            DSET_load( A->anat );
        }

	A->adata = (short *)DSET_BRICK_ARRAY( A->anat, 0 );
	/*fprintf(stderr,"Set A->adata to %p\n",A->adata);*/
    }
    else
	( void )MCW_popup_message( gRX.main,
		    "Serious error.\n"
		    "\n"
		    "Missing anatomical dataset.\n"
		    "Please choose another dataset.\n",
		    MCW_USER_KILL | MCW_TIMER_KILL );

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Callback to output statistics on selected matter.
**
**----------------------------------------------------------------------
 */
static void
r_any_cb_fill_stats(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    short * fptr = gRA.fdata;
    short * aptr = gRA.adata;
    int     fillval = *(int *)client_data;
    int     count, min = 30000, max = -30000, size = 0;

    ENTRY("r_any_cb_fill_stats");

    if ( ! gRA.fdata )
    {
	fputc( 7, stderr );      /* hard-code a beep */
	EXRETURN;
    }

    for ( count = 0; count < gRA.nvox; count++ )
    {
	if ( *fptr == fillval )
	{
	    if ( *aptr > max )
		max = *aptr;
	    if ( *aptr < min )
		min = *aptr;
	    size++;
	}

	aptr++;
	fptr++;
    }
	
    printf( "------------------------------------------------------------\n" );

    if ( size > 0 )
    {
	printf( "fill region min  = %d\n", min  );
	printf( "fill region max  = %d\n", max  );
	printf( "fill region size = %d\n", size );
	printf( "\n" );

	r_histogram( &gRA, min, max, fillval );
    }
    else
	rWARNING( "Fill region is empty." );

    printf( "------------------------------------------------------------\n" );

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Make a histogram from the selected data.
**
**----------------------------------------------------------------------
 */
static void
r_histogram( r_alg_s * A, int min, int max, int check_val )
{
    int   * hist;

    short * fptr = gRA.fdata;
    short * aptr = gRA.adata;
    int     count, size = max - min + 1, total = 0;

    ENTRY("r_histogram");

    if ( ( hist = (int *)malloc( size * sizeof( int ) ) ) == NULL )
    {
	sprintf( gRmessage, "Error: pr_rh_10\n"
		 "Failed alloca for %d ints.", size );
	rWARNING( gRmessage );
	EXRETURN;
    }

    for ( count = 0; count < size; count++ )
	hist[count] = 0;

    for ( count = 0; count < A->nvox; count++ )
    {
	if ( *fptr == check_val )
	{
	    hist[*aptr - min]++;
	    total++;
	}

	aptr++;
	fptr++;
    }
	
    printf( "  value   \t   nvox    \t    %%\n" );
    printf( "--------- \t --------- \t --------- \n" );

    for ( count = 0; count < size; count++)
	printf( " %6d   \t %7d   \t  %7.3f\n",
		min + count, hist[ count ], 100.0 * hist[ count ] / total );
    free(hist);
    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Callback to toggle the strong borders flag.
**
**----------------------------------------------------------------------
*/
static void
r_wt_cb_SB_toggle(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    Boolean set;
    Arg     al[ 10 ];
    int     ac;

    ac = 0;
    XtSetArg( al[ ac ], XmNset, &set );  ac++;
    XtGetValues( w, al, ac );

    if ( set )
	gRA.strong_borders = 1;
    else
	gRA.strong_borders = 0;

    return;
}


/*----------------------------------------------------------------------
**
**  Given an integer value, return the suggested minimum search value.
**
**----------------------------------------------------------------------
 */
static int
r_wtgr_calc_min_frm_val( int value )
{
    return( (int)(0.94 * value - 5) );
}


/*----------------------------------------------------------------------
**
**  Given an integer value, return the suggested minimum search value.
**
**----------------------------------------------------------------------
 */
static int
r_wtgr_calc_max_frm_val( int value )
{
    return( (int)(1.06 * value + 5) );
}


/*----------------------------------------------------------------------
**
**  Hide the main white/gray matter finder.
**
**----------------------------------------------------------------------
 */
static void
r_wtgr_cb_hide( void )
{
    XtUnmapWidget( gRX.wtgr_main );
}


/*----------------------------------------------------------------------
**
**  Callback to suggest range limits for white AND gray matter search.
**
**  Values will be set in the wt_range_min_w and wt_range_max_w widgets.
**
**----------------------------------------------------------------------
 */
static void
r_wtgr_cb_suggest_limits(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    Widget minw, maxw;
    char * cdptr = (char *)client_data;
    char   string[ 10 ] = "";
    Arg    al[ 10 ];
    int    ac, min, max;

    ENTRY("r_wtgr_cb_suggest_limits");

    if ( ! cdptr )
    {
	fprintf( stderr,
		 "Entered r_wtgr_cb_suggest_limits() without a type.\n" );
	EXRETURN;
    }

    min = r_wtgr_calc_min_frm_val( gRA.point_value );
    max = r_wtgr_calc_max_frm_val( gRA.point_value );

    if ( ! strcmp( cdptr, "white" ) )
    {
	gRA.wt_range_min = min;         /* apply the new values */
	gRA.wt_range_max = max;

	minw = gRX.wt_range_min_w;      /* set the widgets to be updated */
	maxw = gRX.wt_range_max_w;
    }
    else        /* then gray */
    {
	max = min - 1;                  /* assume the gray starts below white */
	min = 0.60 * min;

	gRA.gr_range_max = max;
	gRA.gr_range_min = min;

	minw = gRX.gr_range_min_w;
	maxw = gRX.gr_range_max_w;
    }

    /* set the values into the range widgets */

    sprintf( string, "%d", min );

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetValues( minw, al, ac );


    sprintf( string, "%d", max );

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetValues( maxw, al, ac );

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Create boundary (gray matter) buttons.
**
**----------------------------------------------------------------------
 */
static Widget
r_gr_mk_fill_buttons( r_X_s * X, Widget parent )
{
    int      ac;
    Arg      al[ 10 ];
    Widget   form, frame, button;
    XmString xstr;

    ENTRY("r_gr_mk_fill_buttons");

    /* create frame to hold form and buttons */
    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );

    /* create form to hold push buttons */
    ac = 0;
    XtSetArg( al[ac], XmNhorizontalSpacing, R_BUTTON_SPACE );  ac++;
    form = XmCreateForm( frame, "form", al, ac );


    /* region fill button */
    ac = 0;
    xstr = XmStringCreateLtoR( "FILL", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button = XmCreatePushButton( form, "fill", al, ac );
    XtManageChild( button );
    XtAddCallback( button, XmNactivateCallback,
		   ( XtCallbackProc )r_gr_cb_fill, NULL );
    XtSetSensitive( button, True );
    XmStringFree( xstr );
    MCW_register_hint( button , "fill the gray matter region" );

    /* unfill button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button );  ac++;
    xstr = XmStringCreateLtoR( "unfill", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button = XmCreatePushButton( form, "unfill", al, ac );
    XtManageChild( button );
    XtAddCallback( button, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_unfill, &gRA.gr_fill_val );
    XtSetSensitive( button, True );
    XmStringFree( xstr );
    MCW_register_hint( button , "unfill the \"fill value\" region" );


    /* suggest range button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button );  ac++;
    xstr = XmStringCreateLtoR( "suggest range", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button = XmCreatePushButton( form, "suggest_range", al, ac );
    XtManageChild( button );
    XtAddCallback( button, XmNactivateCallback,
		   ( XtCallbackProc )r_wtgr_cb_suggest_limits, "gray" );
    XtSetSensitive( button, True );
    XmStringFree( xstr );
    MCW_register_hint( button , "suggest a range for gray matter values" );


    /* stats button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button );  ac++;
    xstr = XmStringCreateLtoR( "stats", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button = XmCreatePushButton( form, "stats", al, ac );
    XtManageChild( button );
    XtAddCallback( button, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_fill_stats, &gRA.gr_fill_val );
    XtSetSensitive( button, True );
    XmStringFree( xstr );
    MCW_register_hint( button , "get stats for this \"fill value\"" );


    /* hide window button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button );  ac++;
    xstr = XmStringCreateLtoR( "hide", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button = XmCreatePushButton( form, "hide", al, ac );
    XtManageChild( button );
    XtAddCallback( button, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_hide, "wtgr" );
    XtSetSensitive( button, True );
    XmStringFree( xstr );
    MCW_register_hint( button , "temporarily close this window" );


    XtManageChild( form );
    XtManageChild( frame );

    RETURN( frame );
}


/*----------------------------------------------------------------------
**
**  Create FILL type buttons for the white matter search.
**
**----------------------------------------------------------------------
 */
static void
r_wt_mk_fill_buttons( r_X_s * X, Widget parent )
{
    int      ac;
    Arg      al[ 10 ];
    Widget   form, frame, button1, button2, button3;
    XmString xstr;

    ENTRY("r_wt_mk_fill_buttons");

    /* create frame to hold form and buttons */
    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );

    /* create form to hold push buttons */
    ac = 0;
    XtSetArg( al[ac], XmNhorizontalSpacing, R_BUTTON_SPACE );  ac++;
    form = XmCreateForm( frame, "form", al, ac );


    /* region fill button */
    ac = 0;
    xstr = XmStringCreateLtoR( "FILL", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button1 = XmCreatePushButton( form, "fill", al, ac );
    XtManageChild( button1 );
    XtAddCallback( button1, XmNactivateCallback,
		   ( XtCallbackProc )r_wt_cb_fill, NULL );
    XtSetSensitive( button1, True );
    XmStringFree( xstr );
    MCW_register_hint( button1 , "fill the white matter region" );


    /* MORE region fill button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button1 );  ac++;
    xstr = XmStringCreateLtoR( "MORE", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button3 = XmCreatePushButton( form, "more", al, ac );
    XtManageChild( button3 );
    XtAddCallback( button3, XmNactivateCallback,
		   ( XtCallbackProc )r_wt_cb_fill, "0" );
    XtSetSensitive( button3, True );
    XmStringFree( xstr );
    MCW_register_hint( button3 , "fill more white matter (no unfill)" );


    /* unfill button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button3 );  ac++;
    xstr = XmStringCreateLtoR( "unfill", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button2 = XmCreatePushButton( form, "unfill", al, ac );
    XtManageChild( button2 );
    XtAddCallback( button2, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_unfill, &gRA.wt_fill_val );
    XtSetSensitive( button2, True );
    XmStringFree( xstr );
    MCW_register_hint( button2 , "unfill all matter for the \"fill value\"" );


    /* suggest range button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button2 );  ac++;
    xstr = XmStringCreateLtoR( "suggest range", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button1 = XmCreatePushButton( form, "suggest_range", al, ac );
    XtManageChild( button1 );
    XtAddCallback( button1, XmNactivateCallback,
		   ( XtCallbackProc )r_wtgr_cb_suggest_limits, "white" );
    XtSetSensitive( button1, True );
    XmStringFree( xstr );
    MCW_register_hint( button1 , "suggest a white matter range" );


    /* stats button */
    ac = 0;
    XtSetArg( al[ac], XmNleftAttachment, XmATTACH_WIDGET );  ac++;
    XtSetArg( al[ac], XmNleftWidget, button1 );  ac++;
    xstr = XmStringCreateLtoR( "stats", gRX.charset );
    XtSetArg( al[ac], XmNlabelString, xstr );  ac++;
    button2 = XmCreatePushButton( form, "stats", al, ac );
    XtManageChild( button2 );
    XtAddCallback( button2, XmNactivateCallback,
		   ( XtCallbackProc )r_any_cb_fill_stats, &gRA.wt_fill_val );
    XtSetSensitive( button2, True );
    XmStringFree( xstr );
    MCW_register_hint( button2 , "get stats for this \"fill value\"" );


    XtManageChild( form );
    XtManageChild( frame );

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Create the diagonal connections frame.
**
**  This will be a radio button widget for the user flag.
**
**----------------------------------------------------------------------
 */
static Widget
r_wt_mk_diag_conn_fr( r_X_s * X, Widget parent )
{
    Widget   junk, rc, frame;
    XmString xstr;
    Arg      al[ 10 ];
    int      ac;
    char     string[ 15 ];

    ENTRY("r_wt_mk_diag_conn_fr");

    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );

    ac = 0;
    XtSetArg( al[ ac ], XmNpacking, XmPACK_TIGHT );  ac++;
    XtSetArg( al[ ac ], XmNorientation, XmHORIZONTAL );  ac++;
    rc = XmCreateRowColumn( frame, "rowcolumn", al, ac );

    ac = 0;
    xstr = XmStringCreateLtoR( "connection constraint (0-2) : ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateLabel( rc, "label", al, ac );
    XtManageChild( junk );
    XmStringFree( xstr );


    sprintf( string, "%d", gRA.wt_diag_connect );

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 80 );  ac++;
    X->wt_diag_conn_w = XmCreateText( rc, "text", al, ac );
    XtManageChild( X->wt_diag_conn_w );
    XtAddCallback( X->wt_diag_conn_w, XmNactivateCallback,
	    ( XtCallbackProc )r_wt_cb_set_diag_conn, NULL );
    XtAddCallback( X->wt_diag_conn_w, XmNlosingFocusCallback,
	    ( XtCallbackProc )r_wt_cb_set_diag_conn, NULL );


    XtManageChild( rc );
    XtManageChild( frame );

    RETURN( frame );
}

/*----------------------------------------------------------------------
**
**  Create the strong borders frame.
**
**  This will be a radio button widget.
**
**----------------------------------------------------------------------
 */
static Widget
r_wt_mk_strong_bord_fr( r_X_s * X, Widget parent )
{
    Widget   junk, frame;
    XmString xstr;
    Arg      al[ 10 ];
    int      ac;
    char     string[ 15 ];

    ENTRY("r_wt_mk_strong_bord_fr");

    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );
    
    ac = 0;
    XtSetArg( al[ ac ], XmNset,     True );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 1 ); ac++;
    XtSetArg( al[ ac ], XmNspacing, 1 ); ac++;
    XtSetArg( al[ ac ], XmNmarginLeft, 1 ); ac++;
    xstr = XmStringCreateLtoR( "strong borders                ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateToggleButton( frame, "toggle", al, ac );
    XtManageChild( junk );
    XtAddCallback( junk, XmNvalueChangedCallback, r_wt_cb_SB_toggle, NULL);
    XmStringFree( xstr );

    XtManageChild( frame );

    RETURN( frame );
}


/*----------------------------------------------------------------------
**
**  Create the min_nbrs (minimum neighbors) frame.
**
**----------------------------------------------------------------------
 */
static Widget
r_wt_mk_nbrs_fr( r_X_s * X, Widget parent )
{
    Widget   junk, rc, frame;
    XmString xstr;
    Arg      al[ 10 ];
    int      ac;
    char     string[ 15 ];

    ENTRY("r_wt_mk_nbrs_fr");

    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );
    
    ac = 0;
    XtSetArg( al[ ac ], XmNpacking, XmPACK_TIGHT );  ac++;
    XtSetArg( al[ ac ], XmNorientation, XmHORIZONTAL );  ac++;
    rc = XmCreateRowColumn( frame, "rowcolumn", al, ac );

    ac = 0;
    xstr = XmStringCreateLtoR( "neighbors  constraint (0-6) : ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateLabel( rc, "label", al, ac );
    XtManageChild( junk );
    XmStringFree( xstr );


    sprintf( string, "%d", gRA.min_nbrs );

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 80 );  ac++;
    X->wt_min_nbrs_w = XmCreateText( rc, "text", al, ac );
    XtManageChild( X->wt_min_nbrs_w );
    XtAddCallback( X->wt_min_nbrs_w, XmNactivateCallback, 
	    ( XtCallbackProc )r_wt_cb_set_min_nbrs, NULL );
    XtAddCallback( X->wt_min_nbrs_w, XmNlosingFocusCallback, 
	    ( XtCallbackProc )r_wt_cb_set_min_nbrs, NULL );
    
    
    XtManageChild( rc );
    XtManageChild( frame );

    RETURN( frame );
}


/*----------------------------------------------------------------------
**
**  Create the frame to handle max distance in the gray search.
**
**----------------------------------------------------------------------
 */
static Widget
r_gr_mk_max_dist_w( r_X_s * X, Widget parent )
{
    Widget   junk, rc, frame;
    XmString xstr;
    Arg      al[ 10 ];
    int      ac;
    char     string[ 15 ];

    ENTRY("r_gr_mk_max_dist_w");

    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );
    
    ac = 0;
    XtSetArg( al[ ac ], XmNpacking, XmPACK_TIGHT );  ac++;
    XtSetArg( al[ ac ], XmNorientation, XmHORIZONTAL );  ac++;
    rc = XmCreateRowColumn( frame, "rowcolumn", al, ac );

    ac = 0;
    xstr = XmStringCreateLtoR( "max distance : ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateLabel( rc, "label", al, ac );
    XtManageChild( junk );
    XmStringFree( xstr );


    sprintf( string, "%d", gRA.gr_max_dist );

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 80 );  ac++;
    junk = XmCreateText( rc, "text", al, ac );
    XtManageChild( junk );
    XtAddCallback( junk, XmNactivateCallback, 
	    ( XtCallbackProc )r_gr_cb_set_max_dist, NULL );
    XtAddCallback( junk, XmNlosingFocusCallback, 
	    ( XtCallbackProc )r_gr_cb_set_max_dist, NULL );
    
    
    XtManageChild( rc );
    XtManageChild( frame );

    RETURN( frame );
}


/*----------------------------------------------------------------------
**
**  Create the "fill value" widgets.
**
**----------------------------------------------------------------------
 */
static Widget
r_wt_mk_fillval_fr( r_X_s * X, Widget parent )
{
    Widget   junk, rc, frame;
    XmString xstr;
    Arg      al[ 10 ];
    int      ac;
    char     string[ 15 ];


    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );
    
    ac = 0;
    XtSetArg( al[ ac ], XmNpacking, XmPACK_TIGHT );  ac++;
    XtSetArg( al[ ac ], XmNorientation, XmHORIZONTAL );  ac++;
    rc = XmCreateRowColumn( frame, "rowcolumn", al, ac );

    ac = 0;
    xstr = XmStringCreateLtoR( "fill value   : ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateLabel( rc, "label", al, ac );
    XtManageChild( junk );
    XmStringFree( xstr );


    sprintf( string, "%d", gRA.wt_fill_val );

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 80 );  ac++;
    X->wt_fill_val_w = XmCreateText( rc, "text", al, ac );
    XtManageChild( X->wt_fill_val_w );
    XtAddCallback( X->wt_fill_val_w, XmNactivateCallback, 
	    ( XtCallbackProc )r_wt_cb_set_fill_val, NULL );
    XtAddCallback( X->wt_fill_val_w, XmNlosingFocusCallback, 
	    ( XtCallbackProc )r_wt_cb_set_fill_val, NULL );
    
    
    XtManageChild( rc );
    XtManageChild( frame );

    return( frame );
}


/*----------------------------------------------------------------------
**
**  Create the "fill value" widget for the gray matter search.
**
**----------------------------------------------------------------------
 */
static Widget
r_gr_mk_fillval_fr( r_X_s * X, Widget parent )
{
    Widget   junk, rc, frame;
    XmString xstr;
    Arg      al[ 10 ];
    int      ac;
    char     string[ 15 ];


    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );
    
    ac = 0;
    XtSetArg( al[ ac ], XmNpacking, XmPACK_TIGHT );  ac++;
    XtSetArg( al[ ac ], XmNorientation, XmHORIZONTAL );  ac++;
    rc = XmCreateRowColumn( frame, "rowcolumn", al, ac );

    ac = 0;
    xstr = XmStringCreateLtoR( "fill value   : ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateLabel( rc, "label", al, ac );
    XtManageChild( junk );
    XmStringFree( xstr );


    sprintf( string, "%d", gRA.gr_fill_val );

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 80 );  ac++;
    X->gr_fill_val_w = XmCreateText( rc, "text", al, ac );
    XtManageChild( X->gr_fill_val_w );
    XtAddCallback( X->gr_fill_val_w, XmNactivateCallback, 
	    ( XtCallbackProc )r_gr_set_fill_val, NULL );
    XtAddCallback( X->gr_fill_val_w, XmNlosingFocusCallback, 
	    ( XtCallbackProc )r_gr_set_fill_val, NULL );
    
    
    XtManageChild( rc );
    XtManageChild( frame );

    return( frame );
}


/*----------------------------------------------------------------------
**
**  Create the gray matter range frame.
**
**----------------------------------------------------------------------
 */
static Widget
r_gr_mk_range_fr( r_X_s * X, Widget parent )
{
    Widget   junk, rc, frame;
    XmString xstr;
    Arg      al[ 10 ];
    int      ac;
    char     string[ 15 ];


    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );
    
    ac = 0;                     /* to hold labels and text */
    XtSetArg( al[ ac ], XmNpacking, XmPACK_TIGHT );  ac++;
    XtSetArg( al[ ac ], XmNorientation, XmHORIZONTAL );  ac++;
    rc = XmCreateRowColumn( frame, "rowcolumn", al, ac );

    ac = 0;
    xstr = XmStringCreateLtoR( "search range : ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateLabel( rc, "label", al, ac );
    XtManageChild( junk );
    XmStringFree( xstr );


    sprintf( string, "%d", gRA.gr_range_min );  /* init value */

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 80 );  ac++;
    X->gr_range_min_w = XmCreateText( rc, "text", al, ac );
    XtManageChild( X->gr_range_min_w );
    XtAddCallback( X->gr_range_min_w, XmNactivateCallback, 
	    ( XtCallbackProc )r_gr_cb_set_range, "from" );
    XtAddCallback( X->gr_range_min_w, XmNlosingFocusCallback, 
	    ( XtCallbackProc )r_gr_cb_set_range, "from" );
    
    ac = 0;
    xstr = XmStringCreateLtoR( " to ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateLabel( rc, "label", al, ac );
    XtManageChild( junk );
    XmStringFree( xstr );
    

    sprintf( string, "%d", gRA.gr_range_max );

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 80 );  ac++;
    X->gr_range_max_w = XmCreateText( rc, "text", al, ac );
    XtManageChild( X->gr_range_max_w );
    XtAddCallback( X->gr_range_max_w, XmNactivateCallback, 
	    ( XtCallbackProc )r_gr_cb_set_range, "to" );
    XtAddCallback( X->gr_range_max_w, XmNlosingFocusCallback, 
	    ( XtCallbackProc )r_gr_cb_set_range, "to" );
    
    XtManageChild( rc );
    XtManageChild( frame );

    return( frame );
}


/*----------------------------------------------------------------------
**
**  Create the range widgets.
**
**----------------------------------------------------------------------
 */
static Widget
r_wt_mk_range_fr( r_X_s * X, Widget parent )
{
    Widget   junk, rc, frame;
    XmString xstr;
    Arg      al[ 10 ];
    int      ac;
    char     string[ 15 ];


    ac = 0;
    frame = XmCreateFrame( parent, "frame", al, ac );
    
    ac = 0;                     /* to hold labels and text */
    XtSetArg( al[ ac ], XmNpacking, XmPACK_TIGHT );  ac++;
    XtSetArg( al[ ac ], XmNorientation, XmHORIZONTAL );  ac++;
    rc = XmCreateRowColumn( frame, "rowcolumn", al, ac );

    ac = 0;
    xstr = XmStringCreateLtoR( "search range : ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateLabel( rc, "label", al, ac );
    XtManageChild( junk );
    XmStringFree( xstr );


    sprintf( string, "%d", gRA.wt_range_min );  /* init value */

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 80 );  ac++;
    X->wt_range_min_w = XmCreateText( rc, "text", al, ac );
    XtManageChild( X->wt_range_min_w );
    XtAddCallback( X->wt_range_min_w, XmNactivateCallback, 
	    ( XtCallbackProc )r_wt_cb_set_range, "from" );
    XtAddCallback( X->wt_range_min_w, XmNlosingFocusCallback, 
	    ( XtCallbackProc )r_wt_cb_set_range, "from" );
    
    ac = 0;
    xstr = XmStringCreateLtoR( " to ", gRX.charset );
    XtSetArg( al[ ac ], XmNlabelString, xstr );  ac++;
    junk = XmCreateLabel( rc, "label", al, ac );
    XtManageChild( junk );
    XmStringFree( xstr );
    

    sprintf( string, "%d", gRA.wt_range_max );

    ac = 0;
    XtSetArg( al[ ac ], XmNvalue, string );  ac++;
    XtSetArg( al[ ac ], XmNwidth, 80 );  ac++;
    X->wt_range_max_w = XmCreateText( rc, "text", al, ac );
    XtManageChild( X->wt_range_max_w );
    XtAddCallback( X->wt_range_max_w, XmNactivateCallback, 
	    ( XtCallbackProc )r_wt_cb_set_range, "to" );
    XtAddCallback( X->wt_range_max_w, XmNlosingFocusCallback, 
	    ( XtCallbackProc )r_wt_cb_set_range, "to" );
    
    XtManageChild( rc );
    XtManageChild( frame );

    return( frame );
}


/*----------------------------------------------------------------------
**
**  Clear the filled region for the white matter search.
**
**----------------------------------------------------------------------
 */
static void
r_any_cb_unfill(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    short * fptr, * uptr;
    int     count;
    int     unfill_val = *((int *)client_data);

    ENTRY("r_any_cb_unfill");

    if ( ! gRA.fdata )
	EXRETURN;

    /* first store to undo_data */
    fptr = gRA.fdata; uptr = gRA.undo_data;
    for ( count = 0; count < gRA.nvox; count++ )
	*uptr++ = *fptr++;

    for ( count = 0, fptr = gRA.fdata; count < gRA.nvox; count++, fptr++ )
	if ( *fptr == unfill_val )
	    *fptr = 0;

    THD_load_statistics( gRA.func ) ;
    PLUTO_dset_redisplay( gRA.func ) ;

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Make the check to see if a user boundary neighbor exists.
** 
**  Okay neighbors values are zero and testval.
** 
**----------------------------------------------------------------------
 */
static int
r_wt_bad_ngbr_exists( r_alg_s * A, int current, int testval )
{
    int     last;
    int     nx, nxy;
    short   val;
    short * data;

    ENTRY("r_wt_bad_ngbr_exists");

    data = A->fdata + current;
    nx   = A->nx;
    nxy  = A->nxy;
    last = A->nvox - nxy;

    if ( ( current < nxy ) || ( current >= last ) )
	RETURN(1);

    val = data[ -1 ];
    if ( val && ( val != testval ) && ( val != R_BOUND_VAL )  )
	RETURN(1);

    val = data[ 1 ];
    if ( val && ( val != testval ) && ( val != R_BOUND_VAL )  )
	RETURN(1);

    val = data[ -nx ];
    if ( val && ( val != testval ) && ( val != R_BOUND_VAL )  )
	RETURN(1);

    val = data[ nx ];
    if ( val && ( val != testval ) && ( val != R_BOUND_VAL )  )
	RETURN(1);

    val = data[ -nxy ];
    if ( val && ( val != testval ) && ( val != R_BOUND_VAL )  )
	RETURN(1);

    val = data[ nxy ];
    if ( val && ( val != testval ) && ( val != R_BOUND_VAL )  )
	RETURN(1);

    RETURN(0);
}


/*----------------------------------------------------------------------
**
**  Make the check for adding a point to the border.
** 
**----------------------------------------------------------------------
 */
static int
r_wt_check_insert( r_alg_s * A, int current )
{
    short * aptr = A->adata;
    short * fptr = A->fdata;
    short * nptr = A->neighbors;
    short * fvp  = fptr + current;      /* pointer to function value */

    int value, added = 0;

    ENTRY("r_wt_check_insert");

    value = aptr[ current ];

    /* if new and in range, we will set as a result voxel */
    if ( *fvp == 0 )
    {
	if ( ( value >= A->wt_range_min ) &&
	     ( value <= A->wt_range_max ) )
	{
	    /* if it also has enough neighbors */
	    if ( nptr[ current ] >= A->min_nbrs )
	    {
		/* also check for neighbor in the user's boundary */
		if ( ( ! A->strong_borders ) ||
		     ! r_wt_bad_ngbr_exists( A, current, A->wt_fill_val ) )
		{
		    if ( ! r_add_to_boundary( &A->Bnew, current ) )
			RETURN(-1);
		    else
			added = 1;
		}
	    }

	    /* either way, set point as wt_fill_val */
	    *fvp = A->wt_fill_val;
	}
	else    /* add to the 3d boundary */
	{
	    *fvp = R_BOUND_VAL; /* mark as in boundary, clear later */

	    if ( ! r_add_to_boundary( &A->border, current ) )
		RETURN(-1);
	}
    }

    RETURN(added);
}


/*----------------------------------------------------------------------
**
**  Callback to fill the local region for a mask.
**
**----------------------------------------------------------------------
 */
static void
r_wt_cb_fill(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    short  * nptr, * fnptr, * flptr, * aptr, * uptr;
    int      count, value, current;
    int      nxy = gRA.nxy, nx = gRA.nx;
    char   * cp = (char *)client_data;
    points_t B;

    ENTRY("r_wt_cb_fill");

    /* check that we are ready to fill anything */
    if ( ( gRA.point_coord == -1 ) || ( ! gRA.fdata ) )
    {
	fputc( 7, stderr );      /* hard-code a beep */
	EXRETURN;
    }

    if ( !gRA.Bold.points || !gRA.Bnew.points ||
	   !gRA.neighbors || !gRA.undo_data)
    {
	fprintf( stderr, "Error: rcfr10\n"
		 "Memory failure, addresses are %p, %p, %p and %p.\n",
		 gRA.Bold.points, gRA.Bnew.points,
		 gRA.neighbors, gRA.undo_data );
	EXRETURN;
    }

    /* first store to undo_data */
    fnptr = gRA.fdata; uptr = gRA.undo_data;
    for ( count = 0; count < gRA.nvox; count++ )
	*uptr++ = *fnptr++;

    /* give the user an idea of what is happening */
    rWARNING( "filling white matter" );

    if ( ( cp == NULL ) || ( *cp != '0' ) )
    {
	for ( count = 0, fnptr = gRA.fdata; count < gRA.nvox; count++, fnptr++ )            if ( *fnptr == gRA.wt_fill_val )
		*fnptr = 0;
    }

    r_wt_set_neighbors( &gRA );

    /* set borders to nothing */
    gRA.Bold.used    = 0;
    gRA.Bnew.used    = 0;
    gRA.border.used  = 0;

    if ( r_wt_check_insert( &gRA, gRA.point_coord ) != 1 )
	EXRETURN;

    while ( gRA.Bnew.used > 0 )  /* while boundary exists */
    {
	B             = gRA.Bold; /* swap memory and reset Bnew.used to zero */
	gRA.Bold      = gRA.Bnew; /*    - this simply preserves the memory  */
	gRA.Bnew      = B;
	gRA.Bnew.used = 0;
	fputs( ".", stderr );

	for ( count = 0; count < gRA.Bold.used; count++ )
	{
	    current = gRA.Bold.points[count];

	    /* 6 'face sharing' points */
	    r_wt_check_insert( &gRA, current - 1 );
	    r_wt_check_insert( &gRA, current + 1 );
	    r_wt_check_insert( &gRA, current - nx );
	    r_wt_check_insert( &gRA, current + nx );
	    r_wt_check_insert( &gRA, current - nxy );
	    r_wt_check_insert( &gRA, current + nxy );

	    /* 12 'edge sharing' points */
	    if ( gRA.wt_diag_connect < 2 )
	    {
		r_wt_check_insert( &gRA, current + nx - 1 );
		r_wt_check_insert( &gRA, current + nx + 1 );
		r_wt_check_insert( &gRA, current - nx - 1 );
		r_wt_check_insert( &gRA, current - nx + 1 );
		r_wt_check_insert( &gRA, current + nxy - 1 );
		r_wt_check_insert( &gRA, current + nxy + 1 );
		r_wt_check_insert( &gRA, current - nxy - 1 );
		r_wt_check_insert( &gRA, current - nxy + 1 );
		r_wt_check_insert( &gRA, current + nxy - nx );
		r_wt_check_insert( &gRA, current + nxy + nx );
		r_wt_check_insert( &gRA, current - nxy - nx );
		r_wt_check_insert( &gRA, current - nxy + nx );

		/* 8 'corner sharing' points */
		if ( gRA.wt_diag_connect == 0 )
		{
		    r_wt_check_insert( &gRA, current - nxy - nx - 1 );
		    r_wt_check_insert( &gRA, current - nxy - nx + 1 );
		    r_wt_check_insert( &gRA, current - nxy + nx - 1 );
		    r_wt_check_insert( &gRA, current - nxy + nx + 1 );
		    r_wt_check_insert( &gRA, current + nxy - nx - 1 );
		    r_wt_check_insert( &gRA, current + nxy - nx + 1 );
		    r_wt_check_insert( &gRA, current + nxy + nx - 1 );
		    r_wt_check_insert( &gRA, current + nxy + nx + 1 );
		}
	    }
	}
    }

    /* clear bound markings */
    for( count = 0, fnptr = gRA.fdata; count < gRA.nvox; count++, fnptr++ )
	if ( *fnptr == R_BOUND_VAL )
	    *fnptr = 0;

    /* recompute statistics, if the loaded value is big or small */

    THD_load_statistics( gRA.func ) ;

    /* now redisplay dataset, in case anyone is looking at it */

    PLUTO_dset_redisplay( gRA.func ) ;
    dset_changed = 1;

    fputs( "done\n\n", stderr );
    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Callback to fill the gray matter (or so we somewhat suppose).
**
**----------------------------------------------------------------------
 */
static void
r_gr_cb_fill(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    points_t B;
    short  * fnptr, * uptr;
    int    * iptr;
    int      count, dist, added, current;
    int      nx, nxy;

    ENTRY("r_gr_cb_fill");

    nx   = gRA.nx;
    nxy  = gRA.nxy;


    if ( ( gRA.point_coord == -1 ) || ( ! gRA.fdata ) )
    {
	fputc( 7, stderr );      /* hard-code a beep */
	EXRETURN;
    }

    if ( gRA.gr_max_dist <= 0 )
	EXRETURN;

    if ( !gRA.Bold.points || !gRA.Bnew.points ||
	   !gRA.neighbors || !gRA.undo_data)    {
	fprintf( stderr, "Error: rcfg10\n"
		 "Memory failure, addresses are %p, %p, %p and %p.\n",
		 gRA.Bold.points, gRA.Bnew.points,
		 gRA.neighbors, gRA.undo_data );
	EXRETURN;
    }

    /* give the user an idea of what is happening */
    rWARNING( "filling gray matter" );


    /* first store to undo_data */
    fnptr = gRA.fdata; uptr = gRA.undo_data;
    for ( count = 0; count < gRA.nvox; count++ )
	*uptr++ = *fnptr++;

    /* clear old memory */
    for ( count = 0, fnptr = gRA.fdata; count < gRA.nvox; count++, fnptr++ )
	if ( *fnptr == gRA.gr_fill_val )
	    *fnptr = 0;

    /* set old and new borders to nothing */
    gRA.Bold.used     = 0;
    gRA.Bnew.used     = 0;

    gRH.gr_edge.used = 0;               /* trash the old gray edge */

    iptr = gRA.border.points;
    for ( count = 0; count < gRA.border.used; count++ )
    {
	if ( ( added = r_gr_check_insert( &gRA, &gRA.Bold, *iptr ) ) == -1 )
	    EXRETURN;
	iptr++;
    }


    dist = 1;
    fputc( '.', stdout );
    while ( ( gRA.Bold.used > 0 ) && ( dist < gRA.gr_max_dist ) )
    {
	iptr = gRA.Bold.points;
	for ( count = 0; count < gRA.Bold.used; count++ )
	{
	    current = *iptr;

	    ( void )r_gr_check_insert( &gRA, &gRA.Bnew, current - 1 );
	    ( void )r_gr_check_insert( &gRA, &gRA.Bnew, current + 1 );
	    ( void )r_gr_check_insert( &gRA, &gRA.Bnew, current - nx );
	    ( void )r_gr_check_insert( &gRA, &gRA.Bnew, current + nx );
	    ( void )r_gr_check_insert( &gRA, &gRA.Bnew, current - nxy );
	    ( void )r_gr_check_insert( &gRA, &gRA.Bnew, current + nxy );

	    iptr++;
	}

	B             = gRA.Bold;
	gRA.Bold      = gRA.Bnew;
	gRA.Bnew      = B;
	gRA.Bnew.used = 0;

	dist++;
	fputc( '.', stdout );
    }

    /* clear bound markings */
    for( count = 0, fnptr = gRA.fdata; count < gRA.nvox; count++, fnptr++ )
	if ( *fnptr == R_BOUND_VAL )
	    *fnptr = 0;

    fputs( "done\n\n", stdout );

    /* recompute statistics, if the loaded value is big or small */

    THD_load_statistics( gRA.func ) ;

    /* now redisplay dataset, in case anyone is looking at it */

    PLUTO_dset_redisplay( gRA.func ) ;
    dset_changed = 1;
    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Make the check for adding a point to the border.
**
**----------------------------------------------------------------------
 */
static int
r_gr_check_insert( r_alg_s * A, points_t * B, int current )
{
    short    * aptr = A->adata;
    short    * fptr = A->fdata;
    short    * nptr = A->neighbors;
    short    * fvalp;
    int        value, added = 0;

    ENTRY("r_gr_check_insert");

    if ( nptr[ current ] == -1 )        /* do not accept edge points */
	RETURN(0);

    fvalp = fptr + current;
    value = aptr[ current ];

    /* if new and in range, we will consider setting it as a result */
    if ( *fvalp == 0 )
    {
	if ( ( value >= A->gr_range_min ) &&
	     ( value <= A->gr_range_max ) )
	{
	    if ( ! r_add_to_boundary( B, current ) )
		RETURN(-1);

	    *fvalp = A->gr_fill_val;

	    added = 1;
	}
	else    /* mark as in boundary, add to HOLE's search set */
	{
	    *fvalp = R_BOUND_VAL;
	    if ( ! r_add_to_boundary( &gRH.gr_edge, current ) )
		RETURN(-1);
	}
    }

    RETURN(added);
}


/*----------------------------------------------------------------------
**
**  Add a point to the boundary.
**
**----------------------------------------------------------------------
 */
static int
r_add_to_boundary( points_t * B, int index )
{
    ENTRY("r_add_to_boundary");

    if ( !B )
    {
	fprintf( stderr, "Error: atb10\n"
		 "Unexpected error - missing memory for bound structure!\n" );
	RETURN(0);
    }
    else if ( ! B->points )             /* we need initial memory */
    {
	B->used = 0;
	B->M    = 50;
	B->points = (int *)malloc( B->M * sizeof( int ) );

	if ( B->points == NULL )
	{
	    fprintf( stderr, "Error: atb15\n"
		     "Failed to allocate %d ints for boundary.\n", B->M );
	    RETURN(0);
	}
    }
    else if ( B->used == B->M )         /* then we need more memory */
    {
	B->M     *= 2;
	B->points = (int *)realloc( B->points, B->M * sizeof( int ) );

	if ( B->points == NULL )
	{
	    fprintf( stderr, "Error: atb20\n"
		     "Failed to reallocate %d ints for boundary.\n", B->M );
	    RETURN(0);
	}
    }

    B->points[B->used] = index;
    B->used++;

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Fill the neighbors-in-range array.
**
**  init values to zero
**  for each inner value
**      count the number of neighbors (max of 6 - no diagonals) in range
**      ( skip all edges )
**
**----------------------------------------------------------------------
 */
static void
r_wt_set_neighbors( r_alg_s * A )
{
    short * aptr = A->adata;
    short * nptr = A->neighbors;
    int     cx, cy, cz;
    int     nxy = A->nxy, nx = A->nx;

    ENTRY("r_wt_set_neighbors");


    for ( cz = 0; cz < A->nz; cz++ )
	for ( cy = 0; cy < A->ny; cy++ )
	    for ( cx = 0; cx < A->nx; cx++ )
	    {
		*nptr = 0;

		if ( ( cx == 0 ) || ( cx == A->nx - 1 ) ||
		     ( cy == 0 ) || ( cy == A->ny - 1 ) ||
		     ( cz == 0 ) || ( cz == A->nz - 1 )
		   )
		{
		    aptr++;
		    *nptr++ = -1;    /* no future consideration */
		    continue;
		}
		else if ( *aptr > A->wt_range_max || *aptr < A->wt_range_min )
		{
		    aptr++;
		    nptr++;
		    continue;
		}

		if ( ( *(aptr-1) <= A->wt_range_max ) &&
		     ( *(aptr-1) >= A->wt_range_min ) )
		    (*nptr)++;

		if ( ( *(aptr+1) <= A->wt_range_max ) && 
		     ( *(aptr+1) >= A->wt_range_min ) )
		    (*nptr)++;

		if ( ( *(aptr-nx) <= A->wt_range_max ) && 
		     ( *(aptr-nx) >= A->wt_range_min ) )
		    (*nptr)++;

		if ( ( *(aptr+nx) <= A->wt_range_max ) &&
		     ( *(aptr+nx) >= A->wt_range_min ) )
		    (*nptr)++;

		if ( ( *(aptr-nxy) <= A->wt_range_max ) && 
		     ( *(aptr-nxy) >= A->wt_range_min) )
		    (*nptr)++;

		if ( ( *(aptr+nxy) <= A->wt_range_max ) &&
		     ( *(aptr+nxy) >= A->wt_range_min) )
		    (*nptr)++;

		aptr++;
		nptr++;
	    }

    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Callback to set the minimum neighbors in search (1 to 6).
**
**----------------------------------------------------------------------
 */
static void
r_wt_cb_set_diag_conn(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    char * text;
    int    ival;

    ENTRY("r_wt_cb_set_diag_conn");

    text = XmTextGetString( w );

    if ( ! text || ! *text )    /* nothing typed */
    {
	if ( text )
	    XtFree( text );
	EXRETURN;
    }

    /*
    **  Make sure a value has changed (to something acceptable)
    **  before applying.
    */

    ival = atoi( text );
    if ( ( ival < 0 ) || ( ival > 2 ) )
    {
	sprintf( gRmessage, "Value %d is not in range [%d,%d].", ival, 0, 2 );
	rERROR( gRmessage );
	EXRETURN;
    }

    if ( gRA.wt_diag_connect != ival )
	gRA.wt_diag_connect = ival;

    XtFree( text );
    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Callback to set the minimum neighbors in search (1 to 6).
**
**----------------------------------------------------------------------
 */
static void
r_wt_cb_set_min_nbrs(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    char * text;
    int    ival;

    text = XmTextGetString( w );

    if ( ! text || ! *text )    /* nothing typed */
    {
	if ( text )
	    XtFree( text );
	return;
    }

    /*
    **  Make sure a value has changed (to something acceptable)
    **  before applying.
    */

    ival = atoi( text );
    if ( ( ival < 0 ) || ( ival > 6 ) )
    {
	sprintf( gRmessage, "Value %d is not in range [%d,%d].", ival, 0, 6 );
	rERROR( gRmessage );
	return;
    }

    if ( gRA.min_nbrs != ival )
	gRA.min_nbrs = ival;


    XtFree( text );
}


/*----------------------------------------------------------------------
**
**  Callback to set the maximum distance in gray search.
**
**----------------------------------------------------------------------
 */
static void
r_gr_cb_set_max_dist(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    char * text;
    int    ival;

    text = XmTextGetString( w );

    if ( ! text || ! *text )    /* nothing typed */
    {
	if ( text )
	    XtFree( text );
	return;
    }

    /*
    **  Make sure a value has changed (to something acceptable)
    **  before applying.
    */

    ival = atoi( text );
    if ( ( ival < 1 ) || ( ival > 200 ) )
    {
	sprintf( gRmessage, "Value %d is not in range [%d,%d].", ival, 1, 200 );
	rERROR( gRmessage );
	return;
    }

    if ( gRA.gr_max_dist != ival )
	gRA.gr_max_dist = ival;


    XtFree( text );
}


/*----------------------------------------------------------------------
**
**  Callback to set the maximum fill size for the holes app.
**
**----------------------------------------------------------------------
 */
static void
r_HL_cb_set_maxsize(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    char * text;
    int    ival;

    text = XmTextGetString( w );

    if ( ! text || ! *text )    /* nothing typed */
    {
	if ( text )
	    XtFree( text );
	return;
    }

    /*
    **  Make sure a value has changed (to something acceptable)
    **  before applying.
    */

    ival = atoi( text );
    if ( ival < 1 )
    {
	sprintf( gRmessage, "Value %d is not in range [%d,oo).", ival, 1 );
	rERROR( gRmessage );
	return;
    }

    if ( gRH.max_size != ival )
	gRH.max_size = ival;

    XtFree( text );
}


/*----------------------------------------------------------------------
**
**  Callback to set the fill value for the holes app.
**
**----------------------------------------------------------------------
 */
static void
r_HL_cb_set_fill_val(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    char * text;
    int    ival;

    text = XmTextGetString( w );

    if ( ! text || ! *text )    /* nothing typed */
    {
	if ( text )
	    XtFree( text );
	return;
    }

    /*
    **  Make sure a value has changed (to something acceptable)
    **  before applying.
    */

    ival = atoi( text );
    if ( ( ival < 0 ) || ( ival > 255 ) )
    {
	sprintf( gRmessage, "Value %d is not in range [%d,%d].", ival, 0, 255 );
	rERROR( gRmessage );
	return;
    }
    else if ( ival == 0 )
    {
	rWARNING( "Using hole fill value of 0." );
    }


    if ( gRH.fill_val != ival )
	gRH.fill_val = ival;


    XtFree( text );
}


/*----------------------------------------------------------------------
**
**  Callback to set the fill value.
**
**----------------------------------------------------------------------
 */
static void
r_INT_cb_set_fill_val(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    char * text;
    int    ival;

    text = XmTextGetString( w );

    if ( ! text || ! *text )    /* nothing typed */
    {
	if ( text )
	    XtFree( text );
	return;
    }

    /*
    **  Make sure a value has changed (to something acceptable)
    **  before applying.
    */

    ival = atoi( text );
    if ( ( ival < 0 ) || ( ival > 255 ) )
    {
	sprintf( gRmessage, "Value %d is not in range [%d,%d].", ival, 0, 255 );
	rERROR( gRmessage );
	return;
    }
    else if ( ival == 0 )
    {
	rWARNING( "Using interpolation fill value of 0." );
    }

    if ( gRI.fill_val != ival )
	gRI.fill_val = ival;


    XtFree( text );
}


/*----------------------------------------------------------------------
**
**  Callback to set the fill value.
**
**----------------------------------------------------------------------
 */
static void
r_wt_cb_set_fill_val(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    char * text;
    int    ival;

    text = XmTextGetString( w );

    if ( ! text || ! *text )    /* nothing typed */
    {
	if ( text )
	    XtFree( text );
	return;
    }

    /*
    **  Make sure a value has changed (to something acceptable)
    **  before applying.
    */

    ival = atoi( text );
    if ( ( ival < 0 ) || ( ival > 255 ) )
    {
	sprintf( gRmessage, "Value %d is not in range [%d,%d].", ival, 0, 255 );
	rERROR( gRmessage );
	return;
    }
    else if ( ival == 0 )
    {
	rWARNING( "Warning : using white matter fill value of 0." );
    }

    if ( gRA.wt_fill_val != ival )
	gRA.wt_fill_val = ival;


    XtFree( text );
}


/*----------------------------------------------------------------------
**
**  Callback to set the fill value.
**
**----------------------------------------------------------------------
 */
static void
r_gr_set_fill_val(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    char * text;
    int    ival;

    text = XmTextGetString( w );

    if ( ! text || ! *text )    /* nothing typed */
    {
	if ( text )
	    XtFree( text );
	return;
    }

    /*
    **  Make sure a value has changed (to something acceptable)
    **  before applying.
    */

    ival = atoi( text );
    if ( ( ival < 1 ) || ( ival > 255 ) )
    {
	sprintf( gRmessage, "Value %d is not in range [%d,%d].", ival, 1, 255 );
	rERROR( gRmessage );
	return;
    }

    if ( gRA.gr_fill_val != ival )
	gRA.gr_fill_val = ival;


    XtFree( text );
}


/*----------------------------------------------------------------------
**
**  Callback to set the white search range.
**
**----------------------------------------------------------------------
 */
static void
r_wt_cb_set_range(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    char * string = ( char * )client_data;
    char * text;
    int    ival;

    text = XmTextGetString( w );

    if ( ! text || ! *text )    /* nothing typed */
    {
	if ( text )
	    XtFree( text );
	return;
    }

    if ( ! string )  /* strange error - complain and return */
    {
	fprintf( stderr, "r_wt_cb_set_range error - string is NULL\n" );
	return;
    }
    else if ( ! *string )
    {
	fprintf( stderr, "r_wt_cb_set_range error - string is empty\n" );
	return;
    }
    else if ( ! strcmp( string, "to" ) && ! strcmp( string, "from" ) )
    {
	fprintf( stderr, "r_wt_cb_set_range error -\n"
			 "'%s' should be 'to' or 'from'.\n", string );
	return;
    }

    /*
    **  Make sure a value has changed (to something acceptable)
    **  before applying.  Use short range.
    */

    ival = atoi( text );
    if ( ( ival < -32768 ) || ( ival > 32767 ) )
    {
	fprintf( stderr, "Value %d is not in range [%d,%d].\n",
			 ival, -32768, 32767 );
	return;
    }

    if ( ! strcmp( string, "from" ) )
    {
	if ( gRA.wt_range_min != ival )
	{
	    gRA.wt_range_min = ival;

	    if ( gRA.wt_range_min > gRA.wt_range_max )
	    {
		sprintf( gRmessage, "\nWarning!"
		    "  Min value should be less than max value.\n"
		    "Value are %d and %d, respectively.\n",
			 gRA.wt_range_min, gRA.wt_range_max );
		rERROR( gRmessage );
	    }
	}
    }
    else
    {
	if ( gRA.wt_range_max != ival )
	{
	    gRA.wt_range_max = ival;

	    if ( gRA.wt_range_min > gRA.wt_range_max )
	    {
		sprintf( gRmessage, "\nWarning!"
			 "  Min value should be less than max value.\n"
			 "Value are %d and %d, respectively.\n",
			 gRA.wt_range_min, gRA.wt_range_max );
		rERROR( gRmessage );
	    }
	}
    }

    XtFree( text );
}


/*----------------------------------------------------------------------
**
**  Callback to set the search range.
**
**----------------------------------------------------------------------
 */
static void
r_gr_cb_set_range(
	Widget w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    char * string = ( char * )client_data;
    char * text;
    int    ival;

    text = XmTextGetString( w );

    if ( ! text || ! *text )    /* nothing typed */
    {
	if ( text )
	    XtFree( text );
	return;
    }

    if ( ! string )  /* strange error - complain and return */
    {
	fprintf( stderr, "r_gr_cb_set_range error - string is NULL\n" );
	return;
    }
    else if ( ! *string )
    {
	fprintf( stderr, "r_gr_cb_set_range error - string is empty\n" );
	return;
    }
    else if ( ! strcmp( string, "to" ) && ! strcmp( string, "from" ) )
    {
	fprintf( stderr, "r_gr_cb_set_range error -\n"
			 "'%s' should be 'to' or 'from'.\n", string );
	return;
    }

    /*
    **  Make sure a value has changed (to something acceptable)
    **  before applying.  Use short range.
    */

    ival = atoi( text );
    if ( ( ival < -32768 ) || ( ival > 32767 ) )
    {
	sprintf( gRmessage, "Value %d is not in range [%d,%d].",
			 ival, -32768, 32767 );
	rERROR( gRmessage );
	return;
    }

    if ( ! strcmp( string, "from" ) )
    {
	if ( gRA.gr_range_min != ival )
	{
	    gRA.gr_range_min = ival;

	    if ( gRA.gr_range_min > gRA.gr_range_max )
	    {
		sprintf( gRmessage, "\nWarning!"
		    "  Min value should be less than max value.\n"
		    "Value are %d and %d, respectively.\n",
			 gRA.gr_range_min, gRA.gr_range_max );
		rERROR( gRmessage );
	    }
	}
    }
    else
    {
	if ( gRA.gr_range_max != ival )
	{
	    gRA.gr_range_max = ival;

	    if ( gRA.gr_range_min > gRA.gr_range_max )
	    {
		sprintf( gRmessage, "\nWarning!"
			 "  Min value should be less than max value.\n"
			 "Value are %d and %d, respectively.\n",
			 gRA.gr_range_min, gRA.gr_range_max );
		rERROR( gRmessage );
	    }
	}
    }

    XtFree( text );
}


/*----------------------------------------------------------------------
**
**  The user has selected an initial fill point.
**
**  Update the search range.  If the user wants the initial point
**  aligned (moved to local extrema), do it.
**
**----------------------------------------------------------------------
 */
static int
r_afni_set_fill_point(
	int       * coord,      /* offset of initial point (modifiable) */
	r_alg_s * A             /* algorithm structure                  */
	)
{
    ENTRY("r_afni_set_fill_point");

    A->point_coord = *coord;

    /*printf("A = %p,  A->adata = %p,  A->point_coord = %d\n",
	    A,A->adata,A->point_coord);
    fflush(stdout);*/
    
    if ( A->adata == NULL )
    {
	/*fprintf(stderr,"Loading data...\n");
	DSET_load(A->anat);*/
	fputs("r_afni_set_fill_point(): Error: No anatomical data. ( A->adata = NULL )\n",stderr);
	fputs("This may have been caused by selecting Talairach\n",stderr);
	fputs("view before Switch Underlay or Switch Overlay.\n",stderr);
	fputs("Try setting the environment variable AFNI_VIEW_ANAT_BRICK\n",stderr);
	fputs("(The value is irrelevant) and run afni again.\n",stderr);
	RETURN(-1);
    }
    A->point_value = A->adata[A->point_coord];   /* no anat factor? */


    printf( "coord = %d, adata = %d, fdata = %d, ndata = %d\n",
	     *coord, A->adata[*coord], A->fdata[*coord], A->neighbors[*coord] );

    RETURN(0);
}


/*----------------------------------------------------------------------
**
**  Create a scale bar.
**
**----------------------------------------------------------------------
 */

static Widget
r_mk_scale_bar(
	Widget          parent,
	char          * title,
	int             min,
	int             max,
	int             value,
	int             decimal_places,
	XtCallbackProc  callback
	)
{
    int         ac;
    Arg         al[ 20 ];
    XmString    Xtitle;
    Widget      scale;

    Xtitle = XmStringCreateLtoR( title, gRX.charset );

    ac = 0;
    XtSetArg( al[ac], XmNtitleString,      Xtitle );         ac++;
    XtSetArg( al[ac], XmNorientation,      XmHORIZONTAL );   ac++;
    XtSetArg( al[ac], XmNminimum,          min    );         ac++;
    XtSetArg( al[ac], XmNmaximum,          max    );         ac++;
    XtSetArg( al[ac], XmNvalue,            value  );         ac++;
    XtSetArg( al[ac], XmNshowValue,        True   );         ac++;

    if ( decimal_places > 0 )
    {
	XtSetArg( al[ac], XmNdecimalPoints, decimal_places );
	ac++;
    }


    scale = XmCreateScale( parent, "scale_bar", al, ac );

    XmStringFree( Xtitle );

    XtManageChild( scale );

    XtAddCallback( scale, XmNvalueChangedCallback, callback, NULL );

    return scale;
}


/*----------------------------------------------------------------------
**
**  Display the contents of the algorithm structure.
**
**--------------------------------------------------------------------- 
 */

static void
r_main_show_alg_vals( r_alg_s * A )
{
    printf( "-----------------------------------\n" );

    printf(
	"gRA :\n"
	"\n"
	"point value             : %d\n"
	"point coord             : %d\n"
	"adjust point            : %d\n"
	"'save as' name          : %s\n"
	"\n"
	"white fill value        : %d\n"
	"white range min         : %d\n"
	"white range max         : %d\n"
	"white diag connect      : %d\n"
	"\n"
	"gray fill value         : %d\n"
	"gray range min          : %d\n"
	"gray range max          : %d\n"
	"gray max distance       : %d\n"
	"\n"
	"anat dset        (addr) : %p\n"
	"func dset        (addr) : %p\n"
	"adata data       (addr) : %p\n"
	"fdata data       (addr) : %p\n"
	"factor                  : %f\n"
	"nx                      : %d\n"
	"ny                      : %d\n"
	"nz                      : %d\n"
	"nvox                    : %d\n"
	"\n"
	"old bound.M             : %d\n"
	"old bound.used          : %d\n"
	"old bound.points (addr) : %p\n"
	"new bound.M             : %d\n"
	"new bound.used          : %d\n"
	"new bound.points (addr) : %p\n"
	"border.M                : %d\n"
	"border.used             : %d\n"
	"border.points    (addr) : %p\n"
	"\n"
	"neighbors        (addr) : %p\n"
	"undo data        (addr) : %p\n"
	"\n"
	"min neighbors           : %d\n"
	"strong borders          : %d\n",
	A->point_value, A->point_coord, A->adjust_point, A->save_as_name,
	A->wt_fill_val, A->wt_range_min, A->wt_range_max,
	    A->wt_diag_connect,
	A->gr_fill_val, A->gr_range_min, A->gr_range_max, A->gr_max_dist,
	A->anat, A->func,
	A->adata, A->fdata,
	A->factor, A->nx, A->ny, A->nz, A->nvox,
	A->Bold.M, A->Bold.used, A->Bold.points,
	A->Bnew.M, A->Bnew.used, A->Bnew.points,
	A->border.M, A->border.used, A->border.points,
	A->neighbors, A->undo_data,
	A->min_nbrs, A->strong_borders
	);

    printf( "-----------------------------------\n" );
}


/*----------------------------------------------------------------------
**
**  Display the contents of the interpolation structure.
**
**----------------------------------------------------------------------
 */
static void
r_main_show_INT_vals( interp_s * I )
{
    printf( "-----------------------------------\n" );

    printf(
	"gRI :\n"
	"\n"
	"fill value       : %d\n"
	"afni undo        : %d\n"
	"\n"
	"A.M              : %d\n"
	"A.used           : %d\n"
	"A.points  (addr) : %p\n"
	"B.M              : %d\n"
	"B.used           : %d\n"
	"B.points  (addr) : %p\n",
	I->fill_val, I->afni_undo,
	I->A.M, I->A.used, I->A.points,
	I->B.M, I->B.used, I->B.points
	);

    printf( "-----------------------------------\n" );
}


/*----------------------------------------------------------------------
**
**  Display the contents of the holes structure.
**
**----------------------------------------------------------------------
 */
static void
r_main_show_HL_vals( holes_s * H )
{
    printf( "-----------------------------------\n" );
    printf(
	"gRH :\n"
	"\n"
	"max size                : %d\n"
	"fill value              : %d\n"
	"wtgr_edge.M             : %d\n"
	"wtgr_edge.used          : %d\n"
	"wtgr_edge.points (addr) : %p\n"
	"filled.M                : %d\n"
	"filled.used             : %d\n"
	"filled.points (addr)    : %p\n",
	H->max_size, H->fill_val,
	H->wtgr_edge.M, H->wtgr_edge.used, H->wtgr_edge.points,
	H->filled.M, H->filled.used, H->filled.points
	);
    printf( "-----------------------------------\n" );
}


/*----------------------------------------------------------------------
**
**  Display the contents of the point connection structure.
**
**----------------------------------------------------------------------
 */
static void
r_main_show_pt_conn_vals( r_pt_conn_s * PC )
{
    printf( "-----------------------------------\n" );
    printf(
	"gRCP :\n"
	"\n"
	"plist.M             : %d\n"
	"plist.used          : %d\n"
	"plist.points (addr) : %p\n"
	"source point        : (%d,%d,%d)\n"
	"dest point          : (%d,%d,%d)\n"
	"which point         : %d\n",
	PC->plist.M, PC->plist.used, PC->plist.points,
	PC->source.x, PC->source.y, PC->source.z,
	PC->dest.x, PC->dest.y, PC->dest.z,
	PC->cur_pt
	);
    printf( "-----------------------------------\n" );
}


/*----------------------------------------------------------------------
**
**  Callback to get help.  This displays a general process overview.
**
**----------------------------------------------------------------------
 */
static void
r_main_cb_help(
	Widget    w,
	XtPointer client_data,
	XtPointer call_data
	)
{
    ( void )new_MCW_textwin( w,

#include "plug_roiedit.hhh"

	 "Compiled: "
	 __DATE__
	 ", "
	 __TIME__
	 "\n"

	, TEXT_READONLY );

    return;
}


/*----------------------------------------------------------------------
**
**  Callback to quit ( this merely unmanages all the widgets ).
**
**----------------------------------------------------------------------
 */
static void
r_main_cb_quit( void )
{
    r_any_cb_hide( NULL, "all", NULL );
    XtUnmapWidget( gRX.main );
    gRX.main_is_open = 0;
}


/*----------------------------------------------------------------------
**
**  Callback to save the current dataset under a new name.
**
**----------------------------------------------------------------------
 */
static void
r_main_cb_saveas(
	Widget w,
	int       client_data,
	XtPointer call_data
	)
{
    XmSelectionBoxCallbackStruct * cbs;
    char * text;
    int    ival;

    ENTRY("r_main_cb_saveas");

    cbs = ( XmSelectionBoxCallbackStruct * )call_data;

    if ( ! XmStringGetLtoR( cbs->value, gRX.charset, &text ) )
    {
	rERROR( "Failed to get filename from text." );
	EXRETURN;
    }
    else if ( ! *text )
    {
	XtFree( text );
	EXRETURN;
    }

    /* Make sure file name is not too long.  */

    if ( strlen( text ) > R_FILE_L - 14 )  /* leave from for +tlrc.BRIK.gz_ */
    {
	sprintf( gRmessage, "Output filename '%s' exceeds %d characters.\n",
			    text, R_FILE_L - 14 );
	rERROR( gRmessage );
    }
    else
	strcpy( gRA.save_as_name, text );

    XtFree( text );

    r_save_dataset_as( gRA.save_as_name, client_data );
    EXRETURN;
}


/*----------------------------------------------------------------------
**
**  Actually save the new dataset.  Make a copy of the original dset,
**  change the prefix and save the file.
**
**----------------------------------------------------------------------
 */
static int
r_save_dataset_as( char * filename, int overwrite )
{
    THD_3dim_dataset * dset = NULL;

    ENTRY("r_save_dataset_as");

    if ( ! ( dset = PLUTO_copy_dset( gRA.func, filename ) ) )
    {
	sprintf( gRmessage,"Failed to copy dataset with name '%s'.", filename );
	rERROR( gRmessage );
	RETURN(0);
    }

    if( ! overwrite && THD_is_file( dset->dblk->diskptr->header_name ) )
    {
	sprintf( gRmessage, "File '%s' already exists!", filename );
	rERROR( gRmessage );
    }
    else
    {
	DSET_overwrite( dset ) ;

	sprintf( gRmessage, "Dataset copied as '%s'.", filename );
	rWARNING( gRmessage );
    }

    THD_delete_3dim_dataset( dset, False );

    RETURN(1);
}


/*----------------------------------------------------------------------
**
**  Display the contents of internal structures.
**
**----------------------------------------------------------------------
 */
static void
r_main_cb_show_structs( void )
{
    ENTRY("r_main_cb_show_structs");

    printf( "------------------------------------------------------------\n" );
	
    r_main_show_alg_vals    ( &gRA );
    r_main_show_INT_vals    ( &gRI );
    r_main_show_HL_vals     ( &gRH );
    r_main_show_pt_conn_vals( &gRCP );

    printf( "------------------------------------------------------------\n" );
    EXRETURN;
}


/*----------------------------------------------------------------------
**------                                --------------------------------
**------  Begin interpolation routines  --------------------------------
**------                                --------------------------------
**----------------------------------------------------------------------
**
**  The technique here is to keep two curves of data stored.
**
**  Operations that we need to consider are :   
**
**      o  inserting a new line ( on any line draw )
**              if none     { insert first  }
**              else if one { insert second }
**              else        { move second to first, insert second }
**      o  UNDO move
**              if two      { remove second }
**              else        { remove first  }
**      o  interpolate  ( use special interpolation value )
**              - only with the two lines
**              - store previous interpolation first
**                  ( change interpolation values to draw values )
**      o  undo interpolation
**              remove all interpolation values
**
**----------------------------------------------------------------------
 */


/*-------------------------------------------------------------------
  Callback for done button
---------------------------------------------------------------------*/

static void DRAW_done_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
    ENTRY("DRAW_done_CB");

   if( dset != NULL ){
      if( recv_open ) AFNI_receive_control( im3d, recv_key, DRAWING_SHUTDOWN, NULL ) ;
      if( dset_changed ){
	 MCW_invert_widget( done_pb ) ;
	 DSET_overwrite(dset) ;
	 MCW_invert_widget( done_pb ) ;
      }
      DSET_unlock(dset) ; DSET_anyize(dset) ;
      dset = NULL ; dset_changed = 0 ;
   }

   if( undo_buf != NULL ){
      free(undo_buf) ; free(undo_xyz) ;
      undo_buf = NULL; undo_xyz = NULL;
      undo_bufsiz = undo_bufnum = undo_bufuse = 0 ;
   }

   XtUnmapWidget( shell ) ; editor_open = 0 ; recv_open = 0 ; recv_key = -1 ;
   EXRETURN;
}

/*-------------------------------------------------------------------
  Callback for undo button
---------------------------------------------------------------------*/

static void DRAW_undo_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   void * ub ; int * ux, * uy, * uz ;
   int ubs = undo_bufsiz , uis = sizeof(int)*undo_bufuse ;

   ENTRY("DRAW_undo_CB");

   if( undo_bufuse <= 0 ){ XBell( dc->display , 100 ) ; EXRETURN ; }

   /* since the undo_stuff will be modified by the
      drawing function, we must make temporary copies */

   ub =         malloc(ubs) ; memcpy(ub,undo_buf,ubs) ;
   ux = (int *) malloc(uis) ; memcpy(ux,undo_xyz,uis) ;

   gRI.afni_undo = 1;
   DRAW_into_dataset( undo_bufuse , ux,NULL,NULL , ub ) ;
   gRI.afni_undo = 0;

   if ( ( mode_ival == MODE_CURVE ) || ( mode_ival == MODE_CLOSED ) ||
	( mode_ival == MODE_CONN_PTS ) )
   {
	gRI.B.used = 0;         /* RCR - clear any new I-data */

	if ( mode_ival == MODE_CONN_PTS )
	    gRCP.cur_pt = 1;
   }

   free(ub) ; free(ux) ;
   EXRETURN ;
}

/*-------------------------------------------------------------------
  Callback for quit button
---------------------------------------------------------------------*/

static void DRAW_quit_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   ENTRY("DRAW_quit_CB");

   if( dset != NULL ){
      if( recv_open ) AFNI_receive_control( im3d, recv_key, DRAWING_SHUTDOWN, NULL ) ;
      DSET_unlock(dset) ;
      DSET_unload(dset) ; DSET_anyize(dset) ;
      if( dset_changed ){
	 MCW_invert_widget(quit_pb) ;
	 THD_load_statistics( dset ) ;
	 PLUTO_dset_redisplay( dset ) ;
	 MCW_invert_widget(quit_pb) ;
      }
      dset = NULL ; dset_changed = 0 ;
   }

   if( undo_buf != NULL ){
      free(undo_buf) ; free(undo_xyz) ;
      undo_buf = NULL; undo_xyz = NULL;
      undo_bufsiz = undo_bufnum = undo_bufuse = 0 ;
   }

   XtUnmapWidget( shell ) ; editor_open = 0 ; recv_open = 0 ; recv_key = -1 ;
   EXRETURN ;
}

/*-------------------------------------------------------------------
  Callback for save button
---------------------------------------------------------------------*/

static void DRAW_save_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   ENTRY("DRAW_save_CB");

   if( dset == NULL ){ XBell( dc->display , 100 ) ; EXRETURN ; }

   MCW_invert_widget(save_pb) ;

   DSET_overwrite(dset); dset_changed = 0; SENSITIZE(choose_pb,1);

   MCW_invert_widget(save_pb) ; SENSITIZE(save_pb,0) ;
   EXRETURN ;
}

/*-------------------------------------------------------------------
  Callback for help button
---------------------------------------------------------------------*/

static void DRAW_help_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   (void ) new_MCW_textwin( help_pb ,

  "The GyrusFinder plugin is based on the original 'Draw Dataset' plugin\n"
  "by RW Cox.  The original help for that plugin is below.  Help for the\n"
  "new features can be found in the new windows created by GyrusFinder.\n"
  "======================================================================\n"
  "This plugin can be used to edit interactively the voxel contents\n"
  "of a dataset.  Since such changes are irreversible, it is best that\n"
  "you either edit a copy of a dataset, or create a dataset of all zeros.\n"
  "These tasks can be done with the 'Dataset Copy' plugin.\n"
  "\n"
  "***************** Read the WARNINGS section below. *****************\n"
  "\n"
  "---------------------- Bob Cox, February 1998 ----------------------\n"
  "\n"
  "Step 1) Choose a dataset to edit.\n"
  "        * Only datasets that have data BRIKs stored at the current\n"
  "            resolution can be edited.\n"
  "        * Datasets may be copied with the 'Dataset Copy' plugin.\n"
  "        * It is probably best that the dataset being edited be\n"
  "            displayed.  Otherwise it will be impossible to gauge\n"
  "            the effect of the editing operations.\n"
  "        * At this time, only datasets that have a single sub-brick\n"
  "            can be edited.\n"
  "\n"
  "Step 2) Choose a drawing value.\n"
  "        * This is the number that will be placed into the dataset\n"
  "            voxels that are chosen.\n"
  "        * Integer valued datasets can only receive integer values;\n"
  "            float datasets can take floating point values.\n"
  "\n"
  "Step 3) Choose a drawing color.\n"
  "        * This is the color that will be shown in the image windows\n"
  "            while drawing is going on (that is, while mouse button 2\n"
  "            is pressed).\n"
  "        * See 5) for more details about the drawing process.\n"
  "\n"
  "Step 4) Choose a drawing mode.\n"
  "        * 'Open Curve' means to select dataset voxels that lie under\n"
  "            the pixel lines drawn on the image as you move the mouse\n"
  "            with button 2 held down.\n"
  "        * 'Closed Curve ' means to close the curve drawn from the last\n"
  "            point drawn (where button 2 is released) back to the\n"
  "            first point drawn (where button 2 was pressed).\n"
  "        * 'Points' means to take only the voxels corresponding\n"
  "            to the screen pixels about which X11 sends notice\n"
  "            (this is not very useful).\n"
  "        * 'Flood->Value' means to flood fill outwards from the first\n"
  "            chosen voxel, stopping when the Dataset Value is reached.\n"
  "            In conjunction with 'Closed Curve', it can be used to draw\n"
  "            an entire region in a plane.\n"
  "        * 'Flood->Nonzero' means to flood fill, but stopping when any\n"
  "            nonzero voxel value is reached.\n"
  "\n"
  "Step 5) Draw something in an image window.\n"
  "        * Drawing is done using mouse button 2.\n"
  "        * In an image window, drawing a set of pixels is done\n"
  "            by pressing and holding button 2, and dragging\n"
  "            the cursor across the desired pixels.  The drawing\n"
  "            color will be painted over these pixels while the\n"
  "            painting is going on (while button 2 is held down).\n"
  "        * After mouse button 2 is released, the drawing value for\n"
  "            the chosen voxels is copied into the dataset.  The\n"
  "            dataset is then redisplayed -- this will most likely\n"
  "            change the color of the selected voxels, since display\n"
  "            colors depend on the Define Function pbar (for Func\n"
  "            datasets) or on the greyscale map (for Anat datasets).\n"
  "        * That is, the drawing color is ONLY used while button2\n"
  "            is pressed down.  This color should simply be chosen\n"
  "            to provide good contrast for the drawing operations.\n"
  "        * Pressing and releasing button 2 in a graph window\n"
  "            sub-graph will cause that single voxel to get the\n"
  "            drawing value, as well.  You cannot select a group\n"
  "            of voxels in a graph window -- only one voxel per click.\n"
  "\n"
  "Step 6) Undo.\n"
  "        * The last drawing operation can be undone -- that is,\n"
  "            pressing 'Undo' will restore the voxel values before\n"
  "            the last button 2 press-release operation.\n"
  "        * There is only one level of undo.  Undo-ing the undo\n"
  "            will put things back the way they were.  Anyone who\n"
  "            implements a better undo system will be appreciated.\n"
  "\n"
  "Step 7) Save dataset (maybe).\n"
  "        * While a dataset is being edited, it is locked into memory.\n"
  "        * The edited values are saved to disk only when 'Save' or\n"
  "            'Done' are pressed.\n"
  "        * The 'Quit' button can be used to discard the edits of\n"
  "            a dataset.  In that case, the dataset values are\n"
  "            re-read from disk when it is redisplayed.\n"
  "        * Closing the AFNI Editor window using the window manager\n"
  "            is equivalent to pressing 'Quit'.\n"
  "\n"
  "WARNINGS:\n"
  "  * It is important to understand the distinction between 'pixels'\n"
  "      and 'voxels'.  Pixels are on the screen, and while you are\n"
  "      drawing, you are drawing pixels with the drawing color.  When\n"
  "      you release mouse button 2, those dataset voxels to which these\n"
  "      pixels correspond are computed.  The values stored in those\n"
  "      voxels are then altered, and the dataset display is refreshed.\n"
  "  * It is possible to draw on a montaged image window.  However,\n"
  "      only voxels from the first slice drawn into will be altered.\n"
  "  * Using button 2 in an image or graph window before choosing a\n"
  "      dataset to edit will cause the display to beep.\n"
  "  * Closing the AFNI controller window that this was started from\n"
  "      is the equivalent of pressing 'Quit'.\n"
  "  * Doing something that causes the AFNI controller window to\n"
  "      alter its 3D grid location or resolution is also the\n"
  "      equivalent of pressing 'Quit'.  This is because the 3D grid\n"
  "      for the dataset being edited will no longer correspond to\n"
  "      the 3D grid in the image and graph windows.  Such actions\n"
  "      include switching from 'View Brick' to 'Warp on Demand',\n"
  "      switching datasets or sessions, and switching views.\n"
  "  * You can only draw into the windows of the controller from which\n"
  "      the Editor was started.\n"
  "  * Only one copy of the Editor can be active at a time.  If you\n"
  "      use the plugin menu to call up the Editor when it is already\n"
  "      open, that will simply pop the window up to the top of the\n"
  "      stacking order.  If you want to restart the Editor in a\n"
  "      different AFNI controller, you must first close the Editor\n"
  "      (via 'Done' or 'Quit') and then start it from the other\n"
  "      controller's window.\n"
  "  * Peculiar and confusing things can happen using 'Warp-on-Demand'\n"
  "      with the Editor.  My advice is not to try this.\n"
  "  * Edit at your own risk!  Be careful out there.\n"
  "\n"
  "SUGGESTIONS?\n"
  "  * Please send them to " COXEMAIL "\n"
  "  * Even better than suggestions are implementations.\n"


    , TEXT_READONLY ) ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for choose button.
  Criteria for datasets that can be edited:
    - must be in current session
    - must have actual bricks
    - only datasets with nvals=1 can be edited
    - bricks must be on same grid (dataxes) as AFNI controller
  Much of this code is adapted from PLUG_choose_dataset_CB.
  [28 Jul 2003] Modified for new THD_session struct.
---------------------------------------------------------------------*/

static int                  ndsl = 0 ;
static PLUGIN_dataset_link * dsl = NULL ;

static void DRAW_choose_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   THD_session * ss  = im3d->ss_now ;           /* current session */
   int           vv  = im3d->vinfo->view_type ; /* view type */
   THD_3dim_dataset * qset ;
   int id , ltop , llen ;
   char qnam[THD_MAX_NAME] , label[THD_MAX_NAME] ;
   static char ** strlist = NULL ;

   ENTRY("DRAW_choose_CB");

   /* can't do this if a dataset is already active and changed */

   if( dset != NULL && dset_changed ){
      (void) MCW_popup_message( choose_pb ,
				   "Can't change datasets until\n"
				   "you save the changes you've\n"
				   "already made.  Or you could\n"
				   "'Quit' and re-start the Editor" ,
				MCW_USER_KILL | MCW_TIMER_KILL ) ;
      XBell( dc->display , 100 ) ;
      EXRETURN ;
   }

   /* RCR - If data is not on disk, do not accept it (must be a warp).
	Inform the user that they might have to write the anat.
   */
   if ( ! DSET_ONDISK( im3d->anat_now ) )
   {
      (void) MCW_popup_message( choose_pb ,
		   "Anat data is not actually on disk!\n"
		   "You may be in 'Warp-on-Demand' mode.\n"
		   "\n"
		   "Please take the following steps :\n"
		   "   1. 'Write Anat'\n"
		   "   2. 'Rescan This'\n"
		   "   3. Set to 'View Anat Data Brick'",
		MCW_USER_KILL | MCW_TIMER_KILL ) ;
      XBell( dc->display , 100 ) ;
      EXRETURN ;
   }

   /* initialize */

   ndsl = 0 ;

   /* scan anats */

   for( id=0 ; id < ss->num_dsset ; id++ ){
      qset = ss->dsset[id][vv] ;

      if( ! ISVALID_DSET (qset)                        ) continue ;  /* skip */
      if( ! DSET_INMEMORY(qset)                        ) continue ;
      if(   DSET_NVALS(qset) > 1                       ) continue ;
      if( ! EQUIV_DATAXES(qset->daxes,im3d->wod_daxes) ) continue ;

      ndsl++ ;
      dsl = (PLUGIN_dataset_link *)
	      XtRealloc( (char *) dsl , sizeof(PLUGIN_dataset_link)*ndsl ) ;

      make_PLUGIN_dataset_link( qset , dsl + (ndsl-1) ) ;
   }

   /* found nothing?  exit */

   if( ndsl < 1 ){
      (void) MCW_popup_message( choose_pb ,
				   "Didn't find any datasets to edit!\n"
				   "Check if:\n"
				   " - you are in 'Warp-on-Demand' mode\n"
				   " - you are in the correct session" ,
				MCW_USER_KILL | MCW_TIMER_KILL ) ;
      XBell( dc->display , 100 ) ;
      EXRETURN ;
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
		       DRAW_finalize_dset_CB , NULL     ) ;

   EXRETURN ;
}

static void DRAW_finalize_dset_CB( Widget w, XtPointer fd, MCW_choose_cbs * cbs )
{
   int id = cbs->ival ;
   THD_3dim_dataset * qset ;
   XmString xstr ;
   char str[256] ;

   ENTRY("DRAW_finalize_dset_CB");

   /* check for errors */

   if( ! editor_open ){ POPDOWN_strlist_chooser ; XBell(dc->display,100) ; EXRETURN ; }

   if( dset != NULL && dset_changed ){ XBell(dc->display,100) ; EXRETURN ; }

   if( id < 0 || id >= ndsl ){ XBell(dc->display,100) ; EXRETURN ; }

   qset = PLUTO_find_dset( &(dsl[id].idcode) ) ;  /* the new dataset? */

   if( qset == NULL ){ XBell(dc->display,100) ; EXRETURN ; }

   if( ! EQUIV_DATAXES( im3d->wod_daxes , qset->daxes ) ){
      XBell(dc->display,100) ; EXRETURN ;
   }

   /* accept this dataset */

   dset = qset ; dset_changed = 0 ; SENSITIZE(save_pb,0) ;
   dax_save = *(dset->daxes) ;

   /* write the informational label */

   if( DSET_BRICK_FACTOR(dset,0) == 0.0 ){
      strcpy(str,dsl[id].title) ;
   } else {
      char abuf[16] ;
      AV_fval_to_char( DSET_BRICK_FACTOR(dset,0) , abuf ) ;
      sprintf(str,"%s\nbrick factor: %s", dsl[id].title , abuf ) ;
   }
   xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( info_lab , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   /* setup AFNI for drawing */

   if( ! recv_open ){
      recv_key = AFNI_receive_init( im3d, RECEIVE_DRAWING_MASK,
                                    DRAW_receiver,NULL,"DRAW_receiver" ) ;

      if( recv_key < 0 ){
	 (void) MCW_popup_message( im3d->vwid->top_shell ,
				     "Unable to establish\n"
				     "connection to AFNI\n"
				     "drawing routines!" ,
				   MCW_USER_KILL | MCW_TIMER_KILL ) ;

	 dset = NULL ; XBell(dc->display,100) ; EXRETURN ;
      }
   }

   DSET_mallocize(dset) ; DSET_lock(dset) ; DSET_load(dset) ;

   AFNI_receive_control( im3d, recv_key , mode_index , NULL ) ;
   AFNI_receive_control( im3d, recv_key , DRAWING_OVCINDEX, ITOP(color_index) ) ;
   recv_open = 1 ;

   undo_bufuse = 0 ; SENSITIZE(undo_pb,0) ;

   r_init_afni_vars( &gRA, dset );

   EXRETURN ;
}

/*-------------------------------------------------------------------
  Callback for color menu
---------------------------------------------------------------------*/

static void DRAW_color_CB( MCW_arrowval * av , XtPointer cd )
{
   color_index = av->ival ;

   if( dset != NULL && recv_open )
     AFNI_receive_control( im3d, recv_key, DRAWING_OVCINDEX, ITOP(color_index) ) ;

   return ;
}

/*-------------------------------------------------------------------
  Callback for mode menu
---------------------------------------------------------------------*/

static void DRAW_mode_CB( MCW_arrowval * av , XtPointer cd )
{
   mode_ival  = av->ival ;
   mode_index = mode_ints[mode_ival] ;

   if( dset != NULL && recv_open )
      AFNI_receive_control( im3d, recv_key, mode_index , NULL ) ;

   return ;
}

/*-------------------------------------------------------------------
  Callback for value menu
---------------------------------------------------------------------*/

static void DRAW_value_CB( MCW_arrowval * av , XtPointer cd )
{
   value_int   = av->ival ;
   value_float = av->fval ;
   return ;
}

/*******************************************************************
   Receive data from AFNI after drawing, etc.
********************************************************************/

static void DRAW_receiver( int why , int np , void * vp , void * cbd )
{
   ENTRY("DRAW_receiver");

   switch( why ){

      default:
	 fprintf(stderr,"DRAW_receiver: illegal why=%d\n",why) ;
      EXRETURN ;

      /*-- we like this one --*/

      case RECEIVE_POINTS:{
	 int **ip = (int **)vp ;
	 int *xd=ip[0] , *yd=ip[1] , *zd=ip[2] ; /* pts coords */
	 int mode=ip[3][0] ;                     /* how pts are organized */
	 int plane ;

	 if( np <= 0 ) EXRETURN ;  /* some error? */

	 plane = mode - SINGLE_MODE ;
	 if( plane < 1 || plane > 3 ) plane = mode - PLANAR_MODE ;
	 if( plane < 1 || plane > 3 ) plane = 0 ;

	 /* handle selection of initial point for vol_fill, otherwise, */
	 /* anything but flood mode --> just draw given points         */

	 if ( mode_ival == MODE_VOL_FILL )
	 {
	     int coord;

	     if ( yd == NULL )
		 coord = *xd;
	     else
		 coord = *xd +
			 *yd * DSET_NX(gRA.anat) +
			 *zd * DSET_NX(gRA.anat) * DSET_NY(gRA.anat);

	     if ( r_afni_set_fill_point( &coord, &gRA ) < 0 )
                EXRETURN ;

	     DRAW_into_dataset( 0, &coord, NULL, NULL, NULL );
	 }
	 else if ( mode_ival == MODE_CONN_PTS )
	 {   /* store the points into xd and DRAW_into_dataset */
	     r_ipt_t * pptr;
	     int       coord;

	     if ( gRCP.cur_pt == 1 )
		 pptr = &gRCP.source;
	     else if ( gRCP.cur_pt == 2 )
		 pptr = &gRCP.dest;
	     else
	     {
		 rERROR( "In DRAW_receiver() - gRCP.cur_pt is unset." );
		 EXRETURN;
	     }

	     if ( yd == NULL )
		 *pptr = r_index2pt( *xd, gRA.nx, gRA.ny, gRA.nz );
	     else
	     {
		 pptr->x = *xd;
		 pptr->y = *yd;
		 pptr->z = *zd;
	     }

	     /* If first point, just store it.   Otherwise draw entire line. */

	     if ( gRCP.cur_pt == 1 )
	     {
		 gRCP.cur_pt = 2;
		 DRAW_into_dataset( 0, xd, yd, zd, NULL );
		 EXRETURN;
	     }
	     else
	     {
		 points_t * Bp = &gRCP.plist;
		 r_ipt_t    p;
		 float      dx, dy, dz, fx, fy, fz, dcurr;
		 float      tot_dist = r_p_distance( gRCP.source, gRCP.dest );
		 int        pcount = 0;

		 Bp->used = 0;

		 dx = R_DIST_STEP * ( gRCP.dest.x - gRCP.source.x ) / tot_dist;
		 dy = R_DIST_STEP * ( gRCP.dest.y - gRCP.source.y ) / tot_dist;
		 dz = R_DIST_STEP * ( gRCP.dest.z - gRCP.source.z ) / tot_dist;
		 fx = gRCP.source.x;
		 fy = gRCP.source.y;
		 fz = gRCP.source.z;

		 for ( dcurr = 0; dcurr < tot_dist; dcurr += R_DIST_STEP )
		 {
		     coord = (int)fx + gRA.nx*((int)fy + gRA.ny*(int)fz);
		     r_add_to_boundary( Bp, coord );

		     fx += dx;
		     fy += dy;
		     fz += dz;
		 }

		 coord = gRCP.dest.x + gRA.nx*(gRCP.dest.y+gRA.ny*gRCP.dest.z);
		 r_add_to_boundary( Bp, coord );        /* add last point */

		 DRAW_into_dataset( Bp->used, Bp->points, NULL, NULL, NULL );

		 gRCP.cur_pt = 1;

		 /*
		 ** store the line as an interpolator line :
		 ** DRAW_into_dataset() was edited to check for MODE_CONN_PTS
		 ** when it stores the incoming line (point set).
		 */
	     }
	 }
	 else if( plane == 0 ||
	     ((mode_ival != MODE_FLOOD_VAL) && (mode_ival != MODE_FLOOD_NZ)) ){

	    DRAW_into_dataset( np , xd,yd,zd , NULL ) ;

	 } else {

	    /* flood mode! */

	    int   ityp = DSET_BRICK_TYPE(dset,0) ;
	    float bfac = DSET_BRICK_FACTOR(dset,0) ;
	    int nx=DSET_NX(dset) , ny=DSET_NY(dset) , nz=DSET_NZ(dset) ,
		nxy = nx*ny , nxyz = nxy*nz , ii,jj , ixyz ;
	    int base , di,dj , itop,jtop,nij , xx=xd[0],yy=yd[0],zz=zd[0] ,
		ix,jy ;
	    byte * pl ;
	    int nfill , * xyzf , nf ;

	    /* compute stuff for which plane we are in:
		1 -> yz , 2 -> xz , 3 -> xy            */

	    switch(plane){
	       case 1: base=xx    ; di=nx; dj=nxy; itop=ny; jtop=nz;
			ix=yy; jy=zz; break;
	       case 2: base=yy*nx ; di=1 ; dj=nxy; itop=nx; jtop=nz;
			ix=xx; jy=zz; break;
	       case 3: base=zz*nxy; di=1 ; dj=nx ; itop=nx; jtop=ny;
			ix=xx; jy=yy; break;
	    }

	    /* create a 2D array with 0 where dataset != blocking value
			     and with 1 where dataset == blocking value */

	    nij = itop*jtop ;
	    pl  = (byte *) malloc( sizeof(byte) * nij ) ;
	    memset( pl , 0 , sizeof(byte) * nij ) ;

	    if( bfac == 0.0 ) bfac = 1.0 ;
	    switch(ityp){

	       case MRI_short:{
		  short * bp  = (short *) DSET_BRICK_ARRAY(dset,0) ;
		  short   val = (short)   (value_float/bfac) ;

		  if( mode_ival == MODE_FLOOD_VAL ){
		     for( jj=0 ; jj < jtop ; jj++ )
			for( ii=0 ; ii < itop ; ii++ ){
			   ixyz = base + ii*di + jj*dj ;
			   if( bp[ixyz] == val ) pl[ii+jj*itop] = 1 ;
			}
		  } else {
		     for( jj=0 ; jj < jtop ; jj++ )
			for( ii=0 ; ii < itop ; ii++ ){
			   ixyz = base + ii*di + jj*dj ;
			   if( bp[ixyz] != 0 ) pl[ii+jj*itop] = 1 ;
			}
		  }
	       }
	       break ;

	       case MRI_byte:{
		  byte * bp  = (byte *) DSET_BRICK_ARRAY(dset,0) ;
		  byte   val = (byte)   (value_float/bfac) ;

		  if( mode_ival == MODE_FLOOD_VAL ){
		     for( jj=0 ; jj < jtop ; jj++ )
			for( ii=0 ; ii < itop ; ii++ ){
			   ixyz = base + ii*di + jj*dj ;
			   if( bp[ixyz] == val ) pl[ii+jj*itop] = 1 ;
			}
		  } else {
		     for( jj=0 ; jj < jtop ; jj++ )
			for( ii=0 ; ii < itop ; ii++ ){
			   ixyz = base + ii*di + jj*dj ;
			   if( bp[ixyz] != 0 ) pl[ii+jj*itop] = 1 ;
			}
		  }
	       }
	       break ;

	       case MRI_float:{
		  float * bp  = (float *) DSET_BRICK_ARRAY(dset,0) ;
		  float   val = (value_float/bfac) ;

		  if( mode_ival == MODE_FLOOD_VAL ){
		     for( jj=0 ; jj < jtop ; jj++ )
			for( ii=0 ; ii < itop ; ii++ ){
			   ixyz = base + ii*di + jj*dj ;
			   if( bp[ixyz] == val ) pl[ii+jj*itop] = 1 ;
			}
		  } else {
		     for( jj=0 ; jj < jtop ; jj++ )
			for( ii=0 ; ii < itop ; ii++ ){
			   ixyz = base + ii*di + jj*dj ;
			   if( bp[ixyz] != 0.0 ) pl[ii+jj*itop] = 1 ;
			}
		  }
	       }
	       break ;

	       default:
		  free(pl) ;
		  fprintf(stderr,
			 "Flood not implemented for datasets of type %s\a\n",
			 MRI_TYPE_name[ityp] ) ;
	       EXRETURN ;

	    } /* end of switch on type */

	    /* start point must be a 0 (can't fill from an edge) */

	    if( pl[ix+jy*itop] == 1 ){
	       free(pl) ; XBell(dc->display,100) ; EXRETURN ;
	    }

	    /* call a routine to fill the array */

	    DRAW_2dfiller( itop,jtop , ix,jy , pl ) ;

	    /* all filled points are 2 --> these are the locations to draw */

	    nfill = 0 ;
	    for( ii=0 ; ii < nij ; ii++ ) nfill += (pl[ii] == 2) ;
	    if( nfill == 0 ){ free(pl) ; XBell(dc->display,100) ; EXRETURN ; }

	    xyzf = (int *) malloc( sizeof(int) * nfill ) ;

	    for( nf=0,jj=0 ; jj < jtop ; jj++ ){
	       for( ii=0 ; ii < itop ; ii++ ){
		  if( pl[ii+jj*itop] == 2 )
		     xyzf[nf++] = base + ii*di + jj*dj ;
	       }
	    }

	    free(pl) ;
	    DRAW_into_dataset( nfill , xyzf,NULL,NULL , NULL ) ;
	    free(xyzf) ;

	 } /* end of flooding */

      } /* end of dealing with drawn points */
      break ;

      /*-- user closed the controller window!? (the fiend) */

      case RECEIVE_CLOSURE:{
	 if( dset != NULL && dset_changed ) XBell(dc->display,100) ; /* protest */
	 DRAW_quit_CB(NULL,NULL,NULL) ;                              /* and die */
      }
      break ;

      /*-- user altered the controller window!? */

      case RECEIVE_ALTERATION:{

	 /* if we are already editing a dataset, then
	    check if the grid has changed -- if it has, must quit */

	 if( dset != NULL ){
	    if( ! EQUIV_DATAXES( im3d->wod_daxes , &dax_save ) ){
	       XBell( dc->display , 100 ) ;    /* feeble protest */
	       DRAW_quit_CB(NULL,NULL,NULL) ;  /* die */


		{       /* RCR stuff */

		    printf("Initializing gRA...\n");
		    gRA.anat  = NULL;
		    gRA.func  = NULL;
		    gRA.adata = NULL;
		    gRA.fdata = NULL;

		    r_main_cb_quit();
		}

	       /* less feeble protest */
	       (void) MCW_popup_message( im3d->vwid->top_shell ,
					   "Controller grid was altered!\n"
					   "Editor was forced to quit.\n"
					   "Any un-Saved changes were lost." ,
					 MCW_USER_KILL | MCW_TIMER_KILL ) ;
	    }
	 }
      }
      break ;

   } /* end of switch on why */

   EXRETURN ;
}

/*--------------------------------------------------------------------------
  Routine to draw into a dataset.
  If yd & zd are NULL, then xd is used as the direct 3D array index,
    otherwise xd,yd,zd are used as the 3-index.
  If var == NULL, then the value_av is used, otherwise the array var[]
    will be the source of the data.
----------------------------------------------------------------------------*/

static void DRAW_into_dataset( int np , int * xd , int * yd , int * zd , void * var )
{
   int   ityp = DSET_BRICK_TYPE(dset,0) ;
   float bfac = DSET_BRICK_FACTOR(dset,0) ;
   int nx=DSET_NX(dset) , ny=DSET_NY(dset) , nz=DSET_NZ(dset) ,
       nxy = nx*ny , nxyz = nxy*nz , ii , ixyz ;
   float vload = 0.0 ;
   int nbytes ;

   ENTRY("DRAW_into_dataset");

   /* sanity check */

   if( dset==NULL || np <= 0 || xd==NULL ) EXRETURN ;

   /* make space for undo */

   nbytes = np * mri_datum_size((MRI_TYPE)ityp) ; /* bytes needed for save */
   if( nbytes > undo_bufsiz ){
      if( undo_buf != NULL ) free(undo_buf) ;
      undo_buf    = malloc(nbytes) ;
      undo_bufsiz = nbytes ;
   }
   if( np > undo_bufnum ){
      if( undo_xyz != NULL ) free(undo_xyz);
      undo_xyz    = (int *) malloc(sizeof(int)*np) ;
      undo_bufnum = np ;
   }

   /* compute (or copy) data index into undo_xyz */

   if( yd == NULL ){                       /* direct supply of index */
      memcpy(undo_xyz,xd,sizeof(int)*np) ;
   } else {                                /* collapse 3-index into 1 */
      for( ii=0 ; ii < np ; ii++ )
	 undo_xyz[ii] = xd[ii] + yd[ii] * nx + zd[ii] * nxy ;
   }

   /* actually copy data, based on type */

   if( bfac == 0.0 ) bfac = 1.0 ;
   vload = value_float ;

   /* RCR - code for interpolation */
   if ( ! gRI.afni_undo && 
	( ( mode_ival == MODE_CURVE ) || ( mode_ival == MODE_CLOSED ) ||
	  ( mode_ival == MODE_CONN_PTS ) )
      )
   {
	/* insert the points into the interpolation structure */

	int        count, coord;
	int      * ipA, * ipB, * ipc;
	points_t * Bp, tmpB;
	
	if ( gRI.A.used == 0 )  /* if first IP set is empty, use it */
	    Bp = &gRI.A;
	else if ( gRI.B.used == 0 )     /* else, similarly for second       */
	    Bp = &gRI.B;
	else                            /* else, move 2->1 and use second   */
	{
	    tmpB       = gRI.A;         /* swap and empty new one */
	    gRI.A      = gRI.B;
	    gRI.B      = tmpB;
	    gRI.B.used = 0;

	    Bp = &gRI.B;
	}

	for ( count = 0; count < np; count++ )
	{
	    coord = undo_xyz[ count ];

	    if ( coord >= 0 && coord < nxyz )
		r_add_to_boundary( Bp, coord );
	}
   }

   switch( ityp ){

      default: fprintf(stderr,"Illegal brick type=%s in AFNI Editor!\n",
		       MRI_TYPE_name[ityp] ) ;
      break ;

      case MRI_short:{
	 short * bp  = (short *) DSET_BRICK_ARRAY(dset,0) ;
	 short * up  = (short *) undo_buf ;
	 short * vvv = (short *) var ;
	 short   val = (short)   (value_float/bfac) ;

	 for( ii=0 ; ii < np ; ii++ ){  /* save into undo buffer */
	    ixyz = undo_xyz[ii] ;
	    up[ii] = (ixyz >= 0 && ixyz < nxyz) ? bp[ixyz] : 0 ;
	 }

	 for( ii=0 ; ii < np ; ii++ ){  /* put into dataset */
	    ixyz = undo_xyz[ii] ;
	    if( ixyz >= 0 && ixyz < nxyz )
	       bp[ixyz] = (vvv==NULL) ? val : vvv[ii] ;
	 }
      }
      break ;

      case MRI_byte:{
	 byte * bp  = (byte *) DSET_BRICK_ARRAY(dset,0) ;
	 byte * up  = (byte *) undo_buf ;
	 byte * vvv = (byte *) var ;
	 byte   val = (byte)   (value_float/bfac) ;

	 for( ii=0 ; ii < np ; ii++ ){
	    ixyz = undo_xyz[ii] ;
	    up[ii] = (ixyz >= 0 && ixyz < nxyz) ? bp[ixyz] : 0 ;
	 }
	 for( ii=0 ; ii < np ; ii++ ){
	    ixyz = undo_xyz[ii] ;
	    if( ixyz >= 0 && ixyz < nxyz )
	       bp[ixyz] = (vvv==NULL) ? val : vvv[ii] ;
	 }
      }
      break ;

      case MRI_float:{
	 float * bp  = (float *) DSET_BRICK_ARRAY(dset,0) ;
	 float * up  = (float *) undo_buf ;
	 float * vvv = (float *) var ;
	 float   val = (value_float/bfac) ;

	 for( ii=0 ; ii < np ; ii++ ){
	    ixyz = undo_xyz[ii] ;
	    up[ii] = (ixyz >= 0 && ixyz < nxyz) ? bp[ixyz] : 0.0 ;
	 }
	 for( ii=0 ; ii < np ; ii++ ){
	    ixyz = undo_xyz[ii] ;
	    if( ixyz >= 0 && ixyz < nxyz )
	       bp[ixyz] = (vvv==NULL) ? val : vvv[ii] ;
	 }
      }
      break ;

      case MRI_complex:{
	 complex * bp  = (complex *) DSET_BRICK_ARRAY(dset,0) ;
	 complex * up  = (complex *) undo_buf ;
	 complex * vvv = (complex *) var ;
	 complex   val ;
	 static complex cxzero = { 0.0 , 0.0 } ;

	 val = CMPLX( (value_float/bfac) , 0.0 ) ;

	 for( ii=0 ; ii < np ; ii++ ){
	    ixyz = undo_xyz[ii] ;
	    up[ii] = (ixyz >= 0 && ixyz < nxyz) ? bp[ixyz] : cxzero ;
	 }
	 for( ii=0 ; ii < np ; ii++ ){
	    ixyz = undo_xyz[ii] ;
	    if( ixyz >= 0 && ixyz < nxyz )
	       bp[ixyz] = (vvv==NULL) ? val : vvv[ii] ;
	 }
      }
      break ;

   } /* end of switch on brick type */

   /* recompute statistics, if the loaded value is big or small */

   if( !ISVALID_STATISTIC(dset->stats)   ||
       vload > dset->stats->bstat[0].max ||
       vload < dset->stats->bstat[0].min   ) THD_load_statistics( dset ) ;

   /* now redisplay dataset, in case anyone is looking at it */

   PLUTO_dset_redisplay( dset ) ;

   undo_bufuse  = np ;
   dset_changed = 1 ;
   SENSITIZE(save_pb,1) ;
   SENSITIZE(choose_pb,0) ;
   SENSITIZE(undo_pb,1) ;

   EXRETURN ;
}

/*---------------------------------------------------------------------------
   Flood filling a byte array:
     nx = 1st dimension
     ny = 2nd dimension
     ix = start point
     jy = end point
     ar = array, with 0's everwhere except 1's as barriers to flooding

   All filled points (starting with ix,jy) will get the value 2.
-----------------------------------------------------------------------------*/

static void DRAW_2dfiller( int nx , int ny , int ix , int jy , byte * ar )
{
   int ii,jj , ip,jp , num ;

   ENTRY("DRAW_2dfiller");

#define AR(i,j) ar[(i)+(j)*nx]

   /* fill out in cross from 1st point */

   ip = ix ; jp = jy ; AR(ip,jp) = 2 ;

   for( ii=ip+1; ii < nx && AR(ii,jp) == 0; ii++ ) AR(ii,jp) = 2;
   for( ii=ip-1; ii >= 0 && AR(ii,jp) == 0; ii-- ) AR(ii,jp) = 2;
   for( jj=jp+1; jj < ny && AR(ip,jj) == 0; jj++ ) AR(ip,jj) = 2;
   for( jj=jp-1; jj >= 0 && AR(ip,jj) == 0; jj-- ) AR(ip,jj) = 2;

   /* brute force repetition of the cross technique */

   do {
      num = 0 ;
      for( jp=0 ; jp < ny ; jp++ ){
	 for( ip=0 ; ip < nx ; ip++ ){
	    if( AR(ip,jp) == 2 ){
	       for( ii=ip+1; ii < nx && AR(ii,jp) == 0; ii++ ){ AR(ii,jp) = 2; num++; }
	       for( ii=ip-1; ii >= 0 && AR(ii,jp) == 0; ii-- ){ AR(ii,jp) = 2; num++; }
	       for( jj=jp+1; jj < ny && AR(ip,jj) == 0; jj++ ){ AR(ip,jj) = 2; num++; }
	       for( jj=jp-1; jj >= 0 && AR(ip,jj) == 0; jj-- ){ AR(ip,jj) = 2; num++; }
	    }
	 }
      }
   } while( num > 0 ) ;

   EXRETURN ;
}

