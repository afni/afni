/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "vecmat.h"

#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to nudge a dataset around.
  Makes a custom interface.
  -- RWCox -- April 2000
************************************************************************/

/*----------------- prototypes for internal routines -----------------*/

static PLUGIN_interface * plint = NULL ;

char * NUD_main( PLUGIN_interface * ) ;  /* the entry point */

static void NUD_make_widgets(void) ;

static void NUD_nudge_CB ( Widget , XtPointer , XtPointer ) ;
static void NUD_clear_CB ( Widget , XtPointer , XtPointer ) ;
static void NUD_undo_CB  ( Widget , XtPointer , XtPointer ) ;
static void NUD_redo_CB  ( Widget , XtPointer , XtPointer ) ;
static void NUD_help_CB  ( Widget , XtPointer , XtPointer ) ;
static void NUD_quit_CB  ( Widget , XtPointer , XtPointer ) ;
static void NUD_doall_CB ( Widget , XtPointer , XtPointer ) ;
static void NUD_choose_CB( Widget , XtPointer , XtPointer ) ;
static void NUD_print_CB ( Widget , XtPointer , XtPointer ) ;

static void NUD_finalize_dset_CB( Widget , XtPointer , MCW_choose_cbs * ) ;

static void NUD_brick_av_CB( MCW_arrowval * , XtPointer ) ; /* Sub-brick menu */

static void NUD_undopush(void) ;
static void NUD_setcumlab(void) ;

static void NUD_rotate( MRI_IMAGE * im ) ;
static void NUD_update_base( Widget ) ;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   if( ncall > 0 ) return NULL ;  /* only one interface */

   plint = PLUTO_new_interface( "Nudge Dataset" ,
                                "Move bricks around" ,
                                NULL ,
                                PLUGIN_CALL_IMMEDIATELY , NUD_main  ) ;

   PLUTO_add_hint( plint , "Move bricks around" ) ;

   PLUTO_set_sequence( plint , "A:olddset:nudger" ) ;

   return plint ;
}

/**************************************************************************
  return a label string for 3 floats with 3 suffix characters
--------------------------------------------------------------------------*/

#define EPS 0.005

static char * NUD_threestring( float a,float b,float c,char ca,char cb,char cc )
{
   static char label[64] ;
   if( fabs(a) < EPS ) a = 0.0 ;
   if( fabs(b) < EPS ) b = 0.0 ;
   if( fabs(c) < EPS ) c = 0.0 ;
   sprintf(label,"%6.2f%c %6.2f%c %6.2f%c ", a,ca,b,cb,c,cc ) ;
   return label ;
}

static char * NUD_3string( float a,float b,float c,char ca,char cb,char cc )
{
   static char label[64] ;
   if( fabs(a) < EPS ) a = 0.0 ;
   if( fabs(b) < EPS ) b = 0.0 ;
   if( fabs(c) < EPS ) c = 0.0 ;
   sprintf(label,"%.2f%c %.2f%c %.2f%c", a,ca,b,cb,c,cc ) ;
   return label ;
}

/***************************************************************************
  Will be called from AFNI when user selects from Plugins menu.
****************************************************************************/

/* Interface widgets */

static Widget shell=NULL , rowcol , info_lab , choose_pb ;
static Widget nudge_pb, clear_pb, undo_pb, redo_pb, help_pb, quit_pb, doall_pb, print_pb ;
static MCW_arrowval * roll_av , * pitch_av , * yaw_av ,
                    * dS_av   , * dL_av    , * dP_av  , * brick_av ;
static Widget angle_cum_lab , shift_cum_lab ;

static MCW_arrowval * interp_av , * clip_av ;

/* other data */

static int nudger_open = 0 ;

static MCW_DC * dc ;                   /* display context */
static Three_D_View * im3d ;           /* AFNI controller */
static THD_3dim_dataset * dset ;       /* The dataset!    */
static MCW_idcode         dset_idc ;
static int new_dset = 0 ;              /* Is it new?      */
static int dset_ival = 0 ;             /* Sub-brick index */
static char dset_title[THD_MAX_NAME] ; /* Title string    */

static char * NUD_dummy_av_label[2] = { "[Nothing At All]", "[Nothing At All]" };

static int iha=1,ax1=0,ax2=1,ax3=2,hax1=1,hax2=2,hax3=3,adx=1,ady=2,adz=3 ;

static int undo_nall , undo_nuse , undo_ntop ;
static THD_dmat33 * undo_rmat=NULL ;
static THD_dfvec3 * undo_svec=NULL ;

static THD_dmat33 rmat ; /* current rotation matrix = undo_rmat[undo_nuse-1] */
static THD_dfvec3 svec ; /* current shift vector    = undo_svec[undo_nuse-1] */

#define NRESAM 6
#define NYESNO 2
static char * REG_resam_strings[NRESAM] = {
             "Linear" , "Cubic" , "Quintic" , "Heptic" , "Fourier" , "Fourier_nopad" } ;

static char * REG_resam_options[NRESAM] = {
             "-linear" , "-cubic" , "-quintic" , "-heptic" , "-Fourier" , "-Fourier_nopad" } ;

static int REG_resam_ints[NRESAM] = {
             MRI_LINEAR , MRI_CUBIC , MRI_QUINTIC , MRI_HEPTIC , MRI_FOURIER , MRI_FOURIER_NOPAD } ;

static char * YESNO_strings[NYESNO] = { "No" , "Yes" } ;

MRI_IMAGE * imbase = NULL ;

/*-------------------------------------------------------------------------------*/

char * NUD_main( PLUGIN_interface * plint )
{
   XmString xstr ;

   /*-- sanity checks --*/

   if( ! IM3D_OPEN(plint->im3d) )
      return " \n AFNI Controller\nnot opened?! \n " ;

   if( nudger_open ){
      if( plint->im3d != im3d ){ /* different controller => close it */
         NUD_quit_CB(NULL,NULL,NULL) ;
      } else {                   /* same controller => just raise up */
         XtMapWidget(shell) ;
         XRaiseWindow( XtDisplay(shell) , XtWindow(shell) ) ;
         return NULL ;
      }
   }

   im3d = plint->im3d ;  /* save for local use */

   /*-- create widgets, first time through --*/

   if( shell == NULL ){
      dc = im3d->dc ;        /* save this too */
      NUD_make_widgets() ;
      PLUTO_set_topshell( plint , shell ) ;  /* 22 Sep 2000 */
      RWC_visibilize_widget( shell ) ;       /* 27 Sep 2000 */
   }

   /*-- set titlebar --*/

   { char ttl[PLUGIN_STRING_SIZE] ;
     sprintf(ttl , "AFNI Nudger %s" , AFNI_controller_label(im3d) ) ;
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

   dset = NULL ;           /* not editing anything */
   ZERO_IDCODE(dset_idc) ;
   dset_ival = 0 ; AV_assign_ival(brick_av,0) ;
   if( imbase != NULL ){ mri_free(imbase); imbase = NULL; }

   nudger_open = 1 ;      /* editor is now open for business */

   SENSITIZE(nudge_pb ,0) ;
   SENSITIZE(undo_pb  ,0) ;
   SENSITIZE(redo_pb  ,0) ;
   SENSITIZE(doall_pb ,0) ;

   SENSITIZE(choose_pb,1) ; AV_SENSITIZE(brick_av,0) ;

   /* initialize nudgerosity */

   NUD_clear_CB(NULL,NULL,NULL) ;

   LOAD_DIAG_DMAT(rmat,1.0,1.0,1.0) ;
   LOAD_DFVEC3(svec,0.0,0.0,0.0)    ;
   NUD_setcumlab() ;

   /* initialize undo stack */

   if( undo_rmat != NULL ){ free(undo_rmat); undo_rmat = NULL; }
   if( undo_svec != NULL ){ free(undo_svec); undo_svec = NULL; }
   undo_nuse = 0 ;
   undo_ntop = 0 ;
   undo_nall = 1 ;
   undo_rmat = (THD_dmat33 *) malloc(sizeof(THD_dmat33)) ;
   undo_svec = (THD_dfvec3 *) malloc(sizeof(THD_dfvec3)) ;
   NUD_undopush() ;  /* top of stack = current transformation */

   return NULL ;
}

/*------------------------------------------------------------------------
  Make the control popup for this thing
--------------------------------------------------------------------------*/

#define SEP_HOR(ww)  XtVaCreateManagedWidget(                     \
                       "AFNI" , xmSeparatorWidgetClass , (ww) ,   \
                          XmNseparatorType , XmSINGLE_LINE ,      \
                          XmNinitialResourcesPersistent , False , \
                       NULL )

#define SEP_VER(ww) XtVaCreateManagedWidget(                      \
                       "AFNI" , xmSeparatorWidgetClass , (ww) ,   \
                          XmNseparatorType , XmDOUBLE_LINE ,      \
                          XmNorientation   , XmVERTICAL ,         \
                          XmNinitialResourcesPersistent , False , \
                       NULL )

/*-- structures defining action buttons (at bottom of popup) --*/

#define NACT 8  /* number of action buttons */

static MCW_action_item NUD_actor[NACT] = {
 {"Nudge",NUD_nudge_CB,NULL,
  "Applies Angles and Shifts\nto chosen Brick of dataset","Apply Angles/Shifts",1} ,

 {"Clear",NUD_clear_CB,NULL,
  "Clears Angles and\nShifts entry fields","Clear Angles/Shifts",0} ,

 {"Undo",NUD_undo_CB,NULL,
  "Undoes previous nudge, if possible","Undo last nudge",0} ,

 {"Redo",NUD_redo_CB,NULL,
  "Redoes previously undone nudge","Redo last undone nudge",0} ,

 {"Help",NUD_help_CB,NULL,
  "Displays more help" , "Displays more help",0} ,

 {"Quit",NUD_quit_CB,NULL,
  "Discard nudges since last\n'Do All' and close down",
  "Discard nudges and close",0} ,

 {"Do All",NUD_doall_CB,NULL,
  "Apply Angles and Shifts to all sub-\nbricks and save dataset to disk" ,
  "Apply Angles/Shifts; write to disk" , 1 } ,

 {"Print",NUD_print_CB,NULL,
  "Print current Angles and Shifts as\na '3drotate' command, to stderr" ,
  "Print 3drotate command to screen" , 0 }
} ;

/*------------------------------------------------------------------------*/

static void NUD_make_widgets(void)
{
   XmString xstr ;
   Widget hrc ;

   /*** top level shell for window manager ***/

   shell =
      XtVaAppCreateShell(
           "AFNI" , "AFNI" , topLevelShellWidgetClass , dc->display ,

           XmNtitle             , "AFNI Nudger" , /* top of window */
           XmNiconName          , "Nudger"      , /* label on icon */
           XmNdeleteResponse    , XmDO_NOTHING  , /* deletion handled below */
           XmNallowShellResize  , True ,          /* let code resize shell? */
           XmNmappedWhenManaged , False ,         /* must map it manually */
           XmNinitialResourcesPersistent , False ,
      NULL ) ;

   DC_yokify( shell , dc ) ; /* 14 Sep 1998 */

#ifndef DONT_INSTALL_ICONS
   if( afni48_good )             /* set icon pixmap */
      XtVaSetValues( shell ,
                        XmNiconPixmap , afni48_pixmap ,
                     NULL ) ;
#endif

   if( MCW_isitmwm(shell) )      /* remove some MWM functions */
      XtVaSetValues( shell ,
                       XmNmwmFunctions ,
                       MWM_FUNC_MOVE | MWM_FUNC_CLOSE | MWM_FUNC_MINIMIZE ,
                     NULL ) ;

   XmAddWMProtocolCallback(      /* make "Close" window menu work */
           shell ,
           XmInternAtom( dc->display , "WM_DELETE_WINDOW" , False ) ,
           NUD_quit_CB , (XtPointer) plint ) ;

   /*** rowcolumn widget to hold all user interface stuff ***/

   rowcol = XtVaCreateWidget(
             "AFNI" , xmRowColumnWidgetClass , shell ,
                XmNpacking     , XmPACK_TIGHT ,
                XmNorientation , XmVERTICAL ,
                XmNtraversalOn , False ,
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
   MCW_register_help( info_lab , "Shows dataset being nudged" ) ;
   MCW_register_hint( info_lab , "Shows dataset being nudged" ) ;

   /***** top row of widgets to choose dataset and sub-brick *****/

   SEP_HOR(rowcol) ;

   hrc =  XtVaCreateWidget(
           "AFNI" , xmRowColumnWidgetClass , rowcol ,
              XmNorientation  , XmHORIZONTAL ,
              XmNpacking      , XmPACK_TIGHT ,
              XmNadjustLast   , False ,
              XmNadjustMargin , False ,
              XmNtraversalOn  , False ,
              XmNmarginWidth  , 0 ,
              XmNmarginHeight , 0 ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   /*** button to let user choose dataset to edit ***/

   xstr = XmStringCreateLtoR( "Choose Dataset" , XmFONTLIST_DEFAULT_TAG ) ;
   choose_pb = XtVaCreateManagedWidget(
                  "AFNI" , xmPushButtonWidgetClass , hrc ,
                     XmNlabelString , xstr ,
                     XmNtraversalOn , False ,
                     XmNinitialResourcesPersistent , False ,
                  NULL ) ;
   XmStringFree(xstr) ;
   XtAddCallback( choose_pb, XmNactivateCallback, NUD_choose_CB, NULL ) ;
   MCW_register_help( choose_pb ,
                      "Use this to popup a\n"
                      "'chooser' that lets\n"
                      "you select which\n"
                      "dataset to nudge."
                    ) ;
   MCW_register_hint( choose_pb , "Popup dataset chooser" ) ;

   /*** menu to let user choose sub-brick to deal with ***/

   SEP_VER(hrc) ;

   brick_av = new_MCW_arrowval(
                          hrc ,                   /* parent Widget */
                          "Brick" ,               /* label */
                          MCW_AV_optmenu ,        /* option menu style */
                          0 ,                     /* first option */
                          1 ,                     /* last option */
                          0 ,                     /* initial selection */
                          MCW_AV_readtext ,       /* ignored but needed */
                          0 ,                     /* decimal shift */
                          NUD_brick_av_CB ,       /* callback when changed */
                          NULL ,                  /* data for above */
                          MCW_av_substring_CB ,   /* text creation routine */
                          NUD_dummy_av_label      /* data for above */
                        ) ;
   MCW_reghelp_children( brick_av->wrowcol ,
                         "Choose the sub-brick\n"
                         "to nudge interactively;\n"
                         "you should also be\n"
                         "viewing this sub-brick" ) ;
   MCW_reghint_children( brick_av->wrowcol , "Sub-brick to nudge" ) ;

   /** some miscellaneous controls **/

   SEP_VER(hrc) ;

   interp_av = new_MCW_arrowval(
                        hrc ,                   /* parent Widget */
                        "Resampling" ,          /* label */
                        MCW_AV_optmenu ,        /* option menu style */
                        0 ,                     /* first option */
                        NRESAM-1 ,              /* last option */
                        3 ,                     /* initial selection */
                        MCW_AV_readtext ,       /* ignored but needed */
                        0 ,                     /* decimal shift */
                        NULL ,                  /* callback when changed */
                        NULL ,                  /* data for above */
                        MCW_av_substring_CB ,   /* text creation routine */
                        REG_resam_strings       /* data for above */
                      ) ;
   MCW_reghint_children( interp_av->wrowcol , "Set interpolation method" ) ;

   SEP_VER(hrc) ;

   clip_av = new_MCW_arrowval(
                        hrc ,                   /* parent Widget */
                        "Clip" ,                /* label */
                        MCW_AV_optmenu ,        /* option menu style */
                        0 ,                     /* first option */
                        NYESNO-1 ,              /* last option */
                        1 ,                     /* initial selection */
                        MCW_AV_readtext ,       /* ignored but needed */
                        0 ,                     /* decimal shift */
                        NULL ,                  /* callback when changed */
                        NULL ,                  /* data for above */
                        MCW_av_substring_CB ,   /* text creation routine */
                        YESNO_strings           /* data for above */
                      ) ;
   MCW_reghint_children( clip_av->wrowcol , "Clip after interpolation?" ) ;

   XtManageChild(hrc) ;

   /********** Angle choosers ***********/

   SEP_HOR(rowcol) ;

   hrc =  XtVaCreateWidget(
           "AFNI" , xmRowColumnWidgetClass , rowcol ,
              XmNorientation  , XmHORIZONTAL ,
              XmNpacking      , XmPACK_TIGHT ,
              XmNadjustLast   , False ,
              XmNadjustMargin , False ,
              XmNtraversalOn  , False ,
              XmNmarginWidth  , 0 ,
              XmNmarginHeight , 0 ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   xstr = XmStringCreateLtoR( "Angles: " ,
                              XmFONTLIST_DEFAULT_TAG ) ;
   (void) XtVaCreateManagedWidget(
                 "AFNI" , xmLabelWidgetClass , hrc ,
                    XmNlabelString , xstr ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   XmStringFree(xstr) ;

   /** the actual angles **/

   roll_av = new_MCW_arrowval( hrc, "I" ,
                                MCW_AV_downup , -300,300,0 ,
                                MCW_AV_editext , 1 ,
                                NULL , NULL , NULL,NULL ) ;
   MCW_reghint_children( roll_av->wrowcol , "Roll angle [I-axis]" ) ;
   XtVaSetValues( roll_av->wtext , XmNcolumns , 6 , NULL ) ;
   SEP_VER(hrc) ;

   pitch_av = new_MCW_arrowval( hrc, "R" ,
                                MCW_AV_downup , -300,300,0 ,
                                MCW_AV_editext , 1 ,
                                NULL , NULL , NULL,NULL ) ;
   MCW_reghint_children( pitch_av->wrowcol , "Pitch angle [R-axis]" ) ;
   XtVaSetValues( pitch_av->wtext , XmNcolumns , 6 , NULL ) ;
   SEP_VER(hrc) ;

   yaw_av = new_MCW_arrowval( hrc, "A" ,
                                MCW_AV_downup , -300,300,0 ,
                                MCW_AV_editext , 1 ,
                                NULL , NULL , NULL,NULL ) ;
   MCW_reghint_children( yaw_av->wrowcol , "Yaw angle [A-axis]" ) ;
   XtVaSetValues( yaw_av->wtext , XmNcolumns , 6 , NULL ) ;
   SEP_VER(hrc) ;

   /** cumulative label **/

   xstr = XmStringCreateLtoR( "--" , XmFONTLIST_DEFAULT_TAG ) ;
   angle_cum_lab = XtVaCreateManagedWidget(
                     "AFNI" , xmLabelWidgetClass , hrc ,
                        XmNlabelString , xstr ,
                        XmNalignment , XmALIGNMENT_CENTER ,
                        XmNinitialResourcesPersistent , False ,
                        XmNrecomputeSize , True ,
#if 0
                        XmNmarginHeight  , 0 ,
                        XmNmarginBottom  , 0 ,
                        XmNmarginLeft    , 0 ,
                        XmNmarginRight   , 0 ,
                        XmNmarginTop     , 0 ,
                        XmNmarginWidth   , 0 ,
#endif
                        XmNtraversalOn , False ,
                     NULL ) ;
   XmStringFree(xstr) ;
   MCW_register_hint( angle_cum_lab , "Cumulative [degrees]" ) ;

   XtManageChild(hrc) ;

   /********** Shift choosers ***********/

   /*** SEP_HOR(rowcol) ; ***/

   hrc =  XtVaCreateWidget(
           "AFNI" , xmRowColumnWidgetClass , rowcol ,
              XmNorientation  , XmHORIZONTAL ,
              XmNpacking      , XmPACK_TIGHT ,
              XmNadjustLast   , False ,
              XmNadjustMargin , False ,
              XmNtraversalOn  , False ,
              XmNmarginWidth  , 0 ,
              XmNmarginHeight , 0 ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   xstr = XmStringCreateLtoR( "Shifts: " ,
                              XmFONTLIST_DEFAULT_TAG ) ;
   (void) XtVaCreateManagedWidget(
                 "AFNI" , xmLabelWidgetClass , hrc ,
                    XmNlabelString , xstr ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   XmStringFree(xstr) ;

   /** the actual shifts **/

   dS_av = new_MCW_arrowval( hrc, "S" ,
                                MCW_AV_downup , -999,999,0 ,
                                MCW_AV_editext , 1 ,
                                NULL , NULL , NULL,NULL ) ;
   MCW_reghint_children( dS_av->wrowcol , "Delta Superior" ) ;
   XtVaSetValues( dS_av->wtext , XmNcolumns , 6 , NULL ) ;
   SEP_VER(hrc) ;

   dL_av = new_MCW_arrowval( hrc, "L" ,
                                MCW_AV_downup , -999,999,0 ,
                                MCW_AV_editext , 1 ,
                                NULL , NULL , NULL,NULL ) ;
   MCW_reghint_children( dL_av->wrowcol , "Delta Left" ) ;
   XtVaSetValues( dL_av->wtext , XmNcolumns , 6 , NULL ) ;
   SEP_VER(hrc) ;

   dP_av = new_MCW_arrowval( hrc, "P" ,
                                MCW_AV_downup , -999,999,0 ,
                                MCW_AV_editext , 1 ,
                                NULL , NULL , NULL,NULL ) ;
   MCW_reghint_children( dP_av->wrowcol , "Delta Posterior" ) ;
   XtVaSetValues( dP_av->wtext , XmNcolumns , 6 , NULL ) ;
   SEP_VER(hrc) ;

   /** cumulative label **/

   xstr = XmStringCreateLtoR( "--" , XmFONTLIST_DEFAULT_TAG ) ;
   shift_cum_lab = XtVaCreateManagedWidget(
                     "AFNI" , xmLabelWidgetClass , hrc ,
                        XmNalignment , XmALIGNMENT_BEGINNING ,
                        XmNlabelString , xstr ,
                        XmNinitialResourcesPersistent , False ,
                        XmNrecomputeSize , True ,
#if 0
                        XmNmarginHeight  , 0 ,
                        XmNmarginHeight  , 0 ,
                        XmNmarginBottom  , 0 ,
                        XmNmarginLeft    , 0 ,
                        XmNmarginRight   , 0 ,
                        XmNmarginTop     , 0 ,
                        XmNmarginWidth   , 0 ,
#endif
                        XmNtraversalOn , False ,
                     NULL ) ;
   XmStringFree(xstr) ;
   MCW_register_hint( shift_cum_lab , "Cumulative [mm]" ) ;

   XtManageChild(hrc) ;

   /*** a set of action buttons below the line ***/

   SEP_HOR(rowcol) ;

   (void) MCW_action_area( rowcol , NUD_actor , NACT ) ;

   nudge_pb = (Widget) NUD_actor[0].data ;
   clear_pb = (Widget) NUD_actor[1].data ;
   undo_pb  = (Widget) NUD_actor[2].data ;
   redo_pb  = (Widget) NUD_actor[3].data ;
   help_pb  = (Widget) NUD_actor[4].data ;
   quit_pb  = (Widget) NUD_actor[5].data ;
   doall_pb = (Widget) NUD_actor[6].data ;
   print_pb = (Widget) NUD_actor[7].data ;

   /*** that's all ***/

   XtManageChild(rowcol) ;
   XtRealizeWidget(shell) ;  /* will not be mapped */
   return ;
}

/*--------------------------------------------------------------------------
   Compute a rotation matrix specified by 3 angles:
      Q = R3 R2 R1, where Ri is rotation about axis axi by angle thi.
   In these routines, the axis codes (ax1,ax2,ax3, hax1,hax2,hax3, and
   adx,ady,adz) are globals, computed when the dataset is loaded.
----------------------------------------------------------------------------*/

static THD_dmat33 rotmatrix( double th1 , double th2 , double th3  )
{
   THD_dmat33 q , p ;

   if( hax1 < 0 ) th1 = -th1 ;
   if( hax2 < 0 ) th2 = -th2 ;
   if( hax3 < 0 ) th3 = -th3 ;

   LOAD_ROT_DMAT( q , th1 , ax1 ) ;
   LOAD_ROT_DMAT( p , th2 , ax2 ) ; q = DMAT_MUL( p , q ) ;
   LOAD_ROT_DMAT( p , th3 , ax3 ) ; q = DMAT_MUL( p , q ) ;

   return q ;
}

/*-----------------------------------------------------------------------
  Compute the rotation angles from the matrix
  [the signs of these may need to be munged]
-------------------------------------------------------------------------*/

static void rotangles( THD_dmat33 rm, double *th1, double *th2, double *th3 )
{
   *th2 = asin( rm.mat[ax3][ax1] ) ;
   *th1 = atan2( -rm.mat[ax3][ax2] , rm.mat[ax3][ax3] ) ;
   *th3 = atan2( -rm.mat[ax2][ax1] , rm.mat[ax1][ax1] ) ;

   if( hax1 < 0 ) *th1 = -(*th1) ;
   if( hax2 < 0 ) *th2 = -(*th2) ;
   if( hax3 < 0 ) *th3 = -(*th3) ;

   return ;
}

/*-----------------------------------------------------------------------
   Compute the shift vector in dataset coordinates from the user inputs.
-------------------------------------------------------------------------*/

static THD_dfvec3 shiftvec( double dx , double dy , double dz )
{
   double qdx=0.0,qdy=0.0,qdz=0.0 ;
   THD_dfvec3 qv ;

   switch( adx ){
      case  1: qdx = -dx ; break ;
      case -1: qdx =  dx ; break ;
      case  2: qdy = -dx ; break ;
      case -2: qdy =  dx ; break ;
      case  3: qdz = -dx ; break ;
      case -3: qdz =  dx ; break ;
   }

   switch( ady ){
      case  1: qdx = -dy ; break ;
      case -1: qdx =  dy ; break ;
      case  2: qdy = -dy ; break ;
      case -2: qdy =  dy ; break ;
      case  3: qdz = -dy ; break ;
      case -3: qdz =  dy ; break ;
   }

   switch( adz ){
      case  1: qdx = -dz ; break ;
      case -1: qdx =  dz ; break ;
      case  2: qdy = -dz ; break ;
      case -2: qdy =  dz ; break ;
      case  3: qdz = -dz ; break ;
      case -3: qdz =  dz ; break ;
   }

   LOAD_DFVEC3(qv,qdx,qdy,qdz) ; return qv ;
}

/*-----------------------------------------------------------------------
   Compute the user-coordinate shifts from the dataset-coordinate vector
-------------------------------------------------------------------------*/

static void shiftdeltas( THD_dfvec3 sv, double *d1, double *d2, double *d3   )
{
   double qdx,qdy,qdz , dx=0.0,dy=0.0,dz=0.0 ;

   UNLOAD_DFVEC3( sv , qdx,qdy,qdz ) ;

   switch( adx ){
      case  1: dx = -qdx ; break ;
      case -1: dx =  qdx ; break ;
      case  2: dx = -qdy ; break ;
      case -2: dx =  qdy ; break ;
      case  3: dx = -qdz ; break ;
      case -3: dx =  qdz ; break ;
   }

   switch( ady ){
      case  1: dy = -qdx ; break ;
      case -1: dy =  qdx ; break ;
      case  2: dy = -qdy ; break ;
      case -2: dy =  qdy ; break ;
      case  3: dy = -qdz ; break ;
      case -3: dy =  qdz ; break ;
   }

   switch( adz ){
      case  1: dz = -qdx ; break ;
      case -1: dz =  qdx ; break ;
      case  2: dz = -qdy ; break ;
      case -2: dz =  qdy ; break ;
      case  3: dz = -qdz ; break ;
      case -3: dz =  qdz ; break ;
   }

   *d1 = dx ; *d2 = dy ; *d3 = dz ; return ;
}

/*-----------------------------------------------------------------------
   Set the cumulative angles/shifts labels from the current state
   (stored in globals rmat and svec)
-------------------------------------------------------------------------*/

static void NUD_setcumlab(void)
{
   double th1,th2,th3 ;
   XmString xstr ;

   rotangles( rmat, &th1,&th2,&th3 ) ;
   th1 *= iha*(180.0/PI) ; th2 *= iha*(180.0/PI) ; th3 *= iha*(180.0/PI) ;
   xstr = XmStringCreateLtoR( NUD_threestring(th1,th2,th3,'I','R','A') ,
                              XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( angle_cum_lab , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   shiftdeltas( svec , &th1,&th2,&th3 ) ;
   xstr = XmStringCreateLtoR( NUD_threestring(th1,th2,th3,'S','L','P') ,
                              XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( shift_cum_lab , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   return ;
}

/*-----------------------------------------------------------------------
  Write a 3drotate partial command to the screen corresponding
  to the current nudge -- 31 Aug 2000
-------------------------------------------------------------------------*/

static void NUD_print_CB( Widget w , XtPointer cd , XtPointer cb )
{
   double th1,th2,th3 ;
   char cbuf[256] = "3drotate" ;

   strcat( cbuf , " " ) ;
   strcat( cbuf , REG_resam_options[interp_av->ival] ) ;

   if( clip_av->ival ) strcat( cbuf , " -clipit" ) ;

   rotangles( rmat, &th1,&th2,&th3 ) ;
   th1 *= iha*(180.0/PI) ; th2 *= iha*(180.0/PI) ; th3 *= iha*(180.0/PI) ;
   strcat( cbuf , " -rotate " ) ;
   strcat( cbuf , NUD_3string(th1,th2,th3,'I','R','A') ) ;

   shiftdeltas( svec , &th1,&th2,&th3 ) ;
   strcat( cbuf , " -ashift " ) ;
   strcat( cbuf , NUD_3string(th1,th2,th3,'S','L','P') ) ;

   strcat( cbuf , " -prefix ???  inputdataset" ) ;

   fprintf(stderr,"\nCurrent Nudge command is:\n%s\n",cbuf ) ;
   return ;
}

/*-----------------------------------------------------------------------
   Push the current state (rmat and svec) onto the undo stack
-------------------------------------------------------------------------*/

static void NUD_undopush(void)
{
   if( undo_nuse >= undo_nall ){
      undo_nall = undo_nuse + 4 ;
      undo_rmat = (THD_dmat33 *)realloc( undo_rmat, sizeof(THD_dmat33)*undo_nall );
      undo_svec = (THD_dfvec3 *)realloc( undo_svec, sizeof(THD_dfvec3)*undo_nall );
   }

   undo_rmat[undo_nuse] = rmat ;
   undo_svec[undo_nuse] = svec ;
   undo_nuse++ ; undo_ntop = undo_nuse ; SENSITIZE(redo_pb,0) ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for Nudge button
---------------------------------------------------------------------*/

static void NUD_nudge_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   float roll,pitch,yaw , dS,dL,dP ;
   THD_dmat33 new_rmat ;
   THD_dfvec3 new_svec , qv ;

   if( dset == NULL ){ XBell(dc->display,100); return; }  /* shouldn't happen */

   roll  = (PI/180.0)*roll_av->fval  ;
   pitch = (PI/180.0)*pitch_av->fval ;
   yaw   = (PI/180.0)*yaw_av->fval   ;
   dS    = dS_av->fval ;
   dL    = dL_av->fval ;
   dP    = dP_av->fval ;

   if( roll==0.0 && pitch==0.0 && yaw==0.0 && dS==0.0 && dL==0.0 && dP==0.0 ){
      (void) MCW_popup_message( nudge_pb ,
                                   " \n"
                                   "** Can't nudge dataset! **\n"
                                   "** All Angle and Shifts **\n"
                                   "** are zero.            **\n" ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
      XBell( dc->display , 100 ) ;
      return ;
   }

   SENSITIZE(undo_pb,1)   ; SENSITIZE(doall_pb,1)    ;
   SENSITIZE(choose_pb,0) ; AV_SENSITIZE(brick_av,0) ;

   new_rmat = rotmatrix( roll, pitch, yaw ) ;
   rmat     = DMAT_MUL( new_rmat , rmat ) ;

   new_svec = DMATVEC( new_rmat , svec )  ;
   qv       = shiftvec( dS , dL , dP ) ;
   svec     = ADD_DFVEC3( new_svec , qv ) ;

   NUD_undopush() ;       /* push new transformatin onto undo list */
   NUD_setcumlab() ;      /* draw the cumulative labels */

   /* actually do something here */

   if( imbase == NULL )                             /* first time: get base */
      imbase = mri_copy( DSET_BRICK(dset,dset_ival) ) ;

   NUD_update_base( nudge_pb ) ; return ;
}

/*-------------------------------------------------------------------
  Callback for Clear button
---------------------------------------------------------------------*/

static void NUD_clear_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   AV_assign_fval( roll_av  , 0.0 ) ;
   AV_assign_fval( pitch_av , 0.0 ) ;
   AV_assign_fval( yaw_av   , 0.0 ) ;
   AV_assign_fval( dS_av    , 0.0 ) ;
   AV_assign_fval( dL_av    , 0.0 ) ;
   AV_assign_fval( dP_av    , 0.0 ) ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for Undo button
---------------------------------------------------------------------*/

static void NUD_undo_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( undo_nuse <= 1 ){ XBell(dc->display,100); return; }

   undo_nuse-- ;
   rmat = undo_rmat[undo_nuse-1] ;
   svec = undo_svec[undo_nuse-1] ;
   NUD_setcumlab() ;
   if( undo_nuse <= 1 ){ SENSITIZE(undo_pb,0); SENSITIZE(doall_pb,0); }
   SENSITIZE(redo_pb,1) ;

   /* actually do something here */

   NUD_update_base( undo_pb ) ; return ;
}

/*-------------------------------------------------------------------
  Callback for Redo button
---------------------------------------------------------------------*/

static void NUD_redo_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( undo_ntop <= undo_nuse ){ XBell(dc->display,100); return; }

   rmat = undo_rmat[undo_nuse] ;
   svec = undo_svec[undo_nuse] ;
   undo_nuse++ ;
   NUD_setcumlab() ;
   if( undo_nuse >= undo_ntop ) SENSITIZE(redo_pb,0) ;
   SENSITIZE(undo_pb,1) ; SENSITIZE(doall_pb,1) ;

   /* actually do something here */

   NUD_update_base( redo_pb ) ; return ;
}

/*-------------------------------------------------------------------
  Callback for Quit button
---------------------------------------------------------------------*/

static void NUD_quit_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( dset != NULL ){
      DSET_unlock(dset) ; DSET_unload(dset) ; DSET_anyize(dset) ;
      if( undo_nuse > 1 ){                 /* if not at the null nudge */
         MCW_invert_widget(quit_pb) ;
         THD_load_statistics( dset ) ;
         PLUTO_dset_redisplay( dset ) ;
         AFNI_process_drawnotice( im3d ) ;
         MCW_invert_widget(quit_pb) ;
      }
      dset = NULL ;
   }

   if( imbase != NULL ){ mri_free(imbase); imbase = NULL; }

   if( undo_rmat != NULL ){ free(undo_rmat); undo_rmat = NULL; }
   if( undo_svec != NULL ){ free(undo_svec); undo_svec = NULL; }
   undo_nall = undo_nuse = 0 ;

   XtUnmapWidget( shell ) ; nudger_open = 0 ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for help button
---------------------------------------------------------------------*/

static void NUD_help_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   (void ) new_MCW_textwin( help_pb ,

     "PURPOSE: Nudge a dataset's position a little.\n"
     "\n"
     "CONTROLS:\n"
     "Choose Dataset: button to choose which dataset to move around.\n"
     "Brick:          which single sub-brick of the dataset will be moved,\n"
     "                  prior to use of 'Do All'.\n"
     "Resampling:     choose interpolation method for brick resampling\n"
     "Clip:           clip each brick after interpolation?\n"
     "---------------------------------------------------------------------\n"
     "Angles: the entry fields are the rotational angles to be applied:\n"
     "          positive I = roll  = looking to the left\n"
     "          positive R = pitch = nodding the head forward\n"
     "          positive A = yaw   = tilting left ear towards shoulder\n"
     "Shifts: the entry fields are the translational shifts to be applied.\n"
     "          positive S = superior  = shifting head upwards\n"
     "          positive L = left      = shifting head leftwards\n"
     "          positive P = posterior = shifting head backwards\n"
     "---------------------------------------------------------------------\n"
     "Nudge:  apply the Angles and Shifts entered above to the chosen Brick;\n"
     "          also updates the cumulative angles/shifts\n"
     "Clear:  set the Angles and Shifts to zero\n"
     "Undo:   undo the previous Nudge\n"
     "Redo:   redo the previously undone Nudge\n"
     "Quit:   exit, restoring the dataset to its values stored on disk\n"
     "Do All: apply cumulative angles/shifts to all sub-bricks; write to disk\n"
     "Print:  print (to stderr) 3drotate command equivalent to current nudge\n"
     "        [use 'Print' before 'Do All', since 'Do All' sets nudge to 0]\n"
     "=======================================================================\n"
     "USAGE SUGGESTIONS:\n"
     "* Load the dataset and brick to nudge into this plugin.\n"
     "* Switch the image viewers to the same dataset and brick;\n"
     "    as the brick is nudged, then images will be redrawn.\n"
     "* You can also use the rendering plugin - if DynaDraw is on,\n"
     "    the brick will be re-rendered with each nudge.\n"
     "* If you are comparing the nudged dataset to a reference, and\n"
     "    trying to realign the two, one way is to temporarily make\n"
     "    one of them a fim dataset (using '3drefit -fim'), and then\n"
     "    display it as a color overlay.  When you are happy with\n"
     "    the alignment, you can quit AFNI and use 3drefit to change\n"
     "    the dataset back to whatever it was before.\n"
     "* When using the '-fim' trick on the nudged dataset, you will\n"
     "    have to set the colors and color scaling range appropriately\n"
     "    on the 'Define Function' control panel, otherwise the color\n"
     "    overlay will look so peculiar as to be useless.\n"
     "* Nudge-ing on the single sub-brick is done only in memory, so if\n"
     "    you Quit, the dataset on disk will be unchanged.  When you use\n"
     "    'Do All', all sub-bricks will be nudged the same way and then\n"
     "    be written out to disk, overwriting the original dataset .BRIK.\n"
     "* Instead of using 'Do All', you can use 'Print' to see the 3drotate\n"
     "    parameters to use.  You can then apply these to as many datasets\n"
     "    you want (e.g., in a shell script, to nudge a whole bunch of\n"
     "    datasets exactly the same way).\n"
     "* I suggest you do NOT nudge functional activation maps.  It is better\n"
     "    to nudge the anatomical underlay, or nudge the original EPI time\n"
     "    series.  Nudging a dataset implies interpolating to a new grid,\n"
     "    and this is problematical for the non-smooth activation maps.\n"
     "=======================================================================\n"
     "WARNINGS:\n"
     "* Values past the edge of the dataset are 0, and if they are shifted\n"
     "    into the volume covered by the dataset, you will get 0's there.\n"
     "* Values shifted past the edge of the volume covered by the dataset\n"
     "    will be LOST.  This may seem obvious, but when you are shifting\n"
     "    a functional dataset that is smaller than the anatomical underlay,\n"
     "    it can look mysterious.\n"
     "* One solution to the problem above is to use 3dZeropad to explicitly\n"
     "    put a layer of 0's around the outside of the functional dataset\n"
     "    volume.  Shifted values will then go into this 0 buffer, and\n"
     "    will not be lost.\n"
     "=======================================================================\n"
     "ALGORITHM:\n"
     "* Uses the same basic routines as program 3drotate; see\n"
     "      RW Cox and A Jesmanowicz.\n"
     "      Real-time 3D image registration for functional MRI.\n"
     "      Magnetic Resonance in Medicine, 42: 1014-1018, 1999.\n"
     "    Also see 3drotate.c, 3dvolreg.c, and thd_rot3d.c.\n"
     "* Bricks are not repeatedly interpolated as you nudge - each nudge\n"
     "    takes place using the cumulative angles/shifts starting from the\n"
     "    brick read in from disk.  However, if you re-nudge a dataset\n"
     "    after using 'Do All' to write to disk, you will then be re-\n"
     "    interpolating an already interpolated dataset.\n"
     "* Note that cumulative angles/shifts may not be exactly the sum of\n"
     "    the incremental nudges.  This effect is due to the non-Abelian\n"
     "    nature of 3D rotation (i.e., doing rotation A then B is not the\n"
     "    same as doing rotation B then A).\n"
     "* The angle and shift parameters are specified in the same order\n"
     "    as output by 3dvolreg, and would be input to 3drotate as\n"
     "      -rotate <roll>I <pitch>R <yaw>A  -ashift <dS>S <dL>L <dP>P\n"
     "* Rotations are about the center of the rectangular volume of the\n"
     "    dataset.  This is not likely to be the center of the brain.\n"
     "=======================================================================\n"
     "AUTHOR: RWCox, April 2000\n"
     "=======================================================================\n"

    , TEXT_READONLY ) ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for choose button - give the user some dataset choices.
  Criteria for datasets that can be nudged:
    - must be in current session
    - must have actual bricks
---------------------------------------------------------------------*/

static int                  ndsl = 0 ;
static PLUGIN_dataset_link * dsl = NULL ;

static void NUD_choose_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   THD_session * ss  = im3d->ss_now ;           /* current session */
   int           vv  = im3d->vinfo->view_type ; /* view type */
   THD_3dim_dataset * qset ;
   int id , ltop , llen ;
   char qnam[THD_MAX_NAME] , label[THD_MAX_NAME] ;
   static char ** strlist = NULL ;

   /* can't do this if a dataset is already active and changed */

   if( dset != NULL && undo_nuse > 1 ){
      (void) MCW_popup_message( choose_pb ,
                                   "Can't change datasets until\n"
                                   "you save the changes you've\n"
                                   "already made.  Or you could\n"
                                   "'Quit' and re-start the Editor" ,
                                MCW_USER_KILL | MCW_TIMER_KILL ) ;
      XBell( dc->display , 100 ) ;
      return ;
   }

   /* initialize */

   ndsl = 0 ;

   /* scan anats */

   for( id=0 ; id < ss->num_anat ; id++ ){
      qset = ss->anat[id][vv] ;

      if( ! ISVALID_DSET (qset) ) continue ;  /* skip */
      if( ! DSET_INMEMORY(qset) ) continue ;
      if( DSET_BRICK_TYPE(qset,0) == MRI_complex ) continue ;

      ndsl++ ;
      dsl = (PLUGIN_dataset_link *)
              XtRealloc( (char *) dsl , sizeof(PLUGIN_dataset_link)*ndsl ) ;

      make_PLUGIN_dataset_link( qset , dsl + (ndsl-1) ) ;
   }

   /* scan funcs */

   for( id=0 ; id < ss->num_func ; id++ ){
      qset = ss->func[id][vv] ;

      if( ! ISVALID_DSET (qset) ) continue ;  /* skip */
      if( ! DSET_INMEMORY(qset) ) continue ;
      if( DSET_BRICK_TYPE(qset,0) == MRI_complex ) continue ;

      ndsl++ ;
      dsl = (PLUGIN_dataset_link *)
              XtRealloc( (char *) dsl , sizeof(PLUGIN_dataset_link)*ndsl ) ;

      make_PLUGIN_dataset_link( qset , dsl + (ndsl-1) ) ;
   }

   /* found nothing? exit */

   if( ndsl < 1 ){
      (void) MCW_popup_message( choose_pb ,
                                   " \nDidn't find any datasets to edit!\n" ,
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
                       NUD_finalize_dset_CB , NULL     ) ;

   return ;
}

/*---------------------------------------------------------------------------
   Make a label for a sub-brick selector menu
-----------------------------------------------------------------------------*/

static char * NUD_brick_av_label_CB( MCW_arrowval * av , XtPointer cd )
{
   static char blab[32] ;
   THD_3dim_dataset * dset = (THD_3dim_dataset *) cd ;
   static char *lfmt[3] = { "#%1d %-14.14s", "#%2d %-14.14s", "#%3d %-14.14s" };
   static char *rfmt[3] = { "%-14.14s #%1d", "%-14.14s #%2d", "%-14.14s #%3d" };

   if( ISVALID_3DIM_DATASET(dset) ){

#ifdef USE_RIGHT_BUCK_LABELS
      if( DSET_NVALS(dset) < 10 )
        sprintf(blab, rfmt[0] , DSET_BRICK_LABEL(dset,av->ival) , av->ival ) ;
      else if( DSET_NVALS(dset) < 100 )
        sprintf(blab, rfmt[1] , DSET_BRICK_LABEL(dset,av->ival) , av->ival ) ;
      else
        sprintf(blab, rfmt[2] , DSET_BRICK_LABEL(dset,av->ival) , av->ival ) ;
#else
      if( DSET_NVALS(dset) < 10 )
        sprintf(blab, lfmt[0] , av->ival , DSET_BRICK_LABEL(dset,av->ival) ) ;
      else if( DSET_NVALS(dset) < 100 )
        sprintf(blab, lfmt[1] , av->ival , DSET_BRICK_LABEL(dset,av->ival) ) ;
      else
        sprintf(blab, lfmt[2] , av->ival , DSET_BRICK_LABEL(dset,av->ival) ) ;
#endif
   }
   else
      sprintf(blab," #%d ",av->ival) ;  /* should not happen! */

   return blab ;
}

/*------------------------------------------------------------------------------
  Called when the user actually makes the choice of dataset
--------------------------------------------------------------------------------*/

static void NUD_finalize_dset_CB( Widget w, XtPointer fd, MCW_choose_cbs * cbs )
{
   int id = cbs->ival ;
   THD_3dim_dataset * qset ;
   XmString xstr ;
   char str[256] ;

   /* check for errors */

   if( !nudger_open ){ POPDOWN_strlist_chooser; XBell(dc->display,100); return; }

   if( dset != NULL && undo_nuse > 1 ){ XBell(dc->display,100); return; }

   if( id < 0 || id >= ndsl ){ XBell(dc->display,100); return; }

   qset = PLUTO_find_dset( &(dsl[id].idcode) ) ;  /* the new dataset */

   if( qset == NULL ){ XBell(dc->display,100); return; } /* shouldn't happen */

   /* if not same as old dataset, close that one down */

   if( dset != NULL && qset != dset ){
      DSET_unlock(dset) ; DSET_unload(dset) ; DSET_anyize(dset) ;
   }

   /* accept this dataset */

   dset = qset ; dset_idc = dset->idcode ;

   undo_nuse = 1 ;
   undo_ntop = 1 ;
   rmat = undo_rmat[0] ;
   svec = undo_svec[0] ; NUD_setcumlab() ;

   SENSITIZE(undo_pb ,0) ;
   SENSITIZE(redo_pb ,0) ;
   SENSITIZE(nudge_pb,1) ;
   SENSITIZE(doall_pb,0) ;

   /* write the informational label */

   xstr = XmStringCreateLtoR( dsl[id].title , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( info_lab , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   /* lock and load this one into memory (not mmap) */

   DSET_mallocize(dset) ; DSET_lock(dset) ; DSET_load(dset) ;

   if( imbase != NULL ){ mri_free(imbase); imbase = NULL; }

   /* refit the sub-brick selector menu */

   if( dset_ival >= DSET_NVALS(dset) ) dset_ival = DSET_NVALS(dset)-1 ;

   refit_MCW_optmenu( brick_av ,
                      0 ,                       /* new minval */
                      DSET_NVALS(dset)-1 ,      /* new maxval */
                      dset_ival ,               /* new inival */
                      0 ,                       /* new decim? */
                      NUD_brick_av_label_CB ,   /* text routine */
                      dset                      /* text data */
                    ) ;

   AV_SENSITIZE( brick_av , (DSET_NVALS(dset) > 1) ) ;

   /* set codes indicating rotation axes:
      iha = left or right handed coordinate order in dataset
      ax1 = axis index for 'I'   hax1 = sign for roll angle
      ax2 = axis index for 'R'   hax2 = sign for pitch angle
      ax3 = axis index for 'A'   hax3 = sign for yaw angle   */

   iha = THD_handedness( dset ) ;
   ax1 = THD_axcode(dset,'I') ; hax1 = ax1 ; ax1 = abs(ax1)-1 ; /* roll */
   ax2 = THD_axcode(dset,'R') ; hax2 = ax2 ; ax2 = abs(ax2)-1 ; /* pitch */
   ax3 = THD_axcode(dset,'A') ; hax3 = ax3 ; ax3 = abs(ax3)-1 ; /* yaw */

   adx = THD_axcode(dset,'S') ;  /* for shifts */
   ady = THD_axcode(dset,'L') ;
   adz = THD_axcode(dset,'P') ;

#if 0
   fprintf(stderr,"NUD_finalize_dset_CB: iha=%d\n"
                  "  ax1 =%2d ax2 =%2d ax3 =%2d\n"
                  "  hax1=%2d hax2=%2d hax3=%2d\n"
                  "  adx =%2d ady =%2d adz =%2d\n" ,
           iha , ax1,ax2,ax3 , hax1,hax2,hax3 , adx,ady,adz ) ;
#endif

   return ;
}

/*-------------------------------------------------------------------
  Callback for sub-brick index arrowval
---------------------------------------------------------------------*/

static void NUD_brick_av_CB( MCW_arrowval * av , XtPointer cd )
{
   if( imbase != NULL ){ XBell(dc->display,100); return; }
   dset_ival = av->ival ;
   return ;
}

/*-------------------------------------------------------------------
   Rotate an image in place according to the current specs
---------------------------------------------------------------------*/

static void NUD_rotate( MRI_IMAGE * im )
{
   int clipit=clip_av->ival , mode=REG_resam_ints[interp_av->ival] ;
   float cbot,ctop ;
   float * fvol ;
   double th1,th2,th3 , dx,dy,dz ;

   if( im == NULL || dset == NULL ) return ;

   rotangles( rmat, &th1,&th2,&th3 ) ;
   if( hax1 < 0 ) th1 = -th1 ;
   if( hax2 < 0 ) th2 = -th2 ;
   if( hax3 < 0 ) th3 = -th3 ;
   UNLOAD_DFVEC3( svec , dx,dy,dz ) ;

#if 0
fprintf(stderr,"th1=%g th2=%g th3=%g\n",th1,th2,th3) ;
#endif

   /* nothing to do? */

   if( fabs(th1) < EPS && fabs(th2) < EPS && fabs(th3) < EPS &&
       fabs(dx)  < EPS && fabs(dy)  < EPS && fabs(dz)  < EPS   ) return ;

#if 0
   if( clipit && mode == MRI_LINEAR ) clipit = 0 ;
#endif

   /* need a copy? */

   if( im->kind != MRI_float ){
      fvol = (float *) malloc( sizeof(float) * im->nvox ) ;
      EDIT_coerce_type( im->nvox  ,
                        im->kind  , mri_data_pointer(im) ,
                        MRI_float , fvol ) ;
   } else {
      fvol = MRI_FLOAT_PTR(im) ;
   }

   /* compute bounds? */

   if( clipit ){
      register int ii ; register float bb,tt ;
      bb = tt = fvol[0] ;
      for( ii=1 ; ii < im->nvox ; ii++ ){
              if( fvol[ii] < bb ) bb = fvol[ii] ;
         else if( fvol[ii] > tt ) tt = fvol[ii] ;
      }
      cbot = bb ; ctop = tt ;
   }

   /* actually rotate! */

   THD_rota_method( mode ) ;  /* this line fixed 28 Nov 2000 */

   THD_rota_vol( im->nx , im->ny , im->nz ,
                 fabs(DSET_DX(dset)), fabs(DSET_DY(dset)), fabs(DSET_DZ(dset)),
                 fvol , ax1,th1 , ax2,th2 , ax3,th3 , DELTA_AFTER,dx,dy,dz ) ;

   /* apply bounds? */

   if( clipit ){
      register int ii ; register float bb,tt ;
      bb = cbot ; tt = ctop ;
      for( ii=0 ; ii < im->nvox ; ii++ ){
              if( fvol[ii] < bb ) fvol[ii] = bb ;
         else if( fvol[ii] > tt ) fvol[ii] = tt ;
      }
   }

   /* convert type? */

   if( im->kind != MRI_float ){
      EDIT_coerce_type( im->nvox , MRI_float , fvol ,
                        im->kind  , mri_data_pointer(im) ) ;
      free(fvol) ;
   }

   return ;
}

/*--------------------------------------------------------------------------
  Actually nudge the base brick, and then redisplay it.
----------------------------------------------------------------------------*/

static void NUD_update_base(Widget w)
{
   MRI_IMAGE * im ;

   if( dset == NULL || imbase == NULL || dset_ival >= DSET_NVALS(dset) ) return;

   if( w != NULL ) MCW_invert_widget(w) ;

   im = mri_copy(imbase) ;                                  /* copy base */
   NUD_rotate( im ) ;                                       /* rotate copy */
   EDIT_substitute_brick( dset , dset_ival ,                /* put into dset */
                          im->kind , mri_data_pointer(im) );

   if( ISVALID_STATISTIC(dset->stats) )                     /* 27 Nov 2000 */
      THD_update_statistics( dset ) ;

   mri_clear_data_pointer( im ) ; mri_free(im) ;            /* toss the trash */

   PLUTO_dset_redisplay( dset ) ;                           /* re-show it */
   AFNI_process_drawnotice( im3d ) ;                        /* anyone cares? */
   if( w != NULL ) MCW_invert_widget(w) ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for Do All button - nudge all bricks
---------------------------------------------------------------------*/

static void NUD_doall_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   int iv , nvals ;
   MRI_IMAGE * im ;
   char str[256] ;
   double th1,th2,th3 ;
   Widget meter ;

   if( dset == NULL || imbase == NULL || undo_nuse == 1 ){  /* bad bad bad */
      XBell(dc->display,100); return;
   }

   /*----- actually do something -----*/

   /* copy imbase back into dataset */

   EDIT_substitute_brick( dset , dset_ival ,
                          imbase->kind , mri_data_pointer(imbase) ) ;
   mri_clear_data_pointer(imbase) ; mri_free(imbase) ; imbase = NULL ;

   /* nudge each sub-brick */

   nvals = DSET_NVALS(dset) ;
   if( nvals > 1 )
      meter = MCW_popup_meter( shell , METER_TOP_WIDE ) ;

   for( iv=0 ; iv < nvals ; iv++ ){
      MCW_invert_widget(doall_pb) ;

      im = mri_copy( DSET_BRICK(dset,iv) ) ;
      NUD_rotate( im ) ;
      EDIT_substitute_brick( dset , iv ,
                             im->kind , mri_data_pointer(im) ) ;
      mri_clear_data_pointer( im ) ; mri_free(im) ;

      if( nvals > 1 )
         MCW_set_meter( meter , (int)(100.0*(iv+0.5)/nvals) ) ;
   }

   /* store the history of what we just did */

   rotangles( rmat, &th1,&th2,&th3 ) ;
   th1 *= iha*(180.0/PI) ; th2 *= iha*(180.0/PI) ; th3 *= iha*(180.0/PI) ;
   sprintf(str,"plug_nudge: -rotate %s",
           NUD_threestring(th1,th2,th3,'I','R','A') ) ;

   iv = strlen(str) ;
   shiftdeltas( svec , &th1,&th2,&th3 ) ;
   sprintf(str+iv," -ashift %s" ,
           NUD_threestring(th1,th2,th3,'S','L','P') ) ;

   tross_Append_History( dset , str );

   /* write to disk, and redisplay */

   if( nvals > 1 ) MCW_set_meter( meter , 100 ) ;

   DSET_write( dset ) ;
   PLUTO_dset_redisplay( dset ) ;
   AFNI_process_drawnotice( im3d ) ;

   /*----- reset to 0 nudge -----*/

   rmat = undo_rmat[0] ; svec = undo_svec[0] ; NUD_setcumlab() ;

   /* clear undo stack */

   undo_nuse = undo_ntop = 1 ;
   SENSITIZE(undo_pb,0)  ; SENSITIZE(redo_pb,0) ;

   /* can't Do All again right now */

   SENSITIZE(doall_pb,0) ;

   /* allow user to change datasets again */

   SENSITIZE(choose_pb,1) ;
   AV_SENSITIZE( brick_av , (nvals > 1) ) ;

   if( nvals > 1 )
      MCW_popdown_meter(meter) ;
   if( nvals%2 == 1 ) MCW_invert_widget(doall_pb) ;

   return ;
}
