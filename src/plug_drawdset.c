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
  Plugin to draw values into a dataset.
  Makes a custom interface.
************************************************************************/

/*---------- prototypes for internal routines ----------*/

char * DRAW_main( PLUGIN_interface * ) ;

void DRAW_make_widgets(void) ;

void DRAW_done_CB  ( Widget , XtPointer , XtPointer ) ;
void DRAW_undo_CB  ( Widget , XtPointer , XtPointer ) ;
void DRAW_help_CB  ( Widget , XtPointer , XtPointer ) ;
void DRAW_quit_CB  ( Widget , XtPointer , XtPointer ) ;
void DRAW_save_CB  ( Widget , XtPointer , XtPointer ) ;
void DRAW_choose_CB( Widget , XtPointer , XtPointer ) ;
void DRAW_color_CB ( MCW_arrowval * , XtPointer ) ;
void DRAW_mode_CB  ( MCW_arrowval * , XtPointer ) ;
void DRAW_value_CB ( MCW_arrowval * , XtPointer ) ;
void DRAW_fillin_CB( Widget , XtPointer , XtPointer ) ; /* 19 Mar 2001 */

void DRAW_ttatlas_CB( Widget , XtPointer , XtPointer ) ; /* 22 Aug 2001 */

void DRAW_receiver( int , int , void * , void * ) ;
int  DRAW_into_dataset( int , int * , int * , int * , void * ) ;
void DRAW_finalize_dset_CB( Widget , XtPointer , MCW_choose_cbs * ) ;
void DRAW_2dfiller( int nx , int ny , int ix , int jy , byte * ar ) ;

static PLUGIN_interface * plint = NULL ;

static int infill_mode = 0 ;

/***********************************************************************
   Set up the interface to the user.  Note that we bypass the
   normal interface creation, and simply have the menu selection
   directly call the main function, which will create a custom
   set of interface widgets.
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{

   if( ncall > 0 ) return NULL ;  /* only one interface */

   plint = PLUTO_new_interface( "Draw Dataset" , NULL , NULL ,
                                PLUGIN_CALL_IMMEDIATELY , DRAW_main ) ;

   PLUTO_add_hint( plint , "Interactive Dataset Editor" ) ;

   PLUTO_set_sequence( plint , "A:olddset:editor" ) ;

   return plint ;
}

/***************************************************************************
  Will be called from AFNI when user selects from Plugins menu.
****************************************************************************/

/* Interface widgets */

static Widget shell=NULL , rowcol , info_lab , choose_pb ;
static Widget done_pb , undo_pb , help_pb , quit_pb , save_pb ;
static MCW_arrowval * value_av , * color_av , * mode_av ;

static MCW_arrowval * fillin_dir_av , * fillin_gap_av ; /* 19 Mar 2001 */
static Widget fillin_doit_pb ;

static Widget         ttatlas_rowcol=NULL ;             /* 22 Aug 2001 */
static MCW_arrowval * ttatlas_region_av ,
                    * ttatlas_hemisphere_av ;
static Widget         ttatlas_actar ;

#define HAVE_TTATLAS (ttatlas_rowcol != NULL)

#define NHEMI       3
#define HEMI_LEFT   "Left only"
#define HEMI_RIGHT  "Right only"
#define HEMI_BOTH   "Both"
static char *HEMI_strings[NHEMI] = { HEMI_LEFT , HEMI_RIGHT , HEMI_BOTH } ;

#define NUM_TTATLAS_ACT         2
#define TTATLAS_overwrite_label "Load: OverWrite"
#define TTATLAS_infill_label    "Load: InFill"

static MCW_action_item TTATLAS_act[] = {
 { TTATLAS_overwrite_label , DRAW_ttatlas_CB, NULL,NULL, NULL, 0 } ,
 { TTATLAS_infill_label    , DRAW_ttatlas_CB, NULL,NULL, NULL, 0 }
} ;

typedef struct {
   int reg_num ;                 /* number of regions               */
   char *reg_label [TTO_COUNT] ; /* region labels                   */
   short reg_tto   [TTO_COUNT] ; /* index into afni.h TTO_list      */
   short reg_ttbrik[TTO_COUNT] ; /* which sub-brick in TTatlas+tlrc */
   short reg_ttval [TTO_COUNT] ; /* what value in TTatlas+tlrc      */
} ttatlas_compendium ;

static ttatlas_compendium *ttatlas_list=NULL ;

/* Other data */

#define MODE_CURVE       0
#define MODE_CLOSED      1
#define MODE_POINTS      2
#define MODE_FLOOD_VAL   3
#define MODE_FLOOD_NZ    4
#define MODE_FLOOD_ZERO  5   /* 30 Jan 1999 */
#define MODE_ZERO_VAL    6   /* 31 Jan 1999 */

static char * mode_strings[] = {
  "Open Curve"   , "Closed Curve"   , "Points"      ,
  "Flood->Value" , "Flood->Nonzero" , "Flood->Zero" , "Zero->Value" } ;

static int    mode_ints[] = {
  DRAWING_LINES  , DRAWING_FILL   , DRAWING_POINTS ,
  DRAWING_POINTS , DRAWING_POINTS , DRAWING_POINTS , DRAWING_POINTS } ;

#define NUM_modes (sizeof(mode_ints)/sizeof(int))

#define NFILLIN_DIR 3
static char * fillin_dir_strings[NFILLIN_DIR] = { "A-P" , "I-S" , "R-L" } ;
#define NFILLIN_GAP 9

static MCW_DC * dc ;                 /* display context */
static Three_D_View * im3d ;         /* AFNI controller */
static THD_3dim_dataset * dset ;     /* The dataset!    */
static MCW_idcode         dset_idc ; /* 31 Mar 1999     */

static int   color_index = 1 ;               /* from color_av */
static int   mode_ival   = MODE_CURVE ;
static int   mode_index  = DRAWING_LINES ;   /* from mode_av  */
static int   value_int   = 1 ;               /* from value_av */
static float value_float = 1.0 ;             /* ditto         */

static int editor_open  = 0 ;
static int dset_changed = 0 ;
static int recv_open    = 0 ;
static int recv_key     = -1;

static int undo_bufsiz = 0 ;     /* size of undo_buf in bytes */
static int undo_bufnum = 0 ;     /* size of undo_xyz in ints */
static int undo_bufuse = 0 ;     /* number of entries in undo buffer */
static void * undo_buf = NULL ;  /* stores data to be copied back to dataset */
static int  * undo_xyz = NULL ;  /* stores voxel indices for copying */

static THD_dataxes dax_save ;    /* save this for later referenc */

char * DRAW_main( PLUGIN_interface * plint )
{
   XmString xstr ;

   /*-- sanity checks --*/

   if( ! IM3D_OPEN(plint->im3d) )
      return " \n AFNI Controller\nnot opened?! \n " ;

   if( editor_open ){
      XtMapWidget(shell) ;
      XRaiseWindow( XtDisplay(shell) , XtWindow(shell) ) ;
      return NULL ;
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

   { char ttl[PLUGIN_STRING_SIZE] ;
     sprintf(ttl , "AFNI Editor %s" , AFNI_controller_label(im3d) ) ;
     XtVaSetValues( shell , XmNtitle , ttl , NULL ) ;
   }

   /*-- set the info label --*/

   xstr = XmStringCreateLtoR( "[No dataset]" ,
                              XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( info_lab , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   /*-- 22 Aug 2001: perhaps allow TT Atlas stuff --*/

   if( HAVE_TTATLAS )
      XtSetSensitive( ttatlas_rowcol , CAN_TALTO(im3d) ) ;

   /*-- pop the widget up --*/

   XtMapWidget(shell) ;
   PLUTO_cursorize(shell) ;

   /*-- misc initialization --*/

   dset         = NULL ;   /* not editing anything   */
   dset_changed = 0 ;      /* not yet changed */
   editor_open  = 1 ;      /* editor is now open for business */
   recv_open    = 0 ;      /* receiver is not yet open */
   recv_key     = -1;      /* and has no identifier key */

   SENSITIZE(undo_pb,0) ;  undo_bufuse = 0 ;
   SENSITIZE(save_pb,0) ;
   SENSITIZE(choose_pb,1) ;

   return NULL ;
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

void DRAW_make_widgets(void)
{
   XmString xstr ;

   /*** top level shell for window manager ***/

   shell =
      XtVaAppCreateShell(
           "AFNI" , "AFNI" , topLevelShellWidgetClass , dc->display ,

           XmNtitle             , "AFNI Editor" , /* top of window */
           XmNiconName          , "Editor"      , /* label on icon */
           XmNdeleteResponse    , XmDO_NOTHING  , /* deletion handled below */
           XmNallowShellResize  , True ,          /* let code resize shell? */
           XmNmappedWhenManaged , False ,         /* must map it manually */
           XmNinitialResourcesPersistent , False ,
      NULL ) ;

   DC_yokify( shell , dc ) ; /* 14 Sep 1998 */

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
   MCW_register_help( info_lab , "Shows dataset being edited" ) ;
   MCW_register_hint( info_lab , "Shows dataset being edited" ) ;

   /*** separator for visual neatness ***/

   (void) XtVaCreateManagedWidget(
             "AFNI" , xmSeparatorWidgetClass , rowcol ,
                XmNseparatorType , XmDOUBLE_LINE ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   /*** button to let user choose dataset to edit ***/

   xstr = XmStringCreateLtoR( "Choose Dataset" , XmFONTLIST_DEFAULT_TAG ) ;
   choose_pb = XtVaCreateManagedWidget(
                  "AFNI" , xmPushButtonWidgetClass , rowcol ,
                     XmNlabelString , xstr ,
                     XmNtraversalOn , False ,
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
                         "                 to any nonzero point\n"
                         "Flood->Zero    = flood fill from chosen point out\n"
                         "                 to any zero point\n"
                         "Zero->Value    = fill with zeros until the Drawing\n"
                         "                 Value is hit"
                       ) ;
   MCW_reghint_children( mode_av->wrowcol , "How voxels are chosen") ;

   /*** 19 Mar 2001: stuff for linear fillin ***/

   { Widget rc ;

     /*** separator for visual neatness ***/

     (void) XtVaCreateManagedWidget(
                "AFNI" , xmSeparatorWidgetClass , rowcol ,
                   XmNseparatorType , XmDOUBLE_LINE ,
                   XmNinitialResourcesPersistent , False ,
                NULL ) ;

     rc = XtVaCreateWidget( "AFNI" , xmRowColumnWidgetClass , rowcol ,
                       XmNpacking      , XmPACK_TIGHT ,
                       XmNorientation  , XmHORIZONTAL ,
                       XmNmarginHeight , 0 ,
                       XmNmarginWidth  , 0 ,
                       XmNspacing      , 0 ,
                       XmNinitialResourcesPersistent , False ,
                       XmNtraversalOn , False ,
                    NULL ) ;

     fillin_dir_av = new_MCW_optmenu( rc , "Linear Fillin " ,
                                      0 , NFILLIN_DIR-1 , 0 , 0 ,
                                      NULL , NULL ,
                                      MCW_av_substring_CB , fillin_dir_strings ) ;

     fillin_gap_av = new_MCW_optmenu( rc , "Gap" ,
                                      1 , NFILLIN_GAP , 4 , 0 ,
                                      NULL,NULL,NULL,NULL ) ;

     xstr = XmStringCreateLtoR( "Fill" , XmFONTLIST_DEFAULT_TAG ) ;
     fillin_doit_pb = XtVaCreateManagedWidget( "AFNI" , xmPushButtonWidgetClass , rc ,
                                                XmNlabelString , xstr ,
                                                XmNtraversalOn , False ,
                                                XmNinitialResourcesPersistent , False ,
                                               NULL ) ;
     XtAddCallback( fillin_doit_pb , XmNactivateCallback, DRAW_fillin_CB, NULL ) ;
     XmStringFree(xstr) ;
     XtManageChild(rc) ;

   } /* end of fillin */

   /*** 22 Aug 2001: stuff for TT Atlas Regions ***/

   if( TT_load_atlas() > 0 ){
      Widget rc ;
      int ii , jj , nr , qq ;
      XmString xstr ;

      /*** separator for visual neatness ***/

      (void) XtVaCreateManagedWidget(
                 "AFNI" , xmSeparatorWidgetClass , rowcol ,
                    XmNseparatorType , XmDOUBLE_LINE ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;

       /*** rowcol to hold all widgets ***/

       ttatlas_rowcol = rc =
           XtVaCreateWidget( "AFNI" , xmRowColumnWidgetClass , rowcol ,
                      XmNpacking      , XmPACK_TIGHT ,
                      XmNorientation  , XmVERTICAL ,
                      XmNmarginHeight , 0 ,
                      XmNmarginWidth  , 0 ,
                      XmNspacing      , 0 ,
                      XmNinitialResourcesPersistent , False ,
                      XmNtraversalOn , False ,
                   NULL ) ;

       /*** label at top ***/

       xstr = XmStringCreateLtoR( "       TT Atlas Region to Load" ,
                                  XmFONTLIST_DEFAULT_TAG ) ;
       (void) XtVaCreateManagedWidget(
                    "dialog" , xmLabelWidgetClass , rc ,
                       XmNlabelString   , xstr  ,
                       XmNrecomputeSize , False ,
                       XmNmarginWidth   , 0     ,
                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;
       XmStringFree(xstr) ;

       /*** make list of TT atlas regions to include ***/

      ttatlas_list = (ttatlas_compendium *) calloc(1,sizeof(ttatlas_compendium));
      nr = 0 ;
      for( ii=0 ; ii < TTO_COUNT ; ii++ ){

         if( strncmp(TTO_list[ii].name,"Left  ",6) != 0 ) continue ; /* skip */
         if( TTO_list[ii].tdval == 0 )                    continue ; /* skip */

         ttatlas_list->reg_label [nr] = strdup(TTO_list[ii].name+6) ;
         ttatlas_list->reg_tto   [nr] = ii ;
         ttatlas_list->reg_ttbrik[nr] = (TTO_list[ii].tdlev==2) ? 0 : 1 ;
         ttatlas_list->reg_ttval [nr] = TTO_list[ii].tdval ;

         /* trim trailing '.'s */

         qq = 0 ;
         for( jj=strlen(ttatlas_list->reg_label[nr])-1         ;
              jj > 0 && ttatlas_list->reg_label[nr][jj] == '.' ; jj -- ){

            ttatlas_list->reg_label[nr][jj] = '\0' ; qq++ ;
         }
         if( qq > 0 ){
            jj = strlen(ttatlas_list->reg_label[nr]) ;
            ttatlas_list->reg_label[nr][jj] = ' ' ;
         }

         nr++ ;
      }
      ttatlas_list->reg_num = nr ;

      /*** Region chooser ***/

      ttatlas_region_av = new_MCW_optmenu( rc , " " ,
                                           0 , nr-1 , 0 , 0 ,
                                           NULL,NULL ,
                                           MCW_av_substring_CB ,
                                           ttatlas_list->reg_label ) ;
      AVOPT_columnize( ttatlas_region_av , 3 ) ;

      /*** Hemisphere chooser */

      ttatlas_hemisphere_av = new_MCW_optmenu( rc , " Hemisphere(s)" ,
                                               0 , NHEMI-1 , NHEMI-1 , 0 ,
                                               NULL,NULL ,
                                               MCW_av_substring_CB, HEMI_strings );

      /*** row of pushbuttons ***/

      ttatlas_actar = MCW_action_area( rc , TTATLAS_act , NUM_TTATLAS_ACT ) ;

      XtManageChild( rc ) ;

   } /* end of TT Atlas */

   /*** separator for visual neatness ***/

   (void) XtVaCreateManagedWidget(
             "AFNI" , xmSeparatorWidgetClass , rowcol ,
                XmNseparatorType , XmDOUBLE_LINE ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   /*** a set of action buttons below the line ***/

   (void) MCW_action_area( rowcol , DRAW_actor , NACT ) ;

   undo_pb = (Widget) DRAW_actor[0].data ;
   help_pb = (Widget) DRAW_actor[1].data ;
   quit_pb = (Widget) DRAW_actor[2].data ;
   save_pb = (Widget) DRAW_actor[3].data ;
   done_pb = (Widget) DRAW_actor[4].data ;

   /*** that's all ***/

   XtManageChild(rowcol) ;
   XtRealizeWidget(shell) ;  /* will not be mapped */
   return ;
}

/*-------------------------------------------------------------------
  Callback for done button
---------------------------------------------------------------------*/

void DRAW_done_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( dset != NULL ){
      if( recv_open )  /* 31 Mar 1999: changed shutdown to EVERYTHING */
         AFNI_receive_control( im3d, recv_key,EVERYTHING_SHUTDOWN, NULL ) ;
      if( dset_changed ){
         MCW_invert_widget( done_pb ) ;
         DSET_write(dset) ;
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
   return ;
}

/*-------------------------------------------------------------------
  Callback for undo button
---------------------------------------------------------------------*/

void DRAW_undo_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   void * ub ; int * ux, * uy, * uz ;
   int ubs = undo_bufsiz , uis = sizeof(int)*undo_bufuse ;

   if( undo_bufuse <= 0 ){ XBell( dc->display , 100 ) ; return ; }

   /* since the undo_stuff will be modified by the
      drawing function, we must make temporary copies */

   ub =         malloc(ubs) ; memcpy(ub,undo_buf,ubs) ;
   ux = (int *) malloc(uis) ; memcpy(ux,undo_xyz,uis) ;

   DRAW_into_dataset( undo_bufuse , ux,NULL,NULL , ub ) ;

   free(ub) ; free(ux) ;

   AFNI_process_drawnotice( im3d ) ;  /* 30 Mar 1999 */

   return ;
}

/*-------------------------------------------------------------------
  Callback for quit button
---------------------------------------------------------------------*/

void DRAW_quit_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( dset != NULL ){
      if( recv_open ) AFNI_receive_control( im3d, recv_key,DRAWING_SHUTDOWN, NULL ) ;
      DSET_unlock(dset) ;
      DSET_unload(dset) ; DSET_anyize(dset) ;
      if( dset_changed ){
         if( recv_open ) AFNI_process_drawnotice( im3d ) ;  /* 30 Mar 1999 */
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
   return ;
}

/*-------------------------------------------------------------------
  Callback for save button
---------------------------------------------------------------------*/

void DRAW_save_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( dset == NULL ){ XBell( dc->display , 100 ) ; return ; }

   MCW_invert_widget(save_pb) ;

   DSET_write(dset) ; dset_changed = 0 ; SENSITIZE(choose_pb,1) ;

   MCW_invert_widget(save_pb) ; SENSITIZE(save_pb,0) ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for help button
---------------------------------------------------------------------*/

void DRAW_help_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   (void ) new_MCW_textwin( help_pb ,

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
  "        * It is probably best that the dataset being edited be\n"
  "            displayed.  Otherwise it will be impossible to gauge\n"
  "            the effect of the editing operations.\n"
  "        * At this time, only datasets that have a single sub-brick\n"
  "            can be edited.\n"
  "        * Datasets may be copied with the 'Dataset Copy' plugin.\n"
  "            Making an empty dataset with a given geometry can be\n"
  "            done using the 'Zero [One]' option in that plugin.\n"
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
  "        * 'Closed Curve' means to close the curve drawn from the last\n"
  "            point drawn (where button 2 is released) back to the\n"
  "            first point drawn (where button 2 was pressed).\n"
  "        * 'Points' means to take only the voxels corresponding\n"
  "            to the screen pixels about which X11 sends notice.\n"
  "        * 'Flood->Value' means to flood fill outwards from the first\n"
  "            chosen voxel, stopping when the Dataset Value is reached.\n"
  "            In conjunction with 'Closed Curve', it can be used to draw\n"
  "            an entire region in a plane.\n"
  "        * 'Flood->Nonzero' means to flood fill, but stopping when any\n"
  "            nonzero voxel value is reached.\n"
  "        * 'Flood->Zero' means to flood fill, but stopping when any\n"
  "            zero voxel value is reached.\n"
  "        * 'Zero->Value' means to flood fill the slice with zeros,\n"
  "            stopping when a voxel with the drawing value is reached.\n"
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
  "            datasets) or on the grayscale map (for Anat datasets).\n"
  "        * That is, the drawing color is ONLY used while button2\n"
  "            is pressed down.  This color should simply be chosen\n"
  "            to provide good contrast for the drawing operations.\n"
  "        * Pressing and releasing button 2 in a graph window\n"
  "            sub-graph will cause that single voxel to get the\n"
  "            drawing value, as well.  You cannot select a group\n"
  "            of voxels in a graph window -- only one voxel per click.\n"
  "        * Linear Fillin provides the same functionality as program\n"
  "            3dRowFillin.  It lets you fill in gaps (zeros) between\n"
  "            the same value in a particular anatomical direction.\n"
  "            For example, you could draw on every 4th coronal slice,\n"
  "            and then use Fill in the A-P direction with a maximum\n"
  "            gap setting of 3 to fill in the slices you didn't draw.\n"
  "            (Then you could manually fix up the intermediate slices.)\n"
  "           N.B.: Linear Fillin cannot be undone!!!\n"
  "        * TT Atlas Regions can be loaded into the edited volume.  The\n"
  "            chosen region+hemisphere(s) will be loaded with the current\n"
  "            Drawing Value.  'OverWrite' loading means that all voxels\n"
  "            from the region will be replaced with the Drawing Value.\n"
  "            'InFill' loading means that only voxels that are currently\n"
  "            nonzero will be replaced with the Drawing Value.\n"
  "           N.B.: TT Atlas regions may not be good representations of\n"
  "                   any given subject's anatomy.  You will probably\n"
  "                   want to edit the mask after doing the loading.\n"
  "                 This feature requires the presence of the TTatlas+tlrc\n"
  "                   dataset in the plugin directory.  It also requires\n"
  "                   that you be editing in +tlrc coordinates, or in\n"
  "                   +orig coordinates with a mapping to +tlrc coordinates\n"
  "                   having already been established.\n"
  "                 Unlike Linear Fillin, TT Atlas drawing can be undone.\n"
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
  "WARNINGS++:\n"
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
  "  * Note that using a Session rescan button (from the 'Define Datamode'\n"
  "      control panel) will close all datasets while rescanning the\n"
  "      session.  This can result in the loss of un-Saved edits.\n"
  "  * It is possible to edit the same dataset that you are also viewing\n"
  "      with the 'Render Dataset' plugin.  In this way, you can see a\n"
  "      3D visualization of your drawing as you do it.\n"
  "  * If you are drawing anatomically-based ROIs, you can only draw every\n"
  "      5th slice (say) and then use program 3dRowFillin to fill in the\n"
  "      inter-slice gaps.\n"
  "  * Edit at your own risk!  Be careful out there.\n"
  "\n"
  "SUGGESTIONS?\n"
  "  * Please send them to " COXEMAIL "\n"
  "  * Even better than suggestions are implementations.\n"
  "  * Even better than implementations are chocolate chip bagels.\n"
  "Author -- RW Cox"

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
---------------------------------------------------------------------*/

static int                  ndsl = 0 ;
static PLUGIN_dataset_link * dsl = NULL ;

void DRAW_choose_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   THD_session * ss  = im3d->ss_now ;           /* current session */
   int           vv  = im3d->vinfo->view_type ; /* view type */
   THD_3dim_dataset * qset ;
   int id , ltop , llen ;
   char qnam[THD_MAX_NAME] , label[THD_MAX_NAME] ;
   static char ** strlist = NULL ;

   /* can't do this if a dataset is already active and changed */

   if( dset != NULL && dset_changed ){
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

      if( ! ISVALID_DSET (qset)                        ) continue ;  /* skip */
      if( ! DSET_INMEMORY(qset)                        ) continue ;
      if( DSET_NVALS(qset) > 1                         ) continue ;
      if( ! EQUIV_DATAXES(qset->daxes,im3d->wod_daxes) ) continue ;

      ndsl++ ;
      dsl = (PLUGIN_dataset_link *)
              XtRealloc( (char *) dsl , sizeof(PLUGIN_dataset_link)*ndsl ) ;

      make_PLUGIN_dataset_link( qset , dsl + (ndsl-1) ) ;
   }

   /* scan funcs */

   for( id=0 ; id < ss->num_func ; id++ ){
      qset = ss->func[id][vv] ;

      if( ! ISVALID_DSET (qset)                        ) continue ;  /* skip */
      if( ! DSET_INMEMORY(qset)                        ) continue ;
      if( DSET_NVALS(qset) > 1                         ) continue ;
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
                       DRAW_finalize_dset_CB , NULL     ) ;

   return ;
}

void DRAW_finalize_dset_CB( Widget w, XtPointer fd, MCW_choose_cbs * cbs )
{
   int id = cbs->ival ;
   THD_3dim_dataset * qset ;
   XmString xstr ;
   char str[256] ;

   /* check for errors */

   if( ! editor_open ){ POPDOWN_strlist_chooser ; XBell(dc->display,100) ; return ; }

   if( dset != NULL && dset_changed ){ XBell(dc->display,100) ; return ; }

   if( id < 0 || id >= ndsl ){ XBell(dc->display,100) ; return ; }

   qset = PLUTO_find_dset( &(dsl[id].idcode) ) ;  /* the new dataset? */

   if( qset == NULL ){ XBell(dc->display,100) ; return ; }

   if( ! EQUIV_DATAXES( im3d->wod_daxes , qset->daxes ) ){
      XBell(dc->display,100) ; return ;
   }

   /* accept this dataset */

   dset = qset ; dset_changed = 0 ; SENSITIZE(save_pb,0) ;
   dax_save = *(dset->daxes) ;
   dset_idc = qset->idcode ;   /* 31 Mar 1999 */

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
      recv_key = id = AFNI_receive_init( im3d, RECEIVE_DRAWING_MASK   |
                                               RECEIVE_DSETCHANGE_MASK ,  /* 31 Mar 1999 */
                                         DRAW_receiver,NULL ) ;

      if( id < 0 ){
         (void) MCW_popup_message( im3d->vwid->top_shell ,
                                     "Unable to establish\n"
                                     "connection to AFNI\n"
                                     "drawing routines!" ,
                                   MCW_USER_KILL | MCW_TIMER_KILL ) ;

         dset = NULL ; XBell(dc->display,100) ; return ;
      }
   }

   DSET_mallocize(dset) ; DSET_lock(dset) ; DSET_load(dset) ;

   AFNI_receive_control( im3d, recv_key,mode_index , NULL ) ;
   AFNI_receive_control( im3d, recv_key,DRAWING_OVCINDEX, (void *)color_index ) ;
   recv_open = 1 ;

   undo_bufuse = 0 ; SENSITIZE(undo_pb,0) ;

   return ;
}

/*-------------------------------------------------------------------
  Callback for color menu
---------------------------------------------------------------------*/

void DRAW_color_CB( MCW_arrowval * av , XtPointer cd )
{
   color_index = av->ival ;

   if( dset != NULL && recv_open )
      AFNI_receive_control( im3d, recv_key,DRAWING_OVCINDEX, (void *)color_index ) ;

   return ;
}

/*-------------------------------------------------------------------
  Callback for mode menu
---------------------------------------------------------------------*/

void DRAW_mode_CB( MCW_arrowval * av , XtPointer cd )
{
   mode_ival  = av->ival ;
   mode_index = mode_ints[mode_ival] ;

   if( dset != NULL && recv_open )
      AFNI_receive_control( im3d, recv_key,mode_index , NULL ) ;

   return ;
}

/*-------------------------------------------------------------------
  Callback for value menu
---------------------------------------------------------------------*/

void DRAW_value_CB( MCW_arrowval * av , XtPointer cd )
{
   value_int   = av->ival ;
   value_float = av->fval ;
   return ;
}

/*******************************************************************
   Receive data from AFNI after drawing, etc.
********************************************************************/

void DRAW_receiver( int why , int np , void * vp , void * cbd )
{
   switch( why ){

      default:
         fprintf(stderr,"DRAW_receiver: illegal why=%d\n",why) ;
      return ;

      /*-- we like this one --*/

      case RECEIVE_POINTS:{
         int **ip = (int **)vp ;
         int *xd=ip[0] , *yd=ip[1] , *zd=ip[2] ; /* pts coords */
         int mode=ip[3][0] ;                     /* how pts are organized */
         int plane ;

         if( np <= 0 ) return ;  /* some error? */

         plane = mode - SINGLE_MODE ;
         if( plane < 1 || plane > 3 ) plane = mode - PLANAR_MODE ;
         if( plane < 1 || plane > 3 ) plane = 0 ;

         /* anything but flood mode --> just draw given points */

         if( plane == 0 ||
             ((mode_ival != MODE_FLOOD_VAL )  &&
              (mode_ival != MODE_FLOOD_NZ  )  &&
              (mode_ival != MODE_FLOOD_ZERO)  &&
              (mode_ival != MODE_ZERO_VAL  ))   ){

            DRAW_into_dataset( np , xd,yd,zd , NULL ) ;

         } else {

            /* flood mode! */

            int   ityp = DSET_BRICK_TYPE(dset,0) ;
            float bfac = DSET_BRICK_FACTOR(dset,0) ;
            int nx=DSET_NX(dset) , ny=DSET_NY(dset) , nz=DSET_NZ(dset) ,
                nxy = nx*ny , nxyz = nxy*nz , ii,jj , ixyz ;
            int base , di,dj , itop,jtop,nij , xx=xd[0],yy=yd[0],zz=zd[0] , ix,jy ;
            byte * pl ;
            int nfill , * xyzf , nf ;

            /* compute stuff for which plane we are in:
                1 -> yz , 2 -> xz , 3 -> xy            */

            switch(plane){
               case 1: base=xx    ; di=nx; dj=nxy; itop=ny; jtop=nz; ix=yy; jy=zz; break;
               case 2: base=yy*nx ; di=1 ; dj=nxy; itop=nx; jtop=nz; ix=xx; jy=zz; break;
               case 3: base=zz*nxy; di=1 ; dj=nx ; itop=nx; jtop=ny; ix=xx; jy=yy; break;
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

                  if( mode_ival == MODE_FLOOD_ZERO ) val = 0 ;

                  if( mode_ival == MODE_FLOOD_VAL  ||
                      mode_ival == MODE_FLOOD_ZERO || mode_ival == MODE_ZERO_VAL ){
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

                  if( mode_ival == MODE_FLOOD_ZERO ) val = 0 ;

                  if( mode_ival == MODE_FLOOD_VAL  ||
                      mode_ival == MODE_FLOOD_ZERO || mode_ival == MODE_ZERO_VAL ){
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

                  if( mode_ival == MODE_FLOOD_ZERO ) val = 0 ;

                  if( mode_ival == MODE_FLOOD_VAL  ||
                      mode_ival == MODE_FLOOD_ZERO || mode_ival == MODE_ZERO_VAL ){
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
               return ;

            } /* end of switch on type */

            /* start point must be a 0 (can't fill from an edge) */

            if( pl[ix+jy*itop] == 1 ){
               free(pl) ; XBell(dc->display,100) ; return ;
            }

            /* call a routine to fill the array */

            DRAW_2dfiller( itop,jtop , ix,jy , pl ) ;

            /* all filled points are 2 --> these are the locations to draw */

            nfill = 0 ;
            for( ii=0 ; ii < nij ; ii++ ) nfill += (pl[ii] == 2) ;
            if( nfill == 0 ){ free(pl) ; XBell(dc->display,100) ; return ; }

            xyzf = (int *) malloc( sizeof(int) * nfill ) ;

            for( nf=0,jj=0 ; jj < jtop ; jj++ ){
               for( ii=0 ; ii < itop ; ii++ ){
                  if( pl[ii+jj*itop] == 2 )
                     xyzf[nf++] = base + ii*di + jj*dj ;
               }
            }

            free(pl) ;

            if( mode_ival == MODE_ZERO_VAL ){ bfac = value_float; value_float = 0.0; }

            DRAW_into_dataset( nfill , xyzf,NULL,NULL , NULL ) ;

            if( mode_ival == MODE_ZERO_VAL ) value_float = bfac ;

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

      /*-- user changed dataset pointers on us? --*/

      case RECEIVE_DSETCHANGE:{   /* 31 Mar 1999 */
         if( dset != NULL ){
            dset = PLUTO_find_dset( &dset_idc ) ;
            DSET_mallocize(dset) ; DSET_lock(dset) ; DSET_load(dset) ;
            if( dset_changed ){
               THD_load_statistics( dset ) ;
               PLUTO_dset_redisplay( dset ) ;

               XBell( dc->display , 100 ) ;
               (void) MCW_popup_message( im3d->vwid->top_shell ,
                                            "********* WARNING *********\n"
                                            "* Session rescan may have *\n"
                                            "* caused loss of unsaved  *\n"
                                            "* editing changes!        *\n"
                                            "***************************"   ,
                                         MCW_USER_KILL | MCW_TIMER_KILL ) ;
            }
         }
      }
      break ;

   } /* end of switch on why */

   return ;
}

/*--------------------------------------------------------------------------
  Routine to draw into a dataset.
  If yd & zd are NULL, then xd is used as the direct 3D array index,
    otherwise xd,yd,zd are used as the 3-index.
  If var == NULL, then the value_av is used, otherwise the array var[]
    will be the source of the data.
----------------------------------------------------------------------------*/

int DRAW_into_dataset( int np , int * xd , int * yd , int * zd , void * var )
{
   int   ityp = DSET_BRICK_TYPE(dset,0) ;
   float bfac = DSET_BRICK_FACTOR(dset,0) ;
   int nx=DSET_NX(dset) , ny=DSET_NY(dset) , nz=DSET_NZ(dset) ,
       nxy = nx*ny , nxyz = nxy*nz , ii , ixyz ;
   int nbytes , ndrawn=0 ;

   /* sanity check */

   if( dset==NULL || np <= 0 || xd==NULL ) return 0 ;

   /* make space for undo */

   nbytes = np * mri_datum_size(ityp) ;       /* bytes needed for save */
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

   switch( ityp ){

      default: fprintf(stderr,"Illegal brick type=%s in AFNI Editor!\n",
                       MRI_TYPE_name[ityp] ) ;
      break ;

#define DOIT (infill_mode==0 || bp[ixyz]==0)

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
            if( ixyz >= 0 && ixyz < nxyz && DOIT ){
               bp[ixyz] = (vvv==NULL) ? val : vvv[ii] ; ndrawn++ ;
            }
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
            if( ixyz >= 0 && ixyz < nxyz && DOIT ){
               bp[ixyz] = (vvv==NULL) ? val : vvv[ii] ; ndrawn++ ;
            }
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
            if( ixyz >= 0 && ixyz < nxyz && DOIT ){
               bp[ixyz] = (vvv==NULL) ? val : vvv[ii] ; ndrawn++ ;
            }
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
            if( ixyz >= 0 && ixyz < nxyz && (infill_mode==0 || bp[ixyz].r==0) ){
               bp[ixyz] = (vvv==NULL) ? val : vvv[ii] ; ndrawn++ ;
            }
         }
      }
      break ;

   } /* end of switch on brick type */

   /* recompute statistics */

   THD_load_statistics( dset ) ;

   /* now redisplay dataset, in case anyone is looking at it */

   PLUTO_dset_redisplay( dset ) ;

   undo_bufuse  = np ;
   dset_changed = 1 ;
   SENSITIZE(save_pb,1) ;
   SENSITIZE(choose_pb,0) ;
   SENSITIZE(undo_pb,1) ;

   return ndrawn ;
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

void DRAW_2dfiller( int nx , int ny , int ix , int jy , byte * ar )
{
   int ii,jj , ip,jp , num ;

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

   return ;
}

/*----------------------------------------------------------------------------------*/

void DRAW_fillin_CB( Widget w , XtPointer cd , XtPointer cb )
{
   int dcode=-1 , maxgap , nftot ;
   char dir ;

   /* check for errors */

   if( !editor_open || dset == NULL ){ XBell(dc->display,100) ; return ; }

   dir = fillin_dir_strings[ fillin_dir_av->ival ][0] ;

   if( dir == ORIENT_tinystr[dset->daxes->xxorient][0] ||
       dir == ORIENT_tinystr[dset->daxes->xxorient][1]   ) dcode = 1 ;

   if( dir == ORIENT_tinystr[dset->daxes->yyorient][0] ||
       dir == ORIENT_tinystr[dset->daxes->yyorient][1]   ) dcode = 2 ;

   if( dir == ORIENT_tinystr[dset->daxes->zzorient][0] ||
       dir == ORIENT_tinystr[dset->daxes->zzorient][1]   ) dcode = 3 ;

   if( dcode < 0 ){ XBell(dc->display,100) ; return ; } /* should not happen! */

   maxgap = fillin_gap_av->ival ;
   if( maxgap < 1 ){ XBell(dc->display,100) ; return ; } /* should not happen! */

   nftot = THD_dataset_rowfillin( dset , 0 , dcode , maxgap ) ;
   if( nftot > 0 ){
     fprintf(stderr,"++ Fillin filled %d voxels\n",nftot) ;
     PLUTO_dset_redisplay( dset ) ;
     dset_changed = 1 ;
     SENSITIZE(save_pb,1) ;
     if( recv_open ) AFNI_process_drawnotice( im3d ) ;
   } else if( nftot < 0 ) {
      fprintf(stderr,"** Fillin failed for some reason!\n") ;
      XBell(dc->display,100) ;
   } else {
      fprintf(stderr,"++ No Fillin voxels found\n") ;
   }

   return ;
}

/*--------------------------------------------------------------------------
   22 Aug 2001: TT Atlas Regions action callback
----------------------------------------------------------------------------*/

void DRAW_ttatlas_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   THD_3dim_dataset *dseTT ;
   byte *bb , *voxout , bval ;
   int nvoxTT, nvoxout , xx , brik , iv,jv,kv , ijk ;
   int hbot,htop , nzTT,nyTT,nxTT,nxyTT ,
       nxout,nyout,nzout,nxyout , i,j,k,ip,jp,kp , nftot ;
   float dxTT,dyTT,dzTT , xorgTT,yorgTT,zorgTT ;
   float dxout,dyout,dzout , xorgout,yorgout,zorgout ;
   float z1,z2 , y1,y2 , x1,x2 , xx1,xx2,yy1,yy2,zz1,zz2 ;
   float f1,f2,f , g1,g2,g , h1,h2,h , sx,sy,sz , tx,ty,tz , sxyz ;
   THD_fvec3 vv ;

   /* sanity checks */

   if( !editor_open || dset == NULL ){ XBell(dc->display,100) ; return ; }

   if( !CAN_TALTO(im3d) ){ XBell(dc->display,100); return; }

   /* get TTatlas+tlrc dataset */

   dseTT = TT_retrieve_atlas_either() ;
   DSET_load(dseTT) ;

   /* setup other info */

   bval = ttatlas_list->reg_ttval [ ttatlas_region_av->ival ] ;
   brik = ttatlas_list->reg_ttbrik[ ttatlas_region_av->ival ] ;
   bb   = DSET_ARRAY(dseTT,brik) ;
   if( bb == NULL ){ XBell(dc->display,100); return; }

   nvoxTT= DSET_NVOX(dseTT) ;
   nxTT  =dseTT->daxes->nxx  ; nyTT  =dseTT->daxes->nyy  ; nzTT  =dseTT->daxes->nzz  ;
   dxTT  =dseTT->daxes->xxdel; dyTT  =dseTT->daxes->yydel; dzTT  =dseTT->daxes->zzdel;
   xorgTT=dseTT->daxes->xxorg; yorgTT=dseTT->daxes->yyorg; zorgTT=dseTT->daxes->zzorg;

   nvoxout= DSET_NVOX(dset) ;
   voxout = (byte *) calloc(sizeof(byte),nvoxout) ;
   nxout  =dset->daxes->nxx  ; nyout  =dset->daxes->nyy  ; nzout  =dset->daxes->nzz  ;
   dxout  =dset->daxes->xxdel; dyout  =dset->daxes->yydel; dzout  =dset->daxes->zzdel;
   xorgout=dset->daxes->xxorg; yorgout=dset->daxes->yyorg; zorgout=dset->daxes->zzorg;
   nxyout = nxout*nyout ;
   nxyTT  = nxTT *nyTT  ;

   switch( ttatlas_hemisphere_av->ival ){
      case TTRR_HEMI_LEFT:  hbot=1+nxTT/2 ; htop=nxTT     ; break ;
      case TTRR_HEMI_RIGHT: hbot= 0       ; htop=1+nxTT/2 ; break ;

      default:
      case TTRR_HEMI_BOTH:  hbot= 0       ; htop=nxTT     ; break ;
   }

   /* loop over voxels in the TTatlas+tlrc dataset,
      transform to current dataset coordinates,
      count overlap (a la 3dfractionize)            */

   for( kv=0 ; kv < nzTT ; kv++ ){
    z1 = zorgTT + dzTT * (kv-0.5) ; z2 = zorgTT + dzTT * (kv+0.49999) ;

    for( jv=0 ; jv < nyTT ; jv++ ){
     y1 = yorgTT + dyTT * (jv-0.5) ; y2 = yorgTT + dyTT * (jv+0.49999) ;

     for( iv=hbot ; iv < htop ; iv++ ){
      ijk = iv + jv*nxTT + kv*nxyTT ;   /* 1D index of voxel (iv,jv,kv) */
      if( bb[ijk] != bval ) continue ;  /* not the right value, so skip it */

      x1 = xorgTT + dxTT * (iv-0.5) ; x2 = xorgTT + dxTT * (iv+0.49999) ;

      /* input voxel (iv,jv,kv) spans coordinates [x1,x2] X [y1,y2] X [z1,z2] */

      /* transform these corner coordinates to output dataset grid coordinates */

      if( dset->view_type == VIEW_TALAIRACH_TYPE ){
         xx1 = x1 ; yy1 = y1 ; zz1 = z1 ;
         xx2 = x2 ; yy2 = y2 ; zz2 = z2 ;
      } else {
         LOAD_FVEC3(vv , x1,y1,z1) ;
         vv = AFNI_transform_vector( im3d->anat_dset[VIEW_TALAIRACH_TYPE] ,
                                     vv , im3d->anat_now ) ;
         vv = THD_dicomm_to_3dmm( dset , vv );
         UNLOAD_FVEC3(vv , xx1,yy1,zz1) ;

         LOAD_FVEC3(vv , x2,y2,z2) ;
         vv = AFNI_transform_vector( im3d->anat_dset[VIEW_TALAIRACH_TYPE] ,
                                     vv , im3d->anat_now ) ;
         vv = THD_dicomm_to_3dmm( dset , vv ) ;
         UNLOAD_FVEC3(vv , xx2,yy2,zz2) ;
      }

      /* [xx1,xx2] X [yy1,yy2] X [zz1,zz2] is now in coordinates of output dataset */

      /* compute indices into output dataset voxel (keeping fractions) */

      f1 = (xx1-xorgout)/dxout + 0.49999 ; f2 = (xx2-xorgout)/dxout + 0.49999 ;
      if( f1 > f2 ){ tx = f1 ; f1 = f2 ; f2 = tx ; }
      if( f1 >= nxout || f2 <= 0.0 ) continue ;
      if( f1 < 0.0 ) f1 = 0.0 ;  if( f2 >= nxout ) f2 = nxout - 0.001 ;

      g1 = (yy1-yorgout)/dyout + 0.49999 ; g2 = (yy2-yorgout)/dyout + 0.49999 ;
      if( g1 > g2 ){ ty = g1 ; g1 = g2 ; g2 = ty ; }
      if( g1 >= nyout || g2 <= 0.0 ) continue ;
      if( g1 < 0.0 ) g1 = 0.0 ;  if( g2 >= nyout ) g2 = nyout - 0.001 ;

      h1 = (zz1-zorgout)/dzout + 0.49999 ; h2 = (zz2-zorgout)/dzout + 0.49999 ;
      if( h1 > h2 ){ tz = h1 ; h1 = h2 ; h2 = tz ; }
      if( h1 >= nzout || h2 <= 0.0 ) continue ;
      if( h1 < 0.0 ) h1 = 0.0 ;  if( h2 >= nzout ) h2 = nzout - 0.001 ;

      /* input voxel covers voxels [f1,f2] X [g1,g2] X [h1,h2] in the output */

      /* For example, [6.3,7.2] X [9.3,9.6] X [11.7,13.4], which must be     */
      /* distributed into these voxels:                                      */
      /*  (6,9,11), (7,9,11), (6,9,12), (7,9,12), (6,9,13), and (7,9,13)     */

      for( f=f1 ; f < f2 ; f = ip ){
         i = (int) f ; ip = i+1 ; tx = MIN(ip,f2) ; sx = tx - f ;
         for( g=g1 ; g < g2 ; g = jp ){
            j = (int) g ; jp = j+1 ; ty = MIN(jp,g2) ; sy = ty - g ;
            for( h=h1 ; h < h2 ; h = kp ){
               k = (int) h ; kp = k+1 ; tz = MIN(kp,h2) ; sz = tz - h ;
               sxyz = sx * sy * sz ;
               voxout[ i + j*nxout + k * nxyout ] += (byte)(100.0*sxyz) ;
            }
         }
      }

   }}} /* end of loop over voxels */

   /** at this point, voxout[ijk] stores how much overlap each output
       voxel has with an Atlas voxel which had the target value;
       now, count voxels with enough overlap, and store their indexes **/

#define VTHRESH 49  /* at least 49% overlap */

   for( nftot=ijk=0 ; ijk < nvoxout ; ijk++ )
      if( voxout[ijk] >= VTHRESH ) nftot++ ;

   /* now load results into dataset */

   if( nftot > 0 ){
     int *xd = (int *) malloc(sizeof(int)*nftot) , ff ;

     for( ff=ijk=0 ; ijk < nvoxout ; ijk++ )
       if( voxout[ijk] >= VTHRESH ) xd[ff++] = ijk ;

     infill_mode = strcmp(XtName(w),TTATLAS_infill_label) == 0 ;
     ff = DRAW_into_dataset( nftot , xd,NULL,NULL , NULL ) ;
     infill_mode = 0 ;

     free(xd) ;

     fprintf(stderr,"++ %d TT Atlas voxels drawn into dataset\n",ff) ;
     PLUTO_dset_redisplay( dset ) ;
     dset_changed = 1 ;
     SENSITIZE(save_pb,1) ;
     if( recv_open ) AFNI_process_drawnotice( im3d ) ;
   } else {
      fprintf(stderr,"++ No TT Atlas voxels found for some reason!?\a\n") ;
   }

   free(voxout) ; /* toss trash */
   return ;
}
