/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "afni.h"
#include "thd_ttatlas_query.h"
#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

#undef  BGCOLOR_ARG
#define BGCOLOR_ARG(str) \
  XtVaTypedArg , XmNbackground , XmRString , (str) , strlen(str)+1

#undef  RCOL
#define RCOL "#440011"  /* 26 Oct 2007 */
#undef  GCOL
#define GCOL "#003311"

/***********************************************************************
  Plugin to draw values into a dataset.
  Makes a custom interface.
************************************************************************/

/*---------- prototypes for internal routines ----------*/

char * DRAW_main( PLUGIN_interface * ) ;

void DRAW_make_widgets(void) ;

void DRAW_done_CB  ( Widget , XtPointer , XtPointer ) ;
void DRAW_undo_CB  ( Widget , XtPointer , XtPointer ) ;
void DRAW_redo_CB  ( Widget , XtPointer , XtPointer ) ;  /* 19 Nov 2003 */
void DRAW_help_CB  ( Widget , XtPointer , XtPointer ) ;
void DRAW_quit_CB  ( Widget , XtPointer , XtPointer ) ;
void DRAW_save_CB  ( Widget , XtPointer , XtPointer ) ;
void DRAW_saveas_CB( Widget , XtPointer , XtPointer ) ;  /* 24 Sep 2001 */
void DRAW_choose_CB( Widget , XtPointer , XtPointer ) ;
void DRAW_color_CB ( MCW_arrowval * , XtPointer ) ;
void DRAW_mode_CB  ( MCW_arrowval * , XtPointer ) ;
void DRAW_value_CB ( MCW_arrowval * , XtPointer ) ;
void DRAW_fillin_CB( Widget , XtPointer , XtPointer ) ; /* 19 Mar 2001 */

void DRAW_ttatlas_CB( Widget , XtPointer , XtPointer ) ; /* 22 Aug 2001 */

void DRAW_label_CB( Widget , XtPointer , XtPointer ) ; /* 15 Oct 2003 */
void DRAW_label_EV( Widget , XtPointer , XEvent * , Boolean * ) ;
void DRAW_attach_dtable( Dtable *, char *, THD_3dim_dataset * ) ;

void DRAW_receiver( int , int , void * , void * ) ;
int  DRAW_into_dataset( int , int * , int * , int * , void * ) ;
void DRAW_finalize_dset_CB( Widget , XtPointer , MCW_choose_cbs * ) ;
void DRAW_2dfiller( int nx , int ny , int ix , int jy , byte * ar ) ;

void DRAW_saveas_finalize_CB( Widget , XtPointer , MCW_choose_cbs * ) ;

static int Check_value(void);
static void Sensitize_copy_bbox(int);

static void DRAW_2D_expand( int, int *, int *, int *, int, int *, int ** ) ;  /* 07 Oct 2002 */
static void DRAW_3D_expand( int, int *, int *, int *, int, int *, int ** ) ;  /* 07 Oct 2002 */

static void DRAW_2D_circle( int, int *, int *, int *, int, int *, int ** ) ;  /* 16 Oct 2002 */
static void DRAW_3D_sphere( int, int *, int *, int *, int, int *, int ** ) ;  /* 16 Oct 2002 */

#define USE_COLLAPSAR
#ifdef  USE_COLLAPSAR
static void DRAW_collapsar( int * , int * ) ;                                 /* 21 Oct 2002 */
#endif

static PLUGIN_interface * plint = NULL ;

static int infill_mode = 0 ;

void DRAW_set_value_label(void) ;
char * DRAW_value_string( float val ) ;

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
static Widget done_pb, undo_pb,redo_pb, help_pb, quit_pb, save_pb, saveas_pb ;
static MCW_arrowval *value_av , *color_av , *mode_av ;
static MCW_arrowval *rad_av ;                         /* 16 Oct 2002 */
static Widget label_textf , label_label ;             /* 15 Oct 2003 */

#if 0
# define ENABLE_rad_av \
   AV_SENSITIZE( rad_av , (mode_ival >= FIRST_RAD_MODE && mode_ival <= LAST_RAD_MODE) )
#else
# define ENABLE_rad_av                                                   \
   do{ if( mode_ival >= FIRST_RAD_MODE && mode_ival <= LAST_RAD_MODE )  \
         XtManageChild( rad_av->wrowcol ) ;                             \
       else                                                             \
         XtUnmanageChild( rad_av->wrowcol ) ;                           \
   } while(0)
#endif

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

/* 24 Sep 2001: stuff for Copy on input */

static MCW_bbox *copy_bbox ;
static MCW_arrowval *copy_mode_av , *copy_type_av , *copy_datum_av ;

void DRAW_copy_bbox_CB( Widget , XtPointer , XtPointer ) ;
THD_3dim_dataset * DRAW_copy_dset( THD_3dim_dataset *, int,int,int ) ;

/* Other data */

#define MODE_CURVE       0
#define MODE_CLOSED      1
#define MODE_POINTS      2
#define MODE_FLOOD_VAL   3
#define MODE_FLOOD_NZ    4
#define MODE_FLOOD_ZERO  5   /* 30 Jan 1999 */
#define MODE_ZERO_VAL    6   /* 31 Jan 1999 */
#define MODE_FLOOD_VZ    7   /* 30 Apr 2002 */
#define MODE_FILLED      8   /* 25 Sep 2001 */

#define MODE_2D_NN1      9   /* 07 Oct 2002 */
#define MODE_2D_NN2     10
#define MODE_2D_NN3     11
#define MODE_2D_NN4     12
#define MODE_2D_NN5     13

#define MODE_3D_NN1     14
#define MODE_3D_NN2     15
#define MODE_3D_NN3     16
#define MODE_3D_NN4     17
#define MODE_3D_NN5     18
#define MODE_3D_NN6     19
#define MODE_3D_5x5     20   /* 08 Oct 2002 */

#define MODE_2D_CIRC    21   /* 16 Oct 2002 */
#define MODE_3D_SPHR    22   /* 16 Oct 2002 */

#define FIRST_2D_MODE   MODE_2D_NN1
#define LAST_2D_MODE    MODE_2D_NN5

#define FIRST_3D_MODE   MODE_3D_NN1
#define LAST_3D_MODE    MODE_3D_5x5

#define FIRST_RAD_MODE  MODE_2D_CIRC
#define LAST_RAD_MODE   MODE_3D_SPHR

static char * mode_strings[] = {
  "Open Curve"       ,                 /* MODE_CURVE      */
  "Closed Curve"     ,                 /* MODE_CLOSED     */
  "Points"           ,                 /* MODE_POINTS     */
  "Flood->Value"     ,                 /* MODE_FLOOD_VAL  */
  "Flood->Nonzero"   ,                 /* MODE_FLOOD_NZ   */
  "Flood->Zero"      ,                 /* MODE_FLOOD_ZERO */
  "Zero->Value"      ,                 /* MODE_ZERO_VAL   */
  "Flood->Val/Zero"  ,                 /* MODE_FLOOD_VZ   */
  "Filled Curve"     ,                 /* MODE_FILLED     */

  " 2D Nbhd: 1st NN" ,                 /* 07 Oct 2002 */
  " 2D Nbhd: 2nd NN" ,
  " 2D Nbhd: 3rd NN" ,
  " 2D Nbhd: 4th NN" ,
  " 2D Nbhd: 5th NN" ,

  "*3D Nbhd: 1st NN" ,
  "*3D Nbhd: 2nd NN" ,
  "*3D Nbhd: 3rd NN" ,
  "*3D Nbhd: 4th NN" ,
  "*3D Nbhd: 5th NN" ,
  "*3D Nbhd: 6th NN" ,
  "*3D Nbhd: 5x5x5"  ,

  " 2D Circle"       ,                 /* 16 Oct 2002 */
  " 3D Sphere"
} ;

static int  mode_width[] = {    /* 08 Oct 2002: line width for button2 drawing */
  2,2 , 0,0,0,0,0,0 , 2 ,
  3,3,5,5,7 ,
  3,3,3,5,5,5,5 ,
  2,2
} ;

static int    mode_ints[] = {
  DRAWING_LINES  , DRAWING_FILL   , DRAWING_POINTS ,
  DRAWING_POINTS , DRAWING_POINTS , DRAWING_POINTS , DRAWING_POINTS ,
  DRAWING_POINTS ,
  DRAWING_FILL   ,
  DRAWING_LINES  , DRAWING_LINES  , DRAWING_LINES  , DRAWING_LINES  , DRAWING_LINES  ,
  DRAWING_LINES  , DRAWING_LINES  , DRAWING_LINES  , DRAWING_LINES  , DRAWING_LINES  ,
  DRAWING_LINES  , DRAWING_LINES  ,
  DRAWING_LINES  , DRAWING_LINES
} ;

#define NUM_modes (sizeof(mode_ints)/sizeof(int))

#define NFILLIN_DIR 3
static char *fillin_dir_strings[NFILLIN_DIR] = { "A-P" , "I-S" , "R-L" } ;
#define NFILLIN_GAP 9

static MCW_DC *dc ;                 /* display context */
static Three_D_View *im3d ;         /* AFNI controller */
static THD_3dim_dataset *dset ;     /* The dataset!    */
static MCW_idcode        dset_idc ; /* 31 Mar 1999     */

static Dtable *vl_dtable=NULL ;      /* 17 Oct 2003     */

static int   color_index = 1 ;               /* from color_av */
static int   mode_ival   = MODE_FILLED ;
static int   mode_index  = DRAWING_FILL ;    /* from mode_av  */
static int   value_int   = 1 ;               /* from value_av */
static float value_float = 1.0 ;             /* ditto         */

static int editor_open  = 0 ;
static int dset_changed = 0 ;
static int recv_open    = 0 ;
static int recv_key     = -1;

/****** 19 Nov 2003: new stuff for multiple undo/redo ******/

typedef struct {   /* structure holds one drawing operation */
  int npt , btyp ; /* number of data points, type of data */
  int *xyz ;       /* 1D index into dataset array */
  void *buf ;      /* data from dataset array */
} dobuf ;

  /* macro to create an empty buffer */

#define CREATE_DOBUF(db,np,ip)                                       \
 do{ db      = (dobuf *)calloc(1 ,sizeof(dobuf)) ;                   \
     db->xyz = (int *)  calloc(np,sizeof(int)) ;                     \
     db->buf = (void *) calloc(np,mri_datum_size(ip)) ;              \
     db->npt = np ; db->btyp = ip ;                                  \
 } while(0)

  /* macro to delete a buffer from the Macrocosmic All */

#define DESTROY_DOBUF(db)  do{ if( db != NULL ){                     \
                                if( db->xyz != NULL ) free(db->xyz); \
                                if( db->buf != NULL ) free(db->buf); \
                                free(db) ;                           \
                           }} while(0)

  /* amount of memory used by a buffer */

#define SIZEOF_DOBUF(db)                                             \
  ( db->npt * ( sizeof(int) + mri_datum_size(db->btyp) ) )

static int undo_num       = 0 ;     /* depth of undo stack */
static int redo_num       = 0 ;     /* depth of redo stack */
static dobuf **undo_stack = NULL ;  /* undo stack */
static dobuf **redo_stack = NULL ;  /* redo stack */
static int undo_how       = 0 ;     /* where to save undo info */

static void DRAW_undo_sizecheck(void) ;  /* check/limit undo stack size */

static void DRAW_undo_butlab( Widget w , int ) ;  /* label undo/redo button */

#define UNDO_button_labelize DRAW_undo_butlab(undo_pb,undo_num)
#define REDO_button_labelize DRAW_undo_butlab(redo_pb,redo_num)

  /* this macro erases the undo stack */

#define CLEAR_UNDOBUF                                                \
   do{ if( undo_num > 0 || undo_stack != NULL ){                     \
        int ii ;                                                     \
        for( ii=0 ; ii < undo_num ; ii++ )                           \
          DESTROY_DOBUF( undo_stack[ii] ) ;                          \
        if( undo_stack != NULL ) free( undo_stack ) ;                \
        undo_num = 0 ; undo_stack = NULL ;                           \
       }                                                             \
       UNDO_button_labelize ;                                        \
   } while(0)

  /* this macro erases the redo stack */

#define CLEAR_REDOBUF                                                \
   do{ if( redo_num > 0 || redo_stack != NULL ){                     \
        int ii ;                                                     \
        for( ii=0 ; ii < redo_num ; ii++ )                           \
          DESTROY_DOBUF( redo_stack[ii] ) ;                          \
        if( redo_stack != NULL )free( redo_stack ) ;                 \
        redo_num = 0 ; redo_stack = NULL ;                           \
       }                                                             \
       REDO_button_labelize ;                                        \
   } while(0)

  /* this macro erases both stacks */

#define CLEAR_UNREDOBUF                                              \
   do{ CLEAR_UNDOBUF ; CLEAR_REDOBUF ; undo_how = 0 ; } while(0)

/******/

static THD_dataxes dax_save ;    /* save this for later reference */

static int old_stroke_autoplot = 0 ;  /* 27 Oct 2003 */

char * DRAW_main( PLUGIN_interface *plint )
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

   xstr = XmStringCreateLtoR( "[No dataset]" , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( info_lab ,
                    XmNlabelString , xstr ,
                    BGCOLOR_ARG(RCOL) ,
                  NULL ) ;
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

   if( vl_dtable != NULL ){   /* 20 Oct 2003 */
     destroy_Dtable(vl_dtable) ; vl_dtable = NULL ;
   }

   SENSITIZE(save_pb,0) ; SENSITIZE(saveas_pb,0) ;
   SENSITIZE(choose_pb,1) ;
   Sensitize_copy_bbox(1);

   /* 19 Nov 2003: new undo/redo stuff */

   undo_num = redo_num = undo_how = 0 ;
   undo_stack = redo_stack = NULL ;
   UNDO_button_labelize ; REDO_button_labelize ;

   old_stroke_autoplot = AFNI_yesenv("AFNI_STROKE_AUTOPLOT") ;
   if( old_stroke_autoplot ) putenv("AFNI_STROKE_AUTOPLOT=NO") ;

   return NULL ;
}

/*------------------------------------------------------------------------
  Make the control popup for this thing
--------------------------------------------------------------------------*/

/*-- structures defining action buttons (at bottom of popup) --*/

#define NACT 7  /* number of action buttons */

static MCW_action_item DRAW_actor[NACT] = {
 {"Undo[0]",DRAW_undo_CB,NULL,
  "Undoes previous draw\naction, if possible","Undo last change",0} ,

 {"Redo[0]",DRAW_redo_CB,NULL,
  "Redoes previous undone\naction, if possible","Redo last undo",0} ,

 {"Help",DRAW_help_CB,NULL,
  "Displays more help" , "Displays more help",0} ,

 {"Quit",DRAW_quit_CB,NULL,
  "Discard edits since last Save\nand close Editor" ,
   "Discard edits and close",0} ,

 {"Save",DRAW_save_CB,NULL,
  "Save edits to disk\nand continue" , "Save to disk and continue",0} ,

 {"SaveAs",DRAW_saveas_CB,NULL,                        /* 24 Sep 2001 */
  "Save edits to disk\nin a new dataset\nand continue" ,
  "Save to disk in new dataset, continue",0} ,

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
                XmNtraversalOn , True  ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   /*** label at top to let user know who we are ***/

   xstr = XmStringCreateLtoR( "[No dataset]" ,
                              XmFONTLIST_DEFAULT_TAG ) ;
   info_lab = XtVaCreateManagedWidget(
                 "AFNI" , xmLabelWidgetClass , rowcol ,
                    XmNlabelString , xstr ,
                    BGCOLOR_ARG(RCOL) ,
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

   /*-- 24 Sep 2001: Copy mode stuff [moved up 06 Oct 2002] --*/

   { Widget rc ;
     static char *cbox_label[1]   = { "Copy Dataset" } ;
     static char *cmode_label[2]  = { "Data"  , "Zero" } ;
     static char *ctype_label[3]  = { "As Is" , "Func" , "Anat" } ;
     static char *cdatum_label[4] = { "As Is" , "Byte" , "Short" , "Float" } ;

     /*** rowcol to hold Copy widgets ***/

     rc = XtVaCreateWidget( "AFNI" , xmRowColumnWidgetClass , rowcol ,
                              XmNpacking      , XmPACK_TIGHT ,
                              XmNorientation  , XmHORIZONTAL ,
                              XmNmarginHeight , 0 ,
                              XmNmarginWidth  , 0 ,
                              XmNspacing      , 0 ,
                              XmNinitialResourcesPersistent , False ,
                              XmNtraversalOn , True  ,
                           NULL ) ;

     /*** button box to turn copy mode on or off ***/

     copy_bbox = new_MCW_bbox( rc, 1,cbox_label,
                               MCW_BB_check,MCW_BB_noframe, DRAW_copy_bbox_CB,NULL ) ;

     MCW_reghint_children( copy_bbox->wrowcol ,
                           "Make copy of dataset on input" ) ;
     MCW_reghelp_children( copy_bbox->wrowcol ,
                           "Make copy of dataset on input?" ) ;

     /*** arrowvals to let user choose Copy method ***/
     copy_mode_av = new_MCW_optmenu( rc , NULL ,
                                     0 , 1 , 1 , 0 , NULL,NULL ,
                                     MCW_av_substring_CB , cmode_label ) ;

     MCW_reghint_children( copy_mode_av->wrowcol ,
                           "How to copy values from dataset" ) ;
     MCW_reghelp_children( copy_mode_av->wrowcol ,
                           "How to copy values from dataset:\n"
                           "Data => use input dataset values\n"
                           "Zero => fill dataset with zeros" ) ;

     copy_type_av = new_MCW_optmenu( rc , NULL ,
                                     0 , 2 , 1 , 0 , NULL,NULL ,
                                     MCW_av_substring_CB , ctype_label ) ;

     MCW_reghint_children( copy_type_av->wrowcol ,
                           "Copy is Functional overlay or Anatomical underlay" ) ;
     MCW_reghelp_children( copy_type_av->wrowcol ,
                           "Copy will be Functional overlay\n"
                           "or will be Anatomical underlay" ) ;

     copy_datum_av= new_MCW_optmenu( rc , NULL ,
                                     0 , 3 , 0 , 0 , NULL,NULL ,
                                     MCW_av_substring_CB , cdatum_label ) ;

     MCW_reghint_children( copy_datum_av->wrowcol ,
                           "Data storage type for copy" ) ;
     MCW_reghelp_children( copy_datum_av->wrowcol ,
                           "Data storage type for zero-filled copy:\n"
                           "As Is => use data type in input dataset\n"
                           "Byte  => store new dataset as bytes\n"
                           "Short => store new dataset as shorts\n"
                           "Float => store new dataset as floats") ;

     MCW_set_bbox(copy_bbox, 1); /* turn copy on by default - drg 4/3/2006 */

     AV_SENSITIZE( copy_mode_av , True ) ;
     AV_SENSITIZE( copy_type_av , True ) ;
     AV_SENSITIZE( copy_datum_av, True ) ;
     
     XtManageChild(rc) ;

   } /* end of Copy mode stuff */

   /*** button to let user choose dataset to edit ***/

   xstr = XmStringCreateLtoR( "  Choose dataset for copying" , XmFONTLIST_DEFAULT_TAG ) ;
   choose_pb = XtVaCreateManagedWidget(
                  "AFNI" , xmPushButtonWidgetClass , rowcol ,
                     XmNlabelString , xstr ,
                     XmNtraversalOn , True  ,
                     BGCOLOR_ARG(GCOL) ,
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

   /*** separator for visual neatness ***/

   (void) XtVaCreateManagedWidget(
               "AFNI" , xmSeparatorWidgetClass , rowcol ,
                  XmNseparatorType , XmDOUBLE_LINE ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;

   /***  arrowval to choose value that is drawn into dataset voxels  ***/

   { Widget rc ;

     rc = XtVaCreateWidget( "AFNI" , xmRowColumnWidgetClass , rowcol ,
                       XmNpacking      , XmPACK_TIGHT ,
                       XmNorientation  , XmHORIZONTAL ,
                       XmNmarginHeight , 0 ,
                       XmNmarginWidth  , 0 ,
                       XmNspacing      , 0 ,
                       XmNinitialResourcesPersistent , False ,
                       XmNtraversalOn , True  ,
                    NULL ) ;

     value_av = new_MCW_arrowval( rc , "Value " ,
                                  MCW_AV_downup , -32767,32767,value_int ,
                                  MCW_AV_editext , 0 ,
                                  DRAW_value_CB , NULL , NULL,NULL ) ;

     MCW_reghelp_children( value_av->wrowcol ,
                           "Use this to set the value that\n"
                           "will be drawn into the dataset\n"
                           "using mouse button 2."
                         ) ;
     MCW_reghint_children( value_av->wrowcol , "Goes into dataset voxels" ) ;

     /*-- 15 Oct 2003: Label for the value --*/

     xstr = XmStringCreateLtoR( " Label" , XmFONTLIST_DEFAULT_TAG ) ;
     label_label = XtVaCreateManagedWidget(
                     "dialog" , xmLabelWidgetClass , rc ,
                       XmNlabelString   , xstr  ,
                       XmNrecomputeSize , False ,
                       XmNmarginWidth   , 0     ,
                       XmNinitialResourcesPersistent , False ,
                     NULL ) ;
     XmStringFree(xstr) ;

     label_textf = XtVaCreateManagedWidget(
                       "dialog" , xmTextFieldWidgetClass , rc ,
                           XmNcolumns      , 19 ,
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
     XtSetSensitive( label_label , (Boolean)(value_int != 0) ) ;
     XtSetSensitive( label_textf , (Boolean)(value_int != 0) ) ;

     XtAddCallback( label_textf, XmNactivateCallback    ,
                                 DRAW_label_CB , NULL ) ; /* return key */

     XtAddCallback( label_textf, XmNlosingFocusCallback ,
                                 DRAW_label_CB , NULL ) ; /* tab key */

     XtInsertEventHandler( label_textf ,      /* notify when */
                           LeaveWindowMask ,  /* pointer leaves */
                           FALSE ,            /* this window */
                           DRAW_label_EV ,
                           (XtPointer) NULL ,
                           XtListTail ) ;     /* last in queue */

     XtInsertEventHandler( label_label ,      /* button press in label */
                           ButtonPressMask ,
                           FALSE ,
                           DRAW_label_EV ,
                           (XtPointer) NULL ,
                           XtListTail ) ;
     POPUP_cursorize( label_label ) ;

     XtManageChild(rc) ;
   }

   /*** option menu to choose drawing color ***/

   color_av = new_MCW_colormenu( rowcol , "Color " , dc ,
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
   /*-- 16 Oct 2002: put in a horiz rowcol, and add rad_av button --*/

   { Widget rc ;

     rc = XtVaCreateWidget( "AFNI" , xmRowColumnWidgetClass , rowcol ,
                       XmNpacking      , XmPACK_TIGHT ,
                       XmNorientation  , XmHORIZONTAL ,
                       XmNmarginHeight , 0 ,
                       XmNmarginWidth  , 0 ,
                       XmNspacing      , 0 ,
                       XmNinitialResourcesPersistent , False ,
                       XmNtraversalOn , True  ,
                    NULL ) ;

     mode_av = new_MCW_optmenu( rc , "Mode  " ,
                              0 , NUM_modes-1 , mode_ival,0 ,
                              DRAW_mode_CB , NULL ,
                              MCW_av_substring_CB , mode_strings ) ;

     AVOPT_columnize( mode_av , 2 ) ;

     MCW_reghelp_children( mode_av->wrowcol ,
                           "Use this to set the way in which\n"
                           "drawing pixels on the screen is\n"
                           "used to select dataset voxels:\n"
                           "Open Curve      = voxels picked along lines drawn;\n"
                           "Closed Curve    = voxels forming a closed curve\n"
                           "Points          = only voxels at X11 notify pixels;\n"
                           "Flood->Value    = flood fill from the chosen point\n"
                           "                   out to points = Value\n"
                           "Flood->Nonzero  = flood fill from chosen point out\n"
                           "                   to any nonzero point\n"
                           "Flood->Zero     = flood fill from chosen point out\n"
                           "                   to any zero point\n"
                           "Zero->Value     = flood fill with zeros until the\n"
                           "                   Value is hit\n"
                           "Flood->Val/Zero = flood fill from the chosen point\n"
                           "                   until the Value OR zero is hit\n"
                           "Filled Curve    = fill inside of closed curve with\n"
                           "                   Value\n"
                           "\n"
                           "2D Nbhd         = like Open Curve, but fills in around\n"
                           "                   the in-plane neighborhood of each\n"
                           "                   drawn point 'x' with the patterns:\n"
                           "                        5 4 3 4 5\n"
                           "                        4 2 1 2 4\n"
                           "                        3 1 x 1 3\n"
                           "                        4 2 1 2 4\n"
                           "                        5 4 3 4 5\n"
                           "                   where the number indicates the\n"
                           "                   Nearest Neighbor order of the\n"
                           "                   points nearby 'x'.\n"
                           "3D Nbhd         = Similar, but in 3D (out-of-plane)\n"
                           "\n"
                           "2D Circle       = Draw a circle of given Radius\n"
                           "3D Sphere       = Draw a sphere of given Radius\n"
                         ) ;
     MCW_reghint_children( mode_av->wrowcol , "How voxels are chosen") ;

     /** 16 Oct 2002: radius chooser **/

     rad_av = new_MCW_arrowval( rc             ,    /* parent */
                                "R"            ,    /* label */
                                MCW_AV_downup  ,    /* arrow directions */
                                1              ,    /* min value (0.1 mm from decim) */
                                999            ,    /* max value (99.9 mm) */
                                40             ,    /* init value */
                                MCW_AV_editext ,    /* input/output text display */
                                1              ,    /* decimal shift */
                                NULL           ,    /* routine to call when button */
                                NULL           ,    /* is pressed, and its data */
                                NULL,NULL           /* no special display */
                              ) ;
     XtVaSetValues( rad_av->wtext   , XmNcolumns , 5 , NULL ) ;
     MCW_reghint_children( rad_av->wrowcol , "Radius of Circles and Spheres" ) ;
     MCW_reghelp_children( rad_av->wrowcol ,
                            " \n"
                            "Sets the radius (in mm) of the 2D Circle\n"
                            "or 3D Sphere drawing modes.  Voxels whose\n"
                            "center-to-center distance is <= this value\n"
                            "will be filled in.\n"
                          ) ;
     ENABLE_rad_av ;   /* turn it on or off */

     XtManageChild(rc) ;
   }

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
                       XmNtraversalOn , True  ,
                    NULL ) ;

     fillin_dir_av = new_MCW_optmenu( rc , "Linear Fillin " ,
                                      0 , NFILLIN_DIR-1 , 0 , 0 ,
                                      NULL , NULL ,
                                      MCW_av_substring_CB , fillin_dir_strings ) ;

     fillin_gap_av = new_MCW_optmenu( rc , " Gap" ,
                                      1 , NFILLIN_GAP , 4 , 0 ,
                                      NULL,NULL,NULL,NULL ) ;

     xstr = XmStringCreateLtoR( "*Do the Fill*" , XmFONTLIST_DEFAULT_TAG ) ;
     fillin_doit_pb = XtVaCreateManagedWidget( "AFNI" , xmPushButtonWidgetClass , rc ,
                                                XmNlabelString , xstr ,
                                                XmNtraversalOn , True  ,
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
                      XmNtraversalOn , True  ,
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

   undo_pb   = (Widget) DRAW_actor[0].data ;
   redo_pb   = (Widget) DRAW_actor[1].data ;  /* 19 Nov 2003 */
   help_pb   = (Widget) DRAW_actor[2].data ;
   quit_pb   = (Widget) DRAW_actor[3].data ;
   save_pb   = (Widget) DRAW_actor[4].data ;
   saveas_pb = (Widget) DRAW_actor[5].data ;  /* 24 Sep 2001 */
   done_pb   = (Widget) DRAW_actor[6].data ;

   /*** that's all ***/

   XtManageChild(rowcol) ;
   XtRealizeWidget(shell) ; NI_sleep(1) ; /* will not be mapped */
   return ;
}

/*-------------------------------------------------------------------
  Callback for copy_bbox -- 24 Sep 2001 - RWCox
---------------------------------------------------------------------*/

void DRAW_copy_bbox_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   int sens = (MCW_val_bbox(copy_bbox) != 0);
   AV_SENSITIZE( copy_mode_av , sens ) ;
   AV_SENSITIZE( copy_type_av , sens ) ;
   AV_SENSITIZE( copy_datum_av, sens ) ;
   if(sens)
     MCW_set_widget_label( choose_pb , "  Choose dataset for copying" );
   else
     MCW_set_widget_label( choose_pb , "  Choose dataset to change directly" );
   
   return ;
}

/*
  turn on or off copy dataset check box and related buttons 
*/
static void 
Sensitize_copy_bbox(int  sens) 
{
   XtPointer clienttemp=NULL; 

   SENSITIZE(copy_bbox->wbut[0], sens);
   SENSITIZE(copy_bbox->wbut[1], sens);
   /* dummy pointers passed */
   if(sens) 
      DRAW_copy_bbox_CB(copy_bbox->wbut[0], clienttemp, clienttemp);
  else
   {
      AV_SENSITIZE( copy_mode_av , 0 ) ;
      AV_SENSITIZE( copy_type_av , 0 ) ;
      AV_SENSITIZE( copy_datum_av, 0 ) ;
   }
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
       DRAW_attach_dtable( vl_dtable, "VALUE_LABEL_DTABLE",  dset ) ;
       DSET_overwrite(dset) ;
       MCW_invert_widget( done_pb ) ;
     }
     DSET_unlock(dset) ; DSET_anyize(dset) ;
     dset = NULL ; dset_changed = 0 ;
   }

   CLEAR_UNREDOBUF ;  /* 19 Nov 2003 */

   XtUnmapWidget( shell ); editor_open = 0; recv_open = 0; recv_key = -1;
   if( old_stroke_autoplot ) putenv("AFNI_STROKE_AUTOPLOT=YES") ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for undo button [heavily modified 19 Nov 2003]
---------------------------------------------------------------------*/

void DRAW_undo_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   dobuf *sb ;  /* saved drawing buffer that will be redrawn */

   if( undo_num <= 0 || undo_stack == NULL ){ XBell(dc->display,100); return; }

   undo_how = 1 ;  /* the next drawing save will be onto redo stack */

   sb = undo_stack[undo_num-1] ;  /* saved buffer */

   DRAW_into_dataset( sb->npt , sb->xyz,NULL,NULL , sb->buf ) ;

   DESTROY_DOBUF(sb) ;  /* purge and pop top of undo stack */
   undo_num-- ;
   UNDO_button_labelize ;

   AFNI_process_drawnotice( im3d ) ;  /* 30 Mar 1999 */

   undo_how = 0 ; /* further draws go onto undo stack */
   return ;
}

/*-------------------------------------------------------------------
  Callback for redo button [from DRAW_undo_CB(), 19 Nov 2003]
---------------------------------------------------------------------*/

void DRAW_redo_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   dobuf *sb ;

   if( redo_num <= 0 || redo_stack == NULL ){ XBell(dc->display,100); return; }

   undo_how = 2 ;  /* drawing save will be onto undo stack */

   sb = redo_stack[redo_num-1] ;  /* saved buffer */

   DRAW_into_dataset( sb->npt , sb->xyz,NULL,NULL , sb->buf ) ;

   DESTROY_DOBUF(sb) ;  /* purge and pop top of redo stack */
   redo_num-- ;
   REDO_button_labelize ;

   AFNI_process_drawnotice( im3d ) ;  /* 30 Mar 1999 */

   undo_how = 0 ; /* further draws go onto undo stack */
   return ;
}

/*-------------------------------------------------------------------
  Callback for quit button
---------------------------------------------------------------------*/

void DRAW_quit_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( dset != NULL ){
     if( recv_open ) AFNI_receive_control( im3d,recv_key,DRAWING_SHUTDOWN,NULL );
     DSET_unlock(dset) ;
     DSET_unload(dset) ; DSET_anyize(dset) ;
     if( dset_changed ){
       if( recv_open ){
         AFNI_process_drawnotice( im3d ) ;  /* 30 Mar 1999 */
         AFNI_receive_control( im3d, recv_key,EVERYTHING_SHUTDOWN, NULL ) ; /* 25 Sep 2001 */
       }
       MCW_invert_widget(quit_pb) ;
       THD_load_statistics( dset ) ;
       PLUTO_dset_redisplay( dset ) ;
       MCW_invert_widget(quit_pb) ;
     }
     dset = NULL ; dset_changed = 0 ;
   }

   CLEAR_UNREDOBUF ;  /* 19 Nov 2003 */

   XtUnmapWidget( shell ); editor_open = 0; recv_open = 0; recv_key = -1;
   if( old_stroke_autoplot ) putenv("AFNI_STROKE_AUTOPLOT=YES") ;
   return ;
}

/*-------------------------------------------------------------------
  Callback for Save button
---------------------------------------------------------------------*/

void DRAW_save_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( dset == NULL ){ XBell(dc->display,100) ; return ; }

   MCW_invert_widget(save_pb) ;

   DRAW_attach_dtable( vl_dtable, "VALUE_LABEL_DTABLE",  dset ) ;
   DSET_overwrite(dset); dset_changed = 0; SENSITIZE(choose_pb,1);
   Sensitize_copy_bbox(1);   /* turn copy dataset widgets back on  - drg 4/4/2006 */
   MCW_invert_widget(save_pb) ;
   SENSITIZE(save_pb,0) ; SENSITIZE(saveas_pb,0) ;
   return ;
}

/*-------------------------------------------------------------------
  24 Sep 2001: Callback for Save As button
---------------------------------------------------------------------*/

void DRAW_saveas_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( dset == NULL ){ XBell(dc->display,100) ; return ; }

   MCW_choose_string( saveas_pb , "Enter new prefix" ,
                      NULL , DRAW_saveas_finalize_CB , NULL ) ;
}

/*--------------------------------------------------------------------*/

void DRAW_saveas_finalize_CB( Widget w, XtPointer fd, MCW_choose_cbs * cbs )
{
   THD_3dim_dataset *cset ;
   char str[256] ;
   XmString xstr ;

   /*-- check for craziness --*/

   if( !editor_open || dset == NULL ){
     POPDOWN_strlist_chooser; XBell(dc->display,100); return;
   }

   if( !PLUTO_prefix_ok(cbs->cval) ){ XBell(dc->display,100); return; }

   /*-- make a copy of this dataset --*/

   MCW_invert_widget(saveas_pb) ;

   cset = DRAW_copy_dset( dset , 0,0,-1 ) ;
   if( cset == NULL ){                      /* should not happen */
     (void) MCW_popup_message( saveas_pb ,
                                 " \n"
                                 "*** Cannot make copy of edited  ***\n"
                                 "*** dataset for unknown reasons ***\n " ,
                               MCW_USER_KILL | MCW_TIMER_KILL ) ;

     MCW_invert_widget(saveas_pb); XBell(dc->display,100); return;
   }
   EDIT_dset_items( cset , ADN_prefix,cbs->cval , ADN_none ) ;

   if( THD_is_file(DSET_BRIKNAME(cset)) ){  /* stupid user */
     (void) MCW_popup_message( saveas_pb ,
                                 " \n"
                                 "*** Cannot SaveAs this edited   ***\n"
                                 "*** dataset since a dataset     ***\n"
                                 "*** with that prefix is on disk ***\n " ,
                               MCW_USER_KILL | MCW_TIMER_KILL ) ;
     DSET_delete(cset) ;
     MCW_invert_widget(saveas_pb); XBell(dc->display,100); return;
   }

   /*-- tell AFNI about the new dataset --*/

   PLUTO_add_dset( plint , cset , DSET_ACTION_MAKE_CURRENT ) ;

   /*-- remove current dataset from further consideration --*/

   DSET_unlock(dset) ; DSET_unload(dset) ; DSET_anyize(dset) ;

   /*-- switch current dataset to be the copy just made --*/

   dset = cset ; dset_idc = dset->idcode ;
   DRAW_attach_dtable( vl_dtable, "VALUE_LABEL_DTABLE",  dset ) ;
   DSET_overwrite(dset); DSET_mallocize(dset); DSET_load(dset); DSET_lock(dset);

   /*-- re-write the informational label --*/

   if( DSET_BRICK_FACTOR(dset,0) == 0.0 ){
     strcpy(str,DSET_BRIKNAME(dset)) ;
   } else {
     char abuf[16] ;
     AV_fval_to_char( DSET_BRICK_FACTOR(dset,0) , abuf ) ;
     sprintf(str,"%s\nbrick factor: %s", DSET_BRIKNAME(dset) , abuf ) ;
   }
   xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( info_lab ,
                    XmNlabelString , xstr ,
                    BGCOLOR_ARG(GCOL) ,
                  NULL ) ;
   XmStringFree(xstr) ;

   /*-- finish up --*/

   dset_changed = 0 ; SENSITIZE(choose_pb,1) ;
   MCW_invert_widget(saveas_pb) ;
   SENSITIZE(save_pb,0) ; SENSITIZE(saveas_pb,0) ;
   Sensitize_copy_bbox(1); 

   return ;
}

/*-------------------------------------------------------------------
  Callback for Help button
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
  "Step 1) Choose a dataset to edit.\n"
  "        * Only datasets that have data BRIKs stored at the current\n"
  "            resolution as the underlay dataset can be edited.\n"
  "        * It is probably best that the dataset being edited be\n"
  "            displayed.  Otherwise it will be impossible to gauge\n"
  "            the effect of the editing operations.\n"
  "        * At this time, only datasets that have a single sub-brick\n"
  "            can be edited.\n"
  "        * Datasets may be copied when chosen by selecting the\n"
  "            'Copy' toggle (on by default). The choosers to the right\n"
  "            of this let you control how the input dataset is copied:\n"
  "            (a)  Data => data value are copied\n"
  "                 Zero => copy is full of zeros\n"
  "            (b) As Is => copy is same dataset type as input dataset\n"
  "                 Func => copy is a functional overlay (fim) dataset\n"
  "                 Anat => copy is an anatomical underlay dataset\n"
  "            (c) As Is => copy is stored as input dataset is stored\n"
  "                 Byte => copy is stored as bytes\n"
  "                Short => copy is stored as shorts\n"
  "                Float => copy is stored as floats\n"
  "                NOTE: you can only change the data storage of the\n"
  "                      copy from the input if you are using 'Zero';\n"
  "                      with 'Data', the data storage of the copy will\n"
  "                      always be made 'As Is'.\n"
  "            The copy is made when you finalize the dataset choice.\n"
  "            The copied dataset has the same prefix as the input\n"
  "            dataset, with the string 'COPY_' prepended.  You can\n"
  "            alter this name later using the 'Dataset Rename' plugin,\n"
  "            AFTER the dataset has been Save-d, or with the '3drename'\n"
  "            program after you exit AFNI, or with the 'SaveAs' button\n"
  "            in this plugin.\n"
  "        * Datasets may also be copied with the 'Dataset Copy' plugin.\n"
  "            Making an empty dataset with a given geometry can be\n"
  "            done using the 'Zero [One]' option in that plugin.\n"

  "\n"
  "Step 2) Choose a drawing value.\n"
  "        * This is the number that will be placed into the dataset\n"
  "            voxels that are chosen.\n"
  "        * Integer valued datasets can only receive integer values;\n"
  "            float datasets can take floating point values.\n"
  "        * You can attach a label string to each drawing value.\n"
  "            The value-label table will be saved with in the dataset\n"
  "            .HEAD file when you use 'Save', 'SaveAs' or 'Done'.\n"
  "        * You can also setup a standard value-label table in a file,\n"
  "            whose name is specified by setting environment variable\n"
  "            AFNI_VALUE_LABEL_DTABLE -- cf. file README.environment.\n"
  "        * Button-3-clicking in the 'Label' next to the text-entry field\n"
  "            will bring up a menu of all current value-label pairs.\n"
  "            You can choose from them to set a new drawing value.\n"
  "        * Button-1-clicking in the 'Label' will ask for a filename\n"
  "            to read that loads the value-label pairs.  The format\n"
  "            of this file is described in README.environment.\n"
  "            Reading in a file like this will erase any existing\n"
  "            value-label associations in the plugin.\n"
  "\n"
  "Step 3) Choose a drawing color.\n"
  "        * This is the color that will be shown in the image windows\n"
  "            while drawing is going on (that is, while the mouse button\n"
  "            is pressed).\n"
  "        * This color is NOT the color that will be displayed once\n"
  "            you stop drawing (release the mouse button).\n"
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
  "        * 'Flood->Val/Zero' means to flood fill the slice with the\n"
  "            Value until voxels whose values are either zero or the\n"
  "            Value are hit\n"
  "        * 'Filled Curve' means to draw a closed curve and then fill\n"
  "            its interior with the drawing value.  It is similar to\n"
  "            doing 'Closed Curve' followed by 'Flood->Value', but\n"
  "            more convenient.\n"
  "        * '2D Nbhd: Kth NN' is like 'Open Curve', but each the 2D in-slice\n"
  "            neighborhood of a point 'x' is filled in with the following\n"
  "            pattern of points, for K=1..5:\n"
  "                                              5 4 3 4 5\n"
  "                                              4 2 1 2 4\n"
  "                                              3 1 x 1 3\n"
  "                                              4 2 1 2 4\n"
  "                                              5 4 3 4 5\n"
  "            In a cubical lattice with voxel edge length=1, the 2D Kth NN\n"
  "            volume is a 'circle' out to radius:\n"
  "                  K=1  r=sqrt(1)  [e.g., (+1, 0)]\n"
  "                  K=2  r=sqrt(2)  [e.g., (+1,+1) => 3x3 square]\n"
  "                  K=3  r=sqrt(4)  [e.g., (+2, 0)]\n"
  "                  K=4  r=sqrt(5)  [e.g., (+2,+1)]\n"
  "                  K=5  r=sqrt(8)  [the whole 5x5 square about 'x']\n"
  "        * '3D Nbhd: Kth NN' is similar, but with the 3D neighborhood\n"
  "            of each point (so you are drawing out-of-slice).  In this\n"
  "            case, the 3D Kth NN volume is a 'sphere' out to radius\n"
  "                  K=1  r=sqrt(1)  [e.g., (+1, 0, 0)]\n"
  "                  K=2  r=sqrt(2)  [e.g., (+1,+1, 0)]\n"
  "                  K=3  r=sqrt(3)  [e.g., (+1,+1,+1) => 3x3x3 cube]\n"
  "                  K=4  r=sqrt(4)  [e.g., (+2, 0, 0)]\n"
  "                  K=5  r=sqrt(5)  [e.g., (+2,+1, 0)]\n"
  "                  K=6  r=sqrt(6)  [e.g., (+2,+1,+1)]\n"
  "                5x5x5  fills out the 5x5x5 cube about each drawn point.\n"
  "        * '2D Circle' and '3D Sphere' draw in-plane circles and 3D spheres\n"
  "            about each drawn point 'x'.  The radius (in mm) is set using\n"
  "            the 'R' chooser that becomes active when one of these drawing\n"
  "            modes is selected.  These drawing modes use the actual voxel\n"
  "            sizes in the dataset, unlike the 'Nbhd' modes described above.\n"
  "\n"
  "Step 5) Draw something in an image window.\n"
  "        * Drawing is done using mouse button 2 (the middle button).\n"
  "        * If you have a scroll-wheel for the middle button, it can\n"
  "            be hard to use this for drawing.  Two alternatives are:\n"
  "          ** Mouse button 1 (the left button), with the keyboard\n"
  "             Shift key held down simultaneously.\n"
  "          ** Mouse button 1 by itself, if you first click the 'pen'\n"
  "             toggle (at the image viewer's right) to be 'on'.\n"
  "             (Mouse cursor should then change to a stylized pen.)\n"
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
  "        * That is, the drawing color is ONLY used while button 2\n"
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
  "        * TT Atlas Regions can be loaded into the edited volume.  The\n"
  "            chosen region+hemisphere(s) will be loaded with the current\n"
  "            Value.  'OverWrite' loading means that all voxels from\n"
  "            the TT region will be replaced with the Value.\n"
  "            'InFill' loading means that only voxels that are currently\n"
  "            zero in the TT region will be replaced with the Value.\n"
  "           N.B.: TT Atlas regions may not be good representations of\n"
  "                   any given subject's anatomy.  You will probably\n"
  "                   want to edit the mask after doing the loading.\n"
  "                 This feature requires the presence of the TTatlas+tlrc\n"
  "                   (or TTatlas.nii.gz) dataset in the plugin directory.\n"
  "                   It also requires that you be editing in +tlrc coordinates,\n"
  "                   or in +orig coordinates with a mapping to +tlrc\n"
  "                   coordinates having already been established.\n"
  "                 Unlike Linear Fillin, TT Atlas drawing can be undone.\n"
  "\n"
  "Step 6) Undo and Redo.\n"
  "        * The last drawing operation can be undone -- that is,\n"
  "            pressing 'Undo' will restore the voxel values before\n"
  "            the last button 2 press-release operation.\n"
  "        * 'Redo' will undo the previous 'Undo'.\n"
  "        * Multiple levels of Undo/Redo are available.\n"
  "        * The amount of memory set aside for Undo/Redo operations\n"
  "            is controlled by environment variable AFNI_DRAW_UNDOSIZE,\n"
  "            which is in megabytes; its value defaults to 6.\n"
  "        * The numbers (as in '[3]') on the Undo and Redo buttons\n"
  "            indicate how many levels are available at any moment.\n"
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
  "        * The 'SaveAs' button lets you save the changes to a new\n"
  "            dataset.  The new dataset will become the current dataset\n"
  "            for further editing and for AFNI display.\n"
  "\n"
  "++WARNINGS++:\n"
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
  "  * You can only draw into the windows of the AFNI controller from\n"
  "      which the Editor was started.\n"
  "  * Only one copy of the Editor can be active at a time.  If you\n"
  "      use the plugin menu to call up the Editor when it is already\n"
  "      open, that will simply pop the window up to the top of the\n"
  "      stacking order.  If you want to restart the Editor in a\n"
  "      different AFNI controller, you must first close the Editor\n"
  "      (via 'Done' or 'Quit') and then start it from the other\n"
  "      controller's window.\n"
  "  * Peculiar and confusing things can happen using 'Warp-on-Demand'\n"
  "      with the Editor.  My advice is NOT to try this.\n"
  "  * Note that using a Session rescan button (from the 'Define Datamode'\n"
  "      control panel) will close all datasets while rescanning the\n"
  "      session.  This can result in the loss of un-Saved edits.\n"
  "  * It is possible to edit the same dataset that you are also viewing\n"
  "      with the 'Render Dataset' plugin.  In this way, you can see a\n"
  "      3D visualization of your drawing as you do it.  You need to turn\n"
  "      on 'DynaDraw' in the rendering plugin; then, if the dataset you\n"
  "      are drawing on is the same as the renderer's overlay, each drawing\n"
  "      action will cause a re-rendering.  This works well if you have\n"
  "      set the renderer's 'Color Opacity' to 'ShowThru'.  This is also\n"
  "      a lot of fun.\n"
  "  * If you are drawing anatomically-based ROIs, you can only draw every\n"
  "      5th slice (say) and then use program 3dRowFillin to fill in the\n"
  "      inter-slice gaps.  Or use the Linear Fillin interactive feature.\n"
  "  * Edit at your own risk!  You can destroy datasets this way.  That's\n"
  "      why the 'Copy dataset' feature is on by default.\n"
  "  * Be careful out there.\n"
  "\n"
  "SUGGESTIONS?\n"
  "  * Please send them to " COXEMAIL "\n"
  "  * Better than suggestions are implementations.\n"
  "  * Better than implementations are pumpernickel bagels.\n"
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
      XBell(dc->display,100) ; return ;
   }

   /* initialize */

   ndsl = 0 ;

   /* scan datasets */

   for( id=0 ; id < ss->num_dsset ; id++ ){
      qset = ss->dsset[id][vv] ;

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
                                   " \n"
                                   "Didn't find any datasets to edit!\n"
                                   "Check if:\n"
                                   " - you are in 'Warp-on-Demand' mode\n"
                                   " - you are in the correct session\n"
                                   "Also:\n"
                                   " * Only datasets with 1 sub-brick can\n"
                                   "    be edited.\n"
                                   " * The dataset must match the resolution\n"
                                   "    of the current anatomical view.\n"
                               , MCW_USER_KILL | MCW_TIMER_KILL ) ;
      XBell(dc->display,100) ; return ;
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

/*-----------------------------------------------------------------------------*/

void DRAW_finalize_dset_CB( Widget w, XtPointer fd, MCW_choose_cbs *cbs )
{
   int id=cbs->ival , copied=0 ;
   THD_3dim_dataset * qset ;
   XmString xstr ;
   char str[256] , *dtit ;
   THD_slist_find slf ;   /* 29 Jul 2003 */
   MCW_choose_cbs cbss ;

   /*-- check for errors --*/

   if( !editor_open ){ POPDOWN_strlist_chooser; XBell(dc->display,100); return; }

   if( dset != NULL && dset_changed ){ XBell(dc->display,100) ; return ; }

   if( id < 0 || id >= ndsl ){ XBell(dc->display,100) ; return ; }

   qset = PLUTO_find_dset( &(dsl[id].idcode) ) ;  /* the new dataset? */

   if( qset == NULL ){ XBell(dc->display,100) ; return ; }

   if( ! EQUIV_DATAXES( im3d->wod_daxes , qset->daxes ) ){
      XBell(dc->display,100) ; return ;
   }

   /*-- 24 Sep 2001: make a copy of the dataset, if desired --*/

   if( MCW_val_bbox(copy_bbox) != 0 ){
      THD_3dim_dataset *cset ;
      int zfill , ftype , dtype ;

      zfill = (copy_mode_av->ival == 1) ;     /* zero fill? */

      switch( copy_type_av->ival ){
         default: ftype = -1 ; break ;        /* As Is */
         case 1:  ftype =  1 ; break ;        /* Func  */
         case 2:  ftype =  2 ; break ;        /* Anat  */
      }

      switch( copy_datum_av->ival ){
         default: dtype = -1        ; break ; /* As Is */
         case 1:  dtype = MRI_byte  ; break ; /* Byte  */
         case 2:  dtype = MRI_short ; break ; /* Short */
         case 3:  dtype = MRI_float ; break ; /* Float */
      }

      cset = DRAW_copy_dset( qset , zfill,ftype,dtype ) ; /* make copy! */

      if( cset == NULL ){                                 /* this is bad */
         (void) MCW_popup_message( choose_pb ,
                                     " \n"
                                     "*** Cannot make copy of input   ***\n"
                                     "*** dataset for unknown reasons ***\n " ,
                                   MCW_USER_KILL ) ;
         XBell(dc->display,100) ; return ;
      }

      DSET_unload(qset) ;
      PLUTO_add_dset( plint , cset , DSET_ACTION_MAKE_CURRENT ) ;
      qset = cset ; copied = 1 ;
   }

   /*-- accept this dataset --*/

   dset = qset ; dset_changed = 0 ;
   dax_save = *(dset->daxes) ;
   dset_idc = dset->idcode ;   /* 31 Mar 1999 */

   SENSITIZE(save_pb,0) ; SENSITIZE(saveas_pb,0) ;
   Sensitize_copy_bbox(0); 

   /*-- write the informational label --*/

   if( copied ) dtit = DSET_BRIKNAME(dset) ;  /* 24 Sep 2001 */
   else         dtit = dsl[id].title ;

   if( DSET_BRICK_FACTOR(dset,0) == 0.0 ){
      strcpy(str,dtit) ;
   } else {
      char abuf[16] ;
      AV_fval_to_char( DSET_BRICK_FACTOR(dset,0) , abuf ) ;
      sprintf(str,"%s\nbrick factor: %s", dtit , abuf ) ;
   }
   xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( info_lab ,
                    XmNlabelString , xstr ,
                    BGCOLOR_ARG(GCOL) ,
                  NULL ) ;
   XmStringFree(xstr) ;

   /*-- setup AFNI for drawing --*/

   if( ! recv_open ){
      recv_key = id = AFNI_receive_init( im3d, RECEIVE_DRAWING_MASK   |
                                               RECEIVE_DSETCHANGE_MASK ,  /* 31 Mar 1999 */
                                         DRAW_receiver,NULL ,
                                        "DRAW_receiver" ) ;

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
   AFNI_receive_control( im3d, recv_key,DRAWING_OVCINDEX, ITOP(color_index) ) ;
   recv_open = 1 ;

   CLEAR_UNREDOBUF ;  /* 19 Nov 2003 */

   /* 29 Jul 2003: switch to this dataset */

   slf = THD_dset_in_session( FIND_IDCODE , &(dset->idcode) , im3d->ss_now ) ;
   if( slf.dset_index >= 0 ){
     cbss.ival = slf.dset_index ;
     if( ISFUNC(dset) ){
       AFNI_finalize_dataset_CB( im3d->vwid->view->choose_func_pb ,
                                 (XtPointer) im3d ,  &cbss         ) ;
       AFNI_SEE_FUNC_ON(im3d) ; /* 30 Apr 2002 */
     } else {
       AFNI_finalize_dataset_CB( im3d->vwid->view->choose_anat_pb ,
                                 (XtPointer) im3d ,  &cbss         ) ;
     }
   }

   /* 20 Oct 2003: get VALUE_LABEL_DTABLE, if present */

   if( vl_dtable != NULL ){ destroy_Dtable(vl_dtable); vl_dtable = NULL; }

   { ATR_string *atr ;
     atr = THD_find_string_atr( dset->dblk , "VALUE_LABEL_DTABLE" ) ;
     if( atr != NULL && atr->nch > 5 )
       vl_dtable = Dtable_from_nimlstring( atr->ch ) ;
     if( vl_dtable == NULL ){
       char *str = AFNI_suck_file( getenv("AFNI_VALUE_LABEL_DTABLE") ) ;
       if( str != NULL ){ vl_dtable = Dtable_from_nimlstring(str); free(str); }
     }
     DRAW_set_value_label() ;
   }

   return ;
}

/*-------------------------------------------------------------------
  Callback for color menu
---------------------------------------------------------------------*/

void DRAW_color_CB( MCW_arrowval * av , XtPointer cd )
{
   color_index = av->ival ;

   if( dset != NULL && recv_open )
      AFNI_receive_control( im3d, recv_key,DRAWING_OVCINDEX, ITOP(color_index) ) ;

   return ;
}

/*-------------------------------------------------------------------
  Callback for mode menu
---------------------------------------------------------------------*/

void DRAW_mode_CB( MCW_arrowval * av , XtPointer cd )
{
   mode_ival  = av->ival ;
   mode_index = mode_ints[mode_ival] ;

   if( dset != NULL && recv_open ){
      AFNI_receive_control( im3d, recv_key,mode_index , NULL ) ;

      /* 08 Oct 2002: set drawing line width */

      AFNI_receive_control( im3d, recv_key, DRAWING_LINEWIDTH ,
                            ITOP(mode_width[mode_ival])      ) ;
   }

   /* 16 Oct 2002: turn rad_av (radius) on if mode needs it */

   ENABLE_rad_av ;

   return ;
}

/*-------------------------------------------------------------------
  Callback for value menu
---------------------------------------------------------------------*/

void DRAW_value_CB( MCW_arrowval * av , XtPointer cd )
{
   value_int   = av->ival ;
   value_float = av->fval ;

   if( value_float != 0.0 ){
     XtSetSensitive( label_label , True ) ;
     XtSetSensitive( label_textf , True ) ;
     if(Check_value()) {
/*        value_float = 1.0;
	av->ival = 1;
	av->fval = 1.0;
*/        XtSetSensitive( label_label , False ) ;
        XtSetSensitive( label_textf , False ) ;
      }
   } else {
     XtSetSensitive( label_label , False ) ;
     XtSetSensitive( label_textf , False ) ;
   }

   DRAW_set_value_label() ;

   return ;
}

/* check if the value in the value field will be changed 
  once it gets applied to the dataset */
/* error message is popped up if data value can not be applied*/
/* also returns 0 for data okay, 1 if can not use the data value */  
static int Check_value(void)
{
   int   ityp;
   float bfac; 
   float value_float2, delta;
   
   /* sanity check */

   if( dset==NULL) {
     (void) MCW_popup_message( label_textf , \
        "Please choose dataset first\n", \
	 MCW_USER_KILL | MCW_TIMER_KILL) ;
     PLUTO_beep() ;  
   
    return(1) ;
   }
   ityp = DSET_BRICK_TYPE(dset,0) ;
   bfac = DSET_BRICK_FACTOR(dset,0) ;
   
   if( bfac == 0.0 ) bfac = 1.0 ;

   switch( ityp ){

      default: fprintf(stderr,"Illegal brick type=%s in AFNI Editor!\n",
                       MRI_TYPE_name[ityp] ) ;
		       return(0);
      break ;

      case MRI_short:{
        short   val = (short)   (value_float/bfac) ;
	value_float2 = val * bfac;
      }
      break ;

      case MRI_byte:{
        byte   val = (byte)   (value_float/bfac) ;
	value_float2 = val * bfac;
      }
      break ;

      case MRI_float:{
        float   val = (value_float/bfac) ;
	value_float2 = val * bfac;
      }
      break ;

      case MRI_complex:{
        complex   val ;
        /*static complex cxzero = { 0.0 , 0.0 } ;*/

        val = CMPLX( (value_float/bfac) , 0.0 ) ;
        return(0);  /* assume everything is okay for complex for now */
      }
      break ;

   } /* end of switch on brick type */

   delta = fabs(value_float2 - value_float);
   if(delta>0.000001) {
     (void) MCW_popup_message( label_textf , \
          "**************************************************************\n" \
	  "This dataset type does not accept this value in this plug-in\n" \
	  "Use 3D Edit plug-in, 3dcalc or 3dmerge to copy the dataset\n"   \
	  "to a new datum type.\n"
          "**************************************************************",\
	 MCW_USER_KILL | MCW_TIMER_KILL) ;
     PLUTO_beep() ;
     AV_assign_fval( value_av ,  value_float2 ) ;
     value_int   = value_av->ival ;
     value_float = value_av->fval ;
     DRAW_set_value_label();  /* reset value and label field to previous entry */
     return(1);
   }  

   return(0);
}


/*---------------------------------------------------------------------
  Callbacks and functions for value label and text field [15 Oct 2003]
-----------------------------------------------------------------------*/

static void dump_vallab(void)
{
#if 0
   char *str = Dtable_to_nimlstring( vl_dtable , "VALUE_LABEL_DTABLE" ) ;
   if( str != NULL ){ printf("%s\n",str); free(str); }
   return ;
#endif
}

/*---------------------------------------------------------------------*/

char * DRAW_value_string( float val )  /* returns a fixed pointer! */
{
   static char str[32] ;
   sprintf(str,"%.5g",val) ;
   return str ;
}

/*---------------------------------------------------------------------*/

void DRAW_set_value_label(void)
{
   if( vl_dtable == NULL || value_float == 0.0 ){
     XmTextFieldSetString( label_textf , "" ) ;
   } else {
     char *str_val = DRAW_value_string( value_float ) ;
     char *str_lab = findin_Dtable_a( str_val , vl_dtable ) ;
     XmTextFieldSetString( label_textf ,
                           (str_lab != NULL) ? str_lab : "" ) ;
   }
   return ;
}

/*---------------------------------------------------------------------*/

void DRAW_label_CB( Widget wtex , XtPointer cld, XtPointer cad )
{
   char *str_val , *str_lab , *str_old ;
   int ll , ii ;

   /* get string from text field, see if it is empty or ends in blanks */

   str_lab = XmTextFieldGetString( label_textf ) ;
   if( str_lab == NULL ){
     if( vl_dtable == NULL ) return ;  /* do nothing */
   } else {
     ll = strlen(str_lab) ;
     for( ii=ll-1 ; ii >= 0 && isspace(str_lab[ii]) ; ii-- ) ; /* nada */
     if( ii < 0 ){                       /*-- all blanks */
       if( vl_dtable == NULL ) return ;    /* do nothing */
       free(str_lab) ; str_lab = NULL ;    /* otherwise, clobber entry */
     } else if( ii < ll-1 ){             /*-- ends in blanks */
       str_lab[ii+1] = '\0' ;              /* so truncate them */
     }
   }

   /* create (value,label) pair Dtable -- NULL label ==> erase old label */

   if( vl_dtable == NULL ) vl_dtable = new_Dtable(7) ;

   str_val = DRAW_value_string( value_float ) ;   /* value string */

   /* check if old label for this value is same as new label;
      if it is, then don't need to do anything                */

   str_old = findin_Dtable_a( str_val , vl_dtable ) ;
   if( str_old != NULL ){
     if( str_lab != NULL && strcmp(str_old,str_lab) == 0 ){  /* same as old */
       free(str_lab) ; return ;
     } else if( str_lab == NULL ){                     /* erase the old one */
       removefrom_Dtable_a( str_val , vl_dtable ) ;
       dump_vallab() ;
       return ;
     }
   }
   if( str_lab == NULL ) return ;  /* is NULL ==> nothing to do here */

   /* check if new label is already in the table under a different value */

   str_old = findin_Dtable_b( str_lab , vl_dtable ) ;
   if( str_old != NULL && strcmp(str_old,str_val) != 0 ){
     char msg[1024] ;
     sprintf(msg," \n"
                 " *********************************** \n"
                 " ** ERROR * ERROR * ERROR * ERROR ** \n"
                 " **\n"
                 " ** Label = %s\n"
                 " **   is already associated with\n"
                 " ** Value = %s\n"
                 " **\n"
                 " ** Value,Label pairs must be unique \n"
                 " *********************************** \n"
             , str_lab , str_old ) ;  
     /* changed popup to disappear with timer to make it easier to continue */
     (void) MCW_popup_message( label_textf , msg , MCW_USER_KILL | MCW_TIMER_KILL) ;
     PLUTO_beep() ;
     DRAW_set_value_label();
     free(str_lab) ; return ;
   }

   /* add new value,label pair to Dtable (will clobber old one, if present) */

   addto_Dtable( str_val , str_lab , vl_dtable ) ;
   free(str_lab) ;

   dump_vallab() ;
   return ;
}

/*--------------------------------------------------------------------------*/

static char **vl_strlist=NULL ;
static  int  vl_nstrlist=0 ;

static void DRAW_label_finalize( Widget w, XtPointer cd, MCW_choose_cbs *cbs )
{
   int ival = cbs->ival , nn ;
   float val=0.0 ;

   if( !editor_open ){ PLUTO_beep(); POPDOWN_strlist_chooser; return; }

   nn = sscanf( vl_strlist[ival] , "%f" , &val ) ;
   if( nn == 0 || val == 0.0 ) return ;

   AV_assign_fval( value_av ,  val ) ;
   value_int   = value_av->ival ;
   value_float = value_av->fval ;
   DRAW_set_value_label() ;
   return ;
}

/*---------------------------------------------------------------------*/

static void DRAW_label_getfile( Widget w, XtPointer cd, MCW_choose_cbs *cbs )
{
   char *str ;

   if( !editor_open ){ PLUTO_beep(); POPDOWN_string_chooser; return; }

   str = AFNI_suck_file( cbs->cval ) ;
   if( str != NULL ){
     if( vl_dtable != NULL ) destroy_Dtable(vl_dtable) ;
     vl_dtable = Dtable_from_nimlstring(str) ;
     DRAW_set_value_label() ;
   } else {
     PLUTO_beep() ;
   }
   return ;
}

/*---------------------------------------------------------------------*/

void DRAW_label_EV( Widget w , XtPointer cld ,
                    XEvent *ev , Boolean *continue_to_dispatch )
{

   /* handle leave event in text field */

   if( w == label_textf ){
     XmAnyCallbackStruct cbs ;
     XLeaveWindowEvent *lev = (XLeaveWindowEvent *) ev ;
     if( lev->type != LeaveNotify ) return ;
     cbs.reason = XmCR_ACTIVATE ;  /* simulate a return press */
     DRAW_label_CB( w , NULL , &cbs ) ;
   }

   /* handle Button-3 press in label */

   else if( w == label_label ){
     XButtonEvent *bev = (XButtonEvent *) ev ;
     int nn,ic,ll ; char **la, **lb ; float val ;

     if( bev->button == Button1 ){
       MCW_choose_string( w , "Enter Value-Label filename:" ,
                          NULL , DRAW_label_getfile , NULL   ) ;
       return ;
     }
     if( bev->button != Button3 ) return ;
     nn = listize_Dtable( vl_dtable , &la , &lb ) ;
     if( nn <= 0 || la == NULL || lb == NULL ) return ;

     /** get ready to popup a new list chooser **/

     POPDOWN_strlist_chooser ;

     /** clear old strings **/

     for( ic=0 ; ic < vl_nstrlist ; ic++ ) free(vl_strlist[ic]) ;

     /** make a list of value-label strings **/

     vl_nstrlist = nn ;
     vl_strlist  = (char **) realloc( vl_strlist , sizeof(char *)*vl_nstrlist ) ;
     for( nn=ic=0 ; ic < vl_nstrlist ; ic++ ){
       if( la[ic] != NULL && lb[ic] != NULL ){  /* should always be true */
         ll = strlen(la[ic])+strlen(lb[ic])+8 ;
         vl_strlist[nn] = calloc(1,ll) ;
         sprintf( vl_strlist[nn] , "%s = %s" , la[ic],lb[ic] ) ;
         nn++ ;
       }
     }
     free(la); free(lb); if( nn == 0 ) return ;

     /* sort list for the user's convenience */

     if( nn > 1 ){
       int redo ; char *t ;
      BSort:
       for( redo=ic=0 ; ic < nn-1 ; ic++ ){
         if( strcmp(vl_strlist[ic],vl_strlist[ic+1]) > 0 ){
           t=vl_strlist[ic]; vl_strlist[ic]=vl_strlist[ic+1]; vl_strlist[ic+1]=t;
           redo = 1 ;
         }
       }
       if( redo ) goto BSort ;
     }

     /* find current value in list, if any */

     for( ic=0 ; ic < nn ; ic++ ){
       sscanf( vl_strlist[ic] , "%f" , &val ) ;
       if( val == value_float ) break ;
     }
     if( ic == nn ) ic = -1 ;

     /* let the user choose one */

     MCW_choose_strlist( w , "Value = Label" , nn ,
                         ic , vl_strlist , DRAW_label_finalize , NULL ) ;
   }

   return ;
}

/*---------------------------------------------------------------------*/

void DRAW_attach_dtable( Dtable *dt, char *atname, THD_3dim_dataset *ds )
{
   char *str ;
   if( dt == NULL || atname == NULL || ds == NULL ) return ;
   str = Dtable_to_nimlstring( dt , atname ) ;
   if( str == NULL ) return ;
   THD_set_string_atr( ds->dblk , atname , str ) ;
   free(str) ; return ;
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

         /*-- 20 Feb 2003: undo via keypress --*/

         if( mode == UNDO_MODE ){
           if( undo_num > 0 ) DRAW_undo_CB( undo_pb,NULL,NULL ) ;
           else               XBell(dc->display,100) ;
           return ;
         }

         if( mode == INCVAL_MODE || mode == DECVAL_MODE ){ /* 13 Sep 2008 */
           float nval ;
           nval = value_float + ((mode==INCVAL_MODE) ? (1.0f) : (-1.0f)) ;
           AV_assign_fval( value_av , nval ) ;
           value_int   = value_av->ival ;
           value_float = value_av->fval ;
           DRAW_set_value_label() ;
           return ;
         }

         /*-- Did we get points? --*/

         if( np <= 0 ) return ;

         plane = mode - SINGLE_MODE ;
         if( plane < 1 || plane > 3 ) plane = mode - PLANAR_MODE ;
         if( plane < 1 || plane > 3 ) plane = 0 ;

         /* anything but flood mode --> just draw given points */

         if( plane == 0 ||
             ((mode_ival != MODE_FLOOD_VAL )  &&
              (mode_ival != MODE_FLOOD_NZ  )  &&
              (mode_ival != MODE_FLOOD_ZERO)  &&
              (mode_ival != MODE_ZERO_VAL  )  &&
              (mode_ival != MODE_FLOOD_VZ  )  &&
              (mode_ival != MODE_FILLED    ))   ){

            /* 07 Oct 2002: expand set of points using a mask? */

            if( plane != 0 && mode_ival >= FIRST_2D_MODE && mode_ival <= LAST_2D_MODE ){
              int nfill=0, *xyzf=NULL ;

              DRAW_2D_expand( np , xd,yd,zd , plane , &nfill , &xyzf ) ;
              if( nfill > 0 && xyzf != NULL ){
                DRAW_into_dataset( nfill , xyzf,NULL,NULL , NULL ) ;
                free(xyzf) ;
              }

            } else if( plane != 0 && mode_ival >= FIRST_3D_MODE && mode_ival <= LAST_3D_MODE ){
              int nfill=0, *xyzf=NULL ;

              DRAW_3D_expand( np , xd,yd,zd , plane , &nfill , &xyzf ) ;
              if( nfill > 0 && xyzf != NULL ){
                DRAW_into_dataset( nfill , xyzf,NULL,NULL , NULL ) ;
                free(xyzf) ;
              }

            /* 16 Oct 2002: expand geometrically (circle or sphere)? */

            } else if( plane != 0 && mode_ival >= FIRST_RAD_MODE && mode_ival <= LAST_RAD_MODE ){
              int nfill=0, *xyzf=NULL ;

              switch( mode_ival ){
                case MODE_2D_CIRC:
                  DRAW_2D_circle( np , xd,yd,zd , plane , &nfill , &xyzf ) ;
                break ;
                case MODE_3D_SPHR:
                  DRAW_3D_sphere( np , xd,yd,zd , plane , &nfill , &xyzf ) ;
                break ;
              }

              if( nfill > 0 && xyzf != NULL ){
                DRAW_into_dataset( nfill , xyzf,NULL,NULL , NULL ) ;
                free(xyzf) ;
              } else {
                DRAW_into_dataset( np , xd,yd,zd , NULL ) ; /* should never happen */
              }

            } else {                                        /* the old way:     */
              DRAW_into_dataset( np , xd,yd,zd , NULL ) ;   /* just draw points */
            }

         } else {

            /* flood mode! */

            int   ityp = DSET_BRICK_TYPE(dset,0) ;
            float bfac = DSET_BRICK_FACTOR(dset,0) ;
            int nx=DSET_NX(dset) , ny=DSET_NY(dset) , nz=DSET_NZ(dset) ,
                nxy = nx*ny , nxyz = nxy*nz , ii,jj , ixyz ;
            int base , di,dj , itop,jtop,nij , xx=xd[0],yy=yd[0],zz=zd[0] , ix,jy ;
            byte * pl ;
            int nfill , *xyzf , nf ;

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
            pl  = (byte *) calloc( nij , sizeof(byte) ) ;

            if( mode_ival != MODE_FILLED ){  /* old code: flood to a dataset value */

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
                    } else if( mode_ival == MODE_FLOOD_VZ ){  /* 30 Apr 2002 */
                       for( jj=0 ; jj < jtop ; jj++ )
                          for( ii=0 ; ii < itop ; ii++ ){
                             ixyz = base + ii*di + jj*dj ;
                             if( bp[ixyz] == val || bp[ixyz] == 0 ) pl[ii+jj*itop] = 1 ;
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
                    } else if( mode_ival == MODE_FLOOD_VZ ){  /* 30 Apr 2002 */
                       for( jj=0 ; jj < jtop ; jj++ )
                          for( ii=0 ; ii < itop ; ii++ ){
                             ixyz = base + ii*di + jj*dj ;
                             if( bp[ixyz] == val || bp[ixyz] == 0 ) pl[ii+jj*itop] = 1 ;
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
                    } else if( mode_ival == MODE_FLOOD_VZ ){  /* 30 Apr 2002 */
                       for( jj=0 ; jj < jtop ; jj++ )
                          for( ii=0 ; ii < itop ; ii++ ){
                             ixyz = base + ii*di + jj*dj ;
                             if( bp[ixyz] == val || bp[ixyz] == 0 ) pl[ii+jj*itop] = 1 ;
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

            } /*-- end of flood code --*/

            else {  /*-- 25 Sep 2001: fill the interior of the drawn curve --*/

              int *iip , *jjp ;

              switch(plane){                           /* select which   */
                case 1: iip = yd ; jjp = zd ; break ;  /* arrays to draw */
                case 2: iip = xd ; jjp = zd ; break ;  /* curve from     */
                case 3: iip = xd ; jjp = yd ; break ;
              }

              for( ii=0 ; ii < np ; ii++ ){  /* draw curve into fill array */
                 pl[ iip[ii] + jjp[ii]*itop ] = 1 ;
              }

              /* now find an edge point that is not on the curve */

              ix = -1 ;
              for( ii=0 ; ii < itop ; ii++ ){
                 if( pl[ii]               == 0 ){ ix = ii; jy = 0     ; break; }
                 if( pl[ii+(jtop-1)*itop] == 0 ){ ix = ii; jy = jtop-1; break; }
              }
              if( ix < 0 ){
                for( jj=0 ; jj < jtop ; jj++ ){
                  if( pl[jj*itop]          == 0 ){ ix = 0     ; jy = jj; break; }
                  if( pl[(itop-1)+jj*itop] == 0 ){ ix = itop-1; jy = jj; break; }
                }
              }
              if( ix < 0 ){ /* should never happen */
                 free(pl) ; XBell(dc->display,100) ; return ;
              }

              /* fill the array from the edge */

              DRAW_2dfiller( itop,jtop , ix,jy , pl ) ;

              /* all filled points are 2 --> these are NOT the locations to draw */

              nfill = 0 ;
              for( ii=0 ; ii < nij ; ii++ ) nfill += (pl[ii] != 2) ;
              if( nfill == 0 ){ free(pl) ; XBell(dc->display,100) ; return ; }

              xyzf = (int *) malloc( sizeof(int) * nfill ) ;

              for( nf=0,jj=0 ; jj < jtop ; jj++ ){
                 for( ii=0 ; ii < itop ; ii++ ){
                    if( pl[ii+jj*itop] != 2 )
                       xyzf[nf++] = base + ii*di + jj*dj ;
                 }
              }

              free(pl) ;

              DRAW_into_dataset( nfill , xyzf,NULL,NULL , NULL ) ;

              free(xyzf) ;

            } /* end of interior fill code */

         } /* end of flooding or filling */

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
               XBell(dc->display,100) ;        /* feeble protest */
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

               XBell(dc->display,100) ;
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

int DRAW_into_dataset( int np , int *xd , int *yd , int *zd , void *var )
{
   int   ityp = DSET_BRICK_TYPE(dset,0) ;
   float bfac = DSET_BRICK_FACTOR(dset,0) ;
   int nx=DSET_NX(dset) , ny=DSET_NY(dset) , nz=DSET_NZ(dset) ,
       nxy = nx*ny , nxyz = nxy*nz , ii , ixyz ;
   int ndrawn=0 ;
   dobuf *sb ;  /* 19 Nov 2003: save buffer */
   int *xyz ;

   /* sanity check */

   if( dset==NULL || np <= 0 || xd==NULL ) return 0 ;

   /* make space for undo/redo (save old state in buffer) [19 Nov 2003] */

   CREATE_DOBUF(sb,np,ityp) ;
   xyz = sb->xyz ;             /* list of indexes to be altered */

   /* compute (or copy) data index into save buffer */

   if( yd == NULL ){                       /* direct supply of 1-index */
     memcpy(xyz,xd,sizeof(int)*np) ;
   } else {                                /* collapse 3-index into 1 */
     for( ii=0 ; ii < np ; ii++ )
       xyz[ii] = xd[ii] + yd[ii] * nx + zd[ii] * nxy ;
   }

   /* copy data into save buffer, based on type */

   if( bfac == 0.0 ) bfac = 1.0 ;

   switch( ityp ){

      default: fprintf(stderr,"Illegal brick type=%s in AFNI Editor!\n",
                       MRI_TYPE_name[ityp] ) ;
      break ;

#define DOIT (infill_mode==0 || bp[ixyz]==0)

      case MRI_short:{
        short * bp  = (short *) DSET_BRICK_ARRAY(dset,0) ;
        short * up  = (short *) sb->buf ;
        short * vvv = (short *) var ;
        short   val = (short)   (value_float/bfac) ;

        for( ii=0 ; ii < np ; ii++ ){  /* save into buffer */
          ixyz = xyz[ii] ;
          up[ii] = (ixyz >= 0 && ixyz < nxyz) ? bp[ixyz] : 0 ;
        }
        for( ii=0 ; ii < np ; ii++ ){  /* put into dataset */
          ixyz = xyz[ii] ;
          if( ixyz >= 0 && ixyz < nxyz && DOIT ){
            bp[ixyz] = (vvv==NULL) ? val : vvv[ii] ; ndrawn++ ;
          }
        }
      }
      break ;

      case MRI_byte:{
        byte * bp  = (byte *) DSET_BRICK_ARRAY(dset,0) ;
        byte * up  = (byte *) sb->buf ;
        byte * vvv = (byte *) var ;
        byte   val = (byte)   (value_float/bfac) ;

        for( ii=0 ; ii < np ; ii++ ){
          ixyz = xyz[ii] ;
          up[ii] = (ixyz >= 0 && ixyz < nxyz) ? bp[ixyz] : 0 ;
        }
        for( ii=0 ; ii < np ; ii++ ){
          ixyz = xyz[ii] ;
          if( ixyz >= 0 && ixyz < nxyz && DOIT ){
            bp[ixyz] = (vvv==NULL) ? val : vvv[ii] ; ndrawn++ ;
          }
        }
      }
      break ;

      case MRI_float:{
        float * bp  = (float *) DSET_BRICK_ARRAY(dset,0) ;
        float * up  = (float *) sb->buf ;
        float * vvv = (float *) var ;
        float   val = (value_float/bfac) ;

        for( ii=0 ; ii < np ; ii++ ){
          ixyz = xyz[ii] ;
          up[ii] = (ixyz >= 0 && ixyz < nxyz) ? bp[ixyz] : 0.0 ;
        }
        for( ii=0 ; ii < np ; ii++ ){
          ixyz = xyz[ii] ;
          if( ixyz >= 0 && ixyz < nxyz && DOIT ){
            bp[ixyz] = (vvv==NULL) ? val : vvv[ii] ; ndrawn++ ;
          }
        }
      }
      break ;

      case MRI_complex:{
        complex * bp  = (complex *) DSET_BRICK_ARRAY(dset,0) ;
        complex * up  = (complex *) sb->buf ;
        complex * vvv = (complex *) var ;
        complex   val ;
        static complex cxzero = { 0.0 , 0.0 } ;

        val = CMPLX( (value_float/bfac) , 0.0 ) ;

        for( ii=0 ; ii < np ; ii++ ){
          ixyz = xyz[ii] ;
          up[ii] = (ixyz >= 0 && ixyz < nxyz) ? bp[ixyz] : cxzero ;
        }
        for( ii=0 ; ii < np ; ii++ ){
          ixyz = xyz[ii] ;
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
   dset_changed = 1 ;
   SENSITIZE(save_pb,1) ; SENSITIZE(saveas_pb,1) ;
   SENSITIZE(choose_pb,0) ;
   Sensitize_copy_bbox(0); 

   /* save buffer pushed onto appropriate stack */

   if( undo_how == 1 ){   /* save on redo stack */
     redo_stack = realloc( (void *)redo_stack, sizeof(dobuf *)*(redo_num+1) );
     redo_stack[redo_num++] = sb ;
     REDO_button_labelize ;
   } else {               /* save on undo stack */
     undo_stack = realloc( (void *)undo_stack, sizeof(dobuf *)*(undo_num+1) );
     undo_stack[undo_num++] = sb ;
     UNDO_button_labelize ;
     DRAW_undo_sizecheck() ;
     if( undo_how == 0 ){  /* normal draw ==> can't redo */
       CLEAR_REDOBUF ;
     }
   }

   return ndrawn ;
}

/*---------------------------------------------------------------------------*/
/*!  Limit size of data allowed in undo buffers. [19 Nov 2003]
-----------------------------------------------------------------------------*/

static void DRAW_undo_sizecheck(void)
{
  int ii,jj , ss , lim=6 ;
  char *eee ;

  if( undo_num <= 1 ) return ;  /* will always keep 1 level of undo */

  /* get the limit of allowed mem usage for the undo buffers */

  eee = getenv("AFNI_DRAW_UNDOSIZE") ;
  if( eee != NULL ){
    ii = 0 ; sscanf(eee,"%d",&ii) ;
    if( ii > 0 ) lim = ii ; if( lim > 1024 ) lim = 1024 ;
  }
  lim *= (1024*1024) ;  /* megabytes */

  /* scan from top of stack,
     stopping when total size goes over the limit */

  for( ss=0,ii=undo_num-1 ; ii >= 0 && ss < lim ; ii-- )
    ss += SIZEOF_DOBUF( undo_stack[ii] ) ;

  if( ii <= 0 ) return ;  /* didn't go over limit before bottom */

  /* if here, stack elements from 0..ii-1 should be removed
              and the elements above them moved down to fill in */

  for( jj=0 ; jj < ii ; jj++ )         /* removal */
    DESTROY_DOBUF( undo_stack[jj] ) ;

  for( jj=ii ; jj < undo_num ; jj++ )  /* move-al */
    undo_stack[jj-ii] = undo_stack[jj] ;

  undo_num = undo_num - ii ;
  return ;
}

/*---------------------------------------------------------------------------*/
/*! Set label of Undo or Redo button to reflect number of levels
    available, and set sensitivity while we are at it.  [19 Nov 2003]
-----------------------------------------------------------------------------*/

static void DRAW_undo_butlab( Widget w , int n )
{
   XmString xstr ;
   char label[32] ;
   int nfmt ;
   static char *fmt[3] = { "%s[%d]" , "%s:%d" , "%s%03d" } ;

   if( w == (Widget)NULL ) return ;  /* oom-possible? */

        if( n <  10  ) nfmt = 0 ;     /* choose format based */
   else if( n < 100  ) nfmt = 1 ;     /* on number of digits */
   else                nfmt = 2 ;

   sprintf( label, fmt[nfmt], (w==undo_pb) ? "Undo" : "Redo" , n%1000 ) ;

   xstr = XmStringCreateLtoR( label , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( w , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   SENSITIZE( w , (n>0) ) ;
   return ;
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
   MRI_IMAGE *bim , *tbim ; /* 21 Nov 2003: to allow undo of fillin */

   /* check for errors */

   if( !editor_open || dset == NULL ){ XBell(dc->display,100); return; }

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

   bim  = DSET_BRICK(dset,0) ;  /* 21 Nov 2003: for undo */
   tbim = mri_copy( bim ) ;     /* copy brick before the change */

   nftot = THD_dataset_rowfillin( dset , 0 , dcode , maxgap ) ;
   if( nftot > 0 ){
     fprintf(stderr,"++ Fillin filled %d voxels\n",nftot) ;
     PLUTO_dset_redisplay( dset ) ;
     dset_changed = 1 ;
     SENSITIZE(save_pb,1) ; SENSITIZE(saveas_pb,1) ;
     if( recv_open ) AFNI_process_drawnotice( im3d ) ;
    
     { void *bar , *tbar ;     /* 21 Nov 2003: compute the undo stuff */
       int ityp=bim->kind, ii,jj, nvox=bim->nvox, ndel=0 ;
       dobuf *sb=NULL ;
       switch( ityp ){
         case MRI_short:{
           short *bar = MRI_SHORT_PTR(bim), *tbar = MRI_SHORT_PTR(tbim), *up ;
           for( ii=0 ; ii < nvox ; ii++ ) if( bar[ii] != tbar[ii] ) ndel++ ;
           if( ndel > 0 ){
             CREATE_DOBUF(sb,ndel,MRI_short) ; up = (short *)sb->buf ;
             for( ii=jj=0 ; ii < nvox ; ii++ )
               if( bar[ii] != tbar[ii] ){ sb->xyz[jj]=ii; up[jj++]=tbar[ii]; }
           }
         }
         break ;
         case MRI_float:{
           float *bar = MRI_FLOAT_PTR(bim), *tbar = MRI_FLOAT_PTR(tbim), *up ;
           for( ii=0 ; ii < nvox ; ii++ ) if( bar[ii] != tbar[ii] ) ndel++ ;
           if( ndel > 0 ){
             CREATE_DOBUF(sb,ndel,MRI_float) ; up = (float *)sb->buf ;
             for( ii=jj=0 ; ii < nvox ; ii++ )
               if( bar[ii] != tbar[ii] ){ sb->xyz[jj]=ii; up[jj++]=tbar[ii]; }
           }
         }
         break ;
         case MRI_byte:{
           byte *bar = MRI_BYTE_PTR(bim), *tbar = MRI_BYTE_PTR(tbim), *up ;
           for( ii=0 ; ii < nvox ; ii++ ) if( bar[ii] != tbar[ii] ) ndel++ ;
           if( ndel > 0 ){
             CREATE_DOBUF(sb,ndel,MRI_byte) ; up = (byte *)sb->buf ;
             for( ii=jj=0 ; ii < nvox ; ii++ )
               if( bar[ii] != tbar[ii] ){ sb->xyz[jj]=ii; up[jj++]=tbar[ii]; }
           }
         }
         break ;
       } /* end of switch on brick type */

       if( sb != NULL ){  /* if we created an undo buffer, push onto stack */
         undo_stack = realloc( (void *)undo_stack, sizeof(dobuf *)*(undo_num+1) );
         undo_stack[undo_num++] = sb ;
         UNDO_button_labelize ;
         DRAW_undo_sizecheck() ;
         CLEAR_REDOBUF ;         /* can't redo after a drawing */
       }
     } /* 21 Nov 2003: end of allowing for undo stuff */

   } else if( nftot < 0 ) {
      fprintf(stderr,"** Fillin failed for some reason!\n") ;
      XBell(dc->display,100) ;
   } else {
      fprintf(stderr,"++ No Fillin voxels found\n") ;
   }

   mri_free(tbim) ; /* 21 Nov 2003: toss old copy */
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

     infill_mode = (strcmp(XtName(w),TTATLAS_infill_label) == 0) ;
     ff = DRAW_into_dataset( nftot , xd,NULL,NULL , NULL ) ;
     infill_mode = 0 ;

     free(xd) ;

     fprintf(stderr,"++ %d TT Atlas voxels drawn into dataset\n",ff) ;
     PLUTO_dset_redisplay( dset ) ;
     dset_changed = 1 ;
     SENSITIZE(save_pb,1) ; SENSITIZE(saveas_pb,1) ;
     if( recv_open ) AFNI_process_drawnotice( im3d ) ;
   } else {
      fprintf(stderr,"++ No TT Atlas voxels found for some reason!?\a\n") ;
   }

   free(voxout) ; /* toss trash */
   return ;
}

/*-----------------------------------------------------------------------
  Copy a dataset; new prefix is "COPY_" + old prefix.
    zfill != 0  ==> zero fill
    ftyp  <= 0  ==> same func type
    ftyp  == 1  ==> fim
    ftyp  == 2  ==> anat (omri)
    dtype <  0  ==> same datum
    dtype >= 0  ==> new datum (only valid for zfill)
                          Adapted from plug_copy.c -- 24 Sep 2001 - RWCox
-------------------------------------------------------------------------*/

THD_3dim_dataset * DRAW_copy_dset( THD_3dim_dataset *dset ,
                                   int zfill , int ftyp , int dtype )
{
   THD_3dim_dataset *new_dset ;
   char new_prefix[THD_MAX_PREFIX] ;
   int ival ;

   if( !ISVALID_DSET(dset) ) return NULL ;

   if( strstr(DSET_PREFIX(dset),"COPY") != NULL ) strcpy(new_prefix,"C") ;
   else                                           strcpy(new_prefix,"COPY_") ;
   ival = strlen(new_prefix) ;
   MCW_strncpy(new_prefix+ival,DSET_PREFIX(dset),THD_MAX_PREFIX-ival) ;

   /*-- make a new dataset, somehow --*/

   if( zfill == 0 ){
     new_dset = PLUTO_copy_dset( dset , new_prefix ) ;  /* full copy */
     dtype = -1 ;
   } else {
     new_dset = EDIT_empty_copy( dset ) ;               /* zero fill */
     EDIT_dset_items( new_dset, ADN_prefix,new_prefix, ADN_none ) ;
   }

   if( new_dset == NULL ) return NULL ; /* bad, real bad */

   tross_Copy_History( dset , new_dset ) ;  /* make some History, dude! */
   { char str[256] ;
     strcpy(str,"Drawing plugin COPY:") ;
     if( zfill ) strcat(str," Fill->Zero") ;
     else        strcat(str," Fill->Data") ;
          if( ftyp == 1 ) strcat(str," Type->Func") ;
     else if( ftyp == 2 ) strcat(str," Type->Anat") ;
     if( dtype >= 0 ){
       strcat(str," Datum->") ; strcat(str,MRI_TYPE_name[dtype]) ;
     }
     tross_Append_History( new_dset , str ) ;
   }

   /*--- modify new dataset, if desired ---*/

   if( ftyp == 1 )
      EDIT_dset_items( new_dset ,
                         ADN_type      , HEAD_FUNC_TYPE ,
                         ADN_func_type , FUNC_FIM_TYPE  ,
                       ADN_none ) ;
   else if( ftyp == 2 )
      EDIT_dset_items( new_dset ,
                         ADN_type      , HEAD_ANAT_TYPE ,
                         ADN_func_type , ANAT_OMRI_TYPE ,
                       ADN_none ) ;

   if( zfill == 0 ) return new_dset ;  /* done if not zero-filling */

   /*--- change type of data stored? ---*/

   if( dtype >= 0 ) EDIT_dset_items( new_dset ,
                                       ADN_datum_all, dtype,
                                     ADN_none ) ;
   /* zero fill */

   {  int ityp , nbytes , nvals , ival ;
      void * new_brick , * bp ;

      nvals = DSET_NVALS(new_dset) ;

      for( ival=0 ; ival < nvals ; ival++)             /* get memory for bricks */
      {                                                /* and zero fill */
         ityp      = DSET_BRICK_TYPE(new_dset,ival) ;
         nbytes    = DSET_BRICK_BYTES(new_dset,ival) ; /* how much data */
         new_brick = malloc( nbytes ) ;
         EDIT_substitute_brick( new_dset , ival , ityp , new_brick ) ;

         bp = DSET_BRICK_ARRAY(new_dset,ival) ;        /* brick pointer */
         EDIT_BRICK_FACTOR(new_dset,ival,0.0) ;        /* brick factor  */
         memset( bp , 0 , nbytes ) ;
      }
   }

   /* 20 Oct 2003: copy VALUE_LABEL_DTABLE attribute, if present */

   { ATR_string *atr ;
     atr = THD_find_string_atr( dset->dblk , "VALUE_LABEL_DTABLE" ) ;
     if( atr != NULL )
       THD_set_char_atr( new_dset->dblk , "VALUE_LABEL_DTABLE" ,
                         atr->nch , atr->ch                     ) ;
   }

   /*-- done successfully!!! --*/

   return new_dset ;
}

/*-----------------------------------------------------------------------------*/
/*! Expand set of points in 2D plane.  RWCox - 07 Oct 2002.
-------------------------------------------------------------------------------*/

static void DRAW_2D_expand( int np, int *xd, int *yd, int *zd, int plane ,
                            int *nfill , int **xyzf )
{
   int base , di,dj , itop,jtop,nij , xx,yy,zz , ix,jy , *ip,*jp ;
   int nx=DSET_NX(dset) , ny=DSET_NY(dset) , nz=DSET_NZ(dset) , nxy = nx*ny ;
   int kadd , ii,jj,kk , ixn,jyn , mm,qq ;
   int nnew , *xyzn ;

   static int nadd[5] = { 4 , 8 , 12 , 20 , 24 } ;
   static int nn[24][2] = { {-1, 0} , { 1, 0} , { 0, 1} , { 0,-1} ,
                            {-1,-1} , {-1, 1} , { 1,-1} , { 1, 1} ,
                            {-2, 0} , { 2, 0} , { 0, 2} , { 0,-2} ,
                            {-2, 1} , {-2,-1} , {-1, 2} , {-1,-2} ,
                            { 2, 1} , { 2,-1} , { 1, 2} , { 1,-2} ,
                            {-2,-2} , {-2, 2} , { 2,-2} , { 2, 2}  } ;

   /* check inputs */

   if( np <= 0 || xd == NULL || yd == NULL || zd == NULL )     return ;
   if( mode_ival < FIRST_2D_MODE && mode_ival > LAST_2D_MODE ) return ;
   if( nfill == NULL || xyzf == NULL )                         return ;

   /* compute stuff for which plane we are in:
       1 -> yz , 2 -> xz , 3 -> xy            */

   xx = xd[0] ; yy = yd[0] ; zz = zd[0] ;
   switch(plane){
     case 1: base=xx    ; di=nx; dj=nxy; itop=ny; jtop=nz; ip=yd; jp=zd; break;
     case 2: base=yy*nx ; di=1 ; dj=nxy; itop=nx; jtop=nz; ip=xd; jp=zd; break;
     case 3: base=zz*nxy; di=1 ; dj=nx ; itop=nx; jtop=ny; ip=xd; jp=yd; break;
     default: return ;  /* bad input */
   }

   kadd = nadd[mode_ival-FIRST_2D_MODE] ;  /* how many pts around each input pt */

   xyzn = (int *) malloc( sizeof(int)*np*(kadd+1) ) ;   /* output array */

   /** add points around each input point, culling duplicates **/

   for( ii=jj=0 ; ii < np ; ii++ ){
     ix = ip[ii] ; jy = jp[ii] ;                                /* drawn point 2D index */
     if( ix >= 0 && ix < itop && jy >= 0 && jy < jtop ){
       xyzn[jj++] = base + ix*di + jy*dj ;                      /* load 3D index */
       for( kk=0 ; kk < kadd ; kk++ ){
         ixn = ix+nn[kk][0] ; jyn = jy+nn[kk][1] ;              /* nbhd pt 2D index */
         if( ixn >= 0 && ixn < itop && jyn >= 0 && jyn < jtop ){
           mm = base + ixn*di + jyn*dj ;                        /* 3D index */
           if( ii > 0 )
             for( qq=0 ; qq < jj && xyzn[qq] != mm ; qq++ ) ;   /* nada */
           else
             qq = jj ;
           if( qq == jj ) xyzn[jj++] = mm ;                     /* save 3D index */
         }
       }
     }
   }

   *nfill = jj ; *xyzf  = xyzn ; return ;
}

/*-----------------------------------------------------------------------------*/
/*! Expand set of points in 3D space.  RWCox - 07 Oct 2002.
-------------------------------------------------------------------------------*/

static void DRAW_3D_expand( int np, int *xd, int *yd, int *zd, int plane ,
                            int *nfill , int **xyzf )
{
   int ix,jy,kz ;
   int nx=DSET_NX(dset) , ny=DSET_NY(dset) , nz=DSET_NZ(dset) , nxy = nx*ny ;
   int kadd , ii,jj,kk , ixn,jyn,kzn , mm,qq ;
   int nnew , *xyzn ;

   static int nadd[7] = { 6 , 18 , 26 , 32 , 56 , 80 , 124 } ;

   static int nn[124][3] ={ {-1, 0, 0} , { 1, 0, 0} ,  /* r**2 = 1 */
                            { 0,-1, 0} , { 0, 1, 0} ,
                            { 0, 0,-1} , { 0, 0, 1} ,

                            {-1,-1, 0} , {-1, 1, 0} ,  /* r**2 = 2 */
                            { 1,-1, 0} , { 1, 1, 0} ,
                            { 0,-1,-1} , { 0,-1, 1} ,
                            { 0, 1,-1} , { 0, 1, 1} ,
                            {-1, 0,-1} , {-1, 0, 1} ,
                            { 1, 0,-1} , { 1, 0, 1} ,

                            {-1,-1,-1} , {-1,-1, 1} ,  /* r**2 = 3 */
                            {-1, 1,-1} , {-1, 1, 1} ,
                            { 1,-1,-1} , { 1,-1, 1} ,
                            { 1, 1,-1} , { 1, 1, 1} ,

                            {-2, 0, 0} , { 2, 0, 0} ,  /* r**2 = 4 */
                            { 0,-2, 0} , { 0, 2, 0} ,
                            { 0, 0,-2} , { 0, 0, 2} ,

                            {-2,-1, 0} , {-2, 1, 0} ,  /* r**2 = 5 */
                            { 2,-1, 0} , { 2, 1, 0} ,
                            { 0,-2,-1} , { 0,-2, 1} ,
                            { 0, 2,-1} , { 0, 2, 1} ,
                            {-2, 0,-1} , {-2, 0, 1} ,
                            { 2, 0,-1} , { 2, 0, 1} ,
                            {-1,-2, 0} , {-1, 2, 0} ,
                            { 1,-2, 0} , { 1, 2, 0} ,
                            { 0,-1,-2} , { 0,-1, 2} ,
                            { 0, 1,-2} , { 0, 1, 2} ,
                            {-1, 0,-2} , {-1, 0, 2} ,
                            { 1, 0,-2} , { 1, 0, 2} ,

                            {-2,-1,-1} , {-2,-1, 1} ,  /* r**2 = 6 */
                            {-2, 1,-1} , {-2, 1, 1} ,
                            { 2,-1,-1} , { 2,-1, 1} ,
                            { 2, 1,-1} , { 2, 1, 1} ,
                            {-1,-2,-1} , {-1,-2, 1} ,
                            {-1, 2,-1} , {-1, 2, 1} ,
                            { 1,-2,-1} , { 1,-2, 1} ,
                            { 1, 2,-1} , { 1, 2, 1} ,
                            {-1,-1,-2} , {-1,-1, 2} ,
                            {-1, 1,-2} , {-1, 1, 2} ,
                            { 1,-1,-2} , { 1,-1, 2} ,
                            { 1, 1,-2} , { 1, 1, 2} ,

                            {-2,-2, 0} , {-2, 2, 0} ,  /* r**2 = 8 */
                            { 2,-2, 0} , { 2, 2, 0} ,
                            { 0,-2,-2} , { 0,-2, 2} ,
                            { 0, 2,-2} , { 0, 2, 2} ,
                            {-2, 0,-2} , {-2, 0, 2} ,
                            { 2, 0,-2} , { 2, 0, 2} ,

                            {-2,-2, 1} , {-2, 2, 1} ,  /* r**2 = 9 */
                            { 2,-2, 1} , { 2, 2, 1} ,
                            { 1,-2,-2} , { 1,-2, 2} ,
                            { 1, 2,-2} , { 1, 2, 2} ,
                            {-2, 1,-2} , {-2, 1, 2} ,
                            { 2, 1,-2} , { 2, 1, 2} ,
                            {-2,-2,-1} , {-2, 2,-1} ,
                            { 2,-2,-1} , { 2, 2,-1} ,
                            {-1,-2,-2} , {-1,-2, 2} ,
                            {-1, 2,-2} , {-1, 2, 2} ,
                            {-2,-1,-2} , {-2,-1, 2} ,
                            { 2,-1,-2} , { 2,-1, 2} ,

                            {-2,-2,-2} , {-2,-2, 2} ,  /* r**2 = 12          */
                            {-2, 2,-2} , {-2, 2, 2} ,  /* [corners of 5x5x5] */
                            { 2,-2,-2} , { 2,-2, 2} ,
                            { 2, 2,-2} , { 2, 2, 2}
                          } ;

   /* check inputs */

   if( np <= 0 || xd == NULL || yd == NULL || zd == NULL )     return ;
   if( mode_ival < FIRST_3D_MODE && mode_ival > LAST_3D_MODE ) return ;
   if( nfill == NULL || xyzf == NULL )                         return ;

   kadd = nadd[mode_ival-FIRST_3D_MODE] ;  /* how many pts around each input pt */

   xyzn = (int *) malloc( sizeof(int)*np*(kadd+1) ) ;   /* output array */

   /** add points around each input point, culling duplicates **/

   for( ii=jj=0 ; ii < np ; ii++ ){
     ix = xd[ii] ; jy = yd[ii] ; kz = zd[ii] ;
     if( ix >= 0 && ix < nx && jy >= 0 && jy < ny && kz >= 0 && kz <= nz ){
       xyzn[jj++] = ix + jy*nx + kz*nxy ;                       /* load 3D index */
       for( kk=0 ; kk < kadd ; kk++ ){
         ixn = ix+nn[kk][0] ; jyn = jy+nn[kk][1] ; kzn = kz+nn[kk][2] ;
         if( ixn >= 0 && ixn < nx && jyn >= 0 && jyn < ny && kzn >= 0 && kzn < nz ){
           mm = ixn + jyn*nx + kzn*nxy ;                        /* 3D index */
           if( ii > 0 )
             for( qq=0 ; qq < jj && xyzn[qq] != mm ; qq++ ) ;   /* nada */
           else
             qq = jj ;
           if( qq == jj ) xyzn[jj++] = mm ;                     /* save 3D index */
         }
       }
     }
   }

   *nfill = jj ; *xyzf  = xyzn ; return ;
}

/*-----------------------------------------------------------------------------*/
/*! Expand set of points in 2D plane, in a circle.  RWCox - 16 Oct 2002.
-------------------------------------------------------------------------------*/

static void DRAW_2D_circle( int np, int *xd, int *yd, int *zd, int plane ,
                            int *nfill , int **xyzf )
{
   int base , di,dj , itop,jtop,nij , xx,yy,zz , ix,jy , *ip,*jp ;
   int nx=DSET_NX(dset) , ny=DSET_NY(dset) , nz=DSET_NZ(dset) , nxy = nx*ny ;
   int kadd , ii,jj,kk , ixn,jyn , mm,qq ;
   int nnew , *xyzn ;

   float dx = fabs(DSET_DX(dset)) ;
   float dy = fabs(DSET_DY(dset)) ;
   float dz = fabs(DSET_DZ(dset)) ;
   float rad= rad_av->fval ;
   float fdi,fdj , xq,yq,radq ;
   int idx , jdy , *nn ;

   /* check inputs */

   if( np <= 0 || xd == NULL || yd == NULL || zd == NULL ) return ;
   if( nfill == NULL || xyzf == NULL )                     return ;

   /* compute stuff for which plane we are in:
       1 -> yz , 2 -> xz , 3 -> xy            */

   xx = xd[0] ; yy = yd[0] ; zz = zd[0] ;
   switch(plane){
     case 1: base=xx    ; di=nx; dj=nxy; itop=ny; jtop=nz; ip=yd; jp=zd; fdi=dy; fdj=dz; break;
     case 2: base=yy*nx ; di=1 ; dj=nxy; itop=nx; jtop=nz; ip=xd; jp=zd; fdi=dx; fdj=dz; break;
     case 3: base=zz*nxy; di=1 ; dj=nx ; itop=nx; jtop=ny; ip=xd; jp=yd; fdi=dx; fdj=dy; break;
     default: return ;  /* bad input */
   }

   idx = rad / fdi ; jdy = rad / fdj ;
   if( idx < 1 && jdy < 1 ) return ;       /* circle smaller than in-plane voxel */

   /* make incremental mask */

   radq = 1.001*rad*rad ;
   nn   = (int *) malloc( sizeof(int)*(2*idx+1)*(2*jdy+1)*2 ) ;
   kadd = 0 ;
   for( jj=-jdy ; jj <= jdy ; jj++ ){
     yq = (jj*fdj)*(jj*fdj) ;
     for( ii=-idx ; ii <= idx ; ii++ ){
       xq = (ii*fdi)*(ii*fdi) + yq ;
       if( xq <= radq && xq > 0.0 ){
         nn[2*kadd]   = ii ;
         nn[2*kadd+1] = jj ;
         kadd++ ;
       }
     }
   }

   xyzn = (int *) malloc( sizeof(int)*np*(kadd+1) ) ;   /* output array */

   /** add points around each input point, culling duplicates **/

   for( ii=jj=0 ; ii < np ; ii++ ){
     ix = ip[ii] ; jy = jp[ii] ;                                /* drawn point 2D index */
     if( ix >= 0 && ix < itop && jy >= 0 && jy < jtop ){
       xyzn[jj++] = base + ix*di + jy*dj ;                      /* load 3D index */
       for( kk=0 ; kk < kadd ; kk++ ){
         ixn = ix+nn[2*kk] ; jyn = jy+nn[2*kk+1] ;              /* nbhd pt 2D index */
         if( ixn >= 0 && ixn < itop && jyn >= 0 && jyn < jtop ){
           mm = base + ixn*di + jyn*dj ;                        /* 3D index */
#ifndef USE_COLLAPSAR
           if( ii > 0 )
             for( qq=0 ; qq < jj && xyzn[qq] != mm ; qq++ ) ;   /* nada */
           else
             qq = jj ;
           if( qq == jj ) xyzn[jj++] = mm ;                     /* save 3D index */
#else
           xyzn[jj++] = mm ;                                    /* save 3D index */
#endif
         }
       }

#ifdef USE_COLLAPSAR
       if( ii > 9 && (ii==np-1 || ii%20==0) ) DRAW_collapsar( &jj , xyzn ) ;
#endif
     }
   }

   *nfill = jj ; *xyzf = xyzn ; free(nn) ; return ;
}

/*-----------------------------------------------------------------------------*/
/*! Expand set of points in 3D, in a sphere.  RWCox - 16 Oct 2002.
-------------------------------------------------------------------------------*/

static void DRAW_3D_sphere( int np, int *xd, int *yd, int *zd, int plane ,
                            int *nfill , int **xyzf )
{
   int ix,jy,kz ;
   int nx=DSET_NX(dset) , ny=DSET_NY(dset) , nz=DSET_NZ(dset) , nxy = nx*ny ;
   int kadd , ii,jj,kk , ixn,jyn,kzn , mm,qq ;
   int nnew , *xyzn ;

   float dx = fabs(DSET_DX(dset)) ;
   float dy = fabs(DSET_DY(dset)) ;
   float dz = fabs(DSET_DZ(dset)) ;
   float rad= rad_av->fval ;
   float xq,yq,zq,radq ;
   int idx , jdy , kdz , *nn ;
   int www ;

   /* check inputs */

   if( np <= 0 || xd == NULL || yd == NULL || zd == NULL ) return ;
   if( nfill == NULL || xyzf == NULL )                     return ;

   idx = rad/dx ; jdy = rad/dy ; kdz = rad/dz ;
   if( idx < 1 && jdy < 1 && kdz < 1 ) return ;   /* sphere smaller than voxel */

#if 0
fprintf(stderr,"DRAW_3D_sphere: rad=%g  dx=%g idx=%d  dy=%g jdy=%d  dz=%g kdz=%d\n",
        rad,dx,idx,dy,jdy,dz,kdz ) ;
#endif

   /* make incremental mask */

   radq = 1.001*rad*rad ;
   nn   = (int *) malloc( sizeof(int)*(2*idx+1)*(2*jdy+1)*(2*kdz+1)*3 ) ;
   kadd = 0 ;
   for( kk=-kdz ; kk <= kdz ; kk++ ){
     zq = (kk*dz)*(kk*dz) ;
     for( jj=-jdy ; jj <= jdy ; jj++ ){
       yq = zq + (jj*dy)*(jj*dy) ;
       for( ii=-idx ; ii <= idx ; ii++ ){
         xq = yq + (ii*dx)*(ii*dx) ;
         if( xq <= radq && xq > 0.0 ){
           nn[3*kadd]   = ii ;
           nn[3*kadd+1] = jj ;
           nn[3*kadd+2] = kk ;
           kadd++ ;
         }
       }
     }
   }

   xyzn = (int *) malloc( sizeof(int)*np*(kadd+1) ) ;   /* output array */

   if( xyzn == NULL ){
     fprintf(stderr,"\n** DRAW_3D_sphere ERROR: can't allocate memory!\n\a");
     free(nn); return;
   }

   www = (np*(kadd+1) > 1234567) && (np > 1) ;          /* show waiting? */
   if( www ) SHOW_AFNI_PAUSE ;

   /** add points around each input point **/

   for( ii=jj=0 ; ii < np ; ii++ ){
     ix = xd[ii] ; jy = yd[ii] ; kz = zd[ii] ;
     if( ix >= 0 && ix < nx && jy >= 0 && jy < ny && kz >= 0 && kz <= nz ){
       xyzn[jj++] = ix + jy*nx + kz*nxy ;                       /* load 3D index */
       for( kk=0 ; kk < kadd ; kk++ ){
         ixn = ix+nn[3*kk] ; jyn = jy+nn[3*kk+1] ; kzn = kz+nn[3*kk+2] ;
         if( ixn >= 0 && ixn < nx && jyn >= 0 && jyn < ny && kzn >= 0 && kzn < nz ){
           mm = ixn + jyn*nx + kzn*nxy ;                        /* 3D index */
#ifndef USE_COLLAPSAR
           if( ii > 0 )
             for( qq=0 ; qq < jj && xyzn[qq] != mm ; qq++ ) ;   /* nada */
           else
             qq = jj ;
           if( qq == jj ) xyzn[jj++] = mm ;                     /* save 3D index */
#else
           xyzn[jj++] = mm ;                                    /* save 3D index */
#endif
         }
       }

#ifdef USE_COLLAPSAR
       if( ii > 5 && (ii==np-1 || ii%16==0) ) DRAW_collapsar( &jj , xyzn ) ;
#endif
     }
   }

   if( www ) SHOW_AFNI_READY ;

   *nfill = jj ; *xyzf = xyzn ; free(nn) ; return ;
}

/*--------------------------------------------------------------------------------*/

#ifdef USE_COLLAPSAR

/*! Collapses the input list of points to non-duplicates. */

static void DRAW_collapsar( int *npt , int *xyzn )
{
   int ii , jj , np ;

   if( npt == NULL || xyzn == NULL ) return ;
   np = *npt ; if( np <= 1 ) return ;

   qsort_int( np , xyzn ) ;                /* sort */

   for( ii=1 ; ii < np ; ii++ )            /* find 1st duplicates */
     if( xyzn[ii] == xyzn[ii-1] ) break ;
   if( ii == np ) return ;                 /* no duplicate => done */

   /* if [ii] is different from [jj],
      then add 1 to jj, and copy [ii] into the new [jj] location;
      otherwise, keep jj fixed (thus skipping [ii])               */

   for( jj=ii-1 ; ii < np ; ii++ ){
     if( xyzn[ii] != xyzn[jj] ) xyzn[++jj] = xyzn[ii] ;
   }

   *npt = jj+1 ; return ;
}
#endif  /* USE_COLLAPSAR */
