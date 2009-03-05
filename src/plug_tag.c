/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#undef MAIN
#include "afni.h"
#include <ctype.h>

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/*-- prototypes --*/

static char * TAG_main( PLUGIN_interface * ) ;

static void TAG_make_widgets(void) ;

static void TAG_dset_CB    ( Widget , XtPointer , XtPointer ) ;
static void TAG_quit_CB    ( Widget , XtPointer , XtPointer ) ;
static void TAG_help_CB    ( Widget , XtPointer , XtPointer ) ;
static void TAG_set_CB     ( Widget , XtPointer , XtPointer ) ;
static void TAG_clear_CB   ( Widget , XtPointer , XtPointer ) ;
static void TAG_done_CB    ( Widget , XtPointer , XtPointer ) ;
static void TAG_tog_CB     ( Widget , XtPointer , XtPointer ) ;
static void TAG_read_CB    ( Widget , XtPointer , XtPointer ) ;
static void TAG_write_CB   ( Widget , XtPointer , XtPointer ) ;
static void TAG_copy_CB    ( Widget , XtPointer , XtPointer ) ;
static void TAG_save_CB    ( Widget , XtPointer , XtPointer ) ;
static void TAG_clearall_CB( Widget , XtPointer , XtPointer ) ;
static void TAG_add_CB     ( Widget , XtPointer , XtPointer ) ;
static void TAG_delete_CB  ( Widget , XtPointer , XtPointer ) ;
static void TAG_relabel_CB ( Widget , XtPointer , XtPointer ) ;
static void TAG_beep_CB    ( Widget , XtPointer , XtPointer ) ;

static void TAG_value_CB ( MCW_arrowval * , XtPointer ) ;
static void TAG_columnize(void) ;
static void TAG_reset_widgets(void) ;

static void TAG_onoff( int ) ;

/*-- Widgets for the user interface --*/

static Widget shell=NULL , thetop ;
static Widget dset_pb , info_lab ;
static Widget quit_pb, help_pb, read_pb, write_pb, copy_pb, save_pb, done_pb ;
static Widget set_pb, clear_pb, clearall_pb, add_pb, delete_pb, relabel_pb, beep_pb ;
static Widget scrollw, wframe, workwin ;
static Widget actar ;

static Widget * tagtog ;      /* one for each possible tag */
static int    * toginv ;
static int      tognum = 4 ;  /* default number of active toggles */

static THD_usertaglist * mytagset  = NULL ;  /* what we are working on */
static THD_usertaglist * oldtagset = NULL ;  /* in case of Quit */

static PLUGIN_strval * file_strav ;
static PLUGIN_strval * label_strav ;
static MCW_arrowval  * value_av ;

#define DEFAULT_INFOLAB "[No dataset has yet been selected]"

static int      on_flag = 0 ;
static Widget * onoff_wid[] = {
   &read_pb , &write_pb , &copy_pb     , &save_pb , &done_pb   ,
   &set_pb  , &clear_pb , &clearall_pb ,
   NULL } ;

#define DSET_ON  SENSITIZE(dset_pb,True)
#define DSET_OFF do{ if( dset != NULL ) SENSITIZE(dset_pb,False) ; } while(0)

#define POPUP_MESG(mm) MCW_popup_message(save_pb,(mm),MCW_USER_KILL|MCW_TIMER_KILL)

/*-- Other Data --*/

static PLUGIN_interface * plint = NULL ;

static int   value_int   = 0 ;    /* from value_av */
static float value_float = 0.0 ;  /* ditto         */
static int   active_tog  = -1 ;   /* active toggle index */

static int   editor_open = 0 ;    /* is the editor already on screen? */

static MCW_DC * dc ;
static Three_D_View * im3d ;
static THD_3dim_dataset * dset = NULL ;

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

   plint = PLUTO_new_interface( "Edit Tagset" , NULL , NULL ,
                                PLUGIN_CALL_IMMEDIATELY , TAG_main ) ;

   PLUTO_add_hint( plint , "Interactive Tagset Editor" ) ;

   PLUTO_set_sequence( plint , "A:misc" ) ;
   return plint ;
}

/*--------------------------------------------------------------------------*/

static char * TAG_main( PLUGIN_interface * plint )
{
   int ii ;
   XmString xstr ;

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
      if( mytagset  == NULL ) mytagset  = myXtNew(THD_usertaglist) ;
      if( oldtagset == NULL ) oldtagset = myXtNew(THD_usertaglist) ;
      TAG_make_widgets() ;
      PLUTO_set_topshell( plint , shell ) ;  /* 22 Sep 2000 */
      RWC_visibilize_widget( shell ) ;       /* 27 Sep 2000 */
   }

   /*-- set titlebar --*/

   { char ttl[PLUGIN_STRING_SIZE] ;
     sprintf( ttl , "Tagset Editor %s" , AFNI_controller_label(im3d) ) ;
     XtVaSetValues( shell , XmNtitle , ttl , NULL ) ;
   }

   /*-- reset the info label --*/

   xstr = XmStringCreateLtoR( DEFAULT_INFOLAB , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( info_lab , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   /*-- pop the widgets up --*/

   TAG_onoff(0) ;                 /* widgets off until a dataset is selected */
   DSET_ON ;
   SENSITIZE(beep_pb,True) ;
   tognum = mytagset->num ;
   TAG_reset_widgets() ;

   XtMapWidget(shell) ;
   PLUTO_cursorize(shell) ;

   /*-- misc initialization --*/

   dset         = NULL ;   /* not editing anything   */
   editor_open  = 1 ;      /* editor is now open for business */

   active_tog = -1 ;
   for( ii=0 ; ii < tognum ; ii++ ){
      if( XmToggleButtonGetState( tagtog[ii] ) ){ active_tog = ii; break; }
   }

   oldtagset->num = 0 ;
   return NULL ;
}

/*--------------------------------------------------------------------------
     Make the plugin user interface
----------------------------------------------------------------------------*/

/*-- structures defining 1st set of action buttons --*/

#define NACT1 7  /* number of action buttons */

static MCW_action_item TAG_actor1[NACT1] = {
 {"Quit",TAG_quit_CB,NULL,
  "Discard changes\nand close Tag plugin" ,
   "Discard changes & close",0} ,

 {"Help",TAG_help_CB,NULL,
  "Displays more help" , "Displays more help",0} ,

 {"Read",TAG_read_CB,NULL,
  "Read a tagset from disk" , "Read tagset from disk",0} ,

 {"Write",TAG_write_CB,NULL,
  "Write the tagset to disk" , "Write tagset to disk",0} ,

 {"Copy",TAG_copy_CB,NULL,
  "Copy tagset from another dataset" , "Copy from another dataset",0} ,

 {"Save",TAG_save_CB,NULL,
  "Save tagset in dataset header" , "Save tagset in dataset header",0} ,

 {"Done",TAG_done_CB,NULL,
  "Save changes in dataset\nand close Tag plugin" , "Save and close",1}
} ;

/*-- structures defining 2nd set of action buttons --*/

#define NACT2 7  /* number of action buttons */

static MCW_action_item TAG_actor2[NACT2] = {

 {"Set",TAG_set_CB,NULL,
  "Set the selected tag at the\ncurrent crosshair location" ,
  "Sets selected tag" , 0 } ,

 {"Clear",TAG_clear_CB,NULL,
  "Clears the selected tag\n(opposite of Set)" ,
  "Clears selected tag" , 0 } ,

 {"Clr All",TAG_clearall_CB,NULL,
  "Clears all tags (opposite of Set)" ,
  "Clears all tags" , 0 } ,

 {"Add",TAG_add_CB,NULL,
  "Add a new tag at\nthe end of list" ,
  "Add new tag at end" , 0 } ,

 {"Delete",TAG_delete_CB,NULL,
  "Remove the last\ntag from the list" ,
  "Remove last tag" , 0 } ,

 {"Relabel",TAG_relabel_CB,NULL,
  "Changes label of selected tag;\nUse after editing Tag Label\nstring field below",
  "Changes label of selected tag", 0 } ,

 {"Beep",TAG_beep_CB,NULL, NULL , NULL , 0 }
} ;

/*--------------------------------------------------------------------------*/

static void TAG_make_widgets(void)
{
   XmString xstr ;
   char tlab[MAX_TAG_LABEL] ;
   int ii , ww , hh ;
   Pixel  fg_pix=0 ;
   Widget wjunk ;

   /*** top level shell for window manager ***/

   shell =
      XtVaAppCreateShell(
           "AFNI" , "AFNI" , topLevelShellWidgetClass , dc->display ,

           XmNtitle             , "AFNI Editor" , /* top of window */
           XmNiconName          , "Editor"      , /* label on icon */
           XmNdeleteResponse    , XmDO_NOTHING  , /* deletion handled below */
           XmNallowShellResize  , False ,         /* let code resize shell? */
           XmNmappedWhenManaged , False ,         /* must map it manually */
           XmNinitialResourcesPersistent , False ,
      NULL ) ;

   DC_yokify( shell , dc ) ;

   if( afni48_good )             /* set icon pixmap */
      XtVaSetValues( shell ,
                        XmNiconPixmap , afni48_pixmap ,
                     NULL ) ;

   if( MCW_isitmwm(shell) )      /* remove some MWM functions */
      XtVaSetValues( shell ,
                       XmNmwmFunctions , MWM_FUNC_MOVE     |
                                         MWM_FUNC_CLOSE    |
                                         MWM_FUNC_MINIMIZE |
                                         MWM_FUNC_RESIZE    ,
                     NULL ) ;

   XmAddWMProtocolCallback(      /* make "Close" window menu work */
           shell ,
           XmInternAtom( dc->display , "WM_DELETE_WINDOW" , False ) ,
           TAG_quit_CB , (XtPointer) plint ) ;

   /*** form widget to hold all user interface stuff ***/

   thetop = XtVaCreateWidget(
                 "AFNI" , xmFormWidgetClass , shell ,
                     XmNborderWidth , 0 ,
                     XmNborderColor , 0 ,
                     XmNtraversalOn , True  ,
                     XmNinitialResourcesPersistent , False ,
                 NULL ) ;

   /*** pushbutton to select dataset for tagging ***/

   xstr = XmStringCreateLtoR( "Dataset" , XmFONTLIST_DEFAULT_TAG ) ;
   dset_pb =  XtVaCreateManagedWidget(
                "AFNI" , xmPushButtonWidgetClass , thetop ,
                   XmNlabelString , xstr ,
                   XmNmarginHeight , 4 ,
                   XmNmarginWidth  , 4 ,
                   XmNtraversalOn , True  ,
                   XmNinitialResourcesPersistent , False ,
                   XmNleftAttachment , XmATTACH_FORM ,
                   XmNtopAttachment  , XmATTACH_FORM ,
                   XmNtopOffset      , 7 ,
                   XmNleftOffset     , 5 ,
               NULL ) ;
   XmStringFree(xstr) ;

   XtAddCallback( dset_pb , XmNactivateCallback , TAG_dset_CB , NULL ) ;

   MCW_register_help( dset_pb , "Select dataset for tagging" ) ;
   MCW_register_hint( dset_pb , "Select dataset" ) ;

   /*** label at top to let user know who we are ***/

   xstr = XmStringCreateLtoR( DEFAULT_INFOLAB , XmFONTLIST_DEFAULT_TAG ) ;
   info_lab = XtVaCreateManagedWidget(
                 "AFNI" , xmLabelWidgetClass , thetop ,
                    XmNleftAttachment , XmATTACH_WIDGET ,
                    XmNleftWidget     , dset_pb ,
                    XmNleftOffset     , 10 ,
                    XmNtopAttachment  , XmATTACH_FORM ,
                    XmNtopOffset      , 5 ,
                    XmNrightAttachment, XmATTACH_FORM ,
                    XmNlabelString , xstr ,
                    XmNmarginHeight , 1 ,
                    XmNalignment , XmALIGNMENT_BEGINNING ,
                    XmNrecomputeSize , True ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   XmStringFree(xstr) ;
   MCW_register_help( info_lab , "Shows dataset being tagged" ) ;
   MCW_register_hint( info_lab , "Shows dataset being tagged" ) ;

   /*** separator for visual neatness ***/

   wjunk =  XtVaCreateManagedWidget(
             "AFNI" , xmSeparatorWidgetClass , thetop ,
                XmNseparatorType , XmSINGLE_LINE ,
                XmNleftAttachment , XmATTACH_FORM ,
                XmNrightAttachment, XmATTACH_FORM ,
                XmNtopAttachment  , XmATTACH_WIDGET ,
                XmNtopWidget      , dset_pb ,
                XmNtopOffset      , 5 ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   /*** 1st set of action buttons below the line ***/

   actar = MCW_action_area( thetop , TAG_actor1 , NACT1 ) ;

   XtVaSetValues( actar ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNrightAttachment, XmATTACH_FORM ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopOffset      , 5 ,
                     XmNtopWidget      , wjunk ,
                  NULL ) ;

   quit_pb  = (Widget) TAG_actor1[0].data ;  /* save individual buttons */
   help_pb  = (Widget) TAG_actor1[1].data ;
   read_pb  = (Widget) TAG_actor1[2].data ;
   write_pb = (Widget) TAG_actor1[3].data ;
   copy_pb  = (Widget) TAG_actor1[4].data ;
   save_pb  = (Widget) TAG_actor1[5].data ;
   done_pb  = (Widget) TAG_actor1[6].data ;

   /*** separator for visual neatness ***/

   wjunk =  XtVaCreateManagedWidget(
             "AFNI" , xmSeparatorWidgetClass , thetop ,
                XmNseparatorType  , XmSINGLE_LINE ,
                XmNleftAttachment , XmATTACH_FORM ,
                XmNrightAttachment, XmATTACH_FORM ,
                XmNtopAttachment  , XmATTACH_WIDGET ,
                XmNtopWidget      , actar ,
                XmNtopOffset      , 5 ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   /*** 2nd set of action buttons ***/

   actar = MCW_action_area( thetop , TAG_actor2 , NACT2 ) ;

   XtVaSetValues( actar ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNrightAttachment, XmATTACH_FORM ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopOffset      , 5 ,
                     XmNtopWidget      , wjunk ,
                  NULL ) ;

   set_pb     = (Widget) TAG_actor2[0].data ;  /* save individual buttons */
   clear_pb   = (Widget) TAG_actor2[1].data ;
   clearall_pb= (Widget) TAG_actor2[2].data ;
   add_pb     = (Widget) TAG_actor2[3].data ;
   delete_pb  = (Widget) TAG_actor2[4].data ;
   relabel_pb = (Widget) TAG_actor2[5].data ;
   beep_pb    = (Widget) TAG_actor2[6].data ;

   /*** separator for visual neatness ***/

   wjunk =  XtVaCreateManagedWidget(
             "AFNI" , xmSeparatorWidgetClass , thetop ,
                XmNseparatorType  , XmSINGLE_LINE ,
                XmNleftAttachment , XmATTACH_FORM ,
                XmNrightAttachment, XmATTACH_FORM ,
                XmNtopAttachment  , XmATTACH_WIDGET ,
                XmNtopWidget      , actar ,
                XmNtopOffset      , 5 ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   /*** string chooser for tagset filename ***/

   file_strav = new_PLUGIN_strval( thetop , "Tag File:  " ) ;
   alter_PLUGIN_strval_width( file_strav , 37 ) ;

   MCW_reghelp_children( file_strav->rowcol ,
                         "Use this to enter the tagset filename\n"
                         "before using the Read or Write buttons.\n"
                         "(Tagset filenames must end in '.tag'.)"
                       ) ;
   MCW_reghint_children( file_strav->rowcol , "Used with Read/Write buttons" ) ;

   XtVaSetValues( file_strav->rowcol ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNrightAttachment, XmATTACH_FORM ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopOffset      , 5 ,
                     XmNtopWidget      , wjunk ,
                  NULL ) ;

   /*** string chooser for relabeling tags ***/

   label_strav = new_PLUGIN_strval( thetop , "Tag Label: " ) ;
   alter_PLUGIN_strval_width( label_strav , 37 ) ;

   MCW_reghelp_children( label_strav->rowcol ,
                         "Use this to enter the new tag label\n"
                         "before using the Relabel button."
                       ) ;
   MCW_reghint_children( label_strav->rowcol , "Used with Relabel button" ) ;

   XtVaSetValues( label_strav->rowcol ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNrightAttachment, XmATTACH_FORM ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopOffset      , 5 ,
                     XmNtopWidget      , file_strav->rowcol ,
                  NULL ) ;

   /*** arrowval to choose value that is drawn into dataset voxels ***/

   value_av = new_MCW_arrowval( thetop , "Tag Value: " ,
                                MCW_AV_downup , -32767,32767,value_int ,
                                MCW_AV_editext , 0 ,
                                TAG_value_CB , NULL , NULL,NULL ) ;

   MCW_reghelp_children( value_av->wrowcol ,
                         "Use this to set the value that\n"
                         "will be associated with a tag."
                       ) ;
   MCW_reghint_children( value_av->wrowcol , "Value associated with tag" ) ;

   XtVaSetValues( value_av->wrowcol ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNrightAttachment, XmATTACH_FORM ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopOffset      , 5 ,
                     XmNtopWidget      , label_strav->rowcol ,
                  NULL ) ;

   /*** separator for visual neatness ***/

   wjunk = XtVaCreateManagedWidget(
             "AFNI" , xmSeparatorWidgetClass , thetop ,
                XmNseparatorType , XmSINGLE_LINE ,
                XmNleftAttachment , XmATTACH_FORM ,
                XmNrightAttachment, XmATTACH_FORM ,
                XmNtopAttachment  , XmATTACH_WIDGET ,
                XmNtopWidget      , value_av->wrowcol ,
                XmNtopOffset      , 5 ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   /*** scrolled window to hold the tag selectors ***/

   scrollw =
         XtVaCreateWidget(
           "AFNI" , xmScrolledWindowWidgetClass ,  thetop ,
              XmNscrollingPolicy , XmAUTOMATIC ,
              XmNleftAttachment  , XmATTACH_FORM ,
              XmNrightAttachment , XmATTACH_FORM ,
              XmNtopAttachment   , XmATTACH_WIDGET ,
              XmNbottomAttachment, XmATTACH_FORM ,
              XmNtopWidget       , wjunk ,
              XmNtopOffset       , 5 ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   wframe =
         XtVaCreateWidget(
           "AFNI" , xmFrameWidgetClass , scrollw ,
               XmNshadowType , XmSHADOW_ETCHED_IN ,
               XmNshadowThickness , 5 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   workwin =
         XtVaCreateWidget(
           "AFNI" , xmRowColumnWidgetClass , wframe ,
              XmNpacking     , XmPACK_COLUMN ,
              XmNnumColumns  , 1 ,
              XmNorientation , XmVERTICAL ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   XtVaGetValues( workwin , XmNforeground , &fg_pix , NULL ) ;

   XtVaSetValues( workwin ,
                     XmNradioBehavior , True ,
                     XmNradioAlwaysOne , False ,
                     XmNspacing      , 1 ,
                     XmNmarginHeight , 0 ,
                     XmNmarginWidth  , 0 ,
                  NULL ) ;

   /*** array of toggles for each possible tag ***/

   tagtog = (Widget *) XtMalloc( sizeof(Widget) * MAX_TAG_NUM ) ;
   toginv = (int *)    XtMalloc( sizeof(int)    * MAX_TAG_NUM ) ;

   for( ii=0 ; ii < MAX_TAG_NUM ; ii++ ){
      sprintf(tlab,"Tag #%d",ii) ;
      xstr = XmStringCreateLtoR( tlab , XmFONTLIST_DEFAULT_TAG ) ;

      tagtog[ii] =
           XtVaCreateWidget(
              "dialog" , xmToggleButtonWidgetClass , workwin ,
                 XmNlabelString    , xstr ,
                 XmNvisibleWhenOff , True ,
                 XmNmarginHeight   , 0 ,
                 XmNmarginWidth    , 0 ,
#if 0
                 XmNselectColor    , fg_pix ,
#endif
                 XmNtraversalOn    , True  ,
                 XmNinitialResourcesPersistent , False ,
              NULL ) ;

      XmStringFree(xstr) ;
      toginv[ii] = 0 ;
      XtAddCallback( tagtog[ii] , XmNdisarmCallback , TAG_tog_CB , NULL ) ;

      if( ii < tognum ) XtManageChild( tagtog[ii] ) ;

      mytagset->tag[ii].set = 0 ;
      mytagset->tag[ii].ti  = 0 ;
      mytagset->tag[ii].x   = 0.0 ;
      mytagset->tag[ii].y   = 0.0 ;
      mytagset->tag[ii].z   = 0.0 ;
      mytagset->tag[ii].val = 0.0 ;
      strcpy( mytagset->tag[ii].label , tlab ) ;
   }
   mytagset->num = tognum ;
   strcpy( mytagset->label , "Tar-Palantir" ) ;

   /*** that's almost all ***/

   XtManageChild( workwin ) ;
   XtManageChild( wframe  ) ;
   XtManageChild( scrollw ) ;
   XtManageChild( thetop  ) ;

   XtRealizeWidget( shell ) ;  NI_sleep(1) ; /* will not be mapped */

   /** set up widths and heights **/

   { XmFontList xflist=(XmFontList)NULL ;

     for( ii=0 ; ii < MAX_TAG_LABEL-1 ; ii++ ) tlab[ii] = 'X' ;
     tlab[MAX_TAG_LABEL-1] = '\0' ;

     xstr = XmStringCreateLtoR( tlab , XmFONTLIST_DEFAULT_TAG ) ;

     XtVaGetValues( thetop , XmNbuttonFontList , &xflist , NULL ) ;
     hh = XmStringHeight( xflist , xstr ) ;
     ww = XmStringWidth ( xflist , xstr ) ;
     XmStringFree(xstr) ;
   }

   XtVaSetValues( shell ,
                     XmNminWidth  , ww+19*hh ,  /* semi-empirical numbers; */
                     XmNwidth     , ww+23*hh ,  /* all primes (for luck)  */
                     XmNminHeight , 23*hh ,
                     XmNheight    , 29*hh ,
                  NULL ) ;

   TAG_columnize() ;
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_redraw(void)
{
   if( dset == NULL ) return ;

   if( dset->tagset == NULL ) dset->tagset = myXtNew(THD_usertaglist) ;

   *(dset->tagset) = *mytagset ;  /* copy all data in one swell foop */

   PLUTO_dset_redisplay_mode( dset , REDISPLAY_OVERLAY ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_quit_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( editor_open ){
      if( dset != NULL && dset->tagset != NULL ){
         *(dset->tagset) = *oldtagset ;
         if( dset->tagset->num == 0 ) myXtFree(dset->tagset) ;
         PLUTO_dset_redisplay_mode( dset , REDISPLAY_OVERLAY ) ;
      }
      XtUnmapWidget( shell ) ; editor_open = 0 ; dset = NULL ;
      oldtagset->num = 0 ;
   }
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_value_CB( MCW_arrowval * av , XtPointer cd )
{
   value_int   = av->ival ;
   value_float = av->fval ;

   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_columnize(void)
{
   int nc = (tognum < 21) ? 1 : 2 ;
   XtVaSetValues( workwin , XmNnumColumns , nc , NULL ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

#define UNSET(j)                                                              \
  do{ XmToggleButtonSetState( tagtog[(j)] , 0 , False ) ;                     \
      mytagset->tag[(j)].set = 0 ;                                            \
      if( active_tog == (j) ) active_tog = -1 ;                               \
      if( toginv[(j)] ){ toginv[(j)] = 0; MCW_invert_widget( tagtog[(j)] ); } \
  } while(0)

/*--------------------------------------------------------------------------*/

static void TAG_add_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }

   if( tognum >= MAX_TAG_NUM ){
      char buf[64] ;
      sprintf(buf,"Maximum number of\nallowed tags is %d",MAX_TAG_NUM) ;
      POPUP_MESG(buf) ; BEEPIT ;
      return ;
   }

   UNSET(tognum) ;
   XtManageChild( tagtog[tognum] ) ;
   tognum++ ; mytagset->num = tognum ; TAG_columnize() ; DSET_OFF ;
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_delete_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }

   if( tognum <= 1 ){
      POPUP_MESG("You can't delete the\nonly remaining tag!") ;
      BEEPIT ; return ;
   }

   UNSET(tognum-1) ;
   XtUnmanageChild( tagtog[tognum-1] ) ;
   tognum-- ; mytagset->num = tognum ; TAG_columnize() ;
   TAG_redraw() ; DSET_OFF ;
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_set_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   int ii = active_tog ;

   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }

   if( ii < 0 || ii >= tognum ){
      BEEPIT ;
      POPUP_MESG( "Can't set a tag\nuntil one is selected" ) ;
      return ;
   }

   mytagset->tag[ii].set = 1 ;
   mytagset->tag[ii].ti  = im3d->vinfo->time_index ;
   mytagset->tag[ii].x   = im3d->vinfo->xi ;
   mytagset->tag[ii].y   = im3d->vinfo->yj ;
   mytagset->tag[ii].z   = im3d->vinfo->zk ;
   mytagset->tag[ii].val = value_float ;

   if( !toginv[ii] ){ toginv[ii] = 1 ; MCW_invert_widget( tagtog[ii] ) ; }

   DSET_OFF ; TAG_redraw() ;
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_clear_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   int ii=active_tog , oldset ;

   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }

   if( ii < 0 || ii >= tognum ){
      BEEPIT ;
      POPUP_MESG( "Can't clear a tag\nuntil one is selected" ) ;
      return ;
   }

   oldset = mytagset->tag[ii].set ;
   mytagset->tag[ii].set = 0 ;

   if( toginv[ii] ){ toginv[ii] = 0 ; MCW_invert_widget( tagtog[ii] ) ; }

   if( oldset ){ TAG_redraw() ; DSET_OFF ; }
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_clearall_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   int ii , oldset=0 ;

   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }

   if( tognum < 1 ){ BEEPIT ; return ; }  /* should not occur */

   for( ii=0 ; ii < tognum ; ii++ ){
      if( mytagset->tag[ii].set ) oldset++ ;
      mytagset->tag[ii].set = 0 ;
      if( toginv[ii] ){ toginv[ii] = 0 ; MCW_invert_widget( tagtog[ii] ) ; }
   }

   if( oldset ){ TAG_redraw() ; DSET_OFF ; }
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_relabel_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   char * str=get_PLUGIN_strval(label_strav) ;
   int ii=active_tog , ll,kk,nn ;
   XmString xstr ;

   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }

   if( str == NULL ){ BEEPIT ; return ; }  /* should not occur */

   if( str[0] == '\0' || ii < 0 || ii >= tognum ){
      BEEPIT; XtFree(str);
      POPUP_MESG( "Can't relabel a tag unless\n"
                  "one is selected AND a new\n"
                  "label is typed in" ) ;
      return;
   }

   ll = strlen(str) ;
   if( ll >= MAX_TAG_LABEL ){ str[MAX_TAG_LABEL-1] = '\0'; ll = strlen(str); }
   for( kk=nn=0 ; kk < ll ; kk++ ) if( isspace(str[kk]) ) nn++ ;
   if( nn == ll ){
      BEEPIT; XtFree(str);
      POPUP_MESG( "Can't relabel a\ntag to all blanks!" ) ;
      return ;
   }

   xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( tagtog[ii] , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   strcpy( mytagset->tag[ii].label , str ) ;
   XtFree(str) ; DSET_OFF ;
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_tog_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   int ii ;

   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }

   for( ii=0 ; ii < tognum ; ii++ ){
      if( XmToggleButtonGetState( tagtog[ii] ) ){  /* found it */
         active_tog = ii ;
         if( mytagset->tag[ii].set ){              /* if already set */
            AFNI_jumpto_dicom( im3d , mytagset->tag[ii].x ,
                                      mytagset->tag[ii].y , mytagset->tag[ii].z ) ;

            AV_assign_fval( value_av , mytagset->tag[ii].val ) ;
            value_int   = value_av->ival ;
            value_float = value_av->fval ;
         }
         return ;
      }
   }

   active_tog = -1 ; return ;  /* found nothing */
}

/*--------------------------------------------------------------------------*/

static void TAG_write_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   char * str=get_PLUGIN_strval(file_strav) , * cpt ;
   int ii , jj , ltop ;
   FILE * fp ;

   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }

   if( str == NULL ){ BEEPIT ; return ; }  /* should not occur */

   if( str[0] == '\0' || tognum < 1 ){
      XtFree(str) ; BEEPIT ;
      POPUP_MESG( "Can't write tags to a file\n"
                  "until a filename is typed in" ) ;
      return ;
   }

   cpt = strstr( str , ".tag" ) ;
   if( cpt == NULL ){
      ii = strlen(str) ;
      cpt = XtMalloc(ii+8) ;
      strcpy(cpt,str) ;
      if( cpt[ii-1] != '.' ) strcat( cpt , "." ) ;
      strcat( cpt , "tag" ) ;
      XtFree(str) ;
      str = cpt ;
   }

   if( !THD_filename_ok(str) ){
      XtFree(str) ; BEEPIT ;
      POPUP_MESG( "The filename you entered\nhas illegal characters!" ) ;
      return ;
   }

   fp = fopen( str , "w" ) ;
   if( fp == NULL ){
      XtFree(str) ; BEEPIT ;
      POPUP_MESG( "Can't open output file!\n" ) ;
      return ;
   }

   ltop = 6 ;
   for( ii=0 ; ii < tognum ; ii++ ){
      jj   = strlen( mytagset->tag[ii].label ) ;
      ltop = MAX( jj , ltop ) ;
   }

   cpt = XtMalloc( MAX_TAG_LABEL+256 ) ;

   strcpy(cpt,"# Label") ;
   for( jj=strlen(cpt) ; jj < ltop+2 ; jj++ )
      strcat( cpt , "_" ) ;
   strcat(cpt,"  ") ;
   strcat(cpt," _____x_____ _____y_____ _____z_____ ____val____ _t_") ;
   fprintf(fp,"%s\n",cpt) ;

   for( ii=0 ; ii < tognum ; ii++ ){
      strcpy( cpt , "'" ) ;
      strcat( cpt , mytagset->tag[ii].label ) ;
      strcat( cpt , "'" ) ;

      if( mytagset->tag[ii].set ){
         for( jj=strlen(cpt) ; jj < ltop+4 ; jj++ )
            strcat( cpt , " " ) ;

         fprintf(fp , "%s %11.4g %11.4g %11.4g %11.4g %3d\n" ,
                 cpt, mytagset->tag[ii].x, mytagset->tag[ii].y, mytagset->tag[ii].z,
                      mytagset->tag[ii].val, mytagset->tag[ii].ti ) ;
      } else {
         fprintf(fp , "%s\n" , cpt ) ;
      }
   }

   fclose(fp) ; XtFree(cpt) ;
   fprintf(stderr,"Wrote tag file %s\n",str) ;
   XtFree(str) ; return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_reset_widgets(void)
{
   int ii ;
   XmString xstr ;

   XtUnmanageChild(wframe) ;

   for( ii=0 ; ii < tognum ; ii++ ){

      XtManageChild( tagtog[ii] ) ;
      XmToggleButtonSetState( tagtog[ii] , 0 , False ) ;

      if( mytagset->tag[ii].set && !toginv[ii] ){
         toginv[ii] = 1; MCW_invert_widget( tagtog[ii] );
      } else if( !mytagset->tag[ii].set && toginv[ii] ){
         toginv[ii] = 0; MCW_invert_widget( tagtog[ii] );
      }

      xstr = XmStringCreateLtoR( mytagset->tag[ii].label , XmFONTLIST_DEFAULT_TAG ) ;
      XtVaSetValues( tagtog[ii] , XmNlabelString , xstr , NULL ) ;
      XmStringFree(xstr) ;
   }

   for( ; ii < MAX_TAG_NUM ; ii++ ){
      UNSET(ii) ;
      XtUnmanageChild( tagtog[ii] ) ;
   }

   TAG_columnize() ; XtManageChild(wframe) ;
   active_tog = -1 ; return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_read_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   char * str=get_PLUGIN_strval(file_strav) , * cpt , buf[256] , quote ;
   int ii , jj , ntog , lbuf , kk ;
   FILE * fp ;
   char  new_label[MAX_TAG_LABEL] ;
   float new_x , new_y , new_z , new_val ;
   int   new_ti , new_set ;
   XmString xstr ;

   /*-- sanity checks --*/

   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }

   if( str == NULL ){ BEEPIT ; return ; }  /* should not happen */

   if( str[0] == '\0' ){
      XtFree(str) ; BEEPIT ;
      POPUP_MESG( "Can't read a tagset until\nyou type in a filename" ) ;
      return ;
   }

   /*-- check for suffix on filename --*/

   cpt = strstr( str , ".tag" ) ;
   if( cpt == NULL ){
      ii = strlen(str) ;
      cpt = XtMalloc(ii+8) ;
      strcpy(cpt,str) ;
      if( cpt[ii-1] != '.' ) strcat( cpt , "." ) ;
      strcat( cpt , "tag" ) ;
      XtFree(str) ;
      str = cpt ;
   }

   /*-- open file --*/

   fp = fopen( str , "r" ) ;
   if( fp == NULL ){
      XtFree(str) ; BEEPIT ;
      POPUP_MESG( "Can't open input file!" ) ;
      return ;
   }

   /*-- scan each line for a tag definition --*/

   ntog = 0 ;
   while( ntog < MAX_TAG_NUM ){
      cpt = fgets( buf , 256 , fp ) ;   /* read line from disk */
      if( cpt == NULL ) break ;         /* nothing => exit */
      if( buf[0] == '#'  ) continue ;   /* comment => skip this line */
      if( buf[0] == '\n' ) continue ;
      if( buf[0] == '\0' ) continue ;

      lbuf = strlen(buf) ;              /* skip whitespace at start */
      jj = 0 ;
      while( jj < lbuf && isspace(buf[jj]) ) jj++ ;
      if( jj == lbuf ) continue ;       /* was a blank line => skip */
      if( buf[jj] == '#'  ) continue ;  /* comment */

      /* scan for new label */

      if( buf[jj] == '\'' || buf[jj] == '\"' ){  /* scan to matching quote */
         quote = buf[jj] ; jj++ ; kk = jj ;
         while( kk < lbuf && buf[kk] != quote ) kk++ ;
         if( kk == lbuf ) kk-- ;
      } else {                                   /* scan to nonblank */
         kk = jj+1 ;
         while( kk < lbuf && !isspace(buf[kk]) ) kk++ ;
      }
      for( ii=0 ; ii < MAX_TAG_LABEL-1 && jj < kk ; ii++,jj++ )
         new_label[ii] = buf[jj] ;
      new_label[ii] = '\0' ;
      if( strlen(new_label) == 0 ) continue ;  /* error => skip */
      jj = kk+1 ;

      /* scan for x y z val ti */

      new_set = new_ti = 0 ;
      new_x   = new_y  = new_z = new_val = 0.0 ;
      if( jj < lbuf-4 ){
         kk = sscanf( buf+jj , "%f %f %f %f %d" ,
                      &new_x , &new_y , &new_z , &new_val , &new_ti ) ;
         if( kk >= 3 ) new_set = 1 ;  /* got x y z, at least */
      }

      /* set values */

      strcpy( mytagset->tag[ntog].label , new_label ) ;
      mytagset->tag[ntog].set = new_set ;
      mytagset->tag[ntog].ti  = new_ti  ;
      mytagset->tag[ntog].x   = new_x   ;
      mytagset->tag[ntog].y   = new_y   ;
      mytagset->tag[ntog].z   = new_z   ;
      mytagset->tag[ntog].val = new_val ;
      ntog++ ;
   }

   fclose(fp) ;  /* done with file */

   if( ntog == 0 ){                                 /* no tags ==> error */
      sprintf(buf,"Couldn't read tagset from\n"
                  "file %s" , str ) ;
      POPUP_MESG( buf ) ;
      BEEPIT ; XtFree(str) ; return ;
   }

   /*-- now reset the widgets --*/

   tognum = mytagset->num = ntog ;
   TAG_reset_widgets() ;
   TAG_redraw() ; DSET_OFF ; XtFree(str) ;
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_get_dset_CB( int num, THD_3dim_dataset ** dslist, void * cd )
{
   char str[256] , * tnam ;
   XmString xstr ;

   /*-- sanity checks --*/

   if( !IM3D_OPEN(im3d) || !editor_open ){
      BEEPIT ; POPDOWN_strlist_chooser ;
      TAG_quit_CB(NULL,NULL,NULL) ; return ;
   }

   /* this should not occur: */

   if( num != 1 || dslist == NULL || !ISVALID_DSET(dslist[0]) ){ BEEPIT; return; }

   /* assign global variable */

   dset = dslist[0] ;

   /*-- change the informational label --*/

   sprintf(str,"%s%s", DSET_DIRNAME(dset) , DSET_FILECODE(dset) ) ;
   tnam = THD_trailname( str , SESSTRAIL+1 ) ;
   xstr = XmStringCreateLtoR( tnam , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( info_lab , XmNlabelString , xstr , NULL ) ;
   XmStringFree(xstr) ;

   /*-- if the new dataset has tags, use them --*/

   if( dset->tagset != NULL && dset->tagset->num > 0 ){
      *mytagset  = *(dset->tagset) ;  /* copy all data in one swell foop */
      *oldtagset = *mytagset ;        /* backup copy */
      tognum = mytagset->num ;
      TAG_reset_widgets() ;
   } else {
      oldtagset->num = 0 ;
      if( dset->tagset == NULL ){
         dset->tagset = myXtNew(THD_usertaglist) ;
         dset->tagset->num = 0 ;
      }
      if( tognum > 0 ) TAG_clearall_CB(NULL,NULL,NULL) ;
   }

   TAG_onoff( 1 ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

static int TAG_check_dataset( THD_3dim_dataset * qset, void * cd )
{
#if 0
   return ISANAT(qset) ;
#else
   return 1 ;  /* All of them, Frank */
#endif
}

static int TAG_check_copyset( THD_3dim_dataset * qset, void * cd )
{
   return ( ISANAT(qset)         && !EQUIV_DSETS(qset,dset) &&
            qset->tagset != NULL && qset->tagset->num > 0      ) ;
}

/*--------------------------------------------------------------------------*/

static void TAG_dset_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   /*-- sanity checks --*/

   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }

   /*-- just show a chooser, let the user commune with the list --*/

   PLUTO_popup_dset_chooser( help_pb , im3d->vinfo->view_type , 0 ,
                             TAG_check_dataset , TAG_get_dset_CB , NULL ) ;

   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_save_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }

   if( dset == NULL ) return ;  /* nothing to do */

   if( dset->tagset == NULL ){
      dset->tagset = myXtNew(THD_usertaglist) ;
      ADDTO_KILL( dset->kl , dset->tagset ) ;
   }

   *(dset->tagset) = *mytagset ;  /* copy all data in one swell foop */
   *oldtagset      = *mytagset ;  /* backup copy is replaced */

   putenv("AFNI_DECONFLICT=OVERWRITE") ;
   DSET_overwrite_header(dset) ;
   TAG_redraw() ; DSET_ON ;
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_onoff( int on )
{
   Boolean sen ;
   int ii ;

   sen = (Boolean) on ;
   for( ii=0 ; onoff_wid[ii] != NULL ; ii++ )
      SENSITIZE( *(onoff_wid[ii]) , sen ) ;    /* see xutil.h */

   on_flag = on ;
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_done_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   TAG_save_CB(NULL,NULL,NULL) ; dset = NULL ;
   TAG_quit_CB(NULL,NULL,NULL) ;
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_get_copy_CB( int num, THD_3dim_dataset ** dslist, void * cd )
{
   char str[256] , * tnam ;
   THD_3dim_dataset * qset ;
   XmString xstr ;

   /*-- sanity checks --*/

   if( !IM3D_OPEN(im3d) || !editor_open ){
      BEEPIT ; POPDOWN_strlist_chooser ;
      TAG_quit_CB(NULL,NULL,NULL) ; return ;
   }

   if( num != 1 || dslist == NULL || !ISVALID_DSET(dslist[0]) ){ BEEPIT; return; }

   qset = dslist[0] ; if( qset->tagset == NULL ){ BEEPIT; return; }

   *mytagset = *(qset->tagset) ;
   tognum    = mytagset->num ;

   TAG_reset_widgets() ; TAG_redraw() ; DSET_OFF ;
   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_copy_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   /*-- sanity checks --*/

   if( !IM3D_OPEN(im3d) ){ BEEPIT ; TAG_quit_CB(NULL,NULL,NULL) ; return ; }

   /*-- just show a chooser, let the user commune with the list --*/

   PLUTO_popup_dset_chooser( help_pb , im3d->vinfo->view_type , 0 ,
                             TAG_check_copyset , TAG_get_copy_CB , NULL ) ;

   return ;
}

/*--------------------------------------------------------------------------*/

static void TAG_help_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   (void ) new_MCW_textwin( help_pb ,

     "This plugin can be used to attach a collection of 'tags' to an anatomical\n"
     "dataset.  Each tag has associated with it a label, a set of (x,y,z)\n"
     "coordinates, and a numerical value.  If a tag is 'set', then a marker will\n"
     "be displayed at its location when the dataset is being viewed, and when\n"
     "'See Markers' is enabled on the AFNI control panel.  Tags that are not set\n"
     "are not shown and are not used for any purpose.\n"

     "\n"
     "The first step in defining a set of tags for a dataset is to pick the\n"
     "dataset to be operated upon.  Normally, this would be the dataset that\n"
     "you are viewing in some window (otherwise you won't see the tags).\n"
     "Choosing a dataset for tag operations is done with the 'Dataset' button\n"
     "at the top of the plugin interface.  This will popup a chooser that\n"
     "lets you select any anatomical dataset.  After you select the dataset,\n"
     "its existing tags (if any) are loaded into the plugin and can then\n"
     "be modified.\n"

     "\n"
     "There are two rows of control buttons below the 'Dataset' choice button.\n"
     "The first row handles large operations on the whole set of tags.  The\n"
     "second row is for retail operations on the status of individual tags.\n"
     "Most of the control buttons are deactivated until you select a dataset.\n"
     "Below these rows is a set of data entry fields, and below that a list\n"
     "of all the tags currently defined (whether set or not -- tags that are\n"
     "set are shown in inverted colors).  Each entry in the tag list has a\n"
     "selection button.  At most one tag can be selected at a given time.\n"
     "\n"

     "Row 1 buttons are:\n"
     "-----------------\n"
     "'Quit': Exit the plugin without saving changes back to the dataset.\n"
     "\n"
     "'Help': Display this message.\n"
     "\n"
     "'Read': Read in a tagset description file, replacing the current tag\n"
     "        definitions.  Each line in a tagset file (extension '.tag')\n"
     "        defines one tag.  The first entry on a line is the tag label.\n"
     "        If the label contains any blanks, it should be enclosed in\n"
     "        quotes (either \"double\" or 'single' quotes will work).\n"
     "        Following the label, it is legal to put three numbers, which\n"
     "        will be interpreted as the (x,y,z) coordinates of the tag.\n"
     "        If these numbers are present, the tag will be set and displayed.\n"
     "\n"
     "'Write': Write the current set of tags to a '.tag' file, which can\n"
     "         later be read back in using the 'Read' button.\n"
     "   N.B.: Tag coordinates are stored in Dicom order\n"
     "              x = R-L coordinate (R < 0, L > 0)\n"
     "              y = A-P coordinate (A < 0, P > 0)\n"
     "              z = I-S coordinate (I < 0, S > 0)\n"
     "\n"
     "'Copy': Copy the set of tags from another dataset.\n"
     "\n"
     "'Save': Save the current set of tags into the dataset .HEAD file.\n"
     "        If this is not done before 'Quit', then changes made to the\n"
     "        tagset will not be saved.\n"
     "\n"
     "'Done': Combines the operations of 'Save' and then 'Quit'.\n"
     "\n"

     "Row 2 buttons are:\n"
     "-----------------\n"
     "'Set': Sets the selected tag to be at the current crosshair location.\n"
     "\n"
     "'Clear': Un-sets the current tag.\n"
     "\n"
     "'Clr All': Un-sets all tags.\n"
     "\n"
     "'Add': Adds 1 new tag to the end of the list.  There is a maximum of\n"
     "       100 tags allowed.\n"
     "\n"
     "'Delete': Deletes the last tag from the list.\n"
     "\n"
     "'Relabel': Changes the label of the selected tag.\n"
     "\n"

     "Data entry fields are:\n"
     "---------------------\n"
     "'Tag File': You enter the name of the tagset file for 'Read' and 'Write'\n"
     "            operations here.\n"
     "\n"
     "'Tag Label': You enter the new label for the selected tag here, for use\n"
     "             with the 'Relabel' operation.\n"
     "\n"
     "'Tag Value': You enter the numerical value to be attached to a tag here,\n"
     "             for use with the 'Set' operation.\n"
     "\n"

     "Nota Bene\n"
     "---------\n"
     "The tag value is not used for anything yet.  Any suggestions?\n"
     "\n"
     "By using the 'Add', 'Delete', and 'Relabel' buttons, it is possible to\n"
     "create your own collection of tag definitions.  However, it is generally\n"
     "easier to use an external editor (e.g., 'vi') to create a .tag file and\n"
     "then read it in with the 'Read' button.\n"
     "\n"
     "Once you make a change to a tagset, the 'Dataset' button will be disabled\n"
     "until 'Save' is used.  This is a safety measure to help prevent accidental\n"
     "loss of changes.  If you want to discard the changes, use 'Quit'.\n"
     "\n"
     "The color of a dataset tag is controlled by the 'Primary Markers' color,\n"
     "which is set on the 'Define Markers' control panel in the main AFNI control\n"
     "window.  The tags are displayed as a cross within a diamond.\n"
     "\n"
     "The program 3dTagalign will read the tag coordinates from one dataset and\n"
     "rotate/translate that dataset to match the tag coordinates from another\n"
     "dataset (in the least squares sense).  See '3dTagalign -help' for details.\n"
     "\n"
     "============================\n"
     "AUTHOR: RW Cox, October 1998\n"
     "============================\n"

    , TEXT_READONLY ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

#define NMSG 6

static void TAG_beep_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   static int nbeep = 0 ;
   static char * msg[] = { "What did you expect,\na choir of angels?"                ,
                           "Apparently some people\ncan't take a hint."              ,
                           "You aren't tired\nof this game YET?!"                    ,
                           "I'm getting tired of\nthis.  I'm warning you!"           ,
                           "This is the last time!\nOnce more, and you'll be sorry!" ,
                           "You can't say I didn't warn you.\nNyah Nyah Nyah."
                         } ;

   POPUP_MESG( msg[nbeep] ) ; nbeep++ ; BEEPIT ;

   if( nbeep >= NMSG ) SENSITIZE(beep_pb,False) ;
   return ;
}
