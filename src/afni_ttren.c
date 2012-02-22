#undef MAIN
#include "afni.h"
#include <Xm/XmAll.h>

#define NUM_AV_FIRST 20  /* number of colormenus to create on first pass */

/*-- internal data structure --*/

typedef struct {
   int  reg_num , av_invert ;
   MCW_arrowval **reg_av ;  /* colormenus */
   char  **reg_label ;      /* labels for menus */
   short  *reg_tto   ;      /* index into afni.h TTO_list */
   short  *reg_ttbrik ;     /* which sub-brick in TTatlas+tlrc */
   short  *reg_ttval  ;     /* what value in TTatlas+tlrc */
   short  *reg_ttovc  ;     /* saved value of colormenu */

   Widget shell , scrollw , workwin ;
   MCW_arrowval *meth_av , *hemi_av ;

   Three_D_View * im3d ;
   MCW_DC *dc ;
} TTRR_controls ;

static TTRR_controls *ttc = NULL ;

/*-- prototypes for internal functions --*/
static void TTRR_setup_widgets( MCW_DC * dc );
static void TTRR_action_CB       ( Widget, XtPointer, XtPointer ) ;
static void TTRR_delete_window_CB( Widget, XtPointer, XtPointer ) ;
static void TTRR_av_CB           ( MCW_arrowval * , XtPointer   ) ;

static void TTRR_load_file( char * ) ;                             /* 08 Aug 2002 */
static void TTRR_save_CB  ( Widget , XtPointer , MCW_choose_cbs * ) ;
static void TTRR_load_CB  ( Widget , XtPointer , MCW_choose_cbs * ) ;
static void fill_str(char * instr, char ch, int maxlength);
/*----------------------------------------------------------------------------
  Routine to create widgets for the TT atlas rendering controls
------------------------------------------------------------------------------*/

/***** definitions for the action area controls *****/

#define TTRR_clear_label  "Clear"
#define TTRR_load_label   "Load"
#define TTRR_save_label   "Save"
#define TTRR_redraw_label "Redraw"
#define TTRR_done_label   "Done"
#define TTRR_help_label   "Help"

#define TTRR_clear_hint  "Set all colors to 'none'"
#define TTRR_load_hint   "Load colors from a file"
#define TTRR_save_hint   "Save colors to a file"
#define TTRR_redraw_hint "Redraw using current colors"
#define TTRR_done_hint   "Close this window"

#define NUM_TTRR_ACT 6

static MCW_action_item TTRR_act[] = {
 { TTRR_clear_label , TTRR_action_CB, NULL,NULL, TTRR_clear_hint , 0 } ,
 { TTRR_load_label  , TTRR_action_CB, NULL,NULL, TTRR_load_hint  , 0 } ,
 { TTRR_save_label  , TTRR_action_CB, NULL,NULL, TTRR_save_hint  , 0 } ,
 { TTRR_redraw_label, TTRR_action_CB, NULL,NULL, TTRR_redraw_hint,-1 } ,
 { TTRR_done_label  , TTRR_action_CB, NULL,NULL, TTRR_done_hint  , 1 } ,
 { TTRR_help_label  , TTRR_action_CB, NULL,NULL, NULL            , 0 }
} ;

#define NMETHOD     5
#define METHOD_OFF "Off"
#define METHOD_GAF "First atlas then Overlay"
#define METHOD_AGF "Last atlas then Overlay"
#define METHOD_FGA "Overlay then first atlas"
#define METHOD_FAG "Overlay then last atlas"

static char *METHOD_strings[NMETHOD] = {
  METHOD_OFF ,  METHOD_GAF ,  METHOD_AGF ,  METHOD_FGA ,  METHOD_FAG
} ;

#define NHEMI       3
#define HEMI_LEFT   "Left only"
#define HEMI_RIGHT  "Right only"
#define HEMI_BOTH   "Both"

static char *HEMI_strings[NHEMI] = { HEMI_LEFT , HEMI_RIGHT , HEMI_BOTH } ;

static char helpstring[] =
 "The default list includes the original Talairach Daemon database\n"
 "(kindly provided by Jack Lancaster and Peter Fox of RIC UTHSCSA,\n"
 "the cytoarchitectonic and macrolabel atlases provided by Simon\n"
 "Eickhoff and Karl Zilles, and the probabilistic atlases provided\n"
 "Rutvik Desai.\n"
 "\n"
 "In the database, voxels may have multiple labels for a particular\n"
 "atlas. For example, the voxels may a larger scale 'gyral' [G] name\n"
 "and a finer scale 'area' [A] name. A list of all the labels for the\n"
 "principal default atlas is presented here or from the command\n"
 "line program, 'whereami -show_atlas_code', for any atlas.\n"
  "\n"
  "Method:\n"
  "  To enable display of the selected regions, you must choose the\n"
  "  Method to be something other than 'Off'.  The other Method choices\n"
  "  determine the order in which color overlays take place. At this\n"
  "  time, there is no way to blend the colors from overlapping results.\n"
  "\n"
  "Hemisphere(s):\n"
  "  Use this to control which side(s) of the brain will have brain\n"
  "  region overlays.  At this time, this option only affects the volume\n"
  "  rendering and has no effect on the 2D image viewers, in which\n"
  "  regions from both hemispheres will be rendered, regardless.\n"
  "\n"
  "The regional controls are to set the overlay colors; if a region's\n"
  "color is set to 'none', then it will not be overlaid.\n"
  "\n"
  "* To change all overlay colors to 'none', use the Clear button.\n"
  "* To save the color settings to a file, use the Save button.\n"
  "* To read saved color settings from a file, use the Load button.\n"
  "* Set environment variable AFNI_TTRR_SETUP to the name of a Save\n"
  "    color file, and it will be loaded when you first create this\n"
  "    control panel.  See README.environment for more details.\n"
  "* The Done button closes the control panel, but doesn't change colors.\n"
  "\n"
  "NOTES:\n"
  " * At this time, the Redraw button has no functionality;\n"
  "     after you change the color settings in this window, you must\n"
  "     force an image redisplay to see the changes.  In the 2D image\n"
  "     viewers, you can do this by turning 'See TT Atlas Regions'\n"
  "     off and on;  in the volume renderer, you must press the 'Reload'\n"
  "     button to force the proper redisplay ('Draw' isn't enough).\n"
  " * The region rendering only works if the dataset being drawn in the\n"
  "     2D image viewers and/or Render Dataset plugin is in the +tlrc\n"
  "     coordinates sytem. The atlas regions are resample to the underlay\n"
  "     resolution.\n"
  " * The atlas is only useful as a ROUGH guide to determining where you\n"
  "     are in any individual brain.  Do not rely exclusively on the Atlas\n"
  "     for brain region labeling: you must use your knowledge, skills,\n"
  "     and abilities as well.\n"
  "\n"
  "-- RWCox - July 2001\n"
;

/*----------------------------------------------------------------------------*/
TTRR_controls * New_TTRR_controls(char *atname) 
{
   TTRR_controls * ttlc = NULL;
   int i = 0, tto_count = 0;
   
   if (!atname) return(ttlc);
   if ((tto_count = atlas_n_points(atname)) <= 0) return(ttlc);
   
   ttlc = myXtNew(TTRR_controls) ;
   memset(ttlc, 0, sizeof(TTRR_controls));
   
   ttlc->reg_av = (MCW_arrowval **)calloc(tto_count,sizeof(MCW_arrowval *));
   ttlc->reg_label = (char **)calloc(tto_count,sizeof(char *));
   ttlc->reg_tto = (short *)calloc(tto_count,sizeof(short));
   ttlc->reg_ttbrik = (short *)calloc(tto_count,sizeof(short));
   ttlc->reg_ttval = (short *)calloc(tto_count,sizeof(short));
   ttlc->reg_ttovc = (short *)calloc(tto_count,sizeof(short));
   return(ttlc);
}

void Free_TTRR_controls(TTRR_controls *ttlc)
{
   if(!ttlc) return;
   free(ttlc->reg_av);
   free(ttlc->reg_label);
   free(ttlc->reg_tto);
   free(ttlc->reg_ttbrik);
   free(ttlc->reg_ttval);
   free(ttlc->reg_ttovc);
   free(ttlc);
}


void TTRR_resetup()
{
   MCW_DC *dc;
   if(!ttc) return;  /* hasn't been setup yet, just wait for setup  */
   dc= ttc->dc;
   Free_TTRR_controls(ttc);
   ttc = NULL;
/*   TTRR_setup_widgets(dc);*/
}


static void TTRR_setup_widgets( MCW_DC * dc )
{
   XmString xstr ;
   char lbuf[256] , *ept, TTRR_title[256] ;
   Widget toprc , bar=NULL , actar , frame , separator , label ;
   int ww,hh,bww , ii, n_points, maxlength, levelmark ;
   ATLAS_POINT *tto_list=NULL;
   
ENTRY("TTRR_setup_widgets") ;

   /**** sanity checks ****/

   if( dc == NULL || ttc != NULL ) EXRETURN ;   /* might be better to check ttc->dc */
                                                
   SHOW_AFNI_PAUSE ;

   if (!(tto_list = atlas_points(Current_Atlas_Default_Name()))) {
      ERROR_message("No atlas points!");
      EXRETURN ;
   }

   /**** create output structure ****/

   ttc = New_TTRR_controls(Current_Atlas_Default_Name()); /* will live forever */

   ttc->dc = dc ;

   /* if this is YES, inverts the whole line that contains the structure */
   ttc->av_invert = AFNI_yesenv( "AFNI_TTRR_INVERT" ) ; 

   /**** create Shell that can be opened up later ****/

   ttc->shell =
      XtVaAppCreateShell(
           "AFNI" , "AFNI" , topLevelShellWidgetClass , dc->display ,

           XmNtitle             , "TT Atlas Rendering" , /* top of window */
           XmNiconName          , "TT Atlas"           , /* label on icon */
#if 0
           XmNmappedWhenManaged , False ,                /* must map it manually */
#endif
           XmNdeleteResponse    , XmDO_NOTHING ,         /* deletion handled below */
           XmNallowShellResize  , False ,                /* let code resize shell? */
           XmNinitialResourcesPersistent , False ,
              XmNkeyboardFocusPolicy , XmEXPLICIT ,
      NULL ) ;

   DC_yokify( ttc->shell , dc ) ;

   if( afni48_good )
      XtVaSetValues( ttc->shell ,
                        XmNiconPixmap , afni48_pixmap ,
                     NULL ) ;

   XmAddWMProtocolCallback(           /* make "Close" window menu work */
           ttc->shell ,
           XmInternAtom( dc->display , "WM_DELETE_WINDOW" , False ) ,
           TTRR_delete_window_CB , (XtPointer) ttc ) ;

   /**** create Form to hold all widgets ****/

   toprc = XtVaCreateWidget(
             "AFNI" , xmFormWidgetClass , ttc->shell ,
                 XmNborderWidth , 0 ,
                 XmNborderColor , 0 ,
                  XmNtraversalOn , True  ,
                 XmNinitialResourcesPersistent , False ,
             NULL ) ;

   /**** Label to inform the cretinous user what he's looking at ****/
   sprintf(TTRR_title, "-- Control atlas: %s colors --",Current_Atlas_Default_Name());

   xstr = XmStringCreateLtoR( TTRR_title, XmFONTLIST_DEFAULT_TAG ) ;
   label = XtVaCreateManagedWidget(
             "AFNI" , xmLabelWidgetClass ,  toprc ,
                XmNlabelString , xstr ,
                XmNalignment  , XmALIGNMENT_CENTER ,

                XmNleftAttachment , XmATTACH_FORM ,
                XmNrightAttachment, XmATTACH_FORM ,
                XmNtopAttachment  , XmATTACH_FORM ,
                XmNtopOffset      , 3 ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;
   XmStringFree( xstr ) ;

   MCW_widget_geom( label , &ww , &hh , NULL , NULL ) ; /* temporary */
   XtVaSetValues( ttc->shell ,
                      XmNminWidth  , ww+3  ,
                      XmNminHeight , 11*hh ,
                  NULL ) ;

   separator = XtVaCreateManagedWidget(
                 "AFNI" , xmSeparatorWidgetClass , toprc ,
                    XmNseparatorType  , XmSHADOW_ETCHED_IN ,
                    XmNshadowThickness, 2 ,
                    XmNleftAttachment , XmATTACH_FORM ,
                    XmNrightAttachment, XmATTACH_FORM ,
                    XmNtopAttachment  , XmATTACH_WIDGET ,
                    XmNtopWidget      , label ,
                    XmNtopOffset      , 1 ,
                 NULL ) ;

   /**** create an action area to hold user control buttons ****/

   for( ii=0 ; ii < NUM_TTRR_ACT ; ii++ )
      TTRR_act[ii].data = (XtPointer) ttc ;

   actar = MCW_action_area( toprc , TTRR_act , NUM_TTRR_ACT ) ;

   XtVaSetValues( actar ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNrightAttachment, XmATTACH_FORM ,
                     XmNtopAttachment  , XmATTACH_WIDGET ,
                     XmNtopWidget      , separator ,
                     XmNtopOffset      , 3 ,
                  NULL ) ;

   separator = XtVaCreateManagedWidget(
                 "AFNI" , xmSeparatorWidgetClass , toprc ,
                    XmNseparatorType  , XmSHADOW_ETCHED_IN ,
                    XmNshadowThickness, 2 ,
                    XmNleftAttachment , XmATTACH_FORM ,
                    XmNrightAttachment, XmATTACH_FORM ,
                    XmNtopAttachment  , XmATTACH_WIDGET ,
                    XmNtopWidget      , actar ,
                    XmNtopOffset      , 1 ,
                 NULL ) ;

   /**** a couple of buttons to control operational settings ****/

   ttc->meth_av = new_MCW_optmenu( toprc , "Method" ,
                                   0 , NMETHOD-1 , NMETHOD-1 , 0 ,
                                   NULL,NULL ,
                                   MCW_av_substring_CB, METHOD_strings ) ;

   XtVaSetValues( ttc->meth_av->wrowcol ,
                    XmNleftAttachment , XmATTACH_FORM ,
                    XmNtopAttachment  , XmATTACH_WIDGET ,
                    XmNtopWidget      , separator ,
                    XmNtopOffset      , 3 ,
                    XmNleftOffset     , 3 ,
                 NULL ) ;

   ttc->hemi_av = new_MCW_optmenu( toprc , "Hemisphere(s)" ,
                                   0 , NHEMI-1 , NHEMI-1 , 0 ,
                                   NULL,NULL ,
                                   MCW_av_substring_CB, HEMI_strings ) ;

   XtVaSetValues( ttc->hemi_av->wrowcol ,
                    XmNrightAttachment, XmATTACH_FORM ,
                    XmNtopAttachment  , XmATTACH_WIDGET ,
                    XmNtopWidget      , separator ,
                    XmNtopOffset      , 3 ,
                    XmNrightOffset    , 3 ,
                 NULL ) ;

   separator = XtVaCreateManagedWidget(
                 "AFNI" , xmSeparatorWidgetClass , toprc ,
                    XmNseparatorType  , XmSHADOW_ETCHED_IN ,
                    XmNshadowThickness, 2 ,
                    XmNleftAttachment , XmATTACH_FORM ,
                    XmNrightAttachment, XmATTACH_FORM ,
                    XmNtopAttachment  , XmATTACH_WIDGET ,
                    XmNtopWidget      , ttc->meth_av->wrowcol ,
                    XmNtopOffset      , 1 ,
                 NULL ) ;

   /**** create a Scrolled Window and Form to hold colormenus ****/

   ttc->scrollw =
      XtVaCreateWidget(
        "AFNI" , xmScrolledWindowWidgetClass ,  toprc ,
           XmNscrollingPolicy , XmAUTOMATIC ,
           XmNwidth  , ww+2*hh ,                /* temporary */
           XmNheight ,    3*hh ,                /* ditto     */
           XmNleftAttachment  , XmATTACH_FORM ,
           XmNrightAttachment , XmATTACH_FORM ,
           XmNtopAttachment   , XmATTACH_WIDGET ,
           XmNbottomAttachment, XmATTACH_FORM ,
           XmNtopWidget       , separator ,
           XmNtopOffset       , 3 ,
           XmNtraversalOn , True  ,
           XmNinitialResourcesPersistent , False ,
        NULL ) ;

   frame =
      XtVaCreateWidget(
        "AFNI" , xmFrameWidgetClass , ttc->scrollw ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 5 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   ttc->workwin =
         XtVaCreateWidget(
           "AFNI" , xmFormWidgetClass , frame ,
              XmNborderWidth , 0 ,
              XmNborderColor , 0 ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   /** compute information about regions **/
   n_points =  atlas_n_points(Current_Atlas_Default_Name());
   maxlength = atlas_max_label_length(tto_list, n_points); /*maximum length of name */
   levelmark = atlas_level(tto_list, n_points);   /* any structures marked with a level */
   if(levelmark)
      maxlength = maxlength+4; /*allow for [G]  or [A] in string */

   if(maxlength<20) maxlength = 20;  /* looks better with some trailing ....*/

   ttc->reg_num = 0 ;
   for( ii=0 ; ii < n_points ; ii++ ){
      /*if( tto_list[ii].tdval == 0 )    continue ; */ /* skip */

           if( tto_list[ii].tdlev == 2 ) strcpy(lbuf,"[G] ") ;
      else if( tto_list[ii].tdlev == 4 ) strcpy(lbuf,"[A] ") ;
      if((tto_list[ii].tdlev == 2)||(tto_list[ii].tdlev == 4)) {
          strcat(lbuf,tto_list[ii].name) ;
      }
      else {
          strcpy(lbuf,tto_list[ii].name) ;
      }

      fill_str(lbuf,'.', maxlength);   /* add periods up to a length of maxlength*/

      ttc->reg_label [ttc->reg_num] = strdup(lbuf) ;
      ttc->reg_tto   [ttc->reg_num] = ii ;
      ttc->reg_ttbrik[ttc->reg_num] = (tto_list[ii].tdlev==2) ? 0 : 1 ;
      ttc->reg_ttval [ttc->reg_num] = tto_list[ii].tdval ;
      ttc->reg_ttovc [ttc->reg_num] = 0 ;

      /* only create a few colormenu widgets first,
         because XtRealizeWidget() is so slow with many widgets,
         and the impatient user is likely to be unhappy with us  */

      if( ttc->reg_num < NUM_AV_FIRST ){
         ttc->reg_av[ttc->reg_num] =
            new_MCW_colormenu(
               ttc->workwin ,                 /* parent */
               ttc->reg_label[ttc->reg_num] , /* label  */
               dc ,                           /* display context */
               0 ,                            /* first color */
               dc->ovc->ncol_ov - 1 ,         /* last color */
               0 ,                            /* initial color */
               TTRR_av_CB,NULL                /* callback func,data */
            ) ;

         XtVaSetValues( ttc->reg_av[ttc->reg_num]->wrowcol ,
                         XmNleftAttachment   , XmATTACH_FORM ,
                         XmNtopAttachment    , (ttc->reg_num==0)
                                                ? XmATTACH_FORM     /* 1st row */
                                                : XmATTACH_WIDGET , /* 2nd+ row */
                         XmNtopWidget        , (ttc->reg_num==0)
                                                ? NULL
                                                : ttc->reg_av[ttc->reg_num-1]->wrowcol ,
                         XmNrightAttachment  , XmATTACH_FORM ,
                        NULL ) ;
      }

      ttc->reg_num++ ;
   }

   /* manage the managers */

   XtManageChild( ttc->workwin ) ;
   XtManageChild( frame ) ;
   XtManageChild( ttc->scrollw ) ;
   XtManageChild( toprc ) ;
   XtRealizeWidget( ttc->shell ) ; NI_sleep(5) ;

   WATCH_cursorize( ttc->shell ) ;
   XmUpdateDisplay( ttc->shell ) ;

   /*** set size ***/

#define LUCK   5  /* we all need some */
#define CMMAX 17  /* vertical size = CMMAX colormenus high */
   MCW_widget_geom( ttc->reg_av[0]->wrowcol , &ww , &hh , NULL,NULL ) ;

   XtVaGetValues( ttc->scrollw , XmNverticalScrollBar , &bar , NULL ) ;
   MCW_widget_geom( bar , &bww , NULL,NULL,NULL ) ;

   hh  = CMMAX*hh + LUCK ;
   ww += bww + 5*LUCK ;

   /* but make sure window is at least wide
      enough for the Method and Hemisphere(s) widgets */
   MCW_widget_geom( ttc->meth_av->wrowcol , &ii  , NULL,NULL,NULL ) ;
   MCW_widget_geom( ttc->hemi_av->wrowcol , &bww , NULL,NULL,NULL ) ;
   bww += ii + LUCK ;
   if( ww < bww ) ww = bww ;

   XtVaSetValues( ttc->shell , XmNwidth , ww , XmNheight , hh , NULL ) ;
   XmUpdateDisplay( ttc->shell ) ;

   /*** create rest of colormenu widgets now
        -- this provides some visual feedback, and keeps the user happy ***/

   for( ii=NUM_AV_FIRST ; ii < ttc->reg_num ; ii++ ){
      ttc->reg_av[ii] =
         new_MCW_colormenu(
            ttc->workwin ,                 /* parent */
            ttc->reg_label[ii] ,           /* label  */
            dc ,                           /* display context */
            0 ,                            /* first color */
            dc->ovc->ncol_ov - 1 ,         /* last color */
            0 ,                            /* initial color */
            NULL,NULL                      /* callback func,data */
         ) ;

      XtVaSetValues( ttc->reg_av[ii]->wrowcol ,
                      XmNleftAttachment   , XmATTACH_FORM ,
                      XmNtopAttachment    , XmATTACH_WIDGET ,
                      XmNtopWidget        , ttc->reg_av[ii-1]->wrowcol ,
                      XmNrightAttachment  , XmATTACH_FORM ,
                     NULL ) ;

      XtRealizeWidget( ttc->reg_av[ii]->wrowcol ) ; NI_sleep(1) ;

      if( ii%NUM_AV_FIRST == 0 )          /* show something occasionally */
         XmUpdateDisplay( ttc->shell ) ;
   }

   PLUTO_cursorize( ttc->shell ) ;

   /* 08 Aug 2002: read initial colors */

   ept = getenv( "AFNI_TTRR_SETUP" ) ;
   if( ept != NULL ) TTRR_load_file( ept ) ;

   /*** done!!! ***/
   SHOW_AFNI_READY ; EXRETURN ;
}

/*-----------------------------------------------------------------------
   Called to actually see the damn thing
-------------------------------------------------------------------------*/

void TTRR_popup( Three_D_View * im3d )
{
ENTRY("TTRR_popup") ;

   if( ttc == NULL ) TTRR_setup_widgets( im3d->dc ) ;

   ttc->im3d = im3d ;
   XtMapWidget( ttc->shell ) ;

   EXRETURN ;
}

/*------------------------------------------------------------------------
   What happens when a colormenu item is selected
--------------------------------------------------------------------------*/

static void TTRR_av_CB( MCW_arrowval * av , XtPointer cd )
{
   if( !ttc->av_invert || av == NULL || av->ival == av->old_ival ) return ;

   if( av->ival == 0 ||
       (av->ival != 0 && av->old_ival == 0) ) MCW_invert_widget(av->wrowcol);

   return ;
}

/*------------------------------------------------------------------------
   What happens when an action button is pressed
--------------------------------------------------------------------------*/

static void TTRR_action_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   char * wname = XtName(w) ;
   int ii ;

ENTRY("TTRR_action_CB") ;

   if( strcmp(wname,TTRR_help_label) == 0 ){

      new_MCW_textwin( w , helpstring , TEXT_READONLY ) ;

   } else if( strcmp(wname,TTRR_done_label) == 0 ){

      TTRR_delete_window_CB(NULL,NULL,NULL) ;

   } else if( strcmp(wname,TTRR_clear_label) == 0 ){

      /* restore colormenus to 'none' status */

      for( ii=0 ; ii < ttc->reg_num ; ii++ ){
         if( ttc->reg_av[ii]->ival != 0 ){
            AV_assign_ival( ttc->reg_av[ii] , 0 ) ;

            if( ttc->av_invert && ttc->reg_av[ii]->old_ival != 0 )
               MCW_invert_widget(ttc->reg_av[ii]->wrowcol);
         }
      }

   } else if( strcmp(wname,TTRR_redraw_label) == 0 ){

      BEEPIT ;

   } else if( strcmp(wname,TTRR_load_label) == 0 ){

      MCW_choose_string( w , "Filename to load" , NULL ,
                             TTRR_load_CB , NULL ) ;

   } else if( strcmp(wname,TTRR_save_label) == 0 ){

      MCW_choose_string( w , "Filename to save" , NULL ,
                             TTRR_save_CB , NULL ) ;
   }

   EXRETURN ;
}

/*------------------------------------------------------------------------
   What happens when the user selects "Close" from the window
   menu in a plugin interface menu window
--------------------------------------------------------------------------*/

static void TTRR_delete_window_CB( Widget w , XtPointer cd , XtPointer cbs )
{
ENTRY("TTRR_delete_window_CB") ;

   if( ttc != NULL ){
      XtUnmapWidget(ttc->shell) ;   /* just hide the window */
      XmUpdateDisplay(ttc->shell) ; /* (it's too hard to re-create) */
   }
   EXRETURN ;
}

/*------------------------------------------------------------------------
   Return the current state of the TT atlas colors in a static
   struct (i.e., do NOT free() this!).
--------------------------------------------------------------------------*/

static TTRR_params *ttp = NULL ;

TTRR_params * TTRR_get_params(void)
{
   int ii,jj ;

ENTRY("TTRR_get_params") ;

   if( ttc == NULL ) RETURN(NULL) ;  /* report nothing */

   if( ttc->meth_av->ival == TTRR_METH_OFF ) RETURN(NULL) ;

   /* 1st time in: make reporting struct */

   if( ttp == NULL ){
      ttp = myXtNew(TTRR_params) ;
      ttp->ttbrik = (byte *) malloc(sizeof(byte)*ttc->reg_num) ;
      ttp->ttval  = (short *) malloc(sizeof(short)*ttc->reg_num) ;
      ttp->ttovc  = (byte *) malloc(sizeof(byte)*ttc->reg_num) ;
   }

   /* set method codes */

   ttp->meth = ttc->meth_av->ival ;
   ttp->hemi = ttc->hemi_av->ival ;  /* hemisphere */

   /* make list of all 'on' regions */

   for( ii=jj=0 ; ii < ttc->reg_num ; ii++ ){
      ttc->reg_ttovc[ii] = ttc->reg_av[ii]->ival ;
      if( ttc->reg_ttovc[ii] > 0 ){
         ttp->ttbrik[jj] = (byte) ttc->reg_ttbrik[ii] ;
         ttp->ttval [jj] = (short) ttc->reg_ttval [ii] ;
         ttp->ttovc [jj] = (byte) ttc->reg_ttovc [ii] ;
         jj++ ;
      }
   }

   ttp->num = jj ;  /* number of 'on' regions */
   RETURN(ttp) ;
}

/*----------------------------------------------------------------------*/

static void TTRR_load_file( char * fname )  /* 08 Aug 2002 */
{
  FILE *fp = fopen(fname,"r") ;

#define NLBUF 1024
  if( fp != NULL ){
    char lbuf[NLBUF], **stok , *name, *color, *ept ;
    int ns , ic , ii ;

    while(1){
      ept = afni_fgets( lbuf , NLBUF , fp ) ;         /* get line */
      if( ept == NULL ) break ;                    /* end of file */
      stok = NULL ;
      ns = breakup_string( lbuf , &stok ) ;        /* break it up */
      if( ns <= 0 || stok == NULL ) continue ;            /* skip */
      if( ns == 1 ){ freeup_strings(ns,stok); continue; } /* skip */
      if( stok[0][0] == '#' ||
          (stok[0][0] == '/' && stok[0][1] == '/') )
                   { freeup_strings(ns,stok); continue; } /* skip */
      name = stok[0] ;                             /* region name */
      if( ns == 2 ) color = stok[1] ;       /* overlay color name */
      else          color = stok[2] ;
      ic = DC_find_overlay_color( ttc->dc , color ) ;
      if( ic < 0 ){ freeup_strings(ns,stok); continue; } /* skip */

      /* find region name in list; assign color to menu */

      for( ii=0 ; ii < ttc->reg_num ; ii++ ){
        if( ig_strstr( ttc->reg_label[ii], name, "._ " ) != NULL ){
          AV_assign_ival( ttc->reg_av[ii] , ic ) ;
        }
      }

      freeup_strings(ns,stok) ;
    }

    fclose(fp) ;  /* done with file */
  }

  return ;
}

/*------------------------------------------------------------------*/


static void TTRR_load_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   if( cbs->reason != mcwCR_string ||
       cbs->cval == NULL           || strlen(cbs->cval) == 0 ){

      PLUTO_beep() ; return ;
   }

   if( !THD_is_file(cbs->cval) ){ PLUTO_beep(); return; }

   TTRR_load_file( cbs->cval ) ; return ;
}

/*------------------------------------------------------------------*/

static void TTRR_save_CB( Widget w , XtPointer cd , MCW_choose_cbs * cbs )
{
   int ii , qq , jj ;
   FILE *fp ;
   char name[128] , *color ;

   if( cbs->reason != mcwCR_string ||
       cbs->cval == NULL           || strlen(cbs->cval) == 0 ){

      PLUTO_beep() ; return ;
   }

   fp = fopen( cbs->cval , "w" ) ;
   if( fp == NULL ){ PLUTO_beep(); return; }

   for( ii=0 ; ii < ttc->reg_num ; ii++ ){
     color = ttc->dc->ovc->label_ov[ttc->reg_av[ii]->ival] ;
     qq = (ttc->reg_label[ii][0] == '[') ? 4 : 0 ;
     strcpy(name,ttc->reg_label[ii]+qq) ;
     qq = strlen(name) ;
     for( jj=0 ; jj < qq ; jj++ ){
            if( name[jj] == '.'   ) name[jj] = ' ' ;
       else if( name[jj] == ' '   ) name[jj] = '_' ;
     }
     fprintf(fp, "%s = %s\n",name,color) ;
   }
   fclose(fp) ; return ;
}

/* add periods up to a length of maxlength*/
static void
fill_str(char * instr, char ch, int maxlength)
{
   int i;

   for(i=strlen(instr); i<maxlength;i++)
      instr[i] = ch;  /* replace characters in place in original string */
   instr[maxlength] = '\0';
}
