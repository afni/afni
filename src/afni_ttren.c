#undef MAIN
#include "afni.h"
#include <Xm/XmAll.h>

#ifndef USE_TALAIRACH_TO
static void TTRR_junkfunc(void){ return; }
#else

#define NUM_AV_FIRST 20

typedef struct {
   int  reg_num , reg_numon ;
   MCW_arrowval *reg_av[TTO_COUNT] ;
   char  *reg_label[TTO_COUNT] ;
   short  reg_tto[TTO_COUNT]   ;
   short  reg_ttbrik[TTO_COUNT] ;
   short  reg_ttval[TTO_COUNT]  ;
   short  reg_ttovc[TTO_COUNT]  ;

   Widget shell , scrollw , workwin ;
   MCW_arrowval *meth_av , *hemi_av ;
   MCW_DC *dc ;
} TTRR_controls ;

static TTRR_controls *ttc = NULL ;

static void TTRR_action_CB       ( Widget, XtPointer, XtPointer ) ;
static void TTRR_delete_window_CB( Widget, XtPointer, XtPointer ) ;

/*----------------------------------------------------------------------------
  Routine to create widgets for the TT atlas rendering controls
------------------------------------------------------------------------------*/

/***** definitions for the action area controls *****/

#define TTRR_quit_label   "Quit"
#define TTRR_clear_label  "Clear"
#define TTRR_apply_label  "Apply"
#define TTRR_set_label    "Set"
#define TTRR_help_label   "Help"

#define TTRR_quit_hint  "Close control panel"
#define TTRR_clear_hint "Set all regions to 'none'"
#define TTRR_apply_hint "Apply colors to AFNI display"
#define TTRR_set_hint   "Apply and Quit"

#define NUM_TTRR_ACT 5

static MCW_action_item TTRR_act[] = {
 { TTRR_quit_label , TTRR_action_CB, NULL,NULL, TTRR_quit_hint , 0 } ,
 { TTRR_clear_label, TTRR_action_CB, NULL,NULL, TTRR_clear_hint, 0 } ,
 { TTRR_apply_label, TTRR_action_CB, NULL,NULL, TTRR_apply_hint, 0 } ,
 { TTRR_set_label  , TTRR_action_CB, NULL,NULL, TTRR_set_hint  , 1 } ,
 { TTRR_help_label , TTRR_action_CB, NULL,NULL, NULL           , 0 }
} ;

#define NMETHOD     5
#define METHOD_OFF "Off"
#define METHOD_GAF "Gyral/Area/Func"
#define METHOD_AGF "Area/Gyral/Func"
#define METHOD_FGA "Func/Gyral/Area"
#define METHOD_FAG "Func/Area/Gyral"

static char *METHOD_strings[NMETHOD] = {
  METHOD_OFF ,  METHOD_GAF ,  METHOD_AGF ,  METHOD_FGA ,  METHOD_FAG
} ;

#define NHEMI       3
#define HEMI_LEFT   "Left only"
#define HEMI_RIGHT  "Right only"
#define HEMI_BOTH   "Both"

static char *HEMI_strings[NHEMI] = { HEMI_LEFT , HEMI_RIGHT , HEMI_BOTH } ;

static char helpstring[] =
  "The purpose of these controls is to enable display of the brain\n"
  "regions defined by the Talairach Daemon database (contributed\n"
  "Jack Lancaster and Peter Fox of RIC UTHSCSA).\n"
  "\n"
  "In the database, some voxels have 2 labels - a larger scale\n"
  "'gyral' name and a finer scale 'area' name; these are marked\n"
  "with [G] and [A] in the region list.\n"
  "In the database there are\n"
  "    1,205,737 voxels with at least one label\n"
  "      709,953 voxels with only a 'gyral' label\n"
  "       15,898 voxels with only a 'area' label\n"
  "      479,886 voxels with both types of labels\n"
  "\n"
  "Method:\n"
  "  To enable display of the selected regions, you must choose the\n"
  "  Method to be something other than 'Off'.  The other Method choices\n"
  "  determine the order in which color overlays take place; for example,\n"
  "  'Gyral/Area/Func' means that a 'gyral' color, if present in a voxel,\n"
  "  will overlay on top of any 'area' color there, which would in turn\n"
  "  overlay on top of any functional color there.\n"
  "\n"
  "Hemisphere(s) \n"
  "  Use this to control which side(s) of the brain will have brain region\n"
  "  overlays\n"
  "\n"
  "The regional controls below are to set the overlay colors; if a region's\n"
  "color is set to 'none', then it will not be overlaid.\n"
  "\n"
  "To reset all overlay colors to 'none', use the Clear button.\n"
  "To actually have the choices you make take effect, use the Apply button.\n"
  "\n"
  "**** AT THIS TIME, THESE CONTROLS DO NOTHING ****\n"
  "-- RWCox - July 2001\n"
;


/*----------------------------------------------------------------------------*/

static void TTRR_setup_widgets( MCW_DC * dc )
{
   XmString xstr ;
   char lbuf[256] ;
   Widget toprc , bar , actar , frame , separator , label ;
   int ww,hh,bww , ii ;

ENTRY("TTRR_setup_widgets") ;

   /**** sanity checks ****/

   if( dc == NULL || ttc != NULL ) EXRETURN ;

   SHOW_AFNI_PAUSE ;

   /**** create output structure ****/

   ttc = myXtNew(TTRR_controls) ; /* will live forever */

   ttc->dc = dc ;

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
                  XmNtraversalOn , False ,
                 XmNinitialResourcesPersistent , False ,
             NULL ) ;

   /**** Label to inform the cretinous user what he's looking at ****/

   xstr = XmStringCreateLtoR("-- Control Talairach Daemon display colors --" ,
                             XmFONTLIST_DEFAULT_TAG ) ;
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
                                   0 , NMETHOD-1 , 0 , 0 ,
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
                                   0 , NHEMI-1 , 0 , 0 ,
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
           XmNtraversalOn , False ,
           XmNinitialResourcesPersistent , False ,
        NULL ) ;

   frame =
      XtVaCreateWidget(
        "AFNI" , xmFrameWidgetClass , ttc->scrollw ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 5 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   ttc->workwin =
         XtVaCreateWidget(
           "AFNI" , xmFormWidgetClass , frame ,
              XmNborderWidth , 0 ,
              XmNborderColor , 0 ,
              XmNtraversalOn , False ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   /** compute information about regions **/

   ttc->reg_num = 0 ;
   for( ii=0 ; ii < TTO_COUNT ; ii++ ){

      if( strncmp(TTO_list[ii].name,"Left  ",6) != 0 ) continue ; /* skip */
      if( TTO_list[ii].tdval == 0 )                    continue ; /* skip */

           if( TTO_list[ii].tdlev == 2 ) strcpy(lbuf,"[G] ") ;
      else if( TTO_list[ii].tdlev == 4 ) strcpy(lbuf,"[A] ") ;
      else                               continue ;               /* skip */

      strcat(lbuf,TTO_list[ii].name+6) ;

      ttc->reg_label [ttc->reg_num] = strdup(lbuf) ;
      ttc->reg_tto   [ttc->reg_num] = ii ;
      ttc->reg_ttbrik[ttc->reg_num] = (TTO_list[ii].tdlev==2) ? 0 : 1 ;
      ttc->reg_ttval [ttc->reg_num] = TTO_list[ii].tdval ;
      ttc->reg_ttovc [ttc->reg_num] = 0 ;

      if( ttc->reg_num <  NUM_AV_FIRST ){
         ttc->reg_av[ttc->reg_num] =
            new_MCW_colormenu(
               ttc->workwin ,                 /* parent */
               ttc->reg_label[ttc->reg_num] , /* label  */
               dc ,                           /* display context */
               0 ,                            /* first color */
               dc->ovc->ncol_ov - 1 ,         /* last color */
               0 ,                            /* initial color */
               NULL,NULL                      /* callback func,data */
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

   XtManageChild( ttc->workwin ) ;
   XtManageChild( frame ) ;
   XtManageChild( ttc->scrollw ) ;
   XtManageChild( toprc ) ;
   XtRealizeWidget( ttc->shell ) ;

   PLUTO_cursorize( ttc->shell ) ;
   XtSetSensitive( ttc->shell , False ) ;
   XmUpdateDisplay( ttc->shell ) ;

   /*** set size ***/

#define LUCK   5
#define CMMAX 17

   MCW_widget_geom( ttc->reg_av[0]->wrowcol , &ww , &hh , NULL,NULL ) ;

   XtVaGetValues( ttc->scrollw , XmNverticalScrollBar , &bar , NULL ) ;
   MCW_widget_geom( bar , &bww , NULL,NULL,NULL ) ;

   hh  = CMMAX*hh + LUCK ;
   ww += bww + 5*LUCK ;

   MCW_widget_geom( ttc->meth_av->wrowcol , &ii  , NULL,NULL,NULL ) ;
   MCW_widget_geom( ttc->hemi_av->wrowcol , &bww , NULL,NULL,NULL ) ;
   bww += ii + LUCK ;
   if( ww < bww ) ww = bww ;

   XtVaSetValues( ttc->shell , XmNwidth , ww , XmNheight , hh , NULL ) ;
   XmUpdateDisplay( ttc->shell ) ;

   /*** do rest of widgets now ***/

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

      XtRealizeWidget( ttc->reg_av[ii]->wrowcol ) ;
      if( ii%5 == 0 ) XmUpdateDisplay( ttc->shell ) ;
   }

   XtSetSensitive( ttc->shell , True ) ;
   XmUpdateDisplay( ttc->shell ) ;

   /*** done!!! ***/

   SHOW_AFNI_READY ; EXRETURN ;
}

/*------------------------------------------------------------------------*/

void TTRR_popup( MCW_DC * dc )
{
ENTRY("TTRR_popup") ;

   if( ttc == NULL ) TTRR_setup_widgets( dc ) ;
   XtMapWidget( ttc->shell ) ;
   /** RWC_visibilize_widget( ttc->shell ) ; **/
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

static void TTRR_action_CB( Widget w , XtPointer cd , XtPointer cbs )
{
   char * wname = XtName(w) ;
   int ii ;

ENTRY("TTRR_action_CB") ;

   if( strcmp(wname,TTRR_help_label) == 0 ){

      new_MCW_textwin( w , helpstring , TEXT_READONLY ) ;

   } else if( strcmp(wname,TTRR_quit_label) == 0 ){

      TTRR_delete_window_CB(NULL,NULL,NULL) ;

   } else if( strcmp(wname,TTRR_clear_label) == 0 ){

      for( ii=0 ; ii < ttc->reg_num ; ii++ ){
         if( ttc->reg_av[ii]->ival != 0 )
            AV_assign_ival( ttc->reg_av[ii] , 0 ) ;
      }

   } else if( strcmp(wname,TTRR_apply_label) == 0 ||
              strcmp(wname,TTRR_set_label  ) == 0   ){

      ttc->reg_numon = 0 ;
      for( ii=0 ; ii < ttc->reg_num ; ii++ ){
         ttc->reg_ttovc[ii] = ttc->reg_av[ii]->ival ;
         if( ttc->reg_ttovc[ii] ) ttc->reg_numon ++ ;
      }

      fprintf(stderr,"** TTRR has %d turned on **\n",ttc->reg_numon) ;

      if( strcmp(wname,TTRR_set_label) == 0 )
         TTRR_delete_window_CB(NULL,NULL,NULL) ;
   }

   EXRETURN ;
}

/*------------------------------------------------------------------------
   What happens when the user selects "Close" from the window
   menu in a plugin interface menu window.
--------------------------------------------------------------------------*/

static void TTRR_delete_window_CB( Widget w , XtPointer cd , XtPointer cbs )
{
ENTRY("TTRR_delete_window_CB") ;

   if( ttc != NULL ){
      XtUnmapWidget(ttc->shell) ;
      XmUpdateDisplay(ttc->shell) ;
   }
   EXRETURN ;
}
#endif /* USE_TALAIRACH_TO */
