/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#undef MAIN
#define WANT_LOGO_BITMAP
#define WANT_AFNI_BITMAP
#undef  USE_IMPIX

#include "afni.h"

/** if USE_OPTMENUS is defined, then option menus will
    be used in place of MCW_arrowvals wherever possible **/

#ifndef DONT_USE_OPTMENUS
#  ifndef USE_OPTMENUS
#    define USE_OPTMENUS
#  endif
#endif
#define COLSIZE AV_colsize()

#ifdef USE_OPTMENUS
#  define AVOPT_STYLE MCW_AV_optmenu
#else
#  define AVOPT_STYLE MCW_AV_downup
#endif

#ifdef AFNI_DEBUG
#  define REPORT_PROGRESS(str)  /* nada */
#else
#  define REPORT_PROGRESS(str)  \
    do{ if(AFNI_VERBOSE){printf(str);fflush(stdout);} } while(0)
#endif

static int num_entry = 0 ;  /* 31 Aug 1999 */

#define REFRESH         \
  if( num_entry == 1 ){ \
     XmUpdateDisplay(im3d->vwid->top_shell); REPORT_PROGRESS("."); }

/*---------------------------------------------------------------------
   Make all the rest of the widgets for a Three_D_View
   (after the toplevel shell has been created)
-----------------------------------------------------------------------*/

static char * AFNI_dummy_av_label[2] = { "Nothing 1" , "Nothing 2" } ;

static char * AFNI_crosshair_av_label[9] = {  /* modified 31 Dec 1998 */
    "Off"   , "Single" , "Multi" ,
    " LR+AP", " LR+IS" , " AP+IS",
    "  LR"  , "  AP"   , "  IS"    } ;

static char * AFNI_see_marks_bbox_label[1] = { "See Markers" } ;

static char * AFNI_see_func_bbox_label[1] = { "See Function" } ;

static char * AFNI_wrap_bbox_label[1] = {"Wrap"} ;
static char * AFNI_xhall_bbox_label[1] = {"X+"} ;

static char * AFNI_marks_edits_bbox_label[1] = { "Allow edits" } ;

static char * AFNI_range_bbox_label[1] = { "autoRange:xxxxxxxxx" } ;

static char * AFNI_inten_bbox_label[1] = { "Pos?" } ;

static char * AFNI_tlrc_big_bbox_label[1] = { "Big Talairach Box?" } ;

#define AFNI_tlrc_big_bbox_help                                       \
   "pressed IN:  Uses a larger 'bounding box' when creating\n"        \
   "             the Talairach view; this extends 10 mm more\n"       \
   "             inferior than the old box, which will encompass\n"   \
   "             all of the cerebellum and more of the brainstem.\n"  \
   "\n"                                                               \
   "pressed OUT: Uses the old 'bounding box' size when creating\n"    \
   "             the Talairach view; this is for compatibility\n"     \
   "             with BRIKs created with older versions of AFNI.\n"   \
   "\n"                                                               \
   "Nota Bene:   The state of this button is only used by AFNI at\n"  \
   "             moment the 'Transform Data' button is used to\n"     \
   "             transform from the AC-PC aligned view to the\n"      \
   "             Talairach view."

static char * AFNI_anatmode_bbox_label[2] =
   { "View Anat Data Brick" , "Warp Anat on Demand" } ;

static char * AFNI_funcmode_bbox_label[2] =
   { "View Func Data Brick" , "Warp Func on Demand" } ;

#define AFNI_see_marks_bbox_help                       \
   "pressed IN:  markers for this view will display\n" \
   "pressed OUT: markers for this view won't display\n"\
   "\n"                                                \
   "Markers are used to specify anatomical locations\n"\
   "required for transformation of coordinates.\n"     \
   "\n"                                                \
   "Oct 1998: Also controls the display of dataset\n"  \
   "          'tags' -- see the 'Edit Tagset' plugin."

#define AFNI_see_func_bbox_help \
   "pressed IN:  functional overlay will display\n" \
   "pressed OUT: functional overlay won't display"  \
   "\n"                                             \
   "This is useful for seeing what anatomical\n"    \
   "features are 'under' a particular function."

#define AFNI_marks_edits_bbox_help                            \
   "pressed IN:  you are allowed to change the markers\n"     \
   "pressed OUT: you aren't allowed to change the markers\n\n"\
   "WARNING: if you previously executed a geometric\n"        \
   "transformation, and then re-execute it with\n"            \
   "altered markers, you will re-write the old transformed\n" \
   "dataset, AND also destroy any `downstream' transformed\n" \
   "datasets -- for example, re-doing a AC-PC aligned view\n" \
   "will destroy the Talairach view (if any) that follows it"

#define AFNI_crosshair_av_help                   \
   "Off:    no display of crosshairs\n"          \
   "Single: display of single crosshairs\n"      \
   "Multi:  display of crosshairs for each\n"    \
   "         slice in the 'montage' layouts\n"   \
   "LR+AP:  display crosshairs only parallel\n"  \
   "         to the L-R and A-P axes [etc.]\n\n" \
   "N.B.: When a slice has an image viewer\n"    \
   "  and a grapher open at the same time,\n"    \
   "  then a 'frame' will be drawn around\n"     \
   "  the voxels being graphed.  In Single\n"    \
   "  crosshair mode, only this frame will\n"    \
   "  be drawn.  In Multi mode, the actual\n"    \
   "  crosshairs will also be drawn."

#define AFNI_crosshair_color_help \
   "Controls the\n"               \
   "crosshair color\n"            \
   "in the image\n"               \
   "viewing windows."

#define AFNI_crosshair_gap_help  \
   "Controls the crosshair\n"    \
   "gap (in voxels).\n\n"        \
   "N.B.: When a grapher is\n"   \
   "  opened with an image\n"    \
   "  viewer, then the gap\n"    \
   "  in that viewer will\n"     \
   "  be replaced by a\n"        \
   "  frame drawn around the\n"  \
   "  voxels being graphed."

#define AFNI_crosshair_label_help   \
   "Displays coordinates of\n"      \
   "the crosshair point in the\n"   \
   "DICOM coordinates (3D input)\n" \
   "or voxel indices (image input)"

#define AFNI_view_help                       \
   "Normal:   button opens viewing window\n"  \
   "Inverted: button raises opened window\n\n" \
   "N.B.: AFNI does not read datasets from\n"  \
   "      disk until a window is opened.\n"   \
   "      This can make opening the first\n" \
   "      viewing window be quite slow."    \
   "\n"                                      \
   "The Graph buttons are only enabled for\n" \
   "3D+time datasets that are viewing their\n" \
   ".BRIK files (not warping on demand --\n"  \
   "see the 'Define Datamode' control panel)"

#define AFNI_disp_pcolor_help  \
   "Controls the color used\n" \
   "to display the selected\n" \
   "marker (the 'primary').\n" \
   "\n"                        \
   "Oct 1998: Also controls\n" \
   "   the color for 'tags'."

#define AFNI_disp_scolor_help   \
   "Controls the color used\n"  \
   "to display non-selected\n"  \
   "markers (the 'secondaries')"

#define AFNI_disp_size_help   \
   "Controls the size\n"      \
   "of the markers on\n"      \
   "the screen (pixels)"

#define AFNI_disp_gap_help    \
   "Controls the gap\n"       \
   "in the markers'\n"        \
   "cross (in pixels)"

#define AFNI_marks_set_help    \
   "Use this button to\n"      \
   "set or reset the chosen\n" \
   "marker point"

#define AFNI_marks_clear_help   \
   "Use this button to\n"       \
   "clear (unset) the chosen\n" \
   "marker point"

#define AFNI_marks_quality_help     \
 "Use this button to run quality\n" \
 "checks on the marker set\n"       \
 "after all markers are defined\n"  \
 "and to enable the transformation"

#define AFNI_help_help \
   "Click the hand\n"   \
   "on any button to\n"  \
   "get a little help\n"  \
   " ----- OR -----\n"     \
   "Use the Motif Help\n"   \
   "key (usually F1) if\n"   \
   "the cursor is over\n"     \
   "a button."

#define AFNI_quit_help     \
   "  Press (twice) to\n"  \
   "  close this AFNI\n"   \
   "  control window.\n\n" \
   "N.B.: when the last\n" \
   "  control window is\n" \
   "  closed, AFNI will\n" \
   "  quit.\n\n"            \
   "Pressing this button\n"  \
   "with the Shift or Ctrl\n" \
   "keys, or with the other\n" \
   "mouse buttons also down,\n" \
   "will also make AFNI quit."

#define AFNI_marks_transform_help                             \
   "Use this button to execute the geometric\n"               \
   "transformation when all the marker points\n"              \
   "have been properly defined.\n\n"                          \
   "WARNING: if you previously executed a geometric\n"        \
   "transformation, and then re-execute it with\n"            \
   "altered markers, you will re-write the old transformed\n" \
   "dataset, AND also destroy any `downstream' transformed\n" \
   "datasets -- for example, re-doing a AC-PC aligned view\n" \
   "will destroy the Talairach view (if any) that follows it"

#define AFNI_disp_resam_vox_help            \
   "Use the arrows (or type) to set the\n"  \
   "(cubical) voxel dimensions for the\n"   \
   "data resampling"

/************* data for all widget create sub-functions ***************/

/*** this is needed because of memory problems with the HP compiler ***/
#ifdef HP
#pragma OPT_LEVEL 1
#endif

static   AFNI_widget_set       * vwid  ;
static   AFNI_imaging_widgets  * imag  ;
static   AFNI_viewing_widgets  * view  ;
static   AFNI_marks_widgets    * marks ;
static   AFNI_function_widgets * func  ;
static   AFNI_program_widgets  * prog  ;
static   AFNI_datamode_widgets * dmode ;

static   XmString   xstr ;
static   XmFontList xflist ;
static   char       str[256] ;
static   int        id , npane , last_color ,
                    view_count , view_height , sel_height ;

void AFNI_make_wid1 (Three_D_View *) ;
void AFNI_make_wid2 (Three_D_View *) ;
void AFNI_make_wid3 (Three_D_View *) ;

/*--------------------------------------------------------------------*/

void AFNI_make_widgets( Three_D_View * im3d )
{

ENTRY("AFNI_make_widgets") ;

   /*---- initialize -----*/

   if( ! IM3D_VALID(im3d) )
      FatalError("illegal call to AFNI_make_widgets") ;

   num_entry++ ;

   last_color = im3d->dc->ovc->ncol_ov - 1 ;

   vwid         = im3d->vwid ;
   vwid->parent = im3d ;

#define AFNI_FORM_SPACING 9

STATUS("creating top_form") ;

   vwid->top_form =
      XtVaCreateWidget(
         "dialog" , xmFormWidgetClass , vwid->top_shell ,
            XmNborderWidth , 0 ,
            XmNmarginHeight , AFNI_FORM_SPACING ,
            XmNmarginWidth  , AFNI_FORM_SPACING ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   MCW_register_help( vwid->top_form , AFNI_tophelp ) ;

   vwid->file_dialog = NULL ; /* Mar 1997 */

   /* create pixmaps, if desired */

#if defined(WANT_LOGO_BITMAP) || defined(WANT_AFNI_BITMAP)
   {  Pixel bg_pix  , fg_pix  ;  /* colors: from control window */
      Pixel bot_pix , top_pix ;  /* colors: from image windows  */

#ifdef USE_IMPIX              /** which colors to use for program icons **/
#  define ICON_bg bot_pix
#  define ICON_fg top_pix
#else
#  define ICON_bg bg_pix
#  define ICON_fg fg_pix
#endif

      XtVaGetValues( vwid->top_form ,
                       XmNforeground , &bg_pix ,  /* note reversal of roles here! */
                       XmNbackground , &fg_pix ,
                      NULL ) ;

      bot_pix = im3d->dc->pix_im[0] ;
      top_pix = im3d->dc->pix_im[im3d->dc->ncol_im-1] ;

#ifdef WANT_LOGO_BITMAP
STATUS("WANT_LOGO_BITMAP") ;
      if( logo_pixmap == XmUNSPECIFIED_PIXMAP ){

#ifndef NO_FRIVOLITIES
#include "lll.h"
        if( im3d->dc->visual_class == TrueColor ){  /* 23 Sep 2001 */
          MRI_IMAGE *bim ; XImage *xim ;
          bim = mri_new_vol_empty( lll_width,lll_height,1 , MRI_rgb ) ;
          mri_fix_data_pointer( lll_rgb , bim ) ;
          logo_pixmap = XCreatePixmap( im3d->dc->display ,
                                       RootWindowOfScreen(im3d->dc->screen) ,
                                       lll_width , lll_height ,
                                       im3d->dc->planes ) ;
          xim = rgb_to_XImage( im3d->dc , bim ) ;
          if( xim != NULL )
             XPutImage( im3d->dc->display ,
                        logo_pixmap ,
                        im3d->dc->origGC ,
                        xim , 0,0 , 0,0 , lll_width , lll_height ) ;
          MCW_kill_XImage( xim ); mri_clear_data_pointer(bim); mri_free(bim);
        }
#endif

        if( logo_pixmap == XmUNSPECIFIED_PIXMAP )         /* original code */
          logo_pixmap = XCreatePixmapFromBitmapData(
                          XtDisplay(vwid->top_shell) ,
                          RootWindowOfScreen(XtScreen(vwid->top_shell)) ,
                          logo_bits , logo_width , logo_height ,
                          fg_pix , bg_pix ,
                          DefaultDepthOfScreen(XtScreen(vwid->top_shell)) ) ;
      }
#endif

#ifdef WANT_AFNI_BITMAP
STATUS("WANT_AFNI_BITMAP") ;
      if( afni48_pixmap == XmUNSPECIFIED_PIXMAP )
        afni48_pixmap = XCreatePixmapFromBitmapData(
                         XtDisplay(vwid->top_shell) ,
                         RootWindowOfScreen(XtScreen(vwid->top_shell)) ,
                         afni48_bits , afni48_width , afni48_height ,
                         ICON_fg , ICON_bg ,
                         DefaultDepthOfScreen(XtScreen(vwid->top_shell)) ) ;

      if( afni48cor_pixmap == XmUNSPECIFIED_PIXMAP )
        afni48cor_pixmap = XCreatePixmapFromBitmapData(
                         XtDisplay(vwid->top_shell) ,
                         RootWindowOfScreen(XtScreen(vwid->top_shell)) ,
                         afni48cor_bits , afni48cor_width , afni48cor_height ,
                         ICON_fg , ICON_bg ,
                         DefaultDepthOfScreen(XtScreen(vwid->top_shell)) ) ;

      if( afni48sag_pixmap == XmUNSPECIFIED_PIXMAP )
        afni48sag_pixmap = XCreatePixmapFromBitmapData(
                         XtDisplay(vwid->top_shell) ,
                         RootWindowOfScreen(XtScreen(vwid->top_shell)) ,
                         afni48sag_bits , afni48sag_width , afni48sag_height ,
                         ICON_fg , ICON_bg ,
                         DefaultDepthOfScreen(XtScreen(vwid->top_shell)) ) ;

      if( afni48axi_pixmap == XmUNSPECIFIED_PIXMAP )
        afni48axi_pixmap = XCreatePixmapFromBitmapData(
                         XtDisplay(vwid->top_shell) ,
                         RootWindowOfScreen(XtScreen(vwid->top_shell)) ,
                         afni48axi_bits , afni48axi_width , afni48axi_height ,
                         ICON_fg , ICON_bg ,
                         DefaultDepthOfScreen(XtScreen(vwid->top_shell)) ) ;

      if( afni48gra_pixmap == XmUNSPECIFIED_PIXMAP )
        afni48gra_pixmap = XCreatePixmapFromBitmapData(
                         XtDisplay(vwid->top_shell) ,
                         RootWindowOfScreen(XtScreen(vwid->top_shell)) ,
                         afni48gra_bits , afni48gra_width , afni48gra_height ,
                         ICON_fg , ICON_bg ,
                         DefaultDepthOfScreen(XtScreen(vwid->top_shell)) ) ;

      if( afni48graaxi_pixmap == XmUNSPECIFIED_PIXMAP )
        afni48graaxi_pixmap = XCreatePixmapFromBitmapData(
                         XtDisplay(vwid->top_shell) ,
                         RootWindowOfScreen(XtScreen(vwid->top_shell)) ,
                         afni48graaxi_bits , afni48graaxi_width , afni48graaxi_height ,
                         ICON_fg , ICON_bg ,
                         DefaultDepthOfScreen(XtScreen(vwid->top_shell)) ) ;

      if( afni48grasag_pixmap == XmUNSPECIFIED_PIXMAP )
        afni48grasag_pixmap = XCreatePixmapFromBitmapData(
                         XtDisplay(vwid->top_shell) ,
                         RootWindowOfScreen(XtScreen(vwid->top_shell)) ,
                         afni48grasag_bits , afni48grasag_width , afni48grasag_height ,
                         ICON_fg , ICON_bg ,
                         DefaultDepthOfScreen(XtScreen(vwid->top_shell)) ) ;

      if( afni48gracor_pixmap == XmUNSPECIFIED_PIXMAP )
        afni48gracor_pixmap = XCreatePixmapFromBitmapData(
                         XtDisplay(vwid->top_shell) ,
                         RootWindowOfScreen(XtScreen(vwid->top_shell)) ,
                         afni48gracor_bits , afni48gracor_width , afni48gracor_height ,
                         ICON_fg , ICON_bg ,
                         DefaultDepthOfScreen(XtScreen(vwid->top_shell)) ) ;
#endif  /* WANT_AFNI_BITMAP */
   }
#endif  /* if WANT any of the BITMAPs */

   /* create each control panel, and a container frame for each */

STATUS("creating control panels") ;

   imag =vwid->imag = myXtNew(AFNI_imaging_widgets) ;ADDTO_KILL(im3d->kl,imag);
   view =vwid->view = myXtNew(AFNI_viewing_widgets) ;ADDTO_KILL(im3d->kl,view);
   marks=vwid->marks= myXtNew(AFNI_marks_widgets)   ;ADDTO_KILL(im3d->kl,marks);
   func =vwid->func = myXtNew(AFNI_function_widgets);ADDTO_KILL(im3d->kl,func);
   prog =vwid->prog = myXtNew(AFNI_program_widgets) ;ADDTO_KILL(im3d->kl,prog);
   dmode=vwid->dmode= myXtNew(AFNI_datamode_widgets);ADDTO_KILL(im3d->kl,dmode);

   imag->frame =
      XtVaCreateWidget(
         "dialog" , xmFrameWidgetClass , vwid->top_form ,
            XmNleftAttachment , XmATTACH_FORM ,
            XmNtopAttachment  , XmATTACH_FORM ,
            XmNleftOffset     , AFNI_FORM_SPACING ,
            XmNtopOffset      , AFNI_FORM_SPACING ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 5 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view->frame =
      XtVaCreateWidget(
         "dialog" , xmFrameWidgetClass , vwid->top_form ,
            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , imag->frame ,
            XmNtopAttachment  , XmATTACH_FORM ,
            XmNleftOffset     , AFNI_FORM_SPACING ,
            XmNtopOffset      , AFNI_FORM_SPACING ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 5 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   marks->frame =
      XtVaCreateWidget(
         "dialog" , xmFrameWidgetClass , vwid->top_form ,
            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , view->frame ,
            XmNtopAttachment  , XmATTACH_FORM ,
            XmNleftOffset     , AFNI_FORM_SPACING ,
            XmNtopOffset      , AFNI_FORM_SPACING ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 5 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->frame =
      XtVaCreateWidget(
         "dialog" , xmFrameWidgetClass , vwid->top_form ,
            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , view->frame ,
            XmNtopAttachment  , XmATTACH_FORM ,
            XmNleftOffset     , AFNI_FORM_SPACING ,
            XmNtopOffset      , AFNI_FORM_SPACING ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 5 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   dmode->frame =
      XtVaCreateWidget(
         "dialog" , xmFrameWidgetClass , vwid->top_form ,
            XmNleftAttachment   , XmATTACH_WIDGET ,
            XmNleftWidget       , view->frame ,
            XmNtopAttachment    , XmATTACH_FORM ,
            XmNleftOffset       , AFNI_FORM_SPACING ,
            XmNtopOffset        , AFNI_FORM_SPACING ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 5 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   prog->frame =
      XtVaCreateWidget(
         "dialog" , xmFrameWidgetClass , vwid->top_form ,
            XmNtopAttachment    , XmATTACH_WIDGET ,
            XmNtopWidget        , imag->frame ,
            XmNleftAttachment   , XmATTACH_FORM ,
            XmNleftOffset     , AFNI_FORM_SPACING ,
            XmNtopOffset      , AFNI_FORM_SPACING ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 5 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /************* call other routines to create rest of widgets  *************/

                            REFRESH ;
   AFNI_make_wid1( im3d ) ; REFRESH ;
   AFNI_make_wid2( im3d ) ; REFRESH ;
   AFNI_make_wid3( im3d ) ; REFRESH ;

#ifdef ALLOW_PLUGINS
   im3d->vwid->nplugbut = 0 ;
   AFNI_plugin_button( im3d ) ; /* 07 Oct 1996 */
#endif

   /*---------------------------------------------------*/
   /**************  finish up widgets *******************/
   /*---------------------------------------------------*/

   XtManageChild( imag->frame ) ;
   if( im3d->type == AFNI_3DDATA_VIEW ){
      XtManageChild( view->frame ) ;
   }
   XtManageChild( prog->frame ) ;

   XtManageChild( vwid->top_form ) ;

   EXRETURN ;
}

/*------------------------------------------------------------------*/

void AFNI_raiseup_CB( Widget w , XtPointer cd , XtPointer cb )
{
   XMapRaised( XtDisplay(w) , XtWindow(XtParent(w)) ) ;
}

/*--------------------------------------------------------------------*/

void AFNI_make_wid1( Three_D_View * im3d )
{
   int ii ;

ENTRY("AFNI_make_wid1") ;

   /*----------------------------------------------------------*/
   /***************** imaging controls *************************/
   /*----------------------------------------------------------*/

   /*--- vertical rowcol to hold all imaging stuff ---*/

STATUS("making imag->rowcol") ;

   imag->rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , imag->frame ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- popup menu to handle special imaging concerns ---*/

   imag->topper =           /* invisible widget to be parent of popup */
      XtVaCreateManagedWidget(
         "dialog" , xmSeparatorWidgetClass , imag->rowcol ,
            XmNseparatorType , XmNO_LINE ,
         NULL ) ;

   imag->popmenu =
      XmCreatePopupMenu( imag->topper , "menu" , NULL , 0 ) ;

   SAVEUNDERIZE(XtParent(imag->popmenu)) ; /* 27 Feb 2001 */

   VISIBILIZE_WHEN_MAPPED(imag->popmenu) ;

/***
   XtAddCallback( imag->popmenu ,
                  XmNunmapCallback , AFNI_imag_pop_CB , im3d ) ;
***/

   XtVaSetValues( imag->popmenu ,
                     XmNradioBehavior , True ,
                     XmNradioAlwaysOne , False ,
                     XmNspacing      , 1 ,
                     XmNmarginHeight , 0 ,
                     XmNmarginWidth  , 0 ,
                  NULL ) ;

   /*--- jumpback button in menu ---*/

   imag->pop_jumpback_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
            LABEL_ARG("Jumpback") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( imag->pop_jumpback_pb , XmNactivateCallback ,
                  AFNI_imag_pop_CB , im3d ) ;

   /*--- jumpto button in menu ---*/

   if( im3d->type == AFNI_3DDATA_VIEW ){
      imag->pop_jumpto_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
               LABEL_ARG("Jump to (xyz)") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      XtAddCallback( imag->pop_jumpto_pb , XmNactivateCallback ,
                     AFNI_imag_pop_CB , im3d ) ;
   } else {
      imag->pop_jumpto_pb = NULL ;
   }

   if( im3d->type == AFNI_3DDATA_VIEW ){
      imag->pop_jumpto_ijk_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
               LABEL_ARG("Jump to (ijk)") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      XtAddCallback( imag->pop_jumpto_ijk_pb , XmNactivateCallback ,
                     AFNI_imag_pop_CB , im3d ) ;
   } else {
      imag->pop_jumpto_ijk_pb = NULL ;
   }

   /*--- Talairach To button in menu ---*/

   if( im3d->type == AFNI_3DDATA_VIEW ){
      imag->pop_talto_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
               LABEL_ARG("-Talairach to") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      XtAddCallback( imag->pop_talto_pb , XmNactivateCallback ,
                     AFNI_imag_pop_CB , im3d ) ;

      if( TT_load_atlas() > 0 ){
         imag->pop_whereami_pb =        /* 10 Jul 2001 */
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
                  LABEL_ARG("-Where Am I?") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , False ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;

         XtAddCallback( imag->pop_whereami_pb , XmNactivateCallback ,
                        AFNI_imag_pop_CB , im3d ) ;

         imag->pop_ttren_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
                  LABEL_ARG("-Atlas Colors") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , False ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;

         XtAddCallback( imag->pop_ttren_pb , XmNactivateCallback ,
                        AFNI_imag_pop_CB , im3d ) ;

      } else {
         imag->pop_ttren_pb = imag->pop_whereami_pb = NULL ;
      }
      imag->pop_whereami_twin = NULL ;
   } else {
      imag->pop_talto_pb = NULL ;
      imag->pop_ttren_pb = imag->pop_whereami_pb = NULL ; /* 10 Jul 2001 */
      imag->pop_whereami_twin = NULL ;
   }

   /*--- imageonly button in menu ---*/

   imag->pop_imageonly_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
            LABEL_ARG("Image display") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( imag->pop_imageonly_pb , XmNactivateCallback ,
                  AFNI_imag_pop_CB , im3d ) ;

   /*--- frame to hold all crosshair stuff ---*/

   imag->crosshair_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , imag->rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- rowcol to manage crosshair stuff ---*/

   imag->crosshair_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , imag->crosshair_frame ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- label to display the crosshair location ---*/

   im3d->vinfo->old_crosshair_label = xstr = AFNI_crosshair_label( im3d ) ;

   imag->crosshair_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , imag->crosshair_rowcol ,
            XmNrecomputeSize , False ,
            XmNlabelString , xstr ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   MCW_register_help( imag->crosshair_label , AFNI_crosshair_label_help ) ;
   MCW_register_hint( imag->crosshair_label , "Coordinates of crosshair point" ) ;

   /*--- 01 Jan 1997: horizontal rowcol for crosshair stuff ---*/

STATUS("making imag->xhair_rowcol") ;

     imag->xhair_rowcol =
        XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , imag->crosshair_rowcol ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- arrowval to control crosshair visibility ---*/

   if( im3d->vinfo->crosshair_visible )
      ii = (im3d->vinfo->xhairs_show_montage) ? AFNI_XHAIRS_MULTI
                                              : AFNI_XHAIRS_SINGLE ;
   else
      ii = AFNI_XHAIRS_OFF ;

   im3d->vinfo->xhairs_orimask = ORIMASK_ALL ;  /* 31 Dec 1998 */

STATUS("making imag->crosshair_av") ;

   imag->crosshair_av = new_MCW_arrowval(
                          imag->xhair_rowcol ,        /* parent Widget */
                          "Xhairs" ,                  /* label */
                          AVOPT_STYLE ,               /* option menu style */
                          AFNI_XHAIRS_OFF ,           /* first option */
                          AFNI_XHAIRS_LASTOPTION ,    /* last option */
                          ii ,                        /* initial selection */
                          MCW_AV_readtext ,           /* ignored but needed */
                          0 ,                         /* ditto */
                          AFNI_crosshair_visible_CB , /* callback when changed */
                          (XtPointer) im3d ,          /* data for above */
                          MCW_av_substring_CB ,       /* text creation routine */
                          AFNI_crosshair_av_label     /* data for above */
                        ) ;

   if( AVOPT_STYLE == MCW_AV_optmenu )
      AVOPT_columnize( imag->crosshair_av , 3 ) ;

   imag->crosshair_av->parent     = (XtPointer) im3d ;
   imag->crosshair_av->allow_wrap = True ;

   MCW_reghelp_children( imag->crosshair_av->wrowcol , AFNI_crosshair_av_help ) ;
   MCW_reghint_children( imag->crosshair_av->wrowcol , "Crosshairs style" ) ;

   ADDTO_KILL(im3d->kl,imag->crosshair_av) ;

   /*--- 01 Jan 1997: buttonbox to control "all" or "one" xhairs in montage ---*/

STATUS("making imag->xhall_bbox") ;

   imag->xhall_bbox = new_MCW_bbox( imag->xhair_rowcol ,
                                     1 , AFNI_xhall_bbox_label ,
                                     MCW_BB_check ,
                                     MCW_BB_noframe ,
                                     AFNI_xhall_bbox_CB , (XtPointer) im3d ) ;

   MCW_set_bbox( imag->xhall_bbox , im3d->vinfo->xhairs_all ? 1 : 0 ) ;

   ADDTO_KILL(im3d->kl , imag->xhall_bbox) ;

   MCW_reghelp_children( imag->xhall_bbox->wrowcol ,
      "IN:  Montage will show crosshairs in all slices\n"
      "OUT: Montage will show crosshairs in one slice"
   ) ;
   MCW_reghint_children( imag->xhall_bbox->wrowcol ,
                         "All-or-One Montage crosshairs" ) ;

STATUS("managing imag->xhair_rowcol") ;

   XtManageChild( imag->xhair_rowcol ) ;

   /*--- arrowval to control crosshair color ---*/

   ii = AVOPT_STYLE ;

STATUS("making imag->crosshair_color_av") ;

   if( ii == MCW_AV_downup ){
      imag->crosshair_color_av =
         new_MCW_arrowval( imag->crosshair_rowcol ,        /* parent */
                           "Color " ,                      /* label */
                           MCW_AV_downup ,                 /* arrow directions */
                           1 ,                             /* min value */
                           last_color ,                    /* max value */
                           im3d->vinfo->crosshair_ovcolor ,/* init value */
                           MCW_AV_readtext ,               /* readonly text */
                           0 ,                             /* 0 decimal shift */
                           AFNI_crosshair_color_CB ,       /* click routine */
                           (XtPointer) im3d ,              /* data for above */
                           MCW_DC_ovcolor_text ,           /* text routine */
                           (XtPointer) im3d->dc            /* data for text */
                         ) ;

      (void) MCW_DC_ovcolor_text( imag->crosshair_color_av ,
                                  im3d->dc ) ;    /* set color now! */

      imag->crosshair_color_av->fastdelay  = 333 ;  /* slow down repeat action */
      imag->crosshair_color_av->allow_wrap = 1 ;

   } else {
      imag->crosshair_color_av =
         new_MCW_colormenu( imag->crosshair_rowcol , "Color " , im3d->dc ,
                            1 , last_color , im3d->vinfo->crosshair_ovcolor ,
                            AFNI_crosshair_color_CB , (XtPointer) im3d ) ;
   }

   imag->crosshair_color_av->parent     = (XtPointer) im3d ;
   imag->crosshair_color_av->fastdelay  = 333 ;  /* slow down repeat action */
   imag->crosshair_color_av->allow_wrap = 1 ;

   MCW_reghelp_children( imag->crosshair_color_av->wrowcol ,
                         AFNI_crosshair_color_help ) ;
   MCW_reghint_children( imag->crosshair_color_av->wrowcol , "Crosshairs color" ) ;

   ADDTO_KILL(im3d->kl,imag->crosshair_color_av) ;

   /**** 1 Aug 1996:
         put crosshair gap into a horizontal rowcol so
         can attach a toggle for periodic montages here, too ****/

STATUS("making imag->gap_wrap_rowcol") ;

   imag->gap_wrap_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , imag->crosshair_rowcol ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- arrowval to control crosshair gap ---*/

STATUS("making imag->crosshair_gap_av") ;

   imag->crosshair_gap_av =
      new_MCW_arrowval( imag->gap_wrap_rowcol ,      /* parent */
                        "Gap   " ,                   /* label */
                        AVOPT_STYLE ,                /* arrow directions */
                        -1  ,                        /* min value */
                        MAXOVSIZE ,                  /* max value */
                        im3d->vinfo->crosshair_gap , /* init value */
                        MCW_AV_editext ,             /* input/output text */
                        0 ,                          /* 0 decimal shift */
                        AFNI_crosshair_gap_CB ,      /* click routine */
                        (XtPointer) im3d ,           /* data */
                        NULL , NULL
                      ) ;

   if( AVOPT_STYLE == MCW_AV_optmenu && MAXOVSIZE >= COLSIZE )
      AVOPT_columnize( imag->crosshair_gap_av , 1+(MAXOVSIZE+1)/COLSIZE ) ;

   imag->crosshair_gap_av->parent    = (XtPointer) im3d ;
   imag->crosshair_gap_av->fastdelay = 333 ;  /* slow down repeat action */

   MCW_reghelp_children( imag->crosshair_gap_av->wrowcol ,
                         AFNI_crosshair_gap_help ) ;
   MCW_reghint_children( imag->crosshair_gap_av->wrowcol ,
                         "Gap in crosshairs" ) ;

   ADDTO_KILL(im3d->kl,imag->crosshair_gap_av) ;

   /*--- 1 Aug 1996:
         toggle button box to control periodic montages ---*/

STATUS("making imag->wrap_bbox") ;

   imag->wrap_bbox = new_MCW_bbox( imag->gap_wrap_rowcol ,
                                     1 , AFNI_wrap_bbox_label ,
                                     MCW_BB_check ,
                                     MCW_BB_noframe ,
                                     AFNI_wrap_bbox_CB , (XtPointer) im3d ) ;

   MCW_set_bbox( imag->wrap_bbox , im3d->vinfo->xhairs_periodic ? 1 : 0 ) ;

   ADDTO_KILL(im3d->kl , imag->wrap_bbox) ;

   MCW_reghelp_children( imag->wrap_bbox->wrowcol ,
      "IN:  Montage layout wraps around when slices\n"
      "       go past an edge of the dataset.\n"
      "OUT: Montage layout shows blanks for slices\n"
      "       past an edge of the dataset."
   ) ;
   MCW_reghint_children( imag->wrap_bbox->wrowcol ,
                         "Wrap montage past edge of volume?" ) ;

STATUS("managing imag->gap_wrap_rowcol") ;

   XtManageChild( imag->gap_wrap_rowcol ) ;

   /*--- arrowval to control time index we are viewing ---*/

STATUS("making imag->time_index_av") ;

   imag->time_index_av =
      new_MCW_arrowval( imag->crosshair_rowcol ,     /* parent */
                        "Index " ,                   /* label */
                        MCW_AV_downup ,              /* arrow directions */
                        0  ,                         /* min value */
                        im3d->vinfo->top_index ,     /* max value */
                        im3d->vinfo->time_index ,    /* init value */
                        MCW_AV_editext ,             /* input/output text */
                        0 ,                          /* 0 decimal shift */
                        AFNI_time_index_CB ,         /* click routine */
                        (XtPointer) im3d ,           /* data */
                        NULL , NULL
                      ) ;
   imag->time_index_av->parent     = (XtPointer) im3d ;
   imag->time_index_av->allow_wrap = 1 ;

   MCW_reghelp_children( imag->time_index_av->wrowcol ,
                         "Controls the time index\n"
                         "of the images being viewed.\n"
                         "[For time-dependent datasets.]" ) ;
   MCW_reghint_children( imag->time_index_av->wrowcol ,
                         "Set index in time" ) ;

   ADDTO_KILL(im3d->kl,imag->time_index_av) ;

   /*--- frame to hold all viewing control stuff ---*/

STATUS("imag->view_frame") ;

   imag->view_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , imag->rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- rowcol to manage viewing control stuff ---*/

   imag->view_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , imag->view_frame ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- pushbuttons to turn different views on ---*/

#define xyz_3DIM "Axial   "
#define yzx_3DIM "Sagittal"
#define zxy_3DIM "Coronal "

   /*--------------------------------------------------------------*/
   imag->xyz_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , imag->view_rowcol ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->name_xyz_lab =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , imag->xyz_rowcol ,
            LABEL_ARG( xyz_3DIM ) ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->image_xyz_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->xyz_rowcol ,
            LABEL_ARG("Image") ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->graph_xyz_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->xyz_rowcol ,
            LABEL_ARG("Graph") ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtManageChild( imag->xyz_rowcol ) ;
   /*----------------------------------------------------------------*/

   /*--------------------------------------------------------------*/
   imag->yzx_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , imag->view_rowcol ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->name_yzx_lab =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , imag->yzx_rowcol ,
            LABEL_ARG( yzx_3DIM ) ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->image_yzx_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->yzx_rowcol ,
            LABEL_ARG("Image") ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->graph_yzx_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->yzx_rowcol ,
            LABEL_ARG("Graph") ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtManageChild( imag->yzx_rowcol ) ;
   /*----------------------------------------------------------------*/

   /*--------------------------------------------------------------*/
   imag->zxy_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , imag->view_rowcol ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->name_zxy_lab =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , imag->zxy_rowcol ,
            LABEL_ARG( zxy_3DIM ) ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->image_zxy_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->zxy_rowcol ,
            LABEL_ARG("Image") ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->graph_zxy_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->zxy_rowcol ,
            LABEL_ARG("Graph") ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtManageChild( imag->zxy_rowcol ) ;
   /*----------------------------------------------------------------*/

   XtAddCallback( imag->image_xyz_pb , XmNactivateCallback ,
                  AFNI_view_xyz_CB , im3d ) ;

   XtAddCallback( imag->image_yzx_pb , XmNactivateCallback ,
                  AFNI_view_xyz_CB , im3d ) ;

   XtAddCallback( imag->image_zxy_pb , XmNactivateCallback ,
                  AFNI_view_xyz_CB , im3d ) ;

   XtAddCallback( imag->graph_xyz_pb , XmNactivateCallback ,
                  AFNI_view_xyz_CB , im3d ) ;

   XtAddCallback( imag->graph_yzx_pb , XmNactivateCallback ,
                  AFNI_view_xyz_CB , im3d ) ;

   XtAddCallback( imag->graph_zxy_pb , XmNactivateCallback ,
                  AFNI_view_xyz_CB , im3d ) ;

   MCW_reghelp_children( imag->xyz_rowcol , AFNI_view_help ) ;
   MCW_reghelp_children( imag->yzx_rowcol , AFNI_view_help ) ;
   MCW_reghelp_children( imag->zxy_rowcol , AFNI_view_help ) ;

   MCW_reghint_children( imag->xyz_rowcol , "Open/raise viewing window" ) ;
   MCW_reghint_children( imag->yzx_rowcol , "Open/raise viewing window" ) ;
   MCW_reghint_children( imag->zxy_rowcol , "Open/raise viewing window" ) ;

   /* imaging column finished, so manage its pieces */

   XtManageChild( imag->crosshair_rowcol ) ;
   XtManageChild( imag->view_rowcol ) ;
   XtManageChild( imag->rowcol ) ;

   /*-------------------------------------------------------*/
   /************ Controls for which view we see *************/
   /*-------------------------------------------------------*/

   marks->ov_visible = marks->tag_visible = False ;

   /*--- vertical rowcol to hold all viewing controls stuff ---*/

STATUS("making view->rowcol") ;

   view->rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , view->frame ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view_count = 0 ;  /* count of widgets in this column */

   /*--- radio box to control which view we see ---*/

   view->view_bbox =
      new_MCW_bbox( view->rowcol ,
                    LAST_VIEW_TYPE+1 ,
                    VIEW_typestr ,
                    MCW_BB_radio_one ,
                    MCW_BB_frame ,
                    AFNI_switchview_CB , (XtPointer) im3d ) ;

   for( id=0 ; id <= LAST_VIEW_TYPE ; id++ ){
      if( im3d->anat_dset[id] == NULL )
         XtSetSensitive( view->view_bbox->wbut[id] , False ) ;
   }

   MCW_set_bbox( view->view_bbox , 1 << im3d->vinfo->view_type ) ;

   ADDTO_KILL(im3d->kl,view->view_bbox) ;

   MCW_reghelp_children( view->view_bbox->wrowcol ,
      "Use these to select the\n"
      "type of view for your data" ) ;

   { char * hh[] = { "View data in original coordinates" ,
                     "View data in AC-PC aligned coordinates" ,
                     "View data in Talairach coordinates" } ;
     MCW_bbox_hints( view->view_bbox , 3 , hh ) ;
   }

   view_count += LAST_VIEW_TYPE+1 ;

   /*--- frame for marks buttons ---*/

   view->marks_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , view->rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view->marks_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , view->marks_frame ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- pushbutton to allow user to define marks ---*/

   view->define_marks_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , view->marks_rowcol ,
            LABEL_ARG("Define Markers") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view->marks_pb_inverted = False ;

   XtAddCallback( view->define_marks_pb , XmNactivateCallback ,
                  AFNI_define_CB , im3d ) ;

   MCW_register_help( view->define_marks_pb ,
      "Use this to allow you to define\n"
      "the markers for this view type" ) ;
   MCW_register_hint( view->define_marks_pb ,
                      "Open/close Talairach markers control panel" ) ;

   view_count ++ ;

   /*--- bbox to allow the user to turn marks off and on ---*/

   view->see_marks_bbox =
      new_MCW_bbox( view->marks_rowcol ,
                    1 , AFNI_see_marks_bbox_label ,
                    MCW_BB_check ,
                    MCW_BB_noframe ,
                    AFNI_see_marks_CB , (XtPointer) im3d ) ;

   view->see_marks_bbox->parent = (XtPointer) im3d ;

   MCW_set_bbox( view->see_marks_bbox , (marks->ov_visible) ? 0 : 1 ) ;

   MCW_reghelp_children( view->see_marks_bbox->wrowcol ,
                         AFNI_see_marks_bbox_help ) ;
   MCW_reghint_children( view->see_marks_bbox->wrowcol ,
                         "Visibility of Talairach markers and Tags" ) ;

   ADDTO_KILL(im3d->kl,view->see_marks_bbox) ;

   view_count ++ ;

   /*--- frame for func buttons ---*/

   im3d->vinfo->func_visible = False ;

   view->func_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , view->rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view->func_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , view->func_frame ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- pushbutton to allow user to define functions ---*/

   view->define_func_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , view->func_rowcol ,
            LABEL_ARG("Define Function") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view->func_pb_inverted = False ;

   XtAddCallback( view->define_func_pb , XmNactivateCallback ,
                  AFNI_define_CB , im3d ) ;

   MCW_register_help( view->define_func_pb ,
     "Use this to control the thresholds,\n"
      "colors, etc. for functional overlays" ) ;
   MCW_register_hint( view->define_func_pb ,
                      "Open/close functional overlay control panel" ) ;

   view_count ++ ;

   /*--- bbox to allow the user to turn function overlay off and on ---*/

   view->see_func_bbox =
      new_MCW_bbox( view->func_rowcol ,
                    1 , AFNI_see_func_bbox_label ,
                    MCW_BB_check ,
                    MCW_BB_noframe ,
                    AFNI_see_func_CB , (XtPointer) im3d ) ;

   view->see_func_bbox->parent = (XtPointer) im3d ;

   MCW_set_bbox( view->see_func_bbox , (im3d->vinfo->func_visible) ? 1 : 0 ) ;

   MCW_reghelp_children( view->see_func_bbox->wrowcol ,
                         AFNI_see_func_bbox_help ) ;
   MCW_reghint_children( view->see_func_bbox->wrowcol ,
                         "Visibility of functional overlay" ) ;

   ADDTO_KILL(im3d->kl,view->see_func_bbox) ;

   view_count ++ ;

   /*--- pushbutton to allow user to define datamode ---*/

   view->define_dmode_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , view->rowcol ,
            LABEL_ARG("Define Datamode") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view->dmode_pb_inverted = False ;

   XtAddCallback( view->define_dmode_pb , XmNactivateCallback ,
                  AFNI_define_CB , im3d ) ;

   MCW_register_help( view->define_dmode_pb ,
     "Use this to control the mode in which\n"
     "the anatomy data is viewed, and also\n"
     "to save 3D datasets to disk" ) ;
   MCW_register_hint( view->define_dmode_pb ,
                      "Open/close data manipulation control panel" ) ;

   view_count ++ ;

   /*--- frame for dataset choosers ---*/

   view->dataset_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , view->rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view->dataset_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , view->dataset_frame ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- pushbuttons for dataset choice ---*/

   view->choose_sess_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , view->dataset_rowcol ,
            LABEL_ARG("Switch Session") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( view->choose_sess_pb , XmNactivateCallback ,
                  AFNI_choose_dataset_CB , im3d ) ;

   MCW_register_help( view->choose_sess_pb ,
     "Use this to choose from which\n"
     "session 3D datasets may be viewed." ) ;
   MCW_register_hint( view->choose_sess_pb ,
                      "Switch between session directories" ) ;

   view_count ++ ;

   view->choose_anat_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , view->dataset_rowcol ,
            LABEL_ARG("Switch Anatomy") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( view->choose_anat_pb , XmNactivateCallback ,
                  AFNI_choose_dataset_CB , im3d ) ;

   MCW_register_help( view->choose_anat_pb ,
        "Use this to choose which\n"
        "anatomical 3D dataset to view\n"
        "(from the current session).\n\n"
        "N.B.: Datasets which can be\n"
        "  graphed are marked with a\n"
        "  '*' after their names.\n"
        "  Datasets that are compressed\n"
        "  have 'z' after their names."
   ) ;
   MCW_register_hint( view->choose_anat_pb ,
                      "Switch between anatomical datasets" ) ;

   view_count ++ ;

   view->choose_func_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , view->dataset_rowcol ,
            LABEL_ARG("Switch Function") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( view->choose_func_pb , XmNactivateCallback ,
                  AFNI_choose_dataset_CB , im3d ) ;

   MCW_register_help( view->choose_func_pb ,
     "Use this to choose which\n"
     "functional 3D dataset to view\n"
     "(from the current session).\n"
     "N.B.: Datasets that are compressed\n"
     "  have 'z' after their names."
    ) ;
   MCW_register_hint( view->choose_func_pb ,
                      "Switch between functional datasets" ) ;

   view_count ++ ;

#ifdef POPUP_CHOOSERS
   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , imag->popmenu ,
                XmNseparatorType , XmDOUBLE_LINE ,
            NULL ) ;

   view->popchoose_sess_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
            LABEL_ARG("Switch Session") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( view->popchoose_sess_pb , XmNactivateCallback ,
                  AFNI_choose_dataset_CB , im3d ) ;

   view->popchoose_anat_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
            LABEL_ARG("Switch Anatomy") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( view->popchoose_anat_pb , XmNactivateCallback ,
                  AFNI_choose_dataset_CB , im3d ) ;

   view->popchoose_func_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
            LABEL_ARG("Switch Function") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( view->popchoose_func_pb , XmNactivateCallback ,
                  AFNI_choose_dataset_CB , im3d ) ;

   if( im3d->type == AFNI_IMAGES_VIEW ){
      XtUnmanageChild( view->popchoose_func_pb ) ;
      XtUnmanageChild( view->popchoose_anat_pb ) ;
      XtUnmanageChild( view->popchoose_sess_pb ) ;
   }

#else  /* don't POPUP_CHOOSERS */

   view->popchoose_func_pb =
     view->popchoose_anat_pb =
       view->popchoose_sess_pb = NULL ;

#endif /* POPUP_CHOOSERS*/

   /*--- all view controls made, now manage them ---*/

   XtManageChild( view->marks_rowcol ) ;
   XtManageChild( view->func_rowcol ) ;
   XtManageChild( view->dataset_rowcol ) ;
   XtManageChild( view->rowcol ) ;

   /** compute height of widgets in the view-> column **/

   xstr = XmStringCreateLtoR("(y[M",XmFONTLIST_DEFAULT_TAG );
   XtVaGetValues( view->choose_func_pb , XmNfontList , &xflist , NULL ) ;
   view_height = (12 + XmStringHeight(xflist,xstr)) * view_count ;
   XmStringFree( xstr ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void AFNI_make_wid2( Three_D_View * im3d )
{
   int ii ;
   Widget hrc ;  /* 30 Mar 2001 */

ENTRY("AFNI_make_wid2") ;

   /*-------------------------------------------------------*/
   /************  Marker controls (lots of them) ************/
   /*-------------------------------------------------------*/

   /*----- horizontal rowcol to hold the two columns of controls -----*/

STATUS("making marks->rowcol") ;

   marks->rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , marks->frame ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmHORIZONTAL ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*---------------------------------------------------------------*/
   /*----- COLUMN 1: the toggle switches for selecting markers -----*/
   /*---------------------------------------------------------------*/

   marks->tog_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , marks->rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- rowcol to manage toggle buttons stuff ---*/

   marks->tog_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , marks->tog_frame ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmVERTICAL   ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*----- create radio buttons in pairs,
           one in the rowcol and one in the popmenu;
           N.B.: they are created unmanaged and will be managed
                 later, depending on defined marks for the view -----*/

   XtVaSetValues( marks->tog_rowcol ,
                     XmNradioBehavior , True ,
                     XmNradioAlwaysOne , False ,
                     XmNspacing      , 1 ,
                     XmNmarginHeight , 0 ,
                     XmNmarginWidth  , 0 ,
                  NULL ) ;

   marks->num_sometimes_popup = 0 ;
   marks->num_always_popup    = 0 ;

   marks->always_popup[(marks->num_always_popup)++] =
      XtVaCreateWidget(
         "dialog" , xmSeparatorWidgetClass , imag->popmenu ,
             XmNseparatorType , XmDOUBLE_LINE ,
         NULL ) ;

   { int ib ;
     Dimension isiz ;
     Pixel  fg_pix ;

     XtVaGetValues( marks->tog_rowcol , XmNforeground , &fg_pix , NULL ) ;

     for( ib=0 ; ib < MARKS_MAXNUM ; ib++ ){

        marks->tog[ib] =
           XtVaCreateWidget(
              "dialog" , xmToggleButtonWidgetClass , marks->tog_rowcol ,
                 XmNvisibleWhenOff , True ,
                 XmNmarginHeight , 0 ,
                 XmNmarginWidth  , 0 ,
                 XmNselectColor  , fg_pix ,
                 XmNtraversalOn  , False ,
                 XmNinitialResourcesPersistent , False ,
              NULL ) ;

     /* we set XmNindicatorSize resource for the popup to same as the
        menu above, since menu default size is tiny and looks bad! */

        if( ib==0 )
           XtVaGetValues( marks->tog[0],XmNindicatorSize,&isiz,NULL ) ;

        marks->sometimes_popup[(marks->num_sometimes_popup)++] =
        marks->poptog[ib] =
           XtVaCreateWidget(
              "dialog" , xmToggleButtonWidgetClass , imag->popmenu ,
                 XmNindicatorSize , isiz ,
                 XmNvisibleWhenOff , True ,
                 XmNmarginHeight , 0 ,
                 XmNmarginWidth  , 0 ,
                 XmNselectColor  , fg_pix ,
                 XmNtraversalOn  , False ,
                 XmNinitialResourcesPersistent , False ,
              NULL ) ;

        MCW_register_help( marks->tog[ib],&(marks->tog_help[ib][0]) ) ;

        XtAddCallback( marks->tog[ib] ,
                       XmNdisarmCallback ,
                       AFNI_marktog_CB , im3d ) ;

        XtAddCallback( marks->poptog[ib] ,
                       XmNvalueChangedCallback ,
                       AFNI_marktog_CB , im3d ) ;

        marks->inverted[ib] = False ;
     }  /* end of loop creating toggle buttons */
   }  /* end of radio toggle creation */

   /*----------------------------------------------------*/
   /*----- COLUMN 2: controls to manage the markers -----*/
   /*----------------------------------------------------*/

   marks->control_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , marks->rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- rowcol to manage marker control stuff ---*/

   marks->control_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , marks->control_frame ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmVERTICAL   ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- bbox to control editability of marks ---*/

   marks->edits_bbox =
      new_MCW_bbox( marks->control_rowcol ,
                    1 , AFNI_marks_edits_bbox_label ,
                    MCW_BB_check ,
                    MCW_BB_noframe ,
                    AFNI_marks_edits_CB , (XtPointer) im3d ) ;

   marks->edits_bbox->parent = (XtPointer) im3d ;

   MCW_reghelp_children( marks->edits_bbox->wrowcol ,
                         AFNI_marks_edits_bbox_help ) ;
   MCW_reghint_children( marks->edits_bbox->wrowcol ,
                         "Press IN to allow changes to markers" ) ;

   ADDTO_KILL(im3d->kl,marks->edits_bbox) ;

   /*----- frame to hold markers display stuff -----*/

   marks->disp_frame =
      XtVaCreateManagedWidget(
         "frame" , xmFrameWidgetClass , marks->control_rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*----- rowcol to hold markers display stuff -----*/

   marks->disp_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , marks->disp_frame ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- arrowval to control primary color ---*/

   marks->ov_pcolor = MIN(last_color,INIT_marks1_color) ;
   marks->ov_scolor = MIN(last_color,INIT_marks2_color) ;

   ii = AVOPT_STYLE ;

   if( ii == MCW_AV_downup ){
      marks->disp_pcolor_av =
         new_MCW_arrowval(
            marks->disp_rowcol ,    /* parent */
            "Pcolor" ,              /* label */
            MCW_AV_downup ,         /* arrow directions */
            0 ,                     /* min value */
            last_color ,            /* max value */
            marks->ov_pcolor ,      /* init value */
            MCW_AV_readtext ,       /* readonly text display */
            0 ,                     /* 0 decimal shift */
            AFNI_marks_disp_av_CB , /* click routine */
            (XtPointer) im3d ,      /* data */
            MCW_DC_ovcolor_text ,   /* text routine */
            (XtPointer) im3d->dc    /* data */
          ) ;
      marks->disp_pcolor_av->fastdelay  = 333 ;  /* slow down repeat action */
      marks->disp_pcolor_av->allow_wrap = 1 ;
      (void) MCW_DC_ovcolor_text( marks->disp_pcolor_av ,
                                  im3d->dc               ) ;  /* set color! */
   } else {
      marks->disp_pcolor_av =
         new_MCW_colormenu( marks->disp_rowcol , "Pcolor" , im3d->dc ,
                            0 , last_color , marks->ov_pcolor ,
                            AFNI_marks_disp_av_CB , (XtPointer) im3d ) ;
   }

   marks->disp_pcolor_av->parent = (XtPointer) im3d ;

   MCW_reghelp_children( marks->disp_pcolor_av->wrowcol ,
                         AFNI_disp_pcolor_help ) ;
   MCW_reghint_children( marks->disp_pcolor_av->wrowcol ,
                         "Color of primary marker" ) ;

   ADDTO_KILL(im3d->kl,marks->disp_pcolor_av) ;

   /*--- arrowval to control secondary color ---*/

   if( ii == MCW_AV_downup ){
      marks->disp_scolor_av =
         new_MCW_arrowval(
            marks->disp_rowcol ,    /* parent */
            "Scolor" ,              /* label */
            MCW_AV_downup ,         /* arrow directions */
            0 ,                     /* min value */
            last_color ,            /* max value */
            marks->ov_scolor ,      /* init value */
            MCW_AV_readtext ,       /* readonly text display */
            0 ,                     /* 0 decimal shift */
            AFNI_marks_disp_av_CB , /* click routine */
            (XtPointer) im3d ,      /* data */
            MCW_DC_ovcolor_text ,   /* text routine */
            (XtPointer) im3d->dc    /* data */
          ) ;
      marks->disp_scolor_av->fastdelay  = 333 ;  /* slow down repeat action */
      marks->disp_scolor_av->allow_wrap = 1 ;
      (void) MCW_DC_ovcolor_text( marks->disp_scolor_av ,
                                  im3d->dc               ) ;  /* set color! */
   } else {
      marks->disp_scolor_av =
         new_MCW_colormenu( marks->disp_rowcol , "Scolor" , im3d->dc ,
                            0 , last_color , marks->ov_scolor ,
                            AFNI_marks_disp_av_CB , (XtPointer) im3d ) ;
   }

   marks->disp_scolor_av->parent = (XtPointer) im3d ;

   MCW_reghelp_children( marks->disp_scolor_av->wrowcol ,
                         AFNI_disp_scolor_help ) ;
   MCW_reghint_children( marks->disp_scolor_av->wrowcol ,
                         "Color of secondary markers" ) ;

   ADDTO_KILL(im3d->kl,marks->disp_scolor_av) ;

   /*--- arrowval to control point size  ---*/

   marks->ov_size = INIT_marks_size ;  /* initialize overlay mask */
   marks->ov_gap  = INIT_marks_gap ;
   AFNI_make_ptmask( marks->ov_size , marks->ov_gap , &(marks->ov_mask) ) ;

   marks->disp_size_av =
      new_MCW_arrowval(
         marks->disp_rowcol ,    /* parent */
         "Size  " ,              /* label */
         AVOPT_STYLE ,           /* arrow directions */
         1  ,                    /* min value */
         MAXOVSIZE ,             /* max value */
         marks->ov_size ,        /* init value */
         MCW_AV_editext ,        /* input/output text display */
         0 ,                     /* 0 decimal shift */
         AFNI_marks_disp_av_CB , /* routine to call after click */
         (XtPointer) im3d ,      /* data to pass */
         NULL ,                  /* routine to call for text display */
         NULL                    /* data for text display routine */
      ) ;

   if( AVOPT_STYLE == MCW_AV_optmenu && MAXOVSIZE >= COLSIZE )
      AVOPT_columnize( marks->disp_size_av , 1+(MAXOVSIZE+1)/COLSIZE ) ;

   marks->disp_size_av->parent    = (XtPointer) im3d ;
   marks->disp_size_av->fastdelay = 333 ;  /* slow down repeat action */

   MCW_reghelp_children( marks->disp_size_av->wrowcol ,
                         AFNI_disp_size_help ) ;
   MCW_reghint_children( marks->disp_size_av->wrowcol ,
                         "Size of markers" ) ;

   ADDTO_KILL(im3d->kl,marks->disp_size_av) ;

   /*--- arrowval to control point gap  ---*/

   marks->disp_gap_av =
      new_MCW_arrowval(
         marks->disp_rowcol ,    /* parent */
         "Gap   " ,              /* label */
         AVOPT_STYLE ,           /* arrow directions */
         1  ,                    /* min value */
         MAXOVSIZE ,             /* max value */
         marks->ov_gap ,         /* init value */
         MCW_AV_editext ,        /* input/output text display */
         0 ,                     /* 0 decimal shift */
         AFNI_marks_disp_av_CB , /* routine to call after click */
         (XtPointer) im3d ,      /* data to pass */
         NULL ,                  /* routine to call for text display */
         NULL                    /* data for text display routine */
      ) ;

   if( AVOPT_STYLE == MCW_AV_optmenu && MAXOVSIZE >= COLSIZE )
      AVOPT_columnize( marks->disp_gap_av , 1+(MAXOVSIZE+1)/COLSIZE ) ;

   marks->disp_size_av->parent    = (XtPointer) im3d ;
   marks->disp_size_av->fastdelay = 333 ;  /* slow down repeat action */

   MCW_reghelp_children( marks->disp_gap_av->wrowcol ,
                         AFNI_disp_gap_help ) ;
   MCW_reghint_children( marks->disp_gap_av->wrowcol ,
                         "Size of gap in markers" ) ;

   ADDTO_KILL(im3d->kl,marks->disp_size_av) ;

   /*----- rowcol to hold action pushbuttons -----*/

   marks->action_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , marks->control_rowcol ,
            XmNorientation  , XmHORIZONTAL ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*----- set point pushbutton -----*/

   marks->action_set_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , marks->action_rowcol ,
            LABEL_ARG("Set") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( marks->action_set_pb , XmNactivateCallback ,
                  AFNI_marks_action_CB , im3d ) ;

   MCW_register_help( marks->action_set_pb , AFNI_marks_set_help ) ;
   MCW_register_hint( marks->action_set_pb , "Set marker at crosshairs" ) ;

   /*----- clear point pushbutton -----*/

   marks->action_clear_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , marks->action_rowcol ,
            LABEL_ARG("Clear") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( marks->action_clear_pb , XmNactivateCallback ,
                  AFNI_marks_action_CB , im3d ) ;

   MCW_register_help( marks->action_clear_pb , AFNI_marks_clear_help ) ;
   MCW_register_hint( marks->action_clear_pb , "Un-set primary marker" ) ;

   /*----- copies of these pushbuttons on the popup menu -----*/

   marks->always_popup[(marks->num_always_popup)++] =
      XtVaCreateManagedWidget(
             "dialog" , xmSeparatorWidgetClass , imag->popmenu ,
                 XmNseparatorType , XmDOUBLE_LINE ,
             NULL ) ;

   marks->always_popup[(marks->num_always_popup)++] =
   marks->pop_set_pb =
      XtVaCreateWidget(
         "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
            LABEL_ARG("Set") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( marks->pop_set_pb , XmNactivateCallback ,
                  AFNI_marks_action_CB , im3d ) ;

   marks->always_popup[(marks->num_always_popup)++] =
   marks->pop_clear_pb =
      XtVaCreateWidget(
         "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
            LABEL_ARG("Clear") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( marks->pop_clear_pb , XmNactivateCallback ,
                  AFNI_marks_action_CB , im3d ) ;

   /*----- a "quality" button (not on the popup menu) -----*/

   marks->action_quality_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , marks->action_rowcol ,
            LABEL_ARG("Quality?") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( marks->action_quality_pb , XmNactivateCallback ,
                  AFNI_marks_action_CB , im3d ) ;

   MCW_register_help( marks->action_quality_pb , AFNI_marks_quality_help ) ;
   MCW_register_hint( marks->action_quality_pb , "Check markers for consistency" ) ;

   /*----- below the line, put the transformation controls -----*/

   (void) XtVaCreateManagedWidget(
             "dialog" , xmSeparatorWidgetClass , marks->control_rowcol ,
                 XmNseparatorType , XmDOUBLE_LINE ,
             NULL ) ;

   /*----- the transformation control -----*/

   marks->transform_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , marks->control_rowcol ,
            LABEL_ARG("Transform Data") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( marks->transform_pb , XmNactivateCallback ,
                  AFNI_marks_transform_CB , im3d ) ;

   MCW_register_help( marks->transform_pb , AFNI_marks_transform_help ) ;
   MCW_register_hint( marks->transform_pb , "Compute transformation from markers" ) ;

   /*----- 3/06/96: the "big Talairach box" button -----*/

   marks->tlrc_big_bbox =
      new_MCW_bbox( marks->control_rowcol ,
                    1 , AFNI_tlrc_big_bbox_label ,
                    MCW_BB_check , MCW_BB_noframe ,
                    NULL , NULL                    ) ;    /* no callback */

   marks->tlrc_big_bbox->parent = (XtPointer) im3d ;

   MCW_reghelp_children( marks->tlrc_big_bbox->wrowcol ,
                         AFNI_tlrc_big_bbox_help ) ;
   MCW_reghint_children( marks->tlrc_big_bbox->wrowcol ,
                         "Use 'big' Talairach bounding box" ) ;

   ADDTO_KILL(im3d->kl,marks->tlrc_big_bbox) ;

   if( GLOBAL_argopt.tlrc_big )
      MCW_set_bbox( marks->tlrc_big_bbox , 1 ) ;

   /*----- manage the managers -----*/

   XtManageChild( marks->tog_rowcol ) ;
   XtManageChild( marks->disp_rowcol ) ;
   XtManageChild( marks->action_rowcol ) ;
   XtManageChild( marks->control_rowcol ) ;
   XtManageChild( marks->rowcol ) ;

   /*-----------------------------------------------------*/
   /**************** Function controls ********************/
   /*-----------------------------------------------------*/

/** old hard-wired definition */
#define SELECTOR_HEIGHT 240

   sel_height = view_height - 19 ;

STATUS("making func->rowcol") ;

   func->rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->frame ,
            XmNorientation , XmHORIZONTAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*-- threshold stuff --*/

   func->thr_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->rowcol ,
            XmNorientation , XmVERTICAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->thr_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , func->thr_rowcol ,
            LABEL_ARG("Thr") ,
            XmNrecomputeSize , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

#ifdef FIX_SCALE_VALUE_PROBLEM
   MCW_register_help( func->thr_label ,
      "This version of AFNI has been\n"
      "compiled to show the slider value\n"
      "here, since there is a bug in the\n"
      "Motif library for this computer system."
   ) ;
#else
#endif
   MCW_register_help( func->thr_label ,
      "Shows the type of threshold\n"
      "statistic that is available\n"
      "at this moment.  Names mean:\n\n"
      FUNC_THR_LABEL " = " FUNC_THR_DESCRIPTOR "\n"
      FUNC_COR_LABEL " = " FUNC_COR_DESCRIPTOR "\n"
      FUNC_TT_LABEL  " = " FUNC_TT_DESCRIPTOR  "\n"
      FUNC_FT_LABEL  " = " FUNC_FT_DESCRIPTOR  "\n"
      FUNC_ZT_LABEL  " = " FUNC_ZT_DESCRIPTOR  "\n"
      FUNC_CT_LABEL  " = " FUNC_CT_DESCRIPTOR  "\n"
      FUNC_BT_LABEL  " = " FUNC_BT_DESCRIPTOR  "\n"
      FUNC_BN_LABEL  " = " FUNC_BN_DESCRIPTOR  "\n"
      FUNC_GT_LABEL  " = " FUNC_GT_DESCRIPTOR  "\n"
      FUNC_PT_LABEL  " = " FUNC_PT_DESCRIPTOR  "\n"
   ) ;
   MCW_register_hint( func->thr_label , "Type of threshold statistic" ) ;

   FIX_SCALE_VALUE(im3d) ;

#define SCALE_EXTRA 66

  {Widget qqq ; int iqqq ;
   char thr_str[] = "-----------" ;
   char zork[2] ;

   int smax , stop , decim , sstep ;                  /* 30 Nov 1997:       */
   decim = THR_TOP_EXPON ;                            /* compute parameters */
   smax  = (int)( pow(10.0,decim) + 0.001 ) ;         /* for scale display. */
   stop  = smax - 1 ;
   sstep = smax / 1000 ;  if( sstep < 1 ) sstep = 1 ;

#ifdef BOXUP_SCALE
   qqq = XtVaCreateManagedWidget(
           "dialog" , xmFrameWidgetClass , func->thr_rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
#else
   qqq = func->thr_rowcol ;
#endif

   func->thr_scale =
      XtVaCreateManagedWidget(
         "scale" , xmScaleWidgetClass , qqq ,
            XmNminimum , 0 ,             /* 30 Nov 1997: changed */
            XmNmaximum , stop ,          /* range to be computed */
            XmNscaleMultiple , sstep ,
            XmNdecimalPoints , decim ,
#ifdef FIX_SCALE_VALUE_PROBLEM
            XmNshowValue , False ,
#else
            XmNshowValue , True ,
#endif
            XmNvalue , (int)(smax*im3d->vinfo->func_threshold) ,
            XmNorientation , XmVERTICAL ,
            XmNheight , sel_height ,
            XmNborderWidth , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

#ifdef FIX_SCALE_SIZE_PROBLEM
   XtVaSetValues( func->thr_scale ,
                    XmNuserData , (XtPointer) sel_height ,
                  NULL ) ;
#endif

#ifdef FIX_SCALE_VALUE_PROBLEM
   for( iqqq=0 ; iqqq < strlen(thr_str) ; iqqq++ ){
      zork[0] = thr_str[iqqq] ; zork[1] = '\0' ;
      XtVaCreateManagedWidget( zork,xmLabelWidgetClass,func->thr_scale,NULL ) ;
   }
#endif
  }

   XtAddCallback( func->thr_scale , XmNvalueChangedCallback ,
                  AFNI_thr_scale_CB , im3d ) ;

   XtAddCallback( func->thr_scale , XmNdragCallback ,
                  AFNI_thr_scale_drag_CB , im3d ) ;

   MCW_reghelp_children( func->thr_scale ,
      "Drag the slider bar to\n"
      "adjust the threshold\n"
      "for function display.\n\n"
      "N.B.: this is the only\n"
      "  widget that is usable\n"
      "  during 'real-time'\n"
      "  FIM calculations!"
    ) ;
#if 0
   MCW_register_hint( func->thr_scale , "Threshold for functional overlay" ) ;
#endif

   /** Mar 1996: label for computed p-value, under scale **/

   func->thr_pval_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , func->thr_rowcol ,
            LABEL_ARG( THR_PVAL_LABEL_NONE ) ,
            XmNrecomputeSize , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   MCW_register_help( func->thr_pval_label ,
         "Shows the estimated significance\n"
         "(p value) of the threshold above,\n"
         "if possible.\n"
         "* If not possible, will display as\n"
         "   '[N/A]' instead.\n"
         "* p's that display as 1.2-7 should\n"
         "   be interpreted as 1.2 x 10^(-7).\n"
         "* This is the significance PER VOXEL." ) ;
   MCW_register_hint( func->thr_pval_label , "Estimated p-value per voxel" ) ;

   /** Jul 1997: optmenu to choose top value for scale **/

   func->thr_top_av = new_MCW_arrowval( func->thr_rowcol ,
                                        "**" ,
                                        AVOPT_STYLE ,
                                        0,THR_TOP_EXPON,0 ,
                                        MCW_AV_notext , 0 ,
                                        AFNI_thresh_top_CB , (XtPointer) im3d ,
                                        AFNI_thresh_tlabel_CB , NULL ) ;

   im3d->vinfo->func_thresh_top = 1.0 ;

   MCW_reghelp_children( func->thr_top_av->wrowcol ,
                           "Use this to set\n"
                           "the power-of-10\n"
                           "range of the\n"
                           "threshold slider\n"
                           "above."
                       ) ;

   MCW_reghint_children( func->thr_top_av->wrowcol ,
                         "Power-of-10 range of slider" ) ;

   /*-- intensity threshold stuff --*/

   func->inten_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->rowcol ,
            XmNorientation , XmVERTICAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNpacking , XmPACK_TIGHT ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->inten_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , func->inten_rowcol ,
            LABEL_ARG("Inten") ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /**-- 17 Dec 1997: pbar menu hidden on the inten_label --**/

   func->pbar_menu = XmCreatePopupMenu( func->inten_label , "menu" , NULL , 0 ) ;

   SAVEUNDERIZE(XtParent(func->pbar_menu)) ; /* 27 Feb 2001 */

   VISIBILIZE_WHEN_MAPPED(func->pbar_menu) ;

   XtInsertEventHandler( func->inten_label ,      /* handle events in label */

                               0
                             | ButtonPressMask   /* button presses */
                            ,
                            FALSE ,              /* nonmaskable events? */
                            AFNI_pbar_EV ,       /* handler */
                            (XtPointer) im3d ,   /* client data */
                            XtListTail           /* last in queue */
                          ) ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmLabelWidgetClass , func->pbar_menu ,
               LABEL_ARG("--- Cancel ---") ,
               XmNrecomputeSize , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , func->pbar_menu ,
             XmNseparatorType , XmSINGLE_LINE , NULL ) ;

   func->pbar_equalize_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , func->pbar_menu ,
            LABEL_ARG("Equalize Spacing") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( func->pbar_equalize_pb , XmNactivateCallback ,
                  AFNI_pbar_CB , im3d ) ;

   MCW_register_hint( func->pbar_equalize_pb , "Space separators equally" ) ;

   func->pbar_settop_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , func->pbar_menu ,
            LABEL_ARG("Set Top Value") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( func->pbar_settop_pb , XmNactivateCallback ,
                  AFNI_pbar_CB , im3d ) ;

   MCW_register_hint( func->pbar_settop_pb , "Is scaled by 'range' controls" ) ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , func->pbar_menu ,
             XmNseparatorType , XmSINGLE_LINE , NULL ) ;

   func->pbar_readin_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , func->pbar_menu ,
            LABEL_ARG("Read in palette") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   MCW_register_hint( func->pbar_readin_pb , "Read in a palette file" ) ;

   XtAddCallback( func->pbar_readin_pb , XmNactivateCallback ,
                  AFNI_pbar_CB , im3d ) ;

   func->pbar_writeout_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , func->pbar_menu ,
            LABEL_ARG("Write out palette") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   MCW_register_hint( func->pbar_writeout_pb ,
                      "Write out a palette file" ) ;

   XtAddCallback( func->pbar_writeout_pb , XmNactivateCallback ,
                  AFNI_pbar_CB , im3d ) ;

   /* 15 Jun 2000: image save button */

   func->pbar_saveim_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , func->pbar_menu ,
            LABEL_ARG("Save to PPM") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   MCW_register_hint( func->pbar_saveim_pb ,
                      "Write out as image file" );

   XtAddCallback( func->pbar_saveim_pb , XmNactivateCallback ,
                  AFNI_pbar_CB , im3d ) ;

   func->pbar_showtable_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , func->pbar_menu ,
            LABEL_ARG("Show Palette Table") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( func->pbar_showtable_pb , XmNactivateCallback ,
                  AFNI_pbar_CB , im3d ) ;

   MCW_register_hint( func->pbar_showtable_pb , "Will popup a listing window" ) ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , func->pbar_menu ,
             XmNseparatorType , XmSINGLE_LINE , NULL ) ;

   func->pbar_palette_av = new_MCW_arrowval(
                             func->pbar_menu ,     /* parent Widget */
                             "Set Pal " ,          /* label */
                             MCW_AV_optmenu ,      /* option menu style */
                             0 ,                   /* first option */
                             1 ,                   /* last option */
                             0 ,                   /* initial selection */
                             MCW_AV_readtext ,     /* ignored but needed */
                             0 ,                   /* ditto */
                             AFNI_palette_av_CB ,  /* callback when changed */
                             (XtPointer) im3d ,    /* data for above */
                             MCW_av_substring_CB , /* text creation routine */
                             AFNI_dummy_av_label   /* data for above */
                           ) ;

   MCW_reghint_children( func->pbar_palette_av->wrowcol , "Choose a palette" ) ;

   if( GPT != NULL && PALTAB_NUM(GPT) > 0 ){
      refit_MCW_optmenu( func->pbar_palette_av ,
                           0 ,                     /* new minval */
                           PALTAB_NUM(GPT)-1 ,     /* new maxval */
                           0 ,                     /* new inival */
                           0 ,                     /* new decim? */
                           AFNI_palette_label_CB , /* text routine */
                           NULL                    /* text data */
                        ) ;
   } else {
      XtUnmanageChild( func->pbar_palette_av->wrowcol ) ;
   }

   /*-- 15 Jun 2000: 0D func list --*/

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , func->pbar_menu ,
             XmNseparatorType , XmSINGLE_LINE , NULL ) ;

   func->pbar_transform0D_av = new_MCW_arrowval(
                             func->pbar_menu ,     /* parent Widget */
                             "Tran 0D " ,          /* label */
                             MCW_AV_optmenu ,      /* option menu style */
                             0 ,                   /* first option */
                             1 ,                   /* last option */
                             0 ,                   /* initial selection */
                             MCW_AV_readtext ,     /* ignored but needed */
                             0 ,                   /* ditto */
                             AFNI_palette_tran_CB, /* callback when changed */
                             (XtPointer) im3d ,    /* data for above */
                             MCW_av_substring_CB , /* text creation routine */
                             AFNI_dummy_av_label   /* data for above */
                           ) ;

   MCW_reghint_children( func->pbar_transform0D_av->wrowcol ,
                         "Transform overlay image values" ) ;
   XtUnmanageChild( func->pbar_transform0D_av->wrowcol ) ;
   func->pbar_transform0D_index = 0 ;
   func->pbar_transform0D_func  = NULL ;

   /*-- 16 Jun 2000: 2D func list --*/

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , func->pbar_menu ,
             XmNseparatorType , XmSINGLE_LINE , NULL ) ;

   func->pbar_transform2D_av = new_MCW_arrowval(
                             func->pbar_menu ,     /* parent Widget */
                             "Tran 2D " ,          /* label */
                             MCW_AV_optmenu ,      /* option menu style */
                             0 ,                   /* first option */
                             1 ,                   /* last option */
                             0 ,                   /* initial selection */
                             MCW_AV_readtext ,     /* ignored but needed */
                             0 ,                   /* ditto */
                             AFNI_palette_tran_CB, /* callback when changed */
                             (XtPointer) im3d ,    /* data for above */
                             MCW_av_substring_CB , /* text creation routine */
                             AFNI_dummy_av_label   /* data for above */
                           ) ;

   MCW_reghint_children( func->pbar_transform2D_av->wrowcol ,
                         "Transform overlay image values" ) ;
   XtUnmanageChild( func->pbar_transform2D_av->wrowcol ) ;
   func->pbar_transform2D_index = 0 ;
   func->pbar_transform2D_func  = NULL ;

   /**-- Color pbar to control intensity-to-color mapping --**/

   { float pmin , pmax ;  /* posfunc added 3/21/95 */

     pmax  = 1.0 ;
     pmin  = (im3d->vinfo->use_posfunc) ? (0.0) : (-1.0) ;
     npane = (im3d->vinfo->use_posfunc) ? INIT_panes_pos
                                        : INIT_panes_sgn ;

#if 0
     sel_height -= (8+view_height/view_count) * 1 ; /* 1 = widgets below pbar */
#else
     sel_height -= (8+view_height/view_count) * 0.5 ;
#endif

     func->inten_pbar = new_MCW_pbar(
                          func->inten_rowcol ,        /* parent */
                          im3d->dc ,                  /* display */
                          npane ,                     /* number panes */
                          sel_height / npane ,        /* init pane height */
                          pmin , pmax ,               /* value range */
                          AFNI_inten_pbar_CB ,        /* callback */
                          (XtPointer) im3d    );      /* callback data */
   }

   func->inten_pbar->parent       = (XtPointer) im3d ;
   func->inten_pbar->mode         = (im3d->vinfo->use_posfunc) ? (1) : (0) ;
   func->inten_pbar->npan_save[0] = INIT_panes_sgn ;
   func->inten_pbar->npan_save[1] = INIT_panes_pos ;
   func->inten_pbar->hide_changes = INIT_panes_hide ;

   AFNI_setup_inten_pbar( im3d ) ;  /* other setup stuff (afni_func.c) */

   MCW_reghelp_children( func->inten_pbar->panew ,
      "Drag the separator bars to alter the thresholds.\n"
      "Click in a pane to alter the color for that range.\n\n"
      "The functional dataset value that maps to 1.0 is\n"
      "determined by the 'autoRange' controls to the right."
   ) ;

   MCW_reghelp_children( func->inten_pbar->top ,
      "Drag the separator bars to alter the thresholds.\n"
      "Click in a pane to alter the color for that range.\n\n"
      "The functional dataset value that maps to 1.0 is\n"
      "determined by the 'autoRange' controls to the right."
   ) ;

   MCW_register_help( func->inten_label ,
      "Drag the separator bars to alter the thresholds.\n"
      "Click in a pane to alter the color for that range.\n\n"
      "The functional dataset value that maps to 1.0 is\n"
      "determined by the 'autoRange' controls to the right.\n\n"
      "N.B.: A popup menu to control the palette\n"
      "      setup is 'hidden' under this label."
   ) ;

   MCW_register_hint( func->inten_label ,
                      "Control functional overlay colors" ) ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , func->inten_rowcol ,
                XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

   func->inten_av = new_MCW_arrowval(
                       func->inten_rowcol ,
                        "#" ,
                        AVOPT_STYLE ,
                        NPANE_MIN , NPANE_MAX , npane ,
                        MCW_AV_notext , 0 ,
                        AFNI_inten_av_CB , func->inten_pbar ,
                        NULL,NULL ) ;

   if( AVOPT_STYLE == MCW_AV_optmenu && NPANE_MAX >= COLSIZE )
      AVOPT_columnize( func->inten_av , 1+(NPANE_MAX+1)/COLSIZE ) ;

   func->inten_av->fastdelay  = 4000 ;  /* slow down repeat action */
   func->inten_av->parent     = im3d ;  /* Daddy! */
   func->inten_av->allow_wrap = 1 ;

   MCW_reghelp_children( func->inten_av->wrowcol ,
     "Controls the number of panes\n"
     "in the intensity color/threshold\n"
     "selector above (the 'pbar')"
   ) ;
   MCW_reghint_children( func->inten_av->wrowcol ,
                         "Number of color panes" ) ;

   /*--- toggle button to control posfunc option for pbar ---*/

   func->inten_bbox =
      new_MCW_bbox( func->inten_rowcol ,
                    1 , AFNI_inten_bbox_label ,
                    MCW_BB_check ,
                    MCW_BB_noframe ,
                    AFNI_inten_bbox_CB , (XtPointer) im3d ) ;

   func->inten_bbox->parent = (XtPointer) im3d ;

   MCW_set_bbox( func->inten_bbox ,
                 (im3d->vinfo->use_posfunc) ? (1) : (0) ) ;

   MCW_reghelp_children( func->inten_bbox->wrowcol ,
                         "Pressed In: Displays only positive function\n"
                         "            values in the 'pbar' above and\n"
                         "            in the color overlays.\n"
                         "       Out: Displays positive and negative\n"
                         "            function values.\n\n"
                         "N.B.: Zero function values are never overlaid." ) ;
   MCW_reghint_children( func->inten_bbox->wrowcol ,
                         "Use positive-only or signed functional values" ) ;

   ADDTO_KILL(im3d->kl,func->inten_bbox) ;

   /*-- options controls --*/

   func->options_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->rowcol ,
            XmNorientation , XmVERTICAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->options_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , func->options_rowcol ,
            LABEL_ARG("Options") ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*-- underlay type --*/

   func->underlay_bbox =
      new_MCW_bbox( func->options_rowcol ,
                    LAST_UNDERLAY_TYPE+1 , UNDERLAY_typestr ,
                    MCW_BB_radio_one ,
                    MCW_BB_frame ,
                    AFNI_underlay_CB , (XtPointer) im3d ) ;

   func->underlay_bbox->parent = (XtPointer) im3d ;

   MCW_set_bbox( func->underlay_bbox , 1 << im3d->vinfo->underlay_type ) ;

   MCW_reghelp_children( func->underlay_bbox->wrowcol ,
      "Use these buttons to choose\n"
      "whether the anatomical or\n"
      "functional images appear\n"
      "as the background display" ) ;

   { char * hh[] = { "Use anatomical dataset for background" ,
                     "Use functional dataset for background" ,
                     "Use thresholded functional dataset for background" } ;
     MCW_bbox_hints( func->underlay_bbox , 3 , hh ) ;
   }

   ADDTO_KILL(im3d->kl,func->underlay_bbox) ;

   /*--- 30 Nov 1997: bucket managers ---*/

   func->buck_frame =
      XtVaCreateWidget(
         "dialog" , xmFrameWidgetClass , func->options_rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->buck_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->buck_frame ,
            XmNorientation , XmVERTICAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- 30 Nov 1997: anatomy bucket arrowval ---*/
   /*    (Actual labels are set when used)       */

   func->anat_buck_av = new_MCW_arrowval(
                          func->buck_rowcol    ,  /* parent Widget */
                          "Anat" ,                /* label */
                          MCW_AV_optmenu ,        /* option menu style */
                          0 ,                     /* first option */
                          1 ,                     /* last option */
                          0 ,                     /* initial selection */
                          MCW_AV_readtext ,       /* ignored but needed */
                          0 ,                     /* ditto */
                          AFNI_bucket_CB ,        /* callback when changed */
                          (XtPointer) im3d ,      /* data for above */
                          MCW_av_substring_CB ,   /* text creation routine */
                          AFNI_dummy_av_label     /* data for above */
                        ) ;

   func->anat_buck_av->parent     = (XtPointer) im3d ;
   func->anat_buck_av->allow_wrap = True ;

   MCW_reghelp_children( func->anat_buck_av->wrowcol ,
                         "Use this to choose which\n"
                         "sub-brick of the anatomical\n"
                         "dataset to display (='Anat').\n"
                         "(The sub-brick labels are\n"
                         " assigned when the dataset\n"
                         " is created.  The [index]\n"
                         " values show the numerical\n"
                         " location of the sub-brick\n"
                         " in the dataset.)"           ) ;
   MCW_reghint_children( func->anat_buck_av->wrowcol ,
                         "Choose Anat sub-brick" ) ;

   ADDTO_KILL(im3d->kl,func->anat_buck_av) ;

   XtUnmanageChild( func->anat_buck_av->wrowcol ) ;

   /*--- 30 Nov 1997: function bucket arrowval ---*/

   func->fim_buck_av = new_MCW_arrowval(
                          func->buck_rowcol    ,  /* parent Widget */
                          "Func" ,                /* label */
                          MCW_AV_optmenu ,        /* option menu style */
                          0 ,                     /* first option */
                          1 ,                     /* last option */
                          0 ,                     /* initial selection */
                          MCW_AV_readtext ,       /* ignored but needed */
                          0 ,                     /* ditto */
                          AFNI_bucket_CB ,        /* callback when changed */
                          (XtPointer) im3d ,      /* data for above */
                          MCW_av_substring_CB ,   /* text creation routine */
                          AFNI_dummy_av_label     /* data for above */
                        ) ;

   func->fim_buck_av->parent     = (XtPointer) im3d ;
   func->fim_buck_av->allow_wrap = True ;

   MCW_reghelp_children( func->fim_buck_av->wrowcol ,
                         "Use this to choose which\n"
                         "sub-brick of the functional\n"
                         "dataset to display (='Func').\n"
                         "(The sub-brick labels are\n"
                         " assigned when the dataset\n"
                         " is created.  The [index]\n"
                         " values show the numerical\n"
                         " location of the sub-brick\n"
                         " in the dataset.)"           ) ;
   MCW_reghint_children( func->fim_buck_av->wrowcol ,
                         "Choose Func sub-brick" ) ;

   ADDTO_KILL(im3d->kl,func->fim_buck_av) ;

   XtUnmanageChild( func->fim_buck_av->wrowcol ) ;

   /*--- 30 Nov 1997: threshold bucket arrowval ---*/

   func->thr_buck_av = new_MCW_arrowval(
                          func->buck_rowcol    ,  /* parent Widget */
                          "Thr " ,                /* label */
                          MCW_AV_optmenu ,        /* option menu style */
                          0 ,                     /* first option */
                          1 ,                     /* last option */
                          0 ,                     /* initial selection */
                          MCW_AV_readtext ,       /* ignored but needed */
                          0 ,                     /* ditto */
                          AFNI_bucket_CB ,        /* callback when changed */
                          (XtPointer) im3d ,      /* data for above */
                          MCW_av_substring_CB ,   /* text creation routine */
                          AFNI_dummy_av_label     /* data for above */
                        ) ;

   func->thr_buck_av->parent     = (XtPointer) im3d ;
   func->thr_buck_av->allow_wrap = True ;

   MCW_reghelp_children( func->thr_buck_av->wrowcol ,
                         "Use this to choose which\n"
                         "sub-brick of the functional\n"
                         "dataset with which to threshold\n"
                         "the Func sub-brick (='Thr').\n"
                         "(The sub-brick labels are\n"
                         " assigned when the dataset\n"
                         " is created.  The [index]\n"
                         " values show the numerical\n"
                         " location of the sub-brick\n"
                         " in the dataset.)"           ) ;
   MCW_reghint_children( func->thr_buck_av->wrowcol ,
                         "Choose Thr sub-brick" ) ;

   ADDTO_KILL(im3d->kl,func->thr_buck_av) ;

   XtUnmanageChild( func->thr_buck_av->wrowcol ) ;

   /*--- radio box to control which function we see ---*/

   func->functype_bbox =
      new_MCW_bbox( func->options_rowcol ,
                    LAST_SHOWFUNC_TYPE+1 , SHOWFUNC_typestr ,
                    MCW_BB_radio_one ,
                    MCW_BB_frame ,
                    AFNI_functype_CB , (XtPointer) im3d ) ;

   func->functype_bbox->parent = (XtPointer) im3d ;

   MCW_set_bbox( func->functype_bbox , 1 << im3d->vinfo->showfunc_type ) ;

   MCW_reghelp_children( func->functype_bbox->wrowcol ,
     "Use these to choose the type of\n"
      "function that will be displayed" ) ;

   { char * hh[] = { "Use intensity brick for color overlay" ,
                     "Use threshold brick for color overlay"  } ;
     MCW_bbox_hints( func->functype_bbox , 2 , hh ) ;
   }

   ADDTO_KILL(im3d->kl,func->functype_bbox) ;

   /*--- range controls ---*/

   func->range_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , func->options_rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->range_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->range_frame ,
            XmNorientation , XmVERTICAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- label to show the ranges ---*/

   im3d->vinfo->old_range_label = xstr = AFNI_range_label( NULL ) ;

   func->range_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , func->range_rowcol ,
            XmNrecomputeSize , False ,
            XmNlabelString , xstr ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   MCW_register_help( func->range_label ,
                        "These are the range of values in the\n"
                        "Anatomy and Functional 3D datasets.\n"
                        "The functional values may be useful\n"
                        "for choosing the Range for the pbar.\n"
                        "[If a dataset is warped from a\n"
                        " 'parent', these statistics are\n"
                        " taken from the parent dataset.]"   ) ;

   MCW_register_hint( func->range_label , "Ranges of dataset values" ) ;

   /*--- toggle button to control automatic range scaling for pbar ---*/

   im3d->vinfo->autorange_label =
      XmStringCreateLtoR( AFNI_range_bbox_label[0] , XmFONTLIST_DEFAULT_TAG ) ;

   func->range_bbox =
      new_MCW_bbox( func->range_rowcol ,
                    1 , AFNI_range_bbox_label ,
                    MCW_BB_check ,
                    MCW_BB_noframe ,
                    AFNI_range_bbox_CB , (XtPointer) im3d ) ;

   func->range_bbox->parent = (XtPointer) im3d ;

   MCW_set_bbox( func->range_bbox ,
                 (im3d->vinfo->use_autorange) ? (1) : (0) ) ;

   MCW_reghelp_children( func->range_bbox->wrowcol ,
                         "This button determines whether the program\n"
                         "or the user sets the Func value that maps\n"
                         "to the color pbar level 1.0:\n\n"
                         "Pressed In: use 'autoRange' value for pbar 1.0\n"
                         "       Out: user controls Range value (below)"
                       ) ;
   MCW_reghint_children( func->range_bbox->wrowcol ,
                         "Automatic or user-controlled color range scaling" ) ;

   ADDTO_KILL(im3d->kl,func->range_bbox) ;

   /*--- 30 Mar 2001: put the next 2 things in a horizontal rowcol ---*/

   hrc = XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->range_rowcol ,
            XmNorientation , XmHORIZONTAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- arrowval to provide user control for pbar scaling ---*/

   func->range_av =
      new_MCW_arrowval(
         hrc ,                             /* parent */
         NULL ,                            /* label */
         MCW_AV_downup ,                   /* arrow directions */
         0  ,                              /* min value */
         9999999 ,                         /* max value */
         (int) (im3d->vinfo->fim_range) ,  /* init value */
         MCW_AV_editext ,                  /* input/output text display */
         0 ,                               /* decimal shift */
         AFNI_range_av_CB ,                /* routine to call when button */
         (XtPointer) im3d ,                /* is pressed, and its data */
         NULL,NULL                         /* no special display */
      ) ;

   func->range_av->parent = (XtPointer) im3d ;

   MCW_reghelp_children( func->range_av->wrowcol ,
                         "When the autoRange button above is Out, this\n"
                         "selector is used to set the Func level which\n"
                         "maps to 1.0 on the color pbar."
                       ) ;
   MCW_reghint_children( func->range_av->wrowcol ,
                         "Func value that maps to 1.0 for color overlay" ) ;

   ADDTO_KILL(im3d->kl,func->range_av) ;

   AV_SENSITIZE( func->range_av , ! im3d->vinfo->use_autorange ) ;

   /*--- 30 Mar 2001: rotate pbar ---*/

   func->range_rotate_av = new_MCW_arrowval(
                             hrc , "Rota" ,
                             MCW_AV_downup , 0,0,0 ,
                             MCW_AV_notext , 0 ,
                             AFNI_range_rotate_av_CB , (XtPointer) func->inten_pbar ,
                             NULL,NULL ) ;

   func->range_rotate_av->parent = (XtPointer) im3d ;

   MCW_reghelp_children( func->range_rotate_av->wrowcol ,
                         "Rotate the colors on\n"
                         "the 'pbar' up or down." ) ;
   MCW_reghint_children( func->range_rotate_av->wrowcol ,
                         "Rotate pbar colors" ) ;

   ADDTO_KILL(im3d->kl,func->range_rotate_av) ;

   XtManageChild( hrc ) ;

#ifdef USE_FUNC_FIM
   /*--- fim execution controls ---*/

   func->fim_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , func->options_rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->fim_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->fim_frame ,
            XmNorientation , XmHORIZONTAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->fim_mbar =
         XmCreateMenuBar( func->fim_rowcol, "dialog" , NULL,0 ) ;
   XtVaSetValues( func->fim_mbar ,
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNspacing      , 3 ,
                     XmNborderWidth  , 0 ,
                     XmNtraversalOn  , False ,
                  NULL ) ;
   XtManageChild( func->fim_mbar ) ;

   func->fim_menu = AFNI_new_fim_menu( func->fim_mbar , AFNI_fimmer_menu_CB , 0 ) ;
   func->fim_menu->parent = (XtPointer) im3d ;

   xstr = XmStringCreateLtoR("1234567890123456789",XmFONTLIST_DEFAULT_TAG );
   func->fim_dset_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , func->fim_rowcol ,
            XmNrecomputeSize , False ,
            XmNlabelString , xstr ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XmStringFree(xstr) ;
   MCW_register_help( func->fim_dset_label ,
         "Shows the name of the\n"
         "dataset for which FIM\n"
         "is currently set up."
   ) ;
   MCW_register_hint( func->fim_dset_label , "Dataset to be FIM-ed") ;
#endif

   /* 25 Jul 2001: a toggle box to show the TT Atlas */

   { char *see_ttatlas_label[1] = { "See TT Atlas Regions" } ;
     func->see_ttatlas_bbox =
      new_MCW_bbox( func->options_rowcol ,
                    1 , see_ttatlas_label ,
                    MCW_BB_check ,
                    MCW_BB_frame ,
                    AFNI_see_ttatlas_CB , (XtPointer) im3d ) ;

     func->see_ttatlas_bbox->parent = (XtPointer) im3d ;

     MCW_set_bbox( func->see_ttatlas_bbox ,
                   (im3d->vinfo->see_ttatlas) ? (1) : (0) ) ;

     MCW_reghelp_children( func->see_ttatlas_bbox->wrowcol ,
                           "This button determines whether to show\n"
                           "the Talairach-Tournoux Atlas regions,\n"
                           "which are controlled by the 'Atlas Colors'\n"
                           "item on the image viewing window popup menu."
                         ) ;
     MCW_reghint_children( func->see_ttatlas_bbox->wrowcol ,
                           "Use 'Atlas Colors' from image popup menu" ) ;

     ADDTO_KILL(im3d->kl,func->see_ttatlas_bbox) ;
   }

#ifdef ALLOW_BKGD_LAB
   xstr = XmStringCreateLtoR( "Anat = xxxxxxxxxxxxxxxx\n"
                              "Func = xxxxxxxxxxxxxxxx\n"
                              "Thr  = xxxxxxxxxxxxxxxx" ,
                            XmFONTLIST_DEFAULT_TAG ) ;

   func->bkgd_lab =
      XtVaCreateWidget(
         "dialog" , xmLabelWidgetClass , func->options_rowcol ,
            XmNrecomputeSize , False ,
            XmNlabelString , xstr ,
            XmNmarginHeight, 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XmStringFree( xstr ) ;

   im3d->vinfo->anat_val[0] =
    im3d->vinfo->func_val[0] =
     im3d->vinfo->thr_val[0]  = '\0' ;

   MCW_register_help( func->bkgd_lab ,
                      "Shows the values at the\n"
                      "current crosshair voxel.\n"
                      "\n"
                      "N.B.: Is only active when\n"
                      " all 3 image windows are\n"
                      " open!"
                    ) ;
   MCW_register_hint( func->bkgd_lab , "Values at crosshairs voxel" ) ;
#else
   func->bkgd_lab = NULL ;
#endif

   /*-- manage the managers --*/

   XtManageChild( func->thr_rowcol ) ;
   XtManageChild( func->inten_rowcol ) ;
   XtManageChild( func->range_rowcol ) ;
   XtManageChild( func->options_rowcol ) ;
#ifdef USE_FUNC_FIM
   XtManageChild( func->fim_rowcol ) ;
#endif
   XtManageChild( func->rowcol ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void AFNI_make_wid3( Three_D_View * im3d )
{

ENTRY("AFNI_make_wid3") ;

   /*-----------------------------------------------------*/
   /**************** Datamode controls ********************/
   /*-----------------------------------------------------*/

STATUS("making dmode->rowcol") ;

   dmode->rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , dmode->frame ,
            XmNorientation , XmVERTICAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- bbox to control how we see the anat data ---*/

   dmode->anatmode_bbox =
      new_MCW_bbox( dmode->rowcol ,
                    2 , AFNI_anatmode_bbox_label ,
                    MCW_BB_radio_one ,
                    MCW_BB_frame ,
                    AFNI_anatmode_CB , (XtPointer) im3d ) ;

   dmode->anatmode_bbox->parent = (XtPointer) im3d ;

   MCW_reghelp_children( dmode->anatmode_bbox->wrowcol ,
     "View Anat Data Brick ==> data from anatomy file is displayed\n"
     "                   (will be grayed-out if data is not available)\n"
     "Warp Anat on Demand  ==> data is resampled as needed for display" ) ;

   { char * hh[] = { "View data direct from brick" ,
                     "View data resampled to new grid" } ;
     MCW_bbox_hints( dmode->anatmode_bbox , 2 , hh ) ;
   }

   ADDTO_KILL(im3d->kl,dmode->anatmode_bbox) ;

   /*-- resampling control: anat mode --*/

   dmode->anat_resam_av = new_MCW_arrowval(
                             dmode->rowcol ,
                             "Anat resam mode" ,
                             AVOPT_STYLE ,
                             FIRST_RESAM_TYPE ,
                             LAST_RESAM_TYPE ,
                             im3d->vinfo->anat_resam_mode ,
                             MCW_AV_readtext , 0 ,
                             AFNI_resam_av_CB , (XtPointer) im3d ,
                             AFNI_resam_texter , NULL ) ;

   dmode->anat_resam_av->parent     = (XtPointer) im3d ;
   dmode->anat_resam_av->allow_wrap = 1 ;         /* wrap values */
   dmode->anat_resam_av->fastdelay  = 1000 ;      /* slow it down */

#ifndef USE_OPTMENUS
   XtVaSetValues( dmode->anat_resam_av->wtext ,
                     XmNcolumns   , NSTR_SHORT_RESAM ,
                     XmNmaxLength , NSTR_SHORT_RESAM ,
                  NULL ) ;
#endif

   MCW_reghelp_children( dmode->anat_resam_av->wrowcol ,
     "This controls the resampling mode for\n"
     "anatomical data (for display and writing):\n\n"
     "NN = nearest neighbor resampling [fastest]\n"
     "Li = linear interpolation        [OK]\n"
     "Cu = cubic interpolation         [nice but slow]\n"
     "Bk = blocky interpolation        [between NN & Li]"  ) ;
   MCW_reghint_children( dmode->anat_resam_av->wrowcol , "Resampling method" ) ;

   ADDTO_KILL(im3d->kl,dmode->anat_resam_av) ;

   /*----- resampling: voxel size -----*/

   dmode->resam_vox_av =
      new_MCW_arrowval(
         dmode->rowcol ,                  /* parent */
         "Resam (mm)" ,                   /* label */
         MCW_AV_downup ,                  /* arrow directions */
         1  ,                             /* min value (0.1 mm from decim) */
         40 ,                             /* max value (4.0 mm) */
         (int)(10*INIT_resam_vox) ,       /* init value */
         MCW_AV_editext ,                 /* input/output text display */
         1 ,                              /* decimal shift */
         AFNI_resam_vox_av_CB ,           /* routine to call when button */
         (XtPointer) im3d ,               /* is pressed, and its data */
         NULL,NULL                        /* no special display */
      ) ;

   XtVaSetValues( dmode->resam_vox_av->wtext , XmNcolumns , 7 , NULL ) ;

   dmode->resam_vox_av->parent = (XtPointer) im3d ;

   MCW_reghelp_children( dmode->resam_vox_av->wrowcol ,
                         AFNI_disp_resam_vox_help ) ;
   MCW_reghint_children( dmode->resam_vox_av->wrowcol ,
                         "Resampled voxel size" ) ;

   ADDTO_KILL(im3d->kl,dmode->resam_vox_av) ;

   /*--- separator between anat and func stuff ---*/

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , dmode->rowcol ,
                XmNseparatorType , XmDOUBLE_LINE ,
            NULL ) ;

   /*--- bbox to control how we see the func data ---*/

   dmode->funcmode_bbox =
      new_MCW_bbox( dmode->rowcol ,
                    2 , AFNI_funcmode_bbox_label ,
                    MCW_BB_radio_one ,
                    MCW_BB_frame ,
                    AFNI_funcmode_CB , (XtPointer) im3d ) ;

   dmode->funcmode_bbox->parent = (XtPointer) im3d ;

   MCW_reghelp_children( dmode->funcmode_bbox->wrowcol ,
     "View Func Data Brick ==> data from functional file is displayed\n"
     "                   (will be grayed-out if data is not available)\n"
     "Warp Func on Demand  ==> data is resampled as needed for display\n\n"
     "N.B.: Functional data is always overlaid on anatomical data.\n"
     "  To be displayed directly from the functional data brick,\n"
     "  this brick must conform in dimensions to the anatomical\n"
     "  data being displayed.  Even if the functional brick exists,\n"
     "  if its dimensions do not correspond to the anatomical brick\n"
     "  or the resampling dimension (below), then the functional data\n"
     "  being displayed will be 'warped-on-demand'.  Such warping\n"
     "  always occurs from the 'most original' source.  For example,\n"
     "  if a Talairach view brick is altered (via a plugin, or another\n"
     "  external editing program), then viewing the brick may be quite\n"
     "  different from viewing the warped data, which will be recomputed\n"
     "  from the Original view brick (if available), without reference\n"
     "  to whatever alterations may have been made in the Talairach view."
   ) ;

   { char * hh[] = { "View data direct from brick" ,
                     "View data resampled to new grid" } ;
     MCW_bbox_hints( dmode->funcmode_bbox , 2 , hh ) ;
   }

   ADDTO_KILL(im3d->kl,dmode->funcmode_bbox) ;

   /*-- func resampling control (moved here 03 Nov 1996) --*/

   dmode->func_resam_av = new_MCW_arrowval(
                             dmode->rowcol ,
#ifdef USE_OPTMENUS
                             "Func resam mode" ,
#else
                             "Func mode " ,
#endif
                             AVOPT_STYLE ,
                             FIRST_RESAM_TYPE ,
                             LAST_RESAM_TYPE ,
                             im3d->vinfo->func_resam_mode ,
                             MCW_AV_readtext , 0 ,
                             AFNI_resam_av_CB , (XtPointer) im3d ,
                             AFNI_resam_texter , NULL ) ;

   dmode->func_resam_av->parent     = (XtPointer) im3d ;
   dmode->func_resam_av->allow_wrap = 1 ;       /* wrap values */
   dmode->func_resam_av->fastdelay  = 1000 ;    /* slow it down */

#ifndef USE_OPTMENUS
   XtVaSetValues( dmode->func_resam_av->wtext ,
                     XmNcolumns   , NSTR_SHORT_RESAM ,
                     XmNmaxLength , NSTR_SHORT_RESAM ,
                  NULL ) ;
#endif

   MCW_reghelp_children( dmode->func_resam_av->wrowcol ,
     "This controls the resampling mode for\n"
     "functional data (display and writing):\n\n"
     "NN = nearest neighbor resampling [fastest]\n"
     "Li = linear interpolation        [OK]\n"
     "Cu = cubic interpolation         [nice but slow]\n"
     "Bk = blocky interpolation        [between NN & Li]\n\n"
     "N.B.: Dataset sub-bricks without statistical\n"
     "  parameters attached will be interpolated using\n"
     "  this method.  Those with statistical parameters\n"
     "  will be interpolated using the method chosen below." ) ;

   MCW_reghint_children( dmode->func_resam_av->wrowcol , "Resampling method" ) ;

   /*-- thr resampling control (09 Dec 1997) --*/

   dmode->thr_resam_av = new_MCW_arrowval(
                             dmode->rowcol ,
#ifdef USE_OPTMENUS
                             "Thr  resam mode" ,
#else
                             "Thr mode  " ,
#endif
                             AVOPT_STYLE ,
                             FIRST_RESAM_TYPE ,
                             LAST_RESAM_TYPE ,
                             im3d->vinfo->thr_resam_mode ,
                             MCW_AV_readtext , 0 ,
                             AFNI_resam_av_CB , (XtPointer) im3d ,
                             AFNI_resam_texter , NULL ) ;

   dmode->thr_resam_av->parent     = (XtPointer) im3d ;
   dmode->thr_resam_av->allow_wrap = 1 ;       /* wrap values */
   dmode->thr_resam_av->fastdelay  = 1000 ;    /* slow it down */

#ifndef USE_OPTMENUS
   XtVaSetValues( dmode->thr_resam_av->wtext ,
                     XmNcolumns   , NSTR_SHORT_RESAM ,
                     XmNmaxLength , NSTR_SHORT_RESAM ,
                  NULL ) ;
#endif

   MCW_reghelp_children( dmode->thr_resam_av->wrowcol ,
     "This controls the resampling mode for\n"
     "functional data (threshold only):\n\n"
     "NN = nearest neighbor resampling [fastest]\n"
     "Li = linear interpolation        [OK]\n"
     "Cu = cubic interpolation         [nice but slow]\n"
     "Bk = blocky interpolation        [between NN & Li]\n\n"
     "N.B.: Dataset sub-bricks without statistical\n"
     "  parameters attached will be interpolated using\n"
     "  the method chosen above.  Those with statistical\n"
     "   parameters will be interpolated using this method." ) ;

   MCW_reghint_children( dmode->thr_resam_av->wrowcol , "Resampling method" ) ;

   /*--- separator between func stuff and write buttons ---*/

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , dmode->rowcol ,
                XmNseparatorType , XmDOUBLE_LINE ,
            NULL ) ;

   /*---- 23 Nov 1996: rowcol for Write buttons ----*/

   dmode->write_rowcol =
        XtVaCreateWidget(
           "dialog" , xmRowColumnWidgetClass , dmode->rowcol ,
              XmNorientation , XmHORIZONTAL ,
              XmNpacking , XmPACK_TIGHT ,
              XmNmarginHeight , 0 ,
              XmNmarginWidth  , 0 ,
              XmNspacing      , 1 ,
              XmNtraversalOn , False ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   /*-- 23 Nov 1996: label at left --*/

   (void) XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , dmode->write_rowcol ,
            LABEL_ARG("Write ") ,
            XmNalignment , XmALIGNMENT_BEGINNING ,
            XmNrecomputeSize , False ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- write pushbuttons ---*/

   dmode->write_anat_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->write_rowcol ,
            LABEL_ARG("Anat") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( dmode->write_anat_pb , XmNactivateCallback ,
                  AFNI_write_dataset_CB , im3d ) ;

   MCW_register_hint( dmode->write_anat_pb ,
                      "Write current anatomy to disk" ) ;

   dmode->write_func_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->write_rowcol ,
            LABEL_ARG("Func") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( dmode->write_func_pb , XmNactivateCallback ,
                  AFNI_write_dataset_CB , im3d ) ;

   MCW_register_hint( dmode->write_func_pb ,
                      "Write current function to disk" ) ;

   dmode->write_many_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->write_rowcol ,
            LABEL_ARG("Many") ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( dmode->write_many_pb , XmNactivateCallback ,
                  AFNI_write_many_dataset_CB , im3d ) ;

   MCW_register_hint( dmode->write_many_pb ,
                      "Write multiple datasets to disk" ) ;

   MCW_reghelp_children( dmode->write_rowcol ,
        "The purpose of the `Write' buttons is to recompute\n"
        "entire dataset bricks in the current coordinate\n"
        "system (`view') and write them to disk.\n"
        "\n"
        "The `Resam' controls determine the resolution and\n"
        "interpolation used in creating the new bricks.\n"
        "\n"
        "Anat --> current anatomical dataset brick.\n"
        "Func --> current functional dataset brick.\n"
        "Many --> select one or more datasets from a list.\n"
        "\n"
        "N.B.:\n"
        " + Only dataset bricks that are warped from\n"
        "    a `parent' dataset can be written out.\n"
        "    AFNI will not destroy original data (I hope).\n"
        " + This operation may be very time-consuming,\n"
        "    especially for 3D+time datasets!"
      ) ;

   /*---- 23 Nov 1996: Row of Buttons for Rescan Session ----*/

   dmode->rescan_rowcol =
        XtVaCreateWidget(
           "dialog" , xmRowColumnWidgetClass , dmode->rowcol ,
              XmNorientation , XmHORIZONTAL ,
              XmNpacking , XmPACK_TIGHT ,
              XmNmarginHeight , 0 ,
              XmNmarginWidth  , 0 ,
              XmNspacing      , 1 ,
              XmNtraversalOn , False ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   /*-- 23 Nov 1996: label at left --*/

   (void) XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , dmode->rescan_rowcol ,
            LABEL_ARG("Rescan") ,
            XmNalignment , XmALIGNMENT_BEGINNING ,
            XmNrecomputeSize , False ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*-- pushbutton for one session rescan --*/

   dmode->rescan_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->rescan_rowcol ,
            LABEL_ARG("This") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( dmode->rescan_pb , XmNactivateCallback ,
                  AFNI_rescan_CB , im3d ) ;

   MCW_register_hint( dmode->rescan_pb ,
                      "Read current session again" ) ;

   /*-- 23 Nov 1996: pushbutton for all session rescan --*/

   dmode->rescan_all_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->rescan_rowcol ,
            LABEL_ARG("All ") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( dmode->rescan_all_pb , XmNactivateCallback ,
                  AFNI_rescan_all_CB , im3d ) ;

   MCW_register_hint( dmode->rescan_all_pb ,
                      "Read all sessions again" ) ;

   dmode->rescan_timeseries_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->rescan_rowcol ,
            LABEL_ARG("*.1D") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( dmode->rescan_timeseries_pb , XmNactivateCallback ,
                  AFNI_rescan_timeseries_CB , im3d ) ;

   MCW_register_hint( dmode->rescan_timeseries_pb ,
                      "Read directories for new time series files" ) ;

   MCW_reghelp_children( dmode->rescan_rowcol ,
         "The purpose of the `Rescan' buttons is to read\n"
         "the contents of session directories again in\n"
         "order to make newly created datasets (e.g., from\n"
         "the 3dmerge program) available for AFNI viewing.\n"
         "\n"
         "This --> rescan just the current session.\n"
         "All  --> rescan all session directories.\n"
         "*.1D --> rescan for timeseries files instead\n"
         "         of AFNI datasets.  Note that the program\n"
         "         won't re-read a filename that has already\n"
         "         been read in.  This means that if you change\n"
         "         the contents of a .1D file, AFNI will not\n"
         "         be aware of that fact even after this rescan\n"
         "         operation."
      ) ;

   /*---- 04 Mar 1997: Row of Buttons for Reading ----*/

   dmode->read_rowcol =
        XtVaCreateWidget(
           "dialog" , xmRowColumnWidgetClass , dmode->rowcol ,
              XmNorientation , XmHORIZONTAL ,
              XmNpacking , XmPACK_TIGHT ,
              XmNmarginHeight , 0 ,
              XmNmarginWidth  , 0 ,
              XmNspacing      , 1 ,
              XmNtraversalOn , False ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   /*-- label at left --*/

   (void) XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , dmode->read_rowcol ,
            LABEL_ARG("Read  ") ,
            XmNalignment , XmALIGNMENT_BEGINNING ,
            XmNrecomputeSize , False ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*-- pushbutton for session input --*/

   dmode->read_sess_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->read_rowcol ,
            LABEL_ARG("Sess") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( dmode->read_sess_pb , XmNactivateCallback ,
                  AFNI_read_sess_CB , im3d ) ;

   MCW_register_hint( dmode->read_sess_pb ,
                      "Read in a new session directory" ) ;

   /*-- pushbutton for timeseries input --*/

   dmode->read_1D_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->read_rowcol ,
            LABEL_ARG(" 1D ") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( dmode->read_1D_pb , XmNactivateCallback ,
                  AFNI_read_1D_CB , im3d ) ;

   MCW_register_hint( dmode->read_1D_pb ,
                      "Read in a new time series file" ) ;

   /*-- pushbutton for Web input --*/

   dmode->read_Web_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->read_rowcol ,
            LABEL_ARG("Web ") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( dmode->read_Web_pb , XmNactivateCallback ,
                  AFNI_read_Web_CB , im3d ) ;

   MCW_register_hint( dmode->read_Web_pb ,
                      "Read dataset via http:// or ftp://" ) ;

   MCW_reghelp_children( dmode->read_rowcol ,
         "The purpose of the `Read' buttons is to read\n"
         "in new data.  (The `Rescan' buttons are to\n"
         "re-read data from old directories.)\n"
         "\n"
         "Sess --> Read a new session directory.\n\n"
         "1D   --> Read a new timeseries file.\n\n"
         "Web  --> Read datasets from the Web:\n"
         "   e.g., http://some.place/dir/anat+orig\n"
         "         ftp://some.place/dir/func+orig\n"
         "   or    http://some.place/dir/AFNILIST\n"
         "   where AFNILIST is a text file with one\n"
         "         dataset name per line (will be\n"
         "         fetched from same Web directory;\n"
         "         do NOT put ftp:// or http:// in\n"
         "         the AFNILIST file!)."
      ) ;

   /*---- 04 Nov 1996: a place to put menubars ----*/

   dmode->mbar_rowcol =
        XtVaCreateWidget(
           "dialog" , xmRowColumnWidgetClass , dmode->rowcol ,
              XmNorientation , XmHORIZONTAL ,
              XmNpacking , XmPACK_TIGHT ,
              XmNmarginHeight , 0 ,
              XmNmarginWidth  , 0 ,
              XmNspacing      , 1 ,
              XmNtraversalOn , False ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   /* 04 Nov 1996: make the "Lock" menubar */

   AFNI_lock_button( im3d ) ;

   /* 30 Oct 1996: make the "Misc" menubar */

   AFNI_misc_button( im3d ) ;

   /*-- manage the managers --*/

   XtManageChild( dmode->write_rowcol ) ;
   XtManageChild( dmode->rescan_rowcol ) ;
   XtManageChild( dmode->read_rowcol ) ;
   XtManageChild( dmode->mbar_rowcol ) ;
   XtManageChild( dmode->rowcol ) ;

   /*--------------------------------------------------*/
   /*************** Program controls *******************/
   /*--------------------------------------------------*/

   /*----- rowcol to hold all program controls stuff -----*/

STATUS("making prog->rowcol") ;

   prog->rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , prog->frame ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmVERTICAL ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   if( im3d->type == AFNI_3DDATA_VIEW ){
      prog->rc_top =
         XtVaCreateWidget(
            "dialog" , xmRowColumnWidgetClass , prog->rowcol ,
               XmNpacking     , XmPACK_TIGHT ,
               XmNorientation , XmHORIZONTAL ,
               XmNspacing     , 1 ,
               XmNmarginHeight, 0 ,
               XmNmarginWidth , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      if( MAX_CONTROLLERS > 1 ){
         prog->clone_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->rc_top ,
                 LABEL_ARG("New  ") ,
                 XmNtraversalOn , False ,
                 XmNinitialResourcesPersistent , False ,
               NULL ) ;

         XtAddCallback( prog->clone_pb , XmNactivateCallback ,
                        AFNI_clone_controller_CB , im3d ) ;

         MCW_register_help( prog->clone_pb ,
                            "Use this to open\n"
                            "a new AFNI control\n"
                            "window."
                          ) ;
         MCW_register_hint( prog->clone_pb ,
                            "Open a new AFNI controller window" ) ;
      } else {
          prog->clone_pb = NULL ;
      }

      prog->panel_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , prog->rc_top ,
              LABEL_ARG("Views") ,
              XmNtraversalOn , False ,
              XmNinitialResourcesPersistent , False ,
            NULL ) ;

      prog->panel_pb_inverted = True ;
      MCW_invert_widget( prog->panel_pb ) ;

      MCW_register_help( prog->panel_pb ,
                         "Use this to close and open\n"
                         "the viewing control panel\n"
                         "located to the right."
                       ) ;
      MCW_register_hint( prog->panel_pb ,
                         "Open/close control panel to right" ) ;

      XtAddCallback( prog->panel_pb , XmNactivateCallback ,
                     AFNI_controller_panel_CB , im3d ) ;

   } else {
      prog->rc_top = prog->clone_pb = prog->panel_pb = NULL ;
   }

   /*----- pushbutton: button help -----*/

   prog->rc_bot =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , prog->rowcol ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmHORIZONTAL ,
            XmNspacing     , 1 ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   prog->button_help_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , prog->rc_bot ,
           LABEL_ARG("BHelp") ,
           XmNtraversalOn , False ,
           XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( prog->button_help_pb , XmNactivateCallback ,
                  MCW_click_help_CB , im3d ) ;

   MCW_register_help( prog->button_help_pb , AFNI_help_help ) ;
   MCW_register_hint( prog->button_help_pb , "Gets more help for a button" ) ;

   /*----- pushbutton: quit -----*/

   prog->quit_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , prog->rc_bot ,
            LABEL_ARG("done ") ,
            XmNrecomputeSize , False ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
            XmNuserData , (XtPointer) im3d ,
         NULL ) ;

   MCW_set_widget_bg( prog->quit_pb , MCW_hotcolor(prog->quit_pb) , 0 ) ;

   XtAddCallback( prog->quit_pb , XmNactivateCallback ,
                  AFNI_quit_CB , im3d ) ;

   MCW_register_help( prog->quit_pb , AFNI_quit_help ) ;
   MCW_register_hint( prog->quit_pb , "Click twice to close window" ) ;

   prog->quit_first = True ;  /* mark this button as not pressed yet */

   /*----- manage the managers -----*/

   if( prog->rc_top != NULL ) XtManageChild( prog->rc_top ) ;
   XtManageChild( prog->rc_bot ) ;
   XtManageChild( prog->rowcol ) ;

   /*----------------------------------------*/
   /*  for the overlapping panels, make the  */
   /*  help callback raise them to the top   */
   /*----------------------------------------*/

#if 0
   XtAddCallback( marks->rowcol , XmNhelpCallback , AFNI_raiseup_CB , NULL ) ;
   XtAddCallback( func->rowcol  , XmNhelpCallback , AFNI_raiseup_CB , NULL ) ;
   XtAddCallback( dmode->rowcol , XmNhelpCallback , AFNI_raiseup_CB , NULL ) ;
#endif

   /*-------------------------------------------------------------*/
   /*----- stuff here that goes below all else on popup menu -----*/
   /*-------------------------------------------------------------*/

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , imag->popmenu ,
                XmNseparatorType , XmDOUBLE_LINE ,
            NULL ) ;

   /*--- label for background pixel ---*/

#ifdef ALLOW_BKGD_LAB
   imag->pop_bkgd_lab =
      XtVaCreateWidget(
         "dialog" , xmLabelWidgetClass , imag->popmenu ,
            LABEL_ARG("   bkgd =xxxxxx") ,
            XmNalignment , XmALIGNMENT_BEGINNING ,
            XmNrecomputeSize , False ,
            XmNtraversalOn , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
#else
   imag->pop_bkgd_lab = NULL ;
#endif

   imag->do_bkgd_lab = False ;

   /*----------------------------------------------*/
   /**************  make a picture  ****************/
   /*----------------------------------------------*/

   vwid->picture       = NULL ;  /* default ==> no picture */
   vwid->picture_index = 0 ;

#ifdef WANT_LOGO_BITMAP
   if( im3d->type == AFNI_3DDATA_VIEW ){
      vwid->picture =
          XtVaCreateManagedWidget(
             "dialog" , xmLabelWidgetClass , vwid->top_form ,
                 XmNtopAttachment    , XmATTACH_WIDGET ,
                 XmNtopWidget        , imag->frame ,
                 XmNtopOffset        , 0 ,
                 XmNleftAttachment   , XmATTACH_WIDGET ,
                 XmNleftWidget       , prog->frame ,
                 XmNleftOffset       , 0 ,
                 XmNlabelType        , XmPIXMAP ,
                 XmNalignment        , XmALIGNMENT_CENTER ,
#ifdef OLD_PICTURE
                 XmNlabelInsensitivePixmap , logo_pixmap ,
#endif
                 XmNwidth        , logo_width ,
                 XmNheight       , logo_height ,
                 XmNmarginWidth  , 0 ,
                 XmNmarginHeight , 0 ,
                 XmNrecomputeSize, False ,
                 XmNtraversalOn  , False ,
                 XmNinitialResourcesPersistent , False ,
             NULL ) ;
      MCW_register_help( vwid->picture , AFNI_abohelp ) ;
   }
#else
   MCW_register_help( imag->rowcol  , AFNI_abohelp ) ;
   MCW_register_help( view->rowcol  , AFNI_abohelp ) ;
   MCW_register_help( marks->rowcol , AFNI_abohelp ) ;
   MCW_register_help( func->rowcol  , AFNI_abohelp ) ;
   MCW_register_help( dmode->rowcol , AFNI_abohelp ) ;
   MCW_register_help( prog->rowcol  , AFNI_abohelp ) ;
#endif

   /*** load icon, if desired and possible ***/

#ifdef WANT_AFNI_BITMAP
#ifndef DONT_INSTALL_ICONS
   if( afni48_pixmap != XmUNSPECIFIED_PIXMAP ){
      Boolean good ;

      good = MCW_check_iconsize( afni48_width,afni48_height , im3d->dc ) ;

      if( good ){
         XtVaSetValues( im3d->vwid->top_shell ,
                           XmNiconPixmap , afni48_pixmap ,
                        NULL ) ;
         afni48_good = 1 ;
      }
   }
#endif
#endif

   /*---------------------------------------------------*/
   /*********** hidden popup menu on picture ************/
   /*---------------------------------------------------*/

   prog->hidden_menu = NULL ;

#ifdef USE_HIDDEN
   if( vwid->picture != NULL ){

      /** popup on picture widget (right of Quit button) **/

      prog->hidden_menu =
         XmCreatePopupMenu( vwid->picture , "menu" , NULL , 0 ) ;

      SAVEUNDERIZE(XtParent(prog->hidden_menu)) ; /* 27 Feb 2001 */

      VISIBILIZE_WHEN_MAPPED(prog->hidden_menu) ;

/***
      XtAddCallback( prog->hidden_menu ,
                     XmNunmapCallback , AFNI_hidden_CB , im3d ) ;
***/

/***
      XtVaSetValues( prog->hidden_menu ,
                        XmNspacing      , 1 ,
                        XmNmarginHeight , 0 ,
                        XmNmarginWidth  , 0 ,
                     NULL ) ;
***/

      XtInsertEventHandler( vwid->picture ,      /* handle events in picture */

                               0
                             | ButtonPressMask   /* button presses */
                            ,
                            FALSE ,              /* nonmaskable events? */
                            AFNI_hidden_EV ,     /* handler */
                            (XtPointer) im3d ,   /* client data */
                            XtListTail           /* last in queue */
                          ) ;

      /**--- pullright menu for points ---**/

      prog->hidden_pts_menu =
         XmCreatePulldownMenu( prog->hidden_menu , "menu" , NULL , 0 ) ;

      VISIBILIZE_WHEN_MAPPED(prog->hidden_pts_menu) ;

      /** cascade button to bring this menu up **/

      prog->hidden_pts_cbut =
         XtVaCreateManagedWidget(
            "dialog" , xmCascadeButtonWidgetClass , prog->hidden_menu ,
               LABEL_ARG("Points List") ,
               XmNsubMenuId , prog->hidden_pts_menu ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      /** "Read Pts: IJK" button in pts menu **/

      prog->hidden_readpts_ijk_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , prog->hidden_pts_menu ,
               LABEL_ARG("Read Pts:  IJK") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      XtAddCallback( prog->hidden_readpts_ijk_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;

      /** "Read Pts: XYZ" button in pts menu **/

      prog->hidden_readpts_xyz_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , prog->hidden_pts_menu ,
               LABEL_ARG("Read Pts:  XYZ") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      XtAddCallback( prog->hidden_readpts_xyz_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;

      /** "Write Pts: IJK" button in pts menu **/

      prog->hidden_writepts_ijk_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , prog->hidden_pts_menu ,
               LABEL_ARG("Write Pts: IJK") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      XtAddCallback( prog->hidden_writepts_ijk_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;

      /** "Write Pts: XYZ" button in pts menu **/

      prog->hidden_writepts_xyz_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , prog->hidden_pts_menu ,
               LABEL_ARG("Write Pts: XYZ") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      XtAddCallback( prog->hidden_writepts_xyz_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;

      /** "Set Pts: COLOR" button in pts menu **/

      prog->hidden_colorpts_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , prog->hidden_pts_menu ,
               LABEL_ARG("Set Pts: COLOR") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      XtAddCallback( prog->hidden_colorpts_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;

      /*---- END OF PTS STUFF ----*/

      /*-----------------------------------*/
      /*-- pushbutton for sonnet display --*/
      /*-----------------------------------*/

#ifdef USE_SONNETS
      if( ! NO_frivolities ){
         prog->hidden_sonnet_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->hidden_menu ,
                  LABEL_ARG("Sonnet") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , False ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;

         XtAddCallback( prog->hidden_sonnet_pb , XmNactivateCallback ,
                        AFNI_sonnet_CB , im3d ) ;
      }
#endif

      prog->hidden_mission_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->hidden_menu ,
                  LABEL_ARG("Mission") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , False ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( prog->hidden_mission_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;

   }
#endif  /* USE_HIDDEN */

   EXRETURN ;
}

/*--------------------------------------------------------------------------
   Find out how many controller windows are active at this instant
----------------------------------------------------------------------------*/

int AFNI_count_controllers(void)
{
   int ii , cnt ;

ENTRY("AFNI_count_controllers") ;

   for( ii=0,cnt=0 ; ii < MAX_CONTROLLERS ; ii++ )
      if( IM3D_OPEN(GLOBAL_library.controllers[ii]) ) cnt++ ;

   RETURN(cnt) ;
}

/*-------------------------------------------------------------------
  Find out which controller this is.  Return -1 if there is an error.
---------------------------------------------------------------------*/

int AFNI_controller_index( Three_D_View * im3d )
{
   int ii ;

ENTRY("AFNI_controller_index") ;

   if( ! IM3D_VALID(im3d) ) RETURN(-1) ;

   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ )
      if( GLOBAL_library.controllers[ii] == im3d ) RETURN(ii) ;

   RETURN(-1) ;
}

/*---------------------------------------------------------------------------
   Create and return a new controller for AFNI.
   Inputs:
   "shell" is a top level shell, to contain the entire widget hierarchy.
      If this shell is NULL, then the routine will create it.
      Normally, you would pass in the results of XtVaAppInitialize
      as the first controller's shell, and let the routine create
      any subsequent shells needed.
   "dc" must be created by MCW_new_DC before this routine.  It contains
      all the information pertaining to the X11 display.
   "im3d_type" is one of AFNI_3DDATA_VIEW or AFNI_IMAGES_VIEW.  The former
      is for viewing 3D datasets; the latter is a restricted version for
      viewing images only.
-----------------------------------------------------------------------------*/

Three_D_View * new_AFNI_controller( Widget shell , MCW_DC * dc , int im3d_type )
{
   Three_D_View * im3d ;
   int ii , last_color ;

ENTRY("new_AFNI_controller") ;

   /*-- create the basic stuff --*/

   im3d = myXtNew(Three_D_View) ; INIT_KILL(im3d->kl) ;

   im3d->vwid   = myXtNew(AFNI_widget_set) ; ADDTO_KILL(im3d->kl,im3d->vwid) ;
   im3d->type   = im3d_type ;
   im3d->opened = 0 ;          /* not yet opened up */
   im3d->dc     = dc ;
   im3d->vinfo  = myXtNew( AFNI_view_info ); ADDTO_KILL(im3d->kl,im3d->vinfo);

   im3d->brand_new = 1 ; /* 07 Dec 2001 */

   im3d->fimdata = myXtNew( AFNI_fimmer_type ); ADDTO_KILL(im3d->kl,im3d->fimdata);
   CLEAR_FIMDATA(im3d) ;

   strcpy( im3d->window_title , "GPL AFNI" ) ;

   if( shell != NULL ){
      im3d->vwid->top_shell = shell ;
   } else {
      im3d->vwid->top_shell = XtVaAppCreateShell(
                                 "AFNI" , "AFNI" ,
                                    topLevelShellWidgetClass , dc->display ,
                                    XmNinitialResourcesPersistent , False ,
                                 NULL ) ;
   }

   DC_yokify( im3d->vwid->top_shell , dc ) ;

   XtVaSetValues( im3d->vwid->top_shell ,
                     XmNtitle    , im3d->window_title ,
                     XmNallowShellResize , True ,       /* let code resize shell */
                     XmNdeleteResponse , XmDO_NOTHING , /* deletion handled below */
                  NULL ) ;

   XmAddWMProtocolCallback(           /* make "Close" window menu work */
           im3d->vwid->top_shell ,
           XmInternAtom( dc->display , "WM_DELETE_WINDOW" , False ) ,
           AFNI_quit_CB , im3d ) ;

   if( MCW_isitmwm( im3d->vwid->top_shell ) )
      XtVaSetValues( im3d->vwid->top_shell ,
                        XmNmwmDecorations ,
                        MWM_DECOR_ALL | MWM_DECOR_RESIZEH | MWM_DECOR_MAXIMIZE ,
                     NULL ) ;

   /*----- null out datasets  -----*/

   for( ii=0 ; ii <= LAST_VIEW_TYPE ; ii++ ){
      im3d->anat_dset[ii] = im3d->fim_dset[ii] = NULL ;
   }
   im3d->anat_now = im3d->fim_now = NULL ;
   im3d->ss_now = NULL ;

   /* create the rest of the AFNI control data */

   last_color = im3d->dc->ovc->ncol_ov - 1 ;

   im3d->vinfo->crosshair_gap     = INIT_crosshair_gap ;
   im3d->vinfo->crosshair_gap_old = 0 ;
   im3d->vinfo->crosshair_visible = True ;                /* show crosshairs */
   im3d->vinfo->crosshair_ovcolor = MIN(last_color,INIT_crosshair_color) ;
   im3d->vinfo->time_index        = 0 ;
   im3d->vinfo->top_index         = 9 ;
   im3d->vinfo->view_type         = VIEW_ORIGINAL_TYPE ;  /* show +orig data */
   im3d->vinfo->underlay_type     = UNDERLAY_ANAT ;       /* show anatomy */
   im3d->vinfo->showfunc_type     = SHOWFUNC_FIM ;
   im3d->vinfo->force_anat_wod    = False ;   /* don't force warp-on-demand */
   im3d->vinfo->force_func_wod    = False ;   /* don't force warp-on-demand */
   im3d->vinfo->func_visible      = False ;   /* don't show function */
   im3d->vinfo->pts_visible       = False ;   /* don't show points */
   im3d->vinfo->show_voxind       = False ;
   im3d->vinfo->pts_color         = 0 ;
   im3d->vinfo->resam_vox         = INIT_resam_vox ;
   im3d->vinfo->anat_resam_mode   = INIT_resam_anat ;
   im3d->vinfo->func_resam_mode   = INIT_resam_func ;
   im3d->vinfo->thr_resam_mode    = INIT_resam_thr ;
   im3d->vinfo->func_threshold    = 0.5 ;
   im3d->vinfo->inverted_pause    = False ;

   im3d->vinfo->use_autorange     = True ;   /* use automatic FIM range */
   im3d->vinfo->fim_range         = DEFAULT_FIM_SCALE ;
   im3d->vinfo->fim_autorange     = DEFAULT_FIM_SCALE ;
   im3d->vinfo->use_posfunc       = GLOBAL_argopt.pos_func ;

   im3d->vinfo->anat_index        = 0 ;  /* 30 Nov 1997 */
   im3d->vinfo->fim_index         = 0 ;
   im3d->vinfo->thr_index         = 0 ;

   im3d->vinfo->tempflag          = 0 ;  /* 15 Mar 2000 */
   im3d->vinfo->see_ttatlas       = 0 ;  /* 25 Jul 2001 */

   /* Feb 1998: receive stuff, including drawing */
   /* Mar 1999: modified to allow for multiple receivers */

   im3d->vinfo->receiver          = malloc(sizeof(AFNI_receiver *)) ;
   im3d->vinfo->receiver[0]       = NULL ;
   im3d->vinfo->num_receiver      = 0 ;
   im3d->vinfo->drawing_enabled   = 0 ;
   im3d->vinfo->drawing_mode      = DRAWING_LINES ;
   im3d->vinfo->drawing_pixel     = 0 ;

   /** July 1996: set up the montage crosshair stuff **/

   LOAD_IVEC3(im3d->vinfo->xhairs_ndown,0,0,0) ;
   LOAD_IVEC3(im3d->vinfo->xhairs_nup  ,0,0,0) ;
   LOAD_IVEC3(im3d->vinfo->xhairs_nskip,0,0,0) ;
   im3d->vinfo->xhairs_show_montage = True ;
   im3d->vinfo->xhairs_periodic     = INIT_montage_periodic ;
   im3d->vinfo->xhairs_all          = False ;

   /** June 1996: stuff for interactive FIM **/

   im3d->s123 = im3d->s231 = im3d->s312 = NULL ; /* no viewing windows yet */
   im3d->g123 = im3d->g231 = im3d->g312 = NULL ; /* no graphing windows yet */

   im3d->b123_anat = im3d->b231_anat = im3d->b312_anat =           /* created later */
    im3d->b123_fim  = im3d->b231_fim  = im3d->b312_fim  =
     im3d->b123_ulay = im3d->b231_ulay = im3d->b312_ulay = NULL ;

   im3d->vinfo->showfunc_type = SHOWFUNC_FIM ;

   im3d->ignore_seq_callbacks = AFNI_IGNORE_NOTHING ;   /* don't ignore these now */

   /* set up which datasets to deal with (defaults) */

   im3d->vinfo->sess_num = 0 ;  /* 1st session */
   im3d->vinfo->anat_num = 0 ;  /* 1st anatomy */
   im3d->vinfo->func_num = 0 ;  /* 1st function */

   AFNI_make_widgets( im3d ) ;  /* rest of the widgets */

   /* 02 Nov 1996: create daxes and voxwarp spaces for viewing */

   im3d->wod_daxes          = myXtNew(THD_dataxes) ;
   im3d->wod_daxes->type    = DATAXES_TYPE ;

   im3d->anat_voxwarp       = myXtNew(THD_warp) ;
   im3d->anat_voxwarp->type = ILLEGAL_TYPE ;

   im3d->fim_voxwarp        = myXtNew(THD_warp) ;
   im3d->fim_voxwarp->type  = ILLEGAL_TYPE ;

   RETURN(im3d) ;
}

/*---------------------------------------------------------------*/

static float DSET_bigness( THD_3dim_dataset *dset ) /* 07 Dec 2001 */
{
   float bb ;

   if( !DSET_ONDISK(dset) ) return -1 ;

   bb =  DSET_NVOX(dset)
       * DSET_NVALS(dset)
       * mri_datum_size(DSET_BRICK_TYPE(dset,0)) ;

   if( DSET_COMPRESSED(dset) || DSET_IS_MINC(dset) ) bb *= 1.5 ;

   return bb ;
}

/*---------------------------------------------------------------
   Set up the controller with some data to view!
-----------------------------------------------------------------*/

void AFNI_initialize_controller( Three_D_View * im3d )
{
   int ii ;
   char ttl[16] ;

ENTRY("AFNI_initialize_controller") ;

   /*--- check for various criminal behavior;
         the sentence for anything illegal is death ---*/

   if( ! IM3D_VALID(im3d) ){
      fprintf(stderr,
              "\n*** AFNI_initialize_controller: invalid input ***\n") ;
      EXIT(1) ;
   }

   if( GLOBAL_library.sslist == NULL ){  /* any data to use? */
      fprintf(stderr,
              "\n*** AFNI_initialize_controller: no sessions to view ***\n") ;
      EXIT(1) ;
   }

   im3d->ss_now = GLOBAL_library.sslist->ssar[ im3d->vinfo->sess_num ] ;

   if( im3d->ss_now == NULL ){     /* bad choice of initial session! */

      im3d->vinfo->sess_num = 0 ;  /* reset 1st session */
      im3d->vinfo->anat_num = 0 ;  /* reset 1st anatomy */
      im3d->vinfo->func_num = 0 ;  /* reset 1st function */

      im3d->ss_now = GLOBAL_library.sslist->ssar[ im3d->vinfo->sess_num ] ;

      if( im3d->ss_now == NULL ){  /* still no data? */
         fprintf(stderr,
                 "\n*** AFNI_initialize_controller: illegal initial session ***\n") ;
         EXIT(1) ;
      }
   }

   /* 07 Dec 2001: find "smallest" datasets */

   if( im3d->brand_new && AFNI_yesenv("AFNI_START_SMALL") ){
      int jj,jb=0 ; float bb,mm ;
      for( mm=1.e+33,jb=jj=0 ; jj < im3d->ss_now->num_anat ; jj++ ){
         bb = DSET_bigness(im3d->ss_now->anat[jj][0]) ;
         if( bb > 0 && bb < mm ){ mm = bb; jb = jj; }
      }
      im3d->vinfo->anat_num = jb ;

      for( mm=1.e+33,jb=jj=0 ; jj < im3d->ss_now->num_func ; jj++ ){
         bb = DSET_bigness(im3d->ss_now->func[jj][0]) ;
         if( bb > 0 && bb < mm ){ mm = bb; jb = jj; }
      }
      im3d->vinfo->func_num = jb ;
   }

   /* copy pointers from this session into the controller for fast access */

   for( ii=0 ; ii <= LAST_VIEW_TYPE ; ii++ )
      im3d->anat_dset[ii] = im3d->ss_now->anat[im3d->vinfo->anat_num][ii] ;

   /*--- 11/23/94 addition: scan for a good view in the
        initial setup (to allow for input of datasets w/o +orig view) */

   for( ii=0 ; ii <= LAST_VIEW_TYPE ; ii++ )
      if( ISVALID_3DIM_DATASET(im3d->anat_dset[ii]) ) break ;

   if( ii > LAST_VIEW_TYPE ){
      fprintf(stderr,"\n*** AFNI_initialize_controller: cannot initialize view ***\n") ;
      EXIT(1) ;
   }
   im3d->vinfo->view_type = ii ;  /* first view with a good anat */

   /* now find a function that can be viewed here */

   if( !ISVALID_3DIM_DATASET(im3d->ss_now->func[im3d->vinfo->func_num][ii]) ){
      for( ii=0 ; ii < im3d->ss_now->num_func ; ii++ )
         if(ISVALID_3DIM_DATASET(im3d->ss_now->func[ii][im3d->vinfo->view_type])){
            im3d->vinfo->func_num = ii ; break ;
         }
   }

   for( ii=0 ; ii <= LAST_VIEW_TYPE ; ii++ )
      im3d->fim_dset[ii] = im3d->ss_now->func[im3d->vinfo->func_num][ii] ;

   /*--- 11/23/94 addition end */

   im3d->anat_now = im3d->anat_dset[im3d->vinfo->view_type] ;  /* will not be NULL */
   im3d->fim_now  = im3d->fim_dset [im3d->vinfo->view_type] ;  /* this may be NULL */

   /* initial point of view = middle of dataset brick */

   im3d->vinfo->i1 = im3d->anat_now->daxes->nxx / 2 ;  /* integer indexes */
   im3d->vinfo->j2 = im3d->anat_now->daxes->nyy / 2 ;
   im3d->vinfo->k3 = im3d->anat_now->daxes->nzz / 2 ;
   SAVE_VPT(im3d) ;

   if( im3d->type == AFNI_3DDATA_VIEW ){  /* compute float coordinates, too */
      THD_fvec3 fv ;

      im3d->anat_now->wod_flag = 0 ;  /* 02 Nov 1996 */

      fv = THD_3dind_to_3dmm( im3d->anat_now ,
                              TEMP_IVEC3( im3d->vinfo->i1 ,
                                          im3d->vinfo->j2 ,
                                          im3d->vinfo->k3  ) ) ;

      fv = THD_3dmm_to_dicomm( im3d->anat_now , fv ) ;

      im3d->vinfo->xi = fv.xyz[0] ;  /* set display coords */
      im3d->vinfo->yj = fv.xyz[1] ;  /* to Dicom standard  */
      im3d->vinfo->zk = fv.xyz[2] ;

   } else {
      im3d->vinfo->xi = im3d->vinfo->i1 ;  /* probably never used */
      im3d->vinfo->yj = im3d->vinfo->j2 ;
      im3d->vinfo->zk = im3d->vinfo->k3 ;
   }

   /** set various widgets and values **/

   MCW_set_bbox( im3d->vwid->view->view_bbox     , 1 << im3d->vinfo->view_type ) ;
   MCW_set_bbox( im3d->vwid->view->see_func_bbox , (im3d->vinfo->func_visible) ? 1 : 0 ) ;

   LOAD_IVEC3(im3d->vinfo->xhairs_ndown,0,0,0) ;
   LOAD_IVEC3(im3d->vinfo->xhairs_nup  ,0,0,0) ;
   LOAD_IVEC3(im3d->vinfo->xhairs_nskip,0,0,0) ;
   im3d->vinfo->xhairs_periodic = INIT_montage_periodic ;
   MCW_set_bbox( im3d->vwid->imag->wrap_bbox  , im3d->vinfo->xhairs_periodic ? 1 : 0 ) ;
   MCW_set_bbox( im3d->vwid->imag->xhall_bbox , im3d->vinfo->xhairs_all      ? 1 : 0 ) ;

   CLEAR_FIMDATA(im3d) ;

   sprintf(ttl , "%sAFNI" , AFNI_controller_label(im3d) ) ;
   XtVaSetValues( im3d->vwid->top_shell , XmNiconName , ttl , NULL ) ;

   /* 06 Dec 2001: cool cursor stuff */

   WAIT_for_window(im3d->vwid->top_shell) ;
   POPUP_cursorize( im3d->vwid->func->inten_label ) ;
   POPUP_cursorize( im3d->vwid->picture ) ;

   RESET_AFNI_QUIT(im3d) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------
  23 Sep 2000: A hack to allow the creation of a specific controller,
               instead of letting AFNI_clone_controller_CB choose the index.
-----------------------------------------------------------------------------*/

static int cii_override = -1 ;

void AFNI_make_controller( int cii )
{
   int ii ;

ENTRY("AFNI_make_controller") ;

   if( cii < 0 || cii >= MAX_CONTROLLERS ||
       IM3D_VALID(GLOBAL_library.controllers[cii]) ) EXRETURN ;

   cii_override = cii ;
   AFNI_clone_controller_CB( GLOBAL_library.controllers[0]->vwid->prog->clone_pb ,
                             GLOBAL_library.controllers[0] , NULL ) ;

   cii_override = -1 ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

void AFNI_clone_controller_CB( Widget wcall , XtPointer cd , XtPointer cbs )
{
   int ii , xx , yy ;
   Three_D_View * im3d ;
   Three_D_View * caller_im3d = (Three_D_View *) cd ;
   MCW_DC * new_dc , * old_dc ;

ENTRY("AFNI_clone_controller_CB") ;

   ii = AFNI_count_controllers() ;
   if( ii >= MAX_CONTROLLERS ) EXRETURN ;

   SHOW_AFNI_PAUSE ;

   /** 23 Sep 2000: if cii_override is set, use that for
                    the index of the controller to create **/

   if( cii_override < 0 ){
      /** look for a previously opened but now closed controller **/

      for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
         im3d = GLOBAL_library.controllers[ii] ;
         if( IM3D_VALID(im3d) && ! im3d->opened ) break ;  /* found it */
      }
   } else {
      ii = MAX_CONTROLLERS ;  /* skip this step */
   }

   if( ii >= MAX_CONTROLLERS ){          /* look for an empty one to create */

      if( cii_override < 0 ){
         for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ ){
            im3d = GLOBAL_library.controllers[ii] ;
            if( im3d == NULL ) break ;
         }
      } else {
         ii = cii_override ;
      }
      if( ii >= MAX_CONTROLLERS ){       /* something funny has happened! */
         SHOW_AFNI_READY ; EXRETURN ;
      }

      /* 06 Nov 1996: allow creation of unique dc's for each controller */

      new_dc = NULL ;
      old_dc = GLOBAL_library.dc ;
      if( GLOBAL_argopt.unique_dcs )
         new_dc = MCW_new_DC( wcall , GLOBAL_argopt.ncolor ,
                              INIT_ncolovr , INIT_colovr , INIT_labovr ,
                              GLOBAL_argopt.gamma , 0 ) ;

      if( new_dc == NULL ) new_dc = old_dc ;

      GLOBAL_library.controllers[ii] =
           new_AFNI_controller( NULL , new_dc , AFNI_3DDATA_VIEW ) ;

      if( caller_im3d != NULL ){
         MCW_widget_geom( caller_im3d->vwid->top_shell , NULL,NULL , &xx,&yy ) ;
         xx += 15 ; yy += 15 ;
         XtVaSetValues( GLOBAL_library.controllers[ii]->vwid->top_shell ,
                           XmNx , xx , XmNy , yy , NULL ) ;
      }
   }

   /** at this point, ii = index of a controller to use **/

   im3d = GLOBAL_library.controllers[ii] ;

   OPEN_CONTROLLER( im3d ) ;
   AFNI_initialize_controller( im3d ) ;  /* decide what to see */
   AFNI_initialize_view( NULL , im3d ) ; /* set up to see it */

   AFNI_controller_clonify() ;

   SHOW_AFNI_READY ; EXRETURN ;
}

/*-----------------------------------------------------------------------
   Called to determine if the "New" button should be active
-------------------------------------------------------------------------*/

void AFNI_controller_clonify(void)
{
   Three_D_View * im3d ;
   int id ;
   Boolean clone_on ;

ENTRY("AFNI_controller_clonify") ;

   if( MAX_CONTROLLERS == 1 ) EXRETURN ;

   clone_on = (Boolean)( AFNI_count_controllers() < MAX_CONTROLLERS ) ;

   for( id=0 ; id < MAX_CONTROLLERS ; id++ ){
      im3d = GLOBAL_library.controllers[id] ;
      if( IM3D_OPEN(im3d) && im3d->vwid->prog->clone_pb != NULL ){
         SENSITIZE( im3d->vwid->prog->clone_pb , clone_on ) ;
         RESET_AFNI_QUIT(im3d) ;
      }
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------------
   04 Nov 1996: make a menubar to control the coordinate locking
                of the various controller windows
---------------------------------------------------------------------------*/

void AFNI_lock_button( Three_D_View * im3d )
{
   Widget rc , mbar , menu , cbut , wpar ;
   XmString xstr ;

   static char * clabel[] = {
      "Lock [A]", "Lock [B]", "Lock [C]", "Lock [D]", "Lock [E]",
      "Lock [F]", "Lock [G]", "Lock [H]", "Lock [I]", "Lock [J]",
      "Lock [K]", "Lock [L]", "Lock [M]", "Lock [N]", "Lock [O]",
      "Lock [P]", "Lock [Q]", "Lock [R]", "Lock [S]", "Lock [T]",
      "Lock [U]", "Lock [V]", "Lock [W]", "Lock [X]", "Lock [Y]", "Lock [Z]" } ;

   static char * tlabel[] = { "Time Lock" } ;

   static char * ijklabel[] = { "IJK lock" } ;

ENTRY("AFNI_lock_button") ;

   wpar = im3d->vwid->dmode->mbar_rowcol ;

   rc =  XtVaCreateWidget(
           "dialog" , xmRowColumnWidgetClass , wpar ,
              XmNorientation , XmHORIZONTAL ,
              XmNpacking , XmPACK_TIGHT ,
              XmNtraversalOn , False ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   mbar = XmCreateMenuBar( rc , "dialog" , NULL,0 ) ;
   XtVaSetValues( mbar ,
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNspacing      , 3 ,
                     XmNborderWidth  , 0 ,
                     XmNborderColor  , 0 ,
                     XmNtraversalOn  , False ,
                     XmNbackground   , im3d->dc->ovc->pixov_brightest ,
                  NULL ) ;
   XtManageChild( mbar ) ;

   menu = XmCreatePulldownMenu( mbar , "menu" , NULL,0 ) ;

   VISIBILIZE_WHEN_MAPPED(menu) ;

   xstr = XmStringCreateLtoR( "Lock" , XmFONTLIST_DEFAULT_TAG ) ;
   cbut = XtVaCreateManagedWidget(
            "dialog" , xmCascadeButtonWidgetClass , mbar ,
               XmNlabelString , xstr ,
               XmNsubMenuId , menu ,
               XmNmarginWidth  , 0 ,
               XmNmarginHeight , 0 ,
               XmNmarginBottom , 0 ,
               XmNmarginTop    , 0 ,
               XmNmarginRight  , 0 ,
               XmNmarginLeft   , 0 ,
               XmNtraversalOn  , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XmStringFree( xstr ) ;

   MCW_register_help( cbut , "Pressing this drops down\n"
                             "the menu of controllers\n"
                             "which should be 'locked'\n"
                             "together in their viewpoint\n"
                             "coordinates.\n"
                             "N.B.: The lock will only take\n"
                             "  effect when you next move\n"
                             "  the crosshairs, or click\n"
                             "  on the 'Enforce' button."
                    ) ;
   MCW_register_hint( cbut , "Lock AFNI controller viewpoints" ) ;

   /*** top of menu = a label to click on that does nothing at all ***/

   xstr = XmStringCreateLtoR( "-- Cancel --" , XmFONTLIST_DEFAULT_TAG ) ;
   (void) XtVaCreateManagedWidget(
            "dialog" , xmLabelWidgetClass , menu ,
               XmNlabelString , xstr ,
               XmNrecomputeSize , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XmStringFree(xstr) ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , menu ,
               XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

   /*** button box to select locks ***/

   dmode->lock_bbox = new_MCW_bbox( menu ,
                                    MAX_CONTROLLERS , clabel ,
                                    MCW_BB_check , MCW_BB_noframe ,
                                    AFNI_lock_change_CB , (XtPointer)im3d ) ;

   MCW_set_bbox( dmode->lock_bbox , GLOBAL_library.controller_lock ) ;

   MCW_reghint_children( dmode->lock_bbox->wrowcol ,
                         "Which ones are locked together?" ) ;

   /*** button box to control the time lock ***/

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , menu ,
               XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

   dmode->time_lock_bbox = new_MCW_bbox( menu ,
                                         1 , tlabel ,
                                         MCW_BB_check , MCW_BB_noframe ,
                                         AFNI_time_lock_change_CB , (XtPointer)im3d ) ;

   MCW_set_bbox( dmode->time_lock_bbox , GLOBAL_library.time_lock ) ;

   MCW_reghint_children( dmode->time_lock_bbox->wrowcol ,
                         "Lock time index as well?" ) ;

   dmode->ijk_lock_bbox = new_MCW_bbox( menu ,
                                        1 , ijklabel ,
                                        MCW_BB_check , MCW_BB_noframe ,
                                        AFNI_ijk_lock_change_CB , (XtPointer)im3d ) ;

   MCW_set_bbox( dmode->ijk_lock_bbox , GLOBAL_library.ijk_lock ) ;

   MCW_reghint_children( dmode->ijk_lock_bbox->wrowcol ,
                         "Lock using voxel indices?" ) ;

   /*** pushbuttons ***/

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , menu ,
               XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

   /*** to clear all locks right now ***/

   xstr = XmStringCreateLtoR( "Clear All" , XmFONTLIST_DEFAULT_TAG ) ;
   dmode->lock_clear_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               XmNlabelString , xstr ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->lock_clear_pb , XmNactivateCallback ,
                  AFNI_lock_clear_CB , (XtPointer)im3d ) ;
   XmStringFree(xstr) ;
   MCW_register_hint( dmode->lock_clear_pb , "Clear all locked controllers" ) ;

   /*** to set all locks right now [19 Apr 1999] ***/

   xstr = XmStringCreateLtoR( "Set All" , XmFONTLIST_DEFAULT_TAG ) ;
   dmode->lock_setall_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               XmNlabelString , xstr ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->lock_setall_pb , XmNactivateCallback ,
                  AFNI_lock_setall_CB , (XtPointer)im3d ) ;
   XmStringFree(xstr) ;
   MCW_register_hint( dmode->lock_setall_pb , "Set all locked controllers" ) ;

   /*** to enforce locks right now ***/

   xstr = XmStringCreateLtoR( "Enforce" , XmFONTLIST_DEFAULT_TAG ) ;
   dmode->lock_enforce_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               XmNlabelString , xstr ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->lock_enforce_pb , XmNactivateCallback ,
                  AFNI_lock_enforce_CB , (XtPointer)im3d ) ;
   XmStringFree(xstr) ;
   MCW_register_hint( dmode->lock_enforce_pb , "Make lock work NOW" ) ;

   XtManageChild( rc ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------
   30 Oct 1997: make a menubar for miscellaneous options
-----------------------------------------------------------------------*/

void AFNI_misc_button( Three_D_View * im3d )
{
   Widget rc , mbar , menu , cbut , wpar ;
   XmString xstr ;

ENTRY("AFNI_misc_button") ;

   wpar = im3d->vwid->dmode->mbar_rowcol ;

   rc =  XtVaCreateWidget(
           "dialog" , xmRowColumnWidgetClass , wpar ,
              XmNorientation , XmHORIZONTAL ,
              XmNpacking , XmPACK_TIGHT ,
              XmNtraversalOn , False ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   mbar = XmCreateMenuBar( rc , "dialog" , NULL,0 ) ;
   XtVaSetValues( mbar ,
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNspacing      , 3 ,
                     XmNborderWidth  , 0 ,
                     XmNborderColor  , 0 ,
                     XmNtraversalOn  , False ,
                     XmNbackground   , im3d->dc->ovc->pixov_brightest ,
                  NULL ) ;
   XtManageChild( mbar ) ;

   menu = XmCreatePulldownMenu( mbar , "menu" , NULL,0 ) ;

   VISIBILIZE_WHEN_MAPPED(menu) ;

   xstr = XmStringCreateLtoR( "Misc" , XmFONTLIST_DEFAULT_TAG ) ;
   cbut = XtVaCreateManagedWidget(
            "dialog" , xmCascadeButtonWidgetClass , mbar ,
               XmNlabelString , xstr ,
               XmNsubMenuId , menu ,
               XmNmarginWidth  , 0 ,
               XmNmarginHeight , 0 ,
               XmNmarginBottom , 0 ,
               XmNmarginTop    , 0 ,
               XmNmarginRight  , 0 ,
               XmNmarginLeft   , 0 ,
               XmNtraversalOn  , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XmStringFree( xstr ) ;

   MCW_register_help( cbut , "Pressing this drops down the menu\n"
                             "of miscellaneous options:\n"
#ifdef USE_WRITEOWNSIZE
                             " Write=Own size?   = Write dataset using current\n"
                             "                      anat dimensions, or using\n"
                             "                      size of dataset in its own\n"
                             "                      .HEAD file.\n"
#endif
                             " Voxel Coords?     = Show crosshair location\n"
                             "                      in mm or voxel indexes\n"
#ifndef DONT_USE_HINTS
                             " Show Hints?       = Turn popup hints\n"
                             "                      off or on\n"
#endif
                             " Anat Info         = Show 3dinfo output\n"
                             " Func Info         = for current datasets\n"
#ifdef ALLOW_PLUGINS
                             " Edit Environment  = Control environment vars\n"
                             " Edit 2DChain      = Control 2DChain function\n"
#endif
                             " Save Layout       = Save windows layout\n"
                             " License Info      = GPL & Copyright notice\n"
                             " Version Check     = Check AFNI version\n"
                             " Purge Memory      = Of dataset BRIKs\n"
#ifdef USE_TRACING
                             " Trace=MODE        = Set trace mode to\n"
                             "                      next legal setting\n"
#endif
#ifdef USING_MCW_MALLOC
                             " Malloc Summary    = Show memory usage\n"
                             " Dump Malloc Table = Memory usage to a file"
#endif
                           ) ;
   MCW_register_hint( cbut , "Miscellaneous options" ) ;

   /*** top of menu = a label to click on that does nothing at all ***/

   xstr = XmStringCreateLtoR( "-- Cancel --" , XmFONTLIST_DEFAULT_TAG ) ;
   (void) XtVaCreateManagedWidget(
            "dialog" , xmLabelWidgetClass , menu ,
               XmNlabelString , xstr ,
               XmNrecomputeSize , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XmStringFree(xstr) ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , menu ,
               XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

#ifdef USE_WRITEOWNSIZE
   /*-- 01 Aug 1999: Toggle for Write dimensions --*/

   { char * blab[1] = { "Write=Own Size?" } ;
     dmode->misc_writeownsize_bbox = new_MCW_bbox( menu ,
                                                   1 , blab ,
                                                   MCW_BB_check , MCW_BB_noframe ,
                                                   AFNI_misc_CB , (XtPointer)im3d ) ;
     dmode->misc_writeownsize_pb = dmode->misc_writeownsize_bbox->wbut[0] ;
   }
   MCW_register_hint( dmode->misc_writeownsize_pb , "Controls dimensions using Write" ) ;
#else
   dmode->misc_writeownsize_pb = NULL ;
#endif

   /*-- pushbutton for voxel index toggle --*/

   /* 01 Aug 1999: replace pushbutton with toggle button */

   { char * blab[1] = { "Voxel Coords?" } ;
     dmode->misc_voxind_bbox = new_MCW_bbox( menu ,
                                             1 , blab ,
                                             MCW_BB_check , MCW_BB_noframe ,
                                             AFNI_misc_CB , (XtPointer)im3d ) ;
     dmode->misc_voxind_pb = dmode->misc_voxind_bbox->wbut[0] ;
   }
   MCW_register_hint( dmode->misc_voxind_pb , "Toggle coordinate display" ) ;

    /*-- pushbutton to turn hints on and off --*/

#ifndef DONT_USE_HINTS
   { char * hh = getenv("AFNI_HINTS") ;
     if( hh != NULL && ( strncmp(hh,"KILL",4)==0 ||
                         strncmp(hh,"kill",4)==0 ||
                         strncmp(hh,"Kill",4)==0 ) ){

            dmode->misc_hints_pb =
               XtVaCreateManagedWidget(
                  "dialog" , xmLabelWidgetClass , menu ,
                     LABEL_ARG("*Hints Killed*") ,
                     XmNmarginHeight , 0 ,
                     XmNrecomputeSize , False ,
                     XmNinitialResourcesPersistent , False ,
                  NULL ) ;

         } else {
            { char * blab[1] = { "Show Hints?" } ;
              dmode->misc_hints_bbox = new_MCW_bbox( menu ,
                                                     1 , blab ,
                                                     MCW_BB_check , MCW_BB_noframe ,
                                                     AFNI_misc_CB , (XtPointer)im3d ) ;
              dmode->misc_hints_pb = dmode->misc_hints_bbox->wbut[0] ;
            }
            MCW_register_hint( dmode->misc_hints_pb , "Toggle hints display" ) ;
            MCW_set_bbox( dmode->misc_hints_bbox , GLOBAL_library.hints_on ) ;
         }
   }
#else
   dmode->misc_hints_pb = NULL ;
#endif

   /*-- pushbuttons to popup info about datasets --*/

   dmode->misc_anat_info_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("Anat Info") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_anat_info_pb , XmNactivateCallback ,
                  AFNI_misc_CB , im3d ) ;
   MCW_register_hint( dmode->misc_anat_info_pb , "Popup anat dataset info" ) ;

   dmode->misc_func_info_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("Func Info") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_func_info_pb , XmNactivateCallback ,
                  AFNI_misc_CB , im3d ) ;
   MCW_register_hint( dmode->misc_func_info_pb , "Popup func dataset info" ) ;

   /*--- 20 Jun 2000: pushbutton to popup the Environment pseudo-plugin ---*/

#ifdef ALLOW_PLUGINS
   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , menu ,
               XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

   dmode->misc_environ_pb =                              /* 20 Jun 2000 */
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("Edit Environment") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_environ_pb , XmNactivateCallback ,
                  AFNI_misc_CB , im3d ) ;
   MCW_register_hint( dmode->misc_environ_pb , "Control environment variables" ) ;

   dmode->misc_1dchain_pb =                              /* 03 Jul 2000 */
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("Edit 1DChain") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_1dchain_pb , XmNactivateCallback ,
                  AFNI_misc_CB , im3d ) ;
   MCW_register_hint( dmode->misc_1dchain_pb , "Control 1DChain function" ) ;
   AFNI_misc_CB( dmode->misc_1dchain_pb , im3d , NULL ) ;

   dmode->misc_2dchain_pb =                              /* 03 Jul 2000 */
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("Edit 2DChain") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_2dchain_pb , XmNactivateCallback ,
                  AFNI_misc_CB , im3d ) ;
   MCW_register_hint( dmode->misc_2dchain_pb , "Control 2DChain function" ) ;
   AFNI_misc_CB( dmode->misc_2dchain_pb , im3d , NULL ) ;
#endif

   /*--- 23 Sep 2000: Save Layout [see afni_splash.c] ---*/

   dmode->misc_savelayout_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("Save Layout") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_savelayout_pb , XmNactivateCallback ,
                  AFNI_save_layout_CB , im3d ) ;
   MCW_register_hint( dmode->misc_savelayout_pb , "Save windows layout to file" ) ;

   /*--- 07 Nov 2001: start plugouts [see afni_plugout.c] ---*/

#ifdef ALLOW_PLUGINS
   if( !ALLOW_real_time && !AFNI_have_plugouts() ){
      dmode->misc_plugout_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , menu ,
                  LABEL_ARG("Start Plugouts") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , False ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( dmode->misc_plugout_pb , XmNactivateCallback ,
                     AFNI_misc_CB , im3d ) ;
      MCW_register_hint( dmode->misc_plugout_pb ,
                         "Start listening for plugouts" ) ;
   } else {
      dmode->misc_plugout_pb = NULL ;
   }
#else
   dmode->misc_plugout_pb = NULL ;
#endif

   /*--- Utility buttons ---*/

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , menu ,
               XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

   dmode->misc_license_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("License Info") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_license_pb , XmNactivateCallback ,
                  AFNI_misc_CB , im3d ) ;
   MCW_register_hint( dmode->misc_license_pb,"Display GPL & Copyright Notice" );

   if( !ALLOW_real_time ){    /* 01 May 2000: only if not doing realtime */
      dmode->misc_vcheck_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , menu ,
                  LABEL_ARG("Version Check") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , False ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( dmode->misc_vcheck_pb , XmNactivateCallback ,
                     AFNI_misc_CB , im3d ) ;
      MCW_register_hint( dmode->misc_vcheck_pb,"Compare to master distribution" );
   } else {
      dmode->misc_vcheck_pb = NULL ;
   }

   /*--- pushbutton to purge unused datasets ---*/

   dmode->misc_purge_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("Purge Memory") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_purge_pb , XmNactivateCallback ,
                  AFNI_misc_CB , im3d ) ;
   MCW_register_hint( dmode->misc_purge_pb , "Purge unused datasets" ) ;

   /*--- pushbutton to toggle routine tracing ---*/

#if defined(USE_TRACING) || defined(USING_MCW_MALLOC)
   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , menu ,
               XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;
#endif

#ifdef USE_TRACING
   if( !ALLOW_real_time ){    /* 26 Jan 2001: don't do this if realtime is on */
     dmode->misc_tracing_pb =
           XtVaCreateManagedWidget(
              "dialog" , xmPushButtonWidgetClass , menu ,
                 LABEL_ARG( DBG_label ) ,
                 XmNmarginHeight , 0 ,
                 XmNtraversalOn , False ,
                 XmNinitialResourcesPersistent , False ,
              NULL ) ;
     XtAddCallback( dmode->misc_tracing_pb , XmNactivateCallback ,
                    AFNI_misc_CB , im3d ) ;
     MCW_register_hint( dmode->misc_tracing_pb , "A Debugging Option" ) ;
   } else {
     dmode->misc_tracing_pb = NULL ;
   }
#else
   dmode->misc_tracing_pb = NULL ;
#endif

   /*--- pushbutton to query malloc table ---*/
#ifdef USING_MCW_MALLOC
   if( MCW_MALLOC_enabled ){  /* 06 Mar 1999 */
      dmode->misc_showmalloc_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , menu ,
                  LABEL_ARG("Malloc Summary") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , False ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( dmode->misc_showmalloc_pb , XmNactivateCallback ,
                     AFNI_misc_CB , im3d ) ;

      dmode->misc_dumpmalloc_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , menu ,
                  LABEL_ARG("Dump Malloc Table") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , False ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( dmode->misc_dumpmalloc_pb , XmNactivateCallback ,
                     AFNI_misc_CB , im3d ) ;

      MCW_register_hint( dmode->misc_showmalloc_pb , "A Debugging Option" ) ;
      MCW_register_hint( dmode->misc_dumpmalloc_pb , "A Debugging Option" ) ;
   } else {
      dmode->misc_showmalloc_pb = dmode->misc_dumpmalloc_pb = NULL ;
   }
#else
   dmode->misc_showmalloc_pb = dmode->misc_dumpmalloc_pb = NULL ;
#endif

   /*--- done ---*/

   XtManageChild( rc ) ;
   EXRETURN ;
}
