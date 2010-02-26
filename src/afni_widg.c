/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "afni.h"
#include "afni_plugout.h"

/*---------------------------------------------------------------*/
/*------------ Stuff for logos and pixmap definitions -----------*/
#undef MAIN
#define WANT_LOGO_BITMAP
#define WANT_AFNI_BITMAP
#undef  USE_IMPIX

#include "logo.h"              /* declares global pixmap variables */

#ifdef WANT_AFNI_BITMAP        /* used for various icons          */
#  include "afni48.xbm"        /* iconified controller window     */
#  include "afni48cor.xbm"     /* iconified coronal image window  */
#  include "afni48axi.xbm"     /* iconified axial image window    */
#  include "afni48sag.xbm"     /* iconified sagittal image window */
#  include "afni48gra.xbm"     /* no longer used                  */
#  include "afni48gracor.xbm"  /* iconified coronal graph window  */
#  include "afni48grasag.xbm"  /* iconified sagittal graph window */
#  include "afni48graaxi.xbm"  /* iconified axial graph window    */
#  include "afni16.xbm"        /* used for 'AFNI' form background */
#endif /* WANT_AFNI_BITMAP */

#ifdef WANT_LOGO_BITMAP        /* now only used for PseudoColor */
#ifdef USE_MCWLOGO             /* for TrueColor, the color logo */
#  include "mcw.xbm"           /* in "lll.h" is used instead.   */
#elif defined(USE_RWCLOGO)
#  include "rwc.xbm"
#else
#  include "nih.xbm"
#endif
#endif /* WANT_LOGO_BITMAP */
/*---------------------------------------------------------------*/

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

static char *AFNI_dummy_av_label[2] = { "Nothing 1" , "Nothing 2" } ;

static char *AFNI_crosshair_av_label[9] = {  /* modified 31 Dec 1998 */
    "Off"   , "Single" , "Multi" ,
    " LR+AP", " LR+IS" , " AP+IS",
    "  LR"  , "  AP"   , "  IS"    } ;

static char *AFNI_see_marks_bbox_label[1] = { "See Markers" } ;

static char *AFNI_see_func_bbox_label[1] = { "See OverLay" } ;

static char *AFNI_wrap_bbox_label[1] = {"Wrap"} ;
static char *AFNI_xhall_bbox_label[1] = {"X+"} ;

static char *AFNI_marks_edits_bbox_label[1] = { "Allow edits" } ;

static char *AFNI_range_bbox_label[1] = { "autoRange:xxxxxxxxx" } ;

static char *AFNI_inten_bbox_label[1] = { "Pos?" } ;

static char *AFNI_tlrc_big_bbox_label[1] = { "Big Talairach Box?" } ;

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

static char *AFNI_anatmode_bbox_label[2] =
   { "View ULay Data Brick" , "Warp ULay on Demand" } ;

static char *AFNI_funcmode_bbox_label[2] =
   { "View OLay Data Brick" , "Warp OLay on Demand" } ;

#define AFNI_see_marks_bbox_help                       \
   "pressed IN:  markers for this view will display\n" \
   "pressed OUT: markers for this view won't display\n"\
   "\n"                                                \
   "Markers are used to specify anatomical locations\n"\
   "required for transformation of coordinates.\n"     \
   "\n"                                                \
   "Oct 1998: Also controls the display of dataset\n"  \
   "          'tags' -- see the 'Edit Tagset' plugin."

#define AFNI_see_func_bbox_help                        \
   "pressed IN:  overlay dataset will display\n"       \
   "pressed OUT: overlay dataset won't display"        \
   "\n"                                                \
   "This is useful for seeing what anatomical\n"       \
   "features are 'under' a particular overlay color."

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
   " and a grapher open at the same time,\n"     \
   " then a 'frame' will be drawn around\n"      \
   " the voxels being graphed.  In Single\n"     \
   " crosshair mode, only this frame will\n"     \
   " be drawn.  In Multi mode, the actual\n"     \
   " crosshairs will also be drawn.\n"           \
   "N.B.: You can set AFNI_CROSSHAIR_LINES\n"    \
   " in Datamode->Misc->Edit Environment\n"      \
   " to have crosshairs drawn as thin lines\n"   \
   " rather than as overlaid voxels."

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

#define AFNI_crosshair_label_help      \
   "Displays coordinates of\n"         \
   "the crosshair point in the\n"      \
   "DICOM coordinates (3D input)\n"    \
   "or voxel indices (image input)\n"  \
   "\n"                                \
   "A Button-3 popup menu lets you\n"  \
   "change coordinate display order."

#define AFNI_view_help                            \
   "Normal:   button opens viewing window\n"      \
   "Inverted: button raises opened window\n"      \
   "     * Right-click on inverted button\n"      \
   "       'fetches' image/graph window\n\n"      \
   "N.B.: AFNI does not read datasets from\n"     \
   "      disk until a window is opened.\n"       \
   "      This can make opening the first\n"      \
   "      viewing window be quite slow.\n"        \
   "\n"                                           \
   "The Graph buttons are only enabled for\n"     \
   "datasets that are viewing their data files\n" \
   "directly (NOT warping on demand -- see the\n" \
   "top of the 'Define Datamode' control panel)"

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

static   AFNI_widget_set       *vwid  ;
static   AFNI_imaging_widgets  *imag  ;
static   AFNI_viewing_widgets  *view  ;
static   AFNI_marks_widgets    *marks ;
static   AFNI_function_widgets *func  ;
static   AFNI_program_widgets  *prog  ;
static   AFNI_datamode_widgets *dmode ;

static   XmString   xstr ;
static   XmFontList xflist=(XmFontList)NULL ;
static   char       str[256] ;
static   int        id , npane , last_color ,
                    view_count , view_height , sel_height ;

static int FIRST_widcall = 1 ;  /* 22 Dec 2009 */

void AFNI_make_wid1 (Three_D_View *) ;
void AFNI_make_wid2 (Three_D_View *) ;
void AFNI_make_wid3 (Three_D_View *) ;

/*--------------------------------------------------------------------*/

void AFNI_make_widgets( Three_D_View *im3d )
{

ENTRY("AFNI_make_widgets") ;

   /*---- initialize -----*/

   if( ! IM3D_VALID(im3d) )   /* should never happen! */
     ERROR_exit("illegal call to AFNI_make_widgets") ;

   num_entry++ ;

   last_color = im3d->dc->ovc->ncol_ov - 1 ;

   vwid         = im3d->vwid ;
   vwid->parent = im3d ;

   vwid->butx = vwid->buty = 9 ; /* 17 May 2005 */

#ifdef USING_LESSTIF
   /* In Lesstif, using form spacing, shifts the
   top left corner in unsightly ways. Little
   spacing looks better    Lesstif Patrol Jan 09*/
   #define AFNI_FORM_SPACING 1
#else
   #define AFNI_FORM_SPACING 9
#endif
STATUS("creating top_form") ;

   vwid->top_form =
      XtVaCreateWidget(
         "dialog" , xmFormWidgetClass , vwid->top_shell ,
            XmNborderWidth , 0 ,
            XmNmarginHeight , AFNI_FORM_SPACING ,
            XmNmarginWidth  , AFNI_FORM_SPACING ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   MCW_register_help( vwid->top_form , AFNI_tophelp ) ;

   vwid->file_dialog = NULL ; /* Mar 1997 */

   /* create pixmaps, if desired */

#if defined(WANT_LOGO_BITMAP) || defined(WANT_AFNI_BITMAP)
   {  Pixel bg_pix=0  , fg_pix  ;  /* colors: from control window */
      Pixel bot_pix=0 , top_pix ;  /* colors: from image windows  */

#ifdef USE_IMPIX              /** which colors to use for program icons **/
#  define ICON_bg bot_pix     /* use image display pixels */
#  define ICON_fg top_pix
#else
#  define ICON_bg bg_pix      /* use widget pixels (e.g., FALLback in afni.h) */
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
#include "lll.h"  /* contains the colorized image logos */

#define RGB_TO_PIXMAP(data,pnam)                                           \
 do{ mri_fix_data_pointer( data , bim ) ;                                  \
     pnam = XCreatePixmap( im3d->dc->display ,                             \
                           RootWindowOfScreen(im3d->dc->screen) ,          \
                           lll_width , lll_height , im3d->dc->planes ) ;   \
     xim = rgb_to_XImage( im3d->dc , bim ) ;                               \
     if( xim != NULL ) XPutImage( im3d->dc->display , pnam ,               \
                                  im3d->dc->origGC ,                       \
                                  xim , 0,0, 0,0, lll_width,lll_height ) ; \
     MCW_kill_XImage( xim );                                               \
 } while(0)

        if( im3d->dc->visual_class == TrueColor ){  /* 23 Sep 2001 */
          MRI_IMAGE *bim ; XImage *xim ;
          bim = mri_new_vol_empty( lll_width,lll_height,1 , MRI_rgb ) ;

          RGB_TO_PIXMAP(lll_rgb  ,logo_pixmap   ) ;
          RGB_TO_PIXMAP(vvv_rgb  ,vers_pixmap   ) ;  /* 08 Aug 2005 */
          RGB_TO_PIXMAP(rhdda_rgb,pict_pixmap[0]) ;  /* 19 Oct 2007 */
          RGB_TO_PIXMAP(sbuck_rgb,pict_pixmap[1]) ;  /* 18 Oct 2007 */
          RGB_TO_PIXMAP(sscc_rgb ,pict_pixmap[2]) ;  /* 22 Oct 2007 */
          RGB_TO_PIXMAP(earth_rgb,pict_pixmap[3]) ;  /* 22 Oct 2007 */
          RGB_TO_PIXMAP(nih_rgb  ,pict_pixmap[4]) ;  /* 25 Oct 2007 */
          RGB_TO_PIXMAP(burst_rgb,pict_pixmap[5]) ;  /* 18 Oct 2007 */

          mri_clear_data_pointer(bim); mri_free(bim);
        }
#endif

        if( logo_pixmap == XmUNSPECIFIED_PIXMAP )         /* original code */
          logo_pixmap = XCreatePixmapFromBitmapData(      /* B&W pixmap logo */
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

      /* 28 Jan 2004: just for fun, background pixmaps for top forms */

      if( im3d->dc->visual_class == TrueColor &&
          AFNI_yesenv("AFNI_LOGO16")          &&
          afni16_pixmap[num_entry-1] == XmUNSPECIFIED_PIXMAP ){

        MRI_IMAGE *bim ; XImage *xim ; char ename[32], *ept ;
        sprintf(ename,"AFNI_LOGO16_IMAGE_%c" , 'A'+num_entry-1 ) ;
        ept = getenv(ename) ;
        if( ept == NULL ) ept = getenv( "AFNI_LOGO16_IMAGE" ) ;
        if( ept != NULL ){
          bim = mri_read_just_one( ept ) ;
          if( bim != NULL ){
            if( bim->kind == MRI_rgb ){
              xim = rgb_to_XImage( im3d->dc , bim ) ;
              if( xim != NULL ){
                afni16_pixmap[num_entry-1] = XCreatePixmap( im3d->dc->display ,
                                                 RootWindowOfScreen(im3d->dc->screen) ,
                                                 bim->nx , bim->ny , im3d->dc->planes ) ;
                XPutImage( im3d->dc->display , afni16_pixmap[num_entry-1] ,
                           im3d->dc->origGC , xim , 0,0 , 0,0 , bim->nx , bim->ny ) ;
                MCW_kill_XImage( xim ) ;
              }
            }
            mri_free(bim) ;
          }
        }
      }

      if( AFNI_yesenv("AFNI_COLORIZE_CONTROLLER") &&
          im3d->dc->visual_class     == TrueColor &&
          afni16_pixmap[num_entry-1] == XmUNSPECIFIED_PIXMAP ){ /* 17 Oct 2007 */

        MRI_IMAGE *bim ; XImage *xim ;
        rgbyte ccc[3]={ {0,0,66}, {55,0,0}, {0,44,0} } , col[4] ;

        col[0] = ccc[ (num_entry-1)%4 ]; col[1] = ccc[ (num_entry+0)%4 ];
        col[2] = ccc[ (num_entry+1)%4 ]; col[3] = col[0] ;
        bim = mri_make_rainbow( 4 , 3*im3d->dc->height/4 , 4 , col ) ;
        xim = rgb_to_XImage( im3d->dc , bim ) ;
        afni16_pixmap[num_entry-1] = XCreatePixmap( im3d->dc->display ,
                                      RootWindowOfScreen(im3d->dc->screen) ,
                                      bim->nx , bim->ny , im3d->dc->planes ) ;
        XPutImage( im3d->dc->display , afni16_pixmap[num_entry-1] ,
                   im3d->dc->origGC , xim , 0,0 , 0,0 , bim->nx , bim->ny ) ;
        MCW_kill_XImage( xim ) ;
        mri_free(bim) ;
      }

#if 0
      if( afni16_pixmap[num_entry-1] == XmUNSPECIFIED_PIXMAP && AFNI_yesenv("AFNI_LOGO16") ){
        Pixel fg16=ICON_bg, bg16=ICON_fg ; int ic ; char ename[32] ;
        char *fgn[7] = { "red", "blue-cyan", "green", "violet", "orange", "gray70", "yellow" };

        sprintf(ename,"AFNI_LOGO16_FOREGROUND_%c" , 'A'+num_entry-1 ) ;
                     ic = DC_find_closest_overlay_color(im3d->dc, getenv(ename) ) ;
        if( ic < 0 ) ic = DC_find_closest_overlay_color(im3d->dc, getenv("AFNI_LOGO16_FOREGROUND")) ;
        if( ic < 0 ) ic = DC_find_closest_overlay_color(im3d->dc, fgn[(num_entry-1)%7] ) ;
        if( ic >= 0 ) fg16 = im3d->dc->ovc->pix_ov[ic] ;

        sprintf(ename,"AFNI_LOGO16_BACKGROUND_%c" , 'A'+num_entry-1 ) ;
                     ic = DC_find_closest_overlay_color(im3d->dc, getenv(ename) ) ;
        if( ic < 0 ) ic = DC_find_closest_overlay_color(im3d->dc, getenv("AFNI_LOGO16_BACKGROUND")) ;
#if 0
        if( ic < 0 ) ic = im3d->dc->ovc->ov_darkest ;
#endif
        if( ic >= 0 ) bg16 = im3d->dc->ovc->pix_ov[ic] ;

        afni16_pixmap[num_entry-1] = XCreatePixmapFromBitmapData(
                                      XtDisplay(vwid->top_shell) ,
                                      RootWindowOfScreen(XtScreen(vwid->top_shell)) ,
                                      afni16_bits , afni16_width , afni16_height ,
                                      fg16 , bg16 ,
                                      DefaultDepthOfScreen(XtScreen(vwid->top_shell)) ) ;
      }
#endif

#endif  /* WANT_AFNI_BITMAP */
   }
#endif  /* if WANT any of the BITMAPs */

   if( afni16_pixmap[num_entry-1] != XmUNSPECIFIED_PIXMAP )
     XtVaSetValues( vwid->top_form , XmNbackgroundPixmap,afni16_pixmap[num_entry-1] , NULL ) ;

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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view->frame =
      XtVaCreateWidget(
         "dialog" , xmFrameWidgetClass , vwid->top_form ,
            XmNleftAttachment , XmATTACH_WIDGET ,
            XmNleftWidget     , imag->frame ,
            XmNtopAttachment  , XmATTACH_FORM ,
#ifdef USING_LESSTIF
            XmNleftOffset     , 9 ,
#else
            XmNleftOffset     , AFNI_FORM_SPACING ,
#endif
            XmNtopOffset      , AFNI_FORM_SPACING ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 5 ,
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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

   FIRST_widcall = 0 ;  /* 22 Dec 2009 */
   EXRETURN ;
}

/*------------------------------------------------------------------*/

void AFNI_raiseup_CB( Widget w , XtPointer cd , XtPointer cb )
{
   XMapRaised( XtDisplay(w) , XtWindow(XtParent(w)) ) ;
}

/*--------------------------------------------------------------------*/

void AFNI_make_wid1( Three_D_View *im3d )
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- popup menu to handle special imaging concerns ---*/

   imag->topper =           /* invisible widget to be parent of popup */
      XtVaCreateManagedWidget(
         "dialog" , xmSeparatorWidgetClass , imag->rowcol ,
            XmNseparatorType , XmNO_LINE ,
         NULL ) ;

#ifdef BAD_BUTTON3_POPUPS   /* 21 Jul 2003 */
   imag->popmenu =
      XmCreatePopupMenu( vwid->top_form, "menu" , NULL , 0 ) ;
#else
   imag->popmenu =
      XmCreatePopupMenu( imag->topper  , "menu" , NULL , 0 ) ;
#endif

   SAVEUNDERIZE(XtParent(imag->popmenu)) ; /* 27 Feb 2001 */

   VISIBILIZE_WHEN_MAPPED(imag->popmenu) ;
#if 0
    if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(imag->popmenu) ;
#endif

/***
   XtAddCallback( imag->popmenu ,
                  XmNunmapCallback , AFNI_imag_pop_CB , im3d ) ;
***/

   XtVaSetValues( imag->popmenu ,
                     XmNradioBehavior , True ,
                     XmNradioAlwaysOne , True  ,
                     XmNspacing      , 1 ,
                     XmNmarginHeight , 0 ,
                     XmNmarginWidth  , 0 ,
                  NULL ) ;

   /*--- instacorr set button in menu [06 May 2009] ---*/

   if( im3d->type == AFNI_3DDATA_VIEW ){
      imag->pop_instacorr_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
               LABEL_ARG("InstaCorr Set") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
      XtAddCallback( imag->pop_instacorr_pb , XmNactivateCallback ,
                     AFNI_imag_pop_CB , im3d ) ;
      XtSetSensitive( imag->pop_instacorr_pb , False ) ;

      imag->pop_icorrjump_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
               LABEL_ARG("InstaCorr SeedJump") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
      XtAddCallback( imag->pop_icorrjump_pb , XmNactivateCallback ,
                     AFNI_imag_pop_CB , im3d ) ;
      XtSetSensitive( imag->pop_icorrjump_pb , False ) ;
   } else {
      imag->pop_instacorr_pb = NULL ;
      imag->pop_icorrjump_pb = NULL ;
   }

   /*--- jumpback button in menu ---*/

   imag->pop_jumpback_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
            LABEL_ARG("Jumpback") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
      XtAddCallback( imag->pop_jumpto_ijk_pb , XmNactivateCallback ,
                     AFNI_imag_pop_CB , im3d ) ;
   } else {
      imag->pop_jumpto_ijk_pb = NULL ;
   }

   /*--- mnito button in menu [01 May 2002] ---*/

   if( im3d->type == AFNI_3DDATA_VIEW ){
      imag->pop_mnito_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
               LABEL_ARG("Jump to (MNI)") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
      XtAddCallback( imag->pop_mnito_pb , XmNactivateCallback ,
                     AFNI_imag_pop_CB , im3d ) ;
   } else {
      imag->pop_mnito_pb = NULL ;
   }

   /*--- sumato button in menu ---*/

   if( im3d->type == AFNI_3DDATA_VIEW ){
      imag->pop_sumato_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
               LABEL_ARG("SUMA to (node)") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
      XtAddCallback( imag->pop_sumato_pb , XmNactivateCallback ,
                     AFNI_imag_pop_CB , im3d ) ;
   } else {
      imag->pop_sumato_pb = NULL ;
   }

   /*--- Talairach To button in menu ---*/

   if( im3d->type == AFNI_3DDATA_VIEW ){
      imag->pop_talto_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
               LABEL_ARG("-Talairach to") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
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
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
         XtAddCallback( imag->pop_whereami_pb , XmNactivateCallback ,
                        AFNI_imag_pop_CB , im3d ) ;

         imag->pop_ttren_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
                  LABEL_ARG("-Atlas Colors") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
         XtAddCallback( imag->pop_ttren_pb , XmNactivateCallback ,
                        AFNI_imag_pop_CB , im3d ) ;

      } else {
        static int first=1 ;
        imag->pop_ttren_pb = imag->pop_whereami_pb = NULL ;
        if( first ){
          first = 0 ;
          fprintf(stderr,
           "\n++ WARNING: Can't find TTatlas+tlrc or TTatlas.nii.gz dataset for 'whereami'!\n"
             "++--------- See http://afni.nimh.nih.gov/pub/dist/data/\n" ) ;
        }
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XtAddCallback( imag->pop_imageonly_pb , XmNactivateCallback ,
                  AFNI_imag_pop_CB , im3d ) ;

   /*--- environment button in menu [05 Nov 2003] ---*/

#ifdef ALLOW_PLUGINS
   imag->pop_environment_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
            LABEL_ARG("Edit Environment") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XtAddCallback( imag->pop_environment_pb , XmNactivateCallback ,
                  AFNI_imag_pop_CB , im3d ) ;

   imag->pop_drawdataset_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
            LABEL_ARG("Draw ROI plugin") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XtAddCallback( imag->pop_drawdataset_pb , XmNactivateCallback ,
                  AFNI_imag_pop_CB , im3d ) ;
#else
   imag->pop_environment_pb = NULL ;
   imag->pop_drawdataset_pb = NULL ;
#endif

   /*--- frame to hold all crosshair stuff ---*/

   imag->crosshair_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , imag->rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- rowcol to manage crosshair stuff ---*/

   imag->crosshair_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , imag->crosshair_frame ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmVERTICAL   ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- label to display the crosshair location ---*/

   im3d->vinfo->old_crosshair_label = xstr = AFNI_crosshair_label( im3d ) ;

   im3d->vinfo->view_setter = -1 ;  /* 20 Feb 2003 */

   imag->crosshair_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , imag->crosshair_rowcol ,
            XmNrecomputeSize , False ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   MCW_register_help( imag->crosshair_label , AFNI_crosshair_label_help ) ;
   MCW_register_hint( imag->crosshair_label , "Coordinates of crosshair point" ) ;

   /*--- 12 Mar 2004: coordinate order popup menu ---*/

   if( im3d->type == AFNI_3DDATA_VIEW ){
    imag->crosshair_menu =
      XmCreatePopupMenu( imag->crosshair_label  , "menu" , NULL , 0 ) ;

    SAVEUNDERIZE(XtParent(imag->crosshair_menu)) ;
    VISIBILIZE_WHEN_MAPPED(imag->crosshair_menu) ;
    if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(imag->crosshair_menu) ;

    XtInsertEventHandler( imag->crosshair_label , /* handle events in label */
                             ButtonPressMask ,    /* button presses */
                             FALSE ,              /* nonmaskable events? */
                             AFNI_crosshair_EV ,  /* handler */
                             (XtPointer) im3d ,   /* client data */
                             XtListTail           /* last in queue */
                         ) ;

    (void) XtVaCreateManagedWidget(
             "menu" , xmLabelWidgetClass , imag->crosshair_menu ,
                LABEL_ARG("-Set Coord Order-") ,
                XmNrecomputeSize , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

    (void) XtVaCreateManagedWidget(
             "menu" , xmSeparatorWidgetClass , imag->crosshair_menu ,
              XmNseparatorType , XmSINGLE_LINE , NULL ) ;

    imag->crosshair_dicom_pb =
       XtVaCreateManagedWidget(
          "menu" , xmPushButtonWidgetClass , imag->crosshair_menu ,
             LABEL_ARG(" RAI=DICOM order") ,
             XmNmarginHeight , 0 ,
             XmNtraversalOn , True  ,
             XmNinitialResourcesPersistent , False ,
          NULL ) ;
    XtAddCallback( imag->crosshair_dicom_pb , XmNactivateCallback ,
                   AFNI_crosshair_pop_CB , im3d ) ;

    imag->crosshair_spm_pb =
       XtVaCreateManagedWidget(
          "menu" , xmPushButtonWidgetClass , imag->crosshair_menu ,
             LABEL_ARG(" LPI=SPM order") ,
             XmNmarginHeight , 0 ,
             XmNtraversalOn , True  ,
             XmNinitialResourcesPersistent , False ,
          NULL ) ;
    XtAddCallback( imag->crosshair_spm_pb , XmNactivateCallback ,
                   AFNI_crosshair_pop_CB , im3d ) ;

   } /*- end of crosshair_label popup menu -*/

   /*--- 01 Jan 1997: horizontal rowcol for crosshair stuff ---*/

STATUS("making imag->xhair_rowcol") ;

     imag->xhair_rowcol =
        XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , imag->crosshair_rowcol ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmHORIZONTAL ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmVERTICAL   ,
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->name_xyz_lab =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , imag->xyz_rowcol ,
            LABEL_ARG( xyz_3DIM ) ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->image_xyz_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->xyz_rowcol ,
            LABEL_ARG("Image") ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->graph_xyz_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->xyz_rowcol ,
            LABEL_ARG("Graph") ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , True  ,
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->name_yzx_lab =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , imag->yzx_rowcol ,
            LABEL_ARG( yzx_3DIM ) ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->image_yzx_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->yzx_rowcol ,
            LABEL_ARG("Image") ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->graph_yzx_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->yzx_rowcol ,
            LABEL_ARG("Graph") ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , True  ,
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->name_zxy_lab =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , imag->zxy_rowcol ,
            LABEL_ARG( zxy_3DIM ) ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->image_zxy_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->zxy_rowcol ,
            LABEL_ARG("Image") ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   imag->graph_zxy_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->zxy_rowcol ,
            LABEL_ARG("Graph") ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNrecomputeSize , False ,
            XmNtraversalOn   , True  ,
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

   XtInsertEventHandler( imag->image_xyz_pb ,
                           ButtonPressMask , FALSE ,
                           AFNI_viewbut_EV , (XtPointer)im3d , XtListTail ) ;
   XtInsertEventHandler( imag->graph_xyz_pb ,
                           ButtonPressMask , FALSE ,
                           AFNI_viewbut_EV , (XtPointer)im3d , XtListTail ) ;
   XtInsertEventHandler( imag->image_yzx_pb ,
                           ButtonPressMask , FALSE ,
                           AFNI_viewbut_EV , (XtPointer)im3d , XtListTail ) ;
   XtInsertEventHandler( imag->graph_yzx_pb ,
                           ButtonPressMask , FALSE ,
                           AFNI_viewbut_EV , (XtPointer)im3d , XtListTail ) ;
   XtInsertEventHandler( imag->image_zxy_pb ,
                           ButtonPressMask , FALSE ,
                           AFNI_viewbut_EV , (XtPointer)im3d , XtListTail ) ;
   XtInsertEventHandler( imag->graph_zxy_pb ,
                           ButtonPressMask , FALSE ,
                           AFNI_viewbut_EV , (XtPointer)im3d , XtListTail ) ;

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
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmVERTICAL   ,
            XmNisAligned   , False ,
            XmNtraversalOn , True  ,
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

   { char *hh[] = { "View data in original coordinates" ,
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view->marks_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , view->marks_frame ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmVERTICAL   ,
            XmNisAligned   , False ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- pushbutton to allow user to define marks ---*/

   view->define_marks_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , view->marks_rowcol ,
            LABEL_ARG("Define Markers ->") ,
            XmNmarginHeight , 1 ,
            XmNtraversalOn , True  ,
            XmNalignment , XmALIGNMENT_CENTER ,
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
   if( AFNI_yesenv("AFNI_SEE_OVERLAY") ) im3d->vinfo->func_visible = True ;
   im3d->vinfo->func_visible_count = 0 ;

   view->func_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , view->rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view->func_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , view->func_frame ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmVERTICAL   ,
            XmNisAligned   , False ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- pushbutton to allow user to define functions ---*/

   view->define_func_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , view->func_rowcol ,
            LABEL_ARG("Define OverLay ->") ,
            XmNmarginHeight , 1 ,
            XmNtraversalOn , True  ,
            XmNalignment , XmALIGNMENT_CENTER ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view->func_pb_inverted = False ;

   XtAddCallback( view->define_func_pb , XmNactivateCallback ,
                  AFNI_define_CB , im3d ) ;

   MCW_register_help( view->define_func_pb ,
     "Use this to control the thresholds,\n"
      "colors, etc. for overlays" ) ;
   MCW_register_hint( view->define_func_pb ,
                      "Open/close overlay control panel" ) ;

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
                         "Visibility of color overlay" ) ;

   ADDTO_KILL(im3d->kl,view->see_func_bbox) ;

   view_count ++ ;

   /*--- pushbutton to allow user to define datamode ---*/

   view->define_dmode_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , view->rowcol ,
            LABEL_ARG("Define Datamode ->") ,
            XmNmarginHeight , 1 ,
            XmNtraversalOn , True  ,
            XmNalignment , XmALIGNMENT_CENTER ,
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   view->dataset_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , view->dataset_frame ,
            XmNpacking     , XmPACK_TIGHT ,
            XmNorientation , XmVERTICAL   ,
            XmNisAligned   , False ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*-- 03 Dec 2009: move Session change stuff to a private rowcol --*/

   view->session_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , view->dataset_rowcol ,
            XmNpacking      , XmPACK_TIGHT ,
            XmNorientation  , XmHORIZONTAL ,
            XmNtraversalOn  , True  ,
            XmNmarginHeight , 0 ,
            XmNmarginWidth  , 0 ,
            XmNspacing      , 0 ,
            XmNadjustLast   , False ,
            XmNisAligned    , False ,
            XmNentryAlignment , XmALIGNMENT_CENTER ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- pushbuttons for session choice ---*/

   xstr = XmStringCreateLtoR("DataDir",XmFONTLIST_DEFAULT_TAG );
   view->sess_lab =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , view->session_rowcol ,
            XmNrecomputeSize , False ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
            XmNmarginHeight , 0 ,
            XmNmarginWidth  , 0 ,
            XmNadjustMargin , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XmStringFree( xstr ) ;
   MCW_register_hint( view->sess_lab ,
                      "Switch = change dataset directory; Read = open a new dataset directory" ) ;

   view->choose_sess_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , view->session_rowcol ,
            LABEL_ARG("Switch") ,
            XmNmarginHeight , 0 ,
            XmNmarginWidth  , 1 ,
            XmNtraversalOn , True  ,
            XmNalignment , XmALIGNMENT_CENTER ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XtAddCallback( view->choose_sess_pb , XmNactivateCallback ,
                  AFNI_choose_dataset_CB , im3d ) ;
   MCW_register_help( view->choose_sess_pb ,
     "Use this to choose from which\n"
     "session 3D datasets may be viewed." ) ;
   MCW_register_hint( view->choose_sess_pb ,
                      "Switch between session directories" ) ;
   MCW_set_widget_bg( view->choose_sess_pb , "black"   , 0 ) ;
   MCW_set_widget_fg( view->choose_sess_pb , "#ffddaa" ) ;

   view->read_sess_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , view->session_rowcol ,
            LABEL_ARG("Read") ,
            XmNmarginHeight , 0 ,
            XmNmarginWidth  , 0 ,
            XmNmarginLeft   , 0 ,
            XmNmarginRight  , 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XtAddCallback( view->read_sess_pb , XmNactivateCallback ,
                  AFNI_read_sess_CB , im3d ) ;
   MCW_register_hint( view->read_sess_pb ,
                      "Read in a new session directory" ) ;
   MCW_set_widget_bg( view->read_sess_pb , "#ffddaa" , 0 ) ;

   view_count ++ ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , view->dataset_rowcol ,
                XmNseparatorType , XmSHADOW_ETCHED_IN ,
            NULL ) ;

   /*-- 02 Feb 2007: move Underlay and Overlay choosers into a rowcol --*/

   view->choose_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , view->dataset_rowcol ,
            XmNpacking      , XmPACK_COLUMN ,
            XmNnumColumns   , 2 ,
            XmNorientation  , XmVERTICAL   ,
            XmNtraversalOn  , True  ,
            XmNmarginHeight , 0 ,
            XmNmarginWidth  , 0 ,
            XmNspacing      , 0 ,
            XmNadjustLast   , False ,
            XmNentryAlignment , XmALIGNMENT_CENTER ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

#define UNDERLAY_BUTTON                                             \
 do{                                                                \
   view->choose_anat_pb =                                           \
      XtVaCreateManagedWidget(                                      \
         "dialog" , xmPushButtonWidgetClass , view->choose_rowcol , \
            LABEL_ARG("UnderLay") ,                                 \
            XmNmarginHeight , 1 ,                                   \
            XmNtraversalOn , True  ,                                \
            XmNinitialResourcesPersistent , False ,                 \
         NULL ) ;                                                   \
   XtAddCallback( view->choose_anat_pb , XmNactivateCallback ,      \
                  AFNI_choose_dataset_CB , im3d ) ;                 \
   MCW_register_help( view->choose_anat_pb ,                        \
        "Use this to choose which 3D\n"                             \
        "dataset to view as the underlay\n"                         \
        "(from the current session).\n\n"                           \
        "N.B.: Datasets which can be\n"                             \
        "  graphed are marked with a\n"                             \
        "  '*' after their names.\n"                                \
        "  Datasets that are compressed\n"                          \
        "  have 'z' after their names."                             \
   ) ;                                                              \
   MCW_register_hint( view->choose_anat_pb ,                        \
                      "Switch datasets for underlay/graphs" ) ;     \
   MCW_set_widget_bg( view->choose_anat_pb , "black" , 0 ) ;        \
   MCW_set_widget_fg( view->choose_anat_pb , "#ffddaa" ) ;          \
   view_count ++ ;                                                  \
 } while(0)

#define OVERLAY_BUTTON                                              \
 do{                                                                \
   view->choose_func_pb =                                           \
      XtVaCreateManagedWidget(                                      \
         "dialog" , xmPushButtonWidgetClass , view->choose_rowcol , \
            LABEL_ARG("OverLay") ,                                  \
            XmNmarginHeight , 1 ,                                   \
            XmNtraversalOn , True  ,                                \
            XmNinitialResourcesPersistent , False ,                 \
         NULL ) ;                                                   \
   XtAddCallback( view->choose_func_pb , XmNactivateCallback ,      \
                  AFNI_choose_dataset_CB , im3d ) ;                 \
   MCW_register_help( view->choose_func_pb ,                        \
     "Use this to choose which\n"                                   \
     "overlay 3D dataset to view\n"                                 \
     "(from the current session).\n"                                \
     "N.B.: Datasets that are compressed\n"                         \
     "  have 'z' after their names."                                \
    ) ;                                                             \
   MCW_register_hint( view->choose_func_pb ,                        \
                      "Switch datasets for color overlay" ) ;       \
   MCW_set_widget_bg( view->choose_func_pb , "#ffddaa" , 0 ) ;      \
   view_count ++ ;                                                  \
 } while(0)

   if( AFNI_yesenv("AFNI_OVERLAY_ONTOP") ){  /* 05 Feb 2007 */
     OVERLAY_BUTTON  ;
     UNDERLAY_BUTTON ;
   } else {
     UNDERLAY_BUTTON ;
     OVERLAY_BUTTON  ;
   }

   /* 02 Feb 2007: new Rescan This button here */

   if( !AFNI_yesenv("AFNI_RESCAN_AT_SWITCH") ){
     view->rescan_pb =
        XtVaCreateManagedWidget(
           "dialog" , xmPushButtonWidgetClass , view->choose_rowcol ,
              LABEL_ARG("RescanTh") ,
              XmNmarginHeight , 1 ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;
     XtAddCallback( view->rescan_pb , XmNactivateCallback ,
                    AFNI_rescan_CB , im3d ) ;
     MCW_register_hint( view->rescan_pb ,
                        "Read current session again" ) ;
     MCW_register_help( view->rescan_pb ,
                        "Read current session again\n"
                        "to see if new datasets were\n"
                        "added.  Exactly the same as\n"
                        " Define Datamode->Rescan This\n"
                        "Can also set (in .afnirc)\n"
                        " AFNI_RESCAN_AT_SWITCH = YES\n"
                        "to rescan each time you hit\n"
                        "'Overlay' or 'Underlay'."     ) ;
   } else {
     view->rescan_pb =              /* 23 Feb 2007 */
        XtVaCreateManagedWidget(
           "dialog" , xmPushButtonWidgetClass , view->choose_rowcol ,
              LABEL_ARG("EditEnv") ,
              XmNmarginHeight , 1 ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;
     XtAddCallback( view->rescan_pb , XmNactivateCallback ,
                    AFNI_editenv_CB , im3d ) ;
     MCW_register_hint( view->rescan_pb ,
                        "open Edit Environment controls" ) ;
   }

   /* NIML+PO button here -- 02 Feb 2007 */
   /* Set label to match current status of 01 Feb 2008 */

   { char *label ; int have_po , have_ni ;
     have_po = !GLOBAL_argopt.noplugouts || AFNI_have_plugouts() ;
     have_ni =  GLOBAL_argopt.yes_niml   || AFNI_have_niml() ;
          if( have_po == have_ni ) label = "NIML+PO" ;
     else if( have_po )            label = "NIML"    ;
     else if( have_ni )            label = "Plgouts" ;
     else                          label = "WTF?"    ;  /* should not happen */
     view->nimlpo_pb =
        XtVaCreateManagedWidget(
           "dialog" , xmPushButtonWidgetClass , view->choose_rowcol ,
              LABEL_ARG(label) ,
              XmNmarginHeight , 1 ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;
   }
   XtAddCallback( view->nimlpo_pb , XmNactivateCallback ,
                  AFNI_nimlpo_CB , im3d ) ;
   if( AFNI_have_niml() && AFNI_have_plugouts() ){
     XtSetSensitive(view->nimlpo_pb,False) ;
   } else {
     MCW_register_hint( view->nimlpo_pb ,
                        "Start NIML and Plugout TCP/IP sockets") ;
     MCW_register_help( view->nimlpo_pb ,
                        "Start listening for NIML\n"
                        "and Plugout TCP/IP sockets.\n"
                        "If you forgot to use options\n"
                        "'-niml' and '-yesplugouts'\n"
                        "on the command line, and\n"
                        "are expecting AFNI to talk\n"
                        "SUMA and/or plugout_drive."  ) ;
   }

   /* 19 Aug 2002: Surface chooser! */

   view->choose_surf_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , view->dataset_rowcol ,
            LABEL_ARG("Control Surface") ,
            XmNmarginHeight , 1 ,
            XmNtraversalOn , True  ,
            XmNalignment , XmALIGNMENT_CENTER ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XtSetSensitive( view->choose_surf_pb , False ) ;
   XtAddCallback( view->choose_surf_pb , XmNactivateCallback ,
                  AFNI_choose_surface_CB , im3d ) ;

   MCW_register_help( view->choose_surf_pb ,
                       "Use this to control the display of\n"
                       "overlaid surfaces in image viewers:\n"
                       "\n"
                       "Surface nodes will have little boxes\n"
                       "  drawn, when they appear inside a slice.\n"
                       "Surface triangles will have line segments\n"
                       "  drawn, where they intersect a slice\n"
                       "  center-plane."
                    ) ;
   MCW_register_hint( view->choose_surf_pb ,
                      "Control surface overlay" ) ;

   view->swid = NULL ;   /* no widgets for surface yet */

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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( view->popchoose_sess_pb , XmNactivateCallback ,
                  AFNI_choose_dataset_CB , im3d ) ;

   view->popchoose_anat_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
            LABEL_ARG("Switch UnderLay") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( view->popchoose_anat_pb , XmNactivateCallback ,
                  AFNI_choose_dataset_CB , im3d ) ;

   view->popchoose_func_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , imag->popmenu ,
            LABEL_ARG("Switch OverLay") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , True  ,
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
   XtManageChild( view->session_rowcol ) ;
   XtManageChild( view->choose_rowcol ) ;
   XtManageChild( view->dataset_rowcol ) ;
   XtManageChild( view->rowcol ) ;

   /** compute height of widgets in the view-> column **/

   xstr = XmStringCreateLtoR("(y[M",XmFONTLIST_DEFAULT_TAG );
   XtVaGetValues( view->choose_func_pb , XmNfontList , &xflist , NULL ) ;
   view_height = (10 + XmStringHeight(xflist,xstr)) * view_count ;
   XmStringFree( xstr ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void AFNI_make_wid2( Three_D_View *im3d )
{
   int ii ;
   Widget hrc ;  /* 30 Mar 2001 */
   Widget www ;  /* 26 Mar 2007 */

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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*----- create radio buttons in pairs,
           one in the rowcol and one in the popmenu;
           N.B.: they are created unmanaged and will be managed
                 later, depending on defined marks for the view -----*/

   XtVaSetValues( marks->tog_rowcol ,
                     XmNradioBehavior , True ,
                     XmNradioAlwaysOne , True  ,
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
     Dimension isiz=0 ;
     Pixel  fg_pix=0 ;

     XtVaGetValues( marks->tog_rowcol , XmNforeground , &fg_pix , NULL ) ;

     for( ib=0 ; ib < MARKS_MAXNUM ; ib++ ){

        marks->tog[ib] =
           XtVaCreateWidget(
              "dialog" , xmToggleButtonWidgetClass , marks->tog_rowcol ,
                 XmNvisibleWhenOff , True ,
                 XmNmarginHeight , 0 ,
                 XmNmarginWidth  , 0 ,
                 XmNselectColor  , fg_pix ,
                 XmNtraversalOn  , True  ,
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
                 XmNtraversalOn  , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*----- set point pushbutton -----*/

   marks->action_set_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , marks->action_rowcol ,
            LABEL_ARG("Set") ,
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( marks->pop_clear_pb , XmNactivateCallback ,
                  AFNI_marks_action_CB , im3d ) ;

   /*----- a "quality" button (not on the popup menu) -----*/

   marks->action_quality_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , marks->action_rowcol ,
            LABEL_ARG("Quality?") ,
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
#endif

   /**--------- 05 Sep 2006: creat menu hidden on the thr_label ---------**/

#if 1
   { static char *onofflabel[] = { "Use Threshold?" } ;

#ifdef BAD_BUTTON3_POPUPS
   func->thr_menu = XmCreatePopupMenu( func->thr_rowcol, "menu", NULL, 0 ) ;
#else
   func->thr_menu = XmCreatePopupMenu( func->thr_label , "menu", NULL, 0 ) ;
#endif

   SAVEUNDERIZE(XtParent(func->thr_menu)) ;
   VISIBILIZE_WHEN_MAPPED(func->thr_menu) ;
#if 0
   if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(func->thr_menu) ;
#endif

   XtInsertEventHandler( func->thr_label ,       /* handle events in label */

                               0
                             | ButtonPressMask   /* button presses */
                            ,
                            FALSE ,              /* nonmaskable events? */
                            AFNI_thr_EV ,        /* handler */
                            (XtPointer) im3d ,   /* client data */
                            XtListTail           /* last in queue */
                          ) ;

   /* This --- Cancel --- label does not cause the hangup, so it is
   left alone. See related comments in afni_graph.c
                           LessTif patrol, Jan 07 09 */
   (void) XtVaCreateManagedWidget(
            "dialog" , xmLabelWidgetClass , func->thr_menu ,
               LABEL_ARG("--- Cancel ---") ,
               XmNrecomputeSize , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , func->thr_menu ,
             XmNseparatorType , XmSINGLE_LINE , NULL ) ;

   func->thr_onoff_bbox = new_MCW_bbox( func->thr_menu ,
                                        1 , onofflabel ,
                                        MCW_BB_check , MCW_BB_noframe ,
                                        AFNI_thronoff_change_CB ,
                                        (XtPointer)im3d ) ;
   im3d->vinfo->thr_onoff = 1 ;
   MCW_set_bbox( func->thr_onoff_bbox , 1 ) ;
   MCW_reghint_children( func->thr_onoff_bbox->wrowcol ,
                         "Temporarily ignore threshold?" ) ;

   /* AutoThreshold button */

   func->thr_autothresh_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , func->thr_menu ,
            LABEL_ARG("AutoThreshold") ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XtAddCallback( func->thr_autothresh_pb , XmNactivateCallback ,
                  AFNI_func_autothresh_CB , im3d ) ;
   MCW_register_hint( func->thr_autothresh_pb , "Compute threshold automatically NOW" ) ;

   /* Threshold sign arrowval [08 Aug 2007] */

   { static char *thr_sign_label[3] = { "Pos & Neg",
                                        "Pos only" ,
                                        "Neg only"  } ;
     im3d->vinfo->thr_sign = 0 ;  /* default = "Pos & Neg" */
     func->thr_sign_av =
        new_MCW_arrowval(
           func->thr_menu ,        /* parent */
           "Sign" ,                /* label */
           AVOPT_STYLE ,           /* arrow directions */
           0  ,                    /* min value */
           2  ,                    /* max value */
           im3d->vinfo->thr_sign , /* init value */
           MCW_AV_editext ,        /* input/output text display */
           0 ,                     /* 0 decimal shift */
           AFNI_func_thrsign_CB ,  /* routine to call after click */
           (XtPointer) im3d ,      /* data to pass */
           MCW_av_substring_CB ,   /* text creation routine */
           thr_sign_label          /* data for above */
        ) ;
      MCW_reghint_children( func->thr_sign_av->wrowcol ,
                            "show Positives, Negatives, or Both?" ) ;
    }

   /* FDR button */

   func->thr_fdr_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , func->thr_menu ,
            LABEL_ARG("Add FDR Curves") ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XtAddCallback( func->thr_fdr_pb , XmNactivateCallback ,
                  AFNI_func_fdr_CB , im3d ) ;
   MCW_register_hint( func->thr_fdr_pb ,
                      "Compute FDR curves for OLay statistical sub-bricks" ) ;

   } /*---- end of thr_menu creation for top of threshold slider ----*/
#endif

   FIX_SCALE_VALUE(im3d) ;  /* just in case */

#define SCALE_EXTRA 66

  {Widget qqq ; int iqqq ;
   char thr_str[] = "-----------" ;
   char zork[2] ;

   int smax , stop , decim , sstep ;                  /* 30 Nov 1997:       */
   decim = THR_TOP_EXPON ;                            /* compute parameters */
   smax  = (int)( pow(10.0,decim) + 0.001 ) ;         /* for scale display. */
   stop  = smax - 1 ;
   sstep = smax / 1000 ;
   { char *eee = getenv("AFNI_THRESH_BIGSTEP") ;      /* 09 May 2003 */
     if( eee != NULL ){ iqqq=strtol(eee,NULL,10); if(iqqq > 0) sstep=iqqq; }
   }
   if( sstep < 1 ) sstep = 1 ; else if( sstep > (smax/10) ) sstep = (smax/10) ;

#ifdef BOXUP_SCALE
   qqq = XtVaCreateManagedWidget(
           "dialog" , xmFrameWidgetClass , func->thr_rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

#ifdef FIX_SCALE_SIZE_PROBLEM
   XtVaSetValues( func->thr_scale ,
                    XmNuserData , (XtPointer)ITOP(sel_height) ,
                  NULL ) ;
#endif

#ifdef USING_LESSTIF
   XtVaSetValues( func->thr_scale , XmNscaleWidth,24 , NULL ) ;
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
      "for overlay display.\n\n"
      "* Threshold doesn't apply\n"
      "  if dataset is RGB-format.\n"
      "* Threshold applies to 'Thr'\n"
      "  sub-brick.\n"
    ) ;
#if 0
   MCW_register_hint( func->thr_scale , "Threshold for color overlay" ) ;
#endif

   /** Mar 1996: label for computed p-value, under scale **/

   func->thr_pval_label =
      XtVaCreateManagedWidget(
         "font8" , xmLabelWidgetClass , func->thr_rowcol ,
            LABEL_ARG( "p=N/A   \nq=N/A   " ) ,
            XmNrecomputeSize , False ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   MCW_register_help( func->thr_pval_label ,
      " \n"
      " Shows the estimated significance (p-value) of the threshold\n"
      " slider if possible.  This is the 'uncorrected' or per-voxel\n"
      " value of 'p'.\n"
      "* If not possible, will display as '[N/A]' instead.\n"
      "* p's that display as 1.2-7 mean 1.2 x 10^(-7).\n"
      "* If FDR curves are pre-computed in the dataset header,\n"
      "  then the False Discovery Rate q-value will also be shown.\n"
      "* You can add FDR curves to a dataset with '3drefit -addFDR'\n"
      "   or by using the 'Add FDR Curves' button on the right-click\n"
      "   popup menu on the label atop the threshold slider.\n"
      "* FDR q = estimate of the fraction of above-threshold voxels\n"
      "   that are false detections.\n"
      "* MDF = CRUDE estimate of the fraction of true positive voxels\n"
      "   that are below the current threshold.\n"
      "* MDF is shown in the hint for the label below the slider.\n "
   ) ;
   MCW_register_hint( func->thr_pval_label , "Nominal p-value per voxel; FDR q-value" ) ;

#if 0
   /* 05 Sep 2006: duplicate popup from thr_label */

   XtInsertEventHandler( func->thr_pval_label ,  /* handle events in label */

                               0
                             | ButtonPressMask   /* button presses */
                            ,
                            FALSE ,              /* nonmaskable events? */
                            AFNI_thr_EV ,        /* handler */
                            (XtPointer)im3d ,    /* client data */
                            XtListTail           /* last in queue */
                          ) ;
#endif

   /** Jul 1997: optmenu to choose top value for scale **/

   func->thr_top_av = new_MCW_arrowval( func->thr_rowcol ,
                                        "**" ,
                                        AVOPT_STYLE ,
                                        0,THR_TOP_EXPON,0 ,
                                        MCW_AV_notext , 0 ,
                                        AFNI_thresh_top_CB , (XtPointer)im3d ,
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->inten_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , func->inten_rowcol ,
            LABEL_ARG("Inten") ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /**-- 17 Dec 1997: pbar menu hidden on the inten_label --**/

#ifdef BAD_BUTTON3_POPUPS   /* 21 Jul 2003 */
   func->pbar_menu = XmCreatePopupMenu( func->inten_rowcol, "menu", NULL, 0 ) ;
#else
   func->pbar_menu = XmCreatePopupMenu( func->inten_label , "menu", NULL, 0 ) ;
#endif

   SAVEUNDERIZE(XtParent(func->pbar_menu)) ; /* 27 Feb 2001 */
   VISIBILIZE_WHEN_MAPPED(func->pbar_menu) ;
   if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(func->pbar_menu) ;

   XtInsertEventHandler( func->inten_label ,      /* handle events in label */

                               0
                             | ButtonPressMask   /* button presses */
                            ,
                            FALSE ,              /* nonmaskable events? */
                            AFNI_pbar_EV ,       /* handler */
                            (XtPointer)im3d ,    /* client data */
                            XtListTail           /* last in queue */
                          ) ;

#if 0
   allow_MCW_optmenu_popup(0) ;  /* 12 Dec 2001 */
#endif

   /* This --- Cancel --- label does not cause the hangup, so it is
   left alone. See related comments in afni_graph.c
                           LessTif patrol, Jan 07 09 */
   (void) XtVaCreateManagedWidget(
            "dialog" , xmLabelWidgetClass , func->pbar_menu ,
               LABEL_ARG("--- Cancel ---") ,
               XmNrecomputeSize , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , func->pbar_menu ,
             XmNseparatorType , XmSINGLE_LINE , NULL ) ;

   /*--- environment button in menu [10 Feb 2004] ---*/

#ifdef ALLOW_PLUGINS
   func->pbar_environment_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , func->pbar_menu ,
            LABEL_ARG("Edit Environment") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XtAddCallback( func->pbar_environment_pb , XmNactivateCallback ,
                  AFNI_pbar_CB , im3d ) ;
#else
   func->pbar_environment_pb = NULL ;
#endif

   func->pbar_equalize_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , func->pbar_menu ,
            LABEL_ARG("Equalize Spacing") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
                             (XtPointer)im3d ,     /* data for above */
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
                             (XtPointer)im3d ,     /* data for above */
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
                             (XtPointer)im3d ,     /* data for above */
                             MCW_av_substring_CB , /* text creation routine */
                             AFNI_dummy_av_label   /* data for above */
                           ) ;

   MCW_reghint_children( func->pbar_transform2D_av->wrowcol ,
                         "Transform overlay image values" ) ;
   XtUnmanageChild( func->pbar_transform2D_av->wrowcol ) ;
   func->pbar_transform2D_index = 0 ;
   func->pbar_transform2D_func  = NULL ;

#if 0
   allow_MCW_optmenu_popup(1) ;  /* 12 Dec 2001 */
#endif

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
                          (XtPointer)im3d     ) ;     /* callback data */

     /* 04 Feb 2002: colorscale-ize? */

     if( im3d->dc->visual_class == TrueColor ){
       char *eee = getenv("AFNI_COLORSCALE_DEFAULT") ;
       if( eee == NULL ) eee = getenv("AFNI_COLOR_SCALE_DEFAULT") ;
       if( eee == NULL || strcmp(eee,"NO") != 0 ){
         PBAR_set_bigmode( func->inten_pbar , 1 , pmin,pmax ) ;
         PBAR_set_bigmap( func->inten_pbar , eee ) ;
       }
     }
   }

   func->inten_pbar->parent       = (XtPointer)im3d ;
   func->inten_pbar->mode         = (im3d->vinfo->use_posfunc) ? (1) : (0) ;
   func->inten_pbar->npan_save[0] = INIT_panes_sgn ;
   func->inten_pbar->npan_save[1] = INIT_panes_pos ;
   func->inten_pbar->hide_changes = INIT_panes_hide ;

   AFNI_setup_inten_pbar( im3d ) ;  /* other setup stuff (afni_func.c) */

   MCW_reghelp_children( func->inten_pbar->panew ,
      "Drag the separator bars to alter the thresholds.\n"
      "Click in a pane to alter the color for that range.\n\n"
      "The overlay dataset value that maps to 1.0 is\n"
      "determined by the 'autoRange' controls to the right.\n"
      "\n"
      "In 'continuous' colorscale mode, Button-1 click flips\n"
      "colors top-to-bottom;  Button-3 click shows a menu of\n"
      "available colorscales.\n"
   ) ;

   MCW_reghelp_children( func->inten_pbar->top ,
      "Drag the separator bars to alter the thresholds.\n"
      "Click in a pane to alter the color for that range.\n\n"
      "The overlay dataset value that maps to 1.0 is\n"
      "determined by the 'autoRange' controls to the right.\n"
      "\n"
      "In 'continuous' colorscale mode, Button-1 click flips\n"
      "colors top-to-bottom;  Button-3 click shows a menu of\n"
      "available colorscales.\n"
   ) ;

   MCW_register_help( func->inten_label ,
      "Drag the separator bars to alter the thresholds.\n"
      "Click in a pane to alter the color for that range.\n\n"
      "The overlay dataset value that maps to 1.0 is\n"
      "determined by the 'autoRange' controls to the right.\n\n"
      "N.B.: A popup menu to control the palette\n"
      "      setup is 'hidden' under this label."
   ) ;

   MCW_register_hint( func->inten_label ,
                      "Control overlay colors" ) ;

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , func->inten_rowcol ,
                XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

   func->inten_av = new_MCW_arrowval(
                       func->inten_rowcol ,
                        "#" ,
                        AVOPT_STYLE ,
                        NPANE_MIN , NPANE_MAX+1 ,
                        (func->inten_pbar->bigmode) ? NPANE_MAX+1 : npane ,
                        MCW_AV_notext , 0 ,
                        AFNI_inten_av_CB , func->inten_pbar ,
                        AFNI_inten_av_texter,NULL ) ;

   if( AVOPT_STYLE == MCW_AV_optmenu )
      AVOPT_columnize( func->inten_av , 2 ) ;

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
                    AFNI_inten_bbox_CB , (XtPointer)im3d ) ;

   func->inten_bbox->parent = (XtPointer)im3d ;

   MCW_set_bbox( func->inten_bbox ,
                 (im3d->vinfo->use_posfunc) ? (1) : (0) ) ;

   MCW_reghelp_children( func->inten_bbox->wrowcol ,
                         "Pressed In: Displays only positive overlay\n"
                         "            values in the 'pbar' above and\n"
                         "            in the color overlays.\n"
                         "       Out: Displays positive and negative\n"
                         "            overlay values.\n\n"
                         "N.B.: Zero overlay values are never overlaid." ) ;
   MCW_reghint_children( func->inten_bbox->wrowcol ,
                         "Use positive-only or signed overlay values" ) ;

   ADDTO_KILL(im3d->kl,func->inten_bbox) ;

   /*-- options controls --*/

   func->options_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->rowcol ,
            XmNorientation , XmVERTICAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->options_top_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->options_rowcol ,
            XmNorientation , XmHORIZONTAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNspacing     , 5 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->options_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , func->options_top_rowcol ,
            LABEL_ARG("Background ") ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

#define VEDIT_COLOR "#000088"
#define VEDIT_NOPT  4
   { static char *options_vedit_label[] =
       { "Clusters" , "InstaCorr" , "InstaCalc" , "GrpInCorr" } ;
     int nopt = (num_entry==1) ? VEDIT_NOPT : VEDIT_NOPT-1 ;
     func->options_vedit_av = new_MCW_arrowval(
                               func->options_top_rowcol , /* parent Widget */
                               NULL ,                     /* label */
                               MCW_AV_optmenu ,           /* option menu style */
                               0 ,                        /* first option */
                               nopt - 1 ,                 /* last option */
                               0 ,                        /* initial selection */
                               MCW_AV_readtext ,          /* ignored but needed */
                               0 ,                        /* ditto */
                               AFNI_vedit_CB  ,           /* callback when changed */
                               (XtPointer)im3d ,          /* data for above */
                               MCW_av_substring_CB ,      /* text creation routine */
                               options_vedit_label        /* data for above */
                             ) ;
     colorize_MCW_optmenu( func->options_vedit_av , VEDIT_COLOR  , -1 ) ;
   }
   func->options_vedit_av->parent = (XtPointer)im3d ;
   MCW_reghelp_children( func->options_vedit_av->wrowcol ,
                         "Choose which set of controls\n"
                         "for on-the-fly functional\n"
                         "overlay editing are visible\n"
                         "directly below this menu."     ) ;
   MCW_reghint_children( func->options_vedit_av->wrowcol ,
                         "On-the-fly overlay editing control choice") ;
   ADDTO_KILL(im3d->kl,func->options_vedit_av) ;

   XtManageChild( func->options_top_rowcol ) ;

   func->cwid = NULL;
   func->clu_rep = NULL; func->clu_list = NULL; func->clu_index = -1;
   func->clu_det = NULL; func->clu_num  = 0 ;

   func->iwid = NULL ;  /* 17 Sep 2009 */

   /*-- 26 Mar 2007: rowcol for clustering stuff --*/

   func->ulaclu_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->options_rowcol ,
            XmNorientation , XmHORIZONTAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNspacing     , 5 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*-- underlay type --*/

   func->underlay_bbox =
      new_MCW_bbox( func->ulaclu_rowcol ,
                    LAST_UNDERLAY_TYPE+1 , UNDERLAY_typestr ,
                    MCW_BB_radio_one ,
                    MCW_BB_frame ,
                    AFNI_underlay_CB , (XtPointer)im3d ) ;
   func->underlay_bbox->parent = (XtPointer)im3d ;
   MCW_set_bbox( func->underlay_bbox , 1 << im3d->vinfo->underlay_type ) ;
   MCW_reghelp_children( func->underlay_bbox->wrowcol ,
      "Use these buttons to choose\n"
      "whether the underlay or\n"
      "overlay images appear\n"
      "as the background display" ) ;
   ADDTO_KILL(im3d->kl,func->underlay_bbox) ;

   { static char *hh[] = { "Use underlay dataset for background" ,
                           "Use overlay dataset for background" ,
                           "Use thresholded overlay dataset for background" } ;
     MCW_bbox_hints( func->underlay_bbox , 3 , hh ) ;
   }

   /*--- 26 Mar 2007: clustering stuff moved here ---*/

   func->vedit_frame = XtVaCreateWidget(
           "dialog" , xmFrameWidgetClass , func->ulaclu_rowcol ,
              XmNshadowType , XmSHADOW_ETCHED_IN ,
              XmNshadowThickness , 2 ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   func->clu_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->vedit_frame ,
            XmNorientation , XmVERTICAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNspacing     , 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   im3d->vedset.code = 0 ; im3d->vedset.ival = -1 ;
   im3d->vedskip = 0 ;  /* 20 Dec 2007 */

   func->clu_cluster_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , func->clu_rowcol ,
            LABEL_ARG(" Clusterize") ,
            XmNmarginHeight, 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   MCW_set_widget_bg( func->clu_cluster_pb , VEDIT_COLOR , 0 ) ;
   XtAddCallback( func->clu_cluster_pb , XmNactivateCallback ,
                  AFNI_clu_CB , im3d ) ;
   MCW_register_hint( func->clu_cluster_pb , "Set clustering parameters" ) ;
   MCW_register_help( func->clu_cluster_pb ,
                        "Cluster editing parameters:\n"
                        "  rmm = connectivity radius (rmm=0 -> 1 voxel)\n"
                        " vmul = minimum cluster volume (in microliters)\n"
                        "       ** BUT, if rmm=0, vmul is in voxels,\n"
                        "       ** not in absolute volume!\n\n"
                        "N.B.: Clustering is done at the overlay\n"
                        "      dataset voxel resolution, NOT at\n"
                        "      the underlay resolution.\n\n"
                        "N.B.: Clustering cannot be done if the overlay\n"
                        "      dataset does not have a stored volume\n"
                        "      (e.g., is 'warp-on-demand' only)." ) ;

   hrc = XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->clu_rowcol ,
            XmNorientation , XmHORIZONTAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNspacing     , 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->clu_clear_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , hrc ,
            LABEL_ARG("*Clear") ,
            XmNmarginHeight, 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XtAddCallback( func->clu_clear_pb , XmNactivateCallback ,
                  AFNI_clu_CB , im3d ) ;
   MCW_register_hint( func->clu_clear_pb , "Turn off Cluster Edit" ) ;
   MCW_register_help( func->clu_clear_pb , "Disable on-the-fly\n"
                                           "clustering of the\n"
                                           "thresholded overlay\n"
                                           "volume." ) ;
   func->clu_report_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , hrc ,
            LABEL_ARG("Rpt") ,
            XmNmarginHeight, 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XtAddCallback( func->clu_report_pb , XmNactivateCallback ,
                  AFNI_clu_CB , im3d ) ;
   MCW_register_hint( func->clu_report_pb , "Open cluster report window" ) ;
   XtManageChild( hrc ) ;

   /*--- 05 May 2009: InstaCorr stuff ---*/

   func->icor_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->vedit_frame ,
            XmNorientation , XmVERTICAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNspacing     , 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->icor_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , func->icor_rowcol ,
               LABEL_ARG("Setup ICorr") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   MCW_set_widget_bg( func->icor_pb , VEDIT_COLOR , 0 ) ;
   XtAddCallback( func->icor_pb , XmNactivateCallback , AFNI_misc_CB , im3d ) ;
   MCW_register_hint( func->icor_pb , "Control InstaCorr calculations" ) ;

   xstr = XmStringCreateLtoR( "*NOT Ready* " , XmFONTLIST_DEFAULT_TAG ) ;
   func->icor_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , func->icor_rowcol ,
            XmNrecomputeSize , False ,
            XmNlabelString , xstr ,
            XmNalignment , XmALIGNMENT_CENTER ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XmStringFree(xstr) ;
   MCW_set_widget_bg(func->icor_label,STOP_COLOR,0) ;

   im3d->iset = NULL ;

   /*--- 18 Sep 2009: InstaCalc stuff ---*/

   func->icalc_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->vedit_frame ,
            XmNorientation , XmVERTICAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNmarginHeight, 0 ,
            XmNmarginWidth , 0 ,
            XmNspacing     , 0 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->icalc_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , func->icalc_rowcol ,
               LABEL_ARG("Setup ICalc") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   MCW_set_widget_bg( func->icalc_pb , VEDIT_COLOR , 0 ) ;
   XtAddCallback( func->icalc_pb , XmNactivateCallback , AFNI_misc_CB , im3d ) ;
   MCW_register_hint( func->icalc_pb , "Control InstaCalc calculations" ) ;

   xstr = XmStringCreateLtoR( "*NOT Ready* " , XmFONTLIST_DEFAULT_TAG ) ;
   func->icalc_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , func->icalc_rowcol ,
            XmNrecomputeSize , False ,
            XmNlabelString , xstr ,
            XmNalignment , XmALIGNMENT_CENTER ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   XmStringFree(xstr) ;
   MCW_set_widget_bg(func->icalc_label,STOP_COLOR,0) ;

   im3d->icalc_setup = NULL ;

   /*--- 22 Dec 2009: Group InstaCorr stuff ---*/

   if( num_entry == 1 ){  /* only in controller A */

     func->gicor_rowcol =
        XtVaCreateWidget(
           "dialog" , xmRowColumnWidgetClass , func->vedit_frame ,
              XmNorientation , XmVERTICAL ,
              XmNpacking , XmPACK_TIGHT ,
              XmNmarginHeight, 0 ,
              XmNmarginWidth , 0 ,
              XmNspacing     , 0 ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

     func->gicor_pb =
           XtVaCreateManagedWidget(
              "dialog" , xmPushButtonWidgetClass , func->gicor_rowcol ,
                 LABEL_ARG("Setup GICor") ,
                 XmNmarginHeight , 0 ,
                 XmNtraversalOn , True  ,
                 XmNinitialResourcesPersistent , False ,
              NULL ) ;
     MCW_set_widget_bg( func->gicor_pb , VEDIT_COLOR , 0 ) ;
     XtAddCallback( func->gicor_pb , XmNactivateCallback , AFNI_misc_CB , im3d ) ;
     MCW_register_hint( func->gicor_pb , "Control 3dGroupInCorr calculations" ) ;

     xstr = XmStringCreateLtoR( "*NOT Ready* " , XmFONTLIST_DEFAULT_TAG ) ;
     func->gicor_label =
        XtVaCreateManagedWidget(
           "dialog" , xmLabelWidgetClass , func->gicor_rowcol ,
              XmNrecomputeSize , False ,
              XmNlabelString , xstr ,
              XmNalignment , XmALIGNMENT_CENTER ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;
     XmStringFree(xstr) ;
     MCW_set_widget_bg(func->gicor_label,STOP_COLOR,0) ;
     MCW_register_hint( func->gicor_label , "Will be ** Ready ** when 3dGroupInCorr is running" ) ;

   } else {

     func->gicor_rowcol = func->gicor_pb = func->gicor_label = NULL ;

   }

   im3d->giset = NULL ;

   /*--- 30 Nov 1997: bucket managers ---*/

   func->buck_frame =
      XtVaCreateWidget(
         "dialog" , xmFrameWidgetClass , func->options_rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->buck_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->buck_frame ,
            XmNorientation , XmVERTICAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- 30 Nov 1997: anatomy bucket arrowval ---*/
   /*    (Actual labels are set when used)       */

   func->anat_buck_av = new_MCW_arrowval(
                          func->buck_rowcol    ,  /* parent Widget */
                          "ULay" ,                /* label */
                          MCW_AV_optmenu ,        /* option menu style */
                          0 ,                     /* first option */
                          1 ,                     /* last option */
                          0 ,                     /* initial selection */
                          MCW_AV_readtext ,       /* ignored but needed */
                          0 ,                     /* ditto */
                          AFNI_bucket_CB ,        /* callback when changed */
                          (XtPointer)im3d ,       /* data for above */
                          MCW_av_substring_CB ,   /* text creation routine */
                          AFNI_dummy_av_label     /* data for above */
                        ) ;

   func->anat_buck_av->parent     = (XtPointer)im3d ;
   func->anat_buck_av->allow_wrap = True ;

   MCW_reghelp_children( func->anat_buck_av->wrowcol ,
                         "Use this to choose which\n"
                         "sub-brick of the overlay\n"
                         "dataset to display (='ULay').\n"
                         "(The sub-brick labels are\n"
                         " assigned when the dataset\n"
                         " is created.  The [index]\n"
                         " values show the numerical\n"
                         " location of the sub-brick\n"
                         " in the dataset.)"           ) ;
   MCW_reghint_children( func->anat_buck_av->wrowcol ,
                         "Choose UnderLay sub-brick" ) ;

   ADDTO_KILL(im3d->kl,func->anat_buck_av) ;

   XtUnmanageChild( func->anat_buck_av->wrowcol ) ;

   /*--- 30 Nov 1997: function bucket arrowval ---*/

   func->fim_buck_av = new_MCW_arrowval(
                          func->buck_rowcol    ,  /* parent Widget */
                          "OLay" ,                /* label */
                          MCW_AV_optmenu ,        /* option menu style */
                          0 ,                     /* first option */
                          1 ,                     /* last option */
                          0 ,                     /* initial selection */
                          MCW_AV_readtext ,       /* ignored but needed */
                          0 ,                     /* ditto */
                          AFNI_bucket_CB ,        /* callback when changed */
                          (XtPointer)im3d ,       /* data for above */
                          MCW_av_substring_CB ,   /* text creation routine */
                          AFNI_dummy_av_label     /* data for above */
                        ) ;

   func->fim_buck_av->parent     = (XtPointer)im3d ;
   func->fim_buck_av->allow_wrap = True ;

   MCW_reghelp_children( func->fim_buck_av->wrowcol ,
                         "Use this to choose which\n"
                         "sub-brick of the overlay\n"
                         "dataset to display (='OLay').\n"
                         "(The sub-brick labels are\n"
                         " assigned when the dataset\n"
                         " is created.  The [index]\n"
                         " values show the numerical\n"
                         " location of the sub-brick\n"
                         " in the dataset.)"           ) ;
   MCW_reghint_children( func->fim_buck_av->wrowcol ,
                         "Choose overlay sub-brick" ) ;

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
                          (XtPointer)im3d ,       /* data for above */
                          MCW_av_substring_CB ,   /* text creation routine */
                          AFNI_dummy_av_label     /* data for above */
                        ) ;

   func->thr_buck_av->parent     = (XtPointer)im3d ;
   func->thr_buck_av->allow_wrap = True ;

   MCW_reghelp_children( func->thr_buck_av->wrowcol ,
                         "Use this to choose which\n"
                         "sub-brick of the overlay\n"
                         "dataset with which to threshold\n"
                         "the OLay sub-brick (='Thr').\n"
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

   /*--- range controls ---*/

   func->range_frame =
      XtVaCreateManagedWidget(
         "dialog" , xmFrameWidgetClass , func->options_rowcol ,
            XmNshadowType , XmSHADOW_ETCHED_IN ,
            XmNshadowThickness , 2 ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->range_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->range_frame ,
            XmNorientation , XmVERTICAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- label to show the ranges ---*/

   im3d->vinfo->old_range_label = xstr = AFNI_range_label( NULL ) ;

   func->range_label =
      XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , func->range_rowcol ,
            XmNrecomputeSize , False ,
            XmNlabelString , xstr ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;
   MCW_register_help( func->range_label ,
                        "These are the range of values in the\n"
                        "UnderLay and OverLay 3D datasets.\n"
                        "The overlay values may be useful\n"
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
                    AFNI_range_bbox_CB , (XtPointer)im3d ) ;

   func->range_bbox->parent = (XtPointer) im3d ;

   MCW_set_bbox( func->range_bbox ,
                 (im3d->vinfo->use_autorange) ? (1) : (0) ) ;

   MCW_reghelp_children( func->range_bbox->wrowcol ,
                         "This button determines whether the program\n"
                         "or the user sets the OLay value that maps\n"
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
            XmNtraversalOn , True  ,
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
                         "selector is used to set the OLay level which\n"
                         "maps to 1.0 on the color pbar."
                       ) ;
   MCW_reghint_children( func->range_av->wrowcol ,
                         "OLay value that maps to 1.0 for color overlay" ) ;

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
                         "the 'pbar' up or down.\n"
                         "[Press with Shift  to]\n"
                         "[rotate in steps of 4]"  ) ;
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->fim_rowcol =
      XtVaCreateWidget(
         "dialog" , xmRowColumnWidgetClass , func->fim_frame ,
            XmNorientation , XmHORIZONTAL ,
            XmNpacking , XmPACK_TIGHT ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   func->fim_mbar =
         XmCreateMenuBar( func->fim_rowcol, "dialog" , NULL,0 ) ;
   XtVaSetValues( func->fim_mbar ,
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNspacing      , 3 ,
                     XmNborderWidth  , 0 ,
                     XmNtraversalOn  , True  ,
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
            XmNtraversalOn , True  ,
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

   xstr = XmStringCreateLtoR( "ULay = xxxxxxxxxxxxxxxx\n"
                              "OLay = xxxxxxxxxxxxxxxx\n"
                              "Thr  = xxxxxxxxxxxxxxxx" ,
                            XmFONTLIST_DEFAULT_TAG ) ;

   func->bkgd_lab =
      XtVaCreateWidget(
         "dialog" , xmLabelWidgetClass , func->options_rowcol ,
            XmNrecomputeSize , False ,
            XmNlabelString , xstr ,
            XmNmarginHeight, 0 ,
            XmNtraversalOn , True  ,
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

   /*-- manage the managers --*/

   XtManageChild( func->thr_rowcol ) ;
   XtManageChild( func->inten_rowcol ) ;
   XtManageChild( func->range_rowcol ) ;
   XtManageChild( func->clu_rowcol ) ;
   XtManageChild( func->vedit_frame ) ;
   XtManageChild( func->ulaclu_rowcol ) ;
   XtManageChild( func->options_rowcol ) ;
#ifdef USE_FUNC_FIM
   XtManageChild( func->fim_rowcol ) ;
#endif
   XtManageChild( func->rowcol ) ;

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void AFNI_make_wid3( Three_D_View *im3d )
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
            XmNtraversalOn , True  ,
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
     "View ULay Data Brick ==> data from underlay file is displayed\n"
     "                   (will be grayed-out if data is not available)\n"
     "Warp ULay on Demand  ==> data is resampled as needed for display" ) ;

   { char *hh[] = { "View data direct from brick" ,
                     "View data resampled to new grid" } ;
     MCW_bbox_hints( dmode->anatmode_bbox , 2 , hh ) ;
   }

   ADDTO_KILL(im3d->kl,dmode->anatmode_bbox) ;

   /*-- resampling control: anat mode --*/

   dmode->anat_resam_av = new_MCW_arrowval(
                             dmode->rowcol ,
                             "ULay resam mode" ,
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
     "underlay data (for display and writing):\n\n"
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
     "View OLay Data Brick ==> data from overlay file is displayed\n"
     "                   (will be grayed-out if data is not available)\n"
     "Warp OLay on Demand  ==> data is resampled as needed for display\n\n"
     "N.B.: Overlay data is always on top of underlay data.\n"
     "  To be displayed directly from the overlay data brick,\n"
     "  this brick must conform in dimensions to the underlay\n"
     "  data being displayed.  Even if the overlay brick exists,\n"
     "  if its dimensions do not correspond to the underlay brick\n"
     "  or the resampling dimension (below), then the overlay data\n"
     "  being displayed will be 'warped-on-demand'.  Such warping\n"
     "  always occurs from the 'most original' source.  For example,\n"
     "  if a Talairach view brick is altered (via a plugin, or another\n"
     "  external editing program), then viewing the brick may be quite\n"
     "  different from viewing the warped data, which will be recomputed\n"
     "  from the Original view brick (if available), without reference\n"
     "  to whatever alterations may have been made in the Talairach view."
   ) ;

   { char *hh[] = { "View data direct from brick" ,
                     "View data resampled to new grid" } ;
     MCW_bbox_hints( dmode->funcmode_bbox , 2 , hh ) ;
   }

   ADDTO_KILL(im3d->kl,dmode->funcmode_bbox) ;

   /*-- func resampling control (moved here 03 Nov 1996) --*/

   dmode->func_resam_av = new_MCW_arrowval(
                             dmode->rowcol ,
#ifdef USE_OPTMENUS
                             "OLay resam mode" ,
#else
                             "OLay mode " ,
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
     "overlay data (display and writing):\n\n"
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
                             "Stat resam mode" ,
#else
                             "Stat mode " ,
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
     "overlay data (threshold only):\n\n"
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
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   /*-- 23 Nov 1996: label at left --*/

   (void) XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , dmode->write_rowcol ,
            LABEL_ARG("Write ") ,
            XmNalignment , XmALIGNMENT_BEGINNING ,
            XmNrecomputeSize , False ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*--- write pushbuttons ---*/

   dmode->write_anat_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->write_rowcol ,
            LABEL_ARG("ULay") ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( dmode->write_anat_pb , XmNactivateCallback ,
                  AFNI_write_dataset_CB , im3d ) ;

   MCW_register_hint( dmode->write_anat_pb ,
                      "Write current anatomy to disk" ) ;

   dmode->write_func_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->write_rowcol ,
            LABEL_ARG("OLay") ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   XtAddCallback( dmode->write_func_pb , XmNactivateCallback ,
                  AFNI_write_dataset_CB , im3d ) ;

   MCW_register_hint( dmode->write_func_pb ,
                      "Write current overlay dataset to disk" ) ;

   dmode->write_many_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->write_rowcol ,
            LABEL_ARG("Many") ,
            XmNtraversalOn , True  ,
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
        "ULay --> current overlay dataset brick.\n"
        "OLay --> current underlay dataset brick.\n"
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
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   /*-- 23 Nov 1996: label at left --*/

   (void) XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , dmode->rescan_rowcol ,
            LABEL_ARG("Rescan") ,
            XmNalignment , XmALIGNMENT_BEGINNING ,
            XmNrecomputeSize , False ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*-- pushbutton for one session rescan --*/

   dmode->rescan_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->rescan_rowcol ,
            LABEL_ARG("This") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   /*-- label at left --*/

   (void) XtVaCreateManagedWidget(
         "dialog" , xmLabelWidgetClass , dmode->read_rowcol ,
            LABEL_ARG("Read  ") ,
            XmNalignment , XmALIGNMENT_BEGINNING ,
            XmNrecomputeSize , False ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   /*-- pushbutton for session input --*/

   dmode->read_sess_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , dmode->read_rowcol ,
            LABEL_ARG("Sess") ,
            XmNmarginHeight , 0 ,
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
              XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      if( MAX_CONTROLLERS > 1 ){
         prog->clone_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->rc_top ,
                 LABEL_ARG("New  ") ,
                 XmNtraversalOn , True  ,
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
              XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

   prog->button_help_pb =
      XtVaCreateManagedWidget(
         "dialog" , xmPushButtonWidgetClass , prog->rc_bot ,
           LABEL_ARG("BHelp") ,
           XmNtraversalOn , True  ,
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
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
            XmNuserData , (XtPointer) im3d ,
         NULL ) ;

   MCW_set_widget_bg( prog->quit_pb , MCW_hotcolor(prog->quit_pb) , 0 ) ;

   XtAddCallback( prog->quit_pb , XmNactivateCallback ,
                  AFNI_quit_CB , im3d ) ;
   /* check for -disable_done                21 Aug 2008 [rickr] */
   XtSetSensitive( prog->quit_pb, GLOBAL_argopt.disable_done == 0 ) ;

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

   imag->pop_bkgd_lab =
      XtVaCreateWidget(
         "dialog" , xmLabelWidgetClass , imag->popmenu ,
            LABEL_ARG("   bkgd =xxxxxx") ,
            XmNalignment , XmALIGNMENT_BEGINNING ,
            XmNrecomputeSize , True ,
            XmNtraversalOn , True  ,
            XmNinitialResourcesPersistent , False ,
         NULL ) ;

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
                 XmNwidth            , logo_width ,
                 XmNheight           , logo_height ,
                 XmNmarginWidth      , 0 ,
                 XmNmarginHeight     , 0 ,
                 XmNrecomputeSize    , False ,
                 XmNtraversalOn      , True  ,
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

#ifdef BAD_BUTTON3_POPUPS   /* 21 Jul 2003 */
     prog->hidden_menu =
        XmCreatePopupMenu( prog->quit_pb , "menu" , NULL , 0 ) ;
#else
     prog->hidden_menu =
        XmCreatePopupMenu( vwid->picture , "menu" , NULL , 0 ) ;
#endif

     if( prog->hidden_menu != NULL ){
      SAVEUNDERIZE(XtParent(prog->hidden_menu)) ; /* 27 Feb 2001 */

      VISIBILIZE_WHEN_MAPPED(prog->hidden_menu) ;
      if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(prog->hidden_menu) ;

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
                             | KeyPressMask      /* get keystrokes */
                            ,
                            FALSE ,              /* nonmaskable events? */
                            AFNI_hidden_EV ,     /* handler */
                            (XtPointer) im3d ,   /* client data */
                            XtListTail           /* last in queue */
                          ) ;

#ifdef ALLOW_DATASET_VLIST
      /**--- pullright menu for points ---**/

      prog->hidden_pts_menu =
         XmCreatePulldownMenu( prog->hidden_menu , "menu" , NULL , 0 ) ;

      VISIBILIZE_WHEN_MAPPED(prog->hidden_pts_menu) ;
      if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(prog->hidden_pts_menu) ;

      /** cascade button to bring this menu up **/

      prog->hidden_pts_cbut =
         XtVaCreateManagedWidget(
            "dialog" , xmCascadeButtonWidgetClass , prog->hidden_menu ,
               LABEL_ARG("Points List") ,
               XmNsubMenuId , prog->hidden_pts_menu ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      /** "Read Pts: IJK" button in pts menu **/

      prog->hidden_readpts_ijk_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , prog->hidden_pts_menu ,
               LABEL_ARG("Read Pts:  IJK") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

      XtAddCallback( prog->hidden_colorpts_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;

      /*---- END OF PTS STUFF ----*/
#endif

      /*---- Various Poetry Options ----*/

      xstr = XmStringCreateLtoR( "---- Poetry ----" , XmFONTLIST_DEFAULT_TAG ) ;
      (void) XtVaCreateManagedWidget(
               "dialog" , xmLabelWidgetClass , prog->hidden_menu ,
                  XmNlabelString , xstr ,
                  XmNrecomputeSize , False ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XmStringFree(xstr) ;

      (void) XtVaCreateManagedWidget(
               "dialog" , xmSeparatorWidgetClass , prog->hidden_menu ,
                  XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

      /*----------*/

      prog->hidden_mission_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->hidden_menu ,
                  LABEL_ARG("Mission Statement") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( prog->hidden_mission_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;

      /*----------*/

#ifdef USE_SONNETS
      if( ! NO_frivolities ){
         prog->hidden_sonnet_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->hidden_menu ,
                  LABEL_ARG("Shakespeare") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;

         XtAddCallback( prog->hidden_sonnet_pb , XmNactivateCallback ,
                        AFNI_sonnet_CB , im3d ) ;
      }
#endif

      /*----------*/

      prog->hidden_gamberi_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->hidden_menu ,
                  LABEL_ARG("Gamberi Cattivi") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( prog->hidden_gamberi_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;

      /*----------*/

      (void) XtVaCreateManagedWidget(
               "dialog" , xmSeparatorWidgetClass , prog->hidden_menu ,
                  XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

      prog->hidden_ranpoem_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->hidden_menu ,
                  LABEL_ARG("Random Poem") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( prog->hidden_ranpoem_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;

      /*----------*/

#if !defined(NO_FRIVOLITIES)
      prog->hidden_faces_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->hidden_menu ,
                  LABEL_ARG("All AFNI Faces") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( prog->hidden_faces_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;
#else
      prog->hidden_faces_pb = NULL ;
#endif

      /*----------*/

#if !defined(NO_FRIVOLITIES)
      prog->hidden_splashes_pb =           /* 12 Sep 2007 */
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->hidden_menu ,
                  LABEL_ARG("All AFNI Splashes") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( prog->hidden_splashes_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;
#else
      prog->hidden_splashes_pb = NULL ;
#endif

      /*----------*/

      prog->hidden_broutim_pb =            /* 06 Jun 2005 */
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->hidden_menu ,
                  LABEL_ARG("Brodmann Areas: imag") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( prog->hidden_broutim_pb , XmNactivateCallback ,
                     AFNI_broutim_CB , im3d ) ;

      /*----------*/

      prog->hidden_broutext_pb =            /* 21 Dec 2005 */
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->hidden_menu ,
                  LABEL_ARG("Brodmann Areas: text") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( prog->hidden_broutext_pb , XmNactivateCallback ,
                     AFNI_broutext_CB , im3d ) ;

      /*----------*/

      (void) XtVaCreateManagedWidget(
               "dialog" , xmSeparatorWidgetClass , prog->hidden_menu ,
                  XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

#if 0
      prog->hidden_speech_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , prog->hidden_menu ,
                  LABEL_ARG("Say Something") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( prog->hidden_speech_pb , XmNactivateCallback ,
                     AFNI_hidden_CB , im3d ) ;
#else
      prog->hidden_speech_pb = NULL ;
#endif

      /*---------- modified 30 Dec 2005 ----------*/

      GLOBAL_browser = getenv("AFNI_WEB_BROWSER") ;
#ifdef DARWIN
      if( GLOBAL_browser == NULL )
        GLOBAL_browser = strdup("open") ;  /* for Mac OS X */
#endif
      if( GLOBAL_browser == NULL )
        GLOBAL_browser = THD_find_executable( "firefox" ) ;
      if( GLOBAL_browser == NULL )
        GLOBAL_browser = THD_find_executable( "mozilla" ) ;
      if( GLOBAL_browser == NULL )
        GLOBAL_browser = THD_find_executable( "netscape" ) ;
      if( GLOBAL_browser == NULL )
        GLOBAL_browser = THD_find_executable( "opera" ) ;

      if( GLOBAL_browser != NULL ){
        prog->hidden_browser_pb =
              XtVaCreateManagedWidget(
                 "dialog" , xmPushButtonWidgetClass , prog->hidden_menu ,
                    LABEL_ARG("Web Browser") ,
                    XmNmarginHeight , 0 ,
                    XmNtraversalOn , True  ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
        XtAddCallback( prog->hidden_browser_pb , XmNactivateCallback ,
                       AFNI_hidden_CB , im3d ) ;
      } else {
        prog->hidden_browser_pb = NULL ;
      }

    } /* if prog->hidden_menu isn't NULL */
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

/*-------------------------------------------------------------------*/
/*! Find first open controller [05 Mar 2002].
---------------------------------------------------------------------*/

Three_D_View * AFNI_find_open_controller(void)
{
   int ii ;
   for( ii=0 ; ii < MAX_CONTROLLERS ; ii++ )
     if( IM3D_OPEN(GLOBAL_library.controllers[ii]) )
       return GLOBAL_library.controllers[ii] ;

   return NULL ;  /* should be impossible */
}

/*-------------------------------------------------------------------*/
/*! Popup a message, somewhere, anywhere [05 Mar 2002].
---------------------------------------------------------------------*/

void AFNI_popup_message( char *str )
{
   Three_D_View *im3d ;
   if( str == NULL || str[0] == '\0' ) return ;
   im3d = AFNI_find_open_controller() ;
   (void) MCW_popup_message( im3d->vwid->prog->clone_pb ,
                             str, MCW_USER_KILL|MCW_TIMER_KILL ) ;
   return ;
}

/*-------------------------------------------------------------------
  Find out which controller this is.  Return -1 if there is an error.
---------------------------------------------------------------------*/

int AFNI_controller_index( Three_D_View *im3d )
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
      viewing images only (mostly obsolete now -- use program aiv instead).
-----------------------------------------------------------------------------*/

Three_D_View * new_AFNI_controller( Widget shell , MCW_DC *dc , int im3d_type )
{
   Three_D_View *im3d ;
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

   /* 27 Jan 2004: mark if this currently looking at dummy dataset */

   im3d->dummied = GLOBAL_library.have_dummy_dataset ;

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
              XmNkeyboardFocusPolicy , XmEXPLICIT ,
                                 NULL ) ;
   }

   DC_yokify( im3d->vwid->top_shell , dc ) ;

   XtVaSetValues( im3d->vwid->top_shell ,
                     XmNtitle    , im3d->window_title ,
                     XmNallowShellResize , True ,       /* let code resize shell */
                     XmNdeleteResponse , XmDO_NOTHING , /* deletion handled below */
                  NULL ) ;

   /* make "Close" window menu work if user has not blocked the action */
   if( GLOBAL_argopt.disable_done == 0 )
      XmAddWMProtocolCallback(
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
   im3d->vinfo->time_on           = 0 ;                   /* 29 Jul 2003 */
   im3d->vinfo->view_type         = VIEW_ORIGINAL_TYPE ;  /* show +orig data */
   im3d->vinfo->underlay_type     = UNDERLAY_ANAT ;       /* show anatomy */
   im3d->vinfo->force_anat_wod    = False ;   /* don't force warp-on-demand */
   im3d->vinfo->force_func_wod    = False ;   /* don't force warp-on-demand */
   im3d->vinfo->func_visible      = (Boolean)AFNI_yesenv("AFNI_SEE_OVERLAY") ;
   im3d->vinfo->func_visible_count=0 ;
#ifdef ALLOW_DATASET_VLIST
   im3d->vinfo->pts_visible       = False ;   /* don't show points */
   im3d->vinfo->pts_color         = 0 ;
#endif
   im3d->vinfo->show_voxind       = False ;
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

   im3d->vinfo->stats_anat_ok =
    im3d->vinfo->stats_func_ok =
     im3d->vinfo->stats_thresh_ok = 0 ; /* 29 Mar 2005 */

   /* Feb 1998: receive stuff, including drawing */
   /* Mar 1999: modified to allow for multiple receivers */

   im3d->vinfo->receiver          = AFMALL( AFNI_receiver*, sizeof(AFNI_receiver *));
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

   im3d->ignore_seq_callbacks = AFNI_IGNORE_NOTHING ;   /* don't ignore these now */

   /* set up which datasets to deal with (defaults) */

   im3d->vinfo->sess_num = 0 ;  /* 1st session */
   im3d->vinfo->anat_num = 0 ;  /* 1st dataset */
   im3d->vinfo->func_num = 0 ;  /* 1st dataset */

   AFNI_make_widgets( im3d ) ;  /* rest of the widgets */

   /* 02 Nov 1996: create daxes and voxwarp spaces for viewing */

   im3d->wod_daxes          = myXtNew(THD_dataxes) ;
   im3d->wod_daxes->type    = DATAXES_TYPE ;

   im3d->anat_voxwarp       = myXtNew(THD_warp) ;
   im3d->anat_voxwarp->type = ILLEGAL_TYPE ;

   im3d->fim_voxwarp        = myXtNew(THD_warp) ;
   im3d->fim_voxwarp->type  = ILLEGAL_TYPE ;

   im3d->fim_selfwarp       = NULL ;  /* 27 Aug 2002 */

   RETURN(im3d) ;
}

/*---------------------------------------------------------------*/

static float DSET_bigositiness( THD_3dim_dataset *dset ) /* 07 Dec 2001 */
{
   float bb ;

   if( !DSET_ONDISK(dset) ) return -1 ;

   bb =  DSET_NVOX(dset)
       * DSET_NVALS(dset)
       * mri_datum_size(DSET_BRICK_TYPE(dset,0)) ;

   if( DSET_COMPRESSED(dset) || DSET_IS_MINC(dset) ) bb *= 1.666f ;

   return bb ;
}

/*---------------------------------------------------------------
   Set up the controller with some data to view!
-----------------------------------------------------------------*/

void AFNI_initialize_controller( Three_D_View *im3d )
{
   int ii , sss=0 ;
   char ttl[16] , *eee ;

ENTRY("AFNI_initialize_controller") ;

   /*--- check for various criminal behavior;
         the sentence for anything illegal is death ---*/

   if( ! IM3D_VALID(im3d) )
     ERROR_exit("\n** AFNI_initialize_controller: invalid input **\n") ;

   if( GLOBAL_library.sslist == NULL )  /* any data to use? */
      ERROR_exit(
              "\n** AFNI_initialize_controller: no sessions to view **\n") ;

   im3d->ss_now = GLOBAL_library.sslist->ssar[ im3d->vinfo->sess_num ] ;

   if( im3d->ss_now == NULL ){     /* bad choice of initial session! */

      im3d->vinfo->sess_num = 0 ;  /* reset 1st session */
      im3d->vinfo->anat_num = 0 ;  /* reset 1st anatomy */
      im3d->vinfo->func_num = 0 ;  /* reset 1st function */

      im3d->ss_now = GLOBAL_library.sslist->ssar[ im3d->vinfo->sess_num ] ;

      if( im3d->ss_now == NULL )  /* still no data? */
        ERROR_exit(
                "\n** AFNI_initialize_controller: illegal initial session **\n") ;
   }

   /* 07 Dec 2001: find "smallest" datasets for startup */

   eee = my_getenv("AFNI_START_SMALL") ;
   if( eee != NULL ){
     char ccc = toupper(eee[0]) ;
     sss = (ccc == 'Y') ? 1
          :(ccc == 'O') ? 2 : 0 ;
   }
   if( im3d->brand_new && sss ){
     int jj,jb=0,qb=0 ; float bb,mm ;
     switch( sss ){
       case 2:       /* the OLD way */
         for( mm=1.e+33,jb=jj=0 ; jj < im3d->ss_now->num_dsset ; jj++ ){
           if( ISANAT(im3d->ss_now->dsset[jj][0]) ){
             bb = DSET_bigositiness(im3d->ss_now->dsset[jj][0]) ;
             if( bb > 0 && bb < mm ){ mm = bb; jb = jj; }
           }
         }
         if( mm < 1.e+33 ) im3d->vinfo->anat_num = qb = jb ;
         for( mm=1.e+33,jb=jj=0 ; jj < im3d->ss_now->num_dsset ; jj++ ){
           if( ISFUNC(im3d->ss_now->dsset[jj][0]) ){
             bb = DSET_bigositiness(im3d->ss_now->dsset[jj][0]) ;
             if( jj != qb && bb > 0 && bb < mm ){ mm = bb; jb = jj; }
           }
         }
         if( mm < 1.e+33 ) im3d->vinfo->func_num = jb ;
       break ;

       case 1:
         for( mm=1.e+33,jb=jj=0 ; jj < im3d->ss_now->num_dsset ; jj++ ){
           if( ISVALID_DSET(im3d->ss_now->dsset[jj][0]) ){
             bb = DSET_bigositiness(im3d->ss_now->dsset[jj][0]) ;
             if( bb > 0.0f && bb < mm ){ mm = bb; jb = jj; }
           }
         }
         if( mm < 1.e+33 ) im3d->vinfo->func_num = im3d->vinfo->anat_num = jb ;
       break ;
     }

   } else if( im3d->brand_new ){  /* 29 Jul 2003 */
     int jj ;
     for( jj=0 ; jj < im3d->ss_now->num_dsset ; jj++ )
       if( ISANAT(im3d->ss_now->dsset[jj][0]) ) break ;
     if( jj < im3d->ss_now->num_dsset ) im3d->vinfo->anat_num = jj ;

     for( jj=0 ; jj < im3d->ss_now->num_dsset ; jj++ )
       if( ISFUNC(im3d->ss_now->dsset[jj][0]) ) break ;
     if( jj < im3d->ss_now->num_dsset ) im3d->vinfo->func_num = jj ;
   }

   /* copy pointers from this session into the controller for fast access */

   for( ii=0 ; ii <= LAST_VIEW_TYPE ; ii++ )
      im3d->anat_dset[ii] = im3d->ss_now->dsset[im3d->vinfo->anat_num][ii] ;

   /*--- 11/23/94 addition: scan for a good view in the
        initial setup (to allow for input of datasets w/o +orig view) */

   for( ii=0 ; ii <= LAST_VIEW_TYPE ; ii++ )
      if( ISVALID_DSET(im3d->anat_dset[ii]) ) break ;

   if( ii > LAST_VIEW_TYPE )
      ERROR_exit("\n   AFNI_initialize_controller: cannot initialize view **\n") ;

   im3d->vinfo->view_type = ii ;  /* first view with a good anat */

   /* now find a function that can be viewed here */

   if( !ISVALID_DSET(im3d->ss_now->dsset[im3d->vinfo->func_num][ii]) ){
     for( ii=0 ; ii < im3d->ss_now->num_dsset ; ii++ ){
       if(ISVALID_DSET(im3d->ss_now->dsset[ii][im3d->vinfo->view_type])){
          im3d->vinfo->func_num = ii ; break ;
       }
     }
   }

   for( ii=0 ; ii <= LAST_VIEW_TYPE ; ii++ )
      im3d->fim_dset[ii] = im3d->ss_now->dsset[im3d->vinfo->func_num][ii] ;

   /*--- 11/23/94 addition end */

   im3d->anat_now = im3d->anat_dset[im3d->vinfo->view_type] ;  /* will not be NULL */
   im3d->fim_now  = im3d->fim_dset [im3d->vinfo->view_type] ;  /* this may be NULL */
   if( !ISVALID_DSET(im3d->fim_now) ) AFNI_SEE_FUNC_OFF(im3d) ;     /* 22 May 2009 */

   /* initial point of view = middle of dataset brick */

   im3d->vinfo->i1 = im3d->anat_now->daxes->nxx / 2 ;  /* integer indexes */
   im3d->vinfo->j2 = im3d->anat_now->daxes->nyy / 2 ;
   im3d->vinfo->k3 = im3d->anat_now->daxes->nzz / 2 ;
   SAVE_VPT(im3d) ;

   im3d->vinfo->i1_icor = im3d->vinfo->j2_icor = im3d->vinfo->k3_icor = -1 ;

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

   WAIT_for_window( im3d->vwid->top_shell ) ;

#if 1
   POPUP_cursorize( im3d->vwid->func->thr_label ) ;       /* 05 Sep 2006 */
#endif
#if 0
   POPUP_cursorize( im3d->vwid->func->thr_pval_label ) ;  /* 05 Sep 2006 */
#endif
   POPUP_cursorize( im3d->vwid->func->inten_label ) ;
   POPUP_cursorize( im3d->vwid->picture ) ;
   POPUP_cursorize( imag->crosshair_label ) ;
   POPUP_cursorize( im3d->vwid->func->thr_label ) ;

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
   Three_D_View *im3d ;
   Three_D_View *caller_im3d = (Three_D_View *) cd ;
   MCW_DC *new_dc , *old_dc ;

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

   AFNI_vedit_CB( im3d->vwid->func->options_vedit_av , im3d ) ;  /* 05 May 2009 */

   PICTURE_OFF(im3d) ; SHOW_AFNI_READY ; EXRETURN ;
}

/*-----------------------------------------------------------------------
   Called to determine if the "New" button should be active
-------------------------------------------------------------------------*/

void AFNI_controller_clonify(void)
{
   Three_D_View *im3d ;
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

void AFNI_lock_button( Three_D_View *im3d )
{
   Widget rc , mbar , menu , cbut , wpar ;
   XmString xstr ;

   static char *clabel[] = {
      "Lock [A]", "Lock [B]", "Lock [C]", "Lock [D]", "Lock [E]",
      "Lock [F]", "Lock [G]", "Lock [H]", "Lock [I]", "Lock [J]",
      "Lock [K]", "Lock [L]", "Lock [M]", "Lock [N]", "Lock [O]",
      "Lock [P]", "Lock [Q]", "Lock [R]", "Lock [S]", "Lock [T]",
      "Lock [U]", "Lock [V]", "Lock [W]", "Lock [X]", "Lock [Y]", "Lock [Z]" } ;

   static char *tlabel[] = { "Time Lock" } ;

   static char *ijklabel[] = { "IJK lock" } ;

ENTRY("AFNI_lock_button") ;

   wpar = im3d->vwid->dmode->mbar_rowcol ;

   rc =  XtVaCreateWidget(
           "dialog" , xmRowColumnWidgetClass , wpar ,
              XmNorientation , XmHORIZONTAL ,
              XmNpacking , XmPACK_TIGHT ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   mbar = XmCreateMenuBar( rc , "dialog" , NULL,0 ) ;
   XtVaSetValues( mbar ,
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNspacing      , 3 ,
                     XmNborderWidth  , 0 ,
                     XmNborderColor  , 0 ,
                     XmNtraversalOn  , True  ,
                     XmNbackground   , im3d->dc->ovc->pixov_brightest ,
                  NULL ) ;
   XtManageChild( mbar ) ;

   menu = XmCreatePulldownMenu( mbar , "menu" , NULL,0 ) ;

   VISIBILIZE_WHEN_MAPPED(menu) ;
   if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(menu) ;

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
               XmNtraversalOn  , True  ,
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

   /* This --- Cancel --- label does not cause the hangup, so it is
   left alone. See related comments in afni_graph.c
                           LessTif patrol, Jan 07 09 */
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
               XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
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

void AFNI_misc_button( Three_D_View *im3d )
{
   Widget rc , mbar , menu , cbut , wpar ;
   XmString xstr ;

ENTRY("AFNI_misc_button") ;

   wpar = im3d->vwid->dmode->mbar_rowcol ;

   rc =  XtVaCreateWidget(
           "dialog" , xmRowColumnWidgetClass , wpar ,
              XmNorientation , XmHORIZONTAL ,
              XmNpacking , XmPACK_TIGHT ,
              XmNtraversalOn , True  ,
              XmNinitialResourcesPersistent , False ,
           NULL ) ;

   mbar = XmCreateMenuBar( rc , "dialog" , NULL,0 ) ;
   XtVaSetValues( mbar ,
                     XmNmarginWidth  , 0 ,
                     XmNmarginHeight , 0 ,
                     XmNspacing      , 3 ,
                     XmNborderWidth  , 0 ,
                     XmNborderColor  , 0 ,
                     XmNtraversalOn  , True  ,
                     XmNbackground   , im3d->dc->ovc->pixov_brightest ,
                  NULL ) ;
   XtManageChild( mbar ) ;

   menu = XmCreatePulldownMenu( mbar , "menu" , NULL,0 ) ;

   VISIBILIZE_WHEN_MAPPED(menu) ;
   if( !AFNI_yesenv("AFNI_DISABLE_TEAROFF") ) TEAROFFIZE(menu) ;

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
               XmNtraversalOn  , True  ,
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
                             " ULay Info         = Show 3dinfo output\n"
                             " OLay Info         = for current datasets\n"
#ifdef ALLOW_PLUGINS
                             " Edit Environment  = Control environment vars\n"
                             " Edit 2DChain      = Control 2DChain function\n"
#endif
                             " Save Layout       = Save windows layout/setup\n"
                             " Run Script        = Run an AFNI script file\n"
                             " License Info      = GPL & Copyright notice\n"
                             " Version Check     = Check AFNI version\n"
                             " Message of the Day= Fetch current AFNI MotD\n"
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

   /* This --- Cancel --- label does not cause the hangup, so it is
   left alone. See related comments in afni_graph.c
                           LessTif patrol, Jan 07 09 */
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

   { char *blab[1] = { "Write=Own Size?" } ;
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

   { char *blab[1] = { "Voxel Coords?" } ;
     dmode->misc_voxind_bbox = new_MCW_bbox( menu ,
                                             1 , blab ,
                                             MCW_BB_check , MCW_BB_noframe ,
                                             AFNI_misc_CB , (XtPointer)im3d ) ;
     dmode->misc_voxind_pb = dmode->misc_voxind_bbox->wbut[0] ;
   }
   MCW_register_hint( dmode->misc_voxind_pb , "Toggle coordinate display" ) ;

    /*-- pushbutton to turn hints on and off --*/

#ifndef DONT_USE_HINTS
   { char *hh = getenv("AFNI_HINTS") ;
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
            { char *blab[1] = { "Show Hints?" } ;
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
               LABEL_ARG("ULay Info") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_anat_info_pb , XmNactivateCallback ,
                  AFNI_misc_CB , im3d ) ;
   MCW_register_hint( dmode->misc_anat_info_pb , "Popup anat dataset info" ) ;

   dmode->misc_func_info_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("OLay Info") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
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
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_savelayout_pb , XmNactivateCallback ,
                  AFNI_save_layout_CB , im3d ) ;
   MCW_register_hint( dmode->misc_savelayout_pb , "Save layout to file (or .script)" ) ;

   /*--- 22 Jan 2003: Run Script [see afni_splash.c] ---*/

   dmode->misc_runscript_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("Run Script") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_runscript_pb , XmNactivateCallback ,
                  AFNI_run_script_CB , im3d ) ;
   MCW_register_hint( dmode->misc_runscript_pb , "Run an AFNI script file" ) ;

   /*--- 07 Nov 2001: start plugouts [see afni_plugout.c] ---*/

#ifdef ALLOW_PLUGINS
   if( !AFNI_have_plugouts() ){
      dmode->misc_plugout_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , menu ,
                  LABEL_ARG("Start Plugouts") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
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

   /*-- 02 Mar 2002: button to start NIML --*/

   if( !AFNI_have_niml() ){
     dmode->misc_niml_pb =
           XtVaCreateManagedWidget(
              "dialog" , xmPushButtonWidgetClass , menu ,
                 LABEL_ARG("Start NIML") ,
                 XmNmarginHeight , 0 ,
                 XmNtraversalOn , True  ,
                 XmNinitialResourcesPersistent , False ,
              NULL ) ;
     XtAddCallback( dmode->misc_niml_pb , XmNactivateCallback ,
                    AFNI_misc_CB , im3d ) ;
     MCW_register_hint( dmode->misc_niml_pb ,
                        "Start listening for NIML connections" ) ;
   } else {
     dmode->misc_niml_pb = NULL ;
   }

   /*--- Utility buttons ---*/

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , menu ,
               XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

   dmode->misc_readme_env_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("README.environment") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_readme_env_pb , XmNactivateCallback ,
                  AFNI_misc_CB , im3d ) ;
   MCW_register_hint( dmode->misc_readme_env_pb,"Display README.environment file" );

   dmode->misc_license_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("License Info") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_license_pb , XmNactivateCallback ,
                  AFNI_misc_CB , im3d ) ;
   MCW_register_hint( dmode->misc_license_pb,"Display GPL & Copyright Notice" );

   if( !ALLOW_realtime ){    /* 01 May 2000: only if not doing realtime */
      dmode->misc_vcheck_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , menu ,
                  LABEL_ARG("Version Check") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( dmode->misc_vcheck_pb , XmNactivateCallback ,
                     AFNI_misc_CB , im3d ) ;
      MCW_register_hint( dmode->misc_vcheck_pb,"Compare to master distribution" );
   } else {
      dmode->misc_vcheck_pb = NULL ;
   }

   if( !ALLOW_realtime ){    /* 29 Nov 2005: message of the day */
      dmode->misc_motd_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , menu ,
                  LABEL_ARG("Message of the Day") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( dmode->misc_motd_pb , XmNactivateCallback ,
                     AFNI_misc_CB , im3d ) ;
      MCW_register_hint( dmode->misc_motd_pb,"Display latest AFNI MotD" );
   } else {
      dmode->misc_motd_pb = NULL ;
   }

   /*--- pushbutton to see AFNI's historical documents ---*/

   { char *pg = THD_find_executable("afni_history") ;
     dmode->misc_hist_pb = NULL ;
     if( pg != NULL ){
       dmode->misc_hist_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , menu ,
                  LABEL_ARG("AFNI History") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( dmode->misc_hist_pb , XmNactivateCallback ,
                     AFNI_misc_CB , im3d ) ;
      MCW_register_hint( dmode->misc_hist_pb,"Show the Historical Documents" );
     }
   }

   /*--- pushbutton to purge unused datasets ---*/

   (void) XtVaCreateManagedWidget(
            "dialog" , xmSeparatorWidgetClass , menu ,
               XmNseparatorType , XmSINGLE_LINE ,
            NULL ) ;

   dmode->misc_purge_pb =
         XtVaCreateManagedWidget(
            "dialog" , xmPushButtonWidgetClass , menu ,
               LABEL_ARG("Purge Memory") ,
               XmNmarginHeight , 0 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;
   XtAddCallback( dmode->misc_purge_pb , XmNactivateCallback ,
                  AFNI_misc_CB , im3d ) ;
   MCW_register_hint( dmode->misc_purge_pb , "Purge unused datasets" ) ;

   /*--- pushbutton to toggle routine tracing ---*/

#ifdef USE_TRACING
   if( !ALLOW_realtime ){    /* 26 Jan 2001: don't do this if realtime is on */
     dmode->misc_tracing_pb =
           XtVaCreateManagedWidget(
              "dialog" , xmPushButtonWidgetClass , menu ,
                 LABEL_ARG( DBG_label ) ,
                 XmNmarginHeight , 0 ,
                 XmNtraversalOn , True  ,
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
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;
      XtAddCallback( dmode->misc_showmalloc_pb , XmNactivateCallback ,
                     AFNI_misc_CB , im3d ) ;

      dmode->misc_dumpmalloc_pb =
            XtVaCreateManagedWidget(
               "dialog" , xmPushButtonWidgetClass , menu ,
                  LABEL_ARG("Dump Malloc Table") ,
                  XmNmarginHeight , 0 ,
                  XmNtraversalOn , True  ,
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

/*------------------------------------------------------------------*/
/*! Doesn't do much (for testing new buttons, mostly). */

void AFNI_invert_CB( Widget wcall , XtPointer cd , XtPointer cbs )
{
   if( wcall != (Widget)NULL ) MCW_invert_widget(wcall) ;
}

/*------------------------------------------------------------------*/
/*! Callback for NIML+PO button. */

void AFNI_nimlpo_CB( Widget wcall , XtPointer cd , XtPointer cbs )
{
   Three_D_View *im3d ; int id ;

   AFNI_init_niml() ;
   AFNI_init_plugouts() ;

   for( id=0 ; id < MAX_CONTROLLERS ; id++ ){     /* 01 Feb 2008: */
     im3d = GLOBAL_library.controllers[id] ;    /* disable in all */
     if( IM3D_VALID(im3d) )                        /* controllers */
       XtSetSensitive( im3d->vwid->view->nimlpo_pb , False ) ;
   }

   return ;
}

/*------------------------------------------------------------------*/
/*! Callback for on-the-fly editing controls arrowval. */

void AFNI_vedit_CB( MCW_arrowval *av , XtPointer cd )
{
   Three_D_View *im3d = (Three_D_View *)cd ;

ENTRY("AFNI_vedit_CB") ;

   if( ! IM3D_OPEN(im3d) ) EXRETURN ;

   if( ! im3d->vwid->imag->pop_instacorr_pb ||
       ! im3d->vwid->imag->pop_icorrjump_pb ) {/*This happens when running afni
                                                 with -im option ZSS Aug 31 09*/
      EXRETURN ;
   }

   XtUnmanageChild( im3d->vwid->func->vedit_frame ) ;

   switch( av->ival ){
     /* switch to Clusters */
     case 0:
       DISABLE_INSTACALC(im3d) ;                          /* InstaCalc off */
       DISABLE_INSTACORR(im3d) ;                          /* InstaCorr off */
       DESTROY_ICOR_setup(im3d->iset) ;
       DISABLE_GRPINCORR(im3d) ;                          /* GrpInCorr off */
       XtUnmanageChild( im3d->vwid->func->icalc_rowcol) ;
       XtUnmanageChild( im3d->vwid->func->icor_rowcol ) ;
       if( im3d->vwid->func->gicor_rowcol != NULL )
         XtUnmanageChild( im3d->vwid->func->gicor_rowcol ) ;
       XtManageChild  ( im3d->vwid->func->clu_rowcol  ) ;
     break ;

     /* switch to InstaCorr */
     case 1:
       UNCLUSTERIZE(im3d) ;                               /* Clusters off */
       DISABLE_INSTACALC(im3d) ;                          /* InstaCalc off */
       DISABLE_GRPINCORR(im3d) ;                          /* GrpInCorr off */
       XtUnmanageChild( im3d->vwid->func->icalc_rowcol) ;
       XtUnmanageChild( im3d->vwid->func->clu_rowcol  ) ;
       if( im3d->vwid->func->gicor_rowcol != NULL )
         XtUnmanageChild( im3d->vwid->func->gicor_rowcol ) ;
       XtManageChild  ( im3d->vwid->func->icor_rowcol ) ;
     break ;

     /* switch to InstaCalc [18 Sep 2009] */
     case 2:
       UNCLUSTERIZE(im3d) ;                               /* Clusters off */
       DISABLE_INSTACORR(im3d) ;                          /* InstaCorr off */
       DESTROY_ICOR_setup(im3d->iset) ;
       DISABLE_GRPINCORR(im3d) ;                          /* GrpInCorr off */
       XtUnmanageChild( im3d->vwid->func->clu_rowcol  ) ;
       XtUnmanageChild( im3d->vwid->func->icor_rowcol ) ;
       if( im3d->vwid->func->gicor_rowcol != NULL )
         XtUnmanageChild( im3d->vwid->func->gicor_rowcol ) ;
       XtManageChild  ( im3d->vwid->func->icalc_rowcol) ;
     break ;

     /* switch to Group InstaCorr [22 Dec 2009] */
     case 3:
       if( im3d->vwid->func->gicor_rowcol != NULL ){
         DISABLE_INSTACALC(im3d) ;                          /* InstaCalc off */
         DISABLE_INSTACORR(im3d) ;                          /* InstaCorr off */
         DESTROY_ICOR_setup(im3d->iset) ;
         UNCLUSTERIZE(im3d) ;                               /* Clusters off */
         XtUnmanageChild( im3d->vwid->func->clu_rowcol  ) ;
         XtUnmanageChild( im3d->vwid->func->icor_rowcol ) ;
         XtUnmanageChild( im3d->vwid->func->icalc_rowcol) ;
         XtManageChild  ( im3d->vwid->func->gicor_rowcol) ;
         if( im3d->giset != NULL ) ENABLE_GRPINCORR(im3d) ;
         else                      DISABLE_GRPINCORR(im3d) ;
       }
     break ;
   }
   XtManageChild( im3d->vwid->func->vedit_frame ) ;

   FIX_SCALE_SIZE(im3d) ; FIX_SCALE_VALUE(im3d) ; EXRETURN ;
}


/* 
   Set parameters for ROI colormaps
   ZSS Feb 15 2010 
*/
int AFNI_set_func_range_nval(XtPointer *vp_im3d, float rval)
{
   Three_D_View *im3d=NULL;
   
   ENTRY("AFNI_set_func_range_val") ;

   im3d = (Three_D_View *)vp_im3d;
   MCW_set_bbox( im3d->vwid->func->range_bbox , 0 ) ;   /* autoRange box off */
   im3d->vinfo->use_autorange = 0 ;

   AV_SENSITIZE( im3d->vwid->func->range_av , 1 ) ;
   AV_assign_fval( im3d->vwid->func->range_av , rval ) ;
   AFNI_range_av_CB( im3d->vwid->func->range_av , im3d ) ;
   
   /* positive only */
   MCW_set_bbox( im3d->vwid->func->inten_bbox , 1 ) ;
   AFNI_inten_bbox_CB( im3d->vwid->func->inten_bbox->wbut[PBAR_MODEBUT] ,
                       (XtPointer)im3d , NULL ) ;

   
   RETURN(0) ;
}

int AFNI_set_dset_pbar(XtPointer *vp_im3d)
{
   Three_D_View *im3d=NULL;
   MCW_pbar *pbar = NULL;
   ATR_string *atr=NULL;
   char *pbar_name=NULL;
   byte switched = 0;
   int icmap=-1;
   NI_element *nel=NULL;
   
   ENTRY("AFNI_set_func_range_val") ;

   if (!AFNI_yesenv("AFNI_CMAP_AUTO")) RETURN(0);
   
   im3d = (Three_D_View *)vp_im3d;

   atr = THD_find_string_atr( im3d->fim_now->dblk , 
                              "VALUE_LABEL_DTABLE" ) ;

   if (atr) {
      /* switch to an ROI colormap */
      if (!(nel = NI_read_element_fromstring(atr->ch))) {
         fprintf(stderr,"** WARNING: Poorly formatted VALUE_LABEL_DTABLE\n"); 
         icmap = -1;
      } else {
         pbar_name = NI_get_attribute(nel,"pbar_name");
         icmap = PBAR_get_bigmap_index(pbar_name);
      }
      if (icmap >=0 ) {
         PBAR_set_bigmap( im3d->vwid->func->inten_pbar ,  pbar_name) ;
      } else {
         PBAR_set_bigmap( im3d->vwid->func->inten_pbar , "ROI_i256" ) ;
      }
      switched = 1;
      /* Problem is that when you switch back to a non  ROI dset, 
      you are stuck with the ROI deal. 
      Perhaps one should bite the bullet and create a structure
      that preserves the last colormap setup for a dset.
      Hmmm, got to discuss this with Bob, Daniel, and Rick */
   } else { 
      /* 
      Here one could guess at the moment (See is_integral_dset). 
      But that can be 
      time consuming if integer data are stored as float.
      It is better to have the dataset flagged by a special type
      in the header. 
      */
   }
   
   if (switched) {
      AFNI_inten_pbar_CB( im3d->vwid->func->inten_pbar , im3d , 0 ) ;
      POPUP_cursorize(im3d->vwid->func->inten_pbar->panew ) ;  
   }
   RETURN(0);
}

/*
   Put the label associate with value val in string str 
      (64 chars are copied into str)
*/
int AFNI_get_dset_val_label(THD_3dim_dataset *dset, double val, char *str)
{
   MCW_pbar *pbar = NULL;
   ATR_string *atr=NULL;
   char *pbar_name=NULL;
   byte switched = 0;
   int icmap=-1;
   char *str_lab=NULL, sval[128]={""};
   NI_element *nel=NULL;
   
   ENTRY("AFNI_get_dset_val_label") ;
   
   if (!str) RETURN(1);
   
   str[0]='\0';
      
   if (!dset) RETURN(1);

   if (!dset->Label_Dtable && 
       (atr = THD_find_string_atr( dset->dblk , 
                              "VALUE_LABEL_DTABLE" ))) {
      dset->Label_Dtable = Dtable_from_nimlstring(atr->ch);
   }
   
   if (dset->Label_Dtable) {
      /* Have hash, will travel */
      sprintf(sval,"%d", (int)val);
      str_lab = findin_Dtable_a(sval, 
                                dset->Label_Dtable);
      /* fprintf(stderr,"ZSS: Have label '%s' for value '%s'\n",
                     str_lab ? str_lab:"NULL", sval); */
      if (str_lab) snprintf(str,64, "(%s)",str_lab); 
   }

   RETURN(0);
}
