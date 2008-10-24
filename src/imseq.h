/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _MCW_IMSEQ_HEADER_
#define _MCW_IMSEQ_HEADER_

#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/cursorfont.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>
#include <Xm/DrawingA.h>
#include <Xm/Protocols.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/Label.h>
#include <Xm/AtomMgr.h>

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <signal.h>

#include "mrilib.h"
#include "vecmat.h"

#include "display.h"
#include "bbox.h"
#include "xutil.h"
#include "machdep.h"

#include "coxplot.h"  /* 30 Dec 1998 */

#ifdef  __cplusplus
extern "C" {
#endif

/*** typedefs ***/

#ifndef HAVE_GET_PTR_TYPEDEF
#  define HAVE_GET_PTR_TYPEDEF
   typedef XtPointer (*get_ptr)() ;  /* function type */
#endif

typedef struct {
      int num_total , num_series ;  /* # of images, # in "series" */

      void (* send_CB)() ;   /* callback, if non_NULL */

      MCW_function_list *transforms0D ;
      MCW_function_list *transforms2D ;
      MCW_function_list *slice_proj   ;  /* 31 Jan 2002 */

      XtPointer parent , aux ;
} MCW_imseq_status ;

#define ISQ_DOING_SLICE_PROJ(ss)      \
 ( (ss)->slice_proj_range >  0   &&   \
   (ss)->slice_proj_func  != NULL   )

#define IM_WIDTH(im) \
  ( ((im)->dx > 0) ? ((im)->nx * (im)->dx) : ((im)->nx) )

#define IM_HEIGHT(im) \
  ( ((im)->dy > 0) ? ((im)->ny * (im)->dy) : ((im)->ny) )

/* define dimensions used in the window */

#define FORM_FRAC_BASE  1000
#define IMAGE_FRAC      0.8
#define DFRAC           0.01
#define FRAC_MAX        0.95
#define FRAC_MIN        0.25

#define COLOR_BAR_WIDTH 16
#define COLOR_BAR_SPACE  4

/* button stuff */

#define NBUTTON_BOT 4   /* # buttons at bottom */
#define NBUTTON_RIG 3   /* # buttons at right  */

#define LEADING_BOT        XmNleftAttachment
#define LEADING_WIDGET_BOT XmNleftWidget
#define TRAILING_BOT       XmNrightAttachment
#define EDGING_BOT         XmNbottomAttachment
#define SPACING_BOT        XmNleftOffset

#define LEADING_RIG        XmNtopAttachment
#define LEADING_WIDGET_RIG XmNtopWidget
#define TRAILING_RIG       XmNbottomAttachment
#define EDGING_RIG         XmNrightAttachment
#define SPACING_RIG        XmNtopOffset

#define NARROW 5

/* image destruction stuff */

#define KILL_2XIM(one,two)                          \
   do { if( (two) != (one) ) MCW_kill_XImage(two) ; \
        MCW_kill_XImage(one) ; (one) = (two) = NULL ; } while(0)

#define KILL_2ndXIM(one,two) \
   do { if( (two) != (one) ) MCW_kill_XImage(two) ; \
        (two) = NULL ; } while(0)

#define KILL_1MRI(one) \
   do{ if( (one) != NULL ){ mri_free(one) ; (one) = NULL ; }} while(0)

#define ISQ_VALID(seq) ((seq)!=NULL && ((seq)->valid)>0)
#define ISQ_REALZ(seq) ((seq)!=NULL && ((seq)->valid)>1)

/*----------------------- button box stuff -----------------------*/

typedef struct {
      int nbut ;
      char ** lbut ;
      int type , frame ;

      XtPointer parent , aux ;
} ISQ_boxdef ;

#define NBOX_DISP  9           /* # button boxes in Disp dialog */

#define NBOX_MAX   NBOX_DISP     /* max # boxes to allow for in imseq */

#define ISQ_ROT_0   1   /* values returned by various button boxes */
#define ISQ_ROT_90  2   /* in the Disp dialog (will be transferred */
#define ISQ_ROT_180 4   /* to values in the options data)          */
#define ISQ_ROT_270 8

#define ISQ_TO_MRI_ROT(x) (x)   /* convert ISQ_ROT codes to MRI_ROT codes */

#define ISQ_SCL_AUTO 1
#define ISQ_SCL_GRP  2
#define ISQ_SCL_USER 65536

#define ISQ_RNG_MINTOMAX  1
#define ISQ_RNG_02TO98    2
#define ISQ_RNG_CLIPPED   4

#define ISQ_ASPECT 1

#define ISQ_SAV_NSIZE 1
#define ISQ_SAV_PNM   2
#define ISQ_SAV_ONE   4      /* 26 Jul 2001: no longer used */

#define ISQ_IMPROC_NONE   0  /* values returned by various button   */
#define ISQ_IMPROC_FLAT   1
#define ISQ_IMPROC_SHARP  2  /* boxes in the Disp dialog improc box */
#define ISQ_IMPROC_SOBEL  4  /* (powers of 2 of button indexes)     */

#define ISQ_CX_MAG        1  /* values returned by buttons in */
#define ISQ_CX_PHASE      2  /* the Disp dialog "complex" box */
#define ISQ_CX_REAL       4
#define ISQ_CX_IMAG       8

typedef struct {
      int mirror , rot , no_overlay ,
          scale_group , scale_range , free_aspect ,
          save_nsize , save_pnm , save_one , improc_code , cx_code ;

      XtPointer parent , aux ;

      int save_filter ;  /* 27 Jun 2001 */
      int save_agif   ;  /* 27 Jul 2001 */
      int save_mpeg   ;  /* 02 Jul 2001 */
} ISQ_options ;

#undef  AGIF_MODE
#undef  MPEG_MODE
#undef  JPEG_MODE
#undef  PNG_MODE
#undef  RAW_MODE
#undef  RAWMONT_MODE

#define AGIF_MODE     1  /* 06 Dec 2006 - for ISQ_save_anim() */
#define MPEG_MODE     2
#define JPEG_MODE     3
#define PNG_MODE      4
#define RAW_MODE      5
#define RAWMONT_MODE  6

#define ISQ_OPT_EQUAL(opta,optb)                    \
    ( ((opta).mirror      == (optb).mirror     ) && \
      ((opta).rot         == (optb).rot        ) && \
      ((opta).no_overlay  == (optb).no_overlay ) && \
      ((opta).scale_group == (optb).scale_group) && \
      ((opta).scale_range == (optb).scale_range) && \
      ((opta).free_aspect == (optb).free_aspect) && \
      ((opta).improc_code == (optb).improc_code) && \
      ((opta).cx_code     == (optb).cx_code    )     )

/* 09 Oct 1998 */

#define ISQ_DEFAULT_OPT(opt) do{ (opt).mirror      = FALSE ;            \
                                 (opt).rot         = ISQ_ROT_0 ;        \
                                 (opt).no_overlay  = False ;            \
                                 (opt).scale_group = ISQ_SCL_AUTO ;     \
                                 (opt).scale_range = ISQ_RNG_02TO98 ;   \
                                 (opt).free_aspect = False ;            \
                                 (opt).save_nsize  = False ;            \
                                 (opt).save_pnm    = False ;            \
                                 (opt).save_one    = True ;             \
                                 (opt).improc_code = ISQ_IMPROC_NONE ;  \
                                 (opt).cx_code     = ISQ_CX_MAG ;       \
                                 (opt).parent      = NULL ;             \
                                 (opt).aux         = NULL ;             \
                                 (opt).save_filter = -1   ;             \
                                 (opt).save_agif   = 0    ;             \
                                 (opt).save_mpeg   = 0    ;             \
                               } while(0)

/*------------- statistics for image display scaling -------------*/

#define NHISTOG 500

typedef struct {
      Boolean one_done , glob_done ;
      float   min,max , per02,per98 ,
              scl_mm,lev_mm , scl_per,lev_per ;
      float   entropy ;

      XtPointer parent , aux ;
} ISQ_indiv_statistics ;

typedef struct {
      Boolean mm_done , per_done ;
      float   min,max , per02,per98 ,
              scl_mm,lev_mm , scl_per,lev_per ;
      int hist[NHISTOG] ;
      XtWorkProcId worker ;

      XtPointer parent , aux ;
} ISQ_glob_statistics ;

#define ISQ_SCLEV(mn,mx,dp,sc,lv) \
  ( (lv) = (mn) , (sc) = (((mx)>(mn)) ? (((dp)-0.49)/((mx)-(mn))) : 1.0) )

/*--- "callback" data stuff: info about events in image window ---*/

typedef struct {
      int          reason ;       /* isqCR_??? defined below */
      XEvent *     event ;        /* may be NULL */
      int          xim,yim ;      /* original image coords, */
                                  /* OR new image size */

      int          key ;          /* keyvalue */
      int          nim ;          /* new image number */
      ISQ_options  opt ;          /* new options */
      XtPointer    userdata ;     /* misc stuff */

      XtPointer parent , aux ;
} ISQ_cbs ;

#define isqCR_buttonpress 1   /* button press in image */
#define isqCR_keypress    2   /* key press in image */
#define isqCR_geometry    3   /* the display geometry altered */
#define isqCR_newimage    4   /* moved to a new image */
#define isqCR_newmontage  5   /* a new image montage layout */
#define isqCR_destroy     99  /* the MCW_imseq was destroyed */

#define isqCR_getimage    401
#define isqCR_getoverlay  402
#define isqCR_getstatus   403
#define isqCR_getqimage   404
#define isqCR_getopacim   405  /* 26 Sep 2007 */
#define isqcR_getulayim   406  /* 24 Oct 2008 */
#define isqcR_getolayim   407  /* 24 Oct 2008 */

#define isqCR_getxynim    411  /* 30 Dec 1998 */

#define isqCR_getmemplot  421  /* 21 Feb 2001 */
#define isqCR_getlabel    422  /* 19 Sep 2001 */

#define isqCR_dxplus      301  /* arrowpad reasons */
#define isqCR_dxminus     302
#define isqCR_dyplus      303
#define isqCR_dyminus     304
#define isqCR_appress     309

#define isqCR_button2_points 501  /* Feb 1998 */
#define isqCR_button2_key    502  /* 20 Feb 2003 */

#define isqCR_force_redisplay 601 /* 22 Aug 1998 */
#define isqCR_setindex        602 /* 26 Apr 2007 */

#define COLORMAP_CHANGE(sq)                                          \
  do{ if( ISQ_REALZ((sq)) && (sq)->dc->visual_class == TrueColor ){  \
         if( (sq)->status->send_CB != NULL ){                        \
            ISQ_cbs cbs ;                                            \
            cbs.reason = isqCR_force_redisplay ;                     \
            AFNI_CALL_VOID_3ARG( (sq)->status->send_CB      ,        \
                                 MCW_imseq * , (sq)         ,        \
                                 XtPointer   , (sq)->getaux ,        \
                                 ISQ_cbs *   , &cbs          ) ;     \
         } else {                                                    \
            KILL_2XIM( (sq)->given_xbar , (sq)->sized_xbar ) ;       \
            ISQ_redisplay( (sq) , -1 , isqDR_display ) ;             \
         }                                                           \
    } } while(0)

/*------------------------------*/

#ifndef MONT_NMAX
#define MONT_NMAX 13
#endif

#ifndef MONT_SMAX
#define MONT_SMAX 199
#endif

#ifndef MONT_GMAX
#define MONT_GMAX 13
#endif

extern void ISQ_montage_CB( Widget , XtPointer , XtPointer ) ;
extern void ISQ_montage_action_CB( Widget , XtPointer , XtPointer ) ;

/*------------- the central data type -------------*/

#define ISQ_NHELP   2047
#define ISQ_NWIDGET 128

struct MCW_imseq ;  /* incomplete definition, completed below: */

typedef struct MCW_imseq {

     int valid ;             /* flag if this structure is valid:
                                  0 => no good at all
                                  1 => valid but unrealized
                                  2 => valid and realized        */

     int ignore_redraws ;    /* flag to ignore external redraws */

     int horig , vorig ;     /* horz and vert dimens, sans aspect */

     int hbase , vbase ;     /* horz and vert "standard" image dimensions */
     int hactual , vactual ; /* dimensions, allowing for rotations */
     int old_hact, old_vact; /* recent values */
     float scl , lev ;       /* for scaling intensities */
     int   bot , top ;       /* for clipping intensities */

     Boolean done_first ;    /* for the done button */

     MCW_DC *dc ;            /* graphics data (copy of a pointer) */

     MCW_imseq_status *status ;    /* status of image sequence */

     float last_width_mm , last_height_mm ;  /* physical sizes (in mm, say)*/

     ISQ_options opt, old_opt ;     /* image display options */

     Widget wtop, wform, wimage, wbar, wscale , winfo ,
            wbut_bot[NBUTTON_BOT] , wbut_rig[NBUTTON_RIG] ; /* windows */

     Widget wbar_menu , wbar_rng_but , wbar_zer_but  , wbar_flat_but ,
            wbar_sharp_but ;
     float  rng_bot,rng_top,rng_ztop , flat_bot,flat_top , sharp_fac ;
     int    zer_color , rng_extern ;

     MCW_arrowval *arrow[NARROW] ; /* arrow controls */

     MCW_arrowpad *arrowpad ;      /* arrowpad in lower right corner */

     int marg_bot , marg_rig , wf_wide , wf_high ;

     int       num_bbox ;
     MCW_bbox *bbox[NBOX_MAX] ;  /* button boxes */
     Widget    dialog ;
     int       dialog_starter ;
     ISQ_options save_opt ;

     get_ptr   getim ;  /* pointer to image retrieval procedure */
     XtPointer getaux ; /* pointer to image retrieval auxiliary data */

     int  im_nr ;         /* index of latest and greatest */
     char im_label[64] ;
     char im_helptext[ISQ_NHELP+1] ;

     int    onoff_num , onoff_state ;
     Widget onoff_widgets[ISQ_NWIDGET] ;  /* widgets on & off */

     int last_image_type ;

     int mont_nx    , mont_ny    , mont_skip    , mont_gap    , mont_gapcolor    ;
     int mont_nx_old, mont_ny_old, mont_skip_old, mont_gap_old, mont_gapcolor_old;
     int mont_periodic ;
     MCW_arrowval *mont_across_av , *mont_down_av , *mont_skip_av ,
                  *mont_gap_av , *mont_gapcolor_av ;

     float image_frac ;  /* 25 Oct 1996 */

     MCW_arrowval *transform0D_av ;      /* 30 Oct 1996 */
     generic_func *transform0D_func ;
     int           transform0D_index ;

     MCW_arrowval *transform2D_av ;
     generic_func *transform2D_func ;
     int           transform2D_index ;

     MCW_arrowval *slice_proj_av ;       /* 31 Jan 2002 */
     float_func   *slice_proj_func ;
     int           slice_proj_index ;
     MCW_arrowval *slice_proj_range_av ;
     int           slice_proj_range ;

     MCW_arrowval      *rowgraph_av  ;   /* 30 Dec 1998 */
     int                rowgraph_num ;
     MEM_topshell_data *rowgraph_mtd ;

     MCW_arrowval      *surfgraph_av ;   /* 21 Jan 1999 */
     int                surfgraph_num ;
     MEM_topshell_data *surfgraph_mtd ;
     MCW_arrowpad      *surfgraph_arrowpad ;
     float              surfgraph_theta , surfgraph_phi ;

     int never_drawn ;

     int    button2_enabled , button2_active , button2_drawmode ;
     Pixel  button2_pixel ;
     int    wimage_width , wimage_height ;

     /*--- data below here should be freed before deletion ---*/

     MRI_IMAGE *imim , *ovim ;  /* latest and greatest (already processed) */

     int        need_orim , set_orim ; /* flag to compute orim */
     MRI_IMAGE *orim ;                 /* input underlay image (for rowgraphs) */

     XImage *given_xim  , *sized_xim  ;  /* for actual displaying */
     XImage *given_xbar , *sized_xbar ;

     ISQ_indiv_statistics *imstat ;
     ISQ_glob_statistics  *glstat ;

     /*--- temporary, I hope [Hah!] ---*/

     int saver_from , saver_to ;
     char *saver_prefix ;

     /*--- the obligatory ---*/

     XtPointer parent ;

     /* extra text for winfo [07 Aug 1999] */

     char winfo_extra[64] ;

     /* text for sides of window in default orientation [01 Dec 1999] */

     char winfo_sides[4][16] ;
     char winfo_prefix[16] ;    /* 10 Dec 2007 */

     /* opacity of overlay */

     float ov_opacity ;              /* 07 Mar 2001 */
     MCW_arrowval *ov_opacity_av ;
     Widget ov_opacity_sep ;         /* 08 Mar 2001 */

     Widget record_rc , record_cbut; /* 24 Apr 2001: recording stuff */
     MCW_bbox *record_status_bbox ;
     MCW_bbox *record_method_bbox ;
     int record_status ;
     int record_method ;
     int record_mode ;
     struct MCW_imseq *record_imseq ;
     MRI_IMARR        *record_imarr ;
     MEM_plotdata    **record_mplot ;  /* 05 Jan 2005 */

     MCW_bbox *save_one_bbox ;      /* 26 Jul 2001 */
     MCW_bbox *save_agif_bbox ;     /* 27 Jul 2001 */

     float clbot,cltop , barbot,bartop ; /* 29 Jul 2001 */

     MEM_plotdata *mplot ;              /* 19 Sep 2001 */
     MCW_bbox *wbar_plots_bbox ;        /* 20 Sep 2001 */
     MCW_arrowval *wbar_label_av ;      /* 20 Sep 2001 */
     MCW_arrowval *wbar_labsz_av ;      /* 21 Sep 2001 */

     Widget        zoom_sep              /* 11 Mar 2002 */;
     MCW_arrowval *zoom_val_av ;
     Widget        zoom_drag_pb ;
     int    zoom_fac ;
     float  zoom_hor_off, zoom_ver_off ;
     int    zoom_pw , zoom_ph ;
     Pixmap zoom_pixmap  ;
     XImage *zoom_xim  ;
     int    zoom_button1 , zoom_xp,zoom_yp ; /* 15 Mar 2002 */

     int cropit , crop_xa,crop_xb , crop_ya,crop_yb ; /* 11 Jun 2002 */
     int crop_nxorg , crop_nyorg , crop_allowed ;
     Widget        crop_drag_pb ;                     /* 17 Jun 2002 */
     int           crop_drag ;

     int button2_width ;                              /* 08 Oct 2002 */

     int cursor_state ;                               /* 10 Mar 2003 */

     MCW_bbox *pen_bbox ;                             /* 18 Jul 2003 */

     int last_bx,last_by ;                            /* 23 Oct 2003 */
     int cmap_changed ;

     int do_graymap ;                                 /* 24 Oct 2003 */
     MEM_topshell_data *graymap_mtd ;
     Widget wbar_graymap_pb ;

     XtIntervalId timer_id ;                          /* 03 Dec 2003 */
     int          timer_func, timer_param, timer_delay ;

     int dont_place_dialog ;                          /* 23 Jan 2004 */

     MCW_arrowval *wbar_ticnum_av, *wbar_ticsiz_av ;  /* 23 Feb 2004 */

     float last_dx , last_dy ;                        /* 08 Jun 2004 */

     float rgb_gamma ;                                /* 25 Apr 2005 */
     float rgb_offset ;

     char scl_label[16] ;                             /* 02 Nov 2005 */

     float top_clip ;                                 /* 14 Sep 2007 */
     int   redo_clip ;                                /* 17 Sep 2007 */
} MCW_imseq ;

#define ISQ_TIMERFUNC_INDEX  701
#define ISQ_TIMERFUNC_BOUNCE 702

extern void ISQ_timer_CB( XtPointer , XtIntervalId * ) ; /* 03 Dec 2003 */
extern void ISQ_timer_stop( MCW_imseq * ) ;

extern void ISQ_zoom_av_CB( MCW_arrowval *, XtPointer ) ;
extern void ISQ_zoom_pb_CB( Widget, XtPointer, XtPointer ) ;
extern void ISQ_crop_pb_CB( Widget, XtPointer, XtPointer ) ;
extern void ISQ_actually_pan( MCW_imseq * , int , int ) ;
extern int ISQ_show_zoom( MCW_imseq *seq )  ;

#define CURSOR_NORMAL    0                            /* 10 Mar 2003 */
#define CURSOR_PENCIL    1
#define CURSOR_CROSSHAIR 2                            /* 18 Jul 2003 */

extern void ISQ_set_cursor_state( MCW_imseq * , int ) ;

extern void ISQ_pen_bbox_CB( Widget, XtPointer, XtPointer ) ; /* 18 Jul 2003 */

/*--------------------------------------------------------------------*/

#define ISQ_LABEL_OFF  0  /* 20 Sep 2001 */
#define ISQ_LABEL_UPLF 1
#define ISQ_LABEL_UPRT 2
#define ISQ_LABEL_DNLF 3
#define ISQ_LABEL_DNRT 4
#define ISQ_LABEL_UPMD 5
#define ISQ_LABEL_DNMD 6

/*--------------------------------------------------------------------*/

#define ISQ_USE_SIDES(isq) ( (isq)->winfo_sides[0][0] != '\0' || \
                             (isq)->winfo_sides[1][0] != '\0' || \
                             (isq)->winfo_sides[2][0] != '\0' || \
                             (isq)->winfo_sides[3][0] != '\0'      )

/***---------- prototypes: user callable ----------***/

extern MCW_imseq * open_MCW_imseq( MCW_DC * , get_ptr , XtPointer ) ;

/* Drive Reasons for the next routine */

#define isqDR_imhelptext    101
#define isqDR_options       102
#define isqDR_numtotal      103
#define isqDR_cursor        104
#define isqDR_unrealize     105
#define isqDR_realize       106
#define isqDR_display       107
#define isqDR_overlay       108
#define isqDR_arrowpadon    109
#define isqDR_reimage       110
#define isqDR_reshow        111
#define isqDR_newseq        112
#define isqDR_arrowpadoff   113
#define isqDR_title         114
#define isqDR_clearstat     115
#define isqDR_onoffwid      116
#define isqDR_getimnr       117
#define isqDR_icon          118
#define isqDR_sendmontage   119
#define isqDR_periodicmont  120
#define isqDR_setmontage    121
#define isqDR_setifrac      130
#define isqDR_setrange      131
#define isqDR_bgicon        132
#define isqDR_settopclip    133  /* 14 Sep 2007 */

#define isqDR_arrowpadhint  201
#define isqDR_winfotext     202
#define isqDR_getoptions    203
#define isqDR_winfosides    204
#define isqDR_winfoprefix   205

#define isqDR_destroy       666

#define isqDR_offwid          0
#define isqDR_onwid           1
#define isqDR_togwid          2

#define isqDR_button2_enable  501
#define isqDR_button2_disable 502
#define isqDR_button2_pixel   503
#define isqDR_button2_mode    504
#define isqDR_button2_width   505

#define isqDR_ignore_redraws  521
#define isqDR_keypress        522  /* 18 Feb 2005 */

#define BUTTON2_OPENPOLY        0
#define BUTTON2_CLOSEDPOLY      1
#define BUTTON2_POINTS          2
#define BUTTON2_NODRAW          3

#define isqDR_rebar           602  /* 23 Aug 1998 */
#define isqDR_opacitybut      603  /* 07 Mar 2001 */
#define isqDR_record_mode     604  /* 24 Apr 2001 */
#define isqDR_record_disable  605  /* 24 Apr 2001 */
#define isqDR_zoombut         606  /* 11 Mar 2002 */
#define isqDR_getopacity      607  /* 21 Jan 2003 */
#define isqDR_setopacity      608  /* 21 Jan 2003 */
#define isqDR_setimsave       609  /* 23 Jan 2003 */
#define isqDR_penbbox         610  /* 18 Jul 2003 */
#define isqDR_get_crop        611  /* 03 May 2007 */
#define isqDR_set_crop        612  /* 03 May 2007 */

#define isqDR_plot_label      701  /* 20 Sep 2001 */
#define isqDR_plot_plot       702  /* 20 Sep 2001 */
#define isqDR_save_jpeg       703  /* 28 Jul 2005 */
#define isqDR_save_agif       704  /* 07 Dec 2006 */
#define isqDR_save_mpeg       705  /* 07 Dec 2006 */
#define isqDR_save_jpegall    706  /* 07 Dec 2006 */
#define isqDR_save_png        707  /* 11 Dec 2006 */
#define isqDR_save_filtered   708  /* 14 Dec 2006 */
#define isqDR_save_pngall     709  /* 15 Dec 2006 */
#define isqDR_save_raw        710  /* 13 Nov 2007 */
#define isqDR_save_rawmont    711  /* 13 Nov 2007 */

extern Boolean drive_MCW_imseq( MCW_imseq * , int , XtPointer ) ;

extern Boolean ISQ_setup_new( MCW_imseq * , XtPointer ) ;

/*** prototypes: internals ***/

extern void ISQ_redisplay( MCW_imseq * , int , int ) ;

extern MCW_imseq_status * ISQ_copy_status( MCW_imseq_status * ) ;

extern void ISQ_reset_dimen( MCW_imseq * , float , float ) ;

extern void ISQ_scale_CB( Widget , XtPointer , XtPointer ) ;

extern void ISQ_wbar_plots_CB( Widget , XtPointer , XtPointer ) ; /* 20 Sep 2001 */
extern void ISQ_wbar_label_CB( MCW_arrowval * , XtPointer ) ;

extern void ISQ_wbar_menu_CB( Widget , XtPointer , XtPointer ) ;
extern void ISQ_set_rng_CB( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void ISQ_set_zcol_CB( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void ISQ_set_flat_CB( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void ISQ_set_sharp_CB( Widget , XtPointer , MCW_choose_cbs * ) ;

extern void ISQ_but_disp_CB( Widget , XtPointer , XtPointer ) ;
extern void ISQ_but_save_CB( Widget , XtPointer , XtPointer ) ;
extern void ISQ_but_done_CB( Widget , XtPointer , XtPointer ) ;
extern void ISQ_but_color_CB( Widget , XtPointer , XtPointer ) ;
extern void ISQ_but_cswap_CB( Widget , XtPointer , XtPointer ) ;
extern void ISQ_but_cnorm_CB( Widget , XtPointer , XtPointer ) ;

extern void ISQ_place_dialog( MCW_imseq * ) ;  /* 05 Jan 1999 */
extern void ISQ_place_widget( Widget, Widget ) ;  /* 27 Oct 2003 */

#undef REQUIRE_TWO_DONES
#ifdef REQUIRE_TWO_DONES
   extern void ISQ_but_done_reset( MCW_imseq * ) ;
#else
#  define ISQ_but_done_reset(xx)  /* nada */
#endif

extern void ISQ_disp_act_CB( Widget , XtPointer , XtPointer ) ;

extern void ISQ_drawing_EV( Widget , XtPointer , XEvent * , Boolean * ) ;
extern void ISQ_button2_EV( Widget , XtPointer , XEvent * , Boolean * ) ;

extern void ISQ_make_image( MCW_imseq * ) ;
extern void ISQ_show_image( MCW_imseq * ) ;
extern void ISQ_draw_winfo( MCW_imseq * ) ;

 /* 06 Mar 2001 */
extern MRI_IMAGE * ISQ_overlay( MCW_DC *, MRI_IMAGE *, MRI_IMAGE *, float ) ;
#define ISQ_GOOD_OVERLAY_TYPE(dt) ( (dt)==MRI_short || (dt)==MRI_rgb )

 /* 07 Mar 2001 */
extern void ISQ_opacity_CB( MCW_arrowval * , XtPointer ) ;
extern char * ISQ_opacity_label( int ) ;
extern MRI_IMAGE * ISQ_index_to_rgb( MCW_DC * , int , MRI_IMAGE * ) ;
#define ISQ_SKIP_OVERLAY(isq) ((isq)->opt.no_overlay || (isq)->ov_opacity == 0.0)

extern MRI_IMAGE * ISQ_manufacture_one( int nim , int overlay , MCW_imseq * seq ) ;
extern void ISQ_make_montage( MCW_imseq * ) ;

extern void ISQ_make_bar( MCW_imseq * ) ;
extern void ISQ_show_bar( MCW_imseq * ) ;
extern void ISQ_set_barhint( MCW_imseq * , char * ) ; /* 29 Jul 2001 */

extern MRI_IMAGE * ISQ_process_mri( int , MCW_imseq * , MRI_IMAGE * ) ;

extern MRI_IMAGE    * ISQ_getimage  ( int , MCW_imseq * ) ; /* 31 Jan 2002 */
extern MRI_IMAGE    * ISQ_getoverlay( int , MCW_imseq * ) ; /* 11 Jun 2002 */
extern MEM_plotdata * ISQ_getmemplot( int , MCW_imseq * ) ;
extern char         * ISQ_getlabel  ( int , MCW_imseq * ) ;

extern void ISQ_free_alldata( MCW_imseq * ) ;

extern int ISQ_set_image_number( MCW_imseq * , int ) ;

extern Boolean ISQ_disp_options( MCW_imseq * , Boolean ) ;

/* type for defining buttons */

typedef struct {
   char           * name ;
   XtCallbackProc   func_CB ;

   XtPointer parent , aux ;
} ISQ_bdef ;

Boolean ISQ_statistics_WP( XtPointer ) ;

void ISQ_statify_one( MCW_imseq * , int , MRI_IMAGE * ) ;
void ISQ_statify_all( MCW_imseq * , Boolean ) ;

void ISQ_perpoints( float,float , int h[] , float * , float * ) ;

void ISQ_mapxy   ( MCW_imseq * , int,int , int *,int *,int * ) ;
void ISQ_flipxy  ( MCW_imseq * , int *,int * ) ;
void ISQ_unflipxy( MCW_imseq * , int *,int * ) ;

void ISQ_arrow_CB( MCW_arrowval * , XtPointer ) ;

void ISQ_arrowpad_CB( MCW_arrowpad * , XtPointer ) ;

extern void ISQ_transform_CB     ( MCW_arrowval * , XtPointer ) ;
extern char * ISQ_transform_label( MCW_arrowval * , XtPointer ) ;

extern void ISQ_slice_proj_CB    ( MCW_arrowval * , XtPointer ) ;

#define ROWGRAPH_MAX  9
#define SURFGRAPH_MAX 2

#define ROWGRAPH_MASK  1
#define SURFGRAPH_MASK 2
#define GRAYMAP_MASK   4  /* 24 Oct 2003 */

extern void ISQ_rowgraph_CB     ( MCW_arrowval * , XtPointer ) ;
extern char * ISQ_rowgraph_label( MCW_arrowval * , XtPointer ) ;
extern void ISQ_rowgraph_draw( MCW_imseq * seq ) ;
extern void ISQ_rowgraph_mtdkill( MEM_topshell_data * mp ) ;

extern void ISQ_graymap_draw( MCW_imseq * seq ) ;           /* 24 Oct 2003 */
extern void ISQ_graymap_mtdkill( MEM_topshell_data * mp ) ;

extern void ISQ_surfgraph_CB     ( MCW_arrowval * , XtPointer ) ;
extern char * ISQ_surfgraph_label( MCW_arrowval * , XtPointer ) ;
extern void ISQ_surfgraph_draw( MCW_imseq * seq ) ;
extern void ISQ_surfgraph_mtdkill( MEM_topshell_data * mp ) ;
extern MEM_plotdata * plot_image_surface( MRI_IMAGE * , float,float,float,int,int ) ;
extern void ISQ_surfgraph_arrowpad_CB( MCW_arrowpad * , XtPointer ) ;

/*---- temporary, I hope (yeah, sure, right, uh huh) ----*/

extern void ISQ_saver_CB( Widget , XtPointer , MCW_choose_cbs * ) ;

extern MEM_plotdata * ISQ_plot_label( MCW_imseq *, char * ) ; /* 20 Sep 2001 */

/*---- 24 Apr 2001: recording stuff ----*/

#define RECORD_STATUS_OFF         (1<<0)
#define RECORD_STATUS_NEXTONE     (1<<1)
#define RECORD_STATUS_ON          (1<<2)

#define RECORD_ISON(ib) ((ib) > RECORD_STATUS_OFF)

#define RECORD_METHOD_AFTEREND     (1<<0)
#define RECORD_METHOD_BEFORESTART  (1<<1)
#define RECORD_METHOD_INSERT_MM    (1<<2)
#define RECORD_METHOD_INSERT_PP    (1<<3)
#define RECORD_METHOD_OVERWRITE    (1<<4)
#define RECORD_METHOD_OVERWRITE_MM (1<<5)
#define RECORD_METHOD_OVERWRITE_PP (1<<6)

extern void ISQ_record_button( MCW_imseq * ) ;
extern void ISQ_record_CB( Widget,XtPointer,XtPointer ) ;
extern void ISQ_butsave_EV( Widget, XtPointer, XEvent *, Boolean * ) ;

extern void ISQ_record_open( MCW_imseq * ) ;
extern void ISQ_record_update( MCW_imseq * , int ) ;
extern void ISQ_record_addim( MCW_imseq * , int,int ) ;
extern XtPointer ISQ_record_getim( int , int , XtPointer ) ;
extern void ISQ_record_send_CB( MCW_imseq * , XtPointer , ISQ_cbs * ) ;
extern void ISQ_record_kill_CB( Widget , XtPointer , XtPointer ) ;

extern void ISQ_remove_widget( MCW_imseq * , Widget ) ;
extern void ISQ_cropper( MCW_imseq *, XButtonEvent *) ; /* 17 Jun 2002 */

#define MINCROP 9  /* moved here 03 May 2007 */

extern void ISQ_snapshot( Widget w ) ;                 /* 18 Jun 2003 */
extern void ISQ_snapsave( int,int, byte *, Widget ) ;  /* 03 Jul 2003 */
extern void ISQ_snap_agif( char * ) ;                  /* 06 Dec 2006 */
extern void ISQ_snap_mpeg( char * ) ;
extern void ISQ_snap_jpeg( char * ) ;
extern void ISQ_snap_png ( char * ) ;
extern void ISQ_snap_agif_rng( char *,int,int ) ;      /* 07 Dec 2006 */
extern void ISQ_snap_mpeg_rng( char *,int,int ) ;
extern void ISQ_snap_jpeg_rng( char *,int,int ) ;
extern void ISQ_snap_png_rng ( char *,int,int ) ;

extern int ISQ_handle_keypress( MCW_imseq * , unsigned long ); /* 18 Feb 2005 */

extern void mri_rgb_transform_nD( MRI_IMAGE *, int, generic_func * ) ;

extern void ISQ_save_jpeg( MCW_imseq *seq , char *fname ) ;    /* 28 Jul 2005 */
extern void ISQ_save_png ( MCW_imseq *seq , char *fname ) ;    /* 11 Dec 2006 */
extern void ISQ_save_anim( MCW_imseq *, char *, int,int,int ); /* 06 Dec 2006 */

extern void ISQ_save_raw    ( MCW_imseq *seq , char *fname ) ; /* 13 Nov 2007 */
extern void ISQ_save_rawmont( MCW_imseq *seq , char *fname ) ; /* 13 Nov 2007 */

extern void ISQ_save_image( MCW_imseq *, char *, char *, char *); /* 11 Dec 2006 */

extern void ISQ_setup_ppmto_filters(void); /* 11 May 2006 */
void SNAP_NoDuplicates (void);
void SNAP_OkDuplicates (void);


#ifdef  __cplusplus
}
#endif

#endif /* _MCW_IMSEQ_HEADER_ */
