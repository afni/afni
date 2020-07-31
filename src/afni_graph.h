/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _AFNI_HEADER_GRAPH_
#define _AFNI_HEADER_GRAPH_

/*------------------------------------------------------------------------
   This code is adapted from FD2, which was taken from FD.  It is
   therefore by RW Cox, A Jesmanowicz, and EC Wong.  It may not be
   copied or used for any commercial purpose without explicit permission.
--------------------------------------------------------------------------*/
#include "mrilib.h"
#include "coxplot.h"
#include "display.h"
#include "xutil.h"
#include "bbox.h"
#include "xim.h"

#define WANT_AFNI_BITMAP
#ifdef WANT_AFNI_BITMAP
# include "logo.h"
#endif

#if 0
# include "overfim.h"
# include "pcor.h"
#endif

#ifdef SPARKY
#undef _POSIX_SOURCE
#endif

#include <sys/types.h>
#include <sys/stat.h>

#include <X11/X.h>
#include <X11/cursorfont.h>
#include <X11/Intrinsic.h>

#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/DrawingA.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <X11/Shell.h>
#include <Xm/MwmUtil.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/Separator.h>

#ifdef  __cplusplus
extern "C" {
#endif

#ifndef LABEL_ARG
#define LABEL_ARG(str) \
  XtVaTypedArg , XmNlabelString , XmRString , (str) , strlen(str)+1
#endif

#ifndef DONT_USE_OPTMENUS
#ifndef USE_OPTMENUS
#define USE_OPTMENUS
#endif
#endif

#ifndef BE_AFNI_IGNORANT
#define BE_AFNI_AWARE
#endif

#ifdef BE_AFNI_AWARE
#undef DONT_MANGLE_XYZ
#endif

#define EXRONE(g) if( (g)->status->num_series < 2 ) EXRETURN
#define RONE(g,v) if( (g)->status->num_series < 2 ) RETURN(v)

/***-----------------------------------------------------------------------***/

#define GX_MAX    512                  /* Horizontal size of graph window */
#define GY_MAX    512                  /* Vertical size of graph window */
#define GR_DLX    4                    /* Horizontal delta to right edge */
#define GT_DLY    4                    /* Vertical delta to top edge */
#define GL_DLX    54                   /* Horizontal delta to left edge */
#define GB_DLY    52                   /* Vertical delta to bottom edge */
#define MAT_MAX   21                   /* Maximum array size of graphs */
#define COL_NUM   5                    /* Number of colors */
#define STR_L     256                  /* Max length of string */

#define MIN_XSIZE 120
#define MIN_YSIZE 120

#define XSPACE  8
#define YSPACE  20
#define MYTXT   14
#define MDX1    (GL_DLX + 1)
#define MDY1    (GB_DLY + 1)

/***-----------------------------------------------------------------------***/
typedef struct {
   Widget fim_menu , fim_cbut ;
   Widget fim_plot_menu , fim_plot_cbut ,
          fim_plot_firstref_pb , fim_plot_allrefs_pb ,
          fim_plot_firstort_pb , fim_plot_allorts_pb  ;
   Widget fim_editref_menu , fim_editref_cbut ,
          fim_editref_clear_pb , fim_editref_equals_pb ,
          fim_editref_add_pb , fim_editref_smooth_pb ,
          fim_editref_read_pb , fim_editref_write_pb , fim_editref_store_pb ,
          fim_editref_setshift_pb , fim_editort_clear_pb ,
          fim_polort_choose_pb , fim_bkthr_choose_pb ;
   MCW_bbox *fim_editref_winaver_bbox ;  /* 26 Jan 2004 */
   Widget fim_ignore_menu   , fim_ignore_cbut ,
          fim_ignore_down_pb, fim_ignore_up_pb , fim_ignore_choose_pb ;
   Widget fim_pickref_pb , fim_pickort_pb , fim_execute_pb , fim_execfimp_pb ;
   Widget fimp_setdefault_pb , fimp_setall_pb , fimp_unsetall_pb ;
   Widget fim_pickdset_pb ;

   MCW_bbox * fim_opt_bbox , * fimp_opt_bbox ;
   MCW_bbox * fimp_user_bbox ;

#ifdef USE_OPTMENUS
   MCW_arrowval * fim_ignore_choose_av ;
   MCW_arrowval * fim_polort_choose_av ;
#endif

   XtCallbackProc cbfunc ;
   XtPointer      parent ;
} FIM_menu ;
/***-----------------------------------------------------------------------***/

#ifndef HAVE_GET_PTR_TYPEDEF
#  define HAVE_GET_PTR_TYPEDEF
   typedef XtPointer (*get_ptr)() ;  /* function type */
#endif

typedef struct {
      int num_total , num_series ;  /* # of images, # in "series" */
      int nx , ny , nz ;            /* image dimensions */

      void (* send_CB)() ;   /* callback, if non_NULL */

      MCW_function_list *transforms0D ;
      MCW_function_list *transforms1D ;

      char namecode[32] ;

      XtPointer parent , aux ;
} MCW_grapher_status ;

typedef struct {
     char * wcsuffix ;
} GRA_miscellaneous ;

#ifdef MAIN
   GRA_miscellaneous Grapher_Stuff = { NULL } ;
#else
   extern GRA_miscellaneous Grapher_Stuff ;
#endif

#define MCW_GRAPHER_TYPE 3741

#define GRA_VALID(gr) ((gr)!=NULL && (gr)->type==MCW_GRAPHER_TYPE && (gr)->valid>0)
#define GRA_REALZ(gr) ((gr)!=NULL && (gr)->type==MCW_GRAPHER_TYPE && (gr)->valid>1)

/*--- stuff for changing the graph length: pinning ---*/

#define MIN_PIN    2
#define MAX_PIN    9999
#define MAX_STRIDE 9

/* x-axis plotting range is from time index NBOT to NTOP-1 */

#define NBOT(gr) ( ((gr)->pin_bot < (gr)->status->num_series) ? (gr)->pin_bot : 0 )

#define NTOP(gr) ( ((gr)->pin_top >= MIN_PIN && (gr)->pin_top < (gr)->status->num_series) \
                  ? (gr)->pin_top : (gr)->status->num_series                              )

#define NSTRIDE(gr) ( (gr)->pin_stride )

#define ALLOW_IGNORE
#ifdef  ALLOW_IGNORE
# define NIGNORE(gr) (gr)->init_ignore
#else
# define NIGNORE(gr) 0
#endif

/* #define NABC(a,b,c) ( (int)ceil( ((b)-(a))/(double)(c) ) ) */
#define NABC(a,b,c) ( ((b) - (a) - 1) / (c) + 1 )
#define NPTS(gr)    NABC( NBOT(gr) , NTOP(gr) , NSTRIDE(gr) )

/* data plotting range is from time index TBOT to TTOP-1 */

#define TBOT(gr) NBOT(gr)
#define TTOP(gr) NTOP(gr)

#define TPTS(gr) (TTOP(gr)-TBOT(gr))   /* number of data points visible in graph */

#define ISONE(g) ( TPTS(g) < 2 )       /* if only 1 data point is visible */

/*-- 22 Apr 1997:
     user supplied strings (tuser) for each graph subwindow --*/

#define GRA_NULL_tuser(gr)                     \
   do{ int iq,jq ;                             \
       for( iq=0 ; iq < MAT_MAX ; iq++ )       \
          for( jq=0 ; jq < MAT_MAX ; jq++ )    \
             (gr)->tuser[iq][jq] = NULL ; } while(0)

#define GRA_CLEAR_tuser(gr)                    \
   do{ int iq,jq ;                             \
       for( iq=0 ; iq < MAT_MAX ; iq++ )       \
          for( jq=0 ; jq < MAT_MAX ; jq++ )    \
             myXtFree((gr)->tuser[iq][jq]) ; } while(0)

/******** 16 June 1997:  Stuff for choosing colors in the graph ******/

#include "pbar_color_defs.h"

#define NUM_FIXED_COLORS_SETTING 4
#define NUM_COLOR_ITEMS          10
#define PMPLOT_INDEX             9    /* 02 Jun 2020 */

#define BRIGHTEST_COLOR   -1
#define DARKEST_COLOR     -2
#define REDDEST_COLOR     -3
#define GREENEST_COLOR    -4
#define BLUEST_COLOR      -5

#define DEFAULT_GR_BOXES_COLOR    DARKEST_COLOR
#define DEFAULT_GR_BACKG_COLOR    BRIGHTEST_COLOR
#define DEFAULT_GR_GRID_COLOR     COL_yell_oran
#define DEFAULT_GR_TEXT_COLOR     DARKEST_COLOR
#define DEFAULT_GR_DATA_COLOR     DARKEST_COLOR
#define DEFAULT_GR_IDEAL_COLOR    REDDEST_COLOR
#define DEFAULT_GR_ORT_COLOR      GREENEST_COLOR
#define DEFAULT_GR_IGNORE_COLOR   BLUEST_COLOR
#define DEFAULT_GR_DPLOT_COLOR    REDDEST_COLOR
#define DEFAULT_GR_PMPLOT_COLOR   COL_cyan    /* 02 Jun 2020 */

#define INVERTT_GR_BOXES_COLOR    BRIGHTEST_COLOR
#define INVERTT_GR_BACKG_COLOR    DARKEST_COLOR
#define INVERTT_GR_GRID_COLOR     COL_gry_bb
#define INVERTT_GR_TEXT_COLOR     BRIGHTEST_COLOR
#define INVERTT_GR_DATA_COLOR     BRIGHTEST_COLOR
#define INVERTT_GR_IDEAL_COLOR    REDDEST_COLOR
#define INVERTT_GR_ORT_COLOR      GREENEST_COLOR
#define INVERTT_GR_IGNORE_COLOR   BLUEST_COLOR
#define INVERTT_GR_DPLOT_COLOR    REDDEST_COLOR
#define INVERTT_GR_PMPLOT_COLOR   COL_cyan    /* 02 Jun 2020 */

#ifdef MAIN
int INIT_GR_boxes_color  = DEFAULT_GR_BOXES_COLOR  ,
    INIT_GR_backg_color  = DEFAULT_GR_BACKG_COLOR  ,
    INIT_GR_grid_color   = DEFAULT_GR_GRID_COLOR   ,
    INIT_GR_text_color   = DEFAULT_GR_TEXT_COLOR   ,
    INIT_GR_data_color   = DEFAULT_GR_DATA_COLOR   ,
    INIT_GR_ideal_color  = DEFAULT_GR_IDEAL_COLOR  ,
    INIT_GR_ort_color    = DEFAULT_GR_ORT_COLOR    ,
    INIT_GR_ignore_color = DEFAULT_GR_IGNORE_COLOR ,
    INIT_GR_dplot_color  = DEFAULT_GR_DPLOT_COLOR  ,
    INIT_GR_pmplot_color = DEFAULT_GR_PMPLOT_COLOR  ;    /* 02 Jun 2020 */

int INIT_GR_boxes_thick  = 0 ,  /* if >= 0, then gets a 'Thick Lines' */
    INIT_GR_grid_thick   = 0 ,  /* and these are initial settings */
    INIT_GR_data_thick   = 1 ,
    INIT_GR_ideal_thick  = 1 ,
    INIT_GR_ort_thick    = 1 ,
    INIT_GR_dplot_thick  = 1 , 
    INIT_GR_pmplot_thick = 1  ;

int INIT_GR_ggap         = 4 ;  /* 27 May 1999 */
int INIT_GR_gthick       = 2 ;  /* 06 Oct 2004 */
int INIT_GR_gmat         = 3 ;  /* 10 Feb 2003 */

int fixed_colors[NUM_FIXED_COLORS_SETTING][NUM_COLOR_ITEMS] =
     {
       { DEFAULT_GR_BOXES_COLOR,
         DEFAULT_GR_BACKG_COLOR,
         DEFAULT_GR_GRID_COLOR,
         DEFAULT_GR_TEXT_COLOR,
         DEFAULT_GR_DATA_COLOR,
         DEFAULT_GR_IDEAL_COLOR,
         DEFAULT_GR_ORT_COLOR,
         DEFAULT_GR_IGNORE_COLOR,
         DEFAULT_GR_DPLOT_COLOR,
         DEFAULT_GR_PMPLOT_COLOR   } ,
       { INVERTT_GR_BOXES_COLOR,
         INVERTT_GR_BACKG_COLOR,
         INVERTT_GR_GRID_COLOR,
         INVERTT_GR_TEXT_COLOR,
         INVERTT_GR_DATA_COLOR,
         INVERTT_GR_IDEAL_COLOR,
         INVERTT_GR_ORT_COLOR,
         INVERTT_GR_IGNORE_COLOR,
         INVERTT_GR_DPLOT_COLOR,
         INVERTT_GR_PMPLOT_COLOR   } ,
       { COL_dk_blue ,
         COL_yellow ,
         COL_blue_cyan ,
         COL_dk_blue ,
         COL_dk_blue ,
         DEFAULT_GR_IDEAL_COLOR,
         COL_rbgyr20_07 ,
         DEFAULT_GR_IGNORE_COLOR,
         DEFAULT_GR_DPLOT_COLOR,
         DEFAULT_GR_PMPLOT_COLOR   } ,
       { COL_yell_oran ,
         COL_dk_blue ,
         COL_lt_blue2 ,
         COL_yellow ,
         COL_yellow ,
         COL_hotpink ,
         COL_green ,
         COL_blue_cyan ,
         COL_red,
         COL_violet                  } ,
     } ;
#else
extern int INIT_GR_boxes_color  ,
           INIT_GR_backg_color  ,
           INIT_GR_grid_color   ,
           INIT_GR_text_color   ,
           INIT_GR_data_color   ,
           INIT_GR_ideal_color  ,
           INIT_GR_ort_color    ,
           INIT_GR_ignore_color ,
           INIT_GR_dplot_color  ,
           INIT_GR_pmplot_color  ;

extern int INIT_GR_boxes_thick ,
           INIT_GR_grid_thick  ,
           INIT_GR_data_thick  ,
           INIT_GR_ideal_thick ,
           INIT_GR_ort_thick   ,
           INIT_GR_dplot_thick ,
           INIT_GR_pmplot_thick  ;

extern int INIT_GR_ggap ;
extern int INIT_GR_gthick ;  /* 06 Oct 2004 */
extern int INIT_GR_gmat ;
extern int fixed_colors[NUM_FIXED_COLORS_SETTING][NUM_COLOR_ITEMS] ;
#endif /* MAIN */

#define FG_COLOR(gr)     ((gr)->color_index[0])
#define BG_COLOR(gr)     ((gr)->color_index[1])
#define GRID_COLOR(gr)   ((gr)->color_index[2])
#define TEXT_COLOR(gr)   ((gr)->color_index[3])
#define DATA_COLOR(gr)   ((gr)->color_index[4])
#define IDEAL_COLOR(gr)  ((gr)->color_index[5])
#define ORT_COLOR(gr)    ((gr)->color_index[6])
#define IGNORE_COLOR(gr) ((gr)->color_index[7])
#define DPLOT_COLOR(gr)  ((gr)->color_index[8])
#define PMPLOT_COLOR(gr) ((gr)->color_index[9])    /* 02 Jun 2020 */

static char *gr_color_label[NUM_COLOR_ITEMS] = {
  "Boxes " , "BackG " , "Grid  " , "Text  " ,
  "Data  " , "Ideal " , "Ort   " , "Ignore" , "Dplot " , "PMplot"
} ;

static char *gr_color_hint[NUM_COLOR_ITEMS] = {
  "Color for boxes around graphs" ,
  "Background color"              ,
  "Vertical Grid color"           ,
  "Color for Text"                ,
  "Color for Data graphs"         ,
  "Color for Ideal overplot graph",
  "Color for Ort overplot graph"  ,
  "Color for Ignored timepoints"  ,
  "Color for Dplot overlay"       ,
  "Color for PMplot overlay"
} ;

static int gr_setup_default = 1 ;
static int gr_color_default[NUM_COLOR_ITEMS] ;
static int gr_thick_default[NUM_COLOR_ITEMS] ;

static int gr_points_default[NUM_COLOR_ITEMS] = {
  -1 , -1 , -1 , -1 ,
   0 , -1 , -1 , -1 , 0 , 0
} ;

static int gr_color_start[NUM_COLOR_ITEMS] = {
  1 , 1 , 0 , 1 ,        /* 0 = start with 'none' */
  1 , 1 , 1 , 1 , 1 , 1  /* 1 = skip 'none' color */
} ;

static int gr_unfim[NUM_COLOR_ITEMS] = { 0,0,0,0,0,1,1,1,0,0 } ;  /* Oct 1999 */

#define GRA_COLOR(cd)                                              \
   ( ((cd) == BRIGHTEST_COLOR)  ? (grapher->dc->ovc->ov_brightest) \
    :((cd) == DARKEST_COLOR  )  ? (grapher->dc->ovc->ov_darkest)   \
    :((cd) == REDDEST_COLOR   ) ? (grapher->dc->ovc->ov_reddest)   \
    :((cd) == GREENEST_COLOR )  ? (grapher->dc->ovc->ov_greenest)  \
    :((cd) == BLUEST_COLOR  )   ? (grapher->dc->ovc->ov_bluest)    \
    :(cd) )

#define FG_THICK(gr)      ((gr)->thick_index[0] * (gr)->gthick)
#define BG_THICK(gr)      ((gr)->thick_index[1] * (gr)->gthick)
#define GRID_THICK(gr)    ((gr)->thick_index[2] * (gr)->gthick)
#define TEXT_THICK(gr)    ((gr)->thick_index[3] * (gr)->gthick)
#define DATA_THICK(gr)    ((gr)->thick_index[4] * (gr)->gthick)
#define IDEAL_THICK(gr)   ((gr)->thick_index[5] * (gr)->gthick)
#define ORT_THICK(gr)     ((gr)->thick_index[6] * (gr)->gthick)
#define IGNORE_THICK(gr)  ((gr)->thick_index[7] * (gr)->gthick)
#define DPLOT_THICK(gr)   ((gr)->thick_index[8] * (gr)->gthick)
#define PMPLOT_THICK(gr)  ((gr)->thick_index[9] * (gr)->gthick)    /* 02 Jun 2020 */

#define FG_IS_THICK(gr)     ((gr)->thick_index[0] != 0)
#define BG_IS_THICK(gr)     ((gr)->thick_index[1] != 0)
#define GRID_IS_THICK(gr)   ((gr)->thick_index[2] != 0)
#define TEXT_IS_THICK(gr)   ((gr)->thick_index[3] != 0)
#define DATA_IS_THICK(gr)   ((gr)->thick_index[4] != 0)
#define IDEAL_IS_THICK(gr)  ((gr)->thick_index[5] != 0)
#define ORT_IS_THICK(gr)    ((gr)->thick_index[6] != 0)
#define IGNORE_IS_THICK(gr) ((gr)->thick_index[7] != 0)
#define DPLOT_IS_THICK(gr)  ((gr)->thick_index[8] != 0)
#define PMPLOT_IS_THICK(gr) ((gr)->thick_index[9] != 0)  /* 02 Jun 2020 */

#define DO_UPSAM(gr)        ((gr)->do_upsam)             /* 28 May 2020 */

/* amount of resampling in plot_graphs                      28 May 2020 */
#define XUPSAM(www,npt) ( (int)( 0.499f + 0.3456f*(www) / ((npt)+0.5f) ) )

/* Replacement for XDrawLines, now with chocolate sprinkles [28 May 2020] */
void AFNI_XDrawLines( Display *display, Drawable d,
                      GC gc, XPoint *points, int npoints, int mode , int nupsam ) ;

/* Replacement for XFillPolygon, with champagne truffles [01 Jun 2020] */
void AFNI_XFillPolygon( Display *display, Drawable d,
                        GC gc, XPoint *points, int npoints, int shape ,
                        int mode , int nupsam ) ;

/** 01 Aug 1998: redefine _POINTS and add _LINES **/

#define FG_POINTS(gr)     ((gr)->points_index[0] != 0)
#define BG_POINTS(gr)     ((gr)->points_index[1] != 0)
#define GRID_POINTS(gr)   ((gr)->points_index[2] != 0)
#define TEXT_POINTS(gr)   ((gr)->points_index[3] != 0)
#define DATA_POINTS(gr)   ((gr)->points_index[4] == 1 || (gr)->points_index[4] == 2)
#define IDEAL_POINTS(gr)  ((gr)->points_index[5] != 0)
#define ORT_POINTS(gr)    ((gr)->points_index[6] != 0)
#define IGNORE_POINTS(gr) ((gr)->points_index[7] != 0)
#define DPLOT_POINTS(gr)  ((gr)->points_index[8] != 0)
#define PMPLOT_POINTS(gr) 0                          /* 02 Jun 2020 */

#define PMPLOT_MODE(gr)   ((gr)->points_index[9])    /* 02 Jun 2020 */
#define PMPLOT_OFF           1
#define PMPLOT_CURVES        2
#define PMPLOT_BARS          4
#define PMPLOT_FILL          8

#define FG_LINES(gr)     ((gr)->points_index[0] != 1)
#define BG_LINES(gr)     ((gr)->points_index[1] != 1)
#define GRID_LINES(gr)   ((gr)->points_index[2] != 1)
#define TEXT_LINES(gr)   ((gr)->points_index[3] != 1)
#define DATA_LINES(gr)   ((gr)->points_index[4] == 0 || (gr)->points_index[4] == 2)
#define IDEAL_LINES(gr)  ((gr)->points_index[5] != 1)
#define ORT_LINES(gr)    ((gr)->points_index[6] != 1)
#define IGNORE_LINES(gr) ((gr)->points_index[7] != 1)
#define DPLOT_LINES(gr)  ((gr)->points_index[8] != 1)

#define DATA_BOXED(gr)   ((gr)->points_index[4] == 4)  /* 26 Jun 2007 */

extern void GRA_color_CB( MCW_arrowval * , XtPointer ) ;
extern void GRA_thick_CB( Widget , XtPointer , XtPointer ) ;

/**************************************************************/

#define PLOTCODE_AUTOSCALE 1

typedef struct {
   int type , valid ;

   int gx_max , gy_max ;  /* window sizes */
   int fWIDE  , fHIGH  ;

   get_ptr   getser ;
   XtPointer getaux ;
   MCW_grapher_status *status ;

   /* sub-graph stuff */

   int xorigin[MAT_MAX][MAT_MAX] , yorigin[MAT_MAX][MAT_MAX] ; /* coords of graphs*/
   float pmin[MAT_MAX][MAT_MAX]  , pmax[MAT_MAX][MAT_MAX] ;    /* plot ranges */

   float tmean[MAT_MAX][MAT_MAX] , tbot[MAT_MAX][MAT_MAX] ,    /* statistics */
         ttop[MAT_MAX][MAT_MAX]  , tstd[MAT_MAX][MAT_MAX]  ;
   float dbot[MAT_MAX][MAT_MAX]  , dtop[MAT_MAX][MAT_MAX]  ;   /* 01 Jun 2020 */

   char * tuser[MAT_MAX][MAT_MAX] ;                            /* user strings */

   int mat,mat_max , xpoint,ypoint,zpoint ;
   int grid_index , grid_spacing , grid_fixed ;
   int xFD , yFD , gx,gy , xc,yc ;
   int grid_color , common_base , init_ignore , polort ;
   float fscale ;
   int pin_top ;      /* 27 Apr 1997 - top index to show */
   int pin_bot ;      /* 17 Mar 2004 - bottom index to show */
   int pin_stride ;   /* 19 Jul 2013 - step thru data */
   int HorZ ;         /* 05 Jan 1999 - horizontal line at 0? */

   int key_Nlock , key_lock_sum ;
   int time_index ;

   int        ncen_line , nncen ;
   XPoint    *cen_line ;
   MRI_IMAGE *cen_tsim ;
   MRI_IMAGE *xax_tsim ;  /* 09 Jan 1998 */
   MRI_IMAGE *ave_tsim ;  /* 26 Jan 2004 */
   void      *xax_dset ;  /* 09 Feb 2015 */
   void      *xax_fdbr ;
   MRI_IMAGE *xax_cen  ;  /* 12 Feb 2015 */

   int xx_text_1 , xx_text_2 , xx_text_2p , xx_text_3 , xx_text_igf ;

   /* external time-series stuff */

   MRI_IMARR *ref_ts , *ort_ts ;
   int ref_ts_color , ort_ts_color ;
   int ref_ts_plotall , ort_ts_plotall ;

   /* X11 stuff */

   Pixmap  fd_pxWind ;   /* graphs are drawn into this for speed's sake */
   Widget  top_form ;    /* 24 May 2018 */
   Widget  fdw_graph ;   /* top level widget */
   Widget  draw_fd ;     /* drawing area */
   Pixmap  glogo_pixmap ; /* for the corner */
   int     glogo_width , glogo_height ;

   Widget option_rowcol ;  /* 29 Sep 2000: removed option_mbar */

   Widget opt_menu , opt_cbut ;
   Widget opt_scale_menu    , opt_scale_cbut  ,
          opt_scale_down_pb , opt_scale_up_pb , opt_scale_choose_pb ,
          opt_scale_auto_pb , opt_scale_AUTO_pb;
   Widget opt_mat_menu      , opt_mat_cbut ,
          opt_mat_down_pb   , opt_mat_up_pb   ;
   Widget opt_grid_menu     , opt_grid_cbut   ,
          opt_grid_down_pb  , opt_grid_up_pb  ,
          opt_grid_choose_pb , opt_pin_choose_pb ;
   Widget opt_grid_auto_pb ;                      /* 02 Apr 2004 */
   Widget opt_grid_HorZ_pb ;                      /* 05 Jan 1999 */
   Widget opt_slice_menu    , opt_slice_cbut  ,
          opt_slice_down_pb , opt_slice_up_pb ;

   Widget opt_colors_menu , opt_colors_cbut ;
   MCW_arrowval * opt_color_av[NUM_COLOR_ITEMS] ;
   MCW_bbox     * opt_thick_bbox[NUM_COLOR_ITEMS] ;
   MCW_bbox     * opt_points_bbox[NUM_COLOR_ITEMS] ;
   int color_index[NUM_COLOR_ITEMS] ;
   int thick_index[NUM_COLOR_ITEMS] ;
   int points_index[NUM_COLOR_ITEMS] ;
   int fixed_colors_setting ;

   MCW_arrowval *opt_ggap_av ;   /* 12 Jan 1998 */
   int ggap ;
   MCW_arrowval *opt_gthick_av ; /* 06 Oct 2004 */
   int gthick ;
   MCW_arrowval *opt_upsam_av ;  /* 28 May 2020 */
   int do_upsam ;

   Widget opt_color_up_pb   , opt_save_pb ,
          opt_write_center_pb , opt_write_suffix_pb ;
   Widget opt_quit_pb ;

   Widget opt_xaxis_menu , opt_xaxis_cbut ,        /* 09 Jan 1998 */
          opt_xaxis_pick_pb , opt_xaxis_center_pb ,
          opt_xaxis_clear_pb , opt_xaxis_dset_pb ;

   Widget opt_baseline_menu , opt_baseline_cbut ,   /* 07 Aug 2001 */
          opt_baseline_setglobal_pb ,
          opt_baseline_global_label  ;
   float global_base ;

   MCW_bbox *opt_textgraph_bbox , *opt_baseline_bbox ;    /* 22 Sep 2000 */
   int textgraph ;

#ifdef USE_OPTMENUS
   MCW_arrowval * opt_mat_choose_av , * opt_slice_choose_av ;
#else
   Widget opt_mat_choose_pb , opt_slice_choose_pb ;
#endif

   MCW_arrowval * transform0D_av ;
   generic_func * transform0D_func ;
   int            transform0D_index , transform0D_flags ;

   MCW_arrowval * transform1D_av ;
   generic_func * transform1D_func ;
   int            transform1D_index , transform1D_flags ;

   Widget opt_dplot_menu , opt_dplot_cbut ;   /* 07 Aug 2001 */
   MCW_bbox  * opt_dplot_bbox ;

   FIM_menu * fmenu ;

   Widget but3_menu , but3_label ;

   Widget dialog ;
   MCW_arrowval * setshift_inc_av , * setshift_left_av , * setshift_right_av ;
   float setshift_inc ;
   int   setshift_left , setshift_right ;

   MCW_DC * dc ;

   int never_drawn ;
   int button2_enabled ;
   int mirror ;

   XtPointer parent ;

   float tmed[MAT_MAX][MAT_MAX] , tmad[MAT_MAX][MAT_MAX] ;    /* 08 Mar 2001 */
   int   sbot[MAT_MAX][MAT_MAX] , stop[MAT_MAX][MAT_MAX] ;    /* 19 Mar 2004 */
   float tbmv[MAT_MAX][MAT_MAX] ;                             /* 16 Oct 2009 */

   float xax_bot[MAT_MAX][MAT_MAX], xax_top[MAT_MAX][MAT_MAX] ; /* 12 Feb 2015 */

   XtIntervalId timer_id ;                          /* 04 Dec 2003 */
   int          timer_func, timer_param, timer_delay ;

   int dont_setref ;                                /* 27 Jan 2004 */
   int dont_redraw ;                                /* 27 Jan 2004 */
   int tschosen ;                                   /* 31 Mar 2004 */

   MCW_arrowval *detrend_av ;
   int           detrend ;                          /* 05 Dec 2012 */

   int           thresh_fade ;                      /* Mar 2013 */
   MCW_bbox  *opt_tfade_bbox ;

} MCW_grapher ;

#define GRA_TIMERFUNC_INDEX  701
#define GRA_TIMERFUNC_BOUNCE 702

extern void GRA_timer_CB( XtPointer , XtIntervalId * ) ; /* 04 Dec 2003 */
extern void GRA_timer_stop( MCW_grapher * ) ;

#define BASELINE_INDIVIDUAL  1  /* 07 Aug 2001 */
#define BASELINE_COMMON      2
#define BASELINE_GLOBAL      4

#define DPLOT_OFF            1  /* 07 Aug 2001 */
#define DPLOT_OVERLAY        2
#define DPLOT_PLUSMINUS      4

static int fd_x    = 0 ;
static int fd_y[8] = {100, 130, 160, 190, 220, 250, 280, 310};

/*--- "callback" data stuff: info about events in grapher window ---*/

typedef struct {
      int          reason ;              /* graCR_??? defined below */
      XEvent *     event ;               /* may be NULL */
      int          xcen,ycen,zcen, mat ; /* new x , new y , new matrix */
      int          key ;                 /* keyvalue (if keypress type) */
      XtPointer    userdata ;            /* misc type of data */
} GRA_cbs ;

/* These codes must be distinct from isqCR_* in imseq.h */

#define graCR_getstatus   7701
#define graCR_getseries   7702
#define graCR_getlabel    7703         /* 18 Apr 2011 */

#define graCR_newxyzm     7711
#define graCR_keypress    7714

#define graCR_pickref            7721
#define graCR_pickort            7722
#define graCR_clearfim           7723
#define graCR_timeseries_library 7724
#define graCR_clearort           7725
#define graCR_polort             7726  /* 27 May 1999 */
#define graCR_winaver            7727  /* 27 Jan 2004 */

#define graCR_dofim       7731

#define graCR_refequals   7741
#define graCR_refadd      7742
#define graCR_refsmooth   7745
#define graCR_reflibrary  7746

#define graCR_setignore   7751
#define graCR_setindex    7752

#define graCR_destroy        7777
#define graCR_raiseupthedead 7778  /* 17 Jun 2011 */

#define graCR_button2_points 8801  /* Feb 1998 */

/* The following were stolen from imseq.h, then
   had the serial numbers changed to confuse issue */

#define graDR_helptext    101
#define graDR_cursor      104
#define graDR_unrealize   105
#define graDR_realize     106
#define graDR_redraw      111
#define graDR_newdata     112
#define graDR_newlength   113  /* 22 Apr 1997 */
#define graDR_title       114
#define graDR_icon        118

#define graDR_addref_ts   121
#define graDR_addort_ts   122
#define graDR_setignore   123
#define graDR_setindex    124
#define graDR_polort      125  /* 27 May 1999 */
#define graDR_winaver     126  /* 27 Jan 2004 */

#define graDR_setmatrix   130  /* 22 Sep 2000 */
#define graDR_setgrid     131
#define graDR_setpinnum   graDR_newlength
#define graDR_setpintop   graDR_newlength
#define graDR_setpinbot   132  /* 17 Mar 2004 */
#define graDR_setpins     133  /* 19 Mar 2004 */

#define graDR_destroy     666

#define graDR_button2_enable  501  /* Feb 1998 */
#define graDR_button2_disable 502  /* Feb 1998 */

#define graDR_fim_disable     503  /* Oct 1999 */

#define graDR_mirror          504  /* Jul 2000 */

#define graDR_setglobalbaseline 505 /* 07 Aug 2001 */

/***-----------------------------------------------------------------------***/

extern MCW_grapher * new_MCW_grapher( MCW_DC * , get_ptr , XtPointer ) ;

extern void end_fd_graph_CB(Widget,XtPointer,XtPointer);
extern void fd_px_store(MCW_grapher *);
extern void plot_fdX(MCW_grapher *,int,int,int);
extern void scale_up(MCW_grapher *);
extern void scale_down(MCW_grapher *);

extern void erase_fdw(MCW_grapher *);
extern void fd_txt(MCW_grapher *,int,int,char *);
extern void overlay_txt(MCW_grapher *,int,int,char *);
extern void plot_graphs(MCW_grapher *, int);
extern void draw_grids(MCW_grapher *);
extern void init_mat(MCW_grapher *);
extern void send_newinfo(MCW_grapher *);
extern void mat_down(MCW_grapher *);
extern void mat_up(MCW_grapher *);
extern void grid_down(MCW_grapher *);
extern void grid_up(MCW_grapher *);

extern void text_graphs(MCW_grapher *) ;  /* 22 Sep 2000 */

extern void redraw_graph( MCW_grapher * , int ) ;
extern void init_const( MCW_grapher * ) ;

extern void GRA_small_circle( MCW_grapher * , int,int,int ) ;
extern void GRA_overlay_circle( MCW_grapher * , int,int,int ) ;

extern void GRA_drawing_EV( Widget  , XtPointer , XEvent * , RwcBoolean * ) ;
extern void GRA_handle_keypress( MCW_grapher * , char * , XEvent * ) ;
extern void GRA_new_pixmap( MCW_grapher * , int,int,int ) ;
extern void GRA_opt_CB( Widget , XtPointer , XtPointer ) ;
extern void GRA_fim_CB( Widget , XtPointer , XtPointer ) ;
extern RwcBoolean drive_MCW_grapher( MCW_grapher * , int , XtPointer ) ;

extern void GRA_scale_choose_CB   ( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void GRA_grid_choose_CB    ( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void GRA_pin_choose_CB     ( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void GRA_wcsuffix_choose_CB( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void GRA_refread_choose_CB ( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void GRA_refwrite_choose_CB( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void GRA_refstore_choose_CB( Widget , XtPointer , MCW_choose_cbs * ) ;

#ifdef USE_OPTMENUS
extern void GRA_mat_choose_CB     ( MCW_arrowval * , XtPointer ) ;
extern void GRA_slice_choose_CB   ( MCW_arrowval * , XtPointer ) ;
extern void GRA_ignore_choose_CB  ( MCW_arrowval * , XtPointer ) ;
extern void GRA_polort_choose_CB  ( MCW_arrowval * , XtPointer ) ;
extern void GRA_fmenu_av_CB       ( MCW_arrowval * , XtPointer ) ;
extern void GRA_fix_optmenus      ( MCW_grapher * ) ;
#else
extern void GRA_mat_choose_CB     ( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void GRA_slice_choose_CB   ( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void GRA_ignore_choose_CB  ( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void GRA_polort_choose_CB  ( Widget , XtPointer , MCW_choose_cbs * ) ;
#endif
extern void GRA_bkthr_choose_CB   ( Widget , XtPointer , MCW_choose_cbs * ) ;

extern void GRA_setshift_startup( MCW_grapher * ) ;
extern void GRA_doshift( MCW_grapher * ) ;
extern void GRA_setshift_action_CB( Widget , XtPointer , XtPointer ) ;

extern void GRA_transform_CB     ( MCW_arrowval * , XtPointer ) ;
extern char * GRA_transform_label( MCW_arrowval * , XtPointer ) ;

extern void GRA_detrend_CB       ( MCW_arrowval * , XtPointer ) ;  /* 05 Dec 2012 */

extern void GRA_ggap_CB  ( MCW_arrowval * , XtPointer ) ;
extern void GRA_gthick_CB( MCW_arrowval * , XtPointer ) ;  /* 06 Oct 2004 */
extern void GRA_upsam_CB ( MCW_arrowval * , XtPointer ) ;  /* 28 May 2020 */

extern FIM_menu * AFNI_new_fim_menu( Widget , XtCallbackProc , int ) ;

extern void GRA_redraw_overlay( MCW_grapher * ) ;

extern void GRA_dplot_change_CB( Widget , XtPointer , XtPointer ) ;

extern void GRA_winaver_CB     ( Widget , XtPointer , XtPointer ) ;  /* 27 Jan 2004 */
extern void GRA_winaver_setref ( MCW_grapher * ) ;

extern void GRA_saver_CB( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void GRA_file_pixmap( MCW_grapher * , char * ) ;

extern void GRA_fixup_xaxis( MCW_grapher * , MRI_IMAGE * ) ;
extern void GRA_pick_xaxis_CB( Widget , XtPointer , MCW_choose_cbs * ) ;

extern void GRA_mapmenu_CB( Widget , XtPointer , XtPointer ) ;

extern void GRA_textgraph_CB( Widget , XtPointer , XtPointer ) ;  /* 22 Sep 2000 */
extern void GRA_baseline_CB ( Widget , XtPointer , XtPointer ) ;  /* 22 Sep 2000 */

extern void GRA_tfade_CB( Widget , XtPointer , XtPointer ) ;  /* Mar 2013 */

 /* 07 Aug 2001 */
extern void GRA_finalize_global_baseline_CB( Widget,
                                             XtPointer, MCW_choose_cbs * );

 /* 19 Dec 2018 */
extern int GRA_find_1D_transform( MCW_grapher *grapher , char *nam ) ;
extern void GRA_startup_1D_transform( char *nam ) ;
extern void GRA_set_1D_transform( MCW_grapher *grapher , char *nam ) ;

/***-----------------------------------------------------------------------***/

#ifdef  __cplusplus
}
#endif

#endif
