#ifndef _AFNI_HEADER_GRAPH_
#define _AFNI_HEADER_GRAPH_

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*------------------------------------------------------------------------
   This code is adapted from FD2, which was taken from FD.  It is
   therefore by RW Cox, A Jesmanowicz, and EC Wong.  It may not be
   copied or used for any commercial purpose without explicit permission.
--------------------------------------------------------------------------*/

#include "mrilib.h"
#include "display.h"
#include "xutil.h"
#include "bbox.h"
#include "xim.h"

#define WANT_AFNI_BITMAP
#ifdef WANT_AFNI_BITMAP
# include "mcw.h"
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

#define GRAPHER_ALLOW_ONE  /* 22 Sep 2000: allow "graphing" of n=1 data */

#ifdef GRAPHER_ALLOW_ONE
#  define EXRONE(g) if( (g)->status->num_series < 2 ) EXRETURN
#  define RONE(g,v) if( (g)->status->num_series < 2 ) RETURN(v)
#  define ISONE(g)  ( (g)->status->num_series < 2 )
#else
#  define EXRONE(g) /* nada */
#  define RONE(g,v) /* nada */
#  define ISONE(g)  0
#endif

/***-----------------------------------------------------------------------***/

#define GX_MAX    512                  /* Horizontal size of graph window */
#define GY_MAX    512                  /* Vertical size of graph window */
#define GR_DLX    4                    /* Horizontal delta to right edge */
#define GT_DLY    4                    /* Vertical delta to top edge */
#define GL_DLX    54                   /* Horizontal delta to left edge */
#define GB_DLY    52                   /* Vertical delta to bottom edge */
#define MAT_MAX   21                   /* Maximum array size of graphs */
#define GRID_MAX  12                   /* Maximum grid index */
#define COL_NUM   5                    /* Number of colors */
#define STR_L     256                  /* Max length of string */

#define MIN_XSIZE 120
#define MIN_YSIZE 120

static int grid_ar[GRID_MAX] =
   { 2 , 5 , 10 , 20 , 50 , 100 , 200 , 500 , 1000 , 2000 , 5000 , 10000 } ;

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

      MCW_function_list * transforms0D ;
      MCW_function_list * transforms1D ;

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

#define MIN_PIN 5
#define MAX_PIN 9999
#define NPTS(gr) ( ((gr)->pin_num < MIN_PIN) ? (gr)->status->num_series \
                                             : (gr)->pin_num )

/** 22 Apr 1997:
    user supplied strings (tuser) for each graph subwindow **/

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

#define BRIGHTEST_COLOR   -1
#define DARKEST_COLOR     -2
#define REDDEST_COLOR     -3
#define GREENEST_COLOR    -4
#define BLUEST_COLOR      -5

#define DEFAULT_GR_BOXES_COLOR    DARKEST_COLOR
#define DEFAULT_GR_BACKG_COLOR    BRIGHTEST_COLOR
#define DEFAULT_GR_GRID_COLOR     1
#define DEFAULT_GR_TEXT_COLOR     DARKEST_COLOR
#define DEFAULT_GR_DATA_COLOR     DARKEST_COLOR
#define DEFAULT_GR_IDEAL_COLOR    REDDEST_COLOR
#define DEFAULT_GR_ORT_COLOR      GREENEST_COLOR
#define DEFAULT_GR_IGNORE_COLOR   BLUEST_COLOR
#define DEFAULT_GR_DPLOT_COLOR    REDDEST_COLOR

#ifdef MAIN
int INIT_GR_boxes_color  = DEFAULT_GR_BOXES_COLOR  ,
    INIT_GR_backg_color  = DEFAULT_GR_BACKG_COLOR  ,
    INIT_GR_grid_color   = DEFAULT_GR_GRID_COLOR   ,
    INIT_GR_text_color   = DEFAULT_GR_TEXT_COLOR   ,
    INIT_GR_data_color   = DEFAULT_GR_DATA_COLOR   ,
    INIT_GR_ideal_color  = DEFAULT_GR_IDEAL_COLOR  ,
    INIT_GR_ort_color    = DEFAULT_GR_ORT_COLOR    ,
    INIT_GR_ignore_color = DEFAULT_GR_IGNORE_COLOR ,
    INIT_GR_dplot_color  = DEFAULT_GR_DPLOT_COLOR   ;

int INIT_GR_boxes_thick  = 0 ,
    INIT_GR_grid_thick   = 0 ,
    INIT_GR_data_thick   = 0 ,
    INIT_GR_ideal_thick  = 0 ,
    INIT_GR_ort_thick    = 0 ,
    INIT_GR_dplot_thick  = 0  ;

int INIT_GR_ggap         = 0 ;  /* 27 May 1999 */
#else
extern int INIT_GR_boxes_color  ,
           INIT_GR_backg_color  ,
           INIT_GR_grid_color   ,
           INIT_GR_text_color   ,
           INIT_GR_data_color   ,
           INIT_GR_ideal_color  ,
           INIT_GR_ort_color    ,
           INIT_GR_ignore_color ,
           INIT_GR_dplot_color   ;

extern int INIT_GR_boxes_thick ,
           INIT_GR_grid_thick  ,
           INIT_GR_data_thick  ,
           INIT_GR_ideal_thick ,
           INIT_GR_ort_thick   ,
           INIT_GR_dplot_thick  ;

extern int INIT_GR_ggap ;
#endif /* MAIN */

#define NUM_COLOR_ITEMS 9

#define FG_COLOR(gr)     ((gr)->color_index[0])
#define BG_COLOR(gr)     ((gr)->color_index[1])
#define GRID_COLOR(gr)   ((gr)->color_index[2])
#define TEXT_COLOR(gr)   ((gr)->color_index[3])
#define DATA_COLOR(gr)   ((gr)->color_index[4])
#define IDEAL_COLOR(gr)  ((gr)->color_index[5])
#define ORT_COLOR(gr)    ((gr)->color_index[6])
#define IGNORE_COLOR(gr) ((gr)->color_index[7])
#define DPLOT_COLOR(gr)  ((gr)->color_index[8])

static char * gr_color_label[NUM_COLOR_ITEMS] = {
  "Boxes " , "BackG " , "Grid  " , "Text  " ,
  "Data  " , "Ideal " , "Ort   " , "Ignore" , "Dplot "
} ;

static int gr_setup_default = 1 ;
static int gr_color_default[NUM_COLOR_ITEMS] ;
static int gr_thick_default[NUM_COLOR_ITEMS] ;

static int gr_points_default[NUM_COLOR_ITEMS] = {
  -1 , -1 , -1 , -1 ,
   0 , -1 , -1 , -1 , 0
} ;

static int gr_color_start[NUM_COLOR_ITEMS] = {
  1 , 1 , 0 , 1 ,
  1 , 1 , 1 , 1 , 1
} ;

static int gr_unfim[NUM_COLOR_ITEMS] = { 0,0,0,0,0,1,1,1,0 } ;  /* Oct 1999 */

#define GRA_COLOR(cd)                                              \
   ( ((cd) == BRIGHTEST_COLOR)  ? (grapher->dc->ovc->ov_brightest)  \
    :((cd) == DARKEST_COLOR  )  ? (grapher->dc->ovc->ov_darkest)   \
    :((cd) == REDDEST_COLOR   ) ? (grapher->dc->ovc->ov_reddest)  \
    :((cd) == GREENEST_COLOR )  ? (grapher->dc->ovc->ov_greenest)\
    :((cd) == BLUEST_COLOR  )   ? (grapher->dc->ovc->ov_bluest) \
    :(cd) )

#define THICKKK  2

#define FG_THICK(gr)     ((gr)->thick_index[0] * THICKKK)
#define BG_THICK(gr)     ((gr)->thick_index[1] * THICKKK)
#define GRID_THICK(gr)   ((gr)->thick_index[2] * THICKKK)
#define TEXT_THICK(gr)   ((gr)->thick_index[3] * THICKKK)
#define DATA_THICK(gr)   ((gr)->thick_index[4] * THICKKK)
#define IDEAL_THICK(gr)  ((gr)->thick_index[5] * THICKKK)
#define ORT_THICK(gr)    ((gr)->thick_index[6] * THICKKK)
#define IGNORE_THICK(gr) ((gr)->thick_index[7] * THICKKK)
#define DPLOT_THICK(gr)  ((gr)->thick_index[8] * THICKKK)

#define FG_IS_THICK(gr)     ((gr)->thick_index[0] != 0)
#define BG_IS_THICK(gr)     ((gr)->thick_index[1] != 0)
#define GRID_IS_THICK(gr)   ((gr)->thick_index[2] != 0)
#define TEXT_IS_THICK(gr)   ((gr)->thick_index[3] != 0)
#define DATA_IS_THICK(gr)   ((gr)->thick_index[4] != 0)
#define IDEAL_IS_THICK(gr)  ((gr)->thick_index[5] != 0)
#define ORT_IS_THICK(gr)    ((gr)->thick_index[6] != 0)
#define IGNORE_IS_THICK(gr) ((gr)->thick_index[7] != 0)
#define DPLOT_IS_THICK(gr)  ((gr)->thick_index[8] != 0)

/** 01 Aug 1998: redefine _POINTS and add _LINES **/

#define FG_POINTS(gr)     ((gr)->points_index[0] != 0)
#define BG_POINTS(gr)     ((gr)->points_index[1] != 0)
#define GRID_POINTS(gr)   ((gr)->points_index[2] != 0)
#define TEXT_POINTS(gr)   ((gr)->points_index[3] != 0)
#define DATA_POINTS(gr)   ((gr)->points_index[4] != 0)
#define IDEAL_POINTS(gr)  ((gr)->points_index[5] != 0)
#define ORT_POINTS(gr)    ((gr)->points_index[6] != 0)
#define IGNORE_POINTS(gr) ((gr)->points_index[7] != 0)
#define DPLOT_POINTS(gr)  ((gr)->points_index[8] != 0)

#define FG_LINES(gr)     ((gr)->points_index[0] != 1) 
#define BG_LINES(gr)     ((gr)->points_index[1] != 1)
#define GRID_LINES(gr)   ((gr)->points_index[2] != 1)
#define TEXT_LINES(gr)   ((gr)->points_index[3] != 1)
#define DATA_LINES(gr)   ((gr)->points_index[4] != 1)
#define IDEAL_LINES(gr)  ((gr)->points_index[5] != 1)
#define ORT_LINES(gr)    ((gr)->points_index[6] != 1)
#define IGNORE_LINES(gr) ((gr)->points_index[7] != 1)
#define DPLOT_LINES(gr)  ((gr)->points_index[8] != 1)

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
   MCW_grapher_status * status ;

   /* sub-graph stuff */

   int xorigin[MAT_MAX][MAT_MAX] , yorigin[MAT_MAX][MAT_MAX] ; /* coords of graphs*/
   float pmin[MAT_MAX][MAT_MAX]  , pmax[MAT_MAX][MAT_MAX] ;    /* plot ranges */

   float tmean[MAT_MAX][MAT_MAX] , tbot[MAT_MAX][MAT_MAX] ,    /* statistics */
         ttop[MAT_MAX][MAT_MAX]  , tstd[MAT_MAX][MAT_MAX]  ;

   char * tuser[MAT_MAX][MAT_MAX] ;                            /* user strings */

   int mat,mat_max , xpoint,ypoint,zpoint , grid_index , grid_spacing ;
   int xFD , yFD , gx,gy , xc,yc ;
   int grid_color , common_base , init_ignore , polort ;
   float fscale ;
   int pin_num ;      /* 27 Apr 1997 */
   int HorZ ;         /* 05 Jan 1999 */

   int key_Nlock , key_lock_sum ;
   int time_index ;

   int        ncen_line , nncen ;
   XPoint    * cen_line ;
   MRI_IMAGE * cen_tsim ;
   MRI_IMAGE * xax_tsim ;  /* 09 Jan 1998 */

   int xx_text_1 , xx_text_2 ;

   /* external time-series stuff */

   MRI_IMARR * ref_ts , * ort_ts ;
   int ref_ts_color , ort_ts_color ;
   int ref_ts_plotall , ort_ts_plotall ;

   /* X11 stuff */

   Pixmap  fd_pxWind ;   /* graphs are drawn into this for speed's sake */
   Widget  fdw_graph ;   /* top level widget */
   Widget  draw_fd ;     /* drawing area */
   Pixmap  logo_pixmap ; /* for the corner */
   int     logo_width , logo_height ;

   Widget option_rowcol ;  /* 29 Sep 2000: removed option_mbar */

   Widget opt_menu , opt_cbut ;
   Widget opt_scale_menu    , opt_scale_cbut  ,
          opt_scale_down_pb , opt_scale_up_pb , opt_scale_choose_pb ,
          opt_scale_auto_pb ;
   Widget opt_mat_menu      , opt_mat_cbut ,
          opt_mat_down_pb   , opt_mat_up_pb   ;
   Widget opt_grid_menu     , opt_grid_cbut   ,
          opt_grid_down_pb  , opt_grid_up_pb  ,
          opt_grid_choose_pb , opt_pin_choose_pb ;
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

   MCW_arrowval * opt_ggap_av ; /* 12 Jan 1998 */
   int ggap ;

   Widget opt_color_up_pb   , opt_save_pb ,
          opt_write_center_pb , opt_write_suffix_pb ;
   Widget opt_quit_pb ;

   Widget opt_xaxis_menu , opt_xaxis_cbut ,        /* 09 Jan 1998 */
          opt_xaxis_pick_pb , opt_xaxis_center_pb ,
          opt_xaxis_clear_pb ;

   MCW_bbox * opt_textgraph_bbox , * opt_baseline_bbox ;    /* 22 Sep 2000 */
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
   MCW_bbox     * transform1D_dplot_bbox ;

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
} MCW_grapher ;

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

#define graCR_newxyzm     7711
#define graCR_keypress    7714

#define graCR_pickref            7721
#define graCR_pickort            7722
#define graCR_clearfim           7723
#define graCR_timeseries_library 7724
#define graCR_clearort           7725
#define graCR_polort             7726  /* 27 May 1999 */

#define graCR_dofim       7731

#define graCR_refequals   7741
#define graCR_refadd      7742
#define graCR_refsmooth   7745
#define graCR_reflibrary  7746

#define graCR_setignore   7751
#define graCR_setindex    7752

#define graCR_destroy     7777

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

#define graDR_setmatrix   130  /* 22 Sep 2000 */
#define graDR_setgrid     131
#define graDR_setpinnum   graDR_newlength

#define graDR_destroy     666

#define graDR_button2_enable  501  /* Feb 1998 */
#define graDR_button2_disable 502  /* Feb 1998 */

#define graDR_fim_disable     503  /* Oct 1999 */

#define graDR_mirror          504  /* Jul 2000 */

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

extern void GRA_drawing_EV( Widget  , XtPointer , XEvent * , Boolean * ) ;
extern void GRA_handle_keypress( MCW_grapher * , char * , XEvent * ) ;
extern void GRA_new_pixmap( MCW_grapher * , int,int,int ) ;
extern void GRA_opt_CB( Widget , XtPointer , XtPointer ) ;
extern void GRA_fim_CB( Widget , XtPointer , XtPointer ) ;
extern Boolean drive_MCW_grapher( MCW_grapher * , int , XtPointer ) ;

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

extern void GRA_ggap_CB( MCW_arrowval * , XtPointer ) ;

extern FIM_menu * AFNI_new_fim_menu( Widget , XtCallbackProc , int ) ;

extern void GRA_redraw_overlay( MCW_grapher * ) ;

extern void GRA_dplot_change_CB( Widget , XtPointer , XtPointer ) ;

extern void GRA_saver_CB( Widget , XtPointer , MCW_choose_cbs * ) ;
extern void GRA_file_pixmap( MCW_grapher * , char * ) ;

extern void GRA_fixup_xaxis( MCW_grapher * ) ;
extern void GRA_pick_xaxis_CB( Widget , XtPointer , MCW_choose_cbs * ) ;

extern void GRA_mapmenu_CB( Widget , XtPointer , XtPointer ) ;

extern void GRA_textgraph_CB( Widget , XtPointer , XtPointer ) ;  /* 22 Sep 2000 */
extern void GRA_baseline_CB ( Widget , XtPointer , XtPointer ) ;  /* 22 Sep 2000 */

/***-----------------------------------------------------------------------***/
#endif
