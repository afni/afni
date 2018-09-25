/*      This program displays functional EPI of the form 64x64 up to 512x512
        in X11 windows. Keys and subWindow are added. The ar is malloc().
        It uses 8 or 12 bitplanes and own solid color map, or any RGB mode.
        Extras: image marker, continuous X,Y position.
        EPI part based on Eric C. Wong program fd_multi.c .
        Added rectangular box and time average 11-12-2002 AJ.
        Andre Jesmanowicz, 4-03-2008, Medical College of Wisconsin. */

#define CONTRAST_CHANGE_STEP 15000/* larger step => slower change AJ 11.1.96 */

#define ASC_NUL '\0'
#undef  FILL_WITH_CYAN

#define MAIN

/***---------------- additions for use of array of MRI_IMAGE --------------***/

#include "FD2_inc.c"  /* 16 Jul 2007 -- to avoid use of mrilib */

#include "overfim.h"
#include "pcor.h"

#define INC_ALLIM 8

int dim_allim = 0 ;

MRI_IMAGE ** allim   = NULL ;
MRI_IMAGE ** t_allim = NULL ;  /* tmp pointer of allim when FFT done AJJ */

MRI_IMAGE * im_tmp_ar = NULL ;  /* the working array for displays, etc */
short * tmp_ar = NULL ;
#define SIZE_tmp_ar(n) im_tmp_ar->nx = im_tmp_ar->ny = (n)

short * nowim = NULL ;
int     nowsize ;

#define SAR(k)   MRI_SHORT_PTR(allim[k])   /* get pointer to data for image k */
#define T_SAR(k) MRI_SHORT_PTR(t_allim[k]) /* pointer to temp data of im k */
#define SIZ(k) (allim[k]->nx * allim[k]->ny)
#define DIM(k) (allim[k]->nx)

void add_extra_image() ;

MRI_IMAGE * RWC_pcim = NULL , * RWC_alpim = NULL ;

int RWC_autoscale = 0 ;  /* to autoscale image intensities */
int RWC_overhide  = 0 ;  /* to hide overlay in image */
int RWC_framehide = 0 ;  /* to hide frame in image */
int RWC_checker   = 0 ;  /* to checkerboard or not to checkerboard */
int RWC_groupbase = 0 ;  /* to use a group baseline or not */
int AJ_base = 0;         /* to set base to zero when group baseline is on */

/* additions for use of MCW logo */

#ifdef USE_MCW
   extern int mcw_load() ;  /* prototype */
   extern short mcw_im[] ;  /* image array */
#include <unistd.h>
#endif

/***-----------------------------------------------------------------------***/

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef   SYSV
#include <sys/fcntl.h>
#endif

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <sys/ioctl.h>

#define IM_HEIGHT 256
#define IM_MAX_H 512
int     im_height = IM_HEIGHT;        // minimum image size
#define NUM_STD_COLORS 16
#define IM_ARR    (IM_MAX_H*IM_MAX_H) // include 512x512 images too
#define NCOLORS   808  /* default # of colors */
#define MCOLORS   856  /* maximum # of colors: NCOLORS + NUM_STD_COLORS (16) */
                       /*                    + MAX_EXTRA_COLORS (32) */
#define N_SPCTR   240                 /* def degree of color spectrum */
#define M_SP_COL  360                 /* max degree of color spectrum */
#define BELT_W    24                  /* reference color belt width */
#define BELT_S    3                   /* color belt sides width */
#define BELT_A    (BELT_W*IM_MAX_H)
#define NF_MAX    10000               /* Max # of files */
#define STR_L     256                 /* Max length of string */

#define COL_MIN    0
#define MAX_WIDTH  2048  /* Never less then screen max_width */

#define EPX1      64                   /* List of supported EPI image sizes */
#define EPY1      64                   /* Each triple is: */
#define EPS1      (2*EPX1*EPY1)        /* xsize, ysize, filesize */

#define EPX2      128
#define EPY2      128
#define EPS2      (2*EPX2*EPY2)

#define EPX3      256
#define EPY3      256
#define EPS3      (2*EPX3*EPY3)

#define EPX4      32
#define EPY4      32
#define EPS4      (2*EPX4*EPY4)

#define EPX5      512
#define EPY5      512
#define EPS5      (2*EPX5*EPY5)

#if 0                                 // old stuff for max size 256x256 4.03.2008
  #define OFFSET    (28*IM_HEIGHT)       /* offset to data in 145408 bytes im */
  #define H_SIZE    (OFFSET+IM_ARR)      // 256x256 image with header
#endif

int     gx_max =  512;                 /* Horizontal size of graph window */
int     gy_max =  512;                 /* Vertical size of graph window */
#define GR_DLX    3                    /* Horizontal delta to right edge */
#define GT_DLY    21                   /* Vertical delta to top edge */
#define GL_DLX    50                   /* Horizontal delta to left edge */
#define GB_DLY    50                   /* Vertical delta to bottom edge */
#define MAT_MAX   25                   /* Maximum array size of graphs */
#define GRID_NUM  8                    /* Maximum grid index */
#define COL_NUM   5                    /* Number of colors */
#define GRID_COEF 50.                  /* alternate for slow scannings */

/**************************************************************************/

#define min_max_col(a) ((a) < (256) ? (256) : ((a) > (65280) ? (65280) : (a)))
#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) > (b) ? (a) : (b))

/* keys (small subwindows) stuff ----- vvvvvvvvv ------  AJ */

#define KFONT     "lucidasanstypewriter-bold-12"  /* defaults */
#define TFONT     "pellucidatypewriter10"
#define PADDINGW  3
#define PADDINGH  5
#define KEY_1_Y   50

/* fonts to try if the defaults fail */

static char * tfont ,
            * tfont_hopefuls[] = {
               "filled-in-by-default" ,
               "lucidasanstypewriter-10" ,
               "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1" ,
               "-misc-fixed-medium-r-normal--13-100-100-100-c-70-iso8859-1" ,
               "7x14" , "6x13" , "vtsingle" , "fixed" ,
               NULL } ;

struct _key {
           char   *st;
           int    code;
           int    (*fun)();
           Window wid;
           short  x,y,width,height;
  unsigned long   fore,back; // long is OK here AJJ
           void   (*func)();
            };

struct _key *key;

#define N_KEYS 21
#define LAST_K 8    /* RWC: incremented this to put FIM key in */

#define kROT   0
#define kHLP   1
#define kAVR   2
#define kDIF   3
#define kSIG   4
#define kFFT   5
#define kSCA   6
#define kFIM   7    /* RWC: put this in as new permanent button */
#define kNRM   8
#define kAV1   9
#define kAV2  10
#define kIR1  11
#define kIR2  12

#define kFI1  13   /* RWC: new buttons when FIM key is pressed */
#define kFI2  14
#define kFI3  15   /* the POWER KEY (named by Lloyd Estkowski) */

#define kFT1  16
#define kFT2  17
#define kFT3  18

#define kSC1  19
#define kSC2  20

int  Ims_rot(), Im_help(), Im_diff(), SCA_action(), Ref_im1(), Ref_im2();
int  Im_Aver(), Im_norm(), Av_im1(), Av_im2(), Smooth_line();

#define FFT_first_key kFT1
#define FFT_last_key  kFT3

int  FFT_action(), FFT_selection();
int  FFT_pressed = 0;
int  FT1_pressed = 0, FT2_stat = 0, FT3_stat = 0;
int  z_im1=0, z_imL=0;
char *key_kFFT_FFT   = "FFT" ;
char *key_kFFT_noFT  = "noFT";
char *key_kFT1[2]    = {"edit", "end"};
char *key_kFT2[4]    = {" FT ", "0..0", "from", " to "};
char *key_kFT3[3]    = {"    ", "i FT", "zero"};

#define SCA_first_key kSC1
#define SCA_last_key  kSC2
int   SCA_selection();
int   SCA_pressed = 0;
char  *key_kSCA[2]  = {"Scale", "ScEnd"};
float SCA_ref_val = -1.;
float SCA_ratio = 0.;

#define FIM_first_key kFI1
#define FIM_last_key  kFI3

int  FIM_action() , FIM_selection() ;
int  FIM_pressed = 0 , FIM_modified = 0 ;

char *FIM_selection_name[FIM_last_key-FIM_first_key+1] =
      { "Correlation Coefficient Threshold (0..1)"    , /* dialog box labels */
        "Vector Filename ('!nofim'==none)"            ,
        NULL                                          /* don't use dialog box */
      } ;

char *key_kFIM_FIM = "FIM" ;
char *key_kFIM_GO  = "GO!" ;

/*** DFILT stuff has been deactivated -- RWCox July 1995 ***/

#define DFILT_NONE   0                 /* no derivative filtering */
#define DFILT_TIME   1                 /* temporal mode */
#define DFILT_SIGMA  (4.0*0.42466090)  /* FHWM = 4.0 pixels */
#define DFILT_THRESH 0.05              /* threshold for using 'fit' */
#define DFILT_NREF   3
static char * DFILT_fimcode[2] = { "FIM" , "DFIM" } ;
static int DFILT_code = DFILT_NONE ;   /* flag to do DFILT */

#define MAX_TOTAL_REF (MAX_NUMORT+MAX_POLORT+DFILT_NREF+4)

#define LSQ_NONE     0                     /* no Least square remove in graphs */
#define LSQ_SUBORT   1                     /* remove Orts but not ideal */
#define LSQ_SUBALL   2                     /* remove Orts and Ideal */
#define LSQ_FITORT   3                     /* show fit of Orts */
#define LSQ_FITALL   4                     /* show fit of Orts and Ideal */
#define LSQ_SORFID   5                     /* remove orts, show Ideal fit */

#define LSQ_LASTCODE 5

#define LSQ_EDIT_NONE -1
#define LSQ_EDIT_ORT   1
#define LSQ_EDIT_ALL   0

static int LSQ_imedit[LSQ_LASTCODE+1] =
 { LSQ_EDIT_NONE, LSQ_EDIT_ORT, LSQ_EDIT_ALL, LSQ_EDIT_NONE, LSQ_EDIT_NONE, LSQ_EDIT_NONE } ;
static char * LSQ_fimcode[LSQ_LASTCODE+1] =
 { " " , "SUB:ort" , "SUB:all" , "FIT:ort" , "FIT:all" ,"SUB:ort FIT:ideal"} ;
static int LSQ_code = LSQ_NONE ;
static int LSQ_refcount = 0 ;
static time_series * LSQ_ref[MAX_TOTAL_REF] ;
static MRI_IMARR *   LSQ_fitim = NULL ;
static float * LSQ_fit[MAX_TOTAL_REF] ;

#define kFI3_NUM  11   /* number of options on the POWER KEY */

char *key_kFI3[kFI3_NUM] = { "ref =pixel ->" ,   /* labels for POWER KEY */
                             "ref+=pixel ->" ,
                             "smooth ref ->" ,
                             "thresh -+  ->" ,
                             "  NO FIM   ->" ,
                             "refplt -+  ->" ,
                             "save ref   ->" ,
                             "polort -+  ->" ,
                             "ort = ref  ->" ,
                             "clear orts ->" ,
                             "SubFit -+  ->"
                           } ;

char *key_kFI3_help[kFI3_NUM] = { "set reference function = central pixel" ,
                                  "add central pixel to reference function" ,
                                  "median (of 3) filter reference function" ,
                                  "- or + correlation coefficient threshold" ,
                                  "turn fimming off" ,
                                  "- or + number of reference function plots" ,
                                  "save reference function in a file" ,
                                  "- or + number of polynomial ort functions" ,
                                  "make current ref an ort instead" ,
                                  "clear all current ort functions" ,
                                  "set least squares removal in graphs"
                                } ;

int kFI3_status       = -1 ;
int kFI3_show_ref     = 1 ;
int kFI3_refsum_count = 0 ;

int kROT_doall      = 1 ;
char * key_kROT_all = "Rot" ;
char * key_kROT_one = "Rot1" ;

struct _key xtkeys[N_KEYS] = { {"Rot"  , kROT, Ims_rot},
                               {"Help" , kHLP, Im_help},
                               {"AvIm" , kAVR, Im_Aver},
                               {"Diff" , kDIF, Im_diff},
                               {"Smth" , kSIG, Smooth_line},
                               {"FFT"  , kFFT, FFT_action},
                               {"Scale", kSCA, SCA_action},
                               {"FIM"  , kFIM, FIM_action},
                               {"Norm" , kNRM, Im_norm},
                               {"Average: from this image" , kAV1 , Av_im1},
                               {"Average: to this one.   " , kAV2 , Av_im2},
                               {"Ref. level first image"   , kIR1 , Ref_im1},
                               {"Ref. level last  image"   , kIR1 , Ref_im2},
                               {"set threshold"  , kFI1 , FIM_selection},
                               {"ref/ort file "  , kFI2 , FIM_selection},
                               {"PUSH BUTTON 3"  , kFI3 , FIM_selection},
                               {"edit", kFT1 , FFT_selection},
                               {" FT ", kFT2 , FFT_selection},
                               {"    ", kFT3 , FFT_selection},
                               {"Reference  image",  kSC1 , SCA_selection},
                               {"Scale this image ", kSC2 , SCA_selection}
                             };

char          *kfont, *ffc, *fbc;
unsigned long ForeColor, BackColor, FKeyFore, FKeyBack; // long is OK here AJJ
XColor        xcsd, xced;
XFontStruct   *kfontinfo;
Font          keyfont;
int           keywide[N_KEYS], minwide, keyhigh;
GC            Fkeyigc, Fkeygc;
int           invkey    = -1;
int           exp_done[N_KEYS+3];   /* GWindow + subWindow +topWindow + keys */
short   int   a_rot[IM_ARR], rot_nr = 0, rot_direct , rot_state ;
int           sub_W_x, sub_W_y, top_W_x, top_W_y;
int           v_point_x = -20, v_point_y = 0;
int           diff_im = 0, Im_1, Im_2, im1_done = 0, extra_im = 0;
int           avr_grp = 0, Av_1, Av_2, av1_done = 0, Av_length = 1;
int           fim_dif = 0, fim_avr = 0, redraw;
int           txtW_ON = 0;


/* keys (small subwindows) stuff ----- ^^^^^^^^^ ------  AJ */

/*---------------------------------------------------------------------------*/
#ifdef NEED_DEFS
extern double   strtod();
char            *malloc();
char            *realloc();
#endif
/*---------------------------------------------------------------------------*/

int  STD_colors();

void x_events_loop();
void Resample();
void Put_image();
void plot_line();
void draw_marker();
void scale_up();
void scale_down();
void mat_up();
void mat_down();
void init_mat();
void grid_up();
void grid_down();
void print_plot();
void redo_graph_window();
void window_plane();
void graphic_store();
void plotx();
void plx_txt();
void plx_TXT();
void subW_TXT();
void line_color();
void txt_color();
void DrawSubWindow();
void DrawTopWindow();
int  FIM_edit_time_series() ;

int  c_f = 0;
int    st_8[] = {
     1,  2,  3,  4,  5,  6,  7,  8,  7,  6,  5,  4,  3,  2,  1,  0,
     2,  4,  6,  8, 10, 12, 14, 16, 14, 12, 10,  8,  6,  4,  2,  0,
     3,  6,  9, 12, 15, 18, 21, 24, 21, 18, 15, 12,  9,  6,  3,  0,
     4,  8, 12, 16, 20, 24, 28, 32, 28, 24, 20, 16, 12,  8,  4,  0,
     5, 10, 15, 20, 25, 30, 35, 40, 35, 30, 25, 20, 15, 10,  5,  0,
     6, 12, 18, 24, 30, 36, 42, 48, 42, 36, 30, 24, 18, 12,  6,  0,
     7, 14, 21, 28, 35, 42, 49, 56, 49, 42, 35, 28, 21, 14,  7,  0,
     8, 16, 24, 32, 40, 48, 56, 64, 56, 48, 40, 32, 24, 16,  8,  0,
     7, 14, 21, 28, 35, 42, 49, 56, 49, 42, 35, 28, 21, 14,  7,  0,
     6, 12, 18, 24, 30, 36, 42, 48, 42, 36, 30, 24, 18, 12,  6,  0,
     5, 10, 15, 20, 25, 30, 35, 40, 35, 30, 25, 20, 15, 10,  5,  0,
     4,  8, 12, 16, 20, 24, 28, 32, 28, 24, 20, 16, 12,  8,  4,  0,
     3,  6,  9, 12, 15, 18, 21, 24, 21, 18, 15, 12,  9,  6,  3,  0,
     2,  4,  6,  8, 10, 12, 14, 16, 14, 12, 10,  8,  6,  4,  2,  0,
     1,  2,  3,  4,  5,  6,  7,  8,  7,  6,  5,  4,  3,  2,  1,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 };

int    st_4[] = { 1,  2,  3,  4,  3,  2,  1,  0,      /* resampling array */
                  2,  4,  6,  8,  6,  4,  2,  0,
                  3,  6,  9, 12,  9,  6,  3,  0,
                  4,  8, 12, 16, 12,  8,  4,  0,
                  3,  6,  9, 12,  9,  6,  3,  0,
                  2,  4,  6,  8,  6,  4,  2,  0,
                  1,  2,  3,  4,  3,  2,  1,  0,
                  0,  0,  0,  0,  0,  0,  0,  0 };
int    st_2[] = { 1, 2, 1, 0,
                  2, 4, 2, 0,
                  1, 2, 1, 0,
                  0, 0, 0, 0 };

struct S_16_c {
              int red;
              int green;
              int blue;
              };
struct S_16_c Solid_color[16] = {
   {0xffff, 0xffff, 0xffff}, {0xffff, 0x0000, 0x0000},   /* white,  red     */
   {0x0000, 0xffff, 0x0000}, {0x0000, 0x0000, 0xffff},   /* green,  blue    */
   {0x0000, 0xffff, 0xffff}, {0xffff, 0x0000, 0xd3d3},   /* cyan,   magenta */
   {0xffff, 0xffff, 0x0000}, {0xffff, 0x8a8a, 0xffff},   /* yellow, brown   */
   {0x0000, 0x7000, 0x0fff}, {0x0000, 0xffff, 0x9f9f},   /* green,  green   */
   {0x0000, 0x8a8a, 0xffff}, {0x9494, 0x0000, 0xd3d3},   /* blue,   violet  */
   {0xffff, 0x0000, 0x9494}, {0x6969, 0x6969, 0x6969},   /* pink,   gray    */
   {0xaeae, 0xaeae, 0xaeae}, {0x0000, 0x0000, 0x0000}    /* gray,   black   */
  };

int       color_x11[16] = { -1, -1, -1, -1, -1, -1, -1, -1,
                            -1, -1, -1, -1, -1, -1, -1, -1 };

XPoint          sm_cir[12] = { {-1,-2},{ 0,-2}, { 1,-2},  /* small circle */
                               { 2,-1},{ 2, 0}, { 2, 1},
                               { 1, 2},{ 0, 2}, {-1, 2},
                               {-2, 1},{-2, 0}, {-2,-1} };
char            *f_name[NF_MAX];
char            *ProgramName = NULL;
char            *Xdef_Name   = "FD" ;      /* name to look for resources under */
char            *display, *geom, **ptr;

int             FIRST = 1, SQUE_NR;
int             x00 = 0, y00 = 0;
int             back_to_main = 0;
XEvent          event, w_event, first_event;

Display         *theDisp;
int             theScreen;
Window          rootW, theWindow, GWindow, subWindow, topWindow;
GC              theGC, txtGC;
unsigned long   fcol, bcol; // long is OK here AJJ
Font            mfont;
XFontStruct     *afinfo, *mfinfo;
Visual          *theVisual;
XImage          *theImage, *expImage, *theBelt, *expBelt;
int             eWIDE, eHIGH, aWIDE, eW, eH;

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

void RWC_setup_fims() ;

int    FIM_opt_colors = 0 ;               /* these are for the data */
float  FIM_opt_thr[MAX_FIM_COLORS] ;      /* from the -fim_colors option */
char * FIM_opt_pos[MAX_FIM_COLORS] ,
     * FIM_opt_neg[MAX_FIM_COLORS] ;

int check_color() ;

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

unsigned short  tmp1[NCOLORS], tmp2[NCOLORS], tmp3[NCOLORS];
int             No_Color_init = 1, No_Grey_init = 1;
int             Dispcells, Planes;
int             repeat_status = 0, ncolors, YES_color = 0, INgrey[NCOLORS];
XColor          MYcol[NCOLORS], MYgrey[NCOLORS], any_col, rgb_col;
Colormap        CMap;
unsigned long   plane_masks[1], pixels[MCOLORS]; // long is OK here AJJ
unsigned int    nplmsk = 0, ucolors, uucolors;
Pixmap          pxWind;

int             max_xlines = 0 ;

int             spectrum = N_SPCTR; /* up to 360 degree of colors */
int             Im_Nr = 0, N_im = 0, I_lck = 0, tmp_Nr = 0;
int             N_lck = 0,  old_im = -1;
int             x_mag, ref_ar[IM_ARR], av_ar[IM_ARR], Cx, Cy;
short int       imx[IM_ARR], tmp_imx[IM_ARR] ;
short int       belt_ar[BELT_A], belt_arr[BELT_A];
int             Mltx[MAX_WIDTH], Mlty[MAX_WIDTH];
XPoint          a_line[NF_MAX];

float           del1, coef1, auto_scale;
int             min1;
int             fsize, isize, ar_size;
int             NC;
int             nc_option = 0 ;
char            T_name[STR_L], I_name[STR_L], G_name[STR_L];
int             B1_action = 0, Im_frst = 0, fr_index = -7;

int             Argc;
char            **Argv = NULL;

/* these are my own ... EW */

/* int     plot[MAT_MAX][MAT_MAX][NF_MAX],val[MAT_MAX][MAT_MAX][NF_MAX]; */

int     * plot[MAT_MAX][MAT_MAX] , * val[MAT_MAX][MAT_MAX] ;
int     * LSQ_val , * LSQ_plot ;

int     matx, maty, matx_0 = -1, maty_0 = -1;
int     pmin[MAT_MAX][MAT_MAX],pmax[MAT_MAX][MAT_MAX],xc,yc,mark;
int     xorigin[MAT_MAX][MAT_MAX],yorigin[MAT_MAX][MAT_MAX];
int     i,j,xpoint,ypoint,npoints,iscale,gx,gy;
int     xspace,yspace,grid_index, color_index;
char    plotbuf[10*NF_MAX],pfname[80],fnum[80];
int     xpoint_0 = -1, ypoint_0 = -1; /* old xpoint & ypoint */
float   grid_far[2*GRID_NUM], grid_coef = 1.;

int           mytxt, idX, idY;
int           mdx1, mdy1;
char          strp[STR_L], strF[STR_L], strT[STR_L];
char          *color[5] = {"cyan","red","yellow","green","cornflowerblue"};

int           im_size, offs;
float         gamm;

/* Stuff for gausian smoothing of time course lines */
#define MAX_SMOOTH 301
int           AJ_nr, AJ_off, i_plot[NF_MAX];
float         AJ_sigma, AJ_norm;
float         AJ_gauss[MAX_SMOOTH];
float         f_plot[NF_MAX+MAX_SMOOTH];

/* FFT stuff */
char      *formt[4][4] = {
          {     "%s.%d",      "%s.%02d",      "%s.%03d",      "%s.%04d"},
          {  "%s_%d.%d",   "%s_%d.%02d",   "%s_%d.%03d",   "%s_%d.%04d"},
          {"%s_%02d.%d", "%s_%02d.%02d", "%s_%02d.%03d", "%s_%02d.%04d"},
          {"%s_%03d.%d", "%s_%03d.%02d", "%s_%03d.%03d", "%s_%03d.%04d"} };

void          csfft();
int           t_points, t_N_im; /* keep npoints & N_im FFT done AJJ */
float         t_coef1;
int           t_min1;
complex       *c_arr = NULL;    /* order: [pixel][time] AJJ */
complex       *r_arr = NULL;    /* for reference line */
int           FT_dim;           /* size of single FT */
int           FT_size;          /* size of all FT */
int           FT_disp;          /* size of displayed FT elements */
int           FT_done = 0;
int           FT_grid = 0;      /* = GRID_NUM for FFT graph */
int           FT_graph_on = 0;  /* FT graph is on */
int           grid_timed = 0;
struct _undo_buf {
                 int   im;
                 int   pix;
                 float r;
                 float i;
                 float r2;
                 float i2;
                 };
struct _undo_buf *undo_buf = NULL;
struct _undo_buf *undo_ref = NULL; /* for reference line */
int           act_undo = -1, ref_undo = -1;
char          FT_name[100];
int           im_f, phase = 0; 
#define FFT_MAG .2
float         fft_mag = FFT_MAG; /* fft amplitude magnify factor AJJ */
float         *T_ref; /* tmp pointer of LSQ_ref[0]->ts when FFT done AJJ */

float         *avr_A = NULL;     /* array of average over several pixels */
int           avr_nr = 0; /* for time course average over several pixels AJ */
int           cancell_FT = 0;
/* ------ vvvvv ------ RGB mode 10.17.2001 AJ ----- vvvvv ------ */
XImage        *Load_Any_Arr();
XImage        *Load_Any_ind();
XImage        *Load_Any_RGB();
void          Load_Next_Arr();
void          Load_Next_ind();
void          Load_Next_RGB();
void          AJ_StoreColors();
int           AJ_init_RGB();
void          Make_RGB_lookup();
static int    highbit();
void          Syntax();
void          The_Help();
void          init_grid();
void          get_line_args();
void          init_const();
void          make_belt();
void          load_rect_str();
void          main_FD_EPI();
void          redraw_graph();
void          swap_2();
void          swap_4();
void          top_nr_name();
void          FatalError();
void          AJ_make_STDcol();
void          colmap_init();
void          CreateMainWindow();
void          New_Cursor();
void          MResize();
void          Allow_smaller_im();
void          HandleEvent();
void          DrawKey();
void          discard();
int           save_all_images();
void          c_swap();
int           save_act_im();
int           save_avr_array();
int           get_fft_mag();
int           read_new_im();
int           kill_curr_im();
void          Track_Cursor();
void          c_squeeze();
void          c_bright();
void          c_rotate();
void          Track_Vpointer();
void          InvertKey();
void          discard_Key();
void          color_init();
void          LetGoKey();
void          color_rotate();
void          grey_init();
void          grey_rotate();
void          color_swap();
void          color_squeeze();
void          grey_contrast();
void          color_bright();
void          grey_change();
void          Allow_new_name();
void          Cpointer_PIXWIN();
void          CreateGraphWindow();
void          Setup_subWindow();
void          Setup_topWindow();
void          Setup_keys();
void          Allow_smaller_gr();
void          Vpointer();
void          Cpointer();
void          take_file_name();
int           dec_indx();
void          draw_frame();
void          erase_graph();

typedef struct { unsigned short r;
                 unsigned short g;
                 unsigned short b; } AJ_rgb_str;
int           AJ_PseudoColor = 1; /* as original pseudocolor max 12 bpp FD2 */
                                  /* reg. colors 0-207, 208-223 std colors, */
                                  /* 224-255 fim colors */
AJ_rgb_str    AJ_rgb[MCOLORS];    /* local lookup RGB array */
unsigned int  AJ_RGB[MCOLORS];    /* local lookup machine color packed array */
int           bperpix=8, border;
int           STD_indx[NUM_STD_COLORS + MAX_EXTRA_COLORS];
int           Time_avr = 1;       /* for time course or frame box average */
int           swap_bytes = 0;     // in place of macro #ifdef LINUX

/****************************************************************************/

/* ---------------- */
   int
   main(argc, argv)
   int  argc;
   char *argv[];
/* ---------------- */
{
   int    i, j, k, m, max1;
   char * xdef ;

   Argc = argc;
   Argv = argv;
   NC = 64;

   ProgramName = argv[0];  if( Xdef_Name == NULL ) Xdef_Name = ProgramName ;

   if (argc <  2)  { Syntax(); The_Help(1); }
   display = NULL;
   npoints = 0;
   gamm = 1.4 ;
   LSQ_ref[0] = NULL ;
   init_grid();

   get_line_args(argc, argv);       /* get command line arguments and files */

/*** ---- allocate memory for plot[][] and val[][] ---- ***/

   m = sizeof(int) * npoints ;

   LSQ_plot = (int *) malloc( m ) ;
   LSQ_val  = (int *) malloc( m ) ;
   if( LSQ_plot == NULL || LSQ_val == NULL ){
      fprintf(stderr,"\n*** failure in malloc for graphs! ***\a\n");
      exit(-1);
   }

   for( i=0 ; i < MAT_MAX ; i++ ){
      for( j=0 ; j < MAT_MAX ; j++ ){
         plot[i][j] = (int *) malloc( m ) ;
         val[i][j]  = (int *) malloc( m ) ;

         if( plot[i][j] == NULL || val[i][j] == NULL ){
            fprintf(stderr,"\n*** failure in malloc for graphs! ***\a\n");
            exit(-1);
         }
      }
   }

/* ---------- Open the display. --------- */

   if ( (theDisp=XOpenDisplay(display)) == NULL) {
     fprintf(stderr, "%s: Can't open display (are X11 windows running ?). AJ\a\n"
             ,ProgramName);
     exit(1);
   }

   if( !nc_option ){
      xdef = XGetDefault(theDisp,Xdef_Name,"NColors") ;
      if( xdef != NULL ){
         i = strtol(xdef,NULL,0) ;
         if( i > 4 ) NC = i ;
      }
   }

/* set some image dimensions for everybody to use */

   if      ( fsize == EPS1) {
      im_size = 64;
      x_mag = 4;  /* 64x64 pixels format => mag = 4 */
   }
   else if (fsize == EPS2) {
     im_size = 128;
     x_mag = 2;  /* 128x128 pixels format */
   }
   else if (fsize == EPS3) {
     im_size = 256;
     x_mag = 1;  /* 256x256 pixels format */
   }
   else if (fsize == EPS4) {
     im_size = 32;
     x_mag = 8;  /* 32x32 pixels format */
   }
   else if (fsize == EPS5) {
     im_size = 512;
     x_mag = 1;  /* 512x512 pixels format */
   }
//printf(" AJ fsize: %d [%d], x_mag: %d\n", fsize, EPS5, x_mag);
   ar_size = fsize / 2 ;

   init_const();

   min1 =  32767;    /* find deviation fot central frame and set scale */
   max1 = -32768;
   i = ypoint*im_size + xpoint;
   for (k=0; k < npoints; k++) {
      if ( SAR(k)[i] < min1 ) min1 = SAR(k)[i];
      if ( SAR(k)[i] > max1 ) max1 = SAR(k)[i];
   }
   del1 = max1 - min1;
   if ( (int) del1 > 1) {
      k = (int) (log(del1*auto_scale)/log(2.) + .5) - iscale;
      for (i=0; i < abs(k); i++) {       /* scale graph */
         if ( k < 0 ) scale_up();
         else         scale_down();
      }
   }

   min1 =  32767;
   max1 = -32768;
   for (k=0; k < npoints; k++) {    /* find global min and max for main set */
      nowim = SAR(k) ;
      for (i=0; i < ar_size; i++) {
         if ( nowim[i] < min1 ) min1 = nowim[i] ;
         if ( nowim[i] > max1 ) max1 = nowim[i] ;
      }
   }
   del1 = max1 - min1;
   if (del1 < 1.) del1 = 1.;
   coef1 = ( (float) NC - 1.) / del1;

   make_belt(belt_ar, BELT_W, im_height);     /* make reference color belt */
   load_rect_str(belt_ar, belt_arr, BELT_W, im_height,
                 BELT_S, 15, BELT_S, 15, 0, NC);

   main_FD_EPI();          /* Make image in the window and return Drawable */

   /** if -fim_colors was specified, check its colors for legality now **/

   if( FIM_opt_colors > 0 ){
      for( i=0 ; i < FIM_opt_colors ; i++ ){
         if( !check_color( FIM_opt_pos[i] ) )
           fprintf(stderr,"\n*** color %s invalid\n",FIM_opt_pos[i]) ;

         if( !check_color( FIM_opt_neg[i] ) )
           fprintf(stderr,"\n*** color %s invalid\n",FIM_opt_neg[i]) ;
      }
   }

   /* Initialize extra E.C.W. graph stuff here.  AJ */

   window_plane();  /* create graphic window with extra subwindows and keys */

   redraw_graph();                       /* draw frame and text in it */
   DrawSubWindow();
   DrawTopWindow();

   Put_image(0) ;                     /* put the first image */
   XRaiseWindow(theDisp,theWindow) ;  /* bring image to top  */


   x_events_loop();                              /* Main events check loop */

   exit(0);

}       /* --- End of main() --- */



/* -------- */
   void
   Syntax()
/* -------- */
{
  fprintf (stderr, "\n Functional Display (32x32, 64x64, 128x128, 256x256) in X11 window.");
  fprintf (stderr, "\n EPI images in 256x256 Signa format are accepted too.");
  fprintf (stderr, "\n It displays EPI time or frequency course window.");
  fprintf (stderr, "\n\n Usage: %s [options] image1, [image2, ..., image%d]\n", ProgramName, NF_MAX);
  fprintf (stderr, "\n Where options are:");
  fprintf (stderr, "\n    -d display       - X11 display");
  fprintf (stderr, "\n    -geom geometry   - initial geometry");
  fprintf (stderr, "\n    -nc #_of_colors  - initial number of colors [2-%d] (def %d)", 200, NC);
  fprintf (stderr, "\n    -sp #_of_degree  - range of color spectrum [0-%d] degree (def %d)", M_SP_COL, N_SPCTR);
  fprintf (stderr, "\n    -gam gamma       - gamma correction (1 for no correction)");
  fprintf (stderr, "\n    -num #_of_images - # of images in time course [2-%d].", NF_MAX);
  fprintf (stderr, "\n    -im1 image_#     - first image in time course. Previous images will be ");
  fprintf (stderr, "\n                       filled with this one for proper timing of others.");
  fprintf (stderr, "\n    -ideal ref_file  - use ref_file for fim-like calculations" ) ;
  fprintf (stderr, "\n    -pcthresh #      - use # as threshold for correlation" ) ;
  fprintf (stderr, "\n    -extra           - files after this are not used in time course" );
  fprintf (stderr, "\n                       (used instead of -num option)" );
  fprintf (stderr, "\n    -fim_colors L thr1 pos1 neg2 ... thrL posL negL" );
  fprintf (stderr, "\n                     - set up L thresholds and colors for FIM overlay");
  fprintf (stderr, "\n    -gsize x y       - set graph window size to x by y pixels");
  fprintf (stderr, "\n    -fmag val        - magnify scale of FFT by val");
  fprintf (stderr, "\n    -grid val        - initial grid separation ");
  fprintf (stderr, "\n    -phase           - image has negative values (for FFT)");
  fprintf (stderr, "\n    -cf              - center image to the frame");
  fprintf (stderr, "\n    -swap            - byte-swap data (default is no)");
  fprintf (stderr, "\n                       *** this is a new default!");
  fprintf (stderr, "\n");
}

/* ------------ */
   void
   The_Help(ex)
   int ex;
/* ------------ */
{
  fprintf (stderr, "\n Events:\n");
  fprintf (stderr,"\n  Program quit      : <q> or <Q>");
  fprintf (stderr,"\n  Change to colors  : <C>");
  fprintf (stderr,"\n  Change to B & W   : <B>");
  fprintf (stderr,"\n  Swap colors       : <s>");
  fprintf (stderr,"\n  Restore colors    : Button_3 at image center ");
  fprintf (stderr,"\n  Squeeze colors    : #2 or #3 button - right side of image");
  fprintf (stderr,"\n  Expand  colors    :                   left  side of image");
  fprintf (stderr,"\n  Circ. color bar   : #2 or #3 button at the color bar");
  fprintf (stderr,"\n  Color saturation  : #2 or #3 button - the top or bottom");
  fprintf (stderr,"\n  Exact image number: press <I>, enter_number, <CR>");
  fprintf (stderr,"\n  First image       : 1");
  fprintf (stderr,"\n  Last image        : l");
  fprintf (stderr,"\n  Next     image    : >");
  fprintf (stderr,"\n  Previous image    : <");
  fprintf (stderr,"\n                      dragging red pointer works too");
  fprintf (stderr,"\n  Scale Plot up         : +");
  fprintf (stderr,"\n  Scale Plot down       : -");
  fprintf (stderr,"\n  Increase Grid Spacing : G");
  fprintf (stderr,"\n  Decrease Grid Spacing : g");
  fprintf (stderr,"\n  Toggle Grid and Colors: r");
  fprintf (stderr,"\n  Toggle Frame colors   : R");
  fprintf (stderr,"\n  Toggle time or box avr: t");
  fprintf (stderr,"\n  Increase matrix size  : M");
  fprintf (stderr,"\n  Decrease matrix size  : m");
  fprintf (stderr,"\n  Increase matrix y size: V");
  fprintf (stderr,"\n  Decrease matrix y size: v");
  fprintf (stderr,"\n  Exact matrix size     : N #of_size <CR> (1 to %d only)", MAT_MAX);
  fprintf (stderr,"\n  Save minigraph in ASCII file   : press <p>");
  fprintf (stderr,"\n    [with xxx_yyy.suffix filename] press <w>");
  fprintf (stderr,"\n  Save current image to a file   : press <S>");
  fprintf (stderr,"\n  Save averaged image (not norm) : press <X>");
  fprintf (stderr,"\n  Position frame in the image    : press Button_1 in the image area,");
  fprintf (stderr,"\n                                    drag cursor, and release button.");
  fprintf (stderr,"\n  Center frame on desired pixel  : press Button_1 over desired minigraph.");
  fprintf (stderr,"\n  Rotate image 90 deg. clockwise : press Button_3 in [Rot] window.");
  fprintf (stderr,"\n                counterclockwise : press Button_1 in [Rot] window.");
  fprintf (stderr,"\n  Change to differential display : press [Diff] window. Set first and");
  fprintf (stderr,"\n                                   last image for averaged reference.");
  fprintf (stderr,"\n  Average of set of images       : press [AvIm] (can be used in Diff mode).");
  fprintf (stderr,"\n  Compute FIM overlay            : press [FIM], choose ref file,threshold,");
  fprintf (stderr,"\n                                   then press [GO]");
  fprintf (stderr,"\n") ;
  fprintf (stderr,"\n  Last image in time course      : L") ;
  fprintf (stderr,"\n  Toggle autoscale of images     : A") ;
  fprintf (stderr,"\n  Hide FIM overlay               : H") ;
  fprintf (stderr,"\n  Hide frame in image            : h") ;
  fprintf (stderr,"\n  Toggle overlay checkerboard    : O") ;
  fprintf (stderr,"\n  Read image into program        : F (for file)") ;
  fprintf (stderr,"\n  Remove image from program      : K (for kill)") ;
  fprintf (stderr,"\n  Move to image 1..9             : 1,2,...9") ;
  fprintf (stderr,"\n  Toggle common graph baselines  : b") ;
  fprintf (stderr,"\n  Toggle baseline to zero        : x") ;
  fprintf (stderr,"\n");
  fprintf (stderr,"\n  Add/[subtract] 3600 from pixel : D / [d]");
  fprintf (stderr,"\n  In FT edit mode: ");
  fprintf (stderr,"\n                increase value   : Arrow Up");
  fprintf (stderr,"\n                decrease value   : Arrow Down");
  fprintf (stderr,"\n          Shift or Control Arrow : larger changes ");
  fprintf (stderr,"\n                undo last change : u");
  fprintf (stderr,"\n                undo all changes : U");

#ifdef USE_MCW
  fprintf (stderr,"\n  Show MCW logo              : 0") ;
#endif
  fprintf (stderr,"\n\n ");

  if (ex == 1) exit(0);                 /* 1 -> 0   18 Sep 2018 [rickr] */
}

/* ------------------------- */
   void
   get_line_args(argc, argv)
   int  argc;
   char *argv[];
/* ------------------------- */
{
   register int i, j, k, nopt, nnn;
   int          sp;
   float        fff;

   MRI_IMAGE * imtemp ;
   int input_conversion = 0 ;
   int nextra = -1 ;

   nopt = 0;
   for (i = 1; i < argc; i++) { /* ------- Options ------- */

      if (!strncmp(argv[i], "-d", 2)) {          /* display */
         if (++i >= argc) { Syntax (); exit(1); }
         display = argv[i];
         nopt++; nopt++;
         continue;
      }
      if (!strncmp(argv[i], "-geo", 4)) {          /* initial geometry */
         if (++i >= argc) { Syntax (); exit(1); }
         geom  = argv[i];
         nopt++; nopt++;
         continue;
      }
      if (!strncmp(argv[i], "-h", 2)) {          /* help */
         Syntax();
         The_Help(1);
      }
      if (!strncmp(argv[i], "-nc", 3)) {         /* # of colors */
         if (++i >= argc) { Syntax (); exit(1); }
         nnn = atoi(argv [i]);
         if (nnn < 2 || nnn > NCOLORS ) { Syntax(); exit(1); }
         NC = nnn;  nc_option = 1 ;
         nopt++; nopt++;
         continue;
      }
      if (strncmp(argv [i], "-sp", 3) == 0) {
         if (++i >= argc) { Syntax(); exit(1); }
         ptr = argv;
         sp = strtod(argv[i], ptr);
         if ( **ptr || (sp < 1) || (sp > M_SP_COL) ) {
            fprintf (stderr, "\n !!! Wrong degree value: -sp %d !!!\a\n",sp);
            fprintf (stderr, "\n !!! For help type: %s -h \a\n\n", ProgramName);
            exit(3);
         }
         spectrum = sp;
         nopt++; nopt++;
         continue;
      }
      if (strncmp(argv [i], "-fmag", 5) == 0) {
         if (++i >= argc) Syntax ();
         ptr = argv;
         fff = strtod(argv[i], ptr);
         if ( **ptr || (fff <= 0.) ) {
            fprintf (stderr, "\n !!! Wrong fft magnify value: %g !!!\n\n",fff);
            Syntax();
         }
         fft_mag = fff * FFT_MAG;
         nopt++; nopt++;
         continue;
      }
      if (strncmp(argv [i], "-gam", 4) == 0) {
         if (++i >= argc) Syntax ();
         ptr = argv;
         fff = strtod(argv[i], ptr);
         if ( **ptr || (fff <= 0.) ) {
            fprintf (stderr, "\n !!! Wrong gamma value: %g !!!\a\n\n",fff);
            exit(3) ;
         }
         gamm = fff;
         nopt++; nopt++;
         continue;
      }
      if (strncmp(argv [i], "-num", 4) == 0) {
         if (++i >= argc) { Syntax(); exit(1); }
         ptr = argv;
         npoints = strtod(argv[i], ptr) + .5;
         if ( **ptr || (npoints < 1)) {
            fprintf (stderr, "\n !!! Too few images specified !!!\a\n\n");
            exit(1); /* now symbolic for min npoints = 1 . AJ */
         }
         nopt++; nopt++;
         continue;
      }
      if (strncmp(argv [i], "-pha", 4) == 0) {
	 phase = 1;
         nopt++;
         continue;
      }
      if (strncmp(argv [i], "-cf", 3) == 0) {
	 c_f = 1;
         nopt++;
         continue;
      }
      if (strncmp(argv [i], "-grid", 4) == 0) {
         if (++i >= argc) { Syntax(); exit(1); }
         ptr = argv;
         fff = strtod(argv[i], ptr);
         if ( **ptr || (fff < .1)) {
            fprintf (stderr,
            "\n !!! grid spacing too small [< .1]: %g !!!\a\n\n", fff);
            exit(1);
         }
         if ( fff < 2 ) grid_coef = GRID_COEF;
         else           grid_coef = 1.;
         grid_index = 0;
         grid_far[0] = fff;
         grid_far[1] = 2.*fff;
         grid_far[2] = 5.*fff;
         grid_timed = 1;
         nopt++; nopt++;
         continue;
      }
      if (strncmp(argv [i], "-im1", 4) == 0) {
         if (++i >= argc) { Syntax(); exit(1); }
         ptr = argv;
         Im_frst = strtod(argv[i], ptr) + .5;
         if ( **ptr || (Im_frst < 1)) {
            fprintf (stderr, "\n !!! First_image_# < 1 in -im1 !!!\a\n\n");
            exit(1);
         }
         nopt++; nopt++;
         continue;
      }
      if (strncmp(argv [i], "-mag", 4) == 0) {
         fprintf (stderr, "\n !!! No more -mag option in use !!!");
         fprintf (stderr, "\n !!! Resize window using mouse  !!!\a\n\n");
         if (++i >= argc) { Syntax(); exit(1); }
         nopt++; nopt++;
         continue;
      }
      /* allow the user to specify swapping bytes */
      if (strncmp(argv [i], "-swap", 4) == 0) {
         swap_bytes = 1;
         nopt++;
         continue;
      }

/***************************************************************************/

     if( strncmp(argv[i],"-pcthresh",4) == 0 ){
        if( ++i >= argc ) { Syntax(); exit(1); }
        RWC_pcthresh = strtod( argv[i] , NULL ) ;
        if( RWC_pcthresh <= 0.0 || RWC_pcthresh >= 1.0 ){
           fprintf(stderr,"-pcthresh %11.4g is illegal!\a\n",RWC_pcthresh) ;
           exit(1);
        }
        nopt++ ; nopt++ ;
        continue ;
     }

     if( strncmp(argv[i],"-ideal",4) == 0 ){
        if( RWC_ideal != NULL ){
           fprintf(stderr,"cannot have 2 -ideal options!\a\n") ;
           exit(1) ;
        }
        if( ++i >= argc ) { exit(1); }

        RWC_ideal = RWC_read_time_series( argv[i] ) ;

        if( RWC_ideal == NULL ){
           fprintf(stderr,"cannot read -ideal %s\a\n",argv[i]) ;
           exit(1);
        }
        RWC_do_overfim = 1 ;
        nopt++ ; nopt++ ;
        continue ;
     }

     if( strncmp(argv[i],"-extra",3) == 0 ){
        if( nextra > 0 ){
           fprintf(stderr,"cannot have 2 -extra options!\a\n") ;
           exit(1) ;
        }
        nextra = i ;  /* do NOT count this in nopt! */
        continue ;
     }

     if( strncmp(argv[i],"-gsize",4) == 0 ){
        if( i+2 >= argc ){
           fprintf(stderr,"\n*** nothing follows -gsize!\a\n");
           exit(1) ;
        }
        gx_max = strtod( argv[++i] , NULL ) ;
        gy_max = strtod( argv[++i] , NULL ) ;
        if( gx_max <  100 || gy_max <  100 ||
	    gx_max > 1024 || gy_max > 1024   ){
           fprintf(stderr,"\n*** illegal values following -gsize!\n");
           exit(1) ;
        }
        nopt += 3 ;
        continue ;
     }

     if( strncmp(argv[i],"-fim_colors",8) == 0 ){
        int   ival , ii , good ;
        float fval ;

        if( FIM_opt_colors > 0 ){
           fprintf(stderr,"\n*** cannot have 2 -fim_colors options!\a\n") ;
           exit(1) ;
        }
        if( ++i >= argc ){
           fprintf(stderr,"\n*** nothing follows -fim_colors!\a\n") ;
           exit(1) ;
        }

        ival = strtol( argv[i] , NULL , 10 ) ;
        if( ival <= 0 || ival > MAX_FIM_COLORS ){
           fprintf(stderr,"\n*** illegal value for -fim_colors: %d\a\n",ival) ;
           exit(1) ;
        }
        FIM_opt_colors = ival ;
        if( i + 3*FIM_opt_colors >= argc ){
           fprintf(stderr,"\n*** not enough values follow -fim_colors!\a\n");
           exit(1) ;
        }

        good = 1 ;
        for( ii=0 ; ii < FIM_opt_colors ; ii++ ){
           fval = strtod( argv[++i] , NULL ) ;
           if( fval <= 0.0 || fval >= 1.0 ){
              fprintf(stderr,"\n*** illegal fim_colors threshold %s\a\n",argv[i]);
              good = 0 ;
           }
           FIM_opt_thr[ii] = fval ;      /* store threshold and color strings */
           FIM_opt_pos[ii] = argv[++i] ;
           FIM_opt_neg[ii] = argv[++i] ;
        }
        if( !good ){
           fprintf(stderr,"\n*** cannot continue from fim_colors errors!\n");
           exit(1) ;
        }
        nopt += 3 * FIM_opt_colors + 2 ;
        continue ;
     }

/***************************************************************************/

   }
   nopt++;                                   /* Files to read (minimum one) */

   if( nextra > 0 && nextra <= nopt ){
      fprintf(stderr,"\n*** -extra option too early!\a\n"); exit(1) ;
   }

   if( nextra > nopt && npoints > 0 ){
      fprintf(stderr,"\n*** -extra option conflicts with -num option!\a\n");
      exit(1) ;
   }

   if ( nopt > (argc-1) || nopt < (argc - NF_MAX) ) {  /* Nr of files check */
      fprintf (stderr, "\n Wrong # of files. %d files entered :\a\n", argc-nopt);
      for(i=nopt, j=1; i < argc; i++, j++)
         fprintf (stderr, "  %3d -  %s\n", j, argv[i]);
      exit(1);
   }

   N_im = argc-nopt;                                /* # of images */

   if( nextra > nopt ){
      N_im -- ;                                     /* -1 for -extra option */
      npoints = nextra - nopt ;                     /* no. points in series */
   } else if( npoints == 0 || npoints > N_im ){
      npoints = N_im ;
   }

   if( N_im < 1 || N_im > NF_MAX ){
      fprintf (stderr, "\n Wrong # of files. %d files entered :\a\n", argc-nopt);
      exit(1) ;
   }

   if ( Im_frst > N_im ) {
      fprintf (stderr, "\n!!! First_im_# in -im1 is bigger than number of images!!!\a\n");
      exit(1);
   }

   for( i=0,k=0 ; i < N_im ; i++,k++){
      if( strncmp(Argv[k+nopt],"-extra",3) == 0 ) k++ ;  /* skip -extra */
      f_name[i] = Argv[k+nopt] ;
   }
   if( N_im == 1 ){
      N_im = npoints = 2 ;
      f_name[1] = malloc( strlen(f_name[0]) + 1 ) ;
      strcpy( f_name[1] , f_name[0] ) ;
   }

   for (i=0; i < Im_frst-1; i++) f_name[i] = f_name[Im_frst-1];

   dim_allim = N_im + INC_ALLIM ;  /* allow for some extra images */

   allim = (MRI_IMAGE **) malloc( sizeof(MRI_IMAGE *) * dim_allim ) ;
   if( allim == NULL ){
      fprintf(stderr,"\n*** cannot malloc allim\a\n") ;
      MRI_FATAL_ERROR ;
   }

   /* read and check the length of the first file for validity */

   imtemp = mri_read_nsize( f_name[0] ) ;

   /* swap based on command-line now                   27 Aug 2004 [rickr] */

   if ( swap_bytes && imtemp->kind == MRI_short ) {
     swap_2(MRI_SHORT_PTR(imtemp), imtemp->nx*imtemp->ny*2);
   }
   else if ( swap_bytes && imtemp->kind == MRI_float ) {
     swap_4(MRI_FLOAT_PTR(imtemp), imtemp->nx*imtemp->ny*4);
   }

   if( imtemp == NULL ) exit(-1) ;
   if( imtemp->kind == MRI_short ){
      allim[0] = imtemp ;
   } else {
      allim[0] = mri_to_short( 0.0 , imtemp ) ;
      mri_free( imtemp ) ;
      mri_add_name( f_name[0] , allim[0] ) ;
      input_conversion++ ;
   }

   isize = 2 * allim[0]->nx * allim[0]->ny ;
   if ( (isize != EPS1) && (isize != EPS2)
     && (isize != EPS3) && (isize != EPS4) && (isize != EPS5) ) {
      fprintf (stderr, "\n\n !!! File %s has illegal dimensions !!!\a\n", f_name[0]);
      exit(-1);
   }

   if ( allim[0]->ny > 256 ) im_height = 512; // new for 512 image size
   im_tmp_ar = mri_new( im_height , im_height , MRI_short ) ; // max size
   tmp_ar    = mri_data_pointer( im_tmp_ar ) ;

   fsize   = isize;
   ar_size = fsize / 2 ;

   printf("\nReading files ") ; fflush(stdout) ;

   for( i=1 ; i < N_im ; i++ ){

      if( i%10 == 9 ){ printf(".") ; fflush(stdout) ; }

      imtemp = mri_read_nsize( f_name[i] ) ;

      /* we already know whether to swap     26 Aug 2004 [rickr] */
      if ( swap_bytes && imtemp->kind == MRI_short ) {
        swap_2(MRI_SHORT_PTR(imtemp), imtemp->nx*imtemp->ny*2);
      }
      else if ( swap_bytes && imtemp->kind == MRI_float ) {
        swap_4(MRI_FLOAT_PTR(imtemp), imtemp->nx*imtemp->ny*4);
      }

      if( imtemp == NULL ){
         fprintf(stderr,"\n*** %s does not have exactly one image!\a\n",
                 f_name[i]) ;
         exit(1) ;
      }

      if( imtemp->kind == MRI_short ){
         allim[i] = imtemp ;
      } else {
         allim[i] = mri_to_short( 0.0 , imtemp ) ;
         mri_free( imtemp ) ;
         mri_add_name( f_name[i] , allim[i] ) ;
         input_conversion++ ;
      }
      isize = 2 * allim[i]->nx * allim[i]->ny ;
      if ( i < npoints && isize != fsize) {  /* check lengths of other files */
         fprintf (stderr, "\n\n !!! File %s has different dimensions !!!\a\n",
                  f_name[i]);
         exit(1);
      } else if( isize != EPS1 && isize != EPS2
              && isize != EPS3 && isize != EPS4 && isize != EPS5 ){
         fprintf(stderr,"\n*** file %s has illegal dimensions!\a\n",f_name[i]);
         exit(1) ;
      }
   }
   printf("\n"); fflush(stdout) ;
   if( input_conversion ){
      fprintf(stderr,
              "\n*** %d files converted to shorts on input!\n",
              input_conversion ) ;
   }

   if( RWC_ideal != NULL ){
      FIM_edit_time_series( RWC_ideal ) ;
      if( RWC_ideal->len < npoints ){
         fprintf(stderr,"*** -ideal %s too short!\n",RWC_ideal->fname) ;
         RWC_free_time_series( RWC_ideal ) ;
         RWC_ideal = NULL ;
         RWC_do_overfim = 0 ;
         if( RWC_imover != NULL ) { free(RWC_imover) ; RWC_imover = NULL ; }
      } else {
         kFI3_refsum_count = -1 ;
      }
   }

}

/* ------------------------------------------- Main link from main() C */
   void
   main_FD_EPI()
/* ------------------------------------------------------------------- */
{
   int        i;
   XGCValues  gcv;

   ucolors = SQUE_NR = ncolors = NC;
   uucolors = NC - 1;
   expImage = NULL;

   for (i=0; i<im_height; i++) {   /* init MResize() lookup tables */
      Mltx[i] = i;
      Mlty[i] = i;
   }

   top_nr_name(allim[0]->name);

/* ---------- Open the display. --------- */

   theScreen = DefaultScreen(theDisp);          /* Set Graphic variables */
   rootW     = RootWindow(theDisp,theScreen);
   theGC     = DefaultGC(theDisp,theScreen);
   theVisual = DefaultVisual(theDisp,theScreen);
   CMap      = DefaultColormap(theDisp, theScreen);
   Planes    = DisplayPlanes(theDisp, theScreen);

#if defined(__cplusplus) || defined(c_plusplus)

   if ( (theVisual->c_class != PseudoColor) &&
        (theVisual->c_class != TrueColor) &&
        (theVisual->c_class != DirectColor) )
      FatalError("This program requires PseudoColor or TrueColor or DirectColor modes only. AJ");

   if ( theVisual->c_class != PseudoColor ) AJ_PseudoColor = 0;
#else

   if ( (theVisual->class != PseudoColor) &&
        (theVisual->class != TrueColor) &&
        (theVisual->class != DirectColor) )
      FatalError("This program requires PseudoColor or TrueColor or DirectColor modes only. AJ");

   if ( theVisual->class != PseudoColor ) AJ_PseudoColor = 0;
#endif

   if (!(XAllocNamedColor(theDisp, CMap, "black", &any_col, &rgb_col)))
      FatalError ("XAllocNamedColor problem. AJ");
   fcol = any_col.pixel;

   if (!(XAllocNamedColor(theDisp, CMap, "grey35", &any_col, &rgb_col)))
      FatalError ("XAllocNamedColor problem. AJ");
   bcol = any_col.pixel;

   if ( AJ_PseudoColor ) {
      if (!(XAllocColorCells(theDisp, CMap, True, plane_masks, nplmsk,
         pixels, ucolors))) FatalError ("XAllocColorCells problem. AJ");
   }
   else {
      if ( (AJ_init_RGB()) )
         FatalError ("AJ_init_RGB problem. AJ");;
      AJ_make_STDcol(Solid_color, NUM_STD_COLORS);
   }

   colmap_init();                              /* Initialize color map */


#ifdef USE_MCW
   mcw_load() ;
   for( i=0 ; i < IM_ARR ; i++ ) tmp_ar[i] = mcw_im[i] ;
   SIZE_tmp_ar( im_height ) ;
#else
         /* Create theImage from ar[] #1. Do NOT use here Put_image() ! AJ */
   nowim = SAR(0) ;
   for (i=0; i < ar_size; i++)                   /* renormalize data */
      tmp_ar[i] = (int)((float)(nowim[i]-min1)*coef1 + .5);
   SIZE_tmp_ar( allim[0]->nx ) ;
#endif

   Resample(im_tmp_ar);

   theImage = Load_Any_Arr(imx,im_height,im_height);

   theBelt  = Load_Any_Arr(belt_arr,BELT_W,im_height);

   eWIDE = theImage->width;  eHIGH = theImage->height;
   eW = theBelt->width;      eH = theBelt->height;
   aWIDE = eWIDE + eW;

   if (!(afinfo = XLoadQueryFont(theDisp, KFONT)))
      if (!(afinfo = XLoadQueryFont(theDisp, "fixed"))) {
         sprintf(strp, "Can't open %s or fixed fonts\n", KFONT);
         FatalError (strp);
      }

   mfont=afinfo->fid;
   XSetFont(theDisp,theGC,mfont);
   XSetForeground(theDisp,theGC,fcol);
   XSetBackground(theDisp,theGC,bcol);

   CreateMainWindow(geom, Argc, Argv);   /* Set theWindow properties */

   gcv.function = GXcopy;                /* Extra, Small Text Graphic Context */
   gcv.foreground = fcol;
   txtGC = XCreateGC(theDisp, theWindow, GCForeground|GCFunction, &gcv);

   /** load text font from one of the candidate list **/

   tfont = XGetDefault(theDisp, Xdef_Name , "TextFont");
   if( tfont != NULL ) tfont_hopefuls[0] = tfont ;
   else                tfont_hopefuls[0] = TFONT ;
   { int ifont ;
     for( ifont=0 ; tfont_hopefuls[ifont] != NULL ; ifont++ ){
        mfinfo = XLoadQueryFont(theDisp, tfont_hopefuls[ifont]) ;
        if( mfinfo != NULL ) break ;
     }
     if( mfinfo == NULL ){
         FatalError("Can't open any text font!\n") ;
     }
     tfont = tfont_hopefuls[ifont] ;
   }
   mfont=mfinfo->fid;
   XSetFont(theDisp,txtGC,mfont);
   XSetForeground(theDisp,txtGC,fcol);
   XSetBackground(theDisp,txtGC,bcol);

   New_Cursor(theDisp, theWindow, XC_left_ptr, "red", "white"); /* cursor */

                                                  /* Adjust size before Show */
   MResize(&expImage,&eWIDE,&eHIGH,theImage,eWIDE,eHIGH);
   MResize(&expBelt,&eW,&eH,theBelt,eW,eH);

   XSelectInput(theDisp, theWindow, ExposureMask | KeyPressMask
           | ButtonPressMask | ButtonReleaseMask | StructureNotifyMask);

   XMapWindow(theDisp,theWindow);               /* Show theImage first time */

   while (1) {
      XNextEvent(theDisp, &first_event);        /* Wait for window on */
      switch (first_event.type) {
         case Expose: {
           XExposeEvent *e = (XExposeEvent *) &first_event;
           if (e->window == theWindow) {
            XPutImage(theDisp, theWindow, theGC, expImage,
                      e->x, e->y, e->x, e->y, e->width, e->height);
            XPutImage(theDisp, theWindow, theGC, expBelt,
                      0, 0, eHIGH, 0, eW, eH);
             Allow_smaller_im(Argc, Argv);
         }
#ifdef USE_MCW
   sleep(1) ;    /* show the logo for a second! */
#endif
         return;
       }

         case KeyPress:
         case ButtonPress:
         case ButtonRelease:
         case ConfigureNotify:
         case CirculateNotify:
         case MapNotify:
         case DestroyNotify:
         case GravityNotify:
         case ReparentNotify:
         case UnmapNotify:
         break;

         default:                /* ignore unexpected events */
         break;
      }
   }
}        /* end of main_FD_EPI() */

/* ----------------------- */
   void x_events_loop()
/* ----------------------- */
{
   register int i;

   for (i=LAST_K; i<N_KEYS; i++) XUnmapWindow(theDisp,  key[i].wid);
   back_to_main = 0;
   while (1) {
     if (repeat_status) {       /* For repeated action when button pressed */
       if (XCheckWindowEvent(theDisp, theWindow, ButtonReleaseMask, &w_event))
         event = w_event;
     }
     else {
       XNextEvent(theDisp, &event);  /* single event loop */
     }
     HandleEvent(&event);
     if(back_to_main) return;
   }
}

int C_count = -1;
/* ------------------ */
   void
   HandleEvent(event)
   XEvent *event;
/* ------------------ */
{
   Window wind;

 switch (event->type) {

   case Expose: {
      XExposeEvent *e = (XExposeEvent *) event;
      wind = e->window;
      if (wind == theWindow) {
         XPutImage(theDisp, theWindow, theGC, expImage,
                   e->x, e->y, e->x, e->y, e->width, e->height);
         XPutImage(theDisp, theWindow, theGC, expBelt,
                   0, 0, eHIGH, 0, eW, eH);
      }
      else if (wind == subWindow) {
         DrawSubWindow();
      }
      else if (wind == topWindow) {
         DrawTopWindow();
      }
      else {
         int  i;
         for (i=0; i < N_KEYS; i++)
            if (key[i].wid==wind) DrawKey(i);
         }
      }
       break;

   case KeyPress: {
      XKeyEvent *key_event = (XKeyEvent *) event;
      char      buf[128];
      int       i;
      KeySym    ks;
      XComposeStatus status;

      buf[0] = 0;
      XLookupString(key_event, buf, 128, &ks, &status);
      if (buf[0]=='q' || buf[0]=='Q'){
#ifdef USE_MCW
         XRaiseWindow(theDisp,theWindow) ;
         Put_image(-1) ;
         sleep(1) ;
#endif
         XFreePixmap( theDisp , pxWind ) ;
         XCloseDisplay( theDisp ) ;
	 exit(0);
      }
      if ( (I_lck == 0) && (buf[0]=='I') ) {    /* init image number */
         I_lck = 1;
         New_Cursor(theDisp, theWindow, XC_hand2, "red", "white");
         New_Cursor(theDisp,   GWindow, XC_hand2, "yellow", "red");
         break;
      }
      if ( (N_lck == 0) && (buf[0]=='N') ) {  /* init grid resol. number */
         N_lck = 1;
         matx_0 = matx;
         maty_0 = maty;
         New_Cursor(theDisp, theWindow, XC_hand2, "red", "white");
         New_Cursor(theDisp,   GWindow, XC_hand2, "yellow", "red");
         break;
      }

      /* Arrows keys for edit YYY */
      if ( FT1_pressed ) {
         if ( ks == XK_Right ) {
            int i_tmp = Im_Nr;

            if ( Im_Nr < (npoints - 1) ) Im_Nr += 1;
            if ( i_tmp != Im_Nr ) {
               XClearWindow(theDisp, GWindow);
               Put_image(Im_Nr);
               DrawSubWindow();
               DrawTopWindow();
               discard(KeyPressMask, event);
            }
            break;
         }
         else if ( ks == XK_Left ) {
            int min_im = 0, i_tmp = Im_Nr;

            if(Im_Nr > min_im) Im_Nr -= 1;
            if ( i_tmp != Im_Nr ) {
               XClearWindow(theDisp, GWindow);
               Put_image(Im_Nr);
               DrawSubWindow();
               DrawTopWindow();
               discard(KeyPressMask, event);
            }
            break;
         }
         else if ( ks == XK_Up ) {
            int   i, j, nn;
            float f0 = fft_mag, f1, f2, f3, fff = 1.1;

            if ( key_event->state & ShiftMask )   fff = 1.5;
            if ( key_event->state & ControlMask ) fff = 2.3;
            nn = act_undo + 1;
            act_undo += ar_size;
            undo_buf = realloc(undo_buf, (act_undo+1)*sizeof(struct _undo_buf));
            if ( undo_buf != NULL ) {
               for ( i=0, j=nn; i < ar_size; j++, i++) {
                  undo_buf[j].im  = Im_Nr;
                  undo_buf[j].pix = i;
                  undo_buf[j].r   = c_arr[i*FT_dim+Im_Nr+1].r;
                  undo_buf[j].i   = c_arr[i*FT_dim+Im_Nr+1].i;
                  undo_buf[j].r2  = c_arr[(i+1)*FT_dim-Im_Nr-1].r;
                  undo_buf[j].i2  = c_arr[(i+1)*FT_dim-Im_Nr-1].i;
                  f1 = c_arr[i*FT_dim+Im_Nr+1].r *= fff;
                  f2 = c_arr[i*FT_dim+Im_Nr+1].i *= fff;
                  c_arr[(i+1)*FT_dim-Im_Nr-1].r  *= fff;
                  c_arr[(i+1)*FT_dim-Im_Nr-1].i  *= fff;

                  f3 = f0 * sqrt(f1*f1 + f2*f2);
                  if ( f3 > 32767. ) f3 = 32767.;
                  SAR(Im_Nr)[i] = f3;
               }
               redraw_graph() ;
               DrawSubWindow();
            }
            else {
               act_undo = -1;
               fprintf(stderr,"\n*** cannot realloc undo_buf\a\n") ;
               XBell(theDisp, 100);
            }

            if ( RWC_ideal != NULL  && RWC_ideal->len >= npoints ) {
               ref_undo += 1;
               undo_ref =
                    realloc(undo_ref, (ref_undo+1)*sizeof(struct _undo_buf));
               if ( undo_ref != NULL ) {
                  j = ref_undo;
                  undo_ref[j].im  = Im_Nr;
                  undo_ref[j].r   = r_arr[Im_Nr+1].r;
                  undo_ref[j].i   = r_arr[Im_Nr+1].i;
                  undo_ref[j].r2  = r_arr[FT_dim-Im_Nr-1].r;
                  undo_ref[j].i2  = r_arr[FT_dim-Im_Nr-1].i;
                  f1 = r_arr[Im_Nr+1].r   *= fff;
                  f2 = r_arr[Im_Nr+1].i   *= fff;
                  r_arr[FT_dim-Im_Nr-1].r *= fff;
                  r_arr[FT_dim-Im_Nr-1].i *= fff;
                  f3 = f0 * sqrt(f1*f1 + f2*f2);
                  if ( f3 > 32767. ) f3 = 32767.;
                  RWC_ideal->ts[Im_Nr] = f3;
                  redraw_graph() ;
                  DrawSubWindow();
               }
            }
            else {
               ref_undo = -1;
               fprintf(stderr,"\n*** cannot realloc undo_ref\a\n") ;
               XBell(theDisp, 100);
            }
            discard(KeyPressMask, event);
            break;
         }
         else if ( ks == XK_Down ) {
            int   i, j, nn;
            float f0 = fft_mag, f1, f2, f3, fff = 1./1.1;

            if ( key_event->state & ShiftMask )   fff = 1./1.5;
            if ( key_event->state & ControlMask ) fff = 1./2.3;
            nn = act_undo + 1;
            act_undo += ar_size;
            undo_buf = realloc(undo_buf, (act_undo+1)*sizeof(struct _undo_buf));
            if ( undo_buf != NULL ) {
               for ( i=0, j=nn; i < ar_size; j++, i++) {
                  undo_buf[j].im  = Im_Nr;
                  undo_buf[j].pix = i;
                  undo_buf[j].r   = c_arr[i*FT_dim+Im_Nr+1].r;
                  undo_buf[j].i   = c_arr[i*FT_dim+Im_Nr+1].i;
                  undo_buf[j].r2  = c_arr[(i+1)*FT_dim-Im_Nr-1].r;
                  undo_buf[j].i2  = c_arr[(i+1)*FT_dim-Im_Nr-1].i;
                  f1 = c_arr[i*FT_dim+Im_Nr+1].r *= fff;
                  f2 = c_arr[i*FT_dim+Im_Nr+1].i *= fff;
                  c_arr[(i+1)*FT_dim-Im_Nr-1].r  *= fff;
                  c_arr[(i+1)*FT_dim-Im_Nr-1].i  *= fff;

                  f3 = f0 * sqrt(f1*f1 + f2*f2);
                  if ( f3 > 32767. ) f3 = 32767.;
                  SAR(Im_Nr)[i] = f3;
               }
               redraw_graph() ;
               DrawSubWindow();
            }
            else {
               act_undo = -1;
               fprintf(stderr,"\n*** cannot realloc undo_buf\a\n") ;
               XBell(theDisp, 100);
            }

            if ( RWC_ideal != NULL  && RWC_ideal->len >= npoints ) {
               ref_undo += 1;
               undo_ref =
                  realloc(undo_ref, (ref_undo+1)*sizeof(struct _undo_buf));
               if ( undo_ref != NULL ) {
                  j = ref_undo;
                  undo_ref[j].im  = Im_Nr;
                  undo_ref[j].r   = r_arr[Im_Nr+1].r;
                  undo_ref[j].i   = r_arr[Im_Nr+1].i;
                  undo_ref[j].r2  = r_arr[FT_dim-Im_Nr-1].r;
                  undo_ref[j].i2  = r_arr[FT_dim-Im_Nr-1].i;
                  f1 = r_arr[Im_Nr+1].r   *= fff;
                  f2 = r_arr[Im_Nr+1].i   *= fff;
                  r_arr[FT_dim-Im_Nr-1].r *= fff;
                  r_arr[FT_dim-Im_Nr-1].i *= fff;
                  f3 = f0 * sqrt(f1*f1 + f2*f2);
                  if ( f3 > 32767. ) f3 = 32767.;
                  RWC_ideal->ts[Im_Nr] = f3;
                  redraw_graph() ;
                  DrawSubWindow();
               }
            }
            else {
               ref_undo = -1;
               fprintf(stderr,"\n*** cannot realloc undo_ref\a\n") ;
               XBell(theDisp, 100);
            }
 
            discard(KeyPressMask, event);
            break;
         }
         else if ( buf[0]=='u' ) {
            if ( ref_undo > -1 ) {
               int   j, k;
               float f0 = fft_mag, f1, f2, f3;
               if ( RWC_ideal != NULL  && RWC_ideal->len >= npoints ) {
                  j = ref_undo;
                  k = undo_ref[j].im;
                  f1 = r_arr[k+1].r = undo_ref[j].r;
                  f2 = r_arr[k+1].i = undo_ref[j].i;
                  r_arr[FT_dim-k-1].r = undo_ref[j].r2;
                  r_arr[FT_dim-k-1].i = undo_ref[j].i2;
                  f3 = f0 * sqrt(f1*f1 + f2*f2);
                  if ( f3 > 32767. ) f3 = 32767.;
                  RWC_ideal->ts[k] = f3;
                  ref_undo -= 1;
                  undo_ref = 
                     realloc(undo_ref,(ref_undo+1)*sizeof(struct _undo_buf));
                  if ( undo_ref == NULL ) {
                     ref_undo = -1;
                     fprintf(stderr, "\n*** cannot realloc undo_ref\a\n") ;
                     XBell(theDisp, 100);
                  }
               }
            }
            if ( act_undo > -1 ) {
               int   i, j, k;
               float f0 = fft_mag, f1, f2, f3;
               for ( i=0, j = act_undo; i < ar_size; i++, j--) {
                  Im_Nr = undo_buf[j].im;
                  k = undo_buf[j].pix;
                  f1 = c_arr[k*FT_dim+Im_Nr+1].r = undo_buf[j].r;
                  f2 = c_arr[k*FT_dim+Im_Nr+1].i = undo_buf[j].i;
                  c_arr[(k+1)*FT_dim-Im_Nr-1].r  = undo_buf[j].r2;
                  c_arr[(k+1)*FT_dim-Im_Nr-1].i  = undo_buf[j].i2;
                  f3 = f0 * sqrt(f1*f1 + f2*f2);
                  if ( f3 > 32767. ) f3 = 32767.;
                  SAR(Im_Nr)[k] = f3;
               }

               act_undo -= ar_size;
               undo_buf=realloc(undo_buf,(act_undo+1)*sizeof(struct _undo_buf));

               if ( undo_buf != NULL ) {
                  redraw_graph() ;
                  DrawSubWindow();
 
               }
               else {
                  act_undo = -1;
                  fprintf(stderr,
                          "\n*** cannot realloc undo_buf or undo_ref\a\n") ;
                  XBell(theDisp, 100);
               }
            } 

            discard(KeyPressMask, event);
            break;
         }
         else if ( buf[0]=='U' ) {
            int   j, k;
            float f0 = fft_mag, f1, f2, f3;
            for (j=act_undo; j >= 0; j--) {
               Im_Nr = undo_buf[j].im;
               k = undo_buf[j].pix;
               f1 = c_arr[k*FT_dim+Im_Nr+1].r = undo_buf[j].r;
               f2 = c_arr[k*FT_dim+Im_Nr+1].i = undo_buf[j].i;
               c_arr[(k+1)*FT_dim-Im_Nr-1].r  = undo_buf[j].r2;
               c_arr[(k+1)*FT_dim-Im_Nr-1].i  = undo_buf[j].i2;
               f3 = f0 * sqrt(f1*f1 + f2*f2);
               if ( f3 > 32767. ) f3 = 32767.;
               SAR(Im_Nr)[k] = f3;
            }
            if ( RWC_ideal != NULL  && RWC_ideal->len >= npoints ) {
               for (j=ref_undo; j >= 0; j--) {
                  k = undo_ref[j].im;
                  f1 = r_arr[k+1].r = undo_ref[j].r;
                  f2 = r_arr[k+1].i = undo_ref[j].i;
                  r_arr[FT_dim-k-1].r  = undo_ref[j].r2;
                  r_arr[FT_dim-k-1].i  = undo_ref[j].i2;
                  f3 = f0 * sqrt(f1*f1 + f2*f2);
                  if ( f3 > 32767. ) f3 = 32767.;
                  RWC_ideal->ts[k] = f3;
               }
               free(undo_ref);
               ref_undo = -1;
            }
            free(undo_buf);
            act_undo = -1;
            redraw_graph();
            DrawSubWindow();
            discard(KeyPressMask, event);
            break;
         }
      }
      else {    /* no FT1_pressed */
         if ( ks == XK_Right ) {
            int max_im, i_tmp = Im_Nr;
            int aaa = avr_grp, ddd = diff_im;

            max_im = npoints - Av_length;

            if ( Im_Nr >= npoints ) {
               if ( Im_Nr < (N_im - 1) ) Im_Nr += 1;
            }
            else if ( Im_Nr < max_im ) Im_Nr += 1;
            else if ( npoints < N_im ) {
               avr_grp = 0;
               diff_im = 0;
               Im_Nr = npoints;
            }
            if ( i_tmp != Im_Nr ) {
               redraw =  avr_grp * 4 + (aaa - avr_grp) *2 + ddd - diff_im;
               XClearWindow(theDisp, GWindow);
               Put_image(Im_Nr);
               if ( redraw ) redraw_graph();
               DrawSubWindow();
               DrawTopWindow();
               discard(KeyPressMask, event);
            }
            break;
         }
         else if ( ks == XK_Left ) {
            int min_im = 0, i_tmp = Im_Nr;
            int aaa = avr_grp, ddd = diff_im;

            if ( Im_Nr > npoints ) Im_Nr -= 1;
            else {
               diff_im = fim_dif;
               avr_grp = fim_avr;
               if(Im_Nr > min_im) Im_Nr -= 1;
               if ( avr_grp )
                  if ( Im_Nr > (npoints - Av_length) )
                     Im_Nr = npoints - Av_length;
            }
            if ( i_tmp != Im_Nr ) {
               redraw =  avr_grp * 4 + (aaa - avr_grp) *2 + ddd - diff_im;
               XClearWindow(theDisp, GWindow);
               Put_image(Im_Nr);
               if ( redraw ) redraw_graph();
               DrawSubWindow();
               DrawTopWindow();
               discard(KeyPressMask, event);
            }
            break;
         }
      }


      if ( (I_lck == 1) && (buf[0] == 13) ) { /* make image number */
         if ( tmp_Nr >= 0 ) {
            int aaa = avr_grp, ddd = diff_im;

            if ( tmp_Nr > 0 ) Im_Nr = min(N_im - 1, tmp_Nr - 1);
            New_Cursor(theDisp, theWindow, XC_left_ptr, "red", "white");
            New_Cursor(theDisp,   GWindow, XC_left_ptr, "blue", "yellow");
            XClearWindow(theDisp, GWindow);
            diff_im = fim_dif;
            avr_grp = fim_avr;
            if ( Im_Nr >= npoints ) {
               avr_grp = 0;
               diff_im = 0;
            }
            else
               if ( Im_Nr > npoints - Av_length )
                  Im_Nr = npoints - Av_length;
            redraw =  avr_grp * 4 + (aaa - avr_grp) *2 + ddd - diff_im;
            Put_image(Im_Nr);
            if ( redraw ) redraw_graph();
            DrawSubWindow();
            DrawTopWindow();
            I_lck = 0;
            tmp_Nr = 0;
         }
         break;
      }
      if ( (N_lck == 1) && (buf[0] == 13) ) { /* make grid resol. number */
         if ( tmp_Nr >= 0 ) {
            if ( tmp_Nr > 0 ) {
               matx = min(MAT_MAX, tmp_Nr);
               maty = min(MAT_MAX, tmp_Nr);
            }
            New_Cursor(theDisp, theWindow, XC_left_ptr, "red", "white");
            New_Cursor(theDisp,   GWindow, XC_left_ptr, "blue", "yellow");
            if ( (matx != matx_0) && (maty != maty_0) ) {
               init_mat(0);
               redraw_graph();
            }
	    RWC_framehide = 0 ;
            Put_image(Im_Nr);
            DrawSubWindow();
            N_lck = 0;
            tmp_Nr = 0;
         }
         break;
      }
      if ( (I_lck == 1) || (N_lck == 1) ) {   /* make a number from digits */
         if ( isdigit(buf[0]) ) {
            i = buf[0] - 48;
            tmp_Nr = min(10000, 10*tmp_Nr + i);
         }
         break;
      }
      else {
         switch (buf[0]) {
            /* make time course array of the sum of several pixels */
            case 'a': {   
               int i, index;
               if ( FT_graph_on ) {
                  if ( avr_A == NULL ) {
                     avr_A = (float *) malloc(t_points*sizeof(float));
                     if( avr_A == NULL ){
                        fprintf(stderr,"\n*** cannot malloc avr_A\a\n") ;
                        XBell(theDisp, 100); break;
                     }
                     else {
                        for (i=0; i < t_points; i++) avr_A[i] = 0.;
                     }
                     avr_nr = 0;
                  }
                  index = ypoint*im_size+xpoint;
                  for (i=0; i < t_points; i++) 
                     avr_A[i] += (float) T_SAR(i)[index];
                  avr_nr++;
               }
               discard(KeyPressMask, event);
               break ;
            }
            case 'e': {  /* average to the last pixel */
               int i, index = ar_size - 1;
               float f0;
               if ( FT_graph_on && (avr_A != NULL) ) {
                  if ( avr_nr > 0 ) {
                     f0 = 1. / (float) avr_nr;
                     for (i=0; i < t_points; i++) {
                        T_SAR(i)[index-allim[0]->nx] =
                        T_SAR(i)[index] = f0 * avr_A[i];
                     }
printf(" t_points = %d, avr_nr = %d\n", t_points, avr_nr);
                     free(avr_A);
                     avr_nr = 0;
                     cancell_FT = 1;
                  }
               }
               discard(KeyPressMask, event);
               break ;
            }
            case '~': {
               save_all_images();
               break;
            }
            case '^': {
               New_Cursor(theDisp, theWindow, XC_left_ptr, "red", "white");
               break;
            }
            case 's': {
               c_swap();
               break;
            }
            case 'S': {
               save_act_im();
               discard(KeyPressMask, event);
               break;
            }
            case 'X': {          /* AAJ tmp to save average array as is */
               save_avr_array();
               discard(KeyPressMask, event);
               break;
            }
            case 'f': {
               if ( FT_graph_on ) get_fft_mag(&fft_mag);
               discard(KeyPressMask, event);
               break ;
            }
            case 'F': {
               read_new_im() ;
               discard(KeyPressMask, event);
               break ;
            }
            case 'K': {
               kill_curr_im() ;
               discard(KeyPressMask, event);
               break ;
            }
            case 'A': {
               RWC_autoscale = ! RWC_autoscale ;
               old_im = -1 ;
               Put_image( Im_Nr ) ;
               discard(KeyPressMask, event);
               break ;
            }
            case 'h': {
               RWC_framehide = ! RWC_framehide ;
               Put_image( Im_Nr ) ;
               break ;
            }
            case 'H': {
               RWC_overhide = ! RWC_overhide ;
               Put_image( Im_Nr ) ;
               break ;
            }
	    case 'O': {
               RWC_checker  = ! RWC_checker ;
	       RWC_overhide = 0 ;
               Put_image( Im_Nr ) ;
	       break ;
            }
            case 'C': {
               YES_color = 1;
               colmap_init();
               break;
            }
            case 'B': {
               YES_color = 0;
               colmap_init();
               break;
            }
#ifdef USE_MCW
            case '0': {
               Put_image(-1) ;
               sleep(1) ;
               Put_image(Im_Nr) ;
               discard(KeyPressMask, event);
               break ;
            }
#endif
            case '1':   /* move to image with this number */
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
            case '7':
	    case '8':
	    case '9': {
               int aaa = avr_grp, ddd = diff_im , itmp;

               itmp = buf[0] - '1' ; if( itmp >= N_im ) itmp = N_im - 1 ;

               /* past time course => no averaging, etc */
               if( itmp >= npoints ) {
                  avr_grp = 0;
                  diff_im = 0;
               }
               else {
                  diff_im = fim_dif;
                  avr_grp = fim_avr;
                  if( itmp > npoints-Av_length ) itmp = npoints - Av_length;
               }
	       if( itmp != Im_Nr ){
		  Im_Nr = itmp ;
                  redraw =  avr_grp * 4 + (aaa - avr_grp) *2 + ddd - diff_im;

                  XClearWindow(theDisp, GWindow);
                  Put_image(Im_Nr);
                  if ( redraw ) redraw_graph();
                  DrawSubWindow();
                  DrawTopWindow();
               }
               discard(KeyPressMask, event);
               break;
            }
            case 'L':
            case 'l': {
               int max_im ;
               int aaa = avr_grp, ddd = diff_im;

               if( buf[0] == 'L' ) max_im = npoints - 1 ;
               else                max_im = N_im - 1 ;

               if ( max_im >= npoints ) {
                  avr_grp = 0;
                  diff_im = 0;
               }
               else {
                  diff_im = fim_dif;
                  avr_grp = fim_avr;
                  max_im = npoints - Av_length;
               }
               redraw =  avr_grp * 4 + (aaa - avr_grp) *2 + ddd - diff_im;
               Im_Nr = max_im;
               XClearWindow(theDisp, GWindow);
               Put_image(Im_Nr);
               if ( redraw ) redraw_graph();
               DrawSubWindow();
               DrawTopWindow();
               discard(KeyPressMask, event);
               break;
            }
            case '>': {
               int max_im, i_tmp = Im_Nr;
               int aaa = avr_grp, ddd = diff_im;

               max_im = npoints - Av_length;

               if ( Im_Nr >= npoints ) {
                  if ( Im_Nr < (N_im - 1) ) Im_Nr += 1;
               }
               else if ( Im_Nr < max_im ) Im_Nr += 1;
               else if ( npoints < N_im ) {
                  avr_grp = 0;
                  diff_im = 0;
                  Im_Nr = npoints;
               }
               if ( i_tmp != Im_Nr ) {
                  redraw =  avr_grp * 4 + (aaa - avr_grp) *2 + ddd - diff_im;
                  XClearWindow(theDisp, GWindow);
                  Put_image(Im_Nr);
                  if ( redraw ) redraw_graph();
                  DrawSubWindow();
                  DrawTopWindow();
                  discard(KeyPressMask, event);
               }
               break;
            }
            case '<': {
               int min_im = 0, i_tmp = Im_Nr;
               int aaa = avr_grp, ddd = diff_im;

#ifdef USE_MCW
               if( Im_Nr == 0 ){ 
                  Put_image(-1) ; 
                  sleep(1) ;
                  Put_image(0) ;
                  discard(KeyPressMask, event);
                  break ; 
               }
#endif

               if ( Im_Nr > npoints ) Im_Nr -= 1;
               else {
                  diff_im = fim_dif;
                  avr_grp = fim_avr;
                  if(Im_Nr > min_im) Im_Nr -= 1;
                  if ( avr_grp )
                     if ( Im_Nr > (npoints - Av_length) )
                        Im_Nr = npoints - Av_length;
               }
               if ( i_tmp != Im_Nr ) {
                  redraw =  avr_grp * 4 + (aaa - avr_grp) *2 + ddd - diff_im;
                  XClearWindow(theDisp, GWindow);
                  Put_image(Im_Nr);
                  if ( redraw ) redraw_graph();
                  DrawSubWindow();
                  DrawTopWindow();
                  discard(KeyPressMask, event);
               }
               break;
            }
                   /* + and - keys used for adjusting scale */
            case '-': {
               scale_down();
               redraw_graph();
               DrawSubWindow();
               break;
            }
            case '+': {
               scale_up();
               redraw_graph();
               DrawSubWindow();
               break;
            }
            case 'b': {
               RWC_groupbase = ! RWC_groupbase ;
               redraw_graph();
               DrawSubWindow();
               break;
            }
            case 'x': {
               AJ_base = ! AJ_base;
               redraw_graph();
               DrawSubWindow(); 
               break;
            } /* p used to write plot to file */
            case 'w': case 'p': {
               int ask_file = (buf[0] == 'p') ;

               print_plot(ask_file);
               discard(KeyPressMask, event);
               break;
            }
                   /* m and M keys used for adjusting matrix */
            case 'm': {
               mat_down(0);
               discard(KeyPressMask, event);
               break;
            }
            case 'M': {
               mat_up(0);
               discard(KeyPressMask, event);
               break;
            }
            case 't': {
               Time_avr = ! Time_avr;
               redraw_graph();
               DrawSubWindow();
               discard(KeyPressMask, event);
               break;
            }
            case 'v': {
               mat_down(1);
               discard(KeyPressMask, event);
               break;
            }
            case 'V': {
               mat_up(1);
               discard(KeyPressMask, event);
            }
                   /* g and G keys used for adjusting grid lines */
            case 'g': {
               grid_down();
               discard(KeyPressMask, event);
               break;
            }
            case 'G': {
               grid_up();
               discard(KeyPressMask, event);
               break;
            }
            case 'r': {
               color_index++;
               if (color_index >= COL_NUM) color_index = -1;
               if (color_index < 1) mark = 1 - mark;
               redraw_graph();
               DrawSubWindow();
               discard(KeyPressMask, event);
               break;
            }
            case 'R': {
               fr_index--;
               if (fr_index < -16){
                  fr_index = -1;
#ifdef USE_MCW
                  Put_image(-1) ;
#endif
	       }
	       RWC_framehide = 0 ;
               Put_image(Im_Nr);
               discard(KeyPressMask, event);
               break;
            }
            case 'd': {
               int xx, index = ypoint*im_size+xpoint;
               xx = (int) (SAR(Im_Nr)[index]) - 3600;
               if ( xx > 32767 ) SAR(Im_Nr)[index] = 32767;
               else              SAR(Im_Nr)[index] = xx;
               redraw_graph() ;
               DrawSubWindow();
               old_im = -1;
               Put_image(Im_Nr);
               discard(KeyPressMask, event);
               break;
            }
            case 'D': {
               int xx, index = ypoint*im_size+xpoint;
               xx = (int) (SAR(Im_Nr)[index]) + 3600;
               if ( xx < -32768 ) SAR(Im_Nr)[index] = -32768;
               else               SAR(Im_Nr)[index] = xx;
               redraw_graph() ;
               DrawSubWindow();
               old_im = -1;
               Put_image(Im_Nr);
               discard(KeyPressMask, event);
               break;
            }

         }
      } /* end I_lck = 0 */

   }  /* end of KeyPress */
   break;

   case ButtonPress: {
      XButtonPressedEvent *b = (XButtonPressedEvent *) event;
      int delta = 1;

      wind = b->window;
      if (wind == theWindow ) {

         if (b->button == Button1 && repeat_status == 0) {
            if (b->x > eWIDE) break;
            B1_action = 1;
            Track_Cursor(b->x, b->y);
            discard(ButtonPressMask, event);
            break;
         }

         if ( b->button == Button2 ) {        /* contrast and brightness */
            repeat_status = 1;
            if ( C_count <= 0 ) {
               if (b->x <   eWIDE/3) {
                  c_squeeze( delta);
               }
               if ((b->x > 2*eWIDE/3) && (b->x < eWIDE)) {
                  c_squeeze(-delta);
               }
               if ((b->y <   eHIGH/3) && (b->x < eWIDE)) {
                  c_bright(-delta);
               }
               if ((b->y > 2*eHIGH/3) && (b->x < eWIDE)) {
                  c_bright( delta);
               }
               if ((b->y >   eHIGH/2) && (b->x > eWIDE) && (b->x < eWIDE+eW)) {
                  c_rotate(-delta);
               }
               if ((b->y <   eHIGH/2) && (b->x > eWIDE) && (b->x < eWIDE+eW)) {
                  c_rotate(delta);
               }
               if ( AJ_PseudoColor )
                  C_count = CONTRAST_CHANGE_STEP;
               else
                  C_count = 0;
               break;
            }
            if ( C_count > -1 ) C_count--;
         }

         if (b->button == Button3 && repeat_status == 0) {
            delta = 1;
            if (b->x <   eWIDE/3) {
               c_squeeze( delta); break;
            }
            if ((b->x > 2*eWIDE/3) && (b->x < eWIDE)) {
               c_squeeze(-delta); break;
            }
            if ((b->y <   eHIGH/3) && (b->x < eWIDE)) {
               c_bright(-delta); break;
            }
            if ((b->y > 2*eHIGH/3) && (b->x < eWIDE)) {
               c_bright( delta); break;
            }
            if ((b->y >   eHIGH/3) && (b->y < 2*eHIGH/3) &&
                (b->x >   eWIDE/3) && (b->x < 2*eWIDE/3)) {
               if (YES_color) No_Color_init = 1;
               else           No_Grey_init  = 1;
               colmap_init(); break;
            }
            if ((b->y >   eHIGH/2) && (b->x > eWIDE) && (b->x < eWIDE + eW)) {
               c_rotate(-delta); break;
            }
            if ((b->y <   eHIGH/2) && (b->x > eWIDE) && (b->x < eWIDE + eW)) {
               c_rotate(delta); break;
            }
         }
      }   /* for theWindow */
      else if (wind == GWindow) {
         int i, j, kx , ky;

         if( (b->button == Button1) && (b->x > GL_DLX) ){
            kx = gx_max/matx;
            ky = gy_max/maty;
            i = (b->x - GL_DLX + kx)*matx/gx_max;
            j = (b->y - GT_DLY + ky)*maty/gy_max;
            xpoint += i - (matx + 2)/2;
            ypoint += j - (maty + 2)/2;
            if (xpoint < 0)        xpoint += im_size;
            if (xpoint >= im_size) xpoint -= im_size;
            if (ypoint < 0)        ypoint += im_size;
            if (ypoint >= im_size) ypoint -= im_size;

            if ( (xpoint == xpoint_0 ) && (ypoint == ypoint_0) ) ;
            else {
               RWC_framehide = 0 ;
               Put_image(Im_Nr);
               redraw_graph();
               DrawSubWindow();
               xpoint_0 = xpoint;
               ypoint_0 = ypoint;
            }
         }
         discard(ButtonPressMask, event);
         break;
      }   /* for GWindow */
      else if (wind == subWindow) {
         if (b->button == Button1) {
            if ( (abs(b->x - v_point_x) < 5 ) &&
                 (abs(b->y - v_point_y) < 9 ) ) {
               Track_Vpointer();
            }
         }
      }   /* for subWindow */

      if ( (b->button == Button1) || (b->button == Button2)
                                  || (b->button == Button3) ) {
         int  i;
         if( invkey < 0 ){
            if      (b->button == Button1) rot_direct = -1;
            else if (b->button == Button2) rot_direct =  0;
            else if (b->button == Button3) rot_direct =  1;
            rot_state = b->state ;
            for (i=0; i < N_KEYS; i++) {
               if (key[i].wid==wind) { InvertKey(i); invkey=i; break; }
            }
         }
      }   /* for keys */
   }   /* end of ButtonPress */
   break;

   case ButtonRelease: {
      XButtonReleasedEvent *b = (XButtonReleasedEvent *) event;

      wind = b->window;

      if (wind == theWindow ) {
         if (B1_action == 1) {
            B1_action = 0;
            xpoint = Mltx[b->x]/x_mag;
            ypoint = Mlty[b->y]/x_mag;

            if ( (xpoint == xpoint_0 ) && (ypoint == ypoint_0) ) ;
            else {
               RWC_framehide = 0 ;
               Put_image(Im_Nr);
               redraw_graph();
               DrawSubWindow();
               xpoint_0 = xpoint;
               ypoint_0 = ypoint;
            }
         }
         if (b->button == Button2) {
            repeat_status = 0;
         }
      }
      else if (wind == subWindow ) {
         discard_Key(wind, ButtonPressMask, event);
         discard_Key(wind, ButtonReleaseMask, event);
      }
      else if ( (b->button == Button1) || (b->button == Button2)
                                       || (b->button == Button3) ) {
         int i;
         if( invkey >= 0 ){
            for (i=0; i < N_KEYS; i++) {
               if (wind == key[i].wid) { LetGoKey(invkey); break; }
            }
            invkey = -1;
         }
         discard_Key(wind, ButtonPressMask, event);
         discard_Key(wind, ButtonReleaseMask, event);
      }
   }
   break;

   case ConfigureNotify: {
      XConfigureEvent *conf_event = (XConfigureEvent *) event;
      int wb;
      wb = conf_event->height*BELT_W/im_height;
      if ( (conf_event->window == theWindow) &&
           (conf_event->height != eHIGH)) {
            MResize(&expImage, &eWIDE, &eHIGH, theImage,
                  conf_event->height, conf_event->height);
            MResize(&expBelt, &eW, &eH, theBelt,
                  wb, conf_event->height);
      }
      else if ( (conf_event->window == GWindow) &&
           ((conf_event->height != idY) || (conf_event->width != idX)) ) {
         redo_graph_window(conf_event->width, conf_event->height);
      }
   }
   break;

   case CirculateNotify:
   case MapNotify:
   case DestroyNotify:
   case GravityNotify:
   case ReparentNotify:
   case UnmapNotify:
      break;

   default:      /* ignore unexpected events */
      break;
 }   /* end of switch event->type */
}   /* end of HandleEvent() */

/* It loads color strip made by make_belt() & adds unit color stips */
/* ------------------------------------------------------------- */
   void
   load_rect_str(idata, ipx, x, y, i1, x1, i2, x2, amin, nshade)
   short int idata[], ipx[];
   int       x, y, i1, i2, x1, x2, amin, nshade;
/* ------------------------------------------------------------- */
{
   register int i, ii, j, k;
   int          min0, max1;
   float        adel0, coef;

   min0 =  32767;
   max1 = -32768;
   ii = x*y;
   for (i=0; i < ii; i++) {
      if ( idata[i] < min0 ) min0 = idata[i];
      if ( idata[i] > max1 ) max1 = idata[i];
   }
   adel0 = max1 - min0;

   /* set image shades */

   if (adel0 < 1.) adel0 = 1.;
   coef = ( (float) nshade - 1. ) / adel0;

   k = x - i2;
   for (i=0; i < y; i++) {
      ii = i*x;
      for (j=0; j < i1; j++)
         ipx[ii+j] = -x1;
      for (j=x-i2; j < x; j++)
         ipx[ii+j] = -x2;
      for (j=i1; j < k; j++)
         ipx[ii+j] = (int) ( (float) (idata[ii+j] - min0) * coef + .5) + amin;
   }
}

/* ------------------- */       /* It makes data belt (ramp of indices) */
   void
   make_belt(id, x, y)
   short int *id;
   int       x, y;
/* ------------------- */
{
   register int i, j, ii;

   for(i=0; i < y; i++) {
      ii = i*x;
      for(j=0; j < x; j++)
         id[ii+j] = x - i;
   }
}

/* ------------------------------ */ /* It resizes images universally  */
   void
   MResize(emage,eW,eH,image,w,h)    /* for 12 and 8 bit planes. */
   int     w, h, *eW, *eH;
   XImage  *image,*(*emage);
/* ------------------------------ */
{
   int          lt[MAX_WIDTH], bp16;
   register int iy, ex, ey, iW, iH, iG, iN, w2, tmp;
   char         *ximag;
   char         *Ep, *El, *Ip, *Il, *Id;

    iN = image->bits_per_pixel/8;    /* 1 or 2, pointer increment */
    iG = image->bytes_per_line;      /* Number of bytes in line */
    w2 = w * iN;
    iW = image->width;
    iH = image->height;
    bp16 = (iN < 3) ? 16 : 32;
    if ( iN == 1 ) bp16 = 8;
    if (w==iW && h==iH) {       /* very special case */
        if (*emage != image) {
            if (*emage) XDestroyImage(*emage);
            *emage = image;
            *eW = iW;  *eH = iH;
        }
    }

    else {                              /* have to do some work */

        /* first, kill the old emage, if one exists */
        if (*emage && (*emage != image)) {
           free((*emage)->data);  (*emage)->data = NULL;
           XDestroyImage(*emage);
        }

        /* create emage of the appropriate size */

        *eW = w;  *eH = h;
        ximag = (char *) malloc(w2 * h);

        *emage = XCreateImage(theDisp, theVisual, Planes, ZPixmap, 0, ximag, w, h, bp16, w2);

        if (!ximag || !(*emage)) {
           fprintf(stderr,"ERROR: unable to create a %dx%d image\a\n",w,h);
           exit(1);
        }

        El = Ep = (char *) (*emage)->data;
        Id = (char *) image->data;

        /* ---- Set look up table and Reframe fast ---- */

        for (ex = 0; ex < w; ex++)  {
            tmp = (iW * ex)/w;
            lt[ex] = iN * tmp;
            Mltx[ex] = tmp;
        }

        for (ey = 0; ey < h; ey++, El += w2) {
            iy = (iH * ey)/h;
            Mlty[ey] = iy;
            Ep = El;
            Il = Id + iG * iy;
            for(ex = 0; ex < w; ex++, Ep += iN) {
                Ip = Il + lt[ex];
                if ( iN == 1 ) {
                   *Ep = *Ip;
                }
                else if ( iN == 2) {
                   *Ep = *Ip;
                   *(Ep+1) = *(Ip+1);
                }
                else if ( iN == 3) {
                   *Ep = *Ip;
                   *(Ep+1) = *(Ip+1);
                   *(Ep+2) = *(Ip+2);
                }
                else if ( iN == 4) {
                   *Ep = *Ip;
                   *(Ep+1) = *(Ip+1);
                   *(Ep+2) = *(Ip+2);
                   *(Ep+3) = *(Ip+3);
                }
            }
        }
    }
}

/* ---------------------------- */
   void
   Allow_smaller_im(argc, argv)
   int  argc;
   char *argv[];
/* ---------------------------- */
{
   XSizeHints           hints;

   hints.min_height = 0;
   hints.min_width = 0;
   hints.flags = PMinSize;

   hints.min_aspect.x = im_height + BELT_W;
   hints.max_aspect.x = im_height + BELT_W;
   hints.min_aspect.y = im_height;
   hints.max_aspect.y = im_height;
   hints.flags |= PAspect;

   hints.max_height = DisplayHeight(theDisp, theScreen);
   hints.max_width = hints.max_height *
                     (im_height + BELT_W)/im_height;
   hints.flags |= PMaxSize;
   XSetStandardProperties(theDisp, theWindow, T_name, T_name, None,
                          argv, argc, &hints);
}

/* ---------------------------------- */
   void
   CreateMainWindow(geom, argc, argv)
   char *geom;
   int  argc;
   char *argv[];
/* ---------------------------------- */
{
   XClassHint           class;
   XSetWindowAttributes attr;
   unsigned int         attrmask;
   XSizeHints           hints;
   int                  mask, x, y, w, h, i;

   x = y = w = h = 1;
   mask = XParseGeometry(geom, &x, &y, (unsigned int *)&w, (unsigned int *)&h);

   if (mask & HeightValue) {
     i = im_height + BELT_W;
     if ( i > MAX_WIDTH ) h = (MAX_WIDTH * im_height) / i;
     eH = eHIGH = h;
     eWIDE = eHIGH;
     eW = eH * BELT_W / im_height;
     aWIDE = eWIDE + eWIDE*BELT_W/im_height;
   }

   if (mask & XValue || mask & YValue) hints.flags = USPosition;
   else                                hints.flags = PPosition;

   hints.flags |= USSize;

   if (mask & XValue && mask & XNegative)
        x = XDisplayWidth(theDisp, theScreen)-eWIDE-abs(x);
   if (mask & YValue && mask & YNegative)
        y = XDisplayHeight(theDisp, theScreen)-eHIGH-abs(y);

   class.res_name  = "ImageX";
   class.res_class = "ImageX";

   hints.min_aspect.x = im_height + BELT_W;
   hints.max_aspect.x = im_height + BELT_W;
   hints.min_aspect.y = im_height;
   hints.max_aspect.y = im_height;
   hints.flags |= PAspect;

   hints.x = x;                 hints.y = y;
   hints.width = aWIDE;         hints.height = eHIGH;
   hints.max_width = aWIDE;     hints.max_height = eHIGH;
   hints.flags |= PMaxSize;

   hints.min_width = aWIDE;     hints.min_height = eHIGH;;
   hints.flags |= PMinSize;

   attr.background_pixel = bcol;
   attr.border_pixel     = fcol;
   attrmask = CWBackPixel | CWBorderPixel;

   theWindow = XCreateWindow(theDisp, rootW, x, y, aWIDE, eHIGH, 2,
      CopyFromParent, CopyFromParent, CopyFromParent, attrmask, &attr);

   if (!theWindow)
      FatalError("Can't open window (are X11 windows running ?). AJ");

   XSetClassHint(theDisp, theWindow, &class);
   XSetStandardProperties(theDisp, theWindow, T_name, T_name, None,
                          argv, argc, &hints);
}

/* ----------------- */ /* Set color or grey array of the Image */
   void
   colmap_init()
/* ----------------- */
{
   if (YES_color)  color_init(COL_MIN, spectrum);
   else            grey_init();
}

/* -------------------- */       /* Set grey array of the Image  */
   void
   grey_init()
/* -------------------- */
{
   register int i, k, m;
   float a;

   a = 200./ncolors;

   if (No_Grey_init) {
      for (i=0; i < ncolors; i++) {
         k = 255.*pow((a*i+55.)/255., gamm) + .5;
         m = min_max_col(k << 8);
         INgrey[i] = m;
         MYgrey[i].pixel = pixels[i];
         MYgrey[i].red   = m;
         MYgrey[i].green = m;
         MYgrey[i].blue  = m;
         MYgrey[i].flags = DoRed|DoGreen|DoBlue;
      }
      No_Grey_init = 0;
   }
   if ( AJ_PseudoColor ) XStoreColors(theDisp, CMap, MYgrey, ncolors);
   else                AJ_StoreColors(theDisp, CMap, MYgrey, ncolors, 0);
}

/* ----------------- */ /* Set color array of the Image */
   void
   color_init(a1,a2)    /* OK but sharp edge on green near yellow */
   int a1, a2;
/* ----------------- */
{
   double da, an, c, n, s, sb, cb, ak, ab;
   register int i, m, r=0, g=0, b=0;

   if (No_Color_init) {
      FIRST = 1;        SQUE_NR = ncolors;
      n = ncolors;
      ak = 105.; s = 150.; c = s/60.;
      ab = 65.; sb = 190.; cb = s/60.;
      m=256;
      an = a1;
      da = a2 - a1;     da = da/n;
      an = an-da+360.;

      for(i=0; i < ncolors; i++) {
        an += da;   an=fmod(an,360.);
        if((an >= 0) && (an < 120.)) {
          r = 255.*pow((ak + min(s,(120. - an)*c))/255., gamm) +.5;
          g = 255.*pow((ak + min(s,an*c))/255., gamm) +.5;
          b=0;
        }
        if((an >= 120.) && (an < 240.)) {
          r = 0;
          g = 255.*pow((ak + min(s ,(240. - an)*c))/255., gamm) +.5;
          b = 255.*pow((ab + min(sb,(an - 120.)*cb))/255., gamm) +.5;
        }
        if(an >= 240.) {
          r =  255.*pow((ak + min(s,(an - 240.)*c))/255., gamm) +.5;
          g = 0;
          b = 255.*pow((ak + min(s,(360. - an)*c))/255., gamm) +.5;
        }
        MYcol[i].pixel = pixels[ncolors-1-i];
        MYcol[i].red   = r*m;
        MYcol[i].green = g*m;
        MYcol[i].blue  = b*m;
        MYcol[i].flags = DoRed|DoGreen|DoBlue;
      }
      No_Color_init = 0;
   }
   if ( AJ_PseudoColor ) XStoreColors(theDisp, CMap, MYcol, ncolors);
   else                AJ_StoreColors(theDisp, CMap, MYcol, ncolors, 1);
}

/* --------------- */
   void
   c_rotate(k)
   int k;
/* --------------- */
{
   if (YES_color) color_rotate(k);
   else           grey_rotate(-k);
}


/* --------------- */
   void
   color_rotate(k)
   int k;
/* --------------- */
{
   register int i, j;

   FIRST = 1;
   SQUE_NR = ncolors;


   if(k >= 0) {
     for(i=0; i < k; i++) {
     tmp1[i] = MYcol[i].red;
     tmp2[i] = MYcol[i].green;
     tmp3[i] = MYcol[i].blue;
     }
     for(i=0; i < ncolors - k; i++) {
       MYcol[i].red   = MYcol[i+k].red;
       MYcol[i].green = MYcol[i+k].green;
       MYcol[i].blue  = MYcol[i+k].blue;
     }
     for(i = ncolors - k, j = 0; i < ncolors; i++, j++) {
       MYcol[i].red   = tmp1[j];
       MYcol[i].green = tmp2[j];
       MYcol[i].blue  = tmp3[j];
     }
   }
   else {
     k = -k;
     for(i = ncolors - 1; i >= ncolors - k; i--) {
     tmp1[i] = MYcol[i].red;
     tmp2[i] = MYcol[i].green;
     tmp3[i] = MYcol[i].blue;
     }
     for(i = ncolors - 1; i >= k; i--) {
       MYcol[i].red   = MYcol[i-k].red;
       MYcol[i].green = MYcol[i-k].green;
       MYcol[i].blue  = MYcol[i-k].blue;
     }
     for(i = k-1, j = ncolors - 1; i >=0; i--, j--) {
       MYcol[i].red   = tmp1[j];
       MYcol[i].green = tmp2[j];
       MYcol[i].blue  = tmp3[j];
     }
   }
   if ( AJ_PseudoColor ) XStoreColors(theDisp, CMap, MYcol, ncolors);
   else                AJ_StoreColors(theDisp, CMap, MYcol, ncolors, 1);
}

/* --------------- */
   void
   grey_rotate(k)
   int k;
/* --------------- */
{
   register int i, j;

   FIRST = 1;
   SQUE_NR = ncolors;


   if(k >= 0) {
     for(i=0; i < k; i++) {
     tmp1[i] = MYgrey[i].red;
     tmp2[i] = MYgrey[i].green;
     tmp3[i] = MYgrey[i].blue;
     }
     for(i=0; i < ncolors - k; i++) {
       MYgrey[i].red   = MYgrey[i+k].red;
       MYgrey[i].green = MYgrey[i+k].green;
       MYgrey[i].blue  = MYgrey[i+k].blue;
     }
     for(i = ncolors - k, j = 0; i < ncolors; i++, j++) {
       MYgrey[i].red   = tmp1[j];
       MYgrey[i].green = tmp2[j];
       MYgrey[i].blue  = tmp3[j];
     }
   }
   else {
     k = -k;
     for(i = ncolors - 1; i >= ncolors - k; i--) {
     tmp1[i] = MYgrey[i].red;
     tmp2[i] = MYgrey[i].green;
     tmp3[i] = MYgrey[i].blue;
     }
     for(i = ncolors - 1; i >= k; i--) {
       MYgrey[i].red   = MYgrey[i-k].red;
       MYgrey[i].green = MYgrey[i-k].green;
       MYgrey[i].blue  = MYgrey[i-k].blue;
     }
     for(i = k-1, j = ncolors - 1; i >=0; i--, j--) {
       MYgrey[i].red   = tmp1[j];
       MYgrey[i].green = tmp2[j];
       MYgrey[i].blue  = tmp3[j];
     }
   }
   if ( AJ_PseudoColor ) XStoreColors(theDisp, CMap, MYgrey, ncolors);
   else                AJ_StoreColors(theDisp, CMap, MYgrey, ncolors, 0);
}

/* -------- */
   void
   c_swap()
/* -------- */
{
   if (YES_color) color_swap();
}

/* --------------- */
   void
   color_swap()
/* --------------- */
{
   register int i, k;

   FIRST = 1; SQUE_NR = ncolors;
   k = ncolors - 1;

   for(i=0; i < ncolors; i++) {
    tmp1[i] = MYcol[i].red;
    tmp2[i] = MYcol[i].green;
    tmp3[i] = MYcol[i].blue;
   }
   for(i=0; i < ncolors ; i++) {
     MYcol[i].red   = tmp1[k-i];
     MYcol[i].green = tmp2[k-i];
     MYcol[i].blue  = tmp3[k-i];
   }
   if ( AJ_PseudoColor ) XStoreColors(theDisp, CMap, MYcol, ncolors);
   else                AJ_StoreColors(theDisp, CMap, MYcol, ncolors, 1);
}

/* ------------- */ /* Modify span or contrast */
   void
   c_squeeze(d)
   int d;
/* ------------- */
{
   if (YES_color) color_squeeze(d);
   else           grey_contrast(-2*d);
}

/* -------------------- */      /* Modify contrast of the Image */
   void
   grey_contrast(d_lev)
   int d_lev;
/* -------------------- */
{
   register int i, k, m, delta, dx;

   dx = max(((abs(INgrey[ncolors-1] - INgrey[0])>>6)/ncolors), 1);
   delta = d_lev*dx;
   for(i=0; i < ncolors; i++) {
      k = INgrey[i] += i*delta;
      m = min_max_col(k);
      MYgrey[i].red   = m;
      MYgrey[i].green = m;
      MYgrey[i].blue  = m;
   }
   if ( AJ_PseudoColor ) XStoreColors(theDisp, CMap, MYgrey, ncolors);
   else                AJ_StoreColors(theDisp, CMap, MYgrey, ncolors, 0);
}

/* -------------------- */ /* Modify span of colors*/
   void
   color_squeeze(d)
   int d;
/* -------------------- */
{
   static unsigned int x_arr[NCOLORS];
   register int i, ex, w;

   w = ncolors/2;
   SQUE_NR = max(1, SQUE_NR - d);
   for (ex = 0; ex < ncolors; ex++)  x_arr[ex] = SQUE_NR*(ex - w)/ncolors + w;

   if(FIRST)
   for(i=0; i < ncolors; i++) {
     tmp1[i] = MYcol[i].red;
     tmp2[i] = MYcol[i].green;
     tmp3[i] = MYcol[i].blue;
     FIRST = 0;
   }
   for(i=0; i < ncolors; i++) {
     if(x_arr[i] > uucolors) {
       MYcol[i].red   = 0;
       MYcol[i].green = 0;
       MYcol[i].blue  = 0;
     }
     else {
       MYcol[i].red   = tmp1[x_arr[i]];
       MYcol[i].green = tmp2[x_arr[i]];
       MYcol[i].blue  = tmp3[x_arr[i]];
     }
   }
   if ( AJ_PseudoColor ) XStoreColors(theDisp, CMap, MYcol, ncolors);
   else                AJ_StoreColors(theDisp, CMap, MYcol, ncolors, 1);
}

/* ------------------ */ /* Modify brightness (color or B&W) */
   void
   c_bright(d)
   int d;
/* ------------------ */
{
   if (YES_color) color_bright(d);
   else           grey_change(-2*d);
}

/* ------------------ */   /* Modify brightness of the Image  */
   void
   grey_change(d_lev)
   int d_lev;
/* ------------------ */
{
   register int i, k, m, delta, dx;

   dx = abs((INgrey[ncolors-1] - INgrey[0])/ncolors);
   delta = d_lev*dx;
   for (i=0; i < ncolors; i++) {
      k = INgrey[i] += delta;
      m = min_max_col(k);
      MYgrey[i].red   = m;
      MYgrey[i].green = m;
      MYgrey[i].blue  = m;
   }
   if ( AJ_PseudoColor ) XStoreColors(theDisp, CMap, MYgrey, ncolors);
   else                AJ_StoreColors(theDisp, CMap, MYgrey, ncolors, 0);
}

/* ------------------ */ /* Modify brightness of the Image */
   void
   color_bright(d)
   int d;
/* ------------------ */
{
   double c;
   register int i;
   c = d; c = 1. - c*.005;
   FIRST = 1; SQUE_NR = ncolors;

   for(i=0; i < ncolors; i++) {
     MYcol[i].red   = min_max_col(c*MYcol[i].red);
     MYcol[i].green = min_max_col(c*MYcol[i].green);
     MYcol[i].blue  = min_max_col(c*MYcol[i].blue);
   }
   if ( AJ_PseudoColor ) XStoreColors(theDisp, CMap, MYcol, ncolors);
   else                AJ_StoreColors(theDisp, CMap, MYcol, ncolors, 1);
}

/* -------------- */ /* It assigns index for one of standard 16 colors */
   int
   STD_colors(cx)    /* First index is 1, last 16 (equv. to 0 - black col.). */
   int cx;
/* -------------- */
{
   XColor  any_col;

 if ( AJ_PseudoColor ) {
   if( cx > NUM_STD_COLORS ) return extra_color_x11[cx-NUM_STD_COLORS-1] ;

   if ( color_x11[cx-1] + 1 ) return(color_x11[cx-1]);
   else {
      any_col.red   = Solid_color[cx-1].red;
      any_col.green = Solid_color[cx-1].green;
      any_col.blue  = Solid_color[cx-1].blue;
      any_col.flags = DoRed | DoGreen | DoBlue;

      if ( !XAllocColor(theDisp, CMap, &any_col) )
         FatalError ("XAllocColor problem in STD_colors(). AJ");

      return( color_x11[cx-1] = any_col.pixel );
   }
 }
 else return 0;
}

/* ---------------------------------- */   /* Create Image from the im_arr */
   XImage *Load_Any_Arr(im_arr, x, y)      /* Usage: theImage = Load_An... */
   short int im_arr[];                     /* indexed or RGB colors        */
   int       x, y;
/* ---------------------------------- */
{
   XImage    *image;

   if ( AJ_PseudoColor ) image = Load_Any_ind(im_arr, x, y);
   else                  image = Load_Any_RGB(im_arr, x, y);

   return image;
}

/* ---------------------------------- */   /* Create Image from the im_arr */
   XImage *Load_Any_RGB(im_arr, x, y)      /* Indices for RGB screen       */
   short int im_arr[];
   int       x, y;
/* ---------------------------------- */
{
   register int   i, k;
   int        Width, Hight, last;
   char      *Image;
   XImage    *image;
   unsigned char  *a8, *i8, *m8;
   unsigned short *a16, *i16;
   unsigned int   *a32, *i32;
 
   Width = x;      /* Image width  */
   Hight = y;      /* Image higth  */
   last  = x * y;

   for (i=0; i < last; i++)
      if ( im_arr[i] < 0 ) im_arr[i] = STD_indx[-im_arr[i]-1];

   image = XCreateImage(theDisp, theVisual, Planes, ZPixmap, 0, NULL,
                                Width, Hight, 32, 0);
   if (! image) FatalError("Unable create image in Load_R_G_B_Arr().");

   Image = (char *) malloc(image->bytes_per_line*Hight);
   if (!Image) FatalError("Not enough memory for the Image");
   image->data = Image;

   k = 0;
   switch( bperpix ) {
      case 32:
         a32 = (unsigned int  *) image->data;
         i32 = (unsigned int  *) AJ_RGB;
         for (i=0; i < last; i++) {
            *a32++ =  i32[im_arr[k++]];
         }
      break ;
 
      case 24:
         a8 = (unsigned char *) image->data;
         i8 = (unsigned char *) AJ_RGB;
         for (i=0; i < last; i++) {
            m8 =  i8 + im_arr[k++] * 3;
            *a8++ = *m8++;
            *a8++ = *m8++;
            *a8++ = *m8++;
         }
      break ;

      case 16:
         a16 = (unsigned short *) image->data;
         i16 = (unsigned short *) AJ_RGB;
         for (i=0; i < last; i++) {
            *a16++ =  i16[im_arr[k++]];
         }
      break ;
 
      case 8:
         a8 = (unsigned char *) image->data;
         i8 = (unsigned char *) AJ_RGB;
         for (i=0; i < last; i++) {
            *a8++ =  i8[im_arr[k++]];
         }
      break ;
   }
   return image;
}

/* ---------------------------------- */   /* Create Image from the im_arr */
   XImage *Load_Any_ind(im_arr, x, y)      /* Usage: theImage = Load_An... */
   short int im_arr[];                     /* Uses own 16 colors for nega- */
   int       x, y;                         /* tive numbers (-1 - -16).     */
/* ---------------------------------- */   /* 8 and 12 bit planes work OK. */
{
  register char *ptr;
  register int   i, j, k, iN, iE;
  int        Width, Hight;
  char      *Image;
  XImage    *image;

  Width = x;      /* Image width  */
  Hight = y;      /* Image higth  */

  image = XCreateImage(theDisp, theVisual, Planes, ZPixmap, 0, NULL,
                                Width, Hight, 32, 0);
  if (! image) FatalError("Unable create image in Load_index_Arr().");

  Image = (char *) malloc(image->bytes_per_line*Hight);
  if (!Image) FatalError("Not enough memory for the Image");
  image->data = Image;
  iN = (image->bits_per_pixel + 7) / 8;    /* 1 or 2 pointer increment */
  iE = iN - 1;                             /* 0 or 1 for next pointer */

  ptr = Image;
  k = 0;
  for (i=0; i<Hight; i++)
     for (j=0; j<Width; j++, ptr += iN)
        if(im_arr[k] >= 0) {
          *ptr = pixels[im_arr[k]] >> 8;
          *(ptr + iE) = pixels[im_arr[k++]];
        }
        else {
          *ptr = STD_colors(-im_arr[k]) >> 8;
          *(ptr + iE) = STD_colors(-im_arr[k++]);
        }

  return image;
}

/* -------------------------------------------- */
   void
   New_Cursor(Disp, Wind, shape, fg_col, bg_col)
   Display     *Disp;
   Window       Wind;
   unsigned int shape;
   char        *fg_col, *bg_col;
/* -------------------------------------------- */
/* Assigns new cursor to the Window. Call after creating the Window */
{
   XColor       Im_cur_fg, Im_cur_bg;
   Colormap     CM;
   Cursor       cursor;

   CM = DefaultColormap(Disp, DefaultScreen(Disp));

   cursor = XCreateFontCursor(Disp, shape);        /* new cursor shape */
   XDefineCursor(Disp, Wind, cursor);
   XParseColor(Disp, CM, fg_col, &Im_cur_fg);    /* forground  color */
   XParseColor(Disp, CM, bg_col, &Im_cur_bg);    /* background color */
   XRecolorCursor(Disp, cursor, &Im_cur_fg, &Im_cur_bg);
}

/* ----------------------------------------------------------------
   Read file subroutine fo use in C.        A.Jesmanowicz, MCW 1991
	return error :	0 - OK,
			1 - opening problem,
			2 - file longer then array.
	fname :	file name.
	size  :	on input - max size of the arr or 0 for any length,
		on output- real size of the file (and arr in bytes).
	arr   :	returned file as array.
   ---------------------------------------------------------------- */
/* ----------------------------- */
   int  read_iqm(fname,size,arr)
   int  *size;
   char fname[],arr[];
/* ----------------------------- */
{
	int	isize = *size;
	int	fp;				/* file descriptor  */
	struct	stat file_stat;			/* status structure */
        ssize_t vv;

	if ((fp = open(fname, O_RDONLY)) <= 0)	/* file must exist */
           return(1); 		             	/* or error = 1.   */

	fstat(fp, &file_stat);			/* get file size in bytes   */

	if(file_stat.st_size > isize && isize)	/* file can not be too long */
	   return(2);     	                /* or error = 2.	    */

	*size =  file_stat.st_size;		/* return file size */

	vv = read(fp, arr, file_stat.st_size);	/* read whole file  */
	close(fp);
	return(0);                              /* no error : 0 */
}

/* ----------------------------------------------------------------
   Write file subroutine fo use in C.       A.Jesmanowicz, MCW 1991
	return error :	0 - OK,
			1 - opening problem,
			2 - negative length.
	fname :	file name.
	size  :	size of the file.
	arr   :	file array.
   ---------------------------------------------------------------- */
/* ----------------------------- */
   int WRite_iqm(fname,size,arr)
   int  *size;
   char fname[],arr[];
/* ----------------------------- */
{
	int	isize = *size;
	int	fp;				/* file descriptor  */
        ssize_t vv;

	if(isize < 0)				/* size has to be real */
	   return(2);				/* or error = 2.       */

	if ((fp = open(fname, O_WRONLY|O_CREAT|O_EXCL,0644)) <= 0)
	   return(1);				/* file must not exist */
						/* or error = 1.       */

	vv = write(fp, arr, isize);		/* write whole file */
	close(fp);
	return(0);
}


/*  Directory names (if present) are skipped. and number is added in front */
/* -------------- */
   void
   top_nr_name(name)
   char *name;
/* -------------- */
{
   register int i, k;
   char         *p_name;

   k = strlen( name );
   p_name = name;
   for(i = 0; i < k; i++) {
      if(name[i] != 32 && name[i]) {
         if(name[i] == 47) p_name = &name[i+1];
      }
      else {
         name[i] = 0;
         break;
      }
   }
   sprintf(T_name, "#%d   %s", Im_Nr+1, p_name);
   sprintf(I_name, "G %s", p_name);
   sprintf(G_name, "G %s , EPI_grid", p_name);
}

/* ------------------ */ /* resample image into 256x256 frame using st_? */
   void Resample(im)
   MRI_IMAGE * im ;
/* ------------------ */
{
   register short int *a;
   register int       *s, i1, ix, lx, i, j, k, l, kk, mm, dkk, ii, ij, is;
   register int       II, J=0, IJ, IJk, ijl, k0, IM2, lx2, ly, IJK;
   int ndim ;
   short * all ;

   ndim = im->nx ;
   all  = MRI_SHORT_PTR(im) ;

   if(ndim == EPX4 ) {                    /* 32x32 pixels format */
      s  = st_8;
      i1 = 3;
      is = 6;
      ix = 8;
      lx = 32;
      ly = 32;
   }
   else if(ndim == EPX1 ) {                    /* 64x64 pixels format */
      s  = st_4;
      i1 = 1;
      is = ix = 4;
      lx = 64;
      ly = 64;
   }
   else if(ndim == EPX2) {             /* 128x128 pixels format */
      s  = st_2;
      i1 = 0;
      is = ix = 2;
      lx = 128;
      ly = 128;
   }
   else if ( (ndim == EPX3) || (ndim == EPX5) ) { // 256x256 or 512 pixels => no resample
      a = all;
      lx = ndim * ndim ;
      for (i=0; i < lx ; i++)
         tmp_imx[i] = imx[i] =  a[i];
      return;
   }
   else {
      printf("\n\n !!! Wrong file format in Resample() function !!!\n\n");
      exit(3);
   }

   /* Images of different resolutions will register for c_f=0 (and i1=0)  */
   /* For c_f=1 frame will be centered on the pixel!         AJ 10.18.2000 */
   if ( c_f == 0 ) i1 = 0;

   a = all;

   lx2 = lx*ly;
   IM2 = im_height*im_height;
   k0  = ix*(2*ix-1) - 1;
   dkk = 2*ix*ix;

   i = ly - 1;                     /* horizontal edge with wrap around */
   ii = i*lx;
   II = (i*ix+i1)*im_height + i1;
   for (j=0; j < lx - 1; j++) {
      ij  = ii+j;
      ijl = ij+lx;
      if ( ijl >= lx2 ) ijl -= lx2;
      IJ  = II + j*ix;
      for (k=0; k < ix; k++) {
         IJk = IJ + k*im_height ;
         if ( IJk >= IM2 ) IJk -= IM2;
         kk = k0 - ((k * ix) << 1);
         mm = kk + dkk;
         for (l=0; l < ix; l++) {
            tmp_imx[IJk+l]
              = imx[IJk+l] = ( a[ij]    * s[kk-l]
                             + a[ij+1]  * s[kk-l+ix]
                             + a[ijl]   * s[mm-l]
                             + a[ijl+1] * s[mm-l+ix]) >> is;
         }
      }
   }

   j = lx - 1;
   for (i=0; i < ly - 1; i++) {       /* vertical edge with wrap around */
      ii = i*lx;
      II = (i*ix+i1)*im_height;
      ij  = ii+j;
      ijl = ij+lx;
      J   = j*ix + i1;
      IJ  = II + J;
      for (k=0; k < ix; k++) {
         IJk = IJ + k*im_height ;
         kk = k0 - ((k * ix) << 1);
         mm = kk + dkk;
         for (l=0; l < ix; l++) {
            IJK = IJk;
            if ( (J + l) >= im_height ) IJK = IJk - im_height;
            if ( IJK >= IM2 ) IJK -= IM2;
            tmp_imx[IJK+l]
              = imx[IJK+l] = ( a[ij]      * s[kk-l]
                             + a[ij-lx+1] * s[kk-l+ix]
                             + a[ijl]     * s[mm-l]
                             + a[ijl-lx+1]* s[mm-l+ix]) >> is;
         }
      }
   }

   i = ly - 1;                /* last lower right corner with wrap around */
   j = lx - 1;
   ij = lx*ly - 1;
   ijl = lx - 1;
   IJ = (i*ix+i1)*im_height + j*ix + i1;
   for (k=0; k < ix; k++) {
      IJk = IJ + k*im_height ;
      if ( IJk >= IM2 ) IJk -= IM2;
      kk = k0 - ((k * ix) << 1);
      mm = kk + dkk;
      for (l=0; l < ix; l++) {
         IJK = IJk;
         if ( (J + l) >= im_height ) IJK = IJk - im_height;
         tmp_imx[IJK+l]
           = imx[IJK+l] = ( a[ij]      * s[kk-l]
                          + a[ij-lx+1] * s[kk-l+ix]
                          + a[ijl]     * s[mm-l]
                          + a[ijl-lx+1]* s[mm-l+ix]) >> is;
      }
   }

   for (i=0; i < ly - 1; i++) {        /* the rest of the field */
      ii = i*lx;
      II = (i*ix+i1)*im_height + i1;
      for (j=0; j < lx - 1; j++) {
         ij  = ii+j;
         ijl = ij+lx;
         J   = j*ix;
         IJ  = II + J;
         for (k=0; k < ix; k++) {
            IJk = IJ + k*im_height ;
            kk = k0 - ((k * ix) << 1);
            mm = kk + dkk;
            for (l=0; l < ix; l++) {
               tmp_imx[IJk+l]
                 = imx[IJk+l] = ( a[ij]    * s[kk-l]
                                + a[ij+1]  * s[kk-l+ix]
                                + a[ijl]   * s[mm-l]
                                + a[ijl+1] * s[mm-l+ix]) >> is;
            }
         }
      }
   }
}

/* ------------ */ /* Don't use before first Resample() and Load_Any_Arr(). */
   void            /* Load_Any_Arr() allocate memory for theImage. */
   Put_image(n)    /* Put_image() needs THIS memory. */
   int n;
/* ------------ */
{
   register int    min2, max2, i, dd;
   register float  coef2, del2;
   int             xtemp1, xtemp2, ytemp1, ytemp2, xmat, ymat, xm, ym, xxx;

   if ( (n == old_im) && AJ_PseudoColor ) {
      for (i = 0; i < IM_ARR; i++) imx[i] = tmp_imx[i];
   }
#ifdef USE_MCW 
   else if ( n < 0 ){
      mcw_load() ;
      for( i=0 ; i < IM_ARR ; i++ ) tmp_ar[i] = mcw_im[i] ;
      SIZE_tmp_ar( im_height ) ;
      Resample(im_tmp_ar);
      strcpy(T_name,"Medical College of Wisconsin") ;
      strcpy(G_name,"Medical College of Wisconsin") ;
      strcpy(I_name,"MCW") ;
      Allow_new_name(Argc, Argv); 
      old_im = -9999 ;
   }
#endif
   else {
      nowim   = SAR(n) ;
      nowsize = SIZ(n) ;
      if( RWC_autoscale || (n >= npoints) ){   /* Autoscale these */
         min2 =  32767;
         max2 = -32768;
         for (i=0; i < nowsize; i++) {
            if ( nowim[i] < min2 ) min2 = nowim[i];
            if ( nowim[i] > max2 ) max2 = nowim[i];
         }
         del2 = max2 - min2;
         if (del2 < 1.) del2 = 1.;
         coef2 = ( (float) NC - 1.) / del2;
      }
      else {
         coef2 = coef1;
         min2 = min1;
      }
                             /* make differential single image */
      if ( (n<npoints) && (diff_im || avr_grp) ) {
         if ( ! avr_grp )
            for (i=0; i < ar_size; i++) tmp_ar[i] = nowim[i] - ref_ar[i];

                              /* make average image */
         else {
            Av_1 = n;
            Av_2 = Av_1 + Av_length - 1;
            for (i=0; i< ar_size; i++) av_ar[i] = 0;
            for (i=0; i < ar_size; i++) {
               for (j=Av_1; j <= Av_2; j++) av_ar[i] += SAR(j)[i];
            }
            for (i=0; i < ar_size; i++)  av_ar[i] /= Av_length;

                              /* average against ref image */
            if ( avr_grp && diff_im )
               for (i=0; i < ar_size; i++) tmp_ar[i] = av_ar[i] - ref_ar[i];

                              /* pure average image */
            else
               for (i=0; i < ar_size; i++) tmp_ar[i] = av_ar[i];

         }
                              /* renormalize image */
         min2 =  32767;
         max2 = -32768;
         for (i=0; i < ar_size; i++) {
            if ( tmp_ar[i] < min2 ) min2 = tmp_ar[i];
            if ( tmp_ar[i] > max2 ) max2 = tmp_ar[i];
         }
         del2 = max2 - min2;
         if (del2 < 1.)  del2 = 1.;
         coef2 = ( (float) NC - 1.) / del2;
         for (i=0; i < ar_size; i++)
            tmp_ar[i] = (int)((float)(tmp_ar[i]-min2)*coef2 + .5);
      }
      else if( LSQ_imedit[LSQ_code] != LSQ_EDIT_NONE ){
         int lsqnum , i,j ;
         float sum ;

         lsqnum = LSQ_refcount - LSQ_imedit[LSQ_code] ;

         min2 =  32767;
         max2 = -32768;
         for( i=0 ; i < nowsize ; i++ ){
            sum = nowim[i] ;
            for(j=0 ; j < lsqnum ; j++ )
               sum -= LSQ_ref[j]->ts[n] * LSQ_fit[j][i] ;
            tmp_ar[i] = sum ;
            if ( tmp_ar[i] < min2 ) min2 = tmp_ar[i];
            if ( tmp_ar[i] > max2 ) max2 = tmp_ar[i];
         }
         del2 = max2 - min2;
         if (del2 < 1.)  del2 = 1.;
         coef2 = ( (float) NC - 1.) / del2;
         for (i=0; i < ar_size; i++)
            tmp_ar[i] = (int)((float)(tmp_ar[i]-min2)*coef2 + .5);
      }
      else{                                       /* Normal Image */
         for (i=0; i < nowsize; i++)              /* renormalize data */
            tmp_ar[i] = (int)((float)(nowim[i]-min2)*coef2 + .5);
      }

      SIZE_tmp_ar( allim[n]->nx ) ;

      Resample(im_tmp_ar);
      top_nr_name(allim[n]->name) ;
      Allow_new_name(Argc, Argv);
      DrawTopWindow();
      old_im = n;
   }

   if( !RWC_framehide ){

      if ( x_mag == 1 ) dd = 0;   /* extra correction for 64 and 128 */
      else              dd = 1;
      xxx = max(0, x_mag/2 - 1);        /* stuff for proper pivot and frame */
      xm = x_mag - 1;
      ym = x_mag - 1;
      xmat = x_mag * (matx - 1) + x_mag/2 + 1;
      ymat = x_mag * (maty - 1) + x_mag/2 + 1;

      ytemp1 = x_mag * (ypoint + yc) + ym - dd;
      if (ytemp1 < 0)          ytemp1 += im_height;
      if (ytemp1 >= im_height) ytemp1 -= im_height;

      ytemp2 = x_mag * (ypoint + yc - maty + 1) - dd;
      if (ytemp2 < 0)          ytemp2 += im_height;
      if (ytemp2 >= im_height) ytemp2 -= im_height;

      for (i=0; i < xmat + xxx; i++) {
        xtemp1 = x_mag * (xpoint - xc) + i - dd;
        if (xtemp1 < 0)          xtemp1 += im_height;
        if (xtemp1 >= im_height) xtemp1 -= im_height;

        imx[ytemp1 * im_height + xtemp1] = fr_index;
        imx[ytemp2 * im_height + xtemp1] = fr_index;
      }

      xtemp1 = x_mag * (xpoint - xc) - dd;
      if (xtemp1 < 0)          xtemp1 += im_height;
      if (xtemp1 >= im_height) xtemp1 -= im_height;

      xtemp2 = x_mag * (xpoint - xc + matx - 1) + xm - dd;
      if (xtemp2 < 0)          xtemp2 += im_height;
      if (xtemp2 >= im_height) xtemp2 -= im_height;

      for (i=0; i < ymat; i++) {
         ytemp1 = x_mag * (ypoint + yc) - i + ym - dd;
         if (ytemp1 < 0)          ytemp1 += im_height;
         if (ytemp1 >= im_height) ytemp1 -= im_height;

         imx[ytemp1 * im_height + xtemp1] = fr_index;
         imx[ytemp1 * im_height + xtemp2] = fr_index;
      }
   } /* end if !RWC_framehide */

   if( RWC_do_overfim && !RWC_overhide ){
      short dont_overlay ;
      dont_overlay = fr_index ;
      RWC_short_overlay( im_height,im_height,imx , RWC_nxim,RWC_nyim ,
                         RWC_OVFLAG,dont_overlay, RWC_checker , RWC_imover ) ;
   }

   if ( !AJ_PseudoColor ) {
      Load_Next_Arr(theBelt, belt_arr, BELT_W, im_height);
      MResize(&expBelt,&eW,&eH,theBelt,eW,eH);
   }
   XPutImage(theDisp, theWindow, theGC, expBelt, 0, 0, eHIGH, 0, eW, eH);

   Load_Next_Arr(theImage, imx,im_height,im_height);
   MResize(&expImage,&eWIDE,&eHIGH,theImage,eWIDE,eHIGH);
   XPutImage(theDisp, theWindow, theGC, expImage,0, 0, 0, 0, eWIDE, eHIGH);
}

/* ---------------------------------- */   /* Create Image from the im_arr */
   void
   Load_Next_Arr(Image, im_arr, x, y)      /* Usage: Load_Next... */
   short int im_arr[];
   int       x, y;
   XImage    *Image;
/* ---------------------------------- */
{

   if ( AJ_PseudoColor ) Load_Next_ind(Image, im_arr, x, y);
   else                  Load_Next_RGB(Image, im_arr, x, y);
}

/* ---------------------------------- */
   void
   Load_Next_RGB(Image, im_arr, x, y)
   short int im_arr[];
   int       x, y;
   XImage    *Image;
/* ---------------------------------- */   /* RGB version */
{
   register int   i, k, last;
   byte      *a8, *i8, *m8;
   unsigned short *a16, *i16;
   unsigned int   *a32, *i32;
 
   last  = x * y;
 
   for (i=0; i < last; i++)
      if ( im_arr[i] < 0 ) im_arr[i] = STD_indx[-im_arr[i]-1];
 
   k = 0;
   switch( bperpix ) {
      case 32:
         a32 = (unsigned int  *) Image->data;
         i32 = (unsigned int  *) AJ_RGB;
         for (i=0; i < last; i++) {
            *a32++ =  i32[im_arr[k++]];
         }
      break ;
 
      case 24:
         a8 = (byte *) Image->data;
         i8 = (byte *) AJ_RGB;
         for (i=0; i < last; i++) {
            m8 =  i8 + im_arr[k++] * 3;
            *a8++ = *m8++;
            *a8++ = *m8++;
            *a8++ = *m8++;
         }
      break ;

      case 16:
         a16 = (unsigned short *) Image->data;
         i16 = (unsigned short *) AJ_RGB;
         for (i=0; i < last; i++) {
            *a16++ =  i16[im_arr[k++]];
         }
      break ;
 
      case 8:
         a8 = (byte *) Image->data;
         i8 = (byte *) AJ_RGB;
         for (i=0; i < last; i++) {
            *a8++ =  i8[im_arr[k++]];
         }
      break ;
   }
}

/* ---------------------------------- */   /* Create Image from the im_arr */
   void
   Load_Next_ind(Image, im_arr, x, y)      /* Usage: Load_Next... */
   short int im_arr[];                     /* Uses own 16 colors for nega- */
   int       x, y;                         /* tive numbers (-1 - -16).     */
   XImage    *Image;
/* ---------------------------------- */   /* 8 and 12 bit planes work OK. */
{
  register char *ptr;
  register int   i, j, k, iN, iE;
  int        Width, Hight;

  iN = (Planes + 7)/8;             /* 1 or 2, pointer increment */
  iE = iN - 1;                     /* 0 or 1 for next pointer */

  Width = x;      /* Image width  */
  Hight = y;      /* Image higth  */

  ptr = Image->data;
  k = 0;
  for (i=0; i<Hight; i++)
     for (j=0; j<Width; j++, ptr += iN)
        if(im_arr[k] >= 0) {
          *ptr = pixels[im_arr[k]] >> 8;
          *(ptr + iE) = pixels[im_arr[k++]];
        }
        else {
          *ptr = STD_colors(-im_arr[k]) >> 8;
          *(ptr + iE) = STD_colors(-im_arr[k++]);
        }
}

/* ---------------------------- */
   void
   Allow_new_name(argc, argv)
   int  argc;
   char *argv[];
/* ---------------------------- */
{
   XSetStandardProperties(theDisp, theWindow, T_name, T_name, None,
                          argv, argc, NULL);
   XSetStandardProperties(theDisp, GWindow, G_name, I_name, None,
			  argv, argc, NULL);
}

/* ----------------------------- */   /* plot scaled new_line to pixmap  */
   void plot_line()
/* ----------------------------- */
{
   register int i,j, m, temp, index, ix, iy, xtemp, ytemp;
   int lsqnum , lsqfit , base , use_base , bb ;
   float f0;

   m = ar_size;

   lsqnum = 0 ;
   lsqfit = 0 ;
   if( RWC_do_overfim && RWC_ideal != NULL && LSQ_fitim != NULL ){
      switch( LSQ_code ){
         default:          lsqnum = 0                ; lsqfit = 0 ; break ;
         case LSQ_FITORT:  lsqnum = LSQ_refcount - 1 ; lsqfit = 1 ; break ;
         case LSQ_SUBORT:  lsqnum = LSQ_refcount - 1 ; lsqfit = 0 ; break ;
         case LSQ_FITALL:  lsqnum = LSQ_refcount     ; lsqfit = 1 ; break ;
         case LSQ_SUBALL:  lsqnum = LSQ_refcount     ; lsqfit = 0 ; break ;
         case LSQ_SORFID:  lsqnum = LSQ_refcount - 1 ; lsqfit = 2 ; break ;
      }
   }

   /** RWCox: 2 Aug 1996
              load data from images into "val" array here,
              instead of in main loop.  this allows loading of 'base' **/

   base = 40000 ;
   for (ix=0; ix < matx; ix++) {
      xtemp = xpoint + ix - xc;
      if (xtemp < 0)        xtemp += im_size;
      if (xtemp >= im_size) xtemp -= im_size;
      for (iy=0; iy < maty; iy++) {
         ytemp = ypoint - iy + yc;
         if (ytemp < 0)        ytemp += im_size;
         if (ytemp >= im_size) ytemp -= im_size;
         index = ytemp * im_size + xtemp;
         for (i=0; i < npoints; i++){
            val[ix][iy][i] = bb = SAR(i)[index] ;
            if( bb < base ) base = bb ;
         }
      }
   }

   use_base = (RWC_groupbase && lsqnum == 0) ;

   for (ix=0; ix < matx; ix++) {
      xtemp = xpoint + ix - xc;
      if (xtemp < 0)        xtemp += im_size;
      if (xtemp >= im_size) xtemp -= im_size;
      for (iy=0; iy < maty; iy++) {
         ytemp = ypoint - iy + yc;
         if (ytemp < 0)        ytemp += im_size;
         if (ytemp >= im_size) ytemp -= im_size;
         index = ytemp * im_size + xtemp;

         if( lsqnum > 0 ){
            float sum ;
            for( i=0 ; i < npoints ; i++ ){
               sum = 0.0 ;
               for(j=0 ; j < lsqnum ; j++ )
                  sum += LSQ_ref[j]->ts[i] * LSQ_fit[j][index] ;

               switch( lsqfit ){
                  /*fall*/
                  case 2: LSQ_val[i] = LSQ_ref[j]->ts[i] * LSQ_fit[j][index] ;
                  case 0: val[ix][iy][i] -= sum ;
                          break ;
                  case 1: LSQ_val[i]      = sum ;
                          break ;
               }
            }

         }

         /** scale data **/

         if( use_base ){
            if ( AJ_base ) pmin[ix][iy] = 0;
            else           pmin[ix][iy] = base ;
         } else {
            pmin[ix][iy] = 40000;
            for (i=0; i < npoints; i++)
               pmin[ix][iy] = min(pmin[ix][iy],val[ix][iy][i]);
         }

         if (iscale > 0) {
           for (i=0; i < npoints; i++)
              plot[ix][iy][i] = (val[ix][iy][i]-pmin[ix][iy])*iscale;
           pmax[ix][iy] = pmin[ix][iy] + gy / iscale;
         }
         else {
            temp = -iscale;
            for (i=0; i < npoints; i++)
               plot[ix][iy][i] = (val[ix][iy][i]-pmin[ix][iy])/temp;
            pmax[ix][iy] = pmin[ix][iy] + gy * temp;
         }

         /** plot scaled data **/

         line_color("black");
         if ( AJ_sigma > 0. ) {
            AJ_off = AJ_nr/2;
            for (i=0; i < AJ_off; i++) {
               f_plot[i] = 0.;
               f_plot[npoints+i] = 0.;
            }
            f_plot[npoints+AJ_nr+1] = 0.;
            for (i=0; i < npoints; i++)
               f_plot[AJ_off+i] = plot[ix][iy][i];
            for (m=0; m < npoints; m++) {
               f0 = 0.;
               for (i=0; i <= AJ_nr; i++) f0 += AJ_gauss[i] * f_plot[m+i];
               i_plot[m] = f0 * AJ_norm;
            }
            for (i=0; i < npoints; i++) {
               a_line[i].x = xorigin[ix][iy] + i*gx/(npoints-1);
               a_line[i].y = idY - yorigin[ix][iy] - i_plot[i];
            }
         }
         else {
            for (i=0; i < npoints; i++) {
               a_line[i].x = xorigin[ix][iy] + i*gx/(npoints-1);
               a_line[i].y = idY - yorigin[ix][iy] - plot[ix][iy][i];
            }
         }
         XDrawLines(theDisp, pxWind, theGC, a_line, npoints, CoordModeOrigin);

         /*** draw the LSQ_val fit to the data as well **/

         if( lsqnum > 0 && lsqfit ){
            float *id = RWC_ideal->ts ;

            if (iscale > 0) {
              for (i=0; i < npoints; i++)
                 LSQ_plot[i] = (LSQ_val[i]-pmin[ix][iy])*iscale;
            }
            else {
               temp = -iscale;
               for (i=0; i < npoints; i++)
                  LSQ_plot[i] = (LSQ_val[i]-pmin[ix][iy])/temp;
            }
            for (i=0; i < npoints; i++) {
               a_line[i].x = xorigin[ix][iy] + i*gx/(npoints-1);
               a_line[i].y = idY - yorigin[ix][iy] - LSQ_plot[i];
               if( (matx <= 3) && (maty <= 3) && fabs(id[i]) >= 33333.0 )
                  Cpointer_PIXWIN( a_line[i].x , a_line[i].y , "blue" ) ;
            }
            line_color("red3");
            XDrawLines(theDisp, pxWind, theGC, a_line, npoints, CoordModeOrigin);
         }

      } /* end loop iy */
      XFlush(theDisp);
   } /* end loop ix */

   /*** draw FIM ref function ***/

   if( RWC_do_overfim             &&
       RWC_ideal != NULL          &&
       RWC_ideal->len >= npoints  &&
       kFI3_show_ref > 0             ){

      float idmax , idmin , val , yscal,yoff ;
      float *id = RWC_ideal->ts ;
      int ii , xg,yg , xbot,ybot,xtop,ytop ;
      XPoint one_line[2] ;

#define REFTS_FRAC 0.369  /* fraction of one graph that this takes up */

      idmax = -99999.0 ;
      idmin =  99999.0 ;

      /*** find scale factor for plot ***/

      for( ii=0 ; ii < npoints ; ii++ ){
         val = id[ii] ;
         if( fabs(val) < 33333.0 ){
            idmax = MAX(idmax,val) ;
            idmin = MIN(idmin,val) ;
         }
      }
      if( idmax >= 33333.0 || idmax <= -33333.0 || idmax <= idmin ) return ;

      yscal = REFTS_FRAC * gy / (idmax-idmin) ;
      yoff  = (0.97-REFTS_FRAC) * gy ;

      /*** scale into a_line (with no origins yet) ***/

      for (i=0; i < npoints; i++) {
         a_line[i].x = i*gx/(npoints-1);
         val = ( fabs(id[i]) < 33333.0 ) ? (id[i]) : (idmin) ;
         a_line[i].y = idY - (yoff+yscal*(val-idmin));
      }

      /*** for each line, for each plot, put into pixmap ***/

      line_color("red") ;

      for (i=0; i < npoints-1 ; i++) {
         if( fabs(id[i]) < 33333.0 && fabs(id[i+1]) < 33333.0 ){
            line_color("red") ;   /* red for "good" lines */
         } else {
            line_color("blue") ;  /* blue for "bad" lines */
         }

        /*** draw into every plot in matrix ***/

        if( kFI3_show_ref == 1 ){
           xbot = xc ; xtop = xbot+1 ;  /* just do center plot */
           ybot = yc ; ytop = ybot+1 ;
        } else {
           xbot = ybot = 0 ;            /* do all plots */
           xtop = matx;
           ytop = maty;
        }

        for( yg=ybot ; yg < ytop ; yg++ ){
           for( xg=xbot ; xg < xtop ; xg++ ){
              one_line[0].x = a_line[i].x   + xorigin[xg][yg] ;
              one_line[1].x = a_line[i+1].x + xorigin[xg][yg] ;
              one_line[0].y = a_line[i].y   - yorigin[xg][yg] ;
              one_line[1].y = a_line[i+1].y - yorigin[xg][yg] ;
              XDrawLines(theDisp, pxWind, theGC, one_line, 2, CoordModeOrigin);
              if( (matx <= 3) && (maty <=3) &&
                  kFI3_show_ref == 1 && fabs(id[i]) >= 33333.0 )
                 Cpointer_PIXWIN( one_line[0].x , one_line[0].y , "blue" ) ;
           }
        }  /* end of loops over boxes */
      } /* end of loop over points in plot */

   } /* end of FIM stuff */

}

/* ----------------------------- */ /* draw marker for current image  */
   void draw_marker()
/* ----------------------------- */ /* marker fixed (shifted by one im) AJJ */
{
   register int i, j, k, xo, yo, x1, dx;
   register float g, f0;

   if (mark) {
     line_color(color[color_index]);
     g = grid_coef * grid_far[grid_index + FT_grid];
     for (i=0; i < matx; i++) {
        for (j=1;j<=npoints/g;j++) {
           f0 = ((float) j * g - 1.) * (float) gx / (float) (npoints - 1);
           k = xorigin[i][0] + (int) f0;
           plotx(k,mdy1, 0);
           plotx(k,mdy1+maty*gy, 1);
        }
     }
   }

#ifdef FILL_WITH_CYAN
   line_color("cyan");
   xo = xorigin[xc][yc];
   yo = idY - yorigin[xc][yc] - gy;
   XFillRectangle(theDisp, pxWind, theGC, xo, yo, gx, gy);
#else
   xo = xorigin[xc][yc] ; yo = yorigin[xc][yc] ;
   g  = 5 ;
   for( j=1 ; j <= g ; j++ ){
      plotx( xo+j    , yo+j    , 0 ) ; 
      plotx( xo+j    , yo+gy-j , 1 ) ; 
      plotx( xo+gx-j , yo+gy-j , 1 ) ;
      plotx( xo+gx-j , yo+j    , 1 ) ; 
      plotx( xo+j    , yo+j    , 1 ) ; 
   }
#endif

   xo = xorigin[xc][yc];
   yo = idY - yorigin[xc][yc] - gy;

   if (diff_im) {
      line_color("blue");
      x1 = Im_1*gx/(npoints-1);
      dx = (Im_2 - Im_1)*gx/(npoints-1) + 1;
      XFillRectangle(theDisp, pxWind, theGC, xo + x1, yo, dx, gy);
   }

   if (avr_grp) {
      line_color("red");
      x1 = Av_1*gx/(npoints-1);
      dx = (Av_2 - Av_1)*gx/(npoints-1) + 1;
      XFillRectangle(theDisp, pxWind, theGC, xo + x1, yo, dx, gy);
   }

   if (mark) {
#ifdef FILL_WITH_CYAN
     line_color("white");
#else
     line_color(color[color_index]);
#endif
     g = grid_coef * grid_far[grid_index + FT_grid];
     for (j=1;j<=npoints/g;j++) {
        f0 = ((float) j * g - 1.) * (float) gx / (float) (npoints - 1);
        k = xorigin[xc][yc] + (int) f0;
        plotx(k,yorigin[xc][yc]   , 0);
        plotx(k,yorigin[xc][yc]+gy, 1);
     }
   }
}

/* ----------------------------- */   /* scale plot up and redraw  */
   void scale_up()
/* ----------------------------- */
{
   if (iscale > 0) iscale *= 2;
   else if (iscale < -2) iscale /= 2;
   else iscale = 1;
}

/* ----------------------------- */   /* scale plot up and redraw  */
   void scale_down()
/* ----------------------------- */
{
   if (iscale > 1) iscale /= 2;
   else if (iscale < 0) iscale *= 2;
   else iscale = -2;
}

/* ----------------------------- */   /* decrease matrix and redraw  */
   void mat_down(n)
   int n;
/* ----------------------------- */
{
   matx_0 = matx;
   maty_0 = maty;
   if ( n == 0 ) {
      matx--;
      if      (matx < 1)       matx = 1;
      else if (matx > MAT_MAX) matx = MAT_MAX;
   }
   maty--;
   if      (maty < 1)       maty = 1;
   else if (maty > MAT_MAX) maty = MAT_MAX;
   if ( (matx != matx_0) || (maty != maty_0) ) {
      init_mat();
      RWC_framehide = 0 ;
      Put_image(Im_Nr);
      redraw_graph();
      DrawSubWindow();
   }
}

/* ----------------------------- */   /* increase matrix and redraw  */
   void mat_up(n)
   int n;
/* ----------------------------- */
{
   matx_0 = matx;
   maty_0 = maty;
   if ( n == 0 ) {
      matx++;
      if      (matx < 1)       matx = 1;
      else if (matx > MAT_MAX) matx = MAT_MAX;
   }
   maty++;
   if      (maty < 1)       maty = 1;
   else if (maty > MAT_MAX) maty = MAT_MAX;

   if ( (matx != matx_0) || (maty != maty_0) ) {
      init_mat();
      RWC_framehide = 0 ;
      Put_image(Im_Nr);
      redraw_graph();
      DrawSubWindow();
   }
}

/* ----------------------------- */   /* initialize matrix stuff  */
   void init_mat()
/* ----------------------------- */
{
   int i, j;

   gx = gx_max / matx;
   gy = gy_max / maty;
   if (gx < 1) { gx = 1; gx_max = matx; }
   if (gy < 1) { gy = 1; gy_max = maty; }
   for (i=0; i < matx; i++) {
      for (j=0; j < maty; j++) {
         xorigin[i][j] = mdx1 + i * gx;
         yorigin[i][j] = mdy1 + j * gy;
      }
   }
   xc = matx/2;
   yc = (maty-1)/2;
}

/* ----------- */
   void
   init_grid()
/* ----------- */
{
   grid_far[GRID_NUM]   = grid_far[0] = 2;
   grid_far[GRID_NUM+1] = grid_far[1] = 5.;
   grid_far[GRID_NUM+2] = grid_far[2] = 10.;
   grid_far[GRID_NUM+3] = grid_far[3] = 20.;
   grid_far[GRID_NUM+4] = grid_far[4] = 50.;
   grid_far[GRID_NUM+5] = grid_far[5] = 100.;
   grid_far[GRID_NUM+6] = grid_far[6] = 200.;
   grid_far[GRID_NUM+7] = grid_far[7] = 500.;
   grid_index = 3;
}

/* ----------------------------- */   /* decrease grid spacing and redraw  */
   void grid_down()
/* ----------------------------- */
{
   int old;

   old = grid_index;
   grid_index--;
   if (grid_index < 0) grid_index = 0;
   if (grid_index!= old) {
      redraw_graph();
      DrawSubWindow();
   }
}

/* ----------------------------- */   /* increase grid spacing and redraw  */
   void grid_up()
/* ----------------------------- */
{
   int old;

   old = grid_index;
   grid_index++;
   if (grid_index >= GRID_NUM) grid_index = GRID_NUM - 1;
   if (grid_index!= old) {
      redraw_graph();
      DrawSubWindow();
   }
}

/* ------------ */
   void
   init_const()
/* ------------ */
{
   iscale = 4;
   auto_scale = .4;

   xpoint = im_size/2;
   ypoint = im_size/2;

   xspace = 5;
   yspace = 20;
   mytxt = 20;
   mdx1   = GL_DLX + 1;
   mdy1   = GB_DLY + 1;
   idX   = GL_DLX + gx_max + GR_DLX;
   idY   = GB_DLY + gy_max + GT_DLY;
   mark = 1;
   color_index = 0;
   matx = 3;
   maty = 3;
   init_mat();
}

/* ------------------------------  resize graph window */
   void redo_graph_window(xs, ys)
   int xs, ys;
/* ------------------------------ */
{
   idX = xs;   idY = ys;
   gx_max = idX - GL_DLX - GR_DLX;
   gy_max = idY - GB_DLY - GT_DLY;
   XFreePixmap(theDisp, pxWind) ;
   pxWind = XCreatePixmap(theDisp, GWindow, idX, idY, Planes);
   XMoveResizeWindow(theDisp, subWindow, 0, gy_max + GT_DLY, idX, GB_DLY);
   XMoveResizeWindow(theDisp, topWindow, 0, 0, idX, GT_DLY - 2);
   init_mat();
   redraw_graph();
}

/* ------------------------------------------------- Main link from main() C */
   void window_plane()
/* --------------------------------------------------- */
{
   int  i, out_loop = 0, tmpx;

   XGCValues   gcv;

    CreateGraphWindow(NULL, 0);      /* Set theWindow properties */
/* subWindow, topWindow and keys stuff is here ------- vvvvvvv ------- AJ*/

    ffc = XGetDefault(theDisp, Xdef_Name, "FKeyFore");
    fbc = XGetDefault(theDisp, Xdef_Name, "FKeyBack");

    /* Set normal default colors */
    ForeColor = BlackPixel(theDisp, theScreen);
    BackColor = WhitePixel(theDisp, theScreen);

    FKeyFore =  ForeColor;
    FKeyBack =  BackColor;

    if ( (ffc != NULL) && XAllocNamedColor(theDisp, CMap, ffc, &xcsd, &xced));
    else if (XAllocNamedColor(theDisp, CMap, "blue", &xcsd, &xced));
    else    FatalError ("XAllocNamedColor problem. AJ");
    FKeyFore=xcsd.pixel;

    if ( (fbc != NULL) && XAllocNamedColor(theDisp, CMap, fbc, &xcsd, &xced));
    else if (XAllocNamedColor(theDisp, CMap, "cyan", &xcsd, &xced));
    else    FatalError ("XAllocNamedColor problem. AJ");
    FKeyBack=xcsd.pixel;

   /** load key font from one of the candidate list **/

   tfont = XGetDefault(theDisp, Xdef_Name , "KeyFont");
   if( tfont != NULL ) tfont_hopefuls[0] = tfont ;
   else                tfont_hopefuls[0] = KFONT ;
   { int ifont ;
     for( ifont=0 ; tfont_hopefuls[ifont] != NULL ; ifont++ ){
        kfontinfo = XLoadQueryFont(theDisp, tfont_hopefuls[ifont]) ;
        if( kfontinfo != NULL ) break ;
     }
     if( kfontinfo == NULL ){
         FatalError("Can't open any text font!\n") ;
     }
     kfont = tfont_hopefuls[ifont] ;
   }
    keyfont = kfontinfo->fid;

    minwide = XTextWidth(kfontinfo, "MMMM", 4);
    keyhigh = kfontinfo->ascent + 7;
    for (i=0; i<N_KEYS; i++) {
       tmpx = XTextWidth(kfontinfo, xtkeys[i].st, strlen(xtkeys[i].st));
       keywide[i] = max(minwide, tmpx) + 2;
    }

    gcv.foreground = FKeyFore;
    gcv.function = GXcopy;
    gcv.font = keyfont;
    Fkeygc = XCreateGC(theDisp, GWindow, GCForeground|GCFunction|GCFont, &gcv);

    gcv.function = GXinvert;
    gcv.plane_mask = FKeyFore ^ FKeyBack;
    Fkeyigc = XCreateGC(theDisp, GWindow, GCFunction|GCPlaneMask, &gcv);

    Setup_subWindow();

    Setup_topWindow();

    Setup_keys();

/* SUbWindow, topWindow and keys stuff is here ------- ^^^^^^^ ------- AJ*/

                         /* Make dummy window for text and graphic entry */

   pxWind = XCreatePixmap(theDisp, GWindow, idX, idY, Planes);

   New_Cursor(theDisp, GWindow, XC_left_ptr, "blue", "yellow"); /* cursor */

   /* Text color and font */

   XSelectInput(theDisp, GWindow, ExposureMask | KeyPressMask
                 | ButtonPressMask | StructureNotifyMask);
   XMapWindow(theDisp,GWindow);		/* Show window first time */
   XMapSubwindows(theDisp, GWindow);    /* All keys stuff */


   for (i = 0; i < N_KEYS+2; i++) exp_done[i] = 0;

   while (1) {
      Window wind;

      XNextEvent(theDisp, &first_event);
                                          /* Wait for window on */
      switch (first_event.type) {
         case Expose: {
            XExposeEvent *exp_event = (XExposeEvent *) &first_event;
            wind = exp_event->window;

            if (wind ==   GWindow)
               exp_done[N_KEYS] = 1;
            else if (wind == subWindow) {
               DrawSubWindow();
               exp_done[N_KEYS+1] = 1;
            }
            else if (wind == topWindow) {
               DrawTopWindow();
               exp_done[N_KEYS+2] = 1;
            }
            else {
               for (i=0; i < N_KEYS; i++) {
                  if (wind == key[i].wid) {
                     DrawKey(i);
                     exp_done[i] = 1;
                  }
               }
            }
            out_loop = exp_done[N_KEYS] * exp_done[N_KEYS+1]
                                        * exp_done[N_KEYS+2];
            for (i = 0; i < N_KEYS; i++)  out_loop *= exp_done[i];
            if (out_loop){
               return;
            }
            Allow_smaller_gr(Argc, Argv);
         }
         break;

         default:                /* ignore unexpected events */
         break;
      }
   }
}

/* ----------------- */
   void
   Setup_subWindow()
/* ----------------- */
{
   unsigned long white; // long is OK here AJJ

   if (!(XAllocNamedColor(theDisp, CMap, "white", &any_col, &rgb_col)))
   FatalError ("XAllocNamedColor problem. AJ in Setup_subWindow()");
   white = any_col.pixel;

   sub_W_x = idX;
   sub_W_y = GB_DLY;
   subWindow = XCreateSimpleWindow(theDisp, GWindow, 0, gy_max + GT_DLY,
                     sub_W_x, sub_W_y, 1, white, white);
   XSelectInput(theDisp, subWindow, ExposureMask | ButtonPressMask |
                  ButtonReleaseMask);
}

/* --------------- */ /* Draw subWindow containing text info */
   void
   DrawSubWindow()
/* --------------- */
{
   float   diff_prcnt, f0, f1, f2;
   int     str_x = 5, x, y, loc, i;

   line_color("white");
   XFillRectangle(theDisp, subWindow, theGC, 0, 0, idX, GB_DLY);

   sprintf (strp,"Im. rot.");
   subW_TXT(str_x, 35, strp);
   sprintf (strp,"clockwise");
   subW_TXT(str_x, 20, strp);
   sprintf(strp, "%d*90 deg.", rot_nr);
   subW_TXT(str_x,  5, strp);
   str_x += XTextWidth(mfinfo ,"clockwise", strlen("clockwise")) + 30;

   sprintf (strp,"X: %d", xpoint);
   subW_TXT(str_x, 28, strp);
   sprintf (strp,"Y: %d", ypoint);
   subW_TXT(str_x, 13, strp);
   str_x += XTextWidth(mfinfo, "X: 000", strlen("X: 000")) + 20;

   if( SIZ(Im_Nr) == ar_size ){
      int cenval = SAR(Im_Nr)[ypoint*im_size + xpoint] ;

      if ( grid_timed && (FT_grid == GRID_NUM) ) {
         f0 = (float) (Im_Nr+1) / grid_far[FT_grid];
         sprintf (strp,"Pix. value:%6d at im #%d  %5.2f Hz",
                        cenval, Im_Nr+1, f0);
      }
      else
         sprintf (strp,"Pix. value: %d at im #%d", cenval, Im_Nr+1);
      subW_TXT(str_x, 20, strp);
      loc = str_x;

      if ( grid_timed && (FT_grid == GRID_NUM) ) {
         f0 = grid_coef * grid_far[grid_index + FT_grid] / grid_far[FT_grid];
         sprintf (strp,"Grid:%5.2f Hz", f0);
      }
      else
         sprintf (strp,"Grid:%g", grid_coef * grid_far[grid_index + FT_grid]);
      subW_TXT(str_x, 35, strp);

      str_x += XTextWidth(mfinfo, "Grid:00000f Hz",
                           strlen("Grid:00000f Hz")) + 20;
      sprintf(strp, "Num: %d", npoints);
      subW_TXT(str_x, 35, strp);

      if ( AJ_sigma > 0. ) {
         sprintf(strp, "Sig: %g", AJ_sigma);
         subW_TXT(GL_DLX + 440, 5, strp);
      }

      if( Im_Nr < npoints ){
         i = ypoint*im_size + xpoint;
         if ( diff_im) {
            if ( avr_grp ) {
               diff_prcnt = (float) (200 * (av_ar[i] - ref_ar[i]))
                                 / (float) (av_ar[i] + ref_ar[i]);
            }
            else {
               diff_prcnt = (float) (200 * (val[xc][yc][Im_Nr] - ref_ar[i]))
                                 / (float) (val[xc][yc][Im_Nr] + ref_ar[i]);
            }
            sprintf(strp, "Deviation from ref base: %.2f%%", diff_prcnt);
            txt_color("red");
            subW_TXT(loc, 5, strp);
            txt_color("black");

         } else if( avr_grp ){  /** RWCox: 4 March 1996 **/

            sprintf(strp, "Averaged pixel: %d",
                    av_ar[ypoint*im_size + xpoint] ) ;

            if( (matx > 1) || (maty > 1) ){  /** RWCox: 2 Aug 1996 **/
               int ix,iy , xtemp,ytemp , index ;
               float sum = 0.0 ;
               for( ix=0 ; ix < matx; ix++ ){
                  xtemp = xpoint + ix - xc;
                  if (xtemp < 0)        xtemp += im_size;
                  if (xtemp >= im_size) xtemp -= im_size;
                  for( iy=0 ; iy < maty; iy++ ){
                     ytemp = ypoint - iy + yc;
                     if (ytemp < 0)        ytemp += im_size;
                     if (ytemp >= im_size) ytemp -= im_size;
                     index = ytemp * im_size + xtemp;
                     sum  += av_ar[index] ;
                  }
               }
               sum /= (matx*maty) ;
               ix   = strlen(strp) ;
               sprintf(strp+ix , " [%.1f]" , sum ) ;
            }

            txt_color("red");
            subW_TXT(loc, 5, strp);
            txt_color("black");
         } /* end of pure average */
         else {
            int i, ix, iy;
            f1 = f0 = 0.;
            if ( Time_avr ) {
               for (i=0; i < npoints; i++) f0 += val[xc][yc][i];
               f2 = (float) max(npoints, 1);
               f0 = f0 / f2;
               for (i=0; i < npoints; i++) {
                  f2 = (float) val[xc][yc][i] - f0;
                  f1 += f2 * f2;
               }
               f2 = (float) max(npoints, 1);
               f1 = f1 / f2;
               f1 = sqrt(f1);
               sprintf(strp, "Time avr: %.1f, Std dev: %.1f", f0, f1);
               txt_color("blue");
               subW_TXT(loc, 5, strp);
               txt_color("black");
            }
            else {
               for (ix=0;ix<matx;ix++) {
                  for (iy=0;iy<maty;iy++) {
                     f0 += val[ix][iy][Im_Nr];
                  }
               }
               f0 = f0 / (float) (matx*maty);
               for (ix=0;ix<matx;ix++) {
                  for (iy=0;iy<maty;iy++) {
                     f2 = (float) val[ix][iy][Im_Nr] - f0;
                     f1 += f2 * f2;
                  }
               }
               f1 = f1 / (float) (matx*maty);
               f1 = sqrt(f1);
               sprintf(strp, "Box avr: %.1f, Std dev: %.1f", f0, f1);
               txt_color("blue");
               subW_TXT(loc, 5, strp);
               txt_color("black");
            }
         }

         x = xorigin[xc][yc] + Im_Nr*gx/(npoints-1);
         y = idY - yorigin[xc][yc] - plot[xc][yc][Im_Nr];
         v_point_x = x;
         Vpointer(x, 0);
         Cpointer(x, y );

      }  /* end if */
   }  /* end if */
   return ;
}

/* ----------------- */
   void
   Setup_topWindow()
/* ----------------- */
{
   unsigned long white; // long is OK here AJJ

   if (!(XAllocNamedColor(theDisp, CMap, "white", &any_col, &rgb_col)))
   FatalError ("XAllocNamedColor problem. AJ in Setup_topWindow()");
   white = any_col.pixel;

   top_W_x = idX;
   top_W_y = GT_DLY - 2;
   topWindow = XCreateSimpleWindow(theDisp, GWindow, 0, 0,
                     top_W_x, top_W_y, 1, white, white);
   XSelectInput(theDisp, topWindow, ExposureMask);
}

/* --------------- */ /* Draw topWindow with text info */
   void
   DrawTopWindow()
/* --------------- */
{
   int  strwide , xmin , xleft ;
   char *str = "Differential Image";
   char str_fim[128] ;

   line_color("white");
   XFillRectangle(theDisp, topWindow, theGC, 0, 0, idX, top_W_y);

   txt_color("black");
   XDrawString(theDisp, topWindow, txtGC, 12,
               top_W_y - 5, T_name, strlen(T_name));

   xmin = 3 + 12 + XTextWidth(mfinfo,T_name,strlen(T_name)) ;

   if ( diff_im ) {
      txt_color("red");
      strwide = XTextWidth(mfinfo,str,strlen(str));
      xleft   = GL_DLX + (gx_max-strwide)/2 ;
      xleft   = MAX(xmin,xleft) ;
      XDrawString(theDisp, topWindow, txtGC,
                  xleft , top_W_y - 5, str, strlen(str));
      txt_color("black");
   } else if( RWC_do_overfim && RWC_ideal != NULL ){

      sprintf( str_fim , "%s %s:ref=%s thresh=%5.3f #ort=%d #pol=%d",
               LSQ_fimcode[LSQ_code] , DFILT_fimcode[DFILT_code] ,
               RWC_ideal->fname , RWC_pcthresh , RWC_numort,RWC_polort ) ;

      if( FIM_pressed ) txt_color("blue") ;
      else              txt_color("red");

      if( FFT_pressed ) txt_color("blue") ;
      else              txt_color("red");

      strwide = XTextWidth(mfinfo,str_fim,strlen(str_fim));
      xleft   = GL_DLX + (gx_max-strwide)/2 ;
      xleft   = MAX(xmin,xleft) ;
      XDrawString(theDisp, topWindow, txtGC,
                  xleft , top_W_y - 5, str_fim, strlen(str_fim) );
      txt_color("black");
   }
}

/* ------------ */
   void
   Setup_keys()
/* ------------ */
{
   int i;

   key = &xtkeys[0];

   for (i=0; i < N_KEYS; i++) {
       key[i].x = PADDINGW;
       key[i].y = KEY_1_Y + i*(keyhigh + PADDINGH);
       key[i].width = keywide[i];
       key[i].height = keyhigh;

       key[i].fore=FKeyFore;
       key[i].back=FKeyBack;

       key[i].wid=XCreateSimpleWindow(theDisp, GWindow, key[i].x, key[i].y,
                                      key[i].width, key[i].height, 1,
                                      key[i].fore, key[i].back);
       New_Cursor(theDisp,  key[i].wid, XC_left_ptr, "yellow", "red");
       XSelectInput(theDisp, key[i].wid, ExposureMask | ButtonPressMask |
                  ButtonReleaseMask | EnterWindowMask | LeaveWindowMask);
   }
}

/* --------------- */
   void
   DrawKey(keynum)
   int keynum;
/* --------------- */
{
   char        *str;
   int         strwide;
   struct _key *kp;
   GC          AJkeygc; /* AJ new for proper color of the text */

   if( keynum < 0 || keynum >= N_KEYS ) return;

   kp = &key[keynum];
   str = kp->st;
   strwide = XTextWidth(kfontinfo,str,strlen(str));

   AJkeygc=Fkeygc;

   XClearWindow(theDisp,kp->wid) ;

   XDrawString(theDisp ,kp->wid, AJkeygc, (kp->width-strwide)/2,
               1 + kfontinfo->ascent, str, strlen(str));
}

/* ----------------- */
   void
   InvertKey(keynum)
   int keynum;
/* ----------------- */
{
   struct _key *kp;
   GC           AJkeyigc;

   if( keynum < 0 || keynum >= N_KEYS ) return;

   AJkeyigc = Fkeyigc;
   kp = &key[keynum];

   XFillRectangle(theDisp, kp->wid, AJkeyigc, 0, 0, kp->width, kp->height);
}

/* ---------------- */
   void
   LetGoKey(keynum)
   int keynum;
/* ---------------- */
{
   if( keynum < 0 || keynum >= N_KEYS ) return;
   InvertKey(keynum);
   (*(key[keynum].fun))(keynum);
}

/* ------------- */
   int
   Ims_rot(ikey)
   int ikey;
/* ------------- */
{
   register int i, j, k, l, m, n, s;

   old_im = -1;            /* to avoid faster reload of not rotated image */

   if( rot_direct == 0 ){            /* change key label */
      kROT_doall = ! kROT_doall ;
      xtkeys[kROT].st = (kROT_doall) ? (key_kROT_all) : (key_kROT_one) ;
      DrawKey(kROT) ;
      return 0;
   }

   if( rot_direct == 1 || rot_direct == -1 ){
      RWC_do_overfim = 0 ;
      if( RWC_imover != NULL ) { free(RWC_imover) ; RWC_imover = NULL ; }
      RWC_framehide = 0 ;
   } else {
      fprintf(stderr,"\n*** illegal rot_direct in Ims_rot() ***\n") ;
      XBell(theDisp,100) ;
      return 0;
   }

   if ( rot_direct == 1 ) {                         /* clockwise rotation */
      nowim = SAR(Im_Nr) ; k = DIM(Im_Nr) ; l = k-1 ; s = SIZ(Im_Nr) ;
      for (i=0; i < k; i++) {          /* rotate and redisplay actual one */
         m = i * k;
         for (j=0; j < k; j++)
            a_rot[m+j] = nowim[(l-j)*k+i];
      }
      for (i=0; i < s; i++) nowim[i] = a_rot[i];

      if( ! kROT_doall ){
         Put_image(Im_Nr ) ;
         return 0;
      }

      if ( diff_im && Im_Nr < npoints ) { /* rotate reference array too */
         for (i=0; i < k; i++) {
            m = i * k;
            for (j=0; j < k; j++)
               a_rot[m+j] = ref_ar[(l-j)*k+i];
         }
         for (i=0; i < s; i++) ref_ar[i] = a_rot[i];
      }
      if ( !avr_grp ) Put_image(Im_Nr);         /* put single image here */
                                                     /* rotate other data */
      for (n=0; n < N_im; n++) {
         nowim = SAR(n) ; k = DIM(n) ; l = k-1 ; s = SIZ(n) ;
         if ( n != Im_Nr ) {
            for (i=0; i < k; i++) {
               m = i * k;
               for (j=0; j < k; j++)
                  a_rot[m+j] = nowim[(l-j)*k+i];
            }
            for (i=0; i < s; i++) nowim[i] = a_rot[i];
         }
      }
      rot_nr += rot_direct;
      if ( avr_grp ) Put_image(Im_Nr);  /* put average image after all rot */
   }
   else if ( rot_direct == -1 ) {            /* counterclockwise rotation */
      nowim = SAR(Im_Nr) ; k = DIM(Im_Nr) ; l = k-1 ; s = SIZ(Im_Nr) ;
      for (i=0; i < k; i++) {          /* rotate and redisplay actual one */
         m = i * k;
         for (j=0; j < k; j++)
            a_rot[m+j] = nowim[(j+1)*k-i-1];
      }
      for (i=0; i < s; i++) nowim[i] = a_rot[i];

      if( ! kROT_doall ){
         Put_image(Im_Nr ) ;
         return 0;
      }

      if ( diff_im && Im_Nr < npoints ) {  /* rotate reference array too */
         for (i=0; i < k; i++) {
            m = i * k;
            for (j=0; j < k; j++)
               a_rot[m+j] = ref_ar[(j+1)*k-i-1];
         }
         for (i=0; i < s; i++) ref_ar[i] = a_rot[i];
      }
      if ( !avr_grp ) Put_image(Im_Nr);         /* put single image here */
                                                     /* rotate other data */
      for (n=0; n < N_im; n++) {
         nowim = SAR(n) ; k = DIM(n) ; l = k-1 ; s = SIZ(n) ;
         if ( n != Im_Nr ) {
            for (i=0; i < k; i++) {
               m = i * k;
               for (j=0; j < k; j++)
                  a_rot[m+j] = nowim[(j+1)*k-i-1];
            }
            for (i=0; i < s; i++) nowim[i] = a_rot[i];
         }
      }
      rot_nr += rot_direct;
      if ( avr_grp ) Put_image(Im_Nr);  /* put average image after all rot */
   }

        if( rot_nr ==  3 ) rot_nr = -1 ;
   else if( rot_nr == -3 ) rot_nr =  1 ;

   redraw_graph();
   DrawSubWindow();
   return 0;
}

/* ----------------- */
   int
   Smooth_line(ikey)
   int ikey;
/* ----------------- */
{
   int  i, x, y, hx;
   char *cpt, str[100];
   float fval, fmax, f0, f1, f2;

   if ( txtW_ON ) {
      XBell(theDisp, 100); return(2);
   }

   i = (MAX_SMOOTH - 1) / 10; /* +- 5 sigma range */
   fmax = i;
   sprintf(str, "Enter sigma value [.1-%d]:", i);
   txtW_ON = 1;

   x = 50 + GL_DLX;
   y = 50 + GT_DLY;

   strp[0] = ASC_NUL ;
   take_file_name(theDisp, GWindow , CMap, txtGC, mfinfo, x, y, strp, 41,
                  str, 0);

   fval = strtod( strp , &cpt ) ;
   if( *cpt != ASC_NUL || fval < .1 || fval > fmax ) {
      fprintf(stderr,
         "\n*** Sigma valule out of range [.1-%d] %s!\n" , (int) fmax, strp ) ;
      AJ_sigma = -1.;
      XBell(theDisp, 100);
   }
   else {
      AJ_sigma = fval ;
   }
   AJ_norm = 1.;
   if ( AJ_sigma > 0. ) {
      f2 = 0;
      AJ_nr = (int) (10.*AJ_sigma +.5);
      if ( AJ_nr%2 ) AJ_nr++;
      if ( AJ_nr < 4) AJ_nr = 4;
      hx = AJ_nr / 2;
      f0 = 1. / (2.* AJ_sigma  * AJ_sigma);
      for (i=0; i <= AJ_nr; i++) { /* make symmetrical gauss function */
         f1 = (float) ((i-hx)*(i-hx));
         AJ_gauss[i] = exp(-f1*f0);
         f2 += AJ_gauss[i];
      }
      if ( f2 != 0. ) AJ_norm = 1./ f2;
   }

   txtW_ON = 0;
   redraw_graph();
   DrawSubWindow();

   return(0);
}

/* ----------------- */
   int
   get_fft_mag(fff)
   float *fff;
/* ----------------- */
{
   int  i, k, x, y;
   char *cpt, str[100];
   float fval, fmax, fmin, f0, f1, f2, f3;
   int   max1;

   fmax = 100.;
   fmin = .01;
   if ( txtW_ON ) {
      XBell(theDisp, 100); return(2);
   }

   f0 = *fff / FFT_MAG;
   sprintf(str, "Enter new FT scale value [%g-%g]: %g", fmin, fmax, f0);
   txtW_ON = 1;

   x = 50 + GL_DLX;
   y = 50 + GT_DLY;

   strp[0] = ASC_NUL ;
   take_file_name(theDisp, GWindow , CMap, txtGC, mfinfo, x, y, strp, 46,
                  str, 0);

   fval = strtod( strp , &cpt ) ;
   if( *cpt != ASC_NUL || fval < fmin || fval > fmax ) {
      fprintf(stderr,
         "\n*** FT scale value out of range [%g-%g]: %s!\n", fmin, fmax, strp);
      XBell(theDisp, 100);
      txtW_ON = 0;
      redraw_graph();
      DrawSubWindow();
      return (0);
   }
   else {
      *fff = fval * FFT_MAG;
   }
   f0 = *fff;
   for (i=0; i < FT_disp; i++) {
      for (j=0; j < ar_size; j++) {
         f1 = c_arr[i+1+j*FT_dim].r; /* no zero component here */
         f2 = c_arr[i+1+j*FT_dim].i;
         f3 = f0 * sqrt(f1*f1 + f2*f2);
         if ( f3 > 32767. ) f3 = 32767.;
         SAR(i)[j] = f3;
      }
   }
   min1 =  32767;
   max1 = -32768;
   for (k=0; k < npoints; k++) { /* find global min and max */
      nowim = SAR(k) ;
      for (i=0; i < ar_size; i++) {
         if ( nowim[i] < min1 ) min1 = nowim[i] ;
         if ( nowim[i] > max1 ) max1 = nowim[i] ;
      }
   }
   del1 = max1 - min1;
   if (del1 < 1.) del1 = 1.;
   coef1 = ( (float) NC - 1.) / del1;

   old_im = -1;
   Put_image(Im_Nr);                      /* reload actual image */
   txtW_ON = 0;
   redraw_graph();
   DrawSubWindow();

   return(0);
}
/* ------------ */
   int
   FFT_action()
/* ------------ */
{
   MRI_IMAGE **MM;
   int i;
   float *VV;

   if ( Im_Nr >= npoints) {
      Im_Nr = 0;
      XClearWindow(theDisp, GWindow);
      Put_image(Im_Nr);
      DrawSubWindow();
      DrawTopWindow();
      discard(KeyPressMask, event);
   }

   if( FFT_pressed ) {                /* second press - back to normal graph */
      FT1_pressed = 0;
      FFT_pressed = 0 ;
      FT_graph_on = 0;
      xtkeys[kFFT].st = key_kFFT_FFT;
      DrawKey(kFFT) ;                  /* restore key label */
      FT2_stat = 0;
      z_imL = z_im1 = 0;
      if ( FT_done ) {
         int   mm, min_im = 0;
         float ff;

         if ( undo_buf != NULL ) {
            free(undo_buf);
            act_undo = -1;
         }
         if ( undo_ref != NULL ) {
            free(undo_ref);
            ref_undo = -1;
         }
         if ( RWC_ideal != NULL  && RWC_ideal->len >= npoints ) {
            VV = RWC_ideal->ts; RWC_ideal->ts = T_ref; T_ref = VV;
         }
         MM = allim;     allim = t_allim;   t_allim = MM;
         mm = N_im;       N_im = t_N_im;     t_N_im = mm;
         mm = npoints; npoints = t_points; t_points = mm;
         mm = min1;       min1 = t_min1;     t_min1 = mm;
         ff = coef1;     coef1 = t_coef1;   t_coef1 = ff;
         if ( grid_timed ) if ( grid_coef < .5 ) grid_coef = GRID_COEF;
         Im_Nr = min_im;
         FT_grid = 0;
         redraw_graph();   /* draw frame and text in it */
         DrawSubWindow();
         DrawTopWindow();
         old_im = -1;
         Put_image(0);                      /* put the first image */
      }
      if ( cancell_FT ) {
         cancell_FT = 0;
         FT_done = 0;
         free(c_arr);
         c_arr = NULL;
         free(r_arr);
         r_arr = NULL;
      }
      FT3_stat = FT_done;
      xtkeys[kFT1].st = key_kFT1[FT1_pressed];
      xtkeys[kFT2].st = key_kFT2[FT2_stat];
      xtkeys[kFT3].st = key_kFT3[FT3_stat];
      for(i=FFT_first_key; i <= FFT_last_key; i++)
         XUnmapWindow(theDisp,  key[i].wid);  /* second press */
   }
   else {                            /* first press */
      FFT_pressed  = 1;
      xtkeys[kFFT].st = key_kFFT_noFT;
      DrawKey(kFFT);
      FT3_stat = FT_done;
      if ( FT_done ) {
         int   mm, min_im = 0;
         float ff;

         for(i=FFT_first_key; i <= FFT_last_key; i++)
            XMapWindow  (theDisp,  key[i].wid);  /* first press & FFT done */

         FT_graph_on = 1;
         if ( RWC_ideal != NULL  && RWC_ideal->len >= npoints ) {
            VV = RWC_ideal->ts; RWC_ideal->ts = T_ref; T_ref = VV;
         }
         MM = allim;     allim = t_allim;   t_allim = MM;
         mm = N_im;       N_im = t_N_im;     t_N_im = mm;
         mm = npoints; npoints = t_points; t_points = mm;
         mm = min1;       min1 = t_min1;     t_min1 = mm;
         ff = coef1;     coef1 = t_coef1;   t_coef1 = ff;
         FT_grid = GRID_NUM;
         if ( grid_timed ) if ( grid_coef > 2. ) grid_coef = 1. / GRID_COEF;
         Im_Nr = min_im;
         redraw_graph();   /* draw frame and text in it */
         DrawSubWindow();
         DrawTopWindow();
         old_im = -1;
         Put_image(0);                      /* put the first image */
      }
      else {
         for(i=FFT_first_key + 1; i <= FFT_last_key; i++)
            XMapWindow  (theDisp,  key[i].wid);  /* first press & no FFT */
      }
   }
   return 0;
}

/* ------------------- */
   int
   FFT_selection(ikey)
   int ikey;
/* ------------------- */
{
   int  i, j, k, ii, mm, nn;
   int  fun_modified;

   fun_modified = 0;

   switch( ikey ) {
      /* toggle edit Function - end of Edit button */
      case kFT1:
         FT1_pressed = 1 - FT1_pressed;
         InvertKey(kFT1);
         xtkeys[kFT1].st = key_kFT1[FT1_pressed];
         if ( FT1_pressed ) {                    /* edit mode */
            FT2_stat = 1;
            z_imL = z_im1 = 0;
            FT3_stat = 2;
            xtkeys[kFT2].st = key_kFT2[FT2_stat];
            xtkeys[kFT3].st = key_kFT3[FT3_stat];
         }
         else {
            FT2_stat = 0;
            FT3_stat = FT_done;
            z_imL = z_im1 = 0;
            xtkeys[kFT2].st = key_kFT2[FT2_stat];
            xtkeys[kFT3].st = key_kFT3[FT3_stat];
         }
         DrawKey(kFT1);
         DrawKey(kFT2);
         DrawKey(kFT3);
      break ;  /* end of ikey=kFT1 */

      case kFT2:
         /* make FFT - when not in edit and FT2 = "FT" */
         if ( (FT1_pressed == 0) && (FT2_stat == 0) ) {
            register float f0, f1, f2, f3;
            int min_im = 0, max1;
            int Fnx = allim[0]->nx;
            int Fny = allim[0]->ny;

            if ( c_arr == NULL ) {
               FT_dim = 2;
               while ( FT_dim < npoints ) FT_dim *= 2;
               FT_disp = FT_dim / 2;
               im_f = min(3, dec_indx(FT_disp)-1);  /* 2d index in formt[][] */
               FT_size = FT_dim * ar_size;
               c_arr = (complex *) malloc(sizeof(complex) * FT_size);
               if( c_arr == NULL ){
                  fprintf(stderr,"\n*** cannot malloc c_arr\a\n") ;
                  XBell(theDisp, 100); return(1);
               }
               /* load complex array for FFT */
               for (i=0; i < ar_size; i++) {
                  ii = i * FT_dim;
                  for (j=0; j < npoints; j++) {
                     c_arr[ii+j].r = SAR(j)[i];
                     c_arr[ii+j].i = 0.;
                  }
                  for (j=npoints; j < FT_dim; j++) {
                     c_arr[ii+j].r = SAR(npoints-1)[i];
                     c_arr[ii+j].i = 0.;
                  }
               }
            }
            if ( r_arr == NULL ) {
               r_arr = (complex *) malloc(sizeof(complex) * FT_dim);
               if( r_arr == NULL ){
                  fprintf(stderr,"\n*** cannot malloc r_arr\a\n") ;
                  XBell(theDisp, 100); return(1);
               }
               /* load complex reference array for FFT */
               if ( RWC_ideal != NULL  && RWC_ideal->len >= npoints ) {
                  for (j=0; j < npoints; j++) {
                     r_arr[j].r = RWC_ideal->ts[j];
                     r_arr[j].i = 0.;
                  }
                  for (j=npoints; j < FT_dim; j++) {
                     r_arr[j].r = RWC_ideal->ts[npoints-1];
                     r_arr[j].i = 0.;
                  }
               }
               else {
                  for (j=0; j < FT_dim; j++) {
                     r_arr[j].r = 0.;
                     r_arr[j].i = 0.;
                  }
               }
            }
            csfft( -1, FT_dim, r_arr);    /* FT of reference line */
            for (i=0; i < ar_size; i++) {
               if ( i%100 == 0 ) {
                  printf(".");
                  fflush(stdout);
               }
               ii = i * FT_dim;
               csfft( -1, FT_dim, &c_arr[ii]);
            }
            if ( RWC_ideal != NULL  && RWC_ideal->len >= npoints ) {
               T_ref = RWC_ideal->ts;
               RWC_ideal->ts = NULL;
               RWC_ideal->ts = (float *) malloc( sizeof(float) * FT_disp);
               if( RWC_ideal->ts == NULL ) {
                  fprintf(stderr,
                     "\n*** cannot malloc RWC_ideal->ts for FT\a\n");
                  XBell(theDisp, 100);
                  DrawKey(kFT3);
                  return(2);
               }
            }
            t_allim = allim; t_N_im = N_im; t_points = npoints;
            N_im = npoints = FT_disp;
            allim = NULL;
            Im_Nr = min_im;
            allim = (MRI_IMAGE **) malloc( sizeof(MRI_IMAGE *) * FT_disp);
            if( allim == NULL ) {
               fprintf(stderr,"\n*** cannot malloc allim\a\n") ;
               XBell(theDisp, 100);
               DrawKey(kFT3);
               return(2);
            }
            for (i=0; i < FT_disp; i++) {
               allim[i] = mri_new( Fnx, Fny, MRI_short );
               if( allim == NULL ) {
                  fprintf(stderr,"\n*** cannot malloc allim[%d] in FT\a\n", i) ;
                  XBell(theDisp, 100); return(2);
               }
               sprintf(FT_name, formt[0][im_f], "FT", i+1);
               mri_add_name( FT_name, allim[i]);
            }
            XMapWindow  (theDisp,  key[FFT_first_key].wid); /* can edit now */
            f0 = fft_mag;
            for (i=0; i < FT_disp; i++) {
               for (j=0; j < ar_size; j++) {
                  f1 = c_arr[i+1+j*FT_dim].r; /* no zero component here */
                  f2 = c_arr[i+1+j*FT_dim].i;
                  f3 = f0 * sqrt(f1*f1 + f2*f2);
                  if ( f3 > 32767. ) f3 = 32767.;
                  SAR(i)[j] = f3;
               }
            }
            if ( RWC_ideal != NULL  && RWC_ideal->len >= npoints ) {
               for (i=0; i < FT_disp; i++) {
                  f1 = r_arr[i+1].r; /* no zero component for reference too */
                  f2 = r_arr[i+1].i;
                  f3 = f0 * sqrt(f1*f1 + f2*f2);
                  if ( f3 > 32767. ) f3 = 32767.;
                  RWC_ideal->ts[i] = f3;
               }
            }
            t_min1 = min1; t_coef1 = coef1;
            min1 =  32767;
            max1 = -32768;
            for (k=0; k < npoints; k++) { /* find global min and max */
               nowim = SAR(k) ;
               for (i=0; i < ar_size; i++) {
                  if ( nowim[i] < min1 ) min1 = nowim[i] ;
                  if ( nowim[i] > max1 ) max1 = nowim[i] ;
               }
            }
            del1 = max1 - min1;
            if (del1 < 1.) del1 = 1.;
            coef1 = ( (float) NC - 1.) / del1;

            FT_grid = GRID_NUM;
            if ( grid_timed ) {
               f0 = 2. * (float) FT_disp / grid_far[0];
               grid_far[GRID_NUM]   = f0;
               grid_far[GRID_NUM+1] = 2.*f0;
               grid_far[GRID_NUM+2] = 5.*f0;
               if ( grid_coef > 2. ) grid_coef = 1. / GRID_COEF;
            }

            FT_graph_on = 1;
            redraw_graph();   /* draw frame and text in it */
            DrawSubWindow();
            DrawTopWindow();
            old_im = -1;
            Put_image(0);                      /* put the first image */

            FT3_stat = FT_done = 1;;
            xtkeys[kFT3].st = key_kFT3[FT3_stat];
            DrawKey(kFT3);
         }
         /* edit mode on and FT2 = "0..0" */
         else if ( (FT1_pressed == 1) && (FT2_stat == 1) ) {
            FT2_stat = 2;
            xtkeys[kFT2].st = key_kFT2[FT2_stat];
            DrawKey(kFT2);
         }
         else if ( (FT1_pressed == 1) && (FT2_stat == 2) ) {
            z_im1 = Im_Nr;
            FT2_stat = 3;
            xtkeys[kFT2].st = key_kFT2[FT2_stat];
            DrawKey(kFT2);
         }
         else if ( (FT1_pressed == 1) && (FT2_stat == 3) ) {
            int i, j, k, mm, nn;

            z_imL = Im_Nr + 1;
            /* swap order if last < first */
            if ( z_imL - z_im1 < 0) { 
               i = z_im1;
               z_im1 = z_imL;
               z_imL = i;
            }
            mm = act_undo + 1;
            act_undo += (z_imL - z_im1) * ar_size;
            undo_buf = realloc(undo_buf, (act_undo+1)*sizeof(struct _undo_buf));
            if ( undo_buf != NULL ) {
               for ( k=z_im1, nn=mm; k < z_imL; k++, nn+=ar_size) { 
                  for ( i=0, j=nn; i < ar_size; j++, i++) {
                     undo_buf[j].im  = k;
                     undo_buf[j].pix = i;
                     undo_buf[j].r   = c_arr[i*FT_dim+k+1].r;
                     undo_buf[j].i   = c_arr[i*FT_dim+k+1].i;
                     undo_buf[j].r2  = c_arr[(i+1)*FT_dim-k-1].r;
                     undo_buf[j].i2  = c_arr[(i+1)*FT_dim-k-1].i;
                     SAR(k)[i]   = 0;
                     c_arr[i*FT_dim+k+1].r     = 0.;
                     c_arr[i*FT_dim+k+1].i     = 0.;
                     c_arr[(i+1)*FT_dim-k-1].r = 0.;
                     c_arr[(i+1)*FT_dim-k-1].i = 0.;
                  }
               }
            }
            if ( RWC_ideal != NULL  && RWC_ideal->len >= npoints ) {
               mm = ref_undo + 1;
               ref_undo += (z_imL - z_im1);
               undo_ref =
                      realloc(undo_ref, (ref_undo+1)*sizeof(struct _undo_buf));
               if ( undo_ref != NULL ) {
                  for ( k=z_im1, j=mm; k < z_imL; k++, j++) { 
                     undo_ref[j].im  = k;
                     undo_ref[j].r   = r_arr[k+1].r;
                     undo_ref[j].i   = r_arr[k+1].i;
                     undo_ref[j].r2  = r_arr[FT_dim-k-1].r;
                     undo_ref[j].i2  = r_arr[FT_dim-k-1].i;
                     RWC_ideal->ts[k] = 0;
                     r_arr[k+1].r     = 0.;
                     r_arr[k+1].i     = 0.;
                     r_arr[FT_dim-k-1].r = 0.;
                     r_arr[FT_dim-k-1].i = 0.;
                  }
               }
            }
            redraw_graph() ;
            DrawSubWindow();
            old_im = -1;
            Put_image(Im_Nr);
            FT2_stat = 1;
            z_imL = z_im1 = 0;
            xtkeys[kFT2].st = key_kFT2[FT2_stat];
            DrawKey(kFT2);
         }
      break ;  /* end of ikey=kFT2 */

      /* make inv FFT */
      case kFT3:
         /* make inverse FFT */
         if ( (FT1_pressed == 0) && (FT_done == 1) ) {
            MRI_IMAGE **MM;
            int   min_im = 0, max1;
            register float f0, f1, f2, f3, f4, fpi2;

            im_f = min(3, dec_indx(npoints)-1);  /* 2d index in formt[][] */

            f0 = 1. / (float) FT_dim;
            for (i=0; i < ar_size; i++) {
               if ( i%100 == 0 ) {
                  printf(".");
                  fflush(stdout);
               }
               ii = i * FT_dim;
               csfft(1, FT_dim, &c_arr[ii]);
            }
            csfft(1, FT_dim, r_arr);

            MM = allim;     allim = t_allim;   t_allim = MM;
            mm = N_im;       N_im = t_N_im;     t_N_im = mm;
            mm = npoints; npoints = t_points; t_points = mm;
            if ( grid_timed ) if ( grid_coef < .5 ) grid_coef = GRID_COEF;

            fpi2 = .5 * PI;
            for (i=0; i < ar_size; i++) {
               ii = i * FT_dim;
               for (j=0; j < npoints; j++) {
                  f1 = c_arr[ii+j].r *= f0;;
                  f2 = c_arr[ii+j].i *= f0;;
                  f3 = sqrt(f1*f1 + f2*f2);
                  if ( f3 > 32767. ) f3 = 32767.;
                  if ( phase ) {
                     f4 = atan2(f2, f1);
                     if ( (f4 < fpi2) && (f4 > -fpi2) ) SAR(j)[i] =  f3;
                     else                               SAR(j)[i] = -f3;
                  }
                  else {
                     SAR(j)[i] = f3;
                  }
               }
            }
            if ( RWC_ideal != NULL  && RWC_ideal->len >= npoints ) {
               for (j=0; j < npoints; j++) {
                  f1 = r_arr[j].r *= f0;;
                  f2 = r_arr[j].i *= f0;;
                  f3 = sqrt(f1*f1 + f2*f2);
                  if ( f3 > 32767. ) f3 = 32767.;
                  if ( phase ) {
                     f4 = atan2(f2, f1);
                     if ( (f4 < fpi2) && (f4 > -fpi2) ) RWC_ideal->ts[j] =  f3;
                     else                               RWC_ideal->ts[j] = -f3;
                  }
                  else {
                     RWC_ideal->ts[j] = f3;
                  }
               }
            }
            t_min1 = min1; t_coef1 = coef1;
            min1 =  32767;
            max1 = -32768;
            for (k=0; k < npoints; k++) { /* find global min and max */
               nowim = SAR(k) ;
               for (i=0; i < ar_size; i++) {
                  if ( nowim[i] < min1 ) min1 = nowim[i] ;
                  if ( nowim[i] > max1 ) max1 = nowim[i] ;
               }
            }
            del1 = max1 - min1;
            if (del1 < 1.) del1 = 1.;
            coef1 = ( (float) NC - 1.) / del1;

            Im_Nr = min_im;
            FT_grid = 0;
            redraw_graph();   /* draw frame and text in it */
            DrawSubWindow();
            DrawTopWindow();
            old_im = -1;
            Put_image(0);                      /* put the first image */

            for(i=FFT_first_key; i <= FFT_last_key; i++)
               XUnmapWindow(theDisp,  key[i].wid);  /* as second press */

            xtkeys[kFFT].st = key_kFFT_FFT;
            DrawKey(kFFT) ;                  /* restore key label */
            xtkeys[kFT1].st = key_kFT1[FT1_pressed];
            xtkeys[kFT2].st = key_kFT2[FT2_stat];
            xtkeys[kFT3].st = key_kFT3[FT3_stat];;
            FT1_pressed = 0;
            FFT_pressed = 0 ;
            FT_graph_on = 0;
            FT_done = 0;
            free(c_arr);
            c_arr = NULL;
         }
         /* edit mode on and FT3 pressed = "zero" */
         else if ( FT1_pressed == 1 ) {
            nn = act_undo + 1;
            act_undo += ar_size;
            undo_buf = realloc(undo_buf, (act_undo+1)*sizeof(struct _undo_buf));
            if ( RWC_ideal != NULL  && RWC_ideal->len >= npoints ) {
               ref_undo += 1;
               undo_ref =
                      realloc(undo_ref, (ref_undo+1)*sizeof(struct _undo_buf));
               if ( undo_ref != NULL ) {
                  j = ref_undo;
                  undo_ref[j].im  = Im_Nr;
                  undo_ref[j].r   = r_arr[Im_Nr+1].r;
                  undo_ref[j].i   = r_arr[Im_Nr+1].i;
                  undo_ref[j].r2  = r_arr[FT_dim-Im_Nr-1].r;
                  undo_ref[j].i2  = r_arr[FT_dim-Im_Nr-1].i;
                  RWC_ideal->ts[Im_Nr] = 0;
                  r_arr[Im_Nr+1].r     = 0.;
                  r_arr[Im_Nr+1].i     = 0.;
                  r_arr[FT_dim-Im_Nr-1].r = 0.;
                  r_arr[FT_dim-Im_Nr-1].i = 0.;
               }
            }
            if ( undo_buf != NULL ) {
               for ( i=0, j=nn; i < ar_size; j++, i++) {
                  undo_buf[j].im  = Im_Nr;
                  undo_buf[j].pix = i;
                  undo_buf[j].r   = c_arr[i*FT_dim+Im_Nr+1].r;
                  undo_buf[j].i   = c_arr[i*FT_dim+Im_Nr+1].i;
                  undo_buf[j].r2  = c_arr[(i+1)*FT_dim-Im_Nr-1].r;
                  undo_buf[j].i2  = c_arr[(i+1)*FT_dim-Im_Nr-1].i;
                  SAR(Im_Nr)[i]   = 0;
                  c_arr[i*FT_dim+Im_Nr+1].r     = 0.;
                  c_arr[i*FT_dim+Im_Nr+1].i     = 0.;
                  c_arr[(i+1)*FT_dim-Im_Nr-1].r = 0.;
                  c_arr[(i+1)*FT_dim-Im_Nr-1].i = 0.;
               }
               redraw_graph() ;
               DrawSubWindow();
            }
            old_im = -1;
            Put_image(Im_Nr);
         }
      break ;  /* end of ikey=kFT3 */

   }  /* end of ikey switch */

   if( fun_modified ){
      redraw_graph() ;
      fun_modified = 0;
   }

   return 0;
}

/* ------------- */
   int
   Im_diff(ikey)
   int ikey;
/* ------------- */
{
   XUnmapWindow(theDisp,  key[kDIF].wid);
   XMapWindow  (theDisp,  key[kIR1].wid);
   XMapWindow  (theDisp,  key[kIR2].wid);
   return 0;
}

/* ---------------- */
   int SCA_action(ikey)
   int ikey;
/* ---------------- */
{
   int i;

   if ( SCA_pressed == 0 ) SCA_pressed = 1;
   else SCA_pressed = 0;

   xtkeys[kSCA].st = key_kSCA[SCA_pressed];
   DrawKey(kSCA);
//   XMapWindow  (theDisp,  key[kSCA].wid);

   if ( SCA_pressed ) {                /* first press */
      xtkeys[kSCA].st = key_kSCA[SCA_pressed];
      DrawKey(kSCA);
      for(i=SCA_first_key; i <= SCA_last_key; i++)
         XMapWindow  (theDisp,  key[i].wid);
   }
   else {
      for(i=SCA_first_key; i <= SCA_last_key; i++)
         XUnmapWindow(theDisp,  key[i].wid);  /* second press */
   }
#if 0
         redraw_graph();   /* draw frame and text in it */
         DrawSubWindow();
         DrawTopWindow();
         old_im = -1;
         Put_image(0);                      /* put the first image */
      }
      else {
         for(i=FFT_first_key + 1; i <= FFT_last_key; i++)
            XMapWindow  (theDisp,  key[i].wid);  /* first press & no FFT */
      }




      FT1_pressed = 0;
      FFT_pressed = 0 ;
      FT_graph_on = 0;
      xtkeys[kFFT].st = key_kFFT_FFT;
      DrawKey(kFFT) ;                  /* restore key label */
      FT2_stat = 0;
      FT3_stat = FT_done;
      xtkeys[kFT1].st = key_kFT1[FT1_pressed];
      xtkeys[kFT2].st = key_kFT2[FT2_stat];
      xtkeys[kFT3].st = key_kFT3[FT3_stat];
      for(i=FFT_first_key; i <= FFT_last_key; i++)
         XUnmapWindow(theDisp,  key[i].wid);  /* second press */
   }
   else {                            /* first press */
      FFT_pressed  = 1;
      xtkeys[kFFT].st = key_kFFT_noFT;
      DrawKey(kFFT);


   else {
         redraw_graph();   /* draw frame and text in it */
         DrawSubWindow();
         DrawTopWindow();
         old_im = -1;
         Put_image(0);       
   }
#endif

   return 0;
}


/* ------------------- */
   int
   SCA_selection(ikey)
   int ikey ;
/* ------------------- */
{
   int i, k, ix, iy;
   float f0;

   f0 = 0.;
   switch( ikey ){
      case kSC1:
         for (ix=0;ix<matx;ix++) {
            for (iy=0;iy<maty;iy++) {
               f0 += val[ix][iy][Im_Nr];
            }
         }
         f0 = f0 / (float) (matx*maty);
         SCA_ref_val = f0;
         printf("AJ REF im %d,  Box avr: %.1f\n", Im_Nr + 1, f0);

      break;
      case kSC2:
         if ( SCA_ref_val < 0.000001 ) {
            printf("!!! No reference image chosen yet (ref val: %g) !!!\n", SCA_ref_val);
            return -1;
         }
         for (ix=0;ix<matx;ix++) {
            for (iy=0;iy<maty;iy++) {
               f0 += val[ix][iy][Im_Nr];
            }
         }
         f0 = f0 / (float) (matx*maty);
         if ( f0 < 0.000001 ) SCA_ratio = 1.;
         else                 SCA_ratio = SCA_ref_val / f0;
         printf("AJ IMAGE Box avr: %.1f, coef: %g\n", f0, SCA_ratio);

         nowim = SAR(Im_Nr) ; k = SIZ(Im_Nr) ;
         for (i=0; i < k; i++) nowim[i] = (int) ( ((float) nowim[i]) *SCA_ratio + .5 );

         XClearWindow(theDisp, GWindow);
         Put_image(Im_Nr);
         redraw_graph();
         DrawSubWindow();
         DrawTopWindow();
      break;
   }

   return 0;
}

/* ------------- */
   int
   Im_Aver(ikey)
   int ikey;
/* ------------- */
{
   avr_grp = fim_avr = 0;
   XMapWindow(theDisp,  key[kAV1].wid);
   XMapWindow(theDisp,  key[kAV2].wid);
   return 0;
}

/* ------------- */
   int
   Im_norm(ikey)
   int ikey;
/* ------------- */
{
   diff_im = fim_dif = 0;
   avr_grp = fim_avr = 0;
   Av_length = 1;
   XUnmapWindow(theDisp,  key[kNRM].wid);
   XMapWindow  (theDisp,  key[kDIF].wid);
   old_im = -1;            /* to avoid faster reload of old data */
   Put_image(Im_Nr);
   redraw_graph();
   DrawSubWindow();
   return 0;
}

/* ------------ */
   int
   Av_im1(ikey)
   int ikey;
/* ------------ */  /* set first image for average one */
{
   Av_1 = Im_Nr;
   av1_done = 1;
   return 0;
}

/* ------------ */
   int
   Av_im2(ikey)
   int ikey;
/* ------------ */  /* set second image for average one */
{
   register int  i, j;

   if ( av1_done ) {
      Av_2 = Im_Nr;
      if ( Av_2 < Av_1 ) {
         i = Av_2; Av_2 = Av_1; Av_1 = i;
      }
      Im_Nr = Av_1;
   }
   else     Av_1 = Av_2 = Im_Nr;

   for (i=0; i< ar_size; i++) av_ar[i] = 0;
   for (i=0; i < ar_size; i++) {
      for (j=Av_1; j <= Av_2; j++) av_ar[i] += SAR(j)[i] ;
   }
   Av_length =  Av_2 - Av_1 + 1;
   for (i=0; i < ar_size; i++) av_ar[i] = av_ar[i] / Av_length;

   XUnmapWindow(theDisp,  key[kAV1].wid);
   XUnmapWindow(theDisp,  key[kAV2].wid);
   XMapWindow  (theDisp,  key[kNRM].wid);
   avr_grp = fim_avr = 1;
   av1_done = 0;
   old_im = -1;            /* to avoid faster reload of old data */
   Put_image(Im_Nr);
   redraw_graph();
   DrawSubWindow();
   return 0;
}

/* ------------- */
   int
   Ref_im1(ikey)
   int ikey;
/* ------------- */  /* set first image for average reference one */
{
   Im_1 = Im_Nr;
   im1_done = 1;
   return 0;
}

/* ------------- */
   int
   Ref_im2(ikey)
   int ikey;
/* -------------  set second image for average and make refer im for diff */
{
   register int  i, j, m;

   if ( im1_done ) {
      Im_2 = Im_Nr;
      if ( Im_2 < Im_1 ) {
         i = Im_2; Im_2 = Im_1; Im_1 = i;
      }
   }
   else     Im_1 = Im_2 = Im_Nr;

   for (i=0; i< ar_size; i++) ref_ar[i] = 0;
   for (i=0; i < ar_size; i++) {
      for (j=Im_1; j <= Im_2; j++) ref_ar[i] += SAR(j)[i] ;
   }
   m = Im_2 - Im_1 + 1;
   for (i=0; i < ar_size; i++) ref_ar[i] = ref_ar[i] / m;

   XUnmapWindow(theDisp,  key[kIR1].wid);
   XUnmapWindow(theDisp,  key[kIR2].wid);
   XMapWindow  (theDisp,  key[kNRM].wid);

   diff_im = fim_dif = 1;
   im1_done = 0;
   old_im = -1;            /* to avoid faster reload of old data */
   Put_image(Im_Nr);
   redraw_graph();
   DrawSubWindow();
/*
   DrawTopWindow();
*/
   return 0;
}

/* ------------- */
   int
   Im_help(ikey)
   int ikey;
/* ------------- */
{
   The_Help(0);
   return 0;
}

/* ------------ */
   void
   draw_frame()
/* ------------ */
{
   register int i, yyy;
                               /* draw frame */
   line_color("black");
   for (i=0; i <= maty; i++) {
     yyy = mdy1+i*gy;
     if ( yyy > gy_max + GB_DLY ) yyy = gy_max + GB_DLY;
     plotx(mdx1       , yyy, 0);
     plotx(mdx1+matx*gx, yyy, 1);
   }
   for (i=0;i<=matx;i++) {	
     plotx(mdx1+i*gx,mdy1       , 0);
     plotx(mdx1+i*gx,mdy1+maty*gy, 1);
   }
}

/* --------------------- */	/* Reload pixmap pxWind to GWindow */
   void graphic_store()
/* --------------------- */
{
   XSetWindowBackgroundPixmap(theDisp, GWindow, pxWind);
   XClearWindow(theDisp, GWindow);
   XFlush(theDisp);
}

/* ------------------- */	/* It plots line to point (x,y) for mod = 1 */
   void plotx(x,y,mod)		/* or moves to this point for mod = 0.      */
   int x, y, mod;               /* All into the pxWind.                     */
/* ------------------- */
{
   int	iy = idY - y;

   if(mod == 0) { x00 = x; y00 = iy; }
   if(mod == 1) {
     XDrawLine(theDisp, pxWind, theGC, x00, y00, x, iy);
     x00 = x;	y00 = iy;
   }
}

/* --------------------- */	/* Plot text in pxWind at x,y position */
   void plx_txt(x,y,str)        /*  relative to lower left corner (!). */
   int  x, y;
   char *str;
/* --------------------- */
{
   int	iy = idY - y, n = strlen(str);;
   XDrawString(theDisp, pxWind, txtGC, x, iy, str, n);
}

/* -------------------------- */ /* Plot text in any window w at x, y */
   void plx_TXT(w, x, y, str)    /* relative to lower left corner (!)   */
   Window w;
   int  x, y;
   char *str;
/* -------------------------- */
{
   Window r;
   int x0, y0;
   u_int  width, height, bw, dp;

   if (!XGetGeometry(theDisp, w, &r, &x0, &y0, &width, &height, &bw, &dp)) {
      printf("\n Problem in plx_TXT() with XGetGeometry\n");
      exit(10);
   }
   else
      XDrawString(theDisp, w, txtGC, x, height - y, str, strlen(str));
}

/* -------------------------- */ /* Plot text in subWindow  at x, y */
   void subW_TXT(x, y, str)      /* relative to lower left corner (!) */
   int  x, y;
   char *str;
/* -------------------------- */
{
   XDrawString(theDisp, subWindow, txtGC, x, sub_W_y - y, str, strlen(str));
}

/* ----------------------- */	/* erase to background color */
   void
   erase_graph()        /*   */
/* ----------------------- */
{
   line_color("white");
   XFillRectangle(theDisp, pxWind, theGC, 0, 0, idX, idY);
}

/* ----------------------- */	/* redraw entire graph */
   void
   redraw_graph()
/* ----------------------- */
{
   erase_graph();
   draw_marker();
   draw_frame();
   plot_line();
                                      /* draw min & max values in GWindow */
   sprintf(strp, "%05d", pmax[xc][yc]);
   plx_txt(xspace, GB_DLY + gy_max - mytxt, strp);
   sprintf(strp, "%05d", pmin[xc][yc]);
   plx_txt(xspace, GB_DLY + 5, strp);

   graphic_store();
}

/* -------------------- */     /* Change color for plotting */
   void line_color(col)        /* col - named color         */
   char *col;
/* -------------------- */
{
   XColor  any_col, rgb_col;
   char old_color[64] = "RW Cox" ;

   if( strcmp(col,old_color) == 0 ) return ;

   if (!(XAllocNamedColor(theDisp, CMap, col, &any_col, &rgb_col)))
      FatalError ("XAllocNamedColor problem. AJ");
   XSetForeground(theDisp, theGC, any_col.pixel);

   strcpy( old_color , col ) ;
}

/* -------------------- */     /* Change color for plotting */
   void txt_color(col)         /* col - named color         */
   char *col;
/* -------------------- */
{
   XColor  any_col, rgb_col;

   if (!(XAllocNamedColor(theDisp, CMap, col, &any_col, &rgb_col)))
      FatalError ("XAllocNamedColor problem for text. AJ");
   XSetForeground(theDisp, txtGC, any_col.pixel);
}

/* ----------------------- */
   void
   FatalError (identifier)
   char *identifier;
/* ----------------------- */
{
   fprintf(stderr, "%s: %s\a\n",ProgramName, identifier);
   exit(-1);
}

/* ----------------------------------- */
   void
   CreateGraphWindow(argv, argc)
   int  argc;
   char *argv[];
/* ----------------------------------- */
{
   XClassHint		class;
   XSetWindowAttributes attr;
   unsigned int		attrmask;
   XSizeHints		hints;
   int 			x = 0, y = 0;


   class.res_name  = "Graph";
   class.res_class = "Graph";

   hints.width = idX;         hints.height = idY;
   hints.max_width = idX;     hints.max_height = idY;
   hints.flags = PMaxSize;

   hints.min_width = idX;     hints.min_height = idY;
   hints.flags |= PMinSize;

   attr.background_pixel = bcol;
   attr.border_pixel     = fcol;
   attrmask = CWBackPixel | CWBorderPixel;

   GWindow = XCreateWindow(theDisp, rootW, x, y, idX, idY, 2,
	CopyFromParent, CopyFromParent, CopyFromParent, attrmask, &attr);

   if (!GWindow)
      FatalError("Can't open window (are X11 windows running ?). AJ");

   XSetClassHint(theDisp, GWindow, &class);
   XSetStandardProperties(theDisp, GWindow, G_name, I_name, None,
			  argv, argc, &hints);
}

/* ---------------------------- */
   void
   Allow_smaller_gr(argc, argv)
   int  argc;
   char *argv[];
/* ---------------------------- */
{
   XSizeHints           hints;

   hints.min_height = 150;
   hints.min_width = 200;
   hints.flags = PMinSize;

   hints.max_height = DisplayHeight(theDisp, theScreen);
   hints.max_width  = DisplayWidth(theDisp, theScreen);

   hints.flags |= PMaxSize;
   XSetStandardProperties(theDisp, GWindow, G_name, I_name, None,
                          argv, argc, &hints);
}

/* -------------- */     /* discard events x (of ev) to stop faster */
   void
   discard(x, ev)
   int x;
   XEvent *ev;
/* -------------- */
{
   XSync(theDisp,False) ;
   while ( XCheckWindowEvent(theDisp, theWindow, x, ev) ) ;
   while ( XCheckWindowEvent(theDisp,   GWindow, x, ev) ) ;
}

/* ------------------------ */
   void
   discard_Key(keyW, x, ev)
   int    x;
   XEvent *ev;
   Window keyW;
/* ------------------------ */
{
   while ( XCheckWindowEvent(theDisp, keyW, x, ev) ) ;
}

/* -------------------- */
   void
   Track_Cursor(mx, my)
   int mx, my;
/* -------------------- */
{
   Window       rW, cW;
   u_int        key;
   int          x, y, rx, ry;

   while (XQueryPointer(theDisp, theWindow, &rW, &cW,
                                &rx, &ry, &x, &y, &key)) {
      if ( !(key & Button1Mask) ) break;    /* button released */

      if ( mx != x || my != y ) {   /* this marker was moved */
         xpoint = Mltx[x]/x_mag;
         ypoint = Mlty[y]/x_mag;
         DrawSubWindow();
      }
      mx = x;
      my = y;
   }
}
/* -------------- */
   void
   Vpointer(x, y)
   int x, y;
/* -------------- */
{
   XPoint a[3];
                                 /* fill pattern is shifted by 1 pixel => -1 */
   a[0].x = x-1;    a[0].y = y-1;
   a[1].x = x-6;    a[1].y = y+10;
   a[2].x = x+4;    a[2].y = y+10;

   line_color("red");
   XFillPolygon(theDisp, subWindow, theGC, a, 3, Convex, CoordModeOrigin);
}

/* -------------- */
   void
   Cpointer(x, y )
   int x, y;
/* -------------- */
{
   int  i;
   XPoint a[12];

   for (i=0; i < 12; i++) {
      a[i].x = sm_cir[i].x + x;
      a[i].y = sm_cir[i].y + y;
   }

   line_color("red");
   XDrawPoints(theDisp, GWindow, theGC, a, 12, CoordModeOrigin);
}

/* -------------- */
   void
   Cpointer_PIXWIN(x, y,color )
   int x, y;
   char * color ;
/* -------------- */
{
   int  i;
   XPoint a[12];

   for (i=0; i < 12; i++) {
      a[i].x = sm_cir[i].x + x;
      a[i].y = sm_cir[i].y + y;
   }

   line_color(color);
   XDrawPoints(theDisp, pxWind, theGC, a, 12, CoordModeOrigin);
}

/* ---------------- */ /* tracks continuousely v pointer and sets image nr */
   void
   Track_Vpointer()
/* ---------------- */
{
   int          Im_Old, im, c_im;
   Window       rW, cW;
   u_int        key;
   int          x, y, rx, ry;
   int          aaa = avr_grp, ddd = diff_im, redr;

   im = Im_Old = Im_Nr;

   New_Cursor(theDisp, subWindow, XC_left_ptr, "red", "white");

   while (XQueryPointer(theDisp, subWindow, &rW, &cW,
                                &rx, &ry, &x, &y, &key)) {
      if ( !(key & Button1Mask) ) break;    /* button released */

      if ( v_point_x != x ) {   /* the marker was moved */

         c_im = (min(gx, max(0, x - xorigin[xc][yc]))*npoints) / gx;
         if ( c_im < 0 )       c_im = 0;
         if ( c_im > npoints - Av_length ) c_im = npoints - Av_length;
         if ( im != c_im ) {
            Im_Nr = c_im;
            XClearWindow(theDisp, GWindow);
            DrawSubWindow();
         }
         im = c_im;
      }
   }
   v_point_x = xorigin[xc][yc] + Im_Nr*gx/(npoints-1);

   if ( Im_Nr != Im_Old ) {
      diff_im = fim_dif;
      avr_grp = fim_avr;
      Put_image(Im_Nr);
      redr =  avr_grp * 4 + (aaa - avr_grp) *2 + ddd - diff_im;
      if ( redr ) {
         old_im = -1;    /* to avoid faster reload of old data */
         redraw_graph();
         DrawTopWindow();
         DrawSubWindow();
      }
   }

   New_Cursor(theDisp, subWindow, XC_left_ptr, "blue", "yellow");
}

/* ------------------ */
   int is_file(fname)
   char  *fname;
/* ------------------ */
{
   FILE          *fp;

   if ( (fp = fopen(fname, "r")) != NULL ) {    /* return = 1 if file exist */
      fclose(fp);
      return(1);
   }
   else
      return(0);
}

/* ----------------------------- */   /* write plot to file  */
   void print_plot(ask_file)
   int ask_file ;
/* ----------------------------- */
{
   int  i, x, y;
   static char noask_suffix[40] = "\0" ;

   if( ask_file ){
      if ( txtW_ON ) {
         XBell(theDisp, 100); return;
      }
      txtW_ON = 1;

      x = 50 + GL_DLX;
      y = 50 + GT_DLY;

      strp[0] = ASC_NUL;
      take_file_name(theDisp, GWindow , CMap, txtGC, mfinfo, x, y, strp, 41,
                     "Enter output plot name:", 1 );
      txtW_ON = 0 ;

   } else {

     if( noask_suffix[0] == '\0' ){
       if ( txtW_ON ) {
          XBell(theDisp, 100); return;
       }
       txtW_ON = 1;

       x = 50 + GL_DLX;
       y = 50 + GT_DLY;

       strp[0] = ASC_NUL;
       take_file_name(theDisp, GWindow , CMap, txtGC, mfinfo, x, y, strp, 41,
                     "Enter suffix for plot filenames:" , 0 );
       txtW_ON = 0 ;

       i = strlen(strp) ;
       if( i <= 0 || i >= 38 ){
          XBell(theDisp, 100); return;
       }

       strncpy( noask_suffix , strp , 40 ) ;
     }

     sprintf( strp , "%03d_%03d%s" ,  xpoint,ypoint,noask_suffix ) ;
   }

   if ( strp[0] != ASC_NUL ) {
      sprintf(plotbuf,"%d\n", val[xc][yc][0]);
      for (i=1; i < npoints; i++) {
          sprintf(fnum,"%d\n", val[xc][yc][i]);
          strcat(plotbuf, fnum);
      }

      isize = strlen(plotbuf);
      i = WRite_iqm(strp, &isize, plotbuf);
      if ( i != 0 ){
         XBell(theDisp, 100);
      } else {
         fprintf(stderr,"*** wrote plot file %s\n",strp) ;
      }
   }

   txtW_ON = 0;
}

/* --------------------- */
   int save_all_images()
/* --------------------- */
{
   int  i, x, y;

   if ( txtW_ON ) {
      XBell(theDisp, 100); return(2);
   }
   txtW_ON = 1;

   x = 50 + GL_DLX;
   y = 50 + GT_DLY;

   strp[0] = ASC_NUL ;
   take_file_name(theDisp, GWindow , CMap, txtGC, mfinfo, x, y, strp, 45,
                  "Enter output root name:", 0);
   im_f = min(3, dec_indx(npoints)-1);  /* 2d index in formt[][] */

try_again:
   if ( strp[0] != ASC_NUL ) {
      /* test for first file */
      sprintf(strF, formt[0][im_f], strp, 1);
      if ( is_file(strF) ) {
         strp[0] = ASC_NUL;
         sprintf (strT, "File exist: %s", strF);
         take_file_name(theDisp, GWindow , CMap, txtGC, mfinfo, x, y, strp, 50,
                  strT, 0);
         goto try_again;
      }
      for (i=0; i < npoints; i++) {
         sprintf(strF, formt[0][im_f], strp, i+1);
         /* we have already decided whether to swap */
         if ( swap_bytes && allim[i]->kind == MRI_short ) {
           swap_2(MRI_SHORT_PTR(allim[i]), allim[i]->nx*allim[i]->ny*2);
         }
         else if ( swap_bytes && allim[i]->kind == MRI_float ) {
           swap_4(MRI_FLOAT_PTR(allim[i]), allim[i]->nx*allim[i]->ny*4);
         }

         mri_write(strF, allim[i]) ;

         if ( swap_bytes && allim[i]->kind == MRI_short ) {
           swap_2(MRI_SHORT_PTR(allim[i]), allim[i]->nx*allim[i]->ny*2);
         }
         else if ( swap_bytes && allim[i]->kind == MRI_float ) {
           swap_4(MRI_FLOAT_PTR(allim[i]), allim[i]->nx*allim[i]->ny*4);
         }
      }
   }

   txtW_ON = 0;
   return(0);
}

/* -------------------- save not normalized average image - called by 'X' */
   int save_avr_array()
/* -------------------- */
{
   int  i, x, y;

   if ( txtW_ON ) {
      XBell(theDisp, 100); return(2);
   }
   txtW_ON = 1;

   x = 50 + GL_DLX;
   y = 50 + GT_DLY;

   strp[0] = ASC_NUL ;
   take_file_name(theDisp, GWindow , CMap, txtGC, mfinfo, x, y, strp, 41,
                  "Enter output image name:" , 1 );
   for( i=0 ; i < IM_ARR ; i++ ) a_rot[i] = tmp_ar[i];
   for( i=0 ; i < IM_ARR ; i++ ) tmp_ar[i] = av_ar[i];
   if ( strp[0] != ASC_NUL ) {
      mri_write( strp , im_tmp_ar ) ;
   }
   for( i=0 ; i < IM_ARR ; i++ ) tmp_ar[i] = a_rot[i];

   txtW_ON = 0;
   return(0);
}

/* ----------------- */
   int save_act_im()
/* ----------------- */
{
   int  x, y;

   if ( txtW_ON ) {
      XBell(theDisp, 100); return(2);
   }
   txtW_ON = 1;

/* AJJ  OK in relation to the root window
   XGetWindowAttributes(theDisp, theWindow, &wat);
   XTranslateCoordinates(theDisp, theWindow, wat.root, -wat.border_width,
                        -wat.border_width, &x, &y, &ww);
   x += eWIDE/2;
   y += eHIGH/2;

   strp[0] = ASC_NUL ;
   take_file_name(theDisp, rootW, CMap, txtGC, mfinfo, x, y, strp, 40,
                  "Enter image name:" ,  1 );
*/

   x = 50 + GL_DLX;
   y = 50 + GT_DLY;

   strp[0] = ASC_NUL ;
   take_file_name(theDisp, GWindow , CMap, txtGC, mfinfo, x, y, strp, 41,
                  "Enter output image name:" , 1 );
   if ( strp[0] != ASC_NUL ) {
      /* we have already decided on swapping     27 Aug 2004 [rickr] */

      if ( swap_bytes && im_tmp_ar->kind == MRI_short ) {
        swap_2(MRI_SHORT_PTR(im_tmp_ar), im_tmp_ar->nx*im_tmp_ar->ny*2);
      }
      else if ( swap_bytes && im_tmp_ar->kind == MRI_float ) {
        swap_4(MRI_FLOAT_PTR(im_tmp_ar), im_tmp_ar->nx*im_tmp_ar->ny*4);
      }

      mri_write( strp , im_tmp_ar ) ;

      if ( swap_bytes && im_tmp_ar->kind == MRI_short ) {
        swap_2(MRI_SHORT_PTR(im_tmp_ar), im_tmp_ar->nx*im_tmp_ar->ny*2);
      }
      else if ( swap_bytes && im_tmp_ar->kind == MRI_float ) {
        swap_4(MRI_FLOAT_PTR(im_tmp_ar), im_tmp_ar->nx*im_tmp_ar->ny*4);
      }
   }

   txtW_ON = 0;
   return(0);
}

/* popup window which take a file name of max length str_l */
/* --------------------------------------- return name of file in name */
   void
   take_file_name(theDisp, topW, CMap, txtGC, finf, x, y, name, str_l, text, check)
   Display     *theDisp;
   Window      topW;                    /* top window of this popup one */
   Colormap    CMap;
   GC          txtGC;
   XFontStruct *finf;
   int         x, y;                    /* relative position */
   char *name, *text;                   /* name - file name, max str_l chars */
   int  str_l;                          /* text - window's header */
   int  check ;                         /* to check if name exists already? */
/* --------------------------------------- */
{
   int    w_l1, w_h1, w_l2, w_h2, h_txt, expose = 0, name_OK = 0, length;
   int    x_txt, y_txt, x_l2, y_l2, error = 0;
   int    eee[2];
   char   *errr = "File exist:", *DT = "_";

   XEvent ev;
   XColor             any_col, rgb_col;
   unsigned long      Border, back1, back2; // long is OK here AJJ
   Window             txtWindow1, txtWindow2;
   XSizeHints         hints;

   eee[0] = eee[1] = 0;

   if (!(XAllocNamedColor(theDisp, CMap, "red", &any_col, &rgb_col)))
      FatalError ("XAllocNamedColor problem. AJ in save_act_im()");
   Border = any_col.pixel;
   if (!(XAllocNamedColor(theDisp, CMap, "yellow", &any_col, &rgb_col)))
      FatalError ("XAllocNamedColor problem. AJ in save_act_im()");
   back1= any_col.pixel;
   if (!(XAllocNamedColor(theDisp, CMap, "white", &any_col, &rgb_col)))
      FatalError ("XAllocNamedColor problem. AJ in save_act_im()");
   back2= any_col.pixel;

   hints.flags = USPosition;
   hints.x = x;
   hints.y = y;

   h_txt =  finf->max_bounds.ascent + finf->max_bounds.descent;
   w_l1 = (str_l - 1) * finf->max_bounds.width + 30;
   w_h1 = 3 * h_txt + 18;
   x_l2 = 10;
   y_l2 = 2 * h_txt + 5;
   w_l2 = w_l1 - 20;
   w_h2 = h_txt + 8;
   x_txt = 5;
   y_txt = h_txt;

   txtWindow1 =  XCreateSimpleWindow(theDisp, topW, x, y, w_l1, w_h1, 1,
                                     Border, back1);
   XSelectInput(theDisp, txtWindow1, ExposureMask);
   XSetStandardProperties(theDisp, txtWindow1 , "Text", "Text", None,
                          NULL, 0, &hints);
   txtWindow2 =  XCreateSimpleWindow(theDisp, txtWindow1, x_l2, y_l2,
                            w_l2, w_h2, 1, Border, back2);
   XSelectInput(theDisp, txtWindow2, ExposureMask | KeyPressMask);
   New_Cursor(theDisp, txtWindow1, XC_left_ptr, "blue", "white");
   New_Cursor(theDisp, txtWindow2, XC_xterm, "blue", "white");
   XMapWindow(theDisp, txtWindow1);
   XMapWindow(theDisp, txtWindow2);

   while ( !expose ) {                /* wait for expose */
      Window wind;
      XNextEvent(theDisp, &ev);
      switch (ev.type) {
         case Expose: {
            XExposeEvent *e = (XExposeEvent *) &ev;
            wind = e->window;
            if (wind ==  txtWindow1) eee[0] = 1;
            if (wind ==  txtWindow2) eee[1] = 1;
            expose = eee[0] * eee[1];
         }
         default:                /* ignore unexpected events */
            break;
      }
   } /* end of while( !expose ) */

   txt_color("red");
   XDrawString(theDisp, txtWindow1, txtGC, 9, h_txt + 3 ,
               text, strlen(text));
   strncat(name, DT, 1);
   txt_color("blue");
   XDrawString(theDisp, txtWindow2, txtGC, x_txt, y_txt,
               name, strlen(name));

   txt_color("black");

   while ( !name_OK ) {                /* wait for file name */
      Window wind;
      XNextEvent(theDisp, &ev);
      switch (ev.type) {
         case Expose: {
            XExposeEvent *e = (XExposeEvent *) &ev;

            wind = e->window;
            if (wind ==  txtWindow1) {
               txt_color("red");
               XDrawString(theDisp, txtWindow1, txtGC, 30, h_txt + 3 ,
                           text, strlen(text));
               txt_color("black");
            }
            if (wind ==  txtWindow2) {
               txt_color("blue");
               XDrawString(theDisp, txtWindow2, txtGC, x_txt, y_txt,
                           name, strlen(name));
               txt_color("black");
            }
         }
         case KeyPress: {
            XKeyEvent *key_event = (XKeyEvent *) &ev;
            u_char      buf[128];
            KeySym    ks;
            XComposeStatus status;

            wind = key_event->window;
            if (wind ==  txtWindow2) {
               if (error) {
                  XClearWindow(theDisp, txtWindow1);
                  txt_color("red");
                  XDrawString(theDisp, txtWindow1, txtGC, 25, h_txt + 3 ,
                              text, strlen(text));
                  txt_color("black");
                  error = 0;
               }
               buf[0] = 0;
               XLookupString(key_event, (char *)buf, 128, &ks, &status);
               if ( (ks == XK_Return)   ||
                    (ks == XK_Linefeed) ) {
                  length = strlen(name);
                  name[length-1] = ASC_NUL;
                  if ( name[0] == ASC_NUL ) name_OK = 1;
                  else if ( check && is_file(name) ) {
                     XClearWindow(theDisp, txtWindow1);
                     txt_color("red");
                     XDrawString(theDisp, txtWindow1, txtGC, 10, h_txt ,
                           errr, strlen(errr));
                     XDrawString(theDisp, txtWindow1, txtGC, 15, 2*h_txt ,
                           name, strlen(name));
                     txt_color("black");
                     name[0] = ASC_NUL;
                     strncat(name, DT, 1);
                     XBell(theDisp, 100);
                     error = 1;
                  }
                  else
                     name_OK = 1;
               }
               else if ( ((ks >= XK_plus) && (ks <= XK_9)) ||
                         ((ks >= XK_A) && (ks <= XK_Z))  ||
                         ((ks >= XK_asciicircum) && (ks <= XK_z)) ||
                          (ks == XK_asciitilde)                   ||
                          (ks == XK_numbersign)                   ||
                          (ks == XK_colon     )                   ||
                          (ks == XK_at        )                   ||
                          (ks == XK_underscore)                   ||
                          (ks == XK_exclam)     ) {
                  if ( (length = strlen(name)) + 1 > str_l ) {
                     name[length-1] = ' ';
                     XBell(theDisp, 100);
                  }
                  else if ( ((buf[0] >= 43) && (buf[0] <= 57)) ||
                            ((buf[0] >= 65) && (buf[0] <= 90)) ||
                            ((buf[0] >= 94) && (buf[0] <= 122))||
                             (buf[0] == 126)                   ||
                             (buf[0] == '!') ) {
                     name[length - 1] = ASC_NUL;
                     strncat(name, (char *)buf, 1);
                     strncat(name, DT, 1);
                  }
               }
               else if ( ((ks >= XK_Shift_L) && (ks <= XK_Hyper_R)) ||
                         ((ks >= XK_F1) && (ks <= XK_F35)) ||
                         ((ks >= XK_KP_0) && (ks <= XK_KP_9)) )
                     ;     /* do nothing */
               else if ((ks == XK_BackSpace) || (ks == XK_Delete)) {
                  if ( (length = strlen(name)) > 1 ) {
                     name[length - 2] = ASC_NUL;
                     strncat(name, DT, 1);
                  }
                  else
                     XBell(theDisp, 100);
               }
               else
                  XBell(theDisp, 100);
            }
            XClearWindow(theDisp, txtWindow2);
            txt_color("blue");
            XDrawString(theDisp, txtWindow2, txtGC, x_txt, y_txt,
                        name, strlen(name));
            txt_color("black");
         }
         default:                /* ignore unexpected events */
            break;
      }

   } /* end of while( !name_OK ) */

   txt_color("black");
   XDestroyWindow(theDisp, txtWindow1);
}

/**************************************************************************/
/**************************************************************************/
/**************************************************************************/

#define MAX_NREF (MAX_NUMORT+MAX_POLORT+99)


void RWC_setup_fims( imflag )
   int imflag ;
{
   int kim , nref ;
   float current_refs[MAX_NREF] ;
   float *id ;
   float *pc , *alp ;
   thresh_result thr ;

   float        alp_max , alp_thr[MAX_FIM_COLORS] ;
   register int jj , ii , nvox ;

   static MRI_IMAGE * thrim = NULL ;
   static short *     thrar = NULL ;
   static short       imthr ;

   MRI_IMAGE * flim = NULL;
   float     * flar = NULL;
   double      scl = 0., dkim , pval ;
   int         make_thrim , good ;

   static MRI_IMARR * DFILT_rim  = NULL ;

   /*** check RWC_ideal for legality ***/

   if( RWC_ideal == NULL ){
      RWC_do_overfim = 0 ;
      if( RWC_imover != NULL ) { free(RWC_imover) ; RWC_imover = NULL ; }
      return ;
   }

   id = RWC_ideal->ts ;

   if( RWC_ideal->len < npoints ){
      fprintf(stderr,
              "\n*** reference vector time series file %s too short!\n",
              RWC_ideal->fname ) ;

      RWC_do_overfim = 0 ;
      RWC_free_time_series( RWC_ideal ) ; RWC_ideal = NULL ;
      if( RWC_imover != NULL ) { free(RWC_imover) ; RWC_imover = NULL ; }
      XBell(theDisp,100) ; return ;
   }

   for( ii=0 ; ii < RWC_numort ; ii++ ){
      if( RWC_ort[ii] == NULL || RWC_ort[ii]->len < npoints ){
         fprintf( stderr ,
                  "\n*** ort vector time series file %s too short!\n",
                  RWC_ort[ii]->fname ) ;
         RWC_do_overfim = 0 ;
         if( RWC_imover != NULL ) { free(RWC_imover) ; RWC_imover = NULL ; }
         XBell(theDisp,100) ; return ;
      }
   }

   /*** miscellaneous startup ***/

   if( RWC_fim_colors == 0 ) RWC_init_fim_colors() ;

   nref = RWC_numort + RWC_polort + 2 ;

   if( DFILT_code == DFILT_TIME ){
      nref += DFILT_NREF ;     /* number of refs from DFILT      */
   }

   if( LSQ_ref[0] == NULL ){
      for( ii=0 ; ii < MAX_TOTAL_REF ; ii++ )
         LSQ_ref[ii] = RWC_blank_time_series(NF_MAX) ;
   }
   LSQ_refcount = nref ;

   RWC_nxim = RWC_nyim = im_size ;  nvox = RWC_nxim * RWC_nyim ;

   RWC_refs   = new_references( nref ) ;
   RWC_voxcor = new_voxel_corr( RWC_nxim * RWC_nyim , nref ) ;

   if( RWC_pcim  != NULL ) mri_free( RWC_pcim ) ;
   if( RWC_alpim != NULL ) mri_free( RWC_alpim ) ;

   RWC_pcim  = mri_new( RWC_nxim , RWC_nyim , MRI_float ) ;
   RWC_alpim = mri_new( RWC_nxim , RWC_nyim , MRI_float ) ;
   pc        = mri_data_pointer( RWC_pcim ) ;
   alp       = mri_data_pointer( RWC_alpim ) ;

   if( RWC_refs == NULL || RWC_voxcor == NULL ){
      fprintf(stderr,"*** ==> malloc fail while fimming\a\n") ;
      mri_free( RWC_pcim )  ; RWC_pcim  = NULL ;
      mri_free( RWC_alpim ) ; RWC_alpim = NULL ;
      if( RWC_voxcor != NULL ) free_voxel_corr( RWC_voxcor ) ;
      if( RWC_refs   != NULL ) free_references( RWC_refs ) ;
      if( RWC_ideal  != NULL ) {RWC_free_time_series(RWC_ideal);RWC_ideal=NULL;}
      if( RWC_imover != NULL ) {free(RWC_imover) ; RWC_imover=NULL;}
      RWC_do_overfim = 0 ;
      return ;
   }

/*** prepare to make thresholding image, if not already present ***/

   make_thrim = (thrim == NULL) ;
   if( make_thrim ){
      flim = mri_new( RWC_nxim , RWC_nyim , MRI_float ) ;
      flar = mri_data_pointer( flim ) ;
      scl  = 0.0 ;
      for( ii=0 ; ii < nvox ; ii++ ) flar[ii] = 0.0 ;
   }

/*** find first "good" image and initialize DFILT, if needed ***/

/*** put images thru the fim wringer ***/

   for( kim=0 ; kim < npoints ; kim++ ){
      good = fabs(id[kim]) < 33333.0 ;        /* check ideal and orts for OK */
      for( ii=0 ; ii < RWC_numort ; ii++ )
         good = good && ( fabs(RWC_ort[ii]->ts[kim]) < 33333.0 ) ;

      /** load the current_refs even if not good **/

      dkim = kim / ((double) npoints) ;
      pval = 1.0 ;
      for( ii=0 ; ii <= RWC_polort ; ii++ ){
         current_refs[ii] = pval ;
         pval            *= dkim ;
      }

      for( ii=0 ; ii < RWC_numort ; ii++ )
         current_refs[ii+RWC_polort+1] = RWC_ort[ii]->ts[kim] ;

      current_refs[nref-1] = id[kim] ;       /* load ideal as last ref */

      /** load LSQ_ref **/

      for( ii=0 ; ii < nref ; ii++ ){
         dkim = current_refs[ii] ;
         LSQ_ref[ii]->ts[kim] = (fabs(dkim) < 33333.0) ? dkim : 0.0 ;
      }

      /** process if all refs are good **/

      if( good ){
	 nowim = SAR(kim) ;
         update_references( current_refs , RWC_refs ) ;
         update_voxel_corr( nowim , RWC_refs , RWC_voxcor ) ;

	 if( make_thrim ){
	    for( ii=0 ; ii < nvox ; ii++ ) flar[ii] += abs(nowim[ii]) ;
	    scl++ ;  /* one more image */
         }
      }  /* end if(good) */
   } /* end for kim */

   DESTROY_IMARR( DFILT_rim ) ;

   /*** setup threshold on image intensity ***/

   if( make_thrim ){
      scl   = 1.0 / scl ;
      thrim = mri_to_short( scl , flim ) ;           /* average of abs */
      thrar = mri_data_pointer( thrim ) ;
      imthr = (short) ( 0.05 * mri_max( thrim ) ) ;  /* 5% threshold */
      mri_free( flim ) ;                             /* old junk */
   }

   /*** compute and threshold on correlation coefficient ***/

   get_pcor_thresh_coef( RWC_refs , RWC_voxcor ,
                         0.001 , 0.001 , pc , alp , &thr ) ;

   /*** compute least squares fit coefficients in each voxel ***/

   DESTROY_IMARR( LSQ_fitim ) ;
   INIT_IMARR( LSQ_fitim ) ;
   for( ii=0 ; ii < nref ; ii++ ){
      flim = mri_new( RWC_nxim , RWC_nyim , MRI_float ) ;
      ADDTO_IMARR(LSQ_fitim,flim) ;
      LSQ_fit[ii] = MRI_FLOAT_PTR(flim) ;
   }
   get_lsqfit( RWC_refs , RWC_voxcor , LSQ_fit ) ;

   if( RWC_imover != NULL ){  /* toss old overlay, if any */
      free(RWC_imover) ;
      RWC_imover = NULL ;
   }

/*** threshold results on voxel data and fim results ***/

   alp_max = 0.0 ;
   for( ii=0 ; ii < nvox ; ii++ ){
      if( fabs(pc[ii]) >= RWC_pcthresh ){
         alp_max = (alp[ii]>0) ? MAX(alp_max, alp[ii])
                               : MAX(alp_max,-alp[ii]) ;
      }
   }

   if( alp_max > 0.0 ){

      RWC_imover = RWC_create_overlay( RWC_nxim , RWC_nyim ) ;

      for( ii=0 ; ii < RWC_fim_colors ; ii++ ){
         alp_thr[ii] = RWC_fim_thresh[ii] * alp_max ;
      }

   /*** loop over all voxels ***/

      for( ii=0 ; ii < nvox ; ii++ ){

         if( thrar[ii] >= imthr && fabs(pc[ii]) >= RWC_pcthresh ){

            if( alp[ii] >= alp_thr[0] ){  /* positive over threshold */

               for( jj=1 ; jj < RWC_fim_colors ; jj++ )  /* find place */
                  if( alp[ii] < alp_thr[jj] ) break ;

               RWC_imover[ii] = RWC_fim_colors_pos[jj-1] ;

            }

            else if( alp[ii] <= -alp_thr[0] ){  /* negative over threshold */

               for( jj=1 ; jj < RWC_fim_colors ; jj++ )  /* find place */
                  if( alp[ii] > -alp_thr[jj] ) break ;

               RWC_imover[ii] = RWC_fim_colors_neg[jj-1] ;

            }
         } /* end if nowim && pc */
     } /* end for ii (voxel loop) */

     pc[1]  = 1.0     ; pc[2]  = -1.0 ;
     alp[1] = alp_max ; alp[2] = -alp_max ;

     RWC_overhide = 0 ;

     if( imflag ){  /* display results */

        ii = Im_Nr ;

        mri_add_name( "correlation_image" , RWC_pcim ) ;
        add_extra_image( RWC_pcim ) ;

        mri_add_name( "fim_image" , RWC_alpim ) ;
        add_extra_image( RWC_alpim ) ;

        Im_Nr = ii ;
        Put_image( Im_Nr ) ;
     }

   } /* end if alp_max */

   free_voxel_corr( RWC_voxcor ) ; free_references( RWC_refs ) ;

   return ;
}

/****************************************************************************/

int FIM_action(ikey)
  int ikey ;
{
   int i ;
#ifdef USE_MCW
   static int mcw_count = 0 ;
#endif

   for( i=FIM_first_key ; i <= FIM_last_key ; i++ ){
      if( FIM_pressed ) XUnmapWindow(theDisp,  key[i].wid);  /* second press */
      else              XMapWindow  (theDisp,  key[i].wid);  /* first press */
   }

   if( FIM_pressed ){                  /* second press */
      FIM_pressed = 0 ;
      xtkeys[kFIM].st = key_kFIM_FIM ;
      DrawKey(kFIM) ;                  /* restore key label */

      if( RWC_do_overfim && FIM_modified ){
#ifdef USE_MCW
         mcw_count++ ; Put_image(-1) ;  /* show the logo */
#endif
         InvertKey(kFIM) ;
         XFlush(theDisp) ;
         RWC_setup_fims( 1 ) ;  /* 1 => display results */
         InvertKey(kFIM) ;
      }

      Put_image( Im_Nr ) ;
      DrawTopWindow() ;
      redraw_graph() ;

   } else {                            /* first press */

     FIM_pressed  = 1;
     FIM_modified = 0 ;
     xtkeys[kFIM].st = key_kFIM_GO ;
     DrawKey(kFIM) ;                  /* set key label to "GO" */
   }
   return 0;
}

/****************************************************************************/

int FIM_selection(ikey)
  int ikey ;
{
   int x , y , ifim = ikey - FIM_first_key , ref_modified = 0 ;
   float fval ;
   char *cpt ;
   char stst[64] ;
   time_series *newts ;

   if ( txtW_ON || ikey < FIM_first_key || ikey > FIM_last_key ) {
      XBell(theDisp, 100); return 0;
   }

   if( FIM_selection_name[ifim] != NULL ){  /* if dialog box has a label */
      txtW_ON = 1;

      if( ifim != 1 ){
	 strcpy(stst,FIM_selection_name[ifim]) ;
      } else {
              if( rot_direct == -1 ) strcpy(stst,"Reference ") ;
         else if( rot_direct ==  0 ) strcpy(stst,"Ort ") ;
         else                        strcpy(stst,"Useless ") ;
	 strcat(stst,FIM_selection_name[ifim]) ;
      }

      x = 50 + GL_DLX;
      y = xtkeys[FIM_first_key].y ;

      strp[0] = ASC_NUL ;
      take_file_name(theDisp, GWindow , CMap, txtGC, mfinfo, x, y, strp, 41,
                     stst , 0 );
      txtW_ON = 0 ;
      if( strlen(strp) <= 0 ) return 0;
   }

   switch( ikey ){

      /*** set threshold ***/

      case kFI1:
         fval = strtod( strp , &cpt ) ;
         if( *cpt != ASC_NUL || fval < 0.0 || fval > 1.0 ){
            fprintf(stderr,
                    "\n*** Illegal correlation threshold %s!\n" , strp ) ;
            XBell(theDisp, 100); return 0;
         } else {
            RWC_pcthresh = fval ;
         }
      break ;  /* end of ikey=kFI1 */

     /*** set reference time series ***/

      case kFI2:

         /*** turn FIM off ***/

         if( strcmp(strp,"!nofim") == 0 ){

            RWC_do_overfim = 0 ;
            if( RWC_imover != NULL ) { free(RWC_imover) ; RWC_imover = NULL ; }
            if( RWC_ideal  != NULL ){
               RWC_free_time_series( RWC_ideal ) ;
               RWC_ideal = NULL ;
            }

         } else {

            newts = RWC_read_time_series( strp ) ;
            if( newts == NULL || newts->len < npoints ){
               fprintf(stderr,
                       "\n*** Illegal vector file %s!\n" , strp ) ;
               XBell(theDisp, 100); return 0;
            }

            if( rot_direct == -1 ){  /* left button */
               if( RWC_ideal != NULL ) RWC_free_time_series( RWC_ideal ) ;
               RWC_ideal      = newts ;
               RWC_do_overfim = 1 ;

            } else if( rot_direct == 0 ){ /* middle button */
               if( RWC_numort >= MAX_NUMORT ){ XBell(theDisp, 100); return 0; }
               RWC_ort[RWC_numort++] = newts ;

            } else { XBell(theDisp, 100); return 0; }

         }
         ref_modified      = 1 ;
	 kFI3_refsum_count = -1 ;
      break ;  /* end of ikey=kFI2 */

      case kFI3:
         if( rot_direct == 1 ){  /* right button => change key */

            if( (rot_state & (ShiftMask | ControlMask)) == 0 ){
               kFI3_status = (kFI3_status+1) % (kFI3_NUM) ;       /* forward */
            } else {
               kFI3_status = (kFI3_status-1+kFI3_NUM) % (kFI3_NUM) ;/*backward*/
            }
            xtkeys[kFI3].st = key_kFI3[kFI3_status] ;
            DrawKey(kFI3) ;
            return 0;
         }  /* end of handling right button press */

         /*** perform actions based on status of key ***/

         switch( kFI3_status ){

            case -1:{
               int ii ;

               fprintf(stderr,"\a\n") ;
               fprintf(stderr,"*** Button 3 cycles between options  ***\n");
               fprintf(stderr,"*** [Shift+Button 3 cycles backwards ***\n");
               fprintf(stderr,"*** Buttons 1 and 2 choose options:  ***\n");
               fprintf(stderr,"***   for -+ options, Button 1 is -  ***\n");
               fprintf(stderr,"***                   Button 2 is +  ***\n");
               fprintf(stderr,"*** Keys available:\n") ;

               for( ii=0 ; ii < kFI3_NUM ; ii++ )
                  fprintf(stderr,"    %s %s\n" ,
                          key_kFI3[ii] , key_kFI3_help[ii] ) ;

            }
            /* fall through to default on purpose */

            default:{
               XBell(theDisp,100) ; return 0;
            }

         /*** use current pixel ***/

            case 0:
            case 1:{
               char * fake_name ;
               float * id ;
               int ii , addit ;

               fake_name = (char *) malloc( 24 ) ;
               if( fake_name == NULL ){
                  XBell(theDisp,100) ; return 0;
               }

               addit = (kFI3_status == 1) && ( RWC_ideal != NULL )
                                          && ( RWC_do_overfim )    ;

               if( !addit ){
                  RWC_free_time_series( RWC_ideal ) ;
                  RWC_ideal = RWC_blank_time_series( npoints ) ;
               }

               id = RWC_ideal->ts ;

	       if( addit && kFI3_refsum_count < 0 ){
		  for( ii=0 ; ii < npoints ; ii++ )
		     if( fabs(id[ii]) < 33333.0 ) id[ii] *= 0.01 ;

                  kFI3_refsum_count = 1 ;
               }

               if( addit ){
                  for( ii=0 ; ii < npoints ; ii++ )
                                          id[ii] += 0.01*val[xc][yc][ii];
                  kFI3_refsum_count++ ;
               } else {
                  for( ii=0 ; ii < npoints ; ii++ )
                                          id[ii]  = 0.01*val[xc][yc][ii];
                  kFI3_refsum_count = 1 ;
               }

               if( addit ){
                  sprintf( fake_name , "%d:+PIX%d:%d" , kFI3_refsum_count,xpoint,ypoint ) ;
               } else {
                  sprintf( fake_name , "PIX%d:%d" , xpoint,ypoint ) ;
               }
               if( RWC_ideal->fname != NULL ) free( RWC_ideal->fname ) ;
               RWC_ideal->fname = fake_name ;

               RWC_do_overfim = 1 ;
               ref_modified   = 1 ;
            }
            break ;  /* end of status=0 or 1 */

            /*** median filter ***/

            case 2:{
               char * fake_name ;
               int flen ;

               if( RWC_ideal == NULL || !RWC_do_overfim ){
                  XBell(theDisp,100) ; return 0;
               }

               RWC_medfilt_time_series( RWC_ideal ) ;

               flen = 4 ;
               if( RWC_ideal->fname != NULL ) flen += strlen(RWC_ideal->fname) ;

               fake_name = (char *) malloc( flen ) ;
               if( fake_name != NULL ){
                  strcpy(fake_name,RWC_ideal->fname) ;
                  strcat(fake_name,"!m") ;
                  if( RWC_ideal->fname != NULL ) free( RWC_ideal->fname ) ;
                  RWC_ideal->fname = fake_name ;
               }

               ref_modified = 1 ;
            }
            break ;  /* end of status=2 */

            /*** modify threshold ***/

            case 3:{

               if( RWC_ideal == NULL || !RWC_do_overfim ){
                  XBell(theDisp,100) ; return 0;
               }

               if( rot_direct   == -1  ) RWC_pcthresh -= 0.05 ;
               if( rot_direct   ==  0  ) RWC_pcthresh += 0.05 ;
               if( RWC_pcthresh >= 1.0 ) RWC_pcthresh  = 0.99999 ;
               if( RWC_pcthresh <  0.0 ) RWC_pcthresh  = 0.0 ;
            }
            break ;  /* end of status=3 */

            /*** nofim ***/

            case 4:{
               RWC_do_overfim = 0 ;
               if( RWC_imover != NULL ) {free(RWC_imover); RWC_imover = NULL;}
               if( RWC_ideal != NULL ){
                  RWC_free_time_series( RWC_ideal ) ;
                  RWC_ideal = NULL ;
               }
               ref_modified = 1 ;
            }
            break ;  /* end of status=4 */

            /*** number of ref plots to show ***/

            case 5:{
               int kold = kFI3_show_ref ;

               if( !RWC_do_overfim || RWC_ideal == NULL ){
                  XBell(theDisp,100) ; return 0;
               }
               if( rot_direct == -1 && kFI3_show_ref > 0 ) kFI3_show_ref -- ;
               if( rot_direct ==  0 && kFI3_show_ref < 2 ) kFI3_show_ref ++ ;

               if( kold == kFI3_show_ref ){ XBell(theDisp,100) ; return 0; }

               ref_modified = 1 ;   /* force redraw */
            }
            break ;  /* end of status=5 */

            /*** write current reference to a file ***/

            case 6:{
               float val , yscal , yoff ;
               float *id ;
               int ii , ival ;
               FILE * fp ;

               if( !RWC_do_overfim || RWC_ideal == NULL ){ 
                  XBell(theDisp,100) ;
                  return 0;
               }

               /*** get file name ***/

               txtW_ON = 1;
               x       = 50 + GL_DLX;
               y       = xtkeys[FIM_first_key].y ;
               strp[0] = ASC_NUL ;
               take_file_name(theDisp, GWindow , CMap, txtGC, mfinfo, x, y,
                              strp, 41, "Filename for Reference Function:", 1 );
               txtW_ON = 0 ;
               if( strlen(strp) <= 0 ) return 0;

               /*** find scale factor for plot ***/

	       if( kFI3_refsum_count > 0 ){
		  yscal = 100.0 / kFI3_refsum_count ;
		  yoff  = 0.0 ;
               } else {
		  yscal = 1.0 ;
		  yoff  = 0.0 ;
               }
               id = RWC_ideal->ts ;

/*** this code is for rescaling the plot to range 0..10000 ***/
#if 0
               idmax = -99999.0 ;
               idmin =  99999.0 ;

               for( ii=0 ; ii < npoints ; ii++ ){
                 val = id[ii] ;
                 if( fabs(val) < 33333.0 ){
                    idmax = MAX(idmax,val) ;
                    idmin = MIN(idmin,val) ;
                 }
               }
               if( idmax >= 33333.0 || idmax <= -33333.0 || idmax <= idmin )
                 return 0;

               yscal = 10000.0 / (idmax-idmin) ;
               yoff  = idmin ;
#endif
               /*** write plot ***/

               fp = fopen( strp , "w" ) ;
               if( fp == NULL ){
                  fprintf(stderr,"\n*** cannot open file %s\n",strp) ;
                  XBell(theDisp,100) ; return 0;
               }

               for( ii=0 ; ii < npoints ; ii++ ){
                  val  = id[ii] ;
                  ival = (fabs(val) < 33333.0) ? (int) (yscal*(val-yoff))
                                               : 99999 ;
                  fprintf(fp,"%5d\n",ival) ;
               }
               fclose(fp) ;
               fprintf(stderr,"*** wrote plot file %s\n",strp) ;

               return 0;  /* no need to change status, so exit now */
            }
            break ;  /* end of status=6 */

            /*** increment # of polynomial orts ***/

            case 7:{
               int kold = RWC_polort ;

               if( rot_direct == -1 ){
                  if( RWC_polort >= 0 ) RWC_polort -- ;
               } else if( rot_direct == 0 ){
                  if( RWC_polort < MAX_POLORT ) RWC_polort ++ ;
               }
               if( kold == RWC_polort ){ XBell(theDisp,100) ; return 0; }
            }
            break ;  /* end of status=7 */

            /*** make current ref file an ort also ***/

            case 8:{

               if( RWC_ideal == NULL || RWC_numort == MAX_NUMORT ){
                  XBell(theDisp,100) ; return 0;
               }

               RWC_ort[RWC_numort++] = RWC_ideal ;  /* put into ort array */

               /* now, turn fim off, since don't have a ref anymore */

               ref_modified   = 1 ;
               RWC_do_overfim = 0 ;
               RWC_ideal      = NULL ;
               if( RWC_imover != NULL ) {free(RWC_imover); RWC_imover = NULL;}

	       /* switch to ref=plot status; current status is now useless */

               kFI3_status     = 0 ;
               xtkeys[kFI3].st = key_kFI3[kFI3_status] ;
               DrawKey(kFI3) ;
            }
            break ; /* end of status=8 */

            /*** clear all orts ***/

            case 9:{
               int ii ;
               if( RWC_numort == 0 ){ XBell(theDisp,100) ; return 0; }
               for( ii=0 ; ii < RWC_numort ; ii++ ){
                  RWC_free_time_series( RWC_ort[ii] ) ;
                  RWC_ort[ii] = NULL ;
               }
               RWC_numort = 0 ;
            }
            break ; /* end of status=9 */

            /*** modify LSQ_code ***/

            case 10:{
               int old_imed = LSQ_imedit[LSQ_code] ;

               if( RWC_ideal == NULL || !RWC_do_overfim || LSQ_fitim == NULL ){
                  XBell(theDisp,100) ; return 0;
               }

               if( rot_direct   == -1      ) LSQ_code-- ;
               if( rot_direct   ==  0      ) LSQ_code++ ;
               if( LSQ_code < LSQ_NONE     ) LSQ_code = LSQ_LASTCODE ;
               if( LSQ_code > LSQ_LASTCODE ) LSQ_code = LSQ_NONE ;

               DrawTopWindow() ; redraw_graph() ;
               old_im = -1 ;
               if( old_imed != LSQ_imedit[LSQ_code] && Im_Nr < npoints ) Put_image( Im_Nr ) ;

               return 0;
            }
            break ;  /* end of status=10 */

         }  /* end of kFI3_status switch */
      break ; /* end of ikey=kFI3 */

   }  /* end of ikey switch */

   /*** if we get to here, the user changed something,
        so mark the FIM calculations to be redone, and do some graphics ***/

   InvertKey(kFIM) ;  /* flash the FIM key */
   DrawTopWindow() ;  /* update the labels */
   InvertKey(kFIM) ;

   if( ref_modified ){
      redraw_graph() ;
      FIM_edit_time_series( RWC_ideal ) ;
   }

   FIM_modified = 1 ;
   return 0;
}

/****************************************************************************/

int FIM_edit_time_series( vec )
  time_series * vec ;
{
   int i , itop ;
   if( vec == NULL ) return 0;

   /*** blast out first locations, if requested ***/

   itop = MIN( Im_frst-1 , (vec->len)-1 ) ;
   for (i=0; i < itop ; i++) vec->ts[i] = 99999.99 ;

   return 0;
}

/*--------------------------------------------------------------------------*/

/*** delete the current image, if possible ***/

int kill_curr_im()
{
   int kk ;

   if( Im_Nr < npoints ){ XBell(theDisp,100) ; return 0; }

   mri_free( allim[Im_Nr] ) ;
   for( kk=Im_Nr+1 ; kk < N_im ; kk++ ) allim[kk-1] = allim[kk] ;

   N_im-- ;                           /* one less image in the pile */
   old_im = -1 ;                      /* mark saved image as invalid */
   Im_Nr  = MIN( Im_Nr , N_im-1 ) ;   /* leave Im_Nr same, if possible */

   Put_image(Im_Nr);  /* redisplay */
   DrawSubWindow();
   DrawTopWindow();
   XFlush(theDisp);

   return 0;
}

/*--------------------------------------------------------------------------*/

/*** add an extra image onto the end of allim ***/

void add_extra_image(newim)
     MRI_IMAGE *newim;
{
   if( N_im >= dim_allim ){
      dim_allim += INC_ALLIM ;
      allim      = realloc( allim , sizeof(MRI_IMAGE *) * dim_allim ) ;
      if( allim == NULL ){
         fprintf(stderr,"\n*** cannot allocate space for new image!\a\n") ;
         exit(-1) ;
      }
   }

   allim[N_im] = mri_to_short( 0.0 , newim ) ;
   mri_add_name( newim->name , allim[N_im] ) ;
   N_im++ ;

   Im_Nr = N_im - 1 ;

   Put_image(Im_Nr);
   DrawSubWindow();
   DrawTopWindow();

   return ;
}

/*----------------------------------------------------------------------*/
   int read_new_im()
{
   MRI_IMAGE * im ;
   int x , y ;

   if ( txtW_ON ) {
      XBell(theDisp, 100); return(2);
   }
   txtW_ON = 1;

   x = 50 + GL_DLX;
   y = 50 + GT_DLY;

   strp[0] = ASC_NUL ;
   take_file_name(theDisp, GWindow , CMap, txtGC, mfinfo, x, y, strp, 41,
                  "Enter input image name:" , 0 );
   txtW_ON = 0 ;

   if( strp[0] == '\0' ) return(1) ;

   im = mri_read_nsize( strp ) ;

   /* we have already decided on swapping     27 Aug 2004 [rickr] */

   if ( swap_bytes && im->kind == MRI_short ) {
     swap_2(MRI_SHORT_PTR(im), im->nx*im->ny*2);
   }
   else if ( swap_bytes && im->kind == MRI_float ) {
     swap_4(MRI_FLOAT_PTR(im), im->nx*im->ny*4);
   }

   if( im == NULL ) return(3) ;

   if( (im->nx != im->ny) ||
       ( (im->nx != EPX1) && (im->nx != EPX2) &&
         (im->nx != EPX3) && (im->nx != EPX4) && (im->nx != EPX5) ) ){

      fprintf(stderr,"\n*** input file %s has illegal dimensions!\n",strp) ;
      mri_free( im ) ;
      return(4) ;
   }

   add_extra_image( im ) ;
   mri_free( im ) ;

   return(0);
}

/**-------------------------------------------------------------------------**/

/*** initialize the FIM colors:
        from the command line options (in FIM_opt_* variables), OR
        from the X11 defaults database (in the fim_* variables), OR
        from the default tables (in overfim.h)
***/

void RWC_init_fim_colors()
{
   char * xdef ;
   char stst[32] ;
   int ncol = DEFAULT_FIM_COLORS ;  /* defined in overfim.h */
   int ii , thr_good ;
   XColor test_color ;
   float xx ;

   if( FIM_opt_colors > 0 ){
      ncol = FIM_opt_colors ;
   } else {
      xdef = XGetDefault(theDisp,Xdef_Name,"fim_colors") ;
      if( xdef != NULL ){
         ii = strtol( xdef , NULL , 10 ) ;
         if( ii > 0 && ii <= MAX_FIM_COLORS ){
            ncol = ii ;
         } else {
            fprintf(stderr,
                    "\n*** illegal fim_colors in X11 database: %s\n",xdef) ;
         }
      }
   }
   RWC_fim_colors = ncol ;

   thr_good = 0 ;

   /* read from X defaults or from FIM_opt_* variables */

   for( ii=1 ; ii <= ncol ; ii++ ){

      sprintf( stst , "fim_pos_%d" , ii ) ;
      if( FIM_opt_colors > 0 ){
         xdef = FIM_opt_pos[ii-1] ;
      } else {
         xdef = XGetDefault(theDisp,Xdef_Name,stst) ;
      }
      if( xdef != NULL ){
	 if( check_color(xdef) ) FIM_poscol[ii-1] = xdef ;
	 else
            fprintf(stderr,"\n*** illegal FIM color %s replaced by %s\n",
                    xdef , FIM_poscol[ii-1] ) ;
      }

      sprintf( stst , "fim_neg_%d" , ii ) ;
      if( FIM_opt_colors > 0 ){
         xdef = FIM_opt_neg[ii-1] ;
      } else {
         xdef = XGetDefault(theDisp,Xdef_Name,stst) ;
      }
      if( xdef != NULL ){
	 if( check_color(xdef) ) FIM_negcol[ii-1] = xdef ;
	 else
            fprintf(stderr,"\n*** illegal FIM color %s replaced by %s\n",
                    xdef , FIM_negcol[ii-1] ) ;
      }

      if( FIM_opt_colors > 0 ){
         RWC_fim_thresh[ncol-ii] = FIM_opt_thr[ii-1] ;
         thr_good ++ ;
      } else {
         sprintf( stst , "fim_thr_%d" , ii ) ;
         xdef = XGetDefault(theDisp,Xdef_Name,stst) ;
         if( xdef != NULL ){
            xx = strtod( xdef , NULL ) ;
            if( xx > 0.0 && xx < 1.0 ){
               RWC_fim_thresh[ncol-ii] = xx ;
               thr_good ++ ;
            } else {
               fprintf(stderr,"\n*** illegal FIM threshold %s being replaced\n",
                       xdef ) ;
            }
         }
      }  /* end if FIM_opt_colors */

   }  /* end for ii */

   /*** check if thresholds are all good ***/

   if( thr_good > 0 && thr_good < ncol ){
      fprintf(stderr,"\n*** input thresholds being replaced!\n") ;
   }

   if( thr_good < ncol ){
      float dth = 0.95 / ncol ;
      for( ii=0 ; ii < ncol ; ii++ ) RWC_fim_thresh[ii] = 0.05 + ii*dth ;
   }

   if( thr_good > 0 && thr_good < ncol ){
      for( ii=0 ; ii < ncol ; ii++ )
         fprintf(stderr,"%5.2f ",RWC_fim_thresh[ii]);
   }

   /*** now, actually put color indices into place ***/

   for( ii=0 ; ii < ncol ; ii++ ){
      int rr,gg,bb ;

      XParseColor(theDisp,CMap,FIM_poscol[ncol-ii-1],&test_color) ;
      rr = test_color.red ;
      gg = test_color.green ;
      bb = test_color.blue ;

      add_extra_color( rr,gg,bb , NUM_STD_COLORS + ii + 1) ;
      RWC_fim_colors_pos[ii] =  -(NUM_STD_COLORS + ii + 1) ;

      XParseColor(theDisp,CMap,FIM_negcol[ncol-ii-1],&test_color) ;
      rr = test_color.red ;
      gg = test_color.green ;
      bb = test_color.blue ;

      add_extra_color( rr,gg,bb , NUM_STD_COLORS + ncol + ii + 1) ;
      RWC_fim_colors_neg[ii] =  -(NUM_STD_COLORS + ncol + ii + 1) ;
   }
   return ;
}

/*-------------------------------------------------------------------------*/

/*** add a color with the given r,g,b values to the extra_colors table
     (see routine STD_colors)
***/

void add_extra_color(r, g, b, ind)
     int r;
     int g;
     int b;
     int ind;
{
   XColor any_col;
   int    ic ;
   char *cH;
   char  *a8;
   short *a16;

   ic = ind - NUM_STD_COLORS ;
   if( ic < 1 || ic > MAX_EXTRA_COLORS ){
      fprintf(stderr,"\n*** illegal call to add_extra_color()!\a\n");
      exit(-1) ;
   }

   any_col.red   = r ; any_col.green = g ; any_col.blue = b ;
   any_col.flags = DoRed | DoGreen | DoBlue;

   if ( AJ_PseudoColor ) {
      if( !XAllocColor(theDisp, CMap, &any_col) )
         FatalError ("XAllocColor problem in add_extra_color(). RWC");
      extra_color_x11[ic-1] = any_col.pixel ;
   }
   else {
      switch( bperpix ) {
         case 32:
         default:
            cH = (char *) &AJ_RGB[ind + NCOLORS - 1];
         break;
         case 24:
            a8 = (char *) AJ_RGB;
            cH = a8 + (ind + NCOLORS - 1) * 3;
         break;
         case 16:
            a16 = (short *) AJ_RGB;
            cH = (char *) &a16[ind + NCOLORS - 1];
         break;
         case 8:
            a8 = (char *) AJ_RGB;
            cH = (char *) &a8[ind + NCOLORS - 1];
         break;
      }

      AJ_rgb[0].r = (r >> 8) & 0xff;
      AJ_rgb[0].g = (g >> 8) & 0xff;
      AJ_rgb[0].b = (b >> 8) & 0xff;
      Make_RGB_lookup(AJ_rgb, cH, 1);
   }
}

/* ----------------------------------------------------------------------- */

/*** check that a character string is a valid color name ***/

int check_color( cname )
  char * cname ;
{
   XColor test_color ;

   return XParseColor(theDisp,CMap,cname,&test_color) ;
}

/* --------------- */
   int dec_indx(n)
   int n;
/* --------------- */
{
   int m;

   m = 0;
   while (n) {
      n /= 10;
      m++;
   }
   return (m);
}

/* -------------------- */
   int Get_X_Y(mx, my)
   int *mx, *my;
/* -------------------- */
{
   Window       rW, cW;
   u_int        key;
   int          x, y, rx, ry;

   if ( XQueryPointer(theDisp, theWindow, &rW, &cW,
                                &rx, &ry, &x, &y, &key) ) {
      if ( (x < eWIDE) && (y < eHIGH) ) {
         *mx = Mltx[x]/x_mag;
         *my = Mlty[y]/x_mag;
         return(1);
      }
      else
         return(0);
   }
   else
      return(0);
}

/* ----------------------- */
   void
   swap_2(arR, nr)
   char *arR;
   int nr;
/* ----------------------- */
{
   int i;
   char ctmp;

   for (i=0; i < nr; i+=2){
     ctmp     = arR[i+1];
     arR[i+1] = arR[i];
     arR[i]   = ctmp;
   }
}

/* ----------------------- */
   void
   swap_4(arR, nr)
   char *arR;
   int nr;
/* ----------------------- */
{
   int i;
   char ctmp2, ctmp3;

   for (i=0; i < nr; i+=4){
     ctmp3    = arR[i+3];
     ctmp2    = arR[i+2];
     arR[i+2] = arR[i+1];
     arR[i+3] = arR[i];
     arR[i]   = ctmp3;
     arR[i+1] = ctmp2;
   }
}

/* ----------------------------------------- */
   void
   AJ_StoreColors(Disp, cmap, mc, nc, color)
   Display   *Disp;
   Colormap  cmap;
   XColor    *mc;
   int       nc, color;
/* ----------------------------------------- */
{
   int i;
   if ( color ) {                    /* correction for inverse order AAJ */
      for (i=0; i < nc; i++) {
         AJ_rgb[nc-i-1].r = mc[i].red >> 8;
         AJ_rgb[nc-i-1].g = mc[i].green >> 8;
         AJ_rgb[nc-i-1].b = mc[i].blue >> 8;
      }
   }
   else {
      for (i=0; i < nc; i++) {
         AJ_rgb[i].r = mc[i].red >> 8;
         AJ_rgb[i].g = mc[i].green >> 8;
         AJ_rgb[i].b = mc[i].blue >> 8;
      }
   }
   Make_RGB_lookup(AJ_rgb, AJ_RGB, nc);
   if ( expImage != NULL )  Put_image(Im_Nr);
}

/* --------------------------------- create machine packed RGB values */
   void
   Make_RGB_lookup(in, out, nc)
   AJ_rgb_str *in;
   char *out;
   int nc;
/* --------------------------------- */
{
   unsigned int  r, g, b, rmask, gmask, bmask;
   int           rshift, gshift, bshift, i, *xcol;
   byte          *ip;

   rmask = theVisual->red_mask;   rshift = 7 - highbit(rmask);
   gmask = theVisual->green_mask; gshift = 7 - highbit(gmask);
   bmask = theVisual->blue_mask;  bshift = 7 - highbit(bmask);

   xcol = (int *) malloc(sizeof(int) * nc);
   for (i=0; i < nc; i++) {
      r = in[i].r;
      g = in[i].g;
      b = in[i].b;
      r = (rshift<0) ? (r<<(-rshift)) : (r>>rshift); r = r & rmask;
      g = (gshift<0) ? (g<<(-gshift)) : (g>>gshift); g = g & gmask;
      b = (bshift<0) ? (b<<(-bshift)) : (b>>bshift); b = b & bmask;
      xcol[i] = r | g | b;
   }

   ip = (byte *) out;
   switch( bperpix ) {
      case 32:
        if (border == MSBFirst)
           for( i=0 ; i < nc ; i++ ){
              *ip++ = (xcol[i]>>24) & 0xff ;
              *ip++ = (xcol[i]>>16) & 0xff ;
              *ip++ = (xcol[i]>>8)  & 0xff ;
              *ip++ =  xcol[i]      & 0xff ;
            }
        else
           for( i=0 ; i < nc ; i++ ){
              *ip++ =  xcol[i]      & 0xff ;
              *ip++ = (xcol[i]>>8)  & 0xff ;
              *ip++ = (xcol[i]>>16) & 0xff ;
              *ip++ = (xcol[i]>>24) & 0xff ;
            }
      break ;

      case 24:
        if (border == MSBFirst)
           for( i=0 ; i < nc ; i++ ){
             *ip++ = (xcol[i]>>16) & 0xff ;
             *ip++ = (xcol[i]>>8)  & 0xff ;
             *ip++ =  xcol[i]      & 0xff ;
           }
        else
           for( i=0 ; i < nc ; i++ ){
             *ip++ =  xcol[i]      & 0xff ;
             *ip++ = (xcol[i]>>8)  & 0xff ;
             *ip++ = (xcol[i]>>16) & 0xff ;
           }
      break ;

      case 16:
        if (border == MSBFirst)
           for( i=0 ; i < nc ; i++ ){
             *ip++ = (xcol[i]>>8)  & 0xff ;
             *ip++ =  xcol[i]      & 0xff ;
           }
        else
           for( i=0 ; i < nc ; i++ ){
             *ip++ =  xcol[i]      & 0xff ;
             *ip++ = (xcol[i]>>8)  & 0xff ;
           }
      break ;

      case 8:
           for( i=0 ; i < nc ; i++ )
              *ip++ = xcol[i] & 0xff ;
      break ;
   }
}

/* ---------------------------------- */
   void
   AJ_make_STDcol(mc, nc)
   struct S_16_c  *mc;
   int    nc;
/* ---------------------------------- */
{
   int i;
   char *cH;
   unsigned char *a8;
   short *a16;
 
   switch( bperpix ) {
      case 32:
      default:
         cH = (char *) &AJ_RGB[NCOLORS];
      break;
      case 24:
         a8 = (unsigned char *) AJ_RGB;
         cH = (char *) a8 + NCOLORS * 3;
      break;
      case 16:
         a16 = (short *) AJ_RGB;
         cH = (char *) &a16[NCOLORS];
      break;
      case 8:
         a8 = (unsigned char *) AJ_RGB;
         cH = (char *) &a8[NCOLORS];
      break;
   }
  
   for (i=0; i < nc; i++) {
      AJ_rgb[i].r = mc[i].red >> 8;
      AJ_rgb[i].g = mc[i].green >> 8;
      AJ_rgb[i].b = mc[i].blue >> 8;
   }
   Make_RGB_lookup(AJ_rgb, cH, nc);
}

/* ------------- */
   int
   AJ_init_RGB()
/* ------------- */
{
   int i;
   XImage *tstImage;
   tstImage = XCreateImage(theDisp, theVisual, Planes, ZPixmap,
                           0, NULL, 16, 16, 32, 0);
   if ( tstImage == NULL ) return 1;
   bperpix = tstImage->bits_per_pixel;
   border  = tstImage->byte_order;
   XDestroyImage(tstImage);
  /* AJ our order for RGB */
   for (i=0; i < (NCOLORS); i++) pixels[i] = i;
   for (i=0; i < (NUM_STD_COLORS + MAX_EXTRA_COLORS); i++)
      STD_indx[i] = NCOLORS + i;

   return 0;
}

/*------------------------------------------------------------------------
  Returns position of highest set bit in 'ul' as an integer (0-31),
  or returns -1 if no bit is set.
--------------------------------------------------------------------------*/
/* ------------------------- */
   static int
   highbit(unsigned int ul)
/* ------------------------- */
{
  int i;  unsigned int hb;

  hb = 0x80;  hb = hb << 24;   /* hb = 0x80000000UL */
  for (i=31; ((ul & hb) == 0) && i>=0;  i--, ul<<=1);
  return i;
}
