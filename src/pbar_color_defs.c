#include "afni.h"

/* All definitions here taken out of afni.h  ZSS Jan 2011 */

char * INIT_def_colovr[DEFAULT_NCOLOVR] = {
   "#ffff00" , "#ffcc00"   , "#ff9900"  , "#ff6900" , "#ff4400" , "#ff0000" ,
   "#0000ff" , "#0044ff"   , "#0069ff"  , "#0099ff" , "#00ccff" , "#00ffff" ,
   "green"   , "limegreen" , "violet"   , "hotpink" ,
   "white"   , "#dddddd"   , "#bbbbbb"  , "#010101" ,

   "#cc1033" , "#992066"   , "#663199"  , "#3341cc" ,  /* RGB cycle */
   "#0051ff" , "#0074cc"   , "#009799"  , "#00b966" ,  /* 10 Jun 2002 */
   "#00dc33" , "#00ff00"   , "#33ff00"  , "#66ff00" ,
   "#99ff00" , "#ccff00"   , "#ffff00"  , "#ffcc00" ,
   "#ff9900" , "#ff6600"   , "#ff3300"  , "#ff0000"
} ;

char * INIT_def_labovr[DEFAULT_NCOLOVR] = {
   "yellow" , "yell-oran" , "oran-yell" , "orange"   , "oran-red" , "red"   ,
   "dk-blue", "blue"      , "lt-blue1"  , "lt-blue2" , "blue-cyan", "cyan"  ,
   "green"  , "limegreen" , "violet"    , "hotpink"  ,
   "white"  , "gry-dd"    , "gry-bb"    , "black"    ,

   "rbgyr20_01" , "rbgyr20_02" , "rbgyr20_03" , "rbgyr20_04" , /* RBG cycle */
   "rbgyr20_05" , "rbgyr20_06" , "rbgyr20_07" , "rbgyr20_08" , /* 10 Jun 2002 */
   "rbgyr20_09" , "rbgyr20_10" , "rbgyr20_11" , "rbgyr20_12" ,
   "rbgyr20_13" , "rbgyr20_14" , "rbgyr20_15" , "rbgyr20_16" ,
   "rbgyr20_17" , "rbgyr20_18" , "rbgyr20_19" , "rbgyr20_20"
} ;

#define COL_yellow     1
#define COL_yell_oran  2
#define COL_oran_yell  3
#define COL_orange     4
#define COL_oran_red   5
#define COL_red        6
#define COL_dk_blue    7
#define COL_blue       8
#define COL_lt_blue1   9
#define COL_lt_blue2   10
#define COL_blue_cyan  11
#define COL_cyan       12
#define COL_green      13
#define COL_limegreen  14
#define COL_violet     15
#define COL_hotpink    16
#define COL_white      17
#define COL_gry_dd     18
#define COL_gry_bb     19
#define COL_black      20
#define COL_rbgyr20_01 21
#define COL_rbgyr20_02 22
#define COL_rbgyr20_03 23
#define COL_rbgyr20_04 24
#define COL_rbgyr20_05 25
#define COL_rbgyr20_06 26
#define COL_rbgyr20_07 27
#define COL_rbgyr20_08 28
#define COL_rbgyr20_09 29
#define COL_rbgyr20_10 30
#define COL_rbgyr20_11 31
#define COL_rbgyr20_12 32
#define COL_rbgyr20_13 33
#define COL_rbgyr20_14 34
#define COL_rbgyr20_15 35
#define COL_rbgyr20_16 36
#define COL_rbgyr20_17 37
#define COL_rbgyr20_18 38
#define COL_rbgyr20_19 39
#define COL_rbgyr20_20 40

/** actual colors (from defaults above, or from X11 resources) **/

char * INIT_colovr[MAX_NCOLOVR] ;
char * INIT_labovr[MAX_NCOLOVR] ;

/** misc constants **/

int INIT_ngray           = DEFAULT_NGRAY ,
    INIT_ncolovr         = DEFAULT_NCOLOVR ,
    INIT_crosshair_color = DEFAULT_CROSSHAIR_COLOR ,
    INIT_marks1_color    = DEFAULT_PRIMARY_COLOR ,
    INIT_marks2_color    = DEFAULT_SECONDARY_COLOR ,
    INIT_marks_size      = DEFAULT_MARK_SIZE ,
    INIT_marks_gap       = DEFAULT_MARK_GAP ,
    INIT_crosshair_gap   = DEFAULT_CROSSHAIR_GAP ,
    INIT_purge           = 0 ,
    INIT_posfunc         = 0 ,
    INIT_bigscroll       = 5 ,
    INIT_resam_anat      = RESAM_LINEAR_TYPE ,
    INIT_resam_func      = RESAM_NN_TYPE ,
    INIT_resam_thr       = RESAM_NN_TYPE   ;

float INIT_gamma         = DEFAULT_GAMMA ,
      INIT_resam_vox     = DEFAULT_RESAMPLE_VOX ;

int INIT_ignore           = 0 ;
int INIT_tlrc_big         = 1 ;
int INIT_montage_periodic = 1 ;
int INIT_fim_polort       = 1 ; /* 30 May 1999 */

int INIT_panes_pos  = DEFAULT_PANES_POS ,
    INIT_panes_sgn  = DEFAULT_PANES_SGN ,
    INIT_panes_hide = 0 ;


float INIT_pval_pos[NPANE_MAX+1][NPANE_MAX+1] = {
  { 0 },                                                                        /* 0 panes */
  { 1.00, 0.00 },                                                               /* 1 */
  { 1.00, 0.50,  0.00 },                                                        /* 2 */
  { 1.00, 0.67,  0.33,  0.00 },                                                 /* 3 */
  { 1.00, 0.75,  0.50,  0.25,  0.00 },                                          /* 4 */
  { 1.00, 0.80,  0.60,  0.40,  0.20,  0.00 },                                   /* 5 */
  { 1.00, 0.84,  0.67,  0.50,  0.33,  0.16,  0.00 },                            /* 6 */
  { 1.00, 0.90,  0.75,  0.60,  0.45,  0.30,  0.15,  0.00 },                     /* 7 */
  { 1.00, 0.80,  0.70,  0.60,  0.50,  0.40,  0.30,  0.15,  0.00 },              /* 8 */
  { 1.00, 0.90,  0.80,  0.70,  0.60,  0.50,  0.25,  0.15,  0.05,  0.00 },       /* 9 */
  { 1.00, 0.90,  0.80,  0.70,  0.60,  0.50,  0.40,  0.30,  0.20,  0.10,  0.00 } /*10 */
} ;

int INIT_ovin_pos[NPANE_MAX+1][NPANE_MAX+1] = {
  { 0 } ,                                    /* 0 panes */
  { 1 } ,                                    /* 1 */
  { 1 , 0 } ,                                /* 2 */
  { 1 , 6 , 0 } ,                            /* 3 */
  { 1 , 4 , 6 , 0 } ,                        /* 4 */
  { 1 , 3 , 5 , 6 , 0 } ,                    /* 5 */
  { 1 , 2 , 3 , 5 , 6 , 0 } ,                /* 6 */
  { 1 , 2 , 3 , 4 , 5 , 6 , 0 } ,            /* 7 */
  { 1 , 2 , 3 , 4 , 5 , 6 ,16 , 0 } ,        /* 8 */
  { 1 , 2 , 3 , 4 , 5 , 6 ,16 ,15 , 0 } ,    /* 9 */
  { 1 , 2 , 3 , 5 , 5 , 6 ,16 ,15 , 7 , 0 }  /*10 */
} ;

float INIT_pval_sgn[NPANE_MAX+1][NPANE_MAX+1] = {
  { 0 },                                                                        /* 0 panes */
  { 1.00,-1.00 },                                                               /* 1 */
  { 1.00, 0.00, -1.00 },                                                        /* 2 */
  { 1.00, 0.05, -0.05, -1.00 },                                                 /* 3 */
  { 1.00, 0.50,  0.00, -0.50, -1.00 },                                          /* 4 */
  { 1.00, 0.50,  0.05, -0.05, -0.50, -1.00 },                                   /* 5 */
  { 1.00, 0.66,  0.33,  0.00, -0.33, -0.66, -1.00 },                            /* 6 */
  { 1.00, 0.66,  0.33,  0.05, -0.05, -0.33, -0.66, -1.00 },                     /* 7 */
  { 1.00, 0.75,  0.50,  0.25,  0.00, -0.25, -0.50, -0.75, -1.00 },              /* 8 */
  { 1.00, 0.75,  0.50,  0.25,  0.05, -0.05, -0.25, -0.50, -0.75, -1.00 },       /* 9 */
  { 1.00, 0.80,  0.60,  0.40,  0.20,  0.00, -0.20, -0.40, -0.60, -0.80, -1.00 } /*10 */
} ;

int INIT_ovin_sgn[NPANE_MAX+1][NPANE_MAX+1] = {
  { 0 } ,
  { 1 } ,
  { 1 , 11 } ,
  { 1 , 0 , 11 } ,
  { 1 , 4 ,  8 , 11 } ,
  { 1 , 4 ,  0 ,  8 , 11 } ,
  { 1 , 3 ,  5 ,  7 ,  9 , 11 } ,
  { 1 , 3 ,  5 ,  0 ,  7 ,  9 , 11 } ,
  { 1 , 2 ,  4 ,  5 ,  8 ,  9 , 10 , 11 } ,
  { 1 , 2 ,  4 ,  5 ,  0 ,  8 ,  9 , 10 , 11 } ,
  { 1 , 2 ,  3 ,  4 ,  5 ,  7 ,  8 ,  9 , 10 , 11 }
} ;
