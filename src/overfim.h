#ifndef FD2_OVER_FIM
#define FD2_OVER_FIM

#ifdef EXT
#  undef EXT
#endif

#ifdef INIT
#  undef INIT
#endif

#ifdef MAIN
#   define EXT
#   define INIT(x) x
#else
#   define EXT extern
#   define INIT(x) 
#endif

EXT int RWC_do_overfim INIT(= 0) ;    /* flag to do fim overlay */

/*** stuff for overlaying color fim2 results onto FD images ***/

#define RWC_OVFLAG -12345    /* hopefully, won't be used by any real pixel */

EXT int RWC_nxim , RWC_nyim ;             /* dimensions of overlay */

EXT short * RWC_imover INIT(= NULL) ;    /* data array for overlaying */

extern int     RWC_short_overlay() ;
extern short * RWC_create_overlay() ;

/*** actual color stuff ***/

#define MAX_FIM_COLORS 9

#ifdef RWCOX_LINUX
#  define DEFAULT_FIM_COLORS 1
#else
#  define DEFAULT_FIM_COLORS 3
#endif

#ifdef MAIN
   char * FIM_poscol[MAX_FIM_COLORS] = { "yellow" ,   /* default colors */
					 "orange" ,
					 "red"    ,
					 "red"    ,
					 "red2"   ,
					 "red2"   ,
					 "red2"   ,
					 "red3"   ,
					 "red3"     } ;

   char * FIM_negcol[MAX_FIM_COLORS] = { "cyan"       ,
					 "dodgerblue" ,
					 "blue"       ,
					 "blue"       ,
					 "blue2"      ,
					 "blue2"      ,
					 "blue2"      ,
					 "blue3"      ,
					 "blue3"       } ;
#endif

EXT int RWC_fim_colors INIT(= 0) ;

EXT float RWC_fim_thresh[MAX_FIM_COLORS] ;
EXT short RWC_fim_colors_pos[MAX_FIM_COLORS] ;
EXT short RWC_fim_colors_neg[MAX_FIM_COLORS] ;

#define MAX_EXTRA_COLORS 32  /* at least 2*MAX_FIM_COLORS! */

EXT short extra_color_x11[MAX_EXTRA_COLORS] ;

EXT int EXTRA_STD_colors() ;
EXT void add_extra_color() ;
EXT void RWC_init_fim_colors() ;

/*** fim2 stuff ***/

#define REF_FLOAT_SINGLE   /* force pcor routines to use single floats */
#define VOX_SHORT          /* force pcor routines to accept short data */
#include "pcor.h"
#include "ts.h"

#define MAX_NUMORT 9
#define MAX_POLORT 3

EXT int RWC_numort INIT(= 0) , RWC_polort INIT(= 0) ;
EXT float RWC_pcthresh INIT(= 0.5) ;

EXT references * RWC_refs   INIT(= NULL) ;
EXT voxel_corr * RWC_voxcor INIT(= NULL) ;

EXT time_series * RWC_ideal INIT(= NULL) ;
EXT time_series * RWC_ort[MAX_NUMORT] ;

#ifndef MIN
#define MIN(x,y) (((x)<(y)) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y) (((x)>(y)) ? (x) : (y))
#endif

#endif
