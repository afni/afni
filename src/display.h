#ifndef _MCW_DISPLAY_HEADER_
#define _MCW_DISPLAY_HEADER_

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/cursorfont.h>

#ifndef linux
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/PushB.h>
#endif

#include "mrilib.h"

/*** Macros ***/

#ifndef MAX
#   define MAX(a,b) (((a)<(b)) ? (b) : (a))
#   define MIN(a,b) (((a)>(b)) ? (b) : (a))
#endif

#ifndef myXtFree
#define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

/* these macros are to produce RGB intensities (unsigned shorts) */

#define CLIP_INTEN(i)    (((i)<256) ? (256) : ((i)>65280) ? (65280) : (i))
#define BYTE_TO_INTEN(b) (CLIP_INTEN((b)<<8))
#define INTEN_TO_BYTE(i) ((i)>>8)

#define XCOL_BRIGHTNESS(xc) (0.299*(xc).red + 0.587*(xc).green + 0.114*(xc).blue)

#define XCOL_REDNESS(xc)    (0.299*(xc).red   - MAX(0.587*(xc).green,0.114*(xc).blue ))
#define XCOL_GREENNESS(xc)  (0.587*(xc).green - MAX(0.299*(xc).red  ,0.114*(xc).blue ))
#define XCOL_BLUENESS(xc)   (0.114*(xc).blue  - MAX(0.299*(xc).red  ,0.587*(xc).green))

/* given x in [0..wx-1], map propotionally to [0..wn-1] */

#define MAP_XY(x,wx,wn) (((wn)*(x))/(wx))

/*** typedefs ***/

#define MAX_COLORS 256

typedef struct {
      XtAppContext appcontext ;    /* X and Xt stuff */
      Display *    display ;
      Screen *     screen ;
      int          screen_num ;
      Visual *     visual ;
      Colormap     colormap ;
      GC           myGC , origGC ;
      int          planes ;
      int          depth ;

      int          width , height ;       /* of the screen */

      int          ncol_im , ncol_ov ;    /* # colors we use */
      double       gamma , gamma_init ;   /* gamma factor */
      int          use_xcol_im ;          /* color in use? */

      XColor       xgry_im[MAX_COLORS] ,  /* for images */
                   xcol_im[MAX_COLORS] ,
                   xcol_ov[MAX_COLORS]  ; /* for overlays */

      Pixel        pix_im[MAX_COLORS] ,
                   pix_ov[MAX_COLORS]  ;

      Pixel        pixov_brightest,pixov_darkest,pixov_reddest,pixov_greenest,pixov_bluest;
      int             ov_brightest,   ov_darkest,   ov_reddest,   ov_greenest,   ov_bluest;

      int          xint_im[MAX_COLORS] ;  /* intensity levels for xgry_im */

      char *       name_ov[MAX_COLORS] ;  /* names of overlay colors */
      char *       label_ov[MAX_COLORS] ; /* labels for overlay colors */

      XFontStruct * myFontStruct ;

      Widget       parent_widget ;

      XtPointer    parent , aux ;
} MCW_DC ;

/* fonts to try if the defaults fail */

static char * tfont_hopefuls[] = {
                 "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1" ,
                 "-misc-fixed-medium-r-normal--13-100-100-100-c-70-iso8859-1" ,
                 "lucidasanstypewriter-10" ,
                 "7x14" , "6x13" , "fixed" ,
              NULL } ;

/*** Macro for text widths ***/

#define DC_text_width(dc,str) XTextWidth((dc)->myFontStruct,(str),strlen((str)))

/*** prototypes ***/

extern MCW_DC * MCW_new_DC( Widget, int, int, char * c[] , char * l[] , double ) ;

extern void DC_init_im_gry( MCW_DC * ) ;
extern void DC_init_im_col( MCW_DC * ) ;
extern void DC_init_ov_col( MCW_DC * ) ;

#if 0
extern Pixel RGB_byte_to_color( MCW_DC *, int,int,int ) ;
extern Pixel Name_to_color( MCW_DC * , char * ) ;
#endif

extern int DC_add_overlay_color( MCW_DC * , char * , char * ) ;

extern void load_tmp_colors( int , XColor c[] ) ;

extern void DC_palette_rotate( MCW_DC * , int ) ;
extern void DC_palette_swap( MCW_DC * ) ;

extern void DC_palette_bright( MCW_DC * , int ) ;
extern void DC_palette_squeeze( MCW_DC * , int ) ;

extern void DC_palette_restore( MCW_DC * , double ) ;

extern void DC_gray_change( MCW_DC * , int ) ;
extern void DC_color_bright( MCW_DC * , int ) ;

extern void DC_gray_contrast( MCW_DC * , int ) ;
extern void DC_color_squeeze( MCW_DC * , int ) ;

extern void DC_palette_setgray( MCW_DC * ) ;
extern void DC_palette_setcolor( MCW_DC * ) ;

extern Boolean MCW_check_iconsize( int,int,MCW_DC * ) ;

extern XColor * DCpix_to_XColor( MCW_DC * , int ) ;

extern void DC_fg_color( MCW_DC * , int ) ;
extern void DC_bg_color( MCW_DC * , int ) ;
extern void DC_fg_colortext( MCW_DC * , char * ) ;
extern void DC_linewidth( MCW_DC * , int ) ;

#endif /* _MCW_DISPLAY_HEADER_ */
