#ifndef _MCW_XUTIL_HEADER_
#define _MCW_XUTIL_HEADER_

#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/Protocols.h>
#include <Xm/Scale.h>

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "machdep.h"

#ifndef MAX
#   define MAX(a,b) (((a)<(b)) ? (b) : (a))
#   define MIN(a,b) (((a)>(b)) ? (b) : (a))
#endif

#ifndef myXtFree
#define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

#define SENSITIZE(w,sen)                         \
   do{ Boolean fred = (Boolean) sen ;            \
       if(w != NULL){                            \
          int exp = (XtIsSensitive(w) != fred) ; \
          XtSetSensitive(w,fred) ;               \
          if(exp) MCW_expose_widget(w) ; break ; } } while(0)

extern void MCW_expose_widget( Widget ) ;
extern void MCW_invert_widget( Widget ) ;
extern void MCW_set_widget_bg( Widget , char * , Pixel ) ;

extern void MCW_set_widget_cursor( Widget,int ) ;
extern void MCW_alter_widget_cursor( Widget,int , char * , char * ) ;

extern void MCW_register_hint( Widget , char * ) ;
extern void MCW_reghint_children( Widget , char * ) ;
extern void MCW_hint_toggle(void) ;

extern void MCW_click_help_CB( Widget , XtPointer , XtPointer ) ;
extern void MCW_register_help( Widget , char * ) ;
extern void MCW_reghelp_children( Widget , char * ) ;
extern void MCW_help_CB( Widget , XtPointer , XtPointer ) ;
extern void MCW_unhelp_CB( Widget , XtPointer , XtPointer ) ;

extern void MCW_set_widget_label( Widget , char * ) ;
extern void MCW_widget_geom( Widget , int * , int * , int * , int * ) ;
extern void MCW_discard_events( Widget , int ) ;

typedef struct {
      char         * label ;        /* label for button       */
      XtCallbackProc func_CB ;      /* callback procedure     */
      XtPointer      data ;         /* data for func_CB       */
      char         * help_text ;    /* for MCW_register_help  */
      char         * hint_text ;    /* for MCW_register_hint  */
      int            make_red ;     /* nonzero for red button */

      XtPointer parent , aux ;
} MCW_action_item ;

#define HOTCOLOR(ww,ss) \
  { char * xdef = XGetDefault(XtDisplay(ww),"AFNI","hotcolor") ; \
    (ss) = (xdef != NULL) ? (xdef) : ("red3") ; }

extern Widget MCW_action_area( Widget , MCW_action_item * , int ) ;

#define MCW_CALLER_KILL 1
#define MCW_USER_KILL   2
#define MCW_TIMER_KILL  (1<<10)

extern Widget MCW_popup_message( Widget , char * , int ) ;
extern void MCW_message_CB( Widget , XtPointer , XtPointer ) ;
extern void MCW_message_timer_CB( XtPointer , XtIntervalId * ) ;

#define MCW_nofile    0
#define MCW_readonly  1
#define MCW_readwrite 2

extern int MCW_filetype( char * ) ;

#ifndef DONT_CHECK_FOR_MWM
extern Boolean MCW_isitmwm( Widget ) ;
#else
#define MCW_isitmwm(ww) True
#endif

#define METER_TOP       1
#define METER_TOP_WIDE  2
#define METER_BOT       3
#define METER_BOT_WIDE  4

Widget MCW_popup_meter( Widget , int ) ;
void MCW_popdown_meter( Widget ) ;
void MCW_set_meter( Widget , int ) ;

#endif /* _MCW_XUTIL_HEADER_ */
