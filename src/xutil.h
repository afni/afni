/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _MCW_XUTIL_HEADER_
#define _MCW_XUTIL_HEADER_

#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/cursorfont.h>
#include <Xm/XmAll.h>

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "machdep.h"

#include "mcw_malloc.h"
#include "afni_environ.h"

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

#ifndef MAX
#   define MAX(a,b) (((a)<(b)) ? (b) : (a))
#   define MIN(a,b) (((a)>(b)) ? (b) : (a))
#endif

#ifndef myXtFree
#define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

#ifndef myXtNew
#define myXtNew(type) ((type *) XtCalloc(1,(unsigned) sizeof(type)))
#endif

#define SENSITIZE(w,sen)                         \
   do{ Boolean fred = (Boolean) sen ;            \
       if(w != NULL){                            \
          int exp = (XtIsSensitive(w) != fred) ; \
          XtSetSensitive(w,fred) ;               \
          if(exp) MCW_expose_widget(w) ; break ; } } while(0)

#define SAVEUNDERIZE(w) XtVaSetValues((w),XmNsaveUnder,True,NULL) /* 27 Feb 2001 */

#define   TEAROFFIZE(w) XtVaSetValues((w),XmNtearOffModel,XmTEAR_OFF_ENABLED ,NULL)
#define UNTEAROFFIZE(w) XtVaSetValues((w),XmNtearOffModel,XmTEAR_OFF_DISABLED,NULL)

extern void MCW_expose_widget( Widget ) ;
extern void MCW_invert_widget( Widget ) ;
extern void MCW_set_widget_bg( Widget , char * , Pixel ) ;
extern void MCW_set_widget_fg( Widget , char * ) ;

extern Colormap MCW_get_colormap( Widget ) ;            /* 01 Sep 1998 */
extern void     MCW_set_colormap( Widget , Colormap ) ; /* 14 Sep 1998 */
extern int      MCW_get_depth( Widget ) ;
extern Visual * MCW_get_visual( Widget ) ;

extern void MCW_set_widget_cursor( Widget,int ) ;
extern void MCW_alter_widget_cursor( Widget,int , char * , char * ) ;

extern void RWC_sleep( int ) ;  /* 16 Aug 2002 */

#define WAIT_for_window(w)                                 \
 do{ XSync( XtDisplay(w) , False ) ;                        \
     while( XtWindow(w) == (Window) NULL ) ; /* spin */      \
     XSync( XtDisplay(w) , False ) ;                          \
     RWC_sleep(3) ;                                            \
 } while(0)

#define POPUP_cursorize(w)                                        \
 do{ if( (w) != (Widget)NULL && XtWindow(w) != (Window)NULL )      \
      MCW_alter_widget_cursor( (w), -XC_left_ptr,"yellow","blue" ); \
 } while(0)

#define NORMAL_cursorize(w)                                            \
 do{ if( (w) != (Widget)NULL && XtWindow(w) != (Window)NULL )           \
   MCW_alter_widget_cursor( (w), -XC_top_left_arrow,"#ffb700","blue" ) ; \
 } while(0)

#define WATCH_cursorize(w)                                     \
 do{ if( (w) != (Widget)NULL && XtWindow(w) != (Window)NULL )  \
      MCW_set_widget_cursor( (w) , -XC_watch ) ;               \
 } while(0)

#define HAND_cursorize(w)                                      \
 do{ if( (w) != (Widget)NULL && XtWindow(w) != (Window)NULL )  \
      MCW_set_widget_cursor( (w) , -XC_hand2 ) ;               \
 } while(0)

#define PENCIL_cursorize(w)                                    \
 do{ if( (w) != (Widget)NULL && XtWindow(w) != (Window)NULL )  \
      MCW_set_widget_cursor( (w) , -XC_pencil ) ;              \
 } while(0)

#define CROSSHAIR_cursorize(w)                                 \
 do{ if( (w) != (Widget)NULL && XtWindow(w) != (Window)NULL )  \
      MCW_set_widget_cursor( (w) , -XC_crosshair ) ;           \
 } while(0)

extern void MCW_register_hint( Widget , char * ) ;
extern void MCW_reghint_children( Widget , char * ) ;
extern void MCW_hint_toggle(void) ;

extern void MCW_disable_help(void) ; /* 02 Aug 1999 */
extern void MCW_enable_help (void) ;

extern void MCW_click_help_CB( Widget , XtPointer , XtPointer ) ;
extern void MCW_register_help( Widget , char * ) ;
extern void MCW_reghelp_children( Widget , char * ) ;
extern void MCW_help_CB( Widget , XtPointer , XtPointer ) ;
extern void MCW_unhelp_CB( Widget , XtPointer , XtPointer ) ;

extern void MCW_unregister_help( Widget ) ;        /* 24 Apr 2001 */
extern void MCW_unregister_hint( Widget ) ;        /* 11 Jul 2001 */

extern void MCW_set_widget_label( Widget , char * ) ;
extern void MCW_widget_geom( Widget , int * , int * , int * , int * ) ;
extern void MCW_discard_events( Widget , int ) ;
extern void MCW_discard_events_all( Widget , int ) ;

#if 0
extern void MCW_set_widget_label_tagged( Widget , char * , char *) ;
#endif

typedef struct {
      char         * label ;        /* label for button       */
      XtCallbackProc func_CB ;      /* callback procedure     */
      XtPointer      data ;         /* data for func_CB       */
      char         * help_text ;    /* for MCW_register_help  */
      char         * hint_text ;    /* for MCW_register_hint  */
      int            make_red ;     /* nonzero for red button */

      XtPointer parent , aux ;
} MCW_action_item ;

extern char * MCW_hotcolor(Widget w) ; /* 01 Nov 1999 */

extern Widget MCW_action_area( Widget , MCW_action_item * , int ) ;

#define MCW_CALLER_KILL 1
#define MCW_USER_KILL   2
#define MCW_TIMER_KILL  (1<<10)
#define MCW_QUICK_KILL  (1<<11)

extern Widget MCW_popup_message( Widget , char * , int ) ;
extern void MCW_message_CB( Widget , XtPointer , XtPointer ) ;
extern void MCW_message_timer_CB( XtPointer , XtIntervalId * ) ;

extern void MCW_message_alter( Widget , char * ) ;  /* 10 Jul 2001 */

#define MCW_nofile    0
#define MCW_readonly  1
#define MCW_readwrite 2

extern int MCW_filetype( char * ) ;

#if 0
extern Boolean MCW_isitmwm( Widget ) ;
#else
#define MCW_isitmwm(ww) (!AFNI_noenv("AFNI_X11_REDECORATE"))
#endif

#define METER_TOP       1
#define METER_TOP_WIDE  2
#define METER_BOT       3
#define METER_BOT_WIDE  4

extern Widget MCW_popup_meter( Widget , int ) ;
extern void MCW_popdown_meter( Widget ) ;
extern void MCW_set_meter( Widget , int ) ;

extern int MCW_widget_visible( Widget w ) ;  /* 03 Jan 1999 */

extern char * RWC_getname( Display * , char * ) ; /* 04 Jun 1999 */

/*------------------------------------------------------------------------*/

#define TEXT_READONLY  1
#define TEXT_EDITABLE  2

#ifndef VOID_FUNC
#define VOID_FUNC
typedef void void_func() ;
#endif

typedef struct {
   Widget wshell , wtop , wactar , wscroll , wtext ;
   void_func *kill_func ;
   XtPointer  kill_data ;
   int shell_width , shell_height ;
} MCW_textwin ;

extern MCW_textwin * new_MCW_textwin( Widget, char *, int ) ;
extern void MCW_textwin_CB( Widget , XtPointer , XtPointer ) ;
extern void MCW_textwinkill_CB( Widget , XtPointer , XtPointer ) ;
extern void MCW_textwin_setbig( int b ) ; /* 29 Apr 2009 */

extern void MCW_textwin_alter( MCW_textwin * , char * ) ; /* 10 Jul 2001 */
extern MCW_textwin * new_MCW_textwin_2001(Widget,char *,int,void_func *,XtPointer);

extern void RWC_visibilize_widget( Widget ) ;  /* 09 Nov 1999 */

#define RWC_visibilize RWC_visibilize_widget   /* 27 Sep 2000: sometimes I forget */

extern void RWC_xineramize( Display *,         /* 27 Sep 2000 */
                            int,int,int,int, int *, int * ) ;

extern void RWC_visibilize_CB( Widget , XtPointer , XtPointer ) ; /* 27 Sep 2000 */

#define VISIBILIZE_WHEN_MAPPED(w) \
  XtAddCallback(w,XmNmapCallback,RWC_visibilize_CB,NULL)

extern void MCW_manage_widgets  ( Widget * , int ) ; /* 24 Apr 2001 */
extern void MCW_unmanage_widgets( Widget * , int ) ;

void RWC_destroy_nullify       ( Widget, void ** ) ; /* 31 Jul 2001 */
void RWC_destroy_nullify_cancel( Widget, void ** ) ;

#define NULLIFY_ON_DESTROY(pp,ww)        RWC_destroy_nullify((ww),(void **)&(pp))
#define CANCEL_NULLIFY_ON_DESTROY(pp,ww) RWC_destroy_nullify_cancel((ww),(void **)&(pp))

/* invert the managed status of a widget */

#define INVERT_manage(w)                                 \
 do{ if( XtIsManaged(w) ) XtUnmanageChild(w);            \
     else                 XtManageChild(w); } while(0) /* 21 Sep 2001 */

extern void RWC_drag_rectangle( Widget, int,int, int *,int * ) ; /* 12 Jun 2002 */

extern void RWC_XtPopdown( Widget ) ; /* 30 Jun 2003 */

extern void AFNI_speak( char *string , int nofork ) ;   /* 24 Nov 2003 */
extern void AFNI_speak_setvoice( char *vvv ) ;

#ifdef DONT_USE_XTDESTROY  /** bug fix for some stupid X11 distributions **/
# undef  XtDestroyWidget
# define XtDestroyWidget XtUnrealizeWidget
#endif

#ifdef  __cplusplus
}
#endif

#endif /* _MCW_XUTIL_HEADER_ */
