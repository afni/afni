#ifndef _MCW_BBOX_HEADER_
#define _MCW_BBOX_HEADER_

#include <Xm/XmAll.h>

#undef USE_TEXT_WIDGET

#ifdef USE_TEXT_WIDGET
#  define TEXT_CLASS xmTextWidgetClass
#  define TEXT_GET   XmTextGetString
#  define TEXT_SET   XmTextSetString
#else
#  define TEXT_CLASS xmTextFieldWidgetClass
#  define TEXT_GET   XmTextFieldGetString
#  define TEXT_SET   XmTextFieldSetString
#endif

#define LIST_MAX    25
#define LIST_MAXMAX 29

#include <stdio.h>
#include <string.h>
#include <math.h>

#include "display.h"
#include "xutil.h"
#include "mrilib.h"

#ifndef LABEL_ARG
#define LABEL_ARG(str) \
  XtVaTypedArg , XmNlabelString , XmRString , (str) , strlen(str)+1
#endif

/*---------------------------------------------------------------------------------*/

#define MCW_BB_noframe 0
#define MCW_BB_frame   1

#define MCW_BB_check      1
#define MCW_BB_radio_one  2
#define MCW_BB_radio_zero 3

#define MCW_MAX_BB 16

typedef struct {
          Widget wframe , wrowcol ; /* Frame (if any) & RowColumn holder */
          Widget wtop ;             /* topmost widget (=wframe or wrowcol) */
          int    nbut ;             /* number of buttons */
          Widget wbut[MCW_MAX_BB] ; /* array of ToggleButtonWidget */
          int    value ;            /* OR-ed mask of set buttons */

          XtPointer parent , aux ;
} MCW_bbox ;

/*** prototypes ***/

extern MCW_bbox * new_MCW_bbox( Widget , int , char * lab[] , int , int ,
                                XtCallbackProc , XtPointer ) ;

extern void MCW_set_bbox( MCW_bbox * , int ) ;
extern int  MCW_val_bbox( MCW_bbox * ) ;
extern void MCW_bbox_hints( MCW_bbox * , int , char ** ) ;

/*---------------------------------------------------------------------------------*/
/***--- for arrowval ---***/

#ifndef VOID_FUNC
#define VOID_FUNC
typedef void void_func() ;
#endif

typedef void gen_func() ;   /* generic functions */
typedef char * str_func() ;

typedef struct {
      Widget wrowcol ,                   /* Container */
             wlabel , wdown,wup , wtext ;/* left-to-right widgets */

      Widget wmenu ;                     /* for optmenu style only! */
      int block_assign_actions ;

      int ival , imin,imax , decimals ;  /* value, min & max, decim shift */
      float fval , fmin,fmax ;           /* float values (includes decim) */

      char * sval ;        /* current string value from TextField */

      int old_ival ;       /* values just before the last change */
      float old_fval ;
      char  * old_sval ;

      XtIntervalId timer_id ; /* id from XtAppAddTimeOut */

      gen_func * dval_CB ; /* non-NULL=function to call when value changes */
      XtPointer dval_data ;   /* data for call */

      str_func * text_CB ; /* if non-NULL, function to call to make text */
      XtPointer  text_data ;

      int incr , delay , fastdelay  ;  /* for the timer callback */
      int allow_wrap ;

      XEvent xev ;                     /* copy of event in callback */

      XtPointer parent , aux ;

      float fstep ;  /* 16 Feb 1999 */
} MCW_arrowval ;

/* 08 Mar 1999: this macro is defined
                because I forget to free the string values (sval) */

#define FREE_AV(av) do{ if( (av) != NULL ){                              \
                           XtFree((av)->sval) ; XtFree((av)->old_sval) ; \
                           XtFree((char*)(av)) ; (av) = NULL ;           \
                        }} while(0)

#define MCW_AV_downup    1
#define MCW_AV_leftright 2
#define MCW_AV_optmenu   3
#define MCW_AV_notext    11
#define MCW_AV_editext   12
#define MCW_AV_edittext  12
#define MCW_AV_readtext  13
#define MCW_AV_noactext  14     /* Feb 1999 */

#define MCW_AV_longdelay  1000
#define MCW_AV_shortdelay 111

#define AV_NCOL           9     /* # columns visible */
#define AV_MAXLEN         120   /* Feb 1999 */

#define AVOPT_columnize(av,nc)                                 \
  do{ if( (av)!= NULL && (av)->wmenu != NULL && (nc) > 0 )     \
        XtVaSetValues( (av)->wmenu, XmNpacking,XmPACK_COLUMN , \
                        XmNnumColumns,(nc), NULL ) ; } while(0)

extern MCW_arrowval * new_MCW_optmenu( Widget , char * ,
                                       int,int,int,int,
                                       gen_func * , XtPointer ,
                                       str_func * , XtPointer ) ;

extern void refit_MCW_optmenu( MCW_arrowval * ,
                               int,int,int,int,
                               str_func * , XtPointer ) ;

extern MCW_arrowval * new_MCW_colormenu( Widget , char * label , MCW_DC * ,
                                         int , int , int ,
                                         gen_func * , XtPointer ) ;

extern MCW_arrowval * new_MCW_arrowval( Widget , char * ,
                                        int,int,int,int,int,int ,
                                        gen_func * , XtPointer ,
                                        str_func * , XtPointer ) ;

extern void AV_press_CB( Widget , XtPointer , XtPointer ) ;
extern void AVOPT_press_CB( Widget , XtPointer , XtPointer ) ;
extern void AV_timer_CB( XtPointer , XtIntervalId * ) ;
extern void AV_assign_ival( MCW_arrowval * , int ) ;
extern void AV_assign_fval( MCW_arrowval * , float ) ;
extern void AV_textact_CB( Widget , XtPointer , XtPointer ) ;

extern void AV_leave_EV( Widget , XtPointer , XEvent * , Boolean * ) ;

char * AV_default_text_CB( MCW_arrowval * , XtPointer ) ;

extern void AV_fval_to_char( float , char * ) ;

extern char * MCW_av_substring_CB( MCW_arrowval * , XtPointer ) ;

/* macro to shift a value so many decimal points to the left */

#define AV_SHIFT_VAL(decim,sv)                                 \
   { int iqqq ;                                                \
     if( (decim) > 0 )                                         \
        for( iqqq=0 ; iqqq < (decim) ; iqqq++ ) (sv) *= 0.10 ; \
     else if( (decim) < 0 )                                    \
        for( iqqq=0 ; iqqq > (decim) ; iqqq-- ) (sv) *= 10.0 ; \
   }

/* macro to set sensitivity for an arrowval
   (needed because Motif 1.2 fails to draw correctly
    when simply using XtSetSensitive on av->wrowcol ) */

#define AV_SENSITIZE(av,sss)                                    \
   do{ Boolean sen = (Boolean) sss ;                            \
       if( av != NULL ) {                                       \
      int exp = (XtIsSensitive(av->wrowcol) != sen) ;           \
      if( av->wlabel != NULL ) XtSetSensitive(av->wlabel ,sen); \
      if( av->wup    != NULL ) XtSetSensitive(av->wup    ,sen); \
      if( av->wdown  != NULL ) XtSetSensitive(av->wdown  ,sen); \
      if( av->wtext  != NULL ) XtSetSensitive(av->wtext  ,sen); \
                               XtSetSensitive(av->wrowcol,sen); \
      if( exp ) MCW_expose_widget(av->wrowcol) ;                \
   } } while(0)

/*---------------------------------------------------------------------------------*/

#define POPDOWN_ovcolor_chooser    MCW_choose_ovcolor(NULL,NULL,0,NULL,NULL)
#define POPDOWN_string_chooser     MCW_choose_string(NULL,NULL,NULL,NULL,NULL)
#define POPDOWN_strlist_chooser    MCW_choose_strlist(NULL,NULL,0,0,NULL,NULL,NULL)
#define POPDOWN_integer_chooser    MCW_choose_integer(NULL,NULL,0,0,0,NULL,NULL)
#define POPDOWN_timeseries_chooser MCW_choose_timeseries(NULL,NULL,NULL,0,NULL,NULL)

extern void MCW_list_mode_CB( MCW_arrowval * , XtPointer ) ;

extern char * MCW_DC_ovcolor_text( MCW_arrowval * , MCW_DC * ) ;
extern void   MCW_choose_ovcolor( Widget , MCW_DC * , int , gen_func * , XtPointer ) ;
extern void   MCW_choose_CB( Widget , XtPointer , XtPointer ) ;

extern void   MCW_destroy_chooser_CB( Widget , XtPointer , XtPointer ) ;
extern void   MCW_kill_chooser_CB   ( Widget , XtPointer , XtPointer ) ;

extern void   MCW_choose_integer( Widget , char * ,
                           int,int,int , gen_func * , XtPointer ) ;

extern void   MCW_choose_string( Widget , char * , char * , gen_func * , XtPointer ) ;

extern void   MCW_choose_strlist( Widget , char * , int , int ,
                                  char * strlist[] , gen_func * , XtPointer ) ;

extern void   MCW_choose_multi_strlist( Widget , char * , int ,
                                        int , int * , char * strlist[] ,
                                        gen_func * , XtPointer  ) ;

extern void   MCW_choose_timeseries( Widget , char * , MRI_IMARR * ,
                                     int , gen_func * , XtPointer ) ;

typedef struct {
      int            ctype ;   /* choice type */
      MCW_DC       * dc ;      /* display context */
      Widget         wpop ;    /* popup widget */
      Widget         wcaller ; /* widget that made the popup */
      MCW_arrowval * av ;      /* arrowval making the choices */
      Widget         wchoice ; /* widget making the choice */
      gen_func     * sel_CB ;  /* user callback */
      XtPointer    * sel_cd ;  /* user callback data */
      MRI_IMARR    * tsarr ;   /* array of timeseries to choose from */

      XtPointer parent , aux ;
} MCW_choose_data ;

#define mcwCT_ovcolor    701
#define mcwCT_integer    702
#define mcwCT_string     703
#define mcwCT_timeseries 707

#define mcwCT_single_mode 222
#define mcwCT_multi_mode  223

typedef struct {
      int         reason ;  /* reason for callback */
      XEvent *    event ;   /* event, most likely NULL */
      int         ival ;    /* chosen value */
      float       fval ;    /* chosen value */
      char *      cval ;    /* chosen value */
      int         nilist , * ilist ;  /* many chosen values */
      MRI_IMAGE * imval ;   /* chosen value for timeseries */

      XtPointer parent , aux ;
} MCW_choose_cbs ;

#define mcwCR_ovcolor    201
#define mcwCR_integer    202
#define mcwCR_string     203
#define mcwCR_timeseries 207

/*---------------------------------------------------------------------------------*/
/*---- arrowpad stuff ----*/

#define AP_DOWN  0
#define AP_UP    1
#define AP_LEFT  2
#define AP_RIGHT 3
#define AP_MID   4

#define AP_FBASE 3

typedef struct {
      int atype , atop , abottom , aleft , aright ;
} AP_define ;

static AP_define AP_but_def[5] = {
  { XmARROW_DOWN , 2 , 3 , 1 , 2 } ,
  { XmARROW_UP   , 0 , 1 , 1 , 2 } ,
  { XmARROW_LEFT , 1 , 2 , 0 , 1 } ,
  { XmARROW_RIGHT, 1 , 2 , 2 , 3 } ,
  { 0            , 1 , 2 , 1 , 2 }
} ;

#define AP_MAXCOUNT 5000

typedef struct {
      Widget wform ,            /* Containing Form widget */
             wbut[5] ;          /* 4 arrowbuttons and a pushbutton */

      XtIntervalId timer_id ;   /* id from XtAppAddTimeOut */

      int which_pressed ;

      gen_func * action_CB ;    /* non-NULL=function to call on changes */
      XtPointer action_data ;   /* data for call */

      int delay , fastdelay  ;  /* for the timer callback */
      int count ;

      XEvent xev ;              /* copy of event from callback */

      XtPointer parent , aux ;
} MCW_arrowpad ;

extern MCW_arrowpad * new_MCW_arrowpad( Widget , gen_func * , XtPointer ) ;

extern void AP_press_CB( Widget , XtPointer , XtPointer ) ;
extern void AP_timer_CB( XtPointer , XtIntervalId * ) ;

#endif /* _MCW_BBOX_HEADER_ */
