/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

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
#include "coxplot.h"
#include "niml.h"

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

#ifdef  __cplusplus
extern "C" {
#endif

extern MCW_bbox * new_MCW_bbox( Widget , int , char *lab[] , int , int ,
                                XtCallbackProc , XtPointer ) ;

extern void MCW_set_bbox( MCW_bbox * , int ) ;
extern int  MCW_val_bbox( MCW_bbox * ) ;
extern void MCW_bbox_hints( MCW_bbox * , int , char ** ) ;

extern void BBOX_set_wsubtype( char *wt ) ;  /* Feb 2012 */

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
      Widget wname ;

      Widget wmenu ;                     /* for optmenu style only! */
      int block_assign_actions ;

      int ival , imin,imax , decimals ;  /* value, min & max, decim shift */
      float fval , fmin,fmax ;           /* float values (includes decim) */

      char *sval ;        /* current string value from TextField */

      int old_ival ;       /* values just before the last change */
      float old_fval ;
      char  *old_sval ;

      XtIntervalId timer_id ; /* id from XtAppAddTimeOut */

      gen_func *dval_CB ; /* non-NULL=function to call when value changes */
      XtPointer dval_data ;   /* data for call */

      str_func *text_CB ; /* if non-NULL, function to call to make text */
      XtPointer text_data ;

      int incr , delay , fastdelay  ;  /* for the timer callback */
      int allow_wrap ;

      XEvent xev ;                     /* copy of event in callback */

      XtPointer parent , aux ;

      float fstep ;                    /* 16 Feb 1999 */
      int optmenu_call_if_unchanged ;  /* 10 Oct 2007 */
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
#define MCW_AV_updown    4      /* 03 Jun 2009 */
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

extern int AV_colsize() ;  /* 11 Dec 2001 */

extern MCW_arrowval * new_MCW_optmenu( Widget , char * ,
                                       int,int,int,int,
                                       gen_func * , XtPointer ,
                                       str_func * , XtPointer ) ;

extern MCW_arrowval * new_MCW_optmenu_64fix( Widget , char * ,
                                       int,int,int,int,
                                       gen_func * , XtPointer ,
                                       str_func * , XtPointer ) ;
extern MCW_arrowval * new_MCW_optmenu_orig( Widget , char * ,
                                       int,int,int,int,
                                       gen_func * , XtPointer ,
                                       str_func * , XtPointer ) ;

extern void refit_MCW_optmenu( MCW_arrowval * ,
                               int,int,int,int,
                               str_func * , XtPointer ) ;

extern void colorize_MCW_optmenu( MCW_arrowval *av, char *cname, int ibut ) ;

extern void allow_MCW_optmenu_popup(int) ;  /* 12 Dec 2001 */

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

extern void AV_leave_EV( Widget , XtPointer , XEvent * , RwcBoolean * ) ;

char * AV_default_text_CB( MCW_arrowval * , XtPointer ) ;

extern void AV_fval_to_char( float , char * ) ;
extern char * AV_format_fval( float ) ;   /* 12 Jul 1999 */
extern char * AV_uformat_fval( float ) ;  /* 22 Jan 2003 */

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
   do{ RwcBoolean sen = (RwcBoolean) sss ;                            \
       if( av != NULL ) {                                       \
      int exp = (XtIsSensitive(av->wrowcol) != sen) ;           \
      if( av->wlabel != NULL ) XtSetSensitive(av->wlabel ,sen); \
      if( av->wup    != NULL ) XtSetSensitive(av->wup    ,sen); \
      if( av->wdown  != NULL ) XtSetSensitive(av->wdown  ,sen); \
      if( av->wtext  != NULL ) XtSetSensitive(av->wtext  ,sen); \
                               XtSetSensitive(av->wrowcol,sen); \
      if( exp ) MCW_expose_widget(av->wrowcol) ;                \
   } } while(0)

/* following 2 macros added 12 Mar 2002 */

#define AV_SENSITIZE_UP(av,sss)                                 \
   do{ if( av != NULL && av->wup != NULL )                      \
         XtSetSensitive(av->wup,sss); } while(0)

#define AV_SENSITIZE_DOWN(av,sss)                               \
   do{ if( av != NULL && av->wdown != NULL )                    \
         XtSetSensitive(av->wdown,sss); } while(0)

/*---------------------------------------------------------------------------------*/

#define MSTUF_END     0
#define MSTUF_NONE    0
#define MSTUF_INT     1
#define MSTUF_INTEGER 1
#define MSTUF_STRING  2
#define MSTUF_STRLIST 3
#define MSTUF_YESNO   4

#define POPDOWN_ovcolor_chooser    MCW_choose_ovcolor(NULL,NULL,0,NULL,NULL)
#define POPDOWN_string_chooser     MCW_choose_string(NULL,NULL,NULL,NULL,NULL)
#define POPDOWN_strlist_chooser    MCW_choose_strlist(NULL,NULL,0,0,NULL,NULL,NULL)
#define POPDOWN_integer_chooser    MCW_choose_integer(NULL,NULL,0,0,0,NULL,NULL)
#define POPDOWN_timeseries_chooser MCW_choose_timeseries(NULL,NULL,NULL,0,NULL,NULL)
#define POPDOWN_vector_chooser     MCW_choose_vector(NULL,NULL,0,NULL,NULL,NULL,NULL)
#define POPDOWN_tcsv_chooser       MCW_choose_tcsv(NULL,NULL,NULL,0,NULL,NULL)

#define POPDOWN_editable_strlist_chooser \
                                   MCW_choose_editable_strlist(NULL,NULL,NULL,0,NULL,NULL)

#define POPDOWN_stuff_chooser      MCW_choose_stuff(NULL,NULL,NULL,NULL,0)

extern void MCW_list_mode_CB( MCW_arrowval * , XtPointer ) ;

extern char * MCW_DC_ovcolor_text( MCW_arrowval * , MCW_DC * ) ;
extern void   MCW_choose_ovcolor( Widget , MCW_DC * , int , gen_func * , XtPointer ) ;
extern void   MCW_choose_CB( Widget , XtPointer , XtPointer ) ;

extern void MCW_choose_stuff( Widget wpar , char *mainlabel ,
                              gen_func *func , XtPointer func_data , ... ) ;
extern void MCW_stuff_CB( Widget w , XtPointer client_data , XtPointer call_data ) ;

extern void   MCW_destroy_chooser_CB( Widget , XtPointer , XtPointer ) ;
extern void   MCW_kill_chooser_CB   ( Widget , XtPointer , XtPointer ) ;

extern void   MCW_choose_integer( Widget , char * ,
                                  int,int,int , gen_func *, XtPointer );

extern void   MCW_choose_vector ( Widget, char *,
                                  int, char **, float *, gen_func *, XtPointer ) ;

extern MCW_arrowval ** MCW_choose_vector_avarray( int *nav ) ; /* Aug 2010 */

extern void   MCW_choose_string ( Widget, char *,
                                  char *, gen_func *, XtPointer ) ;

extern void   MCW_choose_strlist( Widget, char *, int, int,
                                  char * strlist[], gen_func *, XtPointer ) ;

extern void   MCW_choose_binary ( Widget, char *, int, char *, char *,
                                  gen_func *, XtPointer ) ; /* 10 Feb 2012 */

extern void   MCW_choose_multi_strlist( Widget, char *, int,
                                        int, int *, char * strlist[],
                                        gen_func *, XtPointer  ) ;

extern void   MCW_choose_timeseries( Widget, char *, MRI_IMARR *,
                                     int, gen_func *, XtPointer ) ;

extern void   MCW_choose_tcsv      ( Widget , char *, NI_ELARR *,
                                     int , gen_func *, XtPointer ) ;

extern void MCW_choose_editable_strlist( Widget, char *,
                                         THD_string_array *,
                                         int, gen_func *, XtPointer ) ;

extern void MCW_choose_multi_editable_strlist( Widget, char *, int,
                                               THD_string_array *,
                                               int *, gen_func *, XtPointer ) ;

extern void MCW_stradd_CB( Widget , XtPointer , XtPointer ) ;

extern void MCW_set_browse_select(int) ;  /* 21 Feb 2007 */

typedef struct {
      int            ctype ;   /* choice type */
      MCW_DC       * dc ;      /* display context */
      Widget         wpop ;    /* popup widget */
      Widget         wcaller ; /* widget that made the popup */
      MCW_arrowval * av ;      /* arrowval making the choices */
      Widget         wchoice ; /* widget making the choice */
      gen_func     * sel_CB ;  /* user callback */
      XtPointer      sel_cd ;  /* user callback data */
      MRI_IMARR    * tsarr ;   /* array of timeseries to choose from */

      XtPointer parent , aux ;

      THD_string_array * sar ; /* array of strings, for editable_strlist */
      Widget             wtf ; /* text field, for editable_strlist */

      int nvec ;               /* 19 Mar 2004: for vector chooser */
} MCW_choose_data ;

#define mcwCT_ovcolor    701
#define mcwCT_integer    702
#define mcwCT_string     703
#define mcwCT_timeseries 707
#define mcwCT_vector     708  /* 19 Mar 2004 */
#define mcwCT_tcsv       709  /* 17 Jun 2020 */

#define mcwCT_single_mode 222
#define mcwCT_multi_mode  223

/* for vector callbacks:
     ival = number of vector values
     (float *)cval = vector array   */

typedef struct {
      int         reason ;  /* reason for callback */
      XEvent *    event ;   /* event, most likely NULL */
      int         ival ;    /* chosen value */
      float       fval ;    /* chosen value */
      char *      cval ;    /* chosen value */
      int         nilist ,
                  *ilist ;  /* many chosen values */
      MRI_IMAGE * imval ;   /* chosen value for timeseries */

      XtPointer parent , aux ;
} MCW_choose_cbs ;

#define mcwCR_ovcolor    201
#define mcwCR_integer    202
#define mcwCR_string     203
#define mcwCR_timeseries 207
#define mcwCR_vector     208  /* 19 Mar 2004 */
#define mcwCR_tcsv       209  /* 17 Jun 2020 */

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

/*! toggle sensitivity of an arrowpad */

#define AP_SENSITIZE(ap,sss)                            \
   do{ RwcBoolean sen = (RwcBoolean) sss ;                    \
       if( ap != NULL ) {                               \
        int exp = (XtIsSensitive(ap->wform) != sen) ;   \
        XtSetSensitive(ap->wbut[0],sen);                \
        XtSetSensitive(ap->wbut[1],sen);                \
        XtSetSensitive(ap->wbut[2],sen);                \
        XtSetSensitive(ap->wbut[3],sen);                \
        XtSetSensitive(ap->wbut[4],sen);                \
        XtSetSensitive(ap->wform,sen);                  \
        if( exp ) MCW_expose_widget(ap->wform) ;        \
   } } while(0)

#if 0
#define AP_MANGLIZE(ap)                                   \
  do{ if( ap != NULL ){                                   \
         XtUnmanageChild( ap->wbut[4] ) ;                 \
         XtUnmanageChild( ap->wform ) ;                   \
         XtVaSetValues( ap->wform ,                       \
                        XmNfractionBase , 6 , NULL ) ;    \
         XtVaSetValues( ap->wbut[0] ,                     \
                        XmNtopPosition    , 4 ,           \
                        XmNbottomPosition , 6 ,           \
                        XmNleftPosition   , 3 ,           \
                        XmNrightPosition  , 5 , NULL ) ;  \
         XtVaSetValues( ap->wbut[1] ,                     \
                        XmNtopPosition    , 0 ,           \
                        XmNbottomPosition , 2 ,           \
                        XmNleftPosition   , 3 ,           \
                        XmNrightPosition  , 5 , NULL ) ;  \
         XtVaSetValues( ap->wbut[2] ,                     \
                        XmNtopPosition    , 2 ,           \
                        XmNbottomPosition , 4 ,           \
                        XmNleftPosition   , 2 ,           \
                        XmNrightPosition  , 4 , NULL ) ;  \
         XtVaSetValues( ap->wbut[3] ,                     \
                        XmNtopPosition    , 2 ,           \
                        XmNbottomPosition , 4 ,           \
                        XmNleftPosition   , 4 ,           \
                        XmNrightPosition  , 6 , NULL ) ;  \
         XtManageChild( ap->wform ) ;                     \
  } } while(0)
#endif

#ifdef  __cplusplus
}
#endif

#endif /* _MCW_BBOX_HEADER_ */
