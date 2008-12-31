/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "xutil.h"
#include "afni_environ.h"
#include "debugtrace.h"    /* 12 Mar 2001 */

#undef  SYNC
#define SYNC(w) XSync(XtDisplay(w),False)

#include "Amalloc.h"
extern char * THD_find_executable( char * ) ;

/*--------------------------------------------------------------------
  force an immediate expose for the widget
----------------------------------------------------------------------*/

/****************************************
  requires the following line somewhere

#include <X11/IntrinsicP.h>

*****************************************/

void MCW_expose_widget( Widget w )
{
   XExposeEvent xev;
   Dimension ww , hh ;

                               if(   w == NULL                 ) return ;
                               if( ! XtIsRealized(w)           ) return ;
                               if( ! XtIsManaged(w)            ) return ;
                               if( ! XtIsWidget(w)             ) return ;
   xev.window  = XtWindow(w) ; if( xev.window == (Window) NULL ) return ;
   xev.type    = Expose ;
   xev.display = XtDisplay(w) ;
   xev.x       = xev.y = 0 ; SYNC(w) ;
   XtVaGetValues( w, XmNwidth, &ww, XmNheight, &hh, NULL ) ;
   if( ww <= 0 || hh <= 0 ) return ;
   xev.width   = ww ; xev.height  = hh ;
   (XtClass (w))->core_class.expose( w, (XEvent *)&xev, NULL ) ;
   XFlush( XtDisplay(w) ) ;
   return ;
}

/*--------------------------------------------------------------------
  Get the Colormap for a widget -- 01 Sep 1998
----------------------------------------------------------------------*/

Colormap MCW_get_colormap( Widget w )
{
   Colormap cmap = (Colormap) 0 ;

   if( w == NULL || ! XtIsWidget(w) ) return (Colormap) 0 ;

   SYNC(w) ;
   XtVaGetValues( w , XmNcolormap  , &cmap , NULL ) ;
   return cmap ;
}

/*--------------------------------------------------------------------*/

int MCW_get_depth( Widget w )  /* 14 Sep 1998 */
{
   int depth = 0 ;

   if( w == NULL || ! XtIsWidget(w) ) return 0 ;
   SYNC(w) ;
   XtVaGetValues( w , XmNdepth  , &depth , NULL ) ;
   return depth  ;
}

/*--------------------------------------------------------------------*/

Visual * MCW_get_visual( Widget w )  /* 14 Sep 1998 */
{
   Visual *visual = NULL ;
   Widget wpar = w ;

   if( w == NULL || ! XtIsWidget(w) ) return NULL ;
   SYNC(w) ;

   while( XtParent(wpar) != NULL ) wpar = XtParent(wpar) ;  /* find top */

   XtVaGetValues( wpar , XmNvisual , &visual , NULL ) ;
   return visual ;
}

/*--------------------------------------------------------------------
  Set the Colormap for a widget -- 14 Sep 1998
  (Will only work if widget is not yet realized)
----------------------------------------------------------------------*/

void MCW_set_colormap( Widget w , Colormap cmap )
{
   if( w == NULL || ! XtIsWidget(w) ) return ;
   SYNC(w) ;
   XtVaSetValues( w , XmNcolormap  , cmap , NULL ) ;
   return ;
}

/*--------------------------------------------------------------------
   swap the fg and bg colors of a widget
   (the shadow colors are altered to fit the new bg)
----------------------------------------------------------------------*/

void MCW_invert_widget( Widget w )
{
   Pixel bg_pix , topsh_pix , botsh_pix , fg_pix , sel_pix ;
   Colormap cmap ;

   if( ! XtIsWidget(w) ) return ;
   SYNC(w) ;

   XtVaGetValues( w , XmNforeground , &bg_pix ,  /* foreground -> bg */
                      XmNbackground , &fg_pix ,  /* background -> fg */
                      XmNcolormap   , &cmap ,
                  NULL ) ;

   XmGetColors( XtScreen(w) , cmap , bg_pix ,
                NULL , &topsh_pix , &botsh_pix , &sel_pix ) ;

   XtVaSetValues( w ,
                    XmNtopShadowColor    , topsh_pix ,
                    XmNbottomShadowColor , botsh_pix ,
                    XmNselectColor       , sel_pix   ,
                    XmNarmColor          , sel_pix   ,
                    XmNborderColor       , fg_pix    ,
                    XmNforeground        , fg_pix    ,
                    XmNbackground        , bg_pix    ,
                  NULL ) ;

   XSync( XtDisplay(w) , False ) ;  /* make it happen NOW */
   XmUpdateDisplay( w ) ;
   return ;
}

/*---------------------------------------------------------------------
   set the background color of a widget; the other colors are
   altered to match, a la Motif;
     cname = character specification of color
     pix   = if cname == NULL, use this pixel value
-----------------------------------------------------------------------*/

void MCW_set_widget_bg( Widget w , char *cname , Pixel pix )
{
   Pixel bg_pix , topsh_pix , botsh_pix , fg_pix , sel_pix ;
   Colormap cmap ;

#if 1
   if( ! XtIsWidget(w) ) return ;
#else
   if( ! XtIsObject(w) ) return ;
#endif

   SYNC(w) ;

   if( cname != NULL && strlen(cname) > 0 ){
      XtVaSetValues( w ,
                     XtVaTypedArg , XmNbackground , XmRString ,
                                    cname , strlen(cname)+1 ,
                     NULL ) ;

      XtVaGetValues( w , XmNbackground , &bg_pix , NULL ) ;
   } else {
      bg_pix = pix ;
   }

#if 0
   XtVaGetValues( w , XmNcolormap , &cmap , NULL ) ;
   XmGetColors( XtScreen(w) , cmap , bg_pix ,
                &fg_pix , &topsh_pix , &botsh_pix , &sel_pix ) ;
   XtVaSetValues( w ,
                    XmNtopShadowColor    , topsh_pix ,
                    XmNbottomShadowColor , botsh_pix ,
                    XmNselectColor       , sel_pix   ,
                    XmNarmColor          , sel_pix   ,
                    XmNborderColor       , fg_pix    ,
                    XmNforeground        , fg_pix    ,
                    XmNbackground        , bg_pix    ,
                  NULL ) ;
   XFlush( XtDisplay(w) ) ;
#else
   XmChangeColor( w , bg_pix ) ;
#endif

   return ;
}

void MCW_set_widget_fg( Widget w , char *cname )
{
   Pixel bg_pix , topsh_pix , botsh_pix , fg_pix , sel_pix ;
   Colormap cmap ;

   if( !XtIsWidget(w) || cname == NULL || *cname == '\0' ) return ;

   SYNC(w) ;

   XtVaSetValues( w , XtVaTypedArg , XmNforeground , XmRString ,
                                     cname , strlen(cname)+1 , NULL ) ;
   return ;
}


/*-------------------------------------------------------------------*/

void MCW_set_widget_label( Widget w , char *str )
{
   XmString xstr ;
   if( w == NULL || str == NULL ) return ;
   xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( w , XmNlabelString , xstr , NULL ) ;
   XmStringFree( xstr ) ;
   MCW_expose_widget( w ) ;
   return ;
}

#if 0
void MCW_set_widget_label_tagged( Widget w , char *str , char *tag )
{
   XmString xstr ;
   if( w == NULL || str == NULL ) return ;
   xstr = XmStringCreateLtoR( str , tag ) ;
   XtVaSetValues( w , XmNlabelString , xstr , NULL ) ;
   XmStringFree( xstr ) ;
   MCW_expose_widget( w ) ;
   return ;
}
#endif

/*-----------------------------------------------------------------------*/

void MCW_widget_geom( Widget w, int *wout, int *hout, int *xout, int *yout )
{
   Dimension nx , ny ;  /* don't try to make these ints! */
   Position  xx , yy ;

   if( w == NULL ) return ;
   SYNC(w) ; RWC_sleep(1) ;

   if( XtIsRealized(w) ){
      XtVaGetValues( w , XmNwidth  , &nx , XmNheight , &ny ,
                         XmNx      , &xx , XmNy      , &yy , NULL ) ;
   } else {
      XtWidgetGeometry wg ;
      (void) XtQueryGeometry( w , NULL , &wg ) ;   /* works! */
      nx = wg.width ; ny = wg.height ;
      xx = wg.x     ; yy = wg.y      ;
   }

#define ASSIF(p,v) if( p!= NULL ) *p = v

   ASSIF(wout,nx) ; ASSIF(hout,ny) ;
   ASSIF(xout,xx) ; ASSIF(yout,yy) ;

   return ;
}

/*--------------------------------------------------------------------------*/

void MCW_discard_events( Widget w , int ev_mask )
{
   XEvent evjunk ;

   if( w == NULL || XtWindow(w) == (Window) NULL ) return ;

   XSync( XtDisplay(w) , False ) ;
   while( XCheckWindowEvent( XtDisplay(w), XtWindow(w) , ev_mask , &evjunk ) ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void MCW_discard_events_all( Widget w , int ev_mask )
{
   XEvent evjunk ;

   if( w == NULL || XtWindow(w) == (Window) NULL ) return ;

   XSync( XtDisplay(w) , False ) ;
   while( XCheckMaskEvent( XtDisplay(w), ev_mask , &evjunk ) ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

char * MCW_hotcolor(Widget w)
{
   static char *redcolor = NULL ;

   if( redcolor == NULL ){
     char *xdef = RWC_getname( (w!=NULL) ? XtDisplay(w) : NULL, "hotcolor" ) ;

     redcolor = (xdef != NULL) ? (xdef) : ("red4") ;
   }
   return redcolor ;
}

/*--------------------------------------------------------------------------
   24 Apr 2001: manage array of widgets, some of which may be NULL
----------------------------------------------------------------------------*/

void MCW_manage_widgets( Widget *war , int nar )
{
   int ii ;
   if( war == NULL ) return ;
   for( ii=0 ; ii < nar ; ii++ )
      if( war[ii] != (Widget) 0 ) XtManageChild( war[ii] ) ;
   return ;
}

void MCW_unmanage_widgets( Widget *war , int nar )
{
   int ii ;
   if( war == NULL ) return ;
   for( ii=0 ; ii < nar ; ii++ )
      if( war[ii] != (Widget) 0 ) XtUnmanageChild( war[ii] ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

#define TIG 25

Widget MCW_action_area( Widget parent, MCW_action_item *action, int num_act )
{
   Widget act_area , ww ;
   int ii ;

   if( parent == NULL ) return NULL ;  /* should not happen */

   act_area = XtVaCreateWidget( "action_area" , xmFormWidgetClass , parent ,
                                    XmNfractionBase , TIG*num_act - 1,

                                    XmNinitialResourcesPersistent , False ,
                                NULL ) ;

   for( ii=0 ; ii < num_act ; ii++ ){

      ww = XtVaCreateManagedWidget(
               action[ii].label , xmPushButtonWidgetClass , act_area ,

                  XmNleftAttachment   ,
           (ii) ? XmATTACH_POSITION : XmATTACH_FORM ,
                  XmNleftPosition     , ii*TIG ,
                  XmNtopAttachment    , XmATTACH_FORM ,
                  XmNbottomAttachment , XmATTACH_FORM ,
                  XmNrightAttachment  ,
                     (ii==num_act-1) ? XmATTACH_FORM : XmATTACH_POSITION ,

                  XmNrightPosition    , ii*TIG + (TIG-1) ,

                  XmNrecomputeSize , False ,
                  XmNtraversalOn   , True  ,
                  XmNinitialResourcesPersistent , False ,
               NULL ) ;

      XtAddCallback( ww , XmNactivateCallback ,
                     action[ii].func_CB , action[ii].data ) ;

      action[ii].data = (XtPointer) ww ;  /* Feb 1998: save widget */

      if( action[ii].help_text != NULL )
         MCW_register_help( ww , action[ii].help_text ) ;

      if( action[ii].hint_text != NULL )
         MCW_register_hint( ww , action[ii].hint_text ) ;

      if( action[ii].make_red > 0 )                  /* for some fun */
         MCW_set_widget_bg( ww , MCW_hotcolor(ww) , 0 ) ;
      else if( action[ii].make_red < 0 )             /* for no fun at all */
         XtSetSensitive( ww , False ) ;

   }

   XtManageChild( act_area ) ;
   return act_area ;
}

/*------------------------------------------------------------------
   Popup a window with a message;  return the Widget.
   The message can be of two types, signified by the 3rd argument:
      msg_type = MCW_CALLER_KILL means the caller has to kill the
                 widget for the window to go away.

               = MCW_USER_KILL means that the user can click in
                  the widget to kill it.

      msg_type may also be OR-ed with MCW_TIMER_KILL to have the
      message automatically killed after 22 seconds.
--------------------------------------------------------------------*/

Widget MCW_popup_message( Widget wparent , char *msg , int msg_type )
{
   Widget wmsg , wlab ;
   int wx,hy,xx,yy , xp,yp , scr_width,scr_height , xr,yr , xpr,ypr , lm ;
   Screen *scr ;
   XEvent ev ;

ENTRY("MCW_popup_message") ;

   if( msg == NULL || (lm=strlen(msg)) == 0 ) RETURN(NULL) ;

   if( wparent == NULL || !XtIsRealized(wparent) ){  /* 21 Aug 2007 */
     fprintf(stderr,"%s\n",msg) ; RETURN(NULL) ;
   }

   /* set position for message box based on parent and screen geometry */

   MCW_widget_geom( wparent , &wx,&hy,&xx,&yy ) ;  /* geometry of parent */

#if 1
   { Position xroot , yroot ;
     XtTranslateCoords( wparent, 0,0, &xroot,&yroot ) ; /* root coords */
     xr = (int) xroot ; yr = (int) yroot ;
   }
#else
     xr = xx ; yr = yy ;  /* the old code, essentially */
#endif

   scr        = XtScreen( wparent ) ;
   scr_width  = WidthOfScreen( scr ) ;
   scr_height = HeightOfScreen( scr ) ;

   xp = xx+8 ;  xpr = xr+8 ;
        if( xpr+50 > scr_width ){ xp -= 100 ; xpr -= 100 ; } /* too right */
   else if( xpr+10 < 0 )        { xpr = xp = 1 ; }           /* too left  */

   yp = yy+hy+8 ;  ypr = yr+hy+8 ;
        if( ypr+50 > scr_height ){ yp = yy-8 ; ypr = yr-100 ;} /* too down */
   else if( ypr+10 < 0 )         { ypr = yp = 1 ;            } /* too up   */

   /* create a popup shell with a label */

   wmsg = XtVaCreatePopupShell(
             "help" , xmDialogShellWidgetClass , wparent ,
                XmNx , xpr ,
                XmNy , ypr ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   if( MCW_isitmwm( wparent ) ){
      XtVaSetValues( wmsg ,
                        XmNmwmDecorations , MWM_DECOR_BORDER ,
                        XmNmwmFunctions   , MWM_FUNC_MOVE ,
                     NULL ) ;
   }

   switch( msg_type & (MCW_CALLER_KILL | MCW_USER_KILL) ){

      case MCW_CALLER_KILL:

         wlab = XtVaCreateManagedWidget(
                  "help" , xmLabelWidgetClass , wmsg ,
                     XtVaTypedArg,XmNlabelString,XmRString,msg,lm+1,
                     XmNalignment , XmALIGNMENT_BEGINNING ,
                     XmNinitialResourcesPersistent , False ,
                  NULL ) ;
      break ;

      default:
      case MCW_USER_KILL:{
         static int first=1 ; char *mmsg = msg ;     /* 'first' stuff  */
         if( first ){                                /* on 06 Apr 2004 */
           if( !AFNI_noenv("AFNI_CLICK_MESSAGE") ){
             mmsg = (char *) malloc(lm+99) ;
             strcpy(mmsg,msg) ;
             strcat(mmsg,"\n [---------------] "
                         "\n [ Click in Text ] "
                         "\n [ to Pop Down!! ]\n" ) ;
           }
         }

         wlab = XtVaCreateManagedWidget(
                  "help" , xmPushButtonWidgetClass , wmsg ,
                     XtVaTypedArg,XmNlabelString,XmRString,mmsg,lm+1,
                     XmNalignment , XmALIGNMENT_BEGINNING ,
                     XmNinitialResourcesPersistent , False ,
                  NULL ) ;

         if( mmsg != msg ){ free((void *)mmsg); first = 0; }

         XtAddCallback( wlab , XmNactivateCallback , MCW_message_CB , NULL ) ;
      }
      break ;
   }

   SAVEUNDERIZE(wmsg) ;           /* 27 Feb 2001 */
   XtPopup( wmsg , XtGrabNone ) ; RWC_sleep(1);

   /* now wait until the label is exposed, and make sure it appears;
      the reason for this stuff is that this routine is likely to be
      called by a long computation that won't return control to Xt   */

   WAIT_for_window(wlab) ;
   XSync( XtDisplay(wlab) , False ) ;
#if 0
   XMaskEvent( XtDisplay(wlab) , ExposureMask , &ev ) ;/* wait for expose  */
#else
   XWindowEvent( XtDisplay(wlab) , XtWindow(wlab) , ExposureMask , &ev ) ;
#endif
   XPutBackEvent( XtDisplay(wlab) , &ev ) ;            /* put expose back  */
   XSync( XtDisplay(wlab) , False ) ;
   XmUpdateDisplay( wlab ) ;                           /* show it for sure */

   /* now add the timer kill, if wanted */

#define ALLOW_TIMER_KILL
#ifdef ALLOW_TIMER_KILL
   if( (msg_type & MCW_TIMER_KILL) != 0 ){
      XtIntervalId tid ;

      tid = XtAppAddTimeOut( XtWidgetToApplicationContext( wmsg ) ,
                             22222 , MCW_message_timer_CB , wmsg   ) ;

      XtVaSetValues( wlab , XmNuserData ,  tid , NULL );/* put tid on wlab; */
   } else {                                             /* shells don't */
      XtVaSetValues( wlab , XmNuserData , 0 , NULL ) ;  /* have XmNuserData */
   }
#endif

   RWC_visibilize(wmsg) ;  /* 27 Sep 2000 */
   NORMAL_cursorize(wmsg) ;
   RETURN(wmsg) ;
}

/*-------------------------------------------------------------------------
  Alter the text in the popup message - 10 Jul 2001
---------------------------------------------------------------------------*/

void MCW_message_alter( Widget wmsg , char *msg )
{
   Widget wlab ;
   Widget *children=NULL ;
   int  num_children=0 ;
   XmString xstr ;

ENTRY("MCW_message_alter") ;

   if( wmsg == NULL || msg == NULL || msg[0] == '\0' ) EXRETURN ;

   XtVaGetValues( wmsg , XmNchildren    , &children ,
                         XmNnumChildren , &num_children , NULL ) ;
   if( num_children < 1 ) EXRETURN ;

   MCW_set_widget_label( children[0] , msg ) ;
   EXRETURN ;
}

/*-------------------------------------------------------------------------
    callback when the popup message created above is a PushButton
    (Note that w is the PushButton widget, so its parent is to be killed)
---------------------------------------------------------------------------*/

void MCW_message_CB( Widget w , XtPointer cd , XtPointer cbs )
{
#ifdef ALLOW_TIMER_KILL
   XtIntervalId tid ;

   XtVaGetValues( w , XmNuserData , &tid , NULL ) ;  /* get timer id */
   XtDestroyWidget( XtParent(w) ) ;

   if( tid > 0 ) XtRemoveTimeOut( tid ) ;  /* if a timer exists, kill it */
   RWC_sleep(1) ;
#else
   XtDestroyWidget( XtParent(w) ) ;
#endif
}

/*----------------------------------------------------*/
/*--- callback when timer expires on popup message ---*/
/*----------------------------------------------------*/

void MCW_message_timer_CB( XtPointer client_data , XtIntervalId *id )
{
   XtDestroyWidget( (Widget) client_data ) ;
   RWC_sleep(1) ;
}

/*------------------------------------------------------------------
   routines to change Widget cursors
     cur argument:  = 0 --> None
                    > 0 --> is a Cursor already allocated
                    < 0 --> -cur is a cursorfont index
--------------------------------------------------------------------*/

void MCW_set_widget_cursor( Widget w , int cur )
{
   MCW_alter_widget_cursor( w , cur , NULL,NULL ) ;
   return ;
}

/*--------------------------------------------------------------------*/

void MCW_alter_widget_cursor( Widget w, int cur, char *fgname, char *bgname )
{
   XColor fg , bg ;
   Cursor ccc ;
   Colormap cmap ;
   Display  *dis ;
   Boolean  good ;
   int ii ;

   static Cursor  cur_font[XC_num_glyphs] ;
   static Boolean first = True ;

   if( AFNI_yesenv("AFNI_DISABLE_CURSORS") ) return ; /* 21 Mar 2004 */

   if( first ){
      for( ii=0 ; ii < XC_num_glyphs ; ii++ ) cur_font[ii] = None ;
      first = False ;
   }

   if( w == NULL || !XtIsRealized(w) || XtWindow(w) == (Window)NULL ) return ;
   RWC_sleep(1) ;

   dis = XtDisplay(w) ;

   if( cur == 0 || cur <= -XC_num_glyphs ){    /* define cursor */
      ccc = None ;
   } else if( cur > 0 ){
      ccc = cur ;
   } else {
      ii = -cur ;
      if( cur_font[ii] == None ) cur_font[ii] = XCreateFontCursor( dis , ii ) ;
      ccc = cur_font[ii] ;
   }

   XDefineCursor( dis , XtWindow(w) , ccc ) ;

   if( fgname != NULL && bgname != NULL ){

      cmap = DefaultColormap( dis , DefaultScreen(dis) ) ;

      good =    XParseColor( dis , cmap , fgname , &fg )
             && XParseColor( dis , cmap , bgname , &bg ) ;

      if( good ) XRecolorCursor( dis , ccc , &fg , &bg ) ;
   }
   return ;
}

/*------------------------------------------------------------------------
   widget help routines (for "button help" buttons)
--------------------------------------------------------------------------*/

void MCW_click_help_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   Widget whelp ;
   XmAnyCallbackStruct cbs ;
   XEvent ev ;
   static Cursor cur = 0 ;
   Display *dis = XtDisplay(w) ;

   if( cur == 0 ) cur = XCreateFontCursor( dis , XC_hand2 ) ;

#ifdef USE_LOCATE  /* old version */
   whelp = XmTrackingLocate( w , cur , False ) ; /* wait for user to click */
#else
   cbs.event = &ev ;
   whelp = XmTrackingEvent( w , cur , False , cbs.event ) ;
#endif

   if( whelp != NULL &&
       XtHasCallbacks(whelp,XmNhelpCallback) == XtCallbackHasSome ){

      cbs.reason = XmCR_HELP ;
      XtCallCallbacks( whelp , XmNhelpCallback , &cbs ) ;  /* call for help */
   } else {
      XBell( dis , 100 ) ;
   }
   return ;
}

/*------------------------------------------------------------------------*/
/*------------------------------------------------------------------------*/

static int disable_helps = 0 ;                     /* 02 Aug 1999 */
void MCW_disable_help(void){ disable_helps = 1 ; }
void MCW_enable_help (void){ disable_helps = 0 ; }

#ifdef DONT_USE_HINTS
void MCW_register_hint( Widget w , char *msg ) { return ; }
void MCW_reghint_children( Widget w , char *msg ) { return ; }
void MCW_hint_toggle(void){ return ; }
void MCW_unregister_hint( Widget w ){ return ; }
#else

#include "LiteClue.h"

static Widget liteClue = NULL ;
static int clueless    = -1 ;

void MCW_hint_toggle(void)
{
#define PBIG 999999
   int period ;
   char *pdef ;

   if( liteClue == NULL ) return ;

   XtVaGetValues( liteClue , XgcNwaitPeriod , &period , NULL ) ;
   if( period < PBIG ){
      period = PBIG ;
   } else {
#if 0
      pdef = XGetDefault(XtDisplay(liteClue),"AFNI","waitperiod") ;
#else
      pdef = RWC_getname(XtDisplay(liteClue),"waitperiod") ;
#endif
      if( pdef == NULL ){
         period = 1066 ;
      } else {
         period = strtol( pdef , NULL , 10 ) ;
         if( period < 100 ) period = 1066 ;
      }
   }
   XtVaSetValues( liteClue , XgcNwaitPeriod , period , NULL ) ;
   return ;
}

/*--------------------------------------------------------------------*/

void MCW_unregister_hint( Widget w )    /* 11 Jul 2001 */
{
   if( liteClue != NULL && w != NULL )
      XcgLiteClueDeleteWidget( liteClue , w ) ;
   return ;
}

/*--------------------------------------------------------------------*/

#define RES_CONVERT( res_name, res_value) \
       XtVaTypedArg, (res_name), XmRString, (res_value), strlen(res_value) + 1

void MCW_register_hint( Widget w , char *msg )
{
   if( disable_helps ) return ;
   if( w == NULL || msg == NULL || clueless == 1 || !XtIsWidget(w) ) return ;

   if( clueless == -1 ){
      char *hh = my_getenv("AFNI_HINTS") ;
      if( hh != NULL && ( strncmp(hh,"KILL",4)==0 ||
                          strncmp(hh,"kill",4)==0 ||
                          strncmp(hh,"Kill",4)==0 ) ){
         clueless = 1 ;
         return ;
      } else {
         clueless = 0 ;
      }
   }

   /*-- Do we have to make the hint-displaying widget? --*/

   if( liteClue == NULL ){
      Widget wpar = w ;
      char *cfont ;

      while( XtParent(wpar) != NULL ) wpar = XtParent(wpar) ;  /* find top */

      cfont = XGetDefault(XtDisplay(wpar),"AFNI","cluefont") ;
      if( cfont != NULL ){
         liteClue = XtVaCreatePopupShell( "help", xcgLiteClueWidgetClass, wpar,
                                             RES_CONVERT(XtNfontSet,cfont) ,
                                          NULL);
      } else {
         liteClue = XtVaCreatePopupShell( "help", xcgLiteClueWidgetClass, wpar,
                                          NULL);
      }
      if( liteClue == NULL ) return ;

      XtVaSetValues( liteClue , XmNsaveUnder , True , NULL ) ;  /* 22 Jan 1999 */
   }

   /*-- attach the hint to the widget, if it is a widget --*/

   if( XtIsWidget(w) ) XcgLiteClueAddWidget( liteClue, w, msg, 0,0 ) ;

   return ;
}

/*--------------------------------------------------------------------*/

void MCW_reghint_children( Widget w , char *msg )
{
   Widget *children=NULL ;
   int  num_children=0 , ic ;

   if( disable_helps ) return ;
   if( w == NULL || msg == NULL || clueless == 1 || !XtIsWidget(w) ) return ;

   XtVaGetValues( w , XmNchildren    , &children ,
                      XmNnumChildren , &num_children , NULL ) ;

   MCW_register_hint( w , msg ) ;
   if( children == NULL || num_children == 0 ) return ;

   for( ic=0 ; ic < num_children ; ic++ )
      MCW_register_hint( children[ic] , msg ) ;

   return ;
}
#endif /* DONT_USE_HINTS */
/*------------------------------------------------------------------------*/
/*------------------------------------------------------------------------*/

void MCW_unregister_help( Widget w ) /* 24 Apr 2001 */
{
   XtCallbackList hc=NULL ;

   if( w == NULL ) return ;
   XtVaGetValues( w , XmNhelpCallback , &hc , NULL ) ;
   if( hc != NULL ) XtRemoveCallbacks( w , XmNhelpCallback , hc ) ;
}

/*------------------------------------------------------------------------*/

void MCW_register_help( Widget w , char *msg )
{
   if( disable_helps ) return ;
   if( w == NULL || msg == NULL ) return ;
   XtAddCallback( w , XmNhelpCallback , MCW_help_CB , msg ) ;
   return ;
}

/*--------------------------------------------------------------------*/

void MCW_reghelp_children( Widget w , char *msg )
{
   Widget *children ;
   int  num_children , ic ;

   if( disable_helps ) return ;
   if( w == NULL || msg == NULL ) return ;

   XtVaGetValues( w , XmNchildren    , &children ,
                      XmNnumChildren , &num_children , NULL ) ;

   XtAddCallback( w , XmNhelpCallback , MCW_help_CB , msg ) ;

   for( ic=0 ; ic < num_children ; ic++ )
      XtAddCallback( children[ic] , XmNhelpCallback , MCW_help_CB , msg ) ;
   return ;
}

/*------------------------------------------------------------------------*/

void MCW_help_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   char *msg          = (char *) client_data ;
   static Widget wpop = NULL , wbut ;
   Position xx,yy ;
   XmString xstr ;
   int ww,hh , sw,sh ;
   char *def ;

#ifndef USE_LOCATE
   XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data ;
#endif

   if( w == NULL ){
      if( wpop != NULL ) XtUnmapWidget(wpop) ;
      return ;
   }

   if( wpop == NULL || ! XtIsWidget(wpop) ){

      Widget wpar = w ;

      while( XtParent(wpar) != NULL ) wpar = XtParent(wpar) ;  /* find top */

      wpop = XtVaCreatePopupShell(
              "help" , xmDialogShellWidgetClass , wpar ,
                 XmNmappedWhenManaged , False ,
                 XmNallowShellResize , True ,
                 XmNdeleteResponse , XmDO_NOTHING ,
                 XmNinitialResourcesPersistent , False ,
              NULL ) ;

#if 0
      def = XGetDefault(XtDisplay(wpar),"AFNI","helpborder") ;
#else
      def = RWC_getname(XtDisplay(wpar),"helpborder") ;
#endif
      if( def != NULL && strcmp(def,"False") == 0 ){
         XtVaSetValues( wpop ,
                           XmNoverrideRedirect , True ,
                        NULL ) ;
      } else if( MCW_isitmwm(wpar) ){
         XtVaSetValues( wpop ,
                           XmNmwmDecorations , MWM_DECOR_BORDER ,
                           XmNmwmFunctions   , MWM_FUNC_MOVE ,
                        NULL ) ;
      }

      wbut = XtVaCreateManagedWidget(
                "help" , xmPushButtonWidgetClass , wpop ,
                   XmNalignment , XmALIGNMENT_BEGINNING ,
                   XmNinitialResourcesPersistent , False ,
                NULL ) ;

      XtAddCallback( wbut , XmNactivateCallback , MCW_unhelp_CB , wpop ) ;

      XmUpdateDisplay( wpar ) ;
      RWC_XtPopdown( wpop ) ;

      XmAddWMProtocolCallback(
           wpop ,
           XmInternAtom( XtDisplay(wpop) , "WM_DELETE_WINDOW" , False ) ,
           MCW_unhelp_CB , wpop ) ;

      if( ! XtIsRealized(wpar) ) return ;
   }

   if( msg == NULL || strlen(msg) == 0 ) return ; /* no popup if no message */

   xstr = XmStringCreateLtoR( msg , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( wbut , XmNlabelString , xstr , NULL ) ;
   XmStringFree( xstr ) ;

#ifndef USE_LOCATE
   if( cbs != NULL && cbs->event != NULL
                   && cbs->event->type == ButtonRelease ){
      XButtonEvent *xev = (XButtonEvent *) cbs->event ;
      xx = xev->x_root ;
      yy = xev->y_root ;
   } else
#endif
   XtTranslateCoords( w , 15,15 , &xx , &yy ) ;    /* coordinates on root */

   MCW_widget_geom( wpop , &ww,&hh , NULL,NULL ) ; /* widget width and height */
   sw = WidthOfScreen (XtScreen(wpop)) ;           /* screen width and height */
   sh = HeightOfScreen(XtScreen(wpop)) ;

   if( xx+ww+3 >= sw && ww <= sw ) xx = sw-ww ;    /* make sure is on screen */
   if( yy+hh+3 >= sh && hh <= sh ) yy = sh-hh ;

   XtVaSetValues( wpop , XmNx , (int) xx , XmNy , (int) yy , NULL ) ;
   XtPopup( wpop , XtGrabNone ) ; RWC_sleep(1);
   RWC_visibilize(wpop) ;  /* 27 Sep 2000 */
   NORMAL_cursorize(wpop) ;
   return ;
}

/*---------------------------------------------------------------------*/

void MCW_unhelp_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   Widget wpop = (Widget) client_data ;

   RWC_XtPopdown(wpop) ;
   return ;
}

/*---------------------------------------------------------------------*/

int MCW_filetype( char *fname )
{
   FILE *fff ;

   if( fname == NULL || strlen(fname) == 0 ) return MCW_nofile ;

   fff = fopen( fname , "r+" ) ;  /* open for read-update */
   fclose( fff ) ;

   if( fff != NULL ) return MCW_readwrite ;

   fff = fopen( fname , "r" ) ;   /* open for read-only */
   fclose(fff) ;

   if( fff != NULL ) return MCW_readonly ;

   return MCW_nofile ;
}

/*-------------------------------------------------------------------*/

#if 0
Boolean MCW_isitmwm( Widget w )
{
   Widget wsh ;

   if( w == NULL || ! XtIsWidget(w) ) return False ;

   wsh = w ;

   while( ! XtIsShell(wsh) ){
      wsh = XtParent(wsh) ;
      if( wsh == NULL ) return False ;
   }

#if 1
   return XmIsMotifWMRunning(wsh) ;
#else
   { int immm = -73 ;
     XtVaGetValues( wsh , XmNmwmDecorations , &immm , NULL ) ;
     if( immm == -73 ) return False ;
     else              return True ;
   }
#endif
}
#endif

/*------------------------------------------------------------------
   Popup a scale that will serve as a progress meter.
   The meter is set initially to 0, and can later be set to any
   value from 0 to 100 using MCW_set_meter().

   wparent is the widget the meter will be attached to
   position is one of
     METER_TOP      = meter on top of wparent, default width
     METER_TOP_WIDE = meter on top of parent, width of parent
     METER_BOT      = similar, but on bottom of parent
     METER_BOT_WIDE
--------------------------------------------------------------------*/

#define METER_HEIGHT 10
#define METER_WIDTH  200

Widget MCW_popup_meter( Widget wparent , int position )
{
   Widget wmsg , wscal ;
   int wx,hy,xx,yy , xp,yp , scr_width,scr_height , xr,yr , xpr,ypr , wid ;
   Screen *scr ;
   XEvent ev ;
   Position xroot , yroot ;

ENTRY("MCW_popup_meter") ;

   if( wparent == NULL || ! XtIsRealized(wparent) ) RETURN(NULL) ;

   /* set position parent and screen geometry */

   MCW_widget_geom( wparent , &wx,&hy,&xx,&yy ) ;     /* geometry of parent */
   XtTranslateCoords( wparent, 0,0, &xroot,&yroot ) ; /* root coords of parent */
   xr = (int) xroot ; yr = (int) yroot ;

   scr        = XtScreen( wparent ) ;
   scr_width  = WidthOfScreen( scr ) ;
   scr_height = HeightOfScreen( scr ) ;

   switch( position ){

      default:
      case METER_TOP:
      case METER_TOP_WIDE:
         xpr = xr ;
         ypr = yr - METER_HEIGHT-2 ;
         wid = (position==METER_TOP_WIDE) ? wx : METER_WIDTH ;
         if( ypr < 0 ) ypr = yr+hy+1 ;
      break ;

      case METER_BOT:
      case METER_BOT_WIDE:
         xpr = xr ;
         ypr = yr+hy+1 ;
         wid = (position==METER_BOT_WIDE) ? wx : METER_WIDTH ;
         if( ypr+METER_HEIGHT > scr_height ) ypr = yr - METER_HEIGHT-2 ;
      break ;
   }

   /* create a popup shell with a scale */

   wmsg = XtVaCreatePopupShell(
             "menu" , xmDialogShellWidgetClass , wparent ,
                XmNx , xpr ,
                XmNy , ypr ,
                XmNborderWidth , 0 ,
                XmNoverrideRedirect , True ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

#if 0
   if( MCW_isitmwm( wparent ) ){
      XtVaSetValues( wmsg ,
                        XmNmwmDecorations , MWM_DECOR_BORDER ,
                        XmNmwmFunctions   , MWM_FUNC_MOVE ,
                     NULL ) ;
   }
#endif

   wscal = XtVaCreateManagedWidget(
            "menu" , xmScaleWidgetClass , wmsg ,
               XmNminimum , 0 ,
               XmNmaximum , 100 ,
               XmNshowValue , False ,
               XmNvalue , 0 ,
               XmNorientation , XmHORIZONTAL ,
               XmNscaleWidth , wid ,
               XmNscaleHeight , METER_HEIGHT ,
               XmNborderWidth , 0 ,
               XmNhighlightThickness , 0 ,
               XmNshadowThickness , 0 ,
               XmNtraversalOn , True  ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   XtPopup( wmsg , XtGrabNone ) ; RWC_sleep(1);

   RETURN(wscal) ;
}

/*--------------------------------------------------------------------*/

void MCW_popdown_meter( Widget wscal )
{
   if( wscal == NULL ) return ;
   XtDestroyWidget( XtParent(wscal) ) ;
   return ;
}

/*--------------------------------------------------------------------*/

void MCW_set_meter( Widget wscal , int percent )
{
   int val , old ;

#undef  NCOL
#define NCOL 30
#ifdef NCOL
   static int icol=0 ;
   static char *cname[] = {
      "#0000ff", "#3300ff", "#6600ff", "#9900ff", "#cc00ff",
      "#ff00ff", "#ff00cc", "#ff0099", "#ff0066", "#ff0033",
      "#ff0000", "#ff3300", "#ff6600", "#ff9900", "#ffcc00",
      "#ffff00", "#ccff00", "#99ff00", "#66ff00", "#33ff00",
      "#00ff00", "#00ff33", "#00ff66", "#00ff99", "#00ffcc",
      "#00ffff", "#00ccff", "#0099ff", "#0066ff", "#0033ff"
    } ;
#endif

   val = percent ;
   if( wscal == NULL || val < 0 || val > 100 ) return ;

   XmScaleGetValue( wscal , &old ) ; if( val == old ) return ;

   XtVaSetValues( wscal , XmNvalue , val , NULL ) ;

#ifdef NCOL
   { Widget ws = XtNameToWidget(wscal,"Scrollbar") ;
     if( ws != NULL )
       XtVaSetValues( ws ,
                       XtVaTypedArg , XmNtroughColor , XmRString ,
                                      cname[icol] , strlen(cname[icol])+1 ,
                      NULL ) ;
     icol = (icol+1) % NCOL ;
   }
#endif

   XmUpdateDisplay(wscal) ;
   return ;
}

/*------------------------------------------------------------------------*/

#if 0
static void MCW_textwin_timer_CB( XtPointer client_data , XtIntervalId *id )
{
   Widget ws = (Widget)client_data ;
   XtVaSetValues( ws , XmNincrement     , 1 ,
                       XmNpageIncrement , 1 ,
                       XmNmaximum       , 100 , NULL ) ;
}
#endif

/*-----------------------------------------------------------------------*/

#define RONLY_NUM 1
#define EDIT_NUM  2

static MCW_action_item TWIN_act[] = {
 { "Quit" , MCW_textwin_CB , NULL , NULL , "Close window" , 0 } ,
 { "Set"  , MCW_textwin_CB , NULL , NULL , "Apply choice and close window" , 0 }
} ;

MCW_textwin * new_MCW_textwin( Widget wpar, char *msg, int type )
{
   return new_MCW_textwin_2001( wpar,msg,type , NULL,NULL ) ;
}

/*-----------------------------------------------------------------------
   Modified 10 Jul 2001 to include killing callback
-------------------------------------------------------------------------*/

MCW_textwin * new_MCW_textwin_2001( Widget wpar, char *msg, int type,
                                    void_func *kill_func , XtPointer kill_data )
{
   MCW_textwin *tw ;
   int wx,hy,xx,yy , xp,yp , scr_width,scr_height , xr,yr , xpr,ypr , ii,nact ;
   int swid , shi ;
   Position xroot , yroot ;
   Screen *scr ;
   Boolean editable , cursorable ;
   Arg wa[64] ; int na ; Widget ws ;

ENTRY("new_MCW_textwin_2001") ;

   /*-- sanity check --*/

   if( wpar == NULL || !XtIsRealized(wpar) ) RETURN(NULL) ;

   /* set position based on parent and screen geometry */

   MCW_widget_geom( wpar , &wx,&hy,&xx,&yy ) ;     /* geometry of parent */
   XtTranslateCoords( wpar, 0,0, &xroot,&yroot ) ; /* root coords */
   xr = (int) xroot ; yr = (int) yroot ;

   scr        = XtScreen(wpar) ;
   scr_width  = WidthOfScreen(scr) ;
   scr_height = HeightOfScreen(scr) ;

   xp = xx+8 ;  xpr = xr+8 ;
        if( xpr+50 > scr_width ){ xp -= 100 ; xpr -= 100 ; } /* too right */
   else if( xpr+10 < 0 )        { xpr = xp = 1 ; }           /* too left  */

   yp = yy+hy+8 ;  ypr = yr+hy+8 ;
        if( ypr+50 > scr_height ){ yp = yy-8 ; ypr = yr-100 ;} /* too down */
   else if( ypr+10 < 0 )         { ypr = yp = 1 ;            } /* too up   */

   /* create a popup shell */

   tw = myXtNew(MCW_textwin) ;

   tw->kill_func = kill_func ;  /* 10 Jul 2001 */
   tw->kill_data = kill_data ;

   tw->wshell = XtVaCreatePopupShell(
                 "menu" , xmDialogShellWidgetClass , wpar ,
                    XmNx , xpr ,
                    XmNy , ypr ,
                    XmNborderWidth , 0 ,
                    XmNborderColor , 0 ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;

   XmAddWMProtocolCallback(
        tw->wshell ,
        XmInternAtom( XtDisplay(tw->wshell) , "WM_DELETE_WINDOW" , False ) ,
        MCW_textwinkill_CB , (XtPointer) tw ) ;

   /* create a form to hold everything else */

   tw->wtop = XtVaCreateWidget(
                "menu" , xmFormWidgetClass , tw->wshell ,
                  XmNborderWidth , 0 ,
                  XmNborderColor , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
                NULL ) ;

   /* create action area */

   editable = (Boolean) (type == TEXT_EDITABLE) ;
   cursorable = True ;  /* 26 Feb 2007 */

   nact = (editable) ? EDIT_NUM : RONLY_NUM ;
   for( ii=0 ; ii < nact ; ii++ ){
     TWIN_act[ii].data     = (XtPointer) tw ;
     TWIN_act[ii].make_red = 0 ;
   }
   TWIN_act[nact-1].make_red = 1 ;

   tw->wactar = MCW_action_area( tw->wtop , TWIN_act , nact ) ;

   /* create text area */

   tw->wscroll = XtVaCreateManagedWidget(
                    "menu" , xmScrolledWindowWidgetClass , tw->wtop ,
                       XmNscrollingPolicy        , XmAUTOMATIC ,
                       XmNvisualPolicy           , XmVARIABLE ,
                       XmNscrollBarDisplayPolicy , XmAS_NEEDED ,

                       XmNleftAttachment  , XmATTACH_FORM ,
                       XmNrightAttachment , XmATTACH_FORM ,
                       XmNbottomAttachment, XmATTACH_FORM ,
                       XmNtopAttachment   , XmATTACH_WIDGET ,
                       XmNtopWidget       , tw->wactar ,
                       XmNtopOffset       , 7 ,

                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;

   XtVaSetValues( tw->wactar ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNrightAttachment, XmATTACH_FORM ,
                     XmNtopAttachment  , XmATTACH_FORM ,
                     XmNtopOffset      , 7 ,
                  NULL ) ;

   tw->wtext = XtVaCreateManagedWidget(
                    "menu" , xmTextWidgetClass , tw->wscroll ,
                       XmNeditMode               , XmMULTI_LINE_EDIT ,
                       XmNautoShowCursorPosition , cursorable ,
                       XmNeditable               , editable ,
                       XmNcursorPositionVisible  , cursorable ,
                    NULL ) ;

   if( msg == NULL ) msg = "\0" ;  /* 27 Sep 2000 */

   if( msg != NULL ){
      int cmax = 20 , ll , nlin ;
      char *cpt , *cold , cbuf[128] ;
      XmString xstr ;
      XmFontList xflist ;

      /* In lesstif, the text length is limited by the XmTextGetMaxLength
       * resource, however setting it via XmTextSetMaxLength does not work.
       * The string lengths seems to be limited to perhaps 100, even though
       * XmTextGetMaxLength might return 256.
       *
       * The solution is to use XmTextSetString(w, str) to set the text.
       * After that, XmTextGetMaxLength returns an updated value.
       *
       * 31 Dec, 2008 [lesstif patrol] */
#ifdef USING_LESSTIF
      XmTextSetString( tw->wtext , msg ) ;
#else
      XtVaSetValues( tw->wtext , XmNvalue , msg , NULL ) ;
#endif

      XtVaGetValues( tw->wtext , XmNfontList , &xflist , NULL ) ;

      cmax = 20 ; nlin = 1 ;
      for( cpt=msg,cold=msg ; *cpt != '\0' ; cpt++ ){
        if( *cpt == '\n' ){
          ll = cpt - cold - 1 ; if( cmax < ll ) cmax = ll ;
          cold = cpt ; nlin++ ;
        }
      }
      ll = cpt - cold - 1 ; if( cmax < ll ) cmax = ll ;
      if( cmax > 100 ) cmax = 100 ;
      cmax +=3 ;
      for( ll=0 ; ll < cmax ; ll++ ) cbuf[ll] = 'x' ;
      cbuf[cmax] = '\0' ;

      xstr = XmStringCreateLtoR( cbuf , XmFONTLIST_DEFAULT_TAG ) ;
      swid = XmStringWidth ( xflist , xstr ) + 44 ;
      shi  = XmStringHeight( xflist , xstr ) * nlin + 66 ;
      XmStringFree( xstr ) ;

      cmax = WidthOfScreen(XtScreen(wpar)) - 128 ;
      if( swid > cmax ) swid = cmax ;

      cmax = HeightOfScreen(XtScreen(wpar)) - 128 ;
      if( shi > cmax ) shi = cmax ;
   } else {
      swid = shi = 100 ;
   }

   XtManageChild( tw->wtop ) ;

   XtVaSetValues( tw->wshell , XmNwidth,swid , XmNheight,shi , NULL ) ;

   XtPopup( tw->wshell , XtGrabNone ) ; RWC_sleep(1);

   RWC_visibilize_widget( tw->wshell ) ;  /* 09 Nov 1999 */

   RWC_xineramize( XtDisplay(tw->wshell) ,
                   xpr,ypr,swid,shi , &xpr,&ypr ); /* 27 Sep 2000 */

   XtVaSetValues( tw->wshell, XmNx,xpr , XmNy,ypr , NULL ) ;

   tw->shell_width = swid ; tw->shell_height = shi ; /* 10 Jul 2001 */

   NORMAL_cursorize( tw->wshell ) ;

   ws = XtNameToWidget(tw->wscroll,"VertScrollBar") ;
   if( ws != NULL ){
#ifdef DARWIN
     XtVaSetValues( ws , XmNshowArrows , XmMIN_SIDE , NULL ) ;
#endif
     (void)XmProcessTraversal( ws , XmTRAVERSE_CURRENT ) ;
#if 0
     (void)XtAppAddTimeOut( XtWidgetToApplicationContext(ws) ,
                            66 , MCW_textwin_timer_CB , ws   ) ;
#endif
   }
#ifdef DARWIN
   ws = XtNameToWidget(tw->wscroll,"HorScrollBar") ;
   if( ws != NULL )
     XtVaSetValues( ws , XmNshowArrows , XmMIN_SIDE , NULL ) ;
#endif

   RETURN(tw) ;
}

/*--------------------------------------------------------------------*/

void MCW_textwin_alter( MCW_textwin *tw , char *mmm ) /* 10 Jul 2001 */
{
   int swid , shi ;
   char *msg = mmm ;
   int cmax = 20 , ll , nlin ;
   char *cpt , *cold , cbuf[128] ;
   XmString xstr ;
   XmFontList xflist ;

ENTRY("MCW_textwin_alter") ;

   if( tw == NULL ) EXRETURN ;     /* bad */

   if( msg == NULL ) msg = " " ; /* don't let user be so stupid */

#if 0
   /*-- compute size of text window with new message in it --*/

   XtVaGetValues( tw->wtext , XmNfontList , &xflist , NULL ) ;

   /* find longest line in msg */

   cmax = 20 ; nlin = 1 ;
   for( cpt=msg,cold=msg ; *cpt != '\0' ; cpt++ ){
      if( *cpt == '\n' ){
         ll = cpt - cold - 1 ; if( cmax < ll ) cmax = ll ;
         cold = cpt ; nlin++ ;
      }
   }
   ll = cpt - cold - 1 ; if( cmax < ll ) cmax = ll ;
   if( cmax > 100 ) cmax = 100 ;

   /* fill a dummy string of that length, plus a bit */

   cmax+=3 ;
   for( ll=0 ; ll < cmax ; ll++ ) cbuf[ll] = 'x' ;
   cbuf[cmax] = '\0' ;

   /* find width, height of the dummy string */

   xstr = XmStringCreateLtoR( cbuf , XmFONTLIST_DEFAULT_TAG ) ;
   swid = XmStringWidth ( xflist , xstr ) + 44 ;
   shi  = XmStringHeight( xflist , xstr ) * nlin + 66 ;
   XmStringFree( xstr ) ;

   /* find width, height of screen */

   cmax = WidthOfScreen(XtScreen(tw->wshell)) - 128 ;
   if( swid > cmax ) swid = cmax ;

   cmax = HeightOfScreen(XtScreen(tw->wshell)) - 128 ;
   if( shi > cmax ) shi = cmax ;
#endif

   /*-- actually set new text --*/

   XtVaSetValues( tw->wtext , XmNvalue , msg , NULL ) ;

#if 1
   MCW_widget_geom( tw->wtext , &swid , &shi , NULL,NULL ) ;
   XtVaSetValues( tw->wshell , XmNwidth,swid+29 , XmNheight,shi+59 , NULL ) ;
   tw->shell_width = swid+29 ; tw->shell_height = shi+59 ;
#endif

#if 0
   /*-- maybe set new window size --*/

   if( swid > tw->shell_width || shi > tw->shell_height ){
      tw->shell_width  = swid = MAX( swid , tw->shell_width ) ;
      tw->shell_height = shi  = MAX( shi  , tw->shell_height ) ;
      XtVaSetValues( tw->wshell , XmNwidth,swid , XmNheight,shi , NULL ) ;
   }
#endif

   EXRETURN ;
}

/*--------------------------------------------------------------------*/

void MCW_textwin_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_textwin *tw = (MCW_textwin *) client_data ;
   char *wname    = XtName(w) ;

   if( client_data == NULL ) return ;

   if( strcmp(wname,"Quit") == 0 ){
      if( tw->kill_func != NULL )
#if 0
        tw->kill_func(tw->kill_data); /* 10 Jul 2001 */
#else
        AFNI_CALL_VOID_1ARG( tw->kill_func , XtPointer , tw->kill_data ) ;
#endif
      XtDestroyWidget( tw->wshell ) ;
      myXtFree( tw ) ;
      return ;
   }

   XBell( XtDisplay(w) , 100 ) ;
   return ;
}

/*--------------------------------------------------------------------*/

void MCW_textwinkill_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_textwin *tw = (MCW_textwin *) client_data ;

   if( tw->kill_func != NULL )
#if 0
     tw->kill_func(tw->kill_data); /* 10 Jul 2001 */
#else
     AFNI_CALL_VOID_1ARG( tw->kill_func , XtPointer , tw->kill_data ) ;
#endif
   XtDestroyWidget( tw->wshell ) ;
   myXtFree( tw ) ;
   return ;
}

/*-----------------------------------------------------------------------
   03 Jan 1999: Check if a widget is potentially visible
                -- return 0 if not, 1 if yes.
-------------------------------------------------------------------------*/

int MCW_widget_visible( Widget w )
{
   Window ww ;
   XWindowAttributes wa ;

   if( w == (Widget)NULL ) return 0 ;
   ww = XtWindow(w) ;
   if( ww == (Window)NULL ) return 0 ;

   XGetWindowAttributes( XtDisplay(w) , ww , &wa ) ;

   return ( (wa.map_state == IsViewable) ? 1 : 0 ) ;
}

/*------------------------------------------------------------------
 June 1999: routine to get string constants either from X defaults
            or from Unix environment variables.  Returns a pointer
            to static storage -- do not free()!
--------------------------------------------------------------------*/

#include <ctype.h>

char * RWC_getname( Display *display , char *name )
{
   char *cval , qqq[256] ;
   int nn , ii ;

   if( name == NULL || name[0] == '\0' ) return NULL ;

   /* try X11 */

   if( display != NULL ){
     cval = XGetDefault(display,"AFNI",name) ;
     if( cval != NULL ) return cval ;
   }

   /* try AFNI_name */

   strcpy(qqq,"AFNI_") ; strcat(qqq,name) ;
   cval = my_getenv(qqq) ;
   if( cval != NULL ) return cval ;

   /* try AFNI_NAME (uppercase it) */

   strcpy(qqq,"AFNI_") ; nn = strlen(name) ;
   for( ii=0 ; ii < nn && ii < 250 ; ii++ ) qqq[ii+5] = toupper(name[ii]) ;
   qqq[ii+5] = '\0' ;
   cval = my_getenv(qqq) ;
   return cval ;
}

/*-------------------------------------------------------------------
  09 Nov 1999: move a widget to make sure it is visible
---------------------------------------------------------------------*/

void RWC_visibilize_widget( Widget w )
{
   Position xroot , yroot ;
   int wx,hy,xx,yy , scr_width,scr_height , xo,yo ;
   Screen *scr ;

ENTRY("RWC_visibilize_widget") ;

   if( w == NULL || !XtIsWidget(w) ) EXRETURN ;

   MCW_widget_geom( w , &wx,&hy,&xx,&yy ) ;     /* geometry of widget */

   scr        = XtScreen( w ) ;
   scr_width  = WidthOfScreen( scr ) ;
   scr_height = HeightOfScreen( scr ) ;

   xo = xx ; yo = yy ;                          /* save original position */

   if( xx+wx > scr_width ) xx = scr_width - wx ;
   if( xx    < 0         ) xx = 0              ;

   if( yy+hy > scr_height ) yy = scr_height - hy ;
   if( yy    < 0          ) yy = 0               ;

   RWC_xineramize( XtDisplay(w) , xx,yy,wx,hy , &xx,&yy ); /* 27 Sep 2000 */

   if( xx != xo || yy != yo )
     XtVaSetValues( w , XmNx , xx , XmNy , yy , NULL ) ;

   RWC_sleep(1) ; MCW_expose_widget(w) ;  /* 09 Nov 2007 */

   EXRETURN ;
}

/*----------------------------------------------------------------------
  A callback version of the above (for use when menus are mapped, say)
------------------------------------------------------------------------*/

static void RWC_visibilize_timeout_CB( XtPointer cd , XtIntervalId *id )
{
   Widget w = (Widget) cd ;
ENTRY("RWC_visibilize_timeout_CB") ;
   RWC_visibilize_widget(w) ; EXRETURN ;
}

/*--------------------------------------------------------------------*/

void RWC_visibilize_CB( Widget w , XtPointer cd , XtPointer cb )
{
   Widget wpar = w ;
ENTRY("RWC_visibilize_CB") ;

   if( AFNI_yesenv("AFNI_DONT_MOVE_MENUS") ) return ;  /* 08 Aug 2001 */

   while( !XtIsShell(wpar) ){ wpar = XtParent(w); } /* find 1st shell parent */

   /* must wait for the thing to actually appear, dammit */

   (void) XtAppAddTimeOut( XtWidgetToApplicationContext(wpar) ,
                           3 , RWC_visibilize_timeout_CB , wpar ) ;
   EXRETURN ;
}

/*----------------------------------------------------------------------
   Given a rectangle (xx..xx+ww,yy..yy+hh), return the new origin
   (xn,yn) so that the rectangle (xn..xn+ww,yn..yn+hh) fits onto
   a single Xinerama sub-screen.  If the AFNI.xinerama X11 resource is
   not found, then this routine uses the display size.
   -- RWCox -- 27 Sep 2000
------------------------------------------------------------------------*/

#define BUF 5  /* buffer around the rectangle */

void RWC_xineramize( Display *dpy,
                     int xx, int yy, int ww, int hh, int *xn, int *yn )
{
   static int first=1 ;
   static int nxsi=0 , *xbot,*ybot,*xtop,*ytop ;
   int ii , ss ;

ENTRY("RWC_xineramize") ;

   if( dpy==NULL || xn==NULL || yn==NULL || ww<0 || hh<0 ) EXRETURN; /* ERROR */

   /*--- first time in: check AFNI.xinerama X resource
                        load boundaries of sub-screens from resource ---*/

   if( first ){
      char *xdef , *xp ;
      int nn,xorg,yorg,wide,high ;

      first = 0 ;                         /* never again */
      xdef  = getenv( "AFNI_XINERAMA" ) ;

      if( xdef != NULL && (xdef[0] == 'N' || xdef[0] == 'n') ){ /* skip Xinerama */
         nxsi = 0 ;
         STATUS("AFNI_XINERAMA is NO") ;
      } else {
         xdef = XGetDefault(dpy,"AFNI","xinerama") ; /* get resource */
         if( xdef == NULL ) xdef = getenv("AFNI_xinerama") ;  /* 27 Oct 2003 */
         if( xdef != NULL ){
            char *qdef = strdup(xdef) ;
            for( nn=0 ; qdef[nn] != '\0' ; nn++ )
              if( qdef[nn] == '_' || qdef[nn] == ':' ) qdef[nn] = ' ' ;

            nn = 0 ; sscanf(qdef,"%d%n",&nxsi,&nn) ;  /* number of sub-screens */
            if( nn <= 0 || nxsi <= 1 ){               /* ERROR */
               nxsi = 0 ;
            } else {
               xbot = (int *) malloc(sizeof(int)*nxsi) ; /* make arrays to */
               ybot = (int *) malloc(sizeof(int)*nxsi) ; /* store sub-screen */
               xtop = (int *) malloc(sizeof(int)*nxsi) ; /* coordinate ranges */
               ytop = (int *) malloc(sizeof(int)*nxsi) ;
               xp = qdef + nn ;
               for( ii=0 ; ii < nxsi ; ii++ ){    /* scan for sub-screen info */
                  nn = 0 ;
                  sscanf(xp,"%d%d%d%d%d%n",&ss,&xorg,&yorg,&wide,&high,&nn) ;
                  if( nn <= 0 ) break ;           /* ERROR */
                  xbot[ii] = xorg ; xtop[ii] = xorg+wide ;
                  ybot[ii] = yorg ; ytop[ii] = yorg+high ;
                  xp += nn ;

                  if(PRINT_TRACING){
                    char str[256] ;
                    sprintf(str," Screen %d: xbot=%4d ybot=%4d xtop=%4d ytop=%4d",
                                  ii,xbot[ii],ybot[ii],xtop[ii],ytop[ii] ) ;
                    STATUS(str) ;
                  }
               }

               nxsi = ii ;  /* in case the scan aborted */
            }
         }
      }

      /* if nothing found yet, use the display size */

      if( nxsi <= 0 ){
         nxsi = 1 ;
         xbot = (int *) malloc(sizeof(int)*nxsi) ;
         ybot = (int *) malloc(sizeof(int)*nxsi) ;
         xtop = (int *) malloc(sizeof(int)*nxsi) ;
         ytop = (int *) malloc(sizeof(int)*nxsi) ;
         xbot[0] = ybot[0] = 0 ;
         xtop[0] = WidthOfScreen(DefaultScreenOfDisplay(dpy)) ;
         ytop[0] = HeightOfScreen(DefaultScreenOfDisplay(dpy)) ;
      }
   }

#if 0                                           /* doesn't occur anymore */
   if( nxsi == 0 ){ *xn=xx; *yn=yy; EXRETURN; } /* not setup? change nothing */
#endif

   /*--- find the Xinerama sub-screen that (xx,yy) is on (if any) ---*/

   if( nxsi > 1 ){
      for( ss=0 ; ss < nxsi ; ss++ ){
         if( xx >= xbot[ss] && xx < xtop[ss] &&
             yy >= ybot[ss] && yy < ytop[ss]   ) break ;
      }
   } else {
      ss = 0 ;  /* must use #0 - what else is there? */
   }

   if(PRINT_TRACING){
      char str[256] ;
      sprintf(str,"Rect: xx=%d yy=%d ww=%d hh=%d; On ss=%d",xx,yy,ww,hh,ss); STATUS(str);
   }

   /*--- if not inside any screen, find one it is closest to ---*/

   if( ss >= nxsi ){
      int dleft,dright,dtop,dbot,dd , dmin , xdif,ydif ;
      dmin = 123456789 ; ss = 0 ;
      for( ii=0 ; ii < nxsi; ii++ ){
         xdif = (xx < xbot[ii]) ? (xbot[ii]-xx)       /* x dist to    */
               :(xx > xtop[ii]) ? (xx-xtop[ii]) : 0 ; /* [xbot..xtop] */

         ydif = (yy < ybot[ii]) ? (ybot[ii]-yy)
               :(yy > ytop[ii]) ? (yy-ytop[ii]) : 0 ;

         dleft  = abs(xx-xbot[ii]) + ydif ;  /* L1 dist to left edge */
         dright = abs(xx-xtop[ii]) + ydif ;
         dbot   = abs(yy-ybot[ii]) + xdif ;
         dtop   = abs(yy-ytop[ii]) + xdif ;

                           dd = dleft ;      /* find smallest dist */
         if( dright < dd ) dd = dright ;
         if( dbot   < dd ) dd = dbot ;
         if( dtop   < dd ) dd = dtop ;

         if( dd < dmin ){ dmin = dd; ss = ii; } /* smallest so far? */
      }

      if(PRINT_TRACING){
         char str[256] ; sprintf(str,"New ss=%d",ss) ; STATUS(str) ;
      }
   }

   /*--- now adjust position so all of rectangle
         (xx..xx+ww,yy..yy+hh) fits on that screen (if possible) ---*/

   if( xx+ww+BUF >= xtop[ss] ){ xx = xtop[ss]-ww-1-2*BUF; } /* move left */
   if( yy+hh+BUF >= ytop[ss] ){ yy = ytop[ss]-hh-1-2*BUF; } /* move up  */
   if( xx    < xbot[ss]+BUF  ){ xx = xbot[ss]+BUF; }        /* move right */
   if( yy    < ybot[ss]+BUF  ){ yy = ybot[ss]+BUF; }        /* move down */


   if(PRINT_TRACING){
      char str[256] ; sprintf(str,"New xx=%d yy=%d",xx,yy) ; STATUS(str) ;
   }

   *xn = xx ; *yn = yy ; EXRETURN ;
}

/*----------------------------------------------------------------------
  NULL out a pointer when a widget is destroyed -- 31 Jul 2001 - RWCox
------------------------------------------------------------------------*/

void RWC_destroy_nullify_CB( Widget w, XtPointer xp, XtPointer cd )
{
   void ** p = (void **) xp ;
ENTRY("RWC_destroy_nullify_CB") ;
   if( p != NULL ) *p = NULL ;
   EXRETURN ;
}

void RWC_destroy_nullify( Widget w, void **p )
{
   if( p != NULL && w != NULL )
     XtAddCallback( w, XmNdestroyCallback, RWC_destroy_nullify_CB, p ) ;
   return ;
}

void RWC_destroy_nullify_cancel( Widget w, void **p )
{
   if( w != NULL )
     XtRemoveCallback( w, XmNdestroyCallback, RWC_destroy_nullify_CB, p ) ;
   return ;
}

/*---------------------------------------------------------------------------*/

static RWC_draw_rect( Display *dis, Window win, GC gc,
                      int x1, int y1, int x2, int y2  )
{
  int xb,yb , xt,yt ;
  unsigned int short w,h ;

  if( x1 < x2 ){ xb=x1; xt=x2; } else { xb=x2; xt=x1; }
  if( y1 < y2 ){ yb=y1; yt=y2; } else { yb=y2; yt=y1; }
  w = xt-xb ; h = yt-yb ;
  if( w || h )
    XDrawRectangle( dis,win,gc , xb,yb,w,h ) ;
  else
    XDrawPoint( dis,win,gc , xb,yb ) ;
}

/*---------------------------------------------------------------------------*/

static Cursor cur = None ;  /* 17 Jun 2002 */

static void RWC_drag_cursor( Display *dis )
{
   XColor fg , bg ;
   Colormap cmap ;
   Boolean  good ;

   if( cur == None ){
     cur  = XCreateFontCursor( dis , XC_arrow ) ;
     cmap = DefaultColormap( dis , DefaultScreen(dis) ) ;
     good =   XParseColor( dis, cmap, "yellow" , &fg )
           && XParseColor( dis, cmap, "red"    , &bg )  ;
     if( good ) XRecolorCursor( dis , cur , &fg , &bg ) ;
   }
}

void RWC_drag_rectangle( Widget w, int x1, int y1, int *x2, int *y2 )
{
   Display *dis ;
   Window win , rW,cW ;
   int grab , xold,yold , x,y, rx,ry , first=1 ;
   unsigned int mask ;                                      /* which buttons */
   unsigned int bmask=Button1Mask|Button2Mask|Button3Mask ; /* all buttons  */
   XGCValues  gcv;
   GC         myGC ;

ENTRY("RWC_drag_rectangle") ;

   /** make a GC for invert drawing **/

   gcv.function = GXinvert ;
   myGC         = XtGetGC( w , GCFunction , &gcv ) ;

   /** grab the pointer (so no one else gets events from it),
       and confine it to the window in question              **/

   dis = XtDisplay(w) ; win = XtWindow(w) ;

   RWC_drag_cursor(dis) ;

   grab = !XGrabPointer(dis, win, False, 0, GrabModeAsync,
                        GrabModeAsync, win, cur , (Time)CurrentTime);

   /* grab fails => exit */

   if( !grab ){ XBell(dis,100); *x2=x1; *y2=y1; EXRETURN; }

   xold = x1 ; yold = y1 ;  /* current location of pointer */

   /** loop and find out where the pointer is (while button is down) **/

   while( XQueryPointer(dis,win,&rW,&cW,&rx,&ry,&x,&y,&mask) ){

     /* check if all buttons are released */

     if( !(mask & bmask) ) break ;  /* no button down => done! */

     /* pointer now at (x,y) in the window */

     /* if it has moved, redraw rectangle */

     if( x != xold || y != yold ){

       if( !first )  /* undraw old rectangle */
         RWC_draw_rect( dis,win,myGC , x1,y1 , xold,yold ) ;

       /* draw new rectangle */

       xold = x ; yold = y ; first = 0 ;
       RWC_draw_rect( dis,win,myGC , x1,y1 , xold,yold ) ;

     } /* end of new (x,y) position */

   } /* end of loop while button is pressed */

   if( !first )  /* undraw old rectangle */
     RWC_draw_rect( dis,win,myGC , x1,y1 , xold,yold ) ;

   /* clean up */

   XtReleaseGC( w , myGC ) ;
   if (grab) XUngrabPointer(dis, (Time)CurrentTime) ;

   *x2 = xold ; *y2 = yold ;  /* output values */
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/

static RWC_draw_circle( Display *dis, Window win, GC gc, int xc, int yc, int rad )
{
   int xb,yb ;
   unsigned int ww ;

   if( rad < 0 ) rad = 0 ;
   xb = xc-rad ; yb = yc-rad ; ww = 2*rad ;
   XDrawArc( dis,win,gc , xb,yb , ww,ww , 0,360*64 ) ;
}

/*---------------------------------------------------------------------------*/

void RWC_drag_circle( Widget w, int x1, int y1, int *radius )
{
   Display *dis ;
   Window win , rW,cW ;
   int grab , xold,yold , x,y, rx,ry , first=1 , rrr=0 ;
   unsigned int mask ;                                      /* which buttons */
   unsigned int bmask=Button1Mask|Button2Mask|Button3Mask ; /* all buttons  */
   XGCValues  gcv;
   GC         myGC ;

ENTRY("RWC_drag_circle") ;

   /** make a GC for invert drawing **/

   gcv.function = GXinvert ;
   myGC         = XtGetGC( w , GCFunction , &gcv ) ;

   /** grab the pointer (so no one else gets events from it),
       and confine it to the window in question              **/

   dis = XtDisplay(w) ; win = XtWindow(w) ;

   RWC_drag_cursor(dis) ;

   grab = !XGrabPointer(dis, win, False, 0, GrabModeAsync,
                        GrabModeAsync, win, cur , (Time)CurrentTime);

   /* grab fails => exit */

   if( !grab ){ XBell(dis,100); *radius=0; EXRETURN; }

   xold = x1 ; yold = y1 ;  /* current location of pointer */

   /** loop and find out where the pointer is (while button is down) **/

   while( XQueryPointer(dis,win,&rW,&cW,&rx,&ry,&x,&y,&mask) ){

     /* check if all buttons are released */

     if( !(mask & bmask) ) break ;  /* no button down => done! */

     /* pointer now at (x,y) in the window */

     /* if it has moved, redraw rectangle */

     if( x != xold || y != yold ){

       if( !first )  /* undraw old circle */
         RWC_draw_circle( dis,win,myGC , x1,y1 , rrr ) ;

       /* draw new circle */

       xold = x ; yold = y ; first = 0 ;
       rrr = (int)rint(sqrt( (x-x1)*(x-x1) + (y-y1)*(y-y1) )) ;
       RWC_draw_circle( dis,win,myGC , x1,y1 , rrr ) ;

     } /* end of new (x,y) position */

   } /* end of loop while button is pressed */

   if( !first )  /* undraw old circle */
     RWC_draw_circle( dis,win,myGC , x1,y1 , rrr ) ;

   /* clean up */

   XtReleaseGC( w , myGC ) ;
   if (grab) XUngrabPointer(dis, (Time)CurrentTime) ;

   *radius = rrr ;
   EXRETURN ;
}

/*-------------------------------------------------------------------*/
/*!  Sleep a given # of milliseconds (uses the Unix select routine).
---------------------------------------------------------------------*/

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

void RWC_sleep( int msec )
{
   struct timeval tv ;
   if( msec <= 0 ) return ;             /* can't wait into the past */
   tv.tv_sec  = msec/1000 ;
   tv.tv_usec = (msec%1000)*1000 ;
   select( 1 , NULL,NULL,NULL , &tv ) ;
   return ;
}

/*-----------------------------------------------------------------*/
/*! Popdown a widget that may not be a shell. [30 Jun 2003]
-------------------------------------------------------------------*/

void RWC_XtPopdown( Widget w )
{
   Widget wpar = w ;

ENTRY("RWC_XtPopdown") ;

   if( wpar == NULL ) EXRETURN ;
   RWC_sleep(1) ;
   while( XtIsShell(wpar)==0 && XtParent(wpar)!=NULL ) wpar = XtParent(wpar);
   XtPopdown(wpar) ; RWC_sleep(1) ;
   EXRETURN ;
}

/*-----------------------------------------------------------------*/
/******************** Speech stuff (for Mac OS X) ******************/

#if !defined(NO_FRIVOLITIES) && defined(DARWIN)

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/wait.h>
#include <sys/types.h>

static int have_say = -1 ;
static char voice[128] = "Cellos" ;

/*-----------------------------------------------------------------*/
/*! Set the voice for the Apple speech synthesizer. */

void AFNI_speak_setvoice( char *vvv )
{
   int ll ;
   if( vvv == NULL || *vvv == '\0' ) return ;
   ll = strlen(vvv) ; if( ll > 100 ) return ;
   strcpy(voice,vvv) ;               return ;
}


/*-----------------------------------------------------------------*/
/*! Speak a string using Apple's say command:
    - string = Apple text-to-speech code
    - nofork = 1 if you want to wait for the speech to finish;
             = 0 if you want the function to return immediately,
                 before speech finishes (or perhaps even starts). */

void AFNI_speak( char *string , int nofork )
{
   char *buf ; pid_t ppp ;

   /* bad input ==> quit */

   if( string == NULL || *string == '\0' ) return ;

   /* user says "don't talk" ==> quit */

   buf = getenv("AFNI_SPEECH") ;
#if 1
   if( buf == NULL || toupper(*buf) != 'Y' ) return ;   /* 02 Apr 2004 */
#else
   if( buf != NULL && toupper(*buf) == 'N' ) return ;
#endif

   /* don't have "say" program ==> quit */

   if( have_say == -1 ) have_say = (THD_find_executable("say") != NULL) ;
   if( have_say == 0 ) return ;

   /* if want speech to run in a separate process ... */

   if( !nofork ){
     ppp = fork() ;
     if( ppp < 0 ) return ; /* fork failed */

     /* parent: wait for child to exit (happens almost instantly) */

     if( ppp > 0 ){ waitpid(ppp,NULL,0); return; }

     /* child: fork again immediately, then this child exits;
        this is to prevent zombie processes from hanging around */

     ppp = fork() ; if( ppp != 0 ) _exit(0) ; /* child exits now */

     /* grandchild continues on to actually do something */
   }

   /* Run the say program using system() */

   buf = (char *)malloc(strlen(string)+32) ;
   sprintf(buf,"say -v%s '%s'",voice,string) ;
   system(buf) ; free(buf) ;

   if( !nofork ) _exit(0) ;  /* grandchild exits */
   return ;                  /* no forking ==> return to caller */
}

#else

void AFNI_speak( char *string , int nofork ){ return; }  /* dummy function */
void AFNI_speak_setvoice( char *vvv ){ return; }

#endif
