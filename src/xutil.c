#include "xutil.h"
#include "afni_environ.h"

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

                               if( ! XtIsRealized(w)           ) return ;
                               if( ! XtIsManaged(w)            ) return ;
   xev.window  = XtWindow(w) ; if( xev.window == (Window) NULL ) return ;
   xev.type    = Expose ;
   xev.display = XtDisplay(w) ;
   xev.x       = xev.y = 0 ;
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

   XtVaGetValues( w , XmNcolormap  , &cmap , NULL ) ;
   return cmap ;
}

int MCW_get_depth( Widget w )  /* 14 Sep 1998 */
{
   int depth = 0 ;

   if( w == NULL || ! XtIsWidget(w) ) return 0 ;
   XtVaGetValues( w , XmNdepth  , &depth , NULL ) ;
   return depth ;
}

Visual * MCW_get_visual( Widget w )  /* 14 Sep 1998 */
{
   Visual * visual = NULL ;
   Widget wpar = w ;

   if( w == NULL || ! XtIsWidget(w) ) return NULL ;

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

void MCW_set_widget_bg( Widget w , char * cname , Pixel pix )
{
   Pixel bg_pix , topsh_pix , botsh_pix , fg_pix , sel_pix ;
   Colormap cmap ;

#if 1
   if( ! XtIsWidget(w) ) return ;
#else
   if( ! XtIsObject(w) ) return ;
#endif


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

/*-------------------------------------------------------------------*/

void MCW_set_widget_label( Widget w , char * str )
{
   XmString xstr ;
   xstr = XmStringCreateLtoR( str , XmFONTLIST_DEFAULT_TAG ) ;
   XtVaSetValues( w , XmNlabelString , xstr , NULL ) ;
   XmStringFree( xstr ) ;
   MCW_expose_widget( w ) ;
}

/*-----------------------------------------------------------------------*/

void MCW_widget_geom( Widget w, int *wout, int *hout, int *xout, int *yout )
{
   Dimension nx , ny ;  /* don't try to make these ints! */
   Position  xx , yy ;

   if( w == NULL ) return ;

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

   while( XCheckWindowEvent( XtDisplay(w), XtWindow(w) , ev_mask , &evjunk ) ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

char * MCW_hotcolor(Widget w)
{
   static char * redcolor = NULL ;

   if( redcolor == NULL ){
     char * xdef = RWC_getname( (w!=NULL) ? XtDisplay(w) : NULL, "hotcolor" ) ;

     redcolor = (xdef != NULL) ? (xdef) : ("red3") ;
   }
   return redcolor ;
}

/*--------------------------------------------------------------------------*/

#define TIG 25

Widget MCW_action_area( Widget parent, MCW_action_item * action, int num_act )
{
   Widget act_area , ww ;
   int ii ;

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
                  XmNtraversalOn   , False ,
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
      message automatically killed after 30 seconds.
--------------------------------------------------------------------*/

Widget MCW_popup_message( Widget wparent , char * msg , int msg_type )
{
   Widget wmsg , wlab ;
   int wx,hy,xx,yy , xp,yp , scr_width,scr_height , xr,yr , xpr,ypr ;
   Screen * scr ;
   XEvent ev ;

   if( ! XtIsRealized( wparent ) ||
       msg == NULL               || strlen(msg) == 0 ) return NULL ;

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
             "menu" , xmDialogShellWidgetClass , wparent ,
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
                  "menu" , xmLabelWidgetClass , wmsg ,
                     XtVaTypedArg,XmNlabelString,XmRString,msg,strlen(msg)+1,
                     XmNalignment , XmALIGNMENT_BEGINNING ,
                     XmNinitialResourcesPersistent , False ,
                  NULL ) ;
      break ;

      default:
      case MCW_USER_KILL:

         wlab = XtVaCreateManagedWidget(
                  "menu" , xmPushButtonWidgetClass , wmsg ,
                     XtVaTypedArg,XmNlabelString,XmRString,msg,strlen(msg)+1,
                     XmNalignment , XmALIGNMENT_BEGINNING ,
                     XmNinitialResourcesPersistent , False ,
                  NULL ) ;

         XtAddCallback( wlab , XmNactivateCallback , MCW_message_CB , NULL ) ;
      break ;
   }

   XtPopup( wmsg , XtGrabNone ) ;

   /* now wait until the label is exposed, and make sure it appears;
      the reason for this stuff is that this routine is likely to be
      called by a long computation that won't return control to Xt   */

   while( XtWindow(wlab) == (Window) NULL ) ;          /* wait for window  */
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
	                     30000 , MCW_message_timer_CB , wmsg   ) ;

      XtVaSetValues( wlab , XmNuserData ,  tid , NULL );/* put tid on wlab; */
   } else {                                             /* shells don't */
      XtVaSetValues( wlab , XmNuserData , 0 , NULL ) ;  /* have XmNuserData */
   }
#endif

   RWC_visibilize(wmsg) ;  /* 27 Sep 2000 */
   return wmsg ;
}

/*- callback when the popup message created above is a PushButton
    (Note that w is the PushButton widget, so its parent is to be killed) -*/

void MCW_message_CB( Widget w , XtPointer cd , XtPointer cbs )
{
#ifdef ALLOW_TIMER_KILL
   XtIntervalId tid ;

   XtVaGetValues( w , XmNuserData , &tid , NULL ) ;  /* get timer id */
   XtDestroyWidget( XtParent(w) ) ;

   if( tid > 0 ) XtRemoveTimeOut( tid ) ;  /* if a timer exists, kill it */
#else
   XtDestroyWidget( XtParent(w) ) ;
#endif
}

/*--- callback when timer expires on popup message ---*/

void MCW_message_timer_CB( XtPointer client_data , XtIntervalId * id )
{
   XtDestroyWidget( (Widget) client_data ) ;
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

void MCW_alter_widget_cursor( Widget w, int cur,
			      char * fgname, char * bgname )
{
   XColor fg , bg ;
   Cursor ccc ;
   Colormap cmap ;
   Display  * dis ;
   Boolean  good ;
   int ii ;

   static Cursor  cur_font[XC_num_glyphs] ;
   static Boolean first = True ;

   if( first ){
      for( ii=0 ; ii < XC_num_glyphs ; ii++ ) cur_font[ii] = None ;
      first = False ;
   }

   if( !XtIsRealized(w) || XtWindow(w) == (Window) NULL ) return ;

   dis = XtDisplay(w) ;

   if( cur == 0 || cur <= -XC_num_glyphs ){    /* define cursor */
      ccc = None ;
   } else if( cur > 0 ){
      ccc = cur ;
   } else {
      ii = -cur ;
      if( cur_font[ii] == None )
	 cur_font[ii] = XCreateFontCursor( dis , ii ) ;

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
   Display * dis = XtDisplay(w) ;

   if( cur == 0 ){
      cur = XCreateFontCursor( dis , XC_hand2 ) ;
   }

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
void MCW_register_hint( Widget w , char * msg ) { return ; }
void MCW_reghint_children( Widget w , char * msg ) { return ; }
void MCW_hint_toggle(void){ return ; }
#else

#include "LiteClue.h"

static Widget liteClue = NULL ;
static int clueless    = -1 ;

void MCW_hint_toggle(void)
{
#define PBIG 999999
   int period ;
   char * pdef ;

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

#define RES_CONVERT( res_name, res_value) \
       XtVaTypedArg, (res_name), XmRString, (res_value), strlen(res_value) + 1

void MCW_register_hint( Widget w , char * msg )
{
   if( disable_helps ) return ;
   if( w == NULL || msg == NULL || clueless == 1 || !XtIsWidget(w) ) return ;

   if( clueless == -1 ){
      char * hh = my_getenv("AFNI_HINTS") ;
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
      char * cfont ;

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

void MCW_reghint_children( Widget w , char * msg )
{
   Widget * children=NULL ;
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

void MCW_register_help( Widget w , char * msg )
{
   if( disable_helps ) return ;
   if( w == NULL || msg == NULL ) return ;
   XtAddCallback( w , XmNhelpCallback , MCW_help_CB , msg ) ;
   return ;
}

void MCW_reghelp_children( Widget w , char * msg )
{
   Widget * children ;
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
   char * msg         = (char *) client_data ;
   static Widget wpop = NULL , wbut ;
   Position xx,yy ;
   XmString xstr ;
   int ww,hh , sw,sh ;
   char * def ;

#ifndef USE_LOCATE
   XmAnyCallbackStruct * cbs = (XmAnyCallbackStruct *) call_data ;
#endif

   if( w == NULL ){
      if( wpop != NULL ) XtUnmapWidget(wpop) ;
      return ;
   }

   if( wpop == NULL || ! XtIsWidget(wpop) ){

      Widget wpar = w ;

      while( XtParent(wpar) != NULL ) wpar = XtParent(wpar) ;  /* find top */

      wpop = XtVaCreatePopupShell(
              "AFNI" , xmDialogShellWidgetClass , wpar ,
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
      XtPopdown( wpop ) ;

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
      XButtonEvent * xev = (XButtonEvent *) cbs->event ;
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
   XtPopup( wpop , XtGrabNone ) ;
   RWC_visibilize(wpop) ;  /* 27 Sep 2000 */
   return ;
}

/*---------------------------------------------------------------------*/

void MCW_unhelp_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   Widget wpop = (Widget) client_data ;

   XtPopdown(wpop) ;
   return ;
}

/*---------------------------------------------------------------------*/

int MCW_filetype( char * fname )
{
   FILE * fff ;

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

#ifndef DONT_CHECK_FOR_MWM
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
#endif /* DONT_CHECK_FOR_MWM */

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
   Screen * scr ;
   XEvent ev ;
   Position xroot , yroot ;

   if( wparent == NULL || ! XtIsRealized( wparent ) ) return NULL ;

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
               XmNtraversalOn , False ,
               XmNinitialResourcesPersistent , False ,
            NULL ) ;

   XtPopup( wmsg , XtGrabNone ) ;

   return wscal ;
}

void MCW_popdown_meter( Widget wscal )
{
   if( wscal == NULL ) return ;
   XtDestroyWidget( XtParent(wscal) ) ;
   return ;
}

void MCW_set_meter( Widget wscal , int percent )
{
   int val , old ;

   val = percent ;
   if( wscal == NULL || val < 0 || val > 100 ) return ;

   XmScaleGetValue( wscal , &old ) ; if( val == old ) return ;

   XtVaSetValues( wscal , XmNvalue , val , NULL ) ;
#if 0
   XFlush( XtDisplay(wscal) ) ;
#endif
   XmUpdateDisplay(wscal) ;
   return ;
}

/*-----------------------------------------------------------------------*/

#define RONLY_NUM 1
#define EDIT_NUM  2

static MCW_action_item TWIN_act[] = {
 { "Quit" , MCW_textwin_CB , NULL , NULL , "Close window" , 0 } ,
 { "Set"  , MCW_textwin_CB , NULL , NULL , "Apply choice and close window" , 0 }
} ;

MCW_textwin * new_MCW_textwin( Widget wpar , char * msg , int type )
{
   MCW_textwin * tw ;
   int wx,hy,xx,yy , xp,yp , scr_width,scr_height , xr,yr , xpr,ypr , ii,nact ;
   int swid , shi ;
   Position xroot , yroot ;
   Screen * scr ;
   Boolean editable ;
   Arg wa[64] ; int na ;

   /*-- sanity check --*/

   if( ! XtIsRealized(wpar) ) return NULL ;

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

   tw->wshell = XtVaCreatePopupShell(
                 "dialog" , xmDialogShellWidgetClass , wpar ,
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
                "dialog" , xmFormWidgetClass , tw->wshell ,
                  XmNborderWidth , 0 ,
                  XmNborderColor , 0 ,
                  XmNtraversalOn , False ,
                  XmNinitialResourcesPersistent , False ,
                NULL ) ;

   /* create action area */

   editable = (Boolean) (type == TEXT_EDITABLE) ;

   nact = (editable) ? EDIT_NUM : RONLY_NUM ;
   for( ii=0 ; ii < nact ; ii++ ){
     TWIN_act[ii].data     = (XtPointer) tw ;
     TWIN_act[ii].make_red = 0 ;
   }
   TWIN_act[nact-1].make_red = 1 ;

   tw->wactar = MCW_action_area( tw->wtop , TWIN_act , nact ) ;

   /* create text area */

   tw->wscroll = XtVaCreateManagedWidget(
                    "dialog" , xmScrolledWindowWidgetClass , tw->wtop ,
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
                    "dialog" , xmTextWidgetClass , tw->wscroll ,
                       XmNeditMode               , XmMULTI_LINE_EDIT ,
                       XmNautoShowCursorPosition , editable ,
                       XmNeditable               , editable ,
                       XmNcursorPositionVisible  , editable ,
                    NULL ) ;

   if( msg == NULL ) msg = "\0" ;  /* 27 Sep 2000 */

   if( msg != NULL ){
      int cmax = 20 , ll , nlin ;
      char * cpt , *cold , cbuf[128] ;
      XmString xstr ;
      XmFontList xflist ;

      XtVaSetValues( tw->wtext , XmNvalue , msg , NULL ) ;
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
      cmax+=3 ;
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

   XtPopup( tw->wshell , XtGrabNone ) ;

   RWC_visibilize_widget( tw->wshell ) ;  /* 09 Nov 1999 */

   RWC_xineramize( XtDisplay(tw->wshell) ,
                   xpr,ypr,swid,shi , &xpr,&ypr ); /* 27 Sep 2000 */

   XtVaSetValues( tw->wshell, XmNx,xpr , XmNy,ypr , NULL ) ;

   return tw ;
}

void MCW_textwin_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_textwin * tw = (MCW_textwin *) client_data ;
   char * wname     = XtName(w) ;

   if( client_data == NULL ) return ;

   if( strcmp(wname,"Quit") == 0 ){
      XtDestroyWidget( tw->wshell ) ;
      myXtFree( tw ) ;
      return ;
   }

   XBell( XtDisplay(w) , 100 ) ;
   return ;
}

void MCW_textwinkill_CB( Widget w , XtPointer client_data , XtPointer call_data )
{
   MCW_textwin * tw = (MCW_textwin *) client_data ;
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

   if( w == (Widget) NULL ) return 0 ;
   ww = XtWindow(w) ;
   if( ww == (Window) NULL ) return 0 ;

   XGetWindowAttributes( XtDisplay(w) , ww , &wa ) ;

   return ( (wa.map_state == IsViewable) ? 1 : 0 ) ;
}

/*------------------------------------------------------------------
 June 1999: routine to get string constants either from X defaults
            or from Unix environment variables.  Returns a pointer
            to static storage -- do not free()!
--------------------------------------------------------------------*/

#include <ctype.h>

char * RWC_getname( Display * display , char * name )
{
   char * cval , qqq[256] ;
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

   /* try AFNI_NAME */

   strcpy(qqq,"AFNI_") ; nn = strlen(name) ;
   for( ii=0 ; ii < nn ; ii++ ) qqq[ii+5] = toupper(name[ii]) ;
   qqq[nn+5] = '\0' ;
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
   Screen * scr ;

   if( w == NULL || !XtIsWidget(w) ) return ;

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

   return ;
}

/*----------------------------------------------------------------------
  A callback version of the above (for use when menus are mapped, say)
------------------------------------------------------------------------*/

#undef DBGXIN

static void RWC_visibilize_timeout_CB( XtPointer cd , XtIntervalId * id )
{
   Widget w = (Widget) cd ;
#ifdef DBGXIN
fprintf(stderr,"RWC_visibilize_timout_CB\n") ;
#endif
   RWC_visibilize_widget(w) ; return ;
}

void RWC_visibilize_CB( Widget w , XtPointer cd , XtPointer cb )
{
   Widget wpar = w ;
#ifdef DBGXIN
fprintf(stderr,"RWC_visibilize_CB\n") ;
#endif
   while( !XtIsShell(wpar) ){ wpar = XtParent(w); } /* find 1st shell parent */

   /* must wait for the thing to actually appear, dammit */

   (void) XtAppAddTimeOut( XtWidgetToApplicationContext(wpar) ,
                           1 , RWC_visibilize_timeout_CB , wpar ) ;
   return ;
}

/*----------------------------------------------------------------------
   Given a rectangle (xx..xx+ww,yy..yy+hh), return the new origin
   (xn,yn) so that the rectangle (xn..xn+ww,yn..yn+hh) fits onto
   a single Xinerama sub-screen.  If the AFNI.xinerama X11 resource is
   not found, then this routine uses the display size.
   -- RWCox -- 27 Sep 2000
------------------------------------------------------------------------*/

#define BUF 5  /* buffer around the rectangle */

void RWC_xineramize( Display * dpy,
                     int xx, int yy, int ww, int hh, int *xn, int *yn )
{
   static int first=1 ;
   static int nxsi=0 , *xbot,*ybot,*xtop,*ytop ;
   int ii , ss ;

   if( dpy==NULL || xn==NULL || yn==NULL || ww<0 || hh<0 ) return ; /* ERROR */

   /*--- first time in: check AFNI.xinerama X resource
                        load boundaries of sub-screens from resource ---*/

   if( first ){
      char * xdef , * xp ;
      int nn,xorg,yorg,wide,high ;

      first = 0 ;                         /* never again */
      xdef  = getenv( "AFNI_XINERAMA" ) ;

      if( xdef != NULL && (xdef[0] == 'N' || xdef[0] == 'n') ){ /* skip Xinerama */
         nxsi = 0 ;
      } else {
         xdef  = XGetDefault(dpy,"AFNI","xinerama") ; /* get resource */
         if( xdef != NULL ){
            nn = 0 ; sscanf(xdef,"%d%n",&nxsi,&nn) ;  /* number of sub-screens */
            if( nn <= 0 || nxsi <= 1 ){               /* ERROR */
               nxsi = 0 ;
            } else {
               xbot = (int *) malloc(sizeof(int)*nxsi) ; /* make arrays to */
               ybot = (int *) malloc(sizeof(int)*nxsi) ; /* store sub-screen */
               xtop = (int *) malloc(sizeof(int)*nxsi) ; /* coordinate ranges */
               ytop = (int *) malloc(sizeof(int)*nxsi) ;
               xp = xdef + nn ;
               for( ii=0 ; ii < nxsi ; ii++ ){    /* scan for sub-screen info */
                  nn = 0 ;
                  sscanf(xp,"%d%d%d%d%d%n",&ss,&xorg,&yorg,&wide,&high,&nn) ;
                  if( nn <= 0 ) break ;           /* ERROR */
                  xbot[ii] = xorg ; xtop[ii] = xorg+wide ;
                  ybot[ii] = yorg ; ytop[ii] = yorg+high ;
                  xp += nn ;
#ifdef DBGXIN
fprintf(stderr,"RWC_xineramize: xbot=%4d ybot=%4d xtop=%4d ytop=%4d\n",
                xbot[ii],ybot[ii],xtop[ii],ytop[ii] ) ;
#endif
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

#if 0                                          /* doesn't occur anymore */
   if( nxsi == 0 ){ *xn=xx; *yn=yy; return; }  /* not setup?  change nothing */
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

#ifdef DBGXIN
fprintf(stderr,"RWC_xineramize: xx=%d yy=%d ww=%d hh=%d ss=%d\n",xx,yy,ww,hh,ss);
#endif

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
#ifdef DBGXIN
fprintf(stderr,"RWC_xineramize: new ss=%d\n",ss) ;
#endif
   }

   /*--- now adjust position so all of rectangle
         (xx..xx+ww,yy..yy+hh) fits on that screen (if possible) ---*/

   if( xx+ww+BUF >= xtop[ss] ){ xx = xtop[ss]-ww-1-2*BUF; } /* move left */
   if( yy+hh+BUF >= ytop[ss] ){ yy = ytop[ss]-hh-1-2*BUF; } /* move up  */
   if( xx    < xbot[ss]+BUF  ){ xx = xbot[ss]+BUF; }        /* move right */
   if( yy    < ybot[ss]+BUF  ){ yy = ybot[ss]+BUF; }        /* move down */

#ifdef DBGXIN
fprintf(stderr,"RWC_xineramize: new xx=%d yy=%d\n",xx,yy) ;
#endif

   *xn = xx ; *yn = yy ; return ;
}

/*----------  Fix a Linux stupidity  ------------------------------------*/

#ifdef NEED_XSETLOCALE
#include <locale.h>

char * _Xsetlocale( int category, const char * locale)
{ return setlocale(category,locale) ; }
#endif
