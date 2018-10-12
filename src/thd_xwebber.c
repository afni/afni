#include "mrilib.h"
#include "xutil.h"

#ifdef DONT_USE_HTMLWIN  /*-------------------- dummy routines ------------------------------*/

MCW_htmlwin * new_MCW_htmlwin( Widget w, char *m, void_func *kf, XtPointer kd ,
                               MCW_action_items *mai, int nact){ return NULL ; }
void MCW_htmlwin_alter( MCW_htmlwin *hw, char *mmm ){ return ; }

char * convert_text_to_html( char *txt ){ return txt; }  /* 06 May 2015 */

#else                    /*---------- non-dummy routines --------------------*/

#include "XmHTML/XmHTML.h"
#include "debugtrace.h"    /* 12 Mar 2001 */

extern int afni_uses_selenium(void) ;
extern int selenium_open_webpage(char *) ;

XmImageConfig *_xmimage_cfg = NULL ;


static void MCW_htmlwinkill_CB( Widget , XtPointer , XtPointer ) ;
static void MCW_htmlwin_CB    ( Widget , XtPointer , XtPointer ) ;

static MCW_action_item HWIN_act[] = {
 { "Quit" , MCW_htmlwin_CB , NULL , NULL , "Close window" , 0 } ,
} ;

/*----------------------------------------------------------------------------*/
/* Not sure what this is for! */

static void armCB( Widget w, XtPointer arg1, XmAnyCallbackStruct *href_data)
{
   XButtonEvent *event;

   event             = (XButtonEvent*)href_data->event ;
   event->window     = DefaultRootWindow(XtDisplay(w)) ;
   event->root       = DefaultRootWindow(XtDisplay(w)) ;
   event->subwindow  = DefaultRootWindow(XtDisplay(w)) ;
   event->send_event = True ;

   XUngrabPointer( XtDisplay(w) , CurrentTime ) ;
   XSendEvent( XtDisplay(w) , DefaultRootWindow(XtDisplay(w)) ,
               True , ButtonPressMask , (XEvent *)event        ) ;
   XFlush(XtDisplay(w)) ;
}

/*----------------------------------------------------------------------------*/
/* For dealing with clicks on links (anchors). */

static void anchorCB( Widget widget, XtPointer client_data,
                      XmHTMLAnchorCallbackStruct *cbs      )
{
ENTRY("anchorCB") ;

  switch( cbs->url_type ){

    case ANCHOR_JUMP:                                /* internal jumps */
      cbs->doit = True ; cbs->visited = True ;
    break ;

    default:
    case ANCHOR_HTTP:{                               /* external http links */
      static char *webb=NULL ; static int first=1 ;
      if( first == 1 ){ webb = GetAfniWebBrowser() ; first = 2 ; }
      if( afni_uses_selenium() ) {
         selenium_open_webpage(cbs->href);
      }
      else{
         if( webb != NULL ){
           char *cmd = (char *)malloc( strlen(webb) + strlen(cbs->href) + 32 ) ;
           sprintf( cmd , "%s '%s' &" , webb , cbs->href ) ;
           system( cmd ) ; free( cmd ) ;
         } else if( first == 2 ){
             INFO_message("No command line Web browser program found in your path.") ;
           ININFO_message("Set environment variable AFNI_WEB_BROWSER to the full"  ) ;
           ININFO_message("pathname of a browser than can be started from the Unix") ;
           ININFO_message("command line -- e.g., '/usr/local/bin/mozilla'"         ) ;
           first = 0 ;
         }
      }
    }
    break ;
  }

  EXRETURN ;
}

/*----------------------------------------------------------------------------*/

#undef  SSUB
#define SSUB(a,b)                                        \
 do{ char *qqq = string_substitute( mmm , (a) , (b) ) ;  \
     if( qqq != NULL ){ free(mmm) ; mmm = qqq ; }        \
 } while(0)

#undef  NSUB
#define NSUB(a) SSUB((a),"\0")

#undef  UOSUB
#undef  UXSUB
#define UOSUB(a) SSUB((a),"<u>")
#define UXSUB(a) SSUB((a),"</u>")

static char * unfontize( char *msg )
{
   char *mmm , *qqq ;

   if( msg == NULL || *msg == '\0' ) return msg ;

   mmm = strdup(msg) ;
   NSUB("<small>"); NSUB("</small>");
   UOSUB("<big>") ; UXSUB("</big>") ;
   UOSUB("<h1>")  ; UXSUB("</h1>")  ;
   UOSUB("<h2>")  ; UXSUB("</h2>")  ;
   UOSUB("<h3>")  ; UXSUB("</h3>")  ;
   UOSUB("<h4>")  ; UXSUB("</h4>")  ;
   UOSUB("<h5>")  ; UXSUB("</h5>")  ;
   UOSUB("<h6>")  ; UXSUB("</h6>")  ;
   UOSUB("<i>")   ; UXSUB("</i>")   ;
   UOSUB("<b>")   ; UXSUB("</b>")   ;
   UOSUB("<em>")  ; UXSUB("</em>")  ;
   UOSUB("<tt>")  ; UXSUB("</tt>")  ;
   SSUB("<font ","<u ") ; UXSUB("</font>") ;
   SSUB("&nbsp;",".") ;

   if( strcmp(mmm,msg) == 0 ){ free(mmm) ; mmm = msg ; }
   return mmm ;
}

/*----------------------------------------------------------------------------*/
/* Mangle a message to be good HTML for display */

static char * htmlize( char *msg )
{
   char *mmm=NULL ; int dounf ;

ENTRY("htmlize") ;

   if( msg == NULL || *msg == '\0' ){
     msg = strdup("<html><body><p>Dummy\n<p>Message</body></html>") ;
     RETURN(msg) ;
   }

   if( strncmp(msg,"<html>",6) == 0 ) RETURN(msg) ;     /* already HTML format */

   if( strncmp(msg,"file:",5) == 0 ){      /* read file */
     char *qqq=AFNI_suck_file(msg+5) ; char *dnam , *repl , *targ ;
     if( qqq != NULL )
       mmm = qqq ;
     else
       mmm = strdup("<html><body><h1>Dummy</h1><h2>Message</h2></body></html>");

     /* edit file to add base directory to all '<img src=' filenames */

     if( strchr(msg+5,'/') != NULL && strstr(mmm,"<img src=") != NULL ){
       dnam = strdup(msg+5) ; qqq  = THD_trailname(dnam,0) ;
       if( qqq != NULL && qqq != dnam ){
         *qqq = '\0' ;                          /* dnam is now base directory */
         repl = (char *)malloc(sizeof(char)*(strlen(dnam)+16)) ;
         targ = "<img src=\"" ;                       /* string to search for */
         sprintf( repl , "%s%s" , targ , dnam ) ;       /* replacement string */
         qqq = string_substitute( mmm , targ , repl ) ;        /* do the work */
         if( qqq != NULL ){ free(mmm) ; mmm = qqq ; }
         free(repl) ;
       }
       free(dnam) ;
     }

   } else if( strncmp(msg,"wami:",5) == 0 ){      /* wami love */
      mmm = (char *)malloc(sizeof(char)*(strlen(msg)+64)) ;
      strcpy(mmm,"<html><body>\n") ;
      strcat(mmm,msg+strlen("wami:")) ;
      strcat(mmm,"\n</body></html>") ;
   } else {                                                 /* add HTML stuff */
     mmm = (char *)malloc(sizeof(char)*(strlen(msg)+64)) ;
     strcpy(mmm,"<html><body>\n") ;
     strcat(mmm,msg) ;
     strcat(mmm,"\n</body></html>") ;
   }

#ifdef UNFONTIZE_HTMLWIN
   dounf = 1 ;
#else
   dounf = AFNI_yesenv("AFNI_UNFONTIZE_HTML") ;
#endif
   if( dounf ){
     char *qqq = unfontize(mmm) ;
     if( qqq != mmm ){ free(mmm) ; mmm = qqq ; }
   }

   RETURN(mmm) ;
}

/*----------------------------------------------------------------------------*/
/* This is a callback to deal with some refresh problems
   that should have been handled by the XmHTML library.
   For now this call is not needed. It looks like the patching
   of XmHTML did the trick ZSS March 2012 */

void RefreshHTML_AtEvent( Widget w , XtPointer client_data ,
                  XEvent * ev , Boolean * continue_to_dispatch )
{
#if 0
    XmHTMLRefresh(client_data);
#endif
}

/*----------------------------------------------------------------------------*/
/* Open a window with an XmHTML widget containing msg.
   If msg starts with "file:", then it indicates a file to read and display.
   Otherwise, it is the content of the page directly.
*//*--------------------------------------------------------------------------*/

MCW_htmlwin * new_MCW_htmlwin( Widget wpar , char *msg ,
                               void_func *kill_func , XtPointer kill_data,
                               MCW_action_item *umai, int nact)

{
   int wx,hy,xx,yy , xp,yp , scr_width,scr_height , xr,yr , xpr,ypr , ii ;
   int swid , shi ;
   Position xroot , yroot ;
   Screen *scr ;
   Arg wa[64] ; int na ; Widget ws ;
   char *wtype = "help" ;
   MCW_htmlwin *hw ;
   char *mymsg ;
   MCW_action_item *mai=NULL;
   static Pixel afg=(Pixel)0 , afgv=(Pixel)0 ;

ENTRY("new_MCW_htmlwin") ;

   /*-- sanity check --*/

   if( wpar == NULL || !XtIsRealized(wpar) || msg == NULL || *msg == '\0' )
     RETURN(NULL) ;


   /*-- set position based on parent and screen geometry --*/

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

   /*-- create a popup shell --*/

   hw = myXtNew(MCW_htmlwin) ;
   hw->kill_func = kill_func ;
   hw->kill_data = kill_data ;

   hw->wshell = XtVaCreatePopupShell(
                 wtype , xmDialogShellWidgetClass , wpar ,
                    XmNx , xpr ,
                    XmNy , ypr ,
                    XmNborderWidth , 0 ,
                    XmNborderColor , 0 ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;

   XmAddWMProtocolCallback(
        hw->wshell ,
        XmInternAtom( XtDisplay(hw->wshell) , "WM_DELETE_WINDOW" , False ) ,
        MCW_htmlwinkill_CB , (XtPointer)hw ) ;

   /*-- create a form to hold everything else --*/

   hw->wtop = XtVaCreateWidget(
                wtype , xmFormWidgetClass , hw->wshell ,
                  XmNborderWidth , 0 ,
                  XmNborderColor , 0 ,
                  XmNtraversalOn , True  ,
                  XmNinitialResourcesPersistent , False ,
                NULL ) ;

   /*-- create action area --*/

   if (!umai) { /* default action item */
      mai = HWIN_act; nact = 1;
      for( ii=0 ; ii < nact ; ii++ ){
        mai[ii].data     = (XtPointer)hw ;
        mai[ii].make_red = 0 ;
      }
      mai[nact-1].make_red = 1 ;
   } else {
      /* use user preference */
      mai = umai;
   }

   hw->wactar = MCW_action_area( hw->wtop , mai , nact ) ;

   XtVaSetValues( hw->wactar ,
                     XmNleftAttachment , XmATTACH_FORM ,
                     XmNrightAttachment, XmATTACH_FORM ,
                     XmNtopAttachment  , XmATTACH_FORM ,
                     XmNtopOffset      , 4 ,
                  NULL ) ;

   /*-- frame to hold HTML widget --*/

   hw->wframe = XtVaCreateManagedWidget(
                  wtype , xmFrameWidgetClass, hw->wtop ,
                  XmNtopAttachment   , XmATTACH_WIDGET ,
                  XmNtopWidget       , hw->wactar ,
                  XmNtopOffset       , 5 ,
                  XmNleftAttachment  , XmATTACH_FORM,
                  XmNleftOffset      , 2 ,
                  XmNbottomAttachment, XmATTACH_FORM,
                  XmNbottomOffset    , 2 ,
                  XmNrightAttachment , XmATTACH_FORM,
                  XmNrightOffset     , 2,
                  XmNshadowType      , XmSHADOW_IN,
                  XmNshadowThickness , 5 ,
                NULL ) ;

   /*---- create HTML area ----*/

   if( afg == (Pixel)0 ){
     afg  = XmHTMLAllocColor( hw->wtop, "#ffdd00", WhitePixelOfScreen(XtScreen(hw->wtop)) ) ;
     afgv = XmHTMLAllocColor( hw->wtop, "#ffcc99", WhitePixelOfScreen(XtScreen(hw->wtop)) ) ;
   }

   swid = WidthOfScreen(XtScreen(wpar))  - 222 ; if( swid > 799 ) swid = 799 ;
   shi  = HeightOfScreen(XtScreen(wpar)) - 222 ; if( shi  > 899 ) shi  = 899 ;

   mymsg = htmlize(msg) ;  /* edit the text */

STATUS("create HTML widget") ;

   hw->whtml = XtVaCreateManagedWidget(
                  wtype , xmHTMLWidgetClass , hw->wframe ,
                  XmNmarginWidth       , 8 ,
                  XmNmarginHeight      , 8 ,
                  XmNwidth             , swid ,
                  XmNheight            , shi ,
                  XmNvalue             , mymsg,
                  XmNfontFamily        , "adobe-helvetica-normal-*" ,
                  XmNfontFamilyFixed   , "adobe-courier-normal-*" ,
                  XmNfontSizeFixedList , "14,10" ,
                  XmNanchorButtons     , False ,
                  XmNanchorForeground        , afg  ,
                  XmNanchorVisitedForeground , afgv ,
                NULL ) ;
   XtAddCallback( hw->whtml, XmNactivateCallback, (XtCallbackProc)anchorCB, NULL ) ;
   XtAddCallback( hw->whtml, XmNarmCallback     , (XtCallbackProc)armCB   , NULL ) ;

#if 0 /* This was needed to deal with some refreshing problems when the scrollbar
         was moved. The patch in XmHTML seems to have do the trick. These are
         left here should we need to reuse them someday */
   XtInsertEventHandler( hw->whtml ,        /* notify when */
                         LeaveWindowMask ,  /* pointer leaves */
                         FALSE ,            /* this window */
                         RefreshHTML_AtEvent,
                         (XtPointer) hw->whtml ,
                         XtListTail ) ;     /* last in queue */
   XtInsertEventHandler( hw->whtml ,        /* notify when */
                         EnterWindowMask ,  /* pointer leaves */
                         FALSE ,            /* this window */
                         RefreshHTML_AtEvent,
                         (XtPointer) hw->whtml ,
                         XtListTail ) ;     /* last in queue */
#endif

STATUS("manage HTML widgets") ;

   XtManageChild( hw->wtop ) ;

#if 0
   XtVaSetValues( hw->wshell , XmNwidth,swid , XmNheight,shi , NULL ) ;
#endif

   /*--- open the window for viewing, and place it on the screen ---*/

   XtPopup( hw->wshell , XtGrabNone ) ; RWC_sleep(16) ;

   RWC_visibilize_widget( hw->wshell ) ;

   RWC_xineramize( XtDisplay(hw->wshell) ,
                   xpr,ypr,swid,shi , &xpr,&ypr ) ;

   XtVaSetValues( hw->wshell, XmNx,xpr , XmNy,ypr , NULL ) ;

   hw->shell_width = swid ; hw->shell_height = shi ;

   NORMAL_cursorize( hw->wshell ) ;

#if 0
   XmHTMLTextSetString( hw->whtml , mymsg ) ;
#endif

   if( mymsg != msg ) free(mymsg) ;               /* toss the trash */

STATUS("force HTML redisplay") ;

   RWC_sleep(66) ; XmHTMLRedisplay( hw->whtml ) ; /* force redraw to be safe */

   RETURN(hw) ;
}

/*-------------------------------------------------------------------------*/
/* replace the contents of an MCW_htmlwin (not tested) */

void MCW_htmlwin_alter( MCW_htmlwin *hw , char *mmm )
{
   int swid , shi ;
   char *msg ;

ENTRY("MCW_htmlwin_alter") ;

   if( hw == NULL || mmm == NULL || *mmm == '\0' ) EXRETURN ;
   msg = htmlize(mmm) ;

   XmHTMLTextSetString( hw->whtml , msg ) ;

   if( msg != mmm ) free(msg) ;

   EXRETURN ;
}

/*-------------------------------------------------------------------------*/
/* Called when the user presses an action button */

static void MCW_htmlwin_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_htmlwin *hw = (MCW_htmlwin *)client_data ;
   char *wname     = XtName(w) ;

ENTRY("MCW_htmlwin_CB") ;

   if( client_data == NULL ) EXRETURN ;

   if( strcmp(wname,"Quit") == 0 ){
      if( hw->kill_func != NULL )
        AFNI_CALL_VOID_1ARG( hw->kill_func , XtPointer , hw->kill_data ) ;
      XtDestroyWidget( hw->wshell ) ;
      myXtFree( hw ) ;
      EXRETURN ;
   }

   EXRETURN ;
}

/*-------------------------------------------------------------------------*/
/* Called when the user kills the window via the window manager controls */

static void MCW_htmlwinkill_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_htmlwin *hw = (MCW_htmlwin *) client_data ;

ENTRY("MCW_htmlwinkill_CB") ;

   if( hw->kill_func != NULL )
     AFNI_CALL_VOID_1ARG( hw->kill_func , XtPointer , hw->kill_data ) ;
   XtDestroyWidget( hw->wshell ) ;
   myXtFree( hw ) ;
   EXRETURN ;
}

/*--------------------------------------------------------------------*/

#define HTTP_check(str) ( strncmp((str),"http://",7) == 0  &&              \
                          !isspace((str)[7])               &&              \
                          !iscntrl((str)[7])               &&              \
                          (str)[7] != '\0'                 &&              \
                          (str)[7] != '*'                  &&              \
                          (str)[7] != '.'                     )

#define HTTPS_check(str) ( strncmp((str),"https://",8) == 0  &&            \
                           !isspace((str)[8])                &&            \
                           !iscntrl((str)[8])                &&            \
                           (str)[8] != '\0'                  &&            \
                           (str)[8] != '*'                   &&            \
                           (str)[8] != '.'                     )

#define CHK4(abcd)                                                         \
  ( tolower(buf[hend-4])==abcd[0] && tolower(buf[hend-3])==abcd[1] &&      \
    tolower(buf[hend-2])==abcd[2] && tolower(buf[hend-1])==abcd[3]   )

#define EMIT_char(ch)                                                      \
 do{ if( itout == atout ){                                                 \
       atout = (int)(1.5f*atout+1024); tout = (char *)realloc(tout,atout); \
     }                                                                     \
     tout[itout++] = (ch) ;                                                \
 } while(0)

#define EMIT_string(cs)                                                    \
 do{ char *cq ;                                                            \
     for( cq=cs ; *cq != '\0' ; cq++ ) EMIT_char(*cq) ;                    \
 } while(0)

/*----------------------------------------------------------------------------*/
/* Convert plain text to HTML, inserting links and a few other miscellany.
   free() the returned string when done, if you don't mind.  [06 May 2015]
*//*--------------------------------------------------------------------------*/

char * convert_text_to_html( char *txt )
{
   char *tout=NULL , tbuf[2048] , *tin=txt , cc ;
   int  itout=0 , atout=0 ;

ENTRY("convert_text_to_html") ;

   if( txt == NULL || *txt == '\0' ) RETURN(tout) ; /* bad input */

   atout = strlen(txt)+1024 ;  /* size of output buffer */
    tout = (char *)malloc(atout) ; itout = 0 ;

   EMIT_string("<html>\n"
               "<head>\n"
               "<title>AFNI Papers</title>\n"
               "</head>\n"
               "<body>\n<br />\n" ) ;

#if 0
   EMIT_string("<center><img src=\"afnigui_logo.jpg\" align=middle></center>\n") ;
#endif

   while( *tin != '\0' ){

#if 0
     /* some HTML already? just pass it thru */

     if( *tin == '<' && ( isalpha(*(tin+1)) || *(tin+1) == '/') ){  /* "<something...>" */
       do{
         EMIT_char(*tin) ; tin++ ;
       } while( *tin != '>' && *tin != '\0' ) ;
       if( *tin == '>' ){ EMIT_char(*tin) ; tin++ ; }
       continue ;
     }
#endif

     /* link to a web page? "http://something" */

     if( HTTP_check(tin) ){
       int hend , ii ;

       /* scan forward to get to end of 'http://something' string at hend */

       for( hend=7 ; tin[hend] != '\0' && !isspace(tin[hend]) ; hend++ ) ; /*nada*/

       /* insert hyperlink here*/

       EMIT_string("<a href=\"") ;
       for( ii=0 ; ii < hend ; ii++ )  EMIT_char(tin[ii]);
       EMIT_string("\">") ;
       for( ii=0 ; ii < hend ; ii++ )  EMIT_char(tin[ii]);
       EMIT_string("</a>") ;

       tin += hend ; continue ;
     }

     /* link to a web page? "https://something" [08 Apr 2016] */

     if( HTTPS_check(tin) ){
       int hend , ii ;

       /* scan forward to get to end of 'https://something' string at hend */

       for( hend=8 ; tin[hend] != '\0' && !isspace(tin[hend]) ; hend++ ) ; /*nada*/

       /* insert hyperlink here*/

       EMIT_string("<a href=\"") ;
       for( ii=0 ; ii < hend ; ii++ )  EMIT_char(tin[ii]);
       EMIT_string("\">") ;
       for( ii=0 ; ii < hend ; ii++ )  EMIT_char(tin[ii]);
       EMIT_string("</a>") ;

       tin += hend ; continue ;
     }

     /* a single normal character */

     cc = *tin ;
          if( cc == ' ' ) EMIT_string("&nbsp;")   ;   /* put in HTML escapes */
     else if( cc == '&' ) EMIT_string("&amp;")    ;   /* for special cases */
     else if( cc == '<' ) EMIT_string("&lt;")     ;
     else if( cc == '>' ) EMIT_string("&gt;")     ;
     else if( cc == '\n') EMIT_string("<br />\n") ;
     else                 EMIT_char  (cc)         ;   /* perfectly normal character */
     tin++ ;
   }

   EMIT_string("\n</body></html>\n") ;
   EMIT_char('\0') ;
   RETURN(tout) ;
}

#endif /* DONT_USE_HTMLWIN */
