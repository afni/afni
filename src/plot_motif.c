#include "coxplot.h"
#include <Xm/XmAll.h>

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

static char print_command[256] = "\0" ;
static char * redcolor = NULL ;

#ifndef LABEL_ARG
#define LABEL_ARG(str) \
  XtVaTypedArg , XmNlabelString , XmRString , (str) , strlen(str)+1
#endif

#ifndef BGCOLOR_ARG
#define BGCOLOR_ARG(str) \
  XtVaTypedArg , XmNbackground , XmRString , (str) , strlen(str)+1
#endif

#ifndef HOTCOLOR
#define HOTCOLOR(ww,ss)                                                        \
  { char * xdef = XGetDefault(XtDisplay(ww),"AFNI","hotcolor") ;               \
    if( xdef == NULL ) xdef = getenv("AFNI_hotcolor") ;                        \
    if( xdef == NULL ) xdef = getenv("AFNI_HOTCOLOR") ;                        \
    if( xdef == NULL ) xdef = XGetDefault(XtDisplay(ww),"AFNI","background") ; \
    (ss) = (xdef != NULL) ? (xdef) : ("gray40") ; }
#endif

/*--------------------------------------------------------------------------
  Callback routines for memplot_to_topshell
----------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------*/

static void beep_CB( Widget w , XtPointer cd , XtPointer cb )
{
   char * str = (char *) cd ;

   if( w != NULL ) XBell(XtDisplay(w),100) ;
   if( str != NULL ) fprintf(stderr,"%s\a\n",str) ;
   return ;
}

/*--------------------------------------------------------------------------
   Save plot to a PostScript file
----------------------------------------------------------------------------*/

/****** cancellation of a print request ******/

static void pscancel_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MEM_topshell_data * mpcb = (MEM_topshell_data *) cd ;

   if( mpcb == NULL || ! MTD_VALID(mpcb) ) return ;

   if( mpcb->dial != NULL ) XtDestroyWidget( mpcb->dial ) ;
   mpcb->dial = mpcb->wtf = NULL ;
   return ;
}

/****** finalization of a print request ******/

static void psfinalize_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MEM_topshell_data * mpcb = (MEM_topshell_data *) cd ;
   char * text , fname[64] ;
   int ii , ll ;

   if( mpcb == NULL || ! MTD_VALID(mpcb) ) return ;

   if( mpcb->dial == NULL ){ XBell(XtDisplay(w),100) ; return ; }

   text = XmTextFieldGetString( mpcb->wtf ) ;
   if( text == NULL || text[0] == '\0' ){ XBell(XtDisplay(w),100) ; return ; }

   ll = strlen(text) ;
   for( ii=0 ; ii < ll ; ii++ ){
      if( iscntrl(text[ii]) || isspace(text[ii]) ||
          text[ii] == '/'   || text[ii] == ';'   ||
          text[ii] == '*'   || text[ii] == '?'   ||
          text[ii] == '&'   || text[ii] == '|'   ||
          text[ii] == '"'   || text[ii] == '>'   ||
          text[ii] == '<'   || text[ii] == '\''  ||
          text[ii] == '['   || text[ii] == ']'     ){

        XBell(XtDisplay(w),100) ; return ;
      }
   }

   strcpy(fname,text) ;
   if( !(ll > 2 && text[ll-2] == 'p' && text[ll-1] == 's' ) )
      strcat(fname,".ps") ;

   memplot_to_postscript( fname , mpcb->mp ) ;

   XtDestroyWidget( mpcb->dial ) ;
   mpcb->dial = mpcb->wtf = NULL ;
   return ;
}

/****** initiation of a print request ******/

static void psfile_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MEM_topshell_data * mpcb = (MEM_topshell_data *) cd ;
   Widget wpop , wrc , wlab , wtf , form , but0 , but1 ;
   int ibut = 0 ;
   Position xx,yy ;

   if( mpcb == NULL || ! MTD_VALID(mpcb) ) return ;

   if( mpcb->dial != NULL ){ XBell(XtDisplay(w),100) ; return ; }

   mpcb->dial = wpop = XtVaCreatePopupShell(
             "AFNI" , xmDialogShellWidgetClass , mpcb->top ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   XtVaSetValues( wpop ,
                     XmNmwmDecorations , MWM_DECOR_BORDER ,
                     XmNmwmFunctions   ,  MWM_FUNC_MOVE ,
                  NULL ) ;

   wrc  = XtVaCreateWidget(
             "menu" , xmRowColumnWidgetClass , wpop ,
                XmNpacking      , XmPACK_TIGHT ,
                XmNorientation  , XmVERTICAL ,
                XmNtraversalOn , False ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   wlab = XtVaCreateManagedWidget(
             "menu" , xmLabelWidgetClass , wrc ,
                LABEL_ARG("PostScript filename:") ,
                XmNinitialResourcesPersistent , False ,
             NULL ) ;

   mpcb->wtf = wtf = XtVaCreateManagedWidget(
             "menu" , xmTextFieldWidgetClass , wrc ,
                 XmNcolumns         , 20 ,
                 XmNeditable        , True ,
                 XmNmaxLength       , 32 ,
                 XmNresizeWidth     , False ,
                 XmNmarginHeight    , 1 ,
                 XmNmarginWidth     , 1 ,
                 XmNcursorPositionVisible , True ,
                 XmNblinkRate , 0 ,
                 XmNautoShowCursorPosition , True ,
                 XmNinitialResourcesPersistent , False ,
                 XmNtraversalOn , False ,
              NULL ) ;
   XtAddCallback( wtf, XmNactivateCallback, psfinalize_CB, cd ) ; /* return key */

#undef TIG
#undef NBUT
#define TIG  20
#define NBUT 2

   form = XtVaCreateWidget( "menu" , xmFormWidgetClass , wrc ,
                               XmNborderWidth , 0 ,
                               XmNfractionBase , TIG*NBUT - 1 ,
                               XmNinitialResourcesPersistent , False ,
                            NULL ) ;

   ibut = 0 ;
   but0 = XtVaCreateManagedWidget(
                 "menu" , xmPushButtonWidgetClass , form ,
                    LABEL_ARG("Cancel") ,
                    XmNtopAttachment  , XmATTACH_FORM ,

                    XmNleftAttachment   ,
                        (ibut!=0) ? XmATTACH_POSITION : XmATTACH_FORM ,
                    XmNleftPosition , ibut*TIG ,

                    XmNrightAttachment  ,
                     (ibut==NBUT-1) ? XmATTACH_FORM : XmATTACH_POSITION ,
                    XmNrightPosition , ibut*TIG + (TIG-1) ,

                    XmNrecomputeSize , False ,
                    XmNtraversalOn   , False ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   XtAddCallback( but0 , XmNactivateCallback , pscancel_CB , cd ) ;


   if( redcolor == NULL ){ HOTCOLOR(form,redcolor) ; }
   ibut++ ;
   but1 = XtVaCreateManagedWidget(
                 "menu" , xmPushButtonWidgetClass , form ,
                    LABEL_ARG("Save") ,
#if 1
                    BGCOLOR_ARG(redcolor) ,
#endif

                    XmNtopAttachment  , XmATTACH_FORM ,

                    XmNleftAttachment   ,
                        (ibut!=0) ? XmATTACH_POSITION : XmATTACH_FORM ,
                    XmNleftPosition , ibut*TIG ,

                    XmNrightAttachment  ,
                     (ibut==NBUT-1) ? XmATTACH_FORM : XmATTACH_POSITION ,
                    XmNrightPosition , ibut*TIG + (TIG-1) ,

                    XmNrecomputeSize , False ,
                    XmNtraversalOn   , False ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   XtAddCallback( but1 , XmNactivateCallback , psfinalize_CB , cd ) ;

   XtTranslateCoords( mpcb->top , 15,15 , &xx , &yy ) ;
   XtVaSetValues( wpop , XmNx , (int) xx , XmNy , (int) yy , NULL ) ;

   XtManageChild( form ) ;
   XtManageChild( wrc ) ;
   XtPopup( wpop , XtGrabNone ) ;
   return ;
}

/*--------------------------------------------------------------------------
   Print plot to a PostScript printer (if possible)
----------------------------------------------------------------------------*/

static void psprint_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MEM_topshell_data * mpcb = (MEM_topshell_data *) cd ;
   MEM_plotdata * mp ;

   if( mpcb == NULL ) return ;
   mp = mpcb->mp ; if( mp == NULL ) return ;
   memplot_to_postscript( print_command , mp ) ;
   return ;
}

/*--------------------------------------------------------------------------
   Close plot window
----------------------------------------------------------------------------*/

static void donebut_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MEM_topshell_data * mpcb = (MEM_topshell_data *) cd ;

   if( mpcb == NULL || ! MTD_VALID(mpcb) ) return ;

   mpcb->valid = 0 ;

   if( mpcb->killfunc != NULL ) mpcb->killfunc( mpcb ) ;

   if( mpcb->dial != NULL ) XtDestroyWidget( mpcb->dial ) ;
#ifdef HAVE_XDBE
   if( mpcb->have_xdbe )
      XdbeDeallocateBackBufferName( XtDisplay(mpcb->top) , mpcb->buf_xdbe ) ;
#endif
   XtDestroyWidget( mpcb->top ) ;
   delete_memplot( mpcb->mp ) ;
   free(mpcb) ;

   return ;
}

/*--------------------------------------------------------------------------
   Draw plot window
----------------------------------------------------------------------------*/

static void expose_CB( Widget w , XtPointer cd , XtPointer cb )
{
   XmDrawingAreaCallbackStruct * cbs = (XmDrawingAreaCallbackStruct *) cb ;
   MEM_topshell_data * mpcb = (MEM_topshell_data *) cd ;
   MEM_plotdata * mp ;
   XEvent evjunk ;
   Display * dpy = XtDisplay(w) ;
   Window  win   = XtWindow(w) ;
   Drawable dw   = win ;                               /* draw into this */

   if( win == (Window) 0 ) return ;  /* no window yet? */
   if( mpcb == NULL ) return ;
   mp = mpcb->mp ; if( mp == NULL ) return ;

   if( cbs != NULL ){
      XExposeEvent * ev = (XExposeEvent *) cbs->event ;
      if( ev->count > 0 ) return ;
   }

#ifdef HAVE_XDBE
   if( use_xdbe > 0 && mpcb->have_xdbe == 0 ){
      XdbeSwapInfo info_xdbe ;

      mpcb->buf_xdbe  = XdbeAllocateBackBufferName( dpy,win,XdbeBackground );
      mpcb->have_xdbe = 1 ;

      set_X11_background( dpy , win , 255,255,255 ) ;
      info_xdbe.swap_window = win ;
      info_xdbe.swap_action = XdbeBackground ;
      XdbeSwapBuffers( dpy , &info_xdbe , 1 ) ;
   }

   if( mpcb->have_xdbe ) dw = mpcb->buf_xdbe ;         /* draw into this */
#endif

   set_X11_background( dpy , win , 255,255,255 ) ;
   if( dw == win ) XClearWindow( dpy , win ) ;
   memplot_to_X11_sef( dpy , dw , mp , 0,0,1 ) ;

#ifdef HAVE_XDBE
   if( mpcb->have_xdbe ){
      XdbeSwapInfo info_xdbe ;
      info_xdbe.swap_window = win ;
      info_xdbe.swap_action = XdbeBackground ;
      XdbeSwapBuffers( dpy , &info_xdbe , 1 ) ;
    }
#endif

   while( XCheckWindowEvent(dpy, win ,
                            ExposureMask|StructureNotifyMask,&evjunk) ) ;

   return ;
}

void redraw_topshell( MEM_topshell_data * mpcb )
{
   if( mpcb == NULL ) return ;
   expose_CB( mpcb->drawing , mpcb , NULL ) ;
   return ;
}

/*--------------------------------------------------------------------------
   Redraw plot window after a resize notice
----------------------------------------------------------------------------*/

static void resize_CB( Widget w , XtPointer cd , XtPointer cb )
{
   expose_CB( w , cd , NULL ) ;
   return ;
}

/*------------------------------------------------------------------
   External killer, for use by user routines
--------------------------------------------------------------------*/

void plotkill_topshell( MEM_topshell_data * mpcb )
{
   if( mpcb == NULL || ! MTD_VALID(mpcb) ) return ;

   donebut_CB( NULL , (XtPointer) mpcb , NULL ) ;
   return ;
}

/*------------------------------------------------------------------
   Make a toplevel widget and put an existing plot into it.

   If kfun is not NULL, when the user closes the window, it
   will be called as in
        kfun(mpcb) ;
   where mpcb is the pointer returned by this function.
   After this has been done, the memory used will be destroyed,
   including all the contents of mp and mpcb.

   The user may attach extra data to the void * pointer
   mpcb->userdata after this function returns mpcb.  If this
   data involves the use of malloc, it is the user's responsibility
   to free it in the call to kfun.
--------------------------------------------------------------------*/

MEM_topshell_data * memplot_to_topshell( Display * dpy,
                                         MEM_plotdata * mp, void_func * kfun )
{
   Widget topshell , drawing , donebut , form , psfilebut , psprintbut ;
   MEM_topshell_data * mpcb ;
   int hmin=200 , wmin , ibut=0 ;
   char * prc ;

   /* sanity check */

   if( dpy == NULL || mp == NULL ) return NULL ;

   mpcb = (MEM_topshell_data *) malloc( sizeof(MEM_topshell_data) ) ;
   mpcb->valid = 0 ;

#ifdef HAVE_XDBE
   init_XDBE(dpy) ; mpcb->have_xdbe = 0 ;
#endif

   wmin = MEMPLOT_ASPECT(mp) * hmin ;

   /* shell to hold it all */

   topshell = XtVaAppCreateShell(
                 "AFNI" , "AFNI" , topLevelShellWidgetClass , dpy ,
                   XmNborderWidth ,   0  ,
                   XmNminHeight   , hmin , XmNheight , hmin ,
                   XmNminWidth    , wmin , XmNwidth  , wmin ,
                   XmNallowShellResize , False ,
                   XmNinitialResourcesPersistent , False ,
                   XmNdeleteResponse   , XmDO_NOTHING ,   /* deletion handled below */
                 NULL ) ;

   XmAddWMProtocolCallback(
        topshell , XmInternAtom(dpy,"WM_DELETE_WINDOW",False) ,
        donebut_CB , (XtPointer) mpcb ) ;

   mpcb->top = topshell ;
   mpcb->mp  = mp ;
   mpcb->dial= NULL ;
   mpcb->wtf = NULL ;

   mpcb->killfunc = kfun ;

   /* form to manage it all */

#undef TIG
#undef NBUT
#define TIG  20
#define NBUT 3

   mpcb->form = form =
        XtVaCreateWidget( "dialog" , xmFormWidgetClass , topshell ,
                             XmNborderWidth , 0 ,
                             XmNfractionBase , TIG*NBUT - 1 ,
                             XmNinitialResourcesPersistent , False ,
                          NULL ) ;

   /* buttons across the top */

   if( redcolor == NULL ){ HOTCOLOR(form,redcolor) ; }

   ibut = 0 ;
   psfilebut = XtVaCreateManagedWidget(
                 "dialog" , xmPushButtonWidgetClass , form ,
                    LABEL_ARG("PS->file") ,
                    XmNtopAttachment  , XmATTACH_FORM ,

                    XmNleftAttachment   ,
                        (ibut!=0) ? XmATTACH_POSITION : XmATTACH_FORM ,
                    XmNleftPosition , ibut*TIG ,

                    XmNrightAttachment  ,
                     (ibut==NBUT-1) ? XmATTACH_FORM : XmATTACH_POSITION ,
                    XmNrightPosition , ibut*TIG + (TIG-1) ,

                    XmNrecomputeSize , False ,
                    XmNtraversalOn   , False ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   XtAddCallback( psfilebut , XmNactivateCallback , psfile_CB , (XtPointer) mpcb ) ;

   ibut++ ;
   psprintbut = XtVaCreateManagedWidget(
                 "dialog" , xmPushButtonWidgetClass , form ,
                    LABEL_ARG("->printer") ,
                    XmNtopAttachment  , XmATTACH_FORM ,

                    XmNleftAttachment   ,
                        (ibut!=0) ? XmATTACH_POSITION : XmATTACH_FORM ,
                    XmNleftPosition , ibut*TIG ,

                    XmNrightAttachment  ,
                     (ibut==NBUT-1) ? XmATTACH_FORM : XmATTACH_POSITION ,
                    XmNrightPosition , ibut*TIG + (TIG-1) ,

                    XmNrecomputeSize , False ,
                    XmNtraversalOn   , False ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   prc = getenv( "AFNI_PSPRINT" ) ;
   if( prc != NULL ){
      sprintf( print_command , "|%.250s" , prc ) ;
      XtAddCallback( psprintbut , XmNactivateCallback , psprint_CB , (XtPointer) mpcb ) ;
   } else {
      XtAddCallback( psprintbut , XmNactivateCallback , beep_CB ,
                     (XtPointer) "*** AFNI_PSPRINT not defined - see README.environment" ) ;
   }

   ibut++ ;
   donebut = XtVaCreateManagedWidget(
                 "dialog" , xmPushButtonWidgetClass , form ,
                    LABEL_ARG("Done") ,
#if 1
                    BGCOLOR_ARG(redcolor) ,
#endif

                    XmNtopAttachment  , XmATTACH_FORM ,

                    XmNleftAttachment   ,
                        (ibut!=0) ? XmATTACH_POSITION : XmATTACH_FORM ,
                    XmNleftPosition , ibut*TIG ,

                    XmNrightAttachment  ,
                     (ibut==NBUT-1) ? XmATTACH_FORM : XmATTACH_POSITION ,
                    XmNrightPosition , ibut*TIG + (TIG-1) ,

                    XmNrecomputeSize , False ,
                    XmNtraversalOn   , False ,
                    XmNinitialResourcesPersistent , False ,
                 NULL ) ;
   XtAddCallback( donebut , XmNactivateCallback , donebut_CB , (XtPointer) mpcb ) ;

   /* drawing area to receive the picture */

   drawing = XtVaCreateManagedWidget( "dialog" , xmDrawingAreaWidgetClass , form ,
                                          XmNtopAttachment    , XmATTACH_WIDGET ,
                                          XmNtopWidget        , donebut ,
                                          XmNleftAttachment   , XmATTACH_FORM ,
                                          XmNrightAttachment  , XmATTACH_FORM ,
                                          XmNbottomAttachment , XmATTACH_FORM ,
                                          XmNinitialResourcesPersistent , False ,
                                        NULL ) ;

   XtAddCallback( drawing , XmNexposeCallback , expose_CB , (XtPointer) mpcb ) ;
   XtAddCallback( drawing , XmNresizeCallback , resize_CB , (XtPointer) mpcb ) ;

   /* finish the job */

   XtVaSetValues( form , BGCOLOR_ARG("white") , NULL ) ;

   XtManageChild(form) ;
   XtRealizeWidget(topshell);

   mpcb->valid = 1 ; mpcb->userdata = NULL ; mpcb->drawing = drawing ;
   return mpcb ;
}
