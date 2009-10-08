#include "coxplot.h"
#include <Xm/XmAll.h>
#include <ctype.h>
#include "mcw_malloc.h"    /* ZSS: Needed because SUMA_plot.c does 
                              allocate some pointers freed here Jan 09 */
#include "Amalloc.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

static char print_command[256] = "\0" ;
static char *redcolor = NULL ;
static char  wintitle[128] = {"AFNI"}; /* ZSS Oct 7 2009 */

void set_wintitle_memplot( char *s )   /* ZSS Oct 7 2009 */
{
   if (s) {
      snprintf(wintitle, 125*sizeof(char), "%s", s);
   } else {
      sprintf(wintitle,"AFNI");
   }  
   return;
}

#ifndef LABEL_ARG
#define LABEL_ARG(str) \
  XtVaTypedArg , XmNlabelString , XmRString , (str) , strlen(str)+1
#endif



/*--------------------------------------------------------------------------*/
#undef  STRING_HAS_SUFFIX
#define STRING_HAS_SUFFIX(ss,suf)              \
  ((ss != NULL) && (suf != NULL) &&            \
   (strlen(ss) >= strlen(suf))   &&            \
   (strcmp(ss+strlen(ss)-strlen(suf),suf) == 0))

typedef void vpfunc(char *,MEM_plotdata *) ;
typedef struct { char *suf ; vpfunc *fun ; } saver_pair ;
static int     num_spair = 0 ;
static saver_pair *spair = NULL ;

void memplot_topshell_setsaver( char *suf, void (*fun)(char *,MEM_plotdata *) )
{
   int nn ;

   if( suf == NULL || *suf == '\0' || fun == NULL ) return ;
   for( nn=0 ; nn < num_spair ; nn++ )
     if( strcmp(suf,spair[nn].suf) == 0 ) return ;

   nn = num_spair + 1 ;
   spair = (saver_pair *)realloc( (void *)spair , sizeof(saver_pair)*nn ) ;
   spair[num_spair].suf = strdup(suf) ;
   spair[num_spair].fun = fun ;
   num_spair            = nn ;
   return ;
}

/*==========================================================================
  Callback routines for memplot_to_topshell
============================================================================*/

/*--------------------------------------------------------------------------*/

static void beep_CB( Widget w , XtPointer cd , XtPointer cb )
{
   char * str = (char *) cd ;

   if( w != NULL ) XBell(XtDisplay(w),100) ;
   if( str != NULL && str[0] != '\0' ) fprintf(stderr,"%s\a\n",str) ;
   return ;
}

/*--------------------------------------------------------------------------
   Save plot to a PostScript file
----------------------------------------------------------------------------*/

/****** cancellation of a print request ******/

static void pscancel_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MEM_topshell_data *mpcb = (MEM_topshell_data *) cd ;

   if( mpcb == NULL || ! MTD_VALID(mpcb) ) return ;

   if( mpcb->dial != NULL ) XtDestroyWidget( mpcb->dial ) ;
   mpcb->dial = mpcb->wtf = NULL ;
   return ;
}

/****** finalization of a print request ******/

static void psfinalize_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MEM_topshell_data *mpcb = (MEM_topshell_data *) cd ;
   char *text , fname[128] ;
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

   for( ii=0 ; ii < num_spair ; ii++ ){               /* 05 Dec 2007 */
     if( STRING_HAS_SUFFIX(fname,spair[ii].suf) ){
       spair[ii].fun( fname , mpcb->mp ) ;
       XtDestroyWidget( mpcb->dial ) ; mpcb->dial = mpcb->wtf = NULL ;
       return ;
     }
   }

   if( !STRING_HAS_SUFFIX(text,"ps") ) strcat(fname,".ps") ;

   memplot_to_postscript( fname , mpcb->mp ) ;

   XtDestroyWidget( mpcb->dial ) ;
   mpcb->dial = mpcb->wtf = NULL ;
   return ;
}

/****** initiation of a print request ******/

void pm_psfile_CB( Widget w , XtPointer cd , XtPointer cb )
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
                LABEL_ARG("PostScript filename:\n[[or .jpg or .png ]]") ,
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

void pm_psprint_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MEM_topshell_data *mpcb = (MEM_topshell_data *) cd ;
   MEM_plotdata *mp ;

   if( mpcb == NULL ) return ;
   mp = mpcb->mp ; if( mp == NULL ) return ;
   memplot_to_postscript( print_command , mp ) ;
   return ;
}

/*--------------------------------------------------------------------------
   Close plot window
----------------------------------------------------------------------------*/

void pm_donebut_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MEM_topshell_data * mpcb = (MEM_topshell_data *) cd ;
   if( mpcb == NULL || ! MTD_VALID(mpcb) ) return ;

   mpcb->valid = 0 ;

   if( mpcb->killfunc != NULL )
#if 0
     mpcb->killfunc( mpcb ) ;
#else
     AFNI_CALL_VOID_1ARG( mpcb->killfunc , MEM_topshell_data * , mpcb ) ;
#endif
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

void pm_expose_CB( Widget w , XtPointer cd , XtPointer cb )
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
   memplot_to_X11_sef( dpy , dw , mp , 0,0,MEMPLOT_FREE_ASPECT ) ;

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
   pm_expose_CB( mpcb->drawing , mpcb , NULL ) ;
   return ;
}

/*--------------------------------------------------------------------------
   Redraw plot window after a resize notice
----------------------------------------------------------------------------*/

void pm_resize_CB( Widget w , XtPointer cd , XtPointer cb )
{
   pm_expose_CB( w , cd , NULL ) ;
   return ;
}

/*--------------------------------------------------------------------------
   Handle input to the drawing area (key or button press) - 06 Aug 2001
----------------------------------------------------------------------------*/

void pm_input_CB( Widget w , XtPointer cd , XtPointer cb )
{
   MEM_topshell_data * mpcb = (MEM_topshell_data *) cd ;
   XmDrawingAreaCallbackStruct * cbs = (XmDrawingAreaCallbackStruct *) cb ;

   if( mpcb == NULL || ! MTD_VALID(mpcb)         ) return ;  /* bad */
   if( cbs  == NULL || cbs->reason != XmCR_INPUT ) return ;  /* real bad */

   switch( cbs->event->type ){

      default: break ;

      /*----- take key press -----*/

      case KeyPress:{
         XKeyEvent * event = (XKeyEvent *) cbs->event ;
         char           buf[32] ;
         KeySym         ks ;

         buf[0] = '\0' ;
         XLookupString( event , buf , 32 , &ks , NULL ) ;

         switch( buf[0] ){
            default: break;
            case 'Q':
            case 'q':
               pm_donebut_CB( NULL , (XtPointer) mpcb , NULL ) ;
               break ;
         }
         break ;
      }
      break ;
   }

   return ;
}

/*------------------------------------------------------------------
   External killer, for use by user routines
--------------------------------------------------------------------*/

void plotkill_topshell( MEM_topshell_data * mpcb )
{
   if( mpcb == NULL || ! MTD_VALID(mpcb) ) return ;

   pm_donebut_CB( NULL , (XtPointer) mpcb , NULL ) ;
   return ;
}

/*---------------------------------------------------------------------------*/

void pm_decode_geom( char * geom , int *ww, int *hh , int *xx, int *yy )
{
   int has_x , has_plus ;

   *ww = *hh = *xx = *yy = -1 ;
   if( geom == NULL || geom[0] == '\0' ) return ;

   has_x    = strstr(geom,"x") != NULL ;
   has_plus = strstr(geom,"+") != NULL ;

   if( has_x && has_plus )
      sscanf(geom,"%dx%d+%d+%d",ww,hh,xx,yy) ;
   else if( has_x )
      sscanf(geom,"%dx%d",ww,hh) ;
   else if( has_plus )
      sscanf(geom,"+%d+%d",xx,yy) ;

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

MEM_topshell_data * memplot_to_topshell( Display *dpy,
                                         MEM_plotdata *mp, void_func *kfun )
{
   Widget topshell , drawing , donebut , form , psfilebut , 
         psprintbut ;
   MEM_topshell_data *mpcb ;
   int hmin=400 , wmin , ibut=0 , hh,ww,xx,yy ;
   char *prc , *ept ;

   /* sanity check */

   if( dpy == NULL || mp == NULL ) return NULL ;

   mpcb = (MEM_topshell_data *) malloc( sizeof(MEM_topshell_data) ) ;
   memset((void*)mpcb, 0, sizeof(MEM_topshell_data));
   mpcb->valid = 0 ;

#ifdef HAVE_XDBE
   init_XDBE(dpy) ; mpcb->have_xdbe = 0 ;
#endif

   wmin = MEMPLOT_ASPECT(mp) * hmin ;

   /* 12 Oct 2000: a crude way to set the geometry of the popup */

   pm_decode_geom( getenv("AFNI_tsplotgeom") , &ww,&hh,&xx,&yy ) ;
   if( ww < wmin ) ww = wmin ;
   if( hh < hmin ) hh = hmin ;

   /* shell to hold it all */

   topshell = XtVaAppCreateShell(
                 "AFNI" , "AFNI" , topLevelShellWidgetClass , dpy ,
                   XmNborderWidth ,   0  ,
                   XmNminHeight   , hmin , XmNheight , hh ,
                   XmNminWidth    , wmin , XmNwidth  , ww ,
                   XmNallowShellResize , False ,
                   XmNinitialResourcesPersistent , False ,
                   XmNdeleteResponse   , XmDO_NOTHING , /* deletion handled 
                                                            below */
                 NULL ) ;
   XtVaSetValues(topshell, XmNtitle, wintitle, NULL); /* ZSS Oct 7 2009 */
   
   XmAddWMProtocolCallback(
        topshell , XmInternAtom(dpy,"WM_DELETE_WINDOW",False) ,
        pm_donebut_CB , (XtPointer) mpcb ) ;

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
                    LABEL_ARG("save image to file") ,
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
   XtAddCallback( psfilebut , XmNactivateCallback , pm_psfile_CB , (XtPointer) mpcb ) ;

   ibut++ ;
   psprintbut = XtVaCreateManagedWidget(
                 "dialog" , xmPushButtonWidgetClass , form ,
                    LABEL_ARG("to printer") ,
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
      XtAddCallback( psprintbut , XmNactivateCallback , pm_psprint_CB , (XtPointer) mpcb ) ;
   } else {
#if 0
      XtAddCallback( psprintbut , XmNactivateCallback , beep_CB ,
                     (XtPointer)"*** AFNI_PSPRINT not defined - see README.environment" );
#elif 0
      XtSetSensitive( psprintbut , False ) ;  /* 05 Nov 2001 */
#else
      XtUnmanageChild( psprintbut ) ;
#endif
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
   XtAddCallback( donebut , XmNactivateCallback , pm_donebut_CB , (XtPointer) mpcb ) ;

   /* drawing area to receive the picture */

   drawing = XtVaCreateManagedWidget( "dialog" , xmDrawingAreaWidgetClass , form ,
                                          XmNtopAttachment    , XmATTACH_WIDGET ,
                                          XmNtopWidget        , donebut ,
                                          XmNleftAttachment   , XmATTACH_FORM ,
                                          XmNrightAttachment  , XmATTACH_FORM ,
                                          XmNbottomAttachment , XmATTACH_FORM ,
                                          XmNinitialResourcesPersistent , False ,
                                        NULL ) ;

   XtAddCallback( drawing , XmNexposeCallback , pm_expose_CB , (XtPointer) mpcb ) ;
   XtAddCallback( drawing , XmNresizeCallback , pm_resize_CB , (XtPointer) mpcb ) ;
   XtAddCallback( drawing , XmNinputCallback  , pm_input_CB  , (XtPointer) mpcb ) ;

   /* finish the job */

   XtVaSetValues( form , BGCOLOR_ARG("white") , NULL ) ;

   if( xx >= 0 && yy >= 0 )
      XtVaSetValues( topshell , XmNx,xx , XmNy,yy , NULL ) ;

   XtManageChild(form) ;
   XtRealizeWidget(topshell);

   mpcb->valid = 1 ; mpcb->userdata = NULL ; mpcb->drawing = drawing ;
   return mpcb ;
}

