/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mcw_graf.h"

#ifndef LABEL_ARG
#define LABEL_ARG(str) \
  XtVaTypedArg , XmNlabelString , XmRString , (str) , strlen(str)+1
#endif

/* graph drawing functions, inspired by the GRAF code in program xv */

#define GRAF_EXTRA   6
#define GRAF_XTICK  16
#define GRAF_NTICK   8

/* 30 Nov 2002: prototypes for internal functions (at end of file) */

static char *get_popup_label( float,float,float,float , int,int ) ;
static double clock_time(void) ;

MCW_graf * new_MCW_graf( Widget wpar , MCW_DC * dc , char * title ,
                         gen_func * cbfunc , void * cbdata )
{
   MCW_graf * gp ;
   Widget rcbox , wf ;
   XmString xstr ;
   char * curve_label[1] = { "Crv" } ;
   int ii ;

   gp = myXtNew(MCW_graf) ;

   /* make the Widgets */

   gp->topform = XtVaCreateWidget(
                    "dialog" , xmFormWidgetClass , wpar ,
                       XmNinitialResourcesPersistent , False ,
                       XmNtraversalOn , False ,
                    NULL ) ;

   if( title == NULL || title[0] == '\0' ) title = "Graphing" ;
   xstr = XmStringCreateLtoR( title , XmFONTLIST_DEFAULT_TAG );
   gp->toplabel = XtVaCreateManagedWidget(
                     "dialog" , xmLabelWidgetClass , gp->topform ,
                        XmNtopAttachment   , XmATTACH_FORM ,
                        XmNleftAttachment  , XmATTACH_FORM ,
                        XmNlabelString     , xstr  ,
                        XmNrecomputeSize   , False ,
                        XmNmarginWidth     , 0     ,
                        XmNalignment       , XmALIGNMENT_BEGINNING ,
                        XmNinitialResourcesPersistent , False ,
                     NULL ) ;
   XmStringFree(xstr) ;

   wf = XtVaCreateWidget( "dialog" , xmFrameWidgetClass , gp->topform ,
                             XmNshadowType , XmSHADOW_IN ,
                             XmNshadowThickness , 4 ,
                             XmNtopAttachment   , XmATTACH_WIDGET ,
                             XmNtopWidget       , gp->toplabel ,
                             XmNleftAttachment  , XmATTACH_FORM ,
                             XmNtraversalOn , False ,
                             XmNinitialResourcesPersistent , False ,
                          NULL ) ;
   gp->drawer = XtVaCreateManagedWidget(
                   "dialog" , xmDrawingAreaWidgetClass , wf ,
                       XmNwidth       , GRAF_SIZE ,
                       XmNheight      , GRAF_SIZE + GRAF_EXTRA ,
                       XmNtraversalOn , False ,
                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;
   XtManageChild(wf) ;

   XtInsertEventHandler( gp->drawer ,    /* handle events in image */

                            0
                          | ButtonPressMask     /* button presses */
                          | ExposureMask        /* exposures */
                         ,
                         FALSE ,                /* nonmaskable events? */
                         GRAF_drawing_EV ,      /* event handler */
                         (XtPointer) gp ,       /* client data */
                         XtListTail ) ;         /* last in queue */

   /* 30 Nov 2002: popup stuff */

#ifdef BAD_BUTTON3_POPUPS
   gp->popmenu  = XmCreatePopupMenu( gp->topform, "help" , NULL , 0 ) ;
#else
   gp->popmenu  = XmCreatePopupMenu( gp->drawer , "help" , NULL , 0 ) ;
#endif
   gp->poplabel = XtVaCreateManagedWidget(
                    "help" , xmLabelWidgetClass , gp->popmenu ,
                    LABEL_ARG("I am a label") ,
                    XmNinitialResourcesPersistent , False ,
                  NULL ) ;

   rcbox = XtVaCreateWidget(
                "dialog" , xmRowColumnWidgetClass , gp->topform ,
                   XmNpacking          , XmPACK_TIGHT ,
                   XmNadjustLast       , False ,
                   XmNadjustMargin     , False ,
                   XmNnumColumns       , 1 ,
                   XmNtopAttachment    , XmATTACH_WIDGET ,
                   XmNtopWidget        , gp->toplabel ,
                   XmNleftAttachment   , XmATTACH_WIDGET ,
                   XmNleftWidget       , wf ,
                   XmNrightAttachment  , XmATTACH_FORM ,
                   XmNinitialResourcesPersistent , False ,
                NULL ) ;

   gp->curve_bbox = new_MCW_bbox( rcbox ,
                                  1 , curve_label ,
                                  MCW_BB_check , MCW_BB_noframe ,
                                  GRAF_curve_CB , (XtPointer) gp ) ;

   gp->handle_av =  new_MCW_arrowval( rcbox , "#" ,
                                      MCW_AV_downup , 2,MAX_GHANDS,4 ,
                                      MCW_AV_notext , 0 ,
                                      GRAF_handle_CB , (XtPointer) gp ,
                                      NULL,NULL ) ;

   gp->reset_pb = XtVaCreateManagedWidget(
                     "dialog" , xmPushButtonWidgetClass , rcbox ,
                        LABEL_ARG("Line") ,
                        XmNalignment     , XmALIGNMENT_CENTER ,
                        XmNtraversalOn   , False ,
                        XmNinitialResourcesPersistent , False ,
                     NULL ) ;

   XtAddCallback( gp->reset_pb, XmNactivateCallback, GRAF_reset_CB , gp ) ;

   XtManageChild( rcbox ) ;
   XtManageChild( gp->topform ) ;

   /* initialize data structure */

   gp->dc     = dc ;
   gp->cbfunc = cbfunc ;
   gp->cbdata = cbdata ;

   gp->fg = gp->bg = 0 ;   /* will be fixed later */
   gp->gwin = (Window) 0 ;

   gp->nhands = 4;
   gp->spline = 0;
   gp->hands[0].x =   0;  gp->hands[0].y =   0;
   gp->hands[1].x =  64;  gp->hands[1].y =  64;
   gp->hands[2].x = 192;  gp->hands[2].y = 192;
   gp->hands[3].x = 255;  gp->hands[3].y = 255;

   GenerateGrafFunc(gp,0);
   memcpy( gp->oldf , gp->func , sizeof(byte)*256 ) ;
   gp->yeqx = 1 ;

   gp->xbot = gp->xtop = gp->ybot = gp->ytop = 0.0 ;  /* 30 Nov 2002 */

   return gp ;
}

/*--------------------------------------------------------------------
  Set the graph parameters and redraw it [14 Jul 1999].
----------------------------------------------------------------------*/

void GRAF_put_setup( MCW_graf * gp , int nh , int * xh , int * yh , int spl )
{
   int ii ;

   if( gp == NULL ||
       nh < 2     || nh > MAX_GHANDS ||
       xh == NULL || yh == NULL        ) return ;

   gp->nhands = nh ;
   gp->spline = (spl != 0 ) ;

   for( ii=0 ; ii < nh ; ii++ ){
      gp->hands[ii].x = xh[ii] ;
      gp->hands[ii].y = yh[ii] ;
   }

   GenerateGrafFunc(gp,1) ; (void) GRAF_changed(gp) ;
   return ;
}

void GRAF_get_setup( MCW_graf * gp , int * nh , int * xh , int * yh , int * spl )
{
   int ii ;

   if( gp == NULL || nh == NULL || xh == NULL || yh == NULL || spl == NULL ) return ;

   *nh  = gp->nhands ; *spl = gp->spline ;

   for( ii=0 ; ii < gp->nhands ; ii++ ){
      xh[ii] = gp->hands[ii].x ;
      yh[ii] = gp->hands[ii].y ;
   }
   return ;
}

/*--------------------------------------------------------------------
   Check to see if graph function has changed.  As a byproduct,
   saves the current function in the backup place, so that if
   called twice in a row, the answer the second time is always no.
----------------------------------------------------------------------*/

int GRAF_changed( MCW_graf * gp )
{
   int ii , cc=0 , yeqx=1 ;

   for( ii=0 ; ii < 256 ; ii++ ){
      if( gp->oldf[ii] != gp->func[ii] ){ cc = 1 ; if( !yeqx ) break ; }
      if( yeqx && gp->func[ii] != ii   ){ yeqx = 0 ; if( cc )  break ; }
   }

   gp->yeqx = yeqx ;
   if( cc ) memcpy( gp->oldf , gp->func , sizeof(byte)*256 ) ;
   return cc ;
}

/*--------------------------------------------------------------------
   Event handler for the drawing area
----------------------------------------------------------------------*/

void GRAF_drawing_EV( Widget w , XtPointer client_data ,
                      XEvent * ev , Boolean * continue_to_dispatch )
{
   MCW_graf * gp = (MCW_graf *) client_data ;

   if( gp == NULL ) return ;

   /* initialize some X11 stuff for easy access later */

   if( gp->bg == 0 && gp->fg == 0 ){
     XtVaGetValues( gp->drawer ,
                       XmNforeground , &(gp->fg) ,
                       XmNbackground , &(gp->bg) ,
                    NULL ) ;
     gp->gwin = XtWindow(gp->drawer) ;
   }

   switch( ev->type ){

      /*----- redraw -----*/

      case Expose:{
         XExposeEvent * event = (XExposeEvent *) ev ;

         XSync( gp->dc->display , False ) ;  /* synchronize with server */

         if( event->count == 0 ){
            if( w == gp->drawer ) drawGraf(gp,0) ;
         }
      }
      break ;  /* end of Expose */

      /*----- Button Press -----*/

      case ButtonPress:{
         XButtonEvent * event = (XButtonEvent *) ev ;
         Window rW,cW ;
         int mx,my , but , rx,ry ;
         int vertonly, x,y , orighx, orighy, grab,h , newx,newy ;
         unsigned int mask ;

         int use_popup=0 , opx,opy ; /* 30 Nov 2002 */
         double ct1,ct2 ;

#undef USE_MyCursor
#ifdef USE_MyCursor
         static int need_MyCursor = 1 ;
         static Cursor MyCursor ;

         if( need_MyCursor ){ /* create MyCursor to be invisible */
            Pixmap      pix;
            static char bits[] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
            XColor      cfg;

            cfg.red = cfg.green = cfg.blue = 0;
            pix = XCreateBitmapFromData(gp->dc->display, gp->gwin, bits, 8, 8);
            MyCursor = XCreatePixmapCursor(gp->dc->display, pix, pix, &cfg, &cfg, 0,0);
            XFreePixmap(gp->dc->display, pix);
            need_MyCursor = 0 ;
         }
#else
#  define MyCursor None
#endif /* USE_MyCursor */

         but = event->button ;
         if( but == Button2 ) return ;  /* bad button */

         /* 30 Nov 2002: label popup if Button3 was pressed */

         if( but == Button3 && gp->popmenu != NULL ){
           char *str = get_popup_label( gp->xbot,gp->xtop ,
                                        gp->ybot,gp->ytop ,
                                        event->x , event->y ) ;
           if( str != NULL ){
             MCW_set_widget_label( gp->poplabel , str ) ;
             XmMenuPosition( gp->popmenu , event ) ;
             XtManageChild ( gp->popmenu ) ;
             use_popup = 1 ; ct1 = clock_time() ;  /* start timer */
             opx = event->x ; opy = event->y ;     /* old popup (x,y) in (opx,opy) */
           }
         }

         /* see if press is within any of the handles */

         mx = event->x ; my = event->y ;  /* window coords */

         for (h=0; h<gp->nhands; h++) {
           if (PTINRECT(mx*2,(127-my)*2,
                        gp->hands[h].x-5,gp->hands[h].y-5,11,11)) break;
         }

         if (h==gp->nhands) return ;  /* meaningless click - not in a "hand" */

         /* grab the mouse (should always work, but you never know) */

         grab = !XGrabPointer(gp->dc->display,
                              gp->gwin, False, 0, GrabModeAsync,
                              GrabModeAsync, gp->gwin, MyCursor , (Time) CurrentTime);

         orighx = gp->hands[h].x; orighy = gp->hands[h].y;  /* current hand location */

         vertonly = (h==0 || h==(gp->nhands-1));   /* 1st & last hands: only y moves */

         /** loop while mouse button is pressed down **/

         while( XQueryPointer(gp->dc->display,gp->gwin,&rW,&cW,&rx,&ry,&x,&y,&mask) ){

          if( !(mask & Button1Mask) && !(mask & Button3Mask) ) break; /* button released */

          /* XQueryPointer returned current mouse position in (x,y); */
          /* now, convert x,y from window to handle coordinates      */

          newx = (vertonly) ? orighx : 2*x ;         /* handle now at */
          newy = (y >= 127) ? 0      : 255 - 2*y ;   /* (newx,newy)   */

          if( !vertonly ){ /* don't let handle x stray past neighbors */
            if( newx <= gp->hands[h-1].x ) newx = gp->hands[h-1].x + 1 ;
            if( newx >= gp->hands[h+1].x ) newx = gp->hands[h+1].x - 1 ;
          }

          RANGE(newx, 0, 255);  /* ensure they are in the legal range! */
          RANGE(newy, 0, 255);

          /* 30 Nov 2002: re-label popup if new (x,y) != (opx,opy) */
#if 1
          x = newx/2 ; y = (255-newy)/2 ;   /* convert back to screen coords */
          if( use_popup && (opx != x || opy != y) ){
            ct2 = clock_time() ;    /* only update every so often */
            if( ct2-ct1 > 0.100 ){
              char *str = get_popup_label( gp->xbot,gp->xtop ,
                                           gp->ybot,gp->ytop , x,y ) ;
              ct1 = ct2 ; opx = x ; opy = y ;
              if( str != NULL ){
                MCW_set_widget_label( gp->poplabel , str ) ;
                XSync( XtDisplay(gp->drawer) , True ) ;
                XmUpdateDisplay( gp->drawer ) ;
              }
            }
          }
#endif

          /* if handle moved, redraw graph */

          if (newx != gp->hands[h].x || newy != gp->hands[h].y) {

            DC_fg_colorpix( gp->dc , gp->bg ) ;  /* erase region around handle */
            XFillRectangle( gp->dc->display, gp->gwin, gp->dc->myGC ,
                     (gp->hands[h].x/2)-3, ((255-gp->hands[h].y)/2)-3, 7,7);

            gp->hands[h].x = newx;  gp->hands[h].y = newy;

            drawGraf(gp,1);           /* erase old trace */
            GenerateGrafFunc(gp,0);   /* generate curve from handles */
            drawGraf(gp,0);           /* redraw new trace */

          } /* end of redraw graph after handle move */

        }  /* end of loop that modifies graph, while mouse button is down */

        /* release the mouse grab */

        if (grab) XUngrabPointer(gp->dc->display, (Time) CurrentTime);

        /* if the graph WAS changed, call the callback function */

        if( GRAF_changed(gp) && gp->cbfunc != NULL ) gp->cbfunc(gp,gp->cbdata) ;

#if 1
        if( use_popup ){                     /* 30 Nov 2002 */
          XtUnmanageChild( gp->popmenu ) ;
          MCW_expose_widget( gp->topform ) ;
        }
#endif
      }
      break ;  /* end of ButtonPress */

   } /* end of switch on event type */

   return ;
}

/************************************************************
  draw or erase the graph
*************************************************************/

void drawGraf( MCW_graf * gp , int erase )
{
  int i,x,y;
  XPoint  pts[129], *pt;
  XSegment segs[GRAF_NTICK] , *sg ;

  /* decide whether to draw in foreground or background */

  DC_fg_colorpix( gp->dc , (erase) ? gp->bg : gp->fg ) ;

  /* draw lines that make up the graph */

  for (i=0, pt=pts; i<256; i+=2,pt++) {
    pt->x = i/2;  pt->y = 127 - (gp->func[i]/2);
    if (i==0) i = -1;   /* kludge to get sequence 0,1,3,5, ... 253,255 */
  }
  DC_linewidth( gp->dc , 2 ) ;
  XDrawLines( gp->dc->display , gp->gwin ,
              gp->dc->myGC , pts, 129, CoordModeOrigin    );
  DC_linewidth( gp->dc , 0 ) ;

  if (erase) return ;  /* just erased the curve */

  /* erase handles prior to drawing new ones */

  DC_fg_colorpix( gp->dc , gp->bg ) ;

  DC_linewidth( gp->dc , 1 ) ;
  for (i=0; i<gp->nhands; i++) {   /* clear inside rectangles */
    x = gp->hands[i].x/2;  y = 127 - gp->hands[i].y/2;
    XFillRectangle(gp->dc->display, gp->gwin,
                   gp->dc->myGC, x-2, y-2, 5,5);
  }

  /* redraw handles */

  DC_fg_colorpix( gp->dc , gp->fg ) ;

  for (i=0; i<gp->nhands; i++) {  /* draw center dots */
    x = gp->hands[i].x/2;  y = 127 - gp->hands[i].y/2;
    XDrawPoint(gp->dc->display, gp->gwin, gp->dc->myGC, x, y);
  }

  for (i=0; i<gp->nhands; i++) {   /* draw rectangles */
    x = gp->hands[i].x/2;  y = 127 - gp->hands[i].y/2;
    XDrawRectangle(gp->dc->display, gp->gwin, gp->dc->myGC, x-3, y-3, 6,6);
  }
  DC_linewidth( gp->dc , 0 ) ;

  /* draw tick marks in the extra space */

  for( i=0,x=GRAF_XTICK,sg=segs ; x < GRAF_SIZE ; i++,sg++,x+=GRAF_XTICK ){
     sg->x1 = x ; sg->y1 = GRAF_SIZE+GRAF_EXTRA-1 ;
     sg->x2 = x ; sg->y2 = GRAF_SIZE ;
  }
  XDrawSegments( gp->dc->display , gp->gwin , gp->dc->myGC , segs , i ) ;

  return ;
}

/*---------------------------------------------------------------------
   Callback for "Crv" button -- curves or lines?
-----------------------------------------------------------------------*/

void GRAF_curve_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_graf * gp = (MCW_graf *) client_data ;
   int bval ;

   bval = MCW_val_bbox( gp->curve_bbox ) ;
   if( bval == gp->spline ) return ;       /* no change */

   gp->spline = bval ;
   GenerateGrafFunc(gp,1);
   if( GRAF_changed(gp) && gp->cbfunc != NULL ) gp->cbfunc(gp,gp->cbdata) ;
   return ;
}

/*---------------------------------------------------------------------
   Callback for "Line" button -- reset the graf
-----------------------------------------------------------------------*/

void GRAF_reset_CB( Widget w, XtPointer client_data, XtPointer call_data )
{
   MCW_graf * gp = (MCW_graf *) client_data ;
   int j ;

   for( j=0 ; j < gp->nhands ; j++ ) gp->hands[j].y = gp->hands[j].x ;

   GenerateGrafFunc(gp,1);
   if( GRAF_changed(gp) && gp->cbfunc != NULL ) gp->cbfunc(gp,gp->cbdata) ;
   return ;
}

/*-------------------------------------------------------------------------
    Set the values stored inside a passive graph
---------------------------------------------------------------------------*/

void GRAF_set_func( MCW_graf * gp , byte * func )
{
   int i ;

   if( gp == NULL ) return ;

   if( func != NULL ){
      for( i=0 ; i < 256 ; i++ ) gp->func[i] = func[i] ;
   } else {
      for( i=0 ; i < 256 ; i++ ) gp->func[i] = i ;
   }

   for( i=0 ; i < gp->nhands ; i++ )
      gp->hands[i].y = gp->func[gp->hands[i].x] ;

   (void) GRAF_changed(gp) ;
   drawGraf(gp,1); drawGraf(gp,0);
   return ;
}

/*---------------------------------------------------------------------
   Callback for "#" arrowval -- how many handles today?
-----------------------------------------------------------------------*/

void GRAF_handle_CB( MCW_arrowval * av , XtPointer client_data )
{
   MCW_graf * gp = (MCW_graf *) client_data ;
   int nh = av->ival , j ;

   if( nh < 2 || nh > MAX_GHANDS || nh == gp->nhands ){  /* error */
      XBell(gp->dc->display,100) ;
      return ;
   }

   if( nh < gp->nhands ){  /* delete a handle */

      /* find (middle) point whose x-distance to previous
         and next points is minimal.  Delete that point */

      int dist, mdist, mpos;

      mdist = (gp->hands[1].x - gp->hands[0].x) +
              (gp->hands[2].x - gp->hands[1].x);
      mpos = 1;

      for (j=2; j<gp->nhands-1; j++) {
         dist = (gp->hands[j  ].x - gp->hands[j-1].x) +
                (gp->hands[j+1].x - gp->hands[j].x);
                if (dist < mdist) { mdist = dist;  mpos = j; }
      }

      /* delete position 'mpos' in hands[] array */

      xvbcopy( (char *) &gp->hands[mpos+1] ,
               (char *) &gp->hands[mpos]   ,
               (gp->nhands-mpos-1) * sizeof(XPoint) ) ;

      gp->nhands--;

   } else if( nh > gp->nhands ){  /* add a handle */

      /* find largest x-gap in handles, put new handle in mid */

      int lgap, lpos, x, y;

      lgap = gp->hands[1].x - gp->hands[0].x;
      lpos = 1;
      for (j=1; j<gp->nhands-1; j++)
         if ((gp->hands[j+1].x - gp->hands[j].x) > lgap) {
            lgap = gp->hands[j+1].x - gp->hands[j].x;
            lpos = j+1;
         }

      /* open up position in hands[] array */

      xvbcopy( (char *) &gp->hands[lpos]   ,
               (char *) &gp->hands[lpos+1] ,
               (gp->nhands - lpos) * sizeof(XPoint) ) ;

      x = gp->hands[lpos-1].x + lgap/2;
      y = gp->func[x];
      gp->hands[lpos].x = x;
      gp->hands[lpos].y = y;
      gp->nhands++;
   }

   GenerateGrafFunc(gp,1);
   if( GRAF_changed(gp) && gp->cbfunc != NULL ) gp->cbfunc(gp,gp->cbdata) ;
   return ;
}

/**********************************************************************
  Compute the graph function from the handle positions
***********************************************************************/

void GenerateGrafFunc( MCW_graf * gp , int redraw )
{
  int i,j,k;

  /* do sanity check.  (x-coords must be sorted (strictly increasing)) */

  for (i=0; i<gp->nhands; i++) {
    RANGE(gp->hands[i].x, 0, 255);
    RANGE(gp->hands[i].y, 0, 255);
  }

  gp->hands[0].x = 0;  gp->hands[gp->nhands-1].x = 255;
  for (i=1; i<gp->nhands-1; i++) {
    if (gp->hands[i].x < i)  gp->hands[i].x = i;
    if (gp->hands[i].x > 256-gp->nhands+i)
        gp->hands[i].x = 256-gp->nhands+i;

    if (gp->hands[i].x <= gp->hands[i-1].x)
      gp->hands[i].x = gp->hands[i-1].x + 1;
  }

  /* recompute the function */

  if (!gp->spline) {  /* do linear interpolation */
      int y,x1,y1,x2,y2;
      double yd;

      for (i=0; i<gp->nhands-1; i++) {
        x1 = gp->hands[ i ].x;  y1 = gp->hands[ i ].y;
        x2 = gp->hands[i+1].x;  y2 = gp->hands[i+1].y;

        for (j=x1,k=0; j<=x2; j++,k++) {  /* x2 <= 255 */
          yd = ((double) k * (y2 - y1)) / (x2 - x1);
          y = y1 + (int) floor(yd + 0.5);
          RANGE(y,0,255);
          gp->func[j] = y;
        }
      }
    }

  else {  /* splinear interpolation */
    static int x[MAX_GHANDS], y[MAX_GHANDS];
    double yf[MAX_GHANDS];
    double yd;

    for (i=0; i<gp->nhands; i++) {
      x[i] = gp->hands[i].x;  y[i] = gp->hands[i].y;
    }

    InitSpline(x, y, gp->nhands, yf);

    for (i=0; i<256; i++) {
      yd = EvalSpline(x, y, yf, gp->nhands, (double) i);
      j = (int) floor(yd + 0.5);
      RANGE(j,0,255);
      gp->func[i] = j;
    }
  }

  if (redraw) {  /* redraw graph */
    XClearWindow( gp->dc->display, gp->gwin ) ;
    drawGraf(gp,0);
  }
}

/***************************************************************************/

void InitSpline(int *x,int *y,int n,double *y2)
{
  /* given arrays of data points x[0..n-1] and y[0..n-1], computes the
     values of the second derivative at each of the data points
     y2[0..n-1] for use in the splint function */

  int i,k;
  double p,qn,sig,un,u[MAX_GHANDS];

  y2[0] = u[0] = 0.0;

  for (i=1; i<n-1; i++) {
    sig = ((double) x[i]-x[i-1]) / ((double) x[i+1] - x[i-1]);
    p = sig * y2[i-1] + 2.0;
    y2[i] = (sig-1.0) / p;
    u[i] = (((double) y[i+1]-y[i]) / (x[i+1]-x[i])) -
           (((double) y[i]-y[i-1]) / (x[i]-x[i-1]));
    u[i] = (6.0 * u[i]/(x[i+1]-x[i-1]) - sig*u[i-1]) / p;
  }
  qn = un = 0.0;

  y2[n-1] = (un-qn*u[n-2]) / (qn*y2[n-2]+1.0);
  for (k=n-2; k>=0; k--)
    y2[k] = y2[k]*y2[k+1]+u[k];
}

/*******************************************************************/

double EvalSpline(int xa[],int ya[],double y2a[],int n,double x)
{
  int klo,khi,k;
  double h,b,a;

  klo = 0;
  khi = n-1;
  while (khi-klo > 1) {
    k = (khi+klo) >> 1;
    if (xa[k] > x) khi = k;
    else klo = k;
  }
  h = xa[khi] - xa[klo];
  if (h==0.0) FatalError("bad xvalues in splint\n");
  a = (xa[khi]-x)/h;
  b = (x-xa[klo])/h;
  return (a*ya[klo] + b*ya[khi] + ((a*a*a-a)*y2a[klo] +(b*b*b-b)*y2a[khi])
          * (h*h) / 6.0);
}


/*******************************************************************/

void xvbcopy(char * src, char * dst, size_t len)
{

  /* determine if the regions overlap
   *
   * 3 cases:  src=dst, src<dst, src>dst
   *
   * if src=dst, they overlap completely, but nothing needs to be moved
   * if src<dst and src+len>dst then they overlap
   */

  if (src==dst || len<=0) return;    /* nothing to do */

  if (src<dst && src+len>dst) {  /* do a backward copy */
    src = src + len - 1;
    dst = dst + len - 1;
    for ( ; len>0; len--, src--, dst--) *dst = *src;
  }

  else {  /* they either overlap (src>dst) or they don't overlap */
    /* do a forward copy */
    for ( ; len>0; len--, src++, dst++) *dst = *src;
  }

  return ;
}

/*===================================================================
   Routines for a passive graph
=====================================================================*/

MCW_pasgraf * new_MCW_pasgraf( Widget wpar , MCW_DC * dc , char * title )
{
   MCW_pasgraf * gp ;
   Widget  wf ;
   XmString xstr ;

   gp = myXtNew(MCW_pasgraf) ;

   /* make the Widgets */

   gp->topform = XtVaCreateWidget(
                    "dialog" , xmFormWidgetClass , wpar ,
                       XmNinitialResourcesPersistent , False ,
                       XmNtraversalOn , False ,
                    NULL ) ;

   if( title == NULL || title[0] == '\0' ) title = "Graphing" ;
   xstr = XmStringCreateLtoR( title , XmFONTLIST_DEFAULT_TAG );
   gp->toplabel = XtVaCreateManagedWidget(
                     "dialog" , xmLabelWidgetClass , gp->topform ,
                        XmNtopAttachment   , XmATTACH_FORM ,
                        XmNleftAttachment  , XmATTACH_FORM ,
                        XmNlabelString     , xstr  ,
                        XmNrecomputeSize   , False ,
                        XmNmarginWidth     , 0     ,
                        XmNalignment       , XmALIGNMENT_BEGINNING ,
                        XmNinitialResourcesPersistent , False ,
                     NULL ) ;
   XmStringFree(xstr) ;

   wf = XtVaCreateWidget( "dialog" , xmFrameWidgetClass , gp->topform ,
                             XmNshadowType , XmSHADOW_IN ,
                             XmNshadowThickness , 4 ,
                             XmNtopAttachment   , XmATTACH_WIDGET ,
                             XmNtopWidget       , gp->toplabel ,
                             XmNleftAttachment  , XmATTACH_FORM ,
                             XmNtraversalOn , False ,
                             XmNinitialResourcesPersistent , False ,
                          NULL ) ;
   gp->drawer = XtVaCreateManagedWidget(
                   "dialog" , xmDrawingAreaWidgetClass , wf ,
                       XmNwidth       , GRAF_SIZE ,
                       XmNheight      , GRAF_SIZE + GRAF_EXTRA ,
                       XmNtraversalOn , False ,
                       XmNinitialResourcesPersistent , False ,
                    NULL ) ;
   XtManageChild(wf) ;

   XtInsertEventHandler( gp->drawer ,    /* handle events in image */

                            0
                          | ButtonPressMask     /* button presses */
                          | ExposureMask        /* exposures */
                         ,
                         FALSE ,                /* nonmaskable events? */
                         GRAF_pasdrawing_EV ,      /* event handler */
                         (XtPointer) gp ,       /* client data */
                         XtListTail ) ;         /* last in queue */

   /* 30 Nov 2002: popup stuff */

#ifdef BAD_BUTTON3_POPUPS
   gp->popmenu  = XmCreatePopupMenu( gp->topform, "help" , NULL , 0 ) ;
#else
   gp->popmenu  = XmCreatePopupMenu( gp->drawer , "help" , NULL , 0 ) ;
#endif
   gp->poplabel = XtVaCreateManagedWidget(
                    "help" , xmLabelWidgetClass , gp->popmenu ,
                    LABEL_ARG("I am a label") ,
                    XmNinitialResourcesPersistent , False ,
                  NULL ) ;

   XtManageChild( gp->topform ) ;

   /* initialize data structure */

   gp->dc = dc ; gp->mode = PASGRAF_LINE ;

   gp->fg = gp->bg = 0 ;   /* will be fixed later */
   gp->gwin = (Window) 0 ;

   gp->xbot = gp->xtop = gp->ybot = gp->ytop = 0.0 ;  /* 30 Nov 2002 */

   return gp ;
}

/*--------------------------------------------------------------------
   Event handler for the drawing area
----------------------------------------------------------------------*/

void GRAF_pasdrawing_EV( Widget w , XtPointer client_data ,
                         XEvent * ev , Boolean * continue_to_dispatch )
{
   MCW_pasgraf * gp = (MCW_pasgraf *) client_data ;

   if( gp == NULL ) return ;

   /* initialize some X11 stuff for easy access later */

   if( gp->bg == 0 && gp->fg == 0 ){
     XtVaGetValues( gp->drawer ,
                       XmNforeground , &(gp->fg) ,
                       XmNbackground , &(gp->bg) ,
                    NULL ) ;
     gp->gwin = XtWindow(gp->drawer) ;
   }

   switch( ev->type ){

      /*----- redraw -----*/

      case Expose:{
         XExposeEvent * event = (XExposeEvent *) ev ;

         XSync( gp->dc->display , False ) ;  /* synchronize with server */

         if( event->count == 0 ){
            if( w == gp->drawer ) redraw_MCW_pasgraf(gp) ;
         }
      }
      break ;  /* end of Expose */

      /*----- Button click -----*/

      case ButtonPress:{
         XButtonEvent * event = (XButtonEvent *) ev ;
         int mx,my , but ;

         but = event->button ;

         /* 30 Nov 2002: label popup if Button3 was pressed */

         if( but == Button3 && gp->popmenu != NULL ){
           char *str = get_popup_label( gp->xbot,gp->xtop ,
                                        gp->ybot,gp->ytop ,
                                        event->x , event->y ) ;
           if( str != NULL ){
             MCW_set_widget_label( gp->poplabel , str ) ;
             XmMenuPosition( gp->popmenu , event ) ;
             XtManageChild ( gp->popmenu ) ;
           }
         }

         if( but != Button1 ) return ;    /* meaningless */
         mx = event->x ; my = event->y ;  /* window coords */

         NEXT_PASGRAF_MODE(gp) ;
         redraw_MCW_pasgraf(gp) ;
      }
      break ;  /* end of ButtonPress */
   }

   return ;
}

void redraw_MCW_pasgraf( MCW_pasgraf * gp )
{
   XPoint pts[GRAF_SIZE], *pt ;
   XSegment segs[GRAF_SIZE] , *sg ;
   int i , x ;

   if( gp->gwin == (Window) 0 ) return ;  /* not ready for prime time */

   XClearWindow( gp->dc->display, gp->gwin ) ;

   DC_fg_colorpix( gp->dc , gp->fg ) ;

   switch( gp->mode ){

      default:
      case PASGRAF_LINE:{                   /* draw lines */

         for( i=0, pt=pts ; i < GRAF_SIZE ; i++ , pt++ ){
            pt->x = i ; pt->y = GRAF_SIZE - gp->func[i] ;
         }
         XDrawLines( gp->dc->display , gp->gwin ,
                     gp->dc->myGC , pts, GRAF_SIZE, CoordModeOrigin );
      }
      break ;

      case PASGRAF_BAR:{                   /* draw bars */

         for( i=0, sg=segs ; i < GRAF_SIZE ; i++ , sg++ ){
            sg->x1 = i ; sg->y1 = GRAF_SIZE - 1 ;
            sg->x2 = i ; sg->y2 = GRAF_SIZE - 1 - gp->func[i] ;
         }
         XDrawSegments( gp->dc->display , gp->gwin ,
                        gp->dc->myGC , segs , GRAF_SIZE ) ;
      }
      break ;
   }

   /* draw tick marks in the extra space */

   for( i=0,x=GRAF_XTICK,sg=segs ; x < GRAF_SIZE ; i++,sg++,x+=GRAF_XTICK ){
      sg->x1 = x ; sg->y1 = GRAF_SIZE+GRAF_EXTRA-1 ;
      sg->x2 = x ; sg->y2 = GRAF_SIZE ;
   }
   XDrawSegments( gp->dc->display , gp->gwin , gp->dc->myGC , segs , i ) ;

   return ;
}

/*-------------------------------------------------------------------------
    Set the values stored inside a passive graph
---------------------------------------------------------------------------*/

void set_MCW_pasgraf( MCW_pasgraf * gp , byte * func )
{
   int i ;
   byte b ;

   if( gp == NULL ) return ;

   if( func != NULL ){
      for( i=0 ; i < GRAF_SIZE ; i++ ){
         b = func[i] ;
         gp->func[i] = (b < GRAF_SIZE) ? b : (GRAF_SIZE-1) ;
      }
   } else {
      memset( gp->func , 0 , sizeof(byte)*GRAF_SIZE ) ;
   }

   return ;
}

void MCW_histo_bytes( int nb , byte * bar , int * har )
{
   int i ;

   if( nb <= 0 || bar == NULL || har == NULL ) return ;

   for( i=0 ; i < 256 ; i++ ) har[i] = 0 ;

   for( i=0 ; i < nb ; i++ ) har[ bar[i] ]++ ;

   return ;
}

/*****************************************************************************/
/*** 30 Nov 2002: stuff for popping up position label ***/

void PASGRAF_set_xyrange( MCW_pasgraf *gp , float xb,float xt, float yb,float yt )
{
  if( gp == NULL ) return ;
  gp->xbot = xb ; gp->xtop = xt ;
  gp->ybot = yb ; gp->ytop = yt ;
}

/*--------------------------------------------------------------------------*/

void GRAF_set_xyrange( MCW_graf *gp , float xb,float xt, float yb,float yt )
{
  if( gp == NULL ) return ;
  gp->xbot = xb ; gp->xtop = xt ;
  gp->ybot = yb ; gp->ytop = yt ;
}

/*--------------------------------------------------------------------------*/

static char *get_popup_label( float xbot,float xtop ,
                              float ybot,float ytop , int x,int y )
{
   static char str[128] ;
   char xbuf[32],ybuf[32] , *xb,*yb ;
   float xx , yy , ff ;

        if( x <  0         ) x = 0 ;
   else if( x >= GRAF_SIZE ) x = GRAF_SIZE-1 ;
        if( y <  0         ) y = 0 ;
   else if( y >= GRAF_SIZE ) y = GRAF_SIZE-1 ;

   if( xtop != xbot ){
     ff = x / (float)(GRAF_SIZE-1) ;
     xx = ff*xtop + (1.0-ff)*xbot ;
   } else {
     xx = x ;
   }

   if( ytop != ybot ){
     ff = y / (float)(GRAF_SIZE-1) ;
     yy = ff*ybot + (1.0-ff)*ytop ;
   } else {
     yy = GRAF_SIZE-1 - y ;
   }

   AV_fval_to_char( xx,xbuf ); xb = xbuf; if( *xb == ' ' ) xb++;
   AV_fval_to_char( yy,ybuf ); yb = ybuf; if( *yb == ' ' ) yb++;
   sprintf(str,"%s,%s",xb,yb) ;
   return str ;
}

/*-----------------------------------------------------------------*/

static double clock_time(void) /* in seconds, since first call to this */
{
   struct timeval  new_tval ;
   struct timezone tzone ;
   static struct timeval old_tval ;     /* save old time */
   static int first = 1 ;

   gettimeofday( &new_tval , &tzone ) ;

   if( first ){                         /* 1st time in: */
      old_tval = new_tval ;             /* just save current time */
      first    = 0 ;
      return 0.0 ;                      /* and return zero */
   }

   if( old_tval.tv_usec > new_tval.tv_usec ){  /* adjust structs */
      new_tval.tv_usec += 1000000 ;            /* so new_tval.tv_usec */
      new_tval.tv_sec -- ;                     /* >= old_tval.tv_usec */
   }

   return (double)( (new_tval.tv_sec  - old_tval.tv_sec )
                   +(new_tval.tv_usec - old_tval.tv_usec)*1.0e-6 ) ;
}
