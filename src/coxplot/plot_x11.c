#include "coxplot.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*--------------------------------------------------------------------------
  Routines to render a memplot into an X11 window.
----------------------------------------------------------------------------*/

static Display      * old_dpy = NULL ;
static X11_colordef * old_cd  = NULL ;
static Window         old_w   = (Window) 0 ;
static GC             old_GC ;

/*--------------------------------------------------------------------------
  If we have a new X11 display, will get its coloring scheme.
  Note the tacit assumption that all windows on the same
  display in the same program will use the same visual and colormap!
----------------------------------------------------------------------------*/

static void setup_X11_plotting( Display * dpy , Window w )
{
   XGCValues gcv;

   if( old_dpy == dpy ){ old_w = w ; return ; }

   FREE_X11_colordef(old_cd) ;
   old_cd = get_X11_colordef( dpy , w ) ;

   if( old_dpy != NULL ) XFreeGC( old_dpy , old_GC ) ;
   gcv.function   = GXcopy ;
   gcv.fill_style = FillSolid ; /* 21 Mar 2001 */
   old_GC         = XCreateGC( dpy , w , GCFunction|GCFillStyle , &gcv ) ;

   old_dpy = dpy ; old_w = getwin_from_XDBE(dpy,w) ;
   return ;
}

/*---------------------------------------------------------------------------
  Set the background color of a window.  This is necessary since
  neither memplot or plotpak contains the concept of a background color,
  only the concept of the line drawing color.
-----------------------------------------------------------------------------*/

void set_X11_background( Display * dpy , Window w ,
                         unsigned char rr , unsigned char gg , unsigned char bb )
{
   unsigned long pix ;

   if( dpy == NULL || w == (Window) 0 ) return ;

   setup_X11_plotting( dpy , w ) ;
   pix = rgb_to_pixel( rr,gg,bb , old_cd ) ;
   XSetWindowBackground( dpy , getwin_from_XDBE(dpy,w) , pix ) ;
   return ;
}

/*--------------------------------------------------------------------------
   Set a sub-box within a window into which the next X11 plot should
   be scaled.  (0,0,0,0) args means use the whole window.  After
   each drawing (memplot_to_X11_sef), will be reset to the whole window
   anyway. -- 26 Feb 2001 -- RWCox
----------------------------------------------------------------------------*/

static int box_xbot=0 , box_xtop=0 ,
           box_ybot=0 , box_ytop=0  ;

void set_memplot_X11_box( int xbot, int ybot, int xtop, int ytop )
{
   if( xbot < xtop && ybot < ytop ){
      box_xbot = xbot ; box_ybot = ybot ;
      box_xtop = xtop ; box_ytop = ytop ;
   } else {
      box_xbot = box_ybot = box_xtop = box_ytop = 0 ;
   }
}

/*------------------------------------------------------------------------*/

/*------------------------------------------------------------------------*/
/*! Get the layout of a window/pixmap.  [12 Mar 2002]
--------------------------------------------------------------------------*/

static void drawable_geom( Display *dpy , Drawable ddd ,
                           int *width , int *height , int *depth )
{
   int xx,yy ;
   unsigned int ww,hh,bb,dd ;
   Window rr ;

   XGetGeometry( dpy,ddd , &rr,&xx,&yy,&ww,&hh,&bb,&dd ) ;

   if( width  != NULL ) *width  = ww ;
   if( height != NULL ) *height = hh ;
   if( depth  != NULL ) *depth  = dd ;
}

/*--------------------------------------------------------------------------
  Actually do the rendering.
  Plotting will start with line #start and go to #end-1.
  If end <= start, will do from #start to the last one in the plot.
  To do all lines, set start=end=0.
  The mask argument controls operations, and is the bitwise sum of
  these possibilities:
    MEMPLOT_FREE_ASPECT  = allow the aspect ratio to be free
    MEMPLOT_ERASE        = draw white over the lines
----------------------------------------------------------------------------*/

#ifdef LMAX
#undef LMAX
#endif
#define LMAX 1023  /* max number of line segments to draw at once */

static XSegment xseg[LMAX] ;  /* line segment drawing buffer */
static int      nseg = 0 ;    /* number in the buffer */

static void draw_xseg(void) ; /* prototype for function below */

void memplot_to_X11_sef( Display * dpy , Window w , MEM_plotdata * mp ,
                         int start , int end , int mask                )
{
   int ii , nline , same ;
   float old_thick , old_color , new_color , new_thick ;
   float scal,xscal,yscal , xoff,yoff ;
   short x1,y1 , x2,y2 ;  /* X11 screen coords are shorts */
   int skip ;
   int w_width, w_height, w_depth ;  /* 12 Mar 2002 */

   int freee = (mask & MEMPLOT_FREE_ASPECT) != 0 ;  /* 16 Nov 2001 */
   int erase = (mask & MEMPLOT_ERASE      ) != 0 ;

   /*--- check for madness ---*/

   if( dpy == NULL || w == (Window) 0 || mp == NULL ) return ;
   if( start < 0 ) start = 0 ;

   nline = MEMPLOT_NLINE(mp) ;
   if( nline < 1 || start >= nline ) return ;

   if( end <= start || end > nline ) end = nline ;

   /*-- if we have a new X11 Display, get its coloring
        (note the tacit assumption that all windows on the same
         display in the same program will use the same visual and colormap!) --*/

   setup_X11_plotting( dpy , w ) ;

   /*-- 12 Mar 2002: replace use of XGetWindowAttributes with XGetGeometry --*/

   drawable_geom( dpy, getwin_from_XDBE(dpy,w), &w_width,&w_height,&w_depth ) ;

   if( w_depth != old_cd->depth ) return ;  /* this is bad */

   /*--- compute scaling from memplot objective
         coordinates to X11 window coordinates, maintaining aspect ---*/

   if( box_xbot >= box_xtop || box_ybot >= box_ytop ){

      xscal = (w_width -0.001) / mp->aspect ; /* aspect = x-axis objective size */
      yscal = (w_height-0.001) / 1.0 ;        /* 1.0    = y-axis objective size */
      xoff  = yoff = 0.0 ;

   } else {  /* 26 Feb 2001: scale to a given sub-box in the window */

      xscal = box_xtop - box_xbot ;
      yscal = box_ytop - box_ybot ;
      xoff  = box_xbot + 0.0      ;
      yoff  = box_ybot + 0.0      ;
   }

   if( !freee ){                           /* no aspect freedom ==> */
      if( yscal < xscal ) xscal = yscal ;  /* use smaller scaling   */
      else                yscal = xscal ;
   }
   scal = sqrt(fabs(xscal*yscal)) ;

   old_color = -1.0 ;            /* these don't occur naturally */
   old_thick = -THCODE_INVALID ;

   if( erase ){                  /* 16 Nov 2001: erase to white */
      float rr=1.0 , gg=1.0 , bb=1.0 ;
      unsigned long pix ;
      pix = rgb_to_pixel( ZO_TO_TFS(rr), ZO_TO_TFS(gg), ZO_TO_TFS(bb), old_cd ) ;
      XSetForeground( old_dpy , old_GC , pix ) ;
   }

   /*--- loop over lines, scale and plot ---*/

   for( ii=start ; ii < end ; ii++ ){

      skip = 0 ;

      /* check if need to change color or thickness of line */

      new_color = MEMPLOT_COL(mp,ii) ;
      if( !erase && new_color != old_color ){
         float rr=COL_TO_RRR(new_color) ,
               gg=COL_TO_GGG(new_color) , bb=COL_TO_BBB(new_color) ;
         unsigned long pix ;

#if 0
fprintf(stderr,"Changing color to %f %f %f\n",rr,gg,bb) ;
#endif

         draw_xseg() ; /* must draw before changing GC */

         pix = rgb_to_pixel( ZO_TO_TFS(rr), ZO_TO_TFS(gg), ZO_TO_TFS(bb), old_cd ) ;
         XSetForeground( old_dpy , old_GC , pix ) ;
         old_color = new_color ;
      }

      new_thick = MEMPLOT_TH(mp,ii) ;
      if( new_thick < 0.0 ){               /* 21 Mar 2001: negative thickness codes */
         int thc = (int)(-new_thick) ;
         switch( thc ){  /* default is to do nothing (e.g., thd = THCODE_INVALID) */

            case THCODE_RECT:{        /* rectangle */
               short xb,yb , xt,yt ;
               unsigned short w,h ;
               x1 = (short)( xoff + xscal * MEMPLOT_X1(mp,ii)         ) ;
               x2 = (short)( xoff + xscal * MEMPLOT_X2(mp,ii)         ) ;
               y1 = (short)( yoff + yscal * (1.0 - MEMPLOT_Y1(mp,ii)) ) ;
               y2 = (short)( yoff + yscal * (1.0 - MEMPLOT_Y2(mp,ii)) ) ;
               if( x1 < x2 ){ xb=x1; xt=x2; } else { xb=x2; xt=x1; }
               if( y1 < y2 ){ yb=y1; yt=y2; } else { yb=y2; yt=y1; }
               w = xt-xb ; h = yt-yb ;
               if( w || h )
                 XFillRectangle( old_dpy,old_w,old_GC , xb,yb,w,h ) ;
               else
                 XDrawPoint( old_dpy,old_w,old_GC , xb,yb ) ;
               skip = 1 ;
            }
            break ;

            case THCODE_CIRC:{        /* circle */
               int xcor,ycor , xcen,ycen , xrad,yrad ;
               unsigned int ww, hh ;
               xcen = (int)(xoff + xscal * MEMPLOT_X1(mp,ii)         );
               ycen = (int)(yoff + yscal * (1.0 - MEMPLOT_Y1(mp,ii)) );
               xrad = (int)(       xscal * MEMPLOT_X2(mp,ii)         );
               yrad = (int)(       yscal * MEMPLOT_X2(mp,ii)         );
               xcor = xcen - xrad ; ww = 2*xrad ;
               ycor = ycen - yrad ; hh = 2*yrad ;
               if( ww || hh )
                 XDrawArc( old_dpy,old_w,old_GC , xcor,ycor,ww,hh , 0,360*64 ) ;
               else
                 XDrawPoint( old_dpy,old_w,old_GC , xcor,ycor ) ;
               skip = 1 ;
            }
            break ;
         }

      } else if( new_thick != old_thick ){ /* normal case: change line thickness */
         XGCValues gcv ;
         int lw = scal * new_thick ;
         if( lw < 0 ) lw = 0 ;
#if 0
fprintf(stderr,"Changing thickness: old=%f  new=%f\n",old_thick,new_thick) ;
#endif

         draw_xseg() ; /* must draw before changing GC */

         gcv.line_width = lw ;
         gcv.join_style = JoinBevel ;
         XChangeGC( old_dpy , old_GC , GCLineWidth | GCJoinStyle , &gcv ) ;
         old_thick = new_thick ;
      }

      if( nseg == LMAX ) draw_xseg() ;  /* draw if list is full */

      /* scale coords to X11 shorts (also see zzphph.f) */
      /* 26 Feb 2001: xoff,yoff are now variables, instead of 0.499 */

      if( !skip ){
        x1 = (short)( xoff + xscal * MEMPLOT_X1(mp,ii)         ) ;
        x2 = (short)( xoff + xscal * MEMPLOT_X2(mp,ii)         ) ;
        y1 = (short)( yoff + yscal * (1.0 - MEMPLOT_Y1(mp,ii)) ) ;
        y2 = (short)( yoff + yscal * (1.0 - MEMPLOT_Y2(mp,ii)) ) ;

      /* add to segment list */

        xseg[nseg].x1 = x1 ; xseg[nseg].y1 = y1 ;
        xseg[nseg].x2 = x2 ; xseg[nseg].y2 = y2 ; nseg++ ;
      }
   }

   /*-- process any segments left over --*/

   draw_xseg() ;
   set_memplot_X11_box(0,0,0,0) ; /* 26 Feb 2001: clear box */
   return ;
}

/*---------------- draw the line segments stored in xseg -----------------*/

static void draw_xseg(void)
{
   int jbot,jtop , ii,nj ;
   XPoint xpt[LMAX+1] ;

   if( nseg <= 0 ) return ;  /* nothing to do */

#if 0
fprintf(stderr,"draw_xseg: %d segments input.\n",nseg) ;
for( ii=0 ; ii < nseg ; ii++ )
  fprintf(stderr,"  %4d: x1=%4d  y1=%4d   x2=%4d  y2=%4d\n",
          ii , xseg[ii].x1 , xseg[ii].y1 , xseg[ii].x2 , xseg[ii].y2 ) ;
#endif

   jbot = 0 ;
   while( jbot < nseg ){  /* scan the line segment list starting at jbot */

      /* scan forward to find a set of connected lines */

      jtop = jbot+1 ;
      while( jtop < nseg && xseg[jtop-1].x2 == xseg[jtop].x1
                         && xseg[jtop-1].y2 == xseg[jtop].y1 ) jtop++ ;

      /* jbot .. jtop-1 are connected;
         if this is more than one line, draw them together
         so that the X11 server will properly join the lines */

      nj = jtop - jbot ;
      if( nj > 1 ){
         xpt[0].x = xseg[jbot].x1 ; xpt[0].y = xseg[jbot].y1 ;
         for( ii=0 ; ii < nj ; ii++ ){
            xpt[ii+1].x = xseg[jbot+ii].x2 ; xpt[ii+1].y = xseg[jbot+ii].y2 ;
         }
         XDrawLines( old_dpy,old_w,old_GC , xpt,nj+1 , CoordModeOrigin ) ;

#if 0
fprintf(stderr,"draw_xseg: XDrawLines for %d\n",nj) ;
#endif

         jbot = jtop ; continue ;  /* start the while loop over */
      }

      /* jbot is not connected to jbot+1;
         scan forward to find a set of disconnected lines */

      while( jtop < nseg &&
             ( xseg[jtop-1].x2 != xseg[jtop].x1 ||
               xseg[jtop-1].y2 != xseg[jtop].y1   ) ) jtop++ ;

      /* jbot .. jtop-1 are disconnected, so draw them as such */

      XDrawSegments( old_dpy,old_w,old_GC , xseg+jbot , jtop-jbot ) ;

#if 0
fprintf(stderr,"draw_xseg: XDrawSegments for %d\n",jtop-jbot) ;
#endif

      jbot = jtop ; continue ;  /* start the while loop over */
   }

   nseg = 0 ; return ;
}

/*------------------------------------------------------------------------
  Returns position of highest set bit in 'ul' as an integer (0-31),
  or returns -1 if no bit is set.
--------------------------------------------------------------------------*/

static int highbit(unsigned long ul)
{
  int i;  unsigned long hb;

  hb = 0x80;  hb = hb << 24;   /* hb = 0x80000000UL */
  for (i=31; ((ul & hb) == 0) && i>=0;  i--, ul<<=1);
  return i;
}

/*-------------------------------------------------------------------------
  Return an X11 pixel value that represents the given color.
  Inputs rr,gg,bb are from 0 to 255.  Output is an unsigned long,
  the standard X11 format for specifying colors to a GC.
  Note that if the result is to be put into an XImage, further
  bit manipulation must be done on the result.
---------------------------------------------------------------------------*/

unsigned long rgb_to_pixel( unsigned char rr , unsigned char gg ,
                            unsigned char bb , X11_colordef * cd )
{
   /*--- TrueColor case: make color by appropriate bit twiddling ---*/

   if( cd->class == TrueColor ){
      unsigned long r , g , b , rgb ;

      r = (cd->rrshift<0) ? (rr<<(-cd->rrshift))
                          : (rr>>cd->rrshift)   ; r = r & cd->rrmask ;

      g = (cd->ggshift<0) ? (gg<<(-cd->ggshift))
                          : (gg>>cd->ggshift)   ; g = g & cd->ggmask ;

      b = (cd->bbshift<0) ? (bb<<(-cd->bbshift))
                          : (bb>>cd->bbshift)   ; b = b & cd->bbmask ;

      rgb = r | g | b ;  /* assemble color from components */
      return rgb ;
   }

   /*--- PseudoColor case: find closest match in colormap.
         Red, green, and blue are weighted according
         to their importance to the human visual system. ---*/

#define RW 2  /* the weights alluded to above */
#define GW 4
#define BW 1

   if( cd->class == PseudoColor ){
      int ii , rdif,gdif,bdif,dif , ibest,dbest ;

      rdif = cd->rr[0] - rr ;
      gdif = cd->gg[0] - gg ;
      bdif = cd->bb[0] - bb ; dif = RW*abs(rdif)+GW*abs(gdif)+BW*abs(bdif) ;
      if( dif == 0 ) return 0 ;

      ibest = 0 ; dbest = dif ;
      for( ii=1 ; ii < cd->ncolors ; ii++ ){
         rdif = cd->rr[ii] - rr ;
         gdif = cd->gg[ii] - gg ;
         bdif = cd->bb[ii] - bb ; dif = RW*abs(rdif)+GW*abs(gdif)+BW*abs(bdif) ;
         if( dif == 0 ) return ii ;
         if( dif < dbest ){ ibest = ii ; dbest = dif ; }
      }
      return ibest ;
   }

   /*--- Illegal case! ---*/

   return 0 ;  /* always valid */
}

/*-------------------------------------------------------------------------*/

static volatile int xwasbad ;  /* 13 Mar 2002 */

typedef int (*xhandler)(Display *, XErrorEvent *) ;

static int qhandler( Display *dpy , XErrorEvent *xev )
{
   xwasbad = 1 ; return 0 ;
}

/*-------------------------------------------------------------------------
   Return a structure that defines the colors available for the given
   window.  This only works for PseudoColor and TrueColor visuals.
   Returns NULL if an error occurs.
---------------------------------------------------------------------------*/

X11_colordef * get_X11_colordef( Display * display , Window w )
{
   Status sss ;
   XWindowAttributes xwat ;
   XColor * xcol ;
   XVisualInfo vinfo , * vin ;
   X11_colordef * cd ;          /* will be the output */
   int count , ii ;
   xhandler old_handler ;       /* 13 Mar 2002 */

   /*--- sanity check ---*/

   if( display == NULL || w == (Window) 0 ) return NULL ;

   /*--- get window attributes ---*/

   /* 13 Mar 2002: deal with the error that occurs
                   when the Window is really a Pixmap */

   xwat.depth = 0 ;
   old_handler = XSetErrorHandler(qhandler) ; xwasbad = 0 ;

   XGetWindowAttributes( display, getwin_from_XDBE(display,w), &xwat ) ;

   (void) XSetErrorHandler(old_handler) ;

   if( xwasbad ){
      int xx,yy ; unsigned int ww,hh,bb,dd ; Window rr ;
      XGetGeometry( display,w , &rr,&xx,&yy,&ww,&hh,&bb,&dd ) ;
      XGetWindowAttributes( display, rr , &xwat ) ;
   }
   if( xwat.depth == 0 ) return NULL ;   /* bad news */

   /*--- get information about the window's Visual ---*/

   vinfo.visualid = XVisualIDFromVisual(xwat.visual) ;
   vin = XGetVisualInfo( display , VisualIDMask , &vinfo , &count ) ;
   if( count == 0 || vin == NULL ) return NULL ;

   /*--- PseudoColor case ---*/

   if( vin->class == PseudoColor ){
      int iz ;

      /* create output */

      cd = (X11_colordef *) malloc( sizeof(X11_colordef) ) ;
      cd->class = PseudoColor ;
      cd->depth = vin->depth ;

      /* get all the colors in the colormap */

      count = vin->colormap_size ;
      xcol  = (XColor *) malloc( sizeof(XColor) * count ) ;
      for( ii=0 ; ii < count ; ii++ ) xcol[ii].pixel = ii ;

      XQueryColors( display , xwat.colormap , xcol , count ) ;

      /* store them in the output, truncated to 8 bits */

      cd->ncolors = count ;
      cd->rr      = (unsigned char *) malloc( count ) ;
      cd->gg      = (unsigned char *) malloc( count ) ;
      cd->bb      = (unsigned char *) malloc( count ) ;

      for( ii=0 ; ii < count ; ii++ ){
         cd->rr[ii] = xcol[ii].red   >> 8 ;
         cd->gg[ii] = xcol[ii].green >> 8 ;
         cd->bb[ii] = xcol[ii].blue  >> 8 ;
      }

      /* find first all zero color; discard others at end of colormap */

      for( iz=0 ; iz < count ; iz++ )
         if( cd->rr[iz] == 0 && cd->gg[iz] == 0 && cd->bb[iz] == 0 ) break ;

      if( iz < count-1 ){  /* if found one before the end */

         for( ii=count-1 ; ii > iz ; ii-- )  /* scan backwards */
            if( cd->rr[ii] != 0 || cd->gg[ii] != 0 || cd->bb[ii] != 0 ) break ;

         count = ii+1 ;  /* number of colors left */

         if( count == 1 ){ /* colormap is all black?! */
            free(xcol) ; XFree(vin) ; FREE_X11_colordef(cd) ; return NULL ;
         }

         cd->ncolors = count ;
      }

      free(xcol) ; XFree(vin) ; return cd ;
   }

   /*--- TrueColor case ---*/

   if( vin->class == TrueColor ){

      /* create output */

      cd = (X11_colordef *) malloc( sizeof(X11_colordef) ) ;
      cd->class = TrueColor ;
      cd->depth = vin->depth ;

      cd->rrmask  = vin->red_mask ;            /* bit masks for color  */
      cd->ggmask  = vin->green_mask ;          /* storage inside pixel */
      cd->bbmask  = vin->blue_mask ;
      cd->rrshift = 7 - highbit(cd->rrmask) ;  /* shift puts high bit of  */
      cd->ggshift = 7 - highbit(cd->ggmask) ;  /* a color byte into place */
      cd->bbshift = 7 - highbit(cd->bbmask) ;  /* +shift == >> ; - == <<  */

      cd->rr = cd->gg = cd->bb = NULL ;        /* not used */

      XFree(vin) ; return cd ;
   }

   /*--- Illegal Visual class! ---*/

   XFree(vin) ; return NULL ;
}

#ifdef HAVE_XDBE
/*----------------------------------  24 Jan 1999  -------------------------------*/
void init_XDBE( Display * dpy )
{
   int sss , ii , jj ;
   char * ec ;

   if( use_xdbe >= 0 ) return ;

   ec = getenv("AFNI_NO_XDBE") ;  /* 28 Jan 2000 - add this environment variable */
   if( ec != NULL && (ec[0]=='Y' || ec[0]=='y') ){
      use_xdbe = 0 ;
   } else {
      sss = (int) XdbeQueryExtension( dpy , &ii , &jj ) ;
      use_xdbe = (sss != 0 ) ;
   }
   return ;
}

Window getwin_from_XDBE( Display * dpy , Drawable w )
{
   XdbeBackBufferAttributes * bat ;
   Window bw ;

   if( w == (Window) 0 || use_xdbe <= 0 ) return w ;

   bat = XdbeGetBackBufferAttributes( dpy , w ) ;
   bw  = bat->window ; XFree(bat) ;
   if( bw == (Window) 0 ) bw = w ;
   return bw ;
}
#endif  /* HAVE_XDBE */
