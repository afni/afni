/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#define MAIN

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <stdio.h>

#include <Xm/XmAll.h>
#include "mrilib.h"

static char * vcl[] =  { "StaticGray"  , "GrayScale" , "StaticColor" ,
                         "PseudoColor" , "TrueColor" , "DirectColor"  } ;

static XImage * xim    = NULL ;
static int      xim_ww = 0 ;
static int      xim_hh = 0 ;

#undef USE_TRUECOLOR

#define USE_PIXMAP
#ifdef  USE_PIXMAP
# define WANT_LOGO_BITMAP
# include "logo.h"
#endif

XImage * rgb_to_XImage( Display * dis , XVisualInfo * vin , MRI_IMAGE * im ) ;

void fred_CB( Widget w , XtPointer cd , XtPointer cb ){ exit(0); }

void elvis_CB( Widget w , XtPointer cd , XtPointer cb )
{
   static int needGC = 1 ;
   static  GC myGC ;
   XmDrawingAreaCallbackStruct * cbs = (XmDrawingAreaCallbackStruct *) cb ;
   XExposeEvent * ev = (XExposeEvent *) cbs->event ;
   Dimension nx=0 , ny=0 ;
   int ii , jj ;

   if( cbs->reason != XmCR_EXPOSE || ev->count > 0 ) return ;

   if( needGC ){
     XGCValues  gcv;
     gcv.function = GXcopy ;
     myGC  = XCreateGC( XtDisplay(w) , XtWindow(w) , GCFunction , &gcv ) ;
     needGC = 0 ;
   }

   XtVaGetValues( w , XmNwidth  , &nx , XmNheight , &ny , NULL ) ;

#ifdef USE_TRUECOLOR
   ii = 0 ;
   do{
      jj = 0 ;
      do{
         XPutImage( XtDisplay(w),XtWindow(w),myGC,xim,0,0,ii,jj,xim_ww,xim_hh) ;
         jj += xim_hh + 4 ;
      } while( jj < ny ) ;
      ii += xim_ww ;
   } while( ii < nx ) ;
#else
# ifdef USE_PIXMAP
   ii = 0 ;
   do{ jj = 0 ;
       do{
          XCopyArea( XtDisplay(w),logo_pixmap , XtWindow(w),myGC ,
                     0,0,logo_width,logo_height , ii,jj ) ;
         jj += logo_height + 4 ;
      } while( jj < ny ) ;
      ii += logo_width ;
   } while( ii < nx ) ;
# endif /* USE_PIXMAP */
#endif /* USE_TRUECOLOR */

   return ;
}

int main( int argc , char * argv[] )
{
        XtAppContext    app;            /* the application context */
        Widget          top;            /* toplevel widget */
        Display         *dpy;           /* display */
        Colormap        colormap;       /* created colormap */
        XVisualInfo     vinfo;          /* template for find visual */
        Visual          *vis ;          /* the Visual itself */
        XVisualInfo     *vinfo_list;    /* returned list of visuals */
        int             count;          /* number of matchs (only 1?) */
        int             vid , stat ;
        Widget          fred , fff ;

        top = XtVaAppInitialize( &app , "test" , NULL , 0 , &argc , argv , NULL , NULL ) ;
        dpy = XtDisplay (top);

#ifndef USE_TRUECOLOR
        stat = XMatchVisualInfo( dpy,XScreenNumberOfScreen(XtScreen(top)),
                                 8,PseudoColor,&vinfo ) ;
        if( stat == 0 ){ printf("no 8 bit visual\n") ; exit(1) ; }
#else
        vid = strtol( argv[1] , NULL , 0 ) ;
        vinfo.visualid = (VisualID) vid ;
        vinfo_list = XGetVisualInfo (dpy, VisualIDMask, &vinfo, &count);
        if( count == 0 || vinfo_list == NULL ){fprintf(stderr,"no match\n");exit(1);}
        vinfo = vinfo_list[0] ;
#endif
        vid = vinfo.visualid ;
        vis = vinfo.visual ;

        colormap = XCreateColormap( dpy, RootWindowOfScreen(XtScreen(top)) ,
                                    vis , AllocNone ) ;

        XtVaSetValues( top ,
                          XtNborderColor , 0 ,
                          XtNbackground  , 0 ,
                          XtNdepth       , vinfo.depth ,
                          XtNcolormap    , colormap ,
                          XtNvisual      , vis ,
                       NULL ) ;

        fff = XtVaCreateWidget( "dialog" , xmFormWidgetClass , top ,
                                   XmNborderWidth , 0 ,
                                NULL ) ;

#ifndef LABEL_ARG
#define LABEL_ARG(str) \
  XtVaTypedArg , XmNlabelString , XmRString , (str) , strlen(str)+1
#endif

        fred = XtVaCreateManagedWidget( "dialog" , xmPushButtonWidgetClass , fff ,
                                          LABEL_ARG("Jumpback") ,
                                          XmNtopAttachment    , XmATTACH_FORM ,
                                          XmNleftAttachment   , XmATTACH_FORM ,
                                          XmNrightAttachment  , XmATTACH_FORM ,
                                         NULL ) ;
        XtAddCallback( fred , XmNactivateCallback , fred_CB , NULL ) ;

        fred = XtVaCreateManagedWidget( "dialog" , xmDrawingAreaWidgetClass , fff ,
                                          XmNtopAttachment    , XmATTACH_WIDGET ,
                                          XmNtopWidget        , fred ,
                                          XmNleftAttachment   , XmATTACH_FORM ,
                                          XmNrightAttachment  , XmATTACH_FORM ,
                                          XmNbottomAttachment , XmATTACH_FORM ,
                                        NULL ) ;

        XtAddCallback( fred , XmNexposeCallback , elvis_CB , NULL ) ;

#ifdef USE_TRUECOLOR
        { MRI_IMAGE * im ;
          im = mri_read_ppm( "bob.ppm" ) ;
          xim = rgb_to_XImage( XtDisplay(top) , &vinfo , im ) ;
          xim_ww = im->nx ; xim_hh = im->ny ;
          mri_free(im) ;
        }
#else
        xim_ww = xim_hh = 77 ;
#endif

        XtVaSetValues( top ,
                         XmNwidth , xim_ww ,
                         XmNheight , xim_hh+40 ,
                       NULL ) ;

#ifdef USE_PIXMAP
   {  Pixel bg_pix=0  , fg_pix=0  ;
# define ICON_bg bg_pix
# define ICON_fg fg_pix

      XtVaGetValues( fred ,
                       XmNforeground , &bg_pix ,  /* note reversal of roles here! */
                       XmNbackground , &fg_pix ,
                     NULL ) ;

      logo_pixmap = XCreatePixmapFromBitmapData(
                        XtDisplay(top) ,
                        RootWindowOfScreen(XtScreen(top)) ,
                        logo_bits , logo_width , logo_height ,
                        fg_pix , bg_pix ,
                        DefaultDepthOfScreen(XtScreen(top)) ) ;

      XtVaSetValues( top , XmNiconPixmap , logo_pixmap , NULL ) ;
   }
#endif


        XtManageChild(fff) ;
        XtRealizeWidget(top); NI_sleep(1);
        XtAppMainLoop(app);

        exit(0); /* never reached */
}

#ifdef USE_TRUECOLOR
/*---------------------------------------------------------------------------
   Create an XImage from an RGB image.  Adapted from program "xv".
   The output of this can be XPutImage-d to a window.
-----------------------------------------------------------------------------*/

static int highbit(unsigned long ul) ;  /* prototype */

XImage * rgb_to_XImage( Display * dis , XVisualInfo * vin , MRI_IMAGE * im )
{
   unsigned long r, g, b, rmask, gmask, bmask ;
   int           rshift, gshift, bshift, bperpix, bperline, border, i,j ;
   int           wide, high ;
   byte         *imagedata, *lip, *ip, *pp ;
   XImage       *xim = NULL ;
   int          *xcol ;

   /* check inputs */

#if defined(__cplusplus) || defined(c_plusplus)
   if( vin==NULL || vin->c_class!=TrueColor || im==NULL || im->kind!=MRI_rgb ){
#else
   if( vin==NULL || vin->class!=TrueColor || im==NULL || im->kind!=MRI_rgb ){
#endif
      fprintf(stderr,"\a\n*** ILLEGAL input to rgb_to_XImage\n") ;
      sleep(1) ; exit(1) ;
   }

   /* get color masks and shifts to put high bit of color byte into place */

   rmask = vin->red_mask   ; rshift = 7 - highbit(rmask) ;
   gmask = vin->green_mask ; gshift = 7 - highbit(gmask) ;
   bmask = vin->blue_mask  ; bshift = 7 - highbit(bmask) ;

   /* input image dimensions */

   wide = im->nx ;
   high = im->ny ;

   /* make output XImage */

   xim = XCreateImage( dis , vin->visual , vin->depth , ZPixmap, 0, NULL,
                       wide,  high, 32, 0) ;

   bperline = xim->bytes_per_line ;
   bperpix  = xim->bits_per_pixel ;
   border   = xim->byte_order ;

   if(bperpix != 8 && bperpix != 16 && bperpix != 24 && bperpix != 32){
      fprintf(stderr,"\a\n*** rgb_to_XImage: can't use %d-bit TrueColor\n",bperpix) ;
      sleep(1) ; exit(1) ;
   }

   /* make output image array */

   imagedata = (byte *) XtMalloc((size_t) (high * bperline)) ;
   xim->data = (char *) imagedata ;

   lip = imagedata ;       /* pointer to row of output image array */
   pp  = MRI_RGB_PTR(im) ; /* pointer to input image data */

   xcol = (int *) malloc(sizeof(int) * wide) ; /* row of output colors */

   /*-- loop over image --*/

   for (i=0; i<high; i++, lip+=bperline) {  /* loop over rows */

      for (j=0, ip=lip; j<wide; j++) {      /* load color for row #i */

        r = *pp++ ;  g = *pp++ ;  b = *pp++ ;  /* get input RGB byte values */

        /* shift each component to the correct position, and mask it off */

        r = (rshift<0) ? (r<<(-rshift)) : (r>>rshift) ; r = r & rmask ;
        g = (gshift<0) ? (g<<(-gshift)) : (g>>gshift) ; g = g & gmask ;
        b = (bshift<0) ? (b<<(-bshift)) : (b>>bshift) ; b = b & bmask ;

        xcol[j] = r | g | b ;  /* assemble color for this pixel */
      }

      /* now put all colors for row #i into output image array,
         allowing for most possible bytes/pixel and byte ordering stupidities */

      switch( bperpix ){
           case 32:
             if (border == MSBFirst)
                for( j=0 ; j < wide ; j++ ){
                   *ip++ = (xcol[j]>>24) & 0xff ;
                   *ip++ = (xcol[j]>>16) & 0xff ;
                   *ip++ = (xcol[j]>>8)  & 0xff ;
                   *ip++ =  xcol[j]      & 0xff ;
                 }
             else
                for( j=0 ; j < wide ; j++ ){
                   *ip++ =  xcol[j]      & 0xff ;
                   *ip++ = (xcol[j]>>8)  & 0xff ;
                   *ip++ = (xcol[j]>>16) & 0xff ;
                   *ip++ = (xcol[j]>>24) & 0xff ;
                 }
           break ;

           case 24:
             if (border == MSBFirst)
                for( j=0 ; j < wide ; j++ ){
                  *ip++ = (xcol[j]>>16) & 0xff ;
                  *ip++ = (xcol[j]>>8)  & 0xff ;
                  *ip++ =  xcol[j]      & 0xff ;
                }
             else
                for( j=0 ; j < wide ; j++ ){
                  *ip++ =  xcol[j]      & 0xff ;
                  *ip++ = (xcol[j]>>8)  & 0xff ;
                  *ip++ = (xcol[j]>>16) & 0xff ;
                }
           break ;

           case 16:
             if (border == MSBFirst)
                for( j=0 ; j < wide ; j++ ){
                  *ip++ = (xcol[j]>>8)  & 0xff ;
                  *ip++ =  xcol[j]      & 0xff ;
                }
             else
                for( j=0 ; j < wide ; j++ ){
                  *ip++ =  xcol[j]      & 0xff ;
                  *ip++ = (xcol[j]>>8)  & 0xff ;
                }
           break ;

           case 8:
                for( j=0 ; j < wide ; j++ )
                   *ip++ = xcol[j] & 0xff ;
           break ;
         }
    } /* end of loop over rows */

   free(xcol) ;
   return xim ;
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
#endif

/*----------  Fix a Linux stupidity  ------------------------------------*/

#include "machdep.h"
#ifdef NEED_XSETLOCALE
#include <locale.h>
char * _Xsetlocale( int category, const char * locale)
{ return setlocale(category,locale) ; }
#endif
