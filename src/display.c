#include "display.h"
#include "mrilib.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

static char * x11_vcl[] =  { "StaticGray"  , "GrayScale" , "StaticColor" ,
                             "PseudoColor" , "TrueColor" , "DirectColor"  } ;

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
   Setup the number of bytes per pixel.  For depth 24, this might
   be 3 or 4, depending on the server.  RWCox -- 23 Aug 1998.
---------------------------------------------------------------------------*/

static void setup_byper( MCW_DC * dc )
{
   XPixmapFormatValues * xpv ;
   int                  nxpv = 0 , ii ;

   xpv = XListPixmapFormats( dc->display , &nxpv ) ;

   if( xpv == NULL || nxpv == 0 ){
      dc->byper = dc->depth / 8 ; dc->bypad = 1 ;  /* defaults */
      return ;
   }

   /* scan for this depth in the array of pixmap formats */

   for( ii=0 ; ii < nxpv ; ii++ ){
      if( xpv[ii].depth == dc->depth ){
         dc->byper = xpv[ii].bits_per_pixel / 8 ;  /* bytes, not bits */
         dc->bypad = xpv[ii].scanline_pad   / 8 ;
         XFree(xpv) ; return ;
      }
   }

   dc->byper = dc->depth / 8 ; dc->bypad = 1 ;  /* defaults */
   XFree(xpv) ; return ;
}

/*------------------------------------------------------------------------
  Create and initialize a new MCW_DC structure.

    wid  = widget used to get info about Display, etc.
    ncol = number of colors to use for images
    novr = number of overlay colors
    covr = array of strings of overlay color names  [used for XAlloc...]
    lovr = array of strings of overlay color labels [used for display]
    gam  = gamma value to use

  22 Aug 1998: Modified to support TrueColor visuals,
               as well as the original PseudoColor -- RWCox.

  14 Sep 1998: Modified to add argument
    newcmap = if nonzero, create a new Colormap;
              if zero, use the default Colormap for the display
------------------------------------------------------------------------*/

static MCW_DCOV * only_ovc = NULL ;  /* Dec 1997 */

MCW_DC * MCW_new_DC( Widget wid , int ncol ,
                     int novr , char * covr[] , char * lovr[] ,
                     double gam , int newcmap )
{
   MCW_DC * dc ;
   int ok , ii , new_ovc ;
   unsigned int nplmsk = 0 ;  /* dummy arguments for XAllocColorCells */
   unsigned long plane_masks[1] ;

   if( ncol < 4 || novr < 0 || ncol > MAX_COLORS || novr > MAX_COLORS ){
      fprintf(stderr,"\n*** MCW_new_DC: ILLEGAL number of colors: %d %d\n",ncol,novr) ;
      exit(1) ;
   }

   dc = myXtNew(MCW_DC) ;

   dc->appcontext = XtWidgetToApplicationContext( wid ) ;
   dc->display    = XtDisplay( wid ) ;
   dc->screen     = XtScreen( wid ) ;
   dc->screen_num = XScreenNumberOfScreen(   dc->screen ) ;
   dc->visual     = DefaultVisualOfScreen(   dc->screen ) ;
   dc->origGC     = DefaultGCOfScreen(       dc->screen ) ;
   dc->planes     = PlanesOfScreen(          dc->screen ) ;
   dc->depth      = DefaultDepthOfScreen(    dc->screen ) ;

   dc->cdef       = NULL ;  /* 11 Feb 1999: will be loaded later */

   setup_byper(dc) ;        /* 23 Aug 1998 */

   dc->default_colormap = DefaultColormapOfScreen( dc->screen ) ; /* 01 Sep 1998 */

   dc->colormap = DefaultColormapOfScreen( dc->screen ) ; /* may be changed later */

   dc->parent_widget = wid ;  /* 06 Oct 1996 */

   /** 07 Aug 1998: get more information about the visual **/

   { XVisualInfo vinfo , * vinfo_list ;
     int count ;

     dc->visual_id  = XVisualIDFromVisual( dc->visual ) ;
     vinfo.visualid = dc->visual_id ;
     vinfo_list     = XGetVisualInfo(dc->display,VisualIDMask,&vinfo,&count) ;
     if( count > 0 && vinfo_list != NULL ){
        dc->visual_info       = vinfo_list ;
        dc->visual_redmask    = dc->visual_info->red_mask ;
        dc->visual_greenmask  = dc->visual_info->green_mask ;
        dc->visual_bluemask   = dc->visual_info->blue_mask ;
        dc->visual_redshift   = 7 - highbit(dc->visual_redmask) ;
        dc->visual_greenshift = 7 - highbit(dc->visual_greenmask) ;
        dc->visual_blueshift  = 7 - highbit(dc->visual_bluemask) ;
        dc->visual_class      = dc->visual_info->class ;

        if( dc->visual_class != PseudoColor &&
            dc->visual_class != TrueColor      ){

           fprintf(stderr,"\n\n"
                          " ** The default X11 visual type on your computer is set to %s.\n"
                          " ** AFNI programs only work with PseudoColor or TrueColor visuals.\n"
                          " ** You must have your superuser modify your system's setup.\a\n" ,
                   x11_vcl[dc->visual_class] ) ;

           dc->visual_class = PseudoColor ;  /* let the program fail later */
        }

        if( dc->visual_class == TrueColor ){
           static int done = 0 ;
           if( !done )
              fprintf(stderr,
                 "\n"
                 " ** The default X11 visual type on your computer is %d bit TrueColor.\n"
                 " ** Support for this is experimental.  AFNI was developed to use 4..12\n"
                 " ** bit PseudoColor visuals for image display -- RW Cox, 22 Aug 1998.\n" ,
                 dc->depth ) ;
           done = 1 ;
        }

#if 0
        fprintf(stderr,"\n"
                       "DC: redmask=%lx greenmask=%lx bluemask=%lx\n"
                       "    redshift=%d greenshift=%d blueshift=%d\n"
                       "    class=%d=%s depth=%d\n",
                dc->visual_redmask , dc->visual_greenmask , dc->visual_bluemask ,
                dc->visual_redshift , dc->visual_greenshift , dc->visual_blueshift ,
                dc->visual_class , x11_vcl[dc->visual_class] , dc->depth ) ;
#endif

     } else {                            /* should never occur! */
        dc->visual_info  = NULL ;
        dc->visual_class = PseudoColor ; /* we hope */
     }
   }

#if 0
 {  long reqmax ;
    reqmax = XMaxRequestSize(dc->display) ;
    printf("max X11 request size = %d\n",reqmax) ;
 }
#endif

#define DEPTH_BOT  4
#define DEPTH_TOP 32

   if( dc->depth < DEPTH_BOT || dc->depth > DEPTH_TOP ){
      fprintf(stderr,"\n\n"
                     " ** Your X11 display is set to %d bitplanes for image display.\n"
                     " ** AFNI programs can only deal with between %d and %d bitplanes.\n"
                     " ** You must have your superuser modify your system's setup.\a\n" ,
              dc->depth , DEPTH_BOT , DEPTH_TOP ) ;
      exit(1) ;
   }

   dc->width   = WidthOfScreen(  dc->screen ) ;
   dc->height  = HeightOfScreen( dc->screen ) ;

   dc->ncol_im = ncol ;
   dc->gamma   = dc->gamma_init = gam  ;

   if( dc->visual_class == PseudoColor ){                  /* 22 Aug 1998 */

      if( newcmap ){                                       /* 14 Sep 1998 */
         int ncold , cc ;
         XColor xcold ;

         /* make a new colormap */

         dc->colormap = XCreateColormap( dc->display , RootWindowOfScreen(dc->screen) ,
                                         dc->visual  , AllocNone ) ;

         /* allocate some colors from the old one (to reduce flashing) */

#define NREUSE 9
         ncold = dc->visual_info->colormap_size ;
         if( ncold > NREUSE ) ncold = NREUSE ;
         for( cc=0 ; cc < ncold ; cc++ ){
            xcold.pixel = cc ;
            XQueryColors( dc->display , dc->default_colormap , &xcold , 1 ) ;
            XAllocColor( dc->display , dc->colormap , &xcold ) ;
         }
      }

      ok = XAllocColorCells( dc->display , dc->colormap ,
                             True , plane_masks , nplmsk ,
                             dc->pix_im , dc->ncol_im ) ;

      if( ! ok ){
         fprintf(stderr,
                 "\a\n** XAllocColorCells fails for %d colors\n",dc->ncol_im) ;
         fprintf(stderr,
                 "\n** try the -ncolor option to reduce # of colors\n");
         exit(1) ;
      }

      dc->pix_im_ready = 1 ;
   } else if( dc->visual_class == TrueColor ){
      dc->pix_im_ready = 0 ;
   }

   DC_init_im_col( dc ) ;
   DC_init_im_gry( dc ) ;

   dc->use_xcol_im = False ;
   DC_set_image_colors( dc ) ;  /* 22 Aug 1998: replaces XStoreColors */

   /* set up overlay colors from list of names
      (since the XImage routines use negative indices
       to indicate overlays, the 0th overlay color is not used) */

   /* Dec 1997: put all overlay stuff into a single place */

   new_ovc = 0 ;
   if( only_ovc == NULL ){ only_ovc = myXtNew(MCW_DCOV) ; new_ovc = 1 ; }
   dc->ovc = only_ovc ;

   if( new_ovc ){
      only_ovc->xcol_ov[0]  = dc->xgry_im[0] ;
      only_ovc->pix_ov[0]   = dc->pix_im[0] ;
      only_ovc->name_ov[0]  = XtNewString("none") ;
      only_ovc->label_ov[0] = only_ovc->name_ov[0] ;
      only_ovc->ncol_ov     = 1 ;
   }

   for( ii=0 ; ii < novr ; ii++ ){

     ok = DC_add_overlay_color( dc , covr[ii] , lovr[ii] ) ;

     if( ok < 0 )
        fprintf(stderr,
          "\n*** can't get X11 colormap entry for overlay color %s" , lovr[ii] ) ;
#ifdef DISPLAY_DEBUG
     else {
        printf("\n*** overlay color %s has pixel %d at index %d" ,
               dc->ovc->name_ov[ok] , (int)dc->ovc->pix_ov[ok] , ok ) ;
        fflush(stdout) ;
     }
#endif
   }
   OVC_mostest( dc->ovc ) ;

   /*-- May 1996: create new GC for use with text and graphics --*/

   { XGCValues  gcv;
     int ifont ;
     XFontStruct * mfinfo = NULL ;
     char * xdef ;

     gcv.function = GXcopy ;
     dc->myGC     = XCreateGC( dc->display,
                               RootWindowOfScreen(dc->screen) ,
                               GCFunction , &gcv ) ;

     xdef = XGetDefault(dc->display,"AFNI","gfont") ;
     if( xdef != NULL )
        mfinfo = XLoadQueryFont(dc->display,xdef) ;

     if( mfinfo == NULL ){
        for( ifont=0 ; tfont_hopefuls[ifont] != NULL ; ifont++ ){
           mfinfo = XLoadQueryFont(dc->display, tfont_hopefuls[ifont]) ;
           if( mfinfo != NULL ) break ;
        }
     }
     if( mfinfo == NULL ){
        fprintf(stderr,
                "\n*** Cannot load any text fonts in display.c ***\n" ) ;
     } else {
        XSetFont( dc->display , dc->myGC , mfinfo->fid ) ;
     }
     XSetForeground(dc->display , dc->myGC , dc->ovc->pixov_darkest ) ;
     XSetBackground(dc->display , dc->myGC , dc->ovc->pixov_brightest ) ;
     dc->myFontStruct = mfinfo ;
   }

   dc->parent = dc->aux = NULL ;

#ifdef DISPLAY_DEBUG
   printf("\n") ;
#endif

#if 0
   reload_DC_colordef( dc ) ;  /* 11 Feb 1999 */
#else
   dc->cdef = NULL ;
#endif

   return dc ;
}

/*-----------------------------------------------------------------------
   Set the image display to grayscale
-------------------------------------------------------------------------*/

void DC_palette_setgray( MCW_DC * dc )
{
   dc->use_xcol_im = False ;
   DC_set_image_colors( dc ) ;  /* 22 Aug 1998 */
   return ;
}

/*-----------------------------------------------------------------------
   Set the image display to colorscale
-------------------------------------------------------------------------*/

void DC_palette_setcolor( MCW_DC * dc )
{
   dc->use_xcol_im = True ;
   DC_set_image_colors( dc ) ;  /* 22 Aug 1998 */
   return ;
}

/*-----------------------------------------------------------------------
   Restore the color and grayscale palettes to their defaults
-------------------------------------------------------------------------*/

void DC_palette_restore( MCW_DC * dc , double new_gamma )
{
   dc->gamma = (new_gamma > 0 ) ? new_gamma : dc->gamma_init ;
   DC_init_im_col( dc ) ;
   DC_init_im_gry( dc ) ;
   DC_set_image_colors( dc ) ;  /* 22 Aug 1998 */
}

/*-----------------------------------------------------------------------
   Initialize the grayscale image palette.
   Modified 22 Aug 1998 for TrueColor support.
-------------------------------------------------------------------------*/

double mypow( double x , double y )  /* replaces the math library pow */
{
   double b ;
   if( x <= 0.0 ) return 0.0 ;
   if( y == 1.0 ) return x ;
   b = log(x) ; b = exp( y*b ) ;
   return b ;
}

void DC_init_im_gry( MCW_DC * dc )
{
   int i, k, m, nc ;
   float a , gamm , b ;

   nc   = dc->ncol_im ;
   gamm = dc->gamma ;
   a    = 200. / nc ;

   for (i=0; i < nc ; i++) {
      b = log( (a*i+55.0)/255.0 ) ;   /* The code that used to be here */
      b = exp( gamm * b ) ;           /* (using pow) was replaced due  */
      k = (int)( 255.0 * b + 0.5 ) ;  /* to some bug in gcc on Linux.  */

      m = BYTE_TO_INTEN(k) ;

      dc->xint_im[i]       = m ;
      dc->xgry_im[i].red   = m;
      dc->xgry_im[i].green = m;
      dc->xgry_im[i].blue  = m;
      dc->xgry_im[i].flags = DoRed|DoGreen|DoBlue;

      if( dc->visual_class == PseudoColor )        /* 22 Aug 1998 */
         dc->xgry_im[i].pixel = dc->pix_im[i];
   }

   return ;
}

/*-----------------------------------------------------------------------
   Initialize the color image palette.
   Modified 22 Aug 1998 for TrueColor support.
-------------------------------------------------------------------------*/

void DC_init_im_col( MCW_DC * dc )
{
   double da, an, c, s, sb, cb, ak, ab , a1,a2 , gamm ;
   int i, r, g, b , nc ;

   a1 = 0.0   ;  /* range of spectrum -- hardwired for now */
   a2 = 240.0 ;

   nc   = dc->ncol_im ;
   gamm = dc->gamma ;

   ak = 105.; s  = 150.; c  = s/60.;
   ab = 65.;  sb = 190.; cb = s/60.;

   an = a1;   da = (a2 - a1)/nc ; an = an-da+360.;

   for( i=0 ; i < nc ; i++ ){

     an += da; an = fmod(an,360.);

     if((an >= 0) && (an < 120.)) {
          r = 255.*mypow((ak + MIN(s,(120. - an)*c))/255., gamm) +.5;
          g = 255.*mypow((ak + MIN(s,an*c))/255., gamm) +.5;
          b = 0;
     } else if((an >= 120.) && (an < 240.)) {
          r = 0;
          g = 255.*mypow((ak + MIN(s ,(240. - an)*c))/255., gamm) +.5;
          b = 255.*mypow((ab + MIN(sb,(an - 120.)*cb))/255., gamm) +.5;
     } else if(an >= 240.) {
          r =  255.*mypow((ak + MIN(s,(an - 240.)*c))/255., gamm) +.5;
          g = 0;
          b = 255.*mypow((ak + MIN(s,(360. - an)*c))/255., gamm) +.5;
     }
     dc->xcol_im[i].red   = BYTE_TO_INTEN(r) ;
     dc->xcol_im[i].green = BYTE_TO_INTEN(g) ;
     dc->xcol_im[i].blue  = BYTE_TO_INTEN(b) ;
     dc->xcol_im[i].flags = DoRed|DoGreen|DoBlue;

     if( dc->visual_class == PseudoColor )        /* 22 Aug 1998 */
        dc->xcol_im[i].pixel = dc->pix_im[i];
   }
   return ;
}

#if 0 /*******************************************************************/
/* --------------------------------------------------------------------------
   Given an triple of bytes (0..255), make a color and return its pixel value
-----------------------------------------------------------------------------*/

Pixel RGB_byte_to_color( MCW_DC * dc , int r , int g , int b )
{
   XColor  any_col;

   any_col.red   = BYTE_TO_INTEN(r);
   any_col.green = BYTE_TO_INTEN(g);
   any_col.blue  = BYTE_TO_INTEN(b);
   any_col.flags = DoRed | DoGreen | DoBlue;
   XAllocColor( dc->display , dc->colormap , &any_col );
   return any_col.pixel ;
}

/*--------------------------------------------------------------------------
  Given a color named by a string, allocate it and return its pixel value
----------------------------------------------------------------------------*/

Pixel Name_to_color( MCW_DC * dc , char * name )
{
   XColor cell , exact ;
   int ok ;

   ok = XAllocNamedColor( dc->display , dc->colormap , name , &cell , &exact ) ;

   if( ok ) return cell.pixel ;
   else     return BlackPixelOfScreen( dc->screen ) ;
}
#endif /********************************************************************/

/*--------------------------------------------------------------------------
   Given a color name, allocate it, put it into the DC overlay table,
   and return its index (negative if an error occurred)
   Dec 1997: modified to use read-write color cells,
             and recycle them if the same label is passed in.
   22 Aug 1998: modified for TrueColor support
----------------------------------------------------------------------------*/

int DC_add_overlay_color( MCW_DC * dc , char * name , char * label )
{
   int ii , ok , newcol ;
   Pixel newpix ;
   XColor cell ;

   if( name == NULL || strlen(name) == 0 ) return -1 ;  /* error */
   if( label == NULL ) label = name ;

   /** see if label is already in the table **/

   for( ii=1 ; ii < dc->ovc->ncol_ov ; ii++ )
      if( strcmp(label,dc->ovc->label_ov[ii]) == 0 ) break ;

   newcol = (ii == dc->ovc->ncol_ov) ;     /** need a new color cell? **/
   if( ii == dc->ovc->ncol_ov ){           /** Yes **/
      unsigned int nplmsk = 0 ;
      unsigned long plane_masks[1] ;

      if( ii >= MAX_COLORS ) return -1 ;   /* too many overlay colors! */

      if( dc->visual_class == PseudoColor ){  /* 22 Aug 1998 */
         ok = XAllocColorCells( dc->display , dc->colormap ,
                                True , plane_masks , nplmsk , &newpix , 1 ) ;
         if( !ok ) return -1 ;                /* couldn't get a new cell */
         cell.pixel = newpix ;
      }

   } else {                                /** Reusing an old cell **/

      if( strcmp(name,dc->ovc->name_ov[ii]) == 0 ) return ii ; /* no change! */

      if( dc->visual_class == PseudoColor )  /* 22 Aug 1998 */
         cell.pixel = dc->ovc->pix_ov[ii] ;
      else if( dc->visual_class == TrueColor )
         XFreeColors( dc->display, dc->colormap, dc->ovc->pix_ov+ii, 1, 0 ) ;
   }

   ok = XParseColor( dc->display , dc->colormap , name, &cell ) ;
   if( !ok ) return -1 ;

   if( newcol ){                      /** made a new cell **/
      dc->ovc->ncol_ov++ ;
   } else {                           /** free old cell stuff **/
      myXtFree( dc->ovc->name_ov[ii] ) ;
      myXtFree( dc->ovc->label_ov[ii] ) ;
   }

   if( dc->visual_class == PseudoColor )                  /* 22 Aug 1998 */
      XStoreColor( dc->display , dc->colormap , &cell ) ;
   else if( dc->visual_class == TrueColor )
      XAllocColor( dc->display , dc->colormap , &cell ) ;

   dc->ovc->xcol_ov[ii]  = cell ;                      /* save cell info */
   dc->ovc->pix_ov[ii]   = cell.pixel ;
   dc->ovc->name_ov[ii]  = XtNewString(name) ;
   dc->ovc->label_ov[ii] = XtNewString(label) ;

   if( dc->visual_class == PseudoColor )  /* 11 Feb 1999: */
      FREE_DC_colordef(dc->cdef) ;        /* will need to be recomputed */

   return ii ;
}

int DC_find_overlay_color( MCW_DC * dc , char * label )
{
   int ii ;
   if( dc == NULL || label == NULL ) return -1 ;
   for( ii=0 ; ii < dc->ovc->ncol_ov ; ii++ )
      if( strcmp(label,dc->ovc->label_ov[ii]) == 0 ) return ii ;
   return -1 ;
}

/*-------------------------------------------------------------------------
  load the tmp? arrays (alas, GLOBAL data, a sin against God and Man)
  from an array of colors
--------------------------------------------------------------------------*/

static unsigned short tmp1[MAX_COLORS] , tmp2[MAX_COLORS] , tmp3[MAX_COLORS] ;
static int            tmpi[MAX_COLORS] ;

void load_tmp_colors( int nc , XColor * ccc )
{
   register int i ;

   for( i=0 ; i < nc ; i++ ){
      tmp1[i] = ccc[i].red ;
      tmp2[i] = ccc[i].green ;
      tmp3[i] = ccc[i].blue ;
   }
   return ;
}

/*-------------------------------------------------------------------------
  rotate active palette k steps
-------------------------------------------------------------------------*/

void DC_palette_rotate( MCW_DC * dc , int kk )
{
   register int i , j , nc , k ;
   XColor * xc ;

   nc = dc->ncol_im ;

   if( dc->use_xcol_im ){
      xc = & ( dc->xcol_im[0] ) ;  /* choose active palette */
      k  = -kk ;                   /* direction of rotation */
   } else {
      xc = & ( dc->xgry_im[0] ) ;
      k  = -kk ;
   }

   load_tmp_colors( nc , xc ) ;    /* store RGB values in tmp? */

   for( i=0 ; i < nc ; i++ ){
      j = (i+nc+k) % nc ;

      xc[i].red   = tmp1[j] ;
      xc[i].green = tmp2[j] ;
      xc[i].blue  = tmp3[j] ;
   }

   if( ! dc->use_xcol_im ){  /* rotate xint_im as well for graymap */
      for( i=0 ; i < nc ; i++ ) tmpi[i] = dc->xint_im[i] ;
      for( i=0 ; i < nc ; i++ ) dc->xint_im[i] = tmpi[(i+nc+k)%nc] ;
   }

   DC_set_image_colors( dc ) ;  /* 22 Aug 1998 */
   return ;
}

/*------------------------------------------------------------------------*/

void DC_palette_swap( MCW_DC * dc )
{
   register int i, k , nc ;
   XColor * xc ;

   nc = dc->ncol_im ;
   k  = nc - 1 ;

   if( dc->use_xcol_im ){
      xc = & ( dc->xcol_im[0] ) ;
   } else {
      xc = & ( dc->xgry_im[0] ) ;
   }

   load_tmp_colors( nc , xc ) ;

   for(i=0; i < nc ; i++) {
     xc[i].red   = tmp1[k-i];
     xc[i].green = tmp2[k-i];
     xc[i].blue  = tmp3[k-i];
   }

   if( ! dc->use_xcol_im ){  /* swap xint_im as well for graymap */
      for( i=0 ; i < nc ; i++ ) tmpi[i] = dc->xint_im[i] ;
      for( i=0 ; i < nc ; i++ ) dc->xint_im[i] = tmpi[k-i] ;
   }

   DC_set_image_colors( dc ) ;  /* 22 Aug 1998 */
   return ;
}

/*-----------------------------------------------------------------------*/

void DC_palette_bright(  MCW_DC * dc , int dd )
{
   if( dc->use_xcol_im ) DC_color_bright( dc ,    dd ) ;
   else                  DC_gray_change(  dc , -2*dd ) ;
   return;
}

/*-----------------------------------------------------------------------*/

void DC_color_bright( MCW_DC * dc , int dlev )
{
   register int i ;
   double c ;
   int      nc = dc->ncol_im ;
   XColor * xc = dc->xcol_im ;

   c = 1.0 - 0.005 * (double) dlev ;

   for( i=0 ; i < nc ; i++ ){
      xc[i].red   = CLIP_INTEN( c * xc[i].red   ) ;
      xc[i].green = CLIP_INTEN( c * xc[i].green ) ;
      xc[i].blue  = CLIP_INTEN( c * xc[i].blue  ) ;
   }
   DC_set_image_colors( dc ) ;  /* 22 Aug 1998 */
   return ;
}

/*------------------------------------------------------------------------*/

void DC_gray_change( MCW_DC * dc , int dlev )
{
   register int i, k, delta ;
   int      nc = dc->ncol_im ;
   XColor * xc = dc->xgry_im ;
   int    * in = dc->xint_im ;

   delta = dlev * abs( (in[nc-1] - in[0]) / nc ) ;

   for( i=0 ; i < nc ; i++ ){
      k = in[i] += delta ;
      xc[i].red = xc[i].green = xc[i].blue = CLIP_INTEN(k) ;
   }
   DC_set_image_colors( dc ) ;  /* 22 Aug 1998 */
   return ;
}

/*------------------------------------------------------------------------*/

void DC_palette_squeeze( MCW_DC * dc , int dd )
{
   if( dc->use_xcol_im ) DC_color_squeeze( dc ,    dd ) ;
   else                  DC_gray_contrast( dc , -2*dd ) ;
   return;
}

/*------------------------------------------------------------------------*/

void DC_color_squeeze( MCW_DC * dc , int dlev )
{
   return ;  /* not implemented */
}


/*------------------------------------------------------------------------*/

void DC_gray_contrast( MCW_DC * dc , int dlev )
{
   register int i, k, delta ;
   int      nc = dc->ncol_im ;
   XColor * xc = dc->xgry_im ;
   int    * in = dc->xint_im ;

   delta = dlev * (abs(in[nc-1] - in[0]) >> 6) / nc ;
   if( delta == 0 ) delta = dlev ;

   for( i=0 ; i < nc ; i++ ){
      k = in[i] += i * delta ;
      xc[i].red = xc[i].green = xc[i].blue = CLIP_INTEN(k) ;
   }
   DC_set_image_colors( dc ) ;  /* 22 Aug 1998 */
   return ;
}

/*-------------------------------------------------------------------*/

Boolean MCW_check_iconsize( int width , int height , MCW_DC * dc )
{
   int ii ;
   Boolean good ;
   int nsl = 0 ;
   XIconSize * xsl = NULL ;

   /** elementary checks **/

   if( width < 1 || height < 1 ) return False ;

   XGetIconSizes( dc->display , RootWindowOfScreen(dc->screen) , &xsl , &nsl ) ;

   if( xsl == NULL || nsl < 1 )  return True ;

   good = False ;

   for( ii=0 ; ii < nsl ; ii++ ){

      if( width  >= xsl[ii].min_width  && width  <= xsl[ii].max_width  &&
          height >= xsl[ii].min_height && height <= xsl[ii].max_height &&

          (width  - xsl[ii].min_width ) % xsl[ii].width_inc  == 0 &&
          (height - xsl[ii].min_height) % xsl[ii].height_inc == 0   ) { good = True ; break ; }
   }

   XFree(xsl) ;
   return good ;
}

/*---------------------------------------------------------------------------
   Given a pixel index, return a pointer to its color (not very efficiently).
   11 Feb 1999: if use_cmap != 0, use the colormap instead of
                the internal dc color arrays
-----------------------------------------------------------------------------*/

XColor * DCpix_to_XColor( MCW_DC * dc , Pixel pp , int use_cmap )
{
   XColor * ulc , * ovc ;
   int ii ;

   if( use_cmap ){              /* 11 Feb 1999 */
      static XColor xc ;
      byte rr,gg,bb ;

      DC_pixel_to_rgb( dc , pp , &rr,&gg,&bb ) ;
      xc.red   = BYTE_TO_INTEN(rr) ;
      xc.green = BYTE_TO_INTEN(gg) ;
      xc.blue  = BYTE_TO_INTEN(bb) ;
      return &xc ;
   }

   ulc = (dc->use_xcol_im) ? dc->xcol_im : dc->xgry_im ;
   ovc = dc->ovc->xcol_ov ;

   for( ii=0 ; ii < dc->ncol_im ; ii++ )
      if( pp == dc->pix_im[ii] ) return (ulc+ii) ;

   for( ii=0 ; ii < dc->ovc->ncol_ov ; ii++ )
      if( pp == dc->ovc->pix_ov[ii] ) return (ovc+ii) ;

   return ulc ;  /* not found, but must return something */
}

/*-------------------------------------------------------------------------*/

void DC_fg_color( MCW_DC * dc , int nov )
{
   XSetForeground( dc->display , dc->myGC , dc->ovc->pix_ov[nov] ) ;
   return ;
}

void DC_bg_color( MCW_DC * dc , int nov )
{
   XSetBackground( dc->display , dc->myGC , dc->ovc->pix_ov[nov] ) ;
   return ;
}

void DC_fg_colorpix( MCW_DC * dc , Pixel pix )
{
   XSetForeground( dc->display , dc->myGC , pix ) ;
   return ;
}

void DC_fg_colortext( MCW_DC * dc , char * cname )
{
   XColor any_col , rgb_col ;

   if( ! XAllocNamedColor( dc->display , dc->colormap ,
                           cname , &any_col, &rgb_col ) ){

      fprintf(stderr,"\n** XAllocNamedColor problem: %s **\n",cname ) ;
   } else {
      XSetForeground( dc->display , dc->myGC , any_col.pixel ) ;
   }
   return ;
}

void DC_linewidth( MCW_DC * dc , int lw )
{
   XGCValues gcv ;

   if( lw >= 0 ){
      gcv.line_width = lw ;
      gcv.join_style = JoinBevel ;
      XChangeGC( dc->display , dc->myGC , GCLineWidth | GCJoinStyle , &gcv ) ;
   }
   return ;
}

void DC_linestyle( MCW_DC * dc , int lw )
{
   XGCValues gcv ;
   gcv.line_style = lw ;
   XChangeGC( dc->display , dc->myGC , GCLineStyle , &gcv ) ;
   return ;
}

/*-------------------------------------------------------------------------
 May 1996: save the indices of the darkest and brightest overlays
 Dec 1997: moved into a separate routine
---------------------------------------------------------------------------*/

void OVC_mostest( MCW_DCOV * ovc )
{
   float bright_inten , dark_inten , red_inten , green_inten , blue_inten , inten ;
   int   bright_ii    , dark_ii    , red_ii    , green_ii    , blue_ii ;
   int   ii ;

   if( ovc == NULL || ovc->ncol_ov < 2 ) return ;

   bright_inten = dark_inten = XCOL_BRIGHTNESS( ovc->xcol_ov[1] ) ;
   bright_ii    = dark_ii    = 1 ;

   red_inten   = XCOL_REDNESS  ( ovc->xcol_ov[1] ) ;  /* June 1997 */
   green_inten = XCOL_GREENNESS( ovc->xcol_ov[1] ) ;
   blue_inten  = XCOL_BLUENESS ( ovc->xcol_ov[1] ) ;
   red_ii = green_ii = blue_ii = 1 ;

   for( ii=2 ; ii < ovc->ncol_ov ; ii++ ){
      inten = XCOL_BRIGHTNESS( ovc->xcol_ov[ii] ) ;
      if( inten > bright_inten ){
         bright_inten = inten ; bright_ii = ii ;
      } else if( inten < dark_inten ){
         dark_inten = inten ; dark_ii = ii ;
      }

      inten = XCOL_REDNESS( ovc->xcol_ov[ii] ) ;
      if( inten > red_inten ){
         red_inten = inten ; red_ii = ii ;
      }

      inten = XCOL_GREENNESS( ovc->xcol_ov[ii] ) ;
      if( inten > green_inten ){
         green_inten = inten ; green_ii = ii ;
      }

      inten = XCOL_BLUENESS( ovc->xcol_ov[ii] ) ;
      if( inten > blue_inten ){
         blue_inten = inten ; blue_ii = ii ;
      }
   }
   ovc->ov_brightest = bright_ii ; ovc->pixov_brightest = ovc->pix_ov[bright_ii] ;
   ovc->ov_darkest   = dark_ii   ; ovc->pixov_darkest   = ovc->pix_ov[dark_ii] ;
   ovc->ov_reddest   = red_ii    ; ovc->pixov_reddest   = ovc->pix_ov[red_ii] ;
   ovc->ov_greenest  = green_ii  ; ovc->pixov_greenest  = ovc->pix_ov[green_ii] ;
   ovc->ov_bluest    = blue_ii   ; ovc->pixov_bluest    = ovc->pix_ov[blue_ii] ;
}

/*-------------------------------------------------------------------------------
   Store the colors needed for image display,
   based on the value of dc->use_xcol_im (colors for image, or grayscale?).
   This replaces the use of XStoreColors directly in the code above.
   22 Aug 1998 -- RWCox.
---------------------------------------------------------------------------------*/

void DC_set_image_colors( MCW_DC * dc )
{
   int ii , nc ;
   XColor * xc ;

   nc = dc->ncol_im ;
   xc = (dc->use_xcol_im) ? (dc->xcol_im) : (dc->xgry_im) ;

   if( dc->visual_class == PseudoColor ){      /* actually change colormap */

      XStoreColors( dc->display , dc->colormap , xc , nc ) ;

#if 0
      if( dc->cdef != NULL ) reload_DC_colordef( dc ) ;  /* 11 Feb 1999 */
#else
      /* FREE_DC_colordef(dc->cdef) ; */
#endif

   } else if( dc->visual_class == TrueColor ){  /* change internal pixel array */

      for( ii=0 ; ii < nc ; ii++ ){
         if( dc->pix_im_ready )
            XFreeColors( dc->display, dc->colormap, dc->pix_im+ii, 1, 0 ) ;

         XAllocColor( dc->display , dc->colormap , xc+ii ) ;
         dc->pix_im[ii] = xc[ii].pixel ;
      }
      dc->pix_im_ready = 1 ;

   }

   return ;
}

/*--------------------------------------------------------------
   Yoke the widget to the DC -- 14 Sep 1998
----------------------------------------------------------------*/

void DC_yokify( Widget w , MCW_DC * dc )
{
   if( w == NULL || dc == NULL || !XtIsWidget(w) ) return ;

   XtVaSetValues( w ,
                     XmNvisual   , dc->visual ,
                     XmNcolormap , dc->colormap ,
                     XmNdepth    , dc->depth ,
                     XmNscreen   , dc->screen ,
                     XmNbackground  , 0 ,
                     XmNborderColor , 0 ,
                  NULL ) ;
   return ;
}

/*---------------------------------------------------------------------
   Load the colordef structure in the DC -- 11 Feb 1999
-----------------------------------------------------------------------*/

void reload_DC_colordef( MCW_DC * dc )
{
   XVisualInfo * vin ;
   DC_colordef * cd ;   /* will be the output */

   /*--- sanity check ---*/

   if( dc == NULL || dc->visual_info == NULL ){
      fprintf(stderr,"reload_DC_colordef: entry values are NULL\n") ;
      return ;
   }

   vin = dc->visual_info ;

   /*--- PseudoColor case ---*/

   if( vin->class == PseudoColor ){
      int iz , count , ii ;
      XColor * xcol ;

      /* create output */

      cd = (DC_colordef *) malloc( sizeof(DC_colordef) ) ;
      cd->class = PseudoColor ;
      cd->depth = vin->depth ;

      /* get all the colors in the colormap */

      count = vin->colormap_size ;
      xcol  = (XColor *) malloc( sizeof(XColor) * count ) ;
      for( ii=0 ; ii < count ; ii++ ) xcol[ii].pixel = ii ;

      XQueryColors( dc->display , dc->colormap , xcol , count ) ;

      /* store them in the output, truncated to 8 bit resolution */

      cd->ncolors = count ;
      cd->rr      = (byte *) malloc( count ) ;
      cd->gg      = (byte *) malloc( count ) ;
      cd->bb      = (byte *) malloc( count ) ;

      for( ii=0 ; ii < count ; ii++ ){
         cd->rr[ii] = xcol[ii].red   >> 8 ;
         cd->gg[ii] = xcol[ii].green >> 8 ;
         cd->bb[ii] = xcol[ii].blue  >> 8 ;
      }

      /* find a pure white color, if any */

      for( iz=0 ; iz < count ; iz++ )
         if( cd->rr[iz] == 255 && cd->gg[iz] == 255 && cd->bb[iz] == 255 ) break ;

      cd->nwhite = (iz < count) ? iz : -1 ;

      /* find first all zero color; discard others at end of colormap */

      for( iz=0 ; iz < count ; iz++ )
         if( cd->rr[iz] == 0 && cd->gg[iz] == 0 && cd->bb[iz] == 0 ) break ;

      cd->nblack = (iz < count) ? iz : -1 ;

      if( iz < count-1 ){  /* if found one before the end */

         for( ii=count-1 ; ii > iz ; ii-- )  /* scan backwards */
            if( cd->rr[ii] != 0 || cd->gg[ii] != 0 || cd->bb[ii] != 0 ) break ;

         count = ii+1 ;  /* number of colors left */

         if( count == 1 ){ /* colormap is all black?! */
            free(xcol) ; FREE_DC_colordef(cd) ;
            fprintf(stderr,"reload_DC_colordef: colormap is all black?\n") ;
            return ;
         }

         cd->ncolors = count ;
      }

      FREE_DC_colordef(dc->cdef) ;  /* if already present, kill it */

      free(xcol) ; dc->cdef = cd ; return ;
   }

   /*--- TrueColor case ---*/

   if( vin->class == TrueColor ){
      unsigned long r , g , b ;
      byte          rr, gg, bb ;

      /* create output */

      cd = (DC_colordef *) malloc( sizeof(DC_colordef) ) ;
      cd->class = TrueColor ;
      cd->depth = vin->depth ;

      cd->rrmask  = vin->red_mask ;            /* bit masks for color  */
      cd->ggmask  = vin->green_mask ;          /* storage inside pixel */
      cd->bbmask  = vin->blue_mask ;
      cd->rrshift = 7 - highbit(cd->rrmask) ;  /* shift puts high bit of  */
      cd->ggshift = 7 - highbit(cd->ggmask) ;  /* a color byte into place */
      cd->bbshift = 7 - highbit(cd->bbmask) ;  /* +shift == >> ; - == <<  */

      /* compute the all white pixel as a common case */

      rr = gg = bb = 255 ;

      r = (cd->rrshift<0) ? (rr<<(-cd->rrshift))
                          : (rr>>cd->rrshift)   ; r = r & cd->rrmask ;

      g = (cd->ggshift<0) ? (gg<<(-cd->ggshift))
                          : (gg>>cd->ggshift)   ; g = g & cd->ggmask ;

      b = (cd->bbshift<0) ? (bb<<(-cd->bbshift))
                          : (bb>>cd->bbshift)   ; b = b & cd->bbmask ;

      cd->whpix = r | g | b ;

      cd->rr = cd->gg = cd->bb = NULL ;        /* not used */

      FREE_DC_colordef(dc->cdef) ;  /* if already present, kill it */

      dc->cdef = cd ; return ;
   }

   /*--- Illegal Visual class! [do nothing]---*/

   fprintf(stderr,"reload_DC_colordef: illegal Visual class %s\n",
                  x11_vcl[vin->class] ) ;
   return ;
}

/*---------------------------------------------------------------------
   Compute the Pixel that is closest to the given (r,g,b) color
-----------------------------------------------------------------------*/

Pixel DC_rgb_to_pixel( MCW_DC * dc, byte rr, byte gg, byte bb )
{
   DC_colordef * cd = dc->cdef ;

   if( cd == NULL ){ reload_DC_colordef(dc) ; cd = dc->cdef ; }

   switch( cd->class ){

      /*--- TrueColor case: make color by appropriate bit twiddling ---*/

      case TrueColor:{
         static unsigned long pold=0 ;
         static byte rold=0 , gold=0 , bold=0 ;
         unsigned long r , g , b ;

         if( rr == 0   && gg == 0   && bb == 0   ) return 0 ;          /* common */
         if( rr == 255 && gg == 255 && bb == 255 ) return cd->whpix ;  /* cases  */

         if( rr == rold && gg == gold && bb == bold ) /* Remembrance of Things Past? */
            return (Pixel) pold ;

         rold = rr ; gold = gg ; bold = bb ;          /* OK, remember for next time */

         r = (cd->rrshift<0) ? (rr<<(-cd->rrshift))
                             : (rr>>cd->rrshift)   ; r = r & cd->rrmask ;

         g = (cd->ggshift<0) ? (gg<<(-cd->ggshift))
                             : (gg>>cd->ggshift)   ; g = g & cd->ggmask ;

         b = (cd->bbshift<0) ? (bb<<(-cd->bbshift))
                             : (bb>>cd->bbshift)   ; b = b & cd->bbmask ;

         pold = r | g | b ;  /* assemble color from components */
         return (Pixel) pold ;
      }

      /*--- PseudoColor case: find closest match in colormap.
            Red, green, and blue are weighted according
            to their importance to the human visual system. ---*/

      case PseudoColor:{

#define RW 2  /* the weights alluded to above */
#define GW 4
#define BW 1
#define RGBSUM 7  /* sum of the weights */

         static int iold=0 , rold=0,gold=0,bold=0 ;
         int ii , rdif,gdif,bdif,dif , ibest,dbest ;

         if( cd->nblack >= 0 && rr == 0 && gg == 0 && bb == 0 )       /* deal with  */
            return (Pixel) cd->nblack ;                               /* 2 special  */
                                                                      /* and common */
         if( cd->nwhite >= 0 && rr == 255 && gg == 255 && bb == 255 ) /* cases      */
            return (Pixel) cd->nwhite ;

         rdif = rold - rr ;
         gdif = gold - gg ;
         bdif = bold - bb ; dif = RW*abs(rdif)+GW*abs(gdif)+BW*abs(bdif) ;
         if( dif <= RGBSUM ) return (Pixel) iold ;   /* Remembrance of Things Past? */

         rold = rr ; gold = gg ; bold = bb ;         /* No? Remember for next time. */

         rdif = cd->rr[0] - rr ;
         gdif = cd->gg[0] - gg ;
         bdif = cd->bb[0] - bb ; dif = RW*abs(rdif)+GW*abs(gdif)+BW*abs(bdif) ;
         if( dif <= RGBSUM ){ iold = 0 ; return 0 ; }

         ibest = 0 ; dbest = dif ;
         for( ii=1 ; ii < cd->ncolors ; ii++ ){
            rdif = cd->rr[ii] - rr ;
            gdif = cd->gg[ii] - gg ;
            bdif = cd->bb[ii] - bb ; dif = RW*abs(rdif)+GW*abs(gdif)+BW*abs(bdif) ;
            if( dif <= RGBSUM ){ iold  = ii ; return (Pixel) ii ; }
            if( dif < dbest   ){ ibest = ii ; dbest = dif ; }
         }
         iold = ibest ; return (Pixel) ibest ;
      }
   }

   /*--- Illegal case! ---*/

   return 0 ;  /* always valid (but useless) */
}

/*---------------------------------------------------------------------
   Compute the (r,g,b) color corresponding to a given Pixel
-----------------------------------------------------------------------*/

void DC_pixel_to_rgb( MCW_DC * dc , Pixel ppp ,
                      byte * rr , byte * gg , byte * bb )
{
   DC_colordef * cd = dc->cdef ;

   if( cd == NULL ){ reload_DC_colordef(dc) ; cd = dc->cdef ; }

   switch( cd->class ){

      /*--- TrueColor case: unmake color by appropriate bit twiddling ---*/

      case TrueColor:{
         unsigned long r , g , b ;

         if( ppp == 0         ){ *rr = *bb = *gg = 0   ; return ; }  /* common cases */
         if( ppp == cd->whpix ){ *rr = *bb = *gg = 255 ; return ; }

         r   = ppp & cd->rrmask ;
         *rr = (cd->rrshift<0) ? (r>>(-cd->rrshift)) : (r<<cd->rrshift) ;

         g   = ppp & cd->ggmask ;
         *gg = (cd->ggshift<0) ? (g>>(-cd->ggshift)) : (g<<cd->ggshift) ;

         b   = ppp & cd->bbmask ;
         *bb = (cd->bbshift<0) ? (b>>(-cd->bbshift)) : (b<<cd->bbshift) ;

         return ;
      }

      /*--- PseudoColor case: extract from colormap ---*/

      case PseudoColor:{
         int ii = (int) ppp ;
         *rr = cd->rr[ii] ; *gg = cd->gg[ii] ; *bb = cd->bb[ii] ; return ;
      }
   }

   return ;
}
