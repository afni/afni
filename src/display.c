#include "display.h"
#include "mrilib.h"

/*------------------------------------------------------------------------
  Create and initialize a new MCW_DC structure.

    wid  = widget used to get info about Display, etc.
    ncol = number of colors to use for images
    novr = number of overlay colors
    covr = array of strings of overlay color names  [used for XAlloc...]
    lovr = array of strings of overlay color labels [used for display]
    gam  = gamma value to use
------------------------------------------------------------------------*/

static MCW_DCOV * only_ovc = NULL ;  /* Dec 1997 */

MCW_DC * MCW_new_DC( Widget wid , int ncol ,
                     int novr , char * covr[] , char * lovr[] , double gam )
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
   dc->colormap   = DefaultColormapOfScreen( dc->screen ) ;
   dc->origGC     = DefaultGCOfScreen(       dc->screen ) ;
   dc->planes     = PlanesOfScreen(          dc->screen ) ;
   dc->depth      = DefaultDepthOfScreen(    dc->screen ) ;

   dc->parent_widget = wid ;  /* 06 Oct 1996 */

/** this stuff is experimentation with X **/

#if 0
   { XIconSize * xsl = NULL ;
     int         nsl = 0 , ii ;
     XGetIconSizes( dc->display , RootWindowOfScreen(dc->screen) , &xsl , &nsl ) ;
     if( xsl != NULL && nsl > 0 ){
        for( ii=0 ; ii < nsl ; ii++ )
           printf("IconSize %d: minx=%d miny=%d maxx=%d maxy=%d dx=%d dy=%d\n",
             ii , xsl[ii].min_width , xsl[ii].min_height ,
                  xsl[ii].max_width , xsl[ii].max_height ,
                  xsl[ii].width_inc , xsl[ii].height_inc  ) ;
     } else {
        printf("XGetIconSizes fails\n") ;
     }
     XFree(xsl) ;
   }
#endif

#if 0
{  XVisualInfo vin ;
   int stat ;
   stat = XMatchVisualInfo( dc->display,dc->screen_num, 8,PseudoColor,&vin ) ;
   if( stat != 0 ) printf("found  8 bit visual\n") ;
   stat = XMatchVisualInfo( dc->display,dc->screen_num,12,PseudoColor,&vin ) ;
   if( stat != 0 ) printf("found 12 bit visual\n") ;
}
#endif

#if 0
{  long reqmax ;
   reqmax = XMaxRequestSize(dc->display) ;
   printf("max X11 request size = %d\n",reqmax) ;
}
#endif

/** end of experimentation **/

   if( dc->planes < 4 || dc->planes > 16 ){
      fprintf(stderr,"\a\n*** ILLEGAL number of bitplanes: %d\n",dc->planes) ;
      exit(-1) ;
   }

   dc->width   = WidthOfScreen(  dc->screen ) ;
   dc->height  = HeightOfScreen( dc->screen ) ;

   dc->ncol_im = ncol ;
   dc->gamma   = dc->gamma_init = gam  ;

   ok = XAllocColorCells( dc->display , dc->colormap ,
                          True , plane_masks , nplmsk ,
                          dc->pix_im , dc->ncol_im ) ;

   if( ! ok ){
      fprintf(stderr,
              "\a\n*** XAllocColorCells fails for %d colors\n",dc->ncol_im) ;
      fprintf(stderr,
              "\n*** try the -ncolor option to reduce # of colors\n");
      exit(-1) ;
   }

   DC_init_im_gry( dc ) ;
   DC_init_im_col( dc ) ;
   XStoreColors( dc->display , dc->colormap , dc->xgry_im , dc->ncol_im ) ;
   dc->use_xcol_im = False ;

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

   return dc ;
}

/*------------------------------------------------------------------------*/

void DC_palette_setgray( MCW_DC * dc )
{
   XStoreColors( dc->display , dc->colormap , dc->xgry_im , dc->ncol_im ) ;
   dc->use_xcol_im = False ;
   return ;
}

void DC_palette_setcolor( MCW_DC * dc )
{
   XStoreColors( dc->display , dc->colormap , dc->xcol_im , dc->ncol_im ) ;
   dc->use_xcol_im = True ;
   return ;
}

/*------------------------------------------------------------------------*/

void DC_palette_restore( MCW_DC * dc , double new_gamma )
{
   dc->gamma = (new_gamma > 0 ) ? new_gamma : dc->gamma_init ;
   DC_init_im_col( dc ) ;
   DC_init_im_gry( dc ) ;

   if( dc->use_xcol_im ){
      XStoreColors( dc->display , dc->colormap , dc->xcol_im , dc->ncol_im ) ;
   } else {
      XStoreColors( dc->display , dc->colormap , dc->xgry_im , dc->ncol_im ) ;
   }
}

/*------------------------------------------------------------------------*/

void DC_init_im_gry( MCW_DC * dc )
{
   register int i, k, m;
   int    nc ;
   double a , gamm ;

   nc   = dc->ncol_im ;
   gamm = dc->gamma ;
   a    = 200. / nc ;

   for (i=0; i < nc ; i++) {
         k = 255.*pow((a*i+55.)/255., gamm) + .5;
         m = BYTE_TO_INTEN(k);

         dc->xint_im[i]       = m ;
         dc->xgry_im[i].pixel = dc->pix_im[i];
         dc->xgry_im[i].red   = m;
         dc->xgry_im[i].green = m;
         dc->xgry_im[i].blue  = m;
         dc->xgry_im[i].flags = DoRed|DoGreen|DoBlue;
   }

   return ;
}

/*-------------------------------------------------------------------------*/

void DC_init_im_col( MCW_DC * dc )
{
   double da, an, c, s, sb, cb, ak, ab , a1,a2 , gamm ;
   register int i, r, g, b , nc ;

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
          r = 255.*pow((ak + MIN(s,(120. - an)*c))/255., gamm) +.5;
          g = 255.*pow((ak + MIN(s,an*c))/255., gamm) +.5;
          b = 0;
     } else if((an >= 120.) && (an < 240.)) {
          r = 0;
          g = 255.*pow((ak + MIN(s ,(240. - an)*c))/255., gamm) +.5;
          b = 255.*pow((ab + MIN(sb,(an - 120.)*cb))/255., gamm) +.5;
     } else if(an >= 240.) {
          r =  255.*pow((ak + MIN(s,(an - 240.)*c))/255., gamm) +.5;
          g = 0;
          b = 255.*pow((ak + MIN(s,(360. - an)*c))/255., gamm) +.5;
     }
     dc->xcol_im[i].pixel = dc->pix_im[i];
     dc->xcol_im[i].red   = BYTE_TO_INTEN(r) ;
     dc->xcol_im[i].green = BYTE_TO_INTEN(g) ;
     dc->xcol_im[i].blue  = BYTE_TO_INTEN(b) ;
     dc->xcol_im[i].flags = DoRed|DoGreen|DoBlue;
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

      ok = XAllocColorCells( dc->display , dc->colormap ,
                             True , plane_masks , nplmsk , &newpix , 1 ) ;
      if( !ok ) return -1 ;                /* couldn't get a new cell */
      cell.pixel = newpix ;
   } else {
      if( strcmp(name,dc->ovc->name_ov[ii]) == 0 ) return ii ; /* no change! */
      cell.pixel = dc->ovc->pix_ov[ii] ;
   }

   ok = XParseColor( dc->display , dc->colormap , name, &cell ) ;
   if( !ok ) return -1 ;

   if( newcol ){                      /** made a new cell **/
      dc->ovc->ncol_ov++ ;
   } else {                           /** free old cell stuff **/
      myXtFree( dc->ovc->name_ov[ii] ) ;
      myXtFree( dc->ovc->label_ov[ii] ) ;
   }

   dc->ovc->xcol_ov[ii]  = cell ;          /** save cell info **/
   dc->ovc->pix_ov[ii]   = cell.pixel ;
   dc->ovc->name_ov[ii]  = XtNewString(name) ;
   dc->ovc->label_ov[ii] = XtNewString(label) ;

   XStoreColor( dc->display , dc->colormap , &cell ) ; /** make it work **/
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
      k  = -kk ;                    /* direction of rotation */
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

   XStoreColors( dc->display , dc->colormap , xc , nc ) ;

   if( ! dc->use_xcol_im ){  /* rotate xint_im as well for graymap */
      for( i=0 ; i < nc ; i++ ) tmpi[i] = dc->xint_im[i] ;
      for( i=0 ; i < nc ; i++ ) dc->xint_im[i] = tmpi[(i+nc+k)%nc] ;
   }

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
   XStoreColors( dc->display , dc->colormap , xc , nc ) ;

   if( ! dc->use_xcol_im ){  /* swap xint_im as well for graymap */
      for( i=0 ; i < nc ; i++ ) tmpi[i] = dc->xint_im[i] ;
      for( i=0 ; i < nc ; i++ ) dc->xint_im[i] = tmpi[k-i] ;
   }

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
   XStoreColors( dc->display , dc->colormap , xc , nc ) ;
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
   XStoreColors( dc->display , dc->colormap , xc , nc ) ;
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
   return ;
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
   XStoreColors( dc->display , dc->colormap , xc , nc ) ;
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
-----------------------------------------------------------------------------*/

XColor * DCpix_to_XColor( MCW_DC * dc , int pp )
{
   XColor * ulc , * ovc ;
   int ii ;

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
