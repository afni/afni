#include "mrilib.h"
#include "coxplot.h"

#undef DEBUG

/*--------------------------------------------------------------------------
  Routines to render a memplot into an MRI_rgb image.
  The assumption is that the plot is over the "object" region [0,1]x[0,1],
  which is rendered into the actual image [0..nx-1]x[0..ny-1].
  The actual rendering is done by functions in mri_drawing.c,
  which were lifted from the PPM library, with some light edits.
----------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------*/
/*! Set a sub-box within a window into which the next RGB plot should
   be scaled.  (0,0,0,0) args means use the whole window.  After
   each drawing (memplot_to_RGB_sef), will be reset to the whole window
   anyway.
----------------------------------------------------------------------------*/

static int box_xbot=0 , box_xtop=0 ,
           box_ybot=0 , box_ytop=0  ;

static int do_thick=0 ;
void memplot_to_mri_set_dothick( int dt ){ do_thick = dt ; }
static int do_freee=0 ;
void memplot_to_mri_set_dofreee( int df ){ do_freee = df ; }

void set_memplot_RGB_box( int xbot, int ybot, int xtop, int ytop )
{
   if( xbot < xtop && ybot < ytop ){
     box_xbot = xbot ; box_ybot = ybot ;
     box_xtop = xtop ; box_ytop = ytop ;
   } else {
     box_xbot = box_ybot = box_xtop = box_ytop = 0 ;
   }
}

/*--------------------------------------------------------------------------*/
/*! Actually do the rendering of a memplot into an existing RGB image.
  - That is, we are drawing on top of whatever is there already.
  - Plotting will start with line #start and go to #end-1.
  - If end <= start, will do from #start to the last one in the plot.
  - To do all lines, set start=end=0.
  - "freee" controls whether the aspect ratio will be free to vary (!= 0),
    or will be fixed (==0).
  - 18 Sep 2001: adapted from X11 routines in coxplot/plot_x11.c
  - 23 Mar 2002: actually tested for the first time
----------------------------------------------------------------------------*/

void memplot_to_RGB_sef( MRI_IMAGE *im , MEM_plotdata *mp ,
                         int start , int end , int freee    )
{
   byte rrr=0,ggg=0,bbb=0 ;
   int ii , nline , same ;
   float old_thick , old_color , new_color , new_thick , sthick=0.0f ;
   float scal,xscal,yscal , xoff,yoff ;
   int x1,y1 , x2,y2 ;
   int x1_old=-666,y1_old=-666 , x2_old=-666,y2_old=-666 ; float sthick_old=-666.f ;
   int skip ;

ENTRY("memplot_to_RGB_sef") ;

   /*--- check for madness ---*/

   if( im == NULL || im->kind != MRI_rgb || mp == NULL ) EXRETURN ;

   if( start < 0 ) start = 0 ;

   nline = MEMPLOT_NLINE(mp) ;
   if( nline < 1 || start >= nline ) EXRETURN ;

   if( end <= start || end > nline ) end = nline ;

   /*--- compute scaling from memplot objective
         coordinates to RGB window coordinates  ---*/

   if( box_xbot >= box_xtop || box_ybot >= box_ytop ){

      xscal = im->nx / mp->aspect ; /* aspect = x-axis objective size */
      yscal = im->ny / 1.0f ;       /* 1.0    = y-axis objective size */
      xoff  = yoff = 0.499f ;

   } else {  /* scale to a given sub-box in the window */

      xscal = box_xtop - box_xbot ;
      yscal = box_ytop - box_ybot ;
      xoff  = box_xbot + 0.499f   ;
      yoff  = box_ybot + 0.499f   ;
   }

   if( !freee && !do_freee ){              /* no aspect freedom ==> */
      if( yscal < xscal ) xscal = yscal ;  /* use smaller scaling   */
      else                yscal = xscal ;
   }
   scal = sqrt(fabs(xscal*yscal)) ;

   old_color = -1.0f ;            /* these don't occur naturally */
   old_thick = -THCODE_INVALID ;

   /*--- loop over lines, scale and plot ---*/

   mri_draw_opacity( 1.0f ) ;

#ifdef DEBUG
INFO_message("memplot_to_RGB_sef from %d to %d",start,end) ;
#endif

   for( ii=start ; ii < end ; ii++ ){

      skip = 0 ;

      /* check if need to change color or thickness of line */

      new_color = MEMPLOT_COL(mp,ii) ;
      if( new_color != old_color ){
         float rr=COL_TO_RRR(new_color) ,
               gg=COL_TO_GGG(new_color) , bb=COL_TO_BBB(new_color) ;

         rrr = ZO_TO_TFS(rr) ; ggg = ZO_TO_TFS(gg) ; bbb = ZO_TO_TFS(bb) ;
         old_color = new_color ;
#ifdef DEBUG
ININFO_message("Changing color to %f->%d %f->%d %f->%d\n",rr,(int)rrr,gg,(int)ggg,bb,(int)bbb) ;
#endif
      }

      new_thick = MEMPLOT_TH(mp,ii) ;
      if( new_thick < 0.0 ){               /* special negative thickness codes */
         int thc = (int)(-new_thick) ;     /* mean special drawing actions */
         switch( thc ){
            case THCODE_FRECT:
            case THCODE_RECT:{        /* rectangle */
               int xb,yb , xt,yt ;
               int w,h ;
               x1 = rint( xoff + xscal * MEMPLOT_X1(mp,ii)         ) ;
               x2 = rint( xoff + xscal * MEMPLOT_X2(mp,ii)         ) ;
               y1 = rint( yoff + yscal * (1.0 - MEMPLOT_Y1(mp,ii)) ) ;
               y2 = rint( yoff + yscal * (1.0 - MEMPLOT_Y2(mp,ii)) ) ;
               if( x1 < x2 ){ xb=x1; xt=x2; } else { xb=x2; xt=x1; }
               if( y1 < y2 ){ yb=y1; yt=y2; } else { yb=y2; yt=y1; }
               w = xt-xb+1 ; h = yt-yb+1 ;
#ifdef DEBUG
ININFO_message("drawfilledrectangle") ;
#endif
               mri_drawfilledrectangle( im , xb,yb , w,h , rrr,ggg,bbb ) ;
               skip = 1 ;
            }
            break ;

            case THCODE_BALL:
            case THCODE_CIRC:{
               int xcor,ycor , xcen,ycen , rad ; float xrad,yrad ;
               unsigned int ww, hh ;
               xcen = rint(xoff + xscal * MEMPLOT_X1(mp,ii)         );
               ycen = rint(yoff + yscal * (1.0 - MEMPLOT_Y1(mp,ii)) );
               xrad = xscal * MEMPLOT_X2(mp,ii) ;
               yrad = yscal * MEMPLOT_X2(mp,ii) ; rad = rintf(sqrtf(xrad*yrad)) ;
#ifdef DEBUG
ININFO_message("drawcircle") ;
#endif
               mri_drawcircle( im , xcen,ycen , rad, rrr,ggg,bbb , (thc==THCODE_BALL) ) ;
               skip = 1 ;
            }
            break ;

            case THCODE_OPAC:{        /* opacity [22 Jul 2004] */
#ifdef DEBUG
ININFO_message("set opacity %f",MEMPLOT_X1(mp,ii)) ;
#endif
               mri_draw_opacity( MEMPLOT_X1(mp,ii) ) ;
               skip = 1 ;
            }
            break ;
         }

      } else if( new_thick != old_thick ){ /* normal case: change line thickness */

         old_thick = new_thick ;  /* thickness not used at this time */
         sthick = new_thick * scal ; /* sthick = MIN(sthick,9.0f) ; */
#ifdef DEBUG
ININFO_message("set thick %f",sthick) ;
#endif

      }

      /* scale line endpoint coords to ints (also see zzphph.f) */

      if( !skip ){
        float a1 = MEMPLOT_X1(mp,ii) ;
        float a2 = MEMPLOT_X2(mp,ii) ;
        float b1 = (1.0f - MEMPLOT_Y1(mp,ii)) ;
        float b2 = (1.0f - MEMPLOT_Y2(mp,ii)) ;

        /* scale from objective coords to pixels */
        x1 = (int)( xoff + xscal * a1 ) ; x2 = (int)( xoff + xscal * a2 ) ;
        y1 = (int)( yoff + yscal * b1 ) ; y2 = (int)( yoff + yscal * b2 ) ;

        /* draw it */
#ifdef DEBUG
ININFO_message("drawline x1=%d y1=%d  x2=%d y2=%d rrr=%d ggg=%d bbb=%d",x1,y1,x2,y2,(int)rrr,(int)ggg,(int)bbb) ;
#endif
        mri_drawline( im , x1,y1 , x2,y2 , rrr,ggg,bbb ) ;

        /* the following code "cheats" to draw thick lines,
           by drawing multiple 1-pixel lines around the central line */

        if( do_thick && sthick >= 1.0f && (x1 != x2 || y1 != y2) ){  /* 06 Dec 2007 */
          float da=a2-a1 , db=b2-b1 , dl=new_thick/sqrtf(da*da+db*db) ;
          float c1,c2 , d1,d2 ;
          int jj , ss=(int)(4.44f*sthick+0.444f) ; /* ss = num parallel lines */

          /* spacing for the parallel lines (in objective space) */

          dl /= (2*ss) ; da *= dl ; db *= dl ; ss = MAX(ss,2) ;
#if 1
          /* this code draws filled circles at the endpoints, to
             minimize possible gaps where thick lines meet at a sharp corner */

          if( sthick >= 2.0f && sthick == sthick_old ){  /* 01 May 2012 */
            int rad = (int)(0.505f*sthick+0.444f) ;
            if( x1 == x2_old && y1 == y2_old ){
              mri_drawcircle( im , x1,y1 , rad, rrr,ggg,bbb , 1 ) ;
            }
            else if( x2 == x1_old && y2 == y1_old ){
              mri_drawcircle( im , x2,y2 , rad, rrr,ggg,bbb , 1 ) ;
            }
          }
          x1_old = x1; x2_old = x2; y1_old = y1; y2_old = y2; sthick_old = sthick;
#endif
          for( jj=-ss ; jj <= ss ; jj++ ){  /* multiple parallel line segments: */
            if( jj == 0 ) continue ;        /* cheap, but it works, so there!! */
            c1 = a1 + jj*db ; c2 = a2 + jj*db ;  /* offsets in x direction */
            d1 = b1 - jj*da ; d2 = b2 - jj*da ;  /* offsets in y direction */
            /* scale from objective coords to pixels */
            x1 = (int)( xoff + xscal * c1 ) ; x2 = (int)( xoff + xscal * c2 ) ;
            y1 = (int)( yoff + yscal * d1 ) ; y2 = (int)( yoff + yscal * d2 ) ;
            mri_drawline( im , x1,y1 , x2,y2 , rrr,ggg,bbb ) ;
          }
        }
      }
   }

#ifdef DEBUG
ININFO_message("----- EXIT -----") ;
#endif
   set_memplot_RGB_box(0,0,0,0) ; /* clear box */
   EXRETURN ;
}

/*-----------------------------------------------------------------------*/
# undef  BOr
# undef  BOg
# undef  BOb
# define BOr(i,j) bout[3*((i)+(j)*nxout)+0]
# define BOg(i,j) bout[3*((i)+(j)*nxout)+1]
# define BOb(i,j) bout[3*((i)+(j)*nxout)+2]
# undef  BIr
# undef  BIg
# undef  BIb
# define BIr(i,j) ((unsigned int)bin[3*((i)+(j)*nxin)+0])
# define BIg(i,j) ((unsigned int)bin[3*((i)+(j)*nxin)+1])
# define BIb(i,j) ((unsigned int)bin[3*((i)+(j)*nxin)+2])

/* this func is used to scale down RGB image size
   by factor of 2, which in turn is used to anti-alias the line drawing;
   the algorithm used is simple averaging over each output pixel's 2x2 region */

MRI_IMAGE * mri_downsize_by2( MRI_IMAGE *imin )
{
   MRI_IMAGE *imout ; int nxin,nyin , nxout,nyout , ii,jj,i2,j2 ;
   byte *bin , *bout ; unsigned int val ;

   if( imin == NULL || imin->kind != MRI_rgb ) return NULL ;

   nxin = imin->nx ; nyin  = imin->ny ;
   nxout = nxin /2 ; nyout = nyin / 2 ;

   imout = mri_new( nxout , nyout , MRI_rgb ) ;
   bout  = MRI_RGB_PTR(imout) ;
   bin   = MRI_RGB_PTR(imin) ;

   for( jj=0 ; jj < nyout ; jj++ ){  /* loop over output pixels */
     j2 = 2*jj ;                     /* input pixel index = double output index */
     for( ii=0 ; ii < nxout ; ii++ ){
       i2 = 2*ii ;
       val = BIr(i2,j2)+BIr(i2+1,j2)+BIr(i2,j2+1)+BIr(i2+1,j2+1)+1; BOr(ii,jj) = (byte)(val >> 2);
       val = BIg(i2,j2)+BIg(i2+1,j2)+BIg(i2,j2+1)+BIg(i2+1,j2+1)+1; BOg(ii,jj) = (byte)(val >> 2);
       val = BIb(i2,j2)+BIb(i2+1,j2)+BIb(i2,j2+1)+BIb(i2+1,j2+1)+1; BOb(ii,jj) = (byte)(val >> 2);
     }
   }

   return imout ;
}

# undef  BOr
# undef  BOg
# undef  BOb
# undef  BIr
# undef  BIg
# undef  BIb

/*-----------------------------------------------------------------------*/

#undef  IMSIZ_DEF
#define IMSIZ_DEF 1024
#undef  IMSIZ_MAX
#define IMSIZ_MAX 8192

static MRI_IMAGE * memplot_to_mri( MEM_plotdata *mp )  /* 05 Dec 2007 */
{
   MRI_IMAGE *im ; int nx , ny , imsiz ;
   byte *imp ;
   int did_dup=0 ;

   if( mp == NULL || MEMPLOT_NLINE(mp) < 1 ) return NULL ;

   /* set image dimensions */

   imsiz = (int)AFNI_numenv("AFNI_1DPLOT_IMSIZE") ;  /* might be 0 */
        if( imsiz <       128 ) imsiz = IMSIZ_DEF ;
   else if( imsiz > IMSIZ_MAX ) imsiz = IMSIZ_MAX ;

   if( mp->aspect > 1.0f ){
     nx = imsiz ; ny = nx / mp->aspect ;
   } else {
     nx = imsiz * mp->aspect ; ny = imsiz ;
   }

   /* for smaller output images, make image twice the size,
      render lines into it, then scale down == anti-aliasing == looks better */

   if( imsiz <= 2048 ){ nx *=2 ; ny *=2 ; did_dup = 1 ; }

   im = mri_new( nx , ny , MRI_rgb ) ;            /* full of 0s = blacked out */
   imp = MRI_RGB_PTR(im) ; memset( imp , 255 , 3*nx*ny ) ; /* white-ize image */
   set_memplot_RGB_box(0,0,0,0) ;
   do_thick = 1 ;   /* allow thick lines */
   memplot_to_RGB_sef( im , mp , 0 , 0 , 0 ) ;         /* the actual drawing! */
   do_thick = 0 ;

   if( did_dup ){                          /* scale image down to output size */
     MRI_IMAGE *qim = mri_downsize_by2(im) ; mri_free(im) ; im = qim ;
   }

   return im ;
}

/*-----------------------------------------------------------------------*/
/* Functions to write memplot to image files */

void memplot_to_jpg( char *fname , MEM_plotdata *mp )  /* 05 Dec 2007 */
{
   MRI_IMAGE *im ;

   if( fname == NULL || *fname == '\0' ) return ;

   im = memplot_to_mri( mp ) ; if( im == NULL ) return ;
   mri_write_jpg(fname,im) ; mri_free(im) ;
   return ;
}

/*-----------------------------------------------------------------------*/

void memplot_to_png( char *fname , MEM_plotdata *mp )  /* 05 Dec 2007 */
{
   MRI_IMAGE *im ;

   if( fname == NULL || *fname == '\0' ) return ;

   im = memplot_to_mri( mp ) ; if( im == NULL ) return ;
   mri_write_png(fname,im) ; mri_free(im) ;
   return ;
}

/*-----------------------------------------------------------------------*/

void memplot_to_pnm( char *fname , MEM_plotdata *mp )  /* 06 Jan 2015 */
{
   MRI_IMAGE *im ;

   if( fname == NULL || *fname == '\0' ) return ;

   im = memplot_to_mri( mp ) ; if( im == NULL ) return ;
   mri_write_pnm(fname,im) ; mri_free(im) ;
   return ;
}

/*-----------------------------------------------------------------------*/
/* Find the min/max coordinates used in a plot structure
   (abstract coords in range 0..1, not yet scaled to actual plotting).
   Error return has out.a >= out.b and/or out.c >= out.d -- 19 Aug 2021
*//*---------------------------------------------------------------------*/

float_quad memplot_bbox( MEM_plotdata *mp )
{
   float_quad out = {0.0f,0.0f,0.0f,0.f} ;
   float xbot,xtop , ybot,ytop ;
   float x1,y1,x2,y2 , new_thick ;
   int ii , nline ;

   if( mp == NULL ) return out ;

   nline = MEMPLOT_NLINE(mp) ; if( nline < 1 ) return out ;

/* swap to make them ordered */
#define SORD(a,b)  if( a > b ){ float t=a; a=b; b=t; } else {}

   xbot = ybot =  666.0f ;
   ytop = xtop = -666.0f ;

   for( ii=0 ; ii < nline ; ii++ ){

      new_thick = MEMPLOT_TH(mp,ii) ;      /* thickness of line segment */

      if( new_thick < 0.0f ){              /* special negative thickness codes */
         int thc = (int)(-new_thick) ;     /* mean special drawing actions */
         switch( thc ){
            case THCODE_FRECT:
            case THCODE_RECT:{        /* rectangle */
               x1 = MEMPLOT_X1(mp,ii) ; x2 = MEMPLOT_X2(mp,ii) ; SORD(x1,x2) ;
               y1 = MEMPLOT_Y1(mp,ii) ; y2 = MEMPLOT_Y2(mp,ii) ; SORD(y1,y2) ;
               xbot = MIN(xbot,x1) ; xtop = MAX(xtop,x2) ;
               ybot = MIN(ybot,y1) ; ytop = MAX(ytop,y2) ;
            }
            break ;

            case THCODE_BALL:
            case THCODE_CIRC:{
               int xcen,ycen , rad ; float xrad,yrad ;
               unsigned int ww, hh ;
               xcen = MEMPLOT_X1(mp,ii) ;
               ycen = MEMPLOT_Y1(mp,ii) ;
               rad  = fabsf(MEMPLOT_X2(mp,ii)) ;
               x1   = xcen - rad ; x2 = xcen + rad ;
               y1   = ycen - rad ; y2 = ycen + rad ;
               xbot = MIN(xbot,x1) ; xtop = MAX(xtop,x2) ;
               ybot = MIN(ybot,y1) ; ytop = MAX(ytop,y2) ;
            }
            break ;

            /* other special codes don't actually cause drawing */
         }

      } else { /* normal line segment */

        x1 = MEMPLOT_X1(mp,ii) ; x2 = MEMPLOT_X2(mp,ii) ; SORD(x1,x2) ;
        y1 = MEMPLOT_Y1(mp,ii) ; y2 = MEMPLOT_Y2(mp,ii) ; SORD(y1,y2) ;
        xbot = MIN(xbot,x1) ; xtop = MAX(xtop,x2) ;
        ybot = MIN(ybot,y1) ; ytop = MAX(ytop,y2) ;

      }
   }

   out.a = xbot ; out.b = xtop ;
   out.c = ybot ; out.d = ytop ;
   return out ;
}
