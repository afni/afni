#include "mrilib.h"
#include "coxplot.h"

/*--------------------------------------------------------------------------
  Routines to render a memplot into an MRI_rgb image.
  The assumption is that the plot is over the region [0,1]x[0,1],
  which is rendered into the image [0..nx-1]x[0..ny-1].
----------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------*/
/*! Set a sub-box within a window into which the next RGB plot should
   be scaled.  (0,0,0,0) args means use the whole window.  After
   each drawing (memplot_to_RGB_sef), will be reset to the whole window
   anyway.
----------------------------------------------------------------------------*/

static int box_xbot=0 , box_xtop=0 ,
           box_ybot=0 , box_ytop=0  ;

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
/*! Actually do the rendering of a memplot into an RGB image.
  - Plotting will start with line #start and go to #end-1.
  - If end <= start, will do from #start to the last one in the plot.
  - To do all lines, set start=end=0.
  - "freee" controls whether the aspect ratio will be free to vary (!= 0),
    or will be fixed (==0).
  - 18 Sep 2001: adapted from X11 routines in coxplot/plot_x11.c
  - 23 Mar 2002: actually tested for the first time
----------------------------------------------------------------------------*/

void memplot_to_RGB_sef( MRI_IMAGE *im , MEM_plotdata * mp ,
                         int start , int end , int freee    )
{
   byte rrr=0,ggg=0,bbb=0 ;
   int ii , nline , same ;
   float old_thick , old_color , new_color , new_thick ;
   float scal,xscal,yscal , xoff,yoff ;
   int x1,y1 , x2,y2 ;
   int skip ;

   /*--- check for madness ---*/

   if( im == NULL || im->kind != MRI_rgb || mp == NULL ) return ;

   if( start < 0 ) start = 0 ;

   nline = MEMPLOT_NLINE(mp) ;
   if( nline < 1 || start >= nline ) return ;

   if( end <= start || end > nline ) end = nline ;

   /*--- compute scaling from memplot objective
         coordinates to RGB window coordinates  ---*/

   if( box_xbot >= box_xtop || box_ybot >= box_ytop ){

      xscal = im->nx / mp->aspect ; /* aspect = x-axis objective size */
      yscal = im->ny / 1.0 ;        /* 1.0    = y-axis objective size */
      xoff  = yoff = 0.499 ;

   } else {  /* scale to a given sub-box in the window */

      xscal = box_xtop - box_xbot ;
      yscal = box_ytop - box_ybot ;
      xoff  = box_xbot + 0.499    ;
      yoff  = box_ybot + 0.499    ;
   }

   if( !freee ){                           /* no aspect freedom ==> */
      if( yscal < xscal ) xscal = yscal ;  /* use smaller scaling   */
      else                yscal = xscal ;
   }
   scal = sqrt(fabs(xscal*yscal)) ;

   old_color = -1.0 ;            /* these don't occur naturally */
   old_thick = -THCODE_INVALID ;

   /*--- loop over lines, scale and plot ---*/

   for( ii=start ; ii < end ; ii++ ){

      skip = 0 ;

      /* check if need to change color or thickness of line */

      new_color = MEMPLOT_COL(mp,ii) ;
      if( new_color != old_color ){
         float rr=COL_TO_RRR(new_color) ,
               gg=COL_TO_GGG(new_color) , bb=COL_TO_BBB(new_color) ;

#if 0
fprintf(stderr,"Changing color to %f %f %f\n",rr,gg,bb) ;
#endif

         rrr = ZO_TO_TFS(rr) ; ggg = ZO_TO_TFS(gg) ; bbb = ZO_TO_TFS(bb) ;
         old_color = new_color ;
      }

      new_thick = MEMPLOT_TH(mp,ii) ;
      if( new_thick < 0.0 ){               /* special negative thickness codes */
         int thc = (int)(-new_thick) ;
         switch( thc ){
            case THCODE_RECT:{        /* rectangle */
               int xb,yb , xt,yt ;
               int w,h ;
               x1 = (int)( xoff + xscal * MEMPLOT_X1(mp,ii)         ) ;
               x2 = (int)( xoff + xscal * MEMPLOT_X2(mp,ii)         ) ;
               y1 = (int)( yoff + yscal * (1.0 - MEMPLOT_Y1(mp,ii)) ) ;
               y2 = (int)( yoff + yscal * (1.0 - MEMPLOT_Y2(mp,ii)) ) ;
               if( x1 < x2 ){ xb=x1; xt=x2; } else { xb=x2; xt=x1; }
               if( y1 < y2 ){ yb=y1; yt=y2; } else { yb=y2; yt=y1; }
               w = xt-xb ; h = yt-yb ;
               mri_drawemptyrectangle( im , xb,yb , w,h , rrr,ggg,bbb ) ;
               skip = 1 ;
            }
            break ;
         }

      } else if( new_thick != old_thick ){ /* normal case: change line thickness */

         old_thick = new_thick ;  /* thickness not used at this time */

      }

      /* scale coords to ints (also see zzphph.f) */

      if( !skip ){
        x1 = (int)( xoff + xscal * MEMPLOT_X1(mp,ii)         ) ;
        x2 = (int)( xoff + xscal * MEMPLOT_X2(mp,ii)         ) ;
        y1 = (int)( yoff + yscal * (1.0 - MEMPLOT_Y1(mp,ii)) ) ;
        y2 = (int)( yoff + yscal * (1.0 - MEMPLOT_Y2(mp,ii)) ) ;

        /* draw it */

        mri_drawline( im , x1,y1 , x2,y2 , rrr,ggg,bbb ) ;
      }
   }

   set_memplot_X11_box(0,0,0,0) ; /* clear box */
   return ;
}
