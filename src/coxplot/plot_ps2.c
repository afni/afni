#include "coxplot.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

/*------------------------------------------------------------------------
  Routine to render a memplot into a PostScript file.
  (Also see the file ps_plot.c.)
--------------------------------------------------------------------------*/

#ifdef PSIZE
#undef PSIZE
#endif
#define PSIZE 4096

void memplot_to_postscript( char * fname , MEM_plotdata * mp )
{
   int ii , nline ;
   float old_thick , old_color , new_color , new_thick ;
   int   x1,y1 , x2,y2 ;
   int   skip ;

   /*-- sanity checks --*/

   if( fname == NULL || fname[0] == '\0' || mp == NULL ) return ;

   nline = MEMPLOT_NLINE(mp) ; if( nline < 1 ) return ;

   /*-- open the output file --*/

   if( ! ps_openpl(fname) ) return ;
   ps_space( 0,0,PSIZE,PSIZE ) ;

   old_color = -1.0 ;
   old_thick = -THCODE_INVALID ;

   /*-- loop over lines, scale and plot --*/

   for( ii=0 ; ii < nline ; ii++ ){

      skip = 0 ;

      /* check if need to change color or thickness of line */

      new_color = MEMPLOT_COL(mp,ii) ;
      if( new_color != old_color ){
         float rr=COL_TO_RRR(new_color) ,
               gg=COL_TO_GGG(new_color) , bb=COL_TO_BBB(new_color) ;
         ps_setrgb( rr , gg , bb ) ;
         old_color = new_color ;
      }
      new_thick = MEMPLOT_TH(mp,ii) ;
      if( new_thick < 0.0 ){           /* 21 Mar 2001: negative thickness codes */
         int thc = (int)(-new_thick) ;
         switch( thc ){
            case THCODE_RECT:{        /* rectangle */
               x1 = 0.499 + PSIZE * (1.0 - MEMPLOT_Y1(mp,ii)) ;
               x2 = 0.499 + PSIZE * (1.0 - MEMPLOT_Y2(mp,ii)) ;
               y1 = 0.499 + PSIZE * MEMPLOT_X1(mp,ii) ;
               y2 = 0.499 + PSIZE * MEMPLOT_X2(mp,ii) ;
               ps_rect( x1,y1 , x2,y2 ) ;
               skip = 1 ;
            }
            break ;

            case THCODE_CIRC:{        /* circle */
               x1 = 0.499 + PSIZE * (1.0 - MEMPLOT_Y1(mp,ii)) ;
               y1 = 0.499 + PSIZE * MEMPLOT_X1(mp,ii) ;
               x2 = 0.499 + PSIZE * MEMPLOT_X2(mp,ii) ;
               ps_circle( x1,y1 , x2 ) ;
               skip = 1 ;
            }
            break ;
         }
      } else if( new_thick != old_thick ){  /* old code to change line thickness */
         float th = PSIZE * new_thick ;
         if( th <= 0.0 ) th = 1.0 ;
         ps_setwidth( th ) ;
         old_thick = new_thick ;
      }

      if( !skip ){
        /* scale coords (also see zzphph.f) */

        x1 = 0.499 + PSIZE * (1.0 - MEMPLOT_Y1(mp,ii)) ;
        x2 = 0.499 + PSIZE * (1.0 - MEMPLOT_Y2(mp,ii)) ;
        y1 = 0.499 + PSIZE * MEMPLOT_X1(mp,ii) ;
        y2 = 0.499 + PSIZE * MEMPLOT_X2(mp,ii) ;

        ps_line( x1,y1 , x2,y2 ) ;
      }
   }

   /*-- done --*/

   ps_closepl() ;
   return ;
}
