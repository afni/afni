/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _MCW_XIM_HEADER_
#define _MCW_XIM_HEADER_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/X.h>
#include <X11/Intrinsic.h>

#include "mrilib.h"
#include "coxplot.h"
#include "display.h"

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

extern void MCW_kill_XImage( XImage * ) ;
extern XImage * mri_to_XImage( MCW_DC * , MRI_IMAGE * ) ;
extern XImage * resize_XImage( MCW_DC * , XImage * , int , int ) ;
extern MRI_IMAGE * XImage_to_mri( MCW_DC * , XImage * , int ) ;

extern XImage * pixar_to_XImage( MCW_DC * , int,int , Pixel * ) ;
extern XImage * rgb_to_XImage ( MCW_DC * , MRI_IMAGE * ) ;
extern XImage * rgba_to_XImage( MCW_DC * , MRI_IMAGE * ) ; /* 13 Feb 2020 */

extern void rectzero_XImage( MCW_DC *dc , XImage *image ,
                      int x1, int y1, int x2, int y2 )    ;   /* 25 Jun 2013 */

#define X2M_USE_CMAP  (1<<0)  /* masks for XImage_to_mri() 3rd arg */
#define X2M_FORCE_RGB (1<<1)

extern int ISQ_snapfile( Widget w ) ;  /* 25 Jun 2003 */
extern int ISQ_snapfile2( Widget w, char *fout ) ;  /* 3 Nov. 2014 */
extern MRI_IMAGE * SNAP_grab_image( Widget , MCW_DC * ) ;

extern void memplot_to_X11_set_DC( MCW_DC *dc ) ;  /* 30 Apr 2012 */
extern void memplot_to_X11_funfunfun( Display *dpy , Window w , MEM_plotdata *mp ,
                                      int start , int end , int mask ) ;

#undef  X11_SET_NEW_PLOT
#define X11_SET_NEW_PLOT      memplot_to_X11_set_substitute(memplot_to_X11_funfunfun)

#undef  X11_SET_OLD_PLOT
#define X11_SET_OLD_PLOT      memplot_to_X11_set_substitute(NULL)

#undef  X11_GET_PLOT_FUNC
#define X11_GET_PLOT_FUNC     memplot_to_X11_get_substitute()

#undef  X11_SET_PLOT_FUNC
#define X11_SET_PLOT_FUNC(pf) memplot_to_X11_set_substitute(pf)

#ifdef  __cplusplus
}
#endif

#endif /* _MCW_XIM_HEADER_ */
