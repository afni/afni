/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _MCW_GRAFFF_HEADER_
#define _MCW_GRAFFF_HEADER_

/* header for MCW_graf stuff -- derived from xv program */

#include "bbox.h"       /* MCW stuff for buttons, etc. */

#include "mcw_malloc.h"

#define MAX_GHANDS 16   /* maximum # of handles */

#define GRAF_SIZE 128

typedef struct {
   Widget topform , toplabel , drawer , reset_pb ;
   MCW_bbox * curve_bbox ;
   MCW_arrowval * handle_av ;
   MCW_DC * dc ;
   Pixel fg , bg ;
   Window gwin ;

   gen_func * cbfunc ;
   void     * cbdata ;

   int spline ;                /* spline curve or lines? */
   int nhands ;                /* current # of handles */
   int yeqx ;                  /* is it y=x? */
   XPoint hands[MAX_GHANDS] ;  /* positions of handles */
   byte func[256] ;            /* output function */
   byte oldf[256] ;            /* for comparison */

   float xbot,xtop,ybot,ytop ; /* for scaling x,y axes */
   Widget popmenu , poplabel ;
} MCW_graf ;

#define UNMANAGE_GRAF(g)                                           \
  do{ Widget wl[6] = { (g)->topform, (g)->toplabel , (g)->drawer , \
                       (g)->reset_pb , (g)->curve_bbox->wrowcol ,  \
                       (g)->handle_av->wrowcol } ;                 \
      XtUnmanageChildren( wl , 6 ) ; } while(0)

#define MANAGE_GRAF(g)                                             \
  do{ Widget wl[6] = { (g)->topform, (g)->toplabel , (g)->drawer , \
                       (g)->reset_pb , (g)->curve_bbox->wrowcol ,  \
                       (g)->handle_av->wrowcol } ;                 \
      XtManageChildren( wl , 6 ) ; } while(0)

/* RANGE forces a to be in the range b..c (inclusive) */

#define RANGE(a,b,c) { if (a < b) a = b;  if (a > c) a = c; }

/* PTINRECT returns '1' if x,y is in rect */

#define PTINRECT(x,y,rx,ry,rw,rh) \
           ((x)>=(rx) && (y)>=(ry) && (x)<((rx)+(rw)) && (y)<((ry)+(rh)))

/* prototypes */

extern MCW_graf * new_MCW_graf( Widget wpar , MCW_DC * , char * title ,
                                gen_func * cbfunc , void * cbdata ) ;

extern void GRAF_drawing_EV( Widget w , XtPointer client_data ,
                             XEvent * ev , Boolean * continue_to_dispatch ) ;

extern void drawGraf( MCW_graf * gp , int erase ) ;

extern void GRAF_curve_CB( Widget w, XtPointer client_data, XtPointer call_data ) ;

extern void GRAF_reset_CB( Widget w, XtPointer client_data, XtPointer call_data ) ;

extern void GRAF_handle_CB( MCW_arrowval * av , XtPointer client_data ) ;

extern void GRAF_put_setup( MCW_graf * , int   , int * , int * , int   ) ;
extern void GRAF_get_setup( MCW_graf * , int * , int * , int * , int * ) ;

extern void GenerateGrafFunc( MCW_graf * gp , int redraw ) ;
extern void InitSpline(int *x,int *y,int n,double *y2) ;
extern double EvalSpline(int xa[],int ya[],double y2a[],int n,double x) ;
extern void xvbcopy(char * src, char * dst, size_t len) ;

extern int GRAF_changed( MCW_graf * gp ) ;
extern void GRAF_set_func( MCW_graf * gp , byte * func ) ;

extern void GRAF_set_xyrange( MCW_graf *gp , float xb,float xt, float yb,float yt ) ;

/* passive graf */

#define PASGRAF_LINE  0        /* for 'mode', below */
#define PASGRAF_BAR   1

#define PASGRAF_MODENUM 2
#define NEXT_PASGRAF_MODE(g) ( (g)->mode = ((g)->mode+1)%PASGRAF_MODENUM )

typedef struct {
   Widget topform , toplabel , drawer ;
   MCW_DC * dc ;
   Pixel fg , bg ;
   Window gwin ;

   int mode ;
   byte func[GRAF_SIZE] ;

   float xbot,xtop,ybot,ytop ; /* for scaling x,y axes */
   Widget popmenu , poplabel ;
} MCW_pasgraf ;

#define UNMANAGE_PASGRAF(g)                                          \
  do{ Widget wl[3] = { (g)->topform, (g)->toplabel , (g)->drawer } ; \
      XtUnmanageChildren( wl , 3 ) ; } while(0)

#define MANAGE_PASGRAF(g)                                            \
  do{ Widget wl[3] = { (g)->topform, (g)->toplabel , (g)->drawer } ; \
      XtManageChildren( wl , 3 ) ; } while(0)

extern MCW_pasgraf * new_MCW_pasgraf( Widget wpar , MCW_DC * dc , char * title ) ;
extern void GRAF_pasdrawing_EV( Widget w , XtPointer client_data ,
                         XEvent * ev , Boolean * continue_to_dispatch ) ;
extern void redraw_MCW_pasgraf( MCW_pasgraf * gp ) ;
extern void set_MCW_pasgraf( MCW_pasgraf * gp , byte * func ) ;

extern void MCW_histo_bytes( int nb , byte * bar , int * har ) ;

extern void PASGRAF_set_xyrange( MCW_pasgraf *gp , float xb,float xt, float yb,float yt ) ;

#endif /* _MCW_GRAFFF_HEADER_ */
