#ifndef _MCW_MEMPLOT_HEADER_
#define _MCW_MEMPLOT_HEADER_

/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*---------------- Header for in-memory adaptation of PLOTPAK ---------------*/

#include <X11/Intrinsic.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#ifdef HAVE_XDBE
#  include <X11/extensions/Xdbe.h>

#  ifdef MAIN_COXPLOT_FILE
      int use_xdbe = -1 ;
#  else
      extern int use_xdbe ;
#  endif

   extern void init_XDBE(Display *) ;
   extern Window getwin_from_XDBE( Display * , Drawable ) ;
   extern int  get_XDBE_suspension(int) ; /* 01 May 2012 */
   extern void set_XDBE_suspension(int) ;
#else
#  define getwin_from_XDBE(dd,ww) (ww)
#  define get_XDBE_suspension(qq) (1)
#  define set_XDBE_suspension(qq) /*nada*/
#endif

#undef  MAX
#define MAX(a,b) (((a)<(b)) ? (b) : (a))

#undef  MIN
#define MIN(a,b) (((a)>(b)) ? (b) : (a))

#ifndef HOTCOLOR
#define HOTCOLOR(ww,ss)                                                        \
  { char * xdef = XGetDefault(XtDisplay(ww),"AFNI","hotcolor") ;               \
    if( xdef == NULL ) xdef = getenv("AFNI_hotcolor") ;                        \
    if( xdef == NULL ) xdef = getenv("AFNI_HOTCOLOR") ;                        \
    if( xdef == NULL ) xdef = XGetDefault(XtDisplay(ww),"AFNI","background") ; \
    (ss) = (xdef != NULL) ? (xdef) : ("gray40") ; }
#endif

#ifndef BGCOLOR_ARG
#define BGCOLOR_ARG(str) \
  XtVaTypedArg , XmNbackground , XmRString , (str) , strlen(str)+1
#endif

#ifndef VOID_FUNC
#define VOID_FUNC
typedef void void_func() ;
#endif

#ifndef floatfix
#ifdef isfinite
# define floatfix(x) if( !isfinite(x) ) (x) = 0.0f ; else
#else
# define floatfix(x) if( !finite(x) ) (x) = 0.0f ; else
#endif
#endif

/*----- data structure to hold a plot -----*/

typedef struct {
   int nxyline , nxyline_all ;
   float aspect ;
   float * xyline ;
   char ident[256] ;
   int insert_at ;    /* 15 Nov 2001 */
} MEM_plotdata ;

/* macros to manipulate a plot */

#define INC_MEMPLOT 64
#define EXP_MEMPLOT 1.2
#define NXY_MEMPLOT 6

#define INIT_MEMPLOT(name,id)                                                    \
  do{ (name) = (MEM_plotdata *) malloc(sizeof(MEM_plotdata)) ;                   \
      (name)->nxyline = 0 ;                                                      \
      (name)->nxyline_all = INC_MEMPLOT ;                                        \
      (name)->xyline = (float *) malloc(sizeof(float)*NXY_MEMPLOT*INC_MEMPLOT) ; \
      strncpy( (name)->ident, (id), 255 ) ; (name)->ident[255] = '\0' ;          \
      (name)->aspect = 1.3 ; (name)->insert_at = -1 ;                            \
  } while(0)

/* put a line at the end of the plot [15 Nov 2001: maybe in the middle]  */

#define ADDTO_MEMPLOT(name,x1,y1,x2,y2,col,th)                                             \
  do{ int nn , ll=(name)->insert_at ;                                                      \
      if( ll >= 0 && ll < (name)->nxyline ){                                               \
         nn = NXY_MEMPLOT * ll ;                                                           \
         (name)->xyline[nn++] = (x1) ; (name)->xyline[nn++] = (y1) ;                       \
         (name)->xyline[nn++] = (x2) ; (name)->xyline[nn++] = (y2) ;                       \
         (name)->xyline[nn++] = (col); (name)->xyline[nn++] = (th) ; break ;               \
      }                                                                                    \
      if( (name)->nxyline == (name)->nxyline_all ){                                        \
        nn = (name)->nxyline_all = EXP_MEMPLOT * (name)->nxyline_all + INC_MEMPLOT ;       \
        (name)->xyline = (float *)realloc( (name)->xyline, sizeof(float)*NXY_MEMPLOT*nn ); \
        if( nn > 999999 ){                                                                 \
          double qq=log10((double)nn) ;                                                    \
          if( qq-(int)qq < 0.08 )                                                          \
            fprintf(stderr,"** WARNING: in memory plot exceeds %d elements!\n",nn) ;       \
      } }                                                                                  \
      nn = NXY_MEMPLOT * (name)->nxyline ;                                                 \
      (name)->xyline[nn++] = (x1) ; (name)->xyline[nn++] = (y1) ;                          \
      (name)->xyline[nn++] = (x2) ; (name)->xyline[nn++] = (y2) ;                          \
      (name)->xyline[nn++] = (col); (name)->xyline[nn++] = (th) ; (name)->nxyline ++ ;     \
  } while(0)

/* this is fatal */

#define DESTROY_MEMPLOT(name)                                  \
  do{ if( (name) != NULL ){                                    \
         if( (name)->xyline != NULL ) free( (name)->xyline ) ; \
         free( (name) ) ; (name) = NULL ; }                    \
  } while(0)

/* 14 Nov 2001: cut off the end of a plot */

#define TRUNC_MEMPLOT(name,num)                                         \
  do{ if( (num) < (name)->nxyline ) (name)->nxyline = (num); } while(0)

#define MEMPLOT_X1(name,ii)  ((name)->xyline[NXY_MEMPLOT*ii])    /* from x */
#define MEMPLOT_Y1(name,ii)  ((name)->xyline[NXY_MEMPLOT*ii+1])  /* from y */
#define MEMPLOT_X2(name,ii)  ((name)->xyline[NXY_MEMPLOT*ii+2])  /* to x   */
#define MEMPLOT_Y2(name,ii)  ((name)->xyline[NXY_MEMPLOT*ii+3])  /* to y    */
#define MEMPLOT_COL(name,ii) ((name)->xyline[NXY_MEMPLOT*ii+4])  /* color    */
#define MEMPLOT_TH(name,ii)  ((name)->xyline[NXY_MEMPLOT*ii+5])  /* thickness */

#define MEMPLOT_NLINE(name)  ((name)->nxyline)                /* number of lines */
#define MEMPLOT_IDENT(name)  ((name)->ident)                  /* identifier string */
#define MEMPLOT_NAME         MEMPLOT_IDENT
#define MEMPLOT_ASPECT(name) ((name)->aspect)                 /* aspect ratio */

 /*-- thickness codes < 0 are special instructions --*/

#define THCODE_RECT       1  /* 21 Mar 2001: rectangle from (x1,y1)..(x2,y2) */
#define THCODE_CIRC       2  /* 10 Mar 2002: circle at (x1,y1), radius x2 */
#define THCODE_OPAC       3  /* 22 Jul 2004: set opacity of further drawing to x1 */
#define THCODE_BALL       4
#define THCODE_FRECT      5  /* 24 Apr 2012: filled rectangle */
#define THCODE_INVALID  666

/* convert (r,g,b) in [0,1]**3 into a single number, and vice-versa */
/* acronyms: ZO == Zero-to-One;  TFS == initials of 256 spelled out */

#define ZO_TO_TFS(x) ((int)(255.99*(x)))    /* produces a number from 0 .. 255   */
#define TFS_TO_ZO(y) ((y)/255.0f)           /* produces a number from 0.0 .. 1.0 */

#define RGB_TO_COL(r,g,b) ( (ZO_TO_TFS(r)<<16) | (ZO_TO_TFS(g)<<8) | (ZO_TO_TFS(b)) )

#define COL_TO_RRR(cc) TFS_TO_ZO( (((int)(cc)) & 0xff0000) >> 16 )
#define COL_TO_GGG(cc) TFS_TO_ZO( (((int)(cc)) & 0x00ff00) >>  8 )
#define COL_TO_BBB(cc) TFS_TO_ZO( (((int)(cc)) & 0x0000ff)       )

/*----- stuff for plotting into an X11 window -----*/

typedef struct {
   int classKRH ;    /* type of colormap: PseudoColor and TrueColor are OK */
   int depth ;

   int ncolors ;  /* This stuff for PseudoColor */
   unsigned char * rr , * gg , * bb ;

   unsigned long rrmask , ggmask , bbmask ;   /* This stuff for TrueColor */
   int           rrshift, ggshift, bbshift;
} X11_colordef ;

#define FREE_X11_colordef(cd)                                    \
  do{ if( (cd) != NULL ){                                        \
         if( (cd)->rr != NULL ){                                 \
            free((cd)->rr) ; free((cd)->gg) ; free((cd)->bb) ; } \
         free((cd)) ; (cd) = NULL ; } } while(0)

/*----- prototypes -----*/

#ifdef  __cplusplus
extern "C" {
#endif

extern MEM_plotdata * find_memplot( char * ) ;
extern int            create_memplot( char * , float ) ;
extern int            set_active_memplot( char * ) ;
extern MEM_plotdata * get_active_memplot(void) ;
extern void           delete_active_memplot(void) ;
extern void           delete_memplot( MEM_plotdata * ) ;
extern void           plotline_memplot( float , float , float , float ) ;
extern void           set_color_memplot( float , float , float ) ;
extern void           set_thick_memplot( float ) ;
extern float          get_thick_memplot(void) ;
extern void           plot_ts_dohist(int) ;                         /* 10 Jun 2014 */
extern void           plot_ts_noline( int ) ;
extern int            nline_active_memplot(void) ;
extern void           plotrect_memplot( float,float,float,float ) ; /* 21 Mar 2001 */
extern void           plotfrect_memplot( float,float,float,float) ; /* 24 Apr 2012 */
extern void           plotcirc_memplot( float,float,float ) ;       /* 10 Mar 2002 */
extern void           plotball_memplot( float,float,float ) ;
extern int            create_memplot_surely( char *, float ) ;      /* 20 Sep 2001 */
extern void           set_opacity_memplot( float ) ;                /* 22 Jul 2004 */
extern float          get_opacity_memplot(void) ;
extern void           plot_ts_do_naked  (int) ;                     /* 14 Sep 2018 */

extern MEM_plotdata * copy_memplot( MEM_plotdata * ) ; /*-- 26 Feb 2001 --*/
extern void           append_to_memplot( MEM_plotdata *,MEM_plotdata * ) ;
extern void           scale_memplot( float,float,float,float,float,MEM_plotdata * );
extern MEM_plotdata * clip_memplot( float,float,float,float , MEM_plotdata * ) ;

extern void           cutlines_memplot( int,int,MEM_plotdata * ) ; /* 15 Nov 2001 */
extern void           insert_at_memplot( int , MEM_plotdata * ) ;
#define insert_atend_memplot(mm) insert_at_memplot(-1,mm)

#define MRI_ROT_0   1  /* codes for various rotations */
#define MRI_ROT_90  2  /* [do not change these unless */
#define MRI_ROT_180 4  /*  mrilib.h is changed also!] */
#define MRI_ROT_270 8
#define MRI_FLMADD  128

extern void flip_memplot( int , int , MEM_plotdata * ) ; /* 30 Aug 2001 */

/*-- draw to a PostScript file: see also plot_ps.c --**/

extern void memplot_to_postscript( char * , MEM_plotdata * ) ;

#define memplot_to_ps(fn) memplot_to_postscript( (fn) , get_active_memplot() ) ;

/*-- draw to an X11 window --*/

extern unsigned long rgb_to_pixel( unsigned char , unsigned char ,
                                   unsigned char , X11_colordef * ) ;

extern X11_colordef * get_X11_colordef( Display * , Window ) ;

#define MEMPLOT_FREE_ASPECT 1  /* masks for memplot_to_X11_sef() */
#define MEMPLOT_ERASE       2

extern void memplot_to_X11_sef( Display * , Window ,
                                MEM_plotdata * , int,int,int ) ;

extern void   memplot_to_X11_set_substitute( void (*msf)() ) ;  /* 30 Apr 2012 */
extern void * memplot_to_X11_get_substitute( void ) ;

extern void drawable_geom( Display *dpy , Drawable ddd ,      /* 30 Apr 2012 */
                           int *width , int *height , int *depth ) ;

extern void set_memplot_X11_box( int,int,int,int ) ;  /* 26 Feb 2001 */

extern void set_X11_background( Display * , Window ,
                                unsigned char , unsigned char , unsigned char ) ;

extern void set_memplot_X11_rectfill( int ) ;  /* 26 Oct 2005 */

#define memplot_to_X11(d,w) \
   memplot_to_X11_sef( (d),(w) , get_active_memplot() , 0,0,0 )

#define memplot_to_X11_free(d,w) \
   memplot_to_X11_sef( (d),(w) , get_active_memplot() , 0,0,1 )

extern void set_memplot_RGB_box( int xbot, int ybot, int xtop, int ytop ) ;

extern void memplot_to_RGB_sef( MRI_IMAGE *im , MEM_plotdata *mp ,
                                int start , int end , int freee    ) ;

extern void memplot_to_jpg( char * , MEM_plotdata * ) ; /* 05 Dec 2007 */
extern void memplot_to_png( char * , MEM_plotdata * ) ;
extern void memplot_to_pnm( char * , MEM_plotdata * ) ; /* 06 Jan 2015 */

extern void memplot_to_mri_set_dothick( int ) ;         /* 30 Apr 2012 */
extern void memplot_to_mri_set_dofreee( int ) ;         /* 30 Apr 2012 */

typedef struct {
   Widget top , dial , wtf , drawing , form, clonebut ;
   int valid ;
   MEM_plotdata * mp ;
   void * userdata ;
   void_func * killfunc ;
#ifdef HAVE_XDBE
   int            have_xdbe ;
   XdbeBackBuffer buf_xdbe ;
#endif

   void (* clonebut_user_cb)(void *data); /* for SUMA */
   int cloned; /* for SUMA */
} MEM_topshell_data ;

#define MTD_PLOTDATA(mpcb)        ((mpcb)->mp)
#define MTD_KILLFUNC(mpcb)        ((mpcb)->killfunc)
#define MTD_VALID(mpcb)           ((mpcb)->valid)
#define MTD_USERDATA(mpcb)        ((mpcb)->userdata)
#define MTD_remove_killfunc(mpcb) ((mpcb)->killfunc = NULL)

#define MTD_replace_plotdata(mpcb,mpnew) \
  do{ delete_memplot((mpcb)->mp) ; (mpcb)->mp = (mpnew) ; } while(0)

extern MEM_topshell_data * memplot_to_topshell(Display *,MEM_plotdata *,void_func *) ;
extern MEM_topshell_data * suma_memplot_to_topshell( Display *dpy,
                                         MEM_plotdata *mp, void_func *kfun );
extern void plotkill_topshell( MEM_topshell_data * ) ;
extern void redraw_topshell( MEM_topshell_data * ) ;
extern void set_wintitle_memplot( char *s );
extern void memplot_topshell_setsaver( char * ,
                                       void (*)(char *,MEM_plotdata *) ) ;

#define memplot_to_shell(d) memplot_to_topshell( (d),get_active_memplot(),1 )

/*-- plot time series --*/

#define TSP_SEPARATE_YBOX    1
#define TSP_SEPARATE_YSCALE  2
#define TSP_SEPARATE_XAXIS   4

extern void plot_ts_xypush( int , int ) ;
extern void plot_ts_dobox ( float ) ;
extern void plot_ts_xfix( int,int , float,float ) ;                /* 22 Jul 2003 */
extern void plot_ts_yfix( int,int , float,float ) ;

extern void plot_ts_set_aspect( float asp ) ;                      /* 03 May 2018 */
extern void plot_ts_do_perim  ( int   dp  ) ;

extern void plot_ts_add_vbox( int,float,float,float,float,float ); /* 24 Apr 2012 */

extern void plot_ts_add_rbox( int ygr ,
                              float x1,float y1 , float x2,float y2,float y3 ,
                              float rr,float gg,float bb,
                              float r2,float g2,float b2 ) ;       /* 29 Jan 2017 */

extern void plot_ts_add_tlin( int ygr ,                            /* 19 Sep 2017 */
                              float x1,float y1 , float x2,float y2,
                              float rr,float gg,float bb, int dcode ) ;

extern void plot_ts_add_sepx( int lx , float *x ) ;                /* 21 Oct 2013 */
extern void plot_ts_clear_sepx(void) ;
extern void plot_ts_fetch_sepx( int *ns , int **ls , float ***sx ) ;
extern void plot_ts_setdash( int ndash , int *code ) ;

extern void plot_ts_setcolors( int, float *, float *, float * ) ;  /* 23 Nov 2007 */
extern void plot_ts_setTHIK( float thk ) ;                         /* 26 Nov 2007 */
extern void plot_ts_setthik( float thk ) ;                         /* 02 May 2012 */
#define plot_ts_setthick plot_ts_setthik /* for clumsy typists */
#define plot_ts_sethik   plot_ts_setthik
#define plot_ts_sethick  plot_ts_setthik
extern void plot_ts_setthik_12( int n1, int n2, float thk ) ;      /* 04 Mar 2013 */

extern void plot_ts_lab( Display *,
                         int,float *, int,float **,
                         char *,char *,char *,char ** , void_func * ) ;

extern MEM_plotdata * plot_ts_ebar( int nx , float *x , float *y , float *ey ,
                                    char *lab_xxx , char *lab_yyy , char *lab_top ) ;

extern void plot_ts_ebar_win( Display *,
                              int,float *,float *,float *,char *,char *,char *,
                              void_func * ) ;

#define plot_ts(a,b,c,d,e) plot_ts_lab((a),(b),(c),(d),(e),NULL,NULL,NULL,NULL,NULL)

extern MEM_topshell_data * plot_ts_init( Display *, float, float,
                                         int, float, float,
                                         char *, char *, char *, char ** , void_func * ) ;

extern void plot_ts_addto( MEM_topshell_data *, int,float *, int,float ** ) ;

extern MEM_plotdata * plot_ts_mem( int,float *, int,int,float **,
                                   char *,char *,char *,char ** ) ;

/*-- 15 Nov 2001: routines for strip plot --*/

extern MEM_topshell_data * plot_strip_init( Display * , int , float ,
                                            int , float , float ,
                                            char * , char * ,
                                            char * , char ** , void_func * ) ;

extern void plot_strip_addto( MEM_topshell_data * , int , float ** ) ;

extern void plot_strip_clear( MEM_topshell_data * ) ;

/*-- routines in this library that will be called from PLOTPAK --*/

extern void zzmpco_( float * , float * , float * ) ;
extern void zzmpli_( float * , float * , float * , float * ) ;

/*-- ps_plot.c routines --*/

extern void ps_move( int , int ) ;               /* move current position     */
extern void ps_line( int , int , int , int ) ;   /* draw a line from a to b   */
extern void ps_cont( int , int ) ;               /* draw a line from current  */
extern void ps_point( int , int ) ;              /* draw a point              */
extern void ps_label( char * ) ;                 /* draw a string (Courier)   */
extern void ps_arc( int , int , int , int , int , int ) ;  /* draw an arc     */
extern void ps_circle( int , int , int ) ;                 /* draw a circle   */
extern void ps_erase( void ) ;                             /* new page        */
extern void ps_linemod( char * ) ;                         /* line styles     */
extern void ps_space( int , int , int , int ) ;            /* set plot space  */
extern int  ps_openpl( char * ) ;                          /* open plot file  */
extern void ps_closepl( void ) ;                           /* close plot file */
extern void ps_setrgb( float , float , float ) ;           /* set color */
extern void ps_setwidth( float ) ;                         /* set linewidth */
extern void ps_rect( int,int,int,int) ;                    /* filled rectangle */

#ifdef  __cplusplus
}
#endif

/*-- routines from PLOTPAK, after running through f2c --*/

#include "f2c.h"
#undef complex

#ifdef  __cplusplus
extern "C" {
#endif

extern int color_(integer *ncol);
extern int fcolor_( real *cr, real *cg, real *cb );
extern int curve_(real *x, real *y, integer *n);
extern int frame_(void);
extern int frstpt_(real *x, real *y);
extern int labmod_(integer *ifmtx, integer *ifmty, integer *numx, integer *numy, integer *jsizx, integer *jsizy, integer *ixdec, integer *iydec, integer *ixor);
extern int line_(real *x1, real *y1, real *x2, real *y2);
extern int memplt_(real *aspect);
extern int perim_(integer *mbx, integer *mlx, integer *mby, integer *mly);
extern int periml_(integer *mbx, integer *mlx, integer *mby, integer *mly);
extern int perimm_(integer *mbx, integer *mlx, integer *mby, integer *mly, integer *ilab);
extern int phdot_(real *x1, real *y1);
extern int phline_(real *x1, real *y1, real *x2, real *y2);
extern int point_(real *x, real *y);
extern int points_(real *x, real *y, integer *n, integer *ichar, integer *ipen);
extern int pwrit_(real *x, real *y, char *ch, integer *nch, integer *isiz, integer *ior, integer *icent, ftnlen ch_len);
extern int pwritf_(real *x, real *y, char *ch, integer *nch, integer *isiz, integer *ior, integer *icent, ftnlen ch_len);
extern integer lastnb_(char *cline, ftnlen cline_len);
extern int zzstro_(char *ch, integer *nch, integer *nstr, real *xstr, real *ystr, logical *lbstr, ftnlen ch_len);
extern int zzconv_(char *chin, integer *nchin, char *chout, integer *nchout, ftnlen chin_len, ftnlen chout_len);
extern int set_(real *xobj1, real *xobj2, real *yobj1, real *yobj2, real *xsub1, real *xsub2, real *ysub1, real *ysub2, integer *ltype);
extern int setdsh_(integer *nd, real *xld);
extern int setfrm_(real *xobj1, real *xobj2, real *yobj1, real *yobj2);
extern int setlin_(integer *ntype);
extern int setw_(real *x1, real *y1, real *x2, real *y2);
extern int srface_(real *x, real *y, real *z__, integer *m, integer *mx, integer *nx, integer *ny, real *s, real *stereo);
extern int srfpl_(integer *n, real *px, real *py);
extern int clset_(real *z__, integer *mx, integer *nx, integer *ny, real *chi, real *clo, real *cinc, integer *nla, integer *nlm, real *cl, integer *ncl, integer *icnst, integer *ioffp, real *spval, real *bigest);
extern int ctcell_(real *z__, integer *mx, integer *nx, integer *ny, integer *m, integer *i0, integer *j0);
extern int draws_(integer *mx1, integer *my1, integer *mx2, integer *my2, integer *idraw, integer *imark);
extern int setr_(real *xmin, real *xmax, real *ymin, real *ymax, real *zmin, real *zmax, real *r0);
extern int trn32s_(real *x, real *y, real *z__, real *xt, real *yt, real *zt, integer *iflag);
extern int srfabd_(void);
extern int tick4_(integer *lmajx, integer *lminx, integer *lmajy, integer *lminy);
extern int vector_(real *x, real *y);
extern int zzaxxx_(real *x1, real *x2, real *y, integer *iside, integer *ilab);
extern int zzaxyy_(real *x, real *y1, real *y2, integer *iside, integer *ilab);
extern int zzchar_(char *ch, real *xp, real *yp, real *ct, real *st, ftnlen ch_len);
extern int zzclip_(real *x1in, real *y1in, real *x2in, real *y2in);
extern int zzlabl_(real *val, char *cout, integer *nchar, ftnlen cout_len);
extern int zzlgin_(real *xt, real *pwrten, integer *nlog);
extern int zzline_(real *x1, real *y1, real *x2, real *y2);
extern int zzlinx_(real *x1, real *x2, real *y, integer *majrx, real *tmaj, integer *minrx, real *tmin);
extern int zzliny_(real *x, real *y1, real *y2, integer *majry, real *tmaj, integer *minry, real *tmin);
extern int zzlogx_(real *x1, real *x2, real *y, integer *ndec, real *tmaj, real *tmin);
extern int zzlogy_(real *x, real *y1, real *y2, integer *ndec, real *tmaj, real *tmin);
extern int zzperi_(integer *ilab);
extern int zzphph_(real *x1, real *y1, real *x2, real *y2);
extern int zzphys_(real *x, real *y);

/*-- Versions that are easier to call from C: --*/

extern void plotpak_curve( float * x , float * y , int n ) ;
extern void plotpak_frame(void) ;
extern void plotpak_frstpt( float x , float y ) ;
extern void plotpak_labmod( int jsizx , int jsizy ) ;
extern void plotpak_line( float x1 , float y1 , float x2 , float y2 ) ;
extern void plotpak_perim( int mbx , int mlx , int mby , int mly ) ;
extern void plotpak_periml( int mbx , int mlx , int mby , int mly ) ;
extern void plotpak_perimm( int mbx , int mlx , int mby , int mly , int ilab ) ;
extern void plotpak_phdot( float x1 , float y1 ) ;
extern void plotpak_phline( float x1 , float y1 , float x2 , float y2 ) ;
extern void plotpak_point( float x1 , float y1 ) ;
extern void plotpak_points( float *x , float *y , int n , int ipen ) ;
extern void plotpak_pwrit( float x , float y , char * ch , int isiz , int ior , int icent ) ;
extern void plotpak_pwritf( float x , float y , char * ch , int isiz , int ior , int icent ) ;
extern void plotpak_pwritf_phys( float x , float y , char * ch , int isiz , int ior , int icent ) ;
extern void plotpak_set( float xo1,float xo2 , float yo1,float yo2 ,
                         float xs1,float xs2 , float ys1,float ys2 , int code ) ;
extern void plotpak_setdsh( int nd , float * xd ) ;
extern void plotpak_setfrm( float xo1,float xo2 , float yo1,float yo2 ) ;
extern void plotpak_setlin( int code ) ;
extern void plotpak_setw( float xo1,float xo2 , float yo1,float yo2 ) ;
extern void plotpak_tick4( int mx, int lx , int my , int ly ) ;
extern void plotpak_vector( float x , float y ) ;

extern void plotpak_srface( float *, float *, float *, int,int, float,float ) ;

extern void plotpak_getset( float *xo1,float *xo2 , float *yo1,float *yo2 ,
                            float *xs1,float *xs2 , float *ys1,float *ys2  ) ;

/* 20 Nov 2001: routines to convert between user and memplot coords */

extern void plotpak_zzphys( float x1 , float y1 , float *x2 , float *y2 ) ;
extern void plotpak_unphys( float x1 , float y1 , float *x2 , float *y2 ) ;

/* 02 Dec 2015 */

extern int find_color_name( char *cnam, float *rr, float *gg, float *bb ) ;

/*----- Commons from PLOTPAK -----*/

#ifdef PLOTPAK_COMMONS
#  define EXT  /* nada */
#else
#  define EXT extern
#endif

typedef struct {
    real xpgmin, ypgmin, xpgmax, ypgmax, xclbot, yclbot, xcltop, ycltop, xbot,
             ybot, xtop, ytop, xmin, ymin, xmax, ymax;
    integer ixcoor, iycoor;
    real alphxx, betaxx, alphyy, betayy, tmajx, tminx, tmajy, tminy;
    integer majrx, minrx, majry, minry, isizx, isizy;
    real xphold, yphold;
} zzzplt_type ;

EXT zzzplt_type zzzplt_ ;

typedef struct {
    real xphmax, yphmax;
    integer ixpmax, iypmax;
    real xpscal, ypscal;
    integer iflip, nplotr;
    char cfile[64];
} zzpltr_type ;

EXT zzpltr_type zzpltr_ ;

typedef struct {
    integer ndash;
    real xldash[8], xid;
} zzdash_type ;

EXT zzdash_type zzdash_ ;

#ifdef  __cplusplus
}
#endif

#endif
