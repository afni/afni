#ifndef _RWC_3DPLOT_HEADER_
#define _RWC_3DPLOT_HEADER_

#define MEM_3DLINE_TYPE     601
#define MEM_3DTRIANGLE_TYPE 602

typedef struct {
  int   type ;
  char  r,g,b,a ;
  float x1,y1,z1 , x2,y2,z2 , thick ;
} MEM_3dline ;

typedef struct {
  int   type ;
  char  r,g,b,a ;
  float x1,y1,z1 , x2,y2,z2 , x3,y3,z3 ;
} MEM_3dtriangle ;

typedef union {
   int            type     ;
   MEM_3dline     line     ;
   MEM_3dtriangle triangle ;
} MEM_3dobject ;

typedef struct {
   int nobj , nobj_all ;
   MEM_3dobject * obj ;
   char ident[256] ;
} MEM_3dplotdata ;

/* macros to manipulate a plot */

#define INC_MEMPLOT3D 64
#define EXP_MEMPLOT3D 1.1

#define MEMPLOT3D_INIT(name,id)                                                  \
  do{ (name) = (MEM_3dplotdata *) malloc(sizeof(MEM_3dplotdata)) ;               \
      (name)->nobj = 0 ;                                                         \
      (name)->nobj_all = INC_MEMPLOT3D ;                                         \
      (name)->obj = (MEM_3dobject *) malloc(sizeof(MEM_3dobject)*INC_MEMPLOT3D); \
      strncpy( (name)->ident, (id), 255 ) ; (name)->ident[255] = '\0' ;          \
  } while(0)

#define MEMPLOT3D_ADDLINE(name,xx1,yy1,zz1,xx2,yy2,zz2,th,rr,gg,bb,aa)               \
  do{ int nn ; MEM_3dline * ll ;                                                     \
      if( (name)->nobj == (name)->nobj_all ){                                        \
        nn = (name)->nobj_all = EXP_MEMPLOT3D*(name)->nobj_all + INC_MEMPLOT3D;      \
        (name)->obj = (MEM_3dobject *) realloc((name)->obj,sizeof(MEM_3dobject)*nn); \
      }                                                                              \
      nn = (name)->nobj ;                                                            \
      ll = (MEM_3dline *)( (name)->obj + nn ) ;                                      \
      ll->type = MEM_3DLINE_TYPE ;                                                   \
      ll->x1 = (xx1) ; ll->y1 = (yy1) ; ll->z1 = (zz1) ;                             \
      ll->x2 = (xx2) ; ll->y2 = (yy2) ; ll->z2 = (zz2) ; ll->thick = (th) ;          \
      ll->r  = (rr)  ; ll->g  = (gg)  ; ll->b  = (bb)  ; ll->a  = (aa)    ;          \
    } while(0)

#define MEMPLOT3D_ADDTRIANGLE(name,xx1,yy1,zz1,xx2,yy2,zz2,xx3,yy3,zz3,th,rr,gg,bb,aa) \
  do{ int nn ; MEM_3dtriangle * ll ;                                                   \
      if( (name)->nobj == (name)->nobj_all ){                                          \
        nn = (name)->nobj_all = EXP_MEMPLOT3D*(name)->nobj_all + INC_MEMPLOT3D;        \
        (name)->obj = (MEM_3dobject *) realloc((name)->obj,sizeof(MEM_3dobject)*nn);   \
      }                                                                                \
      nn = (name)->nobj ;                                                              \
      ll = (MEM_3dtriangle *)( (name)->obj + nn ) ;                                    \
      ll->type = MEM_3DTRIANGE_TYPE ;                                                  \
      ll->x1 = (xx1) ; ll->y1 = (yy1) ; ll->z1 = (zz1) ;                               \
      ll->x2 = (xx2) ; ll->y2 = (yy2) ; ll->z2 = (zz2) ;                               \
      ll->x3 = (xx3) ; ll->y3 = (yy3) ; ll->z3 = (zz3) ;                               \
      ll->r  = (rr)  ; ll->g  = (gg)  ; ll->b  = (bb)  ; ll->a  = (aa) ;               \
    } while(0)

#define MEMPLOT3D_DESTROY(name)                          \
  do{ if( (name) != NULL ){                              \
         if( (name)->obj != NULL ) free( (name)->obj ) ; \
         free( (name) ) ; (name) = NULL ; }              \
  } while(0)

#define MEMPLOT3D_NOBJ(name)   ((name)->nobj)             /* number of objects */
#define MEMPLOT3D_IDENT(name)  ((name)->ident)            /* identifier string */
#define MEMPLOT3D_NAME         MEMPLOT3D_IDENT

/*----- prototypes -----*/

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
extern int            nline_active_memplot(void) ;
extern void           plotrect_memplot( float,float,float,float ) ; /* 21 Mar 2001 */

extern MEM_plotdata * copy_memplot( MEM_plotdata * ) ; /*-- 26 Feb 2001 --*/
extern void           append_to_memplot( MEM_plotdata *,MEM_plotdata * ) ;
extern void           scale_memplot( float,float,float,float,float,
                                     MEM_plotdata * ) ;

/*-- draw to a PostScript file: see also plot_ps.c --**/

extern void memplot_to_postscript( char * , MEM_plotdata * ) ;

#define memplot_to_ps(fn) memplot_to_postscript( (fn) , get_active_memplot() ) ;

/*-- draw to an X11 window --*/

extern unsigned long rgb_to_pixel( unsigned char , unsigned char ,
                                   unsigned char , X11_colordef * ) ;

extern X11_colordef * get_X11_colordef( Display * , Window ) ;

extern void memplot_to_X11_sef( Display * , Window ,
                                MEM_plotdata * , int,int,int ) ;

extern void set_memplot_X11_box( int,int,int,int ) ;  /* 26 Feb 2001 */

extern void set_X11_background( Display * , Window ,
                                unsigned char , unsigned char , unsigned char ) ;

#define memplot_to_X11(d,w) \
   memplot_to_X11_sef( (d),(w) , get_active_memplot() , 0,0,0 )

#define memplot_to_X11_free(d,w) \
   memplot_to_X11_sef( (d),(w) , get_active_memplot() , 0,0,1 )

typedef struct {
   Widget top , dial , wtf , drawing , form ;
   int valid ;
   MEM_plotdata * mp ;
   void * userdata ;
   void_func * killfunc ;

#ifdef HAVE_XDBE
   int            have_xdbe ;
   XdbeBackBuffer buf_xdbe ;
#endif
} MEM_topshell_data ;

#define MTD_PLOTDATA(mpcb)        ((mpcb)->mp)
#define MTD_KILLFUNC(mpcb)        ((mpcb)->killfunc)
#define MTD_VALID(mpcb)           ((mpcb)->valid)
#define MTD_USERDATA(mpcb)        ((mpcb)->userdata)
#define MTD_remove_killfunc(mpcb) ((mpcb)->killfunc = NULL)

#define MTD_replace_plotdata(mpcb,mpnew) \
  do{ delete_memplot((mpcb)->mp) ; (mpcb)->mp = (mpnew) ; } while(0)

extern MEM_topshell_data * memplot_to_topshell(Display *,MEM_plotdata *,void_func *) ;
extern void plotkill_topshell( MEM_topshell_data * ) ;
extern void redraw_topshell( MEM_topshell_data * ) ;

#define memplot_to_shell(d) memplot_to_topshell( (d),get_active_memplot(),1 )

/*-- plot time series --*/

#define TSP_SEPARATE_YBOX    1
#define TSP_SEPARATE_YSCALE  2

extern void plot_ts_lab( Display *,
                         int,float *, int,float **,
                         char *,char *,char *,char ** , void_func * ) ;

#define plot_ts(a,b,c,d,e) plot_ts_lab((a),(b),(c),(d),(e),NULL,NULL,NULL,NULL,NULL)

extern MEM_topshell_data * plot_ts_init( Display *, float, float,
                                         int, float, float,
                                         char *, char *, char *, char ** , void_func * ) ;

extern void plot_ts_addto( MEM_topshell_data *, int,float *, int,float ** ) ;

extern MEM_plotdata * plot_ts_mem( int,float *, int,int,float **,
                                   char *,char *,char *,char ** ) ;

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

/*-- routines from PLOTPAK, after running through f2c --*/

#include "f2c.h"
#undef complex

extern int color_(integer *ncol);
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

/*----- Commons from PLOTPAK -----*/

#ifdef PLOTPAK_COMMONS
#  define EXT  /* nada */
#else
#  define EXT extern
#endif

EXT struct {
    real xpgmin, ypgmin, xpgmax, ypgmax, xclbot, yclbot, xcltop, ycltop, xbot,
             ybot, xtop, ytop, xmin, ymin, xmax, ymax;
    integer ixcoor, iycoor;
    real alphxx, betaxx, alphyy, betayy, tmajx, tminx, tmajy, tminy;
    integer majrx, minrx, majry, minry, isizx, isizy;
    real xphold, yphold;
} zzzplt_;

EXT struct {
    real xphmax, yphmax;
    integer ixpmax, iypmax;
    real xpscal, ypscal;
    integer iflip, nplotr;
    char cfile[64];
} zzpltr_;

EXT struct {
    integer ndash;
    real xldash[8], xid;
} zzdash_;

#endif
