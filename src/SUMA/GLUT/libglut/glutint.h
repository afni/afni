#ifndef __glutint_h__
#define __glutint_h__

/* Copyright (c) Mark J. Kilgard, 1994. */

/* This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. */

#ifdef __sgi
#define SUPPORT_FORTRAN
#endif
#include <X11/Xlib.h>
#include <GL/glx.h>
#include <GL/glut.h>
#ifdef __vms
struct timeval {
  __int64 val;
};
extern int sys$gettim(struct timeval *);
#else
#include <sys/types.h>
#include <sys/time.h>
#endif
#if defined(__vms)

/* One TICK on VMS is 100 nanoseconds; 0.1 microseconds or
   0.0001 milliseconds. This means that there are 0.01
   ticks/ns, 10 ticks/us, 10,000 ticks/ms and 10,000,000
   ticks/second. */

#define TICKS_PER_MILLISECOND 10000
#define TICKS_PER_SECOND      10000000

#define GETTIMEOFDAY(_x) (void) sys$gettim (_x);

#define ADD_TIME(dest, src1, src2) { \
  (dest).val = (src1).val + (src2).val; \
}

#define TIMEDELTA(dest, src1, src2) { \
  (dest).val = (src1).val - (src2).val; \
}

#define IS_AFTER(t1, t2) ((t2).val > (t1).val)

#define IS_AT_OR_AFTER(t1, t2) ((t2).val >= (t1).val)

#else
#if defined(SVR4) && !defined(sun)  /* Sun claims SVR4, but wants 2 args. */
#define GETTIMEOFDAY(_x) gettimeofday(_x)
#else
#define GETTIMEOFDAY(_x) gettimeofday(_x, (struct timezone*) NULL)
#endif
#define ADD_TIME(dest, src1, src2) { \
  if(((dest).tv_usec = \
    (src1).tv_usec + (src2).tv_usec) >= 1000000) { \
    (dest).tv_usec -= 1000000; \
    (dest).tv_sec = (src1).tv_sec + (src2).tv_sec + 1; \
  } else { \
    (dest).tv_sec = (src1).tv_sec + (src2).tv_sec; \
    if(((dest).tv_sec >= 1) && (((dest).tv_usec <0))) { \
      (dest).tv_sec --;(dest).tv_usec += 1000000; \
    } \
  } \
}
#define TIMEDELTA(dest, src1, src2) { \
  if(((dest).tv_usec = (src1).tv_usec - (src2).tv_usec) < 0) { \
    (dest).tv_usec += 1000000; \
    (dest).tv_sec = (src1).tv_sec - (src2).tv_sec - 1; \
  } else { \
     (dest).tv_sec = (src1).tv_sec - (src2).tv_sec; \
  } \
}
#define IS_AFTER(t1, t2) \
  (((t2).tv_sec > (t1).tv_sec) || \
  (((t2).tv_sec == (t1).tv_sec) && \
  ((t2).tv_usec > (t1).tv_usec)))
#define IS_AT_OR_AFTER(t1, t2) \
  (((t2).tv_sec > (t1).tv_sec) || \
  (((t2).tv_sec == (t1).tv_sec) && \
  ((t2).tv_usec >= (t1).tv_usec)))
#endif
#define GLUT_WIND_IS_RGB(x)         (((x) & GLUT_INDEX) == 0)
#define GLUT_WIND_IS_INDEX(x)       (((x) & GLUT_INDEX) != 0)
#define GLUT_WIND_IS_SINGLE(x)      (((x) & GLUT_DOUBLE) == 0)
#define GLUT_WIND_IS_DOUBLE(x)      (((x) & GLUT_DOUBLE) != 0)
#define GLUT_WIND_HAS_ACCUM(x)      (((x) & GLUT_ACCUM) != 0)
#define GLUT_WIND_HAS_ALPHA(x)      (((x) & GLUT_ALPHA) != 0)
#define GLUT_WIND_HAS_DEPTH(x)      (((x) & GLUT_DEPTH) != 0)
#define GLUT_WIND_HAS_STENCIL(x)    (((x) & GLUT_STENCIL) != 0)
#define GLUT_WIND_IS_MULTISAMPLE(x) (((x) & GLUT_MULTISAMPLE) != 0)
#define GLUT_WIND_IS_STEREO(x)      (((x) & GLUT_STEREO) != 0)
#define GLUT_WIND_IS_LUMINANCE(x)   (((x) & GLUT_LUMINANCE) != 0)
#define GLUT_MAP_WORK               (1 << 0)
#define GLUT_EVENT_MASK_WORK        (1 << 1)
#define GLUT_REDISPLAY_WORK         (1 << 2)
#define GLUT_CONFIGURE_WORK         (1 << 3)
#define GLUT_COLORMAP_WORK          (1 << 4)
#define GLUT_DEVICE_MASK_WORK	    (1 << 5)
#define GLUT_FINISH_WORK	    (1 << 6)
#define GLUT_DEBUG_WORK		    (1 << 7)
#define GLUT_DUMMY_WORK		    (1 << 8)
#define GLUT_FULL_SCREEN_WORK       (1 << 9)
#define GLUT_OVERLAY_REDISPLAY_WORK (1 << 10)
/* GLUT callback function types */
typedef void (*GLUTdisplayCB) (void);
typedef void (*GLUTreshapeCB) (int, int);
typedef void (*GLUTkeyboardCB) (unsigned char, int, int);
typedef void (*GLUTmouseCB) (int, int, int, int);
typedef void (*GLUTmotionCB) (int, int);
typedef void (*GLUTpassiveCB) (int, int);
typedef void (*GLUTentryCB) (int);
typedef void (*GLUTvisibilityCB) (int);
typedef void (*GLUTidleCB) (void);
typedef void (*GLUTtimerCB) (int);
typedef void (*GLUTmenuStateCB) (int);  /* DEPRICATED. */
typedef void (*GLUTmenuStatusCB) (int, int, int);
typedef void (*GLUTselectCB) (int);
typedef void (*GLUTspecialCB) (int, int, int);
typedef void (*GLUTspaceMotionCB) (int, int, int);
typedef void (*GLUTspaceRotateCB) (int, int, int);
typedef void (*GLUTspaceButtonCB) (int, int);
typedef void (*GLUTdialsCB) (int, int);
typedef void (*GLUTbuttonBoxCB) (int, int);
typedef void (*GLUTtabletMotionCB) (int, int);
typedef void (*GLUTtabletButtonCB) (int, int, int, int);
#ifdef SUPPORT_FORTRAN
typedef void (*GLUTdisplayFCB) (void);
typedef void (*GLUTreshapeFCB) (int *, int *);
/* NOTE the pressed key is int, not unsigned char for Fortran! */
typedef void (*GLUTkeyboardFCB) (int *, int *, int *);
typedef void (*GLUTmouseFCB) (int *, int *, int *, int *);
typedef void (*GLUTmotionFCB) (int *, int *);
typedef void (*GLUTpassiveFCB) (int *, int *);
typedef void (*GLUTentryFCB) (int *);
typedef void (*GLUTvisibilityFCB) (int *);
typedef void (*GLUTidleFCB) (void);
typedef void (*GLUTtimerFCB) (int *);
typedef void (*GLUTmenuStateFCB) (int *);  /* DEPRICATED. */
typedef void (*GLUTmenuStatusFCB) (int *, int *, int *);
typedef void (*GLUTselectFCB) (int *);
typedef void (*GLUTspecialFCB) (int *, int *, int *);
typedef void (*GLUTspaceMotionFCB) (int *, int *, int *);
typedef void (*GLUTspaceRotateFCB) (int *, int *, int *);
typedef void (*GLUTspaceButtonFCB) (int *, int *);
typedef void (*GLUTdialsFCB) (int *, int *);
typedef void (*GLUTbuttonBoxFCB) (int *, int *);
typedef void (*GLUTtabletMotionFCB) (int *, int *);
typedef void (*GLUTtabletButtonFCB) (int *, int *, int *, int *);
#endif
typedef struct _GLUTcolorcell GLUTcolorcell;
struct _GLUTcolorcell {
  /* GLUT_RED, GLUT_GREEN, GLUT_BLUE */
  GLfloat component[3];
};
typedef struct _GLUTcolormap GLUTcolormap;
struct _GLUTcolormap {
  Visual *visual;       /* visual of the colormap */
  Colormap cmap;        /* X colormap ID */
  int refcnt;           /* number of windows using colormap */
  int size;             /* number of cells in colormap */
  int transparent;      /* transparent pixel, or -1 if opaque */
  GLUTcolorcell *cells; /* array of cells */
  GLUTcolormap *next;   /* next colormap in list */
};
typedef struct _GLUTwindow GLUTwindow;
typedef struct _GLUToverlay GLUToverlay;
struct _GLUTwindow {
  int num;              /* small integer window id (0-based) */
  /* Window system related state. */
  Window win;           /* X window for GLUT window */
  GLXContext ctx;       /* OpenGL context GLUT glut window */
  XVisualInfo *vis;     /* visual for window */
  Colormap cmap;        /* RGB colormap for window; None if CI */
  GLUTcolormap *colormap;  /* colormap; NULL if RGBA */
  GLUToverlay *overlay; /* overlay; NULL if no overlay */
  Window renderWin;     /* X window for rendering (might be
                           overlay) */
  GLXContext renderCtx; /* OpenGL context for rendering (might
                           be overlay) */
  /* GLUT settable or visible window state. */
  int width;            /* window width in pixels */
  int height;           /* window height in pixels */
  int cursor;           /* cursor name */
  int visState;         /* visibility state (-1 is unknown) */
  int shownState;       /* if window mapped */
  int entryState;       /* entry state (-1 is unknown) */
  int damaged;          /* is layer damaged by expose? */
#define GLUT_MAX_MENUS              3

  int menu[GLUT_MAX_MENUS];  /* attatched menu nums */
  /* Window relationship state. */
  GLUTwindow *parent;   /* parent window */
  GLUTwindow *children; /* list of children */
  GLUTwindow *siblings; /* list of siblings */
  /* Misc. non-API visible (hidden) state. */
  Bool fakeSingle;      /* faking single buffer with double */
  Bool forceReshape;    /* force reshape before display */
  Bool isDirect;        /* if direct context */
  long eventMask;       /* mask of X events selected for */
  int buttonUses;       /* number of button uses, ref cnt */
  int tabletPos[2];     /* tablet position (-1 is invalid) */
  /* Work list related state. */
  unsigned int workMask;  /* mask of window work to be done */
  GLUTwindow *prevWorkWin;  /* link list of windows to work on */
  Bool desiredMapState; /* how to mapped window if on map work
                           list */
  int desiredConfMask;  /* mask of desired window configuration 
                         */
  int desiredX;         /* desired X location */
  int desiredY;         /* desired Y location */
  int desiredWidth;     /* desired window width */
  int desiredHeight;    /* desired window height */
  int desiredStack;     /* desired window stack */
  /* Callbacks */
  GLUTdisplayCB display;  /* redraw callback */
  GLUTreshapeCB reshape;  /* resize callback (width,height) */
  GLUTmouseCB mouse;    /* mouse callback (button,state,x,y) */
  GLUTmotionCB motion;  /* motion callback (x,y) */
  GLUTpassiveCB passive;  /* passive motion callback (x,y) */
  GLUTentryCB entry;    /* window entry/exit callback (state) */
  GLUTkeyboardCB keyboard;  /* keyboard callback (ASCII,x,y) */
  GLUTvisibilityCB visibility;  /* visibility callback */
  GLUTspecialCB special;  /* special key callback */
  GLUTbuttonBoxCB buttonBox;  /* button box callback */
  GLUTdialsCB dials;    /* dials callback */
  GLUTspaceMotionCB spaceMotion;  /* Spaceball motion callback */
  GLUTspaceRotateCB spaceRotate;  /* Spaceball rotate callback */
  GLUTspaceButtonCB spaceButton;  /* Spaceball button callback */
  GLUTtabletMotionCB tabletMotion;  /* tablet motion callback */
  GLUTtabletButtonCB tabletButton;  /* tablet button callback */
#ifdef SUPPORT_FORTRAN
  /* Special Fortran display callback unneeded since no
     parameters! */
  GLUTreshapeFCB freshape;  /* Fortran reshape callback */
  GLUTmouseFCB fmouse;  /* Fortran mouse callback */
  GLUTmotionFCB fmotion;  /* Fortran motion callback */
  GLUTpassiveFCB fpassive;  /* Fortran passive callback */
  GLUTentryFCB fentry;  /* Fortran entry callback */
  GLUTkeyboardFCB fkeyboard;  /* Fortran keyboard callback */
  GLUTvisibilityFCB fvisibility;  /* Fortran visibility
                                     callback */
  GLUTspecialFCB fspecial;  /* special key callback */
  GLUTbuttonBoxFCB fbuttonBox;  /* button box callback */
  GLUTdialsFCB fdials;  /* dials callback */
  GLUTspaceMotionFCB fspaceMotion;  /* Spaceball motion
                                       callback */
  GLUTspaceRotateFCB fspaceRotate;  /* Spaceball rotate
                                       callback */
  GLUTspaceButtonFCB fspaceButton;  /* Spaceball button
                                       callback */
  GLUTtabletMotionFCB ftabletMotion;  /* tablet motion callback 

                                       */
  GLUTtabletButtonFCB ftabletButton;  /* tablet button callback 

                                       */
#endif
};
struct _GLUToverlay {
  Window win;
  GLXContext ctx;
  XVisualInfo *vis;     /* visual for window */
  Colormap cmap;        /* RGB colormap for window; None if CI */
  GLUTcolormap *colormap;  /* colormap; NULL if RGBA */
  int shownState;       /* if overlay window mapped */
  Bool fakeSingle;      /* faking single buffer with double */
  Bool isDirect;        /* if direct context */
  int damaged;          /* is layer damaged by expose? */
  int transparentPixel; /* transparent pixel value */
  GLUTdisplayCB display;  /* redraw callback */
  /* Special Fortran display callback unneeded since no
     parameters! */
};
typedef struct _GLUTstale GLUTstale;
struct _GLUTstale {
  GLUTwindow *window;
  Window win;
  GLUTstale *next;
};

extern GLUTstale *__glutStaleWindowList;
#define GLUT_OVERLAY_EVENT_FILTER_MASK \
  (ExposureMask | \
  StructureNotifyMask | \
  EnterWindowMask | \
  LeaveWindowMask)
#define GLUT_DONT_PROPAGATE_FILTER_MASK \
  (ButtonReleaseMask | \
  ButtonPressMask | \
  KeyPressMask | \
  KeyReleaseMask | \
  PointerMotionMask | \
  Button1MotionMask | \
  Button2MotionMask | \
  Button3MotionMask)
#define GLUT_HACK_STOP_PROPAGATE_MASK \
  (KeyPressMask | \
  KeyReleaseMask)
typedef struct _GLUTmenu GLUTmenu;
typedef struct _GLUTmenuItem GLUTmenuItem;
struct _GLUTmenu {
  int id;               /* small integer menu id (0-based) */
  Window win;           /* X window for the menu */
  GLUTselectCB select;  /* callback function of menu */
  GLUTmenuItem *list;   /* list of menu entries */
  int num;              /* number of entries */
  Bool managed;         /* are the InputOnly windows size
                           validated? */
  int pixwidth;         /* width of menu in pixels */
  int pixheight;        /* height of menu in pixels */
  int submenus;         /* number of submenu entries */
  GLUTmenuItem *highlighted;  /* pointer to highlighted menu
                                 entry, NULL not highlighted */
  GLUTmenu *cascade;    /* currently cascading this menu  */
  GLUTmenuItem *anchor; /* currently anchored to this entry */
  int x;                /* current x origin relative to the
                           root window */
  int y;                /* current y origin relative to the
                           root window */
#ifdef SUPPORT_FORTRAN
  GLUTselectFCB fselect;  /* callback function of menu */
#endif
};
struct _GLUTmenuItem {
  Window win;           /* InputOnly X window for entry */
  GLUTmenu *menu;       /* menu entry belongs to */
  Bool isTrigger;       /* is a submenu trigger? */
  int value;            /* value to return for selecting this
                           entry; doubles as submenu id
                           (0-base) if submenu trigger */
  char *label;          /* strdup'ed label string */
  int len;              /* length of label string */
  int pixwidth;         /* width of X window in pixels */
  GLUTmenuItem *next;   /* next menu entry on list for menu */
};
typedef struct _GLUTtimer GLUTtimer;
struct _GLUTtimer {
  GLUTtimer *next;      /* list of timers */
  struct timeval timeout;  /* time to be called */
  GLUTtimerCB func;     /* timer callback (value) */
  int value;            /* callback return value */
#ifdef SUPPORT_FORTRAN
  GLUTtimerFCB ffunc;   /* Fortran timer callback */
#endif
};
typedef struct _GLUTeventParser GLUTeventParser;
struct _GLUTeventParser {
  int (*func) (XEvent *);
  GLUTeventParser *next;
};
/* Declarations to implement glutFullScreen support with
   mwm/4Dwm. */

/* The following X property format is defined in Motif 1.1's
   Xm/MwmUtils.h, but GLUT should not depend on that header
   file. Note: Motif 1.2 expanded this structure with
   uninteresting fields (to GLUT) so just stick with the
   smaller Motif 1.1 structure. */
typedef struct {
#define MWM_HINTS_DECORATIONS   2
  long flags;
  long functions;
  long decorations;
  long input_mode;
} MotifWmHints;
/* private variables from glut_event.c */
extern GLUTwindow *__glutWindowWorkList;
extern int __glutWindowDamaged;
#ifdef SUPPORT_FORTRAN
extern GLUTtimer *__glutTimerList;
extern GLUTtimer *__glutNewTimer;
#endif

/* private variables from glut_init.c */
extern Atom __glutWMDeleteWindow;
extern Display *__glutDisplay;
extern unsigned int __glutDisplayMode;
extern GLboolean __glutDebug;
extern GLboolean __glutForceDirect;
extern GLboolean __glutIconic;
extern GLboolean __glutTryDirect;
extern Window __glutRoot;
extern XSizeHints __glutSizeHints;
extern char **__glutArgv;
extern char *__glutProgramName;
extern int __glutArgc;
extern int __glutConnectionFD;
extern int __glutInitHeight;
extern int __glutInitWidth;
extern int __glutInitX;
extern int __glutInitY;
extern int __glutScreen;
extern int __glutScreenHeight;
extern int __glutScreenWidth;
extern Atom __glutMotifHints;
extern unsigned int __glutModifierMask;

/* private variables from glut_menu.c */
extern GLUTmenu *__glutCurrentMenu;
extern GLUTmenuItem *__glutItemSelected;
extern GLUTmenu *__glutMappedMenu;
extern GLUTwindow *__glutMenuWindow;
extern void (*__glutMenuStatusFunc) (int, int, int);

/* private variables from glut_win.c */
extern GLUTwindow **__glutWindowList;
extern GLUTwindow *__glutCurrentWindow;
extern int __glutWindowListSize;
extern void (*__glutFreeOverlayFunc) (GLUToverlay *);

/* private routines from glut_cindex.c */
extern GLUTcolormap *__glutAssociateColormap(XVisualInfo * vis);
extern void __glutFreeColormap(GLUTcolormap *);

/* private routines from glut_event.c */
extern void (*__glutUpdateInputDeviceMaskFunc) (GLUTwindow *);
extern void __glutPutOnWorkList(GLUTwindow * window,
  int work_mask);
extern void __glutRegisterEventParser(GLUTeventParser * parser);
extern void __glutPostRedisplay(GLUTwindow * window, int layerMask);

/* private routines from glut_init.c */
extern void __glutOpenXConnection(char *display);
extern void __glutInitTime(struct timeval *beginning);

/* private routines for glut_menu.c */
extern GLUTmenu *__glutGetMenu(Window win);
extern GLUTmenu *__glutGetMenuByNum(int menunum);
extern GLUTmenuItem *__glutGetMenuItem(GLUTmenu * menu,
  Window win, int *which);
extern void __glutFinishMenu(Window win, int x, int y);
extern void __glutMenuItemEnterOrLeave(GLUTmenuItem * item,
  int num, int type);
extern void __glutPaintMenu(GLUTmenu * menu);
extern void __glutSetMenu(GLUTmenu * menu);
extern void __glutStartMenu(GLUTmenu * menu,
  GLUTwindow * window, int x, int y, int x_win, int y_win);

/* private routines from glut_util.c */
extern void __glutWarning(char *format,...);
extern void __glutFatalError(char *format,...);
extern void __glutFatalUsage(char *format,...);

/* private routines from glut_win.c */
extern GLUTwindow *__glutGetWindow(Window win);
extern GLUTwindow *__glutToplevelOf(GLUTwindow * window);
extern void __glutChangeWindowEventMask(long mask, Bool add);
extern void __glutEstablishColormapsProperty(
  GLUTwindow * window);
extern XVisualInfo *__glutDetermineVisual(
  unsigned int mode,
  Bool * fakeSingle,
  XVisualInfo * (getVisualInfo) (unsigned int));
extern XVisualInfo *__glutGetVisualInfo(unsigned int mode);
extern void __glutSetWindow(GLUTwindow * window);
extern void __glutReshapeFunc(GLUTreshapeCB reshapeFunc,
  int callingConvention);
extern void __glutDefaultReshape(int, int);
extern void __glutSetupColormap(
  XVisualInfo * vi,
  GLUTcolormap ** colormap,
  Colormap * cmap);

/* private routines from glut_ext.c */
extern int __glutIsSupportedByGLX(char *);

/* private routines from glut_input.c */
extern void __glutUpdateInputDeviceMask(GLUTwindow * window);

#endif /* __glutint_h__ */
