
/* Copyright (c) Mark J. Kilgard, 1996. */

/* This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. */

/* compile: cc -o glxdino glxdino.c -lGLU -lGL -lXmu -lXext -lX11 -lm */

/* C library header files. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* OpenGL header files. */
#include <GL/gl.h>
#include <GL/glx.h>
#include <GL/glu.h>

/* X header files. */
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xmu/StdCmap.h>
#include <X11/keysym.h>

Display *dpy;
Window win;
GLfloat angle = -150;   /* in degrees */
Bool doubleBuffer = True, iconic = False, keepAspect = False;
int W = 300, H = 300;
XSizeHints sizeHints = {0};
GLdouble bodyWidth = 2.0;
int configuration[] = {GLX_DOUBLEBUFFER, GLX_RGBA, GLX_DEPTH_SIZE, 12,
  GLX_RED_SIZE, 1, GLX_BLUE_SIZE, 1, GLX_GREEN_SIZE, 1, None};

typedef enum {
  RESERVED, BODY_SIDE, BODY_EDGE, BODY_WHOLE, ARM_SIDE, ARM_EDGE, ARM_WHOLE,
  LEG_SIDE, LEG_EDGE, LEG_WHOLE, EYE_SIDE, EYE_EDGE, EYE_WHOLE, DINOSAUR
} displayLists;

void extrudeSolidFromPolygon(GLfloat data[][2], unsigned int dataSize,
  GLdouble thickness, GLuint side, GLuint edge, GLuint whole);
void redraw(void);
void makeDinosaur(void);
void contextInit(void);

void
fatalError(char *message)
{
  fprintf(stderr, "glxdino: %s\n", message);
  exit(1);
}

Colormap
getShareableColormap(XVisualInfo * vi)
{
  Status status;
  XStandardColormap *standardCmaps;
  Colormap cmap;
  int i, numCmaps;

  /* be lazy; using DirectColor too involved for this example */
  if (vi->class != TrueColor)
    fatalError("No support for non-TrueColor visual.");
  /* if no standard colormap but TrueColor, just make an
     unshared one */
  status = XmuLookupStandardColormap(dpy, vi->screen, vi->visualid,
    vi->depth, XA_RGB_DEFAULT_MAP, /* replace */ False, /* retain */ True);
  if (status == 1) {
    status = XGetRGBColormaps(dpy, RootWindow(dpy, vi->screen),
      &standardCmaps, &numCmaps, XA_RGB_DEFAULT_MAP);
    if (status == 1)
      for (i = 0; i < numCmaps; i++)
        if (standardCmaps[i].visualid == vi->visualid) {
          cmap = standardCmaps[i].colormap;
          XFree(standardCmaps);
          return cmap;
        }
  }
  cmap = XCreateColormap(dpy, RootWindow(dpy, vi->screen),
    vi->visual, AllocNone);
  return cmap;
}

void extrudeSolidFromPolygon(GLfloat data[][2], unsigned int dataSize,
  GLdouble thickness, GLuint side, GLuint edge, GLuint whole);
GLfloat body[][2] = { {0, 3}, {1, 1}, {5, 1}, {8, 4}, {10, 4}, {11, 5},
  {11, 11.5}, {13, 12}, {13, 13}, {10, 13.5}, {13, 14}, {13, 15}, {11, 16},
  {8, 16}, {7, 15}, {7, 13}, {8, 12}, {7, 11}, {6, 6}, {4, 3}, {3, 2},
  {1, 2} };
GLfloat arm[][2] = { {8, 10}, {9, 9}, {10, 9}, {13, 8}, {14, 9}, {16, 9},
  {15, 9.5}, {16, 10}, {15, 10}, {15.5, 11}, {14.5, 10}, {14, 11}, {14, 10},
  {13, 9}, {11, 11}, {9, 11} };
GLfloat leg[][2] = { {8, 6}, {8, 4}, {9, 3}, {9, 2}, {8, 1}, {8, 0.5}, {9, 0},
  {12, 0}, {10, 1}, {10, 2}, {12, 4}, {11, 6}, {10, 7}, {9, 7} };
GLfloat eye[][2] = { {8.75, 15}, {9, 14.7}, {9.6, 14.7}, {10.1, 15},
  {9.6, 15.25}, {9, 15.25} };
GLfloat skinColor[] = {0.1, 1.0, 0.1, 1.0};   /* Green */
GLfloat eyeColor[] = {1.0, 0.2, 0.2, 1.0};    /* Red */

void
makeDinosaur(void)
{
  GLfloat bodyWidth = 3.0;

  extrudeSolidFromPolygon(body, sizeof(body), bodyWidth,
    BODY_SIDE, BODY_EDGE, BODY_WHOLE);
  extrudeSolidFromPolygon(arm, sizeof(arm), bodyWidth / 4,
    ARM_SIDE, ARM_EDGE, ARM_WHOLE);
  extrudeSolidFromPolygon(leg, sizeof(leg), bodyWidth / 2,
    LEG_SIDE, LEG_EDGE, LEG_WHOLE);
  extrudeSolidFromPolygon(eye, sizeof(eye), bodyWidth + 0.2,
    EYE_SIDE, EYE_EDGE, EYE_WHOLE);
  glNewList(DINOSAUR, GL_COMPILE);
    glMaterialfv(GL_FRONT, GL_DIFFUSE, skinColor);
    glCallList(BODY_WHOLE);
    glPushMatrix();
      glTranslatef(0.0, 0.0, bodyWidth);
      glCallList(ARM_WHOLE);
      glCallList(LEG_WHOLE);
      glTranslatef(0.0, 0.0, -bodyWidth - bodyWidth / 4);
      glCallList(ARM_WHOLE);
      glTranslatef(0.0, 0.0, -bodyWidth / 4);
      glCallList(LEG_WHOLE);
      glTranslatef(0.0, 0.0, bodyWidth / 2 - 0.1);
      glMaterialfv(GL_FRONT, GL_DIFFUSE, eyeColor);
      glCallList(EYE_WHOLE);
    glPopMatrix();
  glEndList();
}

void
extrudeSolidFromPolygon(GLfloat data[][2], unsigned int dataSize,
  GLdouble thickness, GLuint side, GLuint edge, GLuint whole)
{
  static GLUtriangulatorObj *tobj = NULL;
  GLdouble vertex[3], dx, dy, len;
  int i;
  int count = dataSize / (2 * sizeof(GLfloat));

  if (tobj == NULL) {
    tobj = gluNewTess();  /* create and initialize a GLU
                             polygon tessellation object */
    gluTessCallback(tobj, GLU_BEGIN, glBegin);
    gluTessCallback(tobj, GLU_VERTEX, glVertex2fv);  /* tricky */
    gluTessCallback(tobj, GLU_END, glEnd);
  }
  glNewList(side, GL_COMPILE);
    glShadeModel(GL_SMOOTH);  /* smooth minimizes seeing
                                 tessellation */
    gluBeginPolygon(tobj);
    for (i = 0; i < count; i++) {
      vertex[0] = data[i][0];
      vertex[1] = data[i][1];
      vertex[2] = 0;
      gluTessVertex(tobj, vertex, &data[i]);
    }
    gluEndPolygon(tobj);
  glEndList();
  glNewList(edge, GL_COMPILE);
    glShadeModel(GL_FLAT);  /* flat shade keeps angular hands
                               from being "smoothed" */
    glBegin(GL_QUAD_STRIP);
    for (i = 0; i <= count; i++) {
      /* mod function handles closing the edge */
      glVertex3f(data[i % count][0], data[i % count][1], 0.0);
      glVertex3f(data[i % count][0], data[i % count][1], thickness);
      /* Calculate a unit normal by dividing by Euclidean
         distance.  We could be lazy and use glEnable(GL_NORMALIZE)
	 so we could pass in arbitrary normals for a very slight
	 performance hit. */
      dx = data[(i + 1) % count][1] - data[i % count][1];
      dy = data[i % count][0] - data[(i + 1) % count][0];
      len = sqrt(dx * dx + dy * dy);
      glNormal3f(dx / len, dy / len, 0.0);
    }
    glEnd();
  glEndList();
  glNewList(whole, GL_COMPILE);
    glFrontFace(GL_CW);
    glCallList(edge);
    glNormal3f(0.0, 0.0, -1.0);  /* constant normal for side */
    glCallList(side);
    glPushMatrix();
      glTranslatef(0.0, 0.0, thickness);
      glFrontFace(GL_CCW);
      glNormal3f(0.0, 0.0, 1.0);  /* reverse normal for other side */
      glCallList(side);
    glPopMatrix();
  glEndList();
}

GLfloat lightZeroPosition[] = {10.0, 4.0, 10.0, 1.0};
GLfloat lightZeroColor[] = {0.8, 1.0, 0.8, 1.0}; /* green-tinted */
GLfloat lightOnePosition[] = {-1.0, -2.0, 1.0, 0.0};
GLfloat lightOneColor[] = {0.6, 0.3, 0.2, 1.0};  /* red-tinted */

void
contextInit(void)
{
  glEnable(GL_CULL_FACE);  /* Up to 50% better perfomance than no
                              back-face culling. */
  glEnable(GL_DEPTH_TEST); /* Enable depth buffering. */

  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, 1);
  glLightfv(GL_LIGHT0, GL_POSITION, lightZeroPosition);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, lightZeroColor);
  glLightf(GL_LIGHT0, GL_CONSTANT_ATTENUATION, 0.1);
  glLightf(GL_LIGHT0, GL_LINEAR_ATTENUATION, 0.05);
  glLightfv(GL_LIGHT1, GL_POSITION, lightOnePosition);
  glLightfv(GL_LIGHT1, GL_DIFFUSE, lightOneColor);

  /* Enable both lights. */
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHT1);
  glEnable(GL_LIGHTING);

  /* Set up projection transform. */
  glMatrixMode(GL_PROJECTION);
  gluPerspective(
    40.0,               /* Field of view in degree */
    1.0,                /* Aspect ratio */
    1.0,                /* Z near */
    40.0);              /* Z far */

  /* Now, change to modelview. */
  glMatrixMode(GL_MODELVIEW);
  gluLookAt(
    0.0, 0.0, 30.0,     /* Eye is at (0,0,30) */
    0.0, 0.0, 0.0,      /* Center is at (0,0,0) */
    0.0, 1.0, 0.);      /* Up is in positive Y direction */
  glPushMatrix();       /* Dummy push so we can pop on model
                           recalc. */
}

void
main(int argc, char **argv)
{
  XVisualInfo *vi;
  Colormap cmap;
  XSetWindowAttributes swa;
  XWMHints *wmHints;
  Atom wmDeleteWindow;
  GLXContext cx;
  XEvent event;
  KeySym ks;
  Bool needRedraw = False, recalcModelView = True;
  char *display = NULL, *geometry = NULL;
  int flags, x, y, width, height, lastX, i;

  for (i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "-geometry")) {
      if (++i >= argc)
        fatalError("follow -geometry option with geometry parameter");
      geometry = argv[i];
    } else if (!strcmp(argv[i], "-display")) {
      if (++i >= argc)
        fatalError("follow -display option with display parameter");
      display = argv[i];
    } else if (!strcmp(argv[i], "-iconic"))
      iconic = True;
    else if (!strcmp(argv[i], "-keepaspect"))
      keepAspect = True;
    else if (!strcmp(argv[i], "-single"))
      doubleBuffer = False;
    else
      fatalError("bad option");
  }

  dpy = XOpenDisplay(display);
  if (dpy == NULL)
    fatalError("could not open display");

  if (!glXQueryExtension(dpy, NULL, NULL))
    fatalError("X server has no OpenGL GLX extension");

  /* Find an OpenGL-capable RGB visual with depth buffer. */
  if (!doubleBuffer)
    goto SingleBufferOverride;
  vi = glXChooseVisual(dpy, DefaultScreen(dpy), configuration);
  if (vi == NULL) {
  SingleBufferOverride:
    vi = glXChooseVisual(dpy, DefaultScreen(dpy), &configuration[1]);
    if (vi == NULL)
      fatalError("no appropriate RGB visual with depth buffer");
    doubleBuffer = False;
  }
  cmap = getShareableColormap(vi);

  /* Create an OpenGL rendering context. */
  cx = glXCreateContext(dpy, vi,
    /* No sharing of display lists */ NULL,
    /* Direct rendering if possible */ True);
  if (cx == NULL)
    fatalError("could not create rendering context");

  flags = XParseGeometry(geometry, &x, &y,
    (unsigned int *) &width, (unsigned int *) &height);
  if (WidthValue & flags) {
    sizeHints.flags |= USSize;
    sizeHints.width = width;
    W = width;
  }
  if (HeightValue & flags) {
    sizeHints.flags |= USSize;
    sizeHints.height = height;
    H = height;
  }
  if (XValue & flags) {
    if (XNegative & flags)
      x = DisplayWidth(dpy, DefaultScreen(dpy)) + x - sizeHints.width;
    sizeHints.flags |= USPosition;
    sizeHints.x = x;
  }
  if (YValue & flags) {
    if (YNegative & flags)
      y = DisplayHeight(dpy, DefaultScreen(dpy)) + y - sizeHints.height;
    sizeHints.flags |= USPosition;
    sizeHints.y = y;
  }
  if (keepAspect) {
    sizeHints.flags |= PAspect;
    sizeHints.min_aspect.x = sizeHints.max_aspect.x = W;
    sizeHints.min_aspect.y = sizeHints.max_aspect.y = H;
  }
  swa.colormap = cmap;
  swa.border_pixel = 0;
  swa.event_mask = ExposureMask | StructureNotifyMask |
    ButtonPressMask | Button1MotionMask | KeyPressMask;
  win = XCreateWindow(dpy, RootWindow(dpy, vi->screen),
    sizeHints.x, sizeHints.y, W, H,
    0, vi->depth, InputOutput, vi->visual,
    CWBorderPixel | CWColormap | CWEventMask, &swa);
  XSetStandardProperties(dpy, win, "OpenGLosaurus", "glxdino",
    None, argv, argc, &sizeHints);
  wmHints = XAllocWMHints();
  wmHints->initial_state = iconic ? IconicState : NormalState;
  wmHints->flags = StateHint;
  XSetWMHints(dpy, win, wmHints);
  wmDeleteWindow = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
  XSetWMProtocols(dpy, win, &wmDeleteWindow, 1);
  glXMakeCurrent(dpy, win, cx);
  makeDinosaur();
  contextInit();
  XMapWindow(dpy, win);
  for (;;) {
    do {
      XNextEvent(dpy, &event);
      switch (event.type) {
      case ConfigureNotify:
        glViewport(0, 0,
          event.xconfigure.width, event.xconfigure.height);
        /* fall through... */
      case Expose:
        needRedraw = True;
        break;
      case MotionNotify:
        recalcModelView = True;
        angle -= (lastX - event.xmotion.x);
      case ButtonPress:
        lastX = event.xbutton.x;
        break;
      case KeyPress:
        ks = XLookupKeysym((XKeyEvent *) & event, 0);
        if (ks == XK_Escape)
          exit(0);
        break;
      case ClientMessage:
        if (event.xclient.data.l[0] == wmDeleteWindow)
          exit(0);
        break;
      }
    } while (XPending(dpy));  /* Loop to compress events. */
    if (recalcModelView) {
      glPopMatrix();    /* Pop old rotated matrix (or dummy
                           matrix if first time). */
      glPushMatrix();
      glRotatef(angle, 0.0, 1.0, 0.0);
      glTranslatef(-8, -8, -bodyWidth / 2);
      recalcModelView = False;
      needRedraw = True;
    }
    if (needRedraw) {
      redraw();
      needRedraw = False;
    }
  }
}

void
redraw(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glCallList(DINOSAUR);
  if (doubleBuffer)
    glXSwapBuffers(dpy, win);
  else
    glFlush();
}
