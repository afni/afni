
/* Copyright (c) Mark J. Kilgard, 1994. */

/* This program is freely distributable without licensing fees
   and is provided without guarantee or warrantee expressed or
   implied. This program is -not- in the public domain. */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <X11/Xlib.h>

#include <GL/glut.h>
#include "glutint.h"

/* GLUT inter-file variables */
char *__glutProgramName = NULL;
int __glutArgc = 0;
char **__glutArgv = NULL;
char *__glutGeometry = NULL;
Display *__glutDisplay = NULL;
int __glutScreen;
Window __glutRoot;
int __glutScreenHeight;
int __glutScreenWidth;
GLboolean __glutIconic = GL_FALSE;
GLboolean __glutDebug = GL_FALSE;
unsigned int __glutDisplayMode =
  GLUT_RGB | GLUT_SINGLE | GLUT_DEPTH;
int __glutConnectionFD;
XSizeHints __glutSizeHints = {0};
int __glutInitWidth = 300, __glutInitHeight = 300;
int __glutInitX = -1, __glutInitY = -1;
GLboolean __glutForceDirect = GL_FALSE,
  __glutTryDirect = GL_TRUE;
Atom __glutWMDeleteWindow;

static Bool synchronize = False;

#if defined(__vms)
char *
strdup(const char *string)
{
  char *new;

  new = malloc(strlen(string) + 1);
  if (new == NULL)
    return NULL;
  strcpy(new, string);
  return new;
}
#endif

void
__glutOpenXConnection(char *display)
{
  int errorBase, eventBase;

  __glutDisplay = XOpenDisplay(display);
  if (!__glutDisplay)
    __glutFatalError("could not open display: %s",
      XDisplayName(display));
  if (synchronize)
    XSynchronize(__glutDisplay, True);
  if (!glXQueryExtension(__glutDisplay, &errorBase, &eventBase))
    __glutFatalError(
      "OpenGL GLX extension not supported by display: %s",
      XDisplayName(display));
  __glutScreen = DefaultScreen(__glutDisplay);
  __glutRoot = RootWindow(__glutDisplay, __glutScreen);
  __glutScreenWidth = DisplayWidth(__glutDisplay, __glutScreen);
  __glutScreenHeight = DisplayHeight(__glutDisplay,
    __glutScreen);
  __glutConnectionFD = ConnectionNumber(__glutDisplay);
  __glutWMDeleteWindow = XInternAtom(__glutDisplay,
    "WM_DELETE_WINDOW", False);
}

void
__glutInitTime(struct timeval *beginning)
{
  static int beenhere = 0;
  static struct timeval genesis;

  if (!beenhere) {
    GETTIMEOFDAY(&genesis);
    beenhere = 1;
  }
  *beginning = genesis;
}

static void
removeArgs(int *argcp, char **argv, int numToRemove)
{
  int i, j;

  for (i = 0, j = numToRemove; argv[j]; i++, j++) {
    argv[i] = argv[j];
  }
  argv[i] = NULL;
  *argcp -= numToRemove;
}

void
glutInit(int *argcp, char **argv)
{
  char *display = NULL;
  char *str;
  struct timeval unused;
  int i;

  if (__glutDisplay) {
    __glutWarning("glutInit being called a second time.");
    return;
  }
  /* determine temporary program name */
  str = strrchr(argv[0], '/');
  if (str == NULL) {
    __glutProgramName = argv[0];
  } else {
    __glutProgramName = str + 1;
  }

  /* make private copy of command line arguments */
  __glutArgc = *argcp;
  __glutArgv = (char **) malloc(__glutArgc * sizeof(char *));
  if (!__glutArgv)
    __glutFatalError("out of memory.");
  for (i = 0; i < __glutArgc; i++) {
    __glutArgv[i] = strdup(argv[i]);
    if (!__glutArgv[i])
      __glutFatalError("out of memory.");
  }

  /* determine permanent program name */
  str = strrchr(__glutArgv[0], '/');
  if (str == NULL) {
    __glutProgramName = __glutArgv[0];
  } else {
    __glutProgramName = str + 1;
  }

  /* parse arguments for standard options */
  for (i = 1; i < __glutArgc; i++) {
    if (!strcmp(__glutArgv[i], "-display")) {
      if (++i >= __glutArgc) {
        __glutFatalError(
          "follow -display option with X display name.");
      }
      display = __glutArgv[i];
      removeArgs(argcp, &argv[1], 2);
    } else if (!strcmp(__glutArgv[i], "-geometry")) {
      int flags, x, y, width, height;

      if (++i >= __glutArgc) {
        __glutFatalError(
          "follow -geometry option with geometry parameter.");
      }
      /* Fix bogus "{width|height} may be used before set"
         warning */
      width = 0;
      height = 0;

      flags = XParseGeometry(__glutArgv[i], &x, &y,
        (unsigned int *) &width, (unsigned int *) &height);
      if (WidthValue & flags) {
        /* Careful because X does not allow zero or negative
           width windows */
        if (width > 0)
          __glutInitWidth = width;
      }
      if (HeightValue & flags) {
        /* Careful because X does not allow zero or negative
           height windows */
        if (height > 0)
          __glutInitHeight = height;
      }
      glutInitWindowSize(__glutInitWidth, __glutInitHeight);
      if (XValue & flags) {
        if (XNegative & flags)
          x = DisplayWidth(__glutDisplay, __glutScreen) +
            x - __glutSizeHints.width;
        /* Play safe: reject negative X locations */
        if (x >= 0)
          __glutInitX = x;
      }
      if (YValue & flags) {
        if (YNegative & flags)
          y = DisplayHeight(__glutDisplay, __glutScreen) +
            y - __glutSizeHints.height;
        /* Play safe: reject negative Y locations */
        if (y >= 0)
          __glutInitY = y;
      }
      glutInitWindowPosition(__glutInitX, __glutInitY);
      removeArgs(argcp, &argv[1], 2);
    } else if (!strcmp(__glutArgv[i], "-direct")) {
      if (!__glutTryDirect)
        __glutFatalError(
          "cannot force both direct and indirect rendering.");
      __glutForceDirect = GL_TRUE;
      removeArgs(argcp, &argv[1], 1);
    } else if (!strcmp(__glutArgv[i], "-indirect")) {
      if (__glutForceDirect)
        __glutFatalError(
          "cannot force both direct and indirect rendering.");
      __glutTryDirect = GL_FALSE;
      removeArgs(argcp, &argv[1], 1);
    } else if (!strcmp(__glutArgv[i], "-iconic")) {
      __glutIconic = GL_TRUE;
      removeArgs(argcp, &argv[1], 1);
    } else if (!strcmp(__glutArgv[i], "-gldebug")) {
      __glutDebug = GL_TRUE;
      removeArgs(argcp, &argv[1], 1);
    } else if (!strcmp(__glutArgv[i], "-sync")) {
      synchronize = GL_TRUE;
      removeArgs(argcp, &argv[1], 1);
    } else {
      /* once unknown option encountered, stop command line
         processing */
      break;
    }
  }
  __glutOpenXConnection(display);
  __glutInitTime(&unused);
}

void
glutInitWindowPosition(int x, int y)
{
  __glutInitX = x;
  __glutInitY = y;
  if (x >= 0 && y >= 0) {
    __glutSizeHints.x = x;
    __glutSizeHints.y = y;
    __glutSizeHints.flags |= USPosition;
  } else {
    __glutSizeHints.flags &= ~USPosition;
  }
}

void
glutInitWindowSize(int width, int height)
{
  __glutInitWidth = width;
  __glutInitHeight = height;
  if (width > 0 && height > 0) {
    __glutSizeHints.width = width;
    __glutSizeHints.height = height;
    __glutSizeHints.flags |= USSize;
  } else {
    __glutSizeHints.flags &= ~USSize;
  }
}

void
glutInitDisplayMode(unsigned int mask)
{
  __glutDisplayMode = mask;
}

