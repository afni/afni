
/* Copyright (c) Mark J. Kilgard, 1995. */

/* This program is freely distributable without licensing fees 
   and is provided without guarantee or warrantee expressed or 
   implied. This program is -not- in the public domain. */

#include <X11/Xlib.h>

#include "glutint.h"

void
glutFullScreen(void)
{
  if (__glutMotifHints == None) {
    __glutMotifHints = XInternAtom(__glutDisplay, "_MOTIF_WM_HINTS", 0);
    if (__glutMotifHints == None) {
      __glutWarning("Could not intern X atom for WM_COLORMAP_WINDOWS.");
    }
  }
  __glutCurrentWindow->desiredX = 0;
  __glutCurrentWindow->desiredY = 0;
  __glutCurrentWindow->desiredWidth = __glutScreenWidth;
  __glutCurrentWindow->desiredHeight = __glutScreenHeight;
  __glutCurrentWindow->desiredConfMask |= CWX | CWY | CWWidth | CWHeight;
  __glutPutOnWorkList(__glutCurrentWindow,
    GLUT_CONFIGURE_WORK | GLUT_FULL_SCREEN_WORK);
}

