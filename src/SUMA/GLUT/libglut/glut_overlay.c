
/* Copyright (c) Mark J. Kilgard, 1996.  */

/* This program is freely distributable without licensing fees
   and is provided without guarantee or warrantee expressed or
   implied. This program is -not- in the public domain. */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* for XA_RGB_DEFAULT_MAP atom */
#if defined (__vms)
#include <X11/StdCmap.h>  /* for XmuLookupStandardColormap */
#else
#include <X11/Xmu/StdCmap.h>  /* for XmuLookupStandardColormap */
#endif

#include <GL/glut.h>
#include "glutint.h"
#include "layerutil.h"

static int
checkOverlayAcceptability(XVisualInfo * vi, unsigned int mode)
{
  int value;

  /* Must support OpenGL. */
  glXGetConfig(__glutDisplay, vi, GLX_USE_GL, &value);
  if (!value)
    return 1;

  /* Must be color index. */
  glXGetConfig(__glutDisplay, vi, GLX_RGBA, &value);
  if (value)
    return 1;

  /* Must match single/double buffering request. */
  glXGetConfig(__glutDisplay, vi, GLX_DOUBLEBUFFER, &value);
  if (GLUT_WIND_IS_DOUBLE(mode) != (value != 0))
    return 1;

  /* Must match mono/stereo request. */
  glXGetConfig(__glutDisplay, vi, GLX_STEREO, &value);
  if (GLUT_WIND_IS_STEREO(mode) != (value != 0))
    return 1;

  /* Alpha and accumulation buffers incompatible with color
     index. */
  if (GLUT_WIND_HAS_ALPHA(mode) || GLUT_WIND_HAS_ACCUM(mode))
    return 1;

  /* Look for depth buffer if requested. */
  glXGetConfig(__glutDisplay, vi, GLX_DEPTH_SIZE, &value);
  if (GLUT_WIND_HAS_DEPTH(mode) && (value <= 0))
    return 1;

  /* Look for stencil buffer if requested. */
  glXGetConfig(__glutDisplay, vi, GLX_STENCIL_SIZE, &value);
  if (GLUT_WIND_HAS_STENCIL(mode) && (value <= 0))
    return 1;

#if defined(GLX_VERSION_1_1) && defined(GLX_SGIS_multisample)
  /* XXX Multisampled overlay color index??  Pretty unlikely. */
  /* Look for multisampling if requested. */
  if (__glutIsSupportedByGLX("GLX_SGIS_multisample"))
    glXGetConfig(__glutDisplay, vi, GLX_SAMPLES_SGIS, &value);
  else
    value = 0;
  if (GLUT_WIND_IS_MULTISAMPLE(mode) && (value <= 0))
    return 1;
#endif

  return 0;
}

static XVisualInfo *
getOverlayVisualInfoCI(unsigned int mode)
{
  XLayerVisualInfo *vi;
  XLayerVisualInfo template;
  XVisualInfo *goodVisual, *returnVisual;
  int nitems, i, j, bad;

  /* The GLX 1.0 glXChooseVisual is does not permit queries
     based on pixel transparency (and GLX_BUFFER_SIZE uses
     "smallest that meets" its requirement instead of "largest
     that meets" that GLUT wants. So, GLUT implements its own
     visual selection routine for color index overlays. */

  /* Try three overlay layers. */
  for (i = 1; i <= 3; i++) {
    template.vinfo.screen = __glutScreen;
    template.vinfo.class = PseudoColor;
    template.layer = i;
    template.type = TransparentPixel;
    vi = __glutXGetLayerVisualInfo(__glutDisplay,
      VisualTransparentType | VisualScreenMask | VisualClassMask | VisualLayerMask,
      &template, &nitems);
    if (vi) {
      /* Check list for acceptable visual meeting requirements
         of requested display mode. */
      for (j = 0; j < nitems; j++) {
        bad = checkOverlayAcceptability(&vi[j].vinfo, mode);
        if (bad) {
          /* Set vi[j].vinfo.visual to mark it unacceptable. */
          vi[j].vinfo.visual = NULL;
        }
      }

      /* Look through list to find deepest acceptable visual. */
      goodVisual = NULL;
      for (j = 0; j < nitems; j++) {
        if (vi[j].vinfo.visual) {
          if (goodVisual == NULL) {
            goodVisual = &vi[j].vinfo;
          } else {
            if (goodVisual->depth < vi[j].vinfo.depth) {
              goodVisual = &vi[j].vinfo;
            }
          }
        }
      }

      /* If a visual is found, clean up and return the visual. */
      if (goodVisual) {
        returnVisual = (XVisualInfo *) malloc(sizeof(XVisualInfo));
        if (returnVisual) {
          *returnVisual = *goodVisual;
        }
        XFree(vi);
        return returnVisual;
      }
      XFree(vi);
    }
  }
  return NULL;
}

static XVisualInfo *
getOverlayVisualInfoRGB(unsigned int mode)
{

  /* XXX For now, transparent RGBA overlays are not supported
     by GLUT.  RGBA overlays raise difficult questions about
     what the transparent pixel (really color) value should be.

     Color index overlay transparency is "easy" because the
     transparent pixel value does not affect displayable colors
     (except for stealing one color cell) since colors are
     determined by indirection through a colormap, and because
     it is uncommon for arbitrary pixel values in color index to
     be "calculated" (as can occur with a host of RGBA operations
     like lighting, blending, etc) so it is easy to avoid the
     transparent pixel value.

     Since it is typically easy to avoid the transparent pixel
     value in color index mode, if GLUT tells the programmer what
     pixel is transparent, then most program can easily avoid
     generating that pixel value except when they intend
     transparency.  GLUT returns whatever transparent pixel value
     is provided by the system through glutGet(
     GLUT_TRANSPARENT_INDEX).

     Theory versus practice for RGBA overlay transparency: In
     theory, the reasonable thing is enabling overlay transparency 

     when an overlay pixel's destination alpha is 0 because this
     allows overlay transparency to be controlled via alpha and
     all visibile colors are permited, but no hardware I am aware
     of supports this practice (and it requires destination alpha
     which is typically optional and quite uncommon for overlay
     windows!).  In practice, the choice of  transparent pixel
     value is typically "hardwired" into most graphics hardware to 

     a single pixel value.  SGI hardware uses true black (0,0,0)
     without regard for the destination alpha.  This is far from
     ideal because true black (a common color that is easy to
     accidently generate) can not be generated in an RGBA overlay. 

     I am not sure what other vendors do.

     Pragmatically, most of the typical things you want to do in
     the overlays can be done in color index (rubber banding,
     pop-up menus, etc.).  One solution for GLUT would be to
     simply "advertise" what RGB triple (or possibly RGBA
     quadruple or simply A alone) generates transparency.  The
     problem with this approach is that it forces programmers to
     avoid whatever arbitrary color various systems decide is
     transparent.  This is a difficult burden to place on
     programmers that want to portably make use of overlays.

     To actually support transparent RGBA overlays, there are
     really two reaonsable options.  ONE: Simply mandate that true 

     black is the RGBA overlay transparent color (what IRIS GL
     did).  This is nice for programmers since only one option,
     nice for existing SGI hardware, bad for anyone (including
     SGI) who wants to improve upon "true black" RGB transparency. 

     Or TWO: Provide a set of queriable "transparency types" (like 

     "true black" or "alpha == 0" or "true white" or even a
     queriable transparent color).  This is harder for
     programmers, OK for existing SGI hardware, and it leaves open 

     the issue of what other modes are reasonable.

     Option TWO seems the more general approach, but since
     hardware designers will likely only implement a single mode
     (this is a scan out issue where bandwidth is pressing issue),
     codifying multiple speculative approaches nobody may ever
     implement seems silly.  And option ONE fiats a suboptimal
     solution.

     Therefore, I defer any decision of how GLUT should support
     RGBA overlay transparency and leave support for it
     unimplemented. Nobody has been pressing me for RGBA overlay
     transparency (though people have requested color index overlay 

     transparency repeatedly).  Geez, if you read this far you are
     either really bored or maybe actually  interested in this
     topic.  Anyway, if you have ideas (particularly if you plan on 

     implementing a hardware scheme for RGBA overlay transparency), 

     I'd be interested.

     For the record, SGI's expiremental Framebufer Configuration
     experimental GLX extension uses option TWO.  Transparency
     modes for "none" and "RGB" are defined (others could be
     defined later).  What RGB value is the transparent one must be 

     queried. I was hoping GLUT could have something that required
     less work from the programmer to use portably. -mjk */

  __glutWarning("RGBA overlays are not supported by GLUT (for now).");
  return NULL;
}

static XVisualInfo *
getOverlayVisualInfo(unsigned int mode)
{
  /* XXX GLUT_LUMINANCE not implemented for GLUT 3.0. */
  if (GLUT_WIND_IS_LUMINANCE(mode))
    return NULL;

  if (GLUT_WIND_IS_RGB(mode))
    return getOverlayVisualInfoRGB(mode);
  else
    return getOverlayVisualInfoCI(mode);
}

static void
addStaleWindow(GLUTwindow * window, Window win)
{
  GLUTstale *entry;

  entry = (GLUTstale *) malloc(sizeof(GLUTstale));
  if (!entry)
    __glutFatalError("out of memory");
  entry->window = window;
  entry->win = win;
  entry->next = __glutStaleWindowList;
  __glutStaleWindowList = entry;
}

void
__glutFreeOverlay(GLUToverlay * overlay)
{
  XFree(overlay->vis);
  XDestroyWindow(__glutDisplay, overlay->win);
  glXDestroyContext(__glutDisplay, overlay->ctx);
  if (overlay->colormap) {
    /* Only color index overlays have colormap data structure. */
    __glutFreeColormap(overlay->colormap);
  }
  free(overlay);
}

void
glutEstablishOverlay(void)
{
  GLUToverlay *overlay;
  GLUTwindow *window;
  XSetWindowAttributes wa;

  /* Register a routine to free an overlay with glut_win.c;
     this keeps glut_win.c from pulling in all of
     glut_overlay.c when no overlay functionality is used by
     the application. */
  __glutFreeOverlayFunc = __glutFreeOverlay;

  window = __glutCurrentWindow;

  /* Allow for an existant overlay to be re-established perhaps
     if you wanted a different display mode. */
  if (window->overlay) {
    addStaleWindow(window, window->overlay->win);
    __glutFreeOverlay(window->overlay);
  }
  overlay = (GLUToverlay *) malloc(sizeof(GLUToverlay));
  if (!overlay)
    __glutFatalError("out of memory.");

  overlay->vis = __glutDetermineVisual(__glutDisplayMode,
    &overlay->fakeSingle, getOverlayVisualInfo);
  if (!overlay->vis) {
    __glutFatalError("lacks overlay support.");
  }
  overlay->ctx = glXCreateContext(__glutDisplay, overlay->vis,
    None, __glutTryDirect);
  overlay->isDirect = glXIsDirect(__glutDisplay, overlay->ctx);
  if (__glutForceDirect) {
    if (!overlay->isDirect) {
      __glutFatalError("direct rendering not possible.");
    }
  }
  __glutSetupColormap(overlay->vis, &(overlay->colormap), &(overlay->cmap));
  overlay->transparentPixel = __glutGetTransparentPixel(__glutDisplay,
    overlay->vis);

  wa.colormap = overlay->cmap;
  wa.background_pixel = overlay->transparentPixel;
  wa.event_mask = window->eventMask & GLUT_OVERLAY_EVENT_FILTER_MASK;
  wa.border_pixel = 0;
  overlay->win = XCreateWindow(__glutDisplay,
    window->win,
    0, 0, window->width, window->height, 0,
    overlay->vis->depth, InputOutput, overlay->vis->visual,
    CWBackPixel | CWBorderPixel | CWEventMask | CWColormap,
    &wa);
  if (window->children) {
    /* Overlay window must be lowered below any GLUT
       subwindows. */
    XLowerWindow(__glutDisplay, overlay->win);
  }
  XMapWindow(__glutDisplay, overlay->win);
  overlay->shownState = 1;

  overlay->damaged = 0;
  overlay->display = NULL;

  /* Make sure a reshape gets delivered. */
  window->forceReshape = True;

  __glutPutOnWorkList(__glutToplevelOf(window), GLUT_COLORMAP_WORK);

  window->overlay = overlay;
  glutUseLayer(GLUT_OVERLAY);
}

void
glutRemoveOverlay(void)
{
  GLUTwindow *window = __glutCurrentWindow;
  GLUToverlay *overlay = __glutCurrentWindow->overlay;

  if (!window->overlay)
    return;

  /* If using overlay, switch to the normal layer. */
  if (window->renderWin == overlay->win) {
    glutUseLayer(GLUT_NORMAL);
  }
  addStaleWindow(window, overlay->win);
  __glutFreeOverlay(overlay);
  window->overlay = NULL;
  __glutPutOnWorkList(__glutToplevelOf(window), GLUT_COLORMAP_WORK);
}

void
glutUseLayer(GLenum layer)
{
  GLUTwindow *window = __glutCurrentWindow;

  switch (layer) {
  case GLUT_NORMAL:
    window->renderWin = window->win;
    window->renderCtx = window->ctx;
    break;
  case GLUT_OVERLAY:
    window->renderWin = window->overlay->win;
    window->renderCtx = window->overlay->ctx;
    break;
  default:
    __glutWarning("glutUseLayer: unknown layer, %d.", layer);
    break;
  }
  __glutSetWindow(window);
}

void
glutPostOverlayRedisplay(void)
{
  __glutPostRedisplay(__glutCurrentWindow, GLUT_OVERLAY_REDISPLAY_WORK);
}

void
glutOverlayDisplayFunc(GLUTdisplayCB displayFunc)
{
  if (!__glutCurrentWindow->overlay) {
    __glutWarning("glutOverlayDisplayFunc: window has no overlay established");
    return;
  }
  __glutCurrentWindow->overlay->display = displayFunc;
}

void
glutHideOverlay(void)
{
  if (!__glutCurrentWindow->overlay) {
    __glutWarning("glutHideOverlay: window has no overlay established");
    return;
  }
  XUnmapWindow(__glutDisplay, __glutCurrentWindow->overlay->win);
  __glutCurrentWindow->overlay->shownState = 0;
}

void
glutShowOverlay(void)
{
  if (!__glutCurrentWindow->overlay) {
    __glutWarning("glutShowOverlay: window has no overlay established");
    return;
  }
  XMapWindow(__glutDisplay, __glutCurrentWindow->overlay->win);
  __glutCurrentWindow->overlay->shownState = 1;
}

int
glutLayerGet(GLenum param)
{
  switch (param) {
  case GLUT_OVERLAY_POSSIBLE:
    {
      XVisualInfo *vi;
      Bool dummy;

      vi = __glutDetermineVisual(__glutDisplayMode, &dummy, getOverlayVisualInfo);
      if (vi) {
        XFree(vi);
        return 1;
      }
      return 0;
    }
  case GLUT_LAYER_IN_USE:
    return __glutCurrentWindow->renderWin != __glutCurrentWindow->win;
  case GLUT_HAS_OVERLAY:
    return __glutCurrentWindow->overlay != NULL;
  case GLUT_TRANSPARENT_INDEX:
    if (__glutCurrentWindow->overlay) {
      return __glutCurrentWindow->overlay->transparentPixel;
    } else {
      return -1;
    }
  case GLUT_NORMAL_DAMAGED:
    /* __glutWindowDamaged is used so the damage state within
       the window (or overlay belwo) can be cleared before
       calling a display callback so on return, the state does
       not have to be cleared (since upon return from the
       callback the window could be destroyed (or layer
       removed). */
    return __glutCurrentWindow->damaged || __glutWindowDamaged;
  case GLUT_OVERLAY_DAMAGED:
    if (__glutCurrentWindow->overlay) {
      return __glutCurrentWindow->overlay->damaged || __glutWindowDamaged;
    } else {
      return -1;
    }
  default:
    __glutWarning("invalid glutLayerGet param: %d", param);
    return -1;
  }
}
