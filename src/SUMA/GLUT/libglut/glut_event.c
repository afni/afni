
/* Copyright (c) Mark J. Kilgard, 1994, 1995, 1996. */

/* This program is freely distributable without licensing fees
   and is provided without guarantee or warrantee expressed or
   implied. This program is -not- in the public domain. */

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <assert.h>
#ifdef __sgi
#include <bstring.h>    /* prototype for bzero used by FD_ZERO */
#endif
#ifdef AIXV3
#include <sys/select.h> /* select system call interface */
#endif
#include <sys/types.h>
#ifndef __vms
#include <sys/time.h>
#endif
#include <X11/Xlib.h>
#include <X11/keysym.h>
#ifdef __hpux
/* XXX Bert Gijsbers <bert@mc.bio.uva.nl> reports that HP-UX
   needs different keysyms for the End, Insert, and Delete keys
   to work on an HP 715.  It would be better if HP generated
   standard keysyms for standard keys. */
#include <X11/HPkeysym.h>
#endif
#ifdef __vms
#include <ssdef.h>
#include <psldef.h>
extern int SYS$CLREF(int efn);
extern int SYS$SETIMR(unsigned int efn, struct timeval *timeout, void *ast,
  unsigned int request_id, unsigned int flags);
extern int SYS$WFLOR(unsigned int efn, unsigned int mask);
extern int SYS$CANTIM(unsigned int request_id, unsigned int mode);
#endif /* __vms */
#include <GL/glut.h>
#include "glutint.h"

static GLUTtimer *freeTimerList = NULL;
static int mappedMenuButton;

GLUTidleCB __glutIdleFunc = NULL;
GLUTtimer *__glutTimerList = NULL;
#ifdef SUPPORT_FORTRAN
GLUTtimer *__glutNewTimer;
#endif
GLUTwindow *__glutWindowWorkList = NULL;
void (*__glutUpdateInputDeviceMaskFunc) (GLUTwindow *);
Atom __glutMotifHints = None;
unsigned int __glutModifierMask = ~0;  /* ~0 implies not in
                                          core input callback. */
int __glutWindowDamaged = 0;

void
glutIdleFunc(GLUTidleCB idleFunc)
{
  __glutIdleFunc = idleFunc;
}

void
glutTimerFunc(unsigned int interval, GLUTtimerCB timerFunc, int value)
{
  GLUTtimer *timer, *other;
  GLUTtimer **prevptr;
  struct timeval now;

  if (!timerFunc)
    return;

  if (freeTimerList) {
    timer = freeTimerList;
    freeTimerList = timer->next;
  } else {
    timer = (GLUTtimer *) malloc(sizeof(GLUTtimer));
    if (!timer)
      __glutFatalError("out of memory.");
  }

  timer->func = timerFunc;
#ifdef __vms
  /* VMS time is expressed in units of 100 ns */
  timer->timeout.val = interval * TICKS_PER_MILLISECOND;
#else
  timer->timeout.tv_sec = (int) interval / 1000;
  timer->timeout.tv_usec = (int) (interval % 1000) * 1000;
#endif
  timer->value = value;
  timer->next = NULL;
  GETTIMEOFDAY(&now);
  ADD_TIME(timer->timeout, timer->timeout, now);
  prevptr = &__glutTimerList;
  other = *prevptr;
  while (other && IS_AFTER(other->timeout, timer->timeout)) {
    prevptr = &other->next;
    other = *prevptr;
  }
  timer->next = other;
#ifdef SUPPORT_FORTRAN
  __glutNewTimer = timer;  /* for Fortran binding! */
#endif
  *prevptr = timer;
}

static void
handleTimeouts(void)
{
  struct timeval now;
  GLUTtimer *timer;

  if (__glutTimerList) {
    GETTIMEOFDAY(&now);
    while (IS_AT_OR_AFTER(__glutTimerList->timeout, now)) {
      timer = __glutTimerList;
      timer->func(timer->value);
      __glutTimerList = timer->next;
      timer->next = freeTimerList;
      freeTimerList = timer;
      if (!__glutTimerList)
        break;
    }
  }
}

void
__glutPutOnWorkList(GLUTwindow * window, int workMask)
{
  if (window->workMask) {
    /* Already on list; just OR in new workMask. */
    window->workMask |= workMask;
  } else {
    /* Update work mask and add to window work list. */
    window->workMask = workMask;
    window->prevWorkWin = __glutWindowWorkList;
    __glutWindowWorkList = window;
  }
}

void
__glutPostRedisplay(GLUTwindow * window, int layerMask)
{
  int shown = (layerMask == GLUT_REDISPLAY_WORK) ? window->shownState : window->overlay->shownState;

  /* Post a redisplay if the window is visible (or the
     visibility of the window is unknown, ie. window->visState
     == -1) _and_ the layer is known to be shown. */
  if (window->visState != 0 && shown)
    __glutPutOnWorkList(window, layerMask);
}

void
glutPostRedisplay(void)
{
  __glutPostRedisplay(__glutCurrentWindow, GLUT_REDISPLAY_WORK);
}

static GLUTeventParser *eventParserList = NULL;

/* __glutRegisterEventParser allows another module to register
   to intercept X events types not otherwise acted on by the
   GLUT processEvents routine.  The X Input extension support
   code uses an event parser for handling X Input extension
   events.  */

void
__glutRegisterEventParser(GLUTeventParser * parser)
{
  parser->next = eventParserList;
  eventParserList = parser;
}

static void
updateWindowVisibility(GLUTwindow * window, int visState)
{
  if (window->shownState && visState != window->visState) {
    if (window->visibility) {
      window->visState = visState;
      __glutSetWindow(window);
      window->visibility(visState ?
        GLUT_VISIBLE : GLUT_NOT_VISIBLE);
    }
    /* An unmap is only reported on a single window; its
       descendents need to know they are no longer visible. */
    if (!visState) {
      GLUTwindow *child = window->children;

      while (child) {
        updateWindowVisibility(child, visState);
        child = child->siblings;
      }
    }
  }
}

static void
purgeStaleWindow(Window win)
{
  GLUTstale **pEntry = &__glutStaleWindowList;
  GLUTstale *entry = __glutStaleWindowList;

  /* Tranverse singly-linked stale window list look for the
     window ID. */
  while (entry) {
    if (entry->win == win) {
      /* Found it; delete it. */
      *pEntry = entry->next;
      free(entry);
      return;
    } else {
      pEntry = &entry->next;
      entry = *pEntry;
    }
  }
}

static void
processEvents(void)
{
  XEvent event, ahead;
  GLUTwindow *window;
  int width, height;
  GLUTeventParser *parser;

  do {
    XNextEvent(__glutDisplay, &event);
    switch (event.type) {
    case MappingNotify:
      XRefreshKeyboardMapping((XMappingEvent *) & event);
      break;
    case ConfigureNotify:
      window = __glutGetWindow(event.xconfigure.window);
      if (window) {
        if (window->win != event.xconfigure.window) {
          /* Ignore ConfigureNotify sent to the overlay planes.
             GLUT could get here because overlays select for
             StructureNotify events to receive DestroyNotify. */
          break;
        }
        width = event.xconfigure.width;
        height = event.xconfigure.height;
        if (width != window->width || height != window->height) {
          if (window->overlay) {
            XResizeWindow(__glutDisplay, window->overlay->win, width, height);
          }
          window->width = width;
          window->height = height;
          __glutSetWindow(window);
          /* Do not execute OpenGL out of sequence with respect
             to the XResizeWindow request! */
          glXWaitX();
          window->reshape(width, height);
          window->forceReshape = False;
        }
      }
      break;
    case Expose:
      /* compress expose events */
      while (XEventsQueued(__glutDisplay, QueuedAfterReading)
        > 0) {
        XPeekEvent(__glutDisplay, &ahead);
        if (ahead.type != Expose ||
          ahead.xexpose.window != event.xexpose.window)
          break;
        XNextEvent(__glutDisplay, &event);
      }
      if (event.xexpose.count == 0) {
        GLUTmenu *menu;

        if (__glutMappedMenu &&
          (menu = __glutGetMenu(event.xexpose.window))) {
          __glutPaintMenu(menu);
        } else {
          window = __glutGetWindow(event.xexpose.window);
          if (window) {
            if (window->win == event.xexpose.window) {
              window->damaged = 1;
              __glutPostRedisplay(window, GLUT_REDISPLAY_WORK);
            } else if (window->overlay && window->overlay->win == event.xexpose.window) {
              __glutPostRedisplay(window, GLUT_OVERLAY_REDISPLAY_WORK);
              window->overlay->damaged = 1;
            }
          }
        }
      } else {
        /* there are more exposes to read; wait to redisplay */
      }
      break;
    case ButtonPress:
    case ButtonRelease:
      if (__glutMappedMenu && event.type == ButtonRelease
        && mappedMenuButton == event.xbutton.button) {
        /* Menu is currently popped up and its button is
           released. */
        __glutFinishMenu(event.xbutton.window, event.xbutton.x, event.xbutton.y);
      } else {
        window = __glutGetWindow(event.xbutton.window);
        if (window) {
          GLUTmenu *menu;

          menu = __glutGetMenuByNum(
            window->menu[event.xbutton.button - 1]);
          if (menu) {
            if (event.type == ButtonPress && !__glutMappedMenu) {
              __glutStartMenu(menu, window,
                event.xbutton.x_root, event.xbutton.y_root,
                event.xbutton.x, event.xbutton.y);
              mappedMenuButton = event.xbutton.button;
            } else {
              /* Ignore a release of a button with a menu
                 attatched to it when no menu is popped up, or
                 ignore a press when another menu is already
                 popped up. */
            }
          } else if (window->mouse) {
            __glutSetWindow(window);
            __glutModifierMask = event.xbutton.state;
            window->mouse(event.xbutton.button - 1,
              event.type == ButtonRelease ?
              GLUT_UP : GLUT_DOWN,
              event.xbutton.x, event.xbutton.y);
            __glutModifierMask = ~0;
          } else {
            /* Stray mouse events.  Ignore. */
          }
        } else {
          /* Window might have been destroyed and all the 
             events for the window may not yet be received. */
        }
      }
      break;
    case MotionNotify:
      if (!__glutMappedMenu) {
        window = __glutGetWindow(event.xmotion.window);
        if (window) {
          /* If motion function registered _and_ buttons held *
             down, call motion function...  */
          if (window->motion && event.xmotion.state &
            (Button1Mask | Button2Mask | Button3Mask)) {
            __glutSetWindow(window);
            window->motion(event.xmotion.x, event.xmotion.y);
          }
          /* If passive motion function registered _and_
             buttons not held down, call passive motion
             function...  */
          else if (window->passive &&
              ((event.xmotion.state &
                  (Button1Mask | Button2Mask | Button3Mask)) ==
              0)) {
            __glutSetWindow(window);
            window->passive(event.xmotion.x,
              event.xmotion.y);
          }
        }
      } else {
        /* Motion events are thrown away when a pop up menu is
           active. */
      }
      break;
    case KeyPress:
      window = __glutGetWindow(event.xkey.window);
      if (!window) {
        break;
      }
      if (window->keyboard) {
        char tmp[1];
        int rc;

        rc = XLookupString(&event.xkey, tmp, sizeof(tmp),
          NULL, NULL);
        if (rc) {
          __glutSetWindow(window);
          __glutModifierMask = event.xkey.state;
          window->keyboard(tmp[0],
            event.xkey.x, event.xkey.y);
          __glutModifierMask = ~0;
          break;
        }
      }
      if (window->special) {
        KeySym ks;
        int key;

        ks = XLookupKeysym((XKeyEvent *) & event, 0);
        /* XXX Verbose, but makes no assumptions about keysym
           layout. */
        switch (ks) {
          /* function keys */
          case XK_F1:    key = GLUT_KEY_F1; break;
          case XK_F2:    key = GLUT_KEY_F2; break;
          case XK_F3:    key = GLUT_KEY_F3; break;
          case XK_F4:    key = GLUT_KEY_F4; break;
          case XK_F5:    key = GLUT_KEY_F5; break;
          case XK_F6:    key = GLUT_KEY_F6; break;
          case XK_F7:    key = GLUT_KEY_F7; break;
          case XK_F8:    key = GLUT_KEY_F8; break;
          case XK_F9:    key = GLUT_KEY_F9; break;
          case XK_F10:   key = GLUT_KEY_F10; break;
          case XK_F11:   key = GLUT_KEY_F11; break;
          case XK_F12:   key = GLUT_KEY_F12; break;
          /* directional keys */
          case XK_Left:  key = GLUT_KEY_LEFT; break;
          case XK_Up:    key = GLUT_KEY_UP; break;
          case XK_Right: key = GLUT_KEY_RIGHT; break;
          case XK_Down:  key = GLUT_KEY_DOWN; break;
        case XK_Prior:
          /* XK_Prior same as X11R6's XK_Page_Up */
          key = GLUT_KEY_PAGE_UP;
          break;
        case XK_Next:
          /* XK_Next same as X11R6's XK_Page_Down */
          key = GLUT_KEY_PAGE_DOWN;
          break;
        case XK_Home:
          key = GLUT_KEY_HOME;
          break;
        case XK_End:
#ifdef __hpux
        case XK_Select:
#endif
          key = GLUT_KEY_END;
          break;
        case XK_Insert:
#ifdef __hpux
        case XK_InsertChar:
#endif
          key = GLUT_KEY_INSERT;
          break;
#ifdef __hpux
        case XK_DeleteChar:
          /* The Delete character is really an ASCII key. */
          __glutSetWindow(window);
          window->keyboard(127, /* ASCII Delete character. */
            event.xkey.x, event.xkey.y);
          goto skip;
#endif
        default:
          goto skip;
        }
        __glutSetWindow(window);
        __glutModifierMask = event.xkey.state;
        window->special(key, event.xkey.x, event.xkey.y);
        __glutModifierMask = ~0;
      skip:;
      }
      break;
    case EnterNotify:
    case LeaveNotify:
      if (event.xcrossing.mode != NotifyNormal ||
        event.xcrossing.detail == NotifyNonlinearVirtual ||
        event.xcrossing.detail == NotifyVirtual) {

        /* Careful to ignore Enter/LeaveNotify events that come
           from the pop-up menu pointer grab and ungrab.  Also,
           ignore "virtual" Enter/LeaveNotify events since they
           represent the pointer passing through the window
           hierarchy without actually entering or leaving the
           actual real estate of a window.  */

        break;
      }
      if (__glutMappedMenu) {
        GLUTmenuItem *item;
        int num;

        item = __glutGetMenuItem(__glutMappedMenu,
          event.xcrossing.window, &num);
        if (item) {
          __glutMenuItemEnterOrLeave(item, num, event.type);
          break;
        }
      }
      window = __glutGetWindow(event.xcrossing.window);
      if (window) {
        if (window->entry) {
          if (event.type == EnterNotify) {

            /* With overlays established, X can report two
               enter events for both the overlay and normal
               plane window. Do not generate a second enter
               callback if we reported one without an
               intervening leave. */

            if (window->entryState != EnterNotify) {
              int num = window->num;
              Window xid = window->win;

              window->entryState = EnterNotify;
              __glutSetWindow(window);
              window->entry(GLUT_ENTERED);

              if (__glutMappedMenu) {

                /* Do not generate any passive motion events
                   when menus are in use. */

              } else {

                /* An EnterNotify event can result in a
                   "compound" callback if a passive motion
                   callback is also registered. In this case,
                   be a little paranoid about the possibility
                   the window could have been destroyed in the
                   entry callback. */

                window = __glutWindowList[num];
                if (window && window->passive && window->win == xid) {
                  __glutSetWindow(window);
                  window->passive(event.xcrossing.x, event.xcrossing.y);
                }
              }
            }
          } else {
            if (window->entryState != LeaveNotify) {

              /* When an overlay is established for a window
                 already mapped and with the pointer in it, the
                 X server will generate a leave/enter event pair
                 as the pointer leaves (without moving) from the
                 normal plane X window to the newly mapped
                 overlay  X window (or vice versa). This
                 enter/leave pair should not be reported to the
                 GLUT program since the pair is a consequence of
                 creating (or destroying) the overlay, not an
                 actual leave from the GLUT window. */

              if (XEventsQueued(__glutDisplay, QueuedAfterReading)) {
                XPeekEvent(__glutDisplay, &ahead);
                if (ahead.type == EnterNotify &&
                  __glutGetWindow(ahead.xcrossing.window) == window) {
                  XNextEvent(__glutDisplay, &event);
                  break;
                }
              }
              window->entryState = LeaveNotify;
              __glutSetWindow(window);
              window->entry(GLUT_LEFT);
            }
          }
        } else if (window->passive) {
          __glutSetWindow(window);
          window->passive(event.xcrossing.x, event.xcrossing.y);
        }
      }
      break;
    case UnmapNotify:
      /* MapNotify events are not needed to maintain visibility
         state since VisibilityNotify events will be delivered
         when a window becomes visible from mapping.  However,
         VisibilityNotify events are not delivered when a window
         is unmapped (for the window or its children). */
      window = __glutGetWindow(event.xunmap.window);
      if (window) {
        if (window->win != event.xconfigure.window) {
          /* Ignore UnmapNotify sent to the overlay planes.
             GLUT could get here because overlays select for
             StructureNotify events to receive DestroyNotify. */
          break;
        }
        updateWindowVisibility(window, 0);
      }
      break;
    case VisibilityNotify:
      window = __glutGetWindow(event.xvisibility.window);
      if (window) {
        int visState = (event.xvisibility.state != VisibilityFullyObscured);

        if (visState != window->visState) {
          if (window->visibility) {
            window->visState = visState;
            __glutSetWindow(window);
            window->visibility(visState ? GLUT_VISIBLE : GLUT_NOT_VISIBLE);
          }
        }
      }
      break;
    case ClientMessage:
      if (event.xclient.data.l[0] == __glutWMDeleteWindow)
        exit(0);
      break;
    case DestroyNotify:
      purgeStaleWindow(event.xdestroywindow.window);
      break;
    case CirculateNotify:
    case CreateNotify:
    case GravityNotify:
    case ReparentNotify:
      /* Uninteresting to GLUT (but possible for GLUT to
         receive). */
      break;
    default:
      /* Pass events not directly handled by the GLUT main
         event loop to any event parsers that have been
         registered.  In this way, X Input extension events are
         passed to the correct handler without forcing all GLUT
         programs to support X Input event handling. */
      parser = eventParserList;
      while (parser) {
        if (parser->func(&event))
          break;
        parser = parser->next;
      }
      break;
    }
  }
  while (XPending(__glutDisplay));
}

static void
waitForSomething(void)
{
#ifdef __vms
  static struct timeval zerotime = {0};
  unsigned int timer_efn;
#define timer_id 'glut'           /* random :-) number */
  unsigned int wait_mask;
#else
  static struct timeval zerotime = {0, 0};
  fd_set fds;
#endif
  struct timeval now, timeout, waittime;
  int rc;

  /* flush X protocol since XPending does not do this
     implicitly */
  XFlush(__glutDisplay);
  if (XPending(__glutDisplay)) {
    /* It is possible (but quite rare) that XFlush may have
       needed to wait for a writable X connection file
       descriptor, and in the process, may have had to read off
       X protocol from the file descriptor. If XPending is true,
       this case occured and we should avoid waiting in select
       since X protocol buffered within Xlib is due to be
       processed and potentially no more X protocol is on the
       file descriptor, so we would risk waiting improperly in
       select. */
    goto immediatelyHandleXinput;
  }
#ifdef __vms
  timeout = __glutTimerList->timeout;
  GETTIMEOFDAY(&now);
  wait_mask = 1 << (__glutConnectionFD & 31);
  if (IS_AFTER(now, timeout)) {
    /* We need an event flag for the timer. */
    /* XXX The `right' way to do this is to use LIB$GET_EF, but since it needs
       to be in the same cluster as the EFN for the display, we will have
       hack it. */
    timer_efn = __glutConnectionFD - 1;
    if ((timer_efn / 32) != (__glutConnectionFD / 32) ) {
      timer_efn = __glutConnectionFD + 1;
    }
    rc = SYS$CLREF (timer_efn);
    rc = SYS$SETIMR (timer_efn, &timeout, NULL, timer_id, 0);
    wait_mask |= 1 << (timer_efn & 31);
  } else {
    timer_efn = 0;
  }
  rc = SYS$WFLOR (__glutConnectionFD, wait_mask);
  if (timer_efn != 0 && SYS$CLREF (timer_efn) == SS$_WASCLR) {
    rc = SYS$CANTIM (timer_id, PSL$C_USER);
  }
  /* XXX There does not seem to be checking of "rc" in the code
     above.  Can any of the SYS$ routines above fail? */
#else /* not vms */
  FD_ZERO(&fds);
  FD_SET(__glutConnectionFD, &fds);
  timeout = __glutTimerList->timeout;
  GETTIMEOFDAY(&now);
  if (IS_AFTER(now, timeout)) {
    TIMEDELTA(waittime, timeout, now);
  } else {
    waittime = zerotime;
  }
  rc = select(__glutConnectionFD + 1, &fds,
    NULL, NULL, &waittime);
  if (rc < 0 && errno != EINTR)
    __glutFatalError("select error.");
#endif /* not vms */
  /* Without considering the cause of select unblocking, check
     for pending X events *and* then handle any timeouts. We
     always look for X events even if select returned with 0
     (indicating a timeout); otherwise we risk starving X event
     processing by continous timeouts. */
  while (XPending(__glutDisplay)) {
  immediatelyHandleXinput:
    processEvents();
  }
  handleTimeouts();
}

static void
idleWait(void)
{
  while (XPending(__glutDisplay)) {
    processEvents();
  }
  if (__glutTimerList)
    handleTimeouts();
  /* Make sure idle func still exists! */
  if (__glutIdleFunc)
    __glutIdleFunc();
}

static GLUTwindow **beforeEnd;

static GLUTwindow *
processWindowWorkList(GLUTwindow * window)
{
  int workMask;

  if (window->prevWorkWin)
    window->prevWorkWin = processWindowWorkList(window->prevWorkWin);
  else
    beforeEnd = &window->prevWorkWin;

  /* Capture work mask for work that needs to be done to this
     window, then clear the window's work mask (excepting the
     dummy work bit, see below).  Then, process the captured
     work mask.  This allows callbacks in the processing the
     captured work mask to set the window's work mask for
     subsequent processing. */

  workMask = window->workMask;
  assert((workMask & GLUT_DUMMY_WORK) == 0);

  /* Set the dummy work bit, clearing all other bits, to
     indicate that the window is currently on the window work
     list _and_ that the window's work mask is currently being
     processed.  This convinces __glutPutOnWorkList that this
     window is on the work list still. */
  window->workMask = GLUT_DUMMY_WORK;

  /* Optimization: most of the time, the work to do is a
     redisplay and not these other types of work.  Check for
     the following cases as a group to before checking each one 
     individually one by one. This saves about 25 MIPS
     instructions in the common redisplay only case. */
  if (workMask & (GLUT_EVENT_MASK_WORK | GLUT_DEVICE_MASK_WORK |
      GLUT_CONFIGURE_WORK | GLUT_COLORMAP_WORK | GLUT_MAP_WORK)) {
    /* Be sure to set event mask *BEFORE* map window is done. */
    if (workMask & GLUT_EVENT_MASK_WORK) {
      long eventMask;

      /* Make sure children are not propogating events this
         window is selecting for.  Be sure to do this before
         enabling events on the children's parent. */
      if (window->children) {
        GLUTwindow *child = window->children;
        unsigned long attribMask = CWDontPropagate;
        XSetWindowAttributes wa;

        wa.do_not_propagate_mask = window->eventMask & GLUT_DONT_PROPAGATE_FILTER_MASK;
        if (window->eventMask & GLUT_HACK_STOP_PROPAGATE_MASK) {
          wa.event_mask = child->eventMask | (window->eventMask & GLUT_HACK_STOP_PROPAGATE_MASK);
          attribMask |= CWEventMask;
        }
        do {
          XChangeWindowAttributes(__glutDisplay, child->win,
            attribMask, &wa);
          child = child->siblings;
        } while (child);
      }
      eventMask = window->eventMask;
      if (window->parent && window->parent->eventMask & GLUT_HACK_STOP_PROPAGATE_MASK)
        eventMask |= (window->parent->eventMask & GLUT_HACK_STOP_PROPAGATE_MASK);
      XSelectInput(__glutDisplay, window->win, eventMask);

      if (window->overlay)
        XSelectInput(__glutDisplay, window->overlay->win,
          window->eventMask & GLUT_OVERLAY_EVENT_FILTER_MASK);
    }
    /* Be sure to set device mask *BEFORE* map window is done. */
    if (workMask & GLUT_DEVICE_MASK_WORK) {
      __glutUpdateInputDeviceMaskFunc(window);
    }
    /* Be sure to configure window *BEFORE* map window is done. 
     */
    if (workMask & GLUT_CONFIGURE_WORK) {
      XWindowChanges changes;

      changes.x = window->desiredX;
      changes.y = window->desiredY;
      if (window->desiredConfMask & (CWWidth | CWHeight)) {
        changes.width = window->desiredWidth;
        changes.height = window->desiredHeight;
        if (window->overlay)
          XResizeWindow(__glutDisplay, window->overlay->win,
            window->desiredWidth, window->desiredHeight);
        if (__glutMotifHints != None) {
          if (workMask & GLUT_FULL_SCREEN_WORK) {
            MotifWmHints hints;

            hints.flags = MWM_HINTS_DECORATIONS;
            hints.decorations = 0;  /* Absolutely no
                                       decorations. */
            XChangeProperty(__glutDisplay, window->win,
              __glutMotifHints, __glutMotifHints, 32,
              PropModeReplace, (unsigned char *) &hints, 4);
          } else {
            XDeleteProperty(__glutDisplay, window->win, __glutMotifHints);
          }
        }
      }
      if (window->desiredConfMask & CWStackMode) {
        changes.stack_mode = window->desiredStack;
        /* Do not let glutPushWindow push window beneath the
           underlay. */
        if (window->parent && window->parent->overlay && window->desiredStack == Below) {
          changes.stack_mode = Above;
          changes.sibling = window->parent->overlay->win;
          window->desiredConfMask |= CWSibling;
        }
      }
      XConfigureWindow(__glutDisplay, window->win,
        window->desiredConfMask, &changes);
      window->desiredConfMask = 0;
    }
    /* Be sure to establish the colormaps *BEFORE* map window
       is done. */
    if (workMask & GLUT_COLORMAP_WORK) {
      __glutEstablishColormapsProperty(window);
    }
    if (workMask & GLUT_MAP_WORK) {
      switch (window->desiredMapState) {
      case WithdrawnState:
        if (window->parent) {
          XUnmapWindow(__glutDisplay, window->win);
        } else {
          XWithdrawWindow(__glutDisplay, window->win,
            __glutScreen);
        }
        window->shownState = 0;
        break;
      case NormalState:
        XMapWindow(__glutDisplay, window->win);
        window->shownState = 1;
        break;
      case IconicState:
        XIconifyWindow(__glutDisplay, window->win, __glutScreen);
        window->shownState = 0;
        break;
      }
    }
  }
  if (workMask & (GLUT_REDISPLAY_WORK | GLUT_OVERLAY_REDISPLAY_WORK)) {
    if (window->forceReshape) {
      /* Guarantee that before a display callback is generated
         for a window, a reshape callback must be generated. */
      __glutSetWindow(window);
      window->reshape(window->width, window->height);
      window->forceReshape = False;
    }
    /* The code below is more involved than otherwise necessary
       because it is paranoid about the overlay or entire window
       being removed or destroyed in the course of the callbacks.
       Notice how the global __glutWindowDamaged is used to
       record the layers' damage status.  See the code in
       glutLayerGet for how __glutWindowDamaged is used. The 
       point is to not have to update the "damaged" field after 
       the callback since the window (or overlay) may be
       destroyed (or removed) when the callback returns. */

    if (window->overlay && window->overlay->display) {
      int num = window->num;
      Window xid = window->overlay ? window->overlay->win : None;

      /* If an overlay display callback is registered, we
         differentiate between a redisplay needed for the
         overlay and/or normal plane.  If there is no overlay
         display callback registered, we simply use the
         standard display callback. */

      if (workMask & GLUT_REDISPLAY_WORK) {

        /* Render to normal plane. */
        window->renderWin = window->win;
        window->renderCtx = window->ctx;
        __glutWindowDamaged = window->damaged;
        window->damaged = 0;
        __glutSetWindow(window);
        window->display();
        __glutWindowDamaged = 0;
      }
      if (workMask & GLUT_OVERLAY_REDISPLAY_WORK) {
        window = __glutWindowList[num];
        if (window && window->overlay &&
          window->overlay->win == xid && window->overlay->display) {

          /* Render to overlay. */
          window->renderWin = window->overlay->win;
          window->renderCtx = window->overlay->ctx;
          __glutWindowDamaged = window->overlay->damaged;
          window->overlay->damaged = 0;
          __glutSetWindow(window);
          window->overlay->display();
          __glutWindowDamaged = 0;
        } else {
          /* Overlay may have since been destroyed or the
             overlay callback may have been disabled during
             normal display callback. */
        }
      }
    } else {
      __glutWindowDamaged = window->damaged;
      window->damaged = 0;
      if (window->overlay) {
        __glutWindowDamaged |= window->overlay->damaged;
        window->overlay->damaged = 0;
      }
      __glutSetWindow(window);
      window->display();
      __glutWindowDamaged = 0;
    }
  }
  /* Combine workMask with window->workMask to determine what
     finish and debug work there is. */
  workMask |= window->workMask;

  if (workMask & GLUT_FINISH_WORK) {
    __glutSetWindow(window);
    glFinish();
  }
  if (workMask & GLUT_DEBUG_WORK) {
    GLenum error;

    __glutSetWindow(window);
    while ((error = glGetError()) != GL_NO_ERROR)
      __glutWarning("GL error: %s", gluErrorString(error));
  }
  /* Strip out dummy, finish, and debug work bits. */
  window->workMask &= ~(GLUT_DUMMY_WORK | GLUT_FINISH_WORK | GLUT_DEBUG_WORK);
  if (window->workMask) {
    /* Leave on work list. */
    return window;
  } else {
    /* Remove current window from work list. */
    return window->prevWorkWin;
  }
}

void
glutMainLoop(void)
{
  if (!__glutDisplay)
    __glutFatalUsage("main loop entered with out X connection.");
  if (!__glutWindowListSize)
    __glutFatalUsage(
      "main loop entered with no windows created.");
  for (;;) {
    if (__glutWindowWorkList) {
      GLUTwindow *remainder, *work;

      work = __glutWindowWorkList;
      __glutWindowWorkList = NULL;
      if (work) {
        remainder = processWindowWorkList(work);
        if (remainder) {
          *beforeEnd = __glutWindowWorkList;
          __glutWindowWorkList = remainder;
        }
      }
    }
    if (__glutIdleFunc || __glutWindowWorkList) {
      idleWait();
    } else {
      if (__glutTimerList) {
        waitForSomething();
      } else {
        processEvents();
      }
    }
  }
}
