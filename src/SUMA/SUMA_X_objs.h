#ifndef SUMA_SUMA_INCLUDED
#define SUMA_SUMA_INCLUDED

/* avoid "implicit declaration" issues (and hopefully not introduce others
 * [23 Dec 2021 rickr] */
#define GL_GLEXT_PROTOTYPES

/*! includes various include files, no muss no fuss */
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
/** #include <stdarg.h> **/
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>  /* For XA_RGB_DEFAULT_MAP. */
#include <X11/Xmu/StdCmap.h>  /* For XmuLookupStandardColormap. */
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/CascadeB.h>
#include <Xm/Frame.h>
#include <math.h>
#include <signal.h>

/* from Fri Aug  9 17:54:03 EDT 2002 and on SUMA will need motif headers */

#include <Xm/XmAll.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glext.h>

#include "imseq.h"
#include "mrilib.h"
#include "niml.h"
#include "xutil.h"
#include "display.h"
#include "xim.h"
#include "gicor.h"

#include "suma_objs.h"  /* RWC [21 Apr 2020] */

#endif


