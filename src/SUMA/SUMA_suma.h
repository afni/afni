/*! includes various include files, no muss no fuss */
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>  /* For XA_RGB_DEFAULT_MAP. */
#include <X11/Xmu/StdCmap.h>  /* For XmuLookupStandardColormap. */
#include <math.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>

#ifdef SUMA_MOTIF_GLXAREA
	#include <Xm/Form.h>    /* Motif Form widget. */
	#include <Xm/Frame.h>   /* Motif Frame widget. */
	#include <GL/GLwMDrawA.h>  /* Motif OpenGL drawing area. */
#else
	#include <GL/GLwDrawA.h>  /* OpenGL drawing area. */
#endif



#include "mrilib.h"
#include "niml.h"

#include "SUMA_define.h"   
#include "SUMA_Macros.h"  
#include "SUMA_prototype.h"  
#include "SUMA_input.h"   
#include "SUMA_SVmanip.h"
#include "SUMA_DOmanip.h"  
#include "SUMA_MiscFunc.h"   
#include "SUMA_trackball.h"
#include "SUMA_Color.h"
