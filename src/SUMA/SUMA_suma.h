/*! includes various include files, no muss no fuss */
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
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
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>

/* from Fri Aug  9 17:54:03 EDT 2002 and on SUMA will need motif headers */

#include <Xm/XmAll.h>



#ifdef SOLARIS
	#include <GLw/GLwDrawA.h>  /* OpenGL drawing area. */
#else
   #ifdef SUMA_MOTIF_GLXAREA
      #include <GL/GLwMDrawA.h> 
   #else
	   #include <GL/GLwDrawA.h>  /* OpenGL drawing area. */
   #endif
#endif

#include "imseq.h"
#include "mrilib.h"
#include "niml.h"
#include "xutil.h"

#include "SUMA_Algorithms.h"
#include "SUMA_DataSets.h"
#include "SUMA_define.h"   
#include "SUMA_Macros.h"  
#include "SUMA_prototype.h"
#include "SUMA_ParseCommands.h"  
#include "SUMA_niml.h"
#include "SUMA_Engine.h"
#include "SUMA_display.h"
#include "SUMA_input.h"   
#include "SUMA_SVmanip.h"
#include "SUMA_DOmanip.h"  
#include "SUMA_MiscFunc.h"   
#include "SUMA_trackball.h"
#include "SUMA_Color.h"
#include "SUMA_GeomComp.h"
#include "SUMA_CreateDO.h"
#include "SUMA_Load_Surface_Object.h"
#include "SUMA_Surface_IO.h"
#include "SUMA_SphericalMapping.h"
