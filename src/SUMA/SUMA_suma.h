/*! includes various include files, no muss no fuss */
#include <stddef.h>
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
#include <signal.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>
#include <GLUT/GL/glut.h>

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
#include "display.h"
#include "xim.h"



/* SUMA's generic includes */
   #include "SUMA_Algorithms.h"
   #include "SUMA_DataSets.h"
   #include "SUMA_Macros.h"  

   #define SUMA_ENTRY ENTRY(FuncName)
   #define SUMA_RETURN RETURN
   #define SUMA_RETURNe EXRETURN
   #define SUMA_mainENTRY mainENTRY(FuncName)
   
   #include "../mcw_malloc.h"
   
   /* post March 3 04, using AFNI's allocation and tracing routines
   instead of SUMA's 
   If you do not want to use AFNI's allocation functions, then use
   -DDONT_USE_MCW_MALLOC in your compile command
   
   Relevant afni files:
   mcw_malloc.c/h
   debugtrace.c/h
   */
   
   #define SUMA_free mcw_free
   
   /* memory allocation section */
   #ifndef DONT_USE_MCW_MALLOC
      #define SUMA_malloc(a) mcw_malloc((a),__FILE__,__LINE__)
      #define SUMA_calloc(a,b) mcw_calloc((a),(b),__FILE__,__LINE__)
      #define SUMA_realloc(a,b) mcw_realloc((a),(b),__FILE__,__LINE__)
      #define SUMA_MEMTRACE_OFF {   /* No such thing */ }
      #ifdef SUMA_COMPILED
         #define SUMA_MEMTRACE_ON {\
            enable_mcw_malloc() ;   \
            SUMAg_CF->MemTrace = YUP;  \
         }
         #define SUMA_MEMTRACE_TOGGLE {   \
            if (!SUMAg_CF->MemTrace) { \
               SUMAg_CF->MemTrace = YUP; \
               enable_mcw_malloc() ;   \
            }  \
         }
      #else
         #define SUMA_MEMTRACE_ON {\
            enable_mcw_malloc() ;   \
         }
         #define SUMA_MEMTRACE_TOGGLE {   \
            if (!SUMAg_CF->MemTrace) { \
               enable_mcw_malloc() ;   \
            }  \
         }
      #endif
   #else
      #define SUMA_malloc(a) mcw_malloc((a))
      #define SUMA_calloc(a,b) mcw_calloc((a),(b))
      #define SUMA_realloc(a,b) mcw_realloc((a),(b))
      #define SUMA_MEMTRACE_ON {}
      #define SUMA_MEMTRACE_OFF {}
      #define SUMA_MEMTRACE_TOGGLE {}
   #endif
   
   /* debug tracing section */
   #ifdef SUMA_COMPILED
      #ifdef USE_TRACING
         #define SUMA_INOUT_NOTIFY_ON {\
            SUMAg_CF->InOut_Notify = YUP; \
            DBG_trace = 1;\
         }
         #define SUMA_INOUT_NOTIFY_OFF {\
            SUMAg_CF->InOut_Notify = NOPE; \
            DBG_trace = 0; \
         }
         #define SUMA_INOUT_NOTIFY_TOGGLE {\
            SUMAg_CF->InOut_Notify = !SUMAg_CF->InOut_Notify; \
            if (!DBG_trace) DBG_trace = 1;  \
            else DBG_trace = 0;  \
         }
      #else
         #define SUMA_INOUT_NOTIFY_ON {\
            SUMAg_CF->InOut_Notify = YUP; \
         }
         #define SUMA_INOUT_NOTIFY_OFF {\
            SUMAg_CF->InOut_Notify = NOPE; \
         }
         #define SUMA_INOUT_NOTIFY_TOGGLE {\
            SUMAg_CF->InOut_Notify = !SUMAg_CF->InOut_Notify; \
         }
      #endif
   #else
      #ifdef USE_TRACING
         #define SUMA_INOUT_NOTIFY_ON { DBG_trace = 1; }
         #define SUMA_INOUT_NOTIFY_OFF { DBG_trace = 0; }
         #define SUMA_INOUT_NOTIFY_TOGGLE {\
            if (!DBG_trace) DBG_trace = 1;  \
            else DBG_trace = 0;  \
         }
      #else
         #define SUMA_INOUT_NOTIFY_ON { }
         #define SUMA_INOUT_NOTIFY_OFF { }
         #define SUMA_INOUT_NOTIFY_TOGGLE { }
      #endif
   #endif

/* The include files */
#if defined SUMA_COMPILED
   /* SUMA specific includes*/
   #include "SUMA_define.h"   
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
   #include "SUMA_xColBar.h"
#else
   /* define the necessary macros */   
   #define SUMA_STDERR stderr
   #define SUMA_STDOUT stdout
   
   #define SUMA_SLP_Err SUMA_S_Err
   #define SUMA_SL_Err SUMA_S_Err
   #define SUMA_L_Err SUMA_S_Err
   
   #define SUMA_SLP_Note SUMA_S_Note
   #define SUMA_SL_Note SUMA_S_Note
   #define SUMA_L_Note SUMA_S_Note
   
   #define SUMA_SLP_Warn SUMA_S_Warn
   #define SUMA_SL_Warn SUMA_S_Warn
   #define SUMA_L_Warn SUMA_S_Warn
   
   #define SUMA_SLP_Crit SUMA_S_Crit
   #define SUMA_SL_Crit SUMA_S_Crit
   #define SUMA_L_Crit SUMA_S_Crit
   
   #define SUMA_LH(msg) {\
      if (LocalHead) fprintf (SUMA_STDERR, "%s:\n %s\n", FuncName, msg);  \
   }
   
   #define SUMA_S_Warn(msg) {\
      fprintf (SUMA_STDERR, "Warning %s:\n %s\n", FuncName, msg);  \
   }
   
   #define SUMA_S_Note(msg) {\
      fprintf (SUMA_STDERR, "Notice %s:\n %s\n", FuncName, msg);  \
   }
   
   #define SUMA_S_Err(msg) {\
      fprintf (SUMA_STDERR, "Error %s:\n %s\n", FuncName, msg);  \
   }
   
   #define SUMA_S_Crit(msg) {\
      fprintf (SUMA_STDERR, "Critical error %s:\n %s\n", FuncName, msg);  \
   }
    
#endif



/******************************* BEGIN IGNORE THIS CHUNK ********************************/
#ifdef USE_SUMA_MALLOC
   /* NO LONGER SUPPORTED Apr. 09 04 */

   /* The pre-March 3/04 way, SUMA uses its own version of memory allocation
   and tracing. Those tools did not allow for memory corruption checking and
   used linear pointer storage methods making for inefficient searching 
      Use -DUSE_SUMA_MALLOC in the compile line if you wish to use the old stuff
   */
   #define SUMA_free( p ) \
	   SUMA_free_fn( FuncName, p )
   #define SUMA_calloc( nmemb,  size) \
	   SUMA_calloc_fn( FuncName, nmemb, size)
   #define SUMA_malloc(size) \
	   SUMA_malloc_fn( FuncName, size)
   #define SUMA_realloc( ptr, size) \
	   SUMA_realloc_fn( FuncName, ptr, size)
   #define SUMA_ENTRY  { \
      if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);   \
   }
   #define SUMA_RETURN(m_rvar) {\
      if (SUMAg_CF->InOut_Notify) { SUMA_DBG_OUT_NOTIFY(FuncName); }\
      return(m_rvar);\
   }
   #define SUMA_RETURNe  {\
      if (SUMAg_CF->InOut_Notify) { SUMA_DBG_OUT_NOTIFY(FuncName); }\
      return ;\
   }
   #define SUMA_mainENTRY {}

   #define SUMA_INOUT_NOTIFY_ON {\
      SUMAg_CF->InOut_Notify = YUP; \
   }
   #define SUMA_INOUT_NOTIFY_OFF {\
      SUMAg_CF->InOut_Notify = NOPE; \
   }

   #define SUMA_INOUT_NOTIFY_TOGGLE {\
      SUMAg_CF->InOut_Notify = !SUMAg_CF->InOut_Notify; \
   }
   #define SUMA_MEMTRACE_ON {\
      SUMAg_CF->MemTrace = YUP;  \
   }
   #define SUMA_MEMTRACE_OFF {   \
      SUMAg_CF->MemTrace = OFF;  \
   }
   #define SUMA_MEMTRACE_TOGGLE {   \
      SUMAg_CF->MemTrace = !SUMAg_CF->MemTrace; \
   }
#endif
/******************************* END IGNORE THIS CHUNK ********************************/
