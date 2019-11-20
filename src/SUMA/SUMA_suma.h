#ifndef SUMA_SUMA_SUMA_INCLUDED
#define SUMA_SUMA_SUMA_INCLUDED

#include "../suma_suma.h"
#include "SUMA_DataSets.h"

/* memory allocation section, SUMA_COMPILED-specific */
   /* #ifndef  DONT_USE_MCW_MALLOC */
   #if defined(USING_MCW_MALLOC) && !defined(USE_OMP)
      #ifdef SUMA_COMPILED
         #undef SUMA_MEMTRACE_ON
         #undef SUMA_MEMTRACE_TOGGLE
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
      #endif
   #endif
   
   /* debug tracing section */
   /* Undefine all that's been defined in suma_suma.h and is SUMA_COMPILED-specific */
   #ifdef SUMA_COMPILED
      #ifdef USE_TRACING
         #undef SUMA_INOUT_NOTIFY_ON
         #define SUMA_INOUT_NOTIFY_ON {\
            SUMAg_CF->InOut_Notify = YUP; \
            DBG_trace = 1;\
         }
         #undef SUMA_INOUT_NOTIFY_OFF
         #define SUMA_INOUT_NOTIFY_OFF {\
            SUMAg_CF->InOut_Notify = NOPE; \
            DBG_trace = 0; \
         }
         #undef SUMA_INOUT_NOTIFY_TOGGLE
         #define SUMA_INOUT_NOTIFY_TOGGLE {\
            SUMAg_CF->InOut_Notify = !SUMAg_CF->InOut_Notify; \
            if (!DBG_trace) DBG_trace = 1;  \
            else DBG_trace = 0;  \
         }
         #undef SUMA_ECHO_KEYPRESS_ON
         #define SUMA_ECHO_KEYPRESS_ON {\
            SUMAg_CF->Echo_KeyPress = YUP; \
         }
         #undef SUMA_ECHO_KEYPRESS_OFF
         #define SUMA_ECHO_KEYPRESS_OFF {\
            SUMAg_CF->Echo_KeyPress = NOPE; \
         }
         #undef SUMA_ECHO_KEYPRESS_TOGGLE
         #define SUMA_ECHO_KEYPRESS_TOGGLE {\
            SUMAg_CF->Echo_KeyPress = !SUMAg_CF->Echo_KeyPress; \
         }
      #else
         #undef SUMA_INOUT_NOTIFY_ON
         #define SUMA_INOUT_NOTIFY_ON {\
            SUMAg_CF->InOut_Notify = YUP; \
         }
         #undef SUMA_INOUT_NOTIFY_OFF
         #define SUMA_INOUT_NOTIFY_OFF {\
            SUMAg_CF->InOut_Notify = NOPE; \
         }
         #undef SUMA_INOUT_NOTIFY_TOGGLE
         #define SUMA_INOUT_NOTIFY_TOGGLE {\
            SUMAg_CF->InOut_Notify = !SUMAg_CF->InOut_Notify; \
         }
         #undef SUMA_ECHO_KEYPRESS_ON
         #define SUMA_ECHO_KEYPRESS_ON {\
            SUMAg_CF->Echo_KeyPress = YUP; \
         }
         #undef SUMA_ECHO_KEYPRESS_OFF
         #define SUMA_ECHO_KEYPRESS_OFF {\
            SUMAg_CF->Echo_KeyPress = NOPE; \
         }
         #undef SUMA_ECHO_KEYPRESS_TOGGLE
         #define SUMA_ECHO_KEYPRESS_TOGGLE {\
            SUMAg_CF->Echo_KeyPress = !SUMAg_CF->Echo_KeyPress; \
         }
      #endif
   #endif

/* The include files */
#if defined SUMA_COMPILED
   /* Undefine all that's been defined in suma_suma.h and is SUMA_COMPILED-specific */
   
   #undef SUMA_STDERR
   #undef SUMA_STDOUT
   
   #undef SUMA_SLP_Err
   #undef SUMA_SL_Err
   #undef SUMA_L_Err
   
   #undef SUMA_SLP_Note
   #undef SUMA_SL_Note
   #undef SUMA_L_Note
   
   #undef SUMA_SLP_Warn
   #undef SUMA_SL_Warn
   #undef SUMA_L_Warn
   
   #undef SUMA_SLP_Crit
   #undef SUMA_SL_Crit
   #undef SUMA_L_Crit
   
   #undef SUMA_LH
   #undef SUMA_LHv
   #undef SUMA_S_Warn
   #undef SUMA_S_Warnv
   #undef SUMA_S_Note
   #undef SUMA_S_Notev
   #undef SUMA_S_Err
   #undef SUMA_S_Errv
   #undef SUMA_S_Crit
   #undef SUMA_S_Critv
    
   #ifdef SOLARIS
	   #include <GLw/GLwDrawA.h>  /* OpenGL drawing area. */
   #else
      #ifdef SUMA_MOTIF_GLXAREA
         #include <GL/GLwMDrawA.h> 
      #else
	      #include <GL/GLwDrawA.h>  /* OpenGL drawing area. */
      #endif
   #endif
   #include <GL/gl.h>
   #include <GL/glu.h>
   #include <GL/glx.h>
   #include <GL/glut.h>
   

   
   /* SUMA specific includes*/
   #include "SUMA_niml_defines.h"
   #include "SUMA_define.h"   
   #include "SUMA_prototype.h"
   #include "SUMA_ParseCommands.h"  
   #include "SUMA_niml.h"
   #include "SUMA_ExpEval.h"
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
   #include "SUMA_volume_render.h"
   #include "SUMA_Load_Surface_Object.h"
   #include "SUMA_Surface_IO.h"
   #include "SUMA_SphericalMapping.h"
   #include "SUMA_xColBar.h"
   #include "SUMA_SurfClust.h"
   #include "SUMA_IsoSurface.h"
   #include "SUMA_BrainWrap.h"
   #include "SUMA_SurfaceToSurface.h"
   #include "SUMA_LocalStat.h"
   #include "SUMA_dot.h"
   #include "SUMA_SegOpts.h"
   #include "SUMA_SegFunc.h"
   #include "SUMA_driver.h"
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
   
   #define SUMA_LHv( ... ) {\
      if (LocalHead) {  \
         fprintf (SUMA_STDERR, "##      %s (%s:%d):\n ", \
                               FuncName, __FILE__, __LINE__);  \
         fprintf (SUMA_STDERR, __VA_ARGS__ );  \
      }  \
   }
    
   #define SUMA_LH( ... ) {\
      if (LocalHead) { SUMA_LHv( __VA_ARGS__ ); \
                       fprintf (SUMA_STDERR,"\n"); } \
   }
   
   #define SUMA_S_Warnv( ... ) {\
      fprintf (SUMA_STDERR, "oo     Warning %s (%s:%d):\n ", \
                            FuncName, __FILE__, __LINE__);  \
      fprintf (SUMA_STDERR, __VA_ARGS__);  \
   }
   
   #define SUMA_S_Warn( ... ) {\
      SUMA_S_Warnv( __VA_ARGS__ );   \
      fprintf (SUMA_STDERR, "\n");  \
   } 
   
   #define SUMA_S_Notev( ... ) {\
      fprintf (SUMA_STDERR, "++     Notice %s (%s:%d):\n ",\
                            FuncName, __FILE__, __LINE__);  \
      fprintf (SUMA_STDERR, __VA_ARGS__);  \
   }
   
   #define SUMA_S_Note( ... ) {\
      SUMA_S_Notev( __VA_ARGS__ );   \
      fprintf (SUMA_STDERR, "\n");  \
   }
   
   #define SUMA_S_Errv( ... ) {\
      fprintf (SUMA_STDERR, "--     Error %s (%s:%d):\n ", \
                            FuncName, __FILE__, __LINE__);  \
      fprintf (SUMA_STDERR, __VA_ARGS__);  \
   }
   
   #define SUMA_S_Err( ... ) {\
      SUMA_S_Errv( __VA_ARGS__ );   \
      fprintf (SUMA_STDERR, "\n");  \
   }
   
   #define SUMA_S_Critv(...) {\
      fprintf (SUMA_STDERR, "**     Critical error %s (%s:%d):\n ", \
                              FuncName, __FILE__, __LINE__);  \
      fprintf (SUMA_STDERR, __VA_ARGS__);  \
   }
   #define SUMA_S_Crit( ... ) {\
      SUMA_S_Critv( __VA_ARGS__ );   \
      fprintf (SUMA_STDERR, "\n");  \
   }
    
#endif

extern void SUMA_freep(void *) ; /* 07 Oct 2015 */

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


#endif
