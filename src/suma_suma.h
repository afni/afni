#ifndef SUMA_SUMA_INCLUDED
#define SUMA_SUMA_INCLUDED

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

/* from Fri Aug  9 17:54:03 EDT 2002 and on SUMA will need motif headers */

#include <Xm/XmAll.h>



#include "imseq.h"
#include "mrilib.h"
#include "niml.h"
#include "xutil.h"
#include "display.h"
#include "xim.h"



/* SUMA's generic includes */
   #include "uthash.h"
   #include "SUMA/SUMA_label.h"
   #include "suma_algorithms.h"
   #include "suma_afni_surface.h"
   #include "suma_datasets.h"
   #include "SUMA/SUMA_Macros.h"  
   #include "SUMA/SUMA_StringParse.h"
   #define SUMA_ENTRY ENTRY(FuncName)
   #define SUMA_ENTRY_LH ENTRY(FuncName); do { if (LocalHead) { SUMA_DUMP_TRACE("dumping trace at top function entry"); } } while (0)
   #define SUMA_RETURN  RETURN 
   #define SUMA_RETURNe EXRETURN 
   #define SUMA_mainENTRY mainENTRY(FuncName)
   
   #include "mcw_malloc.h"
   
   /* post March 3 04, using AFNI's allocation and tracing routines
   instead of SUMA's 
   If you do not want to use AFNI's allocation functions, then use
   -DDONT_USE_MCW_MALLOC in your compile command
   
   Relevant afni files:
   mcw_malloc.c/h
   debugtrace.c/h
   */
   
   #define SUMA_free mcw_free
   
   
   /* memory allocation section (SUMA_COMPILED-specific)*/
   #ifndef DONT_USE_MCW_MALLOC
      #define SUMA_malloc(a) mcw_malloc((a),__FILE__,__LINE__)
      #define SUMA_calloc(a,b) mcw_calloc((a),(b),__FILE__,__LINE__)
      #define SUMA_realloc(a,b) mcw_realloc((a),(b),__FILE__,__LINE__)
      #define SUMA_MEMTRACE_OFF {   /* No such thing */ }
         #define SUMA_MEMTRACE_ON {\
            enable_mcw_malloc() ;   \
         }
         #define SUMA_MEMTRACE_TOGGLE {   \
            if (!SUMAg_CF->MemTrace) { \
               enable_mcw_malloc() ;   \
            }  \
         }
   #else
      #define SUMA_malloc(a) mcw_malloc((a))
      #define SUMA_calloc(a,b) mcw_calloc((a),(b))
      #define SUMA_realloc(a,b) mcw_realloc((a),(b))
      #define SUMA_MEMTRACE_ON {}
      #define SUMA_MEMTRACE_OFF {}
      #define SUMA_MEMTRACE_TOGGLE {}
   #endif
   
   /* debug tracing section */
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

   
   /* define the necessary macros (SUMA_COMPILED-specific)*/   
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
      if (LocalHead) fprintf (SUMA_STDERR, "##      %s:\n %s\n", FuncName, msg);  \
   }
   #define SUMA_LHv SUMA_LH
   
   #define SUMA_S_Warn(msg) {\
      fprintf (SUMA_STDERR, "Warning %s:\n %s\n", FuncName, msg);  \
   }
   #define SUMA_S_Warnv SUMA_S_Warn 
   
   #define SUMA_S_Note(msg) {\
      fprintf (SUMA_STDERR, "Notice %s:\n %s\n", FuncName, msg);  \
   }
   #define SUMA_S_Notev SUMA_S_Note
   
   #define SUMA_S_Err(msg) {\
      fprintf (SUMA_STDERR, "Error %s:\n %s\n", FuncName, msg);  \
   }
   #define SUMA_S_Errv SUMA_S_Err
   
   #define SUMA_S_Crit(msg) {\
      fprintf (SUMA_STDERR, "Critical error %s:\n %s\n", FuncName, msg);  \
   }
    


#endif


