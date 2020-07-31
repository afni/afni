#ifndef SUMA_OBJS_INCLUDED
#define SUMA_OBJS_INCLUDED

/*! includes various include files, no muss no fuss */
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
/** #include <stdarg.h> **/
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include <signal.h>

/* from Fri Aug  9 17:54:03 EDT 2002 and on SUMA will need motif headers */

#include "mrilib.h"
#include "niml.h"
#include "gicor.h"


/* SUMA's generic includes */
   #include "uthash.h"
   #include "mcw_malloc.h"
   #include "SUMA/SUMA_label.h"
   #include "suma_algorithms.h"
   #include "suma_afni_surface.h"
   #include "suma_utils.h"
   #include "suma_string_manip.h"
   #include "suma_datasets.h"
   #include "suma_help.h"
   #include "SUMA/SUMA_Macros.h"  
   #include "SUMA/SUMA_StringParse.h"
   #define SUMA_ENTRY ENTRY(FuncName)
   #define SUMA_ENTRY_LH ENTRY(FuncName); \
           do { if (LocalHead) {          \
                  SUMA_DUMP_TRACE("dumping trace at top function entry"); } \
              } while (0)
   #define SUMA_RETURN  RETURN 
   #define SUMA_RETURNe EXRETURN 
   #define SUMA_mainENTRY mainENTRY(FuncName)
   
      

   
   /* memory allocation section (SUMA_COMPILED-specific)*/
   #ifdef ALLOW_MCW_MALLOC 
   /*#if defined(USING_MCW_MALLOC) && !defined(USE_OMP)*/
      #define SUMA_malloc(a) mcw_malloc((a),__FILE__,__LINE__)
      #define SUMA_calloc(a,b) mcw_calloc((a),(b),__FILE__,__LINE__)
      #define SUMA_realloc(a,b) mcw_realloc((a),(b),__FILE__,__LINE__)
      #define SUMA_MEMTRACE_OFF {   /* No such thing */ }
      #if defined(USING_MCW_MALLOC) && !defined(USE_OMP)
         #define SUMA_MEMTRACE_ON {\
            enable_mcw_malloc() ;   \
         }
         #define SUMA_MEMTRACE_TOGGLE {   \
            if (!SUMAg_CF->MemTrace) { \
               enable_mcw_malloc() ;   \
            }  \
         }
      #else
         #define SUMA_MEMTRACE_ON {}
         #define SUMA_MEMTRACE_TOGGLE {}
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
   
   #define SUMA_LHv( ... ) {\
      if (LocalHead) {  \
         fprintf (SUMA_STDERR, "##      %s (%s:%d):\n", \
                               FuncName, __FILE__, __LINE__);  \
         fprintf (SUMA_STDERR, __VA_ARGS__);  \
      }  \
   }
   #define SUMA_LH( ... ) {\
      if (LocalHead) {\
         SUMA_LHv(__VA_ARGS__);  \
         fprintf (SUMA_STDERR, "\n");  \
      }  \
   }
   
   
   #define TLH(v) {\
      if (v) { LocalHead = v; SUMA_LH("LocalHead Temp ON"); } \
      else { SUMA_LH("LocalHead Temp OFF"); LocalHead = NOPE; }   \
   }
   
   #ifdef SUMA_noFunc
     #define SUMA_S_Warn(...) {\
      fprintf (SUMA_STDERR, "Warning:\n");  \
      fprintf (SUMA_STDERR , __VA_ARGS__);  \
     }
   #else
     #define SUMA_S_Warn(...) {\
      fprintf (SUMA_STDERR, "Warning %s:\n ", FuncName);  \
      fprintf (SUMA_STDERR , __VA_ARGS__);  \
      fprintf (SUMA_STDERR , "\n"); \
     }
   #endif

   #define SUMA_S_Warnv(...) {\
      fprintf (SUMA_STDERR, "Warning %s:\n", FuncName);  \
      fprintf (SUMA_STDERR, __VA_ARGS__);  \
   } 
   
   #define SUMA_S_Notev(...) {\
      fprintf (SUMA_STDERR, "Notice %s:\n", FuncName);  \
      fprintf (SUMA_STDERR , __VA_ARGS__);  \
   }
   #define SUMA_S_Note(...) {\
      SUMA_S_Notev(__VA_ARGS__);  \
      fprintf (SUMA_STDERR, "\n");  \
   }

   #define SUMA_S_Errv(...) {\
      fprintf (SUMA_STDERR, "Error %s (%s:%d):\n", \
                            FuncName, __FILE__, __LINE__);  \
      fprintf (SUMA_STDERR, __VA_ARGS__);  \
   }
   #define SUMA_S_Err(...) {\
      SUMA_S_Errv(__VA_ARGS__);  \
      fprintf (SUMA_STDERR, "\n");  \
   }
   
   #define SUMA_S_Critv( ... ) {\
      fprintf (SUMA_STDERR, "Critical error %s (%s:%d):\n", \
                            FuncName, __FILE__, __LINE__);  \
      fprintf (SUMA_STDERR, __VA_ARGS__);  \
   }
   #define SUMA_S_Crit(...) {\
      SUMA_S_Critv(__VA_ARGS__);\
      fprintf (SUMA_STDERR,"\n");   \
   }
    

#endif
