/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _MCW_DEBUGTRACE_
#define _MCW_DEBUGTRACE_

#ifdef DONT_USE_MCW_MALLOC

# define MCHECK /* nada */
# define MPROBE /* nada */

#else

# include "mcw_malloc.h"

# define MCHECK                           \
   do{ char * mc = mcw_malloc_status() ;  \
        if( mc != NULL ) printf("** Memory usage: %s\n",mc) ; } while(0)

# define MPROBE do{ if( !DBG_trace ) mcw_malloc_status() ; } while(0)

#endif

#define DBG_SIGNALS ( signal(SIGPIPE,DBG_sigfunc) , \
                      signal(SIGSEGV,DBG_sigfunc) , \
                      signal(SIGINT ,DBG_sigfunc) , \
                      signal(SIGBUS ,DBG_sigfunc)  )

#define ENTRY(rout)   DBG_entry(rout)

#define STATUS(str)   DBG_status(str)

#define DBEXIT        DBG_exit()

#define PRINT_TRACING DBG_print_tracing()

#define STATUS(str)                                               \
  do{ if(PRINT_TRACING){                                           \
        printf("%*.*s%s -- %s\n",DBG_num,DBG_num," ",DBROUT,(str)); \
        fflush(stdout) ; } } while(0)


#define RETURN(val) do{ DBEXIT ; return (val) ; } while(0)
#define EXRETURN    do{ DBEXIT ; return ; } while(0)

extern void DBG_entry(char *) ;
extern void DBG_status(char *) ;
extern void DBG_exit(void) ;
extern int  DBG_print_tracing(void) ;

#endif /* _MCW_DEBUGTRACE_ */
