/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#ifndef _MCW_DEBUGTRACE_
#define _MCW_DEBUGTRACE_

/** inputs:
     USE_TRACING ==> if set, include tracing information **/

/*** Modified 23 Aug 1998 to eliminate PRINT_TRACING.
     Now, if DBG_trace > 1, will print STATUS messages. ***/

/*** Modified 06 Mar 1999 to eliminate GNU malloc stuff,
     and replace it with mcw_malloc stuff.              ***/

/*** Modified 26 Jan 2001 to separate it more fully from
     AFNI itself and incorporate it into the mrilib.    ***/

/*-------------------------------------------------------------------
   29 Jan 2001: a hack for debugging files selectively
---------------------------------------------------------------------*/

#ifndef TWO_TWO
   /** combine two interpreted tokens into one using TWO_TWO **/
 
#  define TWO_ONE(x,y) x ## y
#  define TWO_TWO(x,y) TWO_ONE(x,y)
#endif

#ifndef DEBUGTHISFILE
   /** as in
         if( DEBUGTHISFILE ) fprintf(stderr,"Yo mama!\n") ;
    **/

#  define DEBUGTHISFILE getenv(TWO_TWO(__FILE__, "_DEBUG"))
#endif

/*********************************************************************/
#ifdef USE_TRACING

#define DEBUG_MAX_DEPTH 256  /* way too big, but who cares? */

#define DBG_label DBG_labels[DBG_trace]

/*---------------------------------------------------------------*/

#ifdef DONT_USE_MCW_MALLOC

# define MCHECK /* nada */
# define MPROBE /* nada */

#else

# define MCHECK                           \
   do{ char * mc = mcw_malloc_status() ;  \
        if( mc != NULL ) printf("** Memory usage: %s\n",mc) ; } while(0)

# define MPROBE do{ if( !DBG_trace ) mcw_malloc_status() ; } while(0)

#endif

/*----------------------------------------------------------------
   Define things to be used in debugtrace.c
------------------------------------------------------------------*/

#include <signal.h>

#ifdef _DEBUGTRACE_MAIN_
   char * DBG_rout[DEBUG_MAX_DEPTH] = { "Bottom of Debug Stack" } ;
   int DBG_num   = 1 ;
   int DBG_trace = 0 ;   /* turn off at start (cf. mainENTRY) */

   char * DBG_labels[3] = { "Trace=OFF " , "Trace=LOW " , "Trace=HIGH" } ;

void DBG_sigfunc(int sig)   /** signal handler for fatal errors **/
{
   char * sname ; int ii ;
   switch(sig){
      default:      sname = "unknown" ; break ;
      case SIGPIPE: sname = "SIGPIPE" ; break ;
      case SIGSEGV: sname = "SIGSEGV" ; break ;
      case SIGBUS:  sname = "SIGBUS"  ; break ;
      case SIGINT:  sname = "SIGINT"  ; break ;
   }
   fprintf(stderr,"\nFatal Signal %d (%s) received\n",sig,sname) ;
   if( DBG_num >= 0 ){
      for( ii=DBG_num-1; ii >= 0 ; ii-- )
         fprintf(stderr,"%*.*s%s\n",ii+1,ii+1," ",DBG_rout[ii]) ;
   } else {
      fprintf(stderr,"[No debug tracing stack: DBG_num=%d]\n",DBG_num) ;
   }
   fprintf(stderr,"*** Program Abort ***\n") ; fflush(stderr) ;
   MPROBE ; exit(1) ;
}

/*---------------------------------------------------------------
   Call out the variables defined in debugtrace.c
-----------------------------------------------------------------*/

#else /* not _DEBUGTRACE_MAIN_ */
   extern char * DBG_rout[DEBUG_MAX_DEPTH] ;
   extern int DBG_num ;
   extern int DBG_trace ;
   extern char * DBG_labels[3] ;
   extern void DBG_sigfunc(int) ;
#endif /* _DEBUGTRACE_MAIN_ */

#define DBG_SIGNALS ( signal(SIGPIPE,DBG_sigfunc) , \
                      signal(SIGSEGV,DBG_sigfunc) , \
                      signal(SIGINT ,DBG_sigfunc) , \
                      signal(SIGBUS ,DBG_sigfunc)  )
/*---------------------------------------------------------------*/

/* macros for entering and exiting a function */

#define DBG_LEADER_IN  "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
#define DBG_LEADER_OUT "-----------------------------------------------------------"

#define ENTRY(rout) do{ static char * rrr = (rout) ;  DBG_rout[DBG_num++] = rrr ; \
                        if( DBG_trace ){                                         \
                          printf("%*.*s%s [%d]: ENTRY (file=%s line=%d)\n",     \
                                 DBG_num,DBG_num,DBG_LEADER_IN,rrr,DBG_num,    \
                                 __FILE__ , __LINE__ ) ;                      \
                          MCHECK ; fflush(stdout) ; } } while(0)

#define DBROUT      DBG_rout[DBG_num-1]

#define DBEXIT      do{ if( DBG_trace ){                                      \
                          printf("%*.*s%s [%d]: EXIT (file=%s line=%d)\n",     \
                                 DBG_num,DBG_num,DBG_LEADER_OUT,DBROUT,DBG_num, \
                                 __FILE__ , __LINE__ );                        \
                          MCHECK ; fflush(stdout) ; }                         \
                        DBG_num = (DBG_num>1) ? DBG_num-1 : 1 ; } while(0)

#define mainENTRY(rout)                                            \
  do{ char *e=getenv("AFNI_TRACE");                                \
      if( e != NULL )                                              \
         DBG_trace = (*e=='y') ? 1 : (*e=='Y') ? 2 : 0 ;           \
      DBG_SIGNALS ; ENTRY(rout) ;                        } while(0)

#define PRINT_TRACING (DBG_trace > 1)

#define STATUS(str)                                               \
  do{ if(PRINT_TRACING){                                           \
        printf("%*.*s%s -- %s\n",DBG_num,DBG_num," ",DBROUT,(str)); \
        fflush(stdout) ; } } while(0)

/*********************************************************************/
#else /* don't USE_TRACING */

#  define ENTRY(rout)   /* nada */
#  define DBEXIT        /* nada */
#  define DBROUT        /* nada */
#  define STATUS(str)   /* nada */
#  define DBG_SIGNALS   /* nada */
#  define MCHECK        /* nada */
#  define MPROBE        /* nada */
#  define PRINT_TRACING 0
#  define DBG_trace     0          /* 09 Dec 1999 */

#  ifdef _DEBUGTRACE_MAIN_
      void DBG_sigfunc(int sig){} /* does nada */
#  else
      extern void DBG_sigfunc(int) ;
#  endif

#  define mainENTRY(rout) /* nada */

#endif /* USE_TRACING */
/*********************************************************************/


#define RETURN(val) do{ DBEXIT ; return (val) ; } while(0)
#define EXRETURN    do{ DBEXIT ; return ; } while(0)

/*---------------------------------------------------------------*/

#ifndef MCHECK
# define MCHECK /* nada */
# define MPROBE /* nada */
#endif
/*---------------------------------------------------------------*/

#endif /* _MCW_DEBUGTRACE_ */
