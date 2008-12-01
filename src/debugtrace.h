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

/*** 20 Feb 2001: added TRACEBACK and EXIT macros       ***/

/*-------------------------------------------------------------------
   29 Jan 2001: a hack for debugging files selectively
---------------------------------------------------------------------*/

#ifndef TWO_TWO
   /** combine two interpreted tokens into one using TWO_TWO **/

#  define TWO_ONE(x,y) x ## y
#  define TWO_TWO(x,y) TWO_ONE(x,y)
#endif

/*********************************************************************/
#ifdef USE_TRACING

#define DEBUG_MAX_DEPTH 1024  /* way too big, but who cares? */

#define DBG_label DBG_labels[DBG_trace]

/*---------------------------------------------------------------*/

#ifdef DONT_USE_MCW_MALLOC

# define MCHECK /* nada */
# define MPROBE /* nada */

#else

# define MCHECK                                       \
   do{ char *mc = MCW_MALLOC_status ;                 \
        if( DBG_fp==NULL ) DBG_fp=stdout;             \
        if( mc != NULL ){                             \
          fprintf(DBG_fp,"** Memory usage: %s\n",mc); \
          fflush(DBG_fp) ;                            \
        }                                             \
   } while(0)

# define MPROBE do{ if( !DBG_trace ) (void)MCW_MALLOC_status ; } while(0)

#endif

#define TRACEBACK DBG_traceback()  /* 20 Feb 2001 */

/*----------------------------------------------------------------
   Define things to be used in debugtrace.c
------------------------------------------------------------------*/

#include <signal.h>
#include <unistd.h>

#ifdef  __cplusplus
extern "C" {
#endif

#ifdef _DEBUGTRACE_MAIN_
   char *DBG_rout[DEBUG_MAX_DEPTH] = { "Bottom of Debug Stack" } ;
   int DBG_num   = 1 ;
   int DBG_trace = 0 ;   /* turn off at start (cf. mainENTRY) */
   FILE *DBG_fp  = NULL ;    /* 01 Sep 2006 */

   char *DBG_labels[3] = { "Trace=OFF " , "Trace=LOW " , "Trace=HIGH" } ;

   char last_status[1024] = "\0" ;  /* 22 Apr 2002 */

/*------------------------------------------------------*/
#ifdef SHOWOFF
# undef  SHSH
# undef  SHSHSH
# undef  SHSTRING
# define SHSH(x)   #x
# define SHSHSH(x) SHSH(x)
# define SHSTRING  SHSHSH(SHOWOFF)   /* now in "quotes" */
#else
# undef  SHSTRING
#endif
/*------------------------------------------------------*/

void DBG_traceback(void)
{ int tt ;
  MCHECK ;
  if( last_status[0] != '\0' )
    fprintf(stderr,"Last STATUS: %s\n",last_status) ;
  for( tt=DBG_num-1; tt >= 1 ; tt-- )
    fprintf(stderr,"%*.*s%s\n",tt+1,tt+1," ",DBG_rout[tt]) ;
}

void DBG_sigfunc(int sig)   /** signal handler for fatal errors **/
{
   char *sname ; int ii ;
   static volatile int fff=0 ;
   if( fff ) _exit(1); else fff=1 ;
   switch(sig){
     default:      sname = "unknown" ; break ;
     case SIGPIPE: sname = "SIGPIPE" ; break ;
     case SIGSEGV: sname = "SIGSEGV" ; break ;
     case SIGBUS:  sname = "SIGBUS"  ; break ;
     case SIGINT:  sname = "SIGINT"  ; break ;
     case SIGTERM: sname = "SIGTERM" ; break ;
   }
   fprintf(stderr,"\nFatal Signal %d (%s) received\n",sig,sname) ;
   if( last_status[0] != '\0' )
     fprintf(stderr,"Last STATUS: %s\n",last_status) ;
   if( DBG_num >= 0 ){
     for( ii=DBG_num-1; ii >= 0 ; ii-- )
       fprintf(stderr,"%*.*s%s\n",ii+1,ii+1," ",DBG_rout[ii]) ;
   } else {
     fprintf(stderr,"[No debug tracing stack: DBG_num=%d]\n",DBG_num) ;
   }
#ifdef AFNI_VERSION_LABEL
   fprintf(stderr,"** AFNI version = " AFNI_VERSION_LABEL
                   "  Compile date = " __DATE__ "\n" );
#endif
#ifdef SHSTRING
   fprintf(stderr,"** [[Precompiled binary " SHSTRING ": " __DATE__ "]]\n");
#endif

   fprintf(stderr,"** Program Abort **\n") ;
   if( sig != SIGINT && sig != SIGTERM )
   fprintf(stderr,"** If you report this crash to the AFNI message board,\n"
                  "** please copy the error messages EXACTLY, and give\n"
                  "** the command line you used to run the program, and\n"
                  "** any other information needed to repeat the problem.\n"
                  "** You may later be asked to upload data to help debug.\n");
   fflush(stderr) ;
   MPROBE ; exit(1) ;
}

/*---------------------------------------------------------------
   Call out the variables defined in debugtrace.c
-----------------------------------------------------------------*/

#else /* not _DEBUGTRACE_MAIN_ */
   extern char *DBG_rout[DEBUG_MAX_DEPTH] ;
   extern int DBG_num ;
   extern int DBG_trace ;
   extern FILE *DBG_fp ;
   extern char *DBG_labels[3] ;
   extern void DBG_sigfunc(int) ;
   extern void DBG_traceback(void) ;
   extern char last_status[1024] ;
#endif /* _DEBUGTRACE_MAIN_ */

#define DBG_SIGNALS ( signal(SIGPIPE,DBG_sigfunc) , \
                      signal(SIGSEGV,DBG_sigfunc) , \
                      signal(SIGINT ,DBG_sigfunc) , \
                      signal(SIGBUS ,DBG_sigfunc) , \
                      signal(SIGTERM,DBG_sigfunc) )
/*---------------------------------------------------------------*/

/* macros for entering and exiting a function */

#define DBG_LEADER_IN  "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
#define DBG_LEADER_OUT "-----------------------------------------------------------"

#define ENTRY(rout) do{ static char *rrr = (rout) ;  DBG_rout[DBG_num++] = rrr ; \
                        if( DBG_trace ){                                        \
                          if( DBG_fp == NULL ) DBG_fp = stdout ;               \
                          fprintf(DBG_fp,                                     \
                                  "%*.*s%s [%d]: {ENTRY (file=%s line=%d)\n", \
                                  DBG_num,DBG_num,DBG_LEADER_IN,rrr,DBG_num,  \
                                  __FILE__ , __LINE__ ) ;                     \
                          MCHECK ; fflush(DBG_fp) ; }                         \
                        last_status[0] = '\0' ;                               \
                    } while(0)

#define DBROUT      DBG_rout[DBG_num-1]

#define DBEXIT      do{ if( DBG_trace ){                                     \
                          if( DBG_fp == NULL ) DBG_fp = stdout ;              \
                          fprintf(DBG_fp,                                      \
                                  "%*.*s%s [%d]: EXIT} (file=%s line=%d)\n",    \
                                  DBG_num,DBG_num,DBG_LEADER_OUT,DBROUT,DBG_num, \
                                  __FILE__ , __LINE__ );                         \
                          MCHECK ; fflush(DBG_fp) ; }                            \
                        DBG_num = (DBG_num>1) ? DBG_num-1 : 1 ;                  \
                        last_status[0] = '\0' ;                                  \
                    } while(0)

/*! This macro is only to be used inside main(). */

#define mainENTRY(rout)                                               \
  do{ char *e=getenv("AFNI_TRACE");                                   \
      if( e != NULL ) DBG_trace = (*e=='y') ? 1 : (*e=='Y') ? 2 : 0 ; \
      e = getenv("AFNI_TRACE_FILE") ;                                 \
      if( e != NULL ) DBG_fp=fopen(e,"w") ;                           \
      if( DBG_fp==NULL ) DBG_fp=stdout;                               \
      DBG_SIGNALS; ENTRY(rout); (void)AFNI_prefilter_args(&argc,argv); } while(0)

#define PRINT_TRACING (DBG_trace > 1)

#define STATUS(str)                                                      \
  do{ if(PRINT_TRACING){                                                  \
        MCHECK ; if( DBG_fp==NULL ) DBG_fp=stdout;                         \
        fprintf(DBG_fp,"%*.*s%s -- %s\n",DBG_num,DBG_num," ",DBROUT,(str)); \
        fflush(DBG_fp) ; }                                                   \
      strncpy(last_status,str,1023); last_status[1023]='\0';                  \
  } while(0)

/*********************************************************************/
#else /* don't USE_TRACING */

#  define ENTRY(rout)   /* nada */
#  define DBEXIT        /* nada */
#  define DBROUT        /* nada */
#  define STATUS(str)   /* nada */
#  define DBG_SIGNALS   /* nada */
#  define MCHECK        /* nada */
#  define MPROBE        /* nada */
#  define TRACEBACK     /* nada */
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

/* these macros are always defined here */

#undef RETURN
#undef EXRETURN
#undef EXIT

#define RETURN(val) do{ DBEXIT    ; return (val) ; } while(0)
#define EXRETURN    do{ DBEXIT    ; return       ; } while(0)
#define EXIT(n)     do{ TRACEBACK ; exit(n)      ; } while(0)

/*---------------------------------------------------------------*/

#ifndef MCHECK
# define MCHECK /* nada */
# define MPROBE /* nada */
#endif

#ifdef  __cplusplus
}
#endif

/*---------------------------------------------------------------*/

#include <stdarg.h>

#ifdef  __cplusplus
extern "C" {
#endif

extern void INFO_message   ( char *fmt , ... ) ;  /* 13 Jul 2005 */
extern void ININFO_message ( char *fmt , ... ) ;
extern void WARNING_message( char *fmt , ... ) ;
extern void ERROR_message  ( char *fmt , ... ) ;
extern void ERROR_exit     ( char *fmt , ... ) ;
extern void SET_message_file( char *fname )    ;  /* 09 Mar 2007 */

#define FATAL_ERROR_message ERROR_exit

#ifdef  __cplusplus
}
#endif

#endif /* _MCW_DEBUGTRACE_ */
