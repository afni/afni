#ifndef _MCW_DEBUGTRACE_
#define _MCW_DEBUGTRACE_

/** inputs:
     USE_TRACING   ==> if set, include tracing information in AFNI
     PRINT_TRACING ==> if also set, prints as it goes (otherwise, no print)
     MALLOC_TRACE  ==> if also set, enables GNU malloc and malloc debugging **/

/*********************************************************************/
#ifdef USE_TRACING

#define DEBUG_MAX_DEPTH 1024  /* way too big, but who cares? */

/*---------------------------------------------------------------*/
#if defined(MALLOC_TRACE) && !defined(USE_GNU_MALLOC)
#  define USE_GNU_MALLOC
#endif
/*---------------------------------------------------------------*/

/*---------------------------------------------------------------*/
#ifdef MALLOC_TRACE
#  define malloc  malloc_track
#  define realloc realloc_track
#  define calloc  calloc_track
#  define free    free_track
#  define TRMOUT  printf("check_malloc: %s\n",check_malloc())
#  define TRMNOT  check_malloc()
#  define MCHECK  TRMOUT

  extern int    ptr_tracker( void * fred ) ;
  extern int    nul_tracker(void) ;
  extern void   add_tracker( void * fred , size_t n ) ;
  extern void * malloc_track( size_t n ) ;
  extern void * realloc_track( void * fred , size_t n ) ;
  extern void * calloc_track( size_t n , size_t m ) ;
  extern void   free_track( void * fred ) ;
  extern char * check_malloc(void) ;
#else
#  define TRMOUT 0
#  define TRMNOT 0
#endif /* MALLOC_TRACE */
/*---------------------------------------------------------------*/

/*---------------------------------------------------------------*/
#ifdef MAIN
   char * DBG_rout[DEBUG_MAX_DEPTH] = { "Bottom of Debug Stack" } ;
   int DBG_num = 1 ;

#  ifdef PRINT_TRACING
     int DBG_trace = 1 ;   /* turn on from start */
#  else
     int DBG_trace = 0 ;   /* turn off at start */
#  endif

#include <signal.h>
void DBG_sigfunc(int sig)   /** signal handler for fatal errors **/
{
   char * sname ; int ii ;
   switch(sig){
      default:      sname = "unknown" ; break ;
      case -1:      sname = "malloc"  ; break ;
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
   exit(1) ;
}
#define DBG_SIGNALS ( signal(SIGPIPE,DBG_sigfunc) , \
                      signal(SIGSEGV,DBG_sigfunc) , \
                      signal(SIGINT ,DBG_sigfunc) , \
                      signal(SIGBUS ,DBG_sigfunc)  )
/*---------------------------------------------------------------*/
#else /* not MAIN */
   extern char * DBG_rout[DEBUG_MAX_DEPTH] ;
   extern int DBG_num ;
   extern int DBG_trace ;
#endif /* MAIN */
/*---------------------------------------------------------------*/

/* macros for entering and exiting a function */

#define DBG_LEADER_IN  "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
#define DBG_LEADER_OUT "-----------------------------------------------------------"

#define ENTRY(rout) do{ static char * rrr = (rout) ;  DBG_rout[DBG_num++] = rrr ; \
                        if( DBG_trace ){                                         \
                          printf("%*.*s%s [%d]: ENTRY (file=%s line=%d)\n",     \
                                 DBG_num,DBG_num,DBG_LEADER_IN,rrr,DBG_num,    \
                                 __FILE__ , __LINE__ ) ;                      \
                          TRMOUT ; fflush(stdout) ; } } while(0)

#define DBROUT      DBG_rout[DBG_num-1]

#define DBEXIT      do{ if( DBG_trace ){                                      \
                          printf("%*.*s%s [%d]: EXIT (file=%s line=%d)\n",     \
                                 DBG_num,DBG_num,DBG_LEADER_OUT,DBROUT,DBG_num, \
                                 __FILE__ , __LINE__ );                        \
                          TRMOUT ; fflush(stdout) ; }                         \
                        DBG_num = (DBG_num>1) ? DBG_num-1 : 1 ; } while(0)

/*---------------------------------------------------------------*/
#ifdef PRINT_TRACING
# define STATUS(str) \
     (printf("%*.*s%s -- %s\n",DBG_num,DBG_num," ",DBROUT,(str)),fflush(stdout))
#else
# define STATUS(str) /* nada */
#endif
/*---------------------------------------------------------------*/

/*********************************************************************/
#else /* don't USE_TRACING */

#  define ENTRY(rout)
#  define DBEXIT
#  define DBROUT
#  define STATUS(str)
#  define DBG_SIGNALS
#  define TRMOUT
#  define TRMNOT

#endif /* USE_TRACING */
/*********************************************************************/

#define RETURN(val) do{ DBEXIT ; return (val) ; } while(0)
#define EXRETURN    do{ DBEXIT ; return ; } while(0)

/*---------------------------------------------------------------*/
#ifdef USE_GNU_MALLOC
#  undef XtMalloc
#  undef XtRealloc
#  undef XtCalloc
#  undef XtFree

#  define XtMalloc  malloc
#  define XtRealloc realloc
#  define XtCalloc  calloc
#  define XtFree    free
#endif

#ifndef MCHECK
#define MCHECK /* nada */
#endif
/*---------------------------------------------------------------*/

#endif /* _MCW_DEBUGTRACE_ */
