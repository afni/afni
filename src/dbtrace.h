#ifndef _MCW_DEBUGTRACE_
#define _MCW_DEBUGTRACE_

/** inputs:
     USE_TRACING   ==> if set, include tracing information in AFNI
     PRINT_TRACING ==> if also set, prints as it goes (otherwise, no print)
     MALLOC_TRACE  ==> if also set, enables GNU malloc and malloc debugging **/

/*********************************************************************/
#ifdef USE_TRACING

#define DEBUG_MAX_DEPTH 128
#define DEBUG_MAX_ROUT  128

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
   volatile int DBG_num = 1 ;

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
   }
   fprintf(stderr,"\nFatal Signal %d (%s) received\n",sig,sname) ;
   if( DBG_num > 0 ){
      for( ii=DBG_num-1; ii >= 0 ; ii-- )
         fprintf(stderr,"%*.*s%s\n",ii+1,ii+1," ",DBG_rout[ii]) ;
   } else {
      fprintf(stderr,"[No debug tracing stack]\n") ;
   }
   fprintf(stderr,"*** Program Abort ***\n") ; fflush(stderr) ;
   exit(1) ;
}
#define DBG_SIGNALS ( signal(SIGPIPE,DBG_sigfunc) , \
                      signal(SIGSEGV,DBG_sigfunc) , \
                      signal(SIGBUS ,DBG_sigfunc)  )
/*---------------------------------------------------------------*/
#else /* not MAIN */
   extern char * DBG_rout[DEBUG_MAX_DEPTH] ;
   extern volatile int  DBG_num ;
#endif /* MAIN */
/*---------------------------------------------------------------*/

/*---------------------------------------------------------------*/
#ifdef PRINT_TRACING
#  define DBG_LEADER_IN  "+++++++++++++++++++++++++++++++++++++++++++++++"
#  define DBG_LEADER_OUT "-----------------------------------------------"

#  define ENTRY(rout) ( DBG_rout[DBG_num++] = (rout) , \
                        printf("%*.*s%s -- ENTRY\n",DBG_num,DBG_num,DBG_LEADER_IN,(rout) ) , \
                        TRMOUT , fflush(stdout) )

#  define ROUT        (DBG_rout[DBG_num-1])

#  define EXIT        ( printf("%*.*s%s -- EXIT\n",DBG_num,DBG_num,DBG_LEADER_OUT,ROUT) , \
                        TRMOUT , fflush(stdout) , DBG_num-- )

#  define STATUS(str) (printf("%*.*s%s -- %s\n",DBG_num,DBG_num," ",ROUT,(str)),fflush(stdout))

/*---------------------------------------------------------------*/
#else /* don't PRINT_TRACING */

#  define ENTRY(rout) ( DBG_rout[DBG_num++] = (rout) , TRMNOT )
#  define ROUT
#  define EXIT        ( TRMNOT , DBG_num-- )
#  define STATUS(str) 

#endif /* PRINT_TRACING */
/*---------------------------------------------------------------*/

/*********************************************************************/
#else /* don't USE_TRACING */

#  define ENTRY(rout)
#  define EXIT
#  define ROUT
#  define STATUS(str)
#  define DBG_SIGNALS
#  define TRMOUT
#  define TRMNOT

#endif /* USE_TRACING */
/*********************************************************************/

#define RETURN(val) do{ EXIT ; return (val) ; } while(0)
#define EXRETURN    do{ EXIT ; return ; } while(0)

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
