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

#define DBG_MAX_HIST 32       /* 27 Apr 2015 */

/*---------------------------------------------------------------*/

#ifdef DONT_USE_MCW_MALLOC

# define MCHECK /* nada */
# define MPROBE /* nada */

#else

# define MCHECK                                       \
   do{ char *mc = MCW_MALLOC_status ;                 \
       if( DBG_fp==NULL ) DBG_fp=stdout;              \
       if( mc != NULL ){                              \
         fprintf(DBG_fp,"** Memory usage: %s\n",mc);  \
         fflush(DBG_fp) ;                             \
       }                                              \
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
   int DBG_stoff = 0 ;   /* temporarily disable ENTRY/RETURN stack [01 Jun 2009] */
   int DBG_trace = 0 ;   /* turn off at start (cf. mainENTRY) */
   FILE *DBG_fp  = NULL ;    /* 01 Sep 2006 */
   FILE *DBG_tfp = NULL ;    /* 13 Apr 2015 */

   char *DBG_labels[3] = { "Trace=OFF " , "Trace=LOW " , "Trace=HIGH" } ;

   char last_status[1024] = "\0" ;  /* 22 Apr 2002 */

   char **hist_status = NULL ;      /* 27 Apr 2015 */
   int   nhist_status = 0 ;         /* next place to be written */
   int   DBG_hist     = 1 ;         /* whether to track history */

   char *DBG_commandline = NULL ;   /* 10 Mar 2016 */

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

void DBG_setup_hist_status(void)  /* 27 Apr 2015 */
{
   int ii ;
   if( hist_status != NULL ) return ;
   hist_status = (char **)malloc(DBG_MAX_HIST*sizeof(char *)) ;
   for( ii=0 ; ii < DBG_MAX_HIST ; ii++ )
     hist_status[ii] = (char *)calloc(1024,sizeof(char)) ;
   nhist_status = 0 ;
   return ;
}

/*------------------------------------------------------*/

void DBG_dump_hist_status(FILE *hfp)
{
  int ii ;
  if( hist_status == NULL ) return ;
  if( hfp == NULL ) hfp = stderr ;
  fprintf(hfp,".......... recent internal history .........................................\n") ;
  for( ii=nhist_status+1 ; ii < DBG_MAX_HIST ; ii++ )
    if( hist_status[ii][0] != '\0' ) fprintf(hfp,"%s\n",hist_status[ii]) ;
  for( ii=0 ; ii < nhist_status ; ii++ )
    if( hist_status[ii][0] != '\0' ) fprintf(hfp,"%s\n",hist_status[ii]) ;
  fprintf(hfp,"............................................................................\n") ;
  if( DBG_commandline != NULL ){
    fprintf(hfp,"** Command line was:\n%s\n",DBG_commandline) ;
    fprintf(hfp,"............................................................................\n") ;
  }
  return ;
}

/*------------------------------------------------------*/

void DBG_traceback(void)
{ int tt ;
  MCHECK ;
  if( DBG_tfp == NULL ) DBG_tfp = stderr ;
  if( last_status[0] != '\0' )
    fprintf(DBG_tfp,"Last STATUS: %s\n",last_status) ;
  for( tt=DBG_num-1; tt >= 1 ; tt-- )
    fprintf(DBG_tfp,"%*.*s%s\n",tt+1,tt+1," ",DBG_rout[tt]) ;
  if( DBG_commandline != NULL )
    fprintf(DBG_tfp,"** Command line was:\n%s\n",DBG_commandline) ;
}

/*------------------------------------------------------*/

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
     case SIGABRT: sname = "SIGABRT" ; break ;
   }
   fprintf(stderr,"\nFatal Signal %d (%s) received\n",sig,sname) ;
   if( last_status[0] != '\0' )
     fprintf(stderr,"Last STATUS: %s\n",last_status) ;
   if( DBG_num >= 0 ){
     for( ii=DBG_num-1; ii >= 0 ; ii-- )
       fprintf(stderr,"%*.*s%s\n",ii+1,ii+1," ",DBG_rout[ii]) ;
     if( DBG_commandline != NULL )
       fprintf(stderr,"** Command line was:\n%s\n",DBG_commandline) ;
   } else {
     fprintf(stderr,"[No debug tracing stack: DBG_num=%d]\n",DBG_num) ;
     if( DBG_commandline != NULL )
       fprintf(stderr,"** Command line was:\n%s\n",DBG_commandline) ;
   }
#ifdef AFNI_VERSION_LABEL
   fprintf(stderr,"** AFNI version = " AFNI_VERSION_LABEL
                   "  Compile date = " __DATE__ "\n" );
#endif
#ifdef SHSTRING
   fprintf(stderr,"** [[Precompiled binary " SHSTRING ": " __DATE__ "]]\n");
#endif

   fprintf(stderr,"** Program Death **\n") ; fflush(stderr) ;
   if( sig != SIGINT && sig != SIGTERM ){  /* add crashlog [13 Apr 2015] */
     FILE *dfp ; char *home , fname[1024] ;
     fprintf(stderr,"** If you report this crash to the AFNI message board,\n"
                    "** please copy the error messages EXACTLY, and give\n"
                    "** the command line you used to run the program, and\n"
                    "** any other information needed to repeat the problem.\n"
                    "** You may later be asked to upload data to help debug.\n");

     home = getenv("HOME") ;  /* crashlog stuff starts here */
     if( home != NULL ){
       strcpy(fname,home) ; strcat(fname,"/.afni.crashlog") ;
     } else {
       strcpy(fname,".afni.crashlog") ;
     }
     dfp = fopen( fname , "a" ) ;
     if( dfp != NULL ){
       fprintf(dfp,"\n*********------ CRASH LOG ------------------------------***********") ;
       fprintf(dfp,"\nFatal Signal %d (%s) received\n",sig,sname); fflush(stderr);
       if( DBG_hist ) DBG_dump_hist_status(dfp) ;
       DBG_tfp = dfp ; DBG_traceback() ; DBG_tfp = stderr ;
#ifdef AVERZHN
       fprintf(dfp,"** AFNI version = " AVERZHN "  Compile date = " __DATE__ "\n" );
#else
       fprintf(dfp,"** AFNI compile date = " __DATE__ "\n" ) ;
#endif
#ifdef SHSTRING
       fprintf(dfp,"** [[Precompiled binary " SHSTRING ": " __DATE__ "]]\n") ;
#endif
       fprintf(dfp,"** Program Crash **\n") ;
       if( mcw_malloc_enabled() && getenv("AFNI_CRASH_MALLDUMP") != NULL )
         mcw_malloc_dump_fp(dfp) ; /* 23 Apr 2015 */
       fclose(dfp) ;
       fprintf(stderr,"** Crash log is appended to file %s\n",fname) ;
     }
   }
   MPROBE ; exit(1) ;
}

/*---------------------------------------------------------------
   Call out the variables defined in debugtrace.c
-----------------------------------------------------------------*/

#else /* not _DEBUGTRACE_MAIN_ */
   extern char *DBG_rout[DEBUG_MAX_DEPTH] ;
   extern int DBG_num ;
   extern int DBG_stoff ;
   extern int DBG_trace ;
   extern FILE *DBG_fp ;
   extern FILE *DBG_tfp ;
   extern char *DBG_labels[3] ;
   extern void DBG_sigfunc(int) ;
   extern void DBG_traceback(void) ;
   extern char last_status[1024] ;
   extern char **hist_status ;
   extern int   nhist_status ;
   extern int DBG_hist ;
   extern void DBG_setup_hist_status(void) ;          /* 27 Apr 2015 */
   extern void DBG_dump_hist_status(FILE *hfp) ;
   extern char *DBG_commandline ;
#endif /* _DEBUGTRACE_MAIN_ */

#define DBG_SIGNALS ( signal(SIGPIPE,DBG_sigfunc) , \
                      signal(SIGSEGV,DBG_sigfunc) , \
                      signal(SIGINT ,DBG_sigfunc) , \
                      signal(SIGBUS ,DBG_sigfunc) , \
                      signal(SIGTERM,DBG_sigfunc) , \
                      signal(SIGABRT,DBG_sigfunc) )
/*---------------------------------------------------------------*/

#define PRINT_TRACING (DBG_trace > 1 && !DBG_stoff)

#define TRACK_TRACING ( !DBG_stoff && (DBG_hist || DBG_trace > 1) ) /* 27 Apr 2015 */

/*------------------------------------------------------*/

#define DBG_set_hist_status(hss)                         \
 do{ if( hist_status == NULL ) DBG_setup_hist_status() ; \
     strncpy(hist_status[nhist_status],hss,1023) ;       \
     hist_status[nhist_status][1023] = '\0' ;            \
     nhist_status = (nhist_status+1) % DBG_MAX_HIST ;    \
     hist_status[nhist_status][0] = '\0' ;               \
 } while(0)

/*------------------------------------------------------*/

/* macros for entering and exiting a function */

#define DBG_LEADER_IN  "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
#define DBG_LEADER_OUT "-----------------------------------------------------------"

#define ENTRY(rout)                                                                    \
  do{ if( !DBG_stoff ){                                                                \
        static char *rrr=(rout) ; char *ooo=DBG_rout[DBG_num-1] ;                      \
        DBG_rout[DBG_num++] = rrr ;                                                    \
        if( TRACK_TRACING ){                                                           \
          char sbuf[1024] ;                                                            \
          if( DBG_fp == NULL ) DBG_fp = stdout ;                                       \
          sprintf(sbuf,                                                                \
                  "%*.*s%s [%d]: {ENTRY (file=%s line=%d) from %s",                    \
                  DBG_num,DBG_num,DBG_LEADER_IN,rrr,DBG_num,                           \
                  __FILE__ , __LINE__ , ooo ) ;                                        \
          if( PRINT_TRACING ){ fprintf(DBG_fp,"%s\n",sbuf); fflush(DBG_fp); MCHECK ;}  \
          DBG_set_hist_status(sbuf) ;                                                  \
        }                                                                              \
        last_status[0] = '\0' ;                                                        \
  } } while(0)

#define DBROUT      DBG_rout[DBG_num-1]
#define DBROLD      ( (DBG_num > 1) ? DBG_rout[DBG_num-2] : "none" )

#define DBEXIT                                                                       \
  do{ if( !DBG_stoff ){                                                              \
      if( TRACK_TRACING ){                                                           \
        char sbuf[1024] ;                                                            \
        if( DBG_fp == NULL ) DBG_fp = stdout ;                                       \
        sprintf(sbuf,                                                                \
                "%*.*s%s [%d]: EXIT} (file=%s line=%d) to %s",                       \
                DBG_num,DBG_num,DBG_LEADER_OUT,DBROUT,DBG_num,                       \
                __FILE__ , __LINE__ , DBROLD ) ;                                     \
        if( PRINT_TRACING ){ fprintf(DBG_fp,"%s\n",sbuf); fflush(DBG_fp); MCHECK ;}  \
        DBG_set_hist_status(sbuf) ;                                                  \
       }                                                                             \
       DBG_num = (DBG_num>1) ? DBG_num-1 : 1 ;                                       \
       last_status[0] = '\0' ;                                                       \
 } } while(0)

/* prototypes for clock time logging */

extern void set_program_name(char *) ;  /* 03 Nov 2016 */
extern void clock_time_atexit(void) ;

/*! This macro is only to be used inside main(). */

#define mainENTRY(rout)                                               \
  do{ char *e=getenv("AFNI_TRACE");                                   \
      if( e != NULL ) DBG_trace = (*e=='y') ? 1 : (*e=='Y') ? 2 : 0 ; \
      e = getenv("AFNI_TRACE_FILE") ;                                 \
      if( e != NULL ) DBG_fp=fopen(e,"w") ;                           \
      if( DBG_fp==NULL ) DBG_fp=stdout;                               \
      (void)NI_clock_time() ;                                         \
      DBG_SIGNALS; ENTRY(rout); (void)AFNI_prefilter_args(&argc,argv); } while(0)

#define STATUS(str)                                                                  \
  do{ if(TRACK_TRACING){                                                             \
        char sbuf[1024] ;                                                            \
        if( DBG_fp==NULL ) DBG_fp=stdout;                                            \
        sprintf(sbuf,"%*.*s%s -- %s",DBG_num,DBG_num," ",DBROUT,(str));              \
        if( PRINT_TRACING ){ fprintf(DBG_fp,"%s\n",sbuf); fflush(DBG_fp); MCHECK; }  \
        DBG_set_hist_status(sbuf) ;                                                  \
      }                                                                              \
      if(!DBG_stoff){strncpy(last_status,str,1023); last_status[1023]='\0';}         \
  } while(0)

#define STATUSp(str,p)                                                              \
  do{ char qss[768] ;                                                               \
      sprintf(qss,"%s ptr=%p",(str),(p)) ;                                          \
      if( mcw_malloc_enabled() )                                                    \
        strcat(qss, mcw_malloc_OK(p) ? "  OK" : "  not OK") ;                       \
      if( TRACK_TRACING ){                                                          \
        char sbuf[1024] ;                                                           \
        if( DBG_fp==NULL ) DBG_fp=stdout;                                           \
        sprintf(sbuf,"%*.*s%s -- %s",DBG_num,DBG_num," ",DBROUT,qss);               \
        if( PRINT_TRACING ){ fprintf(DBG_fp,"%s\n",sbuf); fflush(DBG_fp); MCHECK;}  \
        DBG_set_hist_status(sbuf) ;                                                 \
      }                                                                             \
      if(!DBG_stoff){strncpy(last_status,qss,1023); last_status[1023]='\0';}        \
  } while(0)

#define STATUSi(str,i)                                                              \
  do{ char qss[768] ;                                                               \
      sprintf(qss,"%s int=%d",(str),(i)) ;                                          \
      if( TRACK_TRACING ){                                                          \
        char sbuf[1024] ;                                                           \
        if( DBG_fp==NULL ) DBG_fp=stdout;                                           \
        sprintf(sbuf,"%*.*s%s -- %s",DBG_num,DBG_num," ",DBROUT,qss);               \
        if( PRINT_TRACING ){ fprintf(DBG_fp,"%s\n",sbuf); fflush(DBG_fp); MCHECK;}  \
        DBG_set_hist_status(sbuf) ;                                                 \
      }                                                                             \
      if(!DBG_stoff){strncpy(last_status,qss,1023); last_status[1023]='\0';}        \
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
#  define STATUSp(str,p) /* nada */
#  define STATUSi(str,i) /* nada */

#  ifdef _DEBUGTRACE_MAIN_
      void DBG_sigfunc(int sig){} /* does nada */
#  else
      extern void DBG_sigfunc(int) ;
#  endif

#  define mainENTRY(rout) \
   do { (void)AFNI_prefilter_args(&argc,argv); } while(0)

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

/** #include <stdarg.h> **/

#ifdef  __cplusplus
extern "C" {
#endif

extern void INFO_message   ( char *fmt , ... ) ;  /* 13 Jul 2005 */
extern void ININFO_message ( char *fmt , ... ) ;
extern void WARNING_message( char *fmt , ... ) ;
extern void ERROR_message  ( char *fmt , ... ) ;
extern void ERROR_exit     ( char *fmt , ... ) ;
extern void SET_message_file( char *fname )    ;  /* 09 Mar 2007 */

extern void   SET_message_outbuf( int use_outbuf ) ; /* 01 May 2015 */
extern char * GET_message_outbuf(void) ;
extern char * THD_zzprintf( char * sss , char * fmt , ... ) ;

#define FATAL_ERROR_message ERROR_exit

#ifdef  __cplusplus
}
#endif

#endif /* _MCW_DEBUGTRACE_ */
