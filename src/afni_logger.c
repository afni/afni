#include "mrilib.h"

#include "debugtrace.h"

#include <sys/file.h>

/******************************************************************************
  LOCK_file  (FILE *) returns 0 if OK to proceed, -1 if file is already locked
  UNLOCK_file(FILE *) return value is ignored
*******************************************************************************/

#if defined(USE_FLOCK)
#  define LOCK_file(fp)   flock(fileno(fp),LOCK_EX|LOCK_NB)
#  define UNLOCK_file(fp) flock(fileno(fp),LOCK_UN)
#elif defined(USE_LOCKF)
#  define LOCK_file(fp)   lockf(fileno(fp),F_TLOCK,0)
#  define UNLOCK_file(fp) lockf(fileno(fp),F_ULOCK,0)
#else
#  define LOCK_file(fp)   0
#  define UNLOCK_file(fp) 0
#endif

#undef  LOGFILE
#define LOGFILE ".afni.log"

/*------------------------------------------------------------------*/

void AFNI_sleep( int msec )
{
   struct timeval tv ;
   if( msec <= 0 ) return ;
   tv.tv_sec  = msec/1000 ;
   tv.tv_usec = (msec%1000)*1000 ;
   select( 1 , NULL,NULL,NULL , &tv ) ;
   return ;
}

/*---------------------------------------------------------------------
  Log command to a file; return -1 if fails, 0 if good -- 13 Aug 2001
-----------------------------------------------------------------------*/

int AFNI_logger( char *pname , int argc , char **argv )
{
   char *cline, *cdate , *eh , *fn , *logfile=LOGFILE , *eee ;
   FILE *fp ;
   int ll ; unsigned long fll ;

   if( pname == NULL || pname[0] == '\0' ) return -1 ;
   eh = getenv("HOME") ; if( eh == NULL )  return -1 ;
   if( AFNI_yesenv("AFNI_DONT_LOGFILE") )  return -1 ;
   if( argc > 1 ) cline = tross_commandline( pname , argc , argv ) ;
   else           cline = strdup(pname) ;
   if( cline == NULL ) return -1 ;
   DBG_commandline = strdup(cline) ; /* 10 Mar 2016 */
   cdate = tross_datetime() ;
   fn = AFMALL(char,  strlen(eh)+strlen(logfile)+8) ;
   strcpy(fn,eh) ; strcat(fn,"/") ; strcat(fn,logfile) ;
   if( (fll=THD_filesize(fn)) > 100000000 )   /* 01 Jun 2005: for Kevin */
     fprintf(stderr,"++ WARNING: file %s is now %lu (%s) bytes long!\n",
             fn, fll, approximate_number_string((double)fll) ) ;
   fp = fopen(fn,"a") ;
   if( fp == NULL ){ free(fn); free(cdate); free(cline); return -1; }
   ll = LOCK_file(fp) ;
   if( ll ){
#if 0
     fprintf(stderr,"%s: LOCKED\n",cline);
#endif
     ll = strlen(pname) ; if( ll > 11 ) ll = 11 ;
     AFNI_sleep(ll) ; ll = LOCK_file(fp) ;
     if( ll ){ fclose(fp); free(fn); free(cdate); free(cline); return -1; }
   }
   fseek(fp,0,SEEK_END) ;
   fprintf(fp,"[%s] %s\n",cdate,cline) ;
   UNLOCK_file(fp) ; fclose(fp) ;
   eee = getenv("AFNI_ECHO_COMMANDLINE") ;
   if( eee != NULL && (*eee == 'Y' || *eee == 'y') )  /* 13 Dec 2010 */
     fprintf(stderr,"+++++===== %s\n",cline) ;
   free(fn); free(cdate); free(cline) ; return 0;
}

/*-------------------------------------------------------------------------*/

long long AFNI_logfilesize(void)
{
   char *eh , *fn ;  long long fs ;

   eh = getenv("HOME") ; if( eh == NULL )  return 0 ;
   fn = (char *)malloc(sizeof(char)*(strlen(eh)+strlen(LOGFILE)+8)) ;
   strcpy(fn,eh) ; strcat(fn,"/") ; strcat(fn,LOGFILE) ;
   fs = THD_filesize(fn); free((void *)fn); return fs;
}
