#include "afni.h"

/* 27 Jan 2003: print a message if version check is disabled */

#define AFNI_WGET   "wget -nv -m -np -nH --cut-dirs=3 -P %s "          \
                    "http://afni.nimh.nih.gov/AFNI/bin/%s/"

#define AFNI_LATEST "http://afni.nimh.nih.gov/afni/afni_latest.shtml"

#define FAIL_MESSAGE(reason)                                           \
 do{                                                                   \
   fprintf(stderr,"\n"                                                 \
                  "\n** Version check disabled: %s"                    \
                  "\n**  %s"                                           \
                  "\n** has info about latest AFNI news.\n" ,          \
           reason , AFNI_LATEST ) ;                                    \
   disabled = 1 ; } while(0)

static int disabled = 0 ;

/**************************************************************************/
/***************  if don't have shared memory, do almost nothing **********/
#ifdef DONT_USE_SHM /******************************************************/

void AFNI_start_version_check(void){
  FAIL_MESSAGE("system doesn't support shared memory") ;  /* 27 Jan 2003 */
  return;
}

int AFNI_version_check(void){ return 0; }

/**************************************************************************/
/********* if have shared memory, can communicate with child process ******/
#else /********************************************************************/

#define VERSION_URL "http://afni.nimh.nih.gov/afni/AFNI.version"
#if 0
#define SHM_CHILD   "shm:afni_vcheck:1K"
#else
#define SHM_CHILD   "tcp:localhost:20279"
#endif
#define AFNI_HOST   "http://afni.nimh.nih.gov/afni/"
#define VSIZE       1024

static pid_t vc_child_pid = (pid_t)(-1) ;
static IOCHAN *vc_ioc     = NULL ;

/*------------------------------------------------------------------------*/
/*!  This is only called from within the child process. */

static void vexit( int sig )
{
   static volatile int fff=0 ;
   if( fff ) _exit(1) ; else fff = 1 ;
   fprintf(stderr,"** Version Check: fails to complete **\n");
   _exit(1);
}

/*------------------------------------------------------------------------*/
/*! This is called at main AFNI process exit,
    to destroy vc_ioc if it is still open.
--------------------------------------------------------------------------*/

static void vc_exit(void){ iochan_close(vc_ioc) ; }  /* 12 Dec 2002 */

/*------------------------------------------------------------------------*/
/*! Start the Web fetch to check the AFNI version in a child process.
    To complete things, call AFNI_version_check() somewhat later.
--------------------------------------------------------------------------*/

void AFNI_start_version_check(void)
{
   pid_t child_pid ;

#ifdef CYGWIN  /* 18 Dec 2002 */
   FAIL_MESSAGE("not possible under Cygwin") ;
   return ;
#else

   /*-- decide if we are to do anything --*/

   if( AFNI_noenv("AFNI_VERSION_CHECK") ){    /* never check */
     FAIL_MESSAGE("AFNI_VERSION_CHECK forbids") ;
     return ;
   }

   /* check if we did this in the last 12 hours */

   { char *home=getenv("HOME") , mname[VSIZE]="file:" ;
     NI_stream ns ;
     if( home != NULL ) strcat(mname,home) ;
     strcat(mname,"/.afni.vctime") ;
     ns = NI_stream_open( mname , "r" ) ;
     if( ns != NULL ){
       NI_element *nel = NI_read_element(ns,22) ;
       NI_stream_close(ns) ;
       if( nel != NULL ){
         char *rhs ;
         rhs = NI_get_attribute( nel , "version_check_time" ) ;
         if( rhs != NULL ){
           int last_time = strtol(rhs,NULL,10) ;
           int dtime = ((int)time(NULL)) - last_time ;
           if( dtime >= 0 && dtime < 429999 ){ /* don't check */
             disabled = 1 ; return ;
           }
         }
         rhs = NI_get_attribute(nel,"version_string") ;  /* 27 Jan 2003 */
         if( rhs != NULL && strcmp(rhs,VERSION) != 0 ){
           fprintf(stderr,
                   "\n** Your AFNI version changed from %s to %s since last check\n",
                   rhs , VERSION ) ;
         }
         NI_free_element(nel) ;
       }
     }
   }

   /*-- OK, start the child process --*/

   child_pid = fork() ;
   if( child_pid == (pid_t)(-1) ){  /* bad */
     FAIL_MESSAGE("can't fork") ;
     return ;
   }

   if( child_pid > 0 ){                     /* I'm the parent */

     /*-- save PID of child for later use --*/

     vc_child_pid = child_pid ;

     /*-- open a shared mem segment to talk to child --*/

     vc_ioc = iochan_init( SHM_CHILD , "accept" ) ;
     if( vc_ioc == NULL ){
       kill(child_pid,SIGTERM) ;            /* cf. Abraham and Isaac */
       vc_child_pid = (pid_t)(-1) ;
       FAIL_MESSAGE("can't open shared memory with child") ;
     } else {
       fprintf(stderr,"\n** Version check: " VERSION_URL "\n" ); /* 13 Jan 2003 */
       atexit( vc_exit ) ;                                       /* 12 Dec 2002 */
     }
     return ;

   } else {                                 /* I'm the child */
                                            /* (never returns) */
     int nbuf=0 , jj ;
     char *vbuf=NULL ;
     IOCHAN *ioc ;

     iochan_enable_perror(0) ;   /* don't print TCP/IP error messages */
     signal( SIGTERM , vexit ) ; /* if parent kills us, call vexit()  */

     /*-- get information from the AFNI server --*/

     nbuf = read_URL( VERSION_URL , &vbuf ) ;  /* may take a while */

     /*-- if this failed, quit --*/

     if( nbuf <= 0 || vbuf == NULL || vbuf[0] == '\0' ) vexit(1);

     /*-- talk to parent process thru shared memory --*/

     ioc = iochan_init( SHM_CHILD , "create" ) ;
     if( ioc == NULL )                                  vexit(1);

     /*-- wait until ioc is ready for writing --*/

     jj = iochan_writecheck(ioc,-1) ;
     if( jj < 0 )                                       vexit(1);

     /*-- send the info in vbuf --*/

     iochan_sendall( ioc , vbuf , nbuf ) ;
     while( ! iochan_clearcheck(ioc,10) )  /* loop until cleared */
       iochan_sleep(10) ;                  /* by parent process  */

     iochan_sleep(10); /* a little extra napping */     _exit(0);
   }
#endif  /* not CYGWIN */
}

/******************************************/
/* 27 Jan 2003: re-enable the KAPUT macro */

#if 1
# define KAPUT(ss)                                          \
  do{ fprintf(stderr,"** Version Check fails: %s **\n",ss); \
      return ; } while(0)
#else
# define KAPUT(ss) return
#endif

/*------------------------------------------------------------------------*/
/*! Complete the version check by seeing if the child process
    has any data to report from the AFNI web site.  Returns 1 if the
    version at the AFNI site doesn't match the version of this program;
    returns 0 if they match, or it can't tell.  Also prints stuff out.
--------------------------------------------------------------------------*/

int AFNI_version_check(void)
{
   int jj , nbuf=0 , ii ;
   char *vbuf=NULL ;
   char vv[128]="none" ,
        r1[128]="none" , r2[128]="\0" , r3[128] ="\0" ;

   /* if something is rotten, then toss it out */

   if( disabled ) return 0 ;   /* 27 Jan 2003 */

   if( vc_ioc == NULL || vc_child_pid == (pid_t)(-1) ) KAPUT("bad child state");

   jj = kill(vc_child_pid,0) ;                /* is child alive? */
   if( jj < 0 ){
     IOCHAN_CLOSE(vc_ioc); vc_child_pid=(pid_t)(-1);
     KAPUT("child not alive");
   }

   jj = iochan_readcheck( vc_ioc , 333 ) ;    /* is iochan open yet? */
   if( jj <= 0 ){
     IOCHAN_CLOSE(vc_ioc); kill(vc_child_pid,SIGTERM); vc_child_pid=(pid_t)(-1);
     KAPUT("bad shmem to child");
   }

   /* if here, have data ready to read from child! */

   nbuf = 0 ;
   vbuf = AFMALL( char, VSIZE) ;
   while(1){
     jj = iochan_recv( vc_ioc , vbuf+nbuf , VSIZE-nbuf ) ;
     if( jj < 1 ) break ;
     nbuf += jj ;
     if( nbuf >= VSIZE-1 ) break ;
     jj = iochan_readcheck( vc_ioc , 5 ) ;
     if( jj < 1 ) break ;
   }

   /* now wait for child to kill itself */

   waitpid(vc_child_pid,NULL,WNOHANG); vc_child_pid = (pid_t)(-1);
   IOCHAN_CLOSE(vc_ioc);

   /* no data? */

   if( nbuf <= 0 ){ free(vbuf); vbuf = NULL; KAPUT("bad version data"); } /* unlikely */

   /* extract version and data/time strings from data */

   sscanf( vbuf , "%127s %127s %127s %127s" , vv,r1,r2,r3 ); free(vbuf);

   /* record the current time, so we don't check too often */

   { char *home=getenv("HOME") , mname[VSIZE]="file:" ;
     NI_stream ns ;
     if( home != NULL ) strcat(mname,home) ;
     strcat(mname,"/.afni.vctime") ;
     ns = NI_stream_open( mname , "w" ) ;
     if( ns != NULL ){
       NI_element *nel=NI_new_data_element("AFNI_vctime",0); char rhs[32];
       sprintf(rhs,"%d",(int)time(NULL)) ;
       NI_set_attribute( nel , "version_check_time" , rhs ) ;
       if( strcmp(vv,"none") != 0 )                            /* 27 Jan 2003 */
         NI_set_attribute( nel , "version_string" , VERSION ) ;
       NI_write_element( ns , nel , NI_TEXT_MODE ) ;
       NI_stream_close(ns) ;
     }
   }

   /* compare version strings */

   ii = strlen(vv) ; jj = strlen(VERSION) ; jj = MIN(ii,jj) ;
   if( strncmp(vv,VERSION,jj) == 0 ){                /* versions match */
     fprintf(stderr,"\n** Version check: you are up-to-date!\n"
                      "** To disable version check:\n"
                      "** set environment variable AFNI_VERSION_CHECK to NO\n");
     return 0 ;
   }

   /* print a message about version mismatch */

   fprintf(stderr, "\n"
                   "****************************************************\n"
                   " This AFNI was compiled with the following settings:\n"
                   "   Version ID   = %s\n"
                   "   Release date = %s\n"
                   " Latest version at %s\n"
                   "   Version ID   = %s\n"
                   "   Release date = %s %s %s\n"
                   "****************************************************\n"
                   "  %s\n"
                   " has information about features in the latest\n"
                   " releases of the AFNI software package\n"
                   "****************************************************\n"
                   " To disable version check:\n"
                   " set environment variable AFNI_VERSION_CHECK to NO\n"
                   "****************************************************\n"
          , VERSION,RELEASE , AFNI_HOST , vv,r1,r2,r3 , AFNI_LATEST ) ;

   /* 18 Feb 2003: Update info for precompiled binaries */

#ifdef SHOWOFF
#undef SHSH
#undef SHSHSH
#define SHSH(x)   #x
#define SHSHSH(x) SHSH(x)
   { char *bname = SHSHSH(SHOWOFF) ;
     char *aname = THD_find_executable("afni") ;
     if( aname == NULL ) aname = "/your/afni/binary/directory" ;
     fprintf(stderr,
                   " You can download current binaries using\n"
                   AFNI_WGET "\n"
                   "****************************************************\n" ,
             aname , bname ) ;
   }
#undef SHSH
#undef SHSHSH
#endif /* SHOWOFF */

   return 1 ;
}
#endif /* DONT_USE_SHM */

/*----------------------------------------------------------------------*/
/***---------------- 20 Nov 2003: auto-download stuff ----------------***/

#ifdef SHOWOFF
# undef SHSH
# undef SHSHSH
# define SHSH(x)   #x
# define SHSHSH(x) SHSH(x)

# define SNAME "UPDATER"   /* script file name */

char * AFNI_make_update_script(void)
{
   char *pg_ftp , *pg_afni , *pg_gzip , *pg_tar ;
   char hbuf[4096], fname[128], adir[4096], *cpt, cwd[4096] ;
   FILE *fp ;
   static char sname[4096] ;

   pg_ftp  = THD_find_executable("ftp" ) ; if( pg_ftp  == NULL ) return NULL;
   pg_afni = THD_find_executable("afni") ; if( pg_afni == NULL ) return NULL;
   pg_gzip = THD_find_executable("gzip") ; if( pg_gzip == NULL ) return NULL;
   pg_tar  = THD_find_executable("tar" ) ; if( pg_tar  == NULL ) return NULL;

   strcpy(adir,pg_afni) ;                /* extract name of AFNI directory */
   cpt = THD_trailname(adir,0) ;
   *cpt = '\0' ;
   if( strlen(adir) <= 0 ) return NULL;    /* no AFNI directory? */

   strcpy( cwd , adir ) ;                /* try to write a test file there */
   strcat( cwd , "qwerty" ) ;
   fp = fopen( cwd , "a" ) ;
   if( fp == NULL ) return NULL;           /* can't write to AFNI directory? */
   fclose(fp) ; remove(cwd) ;

   getcwd( cwd , 4096 ) ;   /* get current directory for later use */
   chdir( adir ) ;          /* switch to AFNI directory for this work */

   /* write a script to get and install the binary archive via FTP */

   gethostname( hbuf , 4096 ) ;
   strcpy( fname , SHSHSH(SHOWOFF) ) ; strcat( fname , ".tgz" ) ;
   fp = fopen( SNAME , "w" ) ; if( fp == NULL ){ chdir(cwd); return NULL; }
   fprintf( fp ,
            "#!/bin/sh\n"
            "%s -n afni.nimh.nih.gov << EEEEE\n"   /* FTP to get file */
            "user anonymous AFNI@%s\n"
            "binary\n"
            "cd AFNI/tgz\n"
            "get %s\n"
            "bye\n"
            "EEEEE\n"
            "%s -dc %s | %s xf -\n"      /* uncompress and untar file */
            "/bin/rm -f %s\n"            /* delete file */
            "/bin/mv -f %s/* .\n"        /* move files up to here */
            "/bin/rm -rf %s\n"           /* remove directory leftover */
            "exit\n" ,
            pg_ftp ,   /* FTP program */
            hbuf ,     /* hostname, for FTP password */
            fname ,    /* filename to get */
            pg_gzip, fname, pg_tar, /* GZIP program, filename , TAR program */
            fname ,                 /* filename to delete */
            SHSHSH(SHOWOFF) ,       /* directory to copy up */
            SHSHSH(SHOWOFF)         /* directory to delete */
          ) ;
   fclose( fp ) ;
   chmod( SNAME , S_IRUSR | S_IWUSR | S_IXUSR ) ;

   chdir(cwd) ;
   sprintf(sname,"%s%s",adir,SNAME) ; return sname ;
}

#else /* undefined SHOWOFF */
char * AFNI_make_update_script(void){ return NULL; }
#endif /* SHOWOFF */
