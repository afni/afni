#include "afni.h"
#include <sys/utsname.h>

#define FAIL_MESSAGE(reason)                           \
 do{ fprintf(stderr,"\n"                               \
                    "\n** Version check disabled: %s", \
             reason ) ;                                \
     disabled = 1 ; } while(0)

static int disabled = 0 ;

#define VERSION_URL "http://afni.nimh.nih.gov/afni/AFNI.version"
#define STR_CHILD   "tcp:localhost:20279"
#define AFNI_HOST   "http://afni.nimh.nih.gov/afni/"
#define VSIZE       1024

static pid_t vc_child_pid = (pid_t)(-1) ;
static IOCHAN *vc_ioc     = NULL ;

#undef VERBOSE   /* print messages on failure? */

/*------------------------------------------------------------------------*/
/*!  This is only called from within the child process. */

static void vexit( int sig )
{
#ifdef VERBOSE
   static volatile int fff=0 ;
   if( fff ) _exit(1) ; else fff = 1 ;
   fprintf(stderr,"** Version Check: child fails to complete: %d **\n",sig);
#endif
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

#undef  VDELAY
#define VDELAY 429999  /* 429999 s = 5 days */
   /* check if we did this in the last VDELAY seconds */

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
           if( dtime >= 0 && dtime < VDELAY ){ /* don't check */
             NI_free_element(nel) ; disabled = 1 ; return ;
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

     vc_ioc = iochan_init( STR_CHILD , "accept" ) ;
     if( vc_ioc == NULL ){
       kill(child_pid,SIGTERM) ;            /* cf. Abraham and Isaac */
       vc_child_pid = (pid_t)(-1) ;
       FAIL_MESSAGE("can't open connection to child") ;
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
     struct utsname ubuf ;
     char ua[512] ;

     iochan_enable_perror(0) ;   /* don't print TCP/IP error messages */
     signal( SIGTERM , vexit ) ; /* if parent kills us, call vexit()  */

     /*-- get information from the AFNI server --*/

#define USE_HTTP_10

#ifdef USE_HTTP_10
#  undef PCLAB
#  ifdef SHOWOFF
#    undef SHSH
#    undef SHSHSH
#    define SHSH(x)   #x
#    define SHSHSH(x) SHSH(x)
#    define PCLAB     SHSHSH(SHOWOFF)
#  else
#    define PCLAB     "Unknown"
#  endif
#endif

     /** 25 Mar 2005: send more info in the request header **/

#ifdef USE_HTTP_10
     jj = uname( &ubuf ) ;
     if( jj == 0 )
       sprintf( ua ,
               "afni (avers='%s'; prec='%s' node='%s'; sys='%s'; mach='%s')" ,
                VERSION, PCLAB, ubuf.nodename, ubuf.sysname, ubuf.machine   ) ;
     else
       sprintf( ua , "afni (avers='%s'; prec='%s')" , VERSION , PCLAB ) ;

     set_HTTP_10( 1 ) ;
     set_HTTP_user_agent( ua ) ;
#else
     set_HTTP_10( 0 ) ;
#endif

     /* send the request */

     nbuf = read_URL( VERSION_URL , &vbuf ) ;  /* may take a while */

     set_HTTP_10( 0 ) ;

     /*-- if this failed, quit --*/

     if( nbuf <= 0 || vbuf == NULL || vbuf[0] == '\0' ) vexit(1);

     /*-- talk to parent process thru shared memory --*/

     ioc = iochan_init( STR_CHILD , "create" ) ;
     if( ioc == NULL )                                  vexit(2);

     /*-- wait until ioc is ready for writing --*/

     jj = iochan_writecheck(ioc,-1) ;
     if( jj < 0 )                                       vexit(3);

     /*-- send the info in vbuf --*/

     iochan_sendall( ioc , vbuf , nbuf ) ;
     while( ! iochan_clearcheck(ioc,10) )  /* loop until cleared */
       iochan_sleep(10) ;                  /* by parent process  */

     iochan_sleep(10); /* a little extra napping */     _exit(0);
   }
#endif  /* not CYGWIN */
}

/******************************************/

#ifdef VERBOSE
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
   int jj , nbuf=0 ;
   char *vbuf=NULL ;
   char vv[128]="none" ;
   char *sname , *vvbuf ;

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
     KAPUT("connection to child gone bad");
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

#ifdef USE_HTTP_10
   vvbuf = strstr(vbuf,"\r\n\r\n") ;
   if( vvbuf == NULL ) vvbuf = vbuf      ;
   else                vvbuf = vvbuf + 4 ;
#else
   vvbuf = vbuf ;
#endif

   sscanf( vvbuf , "%127s" , vv ); free(vbuf);

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

   if( strcmp(vv,VERSION) == 0 ){                    /* versions match */
     fprintf(stderr,"\n** Version check: you are up-to-date!\n"
                      "** To disable future version checks:\n"
                      "** set environment variable AFNI_VERSION_CHECK to NO.\n"
                      "** Version checks are done about every %.1f days.\n",
             rint(VDELAY/86400.0) ) ;
     return 0 ;
   }

   /* print a message about version mismatch */

   fprintf(stderr, "\n"
                   "****************************************************\n"
                   " This AFNI was compiled with the following settings:\n"
                   "   Version ID   = %s\n"
                   " Latest version at %s\n"
                   "   Version ID   = %s\n"
                   "****************************************************\n"
                   " To disable future version checks:\n"
                   " set environment variable AFNI_VERSION_CHECK to NO\n"
                   "****************************************************\n"
          , VERSION, AFNI_HOST , vv ) ;

   return 1 ;
}

/*----------------------------------------------------------------------*/
/***---------------- 20 Nov 2003: auto-download stuff ----------------***/

#ifdef SHOWOFF
# undef SHSH
# undef SHSHSH
# define SHSH(x)   #x
# define SHSHSH(x) SHSH(x)

# define SNAME "AFNI_UPDATER"   /* script file name */

char * AFNI_make_update_script(void)
{
   char *pg_ftp , *pg_afni , *pg_gzip , *pg_tar ;
   char hbuf[4096], fname[128], adir[4096], *cpt, cwd[4096] ;
   FILE *fp ;
   static char sname[4096] ;

   pg_ftp  = THD_find_executable("ftp" ); if( pg_ftp  == NULL ) return NULL;
   pg_afni = THD_find_executable("afni"); if( pg_afni == NULL ) return NULL;
   pg_gzip = THD_find_executable("gzip"); if( pg_gzip == NULL ) return NULL;
   pg_tar  = THD_find_executable("tar" ); if( pg_tar  == NULL ) return NULL;

   strcpy(adir,pg_afni) ;                /* extract name of AFNI directory */
   cpt = THD_trailname(adir,0) ;
   *cpt = '\0' ;
   if( strlen(adir) <= 0 ) return NULL ; /* no AFNI directory? */

   strcpy( cwd , adir ) ;                /* try to write a test file there */
   strcat( cwd , "afni_qadgop" ) ;
   fp = fopen( cwd , "a" ) ;
   if( fp == NULL ) return NULL ;        /* can't write to AFNI directory? */
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
            "user anonymous AFNI_UPDATER@%s\n"
            "binary\n"
            "cd tgz\n"
            "get %s\n"
            "bye\n"
            "EEEEE\n"
            "%s -dc %s | %s xf -\n"      /* uncompress and untar .tgz file */
            "/bin/rm -f %s\n"            /* delete .tgz file */
            "/bin/mv -f %s/* .\n"        /* move untar-ed files up to here */
            "/bin/rm -rf %s\n"           /* remove any directory leftovers */
            "exit\n" ,
            pg_ftp ,                /* FTP program */
            hbuf ,                  /* hostname, for FTP password */
            fname ,                 /* filename to get */
            pg_gzip, fname, pg_tar, /* GZIP program, filename, TAR program */
            fname ,                 /* filename to delete */
            SHSHSH(SHOWOFF) ,       /* directory to copy up */
            SHSHSH(SHOWOFF)         /* directory to delete */
          ) ;
   fclose( fp ) ;
   chmod( SNAME , S_IRUSR | S_IWUSR | S_IXUSR ) ; /* mark as executable */

   /* get back to current working directory, then leave */

   chdir(cwd) ;
   sprintf(sname,"%s%s",adir,SNAME) ; return sname ;
}

#else /* undefined SHOWOFF */
char * AFNI_make_update_script(void){ return NULL; }
#endif /* SHOWOFF */
