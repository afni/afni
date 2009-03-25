#include "afni.h"
#include <sys/utsname.h>

#define VERSION_URL  "http://afni.nimh.nih.gov/pub/dist/AFNI.version"
#define VERSION_FILE "/Volumes/afni/var/www/html/pub/dist/AFNI.version"

#undef  VSIZE
#define VSIZE  1066
#undef  VDELAY
#define VDELAY 429999  /* 429999 s = 5 days */

/*------------------------------------------------------------------------*/
/*! Function to check AFNI version.  Forks and returns to caller almost
    instantly.  The only output is from the child process to stderr;
    a message will be printed if the version check doesn't match.
--------------------------------------------------------------------------*/

void THD_check_AFNI_version( char *pname )
{
   int nbuf ;
   pid_t ppp ;
   char *vbuf=NULL , vv[128]="none" , *vvaa ;
   char *home , mname[VSIZE]="file:" ;
   char *motd=NULL ;
   NI_stream ns ;

   if( AFNI_noenv("AFNI_VERSION_CHECK") ) return ;

   /* get time of last check -- do nothing if was very recent */

   home=getenv("HOME") ;
   if( home != NULL ) strcat(mname,home);
   strcat(mname,"/.afni.vctime") ;

   ns = NI_stream_open( mname , "r" ) ;
   if( ns != NULL ){
     NI_element *nel = NI_read_element(ns,11) ;
     NI_stream_close(ns) ;
     if( nel != NULL ){
       char *rhs ; int done=0 ;
       rhs = NI_get_attribute(nel,"version_check_time") ;
       if( rhs != NULL ){
         int last_time = (int)strtol(rhs,NULL,10) ;
         int dtime     = ((int)time(NULL)) - last_time ;
         done = ( dtime >= 0 && dtime < VDELAY ) ;  /* too soon */
       }
       rhs = NI_get_attribute(nel,"motd") ;      /* 29 Nov 2005 */ 
       if( rhs != NULL ) motd = strdup(rhs) ;
       NI_free_element(nel) ;
       if( done ) return ;
     }
   }

   /* recall that fork() return value is
        < 0 for an error
        > 0 in the parent
       == 0 in the child  */

   ppp = fork() ;

   if( ppp < 0 ) return ; /* fork failed */

   /* parent: wait for child to exit (happens almost instantly) */

   if( ppp > 0 ){ waitpid(ppp,NULL,0); return; }  /* parent */

   /* below here is the child, which never returns:
      fork again immediately, then this child exits;
      this is to prevent zombie processes from hanging around */

   ppp = fork() ; if( ppp != 0 ) _exit(0) ;

   /* grandchild process continues to do the actual work */

   /*-- setup the "User-agent:" header for HTTP --*/

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

#ifdef USE_HTTP_10
     { int jj ;
       struct utsname ubuf ;
       char ua[512] ;

       if( pname == NULL ) pname = "afni" ;
       ubuf.nodename[0] = ubuf.sysname[0] = ubuf.machine[0] = '\0' ;
       jj = uname( &ubuf ) ;
       if( jj >= 0 && ubuf.nodename[0] != '\0' )
         sprintf( ua ,
                 "%s (avers='%s'; prec='%s' node='%s'; sys='%s'; mach='%s')" ,
                  pname,VERSION,PCLAB,ubuf.nodename,ubuf.sysname,ubuf.machine );
       else
         sprintf( ua , "%s (avers='%s'; prec='%s')" , pname , VERSION , PCLAB );

       set_HTTP_10( 1 ) ;
       set_HTTP_user_agent( ua ) ;
     }
#else
     set_HTTP_10( 0 ) ;
#endif

   /*-- NOW, fetch information from the AFNI master computer --*/

   nbuf = read_URL( VERSION_URL , &vbuf ) ;  /* see thd_http.c */

#ifdef USE_HTTP_10
   set_HTTP_10( 0 ) ;
#endif

   if( nbuf <= 0 || vbuf == NULL || vbuf[0] == '\0' ) _exit(0) ; /* failed */

   vvaa = strstr(vbuf,"AFNI_") ;   if( vvaa == NULL ) _exit(0) ;

   /* get the first string -- that is the current AFNI version number */

   sscanf( vvaa , "%127s" , vv ) ;

   /* compare with compiled-in version (from afni.h) */

   if( strcmp(vv,VERSION) != 0 )
    fprintf(stderr,"\n"
                   "++ VERSION CHECK!  This program = %s\n"
                   "++         Current AFNI website = %s\n" ,
            VERSION , vv ) ;

   /* record the current time and VERSION, so we don't check too often */

   ns = NI_stream_open( mname , "w" ) ;
   if( ns != NULL ){
     NI_element *nel=NI_new_data_element("AFNI_vctime",0); char rhs[32];
     sprintf(rhs,"%d",(int)time(NULL)) ;
     NI_set_attribute( nel , "version_check_time" , rhs ) ;
     if( strcmp(vv,"none") != 0 )
       NI_set_attribute( nel , "version_string" , VERSION ) ;
     if( motd != NULL ){     /* 29 Nov 2005 */
       NI_set_attribute( nel , "motd" , motd ); free((void *)motd) ;
     }
     NI_write_element( ns , nel , NI_TEXT_MODE ) ;
     NI_stream_close(ns) ;
     NI_free_element(nel) ;
   }

   /* Alas, poor Version Check, I knew him well */

   _exit(0) ;
}
