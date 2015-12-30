#include "niml/niml.h"
#include "afni_environ.h"

#undef  AFNI_SERVER
#undef  AFNI_REQUEST

/*! Stream name to connect to AFNI server. */

#define AFNI_SERVER  "tcp:afni.nimh.nih.gov:80"

/*! Format for AFNI logging request. */

#define AFNI_REQUEST "HEAD /AFNIlogpath HTTP/1.0\r\n"  \
                     "User-Agent: %s\r\n\r\n"

/*------------------------------------------------------------------------*/
/*! Log the input string to the AFNI server.  Sends an HTTP request with
    the input string as the 'user agent', which will be saved in the
    Web server logs.
--------------------------------------------------------------------------*/

void AFNI_serverlog( char *str )
{
   pid_t child_pid ;
   NI_stream ns ;
   int nbuf=0 , jj ;
   char *sbuf , *rbuf , *ss ;

   if( str == NULL || *str == '\0'      ) return ;
   if( AFNI_noenv("AFNI_VERSION_CHECK") ) return ;

   /*-- start the child process --*/

   child_pid = fork() ;
   if( child_pid != (pid_t)0 ) return ;   /* parent is done */

   /*-- child will never return alive! --*/

   /* open stream to server */

   ns = NI_stream_open( AFNI_SERVER , "w" ) ;
   if( ns == (NI_stream)NULL ) _exit(0) ;

   /* copy input string, replace bad chars with spaces */

   sbuf = strdup(str) ;
   for( ss=sbuf ; *ss != '\0' ; ss++ ) if( !isgraph(*ss) ) *ss = ' ' ;

   /* truncate trailing spaces */

   nbuf = strlen(sbuf) ;
   for( ss=sbuf+(nbuf-1) ; isspace(*ss) ; ss-- ) *ss = '\0' ;

   /* build request to AFNI server */

   nbuf = strlen(sbuf)+strlen(AFNI_REQUEST)+32 ;
   rbuf = (char *)malloc(nbuf) ;
   sprintf(rbuf,AFNI_REQUEST,sbuf) ;

   /* wait for stream to get good for writing */

   jj = NI_stream_writecheck( ns , 1234 ) ;
   if( jj < 1 ) _exit(0) ;

   /* send the request */

   NI_stream_write( ns , rbuf , strlen(rbuf) ) ;

   /* don't read response: wait a decent interval, close connection, quit */

   NI_sleep(1) ; NI_stream_closenow(ns) ; _exit(0) ;
}
