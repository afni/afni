#include "thd_iochan.h"

/*---------------------------------------------------------------------
  Open a URL in host, port, and filename pieces.
  Wait up to msec milliseconds for network functions to occur.
  If an error occurs, return NULL, otherwise the caller can read
  from this IOCHAN (using iochan_recv, etc.).
-----------------------------------------------------------------------*/

IOCHAN * open_URL_hpf( char * host , int port , char * file , int msec )
{
   IOCHAN * ioc ;
   char str[192] ;
   int ii ;

   if( host == NULL || port <= 0 || file == NULL ) return NULL ;

   sprintf(str,"tcp:%s:%d",host,port) ;
   ioc = iochan_init( str , "create" ) ;
   if( ioc == NULL ) return NULL ;
   ii = iochan_writecheck( ioc , msec ) ;
   if( ii <= 0 ){ IOCHAN_CLOSE(ioc) ; return NULL ; }

   sprintf(str,"GET %s\n",file) ;
   ii = iochan_sendall( ioc , str , strlen(str) ) ;
   if( ii <= 0 ){ IOCHAN_CLOSE(ioc) ; return NULL ; }

   ii = iochan_readcheck( ioc , msec ) ;
   if( ii <= 0 ){ IOCHAN_CLOSE(ioc) ; return NULL ; }
   return ioc ;
}

/*----------------------------------------------------------------------
  Open a URL and prepare to read it (but the caller must actually read).
  At this time, only things of the form "http://host/filename"
  are supported.
------------------------------------------------------------------------*/

#define HTTP     "http://"
#define HTTPLEN  7

IOCHAN * open_URL( char * url , int msec )
{
  char * s, * h , * file ;
  char hostname[192] ;
  int port;
  IOCHAN * ioc ;

  /* check inputs */

  if( url == NULL || strstr(url,HTTP) != url ) return NULL ;

  /* parse hostname */

  for( s=url+HTTPLEN , h=hostname ;
       (*s != '\0') && (*s != ':') && (*s != '/') ;
       s++ , h++ ) *h = *s ;

  *h = '\0' ;

  /* parse port number */

  port = 0 ;
  if( *s == ':' ){
    port = strtol( ++s , &h , 10 ) ;
    s = h ;
  }
  if( port <= 0 ) port = 80 ;

  /* get the file name */

  file = (*s == '/') ? s : "/" ;

  ioc = open_URL_hpf( hostname , port , file , msec ) ;
  return ioc ;
}

/*---------------------------------------------------------------
  Read a URL, with network waits of up to msec milliseconds
  allowed.  Returns number of bytes read -- if this is > 0,
  then *data will be a pointer to malloc-ed bytes holding
  the contents of the file.  If the file is gzip-ed, then
  it will be un-gzip-ed.
-----------------------------------------------------------------*/

#define QBUF 4096

int read_URL( char * url , int msec , char ** data )
{
   IOCHAN * ioc ;
   char * buf=NULL , * cpt , qbuf[QBUF] , qname[128] ;
   int ii , nall , nuse ;
   int cflag ;
   FILE * cfile ;

   /* sanity check */

   if( url == NULL || data == NULL ) return -1 ;

   /* open http channel to get url */

   ioc = open_URL( url , msec ) ;
   if( ioc == NULL ) return -1 ;

   /* check if url will be returned gzip-ed */

   ii = strlen(url) ; cpt = url + (ii-3) ;
   cflag = (strcmp(cpt,".gz") == 0) ;

   if( cflag ){
      strcpy(qname,"/tmp/elvisXXXXXX") ;
      mktemp(qname) ;
      if( qname[0] != '0' ){
         strcat(qname,".gz") ;
         cfile = fopen( qname , "wb" ) ;
         if( cfile == NULL ) cflag == 0 ;
      } else {
         cflag = 0 ;
      }

      if( cflag == 0 ){ IOCHAN_CLOSE(ioc) ; return -1 ; }
   }

   /* read all of url */

   if( !cflag ){ buf = malloc( QBUF ) ; nall = QBUF ; }
   nuse = 0 ;

   do{
      ii = iochan_readcheck( ioc , msec ) ;  /* wait for data to be ready */
      if( ii <= 0 ) break ;                  /* quit if no data */
      ii = iochan_recv( ioc , qbuf , QBUF ) ;
      if( ii <= 0 ) break ;                  /* quit if no data */

      if( cflag ){                           /* write to temp file */
         nall = fwrite( qbuf , 1 , ii , cfile ) ;
         if( nall != ii ){                   /* write failed? */
            fclose(cfile) ; unlink(qname) ;
            IOCHAN_CLOSE(ioc) ; return -1 ;
         }
      } else {                               /* save to buffer */
         if( nuse+ii > nall ){               /* enlarge buffer? */
            nall += QBUF ;
            buf   = realloc( buf , nall ) ;
         }
         memcpy( buf+nuse , qbuf , ii ) ;    /* copy data into buffer */
      }
      nuse += ii ;                           /* how many bytes so far */
   } while(1) ;
   IOCHAN_CLOSE(ioc) ;

   /* didn't get anything? */

   if( nuse <= 0 ){
      if( cflag ){ fclose(cfile) ; unlink(qname) ; }
      else       { free(buf) ; }
      return -1 ;
   }

   /* uncompression time? */

   if( cflag ){
      fclose(cfile) ;
      sprintf( qbuf , "gzip -dq %s" , qname ) ;
      ii = system(qbuf) ;
      if( ii != 0 ){ unlink(qname) ; return -1 ; }  /* gzip failed */
      ii = strlen(qname) ; qname[ii-3] = '\0' ;     /* fix filename */
      nuse = THD_filesize( qname ) ;                /* find how big */
      if( nuse <= 0 ){ unlink(qname) ; return -1 ; }

      cfile = fopen( qname , "rb" ) ;
      if( cfile == NULL ){ unlink(qname) ; return -1 ; }
      buf = malloc(nuse) ;
      fread( buf , 1 , nuse , cfile ) ;
      fclose(cfile) ; unlink(qname) ;
   }

   /* data is in buf, nuse bytes of it */

   *data = buf ;
   return nuse ;
}
