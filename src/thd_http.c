/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "thd_iochan.h"
#include "Amalloc.h"
#include <sys/stat.h>

static int debug = 0 ;
#define FAILED     if(debug)fprintf(stderr," **FAILED\n")
#define DMESS(s,t) if(debug)fprintf(stderr,s,t)

extern unsigned long THD_filesize( char * pathname ) ;

/*---------------------------------------------------------------------*/
static char tmpdir[256] = "\0" ;

static void setup_tmpdir(void)  /* 02 Apr 1999 */
{
   char * td ;

   if( tmpdir[0] != '\0' ) return ;

                    td = getenv("TMPDIR") ;   /* try two possibilities */
   if( td == NULL ) td = getenv("TEMPDIR") ;

   if( td == NULL || td[0] == '\0' || strlen(td) > 222 ){
      strcpy(tmpdir,"/tmp/") ;
   } else {
      int ltd = strlen(td) ;
      strcpy(tmpdir,td) ;
      if( tmpdir[ltd-1] != '/' ) strcat(tmpdir,"/") ;
   }

   return ;
}

/*---------------------------------------------------------------------
  Open an "http://" URL in host, port, and filename pieces.
  Wait up to msec milliseconds for network functions to occur.
  If an error occurs, return NULL, otherwise the caller can read
  from this IOCHAN (using iochan_recv, etc.).
-----------------------------------------------------------------------*/

IOCHAN * open_URL_hpf( char * host , int port , char * file , int msec )
{
   IOCHAN * ioc ;
   char str[256] ;
   int ii ;

   if( host == NULL || port <= 0 || file == NULL ) return NULL ;

   sprintf(str,"tcp:%s:%d",host,port) ;
   DMESS(" ++Opening %s",str);
   ioc = iochan_init( str , "create" ) ;
   if( ioc == NULL ){ FAILED; return NULL; }
   fprintf(stderr,".");
   iochan_set_cutoff( ioc ) ;
   fprintf(stderr,".");
   ii = iochan_writecheck( ioc , msec ) ;
   if( ii <= 0 ){ FAILED; IOCHAN_CLOSE(ioc) ; return NULL ; }

   DMESS(" ++GET %s",file);
   sprintf(str,"GET %s\n",file) ;                     /* HTTP 0.9 */
   ii = iochan_sendall( ioc , str , strlen(str) ) ;
   if( ii <= 0 ){ FAILED; IOCHAN_CLOSE(ioc); return NULL; }

   ii = iochan_readcheck( ioc , msec ) ;
   if( ii <= 0 ){ FAILED; IOCHAN_CLOSE(ioc) ; return NULL ; }
   DMESS("%s"," **OPENED");
   return ioc ;
}

/*----------------------------------------------------------------------
  Open an "http://" URL and prepare to read it (but the caller must
  actually do the reading).  If NULL is returned, an error occurred.
------------------------------------------------------------------------*/

#define HTTP     "http://"
#define HTTPLEN  7

#define FTP      "ftp://"
#define FTPLEN   6

IOCHAN * open_URL_http( char * url , int msec )
{
  char * s, * h , * file ;
  char hostname[256] ;
  int port;
  IOCHAN * ioc ;

  /* check inputs */

  if( url == NULL || strstr(url,HTTP) != url ) return NULL ;

  /* parse hostname */

  for( s=url+HTTPLEN , h=hostname ;
       (*s != '\0') && (*s != ':') && (*s != '/') ; s++ , h++ ) *h = *s ;

  *h = '\0' ; if( hostname[0] == '\0' ) return NULL ;

  /* parse port number if present */

  port = 0 ;
  if( *s == ':' ){ port = strtol( ++s , &h , 10 ) ; s = h ; }
  if( port <= 0 ) port = 80 ;

  /* get the file name (keep leading "/") */

  file = (*s == '/') ? s : "/" ;

  /* do the actual work */

  ioc = open_URL_hpf( hostname , port , file , msec ) ;
  return ioc ;
}

/*--------------------------------------------------------------*/

static int prog=0 ;
void set_URL_progress( int p ){ prog=p; }  /* 20 Mar 2003 */

/*---------------------------------------------------------------
  Read an "http://" URL, with network waits of up to msec
  milliseconds allowed.  Returns number of bytes read -- if this
  is > 0, then *data will be a pointer to malloc-ed bytes holding
  the contents of the file.

  If the file is gzip-ed, then it will be un-gzip-ed before being
  loaded into memory.  This uses temporary files in $TMPDIR or
  /tmp, which must have space to hold the compressed and
  uncompressed file.  If the file is not compressed, then input
  is directly to memory and no temporary files are used.
-----------------------------------------------------------------*/

#define QBUF 4096

int read_URL_http( char * url , int msec , char ** data )
{
   IOCHAN * ioc ;
   char * buf=NULL , * cpt , qbuf[QBUF] , qname[256] ;
   int ii,jj , nall , nuse , nget=0, nmeg=0 ;
   int cflag , first ;
   FILE * cfile ;

   /* sanity check */

   if( url == NULL || data == NULL || msec < 0 ) return( -1 );

   /* open http channel to get url */

   ioc = open_URL_http( url , msec ) ;
   if( ioc == NULL ){ DMESS("%s","\n"); return( -1 ); }

   /* check if url will be returned gzip-ed */

   ii = strlen(url) ;
   if( ii > 3 ){
      cpt = url + (ii-3) ; cflag = (strcmp(cpt,".gz") == 0) ;
   } else {
      cflag = 0 ;
   }

   if( cflag ){
      setup_tmpdir() ;
      strcpy(qname,tmpdir) ; strcat(qname,"gosiaXXXXXX") ;
      mktemp(qname) ;
      if( qname[0] != '\0' ){
         strcat(qname,".gz") ; cfile = fopen( qname , "wb" ) ;
         if( cfile == NULL ) cflag == 0 ;
      } else {
         cflag = 0 ;
      }

      if( cflag == 0 ){
         DMESS(" **Temp file %s FAILS\n",qname); IOCHAN_CLOSE(ioc); return(-1);
      }
      DMESS(" ++Temp file=%s",qname);
   }

   /* read all of url */

   if( !cflag ){ buf = AFMALL(char, QBUF ) ; nall = QBUF ; }
   nuse = 0 ; first = 1 ;

   do{
      if(debug)fprintf(stderr,".");
      ii = iochan_readcheck( ioc , msec ) ;  /* wait for data to be ready */
      if( ii <= 0 ) break ;                  /* quit if no data */
      ii = iochan_recv( ioc , qbuf , QBUF ) ;
      if( ii <= 0 ) break ;                  /* quit if no data */

      if( prog ){
        nget += ii ; jj = nget/(1024*1024) ;
        if( jj > nmeg ){ nmeg=jj; fprintf(stderr,"."); }
      }

      if( first ){                           /* check for "not found" */
         if( buf == NULL ){ buf = AFMALL(char, ii) ; }
         memcpy( buf , qbuf , ii ) ;
         for( jj=0 ; jj < ii ; jj++ ) buf[jj] = toupper(buf[jj]) ;
         buf[ii-1] = '\0' ;
         cpt = strstr(buf,"NOT FOUND") ;
         if( cpt != NULL ){
            if( cflag ){ fclose(cfile) ; unlink(qname) ; }
            DMESS("%s"," **NOT FOUND\n");
            free(buf) ; IOCHAN_CLOSE(ioc) ; return( -1 );
         }
         first = 0 ;
         if( cflag ){ free(buf) ; buf = NULL ; }
      }

      if( cflag ){                           /* write to temp file */
         nall = fwrite( qbuf , 1 , ii , cfile ) ;
         if( nall != ii ){                   /* write failed? */
            DMESS("\n** Write to temp file %s FAILED!\n",qname);
            fclose(cfile) ; unlink(qname) ;
            IOCHAN_CLOSE(ioc) ; return( -1 );
         }
      } else {                               /* save to buffer */
         if( nuse+ii > nall ){               /* enlarge buffer? */
            nall += QBUF ;
            buf   = AFREALL(buf, char, nall) ;
         }
         memcpy( buf+nuse , qbuf , ii ) ;    /* copy data into buffer */
      }
      nuse += ii ;                           /* how many bytes so far */
   } while(1) ;
   IOCHAN_CLOSE(ioc) ;

   if( prog && nmeg > 0 ) fprintf(stderr,"!\n") ;

   /* didn't get anything? */

   if( nuse <= 0 ){
      if( cflag ){ fclose(cfile) ; unlink(qname) ; }
      else       { free(buf) ; }
      FAILED; return(-1);
   }
   if(debug)fprintf(stderr,"!\n");

   /* uncompression time? */

   if( cflag ){
      fclose(cfile) ;
      sprintf( qbuf , "gzip -dq %s" , qname ) ;     /* execute gzip */
      ii = system(qbuf) ;
      if( ii != 0 ){ DMESS("%s"," **gzip failed!\n");
                     unlink(qname) ; return( -1 );   }  /* gzip failed  */
      ii = strlen(qname) ; qname[ii-3] = '\0' ;     /* fix filename */
      nuse = THD_filesize( qname ) ;                /* find how big */
      if( nuse <= 0 ){ DMESS("%s"," **gzip failed!\n");
                       unlink(qname) ; return( -1 );   }

      cfile = fopen( qname , "rb" ) ;
      if( cfile == NULL ){ DMESS("%s"," **gzip failed!\n");
                           unlink(qname) ; return( -1 );   }
      buf = AFMALL(char, nuse) ;
      fread( buf , 1 , nuse , cfile ) ;             /* read file in */
      fclose(cfile) ; unlink(qname) ;
   }

   /* data is in buf, nuse bytes of it */

   DMESS("%s","\n"); *data = buf ; return( nuse );
}

/*---------------------------------------------------------------------*/

static char ftp_name[128] = "anonymous" ;
static char ftp_pwd[128]  = "AFNI@nowhere.org" ;

void set_URL_ftp_ident( char * name , char * pwd )  /* 05 Apr 1999 */
{
   int ll ;

   if( name == NULL || pwd == NULL ) return ;

   ll = strlen(name) ; if( ll < 1 || ll > 127 ) return ;
   ll = strlen(pwd)  ; if( ll < 1 || ll > 127 ) return ;

   strcpy(ftp_name,name) ; strcpy(ftp_pwd,pwd) ; return ;
}

/*---------------------------------------------------------------------
  Reads an "ftp://" URL, similarly to read_URL_http above;
  however, staging is always done through a temporary file.
-----------------------------------------------------------------------*/

int read_URL_ftp( char * url , char ** data )
{
   char * s, * h , * file , qname[256] , sname[256] , * cpt , * buf ;
   char hostname[256] ;
   int port , ii , cflag , nuse ;
   FILE * sp ;

   /* sanity check */

   if( url == NULL || data == NULL || strstr(url,FTP) != url ) return( -1 );

   /* parse hostname */

   for( s=url+FTPLEN , h=hostname ;
        (*s != '\0') && (*s != ':') && (*s != '/') ; s++ , h++ ) *h = *s ;

   *h = '\0' ; if( hostname[0] == '\0' ) return( -1 );

   /* parse port number, if present */

   port = 0 ;
   if( *s == ':' ){ port = strtol( ++s , &h , 10 ) ; s = h ; }

   /* get the file name (strip off leading "/") */

   if( *s == '/' ){
      file = s+1 ; if( file[0] == '\0' ) return( -1 );
   } else {
                                         return( -1 );
   }

   /* check if file will be returned gzip-ed */

   ii = strlen(file) ;
   if( ii > 3 ){
      cpt = file + (ii-3) ; cflag = (strcmp(cpt,".gz") == 0) ;
   } else {
      cflag = 0 ;
   }

   /* make name for output file */

   setup_tmpdir() ;
   strcpy(qname,tmpdir) ; strcat(qname,"elvisXXXXXX") ;
   mktemp(qname) ;
   if( qname[0] == '\0' ) return( -1 );
   if( cflag ) strcat(qname,".gz") ;

   /* write the script file that will be used to run ftp */

   strcpy(sname,tmpdir) ; strcat(sname,"dahmerXXXXXX") ;
   mktemp(sname) ;             if( sname[0] == '\0' ) return( -1 );
   sp = fopen( sname , "w" ) ; if( sp == NULL )       return( -1 );

   fprintf( sp , "#!/bin/sh\n" ) ;
   fprintf( sp , "ftp -n << EEEEE &> /dev/null\n") ;
   if( port > 0 )
      fprintf( sp , "open %s %d\n" , hostname , port ) ;
   else
      fprintf( sp , "open %s\n" , hostname ) ;
   fprintf( sp , "user %s %s\n" , ftp_name, ftp_pwd ) ;
   fprintf( sp , "binary\n" ) ;
   fprintf( sp , "get %s %s\n" , file , qname ) ;
   fprintf( sp , "bye\n" ) ;
   fprintf( sp , "EEEEE\n" ) ;
   fprintf( sp , "exit\n" ) ;
   fclose( sp ) ;
   chmod( sname , S_IRUSR | S_IWUSR | S_IXUSR ) ;

   /* execute the script, then delete it */

   system( sname ) ; unlink( sname ) ;

   /* check the size of the output file */

   nuse = THD_filesize( qname ) ;
   if( nuse <= 0 ){ unlink(qname) ; return( -1 ); }

   /* uncompress the file, if needed */

   if( cflag ){
      sprintf( sname , "gzip -dq %s" , qname ) ;    /* execute gzip */
      ii = system(sname) ;
      if( ii != 0 ){ unlink(qname) ; return( -1 ); }  /* gzip failed  */
      ii = strlen(qname) ; qname[ii-3] = '\0' ;     /* fix filename */
      nuse = THD_filesize( qname ) ;                /* find how big */
      if( nuse <= 0 ){ unlink(qname) ; return( -1 ); }
   }

   /* suck the file into memory */

   sp = fopen( qname , "rb" ) ;
   if( sp == NULL ){ unlink(qname) ; return( -1 ); }
   buf = AFMALL(char,nuse) ; if( buf == NULL ){ unlink(qname) ; return( -1 ); }

   fread( buf , 1 , nuse , sp ) ;  /* AT LAST! */
   fclose(sp) ; unlink(qname) ;

   /* data is in buf, nuse bytes of it */

   *data = buf ; return( nuse );
}

/*-------------------------------------------------------------------
   Read a URL (ftp:// or http://) into memory.  The return value
   is the number of bytes read, and *data points to the data.
   If the return value is negative, then something bad happened.
---------------------------------------------------------------------*/

int read_URL( char * url , char ** data )
{
   int nn ;
   if( url == NULL || data == NULL ) return( -1 );

   if( getenv("AFNI_WWW_DEBUG") != NULL ) debug = 1 ;

   if( strstr(url,HTTP) == url ){
      nn = read_URL_http( url , 4000 , data ) ; return(nn) ;
   }

   else if( strstr(url,FTP) == url ){
      nn = read_URL_ftp( url , data ) ; return(nn) ;
   }

   return( -1 );
}

/*------------------------------------------------------------------
  Read a URL and save it to disk in tmpdir.  The filename
  it is saved in is returned in the malloc-ed space *tname.
  The byte count is the return value of the function;
  if <= 0, then an error transpired (and *tname is not set).
--------------------------------------------------------------------*/

extern char * THD_trailname( char * fname , int lev ) ;

int read_URL_tmpdir( char * url , char ** tname )
{
   int nn , ll ;
   char * data , * fname , * tt ;
   FILE * fp ;

   if( url == NULL || tname == NULL ) return( -1 );

   nn = read_URL( url , &data ) ;  /* get the data into memory */
   if( nn <= 0 ) return( -1 );       /* bad */

   /* make the output filename */

   setup_tmpdir() ;
   fname = AFMALL(char, strlen(url)+strlen(tmpdir)+1) ;
   tt    = THD_trailname(url,0) ;
   strcpy(fname,tmpdir) ; strcat(fname,tt) ; ll = strlen(fname) ;
   if( ll > 3 && strcmp(fname+(ll-3),".gz") == 0 ) fname[ll-3] = '\0' ;

   /* open and write output */

   fp = fopen( fname , "wb" ) ;
   if( fp == NULL ){
      fprintf(stderr,"** Can't open temporary file %s\n",fname);
      free(data) ; return( -1 );
   }
   ll = fwrite(data,1,nn,fp) ; fclose(fp) ; free(data) ;
   if( ll != nn ){ unlink(fname); return( -1 ); } /* write failed */

   *tname = fname ; return( nn );
}
