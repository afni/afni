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

extern long long THD_filesize( char * pathname ) ;
extern char * THD_find_executable( char * ) ;

/*---------------------------------------------------------------------*/
static int   use_http_ver     = 0          ; /* defaults to HTTP 0.9 */
static char *http_user_agent = "read_URL" ;

void set_HTTP_10( int n ){          /* 24 Mar 2005; ZSS Apr. 2011*/
   if (n) use_http_ver = 10;
   else use_http_ver = 0;
}

void set_HTTP_11( int n ){          /* 24 Mar 2005; ZSS Apr. 2011*/
   if (n) use_http_ver = 11;
   else use_http_ver = 0;
}

/*---------------------------------------------------------------------*/

extern void set_HTTP_user_agent( char *ua )
{
   if( ua == NULL || *ua == '\0' ) http_user_agent = "read_URL" ;
   else                            http_user_agent = strdup(ua) ;
}

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
   char str[512] ;
   int ii ;

   if( host == NULL || port <= 0 || file == NULL ) return NULL ;

   sprintf(str,"tcp:%s:%d",host,port) ;
   DMESS(" ++Opening %s",str);
   ioc = iochan_init( str , "create" ) ;
   if( ioc == NULL ){ FAILED; return NULL; }
   if( debug )fprintf(stderr,".");
   iochan_set_cutoff( ioc ) ;
   if( debug )fprintf(stderr,".");
   ii = iochan_writecheck( ioc , msec ) ;
   if( ii <= 0 ){ FAILED; IOCHAN_CLOSE(ioc) ; return NULL ; }

   DMESS(" ++GET %s",file);
   if( use_http_ver == 11)
     sprintf(str,"GET %s HTTP/1.1\r\n"
                 "Host: %s\r\n"
                 "User-Agent: %s\r\n"
                 "\r\n", file , host, http_user_agent ) ;  /* HTTP 1.1 */
   else if( use_http_ver == 10)
     sprintf(str,"GET %s HTTP/1.0\r\n"
                 "User-Agent: %s\r\n"
                 "\r\n", file , http_user_agent ) ;       /* HTTP 1.0 */
   else
     sprintf(str,"GET %s\r\n",file) ;                     /* HTTP 0.9 */
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

#define HTTPS    "https://"
#define HTTPSLEN 8

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

/*---------------------------------------------------------------------*/
/*!
   strnstr is not standard
*/
char *af_strnstr(char *s1, char *s2, size_t n)
{
   int n1=0, n2=0;
   char c1 = '\0', c2 = '\0', *cout=NULL;

   if (s1 && (n1 = strlen(s1)) > n) {c1=s1[n]; s1[n]='\0'; }
   if (s2 && (n2 = strlen(s2)) > n) {c2=s2[n]; s2[n]='\0'; }

   cout = strstr(s1, s2);

   if (n1 > n) s1[n] = c1;
   if (n2 > n) s2[n] = c2;

   return(cout);
}

/*---------------------------------------------------------------------*/
/*
A very simple parsing of HTTP1.1 header fields.
Assumes buf and hname are all upper case.
Function does not know to avoid searching beyond
end of headers             ZSS Mar. 2011
*/

char *HTTP_header_val(char *head, char *hname, size_t max_head)
{
   int n_hname = 0;
   char *cpt = NULL;

   if (!hname || !head) return(NULL);

   if (!af_strnstr(head,"HTTP/1.1", 36)) return(NULL);
   if (max_head <= 0) {
      if (strlen(head)<1024) max_head = strlen(head);
      else max_head = 1024;
   }
   n_hname = strlen(hname);
   cpt = af_strnstr(head,hname, max_head);

   if (cpt) return(cpt+n_hname);

   return(NULL);
}

/*---------------------------------------------------------------------*/

long HTTP_header_long_val(char *head, char *hname, size_t max_head, long errval)
{
   char *cpt = NULL;
   long val=errval;

   if ((cpt = HTTP_header_val(head, hname, max_head))) {
      val = strtol(cpt, NULL, 10);
   }
   return(val);
}

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

  read_URL_http11 is a new version that is meant to handle
  HTTP1.1 header and return as soon as entity body is read
-----------------------------------------------------------------*/

#define QBUF 4096
typedef struct {
   char *page; /* the whole page */
   size_t N_head;  /* size of header */
   int head_complete; /* header reading complete */
   size_t N_page;  /* size of page */
   size_t N_cont;  /* size of content */
   size_t N_alloc; /* allocated size for page */
   float http_ver;
   int status;
   int N_chunks;
   int cflag;
   char *data;
} URL_PAGE;

/*---------------------------------------------------------------------*/

int page_append(char *buf, int n_buf, URL_PAGE *up, int null_term)
{
   if (up->N_page+n_buf > up->N_alloc) {
      do { up->N_alloc += QBUF; } while (up->N_alloc <= up->N_page+n_buf);
      up->page = AFREALL(up->page, char, up->N_alloc+1) ;
   }
   memcpy( up->page+up->N_page, buf, n_buf);
   up->N_page += n_buf;
   if (null_term) {
      /* make sure we've got a plug */
      if (up->page[up->N_page-1] != '\0') up->page[up->N_page]='\0';
   }

   ++up->N_chunks;
   return(1);
}

/*---------------------------------------------------------------------*/

int page_parse_status(URL_PAGE *up) {
   char *ttt=NULL, *cpt=NULL;
   int i, j;

   if (up->status > 0) return(1); /* done */
   if (!up->page || up->N_page < 1) return(0);
   i = 0;
   while (i < up->N_page && up->page[i] != '\r' && up->page[i] != '\n') ++i;
   /* upper casing */
   ttt = (char *)calloc(i+1, sizeof(char));
   for (j=0; j< i; ++j) ttt[j] = toupper(up->page[j]);
   ttt[j] = '\0';

   /* parse status */
   up->http_ver = 0.0; up->status = 0;
   if ((cpt = strstr(ttt,"HTTP/"))) {
      up->http_ver = (float)strtod(cpt+5, NULL);
         /* a more proper parsing should be as 1*DIGIT "." 1*DIGIT */
      j = 0;
      while (!isblank(cpt[j])) ++j;
      up->status = (int)strtol(cpt+j, NULL, 10);
   } else { /* older stuff */
      up->http_ver = 0.9;
      /* search more than 1st line for NOT FOUND */
      ttt = (char *)realloc(ttt, (up->N_page+1)*sizeof(char));
      for (j=0; j< up->N_page; ++j) ttt[j] = toupper(up->page[j]);
      ttt[j] = '\0';
      if (af_strnstr(ttt,"NOT FOUND", 255)) {
         up->status = 404;
      }
      up->status = 200; /* fake it */
   }

   free(ttt); ttt=NULL;
   return(1);
}

/*---------------------------------------------------------------------*/

int page_not_found(URL_PAGE *up) {
   return(up->status >= 400 ? 1:0);
}

/*---------------------------------------------------------------------*/

int page_scan_head(URL_PAGE *up) {
   int i=0, nl=0;

   if (up->head_complete) return(1);

   /* start a couple of characters before the last stop */
   i = up->N_head-5; if (i<1) i = 1;

   /* search for sequential new lines*/
   nl = 0;
   while (i<up->N_page && nl<2) {
           if( up->page[i] == '\r' ) ++nl;
      else if( up->page[i] != '\n' ) nl = 0; /* not blank */
      ++i;
   }
   if (nl == 2) {
      up->head_complete = 1;
   }
   up->N_head += i;

   /* make header all upper case */
   for (i=0; i<up->N_head; ++i) up->page[i] = toupper(up->page[i]);

   /* move till next non new line */
   while (up->page[up->N_head] == '\n' || up->page[up->N_head] == '\r')
      ++up->N_head;
   return(1);
}

/*---------------------------------------------------------------------*/
/* return a copy of the header.
Caller must free pointer */
char *page_header_copy(URL_PAGE *up)
{
   char *hcp=NULL;
   if (!up->page || !up->head_complete) return(NULL);
   hcp = (char *)calloc(up->N_head+1, sizeof(char));
   memcpy(hcp, up->page, (up->N_head+1)*sizeof(char));
   hcp[up->N_head] = '\0';
   return(hcp);
}

/*---------------------------------------------------------------------*/
/* return a pointer to the beginning of the
content. Do not free this pointer */
char *page_content(URL_PAGE *up)
{
   if (up->http_ver < 1.1) return(up->page);
   if (!up->page || !up->head_complete) return(NULL);
   return(up->page+up->N_head);
}

/*---------------------------------------------------------------------*/

int page_init(URL_PAGE *up, char *url)
{
   int ii;
   char *cpt=NULL, qname[256] ;

   memset(up, 0, sizeof(URL_PAGE));
   if (!url) return(0);

   ii = strlen(url) ;
   if( ii > 3 ){
      cpt = url + (ii-3) ; up->cflag = (strcmp(cpt,".gz") == 0) ;
   } else {
      up->cflag = 0 ;
   }

   return(1);
}

/*---------------------------------------------------------------------*/

int page_dump(URL_PAGE *up, FILE *out, char *head)
{
   char cct={'\0'};
   if (out==NULL) out = stderr;

   if (head) fprintf(out,"%s",head);
   fprintf(out,"<page:%zu>%s<\\page:%zu>\n",
               up->N_page, up->page ? up->page:"NULL", up->N_page);
   if (up->page && up->N_head) {
      cct = up->page[up->N_head]; up->page[up->N_head] = '\0';
   }
   fprintf(out,"<head:%zu-%s>%s<\\head:%zu-%s>\n",
               up->N_head, up->head_complete ? "complete":"incomplete",
               up->page ? up->page:"NULL",
               up->N_head, up->head_complete ? "complete":"incomplete");
   if (up->page && up->N_head) up->page[up->N_head] = cct;
   fprintf(out,"<ver>%f<\\ver><status>%d<\\status>\n"
               "<n_chunks>%d<\\n_chunks>\n"
               "<cont_len>%zu<\\cont_len>\n"
               "<cflag>%d<\\cflag>\n"
               "<data>%s<\\data>\n",
               up->http_ver, up->status, up->N_chunks, up->N_cont,
               up->cflag,
               up->data ? up->data:"NULL");
   return(1);
}

/*---------------------------------------------------------------------*/

int page_delete(URL_PAGE *up)
{
   if (up->page) free(up->page);
   if (up->data) free(up->data);
   memset(up, 0, sizeof(URL_PAGE));

   return(1);
}

/*---------------------------------------------------------------------*/

int page_set_data(URL_PAGE *up)
{
   char qname[256], sbuf[512];
   int ii, nuse=0;
   FILE *cfile=NULL;

   if (up->data) return(1);

   if( up->cflag ){ /* uncompress via temp file */
      setup_tmpdir() ;
      strcpy(qname,tmpdir) ; strcat(qname,"gosiaXXXXXX") ;
      mkstemp(qname) ;  /* from mktemp   22 Sep, 2014 [rickr] */
      if( qname[0] != '\0' ){
         strcat(qname,".gz") ; cfile = fopen( qname , "wb" ) ;
         if( cfile == NULL ) up->cflag = 0 ;
      } else {
         up->cflag = 0 ;
      }

      if( up->cflag == 0 ){
         DMESS(" **Temp file %s FAILS\n",qname);
         up->cflag = -1;
         return(-1);
      }
      DMESS(" ++Temp file=%s",qname);

      /* dump to file */
      if( fwrite( up->page+up->N_head , 1 , up->N_cont , cfile )
                     != up->N_page-up->N_head ){           /* write failed? */
            DMESS("\n** Write to temp file %s FAILED!\n",qname);
            page_delete(up);
            return( -1 );
      }
      fclose(cfile); cfile = NULL;

      /* uncompress and bring back */
      sprintf( sbuf , "gzip -dq %s" , qname ) ;     /* execute gzip */
      ii = system(sbuf) ;
      if( ii != 0 ){ DMESS("%s"," **gzip failed!\n");
                     unlink(qname) ; return( -1 );   }  /* gzip failed  */
      ii = strlen(qname) ; qname[ii-3] = '\0' ;     /* fix filename */
      nuse = THD_filesize( qname ) ;                /* find how big */
      if( nuse <= 0 ){ DMESS("%s"," **gzip failed!\n");
                       unlink(qname) ; return( -1 );   }

      cfile = fopen( qname , "rb" ) ;
      if( cfile == NULL ){ DMESS("%s"," **gzip failed!\n");
                           unlink(qname) ; return( -1 );   }
      up->data = AFMALL(char, nuse) ;
      fread( up->data , 1 , nuse , cfile ) ;             /* read file in */
      fclose(cfile) ; unlink(qname) ;
   } else {
      up->data = AFMALL(char, up->N_page - up->N_head+1);
      memcpy(up->data, up->page+up->N_head,
                  sizeof(char)*(up->N_page-up->N_head));
      up->data[up->N_page-up->N_head] = '\0';
      nuse = up->N_page-up->N_head;
   }
   return(nuse);
}

/*---------------------------------------------------------------------*/

int page_received(URL_PAGE *up) {
   if (up->http_ver < 1.1) return(0);
   if (up->head_complete) {
      /* get content length */
      up->N_cont = HTTP_header_long_val(up->page, "CONTENT-LENGTH:",
                                        up->N_head, -1);
      if (up->N_cont >=0 && up->N_page >= up->N_head+up->N_cont) return(1);
   }
   return(0);
}

/*---------------------------------------------------------------------*/
/*!
   Read an HTTP/1.1 url
   url: The URL
   msec: Number of msec to wait before abandoning all hope
   data: A non null pointer to a null char *pointer that will hold the
         content of the response. At call time, data != NULL and *data = NULL
   head: If not null, *head will contain all the headers in the response

   returns -1 in failure, total number of characters in data otherwise
*/
int read_URL_http11( char * url , int msec , char ** data, char **head )
{
   IOCHAN * ioc ;
   char * cpt , qbuf[QBUF] ;
   int ii,jj , nuse , nget=0, nmeg=0 ;
   size_t con_len = -1;
   URL_PAGE up;

   /* sanity check */

   if( url == NULL || data == NULL || *data || (head && *head) || msec < 0 )
      return( -1 );

   /* open http channel to get url */

   ioc = open_URL_http( url , msec ) ;
   if( ioc == NULL ){ DMESS("%s","\n"); return( -1 ); }

   /* check if url will be returned gzip-ed */
   page_init(&up, url);

   /* read all of url */
   nuse = 0 ;
   do{
      if(debug)fprintf(stderr,".");
      ii = iochan_readcheck( ioc , msec ) ;  /* wait for data to be ready */
      if( ii <= 0 ) break ;                  /* quit if no data */
      ii = iochan_recv( ioc , qbuf , QBUF ) ;
      if( ii <= 0 ) break ;                  /* quit if no data */

      if( prog ){
        nget += ii ; jj = nget/(1024*1024) ;
        if( jj > nmeg ){ nmeg=jj; if(debug)fprintf(stderr,"."); }
      }

      page_append(qbuf, ii, &up, 1);   /* append bufer to structure */
      page_parse_status(&up);           /* sets version and status if not set */
      if (page_not_found(&up)) { /* NOT FOUND? */
         page_delete(&up);
         DMESS("%s"," **NOT FOUND\n");
         IOCHAN_CLOSE(ioc) ; return( -1 );
      }
      page_scan_head(&up); /* scan for header, if needed */

      if (debug) page_dump(&up, NULL, NULL );

      nuse += ii ;                           /* how many bytes so far */

   } while(!page_received(&up)) ;

   IOCHAN_CLOSE(ioc) ;

   if( prog && nmeg > 0 ) fprintf(stderr,"!\n") ;

   /* didn't get anything? */

   if( nuse <= 0 ){
      page_delete(&up);
      FAILED; return(-1);
   }
   if(debug) fprintf(stderr,"!\n");

   /* Set data */
   nuse = page_set_data(&up);
   DMESS("%s","\n"); *data = up.data ; up.data=NULL;

   /* want header ? */
   if (head) {
      *head = (char *)realloc(up.page, (up.N_head+1)*sizeof(char));
      *head[up.N_head] = '\0';
      up.page = NULL;
   }
   page_delete(&up);
   return(nuse);
}

/*---------------------------------------------------------------------*/

int read_URL_http( char * url , int msec , char ** data )
{
   IOCHAN * ioc ;
   char * buf=NULL , * cpt , qbuf[QBUF] , qname[256] ;
   int ii,jj , nall=0 , nuse , nget=0, nmeg=0 ;
   int cflag , first ;
   FILE *cfile=NULL ;

   if (use_http_ver == 11) return(read_URL_http11( url , msec , data, NULL ));

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
      mkstemp(qname) ;
      if( qname[0] != '\0' ){
         strcat(qname,".gz") ; cfile = fopen( qname , "wb" ) ;
         if( cfile == NULL ) cflag = 0 ;
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
        if( jj > nmeg ){ nmeg=jj; if(debug)fprintf(stderr,"."); }
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

int read_URL_https( char *url , char **data )  /* 11 Feb 2016 */
{
   FILE *fp ;
   char *cmd , *getprog=NULL , *buf=NULL , qbuf[QBUF] ;
   size_t nbuf=0 , nnn ;

   if( url == NULL || *url == '\0' || data == NULL ) return(-1) ;

   if( getprog == NULL ){
     getprog = THD_find_executable("wget") ;
     if( getprog != NULL ){
       cmd = (char *)malloc(sizeof(char)*(strlen(getprog)+strlen(url)+64)) ;
       sprintf( cmd , "%s -o /dev/null -O - %s",getprog,url) ;
     }
   }

   if( getprog == NULL ){
     getprog = THD_find_executable("curl") ;
     if( getprog != NULL ){
       cmd = (char *)malloc(sizeof(char)*(strlen(getprog)+strlen(url)+64)) ;
       sprintf( cmd , "%s --silent -f -o - %s",getprog,url) ;
     }
   }

   if( getprog == NULL ) return(-1) ;

   fp = popen( cmd , "r" ) ;
   if( fp == NULL ) return(-1) ;

   iochan_sleep(10) ;
   while(1){
     nnn = fread( qbuf , 1,QBUF-1 , fp ) ;
     if( nnn == 0 ) break ;
     if( nnn > 10 ){
       qbuf[nnn] = '\0' ;
       if( strcasestr(qbuf,"404 Not Found") != NULL ){
         pclose(fp) ; if( buf != NULL ) free(buf) ;
         return(-1) ;
       }
     }
     buf = (char *)realloc( buf , nbuf+nnn+1 ) ;
     memcpy( buf+nbuf , qbuf , nnn ) ;
     nbuf += nnn ;
     iochan_sleep(1) ;
   }

   pclose(fp) ;
   if( buf == NULL || nbuf == 0 ) return(-1) ;

   *data = buf ; return((int)nbuf) ;
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
   mkstemp(qname) ;
   if( qname[0] == '\0' ) return( -1 );
   if( cflag ) strcat(qname,".gz") ;

   /* write the script file that will be used to run ftp */

   strcpy(sname,tmpdir) ; strcat(sname,"dahmerXXXXXX") ;
   mkstemp(sname) ;            if( sname[0] == '\0' ) return( -1 );
   sp = fopen( sname , "w" ) ; if( sp == NULL )       return( -1 );

   fprintf( sp , "#!/bin/sh\n" ) ;
   fprintf( sp , "ftp -n << EEEEE &> /dev/null\n") ;
   if( port > 0 )
      fprintf( sp , "open %s %d\n" , hostname , port ) ;
   else
      fprintf( sp , "open %s\n" , hostname ) ;
   fprintf( sp , "user %s %s\n" , ftp_name, ftp_pwd ) ;
   fprintf( sp , "binary\n" ) ;
   fprintf( sp , "passive\n" ) ;
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

   if( strstr(url,HTTPS) == url ){      /* 11 Feb 2016 */
      nn = read_URL_https( url , data ) ; return(nn) ;
   }

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
