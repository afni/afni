#include "niml.h"

/*--------------------------------------------------------------------------*/
/*! Allocate memory (actually is calloc). */

void * NI_malloc( size_t len )
{
   void *p = calloc(1,len) ;
   if( p == NULL ){
      fprintf(stderr,"NI_malloc() fails.\n") ; exit(1) ;
   }
   return p ;
}

/*--------------------------------------------------------------------------*/
/*! Free memory. */

void NI_free( void *p )
{
   if( p != NULL ) free(p) ;
}

/*--------------------------------------------------------------------------*/
/*! Reallocate memory. */

void * NI_realloc( void *p , size_t len )
{
   void *q = realloc( p , len ) ;
   if( q == NULL ){
      fprintf(stderr,"NI_realloc() fails.\n"); exit(1);
   }
   return q ;
}

/*--------------------------------------------------------------------------*/
/*! Like strncpy, but better. */

char * NI_strncpy( char *dest , const char *src , size_t n )
{
   if( dest == NULL || n == 0 ) return NULL ;
   if( src  == NULL || n == 1 ){ dest[0] = '\0' ; return dest ; }
   strncpy( dest , src , n-1 ) ;
   dest[n] = '\0' ; return dest ;
}

/*--------------------------------------------------------------------------*/
/*! Deallocate a header_stuff struct. */

static void destroy_header_stuff( header_stuff *hs )
{
   int ii ;
   if( hs == NULL ) return ;
   NI_free(hs->name) ;
   for( ii=0 ; ii < hs->nattr ; ii++ ){
      if( hs->lhs != NULL ) NI_free( hs->lhs[ii] ) ;
      if( hs->rhs != NULL ) NI_free( hs->rhs[ii] ) ;
   }
   NI_free( hs ) ;
}

/*-------------------------------------------------------------------------*/
/*! Find an isolated string in the input array of char.

    nst = start position
    nch = total number of data bytes
    ch  = array of data bytes

    Return value is an intpair with the .i component indicating the
    start position of the string in the data and the .j indicating
    the byte after the end of the string.  If the .i component is
    negative, then no string was found.
---------------------------------------------------------------------------*/

static intpair find_string( int nst, int nch, char *ch )
{
   intpair ans = {-1,-1} ;
   int ii,jj ;
   char quot ;

   if( nst >= nch || nch < 2 || ch == NULL ) return ans;        /* bad input */

   for( ii=nst ; ii<nch && !IS_START_CHAR(ch[ii]) ; ii++ ) ;/* skip to start */

   if( ii >= nch ) return ans ;                                 /* bad input */

   if( IS_QUOTE_CHAR(ch[ii]) ){                             /* quoted string */
      if( ii == nch-1 ) return ans ;                            /* bad input */
      quot = ch[ii] ; ii++ ;
      for( jj=ii ; jj<nch && ch[jj] != quot ; jj++ ) ;      /* skip to close */
   } else {
      for( jj=ii+1 ; jj<nch && IS_STRING_CHAR(ch[jj]) ; jj++ ) ; /* to blank */
   }

   ans.i = ii ; ans.j = jj ; /* answer starts at ch[ii] and goes to ch[jj-1] */
   return ans ;
}

/*--------------------------------------------------------------------------*/
/*! Parse into strings a <header and=its attributes="OK">.

    ndat  = number of data bytes
    dat   = data bytes

   *nused = number of bytes consumed (=index of byte after the closing '>').
    Return value is a pointer to a header_stuff struct;
    if NULL is returned, something real bad happened (and *nused won't
    be assigned).
----------------------------------------------------------------------------*/

static header_stuff * parse_header_stuff( int ndat, char *dat, int *nused )
{
   header_stuff *hs ; /* return value */
   int id,jd , nn ;
   intpair ss ;

   if( ndat < 2 || dat == NULL ) return NULL ;        /* bad input */

   for( id=0 ; id < ndat && dat[id] != '<' ; id++ ) ; /* skip to opening */

   if( id >= ndat-1 ) return NULL ;                   /* bad input */

   hs = NI_malloc(sizeof(header_stuff)) ;             /* make output */
   hs->nattr = hs->empty = 0 ;
   hs->name  = NULL ;
   hs->lhs   = hs->rhs = NULL ;

   /* find and assign name string */

   ss = find_string( id+1 , ndat , dat ) ;

   if( ss.i < 0 || ss.j <= ss.i ){
      destroy_header_stuff( hs ) ; return NULL ;   /* no name string */
   }

   nn = ss.j - ss.i ;                               /* string length */
   hs->name = NI_malloc(nn+1) ;
   NI_strncpy( hs->name , dat+ss.i , nn+1 ) ;

   /* start scanning for next string at location id */

   id = ss.j ; if( IS_QUOTE_CHAR(dat[id]) ) id++ ;

   /* find and assign attribute strings */

   while(1){

      for( ; id < ndat && isspace(dat[id]) ; id++ ) ; /* skip blanks */

      if( id >= ndat ) break ;                 /* end of input found */

      if( dat[id] == '>' ) break ;                  /* ">" end found */

      if( dat[id] == '/' ){                        /* "/>" end found */
         if( id < ndat-1 ) id++ ;
         hs->empty = 1 ;                   /* mark header as 'empty' */
         break ;
      }

      /* find next string */

      ss = find_string( id , ndat , dat ) ;

      if( ss.i < 0 || ss.j <= ss.i ) break ; /* didn't find a string */

      /* extend size of attribute arrays */

      hs->lhs = NI_realloc( hs->lhs , sizeof(char *)*(hs->nattr+1) ) ;
      hs->rhs = NI_realloc( hs->rhs , sizeof(char *)*(hs->nattr+1) ) ;

      /* this is the LHS string */

      nn = ss.j - ss.i ;                      /* length of string */
      hs->lhs[hs->nattr] = NI_malloc(nn+1) ;
      NI_strncpy( hs->lhs[hs->nattr] , dat+ss.i , nn+1 ) ;

      hs->rhs[hs->nattr] = NULL ;             /* in case there is no RHS */

      id = ss.j ; if( id >= ndat ) break ;    /* end of input ? */

      if( dat[id] != '=' ){                   /* no '=' means no RHS */
         (hs->nattr)++ ;
         continue ;                           /* so get next attribute */
      }

      id++ ; if( id >= ndat ) break ;         /* skip the '=' */

      /* find next string */

      ss = find_string( id , ndat , dat ) ;

      if( ss.i < 0 || ss.j <= ss.i ) break ; /* didn't find a string */

      /* this is the RHS string */

      nn = ss.j - ss.i ;                      /* length of string */
      hs->rhs[hs->nattr] = NI_malloc(nn+1) ;
      NI_strncpy( hs->rhs[hs->nattr] , dat+ss.i , nn+1 ) ;

      (hs->nattr)++ ;                  /* increment attribute count */

      /* start scanning for next string at location id */

      id = ss.j ; if( IS_QUOTE_CHAR(dat[id]) ) id++ ;

   } /* end of loop over input */

   if( nused != NULL ){
      if( id >= ndat ) id = ndat-1 ;
      *nused = id+1 ;              /* number of bytes used from dat */
   }

   return hs ;                         /* the goal of all that work */
}

/*--------------------------------------------------------------------*/
/*! Decode a data type string into an array of integer codes.

  Returns NULL if the input is bad bad bad.
----------------------------------------------------------------------*/

static intarray * decode_type_string( char *ts )
{
   int num, typ, lts, id,jd,kd, nn,kk ;
   intarray *iar ;

   if( ts == NULL || ts[0] == '\0' ) return NULL ;

   iar = NI_malloc(sizeof(intarray)) ;  /* create output */
   iar->num = 0 ; iar->ar = NULL ;

   /* scan type string 1 time to get count */

   lts = strlen(ts) ;
   num = 0 ;            /* will be count of fields */

   for( id=0 ; id < lts ; id++ ){

      if( isdigit(ts[id]) ){   /* a count prefix */
         jd = nn = 0 ;
         sscanf( ts+id , "%d%n" , &jd , &nn ) ;   /* get the count */
         if( jd <= 0 || nn <= 0 ){
            NI_free(iar->ar) ; NI_free(iar) ; return NULL ; /* bad */
         }
         num += jd ;     /* this many fields so far */
         id  += nn ;     /* skip count prefix (plus next character) */

         if( !IS_DATUM_CHAR(ts[id]) ){
            NI_free(iar->ar) ; NI_free(iar) ; return NULL ; /* bad */
         }

         continue ;      /* try for something new */
      }

      switch( ts[id] ){  /* should be a type code */

        default: break ;   /* skip unknown characters */

        case 'b':
        case 's':
        case 'i':
        case 'f':
        case 'd':
        case 'c':
        case 'r':
        case 'R':
        case 'S':
        case 'L':
           num++ ; break ; /* add a single field */
      }
   }

   if( num <= 0 ){
      NI_free(iar->ar) ; NI_free(iar) ; return NULL ; /* bad */
   }

   /* allocate space for result */

   iar->num = num ;
   iar->ar  = NI_malloc(sizeof(int)*num) ;

   /* rescan and assign result */

   for( id=kk=0 ; id < lts ; id++ ){

      if( isdigit(ts[id]) ){   /* a count prefix */
         sscanf( ts+id , "%d%n" , &jd , &nn ) ;   /* get the count */
         id += nn ;      /* skip count prefix */
      } else {
         jd = 1 ;        /* default is a single count */
      }

      switch( ts[id] ){  /* type code (or junk) */

        default: break ;   /* skip junk characters */

        case 'b':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_BYTE ;
           break ;

        case 's':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_SHORT ;
           break ;

        case 'i':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_INT ;
           break ;

        case 'f':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_FLOAT ;
           break ;

        case 'd':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_DOUBLE ;
           break ;

        case 'c':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_COMPLEX ;
           break ;

        case 'r':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_RGB ;
           break ;

        case 'R':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_RGBA ;
           break ;

        case 'S':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_STRING ;
           break ;

        case 'L':
           for( kd=0 ; kd < jd ; kd++ )
              iar->ar[kk++] = NI_LINE ;
           break ;

      }
   }

   return iar ;
}

/*-----------------------------------------------------------------------*/
/*! Construct an empty element from a header.

    The data vectors will have space allocated, but they will be
    filled with all zero bytes.  If the header was "empty" (ended in
    "/>"), then no vectors will be allocated.
-------------------------------------------------------------------------*/

NI_element * make_empty_element( header_stuff *hs )
{
   NI_element *nel ;
   int ii , qq ;

   if( hs == NULL || hs->name == NULL ) return NULL ;

   nel = NI_malloc( sizeof(NI_element) ) ;

   /* move name and attributes from hs to new element */

   nel->name = hs->name ; hs->name = NULL ;

   nel->attr_num = hs->nattr ;

   if( nel->attr_num > 0 ){
      nel->attr_lhs = hs->lhs ; hs->lhs = NULL ;
      nel->attr_rhs = hs->rhs ; hs->rhs = NULL ;
   } else {
      nel->attr_lhs = nel->attr_rhs = NULL ;
   }

   /* set default vector parameters [indicating no data] */

   nel->vec_num = 0 ;
   nel->vec_len = 0 ;
   nel->vec_typ = NULL ;
   nel->vec     = NULL ;

   if( !hs->empty ){  /* find and process ni_* attributes about vectors */

     for( ii=0 ; ii < nel->attr_num ; ii++ ){  /* loop over attributes */

       /* ni_type attribute */

       if( nel->vec_typ == NULL && strcmp(nel->attr_lhs[ii],"ni_type") == 0 ){

         intarray *iar = decode_type_string( nel->attr_rhs[ii] ) ;

         if( iar != NULL ){
           nel->vec_num = iar->num ;  /* number of vectors */
           nel->vec_typ = iar->ar ;   /* vector types */
           NI_free(iar) ;             /* just the shell of the struct */
         }
       }

       /* ni_dimen attribute */

       else if( nel->vec_len == 0                         &&
                nel->attr_rhs[ii] != NULL                 &&
                strcmp(nel->attr_lhs[ii],"ni_dimen") == 0   ){

          int qq = strtol( nel->attr_rhs[ii] , NULL , 10 ) ;
          if( qq > 0 ) nel->vec_len = qq ;
       }

     }

     /* supply vector parameters if none was given */

     if( nel->vec_len == 0 ) nel->vec_len = 1 ;  /* default length */

     if( nel->vec_num == 0 ){                    /* default type */
        nel->vec_num    = 1 ;
        nel->vec_typ    = NI_malloc(sizeof(int)) ;
        nel->vec_typ[0] = NI_BYTE ;
     }

     /* now allocate space for vectors defined above */

     nel->vec = NI_malloc( sizeof(void *)*nel->vec_num ) ;

     for( ii=0 ; ii < nel->vec_num ; ii++ )
       nel->vec[ii] = NI_malloc(NI_type_size(nel->vec_typ[ii]) * nel->vec_len) ;
   }

   return nel ;
}

/*-------------------------------------------------------------------------*/
/*! Name for a given integer type code.  Return value is to static string. */

char * NI_type_name( int val )
{
   static char *NI_names[NI_NUM_TYPES] =
    { "byte"  , "short"  , "int"     ,
      "float" , "double" , "complex" ,
      "rgb"   , "String" , "Line"    ,
      "Rgba"
    } ;

   if( val < 0 || val >= NI_NUM_TYPES ) return NULL ;
   return NI_names[val] ;
}

/*-------------------------------------------------------------------------*/
/*! Byte size of a given integer type code. */

int NI_type_size( int val )
{
   switch( val ){
      case NI_BYTE:     return sizeof(byte)    ;
      case NI_SHORT:    return sizeof(short)   ;
      case NI_INT:      return sizeof(int)     ;
      case NI_FLOAT:    return sizeof(float)   ;
      case NI_DOUBLE:   return sizeof(double)  ;
      case NI_COMPLEX:  return sizeof(complex) ;
      case NI_RGB:      return sizeof(rgb)     ;
      case NI_RGBA:     return sizeof(rgba)    ;
      case NI_STRING:   return sizeof(char *)  ;
      case NI_LINE:     return sizeof(char *)  ;
   }
   return 0 ;
}

/*************************************************************************/
/********************* Functions for NIML I/O ****************************/
/*** See http://www.manualy.sk/sock-faq/unix-socket-faq.html for info. ***/
/*************************************************************************/

/*! To print a system error message. */

#define PERROR(x) perror(x)

#include <signal.h>

/*! for tcp - indicates that SIGPIPE is ignored */

static int nosigpipe = 0 ;

/*! How to close a socket, given the descriptor ss. */

#define CLOSEDOWN(ss) ( shutdown((ss),2) , close((ss)) )

/*! This is used to set the send/receive buffer size for sockets **/

#define SOCKET_BUFSIZE  NI_BUFSIZE

/*! This macro is used so I can replace recv() with something else if I want. */

#define tcp_recv recv

/*! This macro is used so I can replace send() with something else if I want. */

#define tcp_send send

#ifndef MIN
#  define MIN(a,b) (((a)>(b)) ? (b) : (a))
#endif

/*! Next delay in milliseconds, given current delay. */

#define NEXTDMS(dm) MIN(1.1*(dm)+1.01 , 1000.0)

/********************************************************************
  Routines to manipulate TCP/IP stream sockets.
*********************************************************************/

/*-------------------------------------------------------------------*/
/*!  See if the given socket (sd) is ready to read.

   msec is the number of milliseconds to wait:
     zero ==> no waiting
     < 0  ==> wait until something happens (not recommended)

   Return values are
     -1 = some error occured (socket closed at other end?)
      0 = socket is not ready to read
      1 = socket has data
---------------------------------------------------------------------*/

static int tcp_readcheck( int sd , int msec )
{
   int ii ;
   fd_set rfds ;
   struct timeval tv , * tvp ;

   if( sd < 0 ) return -1 ;                     /* bad socket id */

   FD_ZERO(&rfds) ; FD_SET(sd, &rfds) ;         /* check only sd */

   if( msec >= 0 ){                             /* set timer */
      tv.tv_sec  = msec/1000 ;
      tv.tv_usec = (msec%1000)*1000 ;
      tvp        = &tv ;
   } else {
      tvp        = NULL ;                       /* forever */
   }

   ii = select(sd+1, &rfds, NULL, NULL, tvp) ;  /* check it */
   if( ii == -1 ) PERROR( "tcp_readcheck select" ) ;
   return ii ;
}

/*-------------------------------------------------------------------*/
/*! See if the given socket is ready to write.

    msec = max amount of time to wait, in milliseconds.
     zero ==> no waiting
     < 0  ==> wait until something happens (not recommended)

   Return values are
     -1 = some error occured (socket closed at other end?)
      0 = socket is not ready to write
      1 = OK to write to socket
---------------------------------------------------------------------*/

static int tcp_writecheck( int sd , int msec )
{
   int ii ;
   fd_set wfds ;
   struct timeval tv , * tvp ;

   if( sd < 0 ) return -1 ;                     /* bad socket id */

   FD_ZERO(&wfds) ; FD_SET(sd, &wfds) ;         /* check only sd */

   if( msec >= 0 ){                             /* set timer */
      tv.tv_sec  = msec/1000 ;
      tv.tv_usec = (msec%1000)*1000 ;
      tvp        = &tv ;
   } else {
      tvp        = NULL ;                       /* forever */
   }

   ii = select(sd+1, NULL , &wfds, NULL, tvp) ;  /* check it */
   if( ii == -1 ) PERROR( "tcp_writecheck select" ) ;
   return ii ;
}

/*------------------------------------------------------------------------*/
/*! Set a socket so that it will cutoff quickly when it is closed.

   See http://www.manualy.sk/sock-faq/unix-socket-faq.html for more
   information about this stuff.
--------------------------------------------------------------------------*/

static void tcp_set_cutoff( int sd )
{

   if( sd < 0 ) return ;  /* bad input */

#if 1
   /* Turn off "lingering". */

   { struct linger lg ;
     lg.l_onoff  = 1 ;
     lg.l_linger = 0 ;
     setsockopt(sd, SOL_SOCKET, SO_LINGER, (void *)&lg, sizeof(struct linger)) ;
   }
#endif

#if 1
   /* Let the address be reused quickly,
      in case of another connection from the same host on the same port. */

   { int optval = 1;
     setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, (char *)&optval, sizeof(optval)) ;
   }
#endif

   return ;
}

/*-------------------------------------------------------------------*/
/*!  Check if an already active socket is still alive.

   If it is dead, then readcheck will say we can read, but we
   won't actually get any bytes when we try (using peek mode).
   Returns 1 if things are OK, 0 if not.
---------------------------------------------------------------------*/

static int tcp_alivecheck( sd )
{
   int ii ;
   char bbb[4] ;

   ii = tcp_readcheck(sd,0) ;                 /* can I read?          */
   if( ii == 0 ) return 1 ;                   /* can't read is OK     */
   if( ii <  0 ) return 0 ;                   /* some error is bad    */
   errno = 0 ;
   ii = tcp_recv( sd , bbb , 1 , MSG_PEEK ) ; /* try to read one byte */
   if( ii == 1 ) return 1 ;                   /* if we get it, good   */
   if( errno ) PERROR("tcp_alivecheck") ;
   return 0 ;                                 /* no data ==> death!   */
}

/*------------------------------------------------------------------------*/
/*!  Open a socket to the given host, to the given TCP port.

     This function is used to "reach out" to a server that is supposed
     to be listening on the same port.
     Returns socket id; if -1, some error occured (e.g., nobody listening).
--------------------------------------------------------------------------*/

static int tcp_connect( char * host , int port )
{
   int sd , l ;
   struct sockaddr_in sin ;
   struct hostent *   hostp ;

   if( host == NULL || port < 1 ) return -1 ;  /* bad inputs */

   /** open a socket **/

   sd = socket( AF_INET , SOCK_STREAM , 0 ) ;
   if( sd == -1 ){ PERROR("tcp_connect socket"); return -1; }

   /** set socket options (no delays, large buffers) **/

   l = 1;
   setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (void *)&l, sizeof(int)) ;
   l = SOCKET_BUFSIZE ;
   setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (void *)&l, sizeof(int)) ;
   setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (void *)&l, sizeof(int)) ;

   /** set port on remote computer **/

   memset( &sin , 0 , sizeof(sin) ) ;
   sin.sin_family = AF_INET ;
   sin.sin_port   = htons(port) ;

   /** set remote computer IP address from its name **/

   hostp = gethostbyname(host) ;
   if( hostp == NULL ){
      PERROR("tcp_connect gethostbyname"); CLOSEDOWN(sd); return -1;
   }
   sin.sin_addr.s_addr = ((struct in_addr *)(hostp->h_addr))->s_addr ;

   if( connect(sd , (struct sockaddr *)&sin , sizeof(sin)) == -1 ){
      PERROR("tcp_connect connect") ; CLOSEDOWN(sd); return -1;
   }

   return sd ;
}

/*--------------------------------------------------------------------------*/
/*! Set up to listen for a connection on a given port.

   This is intended for use by a server, which will wait for some other
   program to actively connect to this port.  There is no security here -
   connections will be taken from any IP address.

   This function does not actually form the connection.  That must be done
   separately.  Whether someone is trying to connect can be checked for
   with the routine "tcp_readcheck" and then accepted with "tcp_accept".

   The return value is the descriptor for the listening socket.
----------------------------------------------------------------------------*/

static int tcp_listen( int port )
{
   int sd , l ;
   struct sockaddr_in sin ;

   if( port < 1 ) return -1 ; /* bad input */

   /** open a socket **/

   sd = socket( AF_INET , SOCK_STREAM , 0 ) ;
   if( sd == -1 ){ PERROR("tcp_listen socket"); return -1; }

   /** set socket options (no delays, large buffers) **/

   l = 1;
   setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (void *)&l, sizeof(int)) ;
   l = SOCKET_BUFSIZE ;
   setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (void *)&l, sizeof(int)) ;
   setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (void *)&l, sizeof(int)) ;

   /** set port on remote computer **/

   memset( &sin , 0 , sizeof(sin) ) ;
   sin.sin_family      = AF_INET ;
   sin.sin_port        = htons(port) ;
   sin.sin_addr.s_addr = INADDR_ANY ;  /* reader reads from anybody */

   if( bind(sd , (struct sockaddr *)&sin , sizeof(sin)) == -1 ){
      PERROR("tcp_listen bind"); CLOSEDOWN(sd); return -1;
   }

   if( listen(sd,1) == -1 ){
      PERROR("tcp_listen listen"); CLOSEDOWN(sd); return -1;
   }

   return sd ;
}

/*--------------------------------------------------------------------------*/
/*! Accept incoming connection on a socket.

   Return value is the attached socket (which is not the original socket!).
   If -1 is returned, some error occured.  If the accept works, then the
   original socket is still open and listening for further attachments.
   Under many circumstances, you will want to close the original socket
   immediately.

   If hostname is not NULL, then the char * it points to will be filled
   with a pointer to the official name of the host that connected.

   If hostaddr is not NULL, then the char * it points to will be filled
   with a pointer to the Internet address (in 'dot' form) of the host that
   connected.

   Both the char * pointers returned are from malloc, and should be free()-d
   when no longer needed.  If they aren't needed at all, just pass in NULL
   for these arguments.

   Note that this routine will block until somebody connects.  You can
   use tcp_readcheck(sd,0) to see if anyone is waiting to connect before
   calling this routine.
---------------------------------------------------------------------------*/

static int tcp_accept( int sd , char ** hostname , char ** hostaddr )
{
   struct sockaddr_in pin ;
   int addrlen , sd_new ;
   struct hostent * hostp ;
   char * sout , * str ;

   /** accept the connection **/

   addrlen = sizeof(pin) ;
   sd_new = accept( sd , (struct sockaddr *)&pin , &addrlen ) ;
   if( sd_new == -1 ){ PERROR("tcp_accept"); return -1; }

   /** get name of connector **/

   if( hostname != NULL ){
      hostp = gethostbyaddr( (char *) (&pin.sin_addr) ,
                             sizeof(struct in_addr) , AF_INET ) ;
      if( hostp != NULL ){
         sout = (char *) malloc( strlen(hostp->h_name)+1 ) ;
         strcpy(sout,hostp->h_name) ;
      } else {
         sout = (char *) malloc( strlen("UNKNOWN")+1 ) ;
         strcpy(sout,"UNKNOWN") ;
      }
      *hostname = sout ;
   }

   /** get address of connector **/

   if( hostaddr != NULL ){
      str = inet_ntoa( pin.sin_addr ) ;
      sout = (char *) malloc( strlen(str)+1 ) ;
      strcpy(sout,str) ;
      *hostaddr = sout ;
   }

   return sd_new ;
}

/*-----------------------------------------------------------------*/
/*! Open a NIML input or output stream, and return a pointer to it.

    NULL is returned if an error occurs.

  name = "tcp:host:port" to connect a socket to system "host"
             on the given port number.
  name = "file:filename" to open a file for I/O.

  mode = "w" to open a stream for writing
           * tcp: host must be specified
             "w" is for a tcp client

  mode = "r" to open a stream for reading
           * tcp: host is ignored (but must be present);
             "r" is for a tcp server

  For a file: stream, you can either read or write from the stream,
  but not both.  For a tcp: stream, once it is connected, you can
  both read and write.

  The inputs "host" (for tcp:) and "filename" (for file:) are
  limited to a maximum of 127 bytes.

  Since opening a socket requires sychronizing two processes,
  you can't read or write to a tcp: stream immediately.  Instead
  you have to check if it is "good" first.  This can be done using
  the function NI_stream_goodcheck().

  After an tcp: "r" stream is good, then the string ns->name
  contains the IP address of the connecting host, in "dot" form
  (e.g., "201.202.203.204"); here, "ns" is the NI_stream returned
  by this routine.

  For a file: stream, ns->name contains the filename.
------------------------------------------------------------------------*/

NI_stream NI_stream_open( char *name , char *mode )
{
   NI_stream_type *ns ;
   int do_create , do_accept ;

   /** check if inputs are reasonable **/

   if( name == NULL || strlen(name) < 6 || strlen(name) > 127 ) return NULL ;

   if( mode == NULL ) return NULL ;

   do_create = (*mode == 'w') ;
   do_accept = (*mode == 'r') ;

   if( !do_create && !do_accept ) return NULL ;

   /***** deal with TCP/IP sockets *****/

   if( strncmp(name,"tcp:",4) == 0 ){
      char host[128] , *hend ;
      int  port=-1 , ii , jj ;

      /** find "host" substring **/

      hend = strstr( name+4 , ":" ) ;
      if( hend == NULL || hend-name > 128 ) return NULL ;

      for( ii=4 ; name[ii] != ':' ; ii++ ) host[ii-4] = name[ii] ;
      host[ii-4] = '\0' ;

      /** get "port" number **/

      port = strtol( name+ii+1 , NULL , 10 ) ;
      if( port <= 0 ) return NULL ;

      /** initialize NI_stream_type output struct **/

      ns = NI_malloc( sizeof(NI_stream_type) ) ;

      ns->type = NI_TCP_TYPE;   /* what kind is this? */
      ns->port = port ;         /* save the port #    */
      ns->nbuf = 0 ;            /* buffer is empty    */

      /** attach to incoming call "r" **/

      if( do_accept ){
         ns->io_mode = NI_INPUT_MODE ;
         ns->sd = tcp_listen( port ) ;                   /* set up to listen  */
         if( ns->sd < 0 ){                               /* error? must die!  */
            NI_free(ns) ; return NULL ;
         }
         ns->bad = TCP_WAIT_ACCEPT ;                     /* not connected yet */
         ii = tcp_readcheck(ns->sd,1) ;                  /* see if ready      */
         if( ii > 0 ){                                   /* if socket ready:  */
            jj = tcp_accept( ns->sd , NULL,&hend ) ;     /* accept connection */
            if( jj >= 0 ){                               /* if accept worked  */
               CLOSEDOWN( ns->sd ) ;                     /* close old socket  */
               NI_strncpy(ns->name,hend,128) ;           /* put IP into name  */
               free(hend) ; ns->bad = 0 ; ns->sd = jj ;  /* and ready to go!  */
            }
         }
         return ns ;
      }

      /** place an outgoing call "w" **/

      if( do_create ){
         struct hostent *hostp ;
         ns->io_mode = NI_OUTPUT_MODE ;
         hostp = gethostbyname(host) ;                   /* lookup host on net */
         if( hostp == NULL ){                            /* fails? must die!   */
             NI_free(ns) ; return NULL ;
         }
         ns->sd  = tcp_connect( host , port ) ;          /* connect to host    */
         ns->bad = (ns->sd < 0) ? TCP_WAIT_CONNECT : 0 ; /* fails? must wait   */
         NI_strncpy(ns->name,host,128) ;                 /* save the host name */
         return ns ;
      }
      return NULL ;  /* should never be reached */
   }

   /***** deal with simple files *****/

   if( strncmp(name,"file:",5) == 0 ){

      char *fname = name+5 ;
      FILE *fp ;

      if( strlen(fname) < 1 ) return NULL ;

      fp = fopen( fname , do_create ? "wb"       /* always in binary mode */
                                    : "rb" ) ;

      if( fp == NULL ) return NULL ;

      /** initialize NI_stream_type output **/

      ns = NI_malloc( sizeof(NI_stream_type) ) ;

      ns->type     = NI_FILE_TYPE;   /* what kind is this? */
      ns->nbuf     = 0 ;             /* buffer is empty    */
      ns->fp       = fp ;
      ns->io_mode  = do_create ? NI_OUTPUT_MODE
                               : NI_INPUT_MODE  ;
      ns->bad      = 0 ;

      NI_strncpy( ns->name , fname , 128 ) ;
      return ns ;
   }

   return NULL ;  /* should never be reached */
}

/*-----------------------------------------------------------------------*/
/*!  Check if the given NI_stream is ready for I/O.

   If not, wait up to msec milliseconds to establish the connection to
   the other end; if msec < 0, will wait indefinitely.
   Returns 1 if ready; 0 if not; -1 if an error occurs.
   Possible errors are:
     * ns was connected, and now has become disconnected
     * ns is passed in as NULL
-------------------------------------------------------------------------*/

int NI_stream_goodcheck( NI_stream_type *ns , int msec )
{
   int ii , jj ;
   char * bbb ;

   /** check inputs for OK-osity **/

   if( ns == NULL ) return -1 ;

   switch( ns->type ){

      /** File I/O: is good if file is open (should always be true) **/

      case NI_FILE_TYPE:
        if( ns->fp == NULL ) return -1 ;
        return 1 ;

      /** Socket I/O **/ 

      case NI_TCP_TYPE:
        if( ns->bad == 0 ){  /** if good before, then check if is still good **/
           int ich = 1 ;
           ich = tcp_alivecheck(ns->sd) ;
           if( ich == 0 ) return -1 ;
           else           return  1 ;
        }

        /** wasn't good before, so check if that condition has changed **/

        /** TCP/IP waiting to accept call from another host **/

        if( ns->bad == TCP_WAIT_ACCEPT ){
           ii = tcp_readcheck(ns->sd,msec) ;            /* see if ready      */
           if( ii > 0 ){                                /* if socket ready:  */
              jj = tcp_accept( ns->sd , NULL,&bbb ) ;   /* accept connection */
              if( jj >= 0 ){                            /* if accept worked  */
                 CLOSEDOWN( ns->sd ) ;                  /* close old socket  */
                 NI_strncpy(ns->name,bbb,128) ;         /* put IP into name  */
                 free(bbb); ns->bad = 0; ns->sd = jj;   /* and ready to go!  */
              }
           }
        }

        /** TCP/IP waiting to connect call to another host **/

        else if( ns->bad == TCP_WAIT_CONNECT ){
           int dms=0 , ms ;

           if( msec < 0 ) msec = 999999999 ;        /* a long time (11+ days) */
           for( ms=0 ; ms < msec ; ms += dms ){
             ns->sd = tcp_connect( ns->name , ns->port );  /* try to connect  */
             if( ns->sd >= 0 ) break ;                     /* worked? get out */
             dms = NEXTDMS(dms); dms = MIN(dms,msec-ms); NI_sleep(dms);
           }
           if( ns->sd < 0 )                                  /* one last try? */
              ns->sd  = tcp_connect( ns->name , ns->port ) ;

           if( ns->sd >= 0 ) ns->bad = 0 ;                   /* succeeded?    */
        }

        /** see if it turned from bad to good **/

        return (ns->bad == 0) ;
   }

   return -1 ;  /* unreachable, I hope */
}

/*-----------------------------------------------------------------------*/
/*! Close a NI_stream.  Note that this will free what ns points to.

    Use the NI_STREAM_CLOSE macro to also set the pointer "ns" to NULL.
-------------------------------------------------------------------------*/

void NI_stream_close( NI_stream_type *ns )
{
   if( ns == NULL ) return ;

   switch( ns->type ){

      case NI_FILE_TYPE:
        if( ns->fp != NULL ) fclose(ns->fp) ;
        ns->fp = NULL ;
      break ;

      case NI_TCP_TYPE:
        if( ns->sd >= 0 ) CLOSEDOWN(ns->sd) ;
        ns->sd = -1 ;
      break ;
   }

   NI_free(ns) ; return ;
}

/*--------------------------------------------------------------------------*/
/*! Return the file length (-1 if file not found). */

static long NI_filesize( char *pathname )
{
   static struct stat buf ; int ii ;

   if( pathname == NULL ) return -1 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return -1 ;
   return buf.st_size ;
}

/*---------------------------------------------------------------------------*/
/*!  Check if the NI_stream is ready to have data read out of it.

  If not, the routine will wait up to msec milliseconds for data to be
  available.  If msec < 0, this routine will wait indefinitely.
  The return value is 1 if data is ready, 0 if not.
  -1 will be returned if some unrecoverable error is detected.
-----------------------------------------------------------------------------*/

int NI_stream_readcheck( NI_stream_type *ns , int msec )
{
   int ii ;

   /** check if the NI_stream is good **/

   ii = NI_stream_goodcheck(ns,0) ;
   if( ii == -1 ) return -1 ;             /* some error */
   if( ii == 0  ){                        /* not good yet */
      ii = NI_stream_goodchek(ns,msec) ;  /* so wait for it to get good */
      if( ii != 1 ) return 0 ;            /* if still not good, exit */
   }

   switch( ns->type ){

      /** tcp: ==> just use the Unix "select" mechanism **/

      case NI_TCP_TYPE:
        ii = tcp_alivecheck( ns->sd ) ; if( !ii ) return -1 ;
        ii = tcp_readcheck( ns->sd , msec ) ;
        return ii ;

      /** file: ==> check current file position and length of file **/

      case NI_FILE_TYPE:{
         long f_len , f_pos ;

         f_len = NI_filesize( ns->name ) ;
         if( f_len < 0 ) return -1 ;

         f_pos = ftell( ns->fp ) ;
         if( f_pos < 0 ) return -1 ;

         return (f_pos < f_len) ;
      }
   }

   return -1 ;  /* should never happen */
}

/*---------------------------------------------------------------------------*/
/*!  Check if the NI_stream is ready to have data written into it.

  If not, the routine will wait up to msec milliseconds for writing to
  be allowable.  If msec < 0, this routine will wait indefinitely.
  The return value is 1 if data can be sent, 0 if not.
  -1 will be returned if some unrecoverable error is detected.
-----------------------------------------------------------------------------*/

int NI_stream_writecheck( NI_stream_type *ns , int msec )
{
   int ii ;

   /** check if the NI_stream is good **/

   ii = NI_stream_goodcheck(ns,0) ;
   if( ii == -1 ) return -1 ;             /* some error */
   if( ii == 0  ){                        /* not good yet */
      ii = NI_stream_goodcheck(ns,msec);  /* so wait for it to get good */
      if( ii != 1 ) return ii ;           /* if still not good, exit */
   }

   switch( ns->type ){

      /** tcp: ==> just use the Unix "select" mechanism **/

      case NI_TCP_TYPE:
        return tcp_writecheck( ns->sd , msec ) ;

      /** file: ==> if the file was opened in write mode **/

      case NI_FILE_TYPE:
        return (ns->io_mode == NI_OUTPUT_MODE) ;
   }

   return -1 ;  /* should never be reached */
}

/*----------------------------------------------------------------------------*/
/*!  Send nbytes of data from buffer down the NI_stream.

  Return value is the number of bytes actually sent, or is -1 if some error
  occurs.

  tcp: We use blocking sends, so that all the data should be sent properly
       unless the connection to the other end fails for some reason
       (e.g., the planet explodes in a fiery cataclysm).

  file: Everything should be written, unless the filesystem fills up.
------------------------------------------------------------------------------*/

int NI_stream_write( NI_stream_type *ns , char *buffer , int nbytes )
{
   int ii , nsent ;

   /** check for reasonable inputs **/

   if( ns     == NULL || ns->bad    ||
       buffer == NULL || nbytes < 0   ) return -1 ;

   if( nbytes == 0 ) return 0 ;  /* that was easy */

   ii = NI_stream_goodcheck(ns,0) ;
   if( ii != 1 ) return ii ;

   ii = NI_stream_writecheck(ns,1) ;
   if( ii <= 0 ) return ii ;

   switch( ns->type ){

     /** tcp: ==> just use send **/

     case NI_TCP_TYPE:

       /* turn off SIGPIPE signals, which will otherwise be
          raised if we send to a socket when the other end has crashed */

       if( !nosigpipe ){ signal(SIGPIPE,SIG_IGN); nosigpipe = 1; }

       nsent = tcp_send( ns->sd , buffer , nbytes , 0 ) ;
       if( nsent < nbytes ) PERROR("tcp send") ;
       return nsent ;

     /** file: ==> just fwrite **/

     case NI_FILE_TYPE:
       nsent = fwrite( buffer , 1 , nbytes , ns->fp ) ;
       if( nsent < nbytes ) PERROR("fwrite") ;
       return nsent ;
   }

   return -1 ;  /* should not be reached */
}

/*-------------------------------------------------------------------------*/
/*!  Read up to nbytes of data from the NI_stream, into buffer.

   Returns the number of bytes actually read.  For both the case of
   sockets and files, this may be less than nbytes (may even be 0).
   If an error occurs, -1 is returned.
---------------------------------------------------------------------------*/

int NI_stream_read( NI_stream_type *ns , char *buffer , int nbytes )
{
   int ii ;

   /** check for reasonable inputs **/

   if( ns     == NULL || ns->bad    ||
       buffer == NULL || nbytes < 0   ) return -1 ;

   if( nbytes == 0 ) return 0 ;
   if( NI_stream_goodcheck(ns,0) != 1 ) return -1 ;

   switch( ns->type ){

     /** tcp: just use recv **/

     case NI_TCP_TYPE:
       ii = tcp_recv( ns->sd , buffer , nbytes , 0 ) ;
       if( ii == -1 ) PERROR("tcp recv") ;
       return ii ;

     /** file: just use fread **/

     case NI_FILE_TYPE:
       ii = fread( buffer , 1 , nbytes , ns->fp ) ;
       if( ii == -1 ) PERROR("fread") ;
       return ii ;
   }

   return -1 ;  /* should not be reached */
}

/*------------------------------------------------------------------*/
/*!  Sleep a given # of milliseconds (uses the Unix select routine) */

void NI_sleep( int msec )
{
   struct timeval tv ;
   if( msec <= 0 ) return ;
   tv.tv_sec  = msec/1000 ;
   tv.tv_usec = (msec%1000)*1000 ;
   select( 1 , NULL,NULL,NULL , &tv ) ;
   return ;
}
