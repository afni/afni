/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "thd_iochan.h"
#include "Amalloc.h"
#include <errno.h>

static char *error_string=NULL ; /* 21 Nov 2001 */

char *iochan_error_string(void){ return error_string; }

#ifndef DONT_USE_SHM
static int shm_RMID_delay = 0 ;  /* 12 Dec 2002 */
#endif

/*****************************************************************
  Routines to manipulate IOCHANs, something RWCox invented as
  an abstraction of socket or shmem inter-process communication.

  The function iochan_init creates an IOCHAN, and returns a
    pointer to it.
  The function iochan_goodcheck() checks if an IOCHAN is ready
    (connected at both ends); if not, it will wait until the
    connection is made, or until a given amount of time has passed.
  The functions iochan_readcheck() and iochan_writecheck() will
    check if an IOCHAN is good for I/O, and is ready to accept
    reads and writes, respectively.
  The functions iochan_send() and iochan_recv() will transmit
     and receive messages, respectively.  There are also
     the routines iochan_sendall() and iochan_recvall() that
     will block until the specified number of bytes is
     transmitted or received, respectively.
  The function iochan_ctl() allows control of internal IOCHAN
     parameters.

  Socket and shmem IOCHANs are not fully symmetric:
  * Sockets are bidirectional, but shmem IOCHANs can only
      communicate in one direction.
  * Sending data to a socket will result in all the data being
      transmitted, even if the process must block.  Sending
      data to a shmem may result in only part of the data
      being transmitted (if the buffer space available
      isn't big enough to hold the whole message).

  Shmem IOCHANs are implemented as a circular buffer with two
  int indices "bstart" and "bend"; the data between the
  bstart-th and bend-th bytes of the buffer (inclusive) are
  ready to be read.  Writing to the buffer involves increasing
  bend, and is done only by the "writing" process.  Reading from
  the buffer involves increasing bstart, and is done only by the
  "reading" process.  That is, the two processes do not write
  into the same control variables.  (Unless they both try to
  read AND write into the same IOCHAN.)

  June 1997:
  Shmem IOCHANs now can be bidirectional.  This is ordered
  by using the new "size1+size2" specification.
******************************************************************/

/*---------------------------------------------------------------*/
static int pron = 1 ;                             /* 22 Nov 2002 */
void iochan_enable_perror( int q ){ pron = q; }   /* ditto */

#undef DEBUG
#ifdef DEBUG
#  define PERROR(x) perror(x)
#  define STATUS(x) fprintf(stderr,"%s\n",x)
#else
   static char *pqlast = NULL  ;
   static double pqtim = -6.66 ;
#  define PERROR(x)                                                    \
     do{ if( (x) != NULL && pron ){                                    \
           double qtim = COX_clock_time() ;                            \
           int skip = ( qtim-pqtim < 0.666 &&                          \
                        pqlast     != NULL && strcmp(pqlast,x) == 0 ); \
           if( !skip ){                                                \
             perror(x); pqtim = qtim;                                  \
             if( pqlast != NULL ) free(pqlast) ;                       \
             pqlast = strdup(x) ;                                      \
           }                                                           \
        }} while(0)
#  define STATUS(x) /* nada */
#endif
/*---------------------------------------------------------------*/

#include <signal.h>
static int nosigpipe = 0 ;  /* 20 Apr 1997: turn off SIGPIPE signals */

#define USE_SHUTDOWN
#ifdef USE_SHUTDOWN
#  define CLOSEDOWN(ss) ( shutdown((ss),2) , close((ss)) )
#else
#  define CLOSEDOWN(ss) close((ss))
#endif

/** this is used to set the send/receive buffer size for sockets **/

#define SOCKET_BUFSIZE  (31*1024)

/********************************************************************
  Routines to manipulate TCP/IP stream sockets.
*********************************************************************/

/*-------------------------------------------------------------------
   See if the given socket (sd) is ready to read.
   msec is the number of milliseconds to wait:
     zero ==> no waiting
     < 0  ==> wait until something happens

   Return values are
     -1 = some error occured (socket closed at other end?)
      0 = socket is not ready to read
      1 = socket has data
---------------------------------------------------------------------*/

int tcp_readcheck( int sd , int msec )
{
   int ii ;
   fd_set rfds ;
   struct timeval tv , * tvp ;

   if( sd < 0 ){ STATUS("tcp_readcheck: illegal sd") ; return -1 ; } /* bad socket id */

   FD_ZERO(&rfds) ; FD_SET(sd, &rfds) ;         /* check only sd */

   if( msec >= 0 ){                             /* set timer */
      tv.tv_sec  = msec/1000 ;
      tv.tv_usec = (msec%1000)*1000 ;
      tvp        = &tv ;
   } else {
      tvp        = NULL ;                       /* forever */
   }

   /** STATUS("tcp_readcheck: call select") ; **/

   ii = select(sd+1, &rfds, NULL, NULL, tvp) ;  /* check it */
   if( ii == -1 ) PERROR( "Socket gone bad? tcp_readcheck[select]" ) ;
   return ii ;
}

int tcp_writecheck( int sd , int msec )
{
   int ii ;
   fd_set wfds ;
   struct timeval tv , * tvp ;

   if( sd < 0 ){ STATUS("tcp_writecheck: illegal sd") ; return -1 ; } /* bad socket id */

   FD_ZERO(&wfds) ; FD_SET(sd, &wfds) ;         /* check only sd */

   if( msec >= 0 ){                             /* set timer */
      tv.tv_sec  = msec/1000 ;
      tv.tv_usec = (msec%1000)*1000 ;
      tvp        = &tv ;
   } else {
      tvp        = NULL ;                       /* forever */
   }

   /** STATUS("tcp_writecheck: call select") ; **/

   ii = select(sd+1, NULL , &wfds, NULL, tvp) ;  /* check it */
   if( ii == -1 ) PERROR( "Socket gone bad? tcp_writecheck[select]" ) ;
   return ii ;
}

/*------------------------------------------------------------------------
  Version of recv() that will try again if interrupted by a signal
  before any data arrives.
--------------------------------------------------------------------------*/
#ifdef USE_TCP_RECV
int tcp_recv( int s , void * buf , int len , unsigned int flags )
{
   int ii ;

   while(1){
      ii = recv( s , buf , len , flags ) ;
      if( ii >= 0 )        return ii ;
      if( errno != EINTR ) return ii ;
   }
   return 0 ;
}
#endif

/*------------------------------------------------------------------------
   Set a socket so that it will cutoff quickly when it is closed.
--------------------------------------------------------------------------*/

void tcp_set_cutoff( int sd )
{
#ifdef SO_LINGER
   { struct linger lg ;
     lg.l_onoff  = 1 ;
     lg.l_linger = 0 ;
     setsockopt(sd, SOL_SOCKET, SO_LINGER, (void *)&lg, sizeof(struct linger)) ;
   }
#endif
#ifdef SO_REUSEADDR
   { int optval = 1;
     setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, (char *)&optval, sizeof(optval)) ;
   }
#endif
   return ;
}

/*------------------------------------------------------------------------
   Check if an already active socket is still alive.
   If it is dead, then readcheck will say we can read, but we
   won't actually get any bytes when we try (using peek mode).
   Returns 1 if things are OK, 0 if not.
--------------------------------------------------------------------------*/

int tcp_alivecheck( int sd )
{
   int ii ;
   char bbb[4] ;

   ii = tcp_readcheck(sd,0) ;                 /* can I read?          */
   if( ii == 0 ) return 1 ;                   /* can't read is OK     */
   if( ii <  0 ) return 0 ;                   /* some error is bad    */
   errno = 0 ;
   ii = tcp_recv( sd , bbb , 1 , MSG_PEEK ) ; /* try to read one byte */
   if( ii == 1 ) return 1 ;                   /* if we get it, good   */
   if( errno ) PERROR("Socket gone bad? tcp_alivecheck[tcp_recv]") ;
   return 0 ;                                 /* no data ==> death!   */
}

/*------------------------------------------------------------------------
   Open a socket to the given host, to the given TCP port.
   Returns socket id; if -1, some error occured (e.g., nobody listening).
--------------------------------------------------------------------------*/

int tcp_connect( char * host , int port )
{
   int sd , l ;
   struct sockaddr_in sin ;
   struct hostent *   hostp ;

   if( host == NULL || port < 1 ){ STATUS("tcp_connect: illegal inputs") ; return -1 ; }

   /** open a socket **/

   sd = socket( AF_INET , SOCK_STREAM , 0 ) ;
   if( sd == -1 ){ PERROR("Can't create? tcp_connect[socket]"); return -1; }

   /** set socket options (no delays, large buffers) **/

#if 0
   l = 1;
   setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (void *)&l, sizeof(int)) ;
#endif
   l = SOCKET_BUFSIZE ;
   setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (void *)&l, sizeof(int)) ;
   setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (void *)&l, sizeof(int)) ;

   /** set port on remote computer **/

   memset( &sin , 0 , sizeof(sin) ) ;
   sin.sin_family = AF_INET ;
   sin.sin_port   = htons(port) ;

   /** set remote computer **/

   hostp = gethostbyname(host) ;
   if( hostp == NULL ){
      PERROR("Can't lookup? tcp_connect[gethostbyname]"); CLOSEDOWN(sd); return -1;
   }
   sin.sin_addr.s_addr = ((struct in_addr *)(hostp->h_addr))->s_addr ;

   if( connect(sd , (struct sockaddr *)&sin , sizeof(sin)) == -1 ){
      PERROR("Can't connect? tcp_connect[connect]") ; CLOSEDOWN(sd); return -1;
   }

   return sd ;
}

/*--------------------------------------------------------------------------
   Set up to listen for a connection on a given port.  This does not
   actually form the connection.  That must be done separately.
   Whether someone is trying to connect can be checked for with the routine
   "tcp_readcheck" and then accepted with "tcp_accept".

   The return value is the descriptor for the listening socket.
----------------------------------------------------------------------------*/

int tcp_listen( int port )
{
   static int nobindmsg;
   int sd , l ;
   struct sockaddr_in sin ;

   if( port < 1 ){ STATUS("tcp_listen: illegal port") ; return -1 ; }

   /** open a socket **/

   sd = socket( AF_INET , SOCK_STREAM , 0 ) ;
   if( sd == -1 ){ PERROR("Can't create? tcp_listen[socket]"); return -1; }

   /** set socket options (no delays, large buffers) **/

#if 0
   l = 1;
   setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (void *)&l, sizeof(int)) ;
#endif
   l = SOCKET_BUFSIZE ;
   setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (void *)&l, sizeof(int)) ;
   setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (void *)&l, sizeof(int)) ;

   /** set port on remote computer **/

   memset( &sin , 0 , sizeof(sin) ) ;
   sin.sin_family      = AF_INET ;
   sin.sin_port        = htons(port) ;
   sin.sin_addr.s_addr = INADDR_ANY ;  /* reader reads from anybody */

   if( bind(sd , (struct sockaddr *)&sin , sizeof(sin)) == -1 ){
      if (!(nobindmsg % 10000)) { /* slow message printing down! ZSS */
         PERROR("\nCan't bind? tcp_listen[bind]");
         nobindmsg = 0;
      }
      ++nobindmsg;
      CLOSEDOWN(sd); return -1;
   }

   if( listen(sd,1) == -1 ){
      PERROR("Can't listen? tcp_listen[listen]"); CLOSEDOWN(sd); return -1;
   }

   return sd ;
}

/*--------------------------------------------------------------------------
   Accept incoming connection on a socket.  Return value is the attached
   socket (which is not the original socket!).  If -1 is returned, some
   error occured.  If the accept works, then the original socket is
   still open and listening for further attachments.

   If hostname is not NULL, then the char * it points to will be filled
   with a pointer to the official name of the host that connected.

   If hostaddr is not NULL, then the char * it points to will be filled
   with a pointer to the Internet address (in 'dot' form) of the host that
   connected.

   Both the char * pointers returned are from malloc, and should be free-d
   when no longer needed.  If they aren't needed at all, just pass in NULL
   for these arguments.

   Note that this routine will block until somebody connects.  You can
   use tcp_readcheck(sd,0) to see if anyone is waiting to connect before
   calling this routine.
----------------------------------------------------------------------------*/

int tcp_accept( int sd , char ** hostname , char ** hostaddr )
{
   struct sockaddr_in pin ;
   int addrlen , sd_new ;
   struct hostent * hostp ;
   char * sout , * str ;

   /** accept the connection **/

   /** STATUS("tcp_accept: about to call accept") ; **/

   addrlen = sizeof(pin) ;
   sd_new = accept( sd , (struct sockaddr *)&pin , &addrlen ) ;
   if( sd_new == -1 ){ PERROR("Can't accept? tcp_accept[accept]"); return -1; }

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

/****************************************************************
  Routines to manipulate IPC shared memory segments
*****************************************************************/

/*---------------------------------------------------------------
   Convert a string to a key, for IPC operations.
-----------------------------------------------------------------*/

key_t string_to_key( char * key_string )
{
   int ii , sum ;

   sum = 666 ;
   if( key_string == NULL ) return (key_t) sum ;

   for( ii=0 ; key_string[ii] != '\0' ; ii++ )
      sum += ((int)key_string[ii]) << ((ii%3)*8) ;

        if( sum  < 0 ) sum = -sum      ;
   else if( sum == 0 ) sum = 314159265 ;

   return (key_t) sum ;
}

#ifndef DONT_USE_SHM
/*---------------------------------------------------------------
   Get a pre-existing shmem segment.
   Returns the shmid >= 0 if successful; returns -1 if failure.
-----------------------------------------------------------------*/

int shm_accept( char * key_string )
{
   key_t key ;
   int   shmid ;

   key   = string_to_key( key_string ) ;
   shmid = shmget( key , 0 , 0777 ) ;
   return shmid ;
}

/*---------------------------------------------------------------
   Connect to, or create if needed, a shmem segment.
   Returns the shmid >= 0 if successful; returns -1 if failure.
-----------------------------------------------------------------*/

int shm_create( char * key_string , int size )
{
   key_t key ;
   int   shmid ;

   key   = string_to_key( key_string ) ;
   shmid = shmget( key , size , 0777 | IPC_CREAT ) ;
   if( shmid < 0 ){
     PERROR("Can't create? shm_create[shmget]") ;
     if( pron ) fprintf(stderr,"key_string=%s key=%d size=%d\n",
                        key_string , (int)key , size ) ;
   }
   return shmid ;
}

/*---------------------------------------------------------------
   Actually attach to the shmem segment.
   Returns the pointer to the segment start.
   NULL is returned if an error occurs.
-----------------------------------------------------------------*/

char * shm_attach( int shmid )
{
   char * adr ;
   adr = (char *) shmat( shmid , NULL , 0 ) ;
   if( adr == (char *) -1 ){
     adr = NULL ; PERROR("Can't attach? shm_attach[shmat]") ;
   }
   return adr ;
}

/*---------------------------------------------------------------
   Find the size of a shmem segment.
   Returns -1 if an error occurs.
-----------------------------------------------------------------*/

int shm_size( int shmid )
{
   int ii ;
   struct shmid_ds buf ;

   if( shmid < 0 ) return -1 ;
   ii = shmctl( shmid , IPC_STAT , &buf ) ;
   if( ii < 0 ){ PERROR("Can't check? shm_size[shmctl]");  return -1; }
   return buf.shm_segsz ;
}

/*----------------------------------------------------------------
   Find the number of attaches to a shmem segment.
   Returns -1 if an error occurs (like the segment was destroyed).
------------------------------------------------------------------*/

int shm_nattach( int shmid )
{
   int ii ;
   struct shmid_ds buf ;

   if( shmid < 0 ){ STATUS("shm_nattach: illegal shmid") ; return -1 ; }
   errno = 0 ;
   ii = shmctl( shmid , IPC_STAT , &buf ) ;
   if( ii < 0 ){
     PERROR("Has shared memory buffer gone bad? shm_nattach[shmctl]") ;
     return -1 ;
   }
   return buf.shm_nattch ;
}

#else  /** dummy functions, if SysV IPC shared memory isn't available */

int    shm_nattach( int shmid )                   { return -1 ; }
int    shm_size   ( int shmid )                   { return -1 ; }
char * shm_attach ( int shmid )                   { return NULL;}
int    shm_create ( char * key_string , int size ){ return -1 ; }
int    shm_accept ( char * key_string )           { return -1 ; }

#endif /* DONT_USE_SHM */

/****************************************************************
  The IOCHAN routines, which call the tcp_ or shm_ routines,
  as needed.
*****************************************************************/

/*---------------------------------------------------------------
  Create an IOCHAN struct, and return a pointer to it.  NULL is
  returned if an error occurs.

  name = "tcp:host:port" to connect a socket to system "host"
             on the given port.
       = "shm:name:size" to connect a shared memory segment
             with the given name and size in bytes (size can
             end with K or M to denote kilobytes or megabytes).
             With this mode, the creator and acceptor processes
             cannot both read and write -- one of them must
             be the writing process (normally the creator),
             and one must be the reading process (normally
             the acceptor).
       = "shm:name:size1+size2" to connect a shared memory
             segment with buffers of length size1 and size2.
             The creator process will write to the size1 buffer
             and read from the size2 buffer.  The acceptor
             process will reverse this.

  mode = "create" to open a new channel
           * tcp: host must be specified
           * shm: size must be > 0
                  (if in the form size1+size2, both must be > 0)
       = "accept" to log into a channel created by someone else
           * tcp: host is ignored (but must be present)
           * shm: size is ignored (but must be present)
                  (similarly, size1+size2 must be present, but
                   their actual values are ignored)

  The inputs "host" (for tcp:) and "name" (for shm:) are limited
  to a maximum of 127 bytes.

  After an tcp: accept IOCHAN is good, then the string ioc->name
  contains the IP address of the connecting host, in "dot" form
  (e.g., "201.201.201.201"); here, "ioc" is the IOCHAN * returned
  by this routine.
-----------------------------------------------------------------*/

IOCHAN * iochan_init( char * name , char * mode )
{
   IOCHAN * ioc ;
   int do_create , do_accept ;

   /** 12 Dec 2002: check if shm_RMID_delay needs to be set **/

#ifndef DONT_USE_SHM
   { static int first=1 ;
     if( first ){
       char *eee = getenv("IOCHAN_DELAY_RMID") ;
       shm_RMID_delay = ( eee != NULL && (*eee=='Y' || *eee=='y') ) ;
       first = 0 ;
     }
   }
#endif

   /** check if inputs are reasonable **/

   error_string = NULL ;

   if( name == NULL || strlen(name) < 6 || strlen(name) > 127 ){
      error_string = "iochan_init: bad name" ; return NULL ;
   }

   if( mode == NULL ){
      error_string = "iochan_init: bad mode" ; return NULL ;
   }

   do_create = (strcmp(mode,"create") == 0 || strcmp(mode,"w") == 0) ;
   do_accept = (strcmp(mode,"accept") == 0 || strcmp(mode,"r") == 0) ;

   if( !do_create && !do_accept ){
      error_string = "iochan_init: bad mode" ; return NULL ;
   }

#ifdef DEBUG
fprintf(stderr,"iochan_init: name=%s  mode=%s\n",name,mode) ;
#endif

   /***** deal with TCP/IP sockets *****/

   if( strncmp(name,"tcp:",4) == 0 ){
      char host[128] , * hend ;
      int  port=-1 , ii , jj ;

      /** find "host" substring **/

      hend = strstr( name+4 , ":" ) ;
      if( hend == NULL || hend-name > 128 ){
         error_string = "iochan_init: bad name" ; return NULL ;
      }
      for( ii=4 ; name[ii] != ':' ; ii++ ) host[ii-4] = name[ii] ;
      host[ii-4] = '\0' ;

      /** get "port" number **/

      port = strtol( name+ii+1 , NULL , 10 ) ;
      if( port <= 0 ){
         error_string = "iochan_init: bad port" ; return NULL ;
      }

      /** initialize IOCHAN **/

      /* from malloc    12 Feb 2009 [lesstif patrol] */
      ioc = (IOCHAN *) calloc( 1, sizeof(IOCHAN) ) ;

      ioc->type     = TCP_IOCHAN ;   /* what kind is this? */
      ioc->port     = port ;         /* save the port #    */
      ioc->bufsize  = 0 ;            /* TCP has no buffer  */
      ioc->buf      = NULL ;
      ioc->sendsize = 0 ;            /* no upper limit */
      ioc->ioc2     = NULL ;         /* TCP has no second channel */

      /** attach to incoming call **/

      if( do_accept ){
         ioc->whoami = ACCEPTOR ;                         /* 24 June 1997 */
         ioc->id = tcp_listen( port ) ;                   /* set up to listen  */
         if( ioc->id < 0 ){                               /* error? must die!  */
            error_string = "iochan_init: tcp_listen fails" ;
            free(ioc) ; return NULL ;
         }
         ioc->bad = TCP_WAIT_ACCEPT ;                     /* not connected yet */
         ii = tcp_readcheck(ioc->id,1) ;                  /* see if ready      */
         if( ii > 0 ){                                    /* if socket  ready  */
            jj = tcp_accept( ioc->id , NULL,&hend ) ;     /* accept connection */
            if( jj >= 0 ){                                /* if accept worked  */
               CLOSEDOWN( ioc->id ) ;                     /* close old socket  */
               strcpy( ioc->name , hend ) ;               /* put IP into name  */
               free(hend) ; ioc->bad = 0 ; ioc->id = jj ; /* and ready to go!  */
            }
         }
         return ioc ;
      }

      /** place an outgoing call **/

      if( do_create ){
         struct hostent * hostp ;
         ioc->whoami = CREATOR ;                           /* 24 June 1997 */
         hostp = gethostbyname(host) ;                     /* lookup host on net */
         if( hostp == NULL ){                              /* fails? must die!   */
             error_string = "iochan_init: gethostbyname fails" ;
             free(ioc) ; return NULL ;
         }
         ioc->id  = tcp_connect( host , port ) ;           /* connect to host    */
         ioc->bad = (ioc->id < 0) ? TCP_WAIT_CONNECT : 0 ; /* fails? must wait   */
         strcpy( ioc->name , host ) ;                      /* save the host name */
         return ioc ;
      }
      return NULL ;  /* should never be reached */
   }

   /***** deal with shared memory segments *****/

   if( strncmp(name,"shm:",4) == 0 ){
      char key[128] , * kend , shm2[256] ;
      int  size=-1 , ii , jj , size2=-1 ;

#ifdef DONT_USE_SHM
      return NULL ;        /* 18 Dec 2002 */
#endif

      /** get keystring **/

      kend = strstr( name+4 , ":" ) ;
      if( kend == NULL || kend-name > 128 ){
         error_string = "iochan_init: bad name" ; return NULL ;
      }
      for( ii=4 ; name[ii] != ':' ; ii++ ) key[ii-4] = name[ii] ;
      key[ii-4] = '\0' ;

      /** get size **/

      size = strtol( name+ii+1 , &kend , 10 ) ;
      if( size < 0 || (size == 0 && do_create) ){
         error_string = "iochan_init: bad size" ; return NULL ;
      }
           if( *kend == 'K' || *kend == 'k' ){ size *= 1024      ; kend++ ; }
      else if( *kend == 'M' || *kend == 'm' ){ size *= 1024*1024 ; kend++ ; }

      /** 24 June 1997: get second size **/

      if( *kend == '+' ){
         size2 = strtol( kend+1 , &kend , 10 ) ;
         if( size2 < 0 || (size2 == 0 && do_create) ){
            error_string = "iochan_init: bad size2" ; return NULL ;
         }
              if( *kend == 'K' || *kend == 'k' ){ size2 *= 1024      ; kend++ ; }
         else if( *kend == 'M' || *kend == 'm' ){ size2 *= 1024*1024 ; kend++ ; }

         sprintf(shm2,"shm:%s++:%d",key,size2) ;  /* second channel spec */
      } else {
         shm2[0] = '\0' ;                         /* no second channel */
      }

      /** initialize IOCHAN **/

      /* from malloc    12 Feb 2009 [lesstif patrol] */
      ioc = (IOCHAN *) calloc( 1, sizeof(IOCHAN) ) ;

      ioc->type = SHM_IOCHAN ;     /* what type is this? */
      strcpy( ioc->name , key ) ;  /* save the key name  */
      ioc->ioc2 = NULL ;           /* maybe reset below? */

      /** open the second channel, if any **/

      if( shm2[0] != '\0' ){
         ioc->ioc2 = iochan_init( shm2 , mode ) ;
         if( ioc->ioc2 == NULL ){
            error_string = "iochan_init: can't open shm2" ;
            free(ioc) ; return NULL ;
         }
#ifdef DEBUG
         fprintf(stderr,"iochan_init: input=%s shm2=%s\n",name,shm2) ;
#endif
      }

      /** attach to existing shmem segment **/

      if( do_accept ){
         ioc->whoami = ACCEPTOR ;          /* 24 June 1997 */
         for( ii=0 ; ii < 2 ; ii++ ){      /* try to find segment */
            ioc->id = shm_accept( key ) ;  /* several times       */
            if( ioc->id >= 0 ) break ;     /* works? break out    */
            iochan_sleep(1) ;              /* wait 1 millisecond  */
         }
         if( ioc->id < 0 ) ioc->id = shm_accept( key ) ; /* 1 last try? */

         if( ioc->id < 0 ){                       /* failed to find segment? */
            ioc->bad = SHM_WAIT_CREATE ;          /* mark for waiting        */

         } else {                                                /* found it?     */
            char * bbb ;
            bbb = shm_attach( ioc->id ) ;                        /* attach it     */
            if( bbb == NULL ){                                   /* can't? quit   */
               error_string = "iochan_init: shm_attach fails" ;
               iochan_close(ioc) ; return NULL ;
            }
            ioc->bstart  = (int *) bbb ;                         /* set start,    */
            ioc->bend    = (int *) (bbb + sizeof(int)) ;         /* end markers   */
            ioc->buf     = bbb + 2*sizeof(int) ;                 /* after markers */
            ioc->bufsize = shm_size(ioc->id) - 2*sizeof(int) ;   /* get its size  */
            if( ioc->bufsize <= 0 ){                             /* can't? quit   */
               error_string = "iochan_init: bufsize < 0" ;
               iochan_close(ioc) ; return NULL ;
            }
            ioc->bad = 0 ;                                       /* mark ready    */
         }
         return ioc ;
      }

      /** make a new shmem segment **/

      if( do_create ){
         char * bbb ;
         ioc->whoami = CREATOR ;                             /* 24 June 1997*/
         size    = size + 1 ;                                /* extra byte  */
         ioc->id = shm_create( key , size+2*sizeof(int) ) ;  /* create it   */
         if( ioc->id < 0 ){                                  /* can't? quit */
            error_string = "iochan_init: shm_create fails" ;
            iochan_close(ioc->ioc2) ; free(ioc) ; return NULL ;
         }
         bbb = shm_attach( ioc->id ) ;                       /* attach it   */
         if( bbb == NULL ){                                  /* can't? quit */
            error_string = "iochan_init: shm_attach fails" ;
            iochan_close(ioc) ; free(ioc) ; return NULL ;
         }
         ioc->bstart    = (int *) bbb ;                      /* init start, */
         ioc->bend      = (int *) (bbb + sizeof(int)) ;      /* end markers */
         *(ioc->bstart) = 0 ;
         *(ioc->bend)   = size-1 ;
         ioc->buf       = bbb + 2*sizeof(int) ;              /* I/O buffer  */
         ioc->bufsize   = size ;                             /* buffer size */
         ioc->bad       = (shm_nattach(ioc->id) < 2)         /* ready if    */
                          ?  SHM_WAIT_ACCEPT                 /* both are    */
                          :  0 ;                             /* attached    */
         return ioc ;
      }
      return NULL ;  /* should never be reached */
   }

   return NULL ;  /* should never be reached */
}

/*-------------------------------------------------------------------------
  Check if the shmem segment is alive (has 2 attached processes).
  Returns 0 if not alive, 1 if life is happy.
---------------------------------------------------------------------------*/

int shm_alivecheck( int shmid )
{
   if( shmid < 0 ) return 0 ;
   return (shm_nattach(shmid) >= 2) ;
}

/*-------------------------------------------------------------------------
   Check if the given IOCHAN is ready for I/O.  If not, wait up to
   msec milliseconds to establish the connection to the other end;
   if msec < 0, will wait indefinitely.  Returns 1 if ready; 0 if not;
   -1 if an error occurs.  Possible errors are:
     + ioc was connected, and now has become disconnected
     + ioc is passed in as NULL
---------------------------------------------------------------------------*/

int iochan_goodcheck( IOCHAN * ioc , int msec )
{
   int ii , jj ;
   char * bbb ;

   /** check inputs for OK-osity **/

   error_string = NULL ;

   if( ioc == NULL ){
      error_string = "iochan_goodcheck: bad input" ; return -1 ;
   }

   /** if it was good before, then check if it is still good **/

   if( IOC_BAD(ioc) == 0 ){
      int ich = 1 ;

      if( ioc->type == TCP_IOCHAN ){
         ich = tcp_alivecheck(ioc->id) ;
      } else if( ioc->type == SHM_IOCHAN ){
         ich = shm_alivecheck(ioc->id) ;
         if( ich && ioc->ioc2 != NULL )
            ich = shm_alivecheck(ioc->ioc2->id) ;
      }

      if( ich == 0 ){
         error_string = "iochan_goodcheck: no longer alive" ; return -1 ;
      }
      else
         return 1 ;
   }

   /** wasn't good before, so check if that condition has changed **/

   /** TCP/IP waiting to accept call from another host **/

   if( ioc->bad == TCP_WAIT_ACCEPT ){
      ii = tcp_readcheck(ioc->id,msec) ;               /* see if ready      */
      if( ii > 0 ){                                    /* if socket  ready  */
         STATUS("iochan_goodcheck: try to accept tcp");
         jj = tcp_accept( ioc->id , NULL,&bbb ) ;      /* accept connection */
         if( jj >= 0 ){                                /* if accept worked  */
            STATUS("iochan_goodcheck: accept worked!") ;
            CLOSEDOWN( ioc->id ) ;                     /* close old socket  */
            strcpy( ioc->name , bbb ) ;                /* put IP into name  */
            free(bbb) ; ioc->bad = 0 ; ioc->id = jj ;  /* and ready to go!  */
         } else {
           STATUS("iochan_goodcheck: accept failed!") ;
         }
      }
   }

   /** TCP/IP waiting to connect call to another host **/

   else if( ioc->bad == TCP_WAIT_CONNECT ){
      int dms=0 , ms ;

      if( msec < 0 ) msec = 999999999 ;      /* a long time (11+ days) */
      for( ms=0 ; ms < msec ; ms += dms ){
         ioc->id  = tcp_connect( ioc->name , ioc->port ) ; /* try to connect to host */
         if( ioc->id >= 0 ) break ;                        /* worked? break out      */
         dms = NEXTDMS(dms) ; dms = MIN(dms,msec-ms) ; iochan_sleep(dms) ;
      }
      if( ioc->id < 0 )                                    /* one last try?          */
         ioc->id  = tcp_connect( ioc->name , ioc->port ) ;

      if( ioc->id >= 0 ) ioc->bad = 0 ;                    /* succeeded?             */
   }

   /** shmem segment waiting for creation (by someone else) **/

   else if( ioc->bad == SHM_WAIT_CREATE ){
      int dms=0 , ms ;

      if( msec < 0 ) msec = 999999999 ;      /* a long time (11+ days) */
      for( ms=0 ; ms < msec ; ms += dms ){
         ioc->id = shm_accept( ioc->name ) ;  /* try to attach to shmem segment */
         if( ioc->id >= 0 ) break ;           /* works? break out               */
         dms = NEXTDMS(dms) ; dms = MIN(dms,msec-ms) ; iochan_sleep(dms) ;
      }
      if( ioc->id < 0 )                       /* one last try?                  */
         ioc->id = shm_accept( ioc->name ) ;

      if( ioc->id >= 0 ){                                     /* found it?     */
         char * bbb ;
         bbb          = shm_attach( ioc->id ) ;               /* attach it     */
         ioc->bstart  = (int *) bbb ;                         /* set start,    */
         ioc->bend    = (int *) (bbb + sizeof(int)) ;         /* end markers   */
         ioc->buf     = bbb + 2*sizeof(int) ;                 /* after markers */
         ioc->bufsize = shm_size(ioc->id) - 2*sizeof(int) ;   /* get its size  */
         ioc->bad     = 0 ;                                   /* mark ready    */
      }
   }

   /** shmem segment we created waiting for someone else to attach **/

   else if( ioc->bad == SHM_WAIT_ACCEPT ){
      int dms=0 , ms ;

      if( msec < 0 ) msec = 999999999 ;      /* a long time (11+ days) */
      for( ms=0 ; ms < msec ; ms += dms ){
         if( shm_nattach(ioc->id) > 1 ){ ioc->bad = 0 ; break ; }
         dms = NEXTDMS(dms) ; dms = MIN(dms,msec-ms) ; iochan_sleep(dms) ;
      }
      if( ioc->bad && shm_nattach(ioc->id) > 1 ) ioc->bad = 0 ;
   }

   /** if there is a second channel, check it too **/

   if( ioc->ioc2 != NULL && ioc->ioc2->bad != 0 )
      iochan_goodcheck( ioc->ioc2 , msec ) ;

   return ( IOC_BAD(ioc) == 0 ) ;
}

/*-----------------------------------------------------------------------
  Close an IOCHAN.  Note that this will free what ioc points to.
  Use the IOCHAN_CLOSE macro to also set the pointer "ioc" to NULL.
-------------------------------------------------------------------------*/

void iochan_close( IOCHAN * ioc )
{
   if( ioc == NULL ) return ;

   if( ioc->ioc2 != NULL ) iochan_close(ioc->ioc2) ;

   if( ioc->type == TCP_IOCHAN ){
      if( ioc->id >= 0 ) CLOSEDOWN(ioc->id) ;
   }

   else if( ioc->type == SHM_IOCHAN ){
#ifndef DONT_USE_SHM
      if( ioc->id >= 0 ){
         shmdt( (char *) ioc->bstart ) ;       /* detach */
                                               /* then kill */
         if( !shm_RMID_delay || shm_nattach(ioc->id) < 1 )
           shmctl( ioc->id , IPC_RMID , NULL ) ;
      }
#endif
   }

   free( ioc ) ; return ;
}

void iochan_set_cutoff( IOCHAN * ioc )
{
   if( ioc == NULL ) return ;

   if( ioc->type == TCP_IOCHAN && ioc->id >= 0 ) tcp_set_cutoff( ioc->id ) ;
   return ;
}

/*---------------------------------------------------------------------------
  Check if the IOCHAN is ready to have data read out of it.
  If not, the routine will wait up to msec milliseconds for data to be
  available.  If msec < 0, this routine will wait indefinitely.
  For sockets, the return value is 1 if data is ready, 0 if not
  (for sockets, no indication of how much can be read is available).
  For shmem segments, the return value is how many bytes can be
  read (0 if none are available).
  -1 will be returned if some unrecoverable error is detected.
-----------------------------------------------------------------------------*/

int iochan_readcheck( IOCHAN * ioc , int msec )
{
   int ii ;

   /** check if the IOCHAN is good **/

   error_string = NULL ;

   ii = iochan_goodcheck(ioc,0) ;
   if( ii == -1 ) return -1 ;            /* some error */
   if( ii == 0  ){                       /* not good yet */
      ii = iochan_goodcheck(ioc,msec) ;  /* so wait for it to get good */
      if( ii != 1 ) return 0 ;           /* if still not good, exit */
   }

   /** tcp: ==> just use the Unix "select" mechanism **/

   if( ioc->type == TCP_IOCHAN ){
      ii = tcp_alivecheck( ioc->id ) ; if( !ii ) return -1 ;
      ii = tcp_readcheck( ioc->id , msec ) ;
      if( ii < 0 ) error_string = "iochan_readcheck: socket is bad" ;
      return ii ;
   }

   /** shm: ==> must loop and wait ourselves **/

   if( ioc->type == SHM_IOCHAN ){
      int nread , dms=0 , ms ;

      if( msec < 0 ) msec = 999999999 ;      /* a long time (11+ days) */

      /** Compute the number of readable bytes into nread.  This routine
          should be called by the "reading" process.  It will then
          be waiting until the "writing" process increments ioc->bend.   **/

      ioc = SHMIOC_READ(ioc) ;  /* 24 June 1997 */

      for( ms=0 ; ms < msec ; ms += dms ){
         nread = (*(ioc->bend) - *(ioc->bstart) + ioc->bufsize + 1) % (ioc->bufsize) ;
         if( nread > 0 ) return nread ;
         dms = NEXTDMS(dms) ; dms = MIN(dms,msec-ms) ; iochan_sleep(dms) ;
         ii = iochan_goodcheck(ioc,0) ; if( ii == -1 ) return -1 ;
      }
      nread = (*(ioc->bend) - *(ioc->bstart) + ioc->bufsize + 1) % (ioc->bufsize) ;
      if( nread > 0 ) return nread ;
      return 0 ;
   }

   return -1 ;  /* should never be reached */
}

/*---------------------------------------------------------------------------
   Read and discard data from the IOCHAN until it is clear.
   Returns -1 if an error occurs, otherwise returns the number
   of bytes discarded (which may be 0).
-----------------------------------------------------------------------------*/

#define QBUF 1024

int iochan_force_clear( IOCHAN * ioc )
{
   int ii , ntot = 0 ;
   char qbuf[QBUF] ;

   do{
      ii = iochan_readcheck(ioc,0) ;
      if( ii == -1 ) return -1 ;
      if( ii ==  0 ) return  ntot ;

      ii = iochan_recv( ioc , qbuf , QBUF ) ;
      if( ii == -1 ) return -1 ;
      ntot += ii ;

   } while( 1 ) ;  /** loop until readcheck says no data available **/

   return -1 ;  /* should not be reached */
}

/*---------------------------------------------------------------------------
  Check if the IOCHAN is clear -- that is, if the data sent to it
  has already been read.  This is logically impossible for a TCP
  channel, so will normally only be used for shmem segments.  It
  would be called by the writer process to see if the reader process
  has cleared the data all out.
  Will return 1 if clear, 0 if not clear, and -1 if some error occurs.
-----------------------------------------------------------------------------*/

int iochan_clearcheck( IOCHAN * ioc , int msec )
{
   int ii ;

   /** check if the IOCHAN is good **/

   error_string = NULL ;

   ii = iochan_goodcheck(ioc,0) ;
   if( ii == -1 ) return -1 ;            /* some error */
   if( ii == 0  ) return  1 ;            /* not good yet, so can be no data */

   /** tcp: ==> use the Unix "select" mechanism **/

   if( ioc->type == TCP_IOCHAN ) return ( tcp_readcheck(ioc->id,msec) == 0 ) ;

   /** shm: ==> must loop and wait ourselves **/

   if( ioc->type == SHM_IOCHAN ){
      int nread , dms=0 , ms ;

      if( msec < 0 ) msec = 999999999 ;      /* a long time (11+ days) */

      ioc = SHMIOC_WRITE(ioc) ;  /* 24 June 1997 */

      for( ms=0 ; ms < msec ; ms += dms ){
         nread = (*(ioc->bend) - *(ioc->bstart) + ioc->bufsize + 1) % (ioc->bufsize) ;
         if( nread == 0 ) return 1 ;
         dms = NEXTDMS(dms) ; dms = MIN(dms,msec-ms) ; iochan_sleep(dms) ;
         ii = iochan_goodcheck(ioc,0) ; if( ii == -1 ) return -1 ;
      }
      nread = (*(ioc->bend) - *(ioc->bstart) + ioc->bufsize + 1) % (ioc->bufsize) ;
      return (nread == 0) ;
   }

   return -1 ;  /* should never be reached */
}

/*---------------------------------------------------------------------------
  Check if the IOCHAN is ready to have data written into it.
  If not, the routine will wait up to msec milliseconds for writing to
  be allowable.  If msec < 0, this routine will wait indefinitely.
  For sockets, the return value is 1 if data can be sent, 0 if not
  (for sockets, no indication is given of how much data can be sent).
  For shmem segments, the return value is the number of bytes that
  can be sent (0 if none, positive if some).
  -1 will be returned if some unrecoverable error is detected.
-----------------------------------------------------------------------------*/

int iochan_writecheck( IOCHAN * ioc , int msec )
{
   int ii ;

   /** check if the IOCHAN is good **/

   error_string = NULL ;

   ii = iochan_goodcheck(ioc,0) ;
   if( ii == -1 ) return -1 ;            /* some error */
   if( ii == 0  ){                       /* not good yet */
      ii = iochan_goodcheck(ioc,msec) ;  /* so wait for it to get good */
      if( ii != 1 ) return ii ;          /* if still not good, exit */
   }

   /** tcp: ==> just use the Unix "select" mechanism **/

   if( ioc->type == TCP_IOCHAN ){
      ii = tcp_writecheck( ioc->id , msec ) ;
      if( ii == -1 ) error_string = "iochan_writecheck: socket not ready" ;
      return ii ;
   }

   /** shm: ==> must loop and wait ourselves **/

   if( ioc->type == SHM_IOCHAN ){
      int nread , dms=0 , ms , nwrite ;

      if( msec < 0 ) msec = 999999999 ;      /* a long time (11+ days) */

      ioc = SHMIOC_WRITE(ioc) ;  /* 24 June 1997 */

      /** This routine is called by the "writing" process.  It will
          wait until the reading process increments ioc->bstart.    **/

      for( ms=0 ; ms < msec ; ms += dms ){
         nread = (*(ioc->bend) - *(ioc->bstart) + ioc->bufsize + 1) % (ioc->bufsize) ;
         nwrite = ioc->bufsize - 1 - nread ;
         if( nwrite > 0 ) return nwrite ;
         dms = NEXTDMS(dms) ; dms = MIN(dms,msec-ms) ; iochan_sleep(dms) ;
         ii = iochan_goodcheck(ioc,0) ; if( ii == -1 ) return -1 ;
      }
      nread = (*(ioc->bend) - *(ioc->bstart) + ioc->bufsize + 1) % (ioc->bufsize) ;
      nwrite = ioc->bufsize - 1 - nread ;
      if( nwrite > 0 ) return nwrite ;
      return 0 ;
   }

   return -1 ;  /* should never be reached */
}

/*----------------------------------------------------------------------------
  Send (at most) nbytes of data from buffer down the IOCHAN.  Return value is
  the number of bytes actually sent, or is -1 if some error occurs.
  (Zero bytes might be sent under some circumstances.)

  Tcp: IOCHANs use blocking sends, so that all the data should be
       sent properly unless the connection to the other end fails for some
       reason.

  Shm: IOCHANs may not be able to send all the data, if the buffer is
       nearly full.  This function will send what it can, and return
       immediately (will not wait for space to clear up).  If no space is
       available in the buffer, nothing will be sent (0 will be returned).
------------------------------------------------------------------------------*/

int iochan_send( IOCHAN * ioc , char * buffer , int nbytes )
{
   int ii ;

   /** check for reasonable inputs **/

   error_string = NULL ;

   if( ioc    == NULL || IOC_BAD(ioc) != 0 ||
       buffer == NULL || nbytes < 0          ){

     error_string = "iochan_send: bad inputs" ; return -1 ;
   }

   if( nbytes == 0 ) return 0 ;

   ii = iochan_goodcheck(ioc,0) ;
   if( ii != 1 ){
      if( error_string == NULL )
         error_string = "iochan_send: iochan_goodcheck fails" ;
      return ii ;
   }

   ii = iochan_writecheck(ioc,1) ;
   if( ii <= 0 ){
      if( error_string == NULL )
         error_string = "iochan_send: iochan_writecheck fails" ;
      return ii ;
   }

   /** tcp: ==> just use send **/

   if( ioc->type == TCP_IOCHAN ){
      if( !nosigpipe ){ signal( SIGPIPE , SIG_IGN ) ; nosigpipe = 1 ; }

      if( ioc->sendsize <= 0 || nbytes <= ioc->sendsize ){
         int nsent = send( ioc->id , buffer , nbytes , 0 ) ;
         if( nsent == -1 ) PERROR("Can't use socket? tcp[send]") ;
         if( nsent < 0 ) error_string = "iochan_send: tcp send fails" ;
         return nsent ;
      } else {
         int nsent , ntosend , ntot = 0 ;
         do{
            while( tcp_writecheck(ioc->id,1) == 0 ) ;      /* spin */
            ntosend = MIN( ioc->sendsize , nbytes-ntot ) ;
            nsent   = send( ioc->id , buffer+ntot , ntosend , 0 ) ;
            if( nsent == -1 ) PERROR("Can't use socket? tcp[send]") ;
            if( nsent <= 0 ){
               error_string = "iochan_send: tcp send fails" ;
               return ((ntot>0) ? ntot : nsent) ;
            }
            ntot += nsent ;
         } while( ntot < nbytes ) ;
         return ntot ;
      }
   }

   /** shm: ==> write into the circular buffer, past "bend" **/

   if( ioc->type == SHM_IOCHAN ){
      int nread,nwrite , bend,bstart , ebot,etop , size ;

      ioc = SHMIOC_WRITE(ioc) ;  /* 24 June 1997 */

      bend   = *(ioc->bend) ; bstart = *(ioc->bstart) ; size = ioc->bufsize ;
      nread  = ( bend - bstart + size + 1 ) % size ;  /* amount readable  */
      nwrite = size - 1 - nread ;                     /* amount writeable */
      if( nwrite <= 0 ) return 0 ;                    /* can't write!     */

      if( nwrite > nbytes ) nwrite = nbytes ;         /* how much to write */

      ebot = bend+1 ; if( ebot >= size ) ebot = 0 ;   /* start at ebot */
      etop = ebot+nwrite-1 ;                          /* end at etop */

      if( etop < size ){                              /* 1 piece to copy */
         BCOPY( ioc->buf + ebot, buffer, nwrite ) ;   /* copy data       */
         *(ioc->bend) = etop ;                        /* change bend     */
#ifdef DEBUG
fprintf(stderr,"iochan_send: shm 1 piece:  %d to %d\n",ebot,etop) ;
#endif

      } else {                                             /* 2 pieces to copy */
         int nn = size - ebot ;                            /* size of piece 1  */
         BCOPY( ioc->buf + ebot, buffer   , nn        ) ;  /* copy piece 1     */
         BCOPY( ioc->buf       , buffer+nn, nwrite-nn ) ;  /* copy piece 2     */
         *(ioc->bend) = nwrite-nn-1 ;                      /* change bend      */
#ifdef DEBUG
fprintf(stderr,"iochan_send: shm 2 pieces: %d to %d AND %d to %d\n",
        ebot,ebot+nn-1,0,nwrite-nn-1) ;
#endif

      }
      return nwrite ;
   }

   return -1 ;  /* should not be reached */
}

/*----------------------------------------------------------------------------
   Send (exactly) nbytes of data from the buffer down the IOCHAN.  The only
   difference between this and iochan_send is that this function will not
   return until all the data is sent, even if it takes forever.
   Under these circumstances, it would be good if the reader process is
   still working.
------------------------------------------------------------------------------*/

int iochan_sendall( IOCHAN * ioc , char * buffer , int nbytes )
{
   int ii , ntot=0 , dms=0 ;

   error_string = NULL ;

   /** check for reasonable inputs **/

   if( ioc    == NULL || IOC_BAD(ioc) != 0 ||
       buffer == NULL || nbytes < 0          ){

      error_string = "iochan_sendall: bad inputs" ; return -1 ;
   }

   if( nbytes == 0 ) return 0 ;

   while(1){
      ii = iochan_send( ioc , buffer+ntot , nbytes-ntot ); /* send what's left  */
      if( ii == -1 ){                                      /* an error!?        */
         if( error_string == NULL )
            error_string = "iochan_sendall: iochan_send fails" ;
         return -1 ;
      }
      ntot += ii ;                                         /* total sent so far */
      if( ntot == nbytes ) return nbytes ;                 /* all done!?        */
      dms = NEXTDMS(dms) ; iochan_sleep(dms) ;             /* wait a while      */
   }
   return -1 ;   /* should never be reached */
}

/*----------------------------------------------------------------------------
  Read up to nbytes of data from the IOCHAN, into buffer.  Returns the
  number of bytes actually read.  For both the case of sockets and
  shmem segments, this may be less than nbytes (may even be 0).  If an
  error occurs, -1 is returned.
------------------------------------------------------------------------------*/

int iochan_recv( IOCHAN * ioc , char * buffer , int nbytes )
{
   /** check for reasonable inputs **/

   error_string = NULL ;

   if( ioc    == NULL || IOC_BAD(ioc) != 0 ||
       buffer == NULL || nbytes < 0          ){

      error_string = "iochan_recv: bad inputs" ; return -1 ;
   }

   if( nbytes == 0 ) return 0 ;
   if( iochan_goodcheck(ioc,0) != 1 ) return -1 ;

   /** tcp: just use recv **/

   if( ioc->type == TCP_IOCHAN ){
      int ii = tcp_recv( ioc->id , buffer , nbytes , 0 ) ;
      if( ii == -1 ){
         PERROR("Can't read from socket? tcp[recv]") ;
         error_string = "iochan_recv: tcp recv fails" ;
      }
      return ii ;
   }

   /** shm: read from the circular buffer, starting at bstart **/

   if( ioc->type == SHM_IOCHAN ){
      int nread, bend,bstart , size , sbot,stop ;

      ioc = SHMIOC_READ(ioc) ;  /* 24 June 1997 */

      bend  = *(ioc->bend) ; bstart = *(ioc->bstart) ; size = ioc->bufsize ;
      nread = ( bend - bstart + size + 1 ) % size ;    /* readable amount */
      if( nread <= 0 ) return 0 ;                      /* nothing!?       */
      if( nread > nbytes ) nread = nbytes ;            /* amount to read  */

      sbot = bstart ; stop = sbot + nread-1 ;          /* from sbot to stop */

      if( stop < size ){                             /* 1 piece to copy */
         BCOPY( buffer, ioc->buf + sbot, nread ) ;   /* copy the data   */
         *(ioc->bstart) = (stop+1) % size ;          /* move bstart up  */
#ifdef DEBUG
fprintf(stderr,"iochan_recv: get 1 piece:  %d to %d\n",sbot,stop) ;
#endif

      } else {                                             /* 2 pieces to copy */
         int nn = size - sbot ;                            /* size of piece 1  */
         BCOPY( buffer   , ioc->buf + sbot, nn        ) ;  /* copy piece 1     */
         BCOPY( buffer+nn, ioc->buf       , nread-nn  ) ;  /* copy piece 2     */
         *(ioc->bstart) = nread-nn ;                       /* move bstart up   */
#ifdef DEBUG
fprintf(stderr,"iochan_recv: get 2 pieces: %d to %d AND %d to %d\n",
        sbot,sbot+nn-1,0,nread-nn-1) ;
#endif

      }
      return nread ;
   }

   return -1 ;  /* should not be reached */
}

/*----------------------------------------------------------------------------
   Read as much data as possible from the iochan, looping until nothing
   is left -- 22 May 2001 -- RWCox.
------------------------------------------------------------------------------*/

int iochan_recvloop( IOCHAN * ioc , char * buffer , int nbytes )
{
   int jj , nbuf=0 ;

   error_string = NULL ;

   /** check for reasonable inputs **/

   if( ioc    == NULL || IOC_BAD(ioc) != 0 ||
       buffer == NULL || nbytes < 0          ){

      error_string = "iochan_recvloop: bad inputs" ; return -1 ;
   }

   if( iochan_goodcheck(ioc,0) != 1 ) return -1 ;

   if( nbytes == 0 ) return 0 ;

   while(1){
      jj = iochan_recv( ioc , buffer+nbuf , nbytes-nbuf ) ;
      if( jj < 1 ) break ;  /* stop if nothing more comes in */
      nbuf += jj ;
      if( nbuf >= nbytes ) break ;  /* stop if overflow */
      iochan_sleep(1) ;
   }

   return nbuf ;
}

/*----------------------------------------------------------------------------
   Receive (exactly) nbytes of data from the buffer down the IOCHAN.  The only
   difference between this and iochan_recv is that this function will not
   return until all the data is received, even if it takes forever.
   Under these circumstances, it would be good if the writer process is
   still working.
------------------------------------------------------------------------------*/

int iochan_recvall( IOCHAN * ioc , char * buffer , int nbytes )
{
   int ii , ntot=0 , dms=0 ;

   /** check for reasonable inputs **/

   error_string = NULL ;

   if( ioc    == NULL || IOC_BAD(ioc) != 0 ||
       buffer == NULL || nbytes < 0          ){

      error_string = "iochan_recvall: bad inputs" ; return -1 ;
   }

   if( nbytes == 0 ) return 0 ;

   while(1){
      ii = iochan_recv( ioc , buffer+ntot , nbytes-ntot ) ;  /* get what's left */
      if( ii == -1 ) return -1 ;                             /* an error!?      */
      ntot += ii ;                                           /* total so far    */
      if( ntot == nbytes ) return nbytes ;                   /* all done!?      */
      dms = NEXTDMS(dms) ; iochan_sleep(dms) ;               /* wait a while    */
   }
   return -1 ;   /* should never be reached */
}

/*-----------------------------------------------------------------
   Sleep a given # of milliseconds (uses the Unix select routine)
------------------------------------------------------------------*/

void iochan_sleep( int msec )
{
   struct timeval tv ;
   if( msec <= 0 ) return ;
   tv.tv_sec  = msec/1000 ;
   tv.tv_usec = (msec%1000)*1000 ;
   select( 1 , NULL,NULL,NULL , &tv ) ;
   return ;
}

/*-----------------------------------------------------------------
   Send some control command to the IOCHAN.  At present the
   only command available is to limit the size of TCP/IP sends.
------------------------------------------------------------------*/

int iochan_ctl( IOCHAN * ioc , int cmd , int arg )
{
   if( ioc == NULL ) return -1 ;

   switch( cmd ){

      case IOC_TCP_SENDSIZE:
         if( arg >= 0 ){ ioc->sendsize = arg ; return  0 ; }
         else                                  return -1 ;
      break ;

   }
   return -1 ;
}

/*-----------------------------------------------------------------
  Relay data from one IOCHAN to another in a fork()-ed process:
   name_in  = name of input IOCHAN; will be read from only
              - should be opened with "create" in the caller before
                this call, and then the child process will open with
                "accept".
   name_out = name of output IOCHAN; will be written to only
              - will be opened with "create" in the child and then
                child will wait for someone to "accept"
  Return value is the pid of the forked process; (pid_t)-1 if
  something bad happened at startup.  If something bad happens
  later, the child will _exit(1), and it will also close both
  IOCHANs it opened.  You can detect the child exit with waitpid(),
  and can detect the closing of the name_in IOCHAN using
  iochan_writecheck(), as in this fragment:

     pid_t qpid , ppid ;
     IOCHAN *ioc ;
     char * data ;
     int   ndata ;

     ioc  = iochan_init( "shm:Elvis:1M" , "create" ) ;
     if( ioc == NULL ){
        ... do something here if can't open for output
     }
     ppid = iochan_fork_relay( "shm:Elvis:1M" , "tcp:Fabian:1234" ) ;
     if( ppid == (pid_t)-1 ){
        IOCHAN_CLOSE(ioc) ;
        ... do something here if fork failed
     }

     ... later: check if child process died

     qpid = waitpid( ppid , NULL , WNOHANG ) ;
     if( qpid == ppid ){
        IOCHAN_CLOSE(ioc) ;
        ... probably do something else here to, to mark end of output
     }

     ... check if output IOCHAN is ready for data

     if( iochan_writecheck(ioc,0) >= ndata ){
        iochan_sendall( ioc , data , ndata ) ;
     } else {
        IOCHAN_CLOSE(ioc) ;
        ... probably do something else here to, to mark end of output
     }

  As in the example, the usual way to use this would have name_in
  be a shm IOCHAN, and name_out be a tcp IOCHAN; this would then
  let a main program relay socket data through a separate process,
  so if the socket freezes up, then the main program can merrily
  continue (by using iochan_writecheck to see if the shm IOCHAN
  is available for output).

  -- 23 May 2001 -- RWCox
-------------------------------------------------------------------*/

static IOCHAN *ioc_kill_1 = NULL ;
static IOCHAN *ioc_kill_2 = NULL ;

static void iochan_fork_sigfunc(int sig)  /* for when the child dies */
{
   switch( sig ){
      case SIGTERM:
        if( ioc_kill_1 != NULL ) iochan_close(ioc_kill_1) ;
        if( ioc_kill_2 != NULL ) iochan_close(ioc_kill_2) ;
        fprintf(stderr,"\n*** iochan_fork received SIGTERM signal\n");
        fflush(stderr) ;
        _exit(1) ;
      case SIGSEGV:
        if( ioc_kill_1 != NULL ) iochan_close(ioc_kill_1) ;
        if( ioc_kill_2 != NULL ) iochan_close(ioc_kill_2) ;
        fprintf(stderr,"\n*** iochan_fork received SIGSEGV signal\n");
        fflush(stderr) ;
        _exit(1) ;
   }
}

/*-----------------------------------------------------------------*/

pid_t iochan_fork_relay( char * name_in , char * name_out )
{
   pid_t ppid = (pid_t)(-1) ;
   int jj , kk , nbuf ;
#define MBUF 1048576           /* 1 Megabyte */
   char * buf , *sss ;
   IOCHAN *ioc_in, *ioc_out ;

   if( name_in == NULL || name_out == NULL ) return ppid ;

   /*-- fork into two processes --*/

   ppid = fork() ;
   if( ppid == (pid_t)(-1) ){
      perror("iochan_fork failed") ;
      return ppid ;
   }

   if( ppid != 0 ){      /* the parent process */
      pid_t qpid ;
      iochan_sleep(5) ;                         /* wait a little bit */
      qpid = waitpid( ppid , NULL , WNOHANG ) ; /* see if child died */
      if( qpid == ppid ) ppid = (pid_t)(-1) ;   /* if it did, return error */
      return ppid ;
   }

   /*--- from here on is the child process, which never returns ---*/

   ioc_in = iochan_init( name_in , "accept" ) ;  /* open input */
   if( ioc_in == NULL ) _exit(1) ;               /* failed?   */

   ioc_out = iochan_init( name_out , "create" ) ; /* open output */
   if( ioc_out == NULL ){                         /* failed?    */
      iochan_close(ioc_in) ; _exit(1) ;
   }

   /* set signal handler to deal with sudden death situations */

   ioc_kill_1 = ioc_in  ;
   ioc_kill_2 = ioc_out ;
   signal( SIGTERM , iochan_fork_sigfunc ) ;
   signal( SIGSEGV , iochan_fork_sigfunc ) ;

   fprintf(stderr,"forked process for shm->tcp started\n") ;

   do{  /* loop until both iochans are ready */

      jj = iochan_goodcheck(ioc_in ,1) ;
      kk = iochan_goodcheck(ioc_out,1) ;
      if( jj < 0 || kk < 0 ){
         iochan_close(ioc_in) ; iochan_close(ioc_out) ; _exit(1) ;
      }

   } while( jj == 0 || kk == 0 ) ;

   fprintf(stderr,"forked process fully connected\n") ;

   buf = AFMALL(char, MBUF) ; /* workspace for transfers */
   if( buf == NULL ){
      fprintf(stderr,"forked process can't malloc I/O buffer") ;
      iochan_close(ioc_in) ; iochan_close(ioc_out) ; _exit(1) ;
   }

   while(1){  /* loop, waiting for data */

      errno = 0 ;
      jj = iochan_readcheck( ioc_in , 20 ) ;          /* any input? */
      if( jj < 0 ){                                   /* bad news?  */
         if( errno ) perror( "forked readcheck" ) ;
         else        fprintf(stderr,"forked readcheck abort: jj=%d!\n",jj) ;
         sss = iochan_error_string() ;
         if( sss != NULL ) fprintf(stderr," ** %s\n",sss) ;
         break ;
      }
      if( jj == 0 ) continue ;                        /* no news    */

      nbuf = iochan_recvloop( ioc_in , buf , MBUF ) ; /* get input! */
      if( nbuf <= 0 ) continue ;                      /* too weird! */

#if 0
      fprintf(stderr,"forked process read %d bytes\n",nbuf) ;
#endif

      errno = 0 ;
      kk = iochan_writecheck( ioc_out , 1 ) ;         /* check      */
      if( kk == 0 ){
         int qq ;
         fprintf(stderr,"forked writecheck repeat:") ;
         for( qq=0 ; qq < 1000 ; qq++ ){
           if( qq%50 == 0 ) fprintf(stderr," %d",qq+1) ;
           kk = iochan_writecheck( ioc_out , 2 ) ;
           if( kk != 0 ) break ;
         }
         fprintf(stderr,"\n") ;
      }
      if( kk <= 0 ){
         if( errno ) perror( "forked writecheck" ) ;
         else        fprintf(stderr,"forked writecheck abort: kk=%d!\n",kk) ;
         sss = iochan_error_string() ;
         if( sss != NULL ) fprintf(stderr," ** %s\n",sss) ;
         break ;
      }
      kk = iochan_sendall( ioc_out , buf , nbuf ) ;   /* send data! */
      if( kk < 0 ){                                   /* bad news?  */
         if( errno ) perror( "forked sendall" ) ;
         else        fprintf(stderr,"forked sendall abort: kk=%d!\n",kk) ;
         sss = iochan_error_string() ;
         if( sss != NULL ) fprintf(stderr," ** %s\n",sss) ;
         break ;
      }

#if 0
      fprintf(stderr,"forked process wrote %d bytes\n",nbuf) ;
#endif
   }

   /* bad news ==> shut down child operations */

   fprintf(stderr,"forked process fails!\n") ;

   iochan_close(ioc_in) ; iochan_close(ioc_out) ; _exit(1) ;
}

/*-----------------------------------------------------------------
  Return time elapsed since first call to this routine
-------------------------------------------------------------------*/

#include <time.h>

double COX_clock_time(void) /* in seconds */
{
   struct timeval  new_tval ;
   struct timezone tzone ;
   static struct timeval old_tval ;
   static int first = 1 ;

   gettimeofday( &new_tval , &tzone ) ;

   if( first ){
      old_tval = new_tval ;
      first    = 0 ;
      return 0.0 ;
   }

   if( old_tval.tv_usec > new_tval.tv_usec ){
      new_tval.tv_usec += 1000000 ;
      new_tval.tv_sec -- ;
   }

   return (double)( (new_tval.tv_sec  - old_tval.tv_sec )
                   +(new_tval.tv_usec - old_tval.tv_usec)*1.0e-6 ) ;
}

/*-----------------------------------------------------------------
  Return total user-level CPU time for this process.
-------------------------------------------------------------------*/

double COX_cpu_time(void)  /* in seconds */
#ifdef CLK_TCK
{
   struct tms ttt ;

   (void) times( &ttt ) ;
   return (  (double) (ttt.tms_utime /* + ttt.tms_stime */ )
           / (double) CLK_TCK ) ;
}
#else
{ return 0.0 ; }
#endif
