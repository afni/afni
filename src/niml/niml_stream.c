#include "niml_private.h"

/*************************************************************************/
/********************* Functions for NIML I/O ****************************/
/*************************************************************************/

/*! To print a system error message. */

#undef  PERROR

#ifdef NIML_DEBUG
# define PERROR(x) perror(x)
#else
# define PERROR(x)
#endif

#include <signal.h>   /* signal handler stuff */
#include <fcntl.h>    /* file control stuff  */

/*! For tcp - indicates that SIGPIPE is ignored;
    will be set the first time tcp_send is called. */

static int nosigpipe = 0 ;

/*! For tcp - indicates that the SIGURG handler is installed;
    will be set the first time a TCP socket is created.       */

static int sigurg = 0 ;  /* 02 Jan 2004 */

/*! How to close a socket, given the descriptor ss. */

#define CLOSEDOWN(ss) ( shutdown((ss),2) , close((ss)) )

/*! This is used to set the send/receive buffer size for sockets. */

#define SOCKET_BUFSIZE  (63*1024)

/*! This macro is used so I can replace recv() with something else if I want. */

#define tcp_recv recv

/*! This macro is used so I can replace send() with something else if I want. */

#define tcp_send send

#ifndef MIN
/*! Duh. */
#  define MIN(a,b) (((a)>(b)) ? (b) : (a))
#endif

/*! Next delay in milliseconds, given current delay. */

#undef  NEXTDMS
#define NEXTDMS(dm) MIN(1.1*(dm)+1.01,66.0)

/*-------------------------------------------------------------------*/
/*! Number of entries on the list of currently open streams.

    This list is needed so we can deal with the SIGURG signal,
    which we use as a message to shut a socket down.  The signal
    call itself doesn't tell us which socket was the trigger,
    so we have to search all the open sockets for a match:
    hence, this list of open streams.
---------------------------------------------------------------------*/

static int           num_open_streams = 0 ;

/*! The actual array of open NIML streams. */

static NI_stream_type ** open_streams = NULL ;

/*! Signal that we are doing atexit() stuff. */

static volatile int doing_atexit = 0 ;  /* 05 May 2005 */

/*-------------------------------------------------------------------*/
/*! Add a stream to the open list. */

static void add_open_stream( NI_stream_type *ns )
{
   int nn = num_open_streams ;

   if( ns == NULL ) return ;  /* bad input */

   open_streams = (NI_stream_type **)realloc( (void *)open_streams ,
                                              sizeof(NI_stream_type *)*(nn+1) );

   open_streams[nn] = ns ; num_open_streams++ ; return ;
}

/*-------------------------------------------------------------------*/
/*! Remove a stream from the open list. */

static void remove_open_stream( NI_stream_type *ns )
{
   int nn = num_open_streams , ii,jj ;

   if( doing_atexit || nn <= 0 || ns == NULL ) return ;  /* bad input */

   for( ii=0 ; ii < nn ; ii++ )          /* find input */
     if( open_streams[ii] == ns ) break ;
   if( ii == nn ) return ;               /* not found!? */

   for( jj=ii+1 ; jj < nn ; jj++ )       /* move those above down */
     open_streams[jj-1] = open_streams[jj] ;

   open_streams[nn-1] = NULL ; num_open_streams-- ; return ;
}

/*------------------------------------------------------------------*/
/*! At program exit, close all open streams. */

static void atexit_open_streams(void)  /* 22 Apr 2005 */
{
   int ii ;
   if( doing_atexit ) return ;
   doing_atexit = 1 ;
   for( ii=0 ; ii < num_open_streams ; ii++ ){
     NI_sleep(2) ;
     NI_stream_close_keep( open_streams[ii] , 5 ) ;
   }
   return ;
}

/*! Variable to indicate that the atexit() call has/hasn't been made */

static int atexit_is_setup = 0 ;

/*------------------------------------------------------------------*/
/*! Signal handler for SIGURG -- for incoming OOB data on a socket.
    We just close the NI_stream that the socket is attached to.
    But first we have to find it!
--------------------------------------------------------------------*/

static void tcp_sigurg_handler( int sig )
{
   int nn = num_open_streams , ii , sd,sdtop ;
   NI_stream_type *ns ;
   fd_set efds ;
   struct timeval tv ;
   static volatile int busy=0 ;

   if( sig != SIGURG         ||
       busy                  ||
       num_open_streams <= 0 || open_streams == NULL ) return ;  /* bad */

   busy = 1 ;  /* prevent recursion! */

   /* find largest socket descriptor in list of streams,
      and make list of all open socket descriptors in streams */

   FD_ZERO(&efds) ; sdtop = -1 ;
   for( ii=0 ; ii < nn ; ii++ ){
     if( open_streams[ii]       != NULL             &&
         open_streams[ii]->bad  != MARKED_FOR_DEATH &&
         open_streams[ii]->type == NI_TCP_TYPE      &&
         open_streams[ii]->sd   >= 0                  ){

       FD_SET( open_streams[ii]->sd , &efds ) ;
       if( open_streams[ii]->sd > sdtop ) sdtop = open_streams[ii]->sd;
     }
   }
   if( sdtop < 0 ){ busy=0 ; return; }   /* no sockets found? */

   /* do a select to find which socket has an exceptional condition */

   tv.tv_sec  = 0 ;
   tv.tv_usec = 666 ;
   ii = select(sdtop+1, NULL, NULL, &efds, &tv) ;  /* check it */
   if( ii <= 0 ){ busy=0 ; return; }   /* no sockets found? */

   /* loop over found sockets and close their streams */

   for( ii=0 ; ii < nn ; ii++ ){
     if( open_streams[ii] != NULL && open_streams[ii]->type == NI_TCP_TYPE ){
       if( FD_ISSET( open_streams[ii]->sd , &efds ) ){
         CLOSEDOWN( open_streams[ii]->sd ) ;
         open_streams[ii]->bad = MARKED_FOR_DEATH ;
       }
     }
   }

   busy=0 ; return ;
}

/********************************************************************
  Routines to manipulate TCP/IP stream sockets.
  See http://www.manualy.sk/sock-faq/unix-socket-faq.html for info.
*********************************************************************/

/*-------------------------------------------------------------------*/
/*!  See if the given socket (file descriptor sd) is ready to read.

   msec is the number of milliseconds to wait:
     -  zero ==> no waiting
     -  < 0  ==> wait until something happens (not recommended)

   Return values are:
     - -1 = some error occured (socket closed at other end?)
     -  0 = socket is not ready to read
     -  1 = socket has data
---------------------------------------------------------------------*/

static int tcp_readcheck( int sd , int msec )
{
   int ii ;
   fd_set rfds ;
   struct timeval tv , *tvp ;

   if( sd < 0 ) return -1 ;                     /* bad socket id */

   FD_ZERO(&rfds) ; FD_SET(sd, &rfds) ;         /* check only sd */

   if( msec >= 0 ){                             /* set timer */
     tv.tv_sec  = msec/1000 ;
     tv.tv_usec = (msec%1000)*1000 ;
     tvp        = &tv ;
   } else {
     tvp        = NULL ;                        /* forever */
   }

   ii = select(sd+1, &rfds, NULL, NULL, tvp) ;  /* check it */
   if( ii == -1 ) PERROR( "tcp_readcheck(select)" ) ;
   return ii ;
}

/*-------------------------------------------------------------------*/
/*! See if the given socket (file descriptor sd) is ready to write.

    msec = max amount of time to wait, in milliseconds.
     -  zero ==> no waiting
     -  < 0  ==> wait until something happens (not recommended)

   Return values are
     - -1 = some error occured (socket closed at other end?)
     -  0 = socket is not ready to write
     -  1 = OK to write to socket
---------------------------------------------------------------------*/

static int tcp_writecheck( int sd , int msec )
{
   int ii ;
   fd_set wfds ;
   struct timeval tv , *tvp ;

   if( sd < 0 ) return -1 ;                     /* bad socket id */

   FD_ZERO(&wfds) ; FD_SET(sd, &wfds) ;         /* check only sd */

   if( msec >= 0 ){                             /* set timer */
     tv.tv_sec  = msec/1000 ;
     tv.tv_usec = (msec%1000)*1000 ;
     tvp        = &tv ;
   } else {
     tvp        = NULL ;                        /* forever */
   }

   ii = select(sd+1, NULL , &wfds, NULL, tvp);  /* check it */
   if( ii == -1 ) PERROR( "tcp_writecheck(select)" ) ;
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

#ifdef SO_LINGER
   /* Turn off "lingering". */

   { struct linger lg ;
     lg.l_onoff  = 1 ;
     lg.l_linger = 0 ;
     setsockopt(sd, SOL_SOCKET, SO_LINGER, (void *)&lg, sizeof(struct linger)) ;
   }
#endif

#ifdef SO_REUSEADDR
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

static int tcp_alivecheck( int sd )
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

static int tcp_connect( char *host , int port )
{
   int sd , l , q,qq ;
   struct sockaddr_in sin ;
   struct hostent    *hostp ;

   if( host == NULL || port < 1 ) return -1 ;  /* bad inputs */

#ifdef NIML_DEBUG
   NI_dpr("Enter tcp_connect: host=%s port=%d\n",host,port) ;
#endif

   /** open a socket **/

   sd = socket( AF_INET , SOCK_STREAM , 0 ) ;
   if( sd == -1 ){ PERROR("tcp_connect(socket)"); return -1; }

   /** set socket options (no delays, large buffers) **/

#if 0
   { char *eee=getenv( "NIML_TCP_NAGLE" ) ;
     if( eee == NULL || toupper(*eee) != 'Y' ){
       /** disable the Nagle algorithm **/
       l = 1;
       setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (void *)&l, sizeof(int)) ;
     }
   }
#endif

   /* but large I/O buffers are good */

#ifdef SOCKET_BUFSIZE
   q = 0 ; qq = sizeof(int) ;                                 /* 03 Dec 2002:    */
   getsockopt(sd, SOL_SOCKET, SO_SNDBUF, (void *)&q, &qq ) ;  /* only modify      */
   if( q < SOCKET_BUFSIZE ){                                  /* if current buffer */
     l = SOCKET_BUFSIZE ;                                     /* is too small     */
     setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (void *)&l, sizeof(int)) ;
   }
   q = 0 ; qq = sizeof(int) ;
   getsockopt(sd, SOL_SOCKET, SO_RCVBUF, (void *)&q, &qq ) ;
   if( q < SOCKET_BUFSIZE ){
     l = SOCKET_BUFSIZE ;
     setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (void *)&l, sizeof(int)) ;
   }
#endif

   /** set port on remote computer **/

   memset( &sin , 0 , sizeof(sin) ) ;
   sin.sin_family = AF_INET ;
   sin.sin_port   = htons(port) ;

   /** set remote computer IP address from its name **/

   hostp = gethostbyname(host) ;
   if( hostp == NULL ){
      PERROR("tcp_connect(gethostbyname)");
#ifdef NIML_DEBUG
   NI_dpr("      tcp_connect: can't gethostbyname(); errno=%d\n",errno);
#endif
      CLOSEDOWN(sd); return -1;
   }
   sin.sin_addr.s_addr = ((struct in_addr *)(hostp->h_addr))->s_addr ;

   errno = 0 ;
   if( connect(sd,(struct sockaddr *)&sin,sizeof(sin)) != 0 ){
      if( errno != ECONNREFUSED ) PERROR("tcp_connect(connect)") ;
#ifdef NIML_DEBUG
   NI_dpr("      tcp_connect: can't connect(); errno=%d\n",errno);
#endif
      CLOSEDOWN(sd); return -1;
   }

#ifdef NIML_DEBUG
   NI_dpr("      tcp_connect: connected!\n");
#endif

   tcp_set_cutoff( sd ) ;
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
   int sd , l , q,qq ;
   struct sockaddr_in sin ;
   char serr[128]={""};
   
   if( port < 1 ) return -1 ; /* bad input */

   /** open a socket **/

   sd = socket( AF_INET , SOCK_STREAM , 0 ) ;
   if( sd == -1 ){ 
      sprintf(serr,"tcp_listen(socket): (Name %s, Port %d)", 
               get_port_numbered(port), port);
      PERROR(serr); return -1; 
   }

   /** set socket options (no delays, large buffers) **/

#if 0
   { char *eee=getenv( "NIML_TCP_NAGLE" ) ;
     if( eee == NULL || toupper(*eee) != 'Y' ){
       /** disable the Nagle algorithm **/
       l = 1;
       setsockopt(sd, IPPROTO_TCP, TCP_NODELAY, (void *)&l, sizeof(int)) ;
     }
   }
#endif

   /* set socket to have large I/O buffers */

#ifdef SOCKET_BUFSIZE
   q = 0 ; qq = sizeof(int) ;
   getsockopt(sd, SOL_SOCKET, SO_SNDBUF, (void *)&q, &qq ) ;
   if( q < SOCKET_BUFSIZE ){
     l = SOCKET_BUFSIZE ;
     setsockopt(sd, SOL_SOCKET, SO_SNDBUF, (void *)&l, sizeof(int)) ;
   }
   q = 0 ; qq = sizeof(int) ;
   getsockopt(sd, SOL_SOCKET, SO_RCVBUF, (void *)&q, &qq ) ;
   if( q < SOCKET_BUFSIZE ){
     l = SOCKET_BUFSIZE ;
     setsockopt(sd, SOL_SOCKET, SO_RCVBUF, (void *)&l, sizeof(int)) ;
   }
#endif

   /** set port on remote computer **/

   memset( &sin , 0 , sizeof(sin) ) ;
   sin.sin_family      = AF_INET ;
   sin.sin_port        = htons(port) ;
   sin.sin_addr.s_addr = INADDR_ANY ;  /* reader reads from anybody */

   if( bind(sd , (struct sockaddr *)&sin , sizeof(sin)) == -1 ){
     sprintf(serr,"tcp_listen(bind) (Name %s, Port %d, sd %d)", 
               get_port_numbered(port), port, sd);
     PERROR(serr); CLOSEDOWN(sd); return -1;
   }

   if( listen(sd,1) == -1 ){
     sprintf(serr,"tcp_listen(listen) (Name %s, Port %d)", 
               get_port_numbered(port), port);
     PERROR(serr); CLOSEDOWN(sd); return -1;
   }

   tcp_set_cutoff( sd ) ;
   return sd ;
}

/*--------------------------------------------------------------------------*/
/*! Accept incoming connection on a socket.

   Return value is the attached socket (which is not the original socket!).
   If -1 is returned, some error occured.  If the accept works, then the
   original socket is still open and listening for further attachments.
   Under many circumstances, you will want to close the original socket
   immediately.  This can be done with CLOSEDOWN(sd), where sd is the
   input socket.

   If hostname is not NULL, then the char * it points to will be filled
   with a pointer to the official name of the host that connected.

   If hostaddr is not NULL, then the char * it points to will be filled
   with a pointer to the Internet address (in 'dot' form) of the host that
   connected.

   Both the char * pointers returned are from NI_malloc(), and should be
   NI_free()-d when no longer needed.  If they aren't needed at all, just
   pass in NULL for these arguments.

   Note that this routine will block until somebody connects.  You can
   use tcp_readcheck(sd,0) to see if anyone is waiting to connect before
   calling this routine.

   However, if someone connects and the IP address isn't on the
   trusted list, then the connection will be closed immediately.
---------------------------------------------------------------------------*/

static int tcp_accept( int sd , char **hostname , char **hostaddr )
{
   struct sockaddr_in pin ;
   int addrlen , sd_new ;
   struct hostent *hostp ;
   char *str ;

   /** accept the connection **/

   addrlen = sizeof(pin) ;
   sd_new = accept( sd , (struct sockaddr *)&pin , &addrlen ) ;
   if( sd_new == -1 ){ PERROR("tcp_accept"); return -1; }

   /** get dotted form address of connector **/

   str = inet_ntoa( pin.sin_addr ) ;

   if( !NI_trust_host(str) ){
     fprintf(stderr,"\n** ILLEGAL attempt to connect from host %s\n",str) ;
     CLOSEDOWN( sd_new ) ;
     return -1 ;
   }

   if( hostaddr != NULL ) *hostaddr = NI_strdup(str) ;

   /** get name of connector **/

   if( hostname != NULL ){
     hostp = gethostbyaddr( (char *) (&pin.sin_addr) ,
                            sizeof(struct in_addr) , AF_INET ) ;

     if( hostp != NULL ) *hostname = NI_strdup(hostp->h_name) ;
     else                *hostname = NI_strdup("UNKNOWN") ;  /* bad lookup */
   }

   tcp_set_cutoff( sd_new ) ;  /* let it die quickly, we hope */
   return sd_new ;
}

/*******************************************************************/
/*** Functions to setup a "trusted host" list for TCP/IP accept. ***/
/*******************************************************************/

static int     host_num  = 0 ;    /*!< Number of trusted hosts. */
static char ** host_list = NULL ; /*!< IP addresses in dotted form. */

static char *init_hosts[] = {  /*!< Initial list of OK computers */
    "127.0.0.1"    ,           /* localhost is always OK */
    "192.168."     ,           /* private class B networks */
    "128.231.21"               /* SSCC/NIMH/NIH/DHHS/USA */
} ;
#define INIT_NHO (sizeof(init_hosts)/sizeof(char *))
#define HSIZE    32

/*----------------------------------------------------------------*/
/*! Return the Internet address (in 'dot' format, as a string)
   given the name of the host.  If NULL is returned, some
   error occurrrrred.  The string is NI_malloc()-ed, and should
   be NI_free()-ed when no longer needed.
------------------------------------------------------------------*/

char * NI_hostname_to_inet( char *host )
{
   struct hostent *hostp ;
   char * iname = NULL , *str ;
   int ll ;

   if( host == NULL || host[0] == '\0' ) return NULL ;

   hostp = gethostbyname(host) ; if( hostp == NULL ) return NULL ;

   str = inet_ntoa(*((struct in_addr *)(hostp->h_addr))) ;
   if( str == NULL || str[0] == '\0' ) return NULL ;

   iname = NI_strdup(str) ; return iname ;
}

/*----------------------------------------------------------------*/
/*! Check if hostname is in dotted form.
------------------------------------------------------------------*/

static int hostname_dotted( char *hnam )
{
   int ii, nh ;
   if( hnam == NULL ) return 0 ;
   nh = strlen(hnam) ;
   for( ii=0 ; ii < nh ; ii++ )
     if( !isdigit(hnam[ii]) && hnam[ii] != '.' ) return 0 ;
   return 1 ;
}

/*----------------------------------------------------------------*/
/*! Add a host to the trusted list (internal version).
------------------------------------------------------------------*/

static void add_trusted_host( char *hnam )
{
   char *hh=NULL ;
   int ii ;

   if( hnam == NULL || hnam[0] == '\0' ) return ;

   if( !hostname_dotted(hnam) ){         /* not a dotted number */
     hh = NI_hostname_to_inet( hnam ) ;  /* so do a lookup on it */
     if( hh == NULL ) return ;           /* failed? */

   } else if( strlen(hnam) > HSIZE-1 ){   /* something bad? */
     return ;
   } else {
     hh = hnam ;                     /* store dotted number */
   }

   host_list = NI_realloc(host_list, char*,sizeof(char *)*(host_num+1)) ;
   host_list[host_num] = NI_malloc(char, HSIZE) ;
   strcpy( host_list[host_num] , hh ) ; host_num++ ;

   if( hh != hnam ) NI_free(hh) ;
}

/*--------------------------------------------------------------------------*/
/*! Initialize trusted list from the internal table and the environment.
----------------------------------------------------------------------------*/

static void init_trusted_list(void)
{
   int ii ;
   char ename[HSIZE] , *str ;

   if( host_num == 0 ){      /** only execute this once **/
     host_num = INIT_NHO ;
     host_list = NI_malloc(char*, sizeof(char *) * INIT_NHO ) ;
     for( ii=0 ; ii < INIT_NHO ; ii++ ){
       host_list[ii] = NI_malloc(char, HSIZE) ;
       strcpy( host_list[ii] , init_hosts[ii] ) ;
     }

     for( ii=0 ; ii <= 99 ; ii++ ){
       sprintf(ename,"NIML_TRUSTHOST_%02d",ii) ; str = getenv(ename) ;
       if( str == NULL && ii <= 9 ){
         sprintf(ename,"NIML_TRUSTHOST_%1d",ii) ; str = getenv(ename) ;
       }
       if( str == NULL && ii <= 9 ){
         sprintf(ename,"NIML_TRUSTHOST_O%1d",ii) ; str = getenv(ename) ;
       }
       if( str != NULL ) add_trusted_host(str) ;
     }

     for( ii=0 ; ii <= 99 ; ii++ ){
       sprintf(ename,"AFNI_TRUSTHOST_%02d",ii) ; str = getenv(ename) ;
       if( str == NULL && ii <= 9 ){
         sprintf(ename,"AFNI_TRUSTHOST_%1d",ii) ; str = getenv(ename) ;
       }
       if( str == NULL && ii <= 9 ){
         sprintf(ename,"AFNI_TRUSTHOST_O%1d",ii) ; str = getenv(ename) ;
       }
       if( str != NULL ) add_trusted_host(str) ;
     }
   }
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Externally callable routine to add a host to the trusted list.
    If call with NULL, will just initialize the default trusted
    host list.
----------------------------------------------------------------------------*/

void NI_add_trusted_host( char *hostname )
{
   if( host_num == 0 ) init_trusted_list() ;
   if( hostname == NULL || hostname[0] == '\0' ) return ;
   add_trusted_host(hostname) ;
}

/*---------------------------------------------------------------------------*/
/*! Return 1 if we like hostid, 0 if we don't.
-----------------------------------------------------------------------------*/

int NI_trust_host( char *hostid )
{
   int ii ;
   char *hh = hostid ;

   /* if the trusted list is empty,
      see if we want to be completely trusting;
      if not, then initialize the trusted list and then check */

   if( host_num == 0 ){
      char *eee = getenv("NIML_COMPLETE_TRUST") ;
      if( eee != NULL && toupper(*eee) == 'Y' ) return 1 ; /* complete trust */
      init_trusted_list() ;
   }

   if( hostid == NULL || hostid[0] == '\0' ) return 0 ;

   if( !hostname_dotted(hostid) ){
      hh = NI_hostname_to_inet(hostid) ;  /* will be NI_malloc()-ed */
      if( hh == NULL ) return 0 ;
   }

   /* to be trusted, hostid must start with same
      string as something in the trusted host_list array */

   for( ii=0 ; ii < host_num ; ii++ ){
      if( strstr(hh,host_list[ii]) == hh ){
        if( hh != hostid ) NI_free(hh) ;
        return 1 ;
      }
   }

   if( hh != hostid ) NI_free(hh) ;
   return 0 ;
}

#ifndef DONT_USE_SHM
/****************************************************************
  Routines to manipulate IPC shared memory segments for I/O
  [adapted from thd_iochan.c, 31 May 2002 -- RWCox]
*****************************************************************/

/*---------------------------------------------------------------*/
/*!  Convert a string to a key, for IPC operations.
   Augment sum by port offset value (-np option)
-----------------------------------------------------------------*/

static key_t SHM_string_to_key( char *key_string )
{
   int ii , sum = get_user_np() ;
   key_t kk ;

   sum += 987654321 ;
   if( key_string == NULL ) return (key_t) sum ;

   for( ii=0 ; key_string[ii] != '\0' ; ii++ )
      sum += ((int)key_string[ii]) << ((ii%3)*8) ;

   kk = (key_t) sum ;
#ifdef IPC_PRIVATE
   if( kk == IPC_PRIVATE || kk <= 0 ) kk = 666 ;
#endif
   return kk ;
}

/*---------------------------------------------------------------*/
/*! Get a pre-existing shmem segment.
   Returns the shmid >= 0 if successful; returns -1 if failure.
-----------------------------------------------------------------*/

static int SHM_accept( char *key_string )
{
   key_t key ;
   int   shmid ;

   key   = SHM_string_to_key( key_string ) ;
   shmid = shmget( key , 0 , 0777 ) ;
   return shmid ;
}

/*---------------------------------------------------------------*/
/*!  Connect to, or create if needed, a shmem segment.
   Returns the shmid >= 0 if successful; returns -1 if failure.
-----------------------------------------------------------------*/

static int SHM_create( char *key_string , int size )
{
   key_t key ;
   int   shmid ;

   key   = SHM_string_to_key( key_string ) ;
   shmid = shmget( key , size , 0777 | IPC_CREAT ) ;
   if( shmid < 0 ) PERROR("SHM_create") ;
   return shmid ;
}

/*---------------------------------------------------------------*/
/*! Actually attach to the shmem segment.
   Returns the pointer to the segment start.
   NULL is returned if an error occurs.
-----------------------------------------------------------------*/

static char * SHM_attach( int shmid )
{
   char *adr ;
   adr = (char *) shmat( shmid , NULL , 0 ) ;
   if( adr == (char *) -1 ){ adr = NULL ; PERROR("SHM_attach") ; }
   return adr ;
}

/*---------------------------------------------------------------*/
/*!  Find the size of a shmem segment.
   Returns -1 if an error occurs.
-----------------------------------------------------------------*/

static int SHM_size( int shmid )
{
   int ii ;
   struct shmid_ds buf ;

   if( shmid < 0 ) return -1 ;
   ii = shmctl( shmid , IPC_STAT , &buf ) ;
   if( ii < 0 ){ PERROR("SHM_size") ;  return -1 ; }
   return buf.shm_segsz ;
}

/*---------------------------------------------------------------*/
/*! Find the number of attaches to a shmem segment.
   Returns -1 if an error occurs.
-----------------------------------------------------------------*/

static int SHM_nattach( int shmid )
{
   int ii ;
   static struct shmid_ds buf ;
   char *eee = getenv( "NIML_DNAME" ) ;
   static int nid=0;
   
   if( shmid < 0 ) return -1 ;
   ii = shmctl( shmid , IPC_STAT , &buf ) ;
   if( ii < 0 ){
     if( eee != NULL ) 
      fprintf(stderr,
  "SHM_nattach (%s): (shmid=%d, buf.shm_nattach %d, errno %d), trying again!\n"
  "                     EACCES %d, EFAULT %d, EINVAL %d, EPERM %d\n",
        eee, shmid, (int)buf.shm_nattch, errno, EACCES, EFAULT, EINVAL, EPERM) ;
     NI_sleep(9) ;
     ii = shmctl( shmid , IPC_STAT , &buf ) ;
   }
   if( ii < 0 ){
     char *ppp ;
     if( eee != NULL ){
       ppp = (char *)calloc(1,strlen(eee)+32) ;
       strcpy(ppp,"SHM_nattach (") ;
       strcat(ppp,eee) ; strcat(ppp,")") ;
     } else {
       ppp = strdup("SHM_nattach") ;
     }
     PERROR(ppp);
     fprintf(stderr,"%s: called shmctl(%x,%x,%p), got %d\n"
                    "   (shmid=%d, buf.shm_nattach %d, errno %d)\n",
             ppp,(unsigned int)shmid, (unsigned int)IPC_STAT, (void *)&buf,
             ii,
             shmid, (int)buf.shm_nattch, errno ) ;
     nid = 0;
     free((void *)ppp); return -1;
   } else if( eee != NULL ){
     if (!nid) 
      fprintf(stderr,"SHM_nattach (%s): called shmctl(%x,%x,%p), got %d\n"
                     "  Similar messages muted until SHM_nattach fails again.\n",
             eee,
             (unsigned int)shmid, (unsigned int)IPC_STAT, (void *)&buf,
             (int)buf.shm_nattch ) ; 
      ++nid;
   }
   return (int)buf.shm_nattch ;
}

/*---------------------------------------------------------------*/
/*! Fill a SHMioc struct that has just been attached as an "r".
   - ioc->id should be non-negative at this point.
   - return value is 1 if things are good, -1 if not.
-----------------------------------------------------------------*/

static int SHM_fill_accept( SHMioc *ioc )
{
   char *bbb ;
   int jj ;

   if( ioc == NULL || ioc->id < 0 ) return -1 ;      /* bad inputs?   */

   NI_sleep(2) ;                                     /* wait a bit    */
   bbb = SHM_attach( ioc->id ) ;                     /* attach it     */
   if( bbb == NULL ) return -1 ;                     /* can't? quit   */

   if( SHM_nattach(ioc->id) != 2 ){                  /* 2 processes?  */
      NI_sleep(10) ;                                 /* wait a bit,   */
      if( SHM_nattach(ioc->id) != 2 ){               /* and try again */
        shmdt( bbb ) ;                               /* this is bad!  */
        shmctl( ioc->id , IPC_RMID , NULL ) ;
        ioc->bad = SHM_IS_DEAD ; return -1 ;
      }
   }

   jj = SHM_size(ioc->id) ;                          /* shmbuf size   */
   if( jj <= SHM_HSIZE ){                            /* too small?    */
      shmdt( bbb ) ;                                 /* this is bad!  */
      shmctl( ioc->id , IPC_RMID , NULL ) ;
      ioc->bad = SHM_IS_DEAD ; return -1 ;
   }

   ioc->shmbuf   = bbb ;                             /* buffer */
   ioc->shmhead  = (int *) bbb ;                     /* buffer as int */

   ioc->bufsize1 = ioc->shmhead[SHM_SIZE1] ;         /* size of buf 1 */
   ioc->bstart1  = ioc->shmhead + SHM_BSTART1 ;      /* start marker 1*/
   ioc->bend1    = ioc->shmhead + SHM_BEND1 ;        /* end marker 1  */
   ioc->buf1     = ioc->shmbuf  + SHM_HSIZE ;        /* buffer 1      */

   ioc->bufsize2 = ioc->shmhead[SHM_SIZE2] ;         /* size of buf 2 */
   ioc->bstart2  = ioc->shmhead + SHM_BSTART2 ;      /* start marker 2*/
   ioc->bend2    = ioc->shmhead + SHM_BEND2 ;        /* end marker 2  */
   ioc->buf2     = ioc->buf1    + ioc->bufsize1 ;    /* buffer 2      */

   if( jj < SHM_HSIZE+ioc->bufsize1+ioc->bufsize2 ){ /* too small?    */
      shmdt( bbb ) ;                                 /* this is bad!  */
      shmctl( ioc->id , IPC_RMID , NULL ) ;
      ioc->bad = SHM_IS_DEAD ; return -1 ;
   }

   ioc->bad = 0 ; return 1 ;                         /** DONE **/
}

/*---------------------------------------------------------------*/
/*! Create a SHMioc struct for use as a 2-way I/O channel, and
    return a pointer to it.  NULL is returned if an error occurs.

  name = "shm:name:size1+size2" to connect a shared memory
             segment with buffers of length size1 and size2 bytes.
             The creator process will write to the size1 buffer
             and read from the size2 buffer.  The acceptor
             process will reverse this.
         - The size strings can end in 'K' to multiply by 1024,
            or end in 'M' to multiply by 1024*1024.
         - If neither size is given, a default value is used.
         - If only size1 is given, size2=size1.

  mode = "w" to open a new shared memory channel
       = "r" to log into a channel created by someone else

 The input "name" is limited to a maximum of 127 bytes.
-----------------------------------------------------------------*/

static SHMioc * SHM_init( char *name , char *mode )
{
   SHMioc *ioc ;
   int do_create , do_accept ;
   char key[128] , *kend ;
   int  size1=SHM_DEFAULT_SIZE , ii , jj , size2=SHM_DEFAULT_SIZE ;

   /** check if inputs are reasonable **/

   if( name                   == NULL ||
       strlen(name)           >  127  ||
       strncmp(name,"shm:",4) != 0    ||
       mode                   == NULL   ) return NULL ;

   do_create = (*mode == 'w') ;  /* writer */
   do_accept = (*mode == 'r') ;  /* reader */

   if( !do_create && !do_accept ) return NULL ;

   /** get keystring (after "shm:") **/

   for( ii=4 ; name[ii] != ':' && name[ii] != '\0' ; ii++ )
     key[ii-4] = name[ii] ;
   key[ii-4] = '\0' ;

   /** get size1 (after "shm:name:"), if we stopped at a ':' **/

   if( do_create && name[ii] == ':' && name[ii+1] != '\0' ){

     size1 = strtol( name+ii+1 , &kend , 10 ) ;
     if( size1 <= 0 ) size1 = SHM_DEFAULT_SIZE ;
     else {
            if( *kend == 'K' || *kend == 'k' ){ size1 *= 1024     ; kend++; }
       else if( *kend == 'M' || *kend == 'm' ){ size1 *= 1024*1024; kend++; }
     }
     size2 = size1 ;  /* 23 Aug 2002 */

     /** get size2, if we stopped at a + **/

     if( *kend == '+' ){
       size2 = strtol( kend+1 , &kend , 10 ) ;
       if( size2 <= 0 ) size2 = SHM_DEFAULT_SIZE ;
       else {
              if( *kend == 'K' || *kend == 'k' ){ size2 *= 1024     ; kend++; }
         else if( *kend == 'M' || *kend == 'm' ){ size2 *= 1024*1024; kend++; }
       }
     }
   }

   /** initialize SHMioc **/

   ioc = NI_malloc(SHMioc, sizeof(SHMioc) ) ;

   strcpy( ioc->name , key ) ;  /* save the key name  */

   /** attach to existing shmem segment **/

   if( do_accept ){
      ioc->whoami = SHM_ACCEPTOR ;
      for( ii=0 ; ii < 4 ; ii++ ){      /* try to find segment */
         ioc->id = SHM_accept( key ) ;  /* several times       */
         if( ioc->id >= 0 ) break ;     /* works? break out    */
         NI_sleep(ii+1) ;               /* wait 1 millisecond  */
      }
      if( ioc->id < 0 )
        ioc->id = SHM_accept( key ) ;   /* 1 last try? */

      if( ioc->id < 0 ){                /* failed to find segment? */
         ioc->bad = SHM_WAIT_CREATE ;   /* mark for waiting        */
         return ioc ;                   /* and we are DONE for now */
         ioc->goodcheck_time = -99 ; /* 23 Nov 2004 */

      } else {                          /* found it?   */

         jj = SHM_fill_accept( ioc ) ;  /* fill struct */

         if( jj < 0 ){                  /* this is bad */
           NI_free(ioc); return NULL;
         }

         return ioc ;                   /** DONE **/
         ioc->goodcheck_time = -99 ; /* 23 Nov 2004 */
      }
   }

   /** create a new shmem segment **/

   if( do_create ){
      char *bbb ;

      ioc->whoami = SHM_CREATOR ;
      ioc->id = SHM_create( key, size1+size2+SHM_HSIZE+4 ) ; /* create it */
      if( ioc->id < 0 ){                                     /* can't? quit */
         NI_free(ioc); return NULL;
      }
      bbb = SHM_attach( ioc->id ) ;                        /* attach it   */
      if( bbb == NULL ){                                   /* can't? quit */
         NI_free(ioc); return NULL;
      }

      ioc->shmbuf   = bbb ;                                /* buffer */
      ioc->shmhead  = (int *) bbb ;                        /* buffer as int */

      ioc->bufsize1 = ioc->shmhead[SHM_SIZE1] = size1 ;    /* size of buf 1 */
      ioc->bstart1  = ioc->shmhead + SHM_BSTART1 ;         /* start marker 1*/
      ioc->bend1    = ioc->shmhead + SHM_BEND1 ;           /* end marker 1  */
      ioc->buf1     = ioc->shmbuf  + SHM_HSIZE ;           /* buffer 1      */

      ioc->bufsize2 = ioc->shmhead[SHM_SIZE2] = size2 ;    /* size of buf 2 */
      ioc->bstart2  = ioc->shmhead + SHM_BSTART2 ;         /* start marker 2*/
      ioc->bend2    = ioc->shmhead + SHM_BEND2 ;           /* end marker 2  */
      ioc->buf2     = ioc->buf1    + size1 ;               /* buffer 2      */

      *(ioc->bstart1) = 0 ;                                /* init markers 1*/
      *(ioc->bend1)   = size1-1 ;
      *(ioc->bstart2) = 0 ;                                /* init markers 2*/
      *(ioc->bend2)   = size2-1 ;

      NI_sleep(3) ;
      jj = SHM_nattach(ioc->id) ;                          /* # processes */

      if( jj < 2 ){
        NI_sleep(3) ; jj = SHM_nattach(ioc->id) ;
      }

      if( jj > 2 ){                                        /* should not  */
        shmdt( bbb ) ;                                     /* happen ever */
        shmctl( ioc->id , IPC_RMID , NULL ) ;
        NI_free(ioc); return NULL;
      }

      ioc->bad  = (jj < 2)          /* ready if both   */
                 ? SHM_WAIT_ACCEPT  /* processes are   */
                 : 0 ;              /* attached to shm */
      ioc->goodcheck_time = -99 ; /* 23 Nov 2004 */
      return ioc ;
   }

   return NULL ;  /* should never be reached */
}

/*-------------------------------------------------------------------------*/
/*! Check if the shmem segment is alive (has 2 attached processes).
  Returns 0 if not alive, 1 if life is happy.
---------------------------------------------------------------------------*/

static int SHM_alivecheck( int shmid )
{
   if( shmid < 0 ) return 0 ;
   return (SHM_nattach(shmid) == 2) ;
}

/*------------------------------------------*/
#ifndef NEXTDMS
#define NEXTDMS(dm) MIN(1.1*(dm)+1.01,99.0)  /* expanding wait interval */
#endif
/*------------------------------------------*/

/*-------------------------------------------------------------------------*/
/*! Check if the given SHMioc is ready for I/O.  If not, wait up to
   msec milliseconds to establish the connection to the other end;
   if msec < 0, will wait indefinitely.  Returns 1 if ready; 0 if not;
   -1 if an error occurs.  Possible errors are:
     + SHMioc was connected, and now has become disconnected
     + SHMioc is passed in as NULL
---------------------------------------------------------------------------*/

static int SHM_goodcheck( SHMioc *ioc , int msec )
{
   int ii , jj , ct ;
   char *bbb ;

   /** check inputs for OK-osity **/

   if( ioc == NULL || ioc->bad == SHM_IS_DEAD ) return -1 ;

   /** if it was good before, then check if it is still good **/

   if( ioc->bad == 0 ){
     ct = NI_clock_time() ;
     if( ct - ioc->goodcheck_time > 2 ){    /* 23 Nov 2004 */
       ii = SHM_alivecheck(ioc->id) ;
       ioc->goodcheck_time = ct ;
     } else {
       ii = 1 ;
     }
     if( ii <= 0 ){                            /* has died */
#ifdef NIML_DEBUG
        NI_dpr("++ Shared memory connection %s has gone bad!\n",
               ioc->name ) ;
#endif
        shmdt( ioc->shmbuf ) ; ioc->bad = SHM_IS_DEAD ;
        shmctl( ioc->id , IPC_RMID , NULL ) ; return -1 ;
     }
     return 1 ;
   }

   /** wasn't good before, so check if that condition has changed **/

   /** shm "r" process waiting for creation by the "w" process **/

   if( ioc->bad == SHM_WAIT_CREATE ){
      int dms=0 , ms ;

      if( msec < 0 ) msec = 999999999 ;       /* a long time (11+ days) */
      for( ms=0 ; ms < msec ; ms += dms ){
        ioc->id = SHM_accept( ioc->name ) ;  /* try to attach to shmem segment */
        if( ioc->id >= 0 ) break ;           /* works? break out               */
        dms = NEXTDMS(dms) ; dms = MIN(dms,msec-ms) ; NI_sleep(dms) ;
      }
      if( ioc->id < 0 )                /* one last try? */
        ioc->id = SHM_accept( ioc->name ) ;

      if( ioc->id >= 0 ){              /* found it?     */
        jj = SHM_fill_accept( ioc ) ;  /* fill struct   */
        if( jj < 0 ) return -1 ;       /* this is bad   */
        ioc->bad = 0 ;                 /* mark as ready */
        return 1 ;
      }
      return 0 ;
   }

   /** shmem "w" process waiting for "r" process to attach */

   else if( ioc->bad == SHM_WAIT_ACCEPT ){
     int dms=0 , ms ;

     if( msec < 0 ) msec = 999999999 ;      /* a long time (11+ days) */
     for( ms=0 ; ms < msec ; ms += dms ){
       if( SHM_nattach(ioc->id) > 1 ){ ioc->bad = 0 ; return 1 ; }
       dms = NEXTDMS(dms) ; dms = MIN(dms,msec-ms) ; NI_sleep(dms) ;
     }
     if( SHM_nattach(ioc->id) > 1 ){ ioc->bad = 0 ; return 1 ; }
     return 0 ;
   }

   return 0 ;  /* should never be reached */
}

/*-----------------------------------------------------------------------*/
/*! Close a SHMioc.  Note that this will free what ioc points to.
-------------------------------------------------------------------------*/

static void SHM_close( SHMioc *ioc )
{
   if( ioc == NULL ) return ;

   if( ioc->id >= 0 && ioc->bad != SHM_IS_DEAD ){
      shmdt( ioc->shmbuf ) ;                 /* detach */
      shmctl( ioc->id , IPC_RMID , NULL ) ;  /* delete */
      ioc->bad = SHM_IS_DEAD ;               /* leave for dead */
   }

   NI_free(ioc) ; return ;
}

/*---------------------------------------------------------------------------*/
/*! Check if the SHMioc is ready to have data read out of it.
  If not, the routine will wait up to msec milliseconds for data to be
  available.  If msec < 0, this routine will wait indefinitely.
  For shmem segments, the return value is how many bytes can be
  read (0 if none are available).
  -1 will be returned if some unrecoverable error is detected.
-----------------------------------------------------------------------------*/

static int SHM_readcheck( SHMioc *ioc , int msec )
{
   int ii , ct ;
   int nread , dms=0 , ms ;
   int *bstart, *bend , bsize ;  /* for the chosen buffer */

   /** check if the SHMioc is good **/

   ct = NI_clock_time() ;
   if( ct - ioc->goodcheck_time > 2 ){    /* 23 Nov 2004 */
     ii = SHM_goodcheck(ioc,0) ;
     ioc->goodcheck_time = ct ;
     if( ii <= 0  ){                      /* not good yet */
       ii = SHM_goodcheck(ioc,msec) ;     /* so wait for it to get good */
       if( ii <= 0 ) return ii ;          /* if still not good, exit */
     }
   } else if( ioc->bad ) return 0 ;

   /** choose buffer from which to read **/

   switch( ioc->whoami ){

     default: return -1 ;  /* should never happen */

     case SHM_ACCEPTOR:
       bstart = ioc->bstart1 ;
       bend   = ioc->bend1 ;
       bsize  = ioc->bufsize1 ;
     break ;

     case SHM_CREATOR:
       bstart = ioc->bstart2 ;
       bend   = ioc->bend2 ;
       bsize  = ioc->bufsize2 ;
     break ;
   }

   /** must loop and wait **/

   if( msec < 0 ) msec = 999999999 ;      /* a long time (11+ days) */

   /** Compute the number of readable bytes into nread. **/

   for( ms=0 ; ms < msec ; ms += dms ){
     nread = (*bend - *bstart + bsize + 1) % bsize ;
     if( nread > 0 ) return nread ;
     dms = NEXTDMS(dms) ; dms = MIN(dms,msec-ms) ; NI_sleep(dms) ;
     ii = SHM_goodcheck(ioc,0) ; if( ii == -1 ) return -1 ;
   }
   nread = (*bend - *bstart + bsize + 1) % bsize ;
   if( nread > 0 ) return nread ;
   return 0 ;
}

/*---------------------------------------------------------------------------*/
/*! Check if the SHMioc is ready to have data written into it.
  If not, the routine will wait up to msec milliseconds for writing to
  be allowable.  If msec < 0, this routine will wait indefinitely.
  The return value is the number of bytes that can be sent (0 if none,
  positive if some). -1 will be returned if some unrecoverable error is
  detected.
-----------------------------------------------------------------------------*/

static int SHM_writecheck( SHMioc *ioc , int msec )
{
   int ii ;
   int nread , dms=0 , ms , nwrite ;
   int *bstart, *bend , bsize ;  /* for the chosen buffer */

   /** check if the SHMioc is good **/

   ii = SHM_goodcheck(ioc,0) ;
   if( ii == -1 ) return -1 ;        /* some error */
   if( ii == 0  ){                   /* not good yet */
     ii = SHM_goodcheck(ioc,msec) ;  /* so wait for it to get good */
     if( ii <= 0 ) return ii ;       /* if still not good, exit */
   }

   /** choose buffer to which to write **/

   switch( ioc->whoami ){

     default: return -1 ;  /* should never happen */

     case SHM_ACCEPTOR:
       bstart = ioc->bstart2 ;
       bend   = ioc->bend2 ;
       bsize  = ioc->bufsize2 ;
     break ;

     case SHM_CREATOR:
       bstart = ioc->bstart1 ;
       bend   = ioc->bend1 ;
       bsize  = ioc->bufsize1 ;
     break ;
   }

   if( msec < 0 ) msec = 999999999 ;      /* a long time (11+ days) */

   for( ms=0 ; ms < msec ; ms += dms ){
     nread  = (*bend - *bstart + bsize + 1) % bsize ;
     nwrite = bsize - 1 - nread ;
     if( nwrite > 0 ) return nwrite ;
     dms = NEXTDMS(dms) ; dms = MIN(dms,msec-ms) ; NI_sleep(dms) ;
     ii = SHM_goodcheck(ioc,0) ; if( ii == -1 ) return -1 ;
   }
   nread  = (*bend - *bstart + bsize + 1) % bsize ;
   nwrite = bsize - 1 - nread ;
   if( nwrite > 0 ) return nwrite ;
   return 0 ;
}

/*----------------------------------------------------------------------------*/
/*! Send nbytes of data from buffer down the SHMioc.  Return value is
  the number of bytes actually sent, or is -1 if some error occurs.
------------------------------------------------------------------------------*/

static int SHM_send( SHMioc *ioc , char *buffer , int nbytes )
{
   int ii ;
   int nread,nwrite , ebot,etop ;
   int *bstart, *bend , bsize ;  /* for the chosen buffer */
   char *buf ;

   /** check for reasonable inputs **/

   if( ioc    == NULL || ioc->bad   ||
       buffer == NULL || nbytes < 0   ) return -1 ;

   if( nbytes == 0 ) return 0 ;  /* stupid user */

   ii = SHM_goodcheck(ioc,1) ;   /* can't send if it ain't good */
   if( ii <= 0 ) return ii ;

   ii = SHM_writecheck(ioc,1) ;  /* is something is writeable? */
   if( ii <= 0 ) return ii ;

   /** choose buffer in which to write **/

   switch( ioc->whoami ){

     default: return -1 ;  /* should never happen */

     case SHM_ACCEPTOR:
       bstart = ioc->bstart2 ;
       bend   = ioc->bend2 ;
       bsize  = ioc->bufsize2 ;
       buf    = ioc->buf2 ;
     break ;

     case SHM_CREATOR:
       bstart = ioc->bstart1 ;
       bend   = ioc->bend1 ;
       bsize  = ioc->bufsize1 ;
       buf    = ioc->buf1 ;
     break ;
   }

   /** write into the circular buffer, past "bend" **/

   nread  = ( *bend - *bstart + bsize + 1 ) % bsize; /* amount readable  */
   nwrite = bsize - 1 - nread ;                      /* amount writeable */
   if( nwrite <= 0 ) return 0 ;                      /* can't write!     */

   if( nwrite > nbytes ) nwrite = nbytes ;           /* how much to write */

   ebot = *bend+1 ; if( ebot >= bsize ) ebot = 0 ;   /* start at ebot */
   etop = ebot+nwrite-1 ;                            /* end at etop   */

   if( etop < bsize ){                               /* 1 piece to copy  */
      memcpy( buf + ebot, buffer, nwrite ) ;         /* copy data        */
      *bend = etop ;                                 /* change bend      */
   } else {                                          /* 2 pieces to copy */
      int nn = bsize - ebot ;                        /* size of piece 1  */
      memcpy( buf + ebot, buffer   , nn        ) ;   /* copy piece 1     */
      memcpy( buf       , buffer+nn, nwrite-nn ) ;   /* copy piece 2     */
      *bend = nwrite-nn-1 ;                          /* change bend      */
   }
   return nwrite ;
}

/*----------------------------------------------------------------------------*/
/*! Send (exactly) nbytes of data from the buffer down the SHMioc.  The only
   difference between this and SHM_send is that this function will not
   return until all the data is sent, even if it takes forever.
   Under these circumstances, it would be good if the reader process is
   still working.
------------------------------------------------------------------------------*/

static int SHM_sendall( SHMioc *ioc , char *buffer , int nbytes )
{
   int ii , ntot=0 , dms=0 ;

   /** check for reasonable inputs **/

   if( ioc    == NULL || ioc->bad   ||
       buffer == NULL || nbytes < 0   ) return -1 ;

   if( nbytes == 0 ) return 0 ;

   while(1){
      ii = SHM_send( ioc , buffer+ntot , nbytes-ntot ); /* send what's left */
      if( ii == -1 ) return -1 ;                        /* an error!?       */

      if( ii == 0 ){                                    /* nothing sent? */
        dms = NEXTDMS(dms) ;
      } else {                                          /* sent something!   */
        ntot += ii ;                                    /* total sent so far */
        if( ntot >= nbytes ) return nbytes ;            /* all done!?        */
        dms = 1 ;
      }

      NI_sleep(dms) ;                                   /* wait a bit */
   }
   return -1 ;   /* should never be reached */
}

/*----------------------------------------------------------------------------*/
/*! Read up to nbytes of data from the SHMioc, into buffer.  Returns the
  number of bytes actually read.
  This may be less than nbytes (may even be 0).  If an error occurs, -1 is
  returned.
------------------------------------------------------------------------------*/

static int SHM_recv( SHMioc *ioc , char *buffer , int nbytes )
{
   int *bstart, *bend , bsize ;  /* for the chosen buffer */
   char *buf ;
   int nread, sbot,stop , ii ;

   /** check for reasonable inputs **/

   if( ioc    == NULL || ioc->bad   ||
       buffer == NULL || nbytes < 0   ) return -1 ;

   if( nbytes == 0 ) return 0 ;

   ii = SHM_goodcheck(ioc,1) ;
   if( ii <= 0 ) return ii ;

   /** choose buffer from which to read **/

   switch( ioc->whoami ){

     default: return -1 ;  /* should never happen */

     case SHM_ACCEPTOR:
       bstart = ioc->bstart1 ;
       bend   = ioc->bend1 ;
       bsize  = ioc->bufsize1 ;
       buf    = ioc->buf1 ;
     break ;

     case SHM_CREATOR:
       bstart = ioc->bstart2 ;
       bend   = ioc->bend2 ;
       bsize  = ioc->bufsize2 ;
       buf    = ioc->buf2 ;
     break ;
   }

   /** read from the circular buffer, starting at bstart **/

   nread = ( *bend - *bstart + bsize + 1 ) % bsize ;    /* readable amount */
   if( nread <= 0 ) return 0 ;                          /* nothing!?       */
   if( nread > nbytes ) nread = nbytes ;                /* amount to read  */

   sbot = *bstart ; stop = sbot + nread-1 ;             /* from sbot to stop */

   if( stop < bsize ){                                  /* 1 piece to copy */
      memcpy( buffer, buf+sbot, nread ) ;               /* copy the data   */
      *bstart = (stop+1) % bsize ;                      /* move bstart up  */
   } else {                                             /* 2 pieces to copy */
      int nn = bsize - sbot ;                           /* size of piece 1  */
      memcpy( buffer   , buf + sbot, nn        ) ;      /* copy piece 1     */
      memcpy( buffer+nn, buf       , nread-nn  ) ;      /* copy piece 2     */
      *bstart = nread-nn ;                              /* move bstart up   */
   }
   return nread ;
}
#endif /* DONT_USE_SHM */

/*******************************************************************/
/*** Functions to read/write from NI_streams (files or sockets). ***/
/*** These functions use the tcp_ and shm_ functions above to do ***/
/*** most of the real work.                                      ***/
/*******************************************************************/

/*-------------------------------------------------------------------------*/
/*! Open a NIML input or output stream, and return a pointer to it.

  - NULL is returned if an error occurs.
  - Otherwise, you can read and write data using NI_stream_read() and
    NI_stream_write().
  - Buffered input is also available using NI_stream_readbuf() to read
    data from an internal buffer.  The advantage of buffered input is
    that it will return the number of bytes requested (waiting, if needed),
    rather than just the number available at that moment.

  Several different types of streams are available.  The first two
  ("tcp:" and "shm:") are for 2-way interprocess communication.
  The later ones ("file:", "str:", "http:", "ftp:", and "fd:") are
  for 1-way communication, either to read bytes or to write them.
  The formats for the "name" input are described below:

  name = "tcp:host:port" to connect a socket to system "host"
             on the given port number.  One process should open
             in "w" mode and one in "r" mode.

  name = "shm:keyname:size1+size2" to connect to a shared memory
             segment created with "keyname" for the ID and with
             I/O buffer sizes of size1 ("w" process to "r" process)
             and size2 ("r" process to "w" process).
         - Like tcp: streams, one process should open with "w" and the
           other with "r".
         - The "size" strings can end in 'K' to multiply by 1024,
            or end in 'M' to multiply by 1024*1024.
         - If neither size is given, a default value is used.
         - If only size1 is given, size2=size1 (symmetric stream).
         - The total size of the shared memory segment will be
           size1+size2+36 bytes.  (Some systems put an upper limit
           on this size.)
         - "keyname" is a string used to identify this shared memory segment
         - If you are communicating a lot of data between 2 processes on the
           same system, shm: streams are usually much faster than tcp: streams.
         - Also see NI_stream_reopen() for a way to open a tcp: stream and then
           re-open it to another tcp: port or to be a shm: stream.

  name = "file:filename" to open a file for I/O.
         - For this type of name ONLY, you can use "a" as the mode string to
           indicate that you want to append to the file if it already exists.

  name = "str:" to read/write data from/to a string

  name = "http://hostname/filename" to read data from a Web server
  name = "ftp://hostname/filename"  to read data from an FTP server
           - The data for these types is transferred all at once from
             the remote server and stored in a memory buffer (much like
             the str: stream type).
           - Data can be read from this buffer using NI_stream_read().
           - When the stream is closed, the buffer is NI_free()-ed.

  name = "fd:integer" to read or write data from a pre-opened
          file descriptor (returned by the open() function).
           - For example, "fd:1" is used to write to stdout directly.
           - When an "fd:" stream is closed, nothing is actually done;
             closing the descriptor is the responsibility of the application.
           - Descriptors 0,1,2 use stdin, stdout, and stderr, respectively.
           - All other descriptors use fdopen() to open a FILE stream
             and then treat the result like file:.  This means that if
             the descriptor comes from fileno() on a previously opened
             FILE stream, you will have trouble if you mix I/O to this
             stream with NI_stream_read()/NI_stream_write().
           - You can use "stdin:", "stdout:", or "stderr:" as synonyms
             for "fd:0", "fd:1", and "fd:2".

  The formats for the "mode" input are described below:

  mode = "w" to open a stream for writing
           - tcp: host must be specified ("w" is for a tcp client).
           - shm: keyname determines the ID of the segment to create
                  (which can be attached to by a shm: "r" process).
           - file: filename is opened in write mode (and will be
                  overwritten if already exists).
           - str: data will be written to a buffer in the NI_stream
                  struct; you can later access this buffer with the
                  function NI_stream_getbuf(), and clear it with
                  NI_stream_clearbuf().
           - You can't open "fd:0" (stdin) for reading
           - You can't open "http:" or "ftp:" streams for writing.
           - "a" can be used for "file:" ONLY to append to a file.

  mode = "r" to open a stream for reading
           - tcp: host is ignored (but must be present);
                  ("r" is for a tcp server).
           - shm: keyname determines the ID of the segment to attach to
                  (which must be created by a shm: "w" process).
           - file: filename is opened in read mode.
           - str: characters after the colon are the source of
                  the input data (will be copied to internal buffer);
                  OR, you can later set the internal buffer string
                  later with function NI_stream_setbuf().
           - You can't open "fd:1" or "fd:2" (stdout or stderr) for reading.
           - ftp:/http: The remote files are fetched and loaded into
                  memory.  After that, these streams operate pretty
                  much the same as str: streams for reading.

  For a file:, fd:, or str: stream, you can either read from or write to
  the stream, but not both, depending on how you opened it ("r" or "w").
  For a tcp: or shm: stream, once it is connected, you can both read and write.
  The asymmetry in tcp: and shm: streams only comes at the opening (one process
  must make the call using "w" and one must listen for the call using "r").

  The inputs "host" (for tcp:) and "filename" (for file:) are limited to a
  maximum of 127 bytes.  For str:, there is no limit for the "r" stream
  (but clearly you can't have any NUL bytes in there).  For shm:, "keyname"
  is limited to 127 bytes also.

  Since opening a socket or shared memory segment requires sychronizing
  two processes, you can't read or write to a tcp: or shm: stream
  immediately.  Instead you have to check if it is "good" first.  This
  can be done using the function NI_stream_goodcheck().

  After a tcp: "r" stream is good, then the string ns->name
  contains the IP address of the connecting host, in "dot" form
  (e.g., "201.202.203.204"); here, "ns" is the NI_stream returned
  by this routine.
   - You can use the NI_add_trusted_host() function
     to set a list of IP addresses from which the NIML library will accept
     connections.
   - Systems not on the trusted list will have their sockets closed
     immediately after the connection is accepted.  Nothing will be read
     from these sockets.

  For a file: stream, ns->name contains the filename.
---------------------------------------------------------------------------*/

NI_stream NI_stream_open( char *name , char *mode )
{
   NI_stream_type *ns ;
   int do_create , do_accept ;
   int ni_bufsize = NI_BUFSIZE ;  /* 21 Nov 2007 */

   /** perhaps initialize debug output **/

#ifdef NIML_DEBUG
   if( dfp == NULL ){
     char *eee = getenv("NIML_DEBUG") ;
     if( eee != NULL ){
       dfp = (strcmp(eee,"stderr")==0) ? stderr : fopen(eee,"w") ;
       if( dfp == NULL ){ dfp = stderr; eee = "stderr [defaulted]"; }
       fprintf(stderr,"NIML: debug output to %s\n",eee) ;
     }
   }
#endif

#ifdef NIML_DEBUG
   NI_malloc_enable_tracking() ;
#endif

   /** check if inputs are reasonable **/

   if( NI_strlen(name) < 4 ) return NULL ;

   if( mode == NULL ) return NULL ;

   do_create = (*mode == 'w' || *mode == 'a') ;
   do_accept = (*mode == 'r') ;

   if( !do_create && !do_accept ) return NULL ;

   if( ! atexit_is_setup ){         /* 22 Apr 2005 */
     atexit(atexit_open_streams) ; atexit_is_setup = 1 ;
   }

   /* 21 Nov 2007: alter default buffer size? */

   { char *eee ; int bbb ;
                       eee = getenv( "AFNI_NIML_BUFSIZE" ) ;
     if( eee == NULL ) eee = getenv( "NIML_BUFSIZE" ) ;
     if( eee == NULL ) eee = getenv( "BUFSIZE") ;
     if( eee != NULL ){
       bbb = (int)strtod(eee,NULL) ;
       if( bbb > ni_bufsize ) ni_bufsize = bbb ;
     }
   }

   /************************************/
   /***** deal with TCP/IP sockets *****/

   if( strncmp(name,"tcp:",4) == 0 ){
      char host[256] , *hend ;
      int  port=-1 , ii , jj ;

      if( NI_strlen(name) > 127 ) return NULL ;

      /** find "host" substring **/

      hend = strstr( name+4 , ":" ) ;
      if( hend == NULL || hend-name > 255 ) return NULL ;

      for( ii=4 ; name[ii] != ':' ; ii++ ) host[ii-4] = name[ii] ;
      host[ii-4] = '\0' ;

      /** get "port" number **/

      port = strtol( name+ii+1 , NULL , 10 ) ;
      if( port <= 0 ) return NULL ;

      /** initialize NI_stream_type output struct **/

      ns = NI_malloc(NI_stream_type, sizeof(NI_stream_type) ) ;

      ns->type = NI_TCP_TYPE;   /* what kind is this? */
      ns->port = port ;         /* save the port #    */
      ns->nbuf = 0 ;            /* buffer is empty    */
      ns->npos = 0 ;            /* scan starts at 0   */
      ns->b64_numleft = 0 ;

      ns->buf     = NI_malloc(char, ni_bufsize) ;
      ns->bufsize = ni_bufsize ;
      ns->name[0] = '\0' ;
      NI_strncpy(ns->orig_name,name,256) ;  /* 23 Aug 2002 */

      ns->bin_thresh = -1 ;     /* write in text mode */

      /* 02 Jan 2004: setup SIGURG handler for OOB data reception. */

      if( !sigurg ){ signal(SIGURG,tcp_sigurg_handler); sigurg = 1; }

      /** attach to incoming call "r" **/

      if( do_accept ){
         ns->io_mode = NI_INPUT_MODE ;
         ns->sd = tcp_listen( port ) ;                   /* set up to listen  */
         if( ns->sd < 0 ){                               /* error? must die!  */
           NI_free(ns->buf); NI_free(ns); return NULL;
         }
         ns->bad = TCP_WAIT_ACCEPT ;                     /* not connected yet */
         ii = tcp_readcheck(ns->sd,1) ;                  /* see if ready      */
         if( ii > 0 ){                                   /* if socket ready:  */
           jj = tcp_accept( ns->sd , NULL,&hend ) ;      /* accept connection */
           if( jj >= 0 ){                                /* if accept worked  */
             CLOSEDOWN( ns->sd ) ;                       /* close old socket  */
             NI_strncpy(ns->name,hend,256) ;             /* put IP into name  */
             NI_free(hend); ns->bad = 0; ns->sd = jj ;   /* and ready to go!  */
             fcntl( ns->sd, F_SETOWN, (int)getpid() ) ;  /* 02 Jan 2004 */
           }
         }

         add_open_stream(ns) ;       /* 02 Jan 2004 */
         ns->goodcheck_time = -99 ;  /* 23 Nov 2004 */
         return ns ;
      }

      /** place an outgoing call "w" **/

      if( do_create ){
        struct hostent *hostp ;
        ns->io_mode = NI_OUTPUT_MODE ;
        hostp = gethostbyname(host) ;                   /* lookup host on net */
        if( hostp == NULL ){                            /* fails? must die!   */
          NI_free(ns->buf); NI_free(ns); return NULL;
        }
        ns->sd  = tcp_connect( host , port ) ;          /* connect to host    */
        ns->bad = (ns->sd < 0) ? TCP_WAIT_CONNECT : 0 ; /* fails? must wait   */
        NI_strncpy(ns->name,host,256) ;                 /* save the host name */
        if( ns->sd >= 0 )
          fcntl( ns->sd, F_SETOWN, (int)getpid() ) ;    /* 02 Jan 2004 */

        add_open_stream(ns) ;   /* 02 Jan 2004 */
        ns->goodcheck_time = -99 ;  /* 23 Nov 2004 */
        return ns ;
      }
      return NULL ;  /* should never be reached */
   }

#ifndef DONT_USE_SHM
   /*********************************************/
   /***** deal with shared memory transport *****/

   if( strncmp(name,"shm:",4) == 0 ){
      SHMioc *ioc ;

      if( *mode == 'a' ) mode = "w" ;
      ioc = SHM_init( name , mode ) ;  /* open segment */
      if( ioc == NULL ) return NULL ;  /* this is bad bad bad */

      /** initialize NI_stream_type output **/

      ns = NI_malloc(NI_stream_type, sizeof(NI_stream_type) ) ;

      ns->type     = NI_SHM_TYPE;    /* what kind is this? */
      ns->nbuf     = 0 ;             /* buffer is empty    */
      ns->npos     = 0 ;             /* scan starts at 0   */
      ns->io_mode  = do_create ? NI_OUTPUT_MODE
                               : NI_INPUT_MODE  ;
      ns->bad      = 0 ;
      ns->shmioc   = ioc ;
      ns->b64_numleft = 0 ;

      ns->buf      = NI_malloc(char, ni_bufsize) ;
      ns->bufsize  = ni_bufsize ;

      NI_strncpy( ns->name , name , 256 ) ;

      NI_strncpy(ns->orig_name,name,256) ;  /* 23 Aug 2002 */

      add_open_stream(ns) ;  /* 02 Jan 2004 */
      ns->goodcheck_time = -99 ;  /* 23 Nov 2004 */
      return ns ;
   }
#endif /* DONT_USE_SHM */

   /**********************************/
   /***** deal with simple files *****/

   if( strncmp(name,"file:",5) == 0 ){

      char *fname = name+5 , *fmode ;
      FILE *fp ;

      if( NI_strlen(name) > 255 || NI_strlen(fname) < 1 ) return NULL ;

      if( *mode == 'a' ) fmode = "ab" ;
      else               fmode = do_create ? (char *)"wb" : (char *)"rb" ;
      fp = fopen( fname , fmode ) ;

      if( fp == NULL ) return NULL ;

      /** initialize NI_stream_type output **/

      ns = NI_malloc(NI_stream_type, sizeof(NI_stream_type) ) ;

      ns->type     = NI_FILE_TYPE;   /* what kind is this? */
      ns->nbuf     = 0 ;             /* buffer is empty    */
      ns->npos     = 0 ;             /* scan starts at 0   */
      ns->fp       = fp ;
      ns->io_mode  = do_create ? NI_OUTPUT_MODE
                               : NI_INPUT_MODE  ;
      ns->bad      = 0 ;
      ns->b64_numleft = 0 ;

      ns->bufsize  = do_create ? 16 : ni_bufsize ;
      ns->buf      = NI_malloc(char, ns->bufsize) ;

      NI_strncpy( ns->name , fname , 256 ) ;

      NI_strncpy(ns->orig_name,name,256) ;  /* 23 Aug 2002 */

      if( ns->io_mode == NI_INPUT_MODE )     /* save the file size */
         ns->fsize = NI_filesize( fname ) ;  /* if we are reading  */
      else
         ns->fsize = -1 ;

      add_open_stream(ns) ;  /* 02 Jan 2004 */
      ns->goodcheck_time = -99 ;  /* 23 Nov 2004 */
      return ns ;
   }

   /********************************************************************/
   /***** fd: very similar to a file, but we don't have to open it *****/

        if( strncmp(name,"stdin:" ,6) == 0 ) name = "fd:0" ;  /* 25 Mar 2003 */
   else if( strncmp(name,"stdout:",7) == 0 ) name = "fd:1" ;
   else if( strncmp(name,"stderr:",7) == 0 ) name = "fd:2" ;

   if( strncmp(name,"fd:",3) == 0 ){
      int fd=-1 ; FILE *fp ;

      sscanf(name+3,"%d",&fd) ;
      if( fd < 0 ) return NULL ;   /* bad integer */

      switch( fd ){
        default:
          fp = fdopen( fd , do_create ? "wb" : "rb" ) ;
          if( fp == NULL ) return NULL ;
        break ;

        case 0:
          fp = stdin ;
          if( do_create ) return NULL ;
        break ;

        case 1:
          fp = stdout ;
          if( !do_create ) return NULL ;
        break ;

        case 2:
          fp = stderr ;
          if( !do_create ) return NULL ;
        break ;
      }

      /** initialize NI_stream_type output **/

      ns = NI_malloc(NI_stream_type, sizeof(NI_stream_type) ) ;

      ns->type     = NI_FD_TYPE;     /* what kind is this? */
      ns->nbuf     = 0 ;             /* buffer is empty    */
      ns->npos     = 0 ;             /* scan starts at 0   */
      ns->fp       = fp ;
      ns->io_mode  = do_create ? NI_OUTPUT_MODE
                               : NI_INPUT_MODE  ;
      ns->bad      = 0 ;
      ns->b64_numleft = 0 ;

      ns->bufsize  = do_create ? 16 : ni_bufsize ;
      ns->buf      = NI_malloc(char, ns->bufsize) ;

      NI_strncpy( ns->name , name , 256 ) ;

      NI_strncpy(ns->orig_name,name,256) ;  /* 23 Aug 2002 */

      ns->fsize = -1 ;

      add_open_stream(ns) ;  /* 02 Jan 2004 */
      ns->goodcheck_time = -99 ;  /* 23 Nov 2004 */
      return ns ;
   }

   /*********************************/
   /***** str: string array I/O *****/

   if( strncmp(name,"str:",4) == 0 ){

      int nn = NI_strlen(name+4) ;  /* may be 0 */

      ns = NI_malloc(NI_stream_type, sizeof(NI_stream_type) ) ;

      ns->type     = NI_STRING_TYPE; /* what kind is this? */
      ns->io_mode  = do_create ? NI_OUTPUT_MODE
                               : NI_INPUT_MODE  ;
      ns->bad      = 0 ;
      ns->npos     = 0 ;             /* scan starts at 0   */
      ns->b64_numleft = 0 ;

      /* Note that bufsize == nbuf+1 for str:
         This is because we don't count the terminal NUL
         in nbuf (number of readable bytes),
         but do count it in bufsize (size of the buf array) */

      if( do_accept ){               /* read from stuff after str: */
         ns->nbuf    = nn ;
         ns->bufsize = nn+1 ;
         ns->buf     = NI_malloc(char, nn+1) ;
         strcpy(ns->buf,name+4) ;
      } else {                       /* write to a string */
         ns->nbuf    = 0 ;
         ns->bufsize = 1 ;
         ns->buf     = NI_malloc(char, 1) ; /* 1 byte set to zero */
      }

      strcpy( ns->name , "ElvisHasLeftTheBuilding" ) ;

      NI_strncpy(ns->orig_name,name,256) ;  /* 23 Aug 2002 */

      add_open_stream(ns) ;  /* 02 Jan 2004 */
      ns->goodcheck_time = -99 ;  /* 23 Nov 2004 */
      return ns ;
   }

   /*********************************/
   /***** http:// or ftp:// I/O *****/

   if( strncmp(name,"http://",7) == 0 || strncmp(name,"ftp://",6) == 0 ){
      int nn ;
      char *data=NULL ;

      if( do_create ) return NULL ;                  /* bad */

      nn = NI_read_URL( name , &data ) ;

      if( data == NULL || nn <= 4 ){                 /* bad */
         NI_free(data); return NULL;
      }

      ns = NI_malloc(NI_stream_type, sizeof(NI_stream_type) ) ;

      ns->type     = NI_REMOTE_TYPE; /* what kind is this? */
      ns->io_mode  = NI_INPUT_MODE  ;
      ns->bad      = 0 ;
      ns->npos     = 0 ;             /* scan starts at 0   */
      ns->nbuf     = nn ;
      ns->bufsize  = nn ;
      ns->buf      = data ;
      ns->b64_numleft = 0 ;

      NI_strncpy( ns->name , name , 256 ) ;

      NI_strncpy(ns->orig_name,name,256) ;  /* 23 Aug 2002 */

      add_open_stream(ns) ;  /* 02 Jan 2004 */
      ns->goodcheck_time = -99 ;  /* 23 Nov 2004 */
      return ns ;
   }

   return NULL ;  /* should never be reached */
}

/*---------------------------------------------------------------------------*/
/*! Re-open a NI_stream on a different channel.  This is only possible
    if the input original stream (ns) is tcp: type.
     - The new stream (nname) can be of the form "tcp::port",
       which will reopen the stream to the same host on the new port.
     - Or the new stream can be of the form "shm:key:size1+size2",
       but only if the existing stream was opened to localhost.
     - The main purpose of this function is to let a process connect
       to a "standard" port on a server process, then tell the server
       to reopen on another port (or to use shm:).  In this way, the
       standard port can be freed up quickly for reuse.

    If necessary, this function will wait until the connection to the
    other program is ready.  Then it will exchange the information with
    the other program about changing things, and will again wait until
    the new connection is established.  Assuming all goes well, then
    when this function returns, the input stream (ns) will be modified
    so that it now refers to the new connection.

    Return value is 1 if things are OK, 0 if not.  Failure can occur
    because:
     - Input ns or nname was badly formed.
     - You tried to open shm: when the input tcp: stream was not to localhost.
     - The input tcp: stream can't become connected within 10 seconds.
-----------------------------------------------------------------------------*/

int NI_stream_reopen( NI_stream_type *ns , char *nname )
{
   NI_stream_type *nsnew ;
   int typ_new=0 , port_new=0 , jj,kk ;
   char msg[1024] ;

   /* check inputs for sanity */

   if( ns == NULL || ns->type != NI_TCP_TYPE ) return 0 ; /* bad input stream */
   if( ns->bad == MARKED_FOR_DEATH )           return 0 ; /* really bad */
   if( nname == NULL || nname[0] == '\0' )     return 0 ; /* bad new name */

   if( strncmp(nname,"tcp::",5) == 0 ){                   /* new is tcp:? */
      typ_new = NI_TCP_TYPE ;
      port_new = strtol(nname+5,NULL,10) ;
      if( port_new <= 0        ) return 0 ;               /* bad new port */
      if( port_new == ns->port ) return 1 ;               /* same as before? */
#ifndef DONT_USE_SHM
   } else if( strncmp(nname,"shm:" ,4) == 0 ){            /* new is shm:? */
      char *eee = getenv("AFNI_NOSHM") ;                  /* 06 Jun 2003 */
      if( eee != NULL && toupper(*eee) == 'Y' ){          /* shm: is disabled */
        fprintf(stderr,"** NI_stream_reopen: shm is disabled\n");
        return 0 ;
      }
      if( strstr(ns->orig_name,":localhost:") == NULL ){  /* can't do shm: */
        fprintf(stderr,"** NI_stream_reopen: shm not localhost!\n");  /* but on localhost */
        return 0 ;
      }
#endif
   } else {
     fprintf(stderr,"** NI_stream_reopen: illegal input '%s'\n",nname);
     return 0 ;                                           /* bad new name */
   }

#ifdef NIML_DEBUG
NI_dpr("NI_stream_reopen: waiting for original connection to be good\n") ;
#endif

   /* wait for existing stream to be connected */

   for( kk=0 ; kk < 10 ; kk++ ){
     jj = NI_stream_goodcheck( ns , 1000 ) ;   /* wait 1 sec */
     if( jj > 0 ) break;                       /* good :-) */
     if( kk == 0 )
       fprintf(stderr,"++ NI_stream_reopen: Waiting for socket connection") ;
     else
       fprintf(stderr,".") ;
   }
   if( kk == 10 ){ fprintf(stderr," *Failed*\n"); return 0; }
   if( kk >  0  )  fprintf(stderr," *Good*\n") ;

   /* open new stream as the writer */

   if( strncmp(nname,"tcp::",5) == 0 ){
     sprintf(msg,"tcp:%s:%d",ns->name,port_new) ;  /* old hostname */
   }
#ifndef DONT_USE_SHM
   else if( strncmp(nname,"shm:" ,4) == 0 ){
     NI_strncpy(msg,nname,1024) ;
   }
#endif

#ifdef NIML_DEBUG
NI_dpr("NI_stream_reopen: opening new stream %s\n",msg) ;
#endif

   nsnew = NI_stream_open( msg, "w" ) ;
   if( nsnew == NULL ) return 0 ;             /* bad :-( */

   /* send message on old stream to other
      program, telling it to open the new stream */

   sprintf(msg,"<?ni_do ni_verb='reopen_this' ni_object='%s' ?>\n",nname) ;
   kk = strlen(msg) ;

#ifdef NIML_DEBUG
NI_dpr("NI_stream_reopen: sending message %s",msg) ;
#endif

   jj = NI_stream_write( ns , msg , kk ) ;
   if( jj < kk ){
     NI_stream_closenow(nsnew) ; return 0 ;  /* bad write! */
   }

   /* now wait for other program to open the new stream */

#ifdef NIML_DEBUG
NI_dpr("NI_stream_reopen: waiting for new stream to be good\n") ;
#endif

   jj = NI_stream_goodcheck( nsnew , 5000 ) ;  /* wait up to 5 sec */
   if( jj <= 0 ){
     NI_stream_closenow(nsnew) ; return 0 ;  /* never got good */
   }

   /* if here, new stream is ready:
      close the old stream and replace its
      contents with the contents of the new stream */

#ifdef NIML_DEBUG
NI_dpr("NI_stream_reopen: closing old stream\n") ;
#endif

   NI_stream_close_keep(ns,0) ;   /* will be removed from open streams list */

   *ns = *nsnew ;

   /* 10 Jun 2005: at this point, nsnew is in the open streams list,
                   but the pointer nsnew is about to die, and so we
                   must munge the open streams list around now to
                   make sure that nsnew is removed and ns is re-added! */

   remove_open_stream(nsnew) ; NI_free(nsnew) ; add_open_stream(ns) ;

   return 1 ; /* :-) */
}

/*-----------------------------------------------------------------------*/
/*! Seek file: stream to a specific offset location.
      - whence is one of SEEK_SET, SEEK_CUR, SEEK_END (cf. "man fseek").
      - Also clears the stream buffer.
-------------------------------------------------------------------------*/

void NI_stream_seek( NI_stream_type *ns , int offset , int whence )
{
   if( ns          == NULL             ||
       ns->bad     == MARKED_FOR_DEATH ||
       ns->type    != NI_FILE_TYPE     ||
       ns->fp      == NULL               ) return ;

   fseek( ns->fp , offset , whence ) ;   /* seek file */
   ns->nbuf = ns->npos = 0 ;             /* clear buffer */
}

/*-----------------------------------------------------------------------*/
/*! Return 1 if it is legal to read from this stream, 0 if it isn't.
    This doesn't say anything about if it is practical to read
    at this moment; for that, use NI_stream_readcheck().
-------------------------------------------------------------------------*/

int NI_stream_readable( NI_stream_type *ns )
{
   if( ns == NULL || ns->bad == MARKED_FOR_DEATH ) return 0 ;
   if( ns->type == NI_TCP_TYPE || ns->type == NI_SHM_TYPE ) return 1 ;
   return (ns->io_mode == NI_INPUT_MODE) ;
}

/*-----------------------------------------------------------------------*/
/*! Return 1 if it is legal to write to this stream, 0 if it isn't.
    This doesn't say anything about if it is practical to write
    at this moment; for that, use NI_stream_writecheck().
-------------------------------------------------------------------------*/

int NI_stream_writeable( NI_stream_type *ns )
{
   if( ns == NULL || ns->bad == MARKED_FOR_DEATH ) return 0 ;
   if( ns->type == NI_TCP_TYPE || ns->type == NI_SHM_TYPE ) return 1 ;
   return (ns->io_mode == NI_OUTPUT_MODE) ;
}

/*-----------------------------------------------------------------------*/
/*! Return the name set in the NI_stream header.  (This is the pointer
    to the internal string, so don't free it!)
-------------------------------------------------------------------------*/

char * NI_stream_name( NI_stream_type *ns )
{
   if( ns == NULL ) return NULL ;
   return ns->name ;
}

/*-----------------------------------------------------------------------*/
/*! Alter the input buffer size for a NI_stream.
    - Only works for tcp: & shm: streams, and for "r" file: & fd: streams.
    - Return value is 1 if it worked OK, -1 if it didn't.
    - NI_realloc() is used, so buffer contents aren't affected (if the
      size is increased!).
-------------------------------------------------------------------------*/

int NI_stream_setbufsize( NI_stream_type *ns , int bs )   /* 03 Jan 2003 */
{
   char *qbuf ;
   if( ns       == NULL             ||
       ns->type == NI_STRING_TYPE   ||
       ns->bad  == MARKED_FOR_DEATH ||
       bs       <  666              ||
       bs       <  ns->nbuf           ) return -1 ;  /* bad inputs */

   if( !(  ns->type == NI_TCP_TYPE  || ns->type    == NI_SHM_TYPE    ||
          (ns->type == NI_FILE_TYPE && ns->io_mode == NI_INPUT_MODE) ||
          (ns->type == NI_FD_TYPE   && ns->io_mode == NI_INPUT_MODE)   ) )
    return -1 ;

   qbuf = NI_realloc( ns->buf , char , bs ) ;
   if( qbuf == NULL ) return -1 ;         /* this is bad */
   ns->buf     = qbuf ;
   ns->bufsize = bs ;
   return 1 ;
}

/*-----------------------------------------------------------------------*/
/*! Get the input buffer size for a NI_stream.
    Returns -1 if the stream is bad, or has been sentenced to death.
-------------------------------------------------------------------------*/

int NI_stream_getbufsize( NI_stream_type *ns )            /* 03 Jan 2003 */
{
   if( ns == NULL || ns->bad  == MARKED_FOR_DEATH ) return -1 ;
   return ns->bufsize ;
}

/*-----------------------------------------------------------------------*/
/*! Return the output string buffer for a NI_stream of str: type.
    If the input is not a "w" str: stream, then NULL is returned.
    Otherwise a pointer to the internal buffer is returned.
    This will be a NUL terminated string.
-------------------------------------------------------------------------*/

char * NI_stream_getbuf( NI_stream_type *ns )
{
   if( ns          == NULL             ||
       ns->type    != NI_STRING_TYPE   ||
       ns->io_mode != NI_OUTPUT_MODE   ||
       ns->bad     == MARKED_FOR_DEATH   ) return NULL ;  /* bad inputs */

   return ns->buf ;
}

/*-----------------------------------------------------------------------*/
/*! Clear the buffer of a str: writing NI_stream.  This is intended to
    let you write anew without having to close and open again.
-------------------------------------------------------------------------*/

void NI_stream_clearbuf( NI_stream_type *ns )
{
   if( ns          == NULL           ||
       ns->type    != NI_STRING_TYPE ||
       ns->io_mode != NI_OUTPUT_MODE   ) return ;  /* bad inputs */

   NI_free(ns->buf) ;
   ns->nbuf    = 0 ;
   ns->bufsize = 1 ;
   ns->buf     = NI_malloc(char, 1) ; /* 1 byte set to zero */
}

/*-----------------------------------------------------------------------*/
/*! Reset the input string buffer for a NI_stream of str: type.
    If the input is not a "r" str: stream, then nothing happens.
    Otherwise, the current contents of the buffer are discarded,
    and the buffer is replaced with a copy of the input string.
-------------------------------------------------------------------------*/

void NI_stream_setbuf( NI_stream_type *ns , char *str )
{
   int nn ;

   if( ns          == NULL             ||
       ns->type    != NI_STRING_TYPE   ||
       ns->io_mode != NI_INPUT_MODE    ||
       str         == NULL             ||
       ns->bad     == MARKED_FOR_DEATH   ) return ;  /* bad inputs */

   NI_free(ns->buf) ;               /* take out the trash */
   nn = NI_strlen(str) ;            /* size of new buffer string */
   ns->nbuf    = nn ;               /* set num char in new buffer */
   ns->npos    = 0  ;               /* reset scan position */
   ns->bufsize = nn+1 ;             /* allow space for NUL byte */
   ns->buf     = NI_malloc(char, nn+1) ;  /* and make the buffer */
   strcpy(ns->buf,str) ;            /* and set its contents */
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Check if the given NI_stream is properly opened for I/O.

   If not, wait up to msec milliseconds to establish the connection to
   the other end; if msec < 0, will wait nearly forever.
   Returns 1 if ready; 0 if not (but may become good later);
   -1 if an error occurs.
   Possible -1 errors are:
     - ns was connected to a socket, and now has become disconnected
     - ns is passed in as NULL (bad user, bad bad bad)
     - ns is reading a file or a string, and we are already at its end
     - ns is marked for death
   The only cases in which 0 is returned is if the NI_stream is
   tcp: or shm: and the stream is waiting for a connection from
   the other program.  These are also the only cases in which input
   parameter msec is actually used.
-------------------------------------------------------------------------*/

int NI_stream_goodcheck( NI_stream_type *ns , int msec )
{
   int ii , jj ;
   char *bbb ;

   /** check inputs for OK-osity **/

   if( ns == NULL || ns->bad == MARKED_FOR_DEATH ) return -1 ;

   switch( ns->type ){

#ifndef DONT_USE_SHM
      /** Shared memory **/

      case NI_SHM_TYPE:
        return SHM_goodcheck( ns->shmioc , msec ) ;
#endif

      /** File I/O [there is never any waiting here] **/

      case NI_FILE_TYPE:
        if( ns->fp == NULL ) return -1 ;        /* should never happen */
        if( ns->io_mode == NI_INPUT_MODE )
          return NI_stream_readcheck(ns,0) ;    /* input mode */
        else
          return 1 ;                            /* output mode */

      case NI_FD_TYPE:
          return 1 ;                            /* no way to check */

      /** String I/O **/

      case NI_STRING_TYPE:
        if( ns->io_mode == NI_INPUT_MODE )
          return NI_stream_readcheck(ns,0) ;    /* input mode */
        else
          return 1 ;                            /* output mode */

      /** remote Web input */

      case NI_REMOTE_TYPE:
        if( ns->io_mode == NI_INPUT_MODE )
          return NI_stream_readcheck(ns,0) ;    /* input mode */
        else
          return -1 ;                           /* output mode */

      /** Socket I/O **/

      case NI_TCP_TYPE:
        if( ns->bad == 0 ){  /** if good before, then check if is still good **/
          int ich ;
          ich = tcp_alivecheck(ns->sd) ;

#ifdef NIML_DEBUG
          if( ich == 0 )  /* 17 Jun 2003 */
            NI_dpr("++ Socket %s (port %d) has gone bad!\n",ns->name,ns->port);
#endif

          if( ich == 0 ) return -1 ;
          return 1 ;
        }

        /** wasn't good before, so check if that condition has changed **/

        /** TCP/IP waiting to accept call from another host **/

        if( ns->bad == TCP_WAIT_ACCEPT ){
          ii = tcp_readcheck(ns->sd,msec) ;             /* see if ready      */
          if( ii > 0 ){                                 /* if socket ready:  */
            jj = tcp_accept( ns->sd , NULL,&bbb ) ;     /* accept connection */
            if( jj >= 0 ){                              /* if accept worked  */
              CLOSEDOWN( ns->sd ) ;                     /* close old socket  */
              NI_strncpy(ns->name,bbb,256) ;            /* put IP into name  */
              NI_free(bbb); ns->bad = 0; ns->sd = jj;   /* and ready to go!  */
              fcntl( ns->sd, F_SETOWN, (int)getpid() ); /* 02 Jan 2004 */
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
          if( ns->sd >= 0 )
            fcntl( ns->sd, F_SETOWN, (int)getpid() );         /* 02 Jan 2004 */
        }

        /** see if it turned from bad to good **/

        return (ns->bad == 0) ;
   }

   return -1 ;  /* unreachable, I hope */
}

/*---------------------------------------------------------------------------*/
/*! Close a NI_stream, but don't free the insides.
    If (flag&1 != 0) send a "close_this" message to the other end.
    If (flag&2 != 0) use TCP OOB data to send a SIGURG to the other end.
    If (flag&4 != 0) don't remove from open_stream list [only from atexit()]
-----------------------------------------------------------------------------*/

void NI_stream_close_keep( NI_stream_type *ns , int flag )
{
   if( ns == NULL || !isgraph(ns->orig_name[0]) ) return ;

   if( (flag & 4) == 0 )         /* 22 Apr 2005 */
     remove_open_stream( ns ) ;  /* 02 Jan 2004 */

   if( ns->bad == MARKED_FOR_DEATH ){
     if( ns->buf != NULL ){ NI_free(ns->buf); ns->buf = NULL;}
     return ;
   }

   /*-- 20 Dec 2002: write a farewell message to the other end? --*/

   if( (flag & 1) != 0                                      &&
       (ns->type == NI_TCP_TYPE || ns->type == NI_SHM_TYPE) &&
       NI_stream_writecheck(ns,1) > 0                          ){

     NI_stream_writestring( ns , "<?ni_do ni_verb='close_this' ?>\n" ) ;
     NI_sleep(9) ;  /* give it an instant to read the message */
   }

   /*-- mechanics of closing for different stream types --*/

   switch( ns->type ){

#ifndef DONT_USE_SHM
      case NI_SHM_TYPE:
        NI_sleep(9) ;                          /* 31 Mar 2005 */
        SHM_close( ns->shmioc ) ;              /* detach shared memory */
      break ;
#endif

      case NI_FD_TYPE:
        if( ns->fp != NULL && ns->io_mode == NI_OUTPUT_MODE ) fflush(ns->fp) ;
      break ;

      case NI_REMOTE_TYPE:
      case NI_STRING_TYPE:                     /* nothing to do */
      break ;

      case NI_FILE_TYPE:
        if( ns->fp != NULL ) fclose(ns->fp) ;  /* close file */
      break ;

      case NI_TCP_TYPE:
        if( ns->sd >= 0 ){
          if( (flag & 2) != 0 ){
            tcp_send( ns->sd , "X" , 1 , MSG_OOB ) ;   /* 02 Jan 2004 */
            NI_sleep(9) ;
          }
          NI_sleep(2) ;        /* 31 Mar 2005 */
          CLOSEDOWN(ns->sd) ;  /* close socket */
        }
      break ;
   }

   ns->bad = MARKED_FOR_DEATH ; /* label this as unclean, not to be touched */
   if( (flag & 4) == 0 ){       /* only free buf if program is NOT exiting */
     NI_free(ns->buf) ; ns->buf = NULL ;
   }
   return ;
}

/*-----------------------------------------------------------------------*/
/*! Close a NI_stream.  Note that this will also free what ns points to.
    Don't use this pointer again.
    Use the NI_STREAM_CLOSE macro to call this function and then
    also set the pointer "ns" to NULL.
-------------------------------------------------------------------------*/

void NI_stream_close( NI_stream_type *ns )
{
   NI_stream_close_keep(ns,1) ; NI_free(ns) ; return ;
}

/*-----------------------------------------------------------------------*/
/*! Close a NI_stream without sending a "close_this"
    message to the other end of the stream.         */

void NI_stream_closenow( NI_stream_type *ns )
{
   NI_stream_close_keep(ns,0) ; NI_free(ns) ; return ;
}

/*-----------------------------------------------------------------------*/
/* Close a NI_stream with a "close_this" message and also using
   TCP OOB data (for socket streams, that is) to notify the other end. */

void NI_stream_kill( NI_stream_type *ns )
{
   NI_stream_close_keep(ns,3) ; NI_free(ns) ; return ;
}

/*---------------------------------------------------------------------------*/
/*!  Check if the NI_stream has data read to be read, or has data stored
     in its internal buffer.
     - Return values are as in NI_stream_readcheck().
     - Also see NI_stream_readbuf().
-----------------------------------------------------------------------------*/

int NI_stream_hasinput( NI_stream_type *ns , int msec )
{
   if( ns == NULL || ns->bad == MARKED_FOR_DEATH ) return -1 ;

   if( ns->npos < ns->nbuf ) return 1 ;      /* check if has data in buffer */
   return NI_stream_readcheck( ns , msec ) ; /* see if any data can be read */
}

/*---------------------------------------------------------------------------*/
/*!  Check if the NI_stream is ready to have data read out of it.

  If not, the routine will wait up to msec milliseconds for data to be
  available.  If msec < 0, this routine will wait nearly forever.
  The return value is 1 if data is ready, 0 if not; -1 will be returned
  if some unrecoverable error is detected:
   - tcp: the socket connection was dropped
   - shm: the other process died or detached the segment
   - file: you have reached the end of the file, and are still trying to read.

  Also see NI_stream_hasinput() and NI_stream_readbuf().
-----------------------------------------------------------------------------*/

int NI_stream_readcheck( NI_stream_type *ns , int msec )
{
   int ii ;

   if( ns == NULL || ns->bad == MARKED_FOR_DEATH ) return -1 ;

   switch( ns->type ){

#ifndef DONT_USE_SHM
      case NI_SHM_TYPE:
        ii = SHM_readcheck( ns->shmioc , msec ) ;
        if( ii > 0 ) ii = 1 ;
        return ii ;
#endif

      /** tcp: ==> uses the Unix "select" mechanism **/

      case NI_TCP_TYPE:
        ii = NI_stream_goodcheck(ns,0) ;       /* check if it is connected */
        if( ii == -1 ) return -1 ;             /* some error */
        if( ii == 0  ){                        /* not good yet */
          ii = NI_stream_goodcheck(ns,msec) ;  /* so wait for it to get good */
          if( ii != 1 ) return ii ;            /* if still not good, exit */
        }
        ii = tcp_alivecheck( ns->sd ) ;        /* see if it is still open  */
        if( !ii ) return -1 ;                  /* if not open, error exit  */
        ii = tcp_readcheck( ns->sd , msec ) ;  /* see if any data is there */
        return ii ;

      /** fd: ==> use select, as in tcp: **/

      case NI_FD_TYPE:
        ii = tcp_readcheck( fileno(ns->fp) , msec ) ;
        return ii ;

      /** file: ==> check current file position and length of file **/

      case NI_FILE_TYPE:{
         long f_len , f_pos ;

         if( ns->fp == NULL                ||
             ns->io_mode == NI_OUTPUT_MODE   ) return -1 ; /* never? */

         f_len = ns->fsize ;               /* length of file      */
         if( f_len < 0 ) return -1 ;       /* file not found (?)  */

         f_pos = ftell( ns->fp ) ;         /* where are we now?   */
         if( f_pos < 0 ) return -1 ;       /* should never happen */

         return (f_pos < f_len) ? 1 : -1 ; /* is good or bad, but */
                                           /* never just neutral  */
      }

      /** str: ==> check current buffer position **/

      case NI_REMOTE_TYPE:
      case NI_STRING_TYPE:{
         if( ns->io_mode == NI_OUTPUT_MODE ) return -1 ; /* never? */

         return (ns->npos < ns->nbuf) ? 1 : -1 ;  /* is data left? */
      }
   }

   return -1 ;  /* should never happen */
}

/*---------------------------------------------------------------------------*/
/*!  Check if the NI_stream is ready to have data written into it.

  If not, the routine will wait up to msec milliseconds for writing to
  be allowable.  If msec < 0, this routine will wait nearly forever.
  The return value is 1 if data can be sent, 0 if not;
  -1 will be returned if some unrecoverable error is detected:
    - tcp: the socket closed down at the other end
    - file: this should never happen, unless you try to write to
            a readonly NI_stream
-----------------------------------------------------------------------------*/

int NI_stream_writecheck( NI_stream_type *ns , int msec )
{
   int ii ;

   if( !NI_stream_writeable(ns) ) return -1 ;

   switch( ns->type ){

#ifndef DONT_USE_SHM
      case NI_SHM_TYPE:
        ii = SHM_writecheck( ns->shmioc , msec ) ;
        if( ii > 0 ) ii = 1 ;
        return ii ;
#endif

      /** tcp: ==> uses the Unix "select" mechanism **/

      case NI_TCP_TYPE:
        if( ns->bad ){                         /* not marked as good */
          ii = NI_stream_goodcheck(ns,0) ;     /* check if has become good */
          if( ii == -1 ) return -1 ;           /* some error when checking */
          if( ii == 0  ){                      /* not good yet, */
            ii = NI_stream_goodcheck(ns,msec); /* so wait for it to get good */
            if( ii != 1 ) return ii ;          /* if still not good, exit */
          }
        }
                                               /* socket is good, so */
        return tcp_writecheck(ns->sd,msec) ;   /* check if we can write bytes */

      /** fd: ==> use select, as in tcp: **/

      case NI_FD_TYPE:
        return tcp_writecheck( fileno(ns->fp) , msec ) ;

      /** file: ==> if the file was opened in write mode **/

      case NI_FILE_TYPE:
        return ( (ns->fp != NULL && ns->io_mode == NI_OUTPUT_MODE) ? 1
                                                                   : -1 ) ;

      /** str: ==> if the string was opened in write mode **/

      case NI_STRING_TYPE:
        return ( (ns->io_mode == NI_OUTPUT_MODE) ? 1
                                                 : -1 ) ;
      /** http: or ftp: **/

      case NI_REMOTE_TYPE:   /* can't write to remote files */
        return -1 ;
   }

   return -1 ;  /* should never be reached */
}

/*----------------------------------------------------------------------------*/
/*! Send a string (without the NUL byte) down the NI_stream. [15 Oct 2002]
------------------------------------------------------------------------------*/

int NI_stream_writestring( NI_stream_type *ns , char *str )
{
   if( str == NULL ) return -1 ;
   return NI_stream_write( ns , str , strlen(str) ) ;
}

/*----------------------------------------------------------------------------*/
/*!  Send nbytes of data from buffer down the NI_stream.

  Return value is the number of bytes actually sent, or is -1 if some error
  occurs (which means that the NI_stream is bad).  If 0 is returned, this
  means you tried to write to something that is temporarily unavailable.

  - tcp: We use blocking sends, so that all the data should be sent properly
          unless the connection to the other end fails for some reason
          (e.g., the planet explodes in a fiery cataclysm of annihilation).
  - shm: We also block until everything can be written, even if it requires
          filling the shared memory buffer many times and waiting for the
          reading process to empty it many times.
  - file: Everything should be written, unless the filesystem fills up.
          If nothing at all gets written, -1 is returned.
  - str: Everything will be written, or the program will crash.
          Do not include the NUL byte at the end of the string in
          the nbytes count.
------------------------------------------------------------------------------*/

int NI_stream_write( NI_stream_type *ns , char *buffer , int nbytes )
{
   int ii , nsent ;

   /** check for reasonable inputs **/

   if( ns     == NULL || ns->bad    ||
       buffer == NULL || nbytes < 0 || ns->bad == MARKED_FOR_DEATH ) return -1;

   if( nbytes == 0 ) return 0 ;  /* that was easy */

#ifdef NIML_DEBUG
NI_dpr("ENTER NI_stream_write\n") ;
#endif

   if( ns->type != NI_TCP_TYPE ){
     ii = NI_stream_writecheck(ns,66) ; /* check if stream is still OK */
     if( ii < 0 ) return ii ;           /* if not, vamoose the ranch  */
   }

   switch( ns->type ){

#ifndef DONT_USE_SHM
     case NI_SHM_TYPE:
       return SHM_sendall( ns->shmioc , buffer , nbytes ) ;
#endif

     /** tcp: ==> just use send **/

     case NI_TCP_TYPE:

       if( ns->bad ) return 0 ;  /* socket not ready yet */

       /* turn off SIGPIPE signals, which will otherwise be
          raised if we send to a socket when the other end has crashed */

       if( !nosigpipe ){ signal(SIGPIPE,SIG_IGN); nosigpipe = 1; }

#if 0
       /* 03 Mar 2002: wait until we can write fer shur */
       do{ ii=tcp_writecheck(ns->sd,1) ; } while(ii==0) ;
       if( ii < 0 ) return -1 ;
#endif

       errno = 0 ;
       nsent = tcp_send( ns->sd , buffer , nbytes , 0 ) ;
       if( nsent < nbytes || errno != 0 ) PERROR("NI_stream_write(send)") ;
       if( nsent == 0 ){ fprintf(stderr,"tcp send: 0/%d\n",nbytes); nsent=-1; }
       return nsent ;

     /** file: ==> just fwrite **/

     case NI_FD_TYPE:
     case NI_FILE_TYPE:
#ifdef NIML_DEBUG
NI_dpr("  file: about to write %d bytes\n",nbytes) ;
#endif
       nsent = fwrite( buffer , 1 , nbytes , ns->fp ) ;
       if( nsent < nbytes ) PERROR("NI_stream_write(fwrite)") ;
#ifdef NIML_DEBUG
NI_dpr("  file: actually wrote %d bytes\n",nsent) ;
#endif
       if( nsent == 0 ) nsent = -1 ;
       fflush(ns->fp) ;
       return nsent ;

     /** str: ==> append to buffer in stream struct **/

     case NI_STRING_TYPE:
#ifdef NIML_DEBUG
NI_dpr("NI_stream_write str: input=%s\n",ns->buf) ;
#endif
        ns->buf = NI_realloc( ns->buf , char , ns->bufsize+nbytes ) ;
        memcpy( ns->buf+ns->nbuf , buffer , nbytes ) ;
        ns->nbuf    += nbytes ; ns->buf[ns->nbuf] = '\0' ;
        ns->bufsize += nbytes ;
#ifdef NIML_DEBUG
NI_dpr("NI_stream_write str: output=%s\n",ns->buf) ;
#endif
        return nbytes ;

     /** ftp: or http: ==> can't write! */

     case NI_REMOTE_TYPE:
        return -1 ;
   }

   return -1 ;  /* should not be reached */
}

/*-------------------------------------------------------------------------*/
/*!  Read up to nbytes of data from the NI_stream, into buffer.

   Returns the number of bytes actually read.  For both the case of
   sockets and files, this may be less than nbytes (may even be 0).
   If an error occurs and no data is read, -1 is returned.

   For tcp: streams, if no data is available, this function will
   wait until something can be read.  If this behavior is undesirable,
   then you should use NI_stream_readcheck() before calling this function
   in order to see if any data is available.

   For shm: streams, will return immediately if no data is available.

   For file: streams, this function simply tries to read from the file.
   Whether or not it succeeds, it will return immediately. It should
   never return -1; if it returns 0, this means end-of-file.
---------------------------------------------------------------------------*/

int NI_stream_read( NI_stream_type *ns , char *buffer , int nbytes )
{
   int ii ;

   /** check for reasonable inputs **/

   if( ns == NULL || ns->bad || buffer == NULL || nbytes < 0 ) return -1 ;

   if( nbytes == 0 ) return 0 ;

#ifdef NIML_DEBUG
NI_dpr("ENTER NI_stream_read\n") ;
#endif

   switch( ns->type ){

#ifndef DONT_USE_SHM
     case NI_SHM_TYPE:
       return SHM_recv( ns->shmioc , buffer , nbytes ) ;
#endif

     /** tcp: just use recv **/

     case NI_TCP_TYPE:
       ii = NI_stream_goodcheck(ns,1) ; if( ii != 1 ) return ii ;
#if 0
       /* wait 'till we can read fer shur */
       do{ ii=tcp_readcheck(ns->sd,1); } while( ii==0 ) ;
       if( ii < 0 ) return -1 ;
#endif
       errno = 0 ;
       ii = tcp_recv( ns->sd , buffer , nbytes , 0 ) ;
       if( ii == -1 || errno != 0 ) PERROR("NI_stream_read(recv)") ;
#ifdef NIML_DEBUG
NI_dpr("  tcp: got %d/%d bytes ***\n",ii,nbytes) ;
#endif
       return ii ;

     /** file: just use fread **/

     case NI_FD_TYPE:
     case NI_FILE_TYPE:
       if( ns->fp == NULL || ns->io_mode == NI_OUTPUT_MODE ) return -1 ;
       ii = fread( buffer , 1 , nbytes , ns->fp ) ;
       return ii ;

     /** str: copy bytes out of the buffer string **/

     case NI_REMOTE_TYPE:
     case NI_STRING_TYPE:
       if( ns->io_mode == NI_OUTPUT_MODE ) return -1 ; /* bad stream */
       ii = ns->nbuf - ns->npos ;                      /* how much is left */
       if( ii <= 0                       ) return -1 ; /* no data left */
       if( ii > nbytes ) ii = nbytes ;                 /* amount to copy */
       memcpy( buffer , ns->buf+ns->npos , ii ) ;      /* copy it */
       ns->npos += ii ;                                /* advance position */
       return ii ;
   }

   return -1 ;  /* should not be reached */
}

/*-----------------------------------------------------------------------*/
/*! Try to fill up the stream's input buffer.
    Don't call this function until NI_stream_goodcheck() is 1!

  - minread = Minimum number of bytes to read.
              Will wait until we get at least this many,
              until the stream is bad or the buffer is full.
              If minread=0, then may read nothing (but will try).

  - msec    = Maximum amount of time to wait to satisfy minread,
              in milliseconds.  If msec<0, will wait nearly forever.
              If msec=0, will return after 1st read attempt, even
              if nothing was obtained.

    Returns number of bytes read (-1 if input stream goes bad before
    any data is read).  If the input stream goes bad AFTER some data
    is read, there is no indication of that (until the next time
    you call this, of course).
-------------------------------------------------------------------------*/

int NI_stream_fillbuf( NI_stream_type *ns, int minread, int msec )
{
   int nn , ii=0 , ntot=0 , ngood=0 , mwait=0 ;
   int start_msec = NI_clock_time() ;

   if( NI_stream_goodcheck(ns,0) < 0 ) return -1 ;   /* bad input */

   if( ns->type == NI_STRING_TYPE ) return -1 ;      /* goofy input */
   if( ns->type == NI_REMOTE_TYPE ) return -1 ;      /* goofy input */

   if( ns->nbuf >= ns->bufsize ) return 0 ; /* buffer already full */

   if( msec < 0 ) msec = 999999999 ;        /* a long time (11+ days) */

   /* read loop */

   while(1){

      ngood = NI_stream_readcheck(ns,mwait); /* check if data can be read */

      if( ngood < 0 ) break ;                /* data stream gone bad, so exit */

      ii = 0 ;
      if( ngood > 0 ){                       /* we can read ==> */
                                             /* try to fill buffer completely */

         ii = NI_stream_read( ns, ns->buf+ns->nbuf, ns->bufsize-ns->nbuf ) ;

         if( ii > 0 ){                 /* we got data! */
            ns->nbuf += ii ;           /* buffer is now longer */
            ntot     += ii ;           /* total bytes read here so far */

            /* if buffer is full,
               or we have all the data that was asked for, then exit */

            if( ns->nbuf >= ns->bufsize || ntot >= minread ) break ;

         } else if( ii < 0 ){          /* stream suddenly died horribly? */
            ngood = -1 ; break ;
         }
      }

      /* if we don't require data, then exit no matter what our status is */

      if( minread <= 0 ) break ;

      /* if the max time has elapsed, then exit */

      if( NI_clock_time()-start_msec >= msec ) break ;

      /* otherwise, sleep a little bit before trying again */

      if( mwait < 9 && ii < 4096 ) mwait++ ;
   }

   /* if didn't get any data, and
      if the NI_stream was bad, return -1 as a flag of displeasure */

   if( ntot == 0 && ngood < 0 ) ntot = -1 ;

   return ntot ;  /* otherwise, return # of bytes read (may be 0) */
}

/*-----------------------------------------------------------------------*/
/*! Buffered read from a NI_stream.  Unlike NI_stream_read(), will try
    to read all nbytes of data, waiting if necessary.  Also works through
    the internal buffer, rather than directly to the stream.

    Return value is number of bytes read.  May be less than nbytes if
    the stream closed (or was used up) before nbytes of data was read.
    Will return -1 if something is rotten.
-------------------------------------------------------------------------*/

int NI_stream_readbuf( NI_stream_type *ns , char *buffer , int nbytes )
{
   int ii , jj , bs , nout=0 ;

   /** check for reasonable inputs **/

   if( nbytes  == 0                        ) return  0; /* that was real easy */
   if( buffer  == NULL || nbytes      <  0 ) return -1; /* stupid caller */
   if( ns->buf == NULL || ns->bufsize == 0 ) return -1; /* shouldn't happen */
   if( !NI_stream_readable(ns) )             return -1; /* stupid stream */

   /* see how many unused bytes are already in the input buffer */

   ii = ns->nbuf - ns->npos ;

   if( ii >= nbytes ){    /* have all the data we need already */
     memcpy( buffer , ns->buf + ns->npos , nbytes ) ;
     ns->npos += nbytes ;
     if( ns->npos == ns->nbuf ) ns->nbuf = ns->npos = 0 ;  /* buffer used up */
     return nbytes ;
   }

   /* copy what data we already have, if any */

   if( ii > 0 ){
     memcpy( buffer , ns->buf + ns->npos , ii ) ; nout = ii ;
   }
   ns->nbuf = ns->npos = 0 ;                               /* buffer used up */

   /* input streams with fixed length buffers ==> can't do no more */

   if( ns->type == NI_REMOTE_TYPE || ns->type == NI_STRING_TYPE )
     return (nout > 0) ? nout : -1 ;

   /* otherwise, fill the buffer and try again */

   bs = ns->bufsize ;

   while( nout < nbytes ){

     jj = MIN( bs , nbytes-nout ) ;         /* how much to try to read */
     ii = NI_stream_fillbuf( ns,jj,1666 ) ; /* read into stream buffer */

     if( ii > 0 ){                          /* got something */
       ii = ns->nbuf ;                      /* how much now in buffer */
       if( ii > nbytes-nout ) ii = nbytes-nout ;
       memcpy( buffer+nout , ns->buf , ii ) ; nout += ii ;
       ns->npos += ii ; NI_reset_buffer( ns ) ;
     } else {                               /* got nothing */
       break ;                              /* so quit */
     }
   }

   if( nout == 0 && ii < 0 ) nout = -1 ;    /* no data and an I/O error */
   return nout ;
}

/*-----------------------------------------------------------------------*/
/*! Buffered read from a NI_stream, like NI_stream_readbuf, but also:
      - Converts from Base64 to binary 'on the fly'.
      - Will stop at a '<'.

    Return value is number of bytes put into the buffer.  May be less than
    nbytes if the stream closed (or was used up, or hit a '<') before
    nbytes of data was read.  Will return -1 if something is rotten.
-------------------------------------------------------------------------*/

int NI_stream_readbuf64( NI_stream_type *ns , char *buffer , int nbytes )
{
   int ii , jj , bs , nout=0 ;
   byte a=0,b=0,c=0 , w,x,y,z ;
   byte ag,bg,cg ;
   int num_reread , bpos ;

   /** check for reasonable inputs **/

   if( nbytes  == 0                        ) return  0; /* that was real easy */
   if( buffer  == NULL || nbytes      <  0 ) return -1; /* stupid caller */
   if( ns->buf == NULL || ns->bufsize == 0 ) return -1; /* shouldn't happen */
   if( !NI_stream_readable(ns) )             return -1; /* stupid stream */

   /* are there decoded leftover bytes from a previous call?
      if so, use them up first */

   if( ns->b64_numleft > 0 ){

     if( ns->b64_numleft >= nbytes ){    /* have enough leftovers for all! */
       memcpy( buffer , ns->b64_left , nbytes ) ;
       ns->b64_numleft -= nbytes ;
       if( ns->b64_numleft > 0 )   /* must shift remaining leftovers down */
         memmove( ns->b64_left , ns->b64_left + nbytes , ns->b64_numleft ) ;
       return nbytes ;                                 /* done done done! */
     }

     /* if here, have a few bytes leftover, but not enough */

     memcpy( buffer , ns->b64_left , ns->b64_numleft ) ;
     nout            = ns->b64_numleft ;   /* how many so far */
     ns->b64_numleft = 0 ;                 /* have none left now */
   }

   /* now need to decode some bytes from the input stream;
      this is done 4 input bytes at a time,
      which are decoded to 3 output bytes                   */

   load_decode_table() ;   /* prepare for Base64 decoding */

   /** loopback point for reading more data from stream into internal buffer **/

   num_reread = 0 ;
 Base64Reread:
   ag = bg = cg = 0 ;
   num_reread++ ; if( num_reread > 5 ) goto Base64Done ; /* done waiting! */

   /* read more data into buffer, if needed */

   if( num_reread > 1 || ns->nbuf - ns->npos < 4 ){
     NI_reset_buffer(ns) ;          /* discard used up data => ns->npos==0 */
     ii = 5 - ns->nbuf ; if( ii <= 1 ) ii = 2 ;
     ii = NI_stream_fillbuf( ns , ii , 1666 ) ;
     if( ns->nbuf < 4 ) goto Base64Done ;     /* can't get no satisfaction! */
   }

   /*** Copy valid Base64 bytes out of buffer (skipping others),
        converting them to binary as we get full quads,
        putting the results into buffer.

        Exit loop if we hit a '<' character (end of NIML element),
        or hit an '=' character (end of Base64 data stream).

        Jump back to Base64Reread (above) if we run out of data in the
        buffer before we fulfill the caller's demand for nbytes of output. ***/

   while( 1 ){
     ag = bg = cg = 0 ;
     bpos = ns->npos ;    /* scan forward in buffer using bpos */

     /* get next valid Base64 character into w;
        skip whitespaces and other non-Base64 stuff;
        if we hit the end token '<' first, quit;
        if we hit the end of the buffer first, need more data */

     w = ns->buf[bpos++] ;
     while( !B64_goodchar(w) && w != '<' && bpos < ns->nbuf )
       w = ns->buf[bpos++] ;
     ns->npos = bpos-1 ;  /** if we have to reread, will start here, at w **/
     if( w == '<' ) goto Base64Done;
     if( bpos == ns->nbuf ) goto Base64Reread;  /* not enuf data yet */

     /* repeat to fill x */

     x = ns->buf[bpos++] ;
     while( !B64_goodchar(x) && x != '<' && bpos < ns->nbuf )
       x = ns->buf[bpos++] ;
     if( x == '<' ){ ns->npos = bpos-1; goto Base64Done; }
     if( bpos == ns->nbuf ) goto Base64Reread;

     /* repeat to fill y */

     y = ns->buf[bpos++] ;
     while( !B64_goodchar(y) && y != '<' && bpos < ns->nbuf )
       y = ns->buf[bpos++] ;
     if( y == '<' ){ ns->npos = bpos-1; goto Base64Done; }
     if( bpos == ns->nbuf ) goto Base64Reread;

     /* repeat to fill z */

     z = ns->buf[bpos++] ;
     while( !B64_goodchar(z) && z != '<' && bpos < ns->nbuf )
       z = ns->buf[bpos++] ;
     if( z == '<' ){ ns->npos = bpos-1; goto Base64Done; }

     /* at this point, have w,x,y,z to decode */

     ns->npos = bpos ;  /* scan continues at next place in buffer */

     B64_decode4(w,x,y,z,a,b,c) ;  /* decode 4 bytes into 3 */

     if( z == '=' ){                        /* got to the end of Base64? */
       int nn = B64_decode_count(w,x,y,z) ; /* see how many bytes to save */
       ag = (nn > 0) ;  /* a byte is good? */
       bg = (nn > 1) ;  /* b byte is good? */
       cg = 0        ;  /* c byte is bad!! */

       /* save good bytes into output buffer;
          if we reach end of the required number of bytes, we're done */

       if( ag ){ buffer[nout++]=a; ag=0; if(nout >= nbytes) goto Base64Done; }
       if( bg ){ buffer[nout++]=b; bg=0; if(nout >= nbytes) goto Base64Done; }
       goto Base64Done ;
     }

     /* not at the end of Base64 =>
        save bytes, and skip out if we fill up the output array */

     ag = bg = cg = 1 ;  /* all 3 bytes are good */
     buffer[nout++]=a; ag=0; if(nout >= nbytes) goto Base64Done;
     buffer[nout++]=b; bg=0; if(nout >= nbytes) goto Base64Done;
     buffer[nout++]=c; cg=0; if(nout >= nbytes) goto Base64Done;

     /* now, loop back to decode the next 4 bytes;
        BUT, if we don't have at least 4 bytes in the input buffer,
             must do a re-read first!                               */

     num_reread = 1 ; /* finished at least 1 quad ==> reset penalty clock */
     if( ns->nbuf - ns->npos < 4 ) goto Base64Reread ;

   } /* end of while(1) loop */

  /* At this point:
      have finished reading and decoding,
      have nout bytes in output buffer,
      and might have some good bytes left that need to be saved */

Base64Done:
   ns->b64_numleft = 0 ;
   if( ag ) ns->b64_left[ ns->b64_numleft++ ] = a ;
   if( bg ) ns->b64_left[ ns->b64_numleft++ ] = b ;
   if( cg ) ns->b64_left[ ns->b64_numleft++ ] = c ;

   return nout ;
}
