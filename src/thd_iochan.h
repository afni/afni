#ifndef _MCW_IOCHAN_HEADER_
#define _MCW_IOCHAN_HEADER_

/***** Header for TCP/IP and shared memory "I/O channels" *****/

#ifdef SPARKY
#undef _POSIX_SOURCE
#endif

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/times.h>
#include <limits.h>

#ifndef MIN
#  define MIN(a,b) (((a)<(b)) ? (a) : (b))
#endif
#ifndef MAX
#  define MAX(a,b) (((a)>(b)) ? (a) : (b))
#endif

#define NEXTDMS(dm) MIN(1.1*(dm)+1.01 , 1000.0)

#define BCOPY memcpy

#define TCP_IOCHAN  1
#define SHM_IOCHAN  2

#define SHM_BUFSIZ    (2*1024*1024)

#define TCP_WAIT_ACCEPT   7
#define TCP_WAIT_CONNECT  8
#define SHM_WAIT_CREATE   9
#define SHM_WAIT_ACCEPT  10

#define CREATOR          33
#define ACCEPTOR         44

#define SHMIOC_READ(ic) \
 ( ((ic)->whoami == CREATOR && (ic)->ioc2 != NULL) ? (ic)->ioc2 : (ic) )

#define SHMIOC_WRITE(ic) \
 ( ((ic)->whoami == ACCEPTOR && (ic)->ioc2 != NULL) ? (ic)->ioc2 : (ic) )

#define IOC_BAD(ic) \
 ( ((ic)->ioc2 != NULL) ? MAX( (ic)->bad , (ic)->ioc2->bad ) : (ic)->bad )

#define IOC_BIDIR(ic)            \
 ( ((ic)->type == TCP_IOCHAN) || \
   ((ic)->type == SHM_IOCHAN && (ic)->ioc2 != NULL) )

typedef struct IOCHAN {
   int type ;        /* one of the _IOCHAN values above */
   int id ;          /* socket descriptor or shmid */
   int bad ;         /* tells whether I/O is OK for this yet */
   int port ;        /* TCP only: port number */
   int whoami ;      /* CREATOR or ACCEPTOR? */

   char name[128] ;  /* hostname or keystring */

   /* SHM only */

   int bufsize ;     /* size of internal buffer */
   char * buf ;      /* internal buffer, if any */
   int  * bstart ;
   int  * bend ;
   struct IOCHAN * ioc2 ;

   /* TCP only */

   int sendsize ;    /* max # bytes to send at once */
} IOCHAN ;

/*----- prototypes -----*/

extern int  tcp_readcheck( int , int ) ;
extern int  tcp_writecheck( int , int ) ;
extern int  tcp_connect( char * , int ) ;
extern int  tcp_accept( int , char ** , char ** ) ;
extern int  tcp_listen( int ) ;
extern int  tcp_alivecheck( int ) ;
extern void tcp_set_cutoff( int ) ;

#undef USE_TCP_RECV
#ifdef USE_TCP_RECV
   extern int tcp_recv( int , void * , int , unsigned int ) ;
#else
#  define tcp_recv recv
#endif

extern key_t  string_to_key( char * ) ;
extern int    shm_accept( char * ) ;
extern int    shm_create( char * , int ) ;
extern int    shm_size( int ) ;
extern int    shm_nattach( int ) ;
extern char * shm_attach( int ) ;
extern int    shm_alivecheck( int ) ;

extern void     iochan_sleep( int ) ;
extern IOCHAN * iochan_init( char * , char * ) ;
extern int      iochan_goodcheck( IOCHAN * , int ) ;
extern void     iochan_close( IOCHAN * ) ;
extern void     iochan_set_cutoff( IOCHAN * ) ;
extern int      iochan_writecheck( IOCHAN * , int ) ;
extern int      iochan_readcheck( IOCHAN * , int ) ;
extern int      iochan_clearcheck( IOCHAN * , int ) ;
extern int      iochan_send( IOCHAN * , char * , int ) ;
extern int      iochan_recv( IOCHAN * , char * , int ) ;
extern int      iochan_sendall( IOCHAN * , char * , int ) ;
extern int      iochan_recvall( IOCHAN * , char * , int ) ;
extern int      iochan_ctl( IOCHAN * , int , int ) ;
extern int      iochan_force_clear( IOCHAN * ) ;

/** codes for the "cmd" argument to iochan_ctl **/

#define IOC_TCP_SENDSIZE 501

#define IOCHAN_CLOSE(ioc) ( iochan_close(ioc) , (ioc)=NULL )

extern double COX_clock_time(void) ; /* return total elapsed time in seconds */
extern double COX_cpu_time(void) ;   /* return total cpu time used in seconds */

#define CLEAR_SHM(ic,ms)                             \
  do{ if( (ic) != NULL && (ic)->type == SHM_IOCHAN ) \
         iochan_clearcheck( (ic) , (ms) ) ;          \
    } while(0)

/*---------------------------------------------------------------------------*/

extern IOCHAN * open_URL_hpf( char * host, int port, char * file, int msec ) ;
extern IOCHAN * open_URL( char * url, int msec ) ;
extern int      read_URL( char * url, int msec , char ** data ) ;

#endif
