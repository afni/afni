#ifndef _NIML_HEADER_FILE_
#define _NIML_HEADER_FILE_

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <fcntl.h>
#include <sys/times.h>
#include <limits.h>

/*-----------------------------------------------------------*/

/* This is suppose to be defined in stddef.h, but
   apparently it isn't on all systems for some reason. */

#ifndef offsetof
# define offsetof(TYPE,MEMBER) ((size_t) &((TYPE *)0)->MEMBER)
#endif

/*-----------------------------------------------------------*/

#ifndef TYPEDEF_byte
#define TYPEDEF_byte
typedef unsigned char byte ;
#endif

#ifndef TYPEDEF_rgb
#define TYPEDEF_rgb
typedef struct { byte r,g,b ; } rgb ;
#endif

#ifndef TYPEDEF_rgba
#define TYPEDEF_rgba
typedef struct { byte r,g,b,a ; } rgba ;
#endif

#ifndef TYPEDEF_complex
#define TYPEDEF_complex
typedef struct { float r,i ; } complex ;
#endif

/*-----------------------------------------*/

/* Macros for data type codes. */

#define NI_BYTE        0
#define NI_SHORT       1
#define NI_INT         2
#define NI_FLOAT32     3
#define NI_FLOAT       NI_FLOAT32
#define NI_FLOAT64     4
#define NI_DOUBLE      NI_FLOAT64
#define NI_COMPLEX64   5
#define NI_COMPLEX     NI_COMPLEX64
#define NI_RGB         6
#define NI_RGBA        7
#define NI_STRING      8
#define NI_LINE        9

/*! One more than the last NI_ data type code defined above. */

#define NI_NUM_TYPES   10

/*! Valid data type character codes. */

#define IS_DATUM_CHAR(c) ( (c) == 'b' || (c) == 's' || (c) == 'i' ||  \
                           (c) == 'f' || (c) == 'd' || (c) == 'c' ||  \
                           (c) == 'r' || (c) == 'S' || (c) == 'L' ||  \
                           (c) == 'R'                               )

/*-----------------------------------------*/

#define NI_ELEMENT_TYPE  17
#define NI_GROUP_TYPE    18

/*! A data element. */

#ifndef TYPEDEF_NI_element
#define TYPEDEF_NI_element
typedef struct {
   int    type ;       /*!< What type of struct is this? */
   char  *name ;       /*!< Name of element. */
   int    attr_num ;   /*!< Number of attributes. */
   char **attr_lhs ;   /*!< Left-hand-sides of attributes. */
   char **attr_rhs ;   /*!< Right-hand-sides of attributes. */
   int    vec_num ;    /*!< Number of vectors (may be 0). */
   int    vec_len ;    /*!< Length of each vector. */
   int    vec_filled ; /*!< Length that each one was filled up. */
   int   *vec_typ ;    /*!< Type code for each vector. */
   void **vec ;        /*!< Pointer to each vector. */

   int    vec_rank ;        /*!< Number of dimensions, from ni_dimen. */
   int   *vec_axis_len ;    /*!< Array of dimensions, from ni_dimen. */
   float *vec_axis_delta ;  /*!< Array of step sizes, from ni_delta. */
   float *vec_axis_origin ; /*!< Array of origins, from ni_origin. */
   char **vec_axis_unit ;   /*!< Array of units, from ni_units. */
   char **vec_axis_label ;  /*!< Array of labels, from ni_axes. */

   int  rowmap_num ;   /*!< Is >0 for use with NI_add_row(). */
   int  rowmap_cod ;
   int *rowmap_off ;   /*!< Array of offsets into struct. */
   int *rowmap_siz ;   /*!< Array of sizes of components. */
} NI_element ;
#endif

/*! A bunch of elements. */

#ifndef TYPEDEF_NI_group
#define TYPEDEF_NI_group
typedef struct {
   int    type ;       /*!< What type of struct is this? */
   int    attr_num ;   /*!< Number of attributes. */
   char **attr_lhs ;   /*!< Left-hand-sides of attributes. */
   char **attr_rhs ;   /*!< Right-hand-sides of attributes. */

   int    part_num ;   /*!< Number of parts within this group. */
   int   *part_typ ;   /*!< Type of each part (element or group). */
   void **part ;       /*!< Pointer to each part. */

   char  *name ;       /*!< Name (default="ni_group") - 03 Jun 2002 */
} NI_group ;
#endif

/*-----------------------------------------------------------------
  Stuff for shared memory transport between processes
-------------------------------------------------------------------*/

#ifdef CYGWIN
# define DONT_USE_SHM
#endif

#ifndef DONT_USE_SHM
# include <sys/ipc.h>
# include <sys/shm.h>

# define SHM_WAIT_CREATE   9
# define SHM_WAIT_ACCEPT  10
# define SHM_IS_DEAD      99

# define SHM_CREATOR      33
# define SHM_ACCEPTOR     44

# define SHM_DEFAULT_SIZE 196689

# define SHM_HSIZE        128  /* header size in bytes    */
# define SHM_SIZE1        0    /* size1   = shmhead[this] */
# define SHM_BSTART1      1    /* bstart1 = shmhead[this] */
# define SHM_BEND1        2    /* bend1   = shmhead[this] */
# define SHM_SIZE2        3
# define SHM_BSTART2      4
# define SHM_BEND2        5

 /**
   The shm segment is split into a 128 byte header and 2 buffers:
     buf1 is written into by the "w" creator and read by the "r" acceptor;
     buf2 is written into by the "r" acceptor and read by the "w" creator.
   Each of these is a circular buffer, as described below.
   The header currently contains 6 ints.  For buf1:
     size1   = size of buf1 in bytes (fixed by creator)
     bstart1 = offset into buf1 where good data starts (changed by acceptor)
     bend1   = offset into buf1 where good data ends (changed by creator)
   For buf2, a similar triple is set (mutatis mutandum).
 **/

 typedef struct {
   int id ;          /* shmid */
   int bad ;         /* tells whether I/O is OK for this yet */
   int whoami ;      /* SHM_CREATOR or SHM_ACCEPTOR? */

   char name[128] ;  /* keystring */

   char * shmbuf ;   /* actual shm buffer */
   int  * shmhead ;  /* buffer as ints */

   int bufsize1 ;    /* size of 1st internal buffer */
   char * buf1 ;     /* 1st internal buffer [after header] */
   int  * bstart1 ;
   int  * bend1 ;

   int bufsize2 ;    /* size of 2nd internal buffer */
   char * buf2 ;     /* 2nd internal buffer [after buf1] */
   int  * bstart2 ;
   int  * bend2 ;
 } SHMioc ;

#else  /* DONT_USE_SHM */

# define SHMioc void  /* dummy definition */

#endif /* DONT_USE_SHM */
/*-----------------------------------------------------------------*/

/*! Size of NI_stream buffer. */

#define NI_BUFSIZE (16*1024)

/*! Data needed to process input stream. */

#ifndef TYPEDEF_NI_stream_type
#define TYPEDEF_NI_stream_type
typedef struct {
   int type ;        /*!< NI_TCP_TYPE or NI_FILE_TYPE */
   int bad ;         /*!< Tells whether I/O is OK for this yet */

   int port ;        /*!< TCP only: port number */
   int sd ;          /*!< TCP only: socket descriptor */

   FILE *fp ;        /*!< FILE only: pointer to open file */
   int fsize ;       /*!< FILE only: length of file for input */

   char name[256] ;  /*!< Hostname or filename */

   int io_mode ;     /*!< Input or output? */
   int data_mode ;   /*!< Text, binary, or base64? */

   int bin_thresh ;  /*!< Threshold size for binary write. */

   int nbuf ;              /*!< Number of bytes left in buf. */
   int npos ;              /*!< Index of next unscanned byte in buf. */
   int bufsize ;           /*!< Length of buf array. */
   char *buf ;             /*!< I/O buffer (may be NULL). */

   SHMioc *shmioc ;        /*!< for NI_SHM_TYPE only */
} NI_stream_type ;
#endif

/*! Opaque type for the C API. */

#ifndef TYPEDEF_NI_stream
#define TYPEDEF_NI_stream
typedef NI_stream_type *NI_stream ;
#endif

#define NI_TCP_TYPE    1  /* tcp: */
#define NI_FILE_TYPE   2  /* file: */
#define NI_STRING_TYPE 3  /* str: */
#define NI_REMOTE_TYPE 4  /* http: or ftp: */
#define NI_FD_TYPE     5  /* fd: */
#define NI_SHM_TYPE    6  /* shm: */

#define TCP_WAIT_ACCEPT   7
#define TCP_WAIT_CONNECT  8

/* I/O Modes for a NI_stream_type: input or output. */

#define NI_INPUT_MODE  0
#define NI_OUTPUT_MODE 1

/* Data modes for a NI_stream_type: text, binary, base64. */

#define NI_TEXT_MODE    0
#define NI_BINARY_MODE  1
#define NI_BASE64_MODE  2

#define NI_LSB_FIRST    1
#define NI_MSB_FIRST    2

/*-------------- prototypes ---------------*/

extern void * NI_malloc( size_t ) ;
extern void   NI_free( void * ) ;
extern void * NI_realloc( void *, size_t ) ;
extern char * NI_strncpy( char *, const char *, size_t ) ;
extern long   NI_filesize( char * ) ;
extern int    NI_clock_time(void) ;
extern int    NI_byteorder(void) ;
extern void   NI_swap2( int, void * ) ;
extern void   NI_swap4( int, void * ) ;
extern void   NI_swap8( int, void * ) ;

extern char * NI_mktemp( char * ) ;  /* 21 Aug 2002 */

extern char * NI_type_name( int ) ;
extern int    NI_type_size( int ) ;

extern int NI_element_rowsize( NI_element * ) ;
extern int NI_element_allsize( NI_element * ) ;

extern void NI_free_element( void * ) ;
extern int  NI_element_type( void * ) ;

extern NI_element * NI_new_data_element( char *, int ) ;
extern void NI_add_column( NI_element *, int, void * ) ;
extern void NI_set_attribute( void *, char *, char * ) ;
extern char * NI_get_attribute( void *, char * ) ;

extern NI_group * NI_new_group_element(void) ;
extern void NI_add_to_group( NI_group *, void * ) ;
extern void NI_rename_group( NI_group *, char * ) ;  /* 03 Jun 2002 */

extern void NI_swap_vector( int, int, void * ) ;

#include <stdarg.h>
extern void NI_define_rowmap_AR( NI_element *, int,int *,int *) ;
extern void NI_define_rowmap_VA( NI_element *, ... ) ;

extern void NI_add_row( NI_element *, void * ) ;
extern void NI_get_row( NI_element *, int, void * ) ;

extern void NI_add_many_rows( NI_element *, int,int, void * ) ;

/** I/O functions **/

extern NI_stream NI_stream_open( char *, char * ) ;
extern int NI_stream_goodcheck( NI_stream_type *, int ) ;
extern void NI_stream_close( NI_stream_type * ) ;
extern int NI_stream_readcheck( NI_stream_type *, int  ) ;
extern int NI_stream_writecheck( NI_stream_type *, int  ) ;
extern int NI_stream_write( NI_stream_type *, char *, int ) ;
extern int NI_stream_read( NI_stream_type *, char *, int ) ;
extern void NI_binary_threshold( NI_stream_type *, int ) ;
extern void NI_sleep( int ) ;
extern char * NI_stream_getbuf( NI_stream_type * ) ;
extern void   NI_stream_clearbuf( NI_stream_type * ) ;
extern void   NI_stream_setbuf( NI_stream_type *, char * ) ;
extern char * NI_stream_name( NI_stream_type * ) ;
extern int NI_stream_readable( NI_stream_type * ) ;
extern int NI_stream_writeable( NI_stream_type * ) ;
extern int NI_stream_hasinput( NI_stream_type * , int ) ;

extern void NI_binary_threshold( NI_stream_type *, int ) ;

extern void * NI_read_element( NI_stream_type *, int ) ;
extern int    NI_write_element( NI_stream_type *, void *, int ) ;

/* prototypes for Web data fetchers */

extern int  NI_read_URL_tmpdir( char *url, char **tname ) ;
extern int  NI_read_URL       ( char *url, char **data  ) ;
extern void NI_set_URL_ftp_ident( char *name, char *pwd ) ;

/* prototypes for Base64 and MD5 functions */

extern void   B64_set_crlf( int nn ) ;
extern void   B64_set_linelen( int ll ) ;
extern void   B64_to_binary( int nb64, byte * b64, int * nbin, byte ** bin ) ;
extern void   B64_to_base64( int nbin, byte * bin, int * nb64, byte ** b64 ) ;

extern char * MD5_static_array( int n, char * bytes ) ;
extern char * MD5_malloc_array( int n, char * bytes ) ;
extern char * MD5_static_string( char * string ) ;
extern char * MD5_malloc_string( char * string ) ;
extern char * MD5_static_file(char * filename) ;
extern char * MD5_malloc_file(char * filename) ;

extern char * MD5_B64_array( int n, char * bytes ) ;
extern char * MD5_B64_string( char * string ) ;
extern char * MD5_B64_file(char * filename) ;

extern char * UNIQ_idcode(void) ;
extern void   UNIQ_idcode_fill( char * ) ;
extern char * UNIQ_hashcode( char * ) ;

extern char * UUID_hashcode( char * ) ;   /* 20 Aug 2002 */
extern char * UUID_idcode(void) ;

/* trusted host manipulation */

extern char * NI_hostname_to_inet( char *host ) ;
extern void   NI_add_trusted_host( char *hostname ) ;
extern int    NI_trust_host( char *hostid ) ;

/*! Close a NI_stream, and set the pointer to NULL. */

#define NI_STREAM_CLOSE(nn) do{ NI_stream_close(nn); (nn)=NULL; } while(0)

#endif /* _NIML_HEADER_FILE */
