#ifndef _NIML_HEADER_FILE_
#define _NIML_HEADER_FILE_

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

/*-----------------------------------------*/

typedef unsigned char byte ;

typedef struct { byte r,g,b ; } rgb ;

typedef struct { byte r,g,b,a ; } rgba ;

typedef struct { float r,i ; } complex ;

/*-----------------------------------------*/

/*! Holds strings from the <header and=attributes> */

typedef struct {
   int nattr ;            /*!< Number of attributes. */
   int empty ;            /*!< Did header end in '/>'? */
   char *name ;           /*!< Header name string. */
   char **lhs ;           /*!< Left-hand-sides of attributes. */
   char **rhs ;           /*!< Right-hand-sides of attributes (may be NULL). */
} header_stuff ;

/*! A pair of integers. */

typedef struct { int i,j ; } intpair ;

/*! An array of integers. */

typedef struct { int num; int *ar; } intarray ;

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
#define NI_STRING      7
#define NI_LINE        8
#define NI_RGBA        9

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
} NI_element ;

/*! A bunch of elements. */

typedef struct {
   int    type ;       /*!< What type of struct is this? */
   int    attr_num ;   /*!< Number of attributes. */
   char **attr_lhs ;   /*!< Left-hand-sides of attributes. */
   char **attr_rhs ;   /*!< Right-hand-sides of attributes. */

   int    part_num ;   /*!< Number of parts within this group. */
   int   *part_typ ;   /*!< Type of each part (element or group). */
   void **part ;       /*!< Pointer to each part. */
} NI_group ;

/*! Size of NI_stream buffer. */

#define NI_BUFSIZE (64*1024)

/*! Data needed to process input stream. */

typedef struct {
   int type ;        /*!< NI_TCP_TYPE or NI_FILE_TYPE */
   int bad ;         /*!< Tells whether I/O is OK for this yet */

   int port ;        /*!< TCP only: port number */
   int sd ;          /*!< TCP only: socket descriptor */

   FILE *fp ;        /*!< FILE only: pointer to open file */
   int fsize ;       /*!< FILE only: length of file for input */

   char name[128] ;  /*!< Hostname or filename */

   int io_mode ;     /*!< Input or output? */
   int data_mode ;   /*!< Text, binary, or base64? */

   int bin_thresh ;  /*!< Threshold size for binary write. */

   int nbuf ;              /*!< Number of bytes left in buf. */
   int npos ;              /*!< Index of next unscanned byte in buf. */
   int bufsize ;           /*!< Length of buf array. */
   char *buf ;             /*!< I/O buffer (may be NULL). */
} NI_stream_type ;

#define NI_TCP_TYPE    1
#define NI_FILE_TYPE   2
#define NI_STRING_TYPE 3

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

/*! Opaque type for the C API. */

typedef NI_stream_type *NI_stream ;

/*-------------- prototypes ---------------*/

extern void * NI_malloc( size_t ) ;
extern void   NI_free( void * ) ;
extern void * NI_realloc( void * , size_t ) ;
extern char * NI_strncpy( char * , const char * , size_t ) ;
extern long   NI_filesize( char * ) ;
extern int    NI_clock_time(void) ;
extern int    NI_byteorder(void) ;
extern void   NI_swap2( int , void * ) ;
extern void   NI_swap4( int , void * ) ;
extern void   NI_swap8( int , void * ) ;

extern char * NI_type_name( int ) ;
extern int    NI_type_size( int ) ;

extern int NI_element_rowsize( NI_element * ) ;
extern int NI_element_allsize( NI_element * ) ;

extern void NI_free_element( void * ) ;
extern int  NI_element_type( void * ) ;

extern NI_element * NI_new_data_element( char *, int ) ;
extern void NI_add_vector( NI_element * , int , void * ) ;
extern void NI_set_attribute( void * , char * , char * ) ;

extern NI_group * NI_new_group_element(void) ;
extern void NI_add_to_group( NI_group * , void * ) ;

extern void NI_swap_vector( int , int , void * ) ;

/** I/O functions **/

extern NI_stream NI_stream_open( char * , char * ) ;
extern int NI_stream_goodcheck( NI_stream_type * , int ) ;
extern void NI_stream_close( NI_stream_type * ) ;
extern int NI_stream_readcheck( NI_stream_type * , int  ) ;
extern int NI_stream_writecheck( NI_stream_type * , int  ) ;
extern int NI_stream_write( NI_stream_type * , char * , int ) ;
extern int NI_stream_read( NI_stream_type * , char * , int ) ;
extern void NI_binary_threshold( NI_stream_type * , int ) ;
extern void NI_sleep( int ) ;
extern char * NI_stream_buffer( NI_stream_type * ) ;

extern void NI_binary_threshold( NI_stream_type *, int ) ;

extern void * NI_read_element( NI_stream_type * , int ) ;
extern int    NI_write_element( NI_stream_type * , void * , int ) ;

/*! Close a NI_stream, and set the pointer to NULL. */

#define NI_STREAM_CLOSE(nn) do{ NI_stream_close(nn); (nn)=NULL; } while(0)

#endif /* _NIML_HEADER_FILE */
