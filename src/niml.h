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

/*! Characters allowed inside unquoted strings. */

#define IS_STRING_CHAR(c) ( isgraph(c) && !isspace(c) &&  \
                            (c) != '>' && (c) != '/'  &&  \
                            (c) != '='                  )

#define IS_QUOTE_CHAR(c)  ( (c) == '"' || (c) == '\'' )

/*! Characters that can start a string. */

#define IS_START_CHAR(c)  ( IS_STRING_CHAR(c) || IS_QUOTE_CHAR(c) )

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

/*! A data element. */

typedef struct {
   char  *name ;       /*!< Name of element. */
   int    attr_num ;   /*!< Number of attributes. */
   char **attr_lhs ;   /*!< Left-hand-sides of attributes. */
   char **attr_rhs ;   /*!< Right-hand-sides of attributes. */
   int    vec_num ;    /*!< Number of vectors (may be 0). */
   int    vec_len ;    /*!< Length of each vector. */
   int   *vec_typ ;    /*!< Type code for each vector. */
   void **vec ;        /*!< Pointer to each vector. */
} NI_element ;

/*! A bunch of elements. */

typedef struct {
   int    attr_num ;   /*!< Number of attributes. */
   char **attr_lhs ;   /*!< Left-hand-sides of attributes. */
   char **attr_rhs ;   /*!< Right-hand-sides of attributes. */

   int    part_num ;   /*!< Number of parts within this group. */
   int   *part_typ ;   /*!< Type of each part (element or group). */
   void **part ;       /*!< Pointer to each part. */
} NI_group ;

/* Part type codes. */

#define NI_ELEMENT  0
#define NI_GROUP    1

/*! Size of NI_stream buffer. */

#define NI_BUFSIZE (64*1024)

/*! Data needed to process input stream. */

typedef struct {
   int type ;        /*!< NI_TCP_TYPE or NI_FILE_TYPE */
   int bad ;         /*!< Tells whether I/O is OK for this yet */

   int port ;        /*!< TCP only: port number */
   int sd ;          /*!< TCP only: socket descriptor */

   FILE *fp ;        /*!< FILE only: pointer to open file */

   char name[128] ;  /*!< Hostname or filename */

   int io_mode ;     /*!< Input or output? */
   int data_mode ;   /*!< Text, binary, or base64? */

   int nbuf ;              /*!< Number of bytes left in buf. */
   char buf[NI_BUFSIZE] ;  /*!< I/O buffer. */
} NI_stream_type ;

#define NI_TCP_TYPE    1
#define NI_FILE_TYPE   2

#define TCP_WAIT_ACCEPT   7
#define TCP_WAIT_CONNECT  8

/* I/O Modes for a NI_stream_type: input or output. */

#define NI_INPUT_MODE  0
#define NI_OUTPUT_MODE 1

/* Data modes for a NI_stream_type: text, binary, base64. */

#define NI_TEXT_MODE            0
#define NI_BINARY_MSBFIRST_MODE 1
#define NI_BINARY_LSBFIRST_MODE 2
#define NI_BASE64_MSBFIRST_MODE 3
#define NI_BASE64_LSBFIRST_MODE 4

/*! Opaque type for the C API. */

typedef NI_stream_type *NI_stream ;

/*-------------- prototypes ---------------*/

void * NI_malloc( size_t ) ;
void   NI_free( void * ) ;
void * NI_realloc( void * , size_t ) ;

char * NI_strncpy( char * , const char * , size_t ) ;

char * NI_type_name( int ) ;

int NI_read_file  ( void *, char *, int ) ;
int NI_read_socket( void *, char *, int ) ;

int NI_write_file  ( void *, char *, int ) ;
int NI_write_socket( void *, char *, int ) ;

NI_stream NI_open_for_input ( int (*getdata)(void *, char *, int) , void * ) ;
NI_stream NI_open_for_output( int (*putdata)(void *, char *, int) , void * ) ;

void NI_close( NI_stream ) ;

NI_group * NI_get_group( NI_stream ) ;

int NI_put_group  ( NI_stream , NI_group *   ) ;
int NI_put_element( NI_stream , NI_element * ) ;

int NI_readcheck( NI_stream ) ;

void NI_free_group  ( NI_group *   ) ;
void NI_free_element( NI_element * ) ;

NI_stream NI_stream_open( char *name , char *mode ) ;
int NI_stream_goodcheck( NI_stream_type *ns , int msec ) ;
void NI_stream_close( NI_stream_type *ns ) ;
int NI_stream_readcheck( NI_stream_type *ns , int msec ) ;
int NI_stream_writecheck( NI_stream_type *ns , int msec ) ;
int NI_stream_write( NI_stream_type *ns , char *buffer , int nbytes ) ;
int NI_stream_read( NI_stream_type *ns , char *buffer , int nbytes ) ;
void NI_sleep( int msec ) ;

/*! Close a NI_stream, and set the pointer to NULL. */

#define NI_STREAM_CLOSE(nn) do{ NI_stream_close(nn); (nn)=NULL; } while(0)

#endif /* _NIML_HEADER_FILE */
