#ifndef _NIML_HEADER_FILE_
#define _NIML_HEADER_FILE_

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

/*-----------------------------------------*/

/*! Holds strings from the <header and=attributes> */

typedef struct {
   int nattr , empty ;
   char *name ;
   char **lhs , **rhs ;
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

/*! One more than the last NI_ data type code defined above. */

#define NI_NUM_TYPES   9

/*! Valid data type character codes. */

#define IS_DATUM_CHAR(c) ( (c) == 'b' || (c) == 's' || (c) == 'i' ||  \
                           (c) == 'f' || (c) == 'd' || (c) == 'c' ||  \
                           (c) == 'r' || (c) == 'S' || (c) == 'L'    )

/*-----------------------------------------*/

/*! A data element. */

typedef struct {
   char  *name ;
   int    attr_num ;
   char **attr_lhs ;
   char **attr_rhs ;
   int    vec_num ;
   int    vec_len ;
   int   *vec_type ;
   void **vec ;
} NI_element ;

/*! A bunch of elements. */

typedef struct {
   int    attr_num ;
   char **attr_lhs ;
   char **attr_rhs ;

   int    part_num ;
   int   *part_typ ;
   void **part ;
} NI_group ;

/* Part type codes. */

#define NI_ELEMENT  0
#define NI_GROUP    1

/*! Size of NI_stream buffer. */

#define NI_BUFSIZE (64*1024)

/*! Data needed to process input stream. */

typedef struct {
   void *user_handle ;
   int (*getdata)(void *, char *, int) ;

   int mode ;
   int nbuf ;
   char buf[NI_BUFSIZE] ;
} NI_stream_type ;

/* Modes for a NI_stream_type: input or output. */

#define NI_INPUT_MODE  0
#define NI_OUTPUT_MODE 1

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

#endif /* _NIML_HEADER_FILE */
