#ifndef _NIML_HEADER_FILE_
#define _NIML_HEADER_FILE_

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

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

#endif /* _NIML_HEADER_FILE */
