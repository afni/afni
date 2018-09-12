#ifndef _NIML_PRIVATE_HEADER_FILE_
#define _NIML_PRIVATE_HEADER_FILE_

#include "niml.h"

#define USE_NEW_IOFUN      /* 13 Feb 2003 */

/****************************************************************************/
/********* NIML private definitions, not needed by user programs ************/
/****************************************************************************/

/************************** Debugging stuff (duh) ***************************/

/*** Debug printout will only be enable if NIML_DEBUG
     is defined here, AND if environment variable NIML_DEBUG
     is also set to a filename (special case: or to the string "stderr"). ***/

#define NIML_DEBUG

#include <stdarg.h>

extern FILE *dfp ;                     /* debug file pointer */
extern void NI_dpr( char * , ... ) ;   /* print debug stuff */

/******************** typedefs used only internally *************************/

/*! Holds strings from the <header and=attributes> */

typedef struct {
   int nattr ;            /*!< Number of attributes. */
   int empty ;            /*!< Did header end in '/>'? */
   char *name ;           /*!< Header name string. */
   char **lhs ;           /*!< Left-hand-sides of attributes. */
   char **rhs ;           /*!< Right-hand-sides of attributes (may be NULL). */
} header_stuff ;

/*! A pair of integers (what did you think it was?). */

#ifndef TYPEDEF_intpair
#define TYPEDEF_intpair
typedef struct { int i,j ; } intpair ;
#endif

/*! An array of integers. */

typedef struct { int num; int *ar; } int_array ;

/****************************************************************************/

extern void NI_stream_close_keep( NI_stream_type * ,int ) ;

extern int string_index( char *targ, int nstr, char *str[] ) ;
extern int unescape_inplace( char *str ) ;
extern char * quotize_string( char *str ) ;
extern char * quotize_string_vector( int num , char **str , char sep ) ;
extern char * quotize_int_vector( int num , int *vec , char sep ) ;
extern char * quotize_float_vector( int num , float *vec , char sep ) ;
extern int NI_is_name( char *str ) ;
extern char * trailname( char *fname , int lev ) ;

/****************************************************************************/

extern int  dtable_mode ;
extern byte dtable[256] ;
extern int  linelen ;
extern int  ncrlf   ;
extern int  nocrlf  ;

#define B64_goodchar(c) (dtable[c] != 0x80)  /* for decode only */

#define B64_EOL1 '\r'   /* CR */
#define B64_EOL2 '\n'   /* LF */

/*! Encode 3 bytes (a,b,c) into 4 bytes (w,x,y,z) */

#define B64_encode3(a,b,c,w,x,y,z)                 \
     ( w = dtable[(a)>>2]                      ,   \
       x = dtable[((a & 3) << 4) | (b >> 4)]   ,   \
       y = dtable[((b & 0xF) << 2) | (c >> 6)] ,   \
       z = dtable[c & 0x3F]                     )

/*! Encode 2 bytes (a,b) into 4 bytes (w,x,y,z) */

#define B64_encode2(a,b,w,x,y,z)                   \
     ( B64_encode3(a,b,0,w,x,y,z) , z = '=' )

/*! Encode 1 byte (a) into 4 bytes (w,x,y,z) */

#define B64_encode1(a,w,x,y,z)                     \
     ( B64_encode3(a,0,0,w,x,y,z) , y=z = '=' )

/*! Decode 4 bytes (w,x,y,z) into 3 bytes (a,b,c) */

#define B64_decode4(w,x,y,z,a,b,c)                 \
     ( a = (dtable[w] << 2) | (dtable[x] >> 4) ,   \
       b = (dtable[x] << 4) | (dtable[y] >> 2) ,   \
       c = (dtable[y] << 6) | dtable[z]         )

/*! Determine how many output bytes are encoded in a quad (w,x,y,z) */

#define B64_decode_count(w,x,y,z)                  \
     ( ((w)=='='||(x)=='=') ? 0                    \
                            : ((y)=='=') ? 1       \
                            : ((z)=='=') ? 2 : 3 )

extern void load_encode_table(void) ;
extern void load_decode_table(void) ;

/****************************************************************************/

typedef unsigned char *POINTER;   /* POINTER defines a generic pointer type */
typedef unsigned short int UINT2; /* UINT2 defines a two byte word */
typedef unsigned long int UINT4;  /* UINT4 defines a four byte word */

/* MD5 context data type */

typedef struct {
  UINT4 state[4];                                        /* state (ABCD) */
  UINT4 count[2];             /* number of bits, modulo 2^64 (lsb first) */
  unsigned char buffer[64];                              /* input buffer */
} MD5_CTX;

/* prototypes for some internal functions */

extern void MD5Init (MD5_CTX *);
extern void MD5Update (MD5_CTX *, unsigned char *, unsigned int);
extern void MD5Final (unsigned char [16], MD5_CTX *);

/****************************************************************************/

extern int     typedef_nib ;
extern int     typedef_num ;
extern char ** typedef_nam ;
extern char ** typedef_typ ;
extern char ** typedef_dim ;

/*! Characters allowed inside unquoted strings. */

#define IS_STRING_CHAR(c) ( isgraph(c) && !isspace(c) &&  \
                            (c) != '>' && (c) != '/'  &&  \
                            (c) != '=' && (c) != '<'    )

/*! Defines what we consider a quoting character. */

#define IS_QUOTE_CHAR(c)  ( (c) == '"' || (c) == '\'' )

/*! Defines characters allowed inside a "name". */

#define IS_NAME_CHAR(c) \
  (isalnum(c) || (c)=='_' || (c)=='.' || (c)=='-' || (c)==':')

extern void destroy_header_stuff( header_stuff *hs ) ;
extern intpair find_string( int nst, int nch, char *ch ) ;
extern header_stuff * parse_header_stuff( int ndat, char *dat, int *nused ) ;
extern intpair decode_type_field( char *tf ) ;
extern int_array * decode_dimen_string( char *ds ) ;
extern int_array * decode_type_string( char *ts ) ;
extern char NI_type_char( int typ ) ;
extern void enhance_header_stuff( header_stuff *hs ) ;
extern char * get_header_attribute( header_stuff *hs , char *attname ) ;

/****************************************************************************/

extern NI_element * make_empty_data_element ( header_stuff *hs ) ;
extern NI_group   * make_empty_group_element( header_stuff *hs ) ;
extern void NI_fill_vector_row( NI_element *nel , int row , char *buf ) ;

extern int NI_stream_writestring( NI_stream_type *ns , char *str ) ;
extern int NI_stream_fillbuf( NI_stream_type *ns, int minread, int msec ) ;

extern void NI_reset_buffer( NI_stream_type * ) ;
extern int  NI_decode_one_double( NI_stream_type *, double *, int ) ;
extern int  NI_decode_one_string( NI_stream_type *, char ** , int ) ;

extern void NI_set_veclab_from_stringlist( NI_element *nel , char *vstr ) ;
extern void NI_set_attribute_from_veclab_array( NI_element *nel , char **vec_lab ) ;

#endif
