#ifndef _NIML_HEADER_FILE_
#define _NIML_HEADER_FILE_

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifndef TCP_NODELAY
# include <netinet/tcp.h>
#endif
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <fcntl.h>
#include <sys/times.h>
#include <limits.h>
#include <math.h>     /* 14 Sep 2018 */

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

/*****---------------------------------------------------*****/

/* This is suppose to be defined in stddef.h, but
   apparently it isn't on all systems for some reason. */

#ifndef offsetof
# define offsetof(TYPE,MEMBER) ((size_t) &((TYPE *)0)->MEMBER)
#endif

/*****---------------------------------------------------*****/

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

#ifdef _SUNPERF_COMPLEX
# define TYPEDEF_complex
#endif

#ifndef TYPEDEF_complex
#define TYPEDEF_complex
typedef struct { float r,i ; } complex ;
#endif

/*****---------------------------------------------------*****/

/***** Macros for data type codes. *****/
/*---- The first 8 match mrilib.h. ----*/

#define NI_BYTE        0               /* == MRI_byte    */
#define NI_SHORT       1               /* == MRI_short   */
#define NI_INT         2               /* == MRI_int     */
#define NI_FLOAT32     3               /* == MRI_float   */
#define NI_FLOAT       NI_FLOAT32
#define NI_FLOAT64     4               /* == MRI_double  */
#define NI_DOUBLE      NI_FLOAT64
#define NI_COMPLEX64   5               /* == MRI_complex */
#define NI_COMPLEX     NI_COMPLEX64
#define NI_RGB         6               /* == MRI_rgb     */
#define NI_RGBA        7               /* == MRI_rgba    */

#define NI_IS_NUMERIC_TYPE(t) ( (t) >= 0 && (t) <= NI_RGBA )

#define NI_STRING      8               /* after "basic" types */

/*! One more than the last NI_ data type code defined above. */

#define NI_NUM_TYPES        9

/*! Number of types of fixed size ("basic" types).
    Note that if this changes,
    the NI_rowtype stuff must be altered accordingly. */

#define NI_NUM_BASIC_TYPES  8

/*! Valid data type character codes. */

#define IS_DATUM_CHAR(c) ( (c) == 'b' || (c) == 's' || (c) == 'i' ||  \
                           (c) == 'f' || (c) == 'd' || (c) == 'c' ||  \
                           (c) == 'r' || (c) == 'S' || (c) == 'L' ||  \
                           (c) == 'R'                               )

#define NI_is_builtin_type(t)                                         \
  ( (t) >= 0 && (t) < NI_NUM_TYPES )

#define NI_is_basic_type(t)                                           \
  ( (t) >= 0 && (t) < NI_NUM_BASIC_TYPES )

/*--------------------------------------------------------------------------*/
/*! This type stores the information about user-defined types. 09 Dec 2002. */

#ifndef TYPEDEF_NI_rowtype
#define TYPEDEF_NI_rowtype
struct NI_rowtype ;  /* incomplete definition */

typedef struct NI_rowtype {
  int   code ;         /*!< unique integer code for this type */
  int   size ;         /*!< number of bytes for this type (w/padding) */
  int   psiz ;         /*!< sum of sizes of the parts (no padding)
                            - will be zero if has variable type arrays */
  int   algn ;         /*!< byte alignment for this type */
  int   flag ;         /*!< various bit flags */
  char *name ;         /*!< unique string name for this type */
  char *userdef ;      /*!< definition user gave for this type */
  int   comp_num ;     /*!< number of components (components may be rowtypes) */
  int  *comp_typ ;     /*!< integer codes of the components */
  int  *comp_dim ;     /*!< if >=0, index of dimension of this component */
  int   part_num ;     /*!< number of parts (parts are usually basic types) */
  int  *part_typ ;     /*!< integer codes of the parts */
  int  *part_off ;     /*!< byte offsets of the parts */
  int  *part_siz ;     /*!< byte sizes of the parts */
  int  *part_dim ;     /*!< if >=0, index of dimension of this part */

  struct NI_rowtype **part_rtp; /*!< rowtype ptr for each part;
                                     N.B.: builtin types point to themselves! */
} NI_rowtype ;
#endif

/*! NI_rowtype bit flag for variable size data. */

#define ROWTYPE_VARSIZE_MASK (1<<0)

/*! Check if a NI_rowtype struct is marked as having variable size data */

#define ROWTYPE_is_varsize(rr) (((rr)->flag & ROWTYPE_VARSIZE_MASK) != 0)

/*! Get the dimension of the qq-th part of
    the struct stored at pointer pt, of type rt.
    This macro should only be used if rt->part_dim[qq] >= 0. */

#define ROWTYPE_part_dimen(rt,pt,qq)                           \
 ( *((int *)( (pt) + (rt)->part_off[ (rt)->part_dim[qq] ] )) )

/*! Macro to delete a NI_rowtype struct.  Only used when an
    error happens when creating one, since new types last forever. */

#define delete_rowtype(rr)                 \
 do{ NI_free((rr)->name)     ;             \
     NI_free((rr)->userdef)  ;             \
     NI_free((rr)->comp_typ) ;             \
     NI_free((rr)->part_typ) ;             \
     NI_free((rr)->part_off) ;             \
     NI_free(rr)             ; } while(0)

extern int          NI_rowtype_define       ( char *, char * ) ;
extern NI_rowtype * NI_rowtype_find_name    ( char * ) ;
extern NI_rowtype * NI_rowtype_find_code    ( int ) ;
extern int          NI_rowtype_name_to_code ( char * ) ;
extern char *       NI_rowtype_code_to_name ( int ) ;
extern char *       NI_rowtype_code_to_alias( int ) ;    /* 19 Feb 2003 */
extern int          NI_rowtype_name_to_size ( char * ) ;
extern int          NI_rowtype_code_to_size ( int ) ;

extern int          NI_rowtype_vsize     ( NI_rowtype *, void * ) ;
extern void         NI_val_to_text       ( NI_rowtype *, char *, char * ) ;
extern int          NI_val_to_binary     ( NI_rowtype *, char *, char * ) ;
extern void         NI_multival_to_text  ( NI_rowtype *, int, char *, char * );
extern int          NI_multival_to_binary( NI_rowtype *, int, char *, char * );
extern int          NI_has_String        ( NI_rowtype * ) ;
extern void         NI_swap_column       ( NI_rowtype * , int , char * ) ;

extern void         NI_rowtype_debug( int ) ;

/*! Used to test if a rowtype code is a basic type. */

#define ROWTYPE_is_basic_code  NI_is_basic_type

/*! Integer type code to name string. */

extern char * NI_type_name( int ) ;

/*****------------------------------------------------------------------*****/

#define NI_ELEMENT_TYPE  17
#define NI_GROUP_TYPE    18
#define NI_PROCINS_TYPE  19

/*! A data element. */

#ifndef TYPEDEF_NI_element
#define TYPEDEF_NI_element
typedef struct {
   int    type ;       /*!< What type of struct is this? */
   int    outmode ;    /*!< If >=0, output mode. */
   char  *name ;       /*!< Name of element. */
   int    attr_num ;   /*!< Number of attributes. */
   char **attr_lhs ;   /*!< Left-hand-sides of attributes. */
   char **attr_rhs ;   /*!< Right-hand-sides of attributes. */
   int    vec_num ;    /*!< Number of vectors (may be 0). */
   int    vec_len ;    /*!< Length of each vector. */
   int    vec_filled ; /*!< Length that each one was filled up. */
   int   *vec_typ ;    /*!< Type code for each vector. */
   void **vec ;        /*!< Pointer to each vector. */
   char **vec_lab ;    /*!< Ptr to label string for each vector [optional] */

   int    vec_rank ;        /*!< Number of dimensions, from ni_dimen. */
   int   *vec_axis_len ;    /*!< Array of dimensions, from ni_dimen. */
   float *vec_axis_delta ;  /*!< Array of step sizes, from ni_delta. */
   float *vec_axis_origin ; /*!< Array of origins, from ni_origin. */
   char **vec_axis_unit ;   /*!< Array of units, from ni_units. */
   char **vec_axis_label ;  /*!< Array of labels, from ni_axes. */
} NI_element ;
#endif

/*! A bunch of elements. */

#ifndef TYPEDEF_NI_group
#define TYPEDEF_NI_group
typedef struct {
   int    type ;       /*!< What type of struct is this? */
   int    outmode ;    /*!< If >=0, output mode. */
   int    attr_num ;   /*!< Number of attributes. */
   char **attr_lhs ;   /*!< Left-hand-sides of attributes. */
   char **attr_rhs ;   /*!< Right-hand-sides of attributes. */

   int    part_num ;   /*!< Number of parts within this group. */
   int   *part_typ ;   /*!< Type of each part (element or group). */
   void **part ;       /*!< Pointer to each part. */

   char  *name ;       /*!< Name (default="ni_group") - 03 Jun 2002 */
} NI_group ;
#endif

/*! A processing instruction. */

#ifndef TYPEDEF_NI_procins
#define TYPEDEF_NI_procins
typedef struct {
   int    type ;       /*!< What type of struct is this? */
   int    attr_num ;   /*!< Number of attributes. */
   char **attr_lhs ;   /*!< Left-hand-sides of attributes. */
   char **attr_rhs ;   /*!< Right-hand-sides of attributes. */
   char  *name ;       /*!< The 'PItarget', as in '<?name ...?>' */
} NI_procins ;
#endif

extern NI_procins * NI_rowtype_procins( NI_rowtype * ) ; /* 19 Apr 2005 */

#ifdef  __cplusplus
}
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

   int goodcheck_time ;   /*!< NI_clock_time() of last SHM_goodcheck() */
 } SHMioc ;

#else  /* DONT_USE_SHM */

# define SHMioc void  /* dummy definition */

#endif /* DONT_USE_SHM */
/*-----------------------------------------------------------------*/

#ifdef  __cplusplus
extern "C" {                    /* care of Greg Balls    7 Aug 2006 [rickr] */
#endif

/*! Size of NI_stream buffer. */

#define NI_BUFSIZE (255*1024)

/*! Data needed to process input stream. */

#ifndef TYPEDEF_NI_stream_type
#define TYPEDEF_NI_stream_type
typedef struct {
   int type ;        /*!< NI_TCP_TYPE, NI_FILE_TYPE, etc. */
   int bad ;         /*!< Tells whether I/O is OK for this yet */

   int port ;        /*!< TCP only: port number */
   int sd ;          /*!< TCP only: socket descriptor */

   FILE *fp ;        /*!< FILE only: pointer to open file */
   int fsize ;       /*!< FILE only: length of file for input */

   char name[256] ;  /*!< Hostname or filename */

   int io_mode ;     /*!< Input or output? */
   int data_mode ;   /*!< Text, binary, or base64? */

   int bin_thresh ;  /*!< Threshold size for binary write. */

   int nbuf ;             /*!< Number of bytes left in buf. */
   int npos ;             /*!< Index of next unscanned byte in buf. */
   int bufsize ;          /*!< Length of buf array. */
   char *buf ;            /*!< I/O buffer (may be NULL). */

   SHMioc *shmioc ;       /*!< for NI_SHM_TYPE only */

   char orig_name[256] ;  /*!< original (input) name when opened */

   int goodcheck_time ;   /*!< NI_clock_time() of last NI_stream_goodcheck() */

   int b64_numleft ;      /*!< For use in NI_stream_readbuf64() */
   byte b64_left[4] ;     /*!< Leftover decoded bytes from NI_stream_readbuf64() */
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

#define MARKED_FOR_DEATH  6666

/* I/O Modes for a NI_stream_type: input or output. */

#define NI_INPUT_MODE  0
#define NI_OUTPUT_MODE 1

/* Data modes for a NI_stream_type: text, binary, base64. */

#define NI_TEXT_MODE    0
#define NI_BINARY_MODE  1
#define NI_BASE64_MODE  2

#define NI_HEADERONLY_FLAG  (1<<8)  /* 20 Feb 2003 */
#define NI_HEADERSHARP_FLAG (1<<9)  /* 20 Mar 2003 */

#define NI_LSB_FIRST    1
#define NI_MSB_FIRST    2

/* Attribute writing modes [15 Oct 2002] */

#define NI_ATTMODE_NORMAL 0
#define NI_ATTMODE_SPACED 1
#define NI_ATTMODE_LAST   1

extern void NI_set_attribute_mode( int ) ;

/* Type name writing modes [19 Feb 2003] */

#define NI_NAMEMODE_NORMAL 0
#define NI_NAMEMODE_ALIAS  1
#define NI_NAMEMODE_LAST   1

extern void NI_set_typename_mode ( int ) ;

/*****---------- Hash table stuff [26 Aug 2002] ----------*****/

#ifndef TYPEDEF_Htable
#define TYPEDEF_Htable
typedef struct {
  int     len , ntot ;
  void ***vtab ;             /* pointers */
  char ***ctab ;             /* digests */
  int    *ntab ;             /* counts */
} Htable ;
#endif

extern Htable * new_Htable( int ) ;
extern void     destroy_Htable( Htable * ) ;
extern void     addto_Htable( char *, void *, Htable * ) ;
extern void *   findin_Htable( char *, Htable * ) ;
extern void     removefrom_Htable( char *, Htable * ) ;
extern void     profile_Htable( char *, Htable * ) ;
extern void     subsume_Htable( Htable *, Htable * ) ;
extern void     Htable_set_vtkill( int ) ;
extern void     resize_Htable( int , Htable * ) ;

#define         sizeof_Htable(ht) ((ht)->ntot)

/*--- double Htable (string-string pairs) [15 Oct 2003] ---*/

#ifndef TYPEDEF_Dtable
#define TYPEDEF_Dtable
typedef struct { Htable *hta, *htb ; } Dtable ;
#endif

extern Dtable * new_Dtable( int ) ;
extern void     destroy_Dtable( Dtable * ) ;
extern void     addto_Dtable( char *, char *, Dtable * ) ;
extern char *   findin_Dtable_a( char *, Dtable * ) ;
extern char *   findin_Dtable_b( char *, Dtable * ) ;
extern void     removefrom_Dtable_a( char *, Dtable * ) ;
extern void     removefrom_Dtable_b( char *, Dtable * ) ;
extern int      listize_Dtable( Dtable *, char ***, char *** ) ;
extern char *   Dtable_to_nimlstring( Dtable * , char * ) ;
extern Dtable * Dtable_from_nimlstring( char * ) ;

/*****------------------- DIME stuff [04 Nov 2002] ------------------*****/

#ifndef TYPEDEF_DIME_part
#define TYPEDEF_DIME_part
typedef struct {
   int          type ;
   int          flags ;
   char        *id_string ;
   char        *type_string ;
   unsigned int data_len ;
   byte        *data ;
} DIME_part ;
#endif

#ifndef TYPEDEF_DIME_message
#define TYPEDEF_DIME_message
typedef struct {
   int         num_part ;
   DIME_part **part ;
} DIME_message ;
#endif

#define DIME_MB_MASK      (1<<0)
#define DIME_ME_MASK      (1<<1)
#define DIME_CF_MASK      (1<<2)

#define DIME_VERSION_MASK (0xf8)

DIME_message * DIME_read_message( NI_stream_type * , int ) ;
DIME_part    * DIME_read_part   ( NI_stream_type * , int ) ;

void           DIME_destroy_message( DIME_message * ) ;

/*****--------------------------------------------------------------*****/

#if 0
typedef struct { int nar ; float  *ar ; } NI_floatvec ;

#define KILL_NI_floatvec(fv)                 \
  do{ if( (fv) != NULL ){                     \
        if( (fv)->ar != NULL ) free((fv)->ar); \
        free(fv); (fv) = NULL;                  \
  }} while(0)

#define MAKE_NI_floatvec(fv,n)                           \
  do{ (fv) = (NI_floatvec *)malloc(sizeof(NI_floatvec)) ; \
      (fv)->nar = (n) ;                                    \
      (fv)->ar  = (float *)calloc(sizeof(float),(n)) ;      \
  } while(0)

#define RESIZE_NI_floatvec(fv,m)                               \
  do{ if( (fv)->nar != (m) ){                                   \
        (fv)->nar = (m) ;                                        \
        (fv)->ar  = (float *)realloc((fv)->ar,sizeof(float)*(m)); \
  }} while(0)
#endif

/*****------------------------- prototypes -------------------------*****/

/** 18 Nov 2002: replace old malloc functions with new ones **/

#undef NIML_OLD_MALLOC
#if (defined(NIML_OLD_MALLOC) || defined(DONT_USE_MCW_MALLOC)) && !defined(__cplusplus) && !defined(c_plusplus)
#define NI_malloc(typ,a) (typ*) old_NI_malloc((a))
#define NI_calloc(a,b) old_NI_malloc((a)*(b))
#define NI_realloc(a,typ,b)  (typ*) old_NI_realloc((a),(b))

  extern void * old_NI_malloc( size_t ) ;
  extern void   NI_free( void * ) ;
  extern void * old_NI_realloc( void *, size_t ) ;
#else
#  define NI_malloc(typ,a)   (typ*) hidden_NI_malloc((a),__FILE__,__LINE__)
#  define NI_calloc(a,b)   hidden_NI_malloc((a)*(b),__FILE__,__LINE__)
#  define NI_realloc(a,typ,b) (typ*) hidden_NI_realloc((a),(b),__FILE__,__LINE__)
#  define NI_free(a)       hidden_NI_free((a),__FILE__,__LINE__)

  extern void * hidden_NI_malloc( size_t , char * , int ) ;
  extern void * hidden_NI_realloc( void * , size_t , char * , int ) ;
  extern void   hidden_NI_free( void * , char * , int ) ;
#endif

extern char * NI_malloc_status(void) ;
extern void NI_malloc_dump(void) ;
extern void NI_malloc_enable_tracking(void) ;
extern int NI_malloc_tracking_enabled(void) ;

extern int NI_malloc_replace( void *(*um)(size_t)        ,
                              void *(*ur)(void *,size_t) ,
                              void  (*uf)(void *)         ) ;

/*! Free and set pointer to NULL. */
#define NI_FREE(p) ( NI_free(p), (p)=NULL )

/*! Make a new block of a given type. */

/* #define NI_new(typ) ( (typ *)NI_malloc(sizeof(typ)) )   09 Dec 2002 */
#define NI_new(typ) ( NI_malloc(typ, sizeof(typ)) )   /* 15 Dec 2003 */

extern char * NI_strncpy( char *, const char *, size_t ) ;
extern char * NI_strdup( char * ) ;
extern char * NI_strdup_len( char *, int ) ;
extern int    NI_strlen( char * ) ;
extern long   NI_filesize( char * ) ;
extern int    NI_clock_time(void) ;
extern int    NI_byteorder(void) ;
extern void   NI_swap2( int, void * ) ;
extern void   NI_swap4( int, void * ) ;
extern void   NI_swap8( int, void * ) ;

#define NI_is_file(pn) (NI_filesize(pn) >= 0)   /* 10 Dec 2002 */

extern char * NI_mktemp( char * ) ;  /* 21 Aug 2002 */

extern int    NI_type_size( int ) ;

extern int NI_element_rowsize( NI_element * ) ;
extern int NI_element_allsize( NI_element * ) ;

extern void NI_free_element( void * ) ;
extern void NI_free_element_data( void * ) ;  /* 17 Jul 2006 */
extern int  NI_element_type( void * ) ;
extern char * NI_element_name( void * ) ;  /* 18 Apr 2005 */

extern NI_element * NI_new_data_element( char *, int ) ;
extern void NI_add_column( NI_element *, int, void * ) ;
extern void NI_set_column_label( NI_element *nel, int cc, char *lab ) ; /* 11 Sep 2018 */
extern void NI_move_column(NI_element *nel, int ibefore, int iafter);
extern void NI_insert_column( NI_element *nel , int typ , void *arr, int icol );
extern float NI_extract_float_value( NI_element *nel , int row , int col ) ; /* 14 Sep 2018 */
extern void NI_remove_column(NI_element *nel, int irm);
extern void NI_copy_all_attributes( void *nisrc , void *nitrg );
void *NI_duplicate(void *vel, byte with_data);
void *NI_duplicate_element (void *vel, byte with_data);
void *NI_duplicate_group (void *vel, byte with_data);
extern void   NI_kill_attribute( void *, char * ) ;
extern void   NI_set_attribute( void *, char *, char * ) ;
extern char * NI_get_attribute( void *, char * ) ;
extern void NI_insert_value( NI_element *, int,int, void * );       /* 03 Apr 2003 */
extern void NI_insert_column_stride( NI_element *nel, int typ, void *arr, int stride, int icol );
extern void NI_add_column_stride( NI_element *, int, void *, int ); /* 29 May 2003 */
extern void NI_fill_column_stride( NI_element *,int,void *,int,int);/* 23 Mar 2004 */
extern void NI_insert_string( NI_element *, int,int, char *);       /* 19 Apr 2005 */
extern void NI_alter_veclen( NI_element * , int ) ;                 /* 19 Apr 2005 */
extern void NI_set_ni_type_atr( NI_element * ) ;       /* 14 Jul 2006 [rickr] */

extern NI_element * NI_extract_columns( NI_element *nel, int nc, int *cc ) ; /* 13 Sep 2018 */


extern NI_group * NI_new_group_element(void) ;
extern void NI_add_to_group( NI_group *, void * ) ;
extern void NI_rename_group( NI_group *, char * ) ;                 /* 03 Jun 2002 */
extern void NI_remove_from_group( NI_group *, void * ) ;            /* 16 Apr 2005 */

extern int NI_search_group_shallow( NI_group *, char *, void *** ); /* 18 Apr 2005 */
extern int NI_search_group_deep   ( NI_group *, char *, void *** ); /* 18 Apr 2005 */

extern NI_procins * NI_new_processing_instruction( char * ) ;       /* 16 Mar 2005 */

extern void NI_swap_vector( int, int, void * ) ;

#undef  NI_set_attribute_int
#define NI_set_attribute_int(el,nm,vv)  \
 do{ char ib[16]; sprintf(ib,"%d",(vv)); NI_set_attribute((el),(nm),ib); } while(0)

/* port assignment functions from afni_ports.c */
extern int init_ports_list();
extern void set_ports_list_reinit(void);
extern int get_port_named(char *name);
extern void show_ports_list(void);
extern int get_available_npb(void);
extern int set_user_np(int v);
extern int get_user_np(void);
extern char *get_port_numbered(int port);
extern char *get_np_help();
extern int set_user_pif(char *s);
extern char * get_user_pif(void);
extern int set_user_np_bloc(int v);
extern int get_max_port_bloc(void);
extern int get_num_ports(void);
extern int get_user_np_bloc(void);

/** I/O functions **/

extern NI_stream NI_stream_open( char *, char * ) ;
extern int NI_stream_goodcheck( NI_stream_type *, int ) ;
extern void NI_stream_close( NI_stream_type * ) ;
extern void NI_stream_kill ( NI_stream_type * ) ;      /* 02 Jan 2004 */
extern void NI_stream_closenow( NI_stream_type * ) ;   /* 02 Jan 2004 */
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
extern void NI_stream_seek( NI_stream_type * , int , int ) ; /* 24 Mar 2003 */
extern int NI_stream_writestring( NI_stream_type * , char * ) ;

extern int NI_stream_setbufsize( NI_stream_type *, int ) ; /* 03 Jan 2003 */
extern int NI_stream_getbufsize( NI_stream_type * ) ;
extern int NI_stream_readbuf( NI_stream_type *, char *, int ) ;
extern int NI_stream_readbuf64( NI_stream_type *, char *, int ) ;  /* 20 Apr 2005 */
extern int NI_text_to_val  ( NI_stream_type *, NI_rowtype *, void *, int );
extern int NI_binary_to_val( NI_stream_type *, NI_rowtype *, void *, int );
extern int NI_base64_to_val( NI_stream_type *, NI_rowtype *, void *, int );

extern int NI_stream_setb64( NI_stream_type * , int ) ;   /* 20 Apr 2005 */

extern int NI_stream_reopen( NI_stream_type *, char * ) ; /* 23 Aug 2002 */

extern void * NI_read_element ( NI_stream_type *, int ) ;
extern int    NI_write_element( NI_stream_type *, void *, int ) ;
extern int    NI_write_procins( NI_stream_type *, char * ) ; /* 17 Mar 2005 */
extern int    NI_write_columns( NI_stream_type * ,
                                int , int * , int , void ** , int ) ;
extern int    NI_write_rowtype( NI_stream_type * ,
                                NI_rowtype * , int , void * , int ) ;
extern int    NI_read_columns ( NI_stream_type *,
                                int, int *, int, void **, int,int ) ;
extern void   NI_free_column  ( NI_rowtype * , int , void * );
extern void * NI_copy_column  ( NI_rowtype * , int , void * );
extern int    NI_size_column  ( NI_rowtype * , int , void * ); /* 26 Mar 2003 */

extern void   NI_set_read_header_only   ( int ) ;              /* 21 Mar 2003 */
extern int    NI_get_read_header_only( void );                 /* 24 Feb. 2012 */
extern void * NI_read_element_header( NI_stream_type *, int ); /* 26 Mar 2003 */
extern void   NI_skip_procins( int ) ;                         /* 03 Jun 2005 */

extern int NI_write_element_tofile( char *, void *, int ) ;    /* 07 Mar 2007 */
extern void * NI_read_element_fromfile( char * ) ;             /* 12 Mar 2007 */
extern void * NI_read_element_fromstring( char *nstr );     /* 26 Feb 2010 ZSS*/
extern char * NI_write_element_tostring( void *nel ); /* Oct 2011 ZSS */

#define NI_SWAP_MASK  (1<<0)
#define NI_LTEND_MASK (1<<1)

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
extern void   MD5_set_xor_use(int) ;  /* 09 Apr 2015 */

extern char * UNIQ_idcode(void) ;
extern char * UNIQ_idcode_11(void) ;             /* 10 Feb 2016 */
extern void   UNIQ_idcode_fill( char * ) ;
extern char * UNIQ_hashcode( char * ) ;
extern void UNIQ_hashprefix_fill( char *idc ) ;  /* 30 Apr 2013 */
extern char * UNIQ_hashprefix( void ) ;

extern char * UUID_hashcode( char * ) ;   /* 20 Aug 2002 */
extern char * UUID_idcode(void) ;

/* trusted host manipulation */

extern char * NI_hostname_to_inet( char *host ) ;
extern void   NI_add_trusted_host( char *hostname ) ;
extern int    NI_trust_host( char *hostid ) ;

/*! Close a NI_stream, and set the pointer to NULL. */

#define NI_STREAM_CLOSE(nn) do{ NI_stream_close(nn); (nn)=NULL; } while(0)

/****************************************************************************
  This stuff defines various types, macros, and function prototypes
  for generic datasets and domains for neuroimaging applications.
*****************************************************************************/

#ifndef TYPEDEF_NI_INDEX_T
#define TYPEDEF_NI_INDEX_T
typedef int NI_index_t ;      /* used to store indexes, vector lengths */
#endif

/*---------------------------------------------------------------------------*/
/*! Stuff that goes at the top of every NI struct:
     - type is a code that lets us tell what kind of struct it is
     - nref is a reference count
     - idcode is a globally unique string (max 63 characters)
     - name is an arbitrary string for fun, profit, and elucidation
     - either or both of these strings may be NULL
-----------------------------------------------------------------------------*/

#define NI_BASIC_PARTS  \
  int type ;            \
  int nref ;            \
  char *idcode ;        \
  char *name

/*---------------------------------------------------------------------------*/
/*! The minimal NI struct, with only the basic elements. */

typedef struct {
  NI_BASIC_PARTS ;
} NI_struct ;

extern void   NI_free_struct      ( void * ) ;
extern void * NI_copy_struct      ( void * ) ;
extern void * NI_pointto_struct   ( void * ) ;

extern void   NI_register_struct  ( void * ) ;
extern void * NI_find_struct      ( char * ) ;
extern void   NI_unregister_struct( void * ) ;

/*---------------------------------------------------------------------------*/
/*! NI struct to hold one float. */

typedef struct {
  NI_BASIC_PARTS ;
  float val ;
} NI_float_one ;

#define NI_float_val(nd) ((nd)->val)

/*---------------------------------------------------------------------------*/
/*! NI struct to hold the definition of a statistical distribution for
    a NI_vector (call it v):
      - statcode  = one of the NI_STAT_* codes
      - param_num = number of parameters for this distribution
      - param[i]  = parameter #i, for i=0..param_num-1:
             - this will either be a NI_float_one, which means it
                is constant for all elements of the vector
             - OR
             - this will be a NI_vector itself, of float type,
                which means that the #i parameter for v[j] is stored
                in param[i][j]
             - for example, you can have an F-statistic with the
               first DOF param being different for every node and
               the second DOF param being fixed
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  int statcode ;
  NI_index_t param_num ;
  NI_struct  **param ;
} NI_statistic ;

#define NI_stat_code(nd)      ((nd)->statcode)
#define NI_stat_param_num(nd) ((nd)->param_num)
#define NI_stat_param(nd,i,j)                             \
  ( ((nd)->param[i]->type == NI_FLOAT_ONE_TYPE)           \
     ? ( ((NI_float_one *)(nd)->param[i])->val )          \
     : ( ((NI_float_vector *)(nd)->param[i])->vec[j] ) )

/*--- Statistical type codes:
       2..10 match AFNI's 3ddata.h
       the rest match those in NIfTI-1.1 (nifti1.h) ---*/

                                 /** Parameters **/
#define NI_STAT_CORREL      2   /* Samples, fits, orts   */
#define NI_STAT_TTEST       3   /* DOF                   */
#define NI_STAT_FTEST       4   /* 2 DOF                 */
#define NI_STAT_ZSCORE      5   /* no params             */
#define NI_STAT_CHISQ       6   /* DOF                   */
#define NI_STAT_BETA        7   /* a and b params        */
#define NI_STAT_BINOM       8   /* # trials, p per trial */
#define NI_STAT_GAMMA       9   /* shape, scale params   */
#define NI_STAT_POISSON    10   /* mean                  */

#define NI_STAT_NORMAL     11   /* mean, variance        */
#define NI_STAT_FTEST_NONC 12   /* 2 DOF, noncentrality  */
#define NI_STAT_CHISQ_NONC 13   /* DOF, noncentrality    */
#define NI_STAT_LOGISTIC   14   /* location, scale       */
#define NI_STAT_LAPLACE    15   /* location, scale       */
#define NI_STAT_UNIFORM    16   /* start, end            */
#define NI_STAT_TTEST_NONC 17   /* DOF, noncentrality    */
#define NI_STAT_WEIBULL    18   /* location, scale, power*/
#define NI_STAT_CHI        19   /* DOF                   */
#define NI_STAT_INVGAUSS   20   /* mu, lambda            */
#define NI_STAT_EXTVAL     21   /* location, scale       */

#define NI_STAT_PVAL       22
#define NI_STAT_LOGPVAL    23
#define NI_STAT_LOG10PVAL  24

#define NI_STAT_FIRSTCODE   2
#define NI_STAT_LASTCODE   24

extern int    NI_stat_numparam( int ) ;
extern char * NI_stat_distname( int ) ;
extern void   NI_stat_decode( char *, int *, float *, float *, float * ) ;
extern char * NI_stat_encode( int , float,float,float ) ;

/*---------------------------------------------------------------------------*/
/*! NI struct to hold a vector of values:
     - vec_len   = number of values
     - vec_typ   = type of values (e.g., NI_FLOAT, etc.)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
     - the size in bytes of each element of vec can be determined
       by NI_datatype_size(vec_typ)
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t vec_len ;
  int vec_typ ;
  void *vec ;
  void *vec_range ;
  NI_statistic *statistic ;
} NI_vector ;

extern void * NI_new_vector( int , NI_index_t ) ;
extern void   NI_set_vector_range( void * ) ;

/*********************************************************
   The special vector types below are mostly convenient
   for having vectors of the basic types pre-defined.
   Field for field, they match the NI_vector above,
   except that the "void *" components are pre-declared
   to be the correct basic type (don't have to cast).
   Therefore, you can do casts like this:
     NI_vector *vv ;
     if( vv->vec_typ == NI_FLOAT ){
       NI_float_vector *ff = (NI_float_vector *) vv ;
       ff->vec[0] = 7.3 ;
     }
**********************************************************/

/*---------------------------------------------------------------------------*/
/*! NI struct to hold a vector of byte values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NI_BYTE)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t vec_len ;
  int vec_typ ;
  byte *vec ;
  byte *vec_range ;
  NI_statistic *statistic ;
} NI_byte_vector ;

/*---------------------------------------------------------------------------*/
/*! NI struct to hold a vector of short values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NI_SHORT)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t vec_len ;
  int vec_typ ;
  short *vec ;
  short *vec_range ;
  NI_statistic *statistic ;
} NI_short_vector ;

/*---------------------------------------------------------------------------*/
/*! NI struct to hold a vector of int values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NI_INT)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t vec_len ;
  int vec_typ ;
  int *vec ;
  int *vec_range ;
  NI_statistic *statistic ;
} NI_int_vector ;

/*---------------------------------------------------------------------------*/
/*! NI struct to hold a vector of float values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NI_FLOAT)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t vec_len ;
  int vec_typ ;
  float *vec ;
  float *vec_range ;
  NI_statistic *statistic ;
} NI_float_vector ;

/*---------------------------------------------------------------------------*/
/*! NI struct to hold a vector of double values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NI_DOUBLE)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t vec_len ;
  int vec_typ ;
  double *vec ;
  double *vec_range ;
  NI_statistic *statistic ;
} NI_double_vector ;

/*---------------------------------------------------------------------------*/
/*! NI struct to hold a vector of complex values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NI_COMPLEX)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t vec_len ;
  int vec_typ ;
  complex *vec ;
  complex *vec_range ;
  NI_statistic *statistic ;
} NI_complex_vector ;

/*---------------------------------------------------------------------------*/
/*! NI struct to hold a vector of rgb values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NI_RGB)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t vec_len ;
  int vec_typ ;
  rgb *vec ;
  rgb *vec_range ;
  NI_statistic *statistic ;
} NI_rgb_vector ;

/*---------------------------------------------------------------------------*/
/*! NI struct to hold a vector of rgba values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NI_RGBA)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t vec_len ;
  int vec_typ ;
  rgba *vec ;
  rgba *vec_range ;
  NI_statistic *statistic ;
} NI_rgba_vector ;

/*---------------------------------------------------------------------------*/
/*! NI struct to hold a vector of string values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NI_STRING)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t vec_len ;
  int vec_typ ;
  char **vec ;
  char **vec_range ;
  NI_statistic *statistic ;
} NI_string_vector ;

/*---------------------------------------------------------------------------*/
/*! NI struct to define a coordinate mapping between one 3D domain
    and another.
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  float mat[4][4] ;
} NI_affine_3dmap ;

/*---------------------------------------------------------------------------*/
/*! NI struct to define a 1..4 dimensional rectangular domain:
     - nx,ny,nz,nt = number of voxels along each axis
     - nvox        = total number of voxels
     - dx,dy,dz,dt = grid spacing along each axis
     - xo,yo,zo,to = origin of each axis
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t nx,ny,nz,nt , nvox ;
  float dx,dy,dz,dt ;
  float xo,yo,zo,to ;
} NI_rect_domain ;

/*---------------------------------------------------------------------------*/
/*! NI struct to define a domain of scattered points:
     - num_node = number of nodes (points)
     - id       = list of integer node identifiers
     - x,y,z    = list of spatial coordinates
     - seq      = If 1, node id's are sequential
     - seqbase  = If id's are sequential, is smallest id
     - sorted   = If 1, id's are sorted into increasing order
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t  num_node ;
  NI_index_t *id ;
  float        *x , *y , *z ;
  int           seq ;
  int           seqbase ;
  int           sorted ;
} NI_points_domain ;

/*---------------------------------------------------------------------------*/
/*! NI struct to hold a generic dataset, which is a collection of value
    vectors defined over a common domain.
      - num_node = number of nodes in the domain
      - num_val  = number of values at each node
      - order    = code indicated whether the value vectors are
                   along the node direction or value index direction
      - vec[i]   = i-th value vector
      - domain   = definition of domain the nodes occupy (if not NULL)
-----------------------------------------------------------------------------*/

typedef struct {
  NI_BASIC_PARTS ;
  NI_index_t num_node , num_val ;
  int order ;
  NI_vector **vec ;
  NI_struct  *domain ;
} NI_dataset ;

#define NI_NODE_DIRECTION  55   /* for the order element */
#define NI_INDEX_DIRECTION 56

#define NI_dataset_vecnum(nd)  \
  ( ((nd)->order == NI_NODE_DIRECTION) ? (nd)->num_val : (nd)->num_node )

#define NI_dataset_veclen(nd)  \
  ( ((nd)->order == NI_NODE_DIRECTION) ? (nd)->num_node: (nd)->num_val  )

#define NI_opposite_order(oo)  \
  ( ((oo) == NI_NODE_DIRECTION) ? NI_INDEX_DIRECTION : NI_NODE_DIRECTION )

extern void * NI_dataset_transpose( void * ) ;

/*---------------------------------------------------------------------------*/
/* Codes for the "type" element of a NI struct. */

#define NI_STRUCT_TYPE          6660000
#define NI_FLOAT_ONE_TYPE       6660002
#define NI_STATISTIC_TYPE       6660003
#define NI_DATASET_TYPE         6660004

#define NI_VECTOR_TYPE          6660100
#define NI_BYTE_VECTOR_TYPE     6660101
#define NI_SHORT_VECTOR_TYPE    6660102
#define NI_INT_VECTOR_TYPE      6660103
#define NI_FLOAT_VECTOR_TYPE    6660104
#define NI_DOUBLE_VECTOR_TYPE   6660105
#define NI_COMPLEX_VECTOR_TYPE  6660106
#define NI_RGB_VECTOR_TYPE      6660107
#define NI_RGBA_VECTOR_TYPE     6660108
#define NI_STRING_VECTOR_TYPE   6660109

#define NI_is_vector_type(tt)                                 \
 ( (tt) >= NI_VECTOR_TYPE && (tt) <= NI_STRING_VECTOR_TYPE )

#define NI_patch_vector_type(nn)                              \
 do{ if( NI_is_vector_type((nn)->type) &&                     \
         NI_is_builtin_type((nn)->vec_typ) )                  \
       (nn)->type = NI_VECTOR_TYPE + (nn)->vec_typ + 1 ;      \
 } while(0)

#define NI_RECT_DOMAIN_TYPE     6660201
#define NI_POINTS_DOMAIN_TYPE   6660202

#define NI_is_domain_type(tt)                                     \
 ( (tt) >= NI_RECT_DOMAIN_TYPE && (tt) <= NI_POINTS_DOMAIN_TYPE )

#define NI_AFFINE_3DMAP_TYPE    6660301

#define NI_is_3dmap_type(tt)                                      \
 ( (tt) >= NI_AFFINE_3DMAP_TYPE && (tt) <= NI_AFFINE_3DMAP_TYPE )

#define NI_datatype_size(n) NI_rowtype_code_to_size(n)

/*-------------------------------------------------------------------------*/

typedef void NI_voidfunc() ;

extern int NI_do( NI_stream_type * , NI_element * ) ;
extern void NI_register_doer( char *, NI_voidfunc * ) ;
/*-------------------------------------------------------------------------*/
/*! An array of strings, each allocated with NI_malloc(). */

typedef struct { int num; char **str; } NI_str_array ;

#define NI_delete_str_array(sar)             \
  do{ int pp ;                               \
      for( pp=0 ; pp < (sar)->num ; pp++ )   \
        NI_free( (sar)->str[pp] );           \
      NI_free((sar)->str) ; NI_free(sar) ;   \
  } while(0)

extern NI_str_array * NI_decode_string_list( char *ss , char *sep ) ;
extern NI_str_array * NI_strict_decode_string_list( char *ss , char *sep );
#define NI_decode_str_array NI_decode_string_list

extern int NI_str_array_find( char *, NI_str_array *) ; /* 20 May 2010 */

extern int NI_count_numbers( int nstr , char **str ) ;  /* 11 Sep 2018 */

/*-------------------------------------------------------------------------*/
/*! An array of floats. */

typedef struct { int num; float *ar; } NI_float_array ;

#define NI_delete_float_array(far) \
  do{ if( (far)->ar != NULL ) NI_free((far)->ar); NI_free(far); } while(0)

extern NI_float_array * NI_decode_float_list( char *ss , char *sep ) ;
extern char *           NI_encode_float_list( NI_float_array *, char * ) ;
#define NI_decode_float_array NI_decode_float_list

/*-------------------------------------------------------------------------*/
/*! An array of ints. */

typedef struct { int num; int *ar; } NI_int_array ;

#define NI_delete_int_array(iar) \
  do{ if( (iar)->ar != NULL ) NI_free((iar)->ar); NI_free(iar); } while(0)

extern NI_int_array * NI_decode_int_list( char *ss , char* sep ) ;
extern char *         NI_encode_int_list( NI_int_array * , char * ) ;
#define NI_decode_int_array NI_decode_int_list

/*-------------------------------------------------------------------------*/
/* Registry stuff -- niml_registry.c [25 Feb 2005] */

extern void * NI_registry_malloc          ( char *, char *, size_t ) ;
extern void * NI_registry_realloc         ( void *, size_t ) ;
extern void   NI_registry_free            ( void * ) ;
extern void * NI_registry_idcode_to_ptr   ( char * ) ;
extern char * NI_registry_idcode_to_name  ( char * ) ;
extern char * NI_registry_ptr_to_idcode   ( void * ) ;
extern char * NI_registry_ptr_to_name     ( void * ) ;
extern void   NI_registry_idcode_altername( char *, char * ) ;
extern void   NI_registry_ptr_altername   ( void *, char * ) ;
extern size_t NI_registry_idcode_to_len   ( char * ) ;
extern size_t NI_registry_ptr_to_len      ( void * ) ;
extern void * NI_registry_add             ( char *, char *, void * ) ;
extern void * NI_registry_replace         ( void *, void * ) ;

/*-------------------------------------------------------------------------*/

#define IDCODE_LEN 32
#define LEN_IDCODE IDCODE_LEN

#ifndef TYPEDEF_NI_datacontainer
#define TYPEDEF_NI_datacontainer
typedef struct {
  char type_name  [IDCODE_LEN] ; /* e.g., "NI_ELEMENT"   */
  char self_name  [IDCODE_LEN] ; /* e.g., "AFNI_dataset" */
  char self_idcode[IDCODE_LEN] ;
  int  ival , jval ;
  void *self_data ;              /* the actual data */
} NI_objcontainer ;

typedef int (*NI_objconverter_func)( NI_objcontainer * ) ;
#endif

extern char * NI_self_idcode( void * ) ;
extern void   NI_suck_stream( char *, int, int *, NI_objcontainer *** ) ;
extern void   NI_convert_elm_to_obj( NI_objcontainer * ) ;
extern void   NI_convert_obj_to_elm( NI_objcontainer * ) ;
extern void   NI_register_objconverters( char * ,
                                         NI_objconverter_func ,
                                         NI_objconverter_func  ) ;

#ifdef  __cplusplus
}
#endif

#endif /* _NIML_HEADER_FILE */
