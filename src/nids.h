#ifndef _NIDS_HEADERRR_
#define _NIDS_HEADERRR_

/*----------------------------------------------------------------------------
  NIDS = NeuroImaging DataSet

  This file defines various types, macros, and function prototypes for
  generic datasets and domains for neuroimaging applications.
------------------------------------------------------------------------------*/

#include "niml.h"   /* NIML stuff is required */

#define NIDS_free       NI_free
#define NIDS_malloc     NI_malloc
#define NIDS_realloc    NI_realloc
#define NIDS_strncpy    NI_strncpy
#define NIDS_strdup     NI_strdup
#define NIDS_strlen     NI_strlen
#define NIDS_filesize   NI_filesize
#define NIDS_clock_time NI_clock_time
#define NIDS_byteorder  NI_byteorder
#define NIDS_swap2      NI_swap2
#define NIDS_swap4      NI_swap4
#define NIDS_swap8      NI_swap8

#define NIDS_new(x)     (x *)NI_malloc(sizeof(x))

#ifndef TYPEDEF_NIDS_INDEX_T
#define TYPEDEF_NIDS_INDEX_T
typedef int NIDS_index_t ;      /* used to store indexes, vector lengths */
#endif

/*----------------------------------------------------------------------------*/
/*! Stuff that goes at the top of every NIDS struct:
     - type is a code that lets us tell what kind of struct it is
     - nref is a reference count
     - idcode is a globally unique string (max 63 characters)
     - name is an arbitrary string for fun, profit, and elucidation
     - either or both of these strings may be NULL
*/

#define NIDS_BASIC_ELEMENTS  \
  int type ;                 \
  int nref ;                 \
  char *idcode ;             \
  char *name

#define NIDS_type(nd)   ((nd)->type)
#define NIDS_nref(nd)   ((nd)->nref)
#define NIDS_idcode(nd) ((nd)->idcode)
#define NIDS_name(nd)   ((nd)->name)

/*----------------------------------------------------------------------------*/
/*! The minimal NIDS struct, with only the basic elements. */

typedef struct {
  NIDS_BASIC_ELEMENTS ;
} NIDS_struct ;

extern void   NIDS_free_struct      ( void * ) ;
extern void * NIDS_copy_struct      ( void * ) ;
extern void * NIDS_pointto_struct   ( void * ) ;

extern void   NIDS_register_struct  ( void * ) ;
extern void * NIDS_find_struct      ( char * ) ;
extern void   NIDS_unregister_struct( void * ) ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold one float. */

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  float val ;
} NIDS_float_one ;

#define NIDS_float_val(nd) ((nd)->val)

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold the definition of a statistical distribution for
    a NIDS_vector (call it v):
      - statcode  = one of the NIDS_STAT_* codes
      - param_num = number of parameters for this distribution
      - param[i]  = parameter #i, for i=0..param_num-1;
                    - this will either be a NIDS_float_one, which means it
                      is constant for all elements of the vector, or
                    - this will be a NIDS_vector itself, of float type,
                      which means that the #i parameter for v[j] is stored
                      in param[i][j]
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  int statcode ;
  NIDS_index_t param_num ;
  NIDS_struct  **param ;
} NIDS_statistic ;

#define NIDS_stat_code(nd)      ((nd)->statcode)
#define NIDS_stat_param_num(nd) ((nd)->param_num)
#define NIDS_stat_param(nd,i,j)                             \
  ( ((nd)->param[i]->type == NIDS_FLOAT_ONE_TYPE)           \
     ? ( ((NIDS_float_one *)(nd)->param[i])->val )          \
     : ( ((NIDS_float_vector *)(nd)->param[i])->vec[j] ) )

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold a vector of values:
     - vec_len   = number of values
     - vec_typ   = type of values (e.g., NIDS_FLOAT, etc.)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t vec_len ;
  int vec_typ ;
  void *vec ;
  void *vec_range ;
  NIDS_statistic *statistic ;
} NIDS_vector ;

extern void * NIDS_new_vector( int , NIDS_index_t ) ;
extern void   NIDS_set_vector_range( void * ) ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold a vector of byte values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NIDS_BYTE)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t vec_len ;
  int vec_typ ;
  byte *vec ;
  byte *vec_range ;
  NIDS_statistic *statistic ;
} NIDS_byte_vector ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold a vector of short values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NIDS_SHORT)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t vec_len ;
  int vec_typ ;
  short *vec ;
  short *vec_range ;
  NIDS_statistic *statistic ;
} NIDS_short_vector ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold a vector of int values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NIDS_INT)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t vec_len ;
  int vec_typ ;
  int *vec ;
  int *vec_range ;
  NIDS_statistic *statistic ;
} NIDS_int_vector ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold a vector of float values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NIDS_FLOAT)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t vec_len ;
  int vec_typ ;
  float *vec ;
  float *vec_range ;
  NIDS_statistic *statistic ;
} NIDS_float_vector ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold a vector of double values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NIDS_DOUBLE)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t vec_len ;
  int vec_typ ;
  double *vec ;
  double *vec_range ;
  NIDS_statistic *statistic ;
} NIDS_double_vector ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold a vector of complex values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NIDS_COMPLEX)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t vec_len ;
  int vec_typ ;
  complex *vec ;
  complex *vec_range ;
  NIDS_statistic *statistic ;
} NIDS_complex_vector ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold a vector of rgb values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NIDS_RGB)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t vec_len ;
  int vec_typ ;
  rgb *vec ;
  rgb *vec_range ;
  NIDS_statistic *statistic ;
} NIDS_rgb_vector ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold a vector of rgba values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NIDS_RGBA)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t vec_len ;
  int vec_typ ;
  rgba *vec ;
  rgba *vec_range ;
  NIDS_statistic *statistic ;
} NIDS_rgba_vector ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold a vector of string values:
     - vec_len   = number of values
     - vec_typ   = type of values (must be NIDS_STRING)
     - vec       = pointer to array of data of length vec_len
     - vec_range = pointer to array of length 2 (if not NULL):
                   - vec_range[0] = smallest value in vec
                   - vec_range[1] = largest value in vec
     - statistic = defines statistical distribution for these values
                   (if not NULL)
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t vec_len ;
  int vec_typ ;
  char **vec ;
  char **vec_range ;
  NIDS_statistic *statistic ;
} NIDS_string_vector ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to define a coordinate mapping between one 3D domain
    and another.
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  float mat[4][4] ;
} NIDS_affine_3dmap ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to define a 1..4 dimensional rectangular domain:
     - nx,ny,nz,nt = number of voxels along each axis
     - nvox        = total number of voxels
     - dx,dy,dz,dt = grid spacing along each axis
     - xo,yo,zo,to = origin of each axis
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t nx,ny,nz,nt , nvox ;
  float dx,dy,dz,dt ;
  float xo,yo,zo,to ;
} NIDS_rect_domain ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to define a domain of scattered points:
     - num_node = number of nodes (points)
     - id       = list of integer node identifiers
     - x,y,z    = list of spatial coordinates
     - seq      = If 1, node id's are sequential
     - seqbase  = If id's are sequential, is smallest id
     - sorted   = If 1, id's are sorted into increasing order
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t  num_node ;
  NIDS_index_t *id ;
  float        *x , *y , *z ;
  int           seq ;
  int           seqbase ;
  int           sorted ;
} NIDS_points_domain ;

/*----------------------------------------------------------------------------*/
/*! NIDS struct to hold a generic dataset, which is a collection of value
    vectors defined over a common domain.
      - num_node = number of nodes in the domain
      - num_val  = number of values at each node
      - order    = code indicated whether the value vectors are
                   along the node direction or value index direction
      - vec[i]   = i-th value vector
      - domain   = definition of domain the nodes occupy (if not NULL)
*/

typedef struct {
  NIDS_BASIC_ELEMENTS ;
  NIDS_index_t num_node , num_val ;
  int order ;
  NIDS_vector **vec ;
  NIDS_struct  *domain ;
} NIDS_dataset ;

#define NIDS_NODE_DIRECTION  55   /* for the order element */
#define NIDS_INDEX_DIRECTION 56

#define NIDS_dataset_vecnum(nd)  \
  ( ((nd)->order == NIDS_NODE_DIRECTION) ? (nd)->num_val : (nd)->num_node )

#define NIDS_dataset_veclen(nd)  \
  ( ((nd)->order == NIDS_NODE_DIRECTION) ? (nd)->num_node: (nd)->num_val  )

#define NIDS_opposite_order(oo)  \
  ( ((oo) == NIDS_NODE_DIRECTION) ? NIDS_INDEX_DIRECTION : NIDS_NODE_DIRECTION )

extern void * NIDS_dataset_transpose( void * ) ;

/*----------------------------------------------------------------------------*/
/* Codes for the "type" element of a NIDS struct. */

#define NIDS_STRUCT_TYPE          6660000
#define NIDS_FLOAT_ONE_TYPE       6660002
#define NIDS_STATISTIC_TYPE       6660003
#define NIDS_DATASET_TYPE         6660004

#define NIDS_VECTOR_TYPE          6660100
#define NIDS_BYTE_VECTOR_TYPE     6660101
#define NIDS_SHORT_VECTOR_TYPE    6660102
#define NIDS_INT_VECTOR_TYPE      6660103
#define NIDS_FLOAT_VECTOR_TYPE    6660104
#define NIDS_DOUBLE_VECTOR_TYPE   6660105
#define NIDS_COMPLEX_VECTOR_TYPE  6660106
#define NIDS_RGB_VECTOR_TYPE      6660107
#define NIDS_RGBA_VECTOR_TYPE     6660108
#define NIDS_STRING_VECTOR_TYPE   6660109

#define NIDS_is_vector_type(tt)                                 \
 ( (tt) >= NIDS_VECTOR_TYPE && (tt) <= NIDS_STRING_VECTOR_TYPE )

#define NIDS_patch_vector_type(nn)                              \
 do{ if( NIDS_is_vector_type((nn)->type) )                      \
       (nn)->type = NIDS_VECTOR_TYPE + (nn)->vec_type + 1 ;     \
 } while(0)

#define NIDS_RECT_DOMAIN_TYPE     6660201
#define NIDS_POINTS_DOMAIN_TYPE   6660202

#define NIDS_is_domain_type(tt)                                 \
 ( (tt) >= NIDS_RECT_DOMAIN_TYPE && (tt) <= NIDS_POINTS_DOMAIN_TYPE )

#define NIDS_AFFINE_3DMAP_TYPE    6660301

#define NIDS_is_3dmap_type(tt)                                  \
 ( (tt) >= NIDS_AFFINE_3DMAP_TYPE && (tt) <= NIDS_AFFINE_3DMAP_TYPE )

/*--- Statistical type codes (3..10 match AFNI's 3ddata.h) ---*/

#define NIDS_STAT_TTEST       3   /* DOF */
#define NIDS_STAT_FTEST       4   /* 2 DOF */
#define NIDS_STAT_ZSCORE      5   /* no params */
#define NIDS_STAT_CHISQ       6   /* DOF */
#define NIDS_STAT_BETA        7   /* a and b params */
#define NIDS_STAT_BINOM       8   /* # trials, p per trial */
#define NIDS_STAT_GAMMA       9   /* shape, scale params */
#define NIDS_STAT_POISSON    10   /* mean */
#define NIDS_STAT_NORMAL     11   /* mean, variance */
#define NIDS_STAT_FTEST_NONC 12   /* 2 DOF, noncentrality */
#define NIDS_STAT_CHISQ_NONC 13   /* DOF, noncentrality */

/*--- Data type codes (0..8 match niml.h; 0..7 match AFNI's mrilib.h) ---*/

#define NIDS_BYTE        NI_BYTE           /* == AFNI MRI_byte */
#define NIDS_SHORT       NI_SHORT          /* == AFNI MRI_short */
#define NIDS_INT         NI_INT            /* == AFNI MRI_int */
#define NIDS_FLOAT32     NI_FLOAT32        /* == AFNI MRI_float */
#define NIDS_FLOAT       NIDS_FLOAT32
#define NIDS_FLOAT64     NI_FLOAT64        /* == AFNI MRI_double */
#define NIDS_DOUBLE      NIDS_FLOAT64
#define NIDS_COMPLEX64   NI_COMPLEX64      /* == AFNI MRI_complex */
#define NIDS_COMPLEX     NIDS_COMPLEX64
#define NIDS_RGB         NI_RGB            /* == AFNI MRI_rgb */
#define NIDS_RGBA        NI_RGBA           /* == AFNI MRI_rgba */
#define NIDS_STRING      NI_STRING         /* no AFNI type for string */

#define NIDS_FIRST_DATATYPE  NIDS_BYTE
#define NIDS_LAST_DATATYPE   NIDS_STRING

extern int NIDS_datatype_size( int ) ;

/*---*/

#endif /* _NIDS_HEADERRR_ */
