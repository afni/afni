/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#ifndef _MULTIVECTOR_HEADER_
#define _MULTIVECTOR_HEADER_

#include <ctype.h>
#include "mrilib.h"

#define MV_FLOAT  1
#define MV_STRING 2

#define MV_TYPELABEL(i) (((i)==MV_FLOAT)  ? "FLOAT"  :           \
                         ((i)==MV_STRING) ? "STRING" : "unknown")

typedef struct {
   int nvec , ndim ;
   char * name ;
   int * type ;      /* [nvec] */
   char ** label ;   /* [nvec] */
   void ** vec ;     /* [nvec][ndim] */
} multivector ;

#define MV_NVEC(m)           ((m)->nvec)
#define MV_NDIM(m)           ((m)->ndim)

#define MV_FLOAT_VEC(m,i)    ((float *)(m)->vec[(i)])
#define MV_FLOAT_VAL(m,i,j)  (((float *)(m)->vec[(i)])[(j)])

#define MV_STRING_VEC(m,i)   ((char **)(m)->vec[(i)])
#define MV_STRING_VAL(m,i,j) (((char **)(m)->vec[(i)])[(j)])

#define MV_TYPE(m,i)         ((m)->type[(i)])
#define MV_LABEL(m,i)        (((m)->label != NULL) ? (m)->label[(i)] : NULL)
#define MV_NAME(m)           ((m)->name)

extern multivector * multivector_read( char * ) ;
extern int           multivector_write( char * , multivector * ) ;
extern void          multivector_free( multivector * mv ) ;
extern void          multivector_set_name( multivector * , char * ) ;

extern char * MV_format_fval( float ) ;  /* 12 July 01999 */

#define MV_FREE(m) do{ multivector_free((m)); (m)=NULL; }while(0)

#endif /* _MULTIVECTOR_HEADER_ */
