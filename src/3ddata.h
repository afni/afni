/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*! \file
   This file contains the definition of the structs, macros, etc. for AFNI datasets.
*/

#ifndef _MCW_3DDATASET_
#define _MCW_3DDATASET_

/**                                  yyyy-mm-dd  **/
#define DSET_VERSION_LATEST         "1996-07-10"
#define DSET_VERSION_COMPARE(v1,v2) strcmp(v1,v2)

/** #include <dirent.h> **/
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <ctype.h>

#include <X11/Intrinsic.h>

#include "mcw_malloc.h"

#include "killer.h"
#include "vecmat.h"
#include "machdep.h"
#include "mrilib.h"
#include "tagset.h"

#include "thd_compress.h"

#include "nifti1_io.h"   /* 06 Dec 2005 */

#ifndef myXtFree
/*! Macro to free a pointer and NULL-ize it as well. */
#define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

#ifndef myXtNew
/*! Macro to allocate memory and zero-ize it. */
#define myXtNew(type) ((type *) XtCalloc(1,(unsigned) sizeof(type)))
#endif

struct THD_3dim_dataset ;  /* incomplete definition */

#include "niml.h"          /* NIML */
#include "afni_suma.h"     /* SUrface MApper */

#ifdef  __cplusplus
extern "C" {
#endif

/*! Enables compilation of the MINC dataset code. */

#define ALLOW_MINC   /* 29 Oct 2001 */

/*! Macro to check if string ss ends in string suf. */

#define STRING_HAS_SUFFIX(ss,suf)              \
  ((ss != NULL) && (suf != NULL) &&            \
   (strlen(ss) >= strlen(suf))   &&            \
   (strcmp(ss+strlen(ss)-strlen(suf),suf) == 0))

#define STRING_HAS_SUFFIX_CASE(ss,suf)         \
  ((ss != NULL) && (suf != NULL) &&            \
   (strlen(ss) >= strlen(suf))   &&            \
   (strcasecmp(ss+strlen(ss)-strlen(suf),suf) == 0))

#define PREFIX_IS_NIFTI(ss) ( STRING_HAS_SUFFIX(ss,".nii")    ||  \
                              STRING_HAS_SUFFIX(ss,".nii.gz") ||  \
                              STRING_HAS_SUFFIX(ss,".hdr")      )

/***************************** dimensions ***************************/

/*! Max length of a dataset label. */

#define THD_MAX_LABEL     38

/*! Max length of a dataset prefix. */

#define THD_MAX_PREFIX     (255+1)  /* must be more than THD_MAX_LABEL
                                    (  ZSS Jan 07 need room for path specified
                                       with prefix on command line   (why that +1,
                                       I don't know) ) */

/*! Max length of a "name" of a file, or stuff like that. */

#define THD_MAX_NAME      (256+THD_MAX_PREFIX)     /* (ZSS Jan 07)*/

/*! Max length of a dataset view code (+orig, etc). */

#define THD_MAX_VIEWCODE   (4+1)

/*! Max length of a dataset suffix (BRIK, etc). */

#define THD_MAX_SUFFIX     (4+1)

/*! Max length of a dataset filecode (prefix+view). */

#define THD_MAX_FILECODE   (THD_MAX_PREFIX+THD_MAX_VIEWCODE)

/*! Default label for a dataset.

    Labels aren't really used anymore, since the stupid users didn't like them
*/
#define THD_DEFAULT_LABEL "Viggo!"

/*! Max num datasets per session. */

#define THD_MAX_SESSION_SIZE  4096

/*! Max number of directories. */

#define THD_MAX_NUM_SESSION    80

#define THD_MAX_CHOICES THD_MAX_SESSION_SIZE

#define THD_MAX_MARKSET       5

#define FAIL    -1
#define SUCCESS  1

/*! General "type code" for invalid data.

    Various things are labeled with non-negative type codes (e.g., statistics types).
    Negative type codes indicate something is not valid.
*/

#define ILLEGAL_TYPE -666

/***************  generic function with no return value  **********************/

/*! Generic function type returning void. */

typedef void generic_func() ;

/*! Generic function type returning float. */

typedef float float_func() ;

/*! Stores a list of "registered" functions (e.g., "Transforms") */

typedef struct {
   int num ;                     /*!< number of functions */
   int * flags ;                 /*!< flags[i] = bitmask flag for function #i */
   char ** labels ;              /*!< labels[i] = string name for function #i */
   generic_func ** funcs ;       /*!< funcs[i] = function #i */

   void ** func_data ;           /*!< 30 Jan 2000 */
   int *   func_code ;

   generic_func ** func_init ;   /*!< 21 Jul 2003 */
} MCW_function_list ;

/*! MCW_function_list possible bitmask flag */
#define RETURNS_STRING    1

/*! MCW_function_list possible bitmask flag */
#define NEEDS_DSET_INDEX  2

/*! MCW_function_list possible bitmask flag */
#define PROCESS_MRI_IMAGE 4

/*! MCW_function_list possible bitmask flag */
#define SET_DPLOT_OVERLAY 8

/*! MCW_function_list possible func_code */
#define FUNC_0D   0
/*! MCW_function_list possible func_code */
#define FUNC_1D   1
/*! MCW_function_list possible func_code */
#define FUNC_2D   2
/*! MCW_function_list possible func_code */
#define FUNC_3D   3

/*! MCW_function_list possible func_code */
#define FUNC_FIM  71

/******************************** macros ******************************/

/*! First part of TWO_TWO macro. */

#define TWO_ONE(x,y) x ## y

/*! Combine two interpreted tokens into one using TWO_TWO. */

#define TWO_TWO(x,y) TWO_ONE(x,y)

/*! Zero out a variable */

#undef  ZZME
#define ZZME(x) memset(&(x),0,sizeof(x))

/*! Copy n units of the given type "type * ptr", into a structure "str",
     starting at byte offset "off";
   N.B.: str is the structure itself, not a pointer to it
         off is most easily computed with XtOffsetOf       */

#define COPY_INTO_STRUCT(str,off,type,ptr,n) \
   (void) memcpy( (char *)(&(str))+(off), (char *)(ptr), (n)*sizeof(type) )

/*! Copy n units of the given type "type * ptr", from a structure "str",
     starting at byte offset "off";
   N.B.: str is the structure itself, not a pointer to it
         off is most easily computed with XtOffsetOf       */

#define COPY_FROM_STRUCT(str,off,type,ptr,n) \
   (void) memcpy( (char *)(ptr), (char *)(&(str))+(off), (n)*sizeof(type) )

/*! Safe version of strncpy, which always leaves a NUL at the end.

    The standard stupid strncpy(dest,src,n) might not leave a NUL character
    at the end if the src string is too long.  This criminal behavior is
    reformed by this macro.
*/

#ifndef MCW_strncpy
#define MCW_strncpy(dest,src,n) \
   ( (void) strncpy( (dest) , (src) , (n)-1 ) , (dest)[(n)-1] = '\0' )
#endif

/*********************** dynamic array of XtPointers **********************/

#define IC_DSET 44301
#define IC_FLIM 55402

/*! Dynamically extendable array of XtPointer. */

typedef struct {
      int num ;          /*!< Number currently in use */
      int nall ;         /*!< Number currently allocated */
      XtPointer * ar ;   /*!< Array of pointers: [0..num-1] are valid */
      int * ic ;         /*!< added 26 Mar 2001 */
} XtPointer_array ;

/*! Increment for extending XtPointer_array allocation */

#define INC_XTARR 8

/*! Initialize dynamic XtPointer array named "name".

    You must declare "XtPointer_array *name;".
*/
#define INIT_XTARR(name)               \
   ( (name) = XtNew(XtPointer_array) , \
     (name)->num = (name)->nall = 0 ,  \
     (name)->ar  = NULL ,              \
     (name)->ic  = NULL   )

/*! Add a pointer to a dynamic XtPointer array. */

#define ADDTO_XTARR(name,bblk)                                 \
 do{ if( (name)->num == (name)->nall ){                        \
      (name)->nall += INC_XTARR + (name)->nall/8 ;             \
      (name)->ar    = (XtPointer *)                            \
                       XtRealloc( (char *) (name)->ar ,        \
                          sizeof(XtPointer) * (name)->nall ) ; \
      (name)->ic    = (int *) XtRealloc( (char *) (name)->ic , \
                          sizeof(int) * (name)->nall ) ;       \
     }                                                         \
     (name)->ar[(name)->num] = (XtPointer)(bblk) ;             \
     (name)->ic[(name)->num] = 0 ;                             \
     ((name)->num)++ ;                                         \
   } while(0)

/*! Number of good entries in a dynamic XtPointer array. */

#define XTARR_NUM(name)  ((name)->num)

/*! i-th entry in a dynamic XtPointer array. */

#define XTARR_XT(name,i) ((name)->ar[i])

#define XTARR_IC(name,i) ((name)->ic[i])

/*! Free a dynamic XtPointer array.
    But not what the pointers point to - that is a completely separate matter.
*/

#define FREE_XTARR(name)      \
   if( (name) != NULL ){      \
     myXtFree( (name)->ar ) ; \
     myXtFree( (name)->ic ) ; \
     myXtFree( (name) ) ;     \
     (name) = NULL ; }

/*! Duplicate definition for FREE_XTARR */
#define DESTROY_XTARR  FREE_XTARR

/************************* string array stuff *************************/

/*! Dynamic array of character strings. */

typedef struct {
      int num ;      /*!< Number of strings currently stored */
      int nall ;     /*!< Number of strings space is set aside for */
      char ** ar ;   /*!< Array of pointers to strings */
      KILL_list kl ; /*!< For semi-automatic memory cleanup */
} THD_string_array ;

/*! Return pointer to qq-th string in dynamic string array ss. */

#define SARR_STRING(ss,qq) ((ss)->ar[(qq)])

/*! Return number of strings stored in dynamic string array ss. */

#define SARR_NUM(ss)       ((ss)->num)

#define INC_SARR 64

/*! Initialize an empty dynamic string array named "name".

    You must declare "THD_string_array *name;".
*/

#define INIT_SARR(name)                 \
   ( (name) = XtNew(THD_string_array) , \
     (name)->num = (name)->nall = 0 ,   \
     (name)->ar  = NULL ,               \
     INIT_KILL((name)->kl) )

/*! Add string str to dynamic string array "name". */

#define ADDTO_SARR(name,str)                                          \
 do{ if( (name)->num == (name)->nall ){                               \
      (name)->nall += INC_SARR + (name)->nall/8 ;                     \
      (name)->ar    = (char **) XtRealloc( (char *) (name)->ar ,      \
                                 sizeof(char *) * (name)->nall ) ;    \
     }                                                                \
     if( (str) != NULL ){                                             \
      (name)->ar[(name)->num] = (char *) XtMalloc( strlen((str))+1 ); \
      strcpy( (name)->ar[(name)->num] , (str) ) ;                     \
      ADDTO_KILL((name)->kl,(name)->ar[(name)->num]) ;                \
      ((name)->num)++ ;                                               \
     } } while(0)

/*! Remove the ijk-th string from dynamic string array "name". */

#define REMOVEFROM_SARR(name,ijk)                \
 do{ SINGLE_KILL((name)->kl,(name)->ar[(ijk)]) ; \
     (name)->ar[(ijk)] = NULL ; } while(0)

/*! Kill all entries in the dynamic string array "name". */

#define DESTROY_SARR(name)    \
 do{ if( (name) != NULL ){    \
     KILL_KILL((name)->kl) ;  \
     myXtFree( (name)->ar ) ; \
     myXtFree( (name) ) ; } } while(0)

/*! Print all entries in a dynamic string array */

#define PRINTF_SARR(name,lll)                                            \
 do{ int qq ; printf("%s:",(lll)) ;                                      \
     for( qq=0; qq < (name)->num; qq++ ) printf(" '%s'",(name)->ar[qq]); \
     printf("\n") ; } while(0)

extern int SARR_find_string( THD_string_array * sar , char * str ) ;
extern int SARR_find_substring( THD_string_array * sar , char * sub ) ;

extern int SARR_lookfor_string   ( THD_string_array * sar , char * str , int nstart ) ;
extern int SARR_lookfor_substring( THD_string_array * sar , char * sub , int nstart ) ;

/*! Concatenate strings p1 and p2 into string pout, making them a filesystem path.

    If p1 doesn't end in a '/', the '/' between p1/p2 will be added.
    The pout array must be previously allocated.
*/

#define PATH_CONCAT(pout,p1,p2)                            \
  do{ int zq ; strcpy((pout),(p1)) ; zq = strlen((pout)) ; \
      if( (pout)[zq-1] != '/' ) strcat((pout),"/") ;       \
      strcat((pout),(p2)) ; } while(0)

/*************** dynamic array of sorted (x,y,z) points *************/

#undef  ALLOW_DATASET_VLIST
#ifdef  ALLOW_DATASET_VLIST

/*! Dynamic array of xyz and ijk points. */

typedef struct {
      int num ;                          /*!< Number of points currently in use */
      int nall ;                         /*!< Number of points currently allocated */
      THD_fvec3 * xyz ;                  /*!< Array of xyz coordinates in parent */
      THD_ivec3 * ijk ;                  /*!< Array of ijk indexes in parent */
      struct THD_3dim_dataset * parent ; /*!< Dataset these things come from */
} THD_vector_list ;

#define INC_VLIST 64

/*! Initialize a dynamic array of xyz points, attached to datset ddd. */

#define INIT_VLIST(name,ddd) \
   ( (name) = XtNew(THD_vector_list) ,  \
     (name)->num = (name)->nall = 0 ,   \
     (name)->xyz = NULL , (name)->ijk = NULL , \
     (name)->parent = (ddd) )

/*! Add 1 xyz-vector to the array of xyz points.

    The ijk-vector will be converted from the xyz coordinates,
    using the parent dataset for this array.
*/

#define ADD_FVEC_TO_VLIST(name,vec) \
   { if( (name)->num == (name)->nall ){                                    \
      (name)->nall += INC_VLIST ;                                          \
      (name)->xyz   = (THD_fvec3 * ) XtRealloc( (char *) (name)->xyz ,     \
                                      sizeof(THD_fvec3) * (name)->nall ) ; \
      (name)->ijk   = (THD_ivec3 * ) XtRealloc( (char *) (name)->ijk ,     \
                                      sizeof(THD_ivec3) * (name)->nall ) ; \
     }                                                                     \
     (name)->xyz[(name)->num] = (vec);                                     \
     (name)->ijk[(name)->num] = THD_3dmm_to_3dind((name)->parent,(vec)) ;  \
     ((name)->num)++; }

/*! Add one ijk-vector to the array of xyz points.

    The xyz-vector will be converted from the ijk indexes, using
    the parent dataset for this array.
*/

#define ADD_IVEC_TO_VLIST(name,vec) \
   { if( (name)->num == (name)->nall ){                                    \
      (name)->nall += INC_VLIST ;                                          \
      (name)->xyz   = (THD_fvec3 * ) XtRealloc( (char *) (name)->xyz ,     \
                                      sizeof(THD_fvec3) * (name)->nall ) ; \
      (name)->ijk   = (THD_ivec3 * ) XtRealloc( (char *) (name)->ijk ,     \
                                      sizeof(THD_ivec3) * (name)->nall ) ; \
     }                                                                     \
     (name)->ijk[(name)->num] = (vec);                                     \
     (name)->xyz[(name)->num] = THD_3dind_to_3dmm((name)->parent,(vec)) ;  \
     ((name)->num)++; }

/*! Destroy an array of xyz points. */

#define DESTROY_VLIST(name)      \
   { if( (name) != NULL ){       \
       myXtFree( (name)->xyz ) ; \
       myXtFree( (name)->ijk ) ; \
       myXtFree( (name) ) ; } }

#endif /* ALLOW_DATASET_VLIST */

/**************************** typedefs ******************************/

/*---------- structure to hold attributes from disk files ----------*/

#define ATR_STRING_TYPE   0
#define ATR_FLOAT_TYPE    1
#define ATR_INT_TYPE      2

#define FIRST_ATR_TYPE 0
#define LAST_ATR_TYPE  2

/*! Things to look for in the .HEAD file; these define start of an attribute. */

static char * ATR_typestr[] = {
   "string-attribute" , "float-attribute" , "integer-attribute"
} ;

/*! Stores an integer-attribute (array of ints). */

typedef struct {
      int    type ;   /*!< should be ATR_INT_TYPE */
      char * name ;   /*!< name of attribute, read from HEAD file */
      int    nin ;    /*!< number of ints stored here */
      int  * in ;     /*!< array of ints stored here */
} ATR_int ;

/*! Stores a float-attribute (array of floats). */

typedef struct {
      int     type ;  /*!< should be ATR_FLOAT_TYPE */
      char *  name ;  /*!< name of attribute, read from HEAD file */
      int     nfl ;   /*!< number of floats stored here */
      float * fl ;    /*!< array of floats stored here */
} ATR_float ;

/*! Stores a string-attribute (array of strings). */

typedef struct {
      int    type ;   /*!< should be ATR_STRING_TYPE */
      char * name ;   /*!< name of attribute, read from HEAD file */
      int    nch ;    /*!< number of characters in string */
      char * ch ;     /*!< array of characters (may not be NUL terminated) */
} ATR_string ;

#define ZBLOCK 126
#define ZSBLOCK 59  /* kurukuru pa */
extern void THD_zblock(int,char *) ;   /* replace zeros with ZBLOCKs */
extern void THD_unzblock(int,char *) ; /* undo the above */
extern void THD_zblock_ch(int,char *,char) ;   /* 12 Jul 2006 [rickr] */
extern void THD_unzblock_ch(int,char *,char) ; /* undo the above      */

/*! Union type to hold an arbitrary attribute. */

typedef union {
      int          type ;      /*!< Determines type of data here */
      ATR_string   str_atr ;
      ATR_float    flo_atr ;
      ATR_int      int_atr ;
} ATR_any ;

/*---------------------------------------------------------------------*/
/*-------------------- structure for linear mapping -------------------*/

#define MAPPING_LINEAR_TYPE 0
#define MAPPING_LINEAR_STR  "LINEAR_MAPPING"

#define FIRST_MAPPING_TYPE 0
#define LAST_MAPPING_TYPE  0

static char * MAPPING_typestr[] = {
   MAPPING_LINEAR_STR
} ;

/*! Structure to hold a linear mapping between coordinate systems. */

typedef struct {
      int type ;            /*!< type code: only type now is MAPPING_LINEAR_TYPE */

      THD_mat33 mfor ;      /*!< x_map = [mfor] * x_in  - bvec  */
      THD_mat33 mbac ;      /*!< x_in  = [mbac] * x_map - svec  */

      THD_fvec3 bvec;       /* x_map = [mfor] * x_in  - bvec  */
      THD_fvec3 svec;       /* svec = - [mbac] * bvec */
      THD_fvec3 bot ;       /* lower bound for transformation use */
      THD_fvec3 top ;       /* upper bound for transformation use */
} THD_linear_mapping ;

/*! Copy the .bot and .top bounds between two THD_linear_mapping structs. */

#define COPY_LMAP_BOUNDS(m1,m2) ( (m1).bot=(m2).bot , (m1).top=(m2).top )

/*! Use the matrix operations to define a macro
    to load the inverse to a THD_linear_mapping once the forward is done. */

#define LOAD_INVERSE_LMAP(map) \
   ( (map).mbac = MAT_INV((map).mfor) ,          \
     (map).svec = MATVEC((map).mbac,(map).bvec) ,\
     NEGATE_FVEC3((map).svec) )

#define MAPPING_LINEAR_FSTART XtOffsetOf(THD_linear_mapping,mfor)
#define MAPPING_LINEAR_FEND   (XtOffsetOf(THD_linear_mapping,top)+sizeof(THD_fvec3))
#define MAPPING_LINEAR_FSIZE  ((MAPPING_LINEAR_FEND-MAPPING_LINEAR_FSTART)/sizeof(float))

/*! Debugging printout of a THD_linear_mapping struct. */

#define DUMP_LMAP(m) \
( printf("THD_linear_mapping:\n") ,                                     \
  printf("   mfor = %8.4f %8.4f %8.4f\n",                               \
         (m).mfor.mat[0][0], (m).mfor.mat[0][1], (m).mfor.mat[0][2] ) , \
  printf("          %8.4f %8.4f %8.4f\n",                               \
         (m).mfor.mat[1][0], (m).mfor.mat[1][1], (m).mfor.mat[1][2] ) , \
  printf("          %8.4f %8.4f %8.4f\n",                               \
         (m).mfor.mat[2][0], (m).mfor.mat[2][1], (m).mfor.mat[2][2] ) , \
  printf("   mbac = %8.4f %8.4f %8.4f\n",                               \
         (m).mbac.mat[0][0], (m).mbac.mat[0][1], (m).mbac.mat[0][2] ) , \
  printf("          %8.4f %8.4f %8.4f\n",                               \
         (m).mbac.mat[1][0], (m).mbac.mat[1][1], (m).mbac.mat[1][2] ) , \
  printf("          %8.4f %8.4f %8.4f\n",                               \
         (m).mbac.mat[2][0], (m).mbac.mat[2][1], (m).mbac.mat[2][2] ) , \
  printf("   bvec = %8.4f %8.4f %8.4f\n",                               \
         (m).bvec.xyz[0] , (m).bvec.xyz[1] , (m).bvec.xyz[2] ) ,        \
  printf("   svec = %8.4f %8.4f %8.4f\n",                               \
         (m).svec.xyz[0] , (m).svec.xyz[1] , (m).svec.xyz[2] ) ,        \
  printf("   bot  = %8.4f %8.4f %8.4f\n",                               \
         (m).bot.xyz[0]  , (m).bot.xyz[1]  , (m).bot.xyz[2] )  ,        \
  printf("   top  = %8.4f %8.4f %8.4f\n\n",                             \
         (m).top.xyz[0]  , (m).top.xyz[1]  , (m).top.xyz[2] ) )

/*-----------------------------------------------------------------*/
/*--------------- structure for user placed markers ---------------*/

#define MARKS_MAXNUM  10
#define MARKS_MAXLAB  20
#define MARKS_MAXHELP 256
#define MARKS_MAXFLAG 8

/*! Structure for user placed markers. */

typedef struct {
     int numdef ;                             /*!< Number of markers defined */
     int numset ;                             /*!< Number of markers now set */

     char label[MARKS_MAXNUM][MARKS_MAXLAB] ; /*!< Names for these marks */

     char help[MARKS_MAXNUM][MARKS_MAXHELP] ; /*!< Help for these marks */

     int ovcolor[MARKS_MAXNUM] ;              /*!< Overlay color index; -1 --> use defaults */

     Boolean valid[MARKS_MAXNUM] ;            /*!< True if actually set */

     float xyz[MARKS_MAXNUM][3] ;             /*!< Coordinates (3dmm, not DICOM) */

     int aflags[MARKS_MAXFLAG] ;              /*!< Action flags */

     int type ;                               /*!< Type of markers (same as aflags[0]) */
     char name[MARKS_MAXLAB] ;                /*!< Name of this type of markers */
} THD_marker_set ;

#define MARKS_FSIZE  (MARKS_MAXNUM*3)
#define MARKS_FSTART XtOffsetOf(THD_marker_set,xyz)

#define MARKS_LSIZE  (MARKS_MAXNUM*MARKS_MAXLAB)
#define MARKS_LSTART XtOffsetOf(THD_marker_set,label)

#define MARKS_HSIZE  (MARKS_MAXNUM*MARKS_MAXHELP)
#define MARKS_HSTART XtOffsetOf(THD_marker_set,help)

#define MARKS_ASIZE  MARKS_MAXFLAG
#define MARKS_ASTART XtOffsetOf(THD_marker_set,aflags)

/*--------------- definitions for markers I know about now ---------------*/

#define MARKSET_ALIGN    1  /* types of marker sets */
#define MARKSET_BOUNDING 2

#define MARKACTION_NONE     0  /* action codes for marker sets */
#define MARKACTION_WARP     1
#define MARKACTION_REGISTER 2  /* not used at present */

/*........................................................................*/

/*! Number of orig->acpc markers. */

#define NMARK_ALIGN 5

static int THD_align_aflags[MARKS_MAXFLAG] = {
  MARKSET_ALIGN , MARKACTION_WARP
} ;

#define IMARK_ACSE 0
#define IMARK_ACPM 1
#define IMARK_PCIE 2
#define IMARK_MSA1 3
#define IMARK_MSA2 4

/*! Labels for orig->acpc markers. */

static char * THD_align_label[NMARK_ALIGN] = {
   "AC superior edge"     ,
   "AC posterior margin"  ,
   "PC inferior edge"     ,
   "First mid-sag pt"     ,
   "Another mid-sag pt"
} ;

/*! Help for orig->acpc markers. */

static char * THD_align_help[NMARK_ALIGN] = {
   "This is the uppermost point\n"
   "on the anterior commisure,\n"
   "in the mid-sagittal plane." ,

   "This is the rearmost point\n"
   "on the anterior commisure,\n"
   "in the mid-sagittal plane.\n"
   "[Just a couple mm behind and\n"
   " below the AC superior edge.]" ,

   "This is the bottommost point\n"
   "on the posterior commissure,\n"
   "in the mid-sagittal plane." ,

   "You must also specify two other points in the\n"
   "mid-sagittal plane, ABOVE the corpus callosum\n"
   "(i.e., in the longitudinal fissure).  These\n"
   "points are needed to define the vertical plane." ,

   "You must also specify two other points in the\n"
   "mid-sagittal plane, ABOVE the corpus callosum\n"
   "(i.e., in the longitudinal fissure).  These\n"
   "points are needed to define the vertical plane." ,

} ;

/*.....................................................................*/

/*! Number of acpc->tlrc markers. */

#define NMARK_BOUNDING 6

static int THD_bounding_aflags[MARKS_MAXFLAG] = {
   MARKSET_BOUNDING , MARKACTION_WARP
} ;

#define IMARK_MANT 0
#define IMARK_MPOS 1
#define IMARK_MSUP 2
#define IMARK_MINF 3
#define IMARK_MLEF 4
#define IMARK_MRIG 5

/*! Atlas distances for acpc->tlrc markers.
    If you change these, change the helps below too */

#define ATLAS_FRONT_TO_AC 70.0
#define ATLAS_AC_TO_PC    23.0
#define ATLAS_PC_TO_BACK  79.0

#define ATLAS_BOT_TO_AC   42.0
#define ATLAS_AC_TO_TOP   74.0
#define ATLAS_AC_TO_LAT   68.0

#define ATLAS_BBOX_LAT    80.0    /* dimensions used for         */
#define ATLAS_BBOX_ANT    80.0    /* Talairach view clipping box */
#define ATLAS_BBOX_POS   110.0
#define ATLAS_BBOX_INF    55.0
#define ATLAS_BBOX_SUP    85.0

#define ATLAS_BBOX_INF_NEW 65.0   /* 3/06/96: extra 10 mm for cerebellum */

#define ATLAS_ALIGNBOX_LAT  95.0  /* dimensions used for AC-PC */
#define ATLAS_ALIGNBOX_ANT  95.0  /* aligned view clipping box */
#define ATLAS_ALIGNBOX_POS 140.0  /* (3/25/95)                 */
#define ATLAS_ALIGNBOX_SUP 100.0
#define ATLAS_ALIGNBOX_INF  70.0

#define MAX_ALLOWED_DEVIATION 2.0
#define MIN_ALLOWED_DEVIATION 0.5

/*! Labels for acpc->tlrc markers. */

static char * THD_bounding_label[NMARK_BOUNDING] = {
   "Most anterior point"  ,
   "Most posterior point" ,
   "Most superior point"  ,
   "Most inferior point"  ,
   "Most left point"      ,
   "Most right point"
} ;

/*! Help for acpc->tlrc markers. */

static char * THD_bounding_help[NMARK_BOUNDING] = {
"The frontmost point of the frontal cortex;\n"
"needed for brain length [atlas y = -70 mm]" ,

"The hindmost point of the occipital cortex;\n"
"needed for brain length [atlas y = +102 mm]" ,

"The topmost point of the parietal cortex;\n"
"needed for brain height [atlas z = +74 mm]" ,

"The lowest point of the temporal cortex;\n"
"needed for brain height [atlas z = -42 mm]" ,

"The most lateral (left) point of the parietotemporal cortex;\n"
"needed for brain width [atlas x = +68 mm]" ,

"The most lateral (right) point of the parietotemporal cortex;\n"
"needed for brain width [atlas x = -68 mm]"

} ;

/*---------------------------------------------------------------------*/
/*---------------------- structures to hold warps ---------------------*/

#define WARP_AFFINE_TYPE   0
#define WARP_AFFINE_STR    "WARP_AFFINE"

#define WARP_TALAIRACH_12_TYPE 1
#define WARP_TALAIRACH_12_STR  "WARP_TALAIRACH_12"

#define FIRST_WARP_TYPE 0
#define LAST_WARP_TYPE  1

static char * WARP_typestr[] = {
   WARP_AFFINE_STR , WARP_TALAIRACH_12_STR
} ;

/*----------------------- resample types -----------------------*/

#define RESAM_NN_TYPE      0
#define RESAM_NN_STR      "Nearest Neighbor"

#define RESAM_LINEAR_TYPE  1
#define RESAM_LINEAR_STR  "Linear Interpolation"

#define RESAM_CUBIC_TYPE   2
#define RESAM_CUBIC_STR   "Cubic Interpolation"

#define RESAM_BLOCK_TYPE   3
#define RESAM_BLOCK_STR   "Blocky Interpolation"

#define FIRST_RESAM_TYPE 0
#define LAST_RESAM_TYPE  3

static char * RESAM_typestr[] = {
   RESAM_NN_STR , RESAM_LINEAR_STR , RESAM_CUBIC_STR , RESAM_BLOCK_STR
} ;

#define NSTR_SHORT_RESAM 2
static char * RESAM_shortstr[] = { "NN" , "Li" , "Cu" , "Bk" } ;

/*! 12-piece Warp struct for orig/acpc -> tlrc coordinates. */

typedef struct {
      int type ;       /*!< type code: WARP_TALAIRACH_12_TYPE */
      int resam_type ; /*!< Resampling method */

      THD_linear_mapping warp[12] ; /* The 12 pieces of the transformation */
} THD_talairach_12_warp ;

#define W_RAS  0  /* right-anterior -superior mapping index */
#define W_LAS  1  /* left -anterior -superior */
#define W_RMS  2  /* right-medial   -superior */
#define W_LMS  3  /* left -medial   -superior */
#define W_RPS  4  /* right-posterior-superior */
#define W_LPS  5  /* left -posterior-superior */
#define W_RAI  6  /* right-anterior -inferior */
#define W_LAI  7  /* left -anterior -inferior */
#define W_RMI  8  /* right-medial   -inferior */
#define W_LMI  9  /* left -medial   -inferior */
#define W_RPI 10  /* right-posterior-inferior */
#define W_LPI 11  /* left -posterior-inferior */

#define WARP_TALAIRACH_12_SIZE (12*MAPPING_LINEAR_FSIZE)

/*! Debug printout for 1 piece of a Talairach warp. */

#define DUMP_T12_MAP(t12,xx,yy,zz) \
 (  printf("\n--- submap " # xx # yy # zz "\n" ) , \
    DUMP_LMAP( (t12).warp[W_ ## xx ## yy ## zz] )    )

/*! Debug printout for all 12 pieces of a Talairach warp. */

#define DUMP_T12_WARP(t12) \
 ( printf("\n12 region Talairach warp:") ,                 \
   DUMP_T12_MAP((t12),R,A,S) , DUMP_T12_MAP((t12),L,A,S) , \
   DUMP_T12_MAP((t12),R,M,S) , DUMP_T12_MAP((t12),L,M,S) , \
   DUMP_T12_MAP((t12),R,P,S) , DUMP_T12_MAP((t12),L,P,S) , \
   DUMP_T12_MAP((t12),R,A,I) , DUMP_T12_MAP((t12),L,A,I) , \
   DUMP_T12_MAP((t12),R,M,I) , DUMP_T12_MAP((t12),L,M,I) , \
   DUMP_T12_MAP((t12),R,P,I) , DUMP_T12_MAP((t12),L,P,I)    )

/*! Struct to hold a simple affine warp (orig -> acpc). */

typedef struct {
      int type ;         /*!< type code: WARP_AFFINE_TYPE */
      int resam_type ;   /*!< Resampling method */

      THD_linear_mapping warp ; /*!< The single affine mapping */
} THD_affine_warp ;

#define WARP_AFFINE_SIZE (MAPPING_LINEAR_FSIZE)

/*! Union type to hold all possible warp types. */

typedef union {
      int type ;                      /*!< WARP_AFFINE_TYPE or WARP_TALAIRACH_12_TYPE */
      THD_affine_warp       rig_bod ;
      THD_talairach_12_warp tal_12 ;
} THD_warp ;

/*! Check if ww is a good warp. */

#define ISVALID_WARP(ww) ( (ww) != NULL &&                  \
                           (ww)->type >= FIRST_WARP_TYPE && \
                           (ww)->type <= LAST_WARP_TYPE )

/*! Temporary warp. */

static THD_warp tempA_warp ;

/*! Return value is an affine warp set to the identity transformation. */

#define IDENTITY_WARP                                                   \
   ( tempA_warp.rig_bod.type       = WARP_AFFINE_TYPE ,                 \
     tempA_warp.rig_bod.resam_type = RESAM_NN_TYPE ,                    \
     tempA_warp.rig_bod.warp.type  = MAPPING_LINEAR_TYPE ,              \
     LOAD_DIAG_MAT( tempA_warp.rig_bod.warp.mfor ,     1,    1,    1 ) ,\
     LOAD_DIAG_MAT( tempA_warp.rig_bod.warp.mbac ,     1,    1,    1 ) ,\
     LOAD_FVEC3(    tempA_warp.rig_bod.warp.bvec ,     0,    0,    0 ) ,\
     LOAD_FVEC3(    tempA_warp.rig_bod.warp.svec ,     0,    0,    0 ) ,\
     LOAD_FVEC3(    tempA_warp.rig_bod.warp.bot  , -9999,-9999,-9999 ) ,\
     LOAD_FVEC3(    tempA_warp.rig_bod.warp.top  ,  9999, 9999, 9999 ) ,\
     tempA_warp )

/*! Return values is a warp of angle th about axis ff, in aa-bb plane. */

#define ROTGEN_WARP(th,ff,aa,bb)                                        \
   ( tempA_warp.rig_bod.type       = WARP_AFFINE_TYPE ,                 \
     tempA_warp.rig_bod.resam_type = RESAM_NN_TYPE ,                    \
     tempA_warp.rig_bod.warp.type  = MAPPING_LINEAR_TYPE ,              \
     LOAD_ROTGEN_MAT(tempA_warp.rig_bod.warp.mfor, th,ff,aa,bb) ,       \
     LOAD_ROTGEN_MAT(tempA_warp.rig_bod.warp.mbac,-th,ff,aa,bb) ,       \
     LOAD_FVEC3(     tempA_warp.rig_bod.warp.bvec,     0,    0,    0 ) ,\
     LOAD_FVEC3(     tempA_warp.rig_bod.warp.svec,     0,    0,    0 ) ,\
     LOAD_FVEC3(     tempA_warp.rig_bod.warp.bot , -9999,-9999,-9999 ) ,\
     LOAD_FVEC3(     tempA_warp.rig_bod.warp.top ,  9999, 9999, 9999 ) ,\
     tempA_warp )

#define ROTX_WARP(th) ROTGEN_WARP(th,0,1,2)
#define ROTY_WARP(th) ROTGEN_WARP(th,1,2,0)
#define ROTZ_WARP(th) ROTGEN_WARP(th,2,0,1)

/*! Make the affine warp map point (xin,yin,zin) to (xout,yout,zout). */

#define CEN_WARP(ww,xin,yin,zin,xout,yout,zout)                  \
  do{ THD_fvec3 tv , uv ;                                        \
      LOAD_FVEC3(tv,xin,yin,zin) ;                               \
      uv = MATVEC((ww).rig_bod.warp.mfor,tv) ;                   \
      LOAD_FVEC3(tv,xout,yout,zout) ;                            \
      (ww).rig_bod.warp.bvec = SUB_FVEC3(uv,tv) ;                \
      (ww).rig_bod.warp.svec =                                   \
         MATVEC((ww).rig_bod.warp.mbac,(ww).rig_bod.warp.bvec) ; \
      NEGATE_FVEC3((ww).rig_bod.warp.svec) ;                     \
  } while(0)

/*---------------------------------------------------------------------*/
/*----------- structure to hold pointer to data on disk ---------------*/

#define DISKPTR_TYPE  47

#define THD_MAX_RANK       3
#define THD_MIN_RANK       3
#define THD_MAX_RANK_EVER  5

/* none of these should be over 4 characters! */

#define DATASET_HEADER_SUFFIX "HEAD"
#define DATASET_BRICK_SUFFIX  "BRIK"
#define DATASET_NOTES_SUFFIX  "NOTE"

/***
  The following codes define how the data is stored on disk.
  At one time, I started to support more than one storage
  type, but that is history.  Data either isn't stored
  (i.e., is warped-on-demand), or is stored in one big
  brick file.

  Later: OK, now we have more than one type.  However, type #1
         was never implemented (the ill-fated STORAGE_BY_SLICES).

  see also: thd_opendset.c: storage_mode_from_filename()
***/

#define STORAGE_UNDEFINED         0
#define STORAGE_BY_BRICK          2
#define STORAGE_BY_MINC           3
#define STORAGE_BY_VOLUMES        4  /* 20 Jun 2002 */
#define STORAGE_BY_ANALYZE        5
#define STORAGE_BY_CTFMRI         6  /* 04 Dec 2002 */
#define STORAGE_BY_CTFSAM         7
#define STORAGE_BY_1D             8  /* 04 Mar 2003 */
#define STORAGE_BY_3D             9  /* 21 Mar 2003 */
#define STORAGE_BY_NIFTI         10  /* 28 Aug 2003 */
#define STORAGE_BY_MPEG          11  /* 03 Dec 2003 */
#define STORAGE_BY_NIML          12  /* NIML AFNI dset   25 May 2006 [rickr] */
#define STORAGE_BY_NI_SURF_DSET  13  /* NIML surface dset */
#define STORAGE_BY_GIFTI         14  /* GIFTI surface dset */

#define LAST_STORAGE_MODE        14

/*! Contains information about where/how dataset is stored on disk.

     The filenames in this structure are really path names
     (that is, they have the directory name prependend).
*/

typedef struct {
      int type ;                           /*!< must be DISKPTR_TYPE */
      int rank ;                           /*!< must be 3 */
      int nvals ;                          /*!< number of 3D volumes; must agree with THD_datablock */
      int dimsizes[THD_MAX_RANK] ;         /*!< size of each dimension of 3D array */
      int storage_mode ;                   /*!< one of the STORAGE_ codes  */

      int byte_order ;                     /*!< LSB_FIRST or MSB_FIRST [25 Apr 1998] */

      char prefix[THD_MAX_PREFIX] ;        /*!< prefix part of filename */
      char viewcode[THD_MAX_VIEWCODE] ;    /*!< viewcode part of filename */
      char filecode[THD_MAX_FILECODE] ;    /*!< filecode = prefix+viewcode */

      char directory_name[THD_MAX_NAME] ;  /*!< contain all files for this dataset */
      char header_name[THD_MAX_NAME] ;     /*!< contains attributes */
      char brick_name[THD_MAX_NAME] ;      /*!< THIS contains actual data volumes */
} THD_diskptr ;

#define ATRNAME_BYTEORDER "BYTEORDER_STRING"

extern void THD_delete_diskptr( THD_diskptr * ) ;

/*! Determine if THD_diskptr dk is valid. */

#define ISVALID_DISKPTR(dk) ( (dk)!=NULL && (dk)->type==DISKPTR_TYPE )

/*! Convert a file prefix and viewcode into a filecode (prefix+view). */

#define PREFIX_VIEW_TO_FILECODE(pr,vv,fc) sprintf( (fc),"%s+%s",(pr),(vv) )

/*! Extract the prefix from a filecode (prefix+view).

    - If there is no '+', puts an empty string into pr
    - Otherwise, scans backward from end to find last '+'; everything before that is the prefix
    - Space for pr must be allocated beforehand
*/

#define FILECODE_TO_PREFIX(fc,pr)                                     \
  do{ char *qq , *ff , *pp ;                                          \
      if( strstr((fc),"+") == NULL ){                                 \
         (pr)[0] = '\0' ;                                             \
      } else {                                                        \
         for( qq=fc+strlen((fc)) ; *qq != '+' ; qq-- ) ;              \
         for( ff=(fc) , (pp)=(pr) ; ff < qq ; ff++,pp++ ) *pp = *ff ; \
         *pp = '\0' ; } break ; } while(1)

#if 0
#define FILECODE_TO_PREFIX(fc,pr) \
  do{ char *qq , *ff , *pp ;               \
      if( (qq=strstr((fc),"+")) == NULL ){ \
         (pr)[0] = '\0' ;                  \
      } else {                             \
         for( ff=(fc) , (pp)=(pr) ; ff < qq ; ff++,pp++ ) *pp = *ff ; \
         *pp = '\0' ; } break ; } while(1)
#endif

/*! Extract the prefix from a filename. */

#define FILENAME_TO_PREFIX(fn,pr)             \
  do{ int ii ;                                \
      for( ii=strlen((fn)) ; ii >= 0 ; ii-- ) \
         if( (fn)[ii] == '/' ) break ;        \
      FILECODE_TO_PREFIX( (fn)+(ii+1) , (pr) ) ; break ; } while(1)

/*---------------------------------------------------------------------*/
/*------- structure to hold actual 3D data, or pointers thereto -------*/

#define DATABLOCK_TYPE 37

#define DATABLOCK_MEM_UNDEFINED  1
#define DATABLOCK_MEM_MALLOC     2
#define DATABLOCK_MEM_MMAP       4
#define DATABLOCK_MEM_ANY        (DATABLOCK_MEM_MALLOC | DATABLOCK_MEM_MMAP)
#define DATABLOCK_MEM_SHARED     8    /* 02 May 2003 */

/*! Determine if mm is a valid memory allocation code. */

#define ISVALID_MEM_CODE(mm) \
  ( (mm) == DATABLOCK_MEM_MALLOC || (mm) == DATABLOCK_MEM_MMAP \
                                 || (mm) == DATABLOCK_MEM_ANY  \
                                 || (mm) == DATABLOCK_MEM_SHARED )

#ifndef MMAP_THRESHOLD           /* if not previously defined in machdep.h */
/*! A brick file should have this many bytes before we try to use mmap */
#  define MMAP_THRESHOLD 999999
#endif

/*------------------------------------------------------------------*/
/* Stuff for volume-editing on demand.  [05 Sep 2006] */

#define VEDIT_NPARAM 9
typedef struct {
  int code , ival , flags ;
  float param[VEDIT_NPARAM] ;
} VEDIT_settings ;

#define VEDIT_CLUST  1   /* param= ithr,thr,rmm,vmul */
#define VEDIT_LASTCODE 1

#define VEDIT_IVAL(vv)      ((vv).ival)
#define DBLK_VEDIT_IVAL(db) VEDIT_IVAL((db)->vedset)
#define DSET_VEDIT_IVAL(ds) DBLK_VEDIT_IVAL((ds)->dblk)

#define VEDIT_CODE(vv)      ((vv).code)
#define DBLK_VEDIT_CODE(db) VEDIT_CODE((db)->vedset)
#define DSET_VEDIT_CODE(ds) DBLK_VEDIT_CODE((ds)->dblk)

#define VEDIT_FLAGS(vv)      ((vv).flags)
#define DBLK_VEDIT_FLAGS(db) VEDIT_FLAGS((db)->vedset)
#define DSET_VEDIT_FLAGS(db) DBLK_VEDIT_FLAGS((ds)->dblk)

#define VEDIT_good(vv)                                            \
   ( (vv).code>0 && (vv).code<=VEDIT_LASTCODE )
#define DBLK_VEDIT_good(db)                                       \
   ( VEDIT_good((db)->vedset) && (db)->vedset.ival >= 0 &&        \
                                 (db)->vedset.ival < (db)->nvals )
#define DSET_VEDIT_good(ds) DBLK_VEDIT_good((ds)->dblk)
/*------------------------------------------------------------------*/

/*!  All subvolumes are stored in an array of MRI_IMAGE (the "brick").
     - If mmap is used, then the whole external file is mmap()-ed in one
       block and the data pointers for each image computed from this base.
     - If malloc() is used, then each image is separately allocated and input.
     - Each datablock has a brick, even if it doesn't actually contain
       data (is only warp-on-demand).
     - Whether or not a datablock contains actual voxel data can be
       determined by examining the "malloc_type".
    \date Feb 1996
*/

typedef struct {
      int type ;              /*!< type code: DATABLOCK_TYPE */

      int nvals ;             /*!< number of 3D bricks */

      MRI_IMARR * brick  ;    /*!< array of pointers to each 3D brick */
      float * brick_fac  ;    /*!< array of scale factors to convert sub-bricks to floats */
      int *  brick_bytes ;    /*!< array of data size of each sub-brick */

                                /* These fields added for "bucket" datasets: */

      char **  brick_lab  ;     /*!< labels for all sub-bricks                 */
      char **  brick_keywords ; /*!< keywords strings for all sub-bricks       */
      int *    brick_statcode ; /*!< a FUNC_*_TYPE ==> kind of statistic here  */
      float ** brick_stataux ;  /*!< stat_aux parameters for each sub-brick with brick_statcode[iv] > 0 */

      int64_t total_bytes ;   /*!< totality of data storage needed */
      int     malloc_type ;   /*!< memory allocation method */
      int     locked ;        /*!< Feb 1998: locked in memory (un-purgeable) */

                                /* Jan 1999: for datasets that are extracted from a master dataset */
      int    master_nvals ;   /*!< Number of nvals in master dataset */
      int *  master_ival ;    /*!< master_ival[i] = sub-brick index in master of sub-brick #i here */
      int *  master_bytes ;   /*!< master_bytes[i] = size of sub-brick #i in master */

      float master_bot ;      /*!< range of data values to keep from master - bottom */
      float master_top ;      /*!< range of data values to keep from master - top */

      THD_diskptr * diskptr ; /*!< where the data is on disk (if anywhere!) */

      int       natr ;        /*!< number of attributes read from disk (or to write to disk) */
      int       natr_alloc ;  /*!< number of attributes allocated in atr below */
      ATR_any * atr ;         /*!< array of attributes (from the header) */

      int       nnodes ;      /*!< number of node indices [25 May 2006 rickr] */
      int     * node_list ;   /*!< index array for STORAGE_BY_NI_SURF_DSET    */

   /* pointers to other stuff */

      KILL_list kl ;          /*!< Stuff to delete if this struct is deleted */
      XtPointer parent ;      /*!< Somebody who "owns" me */

      char shm_idcode[32] ;   /*!< Idcode for shared memory buffer, if any [02 May 2003]. */
      int  shm_idint ;        /*!< Integer id for shared memory buffer. */

      VEDIT_settings vedset ; /*!< Volume edit-on-the-fly settings */
      MRI_IMAGE *vedim ;      /*!< Volume edit-on-the-fly result */

      floatvec **brick_fdrcurve ; /*!< FDR z(q) as a function of statistic */
      floatvec **brick_mdfcurve ; /*!< FDR mdf as a function of log10(p) */

} THD_datablock ;

/*! Force bricks to be allocated with malloc(). */

#define DBLK_mallocize(db) THD_force_malloc_type((db),DATABLOCK_MEM_MALLOC)

/*! Force bricks to be allocated with mmap(). */

#define DBLK_mmapize(db)   THD_force_malloc_type((db),DATABLOCK_MEM_MMAP)

/*! Don't care how bricks are allocated. */

#define DBLK_anyize(db)    THD_force_malloc_type((db),DATABLOCK_MEM_ANY)

/*! Test if brick is set to be malloc()-ed. */

#define DBLK_IS_MALLOC(db)  ((db)->malloc_type == DATABLOCK_MEM_MALLOC)

/*! Test if brick is set to be mmap()-ed. */

#define DBLK_IS_MMAP(db)    ((db)->malloc_type == DATABLOCK_MEM_MMAP)

/*! Test if brick is set to be shared. */

#define DBLK_IS_SHARED(db)  ((db)->malloc_type == DATABLOCK_MEM_SHARED)

/*! Force bricks to be allocated in shared memory.  */

#define DBLK_shareize(db) THD_force_malloc_type((db),DATABLOCK_MEM_SHARED)

/*! Lock bricks in memory. */

#define DBLK_lock(db)   ((db)->locked = 1)

/*! Unlock bricks from memory, if they aren't "superlocked". */

#define DBLK_unlock(db) ((db)->locked = ((db)->locked<2) ? 0 : 2)

/*! Test if brick is locked into memory. */

#define DBLK_LOCKED(db) ((db)->locked)

/*! Superlock brick in memory.  Can only be undone by explicit access to db->locked. */
#define DBLK_superlock(db) ((db)->locked = 2)

/*! Check if brick is mastered from another dataset. */

#define DBLK_IS_MASTERED(db) \
  ((db)->master_nvals > 0 && (db)->master_ival != NULL && (db)->master_bytes != NULL)

extern void THD_delete_datablock         ( THD_datablock * ) ;
extern void THD_init_datablock_brick     ( THD_datablock * , int , void * ) ;
extern void THD_init_datablock_labels    ( THD_datablock * ) ;
extern void THD_init_datablock_keywords  ( THD_datablock * ) ;
extern void THD_copy_datablock_auxdata   ( THD_datablock * , THD_datablock * ) ;
extern void THD_init_datablock_stataux   ( THD_datablock * ) ;
extern void THD_store_datablock_stataux  ( THD_datablock *,int,int,int,float * );
extern void THD_store_datablock_label    ( THD_datablock * , int , char * ) ;
extern void THD_store_datablock_keywords ( THD_datablock * , int , char * ) ;
extern void THD_append_datablock_keywords( THD_datablock * , int , char * ) ;
extern int  THD_datablock_from_atr       ( THD_datablock *, char *, char * ) ;
extern void atr_print( ATR_any * atr, char *ssep , char *spsep, char quote, int do_name) ;

/*! Initialize all sub-bricks auxiliary data to nothing. */

#define THD_null_datablock_auxdata(blk) ( (blk)->brick_lab      = NULL , \
                                          (blk)->brick_keywords = NULL , \
                                          (blk)->brick_statcode = NULL , \
                                          (blk)->brick_stataux  = NULL  )

extern int  THD_string_has( char * , char * ) ;

/*! Check if datablock is OK. */

#define ISVALID_DATABLOCK(bk) ( (bk) != NULL && (bk)->type == DATABLOCK_TYPE )

/*! Synonym for ISVALID_DATABLOCK. */

#define ISVALID_DBLK           ISVALID_DATABLOCK  /* 26 Mar 2001 */

/*------------- a dynamic array type for datablocks --------------*/

/*! A dynamic array type for datablocks - used when assembling datasets. */

typedef struct {
      int num ;                /*!< Number of datablocks stored */
      int nall ;               /*!< Number of datablocks space allocated for */
      THD_datablock ** ar ;    /*!< Array of datablocks */
} THD_datablock_array ;

#define INC_DBARR 8

/*! Initialize a THD_datablock_array. */

#define INIT_DBARR(name)                  \
   ( (name) = XtNew(THD_datablock_array) ,\
     (name)->num = (name)->nall = 0 ,     \
     (name)->ar  = NULL )

/*! Add a datablock to a THD_datablock_array. */

#define ADDTO_DBARR(name,bblk)                                     \
   { if( (name)->num == (name)->nall ){                            \
      (name)->nall += INC_DBARR + (name)->nall/8 ;                 \
      (name)->ar    = (THD_datablock **)                           \
                       XtRealloc( (char *) (name)->ar ,            \
                        sizeof(THD_datablock *) * (name)->nall ) ; \
     }                                                             \
     if( (bblk) != NULL ){               \
      (name)->ar[(name)->num] = (bblk) ; \
      ((name)->num)++ ;                  \
     } }

/*! Free the space used by a THD_datablock_array (but not the datablocks themselves). */

#define FREE_DBARR(name)      \
   if( (name) != NULL ){      \
     myXtFree( (name)->ar ) ; \
     myXtFree( (name) ) ; }

/*--------------------------------------------------------------------*/
/*---------- stuff to hold axes information for 3D dataset -----------*/

#define DATAXES_TYPE 27

/*! Default resampling grid size (in mm). */

#define DEFAULT_RESAMPLE_VOX 1.0

/*--- orientation types ---*/

#define ORI_R2L_TYPE  0
#define ORI_R2L_STR  "Right-to-Left"

#define ORI_L2R_TYPE  1
#define ORI_L2R_STR  "Left-to-Right"

#define ORI_P2A_TYPE  2
#define ORI_P2A_STR  "Posterior-to-Anterior"

#define ORI_A2P_TYPE  3
#define ORI_A2P_STR  "Anterior-to-Posterior"

#define ORI_I2S_TYPE  4
#define ORI_I2S_STR  "Inferior-to-Superior"

#define ORI_S2I_TYPE  5
#define ORI_S2I_STR  "Superior-to-Inferior"

#define ORI_GEN_TYPE  6
#define ORI_GEN_STR  "General"  /* not used at present */

#define FIRST_ORIENT_TYPE 0
#define LAST_ORIENT_TYPE  5

#define LONGEST_ORIENT_TYPESTR strlen(ORI_P2A_STR)

static char * ORIENT_typestr[] = {
   ORI_R2L_STR , ORI_L2R_STR , ORI_P2A_STR ,
   ORI_A2P_STR , ORI_I2S_STR , ORI_S2I_STR , ORI_GEN_STR
} ;

static char * ORIENT_shortstr[] = {
   "R-L" , "L-R" , "P-A" , "A-P" , "I-S" , "S-I" , "GEN"
} ;

static char * ORIENT_tinystr[] = {
   "RL" , "LR" , "PA" , "AP" , "IS" , "SI" , "??"
} ;

static char ORIENT_xyz[]   = "xxyyzzg" ;  /* DICOM directions are
                                             x = R->L , y = A->P , z = I->S */

/*! Determines if orientation code (0..5) is DICOM positive or negative. */

static char ORIENT_sign[]  = "+--++-" ;

static char ORIENT_first[] = "RLPAIS" ;

static int  ORIENT_xyzint[] = { 1,1 , 2,2 , 3,3 , 666 } ;

#define ORIENT_OPPOSITE(orc) \
  ( ((orc) % 2 == 0) ? ((orc)+1) : ((orc)-1) )

/*! Struct to hold information about 3D brick grid in space.
    Voxel center x[i] is at xxorg + i * xxdel, et cetera.
*/

typedef struct {
      int type ;     /*!< type code: DATAXES_TYPE */

      int nxx ;      /*!< Number of points in grid in x direction */
      int nyy ;      /*!< Number of points in grid in y direction */
      int nzz ;      /*!< Number of points in grid in z direction */

      float xxorg ;  /*!< Center of (0,0,0) voxel */
      float yyorg ;  /*!< Center of (0,0,0) voxel */
      float zzorg ;  /*!< center of (0,0,0) voxel */
      float xxdel ;  /*!< Spacings between voxel centers (mm) - may be negative */
      float yydel ;  /*!< Spacings between voxel centers (mm) - may be negative */
      float zzdel ;  /*!< Spacings between voxel centers (mm) - may be negative */

      float xxmin ;  /*!< Bounding box for grid */
      float xxmax ;  /*!< Bounding box for grid */
      float yymin ;  /*!< Bounding box for grid */
      float yymax ;  /*!< Bounding box for grid */
      float zzmin ;  /*!< Bounding box for grid */
      float zzmax ;  /*!< Bounding box for grid */

      int xxorient ;  /*!< Orientation code */
      int yyorient ;  /*!< Orientation code */
      int zzorient ;  /*!< Orientation code */

      THD_mat33 to_dicomm ; /*!< Orthogonal matrix transforming from
                                dataset coordinates to DICOM coordinates */

      /*** 06 Dec 2005: extensions to allow arbitrarily oriented volumes ***/

      mat44 ijk_to_dicom ;  /* matrix taking ijk indexes to DICOM xyz coords */
      mat44 dicom_to_ijk ;  /* inverse of above */
      float dicom_xxmin , dicom_yymin , dicom_zzmin ;
      float dicom_xxmax , dicom_yymax , dicom_zzmax ;
      /*** 18 May 2007: obliquity */
      mat44 ijk_to_dicom_real ;  /* matrix to convert ijk to DICOM for obliquity*/
   /* pointers to other stuff */

      XtPointer parent ;    /*!< Dataset that "owns" this struct */
} THD_dataxes ;

/*! Center of grid in x-direction. */
#define DAXES_XCEN(dax) ((dax)->xxorg + 0.5*((dax)->nxx - 1) * (dax)->xxdel)

/*! Center of grid in y-direction. */
#define DAXES_YCEN(dax) ((dax)->yyorg + 0.5*((dax)->nyy - 1) * (dax)->yydel)

/*! Center of grid in z-direction. */
#define DAXES_ZCEN(dax) ((dax)->zzorg + 0.5*((dax)->nzz - 1) * (dax)->zzdel)

#if 1
#define DAXES_NUM(dax,ori) \
   ( (ORIENT_xyzint[(ori)] == ORIENT_xyzint[(dax)->xxorient]) ? (dax)->nxx : \
     (ORIENT_xyzint[(ori)] == ORIENT_xyzint[(dax)->yyorient]) ? (dax)->nyy : \
     (ORIENT_xyzint[(ori)] == ORIENT_xyzint[(dax)->zzorient]) ? (dax)->nzz : 0 )
#else
#define DAXES_NUM(dax,ori) ( ( ORIENT_xyzint[(ori)] == 1 ) ? (dax)->nxx : \
                             ( ORIENT_xyzint[(ori)] == 2 ) ? (dax)->nyy : \
                             ( ORIENT_xyzint[(ori)] == 3 ) ? (dax)->nzz : 0 )
#endif

/*!  WARNING:  If you perform surgery on a dataset and change its
               dimensions in the dataxes, you must also reflect
               this in the diskptr.  Otherwise, the .HEAD file
               will not have the correct dimensions!  The macro
               just below will do this for you.
*/

#define DATAXES_TO_DISKPTR(ds)                             \
  ( (ds)->dblk->diskptr->dimsizes[0] = (ds)->daxes->nxx ,  \
    (ds)->dblk->diskptr->dimsizes[1] = (ds)->daxes->nyy ,  \
    (ds)->dblk->diskptr->dimsizes[2] = (ds)->daxes->nzz  )

/*! Check if dax is a valid THD_dataxes struct. */

#define ISVALID_DATAXES(dax) ( (dax) != NULL && (dax)->type == DATAXES_TYPE )

/*! Check if two THD_dataxes are essential equivalent. */

#define EQUIV_DATAXES(cax,dax)                     \
  ( ISVALID_DATAXES((cax))                      && \
    ISVALID_DATAXES((dax))                      && \
    (cax)->nxx == (dax)->nxx                    && \
    (cax)->nyy == (dax)->nyy                    && \
    (cax)->nzz == (dax)->nzz                    && \
    fabs( (cax)->xxorg - (dax)->xxorg ) < 0.01  && \
    fabs( (cax)->yyorg - (dax)->yyorg ) < 0.01  && \
    fabs( (cax)->zzorg - (dax)->zzorg ) < 0.01  && \
    fabs( (cax)->xxdel - (dax)->xxdel ) < 0.001 && \
    fabs( (cax)->yydel - (dax)->yydel ) < 0.001 && \
    fabs( (cax)->zzdel - (dax)->zzdel ) < 0.001 && \
    (cax)->xxorient == (dax)->xxorient          && \
    (cax)->yyorient == (dax)->yyorient          && \
    (cax)->zzorient == (dax)->zzorient    )

#define EQUIV_GRIDS(d1,d2) \
 ( ISVALID_DSET(d1) && ISVALID_DSET(d2) && EQUIV_DATAXES((d1)->daxes,(d2)->daxes) )

extern void THD_edit_dataxes( float , THD_dataxes * , THD_dataxes * ) ;

extern void THD_set_daxes_bbox     ( THD_dataxes * ) ; /* 20 Dec 2005 */
extern void THD_set_daxes_to_dicomm( THD_dataxes * ) ;

int THD_get_axis_direction( THD_dataxes *, int ) ; /* 19 Mar 2003 */

extern int  THD_daxes_to_mat44  ( THD_dataxes *dax ) ; /* 07 Dec 2005 */
extern int  THD_daxes_from_mat44( THD_dataxes *dax ) ;
extern void THD_set_dicom_box   ( THD_dataxes *dax ) ; /* 15 Dec 2005 */
extern mat44 THD_resample_mat44( mat44 , int,int,int ,
                                 float,float,float , int *,int *,int *) ;

/*---------------------------------------------------------------------*/
/* Macros and functions for dealing with mat44 structs. */

/*******
   Useful mat33 and mat44 functions in nifti1_io.c:
      mat44 nifti_mat44_inverse( mat44 R ) ;           == matrix inverse
      mat33 nifti_mat33_inverse( mat33 R ) ;           == matrix inverse
      mat33 nifti_mat33_polar  ( mat33 A ) ;           == polar decomp
      float nifti_mat33_rownorm( mat33 A ) ;           == max row sum
      float nifti_mat33_colnorm( mat33 A ) ;           == max col sum
      float nifti_mat33_determ ( mat33 R ) ;           == determinant
      mat33 nifti_mat33_mul    ( mat33 A, mat33 B ) ;  == matrix multiply
*******/

/******* Function below is not in nifti1_io.c, due to some oversight ******/

extern mat44 THD_mat44_mul( mat44 A , mat44 B ) ;      /* matrix multiply */
static mat44 tempA_mat44 ;

extern mat44 THD_mat44_sqrt( mat44 A ) ;  /* matrix square root [30 Jul 2007] */

#undef  MAT44_MUL
#define MAT44_MUL THD_mat44_mul

#undef  MAT44_SQRT
#define MAT44_SQRT THD_mat44_sqrt

#undef  MAT44_INV
#define MAT44_INV nifti_mat44_inverse

#undef  ISVALID_MAT44
#define ISVALID_MAT44(AA) ((AA).m[3][3] != 0.0f)

/* load the top 3 rows of a mat44 matrix,
   and set the 4th row to [ 0 0 0 1], as required */

#undef  LOAD_MAT44
#define LOAD_MAT44(AA,a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34)    \
  ( AA.m[0][0]=a11 , AA.m[0][1]=a12 , AA.m[0][2]=a13 , AA.m[0][3]=a14 ,   \
    AA.m[1][0]=a21 , AA.m[1][1]=a22 , AA.m[1][2]=a23 , AA.m[1][3]=a24 ,   \
    AA.m[2][0]=a31 , AA.m[2][1]=a32 , AA.m[2][2]=a33 , AA.m[2][3]=a34 ,   \
    AA.m[3][0]=AA.m[3][1]=AA.m[3][2]=0.0f , AA.m[3][3]=1.0f            )

#undef  LOAD_DIAG_MAT44
#define LOAD_DIAG_MAT44(AA,a,b,c)                                         \
  LOAD_MAT44( AA , (a),0,0,0 , 0,(b),0,0 , 0,0,(c),0 )

#undef  ZERO_MAT44
#define ZERO_MAT44(AA) LOAD_DIAG_MAT44(AA,0.0,0.0,0.0)

#undef  LOAD_MAT44_VEC
#define LOAD_MAT44_VEC(AA,x,y,z) ( AA.m[0][3]=(x) , AA.m[1][3]=(y) , AA.m[2][3]=(z) )

#undef  UNLOAD_MAT44_VEC
#define UNLOAD_MAT44_VEC(AA,x,y,z) ( (x)=AA.m[0][3] , (y)=AA.m[1][3] , (z)=AA.m[2][3] )

#undef  UNLOAD_MAT44
#define UNLOAD_MAT44(AA,a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34)  \
  ( a11=AA.m[0][0] , a12=AA.m[0][1] , a13=AA.m[0][2] , a14=AA.m[0][3] ,   \
    a21=AA.m[1][0] , a22=AA.m[1][1] , a23=AA.m[1][2] , a24=AA.m[1][3] ,   \
    a31=AA.m[2][0] , a32=AA.m[2][1] , a33=AA.m[2][2] , a34=AA.m[2][3]  )

#undef  UNLOAD_MAT44_AR
#define UNLOAD_MAT44_AR(AA,vv)                                       \
 UNLOAD_MAT44(AA,(vv)[0],(vv)[1],(vv)[2],(vv)[3],(vv)[4 ],(vv)[5 ],  \
                 (vv)[6],(vv)[7],(vv)[8],(vv)[9],(vv)[10],(vv)[11] )

#undef  LOAD_MAT44_AR
#define LOAD_MAT44_AR(AA,vv)                                       \
 LOAD_MAT44(AA,(vv)[0],(vv)[1],(vv)[2],(vv)[3],(vv)[4 ],(vv)[5 ],  \
               (vv)[6],(vv)[7],(vv)[8],(vv)[9],(vv)[10],(vv)[11] )

/* negate the top 2 rows of a mat44 matrix
   (for transforming between NIfTI-1 and DICOM coord systems) */

#undef  XYINVERT_MAT44
#define XYINVERT_MAT44(AA)                                  \
  ( AA.m[0][0] = -AA.m[0][0] , AA.m[0][1] = -AA.m[0][1] ,   \
     AA.m[0][2] = -AA.m[0][2] , AA.m[0][3] = -AA.m[0][3] ,  \
    AA.m[1][0] = -AA.m[1][0] , AA.m[1][1] = -AA.m[1][1] ,   \
     AA.m[1][2] = -AA.m[1][2] , AA.m[1][3] = -AA.m[1][3] )

#undef MAT44_SUB
#define MAT44_SUB(AA,BB)                                       \
 ( tempA_mat44.m[0][0] = (AA).m[0][0] - (BB).m[0][0] , \
   tempA_mat44.m[1][0] = (AA).m[1][0] - (BB).m[1][0] , \
   tempA_mat44.m[2][0] = (AA).m[2][0] - (BB).m[2][0] , \
   tempA_mat44.m[3][0] = (AA).m[3][0] - (BB).m[3][0] , \
   tempA_mat44.m[0][1] = (AA).m[0][1] - (BB).m[0][1] , \
   tempA_mat44.m[1][1] = (AA).m[1][1] - (BB).m[1][1] , \
   tempA_mat44.m[2][1] = (AA).m[2][1] - (BB).m[2][1] , \
   tempA_mat44.m[3][1] = (AA).m[3][1] - (BB).m[3][1] , \
   tempA_mat44.m[0][2] = (AA).m[0][2] - (BB).m[0][2] , \
   tempA_mat44.m[1][2] = (AA).m[1][2] - (BB).m[1][2] , \
   tempA_mat44.m[2][2] = (AA).m[2][2] - (BB).m[2][2] , \
   tempA_mat44.m[3][2] = (AA).m[3][2] - (BB).m[3][2] , \
   tempA_mat44.m[0][3] = (AA).m[0][3] - (BB).m[0][3] , \
   tempA_mat44.m[1][3] = (AA).m[1][3] - (BB).m[1][3] , \
   tempA_mat44.m[2][3] = (AA).m[2][3] - (BB).m[2][3] , \
   tempA_mat44.m[3][3] = (AA).m[3][3] - (BB).m[3][3] , tempA_mat44 )

#undef  MAT44_NORM
#define MAT44_NORM(AA)             \
 sqrt( (AA).m[0][0]*(AA).m[0][0] + \
       (AA).m[0][1]*(AA).m[0][1] + \
       (AA).m[0][2]*(AA).m[0][2] + \
       (AA).m[0][3]*(AA).m[0][3] + \
       (AA).m[1][0]*(AA).m[1][0] + \
       (AA).m[1][1]*(AA).m[1][1] + \
       (AA).m[1][2]*(AA).m[1][2] + \
       (AA).m[1][3]*(AA).m[1][3] + \
       (AA).m[2][0]*(AA).m[2][0] + \
       (AA).m[2][1]*(AA).m[2][1] + \
       (AA).m[2][2]*(AA).m[2][2] + \
       (AA).m[2][3]*(AA).m[2][3] + \
       (AA).m[3][0]*(AA).m[3][0] + \
       (AA).m[3][1]*(AA).m[3][1] + \
       (AA).m[3][2]*(AA).m[3][2] + \
       (AA).m[3][3]*(AA).m[3][3]  )

#undef  MAT44_COLNORM
#define MAT44_COLNORM(AA,j)            \
 sqrt( (AA).m[0][(j)]*(AA).m[0][(j)] + \
       (AA).m[1][(j)]*(AA).m[1][(j)] + \
       (AA).m[2][(j)]*(AA).m[2][(j)]  )

#undef  MAT44_ROWNORM
#define MAT44_ROWNORM(i)               \
 sqrt( (AA).m[(i)][0]*(AA).m[(i)][0] + \
       (AA).m[(i)][1]*(AA).m[(i)][1] + \
       (AA).m[(i)][2]*(AA).m[(i)][2]  )

/* load a mat33 matrix */

#undef  LOAD_MAT33
#define LOAD_MAT33(AA,a11,a12,a13,a21,a22,a23,a31,a32,a33)  \
  ( AA.m[0][0]=a11 , AA.m[0][1]=a12 , AA.m[0][2]=a13 ,      \
    AA.m[1][0]=a21 , AA.m[1][1]=a22 , AA.m[1][2]=a23 ,      \
    AA.m[2][0]=a31 , AA.m[2][1]=a32 , AA.m[2][2]=a33  )

/* copy the upper left corner of a mat44 struct into a mat33 struct */

#undef  MAT44_TO_MAT33
#define MAT44_TO_MAT33(AA,BB)                      \
  LOAD_MAT33(BB,AA.m[0][0],AA.m[0][1],AA.m[0][2],  \
                AA.m[1][0],AA.m[1][1],AA.m[1][2],  \
                AA.m[2][0],AA.m[2][1],AA.m[2][2] )

/* the reverse: copy mat33 to mat44 upper left corner */

#undef  MAT33_TO_MAT44
#define MAT33_TO_MAT44(AA,BB)                            \
   LOAD_MAT44(BB,AA.m[0][0],AA.m[0][1],AA.m[0][2],0.0f,  \
                 AA.m[1][0],AA.m[1][1],AA.m[1][2],0.0f,  \
                 AA.m[2][0],AA.m[2][1],AA.m[2][2],0.0f )

/* cf. vecmat.h */

#undef  VECMAT_TO_MAT44
#define VECMAT_TO_MAT44(vm,AA)                                                \
 LOAD_MAT44(AA,vm.mm.mat[0][0],vm.mm.mat[0][1],vm.mm.mat[0][2],vm.vv.xyz[0],  \
               vm.mm.mat[1][0],vm.mm.mat[1][1],vm.mm.mat[1][2],vm.vv.xyz[1],  \
               vm.mm.mat[2][0],vm.mm.mat[2][1],vm.mm.mat[2][2],vm.vv.xyz[2] )

#undef  MAT44_TO_VECMAT
#define MAT44_TO_VECMAT(AA,vm)                                                  \
 UNLOAD_MAT44(AA,vm.mm.mat[0][0],vm.mm.mat[0][1],vm.mm.mat[0][2],vm.vv.xyz[0],  \
                 vm.mm.mat[1][0],vm.mm.mat[1][1],vm.mm.mat[1][2],vm.vv.xyz[1],  \
                 vm.mm.mat[2][0],vm.mm.mat[2][1],vm.mm.mat[2][2],vm.vv.xyz[2] )

/* apply a mat44 matrix to a 3 vector (x,y,z) to produce (a,b,c) */

#undef  MAT44_VEC
#define MAT44_VEC(AA,x,y,z,a,b,c)                                        \
 ( (a) = AA.m[0][0]*(x) + AA.m[0][1]*(y) + AA.m[0][2]*(z) + AA.m[0][3] , \
   (b) = AA.m[1][0]*(x) + AA.m[1][1]*(y) + AA.m[1][2]*(z) + AA.m[1][3] , \
   (c) = AA.m[2][0]*(x) + AA.m[2][1]*(y) + AA.m[2][2]*(z) + AA.m[2][3]  )

/* apply a mat33 matrix to a 3 vector (x,y,z) to produce (a,b,c);
   could also be used to apply the upper left 3x3
   corner of a mat44 matrix to (x,y,z), if you insist */

#undef  MAT33_VEC
#define MAT33_VEC(AA,x,y,z,a,b,c)                           \
 ( (a) = AA.m[0][0]*(x) + AA.m[0][1]*(y) + AA.m[0][2]*(z) , \
   (b) = AA.m[1][0]*(x) + AA.m[1][1]*(y) + AA.m[1][2]*(z) , \
   (c) = AA.m[2][0]*(x) + AA.m[2][1]*(y) + AA.m[2][2]*(z)  )

/* L2 norm of i-th column of a matrix (3x3 or 4x4) */

#undef  MAT33_CLEN
#define MAT33_CLEN(AA,i)  \
 sqrt(AA.m[0][i]*AA.m[0][i]+AA.m[1][i]*AA.m[1][i]+AA.m[2][i]*AA.m[2][i])

/* print a mat44 struct to stdout (with a string) */

#undef  DUMP_MAT44
#define DUMP_MAT44(SS,AA)                              \
     printf("# mat44 %s:\n"                            \
            " %13.6f %13.6f %13.6f  %13.6f\n"         \
            " %13.6f %13.6f %13.6f  %13.6f\n"         \
            " %13.6f %13.6f %13.6f  %13.6f\n" ,       \
  SS, AA.m[0][0], AA.m[0][1], AA.m[0][2], AA.m[0][3],  \
      AA.m[1][0], AA.m[1][1], AA.m[1][2], AA.m[1][3],  \
      AA.m[2][0], AA.m[2][1], AA.m[2][2], AA.m[2][3] )


/* modify the last column of a mat44 struct so that the
   same spatial coords apply to an image with pp,qq,rr
   elements added at the lower edges [01 Sep 2006 - RWCox] */

#undef  MAT44_EXTEND_IJK
#define MAT44_EXTEND_IJK(AA,pp,qq,rr)                              \
 ( AA.m[0][3] -= AA.m[0][0]*(pp)+AA.m[0][1]*(qq)+AA.m[0][2]*(rr) , \
   AA.m[1][3] -= AA.m[1][0]*(pp)+AA.m[1][1]*(qq)+AA.m[1][2]*(rr) , \
   AA.m[2][3] -= AA.m[2][0]*(pp)+AA.m[2][1]*(qq)+AA.m[2][2]*(rr)  )

/* elementary rotation matrices:
   rotate about axis #ff, from axis #aa toward #bb,
   where ff, aa, and bb are a permutation of {0,1,2} */

#undef  LOAD_ROTGEN_MAT44
#define LOAD_ROTGEN_MAT44(AA,th,ff,aa,bb)                             \
 ( AA.m[aa][aa] = AA.m[bb][bb] = cos((th)) ,                          \
   AA.m[aa][bb] = sin((th)) ,                                         \
   AA.m[bb][aa] = -AA.m[aa][bb] ,                                     \
   AA.m[ff][ff] = 1.0f ,                                              \
   AA.m[aa][ff] = AA.m[bb][ff] = AA.m[ff][aa] = AA.m[ff][bb] = 0.0f , \
   AA.m[0][3]   = AA.m[1][3]   = AA.m[2][3]   =                       \
   AA.m[3][0]   = AA.m[3][1]   = AA.m[3][2]   = 0.0f , AA.m[3][3]=1.0f  )


/* rotations about x,y,z axes, respectively */

#undef  LOAD_ROTX_MAT44
#undef  LOAD_ROTY_MAT44
#undef  LOAD_ROTZ_MAT44
#define LOAD_ROTX_MAT44(A,th) LOAD_ROTGEN_MAT44(A,th,0,1,2)
#define LOAD_ROTY_MAT44(A,th) LOAD_ROTGEN_MAT44(A,th,1,2,0)
#define LOAD_ROTZ_MAT44(A,th) LOAD_ROTGEN_MAT44(A,th,2,0,1)

/* rotation about axis #i, for i=0,1,2 (x,y,z) */

#undef  LOAD_ROT_MAT44
#define LOAD_ROT_MAT44(A,th,i)                    \
  do{ switch( (i) ){                              \
        case 0: LOAD_ROTX_MAT44(A,th)   ; break ; \
        case 1: LOAD_ROTY_MAT44(A,th)   ; break ; \
        case 2: LOAD_ROTZ_MAT44(A,th)   ; break ; \
       default: LOAD_DIAG_MAT44(A,1,1,1); break ; \
      } } while(0)

/* determinant (could be used on a mat44 or mat33 struct) */

#undef  MAT44_DET
#define MAT44_DET(AA)                                                   \
 (  AA.m[0][0]*AA.m[1][1]*AA.m[2][2] - AA.m[0][0]*AA.m[1][2]*AA.m[2][1] \
  - AA.m[1][0]*AA.m[0][1]*AA.m[2][2] + AA.m[1][0]*AA.m[0][2]*AA.m[2][1] \
  + AA.m[2][0]*AA.m[0][1]*AA.m[1][2] - AA.m[2][0]*AA.m[0][2]*AA.m[1][1]   )

/* trace */

#undef  MAT44_TRACE
#define MAT44_TRACE(AA) ( AA.m[0][0] + AA.m[1][1] + AA.m[2][2] )

/*---------------------------------------------------------------------*/
/*--- data structure for information about time axis of 3D dataset ----*/

#define TIMEAXIS_TYPE 907

#define UNITS_MSEC_TYPE  77001
#define UNITS_SEC_TYPE   77002
#define UNITS_HZ_TYPE    77003

static char * UNITS_TYPE_labelstring[] = { "ms" , "s" , "Hz" } ;

/*! Return a string for the units of the uu-th time unit type. */

#define UNITS_TYPE_LABEL(uu) UNITS_TYPE_labelstring[(uu)-UNITS_MSEC_TYPE]

/*! Struct to hold information about the time axis of a 3D+time datset.

    For 3D+t datasets, there are ntt 3D times; the i-th one is centered
    at ttorg + ttdel*ii seconds, for ii=0..ntt-1.
    Also, ttdur = duration of each sample in time.

    If ( nsl > 0 && toff_sl != NULL), then the data was acquired as
    slices, not as a 3D block.  The slicing direction must be the
    dataset (not DICOM) z-axis.  The extra offset for the data at
    z is given by computing isl = (z - zorg_sl) / dz_sl + 0.5; the
    extra offset is then toff_sl[isl].  Note that dz_sl might be
    different from the dataxes zzdel because the dataset might actually
    be made up of duplicated slices (see program abut.c).

    All this is computed using the routine THD_timeof().

    When transformed, all the slice stuff will be ignored.  That's
    because the warped dataset z-direction will not be the same as the
    original dataset's z-direction.
*/

typedef struct {
   int   type ;     /*!< TIMEAXIS_TYPE */
   int   ntt ;      /*!< Number of time points */
   float ttorg ;    /*!< Time origin (usually 0) */
   float ttdel ;    /*!< Fondly known as TR */
   float ttdur ;    /*!< Duration of image acquisition (usually not known) */

   int units_type ;  /*!< one of the UNITS_ codes */

   int     nsl ;      /*!< Number of slice-dependent time offsets */
   float * toff_sl ;  /*!< toff_sl[i] is time offset for slice #1 */
   float   zorg_sl ;  /*!< z-coordinate origin for slice offsets */
   float   dz_sl ;    /*!< z-coordinate spacing for slice offsets */
} THD_timeaxis ;

/*! Check if tax points to a valid THD_timeaxis struct. */

#define ISVALID_TIMEAXIS(tax) ((tax) != NULL && (tax)->type == TIMEAXIS_TYPE)

/*---------------------------------------------------------------------*/
/*--------- data structure for statistics about a 3D dataset ----------*/

#define STATISTICS_TYPE 17

/*! Statistics about data in a sub-brick.
    (e.g., Used in the Define Function control panel in AFNI.)
*/

typedef struct {
   float min ;      /*!< Smallest value in sub-brick */
   float max ;      /*!< Largest value in sub-brick */
} THD_brick_stats ;

/*! Collection of statistics about all sub-bricks. */

typedef struct {
   int type ;                    /*!< STATISTICS_TYPE */
   int             nbstat ;      /*!< Number of entries below */
   THD_brick_stats *bstat ;      /*!< Array of entries for all sub-bricks */
   XtPointer parent ;            /*!< Owner of this object */
} THD_statistics ;

/*! Check if st is a valid THD_statistics struct. */

#define ISVALID_STATISTIC(st) ( (st) != NULL && (st)->type == STATISTICS_TYPE )

/*! Check if bst is a valid sub-brick statistic. */

#define ISVALID_BSTAT(bst) ( (bst).min <= (bst).max )

/*! Make bst have invalid data. */

#define INVALIDATE_BSTAT(bst) ( (bst).min = 1.0 , (bst).max = -1.0 )

/*! Destroy a THD_statistics struct. */

#define KILL_STATISTIC(st)          \
  do{ if( ISVALID_STATISTIC(st) ){  \
        XtFree((char *)(st)->bstat) ; XtFree((char *)(st)) ; } } while(0)

/*--------------------------------------------------------------------*/

typedef struct {
  float hbot , htop , hdel ; int nbin ;
  int *hist ;
} THD_histogram ;

#define HISTOGRAM_SET_TYPE 1743

typedef struct {
  int type ;
  int           nbhist ;
  THD_histogram *bhist ;
  XtPointer parent ;
} THD_histogram_set ;

/*--------------------------------------------------------------------*/
/*--------------------  Unique ID code for a 3D dataset  -------------*/

#ifndef IDCODE_PREFIX
#  define MCW_IDPREFIX "NIH_"
#else
#  define MCW_IDPREFIX IDCODE_PREFIX
#endif

/*! Size of ID code string. 27 Sep 2001: increased from 16 to 32. */
#define MCW_IDSIZE 32

/*! Size of ID date string. */
#define MCW_IDDATE 48

/*! Struct to hold ID code for a dataset. */

typedef struct {
  char str[MCW_IDSIZE] ;    /*!< Unique ID code string */
  char date[MCW_IDDATE] ;   /*!< Date string was generated */
} MCW_idcode ;

extern MCW_idcode MCW_new_idcode (void) ;
extern void       MCW_hash_idcode( char *, struct THD_3dim_dataset * ) ;

/*! Check if 2 ID code strings are equal. */

#define EQUIV_IDCODES(id,ie) (strncmp((id).str,(ie).str,MCW_IDSIZE) == 0)

/*! Check if 2 AFNI dataset pointers point to the same dataset struct. */

#define EQUIV_DSETS(ds,es) \
   ( (ds)==(es) ||         \
     ((ds)!=NULL && (es)!=NULL && EQUIV_IDCODES((ds)->idcode,(es)->idcode)) )

/*! Check if 2 AFNI dataset pointers are different but have the same ID codes. */

#define DUPLICATE_DSETS(ds,es)   \
   ( (ds) != (es) &&             \
     (ds) != NULL &&             \
     (es) != NULL && EQUIV_IDCODES((ds)->idcode,(es)->idcode) )

/*! Zero out the ID code. */

#define ZERO_IDCODE(id)   ((id).str[0] = (id).date[0] = '\0')

/*! Check if the ID code is zero. */

#define ISZERO_IDCODE(id) ((id).str[0] == '\0')

#define ATRNAME_IDSTRING  "IDCODE_STRING"
#define ATRNAME_IDDATE    "IDCODE_DATE"
#define ATRNAME_IDANATPAR "IDCODE_ANAT_PARENT"
#define ATRNAME_IDWARPPAR "IDCODE_WARP_PARENT"

/*----------------------------------------------------------------------*/
/*------------------- how to present the coordinates -------------------*/

/*! How to present coordinates to the user (vs. the internal RAI/DICOM order). */

typedef struct {
   int xxsign , yysign , zzsign ,
       first  , second , third  ,
       xxor   , yyor   , zzor    ;
   char orcode[4] ;
} THD_coorder ;

extern void THD_coorder_fill( char * , THD_coorder * ) ;
extern void THD_dicom_to_coorder( THD_coorder *, float *, float *, float * ) ;
extern void THD_coorder_to_dicom( THD_coorder *, float *, float *, float * ) ;

/*----------------------------------------------------------------------*/
/*-------------- internal data structure for a 3D dataset --------------*/

/* dataset type codes and string */

#define HEAD_ANAT_TYPE 0
#define HEAD_ANAT_STR  "3DIM_HEAD_ANAT"

#define HEAD_FUNC_TYPE 1
#define HEAD_FUNC_STR  "3DIM_HEAD_FUNC"

#define GEN_ANAT_TYPE  2
#define GEN_ANAT_STR   "3DIM_GEN_ANAT"

#define GEN_FUNC_TYPE  3
#define GEN_FUNC_STR   "3DIM_GEN_FUNC"

#define FIRST_3DIM_TYPE 0
#define LAST_3DIM_TYPE  3

#define LONGEST_3DIM_TYPESTR strlen(HEAD_ANAT_STR)

static char * DATASET_typestr[] = {
   HEAD_ANAT_STR , HEAD_FUNC_STR , GEN_ANAT_STR , GEN_FUNC_STR
} ;

/* view type codes, string, and viewcodes */

#define VIEW_ORIGINAL_TYPE    0
#define VIEW_ORIGINAL_STR     "Original View"
#define VIEW_ORIGINAL_CODE    "orig"

#define VIEW_ACPCALIGNED_TYPE 1
#define VIEW_ACPCALIGNED_STR  "AC-PC Aligned"
#define VIEW_ACPCALIGNED_CODE "acpc"

#define VIEW_TALAIRACH_TYPE   2
#define VIEW_TALAIRACH_STR    "Talairach View"
#define VIEW_TALAIRACH_CODE   "tlrc"

#define VIEW_REGISTERED_TYPE  3
#define VIEW_REGISTERED_STR   "Registered View"
#define VIEW_REGISTERED_CODE  "rgst"

#define FIRST_VIEW_TYPE 0
#define LAST_VIEW_TYPE  2

#define LONGEST_VIEW_TYPESTR strlen(VIEW_REGISTERED_STR)

static char * VIEW_typestr[] = {
   VIEW_ORIGINAL_STR  , VIEW_ACPCALIGNED_STR ,
   VIEW_TALAIRACH_STR , VIEW_REGISTERED_STR
} ;

static char * VIEW_codestr[] = {
   VIEW_ORIGINAL_CODE  , VIEW_ACPCALIGNED_CODE ,
   VIEW_TALAIRACH_CODE , VIEW_REGISTERED_CODE
} ;

/* function type codes, string, and prefixes */

#define FUNC_FIM_TYPE       0
#define FUNC_FIM_STR        "Intensity"
#define FUNC_FIM_PREFIX     "fim"
#define FUNC_FIM_LABEL      "fim"
#define FUNC_FIM_DESCRIPTOR "Functional Intensity"
#define FUNC_FIM_MASK       (1 << FUNC_FIM_TYPE)

/** old PAIR type retained for compatibility **/

#define FUNC_PAIR_TYPE   1
#define FUNC_PAIR_STR    "Inten+Thr"
#define FUNC_PAIR_PREFIX "fith"

#define FUNC_THR_TYPE          FUNC_PAIR_TYPE
#define FUNC_THR_STR           FUNC_PAIR_STR
#define FUNC_THR_PREFIX        FUNC_PAIR_PREFIX
#define FUNC_THR_TOP           1.0        /* maximum true value               */
#define FUNC_THR_SCALE_SHORT   10000      /* stored short = this * true value */
#define FUNC_THR_SCALE_BYTE    100        /* stored byte  = this * true value */
#define FUNC_THR_LABEL         "Thr "     /* <= 4 characters!                 */
#define FUNC_THR_DESCRIPTOR    "Old style threshold"
#define FUNC_THR_MASK          (1 << FUNC_PAIR_TYPE)

#define FUNC_COR_TYPE          2
#define FUNC_COR_STR           "Inten+Cor"
#define FUNC_COR_PREFIX        "fico"
#define FUNC_COR_TOP           1.0
#define FUNC_COR_SCALE_SHORT   10000
#define FUNC_COR_SCALE_BYTE    100
#define FUNC_COR_LABEL         "Corr"
#define FUNC_COR_DESCRIPTOR    "Correlation Coefficient"
#define FUNC_COR_MASK          (1 << FUNC_COR_TYPE)

#define FUNC_TT_TYPE           3
#define FUNC_TT_STR            "Inten+Ttest"
#define FUNC_TT_PREFIX         "fitt"
#define FUNC_TT_TOP           10.0
#define FUNC_TT_SCALE_SHORT   1000
#define FUNC_TT_SCALE_BYTE    10
#define FUNC_TT_LABEL         "T-t "
#define FUNC_TT_DESCRIPTOR    "Student t-statistic"
#define FUNC_TT_MASK          (1 << FUNC_TT_TYPE)

                                                    /* 30 Oct 1996 */
#define FUNC_FT_TYPE           4
#define FUNC_FT_STR            "Inten+Ftest"
#define FUNC_FT_PREFIX         "fift"
#define FUNC_FT_TOP           100.0
#define FUNC_FT_SCALE_SHORT   100
#define FUNC_FT_SCALE_BYTE    1
#define FUNC_FT_LABEL         "F-t "
#define FUNC_FT_DESCRIPTOR    "Fisher F-statistic"
#define FUNC_FT_MASK          (1 << FUNC_FT_TYPE)

                                                    /* 22 Jul 1997 */
#define FUNC_ZT_TYPE           5
#define FUNC_ZT_STR            "Inten+Ztest"
#define FUNC_ZT_PREFIX         "fizt"
#define FUNC_ZT_TOP           10.0
#define FUNC_ZT_SCALE_SHORT   1000
#define FUNC_ZT_SCALE_BYTE    10
#define FUNC_ZT_LABEL         "Z-t "
#define FUNC_ZT_DESCRIPTOR    "Normal (Gaussian) Z"
#define FUNC_ZT_MASK          (1 << FUNC_ZT_TYPE)

                                                    /* 22 Jul 1997 */
#define FUNC_CT_TYPE           6
#define FUNC_CT_STR            "Inten+ChiSq"
#define FUNC_CT_PREFIX         "fict"
#define FUNC_CT_TOP           100.0
#define FUNC_CT_SCALE_SHORT   100
#define FUNC_CT_SCALE_BYTE    1
#define FUNC_CT_LABEL         "ChiS"
#define FUNC_CT_DESCRIPTOR    "Chi-Squared statistic"
#define FUNC_CT_MASK          (1 << FUNC_CT_TYPE)

                                                    /* 22 Jul 1997 */
#define FUNC_BT_TYPE           7
#define FUNC_BT_STR            "Inten+Beta"
#define FUNC_BT_PREFIX         "fibt"
#define FUNC_BT_TOP           1.0
#define FUNC_BT_SCALE_SHORT   10000
#define FUNC_BT_SCALE_BYTE    100
#define FUNC_BT_LABEL         "Beta"
#define FUNC_BT_DESCRIPTOR    "Beta Distribution"
#define FUNC_BT_MASK          (1 << FUNC_BT_TYPE)

                                                    /* 22 Jul 1997 */
#define FUNC_BN_TYPE           8
#define FUNC_BN_STR            "Inten+Binom"
#define FUNC_BN_PREFIX         "fibn"
#define FUNC_BN_TOP           100.0
#define FUNC_BN_SCALE_SHORT   100
#define FUNC_BN_SCALE_BYTE    1
#define FUNC_BN_LABEL         "Bino"
#define FUNC_BN_DESCRIPTOR    "Binomial Distribution"
#define FUNC_BN_MASK          (1 << FUNC_BN_TYPE)

                                                    /* 22 Jul 1997 */
#define FUNC_GT_TYPE           9
#define FUNC_GT_STR            "Inten+Gamma"
#define FUNC_GT_PREFIX         "figt"
#define FUNC_GT_TOP           10.0
#define FUNC_GT_SCALE_SHORT   1000
#define FUNC_GT_SCALE_BYTE    10
#define FUNC_GT_LABEL         "Gam "
#define FUNC_GT_DESCRIPTOR    "Gamma Distribution"
#define FUNC_GT_MASK          (1 << FUNC_GT_TYPE)

                                                    /* 22 Jul 1997 */
#define FUNC_PT_TYPE          10
#define FUNC_PT_STR            "Inten+Poisson"
#define FUNC_PT_PREFIX         "fipt"
#define FUNC_PT_TOP           100.0
#define FUNC_PT_SCALE_SHORT   100
#define FUNC_PT_SCALE_BYTE    1
#define FUNC_PT_LABEL         "Pois"
#define FUNC_PT_DESCRIPTOR    "Poisson Distribution"
#define FUNC_PT_MASK          (1 << FUNC_PT_TYPE)

                                                   /* 30 Nov 1997 */
#define FUNC_BUCK_TYPE          11
#define FUNC_BUCK_STR           "Func-Bucket"
#define FUNC_BUCK_PREFIX        "fbuc"
#define FUNC_BUCK_TOP           1.0
#define FUNC_BUCK_SCALE_SHORT   1
#define FUNC_BUCK_SCALE_BYTE    1
#define FUNC_BUCK_LABEL         "Buck"
#define FUNC_BUCK_DESCRIPTOR    "Function Bucket"
#define FUNC_BUCK_MASK          (1 << FUNC_BUCK_TYPE)

#define FIRST_FUNC_TYPE  0
#define LAST_FUNC_TYPE  11

#define FIRST_STAT_TYPE  2
#define LAST_STAT_TYPE  10

#define FUNC_ALL_MASK (FUNC_FIM_MASK | FUNC_THR_MASK |                \
                       FUNC_COR_MASK | FUNC_TT_MASK  | FUNC_FT_MASK | \
                       FUNC_ZT_MASK  | FUNC_CT_MASK  | FUNC_BT_MASK | \
                       FUNC_BN_MASK  | FUNC_GT_MASK  | FUNC_PT_MASK | \
                       FUNC_BUCK_MASK                                    )

#define LONGEST_FUNC_TYPESTR strlen(FUNC_PT_STR)

static char * FUNC_typestr[] = {
   FUNC_FIM_STR , FUNC_THR_STR , FUNC_COR_STR , FUNC_TT_STR , FUNC_FT_STR ,
   FUNC_ZT_STR  , FUNC_CT_STR  , FUNC_BT_STR  ,
   FUNC_BN_STR  , FUNC_GT_STR  , FUNC_PT_STR  , FUNC_BUCK_STR
} ;

static char * FUNC_prefixstr[] = {
   FUNC_FIM_PREFIX , FUNC_THR_PREFIX , FUNC_COR_PREFIX ,
   FUNC_TT_PREFIX  , FUNC_FT_PREFIX  ,
   FUNC_ZT_PREFIX  , FUNC_CT_PREFIX  , FUNC_BT_PREFIX  ,
   FUNC_BN_PREFIX  , FUNC_GT_PREFIX  , FUNC_PT_PREFIX  , FUNC_BUCK_PREFIX
} ;

static float FUNC_topval[] = {
  1.0 , FUNC_THR_TOP , FUNC_COR_TOP , FUNC_TT_TOP , FUNC_FT_TOP ,
        FUNC_ZT_TOP  , FUNC_CT_TOP  , FUNC_BT_TOP ,
        FUNC_BN_TOP  , FUNC_GT_TOP  , FUNC_PT_TOP , FUNC_BUCK_TOP
} ;

static int FUNC_scale_short[] = {
  1 , FUNC_THR_SCALE_SHORT , FUNC_COR_SCALE_SHORT ,
      FUNC_TT_SCALE_SHORT  , FUNC_FT_SCALE_SHORT  ,
      FUNC_ZT_SCALE_SHORT  , FUNC_CT_SCALE_SHORT  , FUNC_BT_SCALE_SHORT ,
      FUNC_BN_SCALE_SHORT  , FUNC_GT_SCALE_SHORT  , FUNC_PT_SCALE_SHORT ,
      FUNC_BUCK_SCALE_SHORT
} ;

static int FUNC_scale_byte[] = {
  1 , FUNC_THR_SCALE_BYTE , FUNC_COR_SCALE_BYTE ,
      FUNC_TT_SCALE_BYTE  , FUNC_FT_SCALE_BYTE  ,
      FUNC_ZT_SCALE_BYTE  , FUNC_CT_SCALE_BYTE  , FUNC_BT_SCALE_BYTE ,
      FUNC_BN_SCALE_BYTE  , FUNC_GT_SCALE_BYTE  , FUNC_PT_SCALE_BYTE ,
      FUNC_BUCK_SCALE_BYTE
} ;

static char * FUNC_label[] = {
  FUNC_FIM_LABEL , FUNC_THR_LABEL , FUNC_COR_LABEL , FUNC_TT_LABEL , FUNC_FT_LABEL ,
  FUNC_ZT_LABEL  , FUNC_CT_LABEL  , FUNC_BT_LABEL ,
  FUNC_BN_LABEL  , FUNC_GT_LABEL  , FUNC_PT_LABEL , FUNC_BUCK_LABEL
} ;

static char * FUNC_descriptor[] = {
  FUNC_FIM_DESCRIPTOR , FUNC_THR_DESCRIPTOR ,
  FUNC_COR_DESCRIPTOR , FUNC_TT_DESCRIPTOR  , FUNC_FT_DESCRIPTOR ,
  FUNC_ZT_DESCRIPTOR  , FUNC_CT_DESCRIPTOR  , FUNC_BT_DESCRIPTOR ,
  FUNC_BN_DESCRIPTOR  , FUNC_GT_DESCRIPTOR  , FUNC_PT_DESCRIPTOR ,
  FUNC_BUCK_DESCRIPTOR
} ;

#define AFNI_FIRST_STATCODE FUNC_COR_TYPE
#define AFNI_LAST_STATCODE  FUNC_PT_TYPE

static int FUNC_nvals[]    = {  1, 2,2,2,2,2,2,2,2,2,2, 1 } ; /* # in each dataset */
static int FUNC_ival_fim[] = {  0, 0,0,0,0,0,0,0,0,0,0, 0 } ; /* index of fim      */

#define FIMTHR 0   /* set = -1 to disable thresholding for FIM type - 06 Feb 2003 */

static int FUNC_ival_thr[] = { FIMTHR, 1,1,1,1,1,1,1,1,1,1, 0 } ; /* index of thresh */

#define FUNC_HAVE_FIM(ftyp)  ((ftyp) >= 0 && \
                              (ftyp) <= LAST_FUNC_TYPE && FUNC_ival_fim[(ftyp)] >= 0)

#define FUNC_HAVE_THR(ftyp)  ((ftyp) >= 0 && \
                              (ftyp) <= LAST_FUNC_TYPE && FUNC_ival_thr[(ftyp)] >= 0)

#define FUNC_IS_STAT(ftyp)   ((ftyp) >= FIRST_STAT_TYPE && (ftyp) <= LAST_STAT_TYPE)
#define FUNC_HAVE_PVAL       FUNC_IS_STAT

/******* dimension of auxiliary array for functional statistics *******/

#define MAX_STAT_AUX 64

/*! Number of statistical parameters needed for each statistic code. */

static int FUNC_need_stat_aux[] = { 0 , 0 , 3 , 1 , 2 ,
                                    0 , 1 , 2 , 2 , 2 , 1 ,
                                    0 } ; /* # aux data needed */

/*! Labels describing the parameters needed for each statistic code. */

static char * FUNC_label_stat_aux[] = {
   "N/A" , "N/A" ,                                      /* fim, fith */
   "SAMPLES  FIT-PARAMETERS  ORT-PARAMETERS" ,          /* fico */
   "DEGREES-of-FREEDOM" ,                               /* fitt */
   "NUMERATOR and DENOMINATOR DEGREES-of-FREEDOM" ,     /* fift */
   "N/A" ,                                              /* fizt */
   "DEGREES-of-FREEDOM" ,                               /* fict */
   "A (numerator) and B (denominator)" ,                /* fibt */
   "NUMBER-of-TRIALS and PROBABILITY-per-TRIAL" ,       /* fibn */
   "SHAPE and SCALE" ,                                  /* figt */
   "MEAN" ,                                             /* fipt */
   "N/A"                                                /* fbuc */
} ;

/***  stat_aux values:
        FUNC_FIM_TYPE = not used
        FUNC_THR_TYPE = not used
        FUNC_COR_TYPE = # samples, # fit parameters, # ort parameters
        FUNC_TT_TYPE  = # degrees of freedom
        FUNC_FT_TYPE  = DOF for numerator and denominator
        FUNC_ZT_TYPE  = not used
        FUNC_CT_TYPE  = DOF
        FUNC_BT_TYPE  = a and b parameters
        FUNC_BN_TYPE  = number of trials, and probability per trial
        FUNC_GT_TYPE  = shape and scale parameters
        FUNC_PT_TYPE  = mean of Poisson distribution
      FUNC_BUCK_TYPE  = not used
***********************************************************************/

/****   anatomy type codes, strings, and prefixes        ****/
/* (these are not used much at present, but may be someday) */

#define ANAT_SPGR_TYPE   0
#define ANAT_SPGR_STR    "Spoiled GRASS"
#define ANAT_SPGR_PREFIX "spgr"
#define ANAT_SPGR_MASK   (1 << ANAT_SPGR_TYPE)

#define ANAT_FSE_TYPE    1
#define ANAT_FSE_STR     "Fast Spin Echo"
#define ANAT_FSE_PREFIX  "fse"
#define ANAT_FSE_MASK    (1 << ANAT_FSE_TYPE)

#define ANAT_EPI_TYPE    2
#define ANAT_EPI_STR     "Echo Planar"
#define ANAT_EPI_PREFIX  "epan"
#define ANAT_EPI_MASK    (1 << ANAT_EPI_TYPE)

#define ANAT_MRAN_TYPE   3
#define ANAT_MRAN_STR    "MRI Anatomy"
#define ANAT_MRAN_PREFIX "anat"
#define ANAT_MRAN_MASK   (1 << ANAT_MRAN_TYPE)

#define ANAT_CT_TYPE     4
#define ANAT_CT_STR      "CT Scan"
#define ANAT_CT_PREFIX   "ct"
#define ANAT_CT_MASK     (1 << ANAT_CT_TYPE)

#define ANAT_SPECT_TYPE   5
#define ANAT_SPECT_STR    "SPECT Anatomy"
#define ANAT_SPECT_PREFIX "spct"
#define ANAT_SPECT_MASK   (1 << ANAT_SPECT_TYPE)

#define ANAT_PET_TYPE     6
#define ANAT_PET_STR      "PET Anatomy"
#define ANAT_PET_PREFIX   "pet"
#define ANAT_PET_MASK     (1 << ANAT_PET_TYPE)

#define ANAT_MRA_TYPE    7
#define ANAT_MRA_STR     "MR Angiography"
#define ANAT_MRA_PREFIX  "mra"
#define ANAT_MRA_MASK    (1 << ANAT_MRA_TYPE)

#define ANAT_BMAP_TYPE   8
#define ANAT_BMAP_STR    "B-field Map"
#define ANAT_BMAP_PREFIX "bmap"
#define ANAT_BMAP_MASK   (1 << ANAT_BMAP_TYPE)

#define ANAT_DIFF_TYPE   9
#define ANAT_DIFF_STR    "Diffusion Map"
#define ANAT_DIFF_PREFIX "diff"
#define ANAT_DIFF_MASK   (1 << ANAT_DIFF_TYPE)

#define ANAT_OMRI_TYPE   10
#define ANAT_OMRI_STR    "Other MRI"
#define ANAT_OMRI_PREFIX "omri"
#define ANAT_OMRI_MASK   (1 << ANAT_OMRI_TYPE)

#define ANAT_BUCK_TYPE   11
#define ANAT_BUCK_STR    "Anat Bucket"
#define ANAT_BUCK_PREFIX "abuc"
#define ANAT_BUCK_MASK   (1 << ANAT_BUCK_TYPE)

#define ANAT_MAPC_TYPE   12
#define ANAT_MAPC_STR    "Mapped Color"
#define ANAT_MAPC_PREFIX "mapc"
#define ANAT_MAPC_MASK   (1 << ANAT_MAPC_TYPE)

#define FIRST_ANAT_TYPE  0
#define LAST_ANAT_TYPE   11

#define ANAT_ALL_MASK ( ANAT_SPGR_MASK | ANAT_FSE_MASK | ANAT_EPI_MASK   | \
                        ANAT_MRAN_MASK | ANAT_CT_MASK  | ANAT_SPECT_MASK | \
                        ANAT_PET_MASK  | ANAT_MRA_MASK | ANAT_BMAP_MASK  | \
                        ANAT_DIFF_MASK | ANAT_OMRI_MASK| ANAT_BUCK_MASK  | \
                        ANAT_MAPC_MASK )

#define NUM_DSET_TYPES (LAST_FUNC_TYPE + LAST_ANAT_TYPE + 2)

#define LONGEST_ANAT_TYPESTR strlen(ANAT_MRA_STR)

static char * ANAT_typestr[] = {
 ANAT_SPGR_STR , ANAT_FSE_STR   , ANAT_EPI_STR  , ANAT_MRAN_STR ,
 ANAT_CT_STR   , ANAT_SPECT_STR , ANAT_PET_STR  ,
 ANAT_MRA_STR  , ANAT_BMAP_STR  , ANAT_DIFF_STR , ANAT_OMRI_STR ,
 ANAT_BUCK_STR , ANAT_MAPC_STR
} ;

static char * ANAT_prefixstr[] = {
 ANAT_SPGR_PREFIX , ANAT_FSE_PREFIX   , ANAT_EPI_PREFIX  , ANAT_MRAN_PREFIX ,
 ANAT_CT_PREFIX   , ANAT_SPECT_PREFIX , ANAT_PET_PREFIX  ,
 ANAT_MRA_PREFIX  , ANAT_BMAP_PREFIX  , ANAT_DIFF_PREFIX , ANAT_OMRI_PREFIX ,
 ANAT_BUCK_PREFIX , ANAT_MAPC_PREFIX
} ;

/* Feb 1998: put all together */

static char * DSET_prefixstr[NUM_DSET_TYPES] = {
   FUNC_FIM_PREFIX , FUNC_THR_PREFIX , FUNC_COR_PREFIX ,
   FUNC_TT_PREFIX  , FUNC_FT_PREFIX  ,
   FUNC_ZT_PREFIX  , FUNC_CT_PREFIX  , FUNC_BT_PREFIX  ,
   FUNC_BN_PREFIX  , FUNC_GT_PREFIX  , FUNC_PT_PREFIX  , FUNC_BUCK_PREFIX ,
   ANAT_SPGR_PREFIX , ANAT_FSE_PREFIX   , ANAT_EPI_PREFIX  , ANAT_MRAN_PREFIX ,
   ANAT_CT_PREFIX   , ANAT_SPECT_PREFIX , ANAT_PET_PREFIX  ,
   ANAT_MRA_PREFIX  , ANAT_BMAP_PREFIX  , ANAT_DIFF_PREFIX , ANAT_OMRI_PREFIX ,
   ANAT_BUCK_PREFIX
} ;

#define DSET_PREFIXSTR(ds) ( ISFUNC(ds) ? FUNC_prefixstr[(ds)->func_type]  \
                                        : ANAT_prefixstr[(ds)->func_type] )

#define DSET_FUNCLABEL(ds) ( ISFUNC(ds) ? FUNC_label[(ds)->func_type]      \
                                        : ANAT_prefixstr[(ds)->func_type] )

#define DSET_TYPESTR(ds)   ( ISFUNC(ds) ? FUNC_typestr[(ds)->func_type]     \
                                        : ANAT_typestr[(ds)->func_type] )

static int ANAT_nvals[]     = { 1,1,1,1,1,1,1,1,1,1,1,1 , 1 } ;
static int ANAT_ival_zero[] = { 0,0,0,0,0,0,0,0,0,0,0,0 , 0 } ;

/* the data structure itself */

struct THD_3dim_dataset_array ;  /* incomplete definition */

/*! One AFNI dataset structure.
    Most elements are accessed via macros, and should only be changed via EDIT_dset_items(). */

typedef struct THD_3dim_dataset {
      int type ;        /*!< type code: HEAD_ANAT_TYPE or HEAD_FUNC_TYPE or GEN_ANAT_TYPE or GEN_FUNC_TYPE */

      int view_type ;   /*!< view code: VIEW_ORIGINAL_TYPE or VIEW_ACPCALIGNED_TYPE or VIEW_TALAIRACH_TYPE */
      int func_type ;   /*!< dataset type: one of FUNC_*_TYPE or ANAT_*_TYPE codes */

      char label1[THD_MAX_LABEL] ;  /*!< short label #1: not used for anything anymore */
      char label2[THD_MAX_LABEL] ;  /*!< short label #2: even more obsolete */

      THD_datablock   * dblk ;      /*!< pointer to actual data */
      THD_dataxes     * daxes ;     /*!< info about axes (where dataset is) */
      THD_dataxes     * wod_daxes ; /*!< warp-on-demand axes (for viewing interpolated dataset) */
      int               wod_flag ;  /*!< if true, use wod_daxes, otherwise use daxes */

      THD_timeaxis    * taxis ;     /*!< non-NULL --> this is a 3D+t dataset */

      THD_marker_set  * markers ;   /*!< user set mark points (if non-NULL) */

      struct THD_3dim_dataset * warp_parent ; /*!< non-NULL --> this dataset is warped from that one */
      THD_warp                * warp ;        /*!< this is the coordinate-to-coordinate warp */
      THD_warp                * vox_warp ;    /*!< this is the index-to-index warp */

      struct THD_3dim_dataset * anat_parent ;   /*!< non-NULL --> linked to this as anatomical ref */

      THD_statistics          * stats ;      /*!< statistics about the sub-brick data */

      float stat_aux[MAX_STAT_AUX] ;         /*!< global auxiliary statistics info */

      char warp_parent_name[THD_MAX_NAME] ;  /*!< "name" of warp_parent dataset (no longer used) */
      char anat_parent_name[THD_MAX_NAME] ;  /*!< "name" of anat_parent dataset (no longer used) */
      char self_name[THD_MAX_NAME]        ;  /*!< my own "name" (no longer used) */

#ifdef ALLOW_DATASET_VLIST
      THD_vector_list * pts ;     /*!< in dataset coords (not DICOM order!) - for Ted Deyoe */
      Boolean pts_original ;      /*!< true if was read from disk directly */
#endif

      int death_mark ;            /*!< dataset is marked for destruction */

      MCW_idcode idcode ;              /*!< globally unique (I hope) ID code for this dataset */
      MCW_idcode anat_parent_idcode ;  /*!< ID code for warp_parent dataset */
      MCW_idcode warp_parent_idcode ;  /*!< ID code for anat_parent dataset */

      char * keywords ;           /*!< 30 Nov 1997: keyword list for dataset */

      THD_usertaglist * tagset ;  /*!< 23 Oct 1998: see plug_tag.c */

   /* pointers to other stuff */

      KILL_list kl ;              /*!< Stuff to delete if this dataset is deleted (see killer.h) */
      XtPointer parent ;          /*!< Somebody that "owns" this dataset */

   /* 26 Aug 2002: self warp (for w-o-d) */

      THD_warp *self_warp ;

   /* 03 Aug 2004: list of filenames to cat together (cf. THD_open_tcat) */

      char *tcat_list ;
      int   tcat_num ;
      int  *tcat_len ;

} THD_3dim_dataset ;

/*! A marker that defines a dataset that is about to be killed. */

#define DOOMED 665

/*! Mark a dataset to be eliminated by AFNI_mark_for_death() and AFNI_andersonville(). */

#define DSET_MARK_FOR_DEATH(ds)                                         \
 do{ if( ISVALID_DSET(ds) && ds->death_mark >= 0 ) ds->death_mark = DOOMED ; } while(0)

/*! Mark a dataset to be ineligible for elimination during AFNI_rescan_session(). */

#define DSET_MARK_FOR_IMMORTALITY(ds)                                   \
 do{ if( ISVALID_DSET(ds) ) ds->death_mark = -1 ; } while(0)

/*! Mark a dataset to be eligible for elimination if the need arises. */

#define DSET_MARK_FOR_NORMAL(ds)                                        \
 do{ if( ISVALID_DSET(ds) ) ds->death_mark = 0 ; } while(0)

/*! Dataset is tcat-ed? */

#define DSET_IS_TCAT(ds) (ISVALID_DSET(ds) && (ds)->tcat_list != NULL)

/*! Return pointer to current dataset axes (warp-on-demand or permanent). */

#define CURRENT_DAXES(ds) (((ds)->wod_flag) ? ((ds)->wod_daxes) : ((ds)->daxes))

/*! Determine if ds is a pointer to a valid dataset. */

#define ISVALID_3DIM_DATASET(ds)                       \
   ( (ds) != NULL && (ds)->type >= FIRST_3DIM_TYPE  && \
                     (ds)->type <= LAST_3DIM_TYPE   && \
                 (ds)->view_type >= FIRST_VIEW_TYPE && \
                 (ds)->view_type <= LAST_VIEW_TYPE  && \
      ISVALID_DATABLOCK((ds)->dblk)                     )

/*! Determine if ds is a pointer to a valid dataset. */

#define ISVALID_DSET ISVALID_3DIM_DATASET

/*! Determine if nn is a functional dataset type code. */

#define ISFUNCTYPE(nn) ( (nn) == HEAD_FUNC_TYPE || (nn) == GEN_FUNC_TYPE )

/*! Determine if dset is a functional dataset. */

#define ISFUNC(dset) ( ISVALID_DSET(dset) && ISFUNCTYPE((dset)->type) )

/*! Determine if nn is an anatomical dataset type code. */

#define ISANATTYPE(nn) ( (nn) == HEAD_ANAT_TYPE || (nn) == GEN_ANAT_TYPE )

/*! Determine if dset is an anatomical dataset. */

#define ISANAT(dset) ( ISVALID_DSET(dset) && ISANATTYPE((dset)->type) )

/*! Determine if nn is a head dataset type code. */

#define ISHEADTYPE(nn) ( (nn) == HEAD_ANAT_TYPE || (nn) == HEAD_FUNC_TYPE )  /* 09 Sep 2002: ==ugh */

/*! Determine if dset is a head dataset (vs. non-head). */

#define ISHEAD(dset) ( ISVALID_DSET(dset) && ISHEADTYPE((dset)->type) )

/*! Determine if dset is an anatomical bucket dataset */

#define ISANATBUCKET(dset) ( ISANAT(dset) && (dset)->func_type == ANAT_BUCK_TYPE )

/*! Determine if dset is a functional bucket dataset */

#define ISFUNCBUCKET(dset) ( ISFUNC(dset) && (dset)->func_type == FUNC_BUCK_TYPE )

/*! Determine if dset is a bucket dataset (functional or anatomical) */

#define ISBUCKET(dset) ( ISANATBUCKET(dset) || ISFUNCBUCKET(dset) )

/*! Determine if dataset ds is actually stored on disk */

#define DSET_ONDISK(ds) ( ISVALID_DSET(ds) && (ds)->dblk!=NULL && \
                          (ds)->dblk->diskptr->storage_mode!=STORAGE_UNDEFINED )

/*! Determine if dataset ds is stored in a BRIK file on disk */

#define DSET_IS_BRIK(ds) ( ISVALID_DSET(ds) && (ds)->dblk!=NULL && \
                           (ds)->dblk->diskptr->storage_mode == STORAGE_BY_BRICK )

/*! Determine if datablock db is stored in a MINC file on disk */

#define DBLK_IS_MINC(db) ( ISVALID_DBLK(db) && ISVALID_DISKPTR((db)->diskptr) && \
                           (db)->diskptr->storage_mode == STORAGE_BY_MINC )

/*! Determine if dataset ds is stored in a MINC file on disk */

#define DSET_IS_MINC(ds) ( ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) &&       \
                           ISVALID_DISKPTR((ds)->dblk->diskptr) &&               \
                           (ds)->dblk->diskptr->storage_mode == STORAGE_BY_MINC )

/*! Determine if datablock db is stored in a ANALYZE file on disk */

#define DBLK_IS_ANALYZE(db) ( ISVALID_DBLK(db) && ISVALID_DISKPTR((db)->diskptr) && \
                              (db)->diskptr->storage_mode == STORAGE_BY_ANALYZE )

/*! Determine if dataset ds is stored in a ANALYZE file on disk */

#define DSET_IS_ANALYZE(ds) ( ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) &&       \
                              ISVALID_DISKPTR((ds)->dblk->diskptr) &&               \
                              (ds)->dblk->diskptr->storage_mode == STORAGE_BY_ANALYZE )

/*! Determine if datablock db is stored in a CTFMRI file on disk */

#define DBLK_IS_CTFMRI(db) ( ISVALID_DBLK(db) && ISVALID_DISKPTR((db)->diskptr) && \
                             (db)->diskptr->storage_mode == STORAGE_BY_CTFMRI )

/*! Determine if dataset ds is stored in a CTFMRI file on disk */

#define DSET_IS_CTFMRI(ds) ( ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) &&       \
                             ISVALID_DISKPTR((ds)->dblk->diskptr) &&               \
                             (ds)->dblk->diskptr->storage_mode == STORAGE_BY_CTFMRI )

/*! Determine if datablock db is stored in a CTFSAM file on disk */

#define DBLK_IS_CTFSAM(db) ( ISVALID_DBLK(db) && ISVALID_DISKPTR((db)->diskptr) && \
                             (db)->diskptr->storage_mode == STORAGE_BY_CTFSAM )

/*! Determine if dataset ds is stored in a CTFSAM file on disk */

#define DSET_IS_CTFSAM(ds) ( ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) &&       \
                             ISVALID_DISKPTR((ds)->dblk->diskptr) &&               \
                             (ds)->dblk->diskptr->storage_mode == STORAGE_BY_CTFSAM )

/*! Determine if datablock db is stored in a 1D file on disk */

#define DBLK_IS_1D(db) ( ISVALID_DBLK(db) && ISVALID_DISKPTR((db)->diskptr) &&     \
                         (db)->diskptr->storage_mode == STORAGE_BY_1D )

/*! Determine if datablock db is stored in a 3D file on disk */

#define DBLK_IS_3D(db) ( ISVALID_DBLK(db) && ISVALID_DISKPTR((db)->diskptr) &&     \
                         (db)->diskptr->storage_mode == STORAGE_BY_3D )

/*! Determine if datablock db is stored in a NIFTI file on disk */

#define DBLK_IS_NIFTI(db) ( ISVALID_DBLK(db) && ISVALID_DISKPTR((db)->diskptr) &&  \
                           (db)->diskptr->storage_mode == STORAGE_BY_NIFTI )

/*! Determine if datablock db is stored in a NIML file on disk  26 May 2006 */

#define DBLK_IS_NIML(db) ( ISVALID_DBLK(db)               &&  \
                           ISVALID_DISKPTR((db)->diskptr) &&  \
                           (db)->diskptr->storage_mode == STORAGE_BY_NIML )

/*! Determine if datablock db is stored in a NI_SURF_DSET file on disk */

#define DBLK_IS_NI_SURF_DSET(db) ( ISVALID_DBLK(db)   &&  \
                       ISVALID_DISKPTR((db)->diskptr) &&  \
                       (db)->diskptr->storage_mode == STORAGE_BY_NI_SURF_DSET )

/*! Determine if datablock db is stored in a NI_SURF_DSET file on disk */

#define DBLK_IS_GIFTI(db) ( ISVALID_DBLK(db)   &&  \
                       ISVALID_DISKPTR((db)->diskptr) &&  \
                       (db)->diskptr->storage_mode == STORAGE_BY_GIFTI )

/*! Determine if dataset ds is stored in a 1D file on disk */

#define DSET_IS_1D(ds) ( ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) &&           \
                         ISVALID_DISKPTR((ds)->dblk->diskptr) &&                   \
                         (ds)->dblk->diskptr->storage_mode == STORAGE_BY_1D )

/*! Determine if dataset ds is stored in a 3D file on disk */

#define DSET_IS_3D(ds) ( ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) &&           \
                         ISVALID_DISKPTR((ds)->dblk->diskptr) &&                   \
                         (ds)->dblk->diskptr->storage_mode == STORAGE_BY_3D )

/*! Determine if dataset ds is stored in a NIFTI file on disk */

#define DSET_IS_NIFTI(ds) ( ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) &&        \
                            ISVALID_DISKPTR((ds)->dblk->diskptr) &&                \
                            (ds)->dblk->diskptr->storage_mode == STORAGE_BY_NIFTI )

/*! Determine if dataset ds is stored in a NIML file on disk  26 May 2006 */

#define DSET_IS_NIML(ds) ( ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) &&  \
                           ISVALID_DISKPTR((ds)->dblk->diskptr)         &&  \
                         (ds)->dblk->diskptr->storage_mode == STORAGE_BY_NIML )

/*! Determine if dataset ds is stored in a NI_SURF_DSET file on disk */

#define DSET_IS_NI_SURF_DSET(ds) (ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) \
                 && ISVALID_DISKPTR((ds)->dblk->diskptr) &&                    \
                 (ds)->dblk->diskptr->storage_mode == STORAGE_BY_NI_SURF_DSET )

/*! Determine if dataset ds is stored in a GIFTI file on disk */

#define DSET_IS_GIFTI(ds) (ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) \
                 && ISVALID_DISKPTR((ds)->dblk->diskptr) &&                    \
                 (ds)->dblk->diskptr->storage_mode == STORAGE_BY_GIFTI )

/*! Determine if datablock db is stored by volume files rather than 1 big BRIK */

#define DBLK_IS_VOLUMES(db) ( ISVALID_DBLK(db) &&                                \
                              ISVALID_DISKPTR((db)->diskptr) &&                  \
                              (db)->diskptr->storage_mode == STORAGE_BY_VOLUMES )

/*! Determine if dataset ds is stored in volumes files rather than 1 big BRIK */

#define DSET_IS_VOLUMES(ds) ( ISVALID_DSET(ds) &&                                    \
                              ISVALID_DBLK((ds)->dblk) &&                            \
                              ISVALID_DISKPTR((ds)->dblk->diskptr) &&                \
                              (ds)->dblk->diskptr->storage_mode == STORAGE_BY_VOLUMES )

/*! Determine if datablock db is stored in a MPEG file on disk */

#define DBLK_IS_MPEG(db) ( ISVALID_DBLK(db) && ISVALID_DISKPTR((db)->diskptr) && \
                           (db)->diskptr->storage_mode == STORAGE_BY_MPEG )

/*! Determine if dataset ds is stored in a MPEG file on disk */

#define DSET_IS_MPEG(ds) ( ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) &&       \
                           ISVALID_DISKPTR((ds)->dblk->diskptr) &&               \
                           (ds)->dblk->diskptr->storage_mode == STORAGE_BY_MPEG )

/*! Determine if dataset is valid, but has a non-AFNI storage mode */

#define IS_VALID_NON_AFNI_DSET(ds)                                           \
        ( ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) &&                    \
          ISVALID_DISKPTR((ds)->dblk->diskptr) &&                            \
          ( (ds)->dblk->diskptr->storage_mode == STORAGE_BY_MINC         ||  \
            (ds)->dblk->diskptr->storage_mode == STORAGE_BY_ANALYZE      ||  \
            (ds)->dblk->diskptr->storage_mode == STORAGE_BY_CTFMRI       ||  \
            (ds)->dblk->diskptr->storage_mode == STORAGE_BY_CTFSAM       ||  \
            (ds)->dblk->diskptr->storage_mode == STORAGE_BY_1D           ||  \
            (ds)->dblk->diskptr->storage_mode == STORAGE_BY_3D           ||  \
            (ds)->dblk->diskptr->storage_mode == STORAGE_BY_NIFTI        ||  \
            (ds)->dblk->diskptr->storage_mode == STORAGE_BY_MPEG         ||  \
            (ds)->dblk->diskptr->storage_mode == STORAGE_BY_NIML         ||  \
            (ds)->dblk->diskptr->storage_mode == STORAGE_BY_NI_SURF_DSET ||  \
            (ds)->dblk->diskptr->storage_mode == STORAGE_BY_GIFTI            \
          ) )

/*! Determine if AFNI is allowed to over-write dataset ds */

#define DSET_WRITEABLE(ds)       \
 ( ISVALID_DSET(ds)          &&  \
   ISVALID_DBLK((ds)->dblk)  &&  \
   (ds)->warp_parent != NULL &&  \
   !DSET_IS_MINC(ds)         &&  \
   !DSET_IS_ANALYZE(ds)        )

/*! Determine if dataset ds is stored in a compressed format */

#define DSET_COMPRESSED(ds)                  \
   ( ISVALID_DSET(ds) && (ds)->dblk!=NULL && \
     (ds)->dblk->diskptr != NULL          && \
     COMPRESS_filecode((ds)->dblk->diskptr->brick_name) >= 0 )

/*! Purge the data of dataset ds from memory (you can reload it later) */

#define PURGE_DSET(ds)                                                  \
  do{ if( ISVALID_3DIM_DATASET(ds) && DSET_ONDISK(ds) )                 \
         (void) THD_purge_datablock( (ds)->dblk , DATABLOCK_MEM_ANY ) ; \
  } while(0)

/*! Determine if dataset ds is loadable into memory */

#define DSET_INMEMORY(ds)                                                        \
  ( ISVALID_DSET(ds) && (ds)->dblk!=NULL &&                                      \
    (ds)->dblk->malloc_type!=DATABLOCK_MEM_UNDEFINED &&                          \
    ( (ds)->dblk->diskptr->storage_mode!=STORAGE_UNDEFINED || DSET_LOADED(ds) ) )

#define DBLK_BRICK(db,iv) ((db)->brick->imarr[(iv)])

/*! Return the MRI_IMAGE * that is the iv-th volume of dataset ds */

#define DSET_BRICK(ds,iv) DBLK_BRICK((ds)->dblk,(iv))

/*! See if the iv-th volume is purged to disk at this moment */

#define DSET_BRICK_IS_PURGED(ds,iv) MRI_IS_PURGED(DSET_BRICK((ds),(iv)))

#define DBLK_BRICK_TYPE(db,iv) (DBLK_BRICK((db),(iv))->kind)

/*! Return the datum code (MRI_short, etc.) of the iv-th volume of dataset ds */

#define DSET_BRICK_TYPE(ds,iv) DBLK_BRICK_TYPE((ds)->dblk,(iv))

/*! Return the number of voxels in the iv-th volume of dataset ds */

#define DBLK_BRICK_NVOX(db,iv) (DBLK_BRICK((db),(iv))->nvox)

#define DBLK_ARRAY(db,iv) mri_data_pointer( DBLK_BRICK((db),(iv)) )

/*! Return the pointer to the actual data in the iv-th volume of dataset ds */

#define DSET_ARRAY(ds,iv) DBLK_ARRAY((ds)->dblk,(iv))

#define DSET_BRICK_ARRAY DSET_ARRAY  /* Because I sometimes forget the  */
#define DBLK_BRICK_ARRAY DBLK_ARRAY  /* correct names given above - RWC */

#define DBLK_BRICK_FACTOR(db,iv) ((db)->brick_fac[(iv)])

/*! Return the brick scaling factor of the iv-th volume of dataset ds.

    If the scale factor is 0, then the brick is used "as-is"; that is,
    the effective scale factor is 1.  You can assign to this macro
    as in "DSET_BRICK_FACTOR(ds,iv)=3.2;" but I don't recommend this.
    Instead, do something like "EDIT_BRICK_FACTOR(ds,iv,3.2);" (see editvol.h).
*/

#define DSET_BRICK_FACTOR(ds,iv) DBLK_BRICK_FACTOR((ds)->dblk,(iv))

extern int THD_need_brick_factor( THD_3dim_dataset * ) ;

#define DBLK_BRICK_BYTES(db,iv) ((db)->brick_bytes[iv])

/*! Return number of bytes stored in the iv-th volume of dataset ds */

#define DSET_BRICK_BYTES(ds,iv) DBLK_BRICK_BYTES((ds)->dblk,(iv))

/*! Return the volume index of the "most important" sub-brick in dataset ds.

    This is still used in places, but is fairly obsolete
*/
#define DSET_PRINCIPAL_VALUE(ds) ( ISANAT(ds) ? ANAT_ival_zero[(ds)->func_type] \
                                              : FUNC_ival_fim[(ds)->func_type] )

/*! Synonym for DSET_PRINCIPAL_VALUE */

#define DSET_PRINCIPAL_INDEX DSET_PRINCIPAL_VALUE

/*! Return the volume index of the "threshold" sub-brick in dataset ds.

    This is analogous to DSET_PRINCIPAL_VALUE, and is also sort-of-obsolete.
*/
#define DSET_THRESH_VALUE(ds) (ISANAT((ds)) ? -1 : FUNC_ival_thr[(ds)->func_type])

#define DSET_THRESH_INDEX DSET_THRESH_VALUE

/*! Return a pointer to the prefix of dataset ds */

#define DSET_PREFIX(ds) (((ds)->dblk!=NULL && (ds)->dblk->diskptr!=NULL) \
                       ? ((ds)->dblk->diskptr->prefix) : "\0" )

extern char * THD_newprefix(THD_3dim_dataset * dset, char * suffix); /* 16 Feb 2001 */
extern char * THD_deplus_prefix( char *prefix ) ;                    /* 22 Nov 2002 */
extern int    THD_deconflict_prefix( THD_3dim_dataset * ) ;          /* 23 Mar 2007 */

/*! Return a pointer to the filecode of dataset ds (prefix+view) */

#define DSET_FILECODE(ds) (((ds)->dblk!=NULL && (ds)->dblk->diskptr!=NULL) \
                         ? ((ds)->dblk->diskptr->filecode) : "\0" )

/*! Return a pointer to the .HEAD filename of dataset ds */

#define DSET_HEADNAME(ds) ( ((ds)->tcat_list != NULL) ? (ds)->tcat_list     \
                          : ((ds)->dblk!=NULL && (ds)->dblk->diskptr!=NULL) \
                          ? ((ds)->dblk->diskptr->header_name) : "\0" )

/*! Return a pointer to the .BRIK filename of dataset ds */

#define DSET_BRIKNAME(ds) (((ds)->dblk!=NULL && (ds)->dblk->diskptr!=NULL) \
                         ? ((ds)->dblk->diskptr->brick_name) : "\0" )
#define DSET_BRICKNAME DSET_BRIKNAME

/*! Return a pointer to the directory name of dataset ds */

#define DSET_DIRNAME(ds) (((ds)->dblk!=NULL && (ds)->dblk->diskptr!=NULL) \
                         ? ((ds)->dblk->diskptr->directory_name) : "\0" )

#define DSET_SESSNAME DSET_DIRNAME

/*! Return a pointer to the ID code of dataset ds */

#define DSET_IDCODE(ds) (&((ds)->idcode))

/*! Return the ID code string */

#define DSET_IDCODE_STR(ds) ((ds)->idcode.str)

/* 25 April 1998 */

#define DBLK_BYTEORDER(db)  ((db)->diskptr->byte_order)

/*! Return LSB_FIRST or MSB_FIRST for dataset ds */

#define DSET_BYTEORDER(ds)  DBLK_BYTEORDER((ds)->dblk)

/** macros for time-dependent datasets **/

/*! Return number of time points in dataset ds.

    If value is 1, dataset is not time-dependent, but it still may have
    multiple sub-bricks (if it is a bucket dataset, for example)
*/
#define DSET_NUM_TIMES(ds)       ( ((ds)->taxis == NULL) ? 1 : (ds)->taxis->ntt )

/*! Check if have a 3D+time dataset. */

#define HAS_TIMEAXIS(ds)         ( DSET_NUM_TIMES(ds) > 1 )
#define DSET_HAS_TIMEAXIS HAS_TIMEAXIS

/*! Return number of values stored at each time point for dataset ds.

    Will always be 1 in the current version of AFNI!
    (Except for bucket datasets, that is, damn it.)
*/
#define DSET_NVALS_PER_TIME(ds)  ( (ds)->dblk->nvals / DSET_NUM_TIMES(ds) )

/*! Return number of sub-bricks in dataset ds */

#define DSET_NVALS(ds)           ( (ds)->dblk->nvals )

/*! Return number of voxels in each sub-brick of dataset ds */

#define DSET_NVOX(ds) ( (ds)->daxes->nxx * (ds)->daxes->nyy * (ds)->daxes->nzz )

/*! Return number of voxels along x-axis of dataset ds */

#define DSET_NX(ds) ((ds)->daxes->nxx)

/*! Return number of voxels along y-axis of dataset ds */

#define DSET_NY(ds) ((ds)->daxes->nyy)

/*! Return number of voxels along z-axis of dataset ds */

#define DSET_NZ(ds) ((ds)->daxes->nzz)

/*! Return grid spacing (voxel size) along x-axis of dataset ds */

#define DSET_DX(ds) ((ds)->daxes->xxdel)  /* added 17 Aug 1998 */

/*! Return grid spacing (voxel size) along y-axis of dataset ds */

#define DSET_DY(ds) ((ds)->daxes->yydel)

/*! Return grid spacing (voxel size) along z-axis of dataset ds */

#define DSET_DZ(ds) ((ds)->daxes->zzdel)

/*! Return volume of a voxel */

#define DSET_VOXVOL(ds) \
  fabsf((ds)->daxes->xxdel*(ds)->daxes->yydel*(ds)->daxes->zzdel)

/*! Return minimum grid spacing in 2 dimensions for dataset ds */
#define DSET_MIN_DELXY(ds) ((fabs(DSET_DX(ds)) < (fabs(DSET_DY(ds))) ) ?  \
     fabs(DSET_DX(ds)) : fabs(DSET_DY(ds)) )

/*! Return minimum grid spacing in 3 dimensions for dataset ds */
#define DSET_MIN_DEL(ds) ((DSET_MIN_DELXY(ds)<fabs(DSET_DZ(ds))) ? \
     DSET_MIN_DELXY(ds) : fabs(DSET_DZ(ds)))

/*! Return grid origin along x-axis of dataset ds */

#define DSET_XORG(ds) ((ds)->daxes->xxorg)  /* 29 Aug 2001 */

/*! Return grid origin along y-axis of dataset ds */

#define DSET_YORG(ds) ((ds)->daxes->yyorg)

/*! Return grid origin along y-axis of dataset ds */

#define DSET_ZORG(ds) ((ds)->daxes->zzorg)

/*! Return smallest x-coordinate of grid for dataset ds */

#define DSET_XXMIN(ds) ((ds)->daxes->xxmin) /* 11 Sep 2001 */

/*! Return largest x-coordinate of grid for dataset ds */

#define DSET_XXMAX(ds) ((ds)->daxes->xxmax)

/*! Return smallest y-coordinate of grid for dataset ds */

#define DSET_YYMIN(ds) ((ds)->daxes->yymin)

/*! Return largest y-coordinate of grid for dataset ds */

#define DSET_YYMAX(ds) ((ds)->daxes->yymax)

/*! Return smallest z-coordinate of grid for dataset ds */

#define DSET_ZZMIN(ds) ((ds)->daxes->zzmin)

/*! Return largest z-coordinate of grid for dataset ds */

#define DSET_ZZMAX(ds) ((ds)->daxes->zzmax)

  /* these next 4 added 19 Aug 1999 */

/*! Find the x-axis index of a 3D array index in dataset ds */

#define DSET_index_to_ix(ds,ii)         (  (ii) % (ds)->daxes->nxx)

/*! Find the y-axis index of a 3D array index in dataset ds */

#define DSET_index_to_jy(ds,ii)         ( ((ii) / (ds)->daxes->nxx) % (ds)->daxes->nyy )

/*! Find the z-axis index of a 3D array index in dataset ds */

#define DSET_index_to_kz(ds,ii)         (  (ii) /((ds)->daxes->nxx * (ds)->daxes->nyy ))

/*! Convert a triple-index (ix,jy,kz) to a single 3D index for dataset ds */

#define DSET_ixyz_to_index(ds,ix,jy,kz) ((ix)+((jy)+(kz)*(ds)->daxes->nyy)*(ds)->daxes->nxx)

/*! Determine if dataset ds has cubical voxels */

#define DSET_CUBICAL(ds) ( fabs((ds)->daxes->xxdel) == fabs((ds)->daxes->yydel) && \
                           fabs((ds)->daxes->xxdel) == fabs((ds)->daxes->zzdel)   )

#if 0  /* 22 Sep 2000 */
#define DSET_GRAPHABLE(ds) ( ISVALID_3DIM_DATASET(ds) && DSET_INMEMORY(ds)      && \
                             (ds)->wod_flag == False  && DSET_NUM_TIMES(ds) > 1 && \
                             ( DSET_ONDISK(ds) || DSET_LOADED(ds) && DSET_LOCKED(ds) ) )
#else
/*! Determine if a graph window can be opened for dataset ds.

    Cannot graph warp-on-demand datasets.
*/
#define DSET_GRAPHABLE(ds) ( ISVALID_3DIM_DATASET(ds) && DSET_INMEMORY(ds)      && \
                             (ds)->wod_flag == False                            && \
                             ( DSET_ONDISK(ds) || DSET_LOADED(ds) && DSET_LOCKED(ds) ) )
#endif

/*! Return the TR for dataset ts; will be 0 if not time-dependent. */

#define DSET_TIMESTEP(ds)        ( ((ds)->taxis == NULL) ? 0.0 : (ds)->taxis->ttdel )

#define DSET_TR                  DSET_TIMESTEP

/*! Return the time origin for dataset ds.

    Is always 0 in current version of AFNI.
*/
#define DSET_TIMEORIGIN(ds)      ( ((ds)->taxis == NULL) ? 0.0 : (ds)->taxis->ttorg )

/*! Return the time duration of image acquisition for dataset ds.

    Is always 0 in current version of AFNI (was intended for true 3D echo-volume imaging).
*/
#define DSET_TIMEDURATION(ds)    ( ((ds)->taxis == NULL) ? 0.0 : (ds)->taxis->ttdur )

/*! Return the time-step units code for dataset ds.

    Will be one of
      - UNITS_MSEC_TYPE  milliseconds
      - UNITS_SEC_TYPE   seconds
      - UNITS_HZ_TYPE    Hertz
      - ILLEGAL_TYPE     not a time-dependent dataset (d'oh)
*/
#define DSET_TIMEUNITS(ds)       ( ((ds)->taxis == NULL) ? ILLEGAL_TYPE             \
                                                         : (ds)->taxis->units_type )

/*! Alter a dataset's time units from MSEC to SEC, if need be. */

#define DSET_UNMSEC(ds)                                                               \
 do{ int zz ;                                                                         \
   if( (ds)!=NULL && (ds)->taxis!=NULL && (ds)->taxis->units_type==UNITS_MSEC_TYPE ){ \
     (ds)->taxis->units_type = UNITS_SEC_TYPE ;                                       \
     (ds)->taxis->ttdel     *= 0.001 ;                                                \
     (ds)->taxis->ttorg     *= 0.001 ;                                                \
     (ds)->taxis->ttdur     *= 0.001 ;                                                \
     if( (ds)->taxis->toff_sl != NULL )                                               \
      for( zz=0 ; zz < (ds)->taxis->nsl ; zz++ ) (ds)->taxis->toff_sl[zz] *= 0.001 ;  \
   } } while(0)

/*! Return number of time-axis slice offsets for datsaet ds.

    Will be zero for non-time-dependent datasets, and may be zero or positive
    for time-dependent datasets
*/
#define DSET_NUM_TTOFF(ds)       ( ((ds)->taxis == NULL) ? 0 : (ds)->taxis->nsl )

/** 30 Nov 1997 **/

static char tmp_dblab[8] ;
#define DBLK_BRICK_LAB(db,iv) ( ((db)->brick_lab != NULL) ? ((db)->brick_lab[iv]) : "?" )

/*! Return the label string for sub-brick iv of dataset ds.

    This label is used on chooser menus, for example
*/
#define DSET_BRICK_LAB(ds,iv) DBLK_BRICK_LAB((ds)->dblk,(iv))

/*! Synonym for DSET_BRICK_LAB */

#define DSET_BRICK_LABEL      DSET_BRICK_LAB

#define DBLK_BRICK_STATCODE(db,iv)  \
 ( ((db)->brick_statcode != NULL) ? (db)->brick_statcode[iv] : ILLEGAL_TYPE )

/*! Return the statistical type code for the iv-th volume of dataset ds.

    Will be -1 if this sub-brick is not tagged as being an SPM.
*/
#define DSET_BRICK_STATCODE(ds,iv)                                         \
   ( ISBUCKET((ds)) ? DBLK_BRICK_STATCODE((ds)->dblk,(iv))                 \
                    : (ISFUNC(ds) && (iv)==FUNC_ival_thr[(ds)->func_type]) \
                      ? (ds)->func_type : -1 )

#define DBLK_BRICK_STATAUX(db,iv)  \
 ( ((db)->brick_stataux != NULL) ? (db)->brick_stataux[iv] : NULL )

/*! Return float * pointer to statistical parameters for sub-brick iv in dataset ds.

    If return is NULL, there aren't any parameters for this sub-brick,
    otherwise the number of parameters is given by FUNC_need_stat_aux[code],
    where code = DSET_BRICK_STATCODE(ds,iv).
*/
#define DSET_BRICK_STATAUX(ds,iv)                                          \
   ( ISBUCKET((ds)) ? DBLK_BRICK_STATAUX((ds)->dblk,(iv))                  \
                    : (ISFUNC(ds) && (iv)==FUNC_ival_thr[(ds)->func_type]) \
                      ? (ds)->stat_aux : NULL )

#define DBLK_BRICK_STATPAR(db,iv,jj) \
 ( ((db)->brick_stataux != NULL) ? (db)->brick_stataux[iv][jj] : 0.0 )

/*! Return the jj-th statistical parameter for the iv-th volume of dataset ds. */

#define DSET_BRICK_STATPAR(ds,iv,jj)                                       \
   ( ISBUCKET((ds)) ? DBLK_BRICK_STATPAR((ds)->dblk,(iv),(jj))             \
                    : (ISFUNC(ds) && (iv)==FUNC_ival_thr[(ds)->func_type]) \
                      ? (ds)->stat_aux[jj] : 0.0 )

#define DBLK_BRICK_KEYWORDS(db,iv) \
  ( ((db)->brick_keywords != NULL) ? ((db)->brick_keywords[iv]) : NULL )

#define DSET_BRICK_KEYWORDS(ds,iv) DBLK_BRICK_KEYWORDS((ds)->dblk,(iv))

#define DSET_KEYWORDS(ds) ((ds)->keywords)

#define DSET_BRICK_KEYWORDS_HAS(ds,iv,ss) \
   THD_string_has( DSET_BRICK_KEYWORDS((ds),(iv)) , (ss) )

#define DSET_KEYWORDS_HAS(ds,ss) \
   THD_string_has( DSET_KEYWORDS((ds)) , (ss) )

/*---- macros to get the FDR curve for a sub-brick (if any) [23 Jan 2008] ----*/

#define DBLK_BRICK_FDRCURVE(db,ii) \
 ( ((db)->brick_fdrcurve==NULL) ? NULL : (db)->brick_fdrcurve[ii] )

#define DSET_BRICK_FDRCURVE(ds,ii) DBLK_BRICK_FDRCURVE((ds)->dblk,(ii))

#define DBLK_BRICK_FDRCURVE_KILL(db,ii)                                      \
 do{ if( (db)->brick_fdrcurve != NULL ){                                     \
       floatvec *fv = (db)->brick_fdrcurve[ii] ;                             \
       if( fv != NULL ){ KILL_floatvec(fv); (db)->brick_fdrcurve[ii]=NULL; } \
 }} while(0)

#define DSET_BRICK_FDRCURVE_KILL(ds,ii) DBLK_BRICK_FDRCURVE_KILL((ds)->dblk,(ii))

#define DBLK_BRICK_FDRCURVE_ALLKILL(db)                                    \
 do{ if( (db)->brick_fdrcurve != NULL ){                                   \
      int qq;                                                              \
      for( qq=0; qq < (db)->nvals; qq++ ) DBLK_BRICK_FDRCURVE_KILL(db,qq); \
      free((db)->brick_fdrcurve) ; (db)->brick_fdrcurve = NULL ;           \
 }} while(0)

#define DSET_BRICK_FDRCURVE_ALLKILL(ds) DBLK_BRICK_FDRCURVE_ALLKILL((ds)->dblk)

/*---- same for MDF curves [22 Oct 2008] -----*/

#define DBLK_BRICK_MDFCURVE(db,ii) \
 ( ((db)->brick_mdfcurve==NULL) ? NULL : (db)->brick_mdfcurve[ii] )

#define DSET_BRICK_MDFCURVE(ds,ii) DBLK_BRICK_MDFCURVE((ds)->dblk,(ii))

#define DBLK_BRICK_MDFCURVE_KILL(db,ii)                                      \
 do{ if( (db)->brick_mdfcurve != NULL ){                                     \
       floatvec *fv = (db)->brick_mdfcurve[ii] ;                             \
       if( fv != NULL ){ KILL_floatvec(fv); (db)->brick_mdfcurve[ii]=NULL; } \
 }} while(0)

#define DSET_BRICK_MDFCURVE_KILL(ds,ii) DBLK_BRICK_MDFCURVE_KILL((ds)->dblk,(ii))

#define DBLK_BRICK_MDFCURVE_ALLKILL(db)                                    \
 do{ if( (db)->brick_mdfcurve != NULL ){                                   \
      int qq;                                                              \
      for( qq=0; qq < (db)->nvals; qq++ ) DBLK_BRICK_MDFCURVE_KILL(db,qq); \
      free((db)->brick_mdfcurve) ; (db)->brick_mdfcurve = NULL ;           \
 }} while(0)

#define DSET_BRICK_MDFCURVE_ALLKILL(ds) DBLK_BRICK_MDFCURVE_ALLKILL((ds)->dblk)

extern int   THD_create_one_fdrcurve( THD_3dim_dataset *, int ) ;
extern int   THD_create_all_fdrcurves( THD_3dim_dataset * ) ;
extern float THD_fdrcurve_zval( THD_3dim_dataset *, int, float ) ;
extern float THD_mdfcurve_mval( THD_3dim_dataset *, int, float ) ;

/*! Macro to load the self_name and labels of a dataset
    with values computed from the filenames;
    replaces user control/input of these values in to3d
*/

#define DSET_FIX_NAMES(ds)                                       \
  ( strcpy((ds)->self_name,(ds)->dblk->diskptr->directory_name), \
    strcat((ds)->self_name,(ds)->dblk->diskptr->filecode)      , \
    strcpy((ds)->label1   ,(ds)->dblk->diskptr->filecode)      , \
    strcpy((ds)->label2   ,THD_DEFAULT_LABEL) )

/*! Macro to load brick statistics of a dataset if it
      - doesn't have statistics already, or
      - has bad statistics from the (very) old to3d bug
*/

#define RELOAD_STATS(dset)                                                  \
  if( ISVALID_3DIM_DATASET((dset)) &&                                       \
      ( !ISVALID_STATISTIC((dset)->stats) ||                                \
        ( (dset)->dblk->nvals > 1 &&                                        \
          (dset)->stats->bstat[1].min > (dset)->stats->bstat[1].max ) ) ){  \
     THD_load_statistics((dset)) ; }

/*! Determine if the ii-th volume of dataset dset has a valid brick statistic.
    Brick statistics are just the min and max values in the volume.
*/

#define DSET_VALID_BSTAT(dset,ii)                 \
  ( ISVALID_3DIM_DATASET((dset))     &&           \
    ISVALID_STATISTIC((dset)->stats) &&           \
    (ii) < (dset)->stats->nbstat     &&           \
    ISVALID_BSTAT( (dset)->stats->bstat[(ii)] ) )

/*! Mark the ii-th volume's brick statistics to be invalid in dataset dset. */

#define DSET_CRUSH_BSTAT(dset,ii)                                 \
  do{ if( DSET_VALID_BSTAT(dset,ii) )                             \
         INVALIDATE_BSTAT((dset)->stats->bstat[(ii)]) ; } while(0)

/*! Delete all the sub-brick statistics for dataset ds. */

#define DSET_KILL_STATS(ds)                                \
  do{ if( (ds)->stats != NULL ){                           \
         REMOVEFROM_KILL( (ds)->kl, (ds)->stats->bstat ) ; \
         REMOVEFROM_KILL( (ds)->kl, (ds)->stats ) ;        \
         KILL_STATISTIC( (ds)->stats ) ;                   \
         (ds)->stats = NULL ; } } while(0)

/*! Macro to initialize the global stat_aux data in a dataset.

    Note that each sub-brick now has its own stat_aux data, and this
    global data is only used for the older (non-bucket) functional
    dataset types such as "fico".
*/

#define INIT_STAT_AUX(ds,nf,ff)               \
  do{ int is ;                                \
      for( is=0 ; is < MAX_STAT_AUX ; is++ )  \
         (ds)->stat_aux[is] = (is < (nf)) ? (ff)[is] : 0.0 ; } while(0)

/*! Clear the global stat_aux data in a dataset. */

#define ZERO_STAT_AUX(ds)                              \
  do{ int is ; for( is=0 ; is < MAX_STAT_AUX ; is++ )  \
                 (ds)->stat_aux[is] = 0.0 ; } while(0)

/** macros to load and unload a dataset from memory **/

/*! Load dataset ds's sub-bricks into memory.

    If it is already loaded, does nothing (so you can call this without much penalty).
*/
#define DSET_load(ds)   THD_load_datablock( (ds)->dblk )

/*! Unload dataset ds's sub-bricks from memory.

    Won't do anything if the dataset is locked into memory
*/
#define DSET_unload(ds) THD_purge_datablock( (ds)->dblk , DATABLOCK_MEM_ANY )

/*! Unload sub-brick iv in dataset ds from memory.

    Only does something if the dataset is malloc()-ed,
    not mmap()-ed, and not locked in memory
*/
#define DSET_unload_one(ds,iv) THD_purge_one_brick( (ds)->dblk , (iv) )

/*! Delete dataset ds's volumes and struct from memory.

    Does not delete from disk
*/
#define DSET_delete(ds) THD_delete_3dim_dataset((ds),False)

#define DSET_deletepp(ds) \
  do{ THD_delete_3dim_dataset((ds),False); myXtFree((ds)); } while(0)

/*! Write dataset ds to disk.

    Also loads the sub-brick statistics
*/
#define DSET_write(ds)  ( THD_load_statistics( (ds) ) ,                    \
                          THD_write_3dim_dataset( NULL,NULL , (ds),True ) )

/*! Write dataset to disk, fer shur this time, Cletus. [07 Jan 2008] */

#define DSET_overwrite(ds)      \
 do{ THD_force_ok_overwrite(1); \
     DSET_write(ds); THD_force_ok_overwrite(0); } while(0)

extern int THD_deathcon(void) ;             /* 06 Jun 2007 */
extern int THD_ok_overwrite(void) ;         /* Jan 2008 */
extern void THD_force_ok_overwrite( int ) ; /* 07 Jan 2008 */

/*! Write only the dataset header to disk, for dataset ds */

#define DSET_write_header(ds)  THD_write_3dim_dataset( NULL,NULL , (ds),False )

#define DSET_overwrite_header(ds)  \
 do{ THD_force_ok_overwrite(1);    \
     DSET_write_header(ds); THD_force_ok_overwrite(0); } while(0)

/*! Check if dataset ds if fully loaded into memory.

    If return is 0 (false), you could try DSET_load(ds)
*/
#define DSET_LOADED(ds) ( THD_count_databricks((ds)->dblk) == DSET_NVALS(ds) )

/*! Check if a given brick is loaded [14 Sep 2007] */

#define DSET_BRICK_LOADED(ds,iq) \
 ( DSET_BRICK(ds,iq) != NULL && DSET_ARRAY(ds,iq) != NULL )

/*! Lock dataset ds into memory */

#define DSET_lock(ds)      DBLK_lock((ds)->dblk)       /* Feb 1998 */

/*! Unlock dataset ds (so it can be purged) */

#define DSET_unlock(ds)    DBLK_unlock((ds)->dblk)

/*! Check if dataset ds is locked into memory */

#define DSET_LOCKED(ds)    DBLK_LOCKED((ds)->dblk)

/*! Force this dataset to be loaded into memory using malloc().

    If you are altering the dataset contents, this is required,
    since a mmap()-ed dataset is readonly.
*/
#define DSET_mallocize(ds) DBLK_mallocize((ds)->dblk)

/*! Force this dataset to be loaded into memory using mmap()
    You cannot alter any sub-brick data, since mmap() is done in
    readonly mode.
*/
#define DSET_mmapize(ds)   DBLK_mmapize((ds)->dblk)

/*! Force this dataset to be loaded into shared memory.
    You cannot alter any sub-brick data, since is done in
    readonly mode.
*/
#define DSET_shareize(ds)  DBLK_shareize((ds)->dblk)

/*! Let AFNI decide how to load a dataset into memory.

    May choose mmap() or malloc()
*/
#define DSET_anyize(ds)    DBLK_anyize((ds)->dblk)

/*! Super-lock dataset ds into memory.

    Super-locked datasets will not be unlocked by DSET_unlock
*/
#define DSET_superlock(ds) DBLK_superlock((ds)->dblk)  /* 22 Mar 2001 */

/*! Check if dataset ds is loaded into memory using malloc() */

#define DSET_IS_MALLOC(ds)  DBLK_IS_MALLOC((ds)->dblk)

/*! Check if dataset ds is loaded into memory using mmap() */

#define DSET_IS_MMAP(ds)    DBLK_IS_MMAP((ds)->dblk)

/*! Check if dataset ds is loaded into shared memory */

#define DSET_IS_SHARED(ds)  DBLK_IS_SHARED((ds)->dblk)

/*! Check if dataset ds is "mastered": gets its data from someone else.

    Mastered datasets are specified on the command line with the [a..b] syntax, etc.
*/
#define DSET_IS_MASTERED(ds) DBLK_IS_MASTERED((ds)->dblk)

/*-------------------------------------------------------------------*/
#undef  TWOGIG
#define TWOGIG 2147000000   /* 2 gigabytes, aboot */

#define DBLK_mmapfix(db)                                                      \
  do{ if( (db)->malloc_type==DATABLOCK_MEM_MMAP && (db)->total_bytes>TWOGIG ) \
        (db)->malloc_type = DATABLOCK_MEM_MALLOC ; } while(0)

/*---------------------------------------------------------------------------*/

extern void THD_patch_dxyz_all( THD_3dim_dataset * ) ;       /* 05 Jun 2007 */
extern void THD_patch_dxyz_one( THD_3dim_dataset * , int ) ;

/*------------- a dynamic array type for 3D datasets ---------------*/

/*! A dynamic array type for AFNI datasets.

    This is used when collecting all the datasets in a directory into a THD_session.
*/

typedef struct THD_3dim_dataset_array {
      int num ;                  /*!< Number of datasets stored */
      int nall ;                 /*!< Number of datasets slots allocated */
      THD_3dim_dataset **ar ;    /*!< Array of datasets: [0..num-1] are in use */
} THD_3dim_dataset_array ;

#define INC_3DARR 8

/*! Initialize a new AFNI dataset array into variable "name".

    You should declare "THD_3dim_dataset_array *name;".
*/
#define INIT_3DARR(name)                  \
   ( (name) = XtNew(THD_3dim_dataset_array) ,\
     (name)->num = (name)->nall = 0 ,     \
     (name)->ar  = NULL )

/*! Add dataset ddset to AFNI dataset array "name" */

#define ADDTO_3DARR(name,ddset)                                       \
   { if( (name)->num == (name)->nall ){                               \
      (name)->nall += INC_3DARR + (name)->nall/8 ;                    \
      (name)->ar    = (THD_3dim_dataset **)                           \
                       XtRealloc( (char *) (name)->ar ,               \
                        sizeof(THD_3dim_dataset *) * (name)->nall ) ; \
     }                                                             \
     if( (ddset) != NULL ){               \
      (name)->ar[(name)->num] = (ddset) ; \
      ((name)->num)++ ;                  \
     } }

/*! Free the AFNI dataset array (but don't kill the datasets).

    This would be used after the dataset pointers have been moved
    someplace else (e.g., into the THD_session structure).
*/

#define FREE_3DARR(name)      \
   if( (name) != NULL ){      \
     myXtFree( (name)->ar ) ; \
     myXtFree( (name) ) ; }

/*! Macro to access the nn-th dataset in AFNI dataset array name */

#define DSET_IN_3DARR(name,nn) ((name)->ar[(nn)])

/*! Determine if two datasets are properly ordered */

#define DSET_ORDERED(d1,d2)                  \
  ( ( (d1)->view_type < (d2)->view_type ) || \
    ( (d1)->view_type==(d2)->view_type && (d1)->func_type<(d2)->func_type ) )

/*! Swap 2 dataset pointers (thru pointer dt) */

#define DSET_SWAP(d1,d2) (dt=(d1),(d1)=(d2),(d2)=dt)

/*! Sort an AFNI dataset array */

#define SORT_3DARR(name)                                               \
   if( (name) != NULL && (name)->num > 1 ){                            \
      int iid , jjd ; THD_3dim_dataset * dt ;                          \
      for( iid=0 ; iid < (name)->num ; iid++ ){                        \
         for( jjd=1 ; jjd < (name)->num ; jjd++ ){                     \
            if( !DSET_ORDERED( (name)->ar[jjd-1] , (name)->ar[jjd] ) ) \
               DSET_SWAP( (name)->ar[jjd-1] , (name)->ar[jjd] ) ;      \
   }}}

/*-------------------------------------------------------------------*/
/*--------        holds all data from a session!          -----------*/

#define SESSION_TYPE 97

typedef struct { THD_3dim_dataset *drow[LAST_VIEW_TYPE+1] ; } THD_dsetrow ;

/*! Holds all the datasets from a directory (session).
    [28 Jul 2003: modified to put elide distinction between anat and func]
    [20 Jan 2004: modified to put surfaces into here as well]
*/

typedef struct {
      int type     ;                  /*!< code indicating this is a THD_session */
      int num_dsset ;                 /*!< Number of datasets. */
      char sessname[THD_MAX_NAME] ;   /*!< Name of directory datasets were read from */
      char lastname[THD_MAX_NAME] ;   /*!< Just/the/last/name of the directory */

      THD_3dim_dataset *dsset[THD_MAX_SESSION_SIZE][LAST_VIEW_TYPE+1] ;
                                      /*!< array of datasets */

      Htable *warptable ;       /*!< Table of inter-dataset warps [27 Aug 2002] */

      /* 20 Jan 2004: put surfaces here, rather than in the datasets */

      int su_num ;              /*!< Number of surfaces */
      SUMA_surface **su_surf ;  /*!< Surface array */

      int su_numgroup ;                  /*!< Number of surface groups */
      SUMA_surfacegroup **su_surfgroup ; /*!< Surface group array */

      XtPointer parent ;        /*!< generic pointer to "owner" of session */
} THD_session ;

/*! Determine if ss points to a valid THD_session. */

#define ISVALID_SESSION(ss) ( (ss) != NULL && (ss)->type == SESSION_TYPE )

/*! Initialize THD_session ss to hold nothing at all. */

#define BLANK_SESSION(ss)                                                     \
  if( ISVALID_SESSION((ss)) ){                                                \
      int id , vv ;                                                           \
      for( id=0 ; id < THD_MAX_SESSION_SIZE ; id++ )                          \
        for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ) (ss)->dsset[id][vv] = NULL; \
      (ss)->num_dsset = 0 ;                                                   \
      (ss)->su_num    = 0 ; (ss)->su_surf = NULL ;                            \
      (ss)->su_numgroup = 0 ; (ss)->su_surfgroup = NULL ;                     \
      (ss)->warptable = NULL ; }

/*! Determine if session has SUMA surface data attached. */

#define SESSION_HAS_SUMA(ss) ( (ss)!=NULL           &&  \
                               (ss)->su_surf!=NULL  &&  \
                               (ss)->su_num > 0        )

#define SESSIONLIST_TYPE 107

/*! Array of THD_sessions.

    Holds all the datasets read into AFNI from all directories.
*/

typedef struct {
      int type , num_sess ;
      THD_session * ssar[THD_MAX_NUM_SESSION] ;
      XtPointer parent ;
} THD_sessionlist ;

/*! Determine if sl is a valid THD_sessionlist */

#define ISVALID_SESSIONLIST(sl) ( (sl)!=NULL && (sl)->type==SESSIONLIST_TYPE )

/*! Initialize a THD_sessionlist to contain nothing. */

#define BLANK_SESSIONLIST(sl) \
   if( ISVALID_SESSIONLIST((sl)) ){ \
      int is ; \
      for( is=0 ; is < THD_MAX_NUM_SESSION ; is++ ) (sl)->ssar[is] = NULL ; \
      (sl)->num_sess = 0 ; }

/*! Return type for THD_sessionlist searching (see THD_dset_in_*).

    There are different ways to search for a dataset in THD_sessionlist
      - FIND_NAME    to find by the name field (is now obsolete)
      - FIND_IDCODE  to find by the dataset ID code (the best way)
      - FIND_PREFIX  to find by the dataset prefix (an OK way)
*/

typedef struct {
   int sess_index ;            /*!< Session it was found in */
   int dset_index ;            /*!< Index it was found at (if >= 0) */
   int view_index ;            /*!< View index it was found at (if >= 0) */
   THD_3dim_dataset * dset ;   /*!< Pointer to found dataset itself */
} THD_slist_find ;

/*! Set the find codes to indicate a bad result */

#define BADFIND(ff)                                       \
   ( (ff).sess_index=(ff).dset_index=(ff).view_index=-1 , \
     (ff).dset = NULL )

#define FIND_NAME   1
#define FIND_IDCODE 2
#define FIND_PREFIX 3

/*******************************************************************/
/********************** attribute names ****************************/

#define ATRNAME_DATANAME "DATASET_NAME"
#define ATRNAME_LABEL1   "LABEL_1"
#define ATRNAME_LABEL2   "LABEL_2"

#define ATRNAME_ANATOMY_PARENT "ANATOMY_PARENTNAME"

#define ATRNAME_ORIENT_SPECIFIC "ORIENT_SPECIFIC"
#define ATRTYPE_ORIENT_SPECIFIC ATR_INT_TYPE
#define ATRSIZE_ORIENT_SPECIFIC 3

#define ATRNAME_ORIENT_GENERAL "ORIENT_GENERAL"   /*** not used yet  ***/
#define ATRTYPE_ORIENT_GENERAL ATR_FLOAT_TYPE     /* (will someday be  */
#define ATRSIZE_ORIENT_GENERAL 9                  /*  rotation matrix) */

#define ATRNAME_ORIGIN "ORIGIN"
#define ATRTYPE_ORIGIN ATR_FLOAT_TYPE
#define ATRSIZE_ORIGIN 3

#define ATRNAME_DELTA  "DELTA"
#define ATRTYPE_DELTA  ATR_FLOAT_TYPE
#define ATRSIZE_DELTA  3

#define ATRNAME_SKIP   "SKIP"
#define ATRTYPE_SKIP   ATR_FLOAT_TYPE
#define ATRSIZE_SKIP   3

#define ATRNAME_MARKSXYZ  "MARKS_XYZ"
#define ATRTYPE_MARKSXYZ  ATR_FLOAT_TYPE
#define ATRSIZE_MARKSXYZ  MARKS_FSIZE

#define ATRNAME_MARKSLAB  "MARKS_LAB"
#define ATRTYPE_MARKSLAB  ATR_STRING_TYPE
#define ATRSIZE_MARKSLAB  MARKS_LSIZE

#define ATRNAME_MARKSHELP "MARKS_HELP"
#define ATRTYPE_MARKSHELP  ATR_STRING_TYPE
#define ATRSIZE_MARKSHELP  MARKS_HSIZE

#define ATRNAME_MARKSFLAG "MARKS_FLAGS"
#define ATRTYPE_MARKSFLAG  ATR_INT_TYPE
#define ATRSIZE_MARKSFLAG  MARKS_MAXFLAG

#define ATRNAME_TYPESTRING "TYPESTRING"
#define ATRTYPE_TYPESTRING ATR_STRING_TYPE
#define ATRSIZE_TYPESTRING 0                /* 0 size means variable */

#define ATRNAME_WARP_TYPE  "WARP_TYPE"
#define ATRTYPE_WARP_TYPE  ATR_INT_TYPE
#define ATRSIZE_WARP_TYPE  8           /* warp, resample (6 expansions) */

#define ATRNAME_WARP_DATA  "WARP_DATA"
#define ATRTYPE_WARP_DATA  ATR_FLOAT_TYPE
#define ATRSIZE_WARP_DATA  0

#define ATRNAME_WARP_DATA_3DWD_AF  "WARPDRIVE_MATVEC_INV_000000"  /* Talairach warp via 3dWarpDrive */
#define ATRTYPE_WARP_DATA_3DWD_AF  ATR_FLOAT_TYPE
#define ATRSIZE_WARP_DATA_3DWD_AF  0         /* not using this one. Calv. Cool. June 24 */

#define ATRNAME_WARP_PARENT "WARP_PARENTNAME"
#define ATRTYPE_WARP_PARENT ATR_STRING_TYPE
#define ATRSIZE_WARP_PARENT 0

#define ATRNAME_SCENE_TYPE "SCENE_DATA"
#define ATRTYPE_SCENE_TYPE ATR_INT_TYPE
#define ATRSIZE_SCENE_TYPE 8           /* view, func, type (+5) */

#define ATRNAME_DATASET_RANK "DATASET_RANK"
#define ATRTYPE_DATASET_RANK ATR_INT_TYPE
#define ATRSIZE_DATASET_RANK 8         /* # dims, # vals (+6) */

#define ATRNAME_DATASET_DIMENSIONS "DATASET_DIMENSIONS"
#define ATRTYPE_DATASET_DIMENSIONS ATR_INT_TYPE
#define ATRSIZE_DATASET_DIMENSIONS THD_MAX_RANK_EVER

#define ATRNAME_MINMAX "MINMAX"
#define ATRTYPE_MINMAX ATR_INT_TYPE

#if 0
#   define ATRNAME_DATASET_PREFIX  "DATASET_PREFIX"
#   define ATRTYPE_DATASET_PREFIX  ATR_STRING_TYPE
#   define ATRSIZE_DATASET_PREFIX  THD_MAX_PREFIX

#   define ATRNAME_DATASET_VIEWCODE  "DATASET_VIEWCODE"
#   define ATRTYPE_DATASET_VIEWCODE  ATR_STRING_TYPE
#   define ATRSIZE_DATASET_VIEWCODE  THD_MAX_VIEWCODE
#endif

/** additions 1995 Nov 15, for variable brick data types **/

#define ATRNAME_BRICK_TYPES    "BRICK_TYPES"
#define ATRTYPE_BRICK_TYPES    ATR_INT_TYPE
#define ATRSIZE_BRICK_TYPES    0

#define ATRNAME_BRICK_STATS    "BRICK_STATS"
#define ATRTYPE_BRICK_STATS    ATR_FLOAT_TYPE
#define ATRSIZE_BRICK_STATS    0

#define ATRNAME_BRICK_FLTFAC   "BRICK_FLOAT_FACS"
#define ATRTYPE_BRICK_FLTFAC   ATR_FLOAT_TYPE
#define ATRSIZE_BRICK_FLTFAC   0

/** 1996 Mar 26 **/

#define ATRNAME_STAT_AUX       "STAT_AUX"
#define ATRTYPE_STAT_AUX       ATR_FLOAT_TYPE
#define ATRSIZE_STAT_AUX       0

/** 1996 May 14 **/

#define ATRNAME_TAXIS_NUMS     "TAXIS_NUMS"
#define ATRSIZE_TAXIS_NUMS     8

#define ATRNAME_TAXIS_FLOATS   "TAXIS_FLOATS"
#define ATRSIZE_TAXIS_FLOATS   8

#define ATRNAME_TAXIS_OFFSETS  "TAXIS_OFFSETS"
#define ATRSIZE_TAXIS_OFFSETS  0

/** 30 Nov 1997 **/

#define ATRNAME_BRICK_LABS     "BRICK_LABS"
#define ATRNAME_BRICK_STATAUX  "BRICK_STATAUX"
#define ATRNAME_BRICK_KEYWORDS "BRICK_KEYWORDS"

#define ATRNAME_KEYWORDS       "DATASET_KEYWORDS"

#ifdef  __cplusplus
}
#endif

/************************************************************************/
/******************* rest of prototypes *********************************/

#include <stdarg.h>
#ifdef  __cplusplus
extern "C" {
#endif

#ifndef DONT_USE_SCANDIR
#ifdef SCANDIR_WANTS_CONST
   extern int THD_select_dirent( const struct dirent * dp ) ;
#else
   extern int THD_select_dirent( struct dirent * dp ) ;
#endif
#endif

char * ig_strstr( char *, char *, char * ) ; /* 08 Aug 2002 */
void freeup_strings( int n , char **sar ) ;
int breakup_string( char *sin , char ***stok ) ;

extern THD_string_array * THD_get_all_filenames( char * ) ;
extern THD_string_array * THD_extract_regular_files( THD_string_array * ) ;
extern THD_string_array * THD_extract_directories( THD_string_array * ) ;
extern int THD_is_file     ( char * ) ;
extern int THD_is_symlink  ( char * ) ;  /* 03 Mar 1999 */
extern int THD_is_directory( char * ) ;
extern int THD_is_ondisk   ( char * ) ;  /* 19 Dec 2002 */
extern int THD_mkdir       ( char * ) ;  /* 19 Dec 2002 */
extern int THD_cwd         ( char * ) ;  /* 19 Dec 2002 */
extern int THD_equiv_files ( char * , char * ) ;
extern long long THD_filesize( char * pathname ) ;
extern THD_string_array * THD_get_all_subdirs( int , char * ) ;
extern THD_string_array * THD_normalize_flist( THD_string_array * ) ;
extern THD_string_array * THD_get_wildcard_filenames( char * ) ;

extern int THD_check_for_duplicates( int, char **, int ) ; /* 31 May 2007 */

extern time_t THD_file_mtime( char * ) ; /* 05 Dec 2001 */

extern THD_string_array * THD_get_all_executables( char * ) ;    /* 26 Jun 2001 */
extern THD_string_array * THD_getpathprogs( THD_string_array * );
extern int THD_is_executable( char * pathname ) ;
extern char * THD_find_executable( char * ) ;

extern int THD_is_dataset( char * , char * , int ) ; /* 17 Mar 2000 */
extern char * THD_dataset_headname( char * , char * , int ) ;

extern MRI_IMARR * THD_get_all_timeseries( char * ) ;
extern MRI_IMARR * THD_get_many_timeseries( THD_string_array * ) ;
extern char * THD_trailname( char * fname , int lev ) ;

extern int THD_linecount( char * ) ;

extern void THD_read_all_atr ( char * , THD_datablock * ) ;
extern void THD_erase_all_atr( THD_datablock * ) ;
extern void THD_erase_one_atr( THD_datablock * , char * ) ;
extern void THD_read_niml_atr( char * , THD_datablock * ) ; /* 01 Jun 2005 */

extern void THD_anonymize_dset ( THD_3dim_dataset * ) ;  /* 08 Jul 2005 */
extern void THD_anonymize_write( int ) ;

extern ATR_any    * THD_find_atr       ( THD_datablock * , char * ) ;
extern ATR_float  * THD_find_float_atr ( THD_datablock * , char * ) ;
extern ATR_int    * THD_find_int_atr   ( THD_datablock * , char * ) ;
extern ATR_string * THD_find_string_atr( THD_datablock * , char * ) ;

extern void THD_set_atr( THD_datablock * , char * , int,int, void * ) ;

extern ATR_any * THD_copy_atr( ATR_any *atr ) ;  /* 03 Aug 2005 */
extern void THD_insert_atr( THD_datablock *blk , ATR_any *atr ) ;

extern void THD_store_dataset_keywords ( THD_3dim_dataset * , char * ) ;
extern void THD_append_dataset_keywords( THD_3dim_dataset * , char * ) ;
extern char * THD_dataset_info( THD_3dim_dataset * , int ) ;
extern char * THD_zzprintf( char * sss , char * fmt , ... ) ;

extern void THD_set_float_atr( THD_datablock * , char * , int , float * ) ;
extern void THD_set_int_atr  ( THD_datablock * , char * , int , int   * ) ;
extern void THD_set_char_atr ( THD_datablock * , char * , int , char  * ) ;

/*! Macro to set a string attribute from a C string (vs. a char array). */

#define THD_set_string_atr(blk,name,str) \
   THD_set_char_atr( (blk) , (name) , strlen(str)+1 , (str) )

extern void THD_init_diskptr_names( THD_diskptr *, char *,char *,char * ,
                                    int, Boolean ) ;

extern THD_datablock *       THD_init_one_datablock( char *,char * ) ;
extern THD_datablock_array * THD_init_prefix_datablocks( char *, THD_string_array * ) ;

extern XtPointer_array * THD_init_alldir_datablocks( char * ) ;

extern THD_session * THD_init_session( char * ) ;
extern void          THD_order_session( THD_session * ) ;   /* 29 Jul 2003 */

extern char * Add_plausible_path(char *fname);              /* ZSS:Aug. 08 */
extern THD_3dim_dataset * THD_open_one_dataset( char * ) ;
extern THD_3dim_dataset * THD_open_dataset( char * ) ;      /* 11 Jan 1999 */
extern THD_3dim_dataset * THD_open_minc( char * ) ;         /* 29 Oct 2001 */
extern THD_3dim_dataset * THD_open_analyze( char * ) ;      /* 27 Aug 2002 */
extern THD_3dim_dataset * THD_open_ctfmri( char * ) ;       /* 04 Dec 2002 */
extern THD_3dim_dataset * THD_open_ctfsam( char * ) ;       /* 04 Dec 2002 */
extern THD_3dim_dataset * THD_open_1D( char * ) ;           /* 04 Mar 2003 */
extern THD_3dim_dataset * THD_open_3D( char * ) ;           /* 21 Mar 2003 */
extern THD_3dim_dataset * THD_open_nifti( char * ) ;        /* 28 Aug 2003 */
extern THD_3dim_dataset * THD_open_mpeg( char * ) ;         /* 03 Dec 2003 */
extern THD_3dim_dataset * THD_open_tcat( char * ) ;         /* 04 Aug 2004 */
extern THD_3dim_dataset * THD_open_niml( char * ) ;         /* 01 Jun 2006 */
extern THD_3dim_dataset * THD_open_gifti( char * ) ;        /* 13 Feb 2008 */

extern THD_string_array * THD_multiplex_dataset( char * ) ; /* 19 Jul 2007 */

extern THD_3dim_dataset * THD_niml_3D_to_dataset( NI_element *, char * ) ;
extern THD_3dim_dataset * THD_ni_surf_dset_to_afni( NI_group *, int ) ;
extern void * read_niml_file( char *, int ) ;
extern int    storage_mode_from_niml( void * ) ;

extern int        NI_write_gifti( NI_group *, char * , int);
extern NI_group * NI_read_gifti( char * , int ) ;

extern int storage_mode_from_filename( char * fname );      /* 20 Apr 2006 */
extern int has_known_non_afni_extension( char * fname ) ;   /*     [rickr] */
extern char * find_filename_extension( char * fname );

extern void THD_datablock_apply_atr( THD_3dim_dataset * ) ; /* 09 May 2005 */

extern THD_3dim_dataset * THD_fetch_dataset      (char *) ; /* 23 Mar 2001 */
extern XtPointer_array *  THD_fetch_many_datasets(char *) ;
extern MRI_IMAGE *        THD_fetch_1D           (char *) ; /* 26 Mar 2001 */

extern void THD_set_storage_mode( THD_3dim_dataset *,int ); /* 21 Mar 2003 */

extern int * get_count_intlist ( char *str , int *nret);
extern int * MCW_get_intlist( int , char * ) ;
extern void MCW_intlist_allow_negative( int ) ;             /* 22 Nov 1999 */

/* copy a dataset, given a list of sub-bricks          [rickr] 26 Jul 2004 */
extern THD_3dim_dataset * THD_copy_dset_subs( THD_3dim_dataset * , int * ) ;

/*! Help string to explain dataset "mastering" briefly. */

#define MASTER_SHORTHELP_STRING                                                 \
 "INPUT DATASET NAMES\n"                                                        \
 "-------------------\n"                                                        \
 "This program accepts datasets that are modified on input according to the\n"  \
 "following schemes:\n"                                                         \
 "  'r1+orig[3..5]'                                    {sub-brick selector}\n"  \
 "  'r1+orig<100..200>'                                {sub-range selector}\n"  \
 "  'r1+orig[3..5]<100..200>'                          {both selectors}\n"      \
 "  '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}\n"         \
 "For the gruesome details, see the output of 'afni -help'.\n"

/*! Help string to explain dataset "mastering" at length. */

#define MASTER_HELP_STRING                                                    \
    "INPUT DATASET NAMES\n"                                                   \
    "-------------------\n"                                                   \
    " An input dataset is specified using one of these forms:\n"              \
    "    'prefix+view', 'prefix+view.HEAD', or 'prefix+view.BRIK'.\n"         \
    " You can also add a sub-brick selection list after the end of the\n"     \
    " dataset name.  This allows only a subset of the sub-bricks to be\n"     \
    " read in (by default, all of a dataset's sub-bricks are input).\n"       \
    " A sub-brick selection list looks like one of the following forms:\n"    \
    "   fred+orig[5]                     ==> use only sub-brick #5\n"         \
    "   fred+orig[5,9,17]                ==> use #5, #9, and #17\n"           \
    "   fred+orig[5..8]     or [5-8]     ==> use #5, #6, #7, and #8\n"        \
    "   fred+orig[5..13(2)] or [5-13(2)] ==> use #5, #7, #9, #11, and #13\n"  \
    " Sub-brick indexes start at 0.  You can use the character '$'\n"         \
    " to indicate the last sub-brick in a dataset; for example, you\n"        \
    " can select every third sub-brick by using the selection list\n"         \
    "   fred+orig[0..$(3)]\n"                                                 \
    "\n"                                                                      \
    " N.B.: The sub-bricks are read in the order specified, which may\n"      \
    " not be the order in the original dataset.  For example, using\n"        \
    "   fred+orig[0..$(2),1..$(2)]\n"                                         \
    " will cause the sub-bricks in fred+orig to be input into memory\n"       \
    " in an interleaved fashion.  Using\n"                                    \
    "   fred+orig[$..0]\n"                                                    \
    " will reverse the order of the sub-bricks.\n"                            \
    "\n"                                                                      \
    " N.B.: You may also use the syntax <a..b> after the name of an input \n" \
    " dataset to restrict the range of values read in to the numerical\n"     \
    " values in a..b, inclusive.  For example,\n"                             \
    "    fred+orig[5..7]<100..200>\n"                                         \
    " creates a 3 sub-brick dataset with values less than 100 or\n"           \
    " greater than 200 from the original set to zero.\n"                      \
    " If you use the <> sub-range selection without the [] sub-brick\n"       \
    " selection, it is the same as if you had put [0..$] in front of\n"       \
    " the sub-range selection.\n"                                             \
    "\n"                                                                      \
    " N.B.: Datasets using sub-brick/sub-range selectors are treated as:\n"   \
    "  - 3D+time if the dataset is 3D+time and more than 1 brick is chosen\n" \
    "  - otherwise, as bucket datasets (-abuc or -fbuc)\n"                    \
    "    (in particular, fico, fitt, etc datasets are converted to fbuc!)\n"  \
    "\n"                                                                      \
    " N.B.: The characters '$ ( ) [ ] < >'  are special to the shell,\n"      \
    " so you will have to escape them.  This is most easily done by\n"        \
    " putting the entire dataset plus selection list inside forward\n"        \
    " single quotes, as in 'fred+orig[5..7,9]', or double quotes \"x\".\n"

/*! Help string to explain calculated datasets. */

#define CALC_HELP_STRING                                                   \
   "CALCULATED DATASETS\n"                                                 \
   "-------------------\n"                                                 \
   " Datasets may also be specified as runtime-generated results from\n"   \
   " program 3dcalc.  This type of dataset specifier is enclosed in\n"     \
   " quotes, and starts with the string '3dcalc(':\n"                      \
   "    '3dcalc( opt opt ... opt )'\n"                                     \
   " where each 'opt' is an option to program 3dcalc; this program\n"      \
   " is run to generate a dataset in the directory given by environment\n" \
   " variable TMPDIR (default=/tmp).  This dataset is then read into\n"    \
   " memory, locked in place, and deleted from disk.  For example\n"       \
   "    afni -dset '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'\n"    \
   " will let you look at the average of datasets r1+orig and r2+orig.\n"  \
   " N.B.: using this dataset input method will use lots of memory!\n"

/*! Help string to explain 1D column and row selection. [01 May 2003] */

#define TS_HELP_STRING                                                        \
   "TIMESERIES (1D) INPUT\n"                                                  \
   "---------------------\n"                                                  \
   "A timeseries file is in the form of a 1D or 2D table of ASCII numbers;\n" \
   "for example:   3 5 7\n"                                                   \
   "               2 4 6\n"                                                   \
   "               0 3 3\n"                                                   \
   "               7 2 9\n"                                                   \
   "This example has 4 rows and 3 columns.  Each column is considered as\n"   \
   "a timeseries in AFNI.  The convention is to store this type of data\n"    \
   "in a filename ending in '.1D'.\n"                                         \
   "\n"                                                                       \
   "** COLUMN SELECTION WITH [] **\n"                                         \
   "When specifying a timeseries file to an command-line AFNI program, you\n" \
   "can select a subset of columns using the '[...]' notation:\n"             \
   "  'fred.1D[5]'            ==> use only column #5\n"                       \
   "  'fred.1D[5,9,17]'       ==> use columns #5, #9, and #17\n"              \
   "  'fred.1D[5..8]'         ==> use columns #5, #6, #7, and #8\n"           \
   "  'fred.1D[5..13(2)]'     ==> use columns #5, #7, #9, #11, and #13\n"     \
   "Column indices start at 0.  You can use the character '$'\n"           \
   "to indicate the last column in a 1D file; for example, you\n"          \
   "can select every third column in a 1D file by using the selection list\n"           \
   "  'fred.1D[0..$(3)]'      ==> use columns #0, #3, #6, #9, ....\n"         \
   "\n"                                                                       \
   "** ROW SELECTION WITH {} **\n"                                            \
   "Similarly, you select a subset of the rows using the '{...}' notation:\n" \
   "  'fred.1D{0..$(2)}'      ==> use rows #0, #2, #4, ....\n"                \
   "You can also use both notations together, as in\n"                        \
   "  'fred.1D[1,3]{1..$(2)}' ==> columns #1 and #3; rows #1, #3, #5, ....\n" \
   "\n"                                                                       \
   "** DIRECT INPUT OF DATA ON THE COMMAND LINE WITH 1D: **\n"                \
   "You can also input a 1D time series 'dataset' directly on the command\n"  \
   "line, without an external file. The 'filename' for such input has the\n"  \
   "general format\n"                                                         \
   "  '1D:n_1@val_1,n_2@val_2,n_3@val_3,...'\n"                               \
   "where each 'n_i' is an integer and each 'val_i' is a float.  For\n"       \
   "example\n"                                                                \
   "   -a '1D:5@0,10@1,5@0,10@1,5@0'\n"                                       \
   "specifies that variable 'a' be assigned to a 1D time series of 35,\n"     \
   "alternating in blocks between values 0 and value 1.\n"                    \
   " * Spaces or commas can be used to separate values.\n"                    \
   " * A '|' character can be used to start a new input \"line\":\n"          \
   "   Try 1dplot '1D: 3 4 3 5 | 3 5 4 3'\n"                                  \
   "\n"                                                                       \
   "** TRANSPOSITION WITH \\' **\n"                                           \
   "Finally, you can force most AFNI programs to tranpose a 1D file on\n"     \
   "input by appending a single ' character at the end of the filename.\n"    \
   "N.B.: Since the ' character is also special to the shell, you'll\n"       \
   "      probably have to put a \\ character before it. Examples:\n"         \
   "       1dplot '1D: 3 2 3 4 | 2 3 4 3'   and\n"                            \
   "       1dplot '1D: 3 2 3 4 | 2 3 4 3'\\'\n"                               \
   "When you have reached this level of understanding, you are ready to\n"    \
   "take the AFNI Jedi Master test.  I won't insult you by telling you\n"     \
   "where to find this examination.\n"

extern void THD_delete_3dim_dataset( THD_3dim_dataset * , Boolean ) ;
extern THD_3dim_dataset * THD_3dim_from_block( THD_datablock * ) ;
extern void THD_allow_empty_dataset( int ) ; /* 23 Mar 2001 */
extern THD_3dim_dataset_array *
   THD_array_3dim_from_block( THD_datablock_array * blk_arr ) ;

extern Boolean THD_write_3dim_dataset( char *,char * ,
                                       THD_3dim_dataset * , Boolean );

extern void THD_use_3D_format   ( int ) ;  /* 21 Mar 2003 */
extern void THD_use_NIFTI_format( int ) ;  /* 06 Apr 2005 */

extern Boolean THD_write_datablock( THD_datablock * , Boolean ) ;
extern Boolean THD_write_atr( THD_datablock * ) ;
extern Boolean THD_write_nimlatr( THD_datablock * ) ;  /* 01 Jun 2005 */
extern void THD_set_write_compression( int mm ) ;
extern int THD_enviro_write_compression(void) ;
extern int THD_get_write_compression(void) ;

extern void THD_set_write_order( int ) ;
extern void THD_enviro_write_order(void) ;
extern int THD_get_write_order(void) ;

extern int TRUST_host(char *) ;
#define OKHOST(hh) TRUST_host(hh) ;
extern void TRUST_addhost(char *) ;      /* 21 Feb 2001 */

extern Boolean THD_load_datablock( THD_datablock * ) ;
extern void    THD_load_datablock_verbose(int) ;             /* 21 Aug 2002 */
extern void    THD_set_freeup( generic_func * ) ;            /* 18 Oct 2001 */
extern Boolean THD_purge_datablock( THD_datablock * , int ) ;
extern Boolean THD_purge_one_brick( THD_datablock * , int ) ;
extern void    THD_force_malloc_type( THD_datablock * , int ) ;
extern int     THD_count_databricks( THD_datablock * dblk ) ;
extern void    THD_load_minc( THD_datablock * ) ;            /* 29 Oct 2001 */
extern void    THD_load_analyze( THD_datablock * ) ;         /* 27 Aug 2002 */
extern void    THD_load_ctfmri ( THD_datablock * ) ;         /* 04 Dec 2002 */
extern void    THD_load_ctfsam ( THD_datablock * ) ;         /* 04 Dec 2002 */
extern void    THD_load_1D     ( THD_datablock * ) ;         /* 04 Mar 2003 */
extern void    THD_load_3D     ( THD_datablock * ) ;         /* 21 Mar 2003 */
extern void    THD_load_nifti  ( THD_datablock * ) ;         /* 28 Aug 2003 */
extern void    THD_load_mpeg   ( THD_datablock * ) ;         /* 03 Dec 2003 */
extern void    THD_load_tcat   ( THD_datablock * ) ;         /* 04 Aug 2004 */
extern int     THD_load_niml   ( THD_datablock * ) ;         /* 12 Jun 2006 */
extern int     THD_load_gifti  ( THD_datablock * ) ;         /* 13 Feb 2008 */

extern int     THD_count_potential_databricks( THD_datablock *dblk );

extern void    THD_zerofill_dataset( THD_3dim_dataset * ) ;  /* 18 Mar 2005 */
extern int     THD_apply_master_subrange( THD_datablock * ); /* 14 Apr 2006 */
extern void    THD_patch_brickim( THD_3dim_dataset * ) ;     /* 20 Oct 2006 */

extern int THD_datum_constant( THD_datablock * ) ;           /* 30 Aug 2002 */
#define DSET_datum_constant(ds) THD_datum_constant((ds)->dblk)

#define ALLOW_FSL_FEAT  /* 27 Aug 2002 */

#define MINC_FLOATIZE_MASK 1
#define MINC_SWAPIZE_MASK 1<<1
extern int THD_write_minc( char *, THD_3dim_dataset * , int) ; /* 11 Apr 2002 */

extern void THD_write_1D( char *, char *, THD_3dim_dataset *); /* 04 Mar 2003 */
extern void THD_write_3D( char *, char *, THD_3dim_dataset *); /* 21 Mar 2003 */
extern Boolean THD_write_niml( THD_3dim_dataset *, int);
extern Boolean THD_write_gifti( THD_3dim_dataset *, int, int);

extern int  write_niml_file( char *, NI_group *);      /* 12 Jun 2006 [rickr] */

extern void THD_reconcile_parents( THD_sessionlist * ) ;
extern THD_slist_find THD_dset_in_sessionlist( int,void *, THD_sessionlist *, int ) ;
extern THD_slist_find THD_dset_in_session( int,void * , THD_session * ) ;

extern void THD_check_idcodes( THD_sessionlist * ) ; /* 08 Jun 1999 */

extern void THD_load_statistics( THD_3dim_dataset * ) ;
extern void THD_update_statistics( THD_3dim_dataset * ) ;
extern THD_brick_stats THD_get_brick_stats( MRI_IMAGE * ) ;
extern void THD_update_one_bstat( THD_3dim_dataset * , int ) ; /* 29 Mar 2005 */

extern THD_fvec3 THD_3dind_to_3dmm( THD_3dim_dataset * , THD_ivec3 ) ;
extern THD_fvec3 THD_3dind_to_3dmm_no_wod( THD_3dim_dataset * , THD_ivec3 ) ;
extern THD_ivec3 THD_3dmm_to_3dind( THD_3dim_dataset * , THD_fvec3 ) ;
extern THD_ivec3 THD_3dmm_to_3dind_warn( THD_3dim_dataset * , THD_fvec3, int * ) ;
extern THD_ivec3 THD_3dmm_to_3dind_no_wod( THD_3dim_dataset * , THD_fvec3 ) ;
                                                   /* 28 Sep 2004  [rickr] */

extern THD_fvec3 THD_3dfind_to_3dmm( THD_3dim_dataset * , THD_fvec3 ) ;
extern THD_fvec3 THD_3dmm_to_3dfind( THD_3dim_dataset * , THD_fvec3 ) ;

extern THD_fvec3 THD_3dmm_to_dicomm( THD_3dim_dataset * , THD_fvec3 ) ;
extern THD_fvec3 THD_dicomm_to_3dmm( THD_3dim_dataset * , THD_fvec3 ) ;

extern THD_fvec3 THD_tta_to_mni( THD_fvec3 ) ;  /* 29 Apr 2002 */
extern THD_fvec3 THD_mni_to_tta( THD_fvec3 ) ;
extern void THD_3mni_to_3tta( float *, float *, float *) ;
extern void THD_3tta_to_3mni( float *, float *, float *) ;

extern float THD_timeof      ( int , float , THD_timeaxis * ) ;
extern float THD_timeof_vox  ( int , int , THD_3dim_dataset * ) ;
extern float THD_timeof_slice( int , int , THD_3dim_dataset * ) ;  /* BDW */

extern float * TS_parse_tpattern( int, float, char * ) ;  /* 11 Dec 2007 */

extern THD_fvec3 THD_dataset_center( THD_3dim_dataset * ) ;  /* 01 Feb 2001 */
extern int THD_dataset_mismatch(THD_3dim_dataset *, THD_3dim_dataset *) ;

extern int THD_dataset_tshift( THD_3dim_dataset * , int ) ; /* 15 Feb 2001 */

#define MISMATCH_CENTER  (1<<0)  /* within 0.2 voxel */
#define MISMATCH_DELTA   (1<<1)  /* within 0.001 voxel */
#define MISMATCH_ORIENT  (1<<2)
#define MISMATCH_DIMEN   (1<<3)

/*----------------------------------------------------------------*/
/*--------  FD_brick type: for rapid extraction of slices --------*/

/*! This type is to hold information needed for the rapid extraction
           of slices from an AFNI dataset (THD_3dim_dataset struct).

    It exists primarily as a historical artifact.  The earliest version
    of AFNI was to be called FD3, as a successor to FD2.  The FD_brick
    was conceived as part of FD3.  However, FD3 morphed into AFNI within
    a few weeks, but by then I didn't want to throw away the code that
    had already been structured around this (primarily the imseq.c stuff).
*/

typedef struct FD_brick {

   THD_ivec3 nxyz ;     /*!< actual dimensions as read in */
   THD_ivec3 sxyz ;     /*!< starting indices in each dataset dimen */
   THD_ivec3 a123 ;     /*!< axis codes as supplied in THD_3dim_dataset_to_brick */

   int n1 ;             /*!< ni = length in direction i */
   int d1 ;             /*!< di = stride in direction i */
   int e1 ;             /*!< ei = last index in direc i */
   int n2 ;             /*!< ni = length in direction i */
   int d2 ;             /*!< di = stride in direction i */
   int e2 ;             /*!< ei = last index in direc i */
   int n3 ;             /*!< ni = length in direction i */
   int d3 ;             /*!< di = stride in direction i */
   int start ;          /*!< start = offset of 1st elem */

   float del1 ;         /*!< voxel dimensions */
   float del2 ;         /*!< voxel dimensions */
   float del3 ;         /*!< voxel dimensions */

   THD_3dim_dataset * dset ;    /*!< pointer to parent dataset */
   int resam_code ;             /*!< how to resample normal sub-bricks */
   int thr_resam_code ;         /*!< how to resample statistical sub-bricks */

   char namecode[32] ;          /*!< June 1997 */

   XtPointer parent ;           /*!< struct owner */
} FD_brick ;

/*! rotate the three numbers (a,b,c) to (b,c,a) into (na,nb,nc) */

#define ROT3(a,b,c,na,nb,nc) ((na)=(b),(nb)=(c),(nc)=(a))

/*! Determine if this FD_brick can be drawn (in an image or graph) */

#define BRICK_DRAWABLE(br)  ((br)->n1 >  1 && (br)->n2 >  1)
#define BRICK_GRAPHABLE(br) ((br)->n1 >= 1 && (br)->n2 >= 1)

extern FD_brick * THD_3dim_dataset_to_brick( THD_3dim_dataset * ,
                                             int,int,int ) ;

extern MRI_IMAGE * FD_brick_to_mri( int,int , FD_brick * br ) ;
extern MRI_IMAGE * FD_brick_to_series( int , FD_brick * br ) ;

extern float THD_get_voxel( THD_3dim_dataset *dset , int ijk , int ival ) ;

extern MRI_IMAGE * THD_extract_series( int , THD_3dim_dataset * , int ) ;
extern MRI_IMARR * THD_extract_many_series( int, int *, THD_3dim_dataset * );
extern MRI_IMAGE * THD_dset_to_1Dmri( THD_3dim_dataset *dset ) ;

extern int THD_extract_array( int, THD_3dim_dataset *, int, void * ) ;

extern MRI_IMAGE * THD_extract_float_brick( int , THD_3dim_dataset * ) ;

extern void THD_insert_series( int, THD_3dim_dataset *, int, int, void *, int );

extern int THD_voxel_is_constant( int ind , THD_3dim_dataset *dset ) ;

extern floatvec * THD_fitter( int npt , float *far  ,
                              int nref, float *ref[], int meth, float *ccon ) ;

extern floatvec * THD_deconvolve( int npt    , float *far   ,
                                  int minlag , int maxlag   , float *kern,
                                  int nbase  , float *base[],
                                  int meth   , float *ccon  , int dcon   ,
                                  int pencode, float penpar               ) ;

extern floatvec * THD_fitter_fitts( int npt, floatvec *fv,
                                    int nref, float *ref[], float *far ) ;

extern void       THD_fitter_do_fitts(int qq) ;
extern floatvec * THD_retrieve_fitts(void) ;
extern void       THD_fitter_voxid( int i ) ;  /* 10 Sep 2008 */

/*--------------- routines that are in thd_detrend.c ---------------*/

extern void get_linear_trend     ( int, float *, float *, float * ) ;
extern void THD_linear_detrend   ( int, float *, float *, float * ) ;
extern void get_quadratic_trend  ( int, float *, float *, float *, float * ) ;
extern void THD_quadratic_detrend( int, float *, float *, float *, float * ) ;
extern void THD_normalize        ( int, float * ) ;
extern void THD_normRMS          ( int, float * ) ;  /* 06 Jun 2008 */
extern void THD_normmax          ( int, float * ) ;  /* 26 Mar 2008 */
extern void THD_normL1           ( int, float * ) ;  /* 26 Mar 2008 */
extern void THD_cubic_detrend    ( int, float * ) ;  /* 15 Nov 1999 */

extern void THD_const_detrend    ( int, float *, float * ); /* 24 Aug 2001 */
void THD_linear_detrend_complex  ( int, complex * ); /* 05 Mar 2007 */

extern void THD_generic_detrend_LSQ( int, float *, int, int, float **, float *) ;
extern void THD_generic_detrend_L1 ( int, float *, int, int, float **, float *) ;
extern void THD_generic_retrend    ( int, float *, int, int, float **, float *) ;

extern MRI_IMARR * THD_time_fit_dataset( THD_3dim_dataset *, int, float **, int, byte *);
extern void THD_extract_detrended_array( THD_3dim_dataset * ,
                                         int, float **, MRI_IMARR *,
                                         int, int, float * ) ;

extern THD_3dim_dataset * THD_detrend_dataset( THD_3dim_dataset *dset ,
                                               int nref , float **ref ,
                                               int meth , int scl ,
                                               byte *mask , MRI_IMARR **imar ) ;

extern int THD_retrend_dataset( THD_3dim_dataset *dset ,
                                int nref , float **ref ,
                                int scl , byte *mask , MRI_IMARR *imar ) ;

extern float ** THD_build_trigref( int corder , int nvals ) ;
extern float ** THD_build_polyref( int nref   , int nvals ) ; /* 20 Sep 2007 */

#define DETREND_linear(n,f)    THD_linear_detrend(n,f,NULL,NULL)
#define DETREND_quadratic(n,f) THD_quadratic_detrend(n,f,NULL,NULL,NULL)
#define DETREND_cubic(n,f)     THD_cubic_detrend(n,f)
#define DETREND_const(n,f)     THD_const_detrend(n,f,NULL)

/*! Macro to detrend a time series array in to various polynomial orders. */

#define DETREND_polort(p,n,f)                            \
 do{ switch(p){ default:                         break;  \
                 case 0: DETREND_const(n,f)    ; break;  \
                 case 1: DETREND_linear(n,f)   ; break;  \
                 case 2: DETREND_quadratic(n,f); break;  \
                 case 3: DETREND_cubic(n,f)    ; break; } } while(0)

/*------------------------------------------------------------------*/

extern THD_ivec3 THD_fdind_to_3dind( FD_brick * , THD_ivec3 ) ;
extern THD_ivec3 THD_3dind_to_fdind( FD_brick * , THD_ivec3 ) ;

extern THD_fvec3 THD_fdfind_to_3dfind( FD_brick *, THD_fvec3) ; /* 30 Aug 2001 */
extern THD_fvec3 THD_3dfind_to_fdfind( FD_brick *, THD_fvec3) ;

extern FD_brick ** THD_setup_bricks( THD_3dim_dataset * ) ;

extern FD_brick * THD_oriented_brick( THD_3dim_dataset *, char *) ; /* 07 Dec 2001 */

extern int thd_floatscan  ( int , float *   ) ; /* 30 Jul 1999 */
extern int thd_complexscan( int , complex * ) ; /* 14 Sep 1999 */

extern int mri_floatscan  ( MRI_IMAGE * ) ;     /* 22 Feb 2007 */
extern int imarr_floatscan( MRI_IMARR * ) ;
extern int dblk_floatscan ( THD_datablock * ) ;
extern int dset_floatscan ( THD_3dim_dataset * ) ;

extern byte * THD_makemask( THD_3dim_dataset *, int,float,float) ;
extern int    THD_makedsetmask( THD_3dim_dataset *, int,float,float, byte*cmask) ;
extern int *THD_unique_vals( THD_3dim_dataset *mask_dset , int miv, int *n_unique, byte*cmask );

extern int    THD_countmask( int , byte * ) ;
extern byte * THD_automask( THD_3dim_dataset * ) ;         /* 13 Aug 2001 */
extern void   THD_automask_verbose( int ) ;                /* 28 Oct 2003 */
extern void   THD_automask_extclip( int ) ;
extern byte * mri_automask_image( MRI_IMAGE * ) ;          /* 05 Mar 2003 */
extern byte * mri_automask_imarr( MRI_IMARR * ) ;          /* 18 Nov 2004 */

                                                   /* 13 Nov 2006 [rickr] */
extern int    thd_mask_from_brick(THD_3dim_dataset *, int, float, byte **, int);
extern int    thd_multi_mask_from_brick(THD_3dim_dataset *, int, byte **);


extern void THD_autobbox( THD_3dim_dataset * ,             /* 06 Jun 2002 */
                          int *, int * , int *, int * , int *, int * ) ;
extern void MRI_autobbox( MRI_IMAGE * ,
                          int *, int * , int *, int * , int *, int * ) ;
extern void MRI_autobbox_clust( int ) ;                    /* 20 Sep 2006 */
extern void THD_autobbox_clip( int ) ;                     /* 06 Aug 2007 */

extern void THD_automask_set_clipfrac( float f ) ;         /* 20 Mar 2006 */
extern void THD_automask_set_peelcounts( int,int ) ;       /* 24 Oct 2006 */
extern void THD_automask_set_gradualize( int ) ;
extern void THD_automask_set_cheapo( int n ) ;             /* 13 Aug 2007 */

extern int THD_mask_fillin_completely( int,int,int, byte *, int ) ; /* 19 Apr 2002 */
extern int THD_mask_fillin_once      ( int,int,int, byte *, int ) ;

extern int THD_mask_clip_neighbors( int,int,int, byte *, float,float,float *) ; /* 28 Oct 2003 */

extern void THD_mask_clust( int nx, int ny, int nz, byte *mmm ) ;
extern void THD_mask_erode( int nx, int ny, int nz, byte *mmm, int redilate ) ;
extern void THD_mask_erodemany( int nx, int ny, int nz, byte *mmm, int npeel ) ; /* 24 Oct 2006 */

extern int THD_peel_mask( int nx, int ny, int nz , byte *mmm, int pdepth ) ;

extern void THD_mask_dilate( int, int, int, byte *, int ) ;  /* 30 Aug 2002 */

extern float THD_cliplevel( MRI_IMAGE * , float ) ;          /* 12 Aug 2001 */
extern float THD_cliplevel_abs( MRI_IMAGE * , float ) ;      /* 05 Mar 2007 */
extern float mri_topclip( MRI_IMAGE * ) ;                    /* 28 Sep 2006 */
extern MRI_IMAGE * THD_median_brick( THD_3dim_dataset * ) ;  /* 12 Aug 2001 */
extern MRI_IMAGE * THD_mad_brick   ( THD_3dim_dataset * ) ;  /* 07 Dec 2006 */
extern MRI_IMAGE * THD_mean_brick  ( THD_3dim_dataset * ) ;  /* 15 Apr 2005 */
extern MRI_IMAGE * THD_rms_brick   ( THD_3dim_dataset * ) ;  /* 15 Apr 2005 */

extern MRI_IMARR * THD_medmad_bricks   (THD_3dim_dataset *); /* 07 Dec 2006 */
extern MRI_IMARR * THD_meansigma_bricks(THD_3dim_dataset *); /* 07 Dec 2006 */
extern MRI_IMARR * IMARR_medmad_bricks ( MRI_IMARR * )     ; /* 11 Dec 2006 */

extern float THD_cliplevel_partial( MRI_IMAGE *im , float mfrac ,
                                    int xa,int xb, int ya,int yb, int za,int zb ) ;
extern MRI_IMAGE * THD_cliplevel_gradual( MRI_IMAGE *im , float mfrac ) ;

 /* 08 Mar 2001 - functions for dealing with rows */

extern int THD_get_dset_rowcount( THD_3dim_dataset *, int ) ;
extern void * THD_get_dset_row( THD_3dim_dataset *, int, int, int,int,int ) ;
extern void THD_put_dset_row( THD_3dim_dataset *, int,
                              int, int,int,int, void * row ) ;
extern int THD_dataset_rowfillin( THD_3dim_dataset *, int, int, int ) ;
extern int THD_dataset_zfillin( THD_3dim_dataset *, int, int, int ) ; /* 03 Jul 2001 */

extern MRI_IMAGE * mri_get_dset_row( THD_3dim_dataset *, int , int,int,int,int ) ;

/*------------------------------------------------------------------*/
/*-- October 1998: routines for 3D volume rotation and alignment. --*/

#define DELTA_AFTER  1
#define DELTA_BEFORE 2
#define DELTA_FIXED  3

  /*-- see thd_rotangles.c --*/

extern void THD_rotangle_user_to_dset( THD_3dim_dataset * ,
                                       float,char, float,char, float,char,
                                       float*,int* , float*,int* , float*,int* );

extern int THD_axcode( THD_3dim_dataset * , char ) ; /* promoted from static */
extern int THD_handedness( THD_3dim_dataset * ) ;    /* on 06 Feb 2001 - RWCox */

extern THD_dmat33 DBLE_mat_to_dicomm( THD_3dim_dataset * ) ; /* 14 Feb 2001 */
extern THD_mat33  SNGL_mat_to_dicomm( THD_3dim_dataset * ) ; /* 28 Aug 2002 */

extern THD_dvecmat THD_rotcom_to_matvec( THD_3dim_dataset * , char * ) ;

extern THD_dvecmat invert_dvecmat( THD_dvecmat avm ) ; /* 24 Jul 2007 */
extern THD_dvecmat sqrt_dvecmat( THD_dvecmat avm ) ;   /* 30 Jul 2007 */

  /*-- see thd_rot3d.c for these routines --*/

extern void THD_rota_method( int ) ;

extern void THD_rota_setpad( int,int,int ) ; /* 02 Feb 2001 */
extern void THD_rota_clearpad(void) ;

extern void THD_rota_vol( int, int, int, float, float, float, float *,
                          int,float, int,float, int,float,
                          int,float,float,float ) ;

extern MRI_IMAGE * THD_rota3D( MRI_IMAGE * ,
                               int,float, int,float, int,float,
                               int,float,float,float ) ;

extern MRI_IMAGE * THD_rota3D_matvec( MRI_IMAGE *, THD_dmat33,THD_dfvec3 ) ;

  /* routines below added to thd_rot3d.c on 16 Jul 2000 */

extern void THD_rota_vol_matvec( int, int, int, float, float, float, float *,
                                 THD_dmat33 , THD_dfvec3 ) ;

extern THD_dvecmat DLSQ_rot_trans( int, THD_dfvec3 *, THD_dfvec3 *, double * );
extern THD_dvecmat DLSQ_affine   ( int, THD_dfvec3 *, THD_dfvec3 *           );
extern THD_dvecmat DLSQ_rotscl   ( int, THD_dfvec3 *, THD_dfvec3 *, int      );

extern THD_dvecmat THD_read_dvecmat( char * , int ) ;  /* THD_read_vecmat.c */

  /* cf. thd_coords.c for cardinal transformation matrix */
extern void THD_dicom_card_xform (THD_3dim_dataset * dset ,
                      THD_dmat33 *tmat, THD_dfvec3 *dics );
extern void THD_dicom_real_xform (THD_3dim_dataset * dset ,
                      THD_dmat33 *tmat, THD_dfvec3 *dics );
extern float THD_compute_oblique_angle(mat44 ijk_to_dicom44, int verbose);

extern void THD_report_obliquity(THD_3dim_dataset *dset);

extern void THD_set_oblique_report(int n1, int n2);

extern int THD_get_oblique_report(void);

extern void THD_reset_oblique_report_index(void);

extern void THD_check_oblique_field(THD_3dim_dataset *dset);
extern void THD_make_cardinal(THD_3dim_dataset *dset);

  /* cf. thd_tmask.c */

#define TM_IXY 2  /* fixdir-1 for each plane */
#define TM_IYZ 0
#define TM_IZX 1

/*! Struct used in cox_render.c to indicate which lines in a volume are all zero. */

typedef struct {
   int   nmask[3] ;
   byte * mask[3] ;
} Tmask ;

extern void free_Tmask( Tmask * ) ;
extern Tmask * create_Tmask_byte( int, int, int, byte * ) ;
extern Tmask * create_Tmask_rgba( int, int, int, rgba * ) ;

#define TM_ZLINE(tm,i) (tm==NULL || tm->mask[TM_IXY][i])
#define TM_YLINE(tm,i) (tm==NULL || tm->mask[TM_IZX][i])
#define TM_XLINE(tm,i) (tm==NULL || tm->mask[TM_IYZ][i])

  /* routines below created in thd_rot3d_byte.c on 23 Oct 2000 */

extern void THD_rota_vol_byte( int, int, int, float, float, float, byte *,
                               int,float, int,float, int,float,
                               int,float,float,float , Tmask * ) ;

extern void THD_rota_byte_mode( int ) ; /* 07 Nov 2000 */

extern void THD_rota_vol_matvec_byte( int, int, int, float, float, float, byte *,
                                      THD_mat33 , THD_fvec3 , Tmask * ) ;

  /*-- see thd_shift2.c for these routines --*/

extern void SHIFT_set_method( int ) ;
extern int  SHIFT_get_method( void ) ;
extern void SHIFT_two_rows( int , int , float , float *, float , float *) ;

extern void fft_shift2  ( int , int , float , float *, float , float *) ;
extern void hept_shift2 ( int , int , float , float *, float , float *) ;
extern void quint_shift2( int , int , float , float *, float , float *) ;
extern void cub_shift2  ( int , int , float , float *, float , float *) ;
extern void lin_shift2  ( int , int , float , float *, float , float *) ;
extern void nn_shift2   ( int , int , float , float *, float , float *) ;
extern void ts_shift2   ( int , int , float , float *, float , float *) ;

extern void hept_shift ( int , float , float *) ;
extern void nn_shift   ( int , float , float *) ;
extern void lin_shift  ( int , float , float *) ;
extern void cub_shift  ( int , float , float *) ;
extern void quint_shift( int , float , float *) ;

extern void THD_fftshift( THD_3dim_dataset *, float,float,float, int ) ;

  /*-- see mri_3dalign.c for these routines --*/

/*! Struct that holds information used during 3D registration. */

typedef struct {
   MRI_IMARR * fitim ;    /*!< Regression basis images */
   double * chol_fitim ;  /*!< Choleski decomposition of the normal equations */
   int xa,xb , ya,yb , za,zb ; /* trim box */
} MRI_3dalign_basis ;

extern void mri_3dalign_edging( int , int , int ) ;
extern void mri_3dalign_edging_default( int , int , int ) ;
extern void mri_3dalign_force_edging( int ) ;
extern void mri_3dalign_wtrimming( int ) ;
extern void mri_3dalign_wproccing( int ) ;
extern void mri_3dalign_scaleinit( float ) ;  /* 22 Mar 2004 */

extern void mri_3dalign_params( int , float , float , float ,
                                int , int , int , int ) ;

extern void mri_3dalign_method( int , int , int , int ) ;

extern void mri_3dalign_final_regmode( int ) ;

extern MRI_3dalign_basis * mri_3dalign_setup( MRI_IMAGE * , MRI_IMAGE * ) ;
extern MRI_IMAGE * mri_3dalign_one( MRI_3dalign_basis * , MRI_IMAGE * ,
                                    float *, float *, float *,
                                    float *, float *, float * ) ;
extern MRI_IMARR * mri_3dalign_many( MRI_IMAGE *, MRI_IMAGE * , MRI_IMARR *,
                                    float *, float *, float *,
                                    float *, float *, float * ) ;
extern void mri_3dalign_cleanup( MRI_3dalign_basis * ) ;

extern void mri_3dalign_initvals( float,float,float,float,float,float ) ;

/*---------------------------------------------------------------------*/

  /*-- see mri_warp3D_align.c for these routines --*/

#undef  PARAM_MAXTRIAL
#define PARAM_MAXTRIAL 7
typedef struct {
  float min, max, siz, ident, delta, toler ;
  float val_init , val_out , val_fixed , val_pinit ;
  int fixed ;
  float val_trial[PARAM_MAXTRIAL] ;
  char name[32] ;
} MRI_warp3D_param_def ;

  /*! Struct that holds information used during warp3D registration. */

typedef struct {

   /*- this stuff is to be set by the user -*/

   int nparam ;
   MRI_warp3D_param_def *param ;
   float scale_init , scale_out ;
   float delfac , tolfac ;
   float twoblur ;

   int regmode , verb , max_iter , num_iter , wtproc ;
   int xedge , yedge , zedge ;
   int regfinal ;

   MRI_IMAGE *imbase , *imwt ;

   void (*vwfor)(float,float,float,float *,float *,float *) ;
   void (*vwinv)(float,float,float,float *,float *,float *) ;
   void (*vwset)(int,float *) ;
   float (*vwdet)(float,float,float) ;

   /*- below here is not to be touched by the user! -*/

   int        nfree ;
   MRI_IMAGE *imww ;
   MRI_IMAGE *imap ;
   MRI_IMAGE *imps ;
   MRI_IMAGE *imsk ;
   MRI_IMAGE *imps_blur ;

} MRI_warp3D_align_basis ;

extern int         mri_warp3D_align_setup  ( MRI_warp3D_align_basis * ) ;
extern MRI_IMAGE * mri_warp3D_align_one    ( MRI_warp3D_align_basis *, MRI_IMAGE * );
extern void        mri_warp3D_align_cleanup( MRI_warp3D_align_basis * ) ;

extern void THD_check_AFNI_version(char *) ;  /* 26 Aug 2005 */

/*---------------------------------------------------------------------*/

#if 0
extern float THD_thresh_to_pval( float thr , THD_3dim_dataset * dset ) ;
#endif

extern float THD_stat_to_pval  ( float thr , int statcode , float * stataux ) ;
extern float THD_pval_to_stat  ( float pval, int statcode , float * stataux ) ;
extern float THD_stat_to_zscore( float thr , int statcode , float * stataux ) ;

extern int THD_filename_ok( char * ) ;   /* 24 Apr 1997 */
extern int THD_filename_pure( char * ) ; /* 28 Feb 2001 */
extern int THD_freemegabytes( char * ) ; /* 28 Mar 2005 */

extern THD_warp * AFNI_make_voxwarp( THD_warp * , THD_3dim_dataset * ,
                                                  THD_3dim_dataset *  ) ;

extern THD_linear_mapping * AFNI_make_voxmap( THD_linear_mapping * ,
                                              THD_dataxes * , THD_dataxes * ) ;

extern void AFNI_concatenate_warp( THD_warp * , THD_warp * ) ;

extern THD_linear_mapping * AFNI_concatenate_lmap( THD_linear_mapping * ,
                                                   THD_linear_mapping *  ) ;

extern THD_warp * AFNI_make_affwarp_12(float,float,float,float,
                                       float,float,float,float,
                                       float,float,float,float ); /* 27 Aug 2002 */

extern THD_warp * AFNI_make_affwarp_mat   ( THD_mat33 ) ;         /* 28 Aug 2002 */
extern THD_warp * AFNI_make_affwarp_matvec( THD_mat33 , THD_fvec3 ) ;

extern THD_ivec3 THD_matrix_to_orientation( THD_mat33 R ) ;       /* 27 Aug 2003 */

extern THD_3dim_dataset * WINsorize( THD_3dim_dataset * ,
                                     int,int,int, float, char *, int,int,byte * );

#define ZPAD_EMPTY (1<<0)
#define ZPAD_PURGE (1<<1)
#define ZPAD_MM    (1<<2)
#define ZPAD_IJK   (1<<3)     /* ZSS Dec 23 05 pad values are in voxel coords */

extern THD_3dim_dataset * THD_zeropad( THD_3dim_dataset * ,
                                       int,int,int,int,int,int, char *, int );

extern THD_3dim_dataset * THD_warp3D(    /* cf. mri_warp3D.c - 18 May 2003 */
                     THD_3dim_dataset *,
                     void w_in2out(float,float,float,float *,float *,float *),
                     void w_out2in(float,float,float,float *,float *,float *),
                     void * , char *, int , int ) ;

extern THD_3dim_dataset * THD_warp3D_affine(
                     THD_3dim_dataset *, THD_vecmat, void *, char *, int, int );

extern THD_3dim_dataset * THD_warp3D_mni2tta( THD_3dim_dataset *, void *,
                                              char *, int, int );
extern THD_3dim_dataset * THD_warp3D_tta2mni( THD_3dim_dataset *, void *,
                                              char *, int, int );

#define WARP3D_NEWGRID  1
#define WARP3D_NEWDSET  2
#define WARP3D_GRIDMASK 7

/*-- 02 Mar 2001: thd_entropy16.c --*/

extern void   ENTROPY_setup     (void) ;
extern void   ENTROPY_setdown   (void) ;
extern void   ENTROPY_accumulate(int , void *) ;
extern double ENTROPY_compute   (void) ;
extern double ENTROPY_dataset   (THD_3dim_dataset *) ;
extern double ENTROPY_datablock (THD_datablock *) ;

extern int  AFNI_vedit( THD_3dim_dataset *dset , VEDIT_settings vednew ) ;
extern void AFNI_vedit_clear( THD_3dim_dataset *dset ) ;

/*--------------------------------------------------------------------------*/

/*--- Stuff for Tom Ross's NOTES ---*/

#define MAX_DSET_NOTES 999
#define MAX_NOTE_SIZE  4000

extern void   tross_Add_Note   (THD_3dim_dataset *, char *) ;
extern void   tross_Delete_Note(THD_3dim_dataset *, int   ) ;

extern char * tross_Expand_String( char * ) ;
extern char * tross_Encode_String( char * ) ;
extern void tross_Dont_Encode_Slash( int ) ;  /* 13 Mar 2003 */

extern void   tross_Store_Note   ( THD_3dim_dataset * , int , char * ) ;
extern char * tross_Get_Note     ( THD_3dim_dataset * , int ) ;
extern char * tross_Get_Notedate ( THD_3dim_dataset * , int ) ;
extern int    tross_Get_Notecount( THD_3dim_dataset * ) ;

extern void tross_Addto_History( THD_3dim_dataset *, THD_3dim_dataset *) ;

extern char * tross_datetime(void) ;
extern char * tross_username(void) ;
extern char * tross_hostname(void) ;
extern char * tross_commandline( char * , int , char ** ) ;

extern int AFNI_logger( char * , int , char ** ) ; /* 13 Aug 2001 */
extern void AFNI_sleep( int ) ;
#define AFNI_log_string(ss) AFNI_logger(ss,0,NULL)
extern long long AFNI_logfilesize(void) ;          /* 17 Oct 2007 */

extern void AFNI_serverlog( char * ) ;             /* 24 Mar 2005 */

void THD_outlier_count( THD_3dim_dataset *, float, int **, int * ) ; /* 15 Aug 2001 */

extern void   tross_Append_History ( THD_3dim_dataset * , char * ) ;
extern char * tross_Get_History    ( THD_3dim_dataset * ) ;
extern void   tross_Make_History   ( char *, int, char **, THD_3dim_dataset * ) ;
extern void   tross_Copy_History   ( THD_3dim_dataset *, THD_3dim_dataset * ) ;
extern void   tross_Replace_History( THD_3dim_dataset * , char * ) ;

#define tross_Erase_History(ds) THD_erase_one_atr((ds)->dblk,"HISTORY_NOTE")

extern char * tross_breakup_string( char *, int , int ) ;

void tross_multi_Append_History( THD_3dim_dataset * , ... ) ;

/*-----------------------------------------------------------------------*/

extern void B64_to_binary( int, byte *, int *, byte ** ) ; /* thd_base64.c */
extern void B64_to_base64( int, byte *, int *, byte ** ) ;
extern void B64_set_linelen( int ) ;
extern void B64_set_crlf( int ) ;

extern char * MD5_static_array ( int , char * ) ;          /* thd_md5.c */
extern char * MD5_malloc_array ( int , char * ) ;
extern char * MD5_static_string(char *) ;
extern char * MD5_malloc_string(char *) ;
extern char * MD5_static_file  (char *) ;
extern char * MD5_malloc_file  (char *) ;

extern char * MD5_B64_array ( int , char * ) ;
extern char * MD5_B64_string( char * ) ;
extern char * MD5_B64_file  (char * ) ;
extern char * UNIQ_idcode(void) ;            /* 27 Sep 2001 */
extern void   UNIQ_idcode_fill(char *) ;

/*------------------------------------------------------------------------*/

typedef enum { UNKNOWN_SPC=0, /*!< Dunno */
               AFNI_TLRC_SPC, /*!< The Classic */
               MNI_SPC,       /*!< A la Canadienne */ 
               MNI_ANAT_SPC,  /*!< Mit viele liebe */
               
               NUMBER_OF_SPC  /*!< A flag for the number of spaces, 
                                    leave for last */
               } AFNI_STD_SPACES;

extern char * TT_whereami( float , float , float, AFNI_STD_SPACES ) ;
extern char * TT_whereami_old( float , float , float ) ;
extern int  TT_load_atlas (void);
extern void TT_purge_atlas(void);
extern THD_3dim_dataset * TT_retrieve_atlas(void) ;

extern void TT_setup_popup_func( void (*pf)(char *) ) ; /* 26 May 2006 */

extern THD_3dim_dataset * TT_retrieve_atlas_big(void) ; /* 01 Aug 2001 */
extern void TT_purge_atlas_big(void);

extern THD_3dim_dataset * TT_retrieve_atlas_either(void); /* 22 Aug 2001 */

#define TT_ATLAS_NZ_SMALL 141 /* 01 Aug 2001 */
#define TT_ATLAS_NZ_BIG   151

#define TT_retrieve_atlas_nz(nz)                                \
 ( ((nz)==TT_ATLAS_NZ_SMALL)                                    \
    ? TT_retrieve_atlas()                                       \
    : ((nz)==TT_ATLAS_NZ_BIG) ? TT_retrieve_atlas_big() : NULL )

/*------------------------------------------------------------------------*/

extern float THD_spearman_corr( int,float *,float *) ;  /* 23 Aug 2001 */
extern float THD_quadrant_corr( int,float *,float *) ;
extern float THD_pearson_corr ( int,float *,float *) ;

extern float THD_pearson_corr_wt( int,float *,float *,float *) ; /* 13 Sep 2006 */

extern float THD_spearman_corr_nd( int,float *,float *) ;  /* 23 Aug 2006 */
extern float THD_quadrant_corr_nd( int,float *,float *) ;
#define THD_pearson_corr_nd THD_pearson_corr

extern void  rank_order_float     ( int , float * );
extern float spearman_rank_prepare( int , float * );
extern float quadrant_corr_prepare( int , float * );
extern float spearman_rank_corr   ( int , float * , float , float * );
extern float quadrant_corr        ( int , float * , float , float * );

extern float THD_mutual_info_scl( int, float,float,float *,    /* 16 Aug 2006 */
                                       float,float,float *, float * ) ;
extern float THD_mutual_info( int , float *, float * ) ;

extern void THD_correlate_ignore_zerozero(int) ;

extern float THD_corr_ratio_scl( int, float,float,float *,     /* 23 Aug 2006 */
                                      float,float,float *, float * ) ;
extern float THD_corr_ratio( int , float *, float * ) ;
extern void  THD_corr_ratio_mode( int ) ;                      /* 11 Oct 2006 */
#define THD_corr_ratio_sym_not THD_corr_ratio_mode(0)  /* unsymm   */
#define THD_corr_ratio_sym_mul THD_corr_ratio_mode(1)  /* sym by * */
#define THD_corr_ratio_sym_add THD_corr_ratio_mode(2)  /* sym by + */

extern float THD_norm_mutinf_scl( int, float,float,float *,    /* 25 Sep 2006 */
                                       float,float,float *, float * ) ;
extern float THD_norm_mutinf( int , float *, float * ) ;

extern float THD_jointentrop_scl( int, float,float,float *,    /* 25 Sep 2006 */
                                       float,float,float *, float * ) ;
extern float THD_jointentrop( int , float *, float * ) ;

extern float THD_hellinger_scl( int, float,float,float *,      /* 26 Sep 2006 */
                                     float,float,float *, float * ) ;
extern float THD_hellinger( int , float *, float * ) ;

extern THD_fvec3 THD_helnmi_scl( int, float,float,float *,
                                 float,float,float *, float * ) ;
extern THD_fvec3 THD_helnmi( int , float *, float * ) ;

extern int retrieve_2Dhist   ( float **xyhist ) ;     /* 28 Sep 2006 */
extern int retrieve_2Dhist1  ( float **, float ** ) ; /* 07 May 2007 */
extern void set_2Dhist_hpower( double ) ;             /* 03 Oct 2006 */
extern void set_2Dhist_hbin  ( int  ) ;
extern int  get_2Dhist_hbin  ( void ) ;
extern void clear_2Dhist     ( void ) ;
extern void build_2Dhist( int n , float xbot,float xtop,float *x ,
                          float ybot,float ytop,float *y , float *w ) ;

extern void set_2Dhist_xybin( int nb , float *xb , float *yb ) ;  /* 07 May 2007 */
extern int get_2Dhist_xybin( float **xb , float **yb ) ;
extern void set_2Dhist_xybin_eqwide( int,float,float,float,float ) ;
extern void set_2Dhist_xybin_eqhigh( int,int,float *,float * ) ;
extern void set_2Dhist_xyclip      ( int, float *, float * ) ;
extern int  get_2Dhist_xyclip      ( float *, float *, float *, float * ) ;


/*------------------------------------------------------------------------*/

extern THD_fvec3 THD_autonudge( THD_3dim_dataset *dsepi, int ivepi,
                                THD_3dim_dataset *dsant, int ivant,
                                float step,
                                int xstep, int ystep, int zstep, int code ) ;

extern MRI_IMAGE * mri_brainormalize( MRI_IMAGE *, int,int,int , MRI_IMAGE **, MRI_IMAGE **) ; /* 05 Apr 2004 */
extern void mri_brainormalize_verbose( int ) ;
extern void brainnormalize_coord( float  ispat, float  jspat, float  kspat ,
                           float *iorig, float *jorig, float *korig ,
                           THD_3dim_dataset *origset,
                           float *xrai_orig, float *yrai_orig, float *zrai_orig); /* ZSS */
extern MRI_IMAGE * mri_watershedize( MRI_IMAGE * , float ) ;
extern void mri_speciebusiness( int ) ;
extern void mri_brainormalize_initialize(float dx, float dy, float dz);
extern float THD_BN_dxyz(void);
extern int THD_BN_nx(void);
extern int THD_BN_ny(void);
extern int THD_BN_nz(void);
extern float THD_BN_xorg(void);
extern float THD_BN_yorg(void);
extern float THD_BN_zorg(void);
extern float THD_BN_zheight(void);
extern float THD_BN_xcm (void);
extern float THD_BN_ycm (void);
extern float THD_BN_zcm (void);
extern float THD_BN_rat (void);
/*------------------------------------------------------------------------*/
/* 09 May 2005: stuff for converting a dataset to from a NIML group.      */

extern NI_group   * THD_nimlize_dsetatr( THD_3dim_dataset *) ;
extern NI_group   * THD_dset_to_ni_surf_dset( THD_3dim_dataset * , int ) ;
extern NI_element * NI_find_element_by_aname(NI_group *,char *,char *,char *);

extern void       THD_dblkatr_from_niml( NI_group *, THD_datablock * ) ;
extern void       THD_set_dataset_attributes( THD_3dim_dataset * ) ;

extern char     * THD_make_statsym_string(THD_3dim_dataset *, int);

extern THD_3dim_dataset * THD_niml_to_dataset( NI_group * , int ) ;
extern int THD_add_bricks( THD_3dim_dataset * , void * ) ;
extern int THD_add_sparse_data( THD_3dim_dataset * , NI_group * ) ;
extern int THD_add_sparse_bricks( THD_3dim_dataset *, NI_element *) ;

extern int  NI_get_byte_order(NI_element *) ;    /* 29 Aug 2006 [rickr] */
extern int  dtype_nifti_to_niml(int dtype);      /* 19 Feb 2008 [rickr] */
extern int  dtype_niml_to_nifti(int dtype);      /* 20 Feb 2008 [rickr] */
extern int  nsd_string_atr_to_slist(char ***, int, ATR_string *);


extern int  get_gni_debug(void) ;                /*  3 Aug 2006 [rickr] */
extern int  get_gni_to_float(void) ;
extern int  get_gni_write_mode(void) ;
extern void set_gni_debug(int) ;
extern void set_gni_to_float(int) ;
extern void set_gni_write_mode(int) ;
extern int  set_ni_globs_from_env(void) ;
extern int  set_sparse_data_attribs(NI_element *, THD_3dim_dataset *, int) ;


#define SBFLAG_INDEX    (1<<0)
#define SBFLAG_FACTOR   (1<<1)
#define SBFLAG_STATCODE (1<<2)

extern NI_element * THD_subbrick_to_niml( THD_3dim_dataset *, int , int ) ;
extern NI_group * THD_dataset_to_niml( THD_3dim_dataset * ) ;

extern MRI_IMAGE  * niml_to_mri( NI_element * ) ;
extern NI_element * mri_to_niml( MRI_IMAGE *  ) ;

#ifdef  __cplusplus
}
#endif

#endif /* _MCW_3DDATASET_ */
