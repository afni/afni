/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

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

#ifndef myXtFree
#define myXtFree(xp) (XtFree((char *)(xp)) , (xp)=NULL)
#endif

#ifndef myXtNew
#define myXtNew(type) ((type *) XtCalloc(1,(unsigned) sizeof(type)))
#endif

struct THD_3dim_dataset ;  /* incomplete definition */

/***************************** dimensions ***************************/

#define THD_MAX_NAME      256
#define THD_MAX_LABEL     38
#define THD_MAX_PREFIX     (127+1)  /* must be more than THD_MAX_LABEL */
#define THD_MAX_VIEWCODE   (4+1)
#define THD_MAX_SUFFIX     (4+1)
#define THD_MAX_FILECODE   (THD_MAX_PREFIX+THD_MAX_VIEWCODE)

#define THD_DEFAULT_LABEL "Elvis Lives"

#define THD_MAX_SESSION_ANAT  512   /* max num anat datasets per directory */
#define THD_MAX_SESSION_FUNC  512   /* max num func datasets per directory */
#define THD_MAX_NUM_SESSION    80   /* max number of directories */

#define THD_MAX_CHOICES THD_MAX_SESSION_FUNC  /* largest of the above! */

#define THD_MAX_MARKSET       5

#define FAIL    -1
#define SUCCESS  1

#define ILLEGAL_TYPE -666

/***************  generic function with no return value  **********************/

typedef void generic_func() ;
typedef float float_func() ; /* generic function returning float */

typedef struct {                 /* for "registered" functions */
   int num ;
   int * flags ;
   char ** labels ;
   generic_func ** funcs ;

   void ** func_data ;           /* 30 Jan 2000 */
   int *   func_code ;
} MCW_function_list ;

#define RETURNS_STRING   1   /* possible flags bit settings */
#define NEEDS_DSET_INDEX 2

#define FUNC_0D   0   /* possible values of func_code */
#define FUNC_1D   1
#define FUNC_2D   2
#define FUNC_3D   3

#define FUNC_FIM  71

/******************************** macros ******************************/

/** combine two interpreted tokens into one using TWO_TWO **/

#define TWO_ONE(x,y) x ## y
#define TWO_TWO(x,y) TWO_ONE(x,y)

/* copy n units of the given type "type * ptr",
     into a structure "str",
     starting at byte offset "off";
   N.B.: str is the structure itself, not a pointer to it
         off is most easily computed with XtOffsetOf       */

#define COPY_INTO_STRUCT(str,off,type,ptr,n) \
   (void) memcpy( (char *)(&(str))+(off), (char *)(ptr), (n)*sizeof(type) )

#define COPY_FROM_STRUCT(str,off,type,ptr,n) \
   (void) memcpy( (char *)(ptr), (char *)(&(str))+(off), (n)*sizeof(type) )

#define MCW_strncpy(dest,src,n) \
   ( (void) strncpy( (dest) , (src) , (n)-1 ) , (dest)[(n)-1] = '\0' )

/*********************** dynamic array of XtPointers **********************/

#define IC_DSET 44301
#define IC_FLIM 55402

typedef struct {
      int num , nall ;
      XtPointer * ar ;
      int * ic ;         /* added 26 Mar 2001 */
} XtPointer_array ;

#define INC_XTARR 8

#define INIT_XTARR(name)               \
   ( (name) = XtNew(XtPointer_array) , \
     (name)->num = (name)->nall = 0 ,  \
     (name)->ar  = NULL ,              \
     (name)->ic  = NULL   )

#define ADDTO_XTARR(name,bblk)                                 \
   { if( (name)->num == (name)->nall ){                        \
      (name)->nall += INC_XTARR ;                              \
      (name)->ar    = (XtPointer *)                            \
                       XtRealloc( (char *) (name)->ar ,        \
                          sizeof(XtPointer) * (name)->nall ) ; \
      (name)->ic    = (int *) XtRealloc( (char *) (name)->ic , \
                          sizeof(int) * (name)->nall ) ;       \
     }                                                         \
     if( (XtPointer) (bblk) != NULL ){               \
      (name)->ar[(name)->num] = (XtPointer) (bblk) ; \
      (name)->ic[(name)->num] = 0                  ; \
      ((name)->num)++ ;                              \
     } }

#define XTARR_NUM(name)  ((name)->num)
#define XTARR_XT(name,i) ((name)->ar[i])
#define XTARR_IC(name,i) ((name)->ic[i])

#define FREE_XTARR(name)      \
   if( (name) != NULL ){      \
     myXtFree( (name)->ar ) ; \
     myXtFree( (name)->ic ) ; \
     myXtFree( (name) ) ;     \
     (name) = NULL ; }

#define DESTROY_XTARR  FREE_XTARR   /* duplicate definition */

/************************* string array stuff *************************/

typedef struct {
      int num , nall ;
      char ** ar ;
      KILL_list kl ;
} THD_string_array ;

#define SARR_STRING(ss,qq) ((ss)->ar[(qq)])
#define SARR_NUM(ss)       ((ss)->num)

#define INC_SARR 64

#define INIT_SARR(name)                 \
   ( (name) = XtNew(THD_string_array) , \
     (name)->num = (name)->nall = 0 ,   \
     (name)->ar  = NULL ,               \
     INIT_KILL((name)->kl) )

#define ADDTO_SARR(name,str)                                          \
 do{ if( (name)->num == (name)->nall ){                               \
      (name)->nall += INC_SARR ;                                      \
      (name)->ar    = (char **) XtRealloc( (char *) (name)->ar ,      \
                                 sizeof(char *) * (name)->nall ) ;    \
     }                                                                \
     if( (str) != NULL ){                                             \
      (name)->ar[(name)->num] = (char *) XtMalloc( strlen((str))+1 ) ;\
      strcpy( (name)->ar[(name)->num] , (str) ) ;                     \
      ADDTO_KILL((name)->kl,(name)->ar[(name)->num]) ;                \
      ((name)->num)++ ;                                               \
     } } while(0)

#define REMOVEFROM_SARR(name,ijk)                \
 do{ SINGLE_KILL((name)->kl,(name)->ar[(ijk)]) ; \
     (name)->ar[(ijk)] = NULL ; } while(0)

#define DESTROY_SARR(name)    \
 do{ if( (name) != NULL ){    \
     KILL_KILL((name)->kl) ;  \
     myXtFree( (name)->ar ) ; \
     myXtFree( (name) ) ; } } while(0)

extern int SARR_find_string( THD_string_array * sar , char * str ) ;
extern int SARR_find_substring( THD_string_array * sar , char * sub ) ;

extern int SARR_lookfor_string   ( THD_string_array * sar , char * str , int nstart ) ;
extern int SARR_lookfor_substring( THD_string_array * sar , char * sub , int nstart ) ;

#define PATH_CONCAT(pout,p1,p2)                            \
  do{ int zq ; strcpy((pout),(p1)) ; zq = strlen((pout)) ; \
      if( (pout)[zq-1] != '/' ) strcat((pout),"/") ;       \
      strcat((pout),(p2)) ; } while(0)

/*************** dynamic array of sorted (x,y,z) points *************/

typedef struct {
      int num , nall ;
      THD_fvec3 * xyz ;
      THD_ivec3 * ijk ;
      struct THD_3dim_dataset * parent ;
} THD_vector_list ;

#define INC_VLIST 64

#define INIT_VLIST(name,ddd) \
   ( (name) = XtNew(THD_vector_list) ,  \
     (name)->num = (name)->nall = 0 ,   \
     (name)->xyz = NULL , (name)->ijk = NULL , \
     (name)->parent = (ddd) )

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

#define DESTROY_VLIST(name)      \
   { if( (name) != NULL ){       \
       myXtFree( (name)->xyz ) ; \
       myXtFree( (name)->ijk ) ; \
       myXtFree( (name) ) ; } }

/**************************** typedefs ******************************/

/*---------- structure to hold attributes from disk files ----------*/

#define ATR_STRING_TYPE   0
#define ATR_FLOAT_TYPE    1
#define ATR_INT_TYPE      2

#define FIRST_ATR_TYPE 0
#define LAST_ATR_TYPE  2

static char * ATR_typestr[] = {
   "string-attribute" , "float-attribute" , "integer-attribute"
} ;

typedef struct {
      int    type ;
      char * name ;
      int    nin ;
      int  * in ;
} ATR_int ;

typedef struct {
      int     type ;
      char *  name ;
      int     nfl ;
      float * fl ;
} ATR_float ;

typedef struct {
      int    type ;
      char * name ;
      int    nch ;
      char * ch ;
} ATR_string ;

#define ZBLOCK 126
extern void THD_zblock(int,char *) ;   /* replace zeros with ZBLOCKs */
extern void THD_unzblock(int,char *) ; /* undo the above */

typedef union {
      int          type ;
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

typedef struct {
      int type ;            /* type code */

      THD_mat33 mfor,mbac ; /* x_map = [mfor] * x_in  - bvec  */
                            /* x_in  = [mbac] * x_map - svec  */
                            /* ( ==> svec = - [mbac] * bvec ) */
      THD_fvec3 bvec,svec,
                bot,top ;   /* bot,top are bounds (application defined) */
} THD_linear_mapping ;

#define COPY_LMAP_BOUNDS(m1,m2) ( (m1).bot=(m2).bot , (m1).top=(m2).top )

/* use the matrix operations to define a macro
   to load the inverse to a THD_linear_mapping once the forward is done */

#define LOAD_INVERSE_LMAP(map) \
   ( (map).mbac = MAT_INV((map).mfor) ,          \
     (map).svec = MATVEC((map).mbac,(map).bvec) ,\
     NEGATE_FVEC3((map).svec) )

#define MAPPING_LINEAR_FSTART \
   XtOffsetOf(THD_linear_mapping,mfor)
#define MAPPING_LINEAR_FEND   \
   (XtOffsetOf(THD_linear_mapping,top)+sizeof(THD_fvec3))
#define MAPPING_LINEAR_FSIZE  \
   ((MAPPING_LINEAR_FEND-MAPPING_LINEAR_FSTART)/sizeof(float))

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

typedef struct {
     int numdef , numset ; /* # of markers defined, from 1 to MAX_MARKS */

     char label[MARKS_MAXNUM][MARKS_MAXLAB] ; /* names for these marks */

     char help[MARKS_MAXNUM][MARKS_MAXHELP] ; /* help for these marks */

     int ovcolor[MARKS_MAXNUM] ;              /* -1 --> use defaults */

     Boolean valid[MARKS_MAXNUM] ;            /* True if actually set */

     float xyz[MARKS_MAXNUM][3] ;             /* coordinates */
                                              /* (3dmm, not Dicom) */

     int aflags[MARKS_MAXFLAG] ;              /* action flags */

     int type ;                               /* type of markers */
                                              /* (same as aflags[0]) */
     char name[MARKS_MAXLAB] ;                /* name of this type */
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

#define NMARK_ALIGN 5

static int THD_align_aflags[MARKS_MAXFLAG] = {
  MARKSET_ALIGN , MARKACTION_WARP
} ;

#define IMARK_ACSE 0
#define IMARK_ACPM 1
#define IMARK_PCIE 2
#define IMARK_MSA1 3
#define IMARK_MSA2 4

static char * THD_align_label[NMARK_ALIGN] = {
   "AC superior edge"     ,
   "AC posterior margin"  ,
   "PC inferior edge"     ,
   "First mid-sag pt"     ,
   "Another mid-sag pt"
} ;

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

/* if you change these, change the helps below too */

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

static char * THD_bounding_label[NMARK_BOUNDING] = {
   "Most anterior point"  ,
   "Most posterior point" ,
   "Most superior point"  ,
   "Most inferior point"  ,
   "Most left point"      ,
   "Most right point"
} ;

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

typedef struct {
      int type ;  /* type code */
      int resam_type ;

      THD_linear_mapping warp[12] ;
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

#define DUMP_T12_MAP(t12,xx,yy,zz) \
 (  printf("\n--- submap " # xx # yy # zz "\n" ) , \
    DUMP_LMAP( (t12).warp[W_ ## xx ## yy ## zz] )    )

#define DUMP_T12_WARP(t12) \
 ( printf("\n12 region Talairach warp:") ,                 \
   DUMP_T12_MAP((t12),R,A,S) , DUMP_T12_MAP((t12),L,A,S) , \
   DUMP_T12_MAP((t12),R,M,S) , DUMP_T12_MAP((t12),L,M,S) , \
   DUMP_T12_MAP((t12),R,P,S) , DUMP_T12_MAP((t12),L,P,S) , \
   DUMP_T12_MAP((t12),R,A,I) , DUMP_T12_MAP((t12),L,A,I) , \
   DUMP_T12_MAP((t12),R,M,I) , DUMP_T12_MAP((t12),L,M,I) , \
   DUMP_T12_MAP((t12),R,P,I) , DUMP_T12_MAP((t12),L,P,I)    )

typedef struct {
      int type ;
      int resam_type ;

      THD_linear_mapping warp ;
} THD_affine_warp ;

#define WARP_AFFINE_SIZE (MAPPING_LINEAR_FSIZE)

typedef union {
      int type ;
      THD_affine_warp       rig_bod ;
      THD_talairach_12_warp tal_12 ;
} THD_warp ;

#define ISVALID_WARP(ww) ( (ww) != NULL &&                  \
                           (ww)->type >= FIRST_WARP_TYPE && \
                           (ww)->type <= LAST_WARP_TYPE )

static THD_warp tempA_warp ;  /* temporary warp */

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

/* make the affine warp map point (xin,yin,zin) to (xout,yout,zout) */

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
***/

#define STORAGE_UNDEFINED  0
#define STORAGE_BY_BRICK   2

/* the filenames in this structure are really path names
   (that is, they have the directory name prependend)    */

typedef struct {
      int type ;
      int rank , nvals , dimsizes[THD_MAX_RANK] ;
      int storage_mode ;                   /* one of the STORAGE_ codes  */

                                           /* 25 April 1998:         */
      int byte_order ;                     /* LSB_FIRST or MSB_FIRST */

      char prefix[THD_MAX_PREFIX] ;        /* filenames on disk will be  */
      char viewcode[THD_MAX_VIEWCODE] ;    /* formed from filecode.*     */
      char filecode[THD_MAX_FILECODE] ;    /* filecode = prefix+viewcode */

      char directory_name[THD_MAX_NAME] ;  /* contain all files */
      char header_name[THD_MAX_NAME] ;     /* contains attributes */
      char brick_name[THD_MAX_NAME] ;      /* THIS contains data */
} THD_diskptr ;

#define ATRNAME_BYTEORDER "BYTEORDER_STRING"

extern void THD_delete_diskptr( THD_diskptr * ) ;

#define ISVALID_DISKPTR(dk) ( (dk)!=NULL && (dk)->type==DISKPTR_TYPE )

#define PREFIX_VIEW_TO_FILECODE(pr,vv,fc) sprintf( (fc),"%s+%s",(pr),(vv) )

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

#define ISVALID_MEM_CODE(mm) \
  ( (mm) == DATABLOCK_MEM_MALLOC || (mm) == DATABLOCK_MEM_MMAP \
                                 || (mm) == DATABLOCK_MEM_ANY )

/****
    Feb 1996:
      All subvolumes are stored in an array of MRI_IMAGEs (the "brick").
      If mmap is used, then the whole external file is mmap-ed in one
        block and the data pointers for each image computed from this base.
      If malloc is used, then each image is separately allocated and read in.
      Each datablock has a brick, even if it doesn't actually contain
        data (is only warp-on-demand).  Whether or not a datablock contains
        actual voxel data can be determined by examining the "malloc_type".
****/

/* a brick file should have this many bytes before we try to use mmap */

#ifndef MMAP_THRESHOLD           /* if not previously defined in machdep.h */
#  define MMAP_THRESHOLD 99999
#endif

typedef struct {
      int type ;     /* type code */

      int nvals ;             /* number of 3D bricks */

      MRI_IMARR * brick  ;    /* array of pointers to each one */
      float * brick_fac  ;    /* scale factors to convert sub-bricks to floats */
      int *  brick_bytes ;    /* data size of each sub-brick */

                                /* These fields added for "bucket" datasets: */
      char **  brick_lab  ;     /* labels for all sub-bricks                 */
      char **  brick_keywords ; /* keywords strings for all sub-bricks       */
      int *    brick_statcode ; /* a FUNC_*_TYPE ==> kind of statistic here  */
      float ** brick_stataux ;  /* stat_aux parameters for each sub-brick    */
                                /* with brick_statcode[iv] > 0               */

      int    total_bytes ;    /* totality of data storage needed */
      int    malloc_type ;    /* memory allocation method */
      int    locked ;         /* Feb 1998: locked in memory (un-purgeable) */

      int    master_nvals ;   /* Jan 1999: for datasets that are extracted      */
      int *  master_ival ;    /*           in pieces from a master dataset      */
      int *  master_bytes ;   /* master_ival[nvals]; master_bytes[master_nvals] */

      float master_bot,master_top ; /* 21 Feb 2001: range of data to keep */

      THD_diskptr * diskptr ; /* where the data is on disk (if anywhere!) */

      int       natr , natr_alloc ;
      ATR_any * atr ;         /* array of attributes (from the header) */

   /* pointers to other stuff */

      KILL_list kl ;
      XtPointer parent ;
} THD_datablock ;

#define DBLK_mallocize(db) THD_force_malloc_type((db),DATABLOCK_MEM_MALLOC)
#define DBLK_mmapize(db)   THD_force_malloc_type((db),DATABLOCK_MEM_MMAP)
#define DBLK_anyize(db)    THD_force_malloc_type((db),DATABLOCK_MEM_ANY)

#define DBLK_IS_MALLOC(db)  ((db)->malloc_type == DATABLOCK_MEM_MALLOC)
#define DBLK_IS_MMAP(db)    ((db)->malloc_type == DATABLOCK_MEM_MMAP)

#define DBLK_lock(db)   ((db)->locked = 1)
#define DBLK_unlock(db) ((db)->locked = ((db)->locked<2) ? 0 : 2)
#define DBLK_LOCKED(db) ((db)->locked)

#define DBLK_superlock(db) ((db)->locked = 2)  /* 22 Mar 2001: cannot be unlocked */

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

#define THD_null_datablock_auxdata(blk) ( (blk)->brick_lab      = NULL , \
                                          (blk)->brick_keywords = NULL , \
                                          (blk)->brick_statcode = NULL , \
                                          (blk)->brick_stataux  = NULL  )

extern int  THD_string_has( char * , char * ) ;

#define ISVALID_DATABLOCK(bk) ( (bk) != NULL && (bk)->type == DATABLOCK_TYPE )
#define ISVALID_DBLK           ISVALID_DATABLOCK  /* 26 Mar 2001 */

/*------------- a dynamic array type for datablocks --------------*/

typedef struct {
      int num , nall ;
      THD_datablock ** ar ;
} THD_datablock_array ;

#define INC_DBARR 8

#define INIT_DBARR(name)                  \
   ( (name) = XtNew(THD_datablock_array) ,\
     (name)->num = (name)->nall = 0 ,     \
     (name)->ar  = NULL )

#define ADDTO_DBARR(name,bblk)                                     \
   { if( (name)->num == (name)->nall ){                            \
      (name)->nall += INC_DBARR ;                                  \
      (name)->ar    = (THD_datablock **)                           \
                       XtRealloc( (char *) (name)->ar ,            \
                        sizeof(THD_datablock *) * (name)->nall ) ; \
     }                                                             \
     if( (bblk) != NULL ){               \
      (name)->ar[(name)->num] = (bblk) ; \
      ((name)->num)++ ;                  \
     } }

#define FREE_DBARR(name)      \
   if( (name) != NULL ){      \
     myXtFree( (name)->ar ) ; \
     myXtFree( (name) ) ; }

/*--------------------------------------------------------------------*/
/*---------- stuff to hold axes information for 3D dataset -----------*/

#define DATAXES_TYPE 27

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

static char ORIENT_xyz[]   = "xxyyzzg" ;  /* Dicom directions are
                                             x = R->L , y = A->P , z = I->S */
static char ORIENT_sign[]  = "+--++-" ;

static char ORIENT_first[] = "RLPAIS" ;

static int  ORIENT_xyzint[] = { 1,1 , 2,2 , 3,3 , 666 } ;

#define ORIENT_OPPOSITE(orc) \
  ( ((orc) % 2 == 0) ? ((orc)+1) : ((orc)-1) )

/*** voxel center x[i] is at xxorg + i * xxdel ***/

typedef struct {
      int type ;    /* type code */

      int   nxx,nyy,nzz ;            /* number of points in each direction */

      float xxorg , yyorg , zzorg ;  /* center of (0,0,0) voxel */
      float xxdel , yydel , zzdel ;  /* spacings between voxel centers (mm) */

      float xxmin,xxmax , yymin,yymax , zzmin,zzmax ;  /* bounding box */

      int xxorient , yyorient , zzorient ;  /* orientation codes */

      THD_mat33 to_dicomm ; /* orthogonal matrix transforming from */
                            /* dataset coordinates to Dicom coordinates */

   /* pointers to other stuff */

      XtPointer parent ;
} THD_dataxes ;

#define DAXES_XCEN(dax) ((dax)->xxorg + 0.5*((dax)->nxx - 1) * (dax)->xxdel)
#define DAXES_YCEN(dax) ((dax)->yyorg + 0.5*((dax)->nyy - 1) * (dax)->yydel)
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

/***
   WARNING:  If you perform surgery on a dataset and change its
             dimensions in the dataxes, you must also reflect
             this in the diskptr.  Otherwise, the .HEAD file
             will not have the correct dimensions!  The macro
             just below will do this for you.
***/

#define DATAXES_TO_DISKPTR(ds)                             \
  ( (ds)->dblk->diskptr->dimsizes[0] = (ds)->daxes->nxx ,  \
    (ds)->dblk->diskptr->dimsizes[1] = (ds)->daxes->nyy ,  \
    (ds)->dblk->diskptr->dimsizes[2] = (ds)->daxes->nzz  )

#define ISVALID_DATAXES(dax) ( (dax) != NULL && (dax)->type == DATAXES_TYPE )

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

extern void THD_edit_dataxes( float , THD_dataxes * , THD_dataxes * ) ;

/*---------------------------------------------------------------------*/
/*--- data structure for information about time axis of 3D dataset ----*/

/******
    For 3D+t datasets, there are ntt 3D times; the i-th one is centered
    at ttorg + ttdel*ii seconds, for ii=0..ntt-1.
    Also, ttdur = duration of each sample in time.

    If ( nsl > 0 && toff_sl != NULL), then the data was acquired as
    slices, not as a 3D block.  The slicing direction must be the
    dataset (not Dicom) z-axis.  The extra offset for the data at
    z is given by computing isl = (z - zorg_sl) / dz_sl + 0.5; the
    extra offset is then toff_sl[isl].

    All this is computed using the routine THD_timeof.

    When transformed, all the slice stuff will be ignored.  That's
    because the warped dataset z-direction will not be the same as the
    original dataset's z-direction.
******/

#define TIMEAXIS_TYPE 907

#define UNITS_MSEC_TYPE  77001
#define UNITS_SEC_TYPE   77002
#define UNITS_HZ_TYPE    77003

static char * UNITS_TYPE_labelstring[] = { "ms" , "s" , "Hz" } ;

#define UNITS_TYPE_LABEL(uu) UNITS_TYPE_labelstring[(uu)-UNITS_MSEC_TYPE]

typedef struct {
   int   type ;
   int   ntt ;
   float ttorg , ttdel , ttdur ;

   int units_type ;  /* one of the UNITS_ codes above */

   int     nsl ;
   float * toff_sl ;
   float   zorg_sl , dz_sl ;
} THD_timeaxis ;

#define ISVALID_TIMEAXIS(tax) ((tax) != NULL && (tax)->type == TIMEAXIS_TYPE)

/*---------------------------------------------------------------------*/
/*--------- data structure for statistics about a 3D dataset ----------*/

#define STATISTICS_TYPE 17

typedef struct {
   float min , max ;
} THD_brick_stats ;

typedef struct {
   int type ;
   int              nbstat ;      /* number of entries below */
   THD_brick_stats * bstat ;      /* array of entries for all sub-bricks */
   XtPointer parent ;
} THD_statistics ;

#define ISVALID_STATISTIC(st) ( (st) != NULL && (st)->type == STATISTICS_TYPE )

#define ISVALID_BSTAT(bst) ( (bst).min <= (bst).max )

#define INVALIDATE_BSTAT(bst) ( (bst).min = 1.0 , (bst).max = -1.0 )

#define KILL_STATISTIC(st)          \
  do{ if( ISVALID_STATISTIC(st) ){  \
        XtFree((char *)(st)->bstat) ; XtFree((char *)(st)) ; } } while(0)

/*--------------------------------------------------------------------*/
/*--------------------  Unique ID code for a 3D dataset  -------------*/
#ifndef OMIT_DATASET_IDCODES

#ifndef IDCODE_PREFIX
#  define MCW_IDPREFIX "GPL_"
#else
#  define MCW_IDPREFIX IDCODE_PREFIX
#endif

#define MCW_IDSIZE 16
#define MCW_IDDATE 48

typedef struct { char str[MCW_IDSIZE] , date[MCW_IDDATE] ; } MCW_idcode ;
MCW_idcode MCW_new_idcode(void) ;

#define EQUIV_IDCODES(id,ie) (strncmp((id).str,(ie).str,MCW_IDSIZE) == 0)

#define EQUIV_DSETS(ds,es) \
   ( (ds)==(es) ||         \
     ((ds)!=NULL && (es)!=NULL && EQUIV_IDCODES((ds)->idcode,(es)->idcode)) )

#define ZERO_IDCODE(id)   ((id).str[0] = (id).date[0] = '\0')
#define ISZERO_IDCODE(id) ((id).str[0] == '\0')

#define ATRNAME_IDSTRING  "IDCODE_STRING"
#define ATRNAME_IDDATE    "IDCODE_DATE"
#define ATRNAME_IDANATPAR "IDCODE_ANAT_PARENT"
#define ATRNAME_IDWARPPAR "IDCODE_WARP_PARENT"

#endif /* OMIT_DATASET_IDCODES */

/*----------------------------------------------------------------------*/
/*------------------- how to present the coordinates -------------------*/

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

#define MERGER_TYPE           -99
#define MERGER_UNDEFINED_TYPE   0
#define MERGER_FUNCGROUP_TYPE   1

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
  0.0 , FUNC_THR_TOP , FUNC_COR_TOP , FUNC_TT_TOP , FUNC_FT_TOP ,
        FUNC_ZT_TOP  , FUNC_CT_TOP  , FUNC_BT_TOP ,
        FUNC_BN_TOP  , FUNC_GT_TOP  , FUNC_PT_TOP , FUNC_BUCK_TOP
} ;

static int FUNC_scale_short[] = {
  0 , FUNC_THR_SCALE_SHORT , FUNC_COR_SCALE_SHORT ,
      FUNC_TT_SCALE_SHORT  , FUNC_FT_SCALE_SHORT  ,
      FUNC_ZT_SCALE_SHORT  , FUNC_CT_SCALE_SHORT  , FUNC_BT_SCALE_SHORT ,
      FUNC_BN_SCALE_SHORT  , FUNC_GT_SCALE_SHORT  , FUNC_PT_SCALE_SHORT ,
      FUNC_BUCK_SCALE_SHORT
} ;

static int FUNC_scale_byte[] = {
  0 , FUNC_THR_SCALE_BYTE , FUNC_COR_SCALE_BYTE ,
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

static int FUNC_nvals[]    = {  1, 2,2,2,2,2,2,2,2,2,2, 1 } ; /* # in each dataset */
static int FUNC_ival_fim[] = {  0, 0,0,0,0,0,0,0,0,0,0, 0 } ; /* index of fim      */
static int FUNC_ival_thr[] = { -1, 1,1,1,1,1,1,1,1,1,1, 0 } ; /* index of thresh   */

#define FUNC_HAVE_FIM(ftyp)  ((ftyp) >= 0 && \
                              (ftyp) <= LAST_FUNC_TYPE && FUNC_ival_fim[(ftyp)] >= 0)

#define FUNC_HAVE_THR(ftyp)  ((ftyp) >= 0 && \
                              (ftyp) <= LAST_FUNC_TYPE && FUNC_ival_thr[(ftyp)] >= 0)

#define FUNC_HAVE_PVAL(ftyp) (FUNC_HAVE_THR(ftyp)  && (ftyp) != FUNC_PAIR_TYPE)
#define FUNC_IS_STAT(ftyp)   (FUNC_HAVE_PVAL(ftyp) && (ftyp) != FUNC_BUCK_TYPE)

/******* dimension of auxiliary array for functional statistics *******/

#define MAX_STAT_AUX 64

static int FUNC_need_stat_aux[] = { 0 , 0 , 3 , 1 , 2 ,
                                    0 , 1 , 2 , 2 , 2 , 1 ,
                                    0 } ; /* # aux data needed */

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

static int ANAT_nvals[]     = { 1,1,1,1,1,1,1,1,1,1,1,1 , 1 } ;
static int ANAT_ival_zero[] = { 0,0,0,0,0,0,0,0,0,0,0,0 , 0 } ;

/* the data structure itself */

struct THD_3dim_dataset_array ;  /* incomplete definition */

typedef MRI_IMAGE * THD_merger_func( int merger_code, XtPointer merger_data ) ;

typedef struct THD_3dim_dataset {
      int type ;   /* type code */

      int view_type , func_type ;

      char label1[THD_MAX_LABEL] , label2[THD_MAX_LABEL] ;

      THD_datablock   * dblk ;      /* pointer to actual data */
      THD_dataxes     * daxes ;     /* info about axes */
      THD_dataxes     * wod_daxes ; /* warp-on-demand axes */
      int               wod_flag ;  /* if true, use wod_daxes */

      THD_timeaxis    * taxis ;     /* non-NULL --> 3D+t dataset */

      THD_marker_set  * markers ;         /* user set mark points */

      struct THD_3dim_dataset * warp_parent ; /* non-NULL --> a warp */
      THD_warp                * warp ;        /* of some other dataset */
      THD_warp                * vox_warp ;    /* index-to-index warp */

      struct THD_3dim_dataset * anat_parent ;   /* non-NULL --> linked */
                                                /* to anatomical ref */

      THD_statistics          * stats ;      /* statistics about the data */

      float stat_aux[MAX_STAT_AUX] ;         /* auxiliary statistics info */

      char warp_parent_name[THD_MAX_NAME] ,  /* dataset names of parents */
           anat_parent_name[THD_MAX_NAME] ,
           self_name[THD_MAX_NAME]         ; /* my own name */

      struct THD_3dim_dataset_array *  merger_list ; /* non-null ==> this   */
      THD_merger_func               *  merger_func ; /* dataset is a merger */
      int                              merger_code , /* merger operation    */
                                       merger_type  ;

      THD_vector_list * pts ;     /* in dataset coords (not Dicom!) */
      Boolean pts_original ;      /* true if was read from disk directly */

      int death_mark ;

#ifndef OMIT_DATASET_IDCODES
      MCW_idcode idcode ;
      MCW_idcode anat_parent_idcode , warp_parent_idcode ;
#endif

      char * keywords ;   /* 30 Nov 1997 */

      THD_usertaglist * tagset ;  /* 23 Oct 1998 */

   /* pointers to other stuff */

      KILL_list kl ;
      XtPointer parent ;
} THD_3dim_dataset ;

#define DOOMED 665

#define CURRENT_DAXES(ds) (((ds)->wod_flag) ? ((ds)->wod_daxes) : ((ds)->daxes))

#define ISVALID_3DIM_DATASET(ds) \
   ( (ds) != NULL && (ds)->type >= FIRST_3DIM_TYPE && \
                     (ds)->type <= LAST_3DIM_TYPE )

#define ISVALID_DSET ISVALID_3DIM_DATASET

#define ISFUNCTYPE(nn) ( (nn) == HEAD_FUNC_TYPE || (nn) == GEN_FUNC_TYPE )
#define ISFUNC(dset) ( ISVALID_DSET(dset) && ISFUNCTYPE((dset)->type) )

#define ISANATTYPE(nn) ( (nn) == HEAD_ANAT_TYPE || (nn) == GEN_ANAT_TYPE )
#define ISANAT(dset) ( ISVALID_DSET(dset) && ISANATTYPE((dset)->type) )

#define ISHEADTYPE(nn) ( (nn) = HEAD_ANAT_TYPE || (nn) == HEAD_FUNC_TYPE )
#define ISHEAD(dset) ( ISVALID_DSET(dset) && ISHEADTYPE((dset)->type) )

#define ISANATBUCKET(dset) ( ISANAT(dset) && (dset)->func_type == ANAT_BUCK_TYPE )
#define ISFUNCBUCKET(dset) ( ISFUNC(dset) && (dset)->func_type == FUNC_BUCK_TYPE )

#define ISBUCKET(dset) ( ISANATBUCKET(dset) || ISFUNCBUCKET(dset) )

#define ISMERGER(ds) ( ISVALID_DSET(ds) && (ds)->func_type == MERGER_TYPE )

#define DSET_ONDISK(ds) ( ISVALID_DSET(ds) && (ds)->dblk!=NULL && \
                          (ds)->dblk->diskptr->storage_mode!=STORAGE_UNDEFINED )

#define DSET_WRITEABLE(ds)                                                    \
 ( ISVALID_DSET(ds) && ISVALID_DBLK((ds)->dblk) && (ds)->warp_parent != NULL )

#define DSET_COMPRESSED(ds)                  \
   ( ISVALID_DSET(ds) && (ds)->dblk!=NULL && \
     (ds)->dblk->diskptr != NULL          && \
     COMPRESS_filecode((ds)->dblk->diskptr->brick_name) >= 0 )

#define PURGE_DSET(ds)                                  \
 do{ if( ISVALID_3DIM_DATASET(ds) && DSET_ONDISK(ds) )  \
        (void) THD_purge_datablock( (ds)->dblk , DATABLOCK_MEM_ANY ) ; } while(0)

#define DSET_INMEMORY(ds) ( ISVALID_DSET(ds) && (ds)->dblk!=NULL && \
                            (ds)->dblk->malloc_type!=DATABLOCK_MEM_UNDEFINED )

#define DBLK_BRICK(db,iv) ((db)->brick->imarr[(iv)])
#define DSET_BRICK(ds,iv) DBLK_BRICK((ds)->dblk,(iv))

#define DBLK_BRICK_TYPE(db,iv) (DBLK_BRICK((db),(iv))->kind)
#define DSET_BRICK_TYPE(ds,iv) DBLK_BRICK_TYPE((ds)->dblk,(iv))

#define DBLK_BRICK_NVOX(db,iv) (DBLK_BRICK((db),(iv))->nvox)

#define DBLK_ARRAY(db,iv) mri_data_pointer( DBLK_BRICK((db),(iv)) )
#define DSET_ARRAY(ds,iv) DBLK_ARRAY((ds)->dblk,(iv))

#define DSET_BRICK_ARRAY DSET_ARRAY  /* Because I sometimes forget the  */
#define DBLK_BRICK_ARRAY DBLK_ARRAY  /* correct names given above - RWC */

#define DBLK_BRICK_FACTOR(db,iv) ((db)->brick_fac[(iv)])
#define DSET_BRICK_FACTOR(ds,iv) DBLK_BRICK_FACTOR((ds)->dblk,(iv))

extern int THD_need_brick_factor( THD_3dim_dataset * ) ;

#define DBLK_BRICK_BYTES(db,iv) ((db)->brick_bytes[iv])
#define DSET_BRICK_BYTES(ds,iv) DBLK_BRICK_BYTES((ds)->dblk,(iv))

#define DSET_PRINCIPAL_VALUE(ds) ( ISANAT(ds) ? ANAT_ival_zero[(ds)->func_type] \
                                              : FUNC_ival_fim[(ds)->func_type] )

#define DSET_PRINCIPAL_INDEX DSET_PRINCIPAL_VALUE

#define DSET_THRESH_VALUE(ds) (ISANAT((ds)) ? -1 : FUNC_ival_thr[(ds)->func_type])

#define DSET_THRESH_INDEX DSET_THRESH_VALUE

#define DSET_PREFIX(ds) (((ds)->dblk!=NULL && (ds)->dblk->diskptr!=NULL) \
                       ? ((ds)->dblk->diskptr->prefix) : "\0" )

extern char * THD_newprefix(THD_3dim_dataset * dset, char * suffix); /* 16 Feb 2001 */

#define DSET_FILECODE(ds) (((ds)->dblk!=NULL && (ds)->dblk->diskptr!=NULL) \
                         ? ((ds)->dblk->diskptr->filecode) : "\0" )

#define DSET_HEADNAME(ds) (((ds)->dblk!=NULL && (ds)->dblk->diskptr!=NULL) \
                         ? ((ds)->dblk->diskptr->header_name) : "\0" )

#define DSET_BRIKNAME(ds) (((ds)->dblk!=NULL && (ds)->dblk->diskptr!=NULL) \
                         ? ((ds)->dblk->diskptr->brick_name) : "\0" )
#define DSET_BRICKNAME DSET_BRIKNAME

#define DSET_DIRNAME(ds) (((ds)->dblk!=NULL && (ds)->dblk->diskptr!=NULL) \
                         ? ((ds)->dblk->diskptr->directory_name) : "\0" )
#define DSET_SESSNAME DSET_DIRNAME

#define DSET_IDCODE(ds) (&((ds)->idcode))

/* 25 April 1998 */

#define DBLK_BYTEORDER(db)  ((db)->diskptr->byte_order)
#define DSET_BYTEORDER(ds)  DBLK_BYTEORDER((ds)->dblk)

/** macros for time-dependent datasets **/

#define DSET_NUM_TIMES(ds)       ( ((ds)->taxis == NULL) ? 1 : (ds)->taxis->ntt )
#define DSET_NVALS_PER_TIME(ds)  ( (ds)->dblk->nvals / DSET_NUM_TIMES(ds) )
#define DSET_NVALS(ds)           ( (ds)->dblk->nvals )

#define DSET_NVOX(ds) ( (ds)->daxes->nxx * (ds)->daxes->nyy * (ds)->daxes->nzz )

#define DSET_NX(ds) ((ds)->daxes->nxx)
#define DSET_NY(ds) ((ds)->daxes->nyy)
#define DSET_NZ(ds) ((ds)->daxes->nzz)

#define DSET_DX(ds) ((ds)->daxes->xxdel)  /* added 17 Aug 1998 */
#define DSET_DY(ds) ((ds)->daxes->yydel)
#define DSET_DZ(ds) ((ds)->daxes->zzdel)

  /* these next 4 added 19 Aug 1999 */

#define DSET_index_to_ix(ds,ii)         (  (ii) % (ds)->daxes->nxx)
#define DSET_index_to_jy(ds,ii)         ( ((ii) / (ds)->daxes->nxx) % (ds)->daxes->nyy )
#define DSET_index_to_kz(ds,ii)         (  (ii) /((ds)->daxes->nxx * (ds)->daxes->nyy ))
#define DSET_ixyz_to_index(ds,ix,jy,kz) ((ix)+((jy)+(kz)*(ds)->daxes->nyy)*(ds)->daxes->nxx)

#define DSET_CUBICAL(ds) ( fabs((ds)->daxes->xxdel) == fabs((ds)->daxes->yydel) && \
                           fabs((ds)->daxes->xxdel) == fabs((ds)->daxes->zzdel)   )

#if 0  /* 22 Sep 2000 */
#define DSET_GRAPHABLE(ds) ( ISVALID_3DIM_DATASET(ds) && DSET_INMEMORY(ds)      && \
                             (ds)->wod_flag == False  && DSET_NUM_TIMES(ds) > 1 && \
                             ( DSET_ONDISK(ds) || DSET_LOADED(ds) && DSET_LOCKED(ds) ) )
#else
#define DSET_GRAPHABLE(ds) ( ISVALID_3DIM_DATASET(ds) && DSET_INMEMORY(ds)      && \
                             (ds)->wod_flag == False                            && \
                             ( DSET_ONDISK(ds) || DSET_LOADED(ds) && DSET_LOCKED(ds) ) )
#endif

#define DSET_TIMESTEP(ds)        ( ((ds)->taxis == NULL) ? 0.0 : (ds)->taxis->ttdel )
#define DSET_TR                  DSET_TIMESTEP
#define DSET_TIMEORIGIN(ds)      ( ((ds)->taxis == NULL) ? 0.0 : (ds)->taxis->ttorg )
#define DSET_TIMEDURATION(ds)    ( ((ds)->taxis == NULL) ? 0.0 : (ds)->taxis->ttdur )
#define DSET_TIMEUNITS(ds)       ( ((ds)->taxis == NULL) ? ILLEGAL_TYPE \
                                                         : (ds)->taxis->units_type )
#define DSET_NUM_TTOFF(ds)       ( ((ds)->taxis == NULL) ? 0 : (ds)->taxis->nsl )

/** 30 Nov 1997 **/

static char tmp_dblab[8] ;
#define DBLK_BRICK_LAB(db,iv) ( ((db)->brick_lab != NULL) ? ((db)->brick_lab[iv]) : "?" )
#define DSET_BRICK_LAB(ds,iv) DBLK_BRICK_LAB((ds)->dblk,(iv))
#define DSET_BRICK_LABEL      DSET_BRICK_LAB

#define DBLK_BRICK_STATCODE(db,iv)  \
 ( ((db)->brick_statcode != NULL) ? (db)->brick_statcode[iv] : ILLEGAL_TYPE )

#define DSET_BRICK_STATCODE(ds,iv)                                         \
   ( ISBUCKET((ds)) ? DBLK_BRICK_STATCODE((ds)->dblk,(iv))                 \
                    : (ISFUNC(ds) && (iv)==FUNC_ival_thr[(ds)->func_type]) \
                      ? (ds)->func_type : -1 )

#define DBLK_BRICK_STATAUX(db,iv)  \
 ( ((db)->brick_stataux != NULL) ? (db)->brick_stataux[iv] : NULL )

#define DSET_BRICK_STATAUX(ds,iv)                                          \
   ( ISBUCKET((ds)) ? DBLK_BRICK_STATAUX((ds)->dblk,(iv))                  \
                    : (ISFUNC(ds) && (iv)==FUNC_ival_thr[(ds)->func_type]) \
                      ? (ds)->stat_aux : NULL )

#define DBLK_BRICK_STATPAR(db,iv,jj) \
 ( ((db)->brick_stataux != NULL) ? (db)->brick_stataux[iv][jj] : 0.0 )

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

/** macros to load the self_name and labels of a dataset
    with values computed from the filenames --
    replaces user control/input of these values in to3d **/

#define DSET_FIX_NAMES(ds)                                       \
  ( strcpy((ds)->self_name,(ds)->dblk->diskptr->directory_name), \
    strcat((ds)->self_name,(ds)->dblk->diskptr->filecode)      , \
    strcpy((ds)->label1   ,(ds)->dblk->diskptr->filecode)      , \
    strcpy((ds)->label2   ,THD_DEFAULT_LABEL) )

/** macro to load statistics of a dataset if it
      (1) doesn't have statistics already, or
      (2) has bad statistics from the old to3d bug **/

#define RELOAD_STATS(dset)                                                  \
  if( ISVALID_3DIM_DATASET((dset)) &&                                       \
      ( !ISVALID_STATISTIC((dset)->stats) ||                                \
        ( (dset)->dblk->nvals > 1 &&                                        \
          (dset)->stats->bstat[1].min > (dset)->stats->bstat[1].max ) ) ){  \
     THD_load_statistics((dset)) ; }

#define DSET_VALID_BSTAT(dset,ii)                 \
  ( ISVALID_3DIM_DATASET((dset))     &&           \
    ISVALID_STATISTIC((dset)->stats) &&           \
    (ii) < (dset)->stats->nbstat     &&           \
    ISVALID_BSTAT( (dset)->stats->bstat[(ii)] ) )

#define DSET_CRUSH_BSTAT(dset,ii)                                 \
  do{ if( DSET_VALID_BSTAT(dset,ii) )                             \
         INVALIDATE_BSTAT((dset)->stats->bstat[(ii)]) ; } while(0)

#define DSET_KILL_STATS(ds)                                \
  do{ if( (ds)->stats != NULL ){                           \
         REMOVEFROM_KILL( (ds)->kl, (ds)->stats->bstat ) ; \
         REMOVEFROM_KILL( (ds)->kl, (ds)->stats ) ;        \
         KILL_STATISTIC( (ds)->stats ) ;                   \
         (ds)->stats = NULL ; } } while(0)

/** macro to initialize the stat_aux data in a dataset **/

#define INIT_STAT_AUX(ds,nf,ff)               \
  do{ int is ;                                \
      for( is=0 ; is < MAX_STAT_AUX ; is++ )  \
         (ds)->stat_aux[is] = (is < (nf)) ? (ff)[is] : 0.0 ; } while(0)

#define ZERO_STAT_AUX(ds)                              \
  do{ int is ; for( is=0 ; is < MAX_STAT_AUX ; is++ )  \
                 (ds)->stat_aux[is] = 0.0 ; } while(0)

/** macros to load and unload a dataset from memory **/

#define DSET_load(ds)   THD_load_datablock( (ds)->dblk , NULL )
#define DSET_unload(ds) THD_purge_datablock( (ds)->dblk , DATABLOCK_MEM_ANY )

#define DSET_unload_one(ds,iv) THD_purge_one_brick( (ds)->dblk , (iv) )

#define DSET_delete(ds) THD_delete_3dim_dataset((ds),False)

#define DSET_write(ds)  ( THD_load_statistics( (ds) ) ,                    \
                          THD_write_3dim_dataset( NULL,NULL , (ds),True ) )

#define DSET_write_header(ds)  THD_write_3dim_dataset( NULL,NULL , (ds),False )

#define DSET_LOADED(ds) ( THD_count_databricks((ds)->dblk) == DSET_NVALS(ds) )

#define DSET_lock(ds)      DBLK_lock((ds)->dblk)       /* Feb 1998 */
#define DSET_unlock(ds)    DBLK_unlock((ds)->dblk)
#define DSET_LOCKED(ds)    DBLK_LOCKED((ds)->dblk)
#define DSET_mallocize(ds) DBLK_mallocize((ds)->dblk)
#define DSET_mmapize(ds)   DBLK_mmapize((ds)->dblk)
#define DSET_anyize(ds)    DBLK_anyize((ds)->dblk)

#define DSET_superlock(ds) DBLK_superlock((ds)->dblk)  /* 22 Mar 2001 */

#define DSET_IS_MALLOC(ds)  DBLK_IS_MALLOC((ds)->dblk)
#define DSET_IS_MMAP(ds)    DBLK_IS_MMAP((ds)->dblk)

#define DSET_IS_MASTERED(ds) DBLK_IS_MASTERED((ds)->dblk)

/*------------- a dynamic array type for 3D datasets ---------------*/

typedef struct THD_3dim_dataset_array {
      int num , nall ;
      THD_3dim_dataset ** ar ;
} THD_3dim_dataset_array ;

#define INC_3DARR 8

#define INIT_3DARR(name)                  \
   ( (name) = XtNew(THD_3dim_dataset_array) ,\
     (name)->num = (name)->nall = 0 ,     \
     (name)->ar  = NULL )

#define ADDTO_3DARR(name,ddset)                                       \
   { if( (name)->num == (name)->nall ){                               \
      (name)->nall += INC_3DARR ;                                     \
      (name)->ar    = (THD_3dim_dataset **)                           \
                       XtRealloc( (char *) (name)->ar ,               \
                        sizeof(THD_3dim_dataset *) * (name)->nall ) ; \
     }                                                             \
     if( (ddset) != NULL ){               \
      (name)->ar[(name)->num] = (ddset) ; \
      ((name)->num)++ ;                  \
     } }

#define FREE_3DARR(name)      \
   if( (name) != NULL ){      \
     myXtFree( (name)->ar ) ; \
     myXtFree( (name) ) ; }

#define DSET_IN_3DARR(name,nn) ((name)->ar[(nn)])

#define DSET_ORDERED(d1,d2)                  \
  ( ( (d1)->view_type < (d2)->view_type ) || \
    ( (d1)->view_type==(d2)->view_type && (d1)->func_type<(d2)->func_type ) )

#define DSET_SWAP(d1,d2) (dt=(d1),(d1)=(d2),(d2)=dt)

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

typedef struct {
      int type , num_anat , num_func ;
      char sessname[THD_MAX_NAME] , lastname[THD_MAX_LABEL] ;

      THD_3dim_dataset * anat[THD_MAX_SESSION_ANAT][LAST_VIEW_TYPE+1] ;
      THD_3dim_dataset * func[THD_MAX_SESSION_FUNC][LAST_VIEW_TYPE+1] ;

      XtPointer parent ;
} THD_session ;

#define ISVALID_SESSION(ss) ( (ss) != NULL && (ss)->type == SESSION_TYPE )

#define BLANK_SESSION(ss) \
  if( ISVALID_SESSION((ss)) ){ \
      int id , vv ; \
      for( id=0 ; id < THD_MAX_SESSION_ANAT ; id++ ) \
        for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ) (ss)->anat[id][vv] = NULL ; \
      for( id=0 ; id < THD_MAX_SESSION_FUNC ; id++ ) \
        for( vv=0 ; vv <= LAST_VIEW_TYPE ; vv++ ) (ss)->func[id][vv] = NULL ; \
      (ss)->num_anat = (ss)->num_func = 0 ; }

#define SESSIONLIST_TYPE 107

typedef struct {
      int type , num_sess ;
      THD_session * ssar[THD_MAX_NUM_SESSION] ;
      XtPointer parent ;
} THD_sessionlist ;

#define ISVALID_SESSIONLIST(sl) ( (sl)!=NULL && (sl)->type==SESSIONLIST_TYPE )

#define BLANK_SESSIONLIST(sl) \
   if( ISVALID_SESSIONLIST((sl)) ){ \
      int is ; \
      for( is=0 ; is < THD_MAX_NUM_SESSION ; is++ ) (sl)->ssar[is] = NULL ; \
      (sl)->num_sess = 0 ; }

/*--- return type for sessionlist searching (see THD_dset_in_*) ---*/

typedef struct {
   int sess_index , anat_index , func_index , view_index ;
   THD_3dim_dataset * dset ;
} THD_slist_find ;

#define BADFIND(ff) \
   ( (ff).sess_index=(ff).anat_index=(ff).func_index=(ff).view_index=-1 , \
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

/************************************************************************/
/******************* rest of prototypes *********************************/

#ifndef DONT_USE_SCANDIR
#ifdef SCANDIR_WANTS_CONST
   extern int THD_select_dirent( const struct dirent * dp ) ;
#else
   extern int THD_select_dirent( struct dirent * dp ) ;
#endif
#endif

extern THD_string_array * THD_get_all_filenames( char * ) ;
extern THD_string_array * THD_extract_regular_files( THD_string_array * ) ;
extern THD_string_array * THD_extract_directories( THD_string_array * ) ;
extern int THD_is_file( char * pathname ) ;
extern int THD_is_symlink( char * pathname ) ;  /* 03 Mar 1999 */
extern int THD_is_directory( char * pathname ) ;
extern int THD_equiv_files( char * , char * ) ;
extern long THD_filesize( char * pathname ) ;
extern THD_string_array * THD_get_all_subdirs( int , char * ) ;
extern THD_string_array * THD_normalize_flist( THD_string_array * ) ;
extern THD_string_array * THD_get_wildcard_filenames( char * ) ;

extern int THD_is_dataset( char * , char * , int ) ; /* 17 Mar 2000 */
extern char * THD_dataset_headname( char * , char * , int ) ;

extern MRI_IMARR * THD_get_all_timeseries( char * ) ;
extern MRI_IMARR * THD_get_many_timeseries( THD_string_array * ) ;
extern char * THD_trailname( char * fname , int lev ) ;

extern int THD_linecount( char * ) ;

extern void THD_read_all_atr( char * , THD_datablock * ) ;
extern void THD_erase_all_atr( THD_datablock * ) ;
extern void THD_erase_one_atr( THD_datablock * , char * ) ;

extern ATR_any    * THD_find_atr       ( THD_datablock * , char * ) ;
extern ATR_float  * THD_find_float_atr ( THD_datablock * , char * ) ;
extern ATR_int    * THD_find_int_atr   ( THD_datablock * , char * ) ;
extern ATR_string * THD_find_string_atr( THD_datablock * , char * ) ;

extern void THD_set_atr( THD_datablock * , char * , int,int, void * ) ;

extern void THD_store_dataset_keywords ( THD_3dim_dataset * , char * ) ;
extern void THD_append_dataset_keywords( THD_3dim_dataset * , char * ) ;
extern char * THD_dataset_info( THD_3dim_dataset * , int ) ;
extern char * THD_zzprintf( char * sss , char * fmt , ... ) ;

extern void THD_set_float_atr( THD_datablock * , char * , int , float * ) ;
extern void THD_set_int_atr  ( THD_datablock * , char * , int , int   * ) ;
extern void THD_set_char_atr ( THD_datablock * , char * , int , char  * ) ;

#define THD_set_string_atr(blk,name,str) \
   THD_set_char_atr( (blk) , (name) , strlen(str)+1 , (str) )

extern void THD_init_diskptr_names( THD_diskptr *, char *,char *,char * ,
                                    int, Boolean ) ;

extern THD_datablock *       THD_init_one_datablock( char *,char * ) ;
extern THD_datablock_array * THD_init_prefix_datablocks( char *, THD_string_array * ) ;

extern XtPointer_array * THD_init_alldir_datablocks( char * ) ;

extern THD_session * THD_init_session( char * ) ;

extern THD_3dim_dataset * THD_open_one_dataset( char * ) ;
extern THD_3dim_dataset * THD_open_dataset( char * ) ;      /* 11 Jan 1999 */

extern THD_3dim_dataset * THD_fetch_dataset      (char *); /* 23 Mar 2001 */
extern XtPointer_array *  THD_fetch_many_datasets(char *);
extern MRI_IMAGE *        THD_fetch_1D           (char *); /* 26 Mar 2001 */

extern int * MCW_get_intlist( int , char * ) ;
extern void MCW_intlist_allow_negative( int ) ;             /* 22 Nov 1999 */

#define MASTER_SHORTHELP_STRING                                                 \
 "INPUT DATASET NAMES\n"                                                        \
 "-------------------\n"                                                        \
 "This program accepts datasets that are modified on input according to the\n"  \
 "following schemes:\n"                                                         \
 "  'r1+orig[3..5]'                                    {sub-brick selector}\n"  \
 "  'r1+orig<100.200>'                                 {sub-range selector}\n"  \
 "  'r1+orig[3..5]<100..200>'                          {both selectors}\n"      \
 "  '3dcalc( -a r1+orig -b r2+orig -expr 0.5*(a+b) )'  {calculation}\n"         \
 "For the gruesome details, see the output of 'afni -help'.\n"

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
    "   fred+orig[5,9,17]                ==> use #5, #9, and #12\n"           \
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
    " selection, it is the same as if you had put [1..$] in front of\n"       \
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

extern void THD_delete_3dim_dataset( THD_3dim_dataset * , Boolean ) ;
extern THD_3dim_dataset * THD_3dim_from_block( THD_datablock * ) ;
extern void THD_allow_empty_dataset( int ) ; /* 23 Mar 2001 */
extern THD_3dim_dataset_array *
   THD_array_3dim_from_block( THD_datablock_array * blk_arr ) ;

extern Boolean THD_write_3dim_dataset( char *,char * ,
                                       THD_3dim_dataset * , Boolean );

extern Boolean THD_write_datablock( THD_datablock * , Boolean ) ;
extern Boolean THD_write_atr( THD_datablock * ) ;
extern void THD_set_write_compression( int mm ) ;
extern int THD_enviro_write_compression(void) ;
extern int THD_get_write_compression(void) ;

extern void THD_set_write_order( int ) ;
extern void THD_enviro_write_order(void) ;
extern int THD_get_write_order(void) ;

extern int TRUST_host(char *) ;
#define OKHOST(hh) TRUST_host(hh) ;
extern void TRUST_addhost(char *) ;      /* 21 Feb 2001 */

extern Boolean THD_load_datablock ( THD_datablock * , generic_func * ) ;
extern Boolean THD_purge_datablock( THD_datablock * , int ) ;
extern Boolean THD_purge_one_brick( THD_datablock * , int ) ;
extern void    THD_force_malloc_type( THD_datablock * , int ) ;
extern int     THD_count_databricks( THD_datablock * dblk ) ;

extern void THD_reconcile_parents( THD_sessionlist * ) ;
extern THD_slist_find THD_dset_in_sessionlist( int,void *, THD_sessionlist *, int ) ;
extern THD_slist_find THD_dset_in_session( int,void * , THD_session * ) ;

extern void THD_check_idcodes( THD_sessionlist * ) ; /* 08 Jun 1999 */

extern void THD_load_statistics( THD_3dim_dataset * ) ;
extern void THD_update_statistics( THD_3dim_dataset * ) ;
extern THD_brick_stats THD_get_brick_stats( MRI_IMAGE * ) ;

extern THD_fvec3 THD_3dind_to_3dmm( THD_3dim_dataset * , THD_ivec3 ) ;
extern THD_ivec3 THD_3dmm_to_3dind( THD_3dim_dataset * , THD_fvec3 ) ;

extern THD_fvec3 THD_3dfind_to_3dmm( THD_3dim_dataset * , THD_fvec3 ) ;
extern THD_fvec3 THD_3dmm_to_3dfind( THD_3dim_dataset * , THD_fvec3 ) ;

extern THD_fvec3 THD_3dmm_to_dicomm( THD_3dim_dataset * , THD_fvec3 ) ;
extern THD_fvec3 THD_dicomm_to_3dmm( THD_3dim_dataset * , THD_fvec3 ) ;

extern float THD_timeof      ( int , float , THD_timeaxis * ) ;
extern float THD_timeof_vox  ( int , int , THD_3dim_dataset * ) ;
extern float THD_timeof_slice( int , int , THD_3dim_dataset * ) ;  /* BDW */

extern THD_fvec3 THD_dataset_center( THD_3dim_dataset * ) ;  /* 01 Feb 2001 */
extern int THD_dataset_mismatch(THD_3dim_dataset *, THD_3dim_dataset *) ;

extern int THD_dataset_tshift( THD_3dim_dataset * , int ) ; /* 15 Feb 2001 */

#define MISMATCH_CENTER  (1<<0)  /* within 0.2 voxel */
#define MISMATCH_DELTA   (1<<1)  /* within 0.001 voxel */
#define MISMATCH_ORIENT  (1<<2)
#define MISMATCH_DIMEN   (1<<3)

/*----------------------------------------------------------------*/
/*--------  FD_brick type: for rapid extraction of slices --------*/

typedef struct FD_brick {

   THD_ivec3 nxyz ,     /* actual dimensions as read in */
             sxyz ,     /* starting indices in each dataset dimen */
             a123 ;     /* axis codes as supplied in
                           THD_3dim_dataset_to_brick */

   int n1,d1,e1 ,       /* ni = length in direction i */
       n2,d2,e2 ,       /* di = stride in direction i */
       n3,d3,           /* ei = last index in direc i */
       start ;          /* start = offset of 1st elem */

   float del1,del2,del3 ;       /* voxel dimensions */

   THD_3dim_dataset * dset ;    /* pointer to parent dataset */
   int resam_code , thr_resam_code ;

   char namecode[32] ;  /* June 1997 */

   XtPointer parent ;
} FD_brick ;

#define ROT3(a,b,c,na,nb,nc) ((na)=(b),(nb)=(c),(nc)=(a))

#define BRICK_DRAWABLE(br) ((br)->n1 > 1 && (br)->n2 > 1)

extern FD_brick * THD_3dim_dataset_to_brick( THD_3dim_dataset * ,
                                             int,int,int ) ;

extern MRI_IMAGE * FD_brick_to_mri( int,int , FD_brick * br ) ;
extern MRI_IMAGE * FD_brick_to_series( int , FD_brick * br ) ;

extern MRI_IMAGE * THD_extract_series( int , THD_3dim_dataset * , int ) ;
extern MRI_IMARR * THD_extract_many_series( int, int *, THD_3dim_dataset * );

extern void THD_insert_series( int, THD_3dim_dataset *, int, int, void *, int );

/*--------------- routines that are in thd_detrend.c ---------------*/

extern void get_linear_trend     ( int, float *, float *, float * ) ;
extern void THD_linear_detrend   ( int, float *, float *, float * ) ;
extern void get_quadratic_trend  ( int, float *, float *, float *, float * ) ;
extern void THD_quadratic_detrend( int, float *, float *, float *, float * ) ;
extern void THD_normalize        ( int, float * ) ;
extern void THD_cubic_detrend    ( int, float * ) ;  /* 15 Nov 1999 */

#define DETREND_linear(n,f)    THD_linear_detrend(n,f,NULL,NULL)
#define DETREND_quadratic(n,f) THD_quadratic_detrend(n,f,NULL,NULL,NULL)
#define DETREND_cubic(n,f)     THD_cubic_detrend(n,f)

/*------------------------------------------------------------------*/

extern THD_ivec3 THD_fdind_to_3dind( FD_brick * , THD_ivec3 ) ;
extern THD_ivec3 THD_3dind_to_fdind( FD_brick * , THD_ivec3 ) ;

extern FD_brick ** THD_setup_bricks( THD_3dim_dataset * ) ;

extern int thd_floatscan  ( int , float *   ) ; /* 30 Jul 1999 */
extern int thd_complexscan( int , complex * ) ; /* 14 Sep 1999 */

extern byte * THD_makemask( THD_3dim_dataset *, int,float,float) ;
extern int    THD_countmask( int , byte * ) ;

 /* 08 Mar 2001 - functions for dealing with rows */

extern int THD_get_dset_rowcount( THD_3dim_dataset *, int ) ;
extern void * THD_get_dset_row( THD_3dim_dataset *, int, int, int,int,int ) ;
extern void THD_put_dset_row( THD_3dim_dataset *, int,
                              int, int,int,int, void * row ) ;
extern int THD_dataset_rowfillin( THD_3dim_dataset *, int, int, int ) ;

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

extern THD_dvecmat THD_rotcom_to_matvec( THD_3dim_dataset * , char * ) ;

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

extern THD_dvecmat DLSQ_rot_trans( int, THD_dfvec3 *, THD_dfvec3 *, double * ww ) ;

extern THD_dvecmat THD_read_dvecmat( char * , int ) ;  /* THD_read_vecmat.c */

  /* cf. thd_tmask.c */

#define TM_IXY 2  /* fixdir-1 for each plane */
#define TM_IYZ 0
#define TM_IZX 1

typedef struct {
   int   nmask[3] ;
   byte * mask[3] ;
} Tmask ;

extern void free_Tmask( Tmask * ) ;
extern Tmask * create_Tmask_byte( int, int, int, byte * ) ;

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

  /*-- see mri_3dalign.c for these routines --*/

typedef struct {
   MRI_IMARR * fitim ;
   double * chol_fitim ;
} MRI_3dalign_basis ;

extern void mri_3dalign_edging( int , int , int ) ;
extern void mri_3dalign_edging_default( int , int , int ) ;
extern void mri_3dalign_force_edging( int ) ;

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

#if 0
extern float THD_thresh_to_pval( float thr , THD_3dim_dataset * dset ) ;
#endif

extern float THD_stat_to_pval  ( float thr , int statcode , float * stataux ) ;
extern float THD_pval_to_stat  ( float pval, int statcode , float * stataux ) ;
extern float THD_stat_to_zscore( float thr , int statcode , float * stataux ) ;

extern int THD_filename_ok( char * ) ;   /* 24 Apr 1997 */
extern int THD_filename_pure( char * ) ; /* 28 Feb 2001 */

extern THD_warp * AFNI_make_voxwarp( THD_warp * , THD_3dim_dataset * ,
                                                  THD_3dim_dataset *  ) ;

extern THD_linear_mapping * AFNI_make_voxmap( THD_linear_mapping * ,
                                              THD_dataxes * , THD_dataxes * ) ;

extern void AFNI_concatenate_warp( THD_warp * , THD_warp * ) ;

extern THD_linear_mapping * AFNI_concatenate_lmap( THD_linear_mapping * ,
                                                   THD_linear_mapping *  ) ;

extern THD_3dim_dataset * WINsorize( THD_3dim_dataset * ,
                                     int, int, int, float, char *, int ) ;

#define ZPAD_EMPTY (1<<0)
#define ZPAD_PURGE (1<<1)
#define ZPAD_MM    (1<<2)

extern THD_3dim_dataset * THD_zeropad( THD_3dim_dataset * ,
                                       int,int,int,int,int,int , char * , int ) ;

/*-- 02 Mar 2001: thd_entropy16.c --*/

extern void   ENTROPY_setup     (void) ;
extern void   ENTROPY_setdown   (void) ;
extern void   ENTROPY_accumulate(int , void *) ;
extern double ENTROPY_compute   (void) ;
extern double ENTROPY_dataset   (THD_3dim_dataset *) ;
extern double ENTROPY_datablock (THD_datablock *) ;

/*--------------------------------------------------------------------------*/

/*--- Stuff for Tom Ross's NOTES ---*/

#define MAX_DSET_NOTES 999
#define MAX_NOTE_SIZE  4000

extern void   tross_Add_Note   (THD_3dim_dataset *, char *) ;
extern void   tross_Delete_Note(THD_3dim_dataset *, int   ) ;

extern char * tross_Expand_String( char * ) ;
extern char * tross_Encode_String( char * ) ;

extern void   tross_Store_Note   ( THD_3dim_dataset * , int , char * ) ;
extern char * tross_Get_Note     ( THD_3dim_dataset * , int ) ;
extern char * tross_Get_Notedate ( THD_3dim_dataset * , int ) ;
extern int    tross_Get_Notecount( THD_3dim_dataset * ) ;

extern char * tross_datetime(void) ;
extern char * tross_commandline( char * , int , char ** ) ;

extern void   tross_Append_History ( THD_3dim_dataset * , char * ) ;
extern char * tross_Get_History    ( THD_3dim_dataset * ) ;
extern void   tross_Make_History   ( char *, int, char **, THD_3dim_dataset * ) ;
extern void   tross_Copy_History   ( THD_3dim_dataset *, THD_3dim_dataset * ) ;
extern void   tross_Replace_History( THD_3dim_dataset * , char * ) ;

#define tross_Erase_History(ds) THD_erase_one_atr((ds)->dblk,"HISTORY_NOTE")

extern char * tross_breakup_string( char *, int , int ) ;

#include <stdarg.h>
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

#if 0
extern char * MD5_B64_array ( int , char * ) ;
extern char * MD5_B64_string( char * ) ;
extern char * MD5_B64_file  (char * ) ;
#endif

#endif /* _MCW_3DDATASET_ */
