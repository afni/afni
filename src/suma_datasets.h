#ifndef SUMA_DATASETS_INCLUDED
#define SUMA_DATASETS_INCLUDED

#include "matrix.h"
#include "suma_afni_surface.h"

#define MAX_ERRLOG_MSG 1000
#define MAX_ERRLOG_FUNCNAME 200

/*! macro to avoid typecasting warnings when going from 
    void * pointers (or XtPointer) to int and vice versa */
   /* I use INT_MAX and LONG_MAX
   to guess whether or not we have 64 bit pointers.
   I need to guess with #if to do proper type casting and
   avoid compiler warnings */
#if INT_MAX < LONG_MAX
   #define INT_CAST  int)(long int
   #define VOID_CAST  void *)(long int
   #define CVOID_CAST  const void *)(long int
   #define XTP_CAST  XtPointer)(long int
   #define NIGRP_CAST NI_group *)(long int
#else 
   #define INT_CAST int
   #define VOID_CAST void *
   #define CVOID_CAST const void *
   #define XTP_CAST  XtPointer
   #define NIGRP_CAST NI_group *
#endif


#ifdef USE_TRACING
#define SUMA_DUMP_TRACE(ihead) { /* taken from dbtrace.h */\
   int m_ii;  \
   char *head=(char*)ihead; /* a trick to quiet compiler warnings about \
               fixed length   strings addresses always evaluating to true */\
   if (head) { \
      SUMA_S_Note(head);\
   } else {SUMA_S_Note("Dumping Trace:");\
   }   \
   if( DBG_num >= 0 ){  \
      for( m_ii=DBG_num-1; m_ii >= 0 ; m_ii-- ) \
         fprintf(stderr,"%*.*s%s\n",m_ii+1,m_ii+1," ",DBG_rout[m_ii]) ; \
   } else { \
      fprintf(stderr,"[No debug tracing stack: DBG_num=%d]\n",DBG_num) ;   \
   }  \
}
#else
#define SUMA_DUMP_TRACE(x) /* nada */
#endif

typedef struct {
    char macroname[100];
    char msg[MAX_ERRLOG_MSG];
    char FuncName[MAX_ERRLOG_FUNCNAME];
} SUMA_ERRLOG;

typedef struct {
   char *envhelp;
   char *envname;
   char *envval;  /* This is the default */
}ENV_SPEC;


#define SUMA_MAX_NAME_LENGTH 500   /*!< Maximum number of characters in a filename */
#define SUMA_MAX_DIR_LENGTH 2000    /*!< Maximum number of characters in a directory name */
#define SUMA_MAX_FILENAME_LENGTH (SUMA_MAX_NAME_LENGTH+SUMA_MAX_DIR_LENGTH+1)
#ifndef SUMA_IDCODE_LENGTH
   #define SUMA_IDCODE_LENGTH 50
#endif

#define SUMA_VERSION_VECTOR 20060703, 20041229, 20040610, 20040116, \
                            20040106, -10000 /*!< modify this dude and you must update SUMA_New_Additions_perver 
                                       in SUMA_help.c. 
                                       Add to the left of the vector, leave the last value of -10000 untouched
                                       If you like to think of floating point version numbers,divide by 10000
                                       This define is stuck here so that non-SUMA DataSet manipulating programs 
                                       can use it, one hopes.
                                       Numbering is yyyymmdd */

#define SUMA_EMPTY_ATTR "~"
#define AFNI_NI_CSS "~"    
#define AFNI_NI_cSS '~'    /* AFNI's NIML Column String Separator (used to separate strings belonging to different columns of input) 
                              AFNI takes the niml attribute string and turns it into its internal attribute structure.
                              AFNI_NI_CSS[0] is replaced by \0 and the string is broken into its per-column string list.
                              That is not the case for BRICK_STATSYM which is treated as one ';' separated string that gets
                              separately transformed into BRICK_STATAUX attribute entry in AFNI.
                              This is somewhat confusing when viewing an AFNI dataset in NIML format because BRICK_STATSYM is of ni_type = "String"
                              like say BRICK_LABS. However this makes sense in AFNI's format because these strings actually
                              get transformed to attributes of different types in AFNI's internal format.
                              SUMA will use the same separator for all attributes of ni_type = "String".
                              Use Macros SUMA_2_AFNI_NI_PCS and AFNI_2_SUMA_NI_PCS if you need to change between SUMA- and AFNI-per-column-strings  
                              */
#define SUMA_NI_CSS ";"    /* SUMA's NIML  Per-Column-String Separator 
                              (used to separate strings belonging to 
                              different columns of input)  */
#define SUMA_NI_cSS ';'

#define SUMA_2_AFNI_NI_PCS(a) {\
   int m_i = 0; \
   if ((a)) { while ((a)[m_i]) { if ((a)[m_i] == SUMA_NI_cSS) (a)[m_i] = AFNI_NI_cSS; ++m_i; } }\
}

#define AFNI_2_SUMA_NI_PCS(a) {\
   int m_i = 0; \
   if ((a)) { while ((a)[m_i]) { if ((a)[m_i] == AFNI_NI_cSS) (a)[m_i] = SUMA_NI_cSS; ++m_i; } }\
}

#define SUMA_Boolean byte
#define NOPE 0
#define YUP 1
/* typedef enum { NOPE, YUP} SUMA_Boolean;     make sure SUMA_Boolean is byte */ 

typedef enum { SUMA_NO_NUM_UNITS = 0, 
               SUMA_MM_UNITS,
               SUMA_P_VALUE_UNITS,
               SUMA_Q_VALUE_UNITS,
               
               SUMA_N_NUMERICAL_UNITS
               } SUMA_NUMERICAL_UNITS;

typedef enum { SUMA_notypeset = -1, 
               SUMA_byte = NI_BYTE, 
               SUMA_short = NI_SHORT, 
               SUMA_int = NI_INT, 
               SUMA_float = NI_FLOAT32, 
               SUMA_double = NI_FLOAT64, 
               SUMA_complex = NI_COMPLEX64,
               SUMA_string = NI_STRING} SUMA_VARTYPE;

/*! simple vectors */
typedef struct {
   int n;
   int *v;
} SUMA_IVEC;

typedef struct {
   int n;
   float *v;
} SUMA_FVEC;

typedef struct {
   int n;
   double *v;
} SUMA_DVEC;

#define SUMA_MX_VEC_MAX_DIMS 50
typedef struct {
   SUMA_VARTYPE tp;
   int N_dims;
   int N_vals;
   int dims[SUMA_MX_VEC_MAX_DIMS];
   int fdfm[SUMA_MX_VEC_MAX_DIMS];
   void *v;
   byte *bv;
   short *sv;
   int *iv;
   float *fv;
   double *dv;
   complex *cv;
   byte fdf;
   matrix *m;
} SUMA_MX_VEC;

/*! macros to access pointers to elements in type double multiplexed vectors with mxv->fdf = 1;
  \sa SUMA_NewMxVec*/ 
#define mxvdp4(mxv,i,j,k,l) ( mxv->dv + ( (int)(i) + (int)(j) * mxv->fdfm[0] + (int)(k) * mxv->fdfm[1] + (int)(l) * mxv->fdfm[2] ) )
#define mxvdp3(mxv,i,j,k)   ( mxv->dv + ( (int)(i) + (int)(j) * mxv->fdfm[0] + (int)(k) * mxv->fdfm[1]   ) )
#define mxvdp2(mxv,i,j  )   ( mxv->dv + ( (int)(i) + (int)(j) * mxv->fdfm[0]   ) )
#define mxvdp1(mxv,i  )     ( mxv->dv + ( (int)(i)   ) )
/*! macros to access elements in type double multiplexed vectors with mxv->fdf = 1;
  \sa SUMA_NewMxVec*/ 
#define mxvd4(mxv,i,j,k,l) ( mxv->dv[( (int)(i) + (int)(j) * mxv->fdfm[0] + (int)(k) * mxv->fdfm[1] + (int)(l) * mxv->fdfm[2] )] )
#define mxvd3(mxv,i,j,k)   ( mxv->dv[( (int)(i) + (int)(j) * mxv->fdfm[0] + (int)(k) * mxv->fdfm[1]   )] )
#define mxvd2(mxv,i,j  )   ( mxv->dv[( (int)(i) + (int)(j) * mxv->fdfm[0]   )] )
#define mxvd1(mxv,i  )     ( mxv->dv[( (int)(i)   )] )
/*! macros to access elements in type complex multiplexed vectors with mxv->fdf = 1;
  \sa SUMA_NewMxVec*/ 
#define mxvc4(mxv,i,j,k,l) ( mxv->cv[( (int)(i) + (int)(j) * mxv->fdfm[0] + (int)(k) * mxv->fdfm[1] + (int)(l) * mxv->fdfm[2] )] )
#define mxvc3(mxv,i,j,k)   ( mxv->cv[( (int)(i) + (int)(j) * mxv->fdfm[0] + (int)(k) * mxv->fdfm[1]   )] )
#define mxvc2(mxv,i,j  )   ( mxv->cv[( (int)(i) + (int)(j) * mxv->fdfm[0]   )] )
#define mxvc1(mxv,i  )     ( mxv->cv[( (int)(i)   )] )

/*! filename and path */
typedef struct {
   char *Path;
   char *FileName;
}SUMA_FileName;

/*! filename, extension and path */
typedef struct {
   char *AbsPath;
   char *Path;
   char *FileName;
   char *FileName_NoExt;
   char *FullName;
   char *Ext;
   char *NodeSelect;
   char *ColSelect;
   char *RowSelect;
   char *RangeSelect;
   int only_index;
}SUMA_PARSED_NAME;


/*! string stucture 
*/
typedef struct {
   int N_alloc;  /*!< space allocated for s */
   char *s; /*!< string s */
} SUMA_STRING;

typedef enum {
   SUMA_ERROR_DSET_TYPE = -1,
   SUMA_NO_DSET_TYPE,
   SUMA_NODE_BUCKET,
   SUMA_AFNI_NODE_BUCKET,
   SUMA_NODE_ROI, /*!< Col0: Node ID, Col1: ROI label (int) */
   SUMA_NODE_RGB,
   SUMA_NODE_RGBb,
   SUMA_NODE_RGBA,
   SUMA_NODE_RGBAb,
   SUMA_NODE_LABEL,
   SUMA_NODE_XYZ,
   SUMA_NEW_NODE_XYZ,
   SUMA_NODE_CONVEXITY,
   SUMA_NEW_MESH_IJK,
   SUMA_MESH_IJK,
   SUMA_PREP_NEW_SURFACE,
   SUMA_VIEWER_SETTING,
   SUMA_SURFACE_VOLUME_PARENT,
   SUMA_SURFACE_OBJECT,
   SUMA_ENGINE_INSTRUCTION,
   SUMA_SEGMENT_OBJECT,
   SUMA_LABEL_TABLE_OBJECT,
   SUMA_N_DSET_TYPES
} SUMA_DSET_TYPE; /*!<  Type of data set ( should be called Object, not DSET ) 
                        When you add a new element, modify functions
                        SUMA_Dset_Type_Name
                        SUMA_Dset_Type */

typedef enum {
   SUMA_ERROR_DSET_FORMAT = -1,
   SUMA_NO_DSET_FORMAT,       /* 0 */
   SUMA_ASCII_NIML,           /* 1 */
   SUMA_BINARY_NIML,          /* 2 */
   SUMA_NIML,                 /* 3 */
   SUMA_1D,                   /* 4 */
   SUMA_1D_PURE,              /* 5 */
   SUMA_ASCII_OPEN_DX_DSET,   /* 6 */
   SUMA_1D_STDOUT,            /* THIS ONE IS USED AS A MARKER TOO   */    /*7 */
   SUMA_1D_STDERR,            /* 8 */
   SUMA_NIML_STDOUT,          /* 9 */
   SUMA_NIML_STDERR,          /* 10 */
   SUMA_1D_PURE_TRANSPOSE,    /* 11 */
   SUMA_1D_PURE_STDOUT,       /* 12 */
   SUMA_1D_PURE_STDERR,       /* 13 */
   SUMA_1D_PURE_STDOUT_TRANSPOSE,       /* 14 */
   SUMA_1D_PURE_STDERR_TRANSPOSE,/* THIS ONE IS USED AS A MARKER TOO*/  /* 15 */
   SUMA_XML_DSET,                  /* 16 */
   SUMA_XML_ASCII_DSET,            /* 17 */
   SUMA_XML_B64_DSET,              /* 18 */
   SUMA_XML_B64GZ_DSET             /* 19 */
   
} SUMA_DSET_FORMAT; /*!<  Format of data set
                          When you add a new element, modify functions
                          SUMA_Dset_Format_Name
                          SUMA_Dset_Format */ 
#define SUMA_IS_DSET_1D_FORMAT(d) ( (d)==SUMA_1D || (d)==SUMA_1D_PURE || (d)==SUMA_1D_STDOUT || (d)==SUMA_1D_STDERR ) ? 1:0
#define SUMA_IS_DSET_STDXXX_FORMAT(d) ( (d)>=SUMA_1D_STDOUT && (d)<= SUMA_1D_PURE_STDERR_TRANSPOSE) ? 1:0

typedef enum {
   SUMA_ERROR_COL_TYPE = -1,
   SUMA_NO_COL_TYPE,
   SUMA_NODE_INT,    /*!< Generic integer */
   SUMA_NODE_INDEX,  /*!< index of a node to locate it in its domain */
   SUMA_NODE_ILABEL, /*!< An integer coding for a label */
   SUMA_NODE_SLABEL, /*!< An integer coding for a string label */
   SUMA_NODE_FLOAT,  /*!< Generic float */ 
   SUMA_NODE_CX,     /*!< Node convexity */
   SUMA_NODE_X,      /*!< Node X coordinate */
   SUMA_NODE_Y,      /*!< Node Y coordinate */
   SUMA_NODE_Z,      /*!< Node Z coordinate */
   SUMA_NODE_3C,     /*!<  Node XYZ triplets */
   SUMA_NODE_R,      /*!< Node R color */
   SUMA_NODE_G,      /*!< Node G color */
   SUMA_NODE_B,      /*!< Node B color */
   SUMA_NODE_A,      /*!< Node A value */ 
   SUMA_NODE_BYTE,   /*!< Generic byte */
   SUMA_NODE_Rb,      /*!< Node R color in bytes*/
   SUMA_NODE_Gb,      /*!< Node G color in bytes*/
   SUMA_NODE_Bb,      /*!< Node B color in bytes*/
   SUMA_NODE_Ab,      /*!< Node A value in bytes*/ 
   SUMA_NODE_STRING,   /*!< Generic String */
   SUMA_NODE_SHORT,      /*!< Generic short */
   SUMA_NODE_DOUBLE,     /*!< Generic double */
   SUMA_NODE_XCORR      /*!< Cross Correlation Coefficient */ 
}  SUMA_COL_TYPE; /*!<  Column types.
                        When you add a new element, you need to modify
                        SUMA_AddColAttr
                        SUMA_Col_Type 
                        SUMA_Col_Type_Name
                        SUMA_ColType2TypeCast
                        */


/*! 
I do not think we can have both nodes and triangles in this struct.
I guess I can make this be a Node Datum then create a similar struct
for triangle Datum and add them to SUMA_NIML_DRAWN_ROI.
If you do something like this rename:
SUMA_NIML_ROI_DATUM to SUMA_NIML_NODE_ROI_DATUM
*/
typedef struct {
   int action; /*!< action taken with this datum, 
                     see same field in SUMA_ROI_DATUM */
   int Type; /*!< describes the type of the DrawnROI datum 
                  (see SUMA_ROI_TYPE) */
   int N_n; 
   int *nPath;
/* int Type;
   int N_t;
   int *tPath; */
} SUMA_NIML_ROI_DATUM; /*!< a version of SUMA_ROI_DATUM struct 
                           that can be used by niml. */

typedef struct {
   int Type;         /*!< The final type of the DrawnROI, 
                           see SUMA_ROI_DRAWING_TYPE*/
   char *idcode_str;
   char *Parent_idcode_str;
   char *Label;
   char *ColPlaneName;
   float FillColor[4];  /*!< RGB fill color */
   float EdgeColor[4];  /*!< RGB edge color */
   int EdgeThickness;   /*!< thickness of edge */
   int iLabel;
   SUMA_NIML_ROI_DATUM *ROI_datum; /*!< a vector of ROI data 
                                       (a multitude of ROI datum) */
   int N_ROI_datum;
} SUMA_NIML_DRAWN_ROI; /*!< a version of SUMA_DRAWN_ROI struct that 
                           can be used by niml. Fields are a reflection 
                           of those in SUMA_DRAWN_ROI*/

typedef enum { SUMA_NO_PTR_TYPE, 
               SUMA_LINKED_DSET_TYPE, /*!< For pointers to SUMA_DSET */
               SUMA_LINKED_OVERLAY_TYPE, /*!< For pointers to SUMA_OVERLAYS */
               SUMA_LINKED_ND_FRST_NEI_TYPE, /*!< For pointers to SUMA_NODE_FIRST_NEIGHB*/
               SUMA_LINKED_MEMB_FACE_TYPE, /*!< For pointers to SUMA_MEMBER_FACE_SETS*/
               SUMA_LINKED_SURFCONT_TYPE, /*!< For pointers to SUMA_X_SurfCont*/
               SUMA_N_LINKED_PTR_TYPES } SUMA_LINKED_PTR_TYPES;
/*!   
   Structure to track copies of a certain pointer.
   DO NOT CHANGE THE ORDER OF THE STRUCTURE's FIELDS 
*/

typedef struct {
   int LinkedPtrType; /*!< Indicates the type of linked pointer */
   int N_links;   /*!< Number of links to this pointer */
   char owner_id[SUMA_IDCODE_LENGTH];   /*!< The id of whoever created that pointer. Might never get used.... */
} SUMA_LinkedPtr;

/*! Structure to contain a dataset defined on the surface */

typedef struct {
   /* *** DO NOT ADD ANYTHING BEFORE THESE FIELDS
          DO NOT CHANGE THE ORDER OF THESE FIELDS
          These fields are use for tracking copies
          (links) to a pointer.
          ANY CHANGES HERE SHOULD BE REFLECTED IN 
          SUMA_LinkedPtr structure 
   */
   int LinkedPtrType; /*!< Indicates the type of linked pointer */
   int N_links;   /*!< Number of links to this pointer */
   char owner_id[SUMA_IDCODE_LENGTH];   /*!< The id of whoever created that pointer. Might never get used.... */
   
   #ifdef OLD_DSET
   NI_element *nel;  /*!< The whole deal 
      nel is a NIML data element which is briefly
      defined by a set of attributes, and a collection
      of data columns.
      nel contains the following string attributes:
         filename: The filename
         label: A short text label identifying the data set.
                Typically, a short version of the filename                          
         idcode_str: Unique identifier for the data set
         MeshParent_idcode: Unique identifier of the surface containing the mesh  
                            over which this set is defined
         geometry_parent_idcode: Unique identifier of the surface containing the 
                            coordinates of the nodes whose attributes 
                            (values) are in this set.
         sorted_node_def: flag indicating that nodes in NodeDef are sorted
                          see NodeDef below. 
         LabelCol_'i': Label of column i
         RangeCol_'i': Range of values in column i. 
                       See function:
                        SUMA_GetColRange.
         TypeCol_'i': Type of data in column i.
                      See functions:
                        SUMA_Col_Type  
                        SUMA_Col_Type_Name
                        SUMA_ColType2TypeCast 
                      and typedef:
                        SUMA_COL_TYPE             
         AttrCol_'i': Attributes specific to that column type.
                      At the moment, I don't use it much. But
                      think attributes to store with an f-stat
                      column for example and so on.
         RangeCol_, TypeCol_ and AttrCol_: are automatically 
                                           generated, see:
                                             SUMA_AddColAttr
                                             SUMA_AddGenColAttr
      
      nel structure contains the following fields:
         name: A string for the type of dataset.
               See functions:
                  SUMA_Dset_Type_Name
                  SUMA_Dset_Type
               and typedef:
                  SUMA_DSET_TYPE
         vec: A vector of pointers to the data columns. 
         vec_num: Number of columns in vec. So your columns
                  are vec[0] .. vec[vec_num - 1]
                  THINK SUB-BRICKS
         vec_len: Total number of rows in the dset. Think total
                  number of voxels.
         vec_filled: Number of rows (node data) filled in the dset.
                     You'd think this should be equal to vec_len,
                     but in instances where you may be receiving data for a 
                     varying number of nodes, it's a pain to have to destroy 
                     and recreate dsets. The trouble is not one of allocation 
                     but of multiple links and associated structures created
                     for each new dset. So, while the juice is only up to 
                     vec_filled, the allocation is for vec_len 
         NodeDef: A vector containing an explicit list of the node index
                  associated with each row of data.
                  ACTUALLY this is not a field of nel, but it is a column of 
                  data of the type SUMA_NODE_INDEX or "Node_Index". This
                  column may or may not exist. If it exist then NodeDef is
                  (int *)nel->vec[i_node_index] where i_node_index is the
                  index of the column containing node definitions. To 
                  find i_node_index, see function:
                     SUMA_GetColIndex
         
         Functions to read and write dsets:
            SUMA_LoadDset
            SUMA_Load1DDset
            SUMA_LoadNimlDset
            SUMA_WriteDset
            SUMA_RemoveDsetExtension
         Functions to form/access dsets and contents:
            SUMA_NewNel
            SUMA_AddNelCol
            SUMA_FillNelCol
            SUMA_GetColIndex
            SUMA_Col2Float
            SUMA_GetNodeDef
            SUMA_FreeDset
         Functions for debugging:   
            SUMA_ShowNel
            SUMA_NI_nel_Info
            SUMA_DsetInfo
            SUMA_ShowMeSome
         Miscellaneous functions/tools:
            SUMA_Dset_Format 
            SUMA_Dset_Format_Name
            SUMA_AddNelHist
                    
         */              
   #else   /* the post april 06 05 way */
   /* *** You can go crazy below */
   NI_group *ngr; /*!< This is now April 06 05, the container of the dataset, as opposed to the olde days where nel
                        contained everything. The reason that was done is to accomodate large sized attibutes that
                        do not fit nicely in ASCII forms tucked inside the header. 
                        What used to be called nel, is now called dnel (for data-part nel) and is nothing but a copy
                        of the pointer under ngr to the nel that contains the tabular dataset. 
                        
      ngr contains two types of attributes: 
         STRING attributes:
            filename: The filename
            label: A short text label identifying the data set.
                   Typically, a short version of the filename                          
            idcode_str: Unique identifier for the data set
            MeshParent_idcode: Unique identifier of the surface containing the mesh  
                               over which this set is defined
            geometry_parent_idcode: Unique identifier of the surface containing the 
                               coordinates of the nodes whose attributes 
                               (values) are in this set.
            sorted_node_def: flag indicating that nodes in NodeDef are sorted
                             see NodeDef below.

         ELEMENT (data) attributes
            ColumnRange
            ColumnType
            ColumnLabel
            ColumnAttribute
            History
         
         Sample Code: SUMA_TestDSETIO.c
         */    
   NI_element *dnel; /*!< a copy of the NI_element pointer that contains the tabular data inside ngr. Do not free this
                           element separately, and make sure its value is changed in syncrony with the one in ngr. */
   NI_element *inel; /*!< a copy of the NI_element pointer that contains the node index column inside nrg. Do not free this
                           element separately, and make sure its value is changed in syncrony with the one in ngr. */                           
   #endif     
} SUMA_DSET;

#define SUMA_COUNTER_SUFFIX(ic)  ( ((ic) == 1) ? "st" : ((ic) == 2) ? "nd" : ((ic) == 3) ? "rd" : "th" )
#define SUMA_COUNTER_PLURAL(ic)  ( ((ic) == 1) ? "" : "s" )

#define SUMA_SKIP_COMMON_OPTIONS(m_brk, m_kar) {\
   if (!m_brk &&                                     \
       ( (strcmp(argv[m_kar], "-memdbg") == 0) ||    \
         (strcmp(argv[m_kar], "-iodbg") == 0)  ||    \
         (strcmp(argv[m_kar], "-nomall") == 0) ||    \
         (strcmp(argv[m_kar], "-yesmall") == 0) ||   \
         (strcmp(argv[m_kar], "-trace") == 0) ||     \
         (strcmp(argv[m_kar], "-novolreg") == 0) ||   \
         (strcmp(argv[m_kar], "-noxform") == 0) ||   \
         (strcmp(argv[m_kar], "-TRACE") == 0)) ) {   \
		/* valid options, but already taken care of */  \
		m_brk = YUP;                                   \
	}                                               \
}

/*!
   set a to 1 if vector values are sorted in increasing order
            0 if not
*/
#define SUMA_IS_SORTED_UP(v, n_v, a) {\
   int m_i, m_nv = n_v-1; \
   if (v) { \
      a = 1;   \
      for (m_i =0; m_i <m_nv; ++m_i) {  \
         if (v[m_i] > v[m_i+1]) { a = 0; break; }  \
      }  \
   } else {\
      SUMA_S_Warn("NULL vector in SUMA_IS_SORTED_UP\nReturning 0 for ans.\n");   \
      a = 0;   \
   }\
}/*!
   set a to 1 if vector values are sorted in decreasing order
            0 if not
*/
#define SUMA_IS_SORTED_DOWN(v, n_v, a) {\
   int m_i, m_nv = n_v-1; \
   if (v) { \
      a = 1;   \
      for (m_i =0; m_i <m_nv; ++m_i) {  \
         if (v[m_i] < v[m_i+1]) { a = 0; break; }  \
      }  \
   } else {\
      SUMA_S_Warn("NULL vector in SUMA_IS_SORTED_DOWN\nReturning 0 for ans.\n");   \
      a = 0;   \
   }\
}
      
/*!
   Convenience function for SUMA_StringAppend cleanup
*/
#define SUMA_SS2S(SS, stmp)  {\
   if (SS)  {  \
      SS = SUMA_StringAppend(SS, NULL);   \
      stmp = SS->s;  \
      SUMA_free(SS); SS = NULL;   } \
}
/*!
   Frees so, if not NULL
   copies sn into so, takes care of so's allocation
   Does not free sn
*/
#define SUMA_STRING_REPLACE(so, sn) {  \
   if (so) SUMA_free(so);  \
   so = SUMA_copy_string(sn); \
}

#define SUMA_TO_LOWER_C(c) ( (c >= 'A' && c <= 'Z') ? (c + 'a' - 'A'): c )

#define SUMA_TO_LOWER(s) { \
   int m_i, m_d; \
   if (s) { \
      m_d = 'a' - 'A';  \
      for (m_i=0; m_i < strlen(s); ++m_i) { \
         if (s[m_i] >= 'A' && s[m_i] <= 'Z') s[m_i] = s[m_i] + m_d;  \
      }   \
   }  \
}  

#define SUMA_TO_UPPER_C(c) ( (c >= 'a' && c <= 'z') ? (c - 'a' + 'A'): c )

#define SUMA_TO_UPPER(s) { \
   int m_i, m_d; \
   if (s) { \
      m_d = 'a' - 'A';  \
      for (m_i=0; m_i < strlen(s); ++m_i) { \
         if (s[m_i] >= 'a' && s[m_i] <= 'z') s[m_i] = s[m_i] - m_d;  \
      }   \
   }  \
}  



/*!
   Is this attribute string empty ?
*/
#define SUMA_IS_EMPTY_STR_ATTR(str)  ( (!(str) || !strcmp((str),SUMA_EMPTY_ATTR)) ? 1 : 0 )


/*!
   \brief Macros to access dataset elements 
   Almost all of them involve a function call
   so don't use them in loops where the returned
   value is not expected to change
*/
#ifdef OLD_DSET
   #define SDSET_FILENAME(dset) NI_get_attribute(dset->nel,"filename")
   #define SDSET_LABEL(dset) NI_get_attribute(dset->nel,"label")
   #define SDSET_ID(dset) SUMA_sdset_id(dset) 
   #define SDSET_IDGDOM(dset) NI_get_attribute(dset->nel,"geometry_parent_idcode") 
   #define SDSET_IDMDOM(dset) SUMA_sdset_idmdom(dset)
   #define SDSET_SORTED(dset) NI_get_attribute(dset->nel,"sorted_node_def") 
   #define SDSET_TYPE_NAME(dset) dset->nel->name
   #define SDSET_TYPE(dset) SUMA_Dset_Type(dset->nel->name)
   #define SDSET_VECLEN(dset) dset->nel->vec_len
   #define SDSET_VECNUM(dset) dset->nel->vec_num
   #define SDSET_VECFILLED(dset) dset->nel->vec_filled
#else
   #define SDSET_FILENAME(dset) NI_get_attribute(dset->ngr,"filename")
   #define SDSET_LABEL(dset) NI_get_attribute(dset->ngr,"label")
   #define SDSET_ID(dset) SUMA_sdset_id(dset) 
   #define SDSET_IDGDOM(dset) NI_get_attribute(dset->ngr,"geometry_parent_idcode") 
   #define SDSET_IDMDOM(dset) SUMA_sdset_idmdom(dset)
   #define SDSET_SORTED(dset) ( (!dset || !dset->inel) ? NULL:NI_get_attribute(dset->inel,"sorted_node_def") )
   #define SDSET_IS_SORTED(dset) ( (!dset || !dset->inel || !NI_get_attribute(dset->inel,"sorted_node_def") || strcmp(NI_get_attribute(dset->inel,"sorted_node_def"), "Yes") != 0) ? 0 : 1 )
   #define SDSET_TYPE_NAME(dset) NI_get_attribute(dset->ngr,"dset_type")
   #define SDSET_TYPE(dset) SUMA_Dset_Type(NI_get_attribute(dset->ngr,"dset_type"))
   #define SDSET_VECLEN(dset) ( (!dset || !dset->dnel) ? -1:dset->dnel->vec_len)
   #define SDSET_NODEINDLEN(dset) dset->inel->vec_len
   #define SDSET_VECNUM(dset) dset->dnel->vec_num
   #define SDSET_VEC(dset,iii) dset->dnel->vec[iii]
   #define SDSET_NODEINDNUM(dset) dset->inel->vec_num
   #define SDSET_VECFILLED(dset) dset->dnel->vec_filled
   #define SDSET_NODEINDFILLED(dset) dset->inel->vec_filled
   #define SDSET_NODE_INDEX_COL(dset) ( (!dset || !dset->inel || !dset->inel->vec) ? NULL:(int*)(dset->inel->vec[0]) )
#endif
/*!
   \brief Macros to access commonly used colorplane parameters
   DO NOT USE COLP_NODEDEF macro inside a loop where the returned
   value is not to change because it involves a function call (SLOW)
*/
/* Pre March 29 04. 
#define COLP_NODEDEF(cop) SUMA_GetNodeDef(cop->dset_link)
#define COLP_N_NODEDEF(cop) cop->dset_link->nel->vec_filled
*/
/* Post March 29 04. You can't go frugal and use dset's fields
NodeDef might be dynamically changed in the overlay plane */
#define COLP_NODEDEF(cop) cop->NodeDef
#define COLP_N_NODEDEF(cop) cop->N_NodeDef
#ifdef OLD_DSET
   #define COLP_N_ALLOC(cop) cop->dset_link->nel->vec_len
#else
   #define COLP_N_ALLOC(cop) cop->dset_link->dnel->vec_len
#endif
/* #define DSET_(dset) NI_get_attribute(dset->nel,"") */
   
static byte NI_GOT;

#define NI_SET_STR(ngr, name, val)  {\
   if (val && val[0] != '\0') NI_set_attribute(ngr, name, val);  \
   else NI_set_attribute(ngr, name, SUMA_EMPTY_ATTR); \
}
#define NI_GET_STR(ngr, name, val)  {\
   char *m_s = NI_get_attribute(ngr, name);  \
   if (m_s) {  \
      NI_GOT = 1; \
      if (strcmp(m_s,SUMA_EMPTY_ATTR) == 0) val[0] = '\0'; else sprintf(val,"%s", m_s); \
   }  else {   \
      NI_GOT = 0; \
      val[0] = '\0'; \
   }  \
}

#define NI_GET_STR_CP(ngr, name, val)  {\
   char *m_s = NI_get_attribute(ngr, name);  \
   if (m_s) {  \
      NI_GOT = 1; \
      if (strcmp(m_s,SUMA_EMPTY_ATTR) == 0) val = NULL; else val = SUMA_copy_string(m_s); \
   } else { \
      NI_GOT = 0; val = NULL; \
   }  \
}

#define NI_SET_INT(ngr, name, val)  {\
   char m_stmp[100]; sprintf(m_stmp,"%d", val);   \
   NI_set_attribute(ngr, name, m_stmp);  \
}
#define NI_GET_INT(ngr, name, val)  {\
   char *m_s = NI_get_attribute(ngr, name);  \
   if (m_s) { NI_GOT = 1; val = atoi(m_s); } else { NI_GOT = 0; val = 0; }\
}
#define NI_SET_INTv(ngr, name, valv, n) {\
   char m_stmp[400]; int m_i=0, m_s=0; m_stmp[0] = '\0';\
   for (m_i=0; m_i<n && m_s < 350; ++m_i) { \
      sprintf(m_stmp+m_s, " %d", valv[m_i]);   \
      m_s = strlen(m_stmp);  \
      if (m_s >= 350) { SUMA_S_Warn("Too long a vector, might get truncated"); }\
   }\
   NI_set_attribute(ngr, name, m_stmp);  \
}

#define NI_GET_INTv(ngr, name, valv, n, verb) {\
   char *m_s = NI_get_attribute(ngr, name);  \
   int m_nr, m_i; int *m_iv;  \
   for (m_i=0; m_i<n; ++m_i) valv[m_i] = 0.0;   \
   if (m_s) {  \
      NI_GOT = 1; \
      m_iv = (int *)SUMA_strtol_vec(m_s, n, &m_nr, SUMA_int, NULL); \
      if (m_iv) {\
         if (!verb) { \
            if (m_nr < n) { \
               SUMA_S_Warn("Fewer values in field\nProceeding..."); }  \
            else  if (m_nr > n) { \
               SUMA_S_Warn("More values in field\nProceeding..."); }  \
         }  \
         for (m_i=0; m_i<SUMA_MIN_PAIR(n, m_nr);++m_i) valv[m_i] = m_iv[m_i];    \
         SUMA_free(m_iv);  \
      } else {    \
         NI_GOT = 1; \
         if (verb) SUMA_S_Warn("NULL vec, filling with zeros"); \
      }  \
   } else { NI_GOT = 0; }  \
}

#define NI_SET_FLOAT(ngr, name, val)  {\
   char m_stmp[100]; sprintf(m_stmp,"%f", val);   \
   NI_set_attribute(ngr, name, m_stmp);  \
}
#define NI_GET_FLOAT(ngr, name, val)  {\
   char *m_s = NI_get_attribute(ngr, name);  \
   if (m_s) { NI_GOT = 1; val = atof(m_s); } else { NI_GOT = 0; val = 0.0; }\
}

#define NI_SET_FLOATv(ngr, name, valv, n) {\
   char m_stmp[400]; int m_i=0, m_s=0;  m_stmp[0] = '\0';\
   for (m_i=0; m_i<n && m_s < 350; ++m_i) { \
      sprintf(m_stmp+m_s, " %f", valv[m_i]);   \
      m_s = strlen(m_stmp);  \
      if (m_s >= 350) { SUMA_S_Warn("Too long a vector, might get truncated"); }\
   }\
   NI_set_attribute(ngr, name, m_stmp);  \
}

#define NI_GET_FLOATv(ngr, name, valv, n, verb) {\
   char *m_s = NI_get_attribute(ngr, name);  \
   int m_nr, m_i; float *m_fv;  \
   for (m_i=0; m_i<n; ++m_i) valv[m_i] = 0.0;   \
   if (m_s) {  \
      NI_GOT = 1; \
      m_fv = (float *)SUMA_strtol_vec(m_s, n, &m_nr, SUMA_float, NULL); \
      if (m_fv) {\
         if (verb) {\
            if (m_nr < n) { \
               SUMA_S_Warn("Fewer values in field\nProceeding..."); }  \
            else if (m_nr > n) { \
               SUMA_S_Warn("More values in field\nProceeding..."); }  \
         }  \
         for (m_i=0; m_i<SUMA_MIN_PAIR(n, m_nr);++m_i) \
            valv[m_i] = m_fv[m_i];    \
         SUMA_free(m_fv);  \
      } else {    \
         NI_GOT = 1; \
         if (verb) SUMA_S_Warn("NULL vec, filling with zeros"); \
      }  \
   } else { NI_GOT = 0; }  \
}

#define NI_SET_PTR(ngr, name, val) {   \
   char m_stmp[100]; sprintf(m_stmp,"%p",val);  \
   NI_set_attribute(ngr, name, m_stmp);  \
}

#define NI_GET_PTR(ngr, name, val) {   \
   char *m_s = NI_get_attribute(ngr, name);    \
   if (m_s) { NI_GOT = 1; sscanf(m_s,"%p", &val);  }\
}

#define NI_IS_STR_ATTR_EQUAL(ngr, name, stmp) ( (!name || !NI_get_attribute(ngr,name) || !stmp || strcmp(NI_get_attribute(ngr,name), stmp) ) ? 0:1 )
#define NI_IS_STR_ATTR_EMPTY(ngr, name) ( (!name || !NI_get_attribute(ngr,name) || !strlen(NI_get_attribute(ngr,name))  ) ? 0:1 )

#define NI_YES_ATTR(ngr, name) ( \
   (  !name || \
      !NI_get_attribute(ngr,name) ||   \
      strncmp(SUMA_to_lower(NI_get_attribute(ngr,name)), "y",1) )   \
      ? 0:1 )
#define NI_NO_ATTR(ngr, name) ( (!name || !NI_get_attribute(ngr,name) || strncmp(SUMA_to_lower(NI_get_attribute(ngr,name)), "n",1) ) ? 0:1 )

/*!
   NEL_READ macro for reading a NI element from strm
   nel (NI_element *) to contain the deed (if null then read failed)
   frm the source such as: "file:Test_niml_file"
*/
#define NEL_READ(nel, frm) { \
   NI_stream m_ns = NULL;  \
   int m_tt = NI_element_type((void*)nel) ; \
   if (m_tt == NI_GROUP_TYPE) {  SUMA_SL_Err ("Group, use DSET_READ"); }/* SHOULD USE DSET_WRITE_1D */   \
   else {   \
      nel = NULL; \
      m_ns = NI_stream_open( frm , "r" ) ;   \
      if( m_ns == NULL ) {    \
         SUMA_SL_Err ("Failed to open stream");  \
      } else { \
         /* read the element */   \
         if (!(nel = NI_read_element( m_ns , 1 )))  { \
            SUMA_SL_Err ("Failed to read element");  \
         }  \
      }  \
      /* close the stream */  \
      NI_stream_close( m_ns ) ; \
   }  \
}

#define DSET_READ(dset, frm) { \
   NI_stream m_ns = NULL;  \
   if (dset->ngr || dset->dnel) {   SUMA_SL_Err("dset elements not empty!\nNeed a clean dset"); }  \
   else {   \
      m_ns = NI_stream_open( frm , "r" ) ;   \
      if( m_ns == NULL ) {    \
         SUMA_SL_Err ("Failed to open stream");  \
      } else { \
         /* read the element */   \
         if (!(dset->ngr = NI_read_element( m_ns , 1 )))  { \
            SUMA_SL_Err ("Failed to read element");  \
         } else { \
            /* Look for the _data element */ \
            if (!(dset->dnel = SUMA_FindDsetDataElement(dset))) {  \
               SUMA_SL_Err("Cannot find data element!\nCleaning up.\n");   \
               NI_free_element (dset->ngr); dset->ngr = NULL;  \
            }  \
            dset->inel =  SUMA_FindDsetNodeIndexElement(dset); \
         }\
      }  \
      /* close the stream */  \
      NI_stream_close( m_ns ) ; \
   }  \
}

/*! Write an array to a text file, mcol consecutive values per line. 
v is the array
Nel is the total number of values
m is the number of consecutive values to write per line 
If you want to have some index before the entries, use SUMA_WRITE_IND_ARRAY_1D*/
#define SUMA_WRITE_ARRAY_1D(v,Nel,m,iname){  \
   int m_kkk; \
   char *name=(char*)iname;   \
   FILE * m_fp=NULL;\
   m_fp = (name) ? fopen((name),"w"): fopen("yougavemenoname","w");  \
   if (m_fp) { \
      fprintf(m_fp,"# Output from %s, %d values (%d per line).", FuncName, Nel, m);  \
      for (m_kkk=0; m_kkk<Nel; ++m_kkk) { if (!(m_kkk % m)) fprintf(m_fp,"\n"); fprintf(m_fp,"%f   ", (double)v[m_kkk]); }\
      fclose(m_fp); \
   }  \
}
#define SUMA_WRITE_INT_ARRAY_1D(v,Nel,m,name){  \
   int m_kkk; \
   FILE * m_fp=NULL;\
   m_fp = (name) ? fopen((name),"w"): fopen("yougavemenoname","w");  \
   if (m_fp) { \
      fprintf(m_fp,"# Output from %s, %d values (%d per line).", FuncName, Nel, m);  \
      for (m_kkk=0; m_kkk<Nel; ++m_kkk) { if (!(m_kkk % m)) fprintf(m_fp,"\n"); fprintf(m_fp,"%d   ", (int)v[m_kkk]); }\
      fclose(m_fp); \
   }  \
}
/* Just like SUMA_WRITE_ARRAY_1D but ind contains indices
to add at the beginning of each line.
If ind is NULL, then the index will be the line number.
*/
#define SUMA_WRITE_IND_ARRAY_1D(v,m_ind,Nel,m,name){  \
   int m_kkk, *ind = (int *)m_ind;  \
   FILE * m_fp = (name) ? fopen((name),"w"): fopen("yougavemenoidly","w");  \
   if (m_fp) { \
      fprintf(m_fp,  "# Output from %s, index followed by %d values "\
                     "(%d per line).\n", FuncName, Nel, 1);  \
      if (!ind) {  \
         for (m_kkk=0; m_kkk<Nel; ++m_kkk) { \
            if (!(m_kkk % m)) fprintf(m_fp,"\n%d   ", m_kkk/m); \
            fprintf(m_fp,"%f   ", (double)v[m_kkk]); }\
      } else {\
         for (m_kkk=0; m_kkk<Nel; ++m_kkk) { \
            if (!(m_kkk % m)) fprintf(m_fp,"\n%d   ", ind[m_kkk/m]); \
            fprintf(m_fp,"%f   ", (double)v[m_kkk]); }\
      }  \
      fclose(m_fp); \
   }  \
}

/*!
   NEL_WRITE_TX(nel, strm, suc)
   NEL_WRITE_BI(nel, strm, suc)
   NEL_WRITE_1D(nel, strm, suc)
   NEL_WRITE_1D_PURE(nel, strm, suc)
   macros for writing a NI element in  NI_TEXT_MODE, NI_BINARY_MODE  or
                                       NI_TEXT_MODE | NI_HEADERSHARP_FLAG which is a la 1D
   nel is the NI element
   frm is someting like:  "file:Test_write_asc_1D" (for a file output)
                           "fd:1" (for stdout) 
                           or "stderr:" or "stdout:"
   suc is a flag for success (1), failure (0)
*/
#define NEL_WRITE_TX(nel, frm, suc) NEL_WRITE_TX_ENG(nel, frm, suc, NI_TEXT_MODE)
#define NEL_WRITE_TXH(nel, frm, suc) NEL_WRITE_TX_ENG(nel, frm, suc, (NI_TEXT_MODE | NI_HEADERSHARP_FLAG))

#define NEL_WRITE_TX_ENG(nel, frm, suc, form) { \
   NI_stream m_ns = NULL;  \
   suc = 1; \
   m_ns = NI_stream_open( frm , "w" ) ;   \
   if( m_ns == NULL ) {    \
      SUMA_S_Err ("Failed to open stream");  \
      suc = 0; \
   } else { \
      /* write out the element */   \
      if (NI_write_element( m_ns , nel , form ) < 0) { \
         SUMA_S_Err ("Failed to write element");  \
         suc = 0; \
      }  \
   }  \
   /* close the stream */  \
   NI_stream_close( m_ns ) ; \
}



#define DSET_WRITE_1D(dset, frm, suc, addindex) { \
   NI_stream m_ns = NULL;  \
   int m_allnum;  \
   suc = 1; \
   m_allnum = SUMA_is_AllNumeric_dset(dset);   \
   if (!m_allnum) { \
      SUMA_SL_Err ("Element cannont be written to 1D format");    \
      suc = 0; \
   } else {   \
      m_ns = NI_stream_open( frm , "w" ) ;   \
      if( m_ns == NULL ) {    \
         SUMA_SL_Err ("Failed to open stream");  \
         suc = 0; \
      } else { \
         /* write out the element */   \
         if (addindex) { \
            if (!dset->inel) { SUMA_SL_Err ("No inel in dset! No node indices written!\n"); addindex = 0;}   \
            else { NI_insert_column(dset->dnel, dset->inel->vec_typ[0], dset->inel->vec[0], 0);  } \
         }  \
         if (NI_write_element( m_ns , dset->dnel , NI_TEXT_MODE | NI_HEADERSHARP_FLAG) < 0) { \
            SUMA_SL_Err ("Failed to write element");  \
            suc = 0; \
         }  \
         if (addindex) { NI_remove_column(dset->dnel, 0); } \
      }  \
      /* close the stream */  \
      NI_stream_close( m_ns ) ; \
   }  \
}
#define DSET_WRITE_1D_PURE(dset, frm, suc, addindex) { \
   FILE *m_fid = NULL;  \
   int m_ind, m_ival;   \
   int m_allnum;  \
   suc = 1; \
   m_allnum = SUMA_is_AllNumeric_dset(dset);   \
   if (!m_allnum) { \
      SUMA_SL_Err ("Element cannont be written to 1D format");    \
      suc = 0; \
   } else {   \
      if (!strcmp(frm,"stdout")) m_fid = stdout;   \
      else if (!strcmp(frm,"stderr")) m_fid = stderr;   \
      else m_fid = fopen(frm,"w"); \
      if( m_fid == NULL ) {    \
         SUMA_SL_Err ("Failed to open file for output");  \
         suc = 0; \
      } else { \
         if (addindex) { \
            if (!dset->inel) { SUMA_SL_Err ("No inel in dset! No node indices written!\n"); addindex = 0;}   \
         }  \
         if (!addindex) {  \
            for (m_ival=0; m_ival<dset->dnel->vec_len; ++m_ival) { \
               for (m_ind=0; m_ind<dset->dnel->vec_num; ++m_ind) { \
                  fprintf(m_fid,"%f   ", SUMA_GetDsetValInCol2(dset, m_ind, m_ival));  \
               }  \
               fprintf(m_fid,"\n"); \
            }  \
         } else { \
            int *m_n=(int *)dset->inel->vec[0];  \
            for (m_ival=0; m_ival<dset->dnel->vec_len; ++m_ival) { \
               fprintf(m_fid,"%d   ", m_n[m_ival]); \
               for (m_ind=0; m_ind<dset->dnel->vec_num; ++m_ind) { \
                  fprintf(m_fid,"%f   ", SUMA_GetDsetValInCol2(dset, m_ind, m_ival));  \
               }  \
               fprintf(m_fid,"\n"); \
            }  \
         }  \
         fclose(m_fid); m_fid = NULL;  \
      }  \
   }\
}

#define DSET_WRITE_1D_PURE_TRANSPOSE(dset, frm, suc, addindex) { \
   FILE *m_fid = NULL;  \
   int m_ind, m_ival;   \
   int m_allnum;  \
   suc = 1; \
   m_allnum = SUMA_is_AllNumeric_dset(dset);   \
   if (!m_allnum) { \
      SUMA_SL_Err ("Element cannont be written to 1D format");    \
      suc = 0; \
   } else {   \
      if (!strcmp(frm,"stdout")) m_fid = stdout;   \
      else if (!strcmp(frm,"stderr")) m_fid = stderr;   \
      else m_fid = fopen(frm,"w"); \
      if( m_fid == NULL ) {    \
         SUMA_SL_Err ("Failed to open file for output");  \
         suc = 0; \
      } else { \
         if (addindex) { \
            if (!dset->inel) { SUMA_SL_Err ("No inel in dset! No node indices written!\n"); addindex = 0;}   \
         }  \
         if (!addindex) {  \
            for (m_ind=0; m_ind<dset->dnel->vec_num; ++m_ind) { \
               for (m_ival=0; m_ival<dset->dnel->vec_len; ++m_ival) { \
                  fprintf(m_fid,"%f   ", SUMA_GetDsetValInCol2(dset, m_ind, m_ival));  \
               }  \
               fprintf(m_fid,"\n"); \
            }  \
         } else { \
            int *m_n=(int *)dset->inel->vec[0];  \
            for (m_ind=0; m_ind<dset->dnel->vec_num; ++m_ind) { \
               for (m_ival=0; m_ival<dset->dnel->vec_len; ++m_ival) { \
               fprintf(m_fid,"%d   ", m_n[m_ival]); \
                  fprintf(m_fid,"%f   ", \
                           SUMA_GetDsetValInCol2(dset, m_ind, m_ival));  \
               }  \
               fprintf(m_fid,"\n"); \
            }  \
         }  \
         fclose(m_fid); m_fid = NULL;  \
      }  \
   }\
}

#define NEL_WRITE_1D(nel, frm, suc) { \
   NI_stream m_ns = NULL;  \
   int m_tt = NI_element_type((void*)nel) ; \
   int m_allnum;  \
   suc = 1; \
   if (m_tt == NI_GROUP_TYPE) { \
      m_allnum = 0; \
      SUMA_SL_Err ("Group, use DSET_WRITE_1D_PURE"); }/* USE DSET_WRITE_1D */   \
   else m_allnum = SUMA_is_AllNumeric_nel(nel);   \
   if (!m_allnum) { \
      SUMA_SL_Err ("Element cannont be written to 1D format");    \
      suc = 0; \
   } else {   \
      m_ns = NI_stream_open( frm , "w" ) ;   \
      if( m_ns == NULL ) {    \
         SUMA_SL_Err ("Failed to open stream");  \
         suc = 0; \
      } else { \
         /* write out the element */   \
         if (NI_write_element( m_ns , nel , \
                               NI_TEXT_MODE | NI_HEADERSHARP_FLAG) < 0) { \
            SUMA_SL_Err ("Failed to write element");  \
            suc = 0; \
         }  \
      }  \
      /* close the stream */  \
      NI_stream_close( m_ns ) ; \
   }  \
}

/*!
   NEL_WRITE* macros are left for the record, they should not be used 
*/
#define NEL_WRITE_1D_PURE(nel, frm, suc) { \
   FILE *m_fid = NULL;  \
   int m_ind, m_ival;   \
   int m_tt = NI_element_type((void*)nel) ; \
   int m_allnum;  \
   suc = 1; \
   if (m_tt == NI_GROUP_TYPE) { \
      m_allnum = 0;  \
      SUMA_SL_Err ("Group, use DSET_WRITE_1D_PURE"); } /* USE DSET_WRITE_1D */  \
   else m_allnum = SUMA_is_AllNumeric_nel(nel);   \
   if (!m_allnum) { \
      SUMA_SL_Err ("Element cannont be written to 1D format");    \
      suc = 0; \
   } else {   \
      m_fid = fopen(frm,"w"); \
      if( m_fid == NULL ) {    \
         SUMA_SL_Err ("Failed to open file for output");  \
         suc = 0; \
      } else { \
         for (m_ival=0; m_ival<nel->vec_len; ++m_ival) { \
            for (m_ind=0; m_ind<nel->vec_num; ++m_ind) { \
               fprintf(m_fid,"%f   ", SUMA_GetValInCol2(nel, m_ind, m_ival));  \
            }  \
            fprintf(m_fid,"\n"); \
         }  \
         fclose(m_fid); m_fid = NULL;  \
      }  \
   }\
}

#define NEL_WRITE_BI(nel, frm, suc) { \
   NI_stream m_ns = NULL;  \
   suc = 1; \
   m_ns = NI_stream_open( frm , "w" ) ;   \
   if( m_ns == NULL ) {    \
      SUMA_SL_Err ("Failed to open stream");  \
      suc = 0; \
   } else { \
      /* write out the element */   \
      if (NI_write_element( m_ns , nel , NI_BINARY_MODE) < 0) { \
         SUMA_SL_Err ("Failed to write element");  \
         suc = 0; \
      }  \
   }  \
   /* close the stream */  \
   NI_stream_close( m_ns ) ; \
}

/*!
   get a string positioned in column col, row row in NI_element * nel.
   str is a copy of the pointer to that string and must not be freed
*/
#define SUMA_NEL_GET_STRING(nel, row, col, str) {\
   char **m_rc;   \
   m_rc = (char **)(nel)->vec[(col)]; \
   str = m_rc[(row)];\
}
/*!
   replace a string positioned in column col, row row in NI_element * nel.
   str is a copy of the pointer to that string and must not be freed
*/
#define SUMA_NEL_REPLACE_STRING(nel, row, col, str) {\
   char **m_rc;   \
   m_rc = (char **)(nel)->vec[(col)]; \
   if (m_rc[(row)]) NI_free(m_rc[(row)]); \
   m_rc[(row)] = NULL;\
   if (str) { \
      m_rc[(row)] = (char*)NI_malloc(char, (strlen((str))+1)*sizeof(char));\
      strcpy( m_rc[(row)], str );   \
   }  \
}

/*!
   \brief A macro to be run from main() before writing a dset.
   Changes a dset's ID, label (using prefix) and history
*/
#define SUMA_NEWDSET_ID_LABEL_HIST(dset, prefix) {\
   if (dset) { \
      if (!SUMA_NewDsetID (dset))  { SUMA_SL_Err("Failed in SUMA_NewDsetID, proceeding..."); }  \
      if (!SUMA_LabelDset(dset, prefix)) { SUMA_SL_Err("Failed in SUMA_LabelDset, proceeding..."); }  \
      if (!SUMA_AddNgrHist (dset->ngr, FuncName, argc, argv)) { SUMA_SL_Err("Failed in SUMA_AddNgrHist, proceeding..."); } \
   } else {\
      SUMA_SL_Err("NULL dset");  \
   }  \
}

/*! A macro to transport common attributes when 
    copying columns from one dset to another.
    Do not include in ATR_LIST here, any of
    HISTORY_NOTE (this should be done by appending old history separately)
    COLMS_LABELS
    COLMS_TYPES   
    COLMS_RANGE as these are handled at the moment of column creation
*/
#define SUMA_COPY_DSET_COL_ATTRIBUTES(odset, ndset, io, in) {   \
   char *m_ATR_LIST[64] = { \
      "COLMS_STATSYM", "FDRCURVE",  \
       NULL }; \
   if (!SUMA_CopyDsetAttributes (odset, ndset, m_ATR_LIST, io, in)) {   \
      SUMA_S_Err("Failed to copy dset attributes");   \
   }  \
}
#define SUMA_COPY_DSETWIDE_ATTRIBUTES(odset, ndset) {   \
   char *m_ATR_LIST[64] = { \
      "TR",  \
       NULL }; \
   if (!SUMA_CopyDsetAttributes (odset, ndset, m_ATR_LIST, -1, -1)) {   \
      SUMA_S_Err("Failed to copy dset attributes");   \
   }  \
}

#define SUMA_MAX_OPEN_DX_FIELD_COMPONENTS 500
#define SUMA_MAX_OPEN_DX_FIELD_ATTRIBUTES 500
#define SUMA_MAX_OPEN_DX_OBJECTS  500

typedef struct {
   int rank;
   int shape;
   int items;
   int bad_data;
   char *type;
   char *object;
   char *class;
   char *data;
   char *data_off;
   int data_format;
   void *datap;
   int n_comp;
   char *comp_name[SUMA_MAX_OPEN_DX_FIELD_COMPONENTS];
   char *comp_value[SUMA_MAX_OPEN_DX_FIELD_COMPONENTS];
   int n_attr;
   char *attr_name[SUMA_MAX_OPEN_DX_FIELD_ATTRIBUTES];
   char *attr_string[SUMA_MAX_OPEN_DX_FIELD_ATTRIBUTES];
   int *counts;
   int n_counts;
   float *delta;
   int n_delta;
   float *origin;
   int n_origin;
} SUMA_OPEN_DX_STRUCT;

#define SUMA_OK_OPENDX_DATA_TYPE(tp) ( (  tp == SUMA_int || \
                                          tp == SUMA_float ||  \
                                          tp == SUMA_double || \
                                          tp == SUMA_byte )   \
                                           ? 1 : 0 )

#define SUMA_NCOL_OPENDX(dx) ( ( ( (dx)->shape == 0 ) ? 1 : ((dx)->shape) ) )
char *SUMA_getcwd(void);
void SUMA_FreeErrLog ( void *data);
void SUMA_PushErrLog(char *macroname, char *msg, char *fname);
DListElmt* SUMA_PopErrLog(DListElmt *eldone);
void WorkErrLog_ns(void);

NI_element *SUMA_FindDsetDataElement(SUMA_DSET *dset);
NI_element *SUMA_FindDsetNodeIndexElement(SUMA_DSET *dset);
NI_element *SUMA_FindDsetAttributeElement(SUMA_DSET *dset, char *attname);
NI_element *SUMA_FindNgrAttributeElement(NI_group *ngr, char *attname);
NI_element *SUMA_FindNgrDataElement(NI_group *ngr, char *nelname, char *typename);
float SUMA_LatestVersionNumber(void);
char * SUMA_Dset_Type_Name (SUMA_DSET_TYPE tp);
SUMA_DSET_TYPE SUMA_Dset_Type (char *Name);
char * SUMA_Col_Type_Name (SUMA_COL_TYPE tp);
SUMA_COL_TYPE SUMA_Col_Type (char *Name);
char * SUMA_AttrOfDsetColNumb(SUMA_DSET *dset, int ind);
SUMA_COL_TYPE SUMA_TypeOfDsetColNumb(SUMA_DSET *dset, int ind);
SUMA_COL_TYPE SUMA_TypeOfColNumb(NI_element *nel, int ind) ;
SUMA_VARTYPE SUMA_ColType2TypeCast (SUMA_COL_TYPE ctp); 
SUMA_Boolean SUMA_isSameDsetColTypes(SUMA_DSET *dset1, SUMA_DSET *dset2); 
int SUMA_ShowNel (void *nel);
char *SUMA_NI_nel_Info (NI_element *nel, int detail);

void SUMA_allow_nel_use(int al);
int SUMA_AddDsetNelCol ( SUMA_DSET *dset, char *col_label, 
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride);
int SUMA_InsertDsetNelCol ( SUMA_DSET *dset, char *col_label, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride, int icol);
int SUMA_AddNelCol ( NI_element *nel, char *col_label,
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride);
int SUMA_AddDsetColAttr (SUMA_DSET *dset, char *col_label, 
                     SUMA_COL_TYPE ctp, void *col_attr, int col_index, int insert_mode);
int SUMA_AddDsetNodeIndexColAttr (SUMA_DSET *dset, char *col_label, 
                     SUMA_COL_TYPE ctp, void *col_attr );
int SUMA_AddColAttr (NI_element *nel, char *col_label,
                     SUMA_COL_TYPE ctp, void *col_attr, int col_index);
SUMA_Boolean SUMA_isMultiColumnAttr(NI_element *nel);
SUMA_Boolean SUMA_isSingleColumnAttr(NI_element *nel, int *icolb, char *rtname);
SUMA_Boolean SUMA_isDsetwideColumnAttr(NI_element *nel);
SUMA_Boolean SUMA_isDsetNelAttr(NI_element *nel);
char * SUMA_CreateDsetColRangeCompString( SUMA_DSET *dset, int col_index, 
                                          SUMA_COL_TYPE ctp);
char * SUMA_GetDsetColStringAttr( SUMA_DSET *dset, int col_index, 
                                    char *attrname);
char * SUMA_GetNgrColStringAttr( NI_group *ngr, int col_index, 
                                 char *attrname);
SUMA_Boolean SUMA_ParseAttrName(NI_element *nel, int *tp, 
                                 int *icol, char *rtname);
SUMA_Boolean SUMA_CopyDsetAttributes ( SUMA_DSET *src, SUMA_DSET *dest,
                                       char **attrlist, 
                                       int isrc, int idest );
SUMA_Boolean SUMA_NewDsetGrp (SUMA_DSET *dset, SUMA_DSET_TYPE dtp, 
                           char* MeshParent_idcode, 
                          char * geometry_parent_idcode, int N_el, 
                          char *filename, char *thisidcode);
NI_element * SUMA_NewNel (SUMA_DSET_TYPE dtp, char* MeshParent_idcode, 
                          char * geometry_parent_idcode, int N_el, 
                          char *name, char *thisidcode);
SUMA_DSET_FORMAT SUMA_Dset_Format (char *Name);
char * SUMA_Dset_Format_Name (SUMA_DSET_FORMAT fr);
char *SUMA_HistString (char *CallingFunc, int N_arg, char **arg, char *sold);
char * SUMA_GetNgrHist(NI_group *ngr);
int SUMA_AddNgrHist(NI_group *ngr, char *CallingFunc, int N_arg, char **arg);
int SUMA_AddNelHist(NI_element *nel, char *CallingFunc, int N_arg, char **arg);
void SUMA_FreeDset(void *dset);
SUMA_DSET * SUMA_FindDset_ns (char *idcode_str, DList *DsetList);
DListElmt * SUMA_FindDsetEl_ns (char *idcode, DList *DsetList);
SUMA_DSET * SUMA_FindDset_eng (char *idcode_str, DList *DsetList, DListElmt **elp);
char *SUMA_DsetInfo (SUMA_DSET *dset, int detail);
void SUMA_ShowDset (SUMA_DSET *dset, int detail, FILE *out);
char *SUMA_ShowMeSome (void *dt, SUMA_VARTYPE tp, int N_dt, int mxshow, char *title);
SUMA_DSET * SUMA_NewDsetPointer(void);
SUMA_DSET * SUMA_CreateDsetPointer (  
                              char *name, 
                              SUMA_DSET_TYPE tp,
                              char *idcode_str,
                              char *domain_idcode_str,
                              int N_Alloc); 
int SUMA_InsertDsetPointer (SUMA_DSET **dset, DList *DsetList, int replace);
int SUMA_DeleteDsetPointer (SUMA_DSET **dsetp, DList *DsetList);
void * SUMA_GetCx(char *idcode_str, DList *DsetList, int ReturnDsetPointer) ;
#if 0
SUMA_DSET *SUMA_LinkToDset(SUMA_DSET *dset);
SUMA_DSET *SUMA_UnlinkFromDset(SUMA_DSET *dset);
#endif
void *SUMA_LinkToPointer(void *ptr);
void *SUMA_UnlinkFromPointer(void *ptr);
int * SUMA_GetNodeDef(SUMA_DSET *dset);
int SUMA_GetNodeDefColIndex(SUMA_DSET *dset);
int SUMA_FillDsetNelCol (SUMA_DSET *dset, char *col_label,
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride); 
int SUMA_FillDsetNelNodeIndexCol (SUMA_DSET *dset, char *col_label, 
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride); 
SUMA_Boolean SUMA_PopulateDsetNodeIndexNel(SUMA_DSET *dset, int verb);
int SUMA_FillNelCol (NI_element *nel, char *col_label,
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride); 
int *SUMA_GetDsetColIndex (SUMA_DSET *dset, SUMA_COL_TYPE tp, int *N_i);
int *SUMA_GetColIndex (NI_element *nel, SUMA_COL_TYPE tp, int *N_i);
int SUMA_Float2DsetCol (SUMA_DSET *dset, int ind, float *V, int FilledOnly, byte *replacemask);
int SUMA_Vec2DsetCol (SUMA_DSET *dset, int ind, 
                        void *V, SUMA_VARTYPE Vtp,
                        int FilledOnly, 
                        byte *replacemask);
int * SUMA_DsetCol2Int (SUMA_DSET *dset, int ind, int FilledOnly);
float * SUMA_DsetCol2Float (SUMA_DSET *dset, int ind, int FilledOnly);
double * SUMA_DsetCol2Double (SUMA_DSET *dset, int ind, int FilledOnly);
float * SUMA_Col2Float (NI_element *nel, int ind, int FilledOnly);
SUMA_Boolean SUMA_SetUniqueValsAttr(SUMA_DSET *dset, int icol, byte replace);
NI_element * SUMA_GetUniqueValsAttr(SUMA_DSET *dset, int icol);
int SUMA_GetDsetColRange(SUMA_DSET *dset, int col_index, 
                         double range[2], int loc[2]);
int SUMA_GetDsetNodeIndexColRange(SUMA_DSET *dset, 
                                  double range[2], int loc[2], int addifmissing);
int SUMA_GetColRange(NI_element *nel, int col_index, 
                     double range[2], int loc[2]);
int SUMA_AddGenDsetColAttr (SUMA_DSET *dset, SUMA_COL_TYPE ctp, void *col, int stride, int col_index, int insert_mode);
int SUMA_AddGenDsetNodeIndexColAttr (SUMA_DSET *dset, SUMA_COL_TYPE ctp, void *col, int stride) ;
int SUMA_AddGenColAttr (NI_element *nel, SUMA_COL_TYPE ctp, void *col, int stride, int col_index); 
SUMA_DSET *SUMA_LoadNimlDset (char *Name, int verb);
SUMA_DSET *SUMA_LoadGIFTIDset (char *Name, int verb);
SUMA_DSET *SUMA_LoadDset_eng (char *Name, SUMA_DSET_FORMAT *form, int verb);
SUMA_DSET *SUMA_LoadDset_ns (char *Name, SUMA_DSET_FORMAT *form, int verb);
SUMA_DSET *SUMA_Load1DDset_eng (char *Name, int verb);
SUMA_DSET *SUMA_Load1DDset_ns (char *Name, int verb);
SUMA_DSET *SUMA_LoadDXDset_eng (char *Name, int verb);
SUMA_DSET *SUMA_LoadDXDset_ns (char *Name, int verb);
char *SUMA_RemoveDsetExtension_ns (char*Name, SUMA_DSET_FORMAT form);
char *SUMA_RemoveDsetExtension_eng (char*Name, SUMA_DSET_FORMAT form);
char * SUMA_WriteDset_ns (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, int overwrite, int verb); 
int SUMA_WriteDset_NameCheck_ns (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, int verb, char **NameOutp); 
int SUMA_WriteDset_NameCheck_eng (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, int verb, char **NameOutp); 
char * SUMA_WriteDset_eng (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, int overwrite, int verb); 
SUMA_DSET * SUMA_far2dset_eng( char *FullName, char *dset_id, char *dom_id, 
                                 float **farp, int vec_len, int vec_num, 
                                 int ptr_cpy);
SUMA_DSET * SUMA_far2dset_ns( char *FullName, char *dset_id, char *dom_id, 
                                 float **farp, int vec_len, int vec_num, 
                                 int ptr_cpy);
int SUMA_is_AllNumeric_dset(SUMA_DSET *dset);
int SUMA_is_Label_dset(SUMA_DSET *dset, NI_group **NIcmap); 
int * SUMA_UniqueValuesInLabelDset(SUMA_DSET *dset, int *N_unq);
int SUMA_is_AllConsistentNumeric_dset(SUMA_DSET *dset, SUMA_VARTYPE *vtpp);
int SUMA_is_AllNumeric_ngr(NI_group *ngr) ;
int SUMA_is_AllNumeric_nel(NI_element *nel);
int SUMA_is_TimeSeries_dset(SUMA_DSET *dset, double *TRp);
SUMA_Boolean SUMA_SetDsetTR(SUMA_DSET *dset, double TR);
SUMA_Boolean SUMA_NewDsetID (SUMA_DSET *dset);
SUMA_Boolean SUMA_NewDsetID2 (SUMA_DSET *dset, char *str);
char *SUMA_DsetColStringAttrCopy(SUMA_DSET *dset, int i, 
                                 int addcolnum, char *attrname);
char *SUMA_DsetColLabelCopy(SUMA_DSET *dset, int i, int addcolnum);
char *SUMA_ColLabelCopy(NI_element *nel, int i, int addcolnum);
SUMA_DSET * SUMA_PaddedCopyofDset ( SUMA_DSET *odset, int MaxNodeIndex );
SUMA_DSET * SUMA_MaskedCopyofDset(SUMA_DSET *odset, byte *rowmask, byte *colmask, int masked_only, int keep_node_index);
SUMA_DSET * SUMA_MaskedByOrderedNodeIndexCopyofDset(
      SUMA_DSET *odset, int *indexlist, 
      int N_indexlist, byte *colmask, 
      int masked_only, int keep_node_index);
SUMA_DSET * SUMA_MaskedByNodeIndexCopyofDset(SUMA_DSET *odset, int *indexlist, int N_indexlist, byte *colmask, 
                                             int masked_only, int keep_node_index);
void *SUMA_Copy_Part_Column(void *col,  NI_rowtype *rt, int N_col, byte *rowmask, int masked_only, int *n_incopy);
char* SUMA_sdset_id(SUMA_DSET *dset);
char* SUMA_sdset_idmdom(SUMA_DSET *dset);
NI_group *SUMA_oDsetNel2nDsetNgr(NI_element *nel); 
void SUMA_SetParent_DsetToLoad(char *parent);
float *SUMA_Load1D_eng (char *oName, int *ncol, int *nrow, int RowMajor, int verb);
double *SUMA_LoadDouble1D_eng (char *oName, int *ncol, int *nrow, int RowMajor, int verb);
complex *SUMA_LoadComplex1D_eng (char *oName, int *ncol, int *nrow, int RowMajor, int verb);
float *SUMA_Load1D_ns (char *oName, int *ncol, int *nrow, int RowMajor, int verb);
SUMA_OPEN_DX_STRUCT **SUMA_OpenDX_Read(char *fname, int *nobj);
void SUMA_Show_OpenDX_Struct(SUMA_OPEN_DX_STRUCT **dxv, int N_dxv, FILE *out);
SUMA_OPEN_DX_STRUCT *SUMA_Free_OpenDX_Struct(SUMA_OPEN_DX_STRUCT *dx);
SUMA_OPEN_DX_STRUCT *SUMA_Alloc_OpenDX_Struct(void);
void * SUMA_OpenDx_Object_Header_Field(char *op, int nchar, const char *attr, char **op_end);
SUMA_Boolean SUMA_OpenDx_Object_Data(char *op, int nchar, SUMA_OPEN_DX_STRUCT *dx);
SUMA_MX_VEC *SUMA_FreeMxVec(SUMA_MX_VEC *mxv);
SUMA_MX_VEC *SUMA_NewMxVec(SUMA_VARTYPE tp, int N_dims, int *dims, byte first_dim_first);
char *SUMA_MxVec_Info (SUMA_MX_VEC *mxv, int detail, char *title);
void SUMA_ShowMxVec (SUMA_MX_VEC *mxv, int detail, FILE *out, char *title);
int SUMA_MxVecInit(SUMA_MX_VEC *mxv, void *val);
int SUMA_NewMxAllocVec(SUMA_MX_VEC *mxv) ;
SUMA_MX_VEC *SUMA_NewMxNullVec(SUMA_VARTYPE tp, int N_dims, int *dims, byte first_dim_first);
SUMA_MX_VEC *SUMA_VecToMxVec(SUMA_VARTYPE tp, int N_dims, int *dims, byte first_dim_first, void *vec);
int * SUMA_FindNumericDataDsetCols(SUMA_DSET *dset, int *N_icols);
float * SUMA_DsetCol2FloatFullSortedColumn(  
            SUMA_DSET *dset, int ico, byte **nmaskp, float fillval,
            int N_Node, int *N_inmask, SUMA_Boolean MergeMask);
double * SUMA_DsetCol2DoubleFullSortedColumn(  
            SUMA_DSET *dset, int ico, byte **nmaskp, double fillval,
            int N_Node, int *N_inmask, SUMA_Boolean MergeMask);
SUMA_Boolean SUMA_MakeSparseColumnFullSorted(float **vp, int N_v, float mask_val, byte **bmp, SUMA_DSET *dset, int N_Node);
SUMA_Boolean SUMA_MakeSparseDoubleColumnFullSorted (
      double **vp, int N_v, 
      double mask_val, byte **bmp, 
      SUMA_DSET *dset, int N_Node);
SUMA_Boolean SUMA_AddNodeIndexColumn(SUMA_DSET *dset, int N_Node); 
int *SUMA_CreateNodeIndexToRowIndexMap(SUMA_DSET *dset, int maxind, 
                                       double *range);
SUMA_DSET * SUMA_ngr_2_dset(NI_group *nini, int warn);
SUMA_Boolean SUMA_LabelDset(SUMA_DSET *dset, char *lbl);
SUMA_Boolean SUMA_RenameDset(SUMA_DSET *dset, char *filename);
byte *SUMA_load_1D_n_mask(char *name, int N_Node, byte *omask, const char *oper, int *N_inmask);
byte * SUMA_indexlist_2_bytemask(int *ind_list, int N_ind_list, int N_mask, int *N_inmask);  
byte *SUMA_load_1D_b_mask(char *name, int N_Node, byte *omask, const char *oper, int *N_inmask);
byte *SUMA_get_c_mask(char *mask, int N_Node, byte *omask, const char *oper, int *N_inmask);
byte * SUMA_load_all_command_masks(char *bmaskname, char *nmaskname, char *cmask, int N_Node, int *N_inmask);
void SUMA_SetAddIndex_1D(int);
int SUMA_GetAddIndex_1D(void);
THD_3dim_dataset *SUMA_sumadset2afnidset(SUMA_DSET **dsetp, int copy_data, int cleardset);
SUMA_DSET *SUMA_afnidset2sumadset(THD_3dim_dataset **dsetp, int copy_data, int cleardset);
int SUMA_GetDsetColStatAttr(  SUMA_DSET *dset, int col_index, 
                              int *statcode,
                              float *p1, float *p2, float *p3);
float SUMA_fdrcurve_zval( SUMA_DSET *dset , int iv , float thresh );
NI_group *SUMA_NI_Cmap_of_Dset(SUMA_DSET *dset);


/*********************** BEGIN Miscellaneous support functions **************************** */
   #define SUMA_STANDALONE_INIT {   \
      /* install signal handler, shamelessly copied from AFNI) */ \
      signal(SIGINT ,SUMA_sigfunc) ;      \
      signal(SIGBUS ,SUMA_sigfunc) ;   \
      signal(SIGSEGV,SUMA_sigfunc) ;   \
      signal(SIGTERM,SUMA_sigfunc) ;   \
      SUMA_process_environ(); \
      SUMA_ParseInput_basics_ns (argv, argc);   \
   }
char * SUMA_to_lower(char *s) ;
int SUMA_filexists (char *f_name);
int SUMA_search_file(char **fnamep, char *epath);
char *SUMA_help_basics();
char *SUMA_help_cmap();
char *SUMA_help_talk();
char *SUMA_help_mask();
char *SUMA_help_dset();
int get_Domemtrace(void);
void set_Domemtrace(int s);
int get_Doiotrace(void);
void set_Doiotrace(int s) ;
int get_IgnoreXforms(void);
void setIgnoreXforms(int s) ;
void SUMA_process_environ(void);
void SUMA_ParseInput_basics_ns (char *argv[], int argc); 
int SUMA_ParseInput_basics_eng (char *argv[], int argc); 
void WorkErrLog_ns(void);
SUMA_FileName SUMA_StripPath (char *FileName);
SUMA_PARSED_NAME * SUMA_ParseFname (char *FileName, char *cwd);
char *SUMA_Extension(char *filename, char *ext, SUMA_Boolean Remove);
SUMA_DSET_FORMAT SUMA_GuessFormatFromExtension(char *Name, char *fallbackname);
const char *SUMA_ExtensionOfDsetFormat (SUMA_DSET_FORMAT form);
SUMA_Boolean SUMA_isExtension(char *filename, char *ext);
char * SUMA_CropExtension(char *filename, char *ext);
void *SUMA_Free_Parsed_Name(SUMA_PARSED_NAME *Test);
char *SUMA_FnameGet(char *Fname, char *sel, char *cwd);
int SUMA_NumStringUnits (char *s, int marktip); 
int SUMA_StringToNum (char *s, void *vv, int N, int p);
int SUMA_isNumString (char *s, void *p);
int SUMA_CleanNumString (char *s, void *p);
char *SUMA_copy_string(char *buf);
char * SUMA_append_string(char *s1, char *s2);
char * SUMA_append_replace_string(  char *s1, char *s2, 
                                    char *Spc, int whichTofree);
char * SUMA_append_replace_num(char *s1, char *form, double num, SUMA_VARTYPE tp, int whichTofree);
char * SUMA_truncate_string (char *s1, int length);
char *SUMA_set_string_length(char *buf, char cp, int n);
SUMA_STRING * SUMA_StringAppend (SUMA_STRING *SS, char *newstring);
SUMA_STRING * SUMA_StringAppend_va (SUMA_STRING *SS, char *newstring, ... );
void SUMA_sigfunc(int sig);
char * SUMA_pad_string(char *buf, char cp, int n, int add2end);
char * SUMA_GetDsetValInCol(SUMA_DSET *dset, int ind, int ival, double *dval) ;
char * SUMA_GetValInCol(NI_element *nel, int ind, int ival, double *dval); 
void **SUMA_Dset2VecArray(SUMA_DSET *dset, 
                        int *ind, int nind, 
                        int *node, int N_Node,
                        int iNodeMax,
                        int *N_ret,
                        SUMA_VARTYPE tp);
SUMA_DSET *SUMA_VecArray2Dset(void **resv,
                        SUMA_DSET *usethisdset, 
                        int *ind, int nind, 
                        int *node, int N_Node,
                        int iNodeMax,
                        SUMA_VARTYPE tp);
void * SUMA_GetDsetAllNodeValsInCols2(SUMA_DSET *dset, 
                                       int *ind, int nind, 
                                       int node, int N_Node,
                                       int *N_ret, 
                                       SUMA_VARTYPE tp);
double SUMA_GetDsetValInCol2(SUMA_DSET *dset, int ind, int ival) ;
double SUMA_GetValInCol2(NI_element *nel, int ind, int ival); 
int SUMA_GetNodeRow_FromNodeIndex_ns(SUMA_DSET *dset, int node, int N_Node);
int SUMA_GetNodeRow_FromNodeIndex_eng(SUMA_DSET *dset, int node, int N_Node);
int SUMA_GetNodeIndex_FromNodeRow_ns(SUMA_DSET *dset, int row, int N_Node);
int SUMA_GetNodeIndex_FromNodeRow_eng(SUMA_DSET *dset, int row, int N_Node);
double SUMA_GetDsetNodeValInCol2(SUMA_DSET *dset, int ind, 
                                 int node, int N_Node);
NI_str_array *SUMA_free_NI_str_array(NI_str_array *nisa);
NI_str_array *SUMA_comp_str_2_NI_str_ar(char *s, char *sep);
char *SUMA_NI_str_ar_2_comp_str (NI_str_array *nisa, char *sep);
NI_str_array *SUMA_free_NI_str_array(NI_str_array *nisa);
char *SUMA_Get_Sub_String(char *cs, char *sep, int ii);
int SUMA_AddColAtt_CompString(NI_element *nel, int col, char *lbl, 
                              char *sep, int insert_mode);
int SUMA_Remove_Sub_String(char *cs, char *sep, char *strn);
NI_str_array * SUMA_NI_decode_string_list( char *ss , char *sep );
char  * SUMA_NI_get_ith_string( char *ss , char *sep, int i );
SUMA_VARTYPE SUMA_CTypeName2VarType (char *vt);
const char *SUMA_VarType2CTypeName (SUMA_VARTYPE vt);
SUMA_COL_TYPE SUMA_VarType2ColType (char *vt);
int SUMA_SizeOf(SUMA_VARTYPE vt);
void *SUMA_BinarySuck(char *fname, SUMA_VARTYPE data_type, int endian, int start, int end, int *nvals_read);
void SUMA_swap_2(void *ppp);
void SUMA_swap_4(void *ppp);
void SUMA_swap_8(void *ppp);
int SUMA_suck_file( char *fname , char **fbuf );
char * SUMA_file_suck( char *fname , int *nread );
void *SUMA_AdvancePastNumbers(char *op, char **opend, SUMA_VARTYPE tp);
void *SUMA_strtol_vec(char *op, int nvals, int *nread, 
                      SUMA_VARTYPE vtp, char **opend);
SUMA_Boolean SUMA_ShowParsedFname(SUMA_PARSED_NAME *pn, FILE *out);
char *SUMA_EscapeChars(char *s1, char *ca, char *es);
char *SUMA_ReplaceChars(char *s1, char *ca, char *es);
char *SUMA_isEnv(char *env, char *sval);
float SUMA_floatEnv(char *env, float defval);
ENV_SPEC SUMA_envlistelement(int i);
char * SUMA_EnvVal(char *env);
 
/*********************** END Miscellaneous support functions **************************** */

/******** BEGIN functions for surface structure  ******************** */
void SUMA_ShowAfniSurfaceObject(NI_group *aSO, FILE *out,
                              int detail, char *title);
char *SUMA_AfniSurfaceObject_Info(NI_group *aSO, 
                                  int detail, char *title);
NI_group * afni_open_gifti_surf(char * fname, int read_data);
int afni_write_gifti_surf( NI_group *aSO, char * fname, 
                           int write_data, int encoding);


/******** END functions for surface structure  ******************** */


#endif
