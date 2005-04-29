#ifndef SUMA_DATASETS_INCLUDED
#define SUMA_DATASETS_INCLUDED

#define SUMA_MAX_NAME_LENGTH 500   /*!< Maximum number of characters in a filename */
#define SUMA_MAX_DIR_LENGTH 2000    /*!< Maximum number of characters in a directory name */
#ifndef SUMA_IDCODE_LENGTH
   #define SUMA_IDCODE_LENGTH 50
#endif

#define SUMA_VERSION_VECTOR 20041229, 20040610, 20040116, \
                            20040106, -10000 /*!< modify this dude and you must update SUMA_New_Additions_perver 
                                       in SUMA_help.c. 
                                       Add to the left of the vector, leave the last value of -10000 untouched
                                       If you like to think of floating point version numbers,divide by 10000
                                       This define is stuck here so that non-SUMA DataSet manipulating programs 
                                       can use it, one hopes.
                                       Numbering is yyyymmdd */
#define SUMA_EMPTY_ATTR "~"
#define SUMA_NI_SS ","    /* string separator */

typedef enum { NOPE, YUP} SUMA_Boolean;

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

/*! filename and path */
typedef struct {
   char *Path;
   char *FileName;
}SUMA_FileName;

/*! filename, extension and path */
typedef struct {
   char *Path;
   char *FileName;
   char *FileName_NoExt;
   char *Ext;
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
   SUMA_NODE_XYZ,
   SUMA_NEW_NODE_XYZ,
   SUMA_NODE_CONVEXITY,
   SUMA_NEW_MESH_IJK,
   SUMA_MESH_IJK,
   SUMA_PREP_NEW_SURFACE,
   SUMA_VIEWER_SETTING,
   SUMA_SURFACE_VOLUME_PARENT,
   SUMA_SURFACE_OBJECT,
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
} SUMA_DSET_FORMAT; /*!<  Format of data set
                          When you add a new element, modify functions
                          SUMA_Dset_Format_Name
                          SUMA_Dset_Format */ 

typedef enum {
   SUMA_ERROR_COL_TYPE = -1,
   SUMA_NO_COL_TYPE,
   SUMA_NODE_INT,    /*!< Generic integer */
   SUMA_NODE_INDEX,  /*!< index of a node to locate it in its domain */
   SUMA_NODE_ILABEL, /*!< An integer coding for a label */
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
   float FillColor[3];  /*!< RGB fill color */
   float EdgeColor[3];  /*!< RGB edge color */
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
         GeomParent_idcode: Unique identifier of the surface containing the 
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
            SUMA_DsetInfo
            SUMA_ShowMeSome
         Miscellaneous functions/tools:
            SUMA_Dset_Format 
            SUMA_Dset_Format_Name
            SUMA_AddNelHist
                    
         Sample Code: SUMA_Test_DSET_IO_STANDALONE
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
            GeomParent_idcode: Unique identifier of the surface containing the 
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
         */    
   NI_element *dnel; /*!< a copy of the NI_element pointer that contains the tabular data inside ngr. Do not free this
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
         (strcmp(argv[m_kar], "-TRACE") == 0)) ) {   \
		/* valid options, but already taken care of */  \
		m_brk = YUP;                                   \
	}                                               \
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

#define SUMA_TO_LOWER(s) { \
   int m_i; \
   if (s) { \
      for (m_i=0; m_i < strlen(s); ++m_i) { \
         if (s[m_i] >= 'A' && s[m_i] <= 'Z') s[m_i] = s[m_i] + 'a' - 'A';  \
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
   #define SDSET_IDGDOM(dset) NI_get_attribute(dset->nel,"GeomParent_idcode") 
   #define SDSET_IDMDOM(dset) SUMA_sdset_idmdom(dset)
   #define SDSET_SORTED(dset) NI_get_attribute(dset->nel,"sorted_node_def") 
   #define SDSET_TYPE_NAME(dset) dset->nel->name
   #define SDSET_TYPE(dset) SUMA_Dset_Type(dset->nel->name)
   #define SDEST_VECLEN(dset) dset->nel->vec_len
#else
   #define SDSET_FILENAME(dset) NI_get_attribute(dset->ngr,"filename")
   #define SDSET_LABEL(dset) NI_get_attribute(dset->ngr,"label")
   #define SDSET_ID(dset) SUMA_sdset_id(dset) 
   #define SDSET_IDGDOM(dset) NI_get_attribute(dset->ngr,"GeomParent_idcode") 
   #define SDSET_IDMDOM(dset) SUMA_sdset_idmdom(dset)
   #define SDSET_SORTED(dset) NI_get_attribute(dset->ngr,"sorted_node_def") 
   #define SDSET_TYPE_NAME(dset) dset->ngr->name
   #define SDSET_TYPE(dset) SUMA_Dset_Type(dset->ngr->name)
   #define SDEST_VECLEN(dset) dset->dnel->vec_len

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
            if (!(dset->dnel = SUMA_FindDsetDataAttributeElement(dset))) {  \
               SUMA_SL_Err("Cannot find data element!\nCleaning up.\n");   \
               NI_free_element (dset->ngr); dset->ngr = NULL;  \
            }  \
         }\
      }  \
      /* close the stream */  \
      NI_stream_close( m_ns ) ; \
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
   suc is a flag for success (1), failure (0)
*/
#define NEL_WRITE_TX(nel, frm, suc) { \
   NI_stream m_ns = NULL;  \
   suc = 1; \
   m_ns = NI_stream_open( frm , "w" ) ;   \
   if( m_ns == NULL ) {    \
      SUMA_SL_Err ("Failed to open stream");  \
      suc = 0; \
   } else { \
      /* write out the element */   \
      if (NI_write_element( m_ns , nel , NI_TEXT_MODE ) < 0) { \
         SUMA_SL_Err ("Failed to write element");  \
         suc = 0; \
      }  \
   }  \
   /* close the stream */  \
   NI_stream_close( m_ns ) ; \
}
#define DSET_WRITE_1D(dset, frm, suc) { \
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
         if (NI_write_element( m_ns , dset->dnel , NI_TEXT_MODE | NI_HEADERSHARP_FLAG) < 0) { \
            SUMA_SL_Err ("Failed to write element");  \
            suc = 0; \
         }  \
      }  \
      /* close the stream */  \
      NI_stream_close( m_ns ) ; \
   }  \
}
#define DSET_WRITE_1D_PURE(dset, frm, suc) { \
   FILE *m_fid = NULL;  \
   int m_ind, m_ival;   \
   int m_allnum;  \
   suc = 1; \
   m_allnum = SUMA_is_AllNumeric_dset(dset);   \
   if (!m_allnum) { \
      SUMA_SL_Err ("Element cannont be written to 1D format");    \
      suc = 0; \
   } else {   \
      m_fid = fopen(frm,"w"); \
      if( m_fid == NULL ) {    \
         SUMA_SL_Err ("Failed to open file for output");  \
         suc = 0; \
      } else { \
         for (m_ival=0; m_ival<dset->dnel->vec_len; ++m_ival) { \
            for (m_ind=0; m_ind<dset->dnel->vec_num; ++m_ind) { \
               fprintf(m_fid,"%f   ", SUMA_GetDsetValInCol2(dset, m_ind, m_ival));  \
            }  \
            fprintf(m_fid,"\n"); \
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
   if (m_tt == NI_GROUP_TYPE) { m_allnum = 0; SUMA_SL_Err ("Group, use DSET_WRITE_1D_PURE"); }/* SHOULD USE DSET_WRITE_1D */   \
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
         if (NI_write_element( m_ns , nel , NI_TEXT_MODE | NI_HEADERSHARP_FLAG) < 0) { \
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
   if (m_tt == NI_GROUP_TYPE) { m_allnum = 0;  SUMA_SL_Err ("Group, use DSET_WRITE_1D_PURE"); }   /* SHOULD USE DSET_WRITE_1D */  \
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


NI_element *SUMA_FindDsetDataAttributeElement(SUMA_DSET *dset);
NI_element *SUMA_FindDsetAttributeElement(SUMA_DSET *dset, char *attname);
NI_element *SUMA_FindNgrAttributeElement(NI_group *ngr, char *attname);
float SUMA_LatestVersionNumber(void);
char * SUMA_Dset_Type_Name (SUMA_DSET_TYPE tp);
SUMA_DSET_TYPE SUMA_Dset_Type (char *Name);
char * SUMA_Col_Type_Name (SUMA_COL_TYPE tp);
SUMA_COL_TYPE SUMA_Col_Type (char *Name);
char * SUMA_AttrOfDsetColNumb(SUMA_DSET *dset, int ind);
SUMA_COL_TYPE SUMA_TypeOfDsetColNumb(SUMA_DSET *dset, int ind);
SUMA_COL_TYPE SUMA_TypeOfColNumb(NI_element *nel, int ind) ;
SUMA_VARTYPE SUMA_ColType2TypeCast (SUMA_COL_TYPE ctp); 
int SUMA_ShowNel (NI_element *nel);
void SUMA_allow_nel_use(int al);
int SUMA_AddDsetNelCol ( SUMA_DSET *dset, char *col_label, 
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride);
int SUMA_AddNelCol ( NI_element *nel, char *col_label,
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride);
int SUMA_AddDsetColAttr (SUMA_DSET *dset, char *col_label, 
                     SUMA_COL_TYPE ctp, void *col_attr, int col_index);
int SUMA_AddColAttr (NI_element *nel, char *col_label,
                     SUMA_COL_TYPE ctp, void *col_attr, int col_index);
SUMA_Boolean SUMA_NewDsetGrp (SUMA_DSET *dset, SUMA_DSET_TYPE dtp, 
                           char* MeshParent_idcode, 
                          char * GeomParent_idcode, int N_el, 
                          char *filename, char *thisidcode);
NI_element * SUMA_NewNel (SUMA_DSET_TYPE dtp, char* MeshParent_idcode, 
                          char * GeomParent_idcode, int N_el, 
                          char *name, char *thisidcode);
SUMA_DSET_FORMAT SUMA_Dset_Format (char *Name);
char * SUMA_Dset_Format_Name (SUMA_DSET_FORMAT fr);
char *SUMA_HistString (char *CallingFunc, int N_arg, char **arg, char *sold);
char * SUMA_GetNgrHist(NI_group *ngr);
int SUMA_AddNgrHist(NI_group *ngr, char *CallingFunc, int N_arg, char **arg);
int SUMA_AddNelHist(NI_element *nel, char *CallingFunc, int N_arg, char **arg);
void SUMA_FreeDset(void *dset);
SUMA_DSET * SUMA_FindDset (char *idcode_str, DList *DsetList);
char *SUMA_DsetInfo (SUMA_DSET *dset, int detail);
void SUMA_ShowDset (SUMA_DSET *dset, int detail, FILE *out);
char *SUMA_ShowMeSome (void *dt, SUMA_VARTYPE tp, int N_dt, int mxshow);
SUMA_DSET * SUMA_NewDsetPointer(void);
SUMA_DSET * SUMA_CreateDsetPointer (  
                              char *name, 
                              SUMA_DSET_TYPE tp,
                              char *idcode_str,
                              char *domain_idcode_str,
                              int N_Alloc); 
int SUMA_InsertDsetPointer (SUMA_DSET *dset, DList *DsetList);
void * SUMA_GetCx(char *idcode_str, DList *DsetList, int ReturnDsetPointer) ;
#if 0
SUMA_DSET *SUMA_LinkToDset(SUMA_DSET *dset);
SUMA_DSET *SUMA_UnlinkFromDset(SUMA_DSET *dset);
#endif
void *SUMA_LinkToPointer(void *ptr);
void *SUMA_UnlinkFromPointer(void *ptr);
int * SUMA_GetNodeDef(SUMA_DSET *dset);
int SUMA_FillDsetNelCol (SUMA_DSET *dset, char *col_label,
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride); 
int SUMA_FillNelCol (NI_element *nel, char *col_label,
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride); 
int *SUMA_GetDsetColIndex (SUMA_DSET *dset, SUMA_COL_TYPE tp, int *N_i);
int *SUMA_GetColIndex (NI_element *nel, SUMA_COL_TYPE tp, int *N_i);
float * SUMA_DsetCol2Float (SUMA_DSET *dset, int ind, int FilledOnly);
float * SUMA_Col2Float (NI_element *nel, int ind, int FilledOnly);
int SUMA_GetDsetColRange(SUMA_DSET *dset, int col_index, float range[2], int loc[2]);
int SUMA_GetColRange(NI_element *nel, int col_index, float range[2], int loc[2]);
int SUMA_AddGenDsetColAttr (SUMA_DSET *dset, SUMA_COL_TYPE ctp, void *col, int stride, int col_index);
int SUMA_AddGenColAttr (NI_element *nel, SUMA_COL_TYPE ctp, void *col, int stride, int col_index); 
SUMA_DSET *SUMA_LoadNimlDset (char *Name, int verb);
SUMA_DSET *SUMA_LoadDset (char *Name, SUMA_DSET_FORMAT *form, int verb);
SUMA_DSET *SUMA_Load1DDset (char *Name, int verb);
SUMA_DSET *SUMA_LoadDXDset (char *Name, int verb);
char *SUMA_RemoveDsetExtension (char*Name, SUMA_DSET_FORMAT form);
char * SUMA_WriteDset (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, int overwrite, int verb); 
SUMA_DSET * SUMA_far2dset( char *FullName, char *dset_id, char *dom_id, 
                                 float **farp, int vec_len, int vec_num, 
                                 int ptr_cpy);
int SUMA_is_AllNumeric_dset(SUMA_DSET *dset); 
int SUMA_is_AllNumeric_ngr(NI_group *ngr) ;
int SUMA_is_AllNumeric_nel(NI_element *nel);
SUMA_Boolean SUMA_NewDsetID (SUMA_DSET *dset);
char *SUMA_DsetColLabelCopy(SUMA_DSET *dset, int i, int addcolnum);
char *SUMA_ColLabelCopy(NI_element *nel, int i, int addcolnum);
SUMA_DSET * SUMA_MaskedCopyofDset(SUMA_DSET *odset, byte *rowmask, byte *colmask, int masked_only, int keep_node_index);
void *SUMA_Copy_Part_Column(void *col,  NI_rowtype *rt, int N_col, byte *rowmask, int masked_only, int *n_incopy);
char* SUMA_sdset_id(SUMA_DSET *dset);
char* SUMA_sdset_idmdom(SUMA_DSET *dset);
NI_group *SUMA_oDsetNel2nDsetNgr(NI_element *nel); 
void SUMA_SetParent_DsetToLoad(char *parent);
/*********************** BEGIN Miscellaneous support functions **************************** */
#ifdef SUMA_COMPILED
   #define SUMA_STANDALONE_INIT {   \
      /* install signal handler, shamelessly copied from AFNI) */ \
      signal(SIGINT ,SUMA_sigfunc) ;      \
      signal(SIGBUS ,SUMA_sigfunc) ;   \
      signal(SIGSEGV,SUMA_sigfunc) ;   \
      signal(SIGTERM,SUMA_sigfunc) ;   \
      SUMA_process_environ(); \
         SUMAg_CF = SUMA_Create_CommonFields ();   \
	      if (SUMAg_CF == NULL) { \
		      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName); \
		      exit(1); \
	      }  \
         /* SUMAg_CF->scm = SUMA_Build_Color_maps();  require X connection*/\
      SUMA_ParseInput_basics (argv, argc);   \
   }
#else
   #define SUMA_STANDALONE_INIT {   \
      /* install signal handler, shamelessly copied from AFNI) */ \
      signal(SIGINT ,SUMA_sigfunc) ;      \
      signal(SIGBUS ,SUMA_sigfunc) ;   \
      signal(SIGSEGV,SUMA_sigfunc) ;   \
      signal(SIGTERM,SUMA_sigfunc) ;   \
      SUMA_process_environ(); \
      SUMA_ParseInput_basics (argv, argc);   \
   }

#endif   

int SUMA_filexists (char *f_name);
char *SUMA_help_basics();
char *SUMA_help_talk();
void SUMA_process_environ(void);
void SUMA_ParseInput_basics (char *argv[], int argc); 
SUMA_FileName SUMA_StripPath (char *FileName);
SUMA_PARSED_NAME * SUMA_ParseFname (char *FileName);
char *SUMA_Extension(char *filename, char *ext, SUMA_Boolean Remove);
SUMA_DSET_FORMAT SUMA_GuessFormatFromExtension(char *Name);
SUMA_Boolean SUMA_isExtension(char *filename, char *ext);
void *SUMA_Free_Parsed_Name(SUMA_PARSED_NAME *Test);
int SUMA_StringToNum (char *s, float *fv, int N);
int SUMA_isNumString (char *s, void *p);
char *SUMA_copy_string(char *buf);
char * SUMA_append_string(char *s1, char *s2);
char * SUMA_append_replace_string(  char *s1, char *s2, 
                                    char *Spc, int whichTofree);
char * SUMA_truncate_string (char *s1, int length);
char *SUMA_set_string_length(char *buf, char cp, int n);
SUMA_STRING * SUMA_StringAppend (SUMA_STRING *SS, char *newstring);
SUMA_STRING * SUMA_StringAppend_va (SUMA_STRING *SS, char *newstring, ... );
void SUMA_sigfunc(int sig);
char *SUMA_pad_string(char *buf, char cp, int n, int add2end);
char * SUMA_GetDsetValInCol(SUMA_DSET *dset, int ind, int ival, double *dval) ;
char * SUMA_GetValInCol(NI_element *nel, int ind, int ival, double *dval); 
double SUMA_GetDsetValInCol2(SUMA_DSET *dset, int ind, int ival) ;
double SUMA_GetValInCol2(NI_element *nel, int ind, int ival); 
int SUMA_GetNodeRow_FromNodeIndex(SUMA_DSET *dset, int node, int N_Node);
int SUMA_GetNodeIndex_FromNodeRow(SUMA_DSET *dset, int row, int N_Node);
NI_str_array *SUMA_free_NI_str_array(NI_str_array *nisa);
NI_str_array *SUMA_comp_str_2_NI_str_ar(char *s, char *sep);
char *SUMA_NI_str_ar_2_comp_str (NI_str_array *nisa, char *sep);
NI_str_array *SUMA_free_NI_str_array(NI_str_array *nisa);
char *SUMA_Get_Sub_String(char *cs, char *sep, int ii);
int SUMA_AddColAtt_CompString(NI_element *nel, int col, char *lbl, char *sep);
NI_str_array * SUMA_NI_decode_string_list( char *ss , char *sep );
char  * SUMA_NI_get_ith_string( char *ss , char *sep, int i );
SUMA_VARTYPE SUMA_VarType2TypeCast (char *vt);
SUMA_COL_TYPE SUMA_VarType2ColType (char *vt);
int SUMA_SizeOf(SUMA_VARTYPE vt);
void *SUMA_BinarySuck(char *fname, SUMA_VARTYPE data_type, int endian, int start, int end, int *nvals_read);
void SUMA_swap_2(void *ppp);
void SUMA_swap_4(void *ppp);
void SUMA_swap_8(void *ppp);

/*********************** END Miscellaneous support functions **************************** */


#endif
