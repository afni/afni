#ifndef SUMA_DATASETS_INCLUDED
#define SUMA_DATASETS_INCLUDED

#ifndef SUMA_IDCODE_LENGTH
   #define SUMA_IDCODE_LENGTH 50
#endif

typedef enum { NOPE, YUP} SUMA_Boolean;

typedef enum { SUMA_notypeset = -1, SUMA_byte, SUMA_int, SUMA_float, SUMA_double, SUMA_string} SUMA_VARTYPE;

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
   SUMA_NODE_ROI, /*!< Col0: Node ID, Col1: ROI label (int) */
   SUMA_NODE_RGB,
   SUMA_NODE_RGBb,
   SUMA_NODE_RGBA,
   SUMA_NODE_RGBAb,
   SUMA_NODE_XYZ,
   SUMA_NODE_CONVEXITY,
   SUMA_VIEWER_SETTING
} SUMA_DSET_TYPE; /*!<  Type of data set 
                        When you add a new element, modify functions
                        SUMA_Dset_Type_Name
                        SUMA_Dset_Type */

typedef enum {
   SUMA_ERROR_DSET_FORMAT = -1,
   SUMA_NO_DSET_FORMAT,
   SUMA_ASCII_NIML,
   SUMA_BINARY_NIML,
   SUMA_NIML,
   SUMA_1D,
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
   SUMA_NODE_STRING   /*!< Generic String */
}  SUMA_COL_TYPE; /*!<  Column types.
                        When you add a new element, you need to modify
                        SUMA_AddColAttr
                        SUMA_Col_Type 
                        SUMA_Col_Type_Name
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
               SUMA_LINKED_OVERLAY_TYPE, /*!< For pointers to SUMA_OVERLAYS (not used yet)*/
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
   /* WHAT WILL YOU DO ABOUT THE FACT THAT YOU'LL NEED NodeDef 
   and N_NodeDef both here AND in SUMA_OVERLAYS. 
   You Might want to move it here because it is not 
   really a property of SUMA_OVERLAYS. 
   But you'll need a quick way to grab it from SUMA_DSET 
   for a certain overlay and you'll need to create a SUMA_DSET
   when you load a color file ... */
   
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
   
   /* *** You can go crazy below */
   NI_element *nel;  /*!< The whole deal */
   
   #if 0 /* all these fields are inside of nel */
   char *name; /*! ATTRIBUTE: The name of the data set */
   char *Label; /*! ATTRIBUTE: A short label of the data set */
   char *idcode_str; /*!< ATTRIBUTE: Unique identifier for this data set */
   char *domain_idcode_str; /*!< ATTRIBUTE: Unique identifier for the domain over which this set is defined */
   
   int N_NodeDef;    /*!< (Column of type SUMA_NODE_INDEX)
                           ATTRIBUTE: vec_filled. The number of nodes over which the data are defined. */
   int *NodeDef;     /*!< (Column of type SUMA_NODE_INDEX)
                           The vector of nodes over which the data are defined. */ 
   int N_Alloc;      /*!< (ATTRIBUTE: vec_len) This would be You'd think this should be equal to NodeDef, but in 
                          instances where you may be receiving data for a varying
                          number of nodes, it's a pain to have to free and realloc space.
                          So, while the juice is only up to N_NodeDef, the allocation 
                          is for N_Alloc */
   SUMA_Boolean SortedNodeDef; /*!< (ATTRIBUTE:) flag indicating that nodes in NodeDef are sorted.
                                    Need function to do binary search for a certain
                                    node index... IS IT ALWAYS THE CASE THAT NodeDef 
                                    is used or is it assumed that indexing is explicit
                                    when a complete set is loaded ?*/ 
   int N_sub;      /*!< (vec_num) Think number of sub-bricks, subsets ...*/
   /* NO NEED FOR THIS FIELD because you can create columns of structures ....
   See SUMA_NIML_ROI_DATUM and SUMA_NIML_DRAWN_ROI for inspiration */
   int *N_mx;      /*!< Multiplexing of data vector [i]. Typically, 
                       I hope this would be 1. But you could have this
                       number be 3 or the number of time points, should
                       you choose, for efficiency reasons, to store 
                       data that typically goes into multiple sub-bricks 
                       into one vector. I will regret this, I am sure. 
                       This vector is N_sub elements long.
                       Multiplexing is done in the SUMA_ROW_MAJOR order, so 
                       RGB values for node j will be at:
                       data[i][j*3], data[i][j*3+1], data[i][j*3+2] 
                       Should you wish to use SUMA_COLUMN_MAJOR then
                       split your vector along data[i1], data[i2] and data[i3]
                       since there is no memory access disadvantage this way*/
   void **data; /*!< (that's nel->vec baby) data is a vector of N_sub data[i] pointers to data vectors.
                     Each data[i] will point to a vector of N_mx[i]*N_Alloc values.
                     Because of N_mx, data[i1] can be of a different length from data[i2]
                     You do not have to have data associated with an overlay plane,
                     it can be just colors.
                */
   SUMA_VARTYPE *tp; /*!< Already taken care of with functions SUMA_AddNelCol and the like ... 
                        type of values stored in data[i]. This vector is N_sub long. */
   #endif
} SUMA_DSET;

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
   \brief Macros to access dataset elements 
   Almost all of them involve a function call
   so don't use them in loops where the returned
   value is not expected to change
*/
#define SDSET_FILENAME(dset) NI_get_attribute(dset->nel,"filename")
#define SDSET_LABEL(dset) NI_get_attribute(dset->nel,"label")
#define SDSET_ID(dset) NI_get_attribute(dset->nel,"idcode") 
#define SDSET_IDGDOM(dset) NI_get_attribute(dset->nel,"GeomParent_idcode") 
#define SDSET_IDMDOM(dset) NI_get_attribute(dset->nel,"MeshParent_idcode") 
#define SDSET_SORTED(dset) NI_get_attribute(dset->nel,"sorted_node_def") 
#define SDSET_TYPE_NAME(dset) dset->nel->name
#define SDSET_TYPE(dset) SUMA_Dset_Type(dset->nel->name)
#define SDEST_VECLEN(dset) dset->nel->vec_len

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
#define COLP_N_ALLOC(cop) cop->dset_link->nel->vec_len

/* #define DSET_(dset) NI_get_attribute(dset->nel,"") */

/*!
   NEL_READ macro for reading a NI element from strm
   nel (NI_element *) to contain the deed (if null then read failed)
   frm the source such as: "file:Test_niml_file"
*/
#define NEL_READ(nel, frm) { \
   NI_stream m_ns = NULL;  \
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
}

/*!
   NEL_WRITE_TX(nel, strm, suc)
   NEL_WRITE_BI(nel, strm, suc)
   NEL_WRITE_1D(nel, strm, suc)
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
#define NEL_WRITE_1D(nel, frm, suc) { \
   NI_stream m_ns = NULL;  \
   suc = 1; \
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

int SUMA_StringToNum (char *s, float *fv, int N);
int SUMA_isNumString (char *s, void *p);
char * SUMA_Dset_Type_Name (SUMA_DSET_TYPE tp);
SUMA_DSET_TYPE SUMA_Dset_Type (char *Name);
char * SUMA_Col_Type_Name (SUMA_COL_TYPE tp);
SUMA_COL_TYPE SUMA_Col_Type (char *Name);
SUMA_VARTYPE SUMA_ColType2TypeCast (SUMA_COL_TYPE ctp); 
int SUMA_ShowNel (NI_element *nel);
int SUMA_AddNelCol ( NI_element *nel, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride);
int SUMA_AddColAttr (NI_element *nel, SUMA_COL_TYPE ctp, void *col_attr, int col_index);
NI_element * SUMA_NewNel (SUMA_DSET_TYPE dtp, char* MeshParent_idcode, 
                          char * GeomParent_idcode, int N_el, 
                          char *name, char *thisidcode);
SUMA_DSET_FORMAT SUMA_Dset_Format (char *Name);
char * SUMA_Dset_Format_Name (SUMA_DSET_FORMAT fr);
int SUMA_AddNelHist(NI_element *nel, char *CallingFunc, int N_arg, char **arg);
char *SUMA_copy_string(char *buf);
char * SUMA_append_string(char *s1, char *s2);
char * SUMA_append_replace_string(  char *s1, char *s2, 
                                    char *Spc, int whichTofree);
char * SUMA_truncate_string (char *s1, int length);
void SUMA_FreeDset(void *dset);
SUMA_DSET * SUMA_FindDset (char *idcode_str, DList *DsetList);
char *SUMA_DsetInfo (SUMA_DSET *dset, int detail);
char *SUMA_ShowMeSome (void *dt, SUMA_COL_TYPE tp, int N_dt, int mxshow);
SUMA_DSET * SUMA_NewDsetPointer(void);
SUMA_DSET * SUMA_CreateDsetPointer (  
                              char *name, 
                              SUMA_DSET_TYPE tp,
                              char *idcode_str,
                              char *domain_idcode_str,
                              int N_Alloc); 
int SUMA_InsertDsetPointer (SUMA_DSET *dset, DList *DsetList);
SUMA_STRING * SUMA_StringAppend (SUMA_STRING *SS, char *newstring);
SUMA_STRING * SUMA_StringAppend_va (SUMA_STRING *SS, char *newstring, ... );
void * SUMA_GetCx(char *idcode_str, DList *DsetList, int ReturnDsetPointer) ;
#if 0
SUMA_DSET *SUMA_LinkToDset(SUMA_DSET *dset);
SUMA_DSET *SUMA_UnlinkFromDset(SUMA_DSET *dset);
#endif
void *SUMA_LinkToPointer(void *ptr);
void *SUMA_UnlinkFromPointer(void *ptr);
int * SUMA_GetNodeDef(SUMA_DSET *dset);
int SUMA_FillNelCol (NI_element *nel, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride); 
int *SUMA_GetColIndex (NI_element *nel, SUMA_COL_TYPE tp, int *N_i);
float * SUMA_Col2Float (NI_element *nel, int ind, int FilledOnly);
int SUMA_GetColRange(NI_element *nel, int col_index, float range[2], int loc[2]);
int SUMA_AddGenColAttr (NI_element *nel, SUMA_COL_TYPE ctp, void *col, int stride, int col_index); 


#endif
