#ifndef SUMA_DATASETS_INCLUDED
#define SUMA_DATASETS_INCLUDED

#include "replaceXt.h"  /* 09 Nov 2018 */

#include "suma_objs.h"
#include "matrix.h"
#include "suma_afni_surface.h"

#define MAX_ERRLOG_MSG 1000
#define MAX_ERRLOG_FUNCNAME 200
#define TMP_NAME_SEP "_LDPID_"

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


#define SUMA_free(a) mcw_free(a,__FILE__,__LINE__)
#define SUMA_ifree(p)  { if ((p)) {SUMA_free((p));} (p)=NULL; }

#ifdef USE_TRACING
#define SUMA_DUMP_TRACE( ... ) { /* taken from dbtrace.h */\
   int m_ii;  \
   SUMA_S_Note( __VA_ARGS__ );\
   if( DBG_num >= 0 ){  \
      for( m_ii=DBG_num-1; m_ii >= 0 ; m_ii-- ) \
         fprintf(stderr,"%*.*s%s\n",m_ii+1,m_ii+1," ",DBG_rout[m_ii]) ; \
   } else { \
      fprintf(stderr,"[No debug tracing stack: DBG_num=%d]\n",DBG_num) ;   \
   }  \
}
#define SUMA_EDUMP_TRACE( ... ) { /* taken from dbtrace.h */\
   int m_ii;  \
   SUMA_S_Err( __VA_ARGS__ ); \
   if( DBG_num >= 0 ){  \
      for( m_ii=DBG_num-1; m_ii >= 0 ; m_ii-- ) \
         fprintf(stderr,"%*.*s%s\n",m_ii+1,m_ii+1," ",DBG_rout[m_ii]) ; \
   } else { \
      fprintf(stderr,"[No debug tracing stack: DBG_num=%d]\n",DBG_num) ;   \
   }  \
}
#else
#define SUMA_DUMP_TRACE(...) /* nada */
#define SUMA_EDUMP_TRACE( ... ) /* nada */
#endif
#define SUMA_T_Err SUMA_EDUMP_TRACE

typedef struct {
    char macroname[100];
    char msg[MAX_ERRLOG_MSG];
    char FuncName[MAX_ERRLOG_FUNCNAME];
} SUMA_ERRLOG;


typedef enum { SUMA_SIDE_ERROR=-1, SUMA_NO_SIDE, SUMA_LR, SUMA_LEFT, SUMA_RIGHT } SUMA_SO_SIDE; 

typedef enum { SUMA_NO_NUM_UNITS = 0, 
               SUMA_MM_UNITS,
               SUMA_P_VALUE_UNITS,
               SUMA_Q_VALUE_UNITS,
               SUMA_PERC_VALUE_UNITS,
               
               SUMA_N_NUMERICAL_UNITS
               } SUMA_NUMERICAL_UNITS;


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



typedef enum {
   SUMA_ERROR_DSET_TYPE = -1,
   SUMA_NO_DSET_TYPE,
   SUMA_NODE_BUCKET,
   SUMA_VOXEL_BUCKET,
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
   SUMA_GRAPH_BUCKET,
   SUMA_TRACT_BUCKET,
   SUMA_CIFTI_BUCKET,
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
   SUMA_1D_PURE_TRANSPOSE,    /* 7 */
   SUMA_1D_STDOUT,            /* THIS ONE IS USED AS A MARKER TOO   */   /*8 */
   SUMA_1D_STDERR,            /* 9 */
   SUMA_NIML_STDOUT,          /* 10 */
   SUMA_NIML_STDERR,          /* 11 */
   SUMA_1D_PURE_STDOUT,       /* 12 */
   SUMA_1D_PURE_STDERR,       /* 13 */
   SUMA_1D_PURE_STDOUT_TRANSPOSE,       /* 14 */
   SUMA_1D_PURE_STDERR_TRANSPOSE,/* THIS ONE IS USED AS A MARKER TOO*/  /* 15 */
   SUMA_XML_DSET,                  /* 16 */
   SUMA_XML_ASCII_DSET,            /* 17 */
   SUMA_XML_B64_DSET,              /* 18 */
   SUMA_XML_B64GZ_DSET,             /* 19 */
   
   SUMA_N_DSET_FORMATS           /* leave at the end */
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
   SUMA_NODE_INDEX,  /*!< index of a node OR edge to locate it in its domain */
   SUMA_NODE_ILABEL, /*!< An integer coding for an integer label */
   SUMA_NODE_SLABEL, /*!< An string label */
   SUMA_GNODE_IGROUP, /*!< An integer coding for a group integer label of 
                           a graph node*/
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
   SUMA_NODE_COMPLEX,     /*!< Generic complex */
   SUMA_NODE_XCORR,      /*!< Cross Correlation Coefficient */
   SUMA_NODE_ZSCORE,      /*!< Zscore */
   SUMA_NODE_VFR,       /* Visual Field Ration */
   SUMA_NODE_PHASE,     /* Phase of some sort */
   SUMA_NODE_AREA,      /* Area associated with node*/
   SUMA_NODE_VOLUME,    /* Volume associated with node */
   SUMA_NODE_THICKNESS,  /* Thickness (distance between isotopic nodes) */
   SUMA_GNODE_INDEX,     /*!< index of a graph's node/point, 
                              not to be confused with SUMA_NODE_INDEX which 
                              really should be named SUMA_DATUM_INDEX if it were
                              not for all the other _NODE_ which would also
                              need changing*/
   SUMA_EDGE_P1_INDEX,        /* First point (Graph Node) defining an edge */
   SUMA_EDGE_P2_INDEX,        /* Second point defining an edge */
   
   SUMA_MD_NODE_INDEX, /*!< Index col. into a Multiple Domain object a la CIFTI*/
   
   SUMA_N_COL_TYPES           /* MAX number of col types */
}  SUMA_COL_TYPE; /*!<  Column types.
                        When you add a new element, you need to modify
                        SUMA_AddColAttr
                        SUMA_Col_Type 
                        SUMA_Col_Type_Name
                        SUMA_ColType2TypeCast
                        */
#define SUMA_IS_GNODE_IXYZ_COL(ctp) (((ctp)==SUMA_NODE_X || \
                                 (ctp)==SUMA_NODE_Y || \
                                 (ctp)==SUMA_NODE_Z || \
                                 (ctp)==SUMA_GNODE_INDEX || \
                                 (ctp)==SUMA_GNODE_IGROUP || \
                                 (ctp)==SUMA_NODE_R || \
                                 (ctp)==SUMA_NODE_G || \
                                 (ctp)==SUMA_NODE_B || \
                                 (ctp)==SUMA_NODE_SLABEL) ? 1:0)
                                 
#define SUMA_GNODE_IXYZ_CTP2COL(ctp) ( (ctp)==SUMA_NODE_X ? 1 : \
                                       ( (ctp)==SUMA_NODE_Y ? 2: \
                                       ( (ctp)==SUMA_NODE_Z ? 3: \
                                       ( (ctp)==SUMA_NODE_SLABEL ? 4: \
                                       ( (ctp)==SUMA_GNODE_IGROUP ? 5: \
                                       ( (ctp)==SUMA_NODE_R ? 6: \
                                       ( (ctp)==SUMA_NODE_G ? 7: \
                                       ( (ctp)==SUMA_NODE_B ? 8: \
                                       ( (ctp)==SUMA_GNODE_INDEX ? 0: \
                                                         -1 ) ) ) ) ) ) ) ) )
                                       
#define SUMA_IS_DATUM_INDEX_COL(ctp) (((ctp)==SUMA_NODE_INDEX || \
                                 (ctp)==SUMA_EDGE_P1_INDEX || \
                                 (ctp)==SUMA_EDGE_P2_INDEX ) ? 1:0)
#define SUMA_IS_MD_DATUM_INDEX_COL(ctp) (((ctp)==SUMA_NODE_INDEX) ? 1 : 0)
#define SUMA_DATUM_INDEX_CTP2COL(ctp) ( ((ctp)==SUMA_NODE_INDEX || \
                                         (ctp)==SUMA_MD_NODE_INDEX) ? 0 : \
                                    ( (ctp)==SUMA_EDGE_P1_INDEX ? 1: \
                                       ( (ctp)==SUMA_EDGE_P2_INDEX ? 2:-1 ) )  )
                                    
/*!< Displayable Object Types 
                                                                                    S: surface, A: axis, G: grid, 
                                                                                    ROId: Region of interest drawn type,
                                                                                    LS_type: line segment
                                                                                    NBLS_type: Node-based line segment
                 
  OLS_type: oriented line segment
                                                                                    NBOLS_type: Node-based oriented line segment
                                                                                    NBV_type: Node-Based vector (displayed as a line from node)
                                                                                    ONBV_type: NBV with a ball on the bottom (slower to render)
                                                                                    SP_type: spherical markers
                                                                                    NBSP_type: Node-Based spherical markers
                                                                                    PL_type: planes
                                                                                    NBT_type: Node-based text
                                                                                    SBT_type: Screen-based text
                                                                                    DBT_type: Dicom-based text
                                                                                    DIR_type: Directions
                                                                                    ODIR_type: Oriented Directions (not in use yet)
                                                 
  PNT_type: Cloud of points
  
  */
typedef enum { NOT_SET_type = -1,
               not_DO_type, SO_type, AO_type, ROIdO_type, ROIO_type, 
               GO_type, LS_type, NBLS_type, OLS_type, NBOLS_type,
               NBV_type, ONBV_type, SP_type,
               NBSP_type, PL_type, VO_type,
               NBT_type, SBT_type, DBT_type, /*!< Those three will 
                                                   likely not be used */
               NIDO_type, ANY_DSET_type, GDSET_type, MD_DSET_type, TRACT_type,
               GRAPH_LINK_type, MASK_type, DIR_type, ODIR_type, 
               PNT_type,
               CDOM_type, /* The domain of a CIFTI beast */
               N_DO_TYPES } SUMA_DO_Types;   

#define iDO_isSO(i) ( ((i)<0 || (i)>=SUMAg_N_DOv) ? 0: \
                      ( SUMAg_DOv[(i)].ObjectType == SO_type ? 1:0) )
#define iDO_isGLDO(i) ( ((i)<0 || (i)>=SUMAg_N_DOv) ? 0: \
                      ( SUMAg_DOv[(i)].ObjectType == GRAPH_LINK_type ? 1:0) )
#define iDO_isTDO(i) ( ((i)<0 || (i)>=SUMAg_N_DOv) ? 0: \
                      ( SUMAg_DOv[(i)].ObjectType == TRACT_type ? 1:0) )
#define iDO_isMDO(i) ( ((i)<0 || (i)>=SUMAg_N_DOv) ? 0: \
                      ( SUMAg_DOv[(i)].ObjectType == MASK_type ? 1:0) )
#define iDO_isVO(i) ( ((i)<0 || (i)>=SUMAg_N_DOv) ? 0: \
                      ( SUMAg_DOv[(i)].ObjectType == VO_type ? 1:0) )
#define iDO_type(i) ( ((i)<0 || (i)>=SUMAg_N_DOv) ? NOT_SET_type: \
                      ( SUMAg_DOv[(i)].ObjectType ) )
#define iDO_typename(i) ( ((i)<0 || (i)>=SUMAg_N_DOv) ? \
                              "NO OBJECT!": \
                      ( SUMA_ObjectTypeCode2ObjectTypeName(\
                                    SUMAg_DOv[(i)].ObjectType) ) )
#define iDO_ADO(i) ( ((i)<0 || (i)>=SUMAg_N_DOv) ? NULL : \
                              (SUMA_ALL_DO *)SUMAg_DOv[(i)].OP)
#define ADO_iDO(ado) ( (ado) ? SUMA_whichDO(SUMA_ADO_idcode((ado)), \
                                            SUMAg_DOv, SUMAg_N_DOv):-1 )
#define iDO_state SUMA_iDO_state
#define iDO_group SUMA_iDO_group

#define ADO_TNAME(ado) (!(ado) ? \
                              "NULL ADO!": \
                      ( SUMA_ObjectTypeCode2ObjectTypeName(\
                                    (ado)->do_type) ) )
#define ADO_ID(ado) SUMA_ADO_idcode(ado)
#define ADO_LABEL(ado) SUMA_ADO_sLabel(ado)
#define ADO_STATE(ado) SUMA_iDO_state(ADO_iDO(ado))
#define ADO_GROUP(ado) SUMA_iDO_group(ADO_iDO(ado))

#define iDO_label(i) ( ((i)<0 || (i)>=SUMAg_N_DOv) ? \
                              "NO OBJECT!": \
                      ( (SUMA_ADO_Label((SUMA_ALL_DO *)SUMAg_DOv[(i)].OP) ) ) )
#define iDO_variant(i) ( ((i)<0 || (i)>=SUMAg_N_DOv) ? \
                              "NO OBJECT!": \
                      ( (SUMA_ADO_variant((SUMA_ALL_DO *)SUMAg_DOv[(i)].OP) ) ) )

#define iDO_GSaux(i) ( ((i)<0 || (i)>=SUMAg_N_DOv) ? \
                              NULL: \
                      ( (SUMA_ADO_GSaux((SUMA_ALL_DO *)SUMAg_DOv[(i)].OP) ) ) )
#define iDO_idcode(i) ( ((i)<0 || (i)>=SUMAg_N_DOv) ? \
                              NULL: \
                      ( (SUMA_ADO_idcode((SUMA_ALL_DO *)SUMAg_DOv[(i)].OP) ) ) )                      
#define iDO_is_variant(i,var) ( (var) ? !strcmp(iDO_variant(i),(var)):0 )
#define  DO_label(DO) ( !(DO) ? "NO OBJECT!": \
                        ((SUMA_ADO_Label((SUMA_ALL_DO *)(DO)) ) ) )

#define SUMA_IS_GOOD_STATE(mm) ( ((mm)&&strncmp(mm,"TheShadow",9))?1:0 )
#define SUMA_IS_REAL_VARIANT(mm) ( ((mm)&&strncmp(mm,"TheShadow",9))?1:0 )

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
   int Parent_side;
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

typedef struct { /* A structure to contain information about a domain
                    over which a subset of a dataset is defined.
                    This structure was introduced to support CIFTI data */   
   /* The following variables are named after those in CIFTI's documentation for
      BrainModel Element */
      int IndexOffset; /* The number of the first row in dset corresponding to
                       the first datum over this domain  */
      int IndexCount; /* How many consecutive rows in dset correspond to this 
                      domain*/
      int Max_N_Data; /* Maximum number of data points in domain 
                         SO->N_Node, DSET_NVOX(dset) */                
      SUMA_DO_Types ModelType; /* Is this a surface, volume, etc...*/
      SUMA_SO_SIDE  ModelSide; /* Which hemisphere? ...*/
      int Range[4];  /* min , max , imin, imax
                     min, and max are the minimum and maximum node indices
                     present in the full index list for this domain.
                     imin and imax are the rows into the full index list
                     where these min and max indices are found */
      char *Source;
      char *edset_id; /* ID to elementary dataset defining that part of 
      	             the parent multi domain dataset that is 
      	             defined over this one domain only. 
		     'edset' is an elementary (single domain) dataset. */
} SUMA_DSET_DOMAIN;
/* Get the pointer to the beginning of the data indices for domain dom */

#define SUMA_DOMAIN_INDICES(dset,dom) ((( (dset) && \
                                 (dset)->inel && \
                                 (dset)->inel->vec[0] && \
                                 IndexOffset >= 0)) ? \
                                dset->inel->vec[0]+IndexOffset:NULL)

typedef enum { SUMA_NO_PTR_TYPE, 
               SUMA_LINKED_DSET_TYPE, /*!< For pointers to SUMA_DSET */
               SUMA_LINKED_OVERLAY_TYPE, /*!< For pointers to SUMA_OVERLAYS */
               SUMA_LINKED_ND_FRST_NEI_TYPE, 
                                 /*!< For pointers to SUMA_NODE_FIRST_NEIGHB*/
               SUMA_LINKED_MEMB_FACE_TYPE, 
                                 /*!< For pointers to SUMA_MEMBER_FACE_SETS*/
               SUMA_LINKED_SURFCONT_TYPE, /*!< For pointers to SUMA_X_SurfCont*/
               SUMA_LINKED_COLORLIST_TYPE,/* pointers to SUMA_COLORLIST_STRUCT */
               SUMA_LINKED_DRAW_MASKS_TYPE, /*!< Pointers to SUMA_DRAW_MASKS */
               SUMA_N_LINKED_PTR_TYPES } SUMA_LINKED_PTR_TYPES;

typedef enum { MAT_UNKNOWN=-2, MAT_NA = -1, MAT_HEEHAW = 0 /* not set */, 
               MAT_FULL = 1, MAT_TRI, MAT_TRI_DIAG, MAT_SPARSE 
              } SUMA_SQ_MATRIX_SHAPES; 

typedef enum { SURF_DSET, GRAPH_DSET, TRACT_DSET, VOLUME_DSET, 
      	       CIFTI_DSET, MD_DSET} SUMA_DSET_FLAVORS;
typedef enum { SUMA_ELEM_DAT=0, /* Nodes of surface, points of tracts, 
                                 edges of graph */
               SUMA_LEV1_DAT, /* data at the tract level*/
               SUMA_LEV2_DAT, /* data at the bundle level */
               SUMA_N_LEV_DAT} SUMA_DATUM_LEVEL;

typedef struct { /* Something to hold auxiliary datasets structs */
      /* The Saux kids are not always set, they should be
         reached by accessor functions and macros, not
         fished directly from here 
         Where Saux lies depends on the flavor of dset at hand */
   void *Saux; /* A pointer to a structure for SUMA's use */
   void (*FreeSaux)(void *Saux); /* Function to free Saux */
   
   /* Some fields that make queries faster, WARNING, they 
      are duplicates of fields in the NI_group* so you 
      should not set these values explicitly */
   SUMA_SQ_MATRIX_SHAPES matrix_shape;
   long int matrix_max_index;    /* max number of edges */
   long int matrix_size[2];
   long int matrix_2M;
   long int range_edge_index[2]; /* min, max, edge index */
   long int range_node_index[2]; /* min, max, node index 
                                   (points defining edges)*/
   long int N_seg_nodes; /* Number of node indices making up segments*/
   long int N_all_nodes; /* Total number of nodes stored in nodelist of the
                            graph dataset */
   SUMA_DSET_FLAVORS isGraph;
   
   SUMA_DSET_DOMAIN **doms; /* domains over which the dataset 
                               (only CIFTI for now) is defined */
   int N_doms;              /* Number of domains          */
} SUMA_DSET_AUX;

/*!   
   Structure to track copies of a certain pointer.
   DO NOT CHANGE THE ORDER OF THE STRUCTURE's FIELDS 
*/

typedef struct {
   SUMA_DO_Types do_type;  /*!< To check if this is a displayable object 
                              Leave on top for SUMA_ALL_DO and SUMA_DSET */
   int LinkedPtrType; /*!< Indicates the type of linked pointer */
   int N_links;   /*!< Number of links to this pointer */
   char owner_id[SUMA_IDCODE_LENGTH];   /*!< The id of whoever created 
                              that pointer. Might never get used.... */
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
   SUMA_DO_Types do_type;  /*!< To check if this is a displayable object
                                This is to remain on top, to fit with 
                                SUMA_ALL_DO */
   int LinkedPtrType; /*!< Indicates the type of linked pointer */
   int N_links;   /*!< Number of links to this pointer */
   char owner_id[SUMA_IDCODE_LENGTH];   /*!< The id of whoever created 
                                 that pointer. Might never get used.... */
   
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
   NI_group *ngr; /*!< This is now April 06 05, the container of the dataset, 
                       as opposed to the olde days where nel
                       contained everything. The reason that was done is to 
                       accomodate large sized attibutes that
                       do not fit nicely in ASCII forms tucked inside the header.
                       What used to be called nel, is now called dnel (for
                       data-part nel) and is nothing but a copy
                       of the pointer under ngr to the nel that contains the
                       tabular dataset. 
                        
      ngr contains two types of attributes: 
         STRING attributes:
            filename: The filename
            label: A short text label identifying the data set.
                   Typically, a short version of the filename                          
            idcode_str: Unique identifier for the data set
            MeshParent_idcode: Unique identifier of the surface containing 
                               the mesh over which this set is defined
            geometry_parent_idcode: Unique identifier of the surface containing 
                               the coordinates of the nodes whose attributes 
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
   NI_element *dnel; /*!< a copy of the NI_element pointer that contains 
                         the tabular data inside ngr. Do not free this
                         element separately, and make sure its value is changed 
                         in syncrony with the one in ngr. */
   NI_element *inel; /*!< a copy of the NI_element pointer that contains the 
                          node (or edge for graph dsets) index column inside ngr.
                          Do not free this
                          element separately, and make sure its value is 
                          changed in syncrony with the one in ngr. */                
   NI_element *pdnel; /*!< a copy of the NI_element pointer that contains the
                         tabular graph point data inside ngr. Do not free this
                         element separately, and make sure its value is changed 
                         in syncrony with the one in ngr. */
   NI_element *pinel; /*!< a copy of the NI_element pointer that contains the 
                          point index column inside ngr. Do not free this
                          element separately, and make sure its value is 
                          changed in syncrony with the one in ngr. */             
   SUMA_DSET_AUX *Aux;
   #endif     
} SUMA_DSET;

/*!
   Structure containing NIML formatted displayable objects
*/
typedef struct {
      /* FIRST VARIABLES MUST RETAIN THEIR ORDER and follow SUMA_ALL_DO */
   SUMA_DO_Types do_type;
   char *idcode_str;    /*!< unique idcode for DO */
   char *Label; /*!< ascii label for DO */ 
      
      /* Begin specific fields */
   NI_group *ngr;
} SUMA_NIDO;

typedef struct {
   float x;
   float y;
   float z;
} SUMA_XYZ;

typedef struct {
   int N_vals; /* Number of values in each array below.
                  Each array can be NULL, a singleton,
                  or N_vals long. 
                  a singleton is marked by setting (ptr+1) 
                  to NULL */
   SUMA_NIDO **nido; /* Display DOs, perhaps */
   char **lab;
   float *val;
   SUMA_XYZ *loc;
} SUMA_G_DATUM;

/*! structure for holding graph nodes */
typedef struct {
   int id;
   
   SUMA_G_DATUM *gd;
} SUMA_G_NODE;

/*! structure for holding graph edges */
typedef struct {
   int id;
   int n0id; /* id of starting node */
   int n1id; /* id of ending node */
   
   SUMA_G_DATUM *gd;
} SUMA_G_EDGE;

typedef struct {
   SUMA_G_NODE **gn; /* vector of nodes */
   int N_gn;
   int N_alloc;
}  SUMA_G_NODES;

typedef struct {
   SUMA_G_EDGE **ge; /* vector of edges */
   int N_ge;
   int N_alloc;
}  SUMA_G_EDGES;

typedef struct {
   SUMA_G_NODES *nodes;
   SUMA_G_EDGES *edges;
   int O_nodes; /* order of number of nodes, used to increment allocation */
   int O_edges; 
} SUMA_GRAPH;

#define SUMA_COUNTER_SUFFIX(ic)  ( ((ic) == 1) ? "st" : ((ic) == 2) ? "nd" : ((ic) == 3) ? "rd" : "th" )
#define SUMA_COUNTER_PLURAL(ic)  ( ((ic) == 1) ? "" : "s" )

#define SUMA_ECHO_EDU(brk, kar) {   \
   if (!brk && (strcmp(argv[kar], "-echo_edu") == 0)) {  \
      int m_jj;  \
      fprintf(SUMA_STDOUT,"\n+++ Now running:\n   "); \
      for (m_jj=0; m_jj<argc; ++m_jj) {   \
         if (m_jj != kar) {   \
            fprintf(SUMA_STDOUT,"%s ", argv[m_jj]);   \
         }  \
      }  \
      fprintf(SUMA_STDOUT,"\n+++\n");  \
      brk = YUP;   \
   }  \
}
   


#define SUMA_SKIP_COMMON_OPTIONS(m_brk, m_kar) {\
   SUMA_ECHO_EDU(m_brk, m_kar);  \
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
#define SUMA_IS_UPPER_C(c) ( (c >= 'A' && c <= 'Z') )
#define SUMA_IS_LOWER_C(c) ( (c >= 'a' && c <= 'z') )

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
      /* A safer version than SDSET_FILENAME */
   #define SDSET_FILENAME_s(dset) SUMA_sdset_filename(dset)
   
   /* This macro can return NULL, use it wisely, or use the safer macro */
   #define SDSET_LABEL(dset) NI_get_attribute(dset->ngr,"label")
   #define SDSET_LABEL_s(dset) SUMA_sdset_label(dset)
   
   /* This macro can return NULL, can't change behavior of SUMA_sdset_id() 
   unless I check usage of SDSET_ID everywhere in the code... */
   #define SDSET_ID(dset) SUMA_sdset_id(dset) 
   #define SDSET_IDGDOM(dset) \
            NI_get_attribute(dset->ngr,"geometry_parent_idcode") 
   #define SDSET_IDMDOM(dset) SUMA_sdset_idmdom(dset)
   #define SDSET_SORTED(dset) ( (!dset || !dset->inel) ? \
                           NULL:NI_get_attribute(dset->inel,"sorted_node_def") )
   #define SDSET_IS_SORTED(dset) ( (!dset || !dset->inel || \
         !NI_get_attribute(dset->inel,"sorted_node_def") || \
         strcmp(NI_get_attribute(dset->inel,"sorted_node_def"), "Yes") != 0) \
                                                            ? 0 : 1 )
   #define SDSET_TYPE_NAME(dset) NI_get_attribute(dset->ngr,"dset_type")
   #define SDSET_TYPE(dset)   \
      SUMA_Dset_Type(NI_get_attribute(dset->ngr,"dset_type"))
   #define SDSET_DAT_LEVEL(dset) \
      SUMA_sdset_datum_level(dset);
   #define SDSET_COLCAST(dset, i)   \
      SUMA_ColType2TypeCast(SUMA_TypeOfDsetColNumb(dset, i))
   #define SDSET_COLTYPE(dset, i)   \
      SUMA_TypeOfDsetColNumb(dset, i)
   #define SDSET_VECLEN(dset) ( (!dset || !dset->dnel) ? -1:dset->dnel->vec_len)
   #define SDSET_VEC(dset,iii) dset->dnel->vec[iii]
   #define SDSET_NVOX SDSET_VECLEN
   #define SDSET_IS_VOL SUMA_isVolDataset
   #define SDSET_BRICK_FACTOR SUMA_GetBrickFactor
   #define SDSET_BRICK_TYPE   SUMA_GetBrickType
   #define SDSET_ARRAY SDSET_VEC
   #define SDSET_NODEINDLEN(dset) dset->inel->vec_len
   #define SDSET_VECNUM(dset) dset->dnel->vec_num
   #define SDSET_NODEINDNUM(dset) dset->inel->vec_num
   #define SDSET_VECFILLED(dset) dset->dnel->vec_filled
   #define SDSET_NODEINDFILLED(dset) dset->inel->vec_filled
   #define SDSET_NODE_INDEX_COL(dset) ( (!dset || !dset->inel ||  \
                                         !dset->inel->vec) ?   \
                                                NULL:(int*)(dset->inel->vec[0]) )
   #define SDSET_EDGE_INDEX_COL SDSET_NODE_INDEX_COL 
   #define SDSET_EDGE_P1_INDEX_COL(dset) ( (!dset || !dset->inel ||  \
                                      !dset->inel->vec || \
                                      dset->inel->vec_num != 3) ?   \
                                                NULL:(int*)(dset->inel->vec[1]) )
   #define SDSET_EDGE_P2_INDEX_COL(dset) ( (!dset || !dset->inel ||  \
                                      !dset->inel->vec || \
                                      dset->inel->vec_num != 3) ?   \
                                                NULL:(int*)(dset->inel->vec[2]) )
   #define SDSET_EDGE_NODE_INDEX_COLS(dset, PE, P1, P2) {\
      (PE)=(P1)=(P2)=NULL;   \
      if ( (dset) && (dset->inel) && (dset->inel->vec) && \
            dset->inel->vec_num ==3 ) { \
            (PE) = (int*)(dset->inel->vec[0]);  \
            (P1) = (int*)(dset->inel->vec[1]);  \
            (P2) = (int*)(dset->inel->vec[2]);  \
      }  \
   }
                     
   #define SDSET_COL(dset, icol) ( (!dset || !dset->dnel || !dset->dnel->vec) \
                                                 ? NULL:(dset->dnel->vec[icol]) )
   #define SDSET_MATRIX_SZ0(dset) ( (!(dset) || !(dset)->Aux) ? \
                                          -1: ((dset)->Aux->matrix_size[0]) )
   #define SDSET_MATRIX_SZ1(dset) ( (!(dset) || !(dset)->Aux) ? \
                                          -1: ((dset)->Aux->matrix_size[1]) )
   #define GDSET_N_SEG_POINTS(dset) ( (!(dset) || !(dset)->Aux) ? \
                                          -1: ((dset)->Aux->N_seg_nodes) )
   #define GDSET_N_ALL_POINTS(dset) ( (!(dset) || !(dset)->Aux) ? \
                                          -1: ((dset)->Aux->N_all_nodes) )
#endif

#define DSET_MAX_NODE_INDEX(dset, MM) {\
   double r[2]; int loc[2];   \
   if (!SUMA_GetDsetNodeIndexColRange( dset, r, loc, 1)) {  \
      MM = -1; \
   } else { \
      MM = (int)r[1];   \
   }  \
}

/* Edges are the 'nodes' of datasets */
#define GDSET_MAX_EDGE_INDEX DSET_MAX_NODE_INDEX   

/*!
   \brief Macros to access commonly used colorplane parameters
   DO NOT USE COLP_NODEDEF macro inside a loop where the returned
   value is not to change because it involves a function call (SLOW)
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
   char m_stmp[400]={""}; int m_i=0, m_s=0; m_stmp[0] = '\0';\
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
         for (m_i=0; m_i<SUMA_MIN_PAIR(n, m_nr);++m_i) valv[m_i] = m_iv[m_i];  \
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

#define NI_SET_DOUBLE(ngr, name, val)  {\
   char m_stmp[100]; sprintf(m_stmp,"%f", val);   \
   NI_set_attribute(ngr, name, m_stmp);  \
}

#define NI_GET_DOUBLE(ngr, name, val)  {\
   char *m_s = NI_get_attribute(ngr, name);  \
   if (m_s) { NI_GOT = 1; val = strtod(m_s,NULL); } else { NI_GOT = 0; val = 0.0; }\
}

#define NI_SET_DOUBLEv(ngr, name, valv, n) {\
   char m_stmp[400]; int m_i=0, m_s=0;  m_stmp[0] = '\0';\
   for (m_i=0; m_i<n && m_s < 350; ++m_i) { \
      sprintf(m_stmp+m_s, " %f", valv[m_i]);   \
      m_s = strlen(m_stmp);  \
      if (m_s >= 350) { SUMA_S_Warn("Too long a vector, might get truncated"); }\
   }\
   NI_set_attribute(ngr, name, m_stmp);  \
}

#define NI_GET_DOUBLEv(ngr, name, valv, n, verb) {\
   char *m_s = NI_get_attribute(ngr, name);  \
   int m_nr, m_i; double *m_fv;  \
   for (m_i=0; m_i<n; ++m_i) valv[m_i] = 0.0;   \
   if (m_s) {  \
      NI_GOT = 1; \
      m_fv = (double *)SUMA_strtol_vec(m_s, n, &m_nr, SUMA_double, NULL); \
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
   {   \
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
            dset->inel =  SUMA_FindDsetDatumIndexElement(dset); \
         }\
      }  \
      /* close the stream */  \
      NI_stream_close( m_ns ) ; \
   }  \
}

#define DSET_FIND(id) (SUMA_FindDset_s(id, SUMAg_CF->DsetList))

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
      fprintf(m_fp,"# Output from %s, %d values (%d per line).", \
                     FuncName, Nel, m);  \
      for (m_kkk=0; m_kkk<Nel; ++m_kkk) { \
         if (!(m_kkk % m)) fprintf(m_fp,"\n"); \
         fprintf(m_fp,"%d   ", (int)v[m_kkk]); }\
      fclose(m_fp); \
   }  \
}
#define SUMA_WRITE_INT_ARRAY_AND_FLAG_1D(v,Nel,m,name,flg){  \
   int m_kkk; \
   FILE * m_fp=NULL;\
   m_fp = (name) ? fopen((name),"w"): fopen("yougavemenoname","w");  \
   if (m_fp) { \
      fprintf(m_fp,"# Output from %s, %d values (%d per line).", \
                     FuncName, Nel, m);  \
      for (m_kkk=0; m_kkk<Nel; ++m_kkk) { \
         if (!(m_kkk % m)) {  \
            if (m_kkk) fprintf(m_fp,"%d   ", flg); \
            fprintf(m_fp,"\n"); \
         }  \
         fprintf(m_fp,"%d   ", (int)v[m_kkk]); }\
         fprintf(m_fp,"%d   ", flg); \
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
#define SUMA_NEWDSET_ID_LABEL(dset, prefix) {\
   if (dset) { \
      if (!SUMA_NewDsetID (dset))  { \
         SUMA_SL_Err("Failed in SUMA_NewDsetID, proceeding..."); }  \
      if (!SUMA_LabelDset(dset, prefix)) { \
         SUMA_SL_Err("Failed in SUMA_LabelDset, proceeding..."); }  \
   } else {\
      SUMA_SL_Err("NULL dset");  \
   }  \
}

#define SUMA_NEWDSET_ID_LABEL_HIST(dset, prefix) {\
   if (dset) { \
      SUMA_NEWDSET_ID_LABEL(dset, prefix);   \
      if (!SUMA_AddNgrHist (dset->ngr, FuncName, argc, argv)) { \
         SUMA_SL_Err("Failed in SUMA_AddNgrHist, proceeding..."); } \
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
   static char *m_ATR_LIST[64] = { \
      "COLMS_STATSYM", "FDRCURVE",  \
       NULL }; \
   if (!SUMA_CopyDsetAttributes (odset, ndset, m_ATR_LIST, io, in)) {   \
      SUMA_S_Err("Failed to copy dset attributes");   \
   }  \
}
#define SUMA_COPY_DSET_ALL_COL_ATTRIBUTES(odset, ndset) {   \
   int m_i=-1;\
   if (SDSET_VECNUM(odset) != SDSET_VECNUM(ndset)) {\
      SUMA_S_Err("Mismatch in number of columns");   \
   } \
   for (m_i=0; m_i<SDSET_VECNUM(dset); ++m_i) { \
      SUMA_COPY_DSET_COL_ATTRIBUTES(odset, ndset, m_i, m_i);   \
   }  \
}

#define SUMA_COPY_DSETWIDE_ATTRIBUTES(odset, ndset) {   \
   static char *m_ATR_LIST[64] = { \
      "TR",  \
      "AFNI_labeltable",   \
       NULL }; \
   if (!SUMA_CopyDsetAttributes (odset, ndset, m_ATR_LIST, -1, -1)) {   \
      SUMA_S_Err("Failed to copy dset attributes");   \
   }  \
}

/*!
   A typical check on the output status of a selected prefix for
   surface datasets
*/
#define SUMA_CHECK_OUTPUT_SDSET_STATUS(Opref, InName, oform, pre, app, exists) {\
   char *ooo = SUMA_OutputDsetFileStatus(Opref, InName, \
                                          &(oform), pre, app, &(exists)); \
   if (exists && !THD_ok_overwrite()) {   \
      SUMA_S_Errv("Output file %s already exists.\n"  \
                  "Pick another prefix or add -overwrite\n", ooo);   \
      exit(1); \
   }  \
   if (ooo) SUMA_free(ooo); ooo=NULL;\
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
NI_element *SUMA_FindGDsetNodeListElement(SUMA_DSET *dset);
NI_element *SUMA_AddGDsetNodeListElement(SUMA_DSET *dset, 
                                         int *I, float *X, float *Y, float *Z, 
                                         char **names, int *cln, float *cols,
                                         int N_Node);
NI_element *SUMA_FindDsetDatumIndexElement(SUMA_DSET *dset);
NI_element *SUMA_FindSDsetNodeIndexElement(SUMA_DSET *dset);
NI_element *SUMA_FindGDsetEdgeIndexElement(SUMA_DSET *dset);
NI_element *SUMA_FindDsetAttributeElement(SUMA_DSET *dset, char *attname);
NI_element *SUMA_FindNgrAttributeElement(NI_group *ngr, char *attname);
NI_element *SUMA_FindNgrDataElement(NI_group *ngr, char *nelname, 
                                    char *typename);
float SUMA_LatestVersionNumber(void);
int SUMA_IcoNums(int depth, byte bin, char what);
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
int SUMA_AddDsetIndexCol(SUMA_DSET *dset, int *icolu, int *icolp1, int *icolp2);
int SUMA_AddGDsetNelXYZCol ( SUMA_DSET *dset, char *col_label, 
                               SUMA_COL_TYPE ctp, void *col, 
                               void *col_attr, int stride);
int SUMA_AddDsetNelIndexCol ( SUMA_DSET *dset, char *col_label, 
                               SUMA_COL_TYPE ctp, void *col, 
                               void *col_attr, int stride);
int SUMA_AddDsetNelCol ( SUMA_DSET *dset, char *col_label, 
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride);
int SUMA_InsertDsetNelCol ( SUMA_DSET *dset, char *col_label, 
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride, int icol);
int SUMA_AddNelCol ( NI_element *nel, char *col_label,
                     SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride);
int SUMA_AddDsetColAttr (SUMA_DSET *dset, char *col_label, 
                     SUMA_COL_TYPE ctp, void *col_attr, 
                     int col_index, int insert_mode);
int SUMA_AddDsetNodeIndexColAttr (SUMA_DSET *dset, char *col_label, 
                     SUMA_COL_TYPE ctp, void *col_attr );
int SUMA_AddGDsetNodeXYZColAttr (SUMA_DSET *dset, char *col_label, 
                                 SUMA_COL_TYPE ctp, void *col_attr);
int SUMA_AddColAttr (NI_element *nel, char *col_label,
                     SUMA_COL_TYPE ctp, void *col_attr, int col_index);
SUMA_Boolean SUMA_isMultiColumnAttr(NI_element *nel);
SUMA_Boolean SUMA_isSingleColumnAttr(NI_element *nel, int *icolb, char *rtname);
SUMA_Boolean SUMA_isDsetwideColumnAttr(NI_element *nel);
SUMA_Boolean SUMA_isDsetNelAttr(NI_element *nel);
char * SUMA_CreateDsetColRangeCompString( SUMA_DSET *dset, int col_index, 
                                          SUMA_COL_TYPE ctp);
int SUMA_UpdateDsetColRange(SUMA_DSET *dset, int icol);
int SUMA_UpdateDsetColLabel(SUMA_DSET *dset, int icol, char *label);
char * SUMA_GetDsetColStringAttr( SUMA_DSET *dset, int col_index, 
                                    char *attrname);
char * SUMA_GetNgrColStringAttr( NI_group *ngr, int col_index, 
                                 char *attrname);
SUMA_Boolean SUMA_ParseAttrName(NI_element *nel, int *tp, 
                                 int *icol, char *rtname);
SUMA_Boolean SUMA_CopyDsetAttributes ( SUMA_DSET *src, SUMA_DSET *dest,
                                       char **attrlist, 
                                       int isrc, int idest );
                                       
/* A quick way to check graphinity. Use SUMA_isGraphDset for safety */
#define SUMA_isGraphDset_fast(dset) ( ((dset)->Aux->isGraph==GRAPH_DSET) ) 
#define SUMA_isTractDset_fast(dset) ( ((dset)->Aux->isGraph==TRACT_DSET) ) 
#define SUMA_isCIFTIDset_fast(dset) ( ((dset)->Aux->isGraph==CIFTI_DSET) ) 
#define SUMA_isMD_Dset_fast(dset) ( ((dset)->Aux->isGraph==MD_DSET) ) 
byte SUMA_isGraphDset(SUMA_DSET *dset);
byte SUMA_isGraphDsetNgr(NI_group *ngr);
byte SUMA_isTractDset(SUMA_DSET *dset);
byte SUMA_isTractDsetNgr(NI_group *ngr);
byte SUMA_isMD_Dset(SUMA_DSET *dset);
byte SUMA_isCIFTIDset(SUMA_DSET *dset);
byte SUMA_isCIFTIDsetNgr(NI_group *ngr);
SUMA_Boolean SUMA_Add_Dset_Aux(SUMA_DSET *dset);
SUMA_Boolean SUMA_NewDsetGrp (SUMA_DSET *dset, SUMA_DSET_TYPE dtp, 
                           char* MeshParent_idcode, 
                          char * geometry_parent_idcode, 
                          int N_el, int N_eel,
                          char *filename, char *thisidcode);
NI_element * SUMA_NewNel (SUMA_DSET_TYPE dtp, char* MeshParent_idcode, 
                          char * geometry_parent_idcode, int N_el, 
                          char *name, char *thisidcode);
SUMA_DSET_FORMAT SUMA_Dset_Format (char *Name);
long SUMA_sdset_dnel_size(SUMA_DSET *dset);
char * SUMA_Dset_Format_Name (SUMA_DSET_FORMAT fr);
char *SUMA_HistString (char *CallingFunc, int N_arg, char **arg, char *sold);
char * SUMA_GetNgrHist(NI_group *ngr);
int SUMA_AddNgrHist(NI_group *ngr, char *CallingFunc, int N_arg, char **arg);
int SUMA_RemoveNgrHist(NI_group *ngr);
int SUMA_RemoveDsetHist(SUMA_DSET *dset);
int SUMA_AddNelHist(NI_element *nel, char *CallingFunc, int N_arg, char **arg);
void SUMA_FreeDset(void *dset);
SUMA_Boolean SUMA_FreeDsetContent (SUMA_DSET *dset);
SUMA_DSET * SUMA_FindDset_ns (char *idcode_str, DList *DsetList);
SUMA_DSET * SUMA_FindDset2_ns (char *idcode_str, DList *DsetList, char *itype);
DListElmt * SUMA_FindDsetEl_ns (char *idcode, DList *DsetList);
SUMA_DSET * SUMA_FindDset_eng (char *idcode_str, DList *DsetList, 
                                 DListElmt **elp, char *itype);
char *SUMA_DsetInfo (SUMA_DSET *dset, int detail);
void SUMA_ShowDset (SUMA_DSET *dset, int detail, FILE *out);
char *SUMA_ShowMeSome (void *dt, SUMA_VARTYPE tp, int N_dt, 
                       int mxshow, char *title);
SUMA_DSET * SUMA_NewDsetPointer(void);
SUMA_DSET * SUMA_CreateDsetPointer (  
                              char *name, 
                              SUMA_DSET_TYPE tp,
                              char *idcode_str,
                              char *domain_idcode_str,
                              int N_Alloc); 
SUMA_DSET * SUMA_CreateFullDsetPointer (  
                              char *filename, SUMA_DSET_TYPE tp,
                              char *idcode,
                              char *domain_idcode,
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
int SUMA_Float2DsetCol (SUMA_DSET *dset, int ind, float *V, int FilledOnly, 
                        byte *replacemask);
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
NI_element * SUMA_GetUniqueIndicesAttr(SUMA_DSET *dset, int iindex);
int * SUMA_GetUniqueIndicesVec(SUMA_DSET *dset, int iindex);
int * SUMA_GetDatasetDimensions(SUMA_DSET *dset);
float * SUMA_GetDatasetFactors(SUMA_DSET *dset);
MRI_TYPE SUMA_GetBrickType(SUMA_DSET *dset, int ii);
float SUMA_GetBrickFactor(SUMA_DSET *dset, int ii);
float * SUMA_GetDatasetI2X(SUMA_DSET *dset, float M[4][4]);
int SUMA_isVolDataset(SUMA_DSET *dset);
NI_element * SUMA_GetAtlasLabelTable(SUMA_DSET *dset);
NI_element * SUMA_GetValueLabelTable(SUMA_DSET *dset);
SUMA_Boolean SUMA_SetUniqueIndicesAttr(SUMA_DSET *dset, byte replace);
int SUMA_GetDsetColRange(SUMA_DSET *dset, int col_index, 
                         double range[2], int loc[2]);
int SUMA_GetDsetNodeIndexColRange(SUMA_DSET *dset, 
                                  double range[2], int loc[2], int addifmissing);
int SUMA_GetColRange(NI_element *nel, int col_index, 
                     double range[2], int loc[2]);
int SUMA_AddGenDsetColAttr (SUMA_DSET *dset, SUMA_COL_TYPE ctp, void *col, 
                            int stride, int col_index, int insert_mode);
int SUMA_AddGenDsetNodeIndexColAttr (SUMA_DSET *dset, SUMA_COL_TYPE ctp, 
                                     void *col, int stride) ;
int SUMA_AddGenGDsetNodeXYZColAttr (SUMA_DSET *dset, SUMA_COL_TYPE ctp, 
                                     void *col, int stride) ;
int SUMA_AddGenColAttr (NI_element *nel, SUMA_COL_TYPE ctp, void *col, 
                        int stride, int col_index); 
SUMA_DSET *SUMA_LoadNimlDset (char *Name, int verb);
SUMA_DSET *SUMA_LoadGIFTIDset (char *Name, int verb);
SUMA_DSET *SUMA_LoadDset_eng (char *Name, SUMA_DSET_FORMAT *form, int verb);
SUMA_DSET *SUMA_LoadDset_ns (char *Name, SUMA_DSET_FORMAT *form, int verb);
SUMA_DSET *SUMA_Load1DDset_eng (char *Name, int verb);
SUMA_DSET *SUMA_Load1DDset_ns (char *Name, int verb);
SUMA_DSET *SUMA_LoadDXDset_eng (char *Name, int verb);
SUMA_DSET *SUMA_LoadDXDset_ns (char *Name, int verb);
char *SUMA_RemoveDsetExtension_ns (char*Name, SUMA_DSET_FORMAT form);
char *SUMA_RemoveDsetExtension_eng (char*Name, SUMA_DSET_FORMAT *form);
char * SUMA_WriteDset_ns (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, 
                          int overwrite, int verb); 
int SUMA_WriteDset_NameCheck_ns (char *Name, SUMA_DSET *dset, 
                                 SUMA_DSET_FORMAT form, int verb, 
                                 char **NameOutp); 
int SUMA_WriteDset_NameCheck_eng (char *Name, SUMA_DSET *dset, 
                                  SUMA_DSET_FORMAT form, int verb, 
                                  char **NameOutp); 
char * SUMA_WriteDset_eng (char *Name, SUMA_DSET *dset, SUMA_DSET_FORMAT form, 
                           int overwrite, int verb, int rename_autoid); 
SUMA_DSET * SUMA_far2dset_eng( char *FullName, char *dset_id, char *dom_id, 
                                 float **farp, int vec_len, int vec_num, 
                                 int ptr_cpy);
SUMA_DSET * SUMA_far2dset_ns( char *FullName, char *dset_id, char *dom_id, 
                                 float **farp, int vec_len, int vec_num, 
                                 int ptr_cpy);
SUMA_DSET * SUMA_iar2dset_eng( char *FullName, char *dset_id, char *dom_id, 
                                 int **farp, int vec_len, int vec_num, 
                                 int ptr_cpy);
SUMA_DSET * SUMA_iar2dset_ns( char *FullName, char *dset_id, char *dom_id, 
                                 int **farp, int vec_len, int vec_num, 
                                 int ptr_cpy);
int SUMA_is_AllNumeric_dset(SUMA_DSET *dset);
int SUMA_dset_to_Label_dset(SUMA_DSET *dset); 
int SUMA_is_Label_dset(SUMA_DSET *dset, NI_group **NIcmap); 
int SUMA_is_Label_dset_col(SUMA_DSET *dset, int icol);
int SUMA_is_Phase_dset(SUMA_DSET *dset); 
int SUMA_is_RetinoAngle_dset(SUMA_DSET *dset); 
int SUMA_is_VFR_dset(SUMA_DSET *dset); 
NI_group *SUMA_NICmapToNICmap(NI_group *NIcmap);
int * SUMA_UniqueValuesInLabelDset(SUMA_DSET *dset, int *N_unq);
int SUMA_is_AllConsistentNumeric_dset(SUMA_DSET *dset, SUMA_VARTYPE *vtpp);
int SUMA_GetConsistentColType_dset(SUMA_DSET *dset);
int SUMA_is_AllConsistentColType_dset(SUMA_DSET *dset, SUMA_COL_TYPE ctpi); 
int SUMA_is_AllConsistentCastType_dset(SUMA_DSET *dset, int typecast);
int SUMA_is_AllNumeric_ngr(NI_group *ngr) ;
int SUMA_is_AllNumeric_nel(NI_element *nel);
int SUMA_is_TimeSeries_dset(SUMA_DSET *dset, double *TRp);
SUMA_Boolean SUMA_SetDsetTR(SUMA_DSET *dset, double TR);
SUMA_Boolean SUMA_NewDsetID (SUMA_DSET *dset);
SUMA_Boolean SUMA_NewDsetID2 (SUMA_DSET *dset, char *str);
char *SUMA_DsetColStringAttrCopy(SUMA_DSET *dset, int i, 
                                 int addcolnum, char *attrname);
char *SUMA_DsetColLabel(SUMA_DSET *dset, int i);
char *SUMA_DsetColLabelCopy(SUMA_DSET *dset, int i, int addcolnum);
int SUMA_FindDsetColLabeled(SUMA_DSET *dset, char *label);
int SUMA_FindNelColLabeled(NI_element *nelb, char *label);
char **SUMA_AllDsetColLabels(SUMA_DSET *dset);
char **SUMA_FreeAllDsetColLabels(char **);
char *SUMA_ColLabelCopy(NI_element *nel, int i, int addcolnum);
int SUMA_FloatScanDset ( SUMA_DSET *odset, int doNan, int doInf, 
                         int zeroout, int fixrange);
SUMA_Boolean SUMA_Reset_NodeIndex_Element(SUMA_DSET *dset, NI_element **inel);
SUMA_DSET * SUMA_PaddedCopyofDset ( SUMA_DSET *odset, int MaxNodeIndex );
SUMA_DSET * SUMA_MaskedCopyofDset(SUMA_DSET *odset, 
                                  byte *rowmask, byte *colmask, 
                                  int masked_only, int keep_node_index);
SUMA_DSET *SUMA_CoercedCopyofDset( SUMA_DSET *odset, SUMA_VARTYPE vtp,
                                   byte *colmask);
SUMA_DSET * SUMA_MaskedByOrderedNodeIndexCopyofDset(
      SUMA_DSET *odset, int *indexlist, 
      int N_indexlist, byte *colmask, 
      int masked_only, int keep_node_index);
SUMA_DSET * SUMA_MaskedByNodeIndexCopyofDset(SUMA_DSET *odset, int *indexlist, 
                     int N_indexlist, byte *colmask, 
                     int masked_only, int keep_node_index);
SUMA_DSET * SUMA_VcatDset(SUMA_DSET *odset, 
                           byte *rowmask, byte *colmask, 
                           int masked_only, int keep_node_index);
SUMA_Boolean SUMA_Append_Copy_Part_Column(void *col, NI_rowtype *rt, int N_col, 
                                 byte *rowmask, int masked_only, 
                                 void **appendhere, int *append_rowtype_code, 
                                 int *n_inappendhere);
void *SUMA_Copy_Part_Column(void *col,  NI_rowtype *rt, int N_col, 
                            byte *rowmask, int masked_only, int *n_incopy);
char* SUMA_sdset_id(SUMA_DSET *dset);
char *SUMA_sdset_label(SUMA_DSET *dset);
char *SUMA_sdset_filename(SUMA_DSET *dset);
char* SUMA_sdset_idmdom(SUMA_DSET *dset);
char *SUMA_Dset_orcode(SUMA_DSET *dset);
SUMA_DATUM_LEVEL SUMA_sdset_datum_level(SUMA_DSET *dset);
SUMA_Boolean SUMA_sdset_set_datum_level(SUMA_DSET *dset, SUMA_DATUM_LEVEL lvl);
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
void * SUMA_OpenDx_Object_Header_Field(char *op, int nchar, const char *attr, 
                                       char **op_end);
SUMA_Boolean SUMA_OpenDx_Object_Data(char *op, int nchar, 
                                     SUMA_OPEN_DX_STRUCT *dx);
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
SUMA_Boolean SUMA_RenameDset(SUMA_DSET *dset, char *filename, int autoid);
byte *SUMA_load_1D_n_mask(char *name, int N_Node, byte *omask, 
                           const char *oper, int *N_inmask);
byte * SUMA_indexlist_2_bytemask(int *ind_list, int N_ind_list, 
                                 int N_mask, int *N_inmask);  
byte * SUMA_Meshbmask_2_IndexListbmask(byte *Mbmask, int N_Mbmask, 
                                 int *ind_list, int N_ind_list, int *N_ILbmask);
byte *SUMA_load_1D_b_mask(char *name, int N_Node, byte *omask, 
                          const char *oper, int *N_inmask);
byte *SUMA_get_c_mask(char *mask, int N_Node, byte *omask, 
                      const char *oper, int *N_inmask);
byte * SUMA_load_all_command_masks(char *bmaskname, char *nmaskname, 
                                   char *cmask, int N_Node, int *N_inmask);
void SUMA_SetAddIndex_1D(int);
int SUMA_GetAddIndex_1D(void);
THD_3dim_dataset *SUMA_sumadset2afnidset(SUMA_DSET **dsetp, int copy_data, 
                                         int cleardset);
SUMA_DSET *SUMA_afnidset2sumadset(THD_3dim_dataset **dsetp, int copy_data, 
                                  int cleardset, int floatize);
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

#define SUMA_DSET_NAME_CHECK(prefix) { \
   char *NameOut=NULL; SUMA_DSET_FORMAT form=SUMA_NO_DSET_FORMAT; \
   if (!THD_ok_overwrite()) { \
      form = SUMA_GuessFormatFromExtension(prefix, "jeveux.niml.dset" );  \
      if (SUMA_WriteDset_NameCheck_s (prefix, NULL, form,  \
                                  0, &NameOut)) {   \
         SUMA_S_Errv("Dset %s already exists\n", NameOut); \
         SUMA_free(NameOut); \
         exit(1);  \
      }  \
   } if (NameOut) SUMA_free(NameOut); \
}

SUMA_DSET_FORMAT SUMA_FormatFromFormString(char *arg);
SUMA_DSET_FORMAT SUMA_GuessFormatFromExtension(char *Name, char *fallbackname);
const char *SUMA_ExtensionOfDsetFormat (SUMA_DSET_FORMAT form);
char *SUMA_OutputDsetFileStatus(char *prefix, char *inname, 
                            SUMA_DSET_FORMAT *oform, 
                            char *pre, char *app, int *exists);
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
 

/******** BEGIN functions for surface structure  ******************** */
SUMA_SO_SIDE SUMA_giiStringToNumSide(char *cc);
void SUMA_ShowAfniSurfaceObject(NI_group *aSO, FILE *out,
                              int detail, char *title);
char *SUMA_AfniSurfaceObject_Info(NI_group *aSO, 
                                  int detail, char *title);
NI_group * afni_open_gifti_surf(char * fname, int read_data);
int afni_write_gifti_surf( NI_group *aSO, char * fname, 
                           int write_data, int encoding);


/******** END functions for surface structure  ******************** */
int SUMA_init_GISET_setup(NI_stream nsg , NI_element *nel, GICOR_setup *giset,
                          int bmode);   /* 17 Aug 2012 [rickr] */
int SUMA_PopulateDsetsFromGICORnel(NI_element *nel, GICOR_setup *giset, 
                                   SUMA_DSET **sdsetv);
const char *SUMA_ObjectTypeCode2ObjectTypeName(SUMA_DO_Types dd);
SUMA_DO_Types SUMA_ObjectTypeName2ObjectTypeCode(char *cc);
#define SUMA_otn2otc SUMA_ObjectTypeName2ObjectTypeCode
#define SUMA_otc2otn SUMA_ObjectTypeCode2ObjectTypeName

/************************ GRAPH Dset functions  ******************** */

/*! Return the 1D (p) index corresponding to the compact
    columnwise storage of a triangular matrix
    Use the R test function ij2p in CompactIndexing.R
    for details and tests. 
    Note, we pass two_n instead of n, the dimension of the matrix
    for speed.
    i and j, must be integers or floor operation will fail
*/
#define SUMA_CItri_ij2p_diag(i, j, two_n) ((i+(two_n-(j)-1)*(j))/2)
#define SUMA_CItri_ij2p(i, j, two_n)      ((i-1+(two_n-(j)-3)*(j))/2)
#define SUMA_CItri_pmax_diag(n) (((n)*((n)+1))/2-1)
#define SUMA_CItri_pmax(n)      (((n)*((n)-1))/2-1)

SUMA_DSET *SUMA_FloatVec_to_GDSET(float **vec, int vec_num, int vec_len, 
                                  char *mtype, 
                                  char **vec_labs, int *ie, int *i0, int *i1);
SUMA_Boolean SUMA_Dset_to_GDSET(SUMA_DSET **dset, char *mtype,  
                                int ok_verticalize, int *ie, int *i0, int *i1);
byte SUMA_CItri_p2ij(int p, int n, int two_n, byte withdiag, int *i, int *j);
SUMA_Boolean SUMA_GDSET_Set_Aux_matrix_shape(SUMA_DSET *dset);
byte SUMA_GDSET_SegIndexToPoints(SUMA_DSET *dset, int si, 
                                 int *i1, int *i2, int *row);
byte SUMA_GDSET_PointsToSegIndex(SUMA_DSET *dset, int i1, int i2, int *si);
byte SUMA_GDSET_SegRowToPoints(SUMA_DSET *dset, int ri, 
                                 int *i1, int *i2, int *index);
byte SUMA_GDSET_PointsToSegRow(SUMA_DSET *dset, int i1, int i2, int *ri);
byte SUMA_GDSET_PointToDiagSegRowIndex(SUMA_DSET *dset,int i1, int *ri, int *si);
byte SUMA_GDSET_PointToDiagSegIndex(SUMA_DSET *dset, int i1, int *si);
byte SUMA_GDSET_PointToDiagSegRow(SUMA_DSET *dset, int i1, int *ri);
SUMA_SQ_MATRIX_SHAPES SUMA_matrix_shape_name_to_matrix_shape(char *name); 
char * SUMA_matrix_shape_to_matrix_shape_name(SUMA_SQ_MATRIX_SHAPES sq);
int *SUMA_GDSET_GetPointIndexColumn(SUMA_DSET *dset, int *N_vals, NI_element **);
char **SUMA_GDSET_GetPointNamesColumn(SUMA_DSET *dset, int *N_vals, 
                                    NI_element **nelxyzr);
float *SUMA_GDSET_GetPointColumn_f(SUMA_DSET *dset, int *N_vals, 
                                      NI_element **nelxyzr, char *label);
int *SUMA_GDSET_GetPointGroupColumn(SUMA_DSET *dset, int *N_vals, 
                                      NI_element **nelxyzr);
int SUMA_GDSET_Index_To_NodeIndex(SUMA_DSET *dset, int cinode);
int SUMA_GDSET_NodeIndex_To_Index(SUMA_DSET *dset, int node);
int SUMA_GDSET_EdgeIndex_To_Row(SUMA_DSET *dset, int ei);
int SUMA_GDSET_EdgeRow_To_Index(SUMA_DSET *dset, int ri);
int SUMA_GDSET_Max_Edge_Index(SUMA_DSET *dset);
char *SUMA_GDSET_Node_Label(SUMA_DSET *dset, int psel);
char *SUMA_GDSET_Edge_Label(SUMA_DSET *dset, int isel, char *pref, char *sep);

/************************ CIFTI Dset functions ******************** */
SUMA_Boolean SUMA_CIFTI_Set_Domains(SUMA_DSET *dset, int N_doms, 
                                    int *dind, int *dindoff, int *dn,
                                    SUMA_DO_Types *dtp, char **dsrcs);
SUMA_DSET *SUMA_CIFTI_2_edset(SUMA_DSET *dset, int i, byte *colmask, 
      	             	      DList *DsetList, int allowreplace);
SUMA_Boolean SUMA_CIFTI_NgrFromDomains(SUMA_DSET *dset);
SUMA_Boolean SUMA_CIFTI_DomainsFromNgr(SUMA_DSET *dset, DList *DsetList, 
      	             	      	       int allowreplace, SUMA_DSET **ret_edset);
SUMA_Boolean SUMA_CIFTI_free_MD_data(SUMA_DSET *dset);
SUMA_Boolean SUMA_CIFTI_Free_Doms(SUMA_DSET *dset);
#endif
