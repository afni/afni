#ifndef SUMA_DATASETS_INCLUDED
#define SUMA_DATASETS_INCLUDED

#ifndef SUMA_IDCODE_LENGTH
   #define SUMA_IDCODE_LENGTH 50
#endif

typedef enum {
   SUMA_ERROR_DSET_TYPE = -1,
   SUMA_NO_DSET_TYPE,
   SUMA_NODE_BUCKET,
   SUMA_NODE_ROI, /*!< Col0: Node ID, Col1: ROI label (int) */
   SUMA_NODE_RGB,
} SUMA_DSET_TYPE; /*!<  Type of data set 
                        When you add a new element, modify functions
                        SUMA_Dset_Type_Name
                        SUMA_Dset_Type */

typedef enum {
   SUMA_ERROR_DSET_FORMAT = -1,
   SUMA_NO_DSET_FORMAT,
   SUMA_ASCII_NIML,
   SUMA_BINARY_NIML,
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
   SUMA_NODE_X,      /*!< Node X coordinate */
   SUMA_NODE_Y,      /*!< Node Y coordinate */
   SUMA_NODE_Z,      /*!< Node Z coordinate */
   SUMA_NODE_R,      /*!< Node R color */
   SUMA_NODE_G,      /*!< Node G color */
   SUMA_NODE_B,      /*!< Node B color */
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

char * SUMA_Dset_Type_Name (SUMA_DSET_TYPE tp);
SUMA_DSET_TYPE SUMA_Dset_Type (char *Name);
char * SUMA_Col_Type_Name (SUMA_COL_TYPE tp);
SUMA_COL_TYPE SUMA_Col_Type (char *Name);
int SUMA_ShowNel (NI_element *nel);
int SUMA_AddNelCol ( NI_element *nel, SUMA_COL_TYPE ctp, void *col, 
                     void *col_attr, int stride);
int SUMA_AddColAttr (NI_element *nel, SUMA_COL_TYPE ctp, void *col_attr);
NI_element * SUMA_NewNel (SUMA_DSET_TYPE dtp, char *DomParent_idcode, 
                          char* GeomParent_idcode, int N_el);
int *SUMA_GetColIndex (NI_element *nel, SUMA_COL_TYPE tp, int *N_i);
SUMA_DSET_FORMAT SUMA_Dset_Format (char *Name);
char * SUMA_Dset_Format_Name (SUMA_DSET_FORMAT fr);
int SUMA_AddNelHist(NI_element *nel, char *CallingFunc, int N_arg, char **arg);
char *SUMA_copy_string(char *buf);
char * SUMA_append_string(char *s1, char *s2);
char * SUMA_append_replace_string(  char *s1, char *s2, 
                                    char *Spc, int whichTofree);
char * SUMA_truncate_string (char *s1, int length);



#endif
