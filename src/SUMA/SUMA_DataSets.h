#ifndef SUMA_DATASETS_INCLUDED
#define SUMA_DATASETS_INCLUDED

#ifndef SUMA_IDCODE_LENGTH
   #define SUMA_IDCODE_LENGTH 50
#endif

typedef enum {
   SUMA_NO_DSET_TYPE,
   SUMA_NODE_BUCKET,
   SUMA_NODE_ROI,
   SUMA_NODE_RGB,
} SUMA_DSET_TYPE; /*!<  Type of data set 
                        When you add a new element, modify function
                        SUMA_Dset_Name*/

typedef enum {
   SUMA_NO_COL_TYPE,
   SUMA_NODE_INT,
   SUMA_NODE_INDEX,
   SUMA_NODE_FLOAT, 
   SUMA_NODE_X,
   SUMA_NODE_Y,
   SUMA_NODE_Z, 
   SUMA_NODE_R,
   SUMA_NODE_G,
   SUMA_NODE_B,
}  SUMA_COL_TYPE; /*!<  Column types.
                        When you add a new element, you need to modify
                        SUMA_AddColAttr */

char * SUMA_Dset_Name (SUMA_DSET_TYPE tp);
int SUMA_AddNelCol (NI_element *nel, SUMA_COL_TYPE ctp, void *col, void *col_attr);
int SUMA_AddColAttr (NI_element *nel, SUMA_COL_TYPE ctp, void *col_attr);
NI_element * SUMA_NewNel (SUMA_DSET_TYPE dtp, char *DomParent_idcode, 
                          char* GeomParent_idcode, int N_el);




#endif
