#ifndef SUMA_SURFCLUST_INCLUDED
#define SUMA_SURFCLUST_INCLUDED

typedef enum { SUMA_SORT_CLUST_NOT_SET, SUMA_SORT_CLUST_NO_SORT, SUMA_SORT_CLUST_BY_NUMBER_NODES, SUMA_SORT_CLUST_BY_AREA } SUMA_SURF_CLUST_SORT_MODES;
/* structures to be used by surface clusters functions */
#define SURFCLUST_MAX_SURF 1  /*!< Maximum number of surfaces allowed for SurfClust*/
typedef struct {
   SUMA_SO_File_Type iType;
   char *sv_name;
   char *surf_names[SURFCLUST_MAX_SURF];
   int N_surf;
   char *spec_file;
   char *in_name;
   char *surftype;
   int nodecol;
   int labelcol;
   char *out_prefix;   /* this one's dynamically allocated so you'll have to free it yourself */
   float DistLim;
   float AreaLim;
   int NodeLim;
   int DoThreshold;
   float Thresh;
   int tind;
   float update;
   int DoCentrality;
   SUMA_Boolean OutROI;
   SUMA_Boolean OutClustDset;
   SUMA_Boolean WriteFile;
   SUMA_SURF_CLUST_SORT_MODES SortMode;
   SUMA_Boolean FullROIList;
   SUMA_Boolean prepend_node_index;
   SUMA_DSET_FORMAT oform;
} SUMA_SURFCLUST_OPTIONS;

typedef struct {
   int N_Node; /*!< number of nodes in this cluster */
   int *NodeList; /*!< list of node (indices) in this cluster. Allocated space
                        is always for more than N_Node elements. But 
                        values beyond NodeList[N_Node-1] are of no meaning. */
   float *ValueList; /*!< list of corresponding node values. */
   float totalarea;
   float totalvalue;
   float totalabsvalue;
   float minvalue;
   float maxvalue;
   float varvalue;
   int minnode;
   int maxnode;
   int centralnode;
   int weightedcentralnode;
   /* int rank; */  /* completely useless ... rank is inferred from order in list! */
} SUMA_CLUST_DATUM;

SUMA_Boolean SUMA_Show_SurfClust_list(DList *list, FILE *Out, int detail, char *params); 
char *SUMA_Show_SurfClust_list_Info(DList *list, int detail, char *params); 
void SUMA_FreeClustDatum (void * data);
DList *SUMA_FindClusters ( SUMA_SurfaceObject *SO, int *ni, float *nv, int N_ni, 
                           int dothisnode, SUMA_SURFCLUST_OPTIONS *Opt, 
                           float *NodeArea);
                           
SUMA_DSET *SUMA_MaskDsetByClustList(SUMA_DSET *idset, SUMA_SurfaceObject *SO, 
                     DList *list, SUMA_Boolean FullList, char *leName); 
SUMA_DSET *SUMA_SurfClust_list_2_DsetMask(SUMA_SurfaceObject *SO, 
                     DList *list, SUMA_Boolean FullList, char *leName);
int SUMA_ClusterCenterofMass  (SUMA_SurfaceObject *SO, SUMA_CLUST_DATUM *cd, int UseSurfDst);
SUMA_CLUST_DATUM * SUMA_Build_Cluster_From_Node(int dothisnode, SUMA_CLUST_DATUM *AddToThisClust, 
                                                float *ToBeAssigned, int *N_TobeAssigned, float *NodeArea,
                                                SUMA_SurfaceObject *SO, SUMA_SURFCLUST_OPTIONS *Opt);
SUMA_CLUST_DATUM * SUMA_Build_Cluster_From_Node_NoRec    (  int dothisnode, 
                                                            float *ToBeAssigned, int *N_TobeAssigned, float *NodeArea,
                                                            SUMA_SurfaceObject *SO, SUMA_SURFCLUST_OPTIONS *Opt   );

#endif
