#ifndef SUMA_WORLDSTAT_INCLUDED
#define SUMA_WORLDSTAT_INCLUDED

typedef enum { SUMA_NO_BUILD_METHOD, SUMA_OFFSETS2, SUMA_OFFSETS_LL, SUMA_OFFSETS2_NO_REC } SUMA_CLUST_BUILD_METHODS;

void SUMA_FreeClustDatum (void * data);
float *SUMA_CalculateNodeAreas(SUMA_SurfaceObject *SO, byte *mask);
SUMA_CLUST_DATUM * SUMA_Build_Cluster_From_Node(int dothisnode, SUMA_CLUST_DATUM *AddToThisClust, 
                                                float *ToBeAssigned, int *N_TobeAssigned, float *NodeArea,
                                                SUMA_SurfaceObject *SO, SUMA_SURFCLUST_OPTIONS *Opt);
SUMA_CLUST_DATUM * SUMA_Build_Cluster_From_Node_NoRec    (  int dothisnode, 
                                                            float *ToBeAssigned, int *N_TobeAssigned, float *NodeArea,
                                                            SUMA_SurfaceObject *SO, SUMA_SURFCLUST_OPTIONS *Opt   );
DList *SUMA_FindClusters ( SUMA_SurfaceObject *SO, int *ni, float *nv, int N_ni, 
                           int dothisnode, SUMA_SURFCLUST_OPTIONS *Opt, 
                           float *NodeArea);
SUMA_Boolean SUMA_Show_SurfClust_list(DList *list, FILE *Out, int detail, char *params) ;
char *SUMA_Show_SurfClust_list_Info(DList *list, int detail, char *params) ;
SUMA_DSET *SUMA_MaskDsetByClustList(SUMA_DSET *idset, SUMA_SurfaceObject *SO, DList *list, SUMA_Boolean FullList, char *leName); 
SUMA_DSET *SUMA_SurfClust_list_2_DsetMask(SUMA_SurfaceObject *SO, DList *list, SUMA_Boolean FullList, char *leName) ;
int SUMA_ClusterCenterofMass  (SUMA_SurfaceObject *SO, SUMA_CLUST_DATUM *cd, int UseSurfDist);
SUMA_Boolean SUMA_Sort_ClustersList (DList *list, SUMA_SURF_CLUST_SORT_MODES SortMode);
SUMA_DSET *SUMA_CalculateLocalStats(SUMA_SurfaceObject *SO, SUMA_DSET *din, 
                                    byte *nmask, byte strict_mask,
                                    float rhood, SUMA_OFFSET_STRUCT *UseThisOffset,
                                    int ncode, int *code, 
                                    SUMA_DSET *UseThisDout, int NodeDebug,
                                    SUMA_SurfaceObject *SOf);
double SUMA_GetFWHM_MinArea(void);
void SUMA_SetFWHM_MinArea(double);
int SUMA_SurfClust_Get_Method(void) ;
void SUMA_SurfClust_Set_Method(int m);



#endif
