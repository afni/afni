#ifndef SUMA_WORLDSTAT_INCLUDED
#define SUMA_WORLDSTAT_INCLUDED

typedef enum {    SUMA_NO_BUILD_METHOD, SUMA_OFFSETS2, SUMA_OFFSETS_LL, SUMA_OFFSETS2_NO_REC, SUMA_MAXIMA } SUMA_CLUST_BUILD_METHODS;

typedef enum {
   SUMA_NO_GRAD_SCALE, SUMA_MEAN_GRAD_SCALE, SUMA_GMEAN_GRAD_SCALE,
} SUMA_GRAD_SCALE_OPTS;

typedef enum { SUMA_MINIMUS=-1, SUMA_EXTREMUS, SUMA_MAXIMUS } 
                                    SUMA_EXTREMA_DIRECTIONS;

void SUMA_FreeClustDatum (void * data);
float *SUMA_CalculateNodeAreas(SUMA_SurfaceObject *SO, byte *mask);
SUMA_CLUST_DATUM * SUMA_Build_Cluster_From_Node(
      int dothisnode, SUMA_CLUST_DATUM *AddToThisClust, 
      float *ToBeAssigned, int *N_TobeAssigned, float *NodeArea,
      SUMA_SurfaceObject *SO, SUMA_SURFCLUST_OPTIONS *Opt);
SUMA_CLUST_DATUM * SUMA_Build_Cluster_From_Node_NoRec    (  
   int dothisnode, 
   float *ToBeAssigned, int *N_TobeAssigned, float *NodeArea,
   SUMA_SurfaceObject *SO, SUMA_SURFCLUST_OPTIONS *Opt   );
DList *SUMA_FindClusters ( SUMA_SurfaceObject *SO, int *ni, float *nv, int N_ni, 
                           int dothisnode, SUMA_SURFCLUST_OPTIONS *Opt, 
                           float *NodeArea);
SUMA_Boolean SUMA_Show_SurfClust_list(DList *list, FILE *Out, int detail, 
                                      char *params, char *opts) ;
char *SUMA_Show_SurfClust_list_Info(DList *list, int detail, 
                                       char *params, char *opts) ;
NI_element *SUMA_SurfClust_list_2_nel(DList *list, int detail, char *params, 
                                      char *opts);
SUMA_DSET *SUMA_MaskDsetByClustList(SUMA_DSET *idset, SUMA_SurfaceObject *SO, 
                     DList *list, SUMA_Boolean FullList, char *leName); 
SUMA_DSET *SUMA_SurfClust_list_2_DsetMask(SUMA_SurfaceObject *SO, 
                     DList *list, SUMA_Boolean FullList, char *leName);
byte * SUMA_ClustList2Mask(DList *list, int NodeMax);
int SUMA_ClusterCenterofMass  (SUMA_SurfaceObject *SO, SUMA_CLUST_DATUM *cd, int UseSurfDist);
SUMA_Boolean SUMA_Sort_ClustersList (DList *list, SUMA_SURF_CLUST_SORT_MODES SortMode);
SUMA_DSET *SUMA_CalculateLocalStats(
   SUMA_SurfaceObject *SO, SUMA_DSET *din, 
   byte *nmask, byte strict_mask,
   float rhood, SUMA_OFFSET_STRUCT *UseThisOffset,
   int ncode, int *code, 
   SUMA_DSET *UseThisDout, int NodeDebug,
   SUMA_SurfaceObject *SOf);
SUMA_DSET *SUMA_DsetAvgGradient(
   SUMA_SurfaceObject *SO, float **dists, SUMA_DSET *din, 
   byte *nmask, byte mask_by_zeros, SUMA_GRAD_SCALE_OPTS normopt);
float *SUMA_AvgGradient(
   SUMA_SurfaceObject *SO, float **dists, float *nv, 
   byte *nmask, byte mask_by_zeros, SUMA_GRAD_SCALE_OPTS normopt);
SUMA_DSET *SUMA_DsetExtrema(
   SUMA_SurfaceObject *SO, float **FirstNeighbDist, 
   SUMA_DSET *din, SUMA_DSET *dgrad, float r, float fthresh, float gthresh,
   byte *maskp, byte mask_by_zeros, SUMA_EXTREMA_DIRECTIONS dir, char *tout);
double SUMA_GetFWHM_MinArea(void);
void SUMA_SetFWHM_MinArea(double);
int SUMA_SurfClust_Get_Method(void) ;
void SUMA_SurfClust_Set_Method(int m);
SUMA_SURFCLUST_OPTIONS *SUMA_free_SurfClust_Opt(SUMA_SURFCLUST_OPTIONS *Opt);
SUMA_SURFCLUST_OPTIONS *SUMA_create_SurfClust_Opt(char *forwhom);
char *SUMA_ClustCommandLineFromOpt(char *pname, SUMA_SurfaceObject *SO,
                           SUMA_SURFCLUST_OPTIONS *Opt,  char *filler); 
int SUMA_NodeClustNumber(SUMA_OVERLAYS *Sover, int node, 
                         SUMA_SurfaceObject *SO, 
                         SUMA_CLUST_DATUM **cdp);

#endif
