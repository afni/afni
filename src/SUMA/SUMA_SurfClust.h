#ifndef SUMA_SURFCLUST_INCLUDED
#define SUMA_SURFCLUST_INCLUDED

typedef enum { SUMA_SORT_CLUST_NOT_SET, SUMA_SORT_CLUST_NO_SORT, SUMA_SORT_CLUST_BY_NUMBER_NODES, SUMA_SORT_CLUST_BY_AREA } SUMA_SURF_CLUST_SORT_MODES;
/* structures to be used by surface clusters functions */
#define SURFCLUST_MAX_SURF 10  /*!< Maximum number of surfaces allowed for SurfClust*/
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
   SUMA_Boolean OutROI;
   SUMA_Boolean WriteFile;
   SUMA_SURF_CLUST_SORT_MODES SortMode;
   SUMA_Boolean FullROIList;
} SUMA_SURFCLUST_OPTIONS;

typedef struct {
   int N_Node; /*!< number of nodes in this cluster */
   int *NodeList; /*!< list of node (indices) in this cluster. Allocated space
                        is always for more than N_Node elements. But 
                        values beyond NodeList[N_Node-1] are of no meaning. */
   float totalarea;
   /* int rank; */  /* completely useless ... rank is inferred from order in list! */
} SUMA_CLUST_DATUM;

SUMA_Boolean SUMA_Show_SurfClust_list(DList *list, FILE *Out, int detail); 
char *SUMA_Show_SurfClust_list_Info(DList *list, int detail); 
void SUMA_FreeClustDatum (void * data);
DList *SUMA_FindClusters ( SUMA_SurfaceObject *SO, int *ni, float *nv, int N_ni, 
                           DList *laliste, int dothisnode, SUMA_SURFCLUST_OPTIONS *Opt, 
                           SUMA_CLUST_DATUM *clust, float *NodeArea);
SUMA_DSET *SUMA_SurfClust_list_2_Dset(SUMA_SurfaceObject *SO, 
                     DList *list, SUMA_Boolean FullList, char *leName);
float *SUMA_CalculateNodeAreas(SUMA_SurfaceObject *SO);


#endif
