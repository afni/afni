#ifndef SUMA_SURFACE_TO_SURFACE_INCLUDED
#define SUMA_SURFACE_TO_SURFACE_INCLUDED

typedef struct {
  char* M1_IDcode;  
  int M1_N_Nodes; /*! Numer of nodes forming SO1
                     So far, it is the same as M1Nn */
  char* M2_IDcode;
  int M2_N_Nodes; /*! Numer of nodes forming SO2 */
  
  int M1Nn; /*!< number of node considered on M1 */
  int *M1n; /*!< vector of node indices on M1 
               M1n[j] is the index of node j (nj) on mesh M1
               j goes from 0 to M1Nn*/
  int *M2t_M1n; /*!< (M1_N_NodeIndex x 1) index of triangle in M2 
          hosting node in M1. 
          M2t_M1n[j] is the index of the triangle on M2 that hosts node nj on M1
          Hosting means that node nj projects to the triangle.*/ 
  float *M2pb_M1n; /*!< Barycentric coordinates (u, v) of the projection of 
                        node nj of M1 on triangle M2t_M1n[j] 
                        M2pb_M1n[2*j+0] = uj; M2pb_M1n[2*j+1] = vj; 
                        where uj and vj are the barycentric coordinates
                        of the projection of node j*/
  float *M2p_M1n; /*!<  coordinates (x y z) of the projection of node nj of M1 
                        on triangle M2t_M1n[j] 
                M2pb_M1n[3*j+0] =x, M2pb_M1n[3*j+1] = y;  M2pb_M1n[3*j+2] = z; */
  double *PD; /*!< signed projection distance from node nj on M1 to 
                   closest triangle on M2 */
  int *M2Nne_M1n; /*!< (M1_N_NodeIndex x 1) number of nodes on M2 considered 
                       to neighbor nodes in M1 
         M2Nne_M1n[j] is the number of nodes on M2 that neighbor node nj on M1 */
  int **M2ne_M1n; /*!< (M1_N_NodeIndex x 1) vectors of node indices 
                M2ne_M1n[j][k] is the kth node neighbor on M2 of node nj on M1 
      The 1st node M2ne_M1n[j][0] is the closest neigbor and the other two, 
      if specified form the triangle on M2 hosting that node.
                k goes from 0 to M2Nne_M1n[j]*/
  double **M2we_M1n; /*!< (M1_N_NodeIndex x 1) vectors of weights.
      M2we_M1n[j][k] is the weight of the kth neighbor on M2 of node nj on M1.
                        */
} SUMA_M2M_STRUCT;

char *SUMA_M2M_node_Info (SUMA_M2M_STRUCT *M2M, int node);
SUMA_M2M_STRUCT *SUMA_FreeM2M(SUMA_M2M_STRUCT *M2M);
SUMA_M2M_STRUCT *SUMA_NewM2M(char *SO1_id, int N_SO1_nodes, 
                             char *SO2_id, int N_SO2_nodes);
SUMA_M2M_STRUCT *SUMA_GetM2M_NN( SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2,
                                 int *oNL_1, int N_NL_1, float *PD_1, float dlim,
                                 int NodeDbg);
float *SUMA_M2M_interpolate(SUMA_M2M_STRUCT *M2M, float *far_data, int ncol, int nrow,  SUMA_INDEXING_ORDER d_order, int useClosest );
SUMA_M2M_STRUCT *SUMA_MorphInfo2M2M(SUMA_MorphInfo *MI);
NI_group *SUMA_M2M_to_niml (SUMA_M2M_STRUCT *M2M);
SUMA_M2M_STRUCT * SUMA_niml_to_M2M(NI_group *ngr);
SUMA_Boolean SUMA_Save_M2M(char *fname, SUMA_M2M_STRUCT *M2M);
SUMA_M2M_STRUCT *SUMA_Load_M2M (char *fname); 

SUMA_DSET *SUMA_morphDsetToStd (SUMA_DSET *dset, SUMA_M2M_STRUCT *M2M, 
                                 int useclosest);


#endif
