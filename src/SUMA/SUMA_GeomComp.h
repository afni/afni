#ifndef SUMA_GEOMCOMP_INCLUDED
#define SUMA_GEOMCOMP_INCLUDED
SUMA_PATCH * SUMA_getPatch (  int *NodesSelected, int N_Nodes, 
                              int *Full_FaceSetList, int N_Full_FaceSetList, 
                              SUMA_MEMBER_FACE_SETS *Memb, int MinHits);
SUMA_Boolean SUMA_freePatch (SUMA_PATCH *Patch);
SUMA_Boolean *SUMA_MaskOfNodesInPatch(SUMA_SurfaceObject *SO, int *N_NodesUsedInPatch);
SUMA_BRANCH * SUMA_FindBranch (int ** InterMat, int N_InterMat, float ** InterNodes, int ** NodeLoc_in_InterMat, int verbose,  int * WBsz);
SUMA_SURF_PLANE_INTERSECT *SUMA_Surf_Plane_Intersect (SUMA_SurfaceObject *SO, float *PlaneEq);
SUMA_ROI_DATUM *SUMA_Surf_Plane_Intersect_ROI (SUMA_SurfaceObject *SO, int Nfrom, int Nto, float *P);
void SUMA_WeldBranches ( SUMA_BRANCH *branch, int *sz_Branch, int brIndx1, int brIndx2 , int brEnd1, int brEnd2 );
float * SUMA_Plane_Equation (float * P1, float *P2, float *P3);
int SUMA_Find_Edge_Nhost (SUMA_EDGE_LIST *EL, int *IsInter, int N_IsInter, int *kedge, int Nhost);
SUMA_Boolean SUMA_Mark_Tri (SUMA_EDGE_LIST *EL, int E1, int iBranch, int *TriBranch, int *IsInter, int *N_IsInter, int *VisitationOrder, int *ivisit);
int * SUMA_Dijkstra (SUMA_SurfaceObject *SO, int Nx, int Ny, SUMA_Boolean *isNodeInMesh, int *N_isNodeInMesh, int Method_Number, float *Lfinal, int *N_Path);
void SUMA_free_SPI (SUMA_SURF_PLANE_INTERSECT *SPI);
SUMA_SURF_PLANE_INTERSECT * SUMA_Allocate_SPI (SUMA_SurfaceObject *SO);
SUMA_TRI_BRANCH* SUMA_AssignTriBranch (SUMA_SurfaceObject *SO, SUMA_SURF_PLANE_INTERSECT *SPI, int Nx, int *BranchCount, SUMA_Boolean DoCopy);
SUMA_Boolean SUMA_show_STB (SUMA_TRI_BRANCH *B, FILE *Out);
void SUMA_free_STB (SUMA_TRI_BRANCH *Bv, int N_Bv); 
SUMA_Boolean SUMA_Show_SPI (SUMA_SURF_PLANE_INTERSECT *SPI, FILE * Out, SUMA_SurfaceObject *SO);
int *SUMA_NodePath_to_EdgePath (SUMA_EDGE_LIST *EL, int *Path, int N_Path, int *N_Edge);
int *SUMA_NodePath_to_TriPath_Inters_OLD (SUMA_SurfaceObject *SO, SUMA_TRI_BRANCH *Bv, int *Path, int N_Path, int *N_Tri);
int *SUMA_NodePath_to_TriPath_Inters ( SUMA_SurfaceObject *SO, SUMA_SURF_PLANE_INTERSECT *SPI, int *nPath, int N_nPath, int *N_tPath);
int * SUMA_IntersectionStrip (SUMA_SurfaceObject *SO, SUMA_SURF_PLANE_INTERSECT *SPI,  int *nPath, int N_nPath, float *dinters, float dmax, int *N_tPath);
SUMA_Boolean SUMA_FromIntEdgeToIntEdge (int Tri, int E1, int E2, SUMA_EDGE_LIST *EL, SUMA_SURF_PLANE_INTERSECT *SPI, int Ny, SUMA_Boolean *Visited, float *d, float dmax, int *tPath, int *N_tPath);
SUMA_Boolean SUMA_isSameEdge (SUMA_EDGE_LIST *EL, int E1, int E2);
SUMA_CONTOUR_EDGES *SUMA_GetContour (SUMA_SurfaceObject *SO, int *Nodes, int N_Node, int *N_ContEdges, int mode, SUMA_PATCH *UseThisPatch);
SUMA_Boolean SUMA_ShowPatch (SUMA_PATCH *Patch, FILE *Out) ;
SUMA_Boolean SUMA_getoffsets  (int n, SUMA_SurfaceObject *SO, float *Off, float lim);
SUMA_Boolean SUMA_getoffsets2 (int n, SUMA_SurfaceObject *SO, float lim, SUMA_GET_OFFSET_STRUCT *OffS);
SUMA_GET_OFFSET_STRUCT *SUMA_Initialize_getoffsets (int N_Node);
SUMA_Boolean SUMA_AddNodeToLayer (int n, int LayInd, SUMA_GET_OFFSET_STRUCT *OffS);
SUMA_GET_OFFSET_STRUCT * SUMA_Free_getoffsets (SUMA_GET_OFFSET_STRUCT *OffS);
SUMA_Boolean SUMA_Recycle_getoffsets (SUMA_GET_OFFSET_STRUCT *OffS);
SUMA_COMM_STRUCT *SUMA_Create_CommSrtuct(void);
SUMA_COMM_STRUCT *SUMA_Free_CommSrtuct(SUMA_COMM_STRUCT *cs);
float ** SUMA_CalcNeighbDist (SUMA_SurfaceObject *SO);
float ** SUMA_Chung_Smooth_Weights (SUMA_SurfaceObject *SO);
float * SUMA_Chung_Smooth (SUMA_SurfaceObject *SO, float **wgt, 
                           int N_iter, float FWHM, float *fin, 
                           int vpn, SUMA_INDEXING_ORDER d_order, float *fout_user,
                           SUMA_COMM_STRUCT *cs);
SUMA_Boolean  SUMA_Taubin_Smooth_TransferFunc (float l, float m, int N, FILE *Out);
SUMA_Boolean SUMA_Taubin_Smooth_Coef (float k, float *l, float *m);
float * SUMA_Taubin_Smooth (SUMA_SurfaceObject *SO, float **wgt, 
                            float lambda, float mu, float *fin, 
                            int N_iter, int vpn, SUMA_INDEXING_ORDER d_order,
                            float *fout_user, SUMA_COMM_STRUCT *cs);
NI_element * SUMA_NodeVal2irgba_nel (SUMA_SurfaceObject *SO, float *val, SUMA_Boolean cleanup);
SUMA_Boolean SUMA_SendToAfni (SUMA_SurfaceObject *SO, SUMA_COMM_STRUCT *cs, void *data, SUMA_DSET_TYPE dtype, int action);
NI_element * SUMA_NodeXYZ2NodeXYZ_nel (SUMA_SurfaceObject *SO, float *val, SUMA_Boolean cleanup);
SUMA_Boolean SUMA_ApplyAffine (float *NodeList, int N_Node, float M[][4], float *center);
float *SUMA_NN_GeomSmooth( SUMA_SurfaceObject *SO, int Niter, float *fin_orig, 
                           int vpn, SUMA_INDEXING_ORDER d_order, float *fout_final_user,
                           SUMA_COMM_STRUCT *cs);
SUMA_Boolean SUMA_EquateSurfaceSize(SUMA_SurfaceObject *SO, SUMA_SurfaceObject *SOref, float max_off, SUMA_COMM_STRUCT *cs);
SUMA_Boolean SUMA_EquateSurfaceVolumes(SUMA_SurfaceObject *SO, SUMA_SurfaceObject *SOref, float perc_tol, SUMA_COMM_STRUCT *cs);
SUMA_Boolean SUMA_EquateSurfaceAreas(SUMA_SurfaceObject *SO, SUMA_SurfaceObject *SOref, float perc_tol, SUMA_COMM_STRUCT *cs);
double SUMA_Mesh_Volume(SUMA_SurfaceObject *SO, int *FSI, int N_FaceSet) ;
double SUMA_Mesh_Area(SUMA_SurfaceObject *SO, int *FSI, int N_FaceSet) ;
double SUMA_Pattie_Volume (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2, int *Nodes, int N_Node, SUMA_SurfaceObject *UseThisSO, int minhits);
double SUMA_NewAreaAtRadius(SUMA_SurfaceObject *SO, double r, double Rref, float *tmpList);
SUMA_Boolean SUMA_ProjectSurfaceToSphere(SUMA_SurfaceObject *SO, SUMA_SurfaceObject *SOref, float radius, SUMA_COMM_STRUCT *cs);





#endif            






