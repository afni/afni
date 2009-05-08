#ifndef SUMA_GEOMCOMP_INCLUDED
#define SUMA_GEOMCOMP_INCLUDED

#ifndef MAX_NCODE
#define MAX_NCODE 111
#endif

/* 
   faster than SUMA_ApproxNeighbors but a lot more dangerous!
*/
#define SUMA_APPROX_NEIGHBORS(SO, SOf, cent, dnei, SegDist, mask_record, nmask, N_nmask)   \
{  \
   static int m_i;   \
   static float m_dnei_sp, m_alph, m_rsearch;   \
   /* Now cleanup the previous record */  \
   for (m_i=0; m_i<N_nmask; ++m_i) { nmask[mask_record[m_i]] = 0; }  \
   /* Calculate the Equivalent neighborhood distance on the spherical version of the surface */ \
   m_dnei_sp = dnei * SegDist[cent];  \
   m_alph = m_dnei_sp / SOf->SphereRadius; \
   m_rsearch = sin(m_alph/2.0)*2.0*SOf->SphereRadius; \
   SUMA_NODESINSPHERE2( SOf->NodeList, SOf->N_Node, &(SOf->NodeList[3*cent]), m_rsearch, mask_record, N_nmask);   \
   for (m_i=0; m_i<N_nmask; ++m_i) nmask[mask_record[m_i]] = 1;  \
}

typedef enum { SUMA_SMOOTH_NOT_SET, SUMA_EQUAL, SUMA_FUJIWARA, SUMA_DESBRUN } SUMA_TAUBIN_SMOOTH_OPTIONS;

static int SUMA_SSidbg=-1; /*!< Index of node for debug */

typedef struct {
   int ni;
   int layer;
   float off; 
}  SUMA_OFFSET_LL_DATUM;

typedef struct {
   int N_TriIndex;            /*!< Number of triangles considered */
   int *TriIndex;             /*!< vector (N_TriIndex x 1) of 1D indices of triangles considered */
   int **IntersectedVoxels;   /*!< sparse matrix (N_TriIndex x N_InteresectedVoxels[i]) containing
                              1D indices of voxels intersected by the triangle. For example,
                              triangle of index TriIndex[i] intesects N_InteresectedVoxels[i]
                              whose 1D indices are stored in IntersectedVoxels[i] */
                              
   int *N_IntersectedVoxels;  /*!< Number of voxels intersected by some triangle */
} SUMA_VTI; /*!< Voxel Triangle Intersection Structure. Create with SUMA_CreateVTI, Destroy with SUMA_FreeVTI*/

SUMA_Boolean SUMA_TriTrig(float *p1, float *p2, float *p3, 
                           double *s, double *c, double *a);

SUMA_VTI *SUMA_CreateVTI(int N_TriIndex, int *TriIndex);
SUMA_VTI * SUMA_FreeVTI(SUMA_VTI *vti);
SUMA_VTI *SUMA_GetVoxelsIntersectingTriangle( SUMA_SurfaceObject *SO, SUMA_VOLPAR *VolPar, float *NodeIJKlist,
                                                SUMA_VTI *vti);



DList * SUMA_SPI_to_EdgeStrips(SUMA_SurfaceObject *SO, SUMA_SURF_PLANE_INTERSECT *SPI);
SUMA_Boolean SUMA_isEdgeStripClosed(DList *edgestrip, SUMA_SurfaceObject *SO);
int SUMA_isSelfIntersect(SUMA_SurfaceObject *SO, int FullCount, byte * report);
int SUMA_VoxelNeighbors (int ijk, int ni, int nj, int nk, SUMA_VOX_NEIGHB_TYPES ntype, int *nl);
byte *SUMA_FillToVoxelMask(byte *ijkmask, int ijkseed, int ni, int nj, int nk, int *N_in, byte *usethisisin); 
SUMA_Boolean SUMA_VoxelsInBox(int *voxelsijk, int *N_in, float *c1, float *c2);
SUMA_SurfaceObject *SUMA_Patch2Surf(float *NodeList, int N_NodeList, int *PatchFaces, int N_PatchFaces, int PatchDim);
SUMA_PATCH * SUMA_getPatch (  int *NodesSelected, int N_Nodes, 
                              int *Full_FaceSetList, int N_Full_FaceSetList, 
                              SUMA_MEMBER_FACE_SETS *Memb, int MinHits);
SUMA_Boolean SUMA_freePatch (SUMA_PATCH *Patch);
byte *SUMA_MaskOfNodesInPatch(SUMA_SurfaceObject *SO, int *N_NodesUsedInPatch);
SUMA_BRANCH * SUMA_FindBranch (int ** InterMat, int N_InterMat, float ** InterNodes, int ** NodeLoc_in_InterMat, int verbose,  int * WBsz);
SUMA_SURF_PLANE_INTERSECT *SUMA_Surf_Plane_Intersect (SUMA_SurfaceObject *SO, float *PlaneEq);
SUMA_ROI_DATUM *SUMA_Surf_Plane_Intersect_ROI (SUMA_SurfaceObject *SO, int Nfrom, int Nto, float *P);
void SUMA_WeldBranches ( SUMA_BRANCH *branch, int *sz_Branch, int brIndx1, int brIndx2 , int brEnd1, int brEnd2 );
float * SUMA_Plane_Equation (float * P1, float *P2, float *P3, float *thiseq);
int SUMA_Find_Edge_Nhost (SUMA_EDGE_LIST *EL, int *IsInter, int N_IsInter, int *kedge, int Nhost);
SUMA_Boolean SUMA_Mark_Tri (SUMA_EDGE_LIST *EL, int E1, int iBranch, int *TriBranch, int *IsInter, int *N_IsInter, int *VisitationOrder, int *ivisit);
int * SUMA_Dijkstra (SUMA_SurfaceObject *SO, int Nx, int Ny, SUMA_Boolean *isNodeInMesh, int *N_isNodeInMesh, int Method_Number, float *Lfinal, int *N_Path);
void SUMA_free_SPI (SUMA_SURF_PLANE_INTERSECT *SPI);
SUMA_SURF_PLANE_INTERSECT * SUMA_Allocate_SPI (SUMA_SurfaceObject *SO);
SUMA_TRI_BRANCH* SUMA_AssignTriBranch (SUMA_SurfaceObject *SO, SUMA_SURF_PLANE_INTERSECT *SPI, int Nx, int *BranchCount, SUMA_Boolean DoCopy);
SUMA_Boolean SUMA_show_STB (SUMA_TRI_BRANCH *B, FILE *Out);
void SUMA_free_STB (SUMA_TRI_BRANCH *Bv, int N_Bv); 
SUMA_Boolean SUMA_Show_SPI (SUMA_SURF_PLANE_INTERSECT *SPI, FILE * Out, SUMA_SurfaceObject *SO, char *opref, SUMA_SurfaceViewer *sv);
int *SUMA_NodePath_to_EdgePath (SUMA_EDGE_LIST *EL, int *Path, int N_Path, int *N_Edge);
int *SUMA_NodePath_to_TriPath_Inters_OLD (SUMA_SurfaceObject *SO, SUMA_TRI_BRANCH *Bv, int *Path, int N_Path, int *N_Tri);
int *SUMA_NodePath_to_TriPath_Inters ( SUMA_SurfaceObject *SO, SUMA_SURF_PLANE_INTERSECT *SPI, int *nPath, int N_nPath, int *N_tPath);
int * SUMA_IntersectionStrip (SUMA_SurfaceObject *SO, SUMA_SURF_PLANE_INTERSECT *SPI,  int *nPath, int N_nPath, float *dinters, float dmax, int *N_tPath);
SUMA_Boolean SUMA_FromIntEdgeToIntEdge (int Tri, int E1, int E2, SUMA_EDGE_LIST *EL, SUMA_SURF_PLANE_INTERSECT *SPI, int Ny, SUMA_Boolean *Visited, float *d, float dmax, int *tPath, int *N_tPath);
SUMA_Boolean SUMA_isSameEdge (SUMA_EDGE_LIST *EL, int E1, int E2);
SUMA_CONTOUR_EDGES *SUMA_GetContour (SUMA_SurfaceObject *SO, int *Nodes, int N_Node, int *N_ContEdges, int mode, SUMA_PATCH *UseThisPatch);
SUMA_Boolean SUMA_ShowPatch (SUMA_PATCH *Patch, FILE *Out) ;
void SUMA_Set_OffsetDebugNode (int d);
SUMA_Boolean SUMA_getoffsets  (int n, SUMA_SurfaceObject *SO, float *Off, float lim);
SUMA_Boolean SUMA_getoffsets2 (int n, SUMA_SurfaceObject *SO, float lim, SUMA_GET_OFFSET_STRUCT *OffS, int * CoverThisNode, int N_CoverThisNode);
DList * SUMA_getoffsets_ll (int n, SUMA_SurfaceObject *SO, float lim, int *CoverThisNode, int N_CoverThisNode);
SUMA_GET_OFFSET_STRUCT *SUMA_Initialize_getoffsets (int N_Node);
SUMA_Boolean SUMA_AddNodeToLayer (int n, int LayInd, SUMA_GET_OFFSET_STRUCT *OffS);
SUMA_GET_OFFSET_STRUCT * SUMA_Free_getoffsets (SUMA_GET_OFFSET_STRUCT *OffS);
SUMA_Boolean SUMA_Recycle_getoffsets (SUMA_GET_OFFSET_STRUCT *OffS);
float ** SUMA_CalcNeighbDist (SUMA_SurfaceObject *SO);
int SUMA_Chung_Smooth_05_N_iter (double fwhm, double AvgLe, double *sigmap);
float SUMA_SigFromBeta (float Beta);
double SUMA_SigForFWHM(float AvgLe, double dfwhm, int *niter, double *beta);
float ** SUMA_Chung_Smooth_Weights (SUMA_SurfaceObject *SO);
float * SUMA_Chung_Smooth (SUMA_SurfaceObject *SO, float **wgt, 
                           int N_iter, float FWHM, float *fin, 
                           int vpn, SUMA_INDEXING_ORDER d_order, float *fout_user,
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask);
SUMA_Boolean SUMA_Chung_Smooth_dset (SUMA_SurfaceObject *SO, float **wgt, 
                           int N_iter, float FWHM, SUMA_DSET *dset, 
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask);
/* NOTE THAT x passed to the macro must be in units of distance^2 */
#define SUMA_CHUNG_KERNEL_NUMER(x,s) (exp(-(x)/(2.0*(s)*(s)))) 
#define SUMA_FWHM_MEAN(fwhmv, N_fwhmv, meanfwhm, FWHM_mixmode, N) {\
   int m_k=0; \
   N = 0; meanfwhm = 0; \
   if (SUMA_iswordin_ci(FWHM_mixmode, "geom")) {   \
     for (m_k=0; m_k<N_fwhmv; ++m_k) { \
      if (fwhmv[m_k] >= 0.0) { meanfwhm += log(fwhmv[m_k]); ++N; }   \
     }   \
     if (N) meanfwhm = exp(meanfwhm/(double)N) ;  \
     else meanfwhm = 0.0;  \
   } else if (SUMA_iswordin_ci(FWHM_mixmode, "arit")) {  \
     for (m_k=0; m_k<N_fwhmv; ++m_k) { \
      if (fwhmv[m_k] >= 0.0) { meanfwhm += (fwhmv[m_k]); ++N; }  \
     }   \
     if (N) meanfwhm = (meanfwhm/(double)N) ;   \
     else meanfwhm = 0.0;  \
   } else { \
      SUMA_S_Err("Bad or NULL FWHM_mixmode");   \
      N = -1;   \
   }  \
}

float ** SUMA_Chung_Smooth_Weights_05_single (SUMA_SurfaceObject *SO, float fwhm);
float ** SUMA_Chung_Smooth_Weights_05_Pre_07 (SUMA_SurfaceObject *SO, float fwhm);
double ** SUMA_Chung_Smooth_Weights_07 (SUMA_SurfaceObject *SO, double fwhm);
float * SUMA_Chung_Smooth_05 (SUMA_SurfaceObject *SO, float **wgt, 
                           int N_iter, float FWHM, float *fin, 
                           int vpn, SUMA_INDEXING_ORDER d_order, float *fout_user,
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask);
SUMA_Boolean SUMA_Chung_Smooth_05_single_dset (SUMA_SurfaceObject *SO, float **wgt, 
                           int N_iter, float FWHM, SUMA_DSET *dset, 
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask);
SUMA_Boolean SUMA_Chung_Smooth_05_Pre_07_dset (SUMA_SurfaceObject *SO, float **wgt, 
                           int N_iter, float FWHM, SUMA_DSET *dset, 
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask);
SUMA_Boolean SUMA_Chung_Smooth_07_dset (SUMA_SurfaceObject *SO, double **wgt, 
                           int *N_iter, double *FWHM, SUMA_DSET *dset, 
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask);
SUMA_Boolean SUMA_Chung_Smooth_07_toFWHM_dset (SUMA_SurfaceObject *SO, double **wgt, 
                           int *N_iter, double *FWHM, SUMA_DSET *dset, 
                           byte *nmask, byte strict_mask, 
                           char *FWHM_mixmode, float **fwhmrecord);
SUMA_Boolean SUMA_WriteSmoothingRecord (  SUMA_SurfaceObject *SO, 
                                          float *fwhmg, int Niter, 
                                          double *sigma, int cnst_sig,
                                          char *prefix);
SUMA_Boolean  SUMA_Taubin_Smooth_TransferFunc (float l, float m, int N, FILE *Out);
SUMA_Boolean SUMA_Taubin_Smooth_Coef (float k, float *l, float *m);
void SUMA_Set_Taubin_Weights(SUMA_TAUBIN_SMOOTH_OPTIONS tb);
byte SUMA_Get_Taubin_Weights(void);
void SUMA_Set_SurfSmooth_NodeDebug(int n);
float ** SUMA_Taubin_Desbrun_Smooth_Weights (SUMA_SurfaceObject *SO, float *NewNodeList, float ***UseThisWeight);
float ** SUMA_Taubin_Fujiwara_Smooth_Weights (SUMA_SurfaceObject *SO, float *NewNodeList, float ***UseThisWeight);
float * SUMA_Taubin_Smooth (SUMA_SurfaceObject *SO, float **wgt, 
                            float lambda, float mu, float *fin, 
                            int N_iter, int vpn, SUMA_INDEXING_ORDER d_order,
                            float *fout_user, SUMA_COMM_STRUCT *cs,
                            byte *nmask, byte strict_mask);
SUMA_Boolean SUMA_ApplyAffine (float *NodeList, int N_Node, float M[][4], float *center);
float *SUMA_NN_GeomSmooth( SUMA_SurfaceObject *SO, int Niter, float *fin_orig, 
                           int vpn, SUMA_INDEXING_ORDER d_order, float *fout_final_user,
                           SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask);
SUMA_Boolean SUMA_EquateSurfaceSize(SUMA_SurfaceObject *SO, SUMA_SurfaceObject *SOref, float max_off, SUMA_COMM_STRUCT *cs);
SUMA_Boolean SUMA_EquateSurfaceVolumes(SUMA_SurfaceObject *SO, SUMA_SurfaceObject *SOref, float perc_tol, SUMA_COMM_STRUCT *cs);
SUMA_Boolean SUMA_EquateSurfaceAreas(SUMA_SurfaceObject *SO, SUMA_SurfaceObject *SOref, float perc_tol, SUMA_COMM_STRUCT *cs);
double SUMA_Mesh_Volume(SUMA_SurfaceObject *SO, int *FSI, int N_FaceSet) ;
double SUMA_Mesh_Area(SUMA_SurfaceObject *SO, int *FSI, int N_FaceSet) ;
double SUMA_Pattie_Volume (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2, int *Nodes, int N_Node, SUMA_SurfaceObject *UseThisSO, int minhits);
double SUMA_NewAreaAtRadius(SUMA_SurfaceObject *SO, double r, double Rref, float *tmpList);
SUMA_Boolean SUMA_ProjectSurfaceToSphere(SUMA_SurfaceObject *SO, SUMA_SurfaceObject *SOref, float radius, SUMA_COMM_STRUCT *cs);
SUMA_OFFSET_STRUCT *SUMA_FormNeighbOffset ( SUMA_SurfaceObject *SO, float OffsetLim, const char *opts, byte *nmask, float FWHM);
SUMA_OFFSET_STRUCT * SUMA_free_NeighbOffset (SUMA_SurfaceObject *SO, SUMA_OFFSET_STRUCT *OffS_out);
float *SUMA_Offset_GeomSmooth( SUMA_SurfaceObject *SO, int N_iter, float Offestlim, float *fin_orig, 
                              int vpn, SUMA_INDEXING_ORDER d_order, float *fout_final_user,
                              SUMA_COMM_STRUCT *cs);
SUMA_Boolean SUMA_GetOffset2Offset (SUMA_GET_OFFSET_STRUCT *GOS, SUMA_OFFSET_STRUCT *OS);
char * SUMA_ShowOffset_ll_Info (DList *list, int detail);
char * SUMA_ShowOffset_Info (SUMA_GET_OFFSET_STRUCT *OffS, int detail);
SUMA_Boolean SUMA_FixNN_Oversampling ( SUMA_SurfaceObject *SO, SUMA_DSET *dset, byte *nmask, 
                                       int icol, SUMA_Boolean MaskZeros); 

/* Begin function prototypes for VolData.c */
THD_fvec3 SUMA_THD_3dfind_to_3dmm( SUMA_SurfaceObject *SO, THD_fvec3 iv );
THD_fvec3 SUMA_THD_3dfind_to_3dmm_vp( SUMA_VOLPAR *vp, THD_fvec3 iv );
THD_fvec3 SUMA_THD_3dind_to_3dmm( SUMA_SurfaceObject *SO, THD_ivec3 iv );
THD_fvec3 SUMA_THD_3dmm_to_3dfind( SUMA_SurfaceObject *SO , THD_fvec3 fv );
THD_ivec3 SUMA_THD_3dmm_to_3dind( SUMA_SurfaceObject *SO  , THD_fvec3 fv );
THD_ivec3 SUMA_THD_3dmm_to_3dind_warn( SUMA_SurfaceObject *SO  , THD_fvec3 fv, int *out );
THD_fvec3 SUMA_THD_3dmm_to_dicomm( int xxorient, int yyorient, int zzorient , THD_fvec3 imv );
THD_fvec3 SUMA_THD_dicomm_to_3dmm( SUMA_SurfaceObject *SO , THD_fvec3 dicv );
void SUMA_orcode_to_orstring (int xxorient, int yyorient, int zzorient, char *orstr);
void SUMA_sizeto3d_2_deltaHEAD(THD_ivec3 orient, THD_fvec3 *delta);            
void SUMA_originto3d_2_originHEAD(THD_ivec3 orient, THD_fvec3 *origin);
SUMA_Boolean SUMA_vec_3dfind_to_3dmm (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar);
SUMA_Boolean SUMA_vec_3dmm_to_3dfind (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar);
SUMA_Boolean SUMA_vec_dicomm_to_3dfind (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar);
SUMA_Boolean SUMA_vec_3dfind_to_dicomm (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar);
SUMA_Boolean SUMA_vec_3dmm_to_dicomm (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar);
SUMA_Boolean SUMA_vec_dicomm_to_3dmm (float *NodeList, int N_Node, SUMA_VOLPAR *VolPar);
SUMA_Boolean SUMA_CoordChange (char *orc_in, char *orc_out, float *XYZ, int N_xyz);
int SUMA_flip_orient(int xxorient);
int SUMA_ok_orstring (char *orstr);
SUMA_Boolean SUMA_orstring_to_orcode (char *orstr, int *orient);
int SUMA_Subdivide_Mesh(float **NodeListp, int *N_Node, int **FaceSetListp, int *N_FaceSet, float maxarea);
int SUMA_OrientTriangles (float *NodeList, int N_Node, int *FaceSetList, int N_FaceSet, int orient, int Force);
SUMA_Boolean SUMA_Offset_Smooth_dset( SUMA_SurfaceObject *SO, 
                                          float FWHM, float OffsetLim, 
                                          int N_iter,
                                          SUMA_DSET *dset, 
                                          SUMA_COMM_STRUCT *cs, byte *nmask, byte strict_mask); 
float SUMA_estimate_FWHM_1dif( SUMA_SurfaceObject *SO, float *fim , byte *nmask, int nodup );
float *SUMA_estimate_dset_FWHM_1dif(SUMA_SurfaceObject *SO, SUMA_DSET *dset, 
                                    int *icols, int N_icols, byte *nmask, 
                                    int nodup, char *options);
float SUMA_estimate_slice_FWHM_1dif( SUMA_SurfaceObject *SO, float *fim , byte *nmask, int nodup, float *ssvr, DList **striplist_vec);
void SUMA_Set_UseSliceFWHM(int v);
int SUMA_Get_UseSliceFWHM(void);
SUMA_Boolean SUMA_NewSurfaceRadius(SUMA_SurfaceObject *SO, double r, float *Center);
void SUMA_SetDbgFWHM(int i);
int SUMA_GetDbgFWHM(void);
SUMA_Boolean SUMA_CenterOfSphere(double *p1, double *p2, double *p3, double *p4, double *c);
SUMA_Boolean SUMA_GetCenterOfSphereSurface(SUMA_SurfaceObject *SO, int Nquads, double *cs, double *cm);
float *SUMA_SegmentDistortion (SUMA_SurfaceObject *SO1, SUMA_SurfaceObject *SO2);
int SUMA_ApproxNeighbors ( SUMA_SurfaceObject *SO,
                           SUMA_SurfaceObject *SOf,  /* the spherical (or someday flat) version of SO */ 
                           int cent,      /* the central node*/
                           float dnei,     /* the search distance, along the surface from node cent */
                           byte *nmask     /* to contain the nodes within rad from Cent */);

/* End function prototypes for VolData.c */

/* Begin function prototypes for SUMA_ConvexHull.c */
int SUMA_qhull_wrap( int npt , float * xyz , int ** ijk , int fliporient);
/* End function prototypes for SUMA_ConvexHull.c */

DList *SUMA_SliceAlongPlane(SUMA_SurfaceObject *SO, float *Eq, float step);

SUMA_DSET *SUMA_RandomDset(int N_Node, int nc, unsigned int seed, float scale, byte norm); 

/*!
   Macros to merge / join two lists together
   elements of lst2 are set to NULL and you should free lst2 
   when these macros are done.
*/
#define SUMA_MergeLists_Beg2_End1(lst2,lst1){  \
   DListElmt *m_elm = NULL;   \
   if (dlist_size(lst2)){  \
      do{   \
         if (!m_elm) m_elm = dlist_head(lst2);   \
         else m_elm = dlist_next(m_elm);   \
         dlist_ins_next(lst1, dlist_tail(lst1), m_elm->data);   m_elm->data = NULL; /* protect element from erasure */\
      } while (m_elm != dlist_tail(lst2));  \
   }  \
}  
#define SUMA_MergeLists_End2_End1(lst2, lst1) {   \
   DListElmt *m_elm = NULL;   \
   if (dlist_size(lst2)){  \
      do{   \
         if (!m_elm) m_elm = dlist_tail(lst2);   \
         else m_elm = dlist_prev(m_elm);   \
         dlist_ins_next(lst1, dlist_tail(lst1), m_elm->data);   m_elm->data = NULL; /* protect element from erasure */\
     } while (m_elm != dlist_head(lst2));  \
   }  \
}

#define SUMA_MergeLists_End2_Beg1(lst2, lst1) {   \
   DListElmt *m_elm = NULL;   \
   if (dlist_size(lst2)){  \
      do{   \
         if (!m_elm) m_elm = dlist_tail(lst2);   \
         else m_elm = dlist_prev(m_elm);   \
         dlist_ins_prev(lst1, dlist_head(lst1), m_elm->data);   m_elm->data = NULL; /* protect element from erasure */\
      } while (m_elm != dlist_head(lst2));     \
   }\
}
#define SUMA_MergeLists_Beg2_Beg1(lst2, lst1){ \
   DListElmt *m_elm=NULL;  \
   if (dlist_size(lst2)) { \
      do{   \
         if (!m_elm) m_elm = dlist_head(lst2);   \
         else m_elm = dlist_next(m_elm);   \
         dlist_ins_prev(lst1, dlist_head(lst1), m_elm->data);   m_elm->data = NULL; /* protect element from erasure */\
      } while (m_elm != dlist_tail(lst2));  \
   }  \
}


#endif            






