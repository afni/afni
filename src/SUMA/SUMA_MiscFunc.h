#ifndef SUMA_MISCFUNC_INCLUDED
#define SUMA_MISCFUNC_INCLUDED

float * SUMA_Sph2Cart (float *sph, int Nval, float *center ) ;
float * SUMA_Cart2Sph (float *coord, int Nval, float *center ) ;      
void* SUMA_free_fn(const char *CallingFunc, void *ptr);
void *SUMA_calloc_fn (const char *CallingFunc, size_t nmemb, size_t size);
void *SUMA_malloc_fn (const char *CallingFunc, size_t size);
void *SUMA_realloc_fn (const char *CallingFunc, void *ptr, size_t size);
SUMA_MEMTRACE_STRUCT * SUMA_Create_MemTrace (void);
void SUMA_ShowMemTrace (SUMA_MEMTRACE_STRUCT *Mem, FILE *Out);
SUMA_Boolean SUMA_Free_MemTrace (SUMA_MEMTRACE_STRUCT * Mem);
void SUMA_alloc_problem (char *s1);
char **SUMA_allocate2D (int rows,int cols,int element_size);
void SUMA_free2D(char **a,int rows);
void SUMA_error_message (char *s1,char *s2,int ext);
int SUMA_iswordin_ci (const char *sbig,const char *ssub);
int SUMA_iswordin (const char *sbig,const char *ssub);
float SUMA_etime (struct  timeval  *t, int Report);
byte * SUMA_isinpoly(float *P, float *NodeList, int *FaceSetList, int N_FaceSet, int FaceSetDim, int *dims, int *N_in, byte *usethis, byte *mask);
SUMA_ISINBOX SUMA_isinbox (float * NodeList, int nr, float *S_cent , float *S_dim , int BoundIn);
SUMA_Boolean SUMA_Free_IsInBox (SUMA_ISINBOX *IB);
SUMA_ISINSPHERE SUMA_isinsphere (float * NodeList, int nr, float *S_cent , float S_rad , int BoundIn);
SUMA_Boolean SUMA_Free_IsInSphere (SUMA_ISINSPHERE *IB);
float **SUMA_Point_At_Distance(float *U, float *P1, float d);
double **SUMA_dPoint_At_Distance(double *U, double *P1, double d);
SUMA_Boolean SUMA_Point_To_Line_Distance (float *NodeList, int N_points, float *P1, float *P2, float *d2, float *d2min, int *i2min);
SUMA_Boolean SUMA_Point_To_Point_Distance (float *NodeList, int N_points, float *P1, float *d2, float *d2min, int *i2min);
int *SUMA_z_dqsort (int *x , int nx );
int *SUMA_z_dqsort_nsc (int *x , int nx );
int *SUMA_z_qsort (float *x , int nx );
int *SUMA_reorder(int *y, int *isort, int N_isort);
int SUMA_compare_int (int *a, int *b );
int SUMA_compare_float (float *a, float *b );
void SUMA_disp_dmat (int **v,int nr, int nc , int SpcOpt);
void SUMA_disp_mat (float **v,int nr, int nc , int SpcOpt);
void SUMA_disp_vecmat (float *v,int nr, int nc , int SpcOpt, 
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd);
void SUMA_disp_vecdmat (int *v,int nr, int nc , int SpcOpt, 
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd);
void SUMA_disp_vecucmat (unsigned char *v,int nr, int nc , int SpcOpt, 
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd);
void SUMA_disp_dvect (int *v,int l);
void SUMA_disp_vect (float *v,int l);
void SUMA_Set_VoxIntersDbg(int v);
SUMA_Boolean SUMA_isVoxelIntersect_Triangle (float *center, float *dxzy, float *vert0, float *vert1, float *vert2);   
SUMA_Boolean SUMA_MT_isIntersect_Triangle (float *P0, float *P1, float *vert0, float *vert1, float *vert2, float *iP, float *d, int *closest_vert);
SUMA_MT_INTERSECT_TRIANGLE *SUMA_MT_intersect_triangle(float *P0, float *P1, float *NodeList, int N_Node, int *FaceSetList, int N_FaceSet, SUMA_MT_INTERSECT_TRIANGLE * PrevMTI);
void * SUMA_Free_MT_intersect_triangle(SUMA_MT_INTERSECT_TRIANGLE *MTI);
SUMA_Boolean SUMA_Show_MT_intersect_triangle(SUMA_MT_INTERSECT_TRIANGLE *MTI, FILE *Out);
SUMA_Boolean	SUMA_mattoquat (float **mat, float *q);
SUMA_Boolean SUMA_FromToRotation (float *v0, float *v1, float **mtx);
int * SUMA_fqsortrow (float **X , int nr, int nc  );
int * SUMA_dqsortrow (int **X , int nr, int nc  );
int *SUMA_z_doubqsort (double *x , int nx );
int SUMA_float_file_size (char *f_name);
int SUMA_Read_2Dfile (char *f_name, float **x,  int n_rows, int n_cols);
int SUMA_Read_2Ddfile (char *f_name, int **x, int n_rows, int n_cols);
SUMA_Boolean SUMA_MakeConsistent (int *FaceSetList, int N_FaceSet, SUMA_EDGE_LIST *SEL, int detail, int *trouble);
SUMA_EDGE_LIST * SUMA_Make_Edge_List (int *FaceSetList, int N_FaceSet, int N_Node, float *NodeList, char *ownerid);
SUMA_EDGE_LIST * SUMA_Make_Edge_List_eng (int *FaceSetList, int N_FaceSet, int N_Node, float *NodeList, int debug, char *ownerid);
void SUMA_free_Edge_List (SUMA_EDGE_LIST *SEL);
int SUMA_isConsistent (int *T, int *t);
int SUMA_isTriLinked (int*T, int *t, int *cn);
SUMA_FACESET_FIRST_EDGE_NEIGHB *SUMA_allocate_FaceSet_Edge_Neighb (int N_FaceSet);
SUMA_FACESET_FIRST_EDGE_NEIGHB *SUMA_FaceSet_Edge_Neighb (int **EL, int **ELps, int N_EL);
float * SUMA_SmoothAttr_Neighb (float *attr,  int N_attr, float *attr_sm, SUMA_NODE_FIRST_NEIGHB *fn, int nr, byte *nmask);
float * SUMA_SmoothAttr_Neighb_Rec (float *attr, int N_attr, float *attr_sm_orig, 
                                    SUMA_NODE_FIRST_NEIGHB *fn, int nr, int N_rep);
SUMA_NODE_FIRST_NEIGHB * SUMA_Build_FirstNeighb (SUMA_EDGE_LIST *el, int N_Node, char *ownerid);
SUMA_Boolean SUMA_Free_FirstNeighb (SUMA_NODE_FIRST_NEIGHB *FN);
float * SUMA_PolySurf3 (float *NodeList, int N_Node, int *FaceSetList, int N_FaceSet, int PolyDim, float *FaceNormList, SUMA_Boolean SignedArea);
float SUMA_TriSurf3 (float *n0, float *n1, float *n2);
float * SUMA_TriSurf3v (float *NodeList, int *FaceSets, int N_FaceSet);
SUMA_Boolean SUMA_TriNorm (float *n0, float *n1, float *n2, float *norm);
SUMA_SURFACE_CURVATURE * SUMA_Surface_Curvature (float *NodeList, int N_Node, float *NodeNormList, float *Face_A, 
                                                   int N_FaceSet, SUMA_NODE_FIRST_NEIGHB *FN, SUMA_EDGE_LIST *el, char *out);
SUMA_Boolean SUMA_Householder (float *Ni, float **Q);
void SUMA_Free_SURFACE_CURVATURE (SUMA_SURFACE_CURVATURE *SC);
float * SUMA_Convexity (float *NodeList, int N_Node, float *NodeNormList, SUMA_NODE_FIRST_NEIGHB *FN);
float * SUMA_Convexity_Engine (float *NodeList, int N_Node, float *NodeNormList, SUMA_NODE_FIRST_NEIGHB *FN, char *Fname);
int SUMA_Read_file (float *x,char *f_name,int n_points);
int SUMA_Read_dfile (int *x,char *f_name,int n_points);
char * SUMA_pad_str ( char *str, char pad_val , int pad_ln , int opt);
char SUMA_ReadCharStdin (char def, int case_sensitive, char *allowed);
int SUMA_ReadNumStdin (float *fv, int nv);
int * SUMA_Find_inIntVect (int *x, int xsz, int val, int *nValLocation);
int * SUMA_UniqueInt (int *y, int xsz, int *kunq, int Sorted );
int * SUMA_UniqueInt_ind (int *ys, int N_y, int *kunq, int **iup);
void SUMA_Show_Edge_List (SUMA_EDGE_LIST *SEL, FILE *Out);
int SUMA_FindEdge (SUMA_EDGE_LIST *EL, int n1, int n2);
int SUMA_FindEdgeInTri (SUMA_EDGE_LIST *EL, int n1, int n2, int Tri); 
int SUMA_whichTri (SUMA_EDGE_LIST * EL, int n1, int n2, int n3, int IOtrace);
SUMA_Boolean SUMA_Get_Incident(int n1, int n2, SUMA_EDGE_LIST *SEL, int *Incident, int *N_Incident, int IOtrace);
SUMA_Boolean SUMA_Get_NodeIncident(int n1, SUMA_SurfaceObject *SO, int *Incident, int *N_Incident);
SUMA_IRGB *SUMA_Free_IRGB(SUMA_IRGB *irgb);
SUMA_IRGB *SUMA_Read_IRGB_file (char *f_name);
SUMA_IRGB *SUMA_Create_IRGB(int n_el);


#endif

