#ifndef SUMA_MISCFUNC_INCLUDED
#define SUMA_MISCFUNC_INCLUDED

int SUMA_filexists (char *f_name);
void SUMA_alloc_problem (char *s1);
char **SUMA_allocate2D (int rows,int cols,int element_size);
void SUMA_free2D(char **a,int rows);
void SUMA_error_message (char *s1,char *s2,int ext);
int SUMA_iswordin (const char *sbig,const char *ssub);
SUMA_FileName SUMA_StripPath (char *FileName);
float SUMA_etime (struct  timeval  *t, int Report);
SUMA_ISINBOX SUMA_isinbox (float ** XYZ, int nr, float *S_cent , float *S_dim , int BoundIn);
SUMA_Boolean SUMA_Free_IsInBox (SUMA_ISINBOX *IB);
SUMA_ISINSPHERE SUMA_isinsphere (float ** XYZ, int nr, float *S_cent , float S_rad , int BoundIn);
float **SUMA_Point_At_Distance(float *U, float *P1, float d);
SUMA_Boolean SUMA_Point_To_Line_Distance (float **Points, int N_points, float *P1, float *P2, float *d2, float *d2min, int *i2min);
SUMA_Boolean SUMA_Point_To_Point_Distance (float **Points, int N_points, float *P1, float *d2, float *d2min, int *i2min);
int *SUMA_z_dqsort (int *x , int nx );
int *SUMA_z_qsort (float *x , int nx );
void SUMA_disp_dmat (int **v,int nr, int nc , int SpcOpt);
void SUMA_disp_mat (float **v,int nr, int nc , int SpcOpt);
void SUMA_disp_dvect (int *v,int l);
void SUMA_disp_vect (float *v,int l);
SUMA_MT_INTERSECT_TRIANGLE *SUMA_MT_intersect_triangle(float *P0, float *P1, float **NodeList, int N_Node, int **FaceSetList, int N_FaceSet);
SUMA_Boolean SUMA_Free_MT_intersect_triangle(SUMA_MT_INTERSECT_TRIANGLE *MTI);
SUMA_Boolean SUMA_Show_MT_intersect_triangle(SUMA_MT_INTERSECT_TRIANGLE *MTI, FILE *Out);
SUMA_Boolean	SUMA_mattoquat (float **mat, float *q);
SUMA_Boolean SUMA_FromToRotation (float *v0, float *v1, float **mtx);
int * SUMA_fqsortrow (float **X , int nr, int nc  );
int * SUMA_dqsortrow (int **X , int nr, int nc  );
int SUMA_float_file_size (char *f_name);
int SUMA_Read_2Dfile (char *f_name, float **x,  int n_rows, int n_cols);
int SUMA_Read_2Ddfile (char *f_name, int **x, int n_rows, int n_cols);
SUMA_Boolean SUMA_MakeConsistent (int **FaceSetList, int N_FaceSet, SUMA_EDGE_LIST *SEL);
SUMA_EDGE_LIST * SUMA_Make_Edge_List (int **FL, int N_FL, int N_Node);
void SUMA_free_Edge_List (SUMA_EDGE_LIST *SEL);
int SUMA_isConsistent (int *T, int *t);
SUMA_FACESET_FIRST_EDGE_NEIGHB *SUMA_allocate_FaceSet_Edge_Neighb (int N_FaceSet);
SUMA_FACESET_FIRST_EDGE_NEIGHB *SUMA_FaceSet_Edge_Neighb (int **EL, int **ELps, int N_EL);
float * SUMA_SmoothAttr_Neighb (float *attr,  int N_attr, float *attr_sm, SUMA_NODE_FIRST_NEIGHB *fn);
SUMA_NODE_FIRST_NEIGHB * SUMA_Build_FirstNeighb (SUMA_EDGE_LIST *el, int N_Node);
SUMA_Boolean SUMA_Free_FirstNeighb (SUMA_NODE_FIRST_NEIGHB *FN);
float * SUMA_PolySurf3 (float **NodeXYZ, int N_Node, int **FaceSets, int N_FaceSet, int PolyDim, float **FN);
SUMA_SURFACE_CURVATURE * SUMA_Surface_Curvature (float **NodeList, int N_Node, float **NodeNormList, float *Face_A, int N_FaceSet, SUMA_NODE_FIRST_NEIGHB *FN, SUMA_EDGE_LIST *el);
SUMA_Boolean SUMA_Householder (float *Ni, float **Q);
void SUMA_Free_SURFACE_CURVATURE (SUMA_SURFACE_CURVATURE *SC);
float * SUMA_Convexity (float **NL, int N_N, float **NNL, SUMA_NODE_FIRST_NEIGHB *FN);
int SUMA_Read_file (float *x,char *f_name,int n_points);
int SUMA_Read_dfile (int *x,char *f_name,int n_points);



#endif

