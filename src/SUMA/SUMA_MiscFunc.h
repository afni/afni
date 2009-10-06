#ifndef SUMA_MISCFUNC_INCLUDED
#define SUMA_MISCFUNC_INCLUDED

#define MATRIX_NO_TIP (1<<1)
#define MATRIX_OUT_SYMMETRIC (1<<2)
#define MATRIX_B_IS_AT (1<<2)

/*!
   A slimmed macro version of SUMA_nodesinsphere2
*/
#define SUMA_NODESINSPHERE2(XYZ, nr, S_cent, S_dim , nodesin, nin) {\
   static int m_k, m_nr3;  \
   static float m_t0, m_t1, m_t2, m_d2, m_r2; \
   m_k= nin = 0; \
   m_nr3 = 3*nr;   \
   m_r2 = S_dim*S_dim;  \
   nin = 0; \
   /*fprintf(SUMA_STDERR,"%s: inbound, center %f %f %f, dim %f\n", FuncName, *(S_cent), *((S_cent)+1), *((S_cent)+2), S_dim);   */\
   while (m_k < m_nr3) {  \
      /* relative distance to center */   \
         m_t0 = SUMA_ABS((XYZ[m_k] - *(S_cent)));  ++m_k;   \
         if (m_t0 <= S_dim) {  \
            m_t1 = SUMA_ABS((XYZ[m_k] - *((S_cent)+1))); ++m_k;   \
            if (m_t1 <= S_dim) {  \
               m_t2 = SUMA_ABS((XYZ[m_k] - *((S_cent)+2))); ++m_k;   \
               if (m_t2 <= S_dim) {  \
                  /* in box, is it in sphere? */   \
                  m_d2 = (m_t0*m_t0+m_t1*m_t1+m_t2*m_t2);   \
                  if (m_d2 <=m_r2) {   \
                     nodesin[nin] = m_k/3-1;  /* retreat by 1, increment already done */\
                     ++nin;   \
                  }  \
               }  \
            } else ++m_k;  \
         } else m_k += 2;  \
      }  \
   }


double SUMA_factorial (int n);
double *SUMA_factorial_array (int n);
SUMA_MX_VEC *SUMA_KronProd(SUMA_MX_VEC *A, SUMA_MX_VEC *B);
float * SUMA_Sph2Cart (double *sph, int Nval, float *center ) ;
double * SUMA_Cart2Sph (float *coord, int Nval, float *center ) ;      
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
int SUMA_iswordsame_ci (const char *sbig,const char *ssub);
int SUMA_iswordsame (const char *sbig,const char *ssub);
float SUMA_etime (struct  timeval  *t, int Report);
int SUMA_etime2(char *name, char *str, char *strloc);
byte * SUMA_isinpoly(float *P, float *NodeList, int *FaceSetList, int N_FaceSet, int FaceSetDim, int *dims, int *N_in, byte *usethis, byte *mask);
SUMA_ISINBOX SUMA_isinbox (float * NodeList, int nr, float *S_cent , float *S_dim , int BoundIn);
SUMA_Boolean SUMA_Free_IsInBox (SUMA_ISINBOX *IB);
SUMA_ISINSPHERE SUMA_isinsphere (float * NodeList, int nr, float *S_cent , float S_rad , int BoundIn);
SUMA_Boolean SUMA_Free_IsInSphere (SUMA_ISINSPHERE *IB);
int SUMA_nodesinsphere2 (float *XYZ, int nr, float *S_cent , float S_dim , int *nodesin, float *dinsq);
int SUMA_nodesinbox2 (float *XYZ, int nr, float *S_cent , float *S_dim , int *nodesin, float *dinsq);
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
int SUMA_compare_double (double *a, double *b );
void SUMA_disp_dmat (int **v,int nr, int nc , int SpcOpt);
void SUMA_disp_mat (float **v,int nr, int nc , int SpcOpt);
void SUMA_disp_vecmat (float *v,int nr, int nc , int SpcOpt, 
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd);
void SUMA_disp_vecdmat (int *v,int nr, int nc , int SpcOpt, 
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd);
void SUMA_disp_vecucmat (unsigned char *v,int nr, int nc , int SpcOpt, 
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd);
void SUMA_disp_veccmat (char *v,int nr, int nc , int SpcOpt, 
                        SUMA_INDEXING_ORDER d_order, FILE *fout, 
                        SUMA_Boolean AddRowInd);
void SUMA_disp_vecdoubmat (double *v,int nr, int nc , int SpcOpt, 
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd);
void SUMA_disp_veccompmat (complex *v,int nr, int nc , int SpcOpt, 
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd);
void SUMA_disp_vecshortmat (short *v,int nr, int nc , int SpcOpt, 
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd);
void SUMA_disp_vecbytemat (byte *v,int nr, int nc , int SpcOpt, 
                        SUMA_INDEXING_ORDER d_order, FILE *fout, SUMA_Boolean AddRowInd);
void SUMA_disp_dvect (int *v,int l);
void SUMA_disp_vect (float *v,int l);
void SUMA_disp_doubvect (double *v,int l);
int SUMA_WriteMxVec(SUMA_MX_VEC *mxv, char *Name, char *title);
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
float * SUMA_SmoothAttr_Neighb (float *attr,  int N_attr, float *attr_sm, SUMA_NODE_FIRST_NEIGHB *fn, int nr, byte *nmask, byte strict_mask);
float * SUMA_SmoothAttr_Neighb_Rec (float *attr, int N_attr, float *attr_sm_orig, 
                                    SUMA_NODE_FIRST_NEIGHB *fn, int nr, int N_rep);
SUMA_NODE_FIRST_NEIGHB * SUMA_Build_FirstNeighb (SUMA_EDGE_LIST *el, int N_Node, char *ownerid);
SUMA_Boolean SUMA_Free_FirstNeighb (SUMA_NODE_FIRST_NEIGHB *FN);
float * SUMA_PolySurf3 (float *NodeList, int N_Node, int *FaceSetList, int N_FaceSet, int PolyDim, float *FaceNormList, SUMA_Boolean SignedArea);
float SUMA_TriSurf3 (float *n0, float *n1, float *n2);
float * SUMA_TriSurf3v (float *NodeList, int *FaceSets, int N_FaceSet);
SUMA_Boolean SUMA_TriNorm (float *n0, float *n1, float *n2, float *norm);
SUMA_SURFACE_CURVATURE * SUMA_Surface_Curvature (float *NodeList, int N_Node, float *NodeNormList, float *Face_A, 
                                                   int N_FaceSet, SUMA_NODE_FIRST_NEIGHB *FN, SUMA_EDGE_LIST *el, char *out, int verb);
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
int SUMA_whichTri_e (SUMA_EDGE_LIST * EL, int E1, int E2, int IOtrace, 
                     byte quiet);
SUMA_Boolean SUMA_Get_Incident(int n1, int n2, SUMA_EDGE_LIST *SEL, int *Incident, int *N_Incident, int IOtrace, byte quiet);
SUMA_Boolean SUMA_Get_NodeIncident(int n1, SUMA_SurfaceObject *SO, int *Incident, int *N_Incident);
SUMA_IRGB *SUMA_Free_IRGB(SUMA_IRGB *irgb);
SUMA_IRGB *SUMA_Read_IRGB_file (char *f_name);
SUMA_IRGB *SUMA_Create_IRGB(int n_el);
SUMA_MX_VEC * SUMA_MxVecSetIdentity(SUMA_MX_VEC *thisone);
SUMA_MX_VEC * SUMA_MxVecIdentity(SUMA_VARTYPE tp, int n, int dims[], SUMA_MX_VEC *recycle);
SUMA_MX_VEC *SUMA_MxVecMult(SUMA_MX_VEC *va, SUMA_MX_VEC *vb, SUMA_MX_VEC *recycle, int InfoMask);
SUMA_MX_VEC *SUMA_MxVecMult_Engine(SUMA_MX_VEC *va, SUMA_MX_VEC *vb, SUMA_MX_VEC *recycle, SUMA_MX_VEC *vat, SUMA_MX_VEC *vbt, int InfoMask);
SUMA_MX_VEC *SUMA_MxVecMultRect_Engine(SUMA_MX_VEC *va, SUMA_MX_VEC *vb, SUMA_MX_VEC *recycle, SUMA_MX_VEC *vat, SUMA_MX_VEC *vbt, int InfoMask);
SUMA_MX_VEC * SUMA_MxVecTranspose(SUMA_MX_VEC *va, SUMA_MX_VEC *recycle);
SUMA_MX_VEC * SUMA_MxVecInverse(SUMA_MX_VEC *va, SUMA_MX_VEC *recycle);
SUMA_MX_VEC *SUMA_matrix2MxVec(matrix c) ;
SUMA_MX_VEC *SUMA_MxVecAdd(SUMA_MX_VEC *va, SUMA_MX_VEC *vb, int sign, SUMA_MX_VEC *recycle);
SUMA_MX_VEC * SUMA_MxVecCopy(SUMA_MX_VEC *va, SUMA_MX_VEC *recycle);
int SUMA_MxVecSameDims(SUMA_MX_VEC *va,SUMA_MX_VEC *vb);
int SUMA_MxVecSameDims2(int N_dims, int *dims, SUMA_MX_VEC *va);
SUMA_MX_VEC *SUMA_CoerceMxVec(SUMA_MX_VEC *va, SUMA_VARTYPE tp, int abs, SUMA_MX_VEC *recycle); 
int SUMA_MxVecBuildMat(SUMA_MX_VEC *mxv);
void SUMA_TestMxVecMatOps(void);
SUMA_MX_VEC * SUMA_MxVecRand(SUMA_VARTYPE tp, int N_dims,int * dims, SUMA_MX_VEC *recycle);
SUMA_MX_VEC *  SUMA_Read1DMxVec(SUMA_VARTYPE tp, char *Name, int *dims, int *N_dims);

void SUMA_ShowFromTo(char *f, char *t, char *head);
int SUMA_LineNumbersFromTo(char *f, char *t);


#endif

