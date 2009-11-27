#ifndef SUMA_SPHERICALMAPPING_INCLUDED
#define SUMA_SPHERICALMAPPING_INCLUDED

typedef struct {
  char format[100];
  char type[100];
  char fileToRead[SUMA_MAX_NAME_LENGTH];
  char mapRef[SUMA_MAX_NAME_LENGTH];
  char state[100];
  char dim[100];
} SUMA_SpecSurfInfo;

typedef struct {
  int* IDcode;
  int N_Node_std; /* used to be N_Node*/
  int N_Node_orig;
  int N_FaceSet_std; /* used to be N_FaceSet*/
  float *Weight;
  int *ClsNodes;
  int *FaceSetList;
} SUMA_MorphInfo;

typedef struct {
   int N_elem;
   int* nd_list;
   int* vxl_list;
   int* ijk_list;
   int* nvox_list;
   float* valArray;
} SUMA_1dData;

typedef struct {
   int N_bad_nodes;
   int N_bad_facesets;
} SUMA_SPHERE_QUALITY;
/*
typedef struct {
   char fileNm[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
} SUMA_FileNm;
*/
SUMA_MorphInfo * SUMA_Create_MorphInfo (void);
SUMA_Boolean SUMA_Free_MorphInfo (SUMA_MorphInfo *MI);
SUMA_1dData *SUMA_Create_1dData (void);
SUMA_Boolean SUMA_Free_1dData (SUMA_1dData *data); 

SUMA_SurfaceObject * SUMA_morphToStd (SUMA_SurfaceObject *SO, SUMA_MorphInfo *MI, SUMA_Boolean nodeChk);
float *SUMA_readColor (int numNodes, char* colFileNm);
void SUMA_writeColorFile (float *array, int numNode, int *index, char fileNm[]);   
void SUMA_writeFSfile (SUMA_SurfaceObject *SO, char firstLine[], char fileNm[]);
void SUMA_writeSpecFile (SUMA_SpecSurfInfo *surfaces, int numSurf, char program[], char group[], char specFileNm[], char *histnote);
void SUMA_read1D (char* fileNm, int* i_colm, int* i_locInfo, SUMA_1dData* data);
void SUMA_write1D ( int *num, float *vals, int *index, char firstline[], char outFileNm[]);
float * SUMA_createColGradient( float *col, int numSeg, SUMA_Boolean allGvn );
float * SUMA_assignColors( float *vals, float *cols, int numVal, int numCol, float *colRng, float *valDiv );

SUMA_Boolean SUMA_binSearch( float *nodeList, float target, int *seg);
float intersection_map(float a, float b, float c, float d, float val);
float * SUMA_detWeight (float node0[], float node1[], float node2[], float ptHit[]);
SUMA_Boolean SUMA_inNodeNeighb( SUMA_SurfaceObject *surf, float *nodeList, int *node, float *P0, float *P1);
SUMA_SurfaceObject * SUMA_CreateIcosahedron (float r, int recDepth, float ctr[3], char rec[], int ToSphere);
SUMA_MorphInfo * SUMA_MapSurface (SUMA_SurfaceObject *surf1, SUMA_SurfaceObject *surf2, int verb);
void SUMA_Search_Min_Dist( float* pt, float* nodeList, int* seg, float restr, float *dist, int *i_dist );
void SUMA_binTesselate(float *nodeList, int *triList, int *nCtr, int *tCtr, int recDepth, int depth, int n1, int n2, int n3);
void SUMA_tesselate( float *nodeList, int *triList, int *nCtr, int *tCtr, int N_Div, int n0, int n1, int n2);
int * SUMA_divEdge( float *nodeList, int *nCtr, int node1, int node2, int N_Div);
void SUMA_triangulateRow( float *nodeList, int *triList, int *nCtr, int *tCtr, int N_Div, int *currFloor, int node1, int node2);
void SUMA_addNode(float *nodeList, int *ctr, float x, float y, float z);
void SUMA_addTri(int *triList, int *ctr, int n1, int n2, int n3);
int SUMA_Bad_FacesetNorm_Dot_Radius(SUMA_SurfaceObject *SO, byte *FaceMask, double dot_cut, int *face_bad_ind, float *face_bad_dot, int CalcNorm);


SUMA_SO_map *SUMA_Create_SO_map (void);
SUMA_Boolean SUMA_Free_SO_map (SUMA_SO_map *SOM); 
SUMA_Boolean SUMA_Show_SO_map  (SUMA_SO_map *SOM, FILE *out); 
SUMA_SPHERE_QUALITY SUMA_SphereQuality(SUMA_SurfaceObject *SO, char *Froot, char *shist);
SUMA_Boolean SUMA_ProjectToSphere(SUMA_SurfaceObject *SO, float *ctr, float r);

#define SUMA_ICOSAHEDRON_DIMENSIONS(r, a, b, lgth){ /* r is the radius parameter passed to SUMA_CreateIcosahedron or the -rad option in CreateIcosahedron*/ \
   a = r*(1+sqrt(5)) / (sqrt(10+2*sqrt(5))); \
   b = 2*r / (sqrt(10+2*sqrt(5)));  \
   lgth = sqrt( pow(0-b,2) + pow(b-a,2) + pow(-a-0,2) );  /*determine length of edge by dist node0->node1*/ \
}

#endif
