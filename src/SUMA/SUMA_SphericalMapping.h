#ifndef SUMA_SPHERICALMAPPING_INCLUDED
#define SUMA_SPHERICALMAPPING_INCLUDED

typedef struct {
  char format[1000];
  char type[1000];
  char fileToRead[10000];
  char mapRef[1000];
  char state[1000];
  char dim[1000];
} SUMA_SpecSurfInfo;

typedef struct {
  int* IDcode;
  int N_Node;
  int N_FaceSet;
  float *Weight;
  int *ClsNodes;
  int *FaceSetList;
} SUMA_MorphInfo;

SUMA_MorphInfo * SUMA_Create_MorphInfo (void);
SUMA_Boolean SUMA_Free_MorphInfo (SUMA_MorphInfo *MI);

float *SUMA_morphToStd (float *nodeList, SUMA_MorphInfo *MI);
float *SUMA_readColor (int numNodes, char* colFileNm);
void SUMA_writeColorFile (float *array, int numNode, char fileNm[]);
void SUMA_writeFSfile (float *nodeList, int *faceList, int numNode, int numFace, char firstLine[], char fileNm[]);
void SUMA_writeSpecFile (SUMA_SpecSurfInfo *surfaces, int numSurf, char program[], char group[], char specFileNm[]);

void SUMA_binSearch( float *nodeList, float target, int *seg);
float intersection_map(float a, float b, float c, float d, float val);
float * SUMA_detWeight (float node0[], float node1[], float node2[], float ptHit[]);
SUMA_Boolean SUMA_inNodeNeighb( SUMA_SurfaceObject *surf, float *nodeList, int *node, float *P0, float *P1);
SUMA_SurfaceObject * SUMA_CreateIcosahedron (float r, int recDepth, float ctr[3], char rec[]);
SUMA_MorphInfo * SUMA_MapSurface (SUMA_SurfaceObject *surf1, SUMA_SurfaceObject *surf2);
void SUMA_binTesselate(float *nodeList, int *triList, int *nCtr, int *tCtr, int recDepth, int depth, int n1, int n2, int n3);
void SUMA_tesselate( float *nodeList, int *triList, int *nCtr, int *tCtr, int N_Div, int n0, int n1, int n2);
int * SUMA_divEdge( float *nodeList, int *nCtr, int node1, int node2, int N_Div);
void SUMA_triangulateRow( float *nodeList, int *triList, int *nCtr, int *tCtr, int N_Div, int *currFloor, int node1, int node2);
void SUMA_addNode(float *nodeList, int *ctr, float x, float y, float z);
void SUMA_addTri(int *triList, int *ctr, int n1, int n2, int n3);

SUMA_SO_map *SUMA_Create_SO_map (void);
SUMA_Boolean SUMA_Free_SO_map (SUMA_SO_map *SOM); 
SUMA_Boolean SUMA_Show_SO_map (SUMA_SO_map *SOM, FILE *out); 

#endif
