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
  int N_Node;
  int N_FaceSet;
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
/*
typedef struct {
   char fileNm[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
} SUMA_FileNm;
*/
SUMA_MorphInfo * SUMA_Create_MorphInfo (void);
SUMA_Boolean SUMA_Free_MorphInfo (SUMA_MorphInfo *MI);
SUMA_1dData *SUMA_Create_1dData (void);
SUMA_Boolean SUMA_Free_1dData (SUMA_1dData *data); 

float *SUMA_morphToStd (float *nodeList, SUMA_MorphInfo *MI);
float *SUMA_readColor (int numNodes, char* colFileNm);
void SUMA_writeColorFile (float *array, int numNode, int *index, char fileNm[]);   
void SUMA_writeFSfile (SUMA_SurfaceObject *SO, char firstLine[], char fileNm[]);
void SUMA_writeSpecFile (SUMA_SpecSurfInfo *surfaces, int numSurf, char program[], char group[], char specFileNm[]);
//float* SUMA_readMapDump (int* N_Node, char* dumpFileNm);
//void SUMA_quickSort( float *valArray, int num, int *i_curr, float *srtdArray );
void SUMA_read1D (char* fileNm, int* i_colm, int* i_locInfo, SUMA_1dData* data);
//float* SUMA_read1D (int *N_Node, char* fileNm, int i_colm);
//float* SUMA_readANOVA1D (int *N_Node, char* fileNm, SUMA_Boolean sig);
void SUMA_write1D ( int *num, float *vals, int *index, char firstline[], char outFileNm[]);
//void SUMA_write1D ( int num, float *vals, char firstline[], char outFileNm[]);
//int SUMA_colToNum( char col );
float * SUMA_createColGradient( float *col, int numSeg, SUMA_Boolean allGvn );
//float * SUMA_createColGradient( float *col, int numDiv );
//float * SUMA_createColGradient( char *col, int numDiv );
float * SUMA_assignColors( float *vals, float *cols, int numVal, int numCol, float *colRng, float *valDiv );

SUMA_Boolean SUMA_binSearch( float *nodeList, float target, int *seg);
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
