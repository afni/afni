
#define PROCESS_ROIS_SEPARATELY 0   // Applies to marching parabolas
#define BIG 10000000000.0

#include "3ddata.h"
#include "distanceField.h"


typedef float flt;

typedef enum METRIC_TYPE {
         MARCHING_PARABOLAS,
         EROSION
 } METRIC_TYPE ;


int Cleanup(char *inputFileName, char *outputFileName, THD_3dim_dataset *din);
int afni_edt(THD_3dim_dataset * din, float *outImg, bool do_sqrt, bool edges_are_zero_for_nz, bool debugMode);
int erosion(THD_3dim_dataset * din, float *outImg);
int open_input_dset(THD_3dim_dataset ** din, char * fname);
int outputDistanceField(THD_3dim_dataset *dout, char *outputFileName);
int outputDistanceFieldDebug(float *outImg, THD_3dim_dataset *din, char *outputFileName);
void edt_local(float scale, flt * f, int n);
float sqr(float x);
static flt vx(flt * f, int p, int q);
bool sixConnectedAllHi(BYTE *buffer, int index, int nx, int ny, int nz);
int getIndex(int x, int y, int z, int nx, int ny, int nz);
ERROR_NUMBER getNonzeroIndices(int nvox, int *inputImg, int *numIndices, int **indices);
ERROR_NUMBER processIndex(int index, int *inputImg, float **outImg, THD_3dim_dataset *din);
int usage();
ERROR_NUMBER img3d_Euclidean_DT(int *im, int nx, int ny, int nz,
                       bool do_sqrt, bool edges_are_zero_for_nz, float *ad3, float *odt);
ERROR_NUMBER run_EDTD_per_line(int *roi_line, float *dist2_line, int Na,
                       float delta, bool edges_are_zero_for_nz);
float * Euclidean_DT_delta(float *f, int n, float delta);
ERROR_NUMBER  getDistanceFieldDataSet(THD_3dim_dataset *din, THD_3dim_dataset **dout, int metric,
    bool do_sqrt, bool edges_are_zero_for_nz, bool debugMode);
ERROR_NUMBER getDistanceFields(char *inputFileName, char *outputFileName, int metric, bool do_sqrt,
    bool edges_are_zero_for_nz, bool debugMode);
float sqr(float x);

// Debugging variables
int debugNx, debugNy, debugNz;
float   debugScaleFactors[3], *debugOutImage;

