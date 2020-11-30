/********************************************************************************************************

NAME:
    distanceField - determines depth of voxels in 3D binary objects

SYNOPSIS:
    distanceField -i <input filename> [-m <metric>]

The input file is expected to be an AFNI dataset.

The "metric" is a text string (upper case) describing the algorithm used to estimate the depth. It may be
one of the following.
MARCHING_PARABOLAS - Marching parabolas (default)
EROSION - Erosion algorithm.

*********************************************************************************************************/

#include <stdio.h>
#include <dirent.h>
#include <malloc.h>
#include <string.h>
#include <unistd.h>
#include <stdbool.h>
#include "mrilib.h"
#include "distanceField.h"
#include <float.h>


typedef float flt;

typedef enum METRIC_TYPE {
         MARCHING_PARABOLAS,
         EROSION
 } METRIC_TYPE ;


int Cleanup(char *inputFileName, THD_3dim_dataset *din);
static int afni_edt(THD_3dim_dataset * din, float *outImg);
static int erosion(THD_3dim_dataset * din, float *outImg);
int open_input_dset(THD_3dim_dataset ** din, char * fname);
int outputDistanceField(float *outImg, THD_3dim_dataset *din, int metric);
int doesFileExist(char * searchPath, char * prefix,char *appendage , char * outputFileName);
void edt1_local(THD_3dim_dataset * din, flt * df, int n);
void edt_local(float scale, flt * f, int n);
float sqr(float x);
static flt vx(flt * f, int p, int q);
bool sixConnectedAllHi(BYTE *buffer, int index, int nx, int ny, int nz);
int getIndex(int x, int y, int z, int nx, int ny, int nz);
int transposeYZ(float *volume, int nx, int ny, int nz);
int testTransposeFunction(THD_3dim_dataset * din);
int outputTransposedDataSet(float *buffer, THD_3dim_dataset *din, int nx, int nz, int ny);
int shortToByte(THD_3dim_dataset ** din);
ERROR_NUMBER getNonzeroIndices(int nvox, int *inputImg, int *numIndices, int **indices);
ERROR_NUMBER processIndex(int index, int *inputImg, float **outImg, THD_3dim_dataset *din);
int usage();

float sqr(float x){
    return x*x;
}

int main( int argc, char *argv[] )
{
    char    *inputFileName=NULL;
    int     i, metric=MARCHING_PARABOLAS;
    THD_3dim_dataset * din = NULL;
    ERROR_NUMBER    errorNumber;
    float *outImg;

    for (i=0; i<argc; ++i) if (argv[i][0]=='-'){
        switch(argv[i][1]){
        case 'i':
            if (!(inputFileName=(char*)malloc(strlen(argv[++i])+8)))
                return Cleanup(inputFileName,  din);
            sprintf(inputFileName,"%s",argv[i]);
            break;

        case 'm':
            if (!strcmp(argv[++i],"MARCHING_PARABOLAS")){
                metric = MARCHING_PARABOLAS;
            }
            if (!strcmp(argv[i],"EROSION")){
                metric = EROSION;
            } else {
                fprintf(stderr, "Unrecognized metric");
                if (inputFileName) free(inputFileName);
                return ERROR_UNRECOGNIZABLE_SPECIES;
            }
            break;
            default:
                return usage();
        }
    }

    if (!inputFileName) return usage();

    // Open dataset
    if ( (errorNumber=open_input_dset(&din, inputFileName))!=ERROR_NONE ){
        Cleanup(inputFileName, din);
        return errorNumber;
    }

    if (!(outImg = (float *)calloc(DSET_NVOX(din), sizeof(float)))){
        Cleanup(inputFileName, din);
        return ERROR_MEMORY_ALLOCATION;
    }

    // Apply metric
    switch (metric){
    case MARCHING_PARABOLAS:
        if ((errorNumber=afni_edt(din, outImg))!=ERROR_NONE){
            Cleanup(inputFileName, din);
            return errorNumber;
        }
        break;
    case EROSION:
        if ((errorNumber=erosion(din, outImg))!=ERROR_NONE){
            Cleanup(inputFileName, din);
            return errorNumber;
        }
        break;
    }

    // Output result to afni dataset
    outputDistanceField(outImg, din, metric);

    Cleanup(inputFileName,  din);

    return 0;
}

int usage(){
    fprintf(stderr, "SYNOPSIS:\n");
        fprintf(stderr, "\tdistanceField -i <input filename> [-m <metric>]\n\n");

    fprintf(stderr, "The input file is expected to be an AFNI dataset.\n\n");

    fprintf(stderr, "The \"metric\" is a text string (upper case) describing the algorithm used to\n");
    fprintf(stderr, "estimate the depth. It may be one of the following.\n");
    fprintf(stderr, "MARCHING_PARABOLAS - Marching parabolas (default)\n");
    fprintf(stderr, "EROSION - Erosion algorithm.\n");

    return 0;
}

int outputDistanceField(float *outImg, THD_3dim_dataset *din, int metric){
    char *prefix=DSET_PREFIX(din);
    char *searchPath=DSET_DIRNAME(din);
    char *outputFileName;
    char  appendage[256];

    // Set appendage
    switch (metric){
    case MARCHING_PARABOLAS:
        sprintf(appendage, "MarParab");
        break;
    case EROSION:
        sprintf(appendage, "Erosion");
        break;
    }

    // Allocate memory to output name buffer
    if (!(outputFileName=(char *)malloc(strlen(searchPath)+strlen(prefix)+strlen(appendage)+8))){
       return ERROR_MEMORY_ALLOCATION;
    }

    // Determine whether output file already exists
    int outputFileExists = doesFileExist(searchPath,prefix,appendage,outputFileName);

    // Output Fourier spectrum image (if it does not already exist)
    if (!outputFileExists){
        THD_3dim_dataset *dout = EDIT_empty_copy(din);
        sprintf(outputFileName,"%s%s%s",searchPath,prefix,appendage);
        EDIT_dset_items( dout ,
                        ADN_prefix, outputFileName,
                        ADN_type, MRI_float,
                        ADN_none ) ;
        EDIT_substitute_brick(dout, 0, MRI_float, outImg);
        DSET_write(dout);
    }

    // Cleanup
    free(outputFileName);

    return ERROR_NONE;
}

static int erosion(THD_3dim_dataset * din, float *outImg){

    // Get dimensions in voxels
    int nz = DSET_NZ(din);
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);
    int nvox = nx*ny*nz;
    int i;
    bool objectVoxelsLeft=TRUE;
    BYTE * buffer;

/*
    // Get real world voxel sizes
    int yDim = fabs(DSET_DY(din));
    int zDim = fabs(DSET_DZ(din));
    */

	if ((nvox < 1) || (nx < 2) || (ny < 2) || (nz < 1)) return ERROR_DIFFERENT_DIMENSIONS;

    int brickType=DSET_BRICK_TYPE(din, 0);
    if( brickType != MRI_byte ) return ERROR_DATATYPENOTHANDLED;
    BYTE * img = DSET_ARRAY(din, 0);

    // Add uneroded volume to output
    for (i=0; i<nvox; ++i) outImg[i]+=(img[i]!=0);

    // Allocate memory to buffer
    if (!(buffer = (BYTE *)malloc(nvox*sizeof(BYTE)))) return ERROR_MEMORY_ALLOCATION;

    // Erode volume, adding eroede volume to output until no object voxels left
    do {
        objectVoxelsLeft = FALSE;

        // Copy inpout image to buffer
        for (i=0; i<nvox; ++i) buffer[i] = img[i];

        // Erode input
        for (i=0; i<nvox; ++i){
            img[i]=(buffer[i]!=0) && sixConnectedAllHi(buffer, i, nx, ny, nz);
            if (img[i]) objectVoxelsLeft = TRUE;
        }

        // Add eroded volume to output
        for (i=0; i<nvox; ++i) outImg[i]+=(img[i]!=0);
    } while (objectVoxelsLeft);

    // Cleanup
    free(buffer);


    return ERROR_NONE;
}

bool sixConnectedAllHi(BYTE *buffer, int index, int nx, int ny, int nz){
    int planeSize = nx*ny;
    int z = (int)(index/planeSize);
    int y = (int)((index-z*planeSize)/nx);
    int x = index - z*planeSize - y*nx;

    return buffer[getIndex((x>0)? x-1:x,y,z, nx, ny,nz)]  && buffer[getIndex((x<nx-1)? x+1:x,y,z, nx, ny,nz)] &&
        buffer[getIndex(x, (y>0)? y-1:y,z, nx, ny,nz)] && buffer[getIndex(x, (y<ny-1)? y+1:y,z, nx, ny,nz)] &&
        buffer[getIndex(x,y,(z>0)? z-1:z, nx, ny,nz)] && buffer[getIndex(x,y,(z<nz-1)? z+1:z, nx, ny,nz)] ;
}

int getIndex(int x, int y, int z, int nx, int ny, int nz){
    return z*nx*ny + y*nx + x;
}


static int afni_edt(THD_3dim_dataset * din, float *outImg){

    // Get dimensions in voxels
    int nz = DSET_NZ(din);
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);
    int nvox = nx*ny*nz;
    int *inputImg;
    int numIndices;
    int *indices;
    BYTE * byteImg;
    short * shortImg;
    float   *floatImg, *addend;
    ERROR_NUMBER    errorNumber;

	if ((nvox < 1) || (nx < 2) || (ny < 2) || (nz < 1)) return ERROR_DIFFERENT_DIMENSIONS;

	// Alliocate memory to integer input buffer
	if (!(inputImg=(int *)calloc(nvox,sizeof(int)))) return ERROR_MEMORY_ALLOCATION;

	DSET_load(din);
    int brickType=DSET_BRICK_TYPE(din, 0);
    fprintf(stderr, "brickType=%d\n", brickType);
    switch(brickType){
        case MRI_byte:
            byteImg = DSET_ARRAY(din, 0);
            for (int i=0; i<nvox; ++i) inputImg[i] = byteImg[i];
            break;
        case MRI_short:
            shortImg = DSET_ARRAY(din, 0);
            for (int i=0; i<nvox; ++i) inputImg[i] = shortImg[i];
            break;
        case MRI_int:
            free(inputImg);
            inputImg = DSET_ARRAY(din, 0);
            break;
        case MRI_float:
            floatImg = DSET_ARRAY(din, 0);
            for (int i=0; i<nvox; ++i) inputImg[i] = (int)(floatImg[i]+0.5);
            break;
    }

    // Get unique nonzero index values
    if ((errorNumber=getNonzeroIndices(nvox, inputImg, &numIndices, &indices))!=ERROR_NONE){
        free(inputImg);
        return errorNumber;
    }

    // Process each index
    for (int i=0; i<numIndices; ++i){
        if ((errorNumber=processIndex(indices[i], inputImg, &addend, din))!=ERROR_NONE){
            free(inputImg);
            free(indices);
            return errorNumber;
        }
        for (int j=0; j<nvox; ++j) outImg[j]+=addend[j];
    }

	// Cleanup
	free(inputImg);
	free(indices);

	return ERROR_NONE;
}

ERROR_NUMBER processIndex(int index, int *inputImg, float **outImg, THD_3dim_dataset *din){
    // Get dimensions in voxels
    int nz = DSET_NZ(din);
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);
    int nvox = nx*ny*nz;

	int nVol = 1;
	int nvox3D = nx * ny * MAX(nz, 1);
	nVol = nvox / nvox3D;
	if ((nvox3D * nVol) != nvox) return ERROR_DIFFERENT_DIMENSIONS;

    // Get real world voxel sizes
    float xDim = fabs(DSET_DX(din));
    float yDim = fabs(DSET_DY(din));
    float zDim = fabs(DSET_DZ(din));

    zDim=yDim/zDim;
    xDim=yDim/xDim;
    yDim=1.0f;

    if (!(*outImg=(float *)calloc(nvox, sizeof(float)))) return ERROR_MEMORY_ALLOCATION;

	for (size_t i = 0; i < nvox; i++ ) {
		if (inputImg[i] == index)
			(*outImg)[i] = INFINITY;
	}
	size_t nRow = ny*nz;

	//EDT in left-right direction
	for (int r = 0; r < nRow; r++ ) {
		flt * imgRow = (*outImg) + (r * nx);
		// edt1(imgRow, nx);
		edt1_local(din, imgRow, nx);
	}

/**/
	//EDT in anterior-posterior direction
	nRow = nx * nz; //transpose XYZ to YXZ and blur Y columns with XZ Rows
	for (int v = 0; v < nVol; v++ ) { //transpose each volume separately
		flt * img3D = (flt *)calloc(nvox3D*sizeof(flt), 64); //alloc for each volume to allow openmp

		//transpose data
		size_t vo = v * nvox3D; //volume offset
		for (int z = 0; z < nz; z++ ) {
			int zo = z * nx * ny;
			for (int y = 0; y < ny; y++ ) {
				int xo = 0;
				for (int x = 0; x < nx; x++ ) {
					img3D[zo+xo+y] = (*outImg)[vo];
					vo += 1;
					xo += ny;
				}
			}
		}
		//perform EDT for all rows
		for (int r = 0; r < nRow; r++ ) {
			flt * imgRow = img3D + (r * ny);
			edt_local(1.0/yDim, imgRow, ny);
		}
		//transpose data back
		vo = v * nvox3D; //volume offset
		for (int z = 0; z < nz; z++ ) {
			int zo = z * nx * ny;
			for (int y = 0; y < ny; y++ ) {
				int xo = 0;
				for (int x = 0; x < nx; x++ ) {
					(*outImg)[vo] = img3D[zo+xo+y];
					vo += 1;
					xo += ny;
				}
			}
		}
		free (img3D);
	} //for each volume

	//EDT in head-foot direction
	nRow = nx * ny; //transpose XYZ to ZXY and blur Z columns with XY Rows
	for (int v = 0; v < nVol; v++ ) { //transpose each volume separately
		flt * img3D = (flt *)calloc(nvox3D*sizeof(flt), 64); //alloc for each volume to allow openmp
		//transpose data
		size_t vo = v * nvox3D; //volume offset
		for (int z = 0; z < nz; z++ ) {
			for (int y = 0; y < ny; y++ ) {
				int yo = y * nz * nx;
				int xo = 0;
				for (int x = 0; x < nx; x++ ) {
					img3D[z+xo+yo] = (*outImg)[vo]*zDim*zDim;
					vo += 1;
					xo += nz;
				}
			}
		}
		//perform EDT for all "rows"
		for (int r = 0; r < nRow; r++ ) {
			flt * imgRow = img3D + (r * nz);
			edt_local(1.0/zDim, imgRow, nz);
		}
		//transpose data back
		vo = v * nvox3D; //volume offset
		for (int z = 0; z < nz; z++ ) {
			for (int y = 0; y < ny; y++ ) {
				int yo = y * nz * nx;
				int xo = 0;
				for (int x = 0; x < nx; x++ ) {
					(*outImg)[vo] = img3D[z+xo+yo]*zDim*zDim;
					vo += 1;
					xo += nz;
				} //x
			} //y
		} //z
		free (img3D);
	} //for each volume
/**/
	return ERROR_NONE;
}

ERROR_NUMBER getNonzeroIndices(int nvox, int *inputImg, int *numIndices, int **indices){
    int *buffer;
    int i, j, voxelValue;
    bool    old;

    // Initialize
    *numIndices = 0;
    if (!(buffer=(int *)calloc(nvox,sizeof(int)))) return ERROR_MEMORY_ALLOCATION;

    // Count unique indices and fill buffer with set of indices
    for (i=0; i<nvox; ++i) if ((voxelValue=inputImg[i])>0){
        old = false;
        // for (j=0; j<*numIndices; ++j) if ((*indices)[j]==voxelValue) old = true;    // Core dump
        for (j=0; j<*numIndices; ++j) if (buffer[j]==voxelValue) old = true;
        if (!old){
            buffer[(*numIndices)++]=voxelValue;
        }
    }

    // Trim output buffer to number of indices
    if (!(*indices=(int *)malloc(*numIndices*sizeof(int)))){
        free(buffer);
        return ERROR_MEMORY_ALLOCATION;
    }
    for (i=0; i<*numIndices; ++i) (*indices)[i] = buffer[i];
    free(buffer);

    return ERROR_NONE;
}

int transposeYZ(float *volume, int nx, int ny, int nz){
	float * buffer;
	int nvox=nx*ny*nz;

	if (!(buffer = (float *)malloc(nvox*sizeof(float)))!=ERROR_NONE) return ERROR_MEMORY_ALLOCATION;

	int z0 = 0;
	for (int z=0; z<nz; ++z){
        int Y = z0;
        for (int y=0; y<ny; ++y){
            for (int x=0; x<nx; ++x){
                buffer[z0+x]=volume[Y];
                buffer[Y++]=volume[z0+x];
            }
        }
        z0+=nx*ny;
    }
	memcpy((void *)volume, (void *)buffer, nvox*sizeof(float));
	free(buffer);

    return ERROR_NONE;
}

int testTransposeFunction(THD_3dim_dataset * din){
    // Get dimensions in voxels
    int nz = DSET_NZ(din);
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);
    int nvox = nx*ny*nz;
    int i, errorNumber;
    float *buffer;

    // Allocate memory to buffer
    if (!(buffer = (float *)malloc(nvox*sizeof(float)))) return ERROR_MEMORY_ALLOCATION;

    // Read image data
    BYTE * img = DSET_ARRAY(din, 0);

    for (i=0; i<nvox; ++i) buffer[i]=img[i];

    // Apply transpose
    if ((errorNumber=transposeYZ(buffer, nx, ny, nz))!=ERROR_NONE){
        free(buffer);
        return errorNumber;
    }

    // Output transpoed volume to afni dataset
    outputTransposedDataSet(buffer, din, nx, nz, ny);

    free(buffer);
    return ERROR_NONE;
}


int outputTransposedDataSet(float *buffer, THD_3dim_dataset *din, int nx, int nz, int ny){
    char *prefix=DSET_PREFIX(din);
    char *searchPath=DSET_DIRNAME(din);
    char *outputFileName;
    char  appendage[]="Transpose";

    // Allocate memory to output name buffer
    if (!(outputFileName=(char *)malloc(strlen(searchPath)+strlen(prefix)+strlen(appendage)+8))){
       return ERROR_MEMORY_ALLOCATION;
    }

    // Determine whether output file already exists
    int outputFileExists = doesFileExist(searchPath,prefix,appendage,outputFileName);

    // Set output dimensions
    THD_ivec3 nxyz={nx, ny, nz};

    // Output Fourier spectrum image (if it does not already exist)
    if (!outputFileExists){
        THD_3dim_dataset *dout = EDIT_empty_copy(din);
        sprintf(outputFileName,"%s%s%s",searchPath,prefix,appendage);
        EDIT_dset_items( dout ,
                        ADN_prefix, outputFileName,
                        ADN_type, MRI_float,
                        ADN_nxyz, nxyz,
                        ADN_none ) ;
        EDIT_substitute_brick(dout, 0, MRI_float, buffer);
        DSET_write(dout);
    }

    // Cleanup
    free(outputFileName);

    return ERROR_NONE;
}

void edt1_local(THD_3dim_dataset * din, flt * df, int n) { //first dimension is simple

    // Get real world voxel sizes
    float xDim = fabs(DSET_DX(din));
    float yDim = fabs(DSET_DY(din));

    xDim=yDim/xDim;
    yDim=1.0f;

	int q, prevX;
	flt prevY, v;
	prevX = 0;
	prevY = INFINITY;
	//forward
	for (q = 0; q < n; q++ ) {
		if (df[q] == 0) {
			// prevX = q*xDim;
			prevX = q;
			prevY = 0;
		} else
			df[q] = sqr((q-prevX)/xDim)+(prevY/yDim);
	}
	//reverse
	prevX = n;
	prevY = INFINITY;
	for (q = (n-1); q >= 0; q-- ) {
		v = sqr((q-prevX)/xDim)+(prevY/yDim);
		if (df[q] < v) {
        	prevX = q;      // DO NOT CHANGE
        	prevY = df[q];  // DO NOT CHANGE
    	} else
        	df[q] = v;
    }
}

static flt vx(flt * f, int p, int q) {
	if ((f[p] == INFINITY) || (f[q] == INFINITY))
		return INFINITY;
	else
		return ((f[q] + q*q) - (f[p] + p*p)) / (2.0*q - 2.0*p);
}

void edt_local(float scale, flt * f, int n) {
	int q, p, k;
	flt s, dx;
	flt * d = (flt *)calloc((n)*sizeof(flt), 64);
	flt * z = (flt *)calloc((n)*sizeof(flt), 64);
	int * v = (int *)calloc((n)*sizeof(int), 64);

    /*# Find the lower envelope of a sequence of parabolas.
    #   f...source data (returns the Y of the parabola vertex at X)
    #   d...destination data (final distance values are written here)
    #   z...temporary used to store X coords of parabola intersections
    #   v...temporary used to store X coords of parabola vertices
    #   i...resulting X coords of parabola vertices
    #   n...number of pixels in "f" to process
    # Always add the first pixel to the enveloping set since it is
    # obviously lower than all parabolas processed so far.*/
    k = 0;
    v[0] = 0;
    z[0] = -INFINITY;
    z[1] = INFINITY;
    for (q = 1; q < n; q++ ) {
	    /* If the new parabola is lower than the right-most parabola in
        # the envelope, remove it from the envelope. To make this
        # determination, find the X coordinate of the intersection (s)
        # between the parabolas with vertices at (q,f[q]) and (p,f[p]).*/
        p = v[k]; // DO NOT CHANGE
        s = vx(f, p,q);
        while (s <= z[k]) {
            k = k - 1;
            p = v[k];       // DO NOT CHANGE
            s = vx(f, p,q); // DO NOT CHANGE
        }
        //# Add the new parabola to the envelope.
        k = k + 1;
        v[k] = q;           // DO NOT CHANGE
        z[k] = s;
        z[k + 1] = INFINITY;
    }
    /*# Go back through the parabolas in the envelope and evaluate them
    # in order to populate the distance values at each X coordinate.*/
    k = 0;
    for (q = 0; q < n; q++ ) {
	    while (z[k + 1] < q)
            k = k + 1;
        dx = (q - v[k]);    // DO NOT CHANGE
        d[q] = (dx * dx + f[v[k]])/(scale*scale);
    }
    for (q = 0; q < n; q++ )
		f[q] = d[q];
	free (d);
	free (z);
	free (v);
}

int doesFileExist(char * searchPath, char * prefix,char *appendage , char * outputFileName){
    int outputFileExists=0;

    struct dirent *dir;
    DIR *d;
    d = opendir(searchPath);
    sprintf(outputFileName,"%s%s",prefix,appendage);
    if (d) {
        while ((dir = readdir(d)) != NULL) {
          if (strstr(dir->d_name,outputFileName)){
            outputFileExists=1;
            break;
          }
        }
        closedir(d);
    }

    return outputFileExists;
}

int open_input_dset(THD_3dim_dataset ** din, char * fname)
{
   *din = THD_open_dataset(fname);
   if( ! *din ) {
      fprintf(stderr,"** failed to read input dataset '%s'\n", fname);
      return ERROR_READING_FILE;
   }

   return ERROR_NONE;
}

int shortToByte(THD_3dim_dataset ** din){
    BYTE * outImg;
    int     i;

    // Get dimensions in voxels
    int nvox = DSET_NVOX(*din);

    if (!(outImg = (BYTE *)calloc(nvox, sizeof(BYTE)))){
        return ERROR_MEMORY_ALLOCATION;
    }

    // Get input data
    int brickType=DSET_BRICK_TYPE(*din, 0);
    if( brickType != MRI_short ) return ERROR_DATATYPENOTHANDLED;
    DSET_load(*din);
    short * img = (short *)DSET_ARRAY(*din, 0);

    // Map short input into BYTE output
    for (i=0; i<nvox; ++i) outImg[i]+=(img[i]!=0);

    THD_3dim_dataset *dout = EDIT_empty_copy(*din);
    char *prefix=DSET_PREFIX(*din);
    char *searchPath=DSET_DIRNAME(*din);
    EDIT_dset_items( dout ,
                    ADN_prefix, strcat(searchPath, prefix),
                    ADN_type, MRI_byte,
                    ADN_none ) ;
    EDIT_substitute_brick(dout, 0, MRI_byte, outImg);

    // Cleanup (data set)
    DSET_delete(*din);
    *din = dout;

    return ERROR_NONE;
}

int Cleanup(char *inputFileName, THD_3dim_dataset *din){

    if (inputFileName) free(inputFileName);

    return 0;
}
