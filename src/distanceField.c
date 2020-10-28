#include <stdio.h>
#include <dirent.h>
#include <malloc.h>
#include <string.h>
#include <unistd.h>
#include "mrilib.h"
#include "errors.h"
#include "Generic.h"
#include <float.h>


typedef float flt;

typedef enum METRIC_TYPE {
         MARCHING_PARABOLAS
 } METRIC_TYPE ;


int Cleanup(char *inputFileName, THD_3dim_dataset *din);
static int afni_edt(THD_3dim_dataset * din, float **outImg);
int open_input_dset(THD_3dim_dataset ** din, char * fname);
int outputDistanceField(float *outImg, THD_3dim_dataset *din, int metric);
int doesFileExist(char * searchPath, char * prefix,char *appendage , char * outputFileName);
void edt1_local(THD_3dim_dataset * din, flt * df, int n);
void edt_local(float scale, flt * f, int n);
float sqr(float x);
static flt vx(flt * f, int p, int q);

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
            metric = atoi(argv[++i]);
            if (!(inputFileName=(char*)malloc(strlen(argv[++i])+8)))
                return Cleanup(inputFileName,  din);
            sprintf(inputFileName,"%s",argv[i]);
            break;
        }
    }

    // Open dataset
    if ( (errorNumber=open_input_dset(&din, inputFileName))!=ERROR_NONE ){
        Cleanup(inputFileName, din);
        return errorNumber;
    }

    // Apply metric
    switch (metric){
    case MARCHING_PARABOLAS:
        if ((errorNumber=afni_edt(din, &outImg))!=ERROR_NONE){
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


static int afni_edt(THD_3dim_dataset * din, float **outImg){

    // Get dimensions in voxels
    int nz = DSET_NZ(din);
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);
    int nvox = nx*ny*nz;

    // Get real world voxel sizes
    int yDim = fabs(DSET_DY(din));
    int zDim = fabs(DSET_DZ(din));

	if ((nvox < 1) || (nx < 2) || (ny < 2) || (nz < 1)) return ERROR_DIFFERENT_DIMENSIONS;

    int brickType=DSET_BRICK_TYPE(din, 0);
    if( brickType != MRI_byte ) return ERROR_DATATYPENOTHANDLED;
    BYTE * img = DSET_ARRAY(din, 0);
    *outImg = (float *)calloc(nvox, sizeof(float));
	int nVol = 1;
	int nvox3D = nx * ny * MAX(nz, 1);
	nVol = nvox / nvox3D;
	if ((nvox3D * nVol) != nvox) return ERROR_DIFFERENT_DIMENSIONS;
	flt threshold = 0;
	for (size_t i = 0; i < nvox; i++ ) {
		if (img[i] > threshold)
			(*outImg)[i] = INFINITY;
	}
	size_t nRow = ny;

/*	for (int i = 2; i < 8; i++ )
		nRow *= MAX(nim->dim[i],1);
		*/
	//EDT in left-right direction
	for (int r = 0; r < nRow; r++ ) {
		flt * imgRow = *outImg + (r * nx);
		// edt1(imgRow, nx);
		edt1_local(din, imgRow, nx);
	}

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
			edt_local(yDim, imgRow, ny);
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
	#pragma omp parallel for
	for (int v = 0; v < nVol; v++ ) { //transpose each volume separately
		flt * img3D = (flt *)calloc(nvox3D*sizeof(flt), 64); //alloc for each volume to allow openmp
		//transpose data
		size_t vo = v * nvox3D; //volume offset
		for (int z = 0; z < nz; z++ ) {
			for (int y = 0; y < ny; y++ ) {
				int yo = y * nz * nx;
				int xo = 0;
				for (int x = 0; x < nx; x++ ) {
					img3D[z+xo+yo] = (*outImg)[vo];
					vo += 1;
					xo += nz;
				}
			}
		}
		//perform EDT for all "rows"
		for (int r = 0; r < nRow; r++ ) {
			flt * imgRow = img3D + (r * nz);
			edt_local(zDim, imgRow, nz);
		}
		//transpose data back
		vo = v * nvox3D; //volume offset
		for (int z = 0; z < nz; z++ ) {
			for (int y = 0; y < ny; y++ ) {
				int yo = y * nz * nx;
				int xo = 0;
				for (int x = 0; x < nx; x++ ) {
					(*outImg)[vo] = img3D[z+xo+yo];
					vo += 1;
					xo += nz;
				} //x
			} //y
		} //z
		free (img3D);
	} //for each volume

	return 0;
}

void edt1_local(THD_3dim_dataset * din, flt * df, int n) { //first dimension is simple

    // Get real world voxel sizes
    float xDim = fabs(DSET_DX(din));
    float yDim = fabs(DSET_DY(din));

	int q, prevX;
	flt prevY, v;
	prevX = 0;
	prevY = INFINITY;
	//forward
	for (q = 0; q < n; q++ ) {
		if (df[q] == 0) {
			prevX = q;
			prevY = 0;
		} else
			df[q] = sqr((q-prevX)*xDim)+(prevY*yDim);
	}
	//reverse
	prevX = n;
	prevY = INFINITY;
	for (q = (n-1); q >= 0; q-- ) {
		v = sqr((q-prevX)*xDim)+(prevY*yDim);
		if (df[q] < v) {
        	prevX = q;
        	prevY = df[q];
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
        p = v[k];
        s = vx(f, p,q);
        while (s <= z[k]) {
            k = k - 1;
            p = v[k];
            s = vx(f, p,q);
        }
        //# Add the new parabola to the envelope.
        k = k + 1;
        v[k] = q;
        z[k] = s;
        z[k + 1] = INFINITY;
    }
    /*# Go back through the parabolas in the envelope and evaluate them
    # in order to populate the distance values at each X coordinate.*/
    k = 0;
    for (q = 0; q < n; q++ ) {
	    while (z[k + 1] < q)
            k = k + 1;
        dx = scale*(q - v[k]);
        d[q] = dx * dx + f[v[k]];
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

   /* refuse to work with anything but byte here */
   int brickType=DSET_BRICK_TYPE(*din, 0);
   if( brickType == MRI_byte ) {
       /* data is not automatically read in, do it now */
       DSET_load(*din);
   } else {
      fprintf(stderr,"** input must be of type byte, failing...\n");
      return 1;
   }

   return ERROR_NONE;
}

int Cleanup(char *inputFileName, THD_3dim_dataset *din){
    int i, j, ny;

    if (inputFileName) free(inputFileName);

    return 0;
}
