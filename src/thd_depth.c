#include <stdio.h>
#include <dirent.h>

// not supported on standard C, MacOS especially
//   include <malloc.h>

#include <string.h>
#include <unistd.h>
#include <stdbool.h>
#include "mrilib.h"
// #include "distanceField.h"
#include <float.h>
#include "thd_depth.h"

ERROR_NUMBER getDistanceFields(char *inputFileName, char *outputFileName, int metric, bool do_sqrt,
    bool edges_are_zero_for_nz, bool debugMode){

    ERROR_NUMBER    errorNumber;
    THD_3dim_dataset *din = NULL, *dout = NULL;

    // Open dataset
    if ( (errorNumber=open_input_dset(&din, inputFileName))!=ERROR_NONE ){
        Cleanup(inputFileName,  outputFileName, din);
        return errorNumber;
    }

    if ( (errorNumber=getDistanceFieldDataSet(din, &dout, metric, do_sqrt, edges_are_zero_for_nz, debugMode))!=ERROR_NONE ){
        Cleanup(inputFileName,  outputFileName, din);
        return errorNumber;
    }

    if (!outputFileName){        // Default output filename
        char *prefix=DSET_PREFIX(din);
        char *searchPath=DSET_DIRNAME(din);
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

        if (debugMode) sprintf(appendage, "%s", strcat(appendage, "Debug"));

        // Allocate memory to output name buffer
        if (!(outputFileName=(char *)malloc(strlen(searchPath)+strlen(prefix)+strlen(appendage)+8))){
           return ERROR_MEMORY_ALLOCATION;
        }

        sprintf(outputFileName,"%s%s%s",searchPath,prefix,appendage);
    }

    if ( (errorNumber=outputDistanceField(dout, outputFileName))!=ERROR_NONE ){
        Cleanup(inputFileName,  outputFileName, din);
        return errorNumber;
    }
}

int usage(){
    fprintf(stderr, "SYNOPSIS:\n");
        fprintf(stderr, "\tdistanceField -i <input filename> [-o <output filename>][-m <metric>]\n\n");

    fprintf(stderr, "The input file is expected to be an AFNI dataset.\n\n");

    fprintf(stderr, "The \"metric\" is a text string (upper case) describing the algorithm used to\n");
    fprintf(stderr, "estimate the depth. It may be one of the following.\n");
    fprintf(stderr, "MARCHING_PARABOLAS - Marching parabolas (default)\n");
    fprintf(stderr, "EROSION - Erosion algorithm.\n\n");

    fprintf(stderr, "Optional arguments specifically for MARCHING_PARABOLAS:\n");
    fprintf(stderr, "\ts: Square root the output\n");
    fprintf(stderr, "\te: Treat edge of field of view as zero (default)\n");
    fprintf(stderr, "\td: (Debug mode.)  Generate test object set internally\n\n");

    fprintf(stderr, "If the output filename is not specified, a default name is assigned.\n");
    fprintf(stderr, "The default name reflects the input filename, metric and whether \n");
    fprintf(stderr, "the program was run in debug mode.\n");

    return 0;
}

ERROR_NUMBER  getDistanceFieldDataSet(THD_3dim_dataset *din, THD_3dim_dataset **dout, int metric,
    bool do_sqrt, bool edges_are_zero_for_nz, bool debugMode){

    ERROR_NUMBER errorNumber;
    float *outImg;

    if (!(outImg = (float *)calloc(DSET_NVOX(din), sizeof(float)))){
        return ERROR_MEMORY_ALLOCATION;
    }

    // Apply metric
    switch (metric){
    case MARCHING_PARABOLAS:
        if ((errorNumber=afni_edt(din, outImg, do_sqrt, edges_are_zero_for_nz, debugMode))!=ERROR_NONE){
            return errorNumber;
        }
        break;
    case EROSION:
        if ((errorNumber=erosion(din, outImg))!=ERROR_NONE){
            return errorNumber;
        }
        break;
    }

    *dout = EDIT_empty_copy(din);
    EDIT_dset_items( *dout ,
                    ADN_type, MRI_float,
                    ADN_none ) ;

    if (debugMode){
        THD_ivec3 nxyz={20, 40, 80};
        THD_fvec3 xyzdel = {2.0, 1.0, 0.5};

        EDIT_dset_items( *dout ,
                        ADN_nxyz, nxyz,
                        ADN_xyzdel, xyzdel,
                        ADN_none ) ;
    }

    EDIT_substitute_brick(*dout, 0, MRI_float, outImg);

    return ERROR_NONE;
}

int outputDistanceField(THD_3dim_dataset *dout, char *outputFileName){

    // Output Fourier distance image
    EDIT_dset_items( dout ,
                    ADN_prefix, outputFileName,
                    ADN_none ) ;
    DSET_write(dout);

    return ERROR_NONE;
}

int outputDistanceFieldDebug(float *outImg, THD_3dim_dataset *din, char *outputFileName){

    // Set output dimensions
    THD_ivec3 nxyz={debugNx, debugNy, debugNz};
    THD_fvec3 xyzdel = {debugScaleFactors[2], debugScaleFactors[1], debugScaleFactors[0]};

    // Output Fourier distance image
    THD_3dim_dataset *dout = EDIT_empty_copy(din);
    EDIT_dset_items( dout ,
                    ADN_prefix, outputFileName,
                    ADN_type, MRI_float,
                    ADN_nxyz, nxyz,
                    ADN_xyzdel, xyzdel,
                    ADN_none ) ;
    EDIT_substitute_brick(dout, 0, MRI_float, outImg);
    DSET_write(dout);

    return ERROR_NONE;
}

int erosion(THD_3dim_dataset * din, float *outImg){

    // Get dimensions in voxels
    int nz = DSET_NZ(din);
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);
    int nvox = nx*ny*nz;
    int i;
    bool objectVoxelsLeft=TRUE;
    BYTE * buffer;

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


int afni_edt(THD_3dim_dataset * din, float *outImg, bool do_sqrt, bool edges_are_zero_for_nz, bool debugMode){

    // Get dimensions in voxels
    int nz = DSET_NZ(din);
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);
    int nvox = nx*ny*nz;
    int *inputImg;
    int *indices;
    BYTE * byteImg;
    short * shortImg;
    float   *floatImg, ad3[3];
    int *vol;
    int i;

    if ((nvox < 1) || (nx < 2) || (ny < 2) || (nz < 1)) return ERROR_DIFFERENT_DIMENSIONS;

    // Alliocate memory to integer input buffer
    if (!(inputImg=(int *)calloc(nvox,sizeof(int)))) return ERROR_MEMORY_ALLOCATION;

    DSET_load(din);
    int brickType=DSET_BRICK_TYPE(din, 0);
    fprintf(stderr, "brickType=%d\n", brickType);
    switch(brickType){
        case MRI_byte:
            byteImg = DSET_ARRAY(din, 0);
            for (i=0; i<nvox; ++i) inputImg[i] = byteImg[i];
            break;
        case MRI_short:
            shortImg = DSET_ARRAY(din, 0);
            for (i=0; i<nvox; ++i) inputImg[i] = shortImg[i];
            break;
        case MRI_int:
            free(inputImg);
            inputImg = DSET_ARRAY(din, 0);
            break;
        case MRI_float:
            floatImg = DSET_ARRAY(din, 0);
            for (i=0; i<nvox; ++i) inputImg[i] = (int)(floatImg[i]+0.5);
            break;
    }

if (debugMode){
    int x, y, z;
    // DEBUG: Make test volume
    // make a test image: a map of various ROIs
    debugNz = nz = 80;
    debugNy = ny = 40;
    debugNx = nx = 20;
    nvox = nx*ny*nz;
    vol = (int *)calloc(nx*ny*nz, sizeof(int));
    //free(outImg);
    outImg = (float *)calloc(nvox,sizeof(float));
    debugOutImage = outImg;
    ad3[0] = 0.5;
    ad3[1] = 1.0;
    ad3[2] = 2.0;
    memcpy(debugScaleFactors, ad3, 3*sizeof(float));

    // LX, LY, LZ = np.shape(vol)

    int planesize=nx*ny;

    for (z=4; z<18; ++z){
        int zOffset=z*planesize;
        for (y=2; y<8; ++y){
            int yOffset = zOffset + y*nx;
            for (x=4; x<8; ++x){
                vol[yOffset+x] = 1;
            }
        }
    }

    for (z=2; z<8; ++z){
        int zOffset=z*planesize;
        for (y=4; y<18; ++y){
            int yOffset = zOffset + y*nx;
            for (x=4; x<8; ++x){
                vol[yOffset+x] = 4;
            }
        }
    }

    for (z=21; z<30; ++z){
        int zOffset=z*planesize;
        for (y=0; y<10; ++y){
            int yOffset = zOffset + y*nx;
            for (x=11; x<14; ++x){
                vol[yOffset+x] = 7;
            }
        }
    }

    for (z=0; z<nz; ++z){
        int zOffset=z*planesize;
        for (y=0; y<ny; ++y){
            int yOffset = zOffset + y*nx;
            for (x=0; x<nx; ++x){
                if (pow((15-x),2.0) + pow((25-y),2.0) + pow((10-z),2.0)< 31)
                    vol[yOffset+x] = 1;
            }
        }
    }

    for (z=17; z<19; ++z){
        int zOffset=z*planesize;
        for (y=0; y<ny; ++y){
            int yOffset = zOffset + y*nx;
            for (x=3; x<6; ++x){
                vol[yOffset+x] = 1;
            }
        }
    }

    for (z=0; z<nz; ++z){
        int zOffset=z*planesize;
        for (y=19; y<21; ++y){
            int yOffset = zOffset + y*nx;
            for (x=3; x<6; ++x){
                vol[yOffset+x] = 10;
            }
        }
    }

    for (z=60; z<70; ++z){
        int zOffset=z*planesize;
        for (y=28; y<36; ++y){
            int yOffset = zOffset + y*nx;
            for (x=14; x<19; ++x){
                vol[yOffset+x] = 17;
            }
        }
    }

    for (z=0; z<nz; ++z){
        int zOffset=z*planesize;
        for (y=0; y<ny; ++y){
            int yOffset = zOffset + y*nx;
            for (x=0; x<nx; ++x){
                if ((pow((65-x),2.0) + pow((10-y),2.0) + pow((10-z),2.0)< 75) &&
                    !(pow((60-x),2.0) + pow((7-y),2.0) + pow((10-z),2.0)< 31))
                    vol[yOffset+x] = 2;
            }
        }
    }

    for (z=40; z<56; ++z){
        int zOffset=z*planesize;
        for (y=25; y<33; ++y){
            int yOffset = zOffset + y*nx;
            for (x=4; x<8; ++x){
                vol[yOffset+x] = 5;
            }
        }
    }
}

#if PROCESS_ROIS_SEPARATELY
    // Get unique nonzero index values
    if ((errorNumber=getNonzeroIndices(nvox, inputImg, &numIndices, &indices))!=ERROR_NONE){
        free(inputImg);
        return errorNumber;
    }

    // Process each index
    for (i=0; i<numIndices; ++i){
        if ((errorNumber=processIndex(indices[i], inputImg, &addend, din))!=ERROR_NONE){
            free(inputImg);
            free(indices);
            return errorNumber;
        }
        for (j=0; j<nvox; ++j) outImg[j]+=addend[j];
    }

    free(indices);
#else
if (debugMode){
    inputImg = vol;
} else {
    // Get real world voxel sizes
    // float ad3[3]={fabs(DSET_DX(din)), fabs(DSET_DY(din)), fabs(DSET_DZ(din))};
    ad3[0] = fabs(DSET_DX(din));
    ad3[1] = fabs(DSET_DY(din));
    ad3[2] = fabs(DSET_DZ(din));
}

    img3d_Euclidean_DT(inputImg, nx, ny, nz,
                       do_sqrt, edges_are_zero_for_nz, ad3, outImg);
#endif

    // Cleanup
    free(inputImg);

    return ERROR_NONE;
}

ERROR_NUMBER img3d_Euclidean_DT(int *im, int nx, int ny, int nz,
    bool do_sqrt, bool edges_are_zero_for_nz, float *ad3, float *odt){

    int nvox = nx*ny*nz;           // number of voxels
    int planeSize = nx*ny;
    int i, z, y, x;

    // initialize the "output" or answer array
    // for (i=0; i<nvox; ++i) odt[i] = (im[i]>0)? BIG : 0;
    for (i=0; i<nvox; ++i) odt[i] = BIG;

    // first pass: start with all BIGs (along x-axes)
    int *inRow = im;
    float *outRow = odt;
    for (z = 0; z <nz; ++z){
        for (y = 0; y < ny; ++y){
            // Calc with it, and save results
            // [PT: Jan 5, 2020] fix which index goes here
            run_EDTD_per_line( inRow, outRow, nx, ad3[0], edges_are_zero_for_nz) ;

            // Increment row
            inRow += nx;
            outRow += nx;
        }
    }

    if (!(inRow=(int *)malloc(ny*sizeof(int)))) return ERROR_MEMORY_ALLOCATION;
    if (!(outRow=(float *)calloc(ny,sizeof(float)))) {
        free(inRow);
        return ERROR_MEMORY_ALLOCATION;
    }
    for (z = 0; z < nz; ++z){
        int *inPlane = im + (z*planeSize);
        float *outPlane = odt + (z*planeSize);
        for (x = 0; x < nx; ++x){
            // get a line...
            for (y=0; y<ny; ++y){
                int offset = (y*nx) + x;
                inRow[y] = inPlane[offset ];
                outRow[y] = outPlane[offset ];
            }

            // ... and then calc with it, and save results
            run_EDTD_per_line( inRow, outRow, ny, ad3[1], edges_are_zero_for_nz) ;

            // Record new output row
            for (y=0; y<ny; ++y){
                int offset = (y*nx) + x;
                outPlane[offset] = outRow[y];
            }
        }
    }
    free(outRow);
    free(inRow);


    // 2nd pass: start from previous; any other dimensions would carry
    // on from here
    if (!(inRow=(int *)malloc(nz*sizeof(int)))) return ERROR_MEMORY_ALLOCATION;
    if (!(outRow=(float *)calloc(nz, sizeof(float)))) {
        free(inRow);
        return ERROR_MEMORY_ALLOCATION;
    }
    for (y = 0; y < ny; ++y){
        for (x = 0; x < nx; ++x){
            int planeOffset = (y*nx) + x;
            // get a line...
            for (z=0; z<nz; ++z){
                int offset = planeOffset + (z*planeSize);
                inRow[z] = im[offset] ;
                outRow[z] = odt[offset] ;
            }

            // ... and then calc with it, and save results
            // [PT: Jan 5, 2020] fix which index goes here
            run_EDTD_per_line( inRow, outRow, nz, ad3[2], edges_are_zero_for_nz) ;

            // Record new output row
            for (z=0; z<nz; ++z) {
                int offset = planeOffset + (z*planeSize);
                odt[offset] = outRow[z];
            }
        }
    }
    free(outRow);
    free(inRow);

    if (do_sqrt)
        for (i=0; i<nvox; ++i) odt[i] = sqrt(odt[i]);

    return ERROR_NONE;
}

ERROR_NUMBER run_EDTD_per_line(int *roi_line, float *dist2_line, int Na,
                       float delta, bool edges_are_zero_for_nz) {
    int  idx = 0;
    int  m, n, i;
    float   *line_out;

    size_t  rowLengthInBytes = Na*sizeof(float);
    if (!(line_out=(float *)malloc(rowLengthInBytes))) return ERROR_MEMORY_ALLOCATION;

    int limit = Na-1;
    while (idx < Na){
        // get interval of line with current ROI value
        int roi = roi_line[idx];
        n = idx;
        while (n < Na){
            if (roi_line[n] != roi){
                break;
            }
            n += 1;
        }
        n -= 1;
        // n now has the index of last matching element

        float *paddedLine=(float *)calloc(Na+2,sizeof(float));
        // actual ROI is in range of indices [start, stop] in
        // paddedLine.  'inc' will tell us length of distance array
        // put into Euclidean_DT_delta(), which can include padding at
        // either end.
        int start = 0, stop = limit;
        int inc=0;
        if (idx != 0 || (edges_are_zero_for_nz && roi != 0)){
            start = 1;
            inc = 1;
        }
        // put actual values from dist**2 field...
        for (m=idx; m<=n; ++m){
            //paddedLine[inc++] = dist2_line[m];
            paddedLine[inc++] = dist2_line[m];
        }
        // inc finishes 1 greater than the actual end: is length so far
        stop = inc-1;  // 'stop' is index of actual end of roi
        // pad at end?
        if (n < limit || (edges_are_zero_for_nz && roi != 0)){
            inc+=1; // [PT] not 'stop', which is index of actual ROI
                    // (not changing)
        }

        // float *Df = Euclidean_DT_delta(paddedLine, stop-start+1, delta);
        // [PT] and 'inc' should already have correct value from
        // above; don't add 1 here
        float *Df = Euclidean_DT_delta(paddedLine, inc, delta);

        // memcpy(&(line_out[idx]), &(Df[start]), (stop-start+1)*sizeof(float));
        memcpy(&(line_out[idx]), &(Df[start]), (stop-start+1)*sizeof(float));

        free(paddedLine);
        free(Df);

        idx = n+1;
    }

    // DEBUG
    for (i=0; i<Na; ++i) if (line_out[i]==0){
        fprintf(stderr, "Zero valued distance\n");
    }

    memcpy(dist2_line, line_out, rowLengthInBytes);
    free(line_out);

    return ERROR_NONE;
}

float * Euclidean_DT_delta(float *f, int n, float delta){
//    Classical Euclidean Distance Transform (EDT) of Felzenszwalb and
//        Huttenlocher (2012), but for given voxel lengths.
//
//    Assumes that: len(f) < sqrt(10**10).
//
//    In this version, all voxels should have equal length, and units
//    are "edge length" or "number of voxels."
//
//    Parameters
//    ----------
//
//    f     : 1D array or list, distance**2 values (or, to start, binarized
//    between 0 and BIG).
//
//    delta : voxel edge length size along a particular direction


    int *v, k = 0;
    float *z, *Df;
    int q, r;

    if (!(v=(int *)calloc(n, sizeof(int))) ||
        !(z=(float *)calloc(n+1, sizeof(float)))){
            if (v) free(v);
            return NULL;
    }
    z[0] = -BIG;
    z[1] =  BIG;

    for (q = 1; q<n; ++q) {
        float s = f[q] + pow(q*delta, 2.0) - (f[v[k]] + pow(v[k]*delta,2.0));
        s/= 2. * delta * (q - v[k]);
        while (s <= z[k]){
            k-= 1;
            s = f[q] + pow(q*delta,2.0) - (f[v[k]] + pow(v[k]*delta, 2.0));
            s/= 2. * delta * (q - v[k]);
        }
        k+= 1;
        v[k]   = q;
        z[k]   = s;
        z[k+1] = BIG;
    }

    k   = 0;
    if (!(Df=(float *)calloc(n, sizeof(float)))){
        free(v);
        free(z);
        return NULL;
    }
    for (q=0; q<n; ++q){
        while (z[k+1] < q * delta) k+= 1;
        Df[q] = pow(delta*(q - v[k]), 2.0) + f[v[k]];
    }

    free(v);
    free(z);

    return Df;
}

ERROR_NUMBER processIndex(int index, int *inputImg, float **outImg, THD_3dim_dataset *din){
    // Get dimensions in voxels
    int    nz = DSET_NZ(din);
    int    ny = DSET_NY(din);
    int    nx = DSET_NX(din);
    int    nvox = nx*ny*nz;
    int    r, v, x, y, z;

    int    nVol = 1;
    int    nvox3D = nx * ny * MAX(nz, 1);
    size_t si;

    nVol = nvox / nvox3D;
    if ((nvox3D * nVol) != nvox) return ERROR_DIFFERENT_DIMENSIONS;

    // Get real world voxel sizes
    float xDim = fabs(DSET_DX(din));
    float yDim = fabs(DSET_DY(din));
    float zDim = fabs(DSET_DZ(din));

    float yDimSqrd = yDim*yDim;
    float zDimSqrd = zDim*zDim;

    if (!(*outImg=(float *)calloc(nvox, sizeof(float)))) return ERROR_MEMORY_ALLOCATION;

    for (si = 0; si < nvox; si++ ) {
        if (inputImg[si] == index)
            (*outImg)[si] = BIG;
    }
    size_t nRow = ny*nz;

    //EDT in left-right direction
    for (r = 0; r < nRow; r++ ) {
        flt * imgRow = (*outImg) + (r * nx);
        edt_local(xDim, imgRow, nx);
    }

    //EDT in anterior-posterior direction
    nRow = nx * nz; //transpose XYZ to YXZ and blur Y columns with XZ Rows
    for (v = 0; v < nVol; v++ ) { //transpose each volume separately
        flt * img3D = (flt *)calloc(nvox3D*sizeof(flt), 64); //alloc for each volume to allow openmp

        //transpose data
        size_t vo = v * nvox3D; //volume offset
        for (z = 0; z < nz; z++ ) {
            int zo = z * nx * ny;
            for (y = 0; y < ny; y++ ) {
                int xo = 0;
                for (x = 0; x < nx; x++ ) {
                    img3D[zo+xo+y] = (*outImg)[vo]/yDimSqrd;
                    vo += 1;
                    xo += ny;
                }
            }
        }
        //perform EDT for all rows
        for (r = 0; r < nRow; r++ ) {
            flt * imgRow = img3D + (r * ny);
            edt_local(yDim, imgRow, ny);
        }
        //transpose data back
        vo = v * nvox3D; //volume offset
        for (z = 0; z < nz; z++ ) {
            int zo = z * nx * ny;
            for (y = 0; y < ny; y++ ) {
                int xo = 0;
                for (x = 0; x < nx; x++ ) {
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
    for (v = 0; v < nVol; v++ ) { //transpose each volume separately
        flt * img3D = (flt *)calloc(nvox3D*sizeof(flt), 64); //alloc for each volume to allow openmp
        //transpose data
        size_t vo = v * nvox3D; //volume offset
        for (z = 0; z < nz; z++ ) {
            for (y = 0; y < ny; y++ ) {
                int yo = y * nz * nx;
                int xo = 0;
                for (x = 0; x < nx; x++ ) {
                    img3D[z+xo+yo] = (*outImg)[vo]/zDimSqrd;
                    vo += 1;
                    xo += nz;
                }
            }
        }
        //perform EDT for all "rows"
        for (r = 0; r < nRow; r++ ) {
            flt * imgRow = img3D + (r * nz);
            edt_local(zDim, imgRow, nz);
        }
        //transpose data back
        vo = v * nvox3D; //volume offset
        for (z = 0; z < nz; z++ ) {
            for (y = 0; y < ny; y++ ) {
                int yo = y * nz * nx;
                int xo = 0;
                for (x = 0; x < nx; x++ ) {
                    (*outImg)[vo] = img3D[z+xo+yo];
                    vo += 1;
                    xo += nz;
                } //x
            } //y
        } //z
        free (img3D);
    } //for each volume

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

flt vx(flt * f, int p, int q) {
    if ((f[p] == BIG) || (f[q] == BIG))
        return BIG;
    else
        return ((f[q] + q*q) - (f[p] + p*p)) / (2.0*q - 2.0*p);
}

void edt_local(float scale, flt * f, int n) {
    float scaleSqrd = scale*scale;

    int q, p, k;
    flt s, dx;
    flt * d = (flt *)calloc((n)*sizeof(flt), 64);
    flt * z = (flt *)calloc((n)*sizeof(flt), 64);
    int * v = (int *)calloc((n)*sizeof(int), 64);

//    # Find the lower envelope of a sequence of parabolas.
//    #   f...source data (returns the Y of the parabola vertex at X)
//    #   d...destination data (final distance values are written here)
//    #   z...temporary used to store X coords of parabola intersections
//    #   v...temporary used to store X coords of parabola vertices
//    #   i...resulting X coords of parabola vertices
//    #   n...number of pixels in "f" to process
//    # Always add the first pixel to the enveloping set since it is
//    # obviously lower than all parabolas processed so far.

    k = 0;
    v[0] = 0;
    z[0] = -BIG;
    z[1] = BIG;

    for (q = 1; q < n; q++ ) {
//      If the new parabola is lower than the right-most parabola in
//        # the envelope, remove it from the envelope. To make this
//        # determination, find the X coordinate of the intersection (s)
//        # between the parabolas with vertices at (q,f[q]) and (p,f[p]).
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
        z[k + 1] = BIG;
    }
//    # Go back through the parabolas in the envelope and evaluate them
//    # in order to populate the distance values at each X coordinate.
    k = 0;
    for (q = 0; q < n; q++ ) {
        while (z[k + 1] < q)
            k = k + 1;
        dx = (q - v[k])*scale;
        // d[q] = dx * dx + f[v[k]];
        d[q] = dx * dx + f[v[k]]*scaleSqrd;
    }
    for (q = 0; q < n; q++ )
        f[q] = d[q];
    free (d);
    free (z);
    free (v);
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

int Cleanup(char *inputFileName, char *outputFileName, THD_3dim_dataset *din){

    if (inputFileName) free(inputFileName);
    if (outputFileName) free(outputFileName);

    return 0;
}

float sqr(float x){
    return x*x;
}

