/*
    commonLinesReg -- Peter Lauren, 2020-08-21

SYNOPSIS

    commonLinesReg -i <Input Filename> -p <ac|as|cs|acs>

DESCRIPTION

    Uses common lines algorithm to linearly align two volumes.  It does this by finding where orthogonal volumes intersect
    in Fourier space.  If they are rotationally aligned, they should intersect on the appropriate orthogonal cardinal axes.
    The slope of the phase shift, along the lines of intersection, reflect the translational misalignment.  The translations,
    and rotations, are each determined by solving a linear system.  These solutions are completely independent of each other.

NOTES

    1/ The current implementation simply generates the projections and outputs them as JPEG files.
    2/ Two to three projection axes must be chosen:
        - ac: Axial and Coronal
        - as: Axial and Sagital
        - cs: Coronal and Sagital
        - acs: Axial, Coronal and Sagital

*/

#include <stdio.h>
#include <dirent.h>
#include <malloc.h>
#include <string.h>
#include <unistd.h>
#include "mrilib.h"

int Cleanup(char *inputFileName, char *outputFileNames[], THD_3dim_dataset *din);
int open_input_dset(THD_3dim_dataset ** din, char * fname);
int unweave_sections(THD_3dim_dataset *din, THD_3dim_dataset **dodd, THD_3dim_dataset **deven);
int makeProjection(THD_3dim_dataset *din, THD_3dim_dataset **dout, char projCode);

int main( int argc, char *argv[] )  {
    THD_3dim_dataset * din = NULL, *dodd, *deven, *doddSagProj;
    int     i;
    char    *inputFileName=NULL, projectionString[3]={'a','s','\0'};
    char*    outputFileNames[3]={NULL,NULL,NULL};

    for (i=0; i<argc; ++i) if (argv[i][0]=='-'){
        switch(argv[i][1]){
        case 'i':
            if (!(inputFileName=(char*)malloc(strlen(argv[++i])+8)))
                return Cleanup(inputFileName, outputFileNames, din);
            sprintf(inputFileName,"%s",argv[i]);
            break;

        case 'p':
            sprintf(projectionString,"%s",argv[++i]);
            break;
        }
    }

    if( open_input_dset(&din, inputFileName) )
    return Cleanup(inputFileName, outputFileNames, din);

    // Split volume into odd and even slices
    if (!unweave_sections(din, &dodd, &deven))
        return Cleanup(inputFileName, outputFileNames, din);

    // Make projections
    makeProjection(dodd, &doddSagProj, 'c');
    // TODO: Add code

    // Free up 3D datasets
    // TODO: Add code

    // Float projections
    // TODO: Add code

    // Zero pad projections
    // TODO: Add code

    // Make Fourier transforms of projections
    // TODO: Add code

    // Output Fourier spectra, of projections, to a common image file format.
    // TODO: Add code

    // Make CSV file of radial phase shift linearity (RPSL) between pairs of FTs
    // TODO: Add code

    // TODO: Add code
    Cleanup(inputFileName, outputFileNames, din);
    return 1;
}

int makeProjection(THD_3dim_dataset *din, THD_3dim_dataset **dout, char projCode){
    int  nz, ny, nx, inInc, outInc, rows, cols, x;
    char *prefix=DSET_PREFIX(din);
    char *searchPath=DSET_DIRNAME(din);
    char *outputCode=(projCode=='a')? "Axial" : ((projCode=='c')? "Coronal" : "Sagital");
    char *outputFileName;
    int  outputFileExists=0, outPixelCount, start;
    THD_ivec3 iv_nxyz;
    float   *outData=NULL;

    // Determine input dimensions
    nz = DSET_NZ(din);
    ny = DSET_NY(din);
    nx = DSET_NX(din);

    // Get input voxel data
    float   *indata = DSET_ARRAY(din, 0);

    // Apply required projection
    switch (projCode){
    case 'a':        // Axial projection

        // Determine output dimansions
        rows=ny;
        cols=nx;

        // Allocate memory to output voxel data
        outPixelCount=rows*cols;
        if (!(outData=(float *)calloc(outPixelCount,sizeof(float)))) return 0;

        // Fill output data
        start=0;
        for (inInc=0, outInc=0; outInc<outPixelCount; ++outInc){
            inInc=start;
            for (x=0; x<nz; ++x){
                outData[outInc]+=indata[inInc];
                inInc+=outPixelCount;
            }
            start+=1;
        }
        break;
    case 'c':       // Coronal projection

        // Determine output dimansions
        rows=nz;
        cols=nx;

        // Allocate memory to output voxel data
        outPixelCount=rows*cols;
        if (!(outData=(float *)calloc(outPixelCount,sizeof(float)))) return 0;

        // Fill output data
        start=0;
        // int yCount=ny*nz;
        int startInc=nx*ny;
        int z, i;
        for (outInc=outPixelCount-1, z=0; z<nz; ++z){
            start=z*nx*ny;
            for (x=0; x<nx; ++x){
                inInc=start;
                for (i=0; i<ny; ++i){
                    outData[outInc]+=indata[inInc];
                    inInc+=nx;
                }
                start+=1;
                --outInc;
            }
        }
        break;
    case 's':       // Sagital projection

        // Determine output dimansions
        rows=nz;
        cols=ny;

        // Allocate memory to output voxel data
        outPixelCount=rows*cols;
        if (!(outData=(float *)calloc(outPixelCount,sizeof(float)))) return 0;

        // Fill output data
        for (inInc=0, outInc=rows*cols-1; outInc>=0; --outInc){
            for (x=0; x<nx; ++x) outData[outInc]+=indata[inInc++];
        }
        break;
    }

    // Allocate memory to output name buffer
    if (!(outputFileName=(char *)malloc(strlen(searchPath)+strlen(prefix)+64))){
       free(outData);
       return 0;
    }

    // Determine whether output file already exists
    struct dirent *dir;
    DIR *d;
    d = opendir(searchPath);
    sprintf(outputFileName,"%s%s",prefix,outputCode);
    if (d) {
        while ((dir = readdir(d)) != NULL) {
          if (strstr(dir->d_name,outputFileName)){
            outputFileExists=1;
          }
        }
        closedir(d);
    }

    /* Output 2D projection volume */
    *dout = EDIT_empty_copy(din);
    LOAD_IVEC3( iv_nxyz , cols , rows , 1 ) ;
    sprintf(outputFileName,"%s%s%s",searchPath,prefix,outputCode);
    EDIT_dset_items( *dout ,
                    ADN_prefix, outputFileName,
                    ADN_nxyz      , iv_nxyz ,
                    ADN_none ) ;
    EDIT_substitute_brick(*dout, 0, MRI_float, outData);
    if( !outputFileExists ) {  // If even file does not already exist
       DSET_write(*dout);
    }


    // Cleanup  (Don't free evenData or oddData)
    free(outputFileName);

    return 1;
}

int unweave_sections(THD_3dim_dataset *din, THD_3dim_dataset **dodd, THD_3dim_dataset **deven){
    char    *outputFileName;
    int i, nz, ny, nx, nodd, neven, readInc, inIndex, outIndex, planeSize;
    size_t  bytes2Copy;
    float * indata, *oddData=NULL, *evenData=NULL;
    THD_ivec3 iv_nxyz;
    int bEvenFileExists=0, bOddFileExists=0;

    // Determine input dimensions
    nz = DSET_NZ(din);
    ny = DSET_NY(din);
    nx = DSET_NX(din);

    // Determine output section counts
    neven=(int)((nz+1)/2);
    nodd=nz-neven;

    // Determine plane size read increment
    planeSize = nx*ny;
    readInc = 2*planeSize;

    // Allocate memory to output voxel data
    bytes2Copy=planeSize*sizeof(float);
    if (!(oddData=(float *)malloc(nodd*bytes2Copy)) ||
        !(evenData=(float *)malloc(neven*bytes2Copy))){
            if (oddData) free(oddData);
            return 0;
        }

    // Fill even data
    indata = DSET_ARRAY(din, 0);
    inIndex=outIndex=0;
    for (i=0; i<neven; ++i){
        memcpy((void *)&evenData[outIndex],(void *)&indata[inIndex],bytes2Copy);
        inIndex+=readInc;
        outIndex+=planeSize;
    }

    // Fill odd data
    outIndex=0;
    inIndex=planeSize;
    for (i=0; i<nodd; ++i){
        memcpy((void *)&oddData[outIndex],(void *)&indata[inIndex],bytes2Copy);
        inIndex+=readInc;
        outIndex+=planeSize;
    }

    // Get search path, and prefix, from dataset
    char *prefix=DSET_PREFIX(din);
    char *searchPath=DSET_DIRNAME(din);

    // Allocate memory to output name buffer
    if (!(outputFileName=(char *)malloc(strlen(searchPath)+strlen(prefix)+32))){
       free(oddData);
       free(evenData);
       return 0;
    }

    // Determine whether files actually exist
    struct dirent *dir;
    DIR *d;
    d = opendir(searchPath);
    if (d) {
        while ((dir = readdir(d)) != NULL) {
          sprintf(outputFileName,"%sEven",prefix);
          if (strstr(dir->d_name,outputFileName)){
            bEvenFileExists=1;
          } else {
              sprintf(outputFileName,"%sOdd",prefix);
              if (strstr(dir->d_name,outputFileName)){
                bOddFileExists=1;
              }
          }
        }
        closedir(d);
      }


   /* Output even volume */
   *deven = EDIT_empty_copy(din);
   LOAD_IVEC3( iv_nxyz , nx , ny , neven ) ;
   sprintf(outputFileName,"%s%sEven",searchPath,prefix);
   EDIT_dset_items( *deven ,
                    ADN_prefix, outputFileName,
                    ADN_nxyz      , iv_nxyz ,
                    ADN_none ) ;
   EDIT_substitute_brick(*deven, 0, MRI_float, evenData);
   if( !bEvenFileExists ) {  // If even file does not already exist
       DSET_write(*deven);
    }

   *dodd = EDIT_empty_copy(din);
   LOAD_IVEC3( iv_nxyz , nx , ny , nodd ) ;
   sprintf(outputFileName,"%s%sOdd",searchPath,prefix);
   EDIT_dset_items( *dodd ,
                    ADN_prefix, outputFileName,
                    ADN_nxyz      , iv_nxyz ,
                    ADN_none ) ;
   EDIT_substitute_brick(*dodd, 0, MRI_float, oddData);
   if( !bOddFileExists ) {  // If odd file does not already exist
       DSET_write(*dodd);
    }

   // Cleanup  (Don't free evenData or oddData)
   free(outputFileName);

    return 1;
}

int open_input_dset(THD_3dim_dataset ** din, char * fname)
{
    int brickType;
   *din = THD_open_dataset(fname);
   if( ! *din ) {
      fprintf(stderr,"** failed to read input dataset '%s'\n", fname);
      return 1;
   }

   /* refuse to work with anything but float here */
   brickType=DSET_BRICK_TYPE(*din, 0);
   if( DSET_BRICK_TYPE(*din, 0) != MRI_float ) {
      fprintf(stderr,"** input must be of type float, failing...\n");
      return 1;
   }

   /* data is not automatically read in, do it now */
   DSET_load(*din);

   return 0;
}

int Cleanup(char *inputFileName, char *outputFileNames[], THD_3dim_dataset *din){
    int i;

    if (inputFileName) free(inputFileName);
    for (i=0; i<3; ++i) if (outputFileNames[i]) free(outputFileNames[i]);
    if (din) free(din);

    return 0;
}
