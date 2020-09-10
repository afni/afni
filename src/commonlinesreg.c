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

    1/ The current implementation aligns volumes made from unweaving the interlaced sections of a brick.
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

typedef struct{
    float real;
    float imag;
} COMPLEX;

int Cleanup(char *inputFileName, char *outputFileNames[], THD_3dim_dataset *din, COMPLEX **TwoDFt);
int open_input_dset(THD_3dim_dataset ** din, char * fname);
int unweave_sections(THD_3dim_dataset *din, THD_3dim_dataset **dodd, THD_3dim_dataset **deven);
int makeProjection(THD_3dim_dataset *din, THD_3dim_dataset **dout, char projCode);
int float2DImage(THD_3dim_dataset *dset);
int getLargestDimension(THD_3dim_dataset *din);
int zeroPadProjection(THD_3dim_dataset *din, THD_3dim_dataset **dout, int largestDimension, int factor);
int doesFileExist(char * searchPath, char * prefix,char *appendage , char * outputFileName);
int FFT2D(COMPLEX **c,int nx,int ny,int dir);
int FFT(int dir,int m,double *x,double *y);
int Powerof2(int n,int *m,int *twopm);
int get2DFourierTransform(THD_3dim_dataset *din, COMPLEX ***TwoDFT);

int main( int argc, char *argv[] )  {
    THD_3dim_dataset * din = NULL, *dodd, *deven, *doddSagProj, *doddSagProjPad;
    int     i, largestDimension, paddingFactor=2;
    char    *inputFileName=NULL, projectionString[3]={'a','s','\0'};
    char*    outputFileNames[3]={NULL,NULL,NULL};
    COMPLEX **TwoDFt=NULL;

    for (i=0; i<argc; ++i) if (argv[i][0]=='-'){
        switch(argv[i][1]){
        case 'i':
            if (!(inputFileName=(char*)malloc(strlen(argv[++i])+8)))
                return Cleanup(inputFileName, outputFileNames, din, **TwoDFt);
            sprintf(inputFileName,"%s",argv[i]);
            break;

        case 'p':
            sprintf(projectionString,"%s",argv[++i]);
            break;
        }
    }

    if( open_input_dset(&din, inputFileName) )
    return Cleanup(inputFileName, outputFileNames, din, TwoDFt);

    // Split volume into odd and even slices
    if (!unweave_sections(din, &dodd, &deven))
        return Cleanup(inputFileName, outputFileNames, din, TwoDFt);

    // Make projections
    makeProjection(dodd, &doddSagProj, 's');

    // Get largest dimension from data set
    largestDimension=getLargestDimension(dodd);

    // Free up 3D datasets
    DSET_delete(din);

    // Float projections
    // Currently skipped because of head voxels on the edge of the projection image.  Apart from that,
    //  the edge defaults to zero.  Floating creates edge effects rather than suppressing them.
    // float2DImage(doddSagProj);

    // Zero pad projections
    if (!(zeroPadProjection(doddSagProj, &doddSagProjPad, largestDimension, paddingFactor)))
        Cleanup(inputFileName, outputFileNames, doddSagProj, TwoDFt);
    DSET_delete(doddSagProj);

    // Make Fourier transforms of projections
    if (!(get2DFourierTransform(THD_3dim_dataset *din, COMPLEX ***TwoDFT)))
        Cleanup(inputFileName, outputFileNames, doddSagProj, TwoDFt);

    // Output Fourier spectra, of projections.
    // TODO: Add code

    // Make CSV file of radial phase shift linearity (RPSL) between pairs of FTs
    // TODO: Add code

    // TODO: Add code
    Cleanup(inputFileName, outputFileNames, doddSagProj, TwoDFt);
    return 1;
}

int get2DFourierTransform(THD_3dim_dataset *din, COMPLEX ***TwoDFT){
    int x, y, inc;
    float   *inputImage;

    // Get dimensions
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);

    // Allocate memory to complex Fourier plane
    if (!(*TwoDFT=(COMPLEX **)malloc(ny*sizeof(COMPLEX *)))) return 0;
    for (y=0; y<ny: ++y) if (!((*TwoDFT)[y]=(COMPLEX *)malloc(nx*sizeof(COMPLEX)))){
        for (--y; y>=0; --y) free((*TwoDFT)[y]);
        free((*TwoDFT));
        return 0;
    }

    // Fill real components with spatial image data
    inputImage = DSET_ARRAY(din, 0);
    for (y=inc=0; y<ny; ++y){
        for (x=0; x<nx; ++x)
            (*TwoDFT)[y][x] = inputImage[inc++];
    }

    // Fouier transform plane
    if (!FFT2D(COMPLEX **c,int nx,int ny,int dir)){
        for (y=0; y<ny: ++y) free((*TwoDFT)[y]);
        free(*TwoDFT);
        *TwoDFT = NULL;
        return 0;
    }

    return 1;
}

int zeroPadProjection(THD_3dim_dataset *din, THD_3dim_dataset **dout, int largestDimension, int factor){
    char *prefix=DSET_PREFIX(din);
    char *searchPath=DSET_DIRNAME(din);
    char *outputFileName;
    float   *outData;
    int  outputDimension=largestDimension*factor;
    int  y, inInc;
    char  appendage[8];
    THD_ivec3 iv_nxyz;

    // Set output name appendage
    sprintf(appendage, "pad%d", factor);

    // Raise output dimension to the next integral power of two (for Fourier transform)
    outputDimension = pow(2, ceil(log(outputDimension)/log(2)));

    // Make output array
    if (!(outData=(float*)calloc(outputDimension*outputDimension,sizeof(float)))) return 0;

    // Determine input dimensions
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);

    // Determine where input begins in output
    int xOffset=(int)((outputDimension-nx)/2);
    int yOffset=(int)((outputDimension-ny)/2);
    int offset=(outputDimension*yOffset)+xOffset;

    // Get input array
    float   *indata = DSET_ARRAY(din, 0);

    // Write input to output
    size_t  bytes2Copy = nx*sizeof(float);
    for (y=inInc=0; y<ny; ++y){
        memcpy((void *)&(outData[offset]), (void *)&(indata[inInc]), bytes2Copy);
        offset+=outputDimension;
        inInc+=nx;
    }

    // Allocate memory to output name buffer
    if (!(outputFileName=(char *)malloc(strlen(searchPath)+strlen(prefix)+64))){
       free(outData);
       return 0;
    }

    // Determine whether output file already exists
    int outputFileExists = doesFileExist(searchPath,prefix,appendage,outputFileName);

    // Make output dataset
    *dout = EDIT_empty_copy(din);
    LOAD_IVEC3( iv_nxyz , outputDimension , outputDimension , 1 ) ;
    sprintf(outputFileName,"%s%s%s",searchPath,prefix,appendage);
    EDIT_dset_items( *dout ,
                    ADN_prefix, outputFileName,
                    ADN_nxyz      , iv_nxyz ,
                    ADN_none ) ;
    EDIT_substitute_brick(*dout, 0, MRI_float, outData);

    // Debug: Get output dimensions
    ny = DSET_NY(*dout);
    nx = DSET_NX(*dout);

    // Output padded image to file
    if( !outputFileExists ) {  // If even file does not already exist
       DSET_write(*dout);
    }

    // Cleanup
    free(outputFileName);

    return 1;
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

int float2DImage(THD_3dim_dataset *dset){
    char *prefix=DSET_PREFIX(dset);
    char *searchPath=DSET_DIRNAME(dset);
    char *outputFileName;
    char  appendage[8];

    // Subtract mean of perimeter from entire image to avoid spurious edge effects
    //  in Fourier transform
    float   mean=0;
    int     count=0, x, y;

    // Get input pixel data
    float   *indata = DSET_ARRAY(dset, 0);

    // Determine input dimensions
    int ny = DSET_NY(dset);
    int nx = DSET_NX(dset);
    int lastY = ny-1;
    int lastX = nx-1;

    // Get mean value arount perimeter
    int offset=lastY*nx;
    for (x=0; x<nx; ++x){
        mean += indata[x]+indata[offset++];
        count+=2;
    }
    offset=nx;
    for (y=0; y<lastY; ++y){
        mean += indata[offset];
        offset += lastX;
        mean += indata[offset++];
        count+=2;
    }
    mean /= count;

    // Subtract perimeter mean from whole image
    int pixelCount=nx*ny;
    for (x=0; x<pixelCount; ++x) indata[x]-=mean;

    // Allocate memory to output name buffer
    if (!(outputFileName=(char *)malloc(strlen(searchPath)+strlen(prefix)+64))){
       return 0;
    }

    // Set output name appendage
    sprintf(appendage, "float");

    // See if image exists
    int outputFileExists = doesFileExist(searchPath,prefix,appendage,outputFileName);

    // Output floated image to file
    sprintf(outputFileName,"%s%s%s",searchPath,prefix,appendage);
    EDIT_dset_items( dset ,
                    ADN_prefix, outputFileName,
                    ADN_none ) ;
    if( !outputFileExists ) {  // If even file does not already exist
       DSET_write(dset);
    }

    // Cleanup
    free(outputFileName);

    return 1;
}

int getLargestDimension(THD_3dim_dataset *din){

    // Determine input dimensions
    int nz = DSET_NZ(din);
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);

    return (nx>ny)? ((nx>nz)? nx : nz) : ((ny>nz)? ny : nz);
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

    // Cleanup  (Don't free outData)
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

int Cleanup(char *inputFileName, char *outputFileNames[], THD_3dim_dataset *din, COMPLEX **TwoDFt){
    int i, ny;

    if (inputFileName) free(inputFileName);
    for (i=0; i<3; ++i) if (outputFileNames[i]) free(outputFileNames[i]);
    if (din) free(din

    if (TwoDFt){
        ny = DSET_NY(din);
        for (i=0; i<ny; ++i) free(TwoDFt[i]);
        free(TwoDFt);
    }

    return 0;
}


/************************************ 2D FFT by Paul Bourke, 1993 ***************************/



/*-------------------------------------------------------------------------
   Perform a 2D FFT inplace given a complex 2D array
   The direction dir, 1 for forward, -1 for reverse
   The size of the array (nx,ny)
   Return false if there are memory problems or
      the dimensions are not powers of 2
*/
int FFT2D(COMPLEX **c,int nx,int ny,int dir)
{
   int i,j;
   int m,twopm;
   double *real,*imag;

   /* Transform the rows */
   real = (double *)malloc(nx * sizeof(double));
   imag = (double *)malloc(nx * sizeof(double));
   if (real == NULL || imag == NULL)
      return(FALSE);
   if (!Powerof2(nx,&m,&twopm) || twopm != nx)
      return(FALSE);
   for (j=0;j<ny;j++) {
      for (i=0;i<nx;i++) {
         real[i] = c[i][j].real;
         imag[i] = c[i][j].imag;
      }
      FFT(dir,m,real,imag);
      for (i=0;i<nx;i++) {
         c[i][j].real = real[i];
         c[i][j].imag = imag[i];
      }
   }
   free(real);
   free(imag);

   /* Transform the columns */
   real = (double *)malloc(ny * sizeof(double));
   imag = (double *)malloc(ny * sizeof(double));
   if (real == NULL || imag == NULL)
      return(FALSE);
   if (!Powerof2(ny,&m,&twopm) || twopm != ny)
      return(FALSE);
   for (i=0;i<nx;i++) {
      for (j=0;j<ny;j++) {
         real[j] = c[i][j].real;
         imag[j] = c[i][j].imag;
      }
      FFT(dir,m,real,imag);
      for (j=0;j<ny;j++) {
         c[i][j].real = real[j];
         c[i][j].imag = imag[j];
      }
   }
   free(real);
   free(imag);

   return(TRUE);
}

/*-------------------------------------------------------------------------
   This computes an in-place complex-to-complex FFT
   x and y are the real and imaginary arrays of 2^m points.
   dir =  1 gives forward transform
   dir = -1 gives reverse transform

     Formula: forward
                  N-1
                  ---
              1   \          - j k 2 pi n / N
      X(n) = ---   >   x(k) e                    = forward transform
              N   /                                n=0..N-1
                  ---
                  k=0

      Formula: reverse
                  N-1
                  ---
                  \          j k 2 pi n / N
      X(n) =       >   x(k) e                    = forward transform
                  /                                n=0..N-1
                  ---
                  k=0
*/
int FFT(int dir,int m,double *x,double *y)
{
   long nn,i,i1,j,k,i2,l,l1,l2;
   double c1,c2,tx,ty,t1,t2,u1,u2,z;

   /* Calculate the number of points */
   nn = 1;
   for (i=0;i<m;i++)
      nn *= 2;

   /* Do the bit reversal */
   i2 = nn >> 1;
   j = 0;
   for (i=0;i<nn-1;i++) {
      if (i < j) {
         tx = x[i];
         ty = y[i];
         x[i] = x[j];
         y[i] = y[j];
         x[j] = tx;
         y[j] = ty;
      }
      k = i2;
      while (k <= j) {
         j -= k;
         k >>= 1;
      }
      j += k;
   }

   /* Compute the FFT */
   c1 = -1.0;
   c2 = 0.0;
   l2 = 1;
   for (l=0;l<m;l++) {
      l1 = l2;
      l2 <<= 1;
      u1 = 1.0;
      u2 = 0.0;
      for (j=0;j<l1;j++) {
         for (i=j;i<nn;i+=l2) {
            i1 = i + l1;
            t1 = u1 * x[i1] - u2 * y[i1];
            t2 = u1 * y[i1] + u2 * x[i1];
            x[i1] = x[i] - t1;
            y[i1] = y[i] - t2;
            x[i] += t1;
            y[i] += t2;
         }
         z =  u1 * c1 - u2 * c2;
         u2 = u1 * c2 + u2 * c1;
         u1 = z;
      }
      c2 = sqrt((1.0 - c1) / 2.0);
      if (dir == 1)
         c2 = -c2;
      c1 = sqrt((1.0 + c1) / 2.0);
   }

   /* Scaling for forward transform */
   if (dir == 1) {
      for (i=0;i<nn;i++) {
         x[i] /= (double)nn;
         y[i] /= (double)nn;
      }
   }

   return(TRUE);
}

/*-------------------------------------------------------------------------
   Calculate the closest but lower power of two of a number
   twopm = 2**m <= n
   Return TRUE if 2**m == n
*/
int Powerof2(int n,int *m,int *twopm)
{
   if (n <= 1) {
      *m = 0;
      *twopm = 1;
      return(FALSE);
   }

   *m = 1;
   *twopm = 2;
   do {
      (*m)++;
      (*twopm) *= 2;
   } while (2*(*twopm) <= n);

   if (*twopm != n)
      return(FALSE);
   else
      return(TRUE);
}


