/*
    commonLinesReg -- Peter Lauren, 2020-08-21

SYNOPSIS

    commonLinesReg -i <Input Filename> -p <ac|as|cs|acs> -o <orientation> [-f [-l <min. length>]]

DESCRIPTION

    Uses common lines algorithm to linearly align two volumes.  It does this by finding where orthogonal volumes intersect
    in Fourier space.  If they are rotationally aligned, they should intersect on the appropriate orthogonal cardinal axes.
    The slope of the phase shift, along the lines of intersection, reflect the translational misalignment.  The translations,
    and rotations, are each determined by solving a linear system.  These solutions are completely independent of each other.

    -o  Output of "3dinfo -orient mydset" in the linux command line.  This gives the a, y,z z-axes in terms of the axial, coronal
        and sagmital direction.

    -f  Examine effects of frequency range.  The search is over a range of spectral lengths and min. frequencies.  "min. length"
        is the starting length.

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
#include <sys/resource.h>
#include "mrilib.h"
#include "Generic.h"
// #include "VolumeRegistration.h"

#define UNIX 1

int Cleanup(char *inputFileName, COMPLEX ***TwoDFts, THD_3dim_dataset *din);
int open_input_dset(THD_3dim_dataset ** din, char * fname);
int unweave_sections(THD_3dim_dataset *din, THD_3dim_dataset **dodd, THD_3dim_dataset **deven);
int makeProjection(THD_3dim_dataset *din, THD_3dim_dataset **dout, char projCode, char *orientation);
int float2DImage(THD_3dim_dataset *dset);
int getLargestDimension(THD_3dim_dataset *din);
int zeroPadProjection(THD_3dim_dataset *din, THD_3dim_dataset **dout, int largestDimension, int factor);
int doesFileExist(char * searchPath, char * prefix,char *appendage , char * outputFileName);
int FFT2D(COMPLEX **c,int nx,int ny,int dir);
int FFT(int dir,int m,double *x,double *y);
int Powerof2(int n,int *m,int *twopm);
int get2DFourierTransform(THD_3dim_dataset *din, COMPLEX ***TwoDFts);
int outputFourierSpectrum(COMPLEX **TwoDFts, THD_3dim_dataset *din);
int outputRealAndImaginaryPlanes(COMPLEX **TwoDFts, THD_3dim_dataset *din);
COMPLEX **CheckeredInversion(COMPLEX **input, unsigned int dimension);
ERROR_NUMBER OutputFourierIntersectionMap(COMPLEX ***TwoDFts, int lNumberOfImages, int dimension,
		IntRange irFrequencyRange, char *csSearchPath, char *csRootName, float *fpCoplanarAngularShift);
ERROR_NUMBER MakeRadialPhaseSampleArray(ComplexPlane cpFourierTransform, float fIncrementInDegrees,
			IntRange lrFrequencyRange, float ***fpppRadialSamples, long *lpNumRadialSamples);
ERROR_NUMBER RotScaleComplexPlaneM(ComplexPlane *cppComplexPlane, Matrix mTransMatrix, BOOL bClip, BYTE bInterpOrder);
ERROR_NUMBER RotScaleComplexPlane(ComplexPlane *cppComplexPlane, FloatVector fvTranslation,
		float fClockwiseRotationInDegrees, float fScaleX, float fScaleY, BOOL bClip, BYTE bInterpOrder);
ERROR_NUMBER MakeCardinalRadialPhaseSamples(FloatPlane fpReal, FloatPlane fpImaginary,
			short *spCardinalIndices, IntRange lrFrequencyRange, float ***fpppRadialSamples);
ERROR_NUMBER ComparePhaseSamples(float *fpTargetSample, float *fpRefSample,
										IntRange lrFrequencyRange, float *fpComparisonMetric);
float ExpectedAngularShift(int iCurrentAxis);
float GetAngularShift(FloatVector fvCoords);
void DetermineUnclippedOutputDimensions(long lInputColumns, long lInputRows,
	 float fClockwiseRotationInDegrees, FloatVector fvTranslation, float fScaleX, float fScaleY);
void MatVectMult(Matrix mMatrix, float *fpInVector, float *fpOutVector);
ERROR_NUMBER MakeRadialPhaseVectorSample(COMPLEX *cpComplexRadialSample, int iLength, float **fppRadialSample);
ERROR_NUMBER MakePhaseShiftVector(float *fpTargetSample, float *fpRefSample, IntRange lrFrequencyRange,
								  long *lpVectorLength, float **fppPhaseShiftVector);
void GetSlope(float *fVector, IntRange lrFrequencyRange, float *fpSlope, float *fpIntercept);
float GetMSE(float *fVector, float fSlope, float fPhaseShiftIntercept, IntRange lrFrequencyRange);
ERROR_NUMBER MedianFilterFloatVector(float *fpVector, long lLength);
float Float3x1Median(float *fpInput);
void FreeFloatPlane(FloatPlane Float_Plane);
int Round(float arg);
ERROR_NUMBER ErrorOpeningFile(char *csFileName);
float FloatPythag(float fArg1, float fArg2);
double Pythag(double dArg1, double dArg2);
double Interp_BL(float **image, float x_coord, float y_coord);
double Interp_BQ(float **image, float x_coord, float y_coord);
double	Interp_BC(float **image, float x_coord, float y_coord);
long DRound(double arg);
ERROR_NUMBER Complex2Float(ComplexPlane cpInputPlane, FloatPlane *fppRealOut, FloatPlane *fppImagOut);
FloatPlane MakeFloatPlane(long iRows, long iColumns, float **Data);
char * RootName(char * csFullPathName);
ERROR_NUMBER GetDirectory(char *csInputFileName, char *csDirectory);
ERROR_NUMBER shortToFloat(THD_3dim_dataset **din);
ERROR_NUMBER analyzeFrequencyRange(COMPLEX ***TwoDFts, int numberOfImages,
    int paddedDimension, char *searchPath, char *prefix, int minLength, int startingTargetIndex);
ERROR_NUMBER getImsePeakAndMean(COMPLEX ***TwoDFts, int dimension, int refIndex, int targetIndex,
            IntRange irFrequencyRange, float *maxIMSE, float *meanIMSE);


double  dCommonLinesAngleErr;
float   fCommonLinesPeak, fCommonLinesPeakDiff;
LongVector  lvOutputDimensions;


int main( int argc, char *argv[] )  {
	ERROR_NUMBER	enErrorNumber;
	IntRange irFrequencyRange;
	float fCoplanarAngularShift;
    THD_3dim_dataset * din = NULL, *dodd, *deven;
    THD_3dim_dataset* projections[6], *paddedProjections[6];
    int     i, j, largestDimension, paddingFactor=4, minLength=8;
    char    *inputFileName=NULL, projectionString[3]={'a','s','\0'};
    char    orientation[8];
    COMPLEX** TwoDFts[6]={NULL, NULL, NULL};
    Boolean frequencyRangeEffects = false;
    int startingTargetIndex=4;

    sprintf(orientation, "ASL");

    paddingFactor=2;    // DEBUG

    for (i=0; i<argc; ++i) if (argv[i][0]=='-'){
        switch(argv[i][1]){
        case 'f':
            frequencyRangeEffects = true;
            break;

        case 'i':
            if (!(inputFileName=(char*)malloc(strlen(argv[++i])+8)))
                return Cleanup(inputFileName, TwoDFts, din);
            sprintf(inputFileName,"%s",argv[i]);
            break;

        case 'l':
            minLength=atoi(argv[++i]);
            break;

        case 'o':
            sprintf(orientation, "%s", argv[++i]);
            break;

        case 'p':
            sprintf(projectionString,"%s",argv[++i]);
            break;

        case 's':
            startingTargetIndex=atoi(argv[++i]);
            break;
        }
    }

    if( open_input_dset(&din, inputFileName) )
    return Cleanup(inputFileName, TwoDFts, din);

    // Split volume into odd and even slices
    if (!unweave_sections(din, &dodd, &deven))
        return Cleanup(inputFileName, TwoDFts, din);

    // Make odd projections
    makeProjection(dodd, &(projections[0]), 's', orientation);
    makeProjection(dodd, &(projections[1]), 'c', orientation);
    makeProjection(dodd, &(projections[2]), 'a', orientation);

    // Make even projections
    makeProjection(deven, &(projections[3]), 's', orientation);
    makeProjection(deven, &(projections[4]), 'c', orientation);
    makeProjection(deven, &(projections[5]), 'a', orientation);

    // Get largest dimension from data set
    largestDimension=getLargestDimension(dodd);

    // Free up intput dataset
    DSET_delete(din);

    // Float projections
    // Currently skipped because of head voxels on the edge of the projection image.  Apart from that,
    //  the edge defaults to zero.  Floating creates edge effects rather than suppressing them.
    // float2DImage(doddSagProj);

    // Zero pad projections
    for (i=0; i<6; ++i){
        if (!(zeroPadProjection(projections[i], &(paddedProjections[i]), largestDimension, paddingFactor))){
            Cleanup(inputFileName, TwoDFts, projections[2]);
            for (j=0; j<i; ++j) DSET_delete(paddedProjections[j]);
            for (; j<6; ++j) DSET_delete(projections[j]);
        }
        DSET_delete(projections[i]);
    }

    // Make Fourier transforms of projections
    for (i=0; i<6; ++i){
        if (!(get2DFourierTransform(paddedProjections[i], &(TwoDFts[i])))){
            Cleanup(inputFileName, TwoDFts, paddedProjections[0]);
            for (i=0; i<6; ++i) DSET_delete(paddedProjections[i]);
        }
    }

    // Output Fourier spectra, of projections.
#if 1
    for (i=0; i<6; ++i){
        if (!(outputFourierSpectrum(TwoDFts[i], paddedProjections[i]))){
            Cleanup(inputFileName, TwoDFts, paddedProjections[0]);
            for (i=0; i<6; ++i) DSET_delete(paddedProjections[i]);
        }
    }
#endif

    // DEBUG: Output real and imaginary plane of first FT.
    // outputRealAndImaginaryPlanes(TwoDFts[0], paddedProjections[0]);

    // Make CSV file of radial phase shift linearity (RPSL) between pairs of FTs
    int paddedDimension = DSET_NY(paddedProjections[0]);
    int lNumberOfImages = 6;
    char *prefix=DSET_PREFIX(din);
    char *searchPath=(char *)malloc(strlen(inputFileName)*sizeof(char));
    if (enErrorNumber=GetDirectory(inputFileName, searchPath)){
        Cleanup(inputFileName, TwoDFts, paddedProjections[0]);
        for (i=0; i<6; ++i) DSET_delete(paddedProjections[i]);
        free(searchPath);
        return enErrorNumber;
    }
    if (frequencyRangeEffects){
        if ((enErrorNumber=analyzeFrequencyRange(TwoDFts, lNumberOfImages, paddedDimension,
            searchPath, prefix, minLength, startingTargetIndex))!=ERROR_NONE){
            Cleanup(inputFileName, TwoDFts, paddedProjections[0]);
            for (i=0; i<6; ++i) DSET_delete(paddedProjections[i]);
            free(searchPath);
            return enErrorNumber;
        }
    } else {
        irFrequencyRange.iMin = 15;
        irFrequencyRange.iMax = paddedDimension/2-32;
        if ((enErrorNumber=OutputFourierIntersectionMap(TwoDFts, lNumberOfImages, paddedDimension,
            irFrequencyRange, searchPath, prefix, &fCoplanarAngularShift))!=ERROR_NONE){
                Cleanup(inputFileName, TwoDFts, paddedProjections[0]);
                for (i=0; i<6; ++i) DSET_delete(paddedProjections[i]);
                free(searchPath);
                return enErrorNumber;
            }
    }
    free(searchPath);

    Cleanup(inputFileName, TwoDFts, paddedProjections[0]);
    for (i=0; i<6; ++i) DSET_delete(paddedProjections[i]);

    return 1;
}

ERROR_NUMBER MakeRadialPhaseSampleArray(ComplexPlane cpFourierTransform, float fIncrementInDegrees,
			IntRange lrFrequencyRange, float ***fpppRadialSamples, long *lpNumRadialSamples)
{
	ERROR_NUMBER	enErrorNumber;
	long	iSampleIndex, iQuadIndex;
	float	fAngleInDegrees=0;
	ComplexPlane	cpRotatedFT;
	FloatPlane  fpReal, fpImag;
	FloatVector	fvTranslationVector={0,0};
	short	saCardinalIndices[4];

/* TODO: Add later
	if (bSincFunctionInterp) return MakeRadialPhaseSampleArrayWithSincInterp(fpRealInput, fIncrementInDegrees,
			lrFrequencyRange, fpppRadialSamples, lpNumRadialSamples, sFFTPaddingFactor);
			*/

	// Make rotation buffer
	cpRotatedFT=MakeComplexPlane(cpFourierTransform.iRows, cpFourierTransform.iColumns, NULL);
	if (!(cpRotatedFT.Data)) return ERROR_MEMORY_ALLOCATION;

	// Allocate pointer to array
	if (!(*fpppRadialSamples=(float **)malloc(4*((*lpNumRadialSamples)+4)*sizeof(float *))))
	{
		FreeComplexPlane(cpRotatedFT);
		return ERROR_MEMORY_ALLOCATION;
	}

	// Initlialize cardinal indices
	saCardinalIndices[0]=0;
	saCardinalIndices[1]=(short)(*lpNumRadialSamples);
	saCardinalIndices[2]=(short)((*lpNumRadialSamples)*2);
	saCardinalIndices[3]=saCardinalIndices[1]+saCardinalIndices[2];

	// Make samples
	for (iSampleIndex=0; iSampleIndex<(*lpNumRadialSamples); ++iSampleIndex)
	{
		// Copy input image into rotation buffer
		CopyComplexPlane(cpFourierTransform, cpRotatedFT);

		// Apply rotation to spatial plane image
		if (fAngleInDegrees!=0 && (enErrorNumber=RotScaleComplexPlane(&cpRotatedFT,
			fvTranslationVector, -fAngleInDegrees, 1.0f, 1.0f, TRUE, BIQUADRATIC_INTERPOLATION))!=ERROR_NONE)
		{
			FreeComplexPlane(cpRotatedFT);
			for (--iSampleIndex; iSampleIndex>=0; --iSampleIndex) free((*fpppRadialSamples)[iSampleIndex]);
			free(*fpppRadialSamples);
			(*fpppRadialSamples)=NULL;
			return enErrorNumber;
		}

		// Make real and imaginary planes from Fourier transform
		if ((enErrorNumber=Complex2Float(cpRotatedFT, &fpReal, &fpImag))!=ERROR_NONE)
		{
			FreeComplexPlane(cpRotatedFT);
			for (--iSampleIndex; iSampleIndex>=0; --iSampleIndex) free((*fpppRadialSamples)[iSampleIndex]);
			free(*fpppRadialSamples);
			(*fpppRadialSamples)=NULL;
			return enErrorNumber;
		}

		// Make radial samples from cardinal angles
		if ((enErrorNumber=MakeCardinalRadialPhaseSamples(fpReal, fpImag, &(saCardinalIndices[0]),
			lrFrequencyRange, fpppRadialSamples))!=ERROR_NONE)
		{
			FreeComplexPlane(cpRotatedFT);
			FreeFloatPlane(fpReal);
			FreeFloatPlane(fpImag);
			for (--iSampleIndex; iSampleIndex>=0; --iSampleIndex) free((*fpppRadialSamples)[iSampleIndex]);
			free ((*fpppRadialSamples));
			(*fpppRadialSamples)=NULL;
			return enErrorNumber;
		}


		// Increment angle and cardinal indices
		fAngleInDegrees+=fIncrementInDegrees;
		for (iQuadIndex=0; iQuadIndex<4; ++iQuadIndex) ++saCardinalIndices[iQuadIndex];

		// Cleanup
		FreeFloatPlane(fpReal);
		FreeFloatPlane(fpImag);
	}

	// Cleanup
	FreeComplexPlane(cpRotatedFT);

	return ERROR_NONE;
}

ERROR_NUMBER analyzeFrequencyRange(COMPLEX ***TwoDFts, int numberOfImages,
    int paddedDimension, char *searchPath, char *prefix, int minLength, int startingTargetIndex){

    ERROR_NUMBER    enErrorNumber;
	char    csAnalysisFileName[512];
	IntRange irFrequencyRange;
	FILE    *outputFile;
	int     maxMin=paddedDimension/4, maxMax=paddedDimension/2-1;
	int     minRangeLength=minLength;
	// int     minRangeLength=250; // DEBUG
	float   maxIMSE, meanIMSE;  // Max and mean inverse mean squared error
	struct rusage r_usage;  // Track memory usage

    for (int lPlaneIndex=startingTargetIndex; lPlaneIndex<numberOfImages; ++lPlaneIndex){    // Avoid coplanar and self reference
        fprintf(stdout, "Processing image %d\n", lPlaneIndex);

        // Open output file
        sprintf(csAnalysisFileName,"%s/%s_LOI_freqRange%d.csv", searchPath, prefix, lPlaneIndex);
        if (!(outputFile=fopen(csAnalysisFileName, "w")))
            return ErrorOpeningFile(csAnalysisFileName);

        // Make header
        fprintf(outputFile, "Range Length\tFMin\n");
        for (int i=0; i<=maxMin; ++i) fprintf(outputFile, "\t%d", i);
        fprintf(outputFile, "\n");

        // Process for each frequency range length
        for (int rangeLength=minRangeLength; rangeLength<maxMax; ++rangeLength){
            getrusage(RUSAGE_SELF,&r_usage);
            fprintf(stderr, "Processing length %d of %d. Memory usage: %ld kilobytes\n",
                rangeLength, maxMax, r_usage.ru_maxrss);
            fprintf(outputFile, "%d", rangeLength);

            for (int fMin=0; fMin<=maxMin; ++fMin){
                fprintf(stderr, "fMin=%d of %d\r", fMin, maxMin);
                if (fMin+rangeLength>maxMax) fprintf(outputFile, "\t0");
                else {
                    irFrequencyRange.iMin = fMin;
                    irFrequencyRange.iMax = fMin+rangeLength;

                    if ((enErrorNumber=getImsePeakAndMean(TwoDFts, paddedDimension, 0, lPlaneIndex,
                        irFrequencyRange, &maxIMSE, &meanIMSE))!=ERROR_NONE){
                        fclose(outputFile);
                        return enErrorNumber;
                    }

                    fprintf(outputFile, "\t%f", maxIMSE/meanIMSE);
                }
            }
            fprintf(outputFile, "\n");
            fprintf(stderr, "\n");
        }

        fclose(outputFile);
    }

	return ERROR_NONE;
}

ERROR_NUMBER getImsePeakAndMean(COMPLEX ***TwoDFts, int dimension, int refIndex, int targetIndex, IntRange irFrequencyRange,
                                float *maxIMSE, float *meanIMSE){

	ERROR_NUMBER	enErrorNumber;
	ComplexPlane    cpComplexPlane;
	float			fIncrementInDegrees=2.0f*(float)(asin(0.5/irFrequencyRange.iMax)/DEGREES2RADIANS);
	float			**fppRefSamples, **fpTargetSamples, fComparisonMetric;
	long			lPlaneIndex, lNumTargetSamples, lNumRefSamples, lTargetIndex, lRefIndex;

	// Make reference samples
	lNumRefSamples=Round(180.0f/fIncrementInDegrees)-1;
	cpComplexPlane.iColumns=cpComplexPlane.iRows=dimension;
	cpComplexPlane.Data=TwoDFts[refIndex];
	if ((enErrorNumber=MakeRadialPhaseSampleArray(cpComplexPlane, fIncrementInDegrees,
		irFrequencyRange, &fppRefSamples, &lNumRefSamples))!=ERROR_NONE){
		return ERROR_MEMORY_ALLOCATION;
    }

    // Make target array
    lNumTargetSamples=Round(360.0f/fIncrementInDegrees);
    cpComplexPlane.iColumns=cpComplexPlane.iRows=dimension;
    cpComplexPlane.Data=TwoDFts[targetIndex];
    if ((enErrorNumber=MakeRadialPhaseSampleArray(cpComplexPlane, fIncrementInDegrees,
        irFrequencyRange, &fpTargetSamples, &lNumTargetSamples))!=ERROR_NONE)
    {
        for (lRefIndex=0; lRefIndex<lNumRefSamples; ++lRefIndex)
            free(fppRefSamples[lRefIndex]);
        free(fppRefSamples);
        return enErrorNumber;
    }

    // Process samples from reference and target plane
    *maxIMSE = *meanIMSE = 0;
    for (lTargetIndex=0; lTargetIndex<=lNumTargetSamples; ++lTargetIndex)
    {
        // Compare target samples against each reference sample using multiplicative inverse of
        //	mean square distance (MSD)
        for (lRefIndex=0; lRefIndex<lNumRefSamples; ++lRefIndex)
        {
            // Get comparison metric between the two samples
            if ((enErrorNumber=ComparePhaseSamples(fpTargetSamples[lTargetIndex], fppRefSamples[lRefIndex],
                irFrequencyRange, &fComparisonMetric))!=ERROR_NONE)
            {
                for (lRefIndex=0; lRefIndex<lNumRefSamples; ++lRefIndex) free(fppRefSamples[lRefIndex]);
                free(fppRefSamples);
                for (lTargetIndex=0; lTargetIndex<lNumRefSamples; ++lTargetIndex) free(fpTargetSamples[lTargetIndex]);
                free(fpTargetSamples);
                return enErrorNumber;
            }

            // Update mean
            *meanIMSE += fComparisonMetric;

            // Update max
            if (fComparisonMetric>*maxIMSE) *maxIMSE = fComparisonMetric;
        }
    }

    *meanIMSE /= lNumTargetSamples*lNumRefSamples;

    // Cleanup
    int numberToFree = lNumRefSamples*4;
    for (lRefIndex=0; lRefIndex<numberToFree; ++lRefIndex) free(fppRefSamples[lRefIndex]);
    free(fppRefSamples);
    numberToFree = lNumTargetSamples*4;
    for (lTargetIndex=0; lTargetIndex<numberToFree; ++lTargetIndex) free(fpTargetSamples[lTargetIndex]);
    free(fpTargetSamples);

	return ERROR_NONE;
}

ERROR_NUMBER OutputFourierIntersectionMap(COMPLEX ***TwoDFts, int lNumberOfImages, int dimension,
		IntRange irFrequencyRange, char *csSearchPath, char *csRootName, float *fpCoplanarAngularShift)
{
	ERROR_NUMBER	enErrorNumber;
	ComplexPlane    cpComplexPlane;
	char            csAnalysisFileName[512];

	// Increment to avoid false maxima
	float			fIncrementInDegrees=2.0f*(float)(asin(0.5/irFrequencyRange.iMax)/DEGREES2RADIANS);

	float			**fppRefSamples, **fpTargetSamples, fComparisonMetric, faPeakValues[10];
	long			lPlaneIndex, lNumTargetSamples, lNumRefSamples, lTargetIndex, lRefIndex;
	short			sPeakIndex, i, j;
	FloatVector		fvaPeakCoords[10];
	FILE			*fpAnalysisFile, *fpGraphFile;
	BOOL			bAnalysisFile=TRUE;
	char            *graphFileName;
	// , *directory, *rootName;

	// Try reducing increment to improve accuracy
	fIncrementInDegrees /= 2;

	// Allocate memory to graph file name
	if (!(graphFileName=(char *)malloc(strlen(csAnalysisFileName)+32)))
            return ERROR_MEMORY_ALLOCATION;

	// Make reference samples
	lNumRefSamples=Round(180.0f/fIncrementInDegrees)-1;
	cpComplexPlane.iColumns=cpComplexPlane.iRows=dimension;
	cpComplexPlane.Data=TwoDFts[0];
	if ((enErrorNumber=MakeRadialPhaseSampleArray(cpComplexPlane, fIncrementInDegrees,
		irFrequencyRange, &fppRefSamples, &lNumRefSamples))!=ERROR_NONE){
		free(graphFileName);
		return ERROR_MEMORY_ALLOCATION;
    }

    /*
    // Get directory name
    if ((enErrorNumber=GetDirectory(csAnalysisFileName, directory))!=ERROR_NONE){
        free(graphFileName);
        free(directory);
        free(rootName);
        return enErrorNumber;
    }
    */

	// Process target planes
	for (lPlaneIndex=1; lPlaneIndex<lNumberOfImages; ++lPlaneIndex)
        if (lPlaneIndex!=3) // Avoid coplanar
	{
        // Open graph file
        sprintf(graphFileName, "%s/%s_LOI_graph%ld.csv", csSearchPath, csRootName, lPlaneIndex);
        if (!(fpGraphFile=fopen(graphFileName, "w"))){
            free(graphFileName);
            return ErrorOpeningFile(graphFileName);
        }

		// Make target array
		lNumTargetSamples=Round(360.0f/fIncrementInDegrees);
        cpComplexPlane.iColumns=cpComplexPlane.iRows=dimension;
        cpComplexPlane.Data=TwoDFts[lPlaneIndex];
		if ((enErrorNumber=MakeRadialPhaseSampleArray(cpComplexPlane, fIncrementInDegrees,
			irFrequencyRange, &fpTargetSamples, &lNumTargetSamples))!=ERROR_NONE)
		{
			for (lRefIndex=0; lRefIndex<lNumRefSamples; ++lRefIndex)
				free(fppRefSamples[lRefIndex]);
            fclose(fpGraphFile);
			free(fppRefSamples);
			free(graphFileName);
			return enErrorNumber;
		}

		// Write graphic file header
		fprintf(fpGraphFile, "Ref.\\Target");
		for (lRefIndex=0; lRefIndex<=lNumRefSamples; ++lRefIndex)
            fprintf(fpGraphFile, ",%3.2f", fIncrementInDegrees*lRefIndex);
        fprintf(fpGraphFile, "\n");

        // Initialize peak values (Maybe should go in plane loop)
        for (i=0; i<10; ++i) faPeakValues[i]=0.0f;

		// Process samples from target plane
		for (lTargetIndex=0; lTargetIndex<=lNumTargetSamples; ++lTargetIndex)
		{
            // Write graph file entry
            fprintf(fpGraphFile, "%3.2f", fIncrementInDegrees*lTargetIndex);

			// Compare target samples against each reference sample using multiplicative inverse of
			//	mean square distance (MSD)
			for (lRefIndex=0; lRefIndex<lNumRefSamples; ++lRefIndex)
			{
				// Get comparison metric between the two samples
				if ((enErrorNumber=ComparePhaseSamples(fpTargetSamples[lTargetIndex], fppRefSamples[lRefIndex],
					irFrequencyRange, &fComparisonMetric))!=ERROR_NONE)
				{
					for (lRefIndex=0; lRefIndex<lNumRefSamples; ++lRefIndex) free(fppRefSamples[lRefIndex]);
					free(fppRefSamples);
					for (lTargetIndex=0; lTargetIndex<lNumRefSamples; ++lTargetIndex) free(fpTargetSamples[lTargetIndex]);
                    fclose(fpGraphFile);
					free(fpTargetSamples);
					free(graphFileName);
					return enErrorNumber;
				}

				// Add to top 10 peaks if appropriate
				sPeakIndex=10;
				for (i=9; i>=0; --i)
				{
					if (fComparisonMetric<=faPeakValues[i]) break;
					sPeakIndex=i;
				}
				if (sPeakIndex<10)
				{
					for (j=9; j>sPeakIndex; --j)
					{
						faPeakValues[j]=faPeakValues[j-1];
						fvaPeakCoords[j].x=fvaPeakCoords[j-1].x;
						fvaPeakCoords[j].y=fvaPeakCoords[j-1].y;
					}
					faPeakValues[sPeakIndex]=fComparisonMetric;
					fvaPeakCoords[sPeakIndex].x=fIncrementInDegrees*lRefIndex;
					fvaPeakCoords[sPeakIndex].y=fIncrementInDegrees*lTargetIndex;
				}

                // Write graph file entry
                fprintf(fpGraphFile, ",%3.2f", fComparisonMetric);
			}

            // New line in graph file
            fprintf(fpGraphFile, "\n");
		}

		// Close graph file
        fclose(fpGraphFile);

		// Free target samples
		for (lTargetIndex=0; lTargetIndex<lNumRefSamples; ++lTargetIndex) free(fpTargetSamples[lTargetIndex]);
		free(fpTargetSamples);

        // Open analysis file
        sprintf(csAnalysisFileName,"%s/%s_LOI_summ%ld.csv", csSearchPath, csRootName, lPlaneIndex);
        if (bAnalysisFile && !(fpAnalysisFile=fopen(csAnalysisFileName, "w")))
            return ErrorOpeningFile(csAnalysisFileName);

        // Output analysis to analysis file
        if (bAnalysisFile)
        {
            fprintf(fpAnalysisFile, "Rank\t1/MSE\tRef. Coord.\tTarget Coord.\n");
            for (i=0, j=1; i<10; ++i)
            {
                fprintf(fpAnalysisFile, "%d\t%3.2f\t%3.2f\t%3.2f\n", j++,
                    faPeakValues[i], fvaPeakCoords[i].x, fvaPeakCoords[i].y);
            }
        }
        *fpCoplanarAngularShift=fvaPeakCoords[0].y-fvaPeakCoords[0].x;
        if (*fpCoplanarAngularShift>180) *fpCoplanarAngularShift-=360;
        else if (*fpCoplanarAngularShift<-180) *fpCoplanarAngularShift+=360;

        // Close analysis file
        if (bAnalysisFile) fclose(fpAnalysisFile);
	}

	// Cleanup
	for (lRefIndex=0; lRefIndex<lNumRefSamples; ++lRefIndex)
		free(fppRefSamples[lRefIndex]);
	free(fppRefSamples);
	free(graphFileName);

/* TEMPORARAILY COMMENTED OUT 2020-09-28
	// Error analysis
	dCommonLinesAngleErr=fabs((double)(*fpCoplanarAngularShift-ExpectedAngularShift(Z_AXIS)));
	while (dCommonLinesAngleErr>180) dCommonLinesAngleErr=360-dCommonLinesAngleErr;
	while (dCommonLinesAngleErr<-180) dCommonLinesAngleErr=360+dCommonLinesAngleErr;
	*/

	fCommonLinesPeak=faPeakValues[0];
	for (i=0; i<10; ++i)
	{
		if (fabs((double)(*fpCoplanarAngularShift-GetAngularShift(fvaPeakCoords[i])))>2.0f)
		{
			fCommonLinesPeakDiff=faPeakValues[0]-faPeakValues[i];
			break;
		}
	}

	return ERROR_NONE;
}

/*********************************************************************/
 COMPLEX **CheckeredInversion(COMPLEX **input, unsigned int dimension)
/*********************************************************************/
{
unsigned int         i, j;

for (i=0; i<dimension; i+=2)
    {
    for (j=1; j<dimension; j+=2){
        (*(input+i)+j)->imag = 0 - (*(input+i)+j)->imag;
        (*(input+i)+j)->real = 0 - (*(input+i)+j)->real;
        }
    }

for (i=1; i<dimension; i+=2)
    {
    for (j=0; j<dimension; j+=2){
        (*(input+i)+j)->imag = 0 - (*(input+i)+j)->imag;
        (*(input+i)+j)->real = 0 - (*(input+i)+j)->real;
        }
    }

return(input);
}

int outputRealAndImaginaryPlanes(COMPLEX **TwoDFts, THD_3dim_dataset *din){
    int  x, y, inc;
    char *prefix=DSET_PREFIX(din);
    char *searchPath=DSET_DIRNAME(din);
    char *outputFileName;
    float   *outData;
    char  appendageR[] = "Real", appendageI[] = "Imaginary";
    COMPLEX *row;

    // Determine input dimensions
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);

    // Make output array
    if (!(outData=(float*)malloc(nx*ny*sizeof(float)))) return 0;

    // Allocate memory to output name buffer
    if (!(outputFileName=(char *)malloc(strlen(searchPath)+strlen(prefix)+64))){
       free(outData);
       return 0;
    }

    // Fill output data with real data
    for (y=inc=0; y<ny; ++y){
        row = TwoDFts[y];
        for (x=0; x<nx; ++x)
            outData[inc++]=row[x].real;
    }

    // Determine whether real file already exists
    int outputFileExists = doesFileExist(searchPath,prefix,appendageR,outputFileName);

    // Output Fourier spectrum image (if it does not already exist)
    if (!outputFileExists){
        THD_3dim_dataset *dout = EDIT_empty_copy(din);
        sprintf(outputFileName,"%s%s%s",searchPath,prefix,appendageR);
        EDIT_dset_items( dout ,
                        ADN_prefix, outputFileName,
                        ADN_none ) ;
        EDIT_substitute_brick(dout, 0, MRI_float, outData);
        DSET_write(dout);
    }

    // Fill output data with real data
    for (y=inc=0; y<ny; ++y){
        row = TwoDFts[y];
        for (x=0; x<nx; ++x)
            outData[inc++]=row[x].imag;
    }

    // Determine whether real file already exists
    outputFileExists = doesFileExist(searchPath,prefix,appendageI,outputFileName);

    // Output Fourier spectrum image (if it does not already exist)
    if (!outputFileExists){
        THD_3dim_dataset *dout = EDIT_empty_copy(din);
        sprintf(outputFileName,"%s%s%s",searchPath,prefix,appendageI);
        EDIT_dset_items( dout ,
                        ADN_prefix, outputFileName,
                        ADN_none ) ;
        EDIT_substitute_brick(dout, 0, MRI_float, outData);
        DSET_write(dout);
    }

    // Cleanup
    free(outputFileName);
    free(outData);

    return 1;

}

int outputFourierSpectrum(COMPLEX **TwoDFts, THD_3dim_dataset *din){
    int  x, y, inc;
    char *prefix=DSET_PREFIX(din);
    char *searchPath=DSET_DIRNAME(din);
    char *outputFileName;
    float   *outData;
    char  appendage[] = "FTSpect";
    COMPLEX *row;

    // Determine input dimensions
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);

    // Make output array
    if (!(outData=(float*)malloc(nx*ny*sizeof(float)))) return 0;

    // Fill output data with Fourier spectral data
    for (y=inc=0; y<ny; ++y){
        row = TwoDFts[y];
        for (x=0; x<nx; ++x)
            outData[inc++]=hypot(row[x].real,row[x].imag);
    }

    // Allocate memory to output name buffer
    if (!(outputFileName=(char *)malloc(strlen(searchPath)+strlen(prefix)+64))){
       free(outData);
       return 0;
    }

    // Determine whether output file already exists
    int outputFileExists = doesFileExist(searchPath,prefix,appendage,outputFileName);

    // Output Fourier spectrum image (if it does not already exist)
    if (!outputFileExists){
        THD_3dim_dataset *dout = EDIT_empty_copy(din);
        sprintf(outputFileName,"%s%s%s",searchPath,prefix,appendage);
        EDIT_dset_items( dout ,
                        ADN_prefix, outputFileName,
                        ADN_none ) ;
        EDIT_substitute_brick(dout, 0, MRI_float, outData);
        DSET_write(dout);
    }

    // Cleanup
    free(outputFileName);
    free(outData);

    return 1;
}

int get2DFourierTransform(THD_3dim_dataset *din, COMPLEX ***TwoDFts){
    int x, y, X, Y, inc;
    float   *inputImage;
    COMPLEX *row, *newRow, **centeredFT;

    // Get dimensions
    int ny = DSET_NY(din);
    int nx = DSET_NX(din);

    // Allocate memory to complex Fourier plane
    if (!(*TwoDFts=(COMPLEX **)malloc(ny*sizeof(COMPLEX *)))) return 0;
    for (y=0; y<ny; ++y) if (!((*TwoDFts)[y]=(COMPLEX *)calloc(nx,sizeof(COMPLEX)))){
        for (--y; y>=0; --y) free((*TwoDFts)[y]);
        free((*TwoDFts));
        return 0;
    }

    // Fill real components with spatial image data
    inputImage = DSET_ARRAY(din, 0);
    for (y=inc=0; y<ny; ++y){
        row = (*TwoDFts)[y];
        for (x=0; x<nx; ++x)
            row[x].real = inputImage[inc++];
    }

    // Fouier transform plane
    if (!FFT2D(*TwoDFts, nx, ny, 1)){
        for (y=0; y<ny; ++y) free((*TwoDFts)[y]);
        free(*TwoDFts);
        *TwoDFts = NULL;
        return 0;
    }

    // Allocate memory to centering buffer
    if (!(centeredFT=(COMPLEX **)malloc(ny*sizeof(COMPLEX *)))) {
        for (y=0; y<ny; ++y) free((*TwoDFts)[y]);
        free(*TwoDFts);
        *TwoDFts = NULL;
        return 0;
    }
    for (y=0; y<ny; ++y) if (!(centeredFT[y]=(COMPLEX *)calloc(nx,sizeof(COMPLEX)))){
        for (--y; y>=0; --y) free(centeredFT[y]);
        free(centeredFT);
        for (y=0; y<ny; ++y) free((*TwoDFts)[y]);
        free(*TwoDFts);
        *TwoDFts = NULL;
        return 0;
    }

    // Center Fourier transform
    for (y=0, Y=ny/2; Y<ny; ++y, ++Y){
        // New UL and UR quadrant
        newRow=centeredFT[Y];
        row = (*TwoDFts)[y];
        for (x=0, X=nx/2; X<nx; ++x, ++X){
            newRow[x]=row[X];   // UL quadrant
            newRow[X]=row[x];   // UR quadrant
        }

        // New LL and LR quadrant
        newRow=centeredFT[y];
        row = (*TwoDFts)[Y];
        for (x=0, X=nx/2; X<nx; ++x, ++X){
            newRow[x]=row[X];   // LL quadrant
            newRow[X]=row[x];   // LR quadrant
        }
    }

    // Free old, uncentered FT
    for (y=0; y<ny; ++y) free((*TwoDFts)[y]);
    free(*TwoDFts);

    // Centered FT becomes new FT
    *TwoDFts = centeredFT;

    // Checkerboard invert real and imaginary planes
    *TwoDFts=CheckeredInversion(*TwoDFts, ny);

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

int makeProjection(THD_3dim_dataset *din, THD_3dim_dataset **dout, char projCode, char *orientation){
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

    // Determine axis
    switch (tolower(projCode)){
        case 'a':
            if (orientation[0]=='S' || orientation[0]=='I') projCode='x';
            else if (orientation[1]=='S' || orientation[1]=='I') projCode='y';
            else if (orientation[2]=='S' || orientation[2]=='I') projCode='z';
            else return ERROR_SEARCH;
            break;
        case 'c':
            if (orientation[0]=='A' || orientation[0]=='P') projCode='x';
            else if (orientation[1]=='A' || orientation[1]=='P') projCode='y';
            else if (orientation[2]=='A' || orientation[2]=='P') projCode='z';
            else return ERROR_SEARCH;
            break;
        case 's':
            if (orientation[0]=='L' || orientation[0]=='R') projCode='x';
            else if (orientation[1]=='L' || orientation[1]=='R') projCode='y';
            else if (orientation[2]=='L' || orientation[2]=='R') projCode='z';
            else return ERROR_SEARCH;
            break;
    }

    // Apply required projection
    switch (projCode){
    case 'z':        // Axial projection

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
    case 'y':       // Coronal projection

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
    case 'x':       // Sagital projection

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

ERROR_NUMBER shortToFloat(THD_3dim_dataset **din){
    float   *outputBuffer;
    short   *indata;
    int     i;

    // Determine input dimensions
    int nz = DSET_NZ(*din);
    int ny = DSET_NY(*din);
    int nx = DSET_NX(*din);
    int numVoxels=nx*ny*nz;

    // Memory allocation
    if (!(outputBuffer=(float *)malloc(numVoxels*sizeof(float)))){
        return ERROR_MEMORY_ALLOCATION;
        }

    // Fill input data
    indata = DSET_ARRAY(*din, 0);

    // Convert short int data to floating point
    for (i=0; i<numVoxels; ++i){
        outputBuffer[i]=indata[i];
    }

    // Update dataset
    EDIT_dset_items( *din ,
                    ADN_type, MRI_float,
                    ADN_none ) ;
    EDIT_substitute_brick(*din, 0, MRI_float, outputBuffer);

    return ERROR_NONE;
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
    int errorNumber;
   *din = THD_open_dataset(fname);
   if( ! *din ) {
      fprintf(stderr,"** failed to read input dataset '%s'\n", fname);
      return ERROR_READING_FILE;
   }

   /* refuse to work with anything but float here */
   // int brickType=DSET_BRICK_TYPE(*din, 0);
   int brickType=DSET_BRICK_TYPE(*din, 0);
   if( brickType == MRI_float ) {
       /* data is not automatically read in, do it now */
       DSET_load(*din);
   /*
      fprintf(stderr,"** input must be of type float, failing...\n");
      return 1;
      */
   } else if (brickType==MRI_short){

       DSET_load(*din);

       if ((errorNumber=shortToFloat(din))!=ERROR_NONE)
            return errorNumber;
    //  TODO: Add code
   }

   return ERROR_NONE;
}

int Cleanup(char *inputFileName, COMPLEX ***TwoDFts, THD_3dim_dataset *din){
    int i, j, ny;

    if (inputFileName) free(inputFileName);

    for (i=0; i<6; ++i){
        if (TwoDFts[i]){
            ny = DSET_NY(din);
            for (j=0; j<ny; ++j) free(TwoDFts[i][j]);
            free(TwoDFts[i]);
        }
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

/**********************************************************************/
 ComplexPlane MakeComplexPlane(long iRows, long iColumns, COMPLEX **Data)
/**********************************************************************/
 {
	ComplexPlane	buffer;
	size_t	stBytesPerRow=iColumns * sizeof(COMPLEX);

	buffer.iRows=iRows;
	buffer.iColumns=iColumns;
	buffer.Data=Data;

	if (Data == NULL)
	{
		if ((buffer.Data = (COMPLEX **)malloc(buffer.iRows * sizeof(COMPLEX *))))
		{
			for (long i=0; i<buffer.iRows; ++i)
				if ((buffer.Data[i]=(COMPLEX *)calloc(1, stBytesPerRow)) == NULL)
				{
					for (--i; i>=0; --i) free(buffer.Data[i]);
					buffer.Data = NULL;
					return(buffer);
				}
		}
	}

	return(buffer);
 }

/*************************************************/
 void FreeComplexPlane(ComplexPlane Complex_Plane)
/*************************************************/
{
	int	i;

	for (i=0; i<Complex_Plane.iRows; ++i) free(Complex_Plane.Data[i]);
	free(Complex_Plane.Data);
	Complex_Plane.Data = NULL;
}

/**************************************************************************/
 ERROR_NUMBER CopyComplexPlane(ComplexPlane cvInput, ComplexPlane cvOutput)
/**************************************************************************/
 {
	 int	iBytes2Copy, i;

	 if ((cvOutput.iRows != cvInput.iRows) || (cvOutput.iColumns != cvInput.iColumns)) return ERROR_IMAGE_DIMENSIONS;

	 iBytes2Copy = cvInput.iColumns * sizeof(COMPLEX);
	 for (i=0; i<cvInput.iRows; ++i)
		 memcpy( (void *)(cvOutput.Data[i]), (void *)(cvInput.Data[i]), iBytes2Copy );

    return ERROR_NONE;
 }

 ERROR_NUMBER RotScaleComplexPlane(ComplexPlane *cppComplexPlane, FloatVector fvTranslation,
		float fClockwiseRotationInDegrees, float fScaleX, float fScaleY, BOOL bClip, BYTE bInterpOrder)
 {
	Matrix	mTransMatrix;
	float	faaMatrixX[9];
	long	lMatrixIndex=0;
	double	dRotationInRadians=DEGREES2RADIANS*fClockwiseRotationInDegrees;
	// Reverse rotation since matrix is supposed to give source of data, not destination.
	double	dCosTheta=cos(-dRotationInRadians), dSinTheta=sin(-dRotationInRadians);

	// If image not being clipped, determine output dimensions
	if (!bClip) DetermineUnclippedOutputDimensions(cppComplexPlane->iColumns, cppComplexPlane->iRows,
		fClockwiseRotationInDegrees, fvTranslation, 1.0f/fScaleX, 1.0f/fScaleY);

	// Reverse translation since matrix is supposed to give source of data, not destination.
	fvTranslation.x = -fvTranslation.x;
	fvTranslation.y = -fvTranslation.y;

	faaMatrixX[lMatrixIndex++] = fScaleX*(float)dCosTheta;
	faaMatrixX[lMatrixIndex++] = -fScaleX*(float)dSinTheta;
	faaMatrixX[lMatrixIndex++] = fScaleX*fvTranslation.x;
	faaMatrixX[lMatrixIndex++] = fScaleY*(float)dSinTheta;
	faaMatrixX[lMatrixIndex++] = fScaleY*(float)dCosTheta;
	faaMatrixX[lMatrixIndex++] = fScaleY*fvTranslation.y;
	faaMatrixX[lMatrixIndex++] = 0;
	faaMatrixX[lMatrixIndex++] = 0;
	faaMatrixX[lMatrixIndex++] = 1;
	mTransMatrix.fpData=&(faaMatrixX[0]);
	mTransMatrix.lRows=mTransMatrix.lColumns=3;

	return RotScaleComplexPlaneM(cppComplexPlane, mTransMatrix, bClip, bInterpOrder);
 }

ERROR_NUMBER RotScaleComplexPlaneM(ComplexPlane *cppComplexPlane, Matrix mTransMatrix, BOOL bClip, BYTE bInterpOrder)
{
	long	i, j;
	long    iMinX=bInterpOrder+1, iMaxX=cppComplexPlane->iColumns-bInterpOrder-1;
	long	iMinY=bInterpOrder+1, iMaxY=cppComplexPlane->iRows-bInterpOrder-1;
	float	**fppAmplitudeBuffer, **fppPhaseBuffer, *fpAmplitudeRow, *fpPhaseRow;
	float	**fppOutputAmplitudeBuffer, **fppOutputPhaseBuffer, *fpOutputAmplitudeRow, *fpOutputPhaseRow;
	float	fpInputVector[3], fpOutputVector[3];
	long	lHalfRows=cppComplexPlane->iRows/2, lHalfCols=cppComplexPlane->iColumns/2;
	double	dReal, dImaginary;
	COMPLEX	*cpOutputRow;
	LongVector	lvOriginalDimensions={cppComplexPlane->iColumns, cppComplexPlane->iRows};

	// Memory must have been allocated to input plane
	if (!(cppComplexPlane->Data)) return ERROR_NULL_PTR;

	// If clipping, output dimensions same as those of input
	if (bClip)
	{
		lvOutputDimensions.x=cppComplexPlane->iRows;
		lvOutputDimensions.y=cppComplexPlane->iColumns;
	}

	// Allocate memory to phase and amplitude buffers
	if (!(fppAmplitudeBuffer=(float **)malloc(cppComplexPlane->iRows*sizeof(float *))) ||
		!(fppPhaseBuffer=(float **)malloc(cppComplexPlane->iRows*sizeof(float *))) ||
		!(fppOutputAmplitudeBuffer=(float **)malloc(lvOutputDimensions.y*sizeof(float *))) ||
		!(fppOutputPhaseBuffer=(float **)malloc(lvOutputDimensions.y*sizeof(float *))))
	{
		if (fppAmplitudeBuffer) free(fppAmplitudeBuffer);
		if (fppPhaseBuffer) free(fppPhaseBuffer);
		if (fppOutputAmplitudeBuffer) free(fppOutputAmplitudeBuffer);
		return ERROR_MEMORY_ALLOCATION;
	}
	for (i=0; i<cppComplexPlane->iRows; ++i)
		if (!(fppAmplitudeBuffer[i]=(float *)malloc(cppComplexPlane->iColumns*sizeof(float))) ||
			!(fppPhaseBuffer[i]=(float *)malloc(cppComplexPlane->iColumns*sizeof(float))))
		{
			if (fppAmplitudeBuffer[i]) free(fppAmplitudeBuffer[i]);
			for (--i; i>=0; --i)
			{
				free(fppAmplitudeBuffer[i]);
				free(fppPhaseBuffer[i]);
			}
			free(fppAmplitudeBuffer);
			free(fppPhaseBuffer);
			free(fppOutputAmplitudeBuffer);
			free(fppOutputPhaseBuffer);
			return ERROR_MEMORY_ALLOCATION;
		}
	for (i=0; i<lvOutputDimensions.y; ++i)
		if (!(fppOutputAmplitudeBuffer[i]=(float *)malloc(lvOutputDimensions.x*sizeof(float))) ||
			!(fppOutputPhaseBuffer[i]=(float *)malloc(lvOutputDimensions.x*sizeof(float))))
		{
			if (fppOutputAmplitudeBuffer[i]) free(fppOutputAmplitudeBuffer[i]);
			for (--i; i>=0; --i)
			{
				free(fppOutputAmplitudeBuffer[i]);
				free(fppOutputPhaseBuffer[i]);
			}
			for (i=0; i<cppComplexPlane->iRows; ++i)
			{
				free(fppAmplitudeBuffer[i]);
				free(fppPhaseBuffer[i]);
			}
			free(fppAmplitudeBuffer);
			free(fppPhaseBuffer);
			free(fppOutputAmplitudeBuffer);
			free(fppOutputPhaseBuffer);
			return ERROR_MEMORY_ALLOCATION;
		}

	// Load phase and amplitude buffers
	for (i=0; i<cppComplexPlane->iRows; ++i)
	{
		fpAmplitudeRow=fppAmplitudeBuffer[i];
		fpPhaseRow=fppPhaseBuffer[i];
		for (j=0; j<cppComplexPlane->iColumns; ++j)
		{
			dReal=cppComplexPlane->Data[i][j].real;
			dImaginary=cppComplexPlane->Data[i][j].imag;
			fpAmplitudeRow[j] = (float)Pythag(dReal, dImaginary);
			fpPhaseRow[j] = (float)((dImaginary==0)? 0 : ((dReal==0.0)? ((dImaginary>0)? MATH_PI_2 : -MATH_PI_2 ) : atan((dImaginary/dReal))));
			if (dReal<0)
			{
				fpPhaseRow[j]+=(float)MATH_PI;
				if (fpPhaseRow[j]>(float)CIRCLE) fpPhaseRow[j]-=(float)CIRCLE;
				else if (fpPhaseRow[j]<0) fpPhaseRow[j]+=(float)CIRCLE;
			}
		}
	}

	if (bClip)
	{
		fpInputVector[2] = 1;
		for (i=0; i<cppComplexPlane->iRows; ++i)
		{
			fpOutputAmplitudeRow=fppOutputAmplitudeBuffer[i];
			fpOutputPhaseRow=fppOutputPhaseBuffer[i];

			fpInputVector[1] = (float)(i-lHalfRows);	// Rotation around centre of image
			for (j=0; j<cppComplexPlane->iColumns; ++j)
			{
				fpInputVector[0] = (float)(j-lHalfCols);	// Rotation around centre of image

				// Determine transformation from point on output image to corresponding point on input image
				MatVectMult(mTransMatrix, &(fpInputVector[0]), &(fpOutputVector[0]));

				// Correct for offset from UL corner to centre
				fpOutputVector[0] += lHalfCols;
				fpOutputVector[1] += lHalfRows;

				// Find corresponding value if input image point within input image
				if (bInterpOrder>NEAREST_NEIGHBOR_INTERPOLATION && fpOutputVector[0]>iMinX && fpOutputVector[0]<iMaxX &&
					fpOutputVector[1]>iMinY && fpOutputVector[1]<iMaxY)
				{
					fpOutputAmplitudeRow[j] = (
						(bInterpOrder==BILINEAR_INTERPOLATION)? (float)Interp_BL(fppAmplitudeBuffer, fpOutputVector[0], fpOutputVector[1]) :
					((bInterpOrder==BIQUADRATIC_INTERPOLATION)? (float)Interp_BQ(fppAmplitudeBuffer, fpOutputVector[0], fpOutputVector[1]) :
						(float)Interp_BC(fppAmplitudeBuffer, fpOutputVector[0], fpOutputVector[1])));
					fpOutputPhaseRow[j] = (
						(bInterpOrder==BILINEAR_INTERPOLATION)? (float)Interp_BL(fppPhaseBuffer, fpOutputVector[0], fpOutputVector[1]) :
					((bInterpOrder==BIQUADRATIC_INTERPOLATION)? (float)Interp_BQ(fppPhaseBuffer, fpOutputVector[0], fpOutputVector[1]) :
						(float)Interp_BC(fppPhaseBuffer, fpOutputVector[0], fpOutputVector[1])));
				}
				else
				{
					FixedPtVector	iNearestNeighbor={Round(fpOutputVector[0]), Round(fpOutputVector[1])};

					if (iNearestNeighbor.x>=0 && iNearestNeighbor.x<cppComplexPlane->iColumns &&
						iNearestNeighbor.y>=0 && iNearestNeighbor.y<cppComplexPlane->iRows)
					{
						fpOutputAmplitudeRow[j] = (fppAmplitudeBuffer[iNearestNeighbor.y][iNearestNeighbor.x]);
						fpOutputPhaseRow[j] = (fppPhaseBuffer[iNearestNeighbor.y][iNearestNeighbor.x]);
					}
					else
						fpOutputAmplitudeRow[j] = fpOutputPhaseRow[j] = 0;
				}
			}
		}
	}
	else
	{
		LongRange	lrXRange, lrYRange;
		long		lLargeX, lLargeY, lXOffset, lYOffset, x, y;

		// Ensure output dimensions have been determined
		if (lvOutputDimensions.x==-1) return ERROR_IMAGE_DIMENSIONS;

		// Determine dimensions of large rectangle that can just hold both the input and output images
		lLargeX=fmax(lvOutputDimensions.x, cppComplexPlane->iColumns);
		lLargeY=fmax(lvOutputDimensions.y, cppComplexPlane->iRows);
		lHalfCols=lLargeX/2;
		lHalfRows=lLargeY/2;

		// Determine output range relative to large rectangle
		if (lvOutputDimensions.x<cppComplexPlane->iColumns)
		{
			lrXRange.lMin=0;
			lrXRange.lMax=lvOutputDimensions.x;
			lXOffset=-lLargeX/2;
		}
		else
		{
			lrXRange.lMin=fmax(0, (lLargeX-lvOutputDimensions.x)/2);
			lrXRange.lMax=fmin(lvOutputDimensions.x, lrXRange.lMin+lvOutputDimensions.x);
			lXOffset=(lLargeX-cppComplexPlane->iColumns)/2;
		}
		if (lvOutputDimensions.y<cppComplexPlane->iRows)
		{
			lrYRange.lMin=0;
			lrYRange.lMax=lvOutputDimensions.y;
			lYOffset=-lLargeY/2;
		}
		else
		{
			lrYRange.lMin=fmax(0, (lLargeY-lvOutputDimensions.y)/2);
			lrYRange.lMax=fmin(lvOutputDimensions.y, lrYRange.lMin+lvOutputDimensions.y);
			lYOffset=(lLargeY-cppComplexPlane->iRows)/2;
		}

		// Fill output plane
		for (i=lrYRange.lMin, y=0; i<lrYRange.lMax; ++i)
		{
			fpOutputAmplitudeRow=fppOutputAmplitudeBuffer[y];
			fpOutputPhaseRow=fppOutputPhaseBuffer[y];

			fpInputVector[1] = (float)(i-lHalfRows);	// Rotation around centre of image
			for (j=lrXRange.lMin, x=0; j<lrXRange.lMax; ++j)
			{
				fpInputVector[0] = (float)(j-lHalfCols);	// Rotation around centre of image

				// Determine transformation from point on output image to corresponding point on input image
				MatVectMult(mTransMatrix, &(fpInputVector[0]), &(fpOutputVector[0]));

				// Correct for offset from UL corner to centre
				fpOutputVector[0] += lHalfCols-lXOffset;
				fpOutputVector[1] += lHalfRows-lYOffset;

				// Find corresponding value if input image point within input image
				if (bInterpOrder==NEAREST_NEIGHBOR_INTERPOLATION || fpOutputVector[0]<=iMinX || fpOutputVector[0]>=iMaxX ||
					fpOutputVector[1]<=iMinY || fpOutputVector[1]>=iMaxY)
				{
					FixedPtVector	iNearestNeighbor={Round(fpOutputVector[0]), Round(fpOutputVector[1])};

					if (iNearestNeighbor.x>=0 && iNearestNeighbor.x<cppComplexPlane->iColumns &&
						iNearestNeighbor.y>=0 && iNearestNeighbor.y<cppComplexPlane->iRows)
					{
						fpOutputAmplitudeRow[j] = (fppAmplitudeBuffer[iNearestNeighbor.y][iNearestNeighbor.x]);
						fpOutputPhaseRow[j] = (fppPhaseBuffer[iNearestNeighbor.y][iNearestNeighbor.x]);
					}
					else
					{
						if (iNearestNeighbor.x<0) ++(iNearestNeighbor.x);
						else if (iNearestNeighbor.x>=cppComplexPlane->iColumns) --(iNearestNeighbor.x);
						if (iNearestNeighbor.y<0) ++(iNearestNeighbor.y);
						else if (iNearestNeighbor.y>=cppComplexPlane->iRows) --(iNearestNeighbor.y);

						if (iNearestNeighbor.x>=0 && iNearestNeighbor.x<cppComplexPlane->iColumns &&
							iNearestNeighbor.y>=0 && iNearestNeighbor.y<cppComplexPlane->iRows)
						{
							fpOutputAmplitudeRow[j] = fppAmplitudeBuffer[iNearestNeighbor.y][iNearestNeighbor.x];
							fpOutputPhaseRow[j] = fppPhaseBuffer[iNearestNeighbor.y][iNearestNeighbor.x];
						}
						else fpOutputAmplitudeRow[j] = fpOutputPhaseRow[j] = 0;
					}
				}
				else
				{
					fpOutputAmplitudeRow[j] = (
						(bInterpOrder==BILINEAR_INTERPOLATION)? (float)Interp_BL(fppAmplitudeBuffer, fpOutputVector[0], fpOutputVector[1]) :
					((bInterpOrder==BIQUADRATIC_INTERPOLATION)? (float)Interp_BQ(fppAmplitudeBuffer, fpOutputVector[0], fpOutputVector[1]) :
						(float)Interp_BC(fppAmplitudeBuffer, fpOutputVector[0], fpOutputVector[1])));
					fpOutputPhaseRow[j] = (
						(bInterpOrder==BILINEAR_INTERPOLATION)? (float)Interp_BL(fppPhaseBuffer, fpOutputVector[0], fpOutputVector[1]) :
					((bInterpOrder==BIQUADRATIC_INTERPOLATION)? (float)Interp_BQ(fppPhaseBuffer, fpOutputVector[0], fpOutputVector[1]) :
						(float)Interp_BC(fppPhaseBuffer, fpOutputVector[0], fpOutputVector[1])));
				}
				++x;
			}
			++y;
		}

		// Free input plane
		FreeComplexPlane(*cppComplexPlane);

		// Make new plane with new dimensions
		*cppComplexPlane=MakeComplexPlane(lvOutputDimensions.y, lvOutputDimensions.x, NULL);
		if (!(cppComplexPlane->Data))
		{
			for (i=0; i<lvOriginalDimensions.y; ++i)
			{
				free(fppAmplitudeBuffer[i]);
				free(fppPhaseBuffer[i]);
			}
			for (i=0; i<lvOutputDimensions.y; ++i)
			{
				free(fppOutputAmplitudeBuffer[i]);
				free(fppOutputPhaseBuffer[i]);
			}
			free(fppAmplitudeBuffer);
			free(fppPhaseBuffer);
			free(fppOutputAmplitudeBuffer);
			free(fppOutputPhaseBuffer);
			return ERROR_MEMORY_ALLOCATION;
		}
	}

	// Get complex values from phase and amplitude planes
	for (i=0; i<lvOutputDimensions.y; ++i)
	{
		fpOutputAmplitudeRow=fppOutputAmplitudeBuffer[i];
		fpOutputPhaseRow=fppOutputPhaseBuffer[i];
		cpOutputRow=cppComplexPlane->Data[i];

		for (j=0; j<lvOutputDimensions.x; ++j)
		{
		/*  DEBUG
			cpOutputRow[j].real=1;
			cpOutputRow[j].imag=1;
			*/
			cpOutputRow[j].real=(float)(fpOutputAmplitudeRow[j]*cos((double)(fpOutputPhaseRow[j])));
			cpOutputRow[j].imag=(float)(fpOutputAmplitudeRow[j]*sin((double)(fpOutputPhaseRow[j])));
		}
	}

	for (i=0; i<lvOriginalDimensions.y; ++i)
	{
		free(fppAmplitudeBuffer[i]);
		free(fppPhaseBuffer[i]);
	}
	for (i=0; i<lvOutputDimensions.y; ++i)
	{
		free(fppOutputAmplitudeBuffer[i]);
		free(fppOutputPhaseBuffer[i]);
	}
	free(fppAmplitudeBuffer);
	free(fppPhaseBuffer);
	free(fppOutputAmplitudeBuffer);
	free(fppOutputPhaseBuffer);

	return ERROR_NONE;
}


ERROR_NUMBER MakeCardinalRadialPhaseSamples(FloatPlane fpReal, FloatPlane fpImaginary,
			short *spCardinalIndices, IntRange lrFrequencyRange, float ***fpppRadialSamples)
{
	ERROR_NUMBER	enErrorNumber;
	COMPLEX	**cpppComplexRadialSample;
	int		iCenter=fpImaginary.iColumns/2, iRadiusIndex;
	int	iQuadIndex, iaQuadIndices[4]={iCenter, iCenter, iCenter, iCenter};
	size_t	stBufferSize=lrFrequencyRange.iMax+1;

	if (!(cpppComplexRadialSample=(COMPLEX **)malloc(4*sizeof(COMPLEX *)))) return ERROR_MEMORY_ALLOCATION;
	for (iQuadIndex=0; iQuadIndex<4; ++iQuadIndex)
		if (!(cpppComplexRadialSample[iQuadIndex]=(COMPLEX *)malloc(stBufferSize*sizeof(COMPLEX))))
		{
			for (--iQuadIndex; iQuadIndex>=0; --iQuadIndex) free(cpppComplexRadialSample[iQuadIndex]);
			free(cpppComplexRadialSample);
			return ERROR_MEMORY_ALLOCATION;
		}

	// Make complex radial samples
	for (iRadiusIndex=0; iRadiusIndex<lrFrequencyRange.iMax; ++iRadiusIndex)
	{
		// 0 degrees (positive along x-axis)
		cpppComplexRadialSample[0][iRadiusIndex].real=fpReal.Data[iCenter][iaQuadIndices[0]];
		cpppComplexRadialSample[0][iRadiusIndex].imag=fpImaginary.Data[iCenter][iaQuadIndices[0]++];

		// 90 degrees (positive along y-axis)
		cpppComplexRadialSample[1][iRadiusIndex].real=fpReal.Data[iaQuadIndices[1]][iCenter];
		cpppComplexRadialSample[1][iRadiusIndex].imag=fpImaginary.Data[iaQuadIndices[1]++][iCenter];

		// 180 degrees (negative along x-axis)
		cpppComplexRadialSample[2][iRadiusIndex].real=fpReal.Data[iCenter][iaQuadIndices[2]];
		cpppComplexRadialSample[2][iRadiusIndex].imag=fpImaginary.Data[iCenter][iaQuadIndices[2]--];

		// 270 degrees (negative along y-axis)
		cpppComplexRadialSample[3][iRadiusIndex].real=fpReal.Data[iaQuadIndices[3]][iCenter];
		cpppComplexRadialSample[3][iRadiusIndex].imag=fpImaginary.Data[iaQuadIndices[3]--][iCenter];
	}

	// Make radial phase vector samples from complex radial samples
	for (iQuadIndex=0; iQuadIndex<4; ++iQuadIndex)
	{
		if ((enErrorNumber=MakeRadialPhaseVectorSample(cpppComplexRadialSample[iQuadIndex], lrFrequencyRange.iMax+1,
			&((*fpppRadialSamples)[spCardinalIndices[iQuadIndex]])))!=ERROR_NONE)
		{
			for (iQuadIndex=0; iQuadIndex<4; ++iQuadIndex) free(cpppComplexRadialSample[iQuadIndex]);
			free(cpppComplexRadialSample);
			return enErrorNumber;
		}
	}

	// Cleanup
	for (iQuadIndex=0; iQuadIndex<4; ++iQuadIndex) free(cpppComplexRadialSample[iQuadIndex]);
	free(cpppComplexRadialSample);
	return ERROR_NONE;
}


ERROR_NUMBER ComparePhaseSamples(float *fpTargetSample, float *fpRefSample,
										IntRange lrFrequencyRange, float *fpComparisonMetric)
{
	ERROR_NUMBER	enErrorNumber;
	float			*fpPhaseShiftVector, fPhaseShiftSlope, fPhaseShiftIntercept, fMSE;
	long			lVectorLength;

	// Make phase shift vector
	if ((enErrorNumber=MakePhaseShiftVector(fpTargetSample, fpRefSample, lrFrequencyRange, &lVectorLength,
		&fpPhaseShiftVector))!=ERROR_NONE)	return enErrorNumber;

	// Get slope of phase shift vector
	GetSlope(fpPhaseShiftVector, lrFrequencyRange, &fPhaseShiftSlope, &fPhaseShiftIntercept);

	// Determine MSE for phase shift vector
	fMSE=GetMSE(fpPhaseShiftVector, fPhaseShiftSlope, fPhaseShiftIntercept, lrFrequencyRange);

	// Multiplicative inverse of MSE becomes comparison metric
	*fpComparisonMetric=(fMSE==0.0f)? 1.0e7f : 1.0f/fMSE;

	// Cleanup
	free(fpPhaseShiftVector);

	return ERROR_NONE;
}
/* Temporarily commented out 2020-09-28
float ExpectedAngularShift(int iCurrentAxis)
{
	switch (iCurrentAxis)
	{
	case X_AXIS:
		if (v3DTestRotation.y==0 && v3DTestRotation.z==0) return v3DTestRotation.x;
		if (v3DTestRotation.z==0) return v3DTestRotation.x;
		return (float)(25.75 * sin(2.0 * DEGREES2RADIANS * v3DTestRotation.x));
	case Y_AXIS:
		if (v3DTestRotation.x==0 && v3DTestRotation.z==0) return v3DTestRotation.y;
		if (v3DTestRotation.z==0) return -v3DTestRotation.y;
		return (float)(-90.13 * sin(DEGREES2RADIANS * v3DTestRotation.y));
	case Z_AXIS:
		if (v3DTestRotation.x==0 && v3DTestRotation.y==0) return v3DTestRotation.z;
		if (v3DTestRotation.z==0) return (float)(-v3DTestRotation.z+(24.4*sin(2.0 * DEGREES2RADIANS * v3DTestRotation.z)));
		return (float)(25.11 * sin(2.0 * DEGREES2RADIANS * v3DTestRotation.z));
	default: return ERROR_PARAMETERS;
	}
}
*/

float GetAngularShift(FloatVector fvCoords)
{
	float	fBuffer;

	fBuffer=fvCoords.y-fvCoords.x;
	if (fBuffer>180) fBuffer-=360;
	else if (fBuffer<-180) fBuffer+=360;

	return fBuffer;
}

 void DetermineUnclippedOutputDimensions(long lInputColumns, long lInputRows,
	 float fClockwiseRotationInDegrees, FloatVector fvTranslation, float fScaleX, float fScaleY)
 {
	double	dRotationInRadians;
	double	dCosTheta, dSinTheta;

	// Confine rotation angle into (-180,180] degree range.
	 if (fClockwiseRotationInDegrees>180) fClockwiseRotationInDegrees-=360;
	 else if (fClockwiseRotationInDegrees<=-180) fClockwiseRotationInDegrees+=360;
	 dRotationInRadians=DEGREES2RADIANS*fClockwiseRotationInDegrees;

	 // Determine new dimensions on the basis of rotation
	 if (fClockwiseRotationInDegrees<-90)	// Rotation in (-180,-90) degrees
	 {
		 dRotationInRadians=-MATH_PI_2-dRotationInRadians;
		 dCosTheta=cos(dRotationInRadians);
		 dSinTheta=sin(dRotationInRadians);
		 lvOutputDimensions.x=DRound((dSinTheta*lInputColumns)+(dCosTheta*lInputRows)+0.4999999999999);
		 lvOutputDimensions.y=DRound((dCosTheta*lInputColumns)+(dSinTheta*lInputRows)+0.4999999999999);
	 }
	 else if (fClockwiseRotationInDegrees<0)	// Rotation in [-90,0) degrees
	 {
		 dCosTheta=cos(-dRotationInRadians);
		 dSinTheta=sin(-dRotationInRadians);
		 lvOutputDimensions.x=DRound((dCosTheta*lInputColumns)+(dSinTheta*lInputRows)+0.4999999999999);
		 lvOutputDimensions.y=DRound((dSinTheta*lInputColumns)+(dCosTheta*lInputRows)+0.4999999999999);
	 }
	 else if (fClockwiseRotationInDegrees<=90)	// Rotation in [0,90] degrees
	 {
		 dCosTheta=cos(dRotationInRadians);
		 dSinTheta=sin(dRotationInRadians);
		 lvOutputDimensions.x=DRound((dCosTheta*lInputColumns)+(dSinTheta*lInputRows)+0.4999999999999);
		 lvOutputDimensions.y=DRound((dSinTheta*lInputColumns)+(dCosTheta*lInputRows)+0.4999999999999);
	 }
	 else	// Rotation in (90,180] degrees
	 {
		 dRotationInRadians-=MATH_PI_2;
		 dCosTheta=cos(dRotationInRadians);
		 dSinTheta=sin(dRotationInRadians);
		 lvOutputDimensions.x=DRound((dSinTheta*lInputColumns)+(dCosTheta*lInputRows)+0.4999999999999);
		 lvOutputDimensions.y=DRound((dCosTheta*lInputColumns)+(dSinTheta*lInputRows)+0.4999999999999);
	 }

	 // Determine new dimensions on the basis of scaling
	 lvOutputDimensions.x=Round(fScaleX*lvOutputDimensions.x);
	 lvOutputDimensions.y=Round(fScaleY*lvOutputDimensions.y);;

	 // Determine new dimensions on the basis of translation
	 lvOutputDimensions.x+=DRound(fabs(fvTranslation.x));
	 lvOutputDimensions.y+=DRound(fabs(fvTranslation.y));
 }

void MatVectMult(Matrix mMatrix, float *fpInVector, float *fpOutVector)
{
   long i, j;
   long	lMatrixIndex;

   for (i=0; i<mMatrix.lRows; ++i)
   {
	   fpOutVector[i] = 0;
	   lMatrixIndex = i*mMatrix.lColumns;
	  for (j=0; j<mMatrix.lColumns; ++j)
		  fpOutVector[i]+=mMatrix.fpData[lMatrixIndex++]*fpInVector[j];
   }
}

ERROR_NUMBER MakeRadialPhaseVectorSample(COMPLEX *cpComplexRadialSample, int iLength, float **fppRadialSample)
{
	float   fFrom, fTo, fPhase, fOffset=0, fPrevious=0, fChange;
	int		iQuad[2], i;
	double	dThreshold=20, dChangeThresh=MATH_PI_4*3;

	// Memory allocation
	if (!((*fppRadialSample)=(float *)malloc(iLength*sizeof(float)))) return ERROR_MEMORY_ALLOCATION;
/* DEBUG */

	(*fppRadialSample)[0]=0;
	for (i=1; i<iLength; ++i)
    {
        /* DEBUG */

		iQuad[1]=(cpComplexRadialSample[i].real>0.0)?
			((cpComplexRadialSample[i].imag>0.0)? 1 : 4) : ((cpComplexRadialSample[i].imag>0.0)? 2 : 3);

		fTo=(cpComplexRadialSample[i].real==0)? ((cpComplexRadialSample[i].real>0)? (float)MATH_PI_2 : -(float)MATH_PI_2) :
			(float)atan((double)(cpComplexRadialSample[i].imag/cpComplexRadialSample[i].real));
		fFrom=fPrevious-fOffset;

		if (i>1) switch (iQuad[1])
		{
		case 1: if (fFrom< -MATH_PI) fTo -= (float)CIRCLE;
			if (iQuad[0]==4&&fFrom>MATH_PI)
				{
				fOffset+=(float)CIRCLE;
				}
		break;

		case 4: if (fFrom> MATH_PI) fTo += (float)CIRCLE;
			if (iQuad[0]==1&&fFrom< -(float)MATH_PI)
				{
				fOffset-=(float)CIRCLE;
				}
		break;

		case 2: if (fFrom> 0) fTo += (float)MATH_PI;
				else fTo -= (float)MATH_PI;
		break;

		case 3: if (fFrom> 0) fTo += (float)MATH_PI;
				else fTo -= (float)MATH_PI;
		break;
		}

		fPhase = fOffset+fTo;

		// Correct for phase reversal
		fChange=fPhase-fPrevious;
		if (fChange>dChangeThresh &&
			((FloatPythag((cpComplexRadialSample[i].imag), (cpComplexRadialSample[i].real))<dThreshold) ||
			(FloatPythag((cpComplexRadialSample[i-1].imag), (cpComplexRadialSample[i-1].real))<dThreshold)))
		{
			fOffset-=(float)MATH_PI;
			fPhase-=(float)MATH_PI;
		}
		else if (fChange<-dChangeThresh &&
			((FloatPythag((cpComplexRadialSample[i].imag), (cpComplexRadialSample[i].real))<dThreshold) ||
			(FloatPythag((cpComplexRadialSample[i-1].imag), (cpComplexRadialSample[i-1].real))<dThreshold)))
		{
			fOffset+=(float)MATH_PI;
			fPhase+=(float)MATH_PI;
		}

		iQuad[0]=iQuad[1];
		fPrevious=fPhase;
// #if UNIX
#if 1
		{
			char	cDummy;

			// Debug
			if (fabs((double)fPhase)>1400)
			{
				fprintf(stderr, "fPhase=%f, i=%d, cpComplexRadialSample[i].fReal=%f\n",
					fPhase, i, cpComplexRadialSample[i].real);
				fprintf(stderr, "cpComplexRadialSample[i].fImaginary=%f, cpComplexRadialSample[i-1].fReal=%f\n",
					cpComplexRadialSample[i].imag, cpComplexRadialSample[i-1].real);
				fprintf(stderr, "cpComplexRadialSample[i-1].fImaginary=%f\n", cpComplexRadialSample[i-1].imag);
				fscanf(stdin, "%c", &cDummy);
				fprintf(stderr, "\n");
			}
		}
#endif
		(*fppRadialSample)[i]=fPhase;
		/**/
    }
/**/
	return ERROR_NONE;
}

ERROR_NUMBER MakePhaseShiftVector(float *fpTargetSample, float *fpRefSample, IntRange lrFrequencyRange,
								  long *lpVectorLength, float **fppPhaseShiftVector)
{
	ERROR_NUMBER enErrorNumber;
	int	iInIndex, iOutIndex;
	float	*fpShiftPtr;
	float   fPrevious, fDiffUp, fDiffDown, fDiffHere, fOffset=0, fDiffThresh=0.1f;

	// Allocate memory to phase shift vector
	*lpVectorLength=lrFrequencyRange.iMax+1;
	if (!(*fppPhaseShiftVector=(float *)malloc(*lpVectorLength*sizeof(float)))) return ERROR_MEMORY_ALLOCATION;

	// Fill vector.
	fpShiftPtr=&((*fppPhaseShiftVector)[0]);
	for (iOutIndex=0, iInIndex=0; iOutIndex<*lpVectorLength; ++iOutIndex)
	{
		*fpShiftPtr=fpTargetSample[iInIndex]-fpRefSample[iInIndex]+fOffset;

		if (iOutIndex)
		{
			fDiffUp=(*fpShiftPtr+(float)CIRCLE)-fPrevious;
			fDiffDown=fPrevious-*fpShiftPtr+(float)CIRCLE;
			fDiffHere=(float)fabs(fPrevious-*fpShiftPtr);

			do
			{
				if (fDiffHere>fDiffDown || fDiffHere>fDiffUp)
				{
					if (fDiffUp<fDiffDown)
					{
						*fpShiftPtr += (float)CIRCLE;
						fOffset += (float)CIRCLE;
					}
					else
					{
						*fpShiftPtr -= (float)CIRCLE;
						fOffset -= (float)CIRCLE;
					}
				}

				fDiffUp=(*fpShiftPtr+(float)CIRCLE)-fPrevious;
				fDiffDown=fPrevious-*fpShiftPtr+(float)CIRCLE;
				fDiffHere=(float)fabs(fPrevious-*fpShiftPtr)-fDiffThresh;
			} while (fDiffHere>fDiffDown || fDiffHere>fDiffUp);
		}
		fPrevious=(iOutIndex<3)? *fpShiftPtr : (2.0f*fPrevious + *fpShiftPtr)/3.0f;
		++fpShiftPtr;
		++iInIndex;
	}

	// Median filter phase shift vector
	if ((enErrorNumber=MedianFilterFloatVector(*fppPhaseShiftVector, *lpVectorLength))!=ERROR_NONE)
	{
		free(*fppPhaseShiftVector);
		return enErrorNumber;
	}

	return ERROR_NONE;
}


void GetSlope(float *fVector, IntRange lrFrequencyRange, float *fpSlope, float *fpIntercept)
{
	float   fXY, fXSquared, *fpShiftPtr;
	float	fXMean=0, fYMean=0, fXMinusXMean, fYMinusYMean;
	int		i, iCount=lrFrequencyRange.iMax-lrFrequencyRange.iMin;

	// Determine mean X and MeanY
	fpShiftPtr=fVector+lrFrequencyRange.iMin;
	for (i=lrFrequencyRange.iMin; i<lrFrequencyRange.iMax; ++i, ++fpShiftPtr)
	{
		fXMean += i;
		fYMean += *fpShiftPtr;
	}
	fXMean /= iCount;
	fYMean /= iCount;

	fpShiftPtr=fVector+lrFrequencyRange.iMin;
	fXY=fXSquared=0.0f;
	for (i=lrFrequencyRange.iMin; i<lrFrequencyRange.iMax; ++i, ++fpShiftPtr)
	{
		fXMinusXMean = (float)i - fXMean;
		fYMinusYMean = *fpShiftPtr - fYMean;
	    fXY += fXMinusXMean * fYMinusYMean;
	    fXSquared += fXMinusXMean * fXMinusXMean;
	}

	*fpSlope=fXY/fXSquared;
	*fpIntercept=fYMean-(*fpSlope * fXMean);
}

float GetMSE(float *fVector, float fSlope, float fPhaseShiftIntercept, IntRange lrFrequencyRange)
{
	float   *fpShiftPtr, fXExpected, fXObserved, fMse, fDiff;
	float	fOffset=fPhaseShiftIntercept;
	int	    i;

	fpShiftPtr=fVector+lrFrequencyRange.iMin;
	fMse=0.0;

	for (i=lrFrequencyRange.iMin; i<lrFrequencyRange.iMax; ++i, ++fpShiftPtr)
    {
		fXExpected = (fSlope*i)+fOffset;
		fXObserved = *fpShiftPtr;
		fDiff=fXObserved-fXExpected;
		fMse+=fDiff*fDiff;
    }

	return(fMse/(float)(lrFrequencyRange.iMax-lrFrequencyRange.iMin));
}

ERROR_NUMBER MedianFilterFloatVector(float *fpVector, long lLength)
{
	float	*fpBuffer, *fpBufferPtr;
	size_t	stBytes2Copy=lLength*sizeof(float);
	int		i, iLast=lLength-1;

	// Copy vector into buffer
	if (!(fpBuffer=(float *)malloc(stBytes2Copy))) return ERROR_MEMORY_ALLOCATION;
	memcpy((void *)fpBuffer, (void *)fpVector, stBytes2Copy);

	// Write median filtered version of buffer back into vector.
	for (i=1, fpBufferPtr=&(fpBuffer[0]); i<iLast; ++i) fpVector[i]=Float3x1Median(fpBufferPtr++);

	// Cleanup
	free(fpBuffer);
	return ERROR_NONE;
}

float Float3x1Median(float *fpInput)
{
    return((fpInput[0]>fpInput[1])? ((fpInput[1]>fpInput[2])? fpInput[1] : (fpInput[0]>fpInput[2])? fpInput[2] : fpInput[0]) :
		((fpInput[1]<fpInput[2])? fpInput[1] : (fpInput[0]<fpInput[2])? fpInput[2] : fpInput[0]));
}


/*******************************************/
 void FreeFloatPlane(FloatPlane Float_Plane)
/*******************************************/
{
	int	i;

	for (i=0; i<Float_Plane.iRows; ++i) free(Float_Plane.Data[i]);
	free(Float_Plane.Data);
	Float_Plane.Data = NULL;
}

/*********************/
 int Round(float arg) 		// Rounds floating point value off to the nearest integer
/*********************/
 {
	arg+=(arg<0)? -0.5f : 0.5f;
	return((int)arg);
 }

/***********************************************/
 ERROR_NUMBER ErrorOpeningFile(char *csFileName)
/***********************************************/
 {
	 fprintf(stderr, "Error opening %s\n", csFileName);
	 perror("Reason");

	 return ERROR_OPENING_FILE;
 }

/*******************************************/
 float FloatPythag(float fArg1, float fArg2)
/*******************************************/
 {
	 return (float)(sqrt((double)((fArg1*fArg1)+(fArg2*fArg2))));
 }

/******************************************/
  double Pythag(double dArg1, double dArg2)
/******************************************/
 {
	 return(sqrt((dArg1*dArg1)+(dArg2*dArg2)));
 }

/****************************************************************************/
 double	Interp_BC(float **image, float x_coord, float y_coord)
/****************************************************************************/
/*
 Bicubic Interpolation
*/
{
int	i, j, x[4], y[4], X, Y;

double 	Phi[4], Psi[4];
double	x_x0, x_x1, x_x2, x_x3;
double	y_y0, y_y1, y_y2, y_y3;

double	Pxy=(double)0;

x[0]=(int)(x_coord+0.5)-1;
y[0]=(int)(y_coord+0.5)-1;
for (i=1; i<4; ++i)
    {
    x[i]=*x + i;
    y[i]=*y + i;
    }

x_x0=x_coord-x[0];		/* Numerator factors */
x_x1=x_coord-x[1];
x_x2=x_coord-x[2];
x_x3=x_coord-x[3];

y_y0=y_coord-y[0];
y_y1=y_coord-y[1];
y_y2=y_coord-y[2];
y_y3=y_coord-y[3];

Phi[0]=(x_x1*x_x2*x_x3)/(-6);
Phi[1]=(x_x0*x_x2*x_x3)/(2);
Phi[2]=(x_x0*x_x1*x_x3)/(-2);
Phi[3]=(x_x0*x_x1*x_x2)/(6);

Psi[0]=(y_y1*y_y2*y_y3)/(-6);
Psi[1]=(y_y0*y_y2*y_y3)/(2);
Psi[2]=(y_y0*y_y1*y_y3)/(-2);
Psi[3]=(y_y0*y_y1*y_y2)/(6);

X=x[0]; Y=y[0];

for (i=0; i<4; ++i)
    {
    for (j=0; j<4; ++j) Pxy += *(*(image+Y)+X++)*Psi[i]*Phi[j];

    Y++; X= *x;
    }

return(Pxy);
}

/***************************************************************************/
  double Interp_BQ(float **image, float x_coord, float y_coord)
/***************************************************************************/
/*
 Biquadratic Interpolation
*/
{
int	i, j, x[3], y[3], X, Y;

double	Phi[3], Psi[3];
double	x_x0, x_x1, x_x2;
double	y_y0, y_y1, y_y2;

double	Pxy=(double)0;

x[0]=(int)(x_coord+0.5)-1;
y[0]=(int)(y_coord+0.5)-1;

for (i=1; i<3; ++i)
    {
    x[i]=*x + i;
    y[i]=*y + i;
    }

x_x0=x_coord-x[0];		/* Numerator factors */
x_x1=x_coord-x[1];
x_x2=x_coord-x[2];

y_y0=y_coord-y[0];
y_y1=y_coord-y[1];
y_y2=y_coord-y[2];

Phi[0]=(x_x1*x_x2)/(2);
Phi[1]=(x_x0*x_x2)/(-1);
Phi[2]=(x_x0*x_x1)/(2);

Psi[0]=(y_y1*y_y2)/(2);
Psi[1]=(y_y0*y_y2)/(-1);
Psi[2]=(y_y0*y_y1)/(2);

X=x[0]; Y=y[0];

for (i=0; i<3; ++i)
    {
    for (j=0; j<3; ++j)
		Pxy += *(*(image+Y)+X++)*Psi[i]*Phi[j];

    Y++; X= *x;
    }

return(Pxy);
}

/**************************************************************/
  double Interp_BL(float **image, float x_coord, float y_coord)
/**************************************************************/
/*
 Bilinear Interpolation
*/
{
int	i, j, x[2], y[2], X, Y;

double	Phi[2], Psi[2];
double	x_x0, x_x1;
double	y_y0, y_y1;

double	Pxy=(double)0;

x[0]=(int)x_coord;
y[0]=(int)y_coord;

for (i=1; i<2; ++i)
    {
    x[i]=*x + i;
    y[i]=*y + i;
    }

x_x0=x_coord-x[0];		/* Numerator factors */
x_x1=x_coord-x[1];

y_y0=y_coord-y[0];
y_y1=y_coord-y[1];

Phi[0]= -x_x1;
Phi[1]=x_x0;

Psi[0]= -y_y1;
Psi[1]=y_y0;

X=x[0]; Y=y[0];

for (i=0; i<2; ++i)
    {
    for (j=0; j<2; ++j) Pxy += *(*(image+Y)+X++)*Psi[i]*Phi[j];

    Y++; X= *x;
    }

return(Pxy);
}

/***********************/
 long DRound(double arg) 		// Rounds double value off to the nearest integer
/***********************/
{
	arg+=(arg<0)? -0.5 : 0.5;
	return((long)arg);
}

/*****************************************************************************************************/
 ERROR_NUMBER Complex2Float(ComplexPlane cpInputPlane, FloatPlane *fppRealOut, FloatPlane *fppImagOut)
/*****************************************************************************************************/
 {
	 int	iRow, iCol;
	 float	*fpRealRowPtr, *fpImaginaryRowPtr;
	 COMPLEX	*cpInputRowPtr;

	 // Make output planes
	 *fppRealOut=MakeFloatPlane(cpInputPlane.iRows, cpInputPlane.iColumns, NULL);
	 if (!(fppRealOut->Data)) return ERROR_MEMORY_ALLOCATION;
	 *fppImagOut=MakeFloatPlane(cpInputPlane.iRows, cpInputPlane.iColumns, NULL);
	 if (!(fppImagOut->Data))
	 {
		 FreeFloatPlane(*fppRealOut);
		 fppRealOut->Data=NULL;
		 fppImagOut->Data=NULL;
		 return ERROR_MEMORY_ALLOCATION;
	 }

	 // Fill output planes with data from input plane
	 for (iRow=0; iRow<cpInputPlane.iRows; ++iRow)
	 {
		 fpRealRowPtr=fppRealOut->Data[iRow];
		 fpImaginaryRowPtr=fppImagOut->Data[iRow];
		 cpInputRowPtr=cpInputPlane.Data[iRow];
		 for (iCol=0; iCol<cpInputPlane.iColumns; ++iCol)
		 {
			 fpRealRowPtr[iCol]=cpInputRowPtr[iCol].real;
			 fpImaginaryRowPtr[iCol]=cpInputRowPtr[iCol].imag;
		 }
	 }

	 return ERROR_NONE;
}

/******************************************************************/
 FloatPlane MakeFloatPlane(long iRows, long iColumns, float **Data)
/******************************************************************/
{
	FloatPlane	buffer;
	size_t	stBytesPerRow=iColumns * sizeof(float);

	buffer.iRows=iRows;
	buffer.iColumns=iColumns;
	buffer.Data=Data;

	if ((Data == NULL))
	{
		if (buffer.Data = (float **)malloc(buffer.iRows * sizeof(float *)))
		{
			for (long i=0; i<buffer.iRows; ++i)
				if ((buffer.Data[i]=(float *)calloc(1, stBytesPerRow)) == NULL)
				{
					for (--i; i>=0; --i) free(buffer.Data[i]);
					free(buffer.Data);
					buffer.Data = NULL;
					return(buffer);
				}
		}
	}

	return(buffer);
}

/**************************************/
 char * RootName(char * csFullPathName)
/**************************************/
{
   int iPeriodIndex, iBackslashIndex;
   char * FileNameNoExt, * buffer;
   int iFullLength=(int)strlen(csFullPathName);

   if ((FileNameNoExt=(char *)malloc(iFullLength+1))==NULL) return NULL;
   if ((buffer=(char *)malloc(iFullLength+1))==NULL)
   {
	   free(FileNameNoExt);
	   return NULL;
   }
   sprintf(FileNameNoExt, "%s", csFullPathName);

   if ((iPeriodIndex = (int)(strrchr( csFullPathName, '.' ) - csFullPathName))  > 0)
	   FileNameNoExt[iPeriodIndex] = '\0';

#if UNIX
   if ((iBackslashIndex = strrchr( FileNameNoExt, '/' ) - FileNameNoExt)  > 0)
#else
   if ((iBackslashIndex = (int)(strrchr( FileNameNoExt, '\\' ) - FileNameNoExt))  > 0)
#endif
	   sprintf(buffer, "%s", &(FileNameNoExt[iBackslashIndex+1]));
   else
      sprintf(buffer, "%s", FileNameNoExt);

   free(FileNameNoExt);

   return (buffer);
}


/*******************************************************************/
 ERROR_NUMBER GetDirectory(char *csInputFileName, char *csDirectory)
/*******************************************************************/
 {
	char *csCurrentDirectory;
	long iBackslashIndex;
#if UNIX
	csCurrentDirectory;
	iBackslashIndex = (long)(strrchr( csInputFileName, '/' ));
#else
	char csCurrentDirectory[150];
	longlong iBackslashIndex = (longlong)(strrchr( csInputFileName, '\\' ));
#endif

	if (iBackslashIndex == 0)
	{
#if UNIX
		if ((csCurrentDirectory = getcwd(NULL, 150)) == NULL)
		{
			perror("pwd");
			return ERROR_GETTING_DIRECTORY;
		}
#else
		GetCurrentDirectory( 150, (LPSTR)csCurrentDirectory );
#endif
		sprintf(csDirectory, "%s", csCurrentDirectory);
	}
	else
	{
#if UNIX
		iBackslashIndex -= (long)csInputFileName;
#else
		iBackslashIndex -= (longlong)csInputFileName;
#endif
		sprintf(csDirectory, "%s", csInputFileName);
		csDirectory[iBackslashIndex] = '\0';
	}

	return ERROR_NONE;
 }
