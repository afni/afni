#include <stdbool.h>
#include <string.h>
#ifndef HAVE_R
#include "nifti1.h"
#endif

#ifndef MRIpro_nii_dcm_h

#define MRIpro_nii_dcm_h

#ifdef  __cplusplus
extern "C" {
#endif

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

 #ifdef myEnableJasper
  #define kDCMsuf " (JasPer build)"
 #else
  #ifdef myDisableOpenJPEG
    #define kDCMsuf ""
  #else
    #define kDCMsuf " (OpenJPEG build)"
  #endif
 #endif
#if defined(__ICC) || defined(__INTEL_COMPILER)
	#define kCCsuf  " IntelCC" STR(__INTEL_COMPILER)
#elif defined(_MSC_VER)
	#define kCCsuf  " MSC" STR(_MSC_VER)
#elif defined(__clang__)
	#define kCCsuf  " Clang" STR(__clang_major__) "." STR(__clang_minor__) "." STR(__clang_patchlevel__)
#elif defined(__GNUC__) || defined(__GNUG__)
    #define kCCsuf  " GCC" STR(__GNUC__) "." STR(__GNUC_MINOR__) "." STR(__GNUC_PATCHLEVEL__)
#else
	#define kCCsuf " CompilerNA" //unknown compiler!
#endif

 #define kDCMvers "v1.0.20170411" kDCMsuf kCCsuf

static const int kMaxDTI4D = 4096; //maximum number of DTI directions for 4D (Philips) images, also maximum number of 3D slices for Philips 3D and 4D images
#define kDICOMStr 64
#define kMANUFACTURER_UNKNOWN  0
#define kMANUFACTURER_SIEMENS  1
#define kMANUFACTURER_GE  2
#define kMANUFACTURER_PHILIPS  3
#define kMANUFACTURER_TOSHIBA  4
static const int kSliceOrientUnknown = 0;
static const int kSliceOrientTra = 1;
static const int kSliceOrientSag = 2;
static const int kSliceOrientCor = 3;
static const int kSliceOrientMosaicNegativeDeterminant = 4;
static const int kCompressNone = 0;
static const int kCompressYes = 1;
static const int kCompressC3 = 2; //obsolete JPEG lossless
static const int kCompress50 = 3; //obsolete JPEG lossy
    struct TDTI {
        float V[4];
        float sliceTiming;
        int sliceNumberMrPhilips;
    };
    struct TDTI4D {
        struct TDTI S[kMaxDTI4D];
    };

    struct TCSAdata {
    	bool isPhaseMap;
        float dtiV[4], sliceNormV[4], bandwidthPerPixelPhaseEncode, sliceMeasurementDuration;
        int numDti, multiBandFactor, sliceOrder, slice_start, slice_end, mosaicSlices,protocolSliceNumber1,phaseEncodingDirectionPositive;
    };
    struct TDICOMdata {
        long seriesNum;
        int xyzDim[5];
        int patientPositionNumPhilips, coilNum, echoNum, sliceOrient,numberOfDynamicScans, manufacturer, converted2NII, acquNum, imageNum, imageStart, imageBytes, bitsStored, bitsAllocated, samplesPerPixel,patientPositionSequentialRepeats,locationsInAcquisition, compressionScheme;
        float flipAngle, fieldStrength, TE, TI, TR, intenScale, intenIntercept, intenScalePhilips, gantryTilt, lastScanLoc, angulation[4];
        float orient[7], patientPosition[4], patientPositionLast[4], xyzMM[4], stackOffcentre[4];
        float radionuclidePositronFraction, radionuclideTotalDose, radionuclideHalfLife, doseCalibrationFactor; //PET ISOTOPE MODULE ATTRIBUTES (C.8-57)

        double dateTime, acquisitionTime, acquisitionDate;
        bool isXRay, isSlicesSpatiallySequentialPhilips, isNonImage, isValid, is3DAcq, isExplicitVR, isLittleEndian, isPlanarRGB, isSigned, isHasPhase,isHasMagnitude,isHasMixed, isFloat, isResampled;
        char phaseEncodingRC;
        char seriesInstanceUID[kDICOMStr], studyInstanceUID[kDICOMStr], bodyPartExamined[kDICOMStr], procedureStepDescription[kDICOMStr], imageType[kDICOMStr], manufacturersModelName[kDICOMStr], patientID[kDICOMStr], patientOrient[kDICOMStr], patientName[kDICOMStr],seriesDescription[kDICOMStr], sequenceName[kDICOMStr], protocolName[kDICOMStr],sequenceVariant[kDICOMStr],scanningSequence[kDICOMStr], birthDate[kDICOMStr], gender[kDICOMStr], age[kDICOMStr],  studyDate[kDICOMStr],studyTime[kDICOMStr], imageComments[kDICOMStr];
        struct TCSAdata CSA;
    };

    size_t nii_ImgBytes(struct nifti_1_header hdr);
    struct TDICOMdata readDICOMv(char * fname, int isVerbose, int compressFlag, struct TDTI4D *dti4D);
    struct TDICOMdata readDICOM(char * fname);
    struct TDICOMdata clear_dicom_data();
    struct TDICOMdata  nii_readParRec (char * parname, int isVerbose, struct TDTI4D *dti4D);
    unsigned char * nii_flipY(unsigned char* bImg, struct nifti_1_header *h);
    unsigned char * nii_flipZ(unsigned char* bImg, struct nifti_1_header *h);
    unsigned char * nii_reorderSlices(unsigned char* bImg, struct nifti_1_header *h, struct TDTI4D *dti4D);
    void changeExt (char *file_name, const char* ext);
    unsigned char * nii_planar2rgb(unsigned char* bImg, struct nifti_1_header *hdr, int isPlanar);
	int isDICOMfile(const char * fname); //0=not DICOM, 1=DICOM, 2=NOTSURE(not part 10 compliant)
    int headerDcm2Nii2(struct TDICOMdata d, struct TDICOMdata d2, struct nifti_1_header *h);
    //unsigned char * nii_loadImgX(char* imgname, struct nifti_1_header *hdr, struct TDICOMdata dcm, bool iVaries);
    unsigned char * nii_loadImgXL(char* imgname, struct nifti_1_header *hdr, struct TDICOMdata dcm, bool iVaries, int compressFlag, int isVerbose);
    //int foo (float vx);
#ifdef  __cplusplus
}
#endif

#endif
