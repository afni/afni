/** \file fslio.h
    \brief Data structures for using the fslio API.  Written by Mark Jenkinson, FMRIB.
 */

/*
    fslio.h  (Input and output routines for images in FSL)

    Mark Jenkinson
    FMRIB Image Analysis Group

    Copyright (C) 2004 University of Oxford  */

/*  Part of FSL - FMRIB's Software Library
    http://www.fmrib.ox.ac.uk/fsl
    fsl@fmrib.ox.ac.uk
    
    Developed at FMRIB (Oxford Centre for Functional Magnetic Resonance
    Imaging of the Brain), Department of Clinical Neurology, Oxford
    University, Oxford, UK
    
    
    LICENCE
    
    FMRIB Software Library, Release 3.2beta (c) 2004, The University of
    Oxford (the "Software")
    
    The Software remains the property of the University of Oxford ("the
    University").
    
    The Software is distributed "AS IS" under this Licence solely for
    non-commercial use in the hope that it will be useful, but in order
    that the University as a charitable foundation protects its assets for
    the benefit of its educational and research purposes, the University
    makes clear that no condition is made or to be implied, nor is any
    warranty given or to be implied, as to the accuracy of the Software,
    or that it will be suitable for any particular purpose or for use
    under any specific conditions. Furthermore, the University disclaims
    all responsibility for the use which is made of the Software. It
    further disclaims any liability for the outcomes arising from using
    the Software.
    
    The Licensee agrees to indemnify the University and hold the
    University harmless from and against any and all claims, damages and
    liabilities asserted by third parties (including claims for
    negligence) which arise directly or indirectly from the use of the
    Software or the sale of any products based on the Software.
    
    No part of the Software may be reproduced, modified, transmitted or
    transferred in any form or by any means, electronic or mechanical,
    without the express permission of the University. The permission of
    the University is not required if the said reproduction, modification,
    transmission or transference is done without financial return, the
    conditions of this Licence are imposed upon the receiver of the
    product, and all original and amended source code is included in any
    transmitted product. You may be held legally responsible for any
    copyright infringement that is caused or encouraged by your failure to
    abide by these terms and conditions.
    
    You are not permitted under this Licence to use this Software
    commercially. Use for which any financial return is received shall be
    defined as commercial use, and includes (1) integration of all or part
    of the source code or the Software into a product for sale or license
    by or on behalf of Licensee to third parties or (2) use of the
    Software or any derivative of it for research with the final aim of
    developing software products for sale or license to a third party or
    (3) use of the Software or any derivative of it for research with the
    final aim of developing non-software products for sale or license to a
    third party, or (4) use of the Software to provide any service to an
    external organisation for which payment is received. If you are
    interested in using the Software commercially, please contact Isis
    Innovation Limited ("Isis"), the technology transfer company of the
    University, to negotiate a licence. Contact details are:
    innovation@isis.ox.ac.uk quoting reference DE/1112. */

#if !defined(__FSLIO_H)
#define __FSLIO_H

#include <stdio.h>
#include <nifti1_io.h>
#include <znzlib.h>
#include "dbh.h"

#ifdef __cplusplus
extern "C" {
#endif


  /*
    Note that this library is similar to avwio but has changed in many ways.
    It is almost fully backwards compatible, but not quite, as it cannot write
    .nii.gz files using the old style functions.

    Recommended ways of reading and writing images are:

    Reading
    -------
      Use the FslOpen(), FslReadVolumes() and FslClose() functions.  e.g.
        FSLIO *fslio;
	void *buffer;
	int nvols;
	fslio = FslOpen("/some/path/name_of_file","rb");
	  ... can now access header info via the FslGet calls ...
	  ... allocate room for buffer ...
	FslReadVolumes(fslio,buffer,nvols);
	  ... do something ...
	FslClose(fslio);


    Writing
    -------
      This is more complicated due to the nature of gzipped writing, which must be
      done in the correct order, and for single files (*.nii.gz) this means that 
      the header information must be written before any image data.

    (1)
      The best method to use is almost backwards compatible, but requires 
      an FslWriteHeader() call:

        FSLIO* fslio;
	fslio = FslOpen("/some/path/name_of_file","wb");
	  ... set the appropriate header information using FslSet calls ...
	FslWriteHeader(fslio);
	
	  ... now can write one or more volumes at a time using 
	      FslWriteVolumes(fslio,buffer,nvols) ...

	FslClose(fslio); 

      This version is useful if your image data needs to be written from different blocks
      of memory.

    (2)
      Another method is available, but which is discouraged, is:
        FSLIO* fslio;
	fslio = FslOpen("/some/path/name_of_file","wb");
	
	  ... set some appropriate header information using FslSet calls ...
	  ... now can write one or more volumes at a time using 
	      FslWriteVolumes(fslio,buffer,nvols) ...
	  ... set more appropriate header information using FslSet calls ...

	FslClose(fslio);

      WARNING: this cannot write .nii.gz files as the header information cannot be
      written by FslClose() after the image data is written, which is how the previous
      versions have worked.
	

   */

/*! \defgroup FSL_TYPE
    \brief FSL data format type codes
    @{
 */
#define FSL_TYPE_ANALYZE         0
#define FSL_TYPE_NIFTI           1
#define FSL_TYPE_NIFTI_PAIR      2
#define FSL_TYPE_MINC            4
#define FSL_TYPE_ANALYZE_GZ    100
#define FSL_TYPE_NIFTI_GZ      101
#define FSL_TYPE_NIFTI_PAIR_GZ 102
#define FSL_TYPE_MINC_GZ       104
/* @} */

#define FSL_RADIOLOGICAL        -1
#define FSL_NEUROLOGICAL         1


/*! \struct FSLIO
    \brief High level data structure for open datasets in the fslio API.
    \sa nifti_image
    \sa minc_image
 */
typedef struct 
{
  znzFile fileptr;
  nifti_image *niftiptr;
#ifdef USE_MINC
  minc_image *mincptr;
#else
  void *mincptr;
#endif
  int file_mode;
  int write_mode;
  int written_hdr;
} FSLIO;


  /* basic file i/o commands */

FSLIO *FslOpen(const char *filename, const char *opts);
FSLIO *FslXOpen(const char *filename, const char *opts, int filetype);
int FslSeekVolume(FSLIO *fslio, size_t vols);
int FslClose(FSLIO *fslio);

  /* basic read and write commands */

void* FslReadAllVolumes(FSLIO* fslio, char* filename);
void  FslWriteAllVolumes(FSLIO *fslio, const void *buffer);

size_t FslReadVolumes(FSLIO *fslio, void *buffer, size_t nvols);
size_t FslWriteVolumes(FSLIO *fslio, const void *buffer, size_t nvols);

void   FslWriteHeader(FSLIO *fslio);

  /* support functions for file names and types */

int   FslFileExists(const char *filename);
char *FslMakeBaseName(const char *fname);
int   FslCheckForMultipleFileNames(const char* filename);
int   FslGetEnvOutputType();

void  FslSetIgnoreMFQ(int flag);
int   FslGetIgnoreMFQ();
void  FslSetOverrideOutputType(int type);
int   FslGetOverrideOutputType();


int  FslGetFileType(const FSLIO *fslio);
void FslSetFileType(FSLIO *fslio, int filetype);
int  FslIsSingleFileType(int filetype);
int  FslIsCompressedFileType(int filetype);
int  FslBaseFileType(int filetype);
char* FslFileTypeString(int filetype);

int  FslGetWriteMode(const FSLIO *fslio);
void FslSetWriteMode(FSLIO *fslio, int mode);

void AvwSwapHeader(struct dsr *avw);
int  FslReadRawHeader(void *buffer, const char* filename);


  /* simple creation and clone/copy operations */

FSLIO *FslInit();
void   FslInitHeader(FSLIO *fslio, short t, 
		   size_t x, size_t y, size_t z, size_t v,
		   float vx, float vy, float vz, float tr,
		   size_t dim,
		   const char* units);
void   FslSetInit(FSLIO* fslio);
void   FslCloneHeader(FSLIO *dest, const FSLIO *src);


  /* get and set routines for properties */

size_t FslGetVolSize(FSLIO *fslio);

void FslSetDim(FSLIO *fslio, short x, short y, short z, short v);
void FslGetDim(FSLIO *fslio, short *x, short *y, short *z, short *v);
void FslSetDimensionality(FSLIO *fslio, size_t dim);
void FslGetDimensionality(FSLIO *fslio, size_t *dim);
void FslSetVoxDim(FSLIO *fslio, float x, float y, float z, float tr);
void FslGetVoxDim(FSLIO *fslio, float *x, float *y, float *z, float *tr);
void FslGetCalMinMax(FSLIO *fslio, float *min, float *max);
void FslSetCalMinMax(FSLIO *fslio, float  min, float  max);
void FslGetAuxFile(FSLIO *fslio,char *aux_file);
void FslSetAuxFile(FSLIO *fslio,const char *aux_file);
void FslSetTimeUnits(FSLIO *fslio, const char *units);
void FslGetTimeUnits(FSLIO *fslio, char *units);
void FslSetDataType(FSLIO *fslio, short t);
size_t FslGetDataType(FSLIO *fslio, short *t);
int    FslGetIntensityScaling(FSLIO *fslio, float *slope, float *intercept);
void   FslSetIntent(FSLIO *fslio, short intent_code, float p1, float p2, float p3);
short  FslGetIntent(FSLIO *fslio, short *intent_code, float *p1, float *p2,
		    float *p3);


short FslGetStdXform(FSLIO *fslio, mat44 *stdmat);
void  FslSetStdXform(FSLIO *fslio, short sform_code, mat44 stdmat);
void  FslGetMMCoord(mat44 stdmat, float voxx, float voxy, float voxz, 
		    float *mmx, float *mmy, float *mmz);

void  FslGetVoxCoord(mat44 stdmat, float mmx, float mmy, float mmz, 
		     float *voxx, float *voxy, float *voxz); 
short FslGetRigidXform(FSLIO *fslio, mat44 *rigidmat);
void  FslSetRigidXform(FSLIO *fslio, short qform_code, mat44 rigidmat);
int   FslGetLeftRightOrder(FSLIO *fslio);

  /* these two functions are deprecated with the nifti/analyze support */
  /* please do all spatial coordinate origins via the Std and Rigid Xforms */
void  FslSetAnalyzeSform(FSLIO *fslio, const short *orig, 
			 float dx, float dy, float dz);
void  FslGetAnalyzeOrigin(FSLIO *fslio, short orig[5]);

  /* other read and write commands */

size_t FslReadSliceSeries(FSLIO *fslio, void *buffer,short slice, size_t nvols);
size_t FslReadRowSeries(FSLIO *fslio, void *buffer, short row, short slice, size_t nvols);
size_t FslReadTimeSeries(FSLIO *fslio, void *buffer, short xVox, short yVox, short zVox, size_t nvols);

  /* miscellaneous helper stuff */

mat33 mat44_to_mat33(mat44 x);


#ifdef __cplusplus
}
#endif

#endif


/* added by KF pending discussion w/ Mark */
typedef unsigned char	THIS_UINT8; 
typedef char 		THIS_INT8;
typedef unsigned short	THIS_UINT16;
typedef short		THIS_INT16;
typedef unsigned int	THIS_UINT32;
typedef int		THIS_INT32;
typedef unsigned long 	THIS_UINT64;
typedef long 		THIS_INT64;
typedef float		THIS_FLOAT32;
typedef double 		THIS_FLOAT64;

double ****d4matrix(int th, int zh,  int yh, int xh);
double ****FslGetBufferAsScaledDouble(FSLIO *fslio);
