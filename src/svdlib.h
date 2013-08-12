/*
Copyright © 2002, University of Tennessee Research Foundation.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

  Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of the University of Tennessee nor the names of its
  contributors may be used to endorse or promote products derived from this
  software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef SVDLIB_H
#define SVDLIB_H

#ifndef FALSE
#  define FALSE 0
#endif
#ifndef TRUE
#  define TRUE  1
#endif

/******************************** Structures *********************************/
typedef struct smat *SMat;
typedef struct dmat *DMat;
typedef struct svdrec *SVDRec;

/* Harwell-Boeing sparse matrix. */
struct smat {
  long rows;
  long cols;
  long vals;     /* Total non-zero entries. */
  long *pointr;  /* For each col (plus 1), index of first non-zero entry. */
  long *rowind;  /* For each nz entry, the row index. */
  double *value; /* For each nz entry, the value. */
};

/* Row-major dense matrix.  Rows are consecutive vectors. */
struct dmat {
  long rows;
  long cols;
  double **value; /* Accessed by [row][col]. Free value[0] and value to free.*/
};

struct svdrec {
  int d;      /* Dimensionality (rank) */
  DMat Ut;    /* Transpose of left singular vectors. (d by m)
                 The vectors are the rows of Ut. */
  double *S;  /* Array of singular values. (length d) */
  DMat Vt;    /* Transpose of right singular vectors. (d by n)
                 The vectors are the rows of Vt. */
};


/******************************** Variables **********************************/

/* Version info */
extern char *SVDVersion;

/* How verbose is the package: 0, 1 (default), 2 */
extern long SVDVerbosity;

/* Counter(s) used to track how much work is done in computing the SVD. */
enum svdCounters {SVD_MXV, SVD_COUNTERS};
extern long SVDCount[SVD_COUNTERS];
extern void svdResetCounters(void);

enum svdFileFormats {SVD_F_STH, SVD_F_ST, SVD_F_SB, SVD_F_DT, SVD_F_DB};
/*
File formats:
SVD_F_STH: sparse text, SVDPACK-style
SVD_F_ST:  sparse text, SVDLIB-style
SVD_F_DT:  dense text
SVD_F_SB:  sparse binary
SVD_F_DB:  dense binary
*/

/* True if a file format is sparse: */
#define SVD_IS_SPARSE(format) ((format >= SVD_F_STH) && (format <= SVD_F_SB))


/******************************** Functions **********************************/

/* Creates an empty dense matrix. */
extern DMat svdNewDMat(int rows, int cols);
/* Frees a dense matrix. */
extern void svdFreeDMat(DMat D);

/* Creates an empty sparse matrix. */
SMat svdNewSMat(int rows, int cols, int vals);
/* Frees a sparse matrix. */
void svdFreeSMat(SMat S);

/* Creates an empty SVD record. */
SVDRec svdNewSVDRec(void);
/* Frees an svd rec and all its contents. */
void svdFreeSVDRec(SVDRec R);

/* Converts a sparse matrix to a dense one (without affecting former) */
DMat svdConvertStoD(SMat S);
/* Converts a dense matrix to a sparse one (without affecting former) */
SMat svdConvertDtoS(DMat D);

/* Transposes a dense matrix (returning a new one) */
DMat svdTransposeD(DMat D);
/* Transposes a sparse matrix (returning a new one) */
SMat svdTransposeS(SMat S);

/* Writes an array to a file. */
extern void svdWriteDenseArray(double *a, int n, char *filename, char binary);
/* Reads an array from a file, storing its size in *np. */
extern double *svdLoadDenseArray(char *filename, int *np, char binary);

/* Loads a matrix file (in various formats) into a sparse matrix. */
extern SMat svdLoadSparseMatrix(char *filename, int format);
/* Loads a matrix file (in various formats) into a dense matrix. */
extern DMat svdLoadDenseMatrix(char *filename, int format);

/* Writes a dense matrix to a file in a given format. */
extern void svdWriteDenseMatrix(DMat A, char *filename, int format);
/* Writes a sparse matrix to a file in a given format. */
extern void svdWriteSparseMatrix(SMat A, char *filename, int format);


/* Performs the las2 SVD algorithm and returns the resulting Ut, S, and Vt. */
extern SVDRec svdLAS2(SMat A, long dimensions, long iterations, double end[2], 
                      double kappa);
/* Chooses default parameter values.  Set dimensions to 0 for all dimensions: */
extern SVDRec svdLAS2A(SMat A, long dimensions);

#endif /* SVDLIB_H */

#ifndef SVDUTIL_H
#define SVDUTIL_H

#define SAFE_FREE(a) {if (a) {free(a); a = NULL;}}

/* Allocates an array of longs. */
extern long *svd_longArray(long size, char empty, char *name);
/* Allocates an array of doubles. */
extern double *svd_doubleArray(long size, char empty, char *name);

extern void svd_debug(char *fmt, ...);
extern void svd_error(char *fmt, ...);
extern void svd_fatalError(char *fmt, ...);
extern FILE *svd_fatalReadFile(char *filename);
extern FILE *svd_readFile(char *fileName);
extern FILE *svd_writeFile(char *fileName, char append);
extern void svd_closeFile(FILE *file);

extern char svd_readBinInt(FILE *file, int *val);
extern char svd_readBinFloat(FILE *file, float *val);
extern char svd_writeBinInt(FILE *file, int x);
extern char svd_writeBinFloat(FILE *file, float r);

/************************************************************** 
 * returns |a| if b is positive; else fsign returns -|a|      *
 **************************************************************/ 
extern double svd_fsign(double a, double b);

/************************************************************** 
 * returns the larger of two double precision numbers         *
 **************************************************************/ 
extern double svd_dmax(double a, double b);

/************************************************************** 
 * returns the smaller of two double precision numbers        *
 **************************************************************/ 
extern double svd_dmin(double a, double b);

/************************************************************** 
 * returns the larger of two integers                         *
 **************************************************************/ 
extern long svd_imax(long a, long b);

/************************************************************** 
 * returns the smaller of two integers                        *
 **************************************************************/ 
extern long svd_imin(long a, long b);

/************************************************************** 
 * Function scales a vector by a constant.     		      *
 * Based on Fortran-77 routine from Linpack by J. Dongarra    *
 **************************************************************/ 
extern void svd_dscal(long n, double da, double *dx, long incx);

/************************************************************** 
 * function scales a vector by a constant.	     	      *
 * Based on Fortran-77 routine from Linpack by J. Dongarra    *
 **************************************************************/ 
extern void svd_datx(long n, double da, double *dx, long incx, double *dy, long incy);

/************************************************************** 
 * Function copies a vector x to a vector y	     	      *
 * Based on Fortran-77 routine from Linpack by J. Dongarra    *
 **************************************************************/ 
extern void svd_dcopy(long n, double *dx, long incx, double *dy, long incy);

/************************************************************** 
 * Function forms the dot product of two vectors.      	      *
 * Based on Fortran-77 routine from Linpack by J. Dongarra    *
 **************************************************************/ 
extern double svd_ddot(long n, double *dx, long incx, double *dy, long incy);

/************************************************************** 
 * Constant times a vector plus a vector     		      *
 * Based on Fortran-77 routine from Linpack by J. Dongarra    *
 **************************************************************/ 
extern void svd_daxpy (long n, double da, double *dx, long incx, double *dy, long incy);

/********************************************************************* 
 * Function sorts array1 and array2 into increasing order for array1 *
 *********************************************************************/
extern void svd_dsort2(long igap, long n, double *array1, double *array2);

/************************************************************** 
 * Function interchanges two vectors		     	      *
 * Based on Fortran-77 routine from Linpack by J. Dongarra    *
 **************************************************************/ 
extern void svd_dswap(long n, double *dx, long incx, double *dy, long incy);

/***************************************************************** 
 * Function finds the index of element having max. absolute value*
 * based on FORTRAN 77 routine from Linpack by J. Dongarra       *
 *****************************************************************/ 
extern long svd_idamax(long n, double *dx, long incx);

/**************************************************************
 * multiplication of matrix B by vector x, where B = A'A,     *
 * and A is nrow by ncol (nrow >> ncol). Hence, B is of order *
 * n = ncol (y stores product vector).		              *
 **************************************************************/
extern void svd_opb(SMat A, double *x, double *y, double *temp);

/***********************************************************
 * multiplication of matrix A by vector x, where A is 	   *
 * nrow by ncol (nrow >> ncol).  y stores product vector.  *
 ***********************************************************/
extern void svd_opa(SMat A, double *x, double *y);

/***********************************************************************
 *                                                                     *
 *				random2()                              *
 *                        (double precision)                           *
 ***********************************************************************/
extern double svd_random2(long *iy);

/************************************************************** 
 *							      *
 * Function finds sqrt(a^2 + b^2) without overflow or         *
 * destructive underflow.				      *
 *							      *
 **************************************************************/ 
extern double svd_pythag(double a, double b);

#endif /* SVDUTIL_H */
