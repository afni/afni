#ifndef _I_ALREADY_HAVE_SVDLIB_
#define _I_ALREADY_HAVE_SVDLIB_
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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <fcntl.h>
#include "svdlib.h"

#define BUNZIP2  "bzip2 -d"
#define BZIP2    "bzip2 -1"
#define UNZIP    "gzip -d"
#define ZIP      "gzip -1"
#define COMPRESS "compress"

#define MAX_FILENAME 512
#define MAX_PIPES    64
static FILE *Pipe[MAX_PIPES];
static int numPipes = 0;

long *svd_longArray(long size, char empty, char *name) {
  long *a;
  if (empty) a = (long *) calloc(size, sizeof(long));
  else a = (long *) malloc(size * sizeof(long));
  if (!a) {
    perror(name);
    /* exit(errno); */
  }
  return a;
}

double *svd_doubleArray(long size, char empty, char *name) {
  double *a;
  if (empty) a = (double *) calloc(size, sizeof(double));
  else a = (double *) malloc(size * sizeof(double));
  if (!a) {
    perror(name);
    /* exit(errno); */
  }
  return a;
}

void svd_beep(void) {
  fputc('\a', stderr);
  fflush(stderr);
}

void svd_debug(char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
}

void svd_error(char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  svd_beep();
  fprintf(stderr, "ERROR: ");
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  va_end(args);
}

void svd_fatalError(char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  svd_beep();
  fprintf(stderr, "ERROR: ");
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\a\n");
  va_end(args);
  exit(1);
}

static void registerPipe(FILE *p) {
  if (numPipes >= MAX_PIPES) svd_error("Too many pipes open");
  Pipe[numPipes++] = p;
}

static char isPipe(FILE *p) {
  int i;
  for (i = 0; i < numPipes && Pipe[i] != p; i++);
  if (i == numPipes) return FALSE;
  Pipe[i] = Pipe[--numPipes];
  return TRUE;
}

static FILE *openPipe(char *pipeName, char *mode) {
  FILE *pipe;
  fflush(stdout);
  if ((pipe = popen(pipeName, mode))) registerPipe(pipe);
  return pipe;
}

static FILE *readZippedFile(char *command, char *fileName) {
  char buf[MAX_FILENAME];
  sprintf(buf, "%s < %s 2>/dev/null", command, fileName);
  return openPipe(buf, "r");
}

FILE *svd_fatalReadFile(char *filename) {
  FILE *file;
  if (!(file = svd_readFile(filename)))
    svd_fatalError("couldn't read the file %s", filename);
  return file;
}

static int stringEndsIn(char *s, char *t) {
  int ls = strlen(s);
  int lt = strlen(t);
  if (ls < lt) return FALSE;
  return (strcmp(s + ls - lt, t)) ? FALSE : TRUE;
}

/* Will silently return NULL if file couldn't be opened */
FILE *svd_readFile(char *fileName) {
  char fileBuf[MAX_FILENAME];
  struct stat statbuf;

  /* Special file name */
  if (!strcmp(fileName, "-"))
    return stdin;

  /* If it is a pipe */
  if (fileName[0] == '|')
    return openPipe(fileName + 1, "r");

  /* Check if already ends in .gz or .Z and assume compressed */
  if (stringEndsIn(fileName, ".gz") || stringEndsIn(fileName, ".Z")) {
    if (!stat(fileName, &statbuf))
      return readZippedFile(UNZIP, fileName);
    return NULL;
  }
  /* Check if already ends in .bz or .bz2 and assume compressed */
  if (stringEndsIn(fileName, ".bz") || stringEndsIn(fileName, ".bz2")) {
    if (!stat(fileName, &statbuf))
      return readZippedFile(BUNZIP2, fileName);
    return NULL;
  }
  /* Try just opening normally */
  if (!stat(fileName, &statbuf))
    return fopen(fileName, "r");
  /* Try adding .gz */
  sprintf(fileBuf, "%s.gz", fileName);
  if (!stat(fileBuf, &statbuf))
    return readZippedFile(UNZIP, fileBuf);
  /* Try adding .Z */
  sprintf(fileBuf, "%s.Z", fileName);
  if (!stat(fileBuf, &statbuf))
    return readZippedFile(UNZIP, fileBuf);
  /* Try adding .bz2 */
  sprintf(fileBuf, "%s.bz2", fileName);
  if (!stat(fileBuf, &statbuf))
    return readZippedFile(BUNZIP2, fileBuf);
  /* Try adding .bz */
  sprintf(fileBuf, "%s.bz", fileName);
  if (!stat(fileBuf, &statbuf))
    return readZippedFile(BUNZIP2, fileBuf);

  return NULL;
}

static FILE *writeZippedFile(char *fileName, char append) {
  char buf[MAX_FILENAME];
  const char *op = (append) ? ">>" : ">";
  if (stringEndsIn(fileName, ".bz2") || stringEndsIn(fileName, ".bz"))
    sprintf(buf, "%s %s \"%s\"", BZIP2, op, fileName);
  else if (stringEndsIn(fileName, ".Z"))
    sprintf(buf, "%s %s \"%s\"", COMPRESS, op, fileName);
  else
    sprintf(buf, "%s %s \"%s\"", ZIP, op, fileName);
  return openPipe(buf, "w");
}

FILE *svd_writeFile(char *fileName, char append) {
  /* Special file name */
  if (!strcmp(fileName, "-"))
    return stdout;

  /* If it is a pipe */
  if (fileName[0] == '|')
    return openPipe(fileName + 1, "w");

  /* Check if ends in .gz, .Z, .bz, .bz2 */
  if (stringEndsIn(fileName, ".gz") || stringEndsIn(fileName, ".Z") ||
      stringEndsIn(fileName, ".bz") || stringEndsIn(fileName, ".bz2"))
    return writeZippedFile(fileName, append);
  return (append) ? fopen(fileName, "a") : fopen(fileName, "w");
}

/* Could be a file or a stream. */
void svd_closeFile(FILE *file) {
  if (file == stdin || file == stdout) return;
  if (isPipe(file)) pclose(file);
  else fclose(file);
}


char svd_readBinInt(FILE *file, int *val) {
  int x;
  if (fread(&x, sizeof(int), 1, file) == 1) {
    *val = ntohl(x);
    return FALSE;
  }
  return TRUE;
}

/* This reads a float in network order and converts to a real in host order. */
char svd_readBinFloat(FILE *file, float *val) {
  int x;
  float y;
  if (fread(&x, sizeof(int), 1, file) == 1) {
    x = ntohl(x);
    y = *((float *) &x);
    *val = y;
    return FALSE;
  }
  return TRUE;
}

char svd_writeBinInt(FILE *file, int x) {
  int y = htonl(x);
  if (fwrite(&y, sizeof(int), 1, file) != 1) return TRUE;
  return FALSE;
}

/* This takes a real in host order and writes a float in network order. */
char svd_writeBinFloat(FILE *file, float r) {
  int y = htonl(*((int *) &r));
  if (fwrite(&y, sizeof(int), 1, file) != 1) return TRUE;
  return FALSE;
}


/**************************************************************
 * returns |a| if b is positive; else fsign returns -|a|      *
 **************************************************************/
double svd_fsign(double a, double b) {
  if ((a>=0.0 && b>=0.0) || (a<0.0 && b<0.0))return(a);
  else return -a;
}

/**************************************************************
 * returns the larger of two double precision numbers         *
 **************************************************************/
double svd_dmax(double a, double b) {
   return (a > b) ? a : b;
}

/**************************************************************
 * returns the smaller of two double precision numbers        *
 **************************************************************/
double svd_dmin(double a, double b) {
  return (a < b) ? a : b;
}

/**************************************************************
 * returns the larger of two integers                         *
 **************************************************************/
long svd_imax(long a, long b) {
  return (a > b) ? a : b;
}

/**************************************************************
 * returns the smaller of two integers                        *
 **************************************************************/
long svd_imin(long a, long b) {
  return (a < b) ? a : b;
}

/**************************************************************
 * Function scales a vector by a constant.     		      *
 * Based on Fortran-77 routine from Linpack by J. Dongarra    *
 **************************************************************/
void svd_dscal(long n, double da, double *dx, long incx) {
  long i;

  if (n <= 0 || incx == 0) return;
  if (incx < 0) dx += (-n+1) * incx;
  for (i=0; i < n; i++) {
    *dx *= da;
    dx += incx;
  }
  return;
}

/**************************************************************
 * function scales a vector by a constant.	     	      *
 * Based on Fortran-77 routine from Linpack by J. Dongarra    *
 **************************************************************/
void svd_datx(long n, double da, double *dx, long incx, double *dy, long incy) {
  long i;

  if (n <= 0 || incx == 0 || incy == 0 || da == 0.0) return;
  if (incx == 1 && incy == 1)
    for (i=0; i < n; i++) *dy++ = da * (*dx++);

  else {
    if (incx < 0) dx += (-n+1) * incx;
    if (incy < 0) dy += (-n+1) * incy;
    for (i=0; i < n; i++) {
      *dy = da * (*dx);
      dx += incx;
      dy += incy;
    }
  }
  return;
}

/**************************************************************
 * Function copies a vector x to a vector y	     	      *
 * Based on Fortran-77 routine from Linpack by J. Dongarra    *
 **************************************************************/
void svd_dcopy(long n, double *dx, long incx, double *dy, long incy) {
  long i;

  if (n <= 0 || incx == 0 || incy == 0) return;
  if (incx == 1 && incy == 1)
    for (i=0; i < n; i++) *dy++ = *dx++;

  else {
    if (incx < 0) dx += (-n+1) * incx;
    if (incy < 0) dy += (-n+1) * incy;
    for (i=0; i < n; i++) {
      *dy = *dx;
      dx += incx;
      dy += incy;
    }
  }
  return;
}

/**************************************************************
 * Function forms the dot product of two vectors.      	      *
 * Based on Fortran-77 routine from Linpack by J. Dongarra    *
 **************************************************************/
double svd_ddot(long n, double *dx, long incx, double *dy, long incy) {
  long i;
  double dot_product;

  if (n <= 0 || incx == 0 || incy == 0) return(0.0);
  dot_product = 0.0;
  if (incx == 1 && incy == 1)
    for (i=0; i < n; i++) dot_product += (*dx++) * (*dy++);
  else {
    if (incx < 0) dx += (-n+1) * incx;
    if (incy < 0) dy += (-n+1) * incy;
    for (i=0; i < n; i++) {
      dot_product += (*dx) * (*dy);
      dx += incx;
      dy += incy;
      }
  }
  return(dot_product);
}

/**************************************************************
 * Constant times a vector plus a vector     		      *
 * Based on Fortran-77 routine from Linpack by J. Dongarra    *
 **************************************************************/
void svd_daxpy (long n, double da, double *dx, long incx, double *dy, long incy) {
  long i;

  if (n <= 0 || incx == 0 || incy == 0 || da == 0.0) return;
  if (incx == 1 && incy == 1)
    for (i=0; i < n; i++) {
      *dy += da * (*dx++);
      dy++;
    }
  else {
    if (incx < 0) dx += (-n+1) * incx;
    if (incy < 0) dy += (-n+1) * incy;
    for (i=0; i < n; i++) {
      *dy += da * (*dx);
      dx += incx;
      dy += incy;
    }
  }
  return;
}

/*********************************************************************
 * Function sorts array1 and array2 into increasing order for array1 *
 *********************************************************************/
void svd_dsort2(long igap, long n, double *array1, double *array2) {
  double temp;
  long i, j, index;

  if (!igap) return;
  else {
    for (i = igap; i < n; i++) {
      j = i - igap;
      index = i;
      while (j >= 0 && array1[j] > array1[index]) {
        temp = array1[j];
        array1[j] = array1[index];
        array1[index] = temp;
        temp = array2[j];
        array2[j] = array2[index];
        array2[index] = temp;
        j -= igap;
        index = j + igap;
      }
    }
  }
  svd_dsort2(igap/2,n,array1,array2);
}

/**************************************************************
 * Function interchanges two vectors		     	      *
 * Based on Fortran-77 routine from Linpack by J. Dongarra    *
 **************************************************************/
void svd_dswap(long n, double *dx, long incx, double *dy, long incy) {
  long i;
  double dtemp;

  if (n <= 0 || incx == 0 || incy == 0) return;
  if (incx == 1 && incy == 1) {
    for (i=0; i < n; i++) {
      dtemp = *dy;
      *dy++ = *dx;
      *dx++ = dtemp;
    }	
  }
  else {
    if (incx < 0) dx += (-n+1) * incx;
    if (incy < 0) dy += (-n+1) * incy;
    for (i=0; i < n; i++) {
      dtemp = *dy;
      *dy = *dx;
      *dx = dtemp;
      dx += incx;
      dy += incy;
    }
  }
}

/*****************************************************************
 * Function finds the index of element having max. absolute value*
 * based on FORTRAN 77 routine from Linpack by J. Dongarra       *
 *****************************************************************/
long svd_idamax(long n, double *dx, long incx) {
  long ix,i,imax;
  double dtemp, dmax;

  if (n < 1) return(-1);
  if (n == 1) return(0);
  if (incx == 0) return(-1);

  if (incx < 0) ix = (-n+1) * incx;
  else ix = 0;
  imax = ix;
  dx += ix;
  dmax = fabs(*dx);
  for (i=1; i < n; i++) {
    ix += incx;
    dx += incx;
    dtemp = fabs(*dx);
    if (dtemp > dmax) {
      dmax = dtemp;
      imax = ix;
    }
  }
  return(imax);
}

/**************************************************************
 * multiplication of matrix B by vector x, where B = A'A,     *
 * and A is nrow by ncol (nrow >> ncol). Hence, B is of order *
 * n = ncol (y stores product vector).		              *
 **************************************************************/
void svd_opb(SMat A, double *x, double *y, double *temp) {
  long i, j, end;
  long *pointr = A->pointr, *rowind = A->rowind;
  double *value = A->value;
  long n = A->cols;

#ifndef USE_OMP
  SVDCount[SVD_MXV] += 2;
#endif
  memset(y, 0, n * sizeof(double));
  for (i = 0; i < A->rows; i++) temp[i] = 0.0;

  for (i = 0; i < A->cols; i++) {
    end = pointr[i+1];
    for (j = pointr[i]; j < end; j++)
      temp[rowind[j]] += value[j] * (*x);
    x++;
  }
  for (i = 0; i < A->cols; i++) {
    end = pointr[i+1];
    for (j = pointr[i]; j < end; j++)
      *y += value[j] * temp[rowind[j]];
    y++;
  }
  return;
}

/***********************************************************
 * multiplication of matrix A by vector x, where A is 	   *
 * nrow by ncol (nrow >> ncol).  y stores product vector.  *
 ***********************************************************/
void svd_opa(SMat A, double *x, double *y) {
  long end, i, j;
  long *pointr = A->pointr, *rowind = A->rowind;
  double *value = A->value;

#ifndef USE_OMP
  SVDCount[SVD_MXV]++;
#endif
  memset(y, 0, A->rows * sizeof(double));

  for (i = 0; i < A->cols; i++) {
    end = pointr[i+1];
    for (j = pointr[i]; j < end; j++)
      y[rowind[j]] += value[j] * x[i];
  }
  return;
}


/***********************************************************************
 *                                                                     *
 *				random()                               *
 *                        (double precision)                           *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   This is a translation of a Fortran-77 uniform random number
   generator.  The code is based  on  theory and suggestions  given in
   D. E. Knuth (1969),  vol  2.  The argument to the function should
   be initialized to an arbitrary integer prior to the first call to
   random.  The calling program should  not  alter  the value of the
   argument between subsequent calls to random.  Random returns values
   within the interval (0,1).


   Arguments
   ---------

   (input)
   iy	   an integer seed whose value must not be altered by the caller
	   between subsequent calls

   (output)
   random  a double precision random number between (0,1)

 ***********************************************************************/
double svd_random2(long *iy) {
   static long m2 = 0;
   static long ia, ic, mic;
   static double halfm, s;

   /* If first entry, compute (max int) / 2 */
   { if (!m2) {
      m2 = 1 << (8 * (int)sizeof(int) - 2);
      halfm = m2;

      /* compute multiplier and increment for linear congruential method */
      ia = 8 * (long)(halfm * atan(1.0) / 8.0) + 5;
      ic = 2 * (long)(halfm * (0.5 - sqrt(3.0)/6.0)) + 1;
      mic = (m2-ic) + m2;

      /* s is the scale factor for converting to floating point */
      s = 0.5 / halfm;
   }}
   if( iy == NULL ) return 0.0 ;  /* RWCox */

   /* compute next random number */
   *iy = *iy * ia;

   /* for computers which do not allow integer overflow on addition */
   if (*iy > mic) *iy = (*iy - m2) - m2;

   *iy = *iy + ic;

   /* for computers whose word length for addition is greater than
    * for multiplication */
   if (*iy / 2 > m2) *iy = (*iy - m2) - m2;

   /* for computers whose integer overflow affects the sign bit */
   if (*iy < 0) *iy = (*iy + m2) + m2;

   return((double)(*iy) * s);
}

/**************************************************************
 *							      *
 * Function finds sqrt(a^2 + b^2) without overflow or         *
 * destructive underflow.				      *
 *							      *
 **************************************************************/
/**************************************************************

   Funtions used
   -------------

   UTILITY	dmax, dmin

 **************************************************************/
double svd_pythag(double a, double b) {
   double p, r, s, t, u, temp;

   p = svd_dmax(fabs(a), fabs(b));
   if (p != 0.0) {
      temp = svd_dmin(fabs(a), fabs(b)) / p;
      r = temp * temp;
      t = 4.0 + r;
      while (t != 4.0) {
	 s = r / t;
	 u = 1.0 + 2.0 * s;
	 p *= u;
	 temp = s / u;
	 r *= temp * temp;
	 t = 4.0 + r;
      }
   }
   return(p);
}

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

char *SVDVersion = "1.4";
long SVDVerbosity = 0;
long SVDCount[SVD_COUNTERS];

#ifndef USE_OMP
void svdResetCounters(void) {
  int i;
  for (i = 0; i < SVD_COUNTERS; i++)
    SVDCount[i] = 0;
}
#else
void svdResetCounters(void){ return; }
#endif

/********************************* Allocation ********************************/

/* Row major order.  Rows are vectors that are consecutive in memory.  Matrix
   is initialized to empty. */
DMat svdNewDMat(int rows, int cols) {
  int i;
  DMat D = (DMat) malloc(sizeof(struct dmat));
  if (!D) {perror("svdNewDMat"); return NULL;}
  D->rows = rows;
  D->cols = cols;

  D->value = (double **) malloc(rows * sizeof(double *));
  if (!D->value) {SAFE_FREE(D); return NULL;}

  D->value[0] = (double *) calloc(rows * cols, sizeof(double));
  if (!D->value[0]) {SAFE_FREE(D->value); SAFE_FREE(D); return NULL;}

  for (i = 1; i < rows; i++) D->value[i] = D->value[i-1] + cols;
  return D;
}

void svdFreeDMat(DMat D) {
  if (!D) return;
  SAFE_FREE(D->value[0]);
  SAFE_FREE(D->value);
  free(D);
}


SMat svdNewSMat(int rows, int cols, int vals) {
  SMat S = (SMat) calloc(1, sizeof(struct smat));
  if (!S) {perror("svdNewSMat"); return NULL;}
  S->rows = rows;
  S->cols = cols;
  S->vals = vals;
  S->pointr = svd_longArray(cols + 1, TRUE, "svdNewSMat: pointr");
  if (!S->pointr) {svdFreeSMat(S); return NULL;}
  S->rowind = svd_longArray(vals, FALSE, "svdNewSMat: rowind");
  if (!S->rowind) {svdFreeSMat(S); return NULL;}
  S->value  = svd_doubleArray(vals, FALSE, "svdNewSMat: value");
  if (!S->value)  {svdFreeSMat(S); return NULL;}
  return S;
}

void svdFreeSMat(SMat S) {
  if (!S) return;
  SAFE_FREE(S->pointr);
  SAFE_FREE(S->rowind);
  SAFE_FREE(S->value);
  free(S);
}


/* Creates an empty SVD record */
SVDRec svdNewSVDRec(void) {
  SVDRec R = (SVDRec) calloc(1, sizeof(struct svdrec));
  if (!R) {perror("svdNewSVDRec"); return NULL;}
  return R;
}

/* Frees an svd rec and all its contents. */
void svdFreeSVDRec(SVDRec R) {
  if (!R) return;
  if (R->Ut) svdFreeDMat(R->Ut);
  if (R->S) SAFE_FREE(R->S);
  if (R->Vt) svdFreeDMat(R->Vt);
  free(R);
}


/**************************** Conversion *************************************/

/* Converts a sparse matrix to a dense one (without affecting the former) */
DMat svdConvertStoD(SMat S) {
  int i, c;
  DMat D = svdNewDMat(S->rows, S->cols);
  if (!D) {
    svd_error("svdConvertStoD: failed to allocate D");
    return NULL;
  }
  for (i = 0, c = 0; i < S->vals; i++) {
    while (S->pointr[c + 1] <= i) c++;
    D->value[S->rowind[i]][c] = S->value[i];
  }
  return D;
}

/* Converts a dense matrix to a sparse one (without affecting the dense one) */
SMat svdConvertDtoS(DMat D) {
  SMat S;
  int i, j, n;
  for (i = 0, n = 0; i < D->rows; i++)
    for (j = 0; j < D->cols; j++)
      if (D->value[i][j] != 0) n++;

  S = svdNewSMat(D->rows, D->cols, n);
  if (!S) {
    svd_error("svdConvertDtoS: failed to allocate S");
    return NULL;
  }
  for (j = 0, n = 0; j < D->cols; j++) {
    S->pointr[j] = n;
    for (i = 0; i < D->rows; i++)
      if (D->value[i][j] != 0) {
        S->rowind[n] = i;
        S->value[n] = D->value[i][j];
        n++;
      }
  }
  S->pointr[S->cols] = S->vals;
  return S;
}

/* Transposes a dense matrix. */
DMat svdTransposeD(DMat D) {
  int r, c;
  DMat N = svdNewDMat(D->cols, D->rows);
  for (r = 0; r < D->rows; r++)
    for (c = 0; c < D->cols; c++)
      N->value[c][r] = D->value[r][c];
  return N;
}

/* Efficiently transposes a sparse matrix. */
SMat svdTransposeS(SMat S) {
  int r, c, i, j;
  SMat N = svdNewSMat(S->cols, S->rows, S->vals);
  /* Count number nz in each row. */
  for (i = 0; i < S->vals; i++)
    N->pointr[S->rowind[i]]++;
  /* Fill each cell with the starting point of the previous row. */
  N->pointr[S->rows] = S->vals - N->pointr[S->rows - 1];
  for (r = S->rows - 1; r > 0; r--)
    N->pointr[r] = N->pointr[r+1] - N->pointr[r-1];
  N->pointr[0] = 0;
  /* Assign the new columns and values. */
  for (c = 0, i = 0; c < S->cols; c++) {
    for (; i < S->pointr[c+1]; i++) {
      r = S->rowind[i];
      j = N->pointr[r+1]++;
      N->rowind[j] = c;
      N->value[j] = S->value[i];
    }
  }
  return N;
}


/**************************** Input/Output ***********************************/

void svdWriteDenseArray(double *a, int n, char *filename, char binary) {
  int i;
  FILE *file = svd_writeFile(filename, FALSE);
  if (!file)
    return svd_error("svdWriteDenseArray: failed to write %s", filename);
  if (binary) {
    svd_writeBinInt(file, n);
    for (i = 0; i < n; i++)
      svd_writeBinFloat(file, (float) a[i]);
  } else {
    fprintf(file, "%d\n", n);
    for (i = 0; i < n; i++)
      fprintf(file, "%g  ", a[i]);
    fprintf(file,"\n") ;
  }
  svd_closeFile(file);
}

double *svdLoadDenseArray(char *filename, int *np, char binary) {
  int i, n;
  double *a;

  FILE *file = svd_readFile(filename);
  if (!file) {
    svd_error("svdLoadDenseArray: failed to read %s", filename);
    return NULL;
  }
  if (binary) {
    svd_readBinInt(file, np);
  } else if (fscanf(file, " %d", np) != 1) {
    svd_error("svdLoadDenseArray: error reading %s", filename);
    svd_closeFile(file);
    return NULL;
  }
  n = *np;
  a = svd_doubleArray(n, FALSE, "svdLoadDenseArray: a");
  if (!a) return NULL;
  if (binary) {
    float f;
    for (i = 0; i < n; i++) {
      svd_readBinFloat(file, &f);
      a[i] = f;
    }
  } else {
    for (i = 0; i < n; i++) {
      if (fscanf(file, " %lf\n", a + i) != 1) {
	svd_error("svdLoadDenseArray: error reading %s", filename);
	break;
      }
    }
  }
  svd_closeFile(file);
  return a;
}


/* File format has a funny header, then first entry index per column, then the
   row for each entry, then the value for each entry.  Indices count from 1.
   Assumes A is initialized. */
static SMat svdLoadSparseTextHBFile(FILE *file) {
  char line[128];
  long i, x, rows, cols, vals, num_mat;
  SMat S;
  /* Skip the header line: */
  if (!fgets(line, 128, file));
  /* Skip the line giving the number of lines in this file: */
  if (!fgets(line, 128, file));
  /* Read the line with useful dimensions: */
  if (fscanf(file, "%*s%ld%ld%ld%ld\n",
             &rows, &cols, &vals, &num_mat) != 4) {
    svd_error("svdLoadSparseTextHBFile: bad file format on line 3");
    return NULL;
  }
  if (num_mat != 0) {
    svd_error("svdLoadSparseTextHBFile: I don't know how to handle a file "
              "with elemental matrices (last entry on header line 3)");
    return NULL;
  }
  /* Skip the line giving the formats: */
  if (!fgets(line, 128, file));

  S = svdNewSMat(rows, cols, vals);
  if (!S) return NULL;

  /* Read column pointers. */
  for (i = 0; i <= S->cols; i++) {
    if (fscanf(file, " %ld", &x) != 1) {
      svd_error("svdLoadSparseTextHBFile: error reading pointr %d", i);
      return NULL;
    }
    S->pointr[i] = x - 1;
  }
  S->pointr[S->cols] = S->vals;

  /* Read row indices. */
  for (i = 0; i < S->vals; i++) {
    if (fscanf(file, " %ld", &x) != 1) {
      svd_error("svdLoadSparseTextHBFile: error reading rowind %d", i);
      return NULL;
    }
    S->rowind[i] = x - 1;
  }
  for (i = 0; i < S->vals; i++)
    if (fscanf(file, " %lf", S->value + i) != 1) {
      svd_error("svdLoadSparseTextHBFile: error reading value %d", i);
      return NULL;
    }
  return S;
}

static void svdWriteSparseTextHBFile(SMat S, FILE *file) {
  int i;
  long col_lines = ((S->cols + 1) / 8) + (((S->cols + 1) % 8) ? 1 : 0);
  long row_lines = (S->vals / 8) + ((S->vals % 8) ? 1 : 0);
  long total_lines = col_lines + 2 * row_lines;

  char title[32];
  sprintf(title, "SVDLIBC v. %s", SVDVersion);
  fprintf(file, "%-72s%-8s\n", title, "<key>");
  fprintf(file, "%14ld%14ld%14ld%14ld%14d\n", total_lines, col_lines,
          row_lines, row_lines, 0);
  fprintf(file, "%-14s%14ld%14ld%14ld%14d\n", "rra", S->rows, S->cols,
          S->vals, 0);
  fprintf(file, "%16s%16s%16s%16s\n", "(8i)", "(8i)", "(8e)", "(8e)");

  for (i = 0; i <= S->cols; i++)
    fprintf(file, "%ld%s", S->pointr[i] + 1, (((i+1) % 8) == 0) ? "\n" : " ");
  fprintf(file, "\n");
  for (i = 0; i < S->vals; i++)
    fprintf(file, "%ld%s", S->rowind[i] + 1, (((i+1) % 8) == 0) ? "\n" : " ");
  fprintf(file, "\n");
  for (i = 0; i < S->vals; i++)
    fprintf(file, "%g%s", S->value[i], (((i+1) % 8) == 0) ? "\n" : " ");
  fprintf(file, "\n");
}


static SMat svdLoadSparseTextFile(FILE *file) {
  long c, i, n, v, rows, cols, vals;
  SMat S;
  if (fscanf(file, " %ld %ld %ld", &rows, &cols, &vals) != 3) {
    svd_error("svdLoadSparseTextFile: bad file format");
    return NULL;
  }

  S = svdNewSMat(rows, cols, vals);
  if (!S) return NULL;

  for (c = 0, v = 0; c < cols; c++) {
    if (fscanf(file, " %ld", &n) != 1) {
      svd_error("svdLoadSparseTextFile: bad file format");
      return NULL;
    }
    S->pointr[c] = v;
    for (i = 0; i < n; i++, v++) {
      if (fscanf(file, " %ld %lf", S->rowind + v, S->value + v) != 2) {
        svd_error("svdLoadSparseTextFile: bad file format");
        return NULL;
      }
    }
  }
  S->pointr[cols] = vals;
  return S;
}

static void svdWriteSparseTextFile(SMat S, FILE *file) {
  int c, v;
  fprintf(file, "%ld %ld %ld\n", S->rows, S->cols, S->vals);
  for (c = 0, v = 0; c < S->cols; c++) {
    fprintf(file, "%ld\n", S->pointr[c + 1] - S->pointr[c]);
    for (; v < S->pointr[c+1]; v++)
      fprintf(file, "%ld %g\n", S->rowind[v], S->value[v]);
  }
}


static SMat svdLoadSparseBinaryFile(FILE *file) {
  int rows, cols, vals, n, c, i, v, r, e = 0;
  float f;
  SMat S;
  e += svd_readBinInt(file, &rows);
  e += svd_readBinInt(file, &cols);
  e += svd_readBinInt(file, &vals);
  if (e) {
    svd_error("svdLoadSparseBinaryFile: bad file format");
    return NULL;
  }

  S = svdNewSMat(rows, cols, vals);
  if (!S) return NULL;

  for (c = 0, v = 0; c < cols; c++) {
    if (svd_readBinInt(file, &n)) {
      svd_error("svdLoadSparseBinaryFile: bad file format");
      return NULL;
    }
    S->pointr[c] = v;
    for (i = 0; i < n; i++, v++) {
      e += svd_readBinInt(file, &r);
      e += svd_readBinFloat(file, &f);
      if (e) {
        svd_error("svdLoadSparseBinaryFile: bad file format");
        return NULL;
      }
      S->rowind[v] = r;
      S->value[v] = f;
    }
  }
  S->pointr[cols] = vals;
  return S;
}

static void svdWriteSparseBinaryFile(SMat S, FILE *file) {
  int c, v;
  svd_writeBinInt(file, (int) S->rows);
  svd_writeBinInt(file, (int) S->cols);
  svd_writeBinInt(file, (int) S->vals);
  for (c = 0, v = 0; c < S->cols; c++) {
    svd_writeBinInt(file, (int) (S->pointr[c + 1] - S->pointr[c]));
    for (; v < S->pointr[c+1]; v++) {
      svd_writeBinInt(file, (int) S->rowind[v]);
      svd_writeBinFloat(file, (float) S->value[v]);
    }
  }
}


static DMat svdLoadDenseTextFile(FILE *file) {
  long rows, cols, i, j;
  DMat D;
  if (fscanf(file, " %ld %ld", &rows, &cols) != 2) {
    svd_error("svdLoadDenseTextFile: bad file format");
    return NULL;
  }

  D = svdNewDMat(rows, cols);
  if (!D) return NULL;

  for (i = 0; i < rows; i++)
    for (j = 0; j < cols; j++) {
      if (fscanf(file, " %lf", &(D->value[i][j])) != 1) {
        svd_error("svdLoadDenseTextFile: bad file format");
        return NULL;
      }
    }
  return D;
}

static void svdWriteDenseTextFile(DMat D, FILE *file) {
  int i, j;
  fprintf(file, "%ld %ld\n", D->rows, D->cols);
  for (i = 0; i < D->rows; i++)
    for (j = 0; j < D->cols; j++)
      fprintf(file, "%g%c", D->value[i][j], (j == D->cols - 1) ? '\n' : ' ');
}


static DMat svdLoadDenseBinaryFile(FILE *file) {
  int rows, cols, i, j, e = 0;
  float f;
  DMat D;
  e += svd_readBinInt(file, &rows);
  e += svd_readBinInt(file, &cols);
  if (e) {
    svd_error("svdLoadDenseBinaryFile: bad file format");
    return NULL;
  }

  D = svdNewDMat(rows, cols);
  if (!D) return NULL;

  for (i = 0; i < rows; i++)
    for (j = 0; j < cols; j++) {
      if (svd_readBinFloat(file, &f)) {
        svd_error("svdLoadDenseBinaryFile: bad file format");
        return NULL;
      }
      D->value[i][j] = f;
    }
  return D;
}

static void svdWriteDenseBinaryFile(DMat D, FILE *file) {
  int i, j;
  svd_writeBinInt(file, (int) D->rows);
  svd_writeBinInt(file, (int) D->cols);
  for (i = 0; i < D->rows; i++)
    for (j = 0; j < D->cols; j++)
      svd_writeBinFloat(file, (float) D->value[i][j]);
}


SMat svdLoadSparseMatrix(char *filename, int format) {
  SMat S = NULL;
  DMat D = NULL;
  FILE *file = svd_fatalReadFile(filename);
  switch (format) {
  case SVD_F_STH:
    S = svdLoadSparseTextHBFile(file);
    break;
  case SVD_F_ST:
    S = svdLoadSparseTextFile(file);
    break;
  case SVD_F_SB:
    S = svdLoadSparseBinaryFile(file);
    break;
  case SVD_F_DT:
    D = svdLoadDenseTextFile(file);
    break;
  case SVD_F_DB:
    D = svdLoadDenseBinaryFile(file);
    break;
  default: svd_error("svdLoadSparseMatrix: unknown format %d", format);
  }
  svd_closeFile(file);
  if (D) {
    S = svdConvertDtoS(D);
    svdFreeDMat(D);
  }
  return S;
}

DMat svdLoadDenseMatrix(char *filename, int format) {
  SMat S = NULL;
  DMat D = NULL;
  FILE *file = svd_fatalReadFile(filename);
  switch (format) {
  case SVD_F_STH:
    S = svdLoadSparseTextHBFile(file);
    break;
  case SVD_F_ST:
    S = svdLoadSparseTextFile(file);
    break;
  case SVD_F_SB:
    S = svdLoadSparseBinaryFile(file);
    break;
  case SVD_F_DT:
    D = svdLoadDenseTextFile(file);
    break;
  case SVD_F_DB:
    D = svdLoadDenseBinaryFile(file);
    break;
  default: svd_error("svdLoadSparseMatrix: unknown format %d", format);
  }
  svd_closeFile(file);
  if (S) {
    D = svdConvertStoD(S);
    svdFreeSMat(S);
  }
  return D;
}

void svdWriteSparseMatrix(SMat S, char *filename, int format) {
  DMat D = NULL;
  FILE *file = svd_writeFile(filename, FALSE);
  if (!file) {
    svd_error("svdWriteSparseMatrix: failed to write file %s\n", filename);
    return;
  }
  switch (format) {
  case SVD_F_STH:
    svdWriteSparseTextHBFile(S, file);
    break;
  case SVD_F_ST:
    svdWriteSparseTextFile(S, file);
    break;
  case SVD_F_SB:
    svdWriteSparseBinaryFile(S, file);
    break;
  case SVD_F_DT:
    D = svdConvertStoD(S);
    svdWriteDenseTextFile(D, file);
    break;
  case SVD_F_DB:
    D = svdConvertStoD(S);
    svdWriteDenseBinaryFile(D, file);
    break;
  default: svd_error("svdLoadSparseMatrix: unknown format %d", format);
  }
  svd_closeFile(file);
  if (D) svdFreeDMat(D);
}

void svdWriteDenseMatrix(DMat D, char *filename, int format) {
  SMat S = NULL;
  FILE *file = svd_writeFile(filename, FALSE);
  if (!file) {
    svd_error("svdWriteDenseMatrix: failed to write file %s\n", filename);
    return;
  }
  switch (format) {
  case SVD_F_STH:
    S = svdConvertDtoS(D);
    svdWriteSparseTextHBFile(S, file);
    break;
  case SVD_F_ST:
    S = svdConvertDtoS(D);
    svdWriteSparseTextFile(S, file);
    break;
  case SVD_F_SB:
    S = svdConvertDtoS(D);
    svdWriteSparseBinaryFile(S, file);
    break;
  case SVD_F_DT:
    svdWriteDenseTextFile(D, file);
    break;
  case SVD_F_DB:
    svdWriteDenseBinaryFile(D, file);
    break;
  default: svd_error("svdLoadSparseMatrix: unknown format %d", format);
  }
  svd_closeFile(file);
  if (S) svdFreeSMat(S);
}
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


#define MAXLL 2

#define LMTNW   100000000 /* max. size of working area allowed  */

enum storeVals {STORQ = 1, RETRQ, STORP, RETRP};

static char *error_msg[] = {  /* error messages used by function    *
                               * check_parameters                   */
  NULL,
  "",
  "ENDL MUST BE LESS THAN ENDR",
  "REQUESTED DIMENSIONS CANNOT EXCEED NUM ITERATIONS",
  "ONE OF YOUR DIMENSIONS IS LESS THAN OR EQUAL TO ZERO",
  "NUM ITERATIONS (NUMBER OF LANCZOS STEPS) IS INVALID",
  "REQUESTED DIMENSIONS (NUMBER OF EIGENPAIRS DESIRED) IS INVALID",
  "6*N+4*ITERATIONS+1 + ITERATIONS*ITERATIONS CANNOT EXCEED NW",
  "6*N+4*ITERATIONS+1 CANNOT EXCEED NW", NULL};

double **LanStore, *OPBTemp;
double eps, eps1, reps, eps34;
long ierr;
/*
double rnm, anorm, tol;
FILE *fp_out1, *fp_out2;
*/

void   purge(long n, long ll, double *r, double *q, double *ra,
             double *qa, double *wrk, double *eta, double *oldeta, long step,
             double *rnmp, double tol);
void   ortbnd(double *alf, double *eta, double *oldeta, double *bet, long step,
              double rnm);
double startv(SMat A, double *wptr[], long step, long n);
void   store(long, long, long, double *);
void   imtql2(long, long, double *, double *, double *);
void   imtqlb(long n, double d[], double e[], double bnd[]);
void   write_header(long, long, double, double, long, double, long, long,
                    long);
long   check_parameters(SMat A, long dimensions, long iterations,
                        double endl, double endr, long vectors);
int    lanso(SMat A, long iterations, long dimensions, double endl,
             double endr, double *ritz, double *bnd, double *wptr[],
             long *neigp, long n);
long   ritvec(long n, SMat A, SVDRec R, double kappa, double *ritz,
              double *bnd, double *alf, double *bet, double *w2,
              long steps, long neig);
long   lanczos_step(SMat A, long first, long last, double *wptr[],
                    double *alf, double *eta, double *oldeta,
                    double *bet, long *ll, long *enough, double *rnmp,
                    double *tolp, long n);
void   stpone(SMat A, double *wrkptr[], double *rnmp, double *tolp, long n);
long   error_bound(long *, double, double, double *, double *, long step,
                   double tol);
void   machar(long *ibeta, long *it, long *irnd, long *machep, long *negep);

/***********************************************************************
 *                                                                     *
 *                        main()                                       *
 * Sparse SVD(A) via Eigensystem of A'A symmetric Matrix 	       *
 *                  (double precision)                                 *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   This sample program uses landr to compute singular triplets of A via
   the equivalent symmetric eigenvalue problem

   B x = lambda x, where x' = (u',v'), lambda = sigma**2,
   where sigma is a singular value of A,

   B = A'A , and A is m (nrow) by n (ncol) (nrow >> ncol),

   so that {u,sqrt(lambda),v} is a singular triplet of A.
   (A' = transpose of A)

   User supplied routines: svd_opa, opb, store, timer

   svd_opa(     x,y) takes an n-vector x and returns A*x in y.
   svd_opb(ncol,x,y) takes an n-vector x and returns B*x in y.

   Based on operation flag isw, store(n,isw,j,s) stores/retrieves
   to/from storage a vector of length n in s.

   User should edit timer() with an appropriate call to an intrinsic
   timing routine that returns elapsed user time.


   External parameters
   -------------------

   Defined and documented in las2.h


   Local parameters
   ----------------

  (input)
   endl     left end of interval containing unwanted eigenvalues of B
   endr     right end of interval containing unwanted eigenvalues of B
   kappa    relative accuracy of ritz values acceptable as eigenvalues
              of B
	      vectors is not equal to 1
   r        work array
   n	    dimension of the eigenproblem for matrix B (ncol)
   dimensions   upper limit of desired number of singular triplets of A
   iterations   upper limit of desired number of Lanczos steps
   nnzero   number of nonzeros in A
   vectors  1 indicates both singular values and singular vectors are
	      wanted and they can be found in output file lav2;
	      0 indicates only singular values are wanted
   		
  (output)
   ritz	    array of ritz values
   bnd      array of error bounds
   d        array of singular values
   memory   total memory allocated in bytes to solve the B-eigenproblem


   Functions used
   --------------

   BLAS		svd_daxpy, svd_dscal, svd_ddot
   USER		svd_opa, svd_opb, timer
   MISC		write_header, check_parameters
   LAS2		landr


   Precision
   ---------

   All floating-point calculations are done in double precision;
   variables are declared as long and double.


   LAS2 development
   ----------------

   LAS2 is a C translation of the Fortran-77 LAS2 from the SVDPACK
   library written by Michael W. Berry, University of Tennessee,
   Dept. of Computer Science, 107 Ayres Hall, Knoxville, TN, 37996-1301

   31 Jan 1992:  Date written

   Theresa H. Do
   University of Tennessee
   Dept. of Computer Science
   107 Ayres Hall
   Knoxville, TN, 37996-1301
   internet: tdo@cs.utk.edu

 ***********************************************************************/

/***********************************************************************
 *								       *
 *		      check_parameters()			       *
 *								       *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------
   Function validates input parameters and returns error code (long)

   Parameters
   ----------
  (input)
   dimensions   upper limit of desired number of eigenpairs of B
   iterations   upper limit of desired number of lanczos steps
   n        dimension of the eigenproblem for matrix B
   endl     left end of interval containing unwanted eigenvalues of B
   endr     right end of interval containing unwanted eigenvalues of B
   vectors  1 indicates both eigenvalues and eigenvectors are wanted
            and they can be found in lav2; 0 indicates eigenvalues only
   nnzero   number of nonzero elements in input matrix (matrix A)

 ***********************************************************************/

long check_parameters(SMat A, long dimensions, long iterations,
		      double endl, double endr, long vectors) {
   long error_index;
   error_index = 0;

   if (endl >/*=*/ endr)  error_index = 2;
   else if (dimensions > iterations) error_index = 3;
   else if (A->cols <= 0 || A->rows <= 0) error_index = 4;
   /*else if (n > A->cols || n > A->rows) error_index = 1;*/
   else if (iterations <= 0 || iterations > A->cols || iterations > A->rows)
     error_index = 5;
   else if (dimensions <= 0 || dimensions > iterations) error_index = 6;
   if (error_index)
     svd_error("svdLAS2 parameter error: %s\n", error_msg[error_index]);
   return(error_index);
}

/***********************************************************************
 *								       *
 *			  write_header()			       *
 *   Function writes out header of output file containing ritz values  *
 *								       *
 ***********************************************************************/

void write_header(long iterations, long dimensions, double endl, double endr,
                  long vectors, double kappa, long nrow, long ncol,
                  long vals) {
  {
  printf("SOLVING THE [A^TA] EIGENPROBLEM\n");
  printf("NO. OF ROWS               = %6ld\n", nrow);
  printf("NO. OF COLUMNS            = %6ld\n", ncol);
  printf("NO. OF NON-ZERO VALUES    = %6ld\n", vals);
  printf("MATRIX DENSITY            = %6.2f%%\n",
         ((float) vals / nrow) * 100 / ncol);
  /* printf("ORDER OF MATRIX A         = %5ld\n", n); */
  printf("MAX. NO. OF LANCZOS STEPS = %6ld\n", iterations);
  printf("MAX. NO. OF EIGENPAIRS    = %6ld\n", dimensions);
  printf("LEFT  END OF THE INTERVAL = %9.2E\n", endl);
  printf("RIGHT END OF THE INTERVAL = %9.2E\n", endr);
  printf("KAPPA                     = %9.2E\n", kappa);
  /* printf("WANT S-VECTORS?   [T/F]   =     %c\n", (vectors) ? 'T' : 'F'); */
  printf("\n");
  }
  return;
}


/***********************************************************************
 *                                                                     *
 *				landr()				       *
 *        Lanczos algorithm with selective orthogonalization           *
 *                    Using Simon's Recurrence                         *
 *                       (double precision)                            *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   landr() is the LAS2 driver routine that, upon entry,
     (1)  checks for the validity of input parameters of the
	  B-eigenproblem
     (2)  determines several machine constants
     (3)  makes a Lanczos run
     (4)  calculates B-eigenvectors (singular vectors of A) if requested
	  by user


   arguments
   ---------

   (input)
   n        dimension of the eigenproblem for A'A
   iterations   upper limit of desired number of Lanczos steps
   dimensions   upper limit of desired number of eigenpairs
   nnzero   number of nonzeros in matrix A
   endl     left end of interval containing unwanted eigenvalues of B
   endr     right end of interval containing unwanted eigenvalues of B
   vectors  1 indicates both eigenvalues and eigenvectors are wanted
              and they can be found in output file lav2;
	    0 indicates only eigenvalues are wanted
   kappa    relative accuracy of ritz values acceptable as eigenvalues
	      of B (singular values of A)
   r        work array

   (output)
   j        number of Lanczos steps actually taken
   neig     number of ritz values stabilized
   ritz     array to hold the ritz values
   bnd      array to hold the error bounds


   External parameters
   -------------------

   Defined and documented in las2.h


   local parameters
   -------------------

   ibeta    radix for the floating-point representation
   it       number of base ibeta digits in the floating-point significand
   irnd     floating-point addition rounded or chopped
   machep   machine relative precision or round-off error
   negeps   largest negative integer
   wptr	    array of pointers each pointing to a work space


   Functions used
   --------------

   MISC         svd_dmax, machar, check_parameters
   LAS2         ritvec, lanso

 ***********************************************************************/

SVDRec svdLAS2A(SMat A, long dimensions) {
  double end[2] = {-1.0e-30, 1.0e-30};
  double kappa = 1e-6;
  if (!A) {
    svd_error("svdLAS2A called with NULL array\n");
    return NULL;
  }
  return svdLAS2(A, dimensions, 0, end, kappa);
}


SVDRec svdLAS2(SMat A, long dimensions, long iterations, double end[2],
               double kappa) {
  char transpose = FALSE;
  long ibeta, it, irnd, machep, negep, n, i, steps, nsig, neig, m;
  double *wptr[10], *ritz, *bnd;
  SVDRec R = NULL;
  ierr = 0;  // reset the global error flag

  svdResetCounters();
  svd_random2(NULL) ; /* RWCox */

  m = svd_imin(A->rows, A->cols);
  if (dimensions <= 0 || dimensions > m)
    dimensions = m;
  if (iterations <= 0 || iterations > m)
    iterations = m;
  if (iterations < dimensions) iterations = dimensions;

  /* Write output header */
  if (SVDVerbosity > 0)
    write_header(iterations, dimensions, end[0], end[1], TRUE, kappa, A->rows,
                 A->cols, A->vals);

  /* Check parameters */
  if (check_parameters(A, dimensions, iterations, end[0], end[1], TRUE))
    return NULL;

  /* If A is wide, the SVD is computed on its transpose for speed. */
  if (A->cols >= A->rows * 1.2) {
    if (SVDVerbosity > 0) printf("TRANSPOSING THE MATRIX FOR SPEED\n");
    transpose = TRUE;
    A = svdTransposeS(A);
  }

  n = A->cols;
  /* Compute machine precision */
  machar(&ibeta, &it, &irnd, &machep, &negep);
  eps1 = eps * sqrt((double) n);
  reps = sqrt(eps);
  eps34 = reps * sqrt(reps);

  /* Allocate temporary space. */
  if (!(wptr[0] = svd_doubleArray(n, TRUE, "las2: wptr[0]"))) goto abort;
  if (!(wptr[1] = svd_doubleArray(n, FALSE, "las2: wptr[1]"))) goto abort;
  if (!(wptr[2] = svd_doubleArray(n, FALSE, "las2: wptr[2]"))) goto abort;
  if (!(wptr[3] = svd_doubleArray(n, FALSE, "las2: wptr[3]"))) goto abort;
  if (!(wptr[4] = svd_doubleArray(n, FALSE, "las2: wptr[4]"))) goto abort;
  if (!(wptr[5] = svd_doubleArray(n, FALSE, "las2: wptr[5]"))) goto abort;
  if (!(wptr[6] = svd_doubleArray(iterations, FALSE, "las2: wptr[6]")))
    goto abort;
  if (!(wptr[7] = svd_doubleArray(iterations, FALSE, "las2: wptr[7]")))
    goto abort;
  if (!(wptr[8] = svd_doubleArray(iterations, FALSE, "las2: wptr[8]")))
    goto abort;
  if (!(wptr[9] = svd_doubleArray(iterations + 1, FALSE, "las2: wptr[9]")))
    goto abort;
  /* Calloc may be unnecessary: */
  if (!(ritz    = svd_doubleArray(iterations + 1, TRUE, "las2: ritz")))
    goto abort;
  /* Calloc may be unnecessary: */
  if (!(bnd     = svd_doubleArray(iterations + 1, TRUE, "las2: bnd")))
    goto abort;
  memset(bnd, 127, (iterations + 1) * sizeof(double));

  if (!(LanStore = (double **) calloc(iterations + MAXLL, sizeof(double *))))
    goto abort;
  if (!(OPBTemp = svd_doubleArray(A->rows, FALSE, "las2: OPBTemp")))
    goto abort;

  /* Actually run the lanczos thing: */
  steps = lanso(A, iterations, dimensions, end[0], end[1], ritz, bnd, wptr,
                &neig, n);

  /* Print some stuff. */
  if (SVDVerbosity > 0) {
    printf("NUMBER OF LANCZOS STEPS   = %6ld\n"
           "RITZ VALUES STABILIZED    = %6ld\n", steps + 1, neig);
  }
  if (SVDVerbosity > 2) {
    printf("COMPUTED RITZ VALUES  (ERROR BNDS)\n");
    for (i = 0; i <= steps; i++)
      printf("# %3ld  %22.14E  (%11.2E)   ", i + 1, ritz[i], bnd[i]);
    printf("\n") ;
  }

  SAFE_FREE(wptr[0]);
  SAFE_FREE(wptr[1]);
  SAFE_FREE(wptr[2]);
  SAFE_FREE(wptr[3]);
  SAFE_FREE(wptr[4]);
  SAFE_FREE(wptr[7]);
  SAFE_FREE(wptr[8]);

  /* Compute eigenvectors */
  kappa = svd_dmax(fabs(kappa), eps34);

  R = svdNewSVDRec();
  if (!R) {
    svd_error("svdLAS2: allocation of R failed");
    goto cleanup;
  }
  R->d  = /*svd_imin(nsig, dimensions)*/dimensions;
  R->Ut = svdNewDMat(R->d, A->rows);
  R->S  = svd_doubleArray(R->d, TRUE, "las2: R->s");
  R->Vt = svdNewDMat(R->d, A->cols);
  if (!R->Ut || !R->S || !R->Vt) {
    svd_error("svdLAS2: allocation of R failed");
    goto cleanup;
  }

  nsig = ritvec(n, A, R, kappa, ritz, bnd, wptr[6], wptr[9], wptr[5], steps,
                neig);

  if (SVDVerbosity > 1) {
    printf("\nSINGULAR VALUES: ");
    svdWriteDenseArray(R->S, R->d, "-", FALSE);

    if (SVDVerbosity > 2) {
      printf("\nLEFT SINGULAR VECTORS (transpose of U): ");
      svdWriteDenseMatrix(R->Ut, "-", SVD_F_DT);

      printf("\nRIGHT SINGULAR VECTORS (transpose of V): ");
      svdWriteDenseMatrix(R->Vt, "-", SVD_F_DT);
    }
  }
  if (SVDVerbosity > 0) {
    printf("SINGULAR VALUES FOUND     = %6d\n"
	   "SIGNIFICANT VALUES        = %6ld\n", R->d, nsig);
  }

 cleanup:
  for (i = 0; i <= 9; i++)
    SAFE_FREE(wptr[i]);
  SAFE_FREE(ritz);
  SAFE_FREE(bnd);
  if (LanStore) {
    for (i = 0; i < iterations + MAXLL; i++)
      SAFE_FREE(LanStore[i]);
    SAFE_FREE(LanStore);
  }
  SAFE_FREE(OPBTemp);

  /* This swaps and transposes the singular matrices if A was transposed. */
  if (R && transpose) {
    DMat T;
    svdFreeSMat(A);
    T = R->Ut;
    R->Ut = R->Vt;
    R->Vt = T;
  }

  return R;
abort:
  svd_error("svdLAS2: fatal error, aborting");
  return NULL;
}


/***********************************************************************
 *                                                                     *
 *                        ritvec()                                     *
 * 	    Function computes the singular vectors of matrix A	       *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   This function is invoked by landr() only if eigenvectors of the A'A
   eigenproblem are desired.  When called, ritvec() computes the
   singular vectors of A and writes the result to an unformatted file.


   Parameters
   ----------

   (input)
   nrow       number of rows of A
   steps      number of Lanczos iterations performed
   fp_out2    pointer to unformatted output file
   n	      dimension of matrix A
   kappa      relative accuracy of ritz values acceptable as
		eigenvalues of A'A
   ritz       array of ritz values
   bnd        array of error bounds
   alf        array of diagonal elements of the tridiagonal matrix T
   bet        array of off-diagonal elements of T
   w1, w2     work space

   (output)
   xv1        array of eigenvectors of A'A (right singular vectors of A)
   ierr	      error code
              0 for normal return from imtql2()
	      k if convergence did not occur for k-th eigenvalue in
	        imtql2()
   nsig       number of accepted ritz values based on kappa

   (local)
   s	      work array which is initialized to the identity matrix
	      of order (j + 1) upon calling imtql2().  After the call,
	      s contains the orthonormal eigenvectors of the symmetric
	      tridiagonal matrix T

   Functions used
   --------------

   BLAS		svd_dscal, svd_dcopy, svd_daxpy
   USER		store
   		imtql2

 ***********************************************************************/

void rotateArray(double *a, int size, int x) {
  int i, j, n, start;
  double t1, t2;
  if (x == 0) return;
  j = start = 0;
  t1 = a[0];
  for (i = 0; i < size; i++) {
    n = (j >= x) ? j - x : j + size - x;
    t2 = a[n];
    a[n] = t1;
    t1 = t2;
    j = n;
    if (j == start) {
      start = ++j;
      t1 = a[j];
    }
  }
}

long ritvec(long n, SMat A, SVDRec R, double kappa, double *ritz, double *bnd,
            double *alf, double *bet, double *w2, long steps, long neig) {
  long js, jsq, i, k, /*size,*/ id2, tmp, nsig, x;
  double *s, *xv2, tmp0, tmp1, xnorm, *w1 = R->Vt->value[0];

  js = steps + 1;
  jsq = js * js;
  /*size = sizeof(double) * n;*/

  s = svd_doubleArray(jsq, TRUE, "ritvec: s");
  xv2 = svd_doubleArray(n, FALSE, "ritvec: xv2");

  /* initialize s to an identity matrix */
  for (i = 0; i < jsq; i+= (js+1)) s[i] = 1.0;
  svd_dcopy(js, alf, 1, w1, -1);
  svd_dcopy(steps, &bet[1], 1, &w2[1], -1);

  /* on return from imtql2(), w1 contains eigenvalues in ascending
   * order and s contains the corresponding eigenvectors */
  imtql2(js, js, w1, w2, s);

  /*fwrite((char *)&n, sizeof(n), 1, fp_out2);
    fwrite((char *)&js, sizeof(js), 1, fp_out2);
    fwrite((char *)&kappa, sizeof(kappa), 1, fp_out2);*/
  /*id = 0;*/
  nsig = 0;

  if (ierr) {
    R->d = 0;
  } else {
    x = 0;
    id2 = jsq - js;
    for (k = 0; k < js; k++) {
      tmp = id2;
      if (bnd[k] <= kappa * fabs(ritz[k]) && k > js-neig-1) {
	if (--x < 0) x = R->d - 1;
	w1 = R->Vt->value[x];
	for (i = 0; i < n; i++) w1[i] = 0.0;
	for (i = 0; i < js; i++) {
	  store(n, RETRQ, i, w2);
	  svd_daxpy(n, s[tmp], w2, 1, w1, 1);
	  tmp -= js;
	}
	/*fwrite((char *)w1, size, 1, fp_out2);*/

	/* store the w1 vector row-wise in array xv1;
	 * size of xv1 is (steps+1) * (nrow+ncol) elements
	 * and each vector, even though only ncol long,
	 * will have (nrow+ncol) elements in xv1.
	 * It is as if xv1 is a 2-d array (steps+1) by
	 * (nrow+ncol) and each vector occupies a row  */

	/* j is the index in the R arrays, which are sorted by high to low
	   singular values. */

	/*for (i = 0; i < n; i++) R->Vt->value[x]xv1[id++] = w1[i];*/
	/*id += nrow;*/
	nsig++;
      }
      id2++;
    }
    SAFE_FREE(s);

    /* Rotate the singular vectors and values. */
    /* x is now the location of the highest singular value. */
    rotateArray(R->Vt->value[0], R->Vt->rows * R->Vt->cols,
		x * R->Vt->cols);
    R->d = svd_imin(R->d, nsig);
    for (x = 0; x < R->d; x++) {
      /* multiply by matrix B first */
      svd_opb(A, R->Vt->value[x], xv2, OPBTemp);
      tmp0 = svd_ddot(n, R->Vt->value[x], 1, xv2, 1);
      svd_daxpy(n, -tmp0, R->Vt->value[x], 1, xv2, 1);
      tmp0 = sqrt(tmp0);
      xnorm = sqrt(svd_ddot(n, xv2, 1, xv2, 1));

      /* multiply by matrix A to get (scaled) left s-vector */
      svd_opa(A, R->Vt->value[x], R->Ut->value[x]);
      tmp1 = 1.0 / tmp0;
      svd_dscal(A->rows, tmp1, R->Ut->value[x], 1);
      xnorm *= tmp1;
      bnd[i] = xnorm;
      R->S[x] = tmp0;
    }
  }

  SAFE_FREE(s);
  SAFE_FREE(xv2);
  return nsig;
}

/*----------------------------------------------------------------------*/

static int vstep=0 ;
static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[vstep%10] ) ;
   if( vstep%10 == 9) fprintf(stderr,".") ;
   vstep++ ;
}

/***********************************************************************
 *                                                                     *
 *                          lanso()                                    *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   Function determines when the restart of the Lanczos algorithm should
   occur and when it should terminate.

   Arguments
   ---------

   (input)
   n         dimension of the eigenproblem for matrix B
   iterations    upper limit of desired number of lanczos steps
   dimensions    upper limit of desired number of eigenpairs
   endl      left end of interval containing unwanted eigenvalues
   endr      right end of interval containing unwanted eigenvalues
   ritz      array to hold the ritz values
   bnd       array to hold the error bounds
   wptr      array of pointers that point to work space:
  	       wptr[0]-wptr[5]  six vectors of length n		
  	       wptr[6] array to hold diagonal of the tridiagonal matrix T
  	       wptr[9] array to hold off-diagonal of T	
  	       wptr[7] orthogonality estimate of Lanczos vectors at
		 step j
 	       wptr[8] orthogonality estimate of Lanczos vectors at
		 step j-1

   (output)
   j         number of Lanczos steps actually taken
   neig      number of ritz values stabilized
   ritz      array to hold the ritz values
   bnd       array to hold the error bounds
   ierr      (globally declared) error flag
	     ierr = 8192 if stpone() fails to find a starting vector
	     ierr = k if convergence did not occur for k-th eigenvalue
		    in imtqlb()
	     ierr = 0 otherwise


   Functions used
   --------------

   LAS		stpone, error_bound, lanczos_step
   MISC		svd_dsort2
   UTILITY	svd_imin, svd_imax

 ***********************************************************************/

int lanso(SMat A, long iterations, long dimensions, double endl,
          double endr, double *ritz, double *bnd, double *wptr[],
          long *neigp, long n) {
  double *alf, *eta, *oldeta, *bet, *wrk, rnm, tol;
  long ll, first, last, ENOUGH, id2, id3, i, l, neig, j = 0, intro = 0;

  alf = wptr[6];
  eta = wptr[7];
  oldeta = wptr[8];
  bet = wptr[9];
  wrk = wptr[5];

  /* take the first step */
  stpone(A, wptr, &rnm, &tol, n);
  if (!rnm || ierr) return 0;
  eta[0] = eps1;
  oldeta[0] = eps1;
  ll = 0;
  first = 1;
  last = svd_imin(dimensions + svd_imax(8, dimensions), iterations);
  ENOUGH = FALSE;
  /*id1 = 0;*/
  if( SVDVerbosity > 1 ){ fprintf(stderr,"Lanczos:"); vstep=0; }
  while (/*id1 < dimensions && */!ENOUGH) {
    if (rnm <= tol) rnm = 0.0;

    /* the actual lanczos loop */
    if( SVDVerbosity > 1 ) vstep_print() ;
    j = lanczos_step(A, first, last, wptr, alf, eta, oldeta, bet, &ll,
                     &ENOUGH, &rnm, &tol, n);
    if( SVDVerbosity > 1 ) fprintf(stderr,".") ;
    if (ENOUGH) j = j - 1;
    else j = last - 1;
    first = j + 1;
    bet[j+1] = rnm;

    /* analyze T */
    l = 0;
    for (id2 = 0; id2 < j; id2++) {
      if (l > j) break;
      for (i = l; i <= j; i++) if (!bet[i+1]) break;
      if (i > j) i = j;

      /* now i is at the end of an unreduced submatrix */
      svd_dcopy(i-l+1, &alf[l],   1, &ritz[l],  -1);
      svd_dcopy(i-l,   &bet[l+1], 1, &wrk[l+1], -1);

      imtqlb(i-l+1, &ritz[l], &wrk[l], &bnd[l]);

      if (ierr) {
        svd_error("svdLAS2: imtqlb failed to converge (ierr = %ld)\n", ierr);
        svd_error("  l = %ld  i = %ld\n", l, i);
        for (id3 = l; id3 <= i; id3++)
          svd_error("  %ld  %lg  %lg  %lg\n",
                    id3, ritz[id3], wrk[id3], bnd[id3]);
      }
      for (id3 = l; id3 <= i; id3++)
        bnd[id3] = rnm * fabs(bnd[id3]);
      l = i + 1;
    }
    if( SVDVerbosity > 1 ) fprintf(stderr,".") ;

    /* sort eigenvalues into increasing order */
    svd_dsort2((j+1) / 2, j + 1, ritz, bnd);

    /*    for (i = 0; i < iterations; i++)
      printf("%f ", ritz[i]);
      printf("\n"); */

    /* massage error bounds for very close ritz values */
    neig = error_bound(&ENOUGH, endl, endr, ritz, bnd, j, tol);
    *neigp = neig;

    /* should we stop? */
    if (neig < dimensions) {
      if (!neig) {
        last = first + 9;
        intro = first;
      } else last = first + svd_imax(3, 1 + ((j - intro) * (dimensions-neig)) /
                                     neig);
      last = svd_imin(last, iterations);
    } else ENOUGH = TRUE;
    ENOUGH = ENOUGH || first >= iterations;
    /* id1++; */
    /* printf("id1=%d dimen=%d first=%d\n", id1, dimensions, first); */
    if( SVDVerbosity > 1 ) fprintf(stderr,".") ;
  }
  store(n, STORQ, j, wptr[1]);
  return j;
}


/***********************************************************************
 *                                                                     *
 *			lanczos_step()                                 *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   Function embodies a single Lanczos step

   Arguments
   ---------

   (input)
   n        dimension of the eigenproblem for matrix B
   first    start of index through loop				
   last     end of index through loop				
   wptr	    array of pointers pointing to work space		
   alf	    array to hold diagonal of the tridiagonal matrix T
   eta      orthogonality estimate of Lanczos vectors at step j
   oldeta   orthogonality estimate of Lanczos vectors at step j-1
   bet      array to hold off-diagonal of T
   ll       number of intitial Lanczos vectors in local orthog.
              (has value of 0, 1 or 2)			
   enough   stop flag			

   Functions used
   --------------

   BLAS		svd_ddot, svd_dscal, svd_daxpy, svd_datx, svd_dcopy
   USER		store
   LAS		purge, ortbnd, startv
   UTILITY	svd_imin, svd_imax

 ***********************************************************************/

long lanczos_step(SMat A, long first, long last, double *wptr[],
		  double *alf, double *eta, double *oldeta,
		  double *bet, long *ll, long *enough, double *rnmp,
                  double *tolp, long n) {
   double t, *mid, rnm = *rnmp, tol = *tolp, anorm;
   long i, j;

    if( SVDVerbosity > 1 ) fprintf(stderr,"[%d.%d]",(int)first,(int)last) ;
   for (j=first; j<last; j++) {
      mid     = wptr[2];
      wptr[2] = wptr[1];
      wptr[1] = mid;
      mid     = wptr[3];
      wptr[3] = wptr[4];
      wptr[4] = mid;

      store(n, STORQ, j-1, wptr[2]);
      if (j-1 < MAXLL) store(n, STORP, j-1, wptr[4]);
      bet[j] = rnm;



     if( SVDVerbosity > 1 ) fprintf(stderr,"a") ;
      /* restart if invariant subspace is found */
      if (!bet[j]) {
     if( SVDVerbosity > 1 ) fprintf(stderr,"b") ;
	 rnm = startv(A, wptr, j, n);
	 if (ierr) return j;
	 if (!rnm) *enough = TRUE;
      }
      if (*enough) {
     if( SVDVerbosity > 1 ) fprintf(stderr,"c") ;
        /* added by Doug... */
        /* These lines fix a bug that occurs with low-rank matrices */
        mid     = wptr[2];
        wptr[2] = wptr[1];
        wptr[1] = mid;
        /* ...added by Doug */
        break;
      }

      /* take a lanczos step */
      t = 1.0 / rnm;
      svd_datx(n, t, wptr[0], 1, wptr[1], 1);
      svd_dscal(n, t, wptr[3], 1);
      svd_opb(A, wptr[3], wptr[0], OPBTemp);
      svd_daxpy(n, -rnm, wptr[2], 1, wptr[0], 1);
      alf[j] = svd_ddot(n, wptr[0], 1, wptr[3], 1);
      svd_daxpy(n, -alf[j], wptr[1], 1, wptr[0], 1);

      /* orthogonalize against initial lanczos vectors */
      if (j <= MAXLL && (fabs(alf[j-1]) > 4.0 * fabs(alf[j])))
	 *ll = j;
      for (i=0; i < svd_imin(*ll, j-1); i++) {
	 store(n, RETRP, i, wptr[5]);
	 t = svd_ddot(n, wptr[5], 1, wptr[0], 1);
	 store(n, RETRQ, i, wptr[5]);
         svd_daxpy(n, -t, wptr[5], 1, wptr[0], 1);
	 eta[i] = eps1;
	 oldeta[i] = eps1;
      }
     if( SVDVerbosity > 1 ) fprintf(stderr,"d") ;

      /* extended local reorthogonalization */
      t = svd_ddot(n, wptr[0], 1, wptr[4], 1);
      svd_daxpy(n, -t, wptr[2], 1, wptr[0], 1);
      if (bet[j] > 0.0) bet[j] = bet[j] + t;
      t = svd_ddot(n, wptr[0], 1, wptr[3], 1);
      svd_daxpy(n, -t, wptr[1], 1, wptr[0], 1);
      alf[j] = alf[j] + t;
      svd_dcopy(n, wptr[0], 1, wptr[4], 1);
      rnm = sqrt(svd_ddot(n, wptr[0], 1, wptr[4], 1));
      anorm = bet[j] + fabs(alf[j]) + rnm;
      tol = reps * anorm;

      /* update the orthogonality bounds */
      ortbnd(alf, eta, oldeta, bet, j, rnm);

      /* restore the orthogonality state when needed */
      purge(n, *ll, wptr[0], wptr[1], wptr[4], wptr[3], wptr[5], eta, oldeta,
            j, &rnm, tol);
      if (rnm <= tol) rnm = 0.0;
     if( SVDVerbosity > 1 ) fprintf(stderr,"e") ;
   }
   *rnmp = rnm;
   *tolp = tol;
   return j;
}

/***********************************************************************
 *                                                                     *
 *                          ortbnd()                                   *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   Funtion updates the eta recurrence

   Arguments
   ---------

   (input)
   alf      array to hold diagonal of the tridiagonal matrix T
   eta      orthogonality estimate of Lanczos vectors at step j
   oldeta   orthogonality estimate of Lanczos vectors at step j-1
   bet      array to hold off-diagonal of T
   n        dimension of the eigenproblem for matrix B		
   j        dimension of T					
   rnm	    norm of the next residual vector			
   eps1	    roundoff estimate for dot product of two unit vectors

   (output)
   eta      orthogonality estimate of Lanczos vectors at step j+1
   oldeta   orthogonality estimate of Lanczos vectors at step j


   Functions used
   --------------

   BLAS		svd_dswap

 ***********************************************************************/

void ortbnd(double *alf, double *eta, double *oldeta, double *bet, long step,
            double rnm) {
   long i;
   if (step < 1) return;
   if (rnm) {
      if (step > 1) {
	 oldeta[0] = (bet[1] * eta[1] + (alf[0]-alf[step]) * eta[0] -
		      bet[step] * oldeta[0]) / rnm + eps1;
      }
      for (i=1; i<=step-2; i++)
	 oldeta[i] = (bet[i+1] * eta[i+1] + (alf[i]-alf[step]) * eta[i] +
		      bet[i] * eta[i-1] - bet[step] * oldeta[i])/rnm + eps1;
   }
   oldeta[step-1] = eps1;
   svd_dswap(step, oldeta, 1, eta, 1);
   eta[step] = eps1;
   return;
}

/***********************************************************************
 *                                                                     *
 *				purge()                                *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   Function examines the state of orthogonality between the new Lanczos
   vector and the previous ones to decide whether re-orthogonalization
   should be performed


   Arguments
   ---------

   (input)
   n        dimension of the eigenproblem for matrix B		
   ll       number of intitial Lanczos vectors in local orthog.
   r        residual vector to become next Lanczos vector
   q        current Lanczos vector			
   ra       previous Lanczos vector
   qa       previous Lanczos vector
   wrk      temporary vector to hold the previous Lanczos vector
   eta      state of orthogonality between r and prev. Lanczos vectors
   oldeta   state of orthogonality between q and prev. Lanczos vectors
   j        current Lanczos step				

   (output)
   r	    residual vector orthogonalized against previous Lanczos
	      vectors
   q        current Lanczos vector orthogonalized against previous ones


   Functions used
   --------------

   BLAS		svd_daxpy,  svd_dcopy,  svd_idamax,  svd_ddot
   USER		store

 ***********************************************************************/

void purge(long n, long ll, double *r, double *q, double *ra,
	   double *qa, double *wrk, double *eta, double *oldeta, long step,
           double *rnmp, double tol) {
  double t, tq, tr, reps1, rnm = *rnmp;
  long k, iteration, flag, i;

  if (step < ll+2) return;

  k = svd_idamax(step - (ll+1), &eta[ll], 1) + ll;
  if (fabs(eta[k]) > reps) {
    reps1 = eps1 / reps;
    iteration = 0;
    flag = TRUE;
    while (iteration < 2 && flag) {
      if (rnm > tol) {

        /* bring in a lanczos vector t and orthogonalize both
         * r and q against it */
        tq = 0.0;
        tr = 0.0;
        for (i = ll; i < step; i++) {
          store(n,  RETRQ,  i,  wrk);
          t   = -svd_ddot(n, qa, 1, wrk, 1);
          tq += fabs(t);
          svd_daxpy(n,  t,  wrk,  1,  q,  1);
          t   = -svd_ddot(n, ra, 1, wrk, 1);
          tr += fabs(t);
          svd_daxpy(n, t, wrk, 1, r, 1);
        }
        svd_dcopy(n, q, 1, qa, 1);
        t   = -svd_ddot(n, r, 1, qa, 1);
        tr += fabs(t);
        svd_daxpy(n, t, q, 1, r, 1);
        svd_dcopy(n, r, 1, ra, 1);
        rnm = sqrt(svd_ddot(n, ra, 1, r, 1));
        if (tq <= reps1 && tr <= reps1 * rnm) flag = FALSE;
      }
      iteration++;
    }
    for (i = ll; i <= step; i++) {
      eta[i] = eps1;
      oldeta[i] = eps1;
    }
  }
  *rnmp = rnm;
  return;
}


/***********************************************************************
 *                                                                     *
 *                         stpone()                                    *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   Function performs the first step of the Lanczos algorithm.  It also
   does a step of extended local re-orthogonalization.

   Arguments
   ---------

   (input)
   n      dimension of the eigenproblem for matrix B

   (output)
   ierr   error flag
   wptr   array of pointers that point to work space that contains
	    wptr[0]             r[j]
	    wptr[1]             q[j]
	    wptr[2]             q[j-1]
	    wptr[3]             p
	    wptr[4]             p[j-1]
	    wptr[6]             diagonal elements of matrix T


   Functions used
   --------------

   BLAS		svd_daxpy, svd_datx, svd_dcopy, svd_ddot, svd_dscal
   USER		store, opb
   LAS		startv

 ***********************************************************************/

void stpone(SMat A, double *wrkptr[], double *rnmp, double *tolp, long n) {
   double t, *alf, rnm, anorm;
   alf = wrkptr[6];

   /* get initial vector; default is random */
   rnm = startv(A, wrkptr, 0, n);
   if (rnm == 0.0 || ierr != 0) return;

   /* normalize starting vector */
   t = 1.0 / rnm;
   svd_datx(n, t, wrkptr[0], 1, wrkptr[1], 1);
   svd_dscal(n, t, wrkptr[3], 1);

   /* take the first step */
   svd_opb(A, wrkptr[3], wrkptr[0], OPBTemp);
   alf[0] = svd_ddot(n, wrkptr[0], 1, wrkptr[3], 1);
   svd_daxpy(n, -alf[0], wrkptr[1], 1, wrkptr[0], 1);
   t = svd_ddot(n, wrkptr[0], 1, wrkptr[3], 1);
   svd_daxpy(n, -t, wrkptr[1], 1, wrkptr[0], 1);
   alf[0] += t;
   svd_dcopy(n, wrkptr[0], 1, wrkptr[4], 1);
   rnm = sqrt(svd_ddot(n, wrkptr[0], 1, wrkptr[4], 1));
   anorm = rnm + fabs(alf[0]);
   *rnmp = rnm;
   *tolp = reps * anorm;

   return;
}

/***********************************************************************
 *                                                                     *
 *                         startv()                                    *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   Function delivers a starting vector in r and returns |r|; it returns
   zero if the range is spanned, and ierr is non-zero if no starting
   vector within range of operator can be found.

   Parameters
   ---------

   (input)
   n      dimension of the eigenproblem matrix B
   wptr   array of pointers that point to work space
   j      starting index for a Lanczos run
   eps    machine epsilon (relative precision)

   (output)
   wptr   array of pointers that point to work space that contains
	  r[j], q[j], q[j-1], p[j], p[j-1]
   ierr   error flag (nonzero if no starting vector can be found)

   Functions used
   --------------

   BLAS		svd_ddot, svd_dcopy, svd_daxpy
   USER		svd_opb, store
   MISC		random

 ***********************************************************************/

double startv(SMat A, double *wptr[], long step, long n) {
   double rnm2, *r, t;
   long irand;
   long id, i;

   /* get initial vector; default is random */
   rnm2 = svd_ddot(n, wptr[0], 1, wptr[0], 1);
   irand = 918273 + step;
   r = wptr[0];
   for (id = 0; id < 3; id++) {
      if (id > 0 || step > 0 || rnm2 == 0)
	 for (i = 0; i < n; i++) r[i] = svd_random2(&irand);
      svd_dcopy(n, wptr[0], 1, wptr[3], 1);

      /* apply operator to put r in range (essential if m singular) */
      svd_opb(A, wptr[3], wptr[0], OPBTemp);
      svd_dcopy(n, wptr[0], 1, wptr[3], 1);
      rnm2 = svd_ddot(n, wptr[0], 1, wptr[3], 1);
      if (rnm2 > 0.0) break;
   }

   /* fatal error */
   if (rnm2 <= 0.0) {
      ierr = 8192;
      return(-1);
   }
   if (step > 0) {
      for (i = 0; i < step; i++) {
         store(n, RETRQ, i, wptr[5]);
	 t = -svd_ddot(n, wptr[3], 1, wptr[5], 1);
	 svd_daxpy(n, t, wptr[5], 1, wptr[0], 1);
      }

      /* make sure q[step] is orthogonal to q[step-1] */
      t = svd_ddot(n, wptr[4], 1, wptr[0], 1);
      svd_daxpy(n, -t, wptr[2], 1, wptr[0], 1);
      svd_dcopy(n, wptr[0], 1, wptr[3], 1);
      t = svd_ddot(n, wptr[3], 1, wptr[0], 1);
      if (t <= eps * rnm2) t = 0.0;
      rnm2 = t;
   }
   return(sqrt(rnm2));
}

/***********************************************************************
 *                                                                     *
 *			error_bound()                                  *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   Function massages error bounds for very close ritz values by placing
   a gap between them.  The error bounds are then refined to reflect
   this.


   Arguments
   ---------

   (input)
   endl     left end of interval containing unwanted eigenvalues
   endr     right end of interval containing unwanted eigenvalues
   ritz     array to store the ritz values
   bnd      array to store the error bounds
   enough   stop flag


   Functions used
   --------------

   BLAS		svd_idamax
   UTILITY	svd_dmin

 ***********************************************************************/

long error_bound(long *enough, double endl, double endr,
                 double *ritz, double *bnd, long step, double tol) {
  long mid, i, neig;
  double gapl, gap;

  /* massage error bounds for very close ritz values */
  mid = svd_idamax(step + 1, bnd, 1);

  for (i=((step+1) + (step-1)) / 2; i >= mid + 1; i -= 1)
    if (fabs(ritz[i-1] - ritz[i]) < eps34 * fabs(ritz[i]))
      if (bnd[i] > tol && bnd[i-1] > tol) {
        bnd[i-1] = sqrt(bnd[i] * bnd[i] + bnd[i-1] * bnd[i-1]);
        bnd[i] = 0.0;
      }


  for (i=((step+1) - (step-1)) / 2; i <= mid - 1; i +=1 )
    if (fabs(ritz[i+1] - ritz[i]) < eps34 * fabs(ritz[i]))
      if (bnd[i] > tol && bnd[i+1] > tol) {
        bnd[i+1] = sqrt(bnd[i] * bnd[i] + bnd[i+1] * bnd[i+1]);
        bnd[i] = 0.0;
      }

  /* refine the error bounds */
  neig = 0;
  gapl = ritz[step] - ritz[0];
  for (i = 0; i <= step; i++) {
    gap = gapl;
    if (i < step) gapl = ritz[i+1] - ritz[i];
    gap = svd_dmin(gap, gapl);
    if (gap > bnd[i]) bnd[i] = bnd[i] * (bnd[i] / gap);
    if (bnd[i] <= 16.0 * eps * fabs(ritz[i])) {
      neig++;
      if (!*enough) *enough = endl < ritz[i] && ritz[i] < endr;
    }
  }
  return neig;
}

/***********************************************************************
 *                                                                     *
 *				imtqlb()			       *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   imtqlb() is a translation of a Fortran version of the Algol
   procedure IMTQL1, Num. Math. 12, 377-383(1968) by Martin and
   Wilkinson, as modified in Num. Math. 15, 450(1970) by Dubrulle.
   Handbook for Auto. Comp., vol.II-Linear Algebra, 241-248(1971).
   See also B. T. Smith et al, Eispack Guide, Lecture Notes in
   Computer Science, Springer-Verlag, (1976).

   The function finds the eigenvalues of a symmetric tridiagonal
   matrix by the implicit QL method.


   Arguments
   ---------

   (input)
   n      order of the symmetric tridiagonal matrix
   d      contains the diagonal elements of the input matrix
   e      contains the subdiagonal elements of the input matrix in its
          last n-1 positions.  e[0] is arbitrary	

   (output)
   d      contains the eigenvalues in ascending order.  if an error
            exit is made, the eigenvalues are correct and ordered for
            indices 0,1,...ierr, but may not be the smallest eigenvalues.
   e      has been destroyed.					
   ierr   set to zero for normal return, j if the j-th eigenvalue has
            not been determined after 30 iterations.		

   Functions used
   --------------

   UTILITY	svd_fsign
   MISC		svd_pythag

 ***********************************************************************/

void imtqlb(long n, double d[], double e[], double bnd[])

{
   long last, l, m, i, iteration;

   /* various flags */
   long exchange, convergence, underflow;	

   double b, test, g, r, s, c, p, f;

   if (n == 1) return;
   ierr = 0;
   bnd[0] = 1.0;
   last = n - 1;
   for (i = 1; i < n; i++) {
      bnd[i] = 0.0;
      e[i-1] = e[i];
   }
   e[last] = 0.0;
   for (l = 0; l < n; l++) {
      iteration = 0;
      while (iteration <= 30) {
	 for (m = l; m < n; m++) {
	    convergence = FALSE;
	    if (m == last) break;
	    else {
	       test = fabs(d[m]) + fabs(d[m+1]);
	       if (test + fabs(e[m]) == test) convergence = TRUE;
	    }
	    if (convergence) break;
	 }
	    p = d[l];
	    f = bnd[l];
	 if (m != l) {
	    if (iteration == 30) {
	       ierr = l;
	       return;
	    }
	    iteration += 1;
	    /*........ form shift ........*/
	    g = (d[l+1] - p) / (2.0 * e[l]);
	    r = svd_pythag(g, 1.0);
	    g = d[m] - p + e[l] / (g + svd_fsign(r, g));
	    s = 1.0;
	    c = 1.0;
	    p = 0.0;
	    underflow = FALSE;
	    i = m - 1;
	    while (underflow == FALSE && i >= l) {
	       f = s * e[i];
	       b = c * e[i];
	       r = svd_pythag(f, g);
	       e[i+1] = r;
	       if (r == 0.0) underflow = TRUE;
	       else {
		  s = f / r;
		  c = g / r;
		  g = d[i+1] - p;
		  r = (d[i] - g) * s + 2.0 * c * b;
		  p = s * r;
		  d[i+1] = g + p;
		  g = c * r - b;
		  f = bnd[i+1];
		  bnd[i+1] = s * bnd[i] + c * f;
		  bnd[i] = c * bnd[i] - s * f;
		  i--;
	       }
	    }       /* end while (underflow != FALSE && i >= l) */
	    /*........ recover from underflow .........*/
	    if (underflow) {
	       d[i+1] -= p;
	       e[m] = 0.0;
	    }
	    else {
	       d[l] -= p;
	       e[l] = g;
	       e[m] = 0.0;
	    }
	 } 		       		   /* end if (m != l) */
	 else {

            /* order the eigenvalues */
	    exchange = TRUE;
	    if (l != 0) {
	       i = l;
	       while (i >= 1 && exchange == TRUE) {
	          if (p < d[i-1]) {
		     d[i] = d[i-1];
		     bnd[i] = bnd[i-1];
	             i--;
	          }
	          else exchange = FALSE;
	       }
	    }
	    if (exchange) i = 0;
	    d[i] = p;
	    bnd[i] = f;
	    iteration = 31;
	 }
      }			       /* end while (iteration <= 30) */
   }				   /* end for (l=0; l<n; l++) */
   return;
}						  /* end main */

/***********************************************************************
 *                                                                     *
 *				imtql2()			       *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   imtql2() is a translation of a Fortran version of the Algol
   procedure IMTQL2, Num. Math. 12, 377-383(1968) by Martin and
   Wilkinson, as modified in Num. Math. 15, 450(1970) by Dubrulle.
   Handbook for Auto. Comp., vol.II-Linear Algebra, 241-248(1971).
   See also B. T. Smith et al, Eispack Guide, Lecture Notes in
   Computer Science, Springer-Verlag, (1976).

   This function finds the eigenvalues and eigenvectors of a symmetric
   tridiagonal matrix by the implicit QL method.


   Arguments
   ---------

   (input)
   nm     row dimension of the symmetric tridiagonal matrix
   n      order of the matrix
   d      contains the diagonal elements of the input matrix
   e      contains the subdiagonal elements of the input matrix in its
            last n-1 positions.  e[0] is arbitrary	
   z      contains the identity matrix				

   (output)
   d      contains the eigenvalues in ascending order.  if an error
            exit is made, the eigenvalues are correct but unordered for
            for indices 0,1,...,ierr.				
   e      has been destroyed.					
   z      contains orthonormal eigenvectors of the symmetric
            tridiagonal (or full) matrix.  if an error exit is made,
            z contains the eigenvectors associated with the stored
          eigenvalues.					
   ierr   set to zero for normal return, j if the j-th eigenvalue has
            not been determined after 30 iterations.		


   Functions used
   --------------
   UTILITY	svd_fsign
   MISC		svd_pythag

 ***********************************************************************/

void imtql2(long nm, long n, double d[], double e[], double z[])

{
   long index, nnm, j, last, l, m, i, k, iteration, convergence, underflow;
   double b, test, g, r, s, c, p, f;
   if (n == 1) return;
   ierr = 0;
   last = n - 1;
   for (i = 1; i < n; i++) e[i-1] = e[i];
   e[last] = 0.0;
   nnm = n * nm;
   for (l = 0; l < n; l++) {
      iteration = 0;

      /* look for small sub-diagonal element */
      while (iteration <= 30) {
	 for (m = l; m < n; m++) {
	    convergence = FALSE;
	    if (m == last) break;
	    else {
	       test = fabs(d[m]) + fabs(d[m+1]);
	       if (test + fabs(e[m]) == test) convergence = TRUE;
	    }
	    if (convergence) break;
	 }
	 if (m != l) {

	    /* set error -- no convergence to an eigenvalue after
	     * 30 iterations. */
	    if (iteration == 30) {
	       ierr = l;
	       return;
	    }
	    p = d[l];
	    iteration += 1;

	    /* form shift */
	    g = (d[l+1] - p) / (2.0 * e[l]);
	    r = svd_pythag(g, 1.0);
	    g = d[m] - p + e[l] / (g + svd_fsign(r, g));
	    s = 1.0;
	    c = 1.0;
	    p = 0.0;
	    underflow = FALSE;
	    i = m - 1;
	    while (underflow == FALSE && i >= l) {
	       f = s * e[i];
	       b = c * e[i];
	       r = svd_pythag(f, g);
	       e[i+1] = r;
	       if (r == 0.0) underflow = TRUE;
	       else {
		  s = f / r;
		  c = g / r;
		  g = d[i+1] - p;
		  r = (d[i] - g) * s + 2.0 * c * b;
		  p = s * r;
		  d[i+1] = g + p;
		  g = c * r - b;

		  /* form vector */
		  for (k = 0; k < nnm; k += n) {
		     index = k + i;
		     f = z[index+1];
		     z[index+1] = s * z[index] + c * f;
		     z[index] = c * z[index] - s * f;
		  }
		  i--;
	       }
	    }   /* end while (underflow != FALSE && i >= l) */
	    /*........ recover from underflow .........*/
	    if (underflow) {
	       d[i+1] -= p;
	       e[m] = 0.0;
	    }
	    else {
	       d[l] -= p;
	       e[l] = g;
	       e[m] = 0.0;
	    }
	 }
	 else break;
      }		/*...... end while (iteration <= 30) .........*/
   }		/*...... end for (l=0; l<n; l++) .............*/

   /* order the eigenvalues */
   for (l = 1; l < n; l++) {
      i = l - 1;
      k = i;
      p = d[i];
      for (j = l; j < n; j++) {
	 if (d[j] < p) {
	    k = j;
	    p = d[j];
	 }
      }
      /* ...and corresponding eigenvectors */
      if (k != i) {
	 d[k] = d[i];
	 d[i] = p;
	  for (j = 0; j < nnm; j += n) {
	     p = z[j+i];
	     z[j+i] = z[j+k];
	     z[j+k] = p;
	  }
      }
   }
   return;
}		/*...... end main ............................*/

/***********************************************************************
 *                                                                     *
 *				machar()			       *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   This function is a partial translation of a Fortran-77 subroutine
   written by W. J. Cody of Argonne National Laboratory.
   It dynamically determines the listed machine parameters of the
   floating-point arithmetic.  According to the documentation of
   the Fortran code, "the determination of the first three uses an
   extension of an algorithm due to M. Malcolm, ACM 15 (1972),
   pp. 949-951, incorporating some, but not all, of the improvements
   suggested by M. Gentleman and S. Marovich, CACM 17 (1974),
   pp. 276-277."  The complete Fortran version of this translation is
   documented in W. J. Cody, "Machar: a Subroutine to Dynamically
   Determine Determine Machine Parameters," TOMS 14, December, 1988.


   Parameters reported
   -------------------

   ibeta     the radix for the floating-point representation
   it        the number of base ibeta digits in the floating-point
               significand					
   irnd      0 if floating-point addition chops		
             1 if floating-point addition rounds, but not in the
                 ieee style					
             2 if floating-point addition rounds in the ieee style
             3 if floating-point addition chops, and there is
                 partial underflow				
             4 if floating-point addition rounds, but not in the
                 ieee style, and there is partial underflow
             5 if floating-point addition rounds in the ieee style,
                 and there is partial underflow
   machep    the largest negative integer such that
                 1.0+float(ibeta)**machep .ne. 1.0, except that
                 machep is bounded below by  -(it+3)
   negeps    the largest negative integer such that
                 1.0-float(ibeta)**negeps .ne. 1.0, except that
                 negeps is bounded below by  -(it+3)	

 ***********************************************************************/

void machar(long *ibeta, long *it, long *irnd, long *machep, long *negep) {

  volatile double beta, betain, betah, a, b, ZERO, ONE, TWO, temp, tempa,
    temp1;
  long i, itemp;

  ONE = (double) 1;
  TWO = ONE + ONE;
  ZERO = ONE - ONE;

  a = ONE;
  temp1 = ONE;
  while (temp1 - ONE == ZERO) {
    a = a + a;
    temp = a + ONE;
    temp1 = temp - a;
    b += a; /* to prevent icc compiler error */
  }
  b = ONE;
  itemp = 0;
  while (itemp == 0) {
    b = b + b;
    temp = a + b;
    itemp = (long)(temp - a);
  }
  *ibeta = itemp;
  beta = (double) *ibeta;

  *it = 0;
  b = ONE;
  temp1 = ONE;
  while (temp1 - ONE == ZERO) {
    *it = *it + 1;
    b = b * beta;
    temp = b + ONE;
    temp1 = temp - b;
  }
  *irnd = 0;
  betah = beta / TWO;
  temp = a + betah;
  if (temp - a != ZERO) *irnd = 1;
  tempa = a + beta;
  temp = tempa + betah;
  if ((*irnd == 0) && (temp - tempa != ZERO)) *irnd = 2;

  *negep = *it + 3;
  betain = ONE / beta;
  a = ONE;
  for (i = 0; i < *negep; i++) a = a * betain;
  b = a;
  temp = ONE - a;
  while (temp-ONE == ZERO) {
    a = a * beta;
    *negep = *negep - 1;
    temp = ONE - a;
  }
  *negep = -(*negep);

  *machep = -(*it) - 3;
  a = b;
  temp = ONE + a;
  while (temp - ONE == ZERO) {
    a = a * beta;
    *machep = *machep + 1;
    temp = ONE + a;
  }
  eps = a;
  return;
}

/***********************************************************************
 *                                                                     *
 *                     store()                                         *
 *                                                                     *
 ***********************************************************************/
/***********************************************************************

   Description
   -----------

   store() is a user-supplied function which, based on the input
   operation flag, stores to or retrieves from memory a vector.


   Arguments
   ---------

   (input)
   n       length of vector to be stored or retrieved
   isw     operation flag:
	     isw = 1 request to store j-th Lanczos vector q(j)
	     isw = 2 request to retrieve j-th Lanczos vector q(j)
	     isw = 3 request to store q(j) for j = 0 or 1
	     isw = 4 request to retrieve q(j) for j = 0 or 1
   s	   contains the vector to be stored for a "store" request

   (output)
   s	   contains the vector retrieved for a "retrieve" request

   Functions used
   --------------

   BLAS		svd_dcopy

 ***********************************************************************/

void store(long n, long isw, long j, double *s) {
  /* printf("called store %ld %ld\n", isw, j); */
  switch(isw) {
  case STORQ:
    if (!LanStore[j + MAXLL]) {
      if (!(LanStore[j + MAXLL] = svd_doubleArray(n, FALSE, "LanStore[j]")))
        svd_fatalError("svdLAS2: failed to allocate LanStore[%d]", j + MAXLL);
    }
    svd_dcopy(n, s, 1, LanStore[j + MAXLL], 1);
    break;
  case RETRQ:	
    if (!LanStore[j + MAXLL])
      svd_fatalError("svdLAS2: store (RETRQ) called on index %d (not allocated)",
                     j + MAXLL);
    svd_dcopy(n, LanStore[j + MAXLL], 1, s, 1);
    break;
  case STORP:	
    if (j >= MAXLL) {
      svd_error("svdLAS2: store (STORP) called with j >= MAXLL");
      break;
    }
    if (!LanStore[j]) {
      if (!(LanStore[j] = svd_doubleArray(n, FALSE, "LanStore[j]")))
        svd_fatalError("svdLAS2: failed to allocate LanStore[%d]", j);
    }
    svd_dcopy(n, s, 1, LanStore[j], 1);
    break;
  case RETRP:	
    if (j >= MAXLL) {
      svd_error("svdLAS2: store (RETRP) called with j >= MAXLL");
      break;
    }
    if (!LanStore[j])
      svd_fatalError("svdLAS2: store (RETRP) called on index %d (not allocated)",
                     j);
    svd_dcopy(n, LanStore[j], 1, s, 1);
    break;
  }
  return;
}

/******************************************************************************/
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
/******************************************************************************/

void AFNI_svdLAS2( int m, int n, double *a, double *s, double *u, double *v )
{
   DMat aD , uD , vD ; SMat aS ; SVDRec aSVD ;
   int ii , jj , dd ;

   if( a == NULL || s == NULL || m < 1 || n < 1 ) return ;
   if( u == NULL || v == NULL                   ) return ;

#if 0
fprintf(stderr,"enter AFNI_svdLAS2\n") ;
#endif

   /* copy input matrix into local dense matrix struct */

   aD = svdNewDMat(m,n) ;
   for( ii=0 ; ii < m ; ii++ )
     for( jj=0 ; jj < n ; jj++ ) aD->value[ii][jj] = a[ii+jj*m] ;

   /* convert dense matrix struct to sparse */

#if 0
fprintf(stderr," convert dense to sparse\n") ;
#endif
   aS = svdConvertDtoS(aD) ; svdFreeDMat(aD) ;

   /* carry out the SVD */

#if 0
fprintf(stderr," call svdLAS2A\n") ;
#endif
#ifdef USE_OMP
   if( omp_in_parallel() ){
#pragma omp critical
     { aSVD = svdLAS2A(aS,0)   ; svdFreeSMat(aS) ; }
   } else {
       aSVD = svdLAS2A(aS,0)   ; svdFreeSMat(aS) ;
   }
#else
   aSVD = svdLAS2A(aS,0)   ; svdFreeSMat(aS) ;
#endif

   /* unpack the results into the output spaces */

#if 0
fprintf(stderr," unpack results\n") ;
#endif
   dd = aSVD->d  ;
   uD = aSVD->Ut ;   /* Ut has dd rows, each m long */
   vD = aSVD->Vt ;   /* Vt has dd rows, each n long */

   for( jj=0 ; jj < n ; jj++ ){
     if( jj < dd ){
       s[jj] = aSVD->S[jj] ;
       for( ii=0 ; ii < m ; ii++ ) u[ii+jj*m] = uD->value[jj][ii] ;
       for( ii=0 ; ii < n ; ii++ ) v[ii+jj*n] = vD->value[jj][ii] ;
     } else {
       s[jj] = 0.0 ;
       for( ii=0 ; ii < m ; ii++ ) u[ii+jj*m] = 0.0 ;
       for( ii=0 ; ii < n ; ii++ ) v[ii+jj*n] = 0.0 ;
     }
   }

   svdFreeSVDRec(aSVD) ; return ;
}
#endif
