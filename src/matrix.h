#ifndef MATRIX_INCLUDED
#define MATRIX_INCLUDED

/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*
  This is the header file for matrix.c.

  File:     matrix.h
  Author:   B. Douglas Ward
  Date:     23 April 1997

  Mod:      Added routines matrix_file_write and matrix_file_read.
  Date:     02 July 1999

  Mod:      Added routine for calculating square root of matrix.
  Date:     30 September 1999

  Mod:      Added routines matrix_sprint and vector_sprint.
  Date:     04 October 1999

  Mod:      Modified matrix_file_read to use mri_read_ascii routine.
  Date:     12 January 2000

  Mod:      Changed return type of vector_dot from float to double.
  Date:     13 April 2000

  Mod:      Added functions column_to_vector and matrix_extract_rows.
  Date:     21 April 2000

  Mod:      Added functions vector_dotself() and vector_multiply_subtract() -- RWCox.
  Date:     28 Dec 2002

  Mod:      Use one array instead of array of arrays for matrix -- RWCox.
  Date:     04 Mar 2005

*/

/*---------------------------------------------------------------------------*/
/*
  Define matrix and vector data structures.
*/

#include "machdep.h"  /* 07 Mar 2005: to get DONT_USE_MATRIX_MAT */

typedef struct matrix
{
  int      rows;
  int      cols;
  double ** elts;
#ifndef DONT_USE_MATRIX_MAT
  double  * mat ;  /* 04 Mar 2005 */
#endif
}  matrix;


typedef struct vector
{
  int  dim;
  double * elts;
} vector;

#undef  ISVALID_MATRIX
#define ISVALID_MATRIX(m) ((m).rows > 0 && (m).cols > 0 && (m).elts != NULL)

#undef  ISVALID_VECTOR
#define ISVALID_VECTOR(v) ((v).dim > 0 && (v).elts != NULL)

/*---------------------------------------------------------------------------*/
/*
  Routine to print an error message and stop.
*/

void matrix_error (char * message);


/*---------------------------------------------------------------------------*/
/*
  Initialize matrix data structure.
*/

void matrix_initialize (matrix * m);


/*---------------------------------------------------------------------------*/
/*
  Destroy matrix data structure by deallocating memory.
*/

void matrix_destroy (matrix * m);


/*---------------------------------------------------------------------------*/
/*
  Create matrix data structure by allocating memory and initializing values.
*/

void matrix_create (int rows, int cols, matrix * m);


/*---------------------------------------------------------------------------*/
/*
  Print contents of matrix m.
*/

void matrix_print (matrix m);


/*---------------------------------------------------------------------------*/
/*
  Print label and contents of matrix m.
*/

void matrix_sprint (char * s, matrix m);


/*---------------------------------------------------------------------------*/
/*
  Print contents of matrix m to specified file.
*/

void matrix_file_write (char * filename, matrix m);


/*---------------------------------------------------------------------------*/
/*
  Manual entry of matrix data.
*/

void matrix_enter (matrix * m);


/*---------------------------------------------------------------------------*/
/*
  Read contents of matrix m from specified file.
  If unable to read matrix from file, or matrix has wrong dimensions:
     If error_exit flag is set, then print error message and exit.
     Otherwise, return null matrix.
*/

void matrix_file_read (char * filename, int rows, int cols,  matrix * m,
                       int error_exit);


/*---------------------------------------------------------------------------*/
/*
  Convert simple array to matrix structure.
*/

void array_to_matrix (int rows, int cols, float ** f, matrix * m);


/*---------------------------------------------------------------------------*/
/*
  Make a copy of the first matrix, return copy as the second matrix.
*/

void matrix_equate (matrix a, matrix * b);
void matrix_enlarge( int nradd , int ncadd , matrix *a )  ;


/*---------------------------------------------------------------------------*/
/*
  Extract p columns (specified by list) from matrix a.  Result is matrix b.
*/

void matrix_extract (matrix a, int p, int * list, matrix * b);

#define matrix_extract_cols matrix_extract


/*---------------------------------------------------------------------------*/
/*
  Extract p rows (specified by list) from matrix a.  Result is matrix b.
*/

void matrix_extract_rows (matrix a, int p, int * list, matrix * b);


/*---------------------------------------------------------------------------*/
/*
  Create n x n identity matrix.
*/

void matrix_identity (int n, matrix * m);


/*---------------------------------------------------------------------------*/
/*
  Add matrix a to matrix b.  Result is matrix c.
*/

void matrix_add (matrix a, matrix b, matrix * c);


/*---------------------------------------------------------------------------*/
/*
  Subtract matrix b from matrix a.  Result is matrix c.
*/

void matrix_subtract (matrix a, matrix b, matrix * c);


/*---------------------------------------------------------------------------*/
/*
  Multiply matrix a by matrix b.  Result is matrix c.
*/

void matrix_multiply (matrix a, matrix b, matrix * c);


/*---------------------------------------------------------------------------*/
/*
  Multiply matrix a by scalar constant k.  Result is matrix c.
*/

void matrix_scale (double k, matrix a, matrix * c);


/*---------------------------------------------------------------------------*/
/*
  Take transpose of matrix a.  Result is matrix t.
*/

void matrix_transpose (matrix a, matrix * t);


/*---------------------------------------------------------------------------*/
/*
  Use Gaussian elimination to calculate inverse of matrix a.  Result is
  matrix ainv.
*/

int matrix_inverse (matrix a, matrix * ainv);

int matrix_inverse_dsc (matrix a, matrix * ainv);  /* 15 Jul 2004 */


/*---------------------------------------------------------------------------*/
/*
  Calculate square root of symmetric positive definite matrix a.
  Result is matrix s.
*/

int matrix_sqrt (matrix a, matrix * s);


/*---------------------------------------------------------------------------*/
/*
  Initialize vector data structure.
*/

void vector_initialize (vector * v);


/*---------------------------------------------------------------------------*/
/*
  Destroy vector data structure by deallocating memory.
*/

void vector_destroy (vector * v);


/*---------------------------------------------------------------------------*/
/*
  Create vector v by allocating memory and initializing values.
*/

void vector_create (int dim, vector * v);

/*---------------------------------------------------------------------------*/
/*
  Create vector v by allocating memory, but do not initialize values.
*/
void vector_create_noinit(int dim, vector * v);


/*---------------------------------------------------------------------------*/
/*
  Print contents of vector v.
*/

void vector_print (vector v);


/*---------------------------------------------------------------------------*/
/*
  Print label and contents of vector v.
*/

void vector_sprint (char * s, vector v);


/*---------------------------------------------------------------------------*/
/*
  Copy vector a.  Result is vector b.
*/

void vector_equate (vector a, vector * b);


/*---------------------------------------------------------------------------*/
/*
  Convert simple array f into vector v.
*/

void array_to_vector (int dim, float * f, vector * v);


/*---------------------------------------------------------------------------*/
/*
  Convert column c of matrix m into vector v.
*/

void column_to_vector (matrix m, int c, vector * v);
void row_to_vector    (matrix m, int r, vector * v);


/*---------------------------------------------------------------------------*/
/*
  Convert vector v into array f.
*/

void vector_to_array (vector v, float * f);


/*---------------------------------------------------------------------------*/
/*
  Add vector a to vector b.  Result is vector c.
*/

void vector_add (vector a, vector b, vector * c);


/*---------------------------------------------------------------------------*/
/*
  Subtract vector b from vector a.  Result is vector c.
*/

void vector_subtract (vector a, vector b, vector * c);


/*---------------------------------------------------------------------------*/
/*
  Right multiply matrix a by vector b.  Result is vector c.
*/

void vector_multiply (matrix a, vector b, vector * c);
void vector_multiply_transpose (matrix a, vector b, vector * c);

/*---------------------------------------------------------------------------*/
/*
  Right multiply matrix a by vector b, then subtract c.  Result is vector d.
  Also returns sum of squares of elements of d.
*/

double vector_multiply_subtract (matrix a, vector b, vector c, vector * d) ;

/*---------------------------------------------------------------------------*/
/*
  Calculate dot product of vector a with vector b.
*/

double vector_dot (vector a, vector b);

double vector_dotself (vector a);  /* 28 Dec 2002: RWCox */

/*---------------------------------------------------------------------------*/

double matrix_norm     ( matrix a ) ;              /* 03 Mar 2003: RWCox */
double matrix_frobenius( matrix a ) ;              /* 30 Jul 2008 */
void matrix_colsqsums  ( matrix a , vector *v ) ;  /* 30 Jul 2008 */

int * matrix_check_columns( matrix a , double eps ) ; /* 14 Jul 2004: RWCox */

double * matrix_singvals( matrix X ) ; /* 14 Jul 2004 */

void matrix_psinv( matrix X , matrix *XtXinv , matrix *XtXinvXt ) ;  /* 19 Jul 2004 */

extern int matrix_collinearity_fixup( matrix X , matrix *Xa ) ; /* 12 Dec 2008 */

extern void matrix_psinv_seteps( double eps ) ; /* 02 Mar 2007 - MoJM */

extern void matrix_allow_desing( int ) ;          /* 15 Mar 2010 */

extern int  matrix_qrr( matrix X , matrix *R ) ;  /* 03 Jul 2008 */
extern void vector_rr_solve    ( matrix R , vector b , vector *x ) ;
extern void vector_rrtran_solve( matrix R , vector b , vector *x ) ;
extern void matrix_rr_solve    ( matrix R , matrix B , matrix *X ) ;
extern void matrix_rrtran_solve( matrix R , matrix B , matrix *X ) ;

extern double get_matrix_flops(void) ;
extern double get_matrix_dotlen(void) ;

#endif
