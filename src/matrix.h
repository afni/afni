/*
  This is the header file for matrix.c.

  File:     matrix.h
  Author:   B. Douglas Ward
  Date:     23 April 1997

*/


/*---------------------------------------------------------------------------*/
/*
  This software is Copyright 1997 by

            Medical College of Wisconsin
            8701 Watertown Plank Road
            Milwaukee, WI 53226

  License is granted to use this program for nonprofit research purposes only.
  It is specifically against the license to use this program for any clinical
  application. The Medical College of Wisconsin makes no warranty of usefulness
  of this program for any particular purpose.  The redistribution of this
  program for a fee, or the derivation of for-profit works from this program
  is not allowed.
*/


/*---------------------------------------------------------------------------*/
/*
  Define matrix and vector data structures.
*/


typedef struct matrix
{
  int      rows;
  int      cols;
  double ** elts;
}  matrix;


typedef struct vector
{
  int  dim;
  double * elts;
} vector;


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
  Manual entry of matrix data.
*/

void matrix_enter (matrix * m);


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


/*---------------------------------------------------------------------------*/
/*
  Extract p columns (specified by list) from matrix a.  Result is matrix b.
*/

void matrix_extract (matrix a, int p, int * list, matrix * b);


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
  Print contents of vector v.
*/

void vector_print (vector v);


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


/*---------------------------------------------------------------------------*/
/*
  Calculate dot product of vector a with vector b. 
*/

float vector_dot (vector a, vector b);


/*---------------------------------------------------------------------------*/


