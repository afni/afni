/*
  This file contains matrix and vector arithmetic routines.

  File:     matrix.c
  Author:   B. Douglas Ward
  Date:     23 April 1997

  Mod:      05 August 1997
            Changed print format for functions matrix_print and vector_print.

  Mod:      04 November 1997
            Changed initialization in function vector_create.
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
  Routine to print and error message and stop.
*/

void matrix_error (char * message)
{
  printf ("Matrix error: %s \n", message);
  exit (1);
}


/*---------------------------------------------------------------------------*/
/*
  Initialize matrix data structure.
*/

void matrix_initialize (matrix * m)
{
  m->rows = 0;
  m->cols = 0;
  m->elts = NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Destroy matrix data structure by deallocating memory.
*/

void matrix_destroy (matrix * m)
{
  int i, rows;


  if (m->elts != NULL)
    {
      rows = m->rows;
      for (i = 0;  i < rows;  i++)
	if (m->elts[i] != NULL)
	  {  free (m->elts[i]);   m->elts[i] = NULL; }
      free (m->elts); 
    }

  matrix_initialize (m);
}


/*---------------------------------------------------------------------------*/
/*
  Create matrix data structure by allocating memory and initializing values.
*/

void matrix_create (int rows, int cols, matrix * m)
{
  int i, j;


  matrix_destroy (m);

  if ((rows < 1) || (cols < 1))
    matrix_error ("Illegal dimensions for new matrix");

  m->rows = rows;
  m->cols = cols;

  m->elts = (double **) malloc (sizeof(double *) * rows);
  if (m->elts == NULL)
    matrix_error ("Memory allocation error");

  for (i = 0;  i < rows;  i++)
    {
      m->elts[i] = (double *) malloc (sizeof(double) * cols);
      if (m->elts[i] == NULL)
	matrix_error ("Memory allocation error");
    }

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      m->elts[i][j] = 0.0;
}


/*---------------------------------------------------------------------------*/
/*
  Print contents of matrix m.
*/

void matrix_print (matrix m)
{
  int i, j;
  int rows, cols;

  rows = m.rows;
  cols = m.cols;

  for (i = 0;  i < rows;  i++)
    {
      for (j = 0;  j < cols;  j++)
	printf ("%10.4f", m.elts[i][j]);
      printf (" \n");
    }
  printf (" \n");
}


/*---------------------------------------------------------------------------*/
/*
  Manual entry of matrix data.
*/

void matrix_enter (matrix * m)
{
  int rows, cols;
  int i, j;
  float fval;

  printf ("Enter number of rows: ");
  scanf ("%d", &rows);
  printf ("Enter number of cols: ");
  scanf ("%d", &cols);

  matrix_create (rows, cols, m);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      {
	printf ("elts[%d][%d] = ", i, j);
	scanf ("%f", &fval);
	m->elts[i][j] = fval;
      }
}


/*---------------------------------------------------------------------------*/
/*
  Convert simple array to matrix structure.
*/

void array_to_matrix (int rows, int cols, float ** f, matrix * m)
{
  int i, j;

  matrix_create (rows, cols, m);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      m->elts[i][j] = f[i][j];
}


/*---------------------------------------------------------------------------*/
/*
  Make a copy of the first matrix, return copy as the second matrix.
*/

void matrix_equate (matrix a, matrix * b)
{
  int i, j;
  int rows, cols;

  rows = a.rows;
  cols = a.cols;

  matrix_create (rows, cols, b);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      b->elts[i][j] = a.elts[i][j];
}


/*---------------------------------------------------------------------------*/
/*
  Extract p columns (specified by list) from matrix a.  Result is matrix b.
*/

void matrix_extract (matrix a, int p, int * list, matrix * b)
{
  int i, j;
  int rows, cols;

  rows = a.rows;
  cols = p;

  matrix_create (rows, cols, b);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      b->elts[i][j] = a.elts[i][list[j]];
}


/*---------------------------------------------------------------------------*/
/*
  Create n x n identity matrix.
*/

void matrix_identity (int n, matrix * m)
{
  int i, j;

  if (n < 1)
    matrix_error ("Illegal dimensions for identity matrix");

  matrix_create (n, n, m);

  for (i = 0;  i < n;  i++)
    for (j = 0;  j < n;  j++)
      if (i == j)
	m->elts[i][j] = 1.0;
      else
	m->elts[i][j] = 0.0;
}


/*---------------------------------------------------------------------------*/
/*
  Add matrix a to matrix b.  Result is matrix c.
*/

void matrix_add (matrix a, matrix b, matrix * c)
{
  int rows, cols;
  int i, j;

  if ((a.rows != b.rows) || (a.cols != b.cols))
    matrix_error ("Incompatible dimensions for matrix addition");

  rows = a.rows;
  cols = a.cols;

  matrix_create (rows, cols, c);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      c->elts[i][j] = a.elts[i][j] + b.elts[i][j];
}


/*---------------------------------------------------------------------------*/
/*
  Subtract matrix b from matrix a.  Result is matrix c.
*/

void matrix_subtract (matrix a, matrix b, matrix * c)
{
  int rows, cols;
  int i, j;

  if ((a.rows != b.rows) || (a.cols != b.cols))
    matrix_error ("Incompatible dimensions for matrix subtraction");

  rows = a.rows;
  cols = a.cols;

  matrix_create (rows, cols, c);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      c->elts[i][j] = a.elts[i][j] - b.elts[i][j];
}


/*---------------------------------------------------------------------------*/
/*
  Multiply matrix a by matrix b.  Result is matrix c.
*/

void matrix_multiply (matrix a, matrix b, matrix * c)
{
  int rows, cols;
  int i, j, k;

  if (a.cols != b.rows)
    matrix_error ("Incompatible dimensions for matrix multiplication");

  rows = a.rows;
  cols = b.cols;

  matrix_create (rows, cols, c);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      {
	c->elts[i][j] = 0.0;
	for (k = 0;  k < a.cols;  k++)
	  c->elts[i][j] += a.elts[i][k] * b.elts[k][j];
      }
}


/*---------------------------------------------------------------------------*/
/*
  Multiply matrix a by scalar constant k.  Result is matrix c.
*/

void matrix_scale (double k, matrix a, matrix * c)
{
  int rows, cols;
  int i, j;


  rows = a.rows;
  cols = a.cols;

  matrix_create (rows, cols, c);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      c->elts[i][j] = k * a.elts[i][j];
}


/*---------------------------------------------------------------------------*/
/*
  Take transpose of matrix a.  Result is matrix t.
*/

void matrix_transpose (matrix a, matrix * t)
{
  int rows, cols;
  int i, j;

  rows = a.cols;
  cols = a.rows;

  matrix_create (rows, cols, t);
  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      t->elts[i][j] = a.elts[j][i];
}

 
/*---------------------------------------------------------------------------*/
/*
  Use Gaussian elimination to calculate inverse of matrix a.  Result is 
  matrix ainv.
*/

int matrix_inverse (matrix a, matrix * ainv)
{
  const double epsilon = 1.0e-10;
  matrix tmp;
  int i, j, ii, n;
  double fval;
  double fmax;
  double * p;

  matrix_initialize (&tmp);


  if (a.rows != a.cols) 
    matrix_error ("Illegal dimensions for matrix inversion");

  matrix_initialize (&tmp);

  n = a.rows;
  matrix_identity (n, ainv);
  matrix_equate (a, &tmp);

  for (i = 0;  i < n;  i++)
    {
      fmax = fabs(tmp.elts[i][i]);
      for (j = i+1;  j < n;  j++)
	if (fabs(tmp.elts[j][i]) > fmax)
	  {
	    fmax = fabs(tmp.elts[j][i]);
	    p = tmp.elts[i];
	    tmp.elts[i] = tmp.elts[j];
	    tmp.elts[j] = p;
	    p = ainv->elts[i];
	    ainv->elts[i] = ainv->elts[j];
	    ainv->elts[j] = p;
	  }

      if (fmax < epsilon)
	{
	  matrix_destroy (&tmp);
	  return (0);
	}
	

      fval = tmp.elts[i][i];
      for (j = 0;  j < n;  j++)
	{
	  tmp.elts[i][j] /= fval;
	  ainv->elts[i][j] /= fval;
	}
      for (ii = 0;  ii < n;  ii++)
	if (ii != i)
	  {
	    fval = tmp.elts[ii][i];
	    for (j = 0;  j < n;  j++)
	      {
		tmp.elts[ii][j] -= fval*tmp.elts[i][j];
		ainv->elts[ii][j] -= fval*ainv->elts[i][j];
	      }
	  }
	
    }
  matrix_destroy (&tmp);
  return (1);
}


/*---------------------------------------------------------------------------*/
/*
  Initialize vector data structure.
*/

void vector_initialize (vector * v)
{
  v->dim = 0;
  v->elts = NULL;
}


/*---------------------------------------------------------------------------*/
/*
  Destroy vector data structure by deallocating memory.
*/

void vector_destroy (vector * v)
{
  if (v->elts != NULL)  free (v->elts);
  vector_initialize (v);
}


/*---------------------------------------------------------------------------*/
/*
  Create vector v by allocating memory and initializing values.
*/

void vector_create (int dim, vector * v)
{
  int i;

  vector_destroy (v);
  
  if (dim < 1)  matrix_error ("Illegal dimensions for new vector");

  v->dim = dim;
  v->elts = (double *) malloc (sizeof(double) * dim);
  if (v->elts == NULL)
    matrix_error ("Memory allocation error");

  for (i = 0;  i < dim;  i++)
     v->elts[i] = 0.0;
}


/*---------------------------------------------------------------------------*/
/*
  Print contents of vector v.
*/

void vector_print (vector v)
{
  int i;

  for (i = 0;  i < v.dim;  i++)
    printf ("  %10.4f \n", v.elts[i]);
  printf (" \n");
    
}


/*---------------------------------------------------------------------------*/
/*
  Copy vector a.  Result is vector b.
*/

void vector_equate (vector a, vector * b)
{
  int i, dim;

  dim = a.dim;

  vector_create (dim, b);

  for (i = 0;  i < dim;  i++)
    b->elts[i] = a.elts[i];
}


/*---------------------------------------------------------------------------*/
/*
  Convert simple array f into vector v.
*/

void array_to_vector (int dim, float * f, vector * v)
{
  int i;

  vector_create (dim, v);

  for (i = 0;  i < dim;  i++)
    v->elts[i] = f[i];
}



/*---------------------------------------------------------------------------*/
/*
  Convert vector v into array f.
*/

void vector_to_array (vector v, float * f)
{
  int i;
  
  for (i = 0;  i < v.dim;  i++)
    f[i] = v.elts[i];
}


/*---------------------------------------------------------------------------*/
/*
  Add vector a to vector b.  Result is vector c.
*/

void vector_add (vector a, vector b, vector * c)
{
  int i, dim;

  if (a.dim != b.dim)
    matrix_error ("Incompatible dimensions for vector addition");

  dim = a.dim;

  vector_create (dim, c);

  for (i = 0;  i < dim;  i++)
    c->elts[i] = a.elts[i] + b.elts[i];
}


/*---------------------------------------------------------------------------*/
/*
  Subtract vector b from vector a.  Result is vector c.
*/

void vector_subtract (vector a, vector b, vector * c)
{
  int i, dim;

  if (a.dim != b.dim)
    matrix_error ("Incompatible dimensions for vector subtraction");

  dim = a.dim;

  vector_create (dim, c);

  for (i = 0;  i < dim;  i++)
    c->elts[i] = a.elts[i] - b.elts[i];
}


/*---------------------------------------------------------------------------*/
/*
  Right multiply matrix a by vector b.  Result is vector c.
*/

void vector_multiply (matrix a, vector b, vector * c)
{
  int rows, cols;
  int i, j;

  if (a.cols != b.dim)
    matrix_error ("Incompatible dimensions for vector multiplication");

  rows = a.rows;
  cols = a.cols;

  vector_create (rows, c);

  for (i = 0;  i < rows;  i++)
    {
      c->elts[i] = 0.0;
      for (j = 0;  j < cols;  j++)
	c->elts[i] += a.elts[i][j] * b.elts[j];
    }
}


/*---------------------------------------------------------------------------*/
/*
  Calculate dot product of vector a with vector b. 
*/

float vector_dot (vector a, vector b)
{
  int i, dim;
  float sum;

  if (a.dim != b.dim)
    matrix_error ("Incompatible dimensions for vector dot product");

  dim = a.dim;

  sum = 0.0;
  for (i = 0;  i < dim;  i++)
    sum += a.elts[i] * b.elts[i];

  return (sum);
}

/*---------------------------------------------------------------------------*/
