/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*!
  This file contains matrix and vector arithmetic routines.

  File:     matrix.c
  Author:   B. Douglas Ward
  Date:     23 April 1997

  Mod:      Changed print format for functions matrix_print and vector_print.
  Date:     05 August 1997

  Mod:      Changed initialization in function vector_create.
  Date:     04 November 1997

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

  Mod:      Added test for missing matrix file name.
  Date:     08 May 2000

  Mod:      Added "register" declarations and a few other things to speed
            up calculations (including vector_create_noinit) -- RWCox.
  Date:     28 Dec 2001

  Mod:      Allow matrices and vectors with zero rows and columns.
  Date:     26 February 2002

  Mod:      Corrected errors in vector_multiply and vector_multiply_subtract
            routines that would produce segmentation fault for certain input
	    matrices.
  Date:     18 March 2003

  Mod:      UNROLL_VECMUL defined to allow unrolling by 2 of vector-multiply
            dot product loops.

  Mod:      'ipr' added to matrix_print() function.
  Date:     03 Aug 2004 - RWCox
*/


static double flops=0.0l ;
double get_matrix_flops(void){ return flops; }

/*---------------------------------------------------------------------------*/
/*!
  Routine to print and error message and stop.
*/

void matrix_error (char * message)
{
  printf ("Matrix error: %s \n", message);
  exit (1);
}


/*---------------------------------------------------------------------------*/
/*!
  Initialize matrix data structure.
*/

void matrix_initialize (matrix * m)
{
  m->rows = 0;
  m->cols = 0;
  m->elts = NULL;
}


/*---------------------------------------------------------------------------*/
/*!
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
/*!
  Create matrix data structure by allocating memory and initializing values.
*/

void matrix_create (int rows, int cols, matrix * m)
{
  register int i, j;

  matrix_destroy (m);

  if ((rows < 0) || (cols < 0))
    matrix_error ("Illegal dimensions for new matrix");

  m->rows = rows;
  m->cols = cols;
  if ((rows < 1) || (cols < 1))  return;

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
/*!
  Print contents of matrix m.
*/

void matrix_print (matrix m)
{
  int i, j;
  int rows, cols;
  double val ;
  int ipr ;

  rows = m.rows;
  cols = m.cols;

  for( i=0 ; i < rows ; i++ ){
    for( j=0 ; j < cols ; j++ ){
      val = (int)m.elts[i][j] ;
      if( val != m.elts[i][j] || fabs(val) > 9.0l ) goto zork ;
    }
  }
zork:
  ipr = (i==rows && j==cols) ;

  for (i = 0;  i < rows;  i++)
    {
      for (j = 0;  j < cols;  j++)
        if( ipr ) printf (" %2d"   , (int)m.elts[i][j]);
        else      printf (" %10.4g", m.elts[i][j]);
      printf (" \n");
    }
  printf (" \n");
}


/*---------------------------------------------------------------------------*/
/*!
  Print label and contents of matrix m.
*/

void matrix_sprint (char * s, matrix m)
{
  printf ("%s \n", s);

  matrix_print (m);
}


/*---------------------------------------------------------------------------*/
/*!
  Print contents of matrix m to specified file.
*/

void matrix_file_write (char * filename, matrix m)
{
  int i, j;
  int rows, cols;
  FILE * outfile = NULL;


  /*----- First, check for empty file name -----*/
  if (filename == NULL)  matrix_error ("Missing matrix file name");


  outfile = fopen (filename, "w");

  rows = m.rows;
  cols = m.cols;

  for (i = 0;  i < rows;  i++)
    {
      for (j = 0;  j < cols;  j++)
        fprintf (outfile, "  %g", m.elts[i][j]);
      fprintf (outfile, " \n");
    }
  fprintf (outfile, " \n");

  fclose (outfile);
}

/*---------------------------------------------------------------------------*/
/*!
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
/*!
  Read contents of matrix m from specified file.
  If unable to read matrix from file, or matrix has wrong dimensions:
     If error_exit flag is set, then print error message and exit.
     Otherwise, return null matrix.
*/

void matrix_file_read (char * filename, int rows, int cols,  matrix * m,
		       int error_exit)
{
  int i, j;

  MRI_IMAGE * im, * flim;  /* pointers to image structures
			      -- used to read ASCII file */
  float * far;             /* pointer to MRI_IMAGE floating point data */
  char message [80];       /* error message */


  /*----- First, check for empty file name -----*/
  if (filename == NULL)  matrix_error ("Missing matrix file name");


  /*----- Read the matrix file -----*/
  flim = mri_read_1D(filename);
  if (flim == NULL)
    if (error_exit)
      {
	sprintf (message,  "Unable to read matrix from file: %s",  filename);
	matrix_error (message);
      }
    else
      {
	matrix_destroy (m);
	return;
      }


  /*----- Set pointer to data  -----*/
  far = MRI_FLOAT_PTR(flim);


  /*----- Test for correct dimensions -----*/
  if ( (rows != flim->nx) || (cols != flim->ny) )
    if (error_exit)
      {
	sprintf (message,
		 "In matrix file: %s   Expected: %d x %d   Actual: %d x %d",
		 filename, rows, cols, flim->nx, flim->ny);
	matrix_error (message);
      }
    else
      {
	matrix_destroy (m);
	return;
      }


  matrix_create (rows, cols, m);


  /*----- Copy data from image structure to matrix structure -----*/
  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      m->elts[i][j] = far[i + j*rows];


  mri_free (flim);  flim = NULL;

}


/*---------------------------------------------------------------------------*/
/*!
  Convert simple array to matrix structure.
*/

void array_to_matrix (int rows, int cols, float ** f, matrix * m)
{
  register int i, j;

  matrix_create (rows, cols, m);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      m->elts[i][j] = f[i][j];
}


/*---------------------------------------------------------------------------*/
/*!
  Make a copy of the first matrix, return copy as the second matrix.
*/

void matrix_equate (matrix a, matrix * b)
{
  register int i, j;
  register int rows, cols;

  rows = a.rows;
  cols = a.cols;

  matrix_create (rows, cols, b);

  for (i = 0;  i < rows;  i++){
#if 0
    for (j = 0;  j < cols;  j++)
      b->elts[i][j] = a.elts[i][j];
#else
    if( cols > 0 )
      memcpy( b->elts[i] , a.elts[i] , sizeof(double)*cols ) ;  /* RWCox */
#endif
  }
}


/*---------------------------------------------------------------------------*/
/*!
  Extract p columns (specified by list) from matrix a.  Result is matrix b.
*/

void matrix_extract (matrix a, int p, int * list, matrix * b)
{
  register int i, j;
  register int rows, cols;

  rows = a.rows;
  cols = p;

  matrix_create (rows, cols, b);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      b->elts[i][j] = a.elts[i][list[j]];
}


/*---------------------------------------------------------------------------*/
/*!
  Extract p rows (specified by list) from matrix a.  Result is matrix b.
*/

void matrix_extract_rows (matrix a, int p, int * list, matrix * b)
{
  register int i, j;
  register int rows, cols;

  rows = p;
  cols = a.cols;

  matrix_create (rows, cols, b);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      b->elts[i][j] = a.elts[list[i]][j];
}


/*---------------------------------------------------------------------------*/
/*!
  Create n x n identity matrix.
*/

void matrix_identity (int n, matrix * m)
{
  register int i, j;

  if (n < 0)
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
/*!
  Add matrix a to matrix b.  Result is matrix c.
*/

void matrix_add (matrix a, matrix b, matrix * c)
{
  register int rows, cols;
  register int i, j;

  if ((a.rows != b.rows) || (a.cols != b.cols))
    matrix_error ("Incompatible dimensions for matrix addition");

  rows = a.rows;
  cols = a.cols;

  matrix_create (rows, cols, c);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      c->elts[i][j] = a.elts[i][j] + b.elts[i][j];

  flops += rows*cols ;
}


/*---------------------------------------------------------------------------*/
/*!
  Subtract matrix b from matrix a.  Result is matrix c.
*/

void matrix_subtract (matrix a, matrix b, matrix * c)
{
  register int rows, cols;
  register int i, j;

  if ((a.rows != b.rows) || (a.cols != b.cols))
    matrix_error ("Incompatible dimensions for matrix subtraction");

  rows = a.rows;
  cols = a.cols;

  matrix_create (rows, cols, c);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      c->elts[i][j] = a.elts[i][j] - b.elts[i][j];

  flops += rows*cols ;
}


/*---------------------------------------------------------------------------*/
/*!
  Multiply matrix a by matrix b.  Result is matrix c.
*/

void matrix_multiply (matrix a, matrix b, matrix * c)
{
  int rows, cols;
  register int i, j, k;
  register double sum ;

  if (a.cols != b.rows)
    matrix_error ("Incompatible dimensions for matrix multiplication");

  rows = a.rows;
  cols = b.cols;

  matrix_create (rows, cols, c);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      {
        sum = 0.0 ;
	for (k = 0;  k < a.cols;  k++)
	  sum += a.elts[i][k] * b.elts[k][j];
        c->elts[i][j] = sum;
      }

  flops += 2.0l*rows*cols*cols ;
}


/*---------------------------------------------------------------------------*/
/*!
  Multiply matrix a by scalar constant k.  Result is matrix c.
*/

void matrix_scale (double k, matrix a, matrix * c)
{
  register int rows, cols;
  register int i, j;

  rows = a.rows;
  cols = a.cols;

  matrix_create (rows, cols, c);

  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      c->elts[i][j] = k * a.elts[i][j];

  flops += rows*cols ;
}


/*---------------------------------------------------------------------------*/
/*!
  Take transpose of matrix a.  Result is matrix t.
*/

void matrix_transpose (matrix a, matrix * t)
{
  register int rows, cols;
  register int i, j;

  rows = a.cols;
  cols = a.rows;

  matrix_create (rows, cols, t);
  for (i = 0;  i < rows;  i++)
    for (j = 0;  j < cols;  j++)
      t->elts[i][j] = a.elts[j][i];
}

/*---------------------------------------------------------------------------*/
/*!
  Use Gaussian elimination to calculate inverse of matrix a.  Result is
  matrix ainv.
*/

int matrix_inverse (matrix a, matrix * ainv)
{
  const double epsilon = 1.0e-10;
  matrix tmp;
  register int i, j, ii, n;
  register double fval;
  register double fmax;
  register double * p;

  matrix_initialize (&tmp);


  if (a.rows != a.cols)
    matrix_error ("Illegal dimensions for matrix inversion");

#if 0
matrix_sprint("matrix_inverse:",a) ;
#endif

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
	

      fval = 1.0 / tmp.elts[i][i];   /* RWCox: change division by this to */
      for (j = 0;  j < n;  j++)      /*        multiplication by 1.0/this */
	{
	  tmp.elts[i][j]   *= fval;
	  ainv->elts[i][j] *= fval;
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
  flops += 3.0l*n*n*n ;
  return (1);
}


/*---------------------------------------------------------------------------*/
/*!
  Use Gaussian elimination to calculate inverse of matrix a, with diagonal
  scaling applied for stability.  Result is matrix ainv.
*/

int matrix_inverse_dsc (matrix a, matrix * ainv)  /* 15 Jul 2004 - RWCox */
{
  matrix atmp;
  register int i, j, n;
  register double *diag ;
  int mir ;

  if (a.rows != a.cols)
    matrix_error ("Illegal dimensions for matrix inversion");

  matrix_initialize (&atmp);

  n = a.rows;
  matrix_equate (a, &atmp);
  diag = (double *)malloc( sizeof(double)*n ) ;
  for( i=0 ; i < n ; i++ ){
    diag[i] = fabs(atmp.elts[i][i]) ;
    if( diag[i] == 0.0l ) diag[i] = 1.0l ;  /* shouldn't happen? */
    diag[i] = 1.0l / sqrt(diag[i]) ;
  }

  for( i=0 ; i < n ; i++ )                /* scale a */
   for( j=0 ; j < n ; j++ )
    atmp.elts[i][j] *= diag[i]*diag[j] ;

  mir = matrix_inverse( atmp , ainv ) ;   /* invert */

  for( i=0 ; i < n ; i++ )                /* scale inverse */
   for( j=0 ; j < n ; j++ )
    ainv->elts[i][j] *= diag[i]*diag[j] ;

  matrix_destroy (&atmp); free((void *)diag) ;
  flops += 4.0l*n*n + 4.0l*n ;
  return (mir);
}


/*---------------------------------------------------------------------------*/
/*!
  Calculate square root of symmetric positive definite matrix a.
  Result is matrix s.
*/

int matrix_sqrt (matrix a, matrix * s)
{
  const int MAX_ITER = 100;
  int n;
  int ok;
  int iter;
  register float sse, psse;
  register int i, j;
  matrix x, xinv, axinv, xtemp, error;

  matrix_initialize (&x);
  matrix_initialize (&xinv);
  matrix_initialize (&axinv);
  matrix_initialize (&xtemp);
  matrix_initialize (&error);


  if (a.rows != a.cols)
    matrix_error ("Illegal dimensions for matrix square root");


  n = a.rows;
  matrix_identity (n, &x);


  psse = 1.0e+30;
  for (iter = 0;  iter < MAX_ITER;  iter++)
    {
      ok = matrix_inverse (x, &xinv);
      if (! ok)  return (0);
      matrix_multiply (a, xinv, &axinv);
      matrix_add (x, axinv, &xtemp);
      matrix_scale (0.5, xtemp, &x);

      matrix_multiply (x, x, &xtemp);
      matrix_subtract (a, xtemp, &error);
      sse = 0.0;
      for (i = 0;  i < n;  i++)
	for (j = 0;  j < n;  j++)
	  sse += error.elts[i][j] * error.elts[i][j] ;

      if (sse >= psse) break;

      psse = sse;
    }

  if (iter == MAX_ITER)  return (0);

  matrix_equate (x, s);

  matrix_destroy (&x);
  matrix_destroy (&xinv);
  matrix_destroy (&axinv);
  matrix_destroy (&xtemp);

  return (1);

}


/*---------------------------------------------------------------------------*/
/*!
  Initialize vector data structure.
*/

void vector_initialize (vector * v)
{
  v->dim = 0;
  v->elts = NULL;
}


/*---------------------------------------------------------------------------*/
/*!
  Destroy vector data structure by deallocating memory.
*/

void vector_destroy (vector * v)
{
  if (v->elts != NULL)  free (v->elts);
  vector_initialize (v);
}


/*---------------------------------------------------------------------------*/
/*!
  Create vector v by allocating memory and initializing values.
*/

void vector_create (int dim, vector * v)
{
  register int i;

  vector_destroy (v);

  if (dim < 0)  matrix_error ("Illegal dimensions for new vector");

  v->dim = dim;
  if (dim < 1)  return;

  v->elts = (double *) malloc (sizeof(double) * dim);
  if (v->elts == NULL)
    matrix_error ("Memory allocation error");

#if 0
  for (i = 0;  i < dim;  i++)
     v->elts[i] = 0.0;
#else
  memset( v->elts , 0 , sizeof(double)*dim ) ;
#endif
}

/*---------------------------------------------------------------------------*/
static void vector_create_noinit(int dim, vector * v)  /* 28 Dec 2001: RWCox */
{
  register int i;

  vector_destroy (v);

  if (dim < 0)  matrix_error ("Illegal dimensions for new vector");

  v->dim = dim;
  if (dim < 1)  return;

  v->elts = (double *) malloc (sizeof(double) * dim);
  if (v->elts == NULL)
    matrix_error ("Memory allocation error");
}

/*---------------------------------------------------------------------------*/
/*!
  Print contents of vector v.
*/

void vector_print (vector v)
{
  int i;

  for (i = 0;  i < v.dim;  i++)
    printf ("  %10.4g \n", v.elts[i]);
  printf (" \n");

}


/*---------------------------------------------------------------------------*/
/*!
  Print label and contents of vector v.
*/

void vector_sprint (char * s, vector v)
{
  printf ("%s \n", s);

  vector_print (v);
}


/*---------------------------------------------------------------------------*/
/*!
  Copy vector a.  Result is vector b.
*/

void vector_equate (vector a, vector * b)
{
  register int i, dim;

  dim = a.dim;

  vector_create_noinit (dim, b);

#if 0
  for (i = 0;  i < dim;  i++)
    b->elts[i] = a.elts[i];
#else
  if( dim > 0 )
    memcpy( b->elts , a.elts , sizeof(double)*dim ) ;  /* RWCox */
#endif
}


/*---------------------------------------------------------------------------*/
/*!
  Convert simple array f into vector v.
*/

void array_to_vector (int dim, float * f, vector * v)
{
  register int i;

  vector_create_noinit (dim, v);

  for (i = 0;  i < dim;  i++)
    v->elts[i] = f[i];
}



/*---------------------------------------------------------------------------*/
/*!
  Convert column c of matrix m into vector v.
*/

void column_to_vector (matrix m, int c, vector * v)
{
  register int i;
  register int dim;

  dim = m.rows;
  vector_create_noinit (dim, v);

  for (i = 0;  i < dim;  i++)
    v->elts[i] = m.elts[i][c];
}



/*---------------------------------------------------------------------------*/
/*!
  Convert vector v into array f.
*/

void vector_to_array (vector v, float * f)
{
  register int i;

  for (i = 0;  i < v.dim;  i++)
    f[i] = v.elts[i];
}


/*---------------------------------------------------------------------------*/
/*!
  Add vector a to vector b.  Result is vector c.
*/

void vector_add (vector a, vector b, vector * c)
{
  register int i, dim;

  if (a.dim != b.dim)
    matrix_error ("Incompatible dimensions for vector addition");

  dim = a.dim;

  vector_create_noinit (dim, c);

  for (i = 0;  i < dim;  i++)
    c->elts[i] = a.elts[i] + b.elts[i];

  flops += dim ;
}


/*---------------------------------------------------------------------------*/
/*!
  Subtract vector b from vector a.  Result is vector c.
*/

void vector_subtract (vector a, vector b, vector * c)
{
  register int i, dim;
  register double *aa,*bb,*cc ;

  if (a.dim != b.dim)
    matrix_error ("Incompatible dimensions for vector subtraction");

  dim = a.dim;

  vector_create_noinit (dim, c);

  aa = a.elts ; bb = b.elts ; cc = c->elts ;
  for (i = 0;  i < dim;  i++)
#if 0
    c->elts[i] = a.elts[i] - b.elts[i];
#else
    cc[i] = aa[i] - bb[i] ;
#endif

  flops += dim ;
}

#define UNROLL_VECMUL  /* RWCox */

/*---------------------------------------------------------------------------*/
/*!
  Right multiply matrix a by vector b.  Result is vector c.
*/
void vector_multiply (matrix a, vector b, vector * c)
{
  register int rows, cols;
  register int i, j;
  register double *bb , *aa ;
  register double sum ;

  if (a.cols != b.dim){
    char str[444] ;
    sprintf(str,
            "Incompatible dimensions for vector multiplication: %dx%d X %d",
            a.rows,a.cols,b.dim ) ;
    matrix_error(str) ;
  }

  rows = a.rows;
  cols = a.cols;

  vector_create_noinit (rows, c);

  if( cols <= 0 ){
    for( i=0 ; i < rows ; i++ ) c->elts[i] = 0.0l ;
    return ;
  }

  bb = b.elts ;

#ifdef UNROLL_VECMUL
  if( cols%2 == 0 ){              /* even number of cols */
    for (i = 0;  i < rows;  i++){
        sum = 0.0 ; aa = a.elts[i] ;
        for (j = 0;  j < cols;  j+=2 )
          sum += aa[j]*bb[j] + aa[j+1]*bb[j+1];
        c->elts[i] = sum ;
    }
  } else {                        /* odd number of cols */
    for (i = 0;  i < rows;  i++){
        aa = a.elts[i] ; sum = aa[0]*bb[0] ;
        for (j = 1;  j < cols;  j+=2 )
          sum += aa[j]*bb[j] + aa[j+1]*bb[j+1];
        c->elts[i] = sum ;
    }
  }
#else
    for (i = 0;  i < rows;  i++){
        sum = 0.0 ; aa = a.elts[i] ;
        for (j = 0;  j < cols;  j++ ) sum += aa[j]*bb[j] ;
        c->elts[i] = sum ;
    }
#endif /* UNROLL_VECMUL */

    flops += 2.0l*rows*cols ;
}

/*---------------------------------------------------------------------------*/
/*!
  Compute d = c-a*b: a is a matrix; b,c,d are vectors -- RWCox
  26 Feb 2002: return value is sum of squares of d vector
*/

double vector_multiply_subtract (matrix a, vector b, vector c, vector * d)
{
  register int rows, cols;
  register int i, j;
  register double qsum ;
  register double *aa,*bb ;
  register double sum ;

  if (a.cols != b.dim || a.rows != c.dim )
    matrix_error ("Incompatible dimensions for vector multiplication-subtraction");

  rows = a.rows;
  cols = a.cols;

  vector_create_noinit (rows, d);

  if( cols <= 0 ){
    qsum = 0.0 ;
    for( i=0 ; i < rows ; i++ ){
      d->elts[i] = c.elts[i] ;
      qsum += d->elts[i] * d->elts[i] ;
    }
    return qsum ;
  }

  qsum = 0.0 ; bb = b.elts ;

#ifdef UNROLL_VECMUL
  if( cols%2 == 0 ){                   /* even number */
    for (i = 0;  i < rows;  i++){
      aa = a.elts[i] ; sum = c.elts[i] ;
      for (j = 0;  j < cols;  j+=2)
        sum -= aa[j]*bb[j] + aa[j+1]*bb[j+1];
      d->elts[i] = sum ; qsum += sum*sum ;
    }
  } else {                            /* odd number */
    for (i = 0;  i < rows;  i++){
      aa = a.elts[i] ; sum = c.elts[i] - aa[0]*bb[0] ;
      for (j = 1;  j < cols;  j+=2)
        sum -= aa[j]*bb[j] + aa[j+1]*bb[j+1];
      d->elts[i] = sum ; qsum += sum*sum ;
    }
  }
#else
  for (i = 0;  i < rows;  i++){
    aa = a.elts[i] ; sum = c.elts[i] ;
    for (j = 0;  j < cols;  j++) sum -= aa[j] * bb[j] ;
    d->elts[i] = sum ; qsum += sum*sum ;
  }
#endif /* UNROLL_VECMUL */

  flops += 2.0l*rows*(cols+1) ;

  return qsum ;  /* 26 Feb 2003 */
}

/*---------------------------------------------------------------------------*/
/*!
  Calculate dot product of vector a with vector b.
*/

double vector_dot (vector a, vector b)
{
  register int i, dim;
  register double sum;
  register double *aa , *bb ;

  if (a.dim != b.dim)
    matrix_error ("Incompatible dimensions for vector dot product");

  dim = a.dim;

  sum = 0.0;
  aa = a.elts ; bb = b.elts ;
  for (i = 0;  i < dim;  i++)
#if 0
    sum += a.elts[i] * b.elts[i];
#else
    sum += aa[i] * bb[i] ;
#endif

  flops += 2.0l*dim ;
  return (sum);
}

/*--------------------------------------------------------------------------*/
/*!
  Calculate dot product of vector a with itself -- 28 Dec 2001, RWCox.
*/

double vector_dotself( vector a )
{
  register int i, dim;
  register double sum;
  register double *aa ;

  dim = a.dim;
  sum = 0.0;
  aa = a.elts ;
  for (i = 0;  i < dim;  i++)
#if 0
    sum += a.elts[i] * a.elts[i];
#else
    sum += aa[i] * aa[i] ;
#endif

  flops += 2.0l*dim ;
  return (sum);
}

/*---------------------------------------------------------------------------*/
/*!
  Compute the L_infinity norm of a matrix: the max absolute row sum.
*/

double matrix_norm( matrix a )
{
   int i,j , rows=a.rows, cols=a.cols ;
   double sum , smax=0.0 ;

   for (i = 0;  i < rows;  i++){
     sum = 0.0 ;
     for (j = 0;  j < cols;  j++) sum += fabs(a.elts[i][j]) ;
     if( sum > smax ) smax = sum ;
   }
   flops += 2.0l*rows*cols ;
   return smax ;
}

/*---------------------------------------------------------------------------*/
/*! Search a matrix for nearly identical column pairs, where "nearly identical"
    means they are correlated closer than 1-eps.

    Return is a pointer to an int array of the form
      [ i1 j1 i2 j2 ... -1 -1 ]
    where columns (i1,j1) are nearly the same, (i2,j2) also, etc.
    In addition:
     - A pair (i,-1) indicates that column #i is all zeros.
     - The array is terminated with the pair (-1,-1).
     - If there are no bad column pairs or all-zero columns, NULL is returned.
     - Pairs of all-zero columns are NOT reported.
     - The array should be free()-ed when you are done with it.
-----------------------------------------------------------------------------*/

int * matrix_check_columns( matrix a , double eps )  /* 14 Jul 2004 */
{
   int i,j,k , rows=a.rows , cols=a.cols ;
   int *iar=NULL , nar=0 ;
   double sumi,sumj,sumd ;

   if( eps <= 0.0 ) eps = 1.e-5 ;

   for( i=0 ; i < cols ; i++ ){
     sumi = 0.0 ;
     for( k=0 ; k < rows ; k++ ) sumi += a.elts[k][i] * a.elts[k][i] ;
     if( sumi <= 0.0 ){
       iar = (int *)realloc( (void *)iar , sizeof(int)*2*(nar+1) ) ;
       iar[2*nar] = i ; iar[2*nar+1] = -1 ; nar++ ;
       continue ;                           /* skip to next column i */
     }
     for( j=i+1 ; j < cols ; j++ ){
       sumj = sumd = 0.0 ;
       for( k=0 ; k < rows ; k++ ){
         sumj += a.elts[k][j] * a.elts[k][j] ;
         sumd += a.elts[k][j] * a.elts[k][i] ;
       }
       if( sumj > 0.0 ){
         sumd = fabs(sumd) / sqrt(sumi*sumj) ;
         if( sumd >= 1.0-eps ){
           iar = (int *)realloc( (void *)iar , sizeof(int)*2*(nar+1) ) ;
           iar[2*nar] = i ; iar[2*nar+1] = j ; nar++ ;
         }
       }
     }
   }

   if( iar != NULL ){
     iar = (int *)realloc( (void *)iar , sizeof(int)*2*(nar+1) ) ;
     iar[2*nar] = iar[2*nar+1] = -1 ;
   }

   return iar ;
}

/*---------------------------------------------------------------------------*/
/*! Return the eigenvalues of matrix X-transpose X, scaled to diagonal 1.
    The output points to a vector of doubles, of length X.cols.  This
    should be free()-ed when you are done with it.
-----------------------------------------------------------------------------*/

double * matrix_singvals( matrix X )   /* 14 Jul 2004 */
{
   int i,j,k , M=X.rows , N=X.cols ;
   double *a , *e , sum ;

   a = (double *) malloc( sizeof(double)*N*N ) ;
   e = (double *) malloc( sizeof(double)*N   ) ;

   for( i=0 ; i < N ; i++ ){
     for( j=0 ; j <= i ; j++ ){
       sum = 0.0l ;
       for( k=0 ; k < M ; k++ ) sum += X.elts[k][i] * X.elts[k][j] ;
       a[j+N*i] = sum ;
       if( j < i ) a[i+N*j] = sum ;
     }
   }

   for( i=0 ; i < N ; i++ ){
     if( a[i+N*i] > 0.0 ) e[i] = 1.0l / sqrt(a[i+N*i]) ;
     else                 e[i] = 1.0l ;
   }
   for( i=0 ; i < N ; i++ ){
     for( j=0 ; j < N ; j++ ) a[j+N*i] *= e[i]*e[j] ;
   }

   symeigval_double( N , a , e ) ;
   free( (void *)a ) ;
   flops += (M+N+2.0l)*N*N ;
   return e ;
}

/*---------------------------------------------------------------------------*/

extern void svd_double( int, int, double *, double *, double *, double * ) ;

/*---------------------------------------------------------------------------*/
/*! Given MxN matrix X, return the NxN matrix

       [  T  ]-1                       [  T  ]-1 T
       [ X X ]     and the NxM matrix  [ X X ]  X
-----------------------------------------------------------------------------*/

void matrix_psinv( matrix X , matrix *XtXinv , matrix *XtXinvXt )
{
   int m = X.rows , n = X.cols , ii,jj,kk ;
   double *amat , *umat , *vmat , *sval , *xfac , smax,del,sum ;

   if( m < 1 || n < 1 || m < n || (XtXinv == NULL && XtXinvXt == NULL) ) return;

   amat = (double *)calloc( sizeof(double),m*n ) ;  /* input matrix */
   umat = (double *)calloc( sizeof(double),m*n ) ;  /* left singular vectors */
   vmat = (double *)calloc( sizeof(double),n*n ) ;  /* right singular vectors */
   sval = (double *)calloc( sizeof(double),n   ) ;  /* singular values */
   xfac = (double *)calloc( sizeof(double),n   ) ;  /* column norms of [a] */

#define A(i,j) amat[(i)+(j)*m]
#define U(i,j) umat[(i)+(j)*m]
#define V(i,j) vmat[(i)+(j)*n]

   /* copy input matrix into amat */

   for( ii=0 ; ii < m ; ii++ )
     for( jj=0 ; jj < n ; jj++ ) A(ii,jj) = X.elts[ii][jj] ;

   /* scale each column to have norm 1 */

   for( jj=0 ; jj < n ; jj++ ){
     sum = 0.0l ;
     for( ii=0 ; ii < m ; ii++ ) sum += A(ii,jj)*A(ii,jj) ;
     if( sum > 0.0l ) sum = 1.0l/sqrt(sum) ;
     xfac[jj] = sum ;
     for( ii=0 ; ii < m ; ii++ ) A(ii,jj) *= sum ;
   }

   /* compute SVD of scaled matrix */

   svd_double( m , n , amat , sval , umat , vmat ) ; 

   free((void *)amat) ;  /* done with this */

   /* find largest singular value */

   smax = sval[0] ;
   for( ii=1 ; ii < n ; ii++ )
     if( sval[ii] > smax ) smax = sval[ii] ;

   if( smax <= 0.0l ){                        /* this is bad */
     free((void *)xfac); free((void *)sval);
     free((void *)vmat); free((void *)umat); return;
   }

   for( ii=0 ; ii < n ; ii++ )
     if( sval[ii] < 0.0l ) sval[ii] = 0.0l ;

#ifdef FLOATIZE
#define PSINV_EPS 1.e-8
#else
#define PSINV_EPS 1.e-16
#endif

   /* "reciprocals" of singular values:  1/s is actually s/(s^2+del) */

   del  = PSINV_EPS * smax*smax ;
   for( ii=0 ; ii < n ; ii++ )
     sval[ii] = sval[ii] / ( sval[ii]*sval[ii] + del ) ;

   /* create pseudo-inverse */

   if( XtXinvXt != NULL ){
     matrix_create( n , m , XtXinvXt ) ;
     for( ii=0 ; ii < n ; ii++ ){
       for( jj=0 ; jj < m ; jj++ ){
         sum = 0.0l ;
         for( kk=0 ; kk < n ; kk++ )
           sum += sval[kk] * V(ii,kk) * U(jj,kk) ;
         XtXinvXt->elts[ii][jj] = sum * xfac[ii] ;
       }
     }
   }

   if( XtXinv != NULL ){
     matrix_create( n , n , XtXinv ) ;
     for( ii=0 ; ii < n ; ii++ ) sval[ii] = sval[ii] * sval[ii] ;
     matrix_create( n , n , XtXinv ) ;
     for( ii=0 ; ii < n ; ii++ ){
       for( jj=0 ; jj < n ; jj++ ){
         sum = 0.0l ;
         for( kk=0 ; kk < n ; kk++ )
           sum += sval[kk] * V(ii,kk) * V(jj,kk) ;
         XtXinv->elts[ii][jj] = sum * xfac[ii] * xfac[jj] ;
       }
     }
   }

   flops += n*n*(n+2.0l*m+2.0l) ;
   free((void *)xfac); free((void *)sval);
   free((void *)vmat); free((void *)umat); return;
}
