/* GTS - Library for the manipulation of triangulated surfaces
 * Copyright (C) 1999 Stéphane Popinet
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <math.h>
#include "gts.h"

/**
 * gts_matrix_new:
 * @a00: element [0][0].
 * @a01: element [0][1].
 * @a02: element [0][2].
 * @a03: element [0][3].
 * @a10: element [1][0].
 * @a11: element [1][1].
 * @a12: element [1][2].
 * @a13: element [1][3].
 * @a20: element [2][0].
 * @a21: element [2][1].
 * @a22: element [2][2].
 * @a23: element [2][3].
 * @a30: element [3][0].
 * @a31: element [3][1].
 * @a32: element [3][2].
 * @a33: element [3][3].
 *
 * Allocates memory and initializes a new #GtsMatrix.
 *
 * Returns: a pointer to the newly created #GtsMatrix.
 */
GtsMatrix * gts_matrix_new (gdouble a00, gdouble a01, gdouble a02, gdouble a03,
			    gdouble a10, gdouble a11, gdouble a12, gdouble a13,
			    gdouble a20, gdouble a21, gdouble a22, gdouble a23,
			    gdouble a30, gdouble a31, gdouble a32, gdouble a33)
{
  GtsMatrix * m;

  m = g_malloc (4*sizeof (GtsVector4));

  m[0][0] = a00; m[1][0] = a10; m[2][0] = a20; m[3][0] = a30;
  m[0][1] = a01; m[1][1] = a11; m[2][1] = a21; m[3][1] = a31;
  m[0][2] = a02; m[1][2] = a12; m[2][2] = a22; m[3][2] = a32;
  m[0][3] = a03; m[1][3] = a13; m[2][3] = a23; m[3][3] = a33;

  return m;
}

/**
 * gts_matrix_assign:
 * @m: a #GtsMatrix.
 * @a00: element [0][0].
 * @a01: element [0][1].
 * @a02: element [0][2].
 * @a03: element [0][3].
 * @a10: element [1][0].
 * @a11: element [1][1].
 * @a12: element [1][2].
 * @a13: element [1][3].
 * @a20: element [2][0].
 * @a21: element [2][1].
 * @a22: element [2][2].
 * @a23: element [2][3].
 * @a30: element [3][0].
 * @a31: element [3][1].
 * @a32: element [3][2].
 * @a33: element [3][3].
 *
 * Set values of matrix elements.
 */
void gts_matrix_assign (GtsMatrix * m,
			gdouble a00, gdouble a01, gdouble a02, gdouble a03,
			gdouble a10, gdouble a11, gdouble a12, gdouble a13,
			gdouble a20, gdouble a21, gdouble a22, gdouble a23,
			gdouble a30, gdouble a31, gdouble a32, gdouble a33)
{
  g_return_if_fail (m != NULL);

  m[0][0] = a00; m[1][0] = a10; m[2][0] = a20; m[3][0] = a30;
  m[0][1] = a01; m[1][1] = a11; m[2][1] = a21; m[3][1] = a31;
  m[0][2] = a02; m[1][2] = a12; m[2][2] = a22; m[3][2] = a32;
  m[0][3] = a03; m[1][3] = a13; m[2][3] = a23; m[3][3] = a33;
}

/**
 * gts_matrix_projection:
 * @t: a #GtsTriangle.
 *
 * Creates a new #GtsMatrix representing the projection onto a plane of normal
 * given by @t.
 *
 * Returns: a pointer to the newly created #GtsMatrix.
 */
GtsMatrix * gts_matrix_projection (GtsTriangle * t)
{
  GtsVertex * v1, * v2, * v3;
  GtsEdge * e1, * e2, * e3;
  GtsMatrix * m;
  gdouble x1, y1, z1, x2, y2, z2, x3, y3, z3, l;
  
  g_return_val_if_fail (t != NULL, NULL);

  m = g_malloc (4*sizeof (GtsVector4));
  gts_triangle_vertices_edges (t, NULL, &v1, &v2, &v3, &e1, &e2, &e3);

  x1 = GTS_POINT (v2)->x - GTS_POINT (v1)->x; 
  y1 = GTS_POINT (v2)->y - GTS_POINT (v1)->y; 
  z1 = GTS_POINT (v2)->z - GTS_POINT (v1)->z;
  x2 = GTS_POINT (v3)->x - GTS_POINT (v1)->x; 
  y2 = GTS_POINT (v3)->y - GTS_POINT (v1)->y; 
  z2 = GTS_POINT (v3)->z - GTS_POINT (v1)->z;
  x3 = y1*z2 - z1*y2; y3 = z1*x2 - x1*z2; z3 = x1*y2 - y1*x2;
  x2 = y3*z1 - z3*y1; y2 = z3*x1 - x3*z1; z2 = x3*y1 - y3*x1;

  g_assert ((l = sqrt (x1*x1 + y1*y1 + z1*z1)) > 0.0);
  m[0][0] = x1/l; m[1][0] = y1/l; m[2][0] = z1/l; m[3][0] = 0.;
  g_assert ((l = sqrt (x2*x2 + y2*y2 + z2*z2)) > 0.0);
  m[0][1] = x2/l; m[1][1] = y2/l; m[2][1] = z2/l; m[3][1] = 0.;
  g_assert ((l = sqrt (x3*x3 + y3*y3 + z3*z3)) > 0.0);
  m[0][2] = x3/l; m[1][2] = y3/l; m[2][2] = z3/l; m[3][2] = 0.;
  m[0][3] = 0; m[1][3] = 0.; m[2][3] = 0.; m[3][3] = 1.;

  return m;
}

/**
 * gts_matrix_transpose:
 * @m: a #GtsMatrix.
 *
 * Returns: a pointer to a newly created #GtsMatrix transposed of @m.
 */
GtsMatrix * gts_matrix_transpose (GtsMatrix * m)
{
  GtsMatrix * mi;

  g_return_val_if_fail (m != NULL, NULL);

  mi = g_malloc (4*sizeof (GtsVector4));

  mi[0][0] = m[0][0]; mi[1][0] = m[0][1]; 
  mi[2][0] = m[0][2]; mi[3][0] = m[0][3];
  mi[0][1] = m[1][0]; mi[1][1] = m[1][1]; 
  mi[2][1] = m[1][2]; mi[3][1] = m[1][3];
  mi[0][2] = m[2][0]; mi[1][2] = m[2][1]; 
  mi[2][2] = m[2][2]; mi[3][2] = m[2][3];
  mi[0][3] = m[3][0]; mi[1][3] = m[3][1]; 
  mi[2][3] = m[3][2]; mi[3][3] = m[3][3];

  return mi;
}

/*
 * calculate the determinant of a 2x2 matrix.
 * 
 * Adapted from:
 * Matrix Inversion
 * by Richard Carling
 * from "Graphics Gems", Academic Press, 1990
 */
static gdouble det2x2 (gdouble a, gdouble b, gdouble c, gdouble d)
{
  gdouble ans2;

  ans2 = a*d - b*c;
  return ans2;
}

/*
 * calculate the determinant of a 3x3 matrix
 * in the form
 *
 *     | a1,  b1,  c1 |
 *     | a2,  b2,  c2 |
 *     | a3,  b3,  c3 |
 *
 * Adapted from:
 * Matrix Inversion
 * by Richard Carling
 * from "Graphics Gems", Academic Press, 1990
 */
static gdouble det3x3 (gdouble a1, gdouble a2, gdouble a3, 
		       gdouble b1, gdouble b2, gdouble b3, 
		       gdouble c1, gdouble c2, gdouble c3)
{
  gdouble ans3;

  ans3 = a1 * det2x2( b2, b3, c2, c3 )
    - b1 * det2x2( a2, a3, c2, c3 )
    + c1 * det2x2( a2, a3, b2, b3 );
  return ans3;
}

/**
 * gts_matrix_determinant:
 * @m: a #GtsMatrix.
 *
 * Returns: the value of det(@m).
 */
gdouble gts_matrix_determinant (GtsMatrix * m)
{
  gdouble ans4;
  gdouble a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4;

  g_return_val_if_fail (m != NULL, 0.0);

  a1 = m[0][0]; b1 = m[0][1]; 
  c1 = m[0][2]; d1 = m[0][3];
  
  a2 = m[1][0]; b2 = m[1][1]; 
  c2 = m[1][2]; d2 = m[1][3];
  
  a3 = m[2][0]; b3 = m[2][1]; 
  c3 = m[2][2]; d3 = m[2][3];
  
  a4 = m[3][0]; b4 = m[3][1]; 
  c4 = m[3][2]; d4 = m[3][3];
  
  ans4 = a1 * det3x3 (b2, b3, b4, c2, c3, c4, d2, d3, d4)
    - b1 * det3x3 (a2, a3, a4, c2, c3, c4, d2, d3, d4)
    + c1 * det3x3 (a2, a3, a4, b2, b3, b4, d2, d3, d4)
    - d1 * det3x3 (a2, a3, a4, b2, b3, b4, c2, c3, c4);

  return ans4;
}

/* 
 *   adjoint( original_matrix, inverse_matrix )
 * 
 *     calculate the adjoint of a 4x4 matrix
 *
 *      Let  a   denote the minor determinant of matrix A obtained by
 *           ij
 *
 *      deleting the ith row and jth column from A.
 *
 *                    i+j
 *     Let  b   = (-1)    a
 *          ij            ji
 *
 *    The matrix B = (b  ) is the adjoint of A
 *                     ij
 */
static GtsMatrix * adjoint (GtsMatrix * m)
{
  gdouble a1, a2, a3, a4, b1, b2, b3, b4;
  gdouble c1, c2, c3, c4, d1, d2, d3, d4;
  GtsMatrix * ma;

  a1 = m[0][0]; b1 = m[0][1]; 
  c1 = m[0][2]; d1 = m[0][3];
  
  a2 = m[1][0]; b2 = m[1][1]; 
  c2 = m[1][2]; d2 = m[1][3];
  
  a3 = m[2][0]; b3 = m[2][1];
  c3 = m[2][2]; d3 = m[2][3];
  
  a4 = m[3][0]; b4 = m[3][1]; 
  c4 = m[3][2]; d4 = m[3][3];

  ma = g_malloc (4*sizeof (GtsVector4));

  /* row column labeling reversed since we transpose rows & columns */

  ma[0][0]  =   det3x3 (b2, b3, b4, c2, c3, c4, d2, d3, d4);
  ma[1][0]  = - det3x3 (a2, a3, a4, c2, c3, c4, d2, d3, d4);
  ma[2][0]  =   det3x3 (a2, a3, a4, b2, b3, b4, d2, d3, d4);
  ma[3][0]  = - det3x3 (a2, a3, a4, b2, b3, b4, c2, c3, c4);
  
  ma[0][1]  = - det3x3 (b1, b3, b4, c1, c3, c4, d1, d3, d4);
  ma[1][1]  =   det3x3 (a1, a3, a4, c1, c3, c4, d1, d3, d4);
  ma[2][1]  = - det3x3 (a1, a3, a4, b1, b3, b4, d1, d3, d4);
  ma[3][1]  =   det3x3 (a1, a3, a4, b1, b3, b4, c1, c3, c4);
  
  ma[0][2]  =   det3x3 (b1, b2, b4, c1, c2, c4, d1, d2, d4);
  ma[1][2]  = - det3x3 (a1, a2, a4, c1, c2, c4, d1, d2, d4);
  ma[2][2]  =   det3x3 (a1, a2, a4, b1, b2, b4, d1, d2, d4);
  ma[3][2]  = - det3x3 (a1, a2, a4, b1, b2, b4, c1, c2, c4);
  
  ma[0][3]  = - det3x3 (b1, b2, b3, c1, c2, c3, d1, d2, d3);
  ma[1][3]  =   det3x3 (a1, a2, a3, c1, c2, c3, d1, d2, d3);
  ma[2][3]  = - det3x3 (a1, a2, a3, b1, b2, b3, d1, d2, d3);
  ma[3][3]  =   det3x3 (a1, a2, a3, b1, b2, b3, c1, c2, c3);
  
  return ma;
}


/**
 * gts_matrix_inverse:
 * @m: a #GtsMatrix.
 *
 * Returns: a pointer to a newly created #GtsMatrix inverse of @m or %NULL
 * if @m is not invertible.
 */
GtsMatrix * gts_matrix_inverse (GtsMatrix * m)
{
  GtsMatrix * madj;
  gdouble det;
  gint i, j;

  g_return_val_if_fail (m != NULL, NULL);
  
  det = gts_matrix_determinant (m);
  if (det == 0.)
    return NULL;

  madj = adjoint (m);
  for (i = 0; i < 4; i++)
    for(j = 0; j < 4; j++)
      madj[i][j] /= det;

  return madj;
}

/**
 * gts_matrix3_inverse:
 * @m: a 3x3 #GtsMatrix.
 *
 * Returns: a pointer to a newly created 3x3 #GtsMatrix inverse of @m or %NULL
 * if @m is not invertible.
 */
GtsMatrix * gts_matrix3_inverse (GtsMatrix * m)
{
  GtsMatrix * mi;
  gdouble det;

  g_return_val_if_fail (m != NULL, NULL);
  
  det = (m[0][0]*(m[1][1]*m[2][2] - m[2][1]*m[1][2]) - 
	 m[0][1]*(m[1][0]*m[2][2] - m[2][0]*m[1][2]) + 
	 m[0][2]*(m[1][0]*m[2][1] - m[2][0]*m[1][1]));
  if (det == 0.0)
    return NULL;

  mi = g_malloc0 (4*sizeof (GtsVector));

  mi[0][0] = (m[1][1]*m[2][2] - m[1][2]*m[2][1])/det; 
  mi[0][1] = (m[2][1]*m[0][2] - m[0][1]*m[2][2])/det;
  mi[0][2] = (m[0][1]*m[1][2] - m[1][1]*m[0][2])/det; 
  mi[1][0] = (m[1][2]*m[2][0] - m[1][0]*m[2][2])/det; 
  mi[1][1] = (m[0][0]*m[2][2] - m[2][0]*m[0][2])/det; 
  mi[1][2] = (m[1][0]*m[0][2] - m[0][0]*m[1][2])/det; 
  mi[2][0] = (m[1][0]*m[2][1] - m[2][0]*m[1][1])/det; 
  mi[2][1] = (m[2][0]*m[0][1] - m[0][0]*m[2][1])/det; 
  mi[2][2] = (m[0][0]*m[1][1] - m[0][1]*m[1][0])/det; 

  return mi;
}

/**
 * gts_matrix_print:
 * @m: a #GtsMatrix.
 * @fptr: a file descriptor.
 * 
 * Print @m to file @fptr.
 */
void gts_matrix_print (GtsMatrix * m, FILE * fptr)
{
  g_return_if_fail (m != NULL);
  g_return_if_fail (fptr != NULL);

  fprintf (fptr, 
	   "[[%15.7g %15.7g %15.7g %15.7g]\n"
	   " [%15.7g %15.7g %15.7g %15.7g]\n"
	   " [%15.7g %15.7g %15.7g %15.7g]\n"
	   " [%15.7g %15.7g %15.7g %15.7g]]\n",
	   m[0][0], m[0][1], m[0][2], m[0][3],
	   m[1][0], m[1][1], m[1][2], m[1][3],
	   m[2][0], m[2][1], m[2][2], m[2][3],
	   m[3][0], m[3][1], m[3][2], m[3][3]);
}

/**
 * gts_vector_print:
 * @v: a #GtsVector.
 * @fptr: a file descriptor.
 * 
 * Print @s to file @fptr.
 */
void gts_vector_print (GtsVector v, FILE * fptr)
{
  g_return_if_fail (fptr != NULL);

  fprintf (fptr, 
	   "[%15.7g %15.7g %15.7g ]\n",
	   v[0], v[1], v[2]);
}

/**
 * gts_vector4_print:
 * @v: a #GtsVector4.
 * @fptr: a file descriptor.
 * 
 * Print @v to file @fptr.
 */
void gts_vector4_print (GtsVector4 v, FILE * fptr)
{
  g_return_if_fail (fptr != NULL);

  fprintf (fptr, 
	   "[%15.7g %15.7g %15.7g %15.7g]\n",
	   v[0], v[1], v[2], v[3]);
}

/* [cos(alpha)]^2 */
#define COSALPHA2 0.999695413509 /* alpha = 1 degree */
/* [sin(alpha)]^2 */
#define SINALPHA2 3.04586490453e-4 /* alpha = 1 degree */

/**
 * gts_matrix_compatible_row:
 * @A: a #GtsMatrix.
 * @b: a #GtsVector.
 * @n: the number of previous constraints of @A.x=@b.
 * @A1: a #GtsMatrix.
 * @b1: a #GtsVector.
 *
 * Given a system of @n constraints @A.x=@b adds to it the compatible
 * constraints defined by @A1.x=@b1. The compatibility is determined
 * by insuring that the resulting system is well-conditioned (see
 * Lindstrom and Turk (1998, 1999)).
 *
 * Returns: the number of constraints of the resulting system.  
 */
guint gts_matrix_compatible_row (GtsMatrix * A,
				 GtsVector b,
				 guint n,
				 GtsVector A1,
				 gdouble b1)
{
  gdouble na1;
  
  g_return_val_if_fail (A != NULL, 0);

  na1 = gts_vector_scalar (A1, A1);
  if (na1 == 0.0)
    return n;

  /* normalize row */
  na1 = sqrt (na1);
  A1[0] /= na1; A1[1] /= na1; A1[2] /= na1; b1 /= na1;

  if (n == 1) {
    gdouble a0a1 = gts_vector_scalar (A[0], A1);
    if (a0a1*a0a1 >= COSALPHA2)
      return 1;
  }
  else if (n == 2) {
    GtsVector V;
    gdouble s;
    
    gts_vector_cross (V, A[0], A[1]);
    s = gts_vector_scalar (V, A1);
    if (s*s <= gts_vector_scalar (V, V)*SINALPHA2)
      return 2;
  }

  A[n][0] = A1[0]; A[n][1] = A1[1]; A[n][2] = A1[2]; b[n] = b1;
  return n + 1;
}

/**
 * gts_matrix_quadratic_optimization:
 * @A: a #GtsMatrix.
 * @b: a #GtsVector.
 * @n: the number of constraints (must be smaller than 3).
 * @H: a symmetric positive definite Hessian.
 * @c: a #GtsVector.
 *
 * Solve a quadratic optimization problem: Given a quadratic objective function
 * f which can be written as: f(x) = x^t.@H.x + @c^t.x + k, where @H is the 
 * symmetric positive definite Hessian of f and k is a constant, find the
 * minimum of f subject to the set of @n prior linear constraints, defined by
 * the first @n rows of @A and @b (@A.x = @b). The new constraints given by
 * the minimization are added to @A and @b only if they are linearly
 * independent as determined by gts_matrix_compatible_row().
 *
 * Returns: the new number of constraints defined by @A and @b.
 */
guint gts_matrix_quadratic_optimization (GtsMatrix * A,
					 GtsVector b,
					 guint n,
					 GtsMatrix * H,
					 GtsVector c)
{
  g_return_val_if_fail (A != NULL, 0);
  g_return_val_if_fail (b != NULL, 0);
  g_return_val_if_fail (n < 3, 0);
  g_return_val_if_fail (H != NULL, 0);

  switch (n) {
  case 0: {
    n = gts_matrix_compatible_row (A, b, n, H[0], - c[0]);
    n = gts_matrix_compatible_row (A, b, n, H[1], - c[1]);
    n = gts_matrix_compatible_row (A, b, n, H[2], - c[2]);
    return n;
  }
  case 1: {
    GtsVector Q0 = {0., 0., 0.};
    GtsVector Q1 = {0., 0., 0.};
    GtsVector A1;
    gdouble max = A[0][0]*A[0][0];
    guint d = 0;

    /* build a vector orthogonal to the constraint */
    if (A[0][1]*A[0][1] > max) { max = A[0][1]*A[0][1]; d = 1; }
    if (A[0][2]*A[0][2] > max) { max = A[0][2]*A[0][2]; d = 2; }
    switch (d) {
    case 0: Q0[0] = - A[0][2]/A[0][0]; Q0[2] = 1.0; break;
    case 1: Q0[1] = - A[0][2]/A[0][1]; Q0[2] = 1.0; break;
    case 2: Q0[2] = - A[0][0]/A[0][2]; Q0[0] = 1.0; break;
    }

    /* build a second vector orthogonal to the first and to the constraint */
    gts_vector_cross (Q1, A[0], Q0);

    A1[0] = gts_vector_scalar (Q0, H[0]);
    A1[1] = gts_vector_scalar (Q0, H[1]);
    A1[2] = gts_vector_scalar (Q0, H[2]);

    n = gts_matrix_compatible_row (A, b, n, A1, - gts_vector_scalar (Q0, c));
    
    A1[0] = gts_vector_scalar (Q1, H[0]);
    A1[1] = gts_vector_scalar (Q1, H[1]);
    A1[2] = gts_vector_scalar (Q1, H[2]);

    n = gts_matrix_compatible_row (A, b, n, A1, - gts_vector_scalar (Q1, c));

    return n;
  }
  case 2: {
    /* build a vector orthogonal to the two constraints */
    GtsVector A1, Q;

    gts_vector_cross (Q, A[0], A[1]);
    A1[0] = gts_vector_scalar (Q, H[0]);
    A1[1] = gts_vector_scalar (Q, H[1]);
    A1[2] = gts_vector_scalar (Q, H[2]);
    
    n = gts_matrix_compatible_row (A, b, n, A1, - gts_vector_scalar (Q, c));

    return n;
  }
  default:
    g_assert_not_reached ();
  }
  return 0;
}

/**
 * gts_matrix_destroy:
 * @m: a #GtsMatrix.
 *
 * Free all the memory allocated for @m.
 */
void gts_matrix_destroy (GtsMatrix * m)
{
  g_free (m);
}

/**
 * gts_matrix_product:
 * @m1: a #GtsMatrix.
 * @m2: another #GtsMatrix.
 *
 * Returns: a new #GtsMatrix, product of @m1 and @m2.
 */
GtsMatrix * gts_matrix_product (GtsMatrix * m1, GtsMatrix * m2)
{
  guint i, j;
  GtsMatrix * m;

  g_return_val_if_fail (m1 != NULL, NULL);
  g_return_val_if_fail (m2 != NULL, NULL);
  g_return_val_if_fail (m1 != m2, NULL);

  m = g_malloc (4*sizeof (GtsVector4));

  for (i = 0; i < 4; i++)
    for (j = 0; j < 4; j++)
      m[i][j] = m1[i][0]*m2[0][j] + m1[i][1]*m2[1][j] +
        m1[i][2]*m2[2][j] + m1[i][3]*m2[3][j];
  return m;
}

/**
 * gts_matrix_zero:
 * @m: a #GtsMatrix or $NULL.
 *
 * Initializes @m to zeros. Allocates a matrix if @m is %NULL.
 *
 * Returns: the zero'ed matrix.
 */
GtsMatrix * gts_matrix_zero (GtsMatrix * m)
{
  if (m == NULL)
    m = g_malloc0 (4*sizeof (GtsVector4));
  else {
    m[0][0] = m[1][0] = m[2][0] = m[3][0] = 0.;
    m[0][1] = m[1][1] = m[2][1] = m[3][1] = 0.;
    m[0][2] = m[1][2] = m[2][2] = m[3][2] = 0.;
    m[0][3] = m[1][3] = m[2][3] = m[3][3] = 0.;
  }
  return m;
}

/**
 * gts_matrix_identity:
 * @m: a #GtsMatrix or %NULL.
 *
 * Initializes @m to an identity matrix. Allocates a matrix if @m is %NULL.
 *
 * Returns: the identity matrix.
 */
GtsMatrix * gts_matrix_identity (GtsMatrix * m)
{
  m = gts_matrix_zero (m);
  m[0][0] = m[1][1] = m[2][2] = m[3][3] = 1.;
  return m;
}

/**
 * gts_matrix_scale:
 * @m: a #GtsMatrix or %NULL.
 * @s: the scaling vector.
 *
 * Initializes @m to a scaling matrix for @s. Allocates a matrix if @m
 * is %NULL.
 *
 * Returns: the scaling matrix.
 */
GtsMatrix * gts_matrix_scale (GtsMatrix * m, GtsVector s)
{
  m = gts_matrix_zero (m);
  m[0][0] = s[0];
  m[1][1] = s[1];
  m[2][2] = s[2];
  m[3][3] = 1.;
  return m;
}

/**
 * gts_matrix_translate:
 * @m: a #GtsMatrix or %NULL.
 * @t: the translation vector.
 *
 * Initializes @m to a translation matrix for @t.  Allocates a new
 * matrix if @m is %NULL.
 *
 * Returns: the translation matix.
 */
GtsMatrix * gts_matrix_translate (GtsMatrix * m, GtsVector t)
{
  m = gts_matrix_zero (m);
  m[0][3] = t[0];
  m[1][3] = t[1];
  m[2][3] = t[2];
  m[3][3] = 1.;
  m[0][0] = m[1][1] = m[2][2] = 1.;
  return m;
}

/**
 * gts_matrix_rotate:
 * @m: a #GtsMatrix or %NULL.
 * @r: the rotation axis.
 * @angle: the angle (in radians) to rotate by.
 *
 * Initializes @m to a rotation matrix around @r by @angle.
 * Allocates a new matrix if @m is %NULL.
 *
 * Returns: the rotation matrix.
 */
GtsMatrix * gts_matrix_rotate (GtsMatrix * m,
			       GtsVector r,
			       gdouble angle)
{
  gdouble c, c1, s;

  gts_vector_normalize (r);

  c = cos (angle);
  c1 = 1. - c;
  s = sin (angle);

  if (m == NULL)
    m = g_malloc (4*sizeof (GtsVector4));

  m[0][0] = r[0]*r[0]*c1 + c;
  m[0][1] = r[0]*r[1]*c1 - r[2]*s;
  m[0][2] = r[0]*r[2]*c1 + r[1]*s;
  m[0][3] = 0.;

  m[1][0] = r[1]*r[0]*c1 + r[2]*s;
  m[1][1] = r[1]*r[1]*c1 + c;
  m[1][2] = r[1]*r[2]*c1 - r[0]*s;
  m[1][3] = 0.;

  m[2][0] = r[2]*r[0]*c1 - r[1]*s;
  m[2][1] = r[2]*r[1]*c1 + r[0]*s;
  m[2][2] = r[2]*r[2]*c1 + c;
  m[2][3] = 0.;

  m[3][0] = 0.;
  m[3][1] = 0.;
  m[3][2] = 0.;
  m[3][3] = 1.;

  return m;
}
