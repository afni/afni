/*
 * vp_linalg.c
 *
 * A simple linear algebra package.
 *
 * Copyright (c) 1994 The Board of Trustees of The Leland Stanford
 * Junior University.  All rights reserved.
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice and this permission notice appear in
 * all copies of this software and that you do not sell the software.
 * Commercial licensing is available by contacting the author.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Author:
 *    Phil Lacroute
 *    Computer Systems Laboratory
 *    Electrical Engineering Dept.
 *    Stanford University
 */

/*
 * $Date$
 * $Revision$
 */

#include "vp_global.h"

static void MatrixMult ANSI_ARGS((double* p, double *a, double *b,
				  int l, int m, int n));

/*
 * vpIdentity3
 *
 * Initialize a Matrix3 to the identity.
 */

void
vpIdentity3(m)
vpMatrix3 m;
{
    m[0][0] = 1.;    m[0][1] = 0.;    m[0][2] = 0.;
    m[1][0] = 0.;    m[1][1] = 1.;    m[1][2] = 0.;
    m[2][0] = 0.;    m[2][1] = 0.;    m[2][2] = 1.;
}

/*
 * vpIdentity4
 *
 * Initialize a Matrix4 to the identity.
 */

void
vpIdentity4(m)
vpMatrix4 m;
{
    m[0][0] = 1.;    m[0][1] = 0.;    m[0][2] = 0.;    m[0][3] = 0.;
    m[1][0] = 0.;    m[1][1] = 1.;    m[1][2] = 0.;    m[1][3] = 0.;
    m[2][0] = 0.;    m[2][1] = 0.;    m[2][2] = 1.;    m[2][3] = 0.;
    m[3][0] = 0.;    m[3][1] = 0.;    m[3][2] = 0.;    m[3][3] = 1.;
}

/*
 * vpNormalize3
 *
 * Normalize a vector (divide it by its magnitude).  Return VPERROR_SINGULAR
 * if the magnitude is too small.
 */

vpResult
vpNormalize3(v)
vpVector3 v;
{
    double magsqr, invmag;
    int i;

    magsqr = 0.;
    for (i = 0; i < 3; i++)
	magsqr += v[i]*v[i];
    if (fabs(magsqr) < VP_EPS)
	return(VPERROR_SINGULAR);
    invmag = 1. / sqrt(magsqr);
    for (i = 0; i < 3; i++)
	v[i] *= invmag;
    return(VP_OK);
}

/*
 * vpMatrixVectorMult4
 *
 * Perform the matrix-vector multiplication v2 = m*v1.
 */

void
vpMatrixVectorMult4(v2, m, v1)
vpVector4 v2, v1;
vpMatrix4 m;
{
    int i, j;

    for (i = 0; i < 4; i++) {
	v2[i] = 0;
	for (j = 0; j < 4; j++)
	    v2[i] += m[i][j] * v1[j];
    }
}

/*
 * vpMatrixMult4
 *
 * Perform the matrix multiplication m3 = m2 * m1.
 */

void
vpMatrixMult4(m3, m2, m1)
vpMatrix4 m3, m2, m1;
{
    MatrixMult((double *)m3, (double *)m2, (double *)m1, 4, 4, 4);
}

/*
 * MatrixMult
 *
 * Perform the matrix multiplication p = a * b.
 */

static void
MatrixMult(p, a, b, l, m, n)
double *p;	/* result matrix, size l by n */
double *a;	/* first factor, size l by m */
double *b;	/* second factor, size m by n */
int l, m, n;
{
    int i, j, k;

    if (l <= 0 || m <= 0 || n <= 0)
	VPBug("MatrixMult called with non-positive matrix size");
    for (i = 0; i < l; i++) {
	for (j = 0; j < n; j++) {
	    p[i*n+j] = 0;
	    for (k = 0; k < m; k++)
		p[i*n+j] += a[i*n+k] * b[k*n+j];
	}
    }
}

/*
 * vpCrossProduct
 *
 * Compute the cross product p = v * w.
 */

void
vpCrossProduct(p, v, w)
vpVector3 p, v, w;
{
    p[0] = v[1]*w[2] - v[2]*w[1];
    p[1] = v[2]*w[0] - v[0]*w[2];
    p[2] = v[0]*w[1] - v[1]*w[0];
}

/*
 * vpSolveSystem4
 *
 * Solve the linear system a*xi = bi where a is a 4-by-4 matrix and bi
 * is a column of the 4-by-m matrix b.  Each column bi in b is replaced
 * by the corresponding solution vector xi.  The matrix a is destroyed.
 * The method used is Gauss-Jordan elimination with partial pivoting and
 * implicit scaling (based on the discussion in Numerical Recipes in C
 * by Press, Flannery, Teukolsky and Vetterling).
 *
 * Return VPERROR_SINGULAR if matrix is singular.
 */

vpResult
vpSolveSystem4(a, b, m)
vpMatrix4 a;	/* linear system matrix */
double **b;	/* RHS vectors on input, solution vectors on output;
		   b[i] is a Vector4 */
int m;		/* number of vectors in b */
{
    vpVector4 row_scale_factor;	/* normalization for each row */
    int ipivot;			/* row containing pivot */
    int pivot[4];		/* after the reduction loop, row i has
				   been pivoted to row pivot[i] */
    int i, j, k, l;		/* loop indices */
    double *aptr;		/* pointer into a */
    double entry;		/* entry in a */
    double max_entry;		/* maximum entry in row */
    double inv_entry;		/* inverse of an entry in a */
    vpVector4 tmpv;		/* temporary vector for undoing row
				   interchange in solution vectors */

    /* initialize */
    for (i = 0; i < 4; i++)
	pivot[i] = -1;

    /* find the largest element in each row and compute normalization
       for implicit scaling */
    aptr = &a[0][0];
    for (i = 0; i < 4; i++) {
	max_entry = 0.;
	for (j = 0; j < 4; j++) {
	    if (*aptr < 0) {
		if (-*aptr > max_entry)
		    max_entry = -*aptr;
	    } else {
		if (*aptr > max_entry)
		    max_entry = *aptr;
	    }
	    aptr++;
	}
	if (fabs(max_entry) < VP_EPS)
	    return(VPERROR_SINGULAR);
	row_scale_factor[i] = 1. / max_entry;
    }

    /* loop over the columns of a */
    for (j = 0; j < 4; j++) {
	/* loop over the rows of a and choose a pivot element in the
	   current column, ignoring rows containing previous pivots */
	max_entry = 0.;
	for (i = 0; i < 4; i++) {
	    if (pivot[i] < 0) {
		entry = a[i][j] * row_scale_factor[i];
		if (entry < 0) {
		    if (-entry > max_entry) {
			max_entry = -entry;
			ipivot = i;
		    }
		} else {
		    if (entry > max_entry) {
			max_entry = entry;
			ipivot = i;
		    }
		}
	    }
	}
	if (fabs(max_entry) < VP_EPS)
	    return(VPERROR_SINGULAR);
	pivot[ipivot] = j;
	inv_entry = 1. / a[ipivot][j];

	/* scale the pivot row by the pivot element */
	for (l = j+1; l < 4; l++)
	    a[ipivot][l] *= inv_entry;
	for (l = 0; l < m; l++)
	    b[l][ipivot] *= inv_entry;

	/* subtract a multiple of the pivot row from the other rows */
	for (k = 0; k < 4; k++) {
	    if (k != ipivot) {
		entry = a[k][j];
		for (l = j+1; l < 4; l++)
		    a[k][l] -= a[ipivot][l] * entry;
		for (l = 0; l < m; l++)
		    b[l][k] -= b[l][ipivot] * entry;
	    }
	}
    }

    /* undo row interchanges in solution vectors */
    for (j = 0; j < m; j++) {
	for (i = 0; i < 4; i++)
	    tmpv[pivot[i]] = b[j][i];
	for (i = 0; i < 4; i++)
	    b[j][i] = tmpv[i];
    }
    return(VP_OK);
}

/*
 * VPLoadTranslation
 *
 * Load a translation matrix.
 */

void
VPLoadTranslation(m, tx, ty, tz)
vpMatrix4 m;
double tx, ty, tz;
{
    vpIdentity4(m);
    m[0][3] = tx;
    m[1][3] = ty;
    m[2][3] = tz;
}

/*
 * VPLoadRotation
 *
 * Load a rotation matrix.
 */

void
VPLoadRotation(m, axis, degrees)
vpMatrix4 m;
int axis;
double degrees;
{
    vpMatrix4 tmp;
    double radians, sintheta, costheta;

    radians = degrees * M_PI / 180.;
    sintheta = sin(radians);
    costheta = cos(radians);
    vpIdentity4(m);
    switch (axis) {
    case VP_X_AXIS:
	m[1][1] = costheta;
	m[1][2] = sintheta;
	m[2][1] = -sintheta;
	m[2][2] = costheta;
	break;
    case VP_Y_AXIS:
	m[0][0] = costheta;
	m[0][2] = -sintheta;
	m[2][0] = sintheta;
	m[2][2] = costheta;
	break;
    case VP_Z_AXIS:
	m[0][0] = costheta;
	m[0][1] = sintheta;
	m[1][0] = -sintheta;
	m[1][1] = costheta;
	break;
    default:
	VPBug("bad axis in VPLoadRotation");
    }
}

/*
 * VPLoadScale
 *
 * Load a scale matrix.
 */

void
VPLoadScale(m, sx, sy, sz)
vpMatrix4 m;
double sx, sy, sz;
{
    vpIdentity4(m);
    m[0][0] = sx;
    m[1][1] = sy;
    m[2][2] = sz;
}
