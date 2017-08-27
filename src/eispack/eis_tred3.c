/* tred3.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int tred3_(integer *n, integer *nv, doublereal *a, 
	doublereal *d__, doublereal *e, doublereal *e2)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal f, g, h__;
    integer i__, j, k, l;
    doublereal scale, hh;
    integer ii, jk, iz, jm1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED3, */
/*     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971). */

/*     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX, STORED AS */
/*     A ONE-DIMENSIONAL ARRAY, TO A SYMMETRIC TRIDIAGONAL MATRIX */
/*     USING ORTHOGONAL SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        N IS THE ORDER OF THE MATRIX. */

/*        NV MUST BE SET TO THE DIMENSION OF THE ARRAY PARAMETER A */
/*          AS DECLARED IN THE CALLING PROGRAM DIMENSION STATEMENT. */

/*        A CONTAINS THE LOWER TRIANGLE OF THE REAL SYMMETRIC */
/*          INPUT MATRIX, STORED ROW-WISE AS A ONE-DIMENSIONAL */
/*          ARRAY, IN ITS FIRST N*(N+1)/2 POSITIONS. */

/*     ON OUTPUT */

/*        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL */
/*          TRANSFORMATIONS USED IN THE REDUCTION. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL */
/*          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO. */

/*        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E. */
/*          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

/*     .......... FOR I=N STEP -1 UNTIL 1 DO -- .......... */
    /* Parameter adjustments */
    --e2;
    --e;
    --d__;
    --a;

    /* Function Body */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = *n + 1 - ii;
	l = i__ - 1;
	iz = i__ * l / 2;
	h__ = 0.;
	scale = 0.;
	if (l < 1) {
	    goto L130;
	}
/*     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    ++iz;
	    d__[k] = a[iz];
	    scale += (d__1 = d__[k], abs(d__1));
/* L120: */
	}

	if (scale != 0.) {
	    goto L140;
	}
L130:
	e[i__] = 0.;
	e2[i__] = 0.;
	goto L290;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    d__[k] /= scale;
	    h__ += d__[k] * d__[k];
/* L150: */
	}

	e2[i__] = scale * scale * h__;
	f = d__[l];
	d__1 = sqrt(h__);
	g = -d_sign(&d__1, &f);
	e[i__] = scale * g;
	h__ -= f * g;
	d__[l] = f - g;
	a[iz] = scale * d__[l];
	if (l == 1) {
	    goto L290;
	}
	jk = 1;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d__[j];
	    g = 0.;
	    jm1 = j - 1;
	    if (jm1 < 1) {
		goto L220;
	    }

	    i__3 = jm1;
	    for (k = 1; k <= i__3; ++k) {
		g += a[jk] * d__[k];
		e[k] += a[jk] * f;
		++jk;
/* L200: */
	    }

L220:
	    e[j] = g + a[jk] * f;
	    ++jk;
/* L240: */
	}
/*     .......... FORM P .......... */
	f = 0.;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    e[j] /= h__;
	    f += e[j] * d__[j];
/* L245: */
	}

	hh = f / (h__ + h__);
/*     .......... FORM Q .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
/* L250: */
	    e[j] -= hh * d__[j];
	}

	jk = 1;
/*     .......... FORM REDUCED A .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = d__[j];
	    g = e[j];

	    i__3 = j;
	    for (k = 1; k <= i__3; ++k) {
		a[jk] = a[jk] - f * e[k] - g * d__[k];
		++jk;
/* L260: */
	    }

/* L280: */
	}

L290:
	d__[i__] = a[iz + 1];
	a[iz + 1] = scale * sqrt(h__);
/* L300: */
    }

    return 0;
} /* tred3_ */

