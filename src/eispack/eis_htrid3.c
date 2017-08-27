/* htrid3.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int htrid3_(integer *nm, integer *n, doublereal *a, 
	doublereal *d__, doublereal *e, doublereal *e2, doublereal *tau)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal f, g, h__;
    integer i__, j, k, l;
    doublereal scale, fi, gi, hh;
    integer ii;
    doublereal si;
    extern doublereal pythag_(doublereal *, doublereal *);
    integer jm1, jp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF */
/*     THE ALGOL PROCEDURE TRED3, NUM. MATH. 11, 181-195(1968) */
/*     BY MARTIN, REINSCH, AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971). */

/*     THIS SUBROUTINE REDUCES A COMPLEX HERMITIAN MATRIX, STORED AS */
/*     A SINGLE SQUARE ARRAY, TO A REAL SYMMETRIC TRIDIAGONAL MATRIX */
/*     USING UNITARY SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        A CONTAINS THE LOWER TRIANGLE OF THE COMPLEX HERMITIAN INPUT */
/*          MATRIX.  THE REAL PARTS OF THE MATRIX ELEMENTS ARE STORED */
/*          IN THE FULL LOWER TRIANGLE OF A, AND THE IMAGINARY PARTS */
/*          ARE STORED IN THE TRANSPOSED POSITIONS OF THE STRICT UPPER */
/*          TRIANGLE OF A.  NO STORAGE IS REQUIRED FOR THE ZERO */
/*          IMAGINARY PARTS OF THE DIAGONAL ELEMENTS. */

/*     ON OUTPUT */

/*        A CONTAINS INFORMATION ABOUT THE UNITARY TRANSFORMATIONS */
/*          USED IN THE REDUCTION. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE THE TRIDIAGONAL MATRIX. 
*/

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL */
/*          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO. */

/*        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E. */
/*          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED. */

/*        TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    tau -= 3;
    --e2;
    --e;
    --d__;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    tau[(*n << 1) + 1] = 1.;
    tau[(*n << 1) + 2] = 0.;
/*     .......... FOR I=N STEP -1 UNTIL 1 DO -- .......... */
    i__1 = *n;
    for (ii = 1; ii <= i__1; ++ii) {
	i__ = *n + 1 - ii;
	l = i__ - 1;
	h__ = 0.;
	scale = 0.;
	if (l < 1) {
	    goto L130;
	}
/*     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) .......... */
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
/* L120: */
	    scale = scale + (d__1 = a[i__ + k * a_dim1], abs(d__1)) + (d__2 = 
		    a[k + i__ * a_dim1], abs(d__2));
	}

	if (scale != 0.) {
	    goto L140;
	}
	tau[(l << 1) + 1] = 1.;
	tau[(l << 1) + 2] = 0.;
L130:
	e[i__] = 0.;
	e2[i__] = 0.;
	goto L290;

L140:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    a[i__ + k * a_dim1] /= scale;
	    a[k + i__ * a_dim1] /= scale;
	    h__ = h__ + a[i__ + k * a_dim1] * a[i__ + k * a_dim1] + a[k + i__ 
		    * a_dim1] * a[k + i__ * a_dim1];
/* L150: */
	}

	e2[i__] = scale * scale * h__;
	g = sqrt(h__);
	e[i__] = scale * g;
	f = pythag_(&a[i__ + l * a_dim1], &a[l + i__ * a_dim1]);
/*     .......... FORM NEXT DIAGONAL ELEMENT OF MATRIX T .......... */
	if (f == 0.) {
	    goto L160;
	}
	tau[(l << 1) + 1] = (a[l + i__ * a_dim1] * tau[(i__ << 1) + 2] - a[
		i__ + l * a_dim1] * tau[(i__ << 1) + 1]) / f;
	si = (a[i__ + l * a_dim1] * tau[(i__ << 1) + 2] + a[l + i__ * a_dim1] 
		* tau[(i__ << 1) + 1]) / f;
	h__ += f * g;
	g = g / f + 1.;
	a[i__ + l * a_dim1] = g * a[i__ + l * a_dim1];
	a[l + i__ * a_dim1] = g * a[l + i__ * a_dim1];
	if (l == 1) {
	    goto L270;
	}
	goto L170;
L160:
	tau[(l << 1) + 1] = -tau[(i__ << 1) + 1];
	si = tau[(i__ << 1) + 2];
	a[i__ + l * a_dim1] = g;
L170:
	f = 0.;

	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    g = 0.;
	    gi = 0.;
	    if (j == 1) {
		goto L190;
	    }
	    jm1 = j - 1;
/*     .......... FORM ELEMENT OF A*U .......... */
	    i__3 = jm1;
	    for (k = 1; k <= i__3; ++k) {
		g = g + a[j + k * a_dim1] * a[i__ + k * a_dim1] + a[k + j * 
			a_dim1] * a[k + i__ * a_dim1];
		gi = gi - a[j + k * a_dim1] * a[k + i__ * a_dim1] + a[k + j * 
			a_dim1] * a[i__ + k * a_dim1];
/* L180: */
	    }

L190:
	    g += a[j + j * a_dim1] * a[i__ + j * a_dim1];
	    gi -= a[j + j * a_dim1] * a[j + i__ * a_dim1];
	    jp1 = j + 1;
	    if (l < jp1) {
		goto L220;
	    }

	    i__3 = l;
	    for (k = jp1; k <= i__3; ++k) {
		g = g + a[k + j * a_dim1] * a[i__ + k * a_dim1] - a[j + k * 
			a_dim1] * a[k + i__ * a_dim1];
		gi = gi - a[k + j * a_dim1] * a[k + i__ * a_dim1] - a[j + k * 
			a_dim1] * a[i__ + k * a_dim1];
/* L200: */
	    }
/*     .......... FORM ELEMENT OF P .......... */
L220:
	    e[j] = g / h__;
	    tau[(j << 1) + 2] = gi / h__;
	    f = f + e[j] * a[i__ + j * a_dim1] - tau[(j << 1) + 2] * a[j + 
		    i__ * a_dim1];
/* L240: */
	}

	hh = f / (h__ + h__);
/*     .......... FORM REDUCED A .......... */
	i__2 = l;
	for (j = 1; j <= i__2; ++j) {
	    f = a[i__ + j * a_dim1];
	    g = e[j] - hh * f;
	    e[j] = g;
	    fi = -a[j + i__ * a_dim1];
	    gi = tau[(j << 1) + 2] - hh * fi;
	    tau[(j << 1) + 2] = -gi;
	    a[j + j * a_dim1] -= (f * g + fi * gi) * 2.;
	    if (j == 1) {
		goto L260;
	    }
	    jm1 = j - 1;

	    i__3 = jm1;
	    for (k = 1; k <= i__3; ++k) {
		a[j + k * a_dim1] = a[j + k * a_dim1] - f * e[k] - g * a[i__ 
			+ k * a_dim1] + fi * tau[(k << 1) + 2] + gi * a[k + 
			i__ * a_dim1];
		a[k + j * a_dim1] = a[k + j * a_dim1] - f * tau[(k << 1) + 2] 
			- g * a[k + i__ * a_dim1] - fi * e[k] - gi * a[i__ + 
			k * a_dim1];
/* L250: */
	    }

L260:
	    ;
	}

L270:
	i__2 = l;
	for (k = 1; k <= i__2; ++k) {
	    a[i__ + k * a_dim1] = scale * a[i__ + k * a_dim1];
	    a[k + i__ * a_dim1] = scale * a[k + i__ * a_dim1];
/* L280: */
	}

	tau[(l << 1) + 2] = -si;
L290:
	d__[i__] = a[i__ + i__ * a_dim1];
	a[i__ + i__ * a_dim1] = scale * sqrt(h__);
/* L300: */
    }

    return 0;
} /* htrid3_ */

