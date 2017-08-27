/* tqlrat.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static doublereal c_b11 = 1.;

/* Subroutine */ int tqlrat_(integer *n, doublereal *d__, doublereal *e2, 
	integer *ierr)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal b=0.0, c__=0.0, f, g, h__;
    integer i__, j, l, m;
    doublereal p, r__, s, t;
    integer l1, ii;
    extern doublereal pythag_(doublereal *, doublereal *), epslon_(doublereal 
	    *);
    integer mml;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQLRAT, */
/*     ALGORITHM 464, COMM. ACM 16, 689(1973) BY REINSCH. */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC */
/*     TRIDIAGONAL MATRIX BY THE RATIONAL QL METHOD. */

/*     ON INPUT */

/*        N IS THE ORDER OF THE MATRIX. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX. */

/*        E2 CONTAINS THE SQUARES OF THE SUBDIAGONAL ELEMENTS OF THE */
/*          INPUT MATRIX IN ITS LAST N-1 POSITIONS.  E2(1) IS ARBITRARY. 
*/

/*      ON OUTPUT */

/*        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN */
/*          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND */
/*          ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE */
/*          THE SMALLEST EIGENVALUES. */

/*        E2 HAS BEEN DESTROYED. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE J-TH EIGENVALUE HAS NOT BEEN */
/*                     DETERMINED AFTER 30 ITERATIONS. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --e2;
    --d__;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* L100: */
	e2[i__ - 1] = e2[i__];
    }

    f = 0.;
    t = 0.;
    e2[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h__ = (d__1 = d__[l], abs(d__1)) + sqrt(e2[l]);
	if (t > h__) {
	    goto L105;
	}
	t = h__;
	b = epslon_(&t);
	c__ = b * b;
/*     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT ........
.. */
L105:
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (e2[m] <= c__) {
		goto L120;
	    }
/*     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT */
/*                THROUGH THE BOTTOM OF THE LOOP .......... */
/* L110: */
	}

L120:
	if (m == l) {
	    goto L210;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	l1 = l + 1;
	s = sqrt(e2[l]);
	g = d__[l];
	p = (d__[l1] - g) / (s * 2.);
	r__ = pythag_(&p, &c_b11);
	d__[l] = s / (p + d_sign(&r__, &p));
	h__ = g - d__[l];

	i__2 = *n;
	for (i__ = l1; i__ <= i__2; ++i__) {
/* L140: */
	    d__[i__] -= h__;
	}

	f += h__;
/*     .......... RATIONAL QL TRANSFORMATION .......... */
	g = d__[m];
	if (g == 0.) {
	    g = b;
	}
	h__ = g;
	s = 0.;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = m - ii;
	    p = g * h__;
	    r__ = p + e2[i__];
	    e2[i__ + 1] = s * r__;
	    s = e2[i__] / r__;
	    d__[i__ + 1] = h__ + s * (h__ + d__[i__]);
	    g = d__[i__] - e2[i__] / g;
	    if (g == 0.) {
		g = b;
	    }
	    h__ = g * p / r__;
/* L200: */
	}

	e2[l] = s * g;
	d__[l] = h__;
/*     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST ........
.. */
	if (h__ == 0.) {
	    goto L210;
	}
	if ((d__1 = e2[l], abs(d__1)) <= (d__2 = c__ / h__, abs(d__2))) {
	    goto L210;
	}
	e2[l] = h__ * e2[l];
	if (e2[l] != 0.) {
	    goto L130;
	}
L210:
	p = d__[l] + f;
/*     .......... ORDER EIGENVALUES .......... */
	if (l == 1) {
	    goto L250;
	}
/*     .......... FOR I=L STEP -1 UNTIL 2 DO -- .......... */
	i__2 = l;
	for (ii = 2; ii <= i__2; ++ii) {
	    i__ = l + 2 - ii;
	    if (p >= d__[i__ - 1]) {
		goto L270;
	    }
	    d__[i__] = d__[i__ - 1];
/* L230: */
	}

L250:
	i__ = 1;
L270:
	d__[i__] = p;
/* L290: */
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN */
/*                EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* tqlrat_ */

