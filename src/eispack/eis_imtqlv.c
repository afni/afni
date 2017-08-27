/* imtqlv.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static doublereal c_b11 = 1.;

/* Subroutine */ int imtqlv_(integer *n, doublereal *d__, doublereal *e, 
	doublereal *e2, doublereal *w, integer *ind, integer *ierr, 
	doublereal *rv1)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal b, c__, f, g;
    integer i__, j, k, l, m;
    doublereal p, r__, s;
    integer ii;
    extern doublereal pythag_(doublereal *, doublereal *);
    integer tag, mml;
    doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A VARIANT OF  IMTQL1  WHICH IS A TRANSLATION OF 
*/
/*     ALGOL PROCEDURE IMTQL1, NUM. MATH. 12, 377-383(1968) BY MARTIN AND 
*/
/*     WILKINSON, AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971). */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC TRIDIAGONAL */
/*     MATRIX BY THE IMPLICIT QL METHOD AND ASSOCIATES WITH THEM */
/*     THEIR CORRESPONDING SUBMATRIX INDICES. */

/*     ON INPUT */

/*        N IS THE ORDER OF THE MATRIX. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX */
/*          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY. */

/*        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E. */
/*          E2(1) IS ARBITRARY. */

/*     ON OUTPUT */

/*        D AND E ARE UNALTERED. */

/*        ELEMENTS OF E2, CORRESPONDING TO ELEMENTS OF E REGARDED */
/*          AS NEGLIGIBLE, HAVE BEEN REPLACED BY ZERO CAUSING THE */
/*          MATRIX TO SPLIT INTO A DIRECT SUM OF SUBMATRICES. */
/*          E2(1) IS ALSO SET TO ZERO. */

/*        W CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN */
/*          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND */
/*          ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE */
/*          THE SMALLEST EIGENVALUES. */

/*        IND CONTAINS THE SUBMATRIX INDICES ASSOCIATED WITH THE */
/*          CORRESPONDING EIGENVALUES IN W -- 1 FOR EIGENVALUES */
/*          BELONGING TO THE FIRST SUBMATRIX FROM THE TOP, */
/*          2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC.. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          J          IF THE J-TH EIGENVALUE HAS NOT BEEN */
/*                     DETERMINED AFTER 30 ITERATIONS. */

/*        RV1 IS A TEMPORARY STORAGE ARRAY. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --rv1;
    --ind;
    --w;
    --e2;
    --e;
    --d__;

    /* Function Body */
    *ierr = 0;
    k = 0;
    tag = 0;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	w[i__] = d__[i__];
	if (i__ != 1) {
	    rv1[i__ - 1] = e[i__];
	}
/* L100: */
    }

    e2[1] = 0.;
    rv1[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT .......... */
L105:
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    if (m == *n) {
		goto L120;
	    }
	    tst1 = (d__1 = w[m], abs(d__1)) + (d__2 = w[m + 1], abs(d__2));
	    tst2 = tst1 + (d__1 = rv1[m], abs(d__1));
	    if (tst2 == tst1) {
		goto L120;
	    }
/*     .......... GUARD AGAINST UNDERFLOWED ELEMENT OF E2 ........
.. */
	    if (e2[m + 1] == 0.) {
		goto L125;
	    }
/* L110: */
	}

L120:
	if (m <= k) {
	    goto L130;
	}
	if (m != *n) {
	    e2[m + 1] = 0.;
	}
L125:
	k = m;
	++tag;
L130:
	p = w[l];
	if (m == l) {
	    goto L215;
	}
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	g = (w[l + 1] - p) / (rv1[l] * 2.);
	r__ = pythag_(&g, &c_b11);
	g = w[m] - p + rv1[l] / (g + d_sign(&r__, &g));
	s = 1.;
	c__ = 1.;
	p = 0.;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    i__ = m - ii;
	    f = s * rv1[i__];
	    b = c__ * rv1[i__];
	    r__ = pythag_(&f, &g);
	    rv1[i__ + 1] = r__;
	    if (r__ == 0.) {
		goto L210;
	    }
	    s = f / r__;
	    c__ = g / r__;
	    g = w[i__ + 1] - p;
	    r__ = (w[i__] - g) * s + c__ * 2. * b;
	    p = s * r__;
	    w[i__ + 1] = g + p;
	    g = c__ * r__ - b;
/* L200: */
	}

	w[l] -= p;
	rv1[l] = g;
	rv1[m] = 0.;
	goto L105;
/*     .......... RECOVER FROM UNDERFLOW .......... */
L210:
	w[i__ + 1] -= p;
	rv1[m] = 0.;
	goto L105;
/*     .......... ORDER EIGENVALUES .......... */
L215:
	if (l == 1) {
	    goto L250;
	}
/*     .......... FOR I=L STEP -1 UNTIL 2 DO -- .......... */
	i__2 = l;
	for (ii = 2; ii <= i__2; ++ii) {
	    i__ = l + 2 - ii;
	    if (p >= w[i__ - 1]) {
		goto L270;
	    }
	    w[i__] = w[i__ - 1];
	    ind[i__] = ind[i__ - 1];
/* L230: */
	}

L250:
	i__ = 1;
L270:
	w[i__] = p;
	ind[i__] = tag;
/* L290: */
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN */
/*                EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* imtqlv_ */

