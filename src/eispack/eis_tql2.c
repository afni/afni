/* tql2.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static doublereal c_b10 = 1.;

/* Subroutine */ int tql2_(integer *nm, integer *n, doublereal *d__, 
	doublereal *e, doublereal *z__, integer *ierr)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal c__, f, g, h__;
    integer i__, j, k, l, m;
    doublereal p=0.0, r__=0.0, s=0.0, c2=0.0, c3=0.0;
    integer l1, l2;
    doublereal s2=0.0;
    integer ii;
    extern doublereal pythag_(doublereal *, doublereal *);
    doublereal dl1, el1;
    integer mml;
    doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQL2, */
/*     NUM. MATH. 11, 293-306(1968) BY BOWDLER, MARTIN, REINSCH, AND */
/*     WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971). */

/*     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS */
/*     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE QL METHOD. */
/*     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO */
/*     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS */
/*     FULL MATRIX TO TRIDIAGONAL FORM. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX */
/*          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY. */

/*        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE */
/*          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS */
/*          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN */
/*          THE IDENTITY MATRIX. */

/*      ON OUTPUT */

/*        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN */
/*          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT */
/*          UNORDERED FOR INDICES 1,2,...,IERR-1. */

/*        E HAS BEEN DESTROYED. */

/*        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC */
/*          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE, */
/*          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED */
/*          EIGENVALUES. */

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
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --e;
    --d__;

    /* Function Body */
    *ierr = 0;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
/* L100: */
	e[i__ - 1] = e[i__];
    }

    f = 0.;
    tst1 = 0.;
    e[*n] = 0.;

    i__1 = *n;
    for (l = 1; l <= i__1; ++l) {
	j = 0;
	h__ = (d__1 = d__[l], abs(d__1)) + (d__2 = e[l], abs(d__2));
	if (tst1 < h__) {
	    tst1 = h__;
	}
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT .......... */
	i__2 = *n;
	for (m = l; m <= i__2; ++m) {
	    tst2 = tst1 + (d__1 = e[m], abs(d__1));
	    if (tst2 == tst1) {
		goto L120;
	    }
/*     .......... E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT */
/*                THROUGH THE BOTTOM OF THE LOOP .......... */
/* L110: */
	}

L120:
	if (m == l) {
	    goto L220;
	}
L130:
	if (j == 30) {
	    goto L1000;
	}
	++j;
/*     .......... FORM SHIFT .......... */
	l1 = l + 1;
	l2 = l1 + 1;
	g = d__[l];
	p = (d__[l1] - g) / (e[l] * 2.);
	r__ = pythag_(&p, &c_b10);
	d__[l] = e[l] / (p + d_sign(&r__, &p));
	d__[l1] = e[l] * (p + d_sign(&r__, &p));
	dl1 = d__[l1];
	h__ = g - d__[l];
	if (l2 > *n) {
	    goto L145;
	}

	i__2 = *n;
	for (i__ = l2; i__ <= i__2; ++i__) {
/* L140: */
	    d__[i__] -= h__;
	}

L145:
	f += h__;
/*     .......... QL TRANSFORMATION .......... */
	p = d__[m];
	c__ = 1.;
	c2 = c__;
	el1 = e[l1];
	s = 0.;
	mml = m - l;
/*     .......... FOR I=M-1 STEP -1 UNTIL L DO -- .......... */
	i__2 = mml;
	for (ii = 1; ii <= i__2; ++ii) {
	    c3 = c2;
	    c2 = c__;
	    s2 = s;
	    i__ = m - ii;
	    g = c__ * e[i__];
	    h__ = c__ * p;
	    r__ = pythag_(&p, &e[i__]);
	    e[i__ + 1] = s * r__;
	    s = e[i__] / r__;
	    c__ = p / r__;
	    p = c__ * d__[i__] - s * g;
	    d__[i__ + 1] = h__ + s * (c__ * g + s * d__[i__]);
/*     .......... FORM VECTOR .......... */
	    i__3 = *n;
	    for (k = 1; k <= i__3; ++k) {
		h__ = z__[k + (i__ + 1) * z_dim1];
		z__[k + (i__ + 1) * z_dim1] = s * z__[k + i__ * z_dim1] + c__ 
			* h__;
		z__[k + i__ * z_dim1] = c__ * z__[k + i__ * z_dim1] - s * h__;
/* L180: */
	    }

/* L200: */
	}

	p = -s * s2 * c3 * el1 * e[l] / dl1;
	e[l] = s * p;
	d__[l] = c__ * p;
	tst2 = tst1 + (d__1 = e[l], abs(d__1));
	if (tst2 > tst1) {
	    goto L130;
	}
L220:
	d__[l] += f;
/* L240: */
    }
/*     .......... ORDER EIGENVALUES AND EIGENVECTORS .......... */
    i__1 = *n;
    for (ii = 2; ii <= i__1; ++ii) {
	i__ = ii - 1;
	k = i__;
	p = d__[i__];

	i__2 = *n;
	for (j = ii; j <= i__2; ++j) {
	    if (d__[j] >= p) {
		goto L260;
	    }
	    k = j;
	    p = d__[j];
L260:
	    ;
	}

	if (k == i__) {
	    goto L300;
	}
	d__[k] = d__[i__];
	d__[i__] = p;

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    p = z__[j + i__ * z_dim1];
	    z__[j + i__ * z_dim1] = z__[j + k * z_dim1];
	    z__[j + k * z_dim1] = p;
/* L280: */
	}

L300:
	;
    }

    goto L1001;
/*     .......... SET ERROR -- NO CONVERGENCE TO AN */
/*                EIGENVALUE AFTER 30 ITERATIONS .......... */
L1000:
    *ierr = l;
L1001:
    return 0;
} /* tql2_ */

