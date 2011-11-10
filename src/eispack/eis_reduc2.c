/* reduc2.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int reduc2_(integer *nm, integer *n, doublereal *a, 
	doublereal *b, doublereal *dl, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__, j, k;
    doublereal x, y=0.0;
    integer i1, j1, nn;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE REDUC2, */
/*     NUM. MATH. 11, 99-110(1968) BY MARTIN AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 303-314(1971). */

/*     THIS SUBROUTINE REDUCES THE GENERALIZED SYMMETRIC EIGENPROBLEMS */
/*     ABX=(LAMBDA)X OR BAY=(LAMBDA)Y, WHERE B IS POSITIVE DEFINITE, */
/*     TO THE STANDARD SYMMETRIC EIGENPROBLEM USING THE CHOLESKY */
/*     FACTORIZATION OF B. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRICES A AND B.  IF THE CHOLESKY */
/*          FACTOR L OF B IS ALREADY AVAILABLE, N SHOULD BE PREFIXED */
/*          WITH A MINUS SIGN. */

/*        A AND B CONTAIN THE REAL SYMMETRIC INPUT MATRICES.  ONLY THE */
/*          FULL UPPER TRIANGLES OF THE MATRICES NEED BE SUPPLIED.  IF */
/*          N IS NEGATIVE, THE STRICT LOWER TRIANGLE OF B CONTAINS, */
/*          INSTEAD, THE STRICT LOWER TRIANGLE OF ITS CHOLESKY FACTOR L. 
*/

/*        DL CONTAINS, IF N IS NEGATIVE, THE DIAGONAL ELEMENTS OF L. */

/*     ON OUTPUT */

/*        A CONTAINS IN ITS FULL LOWER TRIANGLE THE FULL LOWER TRIANGLE */
/*          OF THE SYMMETRIC MATRIX DERIVED FROM THE REDUCTION TO THE */
/*          STANDARD FORM.  THE STRICT UPPER TRIANGLE OF A IS UNALTERED. 
*/

/*        B CONTAINS IN ITS STRICT LOWER TRIANGLE THE STRICT LOWER */
/*          TRIANGLE OF ITS CHOLESKY FACTOR L.  THE FULL UPPER */
/*          TRIANGLE OF B IS UNALTERED. */

/*        DL CONTAINS THE DIAGONAL ELEMENTS OF L. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          7*N+1      IF B IS NOT POSITIVE DEFINITE. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --dl;
    b_dim1 = *nm;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    *ierr = 0;
    nn = abs(*n);
    if (*n < 0) {
	goto L100;
    }
/*     .......... FORM L IN THE ARRAYS B AND DL .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i1 = i__ - 1;

	i__2 = *n;
	for (j = i__; j <= i__2; ++j) {
	    x = b[i__ + j * b_dim1];
	    if (i__ == 1) {
		goto L40;
	    }

	    i__3 = i1;
	    for (k = 1; k <= i__3; ++k) {
/* L20: */
		x -= b[i__ + k * b_dim1] * b[j + k * b_dim1];
	    }

L40:
	    if (j != i__) {
		goto L60;
	    }
	    if (x <= 0.) {
		goto L1000;
	    }
	    y = sqrt(x);
	    dl[i__] = y;
	    goto L80;
L60:
	    b[j + i__ * b_dim1] = x / y;
L80:
	    ;
	}
    }
/*     .......... FORM THE LOWER TRIANGLE OF A*L */
/*                IN THE LOWER TRIANGLE OF THE ARRAY A .......... */
L100:
    i__2 = nn;
    for (i__ = 1; i__ <= i__2; ++i__) {
	i1 = i__ + 1;

	i__1 = i__;
	for (j = 1; j <= i__1; ++j) {
	    x = a[j + i__ * a_dim1] * dl[j];
	    if (j == i__) {
		goto L140;
	    }
	    j1 = j + 1;

	    i__3 = i__;
	    for (k = j1; k <= i__3; ++k) {
/* L120: */
		x += a[k + i__ * a_dim1] * b[k + j * b_dim1];
	    }

L140:
	    if (i__ == nn) {
		goto L180;
	    }

	    i__3 = nn;
	    for (k = i1; k <= i__3; ++k) {
/* L160: */
		x += a[i__ + k * a_dim1] * b[k + j * b_dim1];
	    }

L180:
	    a[i__ + j * a_dim1] = x;
/* L200: */
	}
    }
/*     .......... PRE-MULTIPLY BY TRANSPOSE(L) AND OVERWRITE .......... */
    i__1 = nn;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i1 = i__ + 1;
	y = dl[i__];

	i__2 = i__;
	for (j = 1; j <= i__2; ++j) {
	    x = y * a[i__ + j * a_dim1];
	    if (i__ == nn) {
		goto L280;
	    }

	    i__3 = nn;
	    for (k = i1; k <= i__3; ++k) {
/* L260: */
		x += a[k + j * a_dim1] * b[k + i__ * b_dim1];
	    }

L280:
	    a[i__ + j * a_dim1] = x;
/* L300: */
	}
    }

    goto L1001;
/*     .......... SET ERROR -- B IS NOT POSITIVE DEFINITE .......... */
L1000:
    *ierr = *n * 7 + 1;
L1001:
    return 0;
} /* reduc2_ */

