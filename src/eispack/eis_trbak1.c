/* trbak1.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int trbak1_(integer *nm, integer *n, doublereal *a, 
	doublereal *e, integer *m, doublereal *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    integer i__, j, k, l;
    doublereal s;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRBAK1, */
/*     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A REAL SYMMETRIC */
/*     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING */
/*     SYMMETRIC TRIDIAGONAL MATRIX DETERMINED BY  TRED1. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANS- */
/*          FORMATIONS USED IN THE REDUCTION BY  TRED1 */
/*          IN ITS STRICT LOWER TRIANGLE. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL */
/*          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY. */

/*        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED. */

/*        Z CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED */
/*          IN ITS FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        Z CONTAINS THE TRANSFORMED EIGENVECTORS */
/*          IN ITS FIRST M COLUMNS. */

/*     NOTE THAT TRBAK1 PRESERVES VECTOR EUCLIDEAN NORMS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --e;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }
    if (*n == 1) {
	goto L200;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	l = i__ - 1;
	if (e[i__] == 0.) {
	    goto L140;
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    s = 0.;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
/* L110: */
		s += a[i__ + k * a_dim1] * z__[k + j * z_dim1];
	    }
/*     .......... DIVISOR BELOW IS NEGATIVE OF H FORMED IN TRED1. 
*/
/*                DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ......
.... */
	    s = s / a[i__ + l * a_dim1] / e[i__];

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
/* L120: */
		z__[k + j * z_dim1] += s * a[i__ + k * a_dim1];
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* trbak1_ */

