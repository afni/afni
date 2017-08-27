/* trbak3.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int trbak3_(integer *nm, integer *n, integer *nv, doublereal 
	*a, integer *m, doublereal *z__)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    doublereal h__;
    integer i__, j, k, l;
    doublereal s;
    integer ik, iz;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRBAK3, */
/*     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A REAL SYMMETRIC */
/*     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING */
/*     SYMMETRIC TRIDIAGONAL MATRIX DETERMINED BY  TRED3. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        NV MUST BE SET TO THE DIMENSION OF THE ARRAY PARAMETER A */
/*          AS DECLARED IN THE CALLING PROGRAM DIMENSION STATEMENT. */

/*        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANSFORMATIONS */
/*          USED IN THE REDUCTION BY  TRED3  IN ITS FIRST */
/*          N*(N+1)/2 POSITIONS. */

/*        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED. */

/*        Z CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED */
/*          IN ITS FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        Z CONTAINS THE TRANSFORMED EIGENVECTORS */
/*          IN ITS FIRST M COLUMNS. */

/*     NOTE THAT TRBAK3 PRESERVES VECTOR EUCLIDEAN NORMS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --a;
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
	iz = i__ * l / 2;
	ik = iz + i__;
	h__ = a[ik];
	if (h__ == 0.) {
	    goto L140;
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    s = 0.;
	    ik = iz;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		++ik;
		s += a[ik] * z__[k + j * z_dim1];
/* L110: */
	    }
/*     .......... DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ......
.... */
	    s = s / h__ / h__;
	    ik = iz;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		++ik;
		z__[k + j * z_dim1] -= s * a[ik];
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* trbak3_ */

