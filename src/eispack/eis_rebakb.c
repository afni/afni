/* rebakb.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int rebakb_(integer *nm, integer *n, doublereal *b, 
	doublereal *dl, integer *m, doublereal *z__)
{
    /* System generated locals */
    integer b_dim1, b_offset, z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    integer i__, j, k;
    doublereal x;
    integer i1, ii;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE REBAKB, */
/*     NUM. MATH. 11, 99-110(1968) BY MARTIN AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 303-314(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A GENERALIZED */
/*     SYMMETRIC EIGENSYSTEM BY BACK TRANSFORMING THOSE OF THE */
/*     DERIVED SYMMETRIC MATRIX DETERMINED BY  REDUC2. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX SYSTEM. */

/*        B CONTAINS INFORMATION ABOUT THE SIMILARITY TRANSFORMATION */
/*          (CHOLESKY DECOMPOSITION) USED IN THE REDUCTION BY  REDUC2 */
/*          IN ITS STRICT LOWER TRIANGLE. */

/*        DL CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATION. */

/*        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED. */

/*        Z CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED */
/*          IN ITS FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        Z CONTAINS THE TRANSFORMED EIGENVECTORS */
/*          IN ITS FIRST M COLUMNS. */

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
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }

    i__1 = *m;
    for (j = 1; j <= i__1; ++j) {
/*     .......... FOR I=N STEP -1 UNTIL 1 DO -- .......... */
	i__2 = *n;
	for (ii = 1; ii <= i__2; ++ii) {
	    i1 = *n - ii;
	    i__ = i1 + 1;
	    x = dl[i__] * z__[i__ + j * z_dim1];
	    if (i__ == 1) {
		goto L80;
	    }

	    i__3 = i1;
	    for (k = 1; k <= i__3; ++k) {
/* L60: */
		x += b[i__ + k * b_dim1] * z__[k + j * z_dim1];
	    }

L80:
	    z__[i__ + j * z_dim1] = x;
/* L100: */
	}
    }

L200:
    return 0;
} /* rebakb_ */

