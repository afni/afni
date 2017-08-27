/* htrib3.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int htrib3_(integer *nm, integer *n, doublereal *a, 
	doublereal *tau, integer *m, doublereal *zr, doublereal *zi)
{
    /* System generated locals */
    integer a_dim1, a_offset, zr_dim1, zr_offset, zi_dim1, zi_offset, i__1, 
	    i__2, i__3;

    /* Local variables */
    doublereal h__;
    integer i__, j, k, l;
    doublereal s, si;



/*     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF */
/*     THE ALGOL PROCEDURE TRBAK3, NUM. MATH. 11, 181-195(1968) */
/*     BY MARTIN, REINSCH, AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A COMPLEX HERMITIAN */
/*     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING */
/*     REAL SYMMETRIC TRIDIAGONAL MATRIX DETERMINED BY  HTRID3. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        A CONTAINS INFORMATION ABOUT THE UNITARY TRANSFORMATIONS */
/*          USED IN THE REDUCTION BY  HTRID3. */

/*        TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS. */

/*        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED. */

/*        ZR CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED */
/*          IN ITS FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE TRANSFORMED EIGENVECTORS */
/*          IN THEIR FIRST M COLUMNS. */

/*     NOTE THAT THE LAST COMPONENT OF EACH RETURNED VECTOR */
/*     IS REAL AND THAT VECTOR EUCLIDEAN NORMS ARE PRESERVED. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    tau -= 3;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }
/*     .......... TRANSFORM THE EIGENVECTORS OF THE REAL SYMMETRIC */
/*                TRIDIAGONAL MATRIX TO THOSE OF THE HERMITIAN */
/*                TRIDIAGONAL MATRIX. .......... */
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    zi[k + j * zi_dim1] = -zr[k + j * zr_dim1] * tau[(k << 1) + 2];
	    zr[k + j * zr_dim1] *= tau[(k << 1) + 1];
/* L50: */
	}
    }

    if (*n == 1) {
	goto L200;
    }
/*     .......... RECOVER AND APPLY THE HOUSEHOLDER MATRICES .......... */
    i__2 = *n;
    for (i__ = 2; i__ <= i__2; ++i__) {
	l = i__ - 1;
	h__ = a[i__ + i__ * a_dim1];
	if (h__ == 0.) {
	    goto L140;
	}

	i__1 = *m;
	for (j = 1; j <= i__1; ++j) {
	    s = 0.;
	    si = 0.;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		s = s + a[i__ + k * a_dim1] * zr[k + j * zr_dim1] - a[k + i__ 
			* a_dim1] * zi[k + j * zi_dim1];
		si = si + a[i__ + k * a_dim1] * zi[k + j * zi_dim1] + a[k + 
			i__ * a_dim1] * zr[k + j * zr_dim1];
/* L110: */
	    }
/*     .......... DOUBLE DIVISIONS AVOID POSSIBLE UNDERFLOW ......
.... */
	    s = s / h__ / h__;
	    si = si / h__ / h__;

	    i__3 = l;
	    for (k = 1; k <= i__3; ++k) {
		zr[k + j * zr_dim1] = zr[k + j * zr_dim1] - s * a[i__ + k * 
			a_dim1] - si * a[k + i__ * a_dim1];
		zi[k + j * zi_dim1] = zi[k + j * zi_dim1] - si * a[i__ + k * 
			a_dim1] + s * a[k + i__ * a_dim1];
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* htrib3_ */

