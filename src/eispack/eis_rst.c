/* rst.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int rst_(integer *nm, integer *n, doublereal *w, doublereal *
	e, integer *matz, doublereal *z__, integer *ierr)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int imtql1_(integer *, doublereal *, doublereal *,
	     integer *), imtql2_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *);



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED) */
/*     OF A REAL SYMMETRIC TRIDIAGONAL MATRIX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRIX. */

/*        W  CONTAINS THE DIAGONAL ELEMENTS OF THE REAL */
/*        SYMMETRIC TRIDIAGONAL MATRIX. */

/*        E  CONTAINS THE SUBDIAGONAL ELEMENTS OF THE MATRIX IN */
/*        ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY. */

/*        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF */
/*        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO */
/*        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS. */

/*     ON OUTPUT */

/*        W  CONTAINS THE EIGENVALUES IN ASCENDING ORDER. */

/*        Z  CONTAINS THE EIGENVECTORS IF MATZ IS NOT ZERO. */

/*        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR */
/*           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR IMTQL1 */
/*           AND IMTQL2.  THE NORMAL COMPLETION CODE IS ZERO. */

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
    --w;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    if (*matz != 0) {
	goto L20;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    imtql1_(n, &w[1], &e[1], ierr);
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    z__[j + i__ * z_dim1] = 0.;
/* L30: */
	}

	z__[i__ + i__ * z_dim1] = 1.;
/* L40: */
    }

    imtql2_(nm, n, &w[1], &e[1], &z__[z_offset], ierr);
L50:
    return 0;
} /* rst_ */

