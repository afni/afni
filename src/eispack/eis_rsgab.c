/* rsgab.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int rsgab_(integer *nm, integer *n, doublereal *a, 
	doublereal *b, doublereal *w, integer *matz, doublereal *z__, 
	doublereal *fv1, doublereal *fv2, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int tred1_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), tred2_(integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *)
	    , rebak_(integer *, integer *, doublereal *, doublereal *, 
	    integer *, doublereal *), reduc2_(integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *), tqlrat_(
	    integer *, doublereal *, doublereal *, integer *), tql2_(integer *
	    , integer *, doublereal *, doublereal *, doublereal *, integer *);



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED) */
/*     FOR THE REAL SYMMETRIC GENERALIZED EIGENPROBLEM  ABX = (LAMBDA)X. 
*/

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRICES  A  AND  B. */

/*        A  CONTAINS A REAL SYMMETRIC MATRIX. */

/*        B  CONTAINS A POSITIVE DEFINITE REAL SYMMETRIC MATRIX. */

/*        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF */
/*        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO */
/*        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS. */

/*     ON OUTPUT */

/*        W  CONTAINS THE EIGENVALUES IN ASCENDING ORDER. */

/*        Z  CONTAINS THE EIGENVECTORS IF MATZ IS NOT ZERO. */

/*        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR */
/*           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR TQLRAT */
/*           AND TQL2.  THE NORMAL COMPLETION CODE IS ZERO. */

/*        FV1  AND  FV2  ARE TEMPORARY STORAGE ARRAYS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --fv2;
    --fv1;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --w;
    b_dim1 = *nm;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    reduc2_(nm, n, &a[a_offset], &b[b_offset], &fv2[1], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    if (*matz != 0) {
	goto L20;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    tred1_(nm, n, &a[a_offset], &w[1], &fv1[1], &fv2[1]);
    tqlrat_(n, &w[1], &fv2[1], ierr);
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L20:
    tred2_(nm, n, &a[a_offset], &w[1], &fv1[1], &z__[z_offset]);
    tql2_(nm, n, &w[1], &fv1[1], &z__[z_offset], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    rebak_(nm, n, &b[b_offset], &fv2[1], n, &z__[z_offset]);
L50:
    return 0;
} /* rsgab_ */

