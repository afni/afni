/* rsp.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int rsp_(integer *nm, integer *n, integer *nv, doublereal *a,
	 doublereal *w, integer *matz, doublereal *z__, doublereal *fv1, 
	doublereal *fv2, integer *ierr)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    extern /* Subroutine */ int tred3_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    integer i__, j;
    extern /* Subroutine */ int trbak3_(integer *, integer *, integer *, 
	    doublereal *, integer *, doublereal *), tqlrat_(integer *, 
	    doublereal *, doublereal *, integer *), tql2_(integer *, integer *
	    , doublereal *, doublereal *, doublereal *, integer *);



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED) */
/*     OF A REAL SYMMETRIC PACKED MATRIX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRIX  A. */

/*        NV  IS AN INTEGER VARIABLE SET EQUAL TO THE */
/*        DIMENSION OF THE ARRAY  A  AS SPECIFIED FOR */
/*        A  IN THE CALLING PROGRAM.  NV  MUST NOT BE */
/*        LESS THAN  N*(N+1)/2. */

/*        A  CONTAINS THE LOWER TRIANGLE OF THE REAL SYMMETRIC */
/*        PACKED MATRIX STORED ROW-WISE. */

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
    --a;

    /* Function Body */
    if (*n <= *nm) {
	goto L5;
    }
    *ierr = *n * 10;
    goto L50;
L5:
    if (*nv >= *n * (*n + 1) / 2) {
	goto L10;
    }
    *ierr = *n * 20;
    goto L50;

L10:
    tred3_(n, nv, &a[1], &w[1], &fv1[1], &fv2[1]);
    if (*matz != 0) {
	goto L20;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    tqlrat_(n, &w[1], &fv2[1], ierr);
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

    tql2_(nm, n, &w[1], &fv1[1], &z__[z_offset], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    trbak3_(nm, n, nv, &a[1], n, &z__[z_offset]);
L50:
    return 0;
} /* rsp_ */

