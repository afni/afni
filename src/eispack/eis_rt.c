/* rt.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int rt_(integer *nm, integer *n, doublereal *a, doublereal *
	w, integer *matz, doublereal *z__, doublereal *fv1, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int figi_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *), figi2_(
	    integer *, integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *), imtql1_(integer *, doublereal *, 
	    doublereal *, integer *), imtql2_(integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *);



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED) */
/*     OF A SPECIAL REAL TRIDIAGONAL MATRIX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRIX  A. */

/*        A  CONTAINS THE SPECIAL REAL TRIDIAGONAL MATRIX IN ITS */
/*        FIRST THREE COLUMNS.  THE SUBDIAGONAL ELEMENTS ARE STORED */
/*        IN THE LAST  N-1  POSITIONS OF THE FIRST COLUMN, THE */
/*        DIAGONAL ELEMENTS IN THE SECOND COLUMN, AND THE SUPERDIAGONAL */
/*        ELEMENTS IN THE FIRST  N-1  POSITIONS OF THE THIRD COLUMN. */
/*        ELEMENTS  A(1,1)  AND  A(N,3)  ARE ARBITRARY. */

/*        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF */
/*        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO */
/*        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS. */

/*     ON OUTPUT */

/*        W  CONTAINS THE EIGENVALUES IN ASCENDING ORDER. */

/*        Z  CONTAINS THE EIGENVECTORS IF MATZ IS NOT ZERO. */

/*        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR */
/*           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR IMTQL1 */
/*           AND IMTQL2.  THE NORMAL COMPLETION CODE IS ZERO. */

/*        FV1  IS A TEMPORARY STORAGE ARRAY. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --fv1;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
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
    figi_(nm, n, &a[a_offset], &w[1], &fv1[1], &fv1[1], ierr);
    if (*ierr > 0) {
	goto L50;
    }
    imtql1_(n, &w[1], &fv1[1], ierr);
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L20:
    figi2_(nm, n, &a[a_offset], &w[1], &fv1[1], &z__[z_offset], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    imtql2_(nm, n, &w[1], &fv1[1], &z__[z_offset], ierr);
L50:
    return 0;
} /* rt_ */

