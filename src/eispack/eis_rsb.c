/* rsb.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int rsb_(integer *nm, integer *n, integer *mb, doublereal *a,
	 doublereal *w, integer *matz, doublereal *z__, doublereal *fv1, 
	doublereal *fv2, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int bandr_(integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, logical *,
	     doublereal *);
    logical tf;
    extern /* Subroutine */ int tqlrat_(integer *, doublereal *, doublereal *,
	     integer *), tql2_(integer *, integer *, doublereal *, doublereal 
	    *, doublereal *, integer *);



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED) */
/*     OF A REAL SYMMETRIC BAND MATRIX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRIX  A. */

/*        MB  IS THE HALF BAND WIDTH OF THE MATRIX, DEFINED AS THE */
/*        NUMBER OF ADJACENT DIAGONALS, INCLUDING THE PRINCIPAL */
/*        DIAGONAL, REQUIRED TO SPECIFY THE NON-ZERO PORTION OF THE */
/*        LOWER TRIANGLE OF THE MATRIX. */

/*        A  CONTAINS THE LOWER TRIANGLE OF THE REAL SYMMETRIC */
/*        BAND MATRIX.  ITS LOWEST SUBDIAGONAL IS STORED IN THE */
/*        LAST  N+1-MB  POSITIONS OF THE FIRST COLUMN, ITS NEXT */
/*        SUBDIAGONAL IN THE LAST  N+2-MB  POSITIONS OF THE */
/*        SECOND COLUMN, FURTHER SUBDIAGONALS SIMILARLY, AND */
/*        FINALLY ITS PRINCIPAL DIAGONAL IN THE  N  POSITIONS */
/*        OF THE LAST COLUMN.  CONTENTS OF STORAGES NOT PART */
/*        OF THE MATRIX ARE ARBITRARY. */

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
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L5;
    }
    *ierr = *n * 10;
    goto L50;
L5:
    if (*mb > 0) {
	goto L10;
    }
    *ierr = *n * 12;
    goto L50;
L10:
    if (*mb <= *n) {
	goto L15;
    }
    *ierr = *n * 12;
    goto L50;

L15:
    if (*matz != 0) {
	goto L20;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    tf = FALSE_;
    bandr_(nm, n, mb, &a[a_offset], &w[1], &fv1[1], &fv2[1], &tf, &z__[
	    z_offset]);
    tqlrat_(n, &w[1], &fv2[1], ierr);
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L20:
    tf = TRUE_;
    bandr_(nm, n, mb, &a[a_offset], &w[1], &fv1[1], &fv1[1], &tf, &z__[
	    z_offset]);
    tql2_(nm, n, &w[1], &fv1[1], &z__[z_offset], ierr);
L50:
    return 0;
} /* rsb_ */

