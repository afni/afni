/* rg.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int rg_(integer *nm, integer *n, doublereal *a, doublereal *
	wr, doublereal *wi, integer *matz, doublereal *z__, integer *iv1, 
	doublereal *fv1, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int balbak_(integer *, integer *, integer *, 
	    integer *, doublereal *, integer *, doublereal *), balanc_(
	    integer *, integer *, doublereal *, integer *, integer *, 
	    doublereal *), elmhes_(integer *, integer *, integer *, integer *,
	     doublereal *, integer *), eltran_(integer *, integer *, integer *
	    , integer *, doublereal *, integer *, doublereal *);
    integer is1, is2;
    extern /* Subroutine */ int hqr_(integer *, integer *, integer *, integer 
	    *, doublereal *, doublereal *, doublereal *, integer *), hqr2_(
	    integer *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *);



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED) */
/*     OF A REAL GENERAL MATRIX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRIX  A. */

/*        A  CONTAINS THE REAL GENERAL MATRIX. */

/*        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF */
/*        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO */
/*        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS. */

/*     ON OUTPUT */

/*        WR  AND  WI  CONTAIN THE REAL AND IMAGINARY PARTS, */
/*        RESPECTIVELY, OF THE EIGENVALUES.  COMPLEX CONJUGATE */
/*        PAIRS OF EIGENVALUES APPEAR CONSECUTIVELY WITH THE */
/*        EIGENVALUE HAVING THE POSITIVE IMAGINARY PART FIRST. */

/*        Z  CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS */
/*        IF MATZ IS NOT ZERO.  IF THE J-TH EIGENVALUE IS REAL, THE */
/*        J-TH COLUMN OF  Z  CONTAINS ITS EIGENVECTOR.  IF THE J-TH */
/*        EIGENVALUE IS COMPLEX WITH POSITIVE IMAGINARY PART, THE */
/*        J-TH AND (J+1)-TH COLUMNS OF  Z  CONTAIN THE REAL AND */
/*        IMAGINARY PARTS OF ITS EIGENVECTOR.  THE CONJUGATE OF THIS */
/*        VECTOR IS THE EIGENVECTOR FOR THE CONJUGATE EIGENVALUE. */

/*        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR */
/*           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR HQR */
/*           AND HQR2.  THE NORMAL COMPLETION CODE IS ZERO. */

/*        IV1  AND  FV1  ARE TEMPORARY STORAGE ARRAYS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --fv1;
    --iv1;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --wi;
    --wr;
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
    balanc_(nm, n, &a[a_offset], &is1, &is2, &fv1[1]);
    elmhes_(nm, n, &is1, &is2, &a[a_offset], &iv1[1]);
    if (*matz != 0) {
	goto L20;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    hqr_(nm, n, &is1, &is2, &a[a_offset], &wr[1], &wi[1], ierr);
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L20:
    eltran_(nm, n, &is1, &is2, &a[a_offset], &iv1[1], &z__[z_offset]);
    hqr2_(nm, n, &is1, &is2, &a[a_offset], &wr[1], &wi[1], &z__[z_offset], 
	    ierr);
    if (*ierr != 0) {
	goto L50;
    }
    balbak_(nm, n, &is1, &is2, &fv1[1], n, &z__[z_offset]);
L50:
    return 0;
} /* rg_ */

