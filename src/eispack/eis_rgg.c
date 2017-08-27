/* rgg.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static doublereal c_b5 = 0.;

/* Subroutine */ int rgg_(integer *nm, integer *n, doublereal *a, doublereal *
	b, doublereal *alfr, doublereal *alfi, doublereal *beta, integer *
	matz, doublereal *z__, integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int qzit_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, logical *, doublereal *, integer *), 
	    qzvec_(integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), qzhes_(
	    integer *, integer *, doublereal *, doublereal *, logical *, 
	    doublereal *), qzval_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, logical *,
	     doublereal *);
    logical tf;



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED) */
/*     FOR THE REAL GENERAL GENERALIZED EIGENPROBLEM  AX = (LAMBDA)BX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRICES  A  AND  B. */

/*        A  CONTAINS A REAL GENERAL MATRIX. */

/*        B  CONTAINS A REAL GENERAL MATRIX. */

/*        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF */
/*        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO */
/*        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS. */

/*     ON OUTPUT */

/*        ALFR  AND  ALFI  CONTAIN THE REAL AND IMAGINARY PARTS, */
/*        RESPECTIVELY, OF THE NUMERATORS OF THE EIGENVALUES. */

/*        BETA  CONTAINS THE DENOMINATORS OF THE EIGENVALUES, */
/*        WHICH ARE THUS GIVEN BY THE RATIOS  (ALFR+I*ALFI)/BETA. */
/*        COMPLEX CONJUGATE PAIRS OF EIGENVALUES APPEAR CONSECUTIVELY */
/*        WITH THE EIGENVALUE HAVING THE POSITIVE IMAGINARY PART FIRST. */

/*        Z  CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS */
/*        IF MATZ IS NOT ZERO.  IF THE J-TH EIGENVALUE IS REAL, THE */
/*        J-TH COLUMN OF  Z  CONTAINS ITS EIGENVECTOR.  IF THE J-TH */
/*        EIGENVALUE IS COMPLEX WITH POSITIVE IMAGINARY PART, THE */
/*        J-TH AND (J+1)-TH COLUMNS OF  Z  CONTAIN THE REAL AND */
/*        IMAGINARY PARTS OF ITS EIGENVECTOR.  THE CONJUGATE OF THIS */
/*        VECTOR IS THE EIGENVECTOR FOR THE CONJUGATE EIGENVALUE. */

/*        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR */
/*           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR QZIT. */
/*           THE NORMAL COMPLETION CODE IS ZERO. */

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
    --beta;
    --alfi;
    --alfr;
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
    if (*matz != 0) {
	goto L20;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    tf = FALSE_;
    qzhes_(nm, n, &a[a_offset], &b[b_offset], &tf, &z__[z_offset]);
    qzit_(nm, n, &a[a_offset], &b[b_offset], &c_b5, &tf, &z__[z_offset], ierr)
	    ;
    qzval_(nm, n, &a[a_offset], &b[b_offset], &alfr[1], &alfi[1], &beta[1], &
	    tf, &z__[z_offset]);
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L20:
    tf = TRUE_;
    qzhes_(nm, n, &a[a_offset], &b[b_offset], &tf, &z__[z_offset]);
    qzit_(nm, n, &a[a_offset], &b[b_offset], &c_b5, &tf, &z__[z_offset], ierr)
	    ;
    qzval_(nm, n, &a[a_offset], &b[b_offset], &alfr[1], &alfi[1], &beta[1], &
	    tf, &z__[z_offset]);
    if (*ierr != 0) {
	goto L50;
    }
    qzvec_(nm, n, &a[a_offset], &b[b_offset], &alfr[1], &alfi[1], &beta[1], &
	    z__[z_offset]);
L50:
    return 0;
} /* rgg_ */

