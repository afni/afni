/* cg.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int cg_(integer *nm, integer *n, doublereal *ar, doublereal *
	ai, doublereal *wr, doublereal *wi, integer *matz, doublereal *zr, 
	doublereal *zi, doublereal *fv1, doublereal *fv2, doublereal *fv3, 
	integer *ierr)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset;

    /* Local variables */
    extern /* Subroutine */ int cbal_(integer *, integer *, doublereal *, 
	    doublereal *, integer *, integer *, doublereal *), corth_(integer 
	    *, integer *, integer *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *), comqr_(integer *, integer *, integer 
	    *, integer *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *), cbabk2_(integer *, integer *, integer *,
	     integer *, doublereal *, integer *, doublereal *, doublereal *), 
	    comqr2_(integer *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *);
    integer is1, is2;



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED) */
/*     OF A COMPLEX GENERAL MATRIX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRIX  A=(AR,AI). */

/*        AR  AND  AI  CONTAIN THE REAL AND IMAGINARY PARTS, */
/*        RESPECTIVELY, OF THE COMPLEX GENERAL MATRIX. */

/*        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF */
/*        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO */
/*        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS. */

/*     ON OUTPUT */

/*        WR  AND  WI  CONTAIN THE REAL AND IMAGINARY PARTS, */
/*        RESPECTIVELY, OF THE EIGENVALUES. */

/*        ZR  AND  ZI  CONTAIN THE REAL AND IMAGINARY PARTS, */
/*        RESPECTIVELY, OF THE EIGENVECTORS IF MATZ IS NOT ZERO. */

/*        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR */
/*           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR COMQR */
/*           AND COMQR2.  THE NORMAL COMPLETION CODE IS ZERO. */

/*        FV1, FV2, AND  FV3  ARE TEMPORARY STORAGE ARRAYS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --fv3;
    --fv2;
    --fv1;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;
    --wi;
    --wr;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;

    /* Function Body */
    if (*n <= *nm) {
	goto L10;
    }
    *ierr = *n * 10;
    goto L50;

L10:
    cbal_(nm, n, &ar[ar_offset], &ai[ai_offset], &is1, &is2, &fv1[1]);
    corth_(nm, n, &is1, &is2, &ar[ar_offset], &ai[ai_offset], &fv2[1], &fv3[1]
	    );
    if (*matz != 0) {
	goto L20;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    comqr_(nm, n, &is1, &is2, &ar[ar_offset], &ai[ai_offset], &wr[1], &wi[1], 
	    ierr);
    goto L50;
/*     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS .......... */
L20:
    comqr2_(nm, n, &is1, &is2, &fv2[1], &fv3[1], &ar[ar_offset], &ai[
	    ai_offset], &wr[1], &wi[1], &zr[zr_offset], &zi[zi_offset], ierr);
    if (*ierr != 0) {
	goto L50;
    }
    cbabk2_(nm, n, &is1, &is2, &fv1[1], n, &zr[zr_offset], &zi[zi_offset]);
L50:
    return 0;
} /* cg_ */

