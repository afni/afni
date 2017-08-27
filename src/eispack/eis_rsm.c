/* rsm.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int rsm_(integer *nm, integer *n, doublereal *a, doublereal *
	w, integer *m, doublereal *z__, doublereal *fwork, integer *iwork, 
	integer *ierr)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset;

    /* Local variables */
    extern /* Subroutine */ int tred1_(integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    integer k1, k2, k3, k4, k5, k6, k7, k8;
    extern /* Subroutine */ int trbak1_(integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *), tqlrat_(integer *, 
	    doublereal *, doublereal *, integer *), imtqlv_(integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     integer *, doublereal *), tinvit_(integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, doublereal *,
	     integer *, doublereal *, integer *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);



/*     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF */
/*     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK) */
/*     TO FIND ALL OF THE EIGENVALUES AND SOME OF THE EIGENVECTORS */
/*     OF A REAL SYMMETRIC MATRIX. */

/*     ON INPUT */

/*        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL */
/*        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*        DIMENSION STATEMENT. */

/*        N  IS THE ORDER OF THE MATRIX  A. */

/*        A  CONTAINS THE REAL SYMMETRIC MATRIX. */

/*        M  THE EIGENVECTORS CORRESPONDING TO THE FIRST M EIGENVALUES */
/*           ARE TO BE COMPUTED. */
/*           IF M = 0 THEN NO EIGENVECTORS ARE COMPUTED. */
/*           IF M = N THEN ALL OF THE EIGENVECTORS ARE COMPUTED. */

/*     ON OUTPUT */

/*        W  CONTAINS ALL N EIGENVALUES IN ASCENDING ORDER. */

/*        Z  CONTAINS THE ORTHONORMAL EIGENVECTORS ASSOCIATED WITH */
/*           THE FIRST M EIGENVALUES. */

/*        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR */
/*           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR TQLRAT, */
/*           IMTQLV AND TINVIT.  THE NORMAL COMPLETION CODE IS ZERO. */

/*        FWORK  IS A TEMPORARY STORAGE ARRAY OF DIMENSION 8*N. */

/*        IWORK  IS AN INTEGER TEMPORARY STORAGE ARRAY OF DIMENSION N. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --iwork;
    --w;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --fwork;

    /* Function Body */
    *ierr = *n * 10;
    if (*n > *nm || *m > *nm) {
	goto L50;
    }
    k1 = 1;
    k2 = k1 + *n;
    k3 = k2 + *n;
    k4 = k3 + *n;
    k5 = k4 + *n;
    k6 = k5 + *n;
    k7 = k6 + *n;
    k8 = k7 + *n;
    if (*m > 0) {
	goto L10;
    }
/*     .......... FIND EIGENVALUES ONLY .......... */
    tred1_(nm, n, &a[a_offset], &w[1], &fwork[k1], &fwork[k2]);
    tqlrat_(n, &w[1], &fwork[k2], ierr);
    goto L50;
/*     .......... FIND ALL EIGENVALUES AND M EIGENVECTORS .......... */
L10:
    tred1_(nm, n, &a[a_offset], &fwork[k1], &fwork[k2], &fwork[k3]);
    imtqlv_(n, &fwork[k1], &fwork[k2], &fwork[k3], &w[1], &iwork[1], ierr, &
	    fwork[k4]);
    tinvit_(nm, n, &fwork[k1], &fwork[k2], &fwork[k3], m, &w[1], &iwork[1], &
	    z__[z_offset], ierr, &fwork[k4], &fwork[k5], &fwork[k6], &fwork[
	    k7], &fwork[k8]);
    trbak1_(nm, n, &a[a_offset], &fwork[k2], m, &z__[z_offset]);
L50:
    return 0;
} /* rsm_ */

