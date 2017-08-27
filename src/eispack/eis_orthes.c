/* orthes.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int orthes_(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *a, doublereal *ort)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    doublereal f, g, h__;
    integer i__, j, m;
    doublereal scale;
    integer la, ii, jj, mp, kp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ORTHES, */
/*     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971). */

/*     GIVEN A REAL GENERAL MATRIX, THIS SUBROUTINE */
/*     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS */
/*     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY */
/*     ORTHOGONAL SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        A CONTAINS THE INPUT MATRIX. */

/*     ON OUTPUT */

/*        A CONTAINS THE HESSENBERG MATRIX.  INFORMATION ABOUT */
/*          THE ORTHOGONAL TRANSFORMATIONS USED IN THE REDUCTION */
/*          IS STORED IN THE REMAINING TRIANGLE UNDER THE */
/*          HESSENBERG MATRIX. */

/*        ORT CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS. */
/*          ONLY ELEMENTS LOW THROUGH IGH ARE USED. */

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
    --ort;

    /* Function Body */
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }

    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
	h__ = 0.;
	ort[m] = 0.;
	scale = 0.;
/*     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) .......... 
*/
	i__2 = *igh;
	for (i__ = m; i__ <= i__2; ++i__) {
/* L90: */
	    scale += (d__1 = a[i__ + (m - 1) * a_dim1], abs(d__1));
	}

	if (scale == 0.) {
	    goto L180;
	}
	mp = m + *igh;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- .......... */
	i__2 = *igh;
	for (ii = m; ii <= i__2; ++ii) {
	    i__ = mp - ii;
	    ort[i__] = a[i__ + (m - 1) * a_dim1] / scale;
	    h__ += ort[i__] * ort[i__];
/* L100: */
	}

	d__1 = sqrt(h__);
	g = -d_sign(&d__1, &ort[m]);
	h__ -= ort[m] * g;
	ort[m] -= g;
/*     .......... FORM (I-(U*UT)/H) * A .......... */
	i__2 = *n;
	for (j = m; j <= i__2; ++j) {
	    f = 0.;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- .......... */
	    i__3 = *igh;
	    for (ii = m; ii <= i__3; ++ii) {
		i__ = mp - ii;
		f += ort[i__] * a[i__ + j * a_dim1];
/* L110: */
	    }

	    f /= h__;

	    i__3 = *igh;
	    for (i__ = m; i__ <= i__3; ++i__) {
/* L120: */
		a[i__ + j * a_dim1] -= f * ort[i__];
	    }

/* L130: */
	}
/*     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) .......... */
	i__2 = *igh;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    f = 0.;
/*     .......... FOR J=IGH STEP -1 UNTIL M DO -- .......... */
	    i__3 = *igh;
	    for (jj = m; jj <= i__3; ++jj) {
		j = mp - jj;
		f += ort[j] * a[i__ + j * a_dim1];
/* L140: */
	    }

	    f /= h__;

	    i__3 = *igh;
	    for (j = m; j <= i__3; ++j) {
/* L150: */
		a[i__ + j * a_dim1] -= f * ort[j];
	    }

/* L160: */
	}

	ort[m] = scale * ort[m];
	a[m + (m - 1) * a_dim1] = scale * g;
L180:
	;
    }

L200:
    return 0;
} /* orthes_ */

