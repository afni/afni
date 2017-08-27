/* ortran.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int ortran_(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *a, doublereal *ort, doublereal *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    doublereal g;
    integer i__, j, kl, mm, mp, mp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ORTRANS, */
/*     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971). */

/*     THIS SUBROUTINE ACCUMULATES THE ORTHOGONAL SIMILARITY */
/*     TRANSFORMATIONS USED IN THE REDUCTION OF A REAL GENERAL */
/*     MATRIX TO UPPER HESSENBERG FORM BY  ORTHES. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANS- */
/*          FORMATIONS USED IN THE REDUCTION BY  ORTHES */
/*          IN ITS STRICT LOWER TRIANGLE. */

/*        ORT CONTAINS FURTHER INFORMATION ABOUT THE TRANS- */
/*          FORMATIONS USED IN THE REDUCTION BY  ORTHES. */
/*          ONLY ELEMENTS LOW THROUGH IGH ARE USED. */

/*     ON OUTPUT */

/*        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE */
/*          REDUCTION BY  ORTHES. */

/*        ORT HAS BEEN ALTERED. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

/*     .......... INITIALIZE Z TO IDENTITY MATRIX .......... */
    /* Parameter adjustments */
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --ort;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L60: */
	    z__[i__ + j * z_dim1] = 0.;
	}

	z__[j + j * z_dim1] = 1.;
/* L80: */
    }

    kl = *igh - *low - 1;
    if (kl < 1) {
	goto L200;
    }
/*     .......... FOR MP=IGH-1 STEP -1 UNTIL LOW+1 DO -- .......... */
    i__1 = kl;
    for (mm = 1; mm <= i__1; ++mm) {
	mp = *igh - mm;
	if (a[mp + (mp - 1) * a_dim1] == 0.) {
	    goto L140;
	}
	mp1 = mp + 1;

	i__2 = *igh;
	for (i__ = mp1; i__ <= i__2; ++i__) {
/* L100: */
	    ort[i__] = a[i__ + (mp - 1) * a_dim1];
	}

	i__2 = *igh;
	for (j = mp; j <= i__2; ++j) {
	    g = 0.;

	    i__3 = *igh;
	    for (i__ = mp; i__ <= i__3; ++i__) {
/* L110: */
		g += ort[i__] * z__[i__ + j * z_dim1];
	    }
/*     .......... DIVISOR BELOW IS NEGATIVE OF H FORMED IN ORTHES.
 */
/*                DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ......
.... */
	    g = g / ort[mp] / a[mp + (mp - 1) * a_dim1];

	    i__3 = *igh;
	    for (i__ = mp; i__ <= i__3; ++i__) {
/* L120: */
		z__[i__ + j * z_dim1] += g * ort[i__];
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* ortran_ */

