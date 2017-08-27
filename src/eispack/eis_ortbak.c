/* ortbak.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int ortbak_(integer *nm, integer *low, integer *igh, 
	doublereal *a, doublereal *ort, integer *m, doublereal *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    doublereal g;
    integer i__, j, la, mm, mp, kp1, mp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ORTBAK, */
/*     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A REAL GENERAL */
/*     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING */
/*     UPPER HESSENBERG MATRIX DETERMINED BY  ORTHES. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED, */
/*          SET LOW=1 AND IGH EQUAL TO THE ORDER OF THE MATRIX. */

/*        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANS- */
/*          FORMATIONS USED IN THE REDUCTION BY  ORTHES */
/*          IN ITS STRICT LOWER TRIANGLE. */

/*        ORT CONTAINS FURTHER INFORMATION ABOUT THE TRANS- */
/*          FORMATIONS USED IN THE REDUCTION BY  ORTHES. */
/*          ONLY ELEMENTS LOW THROUGH IGH ARE USED. */

/*        M IS THE NUMBER OF COLUMNS OF Z TO BE BACK TRANSFORMED. */

/*        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGEN- */
/*          VECTORS TO BE BACK TRANSFORMED IN ITS FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE */
/*          TRANSFORMED EIGENVECTORS IN ITS FIRST M COLUMNS. */

/*        ORT HAS BEEN ALTERED. */

/*     NOTE THAT ORTBAK PRESERVES VECTOR EUCLIDEAN NORMS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --ort;
    a_dim1 = *nm;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;

    /* Function Body */
    if (*m == 0) {
	goto L200;
    }
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }
/*     .......... FOR MP=IGH-1 STEP -1 UNTIL LOW+1 DO -- .......... */
    i__1 = la;
    for (mm = kp1; mm <= i__1; ++mm) {
	mp = *low + *igh - mm;
	if (a[mp + (mp - 1) * a_dim1] == 0.) {
	    goto L140;
	}
	mp1 = mp + 1;

	i__2 = *igh;
	for (i__ = mp1; i__ <= i__2; ++i__) {
/* L100: */
	    ort[i__] = a[i__ + (mp - 1) * a_dim1];
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
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
} /* ortbak_ */

