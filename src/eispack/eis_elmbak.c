/* elmbak.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int elmbak_(integer *nm, integer *low, integer *igh, 
	doublereal *a, integer *int__, integer *m, doublereal *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, z_dim1, z_offset, i__1, i__2, i__3;

    /* Local variables */
    integer i__, j;
    doublereal x;
    integer la, mm, mp, kp1, mp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ELMBAK, */
/*     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A REAL GENERAL */
/*     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING */
/*     UPPER HESSENBERG MATRIX DETERMINED BY  ELMHES. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED, */
/*          SET LOW=1 AND IGH EQUAL TO THE ORDER OF THE MATRIX. */

/*        A CONTAINS THE MULTIPLIERS WHICH WERE USED IN THE */
/*          REDUCTION BY  ELMHES  IN ITS LOWER TRIANGLE */
/*          BELOW THE SUBDIAGONAL. */

/*        INT CONTAINS INFORMATION ON THE ROWS AND COLUMNS */
/*          INTERCHANGED IN THE REDUCTION BY  ELMHES. */
/*          ONLY ELEMENTS LOW THROUGH IGH ARE USED. */

/*        M IS THE NUMBER OF COLUMNS OF Z TO BE BACK TRANSFORMED. */

/*        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGEN- */
/*          VECTORS TO BE BACK TRANSFORMED IN ITS FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE */
/*          TRANSFORMED EIGENVECTORS IN ITS FIRST M COLUMNS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --int__;
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
	mp1 = mp + 1;

	i__2 = *igh;
	for (i__ = mp1; i__ <= i__2; ++i__) {
	    x = a[i__ + (mp - 1) * a_dim1];
	    if (x == 0.) {
		goto L110;
	    }

	    i__3 = *m;
	    for (j = 1; j <= i__3; ++j) {
/* L100: */
		z__[i__ + j * z_dim1] += x * z__[mp + j * z_dim1];
	    }

L110:
	    ;
	}

	i__ = int__[mp];
	if (i__ == mp) {
	    goto L140;
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    x = z__[i__ + j * z_dim1];
	    z__[i__ + j * z_dim1] = z__[mp + j * z_dim1];
	    z__[mp + j * z_dim1] = x;
/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* elmbak_ */

