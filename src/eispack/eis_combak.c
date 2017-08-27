/* combak.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int combak_(integer *nm, integer *low, integer *igh, 
	doublereal *ar, doublereal *ai, integer *int__, integer *m, 
	doublereal *zr, doublereal *zi)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2, i__3;

    /* Local variables */
    integer i__, j, la, mm, mp;
    doublereal xi, xr;
    integer kp1, mp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE COMBAK, */
/*     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A COMPLEX GENERAL */
/*     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING */
/*     UPPER HESSENBERG MATRIX DETERMINED BY  COMHES. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED, */
/*          SET LOW=1 AND IGH EQUAL TO THE ORDER OF THE MATRIX. */

/*        AR AND AI CONTAIN THE MULTIPLIERS WHICH WERE USED IN THE */
/*          REDUCTION BY  COMHES  IN THEIR LOWER TRIANGLES */
/*          BELOW THE SUBDIAGONAL. */

/*        INT CONTAINS INFORMATION ON THE ROWS AND COLUMNS */
/*          INTERCHANGED IN THE REDUCTION BY  COMHES. */
/*          ONLY ELEMENTS LOW THROUGH IGH ARE USED. */

/*        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED. */

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVECTORS TO BE */
/*          BACK TRANSFORMED IN THEIR FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE TRANSFORMED EIGENVECTORS */
/*          IN THEIR FIRST M COLUMNS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --int__;
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;
    zi_dim1 = *nm;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    zr_dim1 = *nm;
    zr_offset = zr_dim1 + 1;
    zr -= zr_offset;

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
	    xr = ar[i__ + (mp - 1) * ar_dim1];
	    xi = ai[i__ + (mp - 1) * ai_dim1];
	    if (xr == 0. && xi == 0.) {
		goto L110;
	    }

	    i__3 = *m;
	    for (j = 1; j <= i__3; ++j) {
		zr[i__ + j * zr_dim1] = zr[i__ + j * zr_dim1] + xr * zr[mp + 
			j * zr_dim1] - xi * zi[mp + j * zi_dim1];
		zi[i__ + j * zi_dim1] = zi[i__ + j * zi_dim1] + xr * zi[mp + 
			j * zi_dim1] + xi * zr[mp + j * zr_dim1];
/* L100: */
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
	    xr = zr[i__ + j * zr_dim1];
	    zr[i__ + j * zr_dim1] = zr[mp + j * zr_dim1];
	    zr[mp + j * zr_dim1] = xr;
	    xi = zi[i__ + j * zi_dim1];
	    zi[i__ + j * zi_dim1] = zi[mp + j * zi_dim1];
	    zi[mp + j * zi_dim1] = xi;
/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* combak_ */

