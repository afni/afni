/* cortb.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int cortb_(integer *nm, integer *low, integer *igh, 
	doublereal *ar, doublereal *ai, doublereal *ortr, doublereal *orti, 
	integer *m, doublereal *zr, doublereal *zi)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, zr_dim1, zr_offset, 
	    zi_dim1, zi_offset, i__1, i__2, i__3;

    /* Local variables */
    doublereal h__;
    integer i__, j, la;
    doublereal gi, gr;
    integer mm, mp, kp1, mp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF */
/*     THE ALGOL PROCEDURE ORTBAK, NUM. MATH. 12, 349-368(1968) */
/*     BY MARTIN AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971). */

/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A COMPLEX GENERAL */
/*     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING */
/*     UPPER HESSENBERG MATRIX DETERMINED BY  CORTH. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED, */
/*          SET LOW=1 AND IGH EQUAL TO THE ORDER OF THE MATRIX. */

/*        AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY */
/*          TRANSFORMATIONS USED IN THE REDUCTION BY  CORTH */
/*          IN THEIR STRICT LOWER TRIANGLES. */

/*        ORTR AND ORTI CONTAIN FURTHER INFORMATION ABOUT THE */
/*          TRANSFORMATIONS USED IN THE REDUCTION BY  CORTH. */
/*          ONLY ELEMENTS LOW THROUGH IGH ARE USED. */

/*        M IS THE NUMBER OF COLUMNS OF ZR AND ZI TO BE BACK TRANSFORMED. 
*/

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE EIGENVECTORS TO BE */
/*          BACK TRANSFORMED IN THEIR FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE TRANSFORMED EIGENVECTORS */
/*          IN THEIR FIRST M COLUMNS. */

/*        ORTR AND ORTI HAVE BEEN ALTERED. */

/*     NOTE THAT CORTB PRESERVES VECTOR EUCLIDEAN NORMS. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --orti;
    --ortr;
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
	if (ar[mp + (mp - 1) * ar_dim1] == 0. && ai[mp + (mp - 1) * ai_dim1] 
		== 0.) {
	    goto L140;
	}
/*     .......... H BELOW IS NEGATIVE OF H FORMED IN CORTH .......... 
*/
	h__ = ar[mp + (mp - 1) * ar_dim1] * ortr[mp] + ai[mp + (mp - 1) * 
		ai_dim1] * orti[mp];
	mp1 = mp + 1;

	i__2 = *igh;
	for (i__ = mp1; i__ <= i__2; ++i__) {
	    ortr[i__] = ar[i__ + (mp - 1) * ar_dim1];
	    orti[i__] = ai[i__ + (mp - 1) * ai_dim1];
/* L100: */
	}

	i__2 = *m;
	for (j = 1; j <= i__2; ++j) {
	    gr = 0.;
	    gi = 0.;

	    i__3 = *igh;
	    for (i__ = mp; i__ <= i__3; ++i__) {
		gr = gr + ortr[i__] * zr[i__ + j * zr_dim1] + orti[i__] * zi[
			i__ + j * zi_dim1];
		gi = gi + ortr[i__] * zi[i__ + j * zi_dim1] - orti[i__] * zr[
			i__ + j * zr_dim1];
/* L110: */
	    }

	    gr /= h__;
	    gi /= h__;

	    i__3 = *igh;
	    for (i__ = mp; i__ <= i__3; ++i__) {
		zr[i__ + j * zr_dim1] = zr[i__ + j * zr_dim1] + gr * ortr[i__]
			 - gi * orti[i__];
		zi[i__ + j * zi_dim1] = zi[i__ + j * zi_dim1] + gr * orti[i__]
			 + gi * ortr[i__];
/* L120: */
	    }

/* L130: */
	}

L140:
	;
    }

L200:
    return 0;
} /* cortb_ */

