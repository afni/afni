/* comhes.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int comhes_(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *ar, doublereal *ai, integer *int__)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Local variables */
    extern /* Subroutine */ int cdiv_(doublereal *, doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *);
    integer i__, j, m, la;
    doublereal xi, yi, xr, yr;
    integer mm1, kp1, mp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE COMHES, */
/*     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971). */

/*     GIVEN A COMPLEX GENERAL MATRIX, THIS SUBROUTINE */
/*     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS */
/*     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY */
/*     STABILIZED ELEMENTARY SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING */
/*          SUBROUTINE  CBAL.  IF  CBAL  HAS NOT BEEN USED, */
/*          SET LOW=1, IGH=N. */

/*        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE COMPLEX INPUT MATRIX. */

/*     ON OUTPUT */

/*        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS, */
/*          RESPECTIVELY, OF THE HESSENBERG MATRIX.  THE */
/*          MULTIPLIERS WHICH WERE USED IN THE REDUCTION */
/*          ARE STORED IN THE REMAINING TRIANGLES UNDER THE */
/*          HESSENBERG MATRIX. */

/*        INT CONTAINS INFORMATION ON THE ROWS AND COLUMNS */
/*          INTERCHANGED IN THE REDUCTION. */
/*          ONLY ELEMENTS LOW THROUGH IGH ARE USED. */

/*     CALLS CDIV FOR COMPLEX DIVISION. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    ai_dim1 = *nm;
    ai_offset = ai_dim1 + 1;
    ai -= ai_offset;
    ar_dim1 = *nm;
    ar_offset = ar_dim1 + 1;
    ar -= ar_offset;
    --int__;

    /* Function Body */
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }

    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
	mm1 = m - 1;
	xr = 0.;
	xi = 0.;
	i__ = m;

	i__2 = *igh;
	for (j = m; j <= i__2; ++j) {
	    if ((d__1 = ar[j + mm1 * ar_dim1], abs(d__1)) + (d__2 = ai[j + 
		    mm1 * ai_dim1], abs(d__2)) <= abs(xr) + abs(xi)) {
		goto L100;
	    }
	    xr = ar[j + mm1 * ar_dim1];
	    xi = ai[j + mm1 * ai_dim1];
	    i__ = j;
L100:
	    ;
	}

	int__[m] = i__;
	if (i__ == m) {
	    goto L130;
	}
/*     .......... INTERCHANGE ROWS AND COLUMNS OF AR AND AI ..........
 */
	i__2 = *n;
	for (j = mm1; j <= i__2; ++j) {
	    yr = ar[i__ + j * ar_dim1];
	    ar[i__ + j * ar_dim1] = ar[m + j * ar_dim1];
	    ar[m + j * ar_dim1] = yr;
	    yi = ai[i__ + j * ai_dim1];
	    ai[i__ + j * ai_dim1] = ai[m + j * ai_dim1];
	    ai[m + j * ai_dim1] = yi;
/* L110: */
	}

	i__2 = *igh;
	for (j = 1; j <= i__2; ++j) {
	    yr = ar[j + i__ * ar_dim1];
	    ar[j + i__ * ar_dim1] = ar[j + m * ar_dim1];
	    ar[j + m * ar_dim1] = yr;
	    yi = ai[j + i__ * ai_dim1];
	    ai[j + i__ * ai_dim1] = ai[j + m * ai_dim1];
	    ai[j + m * ai_dim1] = yi;
/* L120: */
	}
/*     .......... END INTERCHANGE .......... */
L130:
	if (xr == 0. && xi == 0.) {
	    goto L180;
	}
	mp1 = m + 1;

	i__2 = *igh;
	for (i__ = mp1; i__ <= i__2; ++i__) {
	    yr = ar[i__ + mm1 * ar_dim1];
	    yi = ai[i__ + mm1 * ai_dim1];
	    if (yr == 0. && yi == 0.) {
		goto L160;
	    }
	    cdiv_(&yr, &yi, &xr, &xi, &yr, &yi);
	    ar[i__ + mm1 * ar_dim1] = yr;
	    ai[i__ + mm1 * ai_dim1] = yi;

	    i__3 = *n;
	    for (j = m; j <= i__3; ++j) {
		ar[i__ + j * ar_dim1] = ar[i__ + j * ar_dim1] - yr * ar[m + j 
			* ar_dim1] + yi * ai[m + j * ai_dim1];
		ai[i__ + j * ai_dim1] = ai[i__ + j * ai_dim1] - yr * ai[m + j 
			* ai_dim1] - yi * ar[m + j * ar_dim1];
/* L140: */
	    }

	    i__3 = *igh;
	    for (j = 1; j <= i__3; ++j) {
		ar[j + m * ar_dim1] = ar[j + m * ar_dim1] + yr * ar[j + i__ * 
			ar_dim1] - yi * ai[j + i__ * ai_dim1];
		ai[j + m * ai_dim1] = ai[j + m * ai_dim1] + yr * ai[j + i__ * 
			ai_dim1] + yi * ar[j + i__ * ar_dim1];
/* L150: */
	    }

L160:
	    ;
	}

L180:
	;
    }

L200:
    return 0;
} /* comhes_ */

