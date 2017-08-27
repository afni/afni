/* corth.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int corth_(integer *nm, integer *n, integer *low, integer *
	igh, doublereal *ar, doublereal *ai, doublereal *ortr, doublereal *
	orti)
{
    /* System generated locals */
    integer ar_dim1, ar_offset, ai_dim1, ai_offset, i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal f, g, h__;
    integer i__, j, m;
    doublereal scale;
    integer la;
    doublereal fi;
    integer ii, jj;
    doublereal fr;
    integer mp;
    extern doublereal pythag_(doublereal *, doublereal *);
    integer kp1;



/*     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF */
/*     THE ALGOL PROCEDURE ORTHES, NUM. MATH. 12, 349-368(1968) */
/*     BY MARTIN AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971). */

/*     GIVEN A COMPLEX GENERAL MATRIX, THIS SUBROUTINE */
/*     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS */
/*     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY */
/*     UNITARY SIMILARITY TRANSFORMATIONS. */

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
/*          RESPECTIVELY, OF THE HESSENBERG MATRIX.  INFORMATION */
/*          ABOUT THE UNITARY TRANSFORMATIONS USED IN THE REDUCTION */
/*          IS STORED IN THE REMAINING TRIANGLES UNDER THE */
/*          HESSENBERG MATRIX. */

/*        ORTR AND ORTI CONTAIN FURTHER INFORMATION ABOUT THE */
/*          TRANSFORMATIONS.  ONLY ELEMENTS LOW THROUGH IGH ARE USED. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

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
    --orti;
    --ortr;

    /* Function Body */
    la = *igh - 1;
    kp1 = *low + 1;
    if (la < kp1) {
	goto L200;
    }

    i__1 = la;
    for (m = kp1; m <= i__1; ++m) {
	h__ = 0.;
	ortr[m] = 0.;
	orti[m] = 0.;
	scale = 0.;
/*     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) .......... 
*/
	i__2 = *igh;
	for (i__ = m; i__ <= i__2; ++i__) {
/* L90: */
	    scale = scale + (d__1 = ar[i__ + (m - 1) * ar_dim1], abs(d__1)) + 
		    (d__2 = ai[i__ + (m - 1) * ai_dim1], abs(d__2));
	}

	if (scale == 0.) {
	    goto L180;
	}
	mp = m + *igh;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- .......... */
	i__2 = *igh;
	for (ii = m; ii <= i__2; ++ii) {
	    i__ = mp - ii;
	    ortr[i__] = ar[i__ + (m - 1) * ar_dim1] / scale;
	    orti[i__] = ai[i__ + (m - 1) * ai_dim1] / scale;
	    h__ = h__ + ortr[i__] * ortr[i__] + orti[i__] * orti[i__];
/* L100: */
	}

	g = sqrt(h__);
	f = pythag_(&ortr[m], &orti[m]);
	if (f == 0.) {
	    goto L103;
	}
	h__ += f * g;
	g /= f;
	ortr[m] = (g + 1.) * ortr[m];
	orti[m] = (g + 1.) * orti[m];
	goto L105;

L103:
	ortr[m] = g;
	ar[m + (m - 1) * ar_dim1] = scale;
/*     .......... FORM (I-(U*UT)/H) * A .......... */
L105:
	i__2 = *n;
	for (j = m; j <= i__2; ++j) {
	    fr = 0.;
	    fi = 0.;
/*     .......... FOR I=IGH STEP -1 UNTIL M DO -- .......... */
	    i__3 = *igh;
	    for (ii = m; ii <= i__3; ++ii) {
		i__ = mp - ii;
		fr = fr + ortr[i__] * ar[i__ + j * ar_dim1] + orti[i__] * ai[
			i__ + j * ai_dim1];
		fi = fi + ortr[i__] * ai[i__ + j * ai_dim1] - orti[i__] * ar[
			i__ + j * ar_dim1];
/* L110: */
	    }

	    fr /= h__;
	    fi /= h__;

	    i__3 = *igh;
	    for (i__ = m; i__ <= i__3; ++i__) {
		ar[i__ + j * ar_dim1] = ar[i__ + j * ar_dim1] - fr * ortr[i__]
			 + fi * orti[i__];
		ai[i__ + j * ai_dim1] = ai[i__ + j * ai_dim1] - fr * orti[i__]
			 - fi * ortr[i__];
/* L120: */
	    }

/* L130: */
	}
/*     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) .......... */
	i__2 = *igh;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    fr = 0.;
	    fi = 0.;
/*     .......... FOR J=IGH STEP -1 UNTIL M DO -- .......... */
	    i__3 = *igh;
	    for (jj = m; jj <= i__3; ++jj) {
		j = mp - jj;
		fr = fr + ortr[j] * ar[i__ + j * ar_dim1] - orti[j] * ai[i__ 
			+ j * ai_dim1];
		fi = fi + ortr[j] * ai[i__ + j * ai_dim1] + orti[j] * ar[i__ 
			+ j * ar_dim1];
/* L140: */
	    }

	    fr /= h__;
	    fi /= h__;

	    i__3 = *igh;
	    for (j = m; j <= i__3; ++j) {
		ar[i__ + j * ar_dim1] = ar[i__ + j * ar_dim1] - fr * ortr[j] 
			- fi * orti[j];
		ai[i__ + j * ai_dim1] = ai[i__ + j * ai_dim1] + fr * orti[j] 
			- fi * ortr[j];
/* L150: */
	    }

/* L160: */
	}

	ortr[m] = scale * ortr[m];
	orti[m] = scale * orti[m];
	ar[m + (m - 1) * ar_dim1] = -g * ar[m + (m - 1) * ar_dim1];
	ai[m + (m - 1) * ai_dim1] = -g * ai[m + (m - 1) * ai_dim1];
L180:
	;
    }

L200:
    return 0;
} /* corth_ */

