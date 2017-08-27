/* figi2.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int figi2_(integer *nm, integer *n, doublereal *t, 
	doublereal *d__, doublereal *e, doublereal *z__, integer *ierr)
{
    /* System generated locals */
    integer t_dim1, t_offset, z_dim1, z_offset, i__1, i__2;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal h__;
    integer i__, j;



/*     GIVEN A NONSYMMETRIC TRIDIAGONAL MATRIX SUCH THAT THE PRODUCTS */
/*     OF CORRESPONDING PAIRS OF OFF-DIAGONAL ELEMENTS ARE ALL */
/*     NON-NEGATIVE, AND ZERO ONLY WHEN BOTH FACTORS ARE ZERO, THIS */
/*     SUBROUTINE REDUCES IT TO A SYMMETRIC TRIDIAGONAL MATRIX */
/*     USING AND ACCUMULATING DIAGONAL SIMILARITY TRANSFORMATIONS. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        T CONTAINS THE INPUT MATRIX.  ITS SUBDIAGONAL IS */
/*          STORED IN THE LAST N-1 POSITIONS OF THE FIRST COLUMN, */
/*          ITS DIAGONAL IN THE N POSITIONS OF THE SECOND COLUMN, */
/*          AND ITS SUPERDIAGONAL IN THE FIRST N-1 POSITIONS OF */
/*          THE THIRD COLUMN.  T(1,1) AND T(N,3) ARE ARBITRARY. */

/*     ON OUTPUT */

/*        T IS UNALTERED. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE SYMMETRIC MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE SYMMETRIC */
/*          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS NOT SET. */

/*        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN */
/*          THE REDUCTION. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          N+I        IF T(I,1)*T(I-1,3) IS NEGATIVE, */
/*          2*N+I      IF T(I,1)*T(I-1,3) IS ZERO WITH */
/*                     ONE FACTOR NON-ZERO. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    t_dim1 = *nm;
    t_offset = t_dim1 + 1;
    t -= t_offset;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --e;
    --d__;

    /* Function Body */
    *ierr = 0;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
/* L50: */
	    z__[i__ + j * z_dim1] = 0.;
	}

	if (i__ == 1) {
	    goto L70;
	}
	h__ = t[i__ + t_dim1] * t[i__ - 1 + t_dim1 * 3];
	if (h__ < 0.) {
	    goto L900;
	} else if (h__ == 0) {
	    goto L60;
	} else {
	    goto L80;
	}
L60:
	if (t[i__ + t_dim1] != 0. || t[i__ - 1 + t_dim1 * 3] != 0.) {
	    goto L1000;
	}
	e[i__] = 0.;
L70:
	z__[i__ + i__ * z_dim1] = 1.;
	goto L90;
L80:
	e[i__] = sqrt(h__);
	z__[i__ + i__ * z_dim1] = z__[i__ - 1 + (i__ - 1) * z_dim1] * e[i__] /
		 t[i__ - 1 + t_dim1 * 3];
L90:
	d__[i__] = t[i__ + (t_dim1 << 1)];
/* L100: */
    }

    goto L1001;
/*     .......... SET ERROR -- PRODUCT OF SOME PAIR OF OFF-DIAGONAL */
/*                ELEMENTS IS NEGATIVE .......... */
L900:
    *ierr = *n + i__;
    goto L1001;
/*     .......... SET ERROR -- PRODUCT OF SOME PAIR OF OFF-DIAGONAL */
/*                ELEMENTS IS ZERO WITH ONE MEMBER NON-ZERO .......... */
L1000:
    *ierr = (*n << 1) + i__;
L1001:
    return 0;
} /* figi2_ */

