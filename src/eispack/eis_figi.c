/* figi.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int figi_(integer *nm, integer *n, doublereal *t, doublereal 
	*d__, doublereal *e, doublereal *e2, integer *ierr)
{
    /* System generated locals */
    integer t_dim1, t_offset, i__1;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    integer i__;



/*     GIVEN A NONSYMMETRIC TRIDIAGONAL MATRIX SUCH THAT THE PRODUCTS */
/*     OF CORRESPONDING PAIRS OF OFF-DIAGONAL ELEMENTS ARE ALL */
/*     NON-NEGATIVE, THIS SUBROUTINE REDUCES IT TO A SYMMETRIC */
/*     TRIDIAGONAL MATRIX WITH THE SAME EIGENVALUES.  IF, FURTHER, */
/*     A ZERO PRODUCT ONLY OCCURS WHEN BOTH FACTORS ARE ZERO, */
/*     THE REDUCED MATRIX IS SIMILAR TO THE ORIGINAL MATRIX. */

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

/*        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E. */
/*          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          N+I        IF T(I,1)*T(I-1,3) IS NEGATIVE, */
/*          -(3*N+I)   IF T(I,1)*T(I-1,3) IS ZERO WITH ONE FACTOR */
/*                     NON-ZERO.  IN THIS CASE, THE EIGENVECTORS OF */
/*                     THE SYMMETRIC MATRIX ARE NOT SIMPLY RELATED */
/*                     TO THOSE OF  T  AND SHOULD NOT BE SOUGHT. */

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
    --e2;
    --e;
    --d__;

    /* Function Body */
    *ierr = 0;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ == 1) {
	    goto L90;
	}
	e2[i__] = t[i__ + t_dim1] * t[i__ - 1 + t_dim1 * 3];
	if ((d__1 = e2[i__]) < 0.) {
	    goto L1000;
	} else if (d__1 == 0) {
	    goto L60;
	} else {
	    goto L80;
	}
L60:
	if (t[i__ + t_dim1] == 0. && t[i__ - 1 + t_dim1 * 3] == 0.) {
	    goto L80;
	}
/*     .......... SET ERROR -- PRODUCT OF SOME PAIR OF OFF-DIAGONAL */
/*                ELEMENTS IS ZERO WITH ONE MEMBER NON-ZERO ..........
 */
	*ierr = -(*n * 3 + i__);
L80:
	e[i__] = sqrt(e2[i__]);
L90:
	d__[i__] = t[i__ + (t_dim1 << 1)];
/* L100: */
    }

    goto L1001;
/*     .......... SET ERROR -- PRODUCT OF SOME PAIR OF OFF-DIAGONAL */
/*                ELEMENTS IS NEGATIVE .......... */
L1000:
    *ierr = *n + i__;
L1001:
    return 0;
} /* figi_ */

