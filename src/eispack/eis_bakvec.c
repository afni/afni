/* bakvec.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int bakvec_(integer *nm, integer *n, doublereal *t, 
	doublereal *e, integer *m, doublereal *z__, integer *ierr)
{
    /* System generated locals */
    integer t_dim1, t_offset, z_dim1, z_offset, i__1, i__2;

    /* Local variables */
    integer i__, j;



/*     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A NONSYMMETRIC */
/*     TRIDIAGONAL MATRIX BY BACK TRANSFORMING THOSE OF THE */
/*     CORRESPONDING SYMMETRIC MATRIX DETERMINED BY  FIGI. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        T CONTAINS THE NONSYMMETRIC MATRIX.  ITS SUBDIAGONAL IS */
/*          STORED IN THE LAST N-1 POSITIONS OF THE FIRST COLUMN, */
/*          ITS DIAGONAL IN THE N POSITIONS OF THE SECOND COLUMN, */
/*          AND ITS SUPERDIAGONAL IN THE FIRST N-1 POSITIONS OF */
/*          THE THIRD COLUMN.  T(1,1) AND T(N,3) ARE ARBITRARY. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE SYMMETRIC */
/*          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY. */

/*        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED. */

/*        Z CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED */
/*          IN ITS FIRST M COLUMNS. */

/*     ON OUTPUT */

/*        T IS UNALTERED. */

/*        E IS DESTROYED. */

/*        Z CONTAINS THE TRANSFORMED EIGENVECTORS */
/*          IN ITS FIRST M COLUMNS. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          2*N+I      IF E(I) IS ZERO WITH T(I,1) OR T(I-1,3) NON-ZERO. 
*/
/*                     IN THIS CASE, THE SYMMETRIC MATRIX IS NOT SIMILAR 
*/
/*                     TO THE ORIGINAL MATRIX, AND THE EIGENVECTORS */
/*                     CANNOT BE FOUND BY THIS PROGRAM. */

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
    --e;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;

    /* Function Body */
    *ierr = 0;
    if (*m == 0) {
	goto L1001;
    }
    e[1] = 1.;
    if (*n == 1) {
	goto L1001;
    }

    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	if (e[i__] != 0.) {
	    goto L80;
	}
	if (t[i__ + t_dim1] != 0. || t[i__ - 1 + t_dim1 * 3] != 0.) {
	    goto L1000;
	}
	e[i__] = 1.;
	goto L100;
L80:
	e[i__] = e[i__ - 1] * e[i__] / t[i__ - 1 + t_dim1 * 3];
L100:
	;
    }

    i__1 = *m;
    for (j = 1; j <= i__1; ++j) {

	i__2 = *n;
	for (i__ = 2; i__ <= i__2; ++i__) {
	    z__[i__ + j * z_dim1] *= e[i__];
/* L120: */
	}
    }

    goto L1001;
/*     .......... SET ERROR -- EIGENVECTORS CANNOT BE */
/*                FOUND BY THIS PROGRAM .......... */
L1000:
    *ierr = (*n << 1) + i__;
L1001:
    return 0;
} /* bakvec_ */

