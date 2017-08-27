/* tinvit.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Subroutine */ int tinvit_(integer *nm, integer *n, doublereal *d__, 
	doublereal *e, doublereal *e2, integer *m, doublereal *w, integer *
	ind, doublereal *z__, integer *ierr, doublereal *rv1, doublereal *rv2,
	 doublereal *rv3, doublereal *rv4, doublereal *rv6)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal norm;
    integer i__, j, p, q, r__, s;
    doublereal u, v, order;
    integer group=0;
    doublereal x0=0.0, x1;
    integer ii, jj, ip=0;
    doublereal uk=0.0, xu=0.0;
    extern doublereal pythag_(doublereal *, doublereal *), epslon_(doublereal 
	    *);
    integer tag, its;
    doublereal eps2=0.0, eps3=0.0, eps4=0.0;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE INVERSE ITERATION TECH- */
/*     NIQUE IN THE ALGOL PROCEDURE TRISTURM BY PETERS AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971). */

/*     THIS SUBROUTINE FINDS THOSE EIGENVECTORS OF A TRIDIAGONAL */
/*     SYMMETRIC MATRIX CORRESPONDING TO SPECIFIED EIGENVALUES, */
/*     USING INVERSE ITERATION. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX */
/*          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY. */

/*        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E, */
/*          WITH ZEROS CORRESPONDING TO NEGLIGIBLE ELEMENTS OF E. */
/*          E(I) IS CONSIDERED NEGLIGIBLE IF IT IS NOT LARGER THAN */
/*          THE PRODUCT OF THE RELATIVE MACHINE PRECISION AND THE SUM */
/*          OF THE MAGNITUDES OF D(I) AND D(I-1).  E2(1) MUST CONTAIN */
/*          0.0D0 IF THE EIGENVALUES ARE IN ASCENDING ORDER, OR 2.0D0 */
/*          IF THE EIGENVALUES ARE IN DESCENDING ORDER.  IF  BISECT, */
/*          TRIDIB, OR  IMTQLV  HAS BEEN USED TO FIND THE EIGENVALUES, */
/*          THEIR OUTPUT E2 ARRAY IS EXACTLY WHAT IS EXPECTED HERE. */

/*        M IS THE NUMBER OF SPECIFIED EIGENVALUES. */

/*        W CONTAINS THE M EIGENVALUES IN ASCENDING OR DESCENDING ORDER. 
*/

/*        IND CONTAINS IN ITS FIRST M POSITIONS THE SUBMATRIX INDICES */
/*          ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W -- */
/*          1 FOR EIGENVALUES BELONGING TO THE FIRST SUBMATRIX FROM */
/*          THE TOP, 2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC. 
*/

/*     ON OUTPUT */

/*        ALL INPUT ARRAYS ARE UNALTERED. */

/*        Z CONTAINS THE ASSOCIATED SET OF ORTHONORMAL EIGENVECTORS. */
/*          ANY VECTOR WHICH FAILS TO CONVERGE IS SET TO ZERO. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          -R         IF THE EIGENVECTOR CORRESPONDING TO THE R-TH */
/*                     EIGENVALUE FAILS TO CONVERGE IN 5 ITERATIONS. */

/*        RV1, RV2, RV3, RV4, AND RV6 ARE TEMPORARY STORAGE ARRAYS. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --rv6;
    --rv4;
    --rv3;
    --rv2;
    --rv1;
    --e2;
    --e;
    --d__;
    z_dim1 = *nm;
    z_offset = z_dim1 + 1;
    z__ -= z_offset;
    --ind;
    --w;

    /* Function Body */
    *ierr = 0;
    if (*m == 0) {
	goto L1001;
    }
    tag = 0;
    order = 1. - e2[1];
    q = 0;
/*     .......... ESTABLISH AND PROCESS NEXT SUBMATRIX .......... */
L100:
    p = q + 1;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	if (q == *n) {
	    goto L140;
	}
	if (e2[q + 1] == 0.) {
	    goto L140;
	}
/* L120: */
    }
/*     .......... FIND VECTORS BY INVERSE ITERATION .......... */
L140:
    ++tag;
    s = 0;

    i__1 = *m;
    for (r__ = 1; r__ <= i__1; ++r__) {
	if (ind[r__] != tag) {
	    goto L920;
	}
	its = 1;
	x1 = w[r__];
	if (s != 0) {
	    goto L510;
	}
/*     .......... CHECK FOR ISOLATED ROOT .......... */
	xu = 1.;
	if (p != q) {
	    goto L490;
	}
	rv6[p] = 1.;
	goto L870;
L490:
	norm = (d__1 = d__[p], abs(d__1));
	ip = p + 1;

	i__2 = q;
	for (i__ = ip; i__ <= i__2; ++i__) {
/* L500: */
/* Computing MAX */
	    d__3 = norm, d__4 = (d__1 = d__[i__], abs(d__1)) + (d__2 = e[i__],
		     abs(d__2));
	    norm = max(d__3,d__4);
	}
/*     .......... EPS2 IS THE CRITERION FOR GROUPING, */
/*                EPS3 REPLACES ZERO PIVOTS AND EQUAL */
/*                ROOTS ARE MODIFIED BY EPS3, */
/*                EPS4 IS TAKEN VERY SMALL TO AVOID OVERFLOW .........
. */
	eps2 = norm * .001;
	eps3 = epslon_(&norm);
	uk = (doublereal) (q - p + 1);
	eps4 = uk * eps3;
	uk = eps4 / sqrt(uk);
	s = p;
L505:
	group = 0;
	goto L520;
/*     .......... LOOK FOR CLOSE OR COINCIDENT ROOTS .......... */
L510:
	if ((d__1 = x1 - x0, abs(d__1)) >= eps2) {
	    goto L505;
	}
	++group;
	if (order * (x1 - x0) <= 0.) {
	    x1 = x0 + order * eps3;
	}
/*     .......... ELIMINATION WITH INTERCHANGES AND */
/*                INITIALIZATION OF VECTOR .......... */
L520:
	v = 0.;

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
	    rv6[i__] = uk;
	    if (i__ == p) {
		goto L560;
	    }
	    if ((d__1 = e[i__], abs(d__1)) < abs(u)) {
		goto L540;
	    }
/*     .......... WARNING -- A DIVIDE CHECK MAY OCCUR HERE IF */
/*                E2 ARRAY HAS NOT BEEN SPECIFIED CORRECTLY ......
.... */
	    xu = u / e[i__];
	    rv4[i__] = xu;
	    rv1[i__ - 1] = e[i__];
	    rv2[i__ - 1] = d__[i__] - x1;
	    rv3[i__ - 1] = 0.;
	    if (i__ != q) {
		rv3[i__ - 1] = e[i__ + 1];
	    }
	    u = v - xu * rv2[i__ - 1];
	    v = -xu * rv3[i__ - 1];
	    goto L580;
L540:
	    xu = e[i__] / u;
	    rv4[i__] = xu;
	    rv1[i__ - 1] = u;
	    rv2[i__ - 1] = v;
	    rv3[i__ - 1] = 0.;
L560:
	    u = d__[i__] - x1 - xu * v;
	    if (i__ != q) {
		v = e[i__ + 1];
	    }
L580:
	    ;
	}

	if (u == 0.) {
	    u = eps3;
	}
	rv1[q] = u;
	rv2[q] = 0.;
	rv3[q] = 0.;
/*     .......... BACK SUBSTITUTION */
/*                FOR I=Q STEP -1 UNTIL P DO -- .......... */
L600:
	i__2 = q;
	for (ii = p; ii <= i__2; ++ii) {
	    i__ = p + q - ii;
	    rv6[i__] = (rv6[i__] - u * rv2[i__] - v * rv3[i__]) / rv1[i__];
	    v = u;
	    u = rv6[i__];
/* L620: */
	}
/*     .......... ORTHOGONALIZE WITH RESPECT TO PREVIOUS */
/*                MEMBERS OF GROUP .......... */
	if (group == 0) {
	    goto L700;
	}
	j = r__;

	i__2 = group;
	for (jj = 1; jj <= i__2; ++jj) {
L630:
	    --j;
	    if (ind[j] != tag) {
		goto L630;
	    }
	    xu = 0.;

	    i__3 = q;
	    for (i__ = p; i__ <= i__3; ++i__) {
/* L640: */
		xu += rv6[i__] * z__[i__ + j * z_dim1];
	    }

	    i__3 = q;
	    for (i__ = p; i__ <= i__3; ++i__) {
/* L660: */
		rv6[i__] -= xu * z__[i__ + j * z_dim1];
	    }

/* L680: */
	}

L700:
	norm = 0.;

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
/* L720: */
	    norm += (d__1 = rv6[i__], abs(d__1));
	}

	if (norm >= 1.) {
	    goto L840;
	}
/*     .......... FORWARD SUBSTITUTION .......... */
	if (its == 5) {
	    goto L830;
	}
	if (norm != 0.) {
	    goto L740;
	}
	rv6[s] = eps4;
	++s;
	if (s > q) {
	    s = p;
	}
	goto L780;
L740:
	xu = eps4 / norm;

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
/* L760: */
	    rv6[i__] *= xu;
	}
/*     .......... ELIMINATION OPERATIONS ON NEXT VECTOR */
/*                ITERATE .......... */
L780:
	i__2 = q;
	for (i__ = ip; i__ <= i__2; ++i__) {
	    u = rv6[i__];
/*     .......... IF RV1(I-1) .EQ. E(I), A ROW INTERCHANGE */
/*                WAS PERFORMED EARLIER IN THE */
/*                TRIANGULARIZATION PROCESS .......... */
	    if (rv1[i__ - 1] != e[i__]) {
		goto L800;
	    }
	    u = rv6[i__ - 1];
	    rv6[i__ - 1] = rv6[i__];
L800:
	    rv6[i__] = u - rv4[i__] * rv6[i__ - 1];
/* L820: */
	}

	++its;
	goto L600;
/*     .......... SET ERROR -- NON-CONVERGED EIGENVECTOR .......... */
L830:
	*ierr = -r__;
	xu = 0.;
	goto L870;
/*     .......... NORMALIZE SO THAT SUM OF SQUARES IS */
/*                1 AND EXPAND TO FULL ORDER .......... */
L840:
	u = 0.;

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
/* L860: */
	    u = pythag_(&u, &rv6[i__]);
	}

	xu = 1. / u;

L870:
	i__2 = *n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L880: */
	    z__[i__ + r__ * z_dim1] = 0.;
	}

	i__2 = q;
	for (i__ = p; i__ <= i__2; ++i__) {
/* L900: */
	    z__[i__ + r__ * z_dim1] = rv6[i__] * xu;
	}

	x0 = x1;
L920:
	;
    }

    if (q < *n) {
	goto L100;
    }
L1001:
    return 0;
} /* tinvit_ */

