/* tsturm.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static doublereal c_b26 = 1.;

/* Subroutine */ int tsturm_(integer *nm, integer *n, doublereal *eps1, 
	doublereal *d__, doublereal *e, doublereal *e2, doublereal *lb, 
	doublereal *ub, integer *mm, integer *m, doublereal *w, doublereal *
	z__, integer *ierr, doublereal *rv1, doublereal *rv2, doublereal *rv3,
	 doublereal *rv4, doublereal *rv5, doublereal *rv6)
{
    /* System generated locals */
    integer z_dim1, z_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal norm;
    integer i__, j, k=0, p, q, r__=0, s;
    doublereal u, v;
    integer group, m1=0, m2=0;
    doublereal t1, t2, x0=0.0, x1;
    integer ii, jj, ip;
    doublereal uk, xu=0.0;
    extern doublereal pythag_(doublereal *, doublereal *), epslon_(doublereal 
	    *);
    integer isturm, its;
    doublereal eps2, eps3, eps4, tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRISTURM */
/*     BY PETERS AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971). */

/*     THIS SUBROUTINE FINDS THOSE EIGENVALUES OF A TRIDIAGONAL */
/*     SYMMETRIC MATRIX WHICH LIE IN A SPECIFIED INTERVAL AND THEIR */
/*     ASSOCIATED EIGENVECTORS, USING BISECTION AND INVERSE ITERATION. */

/*     ON INPUT */

/*        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL */
/*          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM */
/*          DIMENSION STATEMENT. */

/*        N IS THE ORDER OF THE MATRIX. */

/*        EPS1 IS AN ABSOLUTE ERROR TOLERANCE FOR THE COMPUTED */
/*          EIGENVALUES.  IT SHOULD BE CHOSEN COMMENSURATE WITH */
/*          RELATIVE PERTURBATIONS IN THE MATRIX ELEMENTS OF THE */
/*          ORDER OF THE RELATIVE MACHINE PRECISION.  IF THE */
/*          INPUT EPS1 IS NON-POSITIVE, IT IS RESET FOR EACH */
/*          SUBMATRIX TO A DEFAULT VALUE, NAMELY, MINUS THE */
/*          PRODUCT OF THE RELATIVE MACHINE PRECISION AND THE */
/*          1-NORM OF THE SUBMATRIX. */

/*        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX. */

/*        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX */
/*          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY. */

/*        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E. */
/*          E2(1) IS ARBITRARY. */

/*        LB AND UB DEFINE THE INTERVAL TO BE SEARCHED FOR EIGENVALUES. */
/*          IF LB IS NOT LESS THAN UB, NO EIGENVALUES WILL BE FOUND. */

/*        MM SHOULD BE SET TO AN UPPER BOUND FOR THE NUMBER OF */
/*          EIGENVALUES IN THE INTERVAL.  WARNING. IF MORE THAN */
/*          MM EIGENVALUES ARE DETERMINED TO LIE IN THE INTERVAL, */
/*          AN ERROR RETURN IS MADE WITH NO VALUES OR VECTORS FOUND. */

/*     ON OUTPUT */

/*        EPS1 IS UNALTERED UNLESS IT HAS BEEN RESET TO ITS */
/*          (LAST) DEFAULT VALUE. */

/*        D AND E ARE UNALTERED. */

/*        ELEMENTS OF E2, CORRESPONDING TO ELEMENTS OF E REGARDED */
/*          AS NEGLIGIBLE, HAVE BEEN REPLACED BY ZERO CAUSING THE */
/*          MATRIX TO SPLIT INTO A DIRECT SUM OF SUBMATRICES. */
/*          E2(1) IS ALSO SET TO ZERO. */

/*        M IS THE NUMBER OF EIGENVALUES DETERMINED TO LIE IN (LB,UB). */

/*        W CONTAINS THE M EIGENVALUES IN ASCENDING ORDER IF THE MATRIX */
/*          DOES NOT SPLIT.  IF THE MATRIX SPLITS, THE EIGENVALUES ARE */
/*          IN ASCENDING ORDER FOR EACH SUBMATRIX.  IF A VECTOR ERROR */
/*          EXIT IS MADE, W CONTAINS THOSE VALUES ALREADY FOUND. */

/*        Z CONTAINS THE ASSOCIATED SET OF ORTHONORMAL EIGENVECTORS. */
/*          IF AN ERROR EXIT IS MADE, Z CONTAINS THOSE VECTORS */
/*          ALREADY FOUND. */

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          3*N+1      IF M EXCEEDS MM. */
/*          4*N+R      IF THE EIGENVECTOR CORRESPONDING TO THE R-TH */
/*                     EIGENVALUE FAILS TO CONVERGE IN 5 ITERATIONS. */

/*        RV1, RV2, RV3, RV4, RV5, AND RV6 ARE TEMPORARY STORAGE ARRAYS. 
*/

/*     THE ALGOL PROCEDURE STURMCNT CONTAINED IN TRISTURM */
/*     APPEARS IN TSTURM IN-LINE. */

/*     CALLS PYTHAG FOR  DSQRT(A*A + B*B) . */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --rv6;
    --rv5;
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
    --w;

    /* Function Body */
    *ierr = 0;
    t1 = *lb;
    t2 = *ub;
/*     .......... LOOK FOR SMALL SUB-DIAGONAL ENTRIES .......... */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ == 1) {
	    goto L20;
	}
	tst1 = (d__1 = d__[i__], abs(d__1)) + (d__2 = d__[i__ - 1], abs(d__2))
		;
	tst2 = tst1 + (d__1 = e[i__], abs(d__1));
	if (tst2 > tst1) {
	    goto L40;
	}
L20:
	e2[i__] = 0.;
L40:
	;
    }
/*     .......... DETERMINE THE NUMBER OF EIGENVALUES */
/*                IN THE INTERVAL .......... */
    p = 1;
    q = *n;
    x1 = *ub;
    isturm = 1;
    goto L320;
L60:
    *m = s;
    x1 = *lb;
    isturm = 2;
    goto L320;
L80:
    *m -= s;
    if (*m > *mm) {
	goto L980;
    }
    q = 0;
    r__ = 0;
/*     .......... ESTABLISH AND PROCESS NEXT SUBMATRIX, REFINING */
/*                INTERVAL BY THE GERSCHGORIN BOUNDS .......... */
L100:
    if (r__ == *m) {
	goto L1001;
    }
    p = q + 1;
    xu = d__[p];
    x0 = d__[p];
    u = 0.;

    i__1 = *n;
    for (q = p; q <= i__1; ++q) {
	x1 = u;
	u = 0.;
	v = 0.;
	if (q == *n) {
	    goto L110;
	}
	u = (d__1 = e[q + 1], abs(d__1));
	v = e2[q + 1];
L110:
/* Computing MIN */
	d__1 = d__[q] - (x1 + u);
	xu = min(d__1,xu);
/* Computing MAX */
	d__1 = d__[q] + (x1 + u);
	x0 = max(d__1,x0);
	if (v == 0.) {
	    goto L140;
	}
/* L120: */
    }

L140:
/* Computing MAX */
    d__2 = abs(xu), d__3 = abs(x0);
    d__1 = max(d__2,d__3);
    x1 = epslon_(&d__1);
    if (*eps1 <= 0.) {
	*eps1 = -x1;
    }
    if (p != q) {
	goto L180;
    }
/*     .......... CHECK FOR ISOLATED ROOT WITHIN INTERVAL .......... */
    if (t1 > d__[p] || d__[p] >= t2) {
	goto L940;
    }
    ++r__;

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L160: */
	z__[i__ + r__ * z_dim1] = 0.;
    }

    w[r__] = d__[p];
    z__[p + r__ * z_dim1] = 1.;
    goto L940;
L180:
    u = (doublereal) (q - p + 1);
    x1 = u * x1;
/* Computing MAX */
    d__1 = t1, d__2 = xu - x1;
    *lb = max(d__1,d__2);
/* Computing MIN */
    d__1 = t2, d__2 = x0 + x1;
    *ub = min(d__1,d__2);
    x1 = *lb;
    isturm = 3;
    goto L320;
L200:
    m1 = s + 1;
    x1 = *ub;
    isturm = 4;
    goto L320;
L220:
    m2 = s;
    if (m1 > m2) {
	goto L940;
    }
/*     .......... FIND ROOTS BY BISECTION .......... */
    x0 = *ub;
    isturm = 5;

    i__1 = m2;
    for (i__ = m1; i__ <= i__1; ++i__) {
	rv5[i__] = *ub;
	rv4[i__] = *lb;
/* L240: */
    }
/*     .......... LOOP FOR K-TH EIGENVALUE */
/*                FOR K=M2 STEP -1 UNTIL M1 DO -- */
/*                (-DO- NOT USED TO LEGALIZE -COMPUTED GO TO-) .......... 
*/
    k = m2;
L250:
    xu = *lb;
/*     .......... FOR I=K STEP -1 UNTIL M1 DO -- .......... */
    i__1 = k;
    for (ii = m1; ii <= i__1; ++ii) {
	i__ = m1 + k - ii;
	if (xu >= rv4[i__]) {
	    goto L260;
	}
	xu = rv4[i__];
	goto L280;
L260:
	;
    }

L280:
    if (x0 > rv5[k]) {
	x0 = rv5[k];
    }
/*     .......... NEXT BISECTION STEP .......... */
L300:
    x1 = (xu + x0) * .5;
    if (x0 - xu <= abs(*eps1)) {
	goto L420;
    }
    tst1 = (abs(xu) + abs(x0)) * 2.;
    tst2 = tst1 + (x0 - xu);
    if (tst2 == tst1) {
	goto L420;
    }
/*     .......... IN-LINE PROCEDURE FOR STURM SEQUENCE .......... */
L320:
    s = p - 1;
    u = 1.;

    i__1 = q;
    for (i__ = p; i__ <= i__1; ++i__) {
	if (u != 0.) {
	    goto L325;
	}
	v = (d__1 = e[i__], abs(d__1)) / epslon_(&c_b26);
	if (e2[i__] == 0.) {
	    v = 0.;
	}
	goto L330;
L325:
	v = e2[i__] / u;
L330:
	u = d__[i__] - x1 - v;
	if (u < 0.) {
	    ++s;
	}
/* L340: */
    }

    switch (isturm) {
	case 1:  goto L60;
	case 2:  goto L80;
	case 3:  goto L200;
	case 4:  goto L220;
	case 5:  goto L360;
    }
/*     .......... REFINE INTERVALS .......... */
L360:
    if (s >= k) {
	goto L400;
    }
    xu = x1;
    if (s >= m1) {
	goto L380;
    }
    rv4[m1] = x1;
    goto L300;
L380:
    rv4[s + 1] = x1;
    if (rv5[s] > x1) {
	rv5[s] = x1;
    }
    goto L300;
L400:
    x0 = x1;
    goto L300;
/*     .......... K-TH EIGENVALUE FOUND .......... */
L420:
    rv5[k] = x1;
    --k;
    if (k >= m1) {
	goto L250;
    }
/*     .......... FIND VECTORS BY INVERSE ITERATION .......... */
    norm = (d__1 = d__[p], abs(d__1));
    ip = p + 1;

    i__1 = q;
    for (i__ = ip; i__ <= i__1; ++i__) {
/* L500: */
/* Computing MAX */
	d__3 = norm, d__4 = (d__1 = d__[i__], abs(d__1)) + (d__2 = e[i__], 
		abs(d__2));
	norm = max(d__3,d__4);
    }
/*     .......... EPS2 IS THE CRITERION FOR GROUPING, */
/*                EPS3 REPLACES ZERO PIVOTS AND EQUAL */
/*                ROOTS ARE MODIFIED BY EPS3, */
/*                EPS4 IS TAKEN VERY SMALL TO AVOID OVERFLOW .......... */
    eps2 = norm * .001;
    eps3 = epslon_(&norm);
    uk = (doublereal) (q - p + 1);
    eps4 = uk * eps3;
    uk = eps4 / sqrt(uk);
    group = 0;
    s = p;

    i__1 = m2;
    for (k = m1; k <= i__1; ++k) {
	++r__;
	its = 1;
	w[r__] = rv5[k];
	x1 = rv5[k];
/*     .......... LOOK FOR CLOSE OR COINCIDENT ROOTS .......... */
	if (k == m1) {
	    goto L520;
	}
	if (x1 - x0 >= eps2) {
	    group = -1;
	}
	++group;
	if (x1 <= x0) {
	    x1 = x0 + eps3;
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

	i__2 = group;
	for (jj = 1; jj <= i__2; ++jj) {
	    j = r__ - group - 1 + jj;
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
	    goto L960;
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
/* L920: */
    }

L940:
    if (q < *n) {
	goto L100;
    }
    goto L1001;
/*     .......... SET ERROR -- NON-CONVERGED EIGENVECTOR .......... */
L960:
    *ierr = (*n << 2) + r__;
    goto L1001;
/*     .......... SET ERROR -- UNDERESTIMATE OF NUMBER OF */
/*                EIGENVALUES IN INTERVAL .......... */
L980:
    *ierr = *n * 3 + 1;
L1001:
    *lb = t1;
    *ub = t2;
    return 0;
} /* tsturm_ */

