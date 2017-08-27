/* bisect.f -- translated by f2c (version 19961017).
   You must link the resulting object file with the libraries:
	-lconverted_from_fortran -lm   (in that order)
*/

#include "converted_from_fortran.h"

/* Table of constant values */

static doublereal c_b26 = 1.;

/* Subroutine */ int bisect_(integer *n, doublereal *eps1, doublereal *d__, 
	doublereal *e, doublereal *e2, doublereal *lb, doublereal *ub, 
	integer *mm, integer *m, doublereal *w, integer *ind, integer *ierr, 
	doublereal *rv4, doublereal *rv5)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2, d__3;

    /* Local variables */
    integer i__, j, k=0, l, p, q, r__=0, s;
    doublereal u, v;
    integer m1=0, m2=0;
    doublereal t1, t2, x0=0.0, x1;
    integer ii;
    doublereal xu=0.0;
    doublereal epslon_(doublereal *);
    integer isturm, tag;
    doublereal tst1, tst2;



/*     THIS SUBROUTINE IS A TRANSLATION OF THE BISECTION TECHNIQUE */
/*     IN THE ALGOL PROCEDURE TRISTURM BY PETERS AND WILKINSON. */
/*     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 418-439(1971). */

/*     THIS SUBROUTINE FINDS THOSE EIGENVALUES OF A TRIDIAGONAL */
/*     SYMMETRIC MATRIX WHICH LIE IN A SPECIFIED INTERVAL, */
/*     USING BISECTION. */

/*     ON INPUT */

/*        N IS THE ORDER OF THE MATRIX. */

/*        EPS1 IS AN ABSOLUTE ERROR TOLERANCE FOR THE COMPUTED */
/*          EIGENVALUES.  IF THE INPUT EPS1 IS NON-POSITIVE, */
/*          IT IS RESET FOR EACH SUBMATRIX TO A DEFAULT VALUE, */
/*          NAMELY, MINUS THE PRODUCT OF THE RELATIVE MACHINE */
/*          PRECISION AND THE 1-NORM OF THE SUBMATRIX. */

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
/*          AN ERROR RETURN IS MADE WITH NO EIGENVALUES FOUND. */

/*     ON OUTPUT */

/*        EPS1 IS UNALTERED UNLESS IT HAS BEEN RESET TO ITS */
/*          (LAST) DEFAULT VALUE. */

/*        D AND E ARE UNALTERED. */

/*        ELEMENTS OF E2, CORRESPONDING TO ELEMENTS OF E REGARDED */
/*          AS NEGLIGIBLE, HAVE BEEN REPLACED BY ZERO CAUSING THE */
/*          MATRIX TO SPLIT INTO A DIRECT SUM OF SUBMATRICES. */
/*          E2(1) IS ALSO SET TO ZERO. */

/*        M IS THE NUMBER OF EIGENVALUES DETERMINED TO LIE IN (LB,UB). */

/*        W CONTAINS THE M EIGENVALUES IN ASCENDING ORDER. */

/*        IND CONTAINS IN ITS FIRST M POSITIONS THE SUBMATRIX INDICES */
/*          ASSOCIATED WITH THE CORRESPONDING EIGENVALUES IN W -- */
/*          1 FOR EIGENVALUES BELONGING TO THE FIRST SUBMATRIX FROM */
/*          THE TOP, 2 FOR THOSE BELONGING TO THE SECOND SUBMATRIX, ETC.. 
*/

/*        IERR IS SET TO */
/*          ZERO       FOR NORMAL RETURN, */
/*          3*N+1      IF M EXCEEDS MM. */

/*        RV4 AND RV5 ARE TEMPORARY STORAGE ARRAYS. */

/*     THE ALGOL PROCEDURE STURMCNT CONTAINED IN TRISTURM */
/*     APPEARS IN BISECT IN-LINE. */

/*     NOTE THAT SUBROUTINE TQL1 OR IMTQL1 IS GENERALLY FASTER THAN */
/*     BISECT, IF MORE THAN N/4 EIGENVALUES ARE TO BE FOUND. */

/*     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW, */
/*     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY 
*/

/*     THIS VERSION DATED AUGUST 1983. */

/*     ------------------------------------------------------------------ 
*/

    /* Parameter adjustments */
    --rv5;
    --rv4;
    --e2;
    --e;
    --d__;
    --ind;
    --w;

    /* Function Body */
    *ierr = 0;
    tag = 0;
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
    ++tag;
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
    m1 = p;
    m2 = p;
    rv5[p] = d__[p];
    goto L900;
L180:
    x1 *= q - p + 1;
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
/*     .......... ORDER EIGENVALUES TAGGED WITH THEIR */
/*                SUBMATRIX ASSOCIATIONS .......... */
L900:
    s = r__;
    r__ = r__ + m2 - m1 + 1;
    j = 1;
    k = m1;

    i__1 = r__;
    for (l = 1; l <= i__1; ++l) {
	if (j > s) {
	    goto L910;
	}
	if (k > m2) {
	    goto L940;
	}
	if (rv5[k] >= w[l]) {
	    goto L915;
	}

	i__2 = s;
	for (ii = j; ii <= i__2; ++ii) {
	    i__ = l + s - ii;
	    w[i__ + 1] = w[i__];
	    ind[i__ + 1] = ind[i__];
/* L905: */
	}

L910:
	w[l] = rv5[k];
	ind[l] = tag;
	++k;
	goto L920;
L915:
	++j;
L920:
	;
    }

L940:
    if (q < *n) {
	goto L100;
    }
    goto L1001;
/*     .......... SET ERROR -- UNDERESTIMATE OF NUMBER OF */
/*                EIGENVALUES IN INTERVAL .......... */
L980:
    *ierr = *n * 3 + 1;
L1001:
    *lb = t1;
    *ub = t2;
    return 0;
} /* bisect_ */

